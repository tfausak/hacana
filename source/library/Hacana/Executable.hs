module Hacana.Executable where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar.Entry
import qualified Codec.Compression.GZip as Gzip
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as Stm
import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Catch
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Pool as Pool
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Time as Time
import qualified Data.Time.Clock.POSIX as Time
import qualified Data.Version as Version
import qualified Database.SQLite.Simple as Sql
import qualified Distribution.Parsec as Cabal
import qualified Distribution.Types.PackageVersionConstraint as Cabal
import qualified Hacana.Exception.InvalidOption as InvalidOption
import qualified Hacana.Exception.UnexpectedArgument as UnexpectedArgument
import qualified Hacana.Exception.UnknownOption as UnknownOption
import qualified Hacana.Extra.Either as Either
import qualified Hacana.Model.Blob as Blob
import qualified Hacana.Model.File as File
import qualified Hacana.Model.HackageIndex as HackageIndex
import qualified Hacana.Model.Preference as Preference
import qualified Hacana.Type.Config as Config
import qualified Hacana.Type.Constraint as Constraint
import qualified Hacana.Type.Context as Context
import qualified Hacana.Type.Flag as Flag
import qualified Hacana.Type.Hash as Hash
import qualified Hacana.Type.Model as Model
import qualified Hacana.Type.PackageName as PackageName
import qualified Hacana.Type.Path as Path
import qualified Hacana.Type.Revision as Revision
import qualified Hacana.Type.VersionNumber as VersionNumber
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types as Http
import qualified Paths_hacana as Hacana
import qualified System.Console.GetOpt as GetOpt
import qualified System.Environment as Environment
import qualified System.Exit as Exit

executable :: IO ()
executable = do
  arguments <- Environment.getArgs
  let (flags, args, opts, errs) = GetOpt.getOpt' GetOpt.Permute Flag.optDescrs arguments
  mapM_ (Catch.throwM . UnexpectedArgument.UnexpectedArgument) args
  mapM_ (Catch.throwM . UnknownOption.UnknownOption) opts
  mapM_ (Catch.throwM . InvalidOption.InvalidOption) errs
  config <- Monad.foldM Config.applyFlag Config.initial flags

  Monad.when (Config.help config) $ do
    name <- Environment.getProgName
    putStr $ GetOpt.usageInfo name Flag.optDescrs
    Exit.exitSuccess

  Monad.when (Config.version config) $ do
    putStrLn $ Version.showVersion Hacana.version
    Exit.exitSuccess

  context <- Context.fromConfig config
  Monad.forM_ (pragmas <> migrations) $ \ query ->
    Pool.withResource (Context.pool context) $ \ connection ->
      Sql.execute_ connection query

  logLn "Starting ..."

  let
    loop :: IO ()
    loop = Monad.forever $ do
      Pool.withResource (Context.pool context) $ \ connection -> do
        [Sql.Only n] <- Sql.query_ connection "select count(*) from HackageIndex"
        logLn $ "hackage index count " <> show (n :: Int)
      Concurrent.threadDelay 1_000_000
  Async.withAsync loop $ \ async ->
    flip Catch.onException (Pool.destroyAllResources (Context.pool context) *> Async.cancel async)
    . Monad.forever
    $ do
    logLn "Looping ..."

    do
      logLn "Checking for updates to the Hackage index ..."

      request <- Client.parseUrlThrow "https://hackage.haskell.org/01-index.tar.gz"
      headResponse <- Client.httpNoBody request { Client.method = Http.methodHead } $ Context.manager context
      lastModified <- case lookup "Last-Modified" $ Client.responseHeaders headResponse of
        Nothing -> die "missing Last-Modified header"
        Just x -> Time.parseTimeM False Time.defaultTimeLocale "%a, %d %b %Y %H:%M:%S %Z"
          . Text.unpack
          $ Text.Encoding.decodeUtf8Lenient x

      maybeHackageIndex <- fmap Maybe.listToMaybe . Pool.withResource (Context.pool context) $ \ connection ->
        Sql.query_ connection "select * from HackageIndex order by fetchedAt desc limit 1"
      Monad.when (fmap (HackageIndex.fetchedAt . Model.value) maybeHackageIndex < Just lastModified) $ do
        logLn "Updating Hackage index ..."

        now <- Time.getCurrentTime
        response <- Client.httpLbs request $ Context.manager context
        Pool.withResource (Context.pool context) $ \ connection -> Sql.withTransaction connection $ do
          let contents = LazyByteString.toStrict $ Client.responseBody response
              hash = Hash.new contents
          logLn "Inserting blob ..."
          [Sql.Only blobKey] <- Sql.query connection
            "insert into Blob (contents, hash) values (?, ?) returning key"
            Blob.Blob { Blob.contents = contents, Blob.hash = hash }
          logLn "Inserting Hackage index ..."
          Sql.execute connection
            "insert into HackageIndex (blob, fetchedAt) values (?, ?)"
            HackageIndex.HackageIndex { HackageIndex.blob = blobKey, HackageIndex.fetchedAt = now }

      logLn "Finished updating Hackage index."

    do
      logLn "Pruning old Hackage indexes ..."

      hackageIndexes <- Pool.withResource (Context.pool context) $ \ connection ->
        Sql.query_ connection "select * from HackageIndex order by fetchedAt desc"
      Monad.forM_ (drop 1 hackageIndexes) $ \ hackageIndex ->
        Pool.withResource (Context.pool context) $ \ connection -> Sql.withTransaction connection $ do
          logLn "Deleting Hackage index ..."
          Sql.execute connection "delete from HackageIndex where key = ?" [Model.key hackageIndex]
          logLn "Deleting blob ..."
          Sql.execute connection "delete from Blob where key = ?" [HackageIndex.blob $ Model.value hackageIndex]

      logLn "Finished pruning old Hackage indexes."

    do
      logLn "Processing Hackage index ..."

      hackageIndexes <- Pool.withResource (Context.pool context) $ \ connection ->
        Sql.query_ connection $ "select * from HackageIndex order by fetchedAt desc limit 1"
      hackageIndex <- case hackageIndexes of
        [] -> die "No Hackage index!"
        hackageIndex : _ -> pure hackageIndex

      entries <- do
        logLn "Getting blob ..."
        let blobKey = HackageIndex.blob $ Model.value hackageIndex
        blobs <- Pool.withResource (Context.pool context) $ \ connection ->
          Sql.query connection "select * from Blob where key = ?" [blobKey]
        blob <- case blobs of
          [] -> die $ "No Blob with key: " <> show blobKey
          blob : _ -> pure blob

        logLn "Got blob."
        pure
          . Tar.foldEntries ((:) . Right) [] (pure . Left)
          . Tar.read
          . Gzip.decompress
          . LazyByteString.fromStrict
          . Blob.contents
          $ Model.value blob

      logLn "Processing entries ..."
      revisionsVar <- Stm.newTVarIO Map.empty
      constraintsVar <- Stm.newTVarIO Map.empty

      Monad.forM_ entries $ \ eitherEntry -> do
        entry <- Either.throw eitherEntry
        contents <- case Tar.entryContent entry of
          Tar.NormalFile lazyByteString _ -> pure $ LazyByteString.toStrict lazyByteString
          _ -> die $ "unexpected tar entry content: " <> show entry
        blobKey <- Pool.withResource (Context.pool context) $ \ connection -> Sql.withTransaction connection $ do
          let hash = Hash.new contents
          blobKeys <- Sql.query connection "select key from Blob where hash = ?" [hash]
          case blobKeys of
            [] -> do
              [Sql.Only blobKey] <- Sql.query connection
                "insert into Blob (contents, hash) values (?, ?) returning key"
                Blob.Blob { Blob.contents = contents, Blob.hash = hash }
              pure blobKey
            Sql.Only blobKey : _ -> pure blobKey
        case Foldable.toList . Path.intoSeq . Path.fromTarPath $ Tar.Entry.entryTarPath entry of

          [package, version, "package.json"] -> do
            packageName <- Either.fail . PackageName.fromString $ Text.unpack package
            versionNumber <- Either.fail . VersionNumber.fromString $ Text.unpack version
            Pool.withResource (Context.pool context) $ \ connection ->
              Sql.execute connection
                "insert into File (blob, groupId, groupName, ownerId, ownerName, path, permissions, time) \
                \ values (?, ?, ?, ?, ?, ?, ?, ?) \
                \ on conflict (path) do nothing"
                File.File
                  { File.blob = blobKey
                  , File.groupId = Tar.Entry.groupId $ Tar.Entry.entryOwnership entry
                  , File.groupName = Text.pack . Tar.Entry.groupName $ Tar.Entry.entryOwnership entry
                  , File.ownerId = Tar.Entry.ownerId $ Tar.Entry.entryOwnership entry
                  , File.ownerName = Text.pack . Tar.Entry.ownerName $ Tar.Entry.entryOwnership entry
                  , File.path = Path.fromSeq $ Seq.fromList
                    [ Text.pack $ PackageName.intoString packageName
                    , Text.pack $ VersionNumber.intoString versionNumber
                    , "package.json"
                    ]
                  , File.permissions = Tar.Entry.entryPermissions entry
                  , File.time = Time.posixSecondsToUTCTime . fromIntegral $ Tar.Entry.entryTime entry
                  }

          [package, version, name] -> do
            case Text.stripSuffix ".cabal" name of
              Nothing -> die $ "unexpected extension: " <> show name
              Just stripped -> Monad.when (stripped /= package) . die $ "mismatched package name: " <> show (package, stripped)
            packageName <- Either.fail . PackageName.fromString $ Text.unpack package
            versionNumber <- Either.fail . VersionNumber.fromString $ Text.unpack version
            revision <- Stm.atomically . Stm.stateTVar revisionsVar $ \ m ->
              let k = (packageName, versionNumber)
                  v = Map.findWithDefault Revision.zero k m
              in (v, Map.insert k (Revision.increment v) m)
            Pool.withResource (Context.pool context) $ \ connection ->
              Sql.execute connection
                "insert into File (blob, groupId, groupName, ownerId, ownerName, path, permissions, time) \
                \ values (?, ?, ?, ?, ?, ?, ?, ?) \
                \ on conflict (path) do nothing"
                File.File
                  { File.blob = blobKey
                  , File.groupId = Tar.Entry.groupId $ Tar.Entry.entryOwnership entry
                  , File.groupName = Text.pack . Tar.Entry.groupName $ Tar.Entry.entryOwnership entry
                  , File.ownerId = Tar.Entry.ownerId $ Tar.Entry.entryOwnership entry
                  , File.ownerName = Text.pack . Tar.Entry.ownerName $ Tar.Entry.entryOwnership entry
                  , File.path = Path.fromSeq $ Seq.fromList
                    [ Text.pack $ PackageName.intoString packageName
                    , Text.pack $ VersionNumber.intoString versionNumber
                    , "cabal"
                    , Text.pack $ Revision.intoString revision
                    ]
                  , File.permissions = Tar.Entry.entryPermissions entry
                  , File.time = Time.posixSecondsToUTCTime . fromIntegral $ Tar.Entry.entryTime entry
                  }

          [package, "preferred-versions"] -> do
            packageName <- Either.fail . PackageName.fromString $ Text.unpack package
            constraint <- if ByteString.null contents
              then pure Constraint.any
              else case Cabal.simpleParsecBS contents of
                Nothing -> fail $ "invalid package version constraint: " <> show contents
                Just (Cabal.PackageVersionConstraint p v) -> if p == PackageName.intoCabal packageName
                  then pure $ Constraint.fromCabal v
                  else fail $ "mismatched package: " <> show (p, packageName)
            number <- Stm.atomically . Stm.stateTVar constraintsVar $ \ m ->
              let k = packageName
                  v = Map.findWithDefault Seq.empty k m
              in (Seq.length v, Map.insertWith mappend k (Seq.singleton constraint) m)
            Pool.withResource (Context.pool context) $ \ connection ->
              Sql.execute connection
                "insert into File (blob, groupId, groupName, ownerId, ownerName, path, permissions, time) \
                \ values (?, ?, ?, ?, ?, ?, ?, ?) \
                \ on conflict (path) do nothing"
                File.File
                  { File.blob = blobKey
                  , File.groupId = Tar.Entry.groupId $ Tar.Entry.entryOwnership entry
                  , File.groupName = Text.pack . Tar.Entry.groupName $ Tar.Entry.entryOwnership entry
                  , File.ownerId = Tar.Entry.ownerId $ Tar.Entry.entryOwnership entry
                  , File.ownerName = Text.pack . Tar.Entry.ownerName $ Tar.Entry.entryOwnership entry
                  , File.path = Path.fromSeq $ Seq.fromList
                    [ Text.pack $ PackageName.intoString packageName
                    , "preferred-versions"
                    , Text.pack $ show number
                    ]
                  , File.permissions = Tar.Entry.entryPermissions entry
                  , File.time = Time.posixSecondsToUTCTime . fromIntegral $ Tar.Entry.entryTime entry
                  }

          xs -> die $ "unexpected tar entry path: " <> show xs

      logLn "Upserting preferences ..."
      constraints <- Stm.readTVarIO constraintsVar
      Monad.forM_ (Map.toList constraints) $ \ (p, cs) -> do
        let c = case Seq.viewl cs of
              Seq.EmptyL -> Constraint.any
              x Seq.:< _ -> x
        Pool.withResource (Context.pool context) $ \ connection ->
          Sql.execute connection
            "insert into Preference (\"constraint\", package) \
            \ values (?, ?) \
            \ on conflict (package) do update set \"constraint\" = excluded.\"constraint\""
            Preference.Preference { Preference.constraint = c, Preference.package = p }

      logLn "Done processing entries."

    logLn "Waiting ..."
    Concurrent.threadDelay 60_000_000

logLn :: String -> IO ()
logLn message = do
  now <- Time.getCurrentTime
  putStrLn $ Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%S%3QZ " now <> message

die :: String -> IO a
die = Catch.throwM . userError

pragmas :: [Sql.Query]
pragmas =
  [ "pragma auto_vacuum = 'incremental'"
  , "pragma foreign_keys = true"
  , "pragma journal_mode = 'wal'"
  , "pragma synchronous = 'normal'"
  ]

migrations :: [Sql.Query]
migrations =
  [ "create table if not exists Blob \
    \ ( key integer primary key \
    \ , contents blob not null \
    \ , hash text not null unique )"
  , "create table if not exists HackageIndex \
    \ ( key integer primary key \
    \ , blob integer not null references Blob \
    \ , fetchedAt text not null )"
  , "create table if not exists File \
    \ ( key integer primary key \
    \ , blob integer not null references Blob \
    \ , groupId integer not null \
    \ , groupName text not null \
    \ , ownerId integer not null \
    \ , ownerName text not null \
    \ , path text not null unique \
    \ , permissions integer not null \
    \ , time text not null )"
  , "create table if not exists Preference \
    \ ( key integer primary key \
    \ , \"constraint\" text not null \
    \ , package text not null unique )"
  ]
