module Hacana.Extra.Maybe where

note :: a -> Maybe b -> Either a b
note = flip maybe Right . Left
