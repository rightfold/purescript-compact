module Test.Main
( main
) where

import Control.Monad.Eff (Eff)
import Data.Either.Compact
import Prelude
import Test.Assert (ASSERT, assert)

main :: forall eff. Eff (assert :: ASSERT | eff) Unit
main = do
  assert $ function 1 (_ $ 2) (fromFunction id) == 2
  assert $ function 1 (_ $ 2) (fromArray []) == 1

  assert $ array [1] id (fromArray []) == []
  assert $ array [1] id (fromFunction id) == [1]

  assert $ record 1 _.p (fromRecord {p: 2}) == 2
  assert $ record 1 _.p (fromFunction id) == 1

  assert $ int 1 id (fromInt 2) == 2
  assert $ int 1 id (fromFunction id) == 1

  assert $ number 1.0 id (fromNumber 2.0) == 2.0
  assert $ number 1.0 id (fromFunction id) == 1.0

  assert $ char 'a' id (fromChar 'b') == 'b'
  assert $ char 'a' id (fromFunction id) == 'a'

  assert $ string "a" id (fromString "b") == "b"
  assert $ string "a" id (fromFunction id) == "a"

  assert $ boolean false id (fromBoolean true) == true
  assert $ boolean false id (fromFunction id) == false
