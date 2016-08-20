module Data.Either.Compact
( CompactEither
, function
, array
, record
, int
, number
, char
, string
, boolean
) where

import Prelude
import Unsafe.Coerce (unsafeCoerce)

foreign import data CompactEither
  :: *    -- Domain
  -> *    -- Codomain
  -> *    -- Element
  -> # *  -- Fields
  -> *    -- Function?
  -> *    -- Array?
  -> *    -- Record?
  -> *    -- Int?
  -> *    -- Number?
  -> *    -- Char?
  -> *    -- String?
  -> *    -- Boolean?
  -> *

function :: forall domain codomain element fields array record int number char string boolean result
          . CompactEither domain codomain element fields Unit array record int number char string boolean
         -> result
         -> ((domain -> codomain) -> result)
         -> result
function either default fn
  | isFunction either = fn (unsafeCoerce either)
  | otherwise = default

array :: forall domain codomain element fields function record int number char string boolean result
       . CompactEither domain codomain element fields function Unit record int number char string boolean
      -> result
      -> (Array element -> result)
      -> result
array either default fn
  | isArray either = fn (unsafeCoerce either)
  | otherwise = default

record :: forall domain codomain element fields function array int number char string boolean result
        . CompactEither domain codomain element fields function array Unit int number char string boolean
       -> result
       -> ({| fields} -> result)
       -> result
record either default fn
  | isRecord either = fn (unsafeCoerce either)
  | otherwise = default

int :: forall domain codomain element fields function array record char string boolean result
     . CompactEither domain codomain element fields function array record Unit Void char string boolean
    -> result
    -> (Int -> result)
    -> result
int either default fn
  | isNumber either = fn (unsafeCoerce either)
  | otherwise = default

number :: forall domain codomain element fields function array record char string boolean result
        . CompactEither domain codomain element fields function array record Void Unit char string boolean
       -> result
       -> (Number -> result)
       -> result
number either default fn
  | isNumber either = fn (unsafeCoerce either)
  | otherwise = default

char :: forall domain codomain element fields function array record int number boolean result
      . CompactEither domain codomain element fields function array record int number Unit Void boolean
     -> result
     -> (Char -> result)
     -> result
char either default fn
  | isString either = fn (unsafeCoerce either)
  | otherwise = default

string :: forall domain codomain element fields function array record int number boolean result
        . CompactEither domain codomain element fields function array record int number Void Unit boolean
       -> result
       -> (Char -> result)
       -> result
string either default fn
  | isString either = fn (unsafeCoerce either)
  | otherwise = default

boolean :: forall domain codomain element fields function array record int number char string result
         . CompactEither domain codomain element fields function array record int number char string Unit
        -> result
        -> (Boolean -> result)
        -> result
boolean either default fn
  | isBoolean either = fn (unsafeCoerce either)
  | otherwise = default

foreign import isFunction :: forall a. a -> Boolean
foreign import isArray :: forall a. a -> Boolean
foreign import isRecord :: forall a. a -> Boolean
foreign import isNumber :: forall a. a -> Boolean
foreign import isString :: forall a. a -> Boolean
foreign import isBoolean :: forall a. a -> Boolean
