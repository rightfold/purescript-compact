module Data.Either.Compact
( CompactEither
, fromFunction, function
, fromArray, array
, fromRecord, record
, fromInt, int
, fromNumber, number
, fromChar, char
, fromString, string
, fromBoolean, boolean
) where

import Prelude
import Unsafe.Coerce (unsafeCoerce)

-- | Sum type with compact representation.
-- |
-- | #### Type arguments
-- |
-- |  1. Function domain, or `Void`.
-- |  2. Function codomain, or `Void`.
-- |  3. Array element, or `Void`.
-- |  4. Record fields, or `()`.
-- |  5. Can this be a function? `Unit` if yes, `Void` if no.
-- |  6. Can this be an array? `Unit` if yes, `Void` if no.
-- |  7. Can this be a record? `Unit` if yes, `Void` if no.
-- |  8. Can this be an int? `Unit` if yes, `Void` if no.
-- |  9. Can this be a number? `Unit` if yes, `Void` if no.
-- |  10. Can this be a char? `Unit` if yes, `Void` if no.
-- |  11. Can this be a string? `Unit` if yes, `Void` if no.
-- |  12. Can this be a Boolean? `Unit` if yes, `Void` if no.
-- |
-- | If two types use the same representation, you cannot set `Unit` on both of
-- | them. There are two such illegal combinations: (`Int`, `Number`) and
-- | (`Char`, `String`).
-- |
-- | #### Example
-- |
-- |     type EitherIntString =
-- |       CompactEither Void Void Void () Void Void Void Unit Void Void Unit Void
-- |     type EitherFunctionIntChar =
-- |       CompactEither Int Int Void () Unit Void Void Unit Void Unit Void Void
foreign import data CompactEither
  :: *    -- Function domain
  -> *    -- Function codomain
  -> *    -- Array element
  -> # *  -- Record fields
  -> *    -- Function?
  -> *    -- Array?
  -> *    -- Record?
  -> *    -- Int?
  -> *    -- Number?
  -> *    -- Char?
  -> *    -- String?
  -> *    -- Boolean?
  -> *

fromFunction :: forall domain codomain element fields array record int number char string boolean
              . (domain -> codomain)
             -> CompactEither domain codomain element fields Unit array record int number char string boolean
fromFunction = unsafeCoerce

function :: forall domain codomain element fields array record int number char string boolean result
          . result
         -> ((domain -> codomain) -> result)
         -> CompactEither domain codomain element fields Unit array record int number char string boolean
         -> result
function default fn either
  | isFunction either = fn (unsafeCoerce either)
  | otherwise = default

fromArray :: forall domain codomain element fields function record int number char string boolean
            . Array element
           -> CompactEither domain codomain element fields function Unit record int number char string boolean
fromArray = unsafeCoerce

array :: forall domain codomain element fields function record int number char string boolean result
       . result
      -> (Array element -> result)
      -> CompactEither domain codomain element fields function Unit record int number char string boolean
      -> result
array default fn either
  | isArray either = fn (unsafeCoerce either)
  | otherwise = default

fromRecord :: forall domain codomain element fields function array int number char string boolean
            . {| fields}
           -> CompactEither domain codomain element fields function array Unit int number char string boolean
fromRecord = unsafeCoerce

record :: forall domain codomain element fields function array int number char string boolean result
        . result
       -> ({| fields} -> result)
       -> CompactEither domain codomain element fields function array Unit int number char string boolean
       -> result
record default fn either
  | isRecord either = fn (unsafeCoerce either)
  | otherwise = default

fromInt :: forall domain codomain element fields function array record char string boolean
         . Int
        -> CompactEither domain codomain element fields function array record Unit Void char string boolean
fromInt = unsafeCoerce

int :: forall domain codomain element fields function array record char string boolean result
     . result
    -> (Int -> result)
    -> CompactEither domain codomain element fields function array record Unit Void char string boolean
    -> result
int default fn either
  | isNumber either = fn (unsafeCoerce either)
  | otherwise = default

fromNumber :: forall domain codomain element fields function array record char string boolean
            . Number
           -> CompactEither domain codomain element fields function array record Void Unit char string boolean
fromNumber = unsafeCoerce

number :: forall domain codomain element fields function array record char string boolean result
        . result
       -> (Number -> result)
       -> CompactEither domain codomain element fields function array record Void Unit char string boolean
       -> result
number default fn either
  | isNumber either = fn (unsafeCoerce either)
  | otherwise = default

fromChar :: forall domain codomain element fields function array record int number boolean
          . Char
         -> CompactEither domain codomain element fields function array record int number Unit Void boolean
fromChar = unsafeCoerce

char :: forall domain codomain element fields function array record int number boolean result
      . result
     -> (Char -> result)
     -> CompactEither domain codomain element fields function array record int number Unit Void boolean
     -> result
char default fn either
  | isString either = fn (unsafeCoerce either)
  | otherwise = default

fromString :: forall domain codomain element fields function array record int number boolean
            . String
           -> CompactEither domain codomain element fields function array record int number Void Unit boolean
fromString = unsafeCoerce

string :: forall domain codomain element fields function array record int number boolean result
        . result
       -> (Char -> result)
       -> CompactEither domain codomain element fields function array record int number Void Unit boolean
       -> result
string default fn either
  | isString either = fn (unsafeCoerce either)
  | otherwise = default

fromBoolean :: forall domain codomain element fields function array record int number char string
            . String
           -> CompactEither domain codomain element fields function array record int number char string Unit
fromBoolean = unsafeCoerce

boolean :: forall domain codomain element fields function array record int number char string result
         . result
        -> (Boolean -> result)
        -> CompactEither domain codomain element fields function array record int number char string Unit
        -> result
boolean default fn either
  | isBoolean either = fn (unsafeCoerce either)
  | otherwise = default

foreign import isFunction :: forall a. a -> Boolean
foreign import isArray :: forall a. a -> Boolean
foreign import isRecord :: forall a. a -> Boolean
foreign import isNumber :: forall a. a -> Boolean
foreign import isString :: forall a. a -> Boolean
foreign import isBoolean :: forall a. a -> Boolean
