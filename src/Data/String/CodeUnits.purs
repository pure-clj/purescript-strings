-- | Wraps the functions of Javascript's `String` object.
-- | A String represents a sequence of characters.
-- | For details of the underlying implementation, see [String Reference at MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String).
module Data.String.CodeUnits
  ( singleton
  , fromCharArray
  , charAt
  , toChar
  , toCharArray
  , uncons
  , length
  , countPrefix
  , indexOf
  , indexOf'
  , lastIndexOf
  , lastIndexOf'
  , take
  , takeRight
  , takeWhile
  , drop
  , dropRight
  , dropWhile
  , slice
  , splitAt
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String.Pattern (Pattern)
import Data.String.Unsafe as U

-- | Returns a string of length `1` containing the given character.
-- |
-- | ```purescript
-- | singleton 'l' == "l"
-- | ```
-- |
foreign import singleton :: Char -> String

-- | Converts an array of characters into a string.
-- |
-- | ```purescript
-- | fromCharArray ['H', 'e', 'l', 'l', 'o'] == "Hello"
-- | ```
foreign import fromCharArray :: Array Char -> String

-- | Returns the character at the given index, if the index is within bounds.
-- |
-- | ```purescript
-- | charAt 2 "Hello" == Just 'l'
-- | charAt 10 "Hello" == Nothing
-- | ```
-- |
charAt :: Int -> String -> Maybe Char
charAt = _charAt Just Nothing

foreign import _charAt
  :: (forall a. a -> Maybe a)
  -> (forall a. Maybe a)
  -> Int
  -> String
  -> Maybe Char

-- | Converts the string to a character, if the length of the string is
-- | exactly `1`.
-- |
-- | ```purescript
-- | toChar "l" == Just 'l'
-- | toChar "Hi" == Nothing -- since length is not 1
-- | ```
toChar :: String -> Maybe Char
toChar = _toChar Just Nothing

foreign import _toChar
  :: (forall a. a -> Maybe a)
  -> (forall a. Maybe a)
  -> String
  -> Maybe Char

-- | Converts the string into an array of characters.
-- |
-- | ```purescript
-- | toCharArray "Hello☺\n" == ['H','e','l','l','o','☺','\n']
-- | ```
foreign import toCharArray :: String -> Array Char

-- | Returns the first character and the rest of the string,
-- | if the string is not empty.
-- |
-- | ```purescript
-- | uncons "" == Nothing
-- | uncons "Hello World" == Just { head: 'H', tail: "ello World" }
-- | ```
-- |
uncons :: String -> Maybe { head :: Char, tail :: String }
uncons "" = Nothing
uncons s  = Just { head: U.charAt zero s, tail: drop one s }

-- | Returns the number of characters the string is composed of.
-- |
-- | ```purescript
-- | length "Hello World" == 11
-- | ```
-- |
foreign import length :: String -> Int

-- | Returns the number of contiguous characters at the beginning
-- | of the string for which the predicate holds.
-- |
-- | ```purescript
-- | countPrefix (_ /= ' ') "Hello World" == 5 -- since length "Hello" == 5
-- | ```
-- |
foreign import countPrefix :: (Char -> Boolean) -> String -> Int

-- | Returns the index of the first occurrence of the pattern in the
-- | given string. Returns `Nothing` if there is no match.
-- |
-- | ```purescript
-- | indexOf (Pattern "c") "abcdc" == Just 2
-- | indexOf (Pattern "c") "aaa" == Nothing
-- | ```
-- |
indexOf :: Pattern -> String -> Maybe Int
indexOf = _indexOf Just Nothing

foreign import _indexOf
  :: (forall a. a -> Maybe a)
  -> (forall a. Maybe a)
  -> Pattern
  -> String
  -> Maybe Int

-- | Returns the index of the first occurrence of the pattern in the
-- | given string, starting at the specified index. Returns `Nothing` if there is
-- | no match.
-- |
-- | ```purescript
-- | indexOf' (Pattern "a") 2 "ababa" == Just 2
-- | indexOf' (Pattern "a") 3 "ababa" == Just 4
-- | ```
-- |
indexOf' :: Pattern -> Int -> String -> Maybe Int
indexOf' = _indexOf' Just Nothing

foreign import _indexOf'
  :: (forall a. a -> Maybe a)
  -> (forall a. Maybe a)
  -> Pattern
  -> Int
  -> String
  -> Maybe Int

-- | Returns the index of the last occurrence of the pattern in the
-- | given string. Returns `Nothing` if there is no match.
-- |
-- | ```purescript
-- | lastIndexOf (Pattern "c") "abcdc" == Just 4
-- | lastIndexOf (Pattern "c") "aaa" == Nothing
-- | ```
-- |
lastIndexOf :: Pattern -> String -> Maybe Int
lastIndexOf = _lastIndexOf Just Nothing

foreign import _lastIndexOf
  :: (forall a. a -> Maybe a)
  -> (forall a. Maybe a)
  -> Pattern
  -> String
  -> Maybe Int

-- | Returns the index of the last occurrence of the pattern in the
-- | given string, starting at the specified index
-- | and searching backwards towards the beginning of the string.
-- | Returns `Nothing` if there is no match.
-- |
-- | ```purescript
-- | lastIndexOf' (Pattern "a") 1 "ababa" == Just 0
-- | lastIndexOf' (Pattern "a") 3 "ababa" == Just 2
-- | lastIndexOf' (Pattern "a") 4 "ababa" == Just 4
-- | ```
-- |
lastIndexOf' :: Pattern -> Int -> String -> Maybe Int
lastIndexOf' = _lastIndexOf' Just Nothing

foreign import _lastIndexOf'
  :: (forall a. a -> Maybe a)
  -> (forall a. Maybe a)
  -> Pattern
  -> Int
  -> String
  -> Maybe Int

-- | Returns the first `n` characters of the string.
-- |
-- | ```purescript
-- | take 5 "Hello World" == "Hello"
-- | ```
-- |
foreign import take :: Int -> String -> String

-- | Returns the last `n` characters of the string.
-- |
-- | ```purescript
-- | takeRight 5 "Hello World" == "World"
-- | ```
-- |
takeRight :: Int -> String -> String
takeRight i s = drop (length s - i) s

-- | Returns the longest prefix (possibly empty) of characters that satisfy
-- | the predicate.
-- |
-- | ```purescript
-- | takeWhile (_ /= ':') "http://purescript.org" == "http"
-- | ```
-- |
takeWhile :: (Char -> Boolean) -> String -> String
takeWhile p s = take (countPrefix p s) s

-- | Returns the string without the first `n` characters.
-- |
-- | ```purescript
-- | drop 6 "Hello World" == "World"
-- | ```
-- |
foreign import drop :: Int -> String -> String

-- | Returns the string without the last `n` characters.
-- |
-- | ```purescript
-- | dropRight 6 "Hello World" == "Hello"
-- | ```
-- |
dropRight :: Int -> String -> String
dropRight i s = take (length s - i) s

-- | Returns the suffix remaining after `takeWhile`.
-- |
-- | ```purescript
-- | dropWhile (_ /= '.') "Test.purs" == ".purs"
-- | ```
-- |
dropWhile :: (Char -> Boolean) -> String -> String
dropWhile p s = drop (countPrefix p s) s

-- | Returns the substring at indices `[begin, end)`.
-- | If either index is negative, it is normalised to `length s - index`,
-- | where `s` is the input string. `Nothing` is returned if either
-- | index is out of bounds or if `begin > end` after normalisation.
-- |
-- | ```purescript
-- | slice 0 0   "purescript" == Just ""
-- | slice 0 1   "purescript" == Just "p"
-- | slice 3 6   "purescript" == Just "esc"
-- | slice (-4) (-1) "purescript" == Just "rip"
-- | slice (-4) 3  "purescript" == Nothing
-- | ```
slice :: Int -> Int -> String -> Maybe String
slice b e s = if b' < 0 || b' >= l ||
                 e' < 0 || e' >= l ||
                 b' > e'
              then Nothing
              else Just (_slice b e s)
  where
    l = length s
    norm x | x < 0 = l + x
           | otherwise = x
    b' = norm b
    e' = norm e

foreign import _slice :: Int -> Int -> String -> String

-- | Splits a string into two substrings, where `before` contains the
-- | characters up to (but not including) the given index, and `after` contains
-- | the rest of the string, from that index on.
-- |
-- | ```purescript
-- | splitAt 2 "Hello World" == { before: "He", after: "llo World"}
-- | splitAt 10 "Hi" == { before: "Hi", after: ""}
-- | ```
-- |
-- | Thus the length of `(splitAt i s).before` will equal either `i` or
-- | `length s`, if that is shorter. (Or if `i` is negative the length will be
-- | 0.)
-- |
-- | In code:
-- | ```purescript
-- | length (splitAt i s).before == min (max i 0) (length s)
-- | (splitAt i s).before <> (splitAt i s).after == s
-- | splitAt i s == {before: take i s, after: drop i s}
-- | ```
foreign import splitAt :: Int -> String -> { before :: String, after :: String }
