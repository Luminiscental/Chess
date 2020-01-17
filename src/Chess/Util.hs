{-|
Module      : Chess.Util
Description : Various utility functions.

This module contains generic utility functions used throughout the library that aren't specific
to the domain of the library.
-}
module Chess.Util
    ( mkArray
    , packString
    , rangeExclusive
    , rangeInclusive
    , disambiguate
    )
where

import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as ByteString
import qualified Data.Char                     as Char
import qualified Data.List                     as List
import           Data.Array.IArray              ( Ix
                                                , Array
                                                , array
                                                , range
                                                )

-- | Utility to make an 'Array' given a function from index to value.
mkArray :: (Ix a) => (a -> b) -> (a, a) -> Array a b
mkArray mkElem ixRange =
    array ixRange [ (ix, mkElem ix) | ix <- range ixRange ]

-- | Utility to convert a 'String' to a 'ByteString'.
packString :: String -> ByteString
packString = ByteString.pack . map (fromIntegral . Char.ord)

-- | Return a list of integers between upper and lower exclusive bounds.
-- The function is order indifferent of the bounds:
--
-- prop> rangeExclusive a b = rangeExclusive b a
rangeExclusive :: Int -> Int -> [Int]
rangeExclusive a b = [small + 1 .. big - 1]
  where
    small = min a b
    big   = max a b

-- | Return a list of integers between upper and lower inclusive bounds.
-- The function is order indifferent of the bounds:
--
-- prop> rangeInclusive a b = rangeInclusive b a
rangeInclusive :: Int -> Int -> [Int]
rangeInclusive a b = [small .. big]
  where
    small = min a b
    big   = max a b

-- | Disambiguate elements of a list; given a list of equating functions paired with output
-- functions, maps each element of the list by the output function corresponding to the first
-- equating function that it is unique in.
--
-- For example:
--
-- >>> disambiguate [((==) `on` fst, const 0), ((==) `on` snd, snd)] [(1, 3), (2, 4), (2, 5)]
-- [0, 4, 5]
--
-- >>> disambiguate [((==), negate), (\_ _ -> False, id)] [1, 3, 2, 1, 1]
-- [1, -3, -2, 1, 1]
disambiguate :: [(a -> a -> Bool, a -> b)] -> [a] -> [b]
disambiguate stages = map <$> firstUnique stages <*> id
  where
    firstUnique [] _ _ = error "Couldn't disambiguate list"
    firstUnique ((eqFn, outFn) : rest) list value =
        if (> 1) . length . filter (eqFn value) $ list
            then firstUnique rest list value
            else outFn value
