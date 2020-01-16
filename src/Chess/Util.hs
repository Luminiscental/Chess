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
    , groupsFrom
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

-- | An equivalent of 'Data.List.groupBy' that doesn't only group adjacent elements, and hence
-- doesn't preserve order.
--
-- For example:
--
-- >>> groupsFrom (==) [1, 3, 2, 1, 1, 2, 2, 2]
-- [[1, 1, 1], [3], [2, 2, 2, 2]]
--
-- >>> groupBy (==) [1, 3, 2, 1, 1, 2, 2, 2]
-- [[1], [3], [2], [1, 1], [2, 2, 2]]
groupsFrom :: (a -> a -> Bool) -> [a] -> [[a]]
groupsFrom pred list = case list of
    []     -> []
    x : xs -> (x : similar) : groupsFrom pred different
        where (similar, different) = List.partition (pred x) xs

-- | Disambiguate elements of a list, grouping by a list of comparators and corresponding output
-- functions. Each element of the list is converted by the output function corresponding to the
-- first comparator that it is unique in. If the function runs out of comparators and there are
-- still elements to convert it errors. Order is not preserved!
--
-- For example:
--
-- >>> disambiguate [((==) `on` fst, const 0), ((==) `on` snd, snd)] [(1, 4), (2, 4), (2, 5)]
-- [0, 4, 5]
--
-- >>> disambiguate [((==), negate), (\_ _ -> False, id)] [1, 3, 2, 1, 1]
-- [-3, -2, 1, 1, 1]
disambiguate :: [(a -> a -> Bool, a -> b)] -> [a] -> [b]
disambiguate _  [] = []
disambiguate [] _  = error "Couldn't fully disambiguate list"
disambiguate stages list =
    let (pred, fn) : nextStages = stages
        groups                  = groupsFrom pred list
        (unique, ambiguous)     = List.partition ((== 1) . length) groups
    in  map (fn . head) unique ++ disambiguate nextStages (concat ambiguous)
