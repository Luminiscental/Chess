module Chess.Util
    ( mkArray
    , packString
    , rangeExclusive
    , rangeInclusive
    )
where

import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as ByteString
import qualified Data.Char                     as Char
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
