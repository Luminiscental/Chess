module Chess.Util
    ( mkArray
    , packString
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

-- | Utility to make an array given a function from index to value.
mkArray :: (Ix a) => (a -> b) -> (a, a) -> Array a b
mkArray mkElem ixRange =
    array ixRange [ (ix, mkElem ix) | ix <- range ixRange ]

packString :: String -> ByteString
packString = ByteString.pack . map (fromIntegral . Char.ord)
