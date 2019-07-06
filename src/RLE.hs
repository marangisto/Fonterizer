module RLE(encode) where

import Data.Word
import Data.Bits
import Data.List (groupBy)
import Data.List.Split (chunksOf)

encode :: [Word8] -> [Word8]
encode = map f . concatMap (chunksOf 8) . groupBy (==) . map (`shift` 3)
    where f xs@(x:_)
              | n <= 8 = x .|. fromIntegral n
              | otherwise = error "run too long"
              where n = length xs

