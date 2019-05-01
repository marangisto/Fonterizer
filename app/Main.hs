module Main where

import Graphics.Rendering.TrueType.STB
import Data.Array.Unboxed (UArray)
import Control.Monad
import Data.Array.IArray
import Data.Word

main :: IO ()
main = do
    tt <- loadTTF "ttf/SourceCodeVariable-Roman.ttf"
    (i:_) <- enumerateFonts tt
    ft <- initFont tt i
    forM_ [' '..'~'] $ \c -> do
        Just gy <- findGlyph ft c
        vmu <- getFontVerticalMetrics ft
        let s = scaleForPixelHeight vmu 60
        (bm, bo) <- newGlyphBitmap ft gy (s, s)
        putStrLn $ c : ": " <> show (bitmapSize bm) <> " " <> show bo
        showBitMap (bitmapSize bm) =<< bitmapArray bm
    putStrLn "done!"

showBitMap :: (Int, Int) -> UArray (Int, Int) Word8 -> IO ()
showBitMap (w, h) bs = do
    forM_ [0..h-1] $ \r -> do
        forM_ [0..w-1] $ \c -> do
            putStr $ grayLevel $ bs!(r, c)
        putStrLn ""

grayLevel :: Word8 -> String
grayLevel x = show $ x `div` 32

