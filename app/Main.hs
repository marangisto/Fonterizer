module Main where

import Graphics.Rendering.TrueType.STB
import Control.Monad

main :: IO ()
main = do
--    tt <- loadTTF "/Users/marten/Downloads/cmu-typewriter/Typewriter/cmuntt.ttf"
    tt <- loadTTF "/Users/marten/Downloads/22815_LCOUR.ttf"
    (i:_) <- enumerateFonts tt
    ft <- initFont tt i
    forM_ ['a'..'z'] $ \c -> do
        Just gy <- findGlyph ft c
        vmu <- getFontVerticalMetrics ft
        let s = scaleForPixelHeight vmu 40
        (bm, bo) <- newGlyphBitmap ft gy (s, s)
        putStrLn $ c : ": " <> show (bitmapSize bm) <> " " <> show bo
    putStrLn "done!"
