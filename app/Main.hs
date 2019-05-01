{-# LANGUAGE DeriveDataTypeable, RecordWildCards, OverloadedStrings #-}
module Main where

import Graphics.Rendering.TrueType.STB
import Data.Array.Unboxed (UArray)
import Control.Monad
import Data.Array.IArray
import Data.Word
import Data.Char (ord)
import Data.List (intercalate)
import Numeric (showHex)
import System.Console.CmdArgs

data Options = Options
    { size    :: Float
    , files   :: [FilePath]
    } deriving (Show, Eq, Data, Typeable)

options :: Main.Options
options = Main.Options
    { size = def &= help "bitmap height"
    , files = def &= args &= typ "FILES"
    } &=
    verbosity &=
    help "Rasterize TrueType fonts for embedded use" &=
    summary "Fonterizer v0.0.0, (c) Bengt Marten Agren 2019" &=
    details [ "Fonterizer converts a TrueType font to C++ bitmap definitions."
            ]

main :: IO ()
main = do
    opts@Options{..} <- cmdArgs $ options { size = 20 }
    mapM_ putStrLn preamble
    mapM_ (process size) files

data GlyphData = GlyphData
    { char      :: Char
    , width     :: Int
    , height    :: Int
    , offsetH   :: Int
    , offsetV   :: Int
    , bitmap    :: UArray (Int, Int) Word8
    }

preamble :: [String]
preamble =
    [ "#include <fontlib.h>"
    , ""
    , "namespace fontlib"
    , "{"
    , ""
    ]

process :: Float -> FilePath -> IO ()
process size file = do
    tt <- loadTTF file
    (i:_) <- enumerateFonts tt
    font <- initFont tt i
    let cs = [' '..'Z']
    xs <- mapM (glyphData font size) cs
    mapM_ (putStrLn . unlines . bitmapDecl) xs
    putStrLn $ "static glyph_t glyphs[" <> show (length cs) <> "] ="
    mapM_ putStrLn $ map indent $ initializer $ map glyphDecl xs
    let fontDecl =
            [ show $ round size
            , show $ ord (head cs)
            , show $ ord (last cs)
            , "glyphs"
            ]
    putStrLn ""
    putStrLn $ "font_t font = {" <> (intercalate ", " fontDecl) <> " };"
    putStrLn ""
    putStrLn "} // fontlib"

glyphData :: Font -> Float -> Char -> IO GlyphData
glyphData font size char = do
    Just glyph <- findGlyph font char
    vmu <- getFontVerticalMetrics font
    let s = scaleForPixelHeight vmu size
    (bitmap', (offsetH, offsetV)) <- newGlyphBitmap font glyph (s, s)
    let (width, height) = bitmapSize bitmap'
    bitmap <- bitmapArray bitmap'
    return GlyphData{..}

glyphDecl :: GlyphData -> String
glyphDecl GlyphData{..} = "{ " <> intercalate ", "
    [ show width
    , show height
    , show offsetH
    , show offsetV
    , bitmapName char
    ] <> " }" <> " // '" <> [ char ] <> "'"

bitmapDecl :: GlyphData -> [String]
bitmapDecl GlyphData{..}  = 
    [ "// '" <> [ char ] <> "'"
    , "static uint8_t " <> bitmapName char <> "[] ="
    ] ++ (map indent $ initializer $ map row [0..height-1])
    where row r = intercalate ", " $ map (col r) [0..width-1]
          col r c = hex $ bitmap!(r, c)

bitmapName :: Char -> String
bitmapName char = "bitmap_" <> (show $ ord char)

showBitMap :: (Int, Int) -> UArray (Int, Int) Word8 -> IO ()
showBitMap (w, h) bs = do
    forM_ [0..h-1] $ \r -> do
        putStr "//    "
        forM_ [0..w-1] $ \c -> do
            putStr $ grayLevel $ bs!(r, c)
        putStrLn ""

grayLevel :: Word8 -> String
grayLevel x = show $ x `div` 32

hex :: Word8 -> String
hex x = "0x" ++ showHex x ""

initializer :: [String] -> [String]
initializer [] = [ "{", "};" ]
initializer (x:xs) = "{ " <> x : map (", "<>) xs ++ [ "};" ]

indent :: String -> String
indent = ("    "<>)

