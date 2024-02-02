{-# LANGUAGE DataKinds #-}

module Main where

import Graphics.Image as I
import Graphics.Image.IO
import Numeric (showHex)
import Prelude as P
import qualified Data.Foldable as P
import qualified Data.Function as P
import qualified Data.Ord as O
import qualified Data.List as L

main :: IO ()
main = do
  let imgPath = "/home/jasper/Pictures/wallpapers/tree.jpeg"
  img <- readImageRGB VS imgPath

  let img' = resize Nearest Edge (128, 128) img

  -- writeImage "b.png" img'

  let hsi = toHSI img'

  let sat = dominantSat hsi
  let terminalCols = getTerminalColours sat hsi

  print $ averageI hsi

  let primaryHue = dominantHue hsi

  let primary = P.zip (P.map (fromHSI primaryHue sat) [1.0, 0.8, 0.5, 0.2, 0.1]) ["primary-lightest", "primary-light", "primary-dark", "primary-darker", "primary-darkest"]


  let t = [] :: Theme
  let t' = appendColours t terminalCols
  let t'' = appendColours t' primary

  let theme = serializeTheme t'' imgPath

  putStrLn theme
  writeFile "/home/jasper/.config/theme.txt" theme 

-- 1. Find peak colours
-- 2. 

-- Highly saturated colours will be accent colours
-- Less saturated colours will be background colours
-- The most saturated colour will be the accent colour
-- The most prominent colour will be the background colour
-- A lightness inverted version of the background colour will be the main text colour
-- All colours will be desaturated to some extent
-- If very unique accent, different background colours can be used for terminal, window manager, etc. if
-- there are several important colours
-- terminal colours must all be the same saturation except for the primary colour
-- use blue for main colour cause arch ect.

dominantSat :: Img -> Double
dominantSat img = mode $ filter (/= 0.0) hs
  where
    hs = P.map sat img

dominantHue :: Img -> Double
dominantHue img = mode $ filter (/= 0.0) hs
  where
    hs = P.map hue img

mode :: Ord a => [a] -> a
mode = head . P.maximumBy (O.comparing length) . L.group . L.sort

fromHSI :: Double -> Double -> Double -> C
fromHSI h s i = fromHSIPix $ PixelHSI h s i

type Img = [Pixel HSI Double]

getTerminalColours :: Double -> Img -> [(C, String)]
getTerminalColours sat img = concatMap f hues
  where
    f (h, n) = [light h n, dark h n]
    light h n = (fromHSI h sat 0.7, n ++ "-light")
    dark h n = (fromHSI h sat 0.5, n ++ "-dark")
    hues = getTerminalHues img

getTerminalHues :: Img -> [(Double, String)]
getTerminalHues img = P.map f terminalHues
  where
    f (h, n) = (closest h hs, n)
    hs = P.map hue img

averageSat :: Img -> Double
averageSat img = averageChannel sat img'
  where
    img' = filter (iIsInRange . i) img
    iIsInRange i = i > 0.15


averageHue :: Img -> Double
averageHue = averageChannel hue

averageI :: Img -> Double
averageI = averageChannel i

averageChannel :: (Pixel HSI Double -> Double) -> Img -> Double
averageChannel c img = P.sum sats / fromIntegral (length sats)
  where
    sats = P.map c img

closest :: Double -> [Double] -> Double
closest needle = P.minimumBy (P.on P.compare diff)
  where
    diff' x = abs $ x - needle
    diff x = min (diff' x) (diff' (x + 1.0))

hue :: Pixel HSI Double -> Double
hue p = h
  where
    PixelHSI h _ _ = toPixelHSI p

sat :: Pixel HSI Double -> Double
sat p = s
  where
    PixelHSI _ s _ = toPixelHSI p

i :: Pixel HSI Double -> Double
i p = s
  where
    PixelHSI _ s _ = toPixelHSI p

fromHSIPix :: Pixel HSI Double -> C
fromHSIPix ps = C {colr = i r, colg = i g, colb = i b}
  where
    rgbPix = toPixelRGB ps
    PixelRGB r g b = rgbPix
    i x = round (clamp x * 255.0)
    clamp x
      | x < 0.0 = 0.0
      | x > 1.0 = 1.0
      | otherwise = x

terminalHues = P.map f terminalHues'
  where
    f (h, n) = (h / 360.0, "term-" ++ n)

terminalHues' =
  [ (0, "red"),
    (120, "green"),
    (60, "yellow"),
    (237, "blue"),
    (327, "magenta"),
    (193, "cyan")
  ] ::
    [(Double, String)]

toHSI :: Image VS RGB Double -> Img
toHSI img = concat $ toLists $ toImageHSI img

type Theme = [ThemeColour]

data ThemeColour = ThemeColour
  { themeColourName :: String,
    themeColorHex :: C
  }
  deriving (Show)

appendColour :: Theme -> C -> String -> Theme
appendColour t c n = t ++ [ThemeColour {themeColourName = n, themeColorHex = c}]

appendColours :: Theme -> [(C, String)] -> Theme
appendColours t [] = t
appendColours t ((c, n) : cs) = appendColours (appendColour t c n) cs

serializeTheme :: Theme -> String -> String
serializeTheme theme img = "img = " ++ img ++ "\n" ++ (unlines . P.map sc) theme
  where
    sc tc = themeColourName tc ++ " = " ++ toHex (themeColorHex tc)

data C = C
  { colr :: Int,
    colg :: Int,
    colb :: Int
  }
  deriving (Show)

toHex :: C -> String
toHex c = "#" ++ h (colr c) ++ h (colg c) ++ h (colb c)
  where
    h x = if x < 16 then "0" ++ h' x else h' x
    h' x = showHex x ""
