module Main where

import Linear (
  V2(V2), V3(V3))
import Codec.Picture (
  Image, Pixel8, generateImage, writePng)


type ScalarField2 a = V2 a -> a

-- Write image

resolution :: Int
resolution = 600

imageToCameraSpace :: V2 Int -> V2 Double
imageToCameraSpace (V2 i1 i2) = do
  let x1 = fromIntegral i1 / fromIntegral resolution * 2 - 1
  let x2 = fromIntegral i2 / fromIntegral resolution * 2 - 1
  V2 x1 x2

discreteGrey :: Double -> Pixel8
discreteGrey a
  | a < 0 = 0
  | a > 1 = 255
  | otherwise = round (a * 255)

manifestImage :: ScalarField2 Double -> Image Pixel8
manifestImage rendering = do
  let imageFunction i1 i2 = discreteGrey (rendering (imageToCameraSpace (V2 i1 i2)))
  generateImage imageFunction resolution resolution

main :: IO ()
main = do

  let testImage = \(V2 i1 i2) -> if abs (i1 - i2) > 0.2 then 0.8 else 0.1

  writePng "out/testImage.png" (manifestImage testImage)
