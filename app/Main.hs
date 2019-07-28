module Main where

import Linear (
  V2(V2), V3(V3), (^+^), (^-^), (*^))
import Data.Complex (
  Complex((:+)))
import Codec.Picture (
  Image, Pixel8, generateImage, writePng)


data Edge2 a = Edge2 (V2 a) (V2 a)
  deriving (Show, Eq, Ord)


type ScalarField2 a = V2 a -> a

renderE0 :: Floating a => Ord a => ScalarField2 a
renderE0 (V2 x1 x2)
  | x1 < 0 = 0
  | x1 > 1 = 0
  | otherwise = x2 * exp (negate (sqr x2))

sqr :: Num a => a -> a
sqr x = x * x


cotransformScalarField2 :: (V2 a -> V2 a) -> ScalarField2 a -> ScalarField2 a
cotransformScalarField2 g f = \x -> f (g x)

translate2 :: Num a => V2 a -> V2 a -> V2 a
translate2 = (^+^)

rotate2 :: RealFloat a => Complex a -> V2 a -> V2 a
rotate2 r (V2 x1 x2) = do
  let y1 :+ y2 = r * (x1 :+ x2)
  V2 y1 y2

scale22 :: Num a => a -> V2 a -> V2 a
scale22 s (V2 x1 x2) = V2 x1 (s * x2)


renderEdge2 :: RealFloat a => Ord a => Edge2 a -> ScalarField2 a
renderEdge2 (Edge2 a b) = do
  let w = 0.1
  let V2 c1 c2 = b ^-^ a
  let r = c1 :+ c2
  cotransformScalarField2 (
    scale22 (recip w) . rotate2 (recip r) . translate2 (negate a)) renderE0


-- Write image

resolution :: Int
resolution = 600

imageToCameraSpace :: V2 Int -> V2 Double
imageToCameraSpace (V2 i1 i2) = do
  let x1 = fromIntegral i1 / fromIntegral resolution * 2 - 1
  let x2 = 1 - fromIntegral i2 / fromIntegral resolution * 2
  V2 x1 x2

discreteGrey :: Double -> Pixel8
discreteGrey a
  | a < -1 = 0
  | a > 1 = 255
  | otherwise = round ((0.5 + 0.5 * a) * 255)

manifestImage :: ScalarField2 Double -> Image Pixel8
manifestImage rendering = do
  let imageFunction i1 i2 = discreteGrey (rendering (imageToCameraSpace (V2 i1 i2)))
  generateImage imageFunction resolution resolution

main :: IO ()
main = do

  let testImage = renderEdge2 (Edge2 (V2 (-0.8) 0.3) (V2 0.4 0.8))

  writePng "out/testImage.png" (manifestImage testImage)
