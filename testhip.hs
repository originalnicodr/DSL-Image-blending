--module Testhip where
{-# LANGUAGE FlexibleContexts #-}
import Prelude as P
import Graphics.Image as I

--f (PixelRGB r g b)= PixelRGB (0) (10) (15)

{-cluster2= do
          cluster <- readImageRGB VU "cluster.jpg" --el do de monadas
          writeImage "/home/nico/Desktop/test.jpg" (I.map (fmap (+0.5)) cluster)
          displayImage (I.map (fmap (+0.5)) cluster)-}

--writeImage "/home/nico/Desktop/" grad_gray

{-main= do
        cluster <- readImageRGB VU "cluster.jpg"
        clusterm <- (I.map (fmap f) cluster) --si queres ver la imagen tenes que ejecutar main
          where f::(Double -> Double)
                f x= x+0.5
        displayImage clusterm-}



--Test que funca

{-grad_color = makeImageR VU (200, 200) g
  where g ::(Int, Int) -> Pixel RGB Double
        g (i,j) = PixelRGB (fromIntegral i) (fromIntegral j) (fromIntegral (i + j)) / 400
grad_colorm=I.map (fmap f) grad_color
            where f::(Double -> Double)
                  f x= x+0.5-}

--normal::Double -> Double -> Double
normal a b = b

--multiply::Double -> Double -> Double
multiply a b = a*b

--screen::Double -> Double -> Double
screen a b = a + b - (a*b)

--overlay::Double -> Double -> Double
overlay a b = hardlight b a

--darken::Double -> Double -> Double
darken a b = min a b

--lighten::Double -> Double -> Double
ligthen a b = max a b

--colordodge::Double -> Double -> Double
colordodge a b = if b==1 then 1 else min 1 (a/(1-b))

--colorburn::Double -> Double -> Double
colorburn a b = if b==0 then 0 else 1 - min 1 ((1-a)/b)

hardlight::Double -> Double -> Double
hardlight a b = if (b <= 0.5) then multiply a 2*b else screen a 2*b-1

--softlight::Double -> Double -> Double
softlight a b = if (b <= 0.5) then a - (1-2*b)*a*(1-a) else a + (2*b-1)*((d a) - a)
                where d x = if (x>0.25) then sqrt x else ((16*x-12)*x+4)*x

--difference::Double -> Double -> Double
difference a b = abs (a-b)

--exclusion::Double -> Double -> Double
exclusion a b = a + b -2*a*b

opposite im = I.map (fmap opp) im
              where opp x= 1-x


--Operaciones unarias




fmap2 (PixelRGB r1 g1 b1)  (PixelRGB r2 g2 b2) f = (PixelRGB (f r1 r2) (f g1 g2) (f b1 b2))

--blend::(MArray arr RGB Double, Array arr1 RGB Double, Array arr1 RGB Double) =>Image arr RGB Double -> Image arr RGB Double -> ((Int, Int) -> Pixel RGB Double -> Pixel RGB Double)-> Image arr RGB Double
blend::(MArray arr RGB t,Array arr1 RGB t,Array arr1 RGB e) => Image arr1 RGB t -> Image arr RGB t -> (t -> t -> e) -> Image arr1 RGB e --ATENCION: Estoy usando FlexibleContexts, sin setear eso en ghci se rompe
blend im1 im2 f = I.imap (\(i,j) p1 -> fmap2 p1 (index im2 (i,j)) f) im1

--edit::(MArray arr RGB t,Array arr1 RGB e) => Image arr RGB t -> (t -> Float -> e) -> Image arr1 RGB e
edit im d f = I.map (fmap f d) im

{-sat::PixelRGB->Double
sat PixelRGB r g b = (max (max r g) b) - (min (min r g) b)

clipcolor::PixelRGB->(Double, Double, Double)
clipcolor PixelRGB r g b = let l = lum (r,g,b)
                           in let n = (min (min r g) b)
                              in let x = (max (max r g) b)
                                 in
-}
--(\im1 im2 f -> (I.imap (\(i,j) p1 -> fmap2 p1 (index im2 (i,j)) f) im1))

grad_color = makeImageR VU (200, 200) g
  where g ::(Int, Int) -> Pixel RGB Double
        g (i,j) = PixelRGB (fromIntegral i) (fromIntegral j) (fromIntegral (i + j)) / 400
grad_colorm=I.map (fmap f) grad_color
            where f::(Double -> Double)
                  f x= x+0.5
grad_colorm2=  I.imap (\(i,j) p1 -> fmap2 p1 (index grad_colorm (i,j)) normal) grad_colorm



main= do
        cluster <- readImageRGB VU "cluster.jpg"
        centaurus <- readImageRGB VU "centaurus.jpg"
        writeImage "/home/nico/Desktop/test.jpg"  (blend cluster centaurus multiply) --si queres ver la imagen tenes que ejecutar main


--main= let grad_gray = makeImageR VU (200, 200) (\(i, j) -> PixelY (fromIntegral i) / 200 * (fromIntegral j) / 200)
