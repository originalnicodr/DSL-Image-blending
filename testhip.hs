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




fmap2 (PixelRGBA r1 g1 b1 a1)  (PixelRGBA r2 g2 b2 a2) f = (PixelRGBA (f r1 r2) (f g1 g2) (f b1 b2) a1)

--blend::(MArray arr RGB Double, Array arr1 RGB Double, Array arr1 RGB Double) =>Image arr RGB Double -> Image arr RGB Double -> ((Int, Int) -> Pixel RGB Double -> Pixel RGB Double)-> Image arr RGB Double
--blend::(MArray arr RGBA t,Array arr1 RGBA t,Array arr1 RGBA e) => Image arr1 RGBA t -> Image arr RGBA t -> (t -> t -> e) -> Image arr1 RGBA e --ATENCION: Estoy usando FlexibleContexts, sin setear eso en ghci se rompe
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

{-
grad_color = makeImageR VU (200, 200) g
  where g ::(Int, Int) -> Pixel RGB Double
        g (i,j) = PixelRGB (fromIntegral i) (fromIntegral j) (fromIntegral (i + j)) / 400
grad_colorm=I.map (fmap f) grad_color
            where f::(Double -> Double)
                  f x= x+0.5
grad_colorm2=  I.imap (\(i,j) p1 -> fmap2 p1 (index grad_colorm (i,j)) normal) grad_colorm
-}

checkx1 (a,b)= let (x1,y1)=dims a
              in let (x2,y2)=dims b
                  in if (x1>x2) then (downsample (\a -> False) (\a -> (a<(div (x1-x2) 2)) || ((div (x1-x2) 2)>a)) a, b)
                                else if(x1<x2) then (a,downsample (\a -> False) (\a -> (a<(div (x2-x1) 2)) || ((div (x2-x1) 2)>a)) b)
                                               else (a,b)
checky1 (a,b)= let (x1,y1)=dims a
              in let (x2,y2)=dims b
                  in if (y1>y2) then (downsample (\a -> (a<(div (y1-y2) 2)) || ((div (y1-y2) 2)>a)) (\a -> False) a, b)
                                else if(y1<y2) then (a, downsample (\a -> (a<(div (y2-y1) 2)) || ((div (y2-y1) 2)>a)) (\a -> False) b)
                                               else (a,b)


main1= do
        cluster <- readImageRGBA VU "cluster.jpg"
        simpson <- readImageRGBA VU "/home/nico/Desktop/F.png"
        writeImage "/home/nico/Desktop/test.jpg"  (let (x,y) = checky1 (checkx1 (cluster,simpson))
                                                    in blend x y normal) --si queres ver la imagen tenes que ejecutar main

--I.imap (\(i,j) p1 -> fmap2 p1 (index im2 (i,j)) f) im1
--if(div (x2-x1) 2)


checkx2 (a,b)= let (x1,y1)=dims a
                in let (x2,y2)=dims b
                    in if(x1<x2) then (upsample (const (0, 0)) (\ k -> if (k == 1) then (div (x2-x1) 2, 0) else if (k==x1-1)then (0,div (x2-x1) 2) else (0, 0)) a, b)
                             else if(x1>x2) then (a, upsample (const (0, 0)) (\ k -> if (k == 1) then (div (x1-x2) 2, 0) else if (k==x2-1)then (0,div (x1-x2) 2) else (0, 0)) b)
                                              else (a,b)

checky2 (a,b)= let (x1,y1)=dims a
                in let (x2,y2)=dims b
                    in if(y1<y2) then (upsample (\ k -> if (k == 1) then (div (y2-y1) 2, 0) else if (k==y1-1)then (0,div (y2-y1) 2) else (0, 0)) (const (0, 0)) a, b)
                            else if(y1>y2) then (a, upsample (\ k -> if (k == 1) then (div (y1-y2) 2, 0) else if (k==y2-1)then (0,div (y1-y2) 2) else (0, 0)) (const (0, 0)) b)
                                           else (a,b)


main2= do
        cluster <- readImageRGBA VU "cluster.jpg"
        simpson <- readImageRGBA VU "/home/nico/Desktop/F.png"
        --let (x,y) = checky2 (checkx2 (cluster,simpson))
        --  in print (dims y)
        writeImage "/home/nico/Desktop/test.jpg"  (let (x,y) = checkx2 (checky2 (cluster,simpson))
                                                    in y) --si queres ver la imagen tenes que ejecutar main
--(upsample (const (0, 0)) (\ k -> if (k == 1) then (5, 0) else if (k==snd (dims simpson)-1)then (0,5) else (0, 0))  simpson)

--main= let grad_gray = makeImageR VU (200, 200) (\(i, j) -> PixelY (fromIntegral i) / 200 * (fromIntegral j) / 200)
