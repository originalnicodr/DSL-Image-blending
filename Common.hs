module Common where
import Graphics.Image as I

data Name
   =  Global  String
  deriving (Show, Eq)

-- Términos con nombres
data LamTerm  =  LVar String
              |  Abs String LamTerm
              |  App LamTerm LamTerm
              |  LIC String
              |  LBinOp Op LamTerm LamTerm
              |  LUnOp UOp LamTerm Double
              |  LComplement LamTerm
              deriving Show

data Term  = Bound Int
           | Free Name
           | Term :@: Term
           | Lam Term --No se pone la variable por que no es necesario, ya se guardo con las otras variables al realizar la conversion
           | IC String
           | BinOp Op Term Term--(Pixel RGBA Double->Pixel RGBA Double->Pixel RGBA Double) Term Term
           | UnOp UOp Term Double--(Double->Pixel RGBA Double->Pixel RGBA Double) Term Double
           | Complement Term
           deriving Show

-- Modos de blending (funciones de tipo Image->Image->Image)
data Op = Normal
        | Add
        | Diff
        | Darken
        | Lighten
        | Multiply
        | Screen
        | Overlay
        | HardLight
        | SoftLight
        | ColorDodge
        | ColorBurn
        | Hue
        | Luminosity
        | BlendColor
        | BlendSat
        | Exclusion
        deriving Show

--Modos de edicion (funciones de tipo Image->Double->Image)
data UOp = Temp --entre 1000 y 40000
         | Sat --entre -1 y 1
         | Exposure --entre -1 y 1
         | Contrast-- entre -1 y 4
         | Shadows-- entre -1 y 1
         | Highlights-- entre -1 y 1
         | Opacity--entre -1 y 1
         deriving Show

-- ///////////////////////////////////////////////
-- // Funciones de blending en las componentes/pixeles
-- ///////////////////////////////////////////////
normal::Double -> Double -> Double
normal a b = b
add::Double -> Double -> Double
add a b= a+b

multiply::Double -> Double -> Double
multiply a b = a*b

overlay::Double -> Double -> Double
overlay a b = if (a <= 0.5) then multiply a (clamp (2*b)) else 1 - (1-a)*(1-(clamp (2*b-1)))

hardlight::Double -> Double -> Double
hardlight a b = overlay b a

darken::Double -> Double -> Double
darken a b = min a b

lighten::Double -> Double -> Double
lighten a b = max a b

colordodge::Double -> Double -> Double
colordodge a b = if b==1 then 1 else min 1 (a/(1-b))

softlight::Double -> Double -> Double
softlight a b = if (b <= 0.5) then a - (1-2*b)*a*(1-a) else a + (2*b-1)*((d a) - a)
                where d x = if (x>0.25) then sqrt x else ((16*x-12)*x+4)*x

difference::Double -> Double -> Double
difference a b = abs (a-b)

exclusion::Double -> Double -> Double
exclusion a b = a + b -2*a*b

--Voy a necesitar usar todo el Pixel
hue:: Pixel RGBA Double -> Pixel RGBA Double -> Pixel RGBA Double
hue a@(PixelRGBA r1 g1 b1 a1') b@(PixelRGBA r2 g2 b2 a2') = let PixelHSIA h1 s1 i1 a1 = toPixelHSIA a
                                                              in let PixelHSIA h2 s2 i2 a2 = toPixelHSIA b
                                                                  in let PixelRGBA r g b a'= toPixelRGBA (PixelHSIA h2 s1 i1 a1)
                                                                      in PixelRGBA (r*a2+r1*a1*(1-a2)) (g*a2+g1*a1*(1-a2)) (b*a2+b1*a1*(1-a2)) (a2+a1*(1-a2))--Los calculos finales con alpha

blendsat:: Pixel RGBA Double -> Pixel RGBA Double -> Pixel RGBA Double
blendsat a@(PixelRGBA r1 g1 b1 a1') b@(PixelRGBA r2 g2 b2 a2') = let PixelHSIA h1 s1 i1 a1 = toPixelHSIA a
                                                              in let PixelHSIA h2 s2 i2 a2 = toPixelHSIA b
                                                                  in let PixelRGBA r g b a'= toPixelRGBA (PixelHSIA h1 s2 i1 a1)
                                                                      in PixelRGBA (r*a2+r1*a1*(1-a2)) (g*a2+g1*a1*(1-a2)) (b*a2+b1*a1*(1-a2)) (a2+a1*(1-a2))--Los calculos finales con alpha




blendcolor::Pixel RGBA Double -> Pixel RGBA Double -> Pixel RGBA Double
blendcolor a@(PixelRGBA r1 g1 b1 a1') b@(PixelRGBA r2 g2 b2 a2') = let PixelHSIA h1 s1 i1 a1 = toPixelHSIA a
                                                                    in let PixelHSIA h2 s2 i2 a2 = toPixelHSIA b
                                                                        in let PixelRGBA r g b a'= toPixelRGBA (PixelHSIA h2 s2 i1 a1)
                                                                            in PixelRGBA (r*a2+r1*a1*(1-a2)) (g*a2+g1*a1*(1-a2)) (b*a2+b1*a1*(1-a2)) (a2+a1*(1-a2))--Los calculos finales con alpha

luminosity:: Pixel RGBA Double -> Pixel RGBA Double -> Pixel RGBA Double
luminosity a@(PixelRGBA r1 g1 b1 a1') b@(PixelRGBA r2 g2 b2 a2') = let PixelHSIA h1 s1 i1 a1 = toPixelHSIA a
                                                                    in let PixelHSIA h2 s2 i2 a2 = toPixelHSIA b
                                                                        in let PixelRGBA r g b a'= toPixelRGBA (PixelHSIA h1 s1 i2 a1)
                                                                            in PixelRGBA (r*a2+r1*a1*(1-a2)) (g*a2+g1*a1*(1-a2)) (b*a2+b1*a1*(1-a2)) (a2+a1*(1-a2))--Los calculos finales con alpha

-- ///////////////////////////////////////////////
-- // Funciones de edicion en las componentes/pixeles
-- ///////////////////////////////////////////////

--Funciones auxiliares
----------------------------------------------------------------------------------
clamp x = clamp2 x 0 1

clamp2 x a b=if x<a then a else (if x>b then b else x)

lerp a b t= t*a + (1-t)*b

getLuminance (PixelRGBA r g b a) = r*0.212656+g*0.715158+b*0.072186

curve x = x^3*(x*(x*6-15)+10)

--Funcion map que no me mapea el canal alpha
rgbamap f (PixelRGBA r1 g1 b1 a1) = (PixelRGBA (f r1) (f g1) (f b1) a1)
----------------------------------------------------------------------------------

saturation::Double->Pixel RGBA Double -> Pixel RGBA Double
saturation d a= let PixelHSIA h1 s1 i1 a1=(toPixelHSIA a)
                  in toPixelRGBA (PixelHSIA h1 (clamp s1*(d+1)) i1 a1)--el +1 es para mantener los valores posibles entre -1 y 1

contrast::Double->Pixel RGBA Double -> Pixel RGBA Double
contrast d a= let factor=(1.0157 * (d + 1)) / (1 * (1.0157 - d)) --d entre -1 y 1
               in rgbamap (\v -> factor * (v  - 0.5) + 0.5) a

shadows::Double->Pixel RGBA Double -> Pixel RGBA Double
shadows d c@(PixelRGBA r g b a)=let pLuma = getLuminance c--max (max r g) b--valor de luminosidad
                                in let weight= (curve (max (1-pLuma*2) 0))*d
                                    in (exposure weight c)--bri weight (contrast weight (exposure weight c))

highlights::Double->Pixel RGBA Double -> Pixel RGBA Double
highlights d c@(PixelRGBA r g b a)=let pLuma = getLuminance c--max (max r g) b--valor de luminosidad
                                    in let weight= (curve (max (( pLuma - 0.5) * 2) 0))*d
                                        in (exposure weight c)--bri weight (contrast weight (exposure weight c))

exposure::Double->Pixel RGBA Double -> Pixel RGBA Double
exposure d a=rgbamap (\y -> clamp((1+d)*y)) a--le sumo 1 a d para manener el neutro en 0

opacity::Double->Pixel RGBA Double -> Pixel RGBA Double
opacity d (PixelRGBA r g b a) = (PixelRGBA r g b (clamp (a+d)))--modifica la opacidad uniformemente (y la satura)

kelvinToRGBA::Double->Pixel RGBA Double
kelvinToRGBA k =let kelvin=(clamp2 k 1000 40000)/100
                in if (kelvin>66) then (PixelRGBA (clamp (1.29293618606274509804 * (kelvin - 60)**(-0.1332047592))) (clamp (1.12989086089529411765 * (kelvin - 60)**(-0.0755148492))) 1 1)
                                  else if (kelvin==66) then (PixelRGBA 1 (clamp (0.39008157876901960784 * log( kelvin ) - 0.63184144378862745098)) 1 1)
                                                         else if (kelvin<19) then (PixelRGBA 1 (clamp (0.39008157876901960784 * log( kelvin ) - 0.63184144378862745098)) 0 1)
                                                                               else (PixelRGBA 1 (clamp (0.39008157876901960784 * log( kelvin ) - 0.63184144378862745098)) (clamp (0.54320678911019607843 * log( kelvin - 10) - 1.19625408914)) 1)
temp::Double->Pixel RGBA Double -> Pixel RGBA Double
temp d c@(PixelRGBA r g b a) = let (PixelRGBA kr kg kb ka) = kelvinToRGBA d--entre 1000 y 40000
                                in let (PixelHSIA h s i la)=toPixelHSIA c
                                    in let (PixelRGBA br bg bb ba)=(PixelRGBA (r*kr) (g*kg) (b*kb) a)--blended
                                        in let (PixelHSIA hbr hbg hbb hba)=toPixelHSIA (PixelRGBA br bg bb ba)
                                            in (lerp (PixelRGBA br bg bb ba) (toPixelRGBA (PixelHSIA hbr hbg i hba)) 1)--se puede cambiar ese ultimo 1 por un

--Funcion aplicada en Complement
opposite::Pixel RGBA Double -> Pixel RGBA Double
opposite= (\(PixelRGBA r g b a) -> (PixelRGBA (1-r) (1-g) (1-b) a))

-- ///////////////////////////////////////////////
-- // Modos de blending y edicion aplicado a imagenes
-- ///////////////////////////////////////////////

--Para dos argumentos, mapeo y combino
blendpixel:: (Double -> Double -> Double) -> Pixel RGBA Double -> Pixel RGBA Double -> Pixel RGBA Double
blendpixel f (PixelRGBA r1 g1 b1 a1) (PixelRGBA r2 g2 b2 a2)= (PixelRGBA ((f r1 r2)*a2+r1*a1*(1-a2)) ((f g1 g2)*a2+g1*a1*(1-a2)) ((f b1 b2)*a2+b1*a1*(1-a2)) (a2+a1*(1-a2)))

--Toma la funcion que utilizara para mezclar los pixeles de dos imagenes
blend:: (MArray arr cs1 e1, Array arr1 cs e, Array arr1 cs' e') =>  (Pixel cs' e' -> Pixel cs1 e1 -> Pixel cs e)  -> Image arr1 cs' e'   -> Image arr cs1 e1   ->  Image arr1 cs e
blend f im1 im2= I.imap (\(i,j) p1 -> f p1 (index im2 (i,j))) im1

--Toma la funcion que utilizara para aplicar sobre la imagen utilizando un double como argumento
edit :: (Array arr cs e, Array arr cs' e') =>   (Double -> Pixel cs' e' -> Pixel cs e) -> Image arr cs' e' -> Double -> Image arr cs e
edit f im d= (I.map (f d) im)

--Funciones primitivas
---------------------------------------------------------------------------

-- Operaciones de blending
normalop::     Image VU RGBA Double -> Image VU RGBA Double -> Image VU RGBA Double
normalop      = blend (blendpixel normal)
addop::        Image VU RGBA Double -> Image VU RGBA Double -> Image VU RGBA Double
addop         = blend (blendpixel add)
diffop::       Image VU RGBA Double -> Image VU RGBA Double -> Image VU RGBA Double
diffop        = blend (blendpixel difference)
darkenop::     Image VU RGBA Double -> Image VU RGBA Double -> Image VU RGBA Double
darkenop      = blend (blendpixel darken)
lightenop::    Image VU RGBA Double -> Image VU RGBA Double -> Image VU RGBA Double
lightenop     = blend (blendpixel lighten)
multiplyop::   Image VU RGBA Double -> Image VU RGBA Double -> Image VU RGBA Double
multiplyop    = blend (blendpixel multiply)
overlayop::    Image VU RGBA Double -> Image VU RGBA Double -> Image VU RGBA Double
overlayop     = blend (blendpixel overlay)
softlightop::  Image VU RGBA Double -> Image VU RGBA Double -> Image VU RGBA Double
softlightop   = blend (blendpixel softlight)
colordodgeop:: Image VU RGBA Double -> Image VU RGBA Double -> Image VU RGBA Double
colordodgeop  = blend (blendpixel colordodge)
hueop::        Image VU RGBA Double -> Image VU RGBA Double -> Image VU RGBA Double
hueop         = blend hue
luminosityop:: Image VU RGBA Double -> Image VU RGBA Double -> Image VU RGBA Double
luminosityop  = blend luminosity
blendcolorop:: Image VU RGBA Double -> Image VU RGBA Double -> Image VU RGBA Double
blendcolorop  = blend blendcolor
blendsatop::   Image VU RGBA Double -> Image VU RGBA Double -> Image VU RGBA Double
blendsatop    = blend blendsat
exclusionop::  Image VU RGBA Double -> Image VU RGBA Double -> Image VU RGBA Double
exclusionop   = blend (blendpixel exclusion)
hardlightop::  Image VU RGBA Double -> Image VU RGBA Double -> Image VU RGBA Double
hardlightop   = blend (blendpixel hardlight)

-- Operaciones de edicion
tempop::       Image VU RGBA Double -> Double -> Image VU RGBA Double
tempop       = edit temp
satop::        Image VU RGBA Double -> Double -> Image VU RGBA Double
satop        = edit saturation
exposureop::   Image VU RGBA Double -> Double -> Image VU RGBA Double
exposureop   = edit exposure
contrastop::   Image VU RGBA Double -> Double -> Image VU RGBA Double
contrastop   = edit contrast
shadowsop::    Image VU RGBA Double -> Double -> Image VU RGBA Double
shadowsop    = edit shadows
highlightsop:: Image VU RGBA Double -> Double -> Image VU RGBA Double
highlightsop = edit highlights
opacityop::    Image VU RGBA Double -> Double -> Image VU RGBA Double
opacityop    = edit opacity

oppositeop::   Image VU RGBA Double -> Image VU RGBA Double
oppositeop    = I.map opposite
---------------------------------------------------------------------------

--Funciones derivadas
screenop::     Image VU RGBA Double -> Image VU RGBA Double -> Image VU RGBA Double
screenop      = (\x y ->oppositeop (multiplyop (oppositeop x) (oppositeop y)))
colorburnop::  Image VU RGBA Double -> Image VU RGBA Double -> Image VU RGBA Double
colorburnop   = (\x y ->oppositeop (colordodgeop (oppositeop x) (oppositeop y)))
---------------------------------------------------------------------------
