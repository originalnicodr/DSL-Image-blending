module Common where
import Graphics.Image as I

data Name
   =  Global  String
  deriving (Show, Eq)

-- Términos con nombres (modificado con mi lenguaje)
data LamTerm  =  LVar String
              |  Abs String LamTerm
              |  App LamTerm LamTerm
              |  LIC String
              |  LBinOp Op LamTerm LamTerm
              |  LUnOp UOp LamTerm Double
              |  LComplement LamTerm
              deriving Show

-- Modos de blending (funciones de tipo Image->Image->Image)
data Op = Normal
        | Add
        | Diff
        | Darken
        | Lighten
        | Multiply
        | Screen--sacar
        | Overlay
        | HardLight
        | SoftLight
        | ColorDodge
        | ColorBurn--sacar
        | Hue
        | Luminosity
        | BlendColor
        | BlendSat
        | Exclusion
        deriving Show

--Funciones de tipo Image->Float->Image
data UOp = Temp --entre 1000 y 40000
         | Sat --entre 0 y 1
         | Vib
         | Exposure --entre -1 y 1
         | Contrast-- entre -1 y 1
         | Shadows
         | Highlights
         | Whites
         | Blacks
         | Opacity--entre -1 y 1
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


--Hago una funcion map que no me mapea el canal alpha
rgbamap f (PixelRGBA r1 g1 b1 a1) = (PixelRGBA (f r1) (f g1) (f b1) a1)

--Para dos argumentos, mapeo y combino
blendpixel f (PixelRGBA r1 g1 b1 a1) (PixelRGBA r2 g2 b2 a2)= (PixelRGBA ((f r1 r2)*a2+r1*a1*(1-a2)) ((f g1 g2)*a2+g1*a1*(1-a2)) ((f b1 b2)*a2+b1*a1*(1-a2)) (a2+a1*(1-a2)))

alphablend c1 cr a1 a2=(cr*a2+c1*a1*(1-a2))-- /(a2+a1*(1-a2))


--Funciones de blending

normal::Double -> Double -> Double
normal a b = b
add::Double -> Double -> Double
add a b= a+b

multiply::Double -> Double -> Double
multiply a b = a*b

screen::Double -> Double -> Double
screen a b = a + b - (a*b)

overlay::Double -> Double -> Double
overlay a b = hardlight b a

darken::Double -> Double -> Double
darken a b = min a b

lighten::Double -> Double -> Double
lighten a b = max a b

colordodge::Double -> Double -> Double
colordodge a b = if b==1 then 1 else min 1 (a/(1-b))

colorburn::Double -> Double -> Double
colorburn a b = if b==0 then 0 else 1 - min 1 ((1-a)/b)

hardlight::Double -> Double -> Double
hardlight a b = if (b <= 0.5) then multiply a (clamp (2*b)) else screen a (clamp (2*b-1))

softlight::Double -> Double -> Double
softlight a b = if (b <= 0.5) then a - (1-2*b)*a*(1-a) else a + (2*b-1)*((d a) - a)
                where d x = if (x>0.25) then sqrt x else ((16*x-12)*x+4)*x

difference::Double -> Double -> Double
difference a b = abs (a-b)

exclusion::Double -> Double -> Double
exclusion a b = a + b -2*a*b

--Voy a necesitar usar todo el Pixel
--Se puede escribir mas lindo si se tuviera funciones para acceder a las componentes
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
--Funciones de edicion

--Funciones auxiliares
clamp x = clamp2 x 0 1
clamp2 x a b=if x<a then a else (if x>b then b else x)
lerp a b t= t*a + (1-t)*b
getLuminance (PixelRGBA r g b a) = r*0.212656+g*0.715158+b*0.072186
curve x = x^3*(x*(x*6-15)+10)

saturation::Double->Pixel RGBA Double -> Pixel RGBA Double
saturation d a= let PixelHSIA h1 s1 i1 a1=(toPixelHSIA a)--d de 0 a 1
                  in toPixelRGBA (PixelHSIA h1 (clamp s1*d) i1 a1)

vib::Double->Pixel RGBA Double -> Pixel RGBA Double
vib d c@(PixelRGBA r g b a)= let (x,y)=(min (min r g) b,max (max r g) b)--No me convence
                             in let z= y-x
                                in rgbamap (\v -> lerp (getLuminance c) v (1+d*(1-z))) c

contrast::Double->Pixel RGBA Double -> Pixel RGBA Double
contrast d a= let factor=(1.0157 * (d + 1)) / (1 * (1.0157 - d)) --d entre -1 y 1
               in rgbamap (\v -> factor * (v  - 0.5) + 0.5) a

blacks d a=undefined
--highlights d c@(PixelRGBA r g b a)= if (r>0.7 && g>0.7 && b>0.7)then (PixelRGBA (clamp (r*d)) (clamp(g*d)) (clamp(b*d)) a) else c
--(PixelRGBA (clamp (r*d)) (clamp(g*d)) (clamp(b*d)) a)

bri d a=rgbamap (\y -> clamp(d*y)) a
--hace lo mismo (creo)
{-
highlights d a= let PixelHSIA h1 s1 i1 a1=(toPixelHSIA a)--d de 0 a 1
                  in toPixelRGBA (PixelHSIA h1 s1 (clamp i1*d) a1)
-}

shadows::Double->Pixel RGBA Double -> Pixel RGBA Double
shadows d c@(PixelRGBA r g b a)=let pLuma = getLuminance c--max (max r g) b--valor de luminosidad
                                in let weight= (curve (max (1-pLuma*2) 0))*d
                                    in (exposure weight c)--bri weight (contrast weight (exposure weight c))

highlights::Double->Pixel RGBA Double -> Pixel RGBA Double
highlights d c@(PixelRGBA r g b a)=let pLuma = getLuminance c--max (max r g) b--valor de luminosidad
                                    in let weight= (curve (max (( pLuma - 0.5) * 2) 0))*d
                                        in (exposure weight c)--bri weight (contrast weight (exposure weight c))

--blacks d a=undefined
whites::Double->Pixel RGBA Double -> Pixel RGBA Double
whites d c@(PixelRGBA r g b a)= if(r>0.9 && g>0.9 && b>0.9) then (exposure d c) else c

exposure::Double->Pixel RGBA Double -> Pixel RGBA Double
exposure d a=rgbamap (\y -> clamp(d+y)) a--d de -1 a 1
--buscar otra funcion
{-exposure d c = let x= if d<0 then d/3 else d
                in  rgbamap (\y ->clamp(y * (x*(1-y))+1)) c
-}
opacity::Double->Pixel RGBA Double -> Pixel RGBA Double
opacity d (PixelRGBA r g b a) = (PixelRGBA r g b (clamp (a+d)))--modifica la opasidad uniformemente (y la satura)

kelvinToRGBA::Double->Pixel RGBA Double
kelvinToRGBA k =let kelvin=(clamp2 k 1000 40000)/100
                in if (kelvin>66) then (PixelRGBA (clamp (1.29293618606274509804 * (kelvin - 60)**(-0.1332047592))) (clamp (1.12989086089529411765 * (kelvin - 60)**(-0.0755148492))) 1 1)
                                  else if (kelvin==66) then (PixelRGBA 1 (clamp (0.39008157876901960784 * log( kelvin ) - 0.63184144378862745098)) 1 1)
                                                         else if (kelvin<19) then (PixelRGBA 1 (clamp (0.39008157876901960784 * log( kelvin ) - 0.63184144378862745098)) 0 1)
                                                                               else (PixelRGBA 1 (clamp (0.39008157876901960784 * log( kelvin ) - 0.63184144378862745098)) (clamp (0.54320678911019607843 * log( kelvin - 10) - 1.19625408914)) 1)

temp d c@(PixelRGBA r g b a) = let (PixelRGBA kr kg kb ka) = kelvinToRGBA d--entre 1000 y 40000
                                in let (PixelHSIA h s i la)=toPixelHSIA c
                                    in let (PixelRGBA br bg bb ba)=(PixelRGBA (r*kr) (g*kg) (b*kb) a)--blended
                                        in let (PixelHSIA hbr hbg hbb hba)=toPixelHSIA (PixelRGBA br bg bb ba)
                                            in (lerp (PixelRGBA br bg bb ba) (toPixelRGBA (PixelHSIA hbr hbg i hba)) 1)--se puede cambiar ese ultimo 1 por un
