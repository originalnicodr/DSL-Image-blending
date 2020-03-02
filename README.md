## DSL: Image blending #
## Idea general
Se otorgará una estructura para representar una secuencia de edición sobre imágenes, permitiendo asi exportar una imagen resultante. Se hará uso de la biblioteca hip para el manejo de imágenes en haskell.
## Alcances
El lenguaje puede ser utilizado por programas o aplicaciones como backend para edición de imágenes. Ejemplos puede ser una aplicación de filtros para fotos (en los cuales cada filtro es una expresión del lenguaje) o un programa que agregue una marca de agua a una imagen.

## Dependencias
Para poder utilizar el lengauje debera instalar las siguientes dependencias:

GHC es el compilador e interprete de haskell, mientras que cabal es un instalador de paquetes de haskell. 

```sudo apt install ghc```

```sudo apt install cabal-install```

Options es una biblioteca de haskell para poder utilizar flags en parsers y hip es una biblioteca de imagenes para haskell.

```cabal install options```

```sudo apt install zlib```

```cabal install hip```

 Se requerira instalar la biblioteca zlib de esta forma para utilizar hip ya que una de las bibliotecas que depende hip utiliza zlib y cabal no puede instalar las dependencias que no corresponden a paquetes de haskell.

## Manual de uso
Para ejecutar el programa debe correr el siguiente comando

```runhaskell Eval.hs COMANDO IMAGEN ```

Donde 'COMANDO' hace referencia a los siguientes caracteres:
- i (interpret): Si se desea escribir la expresion a evaluar en la terminal debe utilizar este comando. El termino "IMAGEN" por lo tanto sera una string con la expresion a evaluar.
- f (file): Si lo que se busca es leer una expresion de un archivo se debera utilizar dicho comando en conjunto con el nombre del arhivo (puede escribirse una direccion relativa o absoluta) en lugar del termino "IMAGEN".

Ademas de los comandos presentados se puede utilizar diferentes flags con configuraciones opcionales:
- --exec='s': Permite aplicar la expresion leida a una imagen. Tenga en cuenta que la expresion parseada debe ser una funcion y que s correspondera a el nombre de un archivo o a una expresion dependiendo si se utilizo el comando i o f respectivamente.
- --d='dir': Permite especificar la direccion en donde se guardara el archivo (relativa o absoluta) ademas del nombre y el formato de la imagen de salida. Se exportara el archivo como `output.png` de forma predeterminada en el directorio local.
- --m='x': Permite elegir el modo de evaluacion. Estos son:

    - 1: Las imagenes que se usaran en funciones con dos argumentos necesitaran tener las mismas dimensiones.
    - 2: Las imagenes aplicadas en funciones binarias daran una imagen resultante con el menor tamaño de ambas.
    - 3: Las imagenes aplicadas en funciones binarias daran una imagen resultante con el mayor tamaño de ambas".

El modo 2 y 3 recortaran y aumentaran los tamaños de las imagenes de forma centrada.

Alternativamente puede precindir de haskell compilando el programa con `ghc Eval.hs` y corriendo el ejecutable con los comandos anteriores utilizando './Eval' en lugar de 'runhaskell Eval.hs'.

## Gramatica
```
Exp ::=  Var
      |  "Abs" String Exp
      |  "App" Exp Exp
      |  '<' String '>'
      |  Op Exp Exp
      |  UOp Exp Double
      |  "Complement" Exp
      | '(' Exp ')'

Var ::= String

Op ::= "Normal"
     | "Add"
     | "Diff"
     | "Darken"
     | "Lighten"
     | "Multiply"
     | "Screen"
     | "Overlay"
     | "HardLight"
     | "SoftLight"
     | "ColorDodge"
     | "ColorBurn"
     | "Hue"
     | "Luminosity"
     | "BlendColor"
     | "BlendSat"
     | "Exclusion"

UOp = "Temp"
    | "Sat"
    | "Exposure"
    | "Contrast"
    | "Opacity"
```
## Desiciones de diseño

## Posibles mejoras
Mas modos de edicion

## Bibliografia
- https://github.com/prod80/prod80-ReShade-Repository
- https://en.wikipedia.org/wiki/Alpha_compositing
- https://www.adobe.com/content/dam/acom/en/devnet/pdf/pdf_reference_archive/blend_modes.pdf



EDSL con Deep embedding
