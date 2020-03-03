# DSL: Image blending #
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

### Expresiones

Las expresiones se basan en una combinacion de operaciones binarias y unarias de imagenes. Ademas se agrega el potencial del lambda calculo para definir funciones y variables.

- Var: Variable.
- Abs x y: Definicion de funcion 'y' en donde su variable es 'x'.
- App x y: Aplicacion de la expresion 'x' en 'y' ('y' debe ser una funcion, caso contrario se devolvera un error)
- '<'s'>': Lectura de una imagen, en donde s es una direccion relativa o absoluta de un archivo.
- Op x y: 'Op' es una funcion binaria de blending que mezclara las imagenes resultantes de las expresiones 'x' e 'y'. Esto se interpreta como la imagen 'y' se aplica a la imagen 'x' con la operacion de blending 'Op'. A continuacion las funciones de blending disponibles:
    - Normal: No aplicara ningun blending especial, solo mostrara la imagen 'y' por encima de la imagen 'x'.
    - Add: Suma los valores de los canales RGB de las imagenes.
    - Diff: Resta los valores de los canales RGB de las imagenes.
    - Darken: Da como resultado el componente mas oscuro de cada canal entre dos imagenes.
    - Lighten: Da como resultado el componente mas luminoso de cada canal entre dos imagenes.
    - Multiply: Multiplica las componentes de las dos imagenes canal por canal. El efecto es comparable al de poner dos filminas una encima de la otra y projectarlas juntas. La luz, obligada a pasar por ambas filminas, es debilitada dos veces.
    - Screen: Funcion de mezclado opuesta a 'Multiply', multiplica los opuestos de las imagenes. Esto es  `Screen x y = Multiply (Complement x) (Complement y)`
    - Overlay: Si la componente del canal de 'x' es menor a 0.5, los valores tonales se multiplican, sino se aplicara la funcion de mezclado de 'screen' (despues de haber sido duplicados en ambos casos). 
    - HardLight: Este modo corresponde realizar un mezclado con 'overlay' con las imagenes intercambiadas de lugar. `HardLight x y = Overlay y x`
    - SoftLight: Similar a Overlay pero con efectos reducidos.
    - ColorDodge: El brillo de la imagen 'y' "protege" a la imagen 'x' de exposicion.
    - ColorBurn: El inverso de 'colordodge'. Es decir `ColorBurn x y= Complement (ColorDodge x y)`
    - Hue: Da como resultado un pixel con el brillo y la saturacion de 'x' pero el color (hue) de 'y'.
    - Luminosity: Da como resultado un pixel con el color y la saturacion de 'x' pero el brillo de 'y'.
    - BlendColor: Da como resultado un pixel con el brillo de 'x' pero con el color (hue) y la saturacion de 'y'.
    - BlendSat: Da como resultado un pixel con el color (hue) y el brillo de 'x' pero con la saturacion de 'y'.
    - Exclusion: Un pixel brillants causan la inversion del otro pixel operando.

        Modos simetricos: Add, Darken, Lighten, Multiply, Screen, Exclusion.
        Como las funciones 'Screen', 'HardLight' y 'ColorBurn' pueden definirse a partir de otras funciones primitivas estas seran funciones derivadas.

- UOp x d: 'Uop' es una funcion binaria que toma una imagen resultante de evaluar la expresion 'x' y un numero flotante 'd'. Las funciones disponibles son las siguientes:
    - Temp: Modifica la temperatura de una imagen (es decir cambian el tono de la imagen hacia los azules o hacia los naranjas). El rango sugerido para el argumento flotante es [1000, 40000].
    - Sat: Modifica la saturacion de una imagen. El rango sugerido para el argumento flotante es [0, 1].
    - Exposure: Modifica la exposicion de una imagen. El rango sugerido para el argumento flotante es [-1, 1].
    - Contrast: Modifica el contraste de una imagen. El rango sugerido para el argumento flotante es [-1, 1].
    - Opacity: Modifica el canal alpha de una imagen (es decir su transparencia). El rango sugerido para el argumento flotante es [-1, 1].
- Complement x: Invierte los colores de la imagen resultante de evaluar la expresion 'x'.



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
## Distribucion de modulos

A continuacion una breve descripcion de cada modulo:
- Parser.hs: Contiene las funciones asociadas al parseo de terminos del lenguaje.
- Common.hs: Contiene los tipos con los que se manejara el lenguaje, como asi tambien las funciones de mezclado y edicion.
- Eval.hs: Contiene todas las funciones y monadas relacionadas a la evaluacion de terminos, como asi tambien los tipos y funciones necesarios para facilitar el uso del lenguaje compilado con sus argumentos opcionales.

## Desiciones de diseño

Los modos de blending varian su implementacion entre documentaciones, por lo que los resultados pueden ser levemente diferentes a algunos software de edicion de imagenes.

La implementacion de lambda calculo en el lenguaje me parecio necesaria ya que permite escribir archivos con funciones las cuales seran aplicadas con un input del usuario, facilitando asi la interaccion de un usuario con el lenguaje.

Al utilizar una implementacion de imagenes proporcionado por la biblioteca hip me vi obligado a diseñar el lenguaje como "Deep embedding", que aunque no sea tan elegante como una implementacion de tipo "Shallow embedding" fue mas simple analizar y buscar errores mientras se desarrollaba.

El tipo LamTerm tiene un tipo Op y un tipo UOp entre los argumentos de sus constructores. Estos hacen referencia a una funcion de blending y edicion respectivamente. Se opto por este enfoque en lugar de tener funciones de tipo (Double->Double->Double) como argumentos de los constructores para permitir su impresion en pantalla, facilitando asi la depuracion del lenguaje.

La mayoria de las funciones de mezclado estan conformadas por una funcion que toma dos canales de dos imagenes y da un canal resultante y una funcion que permite la aplicacion de la funcion descrita en los canales de un pixel (y en ultima instancia, en toda la imagen). Se prefirio que esten definidas de esta manera ya que se puede observar muy facilmente que hace cada funcion del lenguaje con los canales de un pixel. Las funciones que no estan definidas de esta manera toman dos pixeles de dos imagenes y dan un pixel resultante; es necesario escribirlos de esta manera ya que se necesita realizar una conversion a otro espacio de colores, necesitando asi las 3 componentes de un pixel RGB. Lo mismo sucede con las funciones de edicion.

Para la monada principal hice uso del concepto de "transformadores de monadas", que se basa en combinar los efectos de diferentes monadas. En mi caso necesitaba combinar una monada error con la monada IO. Dicha monada me permite, una vez realizado la evaluacion del termino, dar un resultado encapsulado en la monada IO () (ya sea teniendo una imagen resultado o un mensaje de error) necesario para que el resultado de la monada original se vea reflejado en la salida de la computadora. Fue necesario el uso de transformadores de monadas ya que la biblioteca de imagenes utilizada devuelve una imagen leida encapsulada en una monada IO, por lo cual definir una nueva monada que contenga los comportamientos de IO con una monada de error no era suficiente.

Decidi utilizar una biblioteca para parsear argumentos opcionales por que creo que es de gran utilidad darle control al usuario de donde se guarda la imagen resultado (como asi tambien su nombre y su formato) entre otras posibilidades, apuntando de esta manera a facilitar el uso para usuarios que quieran mas control sobre lo que hace el lenguaje sin volverlo abrumador para un usuario que no esta familiarizado con el lenguaje.

Se intento trabajar con imagenes con definicion de canales en Float en lugar de Double, pero como la lectura de imagenes y conversiones entre espacios de colores daban como resultado imagenes y pixeles con precision double, la constante conversion requerida para trabajar con las imagenes en precision Float daba como resultado una evaluacion mas lenta.

## Posibles mejoras
Mas modos de edicion

## Bibliografia
- https://github.com/prod80/prod80-ReShade-Repository
- https://en.wikipedia.org/wiki/Alpha_compositing
- https://www.adobe.com/content/dam/acom/en/devnet/pdf/pdf_reference_archive/blend_modes.pdf
- https://en.wikibooks.org/wiki/Haskell/Monad_transformers
