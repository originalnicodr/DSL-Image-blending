# DSL: Image blending #
## Idea general
Se otorgará una estructura para representar una secuencia de edición sobre imágenes, permitiendo así exportar una imagen resultante. Se hará uso de la biblioteca hip para el manejo de imágenes en haskell.
## Alcances
El lenguaje puede ser utilizado por programas o aplicaciones como backend para edición de imágenes. Ejemplos puede ser una aplicación de filtros para fotos (en los cuales cada filtro es una expresión del lenguaje) o un programa que agregue una marca de agua a una imagen.

## Dependencias
Para poder utilizar el lenguaje deberá instalar las siguientes dependencias:

GHC es el compilador e intérprete de haskell, mientras que cabal es un instalador de paquetes de haskell.

```sudo apt install ghc```

```sudo apt install cabal-install```

Options es una biblioteca de haskell para poder utilizar flags en parsers y hip es una biblioteca de imágenes para haskell.

```cabal install options```

```sudo apt install zlib```

```cabal install hip```

 Se requerirá instalar la biblioteca zlib de esta forma para utilizar hip ya que una de las bibliotecas que depende de hip utiliza zlib y cabal no puede instalar las dependencias que no corresponden a paquetes de haskell.

## Manual de uso
Para ejecutar el programa debe correr el siguiente comando

```runhaskell Main.hs COMANDO IMAGEN ```

Donde 'COMANDO' hace referencia a los siguientes caracteres:
- i (interpret): Si se desea escribir la expresion a evaluar en la terminal debe utilizar este comando. El término "IMAGEN" por lo tanto será una string con la expresión a evaluar.
- f (file): Si lo que se busca es leer una expresión de un archivo se deberá utilizar dicho comando en conjunto con el nombre del archivo (puede escribirse una dirección relativa o absoluta) en lugar del término "IMAGEN".

Además de los comandos presentados se puede utilizar diferentes flags con configuraciones opcionales:
- --exec='s': Permite aplicar términos argumentos a un término leído. Tenga en cuenta que el término parseado debe ser una función y que 's' corresponde a una serie de nombre de un archivo o expresiones dependiendo si se utilizó el comando i o f respectivamente (estos separados por comas).
- --d='dir': Permite especificar la dirección en donde se guardará el archivo (relativa o absoluta) además del nombre y el formato de la imagen de salida. Se exportará el archivo como `output.png` de forma predeterminada en el directorio local.
- --m='x': Permite elegir el modo de evaluación. Estos son:

    - 1: Las imágenes que se usarán en funciones con dos argumentos necesitaran tener las mismas dimensiones.
    - 2: Las imágenes aplicadas en funciones binarias darán una imagen resultante con el menor tamaño de ambas.
    - 3: Las imágenes aplicadas en funciones binarias darán una imagen resultante con el mayor tamaño de ambas".

El modo 2 y 3 recortan y aumentan los tamaños de las imágenes de forma centrada.

Alternativamente puede prescindir de haskell compilando el programa con `ghc Main.hs` y corriendo el ejecutable con los comandos anteriores utilizando './Main' en lugar de 'runhaskell Main.hs'.

### Expresiones

Las expresiones se basan en una combinación de operaciones binarias y unarias de imágenes. Además se agrega el potencial del lambda cálculo para definir funciones y variables.

- Var: Variable.
- Abs x y: Definicion de funcion 'y' en donde su variable es 'x'.
- App x y: Aplicación de la expresión 'x' en 'y' ('y' debe ser una función, caso contrario se devolverá un error)
- '<'s'>': Lectura de una imagen, en donde s es una dirección relativa o absoluta de un archivo.
- Op x y: 'Op' es una función binaria de blending que mezclara las imágenes resultantes de las expresiones 'x' e 'y'. Esto se interpreta como la imagen 'y' se aplica a la imagen 'x' con la operación de blending 'Op'. A continuación las funciones de blending disponibles:
    - Normal: No aplica ningún blending especial, solo mostrará la imagen 'y' por encima de la imagen 'x'.
    - Add: Suma los valores de los canales RGB de las imágenes.
    - Diff: Resta los valores de los canales RGB de las imágenes.
    - Darken: Da como resultado el componente más oscuro de cada canal entre dos imágenes.
    - Lighten: Da como resultado el componente más luminoso de cada canal entre dos imágenes.
    - Multiply: Multiplica las componentes de las dos imágenes canal por canal. El efecto es comparable al de poner dos filminas una encima de la otra y proyectarlas juntas. La luz, obligada a pasar por ambas filminas, es debilitada dos veces.
    - Screen: Función de mezclado opuesta a 'Multiply', multiplica los opuestos de las imágenes. Esto es  `Screen x y = (Complement Multiply (Complement x) (Complement y))`
    - Overlay: Si la componente del canal de 'x' es menor a 0.5, los valores tonales se multiplican, si no se aplicará la función de mezclado de 'screen' (después de haber sido duplicados en ambos casos).
    - HardLight: Este modo corresponde realizar un mezclado con 'overlay' con las imágenes intercambiadas de lugar. `HardLight x y = Overlay y x`
    - SoftLight: Similar a Overlay pero con efectos reducidos.
    - ColorDodge: El brillo de la imagen 'y' "protege" a la imagen 'x' de exposición.
    - ColorBurn: El inverso de 'colordodge'. Es decir `ColorBurn x y= Complement (ColorDodge x y)`
    - Hue: Da como resultado un pixel con el brillo y la saturación de 'x' pero el color (hue) de 'y'.
    - Luminosity: Da como resultado un pixel con el color y la saturación de 'x' pero el brillo de 'y'.
    - BlendColor: Da como resultado un pixel con el brillo de 'x' pero con el color (hue) y la saturación de 'y'.
    - BlendSat: Da como resultado un pixel con el color (hue) y el brillo de 'x' pero con la saturación de 'y'.
    - Exclusion: Un pixel brillante causa la inversión del otro pixel operando.

        Modos simetricos: Add, Darken, Lighten, Multiply, Screen, Exclusion.
        Como las funciones 'Screen', 'HardLight' y 'ColorBurn' pueden definirse a partir de otras funciones primitivas estas serán funciones derivadas.

- UOp x d: 'Uop' es una función binaria que toma una imagen resultante de evaluar la expresión 'x' y un número flotante 'd'. Las funciones disponibles son las siguientes:
    - Temp: Modifica la temperatura de una imagen (es decir cambian el tono de la imagen hacia los azules o hacia los naranjas). El rango sugerido para el argumento flotante es [1000, 40000].
    - Sat: Modifica la saturación de una imagen. El rango sugerido para el argumento flotante es [0, 1].
    - Exposure: Modifica la exposición de una imagen. El rango sugerido para el argumento flotante es [-1, 1].
    - Contrast: Modifica el contraste de una imagen. El rango sugerido para el argumento flotante es [-1, 1].
    - Opacity: Modifica el canal alpha de una imagen (es decir su transparencia). El rango sugerido para el argumento flotante es [-1, 1].
- Complement x: Invierte los colores de la imagen resultante de evaluar la expresión 'x'.



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

A continuación una breve descripción de cada módulo:
- Parser.hs: Contiene las funciones asociadas al parseo de términos del lenguaje.
- Common.hs: Contiene los tipos con los que se maneja el lenguaje, como así también las funciones de mezclado y edición.
- Main.hs: Contiene todas las funciones y monadas relacionadas a la evaluación de términos, como así también los tipos y funciones necesarios para facilitar el uso del lenguaje compilado con sus argumentos opcionales. Se optó por poner los evaluadores y las funciones utilizadas en el programa compilado en un mismo archivo ya que se requiere el uso de la opción de lenguaje de contextos flexibles, la cual no se habilita al utilizar el archivo como módulo.

## Decisiones de diseño

Los modos de blending varían su implementación entre documentaciones, por lo que los resultados pueden ser levemente diferentes a algunos software de edicion de imagenes.

La implementación de lambda cálculo en el lenguaje me pareció necesaria ya que permite escribir archivos con funciones las cuales serán aplicadas con un input del usuario, facilitando así la interacción de un usuario con el lenguaje.

Al utilizar una implementación de imágenes proporcionado por la biblioteca hip me vi obligado a diseñar el lenguaje como "Deep embedding", que aunque no sea tan elegante como una implementación de tipo "Shallow embedding" fue más simple analizar y buscar errores mientras se desarrollaba.

El tipo LamTerm tiene un tipo Op y un tipo UOp entre los argumentos de sus constructores. Estos hacen referencia a una función de blending y edición respectivamente. Se optó por este enfoque en lugar de tener funciones de tipo (Double->Double->Double) como argumentos de los constructores para permitir su impresión en pantalla, facilitando así la depuración del lenguaje.

La mayoría de las funciones de mezclado están conformadas por una función que toma dos canales de dos imágenes y da un canal resultante y una función que permite la aplicación de la función descrita en los canales de un pixel (y en última instancia, en toda la imagen). Se prefirió que están definidas de esta manera ya que se puede observar muy fácilmente que hace cada función del lenguaje con los canales de un pixel. Las funciones que no están definidas de esta manera toman dos pixeles de dos imagenes y dan un pixel resultante; es necesario escribirlos de esta manera ya que se necesita realizar una conversión a otro espacio de colores, necesitando así las 3 componentes de un pixel RGB. Lo mismo sucede con las funciones de edición.

Se hizo uso de la opción de lenguaje FlexibleContexts para "tranquilizar" a ghc y poder compilar.

Para la monada principal hice uso del concepto de "transformadores de monadas", que se basa en combinar los efectos de diferentes monadas. En mi caso necesitaba combinar una monada error con la monada IO. Dicha monada me permite, una vez realizado la evaluación del término, dar un resultado encapsulado en la monada IO () (ya sea teniendo una imagen resultado o un mensaje de error) necesario para que el resultado de la monada original se vea reflejado en la salida de la computadora. Fue necesario el uso de transformadores de monadas ya que la biblioteca de imágenes utilizada devuelve una imagen leída encapsulada en una monada IO, por lo cual definir una nueva monada que contenga los comportamientos de IO con una monada de error no era suficiente.

Decidí utilizar una biblioteca para parsear argumentos opcionales por que creo que es de gran utilidad darle control al usuario de donde se guarda la imagen resultado (como así también su nombre y su formato) entre otras posibilidades, apuntando de esta manera a facilitar el uso para usuarios que quieran más control sobre lo que hace el lenguaje sin volverlo abrumador para un usuario que no está familiarizado con el lenguaje.

Se intentó trabajar con imágenes con definición de canales en Float en lugar de Double, pero como la lectura de imágenes y conversiones entre espacios de colores daban como resultado imágenes y pixeles con precisión double, la constante conversión requerida para trabajar con las imágenes en precision Float daba como resultado una evaluación más lenta.

Las funciones de edición reciben primero un Double y luego una imagen para facilitar su uso en la función de edit.

## Posibles mejoras
Más modos de edición.

Optimizacion.

Catchear mensajes de error de haskell relevantes (por ej que no se encontró la imagen).

Agregar soporte para previsualización de previsualizar el resultado de evaluación en lugar de guardarlo.

Agregar soporte para gifs.

## Bibliografia
- https://github.com/prod80/prod80-ReShade-Repository
- https://en.wikipedia.org/wiki/Alpha_compositing
- https://www.adobe.com/content/dam/acom/en/devnet/pdf/pdf_reference_archive/blend_modes.pdf
- https://en.wikibooks.org/wiki/Haskell/Monad_transformers
