---
title: Variables en el lenguaje COBOL
date: 2024-06-20
description: >
  ¿Cómo convertir una COPYBOOK COBOL a una estructura en Go?
categories: [Tutorial]
weight: 10
---

### Variables en el lenguaje COBOL

Cualquier programa COBOL puede ser transformado en un microservicio y desplegado en Kubernetes.

Para ello, simplemente debe compilarse el programa y generar un módulo ejecutable que pueda ser invocado de manera dinámica o estática desde otro programa de aplicación. Puede consultar los [ejemplos](/es/docs/3-Examples) para aprender a realizar llamadas tanto dinámicas como estáticas.

De manera análoga al comportamiento de una función en un lenguaje de programación moderno, estos programas o _subrutinas_ pueden recibir un conjunto de variables como parámetros de entrada/salida. 
El procedimiento es sencillo, se debe codificar la cláusula `USING` en la `PROCEDURE DIVISION` replicado el número, orden y tipo de las variables utilizadas en la llamada realizada desde el programa principal.

En el caso de los programas Batch, el programa _principal_ (asociado a una ficha STEP de un JCL) no suele utilizar parámetros (PARM) para la recepción de datos de entrada al programa. Los programas generalmente utilizan la sentencia `COBOL ACCEPT` o leen la información necesaria directamente desde un fichero.

En el caso de los programas Online, el programa _principal_ se asocia a una transacción y es el gestor transaccional (CICS o IMS) el encargado de invocar dicho programa. Este programa suele utilizar las sentencias correspondientes del gestor transaccional para recibir un mensaje compuesto por diferentes variables (EXEC CICS RECEIVE - GN) sobre un área de memoria previamente definida.

Independientemente del modo de ejecución (Online o Batch), estos programas COBOL _principales_ pueden a su vez realizar distintas llamadas a _subrutinas_ COBOL mediante la sentencia CALL (en el caso de utilizar CICS esa llamada podría realizarse mediante la sentencia EXEC CICS LINK).

Por tanto, si queremos exponer un programa COBOL como un microservicio que pueda ser invocado desde otros microservicios escritos en distintos lenguajes de programación es necesario entender los distintos tipos de datos que puede utilizar un programa COBOL y convertirlos a un tipo estándar utilizable desde cualquier lenguaje de programación.    

### Tipos de variables en COBOL

Definir una variable en un programa COBOL puede resultar confuso para alguien acostumbrado a trabajar con lenguajes de programación actuales. Adicionalmente, existen distintos tipos de datos, ampliamente utilizados por un programador COBOL, que no tienen equivalencia en cualquier lenguaje de programación moderno.

Vamos a intentar descifrar el funcionamiento de los tipos de datos COBOL más utilizados en un programa estándar y a definir su traducción a un lenguaje moderno como Go.

> Se excluyen variables que requieren caracteres de doble byte

Como en cualquier otro lenguaje, para definir una variable, debemos declarar su nombre y su tipo (string, int, float, bytes, etc.), sin embargo en COBOL esa definición no es directa, se realiza mediante las cláusulas `USAGE` y `PICTURE`.

`01 VAR1     PIC S9(3)V9(2) COMP VALUE ZEROES.`

Mediante la cláusula `USAGE` definiremos el tipo de almacenamiento interno que utilizará la variable.

La cláusula `PICTURE` (o `PIC`) define la _máscara_ asociada a la variable y sus características generales, esta definición se realiza mediante la utilización de un conjunto de caracteres o símbolos específicos:

* A, la variable contiene caracteres alfabéticos
* X, la variable contiene caracteres alfanuméricos
* 9, variable numérica
* S, indica una variable numérica con signo
* V, número de posiciones decimales en una variable numérica
* Etc.

#### USAGE DISPLAY

Las variables definidas como `USAGE DISPLAY` puede ser de los siguientes tipos:

##### Alfabéticas

Se definen mediante la utilización del símbolo **A** en la cláusula `PICTURE`.

`01 VAR-ALPHA PIC A(20).`

Sólo pueden almacenar caracteres del alfabeto latino. Su uso no es muy común, siendo generalmente sustituidas por variables alfanuméricas que veremos a continuación.

##### Alfanuméricas

Se definen mediante la utilización del símbolo **X** en la cláusula `PICTURE`.

`01 VAR-CHAR PIC X(20).`

Como en el anterior caso no es necesario definir la cláusula `USAGE`, ya que las variables de tipo **A** o **X** por defecto son de tipo `DISPLAY`.

##### Numéricas

Se definen mediante la utilización del símbolo **9** en la cláusula `PICTURE`.

`01 VAR1     PIC S9(3)V9(2)  USAGE DISPLAY.`

En este caso estamos definiendo una variable numérica de longitud 5 (3 posiciones enteras y 2 decimales) con signo.

La definición de variables numéricas de tipo `DISPLAY` puede hacerse de manera explícita como en el ejemplo anterior o de manera implícita en caso de no codificar la cláusula `USAGE`.

##### Almacenamiento interno

Cada uno de los caracteres definidos se almacena en un byte (EBCDIC), en el caso de variables de tipo numérico con signo, este se define en los primeros 4 bits del último byte.

Vamos a ver un ejemplo para entender mejor el funcionamiento de este tipo de variables.

| Número    | Valor EBCDIC    |
|-----------|-----------------|
| 0         | x'F0'           |
| 1         | x'F1'           |
| 2         | x'F2'           |
| 3         | x'F3'           |
| 4         | x'F4'           |
| 5         | x'F5'           |
| 6         | x'F6'           |
| 7         | x'F7'           |
| 8         | x'F8'           |
| 9         | x'F9'           |

Si le asignamos un valor de **12345** a una variable de tipo

 `PIC 9(5) USAGE DISPLAY`

Está ocuparía 5 bytes

12345 = x’F1F2F3F4F5’

En el caso de variables con signo

 `PIC S9(5) USAGE DISPLAY`

+12345 = x’F1F2F3F4C5’

-12345 = x’F1F2F3F4D5’ 

#### USAGE COMP-1 o COMPUTATIONAL-1 

Variable de tipo float de 32 bits (4 bytes).

#### USAGE COMP-2 o COMPUTATIONAL-2 

Variable de tipo float de 64 bits (8 bytes).

Tanto en el caso de variables tipo `COMP-1` como tipo `COMP-2`, no se puede definir una cláusula `PICTURE` asociada a dicha variable.

#### USAGE COMP-3 o COMPUTATIONAL-3

Variable numérica de tipo Packed-Decimal.

`01 VAR-PACKED     PIC S9(3)V9(2)  USAGE COMP-3.`

Se utilizan 4 bits para almacenar cada uno de los caracteres numéricos de la variable, el signo  se almacena en los últimos 4 bits.

Por lo general, este tipo de variables se definen de longitud impar, para ocupar por completo el número de bytes utilizados.

Una sencilla regla para calcular el almacenamiento ocupado por una variable tipo `COMP-3` es dividir la longitud total de la variable (posiciones enteras + posiciones decimales) entre 2 y sumarle 1, en el ejemplo anterior el almacenamiento requerido para la variable VAR-PACKED

5 / 2 = 2  -> longitud total 5 (3 enteros + 2 decimales)

2 + 1 = 3  -> almacenamiento requerido 3 bytes

Vamos a ver un ejemplo para comprender mejor el funcionamiento de este tipo de variables.

Vamos a asignarle el valor **12345**, como en el ejemplo anterior, a una variable de tipo `COMP-3`.

 `PIC S9(5) USAGE COMP-3.`

Partimos del número en formato DISPLAY

x’F1F2F3F4F5’

Eliminamos los primeros 4 bits de cada byte y añadimos el signo al final

+12345 = x’12345C’

-12345 = x’12345D’

#### USAGE COMP-4 o COMPUTATIONAL-4 o COMP o BINARY

En este caso, el tipo de dato es numérico binario. Los números negativos se representan como  complemento a 2.

El tamaño del almacenamiento requerido depende de la cláusula `PICTURE`.

| PICTURE                  | Almacenamiento  | Valor                                  | 
|--------------------------|-----------------|----------------------------------------|
| PIC S9(1) - S9(4)        | 2 bytes         | -32,768 hasta +32,767                  |
| PIC S9(5) - S9(9)        | 4 bytes         | -2,147,483,648 hasta +2,147,483,647    |
| PIC S9(10) - S9(18)      | 8 bytes         | -9,223,372,036,854,775,808 hasta +9,223,372,036,854,775,807 | 
| PIC 9(1) - 9(4)          | 2 bytes         | 0 hasta 65,535                         |
| PIC 9(5) - 9(9)          | 4 bytes         | 0 hasta 4,294,967,295                  |
| PIC 9(10) - 9(18)        | 8 bytes         | 0 hasta 18,446,744,073,709,551,615     |


Hasta aquí el funcionamiento de este tipo de variables sería equivalente a la representación de variables enteras en la mayoría de lenguajes de programación (variables short, long o double y sus equivalentes sin signo ushort, ulong, udouble), sin embargo mediante el uso de la cláusula `PICTURE` podemos limitar los valores máximos de dicha variable y definir una máscara decimal.

Por ejemplo:

`01 VAR-COMP PIC 9(4)V9(2) USAGE COMP.`

Utiliza 4 bytes de almacenamiento (int32), pero su valor está limitado desde 0 a 9999.99.

¿Qué sentido tiene definir variables enteras y posteriormente definir una máscara fija con el número de posiciones enteras y decimales?

Limitar el uso de operaciones con coma flotante. Hoy en día esto puede resultar extraño, pero tenía sentido hace 30 años para reducir los ciclos de CPU utilizados cuando el coste de computación era extremadamente caro. 

Por otro lado, la práctica totalidad de las operaciones realizadas por instituciones financieras con moneda no requieren cálculos complejos y al operar con números enteros no perdemos precisión en caso de necesitar posiciones decimales (i.e. céntimos).

#### USAGE COMP-5 o COMPUTATIONAL-5

Este tipo de dato también es conocido como binario nativo.

Es equivalente a COMP-4, sin embargo los valores a almacenar no están limitados por la máscara definida en la cláusula `PICTURE`.

| PICTURE               | Almacenamiento    | 
|-----------------------|-------------------|
| PIC S9(4)  COMP-5     | short (int16)     | 
| PIC S9(9)  COMP-5     | long (int32)	    | 
| PIC S9(18) COMP-5     | double (int64)    |  
| PIC 9(4)   COMP-5     | ushort (uint16)   | 
| PIC 9(9)   COMP-5     | ulong (uint32)    | 
| PIC 9(18)  COMP-5     | udouble (uint64)  | 


### Conversión de variables a un tipo estándar

Es importante comentar que solo es necesario convertir las variables expuestas por los programas _principales_, una vez inicializado el runtime de cobol utilizado y ejecutado el programa _principal_, las llamadas entre programas COBOL realizadas mediante la sentencia CALL son responsabilidad del runtime.

Un caso especial es la llamada entre programas COBOL, desplegados en distintos contenedores. Para ello se ha diseñado un mecanismo específico, similar al funcionamiento de la sentencia LINK del gestor transaccional CICS.

Puede consultar cómo realizar llamadas entre programas COBOL desplegados en distintos contenedores aplicativos en el apartado de ejemplos [d8link](/es/docs/3-Examples/Link/).

Como norma general, cualquier variable COBOL podría ser expuesta de distintas formas (int, float, string, etc.) y ser convertida posteriormente, sin embargo y para simplificar y optimizar el proceso de conversión vamos a utilizar un conjunto de reglas sencillas que describiremos a continuación.

#### EBCDIC

Como hemos comentado anteriormente, el mainframe IBM utiliza EBCDIC internamente.

Para intentar replicar al máximo el comportamiento de los programas COBOL entre plataformas, algunos fabricantes permiten seguir utilizando EBCDIC internamente para el manejo de datos de aplicación.

Si bien, a corto plazo esta estrategia podría facilitar la migración del código, presenta enormes problemas de compatibilidad, evolución y soporte a medio largo plazo. Por tanto, los programas COBOL migrados utilizarán caracteres ASCII internamente.

> Es necesario identificar los programas que hagan uso de instrucciones COBOL que manejan caracteres hexadecimales y reemplazar dichas cadenas de caracteres
>
> `MOVE X’F1F2F3F4F5’ TO VAR-NUM1.`

#### Big-endian vs little-endian

La plataforma mainframe IBM es big-endian, el byte más significativo estaría almacenado en la dirección de memoria de menor valor, o dicho de otro modo el signo se almacenaría en el primer bit de la izquierda. 

Por el contrario la plataforma x86 y arm utilizan little-endian para la representación de variables binarias.

Como en el caso anterior, pensamos que la mejor estrategia es utilizar de manera nativa la arquitectura de destino, por tanto los programas serán compilados para utilizar little-endian.

#### Tamaño máximo variables COMP

Como regla general, el tamaño máximo de una variable numérica es 18 posiciones, independientemente del tipo utilizado (`DISPLAY`, `COMP`, `COMP-3`, `COMP-5`).

Existe sin embargo, la capacidad en la plataforma mainframe de extender ese límite hasta 31 dígitos en el caso de algún tipo de variable (por ejemplo `COMP-3`).

Estamos hablando de variables numéricas utilizadas para realizar operaciones aritméticas, salvo en el caso concreto de algún país con episodios de hiperinflación mantenidos durante un largo periodo de tiempo, el límite de 18 dígitos es suficiente.

#### Variables binarias

Aunque no es habitual utilizar variables de tipo binario con máscara decimal (`COMP` o `COMP-4`), pueden utilizarse para operar en binario números de gran tamaño o precisión y evitar así el uso de campos tipo float.

En el caso de variables de tipo `COMP-5` o binarias nativas, no tiene sentido aplicar una máscara decimal, por tanto su implementación será directa a un tipo de variable entera correspondiente a su tamaño.

#### Modelo de conversión


| Tipo COBOL                    | Tipo Go           | 
|-------------------------------|-------------------|
| PIC X(n)                      | string            | 
| COMP-1                        | float32           | 
| COMP-2                        | float64           |  
| PIC S9(1 hasta 4)   COMP-5    | int16             | 
| PIC S9(5 hasta 9)   COMP-5    | int32             | 
| PIC S9(10 hasta 18) COMP-5    | int64             |
| PIC 9(1 hasta 4)    COMP-5    | uint16            | 
| PIC 9(5 hasta 9)    COMP-5    | uint32            | 
| PIC 9(10 hasta 18)  COMP-5    | uint64            | 



>En COBOL no existe el concepto de String, el tamaño de la variable `PIC X(n)` corresponde siempre y exactamente a la longitud definida en la cláusula `PIC`.
>
>En caso de que el tamaño del string sea menor al tamaño definido en COBOL, será necesario justificar con espacios por la derecha.

Hasta aquí las variables que presentan una equivalencia con algún tipo de variable del lenguaje Go, a continuación vamos a definir el comportamiento de los tipos de datos numéricos con máscara decimal.

Estos son los tipos de datos generalmente utilizados por los programadores COBOL. En el caso de que el programa quiera ser expuesto para su invocación desde una plataforma externa, suelen utilizarse variables de tipo `DISPLAY` o zoned decimal para facilitar la conversión de datos (ASCII-EBCDIC) entre plataformas y facilitar la depuración de errores.

En nuestro caso, para simplificar el manejo de datos, todas estas variables se expondrán como  tipo `string`.

| Tipo COBOL                        | Tipo Go           | 
|-----------------------------------|-------------------|
| PIC S9(n)                         | string            | 
| PIC S9(n) COMP-3                  | string            | 
| PIC S9(n) COMP o COMP-4 o BINARY  | string            |


> El programa que realiza la llamada deberá asegurar que el dato enviado corresponde a un valor numérico.
>
> Las variables recibidas deberán ajustarse a la máscara definida en la cláusula `PIC`, respetando el número de posiciones enteras y decimales y justificando con ceros por la izquierda en caso de ser necesario.


### Proceso de conversión

Vamos a partir de un sencillo ejemplo, un programa COBOL que recibe una estructura de datos con distintos tipos de variables.

{{< readfile file="/static/img/include/d8vars/cobol/vars.cbl" code="true" lang="cobol" >}}

A continuación analizaremos dicha estructura (se puede parsear de manera automática) y generaremos dos ficheros:

Un fichero de configuración (vars.yaml) con las características de las variables COBOL presentes en la estructura a convertir (nombre, tipo de variable, longitud, posiciones decimales, signo).
> type = 0 -> Variable numérica de tipo `DISPLAY`
>
> type = 1 -> Variable numérica de tipo `COMP-1`
>
> type = 2 -> Variable numérica de tipo `COMP-2`
>
> type = 3 -> Variable numérica de tipo `COMP-3`
>
> type = 4 -> Variable numérica de tipo `COMP-4 o BINARY o COMP`
>
> type = 5 -> Variable numérica de tipo `COMP-5`  
>
> type = 9 -> Variable alfanumérica de tipo `CHAR`

{{< readfile file="/static/img/include/d8vars/conf/vars.yml" code="true" lang="yaml" >}}

Un fichero (request.go) con la representación en Go de la estructura COBOL.

{{< readfile file="/static/img/include/d8vars/model/request.go" code="true" lang="go" >}}

Y por último, un fichero (response.go). Podemos copiar la estructura utilizada anteriormente ya que los parámetros utilizados por el programa son de entrada/salida.

{{< readfile file="/static/img/include/d8vars/model/response.go" code="true" lang="go" >}}

Ya tenemos todo lo necesario para ejecutar nuestro programa COBOL.

### Ejecución programa de prueba

A continuación se explica cómo ejecutar el programa de pruebas anterior.

Los ejemplos pueden descargarse directamente del repo de [GitHub](https://github.com/driver8soft/d8-examples.git)

La estructura de directorios del ejemplo (**d8vars**), es la siguiente:

```
├── cmd
├── cobol
├── conf 
├── internal
│   └── cgocobol
│   └── common
│   └── service
├── model
│   └── request
│   └── response
├── test
│   go.mod
│   go.sum
|   Dockerfile
``` 

**/cobol**

Contiene los programas COBOL compilados que se vayan a ejecutar (*.dylib o *.so).

Utilice las siguientes opciones de compilación para definir el comportamiento requerido en los campos de tipo binario.

`cobc -m vars.cbl -fbinary-byteorder=native -fbinary-size=2-4-8`

**/conf**

Ficheros de configuración descritos anteriormente, utilizados para describir la estructura de la COPY COBOL. 

**/model**

Contiene la definición de las estructuras de datos en Go (request/response).

**/internal**

En este caso, podemos encontrar una versión simplificada del código necesario para la conversión de datos entre lenguajes y la ejecución de los programas COBOL.

**/test**

Por último, el directorio test contiene una utilidad para generar datos de prueba aleatorios de acuerdo a los tipos de datos definidos en el programa COBOL.

Para ejecutar el código sólo hace falta ir al directorio **/cmd**, abrir un terminal y escribir

`go run .`

Recuerde definir al runtime del lenguaje COBOL, el directorio donde se encuentran los módulos a ejecutar

`export COB_LIBRARY_PATH=/User/my_dir/d8vars/cobol`


Puede utilizar el programa COBOL de ejemplo _loancalc.cbl_ para realizar pruebas adicionales, simplemente compile el programa y modifique los ficheros de configuración y estructura de datos.

Modifique el fichero _app.env_ para definir el nombre del programa COBOL a ejecutar y el nombre de la COPY COBOL a convertir.

`COBOL_PROGRAM="loancalc"`

`COBOL_CONFIG="loancalc.yaml"`

Copie el fichero loancalc.yml a la carpeta _conf_ 
  
{{< readfile file="/static/img/include/d8vars/conf/loancalc.yml" code="true" lang="yaml" >}} 

Y sustituya las estructuras request.go y response.go por las siguientes:

```go
package request

type Request struct {
	PrincipalAmount string
	InterestRate    string
	TimeYears       string
}
  
```

```go
package response

type Response struct {
	Payment  string
	ErrorMsg string
}
```

  
  





 


