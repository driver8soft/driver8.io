---
title: Comience a usar driver8
description: Exponga un programa COBOL como una moderna API REST
categories: [Examples]
tags: [test, docs]
weight: 2
---

Aquí aprenderá como reutilizar un programa COBOL, exponiéndolo como una API REST moderna. Para ello, le enseñaremos a:

* Instalar un compilador COBOL open (en caso de que no disponga de uno).
* Escribir una sencilla "Hello, world" subrutina COBOL.
* Compilar la subrutina COBOL y probarla.
* Construir una API REST con el código anterior. 


## Pre-requisitos

* Experiencia de programación básica. Aunque el programa "Hello, world" es muy sencillo, es necesario tener los conocimientos necesarios para compilarlo estáticamente.
* Entender como funciona el paquete [cgo](https://pkg.go.dev/cmd/cgo) de Go.
* Una herramienta IDE para editar el código. Como hemos comentado anteriormente, el código es muy sencillo, por lo que cualquier herramienta debería servirnos. VSCode (free) dispone de distintas extensiones, incluido COBOL para facilitarnos este tipo de tareas.
* Una aplicación para ejecutar comandos. Linux o Mac terminal.  


## Instalar GNUCobol

Necesitaremos un compilador de COBOL.
Lo ideal, es disponer de un compilador de 64-bit, utilizaremos GNUCobol para compilar y ejecutar el siguiente ejemplo.

> Es posible utilizar compiladores COBOL de terceros, con o sin licencia, siempre que ofrezcan la posibilidad de llamar al código COBOL desde programas C.

**Step1.** Si está utilizando MacOS, instale previamente [Homebrew](https://brew.sh/)

**Step2.** Instale GNUCobol, abra un terminal y ejecute el siguiente comando (MacOS):

`brew install gnu-cobol` 

Para instalar GNUCobol en Linux ejecute el comando equivalente según el tipo de distribución utilizada.

**Step3.** Compruebe si la instalación se realizó de manera correcta ejecutando el siguiente comando:

`cobc -v` 

![](/img/others/cobc-install.png)

## Comenzar a escribir código


1. Abrir la aplicación terminal e ir a nuestro home directory.

```
cd
```

2. Crear un directorio "hello" para nuestro programa COBOL.

```
mkdir hello
cd hello
```

3. En la herramienta de IDE (o editor de texto), crear un fichero con el nombre "hello.cbl".

4. Copiar el siguiente código en el fichero "hello.cbl" y guardarlo.

{{< readfile file="/static/img/include/hello.cbl" code="true" lang="cobol" >}}

El programa COBOL "hello" (subrutina COBOL) recibe un &nombre (INPUT-NAME) y devuelve "Hello, &nombre" (OUTPUT-PARM). En caso de que &nombre no sea informado, el programa devuelve "Hello, World".

5. Compilar el programa COBOL "hello". 

`cobc -c -O -fstatic-call hello.cbl`

> Este comando creará un objeto hello.o en el directorio hello

6. Ahora necesitaremos un programa principal para poder probar nuestra subrutina COBOL. Cree un fichero "launch.cbl" en el mismo directorio y copie el siguiente código:

{{< readfile file="/static/img/include/launch.cbl" code="true" lang="cobol" >}}

7. Compile el programa principal y haga un linkado estático con la subrutina. 

```
cobc -c -x launch.cbl
cobc -x launch.o hello.o
```


8. Para ejecutar el código ejecute el siguiente comando.

```
./launch
```

## Construya una API REST

Usaremos el lenguaje Go para construir nuestra API REST. El código Go de nuestra API llamará de manera estática a la subrutina COBOL, de manera equivalente a como lo hacía el programa principal "launch.cbl" usando cgo.

1. Para ello, necesitaremos instalar Go 

Las instrucciones para la instalación de Go sobre las plataformas MacOS y Linux puede encontrarse [aquí](https://go.dev). Siga las instrucciones e instale la última versión estable de Go.

2. Verifique que Go se ha instalado de manera correcta. Para ello, abra un terminal y ejecute el siguiente comando:

```
go version
```

3. Vamos a crear la siguiente estructura de directorios para almacenar los componentes del proyecto.

```

├── greetings
│   └── include
│   └── libs
│   go.mod
│   go.sum
│   main.go

```

> greetings           ->  Go programs
>
> greetings/include   ->  .h files  
>
> greetings/libs      ->  hello.o (COBOL routine)

Vaya al directorio go/src y teclee los siguientes comandos:

```
mkdir greetings
mkdir greetings/include
mkdir greetings/libs
```

4. Cree un fichero "go.mod" para manejar las dependencias del código.

```
cd greetings
go mod init example/greetings


go: creating new go.mod: module example/greetings
go: to add module requirements and sums:
	go mod tidy
```

5. Cree un fichero "main.go" utilizando la herramienta IDE o el editor de texto y copie el siguiente código 

```go
package main

import (
	"net/http"
	"github.com/gin-gonic/gin"
)

func main() {

	router := gin.Default()
	router.GET("/hello", getName)
	router.GET("/hello/:name", getName)
	router.Run("localhost:8080")
}

func getName(c *gin.Context) {
	d := c.Param("name")
	c.IndentedJSON(http.StatusOK, gin.H{"output-parm": d})
}

```

6. Vamos a ejecutar nuestra primera API Go.

```
go mod tidy
go run .


[GIN-debug] [WARNING] Creating an Engine instance with the Logger and Recovery middleware already attached.

[GIN-debug] [WARNING] Running in "debug" mode. Switch to "release" mode in production.
 - using env:	export GIN_MODE=release
 - using code:	gin.SetMode(gin.ReleaseMode)

[GIN-debug] GET    /hello                    --> main.getName (3 handlers)
[GIN-debug] GET    /hello/:name              --> main.getName (3 handlers)
[GIN-debug] [WARNING] You trusted all proxies, this is NOT safe. We recommend you to set a value.
Please check https://pkg.go.dev/github.com/gin-gonic/gin#readme-don-t-trust-all-proxies for details.
[GIN-debug] Listening and serving HTTP on localhost:8080
```

Ya tenemos nuestra API funcionando, abra una sesión nueva de terminal y utilice curl para probarla.

```
curl http://localhost:8080/hello 
```

```
curl http://localhost:8080/hello/Hooper
```

## Linkado estático de la API REST con el módulo COBOL 

1. Ahora modificaremos el programa "main.go" escrito anteriormente.

```go
func main() {
	C.cob_init(C.int(0), nil)

	router := gin.Default()
	router.GET("/hello", getName)
	router.GET("/hello/:name", getName)
	router.Run("localhost:8080")
}

func getName(c *gin.Context) {
	d := c.Param("name")
	o := callhello(d)
	c.IndentedJSON(http.StatusOK, gin.H{"output-parm": o})
}
```

Desde la función "main" del programa Go tendremos que inicializar el "runtime" COBOL.

_C.cob_init(C.int(0), nil)_

> No vamos a pasar ningún argumento, por lo que será más sencillo pasar un puntero con valor null

Ahora necesitamos crear una nueva función para efectuar la llamada al módulo COBOL "hello.o"

_o := callhello(d)_ 

2. Copie este código, justo encima de la función "main". 

```go
func callhello(d string) string {

	inputName := C.CString(d)
	defer C.free(unsafe.Pointer(inputName))
	outputParm := C.CString("")
	defer C.free(unsafe.Pointer(outputParm))

	returnCode := C.hello(inputName, outputParm)
	if returnCode == 0 || returnCode == 2 {
		return C.GoString(outputParm)
	} else {
		return "ERROR FROM COBOL"
	}
}
```
3. Para utilizar cgo, necesitamos importar el paquete "C". Copie el siguiente código justo después de la sentencia "package".

```go
/*
#cgo CFLAGS: -I${SRCDIR}/include
#cgo LDFLAGS: ${SRCDIR}/libs/hello.o -L/opt/homebrew/Cellar/gnucobol/3.2/lib -lcob
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "hello.h"
extern void cob_init(int argc,char** argv);
*/
import "C"
import (
	"net/http"
	"unsafe"

	"github.com/gin-gonic/gin"
)
```

> El paquete "C" usará las instrucciones definidas como comentarios justo antes de la línea "import C". No agrupe la sentencia "import C" con la importación de otros paquetes y asegúrese de que se encuentra a justo a continuación de las líneas de comentarios

**#cgo CFLAGS: -I${SRCDIR}/include**

Defina el directorio donde se encuentran los ficheros ".h". 

**#cgo LDFLAGS: ${SRCDIR}/libs/hello.o -L/opt/homebrew/Cellar/gnu-cobol/3.2/lib -lcob** 

Defina el directorio donde se encuentra el módulo COBOL (hello.o) y el runtime del lenguaje (GNUCobol).

> Por favor, revise de acuerdo al gestor de paquetes utilizado, donde está instalado GNUCobol y específicamente la librería "libcob". Por ejemplo en MacOs con arquitectura arm (chip M1/M2) homebrew puede instalar GNUCobol en el directorio  _/opt/homebrew/Cellar/gnucobol/3.2/lib_

4. A continuación el código completo del programa "main.go".

{{< readfile file="/static/img/include/greetings.go" code="true" lang="go" >}}

5. Copie el módulo COBOL "hello.o" en el directorio /greetings/libs. 

6. Cree un fichero "hello.h" en el directorio /greetings/include.

```
cd include 
```

Copie el siguiente código en el fichero "hello.h"

```c
extern int hello(char* inputName, char* outputParm); 
```

## ¡Hagamos una prueba!

1. Si el programa "main.go" sigue ejecutandose, detengalo.

2. Sitúese en el directorio que contiene el programa "main.go" y ejecute el comando:

```
go run .
```

3. Probemos nuestra API usando curl

```
curl http://localhost:8080/hello 
```

```
curl http://localhost:8080/hello/Hooper
```

## Aprenda a escribir APIs gRPC

En este sencillo ejemplo hemos aprendido como reutilizar nuestro código COBOL mediante la funcionalidad que nos ofrece Go. Si quiere seguir aprendiendo, puede revisar otros ejemplos en la siguiente sección: [Ejemplos](/docs/3-examples/).
