---
title: Getting Started
description: Learn how to expose a COBOL program as a modern REST API
categories: [Examples]
tags: [test, docs]
weight: 2
---

You'll learn how to expose a COBOL program as a modern REST API. Along the way, you will:

* Install COBOL (if you haven't already done so).
* Write a simple "hello world" program.
* Compile it as a COBOL subroutine and test it.
* Build a REST API with your code. 

## Prerequisites

* Some programming experience. The "Hello, world" example is fairly simple, but you will need to compile it statically.
* Understanding of the basic concepts of Go [cgo](https://pkg.go.dev/cmd/cgo).
* A tool for editing your code. As mentioned above, the code is pretty simple, so any text editor you have will work. 
Editors such as VSCode (free) have support for a variety of languages through extensions.
* A command terminal. Linux and Mac terminals. 


## Install GNUCobol

You'll need a Linux COBOL compiler.
A 64-bit compiler is recommended, you can use GNUCobol to compile and run the following examples.

> It is possible to use third party COBOL compilers, whether licensed or open source, as long as they allow the COBOL code to be called from within C programmes.


**Step1.** If you're using MacOS Install [Homebrew](https://brew.sh/)

**Step2.** Install GNUCobol, open a terminal and run the following command (MacOS):

`brew install gnu-cobol` 

For Linux users, please run the corresponding command.

**Step3.** Check if it is installed correctly using the below command:

`cobc -v` 

![](/img/others/cobc-install.png)

## Write some code

Get started with Hello, world

1. Open a command prompt and cd to your home directory.

```
cd
```

2. Create a hello directory for your Hello COBOL source code. For example:

```
mkdir hello
cd hello
```

3. In your text editor, create a file called hello.cbl in which you will write your code.

4. Copy the following code into your hello.cbl file and save it.

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. hello.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Declare program variables
       LINKAGE SECTION.
      * Data to share with COBOL subroutines 
       01 INPUT-NAME            PIC X(10).
       01 OUTPUT-PARM.
               05 PARM1         PIC X(07).
               05 PARM2         PIC X(10).
       PROCEDURE DIVISION USING INPUT-NAME, OUTPUT-PARM.
           MOVE "Hello," TO PARM1.
           IF INPUT-NAME IS EQUAL TO (SPACES OR LOW-VALUES) 
              MOVE "World"  TO PARM2
              MOVE 2 TO RETURN-CODE
           ELSE 
              MOVE INPUT-NAME TO PARM2
              MOVE 0 TO RETURN-CODE
           END-IF.           
           GOBACK.
           
```

The COBOL program hello (COBOL subroutine) receives a &name (INPUT-NAME) and returns "Hello, &name" (OUTPUT-PARM). In case a name is not provided returns "Hello, World".

5. Compile your hello COBOL subroutine 

`cobc -c -O -fstatic-call hello.cbl`

> It'll create a hello.o object in your hello directory

6. You need a main program to test your hello subroutine. Create a file launch.cbl and copy the following code:


```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. launch.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Declare program variables 
       01 INPUT-NAME        PIC X(10).
       01 OUTPUT-PARM       PIC X(17).
       PROCEDURE DIVISION.
      * code goes here!
           DISPLAY "Your name: " WITH NO ADVANCING.
           ACCEPT INPUT-NAME.
           CALL 'hello' USING INPUT-NAME, OUTPUT-PARM.
           DISPLAY OUTPUT-PARM.
           DISPLAY 'Return Code: ' RETURN-CODE.
           STOP RUN.  

```

7. Compile it and static linking with the subroutine 

```
cobc -c -x launch.cbl
cobc -x launch.o hello.o

./launch
```

## Write a REST API

We'll use Go to build a web service layer. Your Go API will statically link the hello COBOL subroutine using cgo.

1. You need to install Go 

You can find instalation packages for MacOS and Linux, [here](https://go.dev). Follow the instructions to install the latest stable version.

2. Verify that you've installed Go. Open a command prompt and type:

```
go version
```
3. Open a terminal and create the following directorys to store your code.

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


Navigate to your go/src directory and use the following commands

```
mkdir greetings
mkdir greetings/include
mkdir greetings/libs
```

4. Define a go.mod file to enable dependency tracking for your code.

```
cd greetings
go mod init example/greetings


go: creating new go.mod: module example/greetings
go: to add module requirements and sums:
	go mod tidy
```

5. Create a main.go file using your text editor and copy the following code 

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

6. Let's execute your first Go API

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

Your API is running, open a new terminal and use curl to test it.

```
curl http://localhost:8080/hello 
```

```
curl http://localhost:8080/hello/Hooper
```

## Link your REST API with the COBOL module

1. Ok, now you need to modify your main.go program

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

In the main function initialize the cobol runtime 

_C.cob_init(C.int(0), nil)_

> We´re not using arguments, so it's easy to pass a null pointer.

You need to create a new function to call your COBOL hello routine. 

_o := callhello(d)_ 

2. Copy this code, just before the main function. 

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
3. Your are using Go cgo, define de C functions and import the "C" package. Copy this code right after the initial package main line.

```go
/*
#cgo CFLAGS: -I${SRCDIR}/include
#cgo LDFLAGS: ${SRCDIR}/libs/hello.o -L/opt/homebrew/Cellar/gnu-cobol/3.2/lib -lcob
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

> The 'C' package will use the comments defined before the 'C' import. Do not group import packages to ensure that import "C" is encoded just below the comments.

**#cgo CFLAGS: -I${SRCDIR}/include**

Define where Go will find the .h files. 

**#cgo LDFLAGS: ${SRCDIR}/libs/hello.o -L/opt/homebrew/Cellar/gnu-cobol/3.2/lib -lcob** 

Define the location of your COBOL module (hello.o) and COBOL runtime.

> Please check your package manager to find where gnucobol is installed, for example in MacOs with arm architecture the location is _/opt/homebrew/Cellar/gnu-cobol/3.2/lib_

4. This is the complete main.go code

```go
package main

/*
#cgo CFLAGS: -I${SRCDIR}/include
#cgo LDFLAGS: ${SRCDIR}/libs/hello.o -L/opt/homebrew/Cellar/gnu-cobol/3.2/lib -lcob
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
5. Copy COBOL hello.o module to your /greetings/libs directory

6. Define hello.h in your /greetings/include directory

```
cd include 
```

Create a hello.h file and copy the following code

```c
extern int hello(char* inputName, char* outputParm); 
```

## Try it out!

1. If the server is still running, stop it.

2. From the command line in the directory containing main.go, run the code.

```
go run .
```

3. Test it using curl

```
curl http://localhost:8080/hello 
```

```
curl http://localhost:8080/hello/Hooper
```

## Write more code

With this quick introduction, you've learned how to use Go to expose your COBOL code as a modern REST API. To write some more code, take a look at the [Examples](/docs/3-examples/) chapter.
