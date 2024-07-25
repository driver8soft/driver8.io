package main

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
