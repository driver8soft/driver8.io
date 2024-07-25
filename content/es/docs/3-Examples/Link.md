---
title: Calling COBOL containers
date: 2024-06-20
description: >
  Llame a programas COBOL remotos.
categories: [Examples]
tags: [test, sample, docs]
weight: 40
---

De manera equivalente al mecanismo del CICS para llamar a programas remotos (EXEC CICS LINK), realice llamadas entre programas COBOL desplegados en distintos contenedores.

A continuación se describe gráficamente el flujo de ejecución


*loanmain.cbl <--> d8link.go <-----------------------> main.go <--> loancalc.cbl*


1. El programa COBOL loanmain.cbl realiza una llamada (CALL) al conector gRPC _d8link_, esta simula una sentencia EXEC CICS LINK:
  * Se define el programa al que se quiere llamar
  * El área de intercambio de datos (COMMAREA)
  * Y la longitud de la misma
2. El conector gRPC _d8link_ recibe los datos (COMMAREA) y llama al microservicio COBOL correspondiente
3. El controller gPRC (_main.go_) gestiona el mensaje proto, lo convierte a una estructura compatible y llama al programa COBOL loancalc.cbl
4. El programa COBOL actualiza el área de datos y devuelve el control al controlador gRPC
5. Los datos son envíados de vuelta al conector _d8link_ que los copia sobre el área de memoria definida por el programa COBOL

Cree una estructura de directorios como la siguiente:


```
├── d8link
│   └── link_client
│   └── link_server
│   └── link
│   go.mod
│   go.sum
```

En el directorio _link_ definiremos nuestro mensaje proto (_link.proto_)

```go
syntax = "proto3";

option go_package = "examples/link/link";

package link;

// The Link service definition.
service LinkService {
  // Sends a greeting
  rpc CommArea (CommReq) returns (CommResp) {}
}

// The request message containing program to link, commarea and commarea length.
message CommReq {
  string link_prog = 1;
  int32 comm_len = 2;
  bytes input_msg = 3;
}

// The response message containing commarea
message CommResp {
  bytes output_msg = 1;
}

```

A continuación crearemos el programa _d8link.go_ sobre el directorio _link_client_


```go
package main

/*
#include <string.h>
#include <stdlib.h>
*/
import "C"
import (
	"context"
	"flag"
	"log"
	"time"
	"unsafe"

	pb "examples/link/link"

	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"
)

var (
	addr = flag.String("addr", "localhost:50051", "the address to connect to")
)

//export D8link
func D8link(c_program *C.char, c_commarea *C.char, c_commlen *C.int) C.int {
	flag.Parse()

	//C variables to Go variables
	program := C.GoStringN(c_program, 8) //max length of COBOL mainframe program = 8
	commarea := C.GoBytes(unsafe.Pointer(c_commarea), *c_commlen)
	commlen := int32(*c_commlen)

	log.Println("INFO: Call program -", program)

	conn, err := grpc.Dial(*addr, grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		log.Fatalf("ERROR: did not connect: %v", err)
	}
	defer conn.Close()

	c := pb.NewLinkServiceClient(conn)

	// Contact the server
	ctx, cancel := context.WithTimeout(context.Background(), time.Second)
	defer cancel()
	r, err := c.CommArea(ctx, &pb.CommReq{LinkProg: program, CommLen: commlen, InputMsg: commarea})
	if err != nil {
		log.Fatalf("ERROR: calling program - %s - %v", program, err)
	}

	outMsg := r.GetOutputMsg()

	C.memcpy(unsafe.Pointer(c_commarea), unsafe.Pointer(&outMsg[0]), C.size_t(commlen))

	return 0
}

func main() {
}
```

> Vamos a exportar la función D8link para que pueda ser llamada desde un programa COBOL, para ello es necesario compilarla utilizando la opción *c-shared* de Go
>
> El compilador de Go generará un objeto (_D8link.dylib D8link.so_) y un fichero (_D8link.h_) que serán llamados dinámicamente desde el código COBOL 

Y para finalizar crearemos el servidor gRPC (_main.go_) en el directorio *link_server* que será el encargado de recibir el mensaje proto y llamar al programa COBOL destino.

```go
package main

/*
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <libcob.h>
#cgo CFLAGS: -I/opt/homebrew/Cellar/gnucobol/3.2/include
#cgo LDFLAGS: -L/opt/homebrew/Cellar/gnucobol/3.2/lib -lcob
static void* allocArgv(int argc) {
    return malloc(sizeof(char *) * argc);
}
*/
import "C"
import (
	"context"
	"flag"
	"fmt"
	"log"
	"net"
	"strings"
	"time"
	"unsafe"

	pb "examples/link/link"

	"google.golang.org/grpc"
)

var (
	port = flag.Int("port", 50051, "The server port")
)

type server struct {
	pb.UnimplementedLinkServiceServer
}

func (s *server) CommArea(ctx context.Context, in *pb.CommReq) (out *pb.CommResp, err error) {
	start := time.Now()

	//remove trailing spaces from program name
	program := strings.TrimSpace(in.GetLinkProg())
	c_program := C.CString(program)
	defer C.free(unsafe.Pointer(c_program))

	c_commlen := C.int(in.GetCommLen())

	//allocate argc & argv variables
	c_argc := C.int(1)
	c_argv := (*[0xfff]*C.char)(C.allocArgv(c_argc))
	defer C.free(unsafe.Pointer(c_argv))

	c_argv[0] = C.CString(string(in.GetInputMsg()))
	defer C.free(unsafe.Pointer(c_argv[0]))

	//check COBOL program
	n := C.cob_resolve(c_program)
	if n == nil {
		log.Println("ERROR: Module not found. Program name =", program)
	} else {
		log.Printf("INFO: %s started", program)
		ret := C.cob_call(c_program, c_argc, (*unsafe.Pointer)(unsafe.Pointer(c_argv)))
		log.Printf("INFO: %s return-code %v", program, ret)

	}
	c_msg_output := C.GoStringN(c_argv[0], c_commlen)

	elapsed := time.Since(start)
	log.Printf("INFO: %s elapsed time %s", program, elapsed)

	return &pb.CommResp{OutputMsg: []byte(c_msg_output)}, nil
}

func main() {
	flag.Parse()
	//D8 Initialize gnucobol
	C.cob_init(C.int(0), nil)

	lis, err := net.Listen("tcp", fmt.Sprintf(":%d", *port))
	if err != nil {
		log.Fatalf("ERROR: failed to listen: %v", err)
	}

	s := grpc.NewServer()
	pb.RegisterLinkServiceServer(s, &server{})
	log.Printf("INFO: server listening at %v", lis.Addr())
	if err := s.Serve(lis); err != nil {
		log.Fatalf("ERROR: failed to serve: %v", err)
	}
}

```

Pruebe a realizar llamadas remotas entre programas COBOL intercambiando un área de datos (COPYBOOK).
Para ello recuerde que:

* El programa llamador debe compilarse para generar un ejecutable (opción -x GNUCobol)
* El programa llamado debe compilarse para generar un objeto (opción -m GNUCobol)
* Ambos programas deben compilarse utilizando el mismo "byteorder" para compartir datos binarios
* Para simplificar la prueba los programas COBOL puede residir en los directorios definidos anteriormente (*link_client link_server*)

Puede utilizar los programas COBOL de ejemplo cuota.cbl y loancalc.cbl.


```cobol
      ******************************************************************
      *
      * Loan Calculator Main Program
      * ==========================
      *
      * A sample program to demonstrate how to create a gRPC COBOL
      * microservice.
      *
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. loanmain.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       
       DATA DIVISION.

       FILE SECTION.

       WORKING-STORAGE SECTION.
      * In COBOL, you declare variables in the WORKING-STORAGE section
       01 PROG-NAME PIC X(8) VALUE "loancalc".
       01 COMMLEN PIC 9(9) COMP.      
       01 COMMAREA.
           05 INPUT-MSG.
               10 PRIN-AMT      PIC S9(7)      USAGE IS DISPLAY.
               10 INT-RATE      PIC S9(2)V9(2) USAGE IS DISPLAY.
               10 TIMEYR        PIC S9(2)      USAGE IS DISPLAY.
           05 OUTPUT-MSG.
               10 PAYMENT       PIC S9(7)V9(2) USAGE IS DISPLAY.
               10 ERROR-MSG     PIC X(20).

       PROCEDURE DIVISION.
      * code goes here!
           INITIALIZE COMMAREA.

           DISPLAY "Compound Interest Calculator"
           DISPLAY "Principal amount: " WITH NO ADVANCING.
           ACCEPT PRIN-AMT.
           DISPLAY "Interest rate: " WITH NO ADVANCING.
           ACCEPT INT-RATE.
           DISPLAY "Number of years: " WITH NO ADVANCING.
           ACCEPT TIMEYR.   

           COMPUTE COMMLEN = LENGTH OF COMMAREA.
           CALL "D8link" USING PROG-NAME COMMAREA COMMLEN.

           DISPLAY "Error Msg: " ERROR-MSG.
           DISPLAY "Couta: " PAYMENT.

           GOBACK.
```



```cobol
      ******************************************************************
      *
      * Loan Calculator Subroutine
      * ==========================
      *
      * A sample program to demonstrate how to create a gRPC COBOL
      * microservice.
      *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. loancalc.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
      * In COBOL, you declare variables in the WORKING-STORAGE section
       01  WS-MSG.
           05 WS-ERROR      PIC X(01) VALUE 'N'.
           05 WS-MSG00      PIC X(20) VALUE 'OK'.
           05 WS-MSG10      PIC X(20) VALUE 'INVALID INT. RATE'.
           05 WS-MSG12      PIC X(20) VALUE 'INVALID NUMBER YEARS'.
       01  AUX-VARS.
           05 MONTHLY-RATE  USAGE IS COMP-1.
           05 AUX-X         USAGE IS COMP-1.
           05 AUX-Y         USAGE IS COMP-1.
           05 AUX-Z         USAGE IS COMP-1.

       LINKAGE SECTION.
      * Data to share with COBOL subroutines 
       01 LOAN-PARAMS.
           05 INPUT-MSG.
               10 PRIN-AMT      PIC S9(7)      USAGE IS DISPLAY.
               10 INT-RATE      PIC S9(2)V9(2) USAGE IS DISPLAY.
               10 TIMEYR        PIC S9(2)      USAGE IS DISPLAY.
           05 OUTPUT-MSG.
               10 PAYMENT       PIC S9(7)V9(2) USAGE IS DISPLAY.
               10 ERROR-MSG     PIC X(20).

       PROCEDURE DIVISION USING BY REFERENCE LOAN-PARAMS. 
      * code goes here!
       
       000-MAIN.   
           DISPLAY "PRIN-AMT: " PRIN-AMT.  
           DISPLAY "INT-RATE: " INT-RATE.
           DISPLAY "TIMEYR: "   TIMEYR.    
           PERFORM 100-INIT.
           IF WS-ERROR = 'N'
               PERFORM 200-PROCESS
           END-IF.
           PERFORM 300-WRAPUP.    
       
       100-INIT.  
           IF INT-RATE <= 0
               MOVE WS-MSG10 TO ERROR-MSG
               MOVE 10 TO RETURN-CODE
               MOVE 'Y' TO WS-ERROR
           ELSE
               IF TIMEYR <= 0
                   MOVE WS-MSG12 TO ERROR-MSG
                   MOVE 12 TO RETURN-CODE
                    MOVE 'Y' TO WS-ERROR
               END-IF
           END-IF.                  
       200-PROCESS.
           INITIALIZE AUX-VARS.
           COMPUTE MONTHLY-RATE ROUNDED = (INT-RATE / 12 / 100).
           COMPUTE AUX-X ROUNDED = ((1 + MONTHLY-RATE) ** (TIMEYR*12)).
           COMPUTE AUX-Y ROUNDED = AUX-X * MONTHLY-RATE.
           COMPUTE AUX-Z ROUNDED = (AUX-X - 1) / AUX-Y.
           COMPUTE PAYMENT ROUNDED = PRIN-AMT / AUX-Z.
           MOVE WS-MSG00 TO ERROR-MSG.
           MOVE 0 TO RETURN-CODE.

           DISPLAY "PAYMENT: "   PAYMENT.
           DISPLAY "ERROR-MSG: " ERROR-MSG.

       300-WRAPUP.
           GOBACK.


```