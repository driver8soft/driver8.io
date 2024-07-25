---
title: Calling COBOL containers
date: 2024-06-20
description: >
  Call remote COBOL programs.
categories: [Examples]
tags: [test, sample, docs]
weight: 40
---

Similar to the CICS mechanism for calling remote programs (EXEC CICS LINK), you can make calls between COBOL programs deployed in different containers.

The following is a graphical description of the execution flow


*loanmain.cbl <--> d8link.go <-----------------------> main.go <--> loancalc.cbl* 


1. The COBOL program loanmain.cbl makes a CALL to the gRPC connector _d8link_, which simulates an EXEC CICS LINK statement:
  * The program to be called
  * The data exchange area (COMMAREA)
  * And the length of the COMMAREA
2. The gRPC connector _d8link_ receives the data (COMMAREA) and calls the corresponding COBOL microservice.
3. The gPRC controller (_main.go_) handles the protocol message, converts it to a compatible structure and calls the COBOL program loancalc.cbl.
4. The COBOL program updates the data area and returns control to the gRPC controller.
5. The data is sent back to the _d8link_ connector, which copies it into the memory area defined by the COBOL program.

Create a directory structure like this

```
├── d8link
│   └── link_client
│   └── link_server
│   └── link
│   go.mod
│   go.sum
```

In the _link_ directory we will define our proto message (_link.proto_).

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

Next, we will create the _d8link.go_ program in the _link_client_ directory.


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

> We are going to export the D8link function so that it can be called from a COBOL program, to do this it is necessary to compile it using the *c-shared* option of Go.
>
> The Go compiler will generate an object (_D8link.dylib D8link.so_) and a file (_D8link.h_) that will be called dynamically from the COBOL code. 

Finally, we will create the gRPC server (_main.go_) in the *link_server* directory, which will be in charge of receiving the proto message and calling the target COBOL program.


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
Try to make remote calls between COBOL programs by exchanging a data area (COPYBOOK).
To do this, remember that

* The calling program must be compiled to produce an executable (option -x GNUCobol).
* The called program must be compiled to produce a shared library (option -m GNUCobol).
* Both programs must be compiled with the same byte order to share binary data.
* To simplify testing, COBOL programs can be located in the directories defined above (*link_client link_server*).

You can use the example COBOL programs cuota.cbl and loancalc.cbl.


```cobol
      ******************************************************************
      *
      * Loan Calculator Main Program
      * ==========================
      *
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. loanmain.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       
       DATA DIVISION.

       FILE SECTION.

       WORKING-STORAGE SECTION.
      * Declare variables in the WORKING-STORAGE section
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
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. loancalc.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
      * Declare variables in the WORKING-STORAGE section
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