---
title: COBOL gRPC server
date: 2017-01-05
description: >
  Creation of a gRPC server from the COPYBOOK.
categories: [Examples]
tags: [test, sample, docs]
weight: 20
---

Convert a COPYBOOK into a proto-message.
Replace the CICS IMS with a modern and efficient RPC-based mechanism (HTTP/2, compression, encryption, etc.).

In this example, we will implement our COBOL program "Hello, World" as a gRPC server.

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. hello.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Declare variables
       LINKAGE SECTION.
      * Data to share with COBOL subroutines 
       01 RECORD-TYPE.
           05 INPUT-NAME        PIC X(10).
           05 OUTPUT-PARM.
               10 PARM1         PIC X(07).
               10 PARM2         PIC X(10).
       PROCEDURE DIVISION USING RECORD-TYPE.
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

Create a directory structure with the following contents:

```
├── d8grpc
│   └── hello_client
│   └── hello_server
│   └── hello
│   go.mod
│   go.sum
```

The next step is to create the proto message that will be used to expose the COBOL program's COPYBOOK. To do this, create a file named _hello.proto_ in the _d8grpc/hello_ directory and copy the following file.


```go

syntax = "proto3";

option go_package = "examples/hello/hello";

package hello;

// d8grpc hello service definition.
service D8grpc {
  rpc Hello (MsgReq) returns (MsgRes) {}
}

// The request message containing the user's name.
message MsgReq {
  string hello_name = 1;
}

// The response message containing the greetings
message MsgRes {
  string response = 1;
}

```
The fields of the COBOL COPYBOOK:

- INPUT NAME
- OUTPUT-PARM

Are defined as type CHAR (with lengths of 10 and 17) and are converted to string.

To compile the protocol message, execute the following command:

```bash
protoc --go_out=. --go_opt=paths=source_relative \
    --go-grpc_out=. --go-grpc_opt=paths=source_relative \
    hello/hello.proto

```

> First, install the proto-message compiler utility for the Go language.
>
> To do this, follow these [instructions] (https://grpc.io/docs/protoc-installation/)

Let's create the gRPC server that will make the call to the COBOL subroutine, in this case the call will be made dynamically. Create the file _main.go_ in the directory *d8grpc/hello_server* and copy the following file.


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
	"errors"
	"flag"
	"fmt"
	"log"
	"net"
	"time"
	"unsafe"

	pb "examples/hello/hello"

	"google.golang.org/grpc"
)

var (
	port = flag.Int("port", 50051, "The server port")
)

type server struct {
	pb.UnimplementedD8GrpcServer
}

func (s *server) Hello(ctx context.Context, in *pb.MsgReq) (out *pb.MsgRes, err error) {
	start := time.Now()

	//Define argc, argv
	c_argc := C.int(1)
	c_argv := (*[0xfff]*C.char)(C.allocArgv(c_argc))
	defer C.free(unsafe.Pointer(c_argv))
	c_argv[0] = C.CString(in.GetHelloName())

	//Check COBOL program
	n := C.cob_resolve(C.CString("hello"))
	if n == nil {
		err := errors.New("COBOL: program not found")
		log.Println(err)
		return &pb.MsgRes{}, err
	}

	//Call COBOL program
	log.Println("INFO: program hello started")
	ret := C.cob_call(C.CString("hello"), c_argc, (*unsafe.Pointer)(unsafe.Pointer(c_argv)))
	log.Printf("INFO: program hello return-code %v", ret)

	//COBOL COPYBOOK is converted to Go String using COPYBOOK length
	output := C.GoStringN(c_argv[0], 27)

	elapsed := time.Since(start)
	log.Printf("INFO: Hello elapsed time %s", elapsed)

	return &pb.MsgRes{Response: output[9:]}, nil

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
	pb.RegisterD8GrpcServer(s, &server{})
	log.Printf("INFO: server listening at %v", lis.Addr())
	if err := s.Serve(lis); err != nil {
		log.Fatalf("ERROR: failed to serve: %v", err)
	}
}

```

Compile the COBOL subroutine with the following command. The result will be a module (shared library) that we can call dynamically from the Go gRPC server using cgo.

```
cobc -m hello.cbl

```

> The resulting file (*.so, *.dylib) can be left in the *d8grpc/hello_server* directory.
>
> If you decide to leave the COBOL module in another directory, remember to define it (export COB_LIBRARY_PATH=/...my_library.../). 


Open a terminal and start the gRPC server with the following command

```
go run .
```

Finally, we will create a Go client to invoke our gRPC COBOL service. Create a _main.go_ file in the *d8grpc/hello_client* directory and copy the following file.


```go

package main

import (
	"context"
	"flag"
	"log"
	"time"

	pb "examples/hello/hello"

	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"
)

var (
	addr = flag.String("addr", "localhost:50051", "the address to connect to")
)

var (
	name = flag.String("name", "", "name")
)

func main() {
	flag.Parse()
	// Set up a connection to the server.
	conn, err := grpc.Dial(*addr, grpc.WithTransportCredentials(insecure.NewCredentials()))
	if err != nil {
		log.Fatalf("did not connect: %v", err)
	}
	defer conn.Close()
	c := pb.NewD8GrpcClient(conn)

	// Contact the server and print out its response.
	ctx, cancel := context.WithTimeout(context.Background(), time.Second)
	defer cancel()
	r, err := c.Hello(ctx, &pb.MsgReq{HelloName: *name})
	if err == nil {
		log.Printf("Output: %s", r.GetResponse())
	} else {
		log.Printf("ERROR: %v", err)
	}

}


```

To test our COBOL gRPC service, open a new terminal and run the following command.

```
go run main.go -name=Hooper
```


