---
title: COBOL gRPC server
date: 2017-01-05
description: >
  Construya un gRPC server a partir de la COPYBOOK.
categories: [Examples]
tags: [test, sample, docs]
weight: 20
---

Transforme una COPYBOOK en un mensaje proto.
Sustituya el CICS IMS por un moderno y eficiente mecanismo basado en RPC (HTTP/2, compresión, cifrado, etc.).

En este ejemplo vamos a implementar nuestro programa COBOL "Hello, World" como un servidor gRPC.

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

Cree la siguiente estructura de directorios:

```
├── d8grpc
│   └── hello_client
│   └── hello_server
│   └── hello
│   go.mod
│   go.sum
```

A continuación crearemos la definición del mensaje proto que nos servirá para exponer la COPYBOOK del programa COBOL. Para ello cree un fichero con el nombre _hello.proto_ en el directorio _d8grpc/hello_ y copie el siguiente fichero.

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

Los campos de la COPYBOOK COBOL:

- INPUT-NAME
- OUTPUT-PARM

Están definidos como tipo CHAR (con longitudes 10 y 17) y se convierten a string.

Para compilar el mensaje proto ejecute el siguiente comando

```bash
protoc --go_out=. --go_opt=paths=source_relative \
    --go-grpc_out=. --go-grpc_opt=paths=source_relative \
    hello/hello.proto

```

> Instale antes la utilidad de compilación de mensajes proto para el lenguaje Go
>
> Para ello siga las siguientes [instrucciones](https://grpc.io/docs/protoc-installation/)

Vamos a crear el servidor gRPC que realizará la llamada a la subrutina COBOL, en este caso la llamada se realizará de manera dinámica. Cree el fichero _main.go_ en el directorio *d8grpc/hello_server* y copie el siguiente fichero.

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

Compile la subrutina COBOL mediante el siguiente comando. El resultado será un módulo (shared library) que podremos llamar de manera dinámica desde el servidor Go gRPC mediante cgo.

```
cobc -m hello.cbl

```

> El fichero resultante (*.so, *.dylib) puede dejarse en el directorio *d8grpc/hello_server*
>
> Si decide dejar el módulo COBOL en otro directorio recuerde definirlo (export COB_LIBRARY_PATH=/...my_library.../) 


Abra un terminal y ejecute el servidor gRPC mediante el siguiente comando

```
go run .
```

Por último, crearemos un cliente Go para realizar la llamada a nuestro servicio gRPC COBOL. Cree el fichero _main.go_ en el directorio *d8grpc/hello_client* y copie el siguiente fichero.

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

Para probar nuestro servicio COBOL gRPC abra un nuevo terminal y ejecute el comando.

```
go run main.go -name=Hooper
```


