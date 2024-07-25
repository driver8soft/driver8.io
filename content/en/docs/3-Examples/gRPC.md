---
title: COBOL gRPC server
date: 2024-06-20
description: >
  Creation of a gRPC server from the COPYBOOK.
categories: [Examples]
tags: [test, sample, docs]
weight: 20
---

Convert a COPYBOOK into a proto-message.
Replace the CICS IMS with a modern and efficient RPC-based mechanism (HTTP/2, compression, encryption, etc.).

In this example, we will implement our COBOL program "Hello, World" as a gRPC server.

{{< readfile file="/static/img/include/d8grpc/hello_server/hello.cbl" code="true" lang="cobol" >}}

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

{{< readfile file="/static/img/include/d8grpc/hello/hello.proto" code="true" lang="proto" >}}

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

{{< readfile file="/static/img/include/d8grpc/hello_server/main.go" code="true" lang="go" >}}

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

{{< readfile file="/static/img/include/d8grpc/hello_client/main.go" code="true" lang="go" >}}

To test our COBOL gRPC service, open a new terminal and run the following command.

```
go run main.go -name=Hooper
```


