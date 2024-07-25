---
title: COBOL gRPC server
date: 2024-06-20
description: >
  Construya un gRPC server a partir de la COPYBOOK.
categories: [Examples]
tags: [test, sample, docs]
weight: 20
---

Transforme una COPYBOOK en un mensaje proto.
Sustituya el CICS IMS por un moderno y eficiente mecanismo basado en RPC (HTTP/2, compresión, cifrado, etc.).

En este ejemplo vamos a implementar nuestro programa COBOL "Hello, World" como un servidor gRPC.

{{< readfile file="/static/img/include/d8grpc/hello_server/hello.cbl" code="true" lang="cobol" >}}

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

{{< readfile file="/static/img/include/d8grpc/hello/hello.proto" code="true" lang="proto" >}}

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

{{< readfile file="/static/img/include/d8grpc/hello_server/main.go" code="true" lang="go" >}}

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

{{< readfile file="/static/img/include/d8grpc/hello_client/main.go" code="true" lang="go" >}}

Para probar nuestro servicio COBOL gRPC abra un nuevo terminal y ejecute el comando.

```
go run main.go -name=Hooper
```


