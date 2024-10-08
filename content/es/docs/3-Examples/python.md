---
title: Python
date: 2024-06-20
description: >
  Desea utilizar Python
categories: [Examples]
tags: [test, sample, docs]
weight: 80
---

La tecnología gRPC nos permite conectar programas escritos en distintos lenguajes de programación de manera sencilla.

En este ejemplo, crearemos un cliente Python para llamar a nuestro servicio gRPC COBOL (_hello.cbl_).

Para ello primero necesitaremos compilar el mensaje proto para el lenguaje Python.

{{< readfile file="/static/img/include/d8grpc/hello/hello.proto" code="true" lang="proto" >}}

Instale el compilador correspondiente al lenguaje [Python](https://grpc.io/docs/languages/python/quickstart/) y ejecute el siguiente comando

```
python -m grpc_tools.protoc -I. --python_out=. --grpc_python_out=. hello.proto
```

> La compilación del fichero proto creará los stubs necesarios para nuestro cliente Python
>  - hello_pb2.py
>  - hello_pb2_grpc.py


A continuación crearemos un cliente Python gRPC, cree un fichero _client.py_ y copie el siguiente código.


```python
import grpc
import hello_pb2
import hello_pb2_grpc

def run(inputname):
    with grpc.insecure_channel('localhost:50051') as channel:
        stub = hello_pb2_grpc.D8grpcStub(channel)
        r = stub.Hello(hello_pb2.MsgReq(hello_name=inputname))
    print(f"Result: {r.response}")

if __name__ == '__main__':
    # Get user Input 
    inputname = input("Please enter name: ")
    run(inputname)

```

Para probar el nuevo cliente Python, abra un terminal y ejecute

```
python client.py
```

