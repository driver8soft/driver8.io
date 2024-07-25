---
title: Python
date: 2024-06-20
description: >
  Is Python your language of choice?
categories: [Examples]
tags: [test, sample, docs]
weight: 80
---

The gRPC technology allows us to easily connect programs written in different programming languages.

In this example, we will create a Python client to call our gRPC COBOL service (_hello.cbl_).

To do this, we first need to compile the proto-message for the Python language.

```go
syntax = "proto3";

option go_package = "examples/hello/hello";

package hello;

// d8grpc hello service definition.
service D8grpc {
  // Sends a greeting
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

Install the compiler for the [Python language](https://grpc.io/docs/languages/python/quickstart/) and run the following command

```
python -m grpc_tools.protoc -I. --python_out=. --grpc_python_out=. hello.proto
```

> Compiling the proto file will create the necessary stubs for our Python client.
> - hello_pb2.py
> - hello_pb2_grpc.py


Next, create a _client.py_ file and copy the following code.

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

To test the new Python client, open a terminal and run

```
python client.py
```

Easy come, easy Go, easy Python, ...

