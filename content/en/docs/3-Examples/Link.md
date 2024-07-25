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

{{< readfile file="/static/img/include/d8link/d8link/link.proto" code="true" lang="proto" >}}

Next, we will create the _d8link.go_ program in the _link_client_ directory.

{{< readfile file="/static/img/include/d8link/d8link_client/d8link.go" code="true" lang="go" >}}

> We are going to export the D8link function so that it can be called from a COBOL program, to do this it is necessary to compile it using the *c-shared* option of Go.
>
> The Go compiler will generate an object (_D8link.dylib D8link.so_) and a file (_D8link.h_) that will be called dynamically from the COBOL code. 

Finally, we will create the gRPC server (_main.go_) in the *link_server* directory, which will be in charge of receiving the proto message and calling the target COBOL program.

{{< readfile file="/static/img/include/d8link/d8link_server/main.go" code="true" lang="go" >}}

Try to make remote calls between COBOL programs by exchanging a data area (COPYBOOK).
To do this, remember that

* The calling program must be compiled to produce an executable (option -x GNUCobol).
* The called program must be compiled to produce a shared library (option -m GNUCobol).
* Both programs must be compiled with the same byte order to share binary data.
* To simplify testing, COBOL programs can be located in the directories defined above (*link_client link_server*).

You can use the example COBOL programs loanmain.cbl and loancalc.cbl.

{{< readfile file="/static/img/include/loanmain.cbl" code="true" lang="cobol" >}}

{{< readfile file="/static/img/include/loancalc.cbl" code="true" lang="cobol" >}}