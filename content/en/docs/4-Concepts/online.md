---
title: Online Architecture
date: 2017-01-05
description: >
  How do you migrate CICS/IMS transactions to microservices?
categories: [Concepts]
weight: 30
---

The answer should be quite simple, by compiling the COBOL program and deploying the object in a container (e.g. Docker).

However, there are two types of statements in the online programs that are not part of the COBOL language and must be pre-processed:
* The statements of the transactional monitor used (CICS/IMS).
* The access statements to the DB2 database 


#### CICS statatements

Online programs are deployed on a transactional monitor (CICS/IMS) which performs a number of functions that cannot be performed directly using the COBOL programming language. 

The main function would be the sending and receiving of messages.

> The COBOL language has its origins in the mid-20th century when all processing was done in batch, there were no devices to connect to.

Communication is therefore managed by the transactional monitor. COBOL programs define a fixed data structure (COPYBOOK) and include CICS block statements (EXEC CICS SEND/RECEIVE) as part of their code to send or receive application messages.

> The transaction monitor uses the address (pointer) and length of the COPYBOOK to read/write the message to/from it.


The proposed [microservices model](/docs/4-concepts/hexagonal) behaves like CICS/IMS, extending the gRPC/proto capabilities to the COBOL language.

* COBOL program COPYBOOKs (data in LINKAGE SECTION) used to send/receive messages are converted to proto messages.

* The gRPC interface (gRPC server) is managed by a special controller

* The message is converted from proto to COPYBOOK format. The message data (string, int, float, etc.) is transformed to COBOL data (CHAR, DECIMAL, PACKED DECIMAL, etc.) 

* Finally, Go cgo is used to load the COBOL program and execute it by passing the generated data structure to it. 


![](/img/others/arch-online-v1.0.png)


1. The solution allows services to be coded in modern languages that are attractive to developers. At the same time, it allows the use of programs coded in "legacy" languages, whose recoding would result in an unnecessary waste of resources.

2. Internal communication between the different services is implemented using a lightweight and efficient protocol.

3. Services are called from front-ends or third party systems through a secure, resilient and easily scalable exposing mechanism.

4. The operation is supported by automated deployment pipelines and advanced observability capabilities that provide an integrated and consistent view of the entire application flow and the health of the elements involved.

> The remaining CICS statements common to application programs (ASKTIME/FORMATTIME, LINK, READQ TS, WRITEQ TS, RETURN, etc.) can be replaced directly by COBOL code (ASKTIME, RETURN, LINK) or by calling utilities developed in Go cgo.  


#### DB2 statements

DB2 database access statements are of static type.

They must be precompiled. There are two ways to do this:

* Use the pre-processor provided by the database vendor (e.g. Oracle Pro*COBOL). 

* Pre-process the DB2 statements (EXEC SQL) to use the SQL database access proxy provided by the coexistence architecture.