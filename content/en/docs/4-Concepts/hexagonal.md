---
title: Microservice model
date: 2024-06-20
description: >
  What microservices model do we need for COBOL?
categories: [Concepts]
weight: 20
---

The model for building microservices must allow:

* The use of different programming languages (including COBOL)
* Be interoperable (with each other and with mainframe logic)
* Migrate data between platforms (mainframe DB2 / next-gen SQL). 

To do so, the Hexagonal Architecture will be used as a reference model.



![](/img/others/hexagonal-v1.0.jpg)


Looking at the left-hand side of the model, the application programs are decoupled from the interface used to execute them. This concept should be familiar as it is the model used to build COBOL applications on an IBM mainframe.

The COBOL language originated in the 1960s when all processing was done in batch. IBM later developed its CICS/IMS transactional monitors to allow COBOL programs to connect to devices in its SNA communications architecture.

COBOL programs only handle data structures (COBOL COPYBOOKS) and it is the transactional monitor that manages the communication interface (LU0, LU2, Sockets, MQSeries, etc.).

Similarly, the business functionality implemented in microservices is independent of the interface used to invoke it through a specific controller.

This allows us to reuse application logic from different interfaces:

* REST API (json)
* gRPC API (proto)
* Events (Kafka consumers)
* Console (batch processes)
* Etc. 

> COBOL programs are perfectly adapted to this model, only a conversion process from the chosen interface (json / proto) to a COPYBOOK structure is required.

On the right-hand side of the model, the business logic should be agnostic to the infrastructure required for data retrieval.

Although this model has obvious advantages, the level of abstraction and complexity to be introduced in the design and construction of the microservices is high, which leads us to make a partial implementation of the model, focusing on two relevant aspects that provide value;

##### SQL databases

The mainframe DB2 is accessed through a proxy.

This proxy exposes a gRPC interface to allow calls from microservices written in different programming languages.

The same mechanism is replicated for access to other SQL database managers (e.g. Oracle or PostgreSQL).

Cross-platform data migration (e.g. from DB2 to Oracle) is facilitated by configuring the target data source in the microservice.

##### Calling CICS/IMS transactions 

In this case, CICS/IMS programs are exposed as microservices (http/REST or gRPC), facilitating their subsequent migration as long as the data structure handled by the program does not change.