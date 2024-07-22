---
title: Strangler Fig Pattern
date: 2017-01-05
description: >
  How do you safely break down a monolithic architecture?
categories: [Concepts]
weight: 10
---

The IBM mainframe server is a monolithic system, there is no clear separation between the different levels or layers of the technical architecture, all processes reside on the same machine (CICS, batch, database, etc).

Shared memory is used for communication between the different processes (calls between programs, access to the DB2 database, etc.). 
This mechanism has the advantage of being very efficient (necessary in the last century when the cost of computing was very high), but the disadvantage of tightly coupling the processes, making it very difficult to update or replace them. 

This last characteristic makes the most viable alternative for the progressive migration of the functionality deployed on the mainframe to adopt the model described by Matin Fowler as [Strangler Fig](https://martinfowler.com/bliki/StranglerFigApplication.html).



![](/img/others/strangler-pattern-v1.0.jpg)


> 1. Traffic from the channels is gradually moved to an API gateway, which is used to route it to the back-end platforms (mainframe or next-generation)
> 2. The two platforms are connected to enable a phased deployment of applications  
> 3. Gradually migrate the applications until the IBM mainframe server is emptied of its content


#### API Gateway

The connection from the channels is progressively routed to an API gateway.

This API gateway has two main functions:

* On the one hand, we can think of this API Gateway as replacing the functionality provided by the CICS Transactional Monitor, which manages the following:

  * The communication (send/receive) with the channels, we will replace the 4-character transaction codes of CICS with well-formed APIs
  * The authentication process, replacing CESN/CESF and RACF with a mechanism based on LDAP
  * Authorising operations, replacing RACF with a mechanism based on ACLs/RBAC.

* On the other hand, this API gateway is used to route traffic from the channels to the target platform, gradually replacing IBM mainframe server functionality with equivalent next-generation platform functionality.     

#### Architecture for cross-platform coexistence

In order to enable the progressive deployment of functionality, it is necessary to connect the two platforms. These connection mechanisms are essential to avoid "big bang" deployments, to facilitate rollback in the event of problems, to enable parallel deployments, etc, in short, to minimise the risks inherent in a change process such as this. 

There are two basic connection mechanisms

##### DB2 Proxy

z/DB2 provides several mechanisms for accessing DB2 tables using jdbc and odbc drivers.

This is similar to the functionality provided by CICS when connecting to DB2. The DB2 proxy manages a pool of database connections, the identification/authorisation process and the encryption of traffic.

##### CICS/IMS Proxy

Allows transactions to be executed over a low-level connection based on TCP/IP sockets.


#### Deployment of  functionality

When migrating mainframe functionality to a cloud architecture, there are three alternatives.

##### Rebuild 

The functionality can be redesigned, written in a modern programming language (java, python, go, ...) and deployed as a microservice on the next-gen platform.

These new programs can reuse the mainframe platform through the coexistence architecture described above.
* Execute SQL statements accessing DB2 through the DB2 proxy.
* Call the CICS/IM Proxy to invoke a mainframe transaction.

##### Refactor

In this case, we are compiling the COBOL mainframe code on the next-generation platform (Linux-x86/arm) and deploying it as a microservice. This is equivalent to any other microservice built in Java, Python, go, etc.

##### Replace

Invocation of APIs provided by a third-party product that implements the required functionality.

> The above alternatives are not exclusive, different alternatives can be chosen for each functionality or mainframe application, but they all share the same technical architecture, the same pipeline for building and deploying and benefit from the advantages offered by the new technical platform (security, encryption, automation, etc.).

