---
title: Overview
description: Deploy your legacy mainframe code as microservices on a hybrid cloud architecture.
weight: 1
---

### What is driver8?

driver8 is a project that makes it possible to re-use the assets that were developed on an IBM mainframe architecture on a cloud architecture. 

There are several alternatives for the migration of mainframe applications to a native cloud architecture:

* _Rebuid_
* _Refactor_
* _Replace_

driver8 makes it possible to combine the above alternatives according to the needs of the project.

##### Rebuild

Mainframe applications can be redesigned, rewritten (java, python, go, ...) and deployed as microservices. 
By enabling cross-platform coexistence through two basic mechanisms, these new microservices can be deployed incrementally;

* Real-time data access (proxy to DB2 z/OS database).
* Execution of transactions under the CICS / IMS monitors.

##### Refactor

COBOL programs can be recompiled and exposed as microservices. 
These microservices can communicate with any other microservice, regardless of the programming language used (COBOL, java, python, go,...) and inherit all the functionalities of the technical platform (authorisation, encryption, monitoring, automation of deployments, etc).

##### Replace

Finally, the functionality can be replaced by a third-party software. Microservices can easily communicate with the APIs provided by the product.

### Why migrate transactions and batch processes to a cloud platform?

Code developed on a mainframe server still implements critical functions in many organisations. In the financial market (retail banking), most accounting processes and product operations are still based on IBM mainframe technology.
In many cases, the cost, risk and timeframe of rewriting thousands of programs or replacing such functionality with a third party product is not feasible.

In this case, the code can be reused on an open platform. This allows you to take advantage of a hybrid cloud architecture.

#### Which applications are best suited for migration?

Any online or batch mainframe application is suitable for migration. However, certain applications are easier to migrate. 
Use the following list to identify the best candidates for migration;

* Use technical architectures in the CICS/IMS transactional monitors

>The purpose of a technical architecture is to isolate the application programs from the complexities of the monitor being used (sending and receiving messages, logging, error handling, etc).
>
>The programs do not execute CICS/IMS statements and can be compiled directly.

* Data stored in DB2

>Using product drivers (jdbc, odbc), DB2 provides real-time access to data. 
>
>SQL statements can be pre-processed to access a non-Mainframe relational database manager (e.g. PostgreSQL).

* Batch processing using sequential files (QSAM)

> Files can be easily transferred and processes can be run in parallel on both platforms to check output results.


#### What do you need to consider before you start the migration?
 
##### Application Inventory

The existence of thousands of components (source code, copies, JCLs, etc.) in the mainframe change management tool does not mean that all of these components are still in use. 

Define the scope of the project by creating a detailed inventory of active software components.

##### Analyse the relationships between the components 

By analysing the relationships between components, it is possible to identify application repos that will be deployed together at a later stage.

##### Lack of knowledge, lack of documentation

Although it is possible to test the migrated functionality by performing a binary check of the process results (messages, files, etc.), the applications must be maintained in the new technical architecture. 

A well-trained team is essential to ensure the success of the migration project and the future evolution of the software, except for sunset applications.

##### VSAM files

This type of file can be replicated on the target platform (Berkley DB). However, it adds a level of complexity that could be avoided by migrating the data and applications to a SQL database. 

The fields in the file are converted to columns (SQL database) and depending on the file type:

* KSDS, the key is converted into a unique cluster index
* RRDS, in which a counter is used as the cluster index
* ESDS, in this case the table is sorted using a TIMESTAMP

##### Other Programming Languages

Although most of the development work on the mainframe platform is carried out using the COBOL programming language, there may be applications that use programs or routines written in any of the following languages:

* PL/I. PL/I programs can be compiled and executed as microservices using the same architecture as COBOL programs. However, there is currently no stable open PL/I compiler. 
It is therefore necessary to use a licensed product from a third party vendor.
* Assembler. Assembler routines must be converted to C or Go code.

##### EBCDIC

The mainframe platform uses EBCDIC. Although it is possible to continue using EBCDIC on the Linux platform (intel/arm) in order to minimise changes to the migrated COBOL programs, this option presents serious compatibility and development problems, so we discourage its use.

COBOL programs must be checked for statements that use EBCDIC characters (_VARIABLE PIC X(3) VALUE X'F1F2F3'_).

##### 3270 applications

In conversational or pseudo-conversational transactions, the execution/navigation flow and the presentation layer are coded in mainframe programs. Although it is possible to build a "3270 controller", this type of transactions has limited usability due to the protocol they implement (SNA LU2). 

In digital channels, where user experience is a critical issue in the design of the solution, they cannot be reused. 


#### Technology not supported

* IMS/DB database manager. 
* 4GL tools, Natural/Adabas, IDMS, CA-gen, etc.

If you are interested in any of these technologies, or would like to help develop them, please let us know.

#### If you want to know more about driver8

* [Get started with driver8](/docs/2-getting-started/): Develop your first COBOL API
* [Examples](/docs/3-examples/): COBOL example programs!

