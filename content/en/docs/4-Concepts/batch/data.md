---
title: Data access
date: 2017-01-05
description: >
 How to access data stored in SQL files and databases?
categories: [Concepts]
weight: 45
---

#### Files

In mainframe architecture, a _Data Set_ is a set of related records stored in a UNIT / VOLUME.

To understand these concepts, we need to go back to the days when mass storage devices were based on tapes or cartridges. So when a process needed to access the information in a data set, the tape or cartridge had to be mounted in a UNIT and identified by a name or VOLUME.

> Today, information resides on disk and does not need to be mounted/unmounted for access, we can compare mainframe VOLUMEs to an NFS share.

Different mount points can be defined for the application container to isolate the information and protect access (e.g. by environment, development and production). The containers are accessed via SDS (Software Define Storage) to decouple the storage from the process. 

Finally, the mainframe files need to be transferred and converted (EBCDIC) into Linux files for use on the target platform. This process can be automated using off-the-shelf tools or using Spark data conversion processes.

#### SQL databases

The main mainframe database engine is IBM DB2, although other types of products (IMS DB, IDMS, Adabas) are still in use.

For DB2 applications, there are two main strategies for accessing data:

* Replication of DB2 data on a new SQL database (e.g. PostgreSQL).
* Accessing DB2 on the mainframe platform from the Kubernetes cluster using the Coexistence Proxy (DB2 Proxy).

In the first case, replication tools (e.g. IBM CDC) or ETL processes (e.g. using Spark) are used to replicate the data from the DB2 tables to a new SQL database.

The DB2 SQL statements (EXEC SQL ... END-EXEC.) are pre-compiled to be able to access the new database manager, it is necessary to make small changes in the SQL to adapt it, but there is a methodology and tools to carry out this process automatically:

* DDL replication (tablespaces, tables, indexes, columns, etc.)
* Adapting the DATE/TIME data types.
* SQLCODEs
* Upload and download utilities
* Etc

The main drawback of this strategy is the need to maintain the data integrity of the model, generally the referential integrity model of the database is not defined in the DB2 manager, it must be deduced by the logic of the applications.

All read/update processes that access the affected tables (whether batch or online) must either be migrated to the new platform or a coexistence/replication mechanism must be defined between the platforms (mainframe DB2 / next-gen SQL). This mechanism must maintain data integrity on both platforms until the migration process is complete. 

For tables containing master data accessed by a large number of applications, this coexistence is particularly critical.

There is no need to maintain data integrity between platforms (mainframe / next-gen) if you choose to continue accessing DB2 mainframe through the coexistence proxy. 
Processes (online or batch) can be migrated one at a time and in stages (_canary deployment_).

Once the process of migrating the application programs (Online and Batch) has been completed, the data can be migrated to a new database on the target platform (Next-gen).
