---
title: Playing with PostgreSQL
date: 2024-09-05
description: >
  A COBOL PostgreSQL example.
categories: [Examples]
tags: [test, sample, docs]
weight: 30
---

Is COBOL only valid for accessing DB2?

In this simple example, we will access a PostgreSQL database from a COBOL program, using direct calls to its library. 

> As an alternative, your programs can be precompiled (EXEC SQL) to access various SQL databases.  
> This needs an additional precompiler.   

To run this program, you need to install PostgreSQL and create the sample database (dvdrental). Instructions on how to do this can be found [here](https://www.postgresql.org/download/). 

{{< readfile file="/static/img/include/pgcobol.cbl" code="true" lang="cobol" >}}

>Remember to modify the WORKING CONN-STR fields with a valid username and password for the database connection

The functions used by the COBOL program require the PostgreSQL library _'libpq'_, find out where this library is installed and add it when compiling the program, for example:

`cobc -x pgcobol.cbl -L/Library/postgreSQL/16/lib -lpq`
