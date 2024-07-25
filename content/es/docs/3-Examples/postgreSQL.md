---
title: Playing with PostgreSQL
date: 2024-06-20
description: >
  Un ejemplo COBOL PostgreSQL.
categories: [Examples]
tags: [test, sample, docs]
weight: 30
---

¿El lenguaje COBOL sólo puede acceder a DB2?

En este sencillo ejemplo accederemos a una base de datos PostgreSQL desde un programa COBOL. 

> Sus programas pueden ser pre-compilados (EXEC SQL) para acceder a distintas bases de datos SQL. 

Para poder ejecutar este programa es necesario instalar PostgreSQL y crear la base de datos de ejemplo (dvdrental). Puede encontrar las instrucciones de como hacerlo [aquí](https://www.postgresql.org/download/). 

{{< readfile file="/static/img/include/pgcobol.cbl" code="true" lang="cobol" >}}

>Recuerde modificar los campos de WORKING CONN-STR con un usuario y password válidos para la conexión a la base de datos

Las funciones utilizadas por el programa COBOL necesitan la librería de postgreSQL _"libpq"_, localice donde está instalada dicha librería y añadala en el momento de compilar el programa, por ejemplo:

`cobc -x pgcobol.cbl -L/Library/postgreSQL/16/lib -lpq`


