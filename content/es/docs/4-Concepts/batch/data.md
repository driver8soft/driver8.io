---
title: Acceso a los datos
date: 2017-01-05
description: >
 ¿Cómo acceder a los datos almacenados en ficheros y bases de datos SQL?
categories: [Concepts]
weight: 45
---

#### Ficheros

En la arquitectura mainframe un _Data Set_ es un conjunto de registros relacionados que se almacenan en una UNIT / VOLUME.

Para entender estos conceptos hay que volver a retroceder en el tiempo, cuando los dispositivos de almacenamiento masivo estaban basados en cintas o cartuchos. Así, cuando un proceso necesitaba acceder a la información de un Data Set, la cinta o cartucho tenía que ser “montada” en una UNIT y era identificado con un nombre o VOLUME.

> Hoy en día la información reside en disco y no necesita ser montada/desmontada para su acceso, podemos asimilar los VOLUMEs del mainframe a un “share” de un NFS.

Pueden definirse distintos “puntos de montaje” al contenedor aplicativo para aislar la información y proteger su acceso (por ejemplo por entorno, desarrollo y producción). El acceso se ofrece a los contenedores mediante SDS (Software Define Storage) con el objetivo de poder desacoplar el almacenamiento del proceso. 

Por último, los ficheros del mainframe deben transmitirse y convertirse (EBCDIC) a ficheros _"Linux"_ para su utilización en la plataforma destino. Este proceso puede automatizarse mediante herramientas/productos de mercado o usando procesos Spark de conversión de datos.

#### Base de Datos

El principal motor de Base de Datos del mainframe es el IBM DB2, aunque se siguen utilizando de manera residual otro tipo de productos (IMS DB, IDMS, Adabas).

En el caso de las aplicaciones DB2. Existen dos grandes estrategias bien diferenciadas para el acceso a los datos:

* Réplica de los datos DB2 sobre una nueva Base de Datos SQL (por ejemplo, PostgreSQL) 
* Acceso al DB2 de la plataforma mainframe desde el cluster Kubernetes mediante el proxy de convivencia

En el primer caso, los datos de las tablas DB2 se replican sobre un nuevo gestor usando herramientas de replicación (i.e. IBM CDC) o procesos ETL (por ejemplo usando Spark).

Las sentencias SQL DB2 (EXEC SQL … END-EXEC.) se pre-compilan para poder acceder al nuevo gestor de base de datos, es necesario realizar pequeños cambios en el SQL para adaptarlo, sin embargo existe metodología y herramientas para la realización de este proceso de manera automática:

* Réplica del DDL (Tablespaces, Tablas, Índices, Columnas, etc.)
* Adaptación tipos de datos DATE/TIME
* SQLCODEs
* Utilidades de carga y descarga
* Etc

El principal inconveniente de esta estrategia es la necesidad de mantener la integridad de datos del modelo, generalmente el modelo de integridad referencial de la base de datos no está definido en el gestor DB2, debe deducirse por la lógica de las aplicaciones.

Todos los procesos de lectura/actualización que accedan a las tablas afectadas (independientemente de si son Batch/Online) deben ser migrados a la nueva plataforma o definir un mecanismo de convivencia/réplica entre plataformas (Mainframe DB2 / Next-gen SQL) que mantenga la integridad de los datos en ambas hasta la finalización del proceso de migración. 

Esta convivencia se hace especialmente crítica en el caso de tablas con datos maestros accedidos por un elevado número de aplicaciones.

En caso de optar por seguir accediendo al DB2 mainframe mediante el proxy de convivencia no es necesario mantener la integridad de los datos entre plataformas (Mainframe / Next-gen). Los procesos (Online o Batch) se pueden migrar individualmente y de manera progresiva (_canary deployment_)

Una vez finalizado el proceso de migración de los programas de aplicación (Online y Batch) puede realizarse una migración de datos hacia una nueva Base de Datos en la plataforma destino (Next-gen).
