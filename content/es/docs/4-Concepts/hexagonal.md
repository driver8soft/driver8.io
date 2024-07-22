---
title: Modelo de microservicios
date: 2017-01-05
description: >
  ¿Qué modelo de construcción de microservicios necesitamos para ejecutar programas COBOL?
categories: [Concepts]
weight: 20
---

El modelo de construcción de microservicios debe permitir:

* El uso de distintos lenguajes de programación (incluido COBOL)
* Que los microservicios sean interoperables (entre ellos y con la lógica del mainframe)
* La migración de datos entre plataformas (Mainframe DB2 / Next-gen SQL) 

Para ello, vamos a utilizar como modelo de referencia la Hexagonal architecture.



![](/img/others/hexagonal-v1.0.jpg)



Si observamos la parte izquierda del modelo, los programas de aplicación están desacoplados de la interfaz utilizada para su ejecución. Este concepto nos debe resultar familiar ya que es equivalente al modelo utilizado en la construcción de aplicaciones COBOL en un mainframe IBM.

El lenguaje COBOL tiene sus orígenes en los años 60 del siglo pasado, cuando todo el procesamiento se realizaba en Batch. IBM desarrolla posteriormente sus monitores transaccionales CICS/IMS para permitir la conexión de los programas COBOL con dispositivos de su arquitectura de comunicaciones SNA.

Los programas COBOL manejan únicamente estructuras de datos (COBOL COPYBOOKS) y es el monitor transaccional el encargado de gestionar la interfaz de comunicación (LU0, LU2, Sockets, MQSeries, etc)

De manera equivalente, la funcionalidad de negocio implementada en los microservicios se independiza de la interfaz utilizada para su invocación, mediante un “controlador” específico.

Esto nos va a permitir la reutilización de la lógica aplicativa desde distintos interfaces, por ejemplo:

* API REST (json)
* API gRPC (proto)
* Eventos (kafka consumer)
* Consola (procesos Batch)
* Etc. 

> Los programas COBOL se adaptan a la perfección a este modelo, solo es necesario un proceso de conversión desde la interfaz seleccionada (json / proto) a una estructura COPYBOOK.

Atendiendo a la parte derecha del modelo, la lógica de negocio debería ser agnóstica de la infraestructura necesaria para la recuperación de los datos.

Si bien este modelo presenta evidentes ventajas, el nivel de abstracción y complejidad a introducir en el diseño y construcción de los microservicios es elevado, lo que nos lleva a hacer una implementación parcial del modelo, centrándonos en dos aspectos relevantes que aportan valor;

##### Bases de Datos SQL

El acceso al DB2 del mainframe se realiza a través de un proxy.

Este proxy expone una interfaz gRPC para permitir su invocación desde microservicios escritos en distintos lenguajes de programación.

Este mismo mecanismo se replica para el acceso a otros gestores de bases de datos SQL (por ejemplo, Oracle o PostgreSQL).

La migración de datos entre plataformas (por ejemplo de DB2 a Oracle) se facilita mediante la configuración en el microservicio del Data Source destino.

##### Invocación de transacciones CICS/IMS 

En este caso, los programas CICS/IMS se exponen como microservicios (http/REST o gRPC), facilitando su posterior migración siempre que se respete la estructura de datos manejada por el programa.
