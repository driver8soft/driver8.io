---
title: Arquitectura Online
date: 2024-06-20
description: >
  ¿Cómo migrar las transacciones CICS/IMS a microservicios?
categories: [Concepts]
weight: 30
---

La respuesta debería ser bastante sencilla, compilando el programa COBOL y desplegando el objeto en un contenedor (por ejemplo, Docker).

Sin embargo, existen dos tipos de sentencias en los programas Online que forman parte del lenguaje y que deben ser pre-procesadas previamente:
* Las sentencias del monitor transaccional utilizado (CICS/IMS)
* Las sentencias de acceso a la base de datos DB2 


#### Sentencias CICS

Los programas Online se despliegan en un monitor transaccional (CICS/IMS), este realiza una serie de funciones que no pueden realizarse utilizando directamente el lenguaje de programación COBOL. 

La principal función sería el envío/recepción de mensajes.

> El lenguaje COBOL tiene sus orígenes a mediados del SXX, cuando todo el procesamiento se realizaba en batch, no existían dispositivos con los que conectarse

El monitor transaccional es por tanto el encargado de gestionar la comunicación. Los programas COBOL definen una estructura de datos fija (COPYBOOK) e incluyen como parte de su código sentencias CICS (EXEC CICS SEND/RECEIVE) para el envío o recepción de mensajes de aplicación.

> El monitor transaccional utilizará la dirección (puntero) y longitud de la COPYBOOK para leer/escribir sobre ella el mensaje

El [modelo de microservicios](/docs/4-concepts/hexagonal) propuesto se comporta de manera equivalente al CICS/IMS, extendiendo las capacidades del gRPC/proto al lenguaje COBOL.

* Las COPYBOOK del programa COBOL (datos en LINKAGE SECTION) utilizadas para enviar/recibir mensajes se transforman en mensajes proto

* Un controlador especializado se encarga de gestionar la interfaz gRPC (gRPC server)

* El mensaje se convierte de formato proto a COPYBOOK. Se transforman los datos del mensaje (string, int, float, etc.) a datos COBOL (CHAR, DECIMAL, PACKED DECIMAL, etc.)

* Finalmente se usa Go cgo para cargar el programa COBOL y ejecutarlo pasándole la estructura de datos generada 


![](/img/others/arch-online-v1.0.png)


1. La solución permite la codificación de servicios en lenguajes modernos y atractivos para los desarrolladores. A su vez permite el aprovechamiento de piezas codificadas en lenguajes “legacy” cuya recodificación resultaría en un gasto de recursos innecesario

2. La comunicación entre los diferentes servicios, comunicación interna, se implementa mediante un protocolo ligero y eficiente

3. Los servicios son invocados desde frontales o sistemas de terceros (plataformas de pagos de terceros, software de partners u otras entidades, etc.) a través de un mecanismo de exposición securizado, resiliente y fácilmente escalable

4. La operación está soportada por pipelines para la automatización de los despliegues y capacidades avanzadas de observabilidad que permiten una visión integrada y consistente de todo el flujo aplicativo y del estado de salud de los elementos involucrados

> El resto de sentencias CICS comunes en los programas de aplicación (ASKTIME/FORMATTIME, LINK, READQ TS, WRITEQ TS, RETURN, etc) pueden sustituirse directamente por código COBOL (ASKTIME, RETURN, LINK) o por llamadas a utilidades desarrolladas en Go cgo.  


#### Sentencias DB2

Las sentencias de acceso a la base de datos DB2 son de tipo estático.

Estas deben pre-compilarse, existiendo dos opciones:

* Utilizar el pre-procesador proporcionado por el fabricante de la base de datos (por ejemplo, Oracle Pro*COBOL) 

* Pre-procesar las sentencias DB2 (EXEC SQL) para utilizar el Proxy de acceso a base de datos SQL proporcionado por la arquitectura de convivencia