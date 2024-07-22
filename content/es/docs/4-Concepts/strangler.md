---
title: Strangler Fig Pattern
date: 2017-01-05
description: >
  ¿Cómo desmontar una arquitectura monolítica de manera segura?
categories: [Concepts]
weight: 10
---

El servidor IBM mainframe es un sistema monolítico, no existe una separación clara entre los distintos niveles o capas de la arquitectura técnica, todos los procesos residen en la misma máquina (CICS, Batch, Base de Datos, etc).

La comunicación entre los distintos procesos (llamadas entre programas, acceso a la base de datos DB2, etc) se realiza mediante el uso de memoria compartida, este mecanismo presenta la ventaja de ser muy eficiente (necesaria el siglo pasado cuando los costes de computación eran muy elevados) con la contrapartida de acoplar fuertemente los procesos haciendo muy difícil su actualización o sustitución. 

Esta última característica hace que la alternativa más viable para la migración progresiva de la funcionalidad desplegada en el Mainframe sea adoptar el modelo descrito por Matin Fowler como [Strangler Fig](https://martinfowler.com/bliki/StranglerFigApplication.html).



![](/img/others/strangler-pattern-v1.0.jpg)



> 1. El tráfico desde los canales, se mueve progresivamente hacia un API Gateway, que servirá para enrutar este hacia las plataformas de back-end (Mainframe o Next-gen)
> 2. Se conectan ambas plataformas para permitir la realización de un despliegue de aplicaciones faseado  
> 3. Se migran progresivamente las aplicaciones hasta vaciar de contenido el servidor IBM mainframe

#### API Gateway

La conexión desde los canales se deriva de manera progresiva hacia un API Gateway.

Este API Gateway cumple dos funciones principales:

* Por un lado, podemos entender este API Gateway como el sustituto de la funcionalidad proporcionada por el Monitor Transaccional CICS, gestionando;
  * La comunicación (envío/recepción) con los canales, vamos a sustituir los códigos de transacción de 4 caracteres del CICS por APIs bien formadas
  * El proceso de identificación (authentication), sustitución de CESN/CESF y RACF por un mecanismo basado en LDAP
  * La autorización de operaciones, sustitución del RACF por un mecanismo basado en ACLs/RBAC

* Por otro lado, este API Gateway nos servirá para dirigir (“Route”) el tráfico desde los canales hacia la plataforma destino, sustituyendo de manera progresiva la funcionalidad del servidor IBM mainframe por funcionalidad equivalente en la plataforma Next-gen.     

#### Arquitectura de convivencia entre plataformas

Para permitir el despliegue progesivo de funcionalidad es necesario conectar ambas plataformas, estos mecanismos de conexión son esenciales para evitar la realización de despliegues “big-bang”, facilitar la marcha atrás en caso de problemas, permitir la realización de paralelos, etc, en definitiva, minimizar los riesgos inherentes a un proceso de cambio como el planteado. 

Existen dos mecanismos básicos de conexión;

##### Proxy acceso DB2 

El z/DB2 ofrece varios mecanismos de acceso mediante drivers jdbc y odbc.

De manera equivalente a la funcionalidad proporcionada por el CICS en su conexión al DB2, el proxy DB2 gestiona un pool de conexiones a la base de datos, el proceso de identificación/autorización y el cifrado del tráfico.

##### Proxy CICS/IMS

Permite la ejecución de transacciones mediante una conexión de bajo nivel basada en TCP/IP Sockets.

#### Despliegue de funcionalidad

Existen tres alternativas para la migración de la funcionalidad mainframe hacia una arquitectura Cloud.

##### Rebuild 

La funcionalidad puede rediseñarse, escribirse en un lenguaje de programación moderno (java, python, go, …) y desplegarse como un microservicio en la plataforma Next-gen.

Estos nuevos programas pueden reutilizar la plataforma mainframe mediante la arquitectura de convivencia descrita anteriormente.
* Ejecución de sentencias SQL de acceso al DB2 a través del proxy DB2
* Invocación de una transacción mainframe mediante la llamada al proxy CICS/IMS

##### Refactor

En este caso, compilamos el código COBOL mainframe sobre la plataforma Next-gen (Linux-x86/arm) y lo desplegamos como un microservicio, de manera equivalente a un cualquier otro microservicio construido en java, python, go, etc.

##### Replace

Llamando a las APIs proporcionadas por un producto de terceros que implemente la funcionalidad requerida.

> Las anteriores alternativas no son excluyentes, pueden seleccionarse diferentes alternativas para cada funcionalidad o aplicativo mainframe, sin embargo todas comparten la misma arquitectura técnica, el mismo pipeline para la construcción y despliegue y se benefician de las ventajas proporcionadas por la nueva plataforma técnica  (seguridad, cifrado, automatización, etc.).


