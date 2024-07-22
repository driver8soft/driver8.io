---
title: Introducción
description: Despliegue su código COBOL como microservicios en una arquitectura de Cloud híbrida
weight: 1
---

### ¿Qué es driver8?

driver8 es un proyecto que permite reutilizar los activos desarrollados en una arquitectura IBM mainframe para ejecutarlos en una arquitectura Cloud.

Existen distintas alternativas para la migración de aplicaciones mainframe a una arquitectura cloud nativa:

* _Rebuid_
* _Refactor_
* _Replace_

driver8 permite combinar las anteriores alternativas, de acuerdo a las necesidades del proyecto

##### Rebuild

Las aplicaciones mainframe pueden ser rediseñadas y reescritas (java, python, go, ...) como microservicios. Estos nuevos microservicios pueden desplegarse gradualmente al permitirse la convivencia entre plataformas mediante dos mecanismos básicos;
* El acceso a los datos en tiempo real (proxy acceso a la base de datos mainframe DB2)
* La ejecución de transacciones bajo los monitores CICS/IMS  

##### Refactor

El código COBOL puede recompilarse y exponerse como microservicios. Estos microservicios heredan toda la funcionalidad de la plataforma técnica (autorización, cifrado, monitorización, automatización despliegues, etc) y pueden comunicarse con cualquier otro microservicio independientemente del lenguaje de programación utilizado (COBOL, java, python, go,...)  

##### Replace

Por último, la funcionalidad puede sustituirse por un paquete o desarrollo de terceros. Los microservicios pueden comunicarse de manera sencilla con las APIs proporcionadas por el producto

### ¿Por qué migrar transacciones y procesos batch a una plataforma Cloud?

El código desarrollado en un servidor mainframe sigue implementando funciones críticas en muchos negocios, por ejemplo en la industria financiera la mayoría de los procesos contables y la operativa de productos, sigue basándose en tecnología IBM mainframe
La reescritura de miles de programas o la sustitución de dicha funcionalidad por un producto de terceros es en muchos casos inviable por el coste, riesgo y plazo de la iniciativa

En este caso, el código puede reutilizarse en una plataforma open para beneficiarse de las ventajas que ofrece una arquitectura cloud híbrida

#### ¿Qué aplicaciones son ideales para su migración?

En principio cualquier aplicación mainframe online o batch es susceptible de ser migrada, sin embargo, determinadas aplicaciones son más sencillas de migrar. A continuación se enumeran una serie de criterios sencillos que pueden tenerse en cuenta para determinar las aplicaciones candidatas a ser migradas;

* La existencia de arquitecturas técnicas en los monitores transaccionales CICS/IMS

>La existencia de una arquitectura técnica independiza a los programas de aplicación de las complejidades del monitor utilizado (send/receive de mensajes, logging, gestión errores, etc.).
>
>Los programas no ejecutan sentencias CICS/IMS y pueden ser compilados directamente.

* Datos almacenados en DB2

> Facilita el acceso en tiempo real a los datos mediante los drivers del producto (jdbc, odbc), así como la conversión de las sentencias SQL a otro gestor de base de datos relacional.

* Procesamiento batch mediante ficheros secuenciales (QSAM)

> Los ficheros pueden transmitirse fácilmente y ejecutar los procesos en paralelo en ambas plataformas para comprobar los resultados 

#### ¿Qué debería analizar antes de su migración?

##### Inventario de aplicaciones 

La existencia de miles de componentes (fuentes, copys, JCLs, etc) en la herramienta de gestión de cambios mainframe no implica que dichos componentes estén en uso.

Es necesario realizar un inventario detallado de los componentes activos en la instalación antes de proceder a la migración de los mismos.

##### Relación entre componentes

Las relaciones entre componentes permiten identificar "repos" aplicativos que posteriormente serán desplegados de manera conjunta

##### Desconocimiento, ausencia de documentación

Aunque es posible realizar un paralelo de la funcionalidad migrada mediante la comparación binaria de las salidas del proceso (mensajes, ficheros, etc), la aplicación deberá seguir manteniéndose en la nueva arquitectura técnica.

Salvo aplicaciones en sunset sin modificaciones, el disponer de un equipo formado es imprescindible para garantizar con éxito el mantenimiento correctivo/evolutivo del software así como para facilitar el proyecto de migración.

##### Ficheros VSAM

El funcionamiento de este tipo de ficheros puede reproducirse en la plataforma destino (Berkley DB), sin embargo suponen un nivel de complejidad adicional que podría evitarse migrando los datos y aplicaciones hacia una base de datos SQL.

Los campos del fichero se convierten a columnas y dependiendo del tipo de fichero:
  * KSDS, las clave se transforma en un índice único cluster
  * RRDS, se usa como índice cluster un contador
  * ESDS, en este caso de ordena la tabla mediante un TIMESTAMP

##### Otros lenguajes de programación

Aunque la mayoría de los desarrollos sobre la plataforma mainframe se realizan usando el lenguaje de programación COBOL, pueden existir aplicaciones que usen programas o rutinas escritas en:
  * PL/I. Los programas PL/I pueden compilarse y ejecutarse como microservicios usando la misma arquitectura utilizada por los programas COBOL, sin embargo no existe un compilador PL/I "open" siendo necesario utilizar un producto de terceros con licencia
  * Assembler. Las rutinas assembler deben ser convertidas a código C o Go

##### EBCDIC

La plataforma mainframe utiliza EBCDIC, aunque es posible seguir utilizando EBCDIC sobre la plataforma Linux (intel/arm) en los programas COBOL migrados, esta opción presenta serios inconvenientes de compatibilidad y evolución por lo que descartamos su uso.

Los programas COBOL deben ser analizados para detectar sentencias que impliquen la utilización de datos o caracteres en EBCDIC (por ejemplo, _VARIABLE PIC X(3) VALUE X'F1F2F3'_).

##### Aplicaciones 3270

En las transacciones conversacionales o pseudo-conversacionales, el flujo de ejecución/navegación y la presentación está codificada en los programas mainframe, aunque es posible construir un "controlador 3270" este tipo de transacciones tienen una usabilidad limitada por el protocolo que implementan (SNA LU2), siendo difícil su reutilización en canales digitales en los que sea importante la experiencia del usuario final   

#### ¿Qué tipo de tecnología no está soportada?

* Gestor de base de datos IMS/DB.
* Herramientas 4GL, Natural/Adabas, IDMS, CA-gen, etc. 

Si está interesado en alguna de estas tecnologías o desea colaborar en el desarrollo de la misma, háganoslo saber

#### Si quiere conocer más sobre driver8

* [Comience a usar driver8](/docs/2-getting-started/): Desarrolle su primera API COBOL
* [Ejemplos](/docs/3-examples/): Algunos programas COBOL de ejemplo!

