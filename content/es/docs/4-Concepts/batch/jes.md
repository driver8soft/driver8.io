---
title: Replicar JES
date: 2024-06-20
description: >
 ¿Cómo replicar la funcionalidad del JES?
categories: [Concepts]
weight: 42
---

Si está familiarizado con [The Twelve-Factor App](https://12factor.net/es/), conocerá que uno de sus principios pasa por independizar el código de aplicación de cualquier elemento que pueda variar en el despliegue del mismo en distintos entornos (pruebas, calidad, producción, etc).

> _Guardar la configuración en el entorno_
>
> La configuración de una aplicación es todo lo que puede variar entre despliegues (entornos de preproducción, producción, desarrollo, etc)
>
> [The Twelve-Factor App. III Configuraciones](https://12factor.net/es/config)


Podemos asimilar la información contenida en los JCLs a ficheros de configuración (config.yml) que contendrán la información necesaria para la ejecución del código en cada uno de los entornos definidos en la instalación (recursos asignados, conexión a la base de datos, nombre y localización de los ficheros de entrada/salida, nivel de detalle del log, etc). 


Para entender qué funcionalidad debemos replicar vamos a dividir un JCLs en dos partes:
* Ficha JOB
* Ficha EXEC y DD

```jcl

//JOB1    JOB (123),CLASS=C,MSGCLASS=S,MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*
//STEP01   EXEC PGM=BCUOTA
//INPUT1   DD   DSN=DEV.APPL1.SAMPLE,DISP=SHR
//OUTPUT1  DD   DSN=DEV.APPL1.CUOTA,
//              DISP=(NEW,CATLG,DELETE),VOLUME=SER=SHARED,
//              SPACE=(CYL,(1,1),RLSE),UNIT=SYSDA,
//              DCB=(RECFM=FB,LRECL=80,BLKSIZE=800)
//*

```


#### Ficha JOB

En la ficha JOB, vamos a encontrar la información básica para el “scheduling” del proceso en Kubernetes:
* Información para la clasificación del JOB (CLASS). Permite separar los tipos de JOBs en función de sus características y asignarles parámetros de ejecución distintos
* Definir la salida por defecto (MSGCLASS)
* El nivel de información que se enviará a la salida (MSGLEVEL)
* Cantidad de memoria máxima asignada al proceso (REGION)
* Tiempo máximo estimado para la ejecución del proceso (TIME)
* Información de usuario (USER)
* Etc.

En Kubernetes, el componente kube-scheduler será el encargado de realizar estas tareas, buscando un nodo con las características adecuadas para la ejecución de los pods recién creados. Existen distintas opciones;
* Los procesos Batch pueden usar el Job controller de Kubernetes, este ejecutara un pod por cada paso (STEP) del workflow y lo detendrá una vez finalizada la tarea  
* En caso de necesitar funcionalidad más avanzada, por ejemplo para la definición y priorización de distintas colas de ejecución, pueden utilizarse “schedulers” especializados como Volcano
* Por último, es posible desarrollar un controlador kubernetes que se adapte a las necesidades específicas de una instalación  

#### Ficha EXEC y DD

En cada paso (STEP) del JCL podemos encontrar una ficha EXEC y varias fichas DD.

En estas se define el programa (COBOL) a ejecutar y los ficheros de entrada/salida asociados.
A continuación se muestra un ejemplo de cómo transformar un paso (STEP) de un JCL.


```yaml
---
stepname: "step01"
exec:
 pgm: "bcuota"
dd:
 - name: "input1"
   dsn: "dev/appl1/sample.txt"
   disp: "shr"
   normaldisp: "catlg"
   abnormaldisp: "catlg"
 - name: "output1"
   dsn: "dev/appl1/cuota.txt"
   disp: "new"
   normaldisp: "catlg"
   abnormaldisp: "delete"

```

Para la ejecución de programas, las sentencias EXEC y DD se convierten a YAML. Esta información se pasa al controlador _d8parti_, especializado en la ejecución de programas Batch.

El controlador _d8parti_ actúa como el JES:
* Se encarga de validar la sintaxis del fichero YAML
* Asigna los nombres simbólicos en los programas COBOL a ficheros físicos de entrada/salida
* Carga el programa COBOL en memoria para su ejecución
* Escribe información de monitorización/logging


