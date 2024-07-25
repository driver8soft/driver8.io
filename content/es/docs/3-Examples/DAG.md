---
title: JCL to DAG
date: 2024-06-20
description: >
  Transforme un JCL en un fichero de configuración para ejecutar un programa batch.
categories: [Examples]
tags: [test, sample, docs]
weight: 60
---

Vamos a convertir un paso de un JCL en un fichero de configuración (yaml).

{{< readfile file="/static/img/include/d8batch/job1.jcl" code="true" lang="jcl" >}}

Cree un fichero _step.yaml_ y copie el siguiente código.

{{< readfile file="/static/img/include/d8batch/step.yaml" code="true" lang="yaml" >}}

A continuación ejecutaremos un programa batch de lectura/escritura de ficheros usando esta configuración.
El programa principal **bcuota.cbl** lee un fichero de entrada, llama a la rutina COBOL **loancalc.cbl** para calcular la cuota a pagar de un préstamo y escribe el resultado en el fichero de salida.

{{< readfile file="/static/img/include/d8batch/bcuota.cbl" code="true" lang="cobol" >}}

> La rutina *loancalc.cbl* se ha modificado para eliminar la escritura en el log del sistema

{{< readfile file="/static/img/include/d8batch/loancalc.cbl" code="true" lang="cobol" >}}

>Compile ambos programas para generar una librería compartida (*.so, *dylib)
>
> *cobc -m bcouta.cbl loancalc.cbl*

El controlador d8parti será el encargado de reemplazar al JES, a continuación se muestra una versión simplificada de dicho módulo, cree un fichero _d8parti.go_ y copie el siguiente código.

{{< readfile file="/static/img/include/d8batch/d8parti.go" code="true" lang="go" >}}

Para ejecutar el programa COBOL batch de pruebas, simplemente abra una consola y ejecute lo siguiente:

```
go run d8parti.go

```

¿Como crear un fichero de entrada de ejemplo (_infile_)?

El formato del fichero de entrada es muy sencillo

```cobol
01 WS-LOAN.
               05 WS-ACC  PIC X(10).
               05 FILLER  PIC X(1).
               05 WS-AMT  PIC 9(7).
               05 FILLER  PIC X(1).
               05 WS-INT  PIC 9(2)V9(2).
               05 FILLER  PIC X(1).
               05 WS-YEAR PIC 9(2).
```


Consta de un número de cuenta (10 bytes), un importe (7 bytes), un tipo de interés (4 bytes con dos posiciones decimales) y un periodo en años (2 bytes). Los campos se delimitan mediante un separador (FILLER 1 byte) para facilitar la lectura del fichero de entrada.

Puede utilizar el siguiente programa de ejemplo para generar el fichero de entrada.

{{< readfile file="/static/img/include/d8batch/rand.go" code="true" lang="go" >}}
