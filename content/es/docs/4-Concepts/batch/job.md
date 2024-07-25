---
title: Conversión JCLs
date: 2024-06-20
description: >
 ¿Cómo convertir un JCL mainframe en un DAG?
categories: [Concepts]
weight: 41
---

A continuación se muestra un sencillo ejemplo de cómo convertir un JCL a un workflow de Argo (yaml).

Pueden utilizarse otros frameworks o herramientas que permitan la definición de DAGs y tengan una integración nativa con la plataforma Kubernetes.

```yaml

apiVersion: argoproj.io/v1alpha1
kind: Workflow
metadata:
 name: batch-job-example
spec:
 entrypoint: job
 templates:
   - name: job
     dag:
       tasks:
         - name: extracting-data-from-table-a
           template: extractor
           arguments:
         - name: extracting-data-from-table-b
           template: extractor
           arguments:
         - name: extracting-data-from-table-c
           template: extractor
           arguments:
         - name: program-transforming-table-c
           dependencies: [extracting-data-from-table-c]
           template: exec
           arguments:
         - name: program-aggregating-data
           dependencies:
             [
               extracting-data-from-table-a,
               extracting-data-from-table-b,
               program-transforming-table-c,
             ]
           template: exec
           arguments:
         - name: loading-data-into-table1
           dependencies: [program-aggregating-data]
           template: loader
           arguments:
         - name: loading-data-into-table2
           dependencies: [program-aggregating-data]
           template: loader
           arguments:
   - name: extractor
   - name: exec
   - name: loader

```

> Proceso Batch de ETL dividido en tres fases:
> * La extracción de información desde un conjunto de tablas DB2 (template extractor)
> * La transformación y agregación de las mismas mediante programas de aplicación COBOL (template exec)
> * La carga de la información resultante (template loader) 

Cada JOB se transforma en un DAG en el que se define la secuencia de pasos (STEPs) a ejecutar y sus dependencias

Es posible definir “templates” con los principales tipos de tareas Batch de la instalación (descarga datos DB2, ejecución de programas COBOL, transmisión de ficheros, conversión de datos, etc.) de manera equivalente a los PROCS en mainframe

Cada STEP dentro del DAG es ejecutado en un contenedor independiente en un cluster Kubernetes

Las dependencias se definen a nivel de tarea en el DAG, pudiendo establecer árboles de ejecución no lineales

![](/img/others/argo-v1.0.png)

> Resultado de la ejecución del proceso representado de manera gráfica en Argo