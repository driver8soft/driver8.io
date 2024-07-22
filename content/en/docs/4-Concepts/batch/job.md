---
title: Converting JCLs
date: 2017-01-05
description: >
 How to convert a JCL mainframe into a DAG?
categories: [Concepts]
weight: 41
---

Below is a simple example of how to convert a JCL into an Argo workflow (yaml).

Other frameworks or tools that allow the definition of DAGs and have native integration with the Kubernetes platform can be used.

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

> Batch ETL process divided into three phases:
> * The extraction of information from a set of DB2 tables (template extractor).
> * Transforming and aggregating these tables using COBOL applications (template exec).
> * Loading the resulting information (template loader) 

Each JOB is transformed into a DAG in which the sequence of tasks (STEPs) to be executed and their dependencies are defined.

Similarly to PROCS in the mainframe, it is possible to define templates with the main types of batch tasks of the installation (DB2 data download, execution of COBOL programs, file transfer, data conversion, etc.).

Each STEP within the DAG is executed in an independent container on a Kubernetes cluster.

Dependencies are defined at the task level in the DAG and non-linear execution trees can be built.

![](/img/others/argo-v1.0.png)

> Result of the execution of the process, graphically displayed in Argo