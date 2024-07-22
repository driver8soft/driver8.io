---
title: Replicate JES functionality
date: 2017-01-05
description: >
 How to replicate how the JES works?
categories: [Concepts]
weight: 42
---

If you're familiar with [The Twelve-Factor App](https://12factor.net/), you'll know that one of its principles is to make the application code independent of any element that might vary when it's deployed in different environments (test, quality, production, etc.).

> Storing the configuration in the environment
>
> An app’s config is everything that is likely to vary between deploys (staging, production, developer environments, etc)
>
> [The Twelve-Factor App. III Config](https://12factor.net/config)


We can translate the information contained in the JCLs into configuration files (config.yml), which contain the necessary information for running the code in each of the environments defined in the installation (resource allocation, connection to the database, name and location of the input and output files, level of detail of the logging, etc.). 


To understand what functionality we need to replicate, let's divide a JCL into two parts:
* JOB card
* EXEC and DD cards

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


#### JOB card

In the JOB card, we will find the basic information for scheduling the process in Kubernetes:
* Information needed to classify the JOB (CLASS). Allows you to classify the types of JOBs according to their characteristics and assign different execution parameters to them.
* Define default output (MSGCLASS).
* The level of information to be sent to the std out (MSGLEVEL) 
* Maximum amount of memory allocated to the JOB (REGION)
* Maximum estimated time for execution of the process (TIME)
* User information (USER)
* Etc.


In Kubernetes, the kube-scheduler component is responsible for performing these tasks. It searches for a node with the right characteristics to run the newly created pods. 

There are several options;
* Batch processes can use the Kubernetes job controller, it will run a pod for each task (STEP) of the workflow and stop it when the task is completed.  
* If more advanced functionality is required, such as defining and prioritising different execution queues, specialised schedulers such as Volcano can be used.
* Finally, it is possible to develop a Kubernetes controller tailored to the specific needs of an installation.  

#### EXEC & DD cards

In each STEP of the JCL we find an EXEC tab and several DD tabs.

It is in these cards that the (COBOL) program to be executed and the associated input and output files are defined.
Below is an example of how to transform a STEP of JCL.


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

For program execution, EXEC and DD instructions are converted to YAML. This information is passed to the _d8parti_ controller, which specialises in running batch programs.

The _d8parti_ controller acts like the JES:
* It is in charge of the syntax validation of the YAML file
* It maps the symbolic names in COBOL programs to physical input/output files
* Loads COBOL into memory for execution
* Writes monitoring/logging information


