---
title: JCL to DAG
date: 2024-06-20
description: >
  How to convert a JCL into a configuration file in order to run a batch program.
categories: [Examples]
tags: [test, sample, docs]
weight: 60
---

We are going to convert a JCL step into a configuration file (yaml).

{{< readfile file="/static/img/include/d8batch/job1.jcl" code="true" lang="jcl" >}}

Create a _step.yaml_ file and copy and paste the following code into it.

{{< readfile file="/static/img/include/d8batch/step.yaml" code="true" lang="yaml" >}}

Next, using this configuration yaml, we will run a batch file read/write program.
The main program **bcuota.cbl** reads an input file, calls the COBOL routine **loancalc.cbl** to calculate the loan quota, and writes the result to the output file.

{{< readfile file="/static/img/include/d8batch/bcuota.cbl" code="true" lang="cobol" >}}

> The *loancalc.cbl* routine has been modified to avoid writing to the system log.

{{< readfile file="/static/img/include/d8batch/loancalc.cbl" code="true" lang="cobol" >}}

> Compile both programs to create a shared library (*.so, *dylib).
>
>*cobc -m bcouta.cbl loancalc.cbl*.

The d8parti controller will replace the JES mainframe subsystem, here is a simplified version of this module, create a _d8parti.go_ file and copy the following code.

{{< readfile file="/static/img/include/d8batch/d8parti.go" code="true" lang="go" >}}

How do I create a sample input file (_infile_)?

The input file format is very simple.

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

An account number (10 bytes), an amount (7 bytes), an interest rate (4 bytes with two decimal places) and a period of time in years (2 bytes). The fields are delimited by a separator (FILLER 1 byte) to make the input file easier to read.

You can use the following example program to create the input file.

{{< readfile file="/static/img/include/d8batch/rand.go" code="true" lang="go" >}}
