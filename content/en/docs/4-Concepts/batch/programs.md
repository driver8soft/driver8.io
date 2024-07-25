---
title: Program compilation
date: 2024-06-20
description: >
 How to reuse mainframe application programs?
categories: [Concepts]
weight: 43
---

The mainframe COBOL PL/I programs are directly reusable on the targeted technical platform (Linux).

As mentioned above, the _d8parti_ module will be responsible for the following tasks

* Initialise the language runtime (i.e. COBOL)
* Assign the input/output files to the symbolic names of the program
* Loading and execution of the main program (defined in the EXEC tab of the JCL)


> This main program can make various calls to other subroutines using a CALL statement. These calls are managed by the runtime of the language used.


We can visualise this operation as an inverted tree


![](/img/others/arch-batch2-v1.0.png)


Compiled programs can be stored in a shared directory and loaded at runtime (dynamic CALL), mimicking the IBM mainframe (STEPLIB). 

However, it is possible to change the above behaviour and implement an immutable container model, which has several advantages over the above model. In this case, the previous execution tree should be functionally decomposed into one or more repos.

Modifying any of the components of these repos generates a new version of the same and the corresponding regeneration of the container(s) that use it.

With this strategy we achieve

* Simplify the application development and testing process.
* Enable incremental introduction of changes to the system, minimizing risks
* Enable the portability of processes to different Cloud platforms (on-prem, on-cloud).

Once a business function has been isolated in a container with a standard interface, it can be modified or rewritten in any other programming language and deployed transparently without affecting the rest of the system.   
