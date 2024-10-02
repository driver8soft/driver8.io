---
title: COBOL variables
date: 2024-06-20
description: >
  How to convert a COBOL COPYBOOK to a Go Struct?
categories: [Tutorial]
weight: 10
---

### Variables in the COBOL language.

Any COBOL program can be turned into a microservice and deployed in Kubernetes.

To do this, simply compile the program and generate an executable module that can be called dynamically or statically from another application program. You can refer to the [examples](/docs/3-Examples) to learn how to make both dynamic and static calls.

Similar to the functionality of a function in a contemporary programming language, these programs, or _subroutines_, can accept a set of variables as input parameters. The process is straightforward: the `USING` clause in the `PROCEDURE DIVISION` must be coded to replicate the number, sequence, and type of variables used in the call from the main program.

In the case of batch programs, the _main_ program (associated with a STEP tab of a JCL) does not normally use parameters (PARM) to receive input variables to the program. Instead, programs often employ the `COBOL ACCEPT` statement or read the required information directly from a file.

In the case of online programs, the _main_ program is associated with a transaction and the transaction manager (CICS or IMS) is responsible for calling this program. This program typically utilises the designated transaction manager sentences to receive a message comprising multiple variables (EXEC CICS RECEIVE - GN) within a pre-defined memory area.

Whatever the execution mode (online or batch), these _main_ COBOL programs can in turn make multiple calls to COBOL _subroutines_ using the `CALL` statement (in the case of CICS, this call could be made using the EXEC CICS LINK sentence).

Therefore, if we want to expose a COBOL program as a microservice that can be called by other microservices written in different programming languages, it is necessary to understand the different data types that a COBOL program can use and convert them to a standard type that can be used by any programming language.

### Data types in COBOL

Defining a variable in a COBOL program can be confusing to someone used to working with modern programming languages. In addition, there are several data types that are commonly used by a COBOL programmer that have no equivalent in any modern programming language.

Let's try to decipher the operation of the most commonly used COBOL data types in a standard program and define their translation to a modern language such as Go.

> Variables that require double-byte characters are excluded.

As in any other language, to define a variable we must declare its name and type (string, int, float, bytes, etc.), however, in COBOL this definition is not done directly, it is done through the `USAGE` and `PICTURE` clauses.

`01 VAR1 PIC S9(3)V9(2) COMP VALUE ZEROS.`

The `USAGE` clause defines the internal memory storage to be used by the variable.

The `PICTURE` (or `PIC`) clause defines the _mask_  associated with the variable and its general characteristics; this definition is done by using a set of specific characters or symbols:

* A, the variable contains alphabetic characters.
* X, the variable contains alphanumeric characters
* 9, numeric variable
* S, indicates a signed numeric variable 
* V, number of decimal places
* Etc.

#### USAGE DISPLAY

Variables defined as `USAGE DISPLAY` can be of the following types

##### Alphabetical

They are defined by using the symbol **A** in the `PICTURE` clause.

`01 VAR-ALPHA PIC A(20).`

They can only store characters from the Latin alphabet. Their use is not very common, as they are generally replaced by alphanumeric variables, which we will see below.

##### Alphanumeric

They are defined by using the symbol **X** in the `PICTURE` clause.

`01 VAR-CHAR PIC X(20).`

As in the previous case, it is not necessary to define the `USAGE` clause, since variables of type **A** or **X** are of type `DISPLAY` by default.

##### Numeric

They are defined by using the symbol **9** in the `PICTURE` clause.

`01 VAR1 PIC S9(3)V9(2) USAGE DISPLAY.`

In this case, we define a numeric variable of length 5 (3 integer places and 2 decimal places) with sign.

The definition of numeric variables of type `DISPLAY` can be explicit, as in the previous example, or implicit if the `USAGE` clause is not declared.

##### Internal storage

Each of the defined characters is stored in a byte (EBCDIC), in the case of signed numeric type variables, the sign is defined in the first 4 bits of the last byte.

Let's look at an example to better understand how this type of variable works.


| Number    | EBCDIC value    |
|-----------|-----------------|
| 0         | x'F0'           |
| 1         | x'F1'           |
| 2         | x'F2'           |
| 3         | x'F3'           |
| 4         | x'F4'           |
| 5         | x'F5'           |
| 6         | x'F6'           |
| 7         | x'F7'           |
| 8         | x'F8'           |
| 9         | x'F9'           |

If we assign a value of **12345** to a variable of type

 `PIC 9(5) USAGE DISPLAY`

It would take 5 bytes

12345 = x’F1F2F3F4F5’

In case of signed variables

 `PIC S9(5) USAGE DISPLAY`

+12345 = x’F1F2F3F4C5’

-12345 = x’F1F2F3F4D5’ 

#### USAGE COMP-1 or COMPUTATIONAL-1 

32-bit (4 bytes) float variable.

#### USAGE COMP-2 or COMPUTATIONAL-2 

64-bit (8 bytes) float variable.

It is not possible to define a `PICTURE` clause associated with a `COMP-1` or `COMP-2` type variable.

#### USAGE COMP-3 or COMPUTATIONAL-3

Packed-Decimal variable.

`01 VAR-PACKED     PIC S9(3)V9(2)  USAGE COMP-3.`

4 bits are used to store each of the numeric characters of the variable, the sign is stored in the last 4 bits.

Generally, such variables are defined with an odd length to fill the total number of bytes used.

A simple rule to calculate the memory used by a `COMP-3` type variable is to divide the total length of the variable (integer positions + decimal positions) by 2 and add 1, in the above example the memory required for the variable VAR-PACKED is;

5 / 2 = 2 -> Total length 5 (3 integers + 2 decimals)

2 + 1 = 3 -> storage requirement of 3 bytes

Let's look at an example to better understand how this type of variable works.

Let's assign the value **12345** to a variable of type `COMP-3`, as in the previous example.

 `PIC S9(5) USE COMP-3.`

We start with the DISPLAY format number

x'F1F2F3F4F5'.

We remove the first 4 bits of each byte and add the sign at the end.

+12345 = x’12345C’

-12345 = x’12345D’

#### USAGE COMP-4 or COMPUTATIONAL-4 or COMP or BINARY

In this case the data type is binary. Negative numbers are represented as two's complement.

The size of storage required depends on the `PICTURE` clause.

| PICTURE                  | Storage         | Value                                  | 
|--------------------------|-----------------|----------------------------------------|
| PIC S9(1) - S9(4)        | 2 bytes         | -32,768 to +32,767                     |
| PIC S9(5) - S9(9)        | 4 bytes         | -2,147,483,648 to +2,147,483,647       |
| PIC S9(10) - S9(18)      | 8 bytes         | -9,223,372,036,854,775,808 to +9,223,372,036,854,775,807 | 
| PIC 9(1) - 9(4)          | 2 bytes         | 0 to 65,535                            |
| PIC 9(5) - 9(9)          | 4 bytes         | 0 to 4,294,967,295                     |
| PIC 9(10) - 9(18)        | 8 bytes         | 0 to 18,446,744,073,709,551,615        |


Up to this point, the operation of this type of variable would be equivalent to the representation of integer variables in most programming languages (short, long or double variables and their unsigned equivalents ushort, ulong, udouble), but by using the `PICTURE` clause we can limit the maximum values of such a variable and define a decimal mask.

For example:

`01 VAR-COMP PIC 9(4)V9(2) USAGE COMP.`

It uses 4 bytes of memory (int32), but its value is limited from 0 to 9999.99.

What is the point of defining integer variables and then defining a fixed mask with the number of integer and decimal places?

To restrict the use of floating point operations. This may seem strange today, but 30 years ago it made sense to reduce the number of CPU cycles used when the cost of computing was extremely high. 

On the other hand, almost all operations performed by financial institutions with currencies do not require complex calculations, and by operating with integers we do not lose precision when we need decimals (i.e. cents).

#### USAGE COMP-5 or COMPUTATIONAL-5

This data type is also known as native binary.

It is equivalent to `COMP-4`, however the values to be stored are not limited by the mask defined in the `PICTURE` clause.

| PICTURE               | Variable          | 
|-----------------------|-------------------|
| PIC S9(4)  COMP-5     | short (int16)     | 
| PIC S9(9)  COMP-5     | long (int32)	    | 
| PIC S9(18) COMP-5     | double (int64)    |  
| PIC 9(4)   COMP-5     | ushort (uint16)   | 
| PIC 9(9)   COMP-5     | ulong (uint32)    | 
| PIC 9(18)  COMP-5     | udouble (uint64)  | 


### Converting variables to a standard type

It is important to note that it is only necessary to convert the variables exposed by the _main_ programs, once the COBOL runtime has initialised and executed the _main_ program, the calls between COBOL programs made by means of the CALL statement are under the responsibility of the runtime.

A special case is the call between COBOL programs deployed in different containers. A special mechanism has been designed for this, similar to the operation of the LINK statement of the CICS transaction manager.

How to make calls between COBOL programs deployed in different application containers can be found in the examples section [d8link](/docs/3-Examples/Link/).

In general, any COBOL variable could be exposed in different forms (int, float, string, etc.) and then converted, but to simplify and optimise the conversion process, we will use a set of simple rules that we will describe below.

#### EBCDIC

As mentioned above, the IBM mainframe uses EBCDIC internally.

In an attempt to replicate as closely as possible the behaviour of COBOL programs across platforms, some vendors allow EBCDIC to continue to be used internally for application data handling.

While this strategy may facilitate code migration in the short term, it presents huge compatibility, evolution and support problems in the medium to long term. Therefore, migrated COBOL programs will use ASCII characters internally.

> It is necessary to identify programs that use COBOL statements that handle hexadecimal characters and replace these strings.
>
> `MOVE X’F1F2F3F4F5’ TO VAR-NUM1.`

#### Big-endian vs. little-endian

The IBM mainframe platform is big-endian, so the most significant byte would be stored in the memory address with the smallest value, or in other words, the sign would be stored in the first bit from the left. 

In contrast, the x86 and arm platforms use little-endian to represent binary variables.

As in the previous case, we believe that the best strategy is to use the target architecture natively, so the programs are compiled to use little-endian.

#### Maximum size of COMP variables

In general, the maximum size of a numeric variable is 18 digits, regardless of the type used (`DISPLAY`, `COMP`, `COMP-3`, `COMP-5`).

However, on the mainframe platform, it is possible to extend this limit to 31 digits for some types of variables (e.g. `COMP-3`).

We are talking about numeric variables that are used to carry out arithmetic operations, except in the specific case of a country with hyperinflationary episodes that have persisted over a long period of time, the limit of 18 digits is enough.

#### Binary Variables

Although it is not common to use binary variables with a decimal mask (`COMP` or `COMP-4`), they can be used to operate on large numbers using binary instructions, thus avoiding the use of floating point numbers.

In the case of `COMP-5` or native binary variables, it does not make sense to use a decimal mask, so they are implemented directly into an integer variable type corresponding to their size.

#### Data types

| COBOL type                    | Go type           | 
|-------------------------------|-------------------|
| PIC X(n)                      | string            | 
| COMP-1                        | float32           | 
| COMP-2                        | float64           |  
| PIC S9(1 to 4)   COMP-5       | int16             | 
| PIC S9(5 to 9)   COMP-5       | int32             | 
| PIC S9(10 to 18) COMP-5       | int64             |
| PIC 9(1 to 4)    COMP-5       | uint16            | 
| PIC 9(5 to 9)    COMP-5       | uint32            | 
| PIC 9(10 to 18)  COMP-5       | uint64            | 

>There is no string concept in COBOL. The size of the variable `PIC X(n)` is exactly the length defined in the `PIC` clause.
>
>If the size of the string is smaller than the size defined in COBOL, it must be justified with spaces on the right.

Now that we have defined the variables that are equivalent to a certain type of variable in the Go language, we will define the behaviour of the numeric data types with decimal mask.

These are the data types commonly used by COBOL programmers. In the event that the program is to be exposed for invocation from an external platform, variables of type `DISPLAY` or decimal zoned are usually used to facilitate data conversion (ASCII-EBCDIC) between platforms and to facilitate error debugging.

In our case, to simplify data handling, all these variables are exposed as `string` type.


| COBOL type                          | Go type           | 
|-------------------------------------|-------------------|
| PIC S9(n)                           | string            | 
| PIC S9(n) COMP-3                    | string            | 
| PIC S9(n) COMP or COMP-4 or BINARY  | string            |

> The calling program must ensure that the data sent corresponds to a numerical value.
>
> The variables received must conform to the mask defined in the `PIC` clause, while respecting the number of integer and decimal places and, if necessary, justifying with leading zeros.

### The process of conversion

Let's start with a simple example, a COBOL program that receives a data structure with different types of variables.

{{< readfile file="/static/img/include/d8vars/cobol/vars.cbl" code="true" lang="cobol" >}}

Then we will analyse this structure (it can be parsed automatically) and generate two files:

A configuration file (vars.yml) with the characteristics of the COBOL variables present in the structure to be converted (name, type of variable, length, decimal places, sign).
> type = 0 -> numeric, type `DISPLAY`
>
> type = 1 -> numeric, type `COMP-1`
>
> type = 2 -> numeric, type `COMP-2`
>
> type = 3 -> numeric, type `COMP-3`
>
> type = 4 -> numeric, type `COMP-4 or BINARY or COMP`
>
> type = 5 -> numeric, type `COMP-5`  
>
> type = 9 -> alphanumeric, type `CHAR`

{{< readfile file="/static/img/include/d8vars/conf/vars.yml" code="true" lang="yaml" >}}

A file (request.go) containing the Go representation of the COBOL structure.

{{< readfile file="/static/img/include/d8vars/model/request.go" code="true" lang="go" >}}

And finally, a file (response.go). The structure used above can be copied as the parameters used by the program are input/output.

{{< readfile file="/static/img/include/d8vars/model/response.go" code="true" lang="go" >}}

Now we have everything we need to run our COBOL program.

### Running the test program

The following explains how to run the above test program.

The examples can be downloaded directly from the [GitHub](https://github.com/driver8soft/d8-examples/tree/main/d8vars) repo.

The directory structure of the example (**d8vars**) is as follows:

```
├── cmd
├── cobol
├── conf 
├── internal
│   └── cgocobol
│   └── common
│   └── service
├── model
│   └── request
│   └── response
├── test
│   go.mod
│   go.sum
|   Dockerfile
```
 
**/cobol**

Contains the compiled COBOL programs to be executed (*.dylib or *.so).

Use the following compilation options to define the required behaviour for binary type fields.

`cobc -m vars.cbl -fbinary-byteorder=native -fbinary-size=2-4-8`

**/conf**

Configuration files described above, used to describe the COBOL COPY structure. 

**/model**.

Contains the definition of the data structures in Go (request/response).

**/internal**.

In this case we find a simplified version of the code needed to convert data between languages and to execute the COBOL programs.

**/test**.

Finally, the test directory contains a utility for generating random test data according to the data types defined in the COBOL program.

To run the code, simply go to the **/cmd** directory, open a terminal and type

`go run .`

Remember to define to the COBOL language runtime the directory where the modules to be executed are located.

`export COB_LIBRARY_PATH=/my_dir/.../cobol`

The program will generate a random data structure, convert it to a format that can be used by the COBOL program, execute the program and convert the result to a format that can be used by the Go program.

You may wish to use the example COBOL program _loancalc.cbl_ for further testing purposes. To do so, simply compile the program and modify the configuration and data structure files.

Please modify the _app.env_ file to define the name of the COBOL program to be executed and the name of the COBOL copy to be converted.

`COBOL_PROGRAM="loancalc"`

`COBOL_CONFIG="loancalc.yaml"`

Copy the file loancalc.yml to the folder _conf_

{{< readfile file="/static/img/include/d8vars/conf/loancalc.yml" code="true" lang="yaml" >}} 

And replace the request.go and response.go structures with the following:

```go
package request

type Request struct {
	PrincipalAmount string
	InterestRate    string
	TimeYears       string
}
  
```

```go
package response

type Response struct {
	Payment  string
	ErrorMsg string
}
```