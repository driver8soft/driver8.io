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

```jcl

//JOB1    JOB (123),CLASS=C,MSGCLASS=S,MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*
//STEP01   EXEC PGM=BCUOTA
//INFILE   DD   DSN=DEV.APPL1.TEST,DISP=SHR
//OUTFILE  DD   DSN=DEV.APPL1.CUOTA,
//              DISP=(NEW,CATLG,DELETE),VOLUME=SER=SHARED,
//              SPACE=(CYL,(1,1),RLSE),UNIT=SYSDA,
//              DCB=(RECFM=FB,LRECL=80,BLKSIZE=800)
//*

```

Create a _step.yaml_ file and copy and paste the following code into it.

```yaml
---
stepname: "step1"
exec:
  pgm: "bcuota"
dd:
  - name: "infile"
    dsn: "test.txt"
    disp: "shr"
    normaldisp: "catlg"
    abnormaldisp: "catlg"
  - name: "outfile"
    dsn: "cuota.txt"
    disp: "new"
    normaldisp: "catlg"
    abnormaldisp: "delete"

```

Next, using this configuration yaml, we will run a batch file read/write program.
The main program bcuota.cbl reads an input file, calls the COBOL routine loancalc.cbl to calculate the loan quota, and writes the result to the output file.

```cobol
      ******************************************************************
      *
      * Loan Calculator Batch
      * ==========================
      *
      *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. bcuota.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LOAN ASSIGN TO "infile"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS IS SEQUENTIAL.

           SELECT CUOTA ASSIGN TO "outfile"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS IS SEQUENTIAL.    

       DATA DIVISION.
       FILE SECTION.
       FD LOAN.
           01 LOAN-FILE PIC X(26).

       FD CUOTA.
           01 CUOTA-FILE.
               05 CUOTA-ACC  PIC X(10).
               05 CUOTA-PAY  PIC 9(7)V9(2).

       WORKING-STORAGE SECTION.
           01 WS-LOAN.
               05 WS-ACC  PIC X(10).
               05 FILLER  PIC X(1).
               05 WS-AMT  PIC 9(7).
               05 FILLER  PIC X(1).
               05 WS-INT  PIC 9(2)V9(2).
               05 FILLER  PIC X(1).
               05 WS-YEAR PIC 9(2).
           01 WS-EOF PIC X(1) VALUE "N".
           01 WS-COUNTER PIC 9(9) VALUE ZEROES.
      ****************************************************************     
           01 LOAN-PARAMS.
               05 INPUT-MSG.
                   10 PRIN-AMT      PIC S9(7)      USAGE IS DISPLAY.
                   10 INT-RATE      PIC S9(2)V9(2) USAGE IS DISPLAY.
                   10 TIMEYR        PIC S9(2)      USAGE IS DISPLAY.
               05 OUTPUT-MSG.
                   10 PAYMENT       PIC S9(7)V9(2) USAGE IS DISPLAY.
                   10 ERROR-MSG     PIC X(20).

       PROCEDURE DIVISION.

           OPEN INPUT LOAN.
           OPEN OUTPUT CUOTA.
           PERFORM UNTIL WS-EOF='Y'
               READ LOAN INTO WS-LOAN
               AT END MOVE 'Y' TO WS-EOF
               NOT AT END    
                   MOVE WS-AMT TO PRIN-AMT
                   MOVE WS-INT TO INT-RATE
                   MOVE WS-YEAR TO TIMEYR
                   CALL "loancalc" USING LOAN-PARAMS
                   ADD 1 TO WS-COUNTER
                   MOVE WS-ACC TO CUOTA-ACC
                   MOVE PAYMENT TO CUOTA-PAY
                   WRITE CUOTA-FILE
                   END-WRITE
               END-READ
           END-PERFORM.
           CLOSE LOAN.
           CLOSE CUOTA.
           DISPLAY "TOTAL RECORDS PROCESSED: " WS-COUNTER.
           GOBACK.
```

```cobol
      ******************************************************************
      *
      * Loan Calculator Subroutine
      * ==========================
      *
      * A sample program to demonstrate how to create a gRPC COBOL
      * microservice.
      *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. loancalc.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
      * In COBOL, you declare variables in the WORKING-STORAGE section
       01  WS-MSG.
           05 WS-ERROR      PIC X(01) VALUE 'N'.
           05 WS-MSG00      PIC X(20) VALUE 'OK'.
           05 WS-MSG10      PIC X(20) VALUE 'INVALID INT. RATE'.
           05 WS-MSG12      PIC X(20) VALUE 'INVALID NUMBER YEARS'.
       01  AUX-VARS.
           05 MONTHLY-RATE  USAGE IS COMP-1.
           05 AUX-X         USAGE IS COMP-1.
           05 AUX-Y         USAGE IS COMP-1.
           05 AUX-Z         USAGE IS COMP-1.

       LINKAGE SECTION.
      * Data to share with COBOL subroutines 
       01 LOAN-PARAMS.
           05 INPUT-MSG.
               10 PRIN-AMT      PIC S9(7)      USAGE IS DISPLAY.
               10 INT-RATE      PIC S9(2)V9(2) USAGE IS DISPLAY.
               10 TIMEYR        PIC S9(2)      USAGE IS DISPLAY.
           05 OUTPUT-MSG.
               10 PAYMENT       PIC S9(7)V9(2) USAGE IS DISPLAY.
               10 ERROR-MSG     PIC X(20).

       PROCEDURE DIVISION USING BY REFERENCE LOAN-PARAMS. 
      * code goes here!
       
       000-MAIN.   
           DISPLAY "PRIN-AMT: " PRIN-AMT.  
           DISPLAY "INT-RATE: " INT-RATE.
           DISPLAY "TIMEYR: "   TIMEYR.    
           PERFORM 100-INIT.
           IF WS-ERROR = 'N'
               PERFORM 200-PROCESS
           END-IF.
           PERFORM 300-WRAPUP.    
       
       100-INIT.  
           IF INT-RATE <= 0
               MOVE WS-MSG10 TO ERROR-MSG
               MOVE 10 TO RETURN-CODE
               MOVE 'Y' TO WS-ERROR
           ELSE
               IF TIMEYR <= 0
                   MOVE WS-MSG12 TO ERROR-MSG
                   MOVE 12 TO RETURN-CODE
                    MOVE 'Y' TO WS-ERROR
               END-IF
           END-IF.                  
       200-PROCESS.
           INITIALIZE AUX-VARS.
           COMPUTE MONTHLY-RATE ROUNDED = (INT-RATE / 12 / 100).
           COMPUTE AUX-X ROUNDED = ((1 + MONTHLY-RATE) ** (TIMEYR*12)).
           COMPUTE AUX-Y ROUNDED = AUX-X * MONTHLY-RATE.
           COMPUTE AUX-Z ROUNDED = (AUX-X - 1) / AUX-Y.
           COMPUTE PAYMENT ROUNDED = PRIN-AMT / AUX-Z.
           MOVE WS-MSG00 TO ERROR-MSG.
           MOVE 0 TO RETURN-CODE.

           DISPLAY "PAYMENT: "   PAYMENT.
           DISPLAY "ERROR-MSG: " ERROR-MSG.

       300-WRAPUP.
           GOBACK.

```
> Compile both programs to create a shared library (*.so, *dylib).
>
>*cobc -m bcouta.cbl loancalc.cbl*.

The d8parti controller will replace the JES mainframe subsystem, here is a simplified version of this module, create a _d8parti.go_ file and copy the following code.

```go
package main

/*
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <libcob.h>
#cgo CFLAGS: -I/opt/homebrew/Cellar/gnucobol/3.2/include
#cgo LDFLAGS: -L/opt/homebrew/Cellar/gnucobol/3.2/lib -lcob
*/
import "C"
import (
	"errors"
	"flag"
	"fmt"
	"io"
	"log"
	"os"
	"time"
	"unsafe"

	"gopkg.in/yaml.v3"
)

type Step struct {
	Stepname string `yaml:"stepname"`
	Exec     Exec
	Dd       []Dd
}

type Exec struct {
	Pgm string `yaml:"pgm"`
}

type Dd struct {
	Name         string `yaml:"name"`
	Dsn          string `yaml:"dsn"`
	Disp         string `yaml:"disp"`
	Normaldisp   string `yaml:"normaldisp"`
	Abnormaldisp string `yaml:"abnormaldisp"`
}

var step Step

var (
	stepname = flag.String("stepname", "step.yaml", "Name of yaml step exec")
)

func cobInit() {
	C.cob_init(C.int(0), nil)
	log.Println("INFO: gnucobol initialized")
}

func cobCall(p string) (ret int, err error) {

	c_progName := C.CString(p)
	defer C.free(unsafe.Pointer(c_progName))

	n := C.cob_resolve(c_progName)
	if n == nil {
		return 12, errors.New("ERROR: Module not found")
	} else {
		log.Printf("INFO: PGM=%s started", p)
		r := C.cob_call(c_progName, C.int(0), nil)
		log.Printf("INFO: %s return-code %v", p, ret)
		return int(r), nil

	}

}
func cobStop(ret int) {
	C.cob_stop_run(C.int(ret))
}

func config(s string) error {
	configFile, err := os.Open(s)
	if err != nil {
		return err
	}
	log.Println("INFO: Successfully opened config yaml file")
	defer configFile.Close()

	data, _ := io.ReadAll(configFile)

	err = yaml.Unmarshal(data, &step)
	if err != nil {
		return err
	}

	return nil
}

func main() {
	start := time.Now()
	flag.Parse()
	cobInit()

	//Read yaml config file
	err := config(*stepname)
	if err != nil {
		log.Printf("ERROR: Reading config file %s. %s", *stepname, err)
		os.Exit(12)
	}

	//Create Symlink
	for i := 0; i < len(step.Dd); i++ {
		err := os.Symlink(step.Dd[i].Dsn, step.Dd[i].Name)
		if err != nil {
			switch {
			case os.IsExist(err):
				/*DDNAME already exist */
				log.Printf("ERROR: DDNAME=%s already exists. %s", step.Dd[i].Name, err)
			case !os.IsExist(err):
				/*DDNAME invalid */
				log.Printf("ERROR: DDNAME=%s invalid ddname. %s", step.Dd[i].Name, err)
			default:
				log.Println(err)
			}
			os.Exit(04)
		}
	}
	//Call COBOL program -> EXEC PGM defined in JCL
	ret, err := cobCall(step.Exec.Pgm)
	if err != nil {
		fmt.Println(err)
	}

	//Delete Symlink
	for i := 0; i < len(step.Dd); i++ {
		err := os.Remove(step.Dd[i].Name)
		if err != nil {
			log.Printf("INFO: DDNAME=%s does not exists. %s", step.Dd[i].Name, err)

		}
	}

	elapsed := time.Since(start)
	log.Printf("INFO: %s elapsed time %s", step.Exec.Pgm, elapsed)

	cobStop(ret)

}
```
To run the COBOL batch test program, simply open a console and run the following:

```
go run d8parti.go

```

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

```go
package main

import (
	"flag"
	"fmt"
	"math/rand"
	"os"
	"strconv"
	"time"
)

var r1 *rand.Rand

var (
	rows = flag.Int("rows", 1000, "number of rows to generate")
)

var (
	file = flag.String("file", "test.txt", "input file name")
)

func main() {
	flag.Parse()
	s1 := rand.NewSource(time.Now().UnixNano())
	r1 = rand.New(s1)

	f, err := os.Create(*file)
	if err != nil {
		fmt.Println(err)
		return
	}
	for i := 0; i != *rows; i++ {

		output := account(i) + "-" + amount() + "-" + interest() + "-" + yearsPending() + "\n"
		_, err := f.WriteString(output)
		if err != nil {
			fmt.Println(err)
			f.Close()
			return
		}
	}
	err = f.Close()
	if err != nil {
		fmt.Println(err)
		return
	}
}
func account(id int) string {

	return "id:" + fmt.Sprintf("%07d", id+1)

}

func amount() string {
	min := 1000
	max := 1000000
	a := strconv.Itoa(r1.Intn(max-min+1) + min)
	for i := len(a); i != 7; i++ {
		a = "0" + a
	}
	return a

}

func interest() string {

	return "0450"
}

func yearsPending() string {
	min := 5
	max := 25
	y := strconv.Itoa(r1.Intn(max-min+1) + min)
	if len(y) < 2 {
		y = "0" + y
	}
	return y

}

```
