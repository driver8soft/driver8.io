      ******************************************************************
      *
      * Loan Calculator Main Program
      * ==========================
      *
      * A sample program to demonstrate how to create a gRPC COBOL
      * microservice.
      *
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. loanmain.

       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.
       
       DATA DIVISION.

       FILE SECTION.

       WORKING-STORAGE SECTION.
      * In COBOL, you declare variables in the WORKING-STORAGE section
       01 PROG-NAME PIC X(8) VALUE "loancalc".
       01 COMMLEN PIC 9(9) COMP.      
       01 COMMAREA.
           05 INPUT-MSG.
               10 PRIN-AMT      PIC S9(7)      USAGE IS DISPLAY.
               10 INT-RATE      PIC S9(2)V9(2) USAGE IS DISPLAY.
               10 TIMEYR        PIC S9(2)      USAGE IS DISPLAY.
           05 OUTPUT-MSG.
               10 PAYMENT       PIC S9(7)V9(2) USAGE IS DISPLAY.
               10 ERROR-MSG     PIC X(20).

       PROCEDURE DIVISION.
      * code goes here!
           INITIALIZE COMMAREA.

           DISPLAY "Compound Interest Calculator"
           DISPLAY "Principal amount: " WITH NO ADVANCING.
           ACCEPT PRIN-AMT.
           DISPLAY "Interest rate: " WITH NO ADVANCING.
           ACCEPT INT-RATE.
           DISPLAY "Number of years: " WITH NO ADVANCING.
           ACCEPT TIMEYR.   

           COMPUTE COMMLEN = LENGTH OF COMMAREA.
           CALL "D8link" USING PROG-NAME COMMAREA COMMLEN.

           DISPLAY "Error Msg: " ERROR-MSG.
           DISPLAY "Couta: " PAYMENT.

           GOBACK.


