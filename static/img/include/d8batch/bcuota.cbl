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
           


