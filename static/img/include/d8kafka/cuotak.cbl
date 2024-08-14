      ******************************************************************
      *
      * Loan kafka producer
      * ==========================
      *
      *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. cuotak.
       ENVIRONMENT DIVISION.
 
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
           01 WS-LOAN.
               05 WS-AMT  PIC 9(7)V9(2).
               05 WS-INT  PIC 9(2)V9(2).
               05 WS-YEAR PIC 9(2).
      ******************************************************************  
           01 KAFKA.
               05 KAFKA-TOPIC PIC X(05) VALUE "loans".
               05 FILLER     PIC X(1)  VALUE LOW-VALUES.

              05 KAFKA-KEY.
                 10 KAFKA-KEY1 PIC X(15) VALUE "PrincipalAmount".
                 10 FILLER     PIC X(1)  VALUE ",".
                 10 KAFKA-KEY2 PIC X(12) VALUE "InterestRate".
                 10 FILLER     PIC X(1)  VALUE ",".
                 10 KAFKA-KEY1 PIC X(09) VALUE "TimeYears".
                 10 FILLER     PIC X(1)  VALUE LOW-VALUES.
              05 KAFKA-VALUE.
                 10 KAFKA-AMT-VALUE  PIC zzzzzz9.99.
                 10 FILLER     PIC X(1)  VALUE ",".
                 10 KAFKA-INT-VALUE  PIC z9.99.
                 10 FILLER     PIC X(1)  VALUE ",".
                 10 KAFKA-YEAR-VALUE PIC zz.
                 10 FILLER     PIC X(1) VALUE LOW-VALUES.

       PROCEDURE DIVISION.
           INITIALIZE WS-LOAN.

           DISPLAY "Amount: " WITH NO ADVANCING.
           ACCEPT WS-AMT.
           DISPLAY "Interest: " WITH NO ADVANCING.
           ACCEPT WS-INT.
           DISPLAY "Number of Years: " WITH NO ADVANCING.
           ACCEPT WS-YEAR.
           
           MOVE WS-AMT TO KAFKA-AMT-VALUE.
           MOVE WS-INT TO KAFKA-INT-VALUE.
           MOVE WS-YEAR TO KAFKA-YEAR-VALUE.

           CALL "D8kafka" USING KAFKA-TOPIC 
                                KAFKA-KEY
                                KAFKA-VALUE.

           DISPLAY "Return-code: " RETURN-CODE.
           
           GOBACK.
           


