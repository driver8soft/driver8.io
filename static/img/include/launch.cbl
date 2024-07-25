       IDENTIFICATION DIVISION.
       PROGRAM-ID. launch.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Declare program variables 
       01 INPUT-NAME        PIC X(10).
       01 OUTPUT-PARM       PIC X(17).
       PROCEDURE DIVISION.
      * code goes here!
           DISPLAY "Your name: " WITH NO ADVANCING.
           ACCEPT INPUT-NAME.
           CALL 'hello' USING INPUT-NAME, OUTPUT-PARM.
           DISPLAY OUTPUT-PARM.
           DISPLAY "Return Code: " RETURN-CODE.
           STOP RUN. 

