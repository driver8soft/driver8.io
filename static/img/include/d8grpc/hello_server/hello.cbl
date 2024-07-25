       IDENTIFICATION DIVISION.
       PROGRAM-ID. hello.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Declare program variables 
       LINKAGE SECTION.
      * Data to share with COBOL subroutines 
       01 RECORD-TYPE.
           05 INPUT-NAME        PIC X(10).
           05 OUTPUT-PARM.
               10 PARM1         PIC X(07).
               10 PARM2         PIC X(10).
       PROCEDURE DIVISION USING RECORD-TYPE.
           MOVE "Hello," TO PARM1.

           IF INPUT-NAME IS EQUAL TO (SPACES OR LOW-VALUES) 
              MOVE "World" TO PARM2
              MOVE 2 TO RETURN-CODE
           ELSE 
              MOVE INPUT-NAME  TO PARM2
              MOVE 0 TO RETURN-CODE
           END-IF.

           GOBACK.
           