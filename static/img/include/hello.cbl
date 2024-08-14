       IDENTIFICATION DIVISION.
       PROGRAM-ID. hello.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * In COBOL, you declare variables in the WORKING-STORAGE section
       LINKAGE SECTION.
      * Data to share with COBOL subroutines 
       01 INPUT-NAME            PIC X(10).
       01 OUTPUT-PARM.
               05 PARM1         PIC X(07).
               05 PARM2         PIC X(10).
       PROCEDURE DIVISION USING INPUT-NAME, OUTPUT-PARM.
           MOVE "Hello," TO PARM1.
           IF INPUT-NAME IS EQUAL TO (SPACES OR LOW-VALUES) 
              MOVE "World"  TO PARM2
              MOVE 2 TO RETURN-CODE
           ELSE 
              MOVE INPUT-NAME TO PARM2
              MOVE 0 TO RETURN-CODE
           END-IF.           
           GOBACK.
           