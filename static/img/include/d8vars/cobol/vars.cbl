      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. vars.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
      * Declare variables in the WORKING-STORAGE section
       LINKAGE SECTION.
      * Data to share with COBOL subroutines 
       01 MY-RECORD.
               10 CHAR     PIC X(09) VALUE SPACES.
               10 COMP2    COMP-2 VALUE ZEROES.
               10 COMP1    COMP-1 VALUE ZEROES.
               10 COMP5D   PIC S9(18) COMP-5 VALUE ZEROES.
               10 COMP5L   PIC S9(9)  COMP-5 VALUE ZEROES.
               10 COMP5S   PIC S9(4)  COMP-5 VALUE ZEROES.
               10 COMP5UD  PIC  9(18) COMP-5 VALUE ZEROES.
               10 COMP5UL  PIC  9(9)  COMP-5 VALUE ZEROES.
               10 COMP5US  PIC  9(4)  COMP-5 VALUE ZEROES.
               10 NDISPLAY PIC S9(3)V9(2) VALUE ZEROES.
               10 COMP3    PIC S9(3)V9(2) COMP-3 VALUE ZEROES.
               10 COMP4D   PIC S9(8)V9(2) COMP   VALUE ZEROES.
               10 COMP4L   PIC S9(3)V9(2) COMP   VALUE ZEROES.
               10 COMP4S   PIC S9(2)V9(2) COMP   VALUE ZEROES.

       PROCEDURE DIVISION USING BY REFERENCE MY-RECORD. 
      * code goes here!
           
           DISPLAY "char: "  CHAR. 
           DISPLAY "comp-2: " COMP2. 
           DISPLAY "comp-1: " COMP1. 
           DISPLAY "comp5 double: "  COMP5D.
           DISPLAY "comp5 long: "    COMP5L. 
           DISPLAY "comp5 short: "   COMP5S. 
           DISPLAY "comp5 Udouble: "  COMP5UD.
           DISPLAY "comp5 Ulong: "    COMP5UL. 
           DISPLAY "comp5 Ushort: "   COMP5US. 
           DISPLAY "display: " NDISPLAY.
           DISPLAY "comp-3: "  COMP3.
           DISPLAY "comp double: " COMP4D.
           DISPLAY "comp long: " COMP4L. 
           DISPLAY "comp short: " COMP4S. 

           MOVE 0 TO RETURN-CODE.
           GOBACK.


