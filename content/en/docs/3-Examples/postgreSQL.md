---
title: Playing with PostgreSQL
date: 2024-06-20
description: >
  A COBOL PostgreSQL example.
categories: [Examples]
tags: [test, sample, docs]
weight: 30
---

Is COBOL only valid for accessing DB2?

In this simple example, we will access a PostgreSQL database from a COBOL program. 

> Your programs can be precompiled (EXEC SQL) to access various SQL databases. 

To run this program, you need to install PostgreSQL and create the sample database (dvdrental). Instructions on how to do this can be found [here] (https://www.postgresql.org/download/). 


```cobol
      *> **************************************************************
      *> Connect and get data from Postgres
      *> Sample postgre DB "dvdrental" table "actor"
      *> **************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. pgcobol.
       AUTHOR. 
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      
      * CONNECT TO POSGRESQL 
       01 CONN-STR.
          05 FILLER      PIC X(17) VALUE "dbname=dvdrental ".
          05 FILLER      PIC X(14) VALUE "user=postgres ".
          05 FILLER      PIC X(17) VALUE "password=XXXXXXX ".
          05 FILLER      PIC X(15) VALUE "host=localhost ".
          05 FILLER      PIC X(10) VALUE "port=5432 ".
          05 FILLER      PIC X(16) VALUE "sslmode=disable ".
          05 FILLER      PIC X(01) VALUE LOW-VALUES.
       01 CONNECTION         USAGE POINTER.
       01 CONN-STATUS        USAGE BINARY-LONG.
      
      * DECLARE CURSOR
       01 SQL-QUERY.
          05 SQL-QUERY-DATA  PIC X(4096) VALUE SPACES.
          05 FILLER          PIC X(01) VALUE LOW-VALUES. 
       01 DB-CURSOR          USAGE POINTER.

      * SQL ERROR
       01 SQL-STATUS         USAGE BINARY-LONG.
       01 SQL-ERROR-PTR      USAGE POINTER.
       01 SQL-ERROR-STR      PIC X(4096) BASED.
       01 SQL-ERROR-MSG      PIC X(100) VALUE SPACES.

      * COUNTER 
       01 ROW-COUNTER        USAGE BINARY-LONG.
       01 COLUMN-COUNTER     USAGE BINARY-LONG.

      * FETCH 
       01 RESULT-PTR         USAGE POINTER.
       01 RESULT-STR         PIC X(4096) BASED.
       01 RESULT-DATA        PIC X(4096) VALUE SPACES.
       01 TABLE-ROW.
           02 actor_id       PIC 9(4) VALUE ZEROS.
           02 first_name     PIC X(45) VALUE SPACES.
           02 last_name      PIC X(45) VALUE SPACES.
           02 last_update    PIC X(22) VALUE SPACES.

      * AUX VARIABLES
       01 DB-ROW             PIC 9(7) VALUE ZEROS.
       01 DB-COLUMN          PIC 9(3) VALUE ZEROS.

      *> *********************************************************************
       PROCEDURE DIVISION.
           PERFORM CONNECT-DB.

           MOVE "SELECT actor_id, first_name, " &
                        "last_name, last_update " &
                "FROM actor;"
                TO SQL-QUERY-DATA.
           PERFORM DECLARE-CURSOR.
           
           PERFORM ROW-COUNT.
           PERFORM COLUMN-COUNT.

      *> iterate over rows
           PERFORM VARYING DB-ROW FROM 0 BY 1 
                   UNTIL DB-ROW >= ROW-COUNTER

                   PERFORM VARYING DB-COLUMN FROM 0 BY 1 
                   UNTIL DB-COLUMN >= COLUMN-COUNTER
                         PERFORM ROW-FETCH              
                   END-PERFORM   
                   DISPLAY actor_id " - " 
                           first_name " - " 
                           last_name " - "
                           last_update
           END-PERFORM.
           PERFORM DISCONNECT.
           GOBACK.
      *
       CONNECT-DB.
      *> connect, then check status.
           CALL "PQconnectdb" USING CONN-STR 
                RETURNING CONNECTION.
           CALL "PQstatus" USING BY VALUE CONNECTION 
                RETURNING CONN-STATUS.
           IF CONN-STATUS NOT EQUAL 0 THEN
                DISPLAY "Connection error! " CONN-STATUS
                STOP RUN
           END-IF.
       
       DISCONNECT.
      *> close connection
           CALL "PQfinish" USING BY VALUE CONNECTION 
                RETURNING OMITTED.
       
       DECLARE-CURSOR.
      *> query DB, will return a cursor
           CALL "PQexec" USING BY VALUE CONNECTION
                BY REFERENCE SQL-QUERY
                RETURNING DB-CURSOR END-CALL.
       
   
           CALL "PQresultStatus" USING BY VALUE DB-CURSOR 
                RETURNING SQL-STATUS.                
           CALL "PQresStatus" USING BY VALUE SQL-STATUS 
                RETURNING SQL-ERROR-PTR.
           SET ADDRESS OF SQL-ERROR-STR TO SQL-ERROR-PTR.
           STRING SQL-ERROR-STR DELIMITED BY x"00" 
                  INTO SQL-ERROR-MSG 
           END-STRING.

           IF SQL-STATUS NOT EQUAL 2 THEN  
                DISPLAY "Open Cursor error! " SQL-STATUS SQL-ERROR-MSG
                STOP RUN
           END-IF.   

           DISPLAY "sql_status: " SQL-STATUS
                   " sql_error: " SQL-ERROR-MSG. 
           
       ROW-COUNT.
      *> get how many entries there are in the table
           CALL "PQntuples" USING BY VALUE DB-CURSOR 
                RETURNING ROW-COUNTER.
           DISPLAY "number of rows: " ROW-COUNTER.

       COLUMN-COUNT.
      *> get how many fields there are in the table
           CALL "PQnfields" USING BY VALUE DB-CURSOR 
                RETURNING COLUMN-COUNTER.
           DISPLAY "number of fields: " COLUMN-COUNTER.

       ROW-FETCH.
      *> get value from table cell
           CALL "PQgetvalue" USING BY VALUE DB-CURSOR 
                BY VALUE DB-ROW BY VALUE DB-COLUMN
                RETURNING RESULT-PTR END-CALL
           SET ADDRESS OF RESULT-STR TO RESULT-PTR
           INITIALIZE RESULT-DATA.
           STRING RESULT-STR DELIMITED BY x"00" 
                  INTO RESULT-DATA END-STRING.
           EVALUATE DB-COLUMN
            WHEN 0
                  MOVE RESULT-DATA TO actor_id
            WHEN 1
                  MOVE RESULT-DATA TO first_name
            WHEN 2
                  MOVE RESULT-DATA TO last_name
            WHEN 3
                  MOVE RESULT-DATA TO last_update
           END-EVALUATE. 

```


