      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
        DATA DIVISION.
         WORKING-STORAGE SECTION.
           01 WS-TABLE.
             05 WS-A OCCURS 2 TIMES.
               10 WS-B PIC A(10) VALUE ' TUTORIALS'.
               10 WS-C OCCURS 2 TIMES.
                 15 WS-D PIC X(6) VALUE ' POINT'.
        PROCEDURE DIVISION.
           DISPLAY "TWO-D TABLE : "WS-TABLE.
       STOP RUN.
