      ******************************************************************
      * Author: Gerardo González Aguila
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Kalman.
      *AUTHOR Gerardo González Aguilar
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT Data1 ASSIGN TO "Data1.DAT"
        ORGANIZATION IS LINE SEQUENTIAL.
       SELECT Data2 ASSIGN TO "Data2.DAT"
        ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
        FD Data1.
           01 pair-value1       PIC X(12).
        FD Data2.
           01 pair-value2       PIC X(12).
       WORKING-STORAGE SECTION.
       01  800-WIN-USERNAME                PIC X(24)   VALUE "UNKNOWN".
       01  800-WIN-USERPROFILE             PIC X(24)   VALUE "UNKNOWN".
       01  800-WIN-USERDOMAIN              PIC X(24)   VALUE "UNKNOWN".
       01  800-CURRENT-DATE.
           05  800-CURRENT-DATE-YYYY       PIC X(04)   VALUE SPACES.
           05  800-CURRENT-DATE-MM         PIC X(02)   VALUE SPACES.
           05  800-CURRENT-DATE-DD         PIC X(02)   VALUE SPACES.
           05  800-CURRENT-TIME-HH         PIC X(02)   VALUE SPACES.
           05  800-CURRENT-TIME-MM         PIC X(02)   VALUE SPACES.
           05  800-CURRENT-TIME-SS         PIC X(02)   VALUE SPACES.
           05  FILLER                      PIC X(07)   VALUE SPACES.
       01  Raw-value PIC 9(2)V9(3)  VALUE ZEROS.
       01  Random-base PIC 9(2)V9(3)  VALUE ZEROS.
       01  kalman-value PIC 9(2)V9(3)  VALUE ZEROS.
       01  first1 PIC 9 VALUE ZERO.
       01  Rnd-seed-t PIC 9(2)  VALUE ZEROS.
       01  car-count PIC 9(2).
       01  T PIC 9(2).99.


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           MOVE FUNCTION CURRENT-DATE  TO 800-CURRENT-DATE
           DISPLAY "Kallman simulation by Gerardo Gonzalez"
           Display '     Executed at ' 800-current-date-yyyy '/'
             800-current-date-mm   '/' 800-current-date-dd   space
             800-current-time-hh   ':' 800-current-time-mm   ':'
             800-current-time-ss
           ACCEPT 800-WIN-USERNAME FROM ENVIRONMENT "USERNAME"
           ACCEPT 800-WIN-USERDOMAIN FROM ENVIRONMENT "USERDOMAIN"
           ACCEPT 800-WIN-USERPROFILE FROM ENVIRONMENT "USERPROFILE"

           DISPLAY "USERNAME    = " 800-WIN-USERNAME
           DISPLAY "USERPROFILE = " 800-WIN-USERPROFILE
           DISPLAY "USERDOMAIN  = " 800-WIN-USERDOMAIN
           call "SYSTEM" using z"systeminfo >> systeminfo.dat"

           accept Rnd-seed-t from time
           DISPLAY "Random Seed from timer : " Rnd-seed-t
           OPEN OUTPUT Data1
           COMPUTE Random-base = FUNCTION NUMVAL(800-current-time-ss)
      *    DISPLAY "Random-Base = " Random-base
           COMPUTE Raw-value = FUNCTION RANDOM () * Random-base
      *    DISPLAY "Random-Value = " Raw-value
           COMPUTE kalman-value = kalman-value *0.7 + Raw-value * 0.3

           PERFORM 1000 TIMES

              COMPUTE Raw-value = FUNCTION RANDOM () * Random-base
      *    DISPLAY "Random-Value = " Raw-value
              COMPUTE kalman-value = kalman-value *0.7 + Raw-value * 0.3
              MOVE Raw-value  TO T
              MOVE T to pair-value1
             COMPUTE car-count = LENGTH OF T
      *       MOVE 7 to car-count
              ADD 1 to car-count
              MOVE "," TO pair-value1(car-count:)

              ADD 1 to car-count
              MOVE kalman-value TO T
              MOVE T to pair-value1(car-count:)

              DISPLAY  "Generated and Calculated Values " pair-value1
              WRITE pair-value1
           END-PERFORM

          CLOSE Data1
           MOVE FUNCTION CURRENT-DATE  TO 800-CURRENT-DATE
           DISPLAY 'Kalman Finisheded at '
             800-current-date-yyyy '/'
             800-current-date-mm   '/'
             800-current-date-dd   space
             800-current-time-hh   ':'
             800-current-time-mm   ':'
             800-current-time-ss

       STOP RUN.
       END PROGRAM Kalman.

      ***COMPUTE RANDOM-VALUE = FUNCTION RANDOM (1) * 100 + 1
*     ***      procedure division.
      ***   call "system" using z"dir c:\|more"  ("CALL "SYSTEM" USING command-line")
      ***   goback.
