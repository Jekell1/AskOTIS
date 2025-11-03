      * A30 CONVERSION COMPLETED
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DWSSAP.
       DATE-WRITTEN. 6/88.
      *********************************************************
      *          PASSWORD GENERATOR
      *
      *   NAME:  DWSSAP
      *   DESC:  SUM OF DIGITS OF SYSTEM DATE CUBED
      *********************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(55) VALUE
           "@(#)CL/DWSSAP A30 06/06/13-11:38:33 09/18/14-13:46:55 >".
       01  WS-DATE.
           03  WS-D1    PIC 9.
           03  WS-D2    PIC 9.
           03  WS-D3    PIC 9.
           03  WS-D4    PIC 9.
           03  WS-D5    PIC 9.
           03  WS-D6    PIC 9.
       01  WS-SUM       PIC 99.
       01  WS-PROD      PIC 9(6).
       01  WS-ANSWER   REDEFINES WS-PROD.
           03  FILLER   PIC 99.
           03  WS-ANS2  PIC 9999.

       LINKAGE SECTION.
       01  THE-WORD     PIC 9999.

       PROCEDURE DIVISION
           USING THE-WORD.
       MAIN-PARAGRAPH.
           ACCEPT WS-DATE FROM DATE.
           COMPUTE WS-SUM = WS-D1 +
                               WS-D2 +
                                  WS-D3 +
                                     WS-D4 +
                                        WS-D5 +
                                           WS-D6.
           COMPUTE WS-PROD = WS-SUM * WS-SUM * WS-SUM.
           MOVE WS-ANS2 TO THE-WORD.

      **********************************************
      *  END PROGRAM
      ************************************************
       END-PROGRAM SECTION.
       STOP-RUN.
           EXIT PROGRAM.
