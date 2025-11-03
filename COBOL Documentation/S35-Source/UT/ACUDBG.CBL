       IDENTIFICATION DIVISION.
       PROGRAM-ID.    ACUMLT.
       AUTHOR.        JKC.
       DATE-WRITTEN.  06-14-2022.
      ******************************************************************
      *
      * IDENTIFICATION: UT/ACUDBG.CBL
      *
      * DESCRIPTION   : ACU DEBUG TEST PROGRAM
      *
      *=================================================================
      * USED FOR CREATING A NEW ROUTINE THAT TAKES MULSIL VARIABLES
      * AND PUSHES THEM INTO A /USR/TMP FILE.
      *
      *=================================================================
      * REV:
      * JKC 2022-0614 ***NEW***
      *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/ACUDBG S35 08/02/23-08:41:48 >".

      *=================================================================
       01 DEC-VARS.
          05 DEC-FILLER.
             10 FILLER     USAGE DOUBLE VALUE 0.01.
             10 FILLER     USAGE DOUBLE VALUE 0.01.
             10 FILLER     USAGE DOUBLE VALUE 0.00.
             10 FILLER     USAGE DOUBLE VALUE 1.00.
             10 FILLER     USAGE DOUBLE VALUE 0.01.
             10 FILLER     USAGE DOUBLE VALUE 0.01.
             10 FILLER     USAGE DOUBLE VALUE 1.00.
             10 FILLER     USAGE DOUBLE VALUE 0.0001.
             10 FILLER     USAGE DOUBLE VALUE 0.01.
          05 DEC-TABLE     REDEFINES DEC-FILLER.
             10 DEC        USAGE DOUBLE OCCURS 9 TIMES.
       01 WS-VARS.
          05 I-T           USAGE DOUBLE.
          05 I-C           USAGE DOUBLE.
          05 I-B           USAGE DOUBLE.
          05 I-P           USAGE DOUBLE.
          05 I-REGP        USAGE DOUBLE.
          05 I-NOR         USAGE DOUBLE.
          05 I-R           USAGE DOUBLE OCCURS 5 TIMES.
          05 I-BRK         USAGE DOUBLE OCCURS 5 TIMES.
          05 I-RATEIDX     PIC 9(2).
          05 I-RTIMESBRK   USAGE DOUBLE OCCURS 5 TIMES.
          05 P1            USAGE DOUBLE.
          05 P2            USAGE DOUBLE.
          05 B1            USAGE DOUBLE.
          05 B2            USAGE DOUBLE.
          05 AREG          USAGE DOUBLE.
          05 BREG          USAGE DOUBLE.
          05 TREG          PIC S9(9).

       01  WS-A                        PIC S9(10)V99 VALUE ZEROES.
       01  WS-B                        PIC S9(10)V99 VALUE ZEROES.
       01  WS-C                        PIC S9(10)V99 VALUE ZEROES.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBUT/DBMULSILW.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       PROCEDURE DIVISION.
       MAIN-PROGRAM SECTION.

      D    STOP MESS.
           COMPUTE WS-A = WS-B / WS-C.
           DISPLAY "THIS PROGRAM TESTS A DEBUG VARIABLES ROUTINE.".
           DISPLAY "THIS WILL SEND RESULTS TO OUTPUT FILE.".

           MOVE "T" TO DBMULSIL-FIRST-TIME.
           PERFORM DBMULSIL-ROUTINE.

           DISPLAY "PROGRAM COMPLETE. PRESS ENTER TO EXIT." AT LINE 10.
           ACCEPT OMITTED.

      *=================================================================
       MAIN-PROGRAM-EXIT.
           STOP RUN.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBUT/DBMULSIL.CPY".
