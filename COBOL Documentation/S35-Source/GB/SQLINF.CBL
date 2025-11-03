       IDENTIFICATION DIVISION.
       PROGRAM-ID.   SQLINF.
       AUTHOR.       JKC.
       DATE-WRITTEN. 11-03-2023.
      ******************************************************************
      *
      * IDENTIFICATION: WI/SQLINF.CBL
      *
      * DESCRIPTION   : SQL INFORMATION - DATABASE INFORMATION
      *                 F9 FROM LPMENU MAIN MENU
      *
      *=================================================================
      * REV:
      * JKC 2023-1103 ***NEW***
      * JKC 2023-1212 REVISED TO USE EXT-ACUSQL WORKERS INSTEAD OF
      *               READING FROM THE SSP; THE SSP IS ONLY NEEDED
      *               WITHIN GB/SETENV
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)GB/SQLINF.CBL S35 04/30/24-13:02:33 >".

      ******************************************************************
      *
      *   M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  DETAIL-LINE                 PIC X(66)     VALUE SPACES.
       01  D01-LINE                    REDEFINES DETAIL-LINE.
           03  D01-PROMPT              PIC X(16).
           03  D01-INFO                PIC X(50).

       01  DISPLAY-BUF                 VALUE SPACES.
           03  F010                    PIC X(66).
           03  F020                    PIC X(66).
           03  F030                    PIC X(66).
           03  F040                    PIC X(66).

       01  WS-VERSION.
           03  WS-VERSION1             PIC X(50).
           03  WS-VERSION2             PIC X(50).

       COPY "LIBGB/CONAME_EXT.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION.

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
      D    STOP DISPLAY-BUF.

           MOVE EXT-ACUSQL-VERSION         TO WS-VERSION.
 
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE SPACES                     TO DISPLAY-BUF.

           MOVE "DATABASE NAME : "         TO D01-PROMPT.
           MOVE EXT-ACUSQL-DB-NAME         TO D01-INFO.
           MOVE DETAIL-LINE                TO F010.

           MOVE "SERVER   NAME : "         TO D01-PROMPT.
           MOVE EXT-ACUSQL-SERVER-NAME     TO D01-INFO.
           MOVE DETAIL-LINE                TO F020.

           MOVE "SERVER VERSION: "         TO D01-PROMPT.
           MOVE WS-VERSION1                TO D01-INFO.
           MOVE DETAIL-LINE                TO F030.

           MOVE SPACES                     TO D01-PROMPT.
           MOVE WS-VERSION2                TO D01-INFO.
           MOVE DETAIL-LINE                TO F040.

           DISPLAY MESSAGE BOX F010, X"0A"
                               F020, X"0A"
                               F030, X"0A"
                               F040, X"0A".

      *=================================================================
       MAIN-PROGAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
      *
      *    E X I T - P R O G R A M
      *
      ******************************************************************
       END-PROGRAM SECTION.
           EXIT PROGRAM.
