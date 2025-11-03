      ******************************************************************
      *
      * IDENTIFICATION: LIBUT/DBSTEVEW
      *
      * DESCRIPTION   : DEBUG FOR STEVE
      *
      * OUTPUT FILE   : /USR/TMP/DBSTEVE-YYMMDD-HHMMSS
      *
      *=================================================================
      * REV:
      * JKC 2024-0104 ***NEW***
      *
      ******************************************************************
       01  DBSTEVE-DATE-NOW.
           03  DBSTEVE-DATE-CC        PIC 9(2).
           03  DBSTEVE-DATE-YY        PIC 9(2).
           03  DBSTEVE-DATE-MM        PIC 9(2).
           03  DBSTEVE-DATE-DD        PIC 9(2).
       01  FILLER            REDEFINES DBSTEVE-DATE-NOW.
           03  FILLER                  PIC 9(2).
           03  DBSTEVE-DATE-YYMMDD    PIC 9(6).

       01  DBSTEVE-DETAIL-LINE.
           03  DBSTEVE-DETAIL-PROMPT  PIC X(20).
           03  DBSTEVE-DETAIL-TEXT    PIC X(60).
           03  DBSTEVE-DETAIL-DOUBLE  REDEFINES DBSTEVE-DETAIL-TEXT
      *                                PIC 9.9(15)-.
                                       PIC -9.9(14)E+99.
           03  DBSTEVE-DETAIL-FLOAT   REDEFINES DBSTEVE-DETAIL-TEXT
                                       PIC 9.9(15)-.
           03  DBSTEVE-DETAIL-NUM2    REDEFINES DBSTEVE-DETAIL-TEXT
                                       PIC 9(2).
           03  DBSTEVE-DETAIL-NUM2V12 REDEFINES DBSTEVE-DETAIL-TEXT
                                       PIC 9(2).9(12).
           03  DBSTEVE-DETAIL-SIGN9   REDEFINES DBSTEVE-DETAIL-TEXT
                                       PIC 9(9)-.
           03  DBSTEVE-DETAIL-SIGN9-WHOLE REDEFINES 
                                     DBSTEVE-DETAIL-TEXT PIC 9(9).


       01  DBSTEVE-ECHO-CMD.
           03  FILLER                  PIC X(05)     VALUE "ECHO ".
           03  FILLER                  PIC X(01)     VALUE X"22".
           03  DBSTEVE-ECHO-TEXT      PIC X(80)     VALUE SPACES.
           03  FILLER                  PIC X(01)     VALUE X"22".

       01  DBSTEVE-FIRST-TIME         PIC X(01)     VALUE "Y".

       01  DBSTEVE-OUTPUT-FILE.
           03  FILLER                  PIC X(09)     VALUE "/USR/TMP/".
           03  FILLER                  PIC X(08)     VALUE "DBSTEVE-".
           03  DBSTEVE-OUTPUT-DATE    PIC 9(06)     VALUE ZEROES.
           03  FILLER                  PIC X(01)     VALUE "-".
           03  DBSTEVE-OUTPUT-TIME    PIC 9(06)     VALUE ZEROES.

       01  DBSTEVE-SUB                PIC 9(03)     VALUE ZEROES.

       01  DBSTEVE-SYSTEM.
           03  DBSTEVE-SYSTEM-CMD     PIC X(120)    VALUE SPACES.
           03  FILLER                  PIC X(04)     VALUE " >> ".
           03  DBSTEVE-SYSTEM-OUTPUT  PIC X(35)     VALUE SPACES.

       01  DBSTEVE-TIME-NOW.
           03  DBSTEVE-TIME-HH        PIC 9(2).
           03  DBSTEVE-TIME-MM        PIC 9(2).
           03  DBSTEVE-TIME-SS        PIC 9(2).
           03  DBSTEVE-TIME-HD        PIC 9(2).
       01  FILLER            REDEFINES DBSTEVE-TIME-NOW.
           03  DBSTEVE-TIME-HHMMSS    PIC 9(6).
           03  FILLER                  PIC 9(2).

