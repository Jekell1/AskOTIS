      ******************************************************************
      *
      * IDENTIFICATION: LIBUT/DBMULSILW
      *
      * DESCRIPTION   : DEBUG MULSIL VARIABLES
      *                 USED TO GET MULSIL VARIABLES AND LOAD THEM
      *                 INTO A OUTPUT FILE.
      *
      * OUTPUT FILE   : /USR/TMP/MULSIL-YYMMDD-HHMMSS
      *
      *=================================================================
      * REV:
      * JKC 2022-0614 ***NEW***
      *
      ******************************************************************
       01  DBMULSIL-DATE-NOW.
           03  DBMULSIL-DATE-CC        PIC 9(2).
           03  DBMULSIL-DATE-YY        PIC 9(2).
           03  DBMULSIL-DATE-MM        PIC 9(2).
           03  DBMULSIL-DATE-DD        PIC 9(2).
       01  FILLER            REDEFINES DBMULSIL-DATE-NOW.
           03  FILLER                  PIC 9(2).
           03  DBMULSIL-DATE-YYMMDD    PIC 9(6).

       01  DBMULSIL-DETAIL-LINE.
           03  DBMULSIL-DETAIL-PROMPT  PIC X(20).
           03  DBMULSIL-DETAIL-TEXT    PIC X(60).
           03  DBMULSIL-DETAIL-DOUBLE  REDEFINES DBMULSIL-DETAIL-TEXT
      *                                PIC 9.9(15)-.
                                       PIC -9.9(14)E+99.
           03  DBMULSIL-DETAIL-FLOAT   REDEFINES DBMULSIL-DETAIL-TEXT
                                       PIC 9.9(15)-.
           03  DBMULSIL-DETAIL-NUM2    REDEFINES DBMULSIL-DETAIL-TEXT
                                       PIC 9(2).
           03  DBMULSIL-DETAIL-NUM2V12 REDEFINES DBMULSIL-DETAIL-TEXT
                                       PIC 9(2).9(12).
           03  DBMULSIL-DETAIL-SIGN9   REDEFINES DBMULSIL-DETAIL-TEXT
                                       PIC 9(9)-.
           03  DBMULSIL-DETAIL-SIGN9-WHOLE REDEFINES 
                                     DBMULSIL-DETAIL-TEXT PIC 9(9).


       01  DBMULSIL-ECHO-CMD.
           03  FILLER                  PIC X(05)     VALUE "ECHO ".
           03  FILLER                  PIC X(01)     VALUE X"22".
           03  DBMULSIL-ECHO-TEXT      PIC X(80)     VALUE SPACES.
           03  FILLER                  PIC X(01)     VALUE X"22".

       01  DBMULSIL-FIRST-TIME         PIC X(01)     VALUE "Y".

       01  DBMULSIL-OUTPUT-FILE.
           03  FILLER                  PIC X(09)     VALUE "/USR/TMP/".
           03  FILLER                  PIC X(07)     VALUE "MULSIL-".
           03  DBMULSIL-OUTPUT-DATE    PIC 9(06)     VALUE ZEROES.
           03  FILLER                  PIC X(01)     VALUE "-".
           03  DBMULSIL-OUTPUT-TIME    PIC 9(06)     VALUE ZEROES.

       01  DBMULSIL-SUB                PIC 9(03)     VALUE ZEROES.

       01  DBMULSIL-SYSTEM.
           03  DBMULSIL-SYSTEM-CMD     PIC X(120)    VALUE SPACES.
           03  FILLER                  PIC X(04)     VALUE " >> ".
           03  DBMULSIL-SYSTEM-OUTPUT  PIC X(35)     VALUE SPACES.

       01  DBMULSIL-TIME-NOW.
           03  DBMULSIL-TIME-HH        PIC 9(2).
           03  DBMULSIL-TIME-MM        PIC 9(2).
           03  DBMULSIL-TIME-SS        PIC 9(2).
           03  DBMULSIL-TIME-HD        PIC 9(2).
       01  FILLER            REDEFINES DBMULSIL-TIME-NOW.
           03  DBMULSIL-TIME-HHMMSS    PIC 9(6).
           03  FILLER                  PIC 9(2).

