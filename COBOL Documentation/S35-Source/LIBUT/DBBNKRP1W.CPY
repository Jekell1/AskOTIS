      ******************************************************************
      *
      * IDENTIFICATION: LIBUT/DBBNKRP1W
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
       01  DBBNKRP1-DATE-NOW.
           03  DBBNKRP1-DATE-CC        PIC 9(2).
           03  DBBNKRP1-DATE-YY        PIC 9(2).
           03  DBBNKRP1-DATE-MM        PIC 9(2).
           03  DBBNKRP1-DATE-DD        PIC 9(2).
       01  FILLER            REDEFINES DBBNKRP1-DATE-NOW.
           03  FILLER                  PIC 9(2).
           03  DBBNKRP1-DATE-YYMMDD    PIC 9(6).

       01  DBBNKRP1-DETAIL-LINE.
           03  DBBNKRP1-DETAIL-PROMPT  PIC X(20).
           03  DBBNKRP1-DETAIL-TEXT    PIC X(60).
           03  DBBNKRP1-DETAIL-DOUBLE  REDEFINES DBBNKRP1-DETAIL-TEXT
      *                                PIC 9.9(15)-.
                                       PIC -9.9(14)E+99.
           03  DBBNKRP1-DETAIL-FLOAT   REDEFINES DBBNKRP1-DETAIL-TEXT
                                       PIC 9.9(15)-.
           03  DBBNKRP1-DETAIL-NUM2    REDEFINES DBBNKRP1-DETAIL-TEXT
                                       PIC 9(2).
           03  DBBNKRP1-DETAIL-NUM2V12 REDEFINES DBBNKRP1-DETAIL-TEXT
                                       PIC 9(2).9(12).
           03  DBBNKRP1-DETAIL-SIGN9   REDEFINES DBBNKRP1-DETAIL-TEXT
                                       PIC 9(9)-.
           03  DBBNKRP1-DETAIL-SIGN9-WHOLE REDEFINES 
                                     DBBNKRP1-DETAIL-TEXT PIC 9(9).


       01  DBBNKRP1-ECHO-CMD.
           03  FILLER                  PIC X(05)     VALUE "ECHO ".
           03  FILLER                  PIC X(01)     VALUE X"22".
           03  DBBNKRP1-ECHO-TEXT      PIC X(80)     VALUE SPACES.
           03  FILLER                  PIC X(01)     VALUE X"22".

       01  DBBNKRP1-FIRST-TIME         PIC X(01)     VALUE "Y".

       01  DBBNKRP1-OUTPUT-FILE.
           03  FILLER                  PIC X(09)     VALUE "/USR/TMP/".
           03  FILLER                  PIC X(07)     VALUE "MULSIL-".
           03  DBBNKRP1-OUTPUT-DATE    PIC 9(06)     VALUE ZEROES.
           03  FILLER                  PIC X(01)     VALUE "-".
           03  DBBNKRP1-OUTPUT-TIME    PIC 9(06)     VALUE ZEROES.

       01  DBBNKRP1-SUB                PIC 9(03)     VALUE ZEROES.

       01  DBBNKRP1-SYSTEM.
           03  DBBNKRP1-SYSTEM-CMD     PIC X(120)    VALUE SPACES.
           03  FILLER                  PIC X(04)     VALUE " >> ".
           03  DBBNKRP1-SYSTEM-OUTPUT  PIC X(35)     VALUE SPACES.

       01  DBBNKRP1-TIME-NOW.
           03  DBBNKRP1-TIME-HH        PIC 9(2).
           03  DBBNKRP1-TIME-MM        PIC 9(2).
           03  DBBNKRP1-TIME-SS        PIC 9(2).
           03  DBBNKRP1-TIME-HD        PIC 9(2).
       01  FILLER            REDEFINES DBBNKRP1-TIME-NOW.
           03  DBBNKRP1-TIME-HHMMSS    PIC 9(6).
           03  FILLER                  PIC 9(2).

