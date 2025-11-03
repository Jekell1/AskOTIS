      ******************************************************************
      *
      * IDENTIFICATION: LIBUT/DBCALCZLW
      *
      * DESCRIPTION   : DEBUG CALCZL VARIABLES
      *                 USED TO GET CALCZL VARIABLES AND LOAD THEM
      *                 INTO A OUTPUT FILE.
      *
      * OUTPUT FILE   : /USR/TMP/CALCZL-YYMMDD-HHMMSS
      *
      *=================================================================
      * REV:
      * JKC 2022-0712 ***NEW***
      *
      ******************************************************************
       01  DBCALCZL-DATE-NOW.
           03  DBCALCZL-DATE-CC        PIC 9(2).
           03  DBCALCZL-DATE-YY        PIC 9(2).
           03  DBCALCZL-DATE-MM        PIC 9(2).
           03  DBCALCZL-DATE-DD        PIC 9(2).
       01  FILLER            REDEFINES DBCALCZL-DATE-NOW.
           03  FILLER                  PIC 9(2).
           03  DBCALCZL-DATE-YYMMDD    PIC 9(6).

       01  DBCALCZL-DETAIL-LINE.
           03  DBCALCZL-DETAIL-PROMPT  PIC X(20).
           03  DBCALCZL-DETAIL-TEXT    PIC X(60).
           03  DBCALZL-DETAIL-TEXT-5 REDEFINES DBCALCZL-DETAIL-TEXT
                                       PIC X(5).
           03  DBCALCZL-DETAIL-DOUBLE  REDEFINES DBCALCZL-DETAIL-TEXT
                                       PIC -9.9(14)E+99.
           03  DBCALCZL-DETAIL-FLOAT   REDEFINES DBCALCZL-DETAIL-TEXT
                                       PIC 9.9(15)-.
           03  DBCALCZL-DETAIL-NUM     REDEFINES DBCALCZL-DETAIL-TEXT
                                       PIC 9(8)-.
           03  DBCALCZL-DETAIL-NUM2    REDEFINES DBCALCZL-DETAIL-TEXT
                                       PIC 9(2).
           03  DBCALCZL-DETAIL-NUM2V12 REDEFINES DBCALCZL-DETAIL-TEXT
                                       PIC 9(2).9(12).
           03  DBCALCZL-DETAIL-NUM7V2  REDEFINES DBCALCZL-DETAIL-TEXT
                                       PIC 9(7).9(2).
           03  DBCALCZL-DETAIL-RATE    REDEFINES DBCALCZL-DETAIL-TEXT
                                       PIC 9(4).9(4).
           03  DBCALCZL-DETAIL-SIGN5   REDEFINES DBCALCZL-DETAIL-TEXT
                                       PIC 9(5)-.
           03  DBCALCZL-DETAIL-SIGN9   REDEFINES DBCALCZL-DETAIL-TEXT
                                       PIC 9(9)-.
           03  DBCALCZL-DETAIL-SIGN9-WHOLE REDEFINES 
                                     DBCALCZL-DETAIL-TEXT PIC 9(9).
           03  DBCALCZL-DETAIL-SIGN7V99 REDFINES 
                                DBCALCZL-DETAIL-TEXT PIC 9(7).99-.
           03  DBCALCZL-DETAIL-NUM7V99 REDEFINES 
                             DBCALCZL-DETAIL-TEXT PIC 9(7).99.

           03  DBCALCZL-DETAIL-NUM3 REDEFINES DBCALCZL-DETAIL-TEXT
                                               PIC 9(3).
           03  DBCALCZL-DETAIL-NUM1 REDEFINES DBCALCZL-DETAIL-TEXT
                                               PIC 9.
           03  DBCALCZL-DETAIL-NUM4 REDEFINES DBCALCZL-DETAIL-TEXT
                                               PIC 9(4).



       01  DBCALCZL-ECHO-CMD.
           03  FILLER                  PIC X(05)     VALUE "ECHO ".
           03  FILLER                  PIC X(01)     VALUE X"22".
           03  DBCALCZL-ECHO-TEXT      PIC X(80)     VALUE SPACES.
           03  FILLER                  PIC X(01)     VALUE X"22".

       01  DBCALCZL-FIRST-TIME         PIC X(01)     VALUE "Y".

       01  DBCALCZL-OUTPUT-FILE.
           03  FILLER                  PIC X(09)     VALUE "/USR/TMP/".
           03  FILLER                  PIC X(07)     VALUE "CALCZL-".
           03  DBCALCZL-OUTPUT-DATE    PIC 9(06)     VALUE ZEROES.
           03  FILLER                  PIC X(01)     VALUE "-".
           03  DBCALCZL-OUTPUT-TIME    PIC 9(06)     VALUE ZEROES.

       01  DBCALCZL-SUB                PIC 9(03)     VALUE ZEROES.

       01  DBCALCZL-SYSTEM.
           03  DBCALCZL-SYSTEM-CMD     PIC X(120)    VALUE SPACES.
           03  FILLER                  PIC X(04)     VALUE " >> ".
           03  DBCALCZL-SYSTEM-OUTPUT  PIC X(35)     VALUE SPACES.

       01  DBCALCZL-TIME-NOW.
           03  DBCALCZL-TIME-HH        PIC 9(2).
           03  DBCALCZL-TIME-MM        PIC 9(2).
           03  DBCALCZL-TIME-SS        PIC 9(2).
           03  DBCALCZL-TIME-HD        PIC 9(2).
       01  FILLER            REDEFINES DBCALCZL-TIME-NOW.
           03  DBCALCZL-TIME-HHMMSS    PIC 9(6).
           03  FILLER                  PIC 9(2).

