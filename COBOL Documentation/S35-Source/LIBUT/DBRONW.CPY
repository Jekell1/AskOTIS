      ******************************************************************
      *
      * IDENTIFICATION: LIBUT/DBRONW
      *
      * DESCRIPTION   : DEBUG FOR RON
      *
      * OUTPUT FILE   : /USR/TMP/DBRON-YYMMDD-HHMMSS
      *
      *=================================================================
      * REV:
      * JKC 2024-0104 ***NEW***
      *
      ******************************************************************
       01  DBRON-DATE-NOW.
           03  DBRON-DATE-CC           PIC 9(2).
           03  DBRON-DATE-YY           PIC 9(2).
           03  DBRON-DATE-MM           PIC 9(2).
           03  DBRON-DATE-DD           PIC 9(2).
       01  FILLER            REDEFINES DBRON-DATE-NOW.
           03  FILLER                  PIC 9(2).
           03  DBRON-DATE-YYMMDD       PIC 9(6).

       01  DBRON-DETAIL-LINE.
           03  DBRON-DETAIL-PROMPT     PIC X(20).
           03  DBRON-DETAIL-TEXT       PIC X(60).
           03  DBRON-DETAIL-DATE       REDEFINES DBRON-DETAIL-TEXT
                                       PIC 9999/99/99.
           03  DBRON-DETAIL-NUM9       REDEFINES DBRON-DETAIL-TEXT
                                       PIC 9(9).
           03  DBRON-DETAIL-NUM9V2     REDEFINES DBRON-DETAIL-TEXT
                                       PIC 9(9).9(2).
           03  DBRON-DETAIL-SIGN9      REDEFINES DBRON-DETAIL-TEXT
                                       PIC 9(9)-.
           03  DBRON-DETAIL-SIGN9V2    REDEFINES DBRON-DETAIL-TEXT
                                       PIC 9(9).9(2)-.
           03  DBRON-DETAIL-SSNO       REDEFINES DBRON-DETAIL-TEXT
                                       PIC 999/99/9999.

       01  DBRON-ECHO-CMD.
           03  FILLER                  PIC X(05)     VALUE "ECHO ".
           03  FILLER                  PIC X(01)     VALUE X"22".
           03  DBRON-ECHO-TEXT         PIC X(80)     VALUE SPACES.
           03  FILLER                  PIC X(01)     VALUE X"22".

       01  DBRON-FIRST-TIME            PIC X(01)     VALUE "Y".

       01  DBRON-OUTPUT-FILE.
           03  FILLER                  PIC X(09)     VALUE "/USR/TMP/".
           03  FILLER                  PIC X(07)     VALUE "DBRON-".
           03  DBRON-OUTPUT-DATE       PIC 9(06)     VALUE ZEROES.
           03  FILLER                  PIC X(01)     VALUE "-".
           03  DBRON-OUTPUT-TIME       PIC 9(06)     VALUE ZEROES.

       01  DBRON-SUB                   PIC 9(03)     VALUE ZEROES.

       01  DBRON-SYSTEM.
           03  DBRON-SYSTEM-CMD        PIC X(120)    VALUE SPACES.
           03  FILLER                  PIC X(04)     VALUE " >> ".
           03  DBRON-SYSTEM-OUTPUT     PIC X(35)     VALUE SPACES.

       01  DBRON-TIME-NOW.
           03  DBRON-TIME-HH           PIC 9(2).
           03  DBRON-TIME-MM           PIC 9(2).
           03  DBRON-TIME-SS           PIC 9(2).
           03  DBRON-TIME-HD           PIC 9(2).
       01  FILLER                      REDEFINES DBRON-TIME-NOW.
           03  DBRON-TIME-HHMMSS       PIC 9(6).
           03  FILLER                  PIC 9(2).

