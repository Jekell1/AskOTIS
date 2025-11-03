      ******************************************************************
      *
      * IDENTIFICATION: LIBUT/DBJAYW
      *
      * DESCRIPTION   : DEBUG FOR JAY
      *
      * OUTPUT FILE   : /USR/TMP/DBJAY-YYMMDD-HHMMSS
      *
      *=================================================================
      * REV:
      * JKC 2024-0104 ***NEW***
      *
      ******************************************************************
       01  DBJAY-DATE-NOW.
           03  DBJAY-DATE-CC           PIC 9(2).
           03  DBJAY-DATE-YY           PIC 9(2).
           03  DBJAY-DATE-MM           PIC 9(2).
           03  DBJAY-DATE-DD           PIC 9(2).
       01  FILLER            REDEFINES DBJAY-DATE-NOW.
           03  FILLER                  PIC 9(2).
           03  DBJAY-DATE-YYMMDD       PIC 9(6).

       01  DBJAY-DETAIL-LINE.
           03  DBJAY-DETAIL-PROMPT     PIC X(20).
           03  DBJAY-DETAIL-TEXT       PIC X(60).
           03  DBJAY-DETAIL-DATE       REDEFINES DBJAY-DETAIL-TEXT
                                       PIC 9999/99/99.
           03  DBJAY-DETAIL-NUM9       REDEFINES DBJAY-DETAIL-TEXT
                                       PIC 9(9).
           03  DBJAY-DETAIL-NUM9V2     REDEFINES DBJAY-DETAIL-TEXT
                                       PIC 9(9).9(2).
           03  DBJAY-DETAIL-SIGN9      REDEFINES DBJAY-DETAIL-TEXT
                                       PIC 9(9)-.
           03  DBJAY-DETAIL-SIGN9V2    REDEFINES DBJAY-DETAIL-TEXT
                                       PIC 9(9).9(2)-.
           03  DBJAY-DETAIL-SSNO       REDEFINES DBJAY-DETAIL-TEXT
                                       PIC 999/99/9999.

       01  DBJAY-ECHO-CMD.
           03  FILLER                  PIC X(05)     VALUE "ECHO ".
           03  FILLER                  PIC X(01)     VALUE X"22".
           03  DBJAY-ECHO-TEXT         PIC X(80)     VALUE SPACES.
           03  FILLER                  PIC X(01)     VALUE X"22".

       01  DBJAY-FIRST-TIME            PIC X(01)     VALUE "Y".

       01  DBJAY-OUTPUT-FILE.
           03  FILLER                  PIC X(09)     VALUE "/USR/TMP/".
           03  FILLER                  PIC X(07)     VALUE "DBJAY-".
           03  DBJAY-OUTPUT-DATE       PIC 9(06)     VALUE ZEROES.
           03  FILLER                  PIC X(01)     VALUE "-".
           03  DBJAY-OUTPUT-TIME       PIC 9(06)     VALUE ZEROES.

       01  DBJAY-SUB                   PIC 9(03)     VALUE ZEROES.

       01  DBJAY-SYSTEM.
           03  DBJAY-SYSTEM-CMD        PIC X(120)    VALUE SPACES.
           03  FILLER                  PIC X(04)     VALUE " >> ".
           03  DBJAY-SYSTEM-OUTPUT     PIC X(35)     VALUE SPACES.

       01  DBJAY-TIME-NOW.
           03  DBJAY-TIME-HH           PIC 9(2).
           03  DBJAY-TIME-MM           PIC 9(2).
           03  DBJAY-TIME-SS           PIC 9(2).
           03  DBJAY-TIME-HD           PIC 9(2).
       01  FILLER                      REDEFINES DBJAY-TIME-NOW.
           03  DBJAY-TIME-HHMMSS       PIC 9(6).
           03  FILLER                  PIC 9(2).

