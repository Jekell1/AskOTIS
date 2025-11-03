      ******************************************************************
      *
      * IDENTIFICATION: LIBUT/DBJAY1W
      *
      * DESCRIPTION   : DEBUG FOR JAY
      *                 CREATE ONE FILE FOR LOGGING ALL PROGRAMS FOR
      *                 ONE USER; WILL APPEND TO THE FILE IF RUNNING
      *                 OTIS MULTIPLE TIMES.
      *
      * OUTPUT FILE   : /USR/TMP/DBJAY-YYMMDD-USERID
      *
      *=================================================================
      * REV:
      * JKC 2024-0122 ***NEW***
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
           03  DBJAY-DETAIL-TIME       PIC 99/99/99/99.
           03  FILLER                  PIC X(1).
           03  DBJAY-DETAIL-PROGRAM    PIC X(23).
           03  FILLER                  PIC X(1).
           03  DBJAY-DETAIL-TEXT       PIC X(44).

       01  DBJAY-ECHO-CMD.
           03  FILLER                  PIC X(05)     VALUE "ECHO ".
           03  FILLER                  PIC X(01)     VALUE X"22".
           03  DBJAY-ECHO-TEXT         PIC X(80)     VALUE SPACES.
           03  FILLER                  PIC X(01)     VALUE X"22".

       01  DBJAY-OUTPUT-FILE.
           03  FILLER                  PIC X(09)     VALUE "/USR/TMP/".
           03  FILLER                  PIC X(06)     VALUE "DBJAY-".
           03  DBJAY-OUTPUT-DATE       PIC 9(06)     VALUE ZEROES.
           03  FILLER                  PIC X(01)     VALUE "-".
           03  DBJAY-OUTPUT-USERID     PIC X(10)     VALUE ZEROES.

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
           03  DBJAY-TIME-HHMMSSHD     PIC 9(8).
       01  FILLER                      REDEFINES DBJAY-TIME-NOW.
           03  DBJAY-TIME-HHMMSS       PIC 9(6).
           03  FILLER                  PIC 9(2).

