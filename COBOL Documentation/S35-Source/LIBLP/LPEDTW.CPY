      * COPYMEMBER: LIBLP/LPEDTW
      *******************************************************************
      *   E D I T   D A T A
      *   EDITS AS PER PATTERN TABLE
      * REV:
      *  100226 BAH ADDED EDIT TYPE 12, 13 TO MASK FIRST 3 DIGITS OF ACCOUNT
      *             NUMBER WITH "XXX", REGENCY PR# 3545
      *******************************************************************
       01  EDT-CLEAR-FG        PIC X   VALUE "N".
       01  EDT-LINE-BUFFER     VALUE SPACES.
           03  FILLER          PIC X(3).
           03  EDT-LINE.
               05  EDT-CHAR    PIC X   OCCURS 80.
       01  EDT-BUF1.
           03  EDT-1           PIC X   OCCURS 40.
       01  EDT-BUF2 REDEFINES EDT-BUF1.
           03  FILLER          PIC X(20).
           03  EDT-2           PIC 9(13).9(5)-.
       01  EDT-BUF3 REDEFINES EDT-BUF1.
           03  FILLER          PIC X(20).
           03  EDT-3           PIC Z(12)9.9(5)-.
       01  EDT-BUF4 REDEFINES EDT-BUF1.
           03  FILLER          PIC X(16).
           03  EDT-4           PIC Z,ZZZ,ZZZ,ZZZ,ZZ9.9(5)-.
       01  EDT-BUF5 REDEFINES EDT-BUF1.
           03  FILLER          PIC X(15).
           03  EDT-5           PIC $$,$$$,$$$,$$$,$$9.9(5)-.
       01  EDT-BUF6 REDEFINES EDT-BUF1.
           03  FILLER          PIC X(20).
           03  EDT-6           PIC ************9.9(5)-.
       01  EDT-BUF7 REDEFINES EDT-BUF1.
           03  EDT-7           PIC 99/99/99 BLANK ZERO.
           03  FILLER          PIC X(32).
       01  EDT-BUF8 REDEFINES EDT-BUF1.
           03  EDT-PHONE       PIC B999BB999B9999.
           03  FILLER          PIC X(26).
       01  EDT-BUF9 REDEFINES EDT-BUF1.
           03  EDT-SSNO        PIC 999/99/9999.
           03  FILLER          PIC X(29).
       01  EDT-BUF10 REDEFINES EDT-BUF1.
           03  EDT-TAXID       PIC 99/9999999.
           03  FILLER          PIC X(30).
       01  EDT-BUF12 REDEFINES EDT-BUF1.
           03  EDT-X-ACCT      PIC 99999999.
           03  FILLER          PIC X(32).
       01  EDT-BUF13 REDEFINES EDT-BUF1.
           03  EDT-6X-ACCT     PIC 999999.
           03  FILLER          PIC X(34).
       01  EDT-SUB             PIC 9(6) COMP-3.
       01  EDT-ROW             PIC 9(2).
       01  EDT-COL             PIC 9(2).
       01  EDT-EDIT            PIC 9(2).
