      * COPYMEMBER: LIBGL/GL01AL
      ***************************************************
      *  GENERAL LEDGER ALLOCATION FILE
      *
      *  AL1-KEY = AL-ID
      *
      * 161101 ADD AL-LTOUCH-DATE, INCREASE SIZE TO 250.
      * 190514 CS ADD FUTURE FIELDS
      * BAH 20200323 CLEANED UP FUTURE FIELDS, REMOVED GROUP LEVELS
      *  20230600 BAH ADDED FILLER
      ***************************************************
       78  AL-SIZE                  VALUE 240.
       01  AL-REC.
           03  AL1-KEY.
               05  AL-ID          PIC X(12).
      $XFD DATE=YYYYMMDD
           03  AL-LTOUCH-DATE     PIC 9(8) COMP-3.
           03  AL-DESC            PIC X(30).
           03  AL-NO-ACCTS        PIC 99.
           03  AL-TABLE       OCCURS 10.
               05  AL-ACCT        PIC 9(11)      COMP-3.
               05  AL-PCT         PIC S9(3)V99   COMP-3.
           03  AL-TYPE            PIC X.
           03  FILLER             PIC X(100).
