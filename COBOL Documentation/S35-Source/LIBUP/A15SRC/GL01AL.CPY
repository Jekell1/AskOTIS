      * COPYMEMBER: LIBGL/GL01AL
      ***************************************************
      *  GENERAL LEDGER ALLOCATION FILE
      *  KEY 1 - ALLOCATION I.D.
      ***************************************************
       01  AL-REC.
           03  AL1-KEY.
               05  AL-ID      PIC X(12).
           03  AL-DESC        PIC X(30).
           03  AL-NO-ACCTS    PIC 99.
           03  FILLER         OCCURS 10 TIMES.
               05  AL-ACCT    PIC 9(11)      COMP-3.
               05  AL-PCT     PIC S9(3)V99   COMP-3.
           03  AL-TYPE        PIC X.
           03  FILLER         PIC X(29).
