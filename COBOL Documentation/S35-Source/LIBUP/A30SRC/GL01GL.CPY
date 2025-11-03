      * COPYMEMBER: LIBGL/GL01GL
      **************************************************
      *          GLFILE - GENERAL LEDGER MASTER  
      *
      * REV:
      *  MJD 990126 ----- UPDATED IN FLEXGEN -----
      *  BLM 090120 ADDED GL-SPANISH-DESC, WORLD PR# 616
      *  BAH 151204 ADDED FILLER, ADDED GL-LTOUCH-DATE, PD0005
      **************************************************
       78  GL-SIZE                 VALUE 600.
       01  GL-REC.
           03  GL-FLAG             PIC X.

           03  GL1-KEY.
               05  GL-BRANCH       PIC 9(4).
               05  GL-ACCOUNT      PIC 9(7).
               05  GL-ACCT-BREAKDOWN REDEFINES GL-ACCOUNT.
                   07  GL-DEPT     PIC 9(3).
                   07  GL-ACCT     PIC 9(4).
           03  GL-NUMKEY REDEFINES GL1-KEY PIC 9(11).
      *>>XDD DATE=YYYYMMDD
           03  GL-LTOUCH-DATE      PIC 9(8)      COMP-3.

           03  GL2-KEY.
               05  GL-DESCRIPTION  PIC X(36).
           03  GL-CONSOL           PIC 9(4).
           03  GL-OPENBAL          PIC S9(9)V99 COMP-3.

           03  GL-AMOUNTS.
               05  GL-CYBAL        PIC S9(9)V99 COMP-3 OCCURS 12.
               05  GL-LYBAL        PIC S9(9)V99 COMP-3 OCCURS 12.
               05  GL-CYBUD        PIC S9(9)    COMP-3 OCCURS 12.
               05  GL-LYBUD        PIC S9(9)    COMP-3 OCCURS 12.
               05  GL-NYBUD        PIC S9(9)    COMP-3 OCCURS 12.
           03  GL-TABLE REDEFINES GL-AMOUNTS.
               05  GL-TAB1     OCCURS 2.
                   07  GL-BAL      PIC S9(9)V99 COMP-3 OCCURS 12.
               05  GL-TAB2     OCCURS 3.
                   07  GL-BUD      PIC S9(9)    COMP-3 OCCURS 12.

           03  GL-TYPE             PIC X.
           03  GL-CLEAR            PIC X.
           03  GL-NUMBER1          PIC 9(7).
           03  GL-NUMBER2          PIC 9(7).
           03  GL-FS-FLAG          PIC X.
           03  GL-REPORTS          PIC X OCCURS 5.
           03  GL-LYOPENBAL        PIC S9(9)V99 COMP-3.
           03  GL-DLALLOC-GL       PIC X.
           03  GL-SPANISH-DESC     PIC X(36).
           03  FILLER              PIC X(148).
