      * COPYMEMBER: LIBLP/LP01RB
      *************************************************
      *    REBATE / CANCELLATION TABLE FILE
      *
      * RB1-KEY - RB-TBL-NO, RB-TERM
      *
      * 2020.0610 BAH ADDED RB-LTOUCH-DATE
      *  BAH 20230600 ADDED FILLER
      *************************************************
       78  RB-SIZE                     VALUE 500.
       01  RB-REC.
           03  RB1-KEY.
               05  RB-TBL-NO    PIC 99.
               05  RB-TERM      PIC 9(3).
           03  RB-MON-DAY-CD    PIC X.
           03  RB-TBL           OCCURS 75 INDEXED BY RBIX.
               05  RB-AMT       PIC 9(4)  COMP-3.
               05  RB-RATE      PIC 99V99 COMP-3.
      $XFD DATE=YYYYMMDD
           03  RB-LTOUCH-DATE   PIC 9(8)  COMP-3.
           03  FILLER           PIC X(39).
