      * COPYMEMBER: LIBLP/LP01RB
      *************************************************
      *    REBATE / CALCELLATION TABLE FILE
      *             (450 BYTES)
      *
      *************************************************
       01  RB-REC.
           03  RB1-KEY.
               05  RB-TBL-NO    PIC 99.
               05  RB-TERM      PIC 9(3).
           03  RB-MON-DAY-CD    PIC X.
           03  RB-TBL           OCCURS 75 INDEXED BY RBIX.
               05  RB-AMT       PIC 9(4)  COMP-3.
               05  RB-RATE      PIC 99V99 COMP-3.
