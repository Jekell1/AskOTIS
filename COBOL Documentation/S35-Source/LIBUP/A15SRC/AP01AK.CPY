      * COPYMEMBER: LIBAP/AP01AK
      **************************************************
      *    ACCOUNTS PAYABLE TRANSACTION RELEASE HEADER *
      *    KEY1 = LOC, VENDOR, CHECK SEQ               *
      *    KEY2 = CHECK NO                             *
      **************************************************
       01  AK-REC.
           03  AK-KEY-AREA.
               05  AK-LOC             PIC 9(4).
               05  AK-CLASS           PIC 9(2).
      * GLB    EXPAND FROM 6 - 9   11/98
               05  AK-VENDOR          PIC 9(9).
               05  AK-INVESTOR REDEFINES AK-VENDOR PIC 9(9).
               05  AK-CHECK-SEQ       PIC 9(3).
               05  AK-CHECKNO         PIC 9(6).
           03  AK1-KEY-AREA REDEFINES AK-KEY-AREA.
               05  AK1-KEY            PIC X(18).
               05  FILLER             PIC X(6).
           03 AK2-KEY-AREA REDEFINES AK-KEY-AREA.
               05  FILLER             PIC X(18).
               05  AK2-KEY            PIC X(6).
      * GLB    EXPAND DATE FROM 6 - 8   11/98
           03  AK-TRANS-CHECKDATE     PIC 9(8).
           03  AK-CHECKTYPE           PIC X.
      * GLB    EXPAND DATE FROM 6 - 8   11/98
           03  AK-CHECKDATE           PIC 9(8)     COMP-3.
           03  AK-TOTAL-AP            PIC S9(7)V99 COMP-3.
           03  AK-TOTAL-PAY           PIC S9(7)V99 COMP-3.
           03  AK-TOTAL-DISC          PIC S9(5)V99 COMP-3.
           03  AK-VENDOR-NAME         PIC X(25).
           03  AK-INVESTOR-NAME REDEFINES AK-VENDOR-NAME
                                      PIC X(25).
           03  AK-ADR1                PIC X(25).
           03  AK-ADR2                PIC X(25).
           03  AK-CITY                PIC X(15).
           03  AK-STATE               PIC X(4).
           03  AK-ZIP                 PIC X(10).
           03  AK-PRINT-FLAG          PIC X.
           03  AK-VOID                PIC XX.
           03  AK-REMIT-TO-FLAG       PIC X.
      * GLB    NEW FIELD "NP"           11/98
           03  AK-SOURCE-CODE         PIC XX.
      * GLB    NEW FIELD CLASS SEQ, ALPHA FROM NOTE MASTER 12/21/98
           03  AK-NC-SEQNO            PIC 99.
           03  AK-ALPHA               PIC X(30) OCCURS 3.
           03  AK-ACCTNO              PIC 9(6).
           03  AK-RATE                PIC 99V99.
           03  AK-BATCH-ID            PIC 9(5).
           03  AK-BLOC                PIC 9(4).
           03  FILLER                 PIC X(11).
