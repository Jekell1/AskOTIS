      * COPYMEMBER: LIBAP/AP01AK
      **************************************************
      *    ACCOUNTS PAYABLE TRANSACTION RELEASE HEADER *
      *    KEY1 = LOC, VENDOR, CHECK SEQ               *
      *    KEY2 = CHECK NO                             *
      **************************************************
       78  AK-SIZE                    VALUE 330.
       01  AK-REC.
           03  AK1-KEY.
               05  AK-LOC             PIC 9(4).
               05  AK-CLASS           PIC 9(2).
               05  AK-VENDOR          PIC 9(9).
               05  AK-INVESTOR REDEFINES AK-VENDOR PIC 9(9).
               05  AK-CHECK-SEQ       PIC 9(3).
           03  AK2-KEY.
               05  AK-CKNO         PIC 9(6).

      *>>XDD DATE=YYYYMMDD
           03  AK-LTOUCH-DATE         PIC 9(8)     COMP-3.
      *>>XDD DATE=YYYYMMDD
           03  AK-TRANS-CHECKDATE     PIC 9(8)     COMP-3.
           03  AK-CHECKTYPE           PIC X.
      *>>XDD DATE=YYYYMMDD
           03  AK-CHECKDATE           PIC 9(8)     COMP-3.
           03  AK-TOTAL-AP            PIC S9(7)V99 COMP-3.
           03  AK-TOTAL-PAY           PIC S9(7)V99 COMP-3.
           03  AK-TOTAL-DISC          PIC S9(7)V99 COMP-3.
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
      * "NP" 
           03  AK-SOURCE-CODE         PIC XX.
      *  CLASS SEQ, ALPHA FROM NOTE MASTER 
           03  AK-NC-SEQNO            PIC 99.
           03  AK-ALPHA               PIC X(30) OCCURS 3.
           03  AK-ACCTNO              PIC 9(6).
           03  AK-RATE                PIC 99V99.
           03  AK-BATCH-ID            PIC 9(5).
           03  AK-BLOC                PIC 9(4).
           03  FILLER                 PIC X(54).
