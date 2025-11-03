      * COPYMEMBER: LIBLP/LPLCHGW
      *-------------------------------------------------------------------------
      *          (LIBLP/LPLCHGW)
      *
      *   LATE-CHARGE CALCULATION WORKERS
      * REV:
      *  JTG 051089 CHANGED WORKERS TO COMP
      *  SLC 091390 ADDED LCHG-CURPAY.
      *  GLF 030399 ADDED LCHG-DAYS-AFTER-DUE
      *  JTG 990929 EXPANDED LCHG-LCPDTH-DATE TO 9(6)   REGENCY #245 BUG
      *  BAH 170314 ADJUSTED REDEFINE OF LCHG-LCPDTH-DATE FOR CENTURY
      *-------------------------------------------------------------------------
       01  LCHG-CHARGE            PIC S9(7)V99 COMP VALUE 0.
       01  LCHG-LATE              PIC S9(7)V99 COMP VALUE 0.
       01  LCHG-WORK              PIC  9(7)V99      VALUE 0.
       01  FILLER                REDEFINES LCHG-WORK.
           03  LCHG-TRUNC         PIC  9(7).
           03  LCHG-DEC           PIC  9(2).

       01  LCHG-LCPDTH-DATE       PIC  9(8).
       01  FILLER                REDEFINES LCHG-LCPDTH-DATE.
           03  LCHG-LCPDCC        PIC  9(2).
           03  LCHG-LCPDYY        PIC  9(2).
           03  LCHG-LCPDMM        PIC  9(2).
           03  LCHG-LCPDDD        PIC  9(2).
       01  LCHG-CURPAY            PIC S9(7)V99 COMP VALUE 0.
       01  LCHG-DAYS-AFTER-DUE    PIC S9(4)    COMP VALUE 0.

