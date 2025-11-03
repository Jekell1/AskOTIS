      * COPYMEMBER: LIBLP/LPBWINW
      *-------------------------------------------------------------------------
      *    COMPUTE BORROWER INTEREST PAID WORKERS
      *
      *REV:
      * JTG 890131 CHANGED MPRATE FROM 999V999 999V9999999999
      * JTG 010425 ADDED SUBSCRIPT FIELD (BWIN & BWIN-CNT)   COLONIAL TN PR#J017
      * JTG 040107 FIXED BUG IN LIBLP/LPBWIN FOR RULE 78THS, INTERNAL WORKER
      *            WAS ONLY 9(5)V99 AND WHEN RULE OF 78THS COMPUTATION WAS DONE
      *            ON LARGE FINANCE CHARGES AND LONG TERM, INTEREST
      *            PAID WAS TRUNCATED. INCREASE FIELDS TO 9(8)     COLTN  J042
      *-------------------------------------------------------------------------
       01  BWIN-WORKERS.
           03  BWIN-INTPAID         PIC S9(8)V99.
           03  BWIN-APCUR           PIC S9(8)V99.
           03  BWIN-INTCHG          PIC S9(8)V99.
           03  BWIN-PAYMENTS        PIC 9(8)V9(8).
           03  FILLER              REDEFINES BWIN-PAYMENTS.
               05  BWIN-WPAYS       PIC 9(8).
               05  BWIN-PPAYS       PIC V9(8).
           03  BWIN-WORKER          PIC S9(8)V9(8).
           03  BWIN-INTPAID-SAV     PIC S9(8)V99.
           03  BWIN-INTPAID-WORKER  PIC S9(8)V99.
           03  BWIN-EXPINT          PIC S9(8)V99.
           03  BWIN-CNT             PIC S9(3).
           03  BWIN                 PIC S9(3).
