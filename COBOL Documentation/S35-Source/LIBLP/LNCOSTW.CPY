      * COPYMEMBER: LIBLP/LNCOSTW
      **************************************************************************
      *    LOAN COST WORKERS
      *
      * REV:
      *  JTG 010808 ADDED SP-LNCOST-FRMLA-VALID 'P' AMORTIZED
      *             PARTICIPATION EXPENSE                         REGACC PR#1559
      *  JTG 030620 ADDED LOGIC FOR IB SALES FINANCE AMORTIZATED PARTICIPATION
      *             RESERVE PAID EXPENSE, DIFFERENCE BETWEEN A BUY RATE
      *             AND THE CONTRACT RATE                WISCONSIN FINANCE #2049
      *             ADDED SP-LNCOST-FRMLA-VALID '3'
      **************************************************************************
       01  LNCOST-WORKERS.
      * INPUT FIELDS:
           03  LNCOST-LNAMT            PIC S9(6)V99.
           03  LNCOST-DLPARVHELD       PIC S9(6)V99.
           03  LNCOST-DLPARVPAID       PIC S9(6)V99.
      * OUTPUT FIELDS:
           03  LNCOST-LOANCOST         PIC S9(6)V99.
