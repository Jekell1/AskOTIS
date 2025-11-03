      * COPYMEMBER: LIBLP/LPAMTSW
      *******************************************************************
      *    LOAN CALCULATION WORKERS      (LPAMTSW)
      *REV:
      * JTG 051089 CHANGED WORKERS TO COMP
      * JTG 020896 CHANGED LPAMTS-INSUR TO NONMEMO-INS
      *                    LPAMTS-FEES  TO NONMEMO-FEES
      * JTG 073197 ADDED TOTAL-INSFINCHG, TOTAL-FEEFINCHG, WORLD A90
      * BAH 022398 ADDED INSUR-OURS , FEES-OURS, FFG DISC FRMLA PR# 653
      * JTG 990727 ADDED CREDIT-LIMIT FOR LIBWI/LNAPPROVAL RE: REVOLVING
      * JTG 020219 ADDED "CT" COUNTY TAX CODE KENTUCKY AND PERCENTS
      *            ADDED "CY" CITY   TAX CODE KENTUCKY AND PERCENTS
      *            ADDED TOTAL-INS-TAX                                WORLD #288
      *  JTG 020620 CHANGED FOR (MIP) MONTHLY INSURANCE PREMIUMS   REGENCY #1801
      *            ADDED TOTAL-INSPREM-NONMEMO FOR LPAPRS MUST HAVE NON MEMOS
      *  MJD 160823 ADDED GET-LIVECHECK-FEES TO CALCULATE THE AMOUNT OF FEES
      *             FROM LIVE CHECKS THAT NEED TO BE INCLUDED IN
      *             HIGH-CREDIT/LN-CREDITLIM                          61000/1180
      **************************************************************************
       01  AMT-WORKERS.
           03  TOTAL-REPAY      PIC S9(7)V99    COMP.
           03  TOTAL-NOTE       PIC S9(7)V99    COMP.
           03  PRINCIPLE        PIC S9(7)V99    COMP.
           03  ADJ-PRINCIPAL    PIC S9(7)V99    COMP.
           03  CASH-ADVANCE     PIC S9(7)V99    COMP.
           03  SPCASH-ADVANCE   PIC S9(7)V99    COMP.
           03  FINANCED-AMOUNT  PIC S9(7)V99    COMP.
           03  DISCLOSED-FINAMT PIC S9(7)V99    COMP.
           03  HIGH-CREDIT      PIC 9(6)        COMP.
           03  CREDIT-LIMIT     PIC 9(6)        COMP.
           03  INT-CHARGEABLE   PIC S9(7)V99    COMP.
           03  NONMEMO-INS      PIC S9(7)V99    COMP.
           03  NONMEMO-FEES     PIC S9(7)V99    COMP.
           03  LPAMTS-SUB       PIC S9(5)       COMP.
           03  TOTAL-INSPREM-NONMEMO PIC S9(7)V99 COMP.
           03  TOTAL-INSFINCHG  PIC S9(7)V99    COMP.
           03  TOTAL-INS-TAX    PIC S9(7)V99    COMP.
           03  TOTAL-FEEFINCHG  PIC S9(7)V99    COMP.
           03  INSUR-OURS       PIC S9(7)V99    COMP.
           03  FEES-OURS        PIC S9(7)V99    COMP.
           03  LIVE-CHECK-FEES  PIC S9(7)V99    COMP.

       01  LPAMTSW-FEECODE      PIC XX.
       01  LPAMTSW-FEECODEX REDEFINES LPAMTSW-FEECODE.
           03  LPAMTSW-FEECODE-CH1  PIC X.
               88  LPAMTSW-FEECODE-CH1-NUMERIC
                              VALUE '1' '2' '3' '4' '5'
                                    '6' '7' '8' '9' '0'.
           03  FILLER               PIC X.
