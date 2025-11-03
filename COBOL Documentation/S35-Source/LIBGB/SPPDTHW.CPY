      * COPYMEMBER: LIBGB/SPPDTHW

      *-----------------------------------------------------------------------
      *    SPPDTHW      PAID-THRU CALCULATION WORKERS
      * REV:
      *  JTG 040590 ADDED PDTH-ALDEL-FG
      *  JTG 080790 ADDED PDTH, PDTH-WORKER, PDTH-CNT FOR ADDON
      *  JTG 090690 ADDED PDTH-CUR-SCHD-PAY
      *  JTG 070292 ADDED PDTH-NXT-SCHD-PAY, PDTH-NXT-SCHD-DATE
      *  JTG 041593 ADDED PDTH-WK-DA & PDTH-DDYY FOR UNIT PERIODS
      *  JTG 052495 ADDED PDTH-CREDIT, RE: MERCURY SP-CONTR-CREDIT
      *  JTG 082995 ADDED PDTH-DEF-FG
      *  JTG 011196 EXPANDED PDTH-PAYMENTS TO 9(5)
      *  JTG 041596 ADDED PDTH-CUR-SCHD-DATE
      *  JTG 990124 ADDED COPY STATEMENT FOR LIBLP/RVPMTDUE/W
      *  JTG 021111 ADDED PDTH-AMT-TO-ADVANCE                     REGACC #0008
      *  MJD 090219 TESTING CHANGES FOR A NEW BANKRULE (PDTH-E-XXX FIELDS)
      *  BLM 170309 CHANGED PDTH-WK-DATE TO BE CCYYMMDD, CHANGED PDTH-DATE
      *             TO BE CCYYMM, PD0006
      *  BAH 170317 CHANGED PDTH-DATE BACK TO JUST MMYY, WE WONT EVER NEED
      *             IT TO COME OUT AS MMCCYY
      *  BAH 2024.0321 REMOVED PDTH-CREDIT, NOT USED
      *-----------------------------------------------------------------------
      * INPUT:
       01  PDTH-DATE-WORK        PIC 9(8).
       01  PDTH-DATE-WK         REDEFINES PDTH-DATE-WORK.
           03  PDTH-WK-CCYY      PIC 9(4).
           03  FILLER REDEFINES PDTH-WK-CCYY.
               05  PDTH-WK-CC    PIC 99.
               05  PDTH-WK-YY    PIC 99.
           03  PDTH-WK-MM        PIC 99.
           03  PDTH-WK-DD        PIC 99.
       01  PDTH-ALDEL-FG         PIC X       VALUE " ".
       01  PDTH-DEF-FG           PIC X       VALUE " ".

      * OUTPUT:
       01  PDTH-DATE             PIC 9(4).
       01  PDTH-DATE-RED        REDEFINES PDTH-DATE.
           03  PDTH-MM           PIC 99.
           03  PDTH-YY           PIC 9(2).
           03  PDTH-DDYY        REDEFINES PDTH-YY
                                 PIC 9(2).

       01  PDTH-DATE-FULL        PIC 9(8)     COMP.
       01  PDTH-LCPDTHRU         PIC 9V9(12)  COMP.
       01  PDTH-CUR-SCHD-DATE    PIC 9(8)     COMP.
       01  PDTH-CUR-SCHD-PAY     PIC S9(7)V99 COMP.

       01  PDTH-NXT-SCHD-DATE    PIC 9(8)     COMP.
       01  PDTH-NXT-SCHD-PAY     PIC S9(7)V99 COMP.

       01  PDTH-E-1STPYAMT     PIC S9(7)V99 COMP.
       01  PDTH-E-REGPYAMT     PIC S9(7)V99 COMP.
       01  PDTH-E-LASTPYAMT    PIC S9(7)V99 COMP.
       01  PDTH-E-OPTION       PIC X VALUE SPACE.

       01  PDTH-PAYMENTS         PIC 99999V9(12).
       01  PDTH-PY              REDEFINES PDTH-PAYMENTS.
           03  PDTH-PAYS         PIC 99999.
           03  PDTH-REMAIN       PIC V9(12).
       01  PDTH-AMT-TO-ADVANCE   PIC S9(7)V99 COMP.

      * WORKERS:
       01  PDTH-PAYMNTD          PIC S9(7)V99 COMP.
       01  PDTH-MONTHS           PIC S9(3)    COMP.
       01  PDTH-YEARS            PIC 99       COMP.
       01  PDTH-WORKER           PIC S9(7)V99 COMP.
       01  PDTH-CNT              PIC S9(3)    COMP.
       01  PDTH                  PIC S9(3)    COMP.

