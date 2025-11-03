      * COPYMEMBER: LIBLP/LPUERN
      **************************************************************************
      *         COMPUTE UP FRONT ANTICIPATED EARNINGS
      *
      *   NAME: LPUERN
      *   DESC: THIS ROUTINE COMPUTES EARNINGS TO TAKE
      *         AT TIME OF LOAN BOOKING OR PURCHASE.
      *
      *   IN  : UERN
      *             0       - ALL COMPONENTS (DEFAULT)
      *             1 TO 6  - THAT COMPONENT
      *
      *   OUT : UERN-EARNINGS(UERN = 1 TO 6)
      *         UERN-TOT-EARNINGS   UERN = 0
      *
      *   USED:
      *   COPY:
      * REV :
      *  JTG 111992 ALLOWED TEST ON SP-ERNPERCD = C AND D
      *  JTG 082898 ADDED SP-ERNPERCD-EFFSMP  VALUE "S" CITIZENS #733.
      *  JTG 010521 NEW CODE 'T' FOR SP-ERNPERCD-VALID-SC          PAACO PR#1469
      *  JTG 010522 ADDED CERN-APINT, CERN-APLC, CERN-APOTH, CERN-APOT2
      *             NEW CODE '23' FOR SP-ERNFRMLA                  PAACO PR#1469
      **************************************************************************
       COMPUTE-UERNINGS SECTION.
           MOVE 0 TO UERN-EARNINGS(1) UERN-EARNINGS(2)
                     UERN-EARNINGS(3) UERN-EARNINGS(4)
                     UERN-EARNINGS(5) UERN-EARNINGS(6)
                     UERN-TOT-EARNINGS.

           IF UERN > 6
              GO TO COMPUTE-UERNINGS-EXIT.

           IF UERN = 0
              MOVE 1 TO UERN
              PERFORM COMPUTE-UERNINGS-COMPONENT 6 TIMES
           ELSE
              PERFORM COMPUTE-UERNINGS-COMPONENT.

       COMPUTE-UERNINGS-EXIT.
           MOVE 0 TO UERN.

      ************************************************************
      *    GET A COMPONENT'S UPFRONT EARNINGS:
      ************************************************************
       COMPUTE-UERNINGS-COMPONENT SECTION.
           IF SP-ERNPERCD(UERN) = "S"
              PERFORM COMPUTE-UERNINGS-EFFSMP
           ELSE
           IF SP-ERNPERCD(UERN) = "T"
              PERFORM COMPUTE-UERNINGS-TAX
           ELSE
           IF SP-ERNFRMLA(UERN) > 4
              PERFORM COMPUTE-UERNINGS-FIXED
           ELSE
              IF SP-ERNFRMLA(UERN) = 2 OR 4
                 PERFORM COMPUTE-UERNINGS-REBATE.

           IF UERN-EARNINGS(UERN) > LN-ANTICERN(UERN)
              MOVE LN-ANTICERN(UERN) TO UERN-EARNINGS(UERN).

           ADD UERN-EARNINGS(UERN) TO UERN-TOT-EARNINGS.
           ADD 1 TO UERN.

      ************************************************************
      *    COMPUTE UPFORNT EARNINGS
      *           RE: CASH EARNINGS METHODS "2" OR "4"
      ************************************************************
       COMPUTE-UERNINGS-REBATE SECTION.
           MOVE 0 TO CERN-APCUR CERN-PAYMNTD CERN-TOTLCHG
                     CERN-APINT CERN-APLC.
           IF LN-SALEFIN NOT = "Y"
              MOVE LN-ENTDATE TO CERN-PAYDATE
           ELSE
              MOVE LN-PURDATE TO CERN-PAYDATE.
           MOVE UERN TO CERN.
           PERFORM COMPUTE-CERNINGS.
           MOVE CERN-EARNINGS(UERN) TO UERN-EARNINGS(UERN).

      *********************************************************************
      *    COMPUTE EARNINGS VIA FIXED COMPUTATION METHOD
      *             RE: EARNINGS METHODS > THAN "4"
      *********************************************************************
       COMPUTE-UERNINGS-FIXED SECTION.
           IF SP-ERNPERCD(UERN) = " "
              MOVE LN-ANTICERN(UERN) TO UERN-WORKER
           ELSE
           PERFORM LOAN-CALCULATIONS
           IF SP-ERNPERCD(UERN) = "A"
              MOVE TOTAL-REPAY TO UERN-WORKER
           ELSE
           IF SP-ERNPERCD(UERN) = "B"
              MOVE TOTAL-NOTE TO UERN-WORKER
           ELSE
           IF SP-ERNPERCD(UERN) = "C"
              MOVE LN-INTCHG TO UERN-WORKER
           ELSE
           IF SP-ERNPERCD(UERN) = "D"
              MOVE LN-EXTCHG TO UERN-WORKER
           ELSE
              MOVE 0 TO UERN-WORKER.

           COMPUTE UERN-EARNINGS(UERN) ROUNDED =
                   ((SP-ERNPERCT(UERN) / 100) * UERN-WORKER).
           ADD SP-ERNAMT(UERN) TO UERN-EARNINGS(UERN).

       COMPUTE-UERNINGS-FIXED-EXIT.
           EXIT.

      *-----------------------------------------------------------
      *                SP-ERNPERCD = "S"
      *
      *    DETERMINE SIMULATED FIRST PAYMENT EXTENTION AMOUNT
      *    PER SP-PYEXFRMLA = 2, SP-PYEXFRMLA-1ST-INCR-A
      *-----------------------------------------------------------
       COMPUTE-UERNINGS-EFFSMP SECTION.
           PERFORM LOAN-CALCULATIONS.

      * IF LOW RATE, SET ZERO
           IF LN-SMPRATE < 0.011
              MOVE 0 TO UERN-EARNINGS(UERN)
              GO TO COMPUTE-UERNINGS-EFFSMP-EXIT.

      *   GET FIRST PAYMENT EXTENSION DAY:

           MOVE SP-PYEXYEARTP TO ELAPSED-YRTYPE.
           MOVE LN-INTDATE TO NUM-DATE.
           MOVE LN-ORIG-1STPYDATE TO SYS-DATE NDTE-DATE.
           PERFORM TIMALL.

           IF SP-PYEXSPCODE-REMAIN
              IF ELAPSED-REM > 0
                 MOVE ELAPSED-REM TO UERN-WORKER2
              END-IF
           ELSE
              IF SP-PYEXSPCODE-REGZ
                 MOVE -1 TO NDTE-HOLD
                 PERFORM INCREMENT-MONTHS
                 MOVE NDTE-DATE TO SYS-DATE
                 PERFORM TIMALL
                 IF ELAPSED-DAYS > 0
                    MOVE ELAPSED-DAYS TO UERN-WORKER2
                 ELSE
                    MOVE 0 TO UERN-WORKER2.

           IF SP-INTMETHOD-EXACT
              MOVE 365 TO UERN-WORKER
           ELSE
              MOVE 360 TO UERN-WORKER.

           COMPUTE UERN-EARNINGS(UERN) ROUNDED =
                INT-CHARGEABLE * UERN-WORKER2
                    * LN-SMPRATE / (UERN-WORKER * 100).

       COMPUTE-UERNINGS-EFFSMP-EXIT.
           EXIT.

      *******************************************************************
      *                SP-ERNPERCD = "T"
      *
      *    DETERMINE SALES TAX AMOUNT FOR TEXAS AUTO SALES
      *    ON THE PORTION OF THE CASH DOWN PAYMENT
      *
      * JTG 010521 NEW CODE 'T' FOR SP-ERNPERCD-VALID-SC PAACO PR#1469
      *******************************************************************
       COMPUTE-UERNINGS-TAX SECTION.
           COMPUTE UERN-EARNINGS(UERN) ROUNDED =
                   ((SP-ERNPERCT(UERN) / 100) * LN-DOWNPY).

       COMPUTE-UERNINGS-TAX-EXIT.
           EXIT.
