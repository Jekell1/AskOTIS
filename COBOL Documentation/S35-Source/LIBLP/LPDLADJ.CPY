      * COPYMEMBER: LIBLP/LPDLADJ
      **************************************************************************
      *         DEALER INSURANCE ADJUSTMENT
      *
      *   NAME: LPDLADJ
      *   DESC: DETERMINE INSURANCE PREMIUM ADJUSTMENTS REQUIRED
      *         FOR INSURANCE WRITTEN BY THE DEALER FOR US.
      *         CALL LOAN CALCULATION (CALCZ3.C)
      *   IN  :
      *   OUT : DLADJ-INSADJ-OURS
      * REV:
      * JTG 111296 CHANGED LN-INSTBL (LEVEL'S 9 & 10) TO FILLER
      * JTG 011397 MADE THESE SECTIONS LPDLADJ-ZERO-INSURANCE-FLDS
      *                                LPDLADJ-CLEAR-CALC-AREA
      * JTG 082097 CHANGED TO USE 'LPZ3CL' FOR CLEAR-CALC-AREA-Z3
      * JTG 020498 SET CIP-CAN-LOAN-NO, RE: LOGIC TO FORCE ONE CALENDAR MONTH
      *            SEE CALCZ3.C
      * JTG 092598 CHANGED CALL FROM LONPZ3 TO CALCZ3
      * JTG 010507 ALLOWED ENTRY OF 'TRADEIN' RE: SALE PRICE-DP-TI PAACO PR#1467
      * JTG 020220 ADDED "CT" COUNTY TAX CODE KENTUCKY AND PERCENTS
      *            ADDED "CY" CITY   TAX CODE KENTUCKY AND PERCENTS  WORLD #288
      **************************************************************************
       LPDLADJ SECTION.
           MOVE 0 TO DLADJ
                     DLADJ-INS-OURS
                     DLADJ-FEE-AMT
                     DLADJ-INSADJ-OURS.
           PERFORM LPDLADJ-COMPUTE-GL-FEEAMT.
           IF DLADJ-INS-OURS = 0
              GO TO LPDLADJ-EXIT.

           PERFORM CLEAR-CALC-AREA-Z3.
           PERFORM LPDLADJ-SETUP-CALC-PROC.
      *    PERFORM LPDLADJ-SETUP-CALC-PMT.
           PERFORM LPDLADJ-CALL-LOAN-CALCS.

           PERFORM LPDLADJ-GET-ADJ
               VARYING DLADJ FROM 1 BY 1
                   UNTIL DLADJ > 4.

       LPDLADJ-EXIT.
           EXIT.

      ******************************************
      *    GET INSURANCE ADJUSTMENTS
      ******************************************
       LPDLADJ-GET-ADJ SECTION.
           IF PD-INSOURS(DLADJ) = "Y"
              COMPUTE DLADJ-INSADJ-OURS =
                 DLADJ-INSADJ-OURS
                     + (PD-INSPREM(DLADJ) - COP-INS-PREMIUM(DLADJ)).

      **********************************************
      *    SETUP CALCULATION FOR PROCEEDS
      **********************************************
       LPDLADJ-SETUP-CALC-PROC SECTION.
           MOVE "C" TO CIP-OVR-RATE-SW
                       CIP-INS-SW(1)
                       CIP-INS-SW(2)
                       CIP-INS-SW(3)
                       CIP-INS-SW(4)
                       CIP-PROCEEDS-SW.
           MOVE "I" TO CIP-INS-SW(5)
                       CIP-INS-SW(6)
                       CIP-POINTS-SW
                       CIP-BALLOON-SW
                       CIP-FEE-CHG-SW(1)
                       CIP-FEE-CHG-SW(2)
                       CIP-SERVICE-CHG-SW
                       CIP-MAINT-FEES-SW
                       CIP-1ST-PY-EXT-SW
                       CIP-REGPYAMT-SW.
           MOVE "A" TO CIP-OVR-RATE-SW.

           MOVE PD-LOANTYPE TO CIP-LOANTYPE.
           MOVE PD-SMALLOAN TO CIP-SMALLOAN.
           MOVE PD-SALEFIN TO CIP-SALEFIN.
           MOVE PD-SERVACCT TO CIP-SERVACCT.

           MOVE PD-LOANDATE TO CIP-LOANDATE.
           MOVE PD-INTDATE TO CIP-INTDATE.
           MOVE PD-FILEDATE TO CIP-FILEDATE.
           MOVE PD-1STPYDATE TO CIP-1STPYDATE.
           MOVE PD-ORGTERM TO CIP-TERM.

           MOVE PD-PURCD TO CIP-PURPOSE-CD.
           MOVE PD-COLLCD TO CIP-COLLTRL-CD.
           MOVE PD-SRCD TO CIP-SOURCE-CD.
           SUBTRACT PD-DOWNPY PD-TRADEIN FROM PD-SELLPR
                                    GIVING CIP-PROCEEDS.
           MOVE PD-INSURED TO CIP-INS-WHO.
           MOVE PD-INSTYPES TO CIP-INS-TYPE.
           MOVE PD-INSELIGIBLE TO CIP-INS-ELIG.
           MOVE PD-POINTS TO CIP-POINTS.
           MOVE DLADJ-FEE-AMT TO CIP-GL-AMOUNT-TOTAL.

           MOVE PD-APRATE TO CIP-OVR-RATE.
           MOVE PD-EFFRATE TO CIP-EFF-RATE
                              CEP-EFF-RATE.
           MOVE PD-APRATE TO CIP-APR-RATE
                             CEP-APR-RATE.
           MOVE 1 TO DLADJ.
       LPDLADJ-SETUP-CALC-PROC-INS.
           MOVE PD-INSCOMP(DLADJ) TO CIP-INS-CO(DLADJ).
           MOVE PD-INSPREM(DLADJ) TO CIP-INS-PREMIUM(DLADJ)
                                   CEP-INS-PREMIUM(DLADJ).
           MOVE PD-INSTRM(DLADJ) TO CIP-INS-TERM(DLADJ)
                                  CEP-INS-TERM(DLADJ).
           MOVE PD-INSCOVR(DLADJ) TO CIP-INS-COVERAGE(DLADJ)
                                   CEP-INS-COVERAGE(DLADJ).
           MOVE PD-INSOURS(DLADJ) TO CIP-INS-USE-SW(DLADJ).
           IF PD-INSOURS(DLADJ) = "Y"
              MOVE " " TO CIP-INS-TERM-SW(DLADJ)
                          CIP-COVERAGE-SW(DLADJ)
                          CIP-PREMIUM-SW(DLADJ)
           ELSE
              MOVE "C" TO CIP-INS-TERM-SW(DLADJ)
                          CIP-COVERAGE-SW(DLADJ)
                          CIP-PREMIUM-SW(DLADJ).
           IF DLADJ < 6
              ADD 1 TO DLADJ
              GO TO LPDLADJ-SETUP-CALC-PROC-INS.

           IF SP-FEEFRMLA(1) NOT = " "
              MOVE PD-FEEAMT(1) TO CIP-FEE-CHG(1)
                                   CEP-FEE-CHG(1).

           IF SP-FEEFRMLA(2) NOT = " "
              MOVE PD-FEEAMT(2) TO CIP-FEE-CHG(2)
                                   CEP-FEE-CHG(2).

           MOVE PD-SERCHG TO CIP-SERVICE-CHG
                             CEP-SERVICE-CHG.
           MOVE PD-STATE-WORKER TO CIP-STATE-WORKER.
           MULTIPLY PD-MAINTFEE BY PD-ORGTERM
                          GIVING CIP-MAINT-FEES ROUNDED
                                 CEP-MAINT-FEES ROUNDED.
           MOVE PD-EXTCHG TO CIP-1ST-PY-EXT CEP-1ST-PY-EXT.
           MOVE PD-INTCHG TO CIP-INT-CHARGE.
           MOVE PD-FINAMT TO CIP-AMT-FINANCD.
           MOVE PD-LNAMT TO CIP-LOAN-AMOUNT.
           ADD PD-ANTICERN(1)
                PD-ANTICERN(2)
                 PD-ANTICERN(3)
                  PD-ANTICERN(4)
                   PD-ANTICERN(5)
                    PD-ANTICERN(6) GIVING CIP-ANTIC-EARNS.

      * SETUP 1ST PAYMENT AMOUNT:
           IF PD-1STPYAMT = PD-REGPYAMT
              MOVE 0 TO CIP-ODD-1ST-PMT.

      * SETUP LAST PAYMENT AMOUNT:
           IF PD-LASTPYAMT = PD-REGPYAMT
              MOVE "B" TO CIP-BALLOON-LAST-FG
              MOVE 0 TO CIP-BALLOON
           ELSE
              MOVE "L" TO CIP-BALLOON-LAST-FG
              MOVE PD-LASTPYAMT TO CIP-BALLOON.

      * SETUP REGULAR PAYMENT AMOUNT:
           MOVE PD-REGPYAMT TO CIP-REGPYAMT.

      ******************************************
      *    CALL LOAN CALCULATIONS
      ******************************************
       LPDLADJ-CALL-LOAN-CALCS SECTION.
           MOVE PD-UNITPER-CD TO CIP-UNITPER-CD.
           MOVE PD-UNITPER-FREQ TO CIP-UNITPER-FREQ.
           PERFORM MATURITY-DATE-CALCULATION.
           MOVE NDTE-DATE TO CIP-MATURITY-DATE.

           MOVE PD-SELLPR-FG TO CIP-SELLPR-FG.
           MOVE PD-DOWNPY-FG TO CIP-DOWNPY-FG.
           MOVE PD-PERDOWN-FG TO CIP-PERDOWN-FG.
           MOVE PD-SELLPR TO CIP-SELLPR.
           MOVE PD-DOWNPY TO CIP-DOWNPY.
           MOVE PD-PERDOWN TO CIP-PERDOWN.

           MOVE 1 TO CIP-NO-OF-TIMES-CALLED.
           MOVE "LP/CALCZ3" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH
                                CIP-CALCULATION-INPUT-AREA
                                CEP-CALCULATION-EDIT-AREA
                                COP-CALCULATION-OUTPUT-AREA
                                SP-REC
                                BR-REC BW-STATE BW-CITY-TAX-CD
                                BW-COUNTY.
           CANCEL FORM-PROGX.

      ****************************************************************

       LPDLADJ-COMPUTE-GL-FEEAMT SECTION.
           ADD 1 TO DLADJ.
           IF DLADJ < 5
              IF PD-INSOURS(DLADJ) = "Y"
                 ADD PD-INSPREM(DLADJ) TO DLADJ-INS-OURS.
           IF DLADJ < 3
              IF SP-FEEFRMLA(DLADJ) NOT = " "
                 GO TO LPDLADJ-COMPUTE-GL-FEEAMT.
           IF DLADJ NOT > 10
              IF PD-FEEOURS(DLADJ) NOT = "M"
                 ADD PD-FEEAMT(DLADJ) TO DLADJ-FEE-AMT
                 GO TO LPDLADJ-COMPUTE-GL-FEEAMT
              ELSE
                 GO TO LPDLADJ-COMPUTE-GL-FEEAMT.

