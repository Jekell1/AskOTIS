      * COPYMEMBER: LIBLP/LPAPRS
      ***********************************************************************
      *   COMPUTE EQUIVALENT SIMPLE INTEREST RATE
      *
      *   NAME: LPAPRS
      *   DESC: THIS ROUTINE COMPUTES THE EQUIVALENT SIMPLE
      *         INTEREST RATE FOR A LOAN. THIS RATE IS
      *         SOMETIMES REFERRED TO AS THE STATE APR.
      *         APPROXIMATE APR TO 3 DECIMALS ACCURACY
      *
      *         SP-CAL-CHGABLE = FORMULA FOR DETERMINING THE
      *                          INTEREST CHARGABLE
      *         SP-PYEXFRMLA   = FORMULA FOR COMPUTING THE 1ST
      *                          PAYMENT EXTENSION CHARGE
      *         SP-CAL-RATETYPE= METHOD TO COMPUTE INTEREST
      *
      *   OUT : APRS-SMPRATE     - 2 DECIMAL APPROX ACCURACY.
      *         APRS-EFFRATE     - 2 DECIMAL APPROX ACCURACY.
      *         APRS-INTCHGABLE  - AMOUNT INTEREST WAS CHARGED ON.
      *         APRS-REGPYAMT    - PAYMENT TO COVER INTCHGABLE
      *                            AND INTEREST OVER TERM OF
      *                            LOAN.
      *         APRS-CTBL        - SPR CAL TABLE INDEX
      *
      *   COPY: LPAPRSW, LPAPRZ, LPAPR
      *
      * REV:
      *  JTG 062288 ADDED OUTPUT FOR INTCHGABLE AND REGPYAMT
      *  JTG 110388 FIXED BUG RE: INTCHGABLE INCLUDING EXTCHG
      *  JTG 080889 CHANGED TO RETURN INDEX TO RATE TABLE
      *  JTG 090391 CORRECTED ROUNDING PROBLEM FOR EFF RATE
      *             WHEN ZERO FINCHG
      *  JTG 011792 ADDED UNITPER LOGIC
      *  JTG 043093 SET APR-DISFRMLA AND NOT SP-DISFRMLA
      *  JTG 052493 MODIFIED FOR BALLOONS
      *  JTG 061093 ADDED LOGIC FOR SP-PYEXTFRMLA = 9
      *  JTG 062995 ADDED REF. TO ALL 10 INSURANCES
      *  JTG 040896 CORRECTED SINGLE PAYS AND BALLOONS
      *  JTG 043096 FIXED BUG IN USE OF UPER-FRACT-UNITPER
      *  JTG 111296 CHANGED LN-INSTBL (LEVEL'S 9 & 10) TO FILLER
      *  JTG 050697 ADDED USE OF SETCTBL COPY MEMBER
      *  JTG 101497 FIXED BUG WHEN ACCOUNT HAS MAINTENANCE FEES, CITIZENS
      *  JTG 093098 FIXED TO IS INSURE THAT WHEN THEIR IS NO ODD LAST PAY
      *             THAT THE RECOMPUTED APR-REGPYAMT IS ALSO USED
      *             FOR APR-LASTPYAMT
      *  JTG 100998 CORRECTED LOGIC TO CORRECTLY HANDLE BALLOONS
      *  JTG 122398 FIXED MISSING SET OF APRS-REGPYAMT FOR REFUND ROUTINES
      *  JTG 000619 ADDED LOGIC FOR TENNESSEE RATE TYPE 'M' FLAT MONTHLY CHARGE
      *             #1225
      *  JTG 000710 CORRECTED LOGIC WHEN CALCULATING EFFECTIVE RATES IN
      *             EAA-CALC-ADDON & EAA-CALC-DISCOUNT
      *  JTG 000914 CORRECTED BUG IN PASS TO SETCTBL, ROUTINE WAS PASSING
      *             LN-LNAMT INSTEAD OF INTEREST CHARGEABLE     WORLD #J011
      *  JTG 010228 ADDED NEW RATE TYPE 'Z' AMORTIZED WHERE RUNTIME COMPUTES
      *             INTEREST ON A 30 DAY METHOD (REAL ESTATE) AND LOAN CALCS
      *             ARE DONE SIMPLE                   LENDMARK PR#1045 & PR#1380
      *  JTG 020219 ADDED "CT" COUNTY TAX CODE KENTUCKY AND PERCENTS
      *             ADDED "CY" CITY   TAX CODE KENTUCKY AND PERCENTS  WORLD #288
      *  JTG 020620 CHANGED FOR (MIP) MONTHLY INSURANCE PREMIUMS   REGENCY #1801
      *             USED NEW FIELD TOTAL-INSPREM-NONMEMO FROM LPAPRS
      *  JTG 030430 CREATED FORMULA 12 LIKE 2 BUT SPREADS EXTENSION CHARGE
      *             OVER PAYMENTS  88  SP-PYEXFRMLA-SAME2-SPREAD VALUE 12.
      *             ADDED APRS-SPREAD APRS-REM                    CITIZENS #2006
      *  JTG 031112 CHANGED LP-CALCZ3 AND LIBLP/LPAPRS LOGIC TO COMPUTE
      *             SIMPLE RATE FOR SINGLE PAY LOANS UNDER U.S.RULE
      *             DISCLOSURE METHOD 'C'                         LENDMARK #J036
      *  JTG 060728 CORRECTED PROBLEM WITH LN-EFFRATE AND LN-OVRRATE GOING
      *             AWAY AFTER LOAN BOOKING  LITIGANT LOANS       LENDMARK #J053
      *  MJD 091006 CORRECTED TO CORRECTLY CALCULATE APR WHEN MEXICO VAT
      *             TAX IS USED.  IT MAY BE TO LARGE FOR LN-MAINTFEE SO USE
      *             LN-ANTICERN(4) AND DO NOT REMOVE IT FROM PAYMENT AMOUNTS
      *             WHEN CALCULATING APR                          WORLD #0651
      *  MJD 100901  CORRECTED SIMPLE INT CALCULATION.  IF MFFRMLA-05
      *              THEN USE ANTICERN(4) INSTEAD OF MAINTFEE TO
      *              PREVENT TRUNCATION OF MONTHLY AMOUNTS IN THE CALCULATIONS.
      **************************************************************************
       APRS-RATE SECTION.
           PERFORM APRS-GET-RATE-TABLE.
           PERFORM APRS-GET-SIMPLE-RATE.
           PERFORM APRS-GET-EFFRATE.

      *******************************************
      *    DETERMINE RATE TABLE TO USE
      *******************************************
       APRS-GET-RATE-TABLE SECTION.
           PERFORM LOAN-CALCULATIONS.
           MOVE INT-CHARGEABLE TO CTBL-INT-CHARGEABLE.
           MOVE LN-ORGTERM     TO CTBL-TERM.
           PERFORM SETCTBL.
           MOVE CTBL-INDEX TO CTBL
                              APRS-CTBL.

      *******************************************
      *    COMPUTE THE SIMPLE INTEREST RATE
      *******************************************
       APRS-GET-SIMPLE-RATE SECTION.
           MOVE LN-UNITPER-CD   TO APR-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO APR-UNITPER-FREQ.
           MOVE LN-INTDATE      TO APR-INTDATE.
           MOVE LN-1STPYDATE    TO APR-1STPYDATE.

      * COMPUTE TRUE INTEREST CHARGABLE:
           MOVE 0 TO APRS-PYAMT-ADJ.
           IF SP-CAL-CHGABLE(CTBL) = "P" OR "S"
              SUBTRACT TOTAL-INSPREM-NONMEMO
                       TOTAL-INS-TAX
                       FROM APRS-PYAMT-ADJ.
           IF SP-CAL-CHGABLE(CTBL) = "P" OR "I"
              SUBTRACT LN-SERCHG FROM APRS-PYAMT-ADJ.

      *----------------------------------------------------------------------
      * IF MEXICO VAT TAX THEN USE LN-ANTICERN(4), THE LN-MAINTFEE FIELD IS
      * TO SMALL AND THE NUMBER THERE MAY BE TRUNCATED        (WORLD #651)
      *----------------------------------------------------------------------
           IF SP-MFFRMLA-05
              COMPUTE APR-FINANCED =
                   LN-LNAMT - LN-ANTICERN(4) + APRS-PYAMT-ADJ
           ELSE
              COMPUTE APR-FINANCED =
                   LN-LNAMT - (LN-MAINTFEE * LN-ORGTERM)
                            + APRS-PYAMT-ADJ.

           IF LN-LOANTYPE = "P"
              SUBTRACT LN-INTCHG LN-EXTCHG FROM APR-FINANCED.

      * SET INTEREST CHARGABLE AMOUNT:
           MOVE APR-FINANCED TO APRS-INTCHGABLE.

           MOVE LN-ORGTERM TO APR-ORGTERM.

      * DERIVE PAYMENT AMOUNTS FROM TRUE INTEREST CHARGABLE:

           COMPUTE APRS-PYAMT-ADJ =
                   APRS-PYAMT-ADJ / LN-ORGTERM.

           MOVE LN-1STPYAMT   TO APR-1STPYAMT.
           MOVE LN-REGPYAMT   TO APR-REGPYAMT.
           MOVE LN-LASTPYAMT  TO APR-LASTPYAMT.

      *----------------------------------------------------------------------
      * REESTABLISH PAYMENTS PRIOR TO SPREAD OF LN-EXTCHG
      * PER SP-PYEXFRMLA-SAME2-SPREAD (12)
      *----------------------------------------------------------------------
           IF SP-PYEXFRMLA-SAME2-SPREAD
              DIVIDE LN-ORGTERM INTO LN-EXTCHG
                           GIVING APRS-SPREAD
                           REMAINDER APRS-REM
              ADD LN-EXTCHG TO APR-1STPYAMT
              SUBTRACT APRS-SPREAD FROM APR-1STPYAMT
                                        APR-REGPYAMT
              IF LN-ORGTERM NOT = 1
                 SUBTRACT APRS-SPREAD APRS-REM FROM APR-LASTPYAMT.
      *----------------------------------------------------------------------

      *----------------------------------------------------------------------
      *  IF MAINT FORMULA 5 THEM USE LN-ANTICERN(4),
      *  LN-MAINTFEE MAY BE TRUNCATED
      *----------------------------------------------------------------------
           IF SP-MFFRMLA-05
              COMPUTE APR-1STPYAMT =
                            APR-1STPYAMT - LN-EXTCHG
                                         - (LN-ANTICERN(4) / LN-ORGTERM)
                                         + APRS-PYAMT-ADJ
              COMPUTE APR-REGPYAMT
                      APRS-REGPYAMT =
                            APR-REGPYAMT - (LN-ANTICERN(4) / LN-ORGTERM)
                                        + APRS-PYAMT-ADJ
              IF LN-ORGTERM NOT = 1
                 COMPUTE APR-LASTPYAMT =
                     APR-LASTPYAMT - (LN-ANTICERN(4) / LN-ORGTERM)
                                         + APRS-PYAMT-ADJ
              END-IF
           ELSE
              COMPUTE APR-1STPYAMT =
                            APR-1STPYAMT - LN-EXTCHG - LN-MAINTFEE
                                         + APRS-PYAMT-ADJ
              COMPUTE APR-REGPYAMT
                      APRS-REGPYAMT =
                            APR-REGPYAMT - LN-MAINTFEE
                                        + APRS-PYAMT-ADJ
              IF LN-ORGTERM NOT = 1
                 COMPUTE APR-LASTPYAMT =
                     APR-LASTPYAMT - LN-MAINTFEE
                                         + APRS-PYAMT-ADJ.

      *----------------------------------------------------------------------
      * IF 1ST PAYMENT EXTENTION CHARGE FORMULA '1' INDICATING SPREAD
      *        OF 1ST PAYMENT EXTENTION PERIOD CHARGE OVER ALL PAYMENTS
      *          OR WHEN APR-1STPYAMT NOT = APR-REGPYAMT WHICH COULD OCCUR
      *             IN SALES FINANCE INPUT WHICH ALLOWES SETUP OF PAYMENT
      *             STREAM WITH NO RHYME OR REASON
      *    GET TRUE REGZ CALCULATION
      * ELSE
      *    GET REVISED APR FOR EVEN PAYMENT SCHEDULE AND EVEN PAYMENT AMOUNT
      *----------------------------------------------------------------------
           IF (SP-PYEXFRMLA-REG-INCR) OR
              (APR-1STPYAMT NOT = APR-REGPYAMT)
              IF SP-DISFRMLA-USRULE
                 MOVE "C" TO APR-DISFRMLA
              ELSE
                 MOVE "A" TO APR-DISFRMLA
              END-IF
           ELSE
              MOVE "X" TO APR-DISFRMLA.
      *----------------------------------------------------------------------

           PERFORM APR-REGZ.
           COMPUTE APRS-SMPRATE ROUNDED = APR-APR.

      *******************************************
      *    COMPUTE EFFECTIVE RATE
      *******************************************
       APRS-GET-EFFRATE SECTION.
           MOVE 0 TO APRS-EFFRATE.
           IF SP-CAL-RATETYPE(CTBL) = "S" OR "M" OR "Z" OR "G"
              MOVE APRS-SMPRATE TO APRS-EFFRATE
           ELSE
           IF SP-CAL-RATETYPE(CTBL) = "A"
              PERFORM APRS-EFFRATE-ADDON
           ELSE
           IF SP-CAL-RATETYPE(CTBL) = "D"
              PERFORM APRS-EFFRATE-DISCOUNT.

       APRS-GET-EFFRATE-EXIT.
           EXIT.

      *-------------------------------------------------
      *    COMPUTE EFFECTIVE DISCOUNT RATE
      *-------------------------------------------------
       APRS-EFFRATE-DISCOUNT SECTION.
           IF SP-PYEXFRMLA-REG-INCR
              COMPUTE APRS-EFFRATE ROUNDED =
                  (
                    ((1 - APR-FINANCED / (APR-FINANCED + LN-INTCHG)))
                         * (UPER-UNITPER-PER-YEAR
                               / (LN-ORGTERM + UPER-FULL-UNITPER - 1
                                     + UPER-FRACT-UNITPER
                                 )
                           )
                  ) * 100
           ELSE
              COMPUTE APRS-EFFRATE ROUNDED =
                  (
                    ((1 - APR-FINANCED / (APR-FINANCED + LN-INTCHG)))
                         * (UPER-UNITPER-PER-YEAR / LN-ORGTERM)
                  ) * 100.

      *----------------------------------------------
      *    COMPUTE EFFECTIVE ADDON RATE
      *----------------------------------------------
       APRS-EFFRATE-ADDON SECTION.
           IF SP-PYEXFRMLA-REG-INCR
              COMPUTE APRS-EFFRATE ROUNDED =
                  (
                    (((APR-FINANCED + LN-INTCHG) / APR-FINANCED) - 1)
                         * (UPER-UNITPER-PER-YEAR
                               / (LN-ORGTERM + UPER-FULL-UNITPER - 1
                                     + UPER-FRACT-UNITPER)
                           )
                  ) * 100
           ELSE
              COMPUTE APRS-EFFRATE ROUNDED =
                  (
                    (((APR-FINANCED + LN-INTCHG) / APR-FINANCED) - 1)
                         * (UPER-UNITPER-PER-YEAR / LN-ORGTERM)
                  ) * 100.

