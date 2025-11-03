      *================================================================*
      * END COPYBOOK: LIBLP\LPSCHD.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBLP\LPCEPP.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBLP/LPCEPP
      **********************************************************
      *         COMPUTE CONTRACTUAL ELAPSED PAY PERIODS
      *
      *   NAME: LPCEPP
      *   DESC: THIS ROUTINE RETURNS THE NUMBER OF
      *         ELAPSED (CONTRACTUAL) PAY PERIODS
      *         AND THE REMAINING PORTION, IF ANY, IN DAYS.
      *
      *         DEFERRED AND ALLOWABLE DELINQUENT PERIODS
      *         ARE NOT CONSIDERED BY DEFAULT.
      *
      *         CALCULATIONS ARE CAPPED AT MATURITY
      *         BY DEFAULT.
      *
      *         NOTE:
      *               IF CEPP-INCDEF-FG HAS A VALUE
      *               OTHER THAN SPACES, DEFERRED
      *               PERIODS WILL BE CONSIDERED.
      *
      *               IF CEPP-INCALDEL-FG HAS A VALUE
      *               OTHER THAN SPACES, ALLOWABLE
      *               DELINQUENT PERIODS WILL BE CONSIDERED.
      *
      *               IF CEPP-NOCAP-FG HAS A VALUE
      *               OTHER THAN SPACES, ELAPSED
      *               PERIODS WILL NOT BE CAPPED
      *               AT ORIGINAL TERM.
      *
      *   IN  : CEPP1-DATE     (LN-1STPYDATE)
      *         CEPP2-DATE     (TODAYS DATE)
      *   OUT : CEPP-PERIODS
      *         CEPP-UNITPER-REM
      *
      *         N O T E:
      *                  CEPP1-DATE AND CEPP2-DATE NOT ALTERED
      *   USED:
      *   COPY: LPCEPPW
      * REV:
      *  031286 JTG STOPPED ALTERATION OF CEPPX-DATES.
      *  050986 JTG ADDED TEST ON CEPP-NOCAP-FG
      *             SO CONTRACTUAL AGEING DONE BY
      *             AGEING.IN.C WOULD COMPUTE CORRECTLY.
      *  052386 JTG ADDED LINE TO CLEAR CEPP-NOCAP-FG
      *  101686 JTG CHANGED TIME FROM 365 TO 360
      *             TO YIELD TRUE MONTHLY INTERVALS
      *  020287 JTG ADDED THE OPTION OF INCLUDING
      *             DEFERMENTS IN THE CALCULATION.
      *  040590 JTG INCLUDED LN-TOTNOALDEL IN CALCULATION
      *             WITH OPTIONAL DEFERMENTS FOR PACESETTER
      *  071696 JTG ADDED LOGIC FOR UNIT PERIODS
      *             ADDED RETURN OF CEPP-UNITPER-REM (CAC) FL
      *  090696 JTG CORRECTED BUG YIELDING 1 ELAPSED PAY PERIOD
      *             WHEN TODAY IS PRIOR TO FIRST PAY DUE DATE
      **********************************************************
       CONTRACTUAL-ELAPSED-PAYPER SECTION.
           MOVE 0 TO CEPP-PERIODS
                     CEPP-UNITPER-REM.

      * SEE IF TODAY IS PRIOR TO FIRST PAY DATE:
           MOVE CEPP1-DATE TO NUM-DATE.
           MOVE CEPP2-DATE TO SYS-DATE.
           IF SYS-DATE < NUM-DATE
              GO TO CONTRAL-ELAPSED-PAYPER-EXIT.

           IF CEPP-NOCAP-FG = " "
              PERFORM MATURITY-DATE-CALCULATION
              MOVE MDTE-DATE TO NUM-DATE
              MOVE CEPP2-DATE TO SYS-DATE
              IF SYS-DATE > NUM-DATE
                 MOVE MDTE-DATE TO SYS-DATE
              ELSE
                 NEXT SENTENCE
           ELSE
              MOVE CEPP2-DATE TO SYS-DATE.

           MOVE CEPP1-DATE TO NUM-DATE.
           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM TIMUPER.
           IF ELAPSED-DAYS < 0
              GO TO CONTRAL-ELAPSED-PAYPER-EXIT.

           MOVE ELAPSED-UNITPER-REM TO CEPP-UNITPER-REM.

           IF CEPP-INCDEF-FG = " "
              SUBTRACT LN-TOTNODEF FROM ELAPSED-UNITPER.

           IF CEPP-INCALDEL-FG = " "
              SUBTRACT LN-TOTNOALDEL FROM ELAPSED-UNITPER.

           ADD 1 ELAPSED-UNITPER GIVING CEPP-PERIODS.

           IF CEPP-PERIODS < 0
              MOVE 0 TO CEPP-PERIODS
                        CEPP-UNITPER-REM.

       CONTRAL-ELAPSED-PAYPER-EXIT.
           MOVE " " TO CEPP-NOCAP-FG
                         CEPP-INCDEF-FG
                           CEPP-INCALDEL-FG.


      *================================================================*
      * END COPYBOOK: LIBLP\LPCEPP.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBLP\LPAPRS.CPY                           *
      *================================================================*
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
