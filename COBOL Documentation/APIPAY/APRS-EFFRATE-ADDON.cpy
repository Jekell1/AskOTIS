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

      *================================================================*
      * END COPYBOOK: LIBLP\LPAPRS.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBLP\LPAPRZ.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBLP/LPAPRZ
      *****************************************************************
      *         COMPUTE APPROXIMATE APR TO 4 DECIMALS
      *         FEDERAL RESERVE REGULATION Z METHOD
      *
      *   NAME: LPAPRZ
      *   DESC: APPROXIMATE APR TO 4 DECIMALS ACCURACY
      *   IN  : APR-FINANCED        AMOUNT FINANCED
      *         APR-INTDATE         INTEREST START DATE
      *         APR-1STPYDATE       FIRST PAYMENT DATE
      *         APR-ORGTERM         TOTAL NO OF PAYMENTS
      *         APR-1STPYAMT        AMOUNT OF 1ST PAYMENT
      *         APR-REGPYAMT        AMOUNT OF REGULAR PAYMENT STREAM
      *         APR-LASTPYAMT       THE AMOUNT OF THE LAST PAYMENT
      *         APR-UNITPER-CD      PAYMENT PERIOD CODE   EX. 'M' MONTHLY
      *         APR-UNITPER-FREQ    FREQUENCY OF PAYMENTS EX. 2 = EVERY 2 WEEKS
      *         APR-DISFRMLA        DISCLOSURE FORMULA:
      *                               'A' - ACTUARIAL
      *                               'B' - SALE FINANCE RATE CHARTS
      *                               'C' - U.S. RULE
      *                               'X' - FRONT END CALCULATOR
      *                                     SAME AS 'B' BUT ALLOWS
      *                                     ODD LAST PAYMENT
      *
      *   OUT : APR-APR             999.9999 DECIMAL APPROX APR
      *   COPY:
      * REV :
      *  JTG 022190 CORRECTED SET OF APR-FRACT-UNITPER WHEN ELAPSED-DAYS
      *                   BECAME LESS THAN 0
      *  JTG 041690 ADDED SET OF APR-METHOD FOR U.S. RULE
      *  JTG 060590 ADDED ALTERNATIVE SINGLE PAY DISCLOSURE METHOD
      *                   SP-DISFRMLA-SPAY-FG
      *  JTG 121291 ADDED BALLOON PAYMENT INFORMATIOM APR-LASTPYAMT
      *  JTG 042997 CHANGED FOR SINGLE PAY LOANS
      *  JTG 032498 CHANGED TO ALLOW SINGLE PAYS COMPUTE BY U.S.RULE
      *****************************************************************
