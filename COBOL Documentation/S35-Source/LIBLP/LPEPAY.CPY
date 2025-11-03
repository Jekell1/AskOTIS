      * COPYMEMBER: LIBLP/LPEPAY

      ***************************************************************
      *          PAYMENT CALCULATION USING ACTUARIAL OR US RULE
      *
      *   NAME:  LPEPAY
      *   DESC:  THIS ROUTINE WILL DETERMINE THE STANDARD PAYMENT
      *          AMOUNT REQUIRED TO PAYOFF A LOAN GIVEN:
      *
      *            - PRESENT VALUE OF LOAN
      *            - ANNUAL RATE OF INTEREST
      *            - TERM
      *            - SCHEDULED PAY DATES
      *            - DISCLOSURE METHOD
      *            - UNIT PERIOD CODE
      *            - UNIT PERIOD FREQUENCY
      *
      *---------------------------------------------------------
      *
      *   IN  :  EPAY-PVALUE          PRESENT VALUE OF MONIES
      *          EPAY-RATE            INTEREST RATE TO CHARGE
      *          EPAY-ORGTERM         TERM PAYMENT MUST COVER
      *          EPAY-UNITPER-CD      PAYMENT PERIOD CODE
      *                                  EX. 'M' = MONTHLY
      *          EPAY-UNITPER-FREQ    FREQUENCY OF PAYMENTS
      *                                  EX. '2' = EVERY 2 WEEKS
      *          EPAY-INTDATE         INTEREST START DATE
      *          EPAY-1STPYDATE       FIRST PAYMENT DATE
      *          EPAY-DISFRMLA        DISCLOSURE FORMULA
      *                                  A - ACTUARIAL
      *                                  C - US RULE
      *          EPAY-INTMETHOD       360 BY DEFAULT,
      *                               IF OTHER THAN 360, SET IT
      *
      *   OUT :  EPAY-PAYMNT          AMOUNT OF REGULAR PAYMENT
      *   COPY:  LPEPAYW
      * REV :
      *  JTG 050892 CORRECTED EPAY-PAYMNT EQUATION
      *  JTG 071092 ADDOWED FOR 0 DAYS TO 1ST PAYMENT
      *  JTG 100992 STOPPED FORCED USRULE ON SINGLE PAY
      *  JTG 021197 CHANGED TO WORK FOR OTHER THAN MONTHLY PAPER
      ***************************************************************
       EPAY-ACTUARIAL-CALC SECTION.
           MOVE 0 TO EPAY-PAYMNT.

           IF EPAY-PVALUE = 0
                 OR EPAY-ORGTERM = 0
                  OR EPAY-RATE = 0
                   OR EPAY-INTDATE = 0
                    OR EPAY-1STPYDATE = 0
              GO TO EPAY-ACTUARIAL-CALC-EXIT.

      ***********************************************************
      *    SETUP UNIT PERIOD INFORMATION:
      ***********************************************************
           MOVE EPAY-ORGTERM TO UPER-TERM.
           MOVE EPAY-UNITPER-CD TO UPER-UNITPER-CD.
           MOVE EPAY-UNITPER-FREQ TO UPER-UNITPER-FREQ.
           MOVE EPAY-INTDATE TO UPER-INTDATE.
           MOVE EPAY-1STPYDATE TO UPER-1STPYDATE.
           PERFORM UPER-CALCULATION.
           IF UPER-ERRCD NOT = " "
              GO TO EPAY-ACTUARIAL-CALC-EXIT.

      ***************************************************
      *    DETERMINE IF 1STPYDATE IS BEFORE INTDATE
      ***************************************************
           MOVE EPAY-INTDATE TO NUM-DATE.
           MOVE EPAY-1STPYDATE TO SYS-DATE.
           IF SYS-DATE < NUM-DATE
              GO TO EPAY-ACTUARIAL-CALC-EXIT.

      **********************************************************
      *    COMPUTE (EPAYI) INTEREST RATE PER COMPOUND PERIOD:
      **********************************************************
           COMPUTE EPAYI ROUNDED =
              EPAY-RATE / 100 / UPER-UNITPER-PER-YEAR.

      * INTEREST METHOD ADJUSTMENT ONLY MAKES SENSE FOR MONTHLY:
           IF EPAY-INTMETHOD NOT = 0
              IF EPAY-UNITPER-CD = "M"
                 COMPUTE EPAYI ROUNDED =
                      EPAYI * (360 / EPAY-INTMETHOD).

      *********************************************************
      *    SET (EPAYF) FRACTIONAL PERIOD FROM INTDATE TO FIRST
      *    PAYMENT DUE DATE:
      *********************************************************
           MOVE UPER-FRACT-UNITPER TO EPAYF.

      *******************************************************
      *    SET (EPAYT) WHOLE PERIOD(S) FROM INTDATE TO FIRST
      *    PAYMENT DUE DATE:
      *******************************************************
           MOVE UPER-FULL-UNITPER TO EPAYT.

      *************************************************************
      *    COMPUTE (EPAYADJ) ADJUSTMENT FACTOR TO 1ST DUE PAYMENT:
      *
      * NOTE:
      *      COMPOUNDING OVER THE FIRST PERIOD BEYOND ONE
      *      MONTH IS STARED OUT.
      *************************************************************
      * SINGLE PAY LOANS AND US RULE:
      *    IF EPAY-ORGTERM < 2 OR EPAY-DISFRMLA = "C"
           IF EPAY-DISFRMLA = "C"
              COMPUTE EPAYADJ ROUNDED =
                 1 / (1 + (EPAYT + EPAYF) * EPAYI)
           ELSE
      * ACTUARIAL:
              COMPUTE EPAYADJ ROUNDED =
                 1 / ((1 + EPAYF * EPAYI) * (1 + EPAYI) ** EPAYT).

      ***************************************************
      *    ESTABLISH ANNUITY PAYMENTS REQUIREMENTS:
      ***************************************************
           COMPUTE EPAYPW ROUNDED =
              (1 - 1 / (1 + EPAYI) ** (EPAY-ORGTERM - 1)) / EPAYI.

      ***************************************************
      *    COMPUTE REGULAR PAYMENT AMOUNT:
      ***************************************************
           COMPUTE EPAY-PAYMNT ROUNDED =
              EPAY-PVALUE / EPAYADJ / (1 + EPAYPW).

       EPAY-ACTUARIAL-CALC-EXIT.
           MOVE 360 TO EPAY-INTMETHOD.
