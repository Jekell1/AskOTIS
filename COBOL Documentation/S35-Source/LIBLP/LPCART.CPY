      * COPYMEMBER: LIBLP/LPCART
      *****************************************************************
      *                           (RT)
      *         THIS ROUTINE CALCULATES THE REQUIRED PAYMENT
      *         AND BALLOON, NECESSARY TO BOTH:
      *             - YIELD THE EFFECTIVE RATE
      *             - SPREAD THE AMOUNT FINANCED, FINANCE CHARGE
      *               AND REMAINING AMOUNT DUE ON THE LOAN, INTO
      *               EVEN PAYMENT AMOUNTS
      *
      *   NAME: LPCART
      *   DESC: CALCULATE PAYMENT FOR RT USING EFFRATE
      *   IN  : CART-FINANCED          AMOUNT FINANCED
      *         CART-TERM              TERM PAYMENT MUST COVER
      *         CART-SUM-REGPYAMT      CURRENT PAYMENT AMOUNT
      *         CART-INTMETHOD         MAKES SENSE FOR MONTHLY ONLY
      *                                  360 BY DEFAULT,
      *                                    IF OTHER THAN 360, SET IT
      *         CART-RATE              INTEREST RATE TO CHARGE
      *         CART-UNITPER-CD        PAYMENT PERIOD CODE
      *                                   EX. 'M' = MONTHLY
      *         CART-UNITPER-FREQ      FREQUENCY OF PAYMENTS
      *                                   EX. '2' = EVERY 2 WEEKS
      *         CART-INTDATE           INTEREST START DATE
      *         CART-1STPYDATE         FIRST PAYMENT DATE
      *         CART-DISFRMLA          DISCLOSURE FORMULA
      *                                   A - ACTUARIAL
      *                                   C - US RULE
      *         CART-AMTDUE            REMAINING AMOUNT DUE ON LOAN
      *         CART-NEG-FG            TO INDICATE A DECREASE IN PAYMENTS
      *                                   ' '  - POSITIVE ADJUSTMENT
      *                                   'Y'  - NEGATIVE ADJUSTMENT
      *
      *   OUT : CART-PAYMENT           AMOUNT OF REGULAR PAYMENT
      *         CART-BALLOON           AMOUNT OF ????
      *   COPY:
      * REV :
      *  JTG 083192 CORRECT SINGLE PAY ET BALLOON CALC
      *  JTG 042597 CHANGED TO HANDLE UNIT PERIODS
      *****************************************************************
       CART SECTION.
           MOVE 0 TO CART-PAYMENT
                     CART-BALLOON.
           IF CART-TERM = 0
                    OR CART-FINANCED = 0
                     OR CART-INTDATE = 0
                      OR CART-1STPYDATE = 0
              GO TO CART-EXIT.

           PERFORM CART-CALC-FACTORS.
           IF CART-ERRCD NOT = " "
              GO TO CART-EXIT.

           COMPUTE CART-PMT = CART-FINANCED / CART-TERM.
           MOVE 1 TO CART-ADJ.
           MOVE 0 TO CART-COUNT.

       CART-INTERPOLATE-LOOP.
           MOVE CART-ADJ TO CART-ADJ-POS.
           IF CART-ADJ-POS NOT > 0.01
              GO TO CART-INTERPOLATE-END.

           ADD 1 TO CART-COUNT.
           IF CART-COUNT < 50
              PERFORM CART-CALC-PV
              MOVE CART-PV TO CART-PV1
              ADD CART-DELTA TO CART-PMT
              PERFORM CART-CALC-PV
              MOVE CART-PV TO CART-PV2
              COMPUTE CART-ADJ ROUNDED =
                 CART-DELTA * (CART-FINANCED - CART-PV1)
                                     / (CART-PV2 - CART-PV1)
                 ADD CART-ADJ TO CART-PMT ROUNDED
              GO TO CART-INTERPOLATE-LOOP.

       CART-INTERPOLATE-END.
           IF CART-PMT > 0
              IF CART-COUNT NOT > 50
                 MOVE CART-PMT TO CART-PAYMENT
                 PERFORM CART-CALC-BALLOON.
      * THIS COMPUTE PROVIDES FOR AN EQUAL BALLOON TO REG PAYMENT:
      *   THE CART-CALC-BALLOON PROVIDES FOR TRUE EFFECTIVE RATE YIELD:
      *          COMPUTE CART-BALLOON =
      *             CART-TERM * (CART-PAYMENT + CART-SUM-REGPYAMT)
      *               - CART-AMTDUE - CART-TERM-1 * CART-PAYMENT.

       CART-EXIT.
           EXIT.

      ******************************************************
      *    CALCULATE PRESENT VALUE
      ******************************************************
       CART-CALC-PV SECTION.
           IF CART-NEG-FG = " "
              PERFORM CART-CALC-PV-POS
           ELSE
              PERFORM CART-CALC-PV-NEG.

      ******************************************************
      *    CALCULATE PRESENT VALUE POSITIVE ADJUSTMENT
      ******************************************************
       CART-CALC-PV-POS SECTION.
           COMPUTE CART-PV ROUNDED =
              CART-1STFAC *
              (
                CART-PMT * CART-ADNI-1
                  +
                    (
                      CART-TERM
                        * ( CART-PMT + CART-SUM-REGPYAMT )
                              - CART-AMTDUE - CART-TERM-1 * CART-PMT
                    )
                      /
                        (
                          CART-I1 ** CART-TERM-1
                        )
              ).

       CART-CALC-PV-POS-EXIT.
           EXIT.

      ******************************************************
      *    CALCULATE PRESENT VALUE NEGATIVE ADJUSTMENT
      ******************************************************
       CART-CALC-PV-NEG SECTION.
           COMPUTE CART-PV ROUNDED =
              CART-1STFAC *
              (
                CART-PMT * CART-ADNI-1
                  +
                    (
                      CART-TERM
                        * ( CART-PMT - CART-SUM-REGPYAMT )
                               - CART-TERM-1 * CART-PMT + CART-AMTDUE
                    )
                      /
                        (
                          CART-I1 ** CART-TERM-1
                        )
              ).

       CART-CALC-PV-NEG-EXIT.
           EXIT.

      ******************************************************
      *    CALCULATE BALLOON PAYMENT
      ******************************************************
       CART-CALC-BALLOON SECTION.
           COMPUTE CART-BALLOON ROUNDED =
              (
                CART-I1 ** CART-TERM-1
              )
                 *
                   (
                      CART-FINANCED / CART-1STFAC
                         -  CART-PMT * CART-ADNI-1
                   ).

       CART-CALC-BALLOON-EXIT.
           EXIT.

      ******************************************************
      *    CALCULATE ALL FACTORS PER TIME
      * REV:
      *  JTG 042597 CHANGED TO HANDLE UNIT PERIODS
      ******************************************************
       CART-CALC-FACTORS SECTION.
           MOVE " " TO CART-ERRCD.
           IF CART-DISFRMLA = "B"
              MOVE "C" TO CART-DISFRMLA.

       CART-CALC-FACTORS-CONT.
      * TERM USED IN PRESENT VALUE COMPUTATIONS
           SUBTRACT 1 FROM CART-TERM GIVING CART-TERM-1.

           IF CART-1STPYDATE < CART-INTDATE
              GO TO CART-CALC-FACTORS-END.

      * SET UNIT PERIOD INFORMATION
           MOVE CART-UNITPER-CD TO UPER-UNITPER-CD.
           MOVE CART-UNITPER-FREQ TO UPER-UNITPER-FREQ.
           MOVE CART-INTDATE TO UPER-INTDATE.
           MOVE CART-1STPYDATE TO UPER-1STPYDATE.
           MOVE CART-TERM TO UPER-TERM.
           PERFORM UPER-CALCULATION.

      * FIND WHOLE UNIT PERIODS BETWEEN INTDATE AND 1STPYDATE,
      *         RESULT APPEARS IN CART-W
      * FIND FRACTIONAL UNIT PERIOD BETWEEN INTDATE AND 1STPYDATE,
      *         RESULT APPEARS IN CART-F
      *--------------------------------------------------------------
           IF CART-TERM = 1
              MOVE UPER-SINGLE-PAY-CD TO DATER-UNITPER-CD
              MOVE UPER-SINGLE-PAY-FREQ TO DATER-UNITPER-FREQ
           ELSE
              MOVE UPER-UNITPER-CD TO DATER-UNITPER-CD
              MOVE UPER-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           MOVE CART-INTDATE TO NUM-DATE.
           MOVE CART-1STPYDATE TO SYS-DATE.
           PERFORM TIMUPER.
           MOVE ELAPSED-UNITPER TO CART-W.
           IF CART-W < 0
              GO TO CART-CALC-FACTORS-END.
           COMPUTE CART-F ROUNDED = ELAPSED-UNITPER-REM
                                / UPER-DAYS-IN-UNITPER.

      **********************************************************
      * SET UP METHOD ACTUARIAL VS U.S. RULE:
      *   NOTE:
      *    'B' IS DEFAULTING TO U.S. RULE SINCE IT DOESN'T
      *    REQUIRE A POWER RAISE ON WHOLE MONTHS IN
      *    FIRST PAY PERIOD.
      *
      *    ON SINGLE PAY LOANS ACTUARIAL IS DONE LIKE US RULE.
      **********************************************************
           IF CART-DISFRMLA = "A" AND CART-TERM-1 NOT = 0
              MOVE "A" TO CART-METHOD
           ELSE
              MOVE "U" TO CART-METHOD.

           COMPUTE CART-I ROUNDED =
              CART-RATE * 0.01 / UPER-UNITPER-PER-YEAR.
           IF CART-INTMETHOD NOT = 0
              IF UPER-UNITPER-CD = "M"
                 COMPUTE CART-I ROUNDED =
                      CART-I * (360 / CART-INTMETHOD).
           ADD 1 CART-I GIVING CART-I1.

           COMPUTE CART-ADNI-1 ROUNDED =
              CART-I1 * (1 - 1 / CART-I1 ** CART-TERM-1) / CART-I.
           IF CART-TERM < 3
              MOVE 0 TO CART-ANI-2
           ELSE
              COMPUTE CART-ANI-2 ROUNDED =
                  (1 - 1 / CART-I1 ** (CART-TERM-1 - 1)) / CART-I.
           IF CART-METHOD = "A"
              COMPUTE CART-1STFAC ROUNDED =
                1 / ( (1 + CART-I * CART-F) * (CART-I1 ** CART-W) )
           ELSE
              COMPUTE CART-1STFAC ROUNDED =
                1 / ( (1 + CART-I * (CART-F + CART-W)) ).

           GO TO CART-CALC-FACTORS-EXIT.

       CART-CALC-FACTORS-END.
           MOVE "N" TO CART-ERRCD.

       CART-CALC-FACTORS-EXIT.
           EXIT.


      *****************************************************************
      *                           (ET)
      *         THIS ROUTINE CALCULATES THE REQUIRED BALLOON PAYMENT
      *         NECESSARY TO BOTH:
      *             - YIELD THE EFFECTIVE RATE
      *             - SPREAD THE AMOUNT FINANCED, FINANCE CHARGE
      *               AND REMAINING AMOUNT DUE ON THE LOAN, INTO
      *               EVEN PAYMENT AMOUNTS
      *
      *   NAME: LPCART
      *   DESC: CALCULATE BALLOON PAYMENT FOR ET USING EFFRATE
      *         AND THE REQUIRED TERM
      *   IN  : CART-FINANCED          AMOUNT FINANCED
      *         CART-PAYMENT
      *         CART-INTMETHOD         MAKES SENSE FOR MONTHLY ONLY
      *                                  360 BY DEFAULT,
      *                                    IF OTHER THAN 360, SET IT
      *         CART-RATE              INTEREST RATE TO CHARGE
      *         CART-UNITPER-CD        PAYMENT PERIOD CODE
      *                                   EX. 'M' = MONTHLY
      *         CART-UNITPER-FREQ      FREQUENCY OF PAYMENTS
      *                                   EX. '2' = EVERY 2 WEEKS
      *         CART-INTDATE           INTEREST START DATE
      *         CART-1STPYDATE         FIRST PAYMENT DATE
      *         CART-DISFRMLA          DISCLOSURE FORMULA
      *                                   A - ACTUARIAL
      *                                   C - US RULE
      *         CART-AMTDUE            REMAINING AMOUNT DUE ON LOAN
      *
      *   OUT : CART-BALLOON
      *         CART-TERM
      *   COPY:
      * REV :
      *  JTG 042597 CHANGED TO HANDLE UNIT PERIODS
      *****************************************************************
       CARTET SECTION.
           MOVE 0 TO CART-BALLOON.
           IF CART-PAYMENT = 0
                    OR CART-FINANCED = 0
                     OR CART-INTDATE = 0
                      OR CART-1STPYDATE = 0
              GO TO CARTET-EXIT.

           MOVE CART-PAYMENT TO CART-PMT.

           IF CART-AMTDUE NOT = 0
              IF CART-AMTDUE > CART-PMT
                 MOVE 0 TO CART-AMTDUE
              ELSE
                 MOVE CART-1STPYDATE TO NDTE-DATE
                 MOVE -1 TO NDTE-HOLD
                 PERFORM INCREMENT-MONTHS
                 MOVE NDTE-DATE TO CART-1STPYDATE.

           COMPUTE CART-TERM = CART-FINANCED / CART-PMT + 1.

           MOVE 0 TO CART-COUNT.

       CARTET-LOOP.
           IF CART-COUNT < 50
              ADD 1 TO CART-COUNT
              PERFORM CART-CALC-FACTORS
              IF CART-ERRCD NOT = " "
                 GO TO CARTET-EXIT
              ELSE
                 PERFORM CARTET-CALC-BALLOON
                 IF CART-BALLOON = 0
                    SUBTRACT 1 FROM CART-TERM
                 ELSE
                    IF CART-BALLOON < 0
                       IF CART-TERM = 1
                          ADD CART-PMT TO CART-BALLOON
                       ELSE
                          SUBTRACT 1 FROM CART-TERM
                          GO TO CARTET-LOOP
                    ELSE
                       IF CART-BALLOON > CART-PMT
                          COMPUTE CART-TERM = CART-TERM
                                   + ( CART-BALLOON / CART-PMT)
                          GO TO CARTET-LOOP.

       CARTET-EXIT.
           EXIT.

      ******************************************************
      *    CALCULATE BALLOON PAYMENT FOR ET
      ******************************************************
       CARTET-CALC-BALLOON SECTION.
           COMPUTE CART-BALLOON ROUNDED =
              (
                CART-I1 ** CART-TERM-1
              )
                 *
                   (
                     ( CART-FINANCED / CART-1STFAC )
                         - CART-PMT * CART-ANI-2
                          - CART-PMT + CART-AMTDUE
                   ).

       CARTET-CALC-BALLOON-EXIT.
           EXIT.

