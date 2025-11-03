      * COPYMEMBER: LIBLP/LPAPRZL
      *****************************************************************
      *    APR CALCULATION FOR CALCZL.C ONLY
      *           (DIRECT LOAN CALCULATOR)
      *
      *   NAME: LPAPRZL
      *   DESC: APPROXIMATE APR TO 4 DECIMALS ACCURACY
      *   IN  : APR-FINANCED (AMOUNT FINANCED)
      *         APR-NOODDPY   ZERO UNLESS 1STPYAMT DIFFERENT THAN REGPYAMT
      *         APR-1STPYAMT  ZERO UNLESS DIFFERENT FROM REGPYAMT
      *         APR-NOREGPY
      *         APR-REGPYAMT
      *         APR-INTDATE
      *         APR-1STPYDATE
      *         APR-PAYFREQ
      *         SP-DISFRMLA
      *         APR-SPAY-FG   THIS FLAG IS USED BY CALCZ3
      *                       WHEN ITS VALUE IS OTHER THAN " "
      *                       IT STOPS SINGLE PAY LOANS FROM
      *                       USING ALTERNATIVE 365/DAYS METHOD
      *   OUT : APR-APR       999.9999 DECIMAL APPROX APR
      *   COPY:
      * REV :
      *  BAH 010287 AS PER LORN
      *  JPN 020289 APR-ELAPSED-MONTHS
      *  JTG 022190 CORRECTED SET OF APR-FRACTERM WHEN ELAPSED-DAYS
      *             BECAME LESS THAN 0
      *  JTG 041690 ADDED SET OF APR-METHOD FOR U.S. RULE
      *  JTG 060590 ADDED ALTERNATIVE SINGLE PAY DISCLOSURE METHOD
      *                   SP-DISFRMLA-SPAY-FG
      *  JTG 061591 CHANGE ALTERNATIVE SINGLE PAY DISCLOSURE METHOD
      *             TO USE EXACT TIME (365) METHOD
      *  JTG 091391 CHANGE SINGLE PAY DISCLOSURE METHOD (PACESETTER)
      *  MJD 130222 REMOVED CALL TO "C" ROUTINE AND ADDED VARYANT COBOL
      *             APRL CODE.
      *****************************************************************
       APR-REGZ SECTION.
           MOVE 0 TO APR-APR.
           IF (APR-NOREGPY ZERO AND APR-NOODDPY ZERO) OR
              APR-PAYFREQ  = 0 OR
              APR-REGPYAMT = 0 OR
              APR-FINANCED = 0
              GO TO APR-REGZ-EXIT.

           IF SP-DISFRMLA = "A" OR "C"
              GO TO APR-CONTINUE.
           IF SP-DISFRMLA NOT = "B"
              GO TO APR-REGZ-EXIT.

      * IF STATE PARAMETER DISCLOSURE FORMULA = "B"
      * FORCE FIRST PAYMENT DATE TO BE 1 MONTH FROM
      * INTEREST START DATE AND SUBTRACT EXTENSION
      * CHARGES FROM ODD PAYMENT AMOUNT.

           MOVE APR-INTDATE TO NDTE-DATE.
           MOVE 15 TO NDTE-DD.
           MOVE NDTE-DATE TO APR-INTDATE.
           MOVE 1 TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE TO APR-1STPYDATE.
           MOVE APR-REGPYAMT TO APR-1STPYAMT.

       APR-CONTINUE.
      * TERM USED IN PRESENT VALUE COMPUTATIONS
           ADD APR-NOREGPY APR-NOODDPY GIVING APR-PVTERM.
           SUBTRACT 1 FROM APR-PVTERM.

           MOVE APR-INTDATE TO NUM-DATE.
           MOVE APR-1STPYDATE TO SYS-DATE.

      * USE EXACT TIME FOR SINGLE PAY LOANS:
           IF APR-PVTERM = 0
              IF APR-SPAY-FG = " "
                 PERFORM TIM365
                 IF ELAPSED-DAYS < 0
                    GO TO APR-REGZ-EXIT
                 ELSE
                    MOVE ELAPSED-DAYS TO APR-SPAY-DAYS
                    MOVE 1 TO APR-ELAPSED-MONTHS
                    MOVE 0 TO ELAPSED-DAYS
                    GO TO APR-CONTINUE-2.

      * FIND WHOLE UNIT PERIODS (PAYFREQ'S) BETWEEN INTDATE
      * AND 1STPYDATE (USED FOR IRREGULAR 1ST PERIODS)
      * RESULT APPEARS IN APR-ELAPSED-MONTHS
           MOVE 360 TO ELAPSED-YRTYPE.
           PERFORM TIM360.
           DIVIDE APR-PAYFREQ INTO ELAPSED-MONTHS GIVING
              APR-ELAPSED-MONTHS.
           IF APR-ELAPSED-MONTHS < 0
              GO TO APR-REGZ-EXIT.

      * FRACTIONAL UNIT PERIOD BETWEEN INTDATE AND 1STPYDATE
      * (NET OF WHOLE UNIT PERIODS) APR-FRACTERM CONTAINS RESULT
      * NOTE THAT THE DETERMINATION OF FRACTIONAL TERM USING THE
      * BEGINNING OF THE TERM (INSTEAD OF USING ELAPSED-REM) IS
      * INTENTIONAL - SEE REG Z FOR WHOLE MONTH PAYMENT PERIODS.
           MOVE APR-1STPYDATE TO NDTE-DATE.
           COMPUTE NDTE-HOLD = APR-ELAPSED-MONTHS * APR-PAYFREQ * -1.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE TO SYS-DATE.
           PERFORM TIM.

      *****************************************************************
      * THE FOLLOWING IF STATEMENT WAS PUT IN ON 1/4/88 TO CORRECT
      * A ROUNDING PROBLEM WITH APR-FRACTERM. APR-FRACTERM IS DEFINED
      * WITH A PICTURE OF V9(9). ANYTIME ELAPSED-DAYS WAS GREATER
      * THAN OR EQUAL TO THE APR-PAYFREQ * 30 (EG. 30 DAYS), THE
      * APR-FRACTERM WOULD BE TRUNCATED. THIS WOULD CAUSE THE APR TO
      * INCREASE DRAMATICALLY. A NOTE OF CAUTION: IN THE FUTURE
      * PROBLEMS WILL BE CAUSED IF THE APR-PAYFREQ IS ANYTHING SMALLER
      * THAN 1.  APR-FRACTERM WILL HAVE TO BE CHANGED TO CARRY DECIMAL
      * PLACES TO THE LEFT OF THE DECIMAL POINT IF THIS IS THE CASE.
      * DON'T FORGET TO ALSO CHANGE THE 'C' CODE AND RTS32 RUN TIME.
      *                                      L.H.O
      * P.S. THE APR-FRACTERM SIZE ERROR WAS ALSO INSTALLED ON THIS
      *      DATE.
      *
      *                                    12 MONTHS
      *      PAYFREQ (IN MONTHS) =  ----------------------
      *                              NO. OF PAYS PER YEAR
      *
      *                                    12 MONTHS
      *      NO. OF PAYS PER YEAR =  ----------------------
      *                              PAYFREQ (IN MONTHS)
      *****************************************************************
       APR-CONTINUE-2.
           IF ELAPSED-DAYS < 0
              MOVE ZERO TO APR-FRACTERM APR-REUBEN
           ELSE
              IF ELAPSED-DAYS < (APR-PAYFREQ * 30)
                 COMPUTE APR-REUBEN ROUNDED =
                    ELAPSED-DAYS / (APR-PAYFREQ * 30)
                           SIZE ERROR GO TO APR-REGZ-EXIT
              ELSE
                 MOVE ZERO TO APR-FRACTERM APR-REUBEN
                 ADD 1 APR-ELAPSED-MONTHS GIVING APR-ELAPSED-MONTHS.
           MOVE APR-REUBEN TO APR-FRACTERM.

      * SET UP BOTH THE FIRST AND ALL REGULAR PAYMENTS.
           IF APR-1STPYAMT ZERO
              MOVE APR-REGPYAMT TO APR-1ST-PAYMENT
           ELSE
              MOVE APR-1STPYAMT TO APR-1ST-PAYMENT.

      * SET UP APR-METHOD ACTUARIAL VS U.S. RULE:
      * NOTE:
      *  'B' IS DEFAULTING TO U.S. RULE SINCE IT DOESN'T
      *  REQUIRE A POWER RAISE ON WHOLE MONTHS IN
      *  FIRST PAY PERIOD.
      *
      *  ON SINGLE PAY LOANS ACTUARIAL IS DONE LIKE US RULE.

           IF SP-DISFRMLA = "A" AND APR-PVTERM NOT = 0
              MOVE "A" TO APR-METHOD
           ELSE
              MOVE "U" TO APR-METHOD.

      ******************************************************************
      * TAKE ESTIMATED APR IN APR-APR1 TO COMPUTE AMOUNT
      * FINANCED. REPEAT WITH ESTIMATE INCREMENTED BY APR-DELTA;
      * INTERPOLATE NEW APR WITH RESULTS. IF NEWLY
      * COMPUTED APR IS WITHIN .0001 TOLERANCE OF CURRENT
      * ESTIMATE, REGULATION Z APR IS NEW ESTIMATE.
      ******************************************************************
      *
      *  HERE IS THE CALL TO THE "C" PROGRAM. IT WORKS EXACTLY LIKE
      *  THE COBOL THAT IT REPLACES EXCEPT THAT IT USES FLOATING POINT
      *  NUMBERS. THE FLOATING POINT USAGE ALLOW IT TO CALCULATE RATES
      *  OF INTEREST UP TO AND INCLUDING 999.9999% AT 30 YEARS.
      *
      ******************************************************************
           MOVE "11" TO APR-STAT.
      *    CALL CMD USING APR-STAT APR-WORK-AREA.
           PERFORM APRL-MAIN.

           IF APR-PVTERM = 0
              IF APR-SPAY-FG = " "
                 COMPUTE APR-APR ROUNDED = APR-APR / 12
                                             * 365 / APR-SPAY-DAYS.

       APR-REGZ-EXIT.
           MOVE " " TO APR-SPAY-FG.

       APRL-MAIN SECTION.
           PERFORM APRL-GET-ARGS
           PERFORM APRL-PROCESS
           EXIT SECTION.
      *    GOBACK
      *    .

       APRL-GET-ARGS.
           MOVE APR-FINANCED       TO APRL-FINANCED
           MOVE APR-REGPYAMT       TO APRL-REGPYAMT
           MOVE APR-PAYFREQ        TO APRL-PAYFREQ
           MOVE APR-1ST-PAYMENT    TO APRL-FST-PAYMENT
           MOVE APR-PVTERM         TO APRL-PVTERM
           MOVE APR-ELAPSED-MONTHS TO APRL-ELAPSED-MONTHS
           MOVE APR-FRACTERM       TO APRL-FRACTERM
           MOVE APR-DELTA          TO APRL-DELTA
           MOVE APR-METHOD         TO APRL-METHOD
           .

       APRL-PROCESS.
           MOVE ZERO TO APRL-APRL
           MOVE 1.0 TO APRL-ADJ APRL-R
           PERFORM VARYING APRL-CNTR FROM 0 BY 1
                    UNTIL APRL-CNTR > 49 OR
                    FUNCTION ABS(APRL-ADJ) NOT > 0.0001

             MOVE APRL-R TO APRL-AF-R
             PERFORM APRL-FIN
             MOVE APRL-APRLW TO APRL-F0

             COMPUTE APRL-AF-R = APRL-R + APRL-DELTA
             PERFORM APRL-FIN
             MOVE APRL-APRLW TO APRL-F1

             COMPUTE APRL-ADJ = APRL-DELTA * (APRL-FINANCED - APRL-F0)
                                           / (APRL-F1 - APRL-F0)
             COMPUTE APRL-R = APRL-R + APRL-ADJ

           END-PERFORM
           IF APRL-R < 0 OR APRL-CNTR > 50
              MOVE 0 TO APRL-APRL
           ELSE
             IF APRL-R > 999.9999
                MOVE 999.9999 TO APRL-APRL
             ELSE
                MOVE APRL-R TO APRL-APRL
             END-IF
           END-IF
           COMPUTE APRL-APRL =
                          FUNCTION INTEGER ((APRL-APRL * 10000) + 0.5)
           COMPUTE APR-APR = APRL-APRL / 10000
           .

       APRL-FIN.
           COMPUTE APRL-I = APRL-AF-R * APRL-PAYFREQ / 1200
           COMPUTE APRL-I1 = 1.0 + APRL-I
           IF APRL-I NOT = 0
              COMPUTE APRL-ANN = (1 - (APRL-I1 ** (-APRL-PVTERM)) )
                                   / APRL-I
           ELSE
              MOVE APRL-PVTERM TO APRL-ANN
           END-IF
           IF APRL-METHOD = "U" OR APRL-PVTERM = 0
              COMPUTE APRL-DIV-BY = 1.0 + (
                                  (APRL-FRACTERM + APRL-ELAPSED-MONTHS)
                                  * APRL-I)
           ELSE
              COMPUTE APRL-DIV-BY = (1.0 + ( APRL-FRACTERM * APRL-I ) )
                                   * ( APRL-I1 ** APRL-ELAPSED-MONTHS )
           END-IF

           COMPUTE APRL-APRLW = ( APRL-FST-PAYMENT +
                                (APRL-REGPYAMT * APRL-ANN) )
                                       / APRL-DIV-BY
           .

