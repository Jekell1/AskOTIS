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
       APR-REGZ SECTION.
           MOVE 0 TO APR-APR.
           IF (APR-ORGTERM = 0)
            OR (APR-UNITPER-FREQ = 0)
             OR (NOT
                 (APR-UNITPER-CD = "M" OR "S" OR "W"
                                        OR "D" OR "Y" OR "B")
                )
              OR (APR-1STPYAMT = 0)
               OR (APR-FINANCED = 0)
                OR (NOT (APR-DISFRMLA = "A" OR "B" OR "C" OR "X") )
                 OR (APR-LASTPYAMT NOT NUMERIC)
                    GO TO APR-REGZ-EXIT.

      * TERM USED IN PRESENT VALUE COMPUTATIONS:
           MOVE APR-ORGTERM TO APR-PVTERM.

      * SETUP UNIT PERIOD INFORMATION:
           MOVE APR-PVTERM TO UPER-TERM.
           MOVE APR-UNITPER-CD TO UPER-UNITPER-CD.
           MOVE APR-UNITPER-FREQ TO UPER-UNITPER-FREQ.
           MOVE APR-INTDATE TO UPER-INTDATE.
           MOVE APR-1STPYDATE TO UPER-1STPYDATE.
           PERFORM UPER-CALCULATION.
           IF UPER-ERRCD NOT = " "
              GO TO APR-REGZ-EXIT.
           MOVE UPER-UNITPER-PER-YEAR TO APR-UNITPER-PER-YEAR.
           MOVE UPER-FULL-UNITPER TO APR-FULL-UNITPER.
           MOVE UPER-FRACT-UNITPER TO APR-FRACT-UNITPER.

      *--------------------------------------------------------------
      *    IF SINGLE PAY LOAN,
      *       CLEAR APR-LASTPYAMT TO ZERO
      *--------------------------------------------------------------
           IF APR-PVTERM = 1
              MOVE 0 TO APR-LASTPYAMT
      *--------------------------------------------------------------
      *    IF NOT SINGLE PAY LOAN,
      *       IF DISCLOSURE FORMULA = "B"
      *          - FORCE FIRST PAYMENT DATE TO BE 1 MONTH FROM
      *            INTEREST START DATE
      *          - REMOVE EXTENSION CHARGES FROM 1ST PAYMENT AMOUNT
      *          - MAKE LAST PAYMENT = REGULAR PAYMENT AMOUNT
      *       IF DISCLOSURE FORMULA = "X"
      *          - FORCE FIRST PAYMENT DATE TO BE 1 MONTH FROM
      *            INTEREST START DATE
      *          - REMOVE EXTENSION CHARGES FROM 1ST PAYMENT AMOUNT
      *          - KEEP ODD LAST PAYMENT
      *--------------------------------------------------------------
           ELSE
              IF APR-DISFRMLA = "B" OR "X"
                 MOVE 1 TO APR-FULL-UNITPER
                 MOVE 0 TO APR-FRACT-UNITPER
                 IF APR-PVTERM = 2
                    MOVE APR-LASTPYAMT TO APR-1STPYAMT
                 ELSE
                    MOVE APR-REGPYAMT TO APR-1STPYAMT
                    IF APR-DISFRMLA = "B"
                       MOVE APR-REGPYAMT TO APR-LASTPYAMT.

      * SET UP THE FIRST PAYMENT
           MOVE APR-1STPYAMT TO APR-1ST-PAYMENT.

      *--------------------------------------------------------
      * SET UP APR-METHOD ACTUARIAL VS U.S. RULE:
      * NOTE:
      *      'B' AND 'X' ARE DEFAULTING TO U.S. RULE,
      *       SINCE IT DOESN'T REQUIRE A POWER RAISE ON
      *       WHOLE MONTHS IN FIRST PAY PERIOD.
      *--------------------------------------------------------
      *    IF APR-DISFRMLA = "A" OR APR-PVTERM = 1  SINGLE PAY TEST JTG
           IF APR-DISFRMLA = "A"
              MOVE "A" TO APR-METHOD
           ELSE
              MOVE "U" TO APR-METHOD.

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
           PERFORM APRZ-APR-MAIN.

       APR-REGZ-EXIT.
           EXIT.

