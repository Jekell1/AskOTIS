      * CALIFORNIA "CA" STATE LAW  1/15/2013 MULLEN & COSMO
      * WHEN AN ACCOUNT IS PAID IN FULL (REGARDLESS OF THE PAYOFF METHOD)
      * ON OR BEFORE THE 3RD INSTALLMENT DUE DATE (CURRENT 1ST PAY DUE DATE
      * PLUS 2 MONTHS) BASED ON A 360 DAY YEAR TYPE, AS FOLLOWS:
      ********************************************
       CALIFORNIA-SPECIAL-OPTION SECTION.
      ********************************************

           MOVE LN-ORIG-1STPYDATE TO NDTE-DATE
           MOVE 2 TO NDTE-HOLD
           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ
           PERFORM INCREMENT-UNITPER
      * IF NOT WITHIN 3RD INSTALLMENT, EXIT OUT
           IF POFF-PAYDATE > NDTE-DATE
              GO TO EXIT-CALIFORNIA-SPECIAL-OPTION.

      * WITHIN 3RD INSTALLMENT, NOW CALCULATE ELAPSED DAYS SINCE LAST
      * PAYMENT/LOANDATE (IF NO PAYMENTS)

           MOVE 360 TO ELAPSED-YRTYPE.
           MOVE LN-DATE-PAID-LAST TO NUM-DATE.
           IF LN-DATE-PAID-LAST = 0
              MOVE LN-LOANDATE TO NUM-DATE.
           MOVE POFF-PAYDATE TO SYS-DATE.
           PERFORM TIMALL.

           IF LN-CURBAL > 0
      * 1 TO 225.00
              IF LN-CURBAL < 225.01
                 COMPUTE INDU-INTEREST ROUNDED =
                  ( LN-CURBAL * ELAPSED-DAYS ) / 1200
              ELSE
      * 225.01 TO 900.00
              IF LN-CURBAL < 900.01
                 COMPUTE INDU-INTEREST ROUNDED =
                  ( (56.25 + LN-CURBAL) * ELAPSED-DAYS ) / 1500
              ELSE
      * 900.01 TO 1650.00
              IF LN-CURBAL < 1650.01
                 COMPUTE INDU-INTEREST ROUNDED =
                  ( (375.00 + LN-CURBAL) * ELAPSED-DAYS ) / 2000
              ELSE
      * 1650.01 TO 2499.99
                 COMPUTE INDU-INTEREST ROUNDED =
                  ( (1387.50 + LN-CURBAL) * ELAPSED-DAYS ) / 3000.

       EXIT-CALIFORNIA-SPECIAL-OPTION.
           EXIT.

      *********************************************
      *   ROUTINE TO CALL SUBPROGRAM:
      *
      *   MOVE FIELDS TO PASS TO POFF-COMMON
      *   MOVE PROGRAM NAME TO POFF-FORM-PROG
      *********************************************
      *POFF-CALL-SUBPROG SECTION.
      *    MOVE POFF-FORM-PATH TO FORM-NAM.

      *POFF-CALL-SUBPROG-RETRY.
      *    MOVE POFF-PAYDATE TO POFF-COMMON-PAYDATE.
      *    CALL FORM-PROGX USING FORM-PATH POFF-COMMON LN-REC SP-REC
      *         ON OVERFLOW
      *         GO TO POFF-CALL-SUBPROG-RETRY.

      *    CANCEL FORM-PROGX.

       COPY "LIBLP/MIPPO.CPY".

      *-----------------------------------------------------------------------
      *    DUMMY SECTION TO INCLUDE IN COPY MEMBERS THAT NEED TO BE
      *    REFLECTED IN 'WHAT' COMMANDS FOR SUPPORT
      *-----------------------------------------------------------------------
