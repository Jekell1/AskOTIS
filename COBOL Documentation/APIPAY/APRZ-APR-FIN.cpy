       APRZ-APR-FIN.
           MOVE ZERO TO APRZ-WS-APRW
           MOVE ZERO TO APRZ-WS-I APRZ-WS-I1
                        APRZ-WS-REG-ANN APRZ-WS-LAST-FAC

           COMPUTE APRZ-WS-I  = APRZ-WS-AF-R /
                                  ( APRZ-WS-UNITPER-PER-YEAR * 100)
           COMPUTE APRZ-WS-I1 = 1 + APRZ-WS-I
           IF APRZ-WS-I NOT = ZERO
              COMPUTE APRZ-WS-REG-ANN = (1 -
                                  (APRZ-WS-I1 ** (-APRZ-WS-REG-TERM)) )
                                   / APRZ-WS-I
           ELSE
              MOVE APRZ-WS-REG-TERM TO APRZ-WS-REG-ANN
           END-IF
           IF APRZ-WS-LAST-TERM NOT = ZERO
              COMPUTE APRZ-WS-LAST-FAC = APRZ-WS-I1 ** APRZ-WS-LAST-TERM
           ELSE
              MOVE 1 TO APRZ-WS-LAST-FAC
           END-IF

           IF APRZ-WS-METHOD = "U"
              COMPUTE APRZ-WS-APRW-DIV = 1 +
                      ( (APRZ-WS-FRACT-UNITPER + APRZ-WS-FULL-UNITPER )
                         * APRZ-WS-I )
           ELSE
              COMPUTE APRZ-WS-APRW-DIV = ( 1 +
                      ( APRZ-WS-FRACT-UNITPER * APRZ-WS-I ) ) *
                      ( APRZ-WS-I1 ** APRZ-WS-FULL-UNITPER )
           END-IF
           COMPUTE APRZ-WS-APRW = ( APRZ-WS-FSTPYAMT +
                                (APRZ-WS-REGPYAMT * APRZ-WS-REG-ANN)
                             + (APRZ-WS-LASTPYAMT / APRZ-WS-LAST-FAC) )
                             / APRZ-WS-APRW-DIV
           .

      *================================================================*
      * END COPYBOOK: LIBLP\LPAPRZ.CPY                                *
      *================================================================*
      *================================================================*
      * EMBEDDED COPYBOOK: LIBLP\LPUPER.CPY                           *
      *================================================================*
      * COPYMEMBER: LIBLP/LPUPER

      **************************************************************************
      *         CALCULATE INFORMATION ON UNIT PERIODS
      *         FEDERAL RESERVE REGULATION Z METHOD
      *
      *   NAME: LPUPER
      *   DESC: DETERMINE NO. OF UNIT PERIODS PER YEAR
      *                   NO. OF DAYS IN THE UNIT PERIOD
      *                   NO. OF FULL AND FRACTIONAL UNIT PERIODS
      *                       BETWEEN INTDATE AND 1STPYDATE
      *   IN  : UPER-UNITPER-CD
      *         UPER-UNITPER-FREQ
      *         UPER-INTDATE
      *         UPER-1STPYDATE
      *         UPER-TERM
      *
      *   OUT : UPER-UNITPER-PER-YEAR
      *         UPER-DAYS-IN-UNITPER
      *         UPER-FULL-UNITPER              EX. 1   MONTHS
      *         UPER-FRACT-UNITPER                 0.5 MONTHS
      *         UPER-ERRCD
      *
      *         UPER-SINGLE-PAY-CD
      *         UPER-SINGLE-PAY-FREQ
      *
      *         NOTE:
      *         ----> FOR SINGLE PAY LOANS, UPER-SINGLE-PAY-CD AND
      *               UPER-SINGLE-PAY-FREQ (MUST)(MUST) BE USED AT
      *               LOAN CREATE TO OVERRIDE THE NORMAL UNITPER-CD
      *               AND UNITPER-FREQ.
      *
      *               THIS WILL ALLOW FOR DATE ROUTINES TO YIELD
      *               CORRECT ELAPSED UNIT PERIODS AND/OR INCREMENT
      *               UNIT PERIODS TO INCREMENT CORRECTLY.
      *   COPY:
      * REV :
      *  JTG 042397 CORRECTED LOGIC FOR SINGLE PAY > 12 MONTHS AND
      *             REM NOT = 0, WAS MULTIPLYING BY 30 INSTEAD OF 360
      *  JTG 991214 PERFORMED TIMUPER, REGMGM
      *             TESTED ELAPSED-UNITPER-REM FOR SINGLE PAY LOANS WHEN
      *             TERM IS LESS THAN A FULL YEAR
      *             RE: 120199 TO 123199 WHICH YIELDS 1 MONTH 0 REM
      *                 AND 0 ELAPSED-UNITPER 30 ELAPSED-UNITPER-REM
      *  JTG 991217 FOR SINGLE PAY LOANS WITH A TERM IS LESS THAN A FULL
      *             YEAR SET REGZ OPTION TO ALWAYS USE CALCULATION OF
      *             NO OF UNIT PERIODS PER YEAR = 365 / DAYS IN TERM
      *  BAH 000105 CHANGED TEST ON FREQ = 0 TO ALSO TEST TERM NOT = 1,
      *             BECAUSE OF THE FOLLOWING SITUATION AT FUTURE, VA
      *             LOANDATE = 12/22/99   TERM = 1  WHEN CURSER STOPS AT
      *             1STPYDATE OF 01/22/00, UP ARROW AND CHANGE INTEREST START
      *             TO 01/22/00, NOW ELASPED DAYS IS 0, DEFAULTS SINGLE PAY
      *             TO "D" AND 0, THEN THEY CHANGE 1STPYDATE TO 01/30/00, AND
      *             IT WOULD NOT RECALC BECAUSE OF THE FREQ = 0 "IF"..
      *  JTG 060504 CHANGED LOGIC FOR SINGLE PAYS LOANS > 11 MONTHS
      *             TO YIELD REMAINING DAYS FROM UPER-INTDATE PLUS
      *             FULL UNIT PERIODS TO UPER-1STPYDATE
      *             ADDED NEW LOAN CALCULATION FOR RICHARD        LENDMARK #2709
      **************************************************************************
