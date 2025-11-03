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
       UPER-CALCULATION SECTION.
           MOVE " " TO UPER-ERRCD.
           MOVE 0 TO UPER-UNITPER-PER-YEAR
                     UPER-DAYS-IN-UNITPER
                     UPER-FULL-UNITPER
                     UPER-FRACT-UNITPER.
           IF (UPER-UNITPER-FREQ = 0 AND UPER-TERM NOT = 1)
            OR (NOT
                 (UPER-UNITPER-CD = "M" OR "S" OR "W"
                                          OR "D" OR "Y" OR "B")
               )
             OR UPER-TERM = 0
                MOVE "E" TO UPER-ERRCD
                GO TO UPER-CALCULATE-EXIT.

           IF UPER-TERM = 1
              PERFORM UPER-SINGLE-PAYS
           ELSE
           IF UPER-UNITPER-CD = "M"
              COMPUTE UPER-UNITPER-PER-YEAR = 12 / UPER-UNITPER-FREQ
              COMPUTE UPER-DAYS-IN-UNITPER = 30 * UPER-UNITPER-FREQ
              PERFORM UPER-MONTHLY-UNIT-PERIODS
           ELSE
           IF UPER-UNITPER-CD = "S"
              COMPUTE UPER-UNITPER-PER-YEAR = 24 / UPER-UNITPER-FREQ
              COMPUTE UPER-DAYS-IN-UNITPER = 15 * UPER-UNITPER-FREQ
              PERFORM UPER-SEMI-MONTHLY-UNIT-PERIODS
           ELSE
           IF UPER-UNITPER-CD = "W"
              COMPUTE UPER-UNITPER-PER-YEAR = 52 / UPER-UNITPER-FREQ
              COMPUTE UPER-DAYS-IN-UNITPER = 7 * UPER-UNITPER-FREQ
              PERFORM UPER-WEEKLY-UNIT-PERIODS
           ELSE
           IF UPER-UNITPER-CD = "B"
              COMPUTE UPER-UNITPER-PER-YEAR = 26 / UPER-UNITPER-FREQ
              COMPUTE UPER-DAYS-IN-UNITPER = 14 * UPER-UNITPER-FREQ
              PERFORM UPER-WEEKLY-UNIT-PERIODS
           ELSE
           IF UPER-UNITPER-CD = "D"
              COMPUTE UPER-UNITPER-PER-YEAR = 365 / UPER-UNITPER-FREQ
              COMPUTE UPER-DAYS-IN-UNITPER = 1 * UPER-UNITPER-FREQ
              PERFORM UPER-DAILY-UNIT-PERIODS
           ELSE
           IF UPER-UNITPER-CD = "Y"
              COMPUTE UPER-UNITPER-PER-YEAR = 1 / UPER-UNITPER-FREQ
              COMPUTE UPER-DAYS-IN-UNITPER = 365 * UPER-UNITPER-FREQ
              PERFORM UPER-YEARLY-UNIT-PERIODS.

       UPER-CALCULATE-EXIT.
           EXIT.

      ***********************************************
      *    SINGLE PAY LOANS
      ***********************************************
