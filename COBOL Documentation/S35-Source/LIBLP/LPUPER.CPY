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
       UPER-SINGLE-PAYS SECTION.
           MOVE UPER-INTDATE   TO NUM-DATE.
           MOVE UPER-1STPYDATE TO SYS-DATE.
           MOVE 999            TO ELAPSED-YRTYPE.
           PERFORM TIMALL.
           IF ELAPSED-MONTHS < 0
              MOVE "E" TO UPER-ERRCD
              GO TO UPER-SINGLE-PAYS-EXIT.

      * ELAPSED TIME GREATER THAN ONE YEAR:
           IF ELAPSED-MONTHS > 11
              MOVE 1   TO UPER-UNITPER-PER-YEAR
              MOVE 365 TO UPER-DAYS-IN-UNITPER
              MOVE "D" TO UPER-SINGLE-PAY-CD
              MOVE 365 TO UPER-SINGLE-PAY-FREQ
              COMPUTE UPER-FULL-UNITPER = ELAPSED-MONTHS / 12
              MOVE UPER-INTDATE   TO NDTE-DATE
              COMPUTE NDTE-HOLD = UPER-FULL-UNITPER * 12
              PERFORM INCREMENT-MONTHS
              MOVE NDTE-DATE      TO NUM-DATE
              MOVE UPER-1STPYDATE TO SYS-DATE
              PERFORM TIM365
              COMPUTE UPER-FRACT-UNITPER = ELAPSED-DAYS / 365
           ELSE
      * ELAPSED TIME LESS THAN ONE YEAR:
      * JTG 991217 FOR SINGLE PAY LOANS WITH A TERM IS LESS THAN A FULL
      *            YEAR SET REGZ OPTION TO ALWAYS USE CALCULATION OF
      *            NO OF UNIT PERIODS PER YEAR = 365 / DAYS IN TERM
              MOVE 1 TO UPER-FULL-UNITPER
              MOVE 0 TO UPER-FRACT-UNITPER
      *  JTG 991214 PERFORMED TIMUPER, REGMGM
      *             TESTED ELAPSED-UNITPER-REM FOR SINGLE PAY LOANS WHEN
      *             TERM IS LESS THAN A FULL YEAR
      *             RE: 120199 TO 123199 WHICH YIELDS 1 MONTH 0 REM
      *                 AND 0 ELAPSED-UNITPER 30 ELAPSED-UNITPER-REM
      *       PERFORM TIMUPER
      *       IF ELAPSED-UNITPER-REM = 0
      *          MOVE "M" TO UPER-SINGLE-PAY-CD
      *          MOVE ELAPSED-MONTHS TO UPER-SINGLE-PAY-FREQ
      *          COMPUTE UPER-UNITPER-PER-YEAR = 12 / ELAPSED-MONTHS
      *          COMPUTE UPER-DAYS-IN-UNITPER = 30 * ELAPSED-MONTHS
      *       ELSE
                 MOVE UPER-INTDATE   TO NUM-DATE
                 MOVE UPER-1STPYDATE TO SYS-DATE
                 PERFORM TIM365
                 MOVE "D"            TO UPER-SINGLE-PAY-CD
                 MOVE ELAPSED-DAYS   TO UPER-SINGLE-PAY-FREQ
                 COMPUTE UPER-UNITPER-PER-YEAR = 365 / ELAPSED-DAYS
                 COMPUTE UPER-DAYS-IN-UNITPER = ELAPSED-DAYS.

       UPER-SINGLE-PAYS-EXIT.
           EXIT.

      ***********************************************
      *    MONTHLY UNIT PERIODS
      ***********************************************
       UPER-MONTHLY-UNIT-PERIODS SECTION.
           MOVE UPER-INTDATE   TO NUM-DATE.
           MOVE UPER-1STPYDATE TO SYS-DATE.
           PERFORM TIM360.
           IF ELAPSED-MONTHS < 0
              MOVE "E" TO UPER-ERRCD
              GO TO UPER-MONTHLY-UNIT-PERIODS-EXIT.
           COMPUTE UPER-FULL-UNITPER =
                     ELAPSED-MONTHS / UPER-UNITPER-FREQ.

           MOVE UPER-1STPYDATE TO NDTE-DATE.
           COMPUTE NDTE-HOLD =
                     UPER-FULL-UNITPER * UPER-UNITPER-FREQ * -1.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE      TO SYS-DATE.
           PERFORM TIM365.
           MOVE ZERO TO UPER-REUBEN.
           IF ELAPSED-DAYS > 0
              IF ELAPSED-DAYS < UPER-DAYS-IN-UNITPER
                 COMPUTE UPER-REUBEN ROUNDED =
                          ELAPSED-DAYS / UPER-DAYS-IN-UNITPER
              ELSE
                 ADD 1 TO UPER-FULL-UNITPER.
           MOVE UPER-REUBEN TO UPER-FRACT-UNITPER.

       UPER-MONTHLY-UNIT-PERIODS-EXIT.
           EXIT.

      ***********************************************
      *    SEMI MONTHLY UNIT PERIODS
      ***********************************************
       UPER-SEMI-MONTHLY-UNIT-PERIODS SECTION.
           MOVE UPER-INTDATE   TO NUM-DATE.
           MOVE UPER-1STPYDATE TO SYS-DATE.
           PERFORM TIM360.
           IF ELAPSED-DAYS < 0
              MOVE "E" TO UPER-ERRCD
           ELSE
              COMPUTE UPER-FULL-UNITPER =
                          ELAPSED-MONTHS * 30 + ELAPSED-REM
              COMPUTE UPER-FRACT-UNITPER ROUNDED =
                          UPER-FULL-UNITPER / UPER-DAYS-IN-UNITPER
              COMPUTE UPER-FULL-UNITPER =
                          UPER-FULL-UNITPER / UPER-DAYS-IN-UNITPER.

      ***********************************************
      *    WEEKLY UNIT PERIODS
      ***********************************************
       UPER-WEEKLY-UNIT-PERIODS SECTION.
           MOVE UPER-INTDATE   TO NUM-DATE.
           MOVE UPER-1STPYDATE TO SYS-DATE.
           PERFORM TIM365.
           IF ELAPSED-DAYS < 0
              MOVE "E" TO UPER-ERRCD
           ELSE
              COMPUTE UPER-FRACT-UNITPER ROUNDED =
                          ELAPSED-DAYS / UPER-DAYS-IN-UNITPER
              COMPUTE UPER-FULL-UNITPER =
                          ELAPSED-DAYS / UPER-DAYS-IN-UNITPER.

      ***********************************************
      *    DAILY UNIT PERIODS
      ***********************************************
       UPER-DAILY-UNIT-PERIODS SECTION.
           MOVE UPER-INTDATE   TO NUM-DATE.
           MOVE UPER-1STPYDATE TO SYS-DATE.
           PERFORM TIM365.
           IF ELAPSED-DAYS < 0
              MOVE "E" TO UPER-ERRCD
           ELSE
              COMPUTE UPER-FRACT-UNITPER ROUNDED =
                          ELAPSED-DAYS / UPER-DAYS-IN-UNITPER
              COMPUTE UPER-FULL-UNITPER =
                          ELAPSED-DAYS / UPER-DAYS-IN-UNITPER.

      ***********************************************
      *    YEARLY UNIT PERIODS
      ***********************************************
       UPER-YEARLY-UNIT-PERIODS SECTION.
           MOVE UPER-INTDATE   TO NUM-DATE.
           MOVE UPER-1STPYDATE TO SYS-DATE.
           MOVE 999 TO ELAPSED-YRTYPE.
           PERFORM TIMALL.
           IF ELAPSED-DAYS < 0
              MOVE "E" TO UPER-ERRCD
              GO TO UPER-YEARLY-UNIT-PERIODS-EXIT.

           IF ELAPSED-REM = 0
              COMPUTE UPER-FRACT-UNITPER ROUNDED =
                          ELAPSED-MONTHS / (12 * UPER-UNITPER-FREQ)
              COMPUTE UPER-FULL-UNITPER =
                          ELAPSED-MONTHS / (12 * UPER-UNITPER-FREQ)
              GO TO UPER-YEARLY-UNIT-PERIODS-EXIT.

           IF ELAPSED-REM NOT = 0
              COMPUTE UPER-FULL-UNITPER =
                          ELAPSED-MONTHS / (12 * UPER-UNITPER-FREQ).
           MOVE UPER-1STPYDATE TO NDTE-DATE.
           COMPUTE NDTE-HOLD = UPER-FULL-UNITPER
                                          * UPER-UNITPER-FREQ * -12.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE TO SYS-DATE.
           PERFORM TIM365.
           MOVE 0 TO UPER-FRACT-UNITPER.
           IF ELAPSED-DAYS > 0
              IF ELAPSED-DAYS < UPER-DAYS-IN-UNITPER
                 COMPUTE UPER-FRACT-UNITPER ROUNDED =
                                    ELAPSED-DAYS / UPER-DAYS-IN-UNITPER
              ELSE
                 ADD 1 TO UPER-FULL-UNITPER.

       UPER-YEARLY-UNIT-PERIODS-EXIT.
           EXIT.
