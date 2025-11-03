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
