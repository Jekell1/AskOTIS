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
