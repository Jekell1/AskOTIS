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
