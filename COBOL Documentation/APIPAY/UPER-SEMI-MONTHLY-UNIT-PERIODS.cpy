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
