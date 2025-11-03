      *****************************************************************
      *          INCREMENT A DATE BY A NUMBER OF UNIT PERIODS
      *
      *   DESC:  INCREMENT NDTE-DATE BY NDTE-HOLD UNIT PERIODS;
      *          CORRECT DAY AND MONTH IF REQUIRED.
      *   IN  :  DATER-UNITPER-CD   = M, D, Y, S, W, B
      *          DATER-UNITPER-FREQ = FREQUENCY OF UNIT PERIODS
      *          NDTE-DATE          = MMDDYY TO INCREMENT
      *          NDTE-HOLD          = INCREMENT IN UNIT PERIODS (+ OR -)
      *   OUT :  NDTE-DATE = INCREMENTED DATE
      *****************************************************************
       C-INCREMENT-PERIODS SECTION.
           MOVE NDTE-MM TO WS-NDTE-MM-S.
           MOVE NDTE-DD TO WS-NDTE-DD-S.
           MOVE NDTE-CCYY TO WS-NDTE-CCYY-S.
           COMPUTE WS-UP-WORK = NDTE-HOLD * DATER-UNITPER-FREQ.

           EVALUATE DATER-UNITPER-CD
           WHEN "M"
                    PERFORM C-INCR-MONTHS
           WHEN "S"
                    PERFORM C-INCR-HALF-MONTHS
           WHEN "W"
                    COMPUTE WS-UP-WORK = WS-UP-WORK * 7
                    PERFORM C-INCR-DAYS
           WHEN "B"
                    COMPUTE WS-UP-WORK = WS-UP-WORK * 14
                    PERFORM C-INCR-DAYS
           WHEN "D"
                    PERFORM C-INCR-DAYS
           WHEN "Y"
                    COMPUTE WS-UP-WORK = WS-UP-WORK * 12
                    PERFORM C-INCR-MONTHS
           END-EVALUATE.

           MOVE WS-NDTE-MM-S TO NDTE-MM.
           MOVE WS-NDTE-DD-S TO NDTE-DD.
           MOVE WS-NDTE-CCYY-S TO NDTE-CCYY.
           MOVE "N" TO DATER-ACTION-CODE.
           PERFORM C-CALL-NEWDATE.

      *********************************************************
      *          INCREMENT DAYS TO A DATE
      *********************************************************
