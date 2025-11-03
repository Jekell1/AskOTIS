      *********************************************************
      *          INCREMENT DATES BY UNITS OF 1/2 MONTHS
      *          USES C-INCR-MONTHS BASE ROUTINE FOR IT
      *********************************************************
       C-INCR-HALF-MONTHS SECTION.
           MOVE WS-NDTE-DD-S TO WS-HOLD-DD.
           DIVIDE WS-UP-WORK BY 2 GIVING    WS-DIV-RESULT
                                  REMAINDER WS-MOD-2.
           MOVE WS-DIV-RESULT TO WS-UP-WORK.
           IF WS-UP-WORK NOT = 0
              PERFORM C-INCR-MONTHS.

           IF WS-MOD-2 > 0
              IF WS-NDTE-DD-S > 15
                 COMPUTE WS-NDTE-DD-S = WS-HOLD-DD - 15
                 MOVE 1 TO WS-UP-WORK
                 PERFORM C-INCR-MONTHS
              ELSE
                 ADD 15 TO WS-NDTE-DD-S
              END-IF
           ELSE
              IF WS-MOD-2 < 0
                 IF WS-NDTE-DD-S < 16
                    ADD 15 TO WS-NDTE-DD-S
                    MOVE -1 TO WS-UP-WORK
                    PERFORM C-INCR-MONTHS
                 ELSE
                    COMPUTE WS-NDTE-DD-S = WS-HOLD-DD - 15.

      *********************************************************
      *          INCREMENT MONTH ROUTINE
      *
      *          INCREMENTS A DATE BY THE NO.
      *          OF MONTHS FOUND IN INCREMENT.
      *          THE CORRECT LAST DAY OF MONTH IS ADJUSTED.
      *********************************************************
