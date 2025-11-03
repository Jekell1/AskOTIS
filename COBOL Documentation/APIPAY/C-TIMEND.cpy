      **********************************************************
      *          END ROUTINES FOR ALL ELAPSED TIME ROUTINE
      *
      *   NAME:  C-TIMEND
      *   DESC:  THIS ROUTINE SETS THE CORRECT SIGN ON ELAPSED
      *   IN  :
      *   OUT :
      * REV:
      *  JTG 050297 CHANGED FOR 998 ELAPSED UNIT PERIODS
      *********************************************************/
       C-TIMEND SECTION.
           COMPUTE WS-XDAYS = WS-XSIGN * WS-XDAYS.
           IF TA-YRTYPE = 998
              COMPUTE WS-XMONTHS = WS-XMONTHS * WS-XSIGN
              COMPUTE WS-XREM    = WS-XDAYS - (WS-XMONTHS * 30)
           ELSE
              DIVIDE WS-XDAYS BY 30 GIVING WS-XMONTHS
                                 REMAINDER WS-XREM.

           IF TA-YRTYPE = 365
              COMPUTE WS-XDAYS = WS-XSIGN * (WS-JUL-2 - WS-JUL-1).

           IF (WS-XSIGN < 0)
              MOVE WS-DATE2    TO WS-HOLDDATE
              MOVE WS-DATE1    TO WS-DATE2
              MOVE WS-HOLDDATE TO WS-DATE1
              MOVE WS-JUL-2    TO WS-HOLDJUL
              MOVE WS-JUL-1    TO WS-JUL-2
              MOVE WS-HOLDJUL  TO WS-JUL-1.

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

