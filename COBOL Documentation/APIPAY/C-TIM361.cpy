      ******************************************************************
      *         COMPUTE ELAPSED TIME USING 361 YRTYPE
      *   NOTE:
      *         THIS IS A VERSION OF 358 YEAR-TYPE MODIFY FOR NOTES.
      *         IT CAUSES TIME FROM FEB. 28 TO THE (29TH, 30TH OR 31ST)
      *         OF ANOTHER MONTH, TO VIEW FEB. 28 AS THE 30TH.
      *
      *   NAME: C-TIM361
      *   DESC: COMPUTE ELAPSED TIME BETWEEN DATE1 AND DATE2
      *
      *   IN  :
      *   OUT : ELAPSED_MONTHS, ELAPSED_DAYS AND ELAPSED_REM
      ******************************************************************
       C-TIM361 SECTION.
           PERFORM C-TIMBEG.
           IF WS-DATE1-DD > 30
              MOVE 30 TO WS-DATE1-DD.
           IF WS-DATE2-DD > 30
              MOVE 30 TO WS-DATE2-DD.
           IF WS-DATE2-MM = 2 AND WS-DATE2-DD > WS-DATE1-DD
              MOVE WS-DATE2-CCYY TO LEAP-YEAR-CCYY
              PERFORM C-LEAP-YEAR-TEST
              IF LEAP-YEAR-TRUE AND WS-DATE2-DD = 29
                 MOVE 30 TO WS-DATE2-DD
              ELSE
                IF LEAP-YEAR-FALSE AND WS-DATE2-DD = 28
                   MOVE 30 TO WS-DATE2-DD.

           IF WS-DATE2-MM = 2 AND WS-DATE2-DD < WS-DATE1-DD
              MOVE WS-DATE2-CCYY TO LEAP-YEAR-CCYY
              PERFORM C-LEAP-YEAR-TEST
              IF LEAP-YEAR-TRUE AND WS-DATE2-DD = 29
                 MOVE 29 TO WS-DATE1-DD
              ELSE
                IF LEAP-YEAR-FALSE AND WS-DATE2-DD = 28
                   MOVE 28 TO WS-DATE1-DD.

           IF NOT (
              WS-DATE1-MM NOT = 2 OR
              (WS-DATE1-DD < 28 AND WS-DATE1-DD NOT > WS-DATE2-DD) OR
              (WS-DATE1-DD = WS-DATE2-DD AND WS-DATE1-MM = WS-DATE2-MM
               AND WS-DATE1-CCYY = WS-DATE2-CCYY)
                  )
              MOVE WS-DATE1-CCYY TO LEAP-YEAR-CCYY
              PERFORM C-LEAP-YEAR-TEST
              IF LEAP-YEAR-FALSE
                 ADD 2 TO WS-DATE1-DD
              ELSE
                 ADD 1 TO WS-DATE1-DD.

           COMPUTE WS-XDAYS = ((WS-DATE2-CCYY - WS-DATE1-CCYY) * 360)
                            + ((WS-DATE2-MM - WS-DATE1-MM) * 30)
                            + (WS-DATE2-DD - WS-DATE1-DD ).

           PERFORM C-TIMEND.

      ***********************************************************
      *         COMPUTE ELAPSED TIME USING 362 YRTYPE
      *
      *   NAME: C-TIM362
      *   DESC: COMPUTE ELAPSED TIME BETWEEN DATE1 AND DATE2
      *
      *   IN  :
      *   OUT : ELAPSED_MONTHS, ELAPSED_DAYS AND ELAPSED_REM
      ***********************************************************
