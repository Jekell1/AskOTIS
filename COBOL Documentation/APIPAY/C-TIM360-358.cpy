      ***********************************************************
      *         COMPUTE ELAPSED TIME USING 360 OR 358 YRTYPE
      *
      *   NAME: C-TIM360_358
      *   DESC: COMPUTE ELAPSED TIMES BETWEEN DATE1 AND DATE2
      *
      *   IN  : YEAR TYPE
      *   OUT : ELAPSED_MONTHS, ELAPSED_DAYS AND ELAPSED_REM
      ***********************************************************
       C-TIM360-358 SECTION.
           PERFORM C-TIMBEG.
           IF WS-DATE1-DD > 30
              MOVE 30 TO WS-DATE1-DD.
           IF WS-DATE2-DD > 30
              MOVE 30 TO WS-DATE2-DD.

           MOVE WS-DATE2-CCYY TO LEAP-YEAR-CCYY.
           PERFORM C-LEAP-YEAR-TEST.

           IF WS-DATE2-MM = 2 AND WS-DATE2-DD > WS-DATE1-DD
              IF WS-DATE2-DD = 29 AND LEAP-YEAR-TRUE
                 MOVE 30 TO WS-DATE2-DD
              ELSE
                IF WS-DATE2-DD = 28 AND LEAP-YEAR-FALSE
                   MOVE 30 TO WS-DATE2-DD.

           IF WS-DATE2-MM = 2 AND WS-DATE2-DD < WS-DATE1-DD
              IF WS-DATE2-DD = 29 AND LEAP-YEAR-TRUE
                 MOVE 29 TO WS-DATE1-DD
              ELSE
                IF WS-DATE2-DD = 28 AND LEAP-YEAR-FALSE
                   MOVE 28 TO WS-DATE1-DD.

           IF TA-YRTYPE = 358
              MOVE WS-DATE1-CCYY TO LEAP-YEAR-CCYY
              PERFORM C-LEAP-YEAR-TEST
              IF WS-DATE1-MM = 2 AND WS-DATE1-DD > WS-DATE2-DD
                 IF LEAP-YEAR-FALSE
                    ADD 2 TO WS-DATE1-DD
                 ELSE
                    ADD 1 TO WS-DATE1-DD.

           COMPUTE WS-XDAYS = ( (WS-DATE2-CCYY - WS-DATE1-CCYY) * 360 )
                              + ( (WS-DATE2-MM - WS-DATE1-MM) * 30 )
                              + (WS-DATE2-DD - WS-DATE1-DD).
           PERFORM C-TIMEND.

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
