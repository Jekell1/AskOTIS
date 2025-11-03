      ***********************************************************
      *         COMPUTE ELAPSED TIME USING 362 YRTYPE
      *
      *   NAME: C-TIM362
      *   DESC: COMPUTE ELAPSED TIME BETWEEN DATE1 AND DATE2
      *
      *   IN  :
      *   OUT : ELAPSED_MONTHS, ELAPSED_DAYS AND ELAPSED_REM
      ***********************************************************
       C-TIM362 SECTION.
           MOVE WS-DATE2 TO WS-DATEX.
           PERFORM C-TIMBEG.
           COMPUTE WS-XREM    = WS-DATE2-CCYY - WS-DATE1-CCYY.
           COMPUTE WS-XMONTHS = WS-DATE2-MM - WS-DATE1-MM.
           COMPUTE WS-XDAYS   = WS-DATE2-DD - WS-DATE1-DD.

           IF ( WS-XDAYS NOT < 0 )
              COMPUTE WS-XDAYS = (( (WS-XREM * 12) + WS-XMONTHS) * 30 )
                                 + WS-XDAYS
           ELSE
              ADD -1 TO WS-XMONTHS
              COMPUTE WS-HOLD-MOS = (WS-XREM * 12) + WS-XMONTHS
              MOVE -1 TO WS-UP-WORK
              MOVE WS-DATEX-CCYY TO WS-NDTE-CCYY-S
              MOVE WS-DATEX-MM TO WS-NDTE-MM-S
              MOVE WS-DATEX-DD TO WS-NDTE-DD-S
              PERFORM C-INCR-MONTHS
              MOVE WS-NDTE-CCYY-S TO WS-DATEX-CCYY
              MOVE WS-NDTE-MM-S TO WS-DATEX-MM
              MOVE WS-NDTE-DD-S TO WS-DATEX-DD
              MOVE WS-DATE1-DD TO WS-DATEX-DD
              IF WS-DATE1-DD > 28
                 COMPUTE WS-TMP-WORK = DYTOM(WS-DATEX-MM + 1) -
                                       DYTOM(WS-DATEX-MM)
                 MOVE WS-DATEX-CCYY TO LEAP-YEAR-CCYY
                 PERFORM C-LEAP-YEAR-TEST
                 IF LEAP-YEAR-FALSE AND WS-DATEX-MM = 2
                    ADD 1 TO WS-TMP-WORK
                 END-IF
                 IF WS-DATE1-DD > WS-TMP-WORK
                    COMPUTE WS-DATEX-MM = WS-DATE2-MM
                    MOVE 1 TO WS-DATEX-DD
                    MOVE WS-DATE2-CCYY TO WS-DATEX-CCYY
                 END-IF
              END-IF

              MOVE WS-DATE2 TO P-DATE-IN
              PERFORM C-CALL-JUL
              MOVE P-JULIAN-DATE TO WS-TMP-JUL-1

              MOVE WS-DATEX TO P-DATE-IN
              PERFORM C-CALL-JUL
              MOVE P-JULIAN-DATE TO WS-TMP-JUL-2

              COMPUTE WS-XDAYS = WS-TMP-JUL-1 - WS-TMP-JUL-2
              IF WS-XDAYS > 30
                 COMPUTE WS-XDAYS = WS-XDAYS + ((WS-HOLD-MOS - 1) * 30)
              ELSE
                 COMPUTE WS-XDAYS = WS-XDAYS + (WS-HOLD-MOS * 30)
              END-IF
           END-IF.

           PERFORM C-TIMEND.

      ***********************************************************
      *         COMPUTE ELAPSED TIME USING 365 OR 367 YRTYPE
      *
      *   NAME: C-TIM365_367
      *   DESC: COMPUTE ELAPSED TIME BETWEEN DATE1 AND DATE2
      *
      *   IN  :
      *   OUT : ELAPSED_MONTHS, ELAPSED_DAYS AND ELAPSED_REM
      ***********************************************************
