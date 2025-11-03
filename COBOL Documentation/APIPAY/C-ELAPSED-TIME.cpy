      ***********************************************************
      *
      *   DESC: COMPUTE ELAPSED TIME BETWEEN NUM-DATE AND SYS-DATE
      *            - CALCULATIONS ARE DEFAULTED TO:
      *                   DATER-UNITPER-CD    =  M
      *                   DATER-UNITPER-FREQ  =  1
      *            - INPUT DATES NOT ALTERED
      *            - VERIFIES DATES
      *            - 360/361/362/358/999/365/367 YEARTYPE
      *            - OUTPUT MAY BE NEGATIVE
      *   IN     : NUM-DATE               FROM        MMDDYY
      *            SYS-DATE               TO          MMDDYY
      *            ELAPSED-YRTYPE         360/361/362/358/999/365/367
      *   DEFAULT:
      *            DATER-UNITPER-CD       =  M
      *            DATER-UNITPER-FREQ     =  1
      *   OUT    : ELAPSED-DAYS
      *            ELAPSED-MONTHS
      *            ELAPSED-REM
      *            ELAPSED-RESULT         SYS-IS-LT,GT,EQ,LE,GE,GR
      ***********************************************************
       C-ELAPSED-TIME SECTION.
           MOVE DATER-UNITPER-FREQ TO WS-XUNITPER-FREQ.
           MOVE DATER-UNITPER-CD   TO WS-XUNITPER-CODE.
           MOVE NUM-DATE           TO WS-DATE1 WS-DATE1W.
           MOVE SYS-DATE           TO WS-DATE2 WS-DATE2W.
      *----------------------------------------------------------------------
      *      TEST NUM-DATE AND/OR SYS-DATE = 0
      *      IF SO SET ALL OUTPUT = 0
      *          NOTE:
      *                CDTOI(   YR = 0, YIELDS 100   )
      *----------------------------------------------------------------------*/
           IF WS-DATE1 = 0 OR WS-DATE2 = 0
      *       MOVE 010101 TO WS-DATE1 WS-DATE2 WS-DATE1W WS-DATE2W.
              MOVE 19000101 TO WS-DATE1 WS-DATE2 WS-DATE1W WS-DATE2W.

           MOVE ELAPSED-YRTYPE TO TA-YRTYPE.
      *-----------------------------------------------------
      *       BASED ON YEAR TYPE
      *       DETERMINE:
      *                 ELAPSED MONTHS
      *                 ELAPSED DAYS
      *                 ELAPSED REM
      *                 RESULTS OF COMPARE
      *-----------------------------------------------------*
           PERFORM C-TIMALL
           PERFORM C-TIM-RESULTS
           MOVE WS-TIM-RESULTS TO ELAPSED_RESULTS
           MOVE WS-XMONTHS     TO ELAPSED-MONTHS
           MOVE WS-XDAYS       TO ELAPSED-DAYS
           MOVE WS-XREM        TO ELAPSED-REM
      *-----------------------------------------------------
      *       BASED ON UNIT PERIOD CODE & FREQ
      *       DETERMINE:
      *                 ELAPSED UNIT PERIODS
      *                 ELAPSED REM
      *-----------------------------------------------------*
           MOVE WS-DATE1W TO WS-DATE1
           MOVE WS-DATE2W TO WS-DATE2
           IF WS-XUNITPER-CODE = "Y" OR WS-XUNITPER-CODE = "M"
              MOVE 998 TO TA-YRTYPE
              PERFORM C-TIMALL
           ELSE
              IF WS-XUNITPER-CODE = "S"
                 MOVE 360 TO TA-YRTYPE
                 PERFORM C-TIMALL
              ELSE
                 MOVE 365 TO TA-YRTYPE
                 PERFORM C-TIMALL
              END-IF
           END-IF

           EVALUATE WS-XUNITPER-CODE
           WHEN "M"
                   IF WS-XUNITPER-FREQ = 1
                      MOVE WS-XMONTHS TO WS-XUNITPER
                      MOVE WS-XREM    TO WS-XUNITPER-REM
                   ELSE
                      COMPUTE WS-XDAYS = WS-XREM + (WS-XMONTHS * 30)
                      COMPUTE WS-DIV-TMP = WS-XUNITPER-FREQ * 30
                      DIVIDE  WS-XDAYS BY WS-DIV-TMP
                                         GIVING     WS-XUNITPER
                                         REMAINDER  WS-XUNITPER-REM
                   END-IF
           WHEN "S"
                   COMPUTE WS-XDAYS = (WS-XMONTHS * 30) + WS-XREM
                   COMPUTE WS-DIV-TMP = WS-XUNITPER-FREQ * 15
                   DIVIDE  WS-XDAYS BY WS-DIV-TMP
                                       GIVING     WS-XUNITPER
                                       REMAINDER  WS-XUNITPER-REM
           WHEN "W"
                   COMPUTE WS-DIV-TMP = WS-XUNITPER-FREQ * 7
                   DIVIDE  WS-XDAYS BY WS-DIV-TMP
                                       GIVING     WS-XUNITPER
                                       REMAINDER  WS-XUNITPER-REM
           WHEN "B"
                   COMPUTE WS-DIV-TMP = WS-XUNITPER-FREQ * 14
                   DIVIDE  WS-XDAYS BY WS-DIV-TMP
                                       GIVING     WS-XUNITPER
                                       REMAINDER  WS-XUNITPER-REM
           WHEN "D"
                   DIVIDE  WS-XDAYS BY WS-XUNITPER-FREQ
                                       GIVING     WS-XUNITPER
                                       REMAINDER  WS-XUNITPER-REM
           WHEN "Y"
                   COMPUTE WS-DIV-TMP = WS-XUNITPER-FREQ * 12
                   DIVIDE  WS-XMONTHS  BY WS-DIV-TMP
                                       GIVING     WS-XUNITPER
                                       REMAINDER  WS-XUNITPER-REM
           END-EVALUATE
           MOVE WS-XUNITPER     TO ELAPSED-UNITPER
           MOVE WS-XUNITPER-REM TO ELAPSED-UNITPER-REM

      *    MJD NOT PART OF OUTPUT
      *    INITIALIZE NDTE-DATE
           .

      **********************************************************
      *              (358,360,361,362,365,367,999 & 998 UNITPER)
      *
      *   NAME:  C-TIMALL
      *   DESC:  PERFORMS SELECTED TIME ROUTINE:
      *          358/360/361/362/365/367/999  & 998 UNITPER
      *   IN  :  TA-YRTYPE
      *   OUT :
      *********************************************************
