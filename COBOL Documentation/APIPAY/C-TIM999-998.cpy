      ***********************************************************
      * ORIGINAL REMARKS
      *         COMPUTE ELAPSED TIME USING 999 YRTYPE
      *                TRUTH AND LENDING METHOD
      *           AND
      *                 ELAPSED UNIT PERIODS 998 YRTYPE
      *
      *   NAME: TIM999_998
      *   DESC: COMPUTE ELAPSED TIMES BETWEEN DATE1 AND DATE2
      *         BASED ON TRUTH AND LENDING REG. Z
      *
      *         COMPUTE ELAPSED UNIT PERIODS BETWEEN DATE1
      *         AND DATE2
      *
      *   IN  :
      *   OUT : ELAPSED_MONTHS, ELAPSED_DAYS AND ELAPSED_REM
      *         IF 998 ELAPSED_MONTHS = ELAPSED UNIT PERIODS
      * REV:
      *  JTG 050297 CHANGED TO PASS YRTYPE TO C-TIMEND FOR 998
      *             ELAPSED UNIT PERIODS
      *             EXAMPLE:
      *                     NUM-DATE:  030397
      *                     SYS-DATE:  050297
      *      
      *                                     999     998
      *                                     ---     ---
      *                     ELAPSED_MONTHS:   2       1
      *                     ELAPSED_DAYS  :  60      60
      *                     ELAPSED_REM   :   0      30
      ***********************************************************
       C-TIM999-998 SECTION.
           PERFORM C-TIMBEG.
           COMPUTE WS-XMONTHS = (
                           (WS-DATE2-CCYY - WS-DATE1-CCYY) * 12 ) +
                           (  WS-DATE2-MM - WS-DATE1-MM ).

           IF WS-DATE1-DD > WS-DATE2-DD
              ADD -1 TO WS-XMONTHS.

           COMPUTE WS-UP-WORK = 0 - WS-XMONTHS.
           MOVE WS-DATE2-MM TO WS-NDTE-MM-S.
           MOVE WS-DATE2-DD TO WS-NDTE-DD-S.
           MOVE WS-DATE2-CCYY TO WS-NDTE-CCYY-S.
           PERFORM C-INCR-MONTHS.
           MOVE WS-NDTE-CCYY-S TO WS-DATE2-CCYY.
           MOVE WS-NDTE-MM-S TO WS-DATE2-MM.
           MOVE WS-NDTE-DD-S TO WS-DATE2-DD.

           MOVE WS-DATE2 TO P-DATE-IN.
           PERFORM C-CALL-JUL.
           MOVE P-JULIAN-DATE TO WS-JUL-TMP.
           COMPUTE WS-XDAYS = (WS-XMONTHS * 30) + WS-JUL-TMP - WS-JUL-1.

           PERFORM C-TIMEND.

      *********************************************************
      *          INITIAL ROUTINES FOR ALL ELAPSED TIME ROUTINES
      *
      *   NAME:  C-TIMBEG
      *   DESC:  THIS ROUTINE SETS UP THE DATES IN REVERSE
      *          ORDER IF NUMDATE IS AFTER SYSDATE
      *          THIS ROUTINE ALSO CONVERTS DATES TO JULIAN
      *   IN  :
      *   OUT :
      *********************************************************
