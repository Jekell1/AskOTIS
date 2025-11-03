      **********************************************************
      *              (358,360,361,362,365,367,999 & 998 UNITPER)
      *
      *   NAME:  C-TIMALL
      *   DESC:  PERFORMS SELECTED TIME ROUTINE:
      *          358/360/361/362/365/367/999  & 998 UNITPER
      *   IN  :  TA-YRTYPE
      *   OUT :
      *********************************************************
       C-TIMALL SECTION.
           EVALUATE TA-YRTYPE
           WHEN 358 PERFORM C-TIM360-358
           WHEN 360 PERFORM C-TIM360-358
           WHEN 361 PERFORM C-TIM361
           WHEN 362 PERFORM C-TIM362
           WHEN 365 PERFORM C-TIM365-367
           WHEN 367 PERFORM C-TIM365-367
           WHEN 998 PERFORM C-TIM999-998
           WHEN 999 PERFORM C-TIM999-998
           END-EVALUATE
           .

      ***********************************************************
      *         COMPUTE ELAPSED TIME USING 360 OR 358 YRTYPE
      *
      *   NAME: C-TIM360_358
      *   DESC: COMPUTE ELAPSED TIMES BETWEEN DATE1 AND DATE2
      *
      *   IN  : YEAR TYPE
      *   OUT : ELAPSED_MONTHS, ELAPSED_DAYS AND ELAPSED_REM
      ***********************************************************
