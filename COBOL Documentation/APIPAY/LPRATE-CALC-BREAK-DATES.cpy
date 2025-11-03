      ************************************************
      *    CALC BREAK DATES ROUTINE
      *
      * THIS ROUTINE CONVERTS ALL SP-EXPR-BREAK'S
      *  INTO DATES, BASED ON MONTHS AND DAYS FROM
      *  EXPIRATION.
      ************************************************
       LPRATE-CALC-BREAK-DATES SECTION.
      * SET SPR EXPR BREAK MONTHS & DAYS(.99 MONTHS):
           MOVE SP-EXPR-BREAK(EXPR) TO RATE-EXPR-BREAK.
           IF RATE-EXPR-BREAK > 360
              MOVE EXT-JULIAN-END TO NDTE-DATE
           ELSE
              MOVE XDTE-DATE TO NDTE-DATE
              MOVE RATE-EXPR-MONTHS TO NDTE-HOLD
              PERFORM INCREMENT-MONTHS
              COMPUTE NDTE-HOLD ROUNDED = RATE-EXPR-DAYS * 0.30
              PERFORM INCREMENT-DAYS.
           MOVE NDTE-DATE TO RATE-EXPR-TAB-BREAK(EXPR).
           SET EXPR UP BY 1.

      ************************************************
      *    FIND THRU DATES ROUTINE
      ************************************************
