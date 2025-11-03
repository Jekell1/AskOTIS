      ************************************************
      *    COMPARE RATES ROUTINE
      ************************************************
       LPRATE-COMPARE-RATES SECTION.
           PERFORM LPRATE-SET-INTPDTH-DATE.
           MOVE INDU-DATE-1 TO NUM-DATE.
           MOVE XDTE-DATE TO SYS-DATE.
           IF SYS-DATE > NUM-DATE
              MOVE XDTE-DATE TO INDU-DATE-1.
           COMPUTE INDU-CURBAL = LN-CURBAL - LN-OT2BAL.
           MOVE RATE-LPTRCD TO INDU-LPTRCD.
           IF IBPC-STAT = "J"
              PERFORM LPRATE-JUDGEMENT
           ELSE
           IF IBPC-STAT = "R"
              MOVE LN-REDURATE TO INDU-RATE.

      ************************************************
      *    EXPIRED INTEREST ROUTINE
      *
      * THIS ROUTINE WALKS THRU THE SP-EXPR-RATETBL(5)
      *  DETERMINING WHICH RATE TO CHARGE AND FOR
      *  HOW LONG, COMPUTING INTEREST DUE FOR EACH
      *  BREAK.
      ************************************************
