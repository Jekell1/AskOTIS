      **********************************************
      *  RETURN NUMBER OF DAY OF THE WEEK FOR A DATE
      *     1 - SUNDAY, 2 - MONDAY, 3 - TUESDAY, ETC.
      **********************************************
       C-WEEKDAY SECTION.
           MOVE NUM-DATE TO P-DATE-IN.
           PERFORM C-CALL-JUL.
           MOVE P-JULIAN-DATE TO WS-JUL-WK.
           COMPUTE WS-JUL = WS-JUL-WK   + 1.
           DIVIDE WS-JUL BY 7 GIVING WS-DIV-RESULT
                              REMAINDER WS-MOD-7.
           COMPUTE DAY--OF-WEEK = WS-MOD-7 + 1.

      **********************************************
      *  COMPARES TWO DATES
      *  DESC COMPARES SYS-DATE TO NUM-DATE
      *  MJD 130514 DO NOT CALL JUL IF DATES ARE ZERO
      *  MJD 140124 ADDED LOGIC TO ALTER ELAPSED RESULTS WHEN ZERO DATES
      *             ARE USED TO MATCH RESULTS GENERATEDIN A15.
      *         IN A15:
      *            IF SYS-DATE = 0
      *              IF NUM-YR < 50 THEN ELAPSED-RESULTS = "L"  FOR ALL MM/DD
      *              IF NUM-YR >= 50 THAN ELAPSED RESULTS = "G" FOR ALL MM/DD
      *              IF NUM-DATE = 123199 THEN ELAPSED RESULTS = "E"
      *
      *            IF NUM-DATE = 0
      *              IF SYS-YY < 50 THEN ELAPSED-RESULTS = "G" FOR ALL MM/DD
      *              IF SYS-YY >= 50 THAN ELAPSED RESULTS = "L" FOR ALL MM/DD
      *              IF SYS-DATE = 123199 THEN ELAPSED RESULTS = "E"
      * MJD 151110 FIXED FOR CCYY NO LONGER NEED SWING DATE TEST.
      **********************************************
