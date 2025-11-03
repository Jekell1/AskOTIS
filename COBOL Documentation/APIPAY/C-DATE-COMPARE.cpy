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
       C-DATE-COMPARE SECTION.
      * NEW LOGIC ADDED 2017/02/02
            IF SYS-DATE = NUM-DATE
               MOVE "E" TO ELAPSED-RESULTS
            ELSE
            IF SYS-DATE > NUM-DATE
               MOVE "G" TO ELAPSED-RESULTS
            ELSE
               MOVE "L" TO ELAPSED-RESULTS.

      * OLD LOGIC COMMENTED OUT 2017/02/02
      * ===================================
      *     IF NUM-DATE = 0
      *        MOVE 0 TO WS-JUL-1
      *     ELSE
      *        MOVE NUM-DATE TO P-DATE-IN
      *        PERFORM C-CALL-JUL
      *        MOVE P-JULIAN-DATE TO WS-JUL-1
      *     END-IF.

      *     IF SYS-DATE = 0
      *        MOVE 0 TO WS-JUL-2
      *     ELSE
      *        MOVE SYS-DATE TO P-DATE-IN
      *        PERFORM C-CALL-JUL
      *        MOVE P-JULIAN-DATE TO WS-JUL-2
      *     END-IF.

      *     IF (NUM-DATE = 0 AND SYS-DATE = 0)
      **  NOTE SURE ABOUT THIS 123199 TEST.  LOOKS LIKE IT WAS ADDED TO 
      ** FIX A BUG WITH JULIAN WHERE 0 PASSED BACK SAME # AS 123199.
      **       (NUM-DATE = 0 AND SYS-DATE = 123199) OR
      **       (SYS-DATE = 0 AND NUM-DATE = 123199)
      *           MOVE "E" TO ELAPSED-RESULTS
      *     ELSE
      **       IF NUM-DATE = 0 AND S-YY < 50
      **          MOVE "G" TO ELAPSED-RESULTS
      **    ELSE
      **       IF NUM-DATE = 0 AND S-YY >= 50
      **          MOVE "L" TO ELAPSED-RESULTS
      **    ELSE
      **       IF SYS-DATE = 0 AND NUM-YR < 50
      **          MOVE "L" TO ELAPSED-RESULTS
      **    ELSE
      **       IF SYS-DATE = 0 AND NUM-YR >= 50
      **          MOVE "G" TO ELAPSED-RESULTS
      **    ELSE
      *        PERFORM C-TIM-RESULTS
      *        MOVE WS-TIM-RESULTS TO ELAPSED-RESULTS.
      * ===================================

      ***********************************************************
      *         DETERMINE THE RESULTS OF A DATE COMPARE
      *
      *   NAME: TIM_RESULTS
      *   DESC: DETERMINE IF DATE2 IS <, >, OR = TO DATE1
      *
      *   IN  : WS-JUL-1 AND WS-JUL-2
      *   OUT : WS-TIM-RESULTS
      ***********************************************************
