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
       C-TIM-RESULTS SECTION.
           MOVE "L" TO WS-TIM-RESULTS.
           IF WS-JUL-2 > WS-JUL-1
              MOVE "G" TO WS-TIM-RESULTS
           ELSE
           IF WS-JUL-2 = WS-JUL-1
              MOVE "E" TO WS-TIM-RESULTS.

      ***********************************************************
      *   NAME: C-DATE-TEST
      *   DESC: IF A DATE IS VALID, RETURNS THE SAME
      *         DATE WHEN VALID, OR 0 WHEN NOT VALID
      *   IN  : NUM-DATE (CCYYMMDD)
      *   OUT : NUM-DATE (CCYYMMDD)
      *
      * MJD 141015 MADE SECTION
      ***********************************************************
