      *********************************************************
      *          INCREMENT MONTH ROUTINE
      *
      *          INCREMENTS A DATE BY THE NO.
      *          OF MONTHS FOUND IN INCREMENT.
      *          THE CORRECT LAST DAY OF MONTH IS ADJUSTED.
      *********************************************************
       C-INCR-MONTHS SECTION.

      * CHECK TO SEE HOW MANY YEARS & MONTHS TO ADVANCE

           DIVIDE WS-UP-WORK BY 12 GIVING    WS-DIV-RESULT
                                   REMAINDER WS-MOD-12.
           ADD WS-MOD-12     TO WS-NDTE-MM-S.
           ADD WS-DIV-RESULT TO WS-NDTE-CCYY-S.
           IF WS-NDTE-MM-S < 1
              ADD 12 TO WS-NDTE-MM-S
              ADD -1 TO WS-NDTE-CCYY-S
           ELSE
             IF WS-NDTE-MM-S > 12
                ADD -12 TO WS-NDTE-MM-S
                ADD 1 TO WS-NDTE-CCYY-S.

      * THIS PART WAS IGNORED FROM THE ORIGINAL C CODE NOT TO CHANGE
      * THE YEAR WHEN IT IS LESS THAN 50
      * CHANGED FOR 4 DIGIT YEAR AND CHANGED BACK TO 50
      *   ELSE IT DOES NOT MATCH A15 DON'T KNOW WHY EDWIN LEFT THIS OUT
      *    IF WS-NDTE-YY-S < 50
      *       MOVE 50 TO WS-NDTE-YY-S
      *    END-IF

      * COMMENTING OUT NOW THAT WE ARE USING FULL 8 DIGIT YEARS
      *     IF WS-NDTE-YY-S < 1950
      *        MOVE 50 TO WS-NDTE-YY-S
      *     END-IF

      * THIS PART WAS CHANGED FROM THE ORIGINAL C CODE TO LIMIT
      * THE YEAR RESULT TO 99 INSTEAD OF 149 BECAUSE THE OUTPUT
      * DATE FORMAT HAS 2 DIGITS ONLY
      * CHANGED FOR 4 DIGIT YEAR AND CHANGED BACK TO 49
      *   ELSE IT DOES NOT MATCH A15 NOT SURE WHY EDWIN CHANGED THIS
      *MJD IF WS-NDTE-YY-S > 99

      * COMMENTING OUT NOW THAT WE ARE USING FULL 8 DIGIT YEARS
      *    IF WS-NDTE-YY-S > 2049
      *       MOVE 49 TO WS-NDTE-YY-S
      *    END-IF

      * NOW CALL NEW-DATE
           IF WS-NDTE-DD-S > 99
              MOVE 99 TO NDTE-DD
           ELSE
              MOVE WS-NDTE-DD-S TO NDTE-DD.

           MOVE WS-NDTE-CCYY-S TO NDTE-CCYY.
           MOVE WS-NDTE-MM-S   TO NDTE-MM.
           MOVE "N" TO DATER-ACTION-CODE.
           PERFORM C-CALL-NEWDATE.
           MOVE NDTE-CCYY TO WS-NDTE-CCYY-S.
           MOVE NDTE-MM TO WS-NDTE-MM-S.
           MOVE NDTE-DD TO WS-NDTE-DD-S.

      **********************************************
      *  RETURN NUMBER OF DAY OF THE WEEK FOR A DATE
      *     1 - SUNDAY, 2 - MONDAY, 3 - TUESDAY, ETC.
      **********************************************
