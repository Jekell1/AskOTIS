      ************************************************
      *    FIND THRU DATES ROUTINE
      ************************************************
       LPRATE-FIND-THRU-DATE SECTION.
           MOVE RATE-EXPR-TAB-BREAK(EXPR) TO NUM-DATE.
           MOVE RATE-DATE TO SYS-DATE.
           PERFORM TIM.
           IF ELAPSED-DAYS < 0
                   OR RATE-EXPR-TAB-BREAK(EXPR) = 0
              MOVE RATE-DATE TO INDU-DATE-2
              MOVE 0 TO RATE-EXPR-TAB-BREAK(EXPR)
           ELSE
              MOVE INDU-DATE-1 TO NUM-DATE
              MOVE RATE-EXPR-TAB-BREAK(EXPR) TO SYS-DATE
              PERFORM TIM
              IF ELAPSED-DAYS NOT > 0
                 MOVE INDU-DATE-1 TO INDU-DATE-2
              ELSE
                 MOVE RATE-EXPR-TAB-BREAK(EXPR) TO INDU-DATE-2.

      ************************************************
      *    JUDGEMENT ROUTINE
      ************************************************
