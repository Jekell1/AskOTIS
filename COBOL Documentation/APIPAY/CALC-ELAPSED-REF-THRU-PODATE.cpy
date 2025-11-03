      ****************************************************
      * FIND ELAPSED DD MM & REM ON REFERENCE THRU PAYOFF
      ****************************************************
       CALC-ELAPSED-REF-THRU-PODATE SECTION.
           MOVE REB-PAYDATE TO SYS-DATE.
      * TEST FOR MISSISSIPPI REBATE 20 OR 90 DAY OPTION,
      * IF SO ADD 20 OR 90 DAYS TO PAYDATE:
           IF SP-RBSPOPT1(REB-SUB) = 3
              MOVE 20 TO REB-ELAPSED-REM
           ELSE
              IF SP-RBSPOPT1(REB-SUB) = 4
                 MOVE 90 TO REB-ELAPSED-REM.
           IF SP-RBSPOPT1(REB-SUB) = 3 OR 4
              MOVE REB-PAYDATE TO NUM-DATE
              PERFORM JUL
              ADD REB-ELAPSED-REM TO JULIAN-DATE
              PERFORM CJUL
              MOVE NUM-DATE TO SYS-DATE.
           MOVE REB-REFDATE TO NUM-DATE.

      * WHEN SP-RBUNITPER-CD(REB-SUB) = "B"
      *    IF OTHER THAN MONTHLY PAY CODE OR PAYFREQ
      *       COMPUTATION OF ELAPSED-MONTHS, ETC WILL BE DONE ON
      *       UNIT PERIOD BASES

           IF SP-RBUNITPER-CD(REB-SUB) = "B" AND
              (NOT (LN-UNITPER-CD = "M" AND LN-UNITPER-FREQ = 1 ))
              MOVE LN-UNITPER-CD       TO DATER-UNITPER-CD
              MOVE LN-UNITPER-FREQ     TO DATER-UNITPER-FREQ
              PERFORM TIMUPER
              MOVE ELAPSED-UNITPER     TO REB-ELAPSED-MONTHS
              MOVE ELAPSED-UNITPER-REM TO REB-ELAPSED-REM
              MOVE ELAPSED-DAYS        TO REB-ELAPSED-DAYS
           ELSE
              MOVE SP-RBYRTYPE(REB-SUB, REB-SUB2) TO ELAPSED-YRTYPE
              PERFORM TIMALL
              MOVE ELAPSED-MONTHS TO REB-ELAPSED-MONTHS
              MOVE ELAPSED-REM    TO REB-ELAPSED-REM
              MOVE ELAPSED-DAYS   TO REB-ELAPSED-DAYS.

      ********************************************
      *    APPLICATION OF 'SWING RULE':
      *    ESTABLISH REM-TERM
      *    ADJUST TERM BY DEFERMENTS
      ********************************************
