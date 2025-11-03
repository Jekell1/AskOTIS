      ******************************************
      *    REBATE TABLE FORMULA (T)
      *              ADDONS
      ******************************************
       REBATE-TABLE-FORMULA SECTION.
           IF SP-RBSPOPT1(REB-SUB) = 0
              GO TO REBATE-TABLE-FORMULA-EXIT.
      * CALL SUBPROGRAM TO DETERMINE REDUCTION:
           MOVE "T" TO REB-FORM-PROG-TYPE.
           MOVE 01 TO REB-FORM-PROG-NO.
           MOVE SP-RBSPOPT1(REB-SUB) TO T01-TBL-NO.
           MOVE REB-ORGTERM TO T01-ORGTERM.
           MOVE REB-ELAPSED-MONTHS TO T01-ELAPSED-MONTHS.
           MOVE REB-ELAPSED-DAYS TO T01-ELAPSED-DAYS.
           MOVE 100 TO T01-REFUND-RATE.
           PERFORM REBATE-CALL-SUBPROG.
           COMPUTE REB-REBATE ROUNDED =
                  REB-TOTCHG * T01-REFUND-RATE / 100.

       REBATE-TABLE-FORMULA-EXIT.
           EXIT.

      *******************************************************************
      *                     FORMULA (0, 2)
      *    MULLEN'S CALIFORNIA CL NET PAYOFF   BALBOA LIFE & CASUALTY
      *******************************************************************
