      **************************************
      *   SPECIAL REBATE REDUCTION ROUTINES
      **************************************
       REBATE-SPEC-RBREDUC SECTION.
           IF SP-RBREDUC(REB-SUB) NOT = 9999.01
              GO TO REBATE-SPEC-RBREDUC-NV.

           MOVE 0 TO A01-MS-PENALTY REB-MS-PENALTY.
           MOVE "A" TO REB-FORM-PROG-TYPE.
      * MISSISSIPPI ROUTINE:

      * TEST FOR NORMAL PAYOFF:
      *    IF REB-LPTRCD NOT = "PO"
           IF REB-LPTRCD = "PB" OR "RN" OR "SC"
                              OR "RB" OR "RO"
              GO TO REBATE-SPEC-RBREDUC-EXIT.

      * TEST FOR PAYOFF WITHIN 1ST 5 YEARS:
           MOVE 360 TO ELAPSED-YRTYPE.
           MOVE REB-LN-LOANDATE TO NUM-DATE.
           MOVE REB-PAYDATE TO SYS-DATE.
           PERFORM TIMALL.
           IF ELAPSED-MONTHS < 0 OR > 60
              GO TO REBATE-SPEC-RBREDUC-EXIT.

      * CALL SUBPROGRAM TO DETERMINE REDUCTION:
           MOVE 01 TO REB-FORM-PROG-NO.
      * 9/13/01 MS STATE BANKING DEPT SAID WHENEVER THERE ARE REMAINING
      *         DAYS, YOU SHOULD BUMP INTO THE NEXT MONTH, WHICH COULD
      *         CAUSE A DIFFERENT PERCENT !!
      * EXAMPLE:  IF 36 MONTHS AND 5 DAYS, THIS USED TO CHARGE 3%, BUT
      *           IT SHOULD CHARGE 2% BECAUSE YOU ARE INTO THE 4TH YR.
           IF ELAPSED-REM > 0
              ADD 1 TO ELAPSED-MONTHS.
           MOVE ELAPSED-MONTHS TO A01-ELAPSED-MO.
           MOVE REB-PAYDATE TO A01-PAYDATE.
           MOVE REB-REBATE TO REB-COMMON-REBATE.
           PERFORM REBATE-CALL-SUBPROG.
           IF REB-SUB = 0 OR REB-COMMON-RTNCD NOT = " "
              GO TO REBATE-SPEC-RBREDUC-EXIT.
           MOVE REB-COMMON-REBATE TO REB-REBATE.
           MOVE A01-MS-PENALTY TO REB-MS-PENALTY.

           GO TO REBATE-SPEC-RBREDUC-EXIT.

      * ------------------------------------------------------
      * NEVADA ROUTINE:  9999.02 MERCURY ACTION SPRDER = 9
      * TEST FOR CUSTOMER PAYOFF PRIOR TO ORIGINAL MATURITY
      *      IF SO DEDUCT $25.00 FROM REBATE:
      * ------------------------------------------------------
       REBATE-SPEC-RBREDUC-NV.
           IF SP-RBREDUC(REB-SUB) NOT = 9999.02
              GO TO REBATE-SPEC-RBREDUC-EXIT.

           IF NOT (REB-LPTRCD = "PB" OR "RN" OR "SC"
                                   OR "RB" OR "RO")
              MOVE "Y" TO MDTE-ORGTERM-FG
                          MDTE-FLAG
              PERFORM MATURITY-DATE-CALCULATION
              IF REB-PAYDATE < MDTE-DATE
                 SUBTRACT 25.00 FROM REB-REBATE
                 IF REB-REBATE < 0
                    MOVE 0 TO REB-REBATE.

       REBATE-SPEC-RBREDUC-EXIT.
           EXIT.

      *****************************************************
      *    SPRING FINANCE (NJ) PRE-PAYOFF PENALTY
      *
      *    IF PAYOFF(PO) IS PRIOR TO MATURITY TAKE PENALTY
      *****************************************************
