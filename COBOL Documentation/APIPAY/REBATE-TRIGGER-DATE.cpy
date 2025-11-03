      * EXAMPLE FOR MS OPTION1 = 3
      * ON FIRST PAYDATE, WILL MOVE 1 TO REB-SUB2, THEN NEXT IF, ELAPSED
      * IS 0, SO IS 0 >= 1 ? NO, SO MOVE 2 TO REB-SUB2 WHICH IS NOT
      * CONSIDERED PODATE-B4-1STPYDATE AND THAT IS WHAT THEY WANT!
      * AFTER FIRST PAYDATE, WILL MOVE 1 TO REB-SUB2, THEN NEXT IF, ELAPSED
      * IS -1, SO IS -1 >= 1 ? NO, SO MOVE 2 TO REB-SUB2 WHICH IS NOT
      * CONSIDERED PODATE-B4-1STPYDATE AND THAT IS WHAT THEY WANT!
      * PRIOR FIRST PAYDATE, WILL MOVE 1 TO REB-SUB2, THEN NEXT IF, ELAPSED
      * IS 3, SO IS 3 >= 1 ? YES, SO MOVE 1 TO REB-SUB2 WHICH IS
      * CONSIDERED PODATE-B4-1STPYDATE AND THAT IS WHAT THEY WANT!
      ***************************************************
      *    DETERMINE TRIGGER DATE FOR CALC OF REB-SUB2
      *      AND IF SP-RBEARLY(8), CHANGE REB-ORGTERM
      ***************************************************
       REBATE-TRIGGER-DATE SECTION.
           MOVE REB-LN-ORIG-1STPYDATE TO REB-TRIGGER-DATE.

           IF SP-RBEARLY(REB-SUB) = 8
              MOVE REB-ORGTERM TO REB-HOLD-ORGTERM
              MOVE REB-LN-INTDATE TO NDTE-DATE
              MOVE 1 TO NDTE-HOLD
              PERFORM INCREMENT-MONTHS
              MOVE NDTE-DATE TO REB-TRIGGER-DATE
              IF REB-SUB = 7 OR 8 OR 9
                 PERFORM REBATE-GET-EXTDAYS
                 IF REB-TOTAL-EXTDAYS NOT < 30
                    COMPUTE REB-ORGTERM = REB-ORGTERM +
                                          (REB-TOTAL-EXTDAYS / 30).

      ****************************************************
      * FIND ELAPSED DD MM & REM ON REFERENCE THRU PAYOFF
      ****************************************************
