      ****************************************
      *    SET REB-SUB FOR CREDIT LIFE REBATE
      ****************************************
       PAYOFF-SET-CL-REBSUB SECTION.
      * TEST FOR SINGLE OR JOINT LIFE:
           IF LN-INSURED = "J"
              MOVE 2 TO REB-SUB
            ELSE
              MOVE 1 TO REB-SUB.
      * TEST FOR LEVEL LIFE:
           IF LN-INSTYPES-LL
              ADD 2 TO REB-SUB.

      **************************************************
      *    GET ALL LATE CHARGES DUE THRU PAYOFF DATE:
      **************************************************
