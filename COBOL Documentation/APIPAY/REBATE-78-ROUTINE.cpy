      ***************************
      * 78'S ROUTINE
      ***************************
       REBATE-78-ROUTINE SECTION.
           IF REB-ORGTERM = REB-REMTERM
              COMPUTE REB-REBATE = REB-TOTCHG - REB-DEDCHG
           ELSE
              PERFORM REBATE-78-SUMORG
              IF REB-SUMORG NOT = 0
                 COMPUTE REB-SUMORG ROUNDED =
                              (REB-TOTCHG - REB-DEDCHG) / REB-SUMORG
                 PERFORM REBATE-78-REBATE
              ELSE
                 PERFORM REBATE-78-REBATE.

      ******************************************
      * UNIT CHARGE * 78'S FOR REMAINING TERM
      ******************************************
