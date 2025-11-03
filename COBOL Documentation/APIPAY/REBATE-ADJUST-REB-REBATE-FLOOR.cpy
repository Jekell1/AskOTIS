       REBATE-ADJUST-REB-REBATE-FLOOR.
      * NEXT STMT NECESSARY, AS < SP-RBMIN CHECK IS CONDITIONAL
           IF REB-REBATE < 0
              MOVE 0 TO REB-REBATE.

      * NO NEED TO REBATE WHEN BELOW THIS FLOOR
           IF SP-RBSPOPT1(REB-SUB) = 2 AND
              (REB-LPTRCD = "PB" OR "RN" OR "SC"
                              OR "RB" OR "RO")
              NEXT SENTENCE
           ELSE
              IF SP-RBMIN(REB-SUB) NOT = 999.01
                 IF SP-RBMIN(REB-SUB) NOT = 999.02
                    IF REB-REBATE < SP-RBMIN(REB-SUB)
                       MOVE 0 TO REB-REBATE
                    ELSE
                       NEXT SENTENCE
                 ELSE
      * 999.02 MERCURY "OK" PERSONAL PROPERTY
      * IF RENEWED, MINIMUM IS $1.00, IF PAID OUT, MINIMUM IS $5.00
                    IF (REB-LPTRCD = "PB" OR "RN" OR "SC"
                                        OR "RB" OR "RO")
                       IF REB-REBATE < 1.00
                          MOVE 0 TO REB-REBATE
                       ELSE
                          NEXT SENTENCE
                    ELSE
                       IF REB-REBATE < 5.00
                          MOVE 0 TO REB-REBATE.

      * SPECIAL OPTION 2 = 11:
      * THE FINAL REFUND IS ROUNDED UP OR DOWN TO WHOLE DOLLARS
           IF SP-RBSPOPT2(REB-SUB) = 11
              IF REB-REBATE-CENTS < 51
                 MOVE 0 TO REB-REBATE-CENTS
              ELSE
                 COMPUTE REB-UPR-WHOLE ROUNDED = REB-REBATE + 0.50
                 MOVE REB-UPR-WHOLE TO REB-REBATE.

      **************************************
      *   SPECIAL REBATE REDUCTION ROUTINES
      **************************************
