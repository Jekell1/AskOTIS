       PAYOFF-OTHER-INS.
      * COMPUTE OTHER 1 INSURANCE REBATE:
           IF LN-O1-REBATE NOT = "Y"
              IF LN-INSPREM(4) NOT = 0
                 IF LN-INSOURS(4) NOT = "M"
                    MOVE 12 TO REB-SUB
                    MOVE LN-INSPREM(4) TO REB-TOTCHG
                    MOVE 4 TO ITRM-SUB
                    PERFORM INS-TERM-CALCULATION
                    MOVE ITRM-INSTERM TO REB-ORGTERM
                    IF LN-ADDON-FG = "Y"
                       MOVE 4 TO POFF-SUB
                       PERFORM PAYOFF-SET-ADDON-INS
                    END-IF
                    PERFORM PAYOFF-GET-REBATE
                    MOVE REB-REBATE  TO POFF-REBATE(9)
                    MOVE INS-TAX-RB TO POFF-INS-TAX-RB(4).


      * COMPUTE OTHER 2 INSURANCE REBATE:
           IF LN-O2-REBATE NOT = "Y"
              IF LN-INSPREM(5) NOT = 0
                 IF LN-INSOURS(5) NOT = "M"
                    MOVE 13 TO REB-SUB
                    MOVE LN-INSPREM(5) TO REB-TOTCHG
                    MOVE 5 TO ITRM-SUB
                    PERFORM INS-TERM-CALCULATION
                    MOVE ITRM-INSTERM TO REB-ORGTERM
                    IF LN-ADDON-FG = "Y"
                       MOVE 5 TO POFF-SUB
                       PERFORM PAYOFF-SET-ADDON-INS
                    END-IF
                    PERFORM PAYOFF-GET-REBATE
                    MOVE REB-REBATE  TO POFF-REBATE(10)
                    MOVE INS-TAX-RB TO POFF-INS-TAX-RB(5).

      * COMPUTE OTHER 3 INSURANCE REBATE:
           IF LN-O3-REBATE NOT = "Y"
              IF LN-INSPREM(6) NOT = 0
                 IF LN-INSOURS(6) NOT = "M"
                    MOVE 14 TO REB-SUB
                    MOVE LN-INSPREM(6) TO REB-TOTCHG
                    MOVE 6 TO ITRM-SUB
                    PERFORM INS-TERM-CALCULATION
                    MOVE ITRM-INSTERM TO REB-ORGTERM
                    IF LN-ADDON-FG = "Y"
                       MOVE 6 TO POFF-SUB
                       PERFORM PAYOFF-SET-ADDON-INS
                    END-IF
                    PERFORM PAYOFF-GET-REBATE
                    MOVE REB-REBATE  TO POFF-REBATE(11)
                    MOVE INS-TAX-RB TO POFF-INS-TAX-RB(6).

