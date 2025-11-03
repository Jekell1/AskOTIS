       PAYOFF-INT-ADDONS-NEXT.
           PERFORM READ-LTI1-FILE-NEXT.
           IF IO-FG NOT = 0 OR (LTI-BRNO NOT = LTI-PATH-OWNBR)
              GO TO END-PAYOFF-INT-ADDONS.

           IF LTI-ACCTNO NOT = LN-ACCTNO
              GO TO END-PAYOFF-INT-ADDONS.

      * THIS LOGIC IS USED BY LONPG7.C
      *----------------------------------------------------------------
           IF NOT (POFF-INSEFF-TEST = 0 AND POFF-INSEXP-TEST = 0)
              IF NOT ( LTI-INSEFF-DATE = POFF-INSEFF-TEST AND
                       LTI-INSEXP-DATE = POFF-INSEXP-TEST
                     )
                 GO TO PAYOFF-INT-ADDONS-NEXT
              END-IF
              IF LTI-INS-REBATE NOT = " "
                 GO TO PAYOFF-INT-ADDONS-NEXT
              END-IF
              PERFORM PAYOFF-INT-ADDONS-GET-LP-REC
              IF POFF-RTNCD = "Y"
                 GO TO PAYOFF-INT-ADDONS-NEXT.
      *----------------------------------------------------------------

      * COMMON LOGIC TO COMPUTE ADDON INTEREST REFUND:
           PERFORM ADDONRB-GET-INT-REBATE.

           IF REB-SUB NOT = 0
              MULTIPLY POFF-ONE BY REB-REBATE
              ADD REB-REBATE TO POFF-ADDON-INT-REBATE
           ELSE
              MOVE 0 TO REB-REBATE.
           IF LTI-INS-TYPE = 0
              ADD REB-REBATE TO POFF-INT-REB-TBL(9)
           ELSE
              ADD REB-REBATE TO POFF-INT-REB-TBL(LTI-INS-TYPE).
           GO TO PAYOFF-INT-ADDONS-NEXT.

