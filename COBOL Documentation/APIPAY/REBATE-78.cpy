      *********************************
      *    RULE OF 78'S ROUTINE   (1)
      *********************************
       REBATE-78 SECTION.
            IF SP-RBEARLY(REB-SUB) NOT = 4
               PERFORM REBATE-78-ROUTINE
               GO TO REBATE-78-EXIT.

      * TEST FOR GEORGIA NORMAL REBATE FOR INTEREST:
            IF NOT (REB-LPTRCD = "PB" OR "RN" OR "SC"
                                    OR "RB" OR "RO")
               PERFORM REBATE-78-ROUTINE
               GO TO REBATE-78-EXIT.

      * TEST FOR GEORGIA RENEWAL PRORATION:
            PERFORM REBATE-78-ROUTINE.
            MOVE REB-REBATE TO REB-STATEWK.
            SUBTRACT 1 FROM REB-REMTERM.
            PERFORM REBATE-78-ROUTINE.

            COMPUTE REB-REBATE ROUNDED = REB-REBATE +
                (REB-STATEWK - REB-REBATE ) / 30 * REB-ELAPSED-REM.
            GO TO REBATE-78-EXIT.


       REBATE-78-EXIT.
           EXIT.

      ******************************************
      *    ACTUARIAL REBATE ROUTINE  2
      ******************************************
