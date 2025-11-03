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

