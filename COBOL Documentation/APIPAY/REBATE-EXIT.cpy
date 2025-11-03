       REBATE-EXIT.
           IF BR-FORCE-VALID-CITY-COUNTY = "Y"
      *      CREDIT LIFE SALES TAX:
              IF REB-SUB < 5
                 COMPUTE INS-TAX-RB ROUNDED =
                    LN-FEEAMT(5) * REB-REBATE / REB-TOTCHG
                 ADD INS-TAX-RB TO REB-REBATE
              ELSE
      *      A&H SALES TAX:
              IF REB-SUB = 5
                 COMPUTE INS-TAX-RB ROUNDED =
                    LN-FEEAMT(6) * REB-REBATE / REB-TOTCHG
                 ADD INS-TAX-RB TO REB-REBATE
              ELSE
      *      PP & PP2 SALES TAX:
              IF REB-SUB = 6 OR 11
                 COMPUTE INS-TAX-RB ROUNDED =
                    LN-FEEAMT(7) * REB-REBATE / REB-TOTCHG
                 ADD INS-TAX-RB TO REB-REBATE
              ELSE
      *      O1 OR O2 OR O3 OR O4 OR O5 SALES TAX:
              IF REB-SUB > 11 AND < 17
                 COMPUTE INS-TAX-RB ROUNDED =
                    LN-FEEAMT(REB-SUB - 4) * REB-REBATE / REB-TOTCHG
                 ADD INS-TAX-RB TO REB-REBATE.

           MOVE 0 TO REB-STATEWK.
           MOVE SPACES TO REB-SKIPDEF-FG
                          REB-ADDON-FG.


      *********************************
      *    RULE OF 78'S ROUTINE   (1)
      *********************************
