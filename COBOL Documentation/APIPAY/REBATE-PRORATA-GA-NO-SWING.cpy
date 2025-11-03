      * GEORGIA PRORATA / RULE 78 SLICE W/NO SWING FOR MAINT. FEES:
       REBATE-PRORATA-GA-NO-SWING.
           PERFORM REBATE-PRORATA-ROUTINE.
           IF REB-ELAPSED-REM NOT = 30
              COMPUTE REB-REBATE ROUNDED = REB-REBATE -
                  ( LN-MAINTFEE - (30 - REB-ELAPSED-REM)
                          * (30 - REB-ELAPSED-REM + 1)
                                        / 930 * LN-MAINTFEE ).

