      *********************************
      *    PRO RATA REBATE METHOD  (5)
      *********************************
       REBATE-PRORATA SECTION.
           IF ( NOT (SP-RBEARLY(REB-SUB) = 5 OR 6) )
            OR
              ( ( SP-RBEARLY(REB-SUB) = 5 OR 6 ) AND
                               ( REB-ELAPSED-REM =  0 ) )
              PERFORM REBATE-PRORATA-ROUTINE
              GO TO REBATE-PRORATA-EXIT.

           IF SP-RBEARLY(REB-SUB) = 5
              PERFORM REBATE-PRORATA-GA-SWING
           ELSE
              PERFORM REBATE-PRORATA-GA-NO-SWING.
           GO TO REBATE-PRORATA-EXIT.

      * GEORGIA PRORATA / RULE 78 SLICE W/SWING FOR MAINT. FEES:
       REBATE-PRORATA-GA-SWING.
           PERFORM REBATE-PRORATA-ROUTINE.
           MOVE REB-REBATE TO REB-STATEWK.
           MOVE REB-TOTCHG TO REB-STATEWK2.
           MOVE LN-MAINTFEE TO REB-TOTCHG.
           PERFORM REBATE-78.
           MOVE REB-STATEWK2 TO REB-TOTCHG.
           COMPUTE REB-REBATE = REB-STATEWK
                          + (LN-MAINTFEE - REB-REBATE).

      * GEORGIA PRORATA / RULE 78 SLICE W/NO SWING FOR MAINT. FEES:
       REBATE-PRORATA-GA-NO-SWING.
           PERFORM REBATE-PRORATA-ROUTINE.
           IF REB-ELAPSED-REM NOT = 30
              COMPUTE REB-REBATE ROUNDED = REB-REBATE -
                  ( LN-MAINTFEE - (30 - REB-ELAPSED-REM)
                          * (30 - REB-ELAPSED-REM + 1)
                                        / 930 * LN-MAINTFEE ).

       REBATE-PRORATA-ROUTINE.
           IF REB-ORGTERM NOT = 0
              COMPUTE REB-REBATE ROUNDED =
                  (REB-TOTCHG - REB-DEDCHG) *
                         (REB-REMTERM / REB-ORGTERM).


       REBATE-PRORATA-EXIT.
           EXIT.

      *********************************
      *    ACTUARIAL (IB)     (6)
      *
      *    (INTEREST BEARING METHOD)
      *********************************
