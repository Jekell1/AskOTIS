      ***********************************************
      *    PRO RATA, EXACT DAYS REBATE METHOD  (J)
      ***********************************************
       REBATE-PRORATA-EXACT SECTION.
           IF REB-ORGTERM NOT = 0
              COMPUTE REB-REBATE ROUNDED =
                  (REB-TOTCHG - REB-DEDCHG)
                  * ( REB-REMTERM / REB-ORGTERM
                           - REB-ELAPSED-REM
                               / (REB-ORGTERM * 30.41666667)
                    ).

      *****************************************************
      *    TEXAS A & H ANTICIPATED     (P)
      *    UVALDE FINANCE
      *
      *****************************************************
