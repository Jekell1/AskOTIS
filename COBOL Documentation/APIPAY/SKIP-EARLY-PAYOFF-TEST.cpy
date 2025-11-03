      * WHEN BR-BP/LBOX-ALLOW-PMT-ACCT-OTHBAL = "Y"
      *      THAN ALLOW THE BATCH PAYMENT TO BE APPLIED
      *      TO THE ACCOUNT BUT NOT TO OTHER BALANCE
      *
      * 4/12/07  WE THINK THE CHECK BELOW MAKES SENSE IF THERE IS
      *          AN OTHER BAL ON THE ACCOUNT.  IF THERE IS NO OTHER
      *          BAL, THIS STOPS A PAYOFF FROM POSTING AND GIVE
      *          STUPID CUTOFF MESSAGE.   SO WE WILL ONLY DO THIS
      *          CHECK IF THERE IS AN OTHER BAL.  BARB & CINDY
      *
       SKIP-EARLY-PAYOFF-TEST.
           IF LN-OTHBAL = 0
              GO TO TEST-PAYOFF-ALLOWED.

           IF ( ( CD-BR-ALLOT-OPTION = "Y"          ) AND
                ( HOLD-BP-ALLOW-PMT-ACCT-OTHBAL = "Y" ) )
              OR
              ( ( CD-BR-LBOX-OPTION = "Y"             ) AND
                ( HOLD-LBOX-ALLOW-PMT-ACCT-OTHBAL = "Y" ) )
              IF BP-TRAMT > (VALID-POSTING-NOPOFF - LN-OTHBAL)
                 MOVE "AMT > POFF - .01 WITH OTH" TO LOG-MSG
                 MOVE 51                 TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 GO TO END-ROUTINE.

      * TEST IF PAYOFF IS ALLOWED:

