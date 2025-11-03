      *****************************************************
      *    CALIFORNIA A & H ANTICIPATED     (L)
      *    MULLEN FINANCE
      *
      *****************************************************
       REBATE-CA-AH-ANTICIPATED SECTION.
           IF REB-ELAPSED-DAYS < 11
              MOVE REB-TOTCHG TO REB-REBATE
              GO TO REBATE-CA-AH-ANTICIPATED-EXIT.
           MOVE REB-AHSUB TO REB-INSSUB.
           PERFORM REBATE-FIND-INS-RATE.

      * COMPUTE NEW COVERAGE ON REMAINING TERM:
           IF SP-RBSPOPT1(REB-SUB) = 11
              COMPUTE REB-BAL ROUNDED =
                 REB-TOTCHG * REB-REMTERM * REB-HUNDREDS
                       / REB-ORGTERM
              MOVE REB-REMTERM TO REB-WORKER
              MOVE REB-ORGTERM TO REB-REMTERM
              PERFORM REBATE-FIND-INS-RATE
              MOVE REB-WORKER TO REB-REMTERM
              COMPUTE REB-UPR ROUNDED =
                            REB-BAL / REB-HUNDREDS
           ELSE
           IF SP-RBSPOPT1(REB-SUB) = 10
              COMPUTE REB-UPR ROUNDED =
                           (REB-LN-INSCOVR / REB-ORGTERM)
                                   * REB-REMTERM / 100
              COMPUTE REB-UPR = REB-UPR * REB-HUNDREDS
           ELSE
              COMPUTE REB-UPR-WHOLE ROUNDED =
                           (REB-LN-INSCOVR / REB-ORGTERM)
                                   * REB-REMTERM / 100 + 0.99
              MOVE REB-UPR-WHOLE TO REB-UPR
              COMPUTE REB-UPR = REB-UPR * REB-HUNDREDS.

           COMPUTE REB-WFAC = (.0000045573 * (REB-ORGTERM ** 2))
                            + (-.001125 * REB-ORGTERM) + .099375.

      * COMPUTE REFUND CHARGE FACTOR:
           IF REB-ORGTERM < 14
              MOVE 0 TO REB-RCF.

           IF REB-ORGTERM > 13 AND < 49
              COMPUTE REB-RCF1 ROUNDED =
                 (1 - (REB-ORGTERM - REB-REMTERM - 1) /
                                              (REB-ORGTERM - 13))
              IF REB-RCF1 < 0
                 MOVE 0 TO REB-RCF
              ELSE
                 COMPUTE REB-RCF ROUNDED = REB-WFAC * REB-RCF1.

           IF REB-ORGTERM > 48
              COMPUTE REB-RCF1 ROUNDED =
                 1 - (REB-ORGTERM - REB-REMTERM - 1) / 35
              IF REB-RCF1 < 0
                 MOVE 0 TO REB-RCF
              ELSE
                 COMPUTE REB-RCF ROUNDED = REB-WFAC * REB-RCF1.


           COMPUTE REB-REBATE ROUNDED =
                                   REB-UPR * (1 - REB-RCF).

       REBATE-CA-AH-ANTICIPATED-EXIT.
           EXIT.

      ***********************************************
      *    PRO RATA, EXACT DAYS REBATE METHOD  (J)
      ***********************************************
