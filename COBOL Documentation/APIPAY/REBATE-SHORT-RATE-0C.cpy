      *****************************************************
      *                     (0 C)  NEWPORT  90%
      *                     (0 K)  MULLEN   95% AND USES INSEFF DATE
      *     COLLATERAL PROTECTION INSURANCE
      *     SHORT RATE REBATE CANCELLATION FORMULA
      *
      *****************************************************
       REBATE-SHORT-RATE-0C SECTION.

      * "K" NEEDS TO WORK FOR INSURANCES WRITTEN FOR DIFFERENT TERMS
           IF SP-RBFRMLA2(REB-SUB) = "K"
              MOVE REB-LN-INSEFF TO NDTE-DATE
              MOVE REB-ORGTERM   TO NDTE-HOLD
           ELSE
              MOVE REB-LN-LOANDATE TO NDTE-DATE
              MOVE REB-LN-ORGTERM  TO NDTE-HOLD.
           PERFORM INCREMENT-UNITPER.

      *   DETERMINE DAYS IN TERM:
           MOVE NDTE-DATE       TO SYS-DATE.
           IF SP-RBFRMLA2(REB-SUB) = "K"
              MOVE REB-LN-INSEFF TO NUM-DATE
           ELSE
              MOVE REB-LN-LOANDATE TO NUM-DATE.
           PERFORM TIM365.
           MOVE ELAPSED-DAYS    TO REBX-DAYS-IN-TERM.

           IF REB-SUB < 5
              MOVE 1 TO REB-INSSUB
           ELSE
           IF REB-SUB = 5
              MOVE 2 TO REB-INSSUB
           ELSE
           IF REB-SUB = 6 OR 11
              MOVE 3 TO REB-INSSUB
           ELSE
           IF REB-SUB > 10 AND < 17
              SUBTRACT 6 FROM REB-SUB GIVING REB-INSSUB.

      *   DETERMINE DAYS IN FORCE:
           IF SP-RBFRMLA2(REB-SUB) = "K"
              MOVE REB-LN-INSEFF TO NUM-DATE
           ELSE
              MOVE REB-LN-LOANDATE TO NUM-DATE.
           MOVE REB-PAYDATE     TO SYS-DATE.
           PERFORM TIM365.
           MOVE ELAPSED-DAYS    TO REBX-DAYS-IN-FORCE.

      * POLICY FEE IS NOT INCLUDED IN LN-INSPREM OR REB-TOTCHG
      * NET CONTRACT COST:
           MOVE REB-TOTCHG TO REBX-NET-CONTRACT-COST.

      * DETERMINE DAILY COST:
           IF SP-RBSPOPT1(REB-SUB) = 23
              COMPUTE REBX-DAILY-COST-NO-ROUND ROUNDED =
                 REBX-NET-CONTRACT-COST / REBX-DAYS-IN-TERM
           ELSE
              COMPUTE REBX-DAILY-COST ROUNDED =
                  REBX-NET-CONTRACT-COST / REBX-DAYS-IN-TERM.

      * DETERMINE EARNED COST:

           IF SP-RBSPOPT1(REB-SUB) = 23
              COMPUTE REBX-EARNED-COST-NO-ROUND ROUNDED =
               REBX-DAYS-IN-FORCE * REBX-DAILY-COST-NO-ROUND
           ELSE
              COMPUTE REBX-EARNED-COST ROUNDED =
               REBX-DAYS-IN-FORCE * REBX-DAILY-COST.

      * DETERMINE PRO-RATA REFUND:
           IF SP-RBSPOPT1(REB-SUB) = 23
              COMPUTE REBX-PRO-RATA-REFUND-NO-ROUND ROUNDED =
                REBX-NET-CONTRACT-COST - REBX-EARNED-COST-NO-ROUND
           ELSE
              COMPUTE REBX-PRO-RATA-REFUND =
                REBX-NET-CONTRACT-COST - REBX-EARNED-COST.

      * DETERMINE SHORT RATE REFUND 90% OF PRORATA:
           IF SP-RBFRMLA2(REB-SUB) = "K"
              IF SP-RBSPOPT1(REB-SUB) = 23
                 COMPUTE REBX-SHORT-RATE-REFUND-NO-ROUND ROUNDED =
                  REBX-PRO-RATA-REFUND-NO-ROUND * 0.95
              ELSE
                 COMPUTE REBX-SHORT-RATE-REFUND ROUNDED =
                  REBX-PRO-RATA-REFUND * 0.95
           ELSE
              IF SP-RBSPOPT1(REB-SUB) = 23
                 COMPUTE REBX-SHORT-RATE-REFUND-NO-ROUND ROUNDED =
                  REBX-PRO-RATA-REFUND-NO-ROUND * 0.90
              ELSE
                 COMPUTE REBX-SHORT-RATE-REFUND ROUNDED =
                  REBX-PRO-RATA-REFUND * 0.90.

           IF SP-RBSPOPT1(REB-SUB) = 23
              MOVE REBX-SHORT-RATE-REFUND-NO-ROUND TO REB-REBATE
           ELSE
              MOVE REBX-SHORT-RATE-REFUND TO REB-REBATE.

       REBATE-SHORT-RATE-0C-EXIT.
           EXIT.

      ****************************************************************
      *                     (0 D)
      *    REGIONAL ACCEPTANCE
      *    RULE 78THS USING DAYS IN TERM AND ELAPSED DAYS
      *    INSTEAD OF UNIT PERIODS IN TERM AND ELAPSED UNIT PERIODS
      *
      ****************************************************************
