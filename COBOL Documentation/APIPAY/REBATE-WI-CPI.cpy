      ***********************************************
      *              ( 0 8 )
      *    WISCONSIN CPI INSURANCE REFUND FORMULA
      ***********************************************
       REBATE-WI-CPI SECTION.
    
           COMPUTE REB-UPR-WHOLE ROUNDED =
                REB-TOTCHG * (365 - REB-ELAPSED-DAYS) / 366.
           MOVE REB-UPR-WHOLE TO REB-REBATE.
    
           IF REB-REBATE < 0
              MOVE 0 TO REB-REBATE.
    
       REBATE-WI-CPI-EXIT.
           EXIT.

      ***********************************************
      *              ( 0 9 )
      *    TEXAS INTEREST REFUND FORMULA
      ***********************************************
