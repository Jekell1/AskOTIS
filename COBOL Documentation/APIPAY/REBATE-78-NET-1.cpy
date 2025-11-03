      ***********************************************
      *              ( 0 B )
      *    RULE 78THS FOR NET + 1 LIFE REFUND
      ***********************************************
       REBATE-78-NET-1 SECTION.
   
           COMPUTE REB-REBATE ROUNDED = REB-TOTCHG
                * ( (REB-ORGTERM - REB-ELAPSED-MONTHS)
                    * (REB-ORGTERM - REB-ELAPSED-MONTHS + 1)
                    + (REB-ORGTERM - REB-ELAPSED-MONTHS)
                  )
                  / (REB-ORGTERM * (REB-ORGTERM + 1) + REB-ORGTERM)
   
           IF REB-REBATE < 0
              MOVE 0 TO REB-REBATE.
   
       REBATE-78-NET-1-EXIT.
           EXIT.

      ***********************************************
      *    GET EXTENSION DAYS
      ***********************************************
