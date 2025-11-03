      ***********************************************
      *              ( 0 A )
      *    MEAN OF PRORATA AND RULE 78THS
      ***********************************************
       REBATE-MEAN-OF-PRORATA-78 SECTION.
   
           COMPUTE REB-REBATE ROUNDED = REB-TOTCHG
                * ( (REB-ORGTERM - REB-ELAPSED-MONTHS)
                    * (2 * REB-ORGTERM - REB-ELAPSED-MONTHS + 2)
                  )
                  / ((2 * REB-ORGTERM) * (REB-ORGTERM + 1))
   
           IF REB-REBATE < 0
              MOVE 0 TO REB-REBATE.
   
       REBATE-MEAN-OF-PRORATA-78-EXIT.
           EXIT.

      ***********************************************
      *              ( 0 B )
      *    RULE 78THS FOR NET + 1 LIFE REFUND
      ***********************************************
