      ****************************************************
      * TEST IF THE ACCOUNT IS ON AN ALLOTMENT PLAN
      *         OR POSTING WITH A BATCH REFCD
      * AND SHOULD LATE CHARGES SHOULD NOT BE CHARGED:
      ****************************************************
       LCAP-CALC-TEST SECTION.
           IF ( ( LCAP-BATCH-REFCD NOT = " "        ) AND
                ( LCAP-BATCH-REFCD NOT = LOW-VALUES ) AND
                ( LCAP-BATCH-REFCD-LC-FG NOT = "Y"  ) AND
                ( ( LCAP-LPTRCD = "PA" ) OR
                  ( LCAP-POFF-LPTRCD = "PO" AND LCAP-LPTRCD = "AL" )
                )
              )
             OR
              ( ( ( LCAP-BATCH-REFCD = " "          ) OR
                  ( LCAP-BATCH-REFCD = LOW-VALUES   )
                )
               AND
                ( LN-ALLOTCD = "Y"                   ) AND
                ( BR-LATECHG-ON-ALLOTMENTS NOT = "Y" ) AND
                ( ( LCAP-LPTRCD = "PA" ) OR
                  ( LCAP-POFF-LPTRCD = "PO" AND LCAP-LPTRCD = "AL" )
                )
              )
             OR
              ( ( ( LCAP-BATCH-REFCD = " "          ) OR
                  ( LCAP-BATCH-REFCD = LOW-VALUES   )
                )
               AND
                ( LN-LBOX = "Y"                ) AND
                ( BR-LATECHG-ON-LBOX NOT = "Y" ) AND
                ( ( LCAP-LPTRCD = "PA" ) OR
                  ( LCAP-POFF-LPTRCD = "PO" AND LCAP-LPTRCD = "AL" )
                )
              )
             OR
              ( ( LCAP-POFF-LPTRCD = "PB" OR "RN" OR
                                     "SC" OR "RB" OR "RO" ) AND
                ( SP-RBSPOPT1(7) = 19                     ) AND
                ( LCAP-NOLCHG    = 2                      )
              )
              MOVE 0 TO LCHG-CHARGE
           ELSE
              PERFORM LATE-CHARGE-CALCULATION
              ADD 1 TO LCAP-NOLCHG.

      ****************************************************
      * DETERMINE WHICH PAID THRU CALCULATION TO PERFORM
      ****************************************************
