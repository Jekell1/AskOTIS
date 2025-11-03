       LCAP-A-C-BEGIN.

      * PAYMENT CONSUMED ?
           IF LCAP-TRAMT = 0
              GO TO LCAP-A-C-EXIT.

      * LOAN BALANCE 0 ? (THIS IMPLIES ASSESSMENT AFTER MATURITY)
           IF SP-EXPR-FRMLA-CONT
              IF LCAP-LCPAID NOT > 0 OR
                 LCAP-AL-OK = "N"
                 GO TO LCAP-A-C-EXIT.

      * PAST MATURITY ?
           PERFORM LCAP-MDTE-CHK.
           IF NOT SP-EXPR-FRMLA-CONT
              IF ELAPSED-DAYS NOT > 0
                 GO TO LCAP-A-C-EXIT.

      * LCAP-LPTRCD = AL, IF WHERE LCPDTH IS PAID WILL NOT CAUSE
      *                      AN ASSESSMENT, EXIT
           IF LCAP-LPTRCD = "AL"
              PERFORM LCAP-ASSESS-CHK
              IF LCAS-ASSESS NOT = "Y"
                 GO TO LCAP-A-C-EXIT.

      * CHECK FOR PARTIALS
           IF LCAP-TRAMT < LCAP-AMTNEEDED
              MOVE LCAP-TRAMT TO LCAP-WRK
           ELSE
              MOVE LCAP-AMTNEEDED TO LCAP-WRK.

      * TEST FOR OVER LOAN BALANCE:
           IF LCAP-WRK > LCAP-LCPAID
              GO TO LCAP-A-AL-OK-TEST.

           PERFORM LCAP-ASSESS-CHK.

      * DOES PAYMENT AND LCPDTH INDICATE AN ASSESSMENT ?
           IF LCAS-ASSESS NOT = "Y"
              GO TO LCAP-A-C-PMT.

           IF SP-LCFRMLA = "C"
              IF LCAP-AMTNEEDED NOT = LCAP-WRK
                 MOVE 0 TO LCAP-PARTIALS
                           LCHG-CHARGE
                 GO TO LCAP-A-C-PMT
              ELSE
      * LCHG-LATE INITIALIZED FOR SP-LCTYPE = "C" OR "J"
      * SINCE LATE CHARGE IS BASED ON % OF DELINQUENT AMOUNT
                 MOVE LCAP-AMTNEEDED TO LCHG-LATE
                 MOVE LCAP-LCPDTH-DATE TO LCHG-LCPDTH-DATE
                 PERFORM LCAP-CALC-TEST
                 MOVE 0 TO LCAP-PARTIALS
                 GO TO LCAP-A-ACCUM.

      * FOR FORMULA "A" CALCULATE LCHG:
      * ALSO IF ON ALLOTMENT OR LOCK BOX PLAN OR POSTING WITH A BATCH REFCD
      * TEST IF LATE CHARGES SHOULD BE CHARGED:

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
              COMPUTE LCHG-CHARGE ROUNDED =
                 ((SP-LCRATE / 100) / 30)
                        * LCAS-ELAPSED-DAYS * LCAP-WRK
              IF LCAP-AMTNEEDED = LCAP-WRK
                 COMPUTE LCAP-WRK2 = LCHG-CHARGE + LCAP-PARTIALS
                 IF LCAP-WRK2 < SP-LCMINCHG
                    SUBTRACT LCAP-PARTIALS FROM SP-LCMINCHG
                    GIVING LCHG-CHARGE.

           IF LCHG-CHARGE > SP-LCMAXAMT
              MOVE SP-LCMAXAMT TO LCHG-CHARGE.

           IF LCAP-AMTNEEDED = LCAP-WRK
              MOVE 0 TO LCAP-PARTIALS
              ADD 1 TO LCAP-NOLCHG
           ELSE
              ADD LCHG-CHARGE TO LCAP-PARTIALS.

