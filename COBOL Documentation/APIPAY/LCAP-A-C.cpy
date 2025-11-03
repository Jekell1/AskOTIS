      ************************************************
      *   SP-LCFRMLA = A OR C
      ************************************************
       LCAP-A-C SECTION.

      * SET PMT-TO-DATE TEMP; ALSO SET LCAP-LCPAID AS TEMP
      * FOR LN-CURBAL (LCPAID IS NOT USED FOR THIS ROUTINE)

           ADD LCAP-TOTPAYMNTD LCAP-LPAPLC LCAP-LPAPINT
                                            GIVING LCAP-PAYMNTD.
           MOVE LN-CURBAL TO LCAP-LCPAID.
           MOVE "Y" TO LCAP-AL-OK.

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

       LCAP-A-ACCUM.
           ADD LCHG-CHARGE TO LCAP-PAYMNTD
                              LCAP-APP.
           SUBTRACT LCHG-CHARGE FROM LCAP-TRAMT
                                     LCAP-WRK.

       LCAP-A-C-PMT.

      * TEST FOR ASSESS AND 0 CURBAL REACHED:
           IF LCAP-LPTRCD = "AL"
              IF LCAP-LCPAID = 0
                 MOVE LCAP-PAYDATE TO LCAP-LCPDTH-DATE
                 GO TO LCAP-A-C-EXIT.

      * REDUCE LOAN BALANCE TEMP STORED IN LCAP-LCPAID
      * USED WHEN LATE CHARGES ARE ASSESSED BEYOND MATURITY. SEE ABOVE
           SUBTRACT LCAP-WRK FROM LCAP-LCPAID
                                  LCAP-TRAMT.
           ADD LCAP-WRK TO LCAP-PAYMNTD.
           MOVE LCAP-PAYMNTD TO PDTH-PAYMNTD.

      * COMPUTE LCPDTH BASED ON TOTAL PMTS (PMTS + LATE CHGS)
           PERFORM LCAP-CALC-PDTH.

      * IF LCPDTH AS COMPUTED WILL ADVANCE, UPDATE.
      * (THIS IS TO PROVIDE FOR CODES SUCH AS "AL" WHICH
      * ADVANCE LCPDTH AS IF ACTUAL PMTS WERE MADE, EVEN
      * THOUGH LCAP-TOTPAYMNTD WILL ONLY INCLUDE THE LATE CHARGE)

           MOVE LCAP-LCPDTH-DATE    TO NUM-DATE.
           MOVE 1                   TO NUM-DA.
           MOVE PDTH-DATE-FULL      TO SYS-DATE.
           MOVE 1                   TO S-DD.
           PERFORM TIM365.
           IF ELAPSED-DAYS > 0
              PERFORM LCAP-A-AL-OK-TEST
              IF LCAP-AL-OK = "Y"
                 MOVE PDTH-DATE-FULL TO LCAP-LCPDTH-DATE.

           MOVE LN-REGPYAMT TO LCAP-AMTNEEDED.
           GO TO LCAP-A-C-BEGIN.

      * IF SP-LCFRMLA = "A" WE MUST NOT ADVANCE LATE CHARGE PAID THRU
      * TO THE MONTH & YEAR OF THE "AL" DATE, IF THE ACCESS DATE IS
      * PRIOR TO THE NORMAL PAY DAY OF THAT MONTH.
      
       LCAP-A-AL-OK-TEST.
           MOVE "Y" TO LCAP-AL-OK.
           IF LCAP-LPTRCD = "AL"
              IF SP-LCFRMLA = "A"
                 MOVE LCAP-PAYDATE TO NUM-DATE
                 IF PDTH-WK-CCYY = NUM-CCYY
                    IF PDTH-WK-MM = NUM-MO
                       MOVE LCAP-1STPYDATE TO SYS-DATE
                       IF NUM-DA < S-DD
                          MOVE "N" TO LCAP-AL-OK.
       LCAP-A-C-EXIT.
           EXIT.

      ***********************************************
      *  SP-LCFRMLA = B OR J:
      *     "B" == "J" WHICH USES LCPAID
      ***********************************************
