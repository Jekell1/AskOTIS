      * TEST IF PAYOFF IS ALLOWED:
       TEST-PAYOFF-ALLOWED.
           IF ( (CD-BR-ALLOT-OPTION = "Y"  ) AND
                (BR-BP-NOPAYOFF NOT = "Y"  ) )
              OR
              ( (CD-BR-LBOX-OPTION = "Y"   ) AND
                (BR-LBOX-NOPAYOFF NOT = "Y") )
              IF BP-TRAMT = POFF-NETDUE
                 MOVE "ACCOUNT WILL PAYOFF" TO LOG-MSG
                 MOVE 52                 TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 GO TO END-ROUTINE.

           IF ( (CD-BR-ALLOT-OPTION = "Y") AND
                (BR-BP-LC NOT = "Y"      ) )
              OR
              ( (CD-BR-LBOX-OPTION = "Y" ) AND
                (BR-LBOX-LC NOT = "Y"    ) )
              MOVE BP-PAYDATE TO IBPC-DATE
              PERFORM IBPC-TEST
              IF (IBPC-FG = "P" OR LN-LOANTYPE = "I")
                                        AND (LN-JDDATE = 0)
                                         AND (LN-ACCELDATE = 0)
                 MOVE BP-TRAMT     TO LCAP-TRAMT
                 MOVE BP-PAYDATE   TO LCAP-PAYDATE
                 MOVE LN-1STPYDATE TO LCAP-1STPYDATE
                 MOVE "PY"         TO LCAP-LPTRCD
                 MOVE 0            TO LCAP-LPAPLC LCAP-LPAPINT
                 MOVE CD-CODE      TO LCAP-BATCH-REFCD
                 MOVE CD-BR-LC-FG  TO LCAP-BATCH-REFCD-LC-FG
                 PERFORM LATE-CHARGE-APPLY
                 COMPUTE TEST-AMT  = LCAP-APP + LCAP-OWE - LN-LCBAL
                 IF TEST-AMT NOT = 0
                    MOVE "LATE CHARGE IS REQUIRED" TO LOG-MSG
                    MOVE 53                 TO RETURN-STATUS
                    PERFORM CREATE-LOG
                    GO TO END-ROUTINE.

      * TEST IF PAYMENT WILL ONLY COVER LC AND INTEREST:

           IF ( (CD-BR-ALLOT-OPTION = "Y") AND
                (BR-BP-NOPRIN NOT = "Y"  ) )
              OR
              ( (CD-BR-LBOX-OPTION = "Y" ) AND
                (BR-LBOX-NOPRIN NOT = "Y") )
              COMPUTE TEST-AMT = POFF-INTDUE + POFF-LCDUE
              IF BP-TRAMT NOT > TEST-AMT
                 MOVE "PAY APPLIES WITH NO PRIN" TO LOG-MSG
                 MOVE 28                 TO RETURN-STATUS
                 PERFORM CREATE-LOG
                 GO TO END-ROUTINE.

      * TEST IF PAYMENT WILL CAUSE NEGATIVE CURRENT BALANCE:
      *
      * WHEN OT2BAL EXISTS, INSURE AT LEAST ($0.01 + LN-OT2BAL)
      * REMAINS IN LN-CURBAL:

           COMPUTE TEST-AMT = BP-TRAMT - POFF-INTDUE - POFF-LCDUE.
           IF LN-OT2BAL NOT = 0
              IF TEST-AMT >= (LN-CURBAL - TOT-POFF-REBATES)
                 IF ( (CD-BR-ALLOT-OPTION = "Y"    ) AND
                      (HOLD-BP-PAYOFF-NONCASH = "Y"  ) )
                    OR
                    ( (CD-BR-LBOX-OPTION = "Y"     ) AND
                      (HOLD-LBOX-PAYOFF-NONCASH = "Y") )
                    MOVE "99" TO HOLD-BP-TRCD
                 ELSE
                    MOVE "PAY EXCEEDS OTHER 2 BAL." TO LOG-MSG
                    MOVE 55                 TO RETURN-STATUS
                    PERFORM CREATE-LOG
                    GO TO END-ROUTINE
                 END-IF
              END-IF
           ELSE
      * WHEN NO OT2BAL, DON'T LET LN-CURBAL GO NEGATIVE:
              IF TEST-AMT > LN-CURBAL
                 IF ( (CD-BR-ALLOT-OPTION = "Y"    ) AND
                      (HOLD-BP-PAYOFF-NONCASH = "Y"  )
                    )
                    OR
                    ( (CD-BR-LBOX-OPTION = "Y"     ) AND
                      (HOLD-LBOX-PAYOFF-NONCASH = "Y")
                    )
                    MOVE "99" TO HOLD-BP-TRCD
                 ELSE
                    MOVE "PAY EXCEEDS CURBAL" TO LOG-MSG
                    MOVE 56                 TO RETURN-STATUS
                    PERFORM CREATE-LOG
                    GO TO END-ROUTINE.

      * 5/4/12:  IF THERE'S AN O2 BALANCE, IF IT'S NOT ENOUGH TO
      *          PAYOFF, BUT MORE THAN (CURBAL - O2BAL), THEN
      *          REJECT TO HANDLE MANUALLY.  FOR EXAMPLE, LN-CURBAL=70,
      *          20 OF THAT IS LN-OT2BAL.  BP-TRAMT = 50, REBATES = 15.
      *          SO TEST-AMT (50) IS LESS THAN CURBAL-REBATES (55),
      *          BUT > OR = THE BALANCE LESS O2 (50).  THE PROBLEM
      *          HERE IS THERE'S NOT ENOUGH TO PAYOFF(WOULD NEED 55),
      *          BUT WOULD NEED TO APPLY SOME OR ALL TO O2 BALANCE,
      *          AND STILL LEAVE AT LEAST 1 CENT IN BALANCE.  BETTER
      *          TO REQUIRE MANUAL POSTING.  PL 771.    BARB & CINDY

           IF LN-OT2BAL NOT = 0
              COMPUTE TEST-AMT = BP-TRAMT - POFF-INTDUE - POFF-LCDUE
              IF TEST-AMT < (LN-CURBAL - TOT-POFF-REBATES)
                 IF TEST-AMT >= (LN-CURBAL - LN-OT2BAL)
                    MOVE "REQUIRES MANUAL Z2" TO LOG-MSG
                    MOVE 57                 TO RETURN-STATUS
                    PERFORM CREATE-LOG
                    GO TO END-ROUTINE.

