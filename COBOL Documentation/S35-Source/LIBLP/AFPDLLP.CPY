      * COPYMEMBER: LIBLP/AFPDLLP
      ******************************************************************
      *
      *         AFP ACCOUNTS DUE DEALER ROUTINE - FOR LP-FILE.
      *
      *   NAME: AFPDLLP
      *   DESC: THIS ROUTINE DETERMINES THE AMOUNT DUE THE
      *         DEALER ON AFP ACCOUNTS, FOR THE FOLLOWING
      *         TRANSACTIONS.
      *
      *   IN  : AFPDLLP-TODAY
      *
      *   OUT : AFPDLLP-APCUR-PERIOD
      *         AFPDLLP-DELINQ-PYMTS
      *         AFPDLLP-PER
      *         AFPDLLP-REMAINING-ADV
      *         AFPDLLP-TOT-PAYABLE
      *         AFPDLLP-APCUR-TD
      *         AFPDLLP-DUE-DEALER
      *         AFPDLLP-PRIOR-PAYABLE
      *         AFPDLLP-THIS-PAYABLE
      *         AFPDLLP-PROCESS
      *
      *   USED:
      *         LN-ANTICADJ(1); LN-ANTICADJ(5); LN-ANTICERN(1);
      *         LN-ANTICERN(5); LP-APCUR;      LN-LNAMT;
      *         LP-CURBAL;     LN-PROCEEDS;    LP-TRCD;
      *         LP-TRDATE.
      *
      * REV:
      *  JTG 051096 CHANGED P&L PER RELIANCE
      *  KEC 060496 FIXED CALCULATING DUE-DEALER FOR REVERSALS AND
      *             NON-REVERSAL TRANSACTION.
      *  GLF 033099 ADD TEST OF TRCD'S 'PN' AND 'PZ' FOR TFC
      *  BAH 021014 ADDED NEW TRANSACTION CODE OF "IN" INSURANCE LOSS, EXACTLY
      *             LIKE "PE", REGACC PR# 0007
      ******************************************************************
      *    A F P D L L P
      ******************************************************************
       AFPDLLP SECTION.

           MOVE "N" TO AFPDLLP-PROCESS.
           MOVE 0   TO AFPDLLP-DUE-DEALER.

           IF ( LN-LNAMT = 0 )
              GO TO AFPDLLP-EXIT.

           IF ( LP-APCUR = 0 )
              IF NOT ( (LP-TRCD = "PL" OR "P2" OR "P3"
                                        OR "P4" OR "P5")
                           OR
                       (LP-REVTRCD = "PL" OR "P2" OR "P3"
                                           OR "P4" OR "P5")
                               AND LP-TRCD = "RV"
                     )
                 GO TO AFPDLLP-EXIT.

           IF ( LN-PLCD = " " )
              IF (LP-TRCD = "PY" OR "PO" OR "PA" OR "PE" OR "IN"
                                 OR "SS" OR "AH" OR "PN" OR "PZ")
                 PERFORM AFPDLLP-NON-PL
                 GO TO AFPDLLP-EXIT
              ELSE
                 IF ( LP-TRCD = "RV" )
                    IF (LP-REVTRCD = "PY" OR "PO" OR "PA"
                                  OR "PE" OR "SS" OR "AH" OR "IN"
                                  OR "PN" OR "PZ")
                       PERFORM AFPDLLP-NON-PL
                       GO TO AFPDLLP-EXIT
                    END-IF
                    IF (LP-REVTRCD = "PL" OR "P2" OR "P3" OR "P4"
                                   OR "P5")
                       PERFORM AFPDLLP-PL
                       GO TO AFPDLLP-EXIT
                    ELSE
                       GO TO AFPDLLP-EXIT
                 ELSE
                    GO TO AFPDLLP-EXIT.

      * TRANSACTION IS BEFORE/PRIOR TO PLDATE
           IF ( LN-PLCD NOT = " " )
              IF LP-TRDATE > LN-PLDATE
                 GO TO AFPDLLP-EXIT
              ELSE
                 IF LP-TRDATE < LN-PLDATE
                    IF (LP-TRCD = "PY" OR "PO" OR "PA" OR "IN"
                                        OR "PE" OR "SS" OR "AH"
                                            OR "PN" OR "PZ")
                       PERFORM AFPDLLP-NON-PL
                       GO TO AFPDLLP-EXIT
                    END-IF
                    IF ( LP-TRCD = "RV" )
                       IF (LP-REVTRCD = "PY" OR "PO" OR "PA"  OR "IN"
                                              OR "PE" OR "SS" OR "AH"
                                                OR "PN" OR "PZ")
                          PERFORM AFPDLLP-NON-PL
                          GO TO AFPDLLP-EXIT
                       ELSE
                          GO TO AFPDLLP-EXIT
                    ELSE
                       NEXT SENTENCE
                 ELSE
      * TRANSACTION OCCURED ON P&L DATE:
                    IF LP-TRDATE = LN-PLDATE
                       IF NOT (LP-TRCD = "PL" OR "P2" OR "P3"
                                       OR "P4" OR "P5")
                          GO TO AFPDLLP-EXIT
                       ELSE
                          PERFORM AFPDLLP-PL.

       AFPDLLP-EXIT.
           EXIT.

      ******************************************************************
      *    A F P D L L P - N O N - P L
      ******************************************************************
       AFPDLLP-NON-PL SECTION.

           MOVE "Y" TO AFPDLLP-PROCESS.

           MOVE LP-APCUR TO AFPDLLP-APCUR-PERIOD.

           COMPUTE AFPDLLP-APCUR-TD = LN-LNAMT - LP-CURBAL.

           COMPUTE AFPDLLP-PER ROUNDED =
               1.0 - ( ( LN-ANTICERN(1) + LN-ANTICADJ(1) ) +
                       ( LN-ANTICERN(5) + LN-ANTICADJ(5) ) )
                   / LN-LNAMT.

           COMPUTE AFPDLLP-PRIOR-PAYABLE ROUNDED =
               ( AFPDLLP-APCUR-TD - AFPDLLP-APCUR-PERIOD )
               * AFPDLLP-PER.

           COMPUTE AFPDLLP-THIS-PAYABLE ROUNDED =
               AFPDLLP-APCUR-PERIOD * AFPDLLP-PER.

           COMPUTE AFPDLLP-TOT-PAYABLE =
               AFPDLLP-PRIOR-PAYABLE + AFPDLLP-THIS-PAYABLE.

           COMPUTE AFPDLLP-REMAINING-ADV =
               LN-PROCEEDS - AFPDLLP-TOT-PAYABLE.

      * FOR REVERSALS---COMPUTE AMOUNT DUE DEALER ON THIS TRANS.
      
           IF ( LP-TRCD = "RV" )
              COMPUTE AFPDLLP-REMAINING-ADV-RV =
                               LN-PROCEEDS - AFPDLLP-PRIOR-PAYABLE
              COMPUTE AFPDLLP-THIS-PAYABLE-RV = (LP-APCUR * (-1)) *
                                                 AFPDLLP-PER
              IF ( AFPDLLP-REMAINING-ADV-RV NOT < 0 )
                 MOVE 0 TO AFPDLLP-DUE-DEALER
              ELSE
      * REMAINING ADVANCE IS NEGATIVE
              IF (AFPDLLP-REMAINING-ADV-RV = AFPDLLP-THIS-PAYABLE-RV)
                 MOVE 0 TO AFPDLLP-DUE-DEALER
              ELSE
              IF ( AFPDLLP-THIS-PAYABLE-RV >
                 ( AFPDLLP-REMAINING-ADV-RV * -1 ) )
                 MOVE AFPDLLP-REMAINING-ADV-RV  TO AFPDLLP-DUE-DEALER
              ELSE
                 COMPUTE AFPDLLP-DUE-DEALER =
                                  AFPDLLP-THIS-PAYABLE-RV * (-1).

      * FOR NON-REVERSALS---COMPUTE AMOUNT DUE DEALER ON THIS TRANS.
      
           IF ( LP-TRCD NOT = "RV" )
              IF ( AFPDLLP-REMAINING-ADV NOT < 0 )
                 MOVE 0 TO AFPDLLP-DUE-DEALER
              ELSE
      * REMAINING ADVANCE IS NEGATIVE
              IF ( AFPDLLP-REMAINING-ADV = AFPDLLP-THIS-PAYABLE )
                 MOVE 0 TO AFPDLLP-DUE-DEALER
              ELSE
              IF ( AFPDLLP-THIS-PAYABLE > (AFPDLLP-REMAINING-ADV * -1) )
                 COMPUTE AFPDLLP-DUE-DEALER =
                                        AFPDLLP-REMAINING-ADV * -1
              ELSE
                 MOVE AFPDLLP-THIS-PAYABLE TO AFPDLLP-DUE-DEALER.

      * COMPUTE AMOUNT DELINQUENT

           PERFORM AFPDLLP-DELINQ.

       EXIT-AFPDLLP-NON-PL.
           EXIT.

      ******************************************************************
      *    A F P D L L P - P L
      ******************************************************************
       AFPDLLP-PL SECTION.

           MOVE "Y" TO AFPDLLP-PROCESS.

           COMPUTE AFPDLLP-APCUR-PERIOD = LP-INTDUE * (-1).

           COMPUTE AFPDLLP-APCUR-TD = LN-LNAMT - LP-CURBAL.

           COMPUTE AFPDLLP-PER ROUNDED =
               1.0 - ( LN-ANTICERN(1) + LN-ANTICADJ(1)
                          + LN-ANTICERN(5) + LN-ANTICADJ(5)
                     ) / LN-LNAMT.

           COMPUTE AFPDLLP-PRIOR-PAYABLE ROUNDED =
               AFPDLLP-APCUR-TD * AFPDLLP-PER.

           MOVE 0 TO AFPDLLP-THIS-PAYABLE.

           COMPUTE AFPDLLP-TOT-PAYABLE =
               AFPDLLP-PRIOR-PAYABLE + AFPDLLP-THIS-PAYABLE.

           COMPUTE AFPDLLP-REMAINING-ADV =
               LN-PROCEEDS - AFPDLLP-TOT-PAYABLE.

      * COMPUTE AMOUNT DUE DEALER ON THIS TRANSACTION
      
      * IF TOTAL PAYABLE EXCEEDS THE DEALER ORIGINAL ADVANCE
      * THAN NO PENALITY TO DEALER:

           IF AFPDLLP-TOT-PAYABLE > LN-PROCEEDS
              MOVE 0 TO AFPDLLP-DUE-DEALER
           ELSE
      * OTHERWISE PENALITY IS REMAINING ADVANCE:
              IF LP-TRCD = "RV"
                 MOVE AFPDLLP-REMAINING-ADV TO AFPDLLP-DUE-DEALER
              ELSE
                 COMPUTE AFPDLLP-DUE-DEALER =
                             AFPDLLP-REMAINING-ADV * -1.

      * SET AMOUNT DELINQUENT TO ZERO
      
           MOVE 0 TO AFPDLLP-DELINQ-PYMTS.

       EXIT-AFPDLLP-PL.
           EXIT.

      ******************************************************************
      *    COMPUTE AMOUNT DELINQUENT, EXCLUDING LATE CHARGES
      ******************************************************************
       AFPDLLP-DELINQ SECTION.

           MOVE LN-1STPYDATE TO CEPP1-DATE.
           MOVE AFPDLLP-TODAY TO CEPP2-DATE.
           PERFORM CONTRACTUAL-DELINQUENCY.

           IF ( CDEL-ARREARAGE < 0 )
              COMPUTE AFPDLLP-DELINQ-PYMTS =  CDEL-ARREARAGE  * -1
           ELSE
              MOVE 0 TO AFPDLLP-DELINQ-PYMTS.
