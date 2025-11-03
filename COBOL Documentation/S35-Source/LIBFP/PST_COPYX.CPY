      * COPYMEMBER: LIBFP/PST.COPYX
      ******************************************************************
      *
      * IDENTIFICATION:  LIBFP/PST.COPYX
      *
      * DESCRIPTION   :  LASER (PST) FORM COPY LIBRARY
      *                                              PARAGRAPHS/SECTIONS
      *                  - WITH PAYOFF/REFUNDS
      *
      *=================================================================
      * REV:
      * KEC 2011.1027 ***NEW*** POSTSCRIPT
      * BAH 2018.0718 CHANGED CRNORC TO CRNOR2
      *
      ******************************************************************
      *
      *    S E T U P - L I N K - I N F O
      *
      ******************************************************************
       SETUP-LINK-INFO SECTION.

           MOVE LONPM-HELP--FILE           TO HELP-LINK-FILE.
           MOVE LONPM-HELP--DIR            TO HELP-LINK-DIR.

      *=================================================================
       SETUP-LINK-INFO-EXIT.
           EXIT.

      ******************************************************************
      *
      *    V D U - C O N V E R T - F I E L D
      *
      ******************************************************************
       VDU-CONVERT-FIELD SECTION.

           MOVE STAT---GOOD                TO STAT.
           MOVE VDU-FIELD-TO-CONVERT       TO VDU-ALP-FIELD.

           EVALUATE TRUE
              COPY "LIBLP/LONPM_EVA.CPY".
              COPY "LIBLP/CHCKI_EVA.CPY".
              COPY "LIBLP/BCHKI_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *=================================================================
       VDU-CONVERT-FIELD-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P C L - C O P Y - L I B R A R Y - P A R A G R A P H S
      *
      ******************************************************************
       PST-COPY-MISC-PARAGRAPHS SECTION.
      *=================================================================
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".

      *=================================================================
       PST-COPY-MISC-CRNORC.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD              TO CRNOR2-SYSDATE.
           MOVE LN-ACCTNO                  TO CRNOR2-ACCTNO.
           MOVE "CL/CRNOR2"                TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CRNOR2-LINK NO-REC
                                 POFF-WORKERS LN-REC.
           CANCEL FORM-PROGX.

      *=================================================================
       PST-COPY-MISC-PARA-EXIT.
           EXIT.
      ******************************************************************
      *
      *    P S T - C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/RBACT.CPY".
       COPY "LIBGB/REBATE.CPY".
       COPY "LIBGB/SAFECK.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/SPPDTH.CPY".

       COPY "LIBLP/ADDONSPR.CPY".
       COPY "LIBLP/DEFPOL.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/LPAVCR.CPY".
       COPY "LIBLP/LPBAGE.CPY".
       COPY "LIBLP/LPCDEL.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBLP/LPDEF.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPLCAP.CPY".
       COPY "LIBLP/LPLCAS.CPY".
       COPY "LIBLP/LPLCHG.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPNOCL.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBLP/LPPOFF.CPY".
       COPY "LIBLP/LPPOF2.CPY".
       COPY "LIBLP/LPRATE.CPY".
       COPY "LIBLP/LPSCHD.CPY".
       COPY "LIBLP/LPTDEL.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBLP/LPWDEL.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBLP/SETCTBL.CPY".

       COPY "LIBSP/WORDS.CPY".

      ******************************************************************
      *
      *    P C L - C O P Y - X T R A - R O U T I N E
      *
      *=================================================================
      *    DESC:  GET EXTRA INFORMATION (LP-REC/NO-REC).
      ******************************************************************
       PST-COPY-XTRA-ROUTINE SECTION.

           PERFORM PST-COPY-NOTICE-ROUTINE.
           PERFORM PST-COPY-PAYOFF-REFUND.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NOTE: NEED TO SET MATURITY DATE CALCULATION AFTER THE
      *          REFUND/REBATE ROUTINE (PST-COPY-PAYOFF-REFUND).
      *          REFUND/REBATE ROUTINE USES THE MDTE-DATE IN ITS
      *          CALCULATIONS.  SO MUST SET MDTE-DATE AFTER THE
      *          REFUND/REBATE ROUTINE TO GET CORRECT MATURITY DATE.
      *
      *    NOTE: NEED TO SET LOAN CALCULATION ROUTINE AFTER THE
      *          REFUND/REBATE ROUTINE (PST-COPY-PAYOFF-REFUND).
      *          REFUND/REBATE ROUTINE USES THE LOAN-CALCULATION ROUTINE
      *          SEVERAL TIMES.  SO MUST SET MDTE-DATE AFTER THE
      *          REFUND/REBATE ROUTINE TO GET CORRECT MATURITY DATE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM LOAN-CALCULATIONS.
           PERFORM MATURITY-DATE-CALCULATION.

      *=================================================================
       PST-COPY-XTRA-ROUTINE-EXIT.
           EXIT.
      ******************************************************************
      *
      *    P C L - C O P Y - N O T I C E - R O U T I N E
      *
      *=================================================================
      *    IN  :  LN-REC
      *           TRANS-DATE
      *    OUT :  NO-REC
      ******************************************************************
       PST-COPY-NOTICE-ROUTINE SECTION.

           MOVE ZEROES                     TO LP-SSNO.
           MOVE LN-ACCTNO     TO CRNOR2-ACCTNO.
           MOVE TRANS-DATE    TO CRNOR2-SYSDATE.
           MOVE "CL/CRNOR2"   TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CRNOR2-LINK NO-REC
                                 POFF-WORKERS LN-REC.
           CANCEL FORM-PROGX.

       PST-COPY-NOTICE-R-EXIT.
           EXIT.
      ******************************************************************
      *
      *    P C L - C O P Y - P A Y O F F - R E F U N D
      *
      *=================================================================
      *    IN  :  LN-ACCTNO
      *    OUT :  PAYOFF-*
      *           REFUND-*
      *    DESC:  GET EXTRA INFORMATION (LP-REC).
      *           - PAYOFF(PRIOR LOAN) & REFUNDS
      *           - REFUNDS
      ******************************************************************
       PST-COPY-PAYOFF-REFUND SECTION.

           MOVE ZEROES TO PAYOFF-AMT-BALANCE
                          PAYOFF-AMT-APP-LATECHG
                          PAYOFF-AMT-PB          PAYOFF-AMT-PO
                          PAYOFF-AMT-RB          PAYOFF-AMT-RN
                          PAYOFF-AMT-RO          PAYOFF-AMT-SC.

           MOVE ZEROES TO REFUND-AMT-BALANCE
                          REFUND-AMT-RA          REFUND-AMT-RC
                          REFUND-AMT-RI          REFUND-AMT-RM
                          REFUND-AMT-RP          REFUND-AMT-RS
                          REFUND-AMT-R1          REFUND-AMT-R2
                          REFUND-AMT-R3          REFUND-AMT-OT-VSI.

           MOVE SPACES TO REFUND-MANUAL-RA       REFUND-MANUAL-RC
                          REFUND-MANUAL-RI       REFUND-MANUAL-RM
                          REFUND-MANUAL-RP       REFUND-MANUAL-RS
                          REFUND-MANUAL-R1       REFUND-MANUAL-R2
                          REFUND-MANUAL-R3       REFUND-MANUAL-OT-VSI.

           IF ( LN-PRLNNO = ZEROES )
              GO TO PST-COPY-P-R-EXIT.

           IF ( LN-POCD-INRESCISSION )
              GO TO PST-COPY-P-R-RESCISSION.

           PERFORM OPEN-LP1-FILE.

           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LN-PRLNNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE ALL "9"       TO LP-SEQNO
                                 QLP1-WEND-SEQNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.

           PERFORM START-LP1-FILE-NOT-GREATER.

       PST-COPY-P-R-LP-NEXT.

           PERFORM READ-LP1-FILE-PREVIOUS.
           IF ( IO-BAD                    ) OR
              ( LP-BRNO NOT = LN-OWNBR    ) OR
              ( LP-ACCTNO NOT = LN-PRLNNO )
              GO TO PST-COPY-P-R-EXIT.

           IF ( LP-REV = "Y" )
              GO TO PST-COPY-P-R-LP-NEXT.

      *- - - - - - - - - - - - - - - - - - - - -{ APPLIED LATE CHARGES }
           IF ( LP-TRCD = "PB" ) OR
              ( LP-TRCD = "PO" ) OR
              ( LP-TRCD = "RB" ) OR
              ( LP-TRCD = "RN" ) OR
              ( LP-TRCD = "RO" ) OR
              ( LP-TRCD = "SC" )
              ADD LP-APLC TO PAYOFF-AMT-APP-LATECHG.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - -{ PAYOFFS  }
           IF ( LP-TRCD = "PB" )
              ADD LP-TRAMT TO PAYOFF-AMT-PB   PAYOFF-AMT-BALANCE
           ELSE
           IF ( LP-TRCD = "PO" )
              ADD LP-TRAMT TO PAYOFF-AMT-PO   PAYOFF-AMT-BALANCE
           ELSE
           IF ( LP-TRCD = "RB" )
              ADD LP-TRAMT TO PAYOFF-AMT-RB   PAYOFF-AMT-BALANCE
           ELSE
           IF ( LP-TRCD = "RN" )
              ADD LP-TRAMT TO PAYOFF-AMT-RN   PAYOFF-AMT-BALANCE
           ELSE
           IF ( LP-TRCD = "RO" )
              ADD LP-TRAMT TO PAYOFF-AMT-RO   PAYOFF-AMT-BALANCE
           ELSE
           IF ( LP-TRCD = "SC" )
              ADD LP-TRAMT TO PAYOFF-AMT-SC   PAYOFF-AMT-BALANCE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - -{ REFUNDS  }
      *    'OT' = (VSI) VEHICLE SERVICE AGREEMENT
      *    'RA' = (AH)  ACCIDENT & HEALTH
      *    'RC' = (CL)  CREDIT LIFE
      *    'RI' = (INT) INTEREST
      *    'RM' = (MF)  MAINTENANCE FEE
      *    'RP' = (PP)  PERSONAL PROPERTY
      *    'RS' = (SV)  SERVICE CHARGE
      *    'R1' = (O1)  OTHER 1 INS.
      *    'R2' = (O2)  OTHER 2 INS.
      *    'R3' = (O3)  OTHER 3 INS.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( LP-TRCD = "OT" ) AND ( LP-REFNO = "VSI  ")
              ADD  LP-TRAMT     TO REFUND-AMT-OT-VSI
              MOVE LP-MANUAL-RB TO REFUND-MANUAL-OT-VSI
           ELSE
           IF ( LP-TRCD = "RA" )
              ADD  LP-TRAMT     TO REFUND-AMT-RA    REFUND-AMT-BALANCE
              MOVE LP-MANUAL-RB TO REFUND-MANUAL-RA
           ELSE
           IF ( LP-TRCD = "RC" )
              ADD  LP-TRAMT     TO REFUND-AMT-RC    REFUND-AMT-BALANCE
              MOVE LP-MANUAL-RB TO REFUND-MANUAL-RC
           ELSE
           IF ( LP-TRCD = "RI" )
              ADD  LP-TRAMT     TO REFUND-AMT-RI    REFUND-AMT-BALANCE
              MOVE LP-MANUAL-RB TO REFUND-MANUAL-RI
           ELSE
           IF ( LP-TRCD = "RM" )
              ADD  LP-TRAMT     TO REFUND-AMT-RM    REFUND-AMT-BALANCE
              MOVE LP-MANUAL-RB TO REFUND-MANUAL-RM
           ELSE
           IF ( LP-TRCD = "RP" )
              ADD  LP-TRAMT     TO REFUND-AMT-RP    REFUND-AMT-BALANCE
              MOVE LP-MANUAL-RB TO REFUND-MANUAL-RP
           ELSE
           IF ( LP-TRCD = "RS" )
              ADD  LP-TRAMT     TO REFUND-AMT-RS    REFUND-AMT-BALANCE
              MOVE LP-MANUAL-RB TO REFUND-MANUAL-RS
           ELSE
           IF ( LP-TRCD = "R1" )
              ADD  LP-TRAMT     TO REFUND-AMT-R1    REFUND-AMT-BALANCE
              MOVE LP-MANUAL-RB TO REFUND-MANUAL-R1
           ELSE
           IF ( LP-TRCD = "R2" )
              ADD  LP-TRAMT     TO REFUND-AMT-R2    REFUND-AMT-BALANCE
              MOVE LP-MANUAL-RB TO REFUND-MANUAL-R2
           ELSE
           IF ( LP-TRCD = "R3" )
              ADD  LP-TRAMT     TO REFUND-AMT-R3    REFUND-AMT-BALANCE
              MOVE LP-MANUAL-RB TO REFUND-MANUAL-R3.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           GO TO PST-COPY-P-R-LP-NEXT.

      *=================================================================
      *    MUST USE THE NEW LOANS INTDATE FOR PAYOFF REBATES TO BE
      *    CORRECT IF THE LOAN IS IN RESCISSION
      *=================================================================
       PST-COPY-P-R-RESCISSION.

           MOVE LN-INTDATE               TO POFF-PAYDATE.

           MOVE LN-REC                   TO PST-INFO-ORIGINAL-RECORD-LN.
           MOVE SP-REC                   TO PST-INFO-ORIGINAL-RECORD-SP.

      *- - - - - - - - - - - - - - - - - - - - - - - -{ GET PRIOR LOAN }
           MOVE LN-PRLNNO                TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.

      *- - - - - - - - - - - - - - - - - - - - -{ GET PRIOR LOAN'S SPR }
           MOVE LN-ORGST                 TO SP-ORGST.
           MOVE LN-SPRCLASS              TO SP-SPRCLASS.
           MOVE LN-SUBCLASS              TO SP-SUBCLASS.
           MOVE LN-LAWCODE               TO SP-LAWCODE.
           PERFORM READ-SP1-FILE.

      *- - - - - - - - - - - - - - - -{ GET PRIOR LOAN'S PAYOFF/REFUND }
      *KC->MOVE LN-INTDATE               TO POFF-PAYDATE.
           MOVE LN-SRCD                  TO POFF-LPTRCD.
           MOVE "M"                      TO POFF-MAKERCD.
           PERFORM PAYOFF-LOAN-ROUTINE.

           MOVE POFF-REBATE(1)           TO REFUND-AMT-CL.
           MOVE POFF-REBATE(2)           TO REFUND-AMT-AH.
           MOVE POFF-REBATE(3)           TO REFUND-AMT-PP.
           MOVE POFF-REBATE(4)           TO REFUND-AMT-INT.
           MOVE POFF-REBATE(9)           TO REFUND-AMT-O1.
           MOVE POFF-NETDUE              TO PAYOFF-AMT-BALANCE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE PST-INFO-ORIGINAL-RECORD-LN TO LN-REC.
           MOVE PST-INFO-ORIGINAL-RECORD-SP TO SP-REC.

      *=================================================================
       PST-COPY-P-R-EXIT.
           EXIT.
