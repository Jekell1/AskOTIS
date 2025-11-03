      ******************************************************************
       REVERSAL-POSTING SECTION.
      ******************************************************************

      * SOME LOGIC FROM SETUP-LONPF-BUFFER:

           MOVE 0   TO ENTRY-FLAG BYPASS-AMT-ENT-FG.
           MOVE "F" TO FULL-DISPLAY.
           MOVE " " TO REV-REBATE FULL-PAGE-FG ENTER-AMT-FG
                       HOLD-DISPLAY-ERN
                       HOLD1-KEY-DOWN HOLD2-KEY-UP.

           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.

           MOVE 0 TO HOLD-PARTIALS
                     HOLD-LCPAID
                     HOLD-DLPARVHELD-ADJ
                     HOLD-AFPDLLP
                     HOLD-OVPAID-CHECK
                     HOLD-REPO-AUCTION-EXP
                     ADDON-INT-REBATE
                     ADDON-INT-EARNED
                     HOLD-PRIOR-DF-POSTDATE.

           MOVE LN-TOTINT     TO HOLD-LN-TOTINT.
           MOVE LN-TOTERN     TO HOLD-LN-TOTERN.
           MOVE LN-TOTDEF     TO HOLD-LN-TOTDFMNTS.
           MOVE LN-TOTLCHG    TO HOLD-LN-TOTLCHG.
           MOVE LN-TOTSERACCT TO HOLD-LN-TOTSERACCT.
           MOVE LN-JDRATE     TO HOLD-LN-JDRATE.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 13
              MOVE 0 TO REB-AMOUNT(SUB)
              IF SUB < 7
                 MOVE LN-ACCERN(SUB) TO HOLD-LN-ACCERN(SUB)
              END-IF
              IF SUB < 9
                 MOVE " " TO INS-RB-TBL(SUB)
              END-IF
           END-PERFORM.

      * RESTORE LOAN TO ITS ORIG STATE ELSE WILL ERROR OUT
      * OF LONPF2 (LN-REC == WK-SAVE-REC)
      
           MOVE ORIG-LN-LBOX    TO LN-LBOX.
           MOVE ORIG-LN-ALLOTCD TO LN-ALLOTCD.

           MOVE LX-REC  TO WK-LX-REC.
           MOVE LXE-REC TO WK-LXE-REC.
           MOVE LXG-REC TO WK-LXG-REC.
           PERFORM CLOSE-LP1-FILE.

           PERFORM READ-EARNINGS.

      * GET THE GL FOR THIS REVERSAL
           PERFORM OPEN-LXG1-FILE.
           MOVE LP-BRNO   TO LXG-BRNO.
           MOVE LP-ACCTNO TO LXG-ACCTNO.
           MOVE LP-SEQNO  TO LXG-SEQNO.
           PERFORM READ-LXG1-FILE.
           PERFORM CLOSE-LXG1-FILE.
           IF IO-FG NOT = 0
              MOVE 0 TO LXG-GLNO(1)  LXG-GLNO(2)  LXG-GLNO(3)
                        LXG-GLAMT(1) LXG-GLAMT(2) LXG-GLAMT(3).

       REVERSAL-LP1-FILE.
           MOVE LP-TRCD  TO REV-TRCD.
           MOVE LP-REFNO TO REV-REFNO.

           ADD LN-INTBAL LP-APINT GIVING LP-INTBAL.
           ADD LN-OTHBAL LP-APOTH GIVING LP-OTHBAL.
           ADD LN-LCBAL LP-APLC GIVING LP-LCBAL.

           MOVE LN-CURBAL TO LP-CURBAL.

      * "OTWOAPJDPLPIPWRPTSTPSPBKBD" (NOT).
      *  PA NOT IN LIST SO WILL BE CDV-NO-MATCH
           MOVE CDV-17 TO CDV-BUF.
           PERFORM CDV-VERIFY.

      * THE "DF" HAS THE AMOUNT IN INTDUE SO INTBAL COMES OUT RIGHT:
           IF (CDV-NO-MATCH OR (REV-TRCD = "Z2"))
              IF ( REV-TRCD NOT = "WL" ) AND
                 ( SP-CAL-RATETYPE(1) NOT = "Z" ) AND
                 ( NOT (REV-TRCD = "RE" AND LN-LOANTYPE = "I") )
                   SUBTRACT LP-INTDUE FROM LP-INTBAL.

      * ADDON-POSTING, LP-APINTOWE HOLDS LN-INTPDTH BEFORE ADDON:
           IF (CDV-NO-MATCH OR (REV-TRCD = "Z2"))
              IF NOT ADDON-POSTING
                 SUBTRACT LP-APINTOWE FROM LP-LCBAL.

           MULTIPLY -1 BY
              LP-TRAMT LP-APOTH LP-APINT LP-APLC LP-APCUR
              LP-INTDUE LP-DLPROC LP-EARNED(1) LP-EARNED(2)
              LP-EARNED(3) LXG-GLAMT(1) LXG-GLAMT(2) LXG-GLAMT(3)
              LP-INTPAID LXE-EARN(1) LXE-EARN(2) LXE-EARN(3)
              LXE-EARN(4) LXE-EARN(5) LXE-EARN(6) LXE-EARN(7)
              LXE-WORKER LXE-WORKER2 LP-NOLCHG LP-PREPAY-PENALTY
              LP-PRORATED-INS-REB LP-EXTRA-PRIN-LFP LP-APPLIED-TO-ESCROW.

      * ADDON-POSTING, LP-APINTOWE HOLDS LN-INTPDTH BEFORE ADDON
           IF REV-TRCD NOT = "SP" AND NOT = "DC" AND
                        NOT ADDON-POSTING
              MULTIPLY -1 BY LP-APINTOWE.

           IF REB-AMOUNT(1) NOT = 0
              IF LN-ADDON-FG = "Y"
                 ADD LN-ANTICADJ(1) REB-AMOUNT(1)
                                         GIVING ADDON-INT-REBATE.

      * FOR PC ACCOUNTS, 'RI' MAY HAVE A VALUE:
           MOVE SPACES TO HOLD-DL-RVCHGS-CODE(1)
                          HOLD-DL-RVCHGS-CODE(2)
                          HOLD-DL-RVCHGS-CODE(3)
                          HOLD-DL-PARVHELD-CODE
                          HOLD-DL-PARVPAID-CODE.

           IF LN-SERVACCT NOT = "Y"
              PERFORM CALC-HOLD-LN-ACCERN.

           COMPUTE HOLD-LN-TOTERN =
                LN-TOTERN
                    + LP-EARNED(1) + LP-EARNED(2) + LP-EARNED(3)
                     + LXE-EARN(1) + LXE-EARN(2)  + LXE-EARN(3)
                     + LXE-EARN(4) + LXE-EARN(5)  + LXE-EARN(6)
                     + LXE-EARN(7).

           IF LN-SERVACCT NOT = "Y"
              IF NOT ADDON-POSTING
                  IF REV-TRCD NOT = "WI" AND NOT = "WL"
                     IF REV-TRCD NOT = "RD"
                        ADD LP-APINT LP-APLC TO HOLD-LN-TOTERN
                     ELSE
                        SUBTRACT LP-TRAMT FROM HOLD-LN-TOTERN.

           MOVE LN-TOTDEF TO HOLD-LN-TOTDFMNTS.
           IF DF-POSTING
              IF SP-DEFFRMLA = 97 OR 96 OR 95
                 COMPUTE HOLD-LN-TOTINT = LN-TOTINT + LP-APINT
              ELSE
                 COMPUTE HOLD-LN-TOTDFMNTS = LN-TOTDEF + LP-APINT
              END-IF
           ELSE
              IF REV-TRCD = "RD"
                 COMPUTE HOLD-LN-TOTDFMNTS = LN-TOTDEF - LP-TRAMT
              ELSE
                 IF REB-AMOUNT(7) NOT = 0
                   COMPUTE HOLD-LN-TOTDFMNTS = LN-TOTDEF - REB-AMOUNT(7)
                   SUBTRACT REB-AMOUNT(7) FROM HOLD-LN-TOTERN.

           IF NOT (DF-POSTING AND (SP-DEFFRMLA = 97 OR 96 OR 95))
              MOVE LN-TOTINT TO HOLD-LN-TOTINT.
           MOVE LN-TOTLCHG TO HOLD-LN-TOTLCHG.
           MOVE LN-TOTSERACCT TO HOLD-LN-TOTSERACCT.

      * PACESETTER DEFFRMLA 8 CAN HAVE LATE CHARGES
           IF REV-TRCD NOT = "WL"
              IF REVERSE-AFTER-RESCHED-FG = "N"
                 COMPUTE HOLD-LN-TOTLCHG = LN-TOTLCHG + LP-APLC.

           IF REV-TRCD NOT = "WI" AND
                        NOT DF-POSTING AND
                         NOT ADDON-POSTING
              COMPUTE HOLD-LN-TOTINT = LN-TOTINT + LP-APINT.

      * UCCC LATE CHARGES
      * CAN'T TAKE LATE CHARGE ON MONIES PAID WITHIN GRACE DAYS OF DUE DAY

           IF ( SP-LCFRMLA = "F" OR "H" OR "I" OR "J" OR "K"
                                 OR "L" OR "M" OR "P" OR "S" OR "T")
              IF REV-TRCD = "PY" OR "PA" OR "AL" OR "PE" OR "SS"
                         OR "PO" OR "PB" OR "SC" OR "RN" OR "IN"
                                 OR "RB" OR "RO" OR "PR" OR "PC"
                                 OR "PP" OR "AH" OR "PN" OR "PZ"
                 MOVE LP-LCPARTIALS TO HOLD-PARTIALS
                 MOVE LP-LCPAID TO HOLD-LCPAID.

           IF LN-SERVACCT NOT = "Y"
              GO TO BYPASS-SERVICE.

           ADD LP-EARNED(1) TO HOLD-LN-TOTSERACCT.

      * EARNED INTEREST
           ADD LP-EARNED(2) TO HOLD-LN-TOTSERACCT.
      * EARNED LCHG
           ADD LP-EARNED(3) TO HOLD-LN-TOTSERACCT.

       BYPASS-SERVICE.
           MOVE "RV" TO LP-TRCD.

      * LET REVERSAL 'RV' RECORD HAVE THE LP-PAYDATE OF THE
      * RECORD BEING REVERSED:
           MOVE TRANS-DATE TO LP-TRDATE LP-PAYDATE.

      ***********************************************************
      * LOGIC FROM BEGINNING OF DISPLAY-ENTRY-LINE IN LONPF8:
      ***********************************************************
           SUBTRACT LP-APCUR FROM LP-CURBAL.

      * CANT CHANGE LN-REC IN HERE! LONPF2 WILL EXIT IF LN-REC DOESNT
      * MATCH WK-SAVE-RECORD
           MOVE LN-TOTPAYMNTD TO HOLD-LN-TOTPAYMNTD.

      * UPDATE CONTRACTUAL PAYMENTS TO DATE:
           ADD LP-APCUR TO LN-TOTPAYMNTD
           IF SP-CAL-RATETYPE(1) = "Z"
              SUBTRACT LP-EXTRA-PRIN-LFP FROM LN-TOTPAYMNTD
              ADD LP-EXTRA-PRIN-LFP        TO LN-TOTEXCPAYMNTD.
           IF SP-CONTRFRMLA = "A"
              ADD LP-APLC TO LN-TOTPAYMNTD.
           ADD LP-APINT TO LN-TOTPAYMNTD.

      * COMPUTE PAID THRU DATE
           MOVE LN-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-DATE-FULL TO LP-PDTH-DATE.

      * CANT CHANGE LN-REC IN HERE! LONPF2 WILL EXIT IF LN-REC DOESNT
      * MATCH WK-SAVE-RECORD
           MOVE HOLD-LN-TOTPAYMNTD TO LN-TOTPAYMNTD.

           MOVE BP-TRCD TO HOLD-BP-TRCD LP-TRCD.
           MOVE BP-REFCD TO LP-REFNO HOLD-LP-REFNO.

      * PAYMENT UPDATE

           MOVE LN-ACCTNO TO LOAN-KEY.
           MOVE LN-SSNO(1) TO BORROWER-KEY.

       REVERSAL-SAVE-SCREEN.
           MOVE 3 TO WK-KEY.
           PERFORM OPEN-WK-FILE.

           MOVE 0 TO WK-ELE.

      * THIS DOESNT GET DONE IN LONPF8, BUT LONPF1 DOES SET IT, SO IT MUST
      * GET DONE HERE, WITH NO CHANGES MADE TO THE LN-REC IN THIS PROGRAM!
           MOVE LN-REC TO WK-SAVE-RECORD.
           MOVE LP-REC TO WK-LP-REC.
           MOVE LX-REC TO WK-LX-REC.
           MOVE LXE-REC TO WK-LXE-REC.
           MOVE LXG-REC TO WK-LXG-REC.
           PERFORM WRITE-WK-FILE.
           IF IO-FG NOT = 0
              PERFORM REWRITE-WK-FILE.
           PERFORM CLOSE-WK-FILE.

       CALL-LONPF2-FOR-REVERSAL.

           MOVE "LP/LONPF2" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME
                                 PROG-BUF  UPDATE-BUF.
           CANCEL FORM-PROGX.

      * TEST PROG-BUF (ERRCD) TO MAKE SURE LONPF2 DID NOT REJECT
      * THE PROPOSED TRANSACTION
      
           IF ERRCD = "E" OR "X" OR "M"
              MOVE "LONPF2: PMT NOT APPLIED  " TO LOG-MSG
              MOVE 69                 TO RETURN-STATUS
              PERFORM CREATE-LOG
              MOVE "E" TO POSTING-ERRCD.

       REVERSAL-RTN-EXIT.
           EXIT.


      **************************************************
