      **************************************************
       SETUP-LONPF-BUFFER SECTION.
      **************************************************
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
      * ADDED 10/15/2021, WRITE ERROR TRPFILE LONPF2
      * CONVERSION FAILED WHEN CONVERTING DATE (22007) 
                     HOLD-PRIOR-DF-POSTDATE.

           IF BP-TRCD = "SS"
              MOVE BP-AUCTION-FEES TO HOLD-REPO-AUCTION-EXP
              MOVE BP-AUCTION-NAME TO HOLD-REPO-AUCTION-NAME.

           IF BP-TRCD = "PL" OR "P2" OR "P3"
              MOVE BP-PL-REASON TO HOLD-PLREASON.

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

           MOVE LN-ACCTNO TO LOAN-KEY.
           MOVE LN-SSNO(1) TO BORROWER-KEY.
           IF BP-TRCD = "OT"
              MOVE BP-TRAMT TO LP-INTDUE.
           PERFORM SAVE-SCREEN.

      ******************************************************************
