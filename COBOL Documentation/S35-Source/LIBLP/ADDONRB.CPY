      * COPYMEMBER: LIBLP/ADDONRB

      ************************************************************************
      *         COMMON LOGIC TO SETUP FOR ADDON INTEREST REBATE
      *
      *   NAME: ADDONRB
      *   DESC: COMMON LOGIC TO SETUP FOR ADDON INTEREST REBATE
      *
      *   USED: USED IN LPPOF2, LONPFA.C, LONPFB.C
      *   COPY: ADDONRBW
      *
      *REV:
      * JTG 000522 MODIFIED TO CALCULATE REB-LN-LASTPYAMT AND REB-ORGTERM
      *                                                             LIBERAL, NJ
      * JTG 030212 ADDONS CANCELLED VIA ADDON MODULE DO INCORRECT ADDITIONAL
      *            REFUND ON THE 'LT' INTEREST
      *
      *            CHANGED VALUES OF LT-INS-REBATE FLAG SUCH THAT WHEN:
      *              LT-INS-REBATE = " "  NO REFUND OR CANCEL OCCURED
      *                            = "Y"  INS. WAS CANCELLED VIA ADDON POSTING
      *                            = "R"  INS. WAS REBATED   VIA TRANS POSTING
      *                                                               MULL #382
      *  JTG 030305 ALLOW RBFRMLA '8' TO WORK FOR 'AH' & 'ALL COMPUTABLE INS'
      *                                                         PACESETTER #2012
      *  JTG 040913 ADDED NEW FIELDS FOR SP-RBFRMLA (0 D)
      *             RULE 78THS USING DAYS IN TERM AND ELAPSED DAYS INSTEAD
      *             OF UNIT PERIODS IN TERM AND ELAPSE UNIT PERIODS REGACC #0074
      *  160113 BAH SPLIT OUT LTFILE, LTIFILE, RENAMED LT- LTI- PD0003
      *************************************************************************
       ADDONRB-GET-INT-REBATE SECTION.
           IF LTI-INSPREM < 0
              MOVE -1 TO POFF-ONE
           ELSE
              MOVE +1 TO POFF-ONE.

      ************************************************************************
      *      IF SP-CANCEL-ADDON-FG = ' '
      *         - WHEN THE INSURANCE IS CANCELLED VIA ADDON POSTING
      *           EX.'CNL03', THE CANCEL GENERATES THE INTEREST ADJUSTMENT
      *           BY DETERMINING A RULE OF 78THS REBATE AS OF THE CANCEL
      *           DATE AND WE MUST SKIP THE REBATED 'LT INS. RECORD'.
      *         - WHEN THE INSURANCE IS REBATED VIA PAYMENT POSTING
      *           EX. 'R3'  NO INTEREST ADJUSTMENT IS MADE.
      *           THIS ADDON RECORD MUST BE CONSIDERED IN THE INTEREST
      *           ADJUSTMENT AND NOT SKIPPED.
      *
      *      IF SP-CANCEL-ADDON-FG = 'F'
      *         - WHEN THE INSURANCE IS CANCELLED VIA ADDON POSTING
      *           EX. 'CNL03', THE CANCEL GENERATES THE INTEREST ADJUSTMENT
      *           BY DETERMINING THE AMOUNT REQUIRED TO FINANCE THE
      *           PREMIUM CANCELLED OVER THE UNUSED TERM.
      *           THIS ADDON RECORD MUST BE CONSIDERED IN THE INTEREST
      *           ADJUSTMENT AND NOT SKIPPED.
      *         - WHEN THE INSURANCE IS REBATED VIA PAYMENT POSTING
      *           EX. 'R3' NO INTEREST ADJUSTMENT IS MADE.
      *           THIS ADDON RECORD MUST BE CONSIDERED IN THE INTEREST
      *           ADJUSTMENT AND NOT SKIPPED.
      *
      *     WHEN:
      *           LT-INS-REBATE = " "    NO REFUND OR CANCEL OCCURED
      *                         = "Y"    INS. WAS CANCELLED VIA ADDON POSTING
      *                         = "R"    INS. WAS REBATED   VIA TRANS POSTING
      *                                                                MULL #382
      ************************************************************************
           IF SP-CANCEL-ADDON-FG = " "
              MOVE 0 TO REB-REBATE
              IF LTI-INS-REBATE = "Y"
                 GO TO ADDONRB-GET-INT-REBATE-EXIT
              ELSE
              IF NOT (LTI-INT-REBATE = " " OR "C")
                 GO TO ADDONRB-GET-INT-REBATE-EXIT.

           SUBTRACT LTI-PRIOR-NODEF FROM LN-TOTNODEF
                                     GIVING REB-LN-TOTNODEF.
           MOVE LTI-ORIG-1STPYDATE       TO REB-LN-ORIG-1STPYDATE.
           MOVE LTI-ANTIC-START-DATE TO REB-LN-LOANDATE
                                       REB-LN-PURDATE
                                       REB-LN-INTDATE.
           MOVE LTI-INSEFF-DATE           TO REB-LN-INSEFF.
           MOVE LTI-INSEXP-DATE           TO REB-LN-INSEXP.

           MOVE 0                    TO REB-LN-INSCOVR.
           MOVE "N"                  TO REB-LN-SALEFIN.

           MOVE 7                    TO REB-SUB.

           MOVE LTI-ANTICTERM        TO REB-ORGTERM
                                       REB-LN-ORGTERM.
           MULTIPLY POFF-ONE BY LTI-INS-PAYMNT-INCR
                                GIVING REB-LN-1STPYAMT
                                       REB-LN-REGPYAMT.

      *  JTG 000522 MODIFIED TO CALCULATE REB-LN-LASTPYAMT AND REB-ORGTERM
      *                                                            LIBERAL, NJ
           DIVIDE LTI-INS-TOT-ADDON BY LTI-INS-PAYMNT-INCR
                                           GIVING REB-ORGTERM
                                           REMAINDER REB-LN-LASTPYAMT.
           IF REB-LN-LASTPYAMT = 0
              MOVE REB-ORGTERM        TO REB-LN-ORGTERM
              MOVE LTI-INS-PAYMNT-INCR TO REB-LN-LASTPYAMT
           ELSE
              ADD 1 REB-ORGTERM GIVING REB-ORGTERM
                                       REB-LN-ORGTERM.

           MOVE LN-SMPRATE TO REB-LN-APRATE
                              REB-LN-SMPRATE.
           MULTIPLY POFF-ONE BY LTI-ANTICERN(1) GIVING REB-TOTCHG.

           IF SP-RBFRMLA(7) = "2" OR "7" OR "B"
                                   OR "C" OR "D" OR "F"
              IF LTI-TRCD = "NP" OR "LP"
                 MOVE 1 TO REB-ORGTERM REB-LN-ORGTERM
              ELSE
                 IF LTI-TRCD = "ET"
                    COMPUTE REB-ORGTERM
                            REB-LN-ORGTERM =
                         LTI-INS-TOT-ADDON / LTI-INS-PAYMNT-INCR + 1
                    IF REB-ORGTERM = 1
                       MULTIPLY POFF-ONE BY LTI-INS-TOT-ADDON
                                         GIVING REB-LN-1STPYAMT
                                                 REB-LN-REGPYAMT
                                                  REB-LN-LASTPYAMT
                    END-IF
                 END-IF
              END-IF

      *      NOW COMPUTE RATE FOR ACTUARIAL REFUND ROUTINES:
              COMPUTE APR-FINANCED =
                      (LTI-INS-TOT-ADDON * POFF-ONE) - REB-TOTCHG
              MOVE REB-LN-INTDATE          TO APR-INTDATE
              MOVE REB-LN-ORIG-1STPYDATE      TO APR-1STPYDATE
              MOVE REB-LN-ORGTERM          TO APR-ORGTERM
              MOVE REB-LN-1STPYAMT         TO APR-1STPYAMT
              MOVE REB-LN-REGPYAMT         TO APR-REGPYAMT
              MOVE REB-LN-LASTPYAMT        TO APR-LASTPYAMT
              MOVE LN-UNITPER-CD           TO APR-UNITPER-CD
              MOVE LN-UNITPER-FREQ         TO APR-UNITPER-FREQ
              MOVE SP-ADDON-DISFRMLA       TO APR-DISFRMLA
              PERFORM APR-REGZ
              MOVE APR-APR                 TO REB-LN-APRATE
                                              REB-LN-SMPRATE.

           MOVE POFF-PAYDATE TO REB-PAYDATE.
           MOVE POFF-LPTRCD TO REB-LPTRCD.

           MOVE "Y" TO REB-ADDON-FG.

           MOVE SP-DISFRMLA TO POFF-SP-DISFRMLA.
           MOVE SP-ADDON-DISFRMLA TO SP-DISFRMLA.
           PERFORM REBATE.
           MOVE POFF-SP-DISFRMLA TO SP-DISFRMLA.

           IF REB-REBATE > REB-TOTCHG
              MOVE REB-TOTCHG TO REB-REBATE.

       ADDONRB-GET-INT-REBATE-EXIT.
           EXIT.
