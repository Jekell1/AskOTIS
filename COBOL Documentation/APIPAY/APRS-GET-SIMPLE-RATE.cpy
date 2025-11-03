      *******************************************
      *    COMPUTE THE SIMPLE INTEREST RATE
      *******************************************
       APRS-GET-SIMPLE-RATE SECTION.
           MOVE LN-UNITPER-CD   TO APR-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO APR-UNITPER-FREQ.
           MOVE LN-INTDATE      TO APR-INTDATE.
           MOVE LN-1STPYDATE    TO APR-1STPYDATE.

      * COMPUTE TRUE INTEREST CHARGABLE:
           MOVE 0 TO APRS-PYAMT-ADJ.
           IF SP-CAL-CHGABLE(CTBL) = "P" OR "S"
              SUBTRACT TOTAL-INSPREM-NONMEMO
                       TOTAL-INS-TAX
                       FROM APRS-PYAMT-ADJ.
           IF SP-CAL-CHGABLE(CTBL) = "P" OR "I"
              SUBTRACT LN-SERCHG FROM APRS-PYAMT-ADJ.

      *----------------------------------------------------------------------
      * IF MEXICO VAT TAX THEN USE LN-ANTICERN(4), THE LN-MAINTFEE FIELD IS
      * TO SMALL AND THE NUMBER THERE MAY BE TRUNCATED        (WORLD #651)
      *----------------------------------------------------------------------
           IF SP-MFFRMLA-05
              COMPUTE APR-FINANCED =
                   LN-LNAMT - LN-ANTICERN(4) + APRS-PYAMT-ADJ
           ELSE
              COMPUTE APR-FINANCED =
                   LN-LNAMT - (LN-MAINTFEE * LN-ORGTERM)
                            + APRS-PYAMT-ADJ.

           IF LN-LOANTYPE = "P"
              SUBTRACT LN-INTCHG LN-EXTCHG FROM APR-FINANCED.

      * SET INTEREST CHARGABLE AMOUNT:
           MOVE APR-FINANCED TO APRS-INTCHGABLE.

           MOVE LN-ORGTERM TO APR-ORGTERM.

      * DERIVE PAYMENT AMOUNTS FROM TRUE INTEREST CHARGABLE:

           COMPUTE APRS-PYAMT-ADJ =
                   APRS-PYAMT-ADJ / LN-ORGTERM.

           MOVE LN-1STPYAMT   TO APR-1STPYAMT.
           MOVE LN-REGPYAMT   TO APR-REGPYAMT.
           MOVE LN-LASTPYAMT  TO APR-LASTPYAMT.

      *----------------------------------------------------------------------
      * REESTABLISH PAYMENTS PRIOR TO SPREAD OF LN-EXTCHG
      * PER SP-PYEXFRMLA-SAME2-SPREAD (12)
      *----------------------------------------------------------------------
           IF SP-PYEXFRMLA-SAME2-SPREAD
              DIVIDE LN-ORGTERM INTO LN-EXTCHG
                           GIVING APRS-SPREAD
                           REMAINDER APRS-REM
              ADD LN-EXTCHG TO APR-1STPYAMT
              SUBTRACT APRS-SPREAD FROM APR-1STPYAMT
                                        APR-REGPYAMT
              IF LN-ORGTERM NOT = 1
                 SUBTRACT APRS-SPREAD APRS-REM FROM APR-LASTPYAMT.
      *----------------------------------------------------------------------

      *----------------------------------------------------------------------
      *  IF MAINT FORMULA 5 THEM USE LN-ANTICERN(4),
      *  LN-MAINTFEE MAY BE TRUNCATED
      *----------------------------------------------------------------------
           IF SP-MFFRMLA-05
              COMPUTE APR-1STPYAMT =
                            APR-1STPYAMT - LN-EXTCHG
                                         - (LN-ANTICERN(4) / LN-ORGTERM)
                                         + APRS-PYAMT-ADJ
              COMPUTE APR-REGPYAMT
                      APRS-REGPYAMT =
                            APR-REGPYAMT - (LN-ANTICERN(4) / LN-ORGTERM)
                                        + APRS-PYAMT-ADJ
              IF LN-ORGTERM NOT = 1
                 COMPUTE APR-LASTPYAMT =
                     APR-LASTPYAMT - (LN-ANTICERN(4) / LN-ORGTERM)
                                         + APRS-PYAMT-ADJ
              END-IF
           ELSE
              COMPUTE APR-1STPYAMT =
                            APR-1STPYAMT - LN-EXTCHG - LN-MAINTFEE
                                         + APRS-PYAMT-ADJ
              COMPUTE APR-REGPYAMT
                      APRS-REGPYAMT =
                            APR-REGPYAMT - LN-MAINTFEE
                                        + APRS-PYAMT-ADJ
              IF LN-ORGTERM NOT = 1
                 COMPUTE APR-LASTPYAMT =
                     APR-LASTPYAMT - LN-MAINTFEE
                                         + APRS-PYAMT-ADJ.

      *----------------------------------------------------------------------
      * IF 1ST PAYMENT EXTENTION CHARGE FORMULA '1' INDICATING SPREAD
      *        OF 1ST PAYMENT EXTENTION PERIOD CHARGE OVER ALL PAYMENTS
      *          OR WHEN APR-1STPYAMT NOT = APR-REGPYAMT WHICH COULD OCCUR
      *             IN SALES FINANCE INPUT WHICH ALLOWES SETUP OF PAYMENT
      *             STREAM WITH NO RHYME OR REASON
      *    GET TRUE REGZ CALCULATION
      * ELSE
      *    GET REVISED APR FOR EVEN PAYMENT SCHEDULE AND EVEN PAYMENT AMOUNT
      *----------------------------------------------------------------------
           IF (SP-PYEXFRMLA-REG-INCR) OR
              (APR-1STPYAMT NOT = APR-REGPYAMT)
              IF SP-DISFRMLA-USRULE
                 MOVE "C" TO APR-DISFRMLA
              ELSE
                 MOVE "A" TO APR-DISFRMLA
              END-IF
           ELSE
              MOVE "X" TO APR-DISFRMLA.
      *----------------------------------------------------------------------

           PERFORM APR-REGZ.
           COMPUTE APRS-SMPRATE ROUNDED = APR-APR.

      *******************************************
      *    COMPUTE EFFECTIVE RATE
      *******************************************
