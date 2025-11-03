      * COPYMEMBER: LIBLP/PURFLDLN
      **************************************************************************
      *             (PURFLDLN)
      *    ROUTINES TO CALCULATE SALES FINANCE
      *             PURCHASE RELATED FIELDS LOAN VERSION
      *    NOTE:
      *         REMEMBER TO CHANGE 'PURFLDPD'
      *
      *         ALL LN FIELDS THAT ARE MODIFIED/CHANGED IN
      *         COMPUTE-PURCHASE-FIELDS ARE NOW IN LIBLP/PURFLDW.
      *
      * REV:
      * JTG 032596 ADDED DL-RVCHGS-FRMLA = 1, RELIANCE % OF INTCHG
      * MJD 032796 ADDING WORKER TO REPLCE LN-FIELDS THAT ARE CHANGED
      *            IN COMPUTE-PURCHASE-FIELDS SECTION.
      * JTG 032896 ADDED USE OF PURFLD.*-FG FIELDS
      * BAH 050897 ADDED PERFORM DEFAULT-APR TO DISC-FRMLA 2, AND PARV
      *            FORMULA'S 2, LIKE A50 !!
      * BAH 061697 ADDED RVPRIN-FRMLA 01 FOR FEDERATED, PR#508
      * BAH 022398 ADDED DL-DISC-FRMLA 5 FOR FFG, PR#653
      * BAH 040198 ADDED RVPRIN-FRMLA 02 FOR FUTURE, PR#677
      * JTG 061898 ADDED LOGIC FOR S.F. RENEWALS INTO S.F. MERCURY #E176
      * JTG 990322 REMOVED CLEAR OF ANTICERN(6), RE: LOAN COST LOGIC TFC #008
      * MJD 990817 ADDED PURFLD-STAT.
      * JTG 010507 ALLOWED ENTRY OF 'TRADEIN' RE: SALE PRICE-DP-TI PAACO PR#1467
      * JTG 020117 CHANGED FIELDS FROM S9(6)V99 TO S9(7)V99     LENDMARK PR#J021
      * JTG 030619 ADDED LOGIC FOR IB SALES FINANCE AMORTIZATED PARTICIPATION
      *            RESERVE PAID EXPENSE, DIFFERENCE BETWEEN A BUY RATE
      *            AND THE CONTRACT RATE                 WISCONSIN FINANCE #2049
      **************************************************************************
      *****************************************
      *   NAME:  COMPUTE-PURCHASE-FIELDS
      *   DESC:  COMPUTE ALL PURCHASE RELATED FIELDS
      *
      *   IN  :  ALL PURFLD       FIELDS
      *          ALL PURFLD--(FG) FIELDS
      *   OUT :  ALL PURFLD       FIELDS
      *****************************************
       COMPUTE-PURCHASE-FIELDS SECTION.
           IF LN-SERVACCT = "Y"
              GO TO COMPUTE-PURCHASE-FIELDS-EXIT.

           MOVE SPACES TO PURFLD-STAT.
           MOVE LN-DLNO TO DL-DLNO.
           PERFORM GET-DL1.
           IF IO-FG NOT = 0
              MOVE "DL" TO PURFLD-STAT
              GO TO COMPUTE-PURCHASE-FIELDS-EXIT.

           PERFORM COMPUTE-DLRVPRIN
               VARYING RVNO FROM 1 BY 1 UNTIL RVNO > 3.

           PERFORM COMPUTE-DLRVCHGS
               VARYING RVNO FROM 1 BY 1 UNTIL RVNO > 3.

           IF PURFLD-DLPARVHELD-FG = "C"
              MOVE 0 TO PURFLD-DLPARVHELD
              PERFORM COMPUTE-DLPARVHELD.

           IF PURFLD-DLPARVPAID-FG = "C"
              MOVE 0 TO PURFLD-DLPARVPAID
              IF LN-LOANTYPE = "I"
                 PERFORM COMPUTE-DLPARVPAID-IB
              ELSE
                 PERFORM COMPUTE-DLPARVPAID.

           IF PURFLD-DLDISC-FG = "C"
              MOVE 0 TO PURFLD-DLDISC
              PERFORM COMPUTE-DLDISC.

       COMPUTE-PURCHASE-FIELDS-EXIT.
           EXIT.

      **********************************************
      *   NAME:  COMPUTE-DLDISC
      *   DESC:  COMPUTE THE DEALER DISCOUNT
      *
      *   IN  :  PURFLD-DLDISC       LN-DLDISC
      *          PURFLD-DLDISC-FG    LN-DLDISC-FG
      *   OUT :  PURFLD-DLDISC
      **********************************************
       COMPUTE-DLDISC SECTION.
           MOVE 0 TO WORK-RATE.
           MOVE 6 TO SUB.
       COMPUTE-DLDISC-NEXT.
           IF LN-ORGTERM NOT > DL-DISC-TERM(SUB)
              MOVE DL-DISC-RATE(SUB) TO WORK-RATE.
           IF SUB > 1
              SUBTRACT 1 FROM SUB
              GO TO COMPUTE-DLDISC-NEXT.
           IF DL-DISC-FRMLA = 0
              COMPUTE PURFLD-DLDISC ROUNDED =
                    (LN-SELLPR - LN-DOWNPY - LN-TRADEIN)
                                      * (WORK-RATE / 100)
           ELSE
              IF DL-DISC-FRMLA = 1
                 COMPUTE PURFLD-DLDISC ROUNDED =
                       LN-SELLPR * (WORK-RATE / 100)
              ELSE
                 IF DL-DISC-FRMLA = 2
                    PERFORM DEFAULT-APR
                    IF LN-APRATE < WORK-RATE
                       MOVE LN-UNITPER-CD TO CALCPV-UNITPER-CD
                       MOVE LN-UNITPER-FREQ TO CALCPV-UNITPER-FREQ
                       MOVE WORK-RATE TO CALCPV-RATE
                       MOVE LN-ORGTERM TO CALCPV-TERM
                       MOVE LN-1STPYAMT TO CALCPV-1STPYAMT
                       MOVE LN-REGPYAMT TO CALCPV-REGPYAMT
                       MOVE LN-LASTPYAMT TO CALCPV-LASTPYAMT
                       MOVE LN-INTDATE TO CALCPV-INTDATE
                       MOVE LN-1STPYDATE TO CALCPV-1STPYDATE
                       MOVE "A" TO CALCPV-DISFRMLA
                       PERFORM CALCPV-AMT-FINANCED
                       IF APR-FINANCED > CALCPV-FINANCED
                           COMPUTE PURFLD-DLDISC =
                                   (APR-FINANCED - CALCPV-FINANCED)
                       ELSE
                          NEXT SENTENCE
                    ELSE
                       NEXT SENTENCE
                 ELSE
                    IF DL-DISC-FRMLA = 3
                       PERFORM LOAN-CALCULATIONS
                       COMPUTE PURFLD-DLDISC ROUNDED =
                           FINANCED-AMOUNT * (WORK-RATE / 100)
                    ELSE
                    IF DL-DISC-FRMLA = 4
                       PERFORM LOAN-CALCULATIONS
                       COMPUTE PURFLD-DLDISC ROUNDED =
                           TOTAL-REPAY * (WORK-RATE / 100)
                    ELSE
      * FFG 2/23/98
                    IF DL-DISC-FRMLA = 5
                       PERFORM LOAN-CALCULATIONS
                       COMPUTE PURFLD-DLDISC ROUNDED =
                           (FINANCED-AMOUNT - INSUR-OURS - FEES-OURS)
                                   * (WORK-RATE / 100).

      **********************************************
      *   NAME:  COMPUTE-DLRVPRIN
      *   DESC:  COMPUTE RESERVE FROM PRINCIPAL
      *
      *   IN  :  PURFLD-DLRVPRIN()     LN-DLRVPRIN()
      *          PURFLD-DLRVPRIN-FG()  LN-DLRVPRIN-FG()
      *          RVNO    (1-3)
      *   OUT :  PURFLD-DLRVPRIN()
      **********************************************
       COMPUTE-DLRVPRIN SECTION.
           IF PURFLD-DLRVPRIN-FG(RVNO) NOT = "C"
              GO TO COMPUTE-DLRVPRIN-EXIT.
           MOVE 0 TO PURFLD-DLRVPRIN(RVNO).

           MOVE 0 TO WORK-RATE.
           MOVE 6 TO SUB.
       COMPUTE-DLRVPRIN-NEXT.
           IF LN-ORGTERM NOT > DL-RVPRIN-TERM(RVNO  SUB)
              MOVE DL-RVPRIN-RATE(RVNO  SUB) TO WORK-RATE.
           IF SUB > 1
              SUBTRACT 1 FROM SUB
              GO TO COMPUTE-DLRVPRIN-NEXT.
           IF DL-RVPRIN-FRMLA(RVNO) = 0
              COMPUTE PURFLD-DLRVPRIN(RVNO) ROUNDED =
                 DL-RVPRIN-ADDON(RVNO) +
                    (LN-SELLPR - LN-DOWNPY - LN-TRADEIN)
                                      * (WORK-RATE / 100)
           ELSE
           IF DL-RVPRIN-FRMLA(RVNO) = 99
              COMPUTE PURFLD-DLRVPRIN(RVNO) = LN-LNAMT - LN-PURBAL
           ELSE
           IF DL-RVPRIN-FRMLA(RVNO) = 01
              PERFORM LOAN-CALCULATIONS
              COMPUTE PURFLD-DLRVPRIN(RVNO) ROUNDED =
                           FINANCED-AMOUNT * (WORK-RATE / 100)
           ELSE
           IF DL-RVPRIN-FRMLA(RVNO) = 02
      * FUTURE 4/01/98
              PERFORM LOAN-CALCULATIONS
              COMPUTE PURFLD-DLRVPRIN(RVNO) ROUNDED =
                           (FINANCED-AMOUNT - INSUR-OURS - FEES-OURS)
                                   * (WORK-RATE / 100).

       COMPUTE-DLRVPRIN-EXIT.
           EXIT.

      **********************************************
      *   NAME:  COMPUTE-DLRVCHGS
      *   DESC:  COMPUTE RESERVE FROM CHARGES
      *
      *   IN  :  PURFLD-DLRVCHGS()     LN-DLRVCHGS()
      *          PURFLD-DLRVCHGS-FG()  LN-DLRVCHGS-FG()
      *          RVNO    (1-3)
      *   OUT :  PURFLD-DLRVCHGS()
      **********************************************
       COMPUTE-DLRVCHGS SECTION.
           IF PURFLD-DLRVCHGS-FG(RVNO) NOT = "C"
              GO TO COMPUTE-DLRVCHGS-EXIT.
           MOVE 0 TO PURFLD-DLRVCHGS(RVNO).

           MOVE 0 TO WORK-RATE.
           MOVE 6 TO SUB.
       COMPUTE-DLRVCHGS-NEXT.
           IF LN-ORGTERM NOT > DL-RVCHGS-TERM(RVNO  SUB)
              MOVE DL-RVCHGS-RATE(RVNO  SUB) TO WORK-RATE.
           IF SUB > 1
              SUBTRACT 1 FROM SUB
              GO TO COMPUTE-DLRVCHGS-NEXT.
           IF DL-RVCHGS-FRMLA(RVNO) = 0
              COMPUTE PURFLD-DLRVCHGS(RVNO) ROUNDED =
                 DL-RVCHGS-ADDON(RVNO) +
                    (LN-SELLPR - LN-DOWNPY - LN-TRADEIN)
                                      * (WORK-RATE / 100)
           ELSE
              IF DL-RVCHGS-FRMLA(RVNO) = 1
                 COMPUTE PURFLD-DLRVCHGS(RVNO) ROUNDED =
                    DL-RVCHGS-ADDON(RVNO) +
                      LN-INTCHG * (WORK-RATE / 100)
              ELSE
                 IF DL-RVCHGS-FRMLA(RVNO) = 99
                    COMPUTE PURFLD-DLRVCHGS(RVNO) =
                                    LN-LNAMT - LN-PURBAL.

       COMPUTE-DLRVCHGS-EXIT.
           EXIT.

      *******************************************************
      *   NAME:  COMPUTE-DLPARVHELD
      *   DESC:  COMPUTE PARTICIPATION HELD
      *
      *   IN  :  PURFLD-DLPARVHELD       LN-DLPARVHELD
      *          PURFLD-DLPARVHELD-FG    LN-DLPARVHELD-FG
      *          RVNO    (1-3)
      *   OUT :  PURFLD-DLPARVHELD
      *******************************************************
       COMPUTE-DLPARVHELD SECTION.
           MOVE 0 TO WORK-RATE.
           MOVE 6 TO SUB.
       COMPUTE-DLPARVHELD-NEXT.
           IF LN-ORGTERM NOT > DL-PARVHELD-TERM(SUB)
              MOVE DL-PARVHELD-RATE(SUB) TO WORK-RATE.
           IF SUB > 1
              SUBTRACT 1 FROM SUB
              GO TO COMPUTE-DLPARVHELD-NEXT.

           IF DL-PARVHELD-FRMLA = 0
              COMPUTE PURFLD-DLPARVHELD ROUNDED = DL-PARVHELD-ADDON +
                    (LN-SELLPR - LN-DOWNPY - LN-TRADEIN)
                                      * (WORK-RATE / 100)
           ELSE
              IF DL-PARVHELD-FRMLA = 1
                 COMPUTE PURFLD-DLPARVHELD ROUNDED = DL-PARVHELD-ADDON +
                      LN-INTCHG * (WORK-RATE / 100)
              ELSE
                 IF DL-PARVHELD-FRMLA = 2
                    PERFORM DEFAULT-APR
                    IF LN-APRATE > WORK-RATE
                       MOVE LN-UNITPER-CD TO CALCPV-UNITPER-CD
                       MOVE LN-UNITPER-FREQ TO CALCPV-UNITPER-FREQ
                       MOVE WORK-RATE TO CALCPV-RATE
                       MOVE LN-ORGTERM TO CALCPV-TERM
                       MOVE LN-1STPYAMT TO CALCPV-1STPYAMT
                       MOVE LN-REGPYAMT TO CALCPV-REGPYAMT
                       MOVE LN-LASTPYAMT TO CALCPV-LASTPYAMT
                       MOVE LN-INTDATE TO CALCPV-INTDATE
                       MOVE LN-1STPYDATE TO CALCPV-1STPYDATE
                       MOVE "A" TO CALCPV-DISFRMLA
                       PERFORM CALCPV-AMT-FINANCED
                       IF APR-FINANCED < CALCPV-FINANCED
                           COMPUTE PURFLD-DLPARVHELD =
                                   (CALCPV-FINANCED - APR-FINANCED).

      **********************************************
      *   NAME:  COMPUTE-DLPARVPAID
      *   DESC:  COMPUTE PARTICIPATION PAID
      *
      *   IN  :  PURFLD-DLPARVPAID       LN-DLPARVPAID
      *          PURFLD-DLPARVPAID-FG    LN-DLPARVPAID-FG
      *   OUT :  PURFLD-DLPARVPAID
      *****************************************************
      **********************************************
       COMPUTE-DLPARVPAID SECTION.
           MOVE 0 TO WORK-RATE.
           MOVE 6 TO SUB.
       COMPUTE-DLPARVPAID-NEXT.
           IF LN-ORGTERM NOT > DL-PARVPAID-TERM(SUB)
              MOVE DL-PARVPAID-RATE(SUB) TO WORK-RATE.
           IF SUB > 1
              SUBTRACT 1 FROM SUB
              GO TO COMPUTE-DLPARVPAID-NEXT.

           IF DL-PARVPAID-FRMLA = 0
              COMPUTE PURFLD-DLPARVPAID ROUNDED = DL-PARVPAID-ADDON +
                    (LN-SELLPR - LN-DOWNPY - LN-TRADEIN)
                                      * (WORK-RATE / 100)
           ELSE
           IF DL-PARVPAID-FRMLA = 1
              COMPUTE PURFLD-DLPARVPAID ROUNDED = DL-PARVPAID-ADDON +
                   LN-INTCHG * (WORK-RATE / 100)
           ELSE
           IF DL-PARVPAID-FRMLA = 2
              PERFORM DEFAULT-APR
              IF LN-APRATE > WORK-RATE
                 MOVE LN-UNITPER-CD TO CALCPV-UNITPER-CD
                 MOVE LN-UNITPER-FREQ TO CALCPV-UNITPER-FREQ
                 MOVE WORK-RATE TO CALCPV-RATE
                 MOVE LN-ORGTERM TO CALCPV-TERM
                 MOVE LN-1STPYAMT TO CALCPV-1STPYAMT
                 MOVE LN-REGPYAMT TO CALCPV-REGPYAMT
                 MOVE LN-LASTPYAMT TO CALCPV-LASTPYAMT
                 MOVE LN-INTDATE TO CALCPV-INTDATE
                 MOVE LN-1STPYDATE TO CALCPV-1STPYDATE
                 MOVE "A" TO CALCPV-DISFRMLA
                 PERFORM CALCPV-AMT-FINANCED
                 IF APR-FINANCED < CALCPV-FINANCED
                     COMPUTE PURFLD-DLPARVPAID =
                             (CALCPV-FINANCED - APR-FINANCED)
                 END-IF
              END-IF
           ELSE
           IF DL-PARVPAID-FRMLA = 3
              IF PURFLD-BUYRATE NOT = 0
                 PERFORM CALCULATE-APR
                 IF LN-APRATE > PURFLD-BUYRATE
                    MOVE LN-UNITPER-CD   TO CALCPV-UNITPER-CD
                    MOVE LN-UNITPER-FREQ TO CALCPV-UNITPER-FREQ
                    MOVE PURFLD-BUYRATE  TO CALCPV-RATE
                    MOVE LN-ORGTERM      TO CALCPV-TERM
                    MOVE LN-1STPYAMT     TO CALCPV-1STPYAMT
                    MOVE LN-REGPYAMT     TO CALCPV-REGPYAMT
                    MOVE LN-LASTPYAMT    TO CALCPV-LASTPYAMT
                    MOVE LN-INTDATE      TO CALCPV-INTDATE
                    MOVE LN-1STPYDATE    TO CALCPV-1STPYDATE
                    MOVE "A"             TO CALCPV-DISFRMLA
                    PERFORM CALCPV-AMT-FINANCED
                    IF APR-FINANCED < CALCPV-FINANCED
                       COMPUTE PURFLD-DLPARVPAID =
                         (CALCPV-FINANCED - APR-FINANCED).


      *****************************************************
      *   NAME:  COMPUTE-DLPARVPAID-IB
      *   DESC:  COMPUTE PARTICIPATION PAID FOR IB ACCOUNTS
      *
      *   IN  :  PURFLD-DLPARVPAID       LN-DLPARVPAID
      *          PURFLD-DLPARVPAID-FG    LN-DLPARVPAID-FG
      *   OUT :  PURFLD-DLPARVPAID
      *****************************************************
       COMPUTE-DLPARVPAID-IB SECTION.
           IF DL-PARVPAID-FRMLA-IB = 3
              IF PURFLD-BUYRATE NOT = 0
                 PERFORM CALCULATE-APR
                 IF LN-APRATE > PURFLD-BUYRATE
                    MOVE LN-UNITPER-CD   TO CALCPV-UNITPER-CD
                    MOVE LN-UNITPER-FREQ TO CALCPV-UNITPER-FREQ
                    MOVE PURFLD-BUYRATE  TO CALCPV-RATE
                    MOVE LN-ORGTERM      TO CALCPV-TERM
                    MOVE LN-1STPYAMT     TO CALCPV-1STPYAMT
                    MOVE LN-REGPYAMT     TO CALCPV-REGPYAMT
                    MOVE LN-LASTPYAMT    TO CALCPV-LASTPYAMT
                    MOVE LN-INTDATE      TO CALCPV-INTDATE
                    MOVE LN-1STPYDATE    TO CALCPV-1STPYDATE
                    MOVE "A"             TO CALCPV-DISFRMLA
                    PERFORM CALCPV-AMT-FINANCED
                    IF APR-FINANCED < CALCPV-FINANCED
                       COMPUTE PURFLD-DLPARVPAID =
                          (CALCPV-FINANCED - APR-FINANCED).

      ************************************
      *    COMPUTE ANTICIPATED EARNINGS
      ************************************
       COMPUTE-ANTICIPATED SECTION.
           IF LN-SERVACCT = "Y"
              MOVE 0 TO LN-ANTICERN(1) LN-ANTICERN(2)
                        LN-ANTICERN(3) LN-ANTICERN(4)
                        LN-ANTICERN(5) LN-ANTICERN(6)
              GO TO EXIT-COMPUTE-ANTICIPATED.

           IF LN-LOANTYPE = "I"
              MOVE LN-SERCHG TO LN-ANTICERN(3)
              MOVE 0 TO LN-ANTICERN(1) LN-ANTICERN(2)
                        LN-ANTICERN(4)
      *TFC #008         LN-ANTICERN(4) LN-ANTICERN(6)
              MOVE LN-DLDISC TO LN-ANTICERN(5)
              GO TO EXIT-COMPUTE-ANTICIPATED.

      *TFC #008 STOPPED DL-EARNCD = 1
      *       MOVE "P" TO LN-MIDSTREAM
      *       COMPUTE LN-ANTICERN(6) ROUNDED =
      *                  LN-PURBAL * DL-EARNFAC / 100
      *       MOVE 0 TO LN-ANTICERN(1) LN-ANTICERN(2)
      *                 LN-ANTICERN(3) LN-ANTICERN(4)
      *                 LN-ANTICERN(5)
      *       GO TO EXIT-COMPUTE-ANTICIPATED.

           MOVE " " TO LN-MIDSTREAM.
           ADD LN-INTCHG LN-EXTCHG GIVING LN-ANTICERN(1).

           MOVE LN-SERCHG TO LN-ANTICERN(3).
           COMPUTE LN-ANTICERN(4) = LN-ORGTERM * LN-MAINTFEE.
           MOVE LN-DLDISC TO LN-ANTICERN(5).

      *TFC #008 MOVE 0 TO LN-ANTICERN(6).
      * TEST FOR PARTICIPATING RESERVE REDUCTION IN UNEARNED
      * INSTEAD OF OFFSET TO EXPENSE ACCOUNT:
           IF DL-RVCHGS-CODE(1) = " "
              SUBTRACT LN-DLRVCHGS(1) FROM LN-ANTICERN(1).
           IF DL-RVCHGS-CODE(2) = " "
              SUBTRACT LN-DLRVCHGS(2) FROM LN-ANTICERN(1).
           IF DL-RVCHGS-CODE(3) = " "
              SUBTRACT LN-DLRVCHGS(3) FROM LN-ANTICERN(1).
           IF DL-PARVHELD-CODE = "B"
              SUBTRACT LN-DLPARVHELD FROM LN-ANTICERN(1).
           IF DL-PARVPAID-CODE = " "
              SUBTRACT LN-DLPARVPAID FROM LN-ANTICERN(1).

           MOVE LN-ANTICERN(1) TO WK-ANTICERN(1).
           MOVE LN-ANTICERN(2) TO WK-ANTICERN(2).
           MOVE LN-ANTICERN(3) TO WK-ANTICERN(3).
           MOVE LN-ANTICERN(4) TO WK-ANTICERN(4).
           MOVE LN-ANTICERN(5) TO WK-ANTICERN(5).
      *TFC #008 MOVE LN-ANTICERN(6) TO WK-ANTICERN(6).
           PERFORM ADJUST-ANTICERN.
           MOVE WK-ANTICERN(1) TO LN-ANTICERN(1).
           MOVE WK-ANTICERN(2) TO LN-ANTICERN(2).
           MOVE WK-ANTICERN(3) TO LN-ANTICERN(3).
           MOVE WK-ANTICERN(4) TO LN-ANTICERN(4).
           MOVE WK-ANTICERN(5) TO LN-ANTICERN(5).
      *TFC #008 MOVE WK-ANTICERN(6) TO LN-ANTICERN(6).

       EXIT-COMPUTE-ANTICIPATED.
           EXIT.

      *********************************
      *   COMPUTE ANTICIPATED TERM
      *********************************
       COMPUTE-ANTICTERM SECTION.
           IF LN-SERVACCT = "Y"
              MOVE 0 TO LN-ANTICTERM
              GO TO EXIT-COMPUTE-ANTICTERM.
           MOVE LN-ORGTERM TO LN-ANTICTERM.
           IF LN-LOANTYPE = "I"
              GO TO EXIT-COMPUTE-ANTICTERM.
           IF LN-ANTICTERM > DL-MAXPURTRM
              MOVE DL-MAXPURTRM TO LN-ANTICTERM.
           COMPUTE LN-ANTICTERM ROUNDED =
                       LN-ANTICTERM * DL-MAXPURPER / 100.
       EXIT-COMPUTE-ANTICTERM.
           EXIT.


      *********************************
      *   COMPUTE PURCHASED BALANCE
      *********************************
       COMPUTE-PURBAL SECTION.
           IF LN-SERVACCT = "Y"
              MOVE 0 TO LN-PURBAL
                        LN-ORIGPURAMT
                        LN-SUM-ORIGPURAMT
              GO TO EXIT-COMPUTE-PURBAL.

      *       - - - - - - - - - - - - - - - - - - - - - - - -
      *       THE DUPLICATION OF COMPUTE LN-PURBAL IN THE
      *       FOLLOWING IF WAS DONE TO KEEP PURBAL UNDER
      *       IT'S 9(6)V99 LIMIT WHEN LOAN IS VERY LARGE
      *       - - - - - - - - - - - - - - - - - - - - - - - -
           IF LN-LOANTYPE = "I"
              COMPUTE LN-PURBAL = LN-ANTICTERM * LN-REGPYAMT
                                + LN-1STPYAMT - LN-REGPYAMT
                                - LN-INTCHG - LN-EXTCHG
                                - (LN-MAINTFEE * LN-ORGTERM)
           ELSE
              COMPUTE LN-PURBAL = LN-ANTICTERM * LN-REGPYAMT
                                + LN-1STPYAMT - LN-REGPYAMT.
           IF LN-LASTPYAMT NOT = 0
              COMPUTE LN-PURBAL =
                      LN-PURBAL + LN-LASTPYAMT - LN-REGPYAMT.

           MOVE LN-PURBAL TO LN-ORIGPURAMT
                             LN-SUM-ORIGPURAMT.

       EXIT-COMPUTE-PURBAL.
           EXIT.

      **************************************************************************
      *    COMPUTE LOAN PRINCIPLE
      * REV:
      *  JTG 061898 ADDED LOGIC FOR S.F. RENEWALS INTO S.F. MERCURY #E176
      **************************************************************************
       COMPUTE-PRINCIPLE SECTION.
           PERFORM COMPUTE-MEMO-OURS-NOTOURS.

           COMPUTE LN-LNAMT =
               LN-SELLPR + NON-MEMO-CHGS + LN-SERCHG
                  + LN-PRBAL + LN-PRINTDUE
                   - LN-PRFINCHGRB - LN-PRINSRB.
           IF BR-DOWNPY-FLAG NOT = "Y"
              SUBTRACT LN-DOWNPY LN-TRADEIN FROM LN-LNAMT.

           COMPUTE HOLD-TOTAL-REPAY
                   TOTAL-REPAY =  LN-LNAMT + LN-EXTCHG + LN-INTCHG
                                       + (LN-MAINTFEE * LN-ORGTERM).
           IF LN-LOANTYPE = "P"
              MOVE TOTAL-REPAY TO LN-LNAMT.

           MOVE LN-LNAMT TO LN-SUM-LNAMT.

