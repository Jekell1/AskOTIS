      * COPYMEMBER: LIBLP/PURFLDPD
      ****************************************************************
      *             (PURFLDPD)
      *    ROUTINES TO CALCULATE SALES FINANCE
      *             PURCHASE RELATED FIELDS
      *    NOTE:
      *         REMEMBER TO CHANGE 'PURFLDLN'
      *
      *         ALL PD FIELDS THAT ARE MODIFIED/CHANGED IN
      *         COMPUTE-PURCHASE-FIELDS ARE NOW IN LIBLP/PURFLDW.
      *
      * REV:
      * JTG 032596 ADDED DL-RVCHGS-FRMLA = 1, RELIANCE % OF INTCHG
      * MJD 032796 ADDING WORKER TO REPLCE PD-FIELDS THAT ARE CHANGED
      *            IN COMPUTE-PURCHASE-FIELDS SECTION.
      * JTG 032896 ADDED USE OF PURFLD.*-FG FIELDS
      * BAH 050897 ADDED CALCULATE-APR FOR DISC-FRMLA 2, LIKE A50, AND
      *              ALSO PARVHELP AND PARVPAID FRMLA 2.
      * BAH 061697 ADDED RVPRIN-FRMLA 01 FOR FEDERATED, PR#508
      * BAH 022398 ADDED DL-DISC-FRMLA 5 FOR FFG, PR#653
      * BAH 040198 ADDED RVPRIN-FRMLA 02 FOR FUTURE, PR#677
      * JTG 061898 ADDED LOGIC FOR S.F. RENEWALS INTO S.F. MERCURY #E176
      * JTG 990322 REMOVED CLEAR OF ANTICERN(6), RE: LOAN COST LOGIC TFC #008
      * MJD 990817 ADDED PURFLD-STAT
      * JTG 010507 ALLOWED ENTRY OF 'TRADEIN' RE: SALE PRICE-DP-TI PAACO PR#1467
      * JTG 020117 CHANGED FIELDS FROM S9(6)V99 TO S9(7)V99     LENDMARK PR#J021
      * JTG 030617 ADDED LOGIC FOR IB SALES FINANCE AMORTIZATED PARTICIPATION
      *            RESERVE PAID EXPENSE, DIFFERENCE BETWEEN A BUY RATE
      *            AND THE CONTRACT RATE                 WISCONSIN FINANCE #2049
      *************************************************************************
      *   NAME:  COMPUTE-PURCHASE-FIELDS
      *   DESC:  COMPUTE ALL PURCHASE RELATED FIELDS
      *
      *   IN  :  ALL PURFLD       FIELDS
      *          ALL PURFLD--(FG) FIELDS
      *   OUT :  ALL PURFLD       FIELDS
      ***************************************************
       COMPUTE-PURCHASE-FIELDS SECTION.
           IF PD-SERVACCT = "Y"
              GO TO COMPUTE-PURCHASE-FIELDS-EXIT.

           MOVE " "     TO PURFLD-STAT.

           MOVE PD-DLNO TO DL-DLNO.
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
              IF PD-LOANTYPE = "I"
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
      *   IN  :  PURFLD-DLDISC       PD-DLDISC
      *          PURFLD-DLDISC-FG    PD-DLDISC-FG
      *   OUT :  PURFLD-DLDISC
      **********************************************
       COMPUTE-DLDISC SECTION.
           MOVE 0 TO WORK-RATE.
           MOVE 6 TO SUB.
       COMPUTE-DLDISC-NEXT.
           IF PD-ORGTERM NOT > DL-DISC-TERM(SUB)
              MOVE DL-DISC-RATE(SUB) TO WORK-RATE.
           IF SUB > 1
              SUBTRACT 1 FROM SUB
              GO TO COMPUTE-DLDISC-NEXT.
           IF DL-DISC-FRMLA = 0
              COMPUTE PURFLD-DLDISC ROUNDED =
                    (PD-SELLPR - PD-DOWNPY - PD-TRADEIN)
                                      * (WORK-RATE / 100)
           ELSE
              IF DL-DISC-FRMLA = 1
                 COMPUTE PURFLD-DLDISC ROUNDED =
                       PD-SELLPR * (WORK-RATE / 100)
              ELSE
                 IF DL-DISC-FRMLA = 2
                    PERFORM CALCULATE-APR
                    IF PD-APRATE < WORK-RATE
                       MOVE PD-UNITPER-CD TO CALCPV-UNITPER-CD
                       MOVE PD-UNITPER-FREQ TO CALCPV-UNITPER-FREQ
                       MOVE WORK-RATE TO CALCPV-RATE
                       MOVE PD-ORGTERM TO CALCPV-TERM
                       MOVE PD-1STPYAMT TO CALCPV-1STPYAMT
                       MOVE PD-REGPYAMT TO CALCPV-REGPYAMT
                       MOVE PD-LASTPYAMT TO CALCPV-LASTPYAMT
                       MOVE PD-INTDATE TO CALCPV-INTDATE
                       MOVE PD-1STPYDATE TO CALCPV-1STPYDATE
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
                    IF DL-DISC-FRMLA = 5
                       PERFORM LOAN-CALCULATIONS
                       COMPUTE PURFLD-DLDISC ROUNDED =
                           (FINANCED-AMOUNT - INSUR-OURS - FEES-OURS)
                                 * (WORK-RATE / 100).

      ***************************************************
      *   NAME:  COMPUTE-DLRVPRIN
      *   DESC:  COMPUTE RESERVE FROM PRINCIPAL
      *
      *   IN  :  PURFLD-DLRVPRIN()     PD-DLRVPRIN()
      *          PURFLD-DLRVPRIN-FG()  PD-DLRVPRIN-FG()
      *          RVNO    (1-3)
      *   OUT :  PURFLD-DLRVPRIN()
      ***************************************************
       COMPUTE-DLRVPRIN SECTION.
           IF PURFLD-DLRVPRIN-FG(RVNO) NOT = "C"
              GO TO COMPUTE-DLRVPRIN-EXIT.
           MOVE 0 TO PURFLD-DLRVPRIN(RVNO).

           MOVE 0 TO WORK-RATE.
           MOVE 6 TO SUB.
       COMPUTE-DLRVPRIN-NEXT.
           IF PD-ORGTERM NOT > DL-RVPRIN-TERM(RVNO  SUB)
              MOVE DL-RVPRIN-RATE(RVNO  SUB) TO WORK-RATE.
           IF SUB > 1
              SUBTRACT 1 FROM SUB
              GO TO COMPUTE-DLRVPRIN-NEXT.
           IF DL-RVPRIN-FRMLA(RVNO) = 0
              COMPUTE PURFLD-DLRVPRIN(RVNO) ROUNDED =
                 DL-RVPRIN-ADDON(RVNO) +
                    (PD-SELLPR - PD-DOWNPY - PD-TRADEIN)
                                      * (WORK-RATE / 100)
           ELSE
           IF DL-RVPRIN-FRMLA(RVNO) = 99
              COMPUTE PURFLD-DLRVPRIN(RVNO) = PD-LNAMT - PD-PURBAL
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

      ******************************************************
      *   NAME:  COMPUTE-DLRVCHGS
      *   DESC:  COMPUTE RESERVE FROM CHARGES
      *
      *   IN  :  PURFLD-DLRVCHGS()     PD-DLRVCHGS()
      *          PURFLD-DLRVCHGS-FG()  PD-DLRVCHGS-FG()
      *          RVNO    (1-3)
      *   OUT :  PURFLD-DLRVCHGS()
      ******************************************************
       COMPUTE-DLRVCHGS SECTION.
           IF PURFLD-DLRVCHGS-FG(RVNO) NOT = "C"
              GO TO COMPUTE-DLRVCHGS-EXIT.
           MOVE 0 TO PURFLD-DLRVCHGS(RVNO).

           MOVE 0 TO WORK-RATE.
           MOVE 6 TO SUB.
       COMPUTE-DLRVCHGS-NEXT.
           IF PD-ORGTERM NOT > DL-RVCHGS-TERM(RVNO  SUB)
              MOVE DL-RVCHGS-RATE(RVNO  SUB) TO WORK-RATE.
           IF SUB > 1
              SUBTRACT 1 FROM SUB
              GO TO COMPUTE-DLRVCHGS-NEXT.
           IF DL-RVCHGS-FRMLA(RVNO) = 0
              COMPUTE PURFLD-DLRVCHGS(RVNO) ROUNDED =
                 DL-RVCHGS-ADDON(RVNO) +
                    (PD-SELLPR - PD-DOWNPY - PD-TRADEIN)
                                      * (WORK-RATE / 100)
           ELSE
              IF DL-RVCHGS-FRMLA(RVNO) = 1
                 COMPUTE PURFLD-DLRVCHGS(RVNO) ROUNDED =
                      DL-RVCHGS-ADDON(RVNO) +
                              PD-INTCHG * (WORK-RATE / 100)
              ELSE
                IF DL-RVCHGS-FRMLA(RVNO) = 99
                   COMPUTE PURFLD-DLRVCHGS(RVNO) = PD-LNAMT - PD-PURBAL.

       COMPUTE-DLRVCHGS-EXIT.
           EXIT.

      *****************************************************
      *   NAME:  COMPUTE-DLPARVHELD
      *   DESC:  COMPUTE PARTICIPATION HELD
      *
      *   IN  :  PURFLD-DLPARVHELD       PD-DLPARVHELD
      *          PURFLD-DLPARVHELD-FG    PD-DLPARVHELD-FG
      *          RVNO    (1-3)
      *   OUT :  PURFLD-DLPARVHELD
      *****************************************************
       COMPUTE-DLPARVHELD SECTION.
           MOVE 0 TO WORK-RATE.
           MOVE 6 TO SUB.
       COMPUTE-DLPARVHELD-NEXT.
           IF PD-ORGTERM NOT > DL-PARVHELD-TERM(SUB)
              MOVE DL-PARVHELD-RATE(SUB) TO WORK-RATE.
           IF SUB > 1
              SUBTRACT 1 FROM SUB
              GO TO COMPUTE-DLPARVHELD-NEXT.

           IF DL-PARVHELD-FRMLA = 0
              COMPUTE PURFLD-DLPARVHELD ROUNDED = DL-PARVHELD-ADDON +
                    (PD-SELLPR - PD-DOWNPY - PD-TRADEIN)
                                      * (WORK-RATE / 100)
           ELSE
              IF DL-PARVHELD-FRMLA = 1
                 COMPUTE PURFLD-DLPARVHELD ROUNDED = DL-PARVHELD-ADDON +
                      PD-INTCHG * (WORK-RATE / 100)
              ELSE
                 IF DL-PARVHELD-FRMLA = 2
                    PERFORM CALCULATE-APR
                    IF PD-APRATE > WORK-RATE
                       MOVE PD-UNITPER-CD TO CALCPV-UNITPER-CD
                       MOVE PD-UNITPER-FREQ TO CALCPV-UNITPER-FREQ
                       MOVE WORK-RATE TO CALCPV-RATE
                       MOVE PD-ORGTERM TO CALCPV-TERM
                       MOVE PD-1STPYAMT TO CALCPV-1STPYAMT
                       MOVE PD-REGPYAMT TO CALCPV-REGPYAMT
                       MOVE PD-LASTPYAMT TO CALCPV-LASTPYAMT
                       MOVE PD-INTDATE TO CALCPV-INTDATE
                       MOVE PD-1STPYDATE TO CALCPV-1STPYDATE
                       MOVE "A" TO CALCPV-DISFRMLA
                       PERFORM CALCPV-AMT-FINANCED
                       IF APR-FINANCED < CALCPV-FINANCED
                           COMPUTE PURFLD-DLPARVHELD =
                                   (CALCPV-FINANCED - APR-FINANCED).

      *****************************************************
      *   NAME:  COMPUTE-DLPARVPAID
      *   DESC:  COMPUTE PARTICIPATION PAID
      *
      *   IN  :  PURFLD-DLPARVPAID       PD-DLPARVPAID
      *          PURFLD-DLPARVPAID-FG    PD-DLPARVPAID-FG
      *   OUT :  PURFLD-DLPARVPAID
      *****************************************************
       COMPUTE-DLPARVPAID SECTION.
           MOVE 0 TO WORK-RATE.
           MOVE 6 TO SUB.
       COMPUTE-DLPARVPAID-NEXT.
           IF PD-ORGTERM NOT > DL-PARVPAID-TERM(SUB)
              MOVE DL-PARVPAID-RATE(SUB) TO WORK-RATE.
           IF SUB > 1
              SUBTRACT 1 FROM SUB
              GO TO COMPUTE-DLPARVPAID-NEXT.

           IF DL-PARVPAID-FRMLA = 0
              COMPUTE PURFLD-DLPARVPAID ROUNDED = DL-PARVPAID-ADDON +
                    (PD-SELLPR - PD-DOWNPY - PD-TRADEIN)
                                      * (WORK-RATE / 100)
           ELSE
           IF DL-PARVPAID-FRMLA = 1
              COMPUTE PURFLD-DLPARVPAID ROUNDED = DL-PARVPAID-ADDON +
                   PD-INTCHG * (WORK-RATE / 100)
           ELSE
           IF DL-PARVPAID-FRMLA = 2
              PERFORM CALCULATE-APR
              IF PD-APRATE > WORK-RATE
                 MOVE PD-UNITPER-CD TO CALCPV-UNITPER-CD
                 MOVE PD-UNITPER-FREQ TO CALCPV-UNITPER-FREQ
                 MOVE WORK-RATE TO CALCPV-RATE
                 MOVE PD-ORGTERM TO CALCPV-TERM
                 MOVE PD-1STPYAMT TO CALCPV-1STPYAMT
                 MOVE PD-REGPYAMT TO CALCPV-REGPYAMT
                 MOVE PD-LASTPYAMT TO CALCPV-LASTPYAMT
                 MOVE PD-INTDATE TO CALCPV-INTDATE
                 MOVE PD-1STPYDATE TO CALCPV-1STPYDATE
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
                 IF PD-APRATE > PURFLD-BUYRATE
                    MOVE PD-UNITPER-CD   TO CALCPV-UNITPER-CD
                    MOVE PD-UNITPER-FREQ TO CALCPV-UNITPER-FREQ
                    MOVE PURFLD-BUYRATE  TO CALCPV-RATE
                    MOVE PD-ORGTERM      TO CALCPV-TERM
                    MOVE PD-1STPYAMT     TO CALCPV-1STPYAMT
                    MOVE PD-REGPYAMT     TO CALCPV-REGPYAMT
                    MOVE PD-LASTPYAMT    TO CALCPV-LASTPYAMT
                    MOVE PD-INTDATE      TO CALCPV-INTDATE
                    MOVE PD-1STPYDATE    TO CALCPV-1STPYDATE
                    MOVE "A"             TO CALCPV-DISFRMLA
                    PERFORM CALCPV-AMT-FINANCED
                    IF APR-FINANCED < CALCPV-FINANCED
                       COMPUTE PURFLD-DLPARVPAID =
                            (CALCPV-FINANCED - APR-FINANCED).

      *****************************************************
      *   NAME:  COMPUTE-DLPARVPAID-IB
      *   DESC:  COMPUTE PARTICIPATION PAID FOR IB ACCOUNTS
      *
      *   IN  :  PURFLD-DLPARVPAID       PD-DLPARVPAID
      *          PURFLD-DLPARVPAID-FG    PD-DLPARVPAID-FG
      *   OUT :  PURFLD-DLPARVPAID
      *****************************************************
       COMPUTE-DLPARVPAID-IB SECTION.
           IF DL-PARVPAID-FRMLA-IB = 3
              IF PURFLD-BUYRATE NOT = 0
                 PERFORM CALCULATE-APR
                 IF PD-APRATE > PURFLD-BUYRATE
                    MOVE PD-UNITPER-CD   TO CALCPV-UNITPER-CD
                    MOVE PD-UNITPER-FREQ TO CALCPV-UNITPER-FREQ
                    MOVE PURFLD-BUYRATE  TO CALCPV-RATE
                    MOVE PD-ORGTERM      TO CALCPV-TERM
                    MOVE PD-1STPYAMT     TO CALCPV-1STPYAMT
                    MOVE PD-REGPYAMT     TO CALCPV-REGPYAMT
                    MOVE PD-LASTPYAMT    TO CALCPV-LASTPYAMT
                    MOVE PD-INTDATE      TO CALCPV-INTDATE
                    MOVE PD-1STPYDATE    TO CALCPV-1STPYDATE
                    MOVE "A"             TO CALCPV-DISFRMLA
                    PERFORM CALCPV-AMT-FINANCED
                    IF APR-FINANCED < CALCPV-FINANCED
                       COMPUTE PURFLD-DLPARVPAID =
                             (CALCPV-FINANCED - APR-FINANCED).

      ************************************
      *    COMPUTE ANTICIPATED EARNINGS
      ************************************
       COMPUTE-ANTICIPATED SECTION.
           IF PD-SERVACCT = "Y"
              MOVE 0 TO PD-ANTICERN(1) PD-ANTICERN(2)
                        PD-ANTICERN(3) PD-ANTICERN(4)
                        PD-ANTICERN(5) PD-ANTICERN(6)
              GO TO EXIT-COMPUTE-ANTICIPATED.

           IF PD-LOANTYPE = "I"
              MOVE PD-SERCHG TO PD-ANTICERN(3)
              MOVE 0 TO PD-ANTICERN(1) PD-ANTICERN(2)
                        PD-ANTICERN(4)
      *TFC #008         PD-ANTICERN(4) PD-ANTICERN(6)
              MOVE PD-DLDISC TO PD-ANTICERN(5)
              GO TO EXIT-COMPUTE-ANTICIPATED.

      * TFC #008 STOPPED DL-EARNCD = 1
      *    IF DL-EARNCD = 1
      *       MOVE "P" TO PD-MIDSTREAM
      *       COMPUTE PD-ANTICERN(6) ROUNDED =
      *                  PD-PURBAL * DL-EARNFAC / 100
      *       MOVE 0 TO PD-ANTICERN(1) PD-ANTICERN(2)
      *                 PD-ANTICERN(3) PD-ANTICERN(4)
      *                 PD-ANTICERN(5)
      *       GO TO EXIT-COMPUTE-ANTICIPATED.

           MOVE " " TO PD-MIDSTREAM.
           ADD PD-INTCHG PD-EXTCHG GIVING PD-ANTICERN(1).

           MOVE PD-SERCHG TO PD-ANTICERN(3).
           COMPUTE PD-ANTICERN(4) = PD-ORGTERM * PD-MAINTFEE.
           MOVE PD-DLDISC TO PD-ANTICERN(5).

      *TFC #008 MOVE 0 TO PD-ANTICERN(6).
      * TEST FOR PARTICIPATING RESERVE REDUCTION IN UNEARNED
      * INSTEAD OF OFFSET TO EXPENSE ACCOUNT:
           IF DL-RVCHGS-CODE(1) = " "
              SUBTRACT PD-DLRVCHGS(1) FROM PD-ANTICERN(1).
           IF DL-RVCHGS-CODE(2) = " "
              SUBTRACT PD-DLRVCHGS(2) FROM PD-ANTICERN(1).
           IF DL-RVCHGS-CODE(3) = " "
              SUBTRACT PD-DLRVCHGS(3) FROM PD-ANTICERN(1).
           IF DL-PARVHELD-CODE = "B"
              SUBTRACT PD-DLPARVHELD FROM PD-ANTICERN(1).
           IF DL-PARVPAID-CODE = " "
              SUBTRACT PD-DLPARVPAID FROM PD-ANTICERN(1).

           MOVE PD-ANTICERN(1) TO WK-ANTICERN(1).
           MOVE PD-ANTICERN(2) TO WK-ANTICERN(2).
           MOVE PD-ANTICERN(3) TO WK-ANTICERN(3).
           MOVE PD-ANTICERN(4) TO WK-ANTICERN(4).
           MOVE PD-ANTICERN(5) TO WK-ANTICERN(5).
      *TFC #008 MOVE PD-ANTICERN(6) TO WK-ANTICERN(6).
           PERFORM ADJUST-ANTICERN.
           MOVE WK-ANTICERN(1) TO PD-ANTICERN(1).
           MOVE WK-ANTICERN(2) TO PD-ANTICERN(2).
           MOVE WK-ANTICERN(3) TO PD-ANTICERN(3).
           MOVE WK-ANTICERN(4) TO PD-ANTICERN(4).
           MOVE WK-ANTICERN(5) TO PD-ANTICERN(5).
      *TFC #008 MOVE WK-ANTICERN(6) TO PD-ANTICERN(6).

       EXIT-COMPUTE-ANTICIPATED.
           EXIT.

      *********************************
      *   COMPUTE ANTICIPATED TERM
      *********************************
       COMPUTE-ANTICTERM SECTION.
           IF PD-SERVACCT = "Y"
              MOVE 0 TO PD-ANTICTERM
              GO TO EXIT-COMPUTE-ANTICTERM.
           MOVE PD-ORGTERM TO PD-ANTICTERM.
           IF PD-LOANTYPE = "I"
              GO TO EXIT-COMPUTE-ANTICTERM.
           IF PD-ANTICTERM > DL-MAXPURTRM
              MOVE DL-MAXPURTRM TO PD-ANTICTERM.
           COMPUTE PD-ANTICTERM ROUNDED =
                       PD-ANTICTERM * DL-MAXPURPER / 100.
       EXIT-COMPUTE-ANTICTERM.
           EXIT.

      *********************************
      *   COMPUTE PURCHASED BALANCE
      *********************************
       COMPUTE-PURBAL SECTION.
           IF PD-SERVACCT = "Y"
              MOVE 0 TO PD-PURBAL
                        PD-ORIGPURAMT
                        PD-SUM-ORIGPURAMT
              GO TO EXIT-COMPUTE-PURBAL.

      *       - - - - - - - - - - - - - - - - - - - - - - - -
      *       THE DUPLICATION  OF COMPUTE PD-PURBAL IN THE
      *       FOLLOWING IF WAS DONE TO KEEP PURBAL UNDER
      *       IT'S 9(6)V99 LIMIT WHEN LOAN IS VERY LARGE
      *       - - - - - - - - - - - - - - - - - - - - - - - -
           IF PD-LOANTYPE = "I"
              COMPUTE PD-PURBAL = PD-ANTICTERM * PD-REGPYAMT
                                + PD-1STPYAMT - PD-REGPYAMT
                                - PD-INTCHG - PD-EXTCHG
                                - (PD-MAINTFEE * PD-ORGTERM)
           ELSE
              COMPUTE PD-PURBAL = PD-ANTICTERM * PD-REGPYAMT
                                + PD-1STPYAMT - PD-REGPYAMT.
           IF PD-LASTPYAMT NOT = 0
              COMPUTE PD-PURBAL =
                      PD-PURBAL + PD-LASTPYAMT - PD-REGPYAMT.

           MOVE PD-PURBAL TO PD-ORIGPURAMT
                             PD-SUM-ORIGPURAMT.

       EXIT-COMPUTE-PURBAL.
           EXIT.

      **************************************************************************
      *    COMPUTE LOAN PRINCIPLE
      * REV:
      *  JTG 061898 ADDED LOGIC FOR S.F. RENEWALS INTO S.F. MERCURY #E176
      **************************************************************************
       COMPUTE-PRINCIPLE SECTION.
           PERFORM COMPUTE-MEMO-OURS-NOTOURS.

           COMPUTE PD-LNAMT =
               PD-SELLPR + NON-MEMO-CHGS + PD-SERCHG
                  + PD-PRBAL + PD-PRINTDUE
                   - PD-PRFINCHGRB - PD-PRINSRB.
           IF BR-DOWNPY-FLAG NOT = "Y"
              SUBTRACT PD-DOWNPY PD-TRADEIN FROM PD-LNAMT.

           COMPUTE HOLD-TOTAL-REPAY
                   TOTAL-REPAY =  PD-LNAMT + PD-EXTCHG + PD-INTCHG
                                       + (PD-MAINTFEE * PD-ORGTERM).
           IF PD-LOANTYPE = "P"
              MOVE TOTAL-REPAY TO PD-LNAMT.

           MOVE PD-LNAMT TO PD-SUM-LNAMT.

