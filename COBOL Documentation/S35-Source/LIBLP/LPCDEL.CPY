      * COPYMEMBER: LIBLP/LPCDEL
      **************************************************************************
      *         COMPUTE CONTRACTUAL DELINQUENT AMOUNT
      *          (+ ARREARAGE ON PRINCIPAL REDUCTION)
      *                    (FOR P.C. ACCOUNTS)
      *
      *   NAME: LPCDEL
      *   DESC: THIS ROUTINE COMPUTES THE AMOUNT
      *         IN DOLLARS, THAT AN ACCOUNT IS
      *         CONTRACTUALLY DELINQUENT, BASED ON THE
      *         DEFINITION OF SP-CONTRFRMAL WHICH DETERMINES
      *         WHICH MONIES TO ADD TO PAYMENTS TO DATE.
      *
      *         ...ALSO THE ROUTINE COMPUTES ARREARAGE FOR
      *         PRECOMPUTED ACCOUNTS (CONTRACTUAL PRINCIPAL
      *         PAYMENTS THAT SHOULD HAVE BEEN MADE).
      *         NOTE:
      *               FOR REVOLVING ACCOUNTS
      *
      *   IN  : CEPP1-DATE       (LN-1STPYDATE)
      *         CEPP2-DATE       (TODAYS DATE)
      *
      *   OUT : CDEL-DELINQUENT         AMOUNT DUE TO ADVANCE CONTR PAID THRU
      *         CDEL-ARREARAGE          AMOUNT OF CONTRACTUAL PRINCIPAL DUE
      *               +  PAID AHEAD
      *               0  CURRENT
      *               -  DELINQUENT
      *   USED: LN-1STPYAMT, LN-REGPYAMT, LN-TOTPAYMNTD,
      *         LN-CURBAL, LN-PAY-SCHLD-FG
      *   COPY: TIM, TIMW, TIM365, LPMDTE, LPMDTEW, LPCEPP
      *         LPCEPPW, LPCDELW, SP-CONTRFRMLA
      * REV :
      *  030686 JTG  ADDED TEST FOR CURBAL = 0
      *  052786 JTG  ADDED ROUTINE TO COMPUTE ACCOUNT ARREARAGE.
      *  091086 JTG  ADDED LOGIC TO SET ARREARAGE
      *              EQUAL TO DELINQUENCY IF OTHER
      *              THAN SP-CONTRFRMLA = "A", SO
      *              NOTICE RECORDS CAN PRODUCE ARREARAGE
      *              FIGURES FOR ALL THREE CATAGORIES.
      *  101686 JTG  ADDED LOGIC TO TEST FOR THE
      *              AMOUNT COMPUTED DELINQUENT TO BE
      *              LARGER THAN THE CURRENT BALANCE.
      *  041487 JTG  MODIFIED LOGIC FOR CLEARITY
      *  012189 SLC  IF DELINQUENCY > BAL WE DON'T ADJUST IT.
      *  080690 JTG  ADDED LOGIC FOR PAYMENT SCHEDULE
      *  070191 JTG  CHANGED TO CONSIDER "PE" PAYMENTS AND STOPPED
      *              REPORTING DELINQUENCY GREATER THAN LN-CURBAL
      *  121291 JTG  STOPPED LATE CHARGES PAID FROM IMPACTING
      *              DELINQUENCY ON IB'S, RE: CONTRACTUAL FRMLA A
      *  030293 JTG  CHANGED TO WORK FOR LT-PAY-SCHLD-TBL(17)
      *  052797 BAH  DISPLAY ACCTNO BEFORE IO-ERROR ON GET-LT-P-REC
      *  012398 JTG  ADDED CDEL-LPCRNO-FG, RE: LPCRNO LDUE & NXDUE AMOUNTS
      *              ZERO RE: TDUE = 01/31/98 NXDUE = 02/28/98
      *  011099 JTG  ADDED LOGIC FOR REVOLVING ACCOUNTS, REGENCY REVOL
      *  JTG 030703  CHANGED LOGIC TO TEST ARREARAGE AGAINST CURBAL SIMILAR
      *              TO DELINQUENCY, SINCE IF DELINQUENT ARREARAGE IS
      *              GREATER THAN THE CURRENT BALANCE IT MUST BE
      *              FORCED TO NOT EXCEED CURRENT BALANCE         REGACC #J030
      *  160113 BAH SPLIT OUT LTFILE, LTPFILE, RENAMED LT- LTP- PD0003
      *  160615 BLM FIX CONTRACTUAL-DELINQ-REVOL TO CALL GETLTR SINCE 
      *             REVOLVING FIELDS ARE IN LTR, NOT LNFILE, PD0003
      * KEC 2016.1116 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         ADDED LTR-SEQNO TO LTR1-KEY.
      * BAH 20190625 REMOVED LN-REVOLVING, PT0153
      **************************************************************************
       CONTRACTUAL-DELINQUENCY SECTION.
           MOVE 0 TO CDEL-DELINQUENT CDEL-ARREARAGE.
           IF LN-CURBAL = 0
              GO TO CONTRACTUAL-DELINQ-EXIT.

      * TEST FOR TODAY PAST MATURITY, IF SO
      * MAKE LN-CURBAL AMOUNT DELINQUENT:
           PERFORM MATURITY-DATE-CALCULATION.
           MOVE CEPP2-DATE TO NUM-DATE.
           MOVE MDTE-DATE  TO SYS-DATE.
           PERFORM TIM.
           IF ELAPSED-DAYS < 0
              SUBTRACT LN-CURBAL FROM CDEL-DELINQUENT
              MOVE CDEL-DELINQUENT TO CDEL-ARREARAGE
              GO TO CONTRACTUAL-DELINQ-EXIT.

      * COMPUTE FOR PRIOR TO MATURITY:
           PERFORM CONTRACTUAL-ELAPSED-PAYPER.

      *--------------------------------------------------------------------
      *    WHEN PERFORM IS DONE FROM LPCRNO ROUTINE,
      *    INSURE THAT LAST DUE AND NEXT DUE AMOUNTS ARE CORRECT
      *
      *    INCREMENT CEPP-PERIODS BY 1.
      *
      *    EXAMPLE:    LN-1STPYDATE =  04/31/97
      *                TODAY DUE    =  01/23/98     0.00 PDTH = 013198
      *                NEXT  DUE    =  02/28/98     0.00   <---- WRONG
      *--------------------------------------------------------------------
           IF CDEL-LPCRNO-FG = "Y"
              IF LN-UNITPER-CD = "M" AND LN-UNITPER-FREQ = 1
                 IF CEPP1-DA > CEPP2-DA
                    ADD 1 TO CEPP-PERIODS.
      *--------------------------------------------------------------------

           IF LN-PAY-SCHLD-FG = "Y"
              PERFORM CONTRACTUAL-DELINQ-ADDON
           ELSE
              PERFORM CONTRACTUAL-DELINQ-NORMAL.

           COMPUTE CDEL-DELINQUENT
                   CDEL-ARREARAGE = LN-TOTPAYMNTD - CDEL-DELINQUENT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    IF DELINQUENT
      *       IF AMOUNT DELINQUENT > LN-CURBAL
      *          MAKE THE AMOUNT DELINQUENT = LN-CURBAL
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF CDEL-DELINQUENT < 0
              IF ( CDEL-DELINQUENT * -1 ) > LN-CURBAL
                 SUBTRACT LN-CURBAL FROM 0 GIVING CDEL-DELINQUENT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * COMPUTE ARREARAGE - AMOUNT OF PRINCIPAL DUE THRU CEPP2-DATE
      *
      *    EXAMPLE:
      *                 3000.00 DUE THRU CEPP2-DATE     (1STPY + REGPAYS MADE)
      *               - 2800.00 PAYMNTD PRINCIPAL ONLY  (TOTPAYMNTD - TOTLCHG)
      *                 -------
      *                  200.00 OWE IN PRINCIPAL        (ARREARAGE)
      *               -  150.00 LESS LATE CHARGES IF SP-CONTRFRMLA (A)
      *                 -------
      *                   50.00 OWES FOR CONTRACTUAL ADVANCE ONLY  (DELINQUENT)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF SP-CONTRFRMLA = "A"
              SUBTRACT LN-TOTLCHG FROM CDEL-ARREARAGE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    IF ARREARAGE IS DELINQUENT
      *       IF DELINQUENT ARREARAGE > LN-CURBAL
      *          MAKE THE AMOUNT OF ARREARAGE = LN-CURBAL
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF CDEL-ARREARAGE < 0
              IF ( CDEL-ARREARAGE * -1 ) > LN-CURBAL
                 SUBTRACT LN-CURBAL FROM 0 GIVING CDEL-ARREARAGE.

       CONTRACTUAL-DELINQ-EXIT.
           EXIT.

      *-----------------------------------------------------
       CONTRACTUAL-DELINQ-NORMAL SECTION.
           IF LN-1STPYAMT NOT = 0
              IF CEPP-PERIODS > 0
                 ADD LN-1STPYAMT TO CDEL-DELINQUENT
                 SUBTRACT 1 FROM CEPP-PERIODS.
           COMPUTE CDEL-DELINQUENT = CDEL-DELINQUENT
                          + (LN-REGPYAMT * CEPP-PERIODS).

      *-----------------------------------------------------
       CONTRACTUAL-DELINQ-ADDON SECTION.
           PERFORM GET-LT-P-REC.
           IF IO-FG = 9
              STRING LN1-KEY,"-","MISSING PAYMENT SCHEDULE"
                 DELIMITED BY "  " INTO MESS
              PERFORM SEND-MESS
              GO TO IO-ERROR
           ELSE
              MOVE 1 TO CDEL
              PERFORM CONTRACTUAL-DELINQ-ADDON-SUM
                 UNTIL CDEL > 17 OR CEPP-PERIODS = 0.

       CONTRACTUAL-DELINQ-ADDON-SUM SECTION.
           IF LTP-SCHLD-TERM(CDEL) NOT = 0
              IF CEPP-PERIODS > LTP-SCHLD-TERM(CDEL)
                 COMPUTE CDEL-DELINQUENT = CDEL-DELINQUENT
                    + LTP-SCHLD-TERM(CDEL) * LTP-SCHLD-PAYMNT(CDEL)
                 SUBTRACT LTP-SCHLD-TERM(CDEL) FROM CEPP-PERIODS
              ELSE
                 COMPUTE CDEL-DELINQUENT = CDEL-DELINQUENT
                    + CEPP-PERIODS * LTP-SCHLD-PAYMNT(CDEL)
                 MOVE 0 TO CEPP-PERIODS
           ELSE
              MOVE 0 TO CEPP-PERIODS.
           ADD 1 TO CDEL.

