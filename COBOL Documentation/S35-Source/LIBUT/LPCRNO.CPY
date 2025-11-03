      **************************************************************************
      * LIBLP/LPCRNO.CPY                     
      * REV:
      *  MB  03-08-2024  CHANGED LP REC RETRIEVAL FROM OPEN CURSOR NOT-GREATER
      *                   THAN/READ-PREV TO SINGLE SELECT IN UPDATE-LAST-PAYMENT.
      **************************************************************************
       CREATE-NOREC SECTION.
           PERFORM CREATE-NOREC-INITIALIZE.

      * COMPUTE CONTRACTUAL & RECENCY:
           MOVE CRNO-PAYDATE TO AGEING-DATE.
           PERFORM COMPUTE-CONTRACTUAL.
           MOVE CONTRACTUAL TO NO-TRUCONTR.
           PERFORM COMPUTE-RECENCY.
           MOVE RECENCY     TO NO-RECENCY.
           MOVE RECDEL      TO NO-RECDEL.
           MOVE CSUB        TO NO-CSUB.
           MOVE RSUB        TO NO-RSUB.
           MOVE CPOT30      TO NO-CPOT30.
           MOVE RDPOT30     TO NO-RDPOT30.
           MOVE CPOTCHGOFF  TO NO-CPOTCHGOFF.
           MOVE RDPOTCHGOFF TO NO-RDPOTCHGOFF.

      * COMPUTE TOTAL OF ALL BALANCES:
           COMPUTE NO-TDUETBAL =
                LN-CURBAL + LN-OTHBAL + LN-INTBAL + LN-LCBAL.

      * WORLD PR# 254, IF ACCOUNT PAID AHEAD BECAUSE OF LATE CHARGES,
      * THE TODAY DUE WOULD BE 0, BUT NEXT DUE WOULD BE 3 PAYMENTS
      * PLUS LATE CHARGES, THEY WANT TO SEE 1 PAYMENT DUE, AND NO LATE
           MOVE SP-CONTRFRMLA TO CRNO-HOLD-CONTRFRMLA.
           IF GP-IGNORE-LCHG-NXDUE = "Y"
              IF SP-CONTRFRMLA-TOTAL
                 MOVE " " TO SP-CONTRFRMLA
                 MOVE LN-TOTPAYMNTD TO CRNO-HOLD-TOTPAYMNTD
                 COMPUTE LN-TOTPAYMNTD = LN-TOTPAYMNTD - LN-TOTLCHG.

      * COMPUTE CONTRACTUAL AND LATE CHARGE
      * PAID THRU DATES:
           MOVE LN-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-DATE-FULL TO NO-PDTH-DATE.

           MOVE LN-LCPDTH-DATE TO NO-LCPDTH-DATE.

      *  RETURNS NEXT SCHEDULED PAYMENT AMOUNT
      *  AND IF PAYMENT IS PARTIALLY PAID, IT STILL RETURNS IT
           MOVE PDTH-CUR-SCHD-PAY TO NO-SCHED-PAYMENT.

      ***********************************************
      *    COMPUTE TODAY DUE FIGURES:
      ***********************************************
      * IF WDT, AVAIL CREDIT ROUTINE PERFORMS PAYOFF-LOAN ROUTINE FOR
      * A PB PAYOFF TO GET NETDUE FOR THE CURRENT AVAILABLE FIELD,
      * SO DONT DO PAYOFF ROUTINE HERE
           IF BR-AVAIL-CREDIT NOT = "S"
              MOVE CRNO-PAYDATE TO NO-TDUE-DATE
                                   POFF-PAYDATE
              MOVE CRNO-MAKERCD TO POFF-MAKERCD
              MOVE CRNO-DEF-STOP TO POFF-DEF-STOP
              MOVE CRNO-LPTRCD TO POFF-LPTRCD
              MOVE CRNO-RECAST-FG TO POFF-RECAST-FG
              PERFORM PAYOFF-LOAN-ROUTINE
              MOVE POFF-NETDUE TO NO-TDUEPOFF.


           MOVE POFF-NETDUE TO AVCR-POFF-NETDUE.
           MOVE CRNO-PAYDATE TO AVCR-PAYDATE.
      * ---THIS FLAG STOPS COMPUTE-AVAILABLE-CREDIT ROUTINE
      *    FROM COMPUTING A PAYOFF:
           MOVE "Y" TO AVCR-POFF-FG.
           MOVE CRNO-DEF-STOP TO AVCR-DEF-STOP.
           PERFORM COMPUTE-AVAILABLE-CREDIT.
           MOVE AVCR-AVAILCREDIT TO NO-AVAILCREDIT.
           MOVE AVCR-RESTRICT-FG TO CRNO-AVCR-RESTRICT-FG
                                    NO-AVCR-RESTRICT-FG.
           MOVE AVCR-CURRENT-AVAIL TO NO-CURRENT-AVAIL.

      * IF WDT, AVAIL CREDIT ROUTINE PERFORMS PAYOFF-LOAN ROUTINE FOR
      * A PB PAYOFF TO GET NETDUE FOR THE CURRENT AVAILABLE FIELD,
      * SO MUST DO PAYOFF ROUTINE HERE
           IF BR-AVAIL-CREDIT = "S"
              MOVE CRNO-PAYDATE TO NO-TDUE-DATE POFF-PAYDATE
              MOVE CRNO-MAKERCD TO POFF-MAKERCD
              MOVE CRNO-DEF-STOP TO POFF-DEF-STOP
              MOVE CRNO-LPTRCD TO POFF-LPTRCD
              MOVE CRNO-RECAST-FG TO POFF-RECAST-FG
              PERFORM PAYOFF-LOAN-ROUTINE
              MOVE POFF-NETDUE TO NO-TDUEPOFF.

      * TEST TO GET PAYOFF ONLY:
           IF CRNO-GET-PAYOFF-ONLY = "Y"
              GO TO CREATE-NOREC-REC-EXIT.

      * COMPUTE PRINCIPAL PAYMENTS DUE TODAY:
           MOVE LN-1STPYDATE TO CEPP1-DATE.
           MOVE NO-TDUE-DATE TO CEPP2-DATE.
           PERFORM CONTRACTUAL-DELINQUENCY.
           IF CDEL-DELINQUENT < 0
              SUBTRACT CDEL-DELINQUENT FROM 0
                                   GIVING NO-TDUECONTR.
           IF CDEL-ARREARAGE < 0
              SUBTRACT CDEL-ARREARAGE FROM 0
                                   GIVING NO-TDUEPRIN
                                          NO-TDUEAMT.
      * SET CONTRACTUAL PRINCIPLE (DELINQUENCY) FOR
      * (PC) ACCOUNTS.
            IF CDEL-DELINQUENT < 0
               IF IBPC-FG = "P"
                  SUBTRACT CDEL-DELINQUENT FROM 0
                                        GIVING NO-TDUEDELINQ.

      * SETUP LC OR INT AMOUNTS:
           ADD POFF-LCDUE TO NO-TDUELC
                             NO-TDUEAMT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * JTG 000310 CHANGED NO-LCHG-INTDUE TO REFLECT ALL LATE CHARGES
      *            PLUS INTEREST DUE TODAY:
      *
           MOVE LN-LCPDTH-DATE TO LCHG-LCPDTH-DATE.
           MOVE PDTH-CUR-SCHD-PAY TO LCHG-CURPAY
                                     LCHG-LATE.
           PERFORM LATE-CHARGE-CALCULATION.
           MOVE LCHG-CHARGE TO NO-LATE-CHG.

           ADD POFF-INTDUE POFF-LCDUE GIVING NO-LCHG-INTDUE.
      *
      *
      *   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *   ONLY TAKE LATE CHARGES WHEN THE ACCOUNT IS NOT ACCELERATED
      *   AND IT IS IN A PRECOMP STATUS OR IS LOAN CLASS TYPE IB
      *    IF (LN-ACCELDATE = 0) AND
      *       (IBPC-FG = "P" OR LN-LOANTYPE = "I")
      *       MOVE LN-LCPDTH-DATE TO LCHG-LCPDTH-DATE
      *       MOVE PDTH-CUR-SCHD-PAY TO LCHG-CURPAY
      *                                 LCHG-LATE
      *       PERFORM LATE-CHARGE-CALCULATION
      *       ADD POFF-INTDUE LCHG-CHARGE GIVING NO-LCHG-INTDUE
      *       MOVE LCHG-CHARGE TO NO-LATE-CHG
      *    ELSE
      *       MOVE POFF-LCDUE TO NO-LATE-CHG
      *       ADD POFF-INTDUE POFF-LCDUE GIVING NO-LCHG-INTDUE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    SETUP TRUE PAST DUE DAYS
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE CRNO-PAYDATE TO PDUE-TODAY-DATE.
           MOVE LN-1STPYDATE TO PDUE-1STPYDATE.
           PERFORM PAST-DUE-DAYS-CALCULATION.
           MOVE PDUE-DAYS TO NO-PAST-DUE-DAYS.

      * COMPUTE MATURITY DATE:
           PERFORM MATURITY-DATE-CALCULATION.
           IF NO-TDUE-DATE > MDTE-DATE
              MOVE NO-TDUEPOFF TO NO-TDUEAMT.

      ***********************************************
      *    SETUP LAST AND NEXT DUE DATES:
      ***********************************************
           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.

           MOVE LN-1STPYDATE TO NDTE-DATE.
           MOVE NDTE-DD      TO CRNO-HOLDDA.
           COMPUTE NDTE-HOLD = LN-TOTNODEF + LN-TOTNOALDEL - 1.
           PERFORM INCREMENT-UNITPER.

           MOVE NDTE-DATE    TO NUM-DATE.
           MOVE NO-TDUE-DATE TO SYS-DATE.
           PERFORM TIMUPER.
           SUBTRACT 1 FROM ELAPSED-UNITPER GIVING NDTE-HOLD.

       CREATE-NOREC-INCR-DATE.
           IF LN-UNITPER-CD = "M"
              MOVE CRNO-HOLDDA TO NDTE-DD.

           MOVE NDTE-DATE TO NO-LDUE-DATE NUM-DATE.
           PERFORM INCREMENT-UNITPER.

      * 1STPYDATE 10/14/01 "SEMI"
      * TODAY = 3/26/02   LCPDTH 3/14/02
      * THE INQUIRIES WERE SHOWING  LAST DUE = 3/13 (SHOULD BE 14)
      *                            TODAY DUE = 3/26
      *                             NEXT DUE = 3/28 (SHOULD BE 29)
      * 1STPYDATE 1/30/02 "SEMI" LOANDATE 1/15/02
      * TODAY = 3/26/02   LCPDTH 2/28/02
      * THE INQUIRIES WERE SHOWING  LAST DUE = 3/13 (SHOULD BE 15)
      *                            TODAY DUE = 3/26
      *                             NEXT DUE = 3/28 (SHOULD BE 30)
      * BAH PL# 357 (SAME LOGIC IN LPLCAP FROM JTG AND CS)
      *
      *  2/16/2022 CINDY - IF USING A 1ST PAY DAY OF 30TH, NEXT DUE
      *             DATE WILL BE 3/13 AND SHOULD BE THE 15TH.
           IF LN-UNITPER-CD = "S"
              IF (NUM-DA = 28 OR 29) AND NDTE-MM = 03
                 IF LN-1STPYDA = 14 OR 15
                    MOVE LN-1STPYDA TO NDTE-DD
                 ELSE
                 IF LN-1STPYDA = 29 OR 30 OR 31
                    SUBTRACT NUM-DA FROM LN-1STPYDA GIVING NDTE-HOLD
                    PERFORM INCREMENT-DAYS.

      * BAH PL# 357

           IF NO-TDUE-DATE = NDTE-DATE
              MOVE 1 TO NDTE-HOLD
              PERFORM INCREMENT-UNITPER
              MOVE NDTE-DATE TO NO-NXDUE-DATE
           ELSE
              IF NO-TDUE-DATE < NDTE-DATE
                 MOVE NDTE-DATE TO NO-NXDUE-DATE
              ELSE
                 MOVE 1 TO NDTE-HOLD
                 GO TO CREATE-NOREC-INCR-DATE.

      * ADJUST DUE DATES TO PAYDATE:
           IF LN-UNITPER-CD = "M"
              MOVE NO-LDUE-DATE TO NDTE-DATE
              MOVE CRNO-HOLDDA TO NDTE-DD
              PERFORM NEW-DATE-CALCULATION
              MOVE NDTE-DATE TO NO-LDUE-DATE
              MOVE NO-NXDUE-DATE TO NDTE-DATE
              MOVE CRNO-HOLDDA TO NDTE-DD
              PERFORM NEW-DATE-CALCULATION
              MOVE NDTE-DATE TO NO-NXDUE-DATE.

      * TEST IF NEXT DUE IS PRIOR TO PAID THRU:
           IF (NO-NXDUE-DATE > NO-PDTH-DATE) OR
              (BR-PDTH-NXDUE = "Y" AND (NO-NXDUE-DATE = NO-PDTH-DATE) )
              NEXT SENTENCE
           ELSE
              MOVE NO-PDTH-DATE TO NDTE-DATE
              MOVE 1 TO NDTE-HOLD
              IF LN-UNITPER-CD = "M"
                 MOVE CRNO-HOLDDA TO NDTE-DD
                 PERFORM INCREMENT-UNITPER
                 MOVE NDTE-DATE TO NO-NXDUE-DATE
              ELSE
                 PERFORM INCREMENT-UNITPER
                 MOVE NDTE-DATE TO NO-NXDUE-DATE.

      * TEST IF AFTER MATURITY DATE:
           IF NO-LDUE-DATE NOT < MDTE-DATE
              MOVE MDTE-DATE TO NO-LDUE-DATE
              MOVE 0 TO NO-NXDUE-DATE
              GO TO NOTICE-RECORD-LDUE
           ELSE
              IF NO-NXDUE-DATE > MDTE-DATE
                 MOVE 0 TO NO-NXDUE-DATE
                 GO TO NOTICE-RECORD-LDUE.

      * TEST IF PRIOR TO 1STPYDATE:
           IF LN-1STPYDATE > NO-NXDUE-DATE
              MOVE 0 TO NO-LDUE-DATE
                        NO-NXDUE-DATE
           ELSE
              IF LN-1STPYDATE > NO-LDUE-DATE
                 MOVE 0 TO NO-LDUE-DATE.

      ***********************************************
      *    COMPUTE LAST DUE FIGURES:
      ***********************************************
       NOTICE-RECORD-LDUE.
           IF NO-LDUE-DATE NOT = 0
              MOVE LN-1STPYDATE TO CEPP1-DATE
              MOVE NO-LDUE-DATE TO CEPP2-DATE

      *     ....................................................
      *     SET CDEL-LPCRNO-FG TO LET CONTRACTUAL-DELINQUENCY
      *     ROUTINE COMPUTE CORRECT AMOUNT DUE WHEN LAST DUE
      *     DATE DOSN'T FALL ON MONTHLY CYCLE EX. PAYMENTS DUE
      *     ON 31TH AND LAST DUE IS 02/28
              MOVE "Y" TO CDEL-LPCRNO-FG
              PERFORM CONTRACTUAL-DELINQUENCY
              MOVE " " TO CDEL-LPCRNO-FG
      *     ....................................................

              IF CDEL-DELINQUENT < 0
                 SUBTRACT CDEL-DELINQUENT FROM 0
                                      GIVING NO-LDUECONTR
              END-IF
              IF CDEL-ARREARAGE < 0
                 SUBTRACT CDEL-ARREARAGE FROM 0
                                      GIVING NO-LDUEAMT
                 MOVE 0 TO LCAP-OWE
      *   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *         ONLY TAKE LATE CHARGES WHEN THE ACCOUNT IS NOT ACCELERATED
      *         AND IT IS IN A PRECOMP STATUS OR IS LOAN CLASS TYPE IB
                 IF (LN-ACCELDATE = 0) AND
                    (IBPC-FG = "P" OR LN-LOANTYPE = "I")
                    MOVE NO-LDUE-DATE TO LCAP-PAYDATE
                    MOVE LN-1STPYDATE TO LCAP-1STPYDATE
                    MOVE "AL" TO LCAP-LPTRCD
                    MOVE 0 TO LCAP-LPAPLC
                              LCAP-LPAPINT
                    PERFORM LATE-CHARGE-APPLY
                 END-IF
                 ADD LN-LCBAL LCAP-OWE GIVING NO-LDUELC
                 ADD NO-LDUELC TO NO-LDUEAMT.

      ***********************************************
      *    COMPUTE NEXT DUE FIGURES:
      ***********************************************
       NOTICE-RECORD-NXDUE.
           IF NO-NXDUE-DATE NOT = 0
              MOVE LN-1STPYDATE TO CEPP1-DATE
              MOVE NO-NXDUE-DATE TO CEPP2-DATE
      *     ....................................................
      *     SET CDEL-LPCRNO-FG TO LET CONTRACTUAL-DELINQUENCY
      *     ROUTINE COMPUTE CORRECT AMOUNT DUE WHEN NEXT DUE
      *     DATE DOSN'T FALL ON MONTHLY CYCLE EX. PAYMENTS DUE
      *     ON 31TH AND LAST DUE IS 02/28
              MOVE "Y" TO CDEL-LPCRNO-FG
              PERFORM CONTRACTUAL-DELINQUENCY
              MOVE " " TO CDEL-LPCRNO-FG
      *     ....................................................
              IF CDEL-DELINQUENT < 0
                 SUBTRACT CDEL-DELINQUENT FROM 0
                                      GIVING NO-NXDUECONTR
              END-IF
              IF CDEL-ARREARAGE < 0
                 SUBTRACT CDEL-ARREARAGE FROM 0
                                      GIVING NO-NXDUEAMT
                 MOVE 0 TO LCAP-OWE
      *         - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *         ONLY TAKE LATE CHARGES WHEN THE ACCOUNT IS NOT ACCELERATED
      *         AND IT IS IN A PRECOMP STATUS OR IS LOAN CLASS TYPE IB
                 IF (LN-ACCELDATE = 0) AND
                    (IBPC-FG = "P" OR LN-LOANTYPE = "I")
                    MOVE NO-NXDUE-DATE TO LCAP-PAYDATE
                    MOVE LN-1STPYDATE TO LCAP-1STPYDATE
                    MOVE "AL" TO LCAP-LPTRCD
                    MOVE 0 TO LCAP-LPAPLC
                              LCAP-LPAPINT
                    PERFORM LATE-CHARGE-APPLY
                 END-IF
                 ADD LN-LCBAL LCAP-OWE GIVING NO-NXDUELC
                 ADD NO-NXDUELC TO NO-NXDUEAMT.

      ***********************************************
      *    SETUP TRUE DUE DATE BASED ON PAID THRU
      ***********************************************
       UPDATE-TRUE-DUE-DATE.
           MOVE NO-PDTH-DATE TO NDTE-DATE.
      * ADDED IF 3/16/98, 1STPYDATE 7/30/97 , PAID THRU 2/28/98,
      * NEXT DUE 3/30/98 BUT TRUEDUE WAS SHOWING 3/28/98
           IF LN-UNITPER-CD = "M"
              MOVE CRNO-HOLDDA TO NDTE-DD.
           MOVE 1 TO NDTE-HOLD.
           PERFORM INCREMENT-UNITPER.
           MOVE NDTE-DATE TO NO-TRUE-DUE-DATE.

           MOVE INDU-PENALTY          TO NO-INDU-PENALTY.

      ***********************************************
      *    SETUP DEFERMENT AMOUNT FOR TODAY
      ***********************************************
           IF LN-CURBAL NOT = 0
              MOVE "DF" TO DEFPOL-TRCD
              MOVE CRNO-PAYDATE TO DEFPOL-DATE
              PERFORM VALIDATE-DF-POLICY
              IF DEFPOL-MAXIMUM = " "
                 MOVE 1            TO DEF-NODEF
                 MOVE CRNO-PAYDATE TO DEF-PAYDATE
                 MOVE POFF-INTDUE  TO DEF-TOTAL-INTDUE
                 PERFORM DEF-COMPUTE-DEFERMENT
                 MOVE DEF-AMOUNT   TO NO-DEFERMENT.

      ***********************************************
      *    SETUP LAST PAYMENT INFORMATION
      ***********************************************
       UPDATE-LAST-PAYMENT.
           MOVE 0 TO NO-DATE-PAID-LAST
                     NO-AMT-PAID-LAST.
           IF CRNO-LPREC-FG NOT = " "
              GO TO CREATE-NOREC-REC-EXIT.

           PERFORM OPEN-LP1-FILE.

           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE 99999         TO LP-SEQNO
                                 QLP1-WEND-SEQNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.

      *     PERFORM START-LP1-FILE-NOT-GREATER.
      *     PERFORM READ-LP1-FILE-PREVIOUS.
      *     PERFORM CLOSE-LP1-FILE.
      *     IF IO-BAD OR (LP-BRNO NOT = LP-PATH-OWNBR)
      *                OR (LN-ACCTNO NOT = LP-ACCTNO)
      *                 OR (LP-SEQNO = 1)
      *        GO TO CREATE-NOREC-REC-EXIT.

           PERFORM READ-LP1-FILE-LAST-SEQ.
           IF IO-BAD
              GO TO CREATE-NOREC-REC-EXIT.

           MOVE LP-PAYDATE TO NO-DATE-PAID-LAST.
           MOVE LP-TRAMT   TO NO-AMT-PAID-LAST.
           MOVE LP1-KEY    TO NO-LSTLPKEY.

       CREATE-NOREC-REC-EXIT.

           MOVE " " TO CRNO-LPREC-FG
                       CRNO-RECAST-FG
                       CRNO-GET-PAYOFF-ONLY
                       CRNO-DEF-STOP.

           IF GP-IGNORE-LCHG-NXDUE = "Y"
              IF CRNO-HOLD-CONTRFRMLA = "A"
                 MOVE CRNO-HOLD-CONTRFRMLA TO SP-CONTRFRMLA
                 MOVE CRNO-HOLD-TOTPAYMNTD TO LN-TOTPAYMNTD.

      ***************************************
       CREATE-NOREC-INITIALIZE SECTION.
      ***************************************
           PERFORM CLEAR-NO-FILE.

           MOVE LN-OWNBR  TO NO-BRNO.
           MOVE LN-ACCTNO TO NO-ACCTNO.

      *-----------------------------------------------------------------
       READ-LP1-FILE-LAST-SEQ SECTION.
           PERFORM READ-IT.
           MOVE LP-PATH-SQL    TO E-FILE.
           MOVE LP1-KEY        TO E-KEYX.
           INITIALIZE QLP-REC.

            EXEC SQL
            SELECT LPFILE.LP_BRNO,
                   LPFILE.LP_ACCTNO,
                   LPFILE.LP_SEQNO,
                   CAST(LPFILE.LP_PAYDATE AS VARCHAR(10)),
                   LPFILE.LP_TRAMT
              INTO
                  :QLP-BRNO,
                  :QLP-ACCTNO,
                  :QLP-SEQNO,
                  :QLP-PAYDATE,
                  :QLP-TRAMT
              FROM DBO.LPFILE
             WHERE LPFILE.LP_BRNO   = :QLP1-WBEG-BRNO
               AND LPFILE.LP_ACCTNO = :QLP1-WBEG-ACCTNO
               AND LPFILE.LP_SEQNO  =
                  (SELECT MAX(LPFILE.LP_SEQNO)
                     FROM DBO.LPFILE
                    WHERE LPFILE.LP_BRNO   = :QLP1-WBEG-BRNO
                      AND LPFILE.LP_ACCTNO = :QLP1-WBEG-ACCTNO
                      AND LPFILE.LP_SEQNO > 1)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LP1-FILE-LAST-SEQ.

           IF ( IO-FG = 0 )
              PERFORM GET-LP-FIELDS.
