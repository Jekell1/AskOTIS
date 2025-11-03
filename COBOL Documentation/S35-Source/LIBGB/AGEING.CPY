      * COPYMEMBER: LIBGB/AGEING
      *************************************************************************
      *   NAME:  AGEING - COMPUTE-RECENCY
      *   DESC:  GIVEN AGEING DATE (MMDDYY) COMPUTE
      *          RECENCY DAYS AND TABLE SUBSCRIPT
      *          FROM LAST PMT, OR LOAN DATE, IF NO
      *          PAYMENTS HAVE BEEN MADE
      *   IN  :  AGEING-DATE
      *   OUT :  RSUB
      *          RECENCY
      *          RDSUB
      *          RECDEL
      *          RDPOT30        ' ' = N/A
      *                         'Y' = POTENTIAL 30
      *          RDPOTCHGOFF    ' ' = N/A
      *                         'Y' = POTENTIAL CHARGE OFF
      *                         'M' = MISSED CHARGE OFF
      *   USED:  LN-LOANDATE, LN-DATE-PAID-LAST
      *-----------------------------------------------
      *
      *   RSUB
      *   RDSUB     INTERNAL          EXTERNAL
      *   -----     ------------      ------------
      *     1   -   CUR               30
      *     2   -    30               60
      *     3   -    60               90
      *     4   -    90              120
      *     5   -   120              150
      *     6   -   150              180
      *     7   -   180              210+
      *     8   -   210+             N/A
      *     9   -   N/A              POTENTIAL 30
      *    10   -   POTENTIAL 30     CURRENT
      *-----------------------------------------------
      * REV :
      *  JTG 072195 ADDED LOGIC FOR SP-RDPOT30-CD = 'A'
      *                             SP-RDPOTCHGOFF-CD = 'A'
      *  MJD 071595 ADDED A50 LOGIC FOR SP-BANKUNITPER-CD = "A".
      *  BLV 041097 IF SP PAY FREQ CODE = "A" (INSTANT), FORCE 100%
      *             TO PDTHRU % FOR COMPUTING PD THRU FOR AGEING; TO FIX:
      *             AGEING-HOLD-REMAIN > 50%  CAUSE PDTHRU% IS <100, SO
      *             90 ACCT IS AGED AS 60; 60 IS AGED AS 30, ETC.
      *  JTG 091597 ADDED LOGIC FOR NEW STAT/AGEING BUCKETS
      *  JTG 091697 ADDED LOGIC FOR 'D' WORLD A90, SP-BANKRULE & SP-TRWBANKRULE
      *  JTG 990419 ADDED SP-RECFRMLA-CALENDAR-DUEDAY  "D"         FROM REL: A50
      *                   SP-RECFRMLA-PERCNT-PMT       "P"         MERCURY  #183
      *  MJD 090427 ADDED SP-BANKRULE "E"                  (BARNETT) 71284 #3189
      *  MJD 110823 ADDED SP-BANKRULE "F" MX NON MONTHLY   (WORLD) #752
      *  MJD 120402 FIXED BAD 'IF' STATEMENT FOR BANKRULE "D" AND "F"
      *  BLM 160615 FOR BANKRULE R & S, CALL GETLTR SINCE REVOLVING FIELDS 
      *             ARE NOW IN LTRFILE, NOT LNFILE, PD0003
      * KEC 2016.1116 ADDED LTR-SEQNO TO LTR1-KEY.
      * BLM 2017.0208 FIX LOGIC USING LN-PY-YYYYMM-RECDEL FOR 8 DIGIT DATES
      * BAH 2017.0914 RENAMED LN-PYMMYY-RECDEL TO LN-PY-YYYYMM-RECDEL AND FIXED
      *               LOGIC TO USE IT CORRECTLY
      * BAH 2019.0626 REMOVED BANKRULE "R" AND "S" REVOLVING
      *************************************************************************
       COMPUTE-RECENCY SECTION.

      * NOTE:
      *  WHEN SP-BANKRULE = 'D':
      *    -  RECENCY-ROUTINE IS COMPUTED IN COMPUTE-CONTRACTUAL
      *
      *    -  AFTER A MONTHEND:
      *     [ RECENCY CURRENT # & $ ]  =  [ CONTRACTUAL CURRENT # & $ ]
      
           IF SP-BANKRULE = "D"
              PERFORM COMPUTE-CONTRACTUAL
              GO TO COMPUTE-RECENCY-EXIT
           ELSE
              IF SP-BANKRULE = "F"
                 PERFORM COMPUTE-CONTRACTUAL
                 GO TO COMPUTE-RECENCY-EXIT
              ELSE
                 PERFORM RECENCY-ROUTINE.

           PERFORM COMPUTE-RECENCY-SET-EXTERNAL.

       COMPUTE-RECENCY-EXIT.
           EXIT.

      **************************************************
       COMPUTE-RECENCY-SET-EXTERNAL SECTION.
      **************************************************

      * SET EXTERNAL INDEXES FROM INTERNAL:
      
           IF RSUB = 1
              MOVE 10 TO RSUB
           ELSE
              SUBTRACT 1 FROM RSUB.

           IF RDSUB = 1
              MOVE 10 TO RDSUB
           ELSE
              SUBTRACT 1 FROM RDSUB.

      ************************************************************
       RECENCY-ROUTINE SECTION.
      ************************************************************
           MOVE 0 TO RECENCY
                     RECDEL.
           MOVE 1 TO RSUB
                     RDSUB.
           MOVE " " TO RDPOT30
                       RDPOTCHGOFF.

           IF LN-CURBAL = 0
              GO TO RECENCY-ROUTINE-EXIT.

           IF LN-DATE-PAID-LAST = 0
              MOVE LN-LOANDATE       TO NUM-DATE
           ELSE
              MOVE LN-DATE-PAID-LAST TO NUM-DATE.
           MOVE AGEING-DATE          TO SYS-DATE.

           PERFORM COMPUTE-TRUE-RECENCY.
           IF SP-RECFRMLA = " " AND
              (SP-BANKRULE NOT = "D") AND (SP-BANKRULE NOT = "F")
                  MOVE RECENCY TO RECDEL
                  MOVE RSUB TO RDSUB
           ELSE
              PERFORM COMPUTE-RECENCY-DELINQ.

       RECENCY-ROUTINE-EXIT.
           EXIT.

      ****************************************************************
       COMPUTE-TRUE-RECENCY SECTION.
      ****************************************************************
           PERFORM TIM.
           IF ELAPSED-DAYS < 0
              MOVE 0 TO ELAPSED-DAYS.
           MOVE ELAPSED-DAYS TO RECENCY.
           COMPUTE RSUB = (ELAPSED-DAYS / 30) + 1.

           IF RSUB > 8
              MOVE 8 TO RSUB.

      *************************************************************************
      *    RECENCY / DELINQUENCY
      * REV :
      *  JTG 072195 ADDED LOGIC FOR SP-RDPOT30-CD = 'A'
      *                             SP-RDPOTCHGOFF-CD = 'A'
      *  JTG 091597 ADDED LOGIC FOR NEW STAT/AGEING BUCKETS
      *  JTG 091697 ADDED LOGIC FOR 'D' WORLD A90, SP-BANKRULE & SP-TRWBANKRULE
      *  JTG 990419 ADDED SP-RECFRMLA-CALENDAR-DUEDAY  "D"         FROM REL: A50
      *                   SP-RECFRMLA-PERCNT-PMT       "P"         MERCURY  #183
      *************************************************************************
       COMPUTE-RECENCY-DELINQ SECTION.
           IF (SP-BANKRULE = "D")
              PERFORM COMPUTE-RECENCY-DELINQ-D
           ELSE
           IF (SP-BANKRULE = "F")
              PERFORM COMPUTE-RECENCY-DELINQ-F
           ELSE
              IF SP-RECFRMLA-PERCNT-PMT
                 IF SP-RECPDTH = 0
                    MOVE .01 TO AGEING-WORK
                 ELSE
                    COMPUTE AGEING-WORK ROUNDED =
                         LN-REGPYAMT * SP-RECPDTH * .01
                 END-IF
                 IF LN-MTD-PAYS-RECDEL >= AGEING-WORK
                    MOVE 1 TO RDSUB
                 ELSE
                    IF LN-PY-YYYYMM-RECDEL NOT = 0
                       MOVE LN-PY-YYYYMM-RECDEL TO DATE-YYYYMM
                       MOVE DATE-YYYYMM-MM      TO NUM-MO
                       MOVE DATE-YYYYMM-CC      TO NUM-CC
                       MOVE DATE-YYYYMM-YY      TO NUM-YR
                    ELSE
                       MOVE LN-LOANDATE TO NUM-DATE
                    END-IF
                    MOVE AGEING-DATE    TO SYS-DATE
                    MOVE 5              TO NUM-DA S-DD
                    PERFORM TIM360
                    MOVE ELAPSED-MONTHS TO RDSUB
                 END-IF
              ELSE
                 IF SP-RECFRMLA-CALENDAR-DUEDAY
                    IF S-DD < LN-1STPYDA
                       MOVE 5 TO NUM-DA
                       MOVE 4 TO S-DD
                    ELSE
                       MOVE 5 TO NUM-DA S-DD
                    END-IF
                 ELSE
                    MOVE 5 TO NUM-DA S-DD
                 END-IF
                 PERFORM TIM360
                 MOVE ELAPSED-MONTHS TO RDSUB.

      ****************************************************************
      * TEST FOR POTENTIAL 30'S, SET RDPOT30
      ****************************************************************
      
      * CURRENTLY RDPOT30 IS ONLY SET IN COMPUTE-RECENCY-DELINQ-D,
      * IF WORLD SP-BANKRULE = 'D'
      
           IF SP-BANKRULE = "F"
              IF RDSUB > 2
                 DIVIDE RDSUB BY 2 GIVING AGEING-SUB-WORKER
                                   REMAINDER AGEING-SUB-REMAIN
                 IF AGEING-SUB-REMAIN NOT = 0
                    ADD 1 TO AGEING-SUB-WORKER
                 END-IF
                 MOVE AGEING-SUB-WORKER TO RDSUB.

      ****************************************************************
      *    TEST FOR POTENTIAL CHARGE OFF'S
      ****************************************************************

      * MERCURY POTENTIAL CHARGE OFF'S CODE = 'A'

           IF SP-RDPOTCHGOFF-CD = "A"
              IF RDSUB > 5
                 IF LN-PLCD = " "
                    IF RDSUB = 6
                       MOVE "Y" TO RDPOTCHGOFF
                    ELSE
                    IF RDSUB > 6
                       MOVE "M" TO RDPOTCHGOFF.

           IF ELAPSED-MONTHS < 1
              MOVE 1 TO RDSUB.

           COMPUTE RECDEL = RDSUB * 30 - 30.

           IF RDSUB > 8
              MOVE 8 TO RDSUB
           ELSE
              IF RDPOT30 = "Y"
                 MOVE 10 TO RDSUB.

       COMPUTE-RECENCY-DELINQ-EXIT.
           EXIT.

      *****************************************************************
      * RECENCY/DELINQUENCY WORLD FORMULA 'D':
      *
      * REV:
      *  111293 JTG CORRECTED LOGIC FOR LN-1STPYDA PAST 25TH
      *  120493 JTG MODIFIED TO CONFORM TO CHARLIE WALTERS DEFINITION
      *****************************************************************
       COMPUTE-RECENCY-DELINQ-D SECTION.

      * N O T E:
      *     MUST HAVE COMPUTED CONTRACTUAL BEFORE THIS ROUTINE
      *     WILL WORK!
      *
      *     RECENCY IS CALLED FROM COMPUTE-CONTRACTUAL-D.
      
      * TEST FOR CURRENT ACCOUNT:
           IF CSUB = 1
              MOVE 1 TO RDSUB
              IF LN-MTD-PAYS-RECDEL NOT < LN-REGPYAMT
                 GO TO COMPUTE-RECENCY-DELINQ-D-EXIT
              ELSE
                 MOVE CPOT30 TO RDPOT30
                 GO TO COMPUTE-RECENCY-DELINQ-D-EXIT.

      * ACCOUNT IS PAST DUE:
           IF LN-MTD-PAYS-RECDEL NOT < LN-REGPYAMT
              MOVE 1 TO RDSUB
              GO TO COMPUTE-RECENCY-DELINQ-D-EXIT.

           MOVE AGEING-DATE TO NDTE-DATE.
           MOVE 35          TO NDTE-DD.
           MOVE -1          TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE TO SYS-DATE.

           IF LN-PY-YYYYMM-RECDEL NOT = 0
              MOVE LN-PY-YYYYMM-RECDEL TO DATE-YYYYMM
              MOVE DATE-YYYYMM-MM     TO NDTE-MM
              MOVE DATE-YYYYMM-CC     TO NDTE-CC
              MOVE DATE-YYYYMM-YY     TO NDTE-YY
              MOVE S-DD               TO NDTE-DD
           ELSE
              MOVE LN-1STPYDATE TO NDTE-DATE.

           MOVE -1 TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE TO NUM-DATE.

           PERFORM TIM360.
           IF ELAPSED-MONTHS = 1
              MOVE "Y" TO RDPOT30.
           MOVE ELAPSED-MONTHS TO RDSUB.
           IF RDSUB < 1
              MOVE 1 TO RDSUB.

           IF RDSUB > CSUB
              MOVE CSUB TO RDSUB.

       COMPUTE-RECENCY-DELINQ-D-EXIT.
           EXIT.

      *****************************************************************
      * RECENCY/DELINQUENCY WORLD FORMULA 'F':
      *
      * REV:
      *  111293 JTG CORRECTED LOGIC FOR LN-1STPYDA PAST 25TH
      *  120493 JTG MODIFIED TO CONFORM TO CHARLIE WALTERS DEFINITION
      *****************************************************************
       COMPUTE-RECENCY-DELINQ-F SECTION.

      *  N O T E:
      *      MUST HAVE COMPUTED CONTRACTUAL BEFORE THIS ROUTINE
      *      WILL WORK!!!!!!!!!!!!!!
      *
      *      RECENCY IS CALLED FROM COMPUTE-CONTRACTUAL-F.
      

      * ATTEMPTING LOGIC TO DETERMINE HOW MANY CONTRACTUAL PAYMENTS
      * IN THE MONTH (1ST MONTH MAY ON KLY HAVE ON E DUE) WHEN COMPUTING
      * RECENCY.  AS PER JIM, REGARDLESS WE WILL ALWAYS REQUIRE TWO
      * TO BE CONTRACTUALLY RECENT.  LEFT LOGIC HERE AS A BASE
      * IN CASE IT COMES UP AGAIN OR WE NEED TO MAKE WORK FOR
      * OTHER PAY FREQUENCIES
      *--->MOVE LN-1STPYDATE TO NUM-DATE.
      *    MOVE AGEING-DATE TO SYS-DATE.
      *    MOVE 1 TO NUM-DA S-DD.
      *    PERFORM CMPDAT.
      *    IF SYS-IS-EQ
      *       MOVE 1 TO AGEING-SUB-WORKER
      *--->ELSE
              MOVE 2 TO AGEING-SUB-WORKER.

      * TEST FOR CURRENT ACCOUNT:
           IF CSUB = 1
              MOVE 1 TO RDSUB
              IF  LN-MTD-PAYS-RECDEL NOT < (LN-REGPYAMT *
                                            AGEING-SUB-WORKER)
                 GO TO COMPUTE-RECENCY-DELINQ-F-EXIT
              ELSE
                 MOVE CPOT30 TO RDPOT30
                 GO TO COMPUTE-RECENCY-DELINQ-F-EXIT.

      * ACCOUNT IS PAST DUE:
           IF LN-MTD-PAYS-RECDEL NOT < (LN-REGPYAMT * AGEING-SUB-WORKER)
              MOVE 1 TO RDSUB
              GO TO COMPUTE-RECENCY-DELINQ-F-EXIT.

           MOVE AGEING-DATE TO NDTE-DATE.
           MOVE 35          TO NDTE-DD.
           MOVE -1          TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE TO SYS-DATE.

           IF LN-PY-YYYYMM-RECDEL NOT = 0
              MOVE LN-PY-YYYYMM-RECDEL TO DATE-YYYYMM
              MOVE DATE-YYYYMM-MM     TO NDTE-MM
              MOVE DATE-YYYYMM-CC     TO NDTE-CC
              MOVE DATE-YYYYMM-YY     TO NDTE-YY
              MOVE S-DD               TO NDTE-DD
           ELSE
              MOVE LN-1STPYDATE TO NDTE-DATE.

           MOVE NDTE-DATE TO NUM-DATE.

           MOVE LN-UNITPER-CD   TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM TIMUPER.
           IF ELAPSED-UNITPER = 1 OR 2
              MOVE "Y" TO RDPOT30.
           MOVE ELAPSED-UNITPER TO RDSUB.
           IF RDSUB < 1
              MOVE 1 TO RDSUB.

           IF RDSUB > CSUB
              MOVE CSUB TO RDSUB.
           IF RDSUB = 1 OR 2
              MOVE "Y" TO RDPOT30.

       COMPUTE-RECENCY-DELINQ-F-EXIT.
           EXIT.

      *************************************************************************
      *   NAME:  AGEING - COMPUTE-CONTRACTUAL
      *   DESC:  GIVEN AGEING DATE (MMDDYY) COMPUTE
      *          CONTRACTUAL DAYS AND TABLE SUBSCRIPT
      *
      *   IN  :  AGEING-DATE
      *   OUT :  CSUB
      *          CONTRACTUAL
      *          CPOT30          ' ' = N/A
      *                          'Y' = POTENTIAL 30
      *                          'M' = MISSPAY  (MISSED PAYMENT)
      *          CPOTCHGOFF      ' ' = N/A
      *                          'Y' = POTENTIAL CHARGE OFF
      *                          'M' = MISSED CHARGE OFF
      *          CTFC-NM          TFC NON MONTHLY PAPER AGEING TABLE POINTER
      *   USED:
      *------------------------------------------------------------------------
      *
      *   CSUB      INTERNAL          EXTERNAL
      *   -----     ------------      ------------
      *     1   -   CUR               30
      *     2   -    30               60
      *     3   -    60               90
      *     4   -    90              120
      *     5   -   120              150
      *     6   -   150              180
      *     7   -   180              210+
      *     8   -   210+             MISSPAY
      *     9   -   MISSPAY          POTENTIAL 30
      *    10   -   POTENTIAL 30     CURRENT
      *-----------------------------------------------
      * REV:
      *  JTG 050986 SET CEPP-NOCAP-FG SO ELAPSED PERIODS WOULD
      *             NOT BE CAPPED THRU MATURITY.
      *  JTG 052386 ADDED TEST ON SP-BANKRULE
      *             FOR HORRIGAN (CONTRACTUAL).
      *  JTG 060586 FIXED BUG BY ADDING 1 TO AGEING-HOLD
      *  JTG 103086 OPENED UP BANKRULE "B"
      *  JTG 041087 OPENED UP BANKRULE "C"
      *  JTG 040590 ADDED SET OF PDTH-ALDEL-FG WHICH IS PASSED
      *             BY COMPUTE-RECDEL
      *  JTG 051791 CHANGED ELAPSED PAY PERIOD CALC FOR PAID THRU
      *             TO NOT CAP ON MATURITY, SINCE AD'S ALLOW DELINQ
      *             EXTEND ACCOUNTS PAID THRU'S PAST MATURITY
      *  JTG 051895 ADDED MERCURY $2.00 SP-CONTR-CREDIT
      *              AND SP-CONTR-CREDIT-MIN-DEL-PER LOGIC
      *  JTG 072195 ADDED LOGIC FOR SP-CPOT30-CD = 'A'
      *                             SP-CPOTCHGOFF-CD = 'A'
      *  JTG 080696 CHANGED SO SP-BANKUNITPER-CD = "A"
      *             WOULD YIELD POTENTIAL'S
      *  JTG 121396 ADDED SP-BANKUNITPER-CD = 'B'
      *                   SP-TRWBANKUNITPER-CD = 'B'  (FL AUTO)
      *  MJD 121396 ADDED SP-BANKUNITPER-CD = 'C'
      *                   SP-TRWBANKUNITPER-CD = 'C'  (FL AUTO)
      *  JTG 121396 CHANGED SP-BANKUNITPER-CD = 'B' AND 'C'
      *             TO USE PDTH-CUR-SCHD-DATE
      *  JTG 042997 FIXED FOR BANKRULE 'A' & 'C'
      *             CHANGED FIND ELAPSED PERIODS THRU PAIDTHRU:
      *             WHEN DUE DATE IS EX. 30TH AND PAIDTHRU MONTH
      *             IS FEBURARY PDTH = 022897 AND WHEN TODAY IS
      *             EX. 043097 ELAPSED PAY PERIODS IS OFF BY 1
      *  JTG 080697 FIXED FOR BANKRULE 'B'
      *             WHEN DUE DATE IS EX. 30TH AND PAIDTHRU MONTH
      *             IS FEBURARY PDTH = 022897 AND WHEN TODAY IS
      *             EX. 043097 ELAPSED PAY PERIODS IS OFF BY 1
      *  JTG 082897 ADDED SP-BANKUNITPER-CD = 'D'
      *                   SP-TRWBANKUNITPER-CD = 'D'  (INSTANT AUTO)
      *                   NOTE: SAME AS 'A' EXCEPT 80% AND NOT 50%
      *  JTG 090897 CORRECTED SP-BANKUNITPER-CD = 'D', VIA 1.99 - .80
      *  JTG 091597 ADDED LOGIC FOR NEW STAT/AGEING BUCKETS
      *  JTG 112597 PUT BACK OLD LOGIC FOR SP-BANKUNITPER-CD 'A'
      *             PER KEVIN MULLEN, RE: INSTANT
      *  JTG 121897 CHANGED SP-BANKRULE (C) TO PROVIDE A BEGINNING OF
      *             THE MONTH AGEING ROUTINE FOR NON MONTHLY ACCOUNTS
      *  JTG 022498 CHANGED SP-BANKUNITPER-CD = 'A' & 'D', INSTANT AUTO
      *             TO DETERMINE EXACT PAYMENT REQUIREMENT FOR A GIVEN
      *             MONTH. EX. WEEKLY, JUL 5 PAYMENTS, AUG 4 PAYMENTS
      *  JTG 042998 CHANGED SP-BANKRULE (A) TO PROVIDE A BEGINNING OF
      *             THE MONTH AGEING ROUTINE FOR NON MONTHLY ACCOUNTS
      *  JTG 101598 ADDED LOGIC FOR REVOLVING SP-BANKRULE = 'R', REGENCY
      *  JTG 990309 ADDED SP-CPOTCHGOFF-CD-VALID 'B', REGENCY
      *  JTG 990311 ADDED SP-BANKUNITPER-CD & SP-TRWBANKUNITPER-CD = 'E' TFC
      *             AND ADDED OUTPUT OF TFC CTFC-NM TABLE INDEX
      *  JTG 990325 CORRECTED SP-BANKUNITPER-CD & SP-TRWBANKUNITPER-CD = 'E' TFC
      *  JTG 990331 CHANGED CALCULATION OF CONTRACTUAL FOR NON MONTHLY ACCOUNTS
      *             TO BE BASED ON THE NUMBER OF PAY PERIODS FROM
      *             ( PAID THRU + 1 PAY PERIOD ) TO AGEING DATE  [ CTFC-NM ]
      *  JTG 990625 ADDED LOGIC FOR REVOLVING SP-BANKRULE = 'S', REGENCY
      *             NOTE: 'R' WILL BECOME 'S' AND NEW LOGIC PUT INTO 'R' LOGIC
      *  JTG 990914 CORRECTED TO MAKE ACCOUNTS CURRENT AFTER CYCLE AND BEFORE
      *             MONTH END WHEN ONLY ONE AGEING BUCKET EXISTS
      *  BLV 000311 CORRECTED TFC NON-MONTHLY POTS TO INCLUDE ONLY 2 - 5 WEEKS
      *             PAST DUE (WERE INCLUDING 0 - 1 WEEK PAST DUE), TFC PR# 69
      *  BLV 000508 ADDED LOGIC FOR SP-CPOTCHGOFF CODE OF "C", 120+ TO BE
      *             CONSIDERED POTENTIAL CHARGE-OFFS, MERC PR# 230
      **************************************************************************
       COMPUTE-CONTRACTUAL SECTION.

           MOVE 0   TO CONTRACTUAL.
           MOVE 1   TO CSUB.
           MOVE 1   TO CTFC-NM.
           MOVE " " TO CPOT30
                       CPOTCHGOFF.

           IF LN-CURBAL = 0
              IF SP-BANKRULE = "D"
                 MOVE 1 TO RSUB
                           RDSUB
                 MOVE 0 TO RECENCY
                           RECDEL
                 MOVE " " TO RDPOT30
                             RDPOTCHGOFF
              END-IF
              IF SP-BANKRULE = "F"
                 MOVE 1 TO RSUB
                           RDSUB
                 MOVE 0 TO RECENCY
                           RECDEL
                 MOVE " " TO RDPOT30
                             RDPOTCHGOFF
              END-IF
              GO TO COMPUTE-CONTRACTUAL-EXIT.

      *********************************************************************
      *    SP-BANKRULE = D   (WORLD)
      *
      *    -  AFTER A MONTHEND:
      *     [ RECENCY CURRENT # & $ ]  =  [ CONTRACTUAL CURRENT # & $ ]
      *********************************************************************

           IF SP-BANKRULE = "D"
              PERFORM COMPUTE-CONTRACTUAL-GET-PDTH
              MOVE LN-1STPYDATE   TO CEPP1-DATE
              MOVE PDTH-DATE-FULL TO CEPP2-DATE
              MOVE "X"            TO CEPP-NOCAP-FG
              MOVE 1              TO CEPP1-DA
                                     CEPP2-DA
              PERFORM CONTRACTUAL-ELAPSED-PAYPER
              MOVE CEPP-PERIODS TO AGEING-HOLD
              PERFORM COMPUTE-CONTRACTUAL-D
              GO TO COMPUTE-CONTRACTUAL-POT-CHGOFF.

      *********************************************************************
      *    SP-BANKRULE = F   (WORLD MX NON MONTHLY)
      *
      *    -  AFTER A MONTHEND:
      *     [ RECENCY CURRENT # & $ ]  =  [ CONTRACTUAL CURRENT # & $ ]
      *********************************************************************
           IF SP-BANKRULE = "F"
              PERFORM COMPUTE-CONTRACTUAL-GET-PDTH
              MOVE LN-1STPYDATE   TO CEPP1-DATE
              MOVE PDTH-DATE-FULL TO CEPP2-DATE
              MOVE "X"            TO CEPP-NOCAP-FG
              PERFORM CONTRACTUAL-ELAPSED-PAYPER
              MOVE CEPP-PERIODS TO AGEING-HOLD
              PERFORM COMPUTE-CONTRACTUAL-F
              GO TO COMPUTE-CONTRACTUAL-POT-CHGOFF.

      *********************************************************************
      *    SP-BANKUNITPER-CD = E   (TFC)
      *
      *    CONTRACTUAL IS BASED ON THE NUMBER OF PAY PERIODS
      *    FROM ( PAID THRU + 1 PAY PERIOD ) TO AGEING DATE  [ CTFC-NM ]
      *
      *     0 -  1  CURRENT        14 - 17   90
      *     2 -  5  POT            18 - 21  120
      *     6 -  9  30             22 - 25  150
      *    10 - 13  60             26 - 29  180
      *                            30 - ..  210+
      *********************************************************************
           IF (LN-UNITPER-CD NOT = "M") AND (SP-BANKUNITPER-CD = "E")
                IF LN-CURBAL <= 0
                   MOVE 0 TO CSUB
                             CTFC-NM
                ELSE
                   PERFORM COMPUTE-CONTRACTUAL-GET-PDTH
                   MOVE PDTH-DATE-FULL TO NDTE-DATE
                   MOVE 1              TO NDTE-HOLD
                   PERFORM INCREMENT-UNITPER
                   MOVE NDTE-DATE   TO NUM-DATE
                   MOVE AGEING-DATE TO SYS-DATE
                   PERFORM TIM365
                   IF ELAPSED-DAYS <= 0
                      MOVE 0 TO CSUB
                                CTFC-NM
                   ELSE
                      COMPUTE CTFC-NM = ELAPSED-DAYS / 7
                      IF CTFC-NM = 0 OR 1
                         MOVE 0 TO CSUB
                      ELSE
                         IF CTFC-NM > 30
                            MOVE 30 TO CTFC-NM
                         END-IF
                         COMPUTE CSUB = (CTFC-NM + 2) / 4
                      END-IF
                   END-IF
                END-IF
                GO TO COMPUTE-CONTRACTUAL-POT30.

      *****************************************************************
      * SP-BANKRULE = A OR C  AND  OTHER THAN MONTHLY UNIT PERIODS
      *
      *****************************************************************
           IF NOT (SP-BANKUNITPER-CD = "A" OR "D" OR "E")
              IF SP-BANKRULE = "A" OR "C"
                 IF LN-UNITPER-CD NOT = "M"
                    PERFORM COMPUTE-CONTRACTUAL-GET-PDTH
                    MOVE PDTH-CUR-SCHD-DATE TO NUM-DATE
                    MOVE AGEING-DATE        TO SYS-DATE
                    MOVE 7                  TO NUM-DA
                                               S-DD
                    PERFORM TIM360
                    IF ELAPSED-MONTHS < 0
                       MOVE -1 TO ELAPSED-MONTHS
                    END-IF
                    MOVE ELAPSED-MONTHS TO CSUB
                    IF SP-BANKRULE = "C"
                       ADD 1 TO CSUB
                    END-IF
                    GO TO COMPUTE-CONTRACTUAL-POT30.

      *************************************************************
      *    SP-BANKUNITPER-CD = A & D  (INSTANT AUTO)
      *          A == 50% FACTOR
      *          D == 80% FACTOR
      *************************************************************

           IF SP-BANKUNITPER-CD = "A" OR "D"
              IF LN-UNITPER-CD = "M" AND LN-UNITPER-FREQ = 1
                 MOVE SP-CONTPDTH TO AGEING-HOLD-CONTPDTH
                 MOVE 100 TO SP-CONTPDTH
                 PERFORM COMPUTE-CONTRACTUAL-GET-PDTH
                 MOVE AGEING-HOLD-CONTPDTH TO SP-CONTPDTH
              ELSE
                 PERFORM COMPUTE-CONTRACTUAL-GET-PDTH
              END-IF
              PERFORM COMPUTE-CONTR-UNITP-A-D
              GO TO COMPUTE-CONTRACTUAL-POT30.

      *************************************************************
      *    SP-BANKUNITPER-CD = B   (FL AUTO)
      *
      *    CONTRACTUAL IS BASED ON PASTDUE MONTHS
      *************************************************************
           IF SP-BANKUNITPER-CD = "B"
              IF LN-UNITPER-CD NOT = "M"
                 PERFORM COMPUTE-CONTRACTUAL-GET-PDTH
                 MOVE PDTH-CUR-SCHD-DATE TO NUM-DATE
                 MOVE AGEING-DATE        TO SYS-DATE
                 PERFORM TIM360
                 COMPUTE CSUB = ELAPSED-MONTHS + 1
                 GO TO COMPUTE-CONTRACTUAL-POT30.

      *************************************************************
      *    SP-BANKUNITPER-CD = C   (FL AUTO)
      *
      *    CONTRACTUAL IS BASED ON TRUE PASTDUE DAYS
      *************************************************************
           IF SP-BANKUNITPER-CD = "C"
              IF LN-UNITPER-CD NOT = "M"
                 PERFORM COMPUTE-CONTRACTUAL-GET-PDTH
                 MOVE PDTH-CUR-SCHD-DATE TO NUM-DATE
                 MOVE AGEING-DATE        TO SYS-DATE
                 PERFORM TIM
                 IF ELAPSED-DAYS < 0
                    MOVE 0 TO CSUB
                 ELSE
                    COMPUTE CSUB = ELAPSED-DAYS / 30 + 1
                 END-IF
                 GO TO COMPUTE-CONTRACTUAL-POT30.

      * FIND ELAPSED PERIODS THRU TODAY:

           PERFORM COMPUTE-CONTRACTUAL-PAYS-TODAY.

      * GET PAIDTHRU:

           PERFORM COMPUTE-CONTRACTUAL-GET-PDTH.

      * FIND ELAPSED PERIODS THRU PAIDTHRU:

           MOVE LN-1STPYDATE TO CEPP1-DATE.
           MOVE PDTH-DATE-FULL TO CEPP2-DATE.
           MOVE "X" TO CEPP-NOCAP-FG.

      *JTG 080697 WHEN DUE DATE IS EX. 30TH AND PAIDTHRU MONTH
      *           IS FEBURARY PDTH = 022897 AND WHEN TODAY IS
      *           EX. 043097 ELAPSED PAY PERIODS IS OFF BY 1

           IF SP-BANKRULE = "B" OR "E"
              MOVE CEPP2-DA TO CEPP1-DA.

      *JTG 042997 WHEN DUE DATE IS EX. 30TH AND PAIDTHRU MONTH
      *           IS FEBURARY PDTH = 022897 AND WHEN TODAY IS
      *           EX. 043097 ELAPSED PAY PERIODS IS OFF BY 1

           IF SP-BANKRULE = "A" OR "C"
              MOVE 1 TO CEPP1-DA
                        CEPP2-DA.

           PERFORM CONTRACTUAL-ELAPSED-PAYPER.

      * COMPUTE CSUB:

           IF NOT (SP-BANKRULE = "B" OR "C" OR "E")
              ADD 1 TO CEPP-PERIODS.
           COMPUTE CSUB = AGEING-HOLD - CEPP-PERIODS.

      *    TEST FOR POTENTIAL 30'S AND MISSPAY'S
      
       COMPUTE-CONTRACTUAL-POT30.

      *************************************************************
      *   MERCURY POTENTIAL 30 CODE = 'A'
      *      ALL CURRENT ACCOUNTS THAT ARE NOT PAID AHEAD
      *      ARE CONSIDERED POTENTIAL 30
      *************************************************************

           IF SP-CPOT30-CD = "A"
      * CURRENT PAID AHEAD'S ARE NOT POTENTIAL 30:
              IF CSUB = 1
                 MOVE "Y" TO CPOT30.

      * TEST FOR POTENTIAL CHARGE OFF'S
      
       COMPUTE-CONTRACTUAL-POT-CHGOFF.

           IF SP-BANKRULE = "F"
              IF CSUB > 2
                 DIVIDE CSUB BY 2 GIVING AGEING-SUB-WORKER
                                  REMAINDER AGEING-SUB-REMAIN
                 IF AGEING-SUB-REMAIN NOT = 0
                    ADD 1 TO AGEING-SUB-WORKER
                 END-IF
                 MOVE AGEING-SUB-WORKER TO CSUB.

      *****************************************************
      *  POTENTIAL CHARGE OFF'S CODE = 'A'
      *    ALL ACCOUNTS THAT ARE 150 DAYS OR MORE
      *    (USED TO BE MERCURY'S CODE)
      *****************************************************

           IF SP-CPOTCHGOFF-CD = "A"
              IF CSUB > 5
                 IF LN-PLCD = " "
                    IF CSUB > 6
                       MOVE "M" TO CPOTCHGOFF
                    ELSE
                       MOVE "Y" TO CPOTCHGOFF
                    END-IF
                 END-IF
              END-IF
           ELSE
      * REGENCY POTENTIAL CHARGE OFF'S CODE = 'B'
      * ALL ACCOUNTS THAT ARE 180 DAYS OR MORE
           IF SP-CPOTCHGOFF-CD = "B"
              IF CSUB > 6
                 IF LN-PLCD = " "
                    IF CSUB > 7
                       MOVE "M" TO CPOTCHGOFF
                    ELSE
                       MOVE "Y" TO CPOTCHGOFF
                    END-IF
                 END-IF
              END-IF
      * MERCURY POTENTIAL CHARGE OFF'S CODE = 'C'
      * ALL ACCOUNTS THAT ARE 120 DAYS OR MORE
           ELSE
           IF SP-CPOTCHGOFF-CD = "C"
              IF CSUB > 4
                 IF LN-PLCD = " "
                    IF CSUB > 5
                       MOVE "M" TO CPOTCHGOFF
                    ELSE
                       MOVE "Y" TO CPOTCHGOFF.

       COMPUTE-CONTRACTUAL-CONTINUE.
           IF CSUB < 1
              MOVE 1 TO CSUB.

           COMPUTE CONTRACTUAL = (CSUB * 30) - 30.

           IF CSUB > 8
              MOVE 8 TO CSUB
           ELSE
           IF CPOT30 = "M"
              MOVE 9 TO CSUB
           ELSE
           IF CPOT30 = "Y"
              MOVE 10 TO CSUB.

       COMPUTE-CONTRACTUAL-EXIT.
           MOVE " " TO CSUB-ALDEL-FG.

      * SET EXTERNAL INDEXES FROM INTERNAL:
      
           IF CSUB = 1
              MOVE 10 TO CSUB
           ELSE
              SUBTRACT 1 FROM CSUB.

      ************************************************
      *    COMPUTE CONTRACURAL PAYS THRU TODAY
      ************************************************
       COMPUTE-CONTRACTUAL-PAYS-TODAY SECTION.
           MOVE LN-1STPYDATE TO CEPP1-DATE.
           MOVE AGEING-DATE  TO CEPP2-DATE.
           MOVE "X"          TO CEPP-NOCAP-FG.
           IF SP-BANKRULE = "A" OR "C"
              MOVE 1         TO CEPP1-DA
                                CEPP2-DA.
           PERFORM CONTRACTUAL-ELAPSED-PAYPER.
           MOVE CEPP-PERIODS TO AGEING-HOLD.

      ************************************************
      *    GET PAIDTHRU
      ************************************************
       COMPUTE-CONTRACTUAL-GET-PDTH SECTION.
           MOVE LN-TOTPAYMNTD TO AGEING-TOTPAYMNTD-SV.
           IF SP-BANKRULE = "E"
              COMPUTE AGEING-E-1STPYAMT ROUNDED =
                        (LN-1STPYAMT * SP-BANKRULE-RATE)/100
              COMPUTE AGEING-E-REGPYAMT ROUNDED =
                        (LN-REGPYAMT * SP-BANKRULE-RATE)/100
              COMPUTE AGEING-E-LASTPYAMT ROUNDED =
                        (LN-LASTPYAMT * SP-BANKRULE-RATE)/100
              MOVE "E" TO PDTH-E-OPTION
           ELSE
             MOVE LN-1STPYAMT  TO AGEING-E-1STPYAMT
             MOVE LN-REGPYAMT  TO AGEING-E-REGPYAMT
             MOVE LN-LASTPYAMT TO AGEING-E-LASTPYAMT.

           IF SP-CONTR-CREDIT-CD NOT = " "
      * CREDIT IS BASED ON TOTAL PAYS MADE:
              IF SP-CONTR-CREDIT-CD = "A"
                 COMPUTE AGEING-WORK =
                    ( LN-TOTPAYMNTD /
                           (AGEING-E-REGPYAMT - SP-CONTR-CREDIT)
                            - LN-TOTPAYMNTD / AGEING-E-REGPYAMT
                    ) * AGEING-E-REGPYAMT
                 COMPUTE LN-TOTPAYMNTD =
                                LN-TOTPAYMNTD + AGEING-WORK
                                 + SP-CONTR-CREDIT-MIN-DEL-PER
                                 * AGEING-E-REGPYAMT * .01
              ELSE
      * CREDIT IS BASED ON ELAPSED PAY PERIODS:
              IF SP-CONTR-CREDIT-CD = "B"
                 COMPUTE LN-TOTPAYMNTD =
                     LN-TOTPAYMNTD + CEPP-PERIODS * SP-CONTR-CREDIT
                     + SP-CONTR-CREDIT-MIN-DEL-PER
                     * LN-REGPYAMT * .01.

           MOVE LN-1STPYDATE  TO PDTH-DATE-WORK.
           MOVE CSUB-ALDEL-FG TO PDTH-ALDEL-FG.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-REMAIN   TO AGEING-HOLD-REMAIN.

           MOVE AGEING-TOTPAYMNTD-SV TO LN-TOTPAYMNTD.


      *******************************************************
      *    CONTRACTUAL FOR SP-BANKUNITPER-CD = 'A' OR 'D'
      *
      *          A == 50% FACTOR
      *          D == 80% FACTOR
      *
      *  NOTE:
      *       THIS FORMULA DEFAULTS TO SP-BANKRULE C
      *******************************************************
       COMPUTE-CONTR-UNITP-A-D SECTION.
           IF SP-BANKUNITPER-CD = "A"
              MOVE 0.50 TO AGEING-PERCENT-FACTOR
           ELSE
              MOVE 0.80 TO AGEING-PERCENT-FACTOR.

           IF LN-UNITPER-CD = "M" AND LN-UNITPER-FREQ = 1
              MOVE LN-UNITPER-CD   TO DATER-UNITPER-CD
              MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ
              MOVE PDTH-DATE-FULL  TO NUM-DATE
              MOVE AGEING-DATE     TO SYS-DATE
              MOVE 1               TO NUM-DA S-DD
              PERFORM TIMUPER
              IF AGEING-HOLD-REMAIN NOT < AGEING-PERCENT-FACTOR
                 SUBTRACT 1 FROM ELAPSED-UNITPER
              END-IF
              MOVE ELAPSED-UNITPER TO CSUB
              GO TO COMPUTE-CONTR-UNITP-A-D-EXIT.

      ********************************************************
      *    NON MONTHLY ACCOUNT LOGIC
      ********************************************************

      * SEE IF ACCOUNT IS NOT DUE THIS MONTH:

           MOVE AGEING-DATE        TO NUM-DATE.
           MOVE PDTH-CUR-SCHD-DATE TO SYS-DATE.
           MOVE 1                  TO NUM-DA S-DD.
           IF SYS-DATE > NUM-DATE
              MOVE 0 TO CSUB
              GO TO COMPUTE-CONTR-UNITP-A-D-EXIT.

           MOVE LN-UNITPER-CD   TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.

           MOVE LN-1STPYDATE TO NDTE-DATE.
           MOVE -1           TO NDTE-HOLD.
           PERFORM INCREMENT-UNITPER.
           MOVE NDTE-DATE    TO AGEING-WK-DATE.

      * GET NO. OF PAY PERIODS IN CURRENT PAID THRU MONTH
      * PRIOR TO AND INCLUDING PAID THRU:
      
           MOVE PDTH-DATE-FULL TO NUM-DATE
                                  NDTE-DATE.
           MOVE 0              TO AGEING-NO-PAID.
           MOVE -1             TO NDTE-HOLD.
           PERFORM UNTIL ( (NUM-MO NOT = NDTE-MM)
                                OR (NDTE-DATE = AGEING-WK-DATE)
                         )
               IF NDTE-DATE NOT = AGEING-WK-DATE
                  ADD 1 TO AGEING-NO-PAID
               END-IF
               PERFORM INCREMENT-UNITPER
           END-PERFORM.

      * GET NO. OF PAY PERIODS IN CURRENT PAID THRU MONTH
      * THAT HAVEN BEEN PAID:
      
           MOVE PDTH-DATE-FULL TO NUM-DATE
                                  NDTE-DATE.
           MOVE +1             TO NDTE-HOLD.
           MOVE -1             TO AGEING-NO-TOBE-PAID.
           PERFORM UNTIL (NUM-MO NOT = NDTE-MM)
               PERFORM INCREMENT-UNITPER
               ADD 1 TO AGEING-NO-TOBE-PAID
           END-PERFORM.

      * TEST IF PAID THRU MONTH HAS NO PAYMENT REQUIREMENTS
      
           COMPUTE AGEING-WORK = AGEING-NO-PAID + AGEING-NO-TOBE-PAID.
           IF AGEING-WORK = 0
              MOVE PDTH-DATE-FULL TO NDTE-DATE
              MOVE 1 TO NDTE-HOLD
              PERFORM INCREMENT-MONTHS
              MOVE NDTE-DATE TO PDTH-DATE-FULL
           ELSE
      * TEST AGAINST AGEING-PERCENT TO SEE IF MONTH SHOULD BE BUMPED
              IF ( (AGEING-NO-PAID + AGEING-HOLD-REMAIN)
                                              / AGEING-WORK )
                               NOT < AGEING-PERCENT-FACTOR
                 MOVE PDTH-DATE-FULL TO NDTE-DATE
                 MOVE 1 TO NDTE-HOLD
                 PERFORM INCREMENT-MONTHS
                 MOVE NDTE-DATE TO PDTH-DATE-FULL.

      * DETERMINE CONTRACTUAL AGE
      
           MOVE PDTH-DATE-FULL TO NUM-DATE.
           MOVE AGEING-DATE    TO SYS-DATE.
           MOVE 1              TO NUM-DA S-DD.
           PERFORM TIM360.
           ADD 1 ELAPSED-MONTHS GIVING CSUB.

       COMPUTE-CONTR-UNITP-A-D-EXIT.
           EXIT.

      ********************************************************************
      *   AGEING - COMPUTE-CONTRACTUAL WORLD FORMULA D
      *
      *          CPOT30         'Y' OR 'M' OR ' '
      *
      *                          Y  - POTENTIAL (1 TO 29 DAYS PAST DUE
      *                                            AND RECENCY POTENTIAL)
      *                          M  - MISS PAY (1 TO 29 DAYS PAST DUE
      *                                              AND RECENCY CURRENT)
      *                          SP - CURRENT OR CONTRACTUALLY DELINQUENT
      * NOTE:
      *    MISPAYS ARE:
      *       ACCOUNTS THAT ARE 1-29 DAYS PAST DUE AND RECENCY CURRENT,
      *                         BUT NOT RECENCY <P> OR MORE.
      *    -  AFTER A MONTHEND:
      *     [ RECENCY CURRENT # & $ ]  =  [ CONTRACTUAL CURRENT # & $ ]
      ********************************************************************
       COMPUTE-CONTRACTUAL-D SECTION.

           MOVE "X" TO CEPP-NOCAP-FG.

           MOVE AGEING-DATE TO NDTE-DATE.
           MOVE 35          TO NDTE-DD.
           MOVE -1          TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE   TO CEPP2-DATE.

           PERFORM CONTRACTUAL-ELAPSED-PAYPER.
           COMPUTE CSUB = CEPP-PERIODS - AGEING-HOLD.
           IF CSUB = 1
              MOVE "Y" TO CPOT30.

           IF CSUB < 1
              MOVE 1 TO CSUB.

           PERFORM RECENCY-ROUTINE.

      * TEST FOR MISPAYS:

           IF CSUB NOT = 1 OR CPOT30 = "Y"
              GO TO COMPUTE-CONTRACTUAL-D-EXIT.

           IF RDSUB NOT = 1 OR RDPOT30 = "Y"
              GO TO COMPUTE-CONTRACTUAL-D-EXIT.

           MOVE AGEING-DATE  TO PDUE-TODAY-DATE.
           MOVE LN-1STPYDATE TO PDUE-1STPYDATE.
           PERFORM PAST-DUE-DAYS-CALCULATION.
           IF PDUE-DAYS > 0
              MOVE "M" TO CPOT30.

       COMPUTE-CONTRACTUAL-D-EXIT.
           PERFORM COMPUTE-RECENCY-SET-EXTERNAL.
           MOVE " " TO CSUB-ALDEL-FG.

      ********************************************************************
      *   AGEING - COMPUTE-CONTRACTUAL WORLD FORMULA F (MX NON MONTHLY)
      *
      *          CPOT30         'Y' OR 'M' OR ' '
      *
      *                          Y  - POTENTIAL (1 TO 29 DAYS PAST DUE
      *                                            AND RECENCY POTENTIAL)
      *                          M  - MISS PAY (1 TO 29 DAYS PAST DUE
      *                                              AND RECENCY CURRENT)
      *                          SP - CURRENT OR CONTRACTUALLY DELINQUENT
      * NOTE:
      *    MISPAYS ARE:
      *       ACCOUNTS THAT ARE 1-29 DAYS PAST DUE AND RECENCY CURRENT,
      *                         BUT NOT RECENCY <P> OR MORE.
      *    -  AFTER A MONTHEND:
      *     [ RECENCY CURRENT # & $ ]  =  [ CONTRACTUAL CURRENT # & $ ]
      ********************************************************************
       COMPUTE-CONTRACTUAL-F SECTION.

           MOVE "X" TO CEPP-NOCAP-FG.

           MOVE AGEING-DATE TO NDTE-DATE.
           MOVE 35          TO NDTE-DD.
           MOVE -1          TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE   TO CEPP2-DATE.

           PERFORM CONTRACTUAL-ELAPSED-PAYPER.
           COMPUTE CSUB = CEPP-PERIODS - AGEING-HOLD.
           IF CSUB = 1 OR 2
              MOVE "Y" TO CPOT30.

           IF CSUB < 1
              MOVE 1 TO CSUB.

           PERFORM RECENCY-ROUTINE.

      * TEST FOR MISPAYS:

           IF CSUB NOT = 1
                    OR CPOT30 = "Y"
              GO TO COMPUTE-CONTRACTUAL-F-EXIT.

           IF RDSUB NOT = 1
                     OR RDPOT30 = "Y"
              GO TO COMPUTE-CONTRACTUAL-F-EXIT.

           MOVE AGEING-DATE  TO PDUE-TODAY-DATE.
           MOVE LN-1STPYDATE TO PDUE-1STPYDATE.
           PERFORM PAST-DUE-DAYS-CALCULATION.
           IF PDUE-DAYS > 0
              MOVE "M" TO CPOT30.

       COMPUTE-CONTRACTUAL-F-EXIT.
           PERFORM COMPUTE-RECENCY-SET-EXTERNAL.
           MOVE " " TO CSUB-ALDEL-FG.

      *************************************************************************
      *   NAME:  AGEING - COMPUTE-PAID-AHEAD
      *   DESC:  COMPUTES PAID AHEAD WHICH IDENTIFIES THOSE ACCOUNTS
      *          THAT ARE CONTRACTUALLY CURRENT FOR EACH RECENCY BUCKET
      *
      *   IN  :  RSUB, CSUB
      *   OUT :  PSUB
      *
      *       N O T E:
      *                PSUB MUST BE > 0 TO BE VALID
      *-----------------------------------------------
      *
      *   PSUB      INTERNAL & EXTERNAL
      *   -----     -------------------
      *     1   -    30
      *     2   -    60
      *     3   -    90
      *     4   -   120
      *     5   -   150
      *     6   -   180
      *     7   -   210+
      *     8   -   N/A
      *     9   -   POTENTIAL 30
      *    10   -   CURRENT
      *-----------------------------------------------
      * REV:
      *  JTG 010787 TOOK OUT CHECK ON RSUB > 1 TO ALLOW PAID AHEADS
      *             TO APPEAR FOR RECENCY CURRENT ACCOUNTS
      *  JTG 091597 ADDED LOGIC TO TEST FOR MISSPAY & POTS WHICH ARE CURRENT
      *  BLV 091897 USE RDSUB, NOT RSUB
      *************************************************************************
       COMPUTE-PAID-AHEAD SECTION.
           MOVE 0 TO PSUB.
           IF CSUB = 8 OR 9 OR 10
              MOVE RDSUB TO PSUB.

