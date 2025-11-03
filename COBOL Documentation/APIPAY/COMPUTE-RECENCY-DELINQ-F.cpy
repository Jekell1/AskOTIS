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
