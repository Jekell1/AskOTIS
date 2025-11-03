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
