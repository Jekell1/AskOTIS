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
