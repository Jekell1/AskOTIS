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
