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
