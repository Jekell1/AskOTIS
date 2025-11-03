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
