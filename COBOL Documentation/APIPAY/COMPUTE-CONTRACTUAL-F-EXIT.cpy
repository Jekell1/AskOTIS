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
