      ****************************************************************
      *                     (0 E)
      *    WORLD
      *    ALABAMA ACTUARIAL REFUND FORMULA
      *    SINGLE PREMIUM RATE MEMORANDUM
      *
      ****************************************************************
       REBATE-ACTUARIAL-0E SECTION.

           MOVE LN-ORGTERM   TO REBN.
           MOVE REB-ORGTERM  TO REBM.
           MOVE REB-REMTERM  TO REBR.
           SUBTRACT REBM FROM REBN GIVING REBN-M.

      * COMPUTE MONTHLY RATE OF INTEREST:
           COMPUTE REBI ROUNDED = REB-LN-APRATE / 1200.

      * COMPUTE ANNUITY FACTORS:
           COMPUTE REBAN ROUNDED =
               (1 - 1 / (1 + REBI) ** REBN) / REBI.

           COMPUTE REBAN-M ROUNDED =
               (1 - 1 / (1 + REBI) ** (REBN - REBM)) / REBI.

           COMPUTE REBAN-MPLUSR ROUNDED =
               (1 - 1 / (1 + REBI) ** (REBN - REBM + REBR)) / REBI.

      * COMPUTE REBATE:
           COMPUTE REB-REBATE ROUNDED =
               REB-TOTCHG *
                 ( ((REBR - REBAN-MPLUSR + REBAN-M) / REBI) + REBR )
                 /
                 ( ((REBM - REBAN + REBAN-M) / REBI ) + REBM ).

       REBATE-ACTUARIAL-0E-EXIT.
           EXIT.

      ****************************************************************
      *                     (0 F)
      *    WORLD
      *    COLORADO TRUE DAILY PRORATA REFUND FORMULA
      *
      ****************************************************************
