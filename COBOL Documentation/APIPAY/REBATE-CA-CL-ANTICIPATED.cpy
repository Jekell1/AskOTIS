      *****************************************************
      *    CALIFORNIA 'CL' ANTICIPATED     (S)
      *    COSMO
      *
      *****************************************************
       REBATE-CA-CL-ANTICIPATED SECTION.
           MOVE LN-ORGTERM TO REBN.
           MOVE REB-ORGTERM TO REBM.
           SUBTRACT 1 FROM REBM GIVING REBM-1.
           SUBTRACT REB-REMTERM FROM REB-ORGTERM GIVING REBT.
           SUBTRACT REBT FROM REBM GIVING REBM-T.

      ***************************************************
      *    COMPUTE INT. RATE PER PMT/COMPOUND INTERVAL:
      ***************************************************
           COMPUTE REBI ROUNDED = REB-LN-APRATE / 1200.
           ADD 1 REBI GIVING REBI1.
      ***************************************************
      *    COMPUTE CA DISCOUNT RATE PER PMT/COMPOUND INTERVAL:
      ***************************************************
           COMPUTE REBJ ROUNDED = 4.2 / 1200.
           ADD 1 REBJ GIVING REBJ1.

      ***************************************************
      *    COMPUTE ANNUITY FACTOR  (M - 1)
      ***************************************************
           COMPUTE REBAN-M-1 ROUNDED =
              (1 - 1 / REBJ1 ** REBM-1) / REBJ.

      ***************************************************
      *    COMPUTE ANNUITY FACTOR  (M - 1 -T)
      ***************************************************
           COMPUTE REBAN-M-1-T ROUNDED =
              (1 - 1 / REBJ1 ** (REBM-T - 1)) / REBJ.

      * COMPUTE REBATE AMOUNT:
           COMPUTE REBWK1 ROUNDED =
               REBJ1 * (REBI1 ** REBM-T - REBJ1 ** REBM-T)
                   / (
                       REBI1 ** (REBN - REBT) * REBJ1 ** REBM-T
                                                  * (REBI - REBJ)
                     ).
           COMPUTE REBWK2 ROUNDED =
               REBJ1 * ( REBI1 ** REBM - REBJ1 ** REBM)
                   / (
                       REBI1 ** REBN * REBJ1 ** REBM
                                         * (REBI - REBJ)
                     ).
           COMPUTE REB-REBATE ROUNDED =
               REB-TOTCHG * ( 1 + REBAN-M-1-T - REBWK1)
                                     / ( 1 + REBAN-M-1 - REBWK2).

       REBATE-CA-CL-ANTICIPATED-EXIT.
           EXIT.

      ******************************************
      *    REBATE TABLE FORMULA (T)
      *              ADDONS
      ******************************************
