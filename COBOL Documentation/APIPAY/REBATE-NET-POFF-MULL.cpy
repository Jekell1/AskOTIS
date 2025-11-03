      *******************************************************************
      *                     FORMULA (0, 2)
      *    MULLEN'S CALIFORNIA CL NET PAYOFF   BALBOA LIFE & CASUALTY
      *******************************************************************
       REBATE-NET-POFF-MULL  SECTION.
           COMPUTE REBJ ROUNDED = 4.2 / 1200.
      * SET LOAN TERM
           SUBTRACT REB-ELAPSED-MONTHS FROM REB-LN-ORGTERM
                                                 GIVING REBN.
           IF REBN NOT > 1
              GO TO REBATE-NET-POFF-MULL-EXIT.

      * SET INSURANCE REMAINING TERM
           MOVE REB-REMTERM TO REBP.

           COMPUTE REB-A--P-J ROUNDED =
                  (1 - 1 / (1 + REBJ) ** REBP) / REBJ.

           COMPUTE REBI ROUNDED = LN-SMPRATE / 1200.
           COMPUTE REB-A--P-I ROUNDED =
               (1 - 1 / (1 + REBI) ** REBP) / REBI.

           COMPUTE REB-WK ROUNDED =
              ( 1 / REBI * (1 + REBJ)
                     * (REB-A--P-J
                           + (REBJ * REB-A--P-J - REBI * REB-A--P-I)
                              / (1 + REBI) ** (REBN - REBP)
                              / (REBI - REBJ)
                       )
              ).

           COMPUTE REB-REBATE ROUNDED =
                 REB-LN-REGPYAMT * SP-INSRATE(REB-SUB 1)
                                           * .001 * REB-WK.
       REBATE-NET-POFF-MULL-EXIT.
           EXIT.

      *******************************************************************
      *                     FORMULA (0, 3)
      *    MEAN RULE OF 78THS REFUND (MEAN)                  REGACC #
      *******************************************************************
