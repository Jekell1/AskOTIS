      ***************************************************
      *    COMPUTE PRESENT VALUE OF REMAINING PAYMENTS
      *         AT THE TIME OF REBATE
      ***************************************************
       RBACT-ACTUARIAL-PV.
           COMPUTE RBACTPV ROUNDED =
              RBACTADJ * (RBACTFP + RBACTRP * RBACTRP-A--N
                               + RBACTLP * RBACTLP-FAC
                       ).
           MOVE RBACTPV TO RBACT-PRES-VALUE.

      ***************************************************
      *    COMPUTE REBATE AMOUNT
      ***************************************************
           COMPUTE RBACT-REBATE ROUNDED =
              (RBACTFP + RBACTRP * RBACTRP-N + RBACTLP)
                                       - RBACTPV.

