      *****************************************************
      *                     (0 7)
      *    MULLEN 'CL' REFUND PROTECTIVE LIFE
      *           TRUNCATED NET LIFE COVERAGE
      *****************************************************
       REBATE-MULLEN-CL-07 SECTION.

      *   GET CREDIT LIFE MOB RATE:
           IF LN-INSTYPES-DL
              IF LN-INS-SINGLE
                 MOVE SP-INSRATE(1 1) TO REB-MANYDECIMALS
              ELSE
              IF LN-INS-JOINT
                 MOVE SP-INSRATE(2 1) TO REB-MANYDECIMALS
              ELSE
                 MOVE REB-TOTCHG TO REB-REBATE
                 GO TO REBATE-MULLEN-CL-07-EXIT.
           IF LN-INSTYPES-LL
              IF LN-INS-SINGLE
                 MOVE SP-INSRATE(3 1) TO REB-MANYDECIMALS
              ELSE
              IF LN-INS-JOINT
                 MOVE SP-INSRATE(4 1) TO REB-MANYDECIMALS
              ELSE
                 MOVE REB-TOTCHG TO REB-REBATE
                 GO TO REBATE-MULLEN-CL-07-EXIT.

           COMPUTE REBI ROUNDED = REB-LN-APRATE / 1200.
           COMPUTE REB-A--P-I ROUNDED =
              (1 - (1 / (1 + REBI) **
                 (REB-LN-ORGTERM - (REB-ORGTERM - REB-REMTERM)))
              ) / REBI.
           COMPUTE REB-A--P-J ROUNDED =
              (1 - (1 / (1 + 0.0035) ** REB-REMTERM)) / 0.0035.
           COMPUTE REB-WK ROUNDED =
              (1 - (1 / (1 + REBI) ** (REB-LN-ORGTERM - REB-ORGTERM))
              ) / REBI.
           COMPUTE REB-WK ROUNDED =
              REB-A--P-J - REB-A--P-I + REB-WK
                - 0.0035 * REB-A--P-J * REB-WK.
           COMPUTE REB-WK ROUNDED =
              REB-MANYDECIMALS * .001
                 * (1.0035 * REB-WK) / (REBI - 0.0035).

           COMPUTE REB-REBATE ROUNDED = REB-LN-REGPYAMT * REB-WK.

       REBATE-MULLEN-CL-07-EXIT.
           EXIT.

      ***********************************************************
      *                     (0 7)
      *    MULLEN 'AH' REFUND PROTECTIVE LIFE
      *           NON RETRO 14 DAY AH COVERAGE
      *
      *    REFUND = PREMIUM * REMAIN-TERM / ORIG-TERM
      *                * REMAIN-TERM-RATE / ORIG-TERM-RATE - 10
      ***********************************************************
