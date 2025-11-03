      *****************************************************
      *                     (0 7)
      *    MULLEN 'CL' AND 'AH' REFUNDS PROTECTIVE LIFE
      *           TRUNCATED NET LIFE COVERAGE
      *           NON RETRO 14 DAY AH COVERAGE
      *
      *****************************************************
       REBATE-MULLEN-CL-AH-07 SECTION.
           IF REB-SUB < 5
              PERFORM REBATE-MULLEN-CL-07
           ELSE
              PERFORM REBATE-MULLEN-AH-07.

       REBATE-MULLEN-CL-AH-07-EXIT.
           EXIT.

      *****************************************************
      *                     (0 7)
      *    MULLEN 'CL' REFUND PROTECTIVE LIFE
      *           TRUNCATED NET LIFE COVERAGE
      *****************************************************
