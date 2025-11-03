      *******************************************************************
      *                     FORMULA (0, 3)
      *    MEAN RULE OF 78THS REFUND (MEAN)                  REGACC #
      *******************************************************************
       RULE-78THS-MEAN  SECTION.

           IF REB-ORGTERM = REB-REMTERM
              COMPUTE REB-REBATE = REB-TOTCHG - REB-DEDCHG
           ELSE
              COMPUTE REB-SUMREM =
                   REB-REMTERM * (REB-REMTERM + REB-LN-ORGTERM + 2)
              COMPUTE REB-SUMORG =
                   (REB-LN-ORGTERM + 1) * REB-LN-ORGTERM  * 2
              IF REB-SUMORG = 0
                 MOVE 0 TO REB-REBATE
              ELSE
                 COMPUTE REB-REBATE ROUNDED =
                     (REB-TOTCHG - REB-DEDCHG)
                              * REB-SUMREM / REB-SUMORG.
       EXIT-RULE-78THS-MEAN.
           EXIT.

      *******************************************************************
      *                     FORMULA (0, 4)
      *    SHORT RATE 'TN' PP REFUND METHOD            TN, CONSUMER #1067
      *******************************************************************
