      *********************************
      *   ANTICIPATED 78 REBATE  (4)
      *********************************
       REBATE-ANTICIPATED SECTION.
      * UNIT CHARGE
           IF LN-OVRDRATE = 999.999
              MOVE SP-CAL-RATE(1,1) TO REB-PRATE
           ELSE
              MOVE LN-OVRDRATE TO REB-PRATE.
           COMPUTE REB-SUMORG =
                         (REB-PRATE * REB-LN-REGPYAMT) / 12.
           PERFORM REBATE-78-REBATE.
       REBATE-ANTICIPATED-EXIT.
           EXIT.

      *********************************
      *    PRO RATA REBATE METHOD  (5)
      *********************************
