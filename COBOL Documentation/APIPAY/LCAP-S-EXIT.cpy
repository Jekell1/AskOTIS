       LCAP-S-EXIT.
           MOVE LCAP-CURDUE TO LCAP-PARTIALS.
      * REARRANGE DATE TO (MMDD.YY):
           MOVE LCAP-CURDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MULTIPLY DATE-MMDDYY BY 0.01 GIVING LCAP-LCPAID.

      ***********************************************
      *    TEST CURRENT PAYMENT ON TIME
      *
      *    IF SO, ESTABLISH WHETHER ITS A NEW PERIOD
      *    OR LAST PAYMENT PERIOD AND SETUP AMOUNTS
      *    DELIQUENT FOR LAST OR BOTH PERIODS.
      ***********************************************
