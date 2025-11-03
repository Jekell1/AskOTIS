       LATE-CHARGE-CALC-EXIT.
           EXIT.

      ****************************************************************
      *  NOTE:
      *        IF ELREAD.C DETECTS AN INVALID CODE
      *        DUE TO THE OPTION NOT BEENING PURCHASED
      *        AND FX/ELCLR.C NOT HAVING BEEN RUN,
      *
      *        THE SPR FIELDS:
      *          SP-LC-INFO & SP-LCUNITPER-CD  ARE NOT CHANGED
      *        AND
      *          SP-LC-CODE WILL BE SET TO 0 CAUSING NO LATE CHARGES
      *
      ****************************************************************
