      ****************************************************
      * ASSURE LATE CHARGE PAID THRU HAS VALID DAY:
      ****************************************************
       LCAP-ASSURE-LCPDTH SECTION.
           IF LN-UNITPER-CD = "M" AND LN-1STPYDA > 28
              MOVE LCAP-LCPDTH-DATE TO NDTE-DATE
              MOVE LN-1STPYDA  TO NDTE-DD
              PERFORM NEW-DATE-CALCULATION
              MOVE NDTE-DATE   TO LCAP-LCPDTH-DATE.

      ****************************************************
      * COMPARE LCAP-LCPDTH-DATE (UPDATED) WITH MATURITY DATE:
      ****************************************************
