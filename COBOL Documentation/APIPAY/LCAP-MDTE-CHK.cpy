      ****************************************************
      * COMPARE LCAP-LCPDTH-DATE (UPDATED) WITH MATURITY DATE:
      ****************************************************
       LCAP-MDTE-CHK SECTION.
           MOVE MDTE-DATE TO SYS-DATE.
           PERFORM LCAP-LCPDTH-DATE-TEST.

      ****************************************************
      * FOR THIS CODE, STOP ASSESSING WHEN LATE CHARGES
      * ARE BROUGHT UP TO DATE (DETERMINED BY LCAP-PAYDATE):
      * IF NOT AL, ELAPSED SET TO 1 FOR UNIFORM EXIT CHECK
      ****************************************************
