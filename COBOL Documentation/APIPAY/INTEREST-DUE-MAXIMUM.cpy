      **************************************************
       INTEREST-DUE-MAXIMUM SECTION.
      **************************************************
      * INSTANT AUTO #1047:
           IF SP-CAL-MAX-AT-ESTIMATE = "Y"
              IF (INDU-INTEREST + LN-TOTINT) > LN-INTCHG
                 COMPUTE INDU-INTEREST = LN-INTCHG - LN-TOTINT.

      *********************************
      *    SET THE PROPER RATE TABLE
      *    CTBL = 1 OR 2
      *********************************
