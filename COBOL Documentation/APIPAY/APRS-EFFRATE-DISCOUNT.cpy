      *-------------------------------------------------
      *    COMPUTE EFFECTIVE DISCOUNT RATE
      *-------------------------------------------------
       APRS-EFFRATE-DISCOUNT SECTION.
           IF SP-PYEXFRMLA-REG-INCR
              COMPUTE APRS-EFFRATE ROUNDED =
                  (
                    ((1 - APR-FINANCED / (APR-FINANCED + LN-INTCHG)))
                         * (UPER-UNITPER-PER-YEAR
                               / (LN-ORGTERM + UPER-FULL-UNITPER - 1
                                     + UPER-FRACT-UNITPER
                                 )
                           )
                  ) * 100
           ELSE
              COMPUTE APRS-EFFRATE ROUNDED =
                  (
                    ((1 - APR-FINANCED / (APR-FINANCED + LN-INTCHG)))
                         * (UPER-UNITPER-PER-YEAR / LN-ORGTERM)
                  ) * 100.

      *----------------------------------------------
      *    COMPUTE EFFECTIVE ADDON RATE
      *----------------------------------------------
