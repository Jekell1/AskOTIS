      ********************************
      *    VALIDATE SELECTED TABLE
      ********************************
       VALIDATE-SPR-TABLE.
           IF NOT (SP-CAL-SIMPLE(CTBL))
              GO TO INTEREST-DUE-EXIT.
           IF NOT (SP-CAL-TBFRMLA-VALID(CTBL))
              GO TO INTEREST-DUE-EXIT.
           IF SP-CAL-BREAK-ON-TERM(CTBL)
              GO TO INTEREST-DUE-EXIT.

      ********************************
      *    INTEREST DUE LOOPS
      *    USING SP-CAL TABLES
      ********************************
           SET CSTP TO 0.
