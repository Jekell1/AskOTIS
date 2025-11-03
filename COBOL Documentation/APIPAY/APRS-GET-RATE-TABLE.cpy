      *******************************************
      *    DETERMINE RATE TABLE TO USE
      *******************************************
       APRS-GET-RATE-TABLE SECTION.
           PERFORM LOAN-CALCULATIONS.
           MOVE INT-CHARGEABLE TO CTBL-INT-CHARGEABLE.
           MOVE LN-ORGTERM     TO CTBL-TERM.
           PERFORM SETCTBL.
           MOVE CTBL-INDEX TO CTBL
                              APRS-CTBL.

      *******************************************
      *    COMPUTE THE SIMPLE INTEREST RATE
      *******************************************
