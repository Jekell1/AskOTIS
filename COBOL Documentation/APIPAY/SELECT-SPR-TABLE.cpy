      *********************************
      *    SET THE PROPER RATE TABLE
      *    CTBL = 1 OR 2
      *********************************
       SELECT-SPR-TABLE SECTION.
           PERFORM LOAN-CALCULATIONS.
           MOVE INT-CHARGEABLE TO CTBL-INT-CHARGEABLE.
           MOVE LN-ORGTERM     TO CTBL-TERM.
           PERFORM SETCTBL.
           MOVE CTBL-INDEX TO CTBL.

      ******************************************************
      *    GET ALL INSURANCE PREMIUMS
      ******************************************************
