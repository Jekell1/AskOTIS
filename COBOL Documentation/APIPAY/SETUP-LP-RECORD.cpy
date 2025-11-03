      **************************************************
       SETUP-LP-RECORD SECTION.
      **************************************************
           PERFORM CLEAR-LP-FILE.

           MOVE LN-STATUSFG    TO LP-STATUSFG.
           MOVE LN-ACCTNO      TO LP-ACCTNO.
           MOVE TRANS-DATE     TO LP-TRDATE.
           MOVE BP-PAYDATE     TO LP-PAYDATE.
           MOVE LN-SSNO(1)     TO LP-SSNO.
           MOVE LN-LCPDTH-DATE TO LP-LCPDTH-DATE.
      * BECAUSE CASH DRAWER IS NEVER REALY UPDATED DURING THE UPDATE
      * OF A "PA",ETC TRANSACTION, WE DECIDED TO DEFAULT THE DRAWER
      * NUMBER TO ZERO RATHER THAN MAKE THE USER ENTER A UNUSED #.
           MOVE 0              TO LP-TILLNO.
           MOVE BATCH-REFCD    TO LP-REFNO.

           IF REPAY-TRANS-ID NUMERIC
              MOVE REPAY-TRANS-ID TO LP-REPAY-TRANS-ID
           ELSE
              MOVE 0              TO LP-REPAY-TRANS-ID.

           PERFORM CLEAR-LXG-FILE.

           MOVE LP-BRNO   TO LXG-BRNO.
           MOVE LP-ACCTNO TO LXG-ACCTNO.

      **************************************************
