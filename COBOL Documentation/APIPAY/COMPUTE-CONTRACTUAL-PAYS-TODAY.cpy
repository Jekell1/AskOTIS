      ************************************************
      *    COMPUTE CONTRACURAL PAYS THRU TODAY
      ************************************************
       COMPUTE-CONTRACTUAL-PAYS-TODAY SECTION.
           MOVE LN-1STPYDATE TO CEPP1-DATE.
           MOVE AGEING-DATE  TO CEPP2-DATE.
           MOVE "X"          TO CEPP-NOCAP-FG.
           IF SP-BANKRULE = "A" OR "C"
              MOVE 1         TO CEPP1-DA
                                CEPP2-DA.
           PERFORM CONTRACTUAL-ELAPSED-PAYPER.
           MOVE CEPP-PERIODS TO AGEING-HOLD.

      ************************************************
      *    GET PAIDTHRU
      ************************************************
