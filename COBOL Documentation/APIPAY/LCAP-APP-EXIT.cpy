       LCAP-APP-EXIT.
           IF LCAP-LPTRCD = "AL"
              ADD LCAP-APP TO LCAP-OWE
              MOVE 0 TO LCAP-APP.

      * ASSURE LATE CHARGE PAID THRU HAS VALID DAY:

           PERFORM LCAP-ASSURE-LCPDTH.

      ************************************************
      *   SP-LCFRMLA = A OR C
      ************************************************
