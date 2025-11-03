       LCAP-I-L-CURRENT-PERIOD-EXIT.
      * SET LAST CURRENT DATE BACK ONE MONTH TO ALLOW
      * COMPARISON WITH LCPDTH:
           IF LCAP-LST-CURDATE NOT = 0
              MOVE LCAP-LST-CURDATE TO NDTE-DATE
              MOVE -1 TO NDTE-HOLD
              PERFORM INCREMENT-MONTHS
              MOVE NDTE-DATE TO LCAP-LST-CURDATE.

           SUBTRACT LCAP-LPAPLC LCAP-LPAPINT FROM LCAP-TRAMT.

      ***********************************************
      *   SP-LCFRML = G AND K
      *
      *   NOTE: THIS IS THE SAME AS FORMULA "D",
      *         EXCEPT USING PRINCIPLE ONLY AS
      *         CONTRACTUAL MONIES.
      ***********************************************
