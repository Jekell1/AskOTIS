       LCAP-P-CURRENT-PERIOD-EXIT.
      * SET LAST CURRENT DATE BACK ONE MONTH TO ALLOW
      * COMPARISON WITH LCPDTH:
           IF LCAP-LST-CURDATE NOT = 0
              MOVE LCAP-LST-CURDATE TO NDTE-DATE
              MOVE -1 TO NDTE-HOLD
              PERFORM INCREMENT-MONTHS
              MOVE NDTE-DATE TO LCAP-LST-CURDATE.

           SUBTRACT LCAP-LPAPLC LCAP-LPAPINT FROM LCAP-TRAMT.

      ***************************************************************
      *   SP-LCFRML = S    WISCONSIN LATE CHARGE FORMULA
      *                    WORKS LIKE "F" BUT WILL NOT TAKE
      *                    A LC AFTER MATURITY.
      *               T   WISCONSIN LATE CHARGE FORMULA
      *                   SAME AS 'S' BUT WITH TOTAL MONEY
      *
      *   NOTE:
      *        LCAP-PARTIALS (HOLDS LCAP-CURDUE AT EXIT)
      *        LCAP-LCPAID   (HOLDS LCAP-CURDATE [MMDD.YY] AT EXIT)
      ***************************************************************
