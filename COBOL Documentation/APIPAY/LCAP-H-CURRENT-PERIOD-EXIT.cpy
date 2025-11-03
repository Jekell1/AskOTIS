       LCAP-H-CURRENT-PERIOD-EXIT.
      * SET LAST CURRENT DATE BACK ONE MONTH TO ALLOW
      * COMPARISON WITH LCPDTH:
           IF LCAP-LST-CURDATE NOT = 0
              MOVE LCAP-LST-CURDATE TO NDTE-DATE
              MOVE -1 TO NDTE-HOLD
              PERFORM INCREMENT-MONTHS
              MOVE NDTE-DATE TO LCAP-LST-CURDATE.
      * RESET LCAP-TRAMT ADDED AT TOP OF LCAP-H-CURRENT
           SUBTRACT LCAP-LPAPLC LCAP-LPAPINT FROM LCAP-TRAMT.


      **************************************************************
      *   SP-LCFRML = I & L   INDIANA   LATE CHARGE FORMULA (PRIN ONLY)
      *
      *   NOTE:
      *        LCAP-PARTIALS (HOLDS LCAP-CURDUE AT EXIT)
      *        LCAP-LCPAID   (HOLDS LCAP-CURDATE [MMDD.YY] AT EXIT)
      **************************************************************
