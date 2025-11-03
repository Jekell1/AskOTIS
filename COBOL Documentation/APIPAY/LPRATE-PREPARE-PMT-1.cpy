      ************************************************
      *    PREPARE-PMT-1 ROUTINE
      ************************************************
       LPRATE-PREPARE-PMT-1 SECTION.
           MOVE XDTE-DATE TO INDU-DATE-2.
           COMPUTE INDU-CURBAL = LN-CURBAL - LN-OT2BAL.
           MOVE RATE-LPTRCD TO INDU-LPTRCD.
           IF LN-STATUSFG = " " OR LN-STATUSFG = "R" AND
              INDU-RATE NOT = 999.997
              MOVE SP-INTMETHOD TO INDU-INTMETHOD
              MOVE SP-YEARTYPE  TO INDU-YEARTYPE
           ELSE
           IF LN-STATUSFG = " " AND INDU-RATE = 999.997
              MOVE SP-ACC-INTMETH TO INDU-INTMETHOD
              MOVE SP-ACC-YRTYPE  TO INDU-YEARTYPE.

      ************************************************
      *    PREPARE-PMT-2 ROUTINE
      ************************************************
