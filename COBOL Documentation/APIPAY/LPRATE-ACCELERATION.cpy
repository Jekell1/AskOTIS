      ************************************************
      *    ACCELERATION ROUTINE
      ************************************************
       LPRATE-ACCELERATION SECTION.
           IF LN-ACCRATE = 999.998
              MOVE 999.998      TO INDU-RATE
              MOVE SP-INTMETHOD TO INDU-INTMETHOD
              MOVE SP-YEARTYPE  TO INDU-YEARTYPE
           ELSE
           IF LN-ACCRATE = 999.996
              MOVE 999.996      TO INDU-RATE
              MOVE SP-INTMETHOD TO INDU-INTMETHOD
              MOVE SP-YEARTYPE  TO INDU-YEARTYPE
           ELSE
      * ALLOW WORLD TO ENTER 999.998 IN SP-ACC-RATE(1)
      * AND IF DETECTED HERE SKIP USAGE OF ACCELERATION RATE
      * TABLE AND FORCE LN-SMPRATE RE: LN-ACCRATE 999.998 WORLD #306
              IF ( LN-ACCRATE     = 999.997 ) AND
                 ( SP-ACC-RATE(1) = 999.998 )
                 MOVE 999.998      TO INDU-RATE
                 MOVE SP-INTMETHOD TO INDU-INTMETHOD
                 MOVE SP-YEARTYPE  TO INDU-YEARTYPE
              ELSE
      * DEFAULT OF EITHER 999.997 OR (0 TO 999.995)
                 MOVE SP-ACC-INTMETH TO INDU-INTMETHOD
                 MOVE SP-ACC-YRTYPE  TO INDU-YEARTYPE
                 MOVE LN-ACCRATE     TO INDU-RATE.

      ************************************************
      *    SET INTEREST PDTH (ASSESS FROM DATE)
      ************************************************
