      ************************************************
      *    JUDGEMENT ROUTINE
      ************************************************
       LPRATE-JUDGEMENT SECTION.
      * 999.999 - ORIGINAL LOAN CONTRACTED RATE/METHOD:
           IF LN-JDRATE = 999.999
              MOVE LN-OVRDRATE TO INDU-RATE
              MOVE SP-INTMETHOD TO INDU-INTMETHOD
              MOVE SP-YEARTYPE TO INDU-YEARTYPE
           ELSE
           IF LN-JDRATE < 999.999
              MOVE LN-JDRATE TO INDU-RATE
      * 999.998 - EQUIVALENT SIMPLE INTEREST RATE:
      * 999.996 - A.P.R. RATE:
              IF LN-JDRATE = 999.998 OR 999.996
                 MOVE SP-INTMETHOD TO INDU-INTMETHOD
                 MOVE SP-YEARTYPE TO INDU-YEARTYPE
              ELSE
      * 999.997 - ACCELERATION RATES:
              IF LN-JDRATE = 999.997
                 MOVE SP-ACC-INTMETH TO INDU-INTMETHOD
                 MOVE SP-ACC-YRTYPE TO INDU-YEARTYPE
              ELSE
      * > 999.996 - JUST SOME JUDGEMENT RATE:
              IF LN-JDRATE < 999.996
                 MOVE SP-JUDGINTMETH TO INDU-INTMETHOD
                 MOVE SP-JUDGYRTYPE TO INDU-YEARTYPE.

      ************************************************
      *    ACCELERATION ROUTINE
      ************************************************
