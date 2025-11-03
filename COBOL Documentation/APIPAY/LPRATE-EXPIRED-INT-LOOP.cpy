       LPRATE-EXPIRED-INT-LOOP.
           SET EXPR UP BY 1.
           COMPUTE INDU-CURBAL = LN-CURBAL - LN-OT2BAL.
           MOVE SP-EXPR-YRTYPE(EXPR) TO INDU-YEARTYPE.
           MOVE SP-EXPR-INTMETH(EXPR) TO INDU-INTMETHOD.
           IF SP-EXPR-RATE(EXPR) = 999.999
              MOVE LN-OVRDRATE TO INDU-RATE
           ELSE
           IF ( SP-EXPR-FRMLA = "C"              ) AND
              ( SP-EXPR-RATE(EXPR) NOT > 999.995 ) AND
              ( LN-SMPRATE < SP-EXPR-RATE(EXPR)  )
                 MOVE LN-SMPRATE   TO INDU-RATE
                 MOVE SP-INTMETHOD TO INDU-INTMETHOD
                 MOVE SP-YEARTYPE  TO INDU-YEARTYPE
            ELSE
      *JTG HELP FOR JUDY, ADVANCE MORTGAGE 012297:
              MOVE SP-EXPR-RATE(EXPR) TO INDU-RATE
              IF INDU-RATE NOT > 999.995
                 IF SP-EXPR-FRMLA-ORIG
      *                  --ACCOUNT IS/HAS EXPIRED AND IS NOT
      *                    ACCELERATED/JUDGEMENT/REDUCED
                         AND IBPC-STAT = "E"
                          AND IBPC-STAT-2 = " "
                    IF LN-OVRDRATE < INDU-RATE
                       MOVE LN-OVRDRATE TO INDU-RATE
                       MOVE SP-INTMETHOD TO INDU-INTMETHOD
                       MOVE SP-YEARTYPE TO INDU-YEARTYPE.
      *JTG HELP FOR JUDY, ADVANCE MORTGAGE 012297:

           PERFORM LPRATE-FIND-THRU-DATE.
           PERFORM INTEREST-DUE-CALCULATION.
           ADD INDU-INTEREST TO RATE-EXP-INT.
           MOVE INDU-DATE-2 TO INDU-DATE-1.
           IF EXPR < 5
                AND (SP-EXPR-BREAK(EXPR) NOT = 0
                         AND RATE-EXPR-TAB-BREAK(EXPR) NOT = 0
                    )
              GO TO LPRATE-EXPIRED-INT-LOOP.
           MOVE RATE-EXP-INT TO INDU-INTEREST.

      ************************************************
      *    CALC BREAK DATES ROUTINE
      *
      * THIS ROUTINE CONVERTS ALL SP-EXPR-BREAK'S
      *  INTO DATES, BASED ON MONTHS AND DAYS FROM
      *  EXPIRATION.
      ************************************************
