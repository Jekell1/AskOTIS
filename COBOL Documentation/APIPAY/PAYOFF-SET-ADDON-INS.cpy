      ****************************************
      *    SETUP FOR INSURANCE ADDON REFUND
      ****************************************
       PAYOFF-SET-ADDON-INS SECTION.
           MOVE LN-INSEFF-DATE(POFF-SUB) TO REB-LN-LOANDATE
                                       REB-LN-PURDATE
                                       REB-LN-INTDATE
                                       REB-LN-INSEFF
                                       NDTE-DATE.
           MOVE LN-INSEXP-DATE(POFF-SUB) TO REB-LN-INSEXP.

           MOVE 1           TO NDTE-HOLD.
           MOVE LN-UNITPER-CD TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM INCREMENT-UNITPER.
           MOVE NDTE-DATE            TO REB-LN-ORIG-1STPYDATE.
           MOVE REB-ORGTERM          TO REB-LN-ORGTERM.
           MOVE LN-REGPYAMT          TO REB-LN-REGPYAMT.
           MOVE LN-LASTPYAMT         TO REB-LN-LASTPYAMT.
           MOVE LN-APRATE            TO REB-LN-APRATE.
           MOVE LN-SMPRATE           TO REB-LN-SMPRATE.
           MOVE 0                    TO REB-LN-TOTNODEF.
           MOVE LN-INSCOVR(POFF-SUB) TO REB-LN-INSCOVR.
           MOVE "N"                  TO REB-LN-SALEFIN.
           MOVE "Y"                  TO REB-ADDON-FG.

      ****************************************
      *    SET REB-SUB FOR CREDIT LIFE REBATE
      ****************************************
