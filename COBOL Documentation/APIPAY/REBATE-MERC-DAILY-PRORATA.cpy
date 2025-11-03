      *****************************************************
      *    MERC. DAILY PRORATA         (V) & (W)
      *
      *****************************************************
       REBATE-MERC-DAILY-PRORATA SECTION.

           IF SP-RBFRMLA(REB-SUB) = "W"
              MOVE REB-LN-ORIG-1STPYDATE TO NDTE-DATE
              MOVE -1 TO NDTE-HOLD
              PERFORM INCREMENT-MONTHS
              MOVE NDTE-DATE TO SYS-DATE
           ELSE
              MOVE REB-LN-LOANDATE TO NDTE-DATE.
           MOVE REB-LN-ORGTERM TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE TO NUM-DATE.
           IF SP-RBFRMLA(REB-SUB) = "W"
              MOVE SYS-DATE TO NDTE-DATE
           ELSE
              MOVE REB-LN-LOANDATE TO NDTE-DATE.
           COMPUTE NDTE-HOLD = REB-LN-ORGTERM + REB-LN-TOTNODEF.
           PERFORM INCREMENT-MONTHS.
           MOVE REB-PAYDATE TO SYS-DATE.
           MOVE 360 TO ELAPSED-YRTYPE.
           PERFORM TIMALL.
           IF ELAPSED-DAYS <= 0
              COMPUTE REB-REMTERM = (ELAPSED-DAYS * -1) *
                                     REB-LN-TOTNODEF
              COMPUTE ELAPSED-DAYS = ((REB-LN-TOTNODEF *
                                     (REB-LN-TOTNODEF + 1)) / 2) * 30
              COMPUTE REB-REBATE =
                   (REB-REMTERM + ELAPSED-DAYS)
                         * REB-LN-APRATE * REB-LN-REGPYAMT / 36000
           ELSE
              MOVE NDTE-DATE TO NUM-DATE
              MOVE REB-PAYDATE TO SYS-DATE
              MOVE 360 TO ELAPSED-YRTYPE
              PERFORM TIMALL
              IF ELAPSED-DAYS <= 0
                 COMPUTE ELAPSED-DAYS = (ELAPSED-DAYS * -1)
                 MOVE 0 TO REB-WORKER
                 PERFORM UNTIL ELAPSED-DAYS < 0
                    ADD ELAPSED-DAYS TO REB-WORKER
                    SUBTRACT 30 FROM ELAPSED-DAYS
                 END-PERFORM
      *JTG020397 DIVIDE ELAPSED-DAYS BY 30 GIVING REB-REMTERM
      *CORRECTED COMPUTE ELAPSED-DAYS = ELAPSED-DAYS *
      *SLC ERROR      (REB-REMTERM + 1)
                 COMPUTE REB-REBATE =
                     REB-WORKER * REB-LN-APRATE * REB-LN-REGPYAMT
                                    / 36000.

           IF REB-REBATE > REB-TOTCHG
              MOVE REB-TOTCHG TO REB-REBATE.

       REBATE-MERC-DAILY-PRORATA-EXIT.
           EXIT.


      ************************************************
      *               (RBFRMLA2 = 1)
      *    PRO RATA, TO THE DAY REBATE METHOD
      *    MECURY ACTION: FORMULA 9
      ************************************************
