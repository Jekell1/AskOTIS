      * COPYMEMBER: LIBSP/SPWRCL
      ************************************************************
      *  CLEAR RATING SHEET FIELDS
      *  REV:
      *  BLV 031111 UPDATED FOR NEW FIELDS
      *  BLV 040112 UPDATED FOR CHANGED/DELETED FIELDS
      *  GLB 040217 BUG; ADD CLEAR OF WR-WCBC-ELM-AMT-90-DAYS
      *                               WR-WCBC-ELY-AMT-90-DAYS
      *  GLB 040223 ADD NEW FIELDS WR-CASHIER-BONUS,
      *      WR-ASST-MGR-BONUS, WR-MGR-BONUS
      * GLB 040326  ADD NEW FIELDS 1 YR AGO
      * BLV 040414 ADDED CLEAR OF WR-ELM-AMT-90-DAY-ACCTS,
      *                           WR-ELY-AMT-90-DAY-ACCTS
      * BLV 041118 ADDED CLEAR OF NEW LIFE OF THE SOUTH FIELDS
      * BLF 050304 ADDED CLEAR OF WR-OLD-PRETAX-PROFIT
      * BLM 100402 ADDED CLEAR OF WR-OLD-SUPERVISION, WORLD PR# 675
      *  CS 160509 CLEAR OF WR-LTOUCH-DATE
      * BLM 160706 CHANGE WR-OLD-PRETAX-PROFIT TO WR-BSR-BONUS,
      *            WORLD PR# 1163
      *  CS 190606 CLEAR FUTURE FIELDS
      *  BAH 20210427 REMOVED FUTURE FIELDS
      ************************************************************
       CLEAR-WR-FILE SECTION.
           MOVE SPACES TO WR-REC.
           MOVE 0 TO
             WR-NONFILE-ENDING-BALANCE
             WR-CUR-NO-CUST
             WR-ELM-NO-CUST              WR-ELY-NO-CUST
             WR-OUTS-BALANCE
             WR-ELM-OUTS-BALANCE         WR-ELY-OUTS-BALANCE
             WR-NO-NB(1)                 WR-NO-NB(2)
             WR-NO-FB(1)                 WR-NO-FB(2)
             WR-NO-PB(1)                 WR-NO-PB(2)
             WR-GROSS-VOLUME(1)          WR-GROSS-VOLUME(2)
             WR-PRETAX-PROFIT(1)         WR-PRETAX-PROFIT(2)
             WR-INTEREST-ALLOCATION(1)   WR-INTEREST-ALLOCATION(2)
             WR-SUPERVISION(1)           WR-SUPERVISION(2)
             WR-GEN-RESERVE-ADJ(1)       WR-GEN-RESERVE-ADJ(2)
             WR-INTANGIBLE-AMORT(1)      WR-INTANGIBLE-AMORT(2)
             WR-ROBBERIES(1)             WR-ROBBERIES(2)
             WR-RATING-SHEET-INCOME(1)   WR-RATING-SHEET-INCOME(2)
             WR-INT-FEE-INCOME(1)        WR-INT-FEE-INCOME(2)
             WR-INSURANCE-INCOME(1)      WR-INSURANCE-INCOME(2)
             WR-RATING-SHEET-EXPENSES(1) WR-RATING-SHEET-EXPENSES(2)
             WR-PERSONNEL-EXPENSE(1)     WR-PERSONNEL-EXPENSE(2)
             WR-CHARGEOFF-EXPENSE(1)     WR-CHARGEOFF-EXPENSE(2)
             WR-NO-CHARGEOFFS(1)         WR-NO-CHARGEOFFS(2)
             WR-RECOVERY-INCOME(1)       WR-RECOVERY-INCOME(2)
             WR-BAD-DEBT-INCREASE(1)     WR-BAD-DEBT-INCREASE(2)
             WR-NO-30-DAY-ACCTS
             WR-AMT-30-DAY-ACCTS
             WR-NO-60-DAY-ACCTS
             WR-AMT-60-DAY-ACCTS
             WR-NO-90-DAY-ACCTS
             WR-AMT-90-DAY-ACCTS
             WR-ELM-AMT-90-DAY-ACCTS
             WR-ELY-AMT-90-DAY-ACCTS

             WR-YTD-PCT-TURNOVER
             WR-YTD-PCT-RATE-SHT-PROFIT
             WR-YTD-PCT-RS-INC-LEDGER
             WR-YTD-PCT-INT-FEE-LEDGER
             WR-YTD-PCT-INS-INC-LEDGER
             WR-YTD-PCT-EXPENSE-LEDGER
             WR-YTD-PCT-BAD-DEBT-LEDGER
             WR-YTD-PCT-NF-BNK-LEDGER
             WR-YTD-PCT-PRETAX-LEDGER
             WR-YTD-PCT-DEBT-NF-LEDGER

             WR-WCBC-CUR-NO-CUST
             WR-WCBC-ELM-NO-CUST          WR-WCBC-ELY-NO-CUST
             WR-WCBC-OUTS-BALANCE
             WR-WCBC-ELM-OUTS-BAL         WR-WCBC-ELY-OUTS-BAL
             WR-WCBC-NO-NB(1)             WR-WCBC-NO-NB(2)
             WR-WCBC-NO-FB(1)             WR-WCBC-NO-FB(2)
             WR-WCBC-NO-PB(1)             WR-WCBC-NO-PB(2)
             WR-WCBC-GROSS-VOLUME(1)      WR-WCBC-GROSS-VOLUME(2)
             WR-WCBC-LATE-CHGS(1)         WR-WCBC-LATE-CHGS(2)
             WR-WCBC-NET-FIN-CHGS(1)      WR-WCBC-NET-FIN-CHGS(2)
             WR-WCBC-SALES(1)             WR-WCBC-SALES(2)
             WR-WCBC-COST-OF-SALES(1)     WR-WCBC-COST-OF-SALES(2)
             WR-WCBC-EXPENSES(1)          WR-WCBC-EXPENSES(2)
             WR-WCBC-RECOVERY-INCOME(1)   WR-WCBC-RECOVERY-INCOME(2)
             WR-WCBC-CHARGEOFF-EXPENSE(1) WR-WCBC-CHARGEOFF-EXPENSE(2)
             WR-WCBC-BAD-DEBT-BNKRPCY(1)  WR-WCBC-BAD-DEBT-BNKRPCY(2)
             WR-WCBC-NO-30-DAY-ACCTS
             WR-WCBC-AMT-30-DAY-ACCTS
             WR-WCBC-NO-60-DAY-ACCTS
             WR-WCBC-AMT-60-DAY-ACCTS
             WR-WCBC-NO-90-DAY-ACCTS
             WR-WCBC-AMT-90-DAY-ACCTS
             WR-WCBC-ELM-AMT-90-DAYS
             WR-WCBC-ELY-AMT-90-DAYS
             WR-WCBC-PCT-VOLUME
             WR-WCBC-GROSS-PROF-PCT

      *  GLB  11/20/03
             WR-LARGE-CUR-NO-CUST
             WR-LARGE-ELM-NO-CUST
             WR-LARGE-ELY-NO-CUST
             WR-LARGE-OUTS-BALANCE
             WR-LARGE-ELM-OUTS-BAL
             WR-LARGE-ELY-OUTS-BAL
             WR-LARGE-NO-NB(1) WR-LARGE-NO-NB(2)
             WR-LARGE-NO-FB(1) WR-LARGE-NO-FB(2)
             WR-LARGE-NO-PB(1) WR-LARGE-NO-PB(2)
             WR-LARGE-GROSS-VOLUME(1) WR-LARGE-GROSS-VOLUME(2)
             WR-LARGE-NO-30-DAY-ACCTS WR-LARGE-AMT-30-DAY-ACCTS
             WR-LARGE-NO-60-DAY-ACCTS WR-LARGE-AMT-60-DAY-ACCTS
             WR-LARGE-NO-90-DAY-ACCTS WR-LARGE-AMT-90-DAY-ACCTS
             WR-LARGE-ELM-AMT-90-DAYS
             WR-LARGE-ELY-AMT-90-DAYS

             WR-LIFE-CLAIMS(1)            WR-LIFE-CLAIMS(2)
             WR-AH-CLAIMS(1)              WR-AH-CLAIMS(2)
             WR-FIRE-CLAIMS(1)            WR-FIRE-CLAIMS(2)
             WR-AUTO-CLAIMS(1)            WR-AUTO-CLAIMS(2)
             WR-UNEMP-CLAIMS(1)           WR-UNEMP-CLAIMS(2)
             WR-AD-D-CLAIMS(1)            WR-AD-D-CLAIMS(2)
             WR-L-PROT-CLAIMS(1)          WR-L-PROT-CLAIMS(2)
             WR-NF-NON-BK-CLAIMS(1)       WR-NF-NON-BK-CLAIMS(2)
             WR-NF-BK-CLAIMS(1)           WR-NF-BK-CLAIMS(2)
             WR-ROAD-GARD-CLAIMS(1)       WR-ROAD-GARD-CLAIMS(2)

             WR-NO-LIFE-CLAIMS(1)         WR-NO-LIFE-CLAIMS(2)
             WR-NO-AH-CLAIMS(1)           WR-NO-AH-CLAIMS(2)
             WR-NO-FIRE-CLAIMS(1)         WR-NO-FIRE-CLAIMS(2)
             WR-NO-AUTO-CLAIMS(1)         WR-NO-AUTO-CLAIMS(2)
             WR-NO-UNEMP-CLAIMS(1)        WR-NO-UNEMP-CLAIMS(2)
             WR-NO-AD-D-CLAIMS(1)         WR-NO-AD-D-CLAIMS(2)
             WR-NO-L-PROT-CLAIMS(1)       WR-NO-L-PROT-CLAIMS(2)
             WR-NO-NF-NON-BK-CLAIMS(1)    WR-NO-NF-NON-BK-CLAIMS(2)
             WR-NO-NF-BK-CLAIMS(1)        WR-NO-NF-BK-CLAIMS(2)
             WR-NO-ROAD-GARD-CLAIMS(1)    WR-NO-ROAD-GARD-CLAIMS(2)

             WR-LIFE-PREMS(1)             WR-LIFE-PREMS(2)
             WR-AH-PREMS(1)               WR-AH-PREMS(2)
             WR-FIRE-PREMS(1)             WR-FIRE-PREMS(2)
             WR-AUTO-PREMS(1)             WR-AUTO-PREMS(2)
             WR-UNEMP-PREMS(1)            WR-UNEMP-PREMS(2)
             WR-AD-D-PREMS(1)             WR-AD-D-PREMS(2)
             WR-L-PROT-PREMS(1)           WR-L-PROT-PREMS(2)
             WR-NON-FILE-PREMS(1)         WR-NON-FILE-PREMS(2)
             WR-ROAD-GARD-PREMS(1)        WR-ROAD-GARD-PREMS(2)

             WR-NO-LIFE-PREMS(1)          WR-NO-LIFE-PREMS(2)
             WR-NO-AH-PREMS(1)            WR-NO-AH-PREMS(2)
             WR-NO-FIRE-PREMS(1)          WR-NO-FIRE-PREMS(2)
             WR-NO-AUTO-PREMS(1)          WR-NO-AUTO-PREMS(2)
             WR-NO-UNEMP-PREMS(1)         WR-NO-UNEMP-PREMS(2)
             WR-NO-AD-D-PREMS(1)          WR-NO-AD-D-PREMS(2)
             WR-NO-L-PROT-PREMS(1)        WR-NO-L-PROT-PREMS(2)
             WR-NO-NON-FILE-PREMS(1)      WR-NO-NON-FILE-PREMS(2)
             WR-NO-ROAD-GARD-PREMS(1)     WR-NO-ROAD-GARD-PREMS(2)

             WR-LIFE-REFS(1)              WR-LIFE-REFS(2)
             WR-AH-REFS(1)                WR-AH-REFS(2)
             WR-FIRE-REFS(1)              WR-FIRE-REFS(2)
             WR-AUTO-REFS(1)              WR-AUTO-REFS(2)
             WR-UNEMP-REFS(1)             WR-UNEMP-REFS(2)
             WR-AD-D-REFS(1)              WR-AD-D-REFS(2)
             WR-L-PROT-REFS(1)            WR-L-PROT-REFS(2)
             WR-NON-FILE-REFS(1)          WR-NON-FILE-REFS(2)
             WR-ROAD-GARD-REFS(1)         WR-ROAD-GARD-REFS(2)
             WR-NF-RECOVERIES(1)          WR-NF-RECOVERIES(2)

      *  GLB 03/26/04  NEW FIELDS
             WR-NO-CUST-1-YR-AGO
             WR-OUTS-BAL-1-YR-AGO
             WR-WCBC-NO-CUST-1-YR-AGO
             WR-WCBC-OUTS-BAL-1-YR-AGO
             WR-LARGE-NO-CUST-1-YR-AGO
             WR-LARGE-OUTS-BAL-1-YR-AGO
             WR-CUR-NO-CONT-SLOW
             WR-WCBC-CUR-NO-CONT-SLOW
             WR-LARGE-CUR-NO-CONT-SLOW

             WR-CASHIER-BONUS
             WR-ASST-MGR-BONUS
             WR-MGR-BONUS

             WR-LOTS-NF-CLAIMS(1)         WR-LOTS-NF-CLAIMS(2)
             WR-LOTS-NO-NF-CLAIMS(1)      WR-LOTS-NO-NF-CLAIMS(2)
             WR-LOTS-NON-FILE-PREMS(1)    WR-LOTS-NON-FILE-PREMS(2)
             WR-LOTS-NO-NON-FILE-PREMS(1) WR-LOTS-NO-NON-FILE-PREMS(2)
             WR-LOTS-NF-RECOVERIES(1)     WR-LOTS-NF-RECOVERIES(2)

             WR-BSR-BONUS(1)              WR-BSR-BONUS(2).

             MOVE 0 TO WR-LTOUCH-DATE.
