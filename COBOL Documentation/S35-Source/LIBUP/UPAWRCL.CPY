      ************************************************************
      *  UPAWRCL   ASCII CLEAR RATING SHEET FIELDS
      ************************************************************
       CLEAR-AWR-FILE SECTION.

           MOVE ALL X"09" TO AWR-REC.

           MOVE 0 TO
             AWR-NONFILE-ENDING-BALANCE
             AWR-CUR-NO-CUST
             AWR-ELM-NO-CUST              AWR-ELY-NO-CUST
             AWR-OUTS-BALANCE
             AWR-ELM-OUTS-BALANCE         AWR-ELY-OUTS-BALANCE
             AWR-NO-NB(1)                 AWR-NO-NB(2)
             AWR-NO-FB(1)                 AWR-NO-FB(2)
             AWR-NO-PB(1)                 AWR-NO-PB(2)
             AWR-GROSS-VOLUME(1)          AWR-GROSS-VOLUME(2)
             AWR-PRETAX-PROFIT(1)         AWR-PRETAX-PROFIT(2)
             AWR-INTEREST-ALLOCATION(1)   AWR-INTEREST-ALLOCATION(2)
             AWR-SUPERVISION(1)           AWR-SUPERVISION(2)
             AWR-GEN-RESERVE-ADJ(1)       AWR-GEN-RESERVE-ADJ(2)
             AWR-INTANGIBLE-AMORT(1)      AWR-INTANGIBLE-AMORT(2)
             AWR-ROBBERIES(1)             AWR-ROBBERIES(2)
             AWR-RATING-SHEET-INCOME(1)   AWR-RATING-SHEET-INCOME(2)
             AWR-INT-FEE-INCOME(1)        AWR-INT-FEE-INCOME(2)
             AWR-INSURANCE-INCOME(1)      AWR-INSURANCE-INCOME(2)
             AWR-RATING-SHEET-EXPENSES(1) AWR-RATING-SHEET-EXPENSES(2)
             AWR-PERSONNEL-EXPENSE(1)     AWR-PERSONNEL-EXPENSE(2)
             AWR-CHARGEOFF-EXPENSE(1)     AWR-CHARGEOFF-EXPENSE(2)
             AWR-NO-CHARGEOFFS(1)         AWR-NO-CHARGEOFFS(2)
             AWR-RECOVERY-INCOME(1)       AWR-RECOVERY-INCOME(2)
             AWR-BAD-DEBT-INCREASE(1)     AWR-BAD-DEBT-INCREASE(2)
             AWR-NO-30-DAY-ACCTS          AWR-AMT-30-DAY-ACCTS
             AWR-NO-60-DAY-ACCTS          AWR-AMT-60-DAY-ACCTS
             AWR-NO-90-DAY-ACCTS          AWR-AMT-90-DAY-ACCTS
             AWR-ELM-AMT-90-DAY-ACCTS     AWR-ELY-AMT-90-DAY-ACCTS

             AWR-YTD-PCT-TURNOVER         AWR-YTD-PCT-RATE-SHT-PROFIT
             AWR-YTD-PCT-RS-INC-LEDGER    AWR-YTD-PCT-INT-FEE-LEDGER
             AWR-YTD-PCT-INS-INC-LEDGER   AWR-YTD-PCT-EXPENSE-LEDGER
             AWR-YTD-PCT-BAD-DEBT-LEDGER  AWR-YTD-PCT-NF-BNK-LEDGER
             AWR-YTD-PCT-PRETAX-LEDGER    AWR-YTD-PCT-DEBT-NF-LEDGER

             AWR-WCBC-CUR-NO-CUST
             AWR-WCBC-ELM-NO-CUST          AWR-WCBC-ELY-NO-CUST
             AWR-WCBC-OUTS-BALANCE
             AWR-WCBC-ELM-OUTS-BAL         AWR-WCBC-ELY-OUTS-BAL
             AWR-WCBC-NO-NB(1)             AWR-WCBC-NO-NB(2)
             AWR-WCBC-NO-FB(1)             AWR-WCBC-NO-FB(2)
             AWR-WCBC-NO-PB(1)             AWR-WCBC-NO-PB(2)
             AWR-WCBC-GROSS-VOLUME(1)      AWR-WCBC-GROSS-VOLUME(2)
             AWR-WCBC-LATE-CHGS(1)         AWR-WCBC-LATE-CHGS(2)
             AWR-WCBC-NET-FIN-CHGS(1)      AWR-WCBC-NET-FIN-CHGS(2)
             AWR-WCBC-SALES(1)             AWR-WCBC-SALES(2)
             AWR-WCBC-COST-OF-SALES(1)     AWR-WCBC-COST-OF-SALES(2)
             AWR-WCBC-EXPENSES(1)          AWR-WCBC-EXPENSES(2)
             AWR-WCBC-RECOVERY-INCOME(1)   AWR-WCBC-RECOVERY-INCOME(2)
             AWR-WCBC-CHARGEOFF-EXPENSE(1) AWR-WCBC-CHARGEOFF-EXPENSE(2)
             AWR-WCBC-BAD-DEBT-BNKRPCY(1)  AWR-WCBC-BAD-DEBT-BNKRPCY(2)
             AWR-WCBC-NO-30-DAY-ACCTS      AWR-WCBC-AMT-30-DAY-ACCTS
             AWR-WCBC-NO-60-DAY-ACCTS      AWR-WCBC-AMT-60-DAY-ACCTS
             AWR-WCBC-NO-90-DAY-ACCTS      AWR-WCBC-AMT-90-DAY-ACCTS
             AWR-WCBC-ELM-AMT-90-DAYS      AWR-WCBC-ELY-AMT-90-DAYS
             AWR-WCBC-PCT-VOLUME           AWR-WCBC-GROSS-PROF-PCT

             AWR-LARGE-CUR-NO-CUST         AWR-LARGE-ELM-NO-CUST
             AWR-LARGE-ELY-NO-CUST         AWR-LARGE-OUTS-BALANCE
             AWR-LARGE-ELM-OUTS-BAL        AWR-LARGE-ELY-OUTS-BAL
             AWR-LARGE-NO-NB(1)            AWR-LARGE-NO-NB(2)
             AWR-LARGE-NO-FB(1)            AWR-LARGE-NO-FB(2)
             AWR-LARGE-NO-PB(1)            AWR-LARGE-NO-PB(2)
             AWR-LARGE-GROSS-VOLUME(1) AWR-LARGE-GROSS-VOLUME(2)
             AWR-LARGE-NO-30-DAY-ACCTS AWR-LARGE-AMT-30-DAY-ACCTS
             AWR-LARGE-NO-60-DAY-ACCTS AWR-LARGE-AMT-60-DAY-ACCTS
             AWR-LARGE-NO-90-DAY-ACCTS AWR-LARGE-AMT-90-DAY-ACCTS
             AWR-LARGE-ELM-AMT-90-DAYS AWR-LARGE-ELY-AMT-90-DAYS

             AWR-LIFE-CLAIMS(1)            AWR-LIFE-CLAIMS(2)
             AWR-AH-CLAIMS(1)              AWR-AH-CLAIMS(2)
             AWR-FIRE-CLAIMS(1)            AWR-FIRE-CLAIMS(2)
             AWR-AUTO-CLAIMS(1)            AWR-AUTO-CLAIMS(2)
             AWR-UNEMP-CLAIMS(1)           AWR-UNEMP-CLAIMS(2)
             AWR-AD-D-CLAIMS(1)            AWR-AD-D-CLAIMS(2)
             AWR-L-PROT-CLAIMS(1)          AWR-L-PROT-CLAIMS(2)
             AWR-NF-NON-BK-CLAIMS(1)       AWR-NF-NON-BK-CLAIMS(2)
             AWR-NF-BK-CLAIMS(1)           AWR-NF-BK-CLAIMS(2)
             AWR-ROAD-GARD-CLAIMS(1)       AWR-ROAD-GARD-CLAIMS(2)

             AWR-NO-LIFE-CLAIMS(1)         AWR-NO-LIFE-CLAIMS(2)
             AWR-NO-AH-CLAIMS(1)           AWR-NO-AH-CLAIMS(2)
             AWR-NO-FIRE-CLAIMS(1)         AWR-NO-FIRE-CLAIMS(2)
             AWR-NO-AUTO-CLAIMS(1)         AWR-NO-AUTO-CLAIMS(2)
             AWR-NO-UNEMP-CLAIMS(1)        AWR-NO-UNEMP-CLAIMS(2)
             AWR-NO-AD-D-CLAIMS(1)         AWR-NO-AD-D-CLAIMS(2)
             AWR-NO-L-PROT-CLAIMS(1)       AWR-NO-L-PROT-CLAIMS(2)
             AWR-NO-NF-NON-BK-CLAIMS(1)    AWR-NO-NF-NON-BK-CLAIMS(2)
             AWR-NO-NF-BK-CLAIMS(1)        AWR-NO-NF-BK-CLAIMS(2)
             AWR-NO-ROAD-GARD-CLAIMS(1)    AWR-NO-ROAD-GARD-CLAIMS(2)

             AWR-LIFE-PREMS(1)             AWR-LIFE-PREMS(2)
             AWR-AH-PREMS(1)               AWR-AH-PREMS(2)
             AWR-FIRE-PREMS(1)             AWR-FIRE-PREMS(2)
             AWR-AUTO-PREMS(1)             AWR-AUTO-PREMS(2)
             AWR-UNEMP-PREMS(1)            AWR-UNEMP-PREMS(2)
             AWR-AD-D-PREMS(1)             AWR-AD-D-PREMS(2)
             AWR-L-PROT-PREMS(1)           AWR-L-PROT-PREMS(2)
             AWR-NON-FILE-PREMS(1)         AWR-NON-FILE-PREMS(2)
             AWR-ROAD-GARD-PREMS(1)        AWR-ROAD-GARD-PREMS(2)

             AWR-NO-LIFE-PREMS(1)          AWR-NO-LIFE-PREMS(2)
             AWR-NO-AH-PREMS(1)            AWR-NO-AH-PREMS(2)
             AWR-NO-FIRE-PREMS(1)          AWR-NO-FIRE-PREMS(2)
             AWR-NO-AUTO-PREMS(1)          AWR-NO-AUTO-PREMS(2)
             AWR-NO-UNEMP-PREMS(1)         AWR-NO-UNEMP-PREMS(2)
             AWR-NO-AD-D-PREMS(1)          AWR-NO-AD-D-PREMS(2)
             AWR-NO-L-PROT-PREMS(1)        AWR-NO-L-PROT-PREMS(2)
             AWR-NO-NON-FILE-PREMS(1)      AWR-NO-NON-FILE-PREMS(2)
             AWR-NO-ROAD-GARD-PREMS(1)     AWR-NO-ROAD-GARD-PREMS(2)

             AWR-LIFE-REFS(1)              AWR-LIFE-REFS(2)
             AWR-AH-REFS(1)                AWR-AH-REFS(2)
             AWR-FIRE-REFS(1)              AWR-FIRE-REFS(2)
             AWR-AUTO-REFS(1)              AWR-AUTO-REFS(2)
             AWR-UNEMP-REFS(1)             AWR-UNEMP-REFS(2)
             AWR-AD-D-REFS(1)              AWR-AD-D-REFS(2)
             AWR-L-PROT-REFS(1)            AWR-L-PROT-REFS(2)
             AWR-NON-FILE-REFS(1)          AWR-NON-FILE-REFS(2)
             AWR-ROAD-GARD-REFS(1)         AWR-ROAD-GARD-REFS(2)
             AWR-NF-RECOVERIES(1)          AWR-NF-RECOVERIES(2)

             AWR-NO-CUST-1-YR-AGO          AWR-OUTS-BAL-1-YR-AGO
             AWR-WCBC-NO-CUST-1-YR-AGO     AWR-WCBC-OUTS-BAL-1-YR-AGO
             AWR-LARGE-NO-CUST-1-YR-AGO    AWR-LARGE-OUTS-BAL-1-YR-AGO
             AWR-CUR-NO-CONT-SLOW          AWR-WCBC-CUR-NO-CONT-SLOW
             AWR-LARGE-CUR-NO-CONT-SLOW

             AWR-CASHIER-BONUS             AWR-ASST-MGR-BONUS
             AWR-MGR-BONUS

             AWR-LOTS-NF-CLAIMS(1)         AWR-LOTS-NF-CLAIMS(2)
             AWR-LOTS-NO-NF-CLAIMS(1)      AWR-LOTS-NO-NF-CLAIMS(2)
             AWR-LOTS-NON-FILE-PREMS(1)    AWR-LOTS-NON-FILE-PREMS(2)
             AWR-LOTS-NO-NON-FILE-PREMS(1) AWR-LOTS-NO-NON-FILE-PREMS(2)
             AWR-LOTS-NF-RECOVERIES(1)     AWR-LOTS-NF-RECOVERIES(2)

             AWR-BSR-BONUS(1)              AWR-BSR-BONUS(2)
             AWR-LTOUCH-DATE.

           MOVE SPACES TO AWR-MGR-PENALTY-FG.
