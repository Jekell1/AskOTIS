       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOWRIN.
       AUTHOR.        AMM.
       DATE-WRITTEN.  04-21-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOWRIN
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : WRFILE 
      * TABLE DESC    : WORLD RATHING SHEET FILE
      * OUTPUT FILE   : XPC/IOWRIN.PRE
      * USED FOR      : LIBSP/SPWSWR.CPY  --- QWR-REC
      *                 LIBSP/SPWR1R.CPY  --- READ
      *                 LIBSP/SPWR1RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * AMM 2021-0421 ***NEW***
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)IO/IOWRIN.CBL S34 07/30/21-10:00:19 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBSP/SP01WR.CPY".
       COPY "LIBSP/SPWSWR.CPY".

      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S 01 SQL COPY MEMBERS
      *#################################################################

      *=================================================================
      *
      *    Q - F I L E
      *
      *-----------------------------------------------------------------
      *    FOR UPDATING THE FILE'S WS COPY MEMBER: LIB??/??WS??.CPY
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QWR1-WHERE-BEG-KEY.
           03  QWR1-WBEG-YYYYMM            PIC 9(06).
           03  QWR1-WBEG-BRNO              PIC 9(04).
 
       01  QWR1-WHERE-END-KEY.
           03  QWR1-WEND-YYYYMM            PIC 9(06).
           03  QWR1-WEND-BRNO              PIC 9(04).
 
       01  QWR-REC.
           03  QWR-YYYYMM                   PIC 9(6).
           03  QWR-BRNO                     PIC 9(4).
           03  QWR-LTOUCH-DATE              PIC X(10).
           03  QWR-CUR-NO-CUST              PIC S9(5).
           03  QWR-ELM-NO-CUST              PIC S9(5).
           03  QWR-ELY-NO-CUST              PIC S9(5).
           03  QWR-OUTS-BALANCE             PIC S9(9)V99.
           03  QWR-ELM-OUTS-BALANCE         PIC S9(9)V99.
           03  QWR-ELY-OUTS-BALANCE         PIC S9(9)V99.
           03  QWR-NO-NB-1                  PIC S9(5).
           03  QWR-NO-FB-1                  PIC S9(5).
           03  QWR-NO-PB-1                  PIC S9(5).
           03  QWR-GROSS-VOLUME-1           PIC S9(9)V99.
           03  QWR-BSR-BONUS-1              PIC S9(7)V99.
           03  QWR-INTEREST-ALLOCATION-1    PIC S9(7)V99.
           03  QWR-OLD-SUPERVISION-1        PIC S9(7)V99.
           03  QWR-GEN-RESERVE-ADJ-1        PIC S9(7)V99.
           03  QWR-INTANGIBLE-AMORT-1       PIC S9(7)V99.
           03  QWR-ROBBERIES-1              PIC S9(7)V99.
           03  QWR-RATING-SHEET-INCOME-1    PIC S9(7)V99.
           03  QWR-INT-FEE-INCOME-1         PIC S9(7)V99.
           03  QWR-INSURANCE-INCOME-1       PIC S9(7)V99.
           03  QWR-RATING-SHEET-EXPENSES-1  PIC S9(7)V99.
           03  QWR-PERSONNEL-EXPENSE-1      PIC S9(7)V99.
           03  QWR-CHARGEOFF-EXPENSE-1      PIC S9(7)V99.
           03  QWR-NO-CHARGEOFFS-1          PIC S9(5).
           03  QWR-RECOVERY-INCOME-1        PIC S9(7)V99.
           03  QWR-BAD-DEBT-INCREASE-1      PIC S9(7)V99.
           03  QWR-NO-NB-2                  PIC S9(5).
           03  QWR-NO-FB-2                  PIC S9(5).
           03  QWR-NO-PB-2                  PIC S9(5).
           03  QWR-GROSS-VOLUME-2           PIC S9(9)V99.
           03  QWR-BSR-BONUS-2              PIC S9(7)V99.
           03  QWR-INTEREST-ALLOCATION-2    PIC S9(7)V99.
           03  QWR-OLD-SUPERVISION-2        PIC S9(7)V99.
           03  QWR-GEN-RESERVE-ADJ-2        PIC S9(7)V99.
           03  QWR-INTANGIBLE-AMORT-2       PIC S9(7)V99.
           03  QWR-ROBBERIES-2              PIC S9(7)V99.
           03  QWR-RATING-SHEET-INCOME-2    PIC S9(7)V99.
           03  QWR-INT-FEE-INCOME-2         PIC S9(7)V99.
           03  QWR-INSURANCE-INCOME-2       PIC S9(7)V99.
           03  QWR-RATING-SHEET-EXPENSES-2  PIC S9(7)V99.
           03  QWR-PERSONNEL-EXPENSE-2      PIC S9(7)V99.
           03  QWR-CHARGEOFF-EXPENSE-2      PIC S9(7)V99.
           03  QWR-NO-CHARGEOFFS-2          PIC S9(5).
           03  QWR-RECOVERY-INCOME-2        PIC S9(7)V99.
           03  QWR-BAD-DEBT-INCREASE-2      PIC S9(7)V99.
           03  QWR-NO-30-DAY-ACCTS          PIC S9(7).
           03  QWR-AMT-30-DAY-ACCTS         PIC S9(9)V99.
           03  QWR-NO-60-DAY-ACCTS          PIC S9(7).
           03  QWR-AMT-60-DAY-ACCTS         PIC S9(9)V99.
           03  QWR-NO-90-DAY-ACCTS          PIC S9(7).
           03  QWR-AMT-90-DAY-ACCTS         PIC S9(9)V99.
           03  QWR-ELM-AMT-90-DAY-ACCTS     PIC S9(9)V99.
           03  QWR-ELY-AMT-90-DAY-ACCTS     PIC S9(9)V99.
           03  QWR-YTD-PCT-TURNOVER         PIC S9(3)V99.
           03  QWR-YTD-PCT-RATE-SHT-PROFIT  PIC S9(3)V99.
           03  QWR-YTD-PCT-RS-INC-LEDGER    PIC S9(3)V99.
           03  QWR-YTD-PCT-INT-FEE-LEDGER   PIC S9(3)V99.
           03  QWR-YTD-PCT-INS-INC-LEDGER   PIC S9(3)V99.
           03  QWR-YTD-PCT-EXPENSE-LEDGER   PIC S9(3)V99.
           03  QWR-YTD-PCT-BAD-DEBT-LEDGER  PIC S9(3)V99.
           03  QWR-YTD-PCT-NF-BNK-LEDGER    PIC S9(3)V99.
           03  QWR-YTD-PCT-PRETAX-LEDGER    PIC S9(3)V99.
           03  QWR-YTD-PCT-DEBT-NF-LEDGER   PIC S9(3)V99.
           03  QWR-WCBC-CUR-NO-CUST         PIC S9(5).
           03  QWR-WCBC-ELM-NO-CUST         PIC S9(5).
           03  QWR-WCBC-ELY-NO-CUST         PIC S9(5).
           03  QWR-WCBC-OUTS-BALANCE        PIC S9(9)V99.
           03  QWR-WCBC-ELM-OUTS-BAL        PIC S9(9)V99.
           03  QWR-WCBC-ELY-OUTS-BAL        PIC S9(9)V99.
           03  QWR-WCBC-NO-NB-1             PIC S9(5).
           03  QWR-WCBC-NO-FB-1             PIC S9(5).
           03  QWR-WCBC-NO-PB-1             PIC S9(5).
           03  QWR-WCBC-GROSS-VOLUME-1      PIC S9(9)V99.
           03  QWR-WCBC-LATE-CHGS-1         PIC S9(9)V99.
           03  QWR-WCBC-NET-FIN-CHGS-1      PIC S9(7)V99.
           03  QWR-WCBC-SALES-1             PIC S9(7)V99.
           03  QWR-WCBC-COST-OF-SALES-1     PIC S9(7)V99.
           03  QWR-WCBC-EXPENSES-1          PIC S9(7)V99.
           03  QWR-WCBC-RECOVERY-INCOME-1   PIC S9(7)V99.
           03  QWR-WCBC-CHARGEOFF-EXPENSE-1 PIC S9(7)V99.
           03  QWR-WCBC-BAD-DEBT-BNKRPCY-1  PIC S9(7)V99.
           03  QWR-WCBC-NO-NB-2             PIC S9(5).
           03  QWR-WCBC-NO-FB-2             PIC S9(5).
           03  QWR-WCBC-NO-PB-2             PIC S9(5).
           03  QWR-WCBC-GROSS-VOLUME-2      PIC S9(9)V99.
           03  QWR-WCBC-LATE-CHGS-2         PIC S9(9)V99.
           03  QWR-WCBC-NET-FIN-CHGS-2      PIC S9(7)V99.
           03  QWR-WCBC-SALES-2             PIC S9(7)V99.
           03  QWR-WCBC-COST-OF-SALES-2     PIC S9(7)V99.
           03  QWR-WCBC-EXPENSES-2          PIC S9(7)V99.
           03  QWR-WCBC-RECOVERY-INCOME-2   PIC S9(7)V99.
           03  QWR-WCBC-CHARGEOFF-EXPENSE-2 PIC S9(7)V99.
           03  QWR-WCBC-BAD-DEBT-BNKRPCY-2  PIC S9(7)V99.
           03  QWR-WCBC-NO-30-DAY-ACCTS     PIC S9(7).
           03  QWR-WCBC-AMT-30-DAY-ACCTS    PIC S9(9)V99.
           03  QWR-WCBC-NO-60-DAY-ACCTS     PIC S9(7).
           03  QWR-WCBC-AMT-60-DAY-ACCTS    PIC S9(9)V99.
           03  QWR-WCBC-NO-90-DAY-ACCTS     PIC S9(7).
           03  QWR-WCBC-AMT-90-DAY-ACCTS    PIC S9(9)V99.
           03  QWR-WCBC-ELM-AMT-90-DAYS     PIC S9(9)V99.
           03  QWR-WCBC-ELY-AMT-90-DAYS     PIC S9(9)V99.
           03  QWR-WCBC-PCT-VOLUME          PIC S9(3)V99.
           03  QWR-WCBC-GROSS-PROF-PCT      PIC S9(3)V99.
           03  QWR-LARGE-CUR-NO-CUST        PIC S9(5).
           03  QWR-LARGE-ELM-NO-CUST        PIC S9(5).
           03  QWR-LARGE-ELY-NO-CUST        PIC S9(5).
           03  QWR-LARGE-OUTS-BALANCE       PIC S9(9)V99.
           03  QWR-LARGE-ELM-OUTS-BAL       PIC S9(9)V99.
           03  QWR-LARGE-ELY-OUTS-BAL       PIC S9(9)V99.
           03  QWR-LARGE-NO-NB-1            PIC S9(5).
           03  QWR-LARGE-NO-FB-1            PIC S9(5).
           03  QWR-LARGE-NO-PB-1            PIC S9(5).
           03  QWR-LARGE-GROSS-VOLUME-1     PIC S9(9)V99.
           03  QWR-LARGE-NO-NB-2            PIC S9(5).
           03  QWR-LARGE-NO-FB-2            PIC S9(5).
           03  QWR-LARGE-NO-PB-2            PIC S9(5).
           03  QWR-LARGE-GROSS-VOLUME-2     PIC S9(9)V99.
           03  QWR-LARGE-NO-30-DAY-ACCTS    PIC S9(7).
           03  QWR-LARGE-AMT-30-DAY-ACCTS   PIC S9(9)V99.
           03  QWR-LARGE-NO-60-DAY-ACCTS    PIC S9(7).
           03  QWR-LARGE-AMT-60-DAY-ACCTS   PIC S9(9)V99.
           03  QWR-LARGE-NO-90-DAY-ACCTS    PIC S9(7).
           03  QWR-LARGE-AMT-90-DAY-ACCTS   PIC S9(9)V99.
           03  QWR-LARGE-ELM-AMT-90-DAYS    PIC S9(9)V99.
           03  QWR-LARGE-ELY-AMT-90-DAYS    PIC S9(9)V99.
           03  QWR-LIFE-CLAIMS-1            PIC S9(7)V99.
           03  QWR-AH-CLAIMS-1              PIC S9(7)V99.
           03  QWR-FIRE-CLAIMS-1            PIC S9(7)V99.
           03  QWR-AUTO-CLAIMS-1            PIC S9(7)V99.
           03  QWR-UNEMP-CLAIMS-1           PIC S9(7)V99.
           03  QWR-AD-D-CLAIMS-1            PIC S9(7)V99.
           03  QWR-L-PROT-CLAIMS-1          PIC S9(7)V99.
           03  QWR-NF-NON-BK-CLAIMS-1       PIC S9(7)V99.
           03  QWR-NF-BK-CLAIMS-1           PIC S9(7)V99.
           03  QWR-ROAD-GARD-CLAIMS-1       PIC S9(7)V99.
           03  QWR-NO-LIFE-CLAIMS-1         PIC S9(5).
           03  QWR-NO-AH-CLAIMS-1           PIC S9(5).
           03  QWR-NO-FIRE-CLAIMS-1         PIC S9(5).
           03  QWR-NO-AUTO-CLAIMS-1         PIC S9(5).
           03  QWR-NO-UNEMP-CLAIMS-1        PIC S9(5).
           03  QWR-NO-AD-D-CLAIMS-1         PIC S9(5).
           03  QWR-NO-L-PROT-CLAIMS-1       PIC S9(5).
           03  QWR-NO-NF-NON-BK-CLAIMS-1    PIC S9(5).
           03  QWR-NO-NF-BK-CLAIMS-1        PIC S9(5).
           03  QWR-NO-ROAD-GARD-CLAIMS-1    PIC S9(5).
           03  QWR-LIFE-PREMS-1             PIC S9(7)V99.
           03  QWR-AH-PREMS-1               PIC S9(7)V99.
           03  QWR-FIRE-PREMS-1             PIC S9(7)V99.
           03  QWR-AUTO-PREMS-1             PIC S9(7)V99.
           03  QWR-UNEMP-PREMS-1            PIC S9(7)V99.
           03  QWR-AD-D-PREMS-1             PIC S9(7)V99.
           03  QWR-L-PROT-PREMS-1           PIC S9(7)V99.
           03  QWR-NON-FILE-PREMS-1         PIC S9(7)V99.
           03  QWR-ROAD-GARD-PREMS-1        PIC S9(7)V99.
           03  QWR-NO-LIFE-PREMS-1          PIC S9(5).
           03  QWR-NO-AH-PREMS-1            PIC S9(5).
           03  QWR-NO-FIRE-PREMS-1          PIC S9(5).
           03  QWR-NO-AUTO-PREMS-1          PIC S9(5).
           03  QWR-NO-UNEMP-PREMS-1         PIC S9(5).
           03  QWR-NO-AD-D-PREMS-1          PIC S9(5).
           03  QWR-NO-L-PROT-PREMS-1        PIC S9(5).
           03  QWR-NO-NON-FILE-PREMS-1      PIC S9(5).
           03  QWR-NO-ROAD-GARD-PREMS-1     PIC S9(5).
           03  QWR-LIFE-REFS-1              PIC S9(7)V99.
           03  QWR-AH-REFS-1                PIC S9(7)V99.
           03  QWR-FIRE-REFS-1              PIC S9(7)V99.
           03  QWR-AUTO-REFS-1              PIC S9(7)V99.
           03  QWR-UNEMP-REFS-1             PIC S9(7)V99.
           03  QWR-AD-D-REFS-1              PIC S9(7)V99.
           03  QWR-L-PROT-REFS-1            PIC S9(7)V99.
           03  QWR-NON-FILE-REFS-1          PIC S9(7)V99.
           03  QWR-ROAD-GARD-REFS-1         PIC S9(7)V99.
           03  QWR-NF-RECOVERIES-1          PIC S9(7)V99.
           03  QWR-LIFE-CLAIMS-2            PIC S9(7)V99.
           03  QWR-AH-CLAIMS-2              PIC S9(7)V99.
           03  QWR-FIRE-CLAIMS-2            PIC S9(7)V99.
           03  QWR-AUTO-CLAIMS-2            PIC S9(7)V99.
           03  QWR-UNEMP-CLAIMS-2           PIC S9(7)V99.
           03  QWR-AD-D-CLAIMS-2            PIC S9(7)V99.
           03  QWR-L-PROT-CLAIMS-2          PIC S9(7)V99.
           03  QWR-NF-NON-BK-CLAIMS-2       PIC S9(7)V99.
           03  QWR-NF-BK-CLAIMS-2           PIC S9(7)V99.
           03  QWR-ROAD-GARD-CLAIMS-2       PIC S9(7)V99.
           03  QWR-NO-LIFE-CLAIMS-2         PIC S9(5).
           03  QWR-NO-AH-CLAIMS-2           PIC S9(5).
           03  QWR-NO-FIRE-CLAIMS-2         PIC S9(5).
           03  QWR-NO-AUTO-CLAIMS-2         PIC S9(5).
           03  QWR-NO-UNEMP-CLAIMS-2        PIC S9(5).
           03  QWR-NO-AD-D-CLAIMS-2         PIC S9(5).
           03  QWR-NO-L-PROT-CLAIMS-2       PIC S9(5).
           03  QWR-NO-NF-NON-BK-CLAIMS-2    PIC S9(5).
           03  QWR-NO-NF-BK-CLAIMS-2        PIC S9(5).
           03  QWR-NO-ROAD-GARD-CLAIMS-2    PIC S9(5).
           03  QWR-LIFE-PREMS-2             PIC S9(7)V99.
           03  QWR-AH-PREMS-2               PIC S9(7)V99.
           03  QWR-FIRE-PREMS-2             PIC S9(7)V99.
           03  QWR-AUTO-PREMS-2             PIC S9(7)V99.
           03  QWR-UNEMP-PREMS-2            PIC S9(7)V99.
           03  QWR-AD-D-PREMS-2             PIC S9(7)V99.
           03  QWR-L-PROT-PREMS-2           PIC S9(7)V99.
           03  QWR-NON-FILE-PREMS-2         PIC S9(7)V99.
           03  QWR-ROAD-GARD-PREMS-2        PIC S9(7)V99.
           03  QWR-NO-LIFE-PREMS-2          PIC S9(5).
           03  QWR-NO-AH-PREMS-2            PIC S9(5).
           03  QWR-NO-FIRE-PREMS-2          PIC S9(5).
           03  QWR-NO-AUTO-PREMS-2          PIC S9(5).
           03  QWR-NO-UNEMP-PREMS-2         PIC S9(5).
           03  QWR-NO-AD-D-PREMS-2          PIC S9(5).
           03  QWR-NO-L-PROT-PREMS-2        PIC S9(5).
           03  QWR-NO-NON-FILE-PREMS-2      PIC S9(5).
           03  QWR-NO-ROAD-GARD-PREMS-2     PIC S9(5).
           03  QWR-LIFE-REFS-2              PIC S9(7)V99.
           03  QWR-AH-REFS-2                PIC S9(7)V99.
           03  QWR-FIRE-REFS-2              PIC S9(7)V99.
           03  QWR-AUTO-REFS-2              PIC S9(7)V99.
           03  QWR-UNEMP-REFS-2             PIC S9(7)V99.
           03  QWR-AD-D-REFS-2              PIC S9(7)V99.
           03  QWR-L-PROT-REFS-2            PIC S9(7)V99.
           03  QWR-NON-FILE-REFS-2          PIC S9(7)V99.
           03  QWR-ROAD-GARD-REFS-2         PIC S9(7)V99.
           03  QWR-NF-RECOVERIES-2          PIC S9(7)V99.
           03  QWR-NONFILE-ENDING-BALANCE   PIC S9(7)V99.
           03  QWR-NO-CUST-1-YR-AGO         PIC S9(5).
           03  QWR-OUTS-BAL-1-YR-AGO        PIC S9(9)V99.
           03  QWR-WCBC-NO-CUST-1-YR-AGO    PIC S9(5).
           03  QWR-WCBC-OUTS-BAL-1-YR-AGO   PIC S9(9)V99.
           03  QWR-LARGE-NO-CUST-1-YR-AGO   PIC S9(5).
           03  QWR-LARGE-OUTS-BAL-1-YR-AGO  PIC S9(9)V99.
           03  QWR-CUR-NO-CONT-SLOW         PIC S9(5).
           03  QWR-WCBC-CUR-NO-CONT-SLOW    PIC S9(5).
           03  QWR-LARGE-CUR-NO-CONT-SLOW   PIC S9(5).
           03  QWR-CASHIER-BONUS            PIC S9(5).
           03  QWR-ASST-MGR-BONUS           PIC S9(5).
           03  QWR-MGR-BONUS                PIC S9(5)V99.
           03  QWR-LOTS-NF-CLAIMS-1         PIC S9(7)V99.
           03  QWR-LOTS-NO-NF-CLAIMS-1      PIC S9(5).
           03  QWR-LOTS-NON-FILE-PREMS-1    PIC S9(7)V99.
           03  QWR-LOTS-NO-NON-FILE-PREMS-1 PIC S9(5).
           03  QWR-LOTS-NF-RECOVERIES-1     PIC S9(7)V99.
           03  QWR-LOTS-NF-CLAIMS-2         PIC S9(7)V99.
           03  QWR-LOTS-NO-NF-CLAIMS-2      PIC S9(5).
           03  QWR-LOTS-NON-FILE-PREMS-2    PIC S9(7)V99.
           03  QWR-LOTS-NO-NON-FILE-PREMS-2 PIC S9(5).
           03  QWR-LOTS-NF-RECOVERIES-2     PIC S9(7)V99.
           03  QWR-PRETAX-PROFIT-1          PIC S9(9)V99.
           03  QWR-SUPERVISION-1            PIC S9(9)V99.
           03  QWR-PRETAX-PROFIT-2          PIC S9(9)V99.
           03  QWR-SUPERVISION-2            PIC S9(9)V99.
           03  QWR-MGR-PENALTY-FG           PIC X.
           03  QWR-NO-TAX-RETURNS-1         PIC S9(5).
           03  QWR-NO-TAX-RETURNS-2         PIC S9(5).

       EXEC SQL END DECLARE SECTION END-EXEC.
      *#################################################################
      *    ENDING OF UPDATE TO THE FILE'S WS COPY MEMBERS
      *#################################################################

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  RECORDS-FOUND               PIC X(01)     VALUE "X".
           88  NO-RECORDS-FOUND                      VALUE "N".
           88  EXIT-PRIOR-TO-EXECUTION               VALUE "X".

       01  RECORDS-READ                PIC 9(11)     VALUE ZEROES.

       01  SUBSCRIPT-WORKERS.
           03  SUB                     PIC 9(09)     VALUE ZEROES.
           03  S1                      PIC 9(09)     VALUE ZEROES.
           03  S2                      PIC 9(09)     VALUE ZEROES.
           03  S3                      PIC 9(09)     VALUE ZEROES.

      ******************************************************************
      *
      *    L E G E N D - W O R K E R S
      *
      ******************************************************************
       01  LEGEND                      PIC X(77)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(09)     VALUE "LEGEND:B.".

      *=================================================================
       01  LEGEND-ACCEPT-YN.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, UP-ARROW, (".
           03  FILLER PIC X(30) VALUE "Y)ES OR (N)O TO ACCEPT        ".
           03  FILLER PIC X(17) VALUE "   ACCEPT (Y/N): ".

       01  LEGEND-PRINTER-OPTIONS.
           03  FILLER PIC X(30) VALUE "F7-EXIT, ENTER PRINTER OPTIONS".

       01  LEGEND-BEG-YYYYMM.
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING YYYYMM     (F3-ALL) ".

       01  LEGEND-END-YYYYMM.
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING YYYYMM     ".

       01  LEGEND-BEG-BRNO.
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING BRNO       (F3-ALL) ".

       01  LEGEND-END-BRNO.
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING BRNO       ".

       01  LEGEND-PROCESS-KEY.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, V, ENTER NU".
           03  FILLER PIC X(30) VALUE "MBER OF FILE KEY TO PROCESS   ".

       01  LEGEND-BEG-END-RANGE.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, V, ENTER RA".
           03  FILLER PIC X(30) VALUE "ANGE VALUE                    ".

      ******************************************************************
      *
      *    P A C E - W O R K E R S
      *
      ******************************************************************
       78  PACE-COUNTER-MAX                          VALUE 1000.
       77  PACE-COUNTER                PIC 9(6)      VALUE ZEROES.
       77  PACE-DISPLAY-MAX            PIC ZZZ,ZZ9   VALUE ZEROES.
       78  PACE-MAX                                  VALUE 15.
       77  PACE-LEV                    PIC 9(2)      VALUE ZEROES.

       01  PACE-HEAD-LINE.
           03  FILLER PIC X(28) VALUE "TIME        KEY             ".
           03  PACE-HEAD-MAX    PIC ZZZ,ZZ9 VALUE ZEROES.
           03  FILLER PIC X(05) VALUE " RECS".

       01  PACE-TIME.
           03  PACE-TIME-HH            PIC 99.
           03  FILLER                  PIC X(1)      VALUE ":".
           03  PACE-TIME-MM            PIC 99.
           03  FILLER                  PIC X(1)      VALUE ":".
           03  PACE-TIME-SS            PIC 99.
           03  FILLER                  PIC X(1)      VALUE ".".
           03  PACE-TIME-HD            PIC 99.

       01  PACE-DETAIL.
           03  PACE-DETAIL-TIME        PIC 99/99/99/99.
           03  FILLER                  PIC X(1).
           03  PACE-DETAIL-KEY         PIC X(28).

       01  PACE-BUF.
           03  PACE-ELE                PIC X(40)     OCCURS PACE-MAX.

       01  PACE-LIST.
           03  FILLER PIC X(24) VALUE "PACE-01 PACE-02 PACE-03 ".
           03  FILLER PIC X(24) VALUE "PACE-04 PACE-05 PACE-06 ".
           03  FILLER PIC X(24) VALUE "PACE-07 PACE-08 PACE-09 ".
           03  FILLER PIC X(24) VALUE "PACE-10 PACE-11 PACE-12 ".
           03  FILLER PIC X(24) VALUE "PACE-13 PACE-14 PACE-15.".

      ******************************************************************
      *
      *    P A R A M E T E R - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-YYYYMM              PIC 9(06).
           03  END-YYYYMM              PIC 9(06).
           03  BEG-BRNO                PIC 9(04).
           03  END-BRNO                PIC 9(04).

           03  PROCESS-KEY             PIC 9(01).

      *-----------------------------------------------------------------
       01  VERSION-CONTROL             PIC X(01)     VALUE "A".

      ******************************************************************
      *
      *    R E P O R T - W O R K E R S
      *
      ******************************************************************
       01  PRINT-LINE-WORKERS.
           03  LN-COUNT                PIC 9(03)     VALUE 99.
           03  LN-FEED                 PIC 9(02)     VALUE ZEROES.
           03  LN-WK                   PIC 9(03)     VALUE ZEROES.
           03  PAGE-NUMBER             PIC 9(04)     VALUE ZEROES.

       01  STARTING-TIME               PIC X(08)     VALUE SPACES.

      *=================================================================
       01  HEADER-LINE-01.
           03  H01-DATE           PIC X(08) VALUE SPACES.
           03  FILLER             PIC X(02) VALUE SPACES.
           03  H01-HOUR           PIC 9(02) VALUE ZEROES.
           03  FILLER             PIC X(01) VALUE ":".
           03  H01-MINUTE         PIC 9(02) VALUE ZEROES.
           03  FILLER             PIC X(30) VALUE SPACES.
           03  H01-NAME           PIC X(40) VALUE SPACES.
           03  FILLER             PIC X(37) VALUE SPACES.
           03  FILLER             PIC X(06) VALUE "PAGE: ".
           03  H01-PAGE           PIC 9(04).

      *-----------------------------------------------------------------
       01  HEADER-LINE-02.
           03  FILLER             PIC X(10) VALUE "USER ID:".
           03  H02-USERID         PIC X(10) VALUE SPACES.
           03  H02-TITLE          PIC X(92) VALUE SPACES.
           03  FILLER             PIC X(20) VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEADER-LINE-03.
           03  FILLER PIC X(30) VALUE "YYYYMM BRNO                   ".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(12) VALUE SPACES.

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-WR-YYYYMM                  PIC 9(06).
           03  FILLER                         PIC X(01).
           03  D01-WR-BRNO                    PIC 9(04).

      *-------------------------------------------------------{ RANGES }
       01  RANGE-LINE-01                      REDEFINES DETAIL-LINE.
           03  R01-PROMPT                     PIC X(32).
           03  R01-FIELD                      PIC X(100).
           03  R01-DATE                       REDEFINES R01-FIELD
                                              PIC 99/99/99.
           03  R01-NUMERIC                    REDEFINES R01-FIELD
                                              PIC Z(10)9.
           03  R01-TEXT                       REDEFINES R01-FIELD
                                              PIC X(100).

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
      *COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
      *COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".
      *COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/MKTEMPW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
      *-----------------------------------------------------------------
       COPY "LIBIO/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
      *-----------------------------------------------------------------
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/TIMEWK.CPY".

       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBWI/TSTINTW.CPY".
      *COPY "LIBWI/GRSCANW.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBIO/IOWRIN_DEF.CPY".
       COPY "LIBIO/IOWRIN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOWRIN_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

           PERFORM INITIALIZATION.

           IF ( EXT-EOBUF-ERRCD NOT = SPACES )
              GO TO MAIN-PROGRAM-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      D    STOP FORM-PATHNAME.
           MOVE "B"                  TO WR-PATH-OVERRIDE.

           MOVE BEG-YYYYMM           TO QWR1-WBEG-YYYYMM.
           MOVE BEG-BRNO             TO QWR1-WBEG-BRNO.

           MOVE END-YYYYMM           TO QWR1-WEND-YYYYMM.
           MOVE END-BRNO             TO QWR1-WEND-BRNO.

           PERFORM OPEN-WR1-FILE.
           PERFORM START-WR1-FILE.

      *=================================================================
       MAIN-PROGRAM-NEXT.

           ADD 01 TO RECORDS-READ.

           PERFORM READ-WR1-FILE-NEXT.

           IF ( IO-BAD )
              GO TO MAIN-PROGRAM-END.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( WR-YYYYMM < BEG-YYYYMM )
              GO TO MAIN-PROGRAM-NEXT
           ELSE
           IF ( WR-YYYYMM > END-YYYYMM )
              GO TO MAIN-PROGRAM-END
           ELSE
           IF ( WR-BRNO < BEG-BRNO ) OR
              ( WR-BRNO > END-BRNO )
              GO TO MAIN-PROGRAM-NEXT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE WR-YYYYMM                  TO D01-WR-YYYYMM.
           MOVE WR-BRNO                    TO D01-WR-BRNO.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.

           GO TO MAIN-PROGRAM-NEXT.

      *=================================================================
       MAIN-PROGRAM-END.

           PERFORM WRITE-SUMMARY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    DISPLAY SUMMARY TO SCREEN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PROGRAM COMPLETED!!!" TO SENDMESS-HEAD.
           MOVE SPACES TO SENDMESS-MSG1.
           MOVE SPACES TO SENDMESS-MSG2.
           STRING "RECORDS READ: ", RECORDS-READ
                  DELIMITED BY SIZE INTO SENDMESS-MSG2.
           DISPLAY MESSAGE BOX SENDMESS-HEAD, X"0A"
                               SENDMESS-MSG1, X"0A"
                               SENDMESS-MSG2.

      *-----------------------------------------------------------------
       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
      *
      *    I N I T I A L I Z A T I O N
      *
      ******************************************************************
       INITIALIZATION SECTION.
      D    STOP FORM-PATHNAME.

           MOVE "X"               TO RECORDS-FOUND.
           MOVE FORM-PATHNAME     TO FORM-PATH.
           MOVE "IOWRIN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOWRIN-QUEUE-POS  TO Q-POS.
           MOVE IOWRIN-COPIES-POS TO C-POS.
           MOVE IOWRIN-PRU-POS    TO P-POS.
           MOVE SPACES            TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF EXT-ROUTE-BUF = "00"
              MOVE "PROCESSING..." TO LEGEND
              PERFORM SEND-LEGEND.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE "S  A010ACCEPT." TO VDU
              MOVE SPACES           TO VDUBUF
              PERFORM SCREENIO.

           PERFORM GET-GPENV.
           PERFORM OPEN-PR-FILE.

           PERFORM ENABLE-INTERRUPT.
           PERFORM SEND-PACE-HEAD.

           MOVE ZEROES TO PACE-LEV.
           MOVE ZEROES TO RECORDS-READ.

           PERFORM GET-TIME.
           MOVE "N"       TO RECORDS-FOUND.
           MOVE TIME-EDIT TO STARTING-TIME.

           MOVE "R" TO TIMER-TYPE.
           PERFORM TIMER-START.
           MOVE TIMER-LINK-BUF TO P-TIMER-LINK-BUF.

      *=================================================================
       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
      *
      *    E N T E R - P A R A M E T E R S
      *
      ******************************************************************
       ENTER-PARAMETERS SECTION.

           IF ( UP-KEY )
              GO TO ENTER-PROCESS-KEY.

      *=================================================================
       ENTER-BEG-YYYYMM.

           MOVE LEGEND-BEG-YYYYMM TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN060BEG-YYYYMM." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-YYYYMM.
      *KC  IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-YYYYMM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-YYYYMM.

      *-----------------------------------------------------------------
       ENTER-END-YYYYMM.

           MOVE LEGEND-END-YYYYMM TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN060END-YYYYMM." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-YYYYMM.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-YYYYMM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-YYYYMM.

           IF ( BEG-YYYYMM > END-YYYYMM )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-YYYYMM.

           GO TO ENTER-BEG-BRNO.

      *-----------------------------------------------------------------
       ENTER-ALL-YYYYMM.

           MOVE "SNNN060BEG-YYYYMM."   TO VDU.
           MOVE ZEROES                 TO BEG-YYYYMM VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060END-YYYYMM."   TO VDU.
           MOVE ALL "9"                TO END-YYYYMM VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-BRNO.

           MOVE LEGEND-BEG-BRNO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040BEG-BRNO." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-END-YYYYMM.
           IF (        F3-KEY ) GO TO ENTER-ALL-BRNO.
      *KC  IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-BRNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-BRNO.

      *-----------------------------------------------------------------
       ENTER-END-BRNO.

           MOVE LEGEND-END-BRNO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040END-BRNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-BRNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-BRNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-BRNO.

           IF ( BEG-BRNO > END-BRNO )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRNO.

           GO TO ENTER-PROCESS-KEY.

      *-----------------------------------------------------------------
       ENTER-ALL-BRNO.

           MOVE "SNNN040BEG-BRNO."   TO VDU.
           MOVE ZEROES               TO BEG-BRNO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040END-BRNO."   TO VDU.
           MOVE ALL "9"              TO END-BRNO VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-PROCESS-KEY.

           MOVE LEGEND-PROCESS-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN010PROCESS-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-BRNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUNUM0 < ZEROES ) OR
              ( VDUNUM0 > 1      )    
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PROCESS-KEY.

           MOVE VDUNUM0 TO PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE LEGEND-BEG-END-RANGE TO LEGEND.
           PERFORM SEND-LEGEND.

      *=================================================================
       ENTER-ACCEPT-YN.

           MOVE LEGEND-ACCEPT-YN TO LEGEND.
           PERFORM SEND-LEGEND.

      *=================================================================
       ENTER-PARAMETERS-EXIT.
           EXIT.

      *****************************************************************
      *
      *    D I S P L A Y - P A R A M E T E R S
      *
      *****************************************************************
       DISPLAY-PARAMETERS SECTION.

           MOVE "SNNN060BEG-YYYYMM."   TO VDU.
           MOVE BEG-YYYYMM             TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SNNN040END-YYYYMM."   TO VDU.
           MOVE END-YYYYMM             TO VDUBUF.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN040BEG-BRNO."     TO VDU.
           MOVE BEG-BRNO               TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SNNN040END-BRNO."     TO VDU.
           MOVE END-BRNO               TO VDUBUF.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN010PROCESS-KEY." TO VDU
           MOVE PROCESS-KEY           TO VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       DISPLAY-PARAMETERS-EXIT.
           EXIT.

      *****************************************************************
      *
      *    E O - E X E C - A U D I T
      *
      *****************************************************************
       EO-EXEC-AUDIT SECTION.

      *    MOVE EXT-EOBUF-DATE1 TO ???.
      *    MOVE EXT-EOBUF-DATE2 TO ???.

       EO-EXEC-AUDIT-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P A C E - R O U T I N E
      *
      *=================================================================
      * DESC: DISPLAY THE PACE OF THE EXECUTION OF THE PROGRAM.
      ******************************************************************
       PACE-ROUTINE SECTION.

           IF ( EXT-ROUTE-BUF NOT = "00" )
              GO TO PACE-ROUTINE-EXIT.

           ADD 01 TO PACE-COUNTER.

           IF ( PACE-COUNTER <= PACE-COUNTER-MAX )
              GO TO PACE-ROUTINE-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           ADD 01 TO PACE-LEV.

           IF ( PACE-LEV <= PACE-MAX )
              GO TO PACE-ROUTINE-UPDATE.

           PERFORM VARYING SUB FROM 2 BY 1 UNTIL SUB > PACE-MAX
              MOVE PACE-ELE(SUB) TO PACE-ELE(SUB - 1)
           END-PERFORM.

           MOVE PACE-MAX TO PACE-LEV.
           MOVE SPACES   TO PACE-ELE(PACE-LEV).

      *-----------------------------------------------------------------
       PACE-ROUTINE-UPDATE.

           MOVE SPACES TO PACE-DETAIL.

           PERFORM GET-TIME.
           MOVE CUR-HH    TO PACE-TIME-HH.
           MOVE CUR-MM    TO PACE-TIME-MM.
           MOVE CUR-SS    TO PACE-TIME-SS.
           MOVE CUR-HD    TO PACE-TIME-HD.
           MOVE PACE-TIME TO PACE-DETAIL-TIME.

           MOVE WR1-KEY     TO PACE-DETAIL-KEY.

           MOVE PACE-DETAIL TO PACE-ELE(PACE-LEV).
           MOVE PACE-BUF    TO WR-BUF.
           MOVE PACE-LIST   TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE ZEROES TO PACE-COUNTER.

      *=================================================================
       PACE-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - S U M M A R Y
      *
      ******************************************************************
       WRITE-SUMMARY SECTION.

           IF ( NO-RECORDS-FOUND )
              GO TO WRITE-SUMMARY-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 99     TO LN-COUNT.
           MOVE 1      TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING YYYYMM              : "    TO R01-PROMPT.
           MOVE BEG-YYYYMM                            TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    YYYYMM              : "    TO R01-PROMPT.
           MOVE END-YYYYMM                            TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING BRANCH NO.          : "    TO R01-PROMPT.
           MOVE BEG-BRNO                              TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    BRANCH NO.          : "    TO R01-PROMPT.
           MOVE END-BRNO                              TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PROCESS KEY                   : "    TO R01-PROMPT.
           MOVE PROCESS-KEY                           TO R01-NUMERIC.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE "BEGINNING ACCTNO              : "    TO R01-PROMPT.
      *    MOVE BEG-ACCTNO                            TO R01-NUMERIC.
      *    MOVE 1                                     TO LN-FEED.
      *    PERFORM WRITE-DETAIL-LINE.

      *    MOVE "ENDING    ACCTNO              : "    TO R01-PROMPT.
      *    MOVE END-ACCTNO                            TO R01-NUMERIC.
      *    MOVE 1                                     TO LN-FEED.
      *    PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "STARTING TIME                 : "    TO R01-PROMPT.
           MOVE STARTING-TIME                         TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING   TIME                 : "    TO R01-PROMPT.
           PERFORM GET-TIME.
           MOVE TIME-EDIT                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *=================================================================
       WRITE-SUMMARY-EXIT.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - D E T A I L - L I N E
      *
      ******************************************************************
       WRITE-DETAIL-LINE SECTION.

           ADD LN-FEED TO LN-COUNT.

           IF ( LN-COUNT > EXT-CONAME-RPT-LINES )
              PERFORM WRITE-PAGE-HEADER.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.

           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *=================================================================
       WRITE-DETAIL-LINE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - P A G E - H E A D E R
      *
      ******************************************************************
       WRITE-PAGE-HEADER SECTION.

           MOVE 01 TO LN-COUNT.
           ADD  01 TO PAGE-NUMBER.
           PERFORM GET-TIME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{01}
           MOVE EXT-CONAME-SYSDATE               TO H01-DATE.
           MOVE T-HH                             TO H01-HOUR.
           MOVE T-MM                             TO H01-MINUTE.
           MOVE EXT-CONAME-CONAME                TO H01-NAME.
           CALL "C$JUSTIFY"                   USING H01-NAME "C".
           MOVE PAGE-NUMBER                      TO H01-PAGE.

           MOVE HEADER-LINE-01                   TO PR-REC.
           MOVE 99                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{02}
           MOVE EXT-CONAME-USER-ID               TO H02-USERID.
           MOVE "SQL TABLE IO UTILITY - WRFILE"  TO H02-TITLE.
           CALL "C$JUSTIFY"                   USING H02-TITLE "C".

           MOVE HEADER-LINE-02                   TO PR-REC.
           ADD  01                               TO LN-COUNT.
           MOVE 01                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{03}
           MOVE HEADER-LINE-03                   TO PR-REC.
           ADD  01                               TO LN-COUNT.
           MOVE 01                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{04}
           MOVE ALL "="                          TO PR-REC.
           ADD  01                               TO LN-COUNT.
           MOVE 01                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 01 TO LN-FEED.

      *=================================================================
       WRITE-PAGE-HEADER-EXIT.
           EXIT.

      ******************************************************************
      *
      *    F O R M - R E S E T
      *
      ******************************************************************
       FORM-RESET SECTION.

           MOVE EXT-CONAME-CONAME TO IOWRIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOWRIN-SYSDATE.
           DISPLAY IOWRIN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOWRIN-FORM-FIELDS TO WR-BUF.
           MOVE IOWRIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *=================================================================
       FORM-RESET-EXIT.
           EXIT.

      ******************************************************************
      *
      *    S E T U P - L I N K - I N F O
      *
      ******************************************************************
       SETUP-LINK-INFO SECTION.

           MOVE IOWRIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOWRIN-HELP--DIR  TO HELP-LINK-DIR.

      *=================================================================
       SETUP-LINK-INFO-EXIT.
           EXIT.

      ******************************************************************
      *
      *    S E T U P - L I N K - I N F O
      *
      ******************************************************************
       VDU-CONVERT-FIELD SECTION.

           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBIO/IOWRIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *=================================================================
       VDU-CONVERT-FIELD-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
      *COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/DATER.CPY".
      *COPY "LIBGB/EOSPOOL.CPY".
       COPY "LIBGB/GPENV.CPY".
      *COPY "LIBGB/GRSERCH.CPY".
      *COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

       COPY "LIBWI/PFSWINDOWS.CPY".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - P A R A G R A P H S
      *
      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
      *=================================================================
       SEND-LEGEND.

           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       SEND-PACE-HEAD.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE PACE-COUNTER-MAX    TO PACE-HEAD-MAX
              MOVE "S  A400PACE-HEAD." TO VDU
              MOVE PACE-HEAD-LINE      TO VDUBUF
              PERFORM SCREENIO.

      ******************************************************************
      *
      *    I O - R O U T I N E S
      *
      ******************************************************************
       IO-ROUTINE SECTION.
       COPY "LIBIO/CONNECT_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".

      *-----------------------------------------------------------------
      *    LOAD WR-REC FROM QWR-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-WR-FIELDS.
           MOVE QWR-YYYYMM TO WR-YYYYMM.
           MOVE QWR-BRNO TO WR-BRNO.

           IF ( QWR-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO WR-LTOUCH-DATE
           ELSE
              MOVE QWR-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO WR-LTOUCH-DATE.

           MOVE QWR-CUR-NO-CUST TO WR-CUR-NO-CUST.
           MOVE QWR-ELM-NO-CUST TO WR-ELM-NO-CUST.
           MOVE QWR-ELY-NO-CUST TO WR-ELY-NO-CUST.
           MOVE QWR-OUTS-BALANCE TO WR-OUTS-BALANCE.
           MOVE QWR-ELM-OUTS-BALANCE TO WR-ELM-OUTS-BALANCE.
           MOVE QWR-ELY-OUTS-BALANCE TO WR-ELY-OUTS-BALANCE.
           MOVE QWR-NO-NB-1 TO WR-NO-NB(1).
           MOVE QWR-NO-FB-1 TO WR-NO-FB(1).
           MOVE QWR-NO-PB-1 TO WR-NO-PB(1).
           MOVE QWR-GROSS-VOLUME-1 TO WR-GROSS-VOLUME(1).
           MOVE QWR-BSR-BONUS-1 TO WR-BSR-BONUS(1).
           MOVE QWR-INTEREST-ALLOCATION-1 TO WR-INTEREST-ALLOCATION(1).
           MOVE QWR-OLD-SUPERVISION-1 TO WR-OLD-SUPERVISION(1).
           MOVE QWR-GEN-RESERVE-ADJ-1 TO WR-GEN-RESERVE-ADJ(1).
           MOVE QWR-INTANGIBLE-AMORT-1 TO WR-INTANGIBLE-AMORT(1).
           MOVE QWR-ROBBERIES-1 TO WR-ROBBERIES(1).
           MOVE QWR-RATING-SHEET-INCOME-1 TO WR-RATING-SHEET-INCOME(1).
           MOVE QWR-INT-FEE-INCOME-1 TO WR-INT-FEE-INCOME(1).
           MOVE QWR-INSURANCE-INCOME-1 TO WR-INSURANCE-INCOME(1).
           MOVE QWR-RATING-SHEET-EXPENSES-1
              TO WR-RATING-SHEET-EXPENSES(1).
           MOVE QWR-PERSONNEL-EXPENSE-1 TO WR-PERSONNEL-EXPENSE(1).
           MOVE QWR-CHARGEOFF-EXPENSE-1 TO WR-CHARGEOFF-EXPENSE(1).
           MOVE QWR-NO-CHARGEOFFS-1 TO WR-NO-CHARGEOFFS(1).
           MOVE QWR-RECOVERY-INCOME-1 TO WR-RECOVERY-INCOME(1).
           MOVE QWR-BAD-DEBT-INCREASE-1 TO WR-BAD-DEBT-INCREASE(1).
           MOVE QWR-NO-NB-2 TO WR-NO-NB(2).
           MOVE QWR-NO-FB-2 TO WR-NO-FB(2).
           MOVE QWR-NO-PB-2 TO WR-NO-PB(2).
           MOVE QWR-GROSS-VOLUME-2 TO WR-GROSS-VOLUME(2).
           MOVE QWR-BSR-BONUS-2 TO WR-BSR-BONUS(2).
           MOVE QWR-INTEREST-ALLOCATION-2 TO WR-INTEREST-ALLOCATION(2).
           MOVE QWR-OLD-SUPERVISION-2 TO WR-OLD-SUPERVISION(2).
           MOVE QWR-GEN-RESERVE-ADJ-2 TO WR-GEN-RESERVE-ADJ(2).
           MOVE QWR-INTANGIBLE-AMORT-2 TO WR-INTANGIBLE-AMORT(2).
           MOVE QWR-ROBBERIES-2 TO WR-ROBBERIES(2).
           MOVE QWR-RATING-SHEET-INCOME-2 TO WR-RATING-SHEET-INCOME(2).
           MOVE QWR-INT-FEE-INCOME-2 TO WR-INT-FEE-INCOME(2).
           MOVE QWR-INSURANCE-INCOME-2 TO WR-INSURANCE-INCOME(2).
           MOVE QWR-RATING-SHEET-EXPENSES-2
              TO WR-RATING-SHEET-EXPENSES(2).
           MOVE QWR-PERSONNEL-EXPENSE-2 TO WR-PERSONNEL-EXPENSE(2).
           MOVE QWR-CHARGEOFF-EXPENSE-2 TO WR-CHARGEOFF-EXPENSE(2).
           MOVE QWR-NO-CHARGEOFFS-2 TO WR-NO-CHARGEOFFS(2).
           MOVE QWR-RECOVERY-INCOME-2 TO WR-RECOVERY-INCOME(2).
           MOVE QWR-BAD-DEBT-INCREASE-2 TO WR-BAD-DEBT-INCREASE(2).
           MOVE QWR-NO-30-DAY-ACCTS TO WR-NO-30-DAY-ACCTS.
           MOVE QWR-AMT-30-DAY-ACCTS TO WR-AMT-30-DAY-ACCTS.
           MOVE QWR-NO-60-DAY-ACCTS TO WR-NO-60-DAY-ACCTS.
           MOVE QWR-AMT-60-DAY-ACCTS TO WR-AMT-60-DAY-ACCTS.
           MOVE QWR-NO-90-DAY-ACCTS TO WR-NO-90-DAY-ACCTS.
           MOVE QWR-AMT-90-DAY-ACCTS TO WR-AMT-90-DAY-ACCTS.
           MOVE QWR-ELM-AMT-90-DAY-ACCTS TO WR-ELM-AMT-90-DAY-ACCTS.
           MOVE QWR-ELY-AMT-90-DAY-ACCTS TO WR-ELY-AMT-90-DAY-ACCTS.
           MOVE QWR-YTD-PCT-TURNOVER TO WR-YTD-PCT-TURNOVER.
           MOVE QWR-YTD-PCT-RATE-SHT-PROFIT
              TO WR-YTD-PCT-RATE-SHT-PROFIT.
           MOVE QWR-YTD-PCT-RS-INC-LEDGER TO WR-YTD-PCT-RS-INC-LEDGER.
           MOVE QWR-YTD-PCT-INT-FEE-LEDGER
              TO WR-YTD-PCT-INT-FEE-LEDGER.
           MOVE QWR-YTD-PCT-INS-INC-LEDGER
              TO WR-YTD-PCT-INS-INC-LEDGER.
           MOVE QWR-YTD-PCT-EXPENSE-LEDGER
              TO WR-YTD-PCT-EXPENSE-LEDGER.
           MOVE QWR-YTD-PCT-BAD-DEBT-LEDGER
              TO WR-YTD-PCT-BAD-DEBT-LEDGER.
           MOVE QWR-YTD-PCT-NF-BNK-LEDGER TO WR-YTD-PCT-NF-BNK-LEDGER.
           MOVE QWR-YTD-PCT-PRETAX-LEDGER TO WR-YTD-PCT-PRETAX-LEDGER.
           MOVE QWR-YTD-PCT-DEBT-NF-LEDGER
              TO WR-YTD-PCT-DEBT-NF-LEDGER.
           MOVE QWR-WCBC-CUR-NO-CUST TO WR-WCBC-CUR-NO-CUST.
           MOVE QWR-WCBC-ELM-NO-CUST TO WR-WCBC-ELM-NO-CUST.
           MOVE QWR-WCBC-ELY-NO-CUST TO WR-WCBC-ELY-NO-CUST.
           MOVE QWR-WCBC-OUTS-BALANCE TO WR-WCBC-OUTS-BALANCE.
           MOVE QWR-WCBC-ELM-OUTS-BAL TO WR-WCBC-ELM-OUTS-BAL.
           MOVE QWR-WCBC-ELY-OUTS-BAL TO WR-WCBC-ELY-OUTS-BAL.
           MOVE QWR-WCBC-NO-NB-1 TO WR-WCBC-NO-NB(1).
           MOVE QWR-WCBC-NO-FB-1 TO WR-WCBC-NO-FB(1).
           MOVE QWR-WCBC-NO-PB-1 TO WR-WCBC-NO-PB(1).
           MOVE QWR-WCBC-GROSS-VOLUME-1 TO WR-WCBC-GROSS-VOLUME(1).
           MOVE QWR-WCBC-LATE-CHGS-1 TO WR-WCBC-LATE-CHGS(1).
           MOVE QWR-WCBC-NET-FIN-CHGS-1 TO WR-WCBC-NET-FIN-CHGS(1).
           MOVE QWR-WCBC-SALES-1 TO WR-WCBC-SALES(1).
           MOVE QWR-WCBC-COST-OF-SALES-1 TO WR-WCBC-COST-OF-SALES(1).
           MOVE QWR-WCBC-EXPENSES-1 TO WR-WCBC-EXPENSES(1).
           MOVE QWR-WCBC-RECOVERY-INCOME-1
              TO WR-WCBC-RECOVERY-INCOME(1).
           MOVE QWR-WCBC-CHARGEOFF-EXPENSE-1
              TO WR-WCBC-CHARGEOFF-EXPENSE(1).
           MOVE QWR-WCBC-BAD-DEBT-BNKRPCY-1
              TO WR-WCBC-BAD-DEBT-BNKRPCY(1).
           MOVE QWR-WCBC-NO-NB-2 TO WR-WCBC-NO-NB(2).
           MOVE QWR-WCBC-NO-FB-2 TO WR-WCBC-NO-FB(2).
           MOVE QWR-WCBC-NO-PB-2 TO WR-WCBC-NO-PB(2).
           MOVE QWR-WCBC-GROSS-VOLUME-2 TO WR-WCBC-GROSS-VOLUME(2).
           MOVE QWR-WCBC-LATE-CHGS-2 TO WR-WCBC-LATE-CHGS(2).
           MOVE QWR-WCBC-NET-FIN-CHGS-2 TO WR-WCBC-NET-FIN-CHGS(2).
           MOVE QWR-WCBC-SALES-2 TO WR-WCBC-SALES(2).
           MOVE QWR-WCBC-COST-OF-SALES-2 TO WR-WCBC-COST-OF-SALES(2).
           MOVE QWR-WCBC-EXPENSES-2 TO WR-WCBC-EXPENSES(2).
           MOVE QWR-WCBC-RECOVERY-INCOME-2
              TO WR-WCBC-RECOVERY-INCOME(2).
           MOVE QWR-WCBC-CHARGEOFF-EXPENSE-2
              TO WR-WCBC-CHARGEOFF-EXPENSE(2).
           MOVE QWR-WCBC-BAD-DEBT-BNKRPCY-2
              TO WR-WCBC-BAD-DEBT-BNKRPCY(2).
           MOVE QWR-WCBC-NO-30-DAY-ACCTS TO WR-WCBC-NO-30-DAY-ACCTS.
           MOVE QWR-WCBC-AMT-30-DAY-ACCTS TO WR-WCBC-AMT-30-DAY-ACCTS.
           MOVE QWR-WCBC-NO-60-DAY-ACCTS TO WR-WCBC-NO-60-DAY-ACCTS.
           MOVE QWR-WCBC-AMT-60-DAY-ACCTS TO WR-WCBC-AMT-60-DAY-ACCTS.
           MOVE QWR-WCBC-NO-90-DAY-ACCTS TO WR-WCBC-NO-90-DAY-ACCTS.
           MOVE QWR-WCBC-AMT-90-DAY-ACCTS TO WR-WCBC-AMT-90-DAY-ACCTS.
           MOVE QWR-WCBC-ELM-AMT-90-DAYS TO WR-WCBC-ELM-AMT-90-DAYS.
           MOVE QWR-WCBC-ELY-AMT-90-DAYS TO WR-WCBC-ELY-AMT-90-DAYS.
           MOVE QWR-WCBC-PCT-VOLUME TO WR-WCBC-PCT-VOLUME.
           MOVE QWR-WCBC-GROSS-PROF-PCT TO WR-WCBC-GROSS-PROF-PCT.
           MOVE QWR-LARGE-CUR-NO-CUST TO WR-LARGE-CUR-NO-CUST.
           MOVE QWR-LARGE-ELM-NO-CUST TO WR-LARGE-ELM-NO-CUST.
           MOVE QWR-LARGE-ELY-NO-CUST TO WR-LARGE-ELY-NO-CUST.
           MOVE QWR-LARGE-OUTS-BALANCE TO WR-LARGE-OUTS-BALANCE.
           MOVE QWR-LARGE-ELM-OUTS-BAL TO WR-LARGE-ELM-OUTS-BAL.
           MOVE QWR-LARGE-ELY-OUTS-BAL TO WR-LARGE-ELY-OUTS-BAL.
           MOVE QWR-LARGE-NO-NB-1 TO WR-LARGE-NO-NB(1).
           MOVE QWR-LARGE-NO-FB-1 TO WR-LARGE-NO-FB(1).
           MOVE QWR-LARGE-NO-PB-1 TO WR-LARGE-NO-PB(1).
           MOVE QWR-LARGE-GROSS-VOLUME-1 TO WR-LARGE-GROSS-VOLUME(1).
           MOVE QWR-LARGE-NO-NB-2 TO WR-LARGE-NO-NB(2).
           MOVE QWR-LARGE-NO-FB-2 TO WR-LARGE-NO-FB(2).
           MOVE QWR-LARGE-NO-PB-2 TO WR-LARGE-NO-PB(2).
           MOVE QWR-LARGE-GROSS-VOLUME-2 TO WR-LARGE-GROSS-VOLUME(2).
           MOVE QWR-LARGE-NO-30-DAY-ACCTS TO WR-LARGE-NO-30-DAY-ACCTS.
           MOVE QWR-LARGE-AMT-30-DAY-ACCTS
              TO WR-LARGE-AMT-30-DAY-ACCTS.
           MOVE QWR-LARGE-NO-60-DAY-ACCTS TO WR-LARGE-NO-60-DAY-ACCTS.
           MOVE QWR-LARGE-AMT-60-DAY-ACCTS
              TO WR-LARGE-AMT-60-DAY-ACCTS.
           MOVE QWR-LARGE-NO-90-DAY-ACCTS TO WR-LARGE-NO-90-DAY-ACCTS.
           MOVE QWR-LARGE-AMT-90-DAY-ACCTS
              TO WR-LARGE-AMT-90-DAY-ACCTS.
           MOVE QWR-LARGE-ELM-AMT-90-DAYS TO WR-LARGE-ELM-AMT-90-DAYS.
           MOVE QWR-LARGE-ELY-AMT-90-DAYS TO WR-LARGE-ELY-AMT-90-DAYS.
           MOVE QWR-LIFE-CLAIMS-1 TO WR-LIFE-CLAIMS(1).
           MOVE QWR-AH-CLAIMS-1 TO WR-AH-CLAIMS(1).
           MOVE QWR-FIRE-CLAIMS-1 TO WR-FIRE-CLAIMS(1).
           MOVE QWR-AUTO-CLAIMS-1 TO WR-AUTO-CLAIMS(1).
           MOVE QWR-UNEMP-CLAIMS-1 TO WR-UNEMP-CLAIMS(1).
           MOVE QWR-AD-D-CLAIMS-1 TO WR-AD-D-CLAIMS(1).
           MOVE QWR-L-PROT-CLAIMS-1 TO WR-L-PROT-CLAIMS(1).
           MOVE QWR-NF-NON-BK-CLAIMS-1 TO WR-NF-NON-BK-CLAIMS(1).
           MOVE QWR-NF-BK-CLAIMS-1 TO WR-NF-BK-CLAIMS(1).
           MOVE QWR-ROAD-GARD-CLAIMS-1 TO WR-ROAD-GARD-CLAIMS(1).
           MOVE QWR-NO-LIFE-CLAIMS-1 TO WR-NO-LIFE-CLAIMS(1).
           MOVE QWR-NO-AH-CLAIMS-1 TO WR-NO-AH-CLAIMS(1).
           MOVE QWR-NO-FIRE-CLAIMS-1 TO WR-NO-FIRE-CLAIMS(1).
           MOVE QWR-NO-AUTO-CLAIMS-1 TO WR-NO-AUTO-CLAIMS(1).
           MOVE QWR-NO-UNEMP-CLAIMS-1 TO WR-NO-UNEMP-CLAIMS(1).
           MOVE QWR-NO-AD-D-CLAIMS-1 TO WR-NO-AD-D-CLAIMS(1).
           MOVE QWR-NO-L-PROT-CLAIMS-1 TO WR-NO-L-PROT-CLAIMS(1).
           MOVE QWR-NO-NF-NON-BK-CLAIMS-1 TO WR-NO-NF-NON-BK-CLAIMS(1).
           MOVE QWR-NO-NF-BK-CLAIMS-1 TO WR-NO-NF-BK-CLAIMS(1).
           MOVE QWR-NO-ROAD-GARD-CLAIMS-1 TO WR-NO-ROAD-GARD-CLAIMS(1).
           MOVE QWR-LIFE-PREMS-1 TO WR-LIFE-PREMS(1).
           MOVE QWR-AH-PREMS-1 TO WR-AH-PREMS(1).
           MOVE QWR-FIRE-PREMS-1 TO WR-FIRE-PREMS(1).
           MOVE QWR-AUTO-PREMS-1 TO WR-AUTO-PREMS(1).
           MOVE QWR-UNEMP-PREMS-1 TO WR-UNEMP-PREMS(1).
           MOVE QWR-AD-D-PREMS-1 TO WR-AD-D-PREMS(1).
           MOVE QWR-L-PROT-PREMS-1 TO WR-L-PROT-PREMS(1).
           MOVE QWR-NON-FILE-PREMS-1 TO WR-NON-FILE-PREMS(1).
           MOVE QWR-ROAD-GARD-PREMS-1 TO WR-ROAD-GARD-PREMS(1).
           MOVE QWR-NO-LIFE-PREMS-1 TO WR-NO-LIFE-PREMS(1).
           MOVE QWR-NO-AH-PREMS-1 TO WR-NO-AH-PREMS(1).
           MOVE QWR-NO-FIRE-PREMS-1 TO WR-NO-FIRE-PREMS(1).
           MOVE QWR-NO-AUTO-PREMS-1 TO WR-NO-AUTO-PREMS(1).
           MOVE QWR-NO-UNEMP-PREMS-1 TO WR-NO-UNEMP-PREMS(1).
           MOVE QWR-NO-AD-D-PREMS-1 TO WR-NO-AD-D-PREMS(1).
           MOVE QWR-NO-L-PROT-PREMS-1 TO WR-NO-L-PROT-PREMS(1).
           MOVE QWR-NO-NON-FILE-PREMS-1 TO WR-NO-NON-FILE-PREMS(1).
           MOVE QWR-NO-ROAD-GARD-PREMS-1 TO WR-NO-ROAD-GARD-PREMS(1).
           MOVE QWR-LIFE-REFS-1 TO WR-LIFE-REFS(1).
           MOVE QWR-AH-REFS-1 TO WR-AH-REFS(1).
           MOVE QWR-FIRE-REFS-1 TO WR-FIRE-REFS(1).
           MOVE QWR-AUTO-REFS-1 TO WR-AUTO-REFS(1).
           MOVE QWR-UNEMP-REFS-1 TO WR-UNEMP-REFS(1).
           MOVE QWR-AD-D-REFS-1 TO WR-AD-D-REFS(1).
           MOVE QWR-L-PROT-REFS-1 TO WR-L-PROT-REFS(1).
           MOVE QWR-NON-FILE-REFS-1 TO WR-NON-FILE-REFS(1).
           MOVE QWR-ROAD-GARD-REFS-1 TO WR-ROAD-GARD-REFS(1).
           MOVE QWR-NF-RECOVERIES-1 TO WR-NF-RECOVERIES(1).
           MOVE QWR-LIFE-CLAIMS-2 TO WR-LIFE-CLAIMS(2).
           MOVE QWR-AH-CLAIMS-2 TO WR-AH-CLAIMS(2).
           MOVE QWR-FIRE-CLAIMS-2 TO WR-FIRE-CLAIMS(2).
           MOVE QWR-AUTO-CLAIMS-2 TO WR-AUTO-CLAIMS(2).
           MOVE QWR-UNEMP-CLAIMS-2 TO WR-UNEMP-CLAIMS(2).
           MOVE QWR-AD-D-CLAIMS-2 TO WR-AD-D-CLAIMS(2).
           MOVE QWR-L-PROT-CLAIMS-2 TO WR-L-PROT-CLAIMS(2).
           MOVE QWR-NF-NON-BK-CLAIMS-2 TO WR-NF-NON-BK-CLAIMS(2).
           MOVE QWR-NF-BK-CLAIMS-2 TO WR-NF-BK-CLAIMS(2).
           MOVE QWR-ROAD-GARD-CLAIMS-2 TO WR-ROAD-GARD-CLAIMS(2).
           MOVE QWR-NO-LIFE-CLAIMS-2 TO WR-NO-LIFE-CLAIMS(2).
           MOVE QWR-NO-AH-CLAIMS-2 TO WR-NO-AH-CLAIMS(2).
           MOVE QWR-NO-FIRE-CLAIMS-2 TO WR-NO-FIRE-CLAIMS(2).
           MOVE QWR-NO-AUTO-CLAIMS-2 TO WR-NO-AUTO-CLAIMS(2).
           MOVE QWR-NO-UNEMP-CLAIMS-2 TO WR-NO-UNEMP-CLAIMS(2).
           MOVE QWR-NO-AD-D-CLAIMS-2 TO WR-NO-AD-D-CLAIMS(2).
           MOVE QWR-NO-L-PROT-CLAIMS-2 TO WR-NO-L-PROT-CLAIMS(2).
           MOVE QWR-NO-NF-NON-BK-CLAIMS-2 TO WR-NO-NF-NON-BK-CLAIMS(2).
           MOVE QWR-NO-NF-BK-CLAIMS-2 TO WR-NO-NF-BK-CLAIMS(2).
           MOVE QWR-NO-ROAD-GARD-CLAIMS-2 TO WR-NO-ROAD-GARD-CLAIMS(2).
           MOVE QWR-LIFE-PREMS-2 TO WR-LIFE-PREMS(2).
           MOVE QWR-AH-PREMS-2 TO WR-AH-PREMS(2).
           MOVE QWR-FIRE-PREMS-2 TO WR-FIRE-PREMS(2).
           MOVE QWR-AUTO-PREMS-2 TO WR-AUTO-PREMS(2).
           MOVE QWR-UNEMP-PREMS-2 TO WR-UNEMP-PREMS(2).
           MOVE QWR-AD-D-PREMS-2 TO WR-AD-D-PREMS(2).
           MOVE QWR-L-PROT-PREMS-2 TO WR-L-PROT-PREMS(2).
           MOVE QWR-NON-FILE-PREMS-2 TO WR-NON-FILE-PREMS(2).
           MOVE QWR-ROAD-GARD-PREMS-2 TO WR-ROAD-GARD-PREMS(2).
           MOVE QWR-NO-LIFE-PREMS-2 TO WR-NO-LIFE-PREMS(2).
           MOVE QWR-NO-AH-PREMS-2 TO WR-NO-AH-PREMS(2).
           MOVE QWR-NO-FIRE-PREMS-2 TO WR-NO-FIRE-PREMS(2).
           MOVE QWR-NO-AUTO-PREMS-2 TO WR-NO-AUTO-PREMS(2).
           MOVE QWR-NO-UNEMP-PREMS-2 TO WR-NO-UNEMP-PREMS(2).
           MOVE QWR-NO-AD-D-PREMS-2 TO WR-NO-AD-D-PREMS(2).
           MOVE QWR-NO-L-PROT-PREMS-2 TO WR-NO-L-PROT-PREMS(2).
           MOVE QWR-NO-NON-FILE-PREMS-2 TO WR-NO-NON-FILE-PREMS(2).
           MOVE QWR-NO-ROAD-GARD-PREMS-2 TO WR-NO-ROAD-GARD-PREMS(2).
           MOVE QWR-LIFE-REFS-2 TO WR-LIFE-REFS(2).
           MOVE QWR-AH-REFS-2 TO WR-AH-REFS(2).
           MOVE QWR-FIRE-REFS-2 TO WR-FIRE-REFS(2).
           MOVE QWR-AUTO-REFS-2 TO WR-AUTO-REFS(2).
           MOVE QWR-UNEMP-REFS-2 TO WR-UNEMP-REFS(2).
           MOVE QWR-AD-D-REFS-2 TO WR-AD-D-REFS(2).
           MOVE QWR-L-PROT-REFS-2 TO WR-L-PROT-REFS(2).
           MOVE QWR-NON-FILE-REFS-2 TO WR-NON-FILE-REFS(2).
           MOVE QWR-ROAD-GARD-REFS-2 TO WR-ROAD-GARD-REFS(2).
           MOVE QWR-NF-RECOVERIES-2 TO WR-NF-RECOVERIES(2).
           MOVE QWR-NONFILE-ENDING-BALANCE
              TO WR-NONFILE-ENDING-BALANCE.
           MOVE QWR-NO-CUST-1-YR-AGO TO WR-NO-CUST-1-YR-AGO.
           MOVE QWR-OUTS-BAL-1-YR-AGO TO WR-OUTS-BAL-1-YR-AGO.
           MOVE QWR-WCBC-NO-CUST-1-YR-AGO TO WR-WCBC-NO-CUST-1-YR-AGO.
           MOVE QWR-WCBC-OUTS-BAL-1-YR-AGO
              TO WR-WCBC-OUTS-BAL-1-YR-AGO.
           MOVE QWR-LARGE-NO-CUST-1-YR-AGO
              TO WR-LARGE-NO-CUST-1-YR-AGO.
           MOVE QWR-LARGE-OUTS-BAL-1-YR-AGO
              TO WR-LARGE-OUTS-BAL-1-YR-AGO.
           MOVE QWR-CUR-NO-CONT-SLOW TO WR-CUR-NO-CONT-SLOW.
           MOVE QWR-WCBC-CUR-NO-CONT-SLOW TO WR-WCBC-CUR-NO-CONT-SLOW.
           MOVE QWR-LARGE-CUR-NO-CONT-SLOW
              TO WR-LARGE-CUR-NO-CONT-SLOW.
           MOVE QWR-CASHIER-BONUS TO WR-CASHIER-BONUS.
           MOVE QWR-ASST-MGR-BONUS TO WR-ASST-MGR-BONUS.
           MOVE QWR-MGR-BONUS TO WR-MGR-BONUS.
           MOVE QWR-LOTS-NF-CLAIMS-1 TO WR-LOTS-NF-CLAIMS(1).
           MOVE QWR-LOTS-NO-NF-CLAIMS-1 TO WR-LOTS-NO-NF-CLAIMS(1).
           MOVE QWR-LOTS-NON-FILE-PREMS-1 TO WR-LOTS-NON-FILE-PREMS(1).
           MOVE QWR-LOTS-NO-NON-FILE-PREMS-1
              TO WR-LOTS-NO-NON-FILE-PREMS(1).
           MOVE QWR-LOTS-NF-RECOVERIES-1 TO WR-LOTS-NF-RECOVERIES(1).
           MOVE QWR-LOTS-NF-CLAIMS-2 TO WR-LOTS-NF-CLAIMS(2).
           MOVE QWR-LOTS-NO-NF-CLAIMS-2 TO WR-LOTS-NO-NF-CLAIMS(2).
           MOVE QWR-LOTS-NON-FILE-PREMS-2 TO WR-LOTS-NON-FILE-PREMS(2).
           MOVE QWR-LOTS-NO-NON-FILE-PREMS-2
              TO WR-LOTS-NO-NON-FILE-PREMS(2).
           MOVE QWR-LOTS-NF-RECOVERIES-2 TO WR-LOTS-NF-RECOVERIES(2).
           MOVE QWR-PRETAX-PROFIT-1 TO WR-PRETAX-PROFIT(1).
           MOVE QWR-SUPERVISION-1 TO WR-SUPERVISION(1).
           MOVE QWR-PRETAX-PROFIT-2 TO WR-PRETAX-PROFIT(2).
           MOVE QWR-SUPERVISION-2 TO WR-SUPERVISION(2).
           MOVE QWR-MGR-PENALTY-FG TO WR-MGR-PENALTY-FG.
           MOVE QWR-NO-TAX-RETURNS-1 TO WR-NO-TAX-RETURNS(1).
           MOVE QWR-NO-TAX-RETURNS-2 TO WR-NO-TAX-RETURNS(2).
 
      *-----------------------------------------------------------------
      *    LOAD QWR-REC FROM WR-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-WR-FIELDS.
           MOVE WR-YYYYMM TO QWR-YYYYMM.
           MOVE WR-BRNO TO QWR-BRNO.

           IF ( WR-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QWR-LTOUCH-DATE
           ELSE
              MOVE WR-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QWR-LTOUCH-DATE.

           MOVE WR-CUR-NO-CUST TO QWR-CUR-NO-CUST.
           MOVE WR-ELM-NO-CUST TO QWR-ELM-NO-CUST.
           MOVE WR-ELY-NO-CUST TO QWR-ELY-NO-CUST.
           MOVE WR-OUTS-BALANCE TO QWR-OUTS-BALANCE.
           MOVE WR-ELM-OUTS-BALANCE TO QWR-ELM-OUTS-BALANCE.
           MOVE WR-ELY-OUTS-BALANCE TO QWR-ELY-OUTS-BALANCE.
           MOVE WR-NO-NB(1) TO QWR-NO-NB-1.
           MOVE WR-NO-FB(1) TO QWR-NO-FB-1.
           MOVE WR-NO-PB(1) TO QWR-NO-PB-1.
           MOVE WR-GROSS-VOLUME(1) TO QWR-GROSS-VOLUME-1.
           MOVE WR-BSR-BONUS(1) TO QWR-BSR-BONUS-1.
           MOVE WR-INTEREST-ALLOCATION(1) TO QWR-INTEREST-ALLOCATION-1.
           MOVE WR-OLD-SUPERVISION(1) TO QWR-OLD-SUPERVISION-1.
           MOVE WR-GEN-RESERVE-ADJ(1) TO QWR-GEN-RESERVE-ADJ-1.
           MOVE WR-INTANGIBLE-AMORT(1) TO QWR-INTANGIBLE-AMORT-1.
           MOVE WR-ROBBERIES(1) TO QWR-ROBBERIES-1.
           MOVE WR-RATING-SHEET-INCOME(1) TO QWR-RATING-SHEET-INCOME-1.
           MOVE WR-INT-FEE-INCOME(1) TO QWR-INT-FEE-INCOME-1.
           MOVE WR-INSURANCE-INCOME(1) TO QWR-INSURANCE-INCOME-1.
           MOVE WR-RATING-SHEET-EXPENSES(1)
            TO QWR-RATING-SHEET-EXPENSES-1.
           MOVE WR-PERSONNEL-EXPENSE(1) TO QWR-PERSONNEL-EXPENSE-1.
           MOVE WR-CHARGEOFF-EXPENSE(1) TO QWR-CHARGEOFF-EXPENSE-1.
           MOVE WR-NO-CHARGEOFFS(1) TO QWR-NO-CHARGEOFFS-1.
           MOVE WR-RECOVERY-INCOME(1) TO QWR-RECOVERY-INCOME-1.
           MOVE WR-BAD-DEBT-INCREASE(1) TO QWR-BAD-DEBT-INCREASE-1.
           MOVE WR-NO-NB(2) TO QWR-NO-NB-2.
           MOVE WR-NO-FB(2) TO QWR-NO-FB-2.
           MOVE WR-NO-PB(2) TO QWR-NO-PB-2.
           MOVE WR-GROSS-VOLUME(2) TO QWR-GROSS-VOLUME-2.
           MOVE WR-BSR-BONUS(2) TO QWR-BSR-BONUS-2.
           MOVE WR-INTEREST-ALLOCATION(2) TO QWR-INTEREST-ALLOCATION-2.
           MOVE WR-OLD-SUPERVISION(2) TO QWR-OLD-SUPERVISION-2.
           MOVE WR-GEN-RESERVE-ADJ(2) TO QWR-GEN-RESERVE-ADJ-2.
           MOVE WR-INTANGIBLE-AMORT(2) TO QWR-INTANGIBLE-AMORT-2.
           MOVE WR-ROBBERIES(2) TO QWR-ROBBERIES-2.
           MOVE WR-RATING-SHEET-INCOME(2) TO QWR-RATING-SHEET-INCOME-2.
           MOVE WR-INT-FEE-INCOME(2) TO QWR-INT-FEE-INCOME-2.
           MOVE WR-INSURANCE-INCOME(2) TO QWR-INSURANCE-INCOME-2.
           MOVE WR-RATING-SHEET-EXPENSES(2)
            TO QWR-RATING-SHEET-EXPENSES-2.
           MOVE WR-PERSONNEL-EXPENSE(2) TO QWR-PERSONNEL-EXPENSE-2.
           MOVE WR-CHARGEOFF-EXPENSE(2) TO QWR-CHARGEOFF-EXPENSE-2.
           MOVE WR-NO-CHARGEOFFS(2) TO QWR-NO-CHARGEOFFS-2.
           MOVE WR-RECOVERY-INCOME(2) TO QWR-RECOVERY-INCOME-2.
           MOVE WR-BAD-DEBT-INCREASE(2) TO QWR-BAD-DEBT-INCREASE-2.
           MOVE WR-NO-30-DAY-ACCTS TO QWR-NO-30-DAY-ACCTS.
           MOVE WR-AMT-30-DAY-ACCTS TO QWR-AMT-30-DAY-ACCTS.
           MOVE WR-NO-60-DAY-ACCTS TO QWR-NO-60-DAY-ACCTS.
           MOVE WR-AMT-60-DAY-ACCTS TO QWR-AMT-60-DAY-ACCTS.
           MOVE WR-NO-90-DAY-ACCTS TO QWR-NO-90-DAY-ACCTS.
           MOVE WR-AMT-90-DAY-ACCTS TO QWR-AMT-90-DAY-ACCTS.
           MOVE WR-ELM-AMT-90-DAY-ACCTS TO QWR-ELM-AMT-90-DAY-ACCTS.
           MOVE WR-ELY-AMT-90-DAY-ACCTS TO QWR-ELY-AMT-90-DAY-ACCTS.
           MOVE WR-YTD-PCT-TURNOVER TO QWR-YTD-PCT-TURNOVER.
           MOVE WR-YTD-PCT-RATE-SHT-PROFIT
            TO QWR-YTD-PCT-RATE-SHT-PROFIT.
           MOVE WR-YTD-PCT-RS-INC-LEDGER TO QWR-YTD-PCT-RS-INC-LEDGER.
           MOVE WR-YTD-PCT-INT-FEE-LEDGER
            TO QWR-YTD-PCT-INT-FEE-LEDGER.
           MOVE WR-YTD-PCT-INS-INC-LEDGER
            TO QWR-YTD-PCT-INS-INC-LEDGER.
           MOVE WR-YTD-PCT-EXPENSE-LEDGER
            TO QWR-YTD-PCT-EXPENSE-LEDGER.
           MOVE WR-YTD-PCT-BAD-DEBT-LEDGER
            TO QWR-YTD-PCT-BAD-DEBT-LEDGER.
           MOVE WR-YTD-PCT-NF-BNK-LEDGER TO QWR-YTD-PCT-NF-BNK-LEDGER.
           MOVE WR-YTD-PCT-PRETAX-LEDGER TO QWR-YTD-PCT-PRETAX-LEDGER.
           MOVE WR-YTD-PCT-DEBT-NF-LEDGER
            TO QWR-YTD-PCT-DEBT-NF-LEDGER.
           MOVE WR-WCBC-CUR-NO-CUST TO QWR-WCBC-CUR-NO-CUST.
           MOVE WR-WCBC-ELM-NO-CUST TO QWR-WCBC-ELM-NO-CUST.
           MOVE WR-WCBC-ELY-NO-CUST TO QWR-WCBC-ELY-NO-CUST.
           MOVE WR-WCBC-OUTS-BALANCE TO QWR-WCBC-OUTS-BALANCE.
           MOVE WR-WCBC-ELM-OUTS-BAL TO QWR-WCBC-ELM-OUTS-BAL.
           MOVE WR-WCBC-ELY-OUTS-BAL TO QWR-WCBC-ELY-OUTS-BAL.
           MOVE WR-WCBC-NO-NB(1) TO QWR-WCBC-NO-NB-1.
           MOVE WR-WCBC-NO-FB(1) TO QWR-WCBC-NO-FB-1.
           MOVE WR-WCBC-NO-PB(1) TO QWR-WCBC-NO-PB-1.
           MOVE WR-WCBC-GROSS-VOLUME(1) TO QWR-WCBC-GROSS-VOLUME-1.
           MOVE WR-WCBC-LATE-CHGS(1) TO QWR-WCBC-LATE-CHGS-1.
           MOVE WR-WCBC-NET-FIN-CHGS(1) TO QWR-WCBC-NET-FIN-CHGS-1.
           MOVE WR-WCBC-SALES(1) TO QWR-WCBC-SALES-1.
           MOVE WR-WCBC-COST-OF-SALES(1) TO QWR-WCBC-COST-OF-SALES-1.
           MOVE WR-WCBC-EXPENSES(1) TO QWR-WCBC-EXPENSES-1.
           MOVE WR-WCBC-RECOVERY-INCOME(1)
            TO QWR-WCBC-RECOVERY-INCOME-1.
           MOVE WR-WCBC-CHARGEOFF-EXPENSE(1)
            TO QWR-WCBC-CHARGEOFF-EXPENSE-1.
           MOVE WR-WCBC-BAD-DEBT-BNKRPCY(1)
            TO QWR-WCBC-BAD-DEBT-BNKRPCY-1.
           MOVE WR-WCBC-NO-NB(2) TO QWR-WCBC-NO-NB-2.
           MOVE WR-WCBC-NO-FB(2) TO QWR-WCBC-NO-FB-2.
           MOVE WR-WCBC-NO-PB(2) TO QWR-WCBC-NO-PB-2.
           MOVE WR-WCBC-GROSS-VOLUME(2) TO QWR-WCBC-GROSS-VOLUME-2.
           MOVE WR-WCBC-LATE-CHGS(2) TO QWR-WCBC-LATE-CHGS-2.
           MOVE WR-WCBC-NET-FIN-CHGS(2) TO QWR-WCBC-NET-FIN-CHGS-2.
           MOVE WR-WCBC-SALES(2) TO QWR-WCBC-SALES-2.
           MOVE WR-WCBC-COST-OF-SALES(2) TO QWR-WCBC-COST-OF-SALES-2.
           MOVE WR-WCBC-EXPENSES(2) TO QWR-WCBC-EXPENSES-2.
           MOVE WR-WCBC-RECOVERY-INCOME(2)
            TO QWR-WCBC-RECOVERY-INCOME-2.
           MOVE WR-WCBC-CHARGEOFF-EXPENSE(2)
            TO QWR-WCBC-CHARGEOFF-EXPENSE-2.
           MOVE WR-WCBC-BAD-DEBT-BNKRPCY(2)
            TO QWR-WCBC-BAD-DEBT-BNKRPCY-2.
           MOVE WR-WCBC-NO-30-DAY-ACCTS TO QWR-WCBC-NO-30-DAY-ACCTS.
           MOVE WR-WCBC-AMT-30-DAY-ACCTS TO QWR-WCBC-AMT-30-DAY-ACCTS.
           MOVE WR-WCBC-NO-60-DAY-ACCTS TO QWR-WCBC-NO-60-DAY-ACCTS.
           MOVE WR-WCBC-AMT-60-DAY-ACCTS TO QWR-WCBC-AMT-60-DAY-ACCTS.
           MOVE WR-WCBC-NO-90-DAY-ACCTS TO QWR-WCBC-NO-90-DAY-ACCTS.
           MOVE WR-WCBC-AMT-90-DAY-ACCTS TO QWR-WCBC-AMT-90-DAY-ACCTS.
           MOVE WR-WCBC-ELM-AMT-90-DAYS TO QWR-WCBC-ELM-AMT-90-DAYS.
           MOVE WR-WCBC-ELY-AMT-90-DAYS TO QWR-WCBC-ELY-AMT-90-DAYS.
           MOVE WR-WCBC-PCT-VOLUME TO QWR-WCBC-PCT-VOLUME.
           MOVE WR-WCBC-GROSS-PROF-PCT TO QWR-WCBC-GROSS-PROF-PCT.
           MOVE WR-LARGE-CUR-NO-CUST TO QWR-LARGE-CUR-NO-CUST.
           MOVE WR-LARGE-ELM-NO-CUST TO QWR-LARGE-ELM-NO-CUST.
           MOVE WR-LARGE-ELY-NO-CUST TO QWR-LARGE-ELY-NO-CUST.
           MOVE WR-LARGE-OUTS-BALANCE TO QWR-LARGE-OUTS-BALANCE.
           MOVE WR-LARGE-ELM-OUTS-BAL TO QWR-LARGE-ELM-OUTS-BAL.
           MOVE WR-LARGE-ELY-OUTS-BAL TO QWR-LARGE-ELY-OUTS-BAL.
           MOVE WR-LARGE-NO-NB(1) TO QWR-LARGE-NO-NB-1.
           MOVE WR-LARGE-NO-FB(1) TO QWR-LARGE-NO-FB-1.
           MOVE WR-LARGE-NO-PB(1) TO QWR-LARGE-NO-PB-1.
           MOVE WR-LARGE-GROSS-VOLUME(1) TO QWR-LARGE-GROSS-VOLUME-1.
           MOVE WR-LARGE-NO-NB(2) TO QWR-LARGE-NO-NB-2.
           MOVE WR-LARGE-NO-FB(2) TO QWR-LARGE-NO-FB-2.
           MOVE WR-LARGE-NO-PB(2) TO QWR-LARGE-NO-PB-2.
           MOVE WR-LARGE-GROSS-VOLUME(2) TO QWR-LARGE-GROSS-VOLUME-2.
           MOVE WR-LARGE-NO-30-DAY-ACCTS TO QWR-LARGE-NO-30-DAY-ACCTS.
           MOVE WR-LARGE-AMT-30-DAY-ACCTS
            TO QWR-LARGE-AMT-30-DAY-ACCTS.
           MOVE WR-LARGE-NO-60-DAY-ACCTS TO QWR-LARGE-NO-60-DAY-ACCTS.
           MOVE WR-LARGE-AMT-60-DAY-ACCTS
            TO QWR-LARGE-AMT-60-DAY-ACCTS.
           MOVE WR-LARGE-NO-90-DAY-ACCTS TO QWR-LARGE-NO-90-DAY-ACCTS.
           MOVE WR-LARGE-AMT-90-DAY-ACCTS
            TO QWR-LARGE-AMT-90-DAY-ACCTS.
           MOVE WR-LARGE-ELM-AMT-90-DAYS TO QWR-LARGE-ELM-AMT-90-DAYS.
           MOVE WR-LARGE-ELY-AMT-90-DAYS TO QWR-LARGE-ELY-AMT-90-DAYS.
           MOVE WR-LIFE-CLAIMS(1) TO QWR-LIFE-CLAIMS-1.
           MOVE WR-AH-CLAIMS(1) TO QWR-AH-CLAIMS-1.
           MOVE WR-FIRE-CLAIMS(1) TO QWR-FIRE-CLAIMS-1.
           MOVE WR-AUTO-CLAIMS(1) TO QWR-AUTO-CLAIMS-1.
           MOVE WR-UNEMP-CLAIMS(1) TO QWR-UNEMP-CLAIMS-1.
           MOVE WR-AD-D-CLAIMS(1) TO QWR-AD-D-CLAIMS-1.
           MOVE WR-L-PROT-CLAIMS(1) TO QWR-L-PROT-CLAIMS-1.
           MOVE WR-NF-NON-BK-CLAIMS(1) TO QWR-NF-NON-BK-CLAIMS-1.
           MOVE WR-NF-BK-CLAIMS(1) TO QWR-NF-BK-CLAIMS-1.
           MOVE WR-ROAD-GARD-CLAIMS(1) TO QWR-ROAD-GARD-CLAIMS-1.
           MOVE WR-NO-LIFE-CLAIMS(1) TO QWR-NO-LIFE-CLAIMS-1.
           MOVE WR-NO-AH-CLAIMS(1) TO QWR-NO-AH-CLAIMS-1.
           MOVE WR-NO-FIRE-CLAIMS(1) TO QWR-NO-FIRE-CLAIMS-1.
           MOVE WR-NO-AUTO-CLAIMS(1) TO QWR-NO-AUTO-CLAIMS-1.
           MOVE WR-NO-UNEMP-CLAIMS(1) TO QWR-NO-UNEMP-CLAIMS-1.
           MOVE WR-NO-AD-D-CLAIMS(1) TO QWR-NO-AD-D-CLAIMS-1.
           MOVE WR-NO-L-PROT-CLAIMS(1) TO QWR-NO-L-PROT-CLAIMS-1.
           MOVE WR-NO-NF-NON-BK-CLAIMS(1) TO QWR-NO-NF-NON-BK-CLAIMS-1.
           MOVE WR-NO-NF-BK-CLAIMS(1) TO QWR-NO-NF-BK-CLAIMS-1.
           MOVE WR-NO-ROAD-GARD-CLAIMS(1) TO QWR-NO-ROAD-GARD-CLAIMS-1.
           MOVE WR-LIFE-PREMS(1) TO QWR-LIFE-PREMS-1.
           MOVE WR-AH-PREMS(1) TO QWR-AH-PREMS-1.
           MOVE WR-FIRE-PREMS(1) TO QWR-FIRE-PREMS-1.
           MOVE WR-AUTO-PREMS(1) TO QWR-AUTO-PREMS-1.
           MOVE WR-UNEMP-PREMS(1) TO QWR-UNEMP-PREMS-1.
           MOVE WR-AD-D-PREMS(1) TO QWR-AD-D-PREMS-1.
           MOVE WR-L-PROT-PREMS(1) TO QWR-L-PROT-PREMS-1.
           MOVE WR-NON-FILE-PREMS(1) TO QWR-NON-FILE-PREMS-1.
           MOVE WR-ROAD-GARD-PREMS(1) TO QWR-ROAD-GARD-PREMS-1.
           MOVE WR-NO-LIFE-PREMS(1) TO QWR-NO-LIFE-PREMS-1.
           MOVE WR-NO-AH-PREMS(1) TO QWR-NO-AH-PREMS-1.
           MOVE WR-NO-FIRE-PREMS(1) TO QWR-NO-FIRE-PREMS-1.
           MOVE WR-NO-AUTO-PREMS(1) TO QWR-NO-AUTO-PREMS-1.
           MOVE WR-NO-UNEMP-PREMS(1) TO QWR-NO-UNEMP-PREMS-1.
           MOVE WR-NO-AD-D-PREMS(1) TO QWR-NO-AD-D-PREMS-1.
           MOVE WR-NO-L-PROT-PREMS(1) TO QWR-NO-L-PROT-PREMS-1.
           MOVE WR-NO-NON-FILE-PREMS(1) TO QWR-NO-NON-FILE-PREMS-1.
           MOVE WR-NO-ROAD-GARD-PREMS(1) TO QWR-NO-ROAD-GARD-PREMS-1.
           MOVE WR-LIFE-REFS(1) TO QWR-LIFE-REFS-1.
           MOVE WR-AH-REFS(1) TO QWR-AH-REFS-1.
           MOVE WR-FIRE-REFS(1) TO QWR-FIRE-REFS-1.
           MOVE WR-AUTO-REFS(1) TO QWR-AUTO-REFS-1.
           MOVE WR-UNEMP-REFS(1) TO QWR-UNEMP-REFS-1.
           MOVE WR-AD-D-REFS(1) TO QWR-AD-D-REFS-1.
           MOVE WR-L-PROT-REFS(1) TO QWR-L-PROT-REFS-1.
           MOVE WR-NON-FILE-REFS(1) TO QWR-NON-FILE-REFS-1.
           MOVE WR-ROAD-GARD-REFS(1) TO QWR-ROAD-GARD-REFS-1.
           MOVE WR-NF-RECOVERIES(1) TO QWR-NF-RECOVERIES-1.
           MOVE WR-LIFE-CLAIMS(2) TO QWR-LIFE-CLAIMS-2.
           MOVE WR-AH-CLAIMS(2) TO QWR-AH-CLAIMS-2.
           MOVE WR-FIRE-CLAIMS(2) TO QWR-FIRE-CLAIMS-2.
           MOVE WR-AUTO-CLAIMS(2) TO QWR-AUTO-CLAIMS-2.
           MOVE WR-UNEMP-CLAIMS(2) TO QWR-UNEMP-CLAIMS-2.
           MOVE WR-AD-D-CLAIMS(2) TO QWR-AD-D-CLAIMS-2.
           MOVE WR-L-PROT-CLAIMS(2) TO QWR-L-PROT-CLAIMS-2.
           MOVE WR-NF-NON-BK-CLAIMS(2) TO QWR-NF-NON-BK-CLAIMS-2.
           MOVE WR-NF-BK-CLAIMS(2) TO QWR-NF-BK-CLAIMS-2.
           MOVE WR-ROAD-GARD-CLAIMS(2) TO QWR-ROAD-GARD-CLAIMS-2.
           MOVE WR-NO-LIFE-CLAIMS(2) TO QWR-NO-LIFE-CLAIMS-2.
           MOVE WR-NO-AH-CLAIMS(2) TO QWR-NO-AH-CLAIMS-2.
           MOVE WR-NO-FIRE-CLAIMS(2) TO QWR-NO-FIRE-CLAIMS-2.
           MOVE WR-NO-AUTO-CLAIMS(2) TO QWR-NO-AUTO-CLAIMS-2.
           MOVE WR-NO-UNEMP-CLAIMS(2) TO QWR-NO-UNEMP-CLAIMS-2.
           MOVE WR-NO-AD-D-CLAIMS(2) TO QWR-NO-AD-D-CLAIMS-2.
           MOVE WR-NO-L-PROT-CLAIMS(2) TO QWR-NO-L-PROT-CLAIMS-2.
           MOVE WR-NO-NF-NON-BK-CLAIMS(2) TO QWR-NO-NF-NON-BK-CLAIMS-2.
           MOVE WR-NO-NF-BK-CLAIMS(2) TO QWR-NO-NF-BK-CLAIMS-2.
           MOVE WR-NO-ROAD-GARD-CLAIMS(2) TO QWR-NO-ROAD-GARD-CLAIMS-2.
           MOVE WR-LIFE-PREMS(2) TO QWR-LIFE-PREMS-2.
           MOVE WR-AH-PREMS(2) TO QWR-AH-PREMS-2.
           MOVE WR-FIRE-PREMS(2) TO QWR-FIRE-PREMS-2.
           MOVE WR-AUTO-PREMS(2) TO QWR-AUTO-PREMS-2.
           MOVE WR-UNEMP-PREMS(2) TO QWR-UNEMP-PREMS-2.
           MOVE WR-AD-D-PREMS(2) TO QWR-AD-D-PREMS-2.
           MOVE WR-L-PROT-PREMS(2) TO QWR-L-PROT-PREMS-2.
           MOVE WR-NON-FILE-PREMS(2) TO QWR-NON-FILE-PREMS-2.
           MOVE WR-ROAD-GARD-PREMS(2) TO QWR-ROAD-GARD-PREMS-2.
           MOVE WR-NO-LIFE-PREMS(2) TO QWR-NO-LIFE-PREMS-2.
           MOVE WR-NO-AH-PREMS(2) TO QWR-NO-AH-PREMS-2.
           MOVE WR-NO-FIRE-PREMS(2) TO QWR-NO-FIRE-PREMS-2.
           MOVE WR-NO-AUTO-PREMS(2) TO QWR-NO-AUTO-PREMS-2.
           MOVE WR-NO-UNEMP-PREMS(2) TO QWR-NO-UNEMP-PREMS-2.
           MOVE WR-NO-AD-D-PREMS(2) TO QWR-NO-AD-D-PREMS-2.
           MOVE WR-NO-L-PROT-PREMS(2) TO QWR-NO-L-PROT-PREMS-2.
           MOVE WR-NO-NON-FILE-PREMS(2) TO QWR-NO-NON-FILE-PREMS-2.
           MOVE WR-NO-ROAD-GARD-PREMS(2) TO QWR-NO-ROAD-GARD-PREMS-2.
           MOVE WR-LIFE-REFS(2) TO QWR-LIFE-REFS-2.
           MOVE WR-AH-REFS(2) TO QWR-AH-REFS-2.
           MOVE WR-FIRE-REFS(2) TO QWR-FIRE-REFS-2.
           MOVE WR-AUTO-REFS(2) TO QWR-AUTO-REFS-2.
           MOVE WR-UNEMP-REFS(2) TO QWR-UNEMP-REFS-2.
           MOVE WR-AD-D-REFS(2) TO QWR-AD-D-REFS-2.
           MOVE WR-L-PROT-REFS(2) TO QWR-L-PROT-REFS-2.
           MOVE WR-NON-FILE-REFS(2) TO QWR-NON-FILE-REFS-2.
           MOVE WR-ROAD-GARD-REFS(2) TO QWR-ROAD-GARD-REFS-2.
           MOVE WR-NF-RECOVERIES(2) TO QWR-NF-RECOVERIES-2.
           MOVE WR-NONFILE-ENDING-BALANCE
            TO QWR-NONFILE-ENDING-BALANCE.
           MOVE WR-NO-CUST-1-YR-AGO TO QWR-NO-CUST-1-YR-AGO.
           MOVE WR-OUTS-BAL-1-YR-AGO TO QWR-OUTS-BAL-1-YR-AGO.
           MOVE WR-WCBC-NO-CUST-1-YR-AGO TO QWR-WCBC-NO-CUST-1-YR-AGO.
           MOVE WR-WCBC-OUTS-BAL-1-YR-AGO
            TO QWR-WCBC-OUTS-BAL-1-YR-AGO.
           MOVE WR-LARGE-NO-CUST-1-YR-AGO
            TO QWR-LARGE-NO-CUST-1-YR-AGO.
           MOVE WR-LARGE-OUTS-BAL-1-YR-AGO
            TO QWR-LARGE-OUTS-BAL-1-YR-AGO.
           MOVE WR-CUR-NO-CONT-SLOW TO QWR-CUR-NO-CONT-SLOW.
           MOVE WR-WCBC-CUR-NO-CONT-SLOW TO QWR-WCBC-CUR-NO-CONT-SLOW.
           MOVE WR-LARGE-CUR-NO-CONT-SLOW
            TO QWR-LARGE-CUR-NO-CONT-SLOW.
           MOVE WR-CASHIER-BONUS TO QWR-CASHIER-BONUS.
           MOVE WR-ASST-MGR-BONUS TO QWR-ASST-MGR-BONUS.
           MOVE WR-MGR-BONUS TO QWR-MGR-BONUS.
           MOVE WR-LOTS-NF-CLAIMS(1) TO QWR-LOTS-NF-CLAIMS-1.
           MOVE WR-LOTS-NO-NF-CLAIMS(1) TO QWR-LOTS-NO-NF-CLAIMS-1.
           MOVE WR-LOTS-NON-FILE-PREMS(1) TO QWR-LOTS-NON-FILE-PREMS-1.
           MOVE WR-LOTS-NO-NON-FILE-PREMS(1)
            TO QWR-LOTS-NO-NON-FILE-PREMS-1.
           MOVE WR-LOTS-NF-RECOVERIES(1) TO QWR-LOTS-NF-RECOVERIES-1.
           MOVE WR-LOTS-NF-CLAIMS(2) TO QWR-LOTS-NF-CLAIMS-2.
           MOVE WR-LOTS-NO-NF-CLAIMS(2) TO QWR-LOTS-NO-NF-CLAIMS-2.
           MOVE WR-LOTS-NON-FILE-PREMS(2) TO QWR-LOTS-NON-FILE-PREMS-2.
           MOVE WR-LOTS-NO-NON-FILE-PREMS(2)
            TO QWR-LOTS-NO-NON-FILE-PREMS-2.
           MOVE WR-LOTS-NF-RECOVERIES(2) TO QWR-LOTS-NF-RECOVERIES-2.
           MOVE WR-PRETAX-PROFIT(1) TO QWR-PRETAX-PROFIT-1.
           MOVE WR-SUPERVISION(1) TO QWR-SUPERVISION-1.
           MOVE WR-PRETAX-PROFIT(2) TO QWR-PRETAX-PROFIT-2.
           MOVE WR-SUPERVISION(2) TO QWR-SUPERVISION-2.
           MOVE WR-MGR-PENALTY-FG TO QWR-MGR-PENALTY-FG.
           MOVE WR-NO-TAX-RETURNS(1) TO QWR-NO-TAX-RETURNS-1.
           MOVE WR-NO-TAX-RETURNS(2) TO QWR-NO-TAX-RETURNS-2.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      * COPYMEMBER: LIBSP/SPWR1RN
       LOAD-WR1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( WR-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( WR-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO WR-PATH-BASE
              MOVE EXT-FILPATH-MACHINE     TO WR-PATH-MACHINE
              MOVE WR-PATH-FORMAT          TO WR-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( WR-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO WR-PATH-BASE
              MOVE FILPATH-MACHINE         TO WR-PATH-MACHINE
              MOVE WR-PATH-FORMAT          TO WR-PATH.
*G2G  *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*G2G  *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
*G2G  *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-WRFILE NOT = "S" )
*G2G          MOVE WR-PATH-BEG             TO WR-SELECT-PATH-BEG
*G2G          MOVE WR-PATH-FILE            TO WR-SELECT-PATH-FILE
*G2G       ELSE
*G2G          MOVE WR-PATH-SQL             TO WR-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-WR1-FILE.
           PERFORM LOAD-WR1-FILE.
           PERFORM OPEN-IT.
           MOVE WR-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-WR1-FILE.
           PERFORM START-IT.
           MOVE WR-SELECT-PATH TO E-FILE.
           MOVE WR1-KEY TO E-KEYX.

           IF ( WR1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-WR1-FILE.

           IF ( QWR1-WEND-YYYYMM = ZEROES    ) OR
              ( QWR1-WEND-YYYYMM NOT NUMERIC )
              MOVE WR-YYYYMM            TO QWR1-WBEG-YYYYMM
              MOVE ALL "9"              TO QWR1-WEND-YYYYMM
              MOVE WR-BRNO              TO QWR1-WBEG-BRNO
              MOVE ALL "9"              TO QWR1-WEND-BRNO.

           EXEC SQL
            DECLARE WR1_CURSOR CURSOR FOR
            SELECT WRFILE.WR_YYYYMM,
                   WRFILE.WR_BRNO,
                   ISNULL(CAST(WRFILE.WR_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   WRFILE.WR_CUR_NO_CUST,
                   WRFILE.WR_ELM_NO_CUST,
                   WRFILE.WR_ELY_NO_CUST,
                   WRFILE.WR_OUTS_BALANCE,
                   WRFILE.WR_ELM_OUTS_BALANCE,
                   WRFILE.WR_ELY_OUTS_BALANCE,
                   WRFILE.WR_NO_NB_1,
                   WRFILE.WR_NO_FB_1,
                   WRFILE.WR_NO_PB_1,
                   WRFILE.WR_GROSS_VOLUME_1,
                   WRFILE.WR_BSR_BONUS_1,
                   WRFILE.WR_INTEREST_ALLOCATION_1,
                   WRFILE.WR_OLD_SUPERVISION_1,
                   WRFILE.WR_GEN_RESERVE_ADJ_1,
                   WRFILE.WR_INTANGIBLE_AMORT_1,
                   WRFILE.WR_ROBBERIES_1,
                   WRFILE.WR_RATING_SHEET_INCOME_1,
                   WRFILE.WR_INT_FEE_INCOME_1,
                   WRFILE.WR_INSURANCE_INCOME_1,
                   WRFILE.WR_RATING_SHEET_EXPENSES_1,
                   WRFILE.WR_PERSONNEL_EXPENSE_1,
                   WRFILE.WR_CHARGEOFF_EXPENSE_1,
                   WRFILE.WR_NO_CHARGEOFFS_1,
                   WRFILE.WR_RECOVERY_INCOME_1,
                   WRFILE.WR_BAD_DEBT_INCREASE_1,
                   WRFILE.WR_NO_NB_2,
                   WRFILE.WR_NO_FB_2,
                   WRFILE.WR_NO_PB_2,
                   WRFILE.WR_GROSS_VOLUME_2,
                   WRFILE.WR_BSR_BONUS_2,
                   WRFILE.WR_INTEREST_ALLOCATION_2,
                   WRFILE.WR_OLD_SUPERVISION_2,
                   WRFILE.WR_GEN_RESERVE_ADJ_2,
                   WRFILE.WR_INTANGIBLE_AMORT_2,
                   WRFILE.WR_ROBBERIES_2,
                   WRFILE.WR_RATING_SHEET_INCOME_2,
                   WRFILE.WR_INT_FEE_INCOME_2,
                   WRFILE.WR_INSURANCE_INCOME_2,
                   WRFILE.WR_RATING_SHEET_EXPENSES_2,
                   WRFILE.WR_PERSONNEL_EXPENSE_2,
                   WRFILE.WR_CHARGEOFF_EXPENSE_2,
                   WRFILE.WR_NO_CHARGEOFFS_2,
                   WRFILE.WR_RECOVERY_INCOME_2,
                   WRFILE.WR_BAD_DEBT_INCREASE_2,
                   WRFILE.WR_NO_30_DAY_ACCTS,
                   WRFILE.WR_AMT_30_DAY_ACCTS,
                   WRFILE.WR_NO_60_DAY_ACCTS,
                   WRFILE.WR_AMT_60_DAY_ACCTS,
                   WRFILE.WR_NO_90_DAY_ACCTS,
                   WRFILE.WR_AMT_90_DAY_ACCTS,
                   WRFILE.WR_ELM_AMT_90_DAY_ACCTS,
                   WRFILE.WR_ELY_AMT_90_DAY_ACCTS,
                   WRFILE.WR_YTD_PCT_TURNOVER,
                   WRFILE.WR_YTD_PCT_RATE_SHT_PROFIT,
                   WRFILE.WR_YTD_PCT_RS_INC_LEDGER,
                   WRFILE.WR_YTD_PCT_INT_FEE_LEDGER,
                   WRFILE.WR_YTD_PCT_INS_INC_LEDGER,
                   WRFILE.WR_YTD_PCT_EXPENSE_LEDGER,
                   WRFILE.WR_YTD_PCT_BAD_DEBT_LEDGER,
                   WRFILE.WR_YTD_PCT_NF_BNK_LEDGER,
                   WRFILE.WR_YTD_PCT_PRETAX_LEDGER,
                   WRFILE.WR_YTD_PCT_DEBT_NF_LEDGER,
                   WRFILE.WR_WCBC_CUR_NO_CUST,
                   WRFILE.WR_WCBC_ELM_NO_CUST,
                   WRFILE.WR_WCBC_ELY_NO_CUST,
                   WRFILE.WR_WCBC_OUTS_BALANCE,
                   WRFILE.WR_WCBC_ELM_OUTS_BAL,
                   WRFILE.WR_WCBC_ELY_OUTS_BAL,
                   WRFILE.WR_WCBC_NO_NB_1,
                   WRFILE.WR_WCBC_NO_FB_1,
                   WRFILE.WR_WCBC_NO_PB_1,
                   WRFILE.WR_WCBC_GROSS_VOLUME_1,
                   WRFILE.WR_WCBC_LATE_CHGS_1,
                   WRFILE.WR_WCBC_NET_FIN_CHGS_1,
                   WRFILE.WR_WCBC_SALES_1,
                   WRFILE.WR_WCBC_COST_OF_SALES_1,
                   WRFILE.WR_WCBC_EXPENSES_1,
                   WRFILE.WR_WCBC_RECOVERY_INCOME_1,
                   WRFILE.WR_WCBC_CHARGEOFF_EXPENSE_1,
                   WRFILE.WR_WCBC_BAD_DEBT_BNKRPCY_1,
                   WRFILE.WR_WCBC_NO_NB_2,
                   WRFILE.WR_WCBC_NO_FB_2,
                   WRFILE.WR_WCBC_NO_PB_2,
                   WRFILE.WR_WCBC_GROSS_VOLUME_2,
                   WRFILE.WR_WCBC_LATE_CHGS_2,
                   WRFILE.WR_WCBC_NET_FIN_CHGS_2,
                   WRFILE.WR_WCBC_SALES_2,
                   WRFILE.WR_WCBC_COST_OF_SALES_2,
                   WRFILE.WR_WCBC_EXPENSES_2,
                   WRFILE.WR_WCBC_RECOVERY_INCOME_2,
                   WRFILE.WR_WCBC_CHARGEOFF_EXPENSE_2,
                   WRFILE.WR_WCBC_BAD_DEBT_BNKRPCY_2,
                   WRFILE.WR_WCBC_NO_30_DAY_ACCTS,
                   WRFILE.WR_WCBC_AMT_30_DAY_ACCTS,
                   WRFILE.WR_WCBC_NO_60_DAY_ACCTS,
                   WRFILE.WR_WCBC_AMT_60_DAY_ACCTS,
                   WRFILE.WR_WCBC_NO_90_DAY_ACCTS,
                   WRFILE.WR_WCBC_AMT_90_DAY_ACCTS,
                   WRFILE.WR_WCBC_ELM_AMT_90_DAYS,
                   WRFILE.WR_WCBC_ELY_AMT_90_DAYS,
                   WRFILE.WR_WCBC_PCT_VOLUME,
                   WRFILE.WR_WCBC_GROSS_PROF_PCT,
                   WRFILE.WR_LARGE_CUR_NO_CUST,
                   WRFILE.WR_LARGE_ELM_NO_CUST,
                   WRFILE.WR_LARGE_ELY_NO_CUST,
                   WRFILE.WR_LARGE_OUTS_BALANCE,
                   WRFILE.WR_LARGE_ELM_OUTS_BAL,
                   WRFILE.WR_LARGE_ELY_OUTS_BAL,
                   WRFILE.WR_LARGE_NO_NB_1,
                   WRFILE.WR_LARGE_NO_FB_1,
                   WRFILE.WR_LARGE_NO_PB_1,
                   WRFILE.WR_LARGE_GROSS_VOLUME_1,
                   WRFILE.WR_LARGE_NO_NB_2,
                   WRFILE.WR_LARGE_NO_FB_2,
                   WRFILE.WR_LARGE_NO_PB_2,
                   WRFILE.WR_LARGE_GROSS_VOLUME_2,
                   WRFILE.WR_LARGE_NO_30_DAY_ACCTS,
                   WRFILE.WR_LARGE_AMT_30_DAY_ACCTS,
                   WRFILE.WR_LARGE_NO_60_DAY_ACCTS,
                   WRFILE.WR_LARGE_AMT_60_DAY_ACCTS,
                   WRFILE.WR_LARGE_NO_90_DAY_ACCTS,
                   WRFILE.WR_LARGE_AMT_90_DAY_ACCTS,
                   WRFILE.WR_LARGE_ELM_AMT_90_DAYS,
                   WRFILE.WR_LARGE_ELY_AMT_90_DAYS,
                   WRFILE.WR_LIFE_CLAIMS_1,
                   WRFILE.WR_AH_CLAIMS_1,
                   WRFILE.WR_FIRE_CLAIMS_1,
                   WRFILE.WR_AUTO_CLAIMS_1,
                   WRFILE.WR_UNEMP_CLAIMS_1,
                   WRFILE.WR_AD_D_CLAIMS_1,
                   WRFILE.WR_L_PROT_CLAIMS_1,
                   WRFILE.WR_NF_NON_BK_CLAIMS_1,
                   WRFILE.WR_NF_BK_CLAIMS_1,
                   WRFILE.WR_ROAD_GARD_CLAIMS_1,
                   WRFILE.WR_NO_LIFE_CLAIMS_1,
                   WRFILE.WR_NO_AH_CLAIMS_1,
                   WRFILE.WR_NO_FIRE_CLAIMS_1,
                   WRFILE.WR_NO_AUTO_CLAIMS_1,
                   WRFILE.WR_NO_UNEMP_CLAIMS_1,
                   WRFILE.WR_NO_AD_D_CLAIMS_1,
                   WRFILE.WR_NO_L_PROT_CLAIMS_1,
                   WRFILE.WR_NO_NF_NON_BK_CLAIMS_1,
                   WRFILE.WR_NO_NF_BK_CLAIMS_1,
                   WRFILE.WR_NO_ROAD_GARD_CLAIMS_1,
                   WRFILE.WR_LIFE_PREMS_1,
                   WRFILE.WR_AH_PREMS_1,
                   WRFILE.WR_FIRE_PREMS_1,
                   WRFILE.WR_AUTO_PREMS_1,
                   WRFILE.WR_UNEMP_PREMS_1,
                   WRFILE.WR_AD_D_PREMS_1,
                   WRFILE.WR_L_PROT_PREMS_1,
                   WRFILE.WR_NON_FILE_PREMS_1,
                   WRFILE.WR_ROAD_GARD_PREMS_1,
                   WRFILE.WR_NO_LIFE_PREMS_1,
                   WRFILE.WR_NO_AH_PREMS_1,
                   WRFILE.WR_NO_FIRE_PREMS_1,
                   WRFILE.WR_NO_AUTO_PREMS_1,
                   WRFILE.WR_NO_UNEMP_PREMS_1,
                   WRFILE.WR_NO_AD_D_PREMS_1,
                   WRFILE.WR_NO_L_PROT_PREMS_1,
                   WRFILE.WR_NO_NON_FILE_PREMS_1,
                   WRFILE.WR_NO_ROAD_GARD_PREMS_1,
                   WRFILE.WR_LIFE_REFS_1,
                   WRFILE.WR_AH_REFS_1,
                   WRFILE.WR_FIRE_REFS_1,
                   WRFILE.WR_AUTO_REFS_1,
                   WRFILE.WR_UNEMP_REFS_1,
                   WRFILE.WR_AD_D_REFS_1,
                   WRFILE.WR_L_PROT_REFS_1,
                   WRFILE.WR_NON_FILE_REFS_1,
                   WRFILE.WR_ROAD_GARD_REFS_1,
                   WRFILE.WR_NF_RECOVERIES_1,
                   WRFILE.WR_LIFE_CLAIMS_2,
                   WRFILE.WR_AH_CLAIMS_2,
                   WRFILE.WR_FIRE_CLAIMS_2,
                   WRFILE.WR_AUTO_CLAIMS_2,
                   WRFILE.WR_UNEMP_CLAIMS_2,
                   WRFILE.WR_AD_D_CLAIMS_2,
                   WRFILE.WR_L_PROT_CLAIMS_2,
                   WRFILE.WR_NF_NON_BK_CLAIMS_2,
                   WRFILE.WR_NF_BK_CLAIMS_2,
                   WRFILE.WR_ROAD_GARD_CLAIMS_2,
                   WRFILE.WR_NO_LIFE_CLAIMS_2,
                   WRFILE.WR_NO_AH_CLAIMS_2,
                   WRFILE.WR_NO_FIRE_CLAIMS_2,
                   WRFILE.WR_NO_AUTO_CLAIMS_2,
                   WRFILE.WR_NO_UNEMP_CLAIMS_2,
                   WRFILE.WR_NO_AD_D_CLAIMS_2,
                   WRFILE.WR_NO_L_PROT_CLAIMS_2,
                   WRFILE.WR_NO_NF_NON_BK_CLAIMS_2,
                   WRFILE.WR_NO_NF_BK_CLAIMS_2,
                   WRFILE.WR_NO_ROAD_GARD_CLAIMS_2,
                   WRFILE.WR_LIFE_PREMS_2,
                   WRFILE.WR_AH_PREMS_2,
                   WRFILE.WR_FIRE_PREMS_2,
                   WRFILE.WR_AUTO_PREMS_2,
                   WRFILE.WR_UNEMP_PREMS_2,
                   WRFILE.WR_AD_D_PREMS_2,
                   WRFILE.WR_L_PROT_PREMS_2,
                   WRFILE.WR_NON_FILE_PREMS_2,
                   WRFILE.WR_ROAD_GARD_PREMS_2,
                   WRFILE.WR_NO_LIFE_PREMS_2,
                   WRFILE.WR_NO_AH_PREMS_2,
                   WRFILE.WR_NO_FIRE_PREMS_2,
                   WRFILE.WR_NO_AUTO_PREMS_2,
                   WRFILE.WR_NO_UNEMP_PREMS_2,
                   WRFILE.WR_NO_AD_D_PREMS_2,
                   WRFILE.WR_NO_L_PROT_PREMS_2,
                   WRFILE.WR_NO_NON_FILE_PREMS_2,
                   WRFILE.WR_NO_ROAD_GARD_PREMS_2,
                   WRFILE.WR_LIFE_REFS_2,
                   WRFILE.WR_AH_REFS_2,
                   WRFILE.WR_FIRE_REFS_2,
                   WRFILE.WR_AUTO_REFS_2,
                   WRFILE.WR_UNEMP_REFS_2,
                   WRFILE.WR_AD_D_REFS_2,
                   WRFILE.WR_L_PROT_REFS_2,
                   WRFILE.WR_NON_FILE_REFS_2,
                   WRFILE.WR_ROAD_GARD_REFS_2,
                   WRFILE.WR_NF_RECOVERIES_2,
                   WRFILE.WR_NONFILE_ENDING_BALANCE,
                   WRFILE.WR_NO_CUST_1_YR_AGO,
                   WRFILE.WR_OUTS_BAL_1_YR_AGO,
                   WRFILE.WR_WCBC_NO_CUST_1_YR_AGO,
                   WRFILE.WR_WCBC_OUTS_BAL_1_YR_AGO,
                   WRFILE.WR_LARGE_NO_CUST_1_YR_AGO,
                   WRFILE.WR_LARGE_OUTS_BAL_1_YR_AGO,
                   WRFILE.WR_CUR_NO_CONT_SLOW,
                   WRFILE.WR_WCBC_CUR_NO_CONT_SLOW,
                   WRFILE.WR_LARGE_CUR_NO_CONT_SLOW,
                   WRFILE.WR_CASHIER_BONUS,
                   WRFILE.WR_ASST_MGR_BONUS,
                   WRFILE.WR_MGR_BONUS,
                   WRFILE.WR_LOTS_NF_CLAIMS_1,
                   WRFILE.WR_LOTS_NO_NF_CLAIMS_1,
                   WRFILE.WR_LOTS_NON_FILE_PREMS_1,
                   WRFILE.WR_LOTS_NO_NON_FILE_PREMS_1,
                   WRFILE.WR_LOTS_NF_RECOVERIES_1,
                   WRFILE.WR_LOTS_NF_CLAIMS_2,
                   WRFILE.WR_LOTS_NO_NF_CLAIMS_2,
                   WRFILE.WR_LOTS_NON_FILE_PREMS_2,
                   WRFILE.WR_LOTS_NO_NON_FILE_PREMS_2,
                   WRFILE.WR_LOTS_NF_RECOVERIES_2,
                   WRFILE.WR_PRETAX_PROFIT_1,
                   WRFILE.WR_SUPERVISION_1,
                   WRFILE.WR_PRETAX_PROFIT_2,
                   WRFILE.WR_SUPERVISION_2,
                   ISNULL(WRFILE.WR_MGR_PENALTY_FG,''),
                   WRFILE.WR_NO_TAX_RETURNS_1,
                   WRFILE.WR_NO_TAX_RETURNS_2
            FROM DBO.WRFILE
            WHERE WRFILE.WR_YYYYMM >= :QWR1-WBEG-YYYYMM
              AND WRFILE.WR_BRNO >= :QWR1-WBEG-BRNO
              AND WRFILE.WR_YYYYMM <= :QWR1-WEND-YYYYMM
              AND WRFILE.WR_BRNO <= :QWR1-WEND-BRNO
            ORDER BY WRFILE.WR_YYYYMM,
                     WRFILE.WR_BRNO
           END-EXEC.

           EXEC SQL
               OPEN WR1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QWR1-WBEG-YYYYMM
                          QWR1-WEND-YYYYMM
                          QWR1-WEND-BRNO
                          QWR1-WEND-BRNO.

           MOVE STAT---GOOD TO WR1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-WR1-FILE.
           PERFORM READ-IT.
           MOVE WR-SELECT-PATH TO E-FILE.
           MOVE WR1-KEY TO E-KEYX.

           MOVE WR-YYYYMM   TO QWR-YYYYMM.
           MOVE WR-BRNO     TO QWR-BRNO.

           EXEC SQL
            SELECT WRFILE.WR_YYYYMM,
                   WRFILE.WR_BRNO,
                   ISNULL(CAST(WRFILE.WR_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   WRFILE.WR_CUR_NO_CUST,
                   WRFILE.WR_ELM_NO_CUST,
                   WRFILE.WR_ELY_NO_CUST,
                   WRFILE.WR_OUTS_BALANCE,
                   WRFILE.WR_ELM_OUTS_BALANCE,
                   WRFILE.WR_ELY_OUTS_BALANCE,
                   WRFILE.WR_NO_NB_1,
                   WRFILE.WR_NO_FB_1,
                   WRFILE.WR_NO_PB_1,
                   WRFILE.WR_GROSS_VOLUME_1,
                   WRFILE.WR_BSR_BONUS_1,
                   WRFILE.WR_INTEREST_ALLOCATION_1,
                   WRFILE.WR_OLD_SUPERVISION_1,
                   WRFILE.WR_GEN_RESERVE_ADJ_1,
                   WRFILE.WR_INTANGIBLE_AMORT_1,
                   WRFILE.WR_ROBBERIES_1,
                   WRFILE.WR_RATING_SHEET_INCOME_1,
                   WRFILE.WR_INT_FEE_INCOME_1,
                   WRFILE.WR_INSURANCE_INCOME_1,
                   WRFILE.WR_RATING_SHEET_EXPENSES_1,
                   WRFILE.WR_PERSONNEL_EXPENSE_1,
                   WRFILE.WR_CHARGEOFF_EXPENSE_1,
                   WRFILE.WR_NO_CHARGEOFFS_1,
                   WRFILE.WR_RECOVERY_INCOME_1,
                   WRFILE.WR_BAD_DEBT_INCREASE_1,
                   WRFILE.WR_NO_NB_2,
                   WRFILE.WR_NO_FB_2,
                   WRFILE.WR_NO_PB_2,
                   WRFILE.WR_GROSS_VOLUME_2,
                   WRFILE.WR_BSR_BONUS_2,
                   WRFILE.WR_INTEREST_ALLOCATION_2,
                   WRFILE.WR_OLD_SUPERVISION_2,
                   WRFILE.WR_GEN_RESERVE_ADJ_2,
                   WRFILE.WR_INTANGIBLE_AMORT_2,
                   WRFILE.WR_ROBBERIES_2,
                   WRFILE.WR_RATING_SHEET_INCOME_2,
                   WRFILE.WR_INT_FEE_INCOME_2,
                   WRFILE.WR_INSURANCE_INCOME_2,
                   WRFILE.WR_RATING_SHEET_EXPENSES_2,
                   WRFILE.WR_PERSONNEL_EXPENSE_2,
                   WRFILE.WR_CHARGEOFF_EXPENSE_2,
                   WRFILE.WR_NO_CHARGEOFFS_2,
                   WRFILE.WR_RECOVERY_INCOME_2,
                   WRFILE.WR_BAD_DEBT_INCREASE_2,
                   WRFILE.WR_NO_30_DAY_ACCTS,
                   WRFILE.WR_AMT_30_DAY_ACCTS,
                   WRFILE.WR_NO_60_DAY_ACCTS,
                   WRFILE.WR_AMT_60_DAY_ACCTS,
                   WRFILE.WR_NO_90_DAY_ACCTS,
                   WRFILE.WR_AMT_90_DAY_ACCTS,
                   WRFILE.WR_ELM_AMT_90_DAY_ACCTS,
                   WRFILE.WR_ELY_AMT_90_DAY_ACCTS,
                   WRFILE.WR_YTD_PCT_TURNOVER,
                   WRFILE.WR_YTD_PCT_RATE_SHT_PROFIT,
                   WRFILE.WR_YTD_PCT_RS_INC_LEDGER,
                   WRFILE.WR_YTD_PCT_INT_FEE_LEDGER,
                   WRFILE.WR_YTD_PCT_INS_INC_LEDGER,
                   WRFILE.WR_YTD_PCT_EXPENSE_LEDGER,
                   WRFILE.WR_YTD_PCT_BAD_DEBT_LEDGER,
                   WRFILE.WR_YTD_PCT_NF_BNK_LEDGER,
                   WRFILE.WR_YTD_PCT_PRETAX_LEDGER,
                   WRFILE.WR_YTD_PCT_DEBT_NF_LEDGER,
                   WRFILE.WR_WCBC_CUR_NO_CUST,
                   WRFILE.WR_WCBC_ELM_NO_CUST,
                   WRFILE.WR_WCBC_ELY_NO_CUST,
                   WRFILE.WR_WCBC_OUTS_BALANCE,
                   WRFILE.WR_WCBC_ELM_OUTS_BAL,
                   WRFILE.WR_WCBC_ELY_OUTS_BAL,
                   WRFILE.WR_WCBC_NO_NB_1,
                   WRFILE.WR_WCBC_NO_FB_1,
                   WRFILE.WR_WCBC_NO_PB_1,
                   WRFILE.WR_WCBC_GROSS_VOLUME_1,
                   WRFILE.WR_WCBC_LATE_CHGS_1,
                   WRFILE.WR_WCBC_NET_FIN_CHGS_1,
                   WRFILE.WR_WCBC_SALES_1,
                   WRFILE.WR_WCBC_COST_OF_SALES_1,
                   WRFILE.WR_WCBC_EXPENSES_1,
                   WRFILE.WR_WCBC_RECOVERY_INCOME_1,
                   WRFILE.WR_WCBC_CHARGEOFF_EXPENSE_1,
                   WRFILE.WR_WCBC_BAD_DEBT_BNKRPCY_1,
                   WRFILE.WR_WCBC_NO_NB_2,
                   WRFILE.WR_WCBC_NO_FB_2,
                   WRFILE.WR_WCBC_NO_PB_2,
                   WRFILE.WR_WCBC_GROSS_VOLUME_2,
                   WRFILE.WR_WCBC_LATE_CHGS_2,
                   WRFILE.WR_WCBC_NET_FIN_CHGS_2,
                   WRFILE.WR_WCBC_SALES_2,
                   WRFILE.WR_WCBC_COST_OF_SALES_2,
                   WRFILE.WR_WCBC_EXPENSES_2,
                   WRFILE.WR_WCBC_RECOVERY_INCOME_2,
                   WRFILE.WR_WCBC_CHARGEOFF_EXPENSE_2,
                   WRFILE.WR_WCBC_BAD_DEBT_BNKRPCY_2,
                   WRFILE.WR_WCBC_NO_30_DAY_ACCTS,
                   WRFILE.WR_WCBC_AMT_30_DAY_ACCTS,
                   WRFILE.WR_WCBC_NO_60_DAY_ACCTS,
                   WRFILE.WR_WCBC_AMT_60_DAY_ACCTS,
                   WRFILE.WR_WCBC_NO_90_DAY_ACCTS,
                   WRFILE.WR_WCBC_AMT_90_DAY_ACCTS,
                   WRFILE.WR_WCBC_ELM_AMT_90_DAYS,
                   WRFILE.WR_WCBC_ELY_AMT_90_DAYS,
                   WRFILE.WR_WCBC_PCT_VOLUME,
                   WRFILE.WR_WCBC_GROSS_PROF_PCT,
                   WRFILE.WR_LARGE_CUR_NO_CUST,
                   WRFILE.WR_LARGE_ELM_NO_CUST,
                   WRFILE.WR_LARGE_ELY_NO_CUST,
                   WRFILE.WR_LARGE_OUTS_BALANCE,
                   WRFILE.WR_LARGE_ELM_OUTS_BAL,
                   WRFILE.WR_LARGE_ELY_OUTS_BAL,
                   WRFILE.WR_LARGE_NO_NB_1,
                   WRFILE.WR_LARGE_NO_FB_1,
                   WRFILE.WR_LARGE_NO_PB_1,
                   WRFILE.WR_LARGE_GROSS_VOLUME_1,
                   WRFILE.WR_LARGE_NO_NB_2,
                   WRFILE.WR_LARGE_NO_FB_2,
                   WRFILE.WR_LARGE_NO_PB_2,
                   WRFILE.WR_LARGE_GROSS_VOLUME_2,
                   WRFILE.WR_LARGE_NO_30_DAY_ACCTS,
                   WRFILE.WR_LARGE_AMT_30_DAY_ACCTS,
                   WRFILE.WR_LARGE_NO_60_DAY_ACCTS,
                   WRFILE.WR_LARGE_AMT_60_DAY_ACCTS,
                   WRFILE.WR_LARGE_NO_90_DAY_ACCTS,
                   WRFILE.WR_LARGE_AMT_90_DAY_ACCTS,
                   WRFILE.WR_LARGE_ELM_AMT_90_DAYS,
                   WRFILE.WR_LARGE_ELY_AMT_90_DAYS,
                   WRFILE.WR_LIFE_CLAIMS_1,
                   WRFILE.WR_AH_CLAIMS_1,
                   WRFILE.WR_FIRE_CLAIMS_1,
                   WRFILE.WR_AUTO_CLAIMS_1,
                   WRFILE.WR_UNEMP_CLAIMS_1,
                   WRFILE.WR_AD_D_CLAIMS_1,
                   WRFILE.WR_L_PROT_CLAIMS_1,
                   WRFILE.WR_NF_NON_BK_CLAIMS_1,
                   WRFILE.WR_NF_BK_CLAIMS_1,
                   WRFILE.WR_ROAD_GARD_CLAIMS_1,
                   WRFILE.WR_NO_LIFE_CLAIMS_1,
                   WRFILE.WR_NO_AH_CLAIMS_1,
                   WRFILE.WR_NO_FIRE_CLAIMS_1,
                   WRFILE.WR_NO_AUTO_CLAIMS_1,
                   WRFILE.WR_NO_UNEMP_CLAIMS_1,
                   WRFILE.WR_NO_AD_D_CLAIMS_1,
                   WRFILE.WR_NO_L_PROT_CLAIMS_1,
                   WRFILE.WR_NO_NF_NON_BK_CLAIMS_1,
                   WRFILE.WR_NO_NF_BK_CLAIMS_1,
                   WRFILE.WR_NO_ROAD_GARD_CLAIMS_1,
                   WRFILE.WR_LIFE_PREMS_1,
                   WRFILE.WR_AH_PREMS_1,
                   WRFILE.WR_FIRE_PREMS_1,
                   WRFILE.WR_AUTO_PREMS_1,
                   WRFILE.WR_UNEMP_PREMS_1,
                   WRFILE.WR_AD_D_PREMS_1,
                   WRFILE.WR_L_PROT_PREMS_1,
                   WRFILE.WR_NON_FILE_PREMS_1,
                   WRFILE.WR_ROAD_GARD_PREMS_1,
                   WRFILE.WR_NO_LIFE_PREMS_1,
                   WRFILE.WR_NO_AH_PREMS_1,
                   WRFILE.WR_NO_FIRE_PREMS_1,
                   WRFILE.WR_NO_AUTO_PREMS_1,
                   WRFILE.WR_NO_UNEMP_PREMS_1,
                   WRFILE.WR_NO_AD_D_PREMS_1,
                   WRFILE.WR_NO_L_PROT_PREMS_1,
                   WRFILE.WR_NO_NON_FILE_PREMS_1,
                   WRFILE.WR_NO_ROAD_GARD_PREMS_1,
                   WRFILE.WR_LIFE_REFS_1,
                   WRFILE.WR_AH_REFS_1,
                   WRFILE.WR_FIRE_REFS_1,
                   WRFILE.WR_AUTO_REFS_1,
                   WRFILE.WR_UNEMP_REFS_1,
                   WRFILE.WR_AD_D_REFS_1,
                   WRFILE.WR_L_PROT_REFS_1,
                   WRFILE.WR_NON_FILE_REFS_1,
                   WRFILE.WR_ROAD_GARD_REFS_1,
                   WRFILE.WR_NF_RECOVERIES_1,
                   WRFILE.WR_LIFE_CLAIMS_2,
                   WRFILE.WR_AH_CLAIMS_2,
                   WRFILE.WR_FIRE_CLAIMS_2,
                   WRFILE.WR_AUTO_CLAIMS_2,
                   WRFILE.WR_UNEMP_CLAIMS_2,
                   WRFILE.WR_AD_D_CLAIMS_2,
                   WRFILE.WR_L_PROT_CLAIMS_2,
                   WRFILE.WR_NF_NON_BK_CLAIMS_2,
                   WRFILE.WR_NF_BK_CLAIMS_2,
                   WRFILE.WR_ROAD_GARD_CLAIMS_2,
                   WRFILE.WR_NO_LIFE_CLAIMS_2,
                   WRFILE.WR_NO_AH_CLAIMS_2,
                   WRFILE.WR_NO_FIRE_CLAIMS_2,
                   WRFILE.WR_NO_AUTO_CLAIMS_2,
                   WRFILE.WR_NO_UNEMP_CLAIMS_2,
                   WRFILE.WR_NO_AD_D_CLAIMS_2,
                   WRFILE.WR_NO_L_PROT_CLAIMS_2,
                   WRFILE.WR_NO_NF_NON_BK_CLAIMS_2,
                   WRFILE.WR_NO_NF_BK_CLAIMS_2,
                   WRFILE.WR_NO_ROAD_GARD_CLAIMS_2,
                   WRFILE.WR_LIFE_PREMS_2,
                   WRFILE.WR_AH_PREMS_2,
                   WRFILE.WR_FIRE_PREMS_2,
                   WRFILE.WR_AUTO_PREMS_2,
                   WRFILE.WR_UNEMP_PREMS_2,
                   WRFILE.WR_AD_D_PREMS_2,
                   WRFILE.WR_L_PROT_PREMS_2,
                   WRFILE.WR_NON_FILE_PREMS_2,
                   WRFILE.WR_ROAD_GARD_PREMS_2,
                   WRFILE.WR_NO_LIFE_PREMS_2,
                   WRFILE.WR_NO_AH_PREMS_2,
                   WRFILE.WR_NO_FIRE_PREMS_2,
                   WRFILE.WR_NO_AUTO_PREMS_2,
                   WRFILE.WR_NO_UNEMP_PREMS_2,
                   WRFILE.WR_NO_AD_D_PREMS_2,
                   WRFILE.WR_NO_L_PROT_PREMS_2,
                   WRFILE.WR_NO_NON_FILE_PREMS_2,
                   WRFILE.WR_NO_ROAD_GARD_PREMS_2,
                   WRFILE.WR_LIFE_REFS_2,
                   WRFILE.WR_AH_REFS_2,
                   WRFILE.WR_FIRE_REFS_2,
                   WRFILE.WR_AUTO_REFS_2,
                   WRFILE.WR_UNEMP_REFS_2,
                   WRFILE.WR_AD_D_REFS_2,
                   WRFILE.WR_L_PROT_REFS_2,
                   WRFILE.WR_NON_FILE_REFS_2,
                   WRFILE.WR_ROAD_GARD_REFS_2,
                   WRFILE.WR_NF_RECOVERIES_2,
                   WRFILE.WR_NONFILE_ENDING_BALANCE,
                   WRFILE.WR_NO_CUST_1_YR_AGO,
                   WRFILE.WR_OUTS_BAL_1_YR_AGO,
                   WRFILE.WR_WCBC_NO_CUST_1_YR_AGO,
                   WRFILE.WR_WCBC_OUTS_BAL_1_YR_AGO,
                   WRFILE.WR_LARGE_NO_CUST_1_YR_AGO,
                   WRFILE.WR_LARGE_OUTS_BAL_1_YR_AGO,
                   WRFILE.WR_CUR_NO_CONT_SLOW,
                   WRFILE.WR_WCBC_CUR_NO_CONT_SLOW,
                   WRFILE.WR_LARGE_CUR_NO_CONT_SLOW,
                   WRFILE.WR_CASHIER_BONUS,
                   WRFILE.WR_ASST_MGR_BONUS,
                   WRFILE.WR_MGR_BONUS,
                   WRFILE.WR_LOTS_NF_CLAIMS_1,
                   WRFILE.WR_LOTS_NO_NF_CLAIMS_1,
                   WRFILE.WR_LOTS_NON_FILE_PREMS_1,
                   WRFILE.WR_LOTS_NO_NON_FILE_PREMS_1,
                   WRFILE.WR_LOTS_NF_RECOVERIES_1,
                   WRFILE.WR_LOTS_NF_CLAIMS_2,
                   WRFILE.WR_LOTS_NO_NF_CLAIMS_2,
                   WRFILE.WR_LOTS_NON_FILE_PREMS_2,
                   WRFILE.WR_LOTS_NO_NON_FILE_PREMS_2,
                   WRFILE.WR_LOTS_NF_RECOVERIES_2,
                   WRFILE.WR_PRETAX_PROFIT_1,
                   WRFILE.WR_SUPERVISION_1,
                   WRFILE.WR_PRETAX_PROFIT_2,
                   WRFILE.WR_SUPERVISION_2,
                   ISNULL(WRFILE.WR_MGR_PENALTY_FG,''),
                   WRFILE.WR_NO_TAX_RETURNS_1,
                   WRFILE.WR_NO_TAX_RETURNS_2
            INTO :QWR-REC
            FROM DBO.WRFILE
            WHERE WRFILE.WR_YYYYMM = :QWR-YYYYMM
              AND WRFILE.WR_BRNO = :QWR-BRNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-WR1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-WR-FIELDS.

      *-----------------------------------------------------------------
       READ-WR1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE WR-SELECT-PATH TO E-FILE.
           MOVE WR1-KEY TO E-KEYX.
           INITIALIZE QWR-REC.

           IF ( WR1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH WR1_CURSOR INTO :QWR-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-WR1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-WR-FIELDS.

      *-----------------------------------------------------------------
       WRITE-WR1-FILE.

           ACCEPT WR-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE WR-SELECT-PATH TO E-FILE.
           MOVE WR1-KEY TO E-KEYX.
           PERFORM SET-WR-FIELDS.

           EXEC SQL
            INSERT INTO DBO.WRFILE
             (WRFILE.WR_YYYYMM,
              WRFILE.WR_BRNO,
              WRFILE.WR_LTOUCH_DATE,
              WRFILE.WR_CUR_NO_CUST,
              WRFILE.WR_ELM_NO_CUST,
              WRFILE.WR_ELY_NO_CUST,
              WRFILE.WR_OUTS_BALANCE,
              WRFILE.WR_ELM_OUTS_BALANCE,
              WRFILE.WR_ELY_OUTS_BALANCE,
              WRFILE.WR_NO_NB_1,
              WRFILE.WR_NO_FB_1,
              WRFILE.WR_NO_PB_1,
              WRFILE.WR_GROSS_VOLUME_1,
              WRFILE.WR_BSR_BONUS_1,
              WRFILE.WR_INTEREST_ALLOCATION_1,
              WRFILE.WR_OLD_SUPERVISION_1,
              WRFILE.WR_GEN_RESERVE_ADJ_1,
              WRFILE.WR_INTANGIBLE_AMORT_1,
              WRFILE.WR_ROBBERIES_1,
              WRFILE.WR_RATING_SHEET_INCOME_1,
              WRFILE.WR_INT_FEE_INCOME_1,
              WRFILE.WR_INSURANCE_INCOME_1,
              WRFILE.WR_RATING_SHEET_EXPENSES_1,
              WRFILE.WR_PERSONNEL_EXPENSE_1,
              WRFILE.WR_CHARGEOFF_EXPENSE_1,
              WRFILE.WR_NO_CHARGEOFFS_1,
              WRFILE.WR_RECOVERY_INCOME_1,
              WRFILE.WR_BAD_DEBT_INCREASE_1,
              WRFILE.WR_NO_NB_2,
              WRFILE.WR_NO_FB_2,
              WRFILE.WR_NO_PB_2,
              WRFILE.WR_GROSS_VOLUME_2,
              WRFILE.WR_BSR_BONUS_2,
              WRFILE.WR_INTEREST_ALLOCATION_2,
              WRFILE.WR_OLD_SUPERVISION_2,
              WRFILE.WR_GEN_RESERVE_ADJ_2,
              WRFILE.WR_INTANGIBLE_AMORT_2,
              WRFILE.WR_ROBBERIES_2,
              WRFILE.WR_RATING_SHEET_INCOME_2,
              WRFILE.WR_INT_FEE_INCOME_2,
              WRFILE.WR_INSURANCE_INCOME_2,
              WRFILE.WR_RATING_SHEET_EXPENSES_2,
              WRFILE.WR_PERSONNEL_EXPENSE_2,
              WRFILE.WR_CHARGEOFF_EXPENSE_2,
              WRFILE.WR_NO_CHARGEOFFS_2,
              WRFILE.WR_RECOVERY_INCOME_2,
              WRFILE.WR_BAD_DEBT_INCREASE_2,
              WRFILE.WR_NO_30_DAY_ACCTS,
              WRFILE.WR_AMT_30_DAY_ACCTS,
              WRFILE.WR_NO_60_DAY_ACCTS,
              WRFILE.WR_AMT_60_DAY_ACCTS,
              WRFILE.WR_NO_90_DAY_ACCTS,
              WRFILE.WR_AMT_90_DAY_ACCTS,
              WRFILE.WR_ELM_AMT_90_DAY_ACCTS,
              WRFILE.WR_ELY_AMT_90_DAY_ACCTS,
              WRFILE.WR_YTD_PCT_TURNOVER,
              WRFILE.WR_YTD_PCT_RATE_SHT_PROFIT,
              WRFILE.WR_YTD_PCT_RS_INC_LEDGER,
              WRFILE.WR_YTD_PCT_INT_FEE_LEDGER,
              WRFILE.WR_YTD_PCT_INS_INC_LEDGER,
              WRFILE.WR_YTD_PCT_EXPENSE_LEDGER,
              WRFILE.WR_YTD_PCT_BAD_DEBT_LEDGER,
              WRFILE.WR_YTD_PCT_NF_BNK_LEDGER,
              WRFILE.WR_YTD_PCT_PRETAX_LEDGER,
              WRFILE.WR_YTD_PCT_DEBT_NF_LEDGER,
              WRFILE.WR_WCBC_CUR_NO_CUST,
              WRFILE.WR_WCBC_ELM_NO_CUST,
              WRFILE.WR_WCBC_ELY_NO_CUST,
              WRFILE.WR_WCBC_OUTS_BALANCE,
              WRFILE.WR_WCBC_ELM_OUTS_BAL,
              WRFILE.WR_WCBC_ELY_OUTS_BAL,
              WRFILE.WR_WCBC_NO_NB_1,
              WRFILE.WR_WCBC_NO_FB_1,
              WRFILE.WR_WCBC_NO_PB_1,
              WRFILE.WR_WCBC_GROSS_VOLUME_1,
              WRFILE.WR_WCBC_LATE_CHGS_1,
              WRFILE.WR_WCBC_NET_FIN_CHGS_1,
              WRFILE.WR_WCBC_SALES_1,
              WRFILE.WR_WCBC_COST_OF_SALES_1,
              WRFILE.WR_WCBC_EXPENSES_1,
              WRFILE.WR_WCBC_RECOVERY_INCOME_1,
              WRFILE.WR_WCBC_CHARGEOFF_EXPENSE_1,
              WRFILE.WR_WCBC_BAD_DEBT_BNKRPCY_1,
              WRFILE.WR_WCBC_NO_NB_2,
              WRFILE.WR_WCBC_NO_FB_2,
              WRFILE.WR_WCBC_NO_PB_2,
              WRFILE.WR_WCBC_GROSS_VOLUME_2,
              WRFILE.WR_WCBC_LATE_CHGS_2,
              WRFILE.WR_WCBC_NET_FIN_CHGS_2,
              WRFILE.WR_WCBC_SALES_2,
              WRFILE.WR_WCBC_COST_OF_SALES_2,
              WRFILE.WR_WCBC_EXPENSES_2,
              WRFILE.WR_WCBC_RECOVERY_INCOME_2,
              WRFILE.WR_WCBC_CHARGEOFF_EXPENSE_2,
              WRFILE.WR_WCBC_BAD_DEBT_BNKRPCY_2,
              WRFILE.WR_WCBC_NO_30_DAY_ACCTS,
              WRFILE.WR_WCBC_AMT_30_DAY_ACCTS,
              WRFILE.WR_WCBC_NO_60_DAY_ACCTS,
              WRFILE.WR_WCBC_AMT_60_DAY_ACCTS,
              WRFILE.WR_WCBC_NO_90_DAY_ACCTS,
              WRFILE.WR_WCBC_AMT_90_DAY_ACCTS,
              WRFILE.WR_WCBC_ELM_AMT_90_DAYS,
              WRFILE.WR_WCBC_ELY_AMT_90_DAYS,
              WRFILE.WR_WCBC_PCT_VOLUME,
              WRFILE.WR_WCBC_GROSS_PROF_PCT,
              WRFILE.WR_LARGE_CUR_NO_CUST,
              WRFILE.WR_LARGE_ELM_NO_CUST,
              WRFILE.WR_LARGE_ELY_NO_CUST,
              WRFILE.WR_LARGE_OUTS_BALANCE,
              WRFILE.WR_LARGE_ELM_OUTS_BAL,
              WRFILE.WR_LARGE_ELY_OUTS_BAL,
              WRFILE.WR_LARGE_NO_NB_1,
              WRFILE.WR_LARGE_NO_FB_1,
              WRFILE.WR_LARGE_NO_PB_1,
              WRFILE.WR_LARGE_GROSS_VOLUME_1,
              WRFILE.WR_LARGE_NO_NB_2,
              WRFILE.WR_LARGE_NO_FB_2,
              WRFILE.WR_LARGE_NO_PB_2,
              WRFILE.WR_LARGE_GROSS_VOLUME_2,
              WRFILE.WR_LARGE_NO_30_DAY_ACCTS,
              WRFILE.WR_LARGE_AMT_30_DAY_ACCTS,
              WRFILE.WR_LARGE_NO_60_DAY_ACCTS,
              WRFILE.WR_LARGE_AMT_60_DAY_ACCTS,
              WRFILE.WR_LARGE_NO_90_DAY_ACCTS,
              WRFILE.WR_LARGE_AMT_90_DAY_ACCTS,
              WRFILE.WR_LARGE_ELM_AMT_90_DAYS,
              WRFILE.WR_LARGE_ELY_AMT_90_DAYS,
              WRFILE.WR_LIFE_CLAIMS_1,
              WRFILE.WR_AH_CLAIMS_1,
              WRFILE.WR_FIRE_CLAIMS_1,
              WRFILE.WR_AUTO_CLAIMS_1,
              WRFILE.WR_UNEMP_CLAIMS_1,
              WRFILE.WR_AD_D_CLAIMS_1,
              WRFILE.WR_L_PROT_CLAIMS_1,
              WRFILE.WR_NF_NON_BK_CLAIMS_1,
              WRFILE.WR_NF_BK_CLAIMS_1,
              WRFILE.WR_ROAD_GARD_CLAIMS_1,
              WRFILE.WR_NO_LIFE_CLAIMS_1,
              WRFILE.WR_NO_AH_CLAIMS_1,
              WRFILE.WR_NO_FIRE_CLAIMS_1,
              WRFILE.WR_NO_AUTO_CLAIMS_1,
              WRFILE.WR_NO_UNEMP_CLAIMS_1,
              WRFILE.WR_NO_AD_D_CLAIMS_1,
              WRFILE.WR_NO_L_PROT_CLAIMS_1,
              WRFILE.WR_NO_NF_NON_BK_CLAIMS_1,
              WRFILE.WR_NO_NF_BK_CLAIMS_1,
              WRFILE.WR_NO_ROAD_GARD_CLAIMS_1,
              WRFILE.WR_LIFE_PREMS_1,
              WRFILE.WR_AH_PREMS_1,
              WRFILE.WR_FIRE_PREMS_1,
              WRFILE.WR_AUTO_PREMS_1,
              WRFILE.WR_UNEMP_PREMS_1,
              WRFILE.WR_AD_D_PREMS_1,
              WRFILE.WR_L_PROT_PREMS_1,
              WRFILE.WR_NON_FILE_PREMS_1,
              WRFILE.WR_ROAD_GARD_PREMS_1,
              WRFILE.WR_NO_LIFE_PREMS_1,
              WRFILE.WR_NO_AH_PREMS_1,
              WRFILE.WR_NO_FIRE_PREMS_1,
              WRFILE.WR_NO_AUTO_PREMS_1,
              WRFILE.WR_NO_UNEMP_PREMS_1,
              WRFILE.WR_NO_AD_D_PREMS_1,
              WRFILE.WR_NO_L_PROT_PREMS_1,
              WRFILE.WR_NO_NON_FILE_PREMS_1,
              WRFILE.WR_NO_ROAD_GARD_PREMS_1,
              WRFILE.WR_LIFE_REFS_1,
              WRFILE.WR_AH_REFS_1,
              WRFILE.WR_FIRE_REFS_1,
              WRFILE.WR_AUTO_REFS_1,
              WRFILE.WR_UNEMP_REFS_1,
              WRFILE.WR_AD_D_REFS_1,
              WRFILE.WR_L_PROT_REFS_1,
              WRFILE.WR_NON_FILE_REFS_1,
              WRFILE.WR_ROAD_GARD_REFS_1,
              WRFILE.WR_NF_RECOVERIES_1,
              WRFILE.WR_LIFE_CLAIMS_2,
              WRFILE.WR_AH_CLAIMS_2,
              WRFILE.WR_FIRE_CLAIMS_2,
              WRFILE.WR_AUTO_CLAIMS_2,
              WRFILE.WR_UNEMP_CLAIMS_2,
              WRFILE.WR_AD_D_CLAIMS_2,
              WRFILE.WR_L_PROT_CLAIMS_2,
              WRFILE.WR_NF_NON_BK_CLAIMS_2,
              WRFILE.WR_NF_BK_CLAIMS_2,
              WRFILE.WR_ROAD_GARD_CLAIMS_2,
              WRFILE.WR_NO_LIFE_CLAIMS_2,
              WRFILE.WR_NO_AH_CLAIMS_2,
              WRFILE.WR_NO_FIRE_CLAIMS_2,
              WRFILE.WR_NO_AUTO_CLAIMS_2,
              WRFILE.WR_NO_UNEMP_CLAIMS_2,
              WRFILE.WR_NO_AD_D_CLAIMS_2,
              WRFILE.WR_NO_L_PROT_CLAIMS_2,
              WRFILE.WR_NO_NF_NON_BK_CLAIMS_2,
              WRFILE.WR_NO_NF_BK_CLAIMS_2,
              WRFILE.WR_NO_ROAD_GARD_CLAIMS_2,
              WRFILE.WR_LIFE_PREMS_2,
              WRFILE.WR_AH_PREMS_2,
              WRFILE.WR_FIRE_PREMS_2,
              WRFILE.WR_AUTO_PREMS_2,
              WRFILE.WR_UNEMP_PREMS_2,
              WRFILE.WR_AD_D_PREMS_2,
              WRFILE.WR_L_PROT_PREMS_2,
              WRFILE.WR_NON_FILE_PREMS_2,
              WRFILE.WR_ROAD_GARD_PREMS_2,
              WRFILE.WR_NO_LIFE_PREMS_2,
              WRFILE.WR_NO_AH_PREMS_2,
              WRFILE.WR_NO_FIRE_PREMS_2,
              WRFILE.WR_NO_AUTO_PREMS_2,
              WRFILE.WR_NO_UNEMP_PREMS_2,
              WRFILE.WR_NO_AD_D_PREMS_2,
              WRFILE.WR_NO_L_PROT_PREMS_2,
              WRFILE.WR_NO_NON_FILE_PREMS_2,
              WRFILE.WR_NO_ROAD_GARD_PREMS_2,
              WRFILE.WR_LIFE_REFS_2,
              WRFILE.WR_AH_REFS_2,
              WRFILE.WR_FIRE_REFS_2,
              WRFILE.WR_AUTO_REFS_2,
              WRFILE.WR_UNEMP_REFS_2,
              WRFILE.WR_AD_D_REFS_2,
              WRFILE.WR_L_PROT_REFS_2,
              WRFILE.WR_NON_FILE_REFS_2,
              WRFILE.WR_ROAD_GARD_REFS_2,
              WRFILE.WR_NF_RECOVERIES_2,
              WRFILE.WR_NONFILE_ENDING_BALANCE,
              WRFILE.WR_NO_CUST_1_YR_AGO,
              WRFILE.WR_OUTS_BAL_1_YR_AGO,
              WRFILE.WR_WCBC_NO_CUST_1_YR_AGO,
              WRFILE.WR_WCBC_OUTS_BAL_1_YR_AGO,
              WRFILE.WR_LARGE_NO_CUST_1_YR_AGO,
              WRFILE.WR_LARGE_OUTS_BAL_1_YR_AGO,
              WRFILE.WR_CUR_NO_CONT_SLOW,
              WRFILE.WR_WCBC_CUR_NO_CONT_SLOW,
              WRFILE.WR_LARGE_CUR_NO_CONT_SLOW,
              WRFILE.WR_CASHIER_BONUS,
              WRFILE.WR_ASST_MGR_BONUS,
              WRFILE.WR_MGR_BONUS,
              WRFILE.WR_LOTS_NF_CLAIMS_1,
              WRFILE.WR_LOTS_NO_NF_CLAIMS_1,
              WRFILE.WR_LOTS_NON_FILE_PREMS_1,
              WRFILE.WR_LOTS_NO_NON_FILE_PREMS_1,
              WRFILE.WR_LOTS_NF_RECOVERIES_1,
              WRFILE.WR_LOTS_NF_CLAIMS_2,
              WRFILE.WR_LOTS_NO_NF_CLAIMS_2,
              WRFILE.WR_LOTS_NON_FILE_PREMS_2,
              WRFILE.WR_LOTS_NO_NON_FILE_PREMS_2,
              WRFILE.WR_LOTS_NF_RECOVERIES_2,
              WRFILE.WR_PRETAX_PROFIT_1,
              WRFILE.WR_SUPERVISION_1,
              WRFILE.WR_PRETAX_PROFIT_2,
              WRFILE.WR_SUPERVISION_2,
              WRFILE.WR_MGR_PENALTY_FG,
              WRFILE.WR_NO_TAX_RETURNS_1,
              WRFILE.WR_NO_TAX_RETURNS_2)
            VALUES
             (:QWR-YYYYMM,
              :QWR-BRNO,
              NULLIF(:QWR-LTOUCH-DATE,'1900-01-01'),
              :QWR-CUR-NO-CUST,
              :QWR-ELM-NO-CUST,
              :QWR-ELY-NO-CUST,
              :QWR-OUTS-BALANCE,
              :QWR-ELM-OUTS-BALANCE,
              :QWR-ELY-OUTS-BALANCE,
              :QWR-NO-NB-1,
              :QWR-NO-FB-1,
              :QWR-NO-PB-1,
              :QWR-GROSS-VOLUME-1,
              :QWR-BSR-BONUS-1,
              :QWR-INTEREST-ALLOCATION-1,
              :QWR-OLD-SUPERVISION-1,
              :QWR-GEN-RESERVE-ADJ-1,
              :QWR-INTANGIBLE-AMORT-1,
              :QWR-ROBBERIES-1,
              :QWR-RATING-SHEET-INCOME-1,
              :QWR-INT-FEE-INCOME-1,
              :QWR-INSURANCE-INCOME-1,
              :QWR-RATING-SHEET-EXPENSES-1,
              :QWR-PERSONNEL-EXPENSE-1,
              :QWR-CHARGEOFF-EXPENSE-1,
              :QWR-NO-CHARGEOFFS-1,
              :QWR-RECOVERY-INCOME-1,
              :QWR-BAD-DEBT-INCREASE-1,
              :QWR-NO-NB-2,
              :QWR-NO-FB-2,
              :QWR-NO-PB-2,
              :QWR-GROSS-VOLUME-2,
              :QWR-BSR-BONUS-2,
              :QWR-INTEREST-ALLOCATION-2,
              :QWR-OLD-SUPERVISION-2,
              :QWR-GEN-RESERVE-ADJ-2,
              :QWR-INTANGIBLE-AMORT-2,
              :QWR-ROBBERIES-2,
              :QWR-RATING-SHEET-INCOME-2,
              :QWR-INT-FEE-INCOME-2,
              :QWR-INSURANCE-INCOME-2,
              :QWR-RATING-SHEET-EXPENSES-2,
              :QWR-PERSONNEL-EXPENSE-2,
              :QWR-CHARGEOFF-EXPENSE-2,
              :QWR-NO-CHARGEOFFS-2,
              :QWR-RECOVERY-INCOME-2,
              :QWR-BAD-DEBT-INCREASE-2,
              :QWR-NO-30-DAY-ACCTS,
              :QWR-AMT-30-DAY-ACCTS,
              :QWR-NO-60-DAY-ACCTS,
              :QWR-AMT-60-DAY-ACCTS,
              :QWR-NO-90-DAY-ACCTS,
              :QWR-AMT-90-DAY-ACCTS,
              :QWR-ELM-AMT-90-DAY-ACCTS,
              :QWR-ELY-AMT-90-DAY-ACCTS,
              :QWR-YTD-PCT-TURNOVER,
              :QWR-YTD-PCT-RATE-SHT-PROFIT,
              :QWR-YTD-PCT-RS-INC-LEDGER,
              :QWR-YTD-PCT-INT-FEE-LEDGER,
              :QWR-YTD-PCT-INS-INC-LEDGER,
              :QWR-YTD-PCT-EXPENSE-LEDGER,
              :QWR-YTD-PCT-BAD-DEBT-LEDGER,
              :QWR-YTD-PCT-NF-BNK-LEDGER,
              :QWR-YTD-PCT-PRETAX-LEDGER,
              :QWR-YTD-PCT-DEBT-NF-LEDGER,
              :QWR-WCBC-CUR-NO-CUST,
              :QWR-WCBC-ELM-NO-CUST,
              :QWR-WCBC-ELY-NO-CUST,
              :QWR-WCBC-OUTS-BALANCE,
              :QWR-WCBC-ELM-OUTS-BAL,
              :QWR-WCBC-ELY-OUTS-BAL,
              :QWR-WCBC-NO-NB-1,
              :QWR-WCBC-NO-FB-1,
              :QWR-WCBC-NO-PB-1,
              :QWR-WCBC-GROSS-VOLUME-1,
              :QWR-WCBC-LATE-CHGS-1,
              :QWR-WCBC-NET-FIN-CHGS-1,
              :QWR-WCBC-SALES-1,
              :QWR-WCBC-COST-OF-SALES-1,
              :QWR-WCBC-EXPENSES-1,
              :QWR-WCBC-RECOVERY-INCOME-1,
              :QWR-WCBC-CHARGEOFF-EXPENSE-1,
              :QWR-WCBC-BAD-DEBT-BNKRPCY-1,
              :QWR-WCBC-NO-NB-2,
              :QWR-WCBC-NO-FB-2,
              :QWR-WCBC-NO-PB-2,
              :QWR-WCBC-GROSS-VOLUME-2,
              :QWR-WCBC-LATE-CHGS-2,
              :QWR-WCBC-NET-FIN-CHGS-2,
              :QWR-WCBC-SALES-2,
              :QWR-WCBC-COST-OF-SALES-2,
              :QWR-WCBC-EXPENSES-2,
              :QWR-WCBC-RECOVERY-INCOME-2,
              :QWR-WCBC-CHARGEOFF-EXPENSE-2,
              :QWR-WCBC-BAD-DEBT-BNKRPCY-2,
              :QWR-WCBC-NO-30-DAY-ACCTS,
              :QWR-WCBC-AMT-30-DAY-ACCTS,
              :QWR-WCBC-NO-60-DAY-ACCTS,
              :QWR-WCBC-AMT-60-DAY-ACCTS,
              :QWR-WCBC-NO-90-DAY-ACCTS,
              :QWR-WCBC-AMT-90-DAY-ACCTS,
              :QWR-WCBC-ELM-AMT-90-DAYS,
              :QWR-WCBC-ELY-AMT-90-DAYS,
              :QWR-WCBC-PCT-VOLUME,
              :QWR-WCBC-GROSS-PROF-PCT,
              :QWR-LARGE-CUR-NO-CUST,
              :QWR-LARGE-ELM-NO-CUST,
              :QWR-LARGE-ELY-NO-CUST,
              :QWR-LARGE-OUTS-BALANCE,
              :QWR-LARGE-ELM-OUTS-BAL,
              :QWR-LARGE-ELY-OUTS-BAL,
              :QWR-LARGE-NO-NB-1,
              :QWR-LARGE-NO-FB-1,
              :QWR-LARGE-NO-PB-1,
              :QWR-LARGE-GROSS-VOLUME-1,
              :QWR-LARGE-NO-NB-2,
              :QWR-LARGE-NO-FB-2,
              :QWR-LARGE-NO-PB-2,
              :QWR-LARGE-GROSS-VOLUME-2,
              :QWR-LARGE-NO-30-DAY-ACCTS,
              :QWR-LARGE-AMT-30-DAY-ACCTS,
              :QWR-LARGE-NO-60-DAY-ACCTS,
              :QWR-LARGE-AMT-60-DAY-ACCTS,
              :QWR-LARGE-NO-90-DAY-ACCTS,
              :QWR-LARGE-AMT-90-DAY-ACCTS,
              :QWR-LARGE-ELM-AMT-90-DAYS,
              :QWR-LARGE-ELY-AMT-90-DAYS,
              :QWR-LIFE-CLAIMS-1,
              :QWR-AH-CLAIMS-1,
              :QWR-FIRE-CLAIMS-1,
              :QWR-AUTO-CLAIMS-1,
              :QWR-UNEMP-CLAIMS-1,
              :QWR-AD-D-CLAIMS-1,
              :QWR-L-PROT-CLAIMS-1,
              :QWR-NF-NON-BK-CLAIMS-1,
              :QWR-NF-BK-CLAIMS-1,
              :QWR-ROAD-GARD-CLAIMS-1,
              :QWR-NO-LIFE-CLAIMS-1,
              :QWR-NO-AH-CLAIMS-1,
              :QWR-NO-FIRE-CLAIMS-1,
              :QWR-NO-AUTO-CLAIMS-1,
              :QWR-NO-UNEMP-CLAIMS-1,
              :QWR-NO-AD-D-CLAIMS-1,
              :QWR-NO-L-PROT-CLAIMS-1,
              :QWR-NO-NF-NON-BK-CLAIMS-1,
              :QWR-NO-NF-BK-CLAIMS-1,
              :QWR-NO-ROAD-GARD-CLAIMS-1,
              :QWR-LIFE-PREMS-1,
              :QWR-AH-PREMS-1,
              :QWR-FIRE-PREMS-1,
              :QWR-AUTO-PREMS-1,
              :QWR-UNEMP-PREMS-1,
              :QWR-AD-D-PREMS-1,
              :QWR-L-PROT-PREMS-1,
              :QWR-NON-FILE-PREMS-1,
              :QWR-ROAD-GARD-PREMS-1,
              :QWR-NO-LIFE-PREMS-1,
              :QWR-NO-AH-PREMS-1,
              :QWR-NO-FIRE-PREMS-1,
              :QWR-NO-AUTO-PREMS-1,
              :QWR-NO-UNEMP-PREMS-1,
              :QWR-NO-AD-D-PREMS-1,
              :QWR-NO-L-PROT-PREMS-1,
              :QWR-NO-NON-FILE-PREMS-1,
              :QWR-NO-ROAD-GARD-PREMS-1,
              :QWR-LIFE-REFS-1,
              :QWR-AH-REFS-1,
              :QWR-FIRE-REFS-1,
              :QWR-AUTO-REFS-1,
              :QWR-UNEMP-REFS-1,
              :QWR-AD-D-REFS-1,
              :QWR-L-PROT-REFS-1,
              :QWR-NON-FILE-REFS-1,
              :QWR-ROAD-GARD-REFS-1,
              :QWR-NF-RECOVERIES-1,
              :QWR-LIFE-CLAIMS-2,
              :QWR-AH-CLAIMS-2,
              :QWR-FIRE-CLAIMS-2,
              :QWR-AUTO-CLAIMS-2,
              :QWR-UNEMP-CLAIMS-2,
              :QWR-AD-D-CLAIMS-2,
              :QWR-L-PROT-CLAIMS-2,
              :QWR-NF-NON-BK-CLAIMS-2,
              :QWR-NF-BK-CLAIMS-2,
              :QWR-ROAD-GARD-CLAIMS-2,
              :QWR-NO-LIFE-CLAIMS-2,
              :QWR-NO-AH-CLAIMS-2,
              :QWR-NO-FIRE-CLAIMS-2,
              :QWR-NO-AUTO-CLAIMS-2,
              :QWR-NO-UNEMP-CLAIMS-2,
              :QWR-NO-AD-D-CLAIMS-2,
              :QWR-NO-L-PROT-CLAIMS-2,
              :QWR-NO-NF-NON-BK-CLAIMS-2,
              :QWR-NO-NF-BK-CLAIMS-2,
              :QWR-NO-ROAD-GARD-CLAIMS-2,
              :QWR-LIFE-PREMS-2,
              :QWR-AH-PREMS-2,
              :QWR-FIRE-PREMS-2,
              :QWR-AUTO-PREMS-2,
              :QWR-UNEMP-PREMS-2,
              :QWR-AD-D-PREMS-2,
              :QWR-L-PROT-PREMS-2,
              :QWR-NON-FILE-PREMS-2,
              :QWR-ROAD-GARD-PREMS-2,
              :QWR-NO-LIFE-PREMS-2,
              :QWR-NO-AH-PREMS-2,
              :QWR-NO-FIRE-PREMS-2,
              :QWR-NO-AUTO-PREMS-2,
              :QWR-NO-UNEMP-PREMS-2,
              :QWR-NO-AD-D-PREMS-2,
              :QWR-NO-L-PROT-PREMS-2,
              :QWR-NO-NON-FILE-PREMS-2,
              :QWR-NO-ROAD-GARD-PREMS-2,
              :QWR-LIFE-REFS-2,
              :QWR-AH-REFS-2,
              :QWR-FIRE-REFS-2,
              :QWR-AUTO-REFS-2,
              :QWR-UNEMP-REFS-2,
              :QWR-AD-D-REFS-2,
              :QWR-L-PROT-REFS-2,
              :QWR-NON-FILE-REFS-2,
              :QWR-ROAD-GARD-REFS-2,
              :QWR-NF-RECOVERIES-2,
              :QWR-NONFILE-ENDING-BALANCE,
              :QWR-NO-CUST-1-YR-AGO,
              :QWR-OUTS-BAL-1-YR-AGO,
              :QWR-WCBC-NO-CUST-1-YR-AGO,
              :QWR-WCBC-OUTS-BAL-1-YR-AGO,
              :QWR-LARGE-NO-CUST-1-YR-AGO,
              :QWR-LARGE-OUTS-BAL-1-YR-AGO,
              :QWR-CUR-NO-CONT-SLOW,
              :QWR-WCBC-CUR-NO-CONT-SLOW,
              :QWR-LARGE-CUR-NO-CONT-SLOW,
              :QWR-CASHIER-BONUS,
              :QWR-ASST-MGR-BONUS,
              :QWR-MGR-BONUS,
              :QWR-LOTS-NF-CLAIMS-1,
              :QWR-LOTS-NO-NF-CLAIMS-1,
              :QWR-LOTS-NON-FILE-PREMS-1,
              :QWR-LOTS-NO-NON-FILE-PREMS-1,
              :QWR-LOTS-NF-RECOVERIES-1,
              :QWR-LOTS-NF-CLAIMS-2,
              :QWR-LOTS-NO-NF-CLAIMS-2,
              :QWR-LOTS-NON-FILE-PREMS-2,
              :QWR-LOTS-NO-NON-FILE-PREMS-2,
              :QWR-LOTS-NF-RECOVERIES-2,
              :QWR-PRETAX-PROFIT-1,
              :QWR-SUPERVISION-1,
              :QWR-PRETAX-PROFIT-2,
              :QWR-SUPERVISION-2,
              :QWR-MGR-PENALTY-FG,
              :QWR-NO-TAX-RETURNS-1,
              :QWR-NO-TAX-RETURNS-2)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-WR1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-WR1-FILE.

           ACCEPT WR-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE WR-SELECT-PATH TO E-FILE.
           MOVE WR1-KEY TO E-KEYX.
           PERFORM SET-WR-FIELDS.

           EXEC SQL
            UPDATE DBO.WRFILE
             SET
              WRFILE.WR_YYYYMM = :QWR-YYYYMM,
              WRFILE.WR_BRNO = :QWR-BRNO,
              WRFILE.WR_LTOUCH_DATE =
                 NULLIF(:QWR-LTOUCH-DATE,'1900-01-01'),
              WRFILE.WR_CUR_NO_CUST = :QWR-CUR-NO-CUST,
              WRFILE.WR_ELM_NO_CUST = :QWR-ELM-NO-CUST,
              WRFILE.WR_ELY_NO_CUST = :QWR-ELY-NO-CUST,
              WRFILE.WR_OUTS_BALANCE = :QWR-OUTS-BALANCE,
              WRFILE.WR_ELM_OUTS_BALANCE = :QWR-ELM-OUTS-BALANCE,
              WRFILE.WR_ELY_OUTS_BALANCE = :QWR-ELY-OUTS-BALANCE,
              WRFILE.WR_NO_NB_1 = :QWR-NO-NB-1,
              WRFILE.WR_NO_FB_1 = :QWR-NO-FB-1,
              WRFILE.WR_NO_PB_1 = :QWR-NO-PB-1,
              WRFILE.WR_GROSS_VOLUME_1 = :QWR-GROSS-VOLUME-1,
              WRFILE.WR_BSR_BONUS_1 = :QWR-BSR-BONUS-1,
              WRFILE.WR_INTEREST_ALLOCATION_1 =
                   :QWR-INTEREST-ALLOCATION-1,
              WRFILE.WR_OLD_SUPERVISION_1 =
                   :QWR-OLD-SUPERVISION-1,
              WRFILE.WR_GEN_RESERVE_ADJ_1 =
                   :QWR-GEN-RESERVE-ADJ-1,
              WRFILE.WR_INTANGIBLE_AMORT_1 =
                   :QWR-INTANGIBLE-AMORT-1,
              WRFILE.WR_ROBBERIES_1 = :QWR-ROBBERIES-1,
              WRFILE.WR_RATING_SHEET_INCOME_1 =
                   :QWR-RATING-SHEET-INCOME-1,
              WRFILE.WR_INT_FEE_INCOME_1 = :QWR-INT-FEE-INCOME-1,
              WRFILE.WR_INSURANCE_INCOME_1 =
                   :QWR-INSURANCE-INCOME-1,
              WRFILE.WR_RATING_SHEET_EXPENSES_1 =
                   :QWR-RATING-SHEET-EXPENSES-1,
              WRFILE.WR_PERSONNEL_EXPENSE_1 =
                   :QWR-PERSONNEL-EXPENSE-1,
              WRFILE.WR_CHARGEOFF_EXPENSE_1 =
                   :QWR-CHARGEOFF-EXPENSE-1,
              WRFILE.WR_NO_CHARGEOFFS_1 = :QWR-NO-CHARGEOFFS-1,
              WRFILE.WR_RECOVERY_INCOME_1 =
                   :QWR-RECOVERY-INCOME-1,
              WRFILE.WR_BAD_DEBT_INCREASE_1 =
                   :QWR-BAD-DEBT-INCREASE-1,
              WRFILE.WR_NO_NB_2 = :QWR-NO-NB-2,
              WRFILE.WR_NO_FB_2 = :QWR-NO-FB-2,
              WRFILE.WR_NO_PB_2 = :QWR-NO-PB-2,
              WRFILE.WR_GROSS_VOLUME_2 = :QWR-GROSS-VOLUME-2,
              WRFILE.WR_BSR_BONUS_2 = :QWR-BSR-BONUS-2,
              WRFILE.WR_INTEREST_ALLOCATION_2 =
                   :QWR-INTEREST-ALLOCATION-2,
              WRFILE.WR_OLD_SUPERVISION_2 =
                   :QWR-OLD-SUPERVISION-2,
              WRFILE.WR_GEN_RESERVE_ADJ_2 =
                   :QWR-GEN-RESERVE-ADJ-2,
              WRFILE.WR_INTANGIBLE_AMORT_2 =
                   :QWR-INTANGIBLE-AMORT-2,
              WRFILE.WR_ROBBERIES_2 = :QWR-ROBBERIES-2,
              WRFILE.WR_RATING_SHEET_INCOME_2 =
                   :QWR-RATING-SHEET-INCOME-2,
              WRFILE.WR_INT_FEE_INCOME_2 = :QWR-INT-FEE-INCOME-2,
              WRFILE.WR_INSURANCE_INCOME_2 =
                   :QWR-INSURANCE-INCOME-2,
              WRFILE.WR_RATING_SHEET_EXPENSES_2 =
                   :QWR-RATING-SHEET-EXPENSES-2,
              WRFILE.WR_PERSONNEL_EXPENSE_2 =
                   :QWR-PERSONNEL-EXPENSE-2,
              WRFILE.WR_CHARGEOFF_EXPENSE_2 =
                   :QWR-CHARGEOFF-EXPENSE-2,
              WRFILE.WR_NO_CHARGEOFFS_2 = :QWR-NO-CHARGEOFFS-2,
              WRFILE.WR_RECOVERY_INCOME_2 =
                   :QWR-RECOVERY-INCOME-2,
              WRFILE.WR_BAD_DEBT_INCREASE_2 =
                   :QWR-BAD-DEBT-INCREASE-2,
              WRFILE.WR_NO_30_DAY_ACCTS = :QWR-NO-30-DAY-ACCTS,
              WRFILE.WR_AMT_30_DAY_ACCTS = :QWR-AMT-30-DAY-ACCTS,
              WRFILE.WR_NO_60_DAY_ACCTS = :QWR-NO-60-DAY-ACCTS,
              WRFILE.WR_AMT_60_DAY_ACCTS = :QWR-AMT-60-DAY-ACCTS,
              WRFILE.WR_NO_90_DAY_ACCTS = :QWR-NO-90-DAY-ACCTS,
              WRFILE.WR_AMT_90_DAY_ACCTS = :QWR-AMT-90-DAY-ACCTS,
              WRFILE.WR_ELM_AMT_90_DAY_ACCTS =
                   :QWR-ELM-AMT-90-DAY-ACCTS,
              WRFILE.WR_ELY_AMT_90_DAY_ACCTS =
                   :QWR-ELY-AMT-90-DAY-ACCTS,
              WRFILE.WR_YTD_PCT_TURNOVER = :QWR-YTD-PCT-TURNOVER,
              WRFILE.WR_YTD_PCT_RATE_SHT_PROFIT =
                   :QWR-YTD-PCT-RATE-SHT-PROFIT,
              WRFILE.WR_YTD_PCT_RS_INC_LEDGER =
                   :QWR-YTD-PCT-RS-INC-LEDGER,
              WRFILE.WR_YTD_PCT_INT_FEE_LEDGER =
                   :QWR-YTD-PCT-INT-FEE-LEDGER,
              WRFILE.WR_YTD_PCT_INS_INC_LEDGER =
                   :QWR-YTD-PCT-INS-INC-LEDGER,
              WRFILE.WR_YTD_PCT_EXPENSE_LEDGER =
                   :QWR-YTD-PCT-EXPENSE-LEDGER,
              WRFILE.WR_YTD_PCT_BAD_DEBT_LEDGER =
                   :QWR-YTD-PCT-BAD-DEBT-LEDGER,
              WRFILE.WR_YTD_PCT_NF_BNK_LEDGER =
                   :QWR-YTD-PCT-NF-BNK-LEDGER,
              WRFILE.WR_YTD_PCT_PRETAX_LEDGER =
                   :QWR-YTD-PCT-PRETAX-LEDGER,
              WRFILE.WR_YTD_PCT_DEBT_NF_LEDGER =
                   :QWR-YTD-PCT-DEBT-NF-LEDGER,
              WRFILE.WR_WCBC_CUR_NO_CUST = :QWR-WCBC-CUR-NO-CUST,
              WRFILE.WR_WCBC_ELM_NO_CUST = :QWR-WCBC-ELM-NO-CUST,
              WRFILE.WR_WCBC_ELY_NO_CUST = :QWR-WCBC-ELY-NO-CUST,
              WRFILE.WR_WCBC_OUTS_BALANCE =
                   :QWR-WCBC-OUTS-BALANCE,
              WRFILE.WR_WCBC_ELM_OUTS_BAL =
                   :QWR-WCBC-ELM-OUTS-BAL,
              WRFILE.WR_WCBC_ELY_OUTS_BAL =
                   :QWR-WCBC-ELY-OUTS-BAL,
              WRFILE.WR_WCBC_NO_NB_1 = :QWR-WCBC-NO-NB-1,
              WRFILE.WR_WCBC_NO_FB_1 = :QWR-WCBC-NO-FB-1,
              WRFILE.WR_WCBC_NO_PB_1 = :QWR-WCBC-NO-PB-1,
              WRFILE.WR_WCBC_GROSS_VOLUME_1 =
                   :QWR-WCBC-GROSS-VOLUME-1,
              WRFILE.WR_WCBC_LATE_CHGS_1 = :QWR-WCBC-LATE-CHGS-1,
              WRFILE.WR_WCBC_NET_FIN_CHGS_1 =
                   :QWR-WCBC-NET-FIN-CHGS-1,
              WRFILE.WR_WCBC_SALES_1 = :QWR-WCBC-SALES-1,
              WRFILE.WR_WCBC_COST_OF_SALES_1 =
                   :QWR-WCBC-COST-OF-SALES-1,
              WRFILE.WR_WCBC_EXPENSES_1 = :QWR-WCBC-EXPENSES-1,
              WRFILE.WR_WCBC_RECOVERY_INCOME_1 =
                   :QWR-WCBC-RECOVERY-INCOME-1,
              WRFILE.WR_WCBC_CHARGEOFF_EXPENSE_1 =
                   :QWR-WCBC-CHARGEOFF-EXPENSE-1,
              WRFILE.WR_WCBC_BAD_DEBT_BNKRPCY_1 =
                   :QWR-WCBC-BAD-DEBT-BNKRPCY-1,
              WRFILE.WR_WCBC_NO_NB_2 = :QWR-WCBC-NO-NB-2,
              WRFILE.WR_WCBC_NO_FB_2 = :QWR-WCBC-NO-FB-2,
              WRFILE.WR_WCBC_NO_PB_2 = :QWR-WCBC-NO-PB-2,
              WRFILE.WR_WCBC_GROSS_VOLUME_2 =
                   :QWR-WCBC-GROSS-VOLUME-2,
              WRFILE.WR_WCBC_LATE_CHGS_2 = :QWR-WCBC-LATE-CHGS-2,
              WRFILE.WR_WCBC_NET_FIN_CHGS_2 =
                   :QWR-WCBC-NET-FIN-CHGS-2,
              WRFILE.WR_WCBC_SALES_2 = :QWR-WCBC-SALES-2,
              WRFILE.WR_WCBC_COST_OF_SALES_2 =
                   :QWR-WCBC-COST-OF-SALES-2,
              WRFILE.WR_WCBC_EXPENSES_2 = :QWR-WCBC-EXPENSES-2,
              WRFILE.WR_WCBC_RECOVERY_INCOME_2 =
                   :QWR-WCBC-RECOVERY-INCOME-2,
              WRFILE.WR_WCBC_CHARGEOFF_EXPENSE_2 =
                   :QWR-WCBC-CHARGEOFF-EXPENSE-2,
              WRFILE.WR_WCBC_BAD_DEBT_BNKRPCY_2 =
                   :QWR-WCBC-BAD-DEBT-BNKRPCY-2,
              WRFILE.WR_WCBC_NO_30_DAY_ACCTS =
                   :QWR-WCBC-NO-30-DAY-ACCTS,
              WRFILE.WR_WCBC_AMT_30_DAY_ACCTS =
                   :QWR-WCBC-AMT-30-DAY-ACCTS,
              WRFILE.WR_WCBC_NO_60_DAY_ACCTS =
                   :QWR-WCBC-NO-60-DAY-ACCTS,
              WRFILE.WR_WCBC_AMT_60_DAY_ACCTS =
                   :QWR-WCBC-AMT-60-DAY-ACCTS,
              WRFILE.WR_WCBC_NO_90_DAY_ACCTS =
                   :QWR-WCBC-NO-90-DAY-ACCTS,
              WRFILE.WR_WCBC_AMT_90_DAY_ACCTS =
                   :QWR-WCBC-AMT-90-DAY-ACCTS,
              WRFILE.WR_WCBC_ELM_AMT_90_DAYS =
                   :QWR-WCBC-ELM-AMT-90-DAYS,
              WRFILE.WR_WCBC_ELY_AMT_90_DAYS =
                   :QWR-WCBC-ELY-AMT-90-DAYS,
              WRFILE.WR_WCBC_PCT_VOLUME = :QWR-WCBC-PCT-VOLUME,
              WRFILE.WR_WCBC_GROSS_PROF_PCT =
                   :QWR-WCBC-GROSS-PROF-PCT,
              WRFILE.WR_LARGE_CUR_NO_CUST =
                   :QWR-LARGE-CUR-NO-CUST,
              WRFILE.WR_LARGE_ELM_NO_CUST =
                   :QWR-LARGE-ELM-NO-CUST,
              WRFILE.WR_LARGE_ELY_NO_CUST =
                   :QWR-LARGE-ELY-NO-CUST,
              WRFILE.WR_LARGE_OUTS_BALANCE =
                   :QWR-LARGE-OUTS-BALANCE,
              WRFILE.WR_LARGE_ELM_OUTS_BAL =
                   :QWR-LARGE-ELM-OUTS-BAL,
              WRFILE.WR_LARGE_ELY_OUTS_BAL =
                   :QWR-LARGE-ELY-OUTS-BAL,
              WRFILE.WR_LARGE_NO_NB_1 = :QWR-LARGE-NO-NB-1,
              WRFILE.WR_LARGE_NO_FB_1 = :QWR-LARGE-NO-FB-1,
              WRFILE.WR_LARGE_NO_PB_1 = :QWR-LARGE-NO-PB-1,
              WRFILE.WR_LARGE_GROSS_VOLUME_1 =
                   :QWR-LARGE-GROSS-VOLUME-1,
              WRFILE.WR_LARGE_NO_NB_2 = :QWR-LARGE-NO-NB-2,
              WRFILE.WR_LARGE_NO_FB_2 = :QWR-LARGE-NO-FB-2,
              WRFILE.WR_LARGE_NO_PB_2 = :QWR-LARGE-NO-PB-2,
              WRFILE.WR_LARGE_GROSS_VOLUME_2 =
                   :QWR-LARGE-GROSS-VOLUME-2,
              WRFILE.WR_LARGE_NO_30_DAY_ACCTS =
                   :QWR-LARGE-NO-30-DAY-ACCTS,
              WRFILE.WR_LARGE_AMT_30_DAY_ACCTS =
                   :QWR-LARGE-AMT-30-DAY-ACCTS,
              WRFILE.WR_LARGE_NO_60_DAY_ACCTS =
                   :QWR-LARGE-NO-60-DAY-ACCTS,
              WRFILE.WR_LARGE_AMT_60_DAY_ACCTS =
                   :QWR-LARGE-AMT-60-DAY-ACCTS,
              WRFILE.WR_LARGE_NO_90_DAY_ACCTS =
                   :QWR-LARGE-NO-90-DAY-ACCTS,
              WRFILE.WR_LARGE_AMT_90_DAY_ACCTS =
                   :QWR-LARGE-AMT-90-DAY-ACCTS,
              WRFILE.WR_LARGE_ELM_AMT_90_DAYS =
                   :QWR-LARGE-ELM-AMT-90-DAYS,
              WRFILE.WR_LARGE_ELY_AMT_90_DAYS =
                   :QWR-LARGE-ELY-AMT-90-DAYS,
              WRFILE.WR_LIFE_CLAIMS_1 = :QWR-LIFE-CLAIMS-1,
              WRFILE.WR_AH_CLAIMS_1 = :QWR-AH-CLAIMS-1,
              WRFILE.WR_FIRE_CLAIMS_1 = :QWR-FIRE-CLAIMS-1,
              WRFILE.WR_AUTO_CLAIMS_1 = :QWR-AUTO-CLAIMS-1,
              WRFILE.WR_UNEMP_CLAIMS_1 = :QWR-UNEMP-CLAIMS-1,
              WRFILE.WR_AD_D_CLAIMS_1 = :QWR-AD-D-CLAIMS-1,
              WRFILE.WR_L_PROT_CLAIMS_1 = :QWR-L-PROT-CLAIMS-1,
              WRFILE.WR_NF_NON_BK_CLAIMS_1 =
                   :QWR-NF-NON-BK-CLAIMS-1,
              WRFILE.WR_NF_BK_CLAIMS_1 = :QWR-NF-BK-CLAIMS-1,
              WRFILE.WR_ROAD_GARD_CLAIMS_1 =
                   :QWR-ROAD-GARD-CLAIMS-1,
              WRFILE.WR_NO_LIFE_CLAIMS_1 = :QWR-NO-LIFE-CLAIMS-1,
              WRFILE.WR_NO_AH_CLAIMS_1 = :QWR-NO-AH-CLAIMS-1,
              WRFILE.WR_NO_FIRE_CLAIMS_1 = :QWR-NO-FIRE-CLAIMS-1,
              WRFILE.WR_NO_AUTO_CLAIMS_1 = :QWR-NO-AUTO-CLAIMS-1,
              WRFILE.WR_NO_UNEMP_CLAIMS_1 =
                   :QWR-NO-UNEMP-CLAIMS-1,
              WRFILE.WR_NO_AD_D_CLAIMS_1 = :QWR-NO-AD-D-CLAIMS-1,
              WRFILE.WR_NO_L_PROT_CLAIMS_1 =
                   :QWR-NO-L-PROT-CLAIMS-1,
              WRFILE.WR_NO_NF_NON_BK_CLAIMS_1 =
                   :QWR-NO-NF-NON-BK-CLAIMS-1,
              WRFILE.WR_NO_NF_BK_CLAIMS_1 =
                   :QWR-NO-NF-BK-CLAIMS-1,
              WRFILE.WR_NO_ROAD_GARD_CLAIMS_1 =
                   :QWR-NO-ROAD-GARD-CLAIMS-1,
              WRFILE.WR_LIFE_PREMS_1 = :QWR-LIFE-PREMS-1,
              WRFILE.WR_AH_PREMS_1 = :QWR-AH-PREMS-1,
              WRFILE.WR_FIRE_PREMS_1 = :QWR-FIRE-PREMS-1,
              WRFILE.WR_AUTO_PREMS_1 = :QWR-AUTO-PREMS-1,
              WRFILE.WR_UNEMP_PREMS_1 = :QWR-UNEMP-PREMS-1,
              WRFILE.WR_AD_D_PREMS_1 = :QWR-AD-D-PREMS-1,
              WRFILE.WR_L_PROT_PREMS_1 = :QWR-L-PROT-PREMS-1,
              WRFILE.WR_NON_FILE_PREMS_1 = :QWR-NON-FILE-PREMS-1,
              WRFILE.WR_ROAD_GARD_PREMS_1 =
                   :QWR-ROAD-GARD-PREMS-1,
              WRFILE.WR_NO_LIFE_PREMS_1 = :QWR-NO-LIFE-PREMS-1,
              WRFILE.WR_NO_AH_PREMS_1 = :QWR-NO-AH-PREMS-1,
              WRFILE.WR_NO_FIRE_PREMS_1 = :QWR-NO-FIRE-PREMS-1,
              WRFILE.WR_NO_AUTO_PREMS_1 = :QWR-NO-AUTO-PREMS-1,
              WRFILE.WR_NO_UNEMP_PREMS_1 = :QWR-NO-UNEMP-PREMS-1,
              WRFILE.WR_NO_AD_D_PREMS_1 = :QWR-NO-AD-D-PREMS-1,
              WRFILE.WR_NO_L_PROT_PREMS_1 =
                   :QWR-NO-L-PROT-PREMS-1,
              WRFILE.WR_NO_NON_FILE_PREMS_1 =
                   :QWR-NO-NON-FILE-PREMS-1,
              WRFILE.WR_NO_ROAD_GARD_PREMS_1 =
                   :QWR-NO-ROAD-GARD-PREMS-1,
              WRFILE.WR_LIFE_REFS_1 = :QWR-LIFE-REFS-1,
              WRFILE.WR_AH_REFS_1 = :QWR-AH-REFS-1,
              WRFILE.WR_FIRE_REFS_1 = :QWR-FIRE-REFS-1,
              WRFILE.WR_AUTO_REFS_1 = :QWR-AUTO-REFS-1,
              WRFILE.WR_UNEMP_REFS_1 = :QWR-UNEMP-REFS-1,
              WRFILE.WR_AD_D_REFS_1 = :QWR-AD-D-REFS-1,
              WRFILE.WR_L_PROT_REFS_1 = :QWR-L-PROT-REFS-1,
              WRFILE.WR_NON_FILE_REFS_1 = :QWR-NON-FILE-REFS-1,
              WRFILE.WR_ROAD_GARD_REFS_1 = :QWR-ROAD-GARD-REFS-1,
              WRFILE.WR_NF_RECOVERIES_1 = :QWR-NF-RECOVERIES-1,
              WRFILE.WR_LIFE_CLAIMS_2 = :QWR-LIFE-CLAIMS-2,
              WRFILE.WR_AH_CLAIMS_2 = :QWR-AH-CLAIMS-2,
              WRFILE.WR_FIRE_CLAIMS_2 = :QWR-FIRE-CLAIMS-2,
              WRFILE.WR_AUTO_CLAIMS_2 = :QWR-AUTO-CLAIMS-2,
              WRFILE.WR_UNEMP_CLAIMS_2 = :QWR-UNEMP-CLAIMS-2,
              WRFILE.WR_AD_D_CLAIMS_2 = :QWR-AD-D-CLAIMS-2,
              WRFILE.WR_L_PROT_CLAIMS_2 = :QWR-L-PROT-CLAIMS-2,
              WRFILE.WR_NF_NON_BK_CLAIMS_2 =
                   :QWR-NF-NON-BK-CLAIMS-2,
              WRFILE.WR_NF_BK_CLAIMS_2 = :QWR-NF-BK-CLAIMS-2,
              WRFILE.WR_ROAD_GARD_CLAIMS_2 =
                   :QWR-ROAD-GARD-CLAIMS-2,
              WRFILE.WR_NO_LIFE_CLAIMS_2 = :QWR-NO-LIFE-CLAIMS-2,
              WRFILE.WR_NO_AH_CLAIMS_2 = :QWR-NO-AH-CLAIMS-2,
              WRFILE.WR_NO_FIRE_CLAIMS_2 = :QWR-NO-FIRE-CLAIMS-2,
              WRFILE.WR_NO_AUTO_CLAIMS_2 = :QWR-NO-AUTO-CLAIMS-2,
              WRFILE.WR_NO_UNEMP_CLAIMS_2 =
                   :QWR-NO-UNEMP-CLAIMS-2,
              WRFILE.WR_NO_AD_D_CLAIMS_2 = :QWR-NO-AD-D-CLAIMS-2,
              WRFILE.WR_NO_L_PROT_CLAIMS_2 =
                   :QWR-NO-L-PROT-CLAIMS-2,
              WRFILE.WR_NO_NF_NON_BK_CLAIMS_2 =
                   :QWR-NO-NF-NON-BK-CLAIMS-2,
              WRFILE.WR_NO_NF_BK_CLAIMS_2 =
                   :QWR-NO-NF-BK-CLAIMS-2,
              WRFILE.WR_NO_ROAD_GARD_CLAIMS_2 =
                   :QWR-NO-ROAD-GARD-CLAIMS-2,
              WRFILE.WR_LIFE_PREMS_2 = :QWR-LIFE-PREMS-2,
              WRFILE.WR_AH_PREMS_2 = :QWR-AH-PREMS-2,
              WRFILE.WR_FIRE_PREMS_2 = :QWR-FIRE-PREMS-2,
              WRFILE.WR_AUTO_PREMS_2 = :QWR-AUTO-PREMS-2,
              WRFILE.WR_UNEMP_PREMS_2 = :QWR-UNEMP-PREMS-2,
              WRFILE.WR_AD_D_PREMS_2 = :QWR-AD-D-PREMS-2,
              WRFILE.WR_L_PROT_PREMS_2 = :QWR-L-PROT-PREMS-2,
              WRFILE.WR_NON_FILE_PREMS_2 = :QWR-NON-FILE-PREMS-2,
              WRFILE.WR_ROAD_GARD_PREMS_2 =
                   :QWR-ROAD-GARD-PREMS-2,
              WRFILE.WR_NO_LIFE_PREMS_2 = :QWR-NO-LIFE-PREMS-2,
              WRFILE.WR_NO_AH_PREMS_2 = :QWR-NO-AH-PREMS-2,
              WRFILE.WR_NO_FIRE_PREMS_2 = :QWR-NO-FIRE-PREMS-2,
              WRFILE.WR_NO_AUTO_PREMS_2 = :QWR-NO-AUTO-PREMS-2,
              WRFILE.WR_NO_UNEMP_PREMS_2 = :QWR-NO-UNEMP-PREMS-2,
              WRFILE.WR_NO_AD_D_PREMS_2 = :QWR-NO-AD-D-PREMS-2,
              WRFILE.WR_NO_L_PROT_PREMS_2 =
                   :QWR-NO-L-PROT-PREMS-2,
              WRFILE.WR_NO_NON_FILE_PREMS_2 =
                   :QWR-NO-NON-FILE-PREMS-2,
              WRFILE.WR_NO_ROAD_GARD_PREMS_2 =
                   :QWR-NO-ROAD-GARD-PREMS-2,
              WRFILE.WR_LIFE_REFS_2 = :QWR-LIFE-REFS-2,
              WRFILE.WR_AH_REFS_2 = :QWR-AH-REFS-2,
              WRFILE.WR_FIRE_REFS_2 = :QWR-FIRE-REFS-2,
              WRFILE.WR_AUTO_REFS_2 = :QWR-AUTO-REFS-2,
              WRFILE.WR_UNEMP_REFS_2 = :QWR-UNEMP-REFS-2,
              WRFILE.WR_AD_D_REFS_2 = :QWR-AD-D-REFS-2,
              WRFILE.WR_L_PROT_REFS_2 = :QWR-L-PROT-REFS-2,
              WRFILE.WR_NON_FILE_REFS_2 = :QWR-NON-FILE-REFS-2,
              WRFILE.WR_ROAD_GARD_REFS_2 = :QWR-ROAD-GARD-REFS-2,
              WRFILE.WR_NF_RECOVERIES_2 = :QWR-NF-RECOVERIES-2,
              WRFILE.WR_NONFILE_ENDING_BALANCE =
                   :QWR-NONFILE-ENDING-BALANCE,
              WRFILE.WR_NO_CUST_1_YR_AGO = :QWR-NO-CUST-1-YR-AGO,
              WRFILE.WR_OUTS_BAL_1_YR_AGO =
                   :QWR-OUTS-BAL-1-YR-AGO,
              WRFILE.WR_WCBC_NO_CUST_1_YR_AGO =
                   :QWR-WCBC-NO-CUST-1-YR-AGO,
              WRFILE.WR_WCBC_OUTS_BAL_1_YR_AGO =
                   :QWR-WCBC-OUTS-BAL-1-YR-AGO,
              WRFILE.WR_LARGE_NO_CUST_1_YR_AGO =
                   :QWR-LARGE-NO-CUST-1-YR-AGO,
              WRFILE.WR_LARGE_OUTS_BAL_1_YR_AGO =
                   :QWR-LARGE-OUTS-BAL-1-YR-AGO,
              WRFILE.WR_CUR_NO_CONT_SLOW = :QWR-CUR-NO-CONT-SLOW,
              WRFILE.WR_WCBC_CUR_NO_CONT_SLOW =
                   :QWR-WCBC-CUR-NO-CONT-SLOW,
              WRFILE.WR_LARGE_CUR_NO_CONT_SLOW =
                   :QWR-LARGE-CUR-NO-CONT-SLOW,
              WRFILE.WR_CASHIER_BONUS = :QWR-CASHIER-BONUS,
              WRFILE.WR_ASST_MGR_BONUS = :QWR-ASST-MGR-BONUS,
              WRFILE.WR_MGR_BONUS = :QWR-MGR-BONUS,
              WRFILE.WR_LOTS_NF_CLAIMS_1 = :QWR-LOTS-NF-CLAIMS-1,
              WRFILE.WR_LOTS_NO_NF_CLAIMS_1 =
                   :QWR-LOTS-NO-NF-CLAIMS-1,
              WRFILE.WR_LOTS_NON_FILE_PREMS_1 =
                   :QWR-LOTS-NON-FILE-PREMS-1,
              WRFILE.WR_LOTS_NO_NON_FILE_PREMS_1 =
                   :QWR-LOTS-NO-NON-FILE-PREMS-1,
              WRFILE.WR_LOTS_NF_RECOVERIES_1 =
                   :QWR-LOTS-NF-RECOVERIES-1,
              WRFILE.WR_LOTS_NF_CLAIMS_2 = :QWR-LOTS-NF-CLAIMS-2,
              WRFILE.WR_LOTS_NO_NF_CLAIMS_2 =
                   :QWR-LOTS-NO-NF-CLAIMS-2,
              WRFILE.WR_LOTS_NON_FILE_PREMS_2 =
                   :QWR-LOTS-NON-FILE-PREMS-2,
              WRFILE.WR_LOTS_NO_NON_FILE_PREMS_2 =
                   :QWR-LOTS-NO-NON-FILE-PREMS-2,
              WRFILE.WR_LOTS_NF_RECOVERIES_2 =
                   :QWR-LOTS-NF-RECOVERIES-2,
              WRFILE.WR_PRETAX_PROFIT_1 = :QWR-PRETAX-PROFIT-1,
              WRFILE.WR_SUPERVISION_1 = :QWR-SUPERVISION-1,
              WRFILE.WR_PRETAX_PROFIT_2 = :QWR-PRETAX-PROFIT-2,
              WRFILE.WR_SUPERVISION_2 = :QWR-SUPERVISION-2,
              WRFILE.WR_MGR_PENALTY_FG = :QWR-MGR-PENALTY-FG,
              WRFILE.WR_NO_TAX_RETURNS_1 = :QWR-NO-TAX-RETURNS-1,
              WRFILE.WR_NO_TAX_RETURNS_2 = :QWR-NO-TAX-RETURNS-2
            WHERE WRFILE.WR_YYYYMM = :QWR-YYYYMM
              AND WRFILE.WR_BRNO = :QWR-BRNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-WR1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-WR1-FILE.

           PERFORM DELETE-IT.
           MOVE WR-SELECT-PATH TO E-FILE.
           MOVE WR1-KEY TO E-KEYX.
           PERFORM SET-WR-FIELDS.

           EXEC SQL
            DELETE FROM DBO.WRFILE
            WHERE WRFILE.WR_YYYYMM = :QWR-YYYYMM
              AND WRFILE.WR_BRNO = :QWR-BRNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-WR1-FILE.

      *-----------------------------------------------------------------
       CLOSE-WR1-FILE.
           IF ( WR1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE WR1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO WR1-CURSOR-STAT.

      *#################################################################
      *    ENDING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################

      *=================================================================
      *=================================================================
      *=================================================================
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
      *
      *    E N D - P R O G R A M
      *
      ******************************************************************
       END-PROGRAM SECTION.
      *-----------------------------------------------------------------
       END-RUN.

           PERFORM CLOSE-FILES.

           IF ( EXIT-PRIOR-TO-EXECUTION )
              GO TO EXIT-PROG.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-ROUTE-BUF = "00" )
              PERFORM PRU-OUT.

           IF ( NO-RECORDS-FOUND         ) AND
              ( EXT-ROUTE-BUF = "00" OR "70" )
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       EXIT-PROG.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

      *-----------------------------------------------------------------
       STOP-RUN.

           IF ( EXT-ROUTE-BUF = "00" OR "40" OR "50" )
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY IOWRIN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
