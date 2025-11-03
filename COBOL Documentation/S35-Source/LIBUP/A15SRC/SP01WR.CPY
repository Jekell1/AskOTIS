      * COPYMEMBER: LIBSP/SP01WR
      ******************************************************************
      *                                                 
      *        WORLD RATING SHEET FILE                  
      *           STATS/GL/MISC                         
      *                                                 
      *                                                 
      * REV:                                            
      *  BLV 030709 GOT RID OF YTD FOR # CUST & LEDGER BALANCE....IT'S THE
      *             SAME AS MONTH-TO-DATE & JUST CONFUSING!
      *  JTG 030908 MOVED '78  WR-SIZE FROM SP01WR AND PLACED IN SPFDWR' 
      *             SO IT COULD BE USED WITH A 'COPY   REPLACING'
      *  BLV 031017 GOT RID OF SOME FIELDS WHICH WERE CALCULATED FROM
      *             OTHER LINES OR PRIOR MONTH, ADDED FIELDS FOR
      *             WCBC RATING SHEET & SOME LARGE LOAN FIELDS
      *  BLV 031119 ADDED ELM FIELDS FOR 90+ BALANCES FOR DIRECT, WCBC,
      *             & LARGE.  TRASHED WCBC-INCR-DECR-90-DAYS.  ADDED
      *             SOME OTHER BASIC LARGE LOAN FIELDS PER JIM/SANDY:
      *             # LOANS FOR NB, FB, PB, VOLUME $, 30-60-90, ETC.
      *  BLV 040112 CHANGED LINE 41 FROM WR-YTD-PCT-NF-NON-BNK-LEDGER
      *             TO WR-YTD-PCT-PRETAX-LEDGER
      *  BLV 040112 DELETE 2 FIELDS:  WR-YTD-PCT-EXPENSE-INCOME &
      *             WR-YTD-PCT-PERSONNEL-INCOME; THESE ARE CALCULATED
      *             FROM YTD AMOUNTS INSTEAD OF ACCUMULATING PERCENTS
      *  BLV 040223 ADD FIELDS FOR CASHIER, ASSISTANT MANGAGER &
      *             MANAGER MONTHLY BONUSES
      *  BLV 040305 ADD FIELDS FOR NON-FILE END-OF-PERIOD BALANCE,
      *             NO OF ACCTS 1 YEAR AGO FOR ALL TYPES, AND
      *             NO OF CONTRACTUALLY SLOW ACCTS FOR ALL TYPES
      *  BLV 040308 ADD FIELDS FOR OUTSTANDING BAL 1 YEAR AGO FOR
      *             ALL TYPES, NEED FOR SUPERVISOR BONUS REPORT
      *  BLF 050304 WR-PRETAX-PROFIT FIELD NOT LARGE ENOUGH, RENAMED TO
      *             WR-OLD-PRETAX-PROFIT & ADDED LARGER FIELD WITH THE
      *             SAME NAME.  WILL RUN FIX & RECOMPILE ALL PROGRAMS
      *             USING WRFILE.  WORLD PR# 389
      *  CS  060501 CHANGED WR-CASHIER-BONUS, WR-ASST-MGR-BONUS TO 9(5)
      *             [PR#4742 WORLD]
      *  BLM 100402 WR-SUPERVISION FIELD NOT LARGE ENOUGH, RENAMED TO
      *             WR-OLD-SUPERVISION & ADDED LARGER FIELD WITH THE
      *             SAME NAME.  WILL RUN FIX & RECOMPILE ALL PROGRAMS
      *             USING WRFILE.  WORLD PR# 675
      *  BLM 140916 ADD WR-MGR-PENALTY-FG.  BONUS1 WILL SET TO * IF
      *             10% PENALTY, PRINTS ON RATING SHEET, WORLD PR# 1019
      *  BLM 150513 ADD WR-NO-TAX-RETURNS, MTD & YTD.  NOTE, THESE ARE
      *             FOR FUTURE USE, MAY ADD TO BONUS1 UNLESS THEY DO
      *             THEM MANUALLY, WORLD PR# 1079
      *  BLM 160616 ADDED WR-BSR-BONUS, REUSED WR-OLD-PRETAX-PROFIT.
      *             WILL USE WR-BSR-BONUS(1), OTHER OCCURENCE CAN BE
      *             USED IF THEY NEED A DIFFERENT CALC FOR AM IN
      *             THE FUTURE, WORLD PR# 1163
      ******************************************************************
       01  WR-REC.
           03  WR1-KEY.
               05  WR-YYYYMM                 PIC 9(6).
               05  FILLER                   REDEFINES WR-YYYYMM.
                   07  WR-YYYY               PIC 9(4).
                   07  WR-MM                 PIC 9(2).
               05  WR-BRNO                   PIC 9(4).
01         03  WR-CUR-NO-CUST                PIC S9(5)    COMP-3.
           03  WR-ELM-NO-CUST                PIC S9(5)    COMP-3.
           03  WR-ELY-NO-CUST                PIC S9(5)    COMP-3.
03         03  WR-OUTS-BALANCE               PIC S9(9)V99 COMP-3.
           03  WR-ELM-OUTS-BALANCE           PIC S9(9)V99 COMP-3.
           03  WR-ELY-OUTS-BALANCE           PIC S9(9)V99 COMP-3.
           03  WR-MTD-YTD                    OCCURS 2.
05             05  WR-NO-NB                  PIC S9(5)    COMP-3.
06             05  WR-NO-FB                  PIC S9(5)    COMP-3.
07             05  WR-NO-PB                  PIC S9(5)    COMP-3.
09             05  WR-GROSS-VOLUME           PIC S9(9)V99 COMP-3.
               05  WR-BSR-BONUS              PIC S9(7)V99 COMP-3.
14             05  WR-INTEREST-ALLOCATION    PIC S9(7)V99 COMP-3.
               05  WR-OLD-SUPERVISION        PIC S9(7)V99 COMP-3.
16             05  WR-GEN-RESERVE-ADJ        PIC S9(7)V99 COMP-3.
17             05  WR-INTANGIBLE-AMORT       PIC S9(7)V99 COMP-3.
18             05  WR-ROBBERIES              PIC S9(7)V99 COMP-3.
21             05  WR-RATING-SHEET-INCOME    PIC S9(7)V99 COMP-3.
23             05  WR-INT-FEE-INCOME         PIC S9(7)V99 COMP-3.
25             05  WR-INSURANCE-INCOME       PIC S9(7)V99 COMP-3.
27             05  WR-RATING-SHEET-EXPENSES  PIC S9(7)V99 COMP-3.
31             05  WR-PERSONNEL-EXPENSE      PIC S9(7)V99 COMP-3.
33             05  WR-CHARGEOFF-EXPENSE      PIC S9(7)V99 COMP-3.
34             05  WR-NO-CHARGEOFFS          PIC S9(5)    COMP-3.
35             05  WR-RECOVERY-INCOME        PIC S9(7)V99 COMP-3.
36             05  WR-BAD-DEBT-INCREASE      PIC S9(7)V99 COMP-3.
45         03  WR-NO-30-DAY-ACCTS            PIC S9(7)    COMP-3.
46         03  WR-AMT-30-DAY-ACCTS           PIC S9(9)V99 COMP-3.
48         03  WR-NO-60-DAY-ACCTS            PIC S9(7)    COMP-3.
49         03  WR-AMT-60-DAY-ACCTS           PIC S9(9)V99 COMP-3.
51         03  WR-NO-90-DAY-ACCTS            PIC S9(7)    COMP-3.
52         03  WR-AMT-90-DAY-ACCTS           PIC S9(9)V99 COMP-3.
           03  WR-ELM-AMT-90-DAY-ACCTS       PIC S9(9)V99 COMP-3.
           03  WR-ELY-AMT-90-DAY-ACCTS       PIC S9(9)V99 COMP-3.

      ******************************************************************
      *    YTD PERCENTS - MTD IS CALCULATED BUT YTD IS ACCUMULATED
      ******************************************************************
10         03  WR-YTD-PCT-TURNOVER           PIC S9(3)V99 COMP-3.
20         03  WR-YTD-PCT-RATE-SHT-PROFIT    PIC S9(3)V99 COMP-3.
22         03  WR-YTD-PCT-RS-INC-LEDGER      PIC S9(3)V99 COMP-3.
24         03  WR-YTD-PCT-INT-FEE-LEDGER     PIC S9(3)V99 COMP-3.
26         03  WR-YTD-PCT-INS-INC-LEDGER     PIC S9(3)V99 COMP-3.
29         03  WR-YTD-PCT-EXPENSE-LEDGER     PIC S9(3)V99 COMP-3.
37         03  WR-YTD-PCT-BAD-DEBT-LEDGER    PIC S9(3)V99 COMP-3.
39         03  WR-YTD-PCT-NF-BNK-LEDGER      PIC S9(3)V99 COMP-3.
41         03  WR-YTD-PCT-PRETAX-LEDGER      PIC S9(3)V99 COMP-3.
44         03  WR-YTD-PCT-DEBT-NF-LEDGER     PIC S9(3)V99 COMP-3.

      ******************************************************************
      *    SALES FINANCE RATING SHEET  (WCBC)
      ******************************************************************
02         03  WR-WCBC-CUR-NO-CUST           PIC S9(5)    COMP-3.
           03  WR-WCBC-ELM-NO-CUST           PIC S9(5)    COMP-3.
           03  WR-WCBC-ELY-NO-CUST           PIC S9(5)    COMP-3.
04         03  WR-WCBC-OUTS-BALANCE          PIC S9(9)V99 COMP-3.
           03  WR-WCBC-ELM-OUTS-BAL          PIC S9(9)V99 COMP-3.
           03  WR-WCBC-ELY-OUTS-BAL          PIC S9(9)V99 COMP-3.
           03  WR-WCBC-MTD-YTD               OCCURS 2.
06             05  WR-WCBC-NO-NB             PIC S9(5)    COMP-3.
07             05  WR-WCBC-NO-FB             PIC S9(5)    COMP-3.
08             05  WR-WCBC-NO-PB             PIC S9(5)    COMP-3.
11             05  WR-WCBC-GROSS-VOLUME      PIC S9(9)V99 COMP-3.
14             05  WR-WCBC-LATE-CHGS         PIC S9(9)V99 COMP-3.
17             05  WR-WCBC-NET-FIN-CHGS      PIC S9(7)V99 COMP-3.
19             05  WR-WCBC-SALES             PIC S9(7)V99 COMP-3.
20             05  WR-WCBC-COST-OF-SALES     PIC S9(7)V99 COMP-3.
21             05  WR-WCBC-EXPENSES          PIC S9(7)V99 COMP-3.
29             05  WR-WCBC-RECOVERY-INCOME   PIC S9(7)V99 COMP-3.
31             05  WR-WCBC-CHARGEOFF-EXPENSE PIC S9(7)V99 COMP-3.
32             05  WR-WCBC-BAD-DEBT-BNKRPCY  PIC S9(7)V99 COMP-3.
39         03  WR-WCBC-NO-30-DAY-ACCTS       PIC S9(7)    COMP-3.
40         03  WR-WCBC-AMT-30-DAY-ACCTS      PIC S9(9)V99 COMP-3.
41         03  WR-WCBC-NO-60-DAY-ACCTS       PIC S9(7)    COMP-3.
42         03  WR-WCBC-AMT-60-DAY-ACCTS      PIC S9(9)V99 COMP-3.
43         03  WR-WCBC-NO-90-DAY-ACCTS       PIC S9(7)    COMP-3.
44         03  WR-WCBC-AMT-90-DAY-ACCTS      PIC S9(9)V99 COMP-3.
           03  WR-WCBC-ELM-AMT-90-DAYS       PIC S9(9)V99 COMP-3.
           03  WR-WCBC-ELY-AMT-90-DAYS       PIC S9(9)V99 COMP-3.

      ******************************************************************
      *  WCBC YTD PERCENTS - MTD IS CALCULATED BUT YTD IS ACCUMULATED
      ******************************************************************
10         03  WR-WCBC-PCT-VOLUME            PIC S9(3)V99 COMP-3.
28         03  WR-WCBC-GROSS-PROF-PCT        PIC S9(3)V99 COMP-3.

      ******************************************************************
      *  LARGE LOAN FIELDS ?????
      ******************************************************************
           03  WR-LARGE-CUR-NO-CUST          PIC S9(5)    COMP-3.
           03  WR-LARGE-ELM-NO-CUST          PIC S9(5)    COMP-3.
           03  WR-LARGE-ELY-NO-CUST          PIC S9(5)    COMP-3.
           03  WR-LARGE-OUTS-BALANCE         PIC S9(9)V99 COMP-3.
           03  WR-LARGE-ELM-OUTS-BAL         PIC S9(9)V99 COMP-3.
           03  WR-LARGE-ELY-OUTS-BAL         PIC S9(9)V99 COMP-3.

           03  WR-LARGE-MTD-YTD               OCCURS 2.
               05  WR-LARGE-NO-NB             PIC S9(5)    COMP-3.
               05  WR-LARGE-NO-FB             PIC S9(5)    COMP-3.
               05  WR-LARGE-NO-PB             PIC S9(5)    COMP-3.
               05  WR-LARGE-GROSS-VOLUME      PIC S9(9)V99 COMP-3.
           03  WR-LARGE-NO-30-DAY-ACCTS      PIC S9(7)    COMP-3.
           03  WR-LARGE-AMT-30-DAY-ACCTS     PIC S9(9)V99 COMP-3.
           03  WR-LARGE-NO-60-DAY-ACCTS      PIC S9(7)    COMP-3.
           03  WR-LARGE-AMT-60-DAY-ACCTS     PIC S9(9)V99 COMP-3.
           03  WR-LARGE-NO-90-DAY-ACCTS      PIC S9(7)    COMP-3.
           03  WR-LARGE-AMT-90-DAY-ACCTS     PIC S9(9)V99 COMP-3.
           03  WR-LARGE-ELM-AMT-90-DAYS      PIC S9(9)V99 COMP-3.
           03  WR-LARGE-ELY-AMT-90-DAYS      PIC S9(9)V99 COMP-3.

      ******************************************************************
      *  INSURANCE FIELDS
      ******************************************************************
           03  WR-INS-MTD-YTD                OCCURS 2.
               05  WR-LIFE-CLAIMS            PIC S9(7)V99 COMP-3.
               05  WR-AH-CLAIMS              PIC S9(7)V99 COMP-3.
               05  WR-FIRE-CLAIMS            PIC S9(7)V99 COMP-3.
               05  WR-AUTO-CLAIMS            PIC S9(7)V99 COMP-3.
               05  WR-UNEMP-CLAIMS           PIC S9(7)V99 COMP-3.
               05  WR-AD-D-CLAIMS            PIC S9(7)V99 COMP-3.
               05  WR-L-PROT-CLAIMS          PIC S9(7)V99 COMP-3.
40             05  WR-NF-NON-BK-CLAIMS       PIC S9(7)V99 COMP-3.
38             05  WR-NF-BK-CLAIMS           PIC S9(7)V99 COMP-3.
               05  WR-ROAD-GARD-CLAIMS       PIC S9(7)V99 COMP-3.


               05  WR-NO-LIFE-CLAIMS         PIC S9(5) COMP-3.
               05  WR-NO-AH-CLAIMS           PIC S9(5) COMP-3.
               05  WR-NO-FIRE-CLAIMS         PIC S9(5) COMP-3.
               05  WR-NO-AUTO-CLAIMS         PIC S9(5) COMP-3.
               05  WR-NO-UNEMP-CLAIMS        PIC S9(5) COMP-3.
               05  WR-NO-AD-D-CLAIMS         PIC S9(5) COMP-3.
               05  WR-NO-L-PROT-CLAIMS       PIC S9(5) COMP-3.
               05  WR-NO-NF-NON-BK-CLAIMS    PIC S9(5) COMP-3.
               05  WR-NO-NF-BK-CLAIMS        PIC S9(5) COMP-3.
               05  WR-NO-ROAD-GARD-CLAIMS    PIC S9(5) COMP-3.

               05  WR-LIFE-PREMS            PIC S9(7)V99 COMP-3.
               05  WR-AH-PREMS              PIC S9(7)V99 COMP-3.
               05  WR-FIRE-PREMS            PIC S9(7)V99 COMP-3.
               05  WR-AUTO-PREMS            PIC S9(7)V99 COMP-3.
               05  WR-UNEMP-PREMS           PIC S9(7)V99 COMP-3.
               05  WR-AD-D-PREMS            PIC S9(7)V99 COMP-3.
               05  WR-L-PROT-PREMS          PIC S9(7)V99 COMP-3.
               05  WR-NON-FILE-PREMS        PIC S9(7)V99 COMP-3.
               05  WR-ROAD-GARD-PREMS       PIC S9(7)V99 COMP-3.

               05  WR-NO-LIFE-PREMS         PIC S9(5) COMP-3.
               05  WR-NO-AH-PREMS           PIC S9(5) COMP-3.
               05  WR-NO-FIRE-PREMS         PIC S9(5) COMP-3.
               05  WR-NO-AUTO-PREMS         PIC S9(5) COMP-3.
               05  WR-NO-UNEMP-PREMS        PIC S9(5) COMP-3.
               05  WR-NO-AD-D-PREMS         PIC S9(5) COMP-3.
               05  WR-NO-L-PROT-PREMS       PIC S9(5) COMP-3.
               05  WR-NO-NON-FILE-PREMS     PIC S9(5) COMP-3.
               05  WR-NO-ROAD-GARD-PREMS    PIC S9(5) COMP-3.

               05  WR-LIFE-REFS              PIC S9(7)V99 COMP-3.
               05  WR-AH-REFS                PIC S9(7)V99 COMP-3.
               05  WR-FIRE-REFS              PIC S9(7)V99 COMP-3.
               05  WR-AUTO-REFS              PIC S9(7)V99 COMP-3.
               05  WR-UNEMP-REFS             PIC S9(7)V99 COMP-3.
               05  WR-AD-D-REFS              PIC S9(7)V99 COMP-3.
               05  WR-L-PROT-REFS            PIC S9(7)V99 COMP-3.
               05  WR-NON-FILE-REFS          PIC S9(7)V99 COMP-3.
               05  WR-ROAD-GARD-REFS         PIC S9(7)V99 COMP-3.

               05  WR-NF-RECOVERIES          PIC S9(7)V99 COMP-3.

576        03  WR-NONFILE-ENDING-BALANCE     PIC S9(7)V99 COMP-3.
           03  WR-NO-CUST-1-YR-AGO           PIC S9(5)    COMP-3.
           03  WR-OUTS-BAL-1-YR-AGO          PIC S9(9)V99 COMP-3.
           03  WR-WCBC-NO-CUST-1-YR-AGO      PIC S9(5)    COMP-3.
           03  WR-WCBC-OUTS-BAL-1-YR-AGO     PIC S9(9)V99 COMP-3.
           03  WR-LARGE-NO-CUST-1-YR-AGO     PIC S9(5)    COMP-3.
           03  WR-LARGE-OUTS-BAL-1-YR-AGO    PIC S9(9)V99 COMP-3.
           03  WR-CUR-NO-CONT-SLOW           PIC S9(5)    COMP-3.
           03  WR-WCBC-CUR-NO-CONT-SLOW      PIC S9(5)    COMP-3.
           03  WR-LARGE-CUR-NO-CONT-SLOW     PIC S9(5)    COMP-3.

           03  WR-CASHIER-BONUS              PIC S9(5)    COMP-3.
           03  WR-ASST-MGR-BONUS             PIC S9(5)    COMP-3.
           03  WR-MGR-BONUS                  PIC S9(5)V99 COMP-3.


           03  WR-LOTS-INS-MTD-YTD           OCCURS 2.
               05  WR-LOTS-NF-CLAIMS         PIC S9(7)V99 COMP-3.
               05  WR-LOTS-NO-NF-CLAIMS      PIC S9(5) COMP-3.
               05  WR-LOTS-NON-FILE-PREMS    PIC S9(7)V99 COMP-3.
               05  WR-LOTS-NO-NON-FILE-PREMS PIC S9(5) COMP-3.
               05  WR-LOTS-NF-RECOVERIES     PIC S9(7)V99 COMP-3.

           03  WR-EXPANDED-MTD-YTD           OCCURS 2.
12             05  WR-PRETAX-PROFIT          PIC S9(9)V99 COMP-3.
15             05  WR-SUPERVISION            PIC S9(9)V99 COMP-3.
           03  WR-MGR-PENALTY-FG             PIC X.
      ** WR-NO-TAX-RETURNS, MTD & YTD, FOR FUTURE USE (THEY MAY
      ** JUST DO MANUALLY, OR WE WILL ADD LOGIC TO BONUS1
           03  WR-NO-TAX-RETURNS             PIC S9(5)    COMP-3
                                             OCCURS 2.
      ******************************************************************
      *  NOTE, IF YOU ARE ADDING A NEW FIELD, YOU CAN USE
      *  WR-OLD-PRETAX-PROFIT, OCCURS 2, WILL BE 0 OR
      *  WR-OLD-SUPERVISION, OCCURS 2, WILL BE 0
      ******************************************************************
           03  FILLER                        PIC X(282).
