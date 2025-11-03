       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BNKRP1.
       DATE-WRITTEN.    011985.
      ******************************************************************
      *
      *                      (LP)
      *    DESCRIPTION:  YEARLY BANK REPORT - SOURCE,COLLATERAL,PURPOSE
      *
      *=================================================================
      * REV:
      * 101895 JTG ADDED GROUP LOGIC AND CROSS DATA PATH LOGIC
      * 110895 KEC ADDED OPTIONS FOR SALES FINANCE ACCTS(I,S,O);
      *            BULK (OR LN-MIDSTREAM) (I,S,O); SECURITY(S/U/R).
      *            ADDED LN-OTHBAL TO LN-OT2BAL IN ORDER TO PRINT ON
      *            THE DETAIL-LINE.  ALSO ADD BULK TO THE DETAIL-LINE.
      * 082096 MJD ADDED OPTION "C" TO LNAMT-AMTFIN FOR CURBAL.
      * KEC 111496 CHANGED LN-POCD FOR RESCISSION.
      * JTG 051297 ADDED USE OF 'SETCTBL' AND 'SETCTBLW'
      * BLV 051997 IF LN-BR-TFR-FG = "Y" DON'T PRINT DETAIL, JUST
      *            INCLUDE IN A/R
      * BAH 052397 WAS NOT PRINTING IF PIGGYBACK. DONT DISPLAY PROGRESS
      *            MSG IF PIGGBACK
      * BLV 060597 CHANGED FOR LOCAL SPOOL WITH NEW BATCH DRIVER - BREXEC
      * BLV 070897 WAS NOT WORKING IN BATCH - TEST SECURITY-TYPE, NOT
      *            WS-SECURITY-TYPE
      * GLF 090297 REMOVED ALL USE OF GLOBAL
      * BAH 021298 ADDED NEW TABLE TO PRINT A SUMMARY AND TOTAL FOR
      *            RENEWALS LESS THAN 10 PERCENT (WORLD) SC LAW REQUIREMENT
      *            ADDED NEW TABLE TO PRINT A SUMMARY AND TOTAL FOR
      *            PAIDOFFS OTHER THAN RENEWALS. (WORLD) SC LAW REQUIREMENT
      *  CS 042898 ADDED TEST OF "RB" AND "RO" IN DETERMINING RENEWALS
      * KEC 031199 ADDED CLASS RANGE BUT ALL CLASS WILL BE
      *             TOTAL TOGETHER.
      * KEC 033199 MOVED 'END-CLASS' TO END OF 'EOBUF'
      * GLB 990510 ADD REPOS (I,S,O); DEFAULT YEAR-END-DATE DATE;
      *            DONT SKIP IF INVALID COLLATERAL CODE
      * GLF 990923 REPLACED IO-FG NUMBERS WITH IO-FG 88 LEVEL PER DEVELOPMENT
      *            MEETING (082599)
      * KEC 001113 {PR#01341} SMOKY MOUNTAIN [ANDRIA] <A15>
      *             ADDED A NEW TYPE FOR BREAKING DOWN ON THE BRACKET
      *             AMOUNTS; "X" FOR LOAN EXPIRATION.
      * BAH 031027 ONLY SKIP TRANSFERRED LN-BR-TFR-FG ACCOUNTS IF THE
      *            BRACKETS AMOUNTS SELECTION IS "N" (NET RECV) OR "R"
      *            (RECV)...CENTRAL PL# 426
      * BAH 050103 REMOVE SECOND WORK FILE WHEN EXITING THE PROGRAM
      * BAH 070709 USE LN-RESCISSION-DATE FOR LOANDATE WHEN LOAN IS
      *            RESCISSION, COLTN PL# 593
      * BLM 100308 ADDED OPTION TO PRINT NET UNEARNED (CURBAL LESS
      *            NET UNEARNED INSTEAD OF CURBAL LESS REB-REBATES).
      *            NOTE, IF YOU SELECT THIS, IT WILL USE CURRENT
      *            UNEARNED UNLIKE THE REBATES WHICH COME FROM PAYOFF
      *            AS OF SELECTED YEAR-END-DATE DATE.  ALSO ADDED REPORT
      *            TYPE OPTION OF 'B' FOR BRIEF OR BRACKETS - JUST
      *            PRINTS BRANCH & GRAND FOR TOP BRACKET SECTION
      *            WITHOUT ALL THE BREAKOUTS FOR SOURCE CODES, PURPOSE
      *            CODE, ETC.  WORLD PR# 677
      * BLM 100810 WHEN CHECKING LOAN DATE RANGE, IF NOT SALES FINANCE
      *            AND NOT RESCISSION, IF LOAN DATE NOT = ENTRY DATE,
      *            USE ENTRY DATE TO CHECK RANGE (BACK DATED LOANS),
      *            COLTN, PL# 697
      * BLM 110408 CHANGED SELECTED RATE TABLE IF AMTS-ON-APR OR
      *            AMTS-ON-EFFRATE TO BE 9(5)V999 INSTEAD OF 9(6)V99.
      *            JIM WAS TRYING TO GET LOANS TO 199.99 APR, THEN
      *            LOANS 200.00 AND ABOVE.  SINCE THE APR & EFF RATES
      *            ARE ACTUALLY 3 DIGITS IN LOAN RECORD, CHANGED ALL
      *            THE RATE STUFF TO 3 DIGITS, ENTRIES, WORKERS, ETC.
      *            CHANGED VERSION TO D, SINCE BATCHES WILL ONLY HAVE
      *            2 DIGITS ON RATE TYPE REPORTS. PL# 734
      *            NOTE:  VERSION WILL FORCE RE-ENTRY OF PARAMETERS,
      *            RATE TYPE REPORTS WILL DISPLAY 1 DIGIT OFF (EG 3.6
      *            INSTEAD OF 36.0) WHEN YOU CALL UP TO FIX VERSION!
      * BLM 111010 FIX PROBLEM FROM ABOVE CHANGE, EXPANDED DECIMALS ON
      *            RATE TO 3 DIGITS, BUT FORGOT TO ADD DECIMAL IN
      *            COMPUTING AMOUNT-RATE.  CHANGED:
      *               COMPUTE RATE-WK = AMOUNT-RATE(SUB1) + .001
      *            TO:
      *               COMPUTE RATE-WK = AMOUNT-RATE(SUB1) + .0001
      *
      * KEC 120731 {PR#00816} WORLD <A15> [ROSENAUER]
      *            ADDED OPTION TO HAVE BRACKET AMOUNTS ON
      *            ACCELERATION RATE (C).
      * BAH 150331 EXPANDED WORKING STORAGE WORKERS FROM S9(9)V99 TO
      *            S9(11)V99, TRUNCATING BILLIONS, WORLD PR# 1064
      * BAH 160125 SPLIT OUT LTFILE, PD0003
      * BAH 170509 ACTIVATE 8 DIGIT DATES, PD0006
      * KEC 2017.0609 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED WK2-CLASS FROM 9(2) TO 9(3).
      *
      * BAH 181203 GLOBAL LNFILE, L2G
      * BAH 20200423 CLOSE ERROR ON BRFILE, THERE WAS A CLOSE-BR-FILE IN THE
      *              GROUP LOGIC, SECURITY-TYPE WAS DEFINED AS 5, NOT WORKING
      *              CHANGED TO 3 AND REMOVED WS-SECURITY-TYPE
      * BAH 20200902 PUT EOBUFF SECURITY-TYPE BACK TO X(5) SO A34 BATCHES 
      *              HAVE TO BE RE-ENTERED, EOY HAS A TON OF THEM
      * BAH 20220803 HARD CODED START
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBGB/GBFSWKI.CPY".
           SELECT WK2-FILE ASSIGN WK2-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK2-KEY
                  FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBGB/GBFDPR.CPY".

      ******************************************************************
      *   SUMMARY TOTALS WORK RECORD
      ******************************************************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-BR    PIC 9(4)      COMP.
               05  WK-TYPE  PIC 9(2).
               05  WK-CODE  PIC X(3).
           03  WK-RQTY      PIC S9(9)     COMP.
           03  WK-RENEWED   PIC S9(10)V99 COMP.
           03  WK-LNAMT     PIC S9(10)V99 COMP.
           03  WK-NET       PIC S9(10)V99 COMP.
           03  WK-QTY       PIC S9(9)     COMP.
           03  WK-BALANCE   PIC S9(10)V99 COMP.
           03  FILLER       PIC X.

      ******************************************************************
      *   WORK RECORD FOR ANY INVALID RECORDS THAT DO NOT HAVE VALID
      *   COLLATERAL CODE RECORDS.
      ******************************************************************
       FD  WK2-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK2-REC.
           03  WK2-KEY.
               05  WK2-OWNBR  PIC 9(4).
               05  WK2-CLASS  PIC 9(3).
               05  WK2-ACCTNO PIC 9(8).
           03  WK2-CODE       PIC X(3).
      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)LP/BNKRP1 S35 03/18/22-08:58:09 >".

       COPY "LIBGB/GB01GP.CPY".
       COPY "LIBGB/GB01GP_SQL.CPY".
       COPY "LIBGB/GBWSGP.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
      *-----------------------------------------------------------------
       01  PR-IFN               PIC X(2)     VALUE "PR".
       01  WK-IFN               PIC X(2)     VALUE "WK".
       01  WK2-IFN              PIC X(3)     VALUE "WK2".

       01  WK2-PATH             PIC X(35).

      ******************************************************************
      *
      *    L I N E - W O R K E R S
      *
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      ******************************************************************
      *
      *   M I S C E L L E N O U S - W O R K E R S
      *
      ******************************************************************
       01  NEW-BRANCH           PIC X        VALUE "Y".
       01  SUB                  PIC 9(2)     COMP.
       01  SUB1                 PIC 9(2)     COMP.
       01  RANGE-SUB            PIC 9(2).
       01  SUB-RENEWAL          PIC 9(2)     COMP.
       01  SUB-PAIDOFF          PIC 9(2)     COMP.

       01  INS-STATUS           PIC 9 VALUE 0.
           88  INS-FOUND        VALUE 1.
           88  INS-PRINTED      VALUE 2.

       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".

       01  LEV                  PIC 9(2)     COMP.
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  LEGEND-LIST          PIC X(09)    VALUE "LEGEND:B.".
       01  SUMMARY-FG           PIC X        VALUE "N".
       01  VALID-DATE           PIC X        VALUE "Y".
       01  VALID-PAYOFF-DATE    PIC X        VALUE "N".

       01  QTY1-WK              PIC S9(9)       COMP.
       01  QTY2-WK              PIC S9(9)       COMP.
       01  REN-WK               PIC S9(11)V99   COMP.
       01  AMT-WK               PIC S9(10)V999  COMP.
       01  RATE-WK              PIC S9(11)V9999 COMP.
       01  NET-WK               PIC S9(11)V99   COMP.
       01  BAL-WK               PIC S9(11)V99   COMP.
       01  PCT-WK               PIC S9(3)V99    COMP.
       01  HOLD-AMT             PIC S9(11)V99   COMP.
       01  HOLD-RENEW           PIC S9(11)V99   COMP.
       01  LAST-ARREAR          PIC S9(11)V99.
       01  PER-REPORT           PIC S9(5)       COMP.
       01  HOLD-REMAIN          PIC S9(5)       COMP.
       01  PERIOD-AMT           PIC S9(11)V99   COMP.
       01  PERIOD-WK            PIC S9(5)       COMP.
       01  HOLD-BRANCH          PIC 9(4)        COMP   VALUE 0.
       01  HOLD-TYPE            PIC 9(2)        COMP   VALUE 0.

       01  FIRST-TYPE           PIC X        VALUE "Y".
       01  PAIDOFF-FLAG         PIC X        VALUE "N".

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  MONTHS-TILL-EXPIRATION      PIC 9(03)     VALUE ZEROES.
       01  MONTHS-TILL-EXP-STAT        PIC X(01)     VALUE SPACES.
           88  MONTHS-TILL-EXP-STAT-BAD    VALUE "X".
           88  MONTHS-TILL-EXP-STAT-GOOD   VALUE SPACES.

       01  HOLD-SYSDATE             PIC 9(8)     VALUE ZEROES.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    WS-INVALID-COLL-ROUTINE IS A FLAG FOR THE HEADER ROUTINE IN
      *    ORDER TO PRINT ANOTHER HEADER FOR INVALID COLLATERAL CODES.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  WS-INVALID-COLL-ROUTINE  PIC X(1) VALUE "N".

       01  SAVE-WK-KEY.
           03  SAVE-WK-BR       PIC 9(4)     COMP.
           03  SAVE-WK-TYPE     PIC 99.
           03  SAVE-WK-CODE     PIC XXX.

      ******************************************************************
      *
      *    BRANCH/GRAND TOTALS FOR PAID OUT ACCTS OTHER THAN RENEWALS
      *
      ******************************************************************
       01  PAIDOFF-TAB.
           03  PAIDOFF-LEVEL       OCCURS 2.
               05  PAIDOFF-RQTY    PIC S9(9)     COMP-3.
               05  PAIDOFF-REN     PIC S9(9)V99  COMP-3.
               05  PAIDOFF-LAMT    PIC S9(9)V99  COMP-3.
               05  PAIDOFF-NET     PIC S9(9)V99  COMP-3.
               05  PAIDOFF-QTY     PIC S9(9)     COMP-3.
               05  PAIDOFF-BAL     PIC S9(9)V99  COMP-3.

      ******************************************************************
      *
      *    BRANCH/GRAND TOTALS FOR RENEWALS LESS THAN 10 PERCENT.
      *
      ******************************************************************
       01  RENEWAL-TAB.
           03  RENEWAL-LEVEL       OCCURS 2.
               05  RENEWAL-RQTY    PIC S9(9)     COMP-3.
               05  RENEWAL-REN     PIC S9(9)V99  COMP-3.
               05  RENEWAL-LAMT    PIC S9(9)V99  COMP-3.
               05  RENEWAL-NET     PIC S9(9)V99  COMP-3.
               05  RENEWAL-QTY     PIC S9(9)     COMP-3.
               05  RENEWAL-BAL     PIC S9(9)V99  COMP-3.

      ******************************************************************
      *         LEVELS  : BRANCH / GRAND
      *         BRACKETS CORRESPOND TO ALP
      *      FUTURE CORRESPOND TO SUCCEEDING YEARS(5) RECEIVABLE
      *                                      YEARS(6) - REMAINING
      *                                      YEARS(7) - TOTALS
      ******************************************************************
       01  TOTAL-TAB.
           03  TOTAL-LEVEL           OCCURS 2.
               05  TOTAL-BRACKET     OCCURS 11.
                   07  TOTAL-RQTY    PIC S9(9)     COMP.
                   07  TOTAL-REN     PIC S9(10)V99 COMP.
                   07  TOTAL-LAMT    PIC S9(10)V99 COMP.
                   07  TOTAL-NET     PIC S9(10)V99 COMP.
                   07  TOTAL-QTY     PIC S9(9)     COMP.
                   07  TOTAL-BAL     PIC S9(10)V99 COMP.
               05  TOTAL-FUTURE      OCCURS 7.
                   07  TOTAL-DATE    PIC 9(8)      COMP.
                   07  TOTAL-RCV     PIC S9(10)V99 COMP.

       01  BRACKET-ALP-TAB.
           03  BRACKET-ALP-LINE  OCCURS 10.
               05  D-BRAMT1      PIC Z(5)9.99.
               05  D-BRAMT1-T REDEFINES D-BRAMT1
                                 PIC Z(8)9.
               05  D-BRAMT1-R REDEFINES D-BRAMT1
                                 PIC Z(3)9.9999.
               05  D-BRAMT1-X REDEFINES D-BRAMT1
                                 PIC Z(8)9.
               05  D-TAG         PIC X(4).
               05  D-BRAMT1A     PIC Z(5)9.99.
               05  D-BRAMT1A-T REDEFINES D-BRAMT1A
                                 PIC Z(8)9.
               05  D-BRAMT1A-R REDEFINES D-BRAMT1A
                                 PIC Z(3)9.9999.
               05  D-BRAMT1A-X REDEFINES D-BRAMT1A
                                 PIC Z(8)9.
           03  FILLER            PIC X(22) VALUE
           "--------TOTALS--------".

       01  BRACKET-TABX REDEFINES BRACKET-ALP-TAB.
           03  BRACKET-ALP       PIC X(22) OCCURS 11.

       01  TYPE-ALP-TAB.
           03  FILLER            PIC X(18) VALUE
           "COCOLLATERAL     :".
           03  FILLER            PIC X(18) VALUE
           "SRSOURCE         :".
           03  FILLER            PIC X(18) VALUE
           "PUPURPOSE        :".
           03  FILLER            PIC X(18) VALUE
           "SRRENEWALS < 10% :".
           03  FILLER            PIC X(18) VALUE
           "POOTHER PAIDOFFS :".
       01  TYPE-TABX REDEFINES TYPE-ALP-TAB.
           03  TYPE-ALPX         OCCURS 5.
               05  TYPE-KEY      PIC XX.
               05  TYPE-ALP      PIC X(16).

      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-BR                         PIC 9(4).
           03  END-BR                         PIC 9(4).
           03  BEG-CLASS                      PIC 9(3).
           03  BEG-LN-DATE                    PIC 9(8).
           03  END-LN-DATE                    PIC 9(8).
           03  YEAR-END-DATE                  PIC 9(8).
           03  ZBAL                           PIC X.
           03  PL                             PIC X.
           03  SERVA                          PIC X.
           03  SALEFIN                        PIC X.
           03  BULK                           PIC X.
           03  SECURITY-TYPE                  PIC X(5).
           03  SEC-TYPE             REDEFINES SECURITY-TYPE.
               05  SEC-SECURED                PIC X.
               05  FILLER                     PIC X.
               05  SEC-UNSECURED              PIC X.
               05  FILLER                     PIC X.
               05  SEC-REAL-ESTATE            PIC X.
           03  LNAMT-AMTFIN                   PIC X.
           03  AMTS-ON                        PIC X.
               88  AMTS-ON-APR                    VALUE "A".
               88  AMTS-ON-ACCRATE                VALUE "C".
               88  AMTS-ON-EFFRATE                VALUE "E".
               88  AMTS-ON-NET-RECEIVABLES        VALUE "N".
               88  AMTS-ON-RECEIVABLES            VALUE "R".
               88  AMTS-ON-TERM                   VALUE "T".
               88  AMTS-ON-LOAN-EXPIRATION        VALUE "X".
               88  AMTS-ON-VALID                  VALUE "A" "C" "E" "N"
                                                        "R" "T" "X".
           03  AMOUNTS-9.
               05  AMOUNT                     PIC 9(6)V99 OCCURS 10.
           03  FILLER REDEFINES AMOUNTS-9.
               05  AMT01-BUF                  PIC 9(6)V99.
               05  AMT02-BUF                  PIC 9(6)V99.
               05  AMT03-BUF                  PIC 9(6)V99.
               05  AMT04-BUF                  PIC 9(6)V99.
               05  AMT05-BUF                  PIC 9(6)V99.
               05  AMT06-BUF                  PIC 9(6)V99.
               05  AMT07-BUF                  PIC 9(6)V99.
               05  AMT08-BUF                  PIC 9(6)V99.
               05  AMT09-BUF                  PIC 9(6)V99.
               05  AMT10-BUF                  PIC 9(6)V99.
           03  AMOUNTS-X            REDEFINES AMOUNTS-9.
               05  AMOUNT-X                   PIC X(8) OCCURS 10.
           03  AMOUNTS-RATES        REDEFINES AMOUNTS-9.
               05  AMOUNT-RATE                PIC 9(5)V999 OCCURS 10.
           03  REPORT-TYPE                    PIC X.
           03  GROUP-FLAG                     PIC X.
           03  ENT-GROUP                      PIC X(4).
           03  END-CLASS                      PIC 9(2).
           03  REPO                           PIC X.
           03  NET-UNEARNED-FG                PIC X.

       01  VERSION-CONTROL                    PIC X VALUE "D".

       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.
      ******************************************************************
      *
      *     P R I N T   B U F F E R S
      *
      ******************************************************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(31)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(37)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(26)    VALUE " ".
           03  FILLER           PIC X(41)    VALUE
           "- - - - BANKING STATISTICS REPORT - - - -".

      *-----------------------------------------------------------------
       01  HEAD3.
           03  FILLER           PIC X(10) VALUE "BRANCH :".
           03  H-BRNOX.
               05  H-BRNO       PIC 9(4).
           03  FILLER           PIC X VALUE " ".
           03  H-BRNAME         PIC X(20).
           03  FILLER           PIC X(21) VALUE " ".
           03  H-BEG-DATE       PIC 99/99/99.
           03  FILLER           PIC X(6)  VALUE " THRU ".
           03  H-END-DATE       PIC 99/99/99.

      *-----------------------------------------------------------------
       01  HEAD4.
           03  FILLER           PIC X(010)   VALUE "CLASS  :".
           03  H-BEG-CLASS      PIC 9(002)   VALUE ZEROES.
           03  FILLER           PIC X(001)   VALUE SPACES.
           03  H-CLASS-DESC     PIC X(119)   VALUE SPACES.
           03  H-CLASS-RDF      REDEFINES H-CLASS-DESC.
               05  H-TO-CLASS   PIC X(002).
               05  FILLER       PIC X(001).
               05  H-END-CLASS  PIC 9(002).
               05  FILLER       PIC X(114).

      *-----------------------------------------------------------------
       01  HEAD5.
           03  FILLER           PIC X(46) VALUE
           "ACCOUNT #        BORROWER                   SO".
           03  FILLER           PIC X(50) VALUE
           "C-SEC-NO   LOAN-DATE  BR/ST    SC  CC  PC  BK  RP ".
           03  D-HEAD-TAG       PIC X(8)  VALUE "LOAN AMT".
           03  FILLER           PIC X(28) VALUE
           "    OTHER      INT      PRIN".

      *-----------------------------------------------------------------
       01  HEAD6.
           03  FILLER           PIC X(44) VALUE " ".
           03  FILLER           PIC X(7)  VALUE "RENEWED".
           03  FILLER           PIC X(6)  VALUE " ".
           03  D-TOTAL-TAG      PIC X(8)  VALUE "    LOAN".
           03  FILLER           PIC X(18) VALUE " ".
           03  FILLER           PIC X(3)  VALUE "NET".
           03  FILLER           PIC X(31) VALUE " ".
           03  FILLER           PIC X(6)  VALUE "RECEIV".
           03  FILLER           PIC X(10) VALUE " ".

      *-----------------------------------------------------------------
       01  HEAD7.
           03  H-TAG            PIC X(16) VALUE " ".
           03  FILLER           PIC X(12) VALUE " ".
           03  FILLER           PIC X(41) VALUE
           "QTY----%         AMOUNT        AMOUNT----".
           03  FILLER           PIC X(22) VALUE
           "%          RECEIV----%".
           03  H-CONNECT1       PIC X(8) VALUE " ".
           03  FILLER           PIC X(8) VALUE
           "QTY----%".
           03  H-CONNECT2       PIC X(10) VALUE " ".
           03  FILLER           PIC X(13) VALUE
           "AMOUNT----%  ".

      *-----------------------------------------------------------------
       01  HEAD8.
           03  FILLER           PIC XX VALUE " ".
           03  FILLER           PIC X(13) VALUE "RECEIVABLES :".
           03  FILLER           PIC X(117) VALUE " ".

      *------------------------ FOR INVALID COLLATERAL CODE ACCOUNTS ---
       01  HEAD9.
           03  FILLER           PIC X(4)   VALUE SPACES.
           03  FILLER           PIC X(10)  VALUE ALL "- ".
           03  FILLER           PIC X(43)  VALUE
               "ACCOUNTS THAT HAVE INVALID COLLATERAL CODES".
           03  FILLER           PIC X(10)  VALUE ALL "- ".
           03  FILLER           PIC X(65)  VALUE SPACES.

      *------------------------ FOR INVALID COLLATERAL CODE ACCOUNTS ---
       01  HEAD10.
           03  FILLER           PIC X(4)  VALUE SPACES.
           03  FILLER           PIC X(6)  VALUE "BRANCH".
           03  FILLER           PIC X(2)  VALUE SPACES.
           03  FILLER           PIC X(7)  VALUE "ACCOUNT".
           03  FILLER           PIC X(3)  VALUE SPACES.
           03  FILLER           PIC X(5)  VALUE "CLASS".
           03  FILLER           PIC X(2)  VALUE SPACES.
           03  FILLER           PIC X(4)  VALUE "CODE".
           03  FILLER           PIC X(99) VALUE SPACES.

      ******************************************************************
      *
      *    D E T A I L - L I N E
      *
      ******************************************************************
       01  DETAIL-LINE          PIC X(132) VALUE SPACES.
      *-----------------------------------------------------------------
       01  DETAIL1             REDEFINES DETAIL-LINE.
           03  D-LOAN-NUM       PIC X(16).
           03  FILLER           PIC X.
           03  D-NAME           PIC X(26).
           03  FILLER           PIC X.
           03  D-SOC-SEC        PIC 999/99/9999.
           03  FILLER           PIC X(3).
           03  D-LOANDATE       PIC 99/99/99.
           03  FILLER           PIC X(2).
           03  D-ORGBR          PIC 9(4).
           03  D-H3             PIC X.
           03  D-ORGST          PIC XX.
           03  FILLER           PIC XX.
           03  D-SRCD           PIC XX.
           03  FILLER           PIC XX.
           03  D-COLLCD         PIC XX.
           03  FILLER           PIC XX.
           03  D-PURCD          PIC XXX.
           03  FILLER           PIC XX.
           03  D-BULK           PIC X.
           03  FILLER           PIC XX.
           03  D-REPO           PIC X.
           03  FILLER           PIC X.
           03  D-LNAMT          PIC Z(5)9.99-  BLANK ZERO.
           03  D-OTHBAL         PIC ZZZZ9.99-  BLANK ZERO.
           03  D-INTBAL         PIC ZZZZ9.99-  BLANK ZERO.
           03  D-CURBAL         PIC ZZZZZ9.99- BLANK ZERO.
           03  FILLER           PIC X(28).

      *-----------------------------------------------------------------
       01  DETAIL2 REDEFINES DETAIL-LINE.
           03  D-BRACKETX       PIC X(22).
           03  FILLER           PIC X.
           03  D-RQTY           PIC Z(8)-.
           03  D-RPCT           PIC ZZ9.99-    BLANK ZERO.
           03  D-RENEW          PIC Z(8)9.99-  BLANK ZERO.
           03  D-LOAN           PIC Z(9)9.99-  BLANK ZERO.
           03  D-LPCT           PIC ZZ9.99-    BLANK ZERO.
           03  D-NET            PIC Z(9)9.99-  BLANK ZERO.
           03  D-NPCT           PIC ZZ9.99-    BLANK ZERO.
           03  D-QTY            PIC Z(8)-.
           03  D-PCT            PIC ZZ9.99-    BLANK ZERO.
           03  D-RECEV          PIC Z(9)9.99-  BLANK ZERO.
           03  D-RECPCT         PIC ZZ9.99-    BLANK ZERO.

      *-----------------------------------------------------------------
       01  DETAIL3 REDEFINES DETAIL-LINE.
           03  FILLER           PIC XX.
           03  D-CD-CODE        PIC X(3).
           03  FILLER           PIC X.
           03  D-CD-DESC        PIC X(24).
           03  FILLER           PIC X(102).

      *-----------------------------------------------------------------
       01  DETAIL4             REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(4).
           03  D-RECTAG         PIC X(11).
           03  FILLER          REDEFINES D-RECTAG.
               05  D-TAG2       PIC X(7).
               05  D-DATE       PIC 9999      BLANK ZERO.
           03  FILLER           PIC X(15).
           03  D-RECEIV         PIC Z(9)9.99- BLANK ZERO.

      *-----------------------------------------------------------------
       01  DETAIL5             REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(5).
           03  D5-BRANCH        PIC ZZZ9.
           03  FILLER           PIC X(3).
           03  D5-ACCOUNTS      PIC 9(8).
           03  FILLER           PIC X(3).
           03  D5-CLASS         PIC 9(2).
           03  FILLER           PIC X(4).
           03  D5-CODE          PIC 9(3).
           03  FILLER           PIC X(100).

      *-----------------------------------------------------------------
       01  RANGE-LINE          REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(30).
           03  RANGE-SEL        PIC X(10).
           03  FILLER          REDEFINES RANGE-SEL.
               05  RANGE-DATE   PIC 9999/ZZ/ZZ.
           03  FILLER          REDEFINES RANGE-SEL.
               05  RANGE-AMT    PIC ZZZ,ZZ9.99.
           03  FILLER          REDEFINES RANGE-SEL.
               05  RANGE-RATE   PIC ZZ,ZZ9.999.
      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBWI/TSTINTW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBWI/LCSCANW.CPY".

       COPY "LIBGB/PROGIDW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBLP/LPCDELW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBLP/LPEARNW.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBLP/RNFOR10W.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/BNKRP1_DEF.CPY".
       COPY "LIBLP/BNKRP1_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/BNKRP1_SCN.CPY".

      ******************************************************************
      *
      *     P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK2-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-GP-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK2-FILE WK-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BNKRP1" TO FORM-PROG VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS.........." TO WR-BUF
              MOVE LEGEND-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.
           PERFORM ENABLE-INTERRUPT.

           IF GROUP-FLAG = "N"
              MOVE BEG-BR TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           PERFORM OPEN-GR1-FILE.
           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.
           IF WGR-SRESULT = 0
              GO TO END-ROUTINE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF NOT IO-GOOD
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-BAD OR BR-NO > END-BR
              GO TO END-ROUTINE.

       MOVE-BRANCH-PATH-INFO.
           MOVE "Y" TO NEW-BRANCH.

           MOVE BR-NO TO HOLD-BRANCH H-BRNO.
           MOVE BR-NAME TO H-BRNAME.
           MOVE 0 TO HOLD-TYPE.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              LP-PATH-OVERRIDE
                                              LTI-PATH-OVERRIDE
                                              LTP-PATH-OVERRIDE.

           MOVE 9999 TO PROGID-SLEEP-COUNTER.
           PERFORM SEND-PROGRESS-INDICATOR.

           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN3-FILE.

           MOVE BR-NO     TO LN-OWNBR
                             QLN3-WBEG-OWNBR
                             QLN3-WEND-OWNBR.
           MOVE BEG-CLASS TO LN-CLASS
                             QLN3-WBEG-CLASS.
           MOVE END-CLASS TO QLN3-WEND-CLASS.
           MOVE 0         TO LN-ACCTNO
                             QLN3-WBEG-ACCTNO.
           MOVE ALL "9"   TO QLN3-WEND-ACCTNO.

           PERFORM START-LN3-FILE.

       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-OF-BRANCH.

           PERFORM SEND-PROGRESS-INDICATOR.
           PERFORM TRAP-INTERRUPT.

           IF ( LN-POCD-NONLOAN )
              GO TO NEXT-LN.

           IF ( LN-CLASS < BEG-CLASS )
              GO TO NEXT-LN.
           IF ( LN-CLASS > END-CLASS )
              GO TO END-OF-BRANCH.

           IF PL = "O"
              IF LN-PLDATE = 0
                 GO TO NEXT-LN.

           IF PL = "S"
              IF LN-PLDATE NOT = 0
                 GO TO NEXT-LN.

           IF SERVA NOT = " "
              IF (SERVA = "O") AND (LN-SERVACCT NOT = "Y")
                 GO TO NEXT-LN
              ELSE
              IF (SERVA = "S") AND (LN-SERVACCT = "Y")
                 GO TO NEXT-LN.

           IF ZBAL = "O"
              IF NOT (LN-CURBAL = 0
                        AND LN-INTBAL = 0
                         AND LN-LCBAL = 0
                          AND LN-OTHBAL = 0)
                 GO TO NEXT-LN.

           IF ZBAL = "S"
              IF LN-CURBAL = 0
                 IF LN-INTBAL = 0
                    IF LN-LCBAL = 0
                       IF LN-OTHBAL = 0
                             GO TO NEXT-LN.


           IF ( SALEFIN NOT = " " )
              IF (SALEFIN = "O") AND (LN-SALEFIN NOT = "Y")
                 GO TO NEXT-LN
              ELSE
                 IF (SALEFIN = "S") AND (LN-SALEFIN = "Y")
                    GO TO NEXT-LN.

      * AN ACCOUNT IS BULK IF LN-MIDSTREAM IS NOT EQUAL TO A SPACE.

           IF ( BULK NOT = " " )
              IF (BULK = "O") AND (LN-MIDSTREAM = " ")
                 GO TO NEXT-LN
              ELSE
                 IF (BULK = "S") AND (LN-MIDSTREAM NOT = " ")
                    GO TO NEXT-LN.

           IF REPO = "O"
              IF NOT (LN-REPOCD = "X" OR "S")
                 GO TO NEXT-LN.

           IF REPO = "S"
              IF LN-REPOCD = "X" OR "S"
                 GO TO NEXT-LN.

           PERFORM ACCESS-COLLATERAL-CODE.
           IF ( IO-BAD )
              PERFORM INVALID-COLLATERAL-CODE
              GO TO SKIP-COLLATERAL-TESTS.

           IF (CD-CO-SECURED = "S") AND (SEC-SECURED NOT = "S")
              GO TO NEXT-LN
           ELSE
           IF (CD-CO-SECURED = "U") AND (SEC-UNSECURED NOT = "U")
              GO TO NEXT-LN
           ELSE
           IF (CD-CO-SECURED = "R") AND (SEC-REAL-ESTATE NOT = "R")
              GO TO NEXT-LN.

       SKIP-COLLATERAL-TESTS.

           MOVE "Y" TO VALID-DATE.

           MOVE BEG-LN-DATE TO NUM-DATE.
           IF LN-SALEFIN = "Y"
              MOVE LN-PURDATE TO SYS-DATE
           ELSE
              IF LN-RESCISSION = "Y"
                 MOVE LN-RESCISSION-DATE TO SYS-DATE
              ELSE
                 IF LN-ENTDATE NOT = LN-LOANDATE
                    MOVE LN-ENTDATE TO SYS-DATE
                 ELSE
                    MOVE LN-LOANDATE TO SYS-DATE.

           IF SYS-DATE < BEG-LN-DATE OR SYS-DATE > END-LN-DATE
              MOVE "N" TO VALID-DATE.

           PERFORM GET-SP.

      * IF PRINTING AMOUNT-FINANCED INSTEAD OF LNAMT, MUST PERFORM
      * LOAN-CALCS HERE FOR SUMMARY ONLY TO GET DISCLOSED-FINAMT

           IF LNAMT-AMTFIN = "A"
              PERFORM LOAN-CALCULATIONS.

           PERFORM LN3-DETAIL.

           GO TO NEXT-LN.

       END-OF-BRANCH.
           PERFORM CLOSE-LN3-FILE.

           PERFORM BRANCH-TOTALS.

           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              PERFORM RANGE-LINES
              MOVE "N" TO RECORDS-FOUND
           ELSE
              PERFORM GRAND-TOTALS
              PERFORM INVALID-COLLATERAL-ACCTS.
              PERFORM RANGE-LINES.

           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE BNKRP1-QUEUE-POS TO Q-POS.
           MOVE BNKRP1-COPIES-POS TO C-POS.
           MOVE BNKRP1-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BR TO SV-BEG-BR
              MOVE END-BR TO SV-END-BR
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP TO SV-ENT-GROUP
              MOVE EXT-EOBUF-BRNO TO BEG-BR END-BR
              MOVE "N" TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE
           ELSE
              IF ( BEG-BR = 0 AND END-BR = 0 )
                 MOVE BEG-BR TO SV-BEG-BR
                 MOVE END-BR TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BR END-BR.

           PERFORM OPEN-CD1-FILE.
           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-BR-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WK2-PATH.
           PERFORM OPEN-WK2-FILE-OUTPUT.
           PERFORM CLOSE-WK2-FILE.
           PERFORM OPEN-WK2-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           IF ( BEG-CLASS = END-CLASS )
              MOVE BEG-CLASS TO H-BEG-CLASS LC-CODE
              PERFORM OPEN-LC1-FILE
              PERFORM READ-LC1-FILE
              PERFORM CLOSE-LC1-FILE
              MOVE LC-DESC   TO H-CLASS-DESC
           ELSE
              MOVE BEG-CLASS TO H-BEG-CLASS
              MOVE "TO"      TO H-TO-CLASS
              MOVE END-CLASS TO H-END-CLASS.

           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO HOLD-SYSDATE.
           MOVE BEG-LN-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY        TO H-BEG-DATE.
           MOVE END-LN-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY        TO H-END-DATE.

           IF LNAMT-AMTFIN = "A"
              MOVE "FINANCED" TO D-TOTAL-TAG D-HEAD-TAG
           ELSE
           IF LNAMT-AMTFIN = "C"
              MOVE "  CURBAL" TO D-TOTAL-TAG D-HEAD-TAG.

           MOVE 2 TO LEV.
           PERFORM CLEAR-TOTALS.
           PERFORM CLEAR-RENEWAL-TOTALS.
           PERFORM CLEAR-PAIDOFF-TOTALS.
           MOVE 0 TO SUB.

       NEXT-BRACKET.
           ADD 1 TO SUB.

           IF ( AMTS-ON-NET-RECEIVABLES ) OR
              ( AMTS-ON-RECEIVABLES     )
              MOVE 0 TO D-BRAMT1(SUB) D-BRAMT1A(SUB)
           ELSE
           IF ( AMTS-ON-APR     ) OR
              ( AMTS-ON-ACCRATE ) OR
              ( AMTS-ON-EFFRATE )
              MOVE 0 TO D-BRAMT1-R(SUB) D-BRAMT1A-R(SUB)
           ELSE
           IF ( AMTS-ON-TERM )
              MOVE 0 TO D-BRAMT1-T(SUB) D-BRAMT1A-T(SUB)
           ELSE
           IF ( AMTS-ON-LOAN-EXPIRATION )
              MOVE 0 TO D-BRAMT1-X(SUB) D-BRAMT1A-X(SUB).

           MOVE " TO " TO D-TAG(SUB).

           IF SUB < 10
              GO TO NEXT-BRACKET.

           MOVE 0 TO SUB SUB1.

       LOAD-BRACKET.
           ADD 1 TO SUB.

           IF AMOUNT-X(SUB) = SPACE
              GO TO LOAD-YEARS.

           IF ( SUB > 1 )
              IF ( AMTS-ON-NET-RECEIVABLES ) OR
                 ( AMTS-ON-RECEIVABLES     )
                 COMPUTE AMT-WK = AMOUNT(SUB1) + .01
                 MOVE AMT-WK TO D-BRAMT1(SUB)
              ELSE
              IF ( AMTS-ON-TERM )
                 COMPUTE AMT-WK = AMOUNT(SUB1) + 1
                 MOVE AMT-WK TO D-BRAMT1-T(SUB)
              ELSE
              IF ( AMTS-ON-APR     ) OR
                 ( AMTS-ON-ACCRATE ) OR
                 ( AMTS-ON-EFFRATE )
                 COMPUTE RATE-WK = AMOUNT-RATE(SUB1) + .0001
                 MOVE RATE-WK TO D-BRAMT1-R(SUB)
              ELSE
              IF ( AMTS-ON-LOAN-EXPIRATION )
                 COMPUTE AMT-WK = AMOUNT(SUB1) + 1
                 MOVE AMT-WK TO D-BRAMT1-X(SUB).

           IF ( AMTS-ON-NET-RECEIVABLES ) OR
              ( AMTS-ON-RECEIVABLES     )
              MOVE AMOUNT(SUB) TO D-BRAMT1A(SUB)
           ELSE
           IF ( AMTS-ON-TERM )
              MOVE AMOUNT(SUB) TO D-BRAMT1A-T(SUB)
           ELSE
           IF ( AMTS-ON-APR     ) OR
              ( AMTS-ON-ACCRATE ) OR
              ( AMTS-ON-EFFRATE )
              MOVE AMOUNT-RATE(SUB) TO D-BRAMT1A-R(SUB)
           ELSE
           IF ( AMTS-ON-LOAN-EXPIRATION )
              MOVE AMOUNT(SUB) TO D-BRAMT1A-X(SUB).

           IF AMOUNT(SUB) = 999999.99
              GO TO LOAD-YEARS.

           ADD 1 TO SUB1.

           IF SUB < 10
              GO TO LOAD-BRACKET.

       LOAD-YEARS.
           MOVE YEAR-END-DATE TO NDTE-DATE.
           MOVE 0 TO SUB.

       LOAD-NEXT-YEAR.
           ADD 1 TO SUB.
           MOVE 12 TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE TO TOTAL-DATE(1, SUB).

           IF SUB < 5
              GO TO LOAD-NEXT-YEAR.

           MOVE 0 TO TOTAL-DATE(1, 6) TOTAL-DATE(1, 7).

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-LN-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-LN-DATE.

           PERFORM FIND-DEFAULT-YEAR-END-DATE.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO RP-TYPE.

           PERFORM FIND-DEFAULT-YEAR-END-DATE.
           MOVE "S  8080YEAR." TO VDU.
           MOVE YEAR-END-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
      
       BR-01.
           MOVE "E  A040BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF VDUSTATUS = "1<"
              MOVE GRSCAN-BUF TO SCAN-BUF
              PERFORM CALL-WINDOW-PROGRAM
              MOVE SCAN-BUF TO GRSCAN-BUF
              IF SCAN-STATUS NOT = "00"
                 GO TO BR-01
              ELSE
                 MOVE "S  A000BR1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.

           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-01.
           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF WGR-WORD-ZERO
              GO TO BRANCH-IS-NUMERIC.

           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           IF WGR-TYPE = "A"
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BR
              MOVE 9999 TO END-BR
              GO TO ENTER-BEG-CLASS.
      
       BRANCH-IS-NUMERIC.
           MOVE "N"           TO GROUP-FLAG.
           MOVE WGR-SRESULT   TO BEG-BR.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BR        TO VDUNUM0.
           PERFORM SCREENIO.

       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
           MOVE VDUNUM0 TO END-BR.

           IF BEG-BR > END-BR
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           IF ( (BEG-BR = 0) AND (END-BR = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           GO TO ENTER-BEG-CLASS.

       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

       ENTER-BEG-CLASS.

           MOVE "ENNN020BEG-CLASS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO BR-02.
           IF (        F1-KEY ) GO TO EXIT-PARM.
           IF (        F3-KEY ) GO TO ENTER-ALL-CLASS.
           IF (        F7-KEY ) GO TO EXIT-PARM.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-CLASS.

           MOVE VDUNUM0 TO BEG-CLASS.

       ENTER-END-CLASS.

           MOVE "ENNN020END-CLASS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-CLASS.
           IF (        F1-KEY ) GO TO EXIT-PARM.
           IF (        F3-KEY ) GO TO ENTER-ALL-CLASS.
           IF (        F7-KEY ) GO TO EXIT-PARM.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-CLASS.

           MOVE VDUNUM0 TO END-CLASS.

           IF ( END-CLASS = ZEROES ) GO TO ENTER-END-CLASS.

           IF ( BEG-CLASS > END-CLASS )
              MOVE "INVALID CLASS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-CLASS.

           GO TO DT-01.

       ENTER-ALL-CLASS.

           MOVE "SNNN020BEG-CLASS." TO VDU.
           MOVE ZEROES              TO VDUNUM0 BEG-CLASS.
           PERFORM SCREENIO.

           MOVE "SNNN020END-CLASS." TO VDU.
           MOVE 99                  TO VDUNUM0 END-CLASS.
           PERFORM SCREENIO.

       DT-01.
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO YEAR1.
           MOVE "ERNM060DT1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-DT.
           IF VDUSTATUS = "1U" GO TO ENTER-END-CLASS.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DT-01.
           MOVE VDU-DATE-YYYYMMDD TO BEG-LN-DATE.

       DT-02.
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO YEAR1.
           MOVE "ERNM060DT2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DT-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DT-02.
           MOVE VDU-DATE-YYYYMMDD TO END-LN-DATE.

           IF END-LN-DATE < BEG-LN-DATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO DT-01.

           GO TO YEAR1.

       ALL-DT.
           MOVE "S  8080DT1." TO VDU.
           MOVE JULIAN-BEG TO BEG-LN-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080DT2." TO VDU.
           MOVE JULIAN-END TO END-LN-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

       YEAR1.
           PERFORM FIND-DEFAULT-YEAR-END-DATE.
           MOVE "S  8080YEAR." TO VDU.
           MOVE YEAR-END-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "ERNM060YEAR." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.

           IF ( UP-KEY )
              IF ( EXT-ROUTE-BUF = "00" )
                 GO TO DT-02
              ELSE
                 GO TO ENTER-END-CLASS.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DT-01.
           MOVE VDU-DATE-YYYYMMDD TO YEAR-END-DATE.

           IF YEAR-END-DATE = 0 GO TO YEAR1.

           MOVE YEAR-END-DATE TO NUM-DATE.
           PERFORM TSTDAT.
           IF NUM-DATE = 0
              MOVE " INVALID YEAR-END-DATE DATE!" TO MESS
              PERFORM SEND-MESS
              GO TO YEAR1.

       ZB-01.
           MOVE "E  A010ZERO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO YEAR1.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ZB-01.
           MOVE VDUBUF TO ZBAL.
           IF NOT (ZBAL = "I" OR "S" OR "O") GO TO ZB-01.

       PL-01.
           MOVE "E  A010PL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ZB-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO PL-01.
           MOVE VDUBUF TO PL.
           IF NOT (PL = "I" OR "S" OR "O") GO TO PL-01.

       SERVA-01.

           MOVE "E  A010SERVA." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO PL-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO SERVA-01.
           MOVE VDUBUF TO SERVA.
           IF NOT (SERVA = "I" OR "S" OR "O") GO TO SERVA-01.

       SALEFIN-01.
           MOVE "E  A010SFINANCE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO SERVA-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO SALEFIN-01.
           MOVE VDUBUF TO SALEFIN.
           IF NOT (SALEFIN = "I" OR "S" OR "O") GO TO SALEFIN-01.

       BULK-01.
           IF ( SALEFIN = "S" )
              MOVE "S  A010BULK." TO VDU
              MOVE "S" TO BULK VDUBUF
              PERFORM SCREENIO
              GO TO REPO-01.

           MOVE "E  A010BULK." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO SALEFIN-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BULK-01.
           MOVE VDUBUF TO BULK.
           IF NOT (BULK = "I" OR "S" OR "O") GO TO BULK-01.

       REPO-01.

           MOVE "E  A010REPOS." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              IF ( SALEFIN = "S" )
                 GO TO SALEFIN-01.
           IF VDUSTATUS = "1U" GO TO BULK-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO REPO-01.
           MOVE VDUBUF TO REPO.
           IF NOT (REPO = "I" OR "S" OR "O") GO TO REPO-01.

       SECURITY-1.

           MOVE "E  A050SECURITY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REPO-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO SECURITY-1.
           MOVE VDUBUF TO SECURITY-TYPE.

           IF SECURITY-TYPE = SPACES
              GO TO SECURITY-1.

           IF NOT (SEC-SECURED     = SPACES OR "S") GO TO SECURITY-1.
           IF NOT (SEC-UNSECURED   = SPACES OR "U") GO TO SECURITY-1.
           IF NOT (SEC-REAL-ESTATE = SPACES OR "R") GO TO SECURITY-1.

       LNAMT-LA.
           MOVE "E  A010LNAMT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO SECURITY-1.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO LNAMT-LA.
           MOVE VDUBUF TO LNAMT-AMTFIN.
           IF LNAMT-AMTFIN = "L" OR "C" GO TO BAMT-RN.
           IF LNAMT-AMTFIN NOT = "A" GO TO LNAMT-LA.

       BAMT-RN.

           MOVE "E  A010BAMT." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO LNAMT-LA.
           IF (        F1-KEY ) GO TO EXIT-PARM.
           IF (        F7-KEY ) GO TO EXIT-PARM.
           IF ( NOT ENTER-KEY ) GO TO BAMT-RN.

           MOVE VDUBUF TO AMTS-ON.

           IF ( NOT AMTS-ON-VALID )
              GO TO BAMT-RN.

       AMT-01.
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              MOVE "ENNN053AMT01." TO VDU
           ELSE
              MOVE "ENNN062AMT01." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BAMT-RN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO AMT-01.
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              MOVE VDUNUM3 TO AMOUNT-RATE(1)
           ELSE
              MOVE VDUNUM2 TO AMT01-BUF.
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              IF AMOUNT-RATE(1) = 99999.999
                 GO TO RP-TYPE
              END-IF
           ELSE
              IF AMT01-BUF = 999999.99 GO TO RP-TYPE.
           IF AMT01-BUF NOT = 0 GO TO AMT-02.
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              MOVE "SNNN053AMT01." TO VDU
              MOVE 99999.999 TO AMOUNT-RATE(1) VDUNUM3
           ELSE
              MOVE "SNNN062AMT01." TO VDU
              MOVE 999999.99 TO AMT01-BUF VDUNUM2.
           PERFORM SCREENIO.
           GO TO RP-TYPE.

       AMT-02.
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              MOVE "ENNN053AMT02." TO VDU
           ELSE
              MOVE "ENNN062AMT02." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO AMT-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO AMT-02.
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              MOVE VDUNUM3 TO AMOUNT-RATE(2)
           ELSE
              MOVE VDUNUM2 TO AMT02-BUF.
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              IF AMOUNT-RATE(2) = 99999.999
                 GO TO RP-TYPE
              END-IF
           ELSE
              IF AMT02-BUF = 999999.99 GO TO RP-TYPE.
           IF AMT02-BUF NOT = 0 GO TO AMT-03.
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              MOVE "SNNN053AMT02." TO VDU
              MOVE 99999.999 TO AMOUNT-RATE(2) VDUNUM3
           ELSE
              MOVE "SNNN062AMT02." TO VDU
              MOVE 999999.99 TO AMT02-BUF VDUNUM2.
           PERFORM SCREENIO.
           GO TO RP-TYPE.
      
       AMT-03.
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              MOVE "ENNN053AMT03." TO VDU
           ELSE
              MOVE "ENNN062AMT03." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO AMT-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO AMT-03.
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              MOVE VDUNUM3 TO AMOUNT-RATE(3)
           ELSE
              MOVE VDUNUM2 TO AMT03-BUF.
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              IF AMOUNT-RATE(3) = 99999.999
                 GO TO RP-TYPE
              END-IF
           ELSE
              IF AMT03-BUF = 999999.99 GO TO RP-TYPE.
           IF AMT03-BUF NOT = 0 GO TO AMT-04.
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              MOVE "SNNN053AMT03." TO VDU
              MOVE 99999.999 TO AMOUNT-RATE(3) VDUNUM3
           ELSE
              MOVE "SNNN062AMT03." TO VDU
              MOVE 999999.99 TO AMT03-BUF VDUNUM2.
           PERFORM SCREENIO.
           GO TO RP-TYPE.

      
       AMT-04.
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              MOVE "ENNN053AMT04." TO VDU
           ELSE
              MOVE "ENNN062AMT04." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO AMT-03.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO AMT-04.
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              MOVE VDUNUM3 TO AMOUNT-RATE(4)
           ELSE
              MOVE VDUNUM2 TO AMT04-BUF.
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              IF AMOUNT-RATE(4) = 99999.999
                 GO TO RP-TYPE
              END-IF
           ELSE
              IF AMT04-BUF = 999999.99 GO TO RP-TYPE.
           IF AMT04-BUF NOT = 0 GO TO AMT-05.
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              MOVE "SNNN053AMT04." TO VDU
              MOVE 99999.999 TO AMOUNT-RATE(4) VDUNUM3
           ELSE
              MOVE "SNNN062AMT04." TO VDU
              MOVE 999999.99 TO AMT04-BUF VDUNUM2.
           PERFORM SCREENIO.
           GO TO RP-TYPE.

       AMT-05.
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              MOVE "ENNN053AMT05." TO VDU
           ELSE
              MOVE "ENNN062AMT05." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO AMT-04.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO AMT-05.
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              MOVE VDUNUM3 TO AMOUNT-RATE(5)
           ELSE
              MOVE VDUNUM2 TO AMT05-BUF.
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              IF AMOUNT-RATE(5) = 99999.999
                 GO TO RP-TYPE
              END-IF
           ELSE
              IF AMT05-BUF = 999999.99 GO TO RP-TYPE.
           IF AMT05-BUF NOT = 0 GO TO AMT-06.
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              MOVE "SNNN053AMT05." TO VDU
              MOVE 99999.999 TO AMOUNT-RATE(5) VDUNUM3
           ELSE
              MOVE "SNNN062AMT05." TO VDU
              MOVE 999999.99 TO AMT05-BUF VDUNUM2.
           PERFORM SCREENIO.
           GO TO RP-TYPE.
      
       AMT-06.
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              MOVE "ENNN053AMT06." TO VDU
           ELSE
              MOVE "ENNN062AMT06." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO AMT-05.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO AMT-06.
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              MOVE VDUNUM3 TO AMOUNT-RATE(6)
           ELSE
              MOVE VDUNUM2 TO AMT06-BUF.
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              IF AMOUNT-RATE(6) = 99999.999
                 GO TO RP-TYPE
              END-IF
           ELSE
              IF AMT06-BUF = 999999.99 GO TO RP-TYPE.
           IF AMT06-BUF NOT = 0 GO TO AMT-07.
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              MOVE "SNNN053AMT06." TO VDU
              MOVE 99999.999 TO AMOUNT-RATE(6) VDUNUM3
           ELSE
              MOVE "SNNN062AMT06." TO VDU
              MOVE 999999.99 TO AMT06-BUF VDUNUM2.
           PERFORM SCREENIO.
           GO TO RP-TYPE.
      
       AMT-07.
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              MOVE "ENNN053AMT07." TO VDU
           ELSE
              MOVE "ENNN062AMT07." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO AMT-06.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO AMT-07.
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              MOVE VDUNUM3 TO AMOUNT-RATE(7)
           ELSE
              MOVE VDUNUM2 TO AMT07-BUF.
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              IF AMOUNT-RATE(7) = 99999.999
                 GO TO RP-TYPE
              END-IF
           ELSE
              IF AMT07-BUF = 999999.99 GO TO RP-TYPE.
           IF AMT07-BUF NOT = 0 GO TO AMT-08.
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              MOVE "SNNN053AMT07." TO VDU
              MOVE 99999.999 TO AMOUNT-RATE(7) VDUNUM3
           ELSE
              MOVE "SNNN062AMT07." TO VDU
              MOVE 999999.99 TO AMT07-BUF VDUNUM2.
           PERFORM SCREENIO.
           GO TO RP-TYPE.
      
       AMT-08.
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              MOVE "ENNN053AMT08." TO VDU
           ELSE
              MOVE "ENNN062AMT08." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO AMT-07.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO AMT-08.
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              MOVE VDUNUM3 TO AMOUNT-RATE(8)
           ELSE
              MOVE VDUNUM2 TO AMT08-BUF.
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              IF AMOUNT-RATE(8) = 99999.999
                 GO TO RP-TYPE
              END-IF
           ELSE
              IF AMT08-BUF = 999999.99 GO TO RP-TYPE.
           IF AMT08-BUF NOT = 0 GO TO AMT-09.
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              MOVE "SNNN053AMT08." TO VDU
              MOVE 99999.999 TO AMOUNT-RATE(8) VDUNUM3
           ELSE
              MOVE "SNNN062AMT08." TO VDU
              MOVE 999999.99 TO AMT08-BUF VDUNUM2.
           PERFORM SCREENIO.
           GO TO RP-TYPE.
      
       AMT-09.
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              MOVE "ENNN053AMT09." TO VDU
           ELSE
              MOVE "ENNN062AMT09." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO AMT-08.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO AMT-09.
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              MOVE VDUNUM3 TO AMOUNT-RATE(9)
           ELSE
              MOVE VDUNUM2 TO AMT09-BUF.
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              IF AMOUNT-RATE(9) = 99999.999
                 GO TO RP-TYPE
              END-IF
           ELSE
              IF AMT09-BUF = 999999.99 GO TO RP-TYPE.
           IF AMT09-BUF NOT = 0 GO TO AMT-10.
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              MOVE "SNNN053AMT09." TO VDU
              MOVE 99999.999 TO AMOUNT-RATE(9) VDUNUM3
           ELSE
              MOVE "SNNN062AMT09." TO VDU
              MOVE 999999.99 TO AMT09-BUF VDUNUM2.
           PERFORM SCREENIO.
           GO TO RP-TYPE.
      
       AMT-10.
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              MOVE "ENNN053AMT10." TO VDU
           ELSE
              MOVE "ENNN062AMT10." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO AMT-09.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO AMT-10.
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              MOVE VDUNUM3 TO AMOUNT-RATE(10)
           ELSE
              MOVE VDUNUM2 TO AMT10-BUF.
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              MOVE "SNNN053AMT10." TO VDU
              MOVE 99999.999 TO AMOUNT-RATE(10) VDUNUM3
           ELSE
              MOVE "SNNN062AMT10." TO VDU
              MOVE 999999.99 TO AMT10-BUF VDUNUM2.
           PERFORM SCREENIO.
      
       RP-TYPE.
           MOVE "E  A010SUMYN." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO AMT-10.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO RP-TYPE.
           MOVE VDUBUF TO REPORT-TYPE.

           IF NOT (REPORT-TYPE = "D" OR "S" OR "C" OR "B")
              MOVE
              "D-DETAIL,S-SUMMARY ONLY,C-CORP. SUMMARY,B-BRACKETS ONLY"
                                                    TO MESS
              PERFORM SEND-MESS
              GO TO RP-TYPE.

       NET-UNEARNED-YN.
           MOVE "E  A010UNEARNED." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO RP-TYPE.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO NET-UNEARNED-YN.
           MOVE VDUBUF TO NET-UNEARNED-FG.
           IF NET-UNEARNED-FG NOT = "Y"
              IF NET-UNEARNED-FG NOT = "N"
                 GO TO NET-UNEARNED-YN.

       OK-YN.
           MOVE " " TO EXT-EOBUF-ERRCD.

           MOVE 0 TO SUB SUB1.

       CHECK-NEXT-AMT.
           ADD 1 TO SUB.

           IF SUB > 1
              IF AMOUNT(SUB) NOT > AMOUNT(SUB1)
                 MOVE "BRACKET AMTS MUST BE IN ASCENDING ORDER" TO MESS
                 PERFORM SEND-MESS
                 GO TO AMT-01.

           IF AMOUNT(SUB) = 999999.99
              GO TO EXIT-PARM.

           ADD 1 TO SUB1.

           IF SUB < 10
              GO TO CHECK-NEXT-AMT.

       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************

           IF GROUP-FLAG = "Y"
              MOVE "S  A040BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A040BR2." TO VDU
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BR TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BR TO VDUNUM0
              PERFORM SCREENIO.

           MOVE "SNNN020BEG-CLASS." TO VDU.
           MOVE BEG-CLASS           TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020END-CLASS." TO VDU.
           MOVE END-CLASS           TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  8080DT1." TO VDU.
           MOVE BEG-LN-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
      
           MOVE "S  8080DT2." TO VDU.
           MOVE END-LN-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
      
           PERFORM FIND-DEFAULT-YEAR-END-DATE.
           MOVE "S  8080YEAR." TO VDU.
           MOVE YEAR-END-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
      
           MOVE "S  A010ZERO." TO VDU.
           MOVE ZBAL TO VDUBUF.
           PERFORM SCREENIO.
      
           MOVE "S  A010PL." TO VDU.
           MOVE PL TO VDUBUF.
           PERFORM SCREENIO.
      
           MOVE "S  A010SERVA." TO VDU.
           MOVE SERVA TO VDUBUF.
           PERFORM SCREENIO.
      
           MOVE "S  A010SFINANCE." TO VDU.
           MOVE SALEFIN TO VDUBUF.
           PERFORM SCREENIO.
      
           MOVE "S  A010BULK." TO VDU.
           MOVE BULK TO VDUBUF.
           PERFORM SCREENIO.
      
           MOVE "S  A010REPOS." TO VDU.
           MOVE REPO TO VDUBUF.
           PERFORM SCREENIO.
      
           MOVE "S  A050SECURITY." TO VDU.
           MOVE SECURITY-TYPE TO VDUBUF.
           PERFORM SCREENIO.
      
           MOVE "S  A010LNAMT." TO VDU.
           MOVE LNAMT-AMTFIN TO VDUBUF.
           PERFORM SCREENIO.
      
           MOVE "S  A010BAMT." TO VDU.
           MOVE AMTS-ON TO VDUBUF.
           PERFORM SCREENIO.
      
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              MOVE "SNNN053AMT01." TO VDU
              MOVE AMOUNT-RATE(1) TO VDUNUM3
           ELSE
              MOVE "SNNN062AMT01." TO VDU
              MOVE AMT01-BUF TO VDUNUM2.
           PERFORM SCREENIO.
      
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              MOVE "SNNN053AMT02." TO VDU
              MOVE AMOUNT-RATE(2) TO VDUNUM3
           ELSE
              MOVE "SNNN062AMT02." TO VDU
              MOVE AMT02-BUF TO VDUNUM2.
           PERFORM SCREENIO.
      
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              MOVE "SNNN053AMT03." TO VDU
              MOVE AMOUNT-RATE(3) TO VDUNUM3
           ELSE
              MOVE "SNNN062AMT03." TO VDU
              MOVE AMT03-BUF TO VDUNUM2.
           PERFORM SCREENIO.
      
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              MOVE "SNNN053AMT04." TO VDU
              MOVE AMOUNT-RATE(4) TO VDUNUM3
           ELSE
              MOVE "SNNN062AMT04." TO VDU
              MOVE AMT04-BUF TO VDUNUM2.
           PERFORM SCREENIO.
      
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              MOVE "SNNN053AMT05." TO VDU
              MOVE AMOUNT-RATE(5) TO VDUNUM3
           ELSE
              MOVE "SNNN062AMT05." TO VDU
              MOVE AMT05-BUF TO VDUNUM2.
           PERFORM SCREENIO.
      
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              MOVE "SNNN053AMT06." TO VDU
              MOVE AMOUNT-RATE(6) TO VDUNUM3
           ELSE
              MOVE "SNNN062AMT06." TO VDU
              MOVE AMT06-BUF TO VDUNUM2.
           PERFORM SCREENIO.
      
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              MOVE "SNNN053AMT07." TO VDU
              MOVE AMOUNT-RATE(7) TO VDUNUM3
           ELSE
              MOVE "SNNN062AMT07." TO VDU
              MOVE AMT07-BUF TO VDUNUM2.
           PERFORM SCREENIO.
      
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              MOVE "SNNN053AMT08." TO VDU
              MOVE AMOUNT-RATE(8) TO VDUNUM3
           ELSE
              MOVE "SNNN062AMT08." TO VDU
              MOVE AMT08-BUF TO VDUNUM2.
           PERFORM SCREENIO.
      
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              MOVE "SNNN053AMT09." TO VDU
              MOVE AMOUNT-RATE(9) TO VDUNUM3
           ELSE
              MOVE "SNNN062AMT09." TO VDU
              MOVE AMT09-BUF TO VDUNUM2.
           PERFORM SCREENIO.
      
           IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
              MOVE "SNNN053AMT10." TO VDU
              MOVE AMOUNT-RATE(10) TO VDUNUM3
           ELSE
              MOVE "SNNN062AMT10." TO VDU
              MOVE AMT10-BUF TO VDUNUM2.
           PERFORM SCREENIO.
      
           MOVE "S  A010SUMYN." TO VDU.
           MOVE REPORT-TYPE TO VDUBUF.
           PERFORM SCREENIO.

      ****************************************
       FIND-DEFAULT-YEAR-END-DATE SECTION.
      ****************************************

      * NOTE: NEED TO READ THE GPFILE HERE AND NOT USE GPENV;
      *       SOME GP FIELDS MAY HAVE CHANGED SINCE GPENV WAS SET SO
      *       NEED TO MAKE SURE WE HAVE THE LATEST UP-TP-DATE GP-REC.
      *       KEC 2019-0123 A32 - GPFILE GP-REC MODIFICATION 

           MOVE 0 TO YEAR-END-DATE.

           PERFORM OPEN-GP-FILE.
           MOVE 1 TO GP-KEY.
           PERFORM READ-GP-FILE.
           IF IO-BAD
              GO TO IO-ERROR.
           PERFORM CLOSE-GP-FILE.

           MOVE GP-END-FISCAL-DATE(12) TO YEAR-END-DATE.

      ******************************************************************
       LN3-DETAIL SECTION.
      ******************************************************************

           IF (REPORT-TYPE NOT = "D") OR (VALID-DATE = "N")
              GO TO END-LN3-DETAIL.

      * IF TRANSFERED IN, NEED A/R BUT NOT VOLUME:
      * ONLY SKIP THE TRANSFERRED ACCOUNTS IF THE BRACKET SELECTION
      * IS "NET RECV" OR "RECV"

           IF LN-BR-TFR-FG = "Y"
              IF AMTS-ON-NET-RECEIVABLES OR AMTS-ON-RECEIVABLES
                 GO TO END-LN3-DETAIL.

           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO D-LOAN-NUM.
           MOVE LN-SSNO(1)       TO D-SOC-SEC.
           INSPECT D-SOC-SEC REPLACING ALL "/" BY "-".
           PERFORM MOVE-BORROWER.
           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY      TO D-LOANDATE.
           MOVE LN-ORGBR         TO D-ORGBR.
           MOVE "-"              TO D-H3.
           MOVE LN-ORGST         TO D-ORGST.
           MOVE LN-SRCD          TO D-SRCD.
           MOVE LN-COLLCD        TO D-COLLCD.
           MOVE LN-PURCD         TO D-PURCD.
           MOVE LN-MIDSTREAM     TO D-BULK.
           MOVE LN-REPOCD        TO D-REPO.

           IF LNAMT-AMTFIN = "A"
              MOVE DISCLOSED-FINAMT TO D-LNAMT
           ELSE
              IF LNAMT-AMTFIN = "C"
                 MOVE LN-CURBAL     TO D-LNAMT
              ELSE
                 MOVE LN-LNAMT      TO D-LNAMT.

           ADD LN-OTHBAL  TO LN-OT2BAL GIVING D-OTHBAL.
           ADD LN-INTBAL  TO LN-LCBAL  GIVING D-INTBAL.
           MOVE LN-CURBAL TO D-CURBAL.
           PERFORM WRITE-DETAIL-LINE.

       END-LN3-DETAIL.
           IF VALID-DATE = "Y"
              IF (LN-BR-TFR-FG NOT = "Y") OR
                  (LN-BR-TFR-FG = "Y" AND
               (NOT (AMTS-ON-NET-RECEIVABLES AND AMTS-ON-RECEIVABLES)))
                 PERFORM UPDATE-LNAMT-TOTALS.

      * TRANSFERRED ACCOUNTS - ONLY SKIP THEM IF REPORTING ON
      * NET RECV OR RECV.

           COMPUTE HOLD-RENEW =
               (LN-PRBAL + LN-PRINTDUE)
                     - LN-PRFINCHGRB - LN-PRINSRB.

           PERFORM UPDATE-RECEV-TOTALS.
           PERFORM CREATE-TOTAL-RECS.

           IF LN-PLDATE = 0 OR PL = "O"
              PERFORM FUTURE-RECEIVABLES-DUE.

      ******************************************************************
       UPDATE-LNAMT-TOTALS SECTION.
      ******************************************************************
           COMPUTE HOLD-RENEW =
              (LN-PRBAL + LN-PRINTDUE) - LN-PRFINCHGRB - LN-PRINSRB.

           MOVE 0 TO SUB.

       UPDATE-LNAMT-NEXT.
           ADD 1 TO SUB.

           IF AMOUNT(SUB) = 0
              GO TO UPDATE-LNAMT-FOUND.

           IF ( AMTS-ON-NET-RECEIVABLES ) OR
              ( AMTS-ON-RECEIVABLES     )
              IF LNAMT-AMTFIN = "L"
                 IF LN-LNAMT NOT > AMOUNT(SUB)
                    GO TO UPDATE-LNAMT-FOUND
                 ELSE
                    NEXT SENTENCE
              ELSE
              IF LNAMT-AMTFIN = "C"
                 IF LN-CURBAL NOT > AMOUNT(SUB)
                    GO TO UPDATE-LNAMT-FOUND
                 END-IF
              ELSE
                 IF DISCLOSED-FINAMT NOT > AMOUNT(SUB)
                    GO TO UPDATE-LNAMT-FOUND.

           IF ( AMTS-ON-TERM )
              IF ( LN-ORGTERM NOT > AMOUNT(SUB) )
                 GO TO UPDATE-LNAMT-FOUND.

           IF ( AMTS-ON-APR )
              IF LN-APRATE NOT > AMOUNT-RATE(SUB)
                 GO TO UPDATE-LNAMT-FOUND.

           IF ( AMTS-ON-ACCRATE )
              IF LN-ACCRATE NOT > AMOUNT-RATE(SUB)
                 GO TO UPDATE-LNAMT-FOUND.

           IF ( AMTS-ON-EFFRATE )
              IF LN-EFFRATE NOT > AMOUNT-RATE(SUB)
                 GO TO UPDATE-LNAMT-FOUND.

           IF ( AMTS-ON-LOAN-EXPIRATION )
              PERFORM GET-MONTHS-TILL-EXPIRATION
              IF ( MONTHS-TILL-EXPIRATION NOT > AMOUNT(SUB) )
                 GO TO UPDATE-LNAMT-FOUND.

           IF SUB < 10
              GO TO UPDATE-LNAMT-NEXT.

       UPDATE-LNAMT-FOUND.
           ADD 1 TO TOTAL-RQTY(1, SUB) TOTAL-RQTY(1, 11).
           ADD HOLD-RENEW TO TOTAL-REN(1, SUB) TOTAL-REN(1, 11).

           IF LNAMT-AMTFIN = "L"
              ADD LN-LNAMT TO TOTAL-LAMT(1, SUB) TOTAL-LAMT(1, 11)
           ELSE
           IF LNAMT-AMTFIN = "C"
              ADD LN-CURBAL TO TOTAL-LAMT(1, SUB) TOTAL-LAMT(1, 11)
           ELSE
              ADD DISCLOSED-FINAMT TO TOTAL-LAMT(1, SUB)
                                      TOTAL-LAMT(1, 11).

      ******************************************************************
       UPDATE-RECEV-TOTALS SECTION.
      ******************************************************************
           MOVE YEAR-END-DATE TO IBPC-DATE.
           PERFORM IBPC-TEST.
           MOVE 0 TO REB-REBATE.

           IF IBPC-FG = "P"
              IF LN-INT-REBATE = " " OR "C"
                 MOVE YEAR-END-DATE TO REB-PAYDATE
                 MOVE LN-ORGTERM TO REB-ORGTERM
                 MOVE 7 TO REB-SUB
                 MOVE "RI" TO REB-LPTRCD
                 MOVE LN-INTCHG TO REB-TOTCHG
                 PERFORM REBATE.

           MOVE 0 TO SUB.

       UPDATE-RECEV-NEXT.
           ADD 1 TO SUB.
           IF NET-UNEARNED-FG = "Y"
              PERFORM COMPUTE-EARNINGS
              COMPUTE NET-WK = LN-CURBAL - EARN-TOT-UNEARN
           ELSE
              COMPUTE NET-WK = LN-CURBAL - REB-REBATE.

           IF AMOUNT(SUB) = 0
              IF ( AMTS-ON-RECEIVABLES )
                 IF LN-CURBAL > 0
                    GO TO UPDATE-RECEV-FOUND
                 ELSE
                    GO TO UPDATE-RECEV-EXIT
              ELSE
              IF ( AMTS-ON-NET-RECEIVABLES )
                 IF NET-WK > 0
                    GO TO UPDATE-RECEV-FOUND
                 ELSE
                    GO TO UPDATE-RECEV-EXIT
              ELSE
              IF ( AMTS-ON-APR             ) OR
                 ( AMTS-ON-ACCRATE         ) OR
                 ( AMTS-ON-EFFRATE         ) OR
                 ( AMTS-ON-TERM            ) OR
                 ( AMTS-ON-LOAN-EXPIRATION )
                 IF LN-CURBAL > 0
                    GO TO UPDATE-RECEV-FOUND
                 ELSE
                    GO TO UPDATE-RECEV-EXIT.

           IF ( AMTS-ON-RECEIVABLES )
              IF LN-CURBAL NOT > AMOUNT(SUB)
                 IF LN-CURBAL > 0
                    GO TO UPDATE-RECEV-FOUND
                 ELSE
                    GO TO UPDATE-RECEV-EXIT.

           IF ( AMTS-ON-NET-RECEIVABLES )
              IF NET-WK NOT > AMOUNT(SUB)
                 IF NET-WK > 0
                    GO TO UPDATE-RECEV-FOUND
                 ELSE
                    GO TO UPDATE-RECEV-EXIT.

           IF ( AMTS-ON-TERM )
              IF LN-ORGTERM NOT > AMOUNT(SUB)
                 IF LN-CURBAL > 0
                    GO TO UPDATE-RECEV-FOUND
                 ELSE
                    GO TO UPDATE-RECEV-EXIT.

           IF ( AMTS-ON-APR )
              IF LN-APRATE NOT > AMOUNT-RATE(SUB)
                 IF LN-CURBAL > 0
                    GO TO UPDATE-RECEV-FOUND
                 ELSE
                    GO TO UPDATE-RECEV-EXIT.

           IF ( AMTS-ON-ACCRATE )
              IF LN-ACCRATE NOT > AMOUNT-RATE(SUB)
                 IF LN-CURBAL > 0
                    GO TO UPDATE-RECEV-FOUND
                 ELSE
                    GO TO UPDATE-RECEV-EXIT.

           IF ( AMTS-ON-EFFRATE )
              IF LN-EFFRATE NOT > AMOUNT-RATE(SUB)
                 IF LN-CURBAL > 0
                    GO TO UPDATE-RECEV-FOUND
                 ELSE
                    GO TO UPDATE-RECEV-EXIT.

           IF ( AMTS-ON-LOAN-EXPIRATION )
              PERFORM GET-MONTHS-TILL-EXPIRATION
              IF ( MONTHS-TILL-EXPIRATION NOT > AMOUNT(SUB) )
                 IF ( LN-CURBAL > ZEROES )
                    GO TO UPDATE-RECEV-FOUND
                 ELSE
                    GO TO UPDATE-RECEV-EXIT.

           IF SUB < 10
              GO TO UPDATE-RECEV-NEXT.

       UPDATE-RECEV-FOUND.
           IF LN-PLDATE = 0 OR PL = "O"
              ADD 1 TO TOTAL-QTY(1, SUB) TOTAL-QTY(1, 11)
              ADD LN-CURBAL TO TOTAL-BAL(1, SUB) TOTAL-BAL(1, 11)
              IF NET-UNEARNED-FG = "Y"
                 PERFORM COMPUTE-EARNINGS
                 COMPUTE TOTAL-NET(1, SUB) = TOTAL-NET(1, SUB) +
                                     (LN-CURBAL - EARN-TOT-UNEARN)
                 COMPUTE TOTAL-NET(1, 11) = TOTAL-NET(1, 11) +
                                     (LN-CURBAL - EARN-TOT-UNEARN)
              ELSE
                 COMPUTE TOTAL-NET(1, SUB) = TOTAL-NET(1, SUB) +
                                             (LN-CURBAL - REB-REBATE)
                 COMPUTE TOTAL-NET(1, 11) = TOTAL-NET(1, 11) +
                                             (LN-CURBAL - REB-REBATE).

       UPDATE-RECEV-EXIT.
           EXIT.

      ******************************************************************
       CREATE-TOTAL-RECS SECTION.
      ******************************************************************

      * NUMBERS USED INSTEAD OF TYPE TO FORCE REPORT ORDER

           MOVE 1 TO SAVE-WK-TYPE.
           MOVE LN-COLLCD TO SAVE-WK-CODE.
           PERFORM CREATE-WK.

           MOVE 2 TO SAVE-WK-TYPE.
           MOVE LN-SRCD TO SAVE-WK-CODE.
           PERFORM CREATE-WK.

           MOVE 3 TO SAVE-WK-TYPE.
           MOVE LN-PURCD TO SAVE-WK-CODE.
           PERFORM CREATE-WK.

      * PROCEEDS LESS THAN 10%  

           PERFORM COMPUTE-RNFOR10.

           IF ( RNFOR10-FG = "Y" )
              MOVE 4 TO SAVE-WK-TYPE
              MOVE LN-SRCD TO SAVE-WK-CODE
              PERFORM CREATE-WK.

           MOVE "N" TO RNFOR10-FG.

      * PAIDOFFS OTHER THAN RENEWALS 

           IF ( LN-POCD NOT = SPACES ) AND ( LN-POCD NOT = "10" ) AND
              ( LN-POCD NOT = "RN"   ) AND ( LN-POCD NOT = "PB" ) AND
              ( LN-POCD NOT = "RB"   ) AND ( LN-POCD NOT = "RO" )
              PERFORM GET-LAST-PAYMENT
              IF VALID-PAYOFF-DATE = "Y"
                 MOVE "Y" TO PAIDOFF-FLAG
                 MOVE 5 TO SAVE-WK-TYPE
                 MOVE LN-POCD TO SAVE-WK-CODE
                 PERFORM CREATE-WK.

           MOVE "N" TO PAIDOFF-FLAG.

           MOVE 0 TO INS-STATUS.

           IF LN-INSPREM(1) > 0 AND LN-CL-REBATE NOT = "C"
              MOVE 1 TO INS-STATUS
              MOVE 6 TO SAVE-WK-TYPE
              MOVE SPACE TO SAVE-WK-CODE
              PERFORM CREATE-WK.

           IF LN-INSPREM(2) > 0 AND LN-AH-REBATE NOT = "C"
              MOVE 1 TO INS-STATUS
              MOVE 7 TO SAVE-WK-TYPE
              MOVE SPACE TO SAVE-WK-CODE
              PERFORM CREATE-WK.

           IF LN-INSPREM(3) > 0 AND LN-PP-REBATE NOT = "C"
              MOVE 1 TO INS-STATUS
              MOVE 8 TO SAVE-WK-TYPE
              MOVE SPACE TO SAVE-WK-CODE
              PERFORM CREATE-WK.

           IF LN-INSPREM(4) > 0 AND LN-O1-REBATE NOT = "C"
              MOVE 1 TO INS-STATUS
              MOVE 9 TO SAVE-WK-TYPE
              MOVE SPACE TO SAVE-WK-CODE
              PERFORM CREATE-WK.

           IF LN-INSPREM(5) > 0 AND LN-O2-REBATE NOT = "C"
              MOVE 1 TO INS-STATUS
              MOVE 10 TO SAVE-WK-TYPE
              MOVE SPACE TO SAVE-WK-CODE
              PERFORM CREATE-WK.

           IF LN-INSPREM(6) > 0 AND LN-O3-REBATE NOT = "C"
              MOVE 1 TO INS-STATUS
              MOVE 11 TO SAVE-WK-TYPE
              MOVE SPACE TO SAVE-WK-CODE
              PERFORM CREATE-WK.

           IF NOT INS-FOUND
              MOVE 12 TO SAVE-WK-TYPE
              MOVE SPACE TO SAVE-WK-CODE
              PERFORM CREATE-WK.

      ******************************************************************
       CREATE-WK SECTION.
      ******************************************************************

           MOVE LN-OWNBR TO SAVE-WK-BR.

       SAVE-WORK-KEY.
           MOVE SAVE-WK-KEY TO WK-KEY.
           PERFORM READ-WK-FILE.

           IF IO-GOOD
              GO TO CREATE-WK-UPDATE.

           MOVE " " TO WK-REC.
           MOVE SAVE-WK-KEY TO WK-KEY.

           MOVE 0 TO WK-RQTY WK-RENEWED WK-LNAMT WK-NET
                     WK-QTY WK-BALANCE.

           PERFORM WRITE-WK-FILE.
           IF NOT IO-GOOD
              GO TO IO-ERROR.

           GO TO SAVE-WORK-KEY.

       CREATE-WK-UPDATE.
           IF ( VALID-DATE = "Y" ) OR ( PAIDOFF-FLAG = "Y" )
              IF (LN-BR-TFR-FG NOT = "Y") OR
                 (LN-BR-TFR-FG = "Y" AND
                 (NOT (AMTS-ON-NET-RECEIVABLES AND
                                AMTS-ON-RECEIVABLES)))
                 ADD 1 TO WK-RQTY
                 ADD HOLD-RENEW TO WK-RENEWED
                 IF LNAMT-AMTFIN = "L"
                    ADD LN-LNAMT TO WK-LNAMT
                 ELSE
                 IF LNAMT-AMTFIN = "C"
                    ADD LN-CURBAL TO WK-LNAMT
                 ELSE
                    ADD DISCLOSED-FINAMT TO WK-LNAMT.

      * RECEIVABLE

           IF LN-PLDATE = 0 OR PL = "O"
              IF ( AMTS-ON-RECEIVABLES )
                 IF LN-CURBAL > 0
                    IF NET-UNEARNED-FG = "Y"
                       PERFORM COMPUTE-EARNINGS
                       COMPUTE WK-NET = WK-NET +
                                        LN-CURBAL - EARN-TOT-UNEARN
                    ELSE
                       COMPUTE WK-NET = WK-NET + LN-CURBAL - REB-REBATE
                    END-IF
                    ADD LN-CURBAL TO WK-BALANCE
                    ADD 1 TO WK-QTY
                 ELSE
                    NEXT SENTENCE
              ELSE
      * NET RECEIVABLE
                 IF NET-UNEARNED-FG = "Y"
                    PERFORM COMPUTE-EARNINGS
                    IF (LN-CURBAL - EARN-TOT-UNEARN) > 0
                       COMPUTE WK-NET = WK-NET
                                      + LN-CURBAL - EARN-TOT-UNEARN
                       ADD LN-CURBAL TO WK-BALANCE
                       ADD 1 TO WK-QTY
                    END-IF
                 ELSE
                    IF (LN-CURBAL - REB-REBATE) > 0
                       COMPUTE WK-NET = WK-NET + LN-CURBAL - REB-REBATE
                       ADD LN-CURBAL TO WK-BALANCE
                       ADD 1 TO WK-QTY.

      * ADD TO THE RENEWAL-TOTALS

           IF ( SAVE-WK-BR NOT = 0 )
              IF ( RNFOR10-FG = "Y" )
                 PERFORM COMPUTE-RENEWAL-TOTALS.

      * ADD TO THE PAIDOFF-TOTALS

           IF ( SAVE-WK-BR NOT = 0 )
              IF ( PAIDOFF-FLAG = "Y" )
                 PERFORM COMPUTE-PAIDOFF-TOTALS.

           PERFORM REWRITE-WK-FILE.


       END-CREATE-WK.

      * CREATE GRAND TOTAL RECORDS

           IF SAVE-WK-BR NOT = 0
              MOVE 0 TO SAVE-WK-BR
              MOVE SAVE-WK-KEY TO WK-KEY
              GO TO SAVE-WORK-KEY.

       EXIT-CREATE-WK.
           EXIT.

      ******************************************************************
       FUTURE-RECEIVABLES-DUE SECTION.
      ******************************************************************
           MOVE LN-CURBAL TO HOLD-AMT.

           IF LN-LOANTYPE = "I"
              GO TO FUTURE-IB.

           MOVE 0 TO SUB.

       FUTURE-NEXT-PC.
           ADD 1 TO SUB.
           MOVE LN-1STPYDATE       TO CEPP1-DATE.
           MOVE TOTAL-DATE(1, SUB) TO CEPP2-DATE.
           PERFORM CONTRACTUAL-DELINQUENCY.

      * PAID AHEADS

           IF CDEL-ARREARAGE > 0
              MOVE 0 TO CDEL-ARREARAGE
           ELSE
              MULTIPLY -1 BY CDEL-ARREARAGE.

           IF SUB = 1
              ADD CDEL-ARREARAGE TO TOTAL-RCV(1, SUB) TOTAL-RCV(1, 7)
              SUBTRACT CDEL-ARREARAGE FROM HOLD-AMT
           ELSE
              COMPUTE TOTAL-RCV(1, SUB) = TOTAL-RCV(1, SUB) +
                      (CDEL-ARREARAGE - LAST-ARREAR)
              COMPUTE TOTAL-RCV(1, 7) = TOTAL-RCV(1, 7) +
                      (CDEL-ARREARAGE - LAST-ARREAR)
              COMPUTE HOLD-AMT = HOLD-AMT -
                      (CDEL-ARREARAGE - LAST-ARREAR).

           MOVE CDEL-ARREARAGE TO LAST-ARREAR.

           IF SUB < 5
              GO TO FUTURE-NEXT-PC.

      * REMAINING DUE

           ADD HOLD-AMT TO TOTAL-RCV(1, 6) TOTAL-RCV(1, 7).
           GO TO EXIT-FUTURE-RECEIVABLES.

       FUTURE-IB.
           MOVE LN-1STPYDATE TO CEPP1-DATE.
           MOVE YEAR-END-DATE     TO CEPP2-DATE.
           PERFORM CONTRACTUAL-ELAPSED-PAYPER.
           MOVE CEPP-PERIODS TO PER-REPORT.
           PERFORM MATURITY-DATE-CALCULATION.
           MOVE MDTE-DATE    TO CEPP2-DATE.
           MOVE LN-1STPYDATE TO CEPP1-DATE.
           PERFORM CONTRACTUAL-ELAPSED-PAYPER.
           COMPUTE HOLD-REMAIN = CEPP-PERIODS - PER-REPORT.

           IF HOLD-REMAIN NOT > 0
              MOVE 1 TO HOLD-REMAIN.

           COMPUTE PERIOD-AMT ROUNDED = LN-CURBAL / HOLD-REMAIN.
           MOVE 0 TO SUB.

       FUTURE-NEXT-IB.
           ADD 1 TO SUB.

           IF HOLD-REMAIN > 12
              MOVE 12 TO PERIOD-WK
           ELSE
              MOVE HOLD-REMAIN TO PERIOD-WK.

           COMPUTE TOTAL-RCV(1, SUB) = TOTAL-RCV(1, SUB) +
                                       (PERIOD-WK * PERIOD-AMT).

           COMPUTE TOTAL-RCV(1, 7) = TOTAL-RCV(1, 7) +
                                     (PERIOD-WK * PERIOD-AMT).

           COMPUTE HOLD-AMT = HOLD-AMT - (PERIOD-WK * PERIOD-AMT).
           SUBTRACT PERIOD-WK FROM HOLD-REMAIN.

           IF SUB < 5
              GO TO FUTURE-NEXT-IB.

      * REMAINING DUE

           ADD HOLD-AMT TO TOTAL-RCV(1, 6) TOTAL-RCV(1, 7).

       EXIT-FUTURE-RECEIVABLES.
           EXIT.

      **********************************************
       GET-LAST-PAYMENT SECTION.
      **********************************************
           MOVE "N" TO VALID-PAYOFF-DATE.

           PERFORM OPEN-LP1-FILE.

           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE 99999         TO LP-SEQNO
                                 QLP1-WEND-SEQNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.

           PERFORM START-LP1-FILE-NOT-GREATER.
       GET-NEXT-PAYMENT.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF NOT IO-GOOD OR
                LN-OWNBR NOT = LP-BRNO OR
                 LN-ACCTNO NOT = LP-ACCTNO
              GO TO EXIT-GET-LAST-PAYMENT.

      * IF LAST RECORD IS NOT BALANCE = 0, THEN NOT PAIDOUT !
           IF LP-CURBAL NOT = 0
              GO TO EXIT-GET-LAST-PAYMENT.
      * DONT ALLOW A RENEWAL
           IF LP-TRCD = "RN" OR "PB" OR "SC" OR "RB" OR "RO"
              GO TO EXIT-GET-LAST-PAYMENT.

           IF (LP-TRDATE NOT < BEG-LN-DATE) AND
                 (LP-TRDATE NOT > END-LN-DATE)
               MOVE "Y" TO VALID-PAYOFF-DATE.

       EXIT-GET-LAST-PAYMENT.
           PERFORM CLOSE-LP1-FILE.

      ******************************************************************
      *
      *    G E T - M O N T H S - T I L L - E X P I R A T I O N
      *
      *-----------------------------------------------------------------
      *    DESC:  GETTING THE NUMBER OF MONTHS LEFT TILL LOAN EXPIRATION
      *    IN  :  LN-REC
      *    OUT :  MONTHS-TILL-EXPIRATION
      *           MONTHS-TILL-EXP-STAT
      *-----------------------------------------------------------------
      *    FOR ALL OF THIS TO WORK FOR ACCOUNTS PAST MATURITY, THE
      *    FOLLOWING SHOULD BE DONE:
      *
      *      1.  IN THE FIRST BRACKET (AMT01-BUF), YOU MUST SET TO IT
      *          LESS THAN ONE (1.00) FOR MONTHS-TILL-EXPIRATION WILL BE
      *          ZEROES IF PAST MATURITY.
      *
      *      2.  IN THE SECOND BRACKET (AMT02-BUF), YOU MUST SET TO IT
      *          ONE (1.00) OR BEYOND.  THIS WILL THEN PLACE 'ACTIVE'
      *          ACCOUNTS WITHIN THE REMAINING BRACKETS.
      *
      *      *NOTE* PLACING 1 IN MONTHS-TILL-EXPIRATION IF IT IS NOT
      *             PAST MATURITY AND IT IS LESS THAN ONE MONTH BEFORE
      *             EXPIRATION BECAUSE IT NEEDS TO HAVE SOMETHING OTHER
      *             THAN ZEROES OR IT WILL BE CONSIDERED AS PAST
      *             MATURITY.
      *
      ******************************************************************
       GET-MONTHS-TILL-EXPIRATION SECTION.

           MOVE ZEROES TO MONTHS-TILL-EXPIRATION.
           MOVE SPACES TO MONTHS-TILL-EXP-STAT.

           MOVE SPACES          TO MDTE-FLAG.
           PERFORM MATURITY-DATE-CALCULATION.
           MOVE MDTE-DATE       TO SYS-DATE.
           MOVE HOLD-SYSDATE    TO NUM-DATE.
           MOVE LN-UNITPER-CD   TO DATER-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO DATER-UNITPER-FREQ.
           PERFORM TIMUPER.

           IF ( ELAPSED-DAYS < ZEROES )
              MOVE "X" TO MONTHS-TILL-EXP-STAT
              GO TO GET-MONTHS-TILL-EXP-EXIT.

           IF ( ELAPSED-MONTHS = ZEROES )
              MOVE 1              TO MONTHS-TILL-EXPIRATION
           ELSE
              MOVE ELAPSED-MONTHS TO MONTHS-TILL-EXPIRATION.

       GET-MONTHS-TILL-EXP-EXIT.
           EXIT.

      ******************************************************************
       BRANCH-TOTALS SECTION.
      ******************************************************************
           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO SUMMARY-FG.
           MOVE 99 TO LN-COUNT.

           IF REPORT-TYPE NOT = "C"
              PERFORM PRINT-TOTALS.

           MOVE 0 TO SUB.

       NEXT-ACCUM-GRAND.
           ADD 1 TO SUB.
           ADD TOTAL-RQTY(1, SUB) TO TOTAL-RQTY(2, SUB).
           ADD TOTAL-REN(1, SUB) TO TOTAL-REN(2, SUB).
           ADD TOTAL-LAMT(1, SUB) TO TOTAL-LAMT(2, SUB).
           ADD TOTAL-NET(1, SUB) TO TOTAL-NET(2, SUB).
           ADD TOTAL-QTY(1, SUB) TO TOTAL-QTY(2, SUB).
           ADD TOTAL-BAL(1, SUB) TO TOTAL-BAL(2, SUB).

           IF SUB < 8
              MOVE TOTAL-DATE(1, SUB) TO TOTAL-DATE(2, SUB)
              ADD TOTAL-RCV(1, SUB) TO TOTAL-RCV(2, SUB).

           IF SUB < 11
              GO TO NEXT-ACCUM-GRAND.

           MOVE 1 TO LEV.
           PERFORM CLEAR-TOTALS.
           PERFORM CLEAR-RENEWAL-TOTALS.
           PERFORM CLEAR-PAIDOFF-TOTALS.
           MOVE 99 TO LN-COUNT.
           MOVE "N" TO SUMMARY-FG.

      ******************************************************************
       GRAND-TOTALS SECTION.
      ******************************************************************
           MOVE "Y" TO SUMMARY-FG.
           MOVE 0 TO HOLD-BRANCH.

           IF GROUP-FLAG = "Y"
              MOVE ENT-GROUP TO H-BRNOX
           ELSE
              MOVE 0 TO H-BRNO.

           MOVE "ALL" TO H-BRNAME.
           PERFORM PRINT-TOTALS.

      ******************************************************************
       PRINT-TOTALS SECTION.
      ******************************************************************
           MOVE 0 TO INS-STATUS.

           IF HOLD-BRANCH = 0
              MOVE 2 TO LEV
              MOVE "GRAND TOTALS:" TO DETAIL-LINE
           ELSE
              MOVE 1 TO LEV
              MOVE "BRANCH TOTALS:" TO DETAIL-LINE.

           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * TOTALS BY BRACKET

           IF ( AMTS-ON-RECEIVABLES )
              MOVE "RECEIVABLE:" TO H-TAG
           ELSE
           IF ( AMTS-ON-NET-RECEIVABLES )
              MOVE "NET RECEIVABLE:" TO H-TAG
           ELSE
           IF ( AMTS-ON-TERM )
              MOVE "TERMS:" TO H-TAG
           ELSE
           IF ( AMTS-ON-APR )
              MOVE "AN. PERCNT RATE:" TO H-TAG
           ELSE
           IF ( AMTS-ON-ACCRATE )
              MOVE "ACCEL. RATE   :" TO H-TAG
           ELSE
           IF ( AMTS-ON-EFFRATE )
              MOVE "EFFECTIVE RATE:" TO H-TAG
           ELSE
           IF ( AMTS-ON-LOAN-EXPIRATION )
              MOVE "MONTH TILL EXP.:" TO H-TAG.

           PERFORM COLUMN-HEADER.
           MOVE 2 TO LN-FEED.
           MOVE 0 TO SUB.

       PRINT-NEXT-BRACKET.
           ADD 1 TO SUB.
           MOVE BRACKET-ALP(SUB) TO D-BRACKETX.
           MOVE TOTAL-RQTY(LEV, SUB) TO QTY1-WK.
           MOVE TOTAL-REN(LEV, SUB) TO REN-WK.
           MOVE TOTAL-LAMT(LEV, SUB) TO AMT-WK.
           MOVE TOTAL-NET(LEV, SUB) TO NET-WK.
           MOVE TOTAL-QTY(LEV, SUB) TO QTY2-WK.
           MOVE TOTAL-BAL(LEV, SUB) TO BAL-WK.

           PERFORM MOVE-FIGURES.
           PERFORM WRITE-DETAIL-LINE.

           IF AMOUNT(SUB) NOT = 999999.99
              GO TO PRINT-NEXT-BRACKET.

           MOVE BRACKET-ALP(11) TO D-BRACKETX.
           MOVE TOTAL-RQTY(LEV, 11) TO QTY1-WK.
           MOVE TOTAL-REN(LEV, 11) TO REN-WK.
           MOVE TOTAL-LAMT(LEV, 11) TO AMT-WK.
           MOVE TOTAL-NET(LEV, 11) TO NET-WK.
           MOVE TOTAL-QTY(LEV, 11) TO QTY2-WK.
           MOVE TOTAL-BAL(LEV, 11) TO BAL-WK.

           PERFORM MOVE-FIGURES.
           PERFORM WRITE-DETAIL-LINE.

           IF REPORT-TYPE = "B"
              GO TO EXIT-PRINT-TOTALS.

      * TOTALS BY TYPE

           MOVE "Y" TO FIRST-TYPE.
           MOVE HOLD-BRANCH TO WK-BR.
           MOVE 1 TO WK-TYPE.
           MOVE " " TO WK-CODE.
           PERFORM START-WK-FILE.

       PRINT-NEXT-TYPE.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-BAD
              PERFORM TYPE-TOTALS
              GO TO PRINT-DUE.

           IF WK-BR NOT = HOLD-BRANCH
              PERFORM TYPE-TOTALS
              GO TO PRINT-DUE.

           IF FIRST-TYPE = "Y"
              MOVE "N" TO FIRST-TYPE
              PERFORM TYPE-COLUMN-HEADERS.

           IF HOLD-TYPE NOT = WK-TYPE
              PERFORM TYPE-TOTALS
              PERFORM TYPE-COLUMN-HEADERS.

      * THIS IS FOR RENEWALS LESS THAN 10 PERCENT.

           MOVE "N" TO RNFOR10-FG.

           IF ( WK-TYPE = 4 )
              MOVE "Y" TO RNFOR10-FG.

      * THIS IS FOR PAIDOFF ACCOUNTS OTHER THAN RENEWALS.

           MOVE "N" TO PAIDOFF-FLAG.

           IF ( WK-TYPE = 5 )
              MOVE "Y" TO PAIDOFF-FLAG.

           MOVE WK-CODE TO D-CD-CODE CD-CODE.

           IF WK-TYPE < 6
              MOVE TYPE-KEY(WK-TYPE) TO CD-TYPE
              PERFORM READ-CD1-FILE
              IF IO-BAD
                 MOVE "* INVALID *" TO D-CD-DESC
              ELSE
                 MOVE CD-DESC TO D-CD-DESC
           ELSE
              MOVE SPACE TO D-CD-CODE
              IF WK-TYPE = 6
                 MOVE "CREDIT LIFE" TO D-CD-DESC
              ELSE
              IF WK-TYPE = 7
                 MOVE "A & H" TO D-CD-DESC
              ELSE
              IF WK-TYPE = 8
                 MOVE "PROPERTY INS." TO D-CD-DESC
              ELSE
              IF WK-TYPE = 9
                 MOVE "OTHER 1" TO D-CD-DESC
              ELSE
              IF WK-TYPE = 10
                 MOVE "OTHER 2" TO D-CD-DESC
              ELSE
              IF WK-TYPE = 11
                 MOVE "OTHER 3" TO D-CD-DESC
              ELSE
              IF WK-TYPE = 12
                 MOVE "NO INSURANCE" TO D-CD-DESC.

           MOVE WK-RQTY TO QTY1-WK.
           MOVE WK-RENEWED TO REN-WK.
           MOVE WK-LNAMT TO AMT-WK.
           MOVE WK-NET TO NET-WK.
           MOVE WK-QTY TO QTY2-WK.
           MOVE WK-BALANCE TO BAL-WK.
           PERFORM MOVE-FIGURES.
           PERFORM WRITE-DETAIL-LINE.

           GO TO PRINT-NEXT-TYPE.

       PRINT-DUE.
           MOVE 3 TO LN-FEED.
           MOVE HEAD8 TO PR-REC.
           PERFORM WRITE-PR-REC.

           ADD 3 TO LN-COUNT.
           MOVE SPACES TO DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           MOVE 0 TO SUB.

       PRINT-NEXT-DUE.
           ADD 1 TO SUB.
           MOVE TOTAL-DATE(LEV, SUB) TO NUM-DATE.
           MOVE NUM-CCYY             TO D-DATE.
           MOVE "DUE IN "            TO D-TAG2.
           MOVE TOTAL-RCV(LEV, SUB)  TO D-RECEIV.
           PERFORM WRITE-DETAIL-LINE.

           IF SUB < 5
              GO TO PRINT-NEXT-DUE.

           MOVE "REMAIN DUE " TO D-RECTAG.
           MOVE TOTAL-RCV(LEV, 6) TO D-RECEIV.
           PERFORM WRITE-DETAIL-LINE.

           MOVE TOTAL-RCV(LEV, 7) TO D-RECEIV.
           MOVE BRACKET-ALP(11) TO D-BRACKETX.
           PERFORM WRITE-DETAIL-LINE.

       EXIT-PRINT-TOTALS.
           EXIT.

      ******************************************************************
       MOVE-BORROWER SECTION.
      ******************************************************************
           IF LN-SSNO(1) = 0
              GO TO EXIT-MOVE-BORROWER.

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-BAD
              MOVE "* *  NOT FOUND  * *" TO D-NAME
              GO TO EXIT-MOVE-BORROWER.

           MOVE SPACES TO D-NAME.
           STRING BW-LNAME,", ",BW-FNAME DELIMITED BY "  "
                INTO D-NAME.

       EXIT-MOVE-BORROWER.
           EXIT.

      ******************************************************************
       COMPUTE-RENEWAL-TOTALS SECTION.
      ******************************************************************

           MOVE 1 TO SUB-RENEWAL.

       NEXT-RENEWAL-ADD.

           IF ( VALID-DATE = "Y" )
              IF (LN-BR-TFR-FG NOT = "Y") OR
                 (LN-BR-TFR-FG = "Y" AND
                 (NOT (AMTS-ON-NET-RECEIVABLES AND
                                  AMTS-ON-RECEIVABLES)))
                 ADD 1 TO RENEWAL-RQTY(SUB-RENEWAL)
                 ADD HOLD-RENEW TO RENEWAL-REN(SUB-RENEWAL)
                 IF LNAMT-AMTFIN = "L"
                    ADD LN-LNAMT TO RENEWAL-LAMT(SUB-RENEWAL)
                 ELSE
                 IF LNAMT-AMTFIN = "C"
                    ADD LN-CURBAL TO RENEWAL-LAMT(SUB-RENEWAL)
                 ELSE
                    ADD DISCLOSED-FINAMT TO RENEWAL-LAMT(SUB-RENEWAL).

      * RECEIVABLE

           IF LN-PLDATE = 0 OR PL = "O"
              IF ( AMTS-ON-RECEIVABLES )
                 IF LN-CURBAL > 0
                    IF NET-UNEARNED-FG = "Y"
                       PERFORM COMPUTE-EARNINGS
                       COMPUTE RENEWAL-NET(SUB-RENEWAL) =
                          RENEWAL-NET(SUB-RENEWAL) + LN-CURBAL -
                          EARN-TOT-UNEARN
                    ELSE
                       COMPUTE RENEWAL-NET(SUB-RENEWAL) =
                          RENEWAL-NET(SUB-RENEWAL) + LN-CURBAL -
                          REB-REBATE
                    END-IF
                    ADD LN-CURBAL TO RENEWAL-BAL(SUB-RENEWAL)
                    ADD 1 TO RENEWAL-QTY(SUB-RENEWAL)
                 ELSE
                    NEXT SENTENCE
              ELSE
      * NET RECEIVABLE
                 IF NET-UNEARNED-FG = "Y"
                    PERFORM COMPUTE-EARNINGS
                    IF (LN-CURBAL - EARN-TOT-UNEARN) > 0
                       COMPUTE RENEWAL-NET(SUB-RENEWAL) =
                            RENEWAL-NET(SUB-RENEWAL) + LN-CURBAL -
                            EARN-TOT-UNEARN
                       ADD LN-CURBAL TO RENEWAL-BAL(SUB-RENEWAL)
                       ADD 1 TO RENEWAL-QTY(SUB-RENEWAL)
                    END-IF
                 ELSE
                    IF (LN-CURBAL - REB-REBATE) > 0
                       COMPUTE RENEWAL-NET(SUB-RENEWAL) =
                               RENEWAL-NET(SUB-RENEWAL) + LN-CURBAL -
                               REB-REBATE
                       ADD LN-CURBAL TO RENEWAL-BAL(SUB-RENEWAL)
                       ADD 1 TO RENEWAL-QTY(SUB-RENEWAL).

           IF ( SUB-RENEWAL < 2 )
              ADD 1 TO SUB-RENEWAL
              GO TO NEXT-RENEWAL-ADD.

       EXIT-COMPUTE-RENEWAL-TOTALS.
           EXIT.

      ******************************************************************
       CLEAR-RENEWAL-TOTALS SECTION.
      ******************************************************************

           MOVE 1 TO SUB-RENEWAL.

       NEXT-RENEWAL-LEVEL.
           MOVE ZEROES TO RENEWAL-RQTY(SUB-RENEWAL)
                          RENEWAL-REN(SUB-RENEWAL)
                          RENEWAL-LAMT(SUB-RENEWAL)
                          RENEWAL-NET(SUB-RENEWAL)
                          RENEWAL-QTY(SUB-RENEWAL)
                          RENEWAL-BAL(SUB-RENEWAL).

           IF ( SUB-RENEWAL < LEV )
              ADD 1 TO SUB-RENEWAL
              GO TO NEXT-RENEWAL-LEVEL.

       EXIT-CLEAR-RENEWAL-TOTALS.
           EXIT.

      ******************************************************************
       COMPUTE-PAIDOFF-TOTALS SECTION.
      ******************************************************************

           MOVE 1 TO SUB-PAIDOFF.

       NEXT-PAIDOFF-ADD.

           ADD 1 TO PAIDOFF-RQTY(SUB-PAIDOFF).
           ADD HOLD-RENEW TO PAIDOFF-REN(SUB-PAIDOFF).
           IF LNAMT-AMTFIN = "L"
              ADD LN-LNAMT TO PAIDOFF-LAMT(SUB-PAIDOFF)
           ELSE
           IF LNAMT-AMTFIN = "C"
              ADD LN-CURBAL TO PAIDOFF-LAMT(SUB-PAIDOFF)
           ELSE
              ADD DISCLOSED-FINAMT TO PAIDOFF-LAMT(SUB-PAIDOFF).

      * RECEIVABLE
           IF LN-PLDATE = 0 OR PL = "O"
              IF ( AMTS-ON-RECEIVABLES )
                 IF LN-CURBAL > 0
                    IF NET-UNEARNED-FG = "Y"
                       PERFORM COMPUTE-EARNINGS
                       COMPUTE PAIDOFF-NET(SUB-PAIDOFF) =
                         PAIDOFF-NET(SUB-PAIDOFF) + LN-CURBAL -
                                  EARN-TOT-UNEARN
                    ELSE
                       COMPUTE PAIDOFF-NET(SUB-PAIDOFF) =
                               PAIDOFF-NET(SUB-PAIDOFF) + LN-CURBAL -
                                  REB-REBATE
                    END-IF
                    ADD LN-CURBAL TO PAIDOFF-BAL(SUB-PAIDOFF)
                    ADD 1 TO PAIDOFF-QTY(SUB-PAIDOFF)
                 ELSE
                    NEXT SENTENCE
              ELSE
      * NET RECEIVABLE
                 IF NET-UNEARNED-FG = "Y"
                    PERFORM COMPUTE-EARNINGS
                    IF (LN-CURBAL - EARN-TOT-UNEARN) > 0
                       COMPUTE PAIDOFF-NET(SUB-PAIDOFF) =
                           PAIDOFF-NET(SUB-PAIDOFF) + LN-CURBAL -
                           EARN-TOT-UNEARN
                     ADD LN-CURBAL TO PAIDOFF-BAL(SUB-PAIDOFF)
                     ADD 1 TO PAIDOFF-QTY(SUB-PAIDOFF)
                 ELSE
                    IF (LN-CURBAL - REB-REBATE) > 0
                     COMPUTE PAIDOFF-NET(SUB-PAIDOFF) =
                             PAIDOFF-NET(SUB-PAIDOFF) + LN-CURBAL -
                             REB-REBATE
                     ADD LN-CURBAL TO PAIDOFF-BAL(SUB-PAIDOFF)
                     ADD 1 TO PAIDOFF-QTY(SUB-PAIDOFF).

           IF ( SUB-PAIDOFF < 2 )
              ADD 1 TO SUB-PAIDOFF
              GO TO NEXT-PAIDOFF-ADD.

       EXIT-COMPUTE-PAIDOFF-TOTALS.
           EXIT.

      ******************************************************************
       CLEAR-PAIDOFF-TOTALS SECTION.
      ******************************************************************

           MOVE 1 TO SUB-PAIDOFF.

       NEXT-PAIDOFF-LEVEL.
           MOVE ZEROES TO PAIDOFF-RQTY(SUB-PAIDOFF)
                          PAIDOFF-REN(SUB-PAIDOFF)
                          PAIDOFF-LAMT(SUB-PAIDOFF)
                          PAIDOFF-NET(SUB-PAIDOFF)
                          PAIDOFF-QTY(SUB-PAIDOFF)
                          PAIDOFF-BAL(SUB-PAIDOFF).

           IF ( SUB-PAIDOFF < LEV )
              ADD 1 TO SUB-PAIDOFF
              GO TO NEXT-PAIDOFF-LEVEL.

       EXIT-CLEAR-PAIDOFF-TOTALS.
           EXIT.

      ******************************************************************
       CLEAR-TOTALS SECTION.
      ******************************************************************
           MOVE 0 TO SUB SUB1.

       NEXT-LEVEL-0.
           ADD 1 TO SUB.

       NEXT-BRACKET-0.
           ADD 1 TO SUB1.

           MOVE 0 TO TOTAL-RQTY(SUB, SUB1) TOTAL-REN(SUB, SUB1)
                     TOTAL-LAMT(SUB, SUB1)  TOTAL-QTY(SUB, SUB1)
                     TOTAL-NET(SUB, SUB1) TOTAL-BAL(SUB, SUB1).

           IF SUB1 < 8
              MOVE 0 TO TOTAL-RCV(SUB, SUB1).

           IF SUB1 < 11
              GO TO NEXT-BRACKET-0.

           IF SUB  < LEV
              MOVE 0 TO SUB1
              GO TO NEXT-LEVEL-0.

       EXIT-CLEAR-TOTALS.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************

           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.

           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      
           MOVE HEAD3 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      
           MOVE HEAD4 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      
           IF ( WS-INVALID-COLL-ROUTINE = "Y" )
              PERFORM HEAD-INVALID-COLLCD
              MOVE 10 TO LN-COUNT
              GO TO END-HEAD-PAGE.
      
           IF SUMMARY-FG = "N"
              MOVE HEAD5 TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC
              MOVE 6 TO LN-COUNT
           ELSE
              MOVE 9 TO LN-COUNT.

           GO TO END-HEAD-PAGE.

       HEAD-INVALID-COLLCD.
           MOVE HEAD9 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      
           MOVE HEAD10 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 2 TO LN-FEED.

      ******************************************************************
       RANGE-LINES SECTION.
      ******************************************************************
           PERFORM HEAD-PAGE.
           MOVE 3 TO LN-FEED.

           IF GROUP-FLAG = "Y"
              MOVE "GROUP     " TO RANGE-LINE
              MOVE ENT-GROUP TO RANGE-SEL
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEG BRANCH" TO RANGE-LINE
              MOVE BEG-BR TO RANGE-SEL
              PERFORM WRITE-DETAIL-LINE
              MOVE "END BRANCH" TO RANGE-LINE
              MOVE END-BR TO RANGE-SEL
              PERFORM WRITE-DETAIL-LINE.
      
           MOVE "BEGIN CLASS" TO RANGE-LINE.
           MOVE BEG-CLASS     TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "END   CLASS" TO RANGE-LINE.
           MOVE END-CLASS     TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "BEGIN LOAN DATE" TO RANGE-LINE.
           MOVE BEG-LN-DATE TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "END LOAN DATE" TO RANGE-LINE.
           MOVE END-LN-DATE TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "YEAR END " TO RANGE-LINE.
           MOVE YEAR-END-DATE TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "ZERO BALANCE ACCOUNTS" TO RANGE-LINE.
           MOVE ZBAL TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "P & L ACCOUNTS  ?" TO RANGE-LINE.
           MOVE PL TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "SERVICE ACCOUNTS?" TO RANGE-LINE.
           MOVE SERVA TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "SALES FINANCE   ?" TO RANGE-LINE.
           MOVE SALEFIN TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "BULK ACCOUNTS   ?" TO RANGE-LINE.
           MOVE BULK TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "REPO ACCOUNTS   ?" TO RANGE-LINE.
           MOVE REPO TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "SECURITY (S/U/R)?" TO RANGE-LINE.
           MOVE SECURITY-TYPE TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "LOAN AMT, AMT FIN OR CURBAL" TO RANGE-LINE.
           MOVE LNAMT-AMTFIN TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "BRACKETS ON" TO RANGE-LINE.
           MOVE AMTS-ON TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      
           PERFORM VARYING RANGE-SUB FROM 1 BY 1 UNTIL RANGE-SUB > 10
              MOVE "BRACKET &&" TO RANGE-LINE
              INSPECT RANGE-LINE REPLACING FIRST "&&" BY RANGE-SUB
              IF (AMTS-ON-APR OR AMTS-ON-ACCRATE OR AMTS-ON-EFFRATE)
                 MOVE AMOUNT-RATE(RANGE-SUB) TO RANGE-RATE
              ELSE
                 MOVE AMOUNT(RANGE-SUB) TO RANGE-AMT
              END-IF
              PERFORM WRITE-DETAIL-LINE
           END-PERFORM.

      
           MOVE "REPORT TYPE ?" TO RANGE-LINE.
           MOVE REPORT-TYPE TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "NET UNEARNED"  TO RANGE-LINE.
           MOVE NET-UNEARNED-FG TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.
      
      ******************************************************************
       TYPE-COLUMN-HEADERS SECTION.
      ******************************************************************
           IF INS-STATUS = 2
              GO TO EXIT-TYPE-COLUMN-HEADERS.

           MOVE WK-TYPE TO HOLD-TYPE.

           IF WK-TYPE < 6
              MOVE TYPE-ALP(WK-TYPE) TO H-TAG
           ELSE
              MOVE 2 TO INS-STATUS
              MOVE "INSURANCE      :" TO H-TAG.

           PERFORM COLUMN-HEADER.
           MOVE 2 TO LN-FEED.

       EXIT-TYPE-COLUMN-HEADERS.
           EXIT.

      ******************************************************************
       TYPE-TOTALS SECTION.
      ******************************************************************
           IF INS-STATUS = 2
              GO TO EXIT-TYPE-TOTALS.

      * FOR PAIDOFF ACOUNTS OTHER THAN RENEWALS.
      
           IF ( PAIDOFF-FLAG = "Y" )
              GO TO PRINT-PAIDOFF-TOTALS.

      * FOR RENEWAL LESS THAN 10 PERCENT.
      
           IF ( RNFOR10-FG = "Y" )
              GO TO PRINT-RENEWAL-TOTALS.

      * STANDARD TOTALS 

           MOVE BRACKET-ALP(11) TO D-BRACKETX.
           MOVE TOTAL-RQTY(LEV, 11) TO QTY1-WK.
           MOVE TOTAL-REN(LEV, 11) TO REN-WK.
           MOVE TOTAL-LAMT(LEV, 11) TO AMT-WK.
           MOVE TOTAL-NET(LEV, 11) TO NET-WK.
           MOVE TOTAL-QTY(LEV, 11) TO QTY2-WK.
           MOVE TOTAL-BAL(LEV, 11) TO BAL-WK.
           PERFORM MOVE-FIGURES.
           PERFORM WRITE-DETAIL-LINE.

           GO TO EXIT-TYPE-TOTALS.

       PRINT-RENEWAL-TOTALS.
           MOVE BRACKET-ALP(11)   TO D-BRACKETX.
           MOVE RENEWAL-RQTY(LEV) TO QTY1-WK.
           MOVE RENEWAL-REN(LEV)  TO REN-WK.
           MOVE RENEWAL-LAMT(LEV) TO AMT-WK.
           MOVE RENEWAL-NET(LEV)  TO NET-WK.
           MOVE RENEWAL-QTY(LEV)  TO QTY2-WK.
           MOVE RENEWAL-BAL(LEV)  TO BAL-WK.
           PERFORM MOVE-FIGURES.
           PERFORM WRITE-DETAIL-LINE.
           GO TO EXIT-TYPE-TOTALS.

       PRINT-PAIDOFF-TOTALS.
           MOVE BRACKET-ALP(11)   TO D-BRACKETX.
           MOVE PAIDOFF-RQTY(LEV) TO QTY1-WK.
           MOVE PAIDOFF-REN(LEV)  TO REN-WK.
           MOVE PAIDOFF-LAMT(LEV) TO AMT-WK.
           MOVE PAIDOFF-NET(LEV)  TO NET-WK.
           MOVE PAIDOFF-QTY(LEV)  TO QTY2-WK.
           MOVE PAIDOFF-BAL(LEV)  TO BAL-WK.
           PERFORM MOVE-FIGURES.
           PERFORM WRITE-DETAIL-LINE.
           GO TO EXIT-TYPE-TOTALS.

       EXIT-TYPE-TOTALS.
           EXIT.

      ***********************************************
       MOVE-FIGURES SECTION.
      ***********************************************

      * FOR RENEWAL LESS THAN 10 PERCENT.
      
           IF ( RNFOR10-FG = "Y" )
              PERFORM RENEWAL-MOVE
           ELSE
           IF ( PAIDOFF-FLAG = "Y" )
              PERFORM PAIDOFF-MOVE
           ELSE
              PERFORM STANDARD-MOVE.

           GO TO EXIT-MOVE-FIGURES.

       RENEWAL-MOVE.
           MOVE QTY1-WK TO D-RQTY.

           IF RENEWAL-RQTY(LEV) NOT = 0
              COMPUTE PCT-WK ROUNDED = (QTY1-WK * 100)
                                     / RENEWAL-RQTY(LEV)
              MOVE PCT-WK TO D-RPCT.

           MOVE REN-WK TO D-RENEW.
           MOVE AMT-WK TO D-LOAN.

           IF RENEWAL-LAMT(LEV) NOT = 0
              COMPUTE PCT-WK ROUNDED = (AMT-WK * 100)
                                     / RENEWAL-LAMT(LEV)
              MOVE PCT-WK TO D-LPCT.

           MOVE NET-WK TO D-NET.

           IF RENEWAL-NET(LEV) NOT = 0
              COMPUTE PCT-WK ROUNDED = (NET-WK * 100)
                                     / RENEWAL-NET(LEV)
              MOVE PCT-WK TO D-NPCT.

           MOVE QTY2-WK TO D-QTY.

           IF RENEWAL-QTY(LEV) NOT = 0
              COMPUTE PCT-WK ROUNDED = (QTY2-WK * 100)
                                     / RENEWAL-QTY(LEV)
              MOVE PCT-WK TO D-PCT.

           MOVE BAL-WK TO D-RECEV.

           IF RENEWAL-BAL(LEV) NOT = 0
              COMPUTE PCT-WK ROUNDED = (BAL-WK * 100)
                                     / RENEWAL-BAL(LEV)
              MOVE PCT-WK TO D-RECPCT.

       PAIDOFF-MOVE.
           MOVE QTY1-WK TO D-RQTY.

           IF PAIDOFF-RQTY(LEV) NOT = 0
              COMPUTE PCT-WK ROUNDED = (QTY1-WK * 100)
                                     / PAIDOFF-RQTY(LEV)
              MOVE PCT-WK TO D-RPCT.

           MOVE REN-WK TO D-RENEW.
           MOVE AMT-WK TO D-LOAN.

           IF PAIDOFF-LAMT(LEV) NOT = 0
              COMPUTE PCT-WK ROUNDED = (AMT-WK * 100)
                                     / PAIDOFF-LAMT(LEV)
              MOVE PCT-WK TO D-LPCT.

           MOVE NET-WK TO D-NET.

           IF PAIDOFF-NET(LEV) NOT = 0
              COMPUTE PCT-WK ROUNDED = (NET-WK * 100)
                                     / PAIDOFF-NET(LEV)
              MOVE PCT-WK TO D-NPCT.

           MOVE QTY2-WK TO D-QTY.

           IF PAIDOFF-QTY(LEV) NOT = 0
              COMPUTE PCT-WK ROUNDED = (QTY2-WK * 100)
                                     / PAIDOFF-QTY(LEV)
              MOVE PCT-WK TO D-PCT.

           MOVE BAL-WK TO D-RECEV.

           IF PAIDOFF-BAL(LEV) NOT = 0
              COMPUTE PCT-WK ROUNDED = (BAL-WK * 100)
                                     / PAIDOFF-BAL(LEV)
              MOVE PCT-WK TO D-RECPCT.

       STANDARD-MOVE.
           MOVE QTY1-WK TO D-RQTY.

           IF TOTAL-RQTY(LEV, 11) NOT = 0
              COMPUTE PCT-WK ROUNDED = (QTY1-WK * 100)
                                     / TOTAL-RQTY(LEV, 11)
              IF HOLD-TYPE < 6
                 MOVE PCT-WK TO D-RPCT.

           MOVE REN-WK TO D-RENEW.
           MOVE AMT-WK TO D-LOAN.

           IF TOTAL-LAMT(LEV, 11) NOT = 0
              COMPUTE PCT-WK ROUNDED = (AMT-WK * 100)
                                     / TOTAL-LAMT(LEV, 11)
              IF HOLD-TYPE < 6
                 MOVE PCT-WK TO D-LPCT.

           MOVE NET-WK TO D-NET.

           IF TOTAL-NET(LEV, 11) NOT = 0
              COMPUTE PCT-WK ROUNDED = (NET-WK * 100)
                                     / TOTAL-NET(LEV, 11)
              IF HOLD-TYPE < 6
                 MOVE PCT-WK TO D-NPCT.

           MOVE QTY2-WK TO D-QTY.

           IF TOTAL-QTY(LEV, 11) NOT = 0
              COMPUTE PCT-WK ROUNDED = (QTY2-WK * 100)
                                     / TOTAL-QTY(LEV, 11)
              IF HOLD-TYPE < 6
                 MOVE PCT-WK TO D-PCT.

           MOVE BAL-WK TO D-RECEV.

           IF TOTAL-BAL(LEV, 11) NOT = 0
              COMPUTE PCT-WK ROUNDED = (BAL-WK * 100)
                                     / TOTAL-BAL(LEV, 11)
              IF HOLD-TYPE < 6
                 MOVE PCT-WK TO D-RECPCT.

       EXIT-MOVE-FIGURES.
           EXIT.

      ******************************************************************
       INVALID-COLLATERAL-ACCTS SECTION.
      ******************************************************************
           MOVE "Y" TO WS-INVALID-COLL-ROUTINE.
           MOVE 99  TO LN-COUNT.
           PERFORM CLOSE-WK2-FILE.
           MOVE ZEROES TO WK2-KEY.
           PERFORM OPEN-WK2-FILE.

       READ-NEXT-WK2.
           PERFORM READ-WK2-FILE-NEXT.
           IF ( IO-BAD )
              MOVE "N" TO WS-INVALID-COLL-ROUTINE
              GO TO EXIT-INVALID-COLLATERAL-ACCTS.

           MOVE WK2-OWNBR  TO D5-BRANCH.
           MOVE WK2-ACCTNO TO D5-ACCOUNTS.
           MOVE WK2-CLASS  TO D5-CLASS.
           MOVE WK2-CODE   TO D5-CODE.
           MOVE 1          TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           GO TO READ-NEXT-WK2.

       EXIT-INVALID-COLLATERAL-ACCTS.
           EXIT.

      ******************************************************************
       SEND-PROGRESS-INDICATOR SECTION.
      ******************************************************************

           MOVE BR-NO                  TO PROGID-BRANCH.
           MOVE "LEGEND:B."            TO PROGID-LINE-LIST.
           MOVE PRU-LP                 TO PROGID-PRU-LP.
           MOVE EXT-ROUTE-BUF              TO PROGID-ROUTE-BUF.
           MOVE 250                    TO PROGID-SLEEP-COUNTER-MAX.

           PERFORM GET-TIME.
           MOVE TIME-EDIT              TO PROGID-TIME.

           MOVE "B"                    TO PROGID-TYPE.
           MOVE PROGID-TEXT-PROCESSING TO PROGID-TEXT.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PROGID-DISPLAY.

       SEND-PROGID-INDICATOR-EXIT.
           EXIT.

      ************************************
       FORM-RESET SECTION.
      ************************************
           MOVE EXT-CONAME-CONAME TO BNKRP1-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BNKRP1-SYSDATE.
           DISPLAY BNKRP1-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE BNKRP1-FORM-FIELDS TO WR-BUF.
           MOVE BNKRP1-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.


      ************************************
       SETUP-LINK-INFO SECTION.
      ************************************
           MOVE BNKRP1-HELP--FILE TO HELP-LINK-FILE.
           MOVE BNKRP1-HELP--DIR TO HELP-LINK-DIR.

      ************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/BNKRP1_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARA SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/GETENV.CPY".
       INVALID-COLLATERAL-CODE.
           MOVE LN-OWNBR  TO WK2-OWNBR.
           MOVE LN-CLASS  TO WK2-CLASS.
           MOVE LN-ACCTNO TO WK2-ACCTNO.
           MOVE LN-COLLCD TO WK2-CODE.
           PERFORM WRITE-WK2-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.
       ACCESS-COLLATERAL-CODE.
           MOVE "CO"      TO CD-TYPE.
           MOVE LN-COLLCD TO CD-CODE.
           PERFORM READ-CD1-FILE.
       GET-SP.
           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE
              IF NOT IO-GOOD
                 MOVE "INVALID STATE PARAMETER RECORD" TO MESS
                 PERFORM SEND-MESS
                 MOVE "X" TO EXT-EOBUF-ERRCD
                 GO TO IO-ERROR.
       COLUMN-HEADER.
           MOVE HEAD6 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           IF ( AMTS-ON-NET-RECEIVABLES )
              MOVE "----------" TO H-CONNECT1
           ELSE
           IF ( AMTS-ON-RECEIVABLES )
              MOVE "---------" TO H-CONNECT2.

           MOVE HEAD7 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           ADD 3 TO LN-COUNT.
           MOVE SPACES TO DETAIL-LINE.


      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/PROGID.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPCDEL.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBGB/REBATE.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/SETCTBL.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPEARN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/RBACT.CPY".
       COPY "LIBLP/RNFOR10LN.CPY".

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBGPGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".

       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGPRN.CPY".
       COPY "LIBLP/LPLN3RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBGB/GBWK2IN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************

           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO REPORT FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

       EXIT-PROG.
           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
           MOVE WK2-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

           IF SV-BEG-BR NOT = 9999
              IF EXT-EOBUF-GROUP-FG = "Y"
                 MOVE SV-BEG-BR TO BEG-BR
                 MOVE SV-END-BR TO END-BR
                 MOVE SV-GROUP-FLAG TO GROUP-FLAG
                 MOVE SV-ENT-GROUP TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BR TO BEG-BR
                 MOVE SV-END-BR TO END-BR.

           IF EXT-ROUTE-BUF = "00"
              MOVE "LP/LPEYMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN 
              DESTROY BNKRP1-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
