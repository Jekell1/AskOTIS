       IDENTIFICATION DIVISION.
       PROGRAM-ID.   EMTBRG.
       AUTHOR.       MJD.
       DATE-WRITTEN. 112295.
      ******************************************************************
      *
      *    DESCRIPTION:  BRANCH (BULK) ACCOUNT TRANSFER
      *                     TR1-CODE = "B"
      *
      *                  FOR LOANS SOLD (TB-REF-FRTO = "T").
      *                  FOR LOANS BOUGHT (TB-REF-FRTO = "F").
      *
      * REV:
      *  KEC 113095 FIXED LOCAL-SPOOL FOR IF A BRANCH HAS NO DATA.
      *  JFF 121395 CHANGED "IF LOCAL-PRINT-FG = N" TO
      *             "IF LOCAL-PRINT-FG NOT = Y" IN INITIALIZATION.
      *  JFF 030596 ADDED "SUMMARY ONLY" TO FORM & PARAMETER SECTION.
      *  JFF 031196 ADDED PRINT-RANGE-LINES SECTION.
      *  MJD 042696 ADDED GP-FILE, GP-DLR-DISP-TYPE.
      *  JFF 060696 ADDED GROUP TOTALS LOGIC.
      *  MJD 081396 ADDED DISPLAY OF TO/FROM BRANCH AND POST DATE.
      *  MJD 102296 ADDED ENTRY OF BRANCH THAT ACCOUNT WAS
      *             SOLD TO/PURCHASED FROM. ENTRY ALLOWS SINGLE BRANCH
      *             OR F3-ALL.
      *  MJD 041697 FIXED BUG. PR-REC NOT OPENED IF ENTERED WITH GROUP RANGE.
      *  GLF 090597 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *  GLF 022098 DELETED BR-SECURITY-OK AND ADDED LIBGB/BRSECUREW.
      *  JTG 101398 REMOVED SET OF LCSCAN-SALEFIN & LCSCAN-SERVACCT
      *  GLF 043099 WAS MOVING SPACES TO DETAIL-LINE ON SUMMARY ONLY IN
      *             WRONG SPOT CAUSING LINES TO BE MISSING, IS NOW FIXED
      *  MJD 990803 ADDED GP-INCL-OTH-IN-EARNED FLAG. IF "Y" THEN INCLUDE
      *             EARNINGS BUCKET 6 (OTHER) IN ACCUMULATED TOTALS
      *             OTHERWISE EXCLUDE BUCKET 6.
      *  GLF 081999 ADD ARCHIVE PATH INFO. FOR TR FILES NOT FOUND IN DATA
      *             SET FOR SELECTED MONTH AND YEAR
      *  BLV 990826 FIX ARCHIVE PATH LOGIC
      *  BAH 030425 ADDED LC-USE-ALT-GL-FEE AND CD-GC-ALT-GLFEENO FOR
      *             LENDMARK PR# 2048
      *  BAH 070703 DO NOT PRINT SSNO FOR WORLD, WORLD PR# 536
      *   CS 081112 ADD TEST ON GP-INCL-OTH-EARNED-R. REGACC DOES NOT WANT TO
      *             INCLUDE OTHER(ANTIC 6) OR MAINTFEE(ANTIC 4) IN
      *             EARNED/UNEARNED REGACC PR#210
      *  BLM 150526 ADD PARAMETERS FOR P&L AND ZERO BAL ACCOUNTS, SKIP-
      *             INCLUDE-ONLY, WORLD PR# 1084
      *  BLM 150526 MOVE PRCOL-FG (PRINT COLLATERAL) TO BE UNDER EOBUF,
      *             WOULD NOT HAVE CORRECT VALUE IN BATCH
      *  BAH 160118 SPLIT OUT LTFILE, PD0003
      *  KEC 2016.0315 CHANGED CD-FEE???? TO CD-GC-????.
      *  BAH 2017.0530 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 2017.0810 TESTING PL WAS ONLY CHECKING FOR A "P", NEED TO TEST
      *             IF SPACES OR NOT, CAN BE A "P" OR "I" OR "W"! WORLD #1269
      *  BAH 2019.0924 REMOVED ACCESS-CALL TRBFILE
      *  BAH 2021.1111 AUTO COLLATERAL HEADING WAS PRINTING WHEN NO SECOND AUTO
      *               WAS TESTING LTC-AUTO(2) WHICH HAS 0 NUMERICS
      *  RMZ 2023.0912 PERFORMANCE TUNING - ADDED STORED PROCEDURE
      *  RMZ 2024.0116 CHANGED JULIAN-BEG(END,CC) TO EXT-JULIAN-BEG(END,CC)
      *  BAH 2024.0624 REPORT-TYPE WAS NOT DISPLAYING IN DISPLAY-PARAMETERS
      *                                                 #1661 S35Q-72
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBGB/GBFSWKI.CPY".
      *    SELECT WK2-FILE ASSIGN WK2-PATH
      *           ORGANIZATION INDEXED
      *           ACCESS DYNAMIC
      *           LOCK MODE AUTOMATIC WITH LOCK ON RECORD
      *           RECORD KEY WK2-KEY
      *           FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      ******************************************************************
      *  SUMMARY TOTALS WORK RECORD
      ******************************************************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-BR        PIC 9(4).
               05  WK-CLASS     PIC 9(3).
               05  WK-CODE      PIC X(3).
           03  WK-NOLOANS       PIC S9(7)     COMP-3.
           03  WK-LNAMT         PIC S9(7)V99  COMP-3.
           03  WK-FINCHG        PIC S9(7)V99  COMP-3.
           03  WK-INTCHG        PIC S9(7)V99  COMP-3.
           03  WK-SERCHG        PIC S9(7)V99  COMP-3.
           03  WK-EXTCHG        PIC S9(5)V99  COMP-3.
           03  WK-MFEE          PIC S9(5)V99  COMP-3.
           03  WK-EARNINGS      PIC S9(7)V99  COMP-3.
           03  WK-DL            PIC S9(5)V99  COMP-3.
           03  WK-LL            PIC S9(5)V99  COMP-3.
           03  WK-AH            PIC S9(5)V99  COMP-3.
           03  WK-PP            PIC S9(5)V99  COMP-3.
           03  WK-O1            PIC S9(5)V99  COMP-3.
           03  WK-O2            PIC S9(5)V99  COMP-3.
           03  WK-O3            PIC S9(5)V99  COMP-3.
           03  WK-FEES          PIC S9(5)V99  COMP-3.
           03  WK-DLRVPRIN      OCCURS 3
                                PIC S9(7)V99  COMP-3.
           03  WK-DLRVCHGS      OCCURS 3
                                PIC S9(7)V99  COMP-3.
           03  WK-DLPARVHELD    PIC S9(7)V99  COMP-3.
           03  WK-DLPARVPAID    PIC S9(7)V99  COMP-3.
           03  WK-DLDISC        PIC S9(7)V99  COMP-3.
           03  WK-PROCEEDS      PIC S9(7)V99  COMP-3.
           03  WK-PURBAL        PIC S9(7)V99  COMP-3.
           03  WK-ANTICERN      PIC S9(7)V99  COMP-3.

      ******************************************************************
      * SORTED LNFILE BY BRANCH, CLASS, ACCOUNT - 2024 IN STORED PROCEDURE
      ******************************************************************
      *FD  WK2-FILE
      *    LABEL RECORDS ARE STANDARD.
      *01  WK2-REC.
      *    03  WK2-KEY.
      *        05  WK2-BRANCH           PIC 9(4).
      *        05  WK2-CLASS            PIC 9(3).
      *        05  WK2-ACCTNO           PIC 9(8).
      *        05  WK2-DATE             PIC 9(8).
      *        05  WK2-LAWCODE          PIC 99.
      *        05  WK2-LINE             PIC 9(3).
      *        05  WK2-REFBR            PIC 9(4).

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/EMTBRG.CBL S35 12/22/23-11:29:25 >".
      ******************************************************************
      *
      *   F I L E   P A T H S
      *
      ******************************************************************
       COPY "LIBLP/LP01TRB.CPY".
       COPY "LIBLP/LP01TRB_SQL.CPY".
       COPY "LIBLP/LPWSTRB.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01LTC.CPY".
       COPY "LIBLP/LP01LTC_SQL.CPY".
       COPY "LIBLP/LPWSLTC.CPY".
      *-----------------------------------------------------------------

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QWS-BW-OWNBR1        PIC 9(4)     VALUE 0.
       01  QWS-BW-SSNO1         PIC 9(9)     VALUE 0.
       01  QWS-BW-LNAME1        PIC X(35)    VALUE SPACES.
       01  QWS-BW-FNAME1        PIC X(35)    VALUE SPACES.
       01  QWS-BW-ADR11         PIC X(30)    VALUE SPACES.
       01  QWS-BW-ADR21         PIC X(30)    VALUE SPACES.
       01  QWS-BW-CITY1         PIC X(30)    VALUE SPACES.
       01  QWS-BW-STATE1        PIC X(2)     VALUE SPACES.
       01  QWS-BW-ZIP1          PIC X(10)    VALUE SPACES.
       01  QWS-BW-OWNBR2        PIC 9(4)     VALUE 0.
       01  QWS-BW-SSNO2         PIC 9(9)     VALUE 0.
       01  QWS-BW-LNAME2        PIC X(35)    VALUE SPACES.
       01  QWS-BW-FNAME2        PIC X(35)    VALUE SPACES.
       01  QWS-BW-ADR12         PIC X(30)    VALUE SPACES.
       01  QWS-BW-ADR22         PIC X(30)    VALUE SPACES.
       01  QWS-BW-CITY2         PIC X(30)    VALUE SPACES.
       01  QWS-BW-STATE2        PIC X(2)     VALUE SPACES.
       01  QWS-BW-ZIP2          PIC X(10)    VALUE SPACES.
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  TRB1-CONN-OPEN-SW    PIC X        VALUE "N".

       01  WK-IFN               PIC X(2)     VALUE "WK".
      *01  WK2-IFN              PIC X(3)     VALUE "WK2".
      *01  WK2-PATH             PIC X(35).

       01 T-RSVCHRG             PIC 9(6)V99  COMP-3.
       01 T-RSVPRIN             PIC 9(6)V99  COMP-3.
       01 TOT-DLRVPRIN          PIC 9(6)V99  COMP-3.
       01 TOT-DLRVCHGS          PIC 9(6)V99  COMP-3.
       01 T-WK-DLRVPRIN         PIC 9(6)V99  COMP-3.
       01 T-WK-DLRVCHGS         PIC 9(6)V99  COMP-3.
       01 TSUB                  PIC 9.

      ******************************************************************
      *
      *   LINE WORKERS
      *
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  ER-FEED              PIC 9        VALUE 1.

      ******************************************************************
      *
      *   MISC WORKERS
      *
      ******************************************************************
       01  MSEL-LIST            PIC X(5)      VALUE "MSEL.".
       01  SUB                  PIC 9(2)  COMP-3.
       01  I-SUB                PIC 9(6)  COMP-3.
       01  LEV                  PIC 9(2)  COMP-3.

       01  HOLD-BRNO            PIC 9(4)  VALUE 0.
       01  HOLD-CLASS           PIC 9(3)  VALUE 0.

       01  RECORDS-FOUND        PIC X     VALUE " ".
           88  NO-RECORDS-FOUND           VALUE "N".

       01  C-S-Z           PIC X(66).
       01  STATE           PIC XX.
       01  ZIP             PIC X(10).

       01  SAVE-WK-KEY.
           03  SAVE-WK-BR       PIC 9(4).
           03  SAVE-WK-CL       PIC 9(3).
           03  SAVE-WK-CODE     PIC XXX.

       01  SUMMARY-FG           PIC X     VALUE "N".

       01  FIRST-BORROWER-SW    PIC X        VALUE "N".
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

      *-----------------------------------------------------------------
       01  MESSAGE-01-LOC-PRT   PIC X(43)    VALUE
           "CANNOT DO LOCAL SPOOLING WITH GROUP TOTALS".
       01  RPT-BUF.
           03  RPT-PROMPT       PIC X(19).
           03  FILLER           PIC X(8)    VALUE "F3 - ALL".
       01  RPT-LIST             PIC X(24)
           VALUE "RPT-PRMPT:P RPT-PRMPT2:P".

      ******************************************************************
      *
      *    W S - G T T - T A B L E
      *
      ******************************************************************
       01  WS-GTT-TOTAL-TABLE.
           03  WS-GTT-TABLE-GROUP-LEVEL                 OCCURS 10 TIMES.
               05  WS-GTT-NOLOANS         PIC S9(7)     COMP.
               05  WS-GTT-LNAMT           PIC S9(7)V99  COMP.
               05  WS-GTT-FINCHG          PIC S9(7)V99  COMP.
               05  WS-GTT-INTCHG          PIC S9(7)V99  COMP.
               05  WS-GTT-SERCHG          PIC S9(7)V99  COMP.
               05  WS-GTT-EXTCHG          PIC S9(5)V99  COMP.
               05  WS-GTT-MFEE            PIC S9(5)V99  COMP.
               05  WS-GTT-EARNINGS        PIC S9(7)V99  COMP.
               05  WS-GTT-DL              PIC S9(5)V99  COMP.
               05  WS-GTT-LL              PIC S9(5)V99  COMP.
               05  WS-GTT-AH              PIC S9(5)V99  COMP.
               05  WS-GTT-PP              PIC S9(5)V99  COMP.
               05  WS-GTT-O1              PIC S9(5)V99  COMP.
               05  WS-GTT-O2              PIC S9(5)V99  COMP.
               05  WS-GTT-O3              PIC S9(5)V99  COMP.
               05  WS-GTT-FEES            PIC S9(5)V99  COMP.
               05  WS-GTT-DLRVPRIN        PIC S9(7)V99  COMP.
      *--------05  WS-GTT-DLRVPRIN      OCCURS 3
      *                                   PIC S9(7)V99  COMP.
      *        05  WS-GTT-DLRVCHGS      OCCURS 3
      *---------                          PIC S9(7)V99  COMP.
               05  WS-GTT-DLRVCHGS        PIC S9(7)V99  COMP.
               05  WS-GTT-DLPARVHELD      PIC S9(7)V99  COMP.
               05  WS-GTT-DLPARVPAID      PIC S9(7)V99  COMP.
               05  WS-GTT-DLDISC          PIC S9(7)V99  COMP.
               05  WS-GTT-PROCEEDS        PIC S9(7)V99  COMP.
               05  WS-GTT-PURBAL          PIC S9(7)V99  COMP.
               05  WS-GTT-ANTICERN        PIC S9(7)V99  COMP.
      *-----------------------------------------------------------------
       01  WS-HOLD-PROMPT       PIC X(7)  VALUE SPACES.

      ******************************************************************
      *  TOTALS LEV1 = CLASS TOTALS
      *            2 = BRANCH TOTALS
      *            3 = GRAND TOTALS
      ******************************************************************
       01  TOTALS-TAB.
           03  TOTAL-TBL       OCCURS 3.
               05  T-NOLOANS       PIC S9(7)     COMP-3.
               05  T-LNAMT         PIC S9(7)V99  COMP-3.
               05  T-FINCHG        PIC S9(7)V99  COMP-3.
               05  T-INTCHG        PIC S9(7)V99  COMP-3.
               05  T-SERCHG        PIC S9(7)V99  COMP-3.
               05  T-EXTCHG        PIC S9(5)V99  COMP-3.
               05  T-MFEE          PIC S9(5)V99  COMP-3.
               05  T-EARNINGS      PIC S9(7)V99  COMP-3.
               05  T-DL            PIC S9(5)V99  COMP-3.
               05  T-LL            PIC S9(5)V99  COMP-3.
               05  T-AH            PIC S9(5)V99  COMP-3.
               05  T-PP            PIC S9(5)V99  COMP-3.
               05  T-O1            PIC S9(5)V99  COMP-3.
               05  T-O2            PIC S9(5)V99  COMP-3.
               05  T-O3            PIC S9(5)V99  COMP-3.
               05  T-FEES          PIC S9(5)V99  COMP-3.
               05  T-DLRVPRIN      OCCURS 3
                                   PIC S9(7)V99  COMP-3.
               05  T-DLRVCHGS      OCCURS 3
                                   PIC S9(7)V99  COMP-3.
               05  T-DLPARVHELD    PIC S9(7)V99  COMP-3.
               05  T-DLPARVPAID    PIC S9(7)V99  COMP-3.
               05  T-DLDISC        PIC S9(7)V99  COMP-3.
               05  T-PROCEEDS      PIC S9(7)V99  COMP-3.
               05  T-PURBAL        PIC S9(7)V99  COMP-3.
               05  T-ANTICERN      PIC S9(7)V99  COMP-3.

       01  CLASS-FG             PIC X(3)    VALUE "   ".
           88  CLASS-BEG                    VALUE "BEG".
           88  CLASS-END                    VALUE "END".
      *-----------------------------------------------------------------
       COPY "LIBLP/EMTBRG_CMD.CPY".
      *-----------------------------------------------------------------
       01  LEGEND               PIC X(70).

      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-BRANCH       PIC 9(4).
           03  END-BRANCH       PIC 9(4).
           03  BEG-CLASS        PIC 9(3).
           03  END-CLASS        PIC 9(3).
           03  REPORT-TYPE      PIC X.
           03  FROM-TO-BRNO-ALP PIC X(4).
           03  FROM-TO-BRNO     REDEFINES FROM-TO-BRNO-ALP
                                PIC 9(4).
           03  FROM-TO-BRNO-ALL PIC X.
           03  END-DATE.
               05  END-MONTH    PIC 99.
               05  END-YEAR     PIC 99.
           03  EDATE REDEFINES END-DATE PIC 9999.
           03  DAY-OF-MONTH     PIC 99.
           03  GTT-ISO          PIC X.
           03  SUM-ONLY         PIC X.
           03  GROUP-FLAG       PIC X.
           03  ENT-GROUP        PIC X(4).
           03  LOCAL-PRINT-FG   PIC X(1).
           03  TOTAL-LEV-TABLE.
               05  TOTAL-LEVEL-TABLE  PIC X(1)  OCCURS 10.
           03  PL               PIC X.
           03  ZBAL             PIC X.
           03  PRCOL-FG         PIC X.

       01  VERSION-CONTROL      PIC X VALUE "D".
       01  BEGINNING-DATE       PIC 9(8).
       01  ENDING-DATE          PIC 9(8).

       01  SV-BEG-BR            PIC 9999  VALUE 9999.
       01  SV-END-BR            PIC 9999  VALUE 9999.
       01  SV-GROUP-FLAG        PIC X     VALUE SPACES.
       01  SV-ENT-GROUP         PIC X(4)  VALUE SPACES.
      *-----------------------------------------------------------------
       COPY "LIBGB/PASSWDW.CPY".
      *-----------------------------------------------------------------

      ******************************************************************
      *
      *     P R I N T   B U F F E R S
      *
      ******************************************************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-TIME           PIC X(8)     VALUE " ".
           03  FILLER           PIC X(28)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(37)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(26)    VALUE " ".
           03  FILLER           PIC X(39) VALUE
               "BRANCH (BULK) ACCOUNT TRANSFER REGISTER".

      *-----------------------------------------------------------------
       01  HEAD3.
           03  FILLER           PIC X(10) VALUE "BRANCH :".
           03  H-BRNO           PIC 9(4).
           03  FILLER           PIC X VALUE " ".
           03  H-BRDESC         PIC X(20).
           03  FILLER           PIC X(16).
           03  H-RPT-TYP        PIC X(41) VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEAD4.
           03  FILLER           PIC X(10) VALUE "CLASS  :".
           03  H-LCNO           PIC 99.
           03  FILLER           PIC X VALUE " ".
           03  H-LCDESC         PIC X(20).

      *-----------------------------------------------------------------
       01  HEAD5.
           03  FILLER           PIC X(40)  VALUE " ".
           03  FILLER           PIC X(44)  VALUE
           "CC TERM  LOAN-AMT    FIN-CHG   INSURANCES   ".
           03  FILLER           PIC X(47)  VALUE
           "----- FEES -----  RSVPRIN    PARHELD   PROCEEDS".

      *-----------------------------------------------------------------
       01  HEAD6.
           03  FILLER           PIC X(50)  VALUE
           "ACCOUNT NO.   LOANDATE  DEALER  APR  SC PC ATRM   ".
           03  FILLER           PIC X(52)  VALUE
           "PUR-AMT   ANTICERN                GL-NO  AMOUNT     ".
           03  FILLER           PIC X(29)  VALUE
           "RSVCHRG    PARPAID   EARNINGS".

      *-----------------------------------------------------------------
       01  HEAD7.
           03  FILLER          PIC X(60).
           03  FILLER          PIC X(8)   VALUE "REGPYAMT".
           03  FILLER          PIC X(34).
           03  HEAD7-DLR-TYP   PIC X(4)    VALUE "COMM".
           03  FILLER          PIC X(7).
           03  HEAD7-FR-TO     PIC X(7)   VALUE
      *--->"                 PREM-ADJ".
           "FROM BR".
           03  FILLER          PIC X(11)  VALUE
           "   POST DTE".

      *-----------------------------------------------------------------
       01  HEAD8.
           03  FILLER            PIC X(26) VALUE " ".
           03  FILLER            PIC X(47) VALUE
           "LOAN-AMT    FIN-CHG   INTCHRG   EXTCHG   OUR-DL".
           03  FILLER            PIC X(47) VALUE
           "   OUR-AH   OUR-O1      OUR   RSVPRIN   PARHELD".
           03  FILLER            PIC X(12) VALUE
           "   EARNINGS".

      *-----------------------------------------------------------------
       01  HEAD9.
           03  FILLER            PIC X(18) VALUE " ".
           03  FILLER            PIC X(46) VALUE
           "LOANS    PUR-AMT   ANTICERN   SRVCHRG     MFEE".
           03  FILLER            PIC X(49) VALUE
           "   OUR-LL   OUR-PP   OUR-O2     FEES   RSVCHRG   ".
           03  FILLER            PIC X(19) VALUE
           "PARPAID   PROCEEDS".

      *-----------------------------------------------------------------
       01  HEAD10.
           03  FILLER          PIC X(58).
           03  FILLER          PIC X(6)    VALUE " ".
           03  FILLER          PIC X(21).
           03  FILLER          PIC X(18)   VALUE "OUR-O3".
           03  HEAD10-DLR-TYP  PIC X(4)    VALUE "COMM".
           03  FILLER          PIC X(25)   VALUE
           "                PREM-ADJ".

      *-----------------------------------------------------------------
       01  GTT-HEAD1.
           03  FILLER            PIC X(57)  VALUE SPACES.
           03  FILLER            PIC X(18)  VALUE "GROUP TOTALS TABLE".
           03  FILLER            PIC X(57)  VALUE SPACES.

      *-----------------------------------------------------------------
       01  GTT-HEAD2.
           03  FILLER            PIC X(24)  VALUE
               "BRANCH/GROUP   GR       ".
      *----03  FILLER            PIC X(28) VALUE
      *--------"LOAN".
           03  FILLER            PIC X(49) VALUE
           "  LOAN-AMT    FIN-CHG   INTCHRG   EXTCHG   OUR-DL".
           03  FILLER            PIC X(47) VALUE
           "   OUR-AH   OUR-O1      OUR   RSVPRIN   PARHELD".
           03  FILLER            PIC X(12) VALUE
           "   EARNINGS".

      *-----------------------------------------------------------------
       01  GTT-HEAD3.
           03  FILLER            PIC X(17) VALUE "              LEV".
           03  FILLER            PIC X(47) VALUE
           "          PUR-AMT   ANTICERN   SRVCHRG     MFEE".
           03  FILLER            PIC X(49) VALUE
           "   OUR-LL   OUR-PP   OUR-O2     FEES   RSVCHRG   ".
           03  FILLER            PIC X(19) VALUE
           "PARPAID   PROCEEDS".
           03  FILLER            PIC X(28) VALUE " ".
      *----"LOAN".
           03  FILLER            PIC X(42) VALUE
           "EXTCHG   OUR-DL   OUR-AH   OUR-O1      OUR".
           03  FILLER            PIC X(34) VALUE " ".

      *-----------------------------------------------------------------
       01  GTT-HEAD4.
           03  FILLER          PIC X(58).
           03  FILLER          PIC X(4)    VALUE " ".
           03  FILLER          PIC X(23).
           03  FILLER          PIC X(18)   VALUE "OUR-O3".
           03  FILLER          PIC X(4)    VALUE "COMM".
           03  FILLER          PIC X(25)   VALUE
           "                PREM-ADJ".

      ******************************************************************
      *
      *    D E T A I L - L I N E
      *
      ******************************************************************
       01  DETAIL-LINE          PIC X(132)  VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  D-CLASS          PIC 99.
           03  D-H1             PIC X.
           03  D-NUMBER         PIC 9(6).
           03  D-H2             PIC X.
           03  D-LAWCODE        PIC 99.
           03  FILLER           PIC X(2).
           03  D-LNDTE          PIC 99/99/99  BLANK ZERO.
           03  FILLER           PIC X.
           03  D-DLNO           PIC Z(4).99  BLANK ZERO.
           03  FILLER           PIC X.
           03  D-APR            PIC Z9.99    BLANK ZERO.
           03  FILLER           PIC X.
           03  D-SRCD           PIC XX.
           03  FILLER           PIC X.
           03  D-COLCD          PIC XXX.
           03  FILLER           PIC X.
           03  D-TERM           PIC ZZ9.
           03  FILLER           PIC X.
           03  D-LNAMT          PIC Z(6).99-  BLANK ZERO.
           03  D-FINCHG         PIC Z(6)9.99-  BLANK ZERO.
           03  FILLER           PIC X(30).
           03  D-RSV1           PIC Z(6)9.99- BLANK ZERO.
           03  D-RSV2           PIC Z(6)9.99- BLANK ZERO.
           03  D-PROCEEDS       PIC Z(6)9.99-  BLANK ZERO.

      *-----------------------------------------------------------------
       01  DETAIL2 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X.
           03  D-MAKERCD        PIC X.
           03  FILLER           PIC X.
           03  D-BW-SSNO        PIC X(11).
           03  FILLER           PIC XX.
           03  D-BW-NAME        PIC X(23).

      *-----------------------------------------------------------------
       01  DETAIL3 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(2).
           03  D-BW-ADDR        PIC X(56).
           03  FILLER           PIC X(11).

      *-----------------------------------------------------------------
       01  DETAIL4 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(116).
           03  D-FROM-BRNO      PIC ZZZ9      BLANK ZERO.
           03  FILLER           PIC X(3).
           03  D-TRANS-DATE     PIC 99/99/99  BLANK ZERO.
           03  FILLER           PIC X(1).

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  D-LINE2 REDEFINES DETAIL-LINE.
           03  D2-GROUP             PIC X(17).
           03  D2-GROUP-1           REDEFINES D2-GROUP.
               05  S-TOTNAME        PIC X(17).
               05  S-CODENAME REDEFINES S-TOTNAME.
                   07  FILLER       PIC X(10).
                   07  S-DCODE      PIC XXX.
                   07  S-COLON      PIC X.
                   07  FILLER       PIC X(3).
           03  D2-GROUP-2           REDEFINES D2-GROUP.
               05  D2-PROMPT1       PIC X(7).
               05  FILLER           PIC X(2).
               05  D2-GRNO          PIC X(4).
               05  D2-BRNO          REDEFINES D2-GRNO
                                    PIC 9(4).
               05  FILLER           PIC X(3).
               05  D2-GR-LEVEL      PIC 9(1).
           03  S-NOLOANS        PIC Z(5)9-  BLANK ZERO.
           03  S-LNAMT          PIC Z(6)9.99-  BLANK ZERO.
           03  S-FINCHG         PIC Z(6)9.99-  BLANK ZERO.
           03  S-INTCHG         PIC Z(5)9.99-  BLANK ZERO.
           03  S-EXTCHG         PIC Z(4)9.99-  BLANK ZERO.
           03  S-INS-CL-AH.
               05  S-CL         PIC Z(4)9.99-  BLANK ZERO.
               05  S-AH         PIC Z(4)9.99-  BLANK ZERO.
           03  S-OT             PIC Z(4)9.99-  BLANK ZERO.
           03  S-FEEAMT         PIC Z(4)9.99-  BLANK ZERO.
           03  S-RSV1           PIC Z(5)9.99   BLANK ZERO.
           03  S-RSV2           PIC Z(5)9.99   BLANK ZERO.
           03  FILLER           PIC X.
           03  S-EARNINGS       PIC Z(6)9.99-  BLANK ZERO.

      *-----------------------------------------------------------------
      * DETAIL LINE FOR OUTPUT OF AUTOMOTIVE COLLATERAL
      *-----------------------------------------------------------------
       01  D-LINE1  REDEFINES DETAIL-LINE.
           03  FILLER         PIC X.
           03  FILLER         PIC X(17).
           03  D-FIL-MAKE     PIC X(7).
           03  D-MAKE         PIC X(15).
           03  FILLER         PIC XX.
           03  D-FIL-MODEL    PIC X(8).
           03  D-MODEL        PIC X(15).
           03  FILLER         PIC XX.
           03  D-FIL-YEAR     PIC X(7).
           03  D-YEAR         PIC X(4).
           03  FILLER         PIC XX.
           03  D-FIL-ID       PIC X(8).
           03  D-ID           PIC X(20).
           03  FILLER         PIC X(24).

      *-----------------------------------------------------------------
       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(35).
           03  RANGE-SELECTION.
               04  R-NUM        PIC Z(5)9.
               04  R-NUM-REDEF REDEFINES R-NUM.
                   05  R-CHAR   PIC X(6).
      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBWI/GRTOTLW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/EMTBRG_DEF.CPY".
       COPY "LIBLP/EMTBRG_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/EMTBRG_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
      *        PR-FILE WK-FILE WK2-FILE.
               PR-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TRB1-FILE.
           PERFORM CLOSE-TRB1-CONNECTION.
           PERFORM CLOSE-LTC1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK-FILE.
      *    CLOSE WK-FILE WK2-FILE.
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
           MOVE "EMTBRG" TO FORM-PROG VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE "N" TO RECORDS-FOUND.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *-----------------------------------------------------------------
       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.

           IF WGR-SRESULT = 0
              GO TO END-ROUTINE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

      *-----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

      *-----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.
           IF LOCAL-PRINT-FG = "Y"
              PERFORM CLOSE-PR-FILE
              PERFORM CREATE-SPOOL-DIR
              IF STAT NOT = "00"
                 GO TO NEXT-GROUP-LOC.

           IF EXT-ROUTE-BUF = "00"
              MOVE SPACES TO WR-BUF
              STRING "REPORT IN PROGRESS............", BR-NO
                     DELIMITED "  " INTO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              LTC-PATH-OVERRIDE
                                              TRB-PATH-OVERRIDE.

           PERFORM OPEN-TRB1-FILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN1-FILE.

      *    PERFORM SORT-ROUTINE.

      *    IF EXT-EOBUF-ERRCD = "X"
      *       GO TO NEXT-GROUP-LOC.

      *    PERFORM OPEN-WK2-FILE.

           MOVE BR-NO               TO TB-BRNO
                                       QTRB1-WBEG-BRNO
                                       QTRB1-WEND-BRNO.
           MOVE BEGINNING-DATE      TO TB-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRB1-WBEG-DATE.
           MOVE ENDING-DATE         TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRB1-WEND-DATE.
           MOVE BEG-CLASS           TO TB-CLASS
                                       QTRB1-WBEG-CLASS.
           MOVE END-CLASS           TO QTRB1-WEND-CLASS.
           MOVE 0                   TO TB-ACCTNO TB-LAWCODE TB-LINE
                                       QTRB1-WBEG-ACCTNO
                                       QTRB1-WBEG-LAWCODE
                                       QTRB1-WBEG-LINE.
           MOVE ALL "9"             TO QTRB1-WEND-ACCTNO
                                       QTRB1-WEND-LAWCODE
                                       QTRB1-WEND-LINE.

           PERFORM START-TRB1-FILE. *> SSP - TRB,LN,LTC,BW

      *-----------------------------------------------------------------
       NEXT-TR-REPORT.
      *    PERFORM READ-WK2-FILE-NEXT.
      *    IF IO-FG = 9
      *       GO TO END-OF-GROUP.

      *    MOVE WK2-BRANCH  TO TB-BRNO.
      *    MOVE WK2-DATE    TO TB-DATE.
      *    MOVE WK2-CLASS   TO TB-CLASS.
      *    MOVE WK2-ACCTNO  TO TB-ACCTNO.
      *    MOVE WK2-LAWCODE TO TB-LAWCODE.
      *    MOVE WK2-LINE    TO TB-LINE.

      *    PERFORM READ-TRB1-FILE.
      *    IF IO-FG NOT = 0
      *       MOVE "TRANSACTION RECORD MISSING SINCE SORT" TO MESS
      *       PERFORM SEND-MESS
      *       GO TO IO-ERROR.

           PERFORM READ-TRB1-FILE-NEXT. *> EMBEDDED - TRB,LN,LTC,BW
           IF (IO-FG NOT = 0) OR (TRB-PATH-OWNBR NOT = TB-BRNO)
              GO TO END-OF-GROUP.            

           IF TB-DATE > ENDING-DATE
              GO TO END-OF-GROUP.             

           IF TB-CLASS < BEG-CLASS OR TB-CLASS > END-CLASS
              GO TO NEXT-TR-REPORT.

      *----------------------------------------------------------------
      *  TB-REF-FRTO -->  T : TRANSFERRED TO ANOTHER BRANCH (SOLD).
      *                   F : TRANSFERRED FROM ANOTHER BRANCH (BOUGHT).
      *----------------------------------------------------------------
           IF REPORT-TYPE = "1"
              IF TB-REF-FRTO NOT = "F"
                 GO TO NEXT-TR-REPORT
              END-IF
           ELSE
              IF REPORT-TYPE = "2"
                 IF TB-REF-FRTO NOT = "T"
                    GO TO NEXT-TR-REPORT.

      *    -------------------------------------------------------------
      *    TEST BOUGHT FROM/SOLD TO BRANCH
      *    -------------------------------------------------------------
           IF FROM-TO-BRNO-ALL = "N"
              IF TB-REFBR NOT = FROM-TO-BRNO
                 GO TO NEXT-TR-REPORT.

           IF PL = "O"
              IF TB-PLCD = " "
                 GO TO NEXT-TR-REPORT.
           IF PL = "S"
              IF TB-PLCD NOT = " "
                 GO TO NEXT-TR-REPORT.

           IF ZBAL = "O"
              IF TB-PURBAL NOT = 0
                 GO TO NEXT-TR-REPORT.
           IF ZBAL = "S"
              IF TB-PURBAL = 0
                 GO TO NEXT-TR-REPORT.

           IF FIRST-TIME = "Y"
              MOVE TB-BRNO TO HOLD-BRNO
              MOVE TB-CLASS TO HOLD-CLASS
              PERFORM SET-CLASS-HEADINGS
              PERFORM SET-BRANCH-HEADINGS.

           IF TB-BRNO NOT = HOLD-BRNO
              PERFORM CLASS-TOTALS
              PERFORM BRANCH-TOTALS
              MOVE TB-BRNO TO HOLD-BRNO
              MOVE TB-CLASS TO HOLD-CLASS
              PERFORM SET-CLASS-HEADINGS
              PERFORM SET-BRANCH-HEADINGS
              IF ( GTT-ISO NOT = "O" )
                 IF SUM-ONLY = "N"
                    PERFORM HEAD-PAGE.

           IF TB-CLASS NOT = HOLD-CLASS
              PERFORM CLASS-TOTALS
              MOVE TB-CLASS TO HOLD-CLASS
              PERFORM SET-CLASS-HEADINGS
              IF ( GTT-ISO NOT = "O" )
                 IF SUM-ONLY = "N"
                    PERFORM HEAD-PAGE.

           PERFORM TR-DETAIL1.

           MOVE 1 TO LEV.
           PERFORM UPDATE-TABLE.
           PERFORM CREATE-WK.

           GO TO NEXT-TR-REPORT.

      *-----------------------------------------------------------------
       END-OF-GROUP.
           PERFORM CLOSE-TRB1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LN1-FILE.
      *    PERFORM CLOSE-WK2-FILE.
      *    PERFORM OPEN-WK2-FILE-OUTPUT.

           IF ( LOCAL-PRINT-FG = "Y" )
              IF ( FIRST-TIME NOT = "Y" )
                 PERFORM CLASS-TOTALS
                 PERFORM BRANCH-TOTALS
                 MOVE "Y" TO FIRST-TIME
                 MOVE 0 TO PAGE-NUMBER
              ELSE
                 MOVE 0 TO PAGE-NUMBER.

           GO TO NEXT-GROUP-LOC.

      *-----------------------------------------------------------------
       END-ROUTINE.
           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.

           IF LOCAL-PRINT-FG = "N"
              PERFORM CLASS-TOTALS
              PERFORM BRANCH-TOTALS
              PERFORM GRAND-TOTALS
              IF ( GTT-ISO NOT = "S" )
                 MOVE 99 TO LN-COUNT
                 PERFORM GROUP-TOTAL-TABLE
                 PERFORM PRINT-RANGE-LINES
              ELSE
                 PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE EMTBRG-QUEUE-POS TO Q-POS.
           MOVE EMTBRG-COPIES-POS TO C-POS.
           MOVE EMTBRG-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           MOVE 1 TO SUB.

       INIT-NEXT-TOTAL.
           PERFORM CLEAR-TOTAL-TBL.

           IF SUB < 3
              ADD 1 TO SUB
              GO TO INIT-NEXT-TOTAL.

           MOVE 0 TO PAGE-NUMBER.

      * CREATE SORT KEY FILE FOR TRFILE OUTPUT: - 2024 IN STORED PROCEDURE
      *    MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
      *    PERFORM MKTEMP-CALL.
      *    IF STAT NOT = "00"
      *       MOVE "CANNOT CREATE SORT KEY FILE" TO MESS
      *       PERFORM SEND-MESS
      *       GO TO ENDIT.

      *    MOVE MKTEMP-BUF TO WK2-PATH.
      *    PERFORM OPEN-WK2-FILE-OUTPUT.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT NOT = "00"
              MOVE "CANNOT CREATE GL WORK FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           PERFORM GET-GPENV.

           IF BEG-BRANCH = 0
              IF END-BRANCH = 0
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH
                                   END-BRANCH.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BRANCH TO SV-BEG-BR
              MOVE END-BRANCH TO SV-END-BR
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP TO SV-ENT-GROUP
              MOVE EXT-EOBUF-GROUP TO ENT-GROUP
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              MOVE "Y" TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS............" TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

      * SET DATE RANGE FOR TESTING TR FILES
           MOVE END-MONTH TO DATE-MMDDYY-MM.
           MOVE END-YEAR  TO DATE-MMDDYY-YY.
           IF DAY-OF-MONTH = 99
              MOVE 1      TO DATE-MMDDYY-DD
           ELSE
              MOVE DAY-OF-MONTH TO DATE-MMDDYY-DD.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO BEGINNING-DATE.

           MOVE END-MONTH TO DATE-MMDDYY-MM.
           MOVE END-YEAR  TO DATE-MMDDYY-YY.
           IF DAY-OF-MONTH = 99
              MOVE 31      TO DATE-MMDDYY-DD
           ELSE
              MOVE DAY-OF-MONTH TO DATE-MMDDYY-DD.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NDTE-DATE.
           PERFORM NEW-DATE-CALCULATION.
           MOVE NDTE-DATE TO ENDING-DATE.

           IF LOCAL-PRINT-FG NOT = "Y"
              PERFORM OPEN-PR-FILE.

           IF ( GROUP-FLAG = "Y" )
              MOVE LOW-VALUES TO WS-GTT-TOTAL-TABLE
              MOVE ENT-GROUP TO GR-ID
              PERFORM SEARCH-GR-INIT
              GO TO EXIT-INITIALIZE
           ELSE
              MOVE SPACES TO TOTAL-LEV-TABLE
              MOVE "Y"    TO TOTAL-LEVEL-TABLE(9).

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE2 TO SYS-DATE.
           MOVE S-MM TO END-MONTH.
           MOVE S-YY TO END-YEAR.

           IF EXT-EOBUF-DATE1 NOT = EXT-EOBUF-DATE2
              MOVE 99 TO DAY-OF-MONTH
           ELSE
              MOVE S-DD TO DAY-OF-MONTH.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              IF ( GTT-ISO = "O" )
                 GO TO GTT-ISO-PARAGRAPH
              ELSE
                 GO TO SUMYN.

      *-----------------------------------------------------------------
       BR-01.
           MOVE LEGEND-BR-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "R  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO BR-01
              ELSE
                 MOVE "S  A000BR1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.

           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-01.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF WGR-WORD-ZERO
              MOVE 0 TO WGR-SRESULT
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
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.

      *-----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       BR-02.
           MOVE LEGEND-BR-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.

           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           GO TO CHECK-BRANCH-SECURITY.

      *-----------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A160DISPLAY-USER." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

      *-----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

      *-----------------------------------------------------------------
       CL-01.
           MOVE LEGEND-CL-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              MOVE "BEG" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO CL-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-01.
           MOVE VDUNUM0 TO BEG-CLASS.

      *-----------------------------------------------------------------
       CL-02.
           MOVE LEGEND-CL-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              MOVE "END" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO CL-02.

           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-02.
           MOVE VDUNUM0 TO END-CLASS.

           IF BEG-CLASS > END-CLASS
              MOVE "INVALID CLASS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO CL-01.

           GO TO RPT-TYPE.

      *-----------------------------------------------------------------
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       RPT-TYPE.
           MOVE "E NA010RPTYPE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO RPT-TYPE.
           MOVE VDUBUF TO REPORT-TYPE.
           IF REPORT-TYPE NOT = 1 AND REPORT-TYPE NOT = 2
              GO TO RPT-TYPE.
      *-----------------------------------------------------------------
       RPT-BRNO.
           IF REPORT-TYPE = "1"
              MOVE "BOUGHT FROM BRANCH:" TO RPT-PROMPT
           ELSE
              MOVE "SOLD TO BRANCH   :" TO RPT-PROMPT.
           MOVE RPT-BUF TO WR-BUF.
           MOVE RPT-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE "ENNA040RPT-BRNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO RPT-TYPE.
           IF VDUSTATUS = "16" GO TO RPT-ALL.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO RPT-TYPE.
           IF VDUBUF = "****"
              GO TO RPT-ALL.
           MOVE VDUBUF TO FROM-TO-BRNO-ALP.
           INSPECT FROM-TO-BRNO REPLACING ALL SPACES BY ZEROS.
           MOVE FROM-TO-BRNO TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID BRANCH" TO MESS
              PERFORM SEND-MESS
              GO TO RPT-BRNO.
           MOVE "N" TO FROM-TO-BRNO-ALL.
           GO TO DTE.
       RPT-ALL.
           MOVE "S  A040RPT-BRNO." TO VDU.
           MOVE "****" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE 0 TO FROM-TO-BRNO.
           MOVE "Y" TO FROM-TO-BRNO-ALL.

       DTE.
           IF EXT-ROUTE-BUF NOT = "00" AND VDUSTATUS = "1U"
              GO TO CL-02
           ELSE
              IF EXT-ROUTE-BUF NOT = "00"
                 GO TO DA-01.

           MOVE LEGEND-DTE TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040ENDDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO RPT-BRNO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DTE.
           MOVE VDUNUM0 TO EDATE.

           IF END-MONTH < 1 OR > 12
              MOVE "INVALID MONTH" TO MESS
              PERFORM SEND-MESS
              GO TO DTE.

      *-----------------------------------------------------------------
       DA-01.
           MOVE LEGEND-DA-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020DA1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.

           IF VDUSTATUS = "16"
              MOVE "S  A020DA1." TO VDU
              MOVE "**" TO VDUBUF
              PERFORM SCREENIO
              MOVE 99 TO DAY-OF-MONTH
              GO TO PLYN.

           IF VDUSTATUS = "1U" GO TO DTE.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DTE.

           IF VDUNUM0 = 0
              GO TO DA-01.

           MOVE VDUNUM0 TO DAY-OF-MONTH.

           IF DAY-OF-MONTH > 31  OR
              (DAY-OF-MONTH > 30 AND
              (END-MONTH = 2 OR 4 OR 6 OR 9 OR 11)) OR
              (DAY-OF-MONTH > 29 AND END-MONTH = 2)
              MOVE "DAY IS PAST LAST DAY OF MONTH" TO MESS
              PERFORM SEND-MESS
              GO TO DA-01.
      *----------------------------------------------------------------
       PLYN.
           MOVE "E  A010PL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              IF ( DAY-OF-MONTH = 99 )
                 MOVE "SNNN020DA1." TO VDU
                 MOVE 99 TO VDUNUM0
                 PERFORM SCREENIO
                 GO TO DA-01
              ELSE
                 GO TO DA-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO PLYN.
           MOVE VDUBUF TO PL.
           IF NOT (PL = "I" OR "O" OR "S")
              GO TO PLYN.
      *----------------------------------------------------------------
       ZBALYN.
           MOVE "E  A010ZBAL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO PLYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ZBALYN.
           MOVE VDUBUF TO ZBAL.
           IF NOT (ZBAL = "I" OR "O" OR "S")
              GO TO ZBALYN.
      *-----------------------------------------------------------------
       PRINTCOL.
           MOVE LEGEND-PRINTCOL-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010PRCOL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO EXIT-PARM.
           IF VDUSTATUS = "1U"
              GO TO ZBALYN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO PRINTCOL.

           IF VDUBUF NOT = "Y" AND
              VDUBUF NOT = "N"
                 MOVE "ENTER Y OR N ONLY " TO MESS
                 PERFORM SEND-MESS
                 GO TO PRINTCOL
           ELSE
                 MOVE VDUBUF TO PRCOL-FG.

      *-----------------------------------------------------------------
       GTT-ISO-PARAGRAPH.

           IF ( GROUP-FLAG NOT = "Y" )
              MOVE SPACES TO TOTAL-LEV-TABLE
              MOVE "S  A010GTT-ISO." TO VDU
              MOVE "S" TO VDUBUF GTT-ISO
              PERFORM SCREENIO
              GO TO SUMYN.

           MOVE LEGEND-GTT-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010GTT-ISO." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1>" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1U" ) GO TO PRINTCOL.
           IF NOT ( VDUSTATUS = "00" OR "1D" )
              GO TO GTT-ISO-PARAGRAPH.

           IF NOT ( VDUBUF = "I" OR "S" OR "O" )
              GO TO GTT-ISO-PARAGRAPH.

           MOVE VDUBUF TO GTT-ISO.

           IF ( GTT-ISO = "I" OR "O" )
              PERFORM GRTOTL-WINDOW
           ELSE
              MOVE SPACES TO TOTAL-LEV-TABLE.

      *----------------------------------------------------------------
       SUMYN.
           IF ( GTT-ISO = "O" )
              MOVE "S  A010SUMONLY." TO VDU
              MOVE "Y" TO SUM-ONLY VDUBUF
              PERFORM SCREENIO
              GO TO LOCAL-PRINT.

           MOVE "E  A010SUMONLY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO PRINTCOL.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO SUMYN.
           MOVE VDUBUF TO SUM-ONLY.
           IF NOT ( SUM-ONLY = "Y" OR "N" )
              GO TO SUMYN.

      *-----------------------------------------------------------------
       LOCAL-PRINT.
           IF EXT-ROUTE-BUF NOT = "40"
              IF EXT-ROUTE-BUF NOT = "50"
                 MOVE "N" TO LOCAL-PRINT-FG
                 GO TO END-PARM.

           IF BEG-BRANCH = END-BRANCH
              MOVE "N" TO LOCAL-PRINT-FG
              GO TO END-PARM.

      *-----------------------------------------------------------------
       ASK-LOCAL.
           MOVE "SPOOL TO LOCAL DIRECTORY? (Y/N)" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "S  A170SPOOL-PROMPT." TO VDU.
           MOVE "SPOOL TO LOCAL  :"    TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A050YES-NO." TO VDU.
           MOVE "(Y/N)"          TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "E  A010LOCAL-SPOOL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U"
              IF ( GROUP-FLAG = "Y" )
                 IF (GTT-ISO = "O" )
                    GO TO GTT-ISO-PARAGRAPH
                 ELSE
                    GO TO SUMYN
              ELSE
                  GO TO SUMYN.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO ASK-LOCAL.

           IF VDUBUF NOT = "Y"
              IF VDUBUF NOT = "N"
                 GO TO ASK-LOCAL.

           MOVE VDUBUF TO LOCAL-PRINT-FG.

           IF ( GTT-ISO NOT = "S" )
              IF ( LOCAL-PRINT-FG = "Y" )
                 MOVE MESSAGE-01-LOC-PRT TO MESS
                 PERFORM SEND-MESS
                 GO TO ASK-LOCAL.

      *-----------------------------------------------------------------
       END-PARM.
           MOVE LEGEND-ACCEPT TO LEGEND.
           PERFORM SEND-LEGEND.

       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************

           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF GTT-SV-ENT-GROUP
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BRANCH TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BRANCH TO VDUNUM0
              PERFORM SCREENIO.

           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           MOVE "SNNN020CL1." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A010RPTYPE." TO VDU.
           MOVE REPORT-TYPE TO VDUBUF.
           PERFORM SCREENIO.

           IF REPORT-TYPE = "1"
              MOVE "BOUGHT FROM BRANCH:" TO RPT-PROMPT
           ELSE
              MOVE "SOLD TO BRANCH   :" TO RPT-PROMPT.
           MOVE RPT-BUF TO WR-BUF.
           MOVE RPT-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "S  A040RPT-BRNO." TO VDU.
           IF FROM-TO-BRNO-ALL = "Y"
              MOVE "****" TO VDUBUF
           ELSE
              MOVE FROM-TO-BRNO-ALP TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SNNN020DA1." TO VDU.
           MOVE DAY-OF-MONTH TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A010PL." TO VDU.
           MOVE PL TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010ZBAL." TO VDU.
           MOVE ZBAL TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010PRCOL." TO VDU.
           MOVE PRCOL-FG TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010SUMONLY." TO VDU.
           MOVE SUM-ONLY TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010GTT-ISO." TO VDU.
           MOVE GTT-ISO TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A170SPOOL-PROMPT." TO VDU.
           MOVE "SPOOL TO LOCAL  :"    TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A050YES-NO." TO VDU.
           MOVE "(Y/N)"          TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010LOCAL-SPOOL." TO VDU.
           MOVE LOCAL-PRINT-FG        TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
      *SORT-ROUTINE SECTION.
      ******************************************************************
      *    MOVE BR-NO               TO TB-BRNO
      *                                QTRB1-WBEG-BRNO
      *                                QTRB1-WEND-BRNO.
      *    MOVE BEGINNING-DATE      TO TB-DATE SQL-DATE-YYYYMMDD.
      *    PERFORM SQL-SET-DATE.
      *    MOVE SQL-DATE-YYYY-MM-DD TO QTRB1-WBEG-DATE.
      *    MOVE ENDING-DATE         TO  SQL-DATE-YYYYMMDD.
      *    PERFORM SQL-SET-DATE.
      *    MOVE SQL-DATE-YYYY-MM-DD TO QTRB1-WEND-DATE.
      *    MOVE BEG-CLASS           TO TB-CLASS
      *                                QTRB1-WBEG-CLASS.
      *    MOVE END-CLASS           TO QTRB1-WEND-CLASS.
      *    MOVE 0                   TO TB-ACCTNO TB-LAWCODE TB-LINE
      *                                QTRB1-WBEG-ACCTNO
      *                                QTRB1-WBEG-LAWCODE
      *                                QTRB1-WBEG-LINE.
      *    MOVE ALL "9"             TO QTRB1-WEND-ACCTNO
      *                                QTRB1-WEND-LAWCODE
      *                                QTRB1-WEND-LINE.
      *    PERFORM START-TRB1-FILE.

      *-----------------------------------------------------------------
      *SORT-TR-NEXT.
      *    PERFORM READ-TRB1-FILE-NEXT.
      *    IF (IO-FG NOT = 0) OR (TRB-PATH-OWNBR NOT = TB-BRNO)
      *       GO TO EXIT-SORT-ROUTINE.

      *    IF TB-DATE > ENDING-DATE
      *       GO TO EXIT-SORT-ROUTINE.

      *    IF TB-CLASS < BEG-CLASS OR TB-CLASS > END-CLASS
      *       GO TO SORT-TR-NEXT.

      *----------------------------------------------------------------
      *  TB-REF-FRTO -->  T : TRANSFERRED TO ANOTHER BRANCH (SOLD).
      *                   F : TRANSFERRED FROM ANOTHER BRANCH (BOUGHT).
      *----------------------------------------------------------------
      *    IF REPORT-TYPE = "1"
      *       IF TB-REF-FRTO NOT = "F"
      *          GO TO SORT-TR-NEXT
      *       END-IF
      *    ELSE
      *       IF REPORT-TYPE = "2"
      *          IF TB-REF-FRTO NOT = "T"
      *             GO TO SORT-TR-NEXT.

      *    -------------------------------------------------------------
      *    TEST BOUGHT FROM/SOLD TO BRANCH
      *    -------------------------------------------------------------
      *    IF FROM-TO-BRNO-ALL = "N"
      *       IF TB-REFBR NOT = FROM-TO-BRNO
      *          GO TO SORT-TR-NEXT.

      *    IF PL = "O"
      *       IF TB-PLCD = " "
      *          GO TO SORT-TR-NEXT.
      *    IF PL = "S"
      *       IF TB-PLCD NOT = " "
      *          GO TO SORT-TR-NEXT.

      *    IF ZBAL = "O"
      *       IF TB-PURBAL NOT = 0
      *          GO TO SORT-TR-NEXT.
      *    IF ZBAL = "S"
      *       IF TB-PURBAL = 0
      *          GO TO SORT-TR-NEXT.

      *    MOVE TB-DATE    TO WK2-DATE.
      *    MOVE TB-BRNO    TO WK2-BRANCH.
      *    MOVE TB-CLASS   TO WK2-CLASS.
      *    MOVE TB-ACCTNO  TO WK2-ACCTNO.
      *    MOVE TB-LAWCODE TO WK2-LAWCODE.
      *    MOVE TB-LINE    TO WK2-LINE.
      *    MOVE TB-REFBR   TO WK2-REFBR.

      *    PERFORM WRITE-WK2-FILE.
      *    IF IO-FG NOT = 0
      *       GO TO IO-ERROR.

      *    GO TO SORT-TR-NEXT.

      *EXIT-SORT-ROUTINE.
      *    PERFORM CLOSE-WK2-FILE.

      ******************************************************************
       TR-DETAIL1 SECTION.
      ******************************************************************
           IF FIRST-TIME = "Y"
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE 1 TO LN-FEED
              MOVE "Y" TO RECORDS-FOUND
              MOVE "N" TO FIRST-TIME.

           MOVE TB-CLASS TO D-CLASS.
           MOVE "-" TO D-H1.
           MOVE TB-ACCTNO TO D-NUMBER.
           MOVE "-" TO D-H2.
           MOVE TB-LAWCODE TO D-LAWCODE.
           MOVE TB-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-LNDTE.
           MOVE TB-DLNO TO D-DLNO.

           MOVE TB-BRNO  TO LN-OWNBR.
           MOVE TB-ACCTNO TO LN-ACCTNO.
      *    PERFORM READ-LN1-FILE.
      *    IF IO-FG NOT = 0
      *       MOVE 0 TO LN-APRATE.

           COMPUTE D-APR ROUNDED = LN-APRATE * 1.
           MOVE TB-SRCD TO D-SRCD.
           MOVE TB-COLLCD TO D-COLCD.
           MOVE TB-ORGTERM TO D-TERM.
           MOVE TB-LNAMT TO D-LNAMT.
           MOVE TB-FINCHG TO D-FINCHG.

           MOVE 0 TO T-RSVPRIN
                     T-RSVCHRG.

           PERFORM VARYING TSUB FROM 1 BY 1 UNTIL TSUB > 3
                ADD TB-DLRVPRIN(TSUB) TO T-RSVPRIN
                ADD TB-DLRVCHGS(TSUB) TO T-RSVCHRG
           END-PERFORM.

           MOVE T-RSVPRIN TO D-RSV1.
           MOVE TB-DLPARVHELD TO D-RSV2.
           MOVE TB-PROCEEDS TO D-PROCEEDS.
           MOVE 2 TO LN-FEED.
           IF SUM-ONLY = "N"
              PERFORM WRITE-DETAIL-LINE.

      *LINE2
           MOVE TB-MAKERCD(1) TO D-MAKERCD.
           MOVE TB-SSNO(1)    TO BW-SSNO.
           MOVE QWS-BW-OWNBR1 TO BW-OWNBR.
           MOVE QWS-BW-SSNO1  TO BW-SSNO.
           MOVE QWS-BW-LNAME1 TO BW-LNAME.
           MOVE QWS-BW-FNAME1 TO BW-FNAME.
           MOVE QWS-BW-ADR11  TO BW-ADR1.
           MOVE QWS-BW-ADR21  TO BW-ADR2.
           MOVE QWS-BW-CITY1  TO BW-CITY.
           MOVE QWS-BW-STATE1 TO BW-STATE.
           MOVE QWS-BW-ZIP1   TO BW-ZIP.
           MOVE "Y"           TO FIRST-BORROWER-SW.
           PERFORM MOVE-BORROWER.
           MOVE "N"           TO FIRST-BORROWER-SW.
           MOVE TB-PURCD      TO D-COLCD.

           IF TB-PURBAL NOT = TB-LNAMT
              MOVE TB-PURBAL TO D-LNAMT.

           ADD 1 TO I-SUB.

           MOVE T-RSVCHRG TO D-RSV1.
           MOVE TB-DLPARVPAID TO D-RSV2.


           IF GP-INCL-OTH-IN-EARNED-N
              COMPUTE D-PROCEEDS = TB-UNEARN(1) + TB-UNEARN(2) +
                                   TB-UNEARN(3) + TB-UNEARN(4) +
                                   TB-UNEARN(5)
           ELSE
           IF GP-INCL-OTH-IN-EARNED-R
              COMPUTE D-PROCEEDS = TB-UNEARN(1) + TB-UNEARN(2) +
                                   TB-UNEARN(3) + TB-UNEARN(5)
           ELSE

              COMPUTE D-PROCEEDS = TB-UNEARN(1) + TB-UNEARN(2) +
                                   TB-UNEARN(3) + TB-UNEARN(4) +
                                   TB-UNEARN(5) + TB-UNEARN(6).

           IF DETAIL-LINE NOT = SPACES
            IF SUM-ONLY = "N"
              PERFORM WRITE-DETAIL-LINE.

      *LINE3
           MOVE BW-CITY TO C-S-Z.
           MOVE BW-STATE TO STATE.
           MOVE BW-ZIP TO ZIP.
           PERFORM CAT-CSZ.

           IF BW-ADR2 NOT = SPACES
              STRING BW-ADR1,", ",BW-ADR2 DELIMITED BY "   "
                 INTO D-BW-ADDR
           ELSE
              MOVE BW-ADR1 TO D-BW-ADDR.

           STRING D-BW-ADDR,"   ",C-S-Z DELIMITED BY "     "
               INTO D-BW-ADDR.


           ADD 1 TO I-SUB.

           MOVE TB-DLDISC TO D-RSV1.
           MOVE TB-REFBR TO D-FROM-BRNO.
           MOVE TB-POSTDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-TRANS-DATE.

           IF DETAIL-LINE NOT = SPACES
            IF SUM-ONLY = "N"
              PERFORM WRITE-DETAIL-LINE.

      *LINE4
           MOVE TB-MAKERCD(2) TO D-MAKERCD.
           MOVE TB-SSNO(2)    TO BW-SSNO.
           MOVE QWS-BW-OWNBR2 TO BW-OWNBR.
           MOVE QWS-BW-SSNO2  TO BW-SSNO.
           MOVE QWS-BW-LNAME2 TO BW-LNAME.
           MOVE QWS-BW-FNAME2 TO BW-FNAME.
           MOVE QWS-BW-ADR12  TO BW-ADR1.
           MOVE QWS-BW-ADR22  TO BW-ADR2.
           MOVE QWS-BW-CITY2  TO BW-CITY.
           MOVE QWS-BW-STATE2 TO BW-STATE.
           MOVE QWS-BW-ZIP2   TO BW-ZIP.
           PERFORM MOVE-BORROWER.
           ADD 1              TO I-SUB.

           IF DETAIL-LINE NOT = SPACES
            IF SUM-ONLY = "N"
              PERFORM WRITE-DETAIL-LINE.

      *LINE5
           MOVE TB-MAKERCD(3) TO D-MAKERCD.
      *    MOVE TB-SSNO(3)    TO BW-SSNO.
           MOVE 0             TO BW-SSNO.
           PERFORM MOVE-BORROWER.
           ADD 1              TO I-SUB.

           IF DETAIL-LINE NOT = SPACES
            IF SUM-ONLY = "N"
              PERFORM WRITE-DETAIL-LINE.

      *LINE6
           MOVE TB-MAKERCD(4) TO D-MAKERCD.
      *    MOVE TB-SSNO(4)    TO BW-SSNO.
           MOVE 0             TO BW-SSNO.
           PERFORM MOVE-BORROWER.
           ADD 1              TO I-SUB.

           IF DETAIL-LINE NOT = SPACES
            IF SUM-ONLY = "N"
              PERFORM WRITE-DETAIL-LINE.

           IF SUM-ONLY = "Y"
              MOVE SPACES TO DETAIL-LINE.

           PERFORM PRINT-LT-REC-COL.

       END-TR1-DETAIL.
           EXIT.

      ******************************************************************
       CREATE-WK SECTION.
      ******************************************************************
           MOVE TB-SRCD TO SAVE-WK-CODE.
           MOVE TB-BRNO TO SAVE-WK-BR.
           MOVE TB-CLASS TO SAVE-WK-CL.

      *-----------------------------------------------------------------
       SAVE-WORK-KEY.
           MOVE SAVE-WK-KEY TO WK-KEY.
           PERFORM READ-WK-FILE.

           IF IO-FG = 0
              GO TO CREATE-WK-UPDATE.

           MOVE " " TO WK-REC.
           MOVE SAVE-WK-KEY TO WK-KEY.

           MOVE 0 TO WK-NOLOANS WK-LNAMT WK-INTCHG WK-SERCHG
                     WK-EXTCHG WK-MFEE WK-EARNINGS WK-FINCHG
                     WK-DL WK-LL WK-AH WK-PP WK-O1 WK-O2 WK-O3
                     WK-FEES
                     WK-DLRVPRIN(1) WK-DLRVPRIN(2) WK-DLRVPRIN(3)
                     WK-DLRVCHGS(1) WK-DLRVCHGS(2) WK-DLRVCHGS(3)
                     WK-DLPARVHELD WK-DLPARVPAID WK-DLDISC
                     WK-PROCEEDS WK-PURBAL WK-ANTICERN.

           PERFORM WRITE-WK-FILE.

           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO SAVE-WORK-KEY.

      *-----------------------------------------------------------------
       CREATE-WK-UPDATE.
           IF TB-LNAMT < 0
              SUBTRACT 1 FROM WK-NOLOANS
           ELSE
              ADD 1 TO WK-NOLOANS.

           ADD TB-LNAMT TO WK-LNAMT.
           ADD TB-FINCHG TO WK-FINCHG.
           ADD TB-INTCHG TO WK-INTCHG.
           ADD TB-SERCHG TO WK-SERCHG.
           ADD TB-EXTCHG TO WK-EXTCHG.

           COMPUTE WK-MFEE ROUNDED = WK-MFEE
                                   + (TB-MAINTFEE * TB-ORGTERM).

           COMPUTE WK-EARNINGS = WK-EARNINGS +
                                 TB-UNEARN(1) + TB-UNEARN(2) +
                                 TB-UNEARN(3) + TB-UNEARN(5).

           IF NOT (GP-INCL-OTH-IN-EARNED-N OR GP-INCL-OTH-IN-EARNED-R)
              ADD TB-UNEARN(6) TO WK-EARNINGS.

           IF NOT (GP-INCL-OTH-IN-EARNED-R)
              ADD TB-UNEARN(4) TO WK-EARNINGS.

           ADD TB-DLRVPRIN(1) TO WK-DLRVPRIN(1).
           ADD TB-DLRVPRIN(2) TO WK-DLRVPRIN(2).
           ADD TB-DLRVPRIN(3) TO WK-DLRVPRIN(3).
           ADD TB-DLRVCHGS(1) TO WK-DLRVCHGS(1).
           ADD TB-DLRVCHGS(2) TO WK-DLRVCHGS(2).
           ADD TB-DLRVCHGS(3) TO WK-DLRVCHGS(3).
           ADD TB-DLPARVHELD TO WK-DLPARVHELD.
           ADD TB-DLPARVPAID TO WK-DLPARVPAID.
           ADD TB-DLDISC TO WK-DLDISC.
           ADD TB-PROCEEDS TO WK-PROCEEDS.
           ADD TB-PURBAL TO WK-PURBAL.


           PERFORM REWRITE-WK-FILE.

       EXIT-CREATE-WK.
           EXIT.

      ******************************************************************
       MOVE-BORROWER SECTION.
      ******************************************************************

           IF BW-SSNO = 0 AND
              FIRST-BORROWER-SW = "Y"
              GO TO CONTINUE-MOVE-BORROWER.

           IF BW-SSNO = 0
              GO TO EXIT-MOVE-BORROWER.

       CONTINUE-MOVE-BORROWER.

           MOVE SPACES TO D-BW-SSNO.

      *    PERFORM READ-BW1-FILE.

      *    IF IO-FG = 9
           IF BW-LNAME = SPACES
              MOVE "* * NOT FOUND * *" TO BW-LNAME
              MOVE " " TO BW-FNAME.

           MOVE SPACES TO D-BW-NAME.
           STRING BW-LNAME," ",BW-FNAME DELIMITED BY "   "
                 INTO D-BW-NAME.

       EXIT-MOVE-BORROWER.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ******************************************************************
       SET-CLASS-HEADINGS SECTION.
      ******************************************************************
           MOVE HOLD-CLASS TO H-LCNO LC-CODE.
           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE. *> EMBEDDED - LCFILE

           IF IO-FG NOT = 0
              MOVE "INVALID" TO LC-DESC.

           PERFORM CLOSE-LC1-FILE.
           MOVE LC-DESC TO H-LCDESC.
           MOVE "N" TO SUMMARY-FG.

      ******************************************************************
       CLASS-TOTALS SECTION.
      ******************************************************************
           IF ( GTT-ISO = "O" )
              GO TO END-CLASS-TOTALS.

           MOVE "Y" TO SUMMARY-FG.
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
           PERFORM WRITE-DETAIL-LINE.
           MOVE HEAD8 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE HEAD9 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           IF GP-DLR-DISP-TYPE = "C"
              MOVE "COMM" TO HEAD10-DLR-TYP
           ELSE
              MOVE "DISC" TO HEAD10-DLR-TYP.
           MOVE HEAD10 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE HOLD-BRNO TO WK-BR.
           MOVE HOLD-CLASS TO WK-CLASS.
           MOVE " " TO WK-CODE.
           PERFORM START-WK-FILE.

      *-----------------------------------------------------------------
       PRINT-NEXT-CODE.
           PERFORM READ-WK-FILE-NEXT.

           IF IO-FG = 9
              PERFORM TYPE-TOTALS
              GO TO END-CLASS-TOTALS.

           IF WK-CLASS NOT = HOLD-CLASS
              PERFORM TYPE-TOTALS
              GO TO END-CLASS-TOTALS.

           IF WK-BR NOT = HOLD-BRNO
              PERFORM TYPE-TOTALS
              GO TO END-CLASS-TOTALS.

           MOVE WK-CODE TO S-DCODE.
           MOVE ":" TO S-COLON.
           MOVE WK-NOLOANS TO S-NOLOANS.
           MOVE WK-LNAMT TO S-LNAMT.
           MOVE WK-FINCHG TO S-FINCHG.
           MOVE WK-INTCHG TO S-INTCHG.
           MOVE WK-EXTCHG TO S-EXTCHG.
           MOVE WK-DL TO S-CL.
           MOVE WK-AH TO S-AH.
           MOVE WK-O1 TO S-OT.
           MOVE WK-FEES TO S-FEEAMT.

           MOVE 0 TO T-WK-DLRVPRIN
                     T-WK-DLRVCHGS.

           PERFORM VARYING TSUB FROM 1 BY 1
                                UNTIL TSUB > 3
              ADD WK-DLRVPRIN (TSUB) TO T-WK-DLRVPRIN
              ADD WK-DLRVCHGS (TSUB) TO T-WK-DLRVCHGS
           END-PERFORM.

           MOVE T-WK-DLRVPRIN TO S-RSV1.
           MOVE WK-DLPARVHELD TO S-RSV2.
           MOVE WK-EARNINGS TO S-EARNINGS.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF WK-PURBAL NOT = WK-LNAMT
              MOVE WK-PURBAL TO S-LNAMT.

           IF WK-ANTICERN NOT = WK-FINCHG
              MOVE WK-ANTICERN TO S-FINCHG.

           MOVE WK-SERCHG TO S-INTCHG.
           MOVE WK-MFEE TO S-EXTCHG.
           MOVE WK-LL TO S-CL.
           MOVE WK-PP TO S-AH.
           MOVE WK-O2 TO S-OT.
           MOVE T-WK-DLRVCHGS TO S-RSV1.
           MOVE WK-DLPARVPAID TO S-RSV2.
           MOVE WK-PROCEEDS TO S-EARNINGS.
           PERFORM WRITE-DETAIL-LINE.

           MOVE WK-DLDISC TO S-RSV1.
           MOVE WK-O3 TO S-OT.

           IF DETAIL-LINE NOT = SPACES
                PERFORM WRITE-DETAIL-LINE.

           GO TO PRINT-NEXT-CODE.

      *-----------------------------------------------------------------
       END-CLASS-TOTALS.
           MOVE 2 TO LEV.
           MOVE 1 TO SUB.
           ADD T-NOLOANS(1) TO T-NOLOANS(2).
           PERFORM UPDATE-TABLE2.

      * SUB MUST = 1

           PERFORM CLEAR-TOTAL-TBL.
           MOVE "N" TO SUMMARY-FG.

      ******************************************************************
       BRANCH-TOTALS SECTION.
      ******************************************************************
           IF ( GTT-ISO = "O" )
              GO TO END-BRANCH-TOTALS.

           MOVE "Y" TO SUMMARY-FG.
           MOVE 0 TO HOLD-CLASS.
           MOVE 2 TO LEV.
           MOVE "BRANCH TOTALS:" TO S-TOTNAME.
           PERFORM PRINT-SUMMARY.

      *-----------------------------------------------------------------
       END-BRANCH-TOTALS.
           MOVE 3 TO LEV.
           MOVE 2 TO SUB.
           ADD T-NOLOANS(2) TO T-NOLOANS(3).
           PERFORM UPDATE-TABLE2.

      * SUB MUST = 2

           PERFORM CLEAR-TOTAL-TBL.
           MOVE "N" TO SUMMARY-FG.

      ******************************************************************
       GRAND-TOTALS SECTION.
      ******************************************************************
           IF ( GTT-ISO = "O" )
              GO TO END-GRAND-TOTALS.

           MOVE "Y" TO SUMMARY-FG.
           MOVE 3 TO LEV.
           MOVE "GRAND  TOTALS:" TO S-TOTNAME.
           PERFORM PRINT-SUMMARY.

       END-GRAND-TOTALS.
           MOVE "Y" TO SUMMARY-FG.

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE TIME-EDIT TO H-TIME.
           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           IF REPORT-TYPE = "1"
              MOVE "       LOANS BOUGHT         " TO H-RPT-TYP
           ELSE
              MOVE "        LOANS SOLD          " TO H-RPT-TYP.
           MOVE HEAD3 TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE HEAD4 TO PR-REC.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( GTT-HEADER-FG = "Y" )
              MOVE GTT-HEAD1 TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC
              MOVE GTT-HEAD2 TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC
              MOVE GTT-HEAD3 TO PR-REC
              MOVE 1 TO LN-FEED
              PERFORM WRITE-PR-REC
              MOVE GTT-HEAD4 TO PR-REC
              MOVE 1 TO LN-FEED
              PERFORM WRITE-PR-REC
              MOVE ALL "-" TO PR-REC
              PERFORM WRITE-PR-REC
              MOVE 5 TO LN-COUNT
              MOVE 1 TO LN-FEED
           ELSE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF SUMMARY-FG = "N"
              MOVE HEAD5 TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC
              MOVE HEAD6 TO PR-REC
              MOVE 1 TO LN-FEED
              PERFORM WRITE-PR-REC
              IF GP-DLR-DISP-TYPE = "C"
                 MOVE "COMM" TO HEAD7-DLR-TYP
              ELSE
                 MOVE "DISC" TO HEAD7-DLR-TYP
              END-IF
              IF REPORT-TYPE = 1
                 MOVE "FROM BR" TO HEAD7-FR-TO
              ELSE
                 MOVE "TO BRNO" TO HEAD7-FR-TO
              END-IF
              MOVE HEAD7 TO PR-REC
              MOVE 1 TO LN-FEED
              PERFORM WRITE-PR-REC
              MOVE 10 TO LN-COUNT
              MOVE 2 TO LN-FEED
           ELSE
              MOVE 5 TO LN-COUNT
              MOVE 2 TO LN-FEED.

       END-HEAD-PAGE.
           EXIT.

      ******************************************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************************************

           MOVE 4 TO LN-FEED.
           IF GROUP-FLAG = "Y"
              MOVE "BRANCH GROUP" TO RANGE-LINE
              MOVE ENT-GROUP TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEG BRANCH" TO RANGE-LINE
              MOVE BEG-BRANCH TO R-NUM
              PERFORM WRITE-DETAIL-LINE
              MOVE "END BRANCH" TO RANGE-LINE
              MOVE END-BRANCH TO R-NUM
              PERFORM WRITE-DETAIL-LINE.

           MOVE "BEG CLASS" TO RANGE-LINE.
           MOVE BEG-CLASS TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "END CLASS" TO RANGE-LINE.
           MOVE END-CLASS TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.

           IF REPORT-TYPE = 1
               MOVE "REPORT TYPE            LOANS BOUGHT"
               TO RANGE-LINE
           ELSE
               MOVE  "REPORT TYPE           LOANS SOLD"
               TO RANGE-LINE.
           PERFORM WRITE-DETAIL-LINE.


           MOVE "BRANCH" TO RANGE-LINE.
           IF FROM-TO-BRNO-ALL NOT EQUAL TO "Y"
               MOVE FROM-TO-BRNO TO R-NUM
               PERFORM WRITE-DETAIL-LINE
           ELSE
               MOVE "******" TO R-CHAR
               PERFORM WRITE-DETAIL-LINE.

           MOVE "MONTH-END DATE" TO RANGE-LINE.
           MOVE EDATE TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "DAY OF MONTH" TO RANGE-LINE.
           MOVE DAY-OF-MONTH TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "P&L ACCOUNTS" TO RANGE-LINE.
           MOVE PL TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ZERO BAL ACCOUNTS" TO RANGE-LINE.
           MOVE ZBAL TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "PRINT COLLATERAL" TO RANGE-LINE.
           MOVE PRCOL-FG TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "GROUP TOTALS" TO RANGE-LINE.
           MOVE GTT-ISO TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "SUMMARY ONLY" TO RANGE-LINE.
           MOVE SUM-ONLY TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.

      ***************************
       CLASS-SCAN SECTION.
      ***************************
           MOVE 0      TO LCSCAN-KEY.
           MOVE SPACES TO LCSCAN-VERIFY.
           MOVE "WI/LCSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH LCSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF LCSCAN-STATUS NOT = "00" OR LCSCAN-KEY = 0
               MOVE 9 TO IO-FG
               GO TO EXIT-CLASS-SCAN.
           IF ( CLASS-FG = "BEG" )
              MOVE "SNNN020CL1." TO VDU
           ELSE
              MOVE "SNNN020CL2." TO VDU.
           MOVE LCSCAN-KEY TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE LCSCAN-KEY TO VDUNUM0.
           MOVE 0 TO IO-FG.

       EXIT-CLASS-SCAN.
           EXIT.

      ******************************************************************
       GTT-ADD-TOTALS SECTION.
      ******************************************************************

           ADD WK-LNAMT       TO  WS-GTT-LNAMT     ( GTT-TOTAL-SUB ).
           ADD WK-FINCHG      TO  WS-GTT-FINCHG    ( GTT-TOTAL-SUB ).
           ADD WK-INTCHG      TO  WS-GTT-INTCHG    ( GTT-TOTAL-SUB ).
           ADD WK-SERCHG      TO  WS-GTT-SERCHG    ( GTT-TOTAL-SUB ).
           ADD WK-EXTCHG      TO  WS-GTT-EXTCHG    ( GTT-TOTAL-SUB ).
           ADD WK-MFEE        TO  WS-GTT-MFEE      ( GTT-TOTAL-SUB ).
           ADD WK-EARNINGS    TO  WS-GTT-EARNINGS  ( GTT-TOTAL-SUB ).
           ADD WK-LL          TO  WS-GTT-LL        ( GTT-TOTAL-SUB ).
           ADD WK-DL          TO  WS-GTT-DL        ( GTT-TOTAL-SUB ).
           ADD WK-AH          TO  WS-GTT-AH        ( GTT-TOTAL-SUB ).
           ADD WK-PP          TO  WS-GTT-PP        ( GTT-TOTAL-SUB ).
           ADD WK-O1          TO  WS-GTT-O1        ( GTT-TOTAL-SUB ).
           ADD WK-O2          TO  WS-GTT-O2        ( GTT-TOTAL-SUB ).
           ADD WK-O3          TO  WS-GTT-O3        ( GTT-TOTAL-SUB ).
           ADD WK-FEES        TO  WS-GTT-FEES      ( GTT-TOTAL-SUB ).
           ADD WK-DLRVPRIN(1) TO  WS-GTT-DLRVPRIN  ( GTT-TOTAL-SUB ).
           ADD WK-DLRVPRIN(2) TO  WS-GTT-DLRVPRIN  ( GTT-TOTAL-SUB ).
           ADD WK-DLRVPRIN(3) TO  WS-GTT-DLRVPRIN  ( GTT-TOTAL-SUB ).
           ADD WK-DLRVCHGS(1) TO  WS-GTT-DLRVCHGS  ( GTT-TOTAL-SUB ).
           ADD WK-DLRVCHGS(2) TO  WS-GTT-DLRVCHGS  ( GTT-TOTAL-SUB ).
           ADD WK-DLRVCHGS(3) TO  WS-GTT-DLRVCHGS  ( GTT-TOTAL-SUB ).
           ADD WK-DLPARVHELD  TO  WS-GTT-DLPARVHELD( GTT-TOTAL-SUB ).
           ADD WK-DLPARVPAID  TO  WS-GTT-DLPARVPAID( GTT-TOTAL-SUB ).
           ADD WK-DLDISC      TO  WS-GTT-DLDISC    ( GTT-TOTAL-SUB ).
           ADD WK-PROCEEDS    TO  WS-GTT-PROCEEDS  ( GTT-TOTAL-SUB ).
           ADD WK-PURBAL      TO  WS-GTT-PURBAL    ( GTT-TOTAL-SUB ).
           ADD WK-ANTICERN    TO  WS-GTT-ANTICERN  ( GTT-TOTAL-SUB ).

       EXIT-GTT-ADD-TOTALS.
           EXIT.

      ******************************************************************
       GTT-COMPILE-GROUP-TOTALS SECTION.
      ******************************************************************
           MOVE SPACES      TO WK-KEY.
           MOVE SPACES      TO WK-CODE.
           MOVE WGR-SRESULT TO WK-BR.
           MOVE ZEROES      TO WK-CLASS.
           PERFORM START-WK-FILE.

       GTT-NEXT-WK1.

           PERFORM READ-WK-FILE-NEXT.

           IF ( IO-FG   NOT = 0           ) OR
              ( WK-BR NOT = WGR-SRESULT )
                 GO TO EXIT-GTT-COMPILE-GROUP-TOTALS.

           MOVE "N" TO GTT-FIRST-TIME.

       GTT-OK-TO-PROCESS.

           PERFORM GTT-CHECK-ADD-TOTALS.
           GO TO GTT-NEXT-WK1.

       EXIT-GTT-COMPILE-GROUP-TOTALS.
           EXIT.

      ******************************************************************
       GTT-PRINT-GROUP-BR-TOTALS SECTION.
      ******************************************************************

           MOVE WGR-SID(GTT-PREV-SUB) TO WGR-WORD.
           MOVE WGR-NUMB              TO BR-NO.
           PERFORM READ-BR-FILE.
           IF ( IO-FG NOT = 0 )
              MOVE "BRANCH #&&&& NOT ON FILE" TO BR-NAME
              INSPECT BR-NAME REPLACING FIRST "&&&&" BY BR-NO.

           MOVE SPACES    TO DETAIL-LINE.
           MOVE 9         TO GTT-TOTAL-SUB.
           MOVE "BRANCH:" TO D2-PROMPT1 WS-HOLD-PROMPT.
           MOVE BR-NO     TO D2-BRNO.

           PERFORM GTT-PRINT-TOTALS.

       EXIT-GTT-PRINT-GROUP-BR-TOTALS.
           EXIT.

      ******************************************************************
       GTT-PRINT-GROUP-TOTALS SECTION.
      ******************************************************************

           MOVE SPACES    TO DETAIL-LINE.
           MOVE "GROUP :" TO D2-PROMPT1 WS-HOLD-PROMPT.
           MOVE GR-ID     TO D2-GRNO.
           MOVE GR-LEVEL  TO D2-GR-LEVEL.

           PERFORM GTT-PRINT-TOTALS.

       EXIT-GTT-PRINT-GROUP-TOTALS.
           EXIT.

      ******************************************************************
       GTT-PRINT-TOTALS SECTION.
      ******************************************************************
           MOVE WS-GTT-NOLOANS   (GTT-TOTAL-SUB) TO S-NOLOANS.
           MOVE WS-GTT-LNAMT     (GTT-TOTAL-SUB) TO S-LNAMT.
           MOVE WS-GTT-FINCHG    (GTT-TOTAL-SUB) TO S-FINCHG.
           MOVE WS-GTT-INTCHG    (GTT-TOTAL-SUB) TO S-INTCHG.
           MOVE WS-GTT-EXTCHG    (GTT-TOTAL-SUB) TO S-EXTCHG.
           MOVE WS-GTT-DL        (GTT-TOTAL-SUB) TO S-CL.
           MOVE WS-GTT-AH        (GTT-TOTAL-SUB) TO S-AH.
           MOVE WS-GTT-O1        (GTT-TOTAL-SUB) TO S-OT.
           MOVE WS-GTT-FEES      (GTT-TOTAL-SUB) TO S-FEEAMT.

           MOVE WS-GTT-DLRVPRIN (GTT-TOTAL-SUB) TO S-RSV1.
           MOVE WS-GTT-DLPARVHELD (GTT-TOTAL-SUB) TO S-RSV2.
           MOVE WS-GTT-EARNINGS (GTT-TOTAL-SUB) TO S-EARNINGS.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF WS-GTT-PURBAL(GTT-TOTAL-SUB)
                        NOT = WS-GTT-LNAMT (GTT-TOTAL-SUB)
              MOVE WS-GTT-PURBAL (GTT-TOTAL-SUB) TO S-LNAMT.

           IF WS-GTT-ANTICERN(GTT-TOTAL-SUB)
                        NOT = WS-GTT-FINCHG (GTT-TOTAL-SUB)
              MOVE WS-GTT-ANTICERN (GTT-TOTAL-SUB) TO S-FINCHG.

           MOVE WS-GTT-SERCHG (GTT-TOTAL-SUB) TO S-INTCHG.
           MOVE WS-GTT-MFEE (GTT-TOTAL-SUB) TO S-EXTCHG.
           MOVE WS-GTT-LL (GTT-TOTAL-SUB) TO S-CL.
           MOVE WS-GTT-PP (GTT-TOTAL-SUB) TO S-AH.
           MOVE WS-GTT-O2 (GTT-TOTAL-SUB) TO S-OT.
           MOVE WS-GTT-DLRVCHGS (GTT-TOTAL-SUB) TO S-RSV1.
           MOVE WS-GTT-DLPARVPAID (GTT-TOTAL-SUB) TO S-RSV2.
           MOVE WS-GTT-PROCEEDS (GTT-TOTAL-SUB) TO S-EARNINGS.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE WS-GTT-DLDISC (GTT-TOTAL-SUB) TO S-RSV1.
           MOVE WS-GTT-O3 (GTT-TOTAL-SUB) TO S-OT.

           IF DETAIL-LINE NOT= SPACES
               PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - PRINT DETAIL AFTER LOAD - - -
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( WS-HOLD-PROMPT = "GROUP :" )
              MOVE ALL "-" TO DETAIL-LINE
              PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - CLEAR LEVEL IN TABLE  - - -
           MOVE LOW-VALUES TO WS-GTT-TABLE-GROUP-LEVEL(GTT-TOTAL-SUB).

       EXIT-GTT-PRINT-TOTALS.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/EOSPOOL.CPY".
       COPY "LIBGB/GRTOTALS.CPY".
       COPY "LIBWI/GRTOTL.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO EMTBRG-CONAME.
           MOVE EXT-CONAME-SYSDATE TO EMTBRG-SYSDATE.
           DISPLAY EMTBRG-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE EMTBRG-FORM-FIELDS TO WR-BUF. 
           MOVE EMTBRG-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE EMTBRG-HELP--FILE TO HELP-LINK-FILE.
           MOVE EMTBRG-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/EMTBRG_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARA SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       SET-BRANCH-HEADINGS.
           MOVE HOLD-BRNO TO H-BRNO.
           MOVE BR-NAME TO H-BRDESC.
      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       CLEAR-TOTAL-TBL.
           MOVE 0 TO T-NOLOANS(SUB) T-LNAMT(SUB) T-INTCHG(SUB)
                     T-SERCHG(SUB) T-EXTCHG(SUB) T-MFEE(SUB)
                     T-EARNINGS(SUB) T-DL(SUB) T-LL(SUB) T-AH(SUB)
                     T-PP(SUB) T-O1(SUB) T-O2(SUB) T-O3(SUB)
                     T-FEES(SUB)
                     T-DLRVPRIN(SUB 1) T-DLRVPRIN(SUB 2)
                     T-DLRVPRIN(SUB 3) T-DLRVCHGS(SUB 1)
                     T-DLRVCHGS(SUB 2) T-DLRVCHGS(SUB 3)
                     T-DLDISC(SUB) T-DLPARVPAID(SUB)
                     T-DLPARVHELD(SUB) T-PURBAL(SUB) T-FINCHG(SUB)
                     T-PROCEEDS(SUB) T-ANTICERN(SUB).

      *-----------------------------------------------------------------
       CAT-CSZ.
           INSPECT C-S-Z REPLACING FIRST "      " BY ", ++ +".
           INSPECT C-S-Z REPLACING FIRST "++" BY STATE.
           INSPECT C-S-Z REPLACING FIRST "+         " BY ZIP.

      *-----------------------------------------------------------------
       UPDATE-TABLE.
           IF TB-LNAMT < 0
              SUBTRACT 1 FROM T-NOLOANS(1)
           ELSE
              ADD 1 TO T-NOLOANS(1).

           ADD TB-LNAMT TO T-LNAMT(LEV).
           ADD TB-FINCHG TO T-FINCHG(LEV).
           ADD TB-INTCHG TO T-INTCHG(LEV).
           ADD TB-SERCHG TO T-SERCHG(LEV).
           ADD TB-EXTCHG TO T-EXTCHG(LEV).

           COMPUTE T-MFEE(LEV) ROUNDED = T-MFEE(LEV)
                                   + (TB-MAINTFEE * TB-ORGTERM).

           COMPUTE T-EARNINGS(LEV) = T-EARNINGS(LEV) +
                                     TB-UNEARN(1) + TB-UNEARN(2) +
                                     TB-UNEARN(3) + TB-UNEARN(5).

           IF NOT (GP-INCL-OTH-IN-EARNED-N OR GP-INCL-OTH-IN-EARNED-R)
              ADD TB-UNEARN(6) TO T-EARNINGS(LEV).

           IF NOT(GP-INCL-OTH-IN-EARNED-R)
              ADD TB-UNEARN(4) TO T-EARNINGS(LEV).

           ADD TB-DLRVPRIN(1) TO T-DLRVPRIN(LEV 1).
           ADD TB-DLRVPRIN(2) TO T-DLRVPRIN(LEV 2).
           ADD TB-DLRVPRIN(3) TO T-DLRVPRIN(LEV 3).
           ADD TB-DLRVCHGS(1) TO T-DLRVCHGS(LEV 1).
           ADD TB-DLRVCHGS(2) TO T-DLRVCHGS(LEV 2).
           ADD TB-DLRVCHGS(3) TO T-DLRVCHGS(LEV 3).
           ADD TB-DLDISC TO T-DLDISC(LEV).
           ADD TB-DLPARVHELD TO T-DLPARVHELD(LEV).
           ADD TB-DLPARVPAID TO T-DLPARVPAID(LEV).

           ADD TB-PROCEEDS TO T-PROCEEDS(LEV).
           ADD TB-PURBAL TO T-PURBAL(LEV).

      *-----------------------------------------------------------------
       UPDATE-TABLE2.
           ADD T-LNAMT(SUB) TO T-LNAMT(LEV).
           ADD T-FINCHG(SUB) TO T-FINCHG(LEV).
           ADD T-INTCHG(SUB) TO T-INTCHG(LEV).
           ADD T-SERCHG(SUB) TO T-SERCHG(LEV).
           ADD T-EXTCHG(SUB) TO T-EXTCHG(LEV).
           ADD T-MFEE(SUB) TO T-MFEE(LEV).
           ADD T-EARNINGS(SUB) TO T-EARNINGS(LEV).
           ADD T-LL(SUB) TO T-LL(LEV).
           ADD T-DL(SUB) TO T-DL(LEV).
           ADD T-AH(SUB) TO T-AH(LEV).
           ADD T-PP(SUB) TO T-PP(LEV).
           ADD T-O1(SUB) TO T-O1(LEV).
           ADD T-O2(SUB) TO T-O2(LEV).
           ADD T-O3(SUB) TO T-O3(LEV).
           ADD T-FEES(SUB) TO T-FEES(LEV).
           ADD T-DLRVPRIN(SUB 1) TO T-DLRVPRIN(LEV 1).
           ADD T-DLRVPRIN(SUB 2) TO T-DLRVPRIN(LEV 2).
           ADD T-DLRVPRIN(SUB 3) TO T-DLRVPRIN(LEV 3).
           ADD T-DLRVCHGS(SUB 1) TO T-DLRVCHGS(LEV 1).
           ADD T-DLRVCHGS(SUB 2) TO T-DLRVCHGS(LEV 2).
           ADD T-DLRVCHGS(SUB 3) TO T-DLRVCHGS(LEV 3).
           ADD T-DLPARVHELD(SUB) TO T-DLPARVHELD(LEV).
           ADD T-DLPARVPAID(SUB) TO T-DLPARVPAID(LEV).
           ADD T-DLDISC(SUB) TO T-DLDISC(LEV).
           ADD T-PROCEEDS(SUB) TO T-PROCEEDS(LEV).
           ADD T-PURBAL(SUB) TO T-PURBAL(LEV).
           ADD T-ANTICERN(SUB) TO T-ANTICERN(LEV).

      *-----------------------------------------------------------------
       TYPE-TOTALS.
           MOVE 1 TO LEV.
           MOVE "CLASS  TOTALS:" TO S-TOTNAME.
           PERFORM PRINT-SUMMARY.

      *-----------------------------------------------------------------
       PRINT-SUMMARY.
           MOVE T-NOLOANS(LEV) TO S-NOLOANS.
           MOVE T-LNAMT(LEV) TO S-LNAMT.
           MOVE T-FINCHG(LEV) TO S-FINCHG.
           MOVE T-INTCHG(LEV) TO S-INTCHG.
           MOVE T-EXTCHG(LEV) TO S-EXTCHG.
           MOVE T-DL(LEV) TO S-CL.
           MOVE T-AH(LEV) TO S-AH.
           MOVE T-O1(LEV) TO S-OT.
           MOVE T-FEES(LEV) TO S-FEEAMT.

           MOVE 0 TO TOT-DLRVCHGS
                     TOT-DLRVPRIN.

           PERFORM VARYING TSUB FROM 1 BY 1
                                UNTIL TSUB > 3
                ADD T-DLRVPRIN (LEV TSUB) TO TOT-DLRVPRIN
                ADD T-DLRVCHGS (LEV TSUB) TO TOT-DLRVCHGS
           END-PERFORM.
           MOVE TOT-DLRVPRIN TO S-RSV1.
           MOVE T-DLPARVHELD(LEV) TO S-RSV2.
           MOVE T-EARNINGS(LEV) TO S-EARNINGS.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF T-PURBAL(LEV) NOT = T-LNAMT(LEV)
              MOVE T-PURBAL(LEV) TO S-LNAMT.

           IF T-ANTICERN(LEV) NOT = T-FINCHG(LEV)
              MOVE T-ANTICERN(LEV) TO S-FINCHG.

           MOVE T-SERCHG(LEV) TO S-INTCHG.
           MOVE T-MFEE(LEV) TO S-EXTCHG.
           MOVE T-LL(LEV) TO S-CL.
           MOVE T-PP(LEV) TO S-AH.
           MOVE T-O2(LEV) TO S-OT.
           MOVE TOT-DLRVCHGS TO S-RSV1.
           MOVE T-DLPARVPAID (LEV) TO S-RSV2.
           MOVE T-PROCEEDS(LEV) TO S-EARNINGS.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE T-DLDISC(LEV) TO S-RSV1.
           MOVE T-O3(LEV) TO S-OT.

           IF DETAIL-LINE NOT= SPACES
               PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------------
      *
      * PRINT COLLATERAL FROM LT-FILE IF FLAG = YES.
      *
      *-----------------------------------------------------------------
       PRINT-LT-REC-COL.
           IF PRCOL-FG = "Y"
              MOVE LN-OWNBR  TO LTC-BRNO
              MOVE LN-ACCTNO TO LTC-ACCTNO
              MOVE 1         TO LTC-SEQNO
      *       PERFORM OPEN-LTC1-FILE
      *       PERFORM READ-LTC1-FILE
      *       PERFORM CLOSE-LTC1-FILE
      *       IF IO-FG = 0 AND SUM-ONLY = "N"
              IF (SUM-ONLY            = "N")   AND
                 (LTC-MAKE(1)     NOT = SPACES OR
                  LTC-MODEL(1)    NOT = SPACES OR
                  LTC-YEAR(1)     NOT = SPACES OR
                  LTC-VIN(1)      NOT = SPACES)
                 MOVE "MAKE : "      TO D-FIL-MAKE
                 MOVE "MODEL : "     TO D-FIL-MODEL
                 MOVE "YEAR : "      TO D-FIL-YEAR
                 MOVE "VIN # : "     TO D-FIL-ID
                 MOVE LTC-MAKE(1)    TO D-MAKE
                 MOVE LTC-MODEL(1)   TO D-MODEL
                 MOVE LTC-YEAR(1)    TO D-YEAR
                 MOVE LTC-VIN(1)     TO D-ID
                 PERFORM WRITE-DETAIL-LINE
              ELSE
                 MOVE SPACES TO LTC-AUTO(2).

           IF PRCOL-FG = "Y"
              IF (SUM-ONLY            = "N")   AND
                 (LTC-MAKE(2)     NOT = SPACES OR
                  LTC-MODEL(2)    NOT = SPACES OR
                  LTC-YEAR(2)     NOT = SPACES OR
                  LTC-VIN(2)      NOT = SPACES)
                 MOVE "MAKE : "      TO D-FIL-MAKE
                 MOVE "MODEL : "     TO D-FIL-MODEL
                 MOVE "YEAR : "      TO D-FIL-YEAR
                 MOVE "VIN # : "     TO D-FIL-ID
                 MOVE LTC-MAKE(2)    TO D-MAKE
                 MOVE LTC-MODEL(2)   TO D-MODEL
                 MOVE LTC-YEAR(2)    TO D-YEAR
                 MOVE LTC-VIN(2)     TO D-ID
                 PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRBGS_SQL.CPY".
       COPY "LIBLP/LPLTCGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

      * COPY "LIBLP/LPTRB1RN.CPY".
      * COPYMEMBER: LIBLP/LPTRB1RN
       LOAD-TRB1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRB-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRB-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRB-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRB-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRB-ALL-DATA
              MOVE TRB-ALL-PATH             TO TRB-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRB-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRB-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRB-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRB-ALL-DATA
              MOVE TRB-ALL-PATH             TO TRB-PATH.

      *-----------------------------------------------------------------
       OPEN-TRB1-FILE.
           PERFORM LOAD-TRB1-FILE.
           PERFORM OPEN-IT.
           MOVE TRB-PATH-SQL TO E-FILE.

           IF TRB1-CONN-OPEN-SW = "N"
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS TRB1_CONNECT
                        USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE 'Y' TO TRB1-CONN-OPEN-SW.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.
  
      *-----------------------------------------------------------------
       START-TRB1-FILE. *> SSP - TRB,LTC,BW
           PERFORM START-IT.
           MOVE TRB-PATH-OWNBR  TO TB-BRNO.
           MOVE TRB-PATH-SQL    TO E-FILE.
           MOVE TRB1-KEY        TO E-KEYX.

           IF ( TRB1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRB1-FILE.

           IF ( QTRB1-WEND-LINE = ZEROES    ) OR
              ( QTRB1-WEND-LINE NOT NUMERIC )
              MOVE TRB-PATH-OWNBR       TO QTRB1-WBEG-BRNO
              MOVE TRB-PATH-OWNBR       TO QTRB1-WEND-BRNO
              MOVE TB-DATE              TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRB1-WBEG-DATE
              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRB1-WEND-DATE
              MOVE TB-CLASS             TO QTRB1-WBEG-CLASS
              MOVE TB-ACCTNO            TO QTRB1-WBEG-ACCTNO
              MOVE TB-LAWCODE           TO QTRB1-WBEG-LAWCODE
              MOVE TB-LINE              TO QTRB1-WBEG-LINE
              MOVE ALL "9"              TO QTRB1-WEND-CLASS
                                           QTRB1-WEND-ACCTNO
                                           QTRB1-WEND-LAWCODE
                                           QTRB1-WEND-LINE.


      * STORED PROCEDURE       
      *
      *  SELECT TRBFILE.TB_BRNO,
      *         CAST(TRBFILE.TB_DATE AS VARCHAR(10)),
      *         TRBFILE.TB_CLASS,
      *         TRBFILE.TB_ACCTNO,
      *         TRBFILE.TB_LAWCODE,
      *         TRBFILE.TB_LINE,
      *         TRBFILE.TB_SSNO_1,
      *         TRBFILE.TB_SSNO_2,
      *         TRBFILE.TB_SSNO_3,
      *         TRBFILE.TB_SSNO_4,
      *         TRBFILE.TB_MAKERCD_1, 
      *         TRBFILE.TB_MAKERCD_2, 
      *         TRBFILE.TB_MAKERCD_3, 
      *         TRBFILE.TB_MAKERCD_4, 
      *         TRBFILE.TB_REF_FRTO, 
      *         TRBFILE.TB_REFBR,
      *         TRBFILE.TB_PURCD, 
      *         TRBFILE.TB_SRCD, 
      *         TRBFILE.TB_COLLCD, 
      *         CAST(TRBFILE.TB_LOANDATE AS VARCHAR(10)), 
      *         TRBFILE.TB_PROCEEDS,
      *         TRBFILE.TB_LNAMT,
      *         TRBFILE.TB_FINCHG,
      *         TRBFILE.TB_INTCHG,
      *         TRBFILE.TB_SERCHG,
      *         TRBFILE.TB_EXTCHG,
      *         TRBFILE.TB_MAINTFEE,
      *         TRBFILE.TB_ORGTERM,
      *         TRBFILE.TB_DLNO,
      *         TRBFILE.TB_DLDISC,
      *         TRBFILE.TB_PURBAL,
      *         TRBFILE.TB_DLPARVPAID,
      *         TRBFILE.TB_DLPARVHELD,
      *         CAST(TRBFILE.TB_POSTDATE AS VARCHAR(10)), 
      *         TRBFILE.TB_PLCD,
      *         ISNULL(LNFILE.LN_OWNBR, 0),
      *         ISNULL(LNFILE.LN_ACCTNO, 0),
      *         ISNULL(LNFILE.LN_APRATE, 0),
      *         ISNULL(LTCFILE.LTC_BRNO, 0),
      *         ISNULL(LTCFILE.LTC_ACCTNO, 0),
      *         ISNULL(LTCFILE.LTC_SEQNO, 0),
      *         ISNULL(LTCFILE.LTC_DESC_1,''),
      *         ISNULL(LTCFILE.LTC_MAKE_1,''),
      *         ISNULL(LTCFILE.LTC_MODEL_1,''),
      *         ISNULL(LTCFILE.LTC_YEAR_1,''),
      *         ISNULL(LTCFILE.LTC_STYLE_1,''),
      *         ISNULL(LTCFILE.LTC_CLASS_1,''),
      *         ISNULL(LTCFILE.LTC_VIN_1,''),
      *         ISNULL(LTCFILE.LTC_TITLE_NO_1,''),
      *         ISNULL(LTCFILE.LTC_MILEAGE_1, 0),
      *         ISNULL(LTCFILE.LTC_VALUE_1, 0),
      *         ISNULL(LTCFILE.LTC_SELLPR_1, 0),
      *         ISNULL(LTCFILE.LTC_MISC_AMT_1, 0),
      *         ISNULL(LTCFILE.LTC_SENT_FLAG_1,''),
      *         ISNULL(CAST(LTCFILE.LTC_VIN_CHG_DATE_1
      *                                  AS VARCHAR(10)),'1900-01-01'),
      *         ISNULL(LTCFILE.LTC_DESC_2,''),
      *         ISNULL(LTCFILE.LTC_MAKE_2,''),
      *         ISNULL(LTCFILE.LTC_MODEL_2,''),
      *         ISNULL(LTCFILE.LTC_YEAR_2,''),
      *         ISNULL(LTCFILE.LTC_STYLE_2,''),
      *         ISNULL(LTCFILE.LTC_CLASS_2,''),
      *         ISNULL(LTCFILE.LTC_VIN_2,''),
      *         ISNULL(LTCFILE.LTC_TITLE_NO_2,''),
      *         ISNULL(LTCFILE.LTC_MILEAGE_2, 0),
      *         ISNULL(LTCFILE.LTC_VALUE_2, 0),
      *         ISNULL(LTCFILE.LTC_SELLPR_2, 0),
      *         ISNULL(LTCFILE.LTC_MISC_AMT_2, 0),
      *         ISNULL(LTCFILE.LTC_SENT_FLAG_2,''),
      *         ISNULL(CAST(LTCFILE.LTC_VIN_CHG_DATE_2
      *                                  AS VARCHAR(10)),'1900-01-01'),
      *         ISNULL(BWFILE1.BW_OWNBR, 0),
      *         ISNULL(BWFILE1.BW_SSNO, 0),
      *         ISNULL(BWFILE1.BW_LNAME,''),
      *         ISNULL(BWFILE1.BW_FNAME,''),
      *         ISNULL(BWFILE1.BW_ADR1,''),
      *         ISNULL(BWFILE1.BW_ADR2,''),
      *         ISNULL(BWFILE1.BW_CITY,''),
      *         ISNULL(BWFILE1.BW_STATE,''),
      *         ISNULL(BWFILE1.BW_ZIP,''),
      *         ISNULL(BWFILE2.BW_OWNBR, 0),
      *         ISNULL(BWFILE2.BW_SSNO, 0),
      *         ISNULL(BWFILE2.BW_LNAME,''),
      *         ISNULL(BWFILE2.BW_FNAME,''),
      *         ISNULL(BWFILE2.BW_ADR1,''),
      *         ISNULL(BWFILE2.BW_ADR2,''),
      *         ISNULL(BWFILE2.BW_CITY,''),
      *         ISNULL(BWFILE2.BW_STATE,''),
      *         ISNULL(BWFILE2.BW_ZIP,'')
      *
      *    FROM DBO.TRBFILE TRBFILE
      * 
      *    LEFT OUTER JOIN
      *          DBO.LNFILE  LNFILE 
      *      ON (TRBFILE.TB_BRNO        = LNFILE.LN_OWNBR
      *     AND  TRBFILE.TB_ACCTNO      = LNFILE.LN_ACCTNO)
      *
      *   LEFT OUTER JOIN
      *         DBO.LTCFILE LTCFILE 
      *     ON (LNFILE.LN_OWNBR        = LTCFILE.LTC_BRNO
      *    AND  LNFILE.LN_ACCTNO       = LTCFILE.LTC_ACCTNO
      *    AND  LTCFILE.LTC_SEQNO      = 1) 
      *
      *   LEFT OUTER JOIN
      *         DBO.BWFILE  BWFILE1 
      *     ON (TRBFILE.TB_BRNO        = BWFILE1.BW_OWNBR
      *    AND  TRBFILE.TB_SSNO_1      = BWFILE1.BW_SSNO)
      *
      *   LEFT OUTER JOIN
      *         DBO.BWFILE  BWFILE2 
      *     ON (TRBFILE.TB_BRNO        = BWFILE2.BW_OWNBR
      *    AND  TRBFILE.TB_SSNO_2      = BWFILE2.BW_SSNO)
      *
      *   WHERE TRBFILE.TB_BRNO        = @QTRB1_WBEG_BRNO
      *     AND TRBFILE.TB_DATE  BETWEEN @QTRB1_WBEG_DATE
      *                              AND @QTRB1_WEND_DATE
      *     AND TRBFILE.TB_CLASS BETWEEN @QTRB1_WBEG_CLASS
      *                              AND @QTRB1_WEND_CLASS
      *                
      *   ORDER BY TRBFILE.TB_BRNO,
      *            TRBFILE.TB_CLASS,
      *            TRBFILE.TB_ACCTNO,
      *            TRBFILE.TB_DATE,
      *            TRBFILE.TB_LAWCODE,
      *            TRBFILE.TB_LINE,
      *            TRBFILE.TB_REFBR  
      

           EXEC SQL SET CONNECTION TRB1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE TRB1_CURSOR CURSOR FOR
               :TRB1-RETURN-CODE = EXEC SSP_EMTBRG_TRBFILE_SELECT
              (:QTRB1-WBEG-BRNO,  
               :QTRB1-WBEG-DATE,  
               :QTRB1-WEND-DATE,
               :QTRB1-WBEG-CLASS, 
               :QTRB1-WEND-CLASS)
           END-EXEC.

           EXEC SQL
               OPEN TRB1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRB1-WBEG-BRNO 
                          QTRB1-WEND-BRNO
                          QTRB1-WBEG-CLASS
                          QTRB1-WEND-CLASS
                          QTRB1-WBEG-ACCTNO
                          QTRB1-WEND-ACCTNO
                          QTRB1-WBEG-LAWCODE
                          QTRB1-WEND-LAWCODE
                          QTRB1-WBEG-LINE
                          QTRB1-WEND-LINE.
           MOVE SPACES TO QTRB1-WBEG-DATE
                          QTRB1-WEND-DATE.

           MOVE STAT---GOOD TO TRB1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.
      
      *-----------------------------------------------------------------
       READ-TRB1-FILE-NEXT. *> EMBEDDED - TRB,LTC,BW
           PERFORM READ-IT.
           MOVE TRB-PATH-OWNBR  TO TB-BRNO.
           MOVE TRB-PATH-SQL    TO E-FILE.
           MOVE TRB1-KEY        TO E-KEYX.
           INITIALIZE QTRB-REC.
           INITIALIZE QLN-REC.
           INITIALIZE QLTC-REC.
           INITIALIZE QBW-REC.

           EXEC SQL SET CONNECTION TRB1_CONNECT END-EXEC.

           IF ( TRB1-CURSOR-STAT-GOOD )
              EXEC SQL
                FETCH TRB1_CURSOR 
                  INTO  
                    :QTB-BRNO,
                    :QTB-DATE,
                    :QTB-CLASS,
                    :QTB-ACCTNO,
                    :QTB-LAWCODE,
                    :QTB-LINE,
                    :QTB-SSNO-1,
                    :QTB-SSNO-2,
                    :QTB-SSNO-3,
                    :QTB-SSNO-4,
                    :QTB-MAKERCD-1, 
                    :QTB-MAKERCD-2, 
                    :QTB-MAKERCD-3, 
                    :QTB-MAKERCD-4, 
                    :QTB-REF-FRTO, 
                    :QTB-REFBR,
                    :QTB-PURCD, 
                    :QTB-SRCD, 
                    :QTB-COLLCD, 
                    :QTB-LOANDATE, 
                    :QTB-PROCEEDS,
                    :QTB-LNAMT,
                    :QTB-FINCHG,
                    :QTB-INTCHG,
                    :QTB-SERCHG,
                    :QTB-EXTCHG,
                    :QTB-MAINTFEE,
                    :QTB-ORGTERM,
                    :QTB-DLNO,
                    :QTB-DLDISC,
                    :QTB-PURBAL,
                    :QTB-DLPARVPAID,
                    :QTB-DLPARVHELD,
                    :QTB-POSTDATE, 
                    :QTB-PLCD,
                    :QTB-UNEARN-1,          
                    :QTB-UNEARN-2,          
                    :QTB-UNEARN-3,          
                    :QTB-UNEARN-4,          
                    :QTB-UNEARN-5,          
                    :QTB-UNEARN-6,          
                    :QTB-DLRVPRIN-1,        
                    :QTB-DLRVPRIN-2,        
                    :QTB-DLRVPRIN-3,        
                    :QTB-DLRVCHGS-1,        
                    :QTB-DLRVCHGS-2,        
                    :QTB-DLRVCHGS-3, 
                    :QLN-OWNBR,  
                    :QLN-ACCTNO,  
                    :QLN-APRATE,  
                    :QLTC-BRNO,  
                    :QLTC-ACCTNO, 
                    :QLTC-SEQNO,  
                    :QLTC-DESC-1, 
                    :QLTC-MAKE-1, 
                    :QLTC-MODEL-1, 
                    :QLTC-YEAR-1, 
                    :QLTC-STYLE-1, 
                    :QLTC-CLASS-1, 
                    :QLTC-VIN-1, 
                    :QLTC-TITLE-NO-1, 
                    :QLTC-MILEAGE-1,  
                    :QLTC-VALUE-1,  
                    :QLTC-SELLPR-1, 
                    :QLTC-MISC-AMT-1, 
                    :QLTC-SENT-FLAG-1,
                    :QLTC-VIN-CHG-DATE-1,
                    :QLTC-DESC-2, 
                    :QLTC-MAKE-2, 
                    :QLTC-MODEL-2, 
                    :QLTC-YEAR-2, 
                    :QLTC-STYLE-2, 
                    :QLTC-CLASS-2, 
                    :QLTC-VIN-2, 
                    :QLTC-TITLE-NO-2, 
                    :QLTC-MILEAGE-2,
                    :QLTC-VALUE-2,
                    :QLTC-SELLPR-2,
                    :QLTC-MISC-AMT-2,
                    :QLTC-SENT-FLAG-2,
                    :QLTC-VIN-CHG-DATE-2,
                    :QWS-BW-OWNBR1,  
                    :QWS-BW-SSNO1,  
                    :QWS-BW-LNAME1, 
                    :QWS-BW-FNAME1, 
                    :QWS-BW-ADR11, 
                    :QWS-BW-ADR21, 
                    :QWS-BW-CITY1, 
                    :QWS-BW-STATE1, 
                    :QWS-BW-ZIP1, 
                    :QWS-BW-OWNBR2,  
                    :QWS-BW-SSNO2,  
                    :QWS-BW-LNAME2, 
                    :QWS-BW-FNAME2, 
                    :QWS-BW-ADR12, 
                    :QWS-BW-ADR22, 
                    :QWS-BW-CITY2, 
                    :QWS-BW-STATE2, 
                    :QWS-BW-ZIP2
              END-EXEC

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRB1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRB-FIELDS
              PERFORM GET-LN-FIELDS
              PERFORM GET-LTC-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TRB1-FILE.

           PERFORM CLOSE-IT.
           IF ( TRB1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION TRB1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE TRB1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO TRB1-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-TRB1-CONNECTION.

           IF TRB1-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION TRB1_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT TRB1_CONNECT END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE "N" TO TRB1-CONN-OPEN-SW.


      * COPY "LIBLP/LPLC1RN.CPY".
      * COPYMEMBER: LIBLP/LPLC1RN
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-LC1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LC-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LC-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO LC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LC-ALL-MACHINE
              MOVE LC-ALL-PATH             TO LC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LC-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO LC-ALL-BASE
              MOVE FILPATH-MACHINE         TO LC-ALL-MACHINE
              MOVE LC-ALL-PATH             TO LC-PATH.

      *-----------------------------------------------------------------
       OPEN-LC1-FILE.
           PERFORM LOAD-LC1-FILE.
           PERFORM OPEN-IT.
           MOVE LC-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       READ-LC1-FILE. *> EMBEDDED - LCFILE
           PERFORM READ-IT.
           MOVE LC-PATH-SQL TO E-FILE.
           MOVE LC1-KEY TO E-KEYX.
           INITIALIZE QLC-REC.

           MOVE LC-CODE TO QLC-CODE.

           EXEC SQL
            SELECT LCFILE.LC_CODE,
                   LCFILE.LC_DESC
              INTO 
                  :QLC-CODE, 
                  :QLC-DESC
              FROM DBO.LCFILE
             WHERE LCFILE.LC_CODE = :QLC-CODE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO READ-LC1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LC-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LC1-FILE.

           PERFORM CLOSE-IT.
  
       COPY "LIBGB/GBWKIN.CPY".
      *COPY "LIBGB/GBWK2IN.CPY".
       COPY "LIBLP/LPLTC1RN.CPY".
      *-----------------------------------------------------------------
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              IF EXT-ROUTE-BUF = "00" OR "70"
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
      *    MOVE WK2-PATH TO ACCESS-BUF.
      *    PERFORM REMOVE-WORKFILE.

      *-----------------------------------------------------------------
       EXIT-PROG.
           IF SV-BEG-BR NOT = 9999
              MOVE SV-BEG-BR TO BEG-BRANCH
              MOVE SV-END-BR TO END-BRANCH.

           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE SV-BEG-BR TO BEG-BRANCH
              MOVE SV-END-BR TO END-BRANCH
              MOVE SV-GROUP-FLAG TO GROUP-FLAG
              MOVE SV-ENT-GROUP TO ENT-GROUP.

           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY EMTBRG-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
