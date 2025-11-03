       IDENTIFICATION DIVISION.
       PROGRAM-ID.   EMSFRG.
       DATE-WRITTEN. 042084
      ******************************************************************
      *
      *                      (LP)
      *    DESCRIPTION:  MONTHLY SALES FINANCE REGISTER
      *
      * REV:
      *  BLL 071295 ADD PRINT-RANGE-LINES ROUTINE
      *  JTG 071295 ADDED TS-INSPREM-DLADJ
      *  JTG 080595 ADDED LOGIC FOR O3 INSURANCE
      *  BAH 080995 ADDED REGPYAMT
      *  KEC 091595 ADDED LOCAL-SPOOLING LOGIC.
      *  KEC 091595 CHANGED MILITARY TIME TO AM/PM ON HEADER.
      *  KEC 091595 ADDED CHECK FOR VALID BRANCH # FOR CLASS TOTALS.
      *  BAH 092895 RECORDS-FOUND INITIALIZED TO "N" INSTEAD OF " "
      *  JFF 100695 ADDED CLASS SCAN
      *  KEC 101295 COMMENTED OUT DEFAULT BRANCH SETTING IN INITIZATION.
      *  BLL 101295 TESTING GROUP BATCH PROCEDURES - IN PROGRESS!
      *  JFF 111395 CHANGED FORM TO INCLUDE LOCAL-SPOOLING QUESTION.
      *  KEC 113095 FIXED LOCAL-SPOOL FOR IF A BRANCH HAS NO DATA.
      *  JFF 121395 CHANGED "IF LOCAL-PRINT-FG = N" TO
      *             "IF LOCAL-PRINT-FG NOT = Y" IN INITIALIZATION.
      *  JFF 030596 ADDED "SUMMARY ONLY" TO FORM & PARAMETER SECTION.
      *  MJD 042596 ADDED GP-DLR-DISP-TYPE.
      *  JFF 060596 ADDED 'GROUP TOTALS' LOGIC.
      *  JFF 071096 CHANGED PRINTING OF GRAND TOTALS TO PRINT VERTICALLY
      *             INSTEAD OF HORIZONTALLY.
      *  BLV 030397 PRINT BORROWER SCORE
      *  BLV 060597 CHANGED FOR LOCAL SPOOL WITH NEW BATCH DRIVER - BREXEC
      *  GLF 090897 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *  GLF 022098 DELETED BR-SECURITY-OK AND ADDED LIBGB/BRSECUREW.
      *  GLF 022798 REMOVED "MONTH END" REFERENCE FROM TITLE
      *  BAH 051298 EXPANDED INSURANCE TOTALS, MOVED TOTAL LINE AROUND,
      *             FFG, PL# 187
      *  BAH 060498 EXPANDED LNAMT AND PURBAL, AGAIN , FOR FFG
      *  JTG 101398 ADDED SET OF LCSCAN-REVOLVING = 'N', REGENCY REVOL
      *  KEC 121898 INCREASED THE SIZE OF FIELDS FOR CALCULATING TOTALS;
      *              THIS IS PER CINDY (PROBLEM AT MERCURY; BUT PRINT
      *               DISPLAY NEEDS TO BE FIXED).
      *  BAH 990309 PRCOL-FG WAS NOT UNDER EOBUF SO THE BATCH LOST THE
      *             VALUE EVERYTIME.
      *  GLF 043099 WAS MOVING SPACES TO DETAIL-LINE ON SUMMARY ONLY IN
      *             WRONG SPOT CAUSING LINES TO BE MISSING, IS NOW FIXED
      *  MJD 990803 ADDED GP-INCL-OTH-IN-EARNED FLAG.
      *  GLF 081899 ADD ARCHIVE PATH INFO. FOR TR FILES NOT FOUND IN DATA
      *             SET FOR SELECTED MONTH AND YEAR
      *  BLV 990826 FIX ARCHIVE PATH LOGIC
      *  BAH 000120 RSVPRIN AND RSVCHRG GRAND TOTALS WERE TRUNCATED BECAUSE
      *             TOT-DLRVPRIN AND TOT-DLRVCHGS WERE ONLY 9(6)V99, TFC PL#248
      *  MJD 000120 REGACC PR#1046 - DO NOT LET ACCOUNTS SPAN PAGES.
      *             INSTEAD OF PRINTING EACH DETAIL LINE AS IT IS BUILT,
      *             I MOVE IT TO A HOLD BUFFER.  AT THE END OF THE
      *             TR-DETAIL1 ROUTINE I COUNT THE LINES IN THE BUFFER
      *             TO DETERMINE IF THE ACCOUNT WILL FIT ON THE CURRENT
      *             PAGE OF IF IT SHOULD BEGIN THE FOLLOWING PAGE.
      *             IF ANY NEW DETAIL LINE PRINTS ARE ADDED THEN THE
      *             TABLE WILL HAVE TO BE EXPANDED ACCORDINGLY.
      *  CS  000320 ADDED AVERAGE WEIGHTED TERM, TOTAL PAYMENTS /REG PAY.
      *             ADD IN FINCHG IF TS-LOANTYPE = "I"  MERCURY #225
      *  CS  000330 CORRECTED ADDING OF LOAN AMOUNT TO WK-X-LNAMT IF
      *             TS-LOANTYPE = "I"
      *  MJD 000503 FIXED BUG. PURCHASE BALANCE WAS GETTING UPDATED
      *             TWICE FOR EACH ACCOUNT IN THE SOURCE CODE TOTALS
      *  MJD 000505 CHANGED THE WAY TOTALS PRINT . INSREAD OF PRINTING
      *             WITHIN ROWS WITH HEADERS ON THE TOP, THEY NOW PRINT
      *             IN 5 COLUMNS WITH THE HEADER PRINTED NEXT TO THE NUMBER
      *             THIS WAS DONE BECAUSE NUMBERS WERE GETTING TRUNCATED AND
      *             IT WAS EASIER TO READ THEN TO ADD A FOURTH LINE OF
      *             TOTALS AND HEADERS.  (SUPPORT PL#214)
      *  BAH 000523 OUR-DL WAS NOT ALWAYS PRINTING IN THE NEW TOTALS, THERE
      *             WAS A "." MISSING ON THE "IF" STATEMENT ABOVE IT.
      *  MJD 000524 CENTERRED COMPANY NAME ON REPORT.
      *  BAH 000609 T-RSVPRIN AND T-RSVCHRG WERE NOT SIGNED, SO VOIDS WERE
      *             NOT PRINTING CORRECT AND/OR ADDING CORRECTLY, CHALLFN
      *  BAH 020614 ADDED TS-DLPAID FOR REGACC, PR# 1822
      *  CS  021002 ADDED TS-DLCOMM-PP-CMSI, TS-DLCOMM-O3-CMSI IF
      *             GP-DLCOMM-CMSI-FG = "Y".
      *  BAH 021008 FIXED RECORDS-FOUND TO NOT GIVE "NO RECORDS FOUND"
      *             ON A F7 EXIT
      *  BAH 030425 ADDED LC-USE-ALT-GL-FEE AND CD-GC-ALT-GLFEENO FOR
      *             LENDMARK PR# 2048
      *  BAH 040903 REPLACE BR-NAME IN HEADING WITH BR-REGION-NUMBER
      *             FOR GP-I-AM-REGACC, REGACC PR# 77
      *  JTG 050927 CHANGED TO PRINT LN-UNWIND-REASON-CODE AND THE
      *             CASH DRAWER NO POSTED AS AN OFFSET TO DEALER PAYABLE
      *             ADDED LN-POCD-UNWIND "*U" A LN-POCD-NONLOAN
      *             AND LN-UNWIND-REASON-CODE                       REGACC #0105
      *  BAH 070703 DO NOT PRINT SSNO FOR WORLD, WORLD PR# 536
      *   CS 081112 ADD TEST ON GP-INCL-OTH-IN-EARNED = 'R'. THIS IS REGACC WHO
      *             WANTS TO EXCLUDE OTHER(6) AND MAINTFEE(4) FROM EARN/UNEARNED
      *             THEY USE MAINTFEE TO AMORTIZE PARTICIPATION PR#210 REGACC
      *  BAH 090304 CLEAR OUT BW-REC IF RECORD NOT FOUND AND 0 OUT THE
      *             NUMERIC FIELDS, FINMGM PL# 650
      *  BAH 090114 TEST GP-HIDE-FIRST-5-SSNO, REGMGM PR# 3785
      *   CS 120725 ON TOTALS, IF ANTICERN WAS = TO FINCHG, WE DIDNT PRINT AN
      *             ANTICERN. CHANGED TO ALWAYS PRINT ANTICERN ON TOTALS. THE
      *             DETAIL ONLY PRINTS ANTICERN UNDER CERTAIN CONDITIONS,
      *             WAITING FOR JUDY TO DECIDE IF THAT NEEDS CHANGED.
      *                                         FINANCIAL MGMT PR#4084
      *             I ALSO FOUND A BUG WITH THE SUBSCRIPT ON RESERVE, TESTING
      *             > 4, THERE ARE ONLY 3 RESERVES
      *  BAH 150827 ADDED DEALER RANGE, LEADERS PR#4842
      *             IF RANGE NOT ENTERED IN BATCH, WILL DEFAULT TO INCLUDE
      *             ALL, DID NOT FORCE A VERSION CHANGE
      *  BAH 160118 SPLIT OUT LTFILE, PD0003
      * KEC 2016.0315 CHANGED CD-FEE???? TO CD-GC-????.
      * BAH 2016.1103 CHANGE TILLNO TO 9(2)
      * BAH 2017.0530 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 2019.0625 REMOVED LCSCAN-REVOLING PT0153
      * BAH 2019.0922 REMOVED ACCESS-CALL TRSFILE
      **************************************************************************
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
           03  WK-LNAMT         PIC S9(9)V99  COMP-3.
           03  WK-FINCHG        PIC S9(9)V99  COMP-3.
           03  WK-INTCHG        PIC S9(9)V99  COMP-3.
           03  WK-SERCHG        PIC S9(9)V99  COMP-3.
           03  WK-EXTCHG        PIC S9(9)V99  COMP-3.
           03  WK-MFEE          PIC S9(9)V99  COMP-3.
           03  WK-EARNINGS      PIC S9(9)V99  COMP-3.
           03  WK-DL            PIC S9(9)V99  COMP-3.
           03  WK-LL            PIC S9(9)V99  COMP-3.
           03  WK-AH            PIC S9(9)V99  COMP-3.
           03  WK-PP            PIC S9(9)V99  COMP-3.
           03  WK-O1            PIC S9(9)V99  COMP-3.
           03  WK-O2            PIC S9(9)V99  COMP-3.
           03  WK-O3            PIC S9(9)V99  COMP-3.
           03  WK-FEES          PIC S9(9)V99  COMP-3.
           03  WK-DLRVPRIN      OCCURS 3
                                PIC S9(9)V99  COMP-3.
           03  WK-DLRVCHGS      OCCURS 3
                                PIC S9(9)V99  COMP-3.
           03  WK-DLPARVHELD    PIC S9(9)V99  COMP-3.
           03  WK-DLPARVPAID    PIC S9(9)V99  COMP-3.
           03  WK-DLDISC        PIC S9(9)V99  COMP-3.
           03  WK-PROCEEDS      PIC S9(9)V99  COMP-3.
           03  WK-PURBAL        PIC S9(9)V99  COMP-3.
           03  WK-ANTICERN      PIC S9(9)V99  COMP-3.
           03  WK-INSPREM-DLADJ PIC S9(9)V99  COMP-3.
           03  WK-SUM-OF-PMT    PIC S9(9)V99  COMP-3.
           03  WK-X-LNAMT       PIC S9(9)V99  COMP-3.
           03  WK-DLCOMM-PP-CMSI PIC S9(4)V99 COMP-3.
           03  WK-DLCOMM-O3-CMSI PIC S9(4)V99 COMP-3.
      ******************************************************************
      * SORTED LNFILE BY BRANCH, CLASS, ACCOUNT
      ******************************************************************
       FD  WK2-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK2-REC.
           03  WK2-KEY.
               05  WK2-BRANCH           PIC 9(4).
               05  WK2-CLASS            PIC 9(3).
               05  WK2-ACCTNO           PIC 9(8).
               05  WK2-DATE             PIC 9(8).
               05  WK2-LAWCODE          PIC 99.
               05  WK2-LINE             PIC 9(3).

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)LP/EMSFRG S35 07/26/23-13:19:45 >".

       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".
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
       COPY "LIBLP/LP01TRS.CPY".
       COPY "LIBLP/LP01TRS_SQL.CPY".
       COPY "LIBLP/LPWSTRS.CPY".
      *-----------------------------------------------------------------
       01  PR-IFN               PIC X(2)     VALUE "PR".
       01  WK-IFN               PIC X(2)     VALUE "WK".
       01  WK2-IFN              PIC X(3)     VALUE "WK2".
       01  WK2-PATH             PIC X(35).


       01 T-RSVCHRG             PIC S9(9)V99  COMP-3.
       01 T-RSVPRIN             PIC S9(9)V99  COMP-3.
       01 TOT-DLRVPRIN          PIC 9(9)V99  COMP-3.
       01 TOT-DLRVCHGS          PIC 9(9)V99  COMP-3.
       01 T-WK-DLRVPRIN         PIC 9(9)V99  COMP-3.
       01 T-WK-DLRVCHGS         PIC 9(9)V99  COMP-3.
       01 TSUB                  PIC 9.

       01 AVGTERM               PIC S9(9)V99  COMP-3.
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
           03  SAVE-WK-CLASS    PIC 9(3).
           03  SAVE-WK-SRCD     PIC X(3).

       01  SUMMARY-FG           PIC X     VALUE "N".

       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".


       01  MESSAGE-01-LOC-PRT   PIC X(43)    VALUE
           "CANNOT DO LOCAL SPOOLING WITH GROUP TOTALS".

       01  WS-GTT-TOTAL-TABLE.
           03  WS-GTT-TABLE-GROUP-LEVEL                 OCCURS 10 TIMES.
               05  WS-GTT-NOLOANS         PIC S9(7)     COMP.
               05  WS-GTT-LNAMT           PIC S9(9)V99  COMP.
               05  WS-GTT-FINCHG          PIC S9(9)V99  COMP.
               05  WS-GTT-INTCHG          PIC S9(9)V99  COMP.
               05  WS-GTT-SERCHG          PIC S9(9)V99  COMP.
               05  WS-GTT-EXTCHG          PIC S9(9)V99  COMP.
               05  WS-GTT-MFEE            PIC S9(9)V99  COMP.
               05  WS-GTT-EARNINGS        PIC S9(9)V99  COMP.
               05  WS-GTT-DL              PIC S9(9)V99  COMP.
               05  WS-GTT-LL              PIC S9(9)V99  COMP.
               05  WS-GTT-AH              PIC S9(9)V99  COMP.
               05  WS-GTT-PP              PIC S9(9)V99  COMP.
               05  WS-GTT-O1              PIC S9(9)V99  COMP.
               05  WS-GTT-O2              PIC S9(9)V99  COMP.
               05  WS-GTT-O3              PIC S9(9)V99  COMP.
               05  WS-GTT-FEES            PIC S9(9)V99  COMP.
               05  WS-GTT-DLRVPRIN        PIC S9(9)V99  COMP.
               05  WS-GTT-DLRVCHGS        PIC S9(9)V99  COMP.
               05  WS-GTT-DLPARVHELD      PIC S9(9)V99  COMP.
               05  WS-GTT-DLPARVPAID      PIC S9(9)V99  COMP.
               05  WS-GTT-DLDISC          PIC S9(9)V99  COMP.
               05  WS-GTT-PROCEEDS        PIC S9(9)V99  COMP.
               05  WS-GTT-PURBAL          PIC S9(9)V99  COMP.
               05  WS-GTT-ANTICERN        PIC S9(9)V99  COMP.
               05  WS-GTT-INSPREM-DLADJ   PIC S9(9)V99  COMP.
               05  WS-GTT-SUM-OF-PMT      PIC S9(9)V99  COMP.
               05  WS-GTT-X-LNAMT         PIC S9(9)V99  COMP.
               05  WS-GTT-DLCOMM-PP-CMSI  PIC S9(4)V99  COMP.
               05  WS-GTT-DLCOMM-O3-CMSI  PIC S9(4)V99  COMP.

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
               05  T-LNAMT         PIC S9(9)V99  COMP-3.
               05  T-FINCHG        PIC S9(9)V99  COMP-3.
               05  T-INTCHG        PIC S9(9)V99  COMP-3.
               05  T-SERCHG        PIC S9(9)V99  COMP-3.
               05  T-EXTCHG        PIC S9(9)V99  COMP-3.
               05  T-MFEE          PIC S9(9)V99  COMP-3.
               05  T-EARNINGS      PIC S9(9)V99  COMP-3.
               05  T-DL            PIC S9(9)V99  COMP-3.
               05  T-LL            PIC S9(9)V99  COMP-3.
               05  T-AH            PIC S9(9)V99  COMP-3.
               05  T-PP            PIC S9(9)V99  COMP-3.
               05  T-O1            PIC S9(9)V99  COMP-3.
               05  T-O2            PIC S9(9)V99  COMP-3.
               05  T-O3            PIC S9(9)V99  COMP-3.
               05  T-FEES          PIC S9(9)V99  COMP-3.
               05  T-DLRVPRIN      OCCURS 3
                                   PIC S9(9)V99  COMP-3.
               05  T-DLRVCHGS      OCCURS 3
                                   PIC S9(9)V99  COMP-3.
               05  T-DLPARVHELD    PIC S9(9)V99  COMP-3.
               05  T-DLPARVPAID    PIC S9(9)V99  COMP-3.
               05  T-DLDISC        PIC S9(9)V99  COMP-3.
               05  T-PROCEEDS      PIC S9(9)V99  COMP-3.
               05  T-PURBAL        PIC S9(9)V99  COMP-3.
               05  T-ANTICERN      PIC S9(9)V99  COMP-3.
               05  T-INSPREM-DLADJ PIC S9(9)V99  COMP-3.
               05  T-SUM-OF-PMT    PIC S9(9)V99  COMP-3.
               05  T-X-LNAMT       PIC S9(9)V99  COMP-3.
               05  T-DLCOMM-PP-CMSI PIC S9(4)V99 COMP-3.
               05  T-DLCOMM-O3-CMSI PIC S9(4)V99 COMP-3.

       01  CLASS-FG             PIC X(3)    VALUE "   ".
           88  CLASS-BEG                    VALUE "BEG".
           88  CLASS-END                    VALUE "END".
      *-----------------------------------------------------------------
       COPY "LIBLP/EMSFRG_CMD.CPY".
      *-----------------------------------------------------------------
       01  LEGEND               PIC X(70).

      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  BEG-CLASS       PIC 9(3).
           03  END-CLASS       PIC 9(3).
           03  MONTH-END-DATE.
               05  END-MONTH   PIC 99.
               05  END-YEAR    PIC 99.
           03  EDATE REDEFINES MONTH-END-DATE PIC 9999.
           03  DAY-OF-MONTH    PIC 99.
           03  GTT-ISO         PIC X(1).
           03  SUM-ONLY        PIC X.
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).
           03  LOCAL-PRINT-FG  PIC X(1).
           03  TOTAL-LEV-TABLE.
               05  TOTAL-LEVEL-TABLE PIC X(1) OCCURS 10.
           03  PRCOL-FG        PIC X.
           03  BEG-DLNO        PIC 9(4)V99.
           03  BEG-DLNOX REDEFINES BEG-DLNO PIC X(6).
           03  END-DLNO        PIC 9(4)V99.

       01  VERSION-CONTROL     PIC X VALUE "D".

       01  BEG-DATE            PIC 9(8)  VALUE 0.
       01  END-DATE            PIC 9(8)  VALUE 0.
       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.
       01  SV-LOCAL-PRINT-FG   PIC X     VALUE SPACES.
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
           03  FILLER           PIC X(30)    VALUE " ".
           03  FILLER           PIC X(41) VALUE
               "     SALES FINANCE REGISTER    ".

      *-----------------------------------------------------------------
       01  HEAD3.
           03  FILLER           PIC X(10) VALUE "BRANCH :".
           03  H-BRNO           PIC 9(4).
           03  FILLER           PIC X VALUE " ".
           03  H-BRDESC         PIC X(20).
           03  FILLER           PIC X(46) VALUE " ".

      *-----------------------------------------------------------------
       01  HEAD4.
           03  FILLER           PIC X(10) VALUE "CLASS  :".
           03  H-LCNO           PIC 99.
           03  FILLER           PIC X VALUE " ".
           03  H-LCDESC         PIC X(20).

      *-----------------------------------------------------------------
       01  HEAD5.
           03  FILLER           PIC X(40)  VALUE
           "ACCOUNT NO.   LOANDATE  DEALER  APR  SC ".
           03  FILLER           PIC X(44)  VALUE
           "CC TERM  LOAN-AMT    FIN-CHG   INSURANCES   ".
           03  FILLER           PIC X(47)  VALUE
           "----- FEES -----  RSVPRIN    PARHELD   PROCEEDS".

      *-----------------------------------------------------------------
       01  HEAD6.
           03  FILLER           PIC X(36)  VALUE SPACES.
           03  H-CRED-SCORE     PIC X(4)   VALUE SPACES.
           03  FILLER           PIC X(57)  VALUE
           "PC ATRM   PUR-AMT   ANTICERN                GL-NO  AMOUNT".
           03  FILLER           PIC X(34)  VALUE
           "     RSVCHRG    PARPAID   EARNINGS".

      *-----------------------------------------------------------------
       01  HEAD7.
           03  FILLER           PIC X(40).
           03  FILLER           PIC X(20) VALUE
           "   TILL    UNWIND   ".
           03  FILLER          PIC X(8)   VALUE "REGPYAMT".
           03  FILLER          PIC X(34).
           03  HEAD7-DLR-TYP   PIC X(4)    VALUE "COMM".
           03  FILLER          PIC X(26)   VALUE
           "       CHKTYPE   PREM-ADJ".

      *-----------------------------------------------------------------
       01  HEAD8.
           03  FILLER            PIC X(26) VALUE " ".
           03  FILLER            PIC X(46) VALUE
           "LOAN-AMT    FIN-CHG    SRV-CHG     OUR-DL     ".
           03  FILLER            PIC X(51) VALUE
           "OUR-AH     OUR-O1       OUR    RSVPRIN   PARHELD   ".
           03  FILLER            PIC X(8) VALUE
           "EARNINGS".

      *-----------------------------------------------------------------
       01  HEAD9.
           03  FILLER            PIC X(16) VALUE " ".
           03  FILLER            PIC X(45) VALUE
           "LOANS      PUR-AMT   ANTICERN    EXT-CHG     ".
           03  FILLER            PIC X(52) VALUE
           "OUR-LL     OUR-PP     OUR-O2      FEES    RSVCHRG   ".
           03  FILLER            PIC X(18) VALUE
           "PARPAID   PROCEEDS".

      *-----------------------------------------------------------------
       01  HEAD10.
           03  FILLER          PIC X(26)   VALUE " ".
           03  FILLER          PIC X(12)   VALUE "AVG TERM".
           03  FILLER          PIC X(45)   VALUE
           "INT-CHG      M-FEE".
           03  FILLER          PIC X(20)   VALUE "OUR-O3".
           03  FILLER          PIC X(20)   VALUE "COMM".
           03  FILLER          PIC X(8)    VALUE "PREM-ADJ".
      *-----------------------------------------------------------------
       01  HEAD11.
           03  FILLER          PIC X(113)  VALUE SPACES.
           03  FILLER          PIC X(8)    VALUE "DLCOMPP".
           03  FILLER          PIC X(2)    VALUE SPACES.
           03  FILLER          PIC X(7)    VALUE "DLCOMO3".
      *-----------------------------------------------------------------
       01  GTT-HEAD1.
           03  FILLER            PIC X(16)  VALUE
               "              GR".
           03  FILLER            PIC X(41)  VALUE SPACES.
           03  FILLER            PIC X(18)  VALUE "GROUP TOTALS TABLE".
           03  FILLER            PIC X(57)  VALUE SPACES.

      *-----------------------------------------------------------------
       01  GTT-HEAD2.
           03  FILLER            PIC X(17)  VALUE
               "BRANCH/GROUP  LEV".
           03  FILLER            PIC X(115) VALUE SPACES.


      ******************************************************************
      *
      *    D E T A I L - L I N E
      *
      ******************************************************************
      *----------------------------------------------------------------
      * DETAIL LINE TABLE - HOLDS DETAIL LINES UNTIL END OF DETAIL
      *  ROUTINE AND THEN PRINTS THEM ON THE CURRENT OR NEXT PAGE
      *  DEPENDING ON SPACE. IT PREVENTS AN ACCOUNT FROM SPANNING A PAGE
      *----------------------------------------------------------------
       01  DETAIL-LINE-TBL.
           03  DTL-LINE         PIC X(132)  OCCURS 12.
       01  DTL-SUB              PIC 9(02).

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
           03  D-INSPREM        PIC Z(4)9.99-  BLANK ZERO.
           03  D-INSFLAG        PIC X.
           03  D-INSTYPE        PIC X.
           03  D-INSOURS        PIC X.
           03  FILLER           PIC X.
           03  D-FEEGLNO        PIC Z(7).
           03  FILLER           PIC X.
           03  D-FEEAMT         PIC Z(3)9.99-  BLANK ZERO.
           03  D-FEEOURS        PIC X.
           03  D-RSV1           PIC Z(6)9.99- BLANK ZERO.
           03  D-RSV2           PIC Z(6)9.99- BLANK ZERO.
           03  D-RSV2X REDEFINES D-RSV2.
               05  FILLER       PIC X(5).
               05  D-DLPAID     PIC X.
               05  FILLER       PIC X(5).
           03  D-PROCEEDS       PIC Z(6)9.99-  BLANK ZERO.

      *-----------------------------------------------------------------
       01  DETAIL2 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X.
           03  D-MAKERCD        PIC X.
           03  FILLER           PIC X.
           03  D-BW-SSNO        PIC X(11).
           03  FILLER           PIC XX.
           03  D-BW-NAME.
               05  FILLER       PIC X(19).
               03  D-CRED-SCORE PIC ZZZ9.
           03  FILLER           PIC X(74).
           03  D-DLCOMM-PP-CMSI PIC ZZZ9.99-.
           03  FILLER           PIC X(3).
           03  D-DLCOMM-O3-CMSI PIC ZZZ9.99-.
      *-----------------------------------------------------------------
       01  DETAIL3 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(2).
           03  D-BW-ADDR          PIC X(45).
           03  FILLER             PIC X.
           03  D-TILLNO           PIC Z9.
           03  FILLER             PIC X(6).
           03  D-UNWIND-CODE      PIC XX.
           03  FILLER           PIC X(1).
           03  D-REGPYAMT       PIC Z(6).99-   BLANK WHEN ZERO.
      *----------------------------------------------------------------
       01  DETAIL4 REDEFINES DETAIL-LINE.
           03  G-GROUP              PIC X(14).
           03  G-GROUP-1 REDEFINES G-GROUP.
               05  G-TOTNAME        PIC X(14).
               05  G-CODENAME REDEFINES G-TOTNAME.
                   07  FILLER       PIC X(10).
                   07  G-DCODE      PIC XXX.
                   07  G-COLON      PIC X.
           03  G-GROUP-2 REDEFINES G-GROUP.
               05  G-PROMPT1        PIC X(7).
               05  FILLER           PIC X.
               05  G-GRNO           PIC X(4).
               05  G-BRNO           REDEFINES G-GRNO
                                    PIC 9(4).
               05  FILLER           PIC X.
               05  G-GR-LEVEL       PIC 9(1).
           03  FILLER           PIC X(01).
           03  G-HEADING1       PIC X(12).
           03  G-AMOUNT-1       PIC ZZZZZZZZ9.99-  BLANK WHEN ZERO.
           03  FILLER REDEFINES G-AMOUNT-1.
               05  FILLER       PIC X(03).
               05  G-LOANS      PIC ZZZZZ9-    BLANK WHEN ZERO.
               05  FILLER       PIC X(03).
           03  FILLER           PIC X(1).
           03  G-HEADING2       PIC X(09).
           03  G-AMOUNT-2       PIC ZZZZZZZZ9.99-  BLANK WHEN ZERO.
           03  FILLER           PIC X(1).
           03  G-HEADING3       PIC X(09).
           03  G-AMOUNT-3       PIC ZZZZZZZZ9.99-  BLANK WHEN ZERO.
           03  FILLER           PIC X(1).
           03  G-HEADING4       PIC X(09).
           03  G-AMOUNT-4       PIC ZZZZZZZZ9.99-  BLANK WHEN ZERO.
           03  FILLER           PIC X(1).
           03  G-HEADING5       PIC X(09).
           03  G-AMOUNT-5       PIC ZZZZZZZZ9.99-  BLANK WHEN ZERO.

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
               04  R-NUM        PIC Z(6)9.
               04  R-NUM-REDEF REDEFINES R-NUM.
                   05 R-CHAR    PIC X(7).
               04  R-NUMX REDEFINES R-NUM.
                   05  R-DLNO   PIC Z(4).99.

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBWI/GRTOTLW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/EMSFRG_DEF.CPY".
       COPY "LIBLP/EMSFRG_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/EMSFRG_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE WK2-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TRS1-FILE.
           PERFORM CLOSE-LTC1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK-FILE WK2-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.

      D    STOP MESS.
           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "EMSFRG" TO FORM-PROG VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

      * SETUP DATE RANGE TO TEST BASED UPON MMYY AND DAY ENTERED ON SCREEN
      * BEG-DATE AND END-DATE WILL BE THE SAME IF RUNNING FOR ONE DAY
      * BEG-DATE WILL HAVE DAY 01 AND END 31 IF DAY-OF-MONTH IS 99 FOR ALL
      * DO THIS HERE SO BATCH AUDIT/RUN WILL ALSO GET DATE RANGE SET
           MOVE END-MONTH    TO DATE-MMDDYY-MM.
           MOVE DAY-OF-MONTH TO DATE-MMDDYY-DD.
           MOVE END-YEAR     TO DATE-MMDDYY-YY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           IF DAY-OF-MONTH = 99
              MOVE 01           TO DATE-YYYYMMDD-DD
           ELSE
              MOVE DAY-OF-MONTH TO DATE-YYYYMMDD-DD.
           MOVE DATE-YYYYMMDD TO BEG-DATE END-DATE.
           IF DAY-OF-MONTH = 99
              MOVE 31 TO DATE-YYYYMMDD-DD
              MOVE DATE-YYYYMMDD TO NDTE-DATE
              PERFORM NEW-DATE-CALCULATION
              MOVE NDTE-DATE TO END-DATE.

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

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              LTC-PATH-OVERRIDE
                                              TRS-PATH-OVERRIDE.

           PERFORM OPEN-TRS1-FILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN1-FILE.
           
           PERFORM SORT-ROUTINE.

           IF EXT-EOBUF-ERRCD = "X"
              GO TO NEXT-GROUP-LOC.

           PERFORM OPEN-WK2-FILE.

       NEXT-TR-REPORT.
           PERFORM READ-WK2-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-OF-GROUP.

           MOVE WK2-BRANCH  TO TS-BRNO.
           MOVE WK2-CLASS   TO TS-CLASS.
           MOVE WK2-ACCTNO  TO TS-ACCTNO.
           MOVE WK2-DATE    TO TS-DATE.
           MOVE WK2-LAWCODE TO TS-LAWCODE.
           MOVE WK2-LINE    TO TS-LINE.

           PERFORM READ-TRS1-FILE.
           IF IO-FG NOT = 0
              MOVE "TRANSACTION RECORD MISSING SINCE SORT" TO MESS
              PERFORM SEND-MESS
              GO TO IO-ERROR.

           IF FIRST-TIME = "Y"
              MOVE TS-BRNO  TO HOLD-BRNO
              MOVE TS-CLASS TO HOLD-CLASS
              PERFORM SET-CLASS-HEADINGS
              PERFORM SET-BRANCH-HEADINGS.

           IF TS-BRNO NOT = HOLD-BRNO
              PERFORM CLASS-TOTALS
              PERFORM BRANCH-TOTALS
              MOVE TS-BRNO  TO HOLD-BRNO
              MOVE TS-CLASS TO HOLD-CLASS
              PERFORM SET-CLASS-HEADINGS
              PERFORM SET-BRANCH-HEADINGS
              IF ( GTT-ISO NOT = "O" )
                 IF SUM-ONLY = "N"
                    PERFORM HEAD-PAGE.

           IF TS-CLASS NOT = HOLD-CLASS
              PERFORM CLASS-TOTALS
              MOVE TS-CLASS TO HOLD-CLASS
              PERFORM SET-CLASS-HEADINGS
              IF ( GTT-ISO NOT = "O" )
                 IF SUM-ONLY = "N"
                    PERFORM HEAD-PAGE.

           PERFORM TR-DETAIL1.

           MOVE 1 TO LEV.
           PERFORM UPDATE-TABLE.
           PERFORM CREATE-WK.

           GO TO NEXT-TR-REPORT.

       END-OF-GROUP.
           PERFORM CLOSE-TRS1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-WK2-FILE.
           PERFORM OPEN-WK2-FILE-OUTPUT.

           IF ( LOCAL-PRINT-FG = "Y" )
              IF ( FIRST-TIME NOT = "Y" )
                 PERFORM CLASS-TOTALS
                 PERFORM BRANCH-TOTALS
                 MOVE "Y" TO FIRST-TIME
                 MOVE 0 TO PAGE-NUMBER
              ELSE
                 MOVE 0 TO PAGE-NUMBER.

           GO TO NEXT-GROUP-LOC.

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
           MOVE EMSFRG-QUEUE-POS TO Q-POS.
           MOVE EMSFRG-COPIES-POS TO C-POS.
           MOVE EMSFRG-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           MOVE SPACES TO RECORDS-FOUND.

           MOVE 1 TO SUB.

      *-----------------------------------------------------------------
       INIT-NEXT-TOTAL.
           PERFORM CLEAR-TOTAL-TBL.

           IF SUB < 3
              ADD 1 TO SUB
              GO TO INIT-NEXT-TOTAL.

           MOVE 0 TO PAGE-NUMBER.

      * CREATE SORT KEY FILE FOR TRFILE OUTPUT:

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT NOT = "00"
              MOVE "CANNOT CREATE SORT KEY FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           MOVE MKTEMP-BUF TO WK2-PATH.
           PERFORM OPEN-WK2-FILE-OUTPUT.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT NOT = "00"
              MOVE "CANNOT CREATE GL WORK FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BRANCH TO SV-BEG-BR
              MOVE END-BRANCH TO SV-END-BR
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP TO SV-ENT-GROUP
              MOVE LOCAL-PRINT-FG TO SV-LOCAL-PRINT-FG
              MOVE EXT-EOBUF-BRNO TO BEG-BRANCH END-BRANCH
              MOVE "N" TO GROUP-FLAG LOCAL-PRINT-FG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE
           ELSE
              IF ( BEG-BRANCH = 0 AND END-BRANCH = 0 )
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE "SALES REGISTER IN PROGRESS............" TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           IF LOCAL-PRINT-FG NOT = "Y"
              PERFORM OPEN-PR-FILE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           PERFORM GET-GPENV.

           IF ( GROUP-FLAG = "Y" )
              MOVE LOW-VALUES TO WS-GTT-TOTAL-TABLE
              MOVE ENT-GROUP TO GR-ID
              PERFORM SEARCH-GR-INIT
              GO TO EXIT-INITIALIZE
           ELSE
              MOVE SPACES TO TOTAL-LEV-TABLE
              MOVE "Y" TO TOTAL-LEVEL-TABLE(9).

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
              GO TO PRINTCOL.

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

           GO TO DL-01.

      *-----------------------------------------------------------------
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

       DL-01.
           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN042DL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-DL.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF STAT NOT = "00" AND NOT = "1D" GO TO DL-01.
           MOVE VDUNUM2 TO BEG-DLNO.

      *-----------------------------------------------------------------
       DL-02.
           MOVE "ENNN042DL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DL-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF STAT NOT = "00" AND NOT = "1D" GO TO DL-02.
           MOVE VDUNUM2 TO END-DLNO.
           IF BEG-DLNO > END-DLNO
              MOVE "INVALID DEALER RANGE!" TO MESS
              PERFORM SEND-MESS
              GO TO DL-01.

           GO TO DTE.

      *-----------------------------------------------------------------
       ALL-DL.
           MOVE "SNNN042DL1." TO VDU.
           MOVE 0 TO BEG-DLNO VDUNUM2.
           PERFORM SCREENIO.
           MOVE "SNNN042DL2." TO VDU.
           MOVE 9999.99 TO END-DLNO VDUNUM2.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
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
           IF VDUSTATUS = "1U" GO TO DL-02.
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
              GO TO GTT-ISO-PARAGRAPH.

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
           IF ( VDUSTATUS = "1U" ) GO TO DA-01.
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
              GO TO PRINTCOL.

           MOVE "E  A010SUMONLY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF ( VDUSTATUS = "1U" )
              IF ( GROUP-FLAG = "Y" )
                 GO TO GTT-ISO-PARAGRAPH.

           IF VDUSTATUS = "1U"
              IF EXT-ROUTE-BUF = "00"
                 IF ( DAY-OF-MONTH = 99 )
                    MOVE "SNNN020DA1." TO VDU
                    PERFORM SCREENIO
                    GO TO DTE.

           IF VDUSTATUS = "1U"
              IF EXT-ROUTE-BUF NOT = "00"
                 GO TO CL-02
              ELSE
                 GO TO DTE.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO SUMYN.
           MOVE VDUBUF TO SUM-ONLY.
           IF SUM-ONLY NOT = "N" AND NOT = "Y"
              GO TO SUMYN.

      *-----------------------------------------------------------------
       PRINTCOL.
           IF ( SUM-ONLY = "Y" )
              MOVE "S  A010PRCOL." TO VDU
              MOVE "N" TO PRCOL-FG VDUBUF
              PERFORM SCREENIO
              GO TO LOCAL-PRINT.
           MOVE LEGEND-PRINTCOL-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010PRCOL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO SUMYN.
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
                 IF ( GTT-ISO = "O" )
                    GO TO GTT-ISO-PARAGRAPH
                 ELSE
                    GO TO PRINTCOL
              ELSE
                  GO TO PRINTCOL.

           IF VDUSTATUS = "1U" GO TO PRINTCOL.
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

           MOVE "SNNN042DL1." TO VDU.
           MOVE BEG-DLNO TO VDUNUM2.
           PERFORM SCREENIO.
           MOVE "SNNN042DL2." TO VDU.
           MOVE END-DLNO TO VDUNUM2.
           PERFORM SCREENIO.

           MOVE "SNNN020DA1." TO VDU.
           MOVE DAY-OF-MONTH TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A010PRCOL." TO VDU.
           MOVE PRCOL-FG TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010GTT-ISO." TO VDU.
           MOVE GTT-ISO           TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010SUMONLY." TO VDU.
           MOVE SUM-ONLY TO VDUBUF.
           PERFORM SCREENIO.
      *-------------------------------------------------------------
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
       SORT-ROUTINE SECTION.
      ******************************************************************

           MOVE BR-NO               TO TS-BRNO
                                       QTRS1-WBEG-BRNO
                                       QTRS1-WEND-BRNO.
           MOVE BEG-DATE            TO TS-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRS1-WBEG-DATE.
           MOVE END-DATE            TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRS1-WEND-DATE.
           MOVE BEG-CLASS           TO TS-CLASS
                                       QTRS1-WBEG-CLASS.
           MOVE END-CLASS           TO QTRS1-WEND-CLASS.
           MOVE 0                   TO TS-ACCTNO TS-LAWCODE TS-LINE
                                       QTRS1-WBEG-ACCTNO
                                       QTRS1-WBEG-LAWCODE
                                       QTRS1-WBEG-LINE.
           MOVE ALL "9"             TO QTRS1-WEND-ACCTNO
                                       QTRS1-WEND-LAWCODE
                                       QTRS1-WEND-LINE.

           PERFORM START-TRS1-FILE.

       SORT-TR-NEXT.
           PERFORM READ-TRS1-FILE-NEXT.
           IF IO-FG NOT = 0 OR (TS-BRNO NOT = TRS-PATH-OWNBR)
              GO TO SORT-OUT.

           IF TS-DATE > END-DATE 
              GO TO SORT-OUT.

           IF TS-DATE < BEG-DATE 
              GO TO SORT-TR-NEXT.

           IF TS-CLASS < BEG-CLASS OR TS-CLASS > END-CLASS
              GO TO SORT-TR-NEXT.

      * IF BEG-DLNO = SPACES THEN THE RANGES ARE NEW AND THEY NEVER
      * ENTERED THEM, SO LET THEM DEFAULT TO ALL DEALERS LIKE IT USED
      * TO
           IF BEG-DLNOX NOT = SPACES
              IF TS-DLNO < BEG-DLNO OR TS-DLNO > END-DLNO
                 GO TO SORT-TR-NEXT.

           MOVE TS-DATE    TO WK2-DATE.
           MOVE TS-BRNO    TO WK2-BRANCH.
           MOVE TS-CLASS   TO WK2-CLASS.
           MOVE TS-ACCTNO  TO WK2-ACCTNO.
           MOVE TS-LAWCODE TO WK2-LAWCODE.
           MOVE TS-LINE    TO WK2-LINE.

           PERFORM WRITE-WK2-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO SORT-TR-NEXT.

       SORT-OUT.
           PERFORM CLOSE-WK2-FILE.

      ******************************************************************
       TR-DETAIL1 SECTION.
      ******************************************************************
           IF FIRST-TIME = "Y"
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE 1 TO LN-FEED
              MOVE "Y" TO RECORDS-FOUND
              MOVE "N" TO FIRST-TIME.

      * CLEAR DETAIL LINE HOLD TABLE PRIOR TO SETTING UP
      * THE DETAIL LINES

           PERFORM VARYING DTL-SUB FROM 1 BY 1 UNTIL DTL-SUB > 12
              MOVE SPACES TO DTL-LINE(DTL-SUB)
           END-PERFORM.

           MOVE 0 TO DTL-SUB.

           MOVE TS-CLASS   TO D-CLASS.
           MOVE "-"        TO D-H1.
           MOVE TS-ACCTNO  TO D-NUMBER.
           MOVE "-"        TO D-H2.
           MOVE TS-LAWCODE TO D-LAWCODE.
           MOVE TS-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-LNDTE.
           MOVE TS-DLNO     TO D-DLNO.

           MOVE TS-BRNO   TO LN-OWNBR.
           MOVE TS-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              MOVE 0 TO LN-APRATE.

           COMPUTE D-APR ROUNDED = LN-APRATE * 1.
           MOVE TS-SRCD TO D-SRCD.
           MOVE TS-COLLCD TO D-COLCD.
           MOVE TS-ORGTERM TO D-TERM.
           MOVE TS-LNAMT TO D-LNAMT.
           MOVE TS-FINCHG TO D-FINCHG.
           MOVE 1 TO I-SUB.
           PERFORM SELECT-INSURANCE.

           IF I-SUB < 7
              MOVE TS-INSPREM(I-SUB) TO D-INSPREM
              MOVE TS-INSOURS(I-SUB) TO D-INSOURS.

           IF TS-FEEAMT(1) NOT = 0 AND TS-FEEOURS(1) = "Y"
              MOVE TS-FEECODE(1) TO CDB-CODE
              PERFORM GET-CODE.

           IF TS-FEEAMT(1) NOT = 0
              MOVE TS-FEEAMT(1) TO D-FEEAMT
              MOVE TS-FEEOURS(1) TO D-FEEOURS.

           MOVE 0 TO T-RSVPRIN
                     T-RSVCHRG.

           PERFORM VARYING TSUB FROM 1 BY 1 UNTIL TSUB > 3
                ADD TS-DLRVPRIN(TSUB) TO T-RSVPRIN
                ADD TS-DLRVCHGS(TSUB) TO T-RSVCHRG
           END-PERFORM.

           MOVE T-RSVPRIN TO D-RSV1.
           MOVE TS-DLPARVHELD TO D-RSV2.
           MOVE TS-PROCEEDS TO D-PROCEEDS.
           IF SUM-ONLY = "N"
              ADD 1 TO DTL-SUB
              MOVE DETAIL-LINE TO DTL-LINE(DTL-SUB)
              MOVE SPACES TO DETAIL-LINE.

      * LINE 2
           MOVE TS-MAKERCD(1) TO D-MAKERCD.
           MOVE TS-SSNO(1) TO BW-SSNO.
           PERFORM MOVE-BORROWER.
           MOVE TS-PURCD TO D-COLCD.

           IF TS-ANTICTERM NOT = TS-ORGTERM
              MOVE TS-ANTICTERM TO D-TERM.

           IF TS-PURBAL NOT = TS-LNAMT
              MOVE TS-PURBAL TO D-LNAMT.

           IF GP-INCL-OTH-IN-EARNED-N
              IF TS-FINCHG NOT = TS-ANTICERN(1) + TS-ANTICERN(2) +
                                 TS-ANTICERN(3) + TS-ANTICERN(4) +
                                 TS-ANTICERN(5)
                 COMPUTE D-FINCHG = TS-ANTICERN(1) + TS-ANTICERN(2) +
                                    TS-ANTICERN(3) + TS-ANTICERN(4) +
                                    TS-ANTICERN(5)
              END-IF
           ELSE
           IF GP-INCL-OTH-IN-EARNED-R
              IF TS-FINCHG NOT = TS-ANTICERN(1) + TS-ANTICERN(2) +
                                 TS-ANTICERN(3) + TS-ANTICERN(5)
                 COMPUTE D-FINCHG = TS-ANTICERN(1) + TS-ANTICERN(2) +
                                    TS-ANTICERN(3) + TS-ANTICERN(5)
              END-IF

           ELSE
              IF TS-FINCHG NOT = TS-ANTICERN(1) + TS-ANTICERN(2) +
                                 TS-ANTICERN(3) + TS-ANTICERN(4) +
                                 TS-ANTICERN(5) + TS-ANTICERN(6)
                 COMPUTE D-FINCHG = TS-ANTICERN(1) + TS-ANTICERN(2) +
                                    TS-ANTICERN(3) + TS-ANTICERN(4) +
                                    TS-ANTICERN(5) + TS-ANTICERN(6).

           ADD 1 TO I-SUB.

           IF I-SUB < 7
              PERFORM SELECT-INSURANCE
              IF I-SUB < 7
                 MOVE TS-INSPREM(I-SUB) TO D-INSPREM
                 MOVE TS-INSOURS(I-SUB) TO D-INSOURS.

           IF TS-FEEAMT(2) NOT = 0 AND TS-FEEOURS(2) = "Y"
              MOVE TS-FEECODE(2) TO CDB-CODE
              PERFORM GET-CODE.

           IF TS-FEEAMT(2) NOT = 0
              MOVE TS-FEEAMT(2) TO D-FEEAMT
              MOVE TS-FEEOURS(2) TO D-FEEOURS.

           MOVE T-RSVCHRG TO D-RSV1.
           MOVE TS-DLPARVPAID TO D-RSV2.
           COMPUTE D-PROCEEDS = TS-EARNINGS(1) + TS-EARNINGS(2) +
                                TS-EARNINGS(3) + TS-EARNINGS(4) +
                                TS-EARNINGS(5) + TS-EARNINGS(6).

           IF SUM-ONLY = "N"
            IF DETAIL-LINE NOT = SPACES
              ADD 1 TO DTL-SUB
              MOVE DETAIL-LINE TO DTL-LINE(DTL-SUB)
              MOVE SPACES TO DETAIL-LINE.

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

           MOVE TS-TILLNO              TO D-TILLNO.
           MOVE TS-UNWIND-REASON-CODE  TO D-UNWIND-CODE.
           MOVE TS-REGPYAMT TO D-REGPYAMT.

           ADD 1 TO I-SUB.

           IF I-SUB < 7
              PERFORM SELECT-INSURANCE
              IF I-SUB < 7
                 MOVE TS-INSPREM(I-SUB) TO D-INSPREM
                 MOVE TS-INSOURS(I-SUB) TO D-INSOURS.

           IF TS-FEEAMT(3) NOT = 0 AND TS-FEEOURS(3) = "Y"
              MOVE TS-FEECODE(3) TO CDB-CODE
              PERFORM GET-CODE.

           IF TS-FEEAMT(3) NOT = 0
              MOVE TS-FEEAMT(3) TO D-FEEAMT
              MOVE TS-FEEOURS(3) TO D-FEEOURS.

           MOVE TS-DLDISC TO D-RSV1.
           MOVE TS-DLPAID TO D-DLPAID.
           MOVE TS-INSPREM-DLADJ TO D-PROCEEDS.

           IF SUM-ONLY = "N"
            IF DETAIL-LINE NOT = SPACES
              ADD 1 TO DTL-SUB
              MOVE DETAIL-LINE TO DTL-LINE(DTL-SUB)
              MOVE SPACES TO DETAIL-LINE.

      * LINE4
           MOVE TS-MAKERCD(2) TO D-MAKERCD.
           MOVE TS-SSNO(2) TO BW-SSNO.
           PERFORM MOVE-BORROWER.
           ADD 1 TO I-SUB.

           IF I-SUB < 7
              PERFORM SELECT-INSURANCE
              IF I-SUB < 7
                 MOVE TS-INSPREM(I-SUB) TO D-INSPREM
                 MOVE TS-INSOURS(I-SUB) TO D-INSOURS.

           IF TS-FEEAMT(4) NOT = 0 AND TS-FEEOURS(4) = "Y"
              MOVE TS-FEECODE(4) TO CDB-CODE
              PERFORM GET-CODE.

           IF TS-FEEAMT(4) NOT = 0
              MOVE TS-FEEAMT(4) TO D-FEEAMT
              MOVE TS-FEEOURS(4) TO D-FEEOURS.

           IF GP-DLCOMM-CMSI-FG = "Y"
              MOVE TS-DLCOMM-PP-CMSI TO D-DLCOMM-PP-CMSI
              MOVE TS-DLCOMM-O3-CMSI TO D-DLCOMM-O3-CMSI.

           IF SUM-ONLY = "N"
            IF DETAIL-LINE NOT = SPACES
              ADD 1 TO DTL-SUB
              MOVE DETAIL-LINE TO DTL-LINE(DTL-SUB)
              MOVE SPACES TO DETAIL-LINE.

      * LINE5
           MOVE TS-MAKERCD(3) TO D-MAKERCD.
           MOVE TS-SSNO(3) TO BW-SSNO.
           PERFORM MOVE-BORROWER.
           ADD 1 TO I-SUB.

           IF I-SUB < 7
              PERFORM SELECT-INSURANCE
              IF I-SUB < 7
                 MOVE TS-INSPREM(I-SUB) TO D-INSPREM
                 MOVE TS-INSOURS(I-SUB) TO D-INSOURS.

           IF TS-FEEAMT(5) NOT = 0 AND TS-FEEOURS(5) = "Y"
              MOVE TS-FEECODE(5) TO CDB-CODE
              PERFORM GET-CODE.

           IF TS-FEEAMT(5) NOT = 0
              MOVE TS-FEEAMT(5) TO D-FEEAMT
              MOVE TS-FEEOURS(5) TO D-FEEOURS.

           IF SUM-ONLY = "N"
            IF DETAIL-LINE NOT = SPACES
              ADD 1 TO DTL-SUB
              MOVE DETAIL-LINE TO DTL-LINE(DTL-SUB)
              MOVE SPACES TO DETAIL-LINE.

      * LINE6
           MOVE TS-MAKERCD(4) TO D-MAKERCD.
           MOVE TS-SSNO(4) TO BW-SSNO.
           PERFORM MOVE-BORROWER.
           ADD 1 TO I-SUB.

           IF I-SUB < 7
              PERFORM SELECT-INSURANCE
              IF I-SUB < 7
                 MOVE TS-INSPREM(I-SUB) TO D-INSPREM
                 MOVE TS-INSOURS(I-SUB) TO D-INSOURS.

           IF TS-FEEAMT(6) NOT = 0 AND TS-FEEOURS(6) = "Y"
              MOVE TS-FEECODE(6) TO CDB-CODE
              PERFORM GET-CODE.

           IF TS-FEEAMT(6) NOT = 0
              MOVE TS-FEEAMT(6) TO D-FEEAMT
              MOVE TS-FEEOURS(6) TO D-FEEOURS.

           IF SUM-ONLY = "N"
            IF DETAIL-LINE NOT = SPACES
              ADD 1 TO DTL-SUB
              MOVE DETAIL-LINE TO DTL-LINE(DTL-SUB)
              MOVE SPACES TO DETAIL-LINE.

      * LINE7
           IF TS-FEEAMT(7) NOT = 0 AND TS-FEEOURS(7) = "Y"
              MOVE TS-FEECODE(7) TO CDB-CODE
              PERFORM GET-CODE.

           IF TS-FEEAMT(7) NOT = 0
              MOVE TS-FEEAMT(7) TO D-FEEAMT
              MOVE TS-FEEOURS(7) TO D-FEEOURS.

           IF SUM-ONLY = "N"
            IF DETAIL-LINE NOT = SPACES
              ADD 1 TO DTL-SUB
              MOVE DETAIL-LINE TO DTL-LINE(DTL-SUB)
              MOVE SPACES TO DETAIL-LINE.

      * LINE8
           IF TS-FEEAMT(8) NOT = 0 AND TS-FEEOURS(8) = "Y"
              MOVE TS-FEECODE(8) TO CDB-CODE
              PERFORM GET-CODE.

           IF TS-FEEAMT(8) NOT = 0
              MOVE TS-FEEAMT(8) TO D-FEEAMT
              MOVE TS-FEEOURS(8) TO D-FEEOURS.

           IF SUM-ONLY = "N"
            IF DETAIL-LINE NOT = SPACES
              ADD 1 TO DTL-SUB
              MOVE DETAIL-LINE TO DTL-LINE(DTL-SUB)
              MOVE SPACES TO DETAIL-LINE.

      * LINE9
           IF TS-FEEAMT(9) NOT = 0 AND TS-FEEOURS(9) = "Y"
              MOVE TS-FEECODE(9) TO CDB-CODE
              PERFORM GET-CODE.

           IF TS-FEEAMT(9) NOT = 0
              MOVE TS-FEEAMT(9) TO D-FEEAMT
              MOVE TS-FEEOURS(9) TO D-FEEOURS.

           IF SUM-ONLY = "N"
            IF DETAIL-LINE NOT = SPACES
              ADD 1 TO DTL-SUB
              MOVE DETAIL-LINE TO DTL-LINE(DTL-SUB)
              MOVE SPACES TO DETAIL-LINE.

      * LINE 10
           IF TS-FEEAMT(10) NOT = 0 AND TS-FEEOURS(10) = "Y"
              MOVE TS-FEECODE(10) TO CDB-CODE
              PERFORM GET-CODE.

           IF TS-FEEAMT(10) NOT = 0
              MOVE TS-FEEAMT(10) TO D-FEEAMT
              MOVE TS-FEEOURS(10) TO D-FEEOURS.

           IF SUM-ONLY = "N"
            IF DETAIL-LINE NOT = SPACES
              ADD 1 TO DTL-SUB
              MOVE DETAIL-LINE TO DTL-LINE(DTL-SUB)
              MOVE SPACES TO DETAIL-LINE.

      * LINE 11 & 12
           IF SUM-ONLY = "N"
              PERFORM PRINT-LT-REC-COL.

      *    -------------------------------------------------------------
      *    IF DETAIL LINES FOR ACCOUNT WILL EXCEED PAGE END,
      *       THEN ADVANCE TO NEXT PAGE TO PRINT FIRST LINE
      *       ( NOTE: NEED THE +1 BECAUSE WE ARE SKIPPING A LINE
      *               BEFORE PRINTING FIRST LINE FOR EACH ACCOUNT
      *               [I.E. MOVE 2 TO LN-FEED])
      *    -------------------------------------------------------------
           IF (DTL-SUB + LN-COUNT + 1) > EXT-CONAME-RPT-LINES
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.

      *    -------------------------------------------------------------
      *    PRINT ALL DETAIL-LINES FOR THE ACCOUNT.
      *    -------------------------------------------------------------
           MOVE 2 TO LN-FEED.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > DTL-SUB
              MOVE DTL-LINE(SUB) TO DETAIL-LINE
              PERFORM WRITE-DETAIL-LINE
           END-PERFORM.

           IF SUM-ONLY = "Y"
              MOVE SPACES TO DETAIL-LINE.

       END-TR1-DETAIL.
           EXIT.

      ******************************************************************
       CREATE-WK SECTION.
      ******************************************************************
           MOVE TS-SRCD  TO SAVE-WK-SRCD.
           MOVE TS-BRNO  TO SAVE-WK-BR.
           MOVE TS-CLASS TO SAVE-WK-CLASS.

       SAVE-WORK-KEY.
           MOVE SAVE-WK-KEY TO WK-KEY.
           PERFORM READ-WK-FILE.
           IF IO-FG = 0
              GO TO CREATE-WK-UPDATE.

           MOVE SPACES      TO WK-REC.
           MOVE SAVE-WK-KEY TO WK-KEY.

           MOVE 0 TO WK-NOLOANS WK-LNAMT WK-INTCHG WK-SERCHG
                     WK-EXTCHG WK-MFEE WK-EARNINGS WK-FINCHG
                     WK-DL WK-LL WK-AH WK-PP WK-O1 WK-O2 WK-O3
                     WK-FEES
                     WK-DLRVPRIN(1) WK-DLRVPRIN(2) WK-DLRVPRIN(3)
                     WK-DLRVCHGS(1) WK-DLRVCHGS(2) WK-DLRVCHGS(3)
                     WK-DLPARVHELD WK-DLPARVPAID WK-DLDISC
                     WK-PROCEEDS WK-PURBAL WK-ANTICERN
                     WK-INSPREM-DLADJ
                     WK-X-LNAMT WK-SUM-OF-PMT WK-DLCOMM-PP-CMSI
                     WK-DLCOMM-O3-CMSI.

           PERFORM WRITE-WK-FILE.

           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO SAVE-WORK-KEY.

       CREATE-WK-UPDATE.
           IF TS-LNAMT < 0
              SUBTRACT 1 FROM WK-NOLOANS
           ELSE
              ADD 1 TO WK-NOLOANS.

           ADD TS-LNAMT TO WK-LNAMT.
           ADD TS-FINCHG TO WK-FINCHG.
           ADD TS-INTCHG TO WK-INTCHG.
           ADD TS-SERCHG TO WK-SERCHG.
           ADD TS-EXTCHG TO WK-EXTCHG.

           COMPUTE WK-MFEE ROUNDED = WK-MFEE
                                   + (TS-MAINTFEE * TS-ORGTERM).

           COMPUTE WK-EARNINGS = WK-EARNINGS +
                                 TS-EARNINGS(1) + TS-EARNINGS(2) +
                                 TS-EARNINGS(3) + TS-EARNINGS(4) +
                                 TS-EARNINGS(5) + TS-EARNINGS(6).

           IF TS-INSTYPES-DL
              IF TS-INSOURS(1) = "Y"
                 ADD TS-INSPREM(1) TO WK-DL.

           IF NOT TS-INSTYPES-DL
              IF TS-INSOURS(1) = "Y"
                 ADD TS-INSPREM(1) TO WK-LL.

           IF TS-INSOURS(2) = "Y"
              ADD TS-INSPREM(2) TO WK-AH.

           IF TS-INSOURS(3) = "Y"
              ADD TS-INSPREM(3) TO WK-PP.

           IF TS-INSOURS(4) = "Y"
              ADD TS-INSPREM(4) TO WK-O1.

           IF TS-INSOURS(5) = "Y"
              ADD TS-INSPREM(5) TO WK-O2.

           IF TS-INSOURS(6) = "Y"
              ADD TS-INSPREM(6) TO WK-O3.

           MOVE 1 TO I-SUB.

      *-----------------------------------------------------------------
       CREATE-WK-FEES.
           IF TS-FEEOURS(I-SUB) = "Y"
              ADD TS-FEEAMT(I-SUB) TO WK-FEES.

           IF I-SUB < 10
              ADD 1 TO I-SUB
              GO TO CREATE-WK-FEES.

           ADD TS-DLRVPRIN(1) TO WK-DLRVPRIN(1).
           ADD TS-DLRVPRIN(2) TO WK-DLRVPRIN(2).
           ADD TS-DLRVPRIN(3) TO WK-DLRVPRIN(3).
           ADD TS-DLRVCHGS(1) TO WK-DLRVCHGS(1).
           ADD TS-DLRVCHGS(2) TO WK-DLRVCHGS(2).
           ADD TS-DLRVCHGS(3) TO WK-DLRVCHGS(3).
           ADD TS-DLPARVHELD TO WK-DLPARVHELD.
           ADD TS-DLPARVPAID TO WK-DLPARVPAID.
           ADD TS-DLDISC TO WK-DLDISC.
           ADD TS-INSPREM-DLADJ TO WK-INSPREM-DLADJ.
           ADD TS-PROCEEDS TO WK-PROCEEDS.
           ADD TS-PURBAL TO WK-PURBAL.

           IF TS-LOANTYPE = "I"
              ADD TS-FINCHG, TS-LNAMT TO WK-X-LNAMT
           ELSE
              ADD  TS-LNAMT TO WK-X-LNAMT.

           ADD TS-REGPYAMT TO WK-SUM-OF-PMT.

           COMPUTE WK-ANTICERN = WK-ANTICERN +
                                 TS-ANTICERN(1) + TS-ANTICERN(2) +
                                 TS-ANTICERN(3) + TS-ANTICERN(5).

           IF NOT( GP-INCL-OTH-IN-EARNED-N OR GP-INCL-OTH-IN-EARNED-R)
              ADD TS-ANTICERN(6) TO WK-ANTICERN.

           IF NOT (GP-INCL-OTH-IN-EARNED-R)
              ADD TS-ANTICERN(4) TO WK-ANTICERN.

           IF GP-DLCOMM-CMSI-FG = "Y"
              ADD TS-DLCOMM-PP-CMSI TO WK-DLCOMM-PP-CMSI
              ADD TS-DLCOMM-O3-CMSI TO WK-DLCOMM-O3-CMSI.

           PERFORM REWRITE-WK-FILE.

       EXIT-CREATE-WK.
           EXIT.

      ******************************************************************
       MOVE-BORROWER SECTION.
      ******************************************************************

           IF BW-SSNO = 0
              GO TO EXIT-MOVE-BORROWER.

           MOVE SPACES     TO D-BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG = 9
              MOVE SPACES TO BW-REC
              MOVE "* * NOT FOUND * *" TO BW-LNAME
              MOVE 0 TO BW-CREDIT-SCORE BW-PHONE BW-EMPPHONE
                        BW-EMPEXT.

           MOVE SPACES TO D-BW-NAME.
           STRING BW-LNAME," ",BW-FNAME DELIMITED BY "   "
               INTO D-BW-NAME.

           IF BR-CREDIT-SCORING-FG = "Y"
              MOVE BW-CREDIT-SCORE TO D-CRED-SCORE.

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
           PERFORM READ-LC1-FILE.

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
           MOVE HOLD-BRNO TO WK-BR.
           MOVE HOLD-CLASS TO WK-CLASS.
           MOVE SPACES TO WK-CODE.
           PERFORM START-WK-FILE.

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

           MOVE WK-CODE TO G-DCODE.
           MOVE ":" TO G-COLON.

      * TOTALS TAKE UP FIVE LINES PER CODE.
      * MAKE SURE ALL LINES, PER CODE, PRINT ON THE SAME PAGE

           IF (LN-COUNT + 5) > EXT-CONAME-RPT-LINES
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.

      * LINE 1
           MOVE "LOANS   :"   TO G-HEADING1
           MOVE WK-NOLOANS    TO G-LOANS.
           MOVE "INTCHG  :"   TO G-HEADING2
           MOVE WK-INTCHG     TO G-AMOUNT-2
           MOVE "OUR-LL  : "  TO G-HEADING3.
           MOVE WK-LL         TO G-AMOUNT-3.
           MOVE "OUR-O3  :"   TO G-HEADING4.
           MOVE WK-O3         TO G-AMOUNT-4.
           MOVE "PARHELD :"   TO G-HEADING5.
           MOVE WK-DLPARVHELD TO G-AMOUNT-5.
           MOVE 2             TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * LINE 2
           MOVE "LOAN-AMT:"   TO G-HEADING1
           MOVE WK-LNAMT      TO G-AMOUNT-1
           MOVE "SRVCHRG :"   TO G-HEADING2
           MOVE WK-SERCHG     TO G-AMOUNT-2
           MOVE "OUR-AH  :"   TO G-HEADING3.
           MOVE WK-AH         TO G-AMOUNT-3.
           MOVE "OUR FEES:"   TO G-HEADING4.
           MOVE WK-FEES       TO G-AMOUNT-4.
           MOVE "PARPAID :"   TO G-HEADING5.
           MOVE WK-DLPARVPAID TO G-AMOUNT-5.
           MOVE 1             TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * LINE 3
           MOVE "PUR-AMT :"   TO G-HEADING1
           IF WK-PURBAL NOT = WK-LNAMT
              MOVE WK-PURBAL  TO G-AMOUNT-1
           ELSE
              MOVE ZEROS      TO G-AMOUNT-1
           MOVE "EXTCHG  : "  TO G-HEADING2
           MOVE WK-EXTCHG     TO G-AMOUNT-2
           MOVE "OUR-PP  : "  TO G-HEADING3.
           MOVE WK-PP         TO G-AMOUNT-3.
           MOVE "RSVPRIN :"   TO G-HEADING4.
           MOVE 0 TO T-WK-DLRVCHGS
                     T-WK-DLRVPRIN.
           PERFORM VARYING TSUB FROM 1 BY 1
                            UNTIL TSUB > 3
                   ADD WK-DLRVPRIN (TSUB) TO T-WK-DLRVPRIN
                   ADD WK-DLRVCHGS (TSUB) TO T-WK-DLRVCHGS
           END-PERFORM.
           MOVE T-WK-DLRVPRIN TO G-AMOUNT-4.
           MOVE "EARNINGS:"   TO G-HEADING5.
           MOVE WK-EARNINGS   TO G-AMOUNT-5.
           MOVE 1             TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * LINE 4
           MOVE "FIN-CHG :"   TO G-HEADING1
           MOVE WK-FINCHG     TO G-AMOUNT-1
           MOVE "MFEE    :"   TO G-HEADING2
           MOVE WK-MFEE       TO G-AMOUNT-2
           MOVE "OUR-O1  :"   TO G-HEADING3.
           MOVE WK-O1         TO G-AMOUNT-3.
           MOVE "RSVCHRG :"   TO G-HEADING4.
           MOVE T-WK-DLRVCHGS TO G-AMOUNT-4.
           MOVE "PROCEEDS:"   TO G-HEADING5.
           MOVE WK-PROCEEDS   TO G-AMOUNT-5.
           MOVE 1             TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * LINE 5
           MOVE "ANTICERN:"    TO G-HEADING1
      * ALWAYS PRINT THE ANTICERN PR#4084
           MOVE WK-ANTICERN TO G-AMOUNT-1.

           MOVE "OUR-DL  : "     TO G-HEADING2.
           MOVE WK-DL            TO G-AMOUNT-2.
           MOVE "OUR-O2  :"      TO G-HEADING3.
           MOVE WK-O2            TO G-AMOUNT-3.
           MOVE "COMM    :"      TO G-HEADING4.
           MOVE WK-DLDISC        TO G-AMOUNT-4.
           MOVE "PREM-ADJ:"      TO G-HEADING5.
           MOVE WK-INSPREM-DLADJ TO G-AMOUNT-5.
           MOVE 1                TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.


           IF GP-DLCOMM-CMSI-FG = "Y"
              MOVE "DLCOMPP :"   TO G-HEADING4
              MOVE WK-DLCOMM-PP-CMSI TO G-AMOUNT-4
              MOVE "DLCOMO3 :"   TO G-HEADING5
              MOVE WK-DLCOMM-O3-CMSI TO G-AMOUNT-5
              MOVE 1 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

           GO TO PRINT-NEXT-CODE.

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
           MOVE "BRANCH TOTALS:" TO G-TOTNAME.
           PERFORM PRINT-SUMMARY.

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
           PERFORM PRINT-GRAND-TOTALS.

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
           MOVE HEAD3 TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE HEAD4 TO PR-REC.
           PERFORM WRITE-PR-REC.

           IF BR-CREDIT-SCORING-FG = "Y"
              MOVE "CRD " TO H-CRED-SCORE
           ELSE
              MOVE SPACES TO H-CRED-SCORE.
           IF ( GTT-HEADER-FG = "Y" )
              MOVE GTT-HEAD1 TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC
              MOVE GTT-HEAD2 TO PR-REC
              MOVE 1 TO LN-FEED
              PERFORM WRITE-PR-REC
              MOVE ALL "-" TO PR-REC
              PERFORM WRITE-PR-REC
              MOVE 9 TO LN-COUNT
              MOVE 1 TO LN-FEED
           ELSE
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
              MOVE HEAD7 TO PR-REC
              MOVE 1 TO LN-FEED
              PERFORM WRITE-PR-REC
              IF GP-DLCOMM-CMSI-FG = "Y"
                 MOVE HEAD11 TO PR-REC
                 MOVE 1 TO LN-FEED
                 PERFORM WRITE-PR-REC
                 MOVE 9 TO LN-COUNT
                 MOVE 2 TO LN-FEED
              ELSE
                 MOVE 7 TO LN-COUNT
                 MOVE 2 TO LN-FEED
           ELSE
              MOVE 4 TO LN-COUNT
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

           MOVE "BEG DEALER" TO RANGE-LINE.
           MOVE BEG-DLNO TO R-DLNO.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "END DEALER" TO RANGE-LINE.
           MOVE END-DLNO TO R-DLNO.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MONTH-END DATE" TO RANGE-LINE.
           MOVE EDATE TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "DAY OF MONTH" TO RANGE-LINE.
           MOVE DAY-OF-MONTH TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "SUMMARY ONLY" TO RANGE-LINE.
           MOVE SUM-ONLY TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "PRINT COLLATERAL" TO RANGE-LINE.
           MOVE PRCOL-FG TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "GROUP TOTALS" TO RANGE-LINE.
           MOVE GTT-ISO        TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.


      ******************************************************
       SELECT-INSURANCE SECTION.
      ******************************************************
       SELECT-IT.

           IF I-SUB > 6
              GO TO EXIT-SELECT-INSURANCE.

           IF TS-INSPREM(I-SUB) = 0
              ADD 1 TO I-SUB
              GO TO SELECT-IT.

           IF I-SUB = 1
              PERFORM GET-CREDIT-LIFE-TAG
           ELSE
           IF I-SUB = 2
              MOVE "A" TO D-INSTYPE
           ELSE
           IF I-SUB = 3
              MOVE "P" TO D-INSTYPE
           ELSE
           IF I-SUB = 4
              MOVE "1" TO D-INSTYPE
           ELSE
           IF I-SUB = 5
              MOVE "2" TO D-INSTYPE
           ELSE
           IF I-SUB = 6
              MOVE "3" TO D-INSTYPE.

       EXIT-SELECT-INSURANCE.
           EXIT.

      ******************************************************
       CLASS-SCAN SECTION.
      ******************************************************
           MOVE 0      TO LCSCAN-KEY.
           MOVE SPACES TO LCSCAN-VERIFY.
           MOVE "Y"    TO LCSCAN-SALEFIN.
           MOVE "N"    TO LCSCAN-SERVACCT.
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
           ADD WK-NOLOANS     TO  WS-GTT-NOLOANS   ( GTT-TOTAL-SUB ).
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
           ADD WK-INSPREM-DLADJ TO
                             WS-GTT-INSPREM-DLADJ ( GTT-TOTAL-SUB ).
           ADD WK-X-LNAMT     TO WS-GTT-X-LNAMT(GTT-TOTAL-SUB).
           ADD WK-SUM-OF-PMT  TO WS-GTT-SUM-OF-PMT(GTT-TOTAL-SUB).
           IF GP-DLCOMM-CMSI-FG = "Y"
              ADD WK-DLCOMM-PP-CMSI TO
                                 WS-GTT-DLCOMM-PP-CMSI(GTT-TOTAL-SUB)
              ADD WK-DLCOMM-O3-CMSI TO
                                 WS-GTT-DLCOMM-O3-CMSI(GTT-TOTAL-SUB).

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
           MOVE "BRANCH:" TO G-PROMPT1 WS-HOLD-PROMPT.
           MOVE BR-NO     TO G-BRNO.

           PERFORM GTT-PRINT-TOTALS.

       EXIT-GTT-PRINT-GROUP-BR-TOTALS.
           EXIT.

      ******************************************************************
       GTT-PRINT-GROUP-TOTALS SECTION.
      ******************************************************************

           MOVE SPACES    TO DETAIL-LINE.
           MOVE "GROUP :" TO G-PROMPT1 WS-HOLD-PROMPT.
           MOVE GR-ID     TO G-GRNO.
           MOVE GR-LEVEL  TO G-GR-LEVEL.

           PERFORM GTT-PRINT-TOTALS.

       EXIT-GTT-PRINT-GROUP-TOTALS.
           EXIT.

      ******************************************************************
       GTT-PRINT-TOTALS SECTION.
      ******************************************************************

      * TOTALS TAKE UP SIX LINES PER CODE.
      * MAKE SURE ALL LINES, PER BRANCH/GROUP, PRINT ON THE SAME PAGE

           IF (LN-COUNT + 6) > EXT-CONAME-RPT-LINES
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.

      * LINE 1
           MOVE "LOANS   :"                      TO G-HEADING1.
           MOVE WS-GTT-NOLOANS(GTT-TOTAL-SUB)    TO G-LOANS.
           MOVE "INTCHG  :"                      TO G-HEADING2.
           MOVE WS-GTT-INTCHG(GTT-TOTAL-SUB)     TO G-AMOUNT-2.
           MOVE "OUR-LL  : "                     TO G-HEADING3.
           MOVE WS-GTT-LL(GTT-TOTAL-SUB)         TO G-AMOUNT-3.
           MOVE "OUR-O3  :"                      TO G-HEADING4.
           MOVE WS-GTT-O3(GTT-TOTAL-SUB)         TO G-AMOUNT-4.
           MOVE "PARHELD :"                      TO G-HEADING5.
           MOVE WS-GTT-DLPARVHELD(GTT-TOTAL-SUB) TO G-AMOUNT-5.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * LINE 2
           MOVE "LOAN-AMT:"                      TO G-HEADING1.
           MOVE WS-GTT-LNAMT(GTT-TOTAL-SUB)      TO G-AMOUNT-1.
           MOVE "SRVCHRG :"                      TO G-HEADING2.
           MOVE WS-GTT-SERCHG(GTT-TOTAL-SUB)     TO G-AMOUNT-2.
           MOVE "OUR-AH  :"                      TO G-HEADING3.
           MOVE WS-GTT-AH(GTT-TOTAL-SUB)         TO G-AMOUNT-3.
           MOVE "OUR FEES:"                      TO G-HEADING4.
           MOVE WS-GTT-FEES(GTT-TOTAL-SUB)       TO G-AMOUNT-4.
           MOVE "PARPAID :"                      TO G-HEADING5.
           MOVE WS-GTT-DLPARVPAID(GTT-TOTAL-SUB) TO G-AMOUNT-5.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * LINE 3
           MOVE "PUR-AMT :" TO G-HEADING1.
           IF WS-GTT-PURBAL(GTT-TOTAL-SUB) NOT =
                                  WS-GTT-LNAMT(GTT-TOTAL-SUB)
              MOVE WS-GTT-PURBAL(GTT-TOTAL-SUB)  TO G-AMOUNT-1
           ELSE
              MOVE ZEROS                         TO G-AMOUNT-1.
           MOVE "EXTCHG  : "                     TO G-HEADING2.
           MOVE WS-GTT-EXTCHG(GTT-TOTAL-SUB)     TO G-AMOUNT-2.
           MOVE "OUR-PP  : "                     TO G-HEADING3.
           MOVE WS-GTT-PP(GTT-TOTAL-SUB)         TO G-AMOUNT-3.
           MOVE "RSVPRIN :"                      TO G-HEADING4.
           MOVE WS-GTT-DLRVPRIN(GTT-TOTAL-SUB)   TO G-AMOUNT-4.
           MOVE "EARNINGS:"                      TO G-HEADING5.
           MOVE WS-GTT-EARNINGS(GTT-TOTAL-SUB)   TO G-AMOUNT-5.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * LINE 4
           MOVE "FIN-CHG :"                      TO G-HEADING1.
           MOVE WS-GTT-FINCHG(GTT-TOTAL-SUB)     TO G-AMOUNT-1.
           MOVE "MFEE    :"                      TO G-HEADING2.
           MOVE WS-GTT-MFEE(GTT-TOTAL-SUB)       TO G-AMOUNT-2.
           MOVE "OUR-O1  :"                      TO G-HEADING3.
           MOVE WS-GTT-O1(GTT-TOTAL-SUB)         TO G-AMOUNT-3.
           MOVE "RSVCHRG :"                      TO G-HEADING4.
           MOVE WS-GTT-DLRVCHGS(GTT-TOTAL-SUB)   TO G-AMOUNT-4.
           MOVE "PROCEEDS:"                      TO G-HEADING5.
           MOVE WS-GTT-PROCEEDS(GTT-TOTAL-SUB)   TO G-AMOUNT-5.
           MOVE 1                                TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * LINE 5
           MOVE "ANTICERN:"                         TO G-HEADING1.
      * ALWAYS PRINT ANTICERN
           MOVE WS-GTT-ANTICERN(GTT-TOTAL-SUB)      TO G-AMOUNT-1.

           MOVE "OUR-DL  : "                        TO G-HEADING2.
           MOVE WS-GTT-DL(GTT-TOTAL-SUB)            TO G-AMOUNT-2.
           MOVE "OUR-O2  :"                         TO G-HEADING3.
           MOVE WS-GTT-O2(GTT-TOTAL-SUB)            TO G-AMOUNT-3.
           MOVE "COMM    :"                         TO G-HEADING4.
           MOVE WS-GTT-DLDISC(GTT-TOTAL-SUB)        TO G-AMOUNT-4.
           MOVE "PREM-ADJ:"                         TO G-HEADING5.
           MOVE WS-GTT-INSPREM-DLADJ(GTT-TOTAL-SUB) TO G-AMOUNT-5.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * LINE 6
           MOVE "AVG TERM:"       TO G-HEADING1.
           IF WS-GTT-LNAMT(GTT-TOTAL-SUB) NOT = 0
              IF WS-GTT-LNAMT(GTT-TOTAL-SUB) NOT < 0
                 COMPUTE AVGTERM = WS-GTT-X-LNAMT(GTT-TOTAL-SUB) /
                                   WS-GTT-SUM-OF-PMT(GTT-TOTAL-SUB)
                 MOVE AVGTERM TO G-AMOUNT-1
              ELSE
                 COMPUTE AVGTERM = (WS-GTT-X-LNAMT(GTT-TOTAL-SUB) /
                           WS-GTT-SUM-OF-PMT(GTT-TOTAL-SUB)) * -1
                 MOVE AVGTERM TO G-AMOUNT-5.

      
           IF GP-DLCOMM-CMSI-FG = "Y"
              MOVE "DLCOMPP :"    TO G-HEADING4
              MOVE WS-GTT-DLCOMM-PP-CMSI(GTT-TOTAL-SUB) TO G-AMOUNT-4
      
              MOVE "DLCOMO3 :"    TO G-HEADING5
              MOVE WS-GTT-DLCOMM-O3-CMSI(GTT-TOTAL-SUB) TO G-AMOUNT-5.


           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF DETAIL-LINE NOT= SPACES
               PERFORM WRITE-DETAIL-LINE.

      * PRINT DETAIL AFTER LOAD 
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( WS-HOLD-PROMPT = "GROUP :" )
              MOVE ALL "-" TO DETAIL-LINE
              PERFORM WRITE-DETAIL-LINE.

      * CLEAR LEVEL IN TABLE 
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

      *******************************************************************
       FORM-RESET SECTION.
      *******************************************************************
           MOVE EXT-CONAME-CONAME TO EMSFRG-CONAME.
           MOVE EXT-CONAME-SYSDATE TO EMSFRG-SYSDATE.
           DISPLAY EMSFRG-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE EMSFRG-FORM-FIELDS TO WR-BUF.
           MOVE EMSFRG-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.


      *******************************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************************
           MOVE EMSFRG-HELP--FILE TO HELP-LINK-FILE.
           MOVE EMSFRG-HELP--DIR TO HELP-LINK-DIR.

      *******************************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/EMSFRG_EVA.CPY".
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
                     T-PROCEEDS(SUB) T-ANTICERN(SUB)
                     T-INSPREM-DLADJ(SUB)
                     T-X-LNAMT(SUB)
                     T-SUM-OF-PMT(SUB) T-DLCOMM-PP-CMSI(SUB)
                     T-DLCOMM-O3-CMSI(SUB).

      *-----------------------------------------------------------------
       CAT-CSZ.
           INSPECT C-S-Z REPLACING FIRST "      " BY ", ++ +".
           INSPECT C-S-Z REPLACING FIRST "++" BY STATE.
           INSPECT C-S-Z REPLACING FIRST "+         " BY ZIP.

      *-----------------------------------------------------------------
       GET-CODE.
           MOVE "GC"    TO CDB-TYPE.
           MOVE TS-BRNO TO CDB-BRANCH.
      *--> SET BEFORE      CDB-CODE.
           PERFORM OPEN-CDB1-FILE.
           PERFORM READ-CDB1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           IF IO-FG NOT = 0
              MOVE 0 TO CDB-GC-GLFEENO.

           IF LC-USE-ALT-GL-FEE = "Y"
              MOVE CDB-GC-ALT-GLFEENO TO D-FEEGLNO
           ELSE
              MOVE CDB-GC-GLFEENO TO D-FEEGLNO.

      *-----------------------------------------------------------------
       GET-CREDIT-LIFE-TAG.
           IF TS-INSTYPES-DL
              MOVE "D" TO D-INSTYPE
              IF TS-INSURED = "M" OR "S"
                 MOVE "S" TO D-INSFLAG
              ELSE
                 MOVE "J" TO D-INSFLAG
           ELSE
              MOVE "L" TO D-INSTYPE
              IF TS-INSURED = "M" OR "S"
                 MOVE "S" TO D-INSFLAG
              ELSE
                 MOVE "J" TO D-INSFLAG.

      *-----------------------------------------------------------------
       UPDATE-TABLE.
           IF TS-LNAMT < 0
              SUBTRACT 1 FROM T-NOLOANS(1)
           ELSE
              ADD 1 TO T-NOLOANS(1).

           ADD TS-LNAMT TO T-LNAMT(LEV).
           ADD TS-FINCHG TO T-FINCHG(LEV).
           ADD TS-INTCHG TO T-INTCHG(LEV).
           ADD TS-SERCHG TO T-SERCHG(LEV).
           ADD TS-EXTCHG TO T-EXTCHG(LEV).

           COMPUTE T-MFEE(LEV) ROUNDED = T-MFEE(LEV)
                                   + (TS-MAINTFEE * TS-ORGTERM).

           COMPUTE T-EARNINGS(LEV) = T-EARNINGS(LEV) +
                                     TS-EARNINGS(1) + TS-EARNINGS(2) +
                                     TS-EARNINGS(3) + TS-EARNINGS(4) +
                                     TS-EARNINGS(5) + TS-EARNINGS(6).

           ADD TS-DLRVPRIN(1) TO T-DLRVPRIN(LEV 1).
           ADD TS-DLRVPRIN(2) TO T-DLRVPRIN(LEV 2).
           ADD TS-DLRVPRIN(3) TO T-DLRVPRIN(LEV 3).
           ADD TS-DLRVCHGS(1) TO T-DLRVCHGS(LEV 1).
           ADD TS-DLRVCHGS(2) TO T-DLRVCHGS(LEV 2).
           ADD TS-DLRVCHGS(3) TO T-DLRVCHGS(LEV 3).
           ADD TS-DLDISC TO T-DLDISC(LEV).
           ADD TS-DLPARVHELD TO T-DLPARVHELD(LEV).
           ADD TS-DLPARVPAID TO T-DLPARVPAID(LEV).

           IF TS-INSTYPES-DL
              IF TS-INSOURS(1) = "Y"
                 ADD TS-INSPREM(1) TO T-DL(LEV).

           IF NOT TS-INSTYPES-DL
              IF TS-INSOURS(1) = "Y"
                 ADD TS-INSPREM(1) TO T-LL(LEV).

           IF TS-INSOURS(2) = "Y"
              ADD TS-INSPREM(2) TO T-AH(LEV).

           IF TS-INSOURS(3) = "Y"
              ADD TS-INSPREM(3) TO T-PP(LEV).

           IF TS-INSOURS(4) = "Y"
              ADD TS-INSPREM(4) TO T-O1(LEV).

           IF TS-INSOURS(5) = "Y"
              ADD TS-INSPREM(5) TO T-O2(LEV).

           IF TS-INSOURS(6) = "Y"
              ADD TS-INSPREM(6) TO T-O3(LEV).

           MOVE 1 TO I-SUB.
           PERFORM UPDATE-TABLE-NEXT-FEE 10 TIMES.

           ADD TS-PROCEEDS TO T-PROCEEDS(LEV).
           ADD TS-PURBAL TO T-PURBAL(LEV).

           COMPUTE T-ANTICERN(LEV) = T-ANTICERN(LEV) +
                                     TS-ANTICERN(1) + TS-ANTICERN(2) +
                                     TS-ANTICERN(3) + TS-ANTICERN(5).

           IF NOT (GP-INCL-OTH-IN-EARNED-N OR GP-INCL-OTH-IN-EARNED-R)
              ADD TS-ANTICERN(6) TO T-ANTICERN(LEV).

           IF NOT (GP-INCL-OTH-IN-EARNED-R)
              ADD TS-ANTICERN(4) TO T-ANTICERN(LEV).


           ADD TS-INSPREM-DLADJ TO T-INSPREM-DLADJ(LEV).
           IF TS-LOANTYPE = "I"
              ADD TS-FINCHG, TS-LNAMT TO T-X-LNAMT(LEV)
           ELSE
              ADD  TS-LNAMT TO T-X-LNAMT(LEV).

           ADD TS-REGPYAMT      TO T-SUM-OF-PMT(LEV).

           IF GP-DLCOMM-CMSI-FG = "Y"
              ADD TS-DLCOMM-PP-CMSI TO T-DLCOMM-PP-CMSI(LEV)
              ADD TS-DLCOMM-O3-CMSI TO T-DLCOMM-O3-CMSI(LEV).

      *-----------------------------------------------------------------
       UPDATE-TABLE-NEXT-FEE.
           IF TS-FEEOURS(I-SUB) = "Y"
              ADD TS-FEEAMT(I-SUB) TO T-FEES(LEV).

           ADD 1 TO I-SUB.

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
           ADD T-INSPREM-DLADJ(SUB) TO T-INSPREM-DLADJ(LEV).
           ADD T-X-LNAMT(SUB)          TO T-X-LNAMT(LEV).
           ADD T-SUM-OF-PMT(SUB)       TO T-SUM-OF-PMT(LEV).
           IF GP-DLCOMM-CMSI-FG = "Y"
              ADD T-DLCOMM-PP-CMSI(SUB)   TO T-DLCOMM-PP-CMSI(LEV)
              ADD T-DLCOMM-O3-CMSI(SUB)   TO T-DLCOMM-O3-CMSI(LEV).

      *-----------------------------------------------------------------
       TYPE-TOTALS.
           MOVE 1 TO LEV.
           MOVE "CLASS  TOTALS:" TO G-TOTNAME.
           PERFORM PRINT-SUMMARY.

      *-----------------------------------------------------------------
       PRINT-SUMMARY.

      * TOTALS TAKE UP SIX LINES PER CODE.
      * MAKE SURE ALL LINES, PER CL/BR/GRAND, PRINT ON THE SAME PAGE

           IF (LN-COUNT + 6) > EXT-CONAME-RPT-LINES
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.

      * LINE 1
           MOVE "LOANS   :"       TO G-HEADING1
           MOVE T-NOLOANS(LEV)    TO G-LOANS.
           MOVE "INTCHG  :"       TO G-HEADING2
           MOVE T-INTCHG(LEV)     TO G-AMOUNT-2
           MOVE "OUR-LL  : "      TO G-HEADING3.
           MOVE T-LL(LEV)         TO G-AMOUNT-3.
           MOVE "OUR-O3  :"       TO G-HEADING4.
           MOVE T-O3(LEV)         TO G-AMOUNT-4.
           MOVE "PARHELD :"       TO G-HEADING5.
           MOVE T-DLPARVHELD(LEV) TO G-AMOUNT-5.
           MOVE 2                 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * LINE 2
           MOVE "LOAN-AMT:"       TO G-HEADING1
           MOVE T-LNAMT(LEV)      TO G-AMOUNT-1
           MOVE "SRVCHRG :"       TO G-HEADING2
           MOVE T-SERCHG(LEV)     TO G-AMOUNT-2
           MOVE "OUR-AH  :"       TO G-HEADING3.
           MOVE T-AH(LEV)         TO G-AMOUNT-3.
           MOVE "OUR FEES:"       TO G-HEADING4.
           MOVE T-FEES(LEV)       TO G-AMOUNT-4.
           MOVE "PARPAID :"       TO G-HEADING5.
           MOVE T-DLPARVPAID(LEV) TO G-AMOUNT-5.
           MOVE 1                 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * LINE 3
           MOVE "PUR-AMT :"       TO G-HEADING1
           IF T-PURBAL(LEV) NOT = T-LNAMT(LEV)
              MOVE T-PURBAL(LEV)  TO G-AMOUNT-1
           ELSE
              MOVE ZEROS          TO G-AMOUNT-1
           MOVE "EXTCHG  : "      TO G-HEADING2
           MOVE T-EXTCHG(LEV)     TO G-AMOUNT-2
           MOVE "OUR-PP  : "      TO G-HEADING3.
           MOVE T-PP(LEV)         TO G-AMOUNT-3.
           MOVE "RSVPRIN :"       TO G-HEADING4.
           MOVE 0 TO TOT-DLRVCHGS
                     TOT-DLRVPRIN.
           PERFORM VARYING TSUB FROM 1 BY 1
                            UNTIL TSUB > 3
                   ADD T-DLRVPRIN (LEV TSUB) TO TOT-DLRVPRIN
                   ADD T-DLRVCHGS (LEV TSUB) TO TOT-DLRVCHGS
           END-PERFORM.
           MOVE TOT-DLRVPRIN      TO G-AMOUNT-4.
           MOVE "EARNINGS:"       TO G-HEADING5.
           MOVE T-EARNINGS(LEV)   TO G-AMOUNT-5.
           MOVE 1                 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * LINE 4
           MOVE "FIN-CHG :"       TO G-HEADING1
           MOVE T-FINCHG(LEV)     TO G-AMOUNT-1
           MOVE "MFEE    :"       TO G-HEADING2
           MOVE T-MFEE(LEV)       TO G-AMOUNT-2
           MOVE "OUR-O1  :"       TO G-HEADING3.
           MOVE T-O1(LEV)         TO G-AMOUNT-3.
           MOVE "RSVCHRG :"       TO G-HEADING4.
           MOVE TOT-DLRVCHGS      TO G-AMOUNT-4.
           MOVE "PROCEEDS:"       TO G-HEADING5.
           MOVE T-PROCEEDS(LEV)   TO G-AMOUNT-5.
           MOVE 1                 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * LINE 5
           MOVE "ANTICERN:"          TO G-HEADING1.
      * ALWAYS PRINT ANTICERN
           MOVE T-ANTICERN(LEV)      TO G-AMOUNT-1.

           MOVE "OUR-DL  : "         TO G-HEADING2.
           MOVE T-DL(LEV)            TO G-AMOUNT-2.
           MOVE "OUR-O2  :"          TO G-HEADING3.
           MOVE T-O2(LEV)            TO G-AMOUNT-3.
           MOVE "COMM    :"          TO G-HEADING4.
           MOVE T-DLDISC(LEV)        TO G-AMOUNT-4.
           MOVE "PREM-ADJ:"          TO G-HEADING5.
           MOVE T-INSPREM-DLADJ(LEV) TO G-AMOUNT-5.
           MOVE 1                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      * LINE 6
           MOVE "AVG TERM   :"       TO G-HEADING1.
           IF T-LNAMT(LEV) NOT = 0
              IF T-LNAMT(LEV) NOT < 0
                 COMPUTE AVGTERM = T-X-LNAMT(LEV) / T-SUM-OF-PMT(LEV)
                 MOVE AVGTERM TO G-AMOUNT-1
              ELSE
                 COMPUTE AVGTERM = (T-X-LNAMT(LEV) /
                                   T-SUM-OF-PMT(LEV)) * -1
                 MOVE AVGTERM TO G-AMOUNT-1.
      
           IF GP-DLCOMM-CMSI-FG = "Y"
              MOVE "DLCOMPP :"    TO G-HEADING4
              MOVE T-DLCOMM-PP-CMSI(LEV) TO G-AMOUNT-4
      
              MOVE "DLCOMO3 :"    TO G-HEADING5
              MOVE T-DLCOMM-O3-CMSI(LEV) TO G-AMOUNT-5.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *----------------------------------------------------------------
       PRINT-GRAND-TOTALS.
           MOVE 3 TO LEV.
           MOVE "GRAND  TOTALS:" TO G-TOTNAME.
           PERFORM PRINT-SUMMARY.

      *-----------------------------------------------------------------
       PRINT-LT-REC-COL.
           IF PRCOL-FG = "Y"
              MOVE LN-OWNBR  TO LTC-BRNO
              MOVE LN-ACCTNO TO LTC-ACCTNO
              MOVE 1         TO LTC-SEQNO
              PERFORM OPEN-LTC1-FILE
              PERFORM READ-LTC1-FILE
              PERFORM CLOSE-LTC1-FILE
              IF IO-FG = 0
                 MOVE "MAKE : " TO D-FIL-MAKE
                 MOVE "MODEL : " TO D-FIL-MODEL
                 MOVE "YEAR : " TO D-FIL-YEAR
                 MOVE "VIN # : " TO D-FIL-ID
                 MOVE LTC-MAKE(1) TO D-MAKE
                 MOVE LTC-MODEL(1) TO D-MODEL
                 MOVE LTC-YEAR(1) TO D-YEAR
                 MOVE LTC-VIN(1) TO D-ID
                 ADD 1 TO DTL-SUB
                 MOVE DETAIL-LINE TO DTL-LINE(DTL-SUB)
                 MOVE SPACES TO DETAIL-LINE
              ELSE
                 MOVE SPACES TO LTC-AUTO(2).

           IF PRCOL-FG = "Y"
              IF LTC-AUTO(2) NOT = SPACES
                 MOVE "MAKE : " TO D-FIL-MAKE
                 MOVE "MODEL : " TO D-FIL-MODEL
                 MOVE "YEAR : " TO D-FIL-YEAR
                 MOVE "VIN # : " TO D-FIL-ID
                 MOVE LTC-MAKE(2) TO D-MAKE
                 MOVE LTC-MODEL(2) TO D-MODEL
                 MOVE LTC-YEAR(2) TO D-YEAR
                 MOVE LTC-VIN(2) TO D-ID
                 ADD 1 TO DTL-SUB
                 MOVE DETAIL-LINE TO DTL-LINE(DTL-SUB)
                 MOVE SPACES TO DETAIL-LINE.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRSGS_SQL.CPY".
       COPY "LIBLP/LPLTCGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPTRS1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBGB/GBWK2IN.CPY".
       COPY "LIBLP/LPLTC1RN.CPY".
      *-----------------------------------------------------------------
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
           MOVE WK2-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

      *-----------------------------------------------------------------
       EXIT-PROG.

           IF SV-BEG-BR NOT = 9999
              IF EXT-EOBUF-GROUP-FG = "Y"
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH
                 MOVE SV-GROUP-FLAG TO GROUP-FLAG
                 MOVE SV-ENT-GROUP TO ENT-GROUP
                 MOVE SV-LOCAL-PRINT-FG TO LOCAL-PRINT-FG
              ELSE
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY EMSFRG-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
