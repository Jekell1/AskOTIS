       IDENTIFICATION DIVISION.
       PROGRAM-ID.      RZNACR.
       DATE-WRITTEN. 01-15-92
      ******************************************************************
      *                        (LP)
      * DESCRIPTION:    PERIOD NON ACCRUAL INCOME REPORT
      *
      *=================================================================
      * REV:
      * JFF 101195 ADDED CLASS SCAN
      * JTG 101196 ADDED 'LXE-WORKER' & 'LXE-WORKER2' FOR LPCERN-- A50 VERSION
      *                  SP-ERNFRMLA 15 & 16, REAL INTEREST BEARINGING METHOD
      * BAH 120396 ADDED DEALER NUMBER TO SCREEN OPTIONS, BAYWAY, #412
      * MJD 121096 CHANGED TO PERFORM LPSTAT-DISPLAY AFTER READING BWFILE.
      * BAH 121396 SKIP RESCISSION LOANS
      * JTG 110697 INCREASED WORKERS FROM S9(7)V99 TO S9(9)V99
      *            AND DISPLAYS TO Z(8)9.99-
      * MJD 010598 FIXED BUG: WHEN TESTING FOR END-CLASS, PROGRAM WAS TESTING
      *            LN-OWNBR < 99. (SHOULD BE LN-OWNBR < 9999)
      * BAH 031898 CROSS DATA PATHS AND ALLOW GROUPS
      * BAH 043098 IF ACCOUNT ONLY HAD AN "RI", IT WAS NOT INCLUDED BECAUSE
      *            OF THE ALL-ACCTS = "N" TEST. ADDED REBATES TO THE ZERO
      *            TEST, INSTANT AUTO PL# 184
      * BAH 050898 SKIP LOANS IF BOOKED AND VOIDED WITHIN SAME PERIOD, LIKE
      *            AC3ERN, GRAND FINANCE, PL# 188
      * BLV 071498 IF GROUP, WAS MOVING 999 TO END-BRANCH INSTEAD OF 9999
      * JTG 080398 CHANGED REPORT DATE RANGE TO ALLOW FOR ONE (SAME) DAY
      *            AND STOPPED ENTRY OF AN ALL ZERO DATE
      * MJD 990802 ADDED GPENV(W). NEEDED FOR CHANGES TO LPEARN.
      * BLF 050520 CHANGE TO DRIVE OFF OF TRFILE INSTEAD OF LNFILE.
      *            ADDED SORT ROUTINE, WILL PRINT BY CLASS BY ACCOUNT
      *            REMOVED OPTION TO PRINT ALL ACCTS, CAN ONLY RUN FOR
      *            1 MONTH NOW, WORLD PR# 383
      * BLF 050715 ADD 1 BYTE FILLER IN EOBUF FOR PRT-BAL PARAMETER WHICH
      *            WAS REMOVED IN CHANGED ABOVE (INSTEAD OF CHANGING VERSION)
      *  CS 120227 DONT INCLUDE P&L ACCOUNTS WITH 'RI','RS' OR 'RM'  PR#791
      *                                                       WORLD
      * BAH 160125 REMOVED LTFILE, PD0003
      * CS 170412  ACTIVATE 8 DIGIT DATES
      * KEC 2017.0607 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          ADDED 'LN' FILE PREFIX FOR WORKING STORAGE FIELDS.
      * BAH 170705 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181004 GLOBAL BWFILE, L2G
      * BAH 181211 GLOBAL LNFILE, L2G
      * BAH 20190927 REMOVED ACCESS-CALL TRPFILE
      * BLM 20210810 REMOVE TP-CODE, TP-POOLID NO LONGER IN KEY
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT WK-FILE ASSIGN MKTEMP-BUF
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK1-KEY
                  ALTERNATE RECORD KEY WKX-KEY WITH DUPLICATES
                  FILE STATUS FILE-STAT.
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

      /*****************************************************
      *   SUMMARY TOTALS WORK RECORD                       *
      ******************************************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK1-KEY.
               05  WK-BRANCH           PIC 9(4).
               05  WKX-KEY.
                   07  WK-CLASS        PIC 9(3).
           03  WK-NOACCTS              PIC 9(6)      COMP-3.
           03  WK-UNEARN               PIC S9(9)V99  COMP-3.
           03  WK-INT                  PIC S9(9)V99  COMP-3.
           03  WK-INT-RB               PIC S9(9)V99  COMP-3.
           03  WK-ADDON                PIC S9(9)V99  COMP-3.
           03  WK-SRV                  PIC S9(9)V99  COMP-3.
           03  WK-SRV-RB               PIC S9(9)V99  COMP-3.
           03  WK-MFEE                 PIC S9(9)V99  COMP-3.
           03  WK-MFEE-RB              PIC S9(9)V99  COMP-3.
           03  WK-DISC                 PIC S9(9)V99  COMP-3.
           03  WK-OTHER                PIC S9(9)V99  COMP-3.
           03  WK-ACCRUED              PIC S9(9)V99  COMP-3.
           03  WK-ENDING               PIC S9(9)V99  COMP-3.
      ******************************************************************
      * SORTED TRFILE BY BRANCH, CLASS, ACCOUNT
      ******************************************************************
      *FD  WK2-FILE
      *    LABEL RECORDS ARE STANDARD.
      *01  WK2-RECS                     PIC X(75).
      *    03  WK2-KEY.
      *        05  WK2-BRANCH           PIC 9(4).
      *        05  WK2-CLASS            PIC 9(3).
      *        05  WK2-ACCTNO           PIC 9(8).

      *    03  WK2-DATE                 PIC 9(8).
      *    03  WK2-CODE                 PIC X.
      *    03  WK2-LINE                 PIC 9(3).
      *    03  WK2-EARN-WORKERS.
      *        05  WK2-INT              PIC S9(9)V99  COMP VALUE 0.
      *        05  WK2-ADDON            PIC S9(9)V99  COMP VALUE 0.
      *        05  WK2-SRV              PIC S9(9)V99  COMP VALUE 0.
      *        05  WK2-MFEE             PIC S9(9)V99  COMP VALUE 0.
      *        05  WK2-DISC             PIC S9(9)V99  COMP VALUE 0.
      *        05  WK2-OTHER            PIC S9(9)V99  COMP VALUE 0.
      *    03  WK2-EARN-WORK-TABLE      REDEFINES WK2-EARN-WORKERS.
      *        05  WK2-EARN-WORK-TAB    OCCURS 6 TIMES
      *                                 PIC S9(9)V99  COMP.
      *    03  WK2-INT-RB               PIC S9(9)V99  COMP VALUE 0.
      *    03  WK2-SRV-RB               PIC S9(9)V99  COMP VALUE 0.
      *    03  WK2-MFEE-RB              PIC S9(9)V99  COMP VALUE 0.
      *    03  WK2-NON-ACCRUAL-DATE     PIC 9(8)      COMP VALUE 0.


      ******************************************************************

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/RZNACR S35 08/02/23-05:00:35 >".
      ****************************
      *     F I L E   S P E C S  *
      ****************************
       COPY "LIBLP/LP01TRP.CPY".
      *COPY "LIBLP/LP01TRP_SQL.CPY".
      * COPYMEMBER: LIBLP/LP01TRP_SQL
      ******************************************************************
      *
      * DESCRIPTION: SQL - DAILY TRANSACTION FILE - PAYMENTS
      *
      *=================================================================
      * REV:
      * BLM 2021-0503 ***NEW***
      * BAH 20220923 ADDED TRP-LCDUE #1570
      *
      ******************************************************************
      *- - -{GREATER CURSOR FOR START-GREATER & READ-NEXT}
       01  TRP1-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  TRP1-CURSOR-STAT-GOOD             VALUE "00".
           88  TRP1-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".
       01  TRP2-CURSOR-STAT             PIC X(2) VALUE SPACES.
           88  TRP2-CURSOR-STAT-GOOD             VALUE "00".
           88  TRP2-CURSOR-STAT-BAD              VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.

       01  RET-CODE                        PIC S9(4) COMP-5.

       01  WK2-REC.
           03  WK2-KEY.
               05  WK2-BRANCH           PIC 9(4).
               05  WK2-CLASS            PIC 9(3).
               05  WK2-ACCTNO           PIC 9(8).

           03  WK2-DATE                 PIC X(10).
      *    03  WK2-CODE                 PIC X.
           03  WK2-LINE                 PIC 9(3).
           03  WK2-EARN-WORKERS.
               05  WK2-INT              PIC S9(9)V99  COMP VALUE 0.
               05  WK2-ADDON            PIC S9(9)V99  COMP VALUE 0.
               05  WK2-SRV              PIC S9(9)V99  COMP VALUE 0.
               05  WK2-MFEE             PIC S9(9)V99  COMP VALUE 0.
               05  WK2-DISC             PIC S9(9)V99  COMP VALUE 0.
               05  WK2-OTHER            PIC S9(9)V99  COMP VALUE 0.
      *    03  WK2-EARN-WORK-TABLE      REDEFINES WK2-EARN-WORKERS.
      *        05  WK2-EARN-WORK-TAB    OCCURS 6 TIMES
      *                                 PIC S9(9)V99  COMP.
           03  WK2-INT-RB               PIC S9(9)V99  COMP VALUE 0.
           03  WK2-SRV-RB               PIC S9(9)V99  COMP VALUE 0.
           03  WK2-MFEE-RB              PIC S9(9)V99  COMP VALUE 0.
           03  WK2-NON-ACCRUAL-DATE     PIC X(10).                     

       01  QTRP1-WHERE-BEG-KEY.
           03  QTRP1-WBEG-BRNO             PIC 9(04).
           03  QTRP1-WBEG-DATE             PIC X(10).
           03  QTRP1-WBEG-CLASS            PIC 9(03).
           03  QTRP1-WBEG-ACCTNO           PIC 9(08).
           03  QTRP1-WBEG-LINE             PIC 9(03).

       01  QTRP1-WHERE-END-KEY.
           03  QTRP1-WEND-BRNO             PIC 9(04).
           03  QTRP1-WEND-DATE             PIC X(10).
           03  QTRP1-WEND-CLASS            PIC 9(03).
           03  QTRP1-WEND-ACCTNO           PIC 9(08).
           03  QTRP1-WEND-LINE             PIC 9(03).

       01  QTRP2-WHERE-BEG-KEY.
           03  QTRP2-WBEG-DATE             PIC X(10).
           03  QTRP2-WBEG-BRNO             PIC 9(04).
           03  QTRP2-WBEG-CLASS            PIC 9(03).
           03  QTRP2-WBEG-ACCTNO           PIC 9(08).
           03  QTRP2-WBEG-LINE             PIC 9(03).

       01  QTRP2-WHERE-END-KEY.
           03  QTRP2-WEND-DATE             PIC X(10).
           03  QTRP2-WEND-BRNO             PIC 9(04).
           03  QTRP2-WEND-CLASS            PIC 9(03).
           03  QTRP2-WEND-ACCTNO           PIC 9(08).
           03  QTRP2-WEND-LINE             PIC 9(03).

       01  QTRP1-WHERE-SCAN-BEG-KEY.
           03  QTRP1-WSBEG-DATE            PIC X(10).
           03  QTRP1-WSBEG-CLASS           PIC 9(03).
           03  QTRP1-WSBEG-ACCTNO          PIC 9(08).
           03  QTRP1-WSBEG-LINE            PIC 9(03).
 
       01  QTRP-REC.
           03  QTP-BRNO             PIC 9(4).
           03  QTP-DATE             PIC X(10).
           03  QTP-CLASS            PIC 999.
           03  QTP-ACCTNO           PIC 9(8).
           03  QTP-LINE             PIC 999.
           03  QTP-LTOUCH-DATE      PIC X(10).
           03  QTP-LAWCODE          PIC 99.
           03  QTP-JDCD             PIC X.
           03  QTP-PLCD             PIC X.
           03  QTP-SSNO             PIC 9(10).
           03  QTP-MAKER            PIC X.
           03  QTP-IBPC             PIC X.
           03  QTP-TRCD             PIC XX.
           03  QTP-REFNO            PIC X(5).
           03  QTP-PAYDATE          PIC X(10).
           03  QTP-DUEDATE          PIC X(10).
           03  QTP-TRAMT            PIC S9(7)V99.
           03  QTP-INTDUE           PIC S9(7)V99.
           03  QTP-APOTH            PIC S9(7)V99.
           03  QTP-APOT2            PIC S9(7)V99.
           03  QTP-APINT            PIC S9(7)V99.
           03  QTP-APLC             PIC S9(7)V99.
           03  QTP-APCUR            PIC S9(7)V99.
           03  QTP-GLNO-1         PIC 9(11).
           03  QTP-GLAMT-1        PIC S9(7)V99.
           03  QTP-GLNO-2         PIC 9(11).
           03  QTP-GLAMT-2        PIC S9(7)V99.
           03  QTP-GLNO-3         PIC 9(11).
           03  QTP-GLAMT-3        PIC S9(7)V99.
           03  QTP-EARNINGS-1     PIC S9(7)V99.
           03  QTP-EARNINGS-2     PIC S9(7)V99.
           03  QTP-EARNINGS-3     PIC S9(7)V99.
           03  QTP-EARNINGS-4     PIC S9(7)V99.
           03  QTP-EARNINGS-5     PIC S9(7)V99.
           03  QTP-EARNINGS-6     PIC S9(7)V99.
           03  QTP-EARNINGS-7     PIC S9(7)V99.
           03  QTP-EARNINGS-8     PIC S9(7)V99.
           03  QTP-EARNINGS-9     PIC S9(7)V99.
           03  QTP-EARNINGS-10    PIC S9(7)V99.
           03  QTP-WORKAMT-1     PIC S9(7)V99.
           03  QTP-WORKAMT-2     PIC S9(7)V99.
           03  QTP-WORKAMT-3     PIC S9(7)V99.
           03  QTP-WORKAMT-4     PIC S9(7)V99.
           03  QTP-WORKAMT-5     PIC S9(7)V99.
           03  QTP-WORKAMT-6     PIC S9(7)V99.
           03  QTP-WORKAMT-7     PIC S9(7)V99.
           03  QTP-WORKAMT-8     PIC S9(7)V99.
           03  QTP-WORKAMT-9     PIC S9(7)V99.
           03  QTP-WORKAMT-10    PIC S9(7)V99.
           03  QTP-OTHBAL           PIC S9(7)V99.
           03  QTP-OT2BAL           PIC S9(7)V99.
           03  QTP-INTBAL           PIC S9(7)V99.
           03  QTP-LCBAL            PIC S9(7)V99.
           03  QTP-CURBAL           PIC S9(7)V99.
           03  QTP-PDTH-DATE        PIC X(10).
           03  QTP-DLNO             PIC 9(4)V99.
           03  QTP-DLPROC           PIC S9(7)V99.
           03  QTP-SERVACCT         PIC X.
           03  QTP-SALEFIN          PIC X.
           03  QTP-INSURED          PIC X.
           03  QTP-INSOURS          PIC X.
           03  QTP-LOANTYPE         PIC X.
           03  QTP-TILLNO           PIC 9(2).
           03  QTP-LCPDTH-DATE      PIC X(10).
           03  QTP-POSTING-BR       PIC 9(4).
           03  QTP-OTHBR-FG         PIC X.
           03  QTP-REV              PIC X.
           03  QTP-REVTRCD          PIC XX.
           03  QTP-REVSEQNO         PIC 9(5).
           03  QTP-STATUSFG         PIC X.
           03  QTP-KEEPAPINT        PIC X.
           03  QTP-INTPAID          PIC S9(7)V99.
           03  QTP-PRIOR-POOLID     PIC 9(5).
           03  QTP-INSTYPES         PIC X.
           03  QTP-INSEARN-1        PIC S9(7)V99.
           03  QTP-INSEARN-2        PIC S9(7)V99.
           03  QTP-INSEARN-3        PIC S9(7)V99.
           03  QTP-INSEARN-4        PIC S9(7)V99.
           03  QTP-INSEARN-5        PIC S9(7)V99.
           03  QTP-INSEARN-6        PIC S9(7)V99.
           03  QTP-INSEARN-7        PIC S9(7)V99.
           03  QTP-INSEARN-8        PIC S9(7)V99.
           03  QTP-INSEARN-9        PIC S9(7)V99.
           03  QTP-INSEARN-10       PIC S9(7)V99.
           03  QTP-INSWORKAMT-1        PIC S9(7)V99.
           03  QTP-INSWORKAMT-2        PIC S9(7)V99.
           03  QTP-INSWORKAMT-3        PIC S9(7)V99.
           03  QTP-INSWORKAMT-4        PIC S9(7)V99.
           03  QTP-INSWORKAMT-5        PIC S9(7)V99.
           03  QTP-INSWORKAMT-6        PIC S9(7)V99.
           03  QTP-INSWORKAMT-7        PIC S9(7)V99.
           03  QTP-INSWORKAMT-8        PIC S9(7)V99.
           03  QTP-INSWORKAMT-9        PIC S9(7)V99.
           03  QTP-INSWORKAMT-10       PIC S9(7)V99.
           03  QTP-DLRVCHGS-ADJ-1      PIC S9(7)V99.
           03  QTP-DLRVCHGS-ADJ-2      PIC S9(7)V99.
           03  QTP-DLRVCHGS-ADJ-3      PIC S9(7)V99.
           03  QTP-DLPARVHELD-ADJ       PIC S9(7)V99.
           03  QTP-DLPARVPAID-ADJ       PIC S9(7)V99.
           03  QTP-NEWSTAT-RATE         PIC 999V999.
           03  QTP-POCD                 PIC XX.
           03  QTP-PODATE               PIC X(10).
           03  QTP-DL-RVCHGS-CODE-1     PIC X.
           03  QTP-DL-RVCHGS-CODE-2     PIC X.
           03  QTP-DL-RVCHGS-CODE-3     PIC X.
           03  QTP-DL-PARVHELD-CODE     PIC X.
           03  QTP-DL-PARVPAID-CODE     PIC X.
           03  QTP-TOTNODEF             PIC S999.
           03  QTP-LNAMT                PIC S9(7)V99.
           03  QTP-ORIG-1STPYDATE       PIC X(10).
           03  QTP-OLD1ST-PYDATE        PIC X(10).
           03  QTP-1STPYDATE            PIC X(10).
           03  QTP-ORGTERM              PIC 9(3).
           03  QTP-INSCOMM              PIC S9(5)V99.
           03  QTP-MANUAL-RB            PIC X.
           03  QTP-USERID               PIC X(10).
           03  QTP-POSTTIME             PIC 9(6).
           03  QTP-PLTYPE               PIC X.
           03  QTP-PL-OTH-AR            PIC S9(7)V99.
           03  QTP-LP-DLNO              PIC 9(4)V99.
           03  QTP-REPOCD               PIC X.
           03  QTP-SPRCLASS             PIC 999.
           03  QTP-MAX-DEFER-FG         PIC X.
           03  QTP-POSTDATE             PIC X(10).
           03  QTP-UNITPER-CD           PIC X.
           03  QTP-UNITPER-FREQ         PIC 9(3).
           03  QTP-SP-DEFFRMLA          PIC 99.
           03  QTP-COLLECTOR            PIC X(3).
           03  QTP-PENALTY-INT          PIC S9(7)V99.
           03  QTP-INSCOMP              PIC 99.
           03  QTP-PROMDATE             PIC X(10).
           03  QTP-PROMAMT              PIC S9(7)V99.
           03  QTP-PROMID               PIC X(3).
           03  QTP-INS-TAX-RB           PIC S9(7)V99.
           03  QTP-PROMDAYS-PASTDUE     PIC S9(3).
           03  QTP-MIP-FG               PIC X.
           03  QTP-MIP-CLPAID           PIC S9(7)V99.
           03  QTP-MIP-CLCOMM           PIC S9(7)V99.
           03  QTP-MIP-AHPAID           PIC S9(7)V99.
           03  QTP-MIP-AHCOMM           PIC S9(7)V99.
           03  QTP-MIP-CLPDTH-DATE      PIC X(10).
           03  QTP-MIP-AHPDTH-DATE      PIC X(10).
           03  QTP-INS-TAX-COUNTY-CD    PIC X(3).
           03  QTP-INS-TAX-CITY-CD      PIC X(3).
           03  QTP-APPLIED-TO-ESCROW    PIC S9(7)V99.
           03  QTP-ESCROW-AMT           PIC S9(7)V99.
           03  QTP-ESCROW-PREPAID       PIC S9(7)V99.
           03  QTP-ESCROW-DISBURSED     PIC S9(7)V99.
           03  QTP-LP-SEQNO             PIC 9(5).
           03  QTP-PRIOR-DF-CODE        PIC X(2).
           03  QTP-PRIOR-DF-POSTDATE    PIC X(10).
           03  QTP-CANCEL-REASON        PIC X(3).
           03  QTP-FROMCL               PIC 9(3).
           03  QTP-TOCL                 PIC 9(3).
           03  QTP-ADDON-TXT            PIC X(3).
           03  QTP-ADDON-TYP            PIC X(2).
           03  QTP-DLRVNUM              PIC 9.
           03  QTP-P4P5-TRANCD          PIC XX.
           03  QTP-P4P5-DLRVNUM         PIC 9.
           03  QTP-POOLID               PIC 9(5).
           03  QTP-ACTIONCD2            PIC X(4).
           03  QTP-LCDUE                PIC S9(7)V99.

       EXEC SQL END DECLARE SECTION END-EXEC.

       COPY "LIBLP/LPWSTRP.CPY".
       COPY "LIBLP/LP01TRL.CPY".
       COPY "LIBLP/LP01TRL_SQL.CPY".
       COPY "LIBLP/LPWSTRL.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".

       01  PR-IFN        PIC X(2)     VALUE "PR".
       01  WK1-IFN       PIC X(3)     VALUE "WK1".
       01  WK2-IFN       PIC X(3)     VALUE "WK2".
       01  WK2-PATH      PIC X(35).

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-FEED          PIC 99       VALUE 0.
       01  LN-COUNT         PIC 999      VALUE 99.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  DISPLAY-MESS.
           03  FILLER           PIC X(18)    VALUE
           "IN PROGRESS.......".
           03  DISPLAY-MESS-BR  PIC 9(4).
           03  FILLER           PIC X(10)        VALUE " ".

       01  LAST-CLASS-READ      PIC 9(3)     VALUE 0.
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  SUMMARY-FG           PIC X        VALUE "N".
       01  BR-TOTALS            PIC X        VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  NAME-WORKER          PIC X(34).
       01  FIRST-NAME           PIC X(16).
       01  HOLD-LN-OWNBR        PIC 9(4)     VALUE 0.
       01  HOLD-LN-CLASS        PIC 9(3)     VALUE 0.
       01  UNEARN               PIC S9(9)V99  COMP VALUE 0.
       01  ENDING               PIC S9(9)V99  COMP VALUE 0.
       01  ACCRUED              PIC S9(9)V99  COMP VALUE 0.
       01  SUB                  PIC 9(6) COMP.
       01  W-LOAN-NUM.
           03  FILLER           PIC XXX.
           03  LOAN-NUM         PIC X(13).
       01  TEST-EARNINGS        PIC S9(9)V99 VALUE 0.

       01  OPEN-TRP1-SW         PIC X     VALUE "N".

       01  WS-DATE.
           03 WS-YYYY                   PIC X(4).
           03 FILLER                    PIC X(1).
           03 WS-MM                     PIC X(2).
           03 FILLER                    PIC X.
           03 WS-DD                     PIC X(2).

      *   TOTALS SUB 1 = CLASS
      *              2 = BRANCH
       01  TOTALS-TABLE.
           03  TOTAL-TBL               OCCURS 2.
               05  T-NOACCTS           PIC 9(6)         COMP.
               05  T-UNEARN            PIC S9(9)V99     COMP.
               05  T-INT               PIC S9(9)V99     COMP.
               05  T-INT-RB            PIC S9(9)V99     COMP.
               05  T-ADDON             PIC S9(9)V99     COMP.
               05  T-SRV               PIC S9(9)V99     COMP.
               05  T-SRV-RB            PIC S9(9)V99     COMP.
               05  T-MFEE              PIC S9(9)V99     COMP.
               05  T-MFEE-RB           PIC S9(9)V99     COMP.
               05  T-DISC              PIC S9(9)V99     COMP.
               05  T-OTHER             PIC S9(9)V99     COMP.
               05  T-ACCRUED           PIC S9(9)V99     COMP.
               05  T-ENDING            PIC S9(9)V99     COMP.
       01  CLASS-FG        PIC X(3)    VALUE "   ".
           88  CLASS-BEG               VALUE "BEG".
           88  CLASS-END               VALUE "END".

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-DATE           PIC 9(8).
           03  BEGDATE         REDEFINES BEG-DATE.
               05  BEG-CC      PIC 9(2).
               05  BEG-YY      PIC 9(2).
               05  BEG-MM      PIC 9(2).
               05  BEG-DD      PIC 9(2).
           03  END-DATE           PIC 9(8).
           03  ENDDATE         REDEFINES END-DATE.
               05  END-CC      PIC 9(2).
               05  END-YY      PIC 9(2).
               05  END-MM      PIC 9(2).
               05  END-DD      PIC 9(2).
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  BEG-CLASS       PIC 999.
           03  END-CLASS       PIC 999.
           03  DEALER          PIC 9(4)V99.
           03  SUM-ONLY        PIC X.
           03  FILLER          PIC X.
           03  PRT-BAL         PIC X.
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).

       01  VERSION-CONTROL     PIC X VALUE "C".

       01  SV-BEG-BR           PIC 9(4)       VALUE 9999.
       01  SV-END-BR           PIC 9(4)       VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.
      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(30)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(38)    VALUE
           "        9.99R =  AMOUNT OF REFUND".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(10)    VALUE " ".
           03  FILLER           PIC X(44)    VALUE
           "    NON ACCRUAL EARNINGS REPORT FOR PERIOD".
           03  H-BEGDATE        PIC 99/99/99.
           03  FILLER           PIC X(6)     VALUE " THRU ".
           03  H-ENDDATE        PIC 99/99/99.

       01  HEAD3.
           03  FILLER      PIC X(8)     VALUE "BRANCH :".
           03  FILLER      PIC XX       VALUE " ".
           03  H-BRNO      PIC 9(4).
           03  H-BRNOX     REDEFINES H-BRNO
                           PIC X(4).
           03  FILLER      PIC X        VALUE " ".
           03  H-BRDESC    PIC X(20).
           03  FILLER      PIC X(99)    VALUE " ".

       01  HEAD4.
           03  FILLER      PIC X(8)     VALUE "CLASS  :".
           03  FILLER      PIC XX       VALUE " ".
           03  H-CLASS     PIC Z(3).
           03  FILLER      PIC X        VALUE " ".
           03  H-LCDESC    PIC X(20).
           03  FILLER      PIC X(87)    VALUE " ".
           03  FILLER      PIC X(11)    VALUE " ".

       01  HEAD5.
           03  FILLER      PIC X(29)    VALUE SPACE.
           03  FILLER      PIC X(30)    VALUE
               "                         -----".
           03  FILLER      PIC X(46)    VALUE
               "-------- PERIOD NON ACCRUAL EARNINGS ---------".
           03  FILLER      PIC X(28)    VALUE
               "-----    TOT NON            ".

       01  HEAD6.
           03  FILLER      PIC X(28)    VALUE
               "ACCOUNT NO.   BORROWER      ".
           03  FILLER      PIC X(45)    VALUE
               "            DATE         INTEREST    ADDONS  ".
           03  FILLER      PIC X(38)    VALUE
           "  SERCHG  MAINTFEE   DL DISC     OTHER".
           03  HEAD6-3     PIC X(20)    VALUE
           "   ACCRUAL          ".
       01  HEAD6-3B        PIC X(20)    VALUE
           "  EARNINGS   BALANCE".

       01  NEW-BRANCH.
           03  FILLER          PIC X(8) VALUE "BRANCH: ".
           03  D-T-BRNO        PIC 9(4).
           03  FILLER          PIC XX.
           03  D-T-BRDESC      PIC X(20).


       01  DETAIL-LINE         PIC X(132) VALUE SPACES.
       01  D-LINE1             REDEFINES DETAIL-LINE.
           03  D-LOAN-NUM      PIC X(13).
           03  FILLER          REDEFINES D-LOAN-NUM.
               05  FILLER      PIC X(7).
               05  D-VOID      PIC X(6).
           03  FILLER          PIC X.
           03  D-BWNAME        PIC X(20).
           03  FILLER          PIC X.
           03  ANTIC-REPORT.
               05  FILLER      PIC XXX.
               05  D-DATE      PIC 99/99/99       BLANK ZERO.
               05  FILLER      PIC X(6).
               05  D-INT       PIC ZZZZZ9.99-     BLANK ZERO.
               05  FILLER      REDEFINES D-INT.
                   07  FILLER  PIC X(9).
                   07  D-INT-R PIC X.
               05  D-ADDON     PIC ZZZZZ9.99-     BLANK ZERO.
               05  D-SRV       PIC ZZZZZ9.99-     BLANK ZERO.
               05  FILLER      REDEFINES D-SRV.
                   07  FILLER  PIC X(9).
                   07  D-SRV-R PIC X.
               05  D-MFEE      PIC ZZZZZ9.99-     BLANK ZERO.
               05  FILLER      REDEFINES D-MFEE.
                   07  FILLER  PIC X(9).
                   07  D-MFEE-R PIC X.
               05  D-DISC      PIC ZZZZZ9.99-     BLANK ZERO.
               05  D-OTHER     PIC ZZZZZ9.99-     BLANK ZERO.
               05  D-TOTNON    PIC ZZZZZ9.99-     BLANK ZERO.
               05  D-BALANCE   PIC ZZZZZ9.99-     BLANK ZERO.

       01  CLASS-TOTAL              REDEFINES DETAIL-LINE.
           03  D-TAG                PIC X(25).
           03  D-T-NOACCTS-T        PIC ZZZZZ9.
           03  D-T-TAG-T            PIC X.
           03  FILLER               PIC X(7).
           03  D-T-ANTIC.
               05  FILLER           PIC X(20).
               05  D-T-ADDON-T      PIC Z(8)9.99-   BLANK ZERO.
               05  FILLER           PIC X(7).
               05  D-T-MFEE-T       PIC Z(8)9.99-   BLANK ZERO.
               05  FILLER           REDEFINES D-T-MFEE-T.
                   07  FILLER       PIC X(12).
                   07  D-T-MFEE-T-R PIC X.
               05  FILLER           PIC X(7).
               05  D-T-OTHER-T      PIC Z(8)9.99-   BLANK ZERO.
               05  FILLER           PIC X(7).
               05  D-T-BALANCE-T    PIC Z(8)9.99-   BLANK ZERO.

       01  CLASS-TOTAL-2        REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(49).
           03  D-T-INT-T        PIC Z(8)9.99-   BLANK ZERO.
           03  FILLER           REDEFINES D-T-INT-T.
               05  FILLER       PIC X(12).
               05  D-T-INT-T-R  PIC X.
           03  FILLER           PIC X(7).
           03  D-T-SRV-T        PIC Z(8)9.99-   BLANK ZERO.
           03  FILLER           REDEFINES D-T-SRV-T.
               05  FILLER       PIC X(12).
               05  D-T-SRV-T-R  PIC X.
           03  FILLER           PIC X(7).
           03  D-T-DISC-T       PIC Z(8)9.99-   BLANK ZERO.
           03  FILLER           PIC X(7).
           03  D-T-TOTNON-T     PIC Z(8)9.99-   BLANK ZERO.

       01  TOTAL-LINE               REDEFINES DETAIL-LINE.
           03  FILLER           PIC XXX.
           03  D-T-CLASS        PIC 99.
           03  FILLER           PIC X(8).
           03  D-T-NOACCTS      PIC ZZZZZ9.
           03  D-T-TAG          PIC X.
           03  D-T-ANT.
               05  FILLER           PIC X(13).
               05  D-T-INT          PIC Z(8)9.99-   BLANK ZERO.
               05  FILLER           REDEFINES D-T-INT.
                   07  FILLER       PIC X(12).
                   07  D-T-INT-R    PIC X.
               05  D-T-ADDON        PIC ZZZZZZZ9.99-   BLANK ZERO.
               05  D-T-SRV          PIC ZZZZZZZ9.99-   BLANK ZERO.
               05  FILLER           REDEFINES D-T-SRV.
                   07  FILLER       PIC X(11).
                   07  D-T-SRV-R    PIC X.
               05  D-T-MFEE         PIC ZZZZZZZ9.99-   BLANK ZERO.
               05  FILLER           REDEFINES D-T-MFEE.
                   07  FILLER       PIC X(11).
                   07  D-T-MFEE-R   PIC X.
               05  D-T-DISC         PIC Z(8)9.99-   BLANK ZERO.
               05  D-T-OTHER        PIC ZZZZZZZ9.99-   BLANK ZERO.
               05  D-T-TOTNON       PIC ZZZZZZZ9.99-   BLANK ZERO.
               05  D-T-BALANCE      PIC Z(8)9.99-   BLANK ZERO.

       01  RANGE-LN                 REDEFINES DETAIL-LINE.
           03  RANGE-LINE           PIC X(20).
           03  RANGE-SELECTION      PIC X(112).
           03  FILLER REDEFINES RANGE-SELECTION.
               05  RANGE-SELECTION-NUM  PIC 9(4).99.

       01  ERROR-LINE REDEFINES DETAIL-LINE.
           03  D-STAR1          PIC X(20).
           03  FILLER           PIC X(2).
           03  D-ACCT-PROMPT    PIC X(10).
           03  D-ERR-ACCT       PIC 9(6).
           03  FILLER           PIC X(2).
           03  D-ERR-DATE       PIC 99/99/99.
           03  FILLER           PIC X(2).
           03  D-ERR-MESSG      PIC X(52).
           03  D-STAR2          PIC X(20).

       01  SUMMARY1.
           03  FILLER           PIC X(11) VALUE
               "* SUMMARY *".
           03  FILLER           PIC X(108) VALUE " ".

       01  SUMMARY2.
           03  FILLER           PIC X(21) VALUE SPACES.
           03  FILLER           PIC X(14) VALUE
               "NO            ".
           03  FILLER           PIC X(43) VALUE
               "     ------------------- PERIOD NON ACCRUAL".
           03  FILLER           PIC X(32) VALUE
               " EARNINGS -------------------- ".
           03  FILLER           PIC X(21) VALUE
               "  TOT NON            ".

       01  SUMMARY3.
           03  FILLER           PIC X(32) VALUE
               "CLASS           ACCTS           ".
           03  FILLER           PIC X(52) VALUE
               "     INTEREST      ADDONS      SERCHG    MAINTFEE   ".
           03  FILLER           PIC X(22) VALUE
               "   DL DISC       OTHER".
           03  SUMMARY3-1       PIC X(24) VALUE
               "     ACCRUAL            ".
       01  SUMMARY3-1B          PIC X(25) VALUE
               "    EARNINGS      BALANCE".


       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBWI/TSTINTW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBWI/GRSCANW.CPY".

       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".

       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/NONACR_DEF.CPY".
       COPY "LIBLP/NONACR_WKS.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".


       01  MSEL-LIST            PIC X(5)  VALUE "MSEL.".
       01  LEGEND               PIC X(70).

      /***************************************
      *                                      *
      *     P R O G R A M   C O N T R O L    *
      *                                      *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/NONACR_SCN.CPY".


       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE.             
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TRP1-FILE.
      *    PERFORM CLOSE-TRL1-FILE.
      *    PERFORM CLOSE-SP1-FILE.
      *    PERFORM CLOSE-LN1-FILE.
      *    PERFORM CLOSE-LC1-FILE.
      *    PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
      *    CLOSE WK-FILE WK2-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "NONACR" TO FORM-PROG VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE  ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *----------------------------------------------------------------
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

      *----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

      *----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              TRL-PATH-OVERRIDE
                                              TRP-PATH-OVERRIDE.

           IF EXT-ROUTE-BUF = "00"
              MOVE BR-NO TO DISPLAY-MESS-BR
              MOVE DISPLAY-MESS TO MESS
              PERFORM SEND-LEGEND.

      *    PERFORM OPEN-TRL1-FILE.

           IF OPEN-TRP1-SW = "N"
               MOVE "Y" TO OPEN-TRP1-SW
               PERFORM OPEN-TRP1-FILE.
      *    PERFORM OPEN-TRP1-FILE.
     
      *    PERFORM OPEN-LN1-FILE.
      *    PERFORM OPEN-BW1-FILE.

           PERFORM SORT-ROUTINE.

           IF EXT-EOBUF-ERRCD = "X"
              GO TO NEXT-GROUP-LOC.

      *    PERFORM OPEN-WK2-FILE.

      *----------------------------------------------------------------
       NEXT-TR-REPORT.

      D    STOP MESS.
           PERFORM READ-TRP1-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-OF-GROUP.

           PERFORM TRAP-INTERRUPT.

           MOVE WK2-BRANCH TO LN-OWNBR.
           MOVE WK2-ACCTNO TO LN-ACCTNO.
      *    PERFORM READ-LN1-FILE.
           IF (LN-OWNBR = ' ' OR LN-OWNBR = NULL)
              MOVE "********************" TO D-STAR1 D-STAR2
              MOVE "ACCOUNT # " TO D-ACCT-PROMPT
              MOVE WK2-ACCTNO   TO D-ERR-ACCT
              MOVE WK2-DATE     TO WS-DATE         
              MOVE WS-YYYY      TO DATE-YYYYMMDD-YYYY
              MOVE WS-MM        TO DATE-YYYYMMDD-MM
              MOVE WS-DD        TO DATE-YYYYMMDD-DD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY  TO D-ERR-DATE
              MOVE "ACCOUNT NOT ON FILE!!!!!!!" TO D-ERR-MESSG
              MOVE 1 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              GO TO NEXT-TR-REPORT.

           PERFORM TRAP-INTERRUPT.

      * SKIP LOANS IN RESCISSION AND LOANS THAT HAVE BEEN RESCINDED
           IF LN-POCD-INRESCISSION OR LN-POCD-RESCIND
              GO TO NEXT-TR-REPORT.

           IF DEALER NOT = 0
              IF LN-DLNO NOT = DEALER
                 GO TO NEXT-TR-REPORT.


      * SKIP LOAN IF NOT DELETED WITH IN PERIOD:
           IF LN-POCD = "**"
              IF LN-POFFDATE > END-DATE
                 GO TO NEXT-TR-REPORT
              ELSE
                 IF LN-POFFDATE < BEG-DATE
                    GO TO NEXT-TR-REPORT.

      * WE ALREADY KNOW IT WAS DELETED WITHIN ENTERED PERIOD,
      * NOW SKIP IF BOOKED WITHIN SAME PERIOD
           IF LN-POCD = "**"
              IF LN-SALEFIN = "Y"
                 MOVE LN-PURDATE TO SYS-DATE
              ELSE
                 MOVE LN-LOANDATE TO SYS-DATE
              END-IF
              IF SYS-DATE NOT > END-DATE
                 IF SYS-DATE NOT < BEG-DATE
                    GO TO NEXT-TR-REPORT.

           MOVE LN-CLASS TO LC-CODE.
      *    PERFORM GET-LC.

           IF FIRST-TIME = "Y"
              MOVE LN-OWNBR TO HOLD-LN-OWNBR
              MOVE LN-CLASS TO HOLD-LN-CLASS
              PERFORM SET-CLASS-HEADINGS
              PERFORM SET-BRANCH-HEADINGS.

           IF LN-OWNBR NOT = HOLD-LN-OWNBR
              MOVE "Y" TO BR-TOTALS
              PERFORM CLASS-TOTALS
              PERFORM BRANCH-TOTALS
              MOVE LN-OWNBR TO HOLD-LN-OWNBR
              MOVE LN-CLASS TO HOLD-LN-CLASS
              PERFORM SET-CLASS-HEADINGS
              PERFORM SET-BRANCH-HEADINGS.

           IF LN-CLASS NOT = HOLD-LN-CLASS
              PERFORM CLASS-TOTALS
              MOVE LN-CLASS TO HOLD-LN-CLASS
              PERFORM SET-CLASS-HEADINGS.

           PERFORM DETAIL-LINE-PROCESS.

           GO TO NEXT-TR-REPORT.

       END-OF-GROUP.
      *     PERFORM CLOSE-LN1-FILE.
      *     PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-TRP1-FILE.
      *     PERFORM CLOSE-TRL1-FILE.
      *     PERFORM CLOSE-WK2-FILE.
      *     PERFORM OPEN-WK2-FILE-OUTPUT.
      *     PERFORM CLOSE-WK2-FILE.
      *     PERFORM OPEN-WK2-FILE.
           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM.

           MOVE "Y" TO BR-TOTALS.
           IF SUM-ONLY NOT = "Y"
              PERFORM CLASS-TOTALS
              PERFORM BRANCH-TOTALS.
           PERFORM SUMMARY-TOTALS.
           PERFORM CORPORATE-TOTALS.

           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************

           MOVE NONACR-QUEUE-POS TO Q-POS.
           MOVE NONACR-COPIES-POS TO C-POS.
           MOVE NONACR-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

      *     MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
      *     PERFORM MKTEMP-CALL.
      *     MOVE MKTEMP-BUF TO WK2-PATH.
      *     PERFORM OPEN-WK2-FILE-OUTPUT.
      *     PERFORM CLOSE-WK2-FILE.
      *     PERFORM OPEN-WK2-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           PERFORM OPEN-WK1-FILE-OUTPUT.
           PERFORM CLOSE-WK1-FILE.
           PERFORM OPEN-WK1-FILE.

           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BRANCH TO SV-BEG-BR
              MOVE END-BRANCH TO SV-END-BR
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP TO SV-ENT-GROUP
              MOVE EXT-EOBUF-BRNO TO BEG-BRANCH END-BRANCH
              MOVE "N" TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE
           ELSE
              IF ( BEG-BRANCH = 0 AND END-BRANCH = 0 )
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC  TO BEG-BRANCH END-BRANCH.

           PERFORM CLEAR-TOTALS.

       SET-HEAD.
           MOVE BEG-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO H-BEGDATE.

           MOVE END-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO H-ENDDATE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           MOVE " " TO LEGEND.
           PERFORM SEND-MSEL.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
      *    PERFORM OPEN-SP1-FILE.
      *    PERFORM OPEN-LC1-FILE.

           PERFORM ENABLE-INTERRUPT.
           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      *************************************
       EO-EXEC-AUDIT SECTION.
      *************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE.

      *************************************
       ENTER-PARAMETERS SECTION.
      *************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO PRTBAL.

       BEGDT.
      * BATCH WILL USE DATE RANGE:
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO BR-01.
           MOVE "F7-EXIT, F1, ENTER BEGINNING DATE :" TO LEGEND.
           PERFORM SEND-MSEL.
           MOVE "E NM080BEGDT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEGDT.
           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.
       ENDDT.
           MOVE "F7-EXIT, F1, ^-BACK, ENTER ENDING DATE :" TO LEGEND.
           PERFORM SEND-MSEL.
           MOVE "E NM080ENDDT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BEGDT.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENDDT.

           MOVE VDU-DATE-YYYYMMDD TO END-DATE.
           IF END-DATE < BEG-DATE
              GO TO ENDDT.

       CHECK-DATE-RANGE.
           IF (BEG-MM NOT = END-MM) OR (BEG-YY NOT = END-YY) OR
               (BEG-CC NOT = END-CC)
              MOVE "BEGINNING & ENDING DATES MUST BE IN SAME MONTH!"
                                                           TO MESS
                 PERFORM SEND-MESS
                 GO TO BEGDT.

       BR-01.
           MOVE "F7-EXIT, F1, F3-ALL, ^, ENTER BEGINNING BRANCH"
                                                     TO LEGEND.
           PERFORM SEND-MSEL.
           MOVE "E  A040BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO ENDDT.
           IF VDUSTATUS = "1U" GO TO ENDDT.
           IF VDUSTATUS = "16" GO TO ALL-BR.

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
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO CL-01.
      *----------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
      *----------------------------------------------------------
       BR-02.
           MOVE "F7-EXIT, F1, ^-BACK, ENTER ENDING BRANCH" TO LEGEND.
           PERFORM SEND-MSEL.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.
           IF BEG-BRANCH > END-BRANCH
              MOVE " INVALID BRANCH RANGE !" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           IF BEG-BRANCH = 0
              IF END-BRANCH = 0
                 MOVE "S  A160DISPLAY-USER." TO VDU
                 MOVE "USER BRANCH ONLY" TO VDUBUF
                 PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           GO TO CL-01. 
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.
       CL-01.
           MOVE "F7-EXIT, F1, F3-ALL, ^, ENTER BEGINNING CLASS"
                                                     TO LEGEND.
           PERFORM SEND-MSEL.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1<"
              MOVE "BEG" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
                 GO TO CL-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-01.
           MOVE VDUNUM0 TO BEG-CLASS.
       CL-02.
           MOVE "F7-EXIT, F1, ^-BACK, ENTER ENDING CLASS" TO LEGEND.
           PERFORM SEND-MSEL.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF VDUSTATUS = "1<"
              MOVE "END" TO CLASS-FG
              PERFORM CLASS-SCAN
              IF IO-FG NOT = 0
              GO TO CL-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-02.
           MOVE VDUNUM0 TO END-CLASS.
           IF END-CLASS = 0
              GO TO CL-02.
           IF BEG-CLASS > END-CLASS
              MOVE " INVALID LOAN CLASS RANGE !" TO MESS
              PERFORM SEND-MESS
              GO TO CL-01.
           GO TO DL-NO.
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.
       DL-NO.
           MOVE "F7-EXIT, F1, F3-ALL, ^, ENTER DEALER NO"
                                                     TO LEGEND.
           PERFORM SEND-MSEL.
           MOVE "ENNN042DLNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-DL.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DL-NO.
           MOVE VDUNUM2 TO DEALER.
           GO TO SUMYN.
       ALL-DL.
           MOVE "S  A070DLNO." TO VDU.
           MOVE "****.**" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE 0 TO DEALER.
       SUMYN.
           MOVE "F7-EXIT, F1, ^-BACK, SUMMARY ONLY (Y/N):" TO LEGEND.
           PERFORM SEND-MSEL.
           MOVE "E  A010SUMONLY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DL-NO.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO SUMYN.
           MOVE VDUBUF TO SUM-ONLY.
           IF NOT (SUM-ONLY = "N" OR "Y") GO TO SUMYN.

       PRTBAL.
           MOVE "F7-EXIT, F1, ^, PRINT CURRENT ACCOUNT BALANCE (Y/N):"
                                                        TO LEGEND.
           PERFORM SEND-MSEL.
           MOVE "E  A010PRTBAL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO SUMYN.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO PRTBAL.
           MOVE VDUBUF TO PRT-BAL.
           IF NOT (PRT-BAL = "N" OR "Y") GO TO PRTBAL.
           IF PRT-BAL = "Y"
              MOVE HEAD6-3B TO HEAD6-3
              MOVE SUMMARY3-1B TO SUMMARY3-1.

       ACCEPT-IT.
           MOVE "F7-EXIT, F1, ^-BACK, ACCEPT (Y/N):" TO LEGEND.
           PERFORM SEND-MSEL.
       EXIT-PARM.
           EXIT.

      ***********************************
       DISPLAY-PARAMETERS SECTION.
      ***********************************
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

           MOVE "SN N020CL1." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N020CL2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
           IF DEALER = 0
              MOVE "S  A070DLNO." TO VDU
              MOVE "****.**" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN062DLNO." TO VDU
              MOVE DEALER TO VDUNUM2
              PERFORM SCREENIO.
           MOVE "S  A010SUMONLY." TO VDU.
           MOVE SUM-ONLY TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010PRTBAL." TO VDU.
           MOVE PRT-BAL TO VDUBUF.
           PERFORM SCREENIO.

      ***********************************
       DETAIL-LINE-PROCESS SECTION.
      ***********************************

           IF WK2-INT = 0 AND WK2-SRV = 0 AND WK2-MFEE = 0
                          AND WK2-DISC = 0 AND
                         WK2-ADDON = 0 AND WK2-OTHER = 0 AND
                WK2-INT-RB = 0 AND WK2-SRV-RB = 0 AND WK2-MFEE-RB = 0
                   GO TO DETAIL-LINE-PROCESS-EXIT.

           IF FIRST-TIME = "Y"
              MOVE 1 TO LN-FEED
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE "N" TO FIRST-TIME.

      *     PERFORM GET-SP1-FILE.

              IF (SPA-ORGST = ' ' OR SPA-ORGST = NULL)
                 GO TO IO-ERROR.

           MOVE LN-PRIOR-UNEARNED TO UNEARN.

           PERFORM PROCESS-BORROWER.
           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO W-LOAN-NUM.
           MOVE LOAN-NUM TO D-LOAN-NUM.
           IF LN-POCD = "**"
              MOVE "(VOID)" TO D-VOID.
           MOVE WK2-NON-ACCRUAL-DATE TO WS-DATE.
           MOVE WS-YYYY              TO DATE-YYYYMMDD-YYYY.
           MOVE WS-MM                TO DATE-YYYYMMDD-MM.
           MOVE WS-DD                TO DATE-YYYYMMDD-DD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-DATE. 

      *    MOVE UNEARN TO D-UNEARN.
           MOVE WK2-INT TO D-INT.
           MOVE WK2-ADDON TO D-ADDON.
           MOVE WK2-SRV TO D-SRV.
           MOVE WK2-MFEE TO D-MFEE.
           MOVE WK2-DISC TO D-DISC.
           MOVE WK2-OTHER TO D-OTHER.
           ADD WK2-INT WK2-ADDON WK2-SRV WK2-MFEE WK2-DISC WK2-OTHER
               GIVING ACCRUED.
           MOVE ACCRUED TO D-TOTNON.
           IF PRT-BAL = "Y"
              MOVE LN-CURBAL TO ENDING D-BALANCE.
           PERFORM WRITE-DETAIL-LINE.

           IF WK2-INT-RB = 0 AND WK2-SRV-RB = 0 AND WK2-MFEE-RB = 0
              NEXT SENTENCE
           ELSE
              MOVE WK2-INT-RB TO D-INT
              MOVE WK2-SRV-RB TO D-SRV
              MOVE WK2-MFEE-RB TO D-MFEE
              PERFORM SET-D-R
              PERFORM WRITE-DETAIL-LINE.

           PERFORM UPDATE-WK-FILE.
           ADD 1 TO T-NOACCTS(1) T-NOACCTS(2).
           ADD UNEARN TO T-UNEARN(1) T-UNEARN(2).
           ADD WK2-INT TO T-INT(1) T-INT(2).
           ADD WK2-INT-RB TO T-INT-RB(1) T-INT-RB(2).
           ADD WK2-ADDON TO T-ADDON(1) T-ADDON(2).
           ADD WK2-SRV TO T-SRV(1) T-SRV(2).
           ADD WK2-SRV-RB TO T-SRV-RB(1) T-SRV-RB(2).
           ADD WK2-MFEE TO T-MFEE(1) T-MFEE(2).
           ADD WK2-MFEE-RB TO T-MFEE-RB(1) T-MFEE-RB(2).
           ADD WK2-DISC TO T-DISC(1) T-DISC(2).
           ADD WK2-OTHER TO T-OTHER(1) T-OTHER(2).
           ADD ACCRUED TO T-ACCRUED(1) T-ACCRUED(2).
           ADD ENDING TO T-ENDING(1) T-ENDING(2).

       DETAIL-LINE-PROCESS-EXIT.
           EXIT.

      ***********************************
       SORT-ROUTINE SECTION.
      ***********************************

           PERFORM ENABLE-INTERRUPT.

      * WORLD WANTED A VOIDED LOAN THAT WENT THRU THE ACCRUAL & THEN VOIDED
      * TO SHOW NEGATIVE EARNINGS ON THIS REPORT.
      *    PERFORM SORT-TRL-FILE.

           MOVE BR-NO               TO TP-BRNO
                                       QTRP1-WBEG-BRNO
                                       QTRP1-WEND-BRNO.
           MOVE BEG-DATE            TO TP-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRP1-WBEG-DATE.
           MOVE END-DATE            TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRP1-WEND-DATE.

           MOVE BEG-CLASS           TO TP-CLASS
                                       QTRP1-WBEG-CLASS.
           MOVE END-CLASS           TO QTRP1-WEND-CLASS.
           MOVE 0                   TO TP-ACCTNO TP-LINE
                                       QTRP1-WBEG-ACCTNO
                                       QTRP1-WBEG-LINE
                                       QTRL1-WBEG-LAWCODE.
           MOVE ALL "9"             TO QTRP1-WEND-ACCTNO
                                       QTRP1-WEND-LINE
                                       QTRL1-WEND-LAWCODE.

           MOVE TP-DATE              TO NDTE-DATE.
           MOVE 1                    TO NDTE-HOLD.
           PERFORM INCREMENT-DAYS.
           MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QTRP1-WSBEG-DATE.

           IF ( TP-CLASS = 999 )
              MOVE TP-CLASS          TO QTRP1-WSBEG-CLASS
           ELSE
              COMPUTE QTRP1-WSBEG-CLASS  = TP-CLASS + 1.

           IF ( TP-ACCTNO = 99999999 )
              MOVE TP-ACCTNO         TO QTRP1-WSBEG-ACCTNO
           ELSE
              COMPUTE QTRP1-WSBEG-ACCTNO = TP-ACCTNO + 1.

           IF ( TP-LINE = 999 )
              MOVE TP-LINE           TO QTRP1-WSBEG-LINE
           ELSE
              COMPUTE QTRP1-WSBEG-LINE   = TP-LINE + 1.

           PERFORM START-TRP1-FILE.

      *SORT-TR-NEXT.
      *    PERFORM READ-TRP1-FILE-NEXT.
      *    IF (IO-FG NOT = 0) OR (TRP-PATH-OWNBR NOT = TP-BRNO)
      *       GO TO SORT-ROUTINE-EXIT.
 
      *    PERFORM NPR-TRAP-INTERRUPT.

      *    IF TP-DATE > END-DATE
      *       GO TO SORT-ROUTINE-EXIT.

      *    IF TP-PLCD NOT = " "
      *       IF TP-TRCD = "RI" OR "RS" OR "RM"
      *         GO TO SORT-TR-NEXT.

      *    IF TP-CLASS < BEG-CLASS OR TP-CLASS > END-CLASS
      *       GO TO SORT-TR-NEXT.

      *    IF TP-OTHBR-FG = "C"
      *       GO TO SORT-TR-NEXT.

      *    MOVE TP-BRNO   TO WK2-BRANCH.
      *    MOVE TP-CLASS  TO WK2-CLASS.
      *    MOVE TP-ACCTNO TO WK2-ACCTNO.
      *    PERFORM READ-WK2-FILE.
      *    IF IO-GOOD
      *       GO TO UPDATE-WK2-REC.

      *    MOVE TP-BRNO   TO WK2-BRANCH.
      *    MOVE TP-CLASS  TO WK2-CLASS.
      *    MOVE TP-ACCTNO TO WK2-ACCTNO.

      *    MOVE TP-DATE   TO WK2-DATE.
      *    MOVE TP-CODE   TO WK2-CODE.
      *    MOVE TP-LINE   TO WK2-LINE.

      *    MOVE 0 TO WK2-EARN-WORK-TAB(1) WK2-EARN-WORK-TAB(2)
      *              WK2-EARN-WORK-TAB(3) WK2-EARN-WORK-TAB(4)
      *              WK2-EARN-WORK-TAB(5) WK2-EARN-WORK-TAB(6).
      *    MOVE 0 TO WK2-INT-RB WK2-SRV-RB WK2-MFEE-RB.
      *    MOVE 0 TO WK2-NON-ACCRUAL-DATE.

      *    PERFORM WRITE-WK2-FILE.
      *    IF IO-FG NOT = 0
      *       GO TO IO-ERROR.

      *UPDATE-WK2-REC.

      *    IF TP-TRCD = "RI"
      *       ADD TP-TRAMT TO WK2-INT-RB
      *    ELSE
      *    IF TP-TRCD = "RS"
      *       ADD TP-TRAMT TO WK2-SRV-RB
      *    ELSE
      *    IF TP-TRCD = "RM"
      *       ADD TP-TRAMT TO WK2-MFEE-RB.

      *    PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
      *       COMPUTE WK2-EARN-WORK-TAB(SUB) =
      *               WK2-EARN-WORK-TAB(SUB) + TP-EARNINGS(SUB)
      *    END-PERFORM.

      *    COMPUTE TEST-EARNINGS =
      *       TP-EARNINGS(1) + TP-EARNINGS(2) + TP-EARNINGS(3) +
      *       TP-EARNINGS(4) + TP-EARNINGS(5) + TP-EARNINGS(6).

      *    IF TEST-EARNINGS NOT = 0
      *       MOVE TP-DATE TO WK2-NON-ACCRUAL-DATE.

      *    PERFORM REWRITE-WK2-FILE.
      *    IF IO-FG NOT = 0
      *       GO TO IO-ERROR.

      *    GO TO SORT-TR-NEXT.

       SORT-ROUTINE-EXIT.
      *    PERFORM CLOSE-WK2-FILE.
           PERFORM DISABLE-INTERRUPT.

      ******************************************
      *SORT-TRL-FILE SECTION.
      ******************************************

      * WORLD WANTED A VOIDED LOAN THAT WENT THRU THE ACCRUAL & THEN VOIDED
      * TO SHOW NEGATIVE EARNINGS ON THIS REPORT.

      *    MOVE BR-NO               TO TL-BRNO
      *                                QTRL1-WBEG-BRNO
      *                                QTRL1-WEND-BRNO.
      *    MOVE BEG-DATE            TO TL-DATE SQL-DATE-YYYYMMDD.
      *    PERFORM SQL-SET-DATE.
      *    MOVE SQL-DATE-YYYY-MM-DD TO QTRL1-WBEG-DATE.
      *    MOVE END-DATE            TO  SQL-DATE-YYYYMMDD.
      *    PERFORM SQL-SET-DATE.
      *    MOVE SQL-DATE-YYYY-MM-DD TO QTRL1-WEND-DATE.
      *    MOVE BEG-CLASS           TO TL-CLASS
      *                                QTRL1-WBEG-CLASS.
      *    MOVE END-CLASS           TO QTRL1-WEND-CLASS.
      *    MOVE 0                   TO TL-ACCTNO TL-LAWCODE TL-LINE
      *                                QTRL1-WBEG-ACCTNO
      *                                QTRL1-WBEG-LAWCODE
      *                                QTRL1-WBEG-LINE.
      *    MOVE ALL "9"             TO QTRL1-WEND-ACCTNO
      *                                QTRL1-WEND-LAWCODE
      *                                QTRL1-WEND-LINE.
      *    PERFORM START-TRL1-FILE.

      *SORT-TRL-NEXT.
      *    PERFORM READ-TRL1-FILE-NEXT.
      *    IF (IO-FG NOT = 0) OR (TRL-PATH-OWNBR NOT = TL-BRNO)
      *       GO TO SORT-TRL-FILE-EXIT.

      *    PERFORM NPR-TRAP-INTERRUPT.

      *    IF TL-DATE > END-DATE
      *       GO TO SORT-TRL-FILE-EXIT.
 
      *    IF TL-LNAMT NOT < 0
      *       GO TO SORT-TRL-NEXT.

      *    IF TL-CLASS < BEG-CLASS OR TL-CLASS > END-CLASS
      *       GO TO SORT-TRL-NEXT.

      *    MOVE TL-BRNO   TO WK2-BRANCH.
      *    MOVE TL-CLASS  TO WK2-CLASS.
      *    MOVE TL-ACCTNO TO WK2-ACCTNO.
      *    PERFORM READ-WK2-FILE.
      *    IF IO-GOOD
      *       GO TO UPDATE-TRL-WK2-REC.

      *    MOVE TL-BRNO   TO WK2-BRANCH.
      *    MOVE TL-CLASS  TO WK2-CLASS.
      *    MOVE TL-ACCTNO TO WK2-ACCTNO.

      *    MOVE TL-DATE   TO WK2-DATE.
      *    MOVE TL-CODE   TO WK2-CODE.
      *    MOVE TL-LINE   TO WK2-LINE.

      *    MOVE 0 TO WK2-EARN-WORK-TAB(1) WK2-EARN-WORK-TAB(2)
      *              WK2-EARN-WORK-TAB(3) WK2-EARN-WORK-TAB(4)
      *              WK2-EARN-WORK-TAB(5) WK2-EARN-WORK-TAB(6).
      *    MOVE 0 TO WK2-INT-RB WK2-SRV-RB WK2-MFEE-RB.
      *    MOVE 0 TO WK2-NON-ACCRUAL-DATE.

      *    PERFORM WRITE-WK2-FILE.
      *    IF IO-FG NOT = 0
      *       GO TO IO-ERROR.

      *UPDATE-TRL-WK2-REC.

      *    PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
      *       COMPUTE WK2-EARN-WORK-TAB(SUB) =
      *               WK2-EARN-WORK-TAB(SUB) + TL-EARNINGS(SUB)
      *    END-PERFORM.

      *    COMPUTE TEST-EARNINGS =
      *       TL-EARNINGS(1) + TL-EARNINGS(2) + TL-EARNINGS(3) +
      *       TL-EARNINGS(4) + TL-EARNINGS(5) + TL-EARNINGS(6).

      *    IF TEST-EARNINGS NOT = 0
      *       MOVE TL-DATE TO WK2-NON-ACCRUAL-DATE.

      *    PERFORM REWRITE-WK2-FILE.
      *    IF IO-FG NOT = 0
      *       GO TO IO-ERROR.

      *    GO TO SORT-TRL-NEXT.

      *SORT-TRL-FILE-EXIT.
      *    EXIT.

      ***********************************
       SET-D-R SECTION.
      ***********************************
           IF D-INT NOT = " "
              IF D-INT-R = " "
                 MOVE "R" TO D-INT-R.
           IF D-SRV NOT = " "
              IF D-SRV-R = " "
                 MOVE "R" TO D-SRV-R.
           IF D-MFEE NOT = " "
              IF D-MFEE-R = " "
                 MOVE "R" TO D-MFEE-R.

      *******************************
       PRINT-RANGE-LINES SECTION.
      *******************************
           MOVE 5 TO LN-FEED.
           MOVE "BEGINNING BRANCH   :" TO RANGE-LINE.
           MOVE BEG-BRANCH             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING             :" TO RANGE-LINE.
           MOVE END-BRANCH             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING CLASS    :" TO RANGE-LINE.
           MOVE BEG-CLASS              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING             :" TO RANGE-LINE.
           MOVE END-CLASS              TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DEALER NO          :" TO RANGE-LINE.
           IF DEALER = 0
              MOVE "****.**"           TO RANGE-SELECTION
           ELSE
              MOVE DEALER              TO RANGE-SELECTION-NUM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SUMMARY ONLY       :" TO RANGE-LINE.
           MOVE SUM-ONLY               TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

      *****************************************
       UPDATE-WK-FILE SECTION.
      *****************************************
       UPDATE-WK.
           MOVE LN-OWNBR TO WK-BRANCH.
           MOVE LN-CLASS TO WK-CLASS.
           PERFORM READ-WK1-FILE.
           IF IO-FG = 0
              GO TO UPDATE-WK-FIELDS.

           MOVE " " TO WK-REC.
           MOVE LN-OWNBR TO WK-BRANCH.
           MOVE LN-CLASS TO WK-CLASS.
           MOVE 0 TO WK-NOACCTS WK-UNEARN WK-INT WK-INT-RB WK-ADDON
                     WK-SRV WK-SRV-RB WK-MFEE WK-MFEE-RB WK-DISC
                     WK-OTHER WK-ACCRUED WK-ENDING.
           PERFORM WRITE-WK1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           GO TO UPDATE-WK.

       UPDATE-WK-FIELDS.
           ADD 1 TO WK-NOACCTS.
           ADD UNEARN TO WK-UNEARN.
           ADD WK2-INT TO WK-INT.
           ADD WK2-INT-RB TO WK-INT-RB.
           ADD WK2-ADDON TO WK-ADDON.
           ADD WK2-SRV TO WK-SRV.
           ADD WK2-SRV-RB TO WK-SRV-RB.
           ADD WK2-MFEE TO WK-MFEE.
           ADD WK2-MFEE-RB TO WK-MFEE-RB.
           ADD WK2-DISC TO WK-DISC.
           ADD WK2-OTHER TO WK-OTHER.
           ADD ACCRUED TO WK-ACCRUED.
           ADD ENDING TO WK-ENDING.
           PERFORM REWRITE-WK1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.
       END-UPDATE-WK-FILE.
           EXIT.

      **************************************
       PROCESS-BORROWER SECTION.
      **************************************
           MOVE LN-SSNO(1) TO BW-SSNO.
      *    PERFORM READ-BW1-FILE.
           IF (BW-OWNBR = ' ' OR BW-OWNBR = NULL)
              MOVE "**NOT ON FILE**" TO D-BWNAME
              GO TO END-PROCESS-BORROWER.

           MOVE BW-LNAME TO NAME-WORKER.
           MOVE BW-FNAME TO FIRST-NAME.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?".
           INSPECT NAME-WORKER REPLACING FIRST "                "
               BY FIRST-NAME.
           INSPECT NAME-WORKER REPLACING ALL "?" BY " ".
           MOVE NAME-WORKER TO D-BWNAME.

       END-PROCESS-BORROWER.
           EXIT.

      *******************************
       HEAD-PAGE SECTION.
      *******************************
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.
           IF SUMMARY-FG = "Y"
              GO TO HEAD-SUMMARY.

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
           MOVE HEAD5 TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE HEAD6 TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 6 TO LN-COUNT.
           MOVE 2 TO LN-FEED.
           GO TO END-HEAD-PAGE.

       HEAD-SUMMARY.
           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE SUMMARY1 TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE SUMMARY2 TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE SUMMARY3 TO PR-REC.
           PERFORM WRITE-PR-REC.

           MOVE 5 TO LN-COUNT.
           MOVE 2 TO LN-FEED.
       END-HEAD-PAGE.
           EXIT.

      ****************************
       WRITE-DETAIL-LINE SECTION.
      ****************************
           IF SUM-ONLY = "Y"
              IF SUMMARY-FG = "N"
                 GO TO WRITE-DETAIL-LINE-EXIT.
           ADD LN-FEED TO LN-COUNT.
           IF LN-COUNT > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC
           PERFORM WRITE-PR-REC.

       WRITE-DETAIL-LINE-EXIT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ****************************************
       CLASS-TOTALS SECTION.
      ****************************************
           MOVE 3 TO LN-FEED.
           MOVE "CLASS TOTALS:" TO D-TAG.
           MOVE 1 TO SUB.
           PERFORM PRINT-TOTALS.
           IF BR-TOTALS = "N"
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE 1 TO LN-FEED.
       END-CLASS-TOTALS.
           MOVE 0 TO T-NOACCTS(1) T-UNEARN(1) T-INT(1) T-INT-RB(1)
                     T-ADDON(1) T-SRV(1) T-SRV-RB(1) T-MFEE(1)
                     T-MFEE-RB(1) T-DISC(1) T-OTHER(1)
                     T-ACCRUED(1) T-ENDING(1).

      ****************************************
       BRANCH-TOTALS SECTION.
      ****************************************
           MOVE 2 TO LN-FEED.
           MOVE 2 TO SUB.
           IF SUMMARY-FG = "N"
              MOVE "BRANCH TOTALS:" TO D-TAG
              PERFORM PRINT-TOTALS
           ELSE
              MOVE "   ---- TOTAL:" TO D-TAG
              PERFORM PRINT-BRANCH-SUM.
           IF SUMMARY-FG = "N"
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE 1 TO LN-FEED.
           MOVE "N" TO BR-TOTALS.
       END-BRANCH-TOTALS.
           MOVE 0 TO T-NOACCTS(2) T-UNEARN(2) T-INT(2) T-INT-RB(2)
                     T-ADDON(2) T-SRV(2) T-SRV-RB(2) T-MFEE(2)
                     T-MFEE-RB(2) T-DISC(2) T-OTHER(2)
                     T-ACCRUED(2) T-ENDING(2).

      ****************************************
       PRINT-CORPORATE-TOTS SECTION.
      ****************************************
           MOVE 2 TO LN-FEED.

           MOVE "   ---- TOTAL:" TO D-TAG.
           MOVE 1 TO SUB.
           PERFORM PRINT-BRANCH-SUM.
       END-CORP-TOTALS.
           MOVE 0 TO T-NOACCTS(2) T-UNEARN(2) T-INT(2) T-INT-RB(2)
                     T-ADDON(2) T-SRV(2) T-SRV-RB(2) T-MFEE(2)
                     T-MFEE-RB(2) T-DISC(2) T-OTHER(2)
                     T-ACCRUED(2) T-ENDING(2).

      ****************************************
       PRINT-TOTALS SECTION.
      ****************************************
           MOVE T-NOACCTS(SUB) TO D-T-NOACCTS-T.
           MOVE "#" TO D-T-TAG-T.
           MOVE T-ADDON(SUB) TO D-T-ADDON-T.
           MOVE T-MFEE(SUB) TO D-T-MFEE-T.
           MOVE T-OTHER(SUB) TO D-T-OTHER-T.
           MOVE T-ENDING(SUB) TO D-T-BALANCE-T.
           PERFORM WRITE-DETAIL-LINE.
           IF T-MFEE-RB(SUB) NOT = 0
              MOVE T-MFEE-RB(SUB) TO D-T-MFEE-T
              IF D-T-MFEE-T-R = " "
                 MOVE "R" TO D-T-MFEE-T-R
                 PERFORM WRITE-DETAIL-LINE
              ELSE
                 PERFORM WRITE-DETAIL-LINE.
           MOVE T-INT(SUB) TO D-T-INT-T.
           MOVE T-SRV(SUB) TO D-T-SRV-T.
           MOVE T-DISC(SUB) TO D-T-DISC-T.
           MOVE T-ACCRUED(SUB) TO D-T-TOTNON-T.
           PERFORM WRITE-DETAIL-LINE.

           IF T-INT-RB(SUB) = 0 AND T-SRV-RB(SUB) = 0
              NEXT SENTENCE
           ELSE
              MOVE T-INT-RB(SUB) TO D-T-INT-T
              MOVE T-SRV-RB(SUB) TO D-T-SRV-T
              PERFORM SET-D-T-T-R
              PERFORM WRITE-DETAIL-LINE.

      ****************************************
       SET-D-T-T-R SECTION.
      ****************************************
           IF D-T-INT-T NOT = " "
              IF D-T-INT-T-R = " "
                 MOVE "R" TO D-T-INT-T-R.
           IF D-T-SRV-T NOT = " "
              IF D-T-SRV-T-R = " "
                 MOVE "R" TO D-T-SRV-T-R.

      ****************************************
       PRINT-BRANCH-SUM SECTION.
      ****************************************
           MOVE T-NOACCTS(SUB) TO D-T-NOACCTS.
           MOVE "#" TO D-T-TAG.
           MOVE T-ADDON(SUB) TO D-T-ADDON.
           MOVE T-MFEE(SUB) TO D-T-MFEE.
           MOVE T-OTHER(SUB) TO D-T-OTHER.
           MOVE T-INT(SUB) TO D-T-INT.
           MOVE T-SRV(SUB) TO D-T-SRV.
           MOVE T-DISC(SUB) TO D-T-DISC.
           MOVE T-ACCRUED(SUB) TO D-T-TOTNON.
           MOVE T-ENDING(SUB) TO D-T-BALANCE.
           PERFORM WRITE-DETAIL-LINE.

           IF T-INT-RB(SUB) = 0
                 AND T-SRV-RB(SUB) = 0
                  AND T-MFEE-RB(SUB) = 0
              NEXT SENTENCE
           ELSE
              MOVE T-INT-RB(SUB) TO D-T-INT
              MOVE T-SRV-RB(SUB) TO D-T-SRV
              MOVE T-MFEE-RB(SUB) TO D-T-MFEE
              PERFORM SET-D-T-R
              PERFORM WRITE-DETAIL-LINE.

      ****************************************
       SET-D-T-R SECTION.
      ****************************************
           IF D-T-INT NOT = " "
              IF D-T-INT-R = " "
                 MOVE "R" TO D-T-INT-R.
           IF D-T-SRV NOT = " "
              IF D-T-SRV-R = " "
                 MOVE "R" TO D-T-SRV-R.
           IF D-T-MFEE NOT = " "
              IF D-T-MFEE-R = " "
                 MOVE "R" TO D-T-MFEE-R.

      ****************************************
       SUMMARY-TOTALS SECTION.
      ****************************************
           MOVE "Y" TO SUMMARY-FG FIRST-TIME.
           MOVE 0 TO T-NOACCTS(2) T-UNEARN(2) T-INT(2) T-INT-RB(2)
                     T-ADDON(2) T-SRV(2) T-SRV-RB(2) T-MFEE(2)
                     T-MFEE-RB(2) T-DISC(2) T-OTHER(2)
                     T-ACCRUED(2) T-ENDING(2).
           PERFORM CLOSE-WK1-FILE.
           PERFORM OPEN-WK1-FILE.
           MOVE 0 TO WK-BRANCH WK-CLASS.
           PERFORM START-WK1-FILE.
       SUMMARY-NEXT-WK.
           PERFORM READ-WK1-FILE-NEXT.
           IF IO-FG = 9
              PERFORM BRANCH-TOTALS
              GO TO END-SUMMARY-TOTALS.

           IF FIRST-TIME = "Y"
              MOVE WK-BRANCH TO HOLD-LN-OWNBR BR-NO
              PERFORM READ-BR-FILE
              MOVE BR-NO TO D-T-BRNO
              MOVE BR-NAME TO D-T-BRDESC
              MOVE NEW-BRANCH TO DETAIL-LINE.

           IF WK-BRANCH NOT = HOLD-LN-OWNBR
              PERFORM BRANCH-TOTALS
              MOVE 2 TO LN-FEED
              MOVE WK-BRANCH TO HOLD-LN-OWNBR BR-NO
              PERFORM READ-BR-FILE
              MOVE BR-NO TO D-T-BRNO
              MOVE BR-NAME TO D-T-BRDESC
              MOVE NEW-BRANCH TO DETAIL-LINE
              MOVE 3 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE 2 TO LN-FEED.

           IF FIRST-TIME = "Y"
              MOVE 1 TO LN-FEED
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              PERFORM WRITE-DETAIL-LINE
              MOVE 2 TO LN-FEED
              MOVE "N" TO FIRST-TIME.

           MOVE WK-CLASS TO LC-CODE.
           PERFORM GET-LC.
           MOVE LC-CODE TO D-T-CLASS.
           MOVE WK-NOACCTS TO D-T-NOACCTS.
           MOVE WK-INT TO D-T-INT.
           MOVE WK-ADDON TO D-T-ADDON.
           MOVE WK-SRV TO D-T-SRV.
           MOVE WK-MFEE TO D-T-MFEE.
           MOVE WK-DISC TO D-T-DISC.
           MOVE WK-OTHER TO D-T-OTHER.
           MOVE WK-ACCRUED TO D-T-TOTNON.
           MOVE WK-ENDING TO D-T-BALANCE.
           PERFORM WRITE-DETAIL-LINE.

           IF WK-INT-RB = 0
                 AND WK-SRV-RB = 0
                  AND WK-MFEE-RB= 0
              MOVE 2 TO LN-FEED
           ELSE
              MOVE WK-INT-RB TO D-T-INT
              MOVE WK-SRV-RB TO D-T-SRV
              MOVE WK-MFEE-RB TO D-T-MFEE
              PERFORM SET-D-T-R
              PERFORM WRITE-DETAIL-LINE
              MOVE 2 TO LN-FEED.

           ADD WK-NOACCTS TO T-NOACCTS(2).
           ADD WK-UNEARN TO T-UNEARN(2).
           ADD WK-INT TO T-INT(2).
           ADD WK-INT-RB TO T-INT-RB(2).
           ADD WK-ADDON TO T-ADDON(2).
           ADD WK-SRV TO T-SRV(2).
           ADD WK-SRV-RB TO T-SRV-RB(2).
           ADD WK-MFEE TO T-MFEE(2).
           ADD WK-MFEE-RB TO T-MFEE-RB(2).
           ADD WK-DISC TO T-DISC(2).
           ADD WK-OTHER TO T-OTHER(2).
           ADD WK-ACCRUED TO T-ACCRUED(2).
           ADD WK-ENDING TO T-ENDING(2).
           GO TO SUMMARY-NEXT-WK.

       END-SUMMARY-TOTALS.
           MOVE 0 TO PAGE-NUMBER.

      ****************************************
       CORPORATE-TOTALS SECTION.
      ****************************************
           MOVE "Y" TO SUMMARY-FG FIRST-TIME.
           MOVE 0 TO T-NOACCTS(1) T-UNEARN(1) T-INT(1) T-INT-RB(1)
                     T-ADDON(1) T-SRV(1) T-SRV-RB(1) T-MFEE(1)
                     T-MFEE-RB(1) T-DISC(1) T-OTHER(1)
                     T-ACCRUED(1) T-ENDING(1).
           MOVE 0 TO T-NOACCTS(2) T-UNEARN(2) T-INT(2) T-INT-RB(2)
                     T-ADDON(2) T-SRV(2) T-SRV-RB(2) T-MFEE(2)
                     T-MFEE-RB(2) T-DISC(2) T-OTHER(2)
                     T-ACCRUED(2) T-ENDING(2).
           PERFORM CLOSE-WK1-FILE.
           PERFORM OPEN-WK1-FILE.
           MOVE 0 TO WK-CLASS.
           PERFORM START-WKX-FILE.
       CORPORATE-NEXT-WK.
           PERFORM READ-WKX-FILE-NEXT.
           IF IO-FG = 9
              PERFORM PRINT-CORPORATE
              PERFORM PRINT-CORPORATE-TOTS
              GO TO END-CORPORATE-TOTALS.

           IF FIRST-TIME = "Y"
              MOVE 3 TO LN-FEED
              MOVE "CORPORATE TOTALS:" TO DETAIL-LINE
              PERFORM WRITE-DETAIL-LINE
              MOVE 2 TO LN-FEED
              MOVE WK-CLASS TO HOLD-LN-CLASS
              MOVE "N" TO FIRST-TIME.
           IF WK-CLASS NOT = HOLD-LN-CLASS
              PERFORM PRINT-CORPORATE
              MOVE WK-CLASS TO HOLD-LN-CLASS.

           ADD WK-NOACCTS TO T-NOACCTS(2).
           ADD WK-UNEARN TO T-UNEARN(2).
           ADD WK-INT TO T-INT(2).
           ADD WK-INT-RB TO T-INT-RB(2).
           ADD WK-ADDON TO T-ADDON(2).
           ADD WK-SRV TO T-SRV(2).
           ADD WK-SRV-RB TO T-SRV-RB(2).
           ADD WK-MFEE TO T-MFEE(2).
           ADD WK-MFEE-RB TO T-MFEE-RB(2).
           ADD WK-DISC TO T-DISC(2).
           ADD WK-OTHER TO T-OTHER(2).
           ADD WK-ACCRUED TO T-ACCRUED(2).
           ADD WK-ENDING TO T-ENDING(2).
           GO TO CORPORATE-NEXT-WK.

       END-CORPORATE-TOTALS.
           PERFORM CLOSE-WK1-FILE.

      ****************************************
       PRINT-CORPORATE SECTION.
      ****************************************
           MOVE HOLD-LN-CLASS TO LC-CODE.
           PERFORM GET-LC.
           MOVE LC-CODE TO D-T-CLASS.
           MOVE T-NOACCTS(2) TO D-T-NOACCTS.
           MOVE "#" TO D-T-TAG.
           MOVE T-INT(2) TO D-T-INT.
           MOVE T-ADDON(2) TO D-T-ADDON.
           MOVE T-SRV(2) TO D-T-SRV.
           MOVE T-MFEE(2) TO D-T-MFEE.
           MOVE T-DISC(2) TO D-T-DISC.
           MOVE T-OTHER(2) TO D-T-OTHER.
           MOVE T-ACCRUED(2) TO D-T-TOTNON.
           MOVE T-ENDING(2) TO D-T-BALANCE.
           PERFORM WRITE-DETAIL-LINE.

           IF T-INT-RB(SUB) = 0
                 AND T-SRV-RB(SUB) = 0
                  AND T-MFEE-RB(SUB) = 0
              MOVE 2 TO LN-FEED
           ELSE
              MOVE T-INT-RB(SUB) TO D-T-INT
              MOVE T-SRV-RB(SUB) TO D-T-SRV
              MOVE T-MFEE-RB(SUB) TO D-T-MFEE
              PERFORM SET-D-T-R
              PERFORM WRITE-DETAIL-LINE
              MOVE 2 TO LN-FEED.

           ADD T-NOACCTS(2) TO T-NOACCTS(1).
           ADD T-UNEARN(2) TO T-UNEARN(1).
           ADD T-INT(2) TO T-INT(1).
           ADD T-INT-RB(2) TO T-INT-RB(1).
           ADD T-ADDON(2) TO T-ADDON(1).
           ADD T-SRV(2) TO T-SRV(1).
           ADD T-SRV-RB(2) TO T-SRV-RB(1).
           ADD T-MFEE(2) TO T-MFEE(1).
           ADD T-MFEE-RB(2) TO T-MFEE-RB(1).
           ADD T-DISC(2) TO T-DISC(1).
           ADD T-OTHER(2) TO T-OTHER(1).
           ADD T-ACCRUED(2) TO T-ACCRUED(1).
           ADD T-ENDING(2) TO T-ENDING(1).

           MOVE 0 TO T-NOACCTS(2) T-UNEARN(2) T-INT(2) T-INT-RB(2)
                     T-ADDON(2) T-SRV(2) T-SRV-RB(2) T-MFEE(2)
                     T-MFEE-RB(2) T-DISC(2) T-OTHER(2)
                     T-ACCRUED(2) T-ENDING(2).

      *********************************
       CLASS-SCAN SECTION.
      *********************************
           MOVE 0 TO LCSCAN-KEY.
           MOVE " " TO LCSCAN-VERIFY.
           MOVE LCSCAN-BUF TO SCAN-BUF.
           PERFORM CALL-WINDOW-PROGRAM.
           MOVE SCAN-BUF TO LCSCAN-BUF.
           IF SCAN-STATUS NOT = "00"
            OR LCSCAN-KEY = 0
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

      *********************************************
       FORM-RESET SECTION.
      *********************************************
           MOVE EXT-CONAME-CONAME TO NONACR-CONAME.
           MOVE EXT-CONAME-SYSDATE TO NONACR-SYSDATE.
           DISPLAY NONACR-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE NONACR-FORM-FIELDS TO WR-BUF. 
           MOVE NONACR-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           MOVE NONACR-HELP--FILE TO HELP-LINK-FILE.
           MOVE NONACR-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/NONACR_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.


       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".

      *********************************
       MISC-PARAGRAPHS SECTION.
      *********************************
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
      *--------------------------------------------------------
       GET-SP1-FILE.
           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR.
      *--------------------------------------------------------
       CLEAR-TOTALS.
           MOVE 0 TO T-NOACCTS(1)   T-NOACCTS(2)
                     T-UNEARN(1)    T-UNEARN(2)
                     T-INT(1)       T-INT(2)
                     T-INT-RB(1)    T-INT-RB(2)
                     T-ADDON(1)     T-ADDON(2)
                     T-SRV(1)       T-SRV(2)
                     T-SRV-RB(1)    T-SRV-RB(2)
                     T-MFEE(1)      T-MFEE(2)
                     T-MFEE-RB(1)   T-MFEE-RB(2)
                     T-DISC(1)      T-DISC(2)
                     T-OTHER(1)     T-OTHER(2)
                     T-ACCRUED(1)   T-ACCRUED(2)
                     T-ENDING(1)    T-ENDING(2).
       GET-LC.
           IF LC-CODE NOT = LAST-CLASS-READ
              MOVE LC-CODE TO LAST-CLASS-READ
              PERFORM READ-LC1-FILE
              IF IO-FG = 9
                 MOVE "*** NOT FOUND ***" TO LC-DESC.
      *---------------------------------------------------------
       SET-BRANCH-HEADINGS.
           MOVE HOLD-LN-OWNBR TO BR-NO H-BRNO.
      *----PERFORM GET-BR.
           MOVE BR-NAME TO H-BRDESC.
      *---------------------------------------------------------
       SET-CLASS-HEADINGS.
           MOVE HOLD-LN-CLASS TO H-CLASS LC-CODE.
           PERFORM GET-LC.
           MOVE LC-DESC TO H-LCDESC.
      *--------------------------------------------------------
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
      *--------------------------------------------------------
       SEND-MSEL.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      **********************************
       IO-ROUTINE SECTION.
      **********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRPGS_SQL.CPY".
       COPY "LIBLP/LPTRLGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
      *COPY "LIBLP/LPTRP1RN.CPY".
      * COPYMEMBER: LIBLP/LPTRP1RN
      * 20220923 BAH ADDED TP-LCDUE *1570
       LOAD-TRP1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRP-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRP-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRP-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRP-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRP-ALL-DATA
              MOVE TRP-ALL-PATH             TO TRP-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRP-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRP-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRP-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRP-ALL-DATA
              MOVE TRP-ALL-PATH             TO TRP-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-TRPFILE NOT = "S" )
              MOVE TRP-PATH-BEG             TO TRP-SELECT-PATH-BEG
              MOVE TRP-PATH-FILE            TO TRP-SELECT-PATH-FILE
           ELSE
              MOVE TRP-PATH-SQL             TO TRP-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-TRP1-FILE.
           PERFORM LOAD-TRP1-FILE.
           PERFORM OPEN-IT.
           MOVE TRP-SELECT-PATH TO E-FILE.

           EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS WK2_CONNECT
                    USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
           END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-TRP1-FILE.
           PERFORM START-IT.
           MOVE TRP-PATH-OWNBR  TO TP-BRNO.
           MOVE TRP-SELECT-PATH TO E-FILE.
           MOVE TRP1-KEY        TO E-KEYX.

           IF ( TRP1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRP1-FILE.

           IF ( QTRP1-WEND-LINE = ZEROES    ) OR
              ( QTRP1-WEND-LINE NOT NUMERIC )
              MOVE TRP-PATH-OWNBR        TO QTRP1-WBEG-BRNO
              MOVE TRP-PATH-OWNBR        TO QTRP1-WEND-BRNO
              MOVE TP-DATE               TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRP1-WBEG-DATE
              MOVE 99991231         TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRP1-WEND-DATE
              MOVE TP-CLASS             TO QTRP1-WBEG-CLASS
              MOVE TP-ACCTNO            TO QTRP1-WBEG-ACCTNO
              MOVE TP-LINE              TO QTRP1-WBEG-LINE
              MOVE ALL "9"              TO QTRP1-WEND-CLASS
                                           QTRP1-WEND-ACCTNO
                                           QTRP1-WEND-LINE
      *- - - -{ ADD ONE TO WHERE SCAN CONTINUE FIELDS
              MOVE TP-DATE              TO NDTE-DATE
              MOVE 1                    TO NDTE-HOLD
              PERFORM INCREMENT-DAYS
              MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRP1-WSBEG-DATE

              IF ( TP-CLASS = 999 )
                 MOVE TP-CLASS          TO QTRP1-WSBEG-CLASS
              ELSE
                 COMPUTE QTRP1-WSBEG-CLASS  = TP-CLASS + 1
              END-IF

              IF ( TP-ACCTNO = 99999999 )
                 MOVE TP-ACCTNO         TO QTRP1-WSBEG-ACCTNO
              ELSE
                 COMPUTE QTRP1-WSBEG-ACCTNO = TP-ACCTNO + 1
              END-IF

              IF ( TP-LINE = 999 )
                 MOVE TP-LINE           TO QTRP1-WSBEG-LINE
              ELSE
                 COMPUTE QTRP1-WSBEG-LINE   = TP-LINE + 1.

           EXEC SQL SET CONNECTION WK2_CONNECT END-EXEC.

           EXEC SQL
            DECLARE WK2_CURSOR CURSOR FOR
             :RET-CODE = EXEC SP_RZNACR_WK2_SELECT
            (:QTRP1-WBEG-BRNO, :QTRP1-WEND-BRNO, :QTRP1-WBEG-DATE,
             :QTRP1-WSBEG-DATE, :QTRP1-WEND-DATE, :QTRP1-WBEG-CLASS,
             :QTRP1-WSBEG-CLASS, :QTRP1-WEND-CLASS, :QTRP1-WBEG-ACCTNO,
             :QTRP1-WEND-ACCTNO, :QTRP1-WSBEG-ACCTNO, :QTRP1-WBEG-LINE,
             :QTRP1-WEND-LINE, :QTRL1-WBEG-LAWCODE, :QTRL1-WEND-LAWCODE) 
           END-EXEC.

           EXEC SQL
               OPEN WK2_CURSOR
           END-EXEC.

      *    PERFORM SQL-IO-VALIDATION.

      *    MOVE ZEROES TO QTRP1-WBEG-BRNO
      *                   QTRP1-WEND-BRNO
      *                   QTRP1-WBEG-CLASS
      *                   QTRP1-WEND-CLASS
      *                   QTRP1-WBEG-ACCTNO
      *                   QTRP1-WEND-ACCTNO
      *                   QTRP1-WBEG-LINE
      *                   QTRP1-WEND-LINE
      *                   QTRP1-WSBEG-CLASS
      *                   QTRP1-WSBEG-ACCTNO
      *                   QTRP1-WSBEG-LINE
      *                   QTRL1-WBEG-LAWCODE
      *                   QTRL1-WEND-LAWCODE.
      *    MOVE SPACES TO QTRP1-WBEG-DATE
      *                   QTRP1-WEND-DATE
      *                   QTRP1-WSBEG-DATE.

           MOVE STAT---GOOD TO TRP1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-TRP1-FILE.
           PERFORM READ-IT.
           MOVE TRP-PATH-OWNBR  TO TP-BRNO. 
           MOVE TRP-SELECT-PATH TO E-FILE.
           MOVE TRP1-KEY        TO E-KEYX.

           MOVE TP-BRNO             TO QTP-BRNO.
           MOVE TP-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTP-DATE.
           MOVE TP-CLASS            TO QTP-CLASS.
           MOVE TP-ACCTNO           TO QTP-ACCTNO.
           MOVE TP-LINE             TO QTP-LINE.

           EXEC SQL
            SELECT TRPFILE.TP_BRNO,
                   ISNULL(CAST(TRPFILE.TP_DATE AS VARCHAR(10)),'0'),
                   TRPFILE.TP_CLASS,
                   TRPFILE.TP_ACCTNO,
                   TRPFILE.TP_LINE,
                   ISNULL(CAST(TRPFILE.TP_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   TRPFILE.TP_LAWCODE,
                   ISNULL(TRPFILE.TP_JDCD,''),
                   ISNULL(TRPFILE.TP_PLCD,''),
                   TRPFILE.TP_SSNO,
                   ISNULL(TRPFILE.TP_MAKER,''),
                   ISNULL(TRPFILE.TP_IBPC,''),
                   ISNULL(TRPFILE.TP_TRCD,''),
                   ISNULL(TRPFILE.TP_REFNO,''),
                   ISNULL(CAST(TRPFILE.TP_PAYDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRPFILE.TP_DUEDATE AS VARCHAR(10)),'0'),
                   TRPFILE.TP_TRAMT,
                   TRPFILE.TP_INTDUE,
                   TRPFILE.TP_APOTH,
                   TRPFILE.TP_APOT2,
                   TRPFILE.TP_APINT,
                   TRPFILE.TP_APLC,
                   TRPFILE.TP_APCUR,
                   TRPFILE.TP_GLNO_1,
                   TRPFILE.TP_GLAMT_1,
                   TRPFILE.TP_GLNO_2,
                   TRPFILE.TP_GLAMT_2,
                   TRPFILE.TP_GLNO_3,
                   TRPFILE.TP_GLAMT_3,
                   TRPFILE.TP_EARNINGS_1,
                   TRPFILE.TP_EARNINGS_2,
                   TRPFILE.TP_EARNINGS_3,
                   TRPFILE.TP_EARNINGS_4,
                   TRPFILE.TP_EARNINGS_5,
                   TRPFILE.TP_EARNINGS_6,
                   TRPFILE.TP_EARNINGS_7,
                   TRPFILE.TP_EARNINGS_8,
                   TRPFILE.TP_EARNINGS_9,
                   TRPFILE.TP_EARNINGS_10,
                   TRPFILE.TP_WORKAMT_1,
                   TRPFILE.TP_WORKAMT_2,
                   TRPFILE.TP_WORKAMT_3,
                   TRPFILE.TP_WORKAMT_4,
                   TRPFILE.TP_WORKAMT_5,
                   TRPFILE.TP_WORKAMT_6,
                   TRPFILE.TP_WORKAMT_7,
                   TRPFILE.TP_WORKAMT_8,
                   TRPFILE.TP_WORKAMT_9,
                   TRPFILE.TP_WORKAMT_10,
                   TRPFILE.TP_OTHBAL,
                   TRPFILE.TP_OT2BAL,
                   TRPFILE.TP_INTBAL,
                   TRPFILE.TP_LCBAL,
                   TRPFILE.TP_CURBAL,
                   ISNULL(CAST(TRPFILE.TP_PDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   TRPFILE.TP_DLNO,
                   TRPFILE.TP_DLPROC,
                   ISNULL(TRPFILE.TP_SERVACCT,''),
                   ISNULL(TRPFILE.TP_SALEFIN,''),
                   ISNULL(TRPFILE.TP_INSURED,''),
                   ISNULL(TRPFILE.TP_INSOURS,''),
                   ISNULL(TRPFILE.TP_LOANTYPE,''),
                   TRPFILE.TP_TILLNO,
                   ISNULL(CAST(TRPFILE.TP_LCPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   TRPFILE.TP_POSTING_BR,
                   ISNULL(TRPFILE.TP_OTHBR_FG,''),
                   ISNULL(TRPFILE.TP_REV,''),
                   ISNULL(TRPFILE.TP_REVTRCD,''),
                   TRPFILE.TP_REVSEQNO,
                   ISNULL(TRPFILE.TP_STATUSFG,''),
                   ISNULL(TRPFILE.TP_KEEPAPINT,''),
                   TRPFILE.TP_INTPAID,
                   TRPFILE.TP_PRIOR_POOLID,
                   ISNULL(TRPFILE.TP_INSTYPES,''),
                   TRPFILE.TP_INSEARN_1,
                   TRPFILE.TP_INSEARN_2,
                   TRPFILE.TP_INSEARN_3,
                   TRPFILE.TP_INSEARN_4,
                   TRPFILE.TP_INSEARN_5,
                   TRPFILE.TP_INSEARN_6,
                   TRPFILE.TP_INSEARN_7,
                   TRPFILE.TP_INSEARN_8,
                   TRPFILE.TP_INSEARN_9,
                   TRPFILE.TP_INSEARN_10,
                   TRPFILE.TP_INSWORKAMT_1,
                   TRPFILE.TP_INSWORKAMT_2,
                   TRPFILE.TP_INSWORKAMT_3,
                   TRPFILE.TP_INSWORKAMT_4,
                   TRPFILE.TP_INSWORKAMT_5,
                   TRPFILE.TP_INSWORKAMT_6,
                   TRPFILE.TP_INSWORKAMT_7,
                   TRPFILE.TP_INSWORKAMT_8,
                   TRPFILE.TP_INSWORKAMT_9,
                   TRPFILE.TP_INSWORKAMT_10,
                   TRPFILE.TP_DLRVCHGS_ADJ_1,
                   TRPFILE.TP_DLRVCHGS_ADJ_2,
                   TRPFILE.TP_DLRVCHGS_ADJ_3,
                   TRPFILE.TP_DLPARVHELD_ADJ,
                   TRPFILE.TP_DLPARVPAID_ADJ,
                   TRPFILE.TP_NEWSTAT_RATE,
                   ISNULL(TRPFILE.TP_POCD,''),
                   ISNULL(CAST(TRPFILE.TP_PODATE AS VARCHAR(10)),'0'),
                   ISNULL(TRPFILE.TP_DL_RVCHGS_CODE_1,''),
                   ISNULL(TRPFILE.TP_DL_RVCHGS_CODE_2,''),
                   ISNULL(TRPFILE.TP_DL_RVCHGS_CODE_3,''),
                   ISNULL(TRPFILE.TP_DL_PARVHELD_CODE,''),
                   ISNULL(TRPFILE.TP_DL_PARVPAID_CODE,''),
                   TRPFILE.TP_TOTNODEF,
                   TRPFILE.TP_LNAMT,
                   ISNULL(CAST(TRPFILE.TP_ORIG_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRPFILE.TP_OLD1ST_PYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRPFILE.TP_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   TRPFILE.TP_ORGTERM,
                   TRPFILE.TP_INSCOMM,
                   ISNULL(TRPFILE.TP_MANUAL_RB,''),
                   ISNULL(TRPFILE.TP_USERID,''),
                   TRPFILE.TP_POSTTIME,
                   ISNULL(TRPFILE.TP_PLTYPE,''),
                   TRPFILE.TP_PL_OTH_AR,
                   TRPFILE.TP_LP_DLNO,
                   ISNULL(TRPFILE.TP_REPOCD,''),
                   TRPFILE.TP_SPRCLASS,
                   ISNULL(TRPFILE.TP_MAX_DEFER_FG,''),
                   ISNULL(CAST(TRPFILE.TP_POSTDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(TRPFILE.TP_UNITPER_CD,''),
                   TRPFILE.TP_UNITPER_FREQ,
                   TRPFILE.TP_SP_DEFFRMLA,
                   ISNULL(TRPFILE.TP_COLLECTOR,''),
                   TRPFILE.TP_PENALTY_INT,
                   TRPFILE.TP_INSCOMP,
                   ISNULL(CAST(TRPFILE.TP_PROMDATE
                                                  AS VARCHAR(10)),'0'),
                   TRPFILE.TP_PROMAMT,
                   ISNULL(TRPFILE.TP_PROMID,''),
                   TRPFILE.TP_INS_TAX_RB,
                   TRPFILE.TP_PROMDAYS_PASTDUE,
                   ISNULL(TRPFILE.TP_MIP_FG,''),
                   TRPFILE.TP_MIP_CLPAID,
                   TRPFILE.TP_MIP_CLCOMM,
                   TRPFILE.TP_MIP_AHPAID,
                   TRPFILE.TP_MIP_AHCOMM,
                   ISNULL(CAST(TRPFILE.TP_MIP_CLPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(TRPFILE.TP_MIP_AHPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(TRPFILE.TP_INS_TAX_COUNTY_CD,''),
                   ISNULL(TRPFILE.TP_INS_TAX_CITY_CD,''),
                   TRPFILE.TP_APPLIED_TO_ESCROW,
                   TRPFILE.TP_ESCROW_AMT,
                   TRPFILE.TP_ESCROW_PREPAID,
                   TRPFILE.TP_ESCROW_DISBURSED,
                   TRPFILE.TP_LP_SEQNO,
                   ISNULL(TRPFILE.TP_PRIOR_DF_CODE,''),
                   ISNULL(CAST(TRPFILE.TP_PRIOR_DF_POSTDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(TRPFILE.TP_CANCEL_REASON,''),
                   TRPFILE.TP_FROMCL,
                   TRPFILE.TP_TOCL,
                   ISNULL(TRPFILE.TP_ADDON_TXT,''),
                   ISNULL(TRPFILE.TP_ADDON_TYP,''),
                   TRPFILE.TP_DLRVNUM,
                   ISNULL(TRPFILE.TP_P4P5_TRANCD,''),
                   TRPFILE.TP_P4P5_DLRVNUM,
                   TRPFILE.TP_POOLID,
                   ISNULL(TRPFILE.TP_ACTIONCD2,''),
                   TRPFILE.TP_LCDUE
            INTO :QTRP-REC
            FROM DBO.TRPFILE
            WHERE TRPFILE.TP_BRNO = :QTP-BRNO
              AND TRPFILE.TP_DATE = :QTP-DATE
              AND TRPFILE.TP_CLASS = :QTP-CLASS
              AND TRPFILE.TP_ACCTNO = :QTP-ACCTNO
              AND TRPFILE.TP_LINE = :QTP-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRP1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-TRP-FIELDS.

      *-----------------------------------------------------------------
       READ-TRP1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE TRP-PATH-OWNBR  TO TP-BRNO.
           MOVE TRP-SELECT-PATH TO E-FILE.
           MOVE TRP1-KEY        TO E-KEYX.
           INITIALIZE QTRP-REC.
           INITIALIZE QTRL-REC.
           INITIALIZE QLN-REC.
           INITIALIZE QBW-REC.
           INITIALIZE QLC-REC.
           INITIALIZE QSPA-REC.    

          EXEC SQL SET CONNECTION WK2_CONNECT END-EXEC.

              EXEC SQL
                   FETCH NEXT WK2_CURSOR INTO
                   :WK2-BRANCH
                  ,:WK2-CLASS
                  ,:WK2-ACCTNO
                  ,:WK2-DATE
                  ,:WK2-LINE
                  ,:WK2-INT
                  ,:WK2-ADDON
                  ,:WK2-SRV
                  ,:WK2-MFEE
                  ,:WK2-DISC
                  ,:WK2-OTHER
                  ,:WK2-INT-RB
                  ,:WK2-SRV-RB
                  ,:WK2-MFEE-RB
                  ,:WK2-NON-ACCRUAL-DATE
                  ,:QLN-OWNBR 
                  ,:QLN-ACCTNO 
                  ,:QLN-CLASS 
                  ,:QLN-SUBCLASS 
                  ,:QLN-LAWCODE 
                  ,:QLN-SSNO1 
                  ,:QLN-ORGST 
                  ,:QLN-SPRCLASS 
                  ,:QLN-POCD 
                  ,:QLN-SALEFIN 
                  ,:QLN-STATFLAGS 
                  ,:QLN-LOANDATE       
                  ,:QLN-PURDATE        
                  ,:QLN-ORIG-1STPYDATE 
                  ,:QLN-1STPYDATE 
                  ,:QLN-POFFDATE  
                  ,:QLN-ORGTERM 
                  ,:QLN-DLNO 
                  ,:QLN-CURBAL 
                  ,:QLN-PRIOR-UNEARNED 
                  ,:QLN-TOTNODEF 
                  ,:QLN-SUM-ORGTERM 
                  ,:QLN-UNITPER-CD 
                  ,:QLN-UNITPER-FREQ 
                  ,:QBW-OWNBR 
                  ,:QBW-SSNO 
                  ,:QBW-LNAME 
                  ,:QBW-FNAME 
                  ,:QBW-SPSSNO 
                  ,:QBW-SPLNAME 
                  ,:QBW-SPFNAME 
                  ,:QBW-SOLICIT 
                  ,:QLC-CODE 
                  ,:QLC-DESC 
                  ,:QSPA-ORGST 
                  ,:QSPA-SPRCLASS 
                  ,:QSPA-SUBCLASS 
                  ,:QSPA-LAWCODE 
                  ,:QSPA-EXPD-FRMLA


              END-EXEC.

      *    IF ( TRP1-CURSOR-STAT-GOOD )
      *       EXEC SQL
      *            FETCH TRP1_CURSOR INTO :QTRP-REC
      *       END-EXEC
      *    ELSE
      *       MOVE 9        TO SQLCODE
      *       MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRP1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRP-FIELDS
              PERFORM GET-TRL-FIELDS
              PERFORM GET-LN-FIELDS
              PERFORM GET-BW-FIELDS
              PERFORM GET-LC-FIELDS
              PERFORM GET-SPA-FIELDS.

         EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TRP1-FILE.

           PERFORM CLOSE-IT.

           EXEC SQL SET CONNECTION WK2_CONNECT END-EXEC.

           IF ( TRP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE WK2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRP1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

       COPY "LIBLP/LPTRL1RN.CPY".
      *COPY "LIBGB/GBWK2IN.CPY".
      *-----------------------------------------------------------
       OPEN-WK1-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WK1-IFN TO E-FILE.
           OPEN OUTPUT WK-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK1-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK-FILE
              GO TO OPEN-WK1-FILE-OUTPUT.
           UNLOCK WK-FILE.
      *----------------------------------------------------------
       OPEN-WK1-FILE.
           PERFORM OPEN-IT.
           MOVE WK1-IFN TO E-FILE.
           OPEN I-O WK-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK1-FILE.
           IF IO-FG = 7
              CLOSE WK-FILE
              GO TO OPEN-WK1-FILE.
           UNLOCK WK-FILE.
      *----------------------------------------------------------
       READ-WK1-FILE.
           PERFORM READ-IT.
           MOVE WK1-IFN TO E-FILE.
           MOVE WK1-KEY TO E-KEYX.
           READ WK-FILE.
           IF IO-FG = 8
              GO TO READ-WK1-FILE.
      *---------------------------------------------------------
       READ-WK1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE WK1-IFN TO E-FILE.
           MOVE WK1-KEY TO E-KEYX.
           READ WK-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-WK1-FILE-NEXT.
      *--------------------------------------------------------
       START-WK1-FILE.
           PERFORM START-IT.
           MOVE WK1-IFN TO E-FILE.
           MOVE WK1-KEY TO E-KEYX.
           START WK-FILE KEY NOT < WK1-KEY.
           UNLOCK WK-FILE.
      *--------------------------------------------------------
       WRITE-WK1-FILE.
           PERFORM WRITE-IT.
           MOVE WK1-IFN TO E-FILE.
           MOVE WK1-KEY TO E-KEYX.
           WRITE WK-REC.
           IF IO-FG = 8
              GO TO WRITE-WK1-FILE.
           UNLOCK WK-FILE.
      *-------------------------------------------------------
       REWRITE-WK1-FILE.
           PERFORM REWRITE-IT.
           MOVE WK1-IFN TO E-FILE.
           MOVE WK1-KEY TO E-KEYX.
           REWRITE WK-REC.
           IF IO-FG = 8
              GO TO REWRITE-WK1-FILE.
           UNLOCK WK-FILE.
      *------------------------------------------------------
       CLOSE-WK1-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-FILE.
      *------------------------------------------------------
       READ-WKX-FILE-NEXT.
           PERFORM READ-IT.
           MOVE MKTEMP-BUF TO E-FILE.
           MOVE WKX-KEY TO E-KEYX.
           READ WK-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-WKX-FILE-NEXT.
      *------------------------------------------------------
       START-WKX-FILE.
           PERFORM START-IT.
           MOVE MKTEMP-BUF TO E-FILE.
           MOVE WKX-KEY TO E-KEYX.
           START WK-FILE KEY NOT < WKX-KEY.
           UNLOCK WK-FILE.
      *------------------------------------------------------
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      **********************************
       END-PROGRAM SECTION.
      **********************************
       END-RUN.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
           MOVE WK2-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

       EXIT-PROG.
           IF SV-BEG-BR NOT = 9999
              IF EXT-EOBUF-GROUP-FG = "Y"
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH
                 MOVE SV-GROUP-FLAG TO GROUP-FLAG
                 MOVE SV-ENT-GROUP TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY NONACR-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
