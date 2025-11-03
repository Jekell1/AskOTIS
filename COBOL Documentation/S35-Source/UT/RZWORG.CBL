       IDENTIFICATION DIVISION.
       PROGRAM-ID.   EMWORG.
       DATE-WRITTEN. 110990.
      ******************************************************************
      *
      *                      (LP)
      *    DESCRIPTION:  MONTHLY WRITE-OFF ACTIVITY REGISTER
      *                  ALL TRANSACTIONS ON "PL"'S
      *
      * REV:
      *  042995 SLC ADDED PLTYPE LOGIC
      *  061295 BLL FIXED BAD WORK FILE & KEY REDEFINITION
      *  BAH 092895 RECORDS-FOUND INITIALIZED TO "N" INSTEAD OF " "
      *  JFF 100695 ADDED CLASS SCAN
      *  KEC 101295 COMMENTED OUT DEFAULT BRANCH SETTING IN INITIZATION.
      *  JFF 111595 CHANGED FORM TO INCLUDE LOCAL-SPOOLING QUESTION.
      *  KEC 113095 FIXED LOCAL-SPOOL FOR IF A BRANCH HAS NO DATA.
      *  JFF 121395 CHANGED "IF LOCAL-PRINT-FG = N" TO
      *             "IF LOCAL-PRINT-FG NOT = Y" IN INITIALIZATION.
      *  JFF 011696 ADDED LOGIC TO PRINT TOTALS FOR 'PL,P2,P3,P4,P5
      *             IN BRANCH, AND GRAND TOTALS.
      *  KEC 022796 FIXED THE TOTALS ROUTINES (TABLE) FOR A BREAKDOWN
      *             ON CLASS, BRANCH, AND GRAND TOTALS FOR PL'S.
      *  KEC 030596 FIXED PRINT OF DETAIL ANALYSIS AND THE ADDING OF
      *             TOTALS BY NOT ALLOWING 'MOVE' AND 'ADDING' IF THE
      *             TP-TRCD = "JD".
      *  JFF 031196 ADDED PRINT-RANGE-LINES SECTION.
      *  MJD 032096 CHANGED TO SKIP TP-OTHBR-FG = "C" ACCOUNTS.
      *             IF PLTYPE NOT = 2,3,4,5 THEN DEFAULT TO P.
      *             ADDED LOGIC TO SKIP RECOVERIES POSTED TO OTHER BRANCHES
      *             (TP-OTHBR-FG = "C")
      *  JFF 051496 ADDED GROUP LOGIC.
      *  MJD 071896 ADDED WEEKLY UNIT PERIOD LOGIC FOR 6 DIGIT PDTH DATES
      *  BLV 060597 CHANGED FOR LOCAL SPOOL WITH NEW BATCH DRIVER - BREXEC
      *  GLF 022098 DELETED BR-SECURITY-OK AND ADDED LIBGB/BRSECUREW.
      *  BAH 062598 CHANGED TP-TILL PIC 9 TO TP-TILLNO PIC X
      *  GLF 042999 FIXED PROBLEM WITH SKIPPING OF SOME DETAIL LINES
      *             WHEN PRINTING.
      *  BAH 990618 ADDED "ST"
      *  GLF 081999 ADD ARCHIVE PATH INFO. FOR TR FILES NOT FOUND IN DATA
      *             SET FOR SELECTED MONTH AND YEAR
      *  BLV 990826 FIX ARCHIVE PATH LOGIC
      *  MJD 000126 ADDED LOGIC TO EXCLUDE "OT" AND "PI" FROM RECOVERY
      *             TOTALS (AMERICAN PR#266)
      *  MJD 000127 FIXED BUG, DETAIL LINE WAS NOT PRINTING SIGN WITH
      *             NO OF TRANSACTIONS. IF # OF REVERSALS WAS > THE NUMBER
      *             OF POSTINGS FOR A PARTICULAR TRANSACTION, THEN THE COUNT DID
      *             NOT MATCH THE DETAIL.
      *  KEC 000731 {PR#01273} PARADATA FINANCIAL [JUDY]
      *              REPLACED THE PART IN THE DETAIL-LINE IN DISPLAYING
      *              THE ACCTNO WITH SOME STATUS FLAGS WITH STANDARD
      *              LAYOUT FROM LIBLP/LPSTAT (LPS-DISPLAY-LNNO).
      *  BAH 021008 FIXED RECORDS-FOUND TO NOT GIVE "NO RECORDS FOUND"
      *             ON A F7 EXIT
      *  BAH 030102 CHANGED WK2-LINE FROM 9(4) TO 9(2) COMP...WONT FIND
      *             TRANSACTIONS OVER 99...9(2) COMP ACTUALLY STORES THEM
      *             FIXED EMTRRG BACK IN 1998 FOR THIS...
      *  BAH 040903 REPLACE BR-NAME IN HEADING WITH BR-REGION-NUMBER
      *             FOR GP-I-AM-REGACC, REGACC PR# 77
      *  BAH 051207 EXPANDED TOTAL LINE COUNT BY 1 DIGIT, WORLD PR# 441
      *  BLF 060707 FIXED CLASS SUMMARY BY TR CODE INCLUDING LOWER #
      *             BRANCHES TR CODES & TOTALS.  WAS ONLY A PROBLEM
      *             WHEN RUN BY GROUP...WKFILE KEEPS TOTALS FOR GROUP
      *             SUMMARY AND CLASS TOTALS STARTED W/ CURRENT BRNO
      *             BUT DIDN'T CHECK BRNO NOT = HOLD-BRNO
      *  BAH 070703 DO NOT PRINT SSNO FOR WORLD, WORLD PR# 536
      *   CS 081030 IF TP-OTHERBR-FG = C, PRINT 'INTER-OFFICE' INSTEAD OF
      *             'NO BORROWER MASTER', CHANGED FOR/BY BRANCH NUMBER SO IT
      *             WOULD PRINT 4DIGIT BRANCH NO. PR#643 NORTHSTATE
      *  BAH 090114 TEST GP-HIDE-FIRST-5-SSNO, REGMGM PR# 3785
      * BAH 2016.1103 CHANGE TILLNO TO 9(2)
      *
      * KEC 2017.0223 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED WS CLASS FIELDS TO MATCH LN-CLASS & LC-CODE.
      *
      *  BAH 170531 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 20190926 REMOVED ACCESS-CALL TRPFILE
      *  BAH 20200423 WK-GL1-KEY WAS SHORT 1 BYTE, CAUSING WRITE ERROR
      *  BLM 20210810 REMOVE TP-CODE, TP-POOLID NO LONGER IN KEY
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
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
      *-------------------------------------------------
       01  WK-REC.
           03  WK-KEY.
               05  WK-CODE  PIC X.
               05  WK-BRNO  PIC 9(4).
               05  WK-CLASS PIC 9(3).
               05  WK-TRCD  PIC XX.
               05  FILLER   PIC X(4).
           03  WK-NO        PIC S9(7)  COMP-3.
           03  WK-TRAMT     PIC S9(9)V99  COMP-3.
           03  WK-APOTH     PIC S9(9)V99  COMP-3.
           03  WK-APINT     PIC S9(9)V99  COMP-3.
           03  WK-APLC      PIC S9(9)V99  COMP-3.
           03  WK-APCUR    PIC S9(9)V99  COMP-3.
           03  WK-EARNINGS  PIC S9(9)V99  COMP-3.
           03  WK-DLPROC    PIC S9(9)V99  COMP-3.

       01  WK-GL-REC.
           03  WK-GL1-KEY.
               05  FILLER   PIC X(8).
               05  WK-GLNO  PIC 9(11)     COMP-3.
           03  WK-GLAMT     PIC S9(7)V99  COMP-3.
           03  FILLER       PIC X(41).

      ******************************************************************
      *  INDEXED TRFILE BY BRANCH, CLASS, ACCOUNT
      ******************************************************************
       FD  WK2-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK2-REC.
           03  WK2-KEY.
               05  WK2-BRANCH            PIC 9(4).
               05  WK2-CLASS             PIC 9(3).
               05  WK2-ACCTNO            PIC 9(8).
               05  WK2-DATE              PIC 9(8).
      *        05  WK2-CODE              PIC X.
               05  WK2-LINE              PIC 9(3).

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./LP/EMWORG.CBL S34 08/10/21-15:03:11 >".

      ******************************************************************
      *     F I L E   I F N      *
      ******************************************************************
       COPY "LIBLP/LP01TRP.CPY".
      * COPY "LIBLP/LP01TRP_SQL.CPY".
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
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       01  WK2-IFN              PIC X(3)     VALUE "WK2".
       01  WK-IFN               PIC X(2)     VALUE "WK".
       01  WK2-PATH             PIC X(35).


      ******************************************************************
      *   LINE WORKERS     *
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  ER-FEED              PIC 9        VALUE 1.

      ******************************************************************
      *   MISC WORKERS     *
      ******************************************************************
       01  HOLD-EARNINGS        PIC S9(7)V99  COMP-3 VALUE 0.
       01  HOLD-TP-OTHBR-FG     PIC X                VALUE "?".
       01  HOLD-TP-ACCTNO       PIC 9(8)      COMP-3 VALUE 99999999.
       01  HOLD-TP-SSNO         PIC 9(10)     COMP-3 VALUE 9999999999.
       01  SUB                  PIC 9(6)      COMP-3.
       01  LEV                  PIC 99        COMP-3.
       01  LEV-PL               PIC 99        COMP-3.
       01  LEV-NEXT-1           PIC 99        COMP-3.
       01  HOLD-BRNO            PIC 9(4)  VALUE 0.
       01  HOLD-CLASS           PIC 9(3)  VALUE 0.
       01  RECORDS-FOUND        PIC X     VALUE " ".
           88  NO-RECORDS-FOUND           VALUE "N".
       01  SUMMARY-FG           PIC X     VALUE "N".
       01  GRAND-SUMMARY     PIC X     VALUE "N".
       01  NAME-WORKER          PIC X(34).
       01  FIRST-NAME           PIC X(16).

       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  MSEL-LIST            PIC X(5)     VALUE "MSEL.".
       01  TRP1-OPENED-SW       PIC X        VALUE "N".

      *-----------------------------------------------------------------
       01  MESSAGE-01-LOC-PRT   PIC X(43)    VALUE
           "CANNOT DO LOCAL SPOOLING WITH GROUP TOTALS".

      ******************************************************************
      *
      *    W S - G T T - T A B L E
      *
      ******************************************************************
       01  WS-GTT-TOTAL-TABLE.
           03  WS-GTT-TABLE-GROUP-LEVEL                 OCCURS 10 TIMES.
               05  WS-GTT-NO              PIC S9(7)     COMP.
               05  WS-GTT-TRAMT           PIC S9(9)V99  COMP.
               05  WS-GTT-APOTH           PIC S9(9)V99  COMP.
               05  WS-GTT-APINT           PIC S9(9)V99  COMP.
               05  WS-GTT-APLC            PIC S9(9)V99  COMP.
               05  WS-GTT-APCUR          PIC S9(9)V99  COMP.
               05  WS-GTT-EARNINGS        PIC S9(9)V99  COMP.
               05  WS-GTT-DLPROC          PIC S9(9)V99  COMP.

      *-----------------------------------------------------------------
       01  WS-HOLD-PROMPT       PIC X(7)  VALUE SPACES.

      ******************************************************************
      *  TOTALS LEV1 = CLASS TOTALS    | LEV-PL = 1 --- P
      *            2 = BRANCH TOTALS   | LEV-PL = 2 --- 2
      *            3 = GRAND TOTALS    | LEV-PL = 3 --- 3
      *                                | LEV-PL = 4 --- 4
      *                                | LEV-PL = 5 --- 5
      *                                | LEV-PL = 6 --- TOTAL
      ******************************************************************
       01  TOTALS-TAB   VALUE ZEROS.
           03  TOTAL-TBL       OCCURS 3.
               05  TOTAL-TBL2  OCCURS 6.
                   07  T-NO         PIC S9(7)    COMP-3.
                   07  T-TRAMT      PIC S9(9)V99 COMP-3.
                   07  T-APOTH      PIC S9(9)V99 COMP-3.
                   07  T-APINT      PIC S9(9)V99 COMP-3.
                   07  T-APLC       PIC S9(9)V99 COMP-3.
                   07  T-APCUR      PIC S9(9)V99 COMP-3.
                   07  T-EARNINGS   PIC S9(9)V99 COMP-3.
                   07  T-DLPROC     PIC S9(7)V99 COMP-3.
      *         RECOVERIES
                   07  T-R-NO       PIC S9(7)    COMP-3.
                   07  T-R-TRAMT    PIC S9(9)V99 COMP-3.
                   07  T-R-APOTH    PIC S9(9)V99 COMP-3.
                   07  T-R-APINT    PIC S9(9)V99 COMP-3.
                   07  T-R-APLC     PIC S9(9)V99 COMP-3.
                   07  T-R-APCUR    PIC S9(9)V99 COMP-3.
                   07  T-R-EARNINGS PIC S9(9)V99 COMP-3.
                   07  T-R-DLPROC   PIC S9(7)V99 COMP-3.
       01  PTYPE.
           03  PLTYPE               PIC X(6) VALUE
               "P23456".
           03  PL-TYPE REDEFINES PLTYPE
                  OCCURS 6 TIMES    PIC X(1).

       01  CLASS-FG             PIC X(3)       VALUE "   ".
           88  CLASS-BEG                       VALUE "BEG".
           88  CLASS-END                       VALUE "END".
       01  LEGEND               PIC X(70).

      ******************************************************************
      *     S C R E E N   F I E L D S   *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  BEG-CLASS       PIC 9(3).
           03  END-CLASS       PIC 9(3).
           03  END-DATE.
               05  END-MONTH   PIC 99.
               05  END-YEAR    PIC 99.
           03  EDATE REDEFINES END-DATE PIC 9999.
           03  DAY-OF-MONTH    PIC 99.
           03  GTT-ISO         PIC X(1).
           03  SUM-ONLY        PIC X.
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).
           03  LOCAL-PRINT-FG  PIC X(1).
           03  TOTAL-LEV-TABLE.
               05  TOTAL-LEVEL-TABLE  PIC X(1)  OCCURS 10.

       01  VERSION-CONTROL     PIC X VALUE "D".
       01  BEGINNING-DATE       PIC 9(8).
       01  ENDING-DATE          PIC 9(8).

       01  SV-BEG-BR           PIC 9(4)        VALUE 9999.
       01  SV-END-BR           PIC 9(4)        VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.
       01  SV-LOCAL-PRINT-FG   PIC X     VALUE SPACES.

      ******************************************************************
      *     P R I N T   B U F F E R S   *
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

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(30)    VALUE " ".
           03  FILLER           PIC X(38) VALUE
               "MONTHLY WRITE-OFF ACTIVITY REGISTER".

       01  HEAD3.
           03  FILLER           PIC X(10) VALUE "BRANCH :".
           03  H-BRNO           PIC 9(4).
           03  FILLER           PIC X VALUE " ".
           03  H-BRDESC         PIC X(20).
           03  FILLER           PIC X(46) VALUE " ".

       01  HEAD4.
           03  FILLER           PIC X(10) VALUE "CLASS  :".
           03  H-CLASS          PIC Z99.
           03  FILLER           PIC X VALUE " ".
           03  H-LCDESC         PIC X(20).

       01  HEAD5.
           03  FILLER           PIC X(18)    VALUE "ACCTNO".
           03  FILLER           PIC X(28)    VALUE
              "DEALER          IB".
           03  FILLER           PIC X(52)    VALUE
           "TR                      - APPLIED -".
           03  FILLER           PIC X(34)    VALUE
           "---- BALANCES -----          G/L".

       01  HEAD6.
           03  FILLER           PIC X(11)      VALUE
           "    TL".
           03  FILLER           PIC X(54)     VALUE
           "       PROC'D   EARNED PC PAY-DATE CD REFNO   AMOUNT".
           03  FILLER           PIC X(45)     VALUE
           "OT/INT/LC   PRIN  PDTH LCPD     OTH      INT".
           03  FILLER           PIC X(18)     VALUE
           "PRIN      ACCT-AMT".

       01  CLASS-HEAD.
           03  FILLER           PIC X(55)    VALUE
           "          TRANS CODE   NUMBER     AMOUNT    APP-OTH    ".
           03  FILLER           PIC X(48)    VALUE
           "     APP-INT      APP-LC      APP-PRIN  EARNINGS".
           03  FILLER           PIC X(11)    VALUE
           "    DL-PROC".
       01  GRAND-HEAD.
           03  FILLER           PIC X(55)    VALUE
           "CLASS     TRANS CODE   NUMBER     AMOUNT    APP-OTH    ".
           03  FILLER           PIC X(48)    VALUE
           "     APP-INT      APP-LC      APP-PRIN  EARNINGS".
           03  FILLER           PIC X(11)    VALUE
           "    DL-PROC".

      *-----------------------------------------------------------------
       01  GTT-HEAD1.
           03  FILLER            PIC X(57)  VALUE SPACES.
           03  FILLER            PIC X(18)  VALUE "GROUP TOTALS TABLE".
           03  FILLER            PIC X(57)  VALUE SPACES.

      *-----------------------------------------------------------------
       01  GTT-HEAD2.
           03  FILLER            PIC X(24)  VALUE
               "BRANCH/GROUP   GR       ".
           03  FILLER            PIC X(9)   VALUE "NUMBER   ".
           03  FILLER            PIC X(11)  VALUE "TRANS      ".
           03  FILLER            PIC X(16)  VALUE "APPLIED         ".
           03  FILLER            PIC X(12)  VALUE "APPLIED     ".
           03  FILLER            PIC X(12)  VALUE "APPLIED     ".
           03  FILLER            PIC X(14)  VALUE "APPLIED       ".
           03  FILLER            PIC X(12)  VALUE "EARNINGS    ".
           03  FILLER            PIC X(11)  VALUE "DEALER     ".

      *-----------------------------------------------------------------
       01  GTT-HEAD3.
           03  FILLER            PIC X(14)  VALUE SPACES.
           03  FILLER            PIC X(10)  VALUE "LEV       ".
           03  FILLER            PIC X(7)   VALUE "       ".
           03  FILLER            PIC X(11)  VALUE "  AMOUNT   ".
           03  FILLER            PIC X(16)  VALUE "  OTHER         ".
           03  FILLER            PIC X(12)  VALUE "  INTEREST  ".
           03  FILLER            PIC X(12)  VALUE "  LT-CHG    ".
           03  FILLER            PIC X(20)  VALUE "  PRINCIPAL        ".
           03  FILLER            PIC X(16)  VALUE "        PROCEDES".

       01  GL-HEAD.
           03  FILLER           PIC X(17)    VALUE "G/L DIST  SUMMARY".

       01  DETAIL-LINE          VALUE SPACES.
           03  FILLER           PIC X(116).
           03  D-BRXX           PIC X(12).
           03  D-BRX            PIC 9(4).

       01  D-LINE1 REDEFINES DETAIL-LINE.

           03  D-DISPLAY-LNNO   PIC X(16).
           03  FILLER           PIC X(03).
           03  D-NAME           PIC X(26).
           03  FILLER           PIC X.
           03  D-MAKER          PIC X.
           03  FILLER           PIC XX.
           03  D-SSNO           PIC 999/99/9999 BLANK ZERO.
           03  FILLER           PIC X(5).
           03  D-DLNO           PIC 9999.99.

       01  D-LINE2 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(4).
           03  D-TILL           PIC Z9.
           03  FILLER           PIC X(9).
           03  D-DLPROC         PIC ZZZZZ.ZZ-.
           03  D-EARNINGS       PIC ZZZZZ.ZZ-.
           03  FILLER           PIC X.
           03  D-IBPC           PIC X.
           03  FILLER           PIC X.
           03  D-DATE           PIC 99/99/99.
           03  FILLER           PIC X.
           03  D-TRCD           PIC XX.
           03  D-REV            PIC X.
           03  D-REFNO          PIC X(5).
           03  FILLER           PIC X.
           03  D-TRAMT          PIC ZZZZZ.ZZ-.
           03  D-APINT          PIC ZZZZZ.ZZ-.
           03  D-APCUR          PIC ZZZZZ.ZZ-.
           03  FILLER           PIC X.
           03  D-PDTHRU-MO      PIC 99.
           03  D-PDTHRU-DAYR    PIC 99.
           03  FILLER           PIC X.
           03  D-LCPD-MO        PIC 99.
           03  D-LCPD-DAYR      PIC 99.
           03  D-OTHBAL         PIC ZZZZZ.ZZ-.
           03  D-INTBAL         PIC ZZZZZ.ZZ-.
           03  D-PRINBAL        PIC ZZZZ9.99-.
           03  FILLER           PIC X(2).
           03  D-GLNO           PIC ZZZ9/9999999.
           03  D-GLAMT          REDEFINES D-GLNO
                                PIC ZZZZZZZZ.ZZ-.
           03  D-PAIDOFF        REDEFINES D-GLNO
                                PIC X(12).

       01  D-LINE4 REDEFINES DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  D2-GROUP             PIC X(19).
           03  D2-GROUP-1           REDEFINES D2-GROUP.
               05  DS-CLASS         PIC Z99.
               05  FILLER           PIC X(16).
           03  D2-GROUP-2           REDEFINES D2-GROUP.
               05  D2-PROMPT1       PIC X(7).
               05  FILLER           PIC X(2).
               05  D2-GRNO          PIC X(4).
               05  D2-BRNO          REDEFINES D2-GRNO
                                    PIC 9(4).
               05  FILLER           PIC X(3).
               05  D2-GR-LEVEL      PIC 9(1).
               05  FILLER           PIC X(2).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  DS-TRCD          PIC XX.
           03  FILLER           PIC X.
           03  DS-NO            PIC ZZZZ-.
           03  FILLER           PIC X.
           03  DS-TRAMT         PIC ZZZZZZZZZ.ZZ-.
           03  DS-APOTH         PIC ZZZZZZZZZ.ZZ-.
           03  DS-APINT         PIC ZZZZZZZZZ.ZZ-.
           03  DS-APLC          PIC ZZZZZZZZZ.ZZ-.
           03  DS-APCUR        PIC ZZZZZZZZZ.ZZ-.
           03  DS-EARNINGS      PIC ZZZZZZZZZ.ZZ-.
           03  DS-DLPROC        PIC ZZZZZZZZZ.ZZ-.


       01  D-LINE5 REDEFINES DETAIL-LINE.
           03  DS-GLNO          PIC ZZZ9/9999999.
           03  FILLER           PIC XX.
           03  DS-GLAMT         PIC ZZZZ9.99-.

      *-----------------------------------------------------------------
       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(35).
           03  RANGE-SELECTION.
               04  R-NUM        PIC Z(5)9.
               04  R-NUM-REDEF  REDEFINES R-NUM.
                   05 R-CHAR    PIC X(6).

      *****************************************************************
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBLP/EMSFRG_CMD.CPY".
       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBWI/TSTINTW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBWI/LCSCANW.CPY".
       COPY "LIBWI/GRTOTLW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/EMWORG_DEF.CPY".
       COPY "LIBLP/EMWORG_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/EMWORG_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE WK2-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK-FILE WK2-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *****************************************************************
      *
      *     M A I N   P R O G R A M
      *
      *****************************************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME  TO FORM-PATH.
           MOVE "EMWORG"       TO FORM-PROG VDU-FORM-NAME.

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
           IF LOCAL-PRINT-FG = "Y"
              PERFORM CLOSE-PR-FILE
              PERFORM CREATE-SPOOL-DIR
              IF STAT NOT = "00"
                 GO TO NEXT-GROUP-LOC.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BR-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              BW-PATH-OVERRIDE
                                              TRP-PATH-OVERRIDE.

           IF TRP1-OPENED-SW = "N"
               PERFORM OPEN-TRP1-FILE
               MOVE "Y" TO TRP1-OPENED-SW.
      *    PERFORM OPEN-TRP1-FILE.
	  
           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN1-FILE.

           PERFORM SORT-ROUTINE.

           IF EXT-EOBUF-ERRCD = "X"
              GO TO NEXT-GROUP-LOC.

           PERFORM OPEN-WK2-FILE.

      *----------------------------------------------------------------
       NEXT-TR-REPORT.
           PERFORM READ-WK2-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-OF-GROUP.

           PERFORM TRAP-INTERRUPT.

           MOVE WK2-DATE    TO TP-DATE.
           MOVE WK2-BRANCH  TO TP-BRNO.
      *    MOVE WK2-CODE    TO TP-CODE.
      *    MOVE 0           TO TP-POOLID.
           MOVE WK2-CLASS   TO TP-CLASS.
           MOVE WK2-ACCTNO  TO TP-ACCTNO.
           MOVE WK2-LINE    TO TP-LINE.
           PERFORM READ-TRP1-FILE.
           IF IO-FG = 9
              MOVE "TRANSACTION RECORD MISSING SINCE SORT" TO MESS
              PERFORM SEND-MESS
              GO TO IO-ERROR.

           IF FIRST-TIME = "Y"
              MOVE TP-BRNO  TO HOLD-BRNO
              MOVE TP-CLASS TO HOLD-CLASS
              PERFORM SET-CLASS-HEADINGS
              PERFORM SET-BRANCH-HEADINGS.

           IF TP-BRNO NOT = HOLD-BRNO
              PERFORM CLASS-TOTALS
              PERFORM BRANCH-TOTALS
              MOVE TP-BRNO TO HOLD-BRNO
              MOVE TP-CLASS TO HOLD-CLASS
              PERFORM SET-CLASS-HEADINGS
              PERFORM SET-BRANCH-HEADINGS
              IF ( GTT-ISO NOT = "O" )
                 IF SUM-ONLY = "N"
                    PERFORM HEAD-PAGE.

           IF TP-CLASS NOT = HOLD-CLASS
              PERFORM CLASS-TOTALS
              MOVE TP-CLASS TO HOLD-CLASS
              PERFORM SET-CLASS-HEADINGS
              IF ( GTT-ISO NOT = "O" )
                 IF SUM-ONLY = "N"
                    PERFORM HEAD-PAGE.

           PERFORM DETAIL-LINE-PROCESS.

           MOVE 1 TO LEV.
           PERFORM CREATE-WK.

           GO TO NEXT-TR-REPORT.

      *----------------------------------------------------------------
       END-OF-GROUP.
           PERFORM CLOSE-TRP1-FILE.
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

      *----------------------------------------------------------------
       END-ROUTINE.
           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.

           IF LOCAL-PRINT-FG = "N"
              PERFORM CLASS-TOTALS
              PERFORM BRANCH-TOTALS
              MOVE "Y" TO GRAND-SUMMARY
              PERFORM GRAND-TOTALS
              IF ( GTT-ISO NOT = "S" )
                 MOVE 99 TO LN-COUNT
                 PERFORM GROUP-TOTAL-TABLE
                 PERFORM PRINT-RANGE-LINES
                 PERFORM GL-SUMMARY
              ELSE
                 PERFORM PRINT-RANGE-LINES
                 PERFORM GL-SUMMARY.

           GO TO END-PROGRAM.

      *****************************************************************
       INITIALIZATION SECTION.
      *****************************************************************
           MOVE EMWORG-QUEUE-POS TO Q-POS.
           MOVE EMWORG-COPIES-POS TO C-POS.
           MOVE EMWORG-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           MOVE SPACES TO RECORDS-FOUND.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * CREATE INDEXED KEY FILE FOR TRFILE OUTPUT
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.

           IF STAT NOT = "00"
              MOVE "CANNOT MAKE WK2 SORT KEY FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           MOVE MKTEMP-BUF TO WK2-PATH.
           PERFORM OPEN-WK2-FILE-OUTPUT.

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

           IF ( LOCAL-PRINT-FG NOT = "Y" )
              PERFORM OPEN-PR-FILE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-LC1-FILE.
           PERFORM ENABLE-INTERRUPT.

           MOVE 1 TO SUB.
           PERFORM ZERO-TOTAL-TBL
              VARYING LEV-PL FROM 1 BY 1
                 UNTIL LEV-PL > 6.
           MOVE 2 TO SUB.
           PERFORM ZERO-TOTAL-TBL
              VARYING LEV-PL FROM 1 BY 1
                 UNTIL LEV-PL > 6.
           MOVE 3 TO SUB.
           PERFORM ZERO-TOTAL-TBL
              VARYING LEV-PL FROM 1 BY 1
                 UNTIL LEV-PL > 6.
           MOVE 0 TO PAGE-NUMBER.

           PERFORM OPEN-WK2-FILE.

           MOVE SPACES TO WK2-KEY.
           MOVE 0 TO WK2-BRANCH WK2-CLASS WK2-ACCTNO
                     WK2-DATE WK2-LINE.
           PERFORM START-WK2-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

       SET-HEAD.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF ( GROUP-FLAG = "Y" )
              MOVE LOW-VALUES TO WS-GTT-TOTAL-TABLE
              MOVE ENT-GROUP TO GR-ID
              PERFORM SEARCH-GR-INIT
              GO TO EXIT-INITIALIZE.

           MOVE SPACES TO TOTAL-LEV-TABLE.
           MOVE "Y"    TO TOTAL-LEVEL-TABLE(9).
           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      *****************************************************************
       EO-EXEC-AUDIT SECTION.
      *****************************************************************
           MOVE EXT-EOBUF-DATE2 TO SYS-DATE.
           MOVE S-MM TO END-MONTH.
           MOVE S-YY TO END-YEAR.

           IF EXT-EOBUF-DATE1 NOT = EXT-EOBUF-DATE2
              MOVE 99 TO DAY-OF-MONTH
           ELSE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * IF USING 12/29/93 TO 12/29/93 FOR A DAILY BATCH, MUST SET
      * DAY-OF-MONTH TO S-DD
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
              MOVE S-DD TO DAY-OF-MONTH.

      *****************************************************************
       ENTER-PARAMETERS SECTION.
      *****************************************************************
       ENTER-PARM.
           IF VDUSTATUS = "1U"
              IF ( GTT-ISO = "O" )
                 GO TO GTT-ISO-PARAGRAPH
              ELSE
                 GO TO SUMYN.

      * DEFAULT TO USER BRANCH
      *    MOVE "SNNN040BR1." TO VDU.
      *    MOVE 0 TO BEG-BRANCH VDUNUM0.
      *    PERFORM SCREENIO.
      *    MOVE "SNNN040BR2." TO VDU.
      *    MOVE 0 TO END-BRANCH VDUNUM0.
      *--->PERFORM SCREENIO.

      *----------------------------------------------------------------
       BR-01.
           MOVE LEGEND-BR-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "R  A000BR1." TO VDU.
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

      *----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.

       BR-02.
           MOVE LEGEND-BR-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-02.

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

      *----------------------------------------------------------------
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

       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.
           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

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
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-01.

           MOVE VDUNUM0 TO BEG-CLASS.

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
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-02.
           MOVE VDUNUM0 TO END-CLASS.
           IF BEG-CLASS > END-CLASS
              MOVE "INVALID CLASS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO CL-01.
           GO TO DTE.

       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.
       DTE.
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO DA-01.

           MOVE LEGEND-DTE TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040ENDDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DTE.
           MOVE VDUNUM0 TO EDATE.
           IF END-MONTH < 1 OR > 12
              MOVE "INVALID MONTH" TO MESS
              PERFORM SEND-MESS
              GO TO DTE.

      *----------------------------------------------------------------
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

           IF VDUSTATUS = "1U"
              IF EXT-ROUTE-BUF = "00"
                 GO TO DTE
              ELSE
                 GO TO CL-02.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DTE.
           IF VDUNUM0 = 0 GO TO DA-01.
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
              GO TO LOCAL-PRINT.

           MOVE "E  A010SUMONLY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.

           IF ( VDUSTATUS = "1U" )
              IF  ( GROUP-FLAG = "Y" )
                  GO TO GTT-ISO-PARAGRAPH.

           IF VDUSTATUS = "1U"
              IF EXT-ROUTE-BUF = "00"
                 IF ( DAY-OF-MONTH = 99 )
                    MOVE "SNNN020DA1." TO VDU
                    MOVE 99 TO VDUNUM0
                    PERFORM SCREENIO
                    GO TO DTE.

           IF ( VDUSTATUS = "1U" )
              IF ( EXT-ROUTE-BUF NOT = "00" )
                 GO TO CL-02
              ELSE
                 GO TO DTE.

           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO SUMYN.
           MOVE VDUBUF TO SUM-ONLY.
           IF SUM-ONLY NOT = "N" AND NOT = "Y" GO TO SUMYN.
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
           MOVE "S  A170SPOOL-PROMPT." TO VDU.
           MOVE "SPOOL TO LOCAL  :"    TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "SPOOL TO LOCAL DIRECTORY? (Y/N)" TO LEGEND.
           PERFORM SEND-LEGEND.

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
                    GO TO SUMYN
              ELSE
                 GO TO SUMYN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ASK-LOCAL.

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

      *****************************************************************
       DISPLAY-PARAMETERS SECTION.
      *****************************************************************
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
           MOVE "SNNN020DA1." TO VDU.
           MOVE DAY-OF-MONTH TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A010GTT-ISO." TO VDU.
           MOVE GTT-ISO           TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010SUMONLY." TO VDU.
           MOVE SUM-ONLY          TO VDUBUF.
           PERFORM SCREENIO.
      *--------------------------------------------------------
           MOVE "S  A170SPOOL-PROMPT." TO VDU.
           MOVE "SPOOL TO LOCAL  :"    TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A050YES-NO." TO VDU.
           MOVE "(Y/N)"          TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010LOCAL-SPOOL." TO VDU.
           MOVE LOCAL-PRINT-FG        TO VDUBUF.
           PERFORM SCREENIO.

      *****************************************************************
       SORT-ROUTINE SECTION.
      *****************************************************************

           PERFORM ENABLE-INTERRUPT.

           MOVE BR-NO               TO TP-BRNO
                                       TRP-PATH-OWNBR
                                       QTRP1-WBEG-BRNO
                                       QTRP1-WEND-BRNO.
           MOVE BEGINNING-DATE      TO TP-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRP1-WBEG-DATE.
           MOVE ENDING-DATE         TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRP1-WEND-DATE.

           MOVE BEG-CLASS           TO TP-CLASS
                                       QTRP1-WBEG-CLASS.
           MOVE END-CLASS           TO QTRP1-WEND-CLASS.
           MOVE 0                   TO TP-ACCTNO TP-LINE
                                       QTRP1-WBEG-ACCTNO
                                       QTRP1-WBEG-LINE.
           MOVE ALL "9"             TO QTRP1-WEND-ACCTNO
                                       QTRP1-WEND-LINE.

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

       SORT-TR-NEXT.
           PERFORM READ-TRP1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRP-PATH-OWNBR NOT = TP-BRNO)
              GO TO EXIT-SORT-ROUTINE.

           PERFORM NPR-TRAP-INTERRUPT.

           IF TP-DATE > ENDING-DATE
              GO TO EXIT-SORT-ROUTINE.

           IF TP-CLASS < BEG-CLASS OR TP-CLASS > END-CLASS
              GO TO SORT-TR-NEXT.

      *  ONLY INCLUDE RECOVERIES POSTED TO THIS BRANCH
           IF TP-OTHBR-FG = "C"
              GO TO SORT-TR-NEXT.

           IF TP-TRCD = "PL" OR "P2" OR "P3" OR "P4" OR "P5"
              GO TO SORT-TR-NEXT.

           IF TP-PLCD  = SPACES
              GO TO SORT-TR-NEXT.

           IF TP-TRCD = "RT" OR "ET" OR "LP" OR "NP" OR "ST" OR
              (TP-TRCD = "RV" AND (TP-REFNO = "ADD" OR "ADJ" OR "CNL") )
              IF TP-PLCD = SPACES
                 GO TO SORT-TR-NEXT.

           MOVE TP-DATE    TO WK2-DATE.
           MOVE TP-BRNO    TO WK2-BRANCH.
      *    MOVE TP-CODE    TO WK2-CODE.
           MOVE TP-CLASS   TO WK2-CLASS.
           MOVE TP-ACCTNO  TO WK2-ACCTNO.
           MOVE TP-LINE    TO WK2-LINE.
           PERFORM WRITE-WK2-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO SORT-TR-NEXT.

       EXIT-SORT-ROUTINE.
           PERFORM DISABLE-INTERRUPT.
           PERFORM CLOSE-WK2-FILE.

      *****************************************************************
       DETAIL-LINE-PROCESS SECTION.
      *****************************************************************
           IF FIRST-TIME = "Y"
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE 1 TO LN-FEED
              MOVE "Y" TO RECORDS-FOUND
              MOVE "N" TO FIRST-TIME.

      *- - - - - - - - - - - - - - - - - - - -{ GET ACCTNO WITH STATUS }
           PERFORM GET-DISPLAY-LNNO.
           MOVE LPS-DISPLAY-LNNO TO D-DISPLAY-LNNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM FIND-BORROWER.
           MOVE TP-MAKER TO D-MAKER.
           MOVE 0        TO D-SSNO.
           MOVE TP-DLNO  TO D-DLNO.

           IF HOLD-TP-ACCTNO = TP-ACCTNO AND
               HOLD-TP-OTHBR-FG = TP-OTHBR-FG AND
                 HOLD-TP-SSNO = TP-SSNO
              MOVE SPACES TO DETAIL-LINE
            ELSE
              MOVE 1 TO LN-FEED
              MOVE TP-ACCTNO   TO HOLD-TP-ACCTNO
              MOVE TP-OTHBR-FG TO HOLD-TP-OTHBR-FG
              MOVE TP-SSNO     TO HOLD-TP-SSNO
              IF SUM-ONLY = "N"
               IF TP-OTHBR-FG = "G"
                 MOVE "POSTED BY :" TO D-BRXX
                 MOVE TP-POSTING-BR TO D-BRX
                 PERFORM WRITE-DETAIL-LINE
               ELSE
                 IF TP-OTHBR-FG = "C"
                    MOVE "POSTED FOR:" TO D-BRXX
                    MOVE TP-POSTING-BR TO D-BRX
                    PERFORM WRITE-DETAIL-LINE
                  ELSE
                    PERFORM WRITE-DETAIL-LINE.

           MOVE TP-PAYDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-DATE.
           MOVE TP-IBPC TO D-IBPC.
           MOVE 0 TO HOLD-EARNINGS.

           IF TP-SERVACCT = "N"
              COMPUTE HOLD-EARNINGS = TP-EARNINGS(1) + TP-EARNINGS(2) +
                                      TP-EARNINGS(3) + TP-EARNINGS(4) +
                                      TP-EARNINGS(5) + TP-EARNINGS(6)
            ELSE
               IF TP-TRCD NOT = "AP"
                  COMPUTE HOLD-EARNINGS = TP-APCUR + TP-APINT +
                                          TP-APLC - TP-DLPROC.

           MOVE HOLD-EARNINGS TO D-EARNINGS.
           MOVE TP-TILLNO TO D-TILL.

           MOVE TP-DLPROC TO D-DLPROC.
           MOVE TP-TRCD TO D-TRCD.

           IF TP-REV = "Y"
              MOVE "*" TO D-REV.

           MOVE TP-REFNO TO D-REFNO.

           IF TP-TRCD = "OT"
              MOVE TP-INTDUE TO TP-TRAMT
              MOVE 0 TO TP-INTDUE.

           IF ( TP-TRCD NOT = "JD" )
              MOVE TP-TRAMT TO D-TRAMT.

           ADD TP-APOTH TP-APINT TP-APLC GIVING D-APINT.
           MOVE TP-APCUR TO D-APCUR.

           MOVE TP-PDTH-DATE TO NUM-DATE.
           MOVE NUM-MO TO D-PDTHRU-MO.
           IF TP-UNITPER-CD-DAYS
              MOVE NUM-DA TO D-PDTHRU-DAYR
           ELSE
              MOVE NUM-YR TO D-PDTHRU-DAYR.

           MOVE TP-LCPDTH-DATE TO NUM-DATE.
           MOVE NUM-MO TO D-LCPD-MO.
           IF TP-UNITPER-CD-DAYS
              MOVE NUM-DA TO D-LCPD-DAYR
           ELSE
              MOVE NUM-YR TO D-LCPD-DAYR.

           MOVE TP-OTHBAL TO D-OTHBAL.
           ADD TP-INTBAL TP-LCBAL GIVING D-INTBAL.
           MOVE TP-CURBAL TO D-PRINBAL.

           IF SUM-ONLY = "N"
            IF TP-CURBAL = 0
              MOVE "**PAIDOFF**" TO D-PAIDOFF
              PERFORM WRITE-DETAIL-LINE.

           IF TP-GLAMT(1) NOT = 0
              MOVE TP-GLNO(1) TO D-GLNO
              INSPECT D-GLNO REPLACING ALL "/" BY "-".

           IF SUM-ONLY = "N"
              PERFORM WRITE-DETAIL-LINE.

           IF SUM-ONLY = "N"
            IF TP-GLAMT(1) NOT = 0
              MOVE TP-GLAMT(1) TO D-GLAMT
              PERFORM WRITE-DETAIL-LINE.

           IF SUM-ONLY = "N"
            IF TP-GLAMT(2) NOT = 0
              MOVE TP-GLNO(2) TO D-GLNO
              INSPECT D-GLNO REPLACING ALL "/" BY "-"
              PERFORM WRITE-DETAIL-LINE
              MOVE TP-GLAMT(2) TO D-GLAMT
              PERFORM WRITE-DETAIL-LINE.

           IF SUM-ONLY = "N"
            IF TP-GLAMT(3) NOT = 0
              MOVE TP-GLNO(3) TO D-GLNO
              INSPECT D-GLNO REPLACING ALL "/" BY "-"
              PERFORM WRITE-DETAIL-LINE
              MOVE TP-GLAMT(3) TO D-GLAMT
              PERFORM WRITE-DETAIL-LINE.

           IF SUM-ONLY = "Y"
              MOVE SPACES TO DETAIL-LINE.

      *****************************************************************
       CREATE-WK SECTION.
      *****************************************************************
           PERFORM SETUP-WK-KEY.
           PERFORM READ-WK-FILE.

           IF IO-FG = 0
              GO TO CREATE-WK-UPDATE.

           PERFORM SETUP-WK-KEY.
           MOVE 0 TO WK-NO.
           MOVE 0 TO WK-TRAMT WK-APOTH WK-APINT WK-APLC
                     WK-APCUR WK-EARNINGS WK-DLPROC.

      *----------------------------------------------------------------
       CREATE-WK-UPDATE.
           IF TP-REV NOT = "Y"
              ADD 1 TO WK-NO
           ELSE
              SUBTRACT 1 FROM WK-NO.

           IF ( TP-OTHBR-FG = " " OR "G" ) AND
              ( TP-TRCD NOT = "JD"       )
                 ADD TP-TRAMT TO WK-TRAMT.

           IF TP-OTHBR-FG = " " OR "G"
              ADD TP-APOTH      TO WK-APOTH
              ADD TP-APINT      TO WK-APINT
              ADD TP-APLC       TO WK-APLC
              ADD TP-APCUR     TO WK-APCUR
              ADD HOLD-EARNINGS TO WK-EARNINGS
              ADD TP-DLPROC     TO WK-DLPROC.

           PERFORM WRITE-WK-FILE.

           IF IO-FG NOT = 0
              PERFORM REWRITE-WK-FILE.

           MOVE 0 TO SUB.
           PERFORM OUTPUT-GL 3 TIMES.
           PERFORM CREATE-TOTAL-WK.

      *****************************************************************
       CREATE-TOTAL-WK SECTION.
      *****************************************************************
           PERFORM SETUP-SUM-KEY.
           PERFORM READ-WK-FILE.

           IF IO-FG = 0
              GO TO CREATE-SUM-UPDATE.

           PERFORM SETUP-SUM-KEY.
           MOVE 0 TO WK-NO.
           MOVE 0 TO WK-TRAMT WK-APOTH WK-APINT WK-APLC
                     WK-APCUR WK-EARNINGS WK-DLPROC.

      *----------------------------------------------------------------
       CREATE-SUM-UPDATE.
           IF TP-REV NOT = "Y"
              ADD 1 TO WK-NO
           ELSE
              SUBTRACT 1 FROM WK-NO.

           IF ( TP-OTHBR-FG = " " OR "G" ) AND
              ( TP-TRCD NOT = "JD"       )
                 ADD TP-TRAMT TO WK-TRAMT.

           IF TP-OTHBR-FG = " " OR "G"
              ADD TP-APOTH TO WK-APOTH
              ADD TP-APINT TO WK-APINT
              ADD TP-APLC TO WK-APLC
              ADD TP-APCUR TO WK-APCUR
              ADD HOLD-EARNINGS TO WK-EARNINGS
              ADD TP-DLPROC TO WK-DLPROC.

           PERFORM WRITE-WK-FILE.

           IF IO-FG NOT = 0
              PERFORM REWRITE-WK-FILE.

           PERFORM ADD-TO-LEV-PL
                   VARYING LEV FROM 1 BY 1 UNTIL LEV > 3.

      ******************************************************************
      *
      *    ADD TO LEVEL PL SECTION.
      *
      * NOTE: IF TP-PLTYPE NOT = 2,3,4,5 THEN DEFAULT TO P
      ******************************************************************
       ADD-TO-LEV-PL SECTION.

           MOVE 0 TO LEV-PL.

           IF (TP-PLTYPE = "2")
               MOVE 2 TO LEV-PL
           ELSE
           IF (TP-PLTYPE = "3")
               MOVE 3 TO LEV-PL
           ELSE
           IF (TP-PLTYPE = "4")
               MOVE 4 TO LEV-PL
           ELSE
           IF (TP-PLTYPE = "5")
               MOVE 5 TO LEV-PL
           ELSE
               MOVE 1 TO LEV-PL.

           IF TP-REV NOT = "Y"
              ADD 1 TO T-NO(LEV LEV-PL)
                       T-NO(LEV 6)
              IF NOT (TP-TRCD = "PI" OR "OT")
                 ADD 1 TO T-R-NO(LEV LEV-PL)
                          T-R-NO(LEV 6)
              END-IF
           ELSE
              SUBTRACT 1 FROM T-NO(LEV LEV-PL)
                              T-NO(LEV 6)
              IF NOT (TP-TRCD = "PI" OR "OT")
                 SUBTRACT 1 FROM T-R-NO(LEV LEV-PL)
                                 T-R-NO(LEV 6).

           IF ( TP-OTHBR-FG = " " OR "G" ) AND
              ( TP-TRCD NOT = "JD"       )
              ADD TP-TRAMT TO T-TRAMT(LEV,LEV-PL)
                              T-TRAMT(LEV,6)
              IF NOT (TP-TRCD = "PI" OR "OT")
                 ADD TP-TRAMT TO T-R-TRAMT(LEV,LEV-PL)
                                 T-R-TRAMT(LEV,6).

           IF ( TP-OTHBR-FG = " " OR "G" )
              ADD TP-APOTH      TO T-APOTH   (LEV,LEV-PL)
                                   T-APOTH   (LEV,6)
              ADD TP-APINT      TO T-APINT   (LEV,LEV-PL)
                                   T-APINT   (LEV,6)
              ADD TP-APLC       TO T-APLC    (LEV,LEV-PL)
                                   T-APLC    (LEV,6)
              ADD TP-APCUR      TO T-APCUR  (LEV,LEV-PL)
                                   T-APCUR  (LEV,6)
              ADD HOLD-EARNINGS TO T-EARNINGS(LEV,LEV-PL)
                                   T-EARNINGS(LEV,6)
              ADD TP-DLPROC     TO T-DLPROC  (LEV,LEV-PL)
                                   T-DLPROC  (LEV,6)
              IF NOT (TP-TRCD = "PI" OR "OT")
                 ADD TP-APOTH      TO T-R-APOTH   (LEV,LEV-PL)
                                      T-R-APOTH   (LEV,6)
                 ADD TP-APINT      TO T-R-APINT   (LEV,LEV-PL)
                                      T-R-APINT   (LEV,6)
                 ADD TP-APLC       TO T-R-APLC    (LEV,LEV-PL)
                                      T-R-APLC    (LEV,6)
                 ADD TP-APCUR      TO T-R-APCUR  (LEV,LEV-PL)
                                      T-R-APCUR  (LEV,6)
                 ADD HOLD-EARNINGS TO T-R-EARNINGS(LEV,LEV-PL)
                                      T-R-EARNINGS(LEV,6)
                 ADD TP-DLPROC     TO T-R-DLPROC  (LEV,LEV-PL)
                                      T-R-DLPROC  (LEV,6).
       EXIT-ADD-TO-LEV-PL.
           EXIT.

      *****************************************************************
       OUTPUT-GL SECTION.
      *****************************************************************
           ADD 1 TO SUB.
           IF TP-GLAMT(SUB) = 0
              GO TO END-OUTPUT-GL.

      * ONLY GIVE THE GL AMT TO THE BRANCH THAT IT WAS MADE FOR
           IF TP-OTHBR-FG NOT = " " AND NOT = "G"
              GO TO END-OUTPUT-GL.

           PERFORM SETUP-WK-GL1-KEY.
           PERFORM READ-WK-FILE.

           IF IO-FG = 0
              GO TO ACCUM-GL-WK.

           PERFORM SETUP-WK-GL1-KEY.
           MOVE TP-GLAMT(SUB) TO WK-GLAMT.
           PERFORM WRITE-WK-FILE.

           IF IO-FG = 9
              GO TO IO-ERROR.

           GO TO END-OUTPUT-GL.

      *----------------------------------------------------------------
       ACCUM-GL-WK.
           ADD TP-GLAMT(SUB) TO WK-GLAMT.
           PERFORM REWRITE-WK-FILE.

       END-OUTPUT-GL.
           EXIT.

      *****************************************************************
       FIND-BORROWER SECTION.
      *****************************************************************
           IF TP-OTHBR-FG = "C"
              MOVE "INTER-OFFICE PAYMENT" TO D-NAME
              GO TO END-FIND-BORROWER.

           MOVE TP-SSNO TO BW-SSNO.
           PERFORM READ-BW1-FILE.

           IF IO-FG = 9
               MOVE "** NO BORROWER MASTER **" TO D-NAME
               GO TO END-FIND-BORROWER.

           MOVE BW-LNAME TO NAME-WORKER.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?".
           MOVE BW-FNAME TO FIRST-NAME.
           INSPECT NAME-WORKER REPLACING FIRST "                "
               BY FIRST-NAME.
           INSPECT NAME-WORKER REPLACING ALL "?" BY " ".
           MOVE NAME-WORKER TO D-NAME.

       END-FIND-BORROWER.
           EXIT.

      *****************************************************************
       WRITE-DETAIL-LINE SECTION.
      *****************************************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.


      *****************************************************************
      *
      *     S E T   C L A S S   R E P O R T   H E A D I N G S
      *
      *****************************************************************
       SET-CLASS-HEADINGS SECTION.
           MOVE HOLD-CLASS TO H-CLASS LC-CODE.
           PERFORM READ-LC1-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID" TO LC-DESC.

           MOVE LC-DESC TO H-LCDESC.
           MOVE "N" TO SUMMARY-FG.

      *****************************************************************
       CLASS-TOTALS SECTION.
      *****************************************************************
           IF ( GTT-ISO = "O" )
              GO TO END-CLASS-TOTALS.

           MOVE "Y" TO SUMMARY-FG.
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
           PERFORM WRITE-DETAIL-LINE.
           MOVE CLASS-HEAD TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE SPACE TO WK-KEY.
           MOVE "L" TO WK-CODE.
           MOVE HOLD-BRNO TO WK-BRNO.
           MOVE HOLD-CLASS TO WK-CLASS.
           PERFORM START-WK-FILE.

      *----------------------------------------------------------------
       PRINT-NEXT-CODE.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG = 9 OR WK-CODE NOT = "L" OR
              WK-CLASS NOT = HOLD-CLASS OR
              WK-BRNO NOT = HOLD-BRNO
              PERFORM TYPE-TOTALS
              GO TO END-CLASS-TOTALS.

           MOVE WK-TRCD      TO DS-TRCD.
           MOVE WK-NO        TO DS-NO.
           MOVE WK-TRAMT     TO DS-TRAMT.
           MOVE WK-APOTH     TO DS-APOTH.
           MOVE WK-APINT     TO DS-APINT.
           MOVE WK-APLC      TO DS-APLC.
           MOVE WK-APCUR    TO DS-APCUR.
           MOVE WK-EARNINGS  TO DS-EARNINGS.
           MOVE WK-DLPROC    TO DS-DLPROC.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 1            TO LN-FEED.

           GO TO PRINT-NEXT-CODE.

      *----------------------------------------------------------------
       END-CLASS-TOTALS.

           MOVE 1 TO SUB LEV.
           IF ( GTT-ISO NOT = "O" )
               PERFORM PRINT-RECOVERIES.

           MOVE 1 TO SUB.
           PERFORM ZERO-TOTAL-TBL
                   VARYING LEV-PL FROM 1 BY 1 UNTIL LEV-PL > 6.

           MOVE "N" TO SUMMARY-FG.

           IF ( GTT-ISO NOT = "O" )
              MOVE ALL "-" TO DETAIL-LINE
              PERFORM WRITE-DETAIL-LINE.

      *****************************************************************
       BRANCH-TOTALS SECTION.
      *****************************************************************
           IF ( GTT-ISO = "O" )
              GO TO END-BRANCH-TOTALS.

           MOVE "Y" TO SUMMARY-FG.
           MOVE 0 TO HOLD-CLASS.
           MOVE 2 TO LEV.
           MOVE "BRANCH TOTALS:" TO DETAIL-LINE.
           PERFORM PRINT-SUMMARY.

      *----------------------------------------------------------------
       END-BRANCH-TOTALS.

           MOVE 2 TO SUB LEV.
           IF ( GTT-ISO NOT = "O" )
               PERFORM PRINT-RECOVERIES
               MOVE ALL "-" TO DETAIL-LINE
               PERFORM WRITE-DETAIL-LINE.

           MOVE 3 TO LEV.
           MOVE 2 TO SUB.
           PERFORM ZERO-TOTAL-TBL
              VARYING LEV-PL FROM 1 BY 1
                 UNTIL LEV-PL > 6.

           MOVE "N" TO SUMMARY-FG.

      *****************************************************************
       GRAND-TOTALS SECTION.
      *****************************************************************
           IF ( GTT-ISO = "O" )
              GO TO EXIT-PRINT-GRAND-TOTALS.

           MOVE "Y" TO SUMMARY-FG.
           MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.
           PERFORM WRITE-DETAIL-LINE.
           MOVE GRAND-HEAD TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE SPACE TO WK-KEY.
           MOVE "S" TO WK-CODE.
           PERFORM START-WK-FILE.

      *----------------------------------------------------------------
       PRINT-NEXT-SUM.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG NOT = 0 OR WK-CODE NOT = "S"
              GO TO PRINT-GRAND-CD.

           MOVE WK-CLASS TO DS-CLASS.
           MOVE WK-TRCD TO DS-TRCD.
           MOVE WK-NO TO DS-NO.
           MOVE WK-TRAMT TO DS-TRAMT.
           MOVE WK-APOTH TO DS-APOTH.
           MOVE WK-APINT TO DS-APINT.
           MOVE WK-APLC TO DS-APLC.
           MOVE WK-APCUR TO DS-APCUR.
           MOVE WK-EARNINGS TO DS-EARNINGS.
           MOVE WK-DLPROC TO DS-DLPROC.
           PERFORM WRITE-DETAIL-LINE.
           GO TO PRINT-NEXT-SUM.

      *-----------------------------------------------------------
       PRINT-GRAND-CD.
           MOVE "Y" TO SUMMARY-FG.
           MOVE 3 TO LEV.
           MOVE "GRAND TOTALS :" TO DETAIL-LINE.
           PERFORM PRINT-SUMMARY.

           PERFORM PRINT-RECOVERIES.

           MOVE ALL "-" TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

       EXIT-PRINT-GRAND-TOTALS.
           EXIT.

      *****************************************************************
       GL-SUMMARY SECTION.
      *****************************************************************
           MOVE GL-HEAD TO DETAIL-LINE.
           MOVE 4 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE SPACE TO WK-KEY.
           MOVE 0 TO WK-BRNO
                     WK-CLASS
                     WK-GLNO
                     WK-GLAMT.
           MOVE "G" TO WK-CODE.

           IF GRAND-SUMMARY NOT = "Y"
              MOVE HOLD-BRNO TO WK-BRNO
              MOVE HOLD-CLASS TO WK-CLASS.

           PERFORM START-WK-FILE.

      *----------------------------------------------------------------
       PRINT-NEXT-GL.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG NOT = 0 OR WK-CODE NOT = "G"
              GO TO END-GL-SUMMARY.

           IF GRAND-SUMMARY NOT = "Y"
              IF WK-BRNO NOT = HOLD-BRNO
                 OR WK-CLASS NOT = HOLD-CLASS
                 GO TO PRINT-NEXT-GL.

           IF WK-GLAMT = 0
              GO TO PRINT-NEXT-GL.

           MOVE WK-GLNO TO DS-GLNO.
           INSPECT DS-GLNO REPLACING ALL "/" BY "-".
           MOVE WK-GLAMT TO DS-GLAMT.
           PERFORM WRITE-DETAIL-LINE.
           GO TO PRINT-NEXT-GL.

      *----------------------------------------------------------------
       END-GL-SUMMARY.

           IF LEV = 2
              MOVE ALL "-" TO DETAIL-LINE
              PERFORM WRITE-DETAIL-LINE.

      *****************************************************************
       HEAD-PAGE SECTION.
      *****************************************************************
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

           IF GRAND-SUMMARY = "N"
              MOVE HEAD3 TO PR-REC
              PERFORM WRITE-PR-REC
              MOVE HEAD4 TO PR-REC
              PERFORM WRITE-PR-REC
           ELSE
              MOVE 00 TO H-BRNO
              MOVE "ALL BRANCHES" TO H-BRDESC
              MOVE HEAD3 TO PR-REC
              PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
              MOVE ALL "-" TO PR-REC
              PERFORM WRITE-PR-REC
              MOVE 10 TO LN-COUNT
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
              MOVE 6 TO LN-COUNT
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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEG CLASS" TO RANGE-LINE.
           MOVE BEG-CLASS TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "END CLASS" TO RANGE-LINE.
           MOVE END-CLASS TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "MONTH-END DATE" TO RANGE-LINE.
           MOVE EDATE TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "DAY OF MONTH" TO RANGE-LINE.
           MOVE DAY-OF-MONTH TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "GROUP TOTALS" TO RANGE-LINE.
           MOVE GTT-ISO        TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.

      ************************************************************
       CLASS-SCAN SECTION.
      ************************************************************
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

      ******************************************************************
       GTT-ADD-TOTALS SECTION.
      ******************************************************************

           ADD WK-NO          TO  WS-GTT-NO        ( GTT-TOTAL-SUB ).
           ADD WK-TRAMT       TO  WS-GTT-TRAMT     ( GTT-TOTAL-SUB ).
           ADD WK-APOTH       TO  WS-GTT-APOTH     ( GTT-TOTAL-SUB ).
           ADD WK-APINT       TO  WS-GTT-APINT     ( GTT-TOTAL-SUB ).
           ADD WK-APLC        TO  WS-GTT-APLC      ( GTT-TOTAL-SUB ).
           ADD WK-APCUR      TO  WS-GTT-APCUR    ( GTT-TOTAL-SUB ).
           ADD WK-EARNINGS    TO  WS-GTT-EARNINGS  ( GTT-TOTAL-SUB ).
           ADD WK-DLPROC      TO  WS-GTT-DLPROC    ( GTT-TOTAL-SUB ).

       EXIT-GTT-ADD-TOTALS.
           EXIT.

      ******************************************************************
       GTT-COMPILE-GROUP-TOTALS SECTION.
      ******************************************************************
           MOVE SPACES      TO WK-KEY.
           MOVE "L"         TO WK-CODE.
           MOVE WGR-SRESULT TO WK-BRNO.
           MOVE ZEROES      TO WK-CLASS.
           PERFORM START-WK-FILE.

      *-----------------------------------------------------------------
       GTT-NEXT-WK1.

           PERFORM READ-WK-FILE-NEXT.
           IF ( IO-FG   NOT = 0           ) OR
              ( WK-CODE NOT = "L"         ) OR
              ( WK-BRNO NOT = WGR-SRESULT )
                 GO TO EXIT-GTT-COMPILE-GROUP-TOTALS.

           MOVE "N" TO GTT-FIRST-TIME.
           PERFORM TRAP-INTERRUPT.

      *-----------------------------------------------------------------
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
           MOVE WS-GTT-NO        (GTT-TOTAL-SUB) TO DS-NO.
           MOVE WS-GTT-TRAMT     (GTT-TOTAL-SUB) TO DS-TRAMT.
           MOVE WS-GTT-APOTH     (GTT-TOTAL-SUB) TO DS-APOTH.
           MOVE WS-GTT-APINT     (GTT-TOTAL-SUB) TO DS-APINT.
           MOVE WS-GTT-APLC      (GTT-TOTAL-SUB) TO DS-APLC.
           MOVE WS-GTT-APCUR    (GTT-TOTAL-SUB) TO DS-APCUR.
           MOVE WS-GTT-EARNINGS  (GTT-TOTAL-SUB) TO DS-EARNINGS.
           MOVE WS-GTT-DLPROC    (GTT-TOTAL-SUB) TO DS-DLPROC.

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
       GET-DISPLAY-LNNO SECTION.
      ******************************************************************

      * LPS-DISPLAY-LNNO IS SET FIRST SO THAT IF THERE IS A ERROR
      * IT WILL BE ALREADY DISPLAYING FOR A ERROR, OTHERWISE IT WILL
      * RESET LPS-DISPLAY-LNNO IN 'PERFORM LPSTAT-DISPLAY.'

           MOVE TP-CLASS  TO LPS-CLASS.
           MOVE "-"       TO LPS-DASH.
           MOVE TP-ACCTNO TO LPS-NUMBER.
           MOVE "{N/A}"   TO LPS-7STATUS.

           MOVE TP-BRNO  TO LN-OWNBR.
           MOVE TP-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF ( IO-BAD )
              GO TO GET-DISPLAY-LNNO-EXIT.

           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           PERFORM READ-SP1-FILE.
           IF ( IO-BAD )
              GO TO GET-DISPLAY-LNNO-EXIT.

           PERFORM LPSTAT-DISPLAY.

       GET-DISPLAY-LNNO-EXIT.
           EXIT.

      *****************************************************************
      *
      *     C O P Y - L I B R A R Y - S E C T I O N S
      *
      *****************************************************************
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRTOTALS.CPY".
       COPY "LIBWI/GRTOTL.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/EOSPOOL.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPSTAT.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO EMWORG-CONAME.
           MOVE EXT-CONAME-SYSDATE TO EMWORG-SYSDATE.
           DISPLAY EMWORG-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE EMWORG-FORM-FIELDS TO WR-BUF.
           MOVE EMWORG-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE EMWORG-HELP--FILE TO HELP-LINK-FILE.
           MOVE EMWORG-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/EMWORG_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *****************************************************************
       MISC-PARA SECTION.
      *****************************************************************
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
       SETUP-SUM-KEY.
           MOVE SPACE TO WK-REC.
           MOVE "S" TO WK-CODE.
           MOVE 0 TO WK-BRNO.
           MOVE TP-CLASS TO WK-CLASS.
           MOVE TP-TRCD TO WK-TRCD.

      *-----------------------------------------------------------------
       SETUP-WK-KEY.
           MOVE SPACE TO WK-REC.
           MOVE "L" TO WK-CODE.
           MOVE TP-BRNO TO WK-BRNO.
           MOVE TP-CLASS TO WK-CLASS.
           MOVE TP-TRCD TO WK-TRCD.

      *-----------------------------------------------------------------
       SETUP-WK-GL1-KEY.
           MOVE SPACE TO WK-REC.
           MOVE "G" TO WK-CODE.
           MOVE TP-BRNO TO WK-BRNO.
           MOVE TP-CLASS TO WK-CLASS.
           MOVE TP-GLNO(SUB) TO WK-GLNO.

      *-----------------------------------------------------------------
       ZERO-TOTAL-TBL.
           MOVE ZEROS TO T-NO(SUB,LEV-PL).
           MOVE ZEROS TO T-TRAMT(SUB,LEV-PL).
           MOVE ZEROS TO T-APOTH(SUB,LEV-PL).
           MOVE ZEROS TO T-APINT(SUB,LEV-PL).
           MOVE ZEROS TO T-APLC(SUB,LEV-PL).
           MOVE ZEROS TO T-APCUR(SUB,LEV-PL).
           MOVE ZEROS TO T-EARNINGS(SUB,LEV-PL).
           MOVE ZEROS TO T-DLPROC(SUB,LEV-PL).

           MOVE ZEROS TO T-R-NO(SUB,LEV-PL).
           MOVE ZEROS TO T-R-TRAMT(SUB,LEV-PL).
           MOVE ZEROS TO T-R-APOTH(SUB,LEV-PL).
           MOVE ZEROS TO T-R-APINT(SUB,LEV-PL).
           MOVE ZEROS TO T-R-APLC(SUB,LEV-PL).
           MOVE ZEROS TO T-R-APCUR(SUB,LEV-PL).
           MOVE ZEROS TO T-R-EARNINGS(SUB,LEV-PL).
           MOVE ZEROS TO T-R-DLPROC(SUB,LEV-PL).

      *-----------------------------------------------------------------
       UPDATE-TABLE2.
           ADD T-NO(SUB,LEV-PL)       TO T-NO(LEV-NEXT-1,LEV-PL).
           ADD T-TRAMT(SUB,LEV-PL)    TO T-TRAMT(LEV-NEXT-1,LEV-PL).
           ADD T-APOTH(SUB,LEV-PL)    TO T-APOTH(LEV-NEXT-1,LEV-PL).
           ADD T-APINT(SUB,LEV-PL)    TO T-APINT(LEV-NEXT-1,LEV-PL).
           ADD T-APLC(SUB,LEV-PL)     TO T-APLC(LEV-NEXT-1,LEV-PL).
           ADD T-APCUR(SUB,LEV-PL)   TO T-APCUR(LEV-NEXT-1,LEV-PL).
           ADD T-EARNINGS(SUB,LEV-PL) TO T-EARNINGS(LEV-NEXT-1,LEV-PL).
           ADD T-DLPROC(SUB,LEV-PL)   TO T-DLPROC(LEV-NEXT-1,LEV-PL).

           ADD T-R-NO(SUB,LEV-PL)       TO T-R-NO(LEV-NEXT-1,LEV-PL).
           ADD T-R-TRAMT(SUB,LEV-PL)    TO T-R-TRAMT(LEV-NEXT-1,LEV-PL).
           ADD T-R-APOTH(SUB,LEV-PL)    TO T-R-APOTH(LEV-NEXT-1,LEV-PL).
           ADD T-R-APINT(SUB,LEV-PL)    TO T-R-APINT(LEV-NEXT-1,LEV-PL).
           ADD T-R-APLC(SUB,LEV-PL)     TO T-R-APLC(LEV-NEXT-1,LEV-PL).
           ADD T-R-APCUR(SUB,LEV-PL)    TO T-R-APCUR(LEV-NEXT-1,LEV-PL).
           ADD T-R-EARNINGS(SUB,LEV-PL)
                                     TO T-R-EARNINGS(LEV-NEXT-1,LEV-PL).
           ADD T-R-DLPROC(SUB,LEV-PL)  TO T-R-DLPROC(LEV-NEXT-1,LEV-PL).

      *-----------------------------------------------------------------
       TYPE-TOTALS.
           MOVE 1 TO LEV.
           MOVE "CLASS  TOTALS:" TO DETAIL-LINE.
           PERFORM PRINT-SUMMARY.

      *-----------------------------------------------------------------
       PRINT-RECOVERIES.
           MOVE "RECOVERIES:" TO DETAIL-LINE.
           PERFORM  WRITE-DETAIL-LINE.
           PERFORM PRINT-LEVEL-PL
                  VARYING LEV-PL FROM 1 BY 1 UNTIL LEV-PL > 6.
      *-----------------------------------------------------------------
       PRINT-LEVEL-PL.
           IF PL-TYPE(LEV-PL) = "P" MOVE "PL" TO DS-TRCD
           ELSE
           IF PL-TYPE(LEV-PL) = "2" MOVE "P2" TO DS-TRCD
           ELSE
           IF PL-TYPE(LEV-PL) = "3" MOVE "P3" TO DS-TRCD
           ELSE
           IF PL-TYPE(LEV-PL) = "4" MOVE "P4" TO DS-TRCD
           ELSE
           IF PL-TYPE(LEV-PL) = "5" MOVE "P5" TO DS-TRCD
           ELSE
           IF PL-TYPE(LEV-PL) = "6" MOVE "TOTAL" TO DETAIL-LINE.

           MOVE T-R-NO(LEV,LEV-PL)       TO DS-NO.
           MOVE T-R-TRAMT(LEV,LEV-PL)    TO DS-TRAMT.
           MOVE T-R-APOTH(LEV,LEV-PL)    TO DS-APOTH.
           MOVE T-R-APINT(LEV,LEV-PL)    TO DS-APINT.
           MOVE T-R-APLC(LEV,LEV-PL)     TO DS-APLC.
           MOVE T-R-APCUR(LEV,LEV-PL)    TO DS-APCUR.
           MOVE T-R-EARNINGS(LEV,LEV-PL) TO DS-EARNINGS.
           MOVE T-R-DLPROC(LEV,LEV-PL)   TO DS-DLPROC.
           PERFORM WRITE-DETAIL-LINE.

      *-----------------------------------------------------------------
       PRINT-SUMMARY.
           MOVE T-NO(LEV,6) TO DS-NO.
           MOVE T-TRAMT(LEV,6) TO DS-TRAMT.
           MOVE T-APOTH(LEV,6) TO DS-APOTH.
           MOVE T-APINT(LEV,6) TO DS-APINT.
           MOVE T-APLC(LEV,6) TO DS-APLC.
           MOVE T-APCUR(LEV,6) TO DS-APCUR.
           MOVE T-EARNINGS(LEV,6) TO DS-EARNINGS.
           MOVE T-DLPROC(LEV,6) TO DS-DLPROC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE ALL " " TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

      *****************************************************************
       IO-ROUTINES SECTION.
      *****************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRPGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
      * COPY "LIBLP/LPBW1RN.CPY".
      * COPYMEMBER: LIBLP/LPBW1RN
       LOAD-BW1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BW-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BW-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BW-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BW-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO BW-ALL-DATA
              MOVE BW-ALL-PATH             TO BW-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BW-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BW-ALL-BASE
              MOVE FILPATH-MACHINE         TO BW-ALL-MACHINE
              MOVE FILPATH-DATA            TO BW-ALL-DATA
              MOVE BW-ALL-PATH             TO BW-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-BWFILE NOT = "S" )
              MOVE BW-PATH-BEG             TO BW-SELECT-PATH-BEG
              MOVE BW-PATH-FILE            TO BW-SELECT-PATH-FILE
           ELSE
              MOVE BW-PATH-SQL             TO BW-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-BW1-FILE.
           PERFORM LOAD-BW1-FILE.
           PERFORM OPEN-IT.
           MOVE BW-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-BW1-FILE.
           PERFORM START-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR.
           MOVE BW-SELECT-PATH TO E-FILE.
           MOVE BW1-KEY        TO E-KEYX.

           IF ( BW1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BW1-FILE.

           IF ( QBW1-WEND-SSNO = ZEROES    ) OR
              ( QBW1-WEND-SSNO NOT NUMERIC )
              MOVE BW-OWNBR             TO QBW1-WBEG-OWNBR
                                           QBW1-WEND-OWNBR
              MOVE BW-SSNO              TO QBW1-WBEG-SSNO
              MOVE ALL "9"              TO QBW1-WEND-SSNO.

           EXEC SQL
            DECLARE BW1_CURSOR CURSOR FOR
            SELECT BWFILE.BW_OWNBR,
                   BWFILE.BW_SSNO,
                   ISNULL(BWFILE.BW_LNAME,''),
                   ISNULL(BWFILE.BW_FNAME,''),
                   ISNULL(BWFILE.BW_MNAME,''),
                   ISNULL(BWFILE.BW_NICKNAME,''),
                   ISNULL(BWFILE.BW_GENERATION,''),
                   ISNULL(BWFILE.BW_ADR1,''),
                   ISNULL(BWFILE.BW_ADR2,''),
                   ISNULL(BWFILE.BW_CITY,''),
                   ISNULL(BWFILE.BW_STATE,''),
                   ISNULL(BWFILE.BW_ZIP,''),
                   ISNULL(CAST(BWFILE.BW_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_REFDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_BDATE AS VARCHAR(10)),'0'),
                   BWFILE.BW_PHONE,
                   BWFILE.BW_CELL_PHONE,
                   ISNULL(BWFILE.BW_NATIONALITY,''),
                   ISNULL(BWFILE.BW_GENDER,''),
                   BWFILE.BW_DEPEND,
                   ISNULL(BWFILE.BW_RENTCD,''),
                   ISNULL(BWFILE.BW_TIMEADDR,''),
                   ISNULL(BWFILE.BW_CREDITCD,''),
                   ISNULL(BWFILE.BW_STRTGCD,''),
                   ISNULL(BWFILE.BW_DWELLCD,''),
                   ISNULL(BWFILE.BW_COUNTY,''),
                   ISNULL(BWFILE.BW_ORIGIN,''),
                   ISNULL(BWFILE.BW_CENSUSCD,''),
                   BWFILE.BW_ORGSLSMN,
                   ISNULL(BWFILE.BW_REPCODE,''),
                   BWFILE.BW_SPSSNO,
                   ISNULL(BWFILE.BW_SPLNAME,''),
                   ISNULL(BWFILE.BW_SPFNAME,''),
                   ISNULL(CAST(BWFILE.BW_SP_BDATE AS VARCHAR(10)),'0'),
                   ISNULL(BWFILE.BW_EMPLOYER,''),
                   ISNULL(BWFILE.BW_EMPLPOS,''),
                   BWFILE.BW_EMPPHONE,
                   BWFILE.BW_EMPEXT,
                   BWFILE.BW_EMPYEARS,
                   BWFILE.BW_EMPPAY,
                   BWFILE.BW_EMPPAYOT,
                   ISNULL(BWFILE.BW_EMPPAYSR,''),
                   ISNULL(BWFILE.BW_EMPADDR1,''),
                   ISNULL(BWFILE.BW_EMPADDR2,''),
                   ISNULL(BWFILE.BW_EMPCITY,''),
                   ISNULL(BWFILE.BW_EMPSTATE,''),
                   ISNULL(BWFILE.BW_EMPZIP,''),
                   ISNULL(BWFILE.BW_WORKHRS,''),
                   BWFILE.BW_LOANS,
                   BWFILE.BW_INTYTD,
                   BWFILE.BW_INTLYR,
                   BWFILE.BW_LIASMLN,
                   BWFILE.BW_LIAOTH,
                   ISNULL(CAST(BWFILE.BW_LASTPODATE
                                                  AS VARCHAR(10)),'0'),
                   BWFILE.BW_PRLOANS,
                   BWFILE.BW_RE_INTYTD,
                   BWFILE.BW_RE_INTLYR,
                   BWFILE.BW_CREDIT_LIMIT,
                   BWFILE.BW_CREDIT_SCORE,
                   BWFILE.BW_CRED_SCORE_1,
                   BWFILE.BW_CRED_SCORE_2,
                   BWFILE.BW_CRED_SCORE_3,
                   BWFILE.BW_CRED_SCORE_4,
                   BWFILE.BW_BNKRPT_SCORE_1,
                   BWFILE.BW_BNKRPT_SCORE_2,
                   BWFILE.BW_BNKRPT_SCORE_3,
                   BWFILE.BW_BNKRPT_SCORE_4,
                   BWFILE.BW_LAST_FORM_NO,
                   ISNULL(CAST(BWFILE.BW_LAST_FORM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_PRIOR_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_LAST_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BWFILE.BW_ADR_PHONE_CHG_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(BWFILE.BW_BAD_ADR_FG,''),
                   ISNULL(BWFILE.BW_BAD_PHONE_FG,''),
                   ISNULL(BWFILE.BW_BAD_EMPPHONE_FG,''),
                   ISNULL(BWFILE.BW_BAD_MISC_PHONE1_FG,''),
                   ISNULL(BWFILE.BW_BAD_MISC_PHONE2_FG,''),
                   ISNULL(BWFILE.BW_BAD_CELL_FG,''),
                   BWFILE.BW_MISC_PHONE_1,
                   BWFILE.BW_MISC_EXT_1,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_1,''),
                   BWFILE.BW_MISC_PHONE_2,
                   BWFILE.BW_MISC_EXT_2,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_2,''),
                   BWFILE.BW_MISC_PHONE_3,
                   BWFILE.BW_MISC_EXT_3,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_3,''),
                   BWFILE.BW_MISC_PHONE_4,
                   BWFILE.BW_MISC_EXT_4,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_4,''),
                   BWFILE.BW_MISC_PHONE_5,
                   BWFILE.BW_MISC_EXT_5,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_5,''),
                   BWFILE.BW_MISC_PHONE_6,
                   BWFILE.BW_MISC_EXT_6,
                   ISNULL(BWFILE.BW_MISC_PHONE_CD_6,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_1,''),
                   BWFILE.BW_CONTACT_PHONE_1,
                   BWFILE.BW_CONTACT_EXT_1,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_1,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_2,''),
                   BWFILE.BW_CONTACT_PHONE_2,
                   BWFILE.BW_CONTACT_EXT_2,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_2,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_3,''),
                   BWFILE.BW_CONTACT_PHONE_3,
                   BWFILE.BW_CONTACT_EXT_3,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_3,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_4,''),
                   BWFILE.BW_CONTACT_PHONE_4,
                   BWFILE.BW_CONTACT_EXT_4,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_4,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_5,''),
                   BWFILE.BW_CONTACT_PHONE_5,
                   BWFILE.BW_CONTACT_EXT_5,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_5,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_6,''),
                   BWFILE.BW_CONTACT_PHONE_6,
                   BWFILE.BW_CONTACT_EXT_6,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_6,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_7,''),
                   BWFILE.BW_CONTACT_PHONE_7,
                   BWFILE.BW_CONTACT_EXT_7,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_7,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_8,''),
                   BWFILE.BW_CONTACT_PHONE_8,
                   BWFILE.BW_CONTACT_EXT_8,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_8,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_9,''),
                   BWFILE.BW_CONTACT_PHONE_9,
                   BWFILE.BW_CONTACT_EXT_9,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_9,''),
                   ISNULL(BWFILE.BW_CONTACT_DESC_10,''),
                   BWFILE.BW_CONTACT_PHONE_10,
                   BWFILE.BW_CONTACT_EXT_10,
                   ISNULL(BWFILE.BW_CONTACT_RELATION_10,''),
                   ISNULL(BWFILE.BW_SRCD,''),
                   ISNULL(BWFILE.BW_CALL_ON_SUNDAY,''),
                   ISNULL(BWFILE.BW_SOLICIT,''),
                   ISNULL(BWFILE.BW_CONFIDENTIAL,''),
                   ISNULL(BWFILE.BW_PRIVACY_OPT_OUT,''),
                   ISNULL(BWFILE.BW_OPT_IN_EMAIL,''),
                   ISNULL(BWFILE.BW_OPT_IN_TEXT,''),
                   ISNULL(BWFILE.BW_CITY_TAX_CD,''),
                   ISNULL(CAST(BWFILE.BW_DEALTRACK_CHG_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(BWFILE.BW_DECEASED,''),
                   ISNULL(CAST(BWFILE.BW_DECEASED_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(BWFILE.BW_EMAIL,''),
                   ISNULL(BWFILE.BW_IDENT_NO,''),
                   ISNULL(BWFILE.BW_IDENT_TYPE,''),
                   ISNULL(BWFILE.BW_IDENT_STATE,''),
                   ISNULL(BWFILE.BW_MILITARY_FLAG,''),
                   BWFILE.BW_ORGBR
            FROM DBO.BWFILE
            WHERE BWFILE.BW_OWNBR >= :QBW1-WBEG-OWNBR
              AND BWFILE.BW_OWNBR <= :QBW1-WEND-OWNBR
              AND BWFILE.BW_SSNO >= :QBW1-WBEG-SSNO
              AND BWFILE.BW_SSNO <= :QBW1-WEND-SSNO
              ORDER BY BWFILE.BW_OWNBR,
                       BWFILE.BW_SSNO
           END-EXEC.

           EXEC SQL
               OPEN BW1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QBW1-WBEG-OWNBR 
                          QBW1-WEND-OWNBR
                          QBW1-WBEG-SSNO
                          QBW1-WEND-SSNO.

           MOVE STAT---GOOD TO BW1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-BW1-FILE.
           PERFORM READ-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR. 
           MOVE BW-SELECT-PATH TO E-FILE.
           MOVE BW1-KEY        TO E-KEYX.

           MOVE BW-OWNBR   TO QBW-OWNBR.
           MOVE BW-SSNO    TO QBW-SSNO.

           EXEC SQL
            SELECT BWFILE.BW_OWNBR,
                   BWFILE.BW_SSNO,
                   ISNULL(BWFILE.BW_LNAME,''),
                   ISNULL(BWFILE.BW_FNAME,''),
                   ISNULL(BWFILE.BW_SOLICIT,'')
              INTO 
                  :QBW-OWNBR,
                  :QBW-SSNO,
                  :QBW-LNAME,
                  :QBW-FNAME,
                  :QBW-SOLICIT
              FROM DBO.BWFILE
             WHERE BWFILE.BW_OWNBR = :QBW-OWNBR
               AND BWFILE.BW_SSNO = :QBW-SSNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BW1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-BW-FIELDS.

      *-----------------------------------------------------------------
       READ-BW1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR.
           MOVE BW-SELECT-PATH TO E-FILE.
           MOVE BW1-KEY        TO E-KEYX.
           INITIALIZE QBW-REC.

           IF ( BW1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BW1_CURSOR INTO :QBW-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BW1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BW-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-BW1-FILE.

           PERFORM CLOSE-IT.
           IF ( BW1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE BW1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO BW1-CURSOR-STAT.
	  
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
      * COPY "LIBLP/LPTRP1RN.CPY".
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

           EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS TRP1_CONNECT
                    USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
           END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-TRP1-FILE.

      D    STOP MESS.
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


           EXEC SQL SET CONNECTION TRP1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE TRP1_CURSOR CURSOR FOR
                :RET-CODE = EXEC SP_RZWORG_TRPFILE_SELECT
              ( :QTRP1-WBEG-BRNO,   :QTRP1-WEND-BRNO,   
                :QTRP1-WBEG-DATE,   :QTRP1-WSBEG-DATE,  
                :QTRP1-WEND-DATE,   
                :QTRP1-WBEG-CLASS,  :QTRP1-WSBEG-CLASS, 
                :QTRP1-WEND-CLASS,  
                :QTRP1-WBEG-ACCTNO, :QTRP1-WSBEG-ACCTNO,   
                :QTRP1-WEND-ACCTNO, 
                :QTRP1-WBEG-LINE,   :QTRP1-WEND-LINE )  
           END-EXEC.

           EXEC SQL
               OPEN TRP1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRP1-WBEG-BRNO
                          QTRP1-WEND-BRNO
                          QTRP1-WBEG-CLASS
                          QTRP1-WEND-CLASS
                          QTRP1-WBEG-ACCTNO
                          QTRP1-WEND-ACCTNO
                          QTRP1-WBEG-LINE
                          QTRP1-WEND-LINE
                          QTRP1-WSBEG-CLASS
                          QTRP1-WSBEG-ACCTNO
                          QTRP1-WSBEG-LINE.
           MOVE SPACES TO QTRP1-WBEG-DATE
                          QTRP1-WEND-DATE
                          QTRP1-WSBEG-DATE.

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
                    ISNULL(TRPFILE.TP_PLCD,''),
                    TRPFILE.TP_SSNO,
                    ISNULL(TRPFILE.TP_MAKER,''),
                    ISNULL(TRPFILE.TP_IBPC,''),
                    ISNULL(TRPFILE.TP_TRCD,''),
                    ISNULL(TRPFILE.TP_REFNO,''),
                    ISNULL(CAST(TRPFILE.TP_PAYDATE AS VARCHAR(10)),'0'),
                    TRPFILE.TP_TRAMT,
                    TRPFILE.TP_INTDUE,
                    TRPFILE.TP_APOTH,
                    TRPFILE.TP_APINT,
                    TRPFILE.TP_APLC,
                    TRPFILE.TP_APCUR,
                    TRPFILE.TP_GLNO_1,
                    TRPFILE.TP_GLNO_2,
                    TRPFILE.TP_GLNO_3,
                    TRPFILE.TP_GLAMT_1,
                    TRPFILE.TP_GLAMT_2,
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
                    TRPFILE.TP_OTHBAL,
                    TRPFILE.TP_INTBAL,
                    TRPFILE.TP_LCBAL,
                    TRPFILE.TP_CURBAL,
                    ISNULL(CAST(TRPFILE.TP_PDTH_DATE
                                                   AS VARCHAR(10)),'0'),
                    TRPFILE.TP_DLNO,
                    TRPFILE.TP_DLPROC,
                    ISNULL(TRPFILE.TP_SERVACCT,''),
                    TRPFILE.TP_TILLNO,
                    ISNULL(CAST(TRPFILE.TP_LCPDTH_DATE
                                                   AS VARCHAR(10)),'0'),
                    TRPFILE.TP_POSTING_BR,
                    ISNULL(TRPFILE.TP_OTHBR_FG,''),
                    ISNULL(TRPFILE.TP_REV,''),
                    ISNULL(TRPFILE.TP_PLTYPE,''),
                    ISNULL(TRPFILE.TP_UNITPER_CD,'')
              INTO  
                   :QTP-BRNO,
                   :QTP-DATE,
                   :QTP-CLASS,
                   :QTP-ACCTNO,
                   :QTP-LINE,
                   :QTP-PLCD,
                   :QTP-SSNO,
                   :QTP-MAKER,
                   :QTP-IBPC,
                   :QTP-TRCD,
                   :QTP-REFNO,
                   :QTP-PAYDATE,
                   :QTP-TRAMT,
                   :QTP-INTDUE,
                   :QTP-APOTH,
                   :QTP-APINT,
                   :QTP-APLC,
                   :QTP-APCUR,
                   :QTP-GLNO-1,
                   :QTP-GLNO-2,
                   :QTP-GLNO-3,
                   :QTP-GLAMT-1,
                   :QTP-GLAMT-2,
                   :QTP-GLAMT-3,
                   :QTP-EARNINGS-1,
                   :QTP-EARNINGS-2,
                   :QTP-EARNINGS-3,
                   :QTP-EARNINGS-4,
                   :QTP-EARNINGS-5,
                   :QTP-EARNINGS-6,
                   :QTP-EARNINGS-7,
                   :QTP-EARNINGS-8,
                   :QTP-EARNINGS-9,
                   :QTP-EARNINGS-10,
                   :QTP-OTHBAL,
                   :QTP-INTBAL,
                   :QTP-LCBAL,
                   :QTP-CURBAL,
                   :QTP-PDTH-DATE,
                   :QTP-DLNO,
                   :QTP-DLPROC,
                   :QTP-SERVACCT,
                   :QTP-TILLNO,
                   :QTP-LCPDTH-DATE,
                   :QTP-POSTING-BR,
                   :QTP-OTHBR-FG,
                   :QTP-REV,
                   :QTP-PLTYPE,
                   :QTP-UNITPER-CD
              FROM DBO.TRPFILE
             WHERE TRPFILE.TP_BRNO   = :QTP-BRNO
               AND TRPFILE.TP_DATE   = :QTP-DATE
               AND TRPFILE.TP_CLASS  = :QTP-CLASS
               AND TRPFILE.TP_ACCTNO = :QTP-ACCTNO
               AND TRPFILE.TP_LINE   = :QTP-LINE
				
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

           EXEC SQL SET CONNECTION TRP1_CONNECT END-EXEC.

           IF ( TRP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TRP1_CURSOR INTO  
                      :QTP-BRNO,
                      :QTP-DATE,
                      :QTP-CLASS,
                      :QTP-ACCTNO,
                      :QTP-LINE,
                      :QTP-PLCD,
                      :QTP-SSNO,
                      :QTP-MAKER,
                      :QTP-IBPC,
                      :QTP-TRCD,
                      :QTP-REFNO,
                      :QTP-PAYDATE,
                      :QTP-TRAMT,
                      :QTP-INTDUE,
                      :QTP-APOTH,
                      :QTP-APINT,
                      :QTP-APLC,
                      :QTP-APCUR,
                      :QTP-GLNO-1,
                      :QTP-GLNO-2,
                      :QTP-GLNO-3,
                      :QTP-GLAMT-1,
                      :QTP-GLAMT-2,
                      :QTP-GLAMT-3,
                      :QTP-EARNINGS-1,
                      :QTP-EARNINGS-2,
                      :QTP-EARNINGS-3,
                      :QTP-EARNINGS-4,
                      :QTP-EARNINGS-5,
                      :QTP-EARNINGS-6,
                      :QTP-EARNINGS-7,
                      :QTP-EARNINGS-8,
                      :QTP-EARNINGS-9,
                      :QTP-EARNINGS-10,
                      :QTP-OTHBAL,
                      :QTP-INTBAL,
                      :QTP-LCBAL,
                      :QTP-CURBAL,
                      :QTP-PDTH-DATE,
                      :QTP-DLNO,
                      :QTP-DLPROC,
                      :QTP-SERVACCT,
                      :QTP-TILLNO,
                      :QTP-LCPDTH-DATE,
                      :QTP-POSTING-BR,
                      :QTP-OTHBR-FG,
                      :QTP-REV,
                      :QTP-PLTYPE,
                      :QTP-UNITPER-CD
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRP1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRP-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TRP1-FILE.

           PERFORM CLOSE-IT.

           EXEC SQL SET CONNECTION TRP1_CONNECT END-EXEC.

           IF ( TRP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRP1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRP1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.	  	  
	  	  
	  
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
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-LCFILE NOT = "S" )
              MOVE LC-PATH-BEG             TO LC-SELECT-PATH-BEG
              MOVE LC-PATH-FILE            TO LC-SELECT-PATH-FILE
           ELSE
              MOVE LC-PATH-SQL             TO LC-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-LC1-FILE.
           PERFORM LOAD-LC1-FILE.
           PERFORM OPEN-IT.
           MOVE LC-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LC1-FILE.
           PERFORM START-IT.
           MOVE LC-SELECT-PATH TO E-FILE.
           MOVE LC1-KEY TO E-KEYX.

           IF ( LC1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LC1-FILE.

           IF ( QLC1-WEND-CODE = ZEROES    ) OR
              ( QLC1-WEND-CODE NOT NUMERIC )
              MOVE LC-CODE              TO QLC1-WBEG-CODE
              MOVE ALL "9"              TO QLC1-WEND-CODE.

           EXEC SQL
            DECLARE LC1_CURSOR CURSOR FOR
            SELECT LCFILE.LC_CODE,
                   ISNULL(CAST(LCFILE.LC_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LCFILE.LC_DESC,''),
                   ISNULL(LCFILE.LC_SMALLOAN,''),
                   ISNULL(LCFILE.LC_SALEFIN,''),
                   ISNULL(LCFILE.LC_SERVACCT,''),
                   ISNULL(LCFILE.LC_LOANTYPE,''),
                   ISNULL(LCFILE.LC_PROF1,''),
                   ISNULL(LCFILE.LC_PROF2,''),
                   ISNULL(LCFILE.LC_SKIP_TRW,''),
                   ISNULL(LCFILE.LC_DISPLAY_ERN,''),
                   ISNULL(LCFILE.LC_LCARDFG,''),
                   ISNULL(LCFILE.LC_CALC_HI_CREDIT,''),
                   ISNULL(LCFILE.LC_STOP_ON_DLNO,''),
                   ISNULL(LCFILE.LC_SALES_FORCED_ENTRY,''),
                   ISNULL(LCFILE.LC_AFP_CLASS,''),
                   ISNULL(LCFILE.LC_CON_PDUE_DAYS,''),
                   ISNULL(LCFILE.LC_REC_PDUE_DAYS,''),
                   ISNULL(LCFILE.LC_GIFTNET,''),
                   ISNULL(LCFILE.LC_APP_REQ_FOR_LOAN,''),
                   LCFILE.LC_POOLID,
                   ISNULL(LCFILE.LC_USE_ALT_GL_FEE,''),
                   LCFILE.LC_PROGRP_CLASS,
                   ISNULL(LCFILE.LC_ALLOW_CASH_PROCEEDS,'')
            FROM DBO.LCFILE
            WHERE LCFILE.LC_CODE >= :QLC1-WBEG-CODE
              AND LCFILE.LC_CODE <= :QLC1-WEND-CODE
            ORDER BY LCFILE.LC_CODE
           END-EXEC.

           EXEC SQL
               OPEN LC1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLC1-WBEG-CODE 
                          QLC1-WEND-CODE.

           MOVE STAT---GOOD TO LC1-CURSOR-STAT.

      *-----------------------------------------------------------------
       START-LC1-FILE-GREATER.
           PERFORM START-IT.
           MOVE LC-SELECT-PATH TO E-FILE.
           MOVE LC1-KEY TO E-KEYX.

           IF ( LC1-CURSOR-GR-STAT-GOOD )
              PERFORM CLOSE-LC1-FILE.

           IF ( QLC1-WEND-CODE = ZEROES    ) OR
              ( QLC1-WEND-CODE NOT NUMERIC )
              MOVE LC-CODE              TO QLC1-WBEG-CODE
              MOVE ALL "9"              TO QLC1-WEND-CODE.

           EXEC SQL
            DECLARE LC1_CURSOR_GR CURSOR FOR
            SELECT LCFILE.LC_CODE,
                   ISNULL(CAST(LCFILE.LC_LTOUCH_DATE
                                                   AS VARCHAR(10)),'0'),
                   ISNULL(LCFILE.LC_DESC,''),
                   ISNULL(LCFILE.LC_SMALLOAN,''),
                   ISNULL(LCFILE.LC_SALEFIN,''),
                   ISNULL(LCFILE.LC_SERVACCT,''),
                   ISNULL(LCFILE.LC_LOANTYPE,''),
                   ISNULL(LCFILE.LC_PROF1,''),
                   ISNULL(LCFILE.LC_PROF2,''),
                   ISNULL(LCFILE.LC_SKIP_TRW,''),
                   ISNULL(LCFILE.LC_DISPLAY_ERN,''),
                   ISNULL(LCFILE.LC_LCARDFG,''),
                   ISNULL(LCFILE.LC_CALC_HI_CREDIT,''),
                   ISNULL(LCFILE.LC_STOP_ON_DLNO,''),
                   ISNULL(LCFILE.LC_SALES_FORCED_ENTRY,''),
                   ISNULL(LCFILE.LC_AFP_CLASS,''),
                   ISNULL(LCFILE.LC_CON_PDUE_DAYS,''),
                   ISNULL(LCFILE.LC_REC_PDUE_DAYS,''),
                   ISNULL(LCFILE.LC_GIFTNET,''),
                   ISNULL(LCFILE.LC_APP_REQ_FOR_LOAN,''),
                   LCFILE.LC_POOLID,
                   ISNULL(LCFILE.LC_USE_ALT_GL_FEE,''),
                   LCFILE.LC_PROGRP_CLASS,
                   ISNULL(LCFILE.LC_ALLOW_CASH_PROCEEDS,'')
            FROM DBO.LCFILE
            WHERE LCFILE.LC_CODE >  :QLC1-WBEG-CODE
              AND LCFILE.LC_CODE <= :QLC1-WEND-CODE
            ORDER BY LCFILE.LC_CODE
           END-EXEC.

           EXEC SQL
               OPEN LC1_CURSOR_GR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLC1-WBEG-CODE 
                          QLC1-WEND-CODE.

           MOVE STAT---GOOD TO LC1-CURSOR-GR-STAT.

      *-----------------------------------------------------------------
       READ-LC1-FILE.
           PERFORM READ-IT.
           MOVE LC-SELECT-PATH TO E-FILE.
           MOVE LC1-KEY TO E-KEYX.

           MOVE LC-CODE TO QLC-CODE.

           EXEC SQL
            SELECT LCFILE.LC_CODE,
                   ISNULL(LCFILE.LC_DESC,'')
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
      *    STILL LOADING FILE WORKERS LIKE E-WORKERS
      *-----------------------------------------------------------------
       READ-LC1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LC-SELECT-PATH TO E-FILE.
           MOVE LC1-KEY TO E-KEYX.
           INITIALIZE QLC-REC.

           IF ( LC1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LC1_CURSOR INTO :QLC-REC
              END-EXEC
           ELSE
           IF ( LC1-CURSOR-GR-STAT-GOOD )
              EXEC SQL
                   FETCH LC1_CURSOR_GR INTO :QLC-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO READ-LC1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LC-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LC1-FILE.

           PERFORM CLOSE-IT.
           IF ( LC1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LC1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LC1-CURSOR-STAT.

           IF ( LC1-CURSOR-GR-STAT-GOOD )
              EXEC SQL
                   CLOSE LC1_CURSOR_GR
              END-EXEC
              MOVE STAT---BAD TO LC1-CURSOR-GR-STAT.
	  
      * COPY "LIBLP/LPLN1RN.CPY".
      * COPYMEMBER: LIBLP/LPLN1RN
       LOAD-LN1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LN-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LN-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LN-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LN-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LN-ALL-DATA
              MOVE LN-ALL-PATH             TO LN-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LN-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LN-ALL-BASE
              MOVE FILPATH-MACHINE         TO LN-ALL-MACHINE
              MOVE FILPATH-DATA            TO LN-ALL-DATA
              MOVE LN-ALL-PATH             TO LN-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-LNFILE NOT = "S" )
              MOVE LN-PATH-BEG             TO LN-SELECT-PATH-BEG
              MOVE LN-PATH-FILE            TO LN-SELECT-PATH-FILE
           ELSE
              MOVE LN-PATH-SQL             TO LN-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-LN1-FILE.
           PERFORM LOAD-LN1-FILE.
           PERFORM OPEN-IT.
           MOVE LN-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LN1-FILE.
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-SELECT-PATH TO E-FILE.
           MOVE LN1-KEY        TO E-KEYX.

           IF ( LN1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LN1-FILE.

           IF ( QLN1-WEND-OWNBR = ZEROES    ) OR
              ( QLN1-WEND-OWNBR NOT NUMERIC )
              MOVE LN-OWNBR             TO QLN1-WBEG-OWNBR
              MOVE LN-OWNBR             TO QLN1-WEND-OWNBR
              MOVE LN-ACCTNO            TO QLN1-WBEG-ACCTNO
              MOVE ALL "9"              TO QLN1-WEND-ACCTNO.

           EXEC SQL
            DECLARE LN1_CURSOR CURSOR FOR
            SELECT LNFILE.LN_OWNBR,
                   LNFILE.LN_ACCTNO,
                   LNFILE.LN_CLASS,
                   LNFILE.LN_SUBCLASS,
                   LNFILE.LN_LAWCODE,
                   LNFILE.LN_SSNO1,
                   ISNULL(LNFILE.LN_MAKERCD1,''),
                   LNFILE.LN_SSNO2,
                   ISNULL(LNFILE.LN_MAKERCD2,''),
                   LNFILE.LN_SSNO3,
                   ISNULL(LNFILE.LN_MAKERCD3,''),
                   LNFILE.LN_SSNO4,
                   ISNULL(LNFILE.LN_MAKERCD4,''),
                   ISNULL(LNFILE.LN_ORGST,''),
                   LNFILE.LN_ORGBR,
                   ISNULL(LNFILE.LN_OWNST,''),
                   LNFILE.LN_SPRCLASS,
                   ISNULL(LNFILE.LN_CONVERCD,''),
                   ISNULL(LNFILE.LN_MIDSTREAM,''),
                   ISNULL(LNFILE.LN_PURCD,''),
                   ISNULL(LNFILE.LN_SRCD,''),
                   ISNULL(LNFILE.LN_COLLCD,''),
                   ISNULL(LNFILE.LN_COLLID,''),
                   ISNULL(LNFILE.LN_POCD,''),
                   ISNULL(LNFILE.LN_PLCD,''),
                   ISNULL(LNFILE.LN_PLTYPE,''),
                   ISNULL(LNFILE.LN_CONFIDENTCD,''),
                   ISNULL(LNFILE.LN_ALLOTCD,''),
                   ISNULL(LNFILE.LN_RECASTCD,''),
                   ISNULL(LNFILE.LN_SKIP_TRW,''),
                   ISNULL(LNFILE.LN_STATUSFG,''),
                   ISNULL(LNFILE.LN_SMALLOAN,''),
                   ISNULL(LNFILE.LN_SALEFIN,''),
                   ISNULL(LNFILE.LN_SERVACCT,''),
                   ISNULL(LNFILE.LN_LOANTYPE,''),
                   ISNULL(LNFILE.LN_ADDON_FG,''),
                   ISNULL(LNFILE.LN_PAY_SCHLD_FG,''),
                   LNFILE.LN_LC_CODE,
                   ISNULL(LNFILE.LN_CAN_LOAN_NO,''),
                   ISNULL(LNFILE.LN_STATFLAGS,''),
                   ISNULL(CAST(LNFILE.LN_ENTDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LOANDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PURDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INTDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_CASHDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_FILEDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_RESCISSION_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PART_RECOURSE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_ORIG_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_JDDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PLDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PL_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_ACCELDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_REDUDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_POFFDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PO_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LEGALDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPTDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPT_DISMISS_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPT_DISCHARGE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPT_REAFFIRM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_DEBTAIDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_REPODATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_REDEEMDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_SOLDDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INS_CONFIRM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PRIOR_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LAST_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_TRW_DOFD_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LAST_PYPA_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BT_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_METRO2_LAST_PMT_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_FOLLOWUP_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LAST_ACCRUAL_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_APRATE,
                   LNFILE.LN_EFFRATE,
                   LNFILE.LN_SMPRATE,
                   LNFILE.LN_YLDRATE,
                   LNFILE.LN_OVRDRATE,
                   LNFILE.LN_ENDORATE,
                   LNFILE.LN_JDRATE,
                   LNFILE.LN_ACCRATE,
                   LNFILE.LN_REDURATE,
                   LNFILE.LN_CONRATE,
                   LNFILE.LN_SERVPER,
                   LNFILE.LN_SELLPR,
                   LNFILE.LN_DOWNPY,
                   LNFILE.LN_TRADEIN,
                   LNFILE.LN_LNAMT,
                   LNFILE.LN_FINCHG,
                   LNFILE.LN_INTCHG,
                   LNFILE.LN_SERCHG,
                   LNFILE.LN_EXTCHG,
                   LNFILE.LN_MAINTFEE,
                   LNFILE.LN_POINTS,
                   LNFILE.LN_PROCEEDS,
                   LNFILE.LN_ORGTERM,
                   LNFILE.LN_1STPYAMT,
                   LNFILE.LN_NOREGPY,
                   LNFILE.LN_REGPYAMT,
                   LNFILE.LN_LASTPYAMT,
                   LNFILE.LN_PROCASH,
                   LNFILE.LN_TILLNO,
                   ISNULL(LNFILE.LN_STATE_FLAG,''),
                   LNFILE.LN_STATE_WORKER,
                   LNFILE.LN_STATE_WORKER2,
                   ISNULL(LNFILE.LN_FEECODE_1,''),
                   LNFILE.LN_FEEAMT_1,
                   ISNULL(LNFILE.LN_FEEOURS_1,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_1,''),
                   ISNULL(LNFILE.LN_FEECODE_2,''),
                   LNFILE.LN_FEEAMT_2,
                   ISNULL(LNFILE.LN_FEEOURS_2,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_2,''),
                   ISNULL(LNFILE.LN_FEECODE_3,''),
                   LNFILE.LN_FEEAMT_3,
                   ISNULL(LNFILE.LN_FEEOURS_3,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_3,''),
                   ISNULL(LNFILE.LN_FEECODE_4,''),
                   LNFILE.LN_FEEAMT_4,
                   ISNULL(LNFILE.LN_FEEOURS_4,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_4,''),
                   ISNULL(LNFILE.LN_FEECODE_5,''),
                   LNFILE.LN_FEEAMT_5,
                   ISNULL(LNFILE.LN_FEEOURS_5,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_5,''),
                   ISNULL(LNFILE.LN_FEECODE_6,''),
                   LNFILE.LN_FEEAMT_6,
                   ISNULL(LNFILE.LN_FEEOURS_6,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_6,''),
                   ISNULL(LNFILE.LN_FEECODE_7,''),
                   LNFILE.LN_FEEAMT_7,
                   ISNULL(LNFILE.LN_FEEOURS_7,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_7,''),
                   ISNULL(LNFILE.LN_FEECODE_8,''),
                   LNFILE.LN_FEEAMT_8,
                   ISNULL(LNFILE.LN_FEEOURS_8,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_8,''),
                   ISNULL(LNFILE.LN_FEECODE_9,''),
                   LNFILE.LN_FEEAMT_9,
                   ISNULL(LNFILE.LN_FEEOURS_9,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_9,''),
                   ISNULL(LNFILE.LN_FEECODE_10,''),
                   LNFILE.LN_FEEAMT_10,
                   ISNULL(LNFILE.LN_FEEOURS_10,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_10,''),
                   ISNULL(LNFILE.LN_FEECODE_11,''),
                   LNFILE.LN_FEEAMT_11,
                   ISNULL(LNFILE.LN_FEEOURS_11,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_11,''),
                   ISNULL(LNFILE.LN_FEECODE_12,''),
                   LNFILE.LN_FEEAMT_12,
                   ISNULL(LNFILE.LN_FEEOURS_12,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_12,''),
                   ISNULL(LNFILE.LN_FEECODE_13,''),
                   LNFILE.LN_FEEAMT_13,
                   ISNULL(LNFILE.LN_FEEOURS_13,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_13,''),
                   ISNULL(LNFILE.LN_FEECODE_14,''),
                   LNFILE.LN_FEEAMT_14,
                   ISNULL(LNFILE.LN_FEEOURS_14,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_14,''),
                   ISNULL(LNFILE.LN_FEECODE_15,''),
                   LNFILE.LN_FEEAMT_15,
                   ISNULL(LNFILE.LN_FEEOURS_15,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_15,''),
                   ISNULL(LNFILE.LN_FEECODE_16,''),
                   LNFILE.LN_FEEAMT_16,
                   ISNULL(LNFILE.LN_FEEOURS_16,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_16,''),
                   ISNULL(LNFILE.LN_FEECODE_17,''),
                   LNFILE.LN_FEEAMT_17,
                   ISNULL(LNFILE.LN_FEEOURS_17,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_17,''),
                   ISNULL(LNFILE.LN_FEECODE_18,''),
                   LNFILE.LN_FEEAMT_18,
                   ISNULL(LNFILE.LN_FEEOURS_18,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_18,''),
                   ISNULL(LNFILE.LN_FEECODE_19,''),
                   LNFILE.LN_FEEAMT_19,
                   ISNULL(LNFILE.LN_FEEOURS_19,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_19,''),
                   ISNULL(LNFILE.LN_FEECODE_20,''),
                   LNFILE.LN_FEEAMT_20,
                   ISNULL(LNFILE.LN_FEEOURS_20,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_20,''),
                   ISNULL(LNFILE.LN_INSURED,''),
                   ISNULL(LNFILE.LN_INSTYPES,''),
                   ISNULL(LNFILE.LN_INSELIGIBLE,''),
                   LNFILE.LN_INSCOMP_1,
                   ISNULL(LNFILE.LN_INSOURS_1,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_1,
                   LNFILE.LN_INSCOVR_1,
                   LNFILE.LN_INSCOMM_1,
                   LNFILE.LN_INSANTICERN_1,
                   LNFILE.LN_INSANTICADJ_1,
                   LNFILE.LN_INSUFERN_1,
                   LNFILE.LN_INSACCERN_1,
                   LNFILE.LN_INSEARN_ORDER_1,
                   ISNULL(LNFILE.LN_INSFINCHG_1,''),
                   LNFILE.LN_INSCOMP_2,
                   ISNULL(LNFILE.LN_INSOURS_2,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_2,
                   LNFILE.LN_INSCOVR_2,
                   LNFILE.LN_INSCOMM_2,
                   LNFILE.LN_INSANTICERN_2,
                   LNFILE.LN_INSANTICADJ_2,
                   LNFILE.LN_INSUFERN_2,
                   LNFILE.LN_INSACCERN_2,
                   LNFILE.LN_INSEARN_ORDER_2,
                   ISNULL(LNFILE.LN_INSFINCHG_2,''),
                   LNFILE.LN_INSCOMP_3,
                   ISNULL(LNFILE.LN_INSOURS_3,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_3,
                   LNFILE.LN_INSCOVR_3,
                   LNFILE.LN_INSCOMM_3,
                   LNFILE.LN_INSANTICERN_3,
                   LNFILE.LN_INSANTICADJ_3,
                   LNFILE.LN_INSUFERN_3,
                   LNFILE.LN_INSACCERN_3,
                   LNFILE.LN_INSEARN_ORDER_3,
                   ISNULL(LNFILE.LN_INSFINCHG_3,''),
                   LNFILE.LN_INSCOMP_4,
                   ISNULL(LNFILE.LN_INSOURS_4,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_4,
                   LNFILE.LN_INSCOVR_4,
                   LNFILE.LN_INSCOMM_4,
                   LNFILE.LN_INSANTICERN_4,
                   LNFILE.LN_INSANTICADJ_4,
                   LNFILE.LN_INSUFERN_4,
                   LNFILE.LN_INSACCERN_4,
                   LNFILE.LN_INSEARN_ORDER_4,
                   ISNULL(LNFILE.LN_INSFINCHG_4,''),
                   LNFILE.LN_INSCOMP_5,
                   ISNULL(LNFILE.LN_INSOURS_5,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_5,
                   LNFILE.LN_INSCOVR_5,
                   LNFILE.LN_INSCOMM_5,
                   LNFILE.LN_INSANTICERN_5,
                   LNFILE.LN_INSANTICADJ_5,
                   LNFILE.LN_INSUFERN_5,
                   LNFILE.LN_INSACCERN_5,
                   LNFILE.LN_INSEARN_ORDER_5,
                   ISNULL(LNFILE.LN_INSFINCHG_5,''),
                   LNFILE.LN_INSCOMP_6,
                   ISNULL(LNFILE.LN_INSOURS_6,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_6,
                   LNFILE.LN_INSCOVR_6,
                   LNFILE.LN_INSCOMM_6,
                   LNFILE.LN_INSANTICERN_6,
                   LNFILE.LN_INSANTICADJ_6,
                   LNFILE.LN_INSUFERN_6,
                   LNFILE.LN_INSACCERN_6,
                   LNFILE.LN_INSEARN_ORDER_6,
                   ISNULL(LNFILE.LN_INSFINCHG_6,''),
                   LNFILE.LN_INSCOMP_7,
                   ISNULL(LNFILE.LN_INSOURS_7,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_7,
                   LNFILE.LN_INSCOVR_7,
                   LNFILE.LN_INSCOMM_7,
                   LNFILE.LN_INSANTICERN_7,
                   LNFILE.LN_INSANTICADJ_7,
                   LNFILE.LN_INSUFERN_7,
                   LNFILE.LN_INSACCERN_7,
                   LNFILE.LN_INSEARN_ORDER_7,
                   ISNULL(LNFILE.LN_INSFINCHG_7,''),
                   LNFILE.LN_INSCOMP_8,
                   ISNULL(LNFILE.LN_INSOURS_8,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_8,
                   LNFILE.LN_INSCOVR_8,
                   LNFILE.LN_INSCOMM_8,
                   LNFILE.LN_INSANTICERN_8,
                   LNFILE.LN_INSANTICADJ_8,
                   LNFILE.LN_INSUFERN_8,
                   LNFILE.LN_INSACCERN_8,
                   LNFILE.LN_INSEARN_ORDER_8,
                   ISNULL(LNFILE.LN_INSFINCHG_8,''),
                   LNFILE.LN_INSCOMP_9,
                   ISNULL(LNFILE.LN_INSOURS_9,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_9,
                   LNFILE.LN_INSCOVR_9,
                   LNFILE.LN_INSCOMM_9,
                   LNFILE.LN_INSANTICERN_9,
                   LNFILE.LN_INSANTICADJ_9,
                   LNFILE.LN_INSUFERN_9,
                   LNFILE.LN_INSACCERN_9,
                   LNFILE.LN_INSEARN_ORDER_9,
                   ISNULL(LNFILE.LN_INSFINCHG_9,''),
                   LNFILE.LN_INSCOMP_10,
                   ISNULL(LNFILE.LN_INSOURS_10,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_10,
                   LNFILE.LN_INSCOVR_10,
                   LNFILE.LN_INSCOMM_10,
                   LNFILE.LN_INSANTICERN_10,
                   LNFILE.LN_INSANTICADJ_10,
                   LNFILE.LN_INSUFERN_10,
                   LNFILE.LN_INSACCERN_10,
                   LNFILE.LN_INSEARN_ORDER_10,
                   ISNULL(LNFILE.LN_INSFINCHG_10,''),
                   LNFILE.LN_PY_YYYYMM_RECDEL,
                   LNFILE.LN_MTD_PAYS_RECDEL,
                   ISNULL(LNFILE.LN_GARNISH_EXTR,''),
                   ISNULL(LNFILE.LN_SHIP_SAME_FG,''),
                   LNFILE.LN_AD_D_INS_COVR,
                   LNFILE.LN_TOT_PARTIAL_CHGOFF,
                   ISNULL(LNFILE.LN_LPO,''),
                   ISNULL(LNFILE.LN_REP_CD,''),
                   LNFILE.LN_EMP_ID,
                   ISNULL(LNFILE.LN_PROCESS_CD,''),
                   LNFILE.LN_WCBC_CREDITLIM,
                   ISNULL(LNFILE.LN_FOLLOWUP_CD,''),
                   ISNULL(LNFILE.LN_MIP_FG,''),
                   LNFILE.LN_MIP_MTD_CLAHPAID,
                   ISNULL(CAST(LNFILE.LN_MIP_CLPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_MIP_TOT_CLPAID,
                   ISNULL(CAST(LNFILE.LN_MIP_AHPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_MIP_TOT_AHPAID,
                   LNFILE.LN_IL_GROSS_INCOME,
                   LNFILE.LN_IL_GROSS_INCOME2,
                   ISNULL(LNFILE.LN_RESCISSION,''),
                   ISNULL(LNFILE.LN_INTERIM_TRW,''),
                   LNFILE.LN_DLNO,
                   LNFILE.LN_DLPOOL,
                   ISNULL(LNFILE.LN_DLRECOURSE,''),
                   LNFILE.LN_DLPARTRCAMT,
                   LNFILE.LN_ORIGPURAMT,
                   LNFILE.LN_PURBAL,
                   LNFILE.LN_DLRVPRIN_1,
                   LNFILE.LN_DLRVPRIN_2,
                   LNFILE.LN_DLRVPRIN_3,
                   LNFILE.LN_DLRVCHGS_1,
                   LNFILE.LN_DLRVCHGS_2,
                   LNFILE.LN_DLRVCHGS_3,
                   LNFILE.LN_DLPARVHELD,
                   LNFILE.LN_DLPARVPAID,
                   LNFILE.LN_DLDISC,
                   ISNULL(CAST(LNFILE.LN_LCPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INTPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_CURBAL,
                   LNFILE.LN_INTBAL,
                   LNFILE.LN_LCBAL,
                   LNFILE.LN_OTHBAL,
                   LNFILE.LN_OT2BAL,
                   LNFILE.LN_LCPAID,
                   LNFILE.LN_LCPARTIALS,
                   LNFILE.LN_STMT_COUNTER,
                   LNFILE.LN_PRIOR_STMT_COUNTER,
                   ISNULL(LNFILE.LN_FROZEN_REASON,''),
                   ISNULL(LNFILE.LN_COUPON_STATEMENT,''),
                   LNFILE.LN_MONTHEND_BAL,
                   LNFILE.LN_ANTICTERM,
                   LNFILE.LN_ANTICERN_1,
                   LNFILE.LN_ANTICADJ_1,
                   LNFILE.LN_UFERN_1,
                   LNFILE.LN_ACCERN_1,
                   LNFILE.LN_EARN_ORDER_1,
                   LNFILE.LN_ANTICERN_2,
                   LNFILE.LN_ANTICADJ_2,
                   LNFILE.LN_UFERN_2,
                   LNFILE.LN_ACCERN_2,
                   LNFILE.LN_EARN_ORDER_2,
                   LNFILE.LN_ANTICERN_3,
                   LNFILE.LN_ANTICADJ_3,
                   LNFILE.LN_UFERN_3,
                   LNFILE.LN_ACCERN_3,
                   LNFILE.LN_EARN_ORDER_3,
                   LNFILE.LN_ANTICERN_4,
                   LNFILE.LN_ANTICADJ_4,
                   LNFILE.LN_UFERN_4,
                   LNFILE.LN_ACCERN_4,
                   LNFILE.LN_EARN_ORDER_4,
                   LNFILE.LN_ANTICERN_5,
                   LNFILE.LN_ANTICADJ_5,
                   LNFILE.LN_UFERN_5,
                   LNFILE.LN_ACCERN_5,
                   LNFILE.LN_EARN_ORDER_5,
                   LNFILE.LN_ANTICERN_6,
                   LNFILE.LN_ANTICADJ_6,
                   LNFILE.LN_UFERN_6,
                   LNFILE.LN_ACCERN_6,
                   LNFILE.LN_EARN_ORDER_6,
                   LNFILE.LN_ANTICERN_7,
                   LNFILE.LN_ANTICADJ_7,
                   LNFILE.LN_UFERN_7,
                   LNFILE.LN_ACCERN_7,
                   LNFILE.LN_EARN_ORDER_7,
                   LNFILE.LN_ANTICERN_8,
                   LNFILE.LN_ANTICADJ_8,
                   LNFILE.LN_UFERN_8,
                   LNFILE.LN_ACCERN_8,
                   LNFILE.LN_EARN_ORDER_8,
                   LNFILE.LN_ANTICERN_9,
                   LNFILE.LN_ANTICADJ_9,
                   LNFILE.LN_UFERN_9,
                   LNFILE.LN_ACCERN_9,
                   LNFILE.LN_EARN_ORDER_9,
                   LNFILE.LN_ANTICERN_10,
                   LNFILE.LN_ANTICADJ_10,
                   LNFILE.LN_UFERN_10,
                   LNFILE.LN_ACCERN_10,
                   LNFILE.LN_EARN_ORDER_10,
                   LNFILE.LN_LMO_PRIOR_INTEREST,
                   LNFILE.LN_PRIOR_UNEARNED,
                   ISNULL(LNFILE.LN_ERNDELQ_STOP,''),
                   LNFILE.LN_NONACRERN,
                   LNFILE.LN_PRIOR_NONACRERN,
                   ISNULL(CAST(LNFILE.LN_ERN_IB_PDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_ERN_IB_PRINBAL,
                   LNFILE.LN_ERN_IB_INTOWE,
                   LNFILE.LN_TOTERN,
                   LNFILE.LN_TOTOTH,
                   LNFILE.LN_TOTINT,
                   LNFILE.LN_TOTLCHG,
                   LNFILE.LN_TOTDEF,
                   LNFILE.LN_TOTSERACCT,
                   LNFILE.LN_LCWAIVERS,
                   LNFILE.LN_INTWAIVERS,
                   LNFILE.LN_TOTPAYMNTD,
                   LNFILE.LN_TOTEXCPAYMNTD,
                   LNFILE.LN_TOTNODEF,
                   LNFILE.LN_YTDNODEF,
                   LNFILE.LN_TOTNODEFACT,
                   LNFILE.LN_YTDNODEFACT,
                   LNFILE.LN_TOTNOALDEL,
                   LNFILE.LN_YTDNOALDEL,
                   LNFILE.LN_YTDINTPD,
                   LNFILE.LN_LYRINTPD,
                   LNFILE.LN_TOTINTPD,
                   LNFILE.LN_RECASTINT,
                   LNFILE.LN_RECASTPRIN,
                   ISNULL(LNFILE.LN_CL_REBATE,''),
                   ISNULL(LNFILE.LN_AH_REBATE,''),
                   ISNULL(LNFILE.LN_PP_REBATE,''),
                   ISNULL(LNFILE.LN_INT_REBATE,''),
                   ISNULL(LNFILE.LN_SC_REBATE,''),
                   ISNULL(LNFILE.LN_MF_REBATE,''),
                   ISNULL(LNFILE.LN_DF_REBATE,''),
                   ISNULL(LNFILE.LN_EX_REBATE,''),
                   ISNULL(LNFILE.LN_O1_REBATE,''),
                   ISNULL(LNFILE.LN_O2_REBATE,''),
                   ISNULL(LNFILE.LN_O3_REBATE,''),
                   ISNULL(LNFILE.LN_O4_REBATE,''),
                   ISNULL(LNFILE.LN_O5_REBATE,''),
                   ISNULL(LNFILE.LN_O6_REBATE,''),
                   ISNULL(LNFILE.LN_O7_REBATE,''),
                   ISNULL(LNFILE.LN_UN_O7_REB,''),
                   ISNULL(LNFILE.LN_UN_O8_REB,''),
                   ISNULL(LNFILE.LN_UN_O9_REB,''),
                   ISNULL(LNFILE.LN_UN_O10_REB,''),
                   ISNULL(LNFILE.LN_PLREASON,''),
                   ISNULL(LNFILE.LN_COLLECTOR,''),
                   ISNULL(LNFILE.LN_ACTIONCD,''),
                   ISNULL(LNFILE.LN_ACTIONCD2,''),
                   ISNULL(LNFILE.LN_ATCODE,''),
                   ISNULL(LNFILE.LN_RECODE,''),
                   ISNULL(LNFILE.LN_ESCROW_FG,''),
                   LNFILE.LN_PROMDAYS_PASTDUE,
                   ISNULL(LNFILE.LN_PROMID,''),
                   ISNULL(CAST(LNFILE.LN_PROMDATE AS VARCHAR(10)),'0'),
                   LNFILE.LN_PROMAMT,
                   ISNULL(CAST(LNFILE.LN_PROMDATE2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_DATE_PAID_LAST
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_AMT_PAID_LAST,
                   LNFILE.LN_LASTCFORMNO,
                   ISNULL(CAST(LNFILE.LN_LASTCFORMDATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_DELCNT,
                   LNFILE.LN_DELCNT2,
                   LNFILE.LN_CREDITLIM,
                   LNFILE.LN_CRSCORE,
                   LNFILE.LN_LASTSFORMNO,
                   ISNULL(CAST(LNFILE.LN_LASTSFORMDATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_SOLCNT,
                   ISNULL(LNFILE.LN_MESSAGE,''),
                   LNFILE.LN_OLDACCTNO,
                   LNFILE.LN_PRLNNO,
                   LNFILE.LN_PRBAL,
                   LNFILE.LN_PRINTDUE,
                   LNFILE.LN_PRFINCHGRB,
                   LNFILE.LN_PRINSRB,
                   ISNULL(LNFILE.LN_PRPOCD,''),
                   ISNULL(CAST(LNFILE.LN_PRPODATE AS VARCHAR(10)),'0'),
                   LNFILE.LN_PRDELINQAMT,
                   LNFILE.LN_PRLNAMT,
                   LNFILE.LN_PRPDUE_DAYS,
                   LNFILE.LN_PRDLDISC_UNEARN,
                   LNFILE.LN_PRCONPROFILE,
                   LNFILE.LN_SUM_ORIGPURAMT,
                   LNFILE.LN_SUM_LNAMT,
                   LNFILE.LN_SUM_FINCHG,
                   LNFILE.LN_SUM_INTCHG,
                   LNFILE.LN_SUM_ORGTERM,
                   LNFILE.LN_SUM_REGPYAMT,
                   LNFILE.LN_SUM_EXTCHG,
                   LNFILE.LN_SUM_SERCHG,
                   LNFILE.LN_SUM_MAINTFEE,
                   ISNULL(LNFILE.LN_UNITPER_CD,''),
                   LNFILE.LN_UNITPER_FREQ,
                   LNFILE.LN_DAYS_IN_UNITPER,
                   LNFILE.LN_UNITPER_PER_YEAR,
                   ISNULL(LNFILE.LN_REPOCD,''),
                   ISNULL(LNFILE.LN_BR_TFR_FG,''),
                   LNFILE.LN_O3INS_DLPROC,
                   LNFILE.LN_INSPREM_DLADJ,
                   LNFILE.LN_PLAMT,
                   LNFILE.LN_MTD_PRIN,
                   LNFILE.LN_MTD_LC,
                   LNFILE.LN_MTD_INT,
                   LNFILE.LN_MTD_OT,
                   LNFILE.LN_MTD_EX,
                   LNFILE.LN_BNKRPT_CHAPTER,
                   ISNULL(LNFILE.LN_BMO_CON_STATUS,''),
                   ISNULL(LNFILE.LN_BMO_REC_STATUS,''),
                   LNFILE.LN_APP_NUMBER,
                   ISNULL(LNFILE.LN_GIFTNET,''),
                   LNFILE.LN_GIFTNET_OUR_PO,
                   ISNULL(LNFILE.LN_PROC_DISBURSED,''),
                   ISNULL(LNFILE.LN_NOODDPY,''),
                   LNFILE.LN_CONPROFILE_1,
                   LNFILE.LN_CONPROFILE_2,
                   LNFILE.LN_CONPROFILE_3,
                   LNFILE.LN_CONPROFILE_4,
                   LNFILE.LN_CONPROFILE_5,
                   LNFILE.LN_RECPROFILE_1,
                   LNFILE.LN_RECPROFILE_2,
                   LNFILE.LN_RECPROFILE_3,
                   LNFILE.LN_RECPROFILE_4,
                   LNFILE.LN_RECPROFILE_5,
                   ISNULL(LNFILE.LN_TRW_STATUS,''),
                   ISNULL(LNFILE.LN_ORIGIN,''),
                   ISNULL(LNFILE.LN_PO_TRCD,''),
                   LNFILE.LN_NEXIS_CRSCORE,
                   ISNULL(LNFILE.LN_TRW_B_CONSUMER_IND,''),
                   ISNULL(LNFILE.LN_PARVHELD_PAID_FG,''),
                   ISNULL(LNFILE.LN_ACCT_FROZEN,''),
                   ISNULL(LNFILE.LN_TRW_SPECIAL_CODE,''),
                   ISNULL(LNFILE.LN_CREATION_STATUS,''),
                   LNFILE.LN_TOTNOLCHG,
                   ISNULL(LNFILE.LN_LBOX,''),
                   LNFILE.LN_EXTRA_PRIN_LFP,
                   LNFILE.LN_REMAIN_INT_SINCE_LFP,
                   LNFILE.LN_REMAIN_PRIN_SINCE_LFP,
                   ISNULL(LNFILE.LN_INTERIM_COUPON,''),
                   LNFILE.LN_DLCOMM_O3_CMSI,
                   LNFILE.LN_DLCOMM_PP_CMSI,
                   ISNULL(LNFILE.LN_TRW_J_CONSUMER_IND,''),
                   ISNULL(LNFILE.LN_ACH,''),
                   ISNULL(LNFILE.LN_1099_C_FG,''),
                   ISNULL(LNFILE.LN_TRW_COMPLIANCE_CODE,''),
                   ISNULL(LNFILE.LN_UNWIND_REASON_CODE,''),
                   ISNULL(LNFILE.LN_PBI_PBD_FG,''),
                   ISNULL(LNFILE.LN_OLD_ACCT_FORMAT_FG,''),
                   ISNULL(LNFILE.LN_APPROVED_BY_USER,''),
                   ISNULL(LNFILE.LN_APPROVED_BY_CODE,''),
                   ISNULL(CAST(LNFILE.LN_APPROVED_BY_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_APPROVED_BY_TIME,
                   ISNULL(CAST(LNFILE.LN_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_LTOUCH_TIME
            FROM DBO.LNFILE
            WHERE LNFILE.LN_OWNBR >= :QLN1-WBEG-OWNBR
              AND LNFILE.LN_OWNBR <= :QLN1-WEND-OWNBR
              AND LNFILE.LN_ACCTNO >= :QLN1-WBEG-ACCTNO
              AND LNFILE.LN_ACCTNO <= :QLN1-WEND-ACCTNO 
            ORDER BY LNFILE.LN_OWNBR,
                     LNFILE.LN_ACCTNO
           END-EXEC.

           EXEC SQL
               OPEN LN1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLN1-WBEG-OWNBR
                          QLN1-WEND-OWNBR
                          QLN1-WBEG-ACCTNO
                          QLN1-WEND-ACCTNO.

           MOVE STAT---GOOD TO LN1-CURSOR-STAT.

      *-----------------------------------------------------------------
       START-LN1-FILE-GREATER.
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-SELECT-PATH TO E-FILE.
           MOVE LN1-KEY        TO E-KEYX.

           IF ( LN1-CURSOR-GR-STAT-GOOD )
              PERFORM CLOSE-LN1-FILE.

           IF ( QLN1-WEND-OWNBR = ZEROES    ) OR
              ( QLN1-WEND-OWNBR NOT NUMERIC )
              MOVE LN-OWNBR             TO QLN1-WBEG-OWNBR
              MOVE LN-OWNBR             TO QLN1-WEND-OWNBR
              MOVE LN-ACCTNO            TO QLN1-WBEG-ACCTNO
              MOVE ALL "9"              TO QLN1-WEND-ACCTNO.

           EXEC SQL
            DECLARE LN1_CURSOR_GR CURSOR FOR
            SELECT LNFILE.LN_OWNBR,
                   LNFILE.LN_ACCTNO,
                   LNFILE.LN_CLASS,
                   LNFILE.LN_SUBCLASS,
                   LNFILE.LN_LAWCODE,
                   LNFILE.LN_SSNO1,
                   ISNULL(LNFILE.LN_MAKERCD1,''),
                   LNFILE.LN_SSNO2,
                   ISNULL(LNFILE.LN_MAKERCD2,''),
                   LNFILE.LN_SSNO3,
                   ISNULL(LNFILE.LN_MAKERCD3,''),
                   LNFILE.LN_SSNO4,
                   ISNULL(LNFILE.LN_MAKERCD4,''),
                   ISNULL(LNFILE.LN_ORGST,''),
                   LNFILE.LN_ORGBR,
                   ISNULL(LNFILE.LN_OWNST,''),
                   LNFILE.LN_SPRCLASS,
                   ISNULL(LNFILE.LN_CONVERCD,''),
                   ISNULL(LNFILE.LN_MIDSTREAM,''),
                   ISNULL(LNFILE.LN_PURCD,''),
                   ISNULL(LNFILE.LN_SRCD,''),
                   ISNULL(LNFILE.LN_COLLCD,''),
                   ISNULL(LNFILE.LN_COLLID,''),
                   ISNULL(LNFILE.LN_POCD,''),
                   ISNULL(LNFILE.LN_PLCD,''),
                   ISNULL(LNFILE.LN_PLTYPE,''),
                   ISNULL(LNFILE.LN_CONFIDENTCD,''),
                   ISNULL(LNFILE.LN_ALLOTCD,''),
                   ISNULL(LNFILE.LN_RECASTCD,''),
                   ISNULL(LNFILE.LN_SKIP_TRW,''),
                   ISNULL(LNFILE.LN_STATUSFG,''),
                   ISNULL(LNFILE.LN_SMALLOAN,''),
                   ISNULL(LNFILE.LN_SALEFIN,''),
                   ISNULL(LNFILE.LN_SERVACCT,''),
                   ISNULL(LNFILE.LN_LOANTYPE,''),
                   ISNULL(LNFILE.LN_ADDON_FG,''),
                   ISNULL(LNFILE.LN_PAY_SCHLD_FG,''),
                   LNFILE.LN_LC_CODE,
                   ISNULL(LNFILE.LN_CAN_LOAN_NO,''),
                   ISNULL(LNFILE.LN_STATFLAGS,''),
                   ISNULL(CAST(LNFILE.LN_ENTDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LOANDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PURDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INTDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_CASHDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_FILEDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_RESCISSION_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PART_RECOURSE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_ORIG_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_JDDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PLDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PL_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_ACCELDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_REDUDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_POFFDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PO_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LEGALDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPTDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPT_DISMISS_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPT_DISCHARGE_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BNKRPT_REAFFIRM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_DEBTAIDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_REPODATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_REDEEMDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_SOLDDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INS_CONFIRM_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_PRIOR_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LAST_RN_FOR10_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_TRW_DOFD_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LAST_PYPA_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_BT_TRDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_METRO2_LAST_PMT_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_FOLLOWUP_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_LAST_ACCRUAL_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_APRATE,
                   LNFILE.LN_EFFRATE,
                   LNFILE.LN_SMPRATE,
                   LNFILE.LN_YLDRATE,
                   LNFILE.LN_OVRDRATE,
                   LNFILE.LN_ENDORATE,
                   LNFILE.LN_JDRATE,
                   LNFILE.LN_ACCRATE,
                   LNFILE.LN_REDURATE,
                   LNFILE.LN_CONRATE,
                   LNFILE.LN_SERVPER,
                   LNFILE.LN_SELLPR,
                   LNFILE.LN_DOWNPY,
                   LNFILE.LN_TRADEIN,
                   LNFILE.LN_LNAMT,
                   LNFILE.LN_FINCHG,
                   LNFILE.LN_INTCHG,
                   LNFILE.LN_SERCHG,
                   LNFILE.LN_EXTCHG,
                   LNFILE.LN_MAINTFEE,
                   LNFILE.LN_POINTS,
                   LNFILE.LN_PROCEEDS,
                   LNFILE.LN_ORGTERM,
                   LNFILE.LN_1STPYAMT,
                   LNFILE.LN_NOREGPY,
                   LNFILE.LN_REGPYAMT,
                   LNFILE.LN_LASTPYAMT,
                   LNFILE.LN_PROCASH,
                   LNFILE.LN_TILLNO,
                   ISNULL(LNFILE.LN_STATE_FLAG,''),
                   LNFILE.LN_STATE_WORKER,
                   LNFILE.LN_STATE_WORKER2,
                   ISNULL(LNFILE.LN_FEECODE_1,''),
                   LNFILE.LN_FEEAMT_1,
                   ISNULL(LNFILE.LN_FEEOURS_1,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_1,''),
                   ISNULL(LNFILE.LN_FEECODE_2,''),
                   LNFILE.LN_FEEAMT_2,
                   ISNULL(LNFILE.LN_FEEOURS_2,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_2,''),
                   ISNULL(LNFILE.LN_FEECODE_3,''),
                   LNFILE.LN_FEEAMT_3,
                   ISNULL(LNFILE.LN_FEEOURS_3,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_3,''),
                   ISNULL(LNFILE.LN_FEECODE_4,''),
                   LNFILE.LN_FEEAMT_4,
                   ISNULL(LNFILE.LN_FEEOURS_4,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_4,''),
                   ISNULL(LNFILE.LN_FEECODE_5,''),
                   LNFILE.LN_FEEAMT_5,
                   ISNULL(LNFILE.LN_FEEOURS_5,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_5,''),
                   ISNULL(LNFILE.LN_FEECODE_6,''),
                   LNFILE.LN_FEEAMT_6,
                   ISNULL(LNFILE.LN_FEEOURS_6,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_6,''),
                   ISNULL(LNFILE.LN_FEECODE_7,''),
                   LNFILE.LN_FEEAMT_7,
                   ISNULL(LNFILE.LN_FEEOURS_7,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_7,''),
                   ISNULL(LNFILE.LN_FEECODE_8,''),
                   LNFILE.LN_FEEAMT_8,
                   ISNULL(LNFILE.LN_FEEOURS_8,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_8,''),
                   ISNULL(LNFILE.LN_FEECODE_9,''),
                   LNFILE.LN_FEEAMT_9,
                   ISNULL(LNFILE.LN_FEEOURS_9,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_9,''),
                   ISNULL(LNFILE.LN_FEECODE_10,''),
                   LNFILE.LN_FEEAMT_10,
                   ISNULL(LNFILE.LN_FEEOURS_10,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_10,''),
                   ISNULL(LNFILE.LN_FEECODE_11,''),
                   LNFILE.LN_FEEAMT_11,
                   ISNULL(LNFILE.LN_FEEOURS_11,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_11,''),
                   ISNULL(LNFILE.LN_FEECODE_12,''),
                   LNFILE.LN_FEEAMT_12,
                   ISNULL(LNFILE.LN_FEEOURS_12,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_12,''),
                   ISNULL(LNFILE.LN_FEECODE_13,''),
                   LNFILE.LN_FEEAMT_13,
                   ISNULL(LNFILE.LN_FEEOURS_13,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_13,''),
                   ISNULL(LNFILE.LN_FEECODE_14,''),
                   LNFILE.LN_FEEAMT_14,
                   ISNULL(LNFILE.LN_FEEOURS_14,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_14,''),
                   ISNULL(LNFILE.LN_FEECODE_15,''),
                   LNFILE.LN_FEEAMT_15,
                   ISNULL(LNFILE.LN_FEEOURS_15,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_15,''),
                   ISNULL(LNFILE.LN_FEECODE_16,''),
                   LNFILE.LN_FEEAMT_16,
                   ISNULL(LNFILE.LN_FEEOURS_16,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_16,''),
                   ISNULL(LNFILE.LN_FEECODE_17,''),
                   LNFILE.LN_FEEAMT_17,
                   ISNULL(LNFILE.LN_FEEOURS_17,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_17,''),
                   ISNULL(LNFILE.LN_FEECODE_18,''),
                   LNFILE.LN_FEEAMT_18,
                   ISNULL(LNFILE.LN_FEEOURS_18,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_18,''),
                   ISNULL(LNFILE.LN_FEECODE_19,''),
                   LNFILE.LN_FEEAMT_19,
                   ISNULL(LNFILE.LN_FEEOURS_19,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_19,''),
                   ISNULL(LNFILE.LN_FEECODE_20,''),
                   LNFILE.LN_FEEAMT_20,
                   ISNULL(LNFILE.LN_FEEOURS_20,''),
                   ISNULL(LNFILE.LN_FEEFINCHG_20,''),
                   ISNULL(LNFILE.LN_INSURED,''),
                   ISNULL(LNFILE.LN_INSTYPES,''),
                   ISNULL(LNFILE.LN_INSELIGIBLE,''),
                   LNFILE.LN_INSCOMP_1,
                   ISNULL(LNFILE.LN_INSOURS_1,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_1,
                   LNFILE.LN_INSCOVR_1,
                   LNFILE.LN_INSCOMM_1,
                   LNFILE.LN_INSANTICERN_1,
                   LNFILE.LN_INSANTICADJ_1,
                   LNFILE.LN_INSUFERN_1,
                   LNFILE.LN_INSACCERN_1,
                   LNFILE.LN_INSEARN_ORDER_1,
                   ISNULL(LNFILE.LN_INSFINCHG_1,''),
                   LNFILE.LN_INSCOMP_2,
                   ISNULL(LNFILE.LN_INSOURS_2,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_2,
                   LNFILE.LN_INSCOVR_2,
                   LNFILE.LN_INSCOMM_2,
                   LNFILE.LN_INSANTICERN_2,
                   LNFILE.LN_INSANTICADJ_2,
                   LNFILE.LN_INSUFERN_2,
                   LNFILE.LN_INSACCERN_2,
                   LNFILE.LN_INSEARN_ORDER_2,
                   ISNULL(LNFILE.LN_INSFINCHG_2,''),
                   LNFILE.LN_INSCOMP_3,
                   ISNULL(LNFILE.LN_INSOURS_3,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_3,
                   LNFILE.LN_INSCOVR_3,
                   LNFILE.LN_INSCOMM_3,
                   LNFILE.LN_INSANTICERN_3,
                   LNFILE.LN_INSANTICADJ_3,
                   LNFILE.LN_INSUFERN_3,
                   LNFILE.LN_INSACCERN_3,
                   LNFILE.LN_INSEARN_ORDER_3,
                   ISNULL(LNFILE.LN_INSFINCHG_3,''),
                   LNFILE.LN_INSCOMP_4,
                   ISNULL(LNFILE.LN_INSOURS_4,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_4
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_4,
                   LNFILE.LN_INSCOVR_4,
                   LNFILE.LN_INSCOMM_4,
                   LNFILE.LN_INSANTICERN_4,
                   LNFILE.LN_INSANTICADJ_4,
                   LNFILE.LN_INSUFERN_4,
                   LNFILE.LN_INSACCERN_4,
                   LNFILE.LN_INSEARN_ORDER_4,
                   ISNULL(LNFILE.LN_INSFINCHG_4,''),
                   LNFILE.LN_INSCOMP_5,
                   ISNULL(LNFILE.LN_INSOURS_5,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_5
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_5,
                   LNFILE.LN_INSCOVR_5,
                   LNFILE.LN_INSCOMM_5,
                   LNFILE.LN_INSANTICERN_5,
                   LNFILE.LN_INSANTICADJ_5,
                   LNFILE.LN_INSUFERN_5,
                   LNFILE.LN_INSACCERN_5,
                   LNFILE.LN_INSEARN_ORDER_5,
                   ISNULL(LNFILE.LN_INSFINCHG_5,''),
                   LNFILE.LN_INSCOMP_6,
                   ISNULL(LNFILE.LN_INSOURS_6,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_6
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_6,
                   LNFILE.LN_INSCOVR_6,
                   LNFILE.LN_INSCOMM_6,
                   LNFILE.LN_INSANTICERN_6,
                   LNFILE.LN_INSANTICADJ_6,
                   LNFILE.LN_INSUFERN_6,
                   LNFILE.LN_INSACCERN_6,
                   LNFILE.LN_INSEARN_ORDER_6,
                   ISNULL(LNFILE.LN_INSFINCHG_6,''),
                   LNFILE.LN_INSCOMP_7,
                   ISNULL(LNFILE.LN_INSOURS_7,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_7
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_7,
                   LNFILE.LN_INSCOVR_7,
                   LNFILE.LN_INSCOMM_7,
                   LNFILE.LN_INSANTICERN_7,
                   LNFILE.LN_INSANTICADJ_7,
                   LNFILE.LN_INSUFERN_7,
                   LNFILE.LN_INSACCERN_7,
                   LNFILE.LN_INSEARN_ORDER_7,
                   ISNULL(LNFILE.LN_INSFINCHG_7,''),
                   LNFILE.LN_INSCOMP_8,
                   ISNULL(LNFILE.LN_INSOURS_8,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_8
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_8,
                   LNFILE.LN_INSCOVR_8,
                   LNFILE.LN_INSCOMM_8,
                   LNFILE.LN_INSANTICERN_8,
                   LNFILE.LN_INSANTICADJ_8,
                   LNFILE.LN_INSUFERN_8,
                   LNFILE.LN_INSACCERN_8,
                   LNFILE.LN_INSEARN_ORDER_8,
                   ISNULL(LNFILE.LN_INSFINCHG_8,''),
                   LNFILE.LN_INSCOMP_9,
                   ISNULL(LNFILE.LN_INSOURS_9,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_9
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_9,
                   LNFILE.LN_INSCOVR_9,
                   LNFILE.LN_INSCOMM_9,
                   LNFILE.LN_INSANTICERN_9,
                   LNFILE.LN_INSANTICADJ_9,
                   LNFILE.LN_INSUFERN_9,
                   LNFILE.LN_INSACCERN_9,
                   LNFILE.LN_INSEARN_ORDER_9,
                   ISNULL(LNFILE.LN_INSFINCHG_9,''),
                   LNFILE.LN_INSCOMP_10,
                   ISNULL(LNFILE.LN_INSOURS_10,''),
                   ISNULL(CAST(LNFILE.LN_INSEFF_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INSEXP_DATE_10
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_INSPREM_10,
                   LNFILE.LN_INSCOVR_10,
                   LNFILE.LN_INSCOMM_10,
                   LNFILE.LN_INSANTICERN_10,
                   LNFILE.LN_INSANTICADJ_10,
                   LNFILE.LN_INSUFERN_10,
                   LNFILE.LN_INSACCERN_10,
                   LNFILE.LN_INSEARN_ORDER_10,
                   ISNULL(LNFILE.LN_INSFINCHG_10,''),
                   LNFILE.LN_PY_YYYYMM_RECDEL,
                   LNFILE.LN_MTD_PAYS_RECDEL,
                   ISNULL(LNFILE.LN_GARNISH_EXTR,''),
                   ISNULL(LNFILE.LN_SHIP_SAME_FG,''),
                   LNFILE.LN_AD_D_INS_COVR,
                   LNFILE.LN_TOT_PARTIAL_CHGOFF,
                   ISNULL(LNFILE.LN_LPO,''),
                   ISNULL(LNFILE.LN_REP_CD,''),
                   LNFILE.LN_EMP_ID,
                   ISNULL(LNFILE.LN_PROCESS_CD,''),
                   LNFILE.LN_WCBC_CREDITLIM,
                   ISNULL(LNFILE.LN_FOLLOWUP_CD,''),
                   ISNULL(LNFILE.LN_MIP_FG,''),
                   LNFILE.LN_MIP_MTD_CLAHPAID,
                   ISNULL(CAST(LNFILE.LN_MIP_CLPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_MIP_TOT_CLPAID,
                   ISNULL(CAST(LNFILE.LN_MIP_AHPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_MIP_TOT_AHPAID,
                   LNFILE.LN_IL_GROSS_INCOME,
                   LNFILE.LN_IL_GROSS_INCOME2,
                   ISNULL(LNFILE.LN_RESCISSION,''),
                   ISNULL(LNFILE.LN_INTERIM_TRW,''),
                   LNFILE.LN_DLNO,
                   LNFILE.LN_DLPOOL,
                   ISNULL(LNFILE.LN_DLRECOURSE,''),
                   LNFILE.LN_DLPARTRCAMT,
                   LNFILE.LN_ORIGPURAMT,
                   LNFILE.LN_PURBAL,
                   LNFILE.LN_DLRVPRIN_1,
                   LNFILE.LN_DLRVPRIN_2,
                   LNFILE.LN_DLRVPRIN_3,
                   LNFILE.LN_DLRVCHGS_1,
                   LNFILE.LN_DLRVCHGS_2,
                   LNFILE.LN_DLRVCHGS_3,
                   LNFILE.LN_DLPARVHELD,
                   LNFILE.LN_DLPARVPAID,
                   LNFILE.LN_DLDISC,
                   ISNULL(CAST(LNFILE.LN_LCPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_INTPDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_CURBAL,
                   LNFILE.LN_INTBAL,
                   LNFILE.LN_LCBAL,
                   LNFILE.LN_OTHBAL,
                   LNFILE.LN_OT2BAL,
                   LNFILE.LN_LCPAID,
                   LNFILE.LN_LCPARTIALS,
                   LNFILE.LN_STMT_COUNTER,
                   LNFILE.LN_PRIOR_STMT_COUNTER,
                   ISNULL(LNFILE.LN_FROZEN_REASON,''),
                   ISNULL(LNFILE.LN_COUPON_STATEMENT,''),
                   LNFILE.LN_MONTHEND_BAL,
                   LNFILE.LN_ANTICTERM,
                   LNFILE.LN_ANTICERN_1,
                   LNFILE.LN_ANTICADJ_1,
                   LNFILE.LN_UFERN_1,
                   LNFILE.LN_ACCERN_1,
                   LNFILE.LN_EARN_ORDER_1,
                   LNFILE.LN_ANTICERN_2,
                   LNFILE.LN_ANTICADJ_2,
                   LNFILE.LN_UFERN_2,
                   LNFILE.LN_ACCERN_2,
                   LNFILE.LN_EARN_ORDER_2,
                   LNFILE.LN_ANTICERN_3,
                   LNFILE.LN_ANTICADJ_3,
                   LNFILE.LN_UFERN_3,
                   LNFILE.LN_ACCERN_3,
                   LNFILE.LN_EARN_ORDER_3,
                   LNFILE.LN_ANTICERN_4,
                   LNFILE.LN_ANTICADJ_4,
                   LNFILE.LN_UFERN_4,
                   LNFILE.LN_ACCERN_4,
                   LNFILE.LN_EARN_ORDER_4,
                   LNFILE.LN_ANTICERN_5,
                   LNFILE.LN_ANTICADJ_5,
                   LNFILE.LN_UFERN_5,
                   LNFILE.LN_ACCERN_5,
                   LNFILE.LN_EARN_ORDER_5,
                   LNFILE.LN_ANTICERN_6,
                   LNFILE.LN_ANTICADJ_6,
                   LNFILE.LN_UFERN_6,
                   LNFILE.LN_ACCERN_6,
                   LNFILE.LN_EARN_ORDER_6,
                   LNFILE.LN_ANTICERN_7,
                   LNFILE.LN_ANTICADJ_7,
                   LNFILE.LN_UFERN_7,
                   LNFILE.LN_ACCERN_7,
                   LNFILE.LN_EARN_ORDER_7,
                   LNFILE.LN_ANTICERN_8,
                   LNFILE.LN_ANTICADJ_8,
                   LNFILE.LN_UFERN_8,
                   LNFILE.LN_ACCERN_8,
                   LNFILE.LN_EARN_ORDER_8,
                   LNFILE.LN_ANTICERN_9,
                   LNFILE.LN_ANTICADJ_9,
                   LNFILE.LN_UFERN_9,
                   LNFILE.LN_ACCERN_9,
                   LNFILE.LN_EARN_ORDER_9,
                   LNFILE.LN_ANTICERN_10,
                   LNFILE.LN_ANTICADJ_10,
                   LNFILE.LN_UFERN_10,
                   LNFILE.LN_ACCERN_10,
                   LNFILE.LN_EARN_ORDER_10,
                   LNFILE.LN_LMO_PRIOR_INTEREST,
                   LNFILE.LN_PRIOR_UNEARNED,
                   ISNULL(LNFILE.LN_ERNDELQ_STOP,''),
                   LNFILE.LN_NONACRERN,
                   LNFILE.LN_PRIOR_NONACRERN,
                   ISNULL(CAST(LNFILE.LN_ERN_IB_PDTH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_ERN_IB_PRINBAL,
                   LNFILE.LN_ERN_IB_INTOWE,
                   LNFILE.LN_TOTERN,
                   LNFILE.LN_TOTOTH,
                   LNFILE.LN_TOTINT,
                   LNFILE.LN_TOTLCHG,
                   LNFILE.LN_TOTDEF,
                   LNFILE.LN_TOTSERACCT,
                   LNFILE.LN_LCWAIVERS,
                   LNFILE.LN_INTWAIVERS,
                   LNFILE.LN_TOTPAYMNTD,
                   LNFILE.LN_TOTEXCPAYMNTD,
                   LNFILE.LN_TOTNODEF,
                   LNFILE.LN_YTDNODEF,
                   LNFILE.LN_TOTNODEFACT,
                   LNFILE.LN_YTDNODEFACT,
                   LNFILE.LN_TOTNOALDEL,
                   LNFILE.LN_YTDNOALDEL,
                   LNFILE.LN_YTDINTPD,
                   LNFILE.LN_LYRINTPD,
                   LNFILE.LN_TOTINTPD,
                   LNFILE.LN_RECASTINT,
                   LNFILE.LN_RECASTPRIN,
                   ISNULL(LNFILE.LN_CL_REBATE,''),
                   ISNULL(LNFILE.LN_AH_REBATE,''),
                   ISNULL(LNFILE.LN_PP_REBATE,''),
                   ISNULL(LNFILE.LN_INT_REBATE,''),
                   ISNULL(LNFILE.LN_SC_REBATE,''),
                   ISNULL(LNFILE.LN_MF_REBATE,''),
                   ISNULL(LNFILE.LN_DF_REBATE,''),
                   ISNULL(LNFILE.LN_EX_REBATE,''),
                   ISNULL(LNFILE.LN_O1_REBATE,''),
                   ISNULL(LNFILE.LN_O2_REBATE,''),
                   ISNULL(LNFILE.LN_O3_REBATE,''),
                   ISNULL(LNFILE.LN_O4_REBATE,''),
                   ISNULL(LNFILE.LN_O5_REBATE,''),
                   ISNULL(LNFILE.LN_O6_REBATE,''),
                   ISNULL(LNFILE.LN_O7_REBATE,''),
                   ISNULL(LNFILE.LN_UN_O7_REB,''),
                   ISNULL(LNFILE.LN_UN_O8_REB,''),
                   ISNULL(LNFILE.LN_UN_O9_REB,''),
                   ISNULL(LNFILE.LN_UN_O10_REB,''),
                   ISNULL(LNFILE.LN_PLREASON,''),
                   ISNULL(LNFILE.LN_COLLECTOR,''),
                   ISNULL(LNFILE.LN_ACTIONCD,''),
                   ISNULL(LNFILE.LN_ACTIONCD2,''),
                   ISNULL(LNFILE.LN_ATCODE,''),
                   ISNULL(LNFILE.LN_RECODE,''),
                   ISNULL(LNFILE.LN_ESCROW_FG,''),
                   LNFILE.LN_PROMDAYS_PASTDUE,
                   ISNULL(LNFILE.LN_PROMID,''),
                   ISNULL(CAST(LNFILE.LN_PROMDATE AS VARCHAR(10)),'0'),
                   LNFILE.LN_PROMAMT,
                   ISNULL(CAST(LNFILE.LN_PROMDATE2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_DATE_PAID_LAST
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_AMT_PAID_LAST,
                   LNFILE.LN_LASTCFORMNO,
                   ISNULL(CAST(LNFILE.LN_LASTCFORMDATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_DELCNT,
                   LNFILE.LN_DELCNT2,
                   LNFILE.LN_CREDITLIM,
                   LNFILE.LN_CRSCORE,
                   LNFILE.LN_LASTSFORMNO,
                   ISNULL(CAST(LNFILE.LN_LASTSFORMDATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_SOLCNT,
                   ISNULL(LNFILE.LN_MESSAGE,''),
                   LNFILE.LN_OLDACCTNO,
                   LNFILE.LN_PRLNNO,
                   LNFILE.LN_PRBAL,
                   LNFILE.LN_PRINTDUE,
                   LNFILE.LN_PRFINCHGRB,
                   LNFILE.LN_PRINSRB,
                   ISNULL(LNFILE.LN_PRPOCD,''),
                   ISNULL(CAST(LNFILE.LN_PRPODATE AS VARCHAR(10)),'0'),
                   LNFILE.LN_PRDELINQAMT,
                   LNFILE.LN_PRLNAMT,
                   LNFILE.LN_PRPDUE_DAYS,
                   LNFILE.LN_PRDLDISC_UNEARN,
                   LNFILE.LN_PRCONPROFILE,
                   LNFILE.LN_SUM_ORIGPURAMT,
                   LNFILE.LN_SUM_LNAMT,
                   LNFILE.LN_SUM_FINCHG,
                   LNFILE.LN_SUM_INTCHG,
                   LNFILE.LN_SUM_ORGTERM,
                   LNFILE.LN_SUM_REGPYAMT,
                   LNFILE.LN_SUM_EXTCHG,
                   LNFILE.LN_SUM_SERCHG,
                   LNFILE.LN_SUM_MAINTFEE,
                   ISNULL(LNFILE.LN_UNITPER_CD,''),
                   LNFILE.LN_UNITPER_FREQ,
                   LNFILE.LN_DAYS_IN_UNITPER,
                   LNFILE.LN_UNITPER_PER_YEAR,
                   ISNULL(LNFILE.LN_REPOCD,''),
                   ISNULL(LNFILE.LN_BR_TFR_FG,''),
                   LNFILE.LN_O3INS_DLPROC,
                   LNFILE.LN_INSPREM_DLADJ,
                   LNFILE.LN_PLAMT,
                   LNFILE.LN_MTD_PRIN,
                   LNFILE.LN_MTD_LC,
                   LNFILE.LN_MTD_INT,
                   LNFILE.LN_MTD_OT,
                   LNFILE.LN_MTD_EX,
                   LNFILE.LN_BNKRPT_CHAPTER,
                   ISNULL(LNFILE.LN_BMO_CON_STATUS,''),
                   ISNULL(LNFILE.LN_BMO_REC_STATUS,''),
                   LNFILE.LN_APP_NUMBER,
                   ISNULL(LNFILE.LN_GIFTNET,''),
                   LNFILE.LN_GIFTNET_OUR_PO,
                   ISNULL(LNFILE.LN_PROC_DISBURSED,''),
                   ISNULL(LNFILE.LN_NOODDPY,''),
                   LNFILE.LN_CONPROFILE_1,
                   LNFILE.LN_CONPROFILE_2,
                   LNFILE.LN_CONPROFILE_3,
                   LNFILE.LN_CONPROFILE_4,
                   LNFILE.LN_CONPROFILE_5,
                   LNFILE.LN_RECPROFILE_1,
                   LNFILE.LN_RECPROFILE_2,
                   LNFILE.LN_RECPROFILE_3,
                   LNFILE.LN_RECPROFILE_4,
                   LNFILE.LN_RECPROFILE_5,
                   ISNULL(LNFILE.LN_TRW_STATUS,''),
                   ISNULL(LNFILE.LN_ORIGIN,''),
                   ISNULL(LNFILE.LN_PO_TRCD,''),
                   LNFILE.LN_NEXIS_CRSCORE,
                   ISNULL(LNFILE.LN_TRW_B_CONSUMER_IND,''),
                   ISNULL(LNFILE.LN_PARVHELD_PAID_FG,''),
                   ISNULL(LNFILE.LN_ACCT_FROZEN,''),
                   ISNULL(LNFILE.LN_TRW_SPECIAL_CODE,''),
                   ISNULL(LNFILE.LN_CREATION_STATUS,''),
                   LNFILE.LN_TOTNOLCHG,
                   ISNULL(LNFILE.LN_LBOX,''),
                   LNFILE.LN_EXTRA_PRIN_LFP,
                   LNFILE.LN_REMAIN_INT_SINCE_LFP,
                   LNFILE.LN_REMAIN_PRIN_SINCE_LFP,
                   ISNULL(LNFILE.LN_INTERIM_COUPON,''),
                   LNFILE.LN_DLCOMM_O3_CMSI,
                   LNFILE.LN_DLCOMM_PP_CMSI,
                   ISNULL(LNFILE.LN_TRW_J_CONSUMER_IND,''),
                   ISNULL(LNFILE.LN_ACH,''),
                   ISNULL(LNFILE.LN_1099_C_FG,''),
                   ISNULL(LNFILE.LN_TRW_COMPLIANCE_CODE,''),
                   ISNULL(LNFILE.LN_UNWIND_REASON_CODE,''),
                   ISNULL(LNFILE.LN_PBI_PBD_FG,''),
                   ISNULL(LNFILE.LN_OLD_ACCT_FORMAT_FG,''),
                   ISNULL(LNFILE.LN_APPROVED_BY_USER,''),
                   ISNULL(LNFILE.LN_APPROVED_BY_CODE,''),
                   ISNULL(CAST(LNFILE.LN_APPROVED_BY_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_APPROVED_BY_TIME,
                   ISNULL(CAST(LNFILE.LN_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_LTOUCH_TIME
            FROM DBO.LNFILE
            WHERE LNFILE.LN_OWNBR = :QLN1-WBEG-OWNBR
              AND LNFILE.LN_ACCTNO > :QLN1-WBEG-ACCTNO
              AND LNFILE.LN_ACCTNO <= :QLN1-WEND-ACCTNO 
            ORDER BY LNFILE.LN_OWNBR,
                     LNFILE.LN_ACCTNO
           END-EXEC.

           EXEC SQL
               OPEN LN1_CURSOR_GR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLN1-WBEG-OWNBR
                          QLN1-WEND-OWNBR
                          QLN1-WBEG-ACCTNO
                          QLN1-WEND-ACCTNO.

           MOVE STAT---GOOD TO LN1-CURSOR-GR-STAT.

      *-----------------------------------------------------------------
       READ-LN1-FILE.
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-SELECT-PATH TO E-FILE.
           MOVE LN1-KEY        TO E-KEYX.

           MOVE LN-OWNBR  TO QLN-OWNBR.
           MOVE LN-ACCTNO TO QLN-ACCTNO.

           EXEC SQL
            SELECT LNFILE.LN_OWNBR,
                   LNFILE.LN_ACCTNO,
                   LNFILE.LN_CLASS,
                   LNFILE.LN_SUBCLASS,
                   LNFILE.LN_LAWCODE,
                   ISNULL(LNFILE.LN_ORGST,''),
                   LNFILE.LN_SPRCLASS,
                   ISNULL(LNFILE.LN_STATFLAGS,''),
                   ISNULL(CAST(LNFILE.LN_ORIG_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LNFILE.LN_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   LNFILE.LN_ORGTERM,
                   LNFILE.LN_TOTNODEF,
                   LNFILE.LN_SUM_ORGTERM,
                   ISNULL(LNFILE.LN_UNITPER_CD,''),
                   LNFILE.LN_UNITPER_FREQ
            INTO  
                  :QLN-OWNBR,
                  :QLN-ACCTNO,
                  :QLN-CLASS,
                  :QLN-SUBCLASS,
                  :QLN-LAWCODE,
                  :QLN-ORGST,
                  :QLN-SPRCLASS,
                  :QLN-STATFLAGS,
                  :QLN-ORIG-1STPYDATE,
                  :QLN-1STPYDATE,
                  :QLN-ORGTERM,
                  :QLN-TOTNODEF,
                  :QLN-SUM-ORGTERM,
                  :QLN-UNITPER-CD,
                  :QLN-UNITPER-FREQ
            FROM DBO.LNFILE
            WHERE LNFILE.LN_OWNBR  = :QLN-OWNBR
              AND LNFILE.LN_ACCTNO = :QLN-ACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS.

      *-----------------------------------------------------------------
       READ-LN1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-SELECT-PATH TO E-FILE.
           MOVE LN1-KEY        TO E-KEYX.
           INITIALIZE QLN-REC.

           IF ( LN1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LN1_CURSOR INTO :QLN-REC
              END-EXEC
           ELSE
           IF ( LN1-CURSOR-GR-STAT-GOOD )
              EXEC SQL
                   FETCH LN1_CURSOR_GR INTO :QLN-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LN1-FILE.

           PERFORM CLOSE-IT.
           IF ( LN1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LN1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LN1-CURSOR-STAT.

           IF ( LN1-CURSOR-GR-STAT-GOOD )
              EXEC SQL
                   CLOSE LN1_CURSOR_GR
              END-EXEC
              MOVE STAT---BAD TO LN1-CURSOR-GR-STAT.
	  
      * COPY "LIBLP/LPSP1RN.CPY".
      * COPYMEMBER: LIBLP/LPSP1RN
      ******************************************************************
       SET-ALL-TO-SP1.
           MOVE SPA1-KEY   TO SP1-KEY.
           MOVE SPA-AREA-A TO SP-AREA-A.
           MOVE SPB-AREA-B TO SP-AREA-B.
           MOVE SPC-AREA-C TO SP-AREA-C.

       SET-SP1-TO-ALL.
           MOVE SP1-KEY   TO SPA1-KEY.
           MOVE SP-AREA-A TO SPA-AREA-A.
           MOVE SP1-KEY   TO SPB1-KEY.
           MOVE SP-AREA-B TO SPB-AREA-B.
           MOVE SP1-KEY   TO SPC1-KEY.
           MOVE SP-AREA-C TO SPC-AREA-C.

       LOAD-SP1-FILE.
           PERFORM LOAD-SPA1-FILE.
           PERFORM LOAD-SPB1-FILE.
           PERFORM LOAD-SPC1-FILE.

      *-----------------------------------------------------------------
       OPEN-SP1-FILE.
           PERFORM OPEN-SPA1-FILE.
           IF ( IO-FG = 0 )
               MOVE " " TO IO-MULTI-FG
           ELSE
               MOVE "A" TO IO-MULTI-FG.
			   
      *         PERFORM OPEN-SPB1-FILE
      *         IF ( IO-FG = 0 )
      *            PERFORM OPEN-SPC1-FILE
      *            IF ( IO-FG = 0 )
      *               MOVE " " TO IO-MULTI-FG
      *            ELSE
      *               MOVE "C" TO IO-MULTI-FG
      *         ELSE
      *            MOVE "B" TO IO-MULTI-FG
      *      ELSE
      *         MOVE "A" TO IO-MULTI-FG.

      *-----------------------------------------------------------------
       START-SP1-FILE.
           PERFORM SET-SP1-TO-ALL.
           PERFORM START-SPA1-FILE.
           IF ( IO-FG NOT = 0 )
              MOVE "A" TO IO-MULTI-FG.
             
      *-----------------------------------------------------------------
       READ-SP1-FILE.
           PERFORM SET-SP1-TO-ALL.
           PERFORM READ-SPA1-FILE.
           IF ( IO-FG = 0 )
               PERFORM SET-ALL-TO-SP1
           ELSE
               MOVE "A" TO IO-MULTI-FG.
		   
	
      *        PERFORM READ-SPB1-FILE
      *        IF ( IO-FG = 0 )
      *           PERFORM READ-SPC1-FILE
      *           IF ( IO-FG = 0 )
      *              PERFORM SET-ALL-TO-SP1
      *           ELSE
      *              MOVE "C" TO IO-MULTI-FG
      *        ELSE
      *           MOVE "B" TO IO-MULTI-FG
      *     ELSE
      *        MOVE "A" TO IO-MULTI-FG.

      *-----------------------------------------------------------------
       READ-SP1-FILE-NEXT.
           PERFORM SET-SP1-TO-ALL.
           PERFORM READ-SPA1-FILE-NEXT.
           IF ( IO-FG = 0 )
              MOVE SPA1-KEY TO SPB1-KEY
              PERFORM READ-SPB1-FILE
              IF ( IO-FG = 0 )
                 MOVE SPA1-KEY TO SPC1-KEY
                 PERFORM READ-SPC1-FILE
                 IF ( IO-FG = 0 )
                    PERFORM SET-ALL-TO-SP1
                 ELSE
                    MOVE "C" TO IO-MULTI-FG
              ELSE
                 MOVE "B" TO IO-MULTI-FG
           ELSE
              MOVE "A" TO IO-MULTI-FG.

      *-----------------------------------------------------------------
       CLOSE-SP1-FILE.
           PERFORM CLOSE-SPA1-FILE.
      *     PERFORM CLOSE-SPB1-FILE.
      *     PERFORM CLOSE-SPC1-FILE.

      *================================================================E
       LOAD-SPA1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( SP-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( SP-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO SPA-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO SPA-ALL-MACHINE
              MOVE SPA-ALL-PATH             TO SPA-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( SP-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO SPA-ALL-BASE
              MOVE FILPATH-MACHINE         TO SPA-ALL-MACHINE
              MOVE SPA-ALL-PATH             TO SPA-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-SPAFILE NOT = "S" )
              MOVE SPA-PATH-BEG          TO SPA-SELECT-PATH-BEG
              MOVE SPA-PATH-FILE         TO SPA-SELECT-PATH-FILE
           ELSE
              MOVE SPA-PATH-SQL          TO SPA-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-SPA1-FILE.
           PERFORM LOAD-SPA1-FILE.
           PERFORM OPEN-IT.
           MOVE SPA-SELECT-PATH TO E-FILE.
      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-SPA1-FILE.
           PERFORM START-IT.
           MOVE SPA-SELECT-PATH TO E-FILE.
           MOVE SPA1-KEY TO E-KEYX.

           IF ( SPA1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-SPA1-FILE.

           IF ( QSP1-WEND-LAWCODE = 0  ) OR
              ( QSP1-WEND-LAWCODE NOT NUMERIC)
              MOVE SPA-ORGST            TO QSP1-WBEG-ORGST 
              MOVE ALL "Z"              TO QSP1-WEND-ORGST 
              MOVE SPA-SPRCLASS         TO QSP1-WBEG-SPRCLASS
              MOVE ALL "9"              TO QSP1-WEND-SPRCLASS
              MOVE SPA-SUBCLASS         TO QSP1-WBEG-SUBCLASS
              MOVE ALL "9"              TO QSP1-WEND-SUBCLASS
              MOVE SPA-LAWCODE          TO QSP1-WBEG-LAWCODE
              MOVE ALL "9"              TO QSP1-WEND-LAWCODE.

           EXEC SQL
            DECLARE SPA1_CURSOR CURSOR FOR
            SELECT ISNULL(SPAFILE.SPA_ORGST,''),
                   SPAFILE.SPA_SPRCLASS,
                   SPAFILE.SPA_SUBCLASS,
                   SPAFILE.SPA_LAWCODE,
                   ISNULL(CAST(SPAFILE.SPA_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(SPAFILE.SPA_DESC,''),
                   ISNULL(SPAFILE.SPA_NOTES,''),
                   ISNULL(CAST(SPAFILE.SPA_EFFDATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(SPAFILE.SPA_EXPDATE
                                                  AS VARCHAR(10)),'0'),
                   SPAFILE.SPA_EXPD_MONTHS,
                   SPAFILE.SPA_EXPD_DAYS,
                   ISNULL(SPAFILE.SPA_EXPD_FRMLA,''),
                   ISNULL(SPAFILE.SPA_EXPD_EXCEPT,''),
                   ISNULL(SPAFILE.SPA_EXPR_FRMLA,''),
                   ISNULL(SPAFILE.SPA_EXPR_REDU_FRMLA,''),
                   SPAFILE.SPA_EXPR_RATE_1,
                   SPAFILE.SPA_EXPR_BREAK_1,
                   SPAFILE.SPA_EXPR_YRTYPE_1,
                   ISNULL(SPAFILE.SPA_EXPR_INTMETH_1,''),
                   SPAFILE.SPA_EXPR_RATE_2,
                   SPAFILE.SPA_EXPR_BREAK_2,
                   SPAFILE.SPA_EXPR_YRTYPE_2,
                   ISNULL(SPAFILE.SPA_EXPR_INTMETH_2,''),
                   SPAFILE.SPA_EXPR_RATE_3,
                   SPAFILE.SPA_EXPR_BREAK_3,
                   SPAFILE.SPA_EXPR_YRTYPE_3,
                   ISNULL(SPAFILE.SPA_EXPR_INTMETH_3,''),
                   SPAFILE.SPA_EXPR_RATE_4,
                   SPAFILE.SPA_EXPR_BREAK_4,
                   SPAFILE.SPA_EXPR_YRTYPE_4,
                   ISNULL(SPAFILE.SPA_EXPR_INTMETH_4,''),
                   SPAFILE.SPA_EXPR_RATE_5,
                   SPAFILE.SPA_EXPR_BREAK_5,
                   SPAFILE.SPA_EXPR_YRTYPE_5,
                   ISNULL(SPAFILE.SPA_EXPR_INTMETH_5,''),
                   ISNULL(SPAFILE.SPA_EXPR_LCHG,''),
                   ISNULL(SPAFILE.SPA_JUDGFRMLA,''),
                   SPAFILE.SPA_JUDGRATE,
                   SPAFILE.SPA_JUDGYRTYPE,
                   ISNULL(SPAFILE.SPA_JUDGINTMETH,''),
                   ISNULL(SPAFILE.SPA_ACC_FRMLA,''),
                   SPAFILE.SPA_ACC_YRTYPE,
                   ISNULL(SPAFILE.SPA_ACC_INTMETH,''),
                   SPAFILE.SPA_ACC_RATE_1,
                   SPAFILE.SPA_ACC_BREAK_1,
                   SPAFILE.SPA_ACC_RATE_2,
                   SPAFILE.SPA_ACC_BREAK_2,
                   SPAFILE.SPA_ACC_RATE_3,
                   SPAFILE.SPA_ACC_BREAK_3,
                   SPAFILE.SPA_ACC_RATE_4,
                   SPAFILE.SPA_ACC_BREAK_4,
                   SPAFILE.SPA_ACC_RATE_5,
                   SPAFILE.SPA_ACC_BREAK_5,
                   ISNULL(SPAFILE.SPA_1098FRMLA,''),
                   ISNULL(SPAFILE.SPA_DISFRMLA,''),
                   ISNULL(SPAFILE.SPA_CONTRFRMLA,''),
                   ISNULL(SPAFILE.SPA_BANKRULE,''),
                   SPAFILE.SPA_CONTPDTH,
                   SPAFILE.SPA_FINCHG,
                   ISNULL(SPAFILE.SPA_BANKUNITPER_CD,''),
                   ISNULL(SPAFILE.SPA_TRWBANKRULE,''),
                   SPAFILE.SPA_TRWCONTPDTH,
                   ISNULL(SPAFILE.SPA_TRWBANKUNITPER_CD,''),
                   ISNULL(SPAFILE.SPA_RECFRMLA,''),
                   ISNULL(SPAFILE.SPA_RECPAYCD,''),
                   SPAFILE.SPA_RECPDTH,
                   ISNULL(SPAFILE.SPA_DISFRMLA_SPAY_FG,''),
                   SPAFILE.SPA_ERNFRMLA_1,
                   SPAFILE.SPA_ERNAMT_1,
                   SPAFILE.SPA_ERNPERCT_1,
                   ISNULL(SPAFILE.SPA_ERNPERCD_1,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_1,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_1,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_1,''),
                   SPAFILE.SPA_ERN_DAYS_1,
                   SPAFILE.SPA_ERN_TERM_1,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_1,''),
                   SPAFILE.SPA_ERNFRMLA_2,
                   SPAFILE.SPA_ERNAMT_2,
                   SPAFILE.SPA_ERNPERCT_2,
                   ISNULL(SPAFILE.SPA_ERNPERCD_2,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_2,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_2,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_2,''),
                   SPAFILE.SPA_ERN_DAYS_2,
                   SPAFILE.SPA_ERN_TERM_2,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_2,''),
                   SPAFILE.SPA_ERNFRMLA_3,
                   SPAFILE.SPA_ERNAMT_3,
                   SPAFILE.SPA_ERNPERCT_3,
                   ISNULL(SPAFILE.SPA_ERNPERCD_3,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_3,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_3,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_3,''),
                   SPAFILE.SPA_ERN_DAYS_3,
                   SPAFILE.SPA_ERN_TERM_3,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_3,''),
                   SPAFILE.SPA_ERNFRMLA_4,
                   SPAFILE.SPA_ERNAMT_4,
                   SPAFILE.SPA_ERNPERCT_4,
                   ISNULL(SPAFILE.SPA_ERNPERCD_4,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_4,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_4,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_4,''),
                   SPAFILE.SPA_ERN_DAYS_4,
                   SPAFILE.SPA_ERN_TERM_4,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_4,''),
                   SPAFILE.SPA_ERNFRMLA_5,
                   SPAFILE.SPA_ERNAMT_5,
                   SPAFILE.SPA_ERNPERCT_5,
                   ISNULL(SPAFILE.SPA_ERNPERCD_5,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_5,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_5,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_5,''),
                   SPAFILE.SPA_ERN_DAYS_5,
                   SPAFILE.SPA_ERN_TERM_5,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_5,''),
                   SPAFILE.SPA_ERNFRMLA_6,
                   SPAFILE.SPA_ERNAMT_6,
                   SPAFILE.SPA_ERNPERCT_6,
                   ISNULL(SPAFILE.SPA_ERNPERCD_6,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_6,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_6,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_6,''),
                   SPAFILE.SPA_ERN_DAYS_6,
                   SPAFILE.SPA_ERN_TERM_6,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_6,''),
                   SPAFILE.SPA_ERNFRMLA_7,
                   SPAFILE.SPA_ERNAMT_7,
                   SPAFILE.SPA_ERNPERCT_7,
                   ISNULL(SPAFILE.SPA_ERNPERCD_7,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_7,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_7,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_7,''),
                   SPAFILE.SPA_ERN_DAYS_7,
                   SPAFILE.SPA_ERN_TERM_7,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_7,''),
                   SPAFILE.SPA_ERNFRMLA_8,
                   SPAFILE.SPA_ERNAMT_8,
                   SPAFILE.SPA_ERNPERCT_8,
                   ISNULL(SPAFILE.SPA_ERNPERCD_8,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_8,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_8,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_8,''),
                   SPAFILE.SPA_ERN_DAYS_8,
                   SPAFILE.SPA_ERN_TERM_8,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_8,''),
                   SPAFILE.SPA_ERNFRMLA_9,
                   SPAFILE.SPA_ERNAMT_9,
                   SPAFILE.SPA_ERNPERCT_9,
                   ISNULL(SPAFILE.SPA_ERNPERCD_9,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_9,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_9,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_9,''),
                   SPAFILE.SPA_ERN_DAYS_9,
                   SPAFILE.SPA_ERN_TERM_9,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_9,''),
                   SPAFILE.SPA_ERNFRMLA_10,
                   SPAFILE.SPA_ERNAMT_10,
                   SPAFILE.SPA_ERNPERCT_10,
                   ISNULL(SPAFILE.SPA_ERNPERCD_10,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_FG_10,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_START_CD_10,''),
                   ISNULL(SPAFILE.SPA_ERNACCRL_DFCD_10,''),
                   SPAFILE.SPA_ERN_DAYS_10,
                   SPAFILE.SPA_ERN_TERM_10,
                   ISNULL(SPAFILE.SPA_ERN_TYPE_10,''),
                   ISNULL(SPAFILE.SPA_ERNACCEL_CD,''),
                   ISNULL(SPAFILE.SPA_ERNJD_CD,''),
                   ISNULL(SPAFILE.SPA_MIDSTRERNCD,''),
                   SPAFILE.SPA_EARN_ORDER_1,
                   SPAFILE.SPA_EARN_ORDER_2,
                   SPAFILE.SPA_EARN_ORDER_3,
                   SPAFILE.SPA_EARN_ORDER_4,
                   SPAFILE.SPA_EARN_ORDER_5,
                   SPAFILE.SPA_EARN_ORDER_6,
                   SPAFILE.SPA_EARN_ORDER_7,
                   SPAFILE.SPA_EARN_ORDER_8,
                   SPAFILE.SPA_EARN_ORDER_9,
                   SPAFILE.SPA_EARN_ORDER_10,
                   ISNULL(SPAFILE.SPA_ERNDELQFRMLA,''),
                   SPAFILE.SPA_ERNDELQ_START,
                   SPAFILE.SPA_ERNDELQ_STOP,
                   ISNULL(SPAFILE.SPA_1098_INC_LC,''),
                   SPAFILE.SPA_MAX_IB_ACR_DAYS,
                   SPAFILE.SPA_LCGRACE,
                   ISNULL(SPAFILE.SPA_LCGRACECD,''),
                   ISNULL(SPAFILE.SPA_LCTYPE,''),
                   SPAFILE.SPA_LCRATE,
                   ISNULL(SPAFILE.SPA_LCFRMLA,''),
                   SPAFILE.SPA_LCMINCHG,
                   SPAFILE.SPA_LCMAXAMT,
                   SPAFILE.SPA_LCDELFAC,
                   SPAFILE.SPA_LCYRTYPE,
                   SPAFILE.SPA_LC_CODE,
                   ISNULL(SPAFILE.SPA_LCUNITPER_CD,''),
                   SPAFILE.SPA_DEFFRMLA,
                   ISNULL(SPAFILE.SPA_DEFEXCEPT,''),
                   SPAFILE.SPA_DEFWORKER,
                   SPAFILE.SPA_DEFPERCD,
                   SPAFILE.SPA_DEFBASE,
                   ISNULL(SPAFILE.SPA_DEFPOLCD,''),
                   SPAFILE.SPA_DEFPOLMX_1,
                   SPAFILE.SPA_DEFPOLMX_2,
                   SPAFILE.SPA_DEFPOLMX_3,
                   SPAFILE.SPA_DEFMIN,
                   SPAFILE.SPA_DEFMAX,
                   ISNULL(SPAFILE.SPA_DEFUNITPER_CD,''),
                   SPAFILE.SPA_PRFORM_1,
                   ISNULL(SPAFILE.SPA_PRFLAG_1,''),
                   SPAFILE.SPA_PRFORM_2,
                   ISNULL(SPAFILE.SPA_PRFLAG_2,''),
                   SPAFILE.SPA_PRFORM_3,
                   ISNULL(SPAFILE.SPA_PRFLAG_3,''),
                   SPAFILE.SPA_PRFORM_4,
                   ISNULL(SPAFILE.SPA_PRFLAG_4,''),
                   SPAFILE.SPA_PRFORM_5,
                   ISNULL(SPAFILE.SPA_PRFLAG_5,''),
                   SPAFILE.SPA_PRFORM_6,
                   ISNULL(SPAFILE.SPA_PRFLAG_6,''),
                   SPAFILE.SPA_PRFORM_7,
                   ISNULL(SPAFILE.SPA_PRFLAG_7,''),
                   SPAFILE.SPA_PRFORM_8,
                   ISNULL(SPAFILE.SPA_PRFLAG_8,''),
                   SPAFILE.SPA_PRFORM_9,
                   ISNULL(SPAFILE.SPA_PRFLAG_9,''),
                   SPAFILE.SPA_PRFORM_10,
                   ISNULL(SPAFILE.SPA_PRFLAG_10,''),
                   SPAFILE.SPA_PRFORM_11,
                   ISNULL(SPAFILE.SPA_PRFLAG_11,''),
                   SPAFILE.SPA_PRFORM_12,
                   ISNULL(SPAFILE.SPA_PRFLAG_12,''),
                   SPAFILE.SPA_PRFORM_13,
                   ISNULL(SPAFILE.SPA_PRFLAG_13,''),
                   SPAFILE.SPA_PRFORM_14,
                   ISNULL(SPAFILE.SPA_PRFLAG_14,''),
                   SPAFILE.SPA_PRFORM_15,
                   ISNULL(SPAFILE.SPA_PRFLAG_15,''),
                   SPAFILE.SPA_PRFORM_16,
                   ISNULL(SPAFILE.SPA_PRFLAG_16,''),
                   SPAFILE.SPA_PRFORM_17,
                   ISNULL(SPAFILE.SPA_PRFLAG_17,''),
                   SPAFILE.SPA_PRFORM_18,
                   ISNULL(SPAFILE.SPA_PRFLAG_18,''),
                   SPAFILE.SPA_PRFORM_19,
                   ISNULL(SPAFILE.SPA_PRFLAG_19,''),
                   SPAFILE.SPA_PRFORM_20,
                   ISNULL(SPAFILE.SPA_PRFLAG_20,''),
                   ISNULL(SPAFILE.SPA_IMED_REC,''),
                   SPAFILE.SPA_SCFRMLA,
                   SPAFILE.SPA_SCRATE,
                   SPAFILE.SPA_SCMIN,
                   SPAFILE.SPA_SCMAX,
                   SPAFILE.SPA_MFFRMLA,
                   SPAFILE.SPA_MFRATE,
                   SPAFILE.SPA_MFMIN,
                   SPAFILE.SPA_MFMAX,
                   ISNULL(SPAFILE.SPA_INTMETHOD,''),
                   SPAFILE.SPA_YEARTYPE,
                   SPAFILE.SPA_PYEXFRMLA,
                   SPAFILE.SPA_PYEXSPCODE,
                   SPAFILE.SPA_PYEXYEARTP,
                   ISNULL(SPAFILE.SPA_PYEXFMDATE,''),
                   ISNULL(SPAFILE.SPA_PYEXBASECD,''),
                   SPAFILE.SPA_PYEXBASE,
                   SPAFILE.SPA_PYEXDAYS,
                   SPAFILE.SPA_CAL_SPFRMLA,
                   ISNULL(SPAFILE.SPA_CAL_RATETYPE_1,''),
                   ISNULL(SPAFILE.SPA_CAL_BREAKTYPE_1,''),
                   ISNULL(SPAFILE.SPA_CAL_CHGABLE_1,''),
                   SPAFILE.SPA_CAL_NORATES_1,
                   SPAFILE.SPA_CAL_TBFRMLA_1,
                   SPAFILE.SPA_CAL_RATE_1_1,
                   SPAFILE.SPA_CAL_BREAK_1_1,
                   SPAFILE.SPA_CAL_RATE_1_2,
                   SPAFILE.SPA_CAL_BREAK_1_2,
                   SPAFILE.SPA_CAL_RATE_1_3,
                   SPAFILE.SPA_CAL_BREAK_1_3,
                   SPAFILE.SPA_CAL_RATE_1_4,
                   SPAFILE.SPA_CAL_BREAK_1_4,
                   SPAFILE.SPA_CAL_RATE_1_5,
                   SPAFILE.SPA_CAL_BREAK_1_5,
                   ISNULL(SPAFILE.SPA_CAL_RATETYPE_2,''),
                   ISNULL(SPAFILE.SPA_CAL_BREAKTYPE_2,''),
                   ISNULL(SPAFILE.SPA_CAL_CHGABLE_2,''),
                   SPAFILE.SPA_CAL_NORATES_2,
                   SPAFILE.SPA_CAL_TBFRMLA_2,
                   SPAFILE.SPA_CAL_RATE_2_1,
                   SPAFILE.SPA_CAL_BREAK_2_1,
                   SPAFILE.SPA_CAL_RATE_2_2,
                   SPAFILE.SPA_CAL_BREAK_2_2,
                   SPAFILE.SPA_CAL_RATE_2_3,
                   SPAFILE.SPA_CAL_BREAK_2_3,
                   SPAFILE.SPA_CAL_RATE_2_4,
                   SPAFILE.SPA_CAL_BREAK_2_4,
                   SPAFILE.SPA_CAL_RATE_2_5,
                   SPAFILE.SPA_CAL_BREAK_2_5,
                   SPAFILE.SPA_MINSF_FRMLA,
                   SPAFILE.SPA_MINSF_CHG,
                   SPAFILE.SPA_MAXAPR_FRMLA,
                   SPAFILE.SPA_EDT_NOEDITS_1,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_1_1,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_1_1,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_1_1,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_1_1,''),
                   SPAFILE.SPA_EDT_RATE_1_1,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_1_1,''),
                   SPAFILE.SPA_EDT_AMT_1_1,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_1_1,''),
                   SPAFILE.SPA_EDT_TERM_1_1,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_1_2,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_1_2,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_1_2,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_1_2,''),
                   SPAFILE.SPA_EDT_RATE_1_2,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_1_2,''),
                   SPAFILE.SPA_EDT_AMT_1_2,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_1_2,''),
                   SPAFILE.SPA_EDT_TERM_1_2,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_1_3,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_1_3,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_1_3,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_1_3,''),
                   SPAFILE.SPA_EDT_RATE_1_3,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_1_3,''),
                   SPAFILE.SPA_EDT_AMT_1_3,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_1_3,''),
                   SPAFILE.SPA_EDT_TERM_1_3,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_1_4,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_1_4,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_1_4,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_1_4,''),
                   SPAFILE.SPA_EDT_RATE_1_4,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_1_4,''),
                   SPAFILE.SPA_EDT_AMT_1_4,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_1_4,''),
                   SPAFILE.SPA_EDT_TERM_1_4,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_1_5,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_1_5,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_1_5,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_1_5,''),
                   SPAFILE.SPA_EDT_RATE_1_5,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_1_5,''),
                   SPAFILE.SPA_EDT_AMT_1_5,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_1_5,''),
                   SPAFILE.SPA_EDT_TERM_1_5,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_1_6,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_1_6,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_1_6,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_1_6,''),
                   SPAFILE.SPA_EDT_RATE_1_6,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_1_6,''),
                   SPAFILE.SPA_EDT_AMT_1_6,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_1_6,''),
                   SPAFILE.SPA_EDT_TERM_1_6,
                   SPAFILE.SPA_EDT_NOEDITS_2,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_2_1,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_2_1,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_2_1,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_2_1,''),
                   SPAFILE.SPA_EDT_RATE_2_1,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_2_1,''),
                   SPAFILE.SPA_EDT_AMT_2_1,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_2_1,''),
                   SPAFILE.SPA_EDT_TERM_2_1,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_2_2,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_2_2,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_2_2,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_2_2,''),
                   SPAFILE.SPA_EDT_RATE_2_2,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_2_2,''),
                   SPAFILE.SPA_EDT_AMT_2_2,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_2_2,''),
                   SPAFILE.SPA_EDT_TERM_2_2,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_2_3,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_2_3,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_2_3,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_2_3,''),
                   SPAFILE.SPA_EDT_RATE_2_3,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_2_3,''),
                   SPAFILE.SPA_EDT_AMT_2_3,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_2_3,''),
                   SPAFILE.SPA_EDT_TERM_2_3,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_2_4,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_2_4,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_2_4,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_2_4,''),
                   SPAFILE.SPA_EDT_RATE_2_4,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_2_4,''),
                   SPAFILE.SPA_EDT_AMT_2_4,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_2_4,''),
                   SPAFILE.SPA_EDT_TERM_2_4,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_2_5,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_2_5,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_2_5,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_2_5,''),
                   SPAFILE.SPA_EDT_RATE_2_5,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_2_5,''),
                   SPAFILE.SPA_EDT_AMT_2_5,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_2_5,''),
                   SPAFILE.SPA_EDT_TERM_2_5,
                   ISNULL(SPAFILE.SPA_EDT_TYPE_2_6,''),
                   ISNULL(SPAFILE.SPA_EDT_HL_2_6,''),
                   ISNULL(SPAFILE.SPA_EDT_COLL_2_6,''),
                   ISNULL(SPAFILE.SPA_EDT_CD_2_6,''),
                   SPAFILE.SPA_EDT_RATE_2_6,
                   ISNULL(SPAFILE.SPA_EDT_RATETYPE_2_6,''),
                   SPAFILE.SPA_EDT_AMT_2_6,
                   ISNULL(SPAFILE.SPA_EDT_AMTYPE_2_6,''),
                   SPAFILE.SPA_EDT_TERM_2_6
            FROM DBO.SPAFILE
            WHERE SPAFILE.SPA_ORGST >= :QSP1-WBEG-ORGST
              AND SPAFILE.SPA_ORGST <= :QSP1-WEND-ORGST
              AND SPAFILE.SPA_SPRCLASS >= :QSP1-WBEG-SPRCLASS
              AND SPAFILE.SPA_SPRCLASS <= :QSP1-WEND-SPRCLASS
              AND SPAFILE.SPA_SUBCLASS >= :QSP1-WBEG-SUBCLASS
              AND SPAFILE.SPA_SUBCLASS <= :QSP1-WEND-SUBCLASS
              AND SPAFILE.SPA_LAWCODE >= :QSP1-WBEG-LAWCODE 
              AND SPAFILE.SPA_LAWCODE <= :QSP1-WEND-LAWCODE 
            ORDER BY SPAFILE.SPA_ORGST,
                     SPAFILE.SPA_SPRCLASS,
                     SPAFILE.SPA_SUBCLASS,
                     SPAFILE.SPA_LAWCODE
           END-EXEC.

           EXEC SQL
               OPEN SPA1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QSP1-WBEG-ORGST
                          QSP1-WEND-ORGST.

           MOVE ZEROES TO QSP1-WBEG-SPRCLASS
                          QSP1-WEND-SPRCLASS
                          QSP1-WBEG-SUBCLASS
                          QSP1-WEND-SUBCLASS
                          QSP1-WBEG-LAWCODE
                          QSP1-WEND-LAWCODE.

           MOVE STAT---GOOD TO SPA1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-SPA1-FILE.
           PERFORM READ-IT.
           MOVE SPA-SELECT-PATH TO E-FILE.
           MOVE SPA1-KEY TO E-KEYX.

           MOVE SPA-ORGST            TO QSPA-ORGST.
           MOVE SPA-SPRCLASS         TO QSPA-SPRCLASS.
           MOVE SPA-SUBCLASS         TO QSPA-SUBCLASS.
           MOVE SPA-LAWCODE          TO QSPA-LAWCODE.

           EXEC SQL
            SELECT ISNULL(SPAFILE.SPA_ORGST,''),
                   SPAFILE.SPA_SPRCLASS,
                   SPAFILE.SPA_SUBCLASS,
                   SPAFILE.SPA_LAWCODE,
                   ISNULL(SPAFILE.SPA_EXPD_FRMLA,'')
              INTO  
                   :QSPA-ORGST,
                   :QSPA-SPRCLASS,
                   :QSPA-SUBCLASS,
                   :QSPA-LAWCODE,
                   :QSPA-EXPD-FRMLA
              FROM DBO.SPAFILE
             WHERE SPAFILE.SPA_ORGST    = :QSPA-ORGST
               AND SPAFILE.SPA_SPRCLASS = :QSPA-SPRCLASS
               AND SPAFILE.SPA_SUBCLASS = :QSPA-SUBCLASS
               AND SPAFILE.SPA_LAWCODE  = :QSPA-LAWCODE 
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SPA1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-SPA-FIELDS.

      *-----------------------------------------------------------------
       READ-SPA1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE SPA-SELECT-PATH TO E-FILE.
           MOVE SPA1-KEY TO E-KEYX.
           INITIALIZE QSPA-REC.

           IF ( SPA1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH SPA1_CURSOR INTO :QSPA-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SPA1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-SPA-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-SPA1-FILE.
           PERFORM CLOSE-IT.
           IF ( SPA1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE SPA1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO SPA1-CURSOR-STAT.

      *=================================================================
       LOAD-SPB1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( SP-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( SP-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO SPB-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO SPB-ALL-MACHINE
              MOVE SPB-ALL-PATH             TO SPB-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( SP-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO SPB-ALL-BASE
              MOVE FILPATH-MACHINE         TO SPB-ALL-MACHINE
              MOVE SPB-ALL-PATH             TO SPB-PATH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-SPBFILE NOT = "S" )
              MOVE SPB-PATH-BEG          TO SPB-SELECT-PATH-BEG
              MOVE SPB-PATH-FILE         TO SPB-SELECT-PATH-FILE
           ELSE
              MOVE SPB-PATH-SQL          TO SPB-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-SPB1-FILE.
           PERFORM LOAD-SPB1-FILE.
           PERFORM OPEN-IT.
           MOVE SPB-SELECT-PATH TO E-FILE.

      *-----------------------------------------------------------------
       READ-SPB1-FILE.
           PERFORM READ-IT.
           MOVE SPB-SELECT-PATH TO E-FILE.
           MOVE SPB1-KEY TO E-KEYX.

           MOVE SPB-ORGST            TO QSPB-ORGST.
           MOVE SPB-SPRCLASS         TO QSPB-SPRCLASS.
           MOVE SPB-SUBCLASS         TO QSPB-SUBCLASS.
           MOVE SPB-LAWCODE          TO QSPB-LAWCODE.

           EXEC SQL
            SELECT ISNULL(SPBFILE.SPB_ORGST,''),
                   SPBFILE.SPB_SPRCLASS,
                   SPBFILE.SPB_SUBCLASS,
                   SPBFILE.SPB_LAWCODE,
                   SPBFILE.SPB_INSCOM_1,
                   SPBFILE.SPB_INSRATECD_1,
                   SPBFILE.SPB_INSJFAC_1,
                   SPBFILE.SPB_INSDISCNT_1,
                   ISNULL(SPBFILE.SPB_INSCHGABL_1,''),
                   SPBFILE.SPB_INSTERM_1_1,
                   SPBFILE.SPB_INSRATE_1_1,
                   SPBFILE.SPB_INSTERM_1_2,
                   SPBFILE.SPB_INSRATE_1_2,
                   SPBFILE.SPB_INSTERM_1_3,
                   SPBFILE.SPB_INSRATE_1_3,
                   SPBFILE.SPB_INSTERM_1_4,
                   SPBFILE.SPB_INSRATE_1_4,
                   SPBFILE.SPB_INSTERM_1_5,
                   SPBFILE.SPB_INSRATE_1_5,
                   SPBFILE.SPB_INSTERM_1_6,
                   SPBFILE.SPB_INSRATE_1_6,
                   SPBFILE.SPB_INSTERM_1_7,
                   SPBFILE.SPB_INSRATE_1_7,
                   SPBFILE.SPB_INSTERM_1_8,
                   SPBFILE.SPB_INSRATE_1_8,
                   SPBFILE.SPB_INSTERM_1_9,
                   SPBFILE.SPB_INSRATE_1_9,
                   SPBFILE.SPB_INSTERM_1_10,
                   SPBFILE.SPB_INSRATE_1_10,
                   SPBFILE.SPB_INSTERM_1_11,
                   SPBFILE.SPB_INSRATE_1_11,
                   SPBFILE.SPB_INSTERM_1_12,
                   SPBFILE.SPB_INSRATE_1_12,
                   SPBFILE.SPB_INSTERM_1_13,
                   SPBFILE.SPB_INSRATE_1_13,
                   SPBFILE.SPB_INSTERM_1_14,
                   SPBFILE.SPB_INSRATE_1_14,
                   SPBFILE.SPB_INSTERM_1_15,
                   SPBFILE.SPB_INSRATE_1_15,
                   SPBFILE.SPB_INSTERM_1_16,
                   SPBFILE.SPB_INSRATE_1_16,
                   SPBFILE.SPB_INSTERM_1_17,
                   SPBFILE.SPB_INSRATE_1_17,
                   SPBFILE.SPB_INSTERM_1_18,
                   SPBFILE.SPB_INSRATE_1_18,
                   SPBFILE.SPB_INSTERM_1_19,
                   SPBFILE.SPB_INSRATE_1_19,
                   SPBFILE.SPB_INSTERM_1_20,
                   SPBFILE.SPB_INSRATE_1_20,
                   SPBFILE.SPB_INSMINPREM_1,
                   SPBFILE.SPB_INSMINCHGABL_1,
                   SPBFILE.SPB_INSMAXCHGABL_1,
                   SPBFILE.SPB_INSMAXBENIF_1,
                   SPBFILE.SPB_INSMAXTERM_1,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_1,''),
                   SPBFILE.SPB_INSEXCEPT_1,
                   SPBFILE.SPB_INSNOVER_1,
                   SPBFILE.SPB_INSMINAGE_1_1,
                   SPBFILE.SPB_INSMAXAGE_1_1,
                   SPBFILE.SPB_INSMAXCOV_1_1,
                   SPBFILE.SPB_INSMAXTRM_1_1,
                   SPBFILE.SPB_INSMINAGE_1_2,
                   SPBFILE.SPB_INSMAXAGE_1_2,
                   SPBFILE.SPB_INSMAXCOV_1_2,
                   SPBFILE.SPB_INSMAXTRM_1_2,
                   SPBFILE.SPB_INSMINAGE_1_3,
                   SPBFILE.SPB_INSMAXAGE_1_3,
                   SPBFILE.SPB_INSMAXCOV_1_3,
                   SPBFILE.SPB_INSMAXTRM_1_3,
                   SPBFILE.SPB_INSMINAGE_1_4,
                   SPBFILE.SPB_INSMAXAGE_1_4,
                   SPBFILE.SPB_INSMAXCOV_1_4,
                   SPBFILE.SPB_INSMAXTRM_1_4,
                   SPBFILE.SPB_INSMINAGE_1_5,
                   SPBFILE.SPB_INSMAXAGE_1_5,
                   SPBFILE.SPB_INSMAXCOV_1_5,
                   SPBFILE.SPB_INSMAXTRM_1_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_1,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_1,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_1,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_1,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_1,''),
                   SPBFILE.SPB_INSCOM_2,
                   SPBFILE.SPB_INSRATECD_2,
                   SPBFILE.SPB_INSJFAC_2,
                   SPBFILE.SPB_INSDISCNT_2,
                   ISNULL(SPBFILE.SPB_INSCHGABL_2,''),
                   SPBFILE.SPB_INSTERM_2_1,
                   SPBFILE.SPB_INSRATE_2_1,
                   SPBFILE.SPB_INSTERM_2_2,
                   SPBFILE.SPB_INSRATE_2_2,
                   SPBFILE.SPB_INSTERM_2_3,
                   SPBFILE.SPB_INSRATE_2_3,
                   SPBFILE.SPB_INSTERM_2_4,
                   SPBFILE.SPB_INSRATE_2_4,
                   SPBFILE.SPB_INSTERM_2_5,
                   SPBFILE.SPB_INSRATE_2_5,
                   SPBFILE.SPB_INSTERM_2_6,
                   SPBFILE.SPB_INSRATE_2_6,
                   SPBFILE.SPB_INSTERM_2_7,
                   SPBFILE.SPB_INSRATE_2_7,
                   SPBFILE.SPB_INSTERM_2_8,
                   SPBFILE.SPB_INSRATE_2_8,
                   SPBFILE.SPB_INSTERM_2_9,
                   SPBFILE.SPB_INSRATE_2_9,
                   SPBFILE.SPB_INSTERM_2_10,
                   SPBFILE.SPB_INSRATE_2_10,
                   SPBFILE.SPB_INSTERM_2_11,
                   SPBFILE.SPB_INSRATE_2_11,
                   SPBFILE.SPB_INSTERM_2_12,
                   SPBFILE.SPB_INSRATE_2_12,
                   SPBFILE.SPB_INSTERM_2_13,
                   SPBFILE.SPB_INSRATE_2_13,
                   SPBFILE.SPB_INSTERM_2_14,
                   SPBFILE.SPB_INSRATE_2_14,
                   SPBFILE.SPB_INSTERM_2_15,
                   SPBFILE.SPB_INSRATE_2_15,
                   SPBFILE.SPB_INSTERM_2_16,
                   SPBFILE.SPB_INSRATE_2_16,
                   SPBFILE.SPB_INSTERM_2_17,
                   SPBFILE.SPB_INSRATE_2_17,
                   SPBFILE.SPB_INSTERM_2_18,
                   SPBFILE.SPB_INSRATE_2_18,
                   SPBFILE.SPB_INSTERM_2_19,
                   SPBFILE.SPB_INSRATE_2_19,
                   SPBFILE.SPB_INSTERM_2_20,
                   SPBFILE.SPB_INSRATE_2_20,
                   SPBFILE.SPB_INSMINPREM_2,
                   SPBFILE.SPB_INSMINCHGABL_2,
                   SPBFILE.SPB_INSMAXCHGABL_2,
                   SPBFILE.SPB_INSMAXBENIF_2,
                   SPBFILE.SPB_INSMAXTERM_2,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_2,''),
                   SPBFILE.SPB_INSEXCEPT_2,
                   SPBFILE.SPB_INSNOVER_2,
                   SPBFILE.SPB_INSMINAGE_2_1,
                   SPBFILE.SPB_INSMAXAGE_2_1,
                   SPBFILE.SPB_INSMAXCOV_2_1,
                   SPBFILE.SPB_INSMAXTRM_2_1,
                   SPBFILE.SPB_INSMINAGE_2_2,
                   SPBFILE.SPB_INSMAXAGE_2_2,
                   SPBFILE.SPB_INSMAXCOV_2_2,
                   SPBFILE.SPB_INSMAXTRM_2_2,
                   SPBFILE.SPB_INSMINAGE_2_3,
                   SPBFILE.SPB_INSMAXAGE_2_3,
                   SPBFILE.SPB_INSMAXCOV_2_3,
                   SPBFILE.SPB_INSMAXTRM_2_3,
                   SPBFILE.SPB_INSMINAGE_2_4,
                   SPBFILE.SPB_INSMAXAGE_2_4,
                   SPBFILE.SPB_INSMAXCOV_2_4,
                   SPBFILE.SPB_INSMAXTRM_2_4,
                   SPBFILE.SPB_INSMINAGE_2_5,
                   SPBFILE.SPB_INSMAXAGE_2_5,
                   SPBFILE.SPB_INSMAXCOV_2_5,
                   SPBFILE.SPB_INSMAXTRM_2_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_2,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_2,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_2,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_2,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_2,''),
                   SPBFILE.SPB_INSCOM_3,
                   SPBFILE.SPB_INSRATECD_3,
                   SPBFILE.SPB_INSJFAC_3,
                   SPBFILE.SPB_INSDISCNT_3,
                   ISNULL(SPBFILE.SPB_INSCHGABL_3,''),
                   SPBFILE.SPB_INSTERM_3_1,
                   SPBFILE.SPB_INSRATE_3_1,
                   SPBFILE.SPB_INSTERM_3_2,
                   SPBFILE.SPB_INSRATE_3_2,
                   SPBFILE.SPB_INSTERM_3_3,
                   SPBFILE.SPB_INSRATE_3_3,
                   SPBFILE.SPB_INSTERM_3_4,
                   SPBFILE.SPB_INSRATE_3_4,
                   SPBFILE.SPB_INSTERM_3_5,
                   SPBFILE.SPB_INSRATE_3_5,
                   SPBFILE.SPB_INSTERM_3_6,
                   SPBFILE.SPB_INSRATE_3_6,
                   SPBFILE.SPB_INSTERM_3_7,
                   SPBFILE.SPB_INSRATE_3_7,
                   SPBFILE.SPB_INSTERM_3_8,
                   SPBFILE.SPB_INSRATE_3_8,
                   SPBFILE.SPB_INSTERM_3_9,
                   SPBFILE.SPB_INSRATE_3_9,
                   SPBFILE.SPB_INSTERM_3_10,
                   SPBFILE.SPB_INSRATE_3_10,
                   SPBFILE.SPB_INSTERM_3_11,
                   SPBFILE.SPB_INSRATE_3_11,
                   SPBFILE.SPB_INSTERM_3_12,
                   SPBFILE.SPB_INSRATE_3_12,
                   SPBFILE.SPB_INSTERM_3_13,
                   SPBFILE.SPB_INSRATE_3_13,
                   SPBFILE.SPB_INSTERM_3_14,
                   SPBFILE.SPB_INSRATE_3_14,
                   SPBFILE.SPB_INSTERM_3_15,
                   SPBFILE.SPB_INSRATE_3_15,
                   SPBFILE.SPB_INSTERM_3_16,
                   SPBFILE.SPB_INSRATE_3_16,
                   SPBFILE.SPB_INSTERM_3_17,
                   SPBFILE.SPB_INSRATE_3_17,
                   SPBFILE.SPB_INSTERM_3_18,
                   SPBFILE.SPB_INSRATE_3_18,
                   SPBFILE.SPB_INSTERM_3_19,
                   SPBFILE.SPB_INSRATE_3_19,
                   SPBFILE.SPB_INSTERM_3_20,
                   SPBFILE.SPB_INSRATE_3_20,
                   SPBFILE.SPB_INSMINPREM_3,
                   SPBFILE.SPB_INSMINCHGABL_3,
                   SPBFILE.SPB_INSMAXCHGABL_3,
                   SPBFILE.SPB_INSMAXBENIF_3,
                   SPBFILE.SPB_INSMAXTERM_3,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_3,''),
                   SPBFILE.SPB_INSEXCEPT_3,
                   SPBFILE.SPB_INSNOVER_3,
                   SPBFILE.SPB_INSMINAGE_3_1,
                   SPBFILE.SPB_INSMAXAGE_3_1,
                   SPBFILE.SPB_INSMAXCOV_3_1,
                   SPBFILE.SPB_INSMAXTRM_3_1,
                   SPBFILE.SPB_INSMINAGE_3_2,
                   SPBFILE.SPB_INSMAXAGE_3_2,
                   SPBFILE.SPB_INSMAXCOV_3_2,
                   SPBFILE.SPB_INSMAXTRM_3_2,
                   SPBFILE.SPB_INSMINAGE_3_3,
                   SPBFILE.SPB_INSMAXAGE_3_3,
                   SPBFILE.SPB_INSMAXCOV_3_3,
                   SPBFILE.SPB_INSMAXTRM_3_3,
                   SPBFILE.SPB_INSMINAGE_3_4,
                   SPBFILE.SPB_INSMAXAGE_3_4,
                   SPBFILE.SPB_INSMAXCOV_3_4,
                   SPBFILE.SPB_INSMAXTRM_3_4,
                   SPBFILE.SPB_INSMINAGE_3_5,
                   SPBFILE.SPB_INSMAXAGE_3_5,
                   SPBFILE.SPB_INSMAXCOV_3_5,
                   SPBFILE.SPB_INSMAXTRM_3_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_3,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_3,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_3,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_3,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_3,''),
                   SPBFILE.SPB_INSCOM_4,
                   SPBFILE.SPB_INSRATECD_4,
                   SPBFILE.SPB_INSJFAC_4,
                   SPBFILE.SPB_INSDISCNT_4,
                   ISNULL(SPBFILE.SPB_INSCHGABL_4,''),
                   SPBFILE.SPB_INSTERM_4_1,
                   SPBFILE.SPB_INSRATE_4_1,
                   SPBFILE.SPB_INSTERM_4_2,
                   SPBFILE.SPB_INSRATE_4_2,
                   SPBFILE.SPB_INSTERM_4_3,
                   SPBFILE.SPB_INSRATE_4_3,
                   SPBFILE.SPB_INSTERM_4_4,
                   SPBFILE.SPB_INSRATE_4_4,
                   SPBFILE.SPB_INSTERM_4_5,
                   SPBFILE.SPB_INSRATE_4_5,
                   SPBFILE.SPB_INSTERM_4_6,
                   SPBFILE.SPB_INSRATE_4_6,
                   SPBFILE.SPB_INSTERM_4_7,
                   SPBFILE.SPB_INSRATE_4_7,
                   SPBFILE.SPB_INSTERM_4_8,
                   SPBFILE.SPB_INSRATE_4_8,
                   SPBFILE.SPB_INSTERM_4_9,
                   SPBFILE.SPB_INSRATE_4_9,
                   SPBFILE.SPB_INSTERM_4_10,
                   SPBFILE.SPB_INSRATE_4_10,
                   SPBFILE.SPB_INSTERM_4_11,
                   SPBFILE.SPB_INSRATE_4_11,
                   SPBFILE.SPB_INSTERM_4_12,
                   SPBFILE.SPB_INSRATE_4_12,
                   SPBFILE.SPB_INSTERM_4_13,
                   SPBFILE.SPB_INSRATE_4_13,
                   SPBFILE.SPB_INSTERM_4_14,
                   SPBFILE.SPB_INSRATE_4_14,
                   SPBFILE.SPB_INSTERM_4_15,
                   SPBFILE.SPB_INSRATE_4_15,
                   SPBFILE.SPB_INSTERM_4_16,
                   SPBFILE.SPB_INSRATE_4_16,
                   SPBFILE.SPB_INSTERM_4_17,
                   SPBFILE.SPB_INSRATE_4_17,
                   SPBFILE.SPB_INSTERM_4_18,
                   SPBFILE.SPB_INSRATE_4_18,
                   SPBFILE.SPB_INSTERM_4_19,
                   SPBFILE.SPB_INSRATE_4_19,
                   SPBFILE.SPB_INSTERM_4_20,
                   SPBFILE.SPB_INSRATE_4_20,
                   SPBFILE.SPB_INSMINPREM_4,
                   SPBFILE.SPB_INSMINCHGABL_4,
                   SPBFILE.SPB_INSMAXCHGABL_4,
                   SPBFILE.SPB_INSMAXBENIF_4,
                   SPBFILE.SPB_INSMAXTERM_4,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_4,''),
                   SPBFILE.SPB_INSEXCEPT_4,
                   SPBFILE.SPB_INSNOVER_4,
                   SPBFILE.SPB_INSMINAGE_4_1,
                   SPBFILE.SPB_INSMAXAGE_4_1,
                   SPBFILE.SPB_INSMAXCOV_4_1,
                   SPBFILE.SPB_INSMAXTRM_4_1,
                   SPBFILE.SPB_INSMINAGE_4_2,
                   SPBFILE.SPB_INSMAXAGE_4_2,
                   SPBFILE.SPB_INSMAXCOV_4_2,
                   SPBFILE.SPB_INSMAXTRM_4_2,
                   SPBFILE.SPB_INSMINAGE_4_3,
                   SPBFILE.SPB_INSMAXAGE_4_3,
                   SPBFILE.SPB_INSMAXCOV_4_3,
                   SPBFILE.SPB_INSMAXTRM_4_3,
                   SPBFILE.SPB_INSMINAGE_4_4,
                   SPBFILE.SPB_INSMAXAGE_4_4,
                   SPBFILE.SPB_INSMAXCOV_4_4,
                   SPBFILE.SPB_INSMAXTRM_4_4,
                   SPBFILE.SPB_INSMINAGE_4_5,
                   SPBFILE.SPB_INSMAXAGE_4_5,
                   SPBFILE.SPB_INSMAXCOV_4_5,
                   SPBFILE.SPB_INSMAXTRM_4_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_4,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_4,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_4,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_4,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_4,''),
                   SPBFILE.SPB_INSCOM_5,
                   SPBFILE.SPB_INSRATECD_5,
                   SPBFILE.SPB_INSJFAC_5,
                   SPBFILE.SPB_INSDISCNT_5,
                   ISNULL(SPBFILE.SPB_INSCHGABL_5,''),
                   SPBFILE.SPB_INSTERM_5_1,
                   SPBFILE.SPB_INSRATE_5_1,
                   SPBFILE.SPB_INSTERM_5_2,
                   SPBFILE.SPB_INSRATE_5_2,
                   SPBFILE.SPB_INSTERM_5_3,
                   SPBFILE.SPB_INSRATE_5_3,
                   SPBFILE.SPB_INSTERM_5_4,
                   SPBFILE.SPB_INSRATE_5_4,
                   SPBFILE.SPB_INSTERM_5_5,
                   SPBFILE.SPB_INSRATE_5_5,
                   SPBFILE.SPB_INSTERM_5_6,
                   SPBFILE.SPB_INSRATE_5_6,
                   SPBFILE.SPB_INSTERM_5_7,
                   SPBFILE.SPB_INSRATE_5_7,
                   SPBFILE.SPB_INSTERM_5_8,
                   SPBFILE.SPB_INSRATE_5_8,
                   SPBFILE.SPB_INSTERM_5_9,
                   SPBFILE.SPB_INSRATE_5_9,
                   SPBFILE.SPB_INSTERM_5_10,
                   SPBFILE.SPB_INSRATE_5_10,
                   SPBFILE.SPB_INSTERM_5_11,
                   SPBFILE.SPB_INSRATE_5_11,
                   SPBFILE.SPB_INSTERM_5_12,
                   SPBFILE.SPB_INSRATE_5_12,
                   SPBFILE.SPB_INSTERM_5_13,
                   SPBFILE.SPB_INSRATE_5_13,
                   SPBFILE.SPB_INSTERM_5_14,
                   SPBFILE.SPB_INSRATE_5_14,
                   SPBFILE.SPB_INSTERM_5_15,
                   SPBFILE.SPB_INSRATE_5_15,
                   SPBFILE.SPB_INSTERM_5_16,
                   SPBFILE.SPB_INSRATE_5_16,
                   SPBFILE.SPB_INSTERM_5_17,
                   SPBFILE.SPB_INSRATE_5_17,
                   SPBFILE.SPB_INSTERM_5_18,
                   SPBFILE.SPB_INSRATE_5_18,
                   SPBFILE.SPB_INSTERM_5_19,
                   SPBFILE.SPB_INSRATE_5_19,
                   SPBFILE.SPB_INSTERM_5_20,
                   SPBFILE.SPB_INSRATE_5_20,
                   SPBFILE.SPB_INSMINPREM_5,
                   SPBFILE.SPB_INSMINCHGABL_5,
                   SPBFILE.SPB_INSMAXCHGABL_5,
                   SPBFILE.SPB_INSMAXBENIF_5,
                   SPBFILE.SPB_INSMAXTERM_5,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_5,''),
                   SPBFILE.SPB_INSEXCEPT_5,
                   SPBFILE.SPB_INSNOVER_5,
                   SPBFILE.SPB_INSMINAGE_5_1,
                   SPBFILE.SPB_INSMAXAGE_5_1,
                   SPBFILE.SPB_INSMAXCOV_5_1,
                   SPBFILE.SPB_INSMAXTRM_5_1,
                   SPBFILE.SPB_INSMINAGE_5_2,
                   SPBFILE.SPB_INSMAXAGE_5_2,
                   SPBFILE.SPB_INSMAXCOV_5_2,
                   SPBFILE.SPB_INSMAXTRM_5_2,
                   SPBFILE.SPB_INSMINAGE_5_3,
                   SPBFILE.SPB_INSMAXAGE_5_3,
                   SPBFILE.SPB_INSMAXCOV_5_3,
                   SPBFILE.SPB_INSMAXTRM_5_3,
                   SPBFILE.SPB_INSMINAGE_5_4,
                   SPBFILE.SPB_INSMAXAGE_5_4,
                   SPBFILE.SPB_INSMAXCOV_5_4,
                   SPBFILE.SPB_INSMAXTRM_5_4,
                   SPBFILE.SPB_INSMINAGE_5_5,
                   SPBFILE.SPB_INSMAXAGE_5_5,
                   SPBFILE.SPB_INSMAXCOV_5_5,
                   SPBFILE.SPB_INSMAXTRM_5_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_5,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_5,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_5,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_5,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_5,''),
                   SPBFILE.SPB_INSCOM_6,
                   SPBFILE.SPB_INSRATECD_6,
                   SPBFILE.SPB_INSJFAC_6,
                   SPBFILE.SPB_INSDISCNT_6,
                   ISNULL(SPBFILE.SPB_INSCHGABL_6,''),
                   SPBFILE.SPB_INSTERM_6_1,
                   SPBFILE.SPB_INSRATE_6_1,
                   SPBFILE.SPB_INSTERM_6_2,
                   SPBFILE.SPB_INSRATE_6_2,
                   SPBFILE.SPB_INSTERM_6_3,
                   SPBFILE.SPB_INSRATE_6_3,
                   SPBFILE.SPB_INSTERM_6_4,
                   SPBFILE.SPB_INSRATE_6_4,
                   SPBFILE.SPB_INSTERM_6_5,
                   SPBFILE.SPB_INSRATE_6_5,
                   SPBFILE.SPB_INSTERM_6_6,
                   SPBFILE.SPB_INSRATE_6_6,
                   SPBFILE.SPB_INSTERM_6_7,
                   SPBFILE.SPB_INSRATE_6_7,
                   SPBFILE.SPB_INSTERM_6_8,
                   SPBFILE.SPB_INSRATE_6_8,
                   SPBFILE.SPB_INSTERM_6_9,
                   SPBFILE.SPB_INSRATE_6_9,
                   SPBFILE.SPB_INSTERM_6_10,
                   SPBFILE.SPB_INSRATE_6_10,
                   SPBFILE.SPB_INSTERM_6_11,
                   SPBFILE.SPB_INSRATE_6_11,
                   SPBFILE.SPB_INSTERM_6_12,
                   SPBFILE.SPB_INSRATE_6_12,
                   SPBFILE.SPB_INSTERM_6_13,
                   SPBFILE.SPB_INSRATE_6_13,
                   SPBFILE.SPB_INSTERM_6_14,
                   SPBFILE.SPB_INSRATE_6_14,
                   SPBFILE.SPB_INSTERM_6_15,
                   SPBFILE.SPB_INSRATE_6_15,
                   SPBFILE.SPB_INSTERM_6_16,
                   SPBFILE.SPB_INSRATE_6_16,
                   SPBFILE.SPB_INSTERM_6_17,
                   SPBFILE.SPB_INSRATE_6_17,
                   SPBFILE.SPB_INSTERM_6_18,
                   SPBFILE.SPB_INSRATE_6_18,
                   SPBFILE.SPB_INSTERM_6_19,
                   SPBFILE.SPB_INSRATE_6_19,
                   SPBFILE.SPB_INSTERM_6_20,
                   SPBFILE.SPB_INSRATE_6_20,
                   SPBFILE.SPB_INSMINPREM_6,
                   SPBFILE.SPB_INSMINCHGABL_6,
                   SPBFILE.SPB_INSMAXCHGABL_6,
                   SPBFILE.SPB_INSMAXBENIF_6,
                   SPBFILE.SPB_INSMAXTERM_6,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_6,''),
                   SPBFILE.SPB_INSEXCEPT_6,
                   SPBFILE.SPB_INSNOVER_6,
                   SPBFILE.SPB_INSMINAGE_6_1,
                   SPBFILE.SPB_INSMAXAGE_6_1,
                   SPBFILE.SPB_INSMAXCOV_6_1,
                   SPBFILE.SPB_INSMAXTRM_6_1,
                   SPBFILE.SPB_INSMINAGE_6_2,
                   SPBFILE.SPB_INSMAXAGE_6_2,
                   SPBFILE.SPB_INSMAXCOV_6_2,
                   SPBFILE.SPB_INSMAXTRM_6_2,
                   SPBFILE.SPB_INSMINAGE_6_3,
                   SPBFILE.SPB_INSMAXAGE_6_3,
                   SPBFILE.SPB_INSMAXCOV_6_3,
                   SPBFILE.SPB_INSMAXTRM_6_3,
                   SPBFILE.SPB_INSMINAGE_6_4,
                   SPBFILE.SPB_INSMAXAGE_6_4,
                   SPBFILE.SPB_INSMAXCOV_6_4,
                   SPBFILE.SPB_INSMAXTRM_6_4,
                   SPBFILE.SPB_INSMINAGE_6_5,
                   SPBFILE.SPB_INSMAXAGE_6_5,
                   SPBFILE.SPB_INSMAXCOV_6_5,
                   SPBFILE.SPB_INSMAXTRM_6_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_6,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_6,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_6,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_6,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_6,''),
                   SPBFILE.SPB_INSCOM_7,
                   SPBFILE.SPB_INSRATECD_7,
                   SPBFILE.SPB_INSJFAC_7,
                   SPBFILE.SPB_INSDISCNT_7,
                   ISNULL(SPBFILE.SPB_INSCHGABL_7,''),
                   SPBFILE.SPB_INSTERM_7_1,
                   SPBFILE.SPB_INSRATE_7_1,
                   SPBFILE.SPB_INSTERM_7_2,
                   SPBFILE.SPB_INSRATE_7_2,
                   SPBFILE.SPB_INSTERM_7_3,
                   SPBFILE.SPB_INSRATE_7_3,
                   SPBFILE.SPB_INSTERM_7_4,
                   SPBFILE.SPB_INSRATE_7_4,
                   SPBFILE.SPB_INSTERM_7_5,
                   SPBFILE.SPB_INSRATE_7_5,
                   SPBFILE.SPB_INSTERM_7_6,
                   SPBFILE.SPB_INSRATE_7_6,
                   SPBFILE.SPB_INSTERM_7_7,
                   SPBFILE.SPB_INSRATE_7_7,
                   SPBFILE.SPB_INSTERM_7_8,
                   SPBFILE.SPB_INSRATE_7_8,
                   SPBFILE.SPB_INSTERM_7_9,
                   SPBFILE.SPB_INSRATE_7_9,
                   SPBFILE.SPB_INSTERM_7_10,
                   SPBFILE.SPB_INSRATE_7_10,
                   SPBFILE.SPB_INSTERM_7_11,
                   SPBFILE.SPB_INSRATE_7_11,
                   SPBFILE.SPB_INSTERM_7_12,
                   SPBFILE.SPB_INSRATE_7_12,
                   SPBFILE.SPB_INSTERM_7_13,
                   SPBFILE.SPB_INSRATE_7_13,
                   SPBFILE.SPB_INSTERM_7_14,
                   SPBFILE.SPB_INSRATE_7_14,
                   SPBFILE.SPB_INSTERM_7_15,
                   SPBFILE.SPB_INSRATE_7_15,
                   SPBFILE.SPB_INSTERM_7_16,
                   SPBFILE.SPB_INSRATE_7_16,
                   SPBFILE.SPB_INSTERM_7_17,
                   SPBFILE.SPB_INSRATE_7_17,
                   SPBFILE.SPB_INSTERM_7_18,
                   SPBFILE.SPB_INSRATE_7_18,
                   SPBFILE.SPB_INSTERM_7_19,
                   SPBFILE.SPB_INSRATE_7_19,
                   SPBFILE.SPB_INSTERM_7_20,
                   SPBFILE.SPB_INSRATE_7_20,
                   SPBFILE.SPB_INSMINPREM_7,
                   SPBFILE.SPB_INSMINCHGABL_7,
                   SPBFILE.SPB_INSMAXCHGABL_7,
                   SPBFILE.SPB_INSMAXBENIF_7,
                   SPBFILE.SPB_INSMAXTERM_7,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_7,''),
                   SPBFILE.SPB_INSEXCEPT_7,
                   SPBFILE.SPB_INSNOVER_7,
                   SPBFILE.SPB_INSMINAGE_7_1,
                   SPBFILE.SPB_INSMAXAGE_7_1,
                   SPBFILE.SPB_INSMAXCOV_7_1,
                   SPBFILE.SPB_INSMAXTRM_7_1,
                   SPBFILE.SPB_INSMINAGE_7_2,
                   SPBFILE.SPB_INSMAXAGE_7_2,
                   SPBFILE.SPB_INSMAXCOV_7_2,
                   SPBFILE.SPB_INSMAXTRM_7_2,
                   SPBFILE.SPB_INSMINAGE_7_3,
                   SPBFILE.SPB_INSMAXAGE_7_3,
                   SPBFILE.SPB_INSMAXCOV_7_3,
                   SPBFILE.SPB_INSMAXTRM_7_3,
                   SPBFILE.SPB_INSMINAGE_7_4,
                   SPBFILE.SPB_INSMAXAGE_7_4,
                   SPBFILE.SPB_INSMAXCOV_7_4,
                   SPBFILE.SPB_INSMAXTRM_7_4,
                   SPBFILE.SPB_INSMINAGE_7_5,
                   SPBFILE.SPB_INSMAXAGE_7_5,
                   SPBFILE.SPB_INSMAXCOV_7_5,
                   SPBFILE.SPB_INSMAXTRM_7_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_7,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_7,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_7,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_7,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_7,''),
                   SPBFILE.SPB_INSCOM_8,
                   SPBFILE.SPB_INSRATECD_8,
                   SPBFILE.SPB_INSJFAC_8,
                   SPBFILE.SPB_INSDISCNT_8,
                   ISNULL(SPBFILE.SPB_INSCHGABL_8,''),
                   SPBFILE.SPB_INSTERM_8_1,
                   SPBFILE.SPB_INSRATE_8_1,
                   SPBFILE.SPB_INSTERM_8_2,
                   SPBFILE.SPB_INSRATE_8_2,
                   SPBFILE.SPB_INSTERM_8_3,
                   SPBFILE.SPB_INSRATE_8_3,
                   SPBFILE.SPB_INSTERM_8_4,
                   SPBFILE.SPB_INSRATE_8_4,
                   SPBFILE.SPB_INSTERM_8_5,
                   SPBFILE.SPB_INSRATE_8_5,
                   SPBFILE.SPB_INSTERM_8_6,
                   SPBFILE.SPB_INSRATE_8_6,
                   SPBFILE.SPB_INSTERM_8_7,
                   SPBFILE.SPB_INSRATE_8_7,
                   SPBFILE.SPB_INSTERM_8_8,
                   SPBFILE.SPB_INSRATE_8_8,
                   SPBFILE.SPB_INSTERM_8_9,
                   SPBFILE.SPB_INSRATE_8_9,
                   SPBFILE.SPB_INSTERM_8_10,
                   SPBFILE.SPB_INSRATE_8_10,
                   SPBFILE.SPB_INSTERM_8_11,
                   SPBFILE.SPB_INSRATE_8_11,
                   SPBFILE.SPB_INSTERM_8_12,
                   SPBFILE.SPB_INSRATE_8_12,
                   SPBFILE.SPB_INSTERM_8_13,
                   SPBFILE.SPB_INSRATE_8_13,
                   SPBFILE.SPB_INSTERM_8_14,
                   SPBFILE.SPB_INSRATE_8_14,
                   SPBFILE.SPB_INSTERM_8_15,
                   SPBFILE.SPB_INSRATE_8_15,
                   SPBFILE.SPB_INSTERM_8_16,
                   SPBFILE.SPB_INSRATE_8_16,
                   SPBFILE.SPB_INSTERM_8_17,
                   SPBFILE.SPB_INSRATE_8_17,
                   SPBFILE.SPB_INSTERM_8_18,
                   SPBFILE.SPB_INSRATE_8_18,
                   SPBFILE.SPB_INSTERM_8_19,
                   SPBFILE.SPB_INSRATE_8_19,
                   SPBFILE.SPB_INSTERM_8_20,
                   SPBFILE.SPB_INSRATE_8_20,
                   SPBFILE.SPB_INSMINPREM_8,
                   SPBFILE.SPB_INSMINCHGABL_8,
                   SPBFILE.SPB_INSMAXCHGABL_8,
                   SPBFILE.SPB_INSMAXBENIF_8,
                   SPBFILE.SPB_INSMAXTERM_8,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_8,''),
                   SPBFILE.SPB_INSEXCEPT_8,
                   SPBFILE.SPB_INSNOVER_8,
                   SPBFILE.SPB_INSMINAGE_8_1,
                   SPBFILE.SPB_INSMAXAGE_8_1,
                   SPBFILE.SPB_INSMAXCOV_8_1,
                   SPBFILE.SPB_INSMAXTRM_8_1,
                   SPBFILE.SPB_INSMINAGE_8_2,
                   SPBFILE.SPB_INSMAXAGE_8_2,
                   SPBFILE.SPB_INSMAXCOV_8_2,
                   SPBFILE.SPB_INSMAXTRM_8_2,
                   SPBFILE.SPB_INSMINAGE_8_3,
                   SPBFILE.SPB_INSMAXAGE_8_3,
                   SPBFILE.SPB_INSMAXCOV_8_3,
                   SPBFILE.SPB_INSMAXTRM_8_3,
                   SPBFILE.SPB_INSMINAGE_8_4,
                   SPBFILE.SPB_INSMAXAGE_8_4,
                   SPBFILE.SPB_INSMAXCOV_8_4,
                   SPBFILE.SPB_INSMAXTRM_8_4,
                   SPBFILE.SPB_INSMINAGE_8_5,
                   SPBFILE.SPB_INSMAXAGE_8_5,
                   SPBFILE.SPB_INSMAXCOV_8_5,
                   SPBFILE.SPB_INSMAXTRM_8_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_8,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_8,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_8,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_8,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_8,''),
                   SPBFILE.SPB_INSCOM_9,
                   SPBFILE.SPB_INSRATECD_9,
                   SPBFILE.SPB_INSJFAC_9,
                   SPBFILE.SPB_INSDISCNT_9,
                   ISNULL(SPBFILE.SPB_INSCHGABL_9,''),
                   SPBFILE.SPB_INSTERM_9_1,
                   SPBFILE.SPB_INSRATE_9_1,
                   SPBFILE.SPB_INSTERM_9_2,
                   SPBFILE.SPB_INSRATE_9_2,
                   SPBFILE.SPB_INSTERM_9_3,
                   SPBFILE.SPB_INSRATE_9_3,
                   SPBFILE.SPB_INSTERM_9_4,
                   SPBFILE.SPB_INSRATE_9_4,
                   SPBFILE.SPB_INSTERM_9_5,
                   SPBFILE.SPB_INSRATE_9_5,
                   SPBFILE.SPB_INSTERM_9_6,
                   SPBFILE.SPB_INSRATE_9_6,
                   SPBFILE.SPB_INSTERM_9_7,
                   SPBFILE.SPB_INSRATE_9_7,
                   SPBFILE.SPB_INSTERM_9_8,
                   SPBFILE.SPB_INSRATE_9_8,
                   SPBFILE.SPB_INSTERM_9_9,
                   SPBFILE.SPB_INSRATE_9_9,
                   SPBFILE.SPB_INSTERM_9_10,
                   SPBFILE.SPB_INSRATE_9_10,
                   SPBFILE.SPB_INSTERM_9_11,
                   SPBFILE.SPB_INSRATE_9_11,
                   SPBFILE.SPB_INSTERM_9_12,
                   SPBFILE.SPB_INSRATE_9_12,
                   SPBFILE.SPB_INSTERM_9_13,
                   SPBFILE.SPB_INSRATE_9_13,
                   SPBFILE.SPB_INSTERM_9_14,
                   SPBFILE.SPB_INSRATE_9_14,
                   SPBFILE.SPB_INSTERM_9_15,
                   SPBFILE.SPB_INSRATE_9_15,
                   SPBFILE.SPB_INSTERM_9_16,
                   SPBFILE.SPB_INSRATE_9_16,
                   SPBFILE.SPB_INSTERM_9_17,
                   SPBFILE.SPB_INSRATE_9_17,
                   SPBFILE.SPB_INSTERM_9_18,
                   SPBFILE.SPB_INSRATE_9_18,
                   SPBFILE.SPB_INSTERM_9_19,
                   SPBFILE.SPB_INSRATE_9_19,
                   SPBFILE.SPB_INSTERM_9_20,
                   SPBFILE.SPB_INSRATE_9_20,
                   SPBFILE.SPB_INSMINPREM_9,
                   SPBFILE.SPB_INSMINCHGABL_9,
                   SPBFILE.SPB_INSMAXCHGABL_9,
                   SPBFILE.SPB_INSMAXBENIF_9,
                   SPBFILE.SPB_INSMAXTERM_9,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_9,''),
                   SPBFILE.SPB_INSEXCEPT_9,
                   SPBFILE.SPB_INSNOVER_9,
                   SPBFILE.SPB_INSMINAGE_9_1,
                   SPBFILE.SPB_INSMAXAGE_9_1,
                   SPBFILE.SPB_INSMAXCOV_9_1,
                   SPBFILE.SPB_INSMAXTRM_9_1,
                   SPBFILE.SPB_INSMINAGE_9_2,
                   SPBFILE.SPB_INSMAXAGE_9_2,
                   SPBFILE.SPB_INSMAXCOV_9_2,
                   SPBFILE.SPB_INSMAXTRM_9_2,
                   SPBFILE.SPB_INSMINAGE_9_3,
                   SPBFILE.SPB_INSMAXAGE_9_3,
                   SPBFILE.SPB_INSMAXCOV_9_3,
                   SPBFILE.SPB_INSMAXTRM_9_3,
                   SPBFILE.SPB_INSMINAGE_9_4,
                   SPBFILE.SPB_INSMAXAGE_9_4,
                   SPBFILE.SPB_INSMAXCOV_9_4,
                   SPBFILE.SPB_INSMAXTRM_9_4,
                   SPBFILE.SPB_INSMINAGE_9_5,
                   SPBFILE.SPB_INSMAXAGE_9_5,
                   SPBFILE.SPB_INSMAXCOV_9_5,
                   SPBFILE.SPB_INSMAXTRM_9_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_9,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_9,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_9,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_9,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_9,''),
                   SPBFILE.SPB_INSCOM_10,
                   SPBFILE.SPB_INSRATECD_10,
                   SPBFILE.SPB_INSJFAC_10,
                   SPBFILE.SPB_INSDISCNT_10,
                   ISNULL(SPBFILE.SPB_INSCHGABL_10,''),
                   SPBFILE.SPB_INSTERM_10_1,
                   SPBFILE.SPB_INSRATE_10_1,
                   SPBFILE.SPB_INSTERM_10_2,
                   SPBFILE.SPB_INSRATE_10_2,
                   SPBFILE.SPB_INSTERM_10_3,
                   SPBFILE.SPB_INSRATE_10_3,
                   SPBFILE.SPB_INSTERM_10_4,
                   SPBFILE.SPB_INSRATE_10_4,
                   SPBFILE.SPB_INSTERM_10_5,
                   SPBFILE.SPB_INSRATE_10_5,
                   SPBFILE.SPB_INSTERM_10_6,
                   SPBFILE.SPB_INSRATE_10_6,
                   SPBFILE.SPB_INSTERM_10_7,
                   SPBFILE.SPB_INSRATE_10_7,
                   SPBFILE.SPB_INSTERM_10_8,
                   SPBFILE.SPB_INSRATE_10_8,
                   SPBFILE.SPB_INSTERM_10_9,
                   SPBFILE.SPB_INSRATE_10_9,
                   SPBFILE.SPB_INSTERM_10_10,
                   SPBFILE.SPB_INSRATE_10_10,
                   SPBFILE.SPB_INSTERM_10_11,
                   SPBFILE.SPB_INSRATE_10_11,
                   SPBFILE.SPB_INSTERM_10_12,
                   SPBFILE.SPB_INSRATE_10_12,
                   SPBFILE.SPB_INSTERM_10_13,
                   SPBFILE.SPB_INSRATE_10_13,
                   SPBFILE.SPB_INSTERM_10_14,
                   SPBFILE.SPB_INSRATE_10_14,
                   SPBFILE.SPB_INSTERM_10_15,
                   SPBFILE.SPB_INSRATE_10_15,
                   SPBFILE.SPB_INSTERM_10_16,
                   SPBFILE.SPB_INSRATE_10_16,
                   SPBFILE.SPB_INSTERM_10_17,
                   SPBFILE.SPB_INSRATE_10_17,
                   SPBFILE.SPB_INSTERM_10_18,
                   SPBFILE.SPB_INSRATE_10_18,
                   SPBFILE.SPB_INSTERM_10_19,
                   SPBFILE.SPB_INSRATE_10_19,
                   SPBFILE.SPB_INSTERM_10_20,
                   SPBFILE.SPB_INSRATE_10_20,
                   SPBFILE.SPB_INSMINPREM_10,
                   SPBFILE.SPB_INSMINCHGABL_10,
                   SPBFILE.SPB_INSMAXCHGABL_10,
                   SPBFILE.SPB_INSMAXBENIF_10,
                   SPBFILE.SPB_INSMAXTERM_10,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_10,''),
                   SPBFILE.SPB_INSEXCEPT_10,
                   SPBFILE.SPB_INSNOVER_10,
                   SPBFILE.SPB_INSMINAGE_10_1,
                   SPBFILE.SPB_INSMAXAGE_10_1,
                   SPBFILE.SPB_INSMAXCOV_10_1,
                   SPBFILE.SPB_INSMAXTRM_10_1,
                   SPBFILE.SPB_INSMINAGE_10_2,
                   SPBFILE.SPB_INSMAXAGE_10_2,
                   SPBFILE.SPB_INSMAXCOV_10_2,
                   SPBFILE.SPB_INSMAXTRM_10_2,
                   SPBFILE.SPB_INSMINAGE_10_3,
                   SPBFILE.SPB_INSMAXAGE_10_3,
                   SPBFILE.SPB_INSMAXCOV_10_3,
                   SPBFILE.SPB_INSMAXTRM_10_3,
                   SPBFILE.SPB_INSMINAGE_10_4,
                   SPBFILE.SPB_INSMAXAGE_10_4,
                   SPBFILE.SPB_INSMAXCOV_10_4,
                   SPBFILE.SPB_INSMAXTRM_10_4,
                   SPBFILE.SPB_INSMINAGE_10_5,
                   SPBFILE.SPB_INSMAXAGE_10_5,
                   SPBFILE.SPB_INSMAXCOV_10_5,
                   SPBFILE.SPB_INSMAXTRM_10_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_10,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_10,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_10,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_10,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_10,''),
                   SPBFILE.SPB_INSCOM_11,
                   SPBFILE.SPB_INSRATECD_11,
                   SPBFILE.SPB_INSJFAC_11,
                   SPBFILE.SPB_INSDISCNT_11,
                   ISNULL(SPBFILE.SPB_INSCHGABL_11,''),
                   SPBFILE.SPB_INSTERM_11_1,
                   SPBFILE.SPB_INSRATE_11_1,
                   SPBFILE.SPB_INSTERM_11_2,
                   SPBFILE.SPB_INSRATE_11_2,
                   SPBFILE.SPB_INSTERM_11_3,
                   SPBFILE.SPB_INSRATE_11_3,
                   SPBFILE.SPB_INSTERM_11_4,
                   SPBFILE.SPB_INSRATE_11_4,
                   SPBFILE.SPB_INSTERM_11_5,
                   SPBFILE.SPB_INSRATE_11_5,
                   SPBFILE.SPB_INSTERM_11_6,
                   SPBFILE.SPB_INSRATE_11_6,
                   SPBFILE.SPB_INSTERM_11_7,
                   SPBFILE.SPB_INSRATE_11_7,
                   SPBFILE.SPB_INSTERM_11_8,
                   SPBFILE.SPB_INSRATE_11_8,
                   SPBFILE.SPB_INSTERM_11_9,
                   SPBFILE.SPB_INSRATE_11_9,
                   SPBFILE.SPB_INSTERM_11_10,
                   SPBFILE.SPB_INSRATE_11_10,
                   SPBFILE.SPB_INSTERM_11_11,
                   SPBFILE.SPB_INSRATE_11_11,
                   SPBFILE.SPB_INSTERM_11_12,
                   SPBFILE.SPB_INSRATE_11_12,
                   SPBFILE.SPB_INSTERM_11_13,
                   SPBFILE.SPB_INSRATE_11_13,
                   SPBFILE.SPB_INSTERM_11_14,
                   SPBFILE.SPB_INSRATE_11_14,
                   SPBFILE.SPB_INSTERM_11_15,
                   SPBFILE.SPB_INSRATE_11_15,
                   SPBFILE.SPB_INSTERM_11_16,
                   SPBFILE.SPB_INSRATE_11_16,
                   SPBFILE.SPB_INSTERM_11_17,
                   SPBFILE.SPB_INSRATE_11_17,
                   SPBFILE.SPB_INSTERM_11_18,
                   SPBFILE.SPB_INSRATE_11_18,
                   SPBFILE.SPB_INSTERM_11_19,
                   SPBFILE.SPB_INSRATE_11_19,
                   SPBFILE.SPB_INSTERM_11_20,
                   SPBFILE.SPB_INSRATE_11_20,
                   SPBFILE.SPB_INSMINPREM_11,
                   SPBFILE.SPB_INSMINCHGABL_11,
                   SPBFILE.SPB_INSMAXCHGABL_11,
                   SPBFILE.SPB_INSMAXBENIF_11,
                   SPBFILE.SPB_INSMAXTERM_11,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_11,''),
                   SPBFILE.SPB_INSEXCEPT_11,
                   SPBFILE.SPB_INSNOVER_11,
                   SPBFILE.SPB_INSMINAGE_11_1,
                   SPBFILE.SPB_INSMAXAGE_11_1,
                   SPBFILE.SPB_INSMAXCOV_11_1,
                   SPBFILE.SPB_INSMAXTRM_11_1,
                   SPBFILE.SPB_INSMINAGE_11_2,
                   SPBFILE.SPB_INSMAXAGE_11_2,
                   SPBFILE.SPB_INSMAXCOV_11_2,
                   SPBFILE.SPB_INSMAXTRM_11_2,
                   SPBFILE.SPB_INSMINAGE_11_3,
                   SPBFILE.SPB_INSMAXAGE_11_3,
                   SPBFILE.SPB_INSMAXCOV_11_3,
                   SPBFILE.SPB_INSMAXTRM_11_3,
                   SPBFILE.SPB_INSMINAGE_11_4,
                   SPBFILE.SPB_INSMAXAGE_11_4,
                   SPBFILE.SPB_INSMAXCOV_11_4,
                   SPBFILE.SPB_INSMAXTRM_11_4,
                   SPBFILE.SPB_INSMINAGE_11_5,
                   SPBFILE.SPB_INSMAXAGE_11_5,
                   SPBFILE.SPB_INSMAXCOV_11_5,
                   SPBFILE.SPB_INSMAXTRM_11_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_11,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_11,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_11,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_11,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_11,''),
                   SPBFILE.SPB_INSCOM_12,
                   SPBFILE.SPB_INSRATECD_12,
                   SPBFILE.SPB_INSJFAC_12,
                   SPBFILE.SPB_INSDISCNT_12,
                   ISNULL(SPBFILE.SPB_INSCHGABL_12,''),
                   SPBFILE.SPB_INSTERM_12_1,
                   SPBFILE.SPB_INSRATE_12_1,
                   SPBFILE.SPB_INSTERM_12_2,
                   SPBFILE.SPB_INSRATE_12_2,
                   SPBFILE.SPB_INSTERM_12_3,
                   SPBFILE.SPB_INSRATE_12_3,
                   SPBFILE.SPB_INSTERM_12_4,
                   SPBFILE.SPB_INSRATE_12_4,
                   SPBFILE.SPB_INSTERM_12_5,
                   SPBFILE.SPB_INSRATE_12_5,
                   SPBFILE.SPB_INSTERM_12_6,
                   SPBFILE.SPB_INSRATE_12_6,
                   SPBFILE.SPB_INSTERM_12_7,
                   SPBFILE.SPB_INSRATE_12_7,
                   SPBFILE.SPB_INSTERM_12_8,
                   SPBFILE.SPB_INSRATE_12_8,
                   SPBFILE.SPB_INSTERM_12_9,
                   SPBFILE.SPB_INSRATE_12_9,
                   SPBFILE.SPB_INSTERM_12_10,
                   SPBFILE.SPB_INSRATE_12_10,
                   SPBFILE.SPB_INSTERM_12_11,
                   SPBFILE.SPB_INSRATE_12_11,
                   SPBFILE.SPB_INSTERM_12_12,
                   SPBFILE.SPB_INSRATE_12_12,
                   SPBFILE.SPB_INSTERM_12_13,
                   SPBFILE.SPB_INSRATE_12_13,
                   SPBFILE.SPB_INSTERM_12_14,
                   SPBFILE.SPB_INSRATE_12_14,
                   SPBFILE.SPB_INSTERM_12_15,
                   SPBFILE.SPB_INSRATE_12_15,
                   SPBFILE.SPB_INSTERM_12_16,
                   SPBFILE.SPB_INSRATE_12_16,
                   SPBFILE.SPB_INSTERM_12_17,
                   SPBFILE.SPB_INSRATE_12_17,
                   SPBFILE.SPB_INSTERM_12_18,
                   SPBFILE.SPB_INSRATE_12_18,
                   SPBFILE.SPB_INSTERM_12_19,
                   SPBFILE.SPB_INSRATE_12_19,
                   SPBFILE.SPB_INSTERM_12_20,
                   SPBFILE.SPB_INSRATE_12_20,
                   SPBFILE.SPB_INSMINPREM_12,
                   SPBFILE.SPB_INSMINCHGABL_12,
                   SPBFILE.SPB_INSMAXCHGABL_12,
                   SPBFILE.SPB_INSMAXBENIF_12,
                   SPBFILE.SPB_INSMAXTERM_12,
                   ISNULL(SPBFILE.SPB_INSCHGBEN_12,''),
                   SPBFILE.SPB_INSEXCEPT_12,
                   SPBFILE.SPB_INSNOVER_12,
                   SPBFILE.SPB_INSMINAGE_12_1,
                   SPBFILE.SPB_INSMAXAGE_12_1,
                   SPBFILE.SPB_INSMAXCOV_12_1,
                   SPBFILE.SPB_INSMAXTRM_12_1,
                   SPBFILE.SPB_INSMINAGE_12_2,
                   SPBFILE.SPB_INSMAXAGE_12_2,
                   SPBFILE.SPB_INSMAXCOV_12_2,
                   SPBFILE.SPB_INSMAXTRM_12_2,
                   SPBFILE.SPB_INSMINAGE_12_3,
                   SPBFILE.SPB_INSMAXAGE_12_3,
                   SPBFILE.SPB_INSMAXCOV_12_3,
                   SPBFILE.SPB_INSMAXTRM_12_3,
                   SPBFILE.SPB_INSMINAGE_12_4,
                   SPBFILE.SPB_INSMAXAGE_12_4,
                   SPBFILE.SPB_INSMAXCOV_12_4,
                   SPBFILE.SPB_INSMAXTRM_12_4,
                   SPBFILE.SPB_INSMINAGE_12_5,
                   SPBFILE.SPB_INSMAXAGE_12_5,
                   SPBFILE.SPB_INSMAXCOV_12_5,
                   SPBFILE.SPB_INSMAXTRM_12_5,
                   ISNULL(SPBFILE.SPB_INSFEECD_12,''),
                   ISNULL(SPBFILE.SPB_INSFEE_FRMLA_12,''),
                   ISNULL(SPBFILE.SPB_INSCHGABL_FG_12,''),
                   ISNULL(SPBFILE.SPB_INSREJECT_FG_12,''),
                   ISNULL(SPBFILE.SPB_INSFINCHG_12,'')
            INTO :QSPB-REC
            FROM DBO.SPBFILE
            WHERE SPBFILE.SPB_ORGST = :QSPB-ORGST
              AND SPBFILE.SPB_SPRCLASS = :QSPB-SPRCLASS
              AND SPBFILE.SPB_SUBCLASS = :QSPB-SUBCLASS
              AND SPBFILE.SPB_LAWCODE = :QSPB-LAWCODE 
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SPB1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-SPB-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-SPB1-FILE.
           PERFORM CLOSE-IT.

      *=================================================================
       LOAD-SPC1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( SP-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( SP-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO SPC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO SPC-ALL-MACHINE
              MOVE SPC-ALL-PATH             TO SPC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( SP-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO SPC-ALL-BASE
              MOVE FILPATH-MACHINE         TO SPC-ALL-MACHINE
              MOVE SPC-ALL-PATH             TO SPC-PATH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-SPCFILE NOT = "S" )
              MOVE SPC-PATH-BEG          TO SPC-SELECT-PATH-BEG
              MOVE SPC-PATH-FILE         TO SPC-SELECT-PATH-FILE
           ELSE
              MOVE SPC-PATH-SQL          TO SPC-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-SPC1-FILE.
           PERFORM LOAD-SPC1-FILE.
           PERFORM OPEN-IT.
           MOVE SPC-SELECT-PATH TO E-FILE.

      *-----------------------------------------------------------------
       READ-SPC1-FILE.
           PERFORM READ-IT.
           MOVE SPC-SELECT-PATH TO E-FILE.
           MOVE SPC1-KEY TO E-KEYX.

           MOVE SPC-ORGST            TO QSPC-ORGST.
           MOVE SPC-SPRCLASS         TO QSPC-SPRCLASS.
           MOVE SPC-SUBCLASS         TO QSPC-SUBCLASS.
           MOVE SPC-LAWCODE          TO QSPC-LAWCODE.

           EXEC SQL
            SELECT ISNULL(SPCFILE.SPC_ORGST,''),
                   SPCFILE.SPC_SPRCLASS,
                   SPCFILE.SPC_SUBCLASS,
                   SPCFILE.SPC_LAWCODE,
                   SPCFILE.SPC_INSEARN_ORDER_1,
                   SPCFILE.SPC_INSEARN_ORDER_2,
                   SPCFILE.SPC_INSEARN_ORDER_3,
                   SPCFILE.SPC_INSEARN_ORDER_4,
                   SPCFILE.SPC_INSEARN_ORDER_5,
                   SPCFILE.SPC_INSEARN_ORDER_6,
                   SPCFILE.SPC_INSEARN_ORDER_7,
                   SPCFILE.SPC_INSEARN_ORDER_8,
                   SPCFILE.SPC_INSEARN_ORDER_9,
                   SPCFILE.SPC_INSEARN_ORDER_10,
                   SPCFILE.SPC_INSEARN_ORDER_11,
                   SPCFILE.SPC_INSEARN_ORDER_12,
                   ISNULL(SPCFILE.SPC_INSAH_JOINT_FG,''),
                   ISNULL(SPCFILE.SPC_INS_O1_EDIT,''),
                   ISNULL(SPCFILE.SPC_INS_O2_EDIT,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_1,''),
                   ISNULL(SPCFILE.SPC_RBDTE_1_1,''),
                   SPCFILE.SPC_RBDAYS_1_1,
                   SPCFILE.SPC_RBYRTYPE_1_1,
                   ISNULL(SPCFILE.SPC_RBDTE_1_2,''),
                   SPCFILE.SPC_RBDAYS_1_2,
                   SPCFILE.SPC_RBYRTYPE_1_2,
                   SPCFILE.SPC_RBMINRET_1,
                   SPCFILE.SPC_RBNOMON_1,
                   SPCFILE.SPC_RBDEDCHG_1,
                   SPCFILE.SPC_RBRECAST_1,
                   SPCFILE.SPC_RBEARLY_1,
                   SPCFILE.SPC_RBDEFER_1,
                   SPCFILE.SPC_RBPCT_1,
                   SPCFILE.SPC_RBREDUC_1,
                   SPCFILE.SPC_RBMIN_1,
                   SPCFILE.SPC_RBSPOPT1_1,
                   SPCFILE.SPC_RBSPOPT2_1,
                   SPCFILE.SPC_RBMANAGER_1,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_1,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_1,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_2,''),
                   ISNULL(SPCFILE.SPC_RBDTE_2_1,''),
                   SPCFILE.SPC_RBDAYS_2_1,
                   SPCFILE.SPC_RBYRTYPE_2_1,
                   ISNULL(SPCFILE.SPC_RBDTE_2_2,''),
                   SPCFILE.SPC_RBDAYS_2_2,
                   SPCFILE.SPC_RBYRTYPE_2_2,
                   SPCFILE.SPC_RBMINRET_2,
                   SPCFILE.SPC_RBNOMON_2,
                   SPCFILE.SPC_RBDEDCHG_2,
                   SPCFILE.SPC_RBRECAST_2,
                   SPCFILE.SPC_RBEARLY_2,
                   SPCFILE.SPC_RBDEFER_2,
                   SPCFILE.SPC_RBPCT_2,
                   SPCFILE.SPC_RBREDUC_2,
                   SPCFILE.SPC_RBMIN_2,
                   SPCFILE.SPC_RBSPOPT1_2,
                   SPCFILE.SPC_RBSPOPT2_2,
                   SPCFILE.SPC_RBMANAGER_2,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_2,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_2,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_3,''),
                   ISNULL(SPCFILE.SPC_RBDTE_3_1,''),
                   SPCFILE.SPC_RBDAYS_3_1,
                   SPCFILE.SPC_RBYRTYPE_3_1,
                   ISNULL(SPCFILE.SPC_RBDTE_3_2,''),
                   SPCFILE.SPC_RBDAYS_3_2,
                   SPCFILE.SPC_RBYRTYPE_3_2,
                   SPCFILE.SPC_RBMINRET_3,
                   SPCFILE.SPC_RBNOMON_3,
                   SPCFILE.SPC_RBDEDCHG_3,
                   SPCFILE.SPC_RBRECAST_3,
                   SPCFILE.SPC_RBEARLY_3,
                   SPCFILE.SPC_RBDEFER_3,
                   SPCFILE.SPC_RBPCT_3,
                   SPCFILE.SPC_RBREDUC_3,
                   SPCFILE.SPC_RBMIN_3,
                   SPCFILE.SPC_RBSPOPT1_3,
                   SPCFILE.SPC_RBSPOPT2_3,
                   SPCFILE.SPC_RBMANAGER_3,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_3,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_3,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_4,''),
                   ISNULL(SPCFILE.SPC_RBDTE_4_1,''),
                   SPCFILE.SPC_RBDAYS_4_1,
                   SPCFILE.SPC_RBYRTYPE_4_1,
                   ISNULL(SPCFILE.SPC_RBDTE_4_2,''),
                   SPCFILE.SPC_RBDAYS_4_2,
                   SPCFILE.SPC_RBYRTYPE_4_2,
                   SPCFILE.SPC_RBMINRET_4,
                   SPCFILE.SPC_RBNOMON_4,
                   SPCFILE.SPC_RBDEDCHG_4,
                   SPCFILE.SPC_RBRECAST_4,
                   SPCFILE.SPC_RBEARLY_4,
                   SPCFILE.SPC_RBDEFER_4,
                   SPCFILE.SPC_RBPCT_4,
                   SPCFILE.SPC_RBREDUC_4,
                   SPCFILE.SPC_RBMIN_4,
                   SPCFILE.SPC_RBSPOPT1_4,
                   SPCFILE.SPC_RBSPOPT2_4,
                   SPCFILE.SPC_RBMANAGER_4,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_4,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_4,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_5,''),
                   ISNULL(SPCFILE.SPC_RBDTE_5_1,''),
                   SPCFILE.SPC_RBDAYS_5_1,
                   SPCFILE.SPC_RBYRTYPE_5_1,
                   ISNULL(SPCFILE.SPC_RBDTE_5_2,''),
                   SPCFILE.SPC_RBDAYS_5_2,
                   SPCFILE.SPC_RBYRTYPE_5_2,
                   SPCFILE.SPC_RBMINRET_5,
                   SPCFILE.SPC_RBNOMON_5,
                   SPCFILE.SPC_RBDEDCHG_5,
                   SPCFILE.SPC_RBRECAST_5,
                   SPCFILE.SPC_RBEARLY_5,
                   SPCFILE.SPC_RBDEFER_5,
                   SPCFILE.SPC_RBPCT_5,
                   SPCFILE.SPC_RBREDUC_5,
                   SPCFILE.SPC_RBMIN_5,
                   SPCFILE.SPC_RBSPOPT1_5,
                   SPCFILE.SPC_RBSPOPT2_5,
                   SPCFILE.SPC_RBMANAGER_5,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_5,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_5,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_6,''),
                   ISNULL(SPCFILE.SPC_RBDTE_6_1,''),
                   SPCFILE.SPC_RBDAYS_6_1,
                   SPCFILE.SPC_RBYRTYPE_6_1,
                   ISNULL(SPCFILE.SPC_RBDTE_6_2,''),
                   SPCFILE.SPC_RBDAYS_6_2,
                   SPCFILE.SPC_RBYRTYPE_6_2,
                   SPCFILE.SPC_RBMINRET_6,
                   SPCFILE.SPC_RBNOMON_6,
                   SPCFILE.SPC_RBDEDCHG_6,
                   SPCFILE.SPC_RBRECAST_6,
                   SPCFILE.SPC_RBEARLY_6,
                   SPCFILE.SPC_RBDEFER_6,
                   SPCFILE.SPC_RBPCT_6,
                   SPCFILE.SPC_RBREDUC_6,
                   SPCFILE.SPC_RBMIN_6,
                   SPCFILE.SPC_RBSPOPT1_6,
                   SPCFILE.SPC_RBSPOPT2_6,
                   SPCFILE.SPC_RBMANAGER_6,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_6,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_6,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_7,''),
                   ISNULL(SPCFILE.SPC_RBDTE_7_1,''),
                   SPCFILE.SPC_RBDAYS_7_1,
                   SPCFILE.SPC_RBYRTYPE_7_1,
                   ISNULL(SPCFILE.SPC_RBDTE_7_2,''),
                   SPCFILE.SPC_RBDAYS_7_2,
                   SPCFILE.SPC_RBYRTYPE_7_2,
                   SPCFILE.SPC_RBMINRET_7,
                   SPCFILE.SPC_RBNOMON_7,
                   SPCFILE.SPC_RBDEDCHG_7,
                   SPCFILE.SPC_RBRECAST_7,
                   SPCFILE.SPC_RBEARLY_7,
                   SPCFILE.SPC_RBDEFER_7,
                   SPCFILE.SPC_RBPCT_7,
                   SPCFILE.SPC_RBREDUC_7,
                   SPCFILE.SPC_RBMIN_7,
                   SPCFILE.SPC_RBSPOPT1_7,
                   SPCFILE.SPC_RBSPOPT2_7,
                   SPCFILE.SPC_RBMANAGER_7,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_7,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_7,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_8,''),
                   ISNULL(SPCFILE.SPC_RBDTE_8_1,''),
                   SPCFILE.SPC_RBDAYS_8_1,
                   SPCFILE.SPC_RBYRTYPE_8_1,
                   ISNULL(SPCFILE.SPC_RBDTE_8_2,''),
                   SPCFILE.SPC_RBDAYS_8_2,
                   SPCFILE.SPC_RBYRTYPE_8_2,
                   SPCFILE.SPC_RBMINRET_8,
                   SPCFILE.SPC_RBNOMON_8,
                   SPCFILE.SPC_RBDEDCHG_8,
                   SPCFILE.SPC_RBRECAST_8,
                   SPCFILE.SPC_RBEARLY_8,
                   SPCFILE.SPC_RBDEFER_8,
                   SPCFILE.SPC_RBPCT_8,
                   SPCFILE.SPC_RBREDUC_8,
                   SPCFILE.SPC_RBMIN_8,
                   SPCFILE.SPC_RBSPOPT1_8,
                   SPCFILE.SPC_RBSPOPT2_8,
                   SPCFILE.SPC_RBMANAGER_8,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_8,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_8,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_9,''),
                   ISNULL(SPCFILE.SPC_RBDTE_9_1,''),
                   SPCFILE.SPC_RBDAYS_9_1,
                   SPCFILE.SPC_RBYRTYPE_9_1,
                   ISNULL(SPCFILE.SPC_RBDTE_9_2,''),
                   SPCFILE.SPC_RBDAYS_9_2,
                   SPCFILE.SPC_RBYRTYPE_9_2,
                   SPCFILE.SPC_RBMINRET_9,
                   SPCFILE.SPC_RBNOMON_9,
                   SPCFILE.SPC_RBDEDCHG_9,
                   SPCFILE.SPC_RBRECAST_9,
                   SPCFILE.SPC_RBEARLY_9,
                   SPCFILE.SPC_RBDEFER_9,
                   SPCFILE.SPC_RBPCT_9,
                   SPCFILE.SPC_RBREDUC_9,
                   SPCFILE.SPC_RBMIN_9,
                   SPCFILE.SPC_RBSPOPT1_9,
                   SPCFILE.SPC_RBSPOPT2_9,
                   SPCFILE.SPC_RBMANAGER_9,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_9,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_9,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_10,''),
                   ISNULL(SPCFILE.SPC_RBDTE_10_1,''),
                   SPCFILE.SPC_RBDAYS_10_1,
                   SPCFILE.SPC_RBYRTYPE_10_1,
                   ISNULL(SPCFILE.SPC_RBDTE_10_2,''),
                   SPCFILE.SPC_RBDAYS_10_2,
                   SPCFILE.SPC_RBYRTYPE_10_2,
                   SPCFILE.SPC_RBMINRET_10,
                   SPCFILE.SPC_RBNOMON_10,
                   SPCFILE.SPC_RBDEDCHG_10,
                   SPCFILE.SPC_RBRECAST_10,
                   SPCFILE.SPC_RBEARLY_10,
                   SPCFILE.SPC_RBDEFER_10,
                   SPCFILE.SPC_RBPCT_10,
                   SPCFILE.SPC_RBREDUC_10,
                   SPCFILE.SPC_RBMIN_10,
                   SPCFILE.SPC_RBSPOPT1_10,
                   SPCFILE.SPC_RBSPOPT2_10,
                   SPCFILE.SPC_RBMANAGER_10,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_10,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_10,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_11,''),
                   ISNULL(SPCFILE.SPC_RBDTE_11_1,''),
                   SPCFILE.SPC_RBDAYS_11_1,
                   SPCFILE.SPC_RBYRTYPE_11_1,
                   ISNULL(SPCFILE.SPC_RBDTE_11_2,''),
                   SPCFILE.SPC_RBDAYS_11_2,
                   SPCFILE.SPC_RBYRTYPE_11_2,
                   SPCFILE.SPC_RBMINRET_11,
                   SPCFILE.SPC_RBNOMON_11,
                   SPCFILE.SPC_RBDEDCHG_11,
                   SPCFILE.SPC_RBRECAST_11,
                   SPCFILE.SPC_RBEARLY_11,
                   SPCFILE.SPC_RBDEFER_11,
                   SPCFILE.SPC_RBPCT_11,
                   SPCFILE.SPC_RBREDUC_11,
                   SPCFILE.SPC_RBMIN_11,
                   SPCFILE.SPC_RBSPOPT1_11,
                   SPCFILE.SPC_RBSPOPT2_11,
                   SPCFILE.SPC_RBMANAGER_11,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_11,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_11,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_12,''),
                   ISNULL(SPCFILE.SPC_RBDTE_12_1,''),
                   SPCFILE.SPC_RBDAYS_12_1,
                   SPCFILE.SPC_RBYRTYPE_12_1,
                   ISNULL(SPCFILE.SPC_RBDTE_12_2,''),
                   SPCFILE.SPC_RBDAYS_12_2,
                   SPCFILE.SPC_RBYRTYPE_12_2,
                   SPCFILE.SPC_RBMINRET_12,
                   SPCFILE.SPC_RBNOMON_12,
                   SPCFILE.SPC_RBDEDCHG_12,
                   SPCFILE.SPC_RBRECAST_12,
                   SPCFILE.SPC_RBEARLY_12,
                   SPCFILE.SPC_RBDEFER_12,
                   SPCFILE.SPC_RBPCT_12,
                   SPCFILE.SPC_RBREDUC_12,
                   SPCFILE.SPC_RBMIN_12,
                   SPCFILE.SPC_RBSPOPT1_12,
                   SPCFILE.SPC_RBSPOPT2_12,
                   SPCFILE.SPC_RBMANAGER_12,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_12,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_12,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_13,''),
                   ISNULL(SPCFILE.SPC_RBDTE_13_1,''),
                   SPCFILE.SPC_RBDAYS_13_1,
                   SPCFILE.SPC_RBYRTYPE_13_1,
                   ISNULL(SPCFILE.SPC_RBDTE_13_2,''),
                   SPCFILE.SPC_RBDAYS_13_2,
                   SPCFILE.SPC_RBYRTYPE_13_2,
                   SPCFILE.SPC_RBMINRET_13,
                   SPCFILE.SPC_RBNOMON_13,
                   SPCFILE.SPC_RBDEDCHG_13,
                   SPCFILE.SPC_RBRECAST_13,
                   SPCFILE.SPC_RBEARLY_13,
                   SPCFILE.SPC_RBDEFER_13,
                   SPCFILE.SPC_RBPCT_13,
                   SPCFILE.SPC_RBREDUC_13,
                   SPCFILE.SPC_RBMIN_13,
                   SPCFILE.SPC_RBSPOPT1_13,
                   SPCFILE.SPC_RBSPOPT2_13,
                   SPCFILE.SPC_RBMANAGER_13,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_13,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_13,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_14,''),
                   ISNULL(SPCFILE.SPC_RBDTE_14_1,''),
                   SPCFILE.SPC_RBDAYS_14_1,
                   SPCFILE.SPC_RBYRTYPE_14_1,
                   ISNULL(SPCFILE.SPC_RBDTE_14_2,''),
                   SPCFILE.SPC_RBDAYS_14_2,
                   SPCFILE.SPC_RBYRTYPE_14_2,
                   SPCFILE.SPC_RBMINRET_14,
                   SPCFILE.SPC_RBNOMON_14,
                   SPCFILE.SPC_RBDEDCHG_14,
                   SPCFILE.SPC_RBRECAST_14,
                   SPCFILE.SPC_RBEARLY_14,
                   SPCFILE.SPC_RBDEFER_14,
                   SPCFILE.SPC_RBPCT_14,
                   SPCFILE.SPC_RBREDUC_14,
                   SPCFILE.SPC_RBMIN_14,
                   SPCFILE.SPC_RBSPOPT1_14,
                   SPCFILE.SPC_RBSPOPT2_14,
                   SPCFILE.SPC_RBMANAGER_14,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_14,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_14,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_15,''),
                   ISNULL(SPCFILE.SPC_RBDTE_15_1,''),
                   SPCFILE.SPC_RBDAYS_15_1,
                   SPCFILE.SPC_RBYRTYPE_15_1,
                   ISNULL(SPCFILE.SPC_RBDTE_15_2,''),
                   SPCFILE.SPC_RBDAYS_15_2,
                   SPCFILE.SPC_RBYRTYPE_15_2,
                   SPCFILE.SPC_RBMINRET_15,
                   SPCFILE.SPC_RBNOMON_15,
                   SPCFILE.SPC_RBDEDCHG_15,
                   SPCFILE.SPC_RBRECAST_15,
                   SPCFILE.SPC_RBEARLY_15,
                   SPCFILE.SPC_RBDEFER_15,
                   SPCFILE.SPC_RBPCT_15,
                   SPCFILE.SPC_RBREDUC_15,
                   SPCFILE.SPC_RBMIN_15,
                   SPCFILE.SPC_RBSPOPT1_15,
                   SPCFILE.SPC_RBSPOPT2_15,
                   SPCFILE.SPC_RBMANAGER_15,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_15,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_15,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_16,''),
                   ISNULL(SPCFILE.SPC_RBDTE_16_1,''),
                   SPCFILE.SPC_RBDAYS_16_1,
                   SPCFILE.SPC_RBYRTYPE_16_1,
                   ISNULL(SPCFILE.SPC_RBDTE_16_2,''),
                   SPCFILE.SPC_RBDAYS_16_2,
                   SPCFILE.SPC_RBYRTYPE_16_2,
                   SPCFILE.SPC_RBMINRET_16,
                   SPCFILE.SPC_RBNOMON_16,
                   SPCFILE.SPC_RBDEDCHG_16,
                   SPCFILE.SPC_RBRECAST_16,
                   SPCFILE.SPC_RBEARLY_16,
                   SPCFILE.SPC_RBDEFER_16,
                   SPCFILE.SPC_RBPCT_16,
                   SPCFILE.SPC_RBREDUC_16,
                   SPCFILE.SPC_RBMIN_16,
                   SPCFILE.SPC_RBSPOPT1_16,
                   SPCFILE.SPC_RBSPOPT2_16,
                   SPCFILE.SPC_RBMANAGER_16,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_16,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_16,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_17,''),
                   ISNULL(SPCFILE.SPC_RBDTE_17_1,''),
                   SPCFILE.SPC_RBDAYS_17_1,
                   SPCFILE.SPC_RBYRTYPE_17_1,
                   ISNULL(SPCFILE.SPC_RBDTE_17_2,''),
                   SPCFILE.SPC_RBDAYS_17_2,
                   SPCFILE.SPC_RBYRTYPE_17_2,
                   SPCFILE.SPC_RBMINRET_17,
                   SPCFILE.SPC_RBNOMON_17,
                   SPCFILE.SPC_RBDEDCHG_17,
                   SPCFILE.SPC_RBRECAST_17,
                   SPCFILE.SPC_RBEARLY_17,
                   SPCFILE.SPC_RBDEFER_17,
                   SPCFILE.SPC_RBPCT_17,
                   SPCFILE.SPC_RBREDUC_17,
                   SPCFILE.SPC_RBMIN_17,
                   SPCFILE.SPC_RBSPOPT1_17,
                   SPCFILE.SPC_RBSPOPT2_17,
                   SPCFILE.SPC_RBMANAGER_17,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_17,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_17,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_18,''),
                   ISNULL(SPCFILE.SPC_RBDTE_18_1,''),
                   SPCFILE.SPC_RBDAYS_18_1,
                   SPCFILE.SPC_RBYRTYPE_18_1,
                   ISNULL(SPCFILE.SPC_RBDTE_18_2,''),
                   SPCFILE.SPC_RBDAYS_18_2,
                   SPCFILE.SPC_RBYRTYPE_18_2,
                   SPCFILE.SPC_RBMINRET_18,
                   SPCFILE.SPC_RBNOMON_18,
                   SPCFILE.SPC_RBDEDCHG_18,
                   SPCFILE.SPC_RBRECAST_18,
                   SPCFILE.SPC_RBEARLY_18,
                   SPCFILE.SPC_RBDEFER_18,
                   SPCFILE.SPC_RBPCT_18,
                   SPCFILE.SPC_RBREDUC_18,
                   SPCFILE.SPC_RBMIN_18,
                   SPCFILE.SPC_RBSPOPT1_18,
                   SPCFILE.SPC_RBSPOPT2_18,
                   SPCFILE.SPC_RBMANAGER_18,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_18,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_18,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_19,''),
                   ISNULL(SPCFILE.SPC_RBDTE_19_1,''),
                   SPCFILE.SPC_RBDAYS_19_1,
                   SPCFILE.SPC_RBYRTYPE_19_1,
                   ISNULL(SPCFILE.SPC_RBDTE_19_2,''),
                   SPCFILE.SPC_RBDAYS_19_2,
                   SPCFILE.SPC_RBYRTYPE_19_2,
                   SPCFILE.SPC_RBMINRET_19,
                   SPCFILE.SPC_RBNOMON_19,
                   SPCFILE.SPC_RBDEDCHG_19,
                   SPCFILE.SPC_RBRECAST_19,
                   SPCFILE.SPC_RBEARLY_19,
                   SPCFILE.SPC_RBDEFER_19,
                   SPCFILE.SPC_RBPCT_19,
                   SPCFILE.SPC_RBREDUC_19,
                   SPCFILE.SPC_RBMIN_19,
                   SPCFILE.SPC_RBSPOPT1_19,
                   SPCFILE.SPC_RBSPOPT2_19,
                   SPCFILE.SPC_RBMANAGER_19,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_19,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_19,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_20,''),
                   ISNULL(SPCFILE.SPC_RBDTE_20_1,''),
                   SPCFILE.SPC_RBDAYS_20_1,
                   SPCFILE.SPC_RBYRTYPE_20_1,
                   ISNULL(SPCFILE.SPC_RBDTE_20_2,''),
                   SPCFILE.SPC_RBDAYS_20_2,
                   SPCFILE.SPC_RBYRTYPE_20_2,
                   SPCFILE.SPC_RBMINRET_20,
                   SPCFILE.SPC_RBNOMON_20,
                   SPCFILE.SPC_RBDEDCHG_20,
                   SPCFILE.SPC_RBRECAST_20,
                   SPCFILE.SPC_RBEARLY_20,
                   SPCFILE.SPC_RBDEFER_20,
                   SPCFILE.SPC_RBPCT_20,
                   SPCFILE.SPC_RBREDUC_20,
                   SPCFILE.SPC_RBMIN_20,
                   SPCFILE.SPC_RBSPOPT1_20,
                   SPCFILE.SPC_RBSPOPT2_20,
                   SPCFILE.SPC_RBMANAGER_20,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_20,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_20,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_21,''),
                   ISNULL(SPCFILE.SPC_RBDTE_21_1,''),
                   SPCFILE.SPC_RBDAYS_21_1,
                   SPCFILE.SPC_RBYRTYPE_21_1,
                   ISNULL(SPCFILE.SPC_RBDTE_21_2,''),
                   SPCFILE.SPC_RBDAYS_21_2,
                   SPCFILE.SPC_RBYRTYPE_21_2,
                   SPCFILE.SPC_RBMINRET_21,
                   SPCFILE.SPC_RBNOMON_21,
                   SPCFILE.SPC_RBDEDCHG_21,
                   SPCFILE.SPC_RBRECAST_21,
                   SPCFILE.SPC_RBEARLY_21,
                   SPCFILE.SPC_RBDEFER_21,
                   SPCFILE.SPC_RBPCT_21,
                   SPCFILE.SPC_RBREDUC_21,
                   SPCFILE.SPC_RBMIN_21,
                   SPCFILE.SPC_RBSPOPT1_21,
                   SPCFILE.SPC_RBSPOPT2_21,
                   SPCFILE.SPC_RBMANAGER_21,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_21,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_21,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA_22,''),
                   ISNULL(SPCFILE.SPC_RBDTE_22_1,''),
                   SPCFILE.SPC_RBDAYS_22_1,
                   SPCFILE.SPC_RBYRTYPE_22_1,
                   ISNULL(SPCFILE.SPC_RBDTE_22_2,''),
                   SPCFILE.SPC_RBDAYS_22_2,
                   SPCFILE.SPC_RBYRTYPE_22_2,
                   SPCFILE.SPC_RBMINRET_22,
                   SPCFILE.SPC_RBNOMON_22,
                   SPCFILE.SPC_RBDEDCHG_22,
                   SPCFILE.SPC_RBRECAST_22,
                   SPCFILE.SPC_RBEARLY_22,
                   SPCFILE.SPC_RBDEFER_22,
                   SPCFILE.SPC_RBPCT_22,
                   SPCFILE.SPC_RBREDUC_22,
                   SPCFILE.SPC_RBMIN_22,
                   SPCFILE.SPC_RBSPOPT1_22,
                   SPCFILE.SPC_RBSPOPT2_22,
                   SPCFILE.SPC_RBMANAGER_22,
                   ISNULL(SPCFILE.SPC_RBUNITPER_CD_22,''),
                   ISNULL(SPCFILE.SPC_RBFRMLA2_22,''),
                   ISNULL(SPCFILE.SPC_RBCOMCD,''),
                   ISNULL(SPCFILE.SPC_ADDON_DISFRMLA,''),
                   ISNULL(SPCFILE.SPC_ADDON_WHOLE_FG,''),
                   ISNULL(SPCFILE.SPC_ADDON_ORG_MATURITY,''),
                   ISNULL(SPCFILE.SPC_CONSOLIDATE_RT_FG,''),
                   SPCFILE.SPC_1STPY_ADDON_DAYS,
                   SPCFILE.SPC_1STPY_ADDON_YRTYPE,
                   ISNULL(SPCFILE.SPC_ADDON_RATE_FG,''),
                   SPCFILE.SPC_ADDON_RATE,
                   ISNULL(SPCFILE.SPC_FEEFRMLA_1,''),
                   ISNULL(SPCFILE.SPC_FEECD_1,''),
                   ISNULL(SPCFILE.SPC_FEEID_1,''),
                   ISNULL(SPCFILE.SPC_FEEFRMLA_2,''),
                   ISNULL(SPCFILE.SPC_FEECD_2,''),
                   ISNULL(SPCFILE.SPC_FEEID_2,''),
                   ISNULL(SPCFILE.SPC_ROUND_PAY_FG,''),
                   ISNULL(SPCFILE.SPC_IRREGULAR_PAY_FG,''),
                   ISNULL(SPCFILE.SPC_DLRVCHGS_RB_CD_1,''),
                   ISNULL(SPCFILE.SPC_DLRVCHGS_RB_CD_2,''),
                   ISNULL(SPCFILE.SPC_DLRVCHGS_RB_CD_3,''),
                   ISNULL(SPCFILE.SPC_PARVHELD_RB_CD,''),
                   ISNULL(SPCFILE.SPC_PARVPAID_RB_CD,''),
                   SPCFILE.SPC_CONTR_CREDIT,
                   ISNULL(SPCFILE.SPC_CONTR_CREDIT_CD,''),
                   SPCFILE.SPC_CONTR_CREDIT_MIN_DEL_PER,
                   ISNULL(SPCFILE.SPC_OPT_CAL_CHGABLE,''),
                   SPCFILE.SPC_PBDEL_DAYS,
                   SPCFILE.SPC_PBINCR_PERCENT,
                   ISNULL(SPCFILE.SPC_CPOT30_CD,''),
                   ISNULL(SPCFILE.SPC_CPOTCHGOFF_CD,''),
                   ISNULL(SPCFILE.SPC_RDPOT30_CD,''),
                   ISNULL(SPCFILE.SPC_RDPOTCHGOFF_CD,''),
                   SPCFILE.SPC_MAX_ALLOW_DELINQ,
                   ISNULL(SPCFILE.SPC_RESCIND_BUS_DAYS,''),
                   ISNULL(SPCFILE.SPC_RESCIND_SATURDAY,''),
                   SPCFILE.SPC_MIN_CREDIT_SCORE,
                   ISNULL(SPCFILE.SPC_DUAL_IN_BRANCH,''),
                   ISNULL(SPCFILE.SPC_DUAL_IN_STATE,''),
                   ISNULL(SPCFILE.SPC_PACESETTER_FG,''),
                   SPCFILE.SPC_WAGE_ASSIGN_DAYS,
                   ISNULL(SPCFILE.SPC_CAL_PAYMENT_ROUND_FG,''),
                   ISNULL(SPCFILE.SPC_CAN_LOANS_FG,''),
                   ISNULL(SPCFILE.SPC_ALLOW_DELINQ_POLCD,''),
                   SPCFILE.SPC_YTD_MAX_ALLOW_DELINQ,
                   ISNULL(SPCFILE.SPC_LNCOST_FRMLA,''),
                   SPCFILE.SPC_LNCOST_AMT,
                   SPCFILE.SPC_DLDISC_RVPRIN1_PCNT,
                   SPCFILE.SPC_RENEWABLE_PAYS,
                   SPCFILE.SPC_RENEWABLE_DAYS,
                   ISNULL(SPCFILE.SPC_ALLOW_LN_BACK_DATE,''),
                   SPCFILE.SPC_DLDISC_RVPRIN1_AF_PCNT,
                   ISNULL(SPCFILE.SPC_CAL_MAX_AT_ESTIMATE,''),
                   ISNULL(SPCFILE.SPC_INSAH_RETRO,''),
                   ISNULL(SPCFILE.SPC_INSAH_DAYS_WAIT,''),
                   ISNULL(SPCFILE.SPC_ADDON_EXCEPT_FG,''),
                   ISNULL(SPCFILE.SPC_REPO_TRCD_1,''),
                   SPCFILE.SPC_REPO_FMNO_1_1,
                   SPCFILE.SPC_REPO_FMNO_1_2,
                   SPCFILE.SPC_REPO_FMNO_1_3,
                   SPCFILE.SPC_REPO_FMNO_1_4,
                   ISNULL(SPCFILE.SPC_REPO_TRCD_2,''),
                   SPCFILE.SPC_REPO_FMNO_2_1,
                   SPCFILE.SPC_REPO_FMNO_2_2,
                   SPCFILE.SPC_REPO_FMNO_2_3,
                   SPCFILE.SPC_REPO_FMNO_2_4,
                   ISNULL(SPCFILE.SPC_REPO_TRCD_3,''),
                   SPCFILE.SPC_REPO_FMNO_3_1,
                   SPCFILE.SPC_REPO_FMNO_3_2,
                   SPCFILE.SPC_REPO_FMNO_3_3,
                   SPCFILE.SPC_REPO_FMNO_3_4,
                   ISNULL(SPCFILE.SPC_CANCEL_ADDON_FG,''),
                   SPCFILE.SPC_DEFAULT_O2_INS_COMP,
                   ISNULL(SPCFILE.SPC_LC_ON_JUDGEMENTS,''),
                   SPCFILE.SPC_DELQ_IB_AGE,
                   SPCFILE.SPC_DELQ_IB_MAX_DAYS,
                   ISNULL(SPCFILE.SPC_INSURANCE_WRITTEN,''),
                   ISNULL(SPCFILE.SPC_INTERIM_COUPON,''),
                   ISNULL(SPCFILE.SPC_AUTO_ASSESS_LCHG,''),
                   SPCFILE.SPC_MAX_CASH_PROCEEDS,
                   ISNULL(SPCFILE.SPC_ALLOW_ZERO_PROC,''),
                   SPCFILE.SPC_BANKRULE_RATE,
                   ISNULL(SPCFILE.SPC_AUTO_SEND_TRCD_1,''),
                   SPCFILE.SPC_AUTO_SEND_FMNO_1_1,
                   SPCFILE.SPC_AUTO_SEND_FMNO_1_2,
                   SPCFILE.SPC_AUTO_SEND_FMNO_1_3,
                   SPCFILE.SPC_AUTO_SEND_FMNO_1_4,
                   ISNULL(SPCFILE.SPC_AUTO_SEND_TRCD_2,''),
                   SPCFILE.SPC_AUTO_SEND_FMNO_2_1,
                   SPCFILE.SPC_AUTO_SEND_FMNO_2_2,
                   SPCFILE.SPC_AUTO_SEND_FMNO_2_3,
                   SPCFILE.SPC_AUTO_SEND_FMNO_2_4,
                   SPCFILE.SPC_DLDISC_CONV_BF_PCNT,
                   ISNULL(SPCFILE.SPC_UNITPER_CD,''),
                   ISNULL(SPCFILE.SPC_RENEWAL_EDIT,''),
                   ISNULL(SPCFILE.SPC_ROUND_MFEE,''),
                   ISNULL(SPCFILE.SPC_ADV_TO_1ST_NO_CORR,''),
                   ISNULL(SPCFILE.SPC_LC_INT_ON_REPO_NON_PL,''),
                   SPCFILE.SPC_CONTRACTUAL_SET_NONACCRUAL,
                   SPCFILE.SPC_CONTRACTUAL_RESET_ACCRUAL,
                   SPCFILE.SPC_MONTHS_PAY_NAPP_COUNTER,
                   SPCFILE.SPC_SHORT_PAY_THRESHOLD,
                   SPCFILE.SPC_CASH_AVAIL_PERCENT,
                   ISNULL(SPCFILE.SPC_O1_JOINT_FG,''),
                   SPCFILE.SPC_NET_BAL_REQ,
                   SPCFILE.SPC_AMT_PROCEEDS_REQ,
                   SPCFILE.SPC_ALT_PREPAY_MONTHS,
                   SPCFILE.SPC_PERCENT_REGPAY_AVAIL,
                   SPCFILE.SPC_ALT_DEFFRMLA,
                   SPCFILE.SPC_ALT_DEFMIN,
                   SPCFILE.SPC_ALT_DEFMAX,
                   ISNULL(SPCFILE.SPC_NON_LOAN_FLAG,''),
                   ISNULL(SPCFILE.SPC_SKIP_TRW,'')
            INTO :QSPC-REC
            FROM DBO.SPCFILE
            WHERE SPCFILE.SPC_ORGST = :QSPC-ORGST
              AND SPCFILE.SPC_SPRCLASS = :QSPC-SPRCLASS
              AND SPCFILE.SPC_SUBCLASS = :QSPC-SUBCLASS
              AND SPCFILE.SPC_LAWCODE = :QSPC-LAWCODE 
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SPC1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-SPC-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-SPC1-FILE.
           PERFORM CLOSE-IT.	  
	  
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBGB/GBWK2IN.CPY".
      *----------------------------------------------------------------
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      *****************************************************************
       END-PROGRAM SECTION.
      *****************************************************************
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

      *----------------------------------------------------------------
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
              DESTROY EMWORG-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
