       IDENTIFICATION DIVISION.
       PROGRAM-ID.      AC2ERN.
       DATE-WRITTEN. 09-12-86
      ******************************************************************
      *
      *                        (LP)
      * DESCRIPTION:    MONTHLY EARNINGS NON ACCRUAL & P&L UNEARNED
      *                 CALLED FROM AC1ERN.C AND AC3ERN.C
      *
      *=================================================================
      * REV:
      * MJD 082995 WT-FILE IS GENERATED HERE BUT IS READ IN ACRERN.C
      *            TO PRINT TOTALS. LEVEL 1 = NON ACCRUAL
      *                                   2 = PL UNEARNED
      * MJD 091195 CHANGED PRINT FILE NAMES.
      *           EX: AC20895D.NONACCRU  AC20895R.PLACCRUE
      * MJD 100495 FIXED FILE PATHS FOR LX-FILE, LP-FILE, AND LT-FILE.
      *            RESET PAGE NUMBER BETWEEN REPORTS.
      * MJD 121096 CHANGED TO PERFORM LPSTAT-DISPLAY AFTER
      *            READING BWFILE.
      * BAH 121396 SKIP RESCISSION LOANS
      * MJD 091097 CHANGED SIZE OF WORKERS FROM S9(7)V99 TO S9(9)V99 TO
      *            ACCOMODATE GT-AMT
      * JTG 110697 INCREASED WORKERS FROM S9(7)V99 TO S9(9)V99
      *            AND DISPLAYS TO Z(8)9.99-
      * MJD 990730 ADDED GPENV(W). NEEDED FOR CHANGES TO LPEARN.
      * GLF 990923 REPLACED IO-FG NUMBERS WITH IO-FG 88 LEVEL PER DEVELPOMENT
      *           MEETING (082599)
      * BAH 140429 REPLACED RESERVED WORDS INT/DISC WITH WS- PREFIX, VERYANT
      * BLM 140206 FIX TO RUN IN BATCH PROCESSING, REGACC PR# 349
      * BAH 160125 REMOVE LTFILE, PD0003
      *  CS 160203 CHANGED WT-CLASS,WX-CLASS 9(3)
      * BAH 170316 ACTIVATE 8 DIGIT DATES, PD0006
      *
      * KEC 2017.0606 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          ADDED 'LN' FILE PREFIX FOR WORKING STORAGE FIELDS.
      *
      * BAH 181002 GLOBAL BWFILE
      *  CS 181010 SPFILE
      * KEC 2018-1009 CONVERTED LCFILE FOR A32 & A34
      * BAH 181203 GLOBAL LNFILE
      * BAH 190116 GLOBAL LXFILE, SPLIT
      * BAH 20190922 REMOVED ACCESS-CALL LNFILE, BW, SP, LC
      * BAH 20220803 HARD CODED START
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT WT-FILE ASSIGN WT-BUF
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WT1-KEY
                  ALTERNATE RECORD KEY WT2-KEY WITH DUPLICATES
                  FILE STATUS FILE-STAT.
           SELECT WX-FILE ASSIGN WX-BUF
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WX1-KEY
                  FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      /*****************************************************
      *   SUMMARY TOTALS WORK RECORD                       *
      ******************************************************
       FD  WT-FILE
           LABEL RECORDS ARE STANDARD.
       01  WT-REC.
           03  WT1-KEY.
               05  WT-BRANCH           PIC 9(4).
               05  WT2-KEY.
                   07  WT-CLASS        PIC 999.
           03  WT-DATA        OCCURS 2 TIMES.
               05  WT-NOACCTS          PIC 9(6)      COMP-3.
               05  WT-UNEARN           PIC S9(9)V99  COMP-3.
               05  WT-INT              PIC S9(9)V99  COMP-3.
               05  WT-INT-RB           PIC S9(9)V99  COMP-3.
               05  WT-ADDON            PIC S9(9)V99  COMP-3.
               05  WT-SRV              PIC S9(9)V99  COMP-3.
               05  WT-SRV-RB           PIC S9(9)V99  COMP-3.
               05  WT-MFEE             PIC S9(9)V99  COMP-3.
               05  WT-MFEE-RB          PIC S9(9)V99  COMP-3.
               05  WT-DISC             PIC S9(9)V99  COMP-3.
               05  WT-OTHER            PIC S9(9)V99  COMP-3.
               05  WT-ACCRUED          PIC S9(9)V99  COMP-3.
               05  WT-ENDING           PIC S9(9)V99  COMP-3.

      **********************************************************
      *   KEY FILE FOR NON ACCRUAL INCOME & PL UNEARNED REPORTS
      *       WX-TYPE:  N - NON ACCRUAL REPORT
      *                 P - P&L UNEARNED REPORT
      **********************************************************
       FD  WX-FILE
           LABEL RECORDS ARE STANDARD.
       01  WX-REC.
           03  WX1-KEY.
               05  WX-TYPE             PIC X.
               05  WX-OWNBR            PIC 9(4)       COMP-3.
               05  WX-CLASS            PIC 999.
               05  WX-ACCTNO           PIC 9(8)       COMP-3.
           03  WX-LN-NONACRERN         PIC S9(9)V99   COMP-3.
           03  WX-LN-PRIOR-UNEARNED    PIC S9(9)V99   COMP-3.
           03  WX-EARNINGS-WORKERS.
               05  WX-EARN-TOT-ANTICERN   PIC S9(9)V99   COMP-3.
               05  WX-EARN-TOT-ANTICADJ   PIC S9(9)V99   COMP-3.
               05  WX-EARN-TOT-ACCERN  PIC S9(9)V99   COMP-3.
               05  WX-EARN-TOT-UFERN   PIC S9(9)V99   COMP-3.
               05  WX-EARN-TOT-UNEARN  PIC S9(9)V99   COMP-3.
               05  WX-EARN-UNEARN      OCCURS 6 TIMES
                                       PIC S9(9)V99   COMP-3.
               05  WX-EARN             PIC 9.

      **********************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)LP/AC2ERN S35 02/07/24-11:04:53 >".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01LXE.CPY".
       COPY "LIBLP/LP01LXE_SQL.CPY".
       COPY "LIBLP/LPWSLXE.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       01  PR-IFN                      PIC X(2)    VALUE "PR".
       01  WT1-IFN                     PIC X(3)    VALUE "WT1".
       01  WT2-IFN                     PIC X(3)    VALUE "WT2".
       01  WX1-IFN                     PIC X(3)    VALUE "WX1".


       01  WX-BUF                      PIC X(35).
       01  WT-BUF                      PIC X(35).
      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED                 PIC 99       VALUE 0.
           03  LN-COUNT                PIC 999      VALUE 99.
       01  PAGE-NUMBER                 PIC 9(4)     VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  DISPLAY-MESS.
           03  FILLER                  PIC X(18)    VALUE
           "IN PROGRESS.......".
           03  DISPLAY-MESS-BR         PIC 9(4).
           03  FILLER                  PIC X        VALUE " ".
           03  DISPLAY-MESS-CL         PIC 99  BLANK ZERO.
           03  FILLER                  PIC X        VALUE " ".
           03  DISPLAY-MESS-LN         PIC 9(6)  BLANK ZERO.
       01  DISPLAY-LIST                PIC X(6)     VALUE "MESSX.".

       01  NON-ACCRUAL-DATE            PIC 9(8).
       01  LAST-CLASS-READ             PIC 9(3)     VALUE 0.
       01  FIRST-TIME                  PIC X        VALUE "Y".
       01  SUMMARY-FG                  PIC X        VALUE "N".
       01  BR-TOTALS                   PIC X        VALUE "N".
       01  MESS-LIST                   PIC X(5)     VALUE "MESS.".
       01  NAME-WORKER                 PIC X(34).
       01  FIRST-NAME                  PIC X(16).
       01  HOLD-LN-CLASS               PIC 9(3)     VALUE 0.
       01  ONE                         PIC S9        COMP.
       01  UNEARN                      PIC S9(9)V99  COMP VALUE 0.
       01  ENDING                      PIC S9(9)V99  COMP VALUE 0.
       01  ACCRUED                     PIC S9(9)V99  COMP VALUE 0.
       01  INT-RB                      PIC S9(9)V99  COMP VALUE 0.
       01  SRV-RB                      PIC S9(9)V99  COMP VALUE 0.
       01  MFEE-RB                     PIC S9(9)V99  COMP VALUE 0.
       01  SUB                         PIC 9(6) COMP.
       01  LEV1                        PIC 9(6) COMP.

       01  END-DATE-MMYY               PIC 9(4).
       01  FILLER            REDEFINES END-DATE-MMYY.
           03  END-MONTH               PIC 99.
           03  END-YEAR                PIC 99.

       01  W-LOAN-NUM.
           03  FILLER                  PIC XXX.
           03  LOAN-NUM                PIC X(13).

       01  EARN-WORKERS.
           03  WS-INT                   PIC S9(9)V99  COMP VALUE 0.
           03  ADDON                   PIC S9(9)V99  COMP VALUE 0.
           03  SRV                     PIC S9(9)V99  COMP VALUE 0.
           03  MFEE                    PIC S9(9)V99  COMP VALUE 0.
           03  WS-DISC                  PIC S9(9)V99  COMP VALUE 0.
           03  W-OTHER                 PIC S9(9)V99  COMP VALUE 0.
       01  EARN-WORK-TABLE   REDEFINES EARN-WORKERS.
           03  EARN-WORK-TAB           OCCURS 6 TIMES
                                       PIC S9(9)V99  COMP.


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

       COPY "LIBGB/EOBUF_EXT.CPY".
       01  MONTH-END-DATE              PIC 9(8)  VALUE 0.

       01  PRINT-PATH.
           03  PRINT-ALL-BASE          PIC X(09).
           03  FILLER                  PIC X     VALUE "/".
           03  PRINT-ALL-MACHINE       PIC XX.
           03  FILLER                  PIC X     VALUE "/".
           03  PRINT-ALL-DATA          PIC X(6).
           03  PRINT-ALL-PATH          PIC X(7)  VALUE "/ED/AC2".
           03  PRINT-ENDDATE           PIC 9(4).
           03  PRINT-ID                PIC X.
           03  FILLER                  PIC X     VALUE ".".
           03  PRINT-DESC              PIC X(8)  VALUE SPACES.


      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
       01  HEAD1.
           03  H-DATE                  PIC X(8)     VALUE " ".
           03  FILLER                  PIC XX       VALUE " ".
           03  H-HR                    PIC 99       VALUE 0.
           03  FILLER                  PIC X        VALUE ":".
           03  H-MIN                   PIC 99       VALUE 0.
           03  FILLER                  PIC X(30)    VALUE " ".
           03  H-NAME                  PIC X(40)    VALUE " ".
           03  H-RMEMO                 PIC X(38)    VALUE
           "          9.99R = AMOUNT OF REFUND".
           03  FILLER                  PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO               PIC ZZZ9.

       01  HEAD2.
           03  FILLER                  PIC X(10)    VALUE "USER ID:".
           03  H-USERID                PIC X(10).
           03  FILLER                  PIC X(17)    VALUE " ".
           03  H-TITLE                 PIC X(49)    VALUE
           "    NON ACCRUAL EARNINGS REPORT    MONTH ENDING: ".
           03  H-ENDDATE               PIC X(8).
           03  FILLER                  PIC X(38)    VALUE SPACE.

       01  HEAD3.
           03  FILLER                  PIC X(8)    VALUE "BRANCH :".
           03  FILLER                  PIC XX      VALUE " ".
           03  H-BRNO                  PIC 9(4).
           03  H-BRNOX       REDEFINES H-BRNO
                                       PIC X(4).
           03  FILLER                  PIC X        VALUE " ".
           03  H-BRDESC                PIC X(20).
           03  FILLER                  PIC X(97)    VALUE " ".

       01  HEAD4.
           03  FILLER                  PIC X(8)     VALUE "CLASS  :".
           03  FILLER                  PIC X       VALUE " ".
           03  H-LCNO                  PIC Z(3).
           03  FILLER                  PIC X        VALUE " ".
           03  H-LCDESC                PIC X(20).
           03  FILLER                  PIC X(99)    VALUE " ".

       01  HEAD5.
           03  FILLER                  PIC X(29)    VALUE SPACE.
           03  FILLER                  PIC X(30)    VALUE
               "                 BEGIN   -----".
           03  HEAD5-2                 PIC X(46)    VALUE
               "-------- MONTHLY NON ACCRUAL EARNINGS --------".
           03  HEAD5-3                 PIC X(28)    VALUE
               "-----    ACCRUAL    ENDING  ".
       01  HEAD5-2PL.
           03  FILLER                  PIC X(46)    VALUE
               "----------------  UNEARNED INCOME  -----------".

       01  HEAD6.
           03  FILLER                  PIC X(28)    VALUE
               "ACCOUNT NO.   BORROWER      ".
           03  FILLER                  PIC X(45)    VALUE
               "     DATE      UNEARNED  INTEREST    ADDONS  ".
           03  FILLER                  PIC X(38)    VALUE
           "  SERCHG  MAINTFEE   DL DISC     OTHER".
           03  HEAD6-3                 PIC X(20)    VALUE
           "  EARNINGS  UNEARNED".

       01  DETAIL-LINE                 PIC X(132) VALUE SPACES.
       01  D-LINE1           REDEFINES DETAIL-LINE.
           03  D-LOAN-NUM              PIC X(13).
           03  FILLER        REDEFINES D-LOAN-NUM.
               05  FILLER              PIC X(7).
               05  D-VOID              PIC X(6).
           03  FILLER                  PIC X.
           03  D-BWNAME                PIC X(13).
           03  FILLER                  PIC X.
           03  ANTIC-REPORT.
               05  FILLER              PIC XXX.
               05  D-DATE              PIC 99/99/99       BLANK ZERO.
               05  FILLER              PIC XXX.
               05  D-UNEARN            PIC ZZZZZ9.99-     BLANK ZERO.
               05  D-INT               PIC ZZZZZ9.99-     BLANK ZERO.
               05  FILLER    REDEFINES D-INT.
                   07  FILLER          PIC X(9).
                   07  D-INT-R         PIC X.
               05  D-ADDON             PIC ZZZZZ9.99-     BLANK ZERO.
               05  D-SRV               PIC ZZZZZ9.99-     BLANK ZERO.
               05  FILLER    REDEFINES D-SRV.
                   07  FILLER          PIC X(9).
                   07  D-SRV-R PIC X.
               05  D-MFEE              PIC ZZZZZ9.99-     BLANK ZERO.
               05  FILLER   REDEFINES D-MFEE.
                   07  FILLER          PIC X(9).
                   07  D-MFEE-R        PIC X.
               05  D-DISC              PIC ZZZZZ9.99-     BLANK ZERO.
               05  D-OTHER             PIC ZZZZZ9.99-     BLANK ZERO.
               05  D-ACCRUED           PIC ZZZZZ9.99-     BLANK ZERO.
               05  D-ENDING            PIC ZZZZZ9.99-     BLANK ZERO.

       01  CLASS-TOTAL       REDEFINES DETAIL-LINE.
           03  D-TAG                   PIC X(25).
           03  D-T-NOACCTS-T           PIC ZZZZZ9.
           03  D-T-TAG-T               PIC X.
           03  FILLER                  PIC X(7).
           03  D-T-ANTIC.
               05  D-T-UNEARN-T        PIC Z(8)9.99-   BLANK ZERO.
               05  FILLER              PIC X(7).
               05  D-T-ADDON-T         PIC Z(8)9.99-   BLANK ZERO.
               05  FILLER              PIC X(7).
               05  D-T-MFEE-T          PIC Z(8)9.99-   BLANK ZERO.
               05  FILLER    REDEFINES D-T-MFEE-T.
                   07  FILLER          PIC X(12).
                   07  D-T-MFEE-T-R    PIC X.
               05  FILLER              PIC X(7).
               05  D-T-OTHER-T         PIC Z(8)9.99-   BLANK ZERO.
               05  FILLER              PIC X(7).
               05  D-T-ENDING-T        PIC Z(8)9.99-   BLANK ZERO.

       01  CLASS-TOTAL-2     REDEFINES DETAIL-LINE.
           03  FILLER                  PIC X(50).
           03  D-T-INT-T               PIC Z(8)9.99-   BLANK ZERO.
           03  FILLER       REDEFINES D-T-INT-T.
               05  FILLER              PIC X(12).
               05  D-T-INT-T-R         PIC X.
           03  FILLER                  PIC X(7).
           03  D-T-SRV-T               PIC Z(8)9.99-   BLANK ZERO.
           03  FILLER        REDEFINES D-T-SRV-T.
               05  FILLER              PIC X(12).
               05  D-T-SRV-T-R         PIC X.
           03  FILLER                  PIC X(7).
           03  D-T-DISC-T              PIC Z(8)9.99-   BLANK ZERO.
           03  FILLER                  PIC X(7).
           03  D-T-ACCRUED-T           PIC Z(8)9.99-   BLANK ZERO.


       01  TOTAL-LINE        REDEFINES DETAIL-LINE.
           03  D-T-DESC                PIC X(20).
           03  FILLER        REDEFINES D-T-DESC.
               05  FILLER              PIC XXX.
               05  D-T-CLASS           PIC 99.
               05  FILLER              PIC X(8).
               05  D-T-NOACCTS         PIC ZZZZZ9.
               05  D-T-TAG             PIC X.
           03  D-T-ANT.
               05  D-T-UNEARN          PIC Z(8)9.99-   BLANK ZERO.
               05  D-T-INT             PIC Z(8)9.99-   BLANK ZERO.
               05  FILLER    REDEFINES D-T-INT.
                   07  FILLER          PIC X(12).
                   07  D-T-INT-R       PIC X.
               05  D-T-ADDON           PIC ZZZZZZZ9.99-   BLANK ZERO.
               05  D-T-SRV             PIC ZZZZZZZ9.99-   BLANK ZERO.
               05  FILLER    REDEFINES D-T-SRV.
                   07  FILLER          PIC X(11).
                   07  D-T-SRV-R       PIC X.
               05  D-T-MFEE            PIC ZZZZZZZ9.99-   BLANK ZERO.
               05  FILLER    REDEFINES D-T-MFEE.
                   07  FILLER          PIC X(11).
                   07  D-T-MFEE-R      PIC X.
               05  D-T-DISC            PIC Z(8)9.99-   BLANK ZERO.
               05  D-T-OTHER           PIC ZZZZZZZ9.99-   BLANK ZERO.
               05  D-T-ACCRUED         PIC ZZZZZZZ9.99-   BLANK ZERO.
               05  D-T-ENDING          PIC Z(8)9.99-   BLANK ZERO.
       01  SUMMARY1.
           03  FILLER                  PIC X(11) VALUE
               "* SUMMARY *".
           03  FILLER                  PIC X(108) VALUE " ".

       01  SUMMARY2.
           03  FILLER                  PIC X(21) VALUE SPACES.
           03  FILLER                  PIC X(14) VALUE
               "NO       BEGIN".
           03  SUMMARY2-2              PIC X(42) VALUE
               "    ------------------ MONTHLY NON ACCRUAL".
           03  SUMMARY2-3              PIC X(33) VALUE
               " EARNINGS --------------------  ".
           03  SUMMARY2-4              PIC X(21) VALUE
               "  ACCRUAL      ENDING".

       01  SUMMARY2-2PL                PIC X(43) VALUE
               "     ----------------------  UNEARNED INCOM".
       01  SUMMARY2-3PL                PIC X(32) VALUE
               "E  -------------------------".

       01  SUMMARY3.
           03  FILLER                  PIC X(32) VALUE
               "CLASS           ACCTS   UNEARNED".
           03  FILLER                  PIC X(52) VALUE
               "     INTEREST      ADDONS      SERCHG    MAINTFEE   ".
           03  FILLER                  PIC X(22) VALUE
               "   DL DISC       OTHER".
           03  SUMMARY3-1              PIC X(25) VALUE
               "    EARNINGS     UNEARNED".



       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPEARNW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      /***************************************
      *                                      *
      *     P R O G R A M   C O N T R O L    *
      *                                      *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       01  XWX-BUF            PIC X(35).
       01  AC1-WORKER.
           03  HOLD-BR        PIC 9(4).
           03  HOLD-DATE-MMYY PIC 9(4).
           03  XWK-BUF        PIC X(35).
           03  XWT-BUF        PIC X(35).
           03  XWX-RUNTYPE    PIC X.

       PROCEDURE DIVISION
            USING FORM-PATHNAME XWX-BUF AC1-WORKER EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WT-FILE WX-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LXE1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE
               WT-FILE WX-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE XWX-BUF TO WX-BUF.
           MOVE XWT-BUF TO WT-BUF.
           MOVE HOLD-DATE-MMYY TO END-DATE-MMYY.
           MOVE SPACES TO EXT-EOBUF-ERRCD.

           MOVE HOLD-BR TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           PERFORM GET-GPENV.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE
                                              PRINT-ALL-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE
                                              PRINT-ALL-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA
                                              PRINT-ALL-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              LP-PATH-OVERRIDE
                                              LXE-PATH-OVERRIDE.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           PERFORM SEND-ACCOUNT.
           MOVE 0 TO PAGE-NUMBER.
           IF XWX-RUNTYPE = "B" OR "N"
              MOVE 1 TO LEV1
              MOVE "PERFORMING NON-ACCRUAL-REPORT" TO ERRLOGW-EOM-MSG
              PERFORM LOG-ERROR
              PERFORM NON-ACCRUAL-REPORT.
           MOVE 0 TO PAGE-NUMBER.
           IF XWX-RUNTYPE = "B" OR "P"
              MOVE 2 TO LEV1
              MOVE "PERFORMING PL-UNEARNED-REPORT" TO ERRLOGW-EOM-MSG
              PERFORM LOG-ERROR
              PERFORM PL-UNEARNED-REPORT.
           GO TO END-PROGRAM.

      ****************************************
      *    NON ACCRUAL EARNINGS REPORT
      ****************************************
       NON-ACCRUAL-REPORT SECTION.
           PERFORM SET-PRINT-D.
           MOVE PRINT-PATH TO PRU-FILE.

           PERFORM OPEN-WT1-FILE.

           PERFORM CLEAR-TOTALS.

           MOVE MONTH-END-DATE     TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-ALPDATE.
           MOVE ALP-DATE           TO H-ENDDATE.
           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN1-FILE.

           PERFORM OPEN-WX1-FILE.
           MOVE "N" TO WX-TYPE.
           MOVE 0 TO WX-OWNBR WX-CLASS WX-ACCTNO.
           PERFORM START-WX1-FILE.

           MOVE "      NON ACCRUAL EARNINGS REPORT" TO MESS.
           PERFORM SEND-LEGEND.

       NON-ACCRUAL-REPORT-NEXT.
           PERFORM READ-WX1-FILE-NEXT.
           IF IO-BAD
            OR WX-TYPE NOT = "N"
               GO TO NON-ACCRUAL-REPORT-END.

           MOVE WX-EARNINGS-WORKERS TO EARNINGS-WORKERS.
           MOVE WX-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

      * SKIP LOANS IN RESCISSION AND LOANS THAT HAVE BEEN RESCINDED
           IF LN-POCD-INRESCISSION OR LN-POCD-RESCIND
              GO TO NON-ACCRUAL-REPORT-NEXT.

           MOVE LN-CLASS TO LC-CODE.
           PERFORM GET-LC.

           IF FIRST-TIME = "Y"
              MOVE LN-CLASS TO HOLD-LN-CLASS
              PERFORM SET-CLASS-HEADINGS
              PERFORM SET-BRANCH-HEADINGS.
           IF LN-CLASS NOT = HOLD-LN-CLASS
              PERFORM CLASS-TOTALS
              MOVE LN-CLASS TO HOLD-LN-CLASS
              PERFORM SET-CLASS-HEADINGS.

           PERFORM NON-ACCRUAL-PROCESS.

           GO TO NON-ACCRUAL-REPORT-NEXT.

       NON-ACCRUAL-REPORT-END.
           MOVE "Y" TO BR-TOTALS.
           PERFORM CLASS-TOTALS.
           PERFORM BRANCH-TOTALS.
      * TOTALS ARE READ BY ACRERN.C AND PRINTED THERE.
           PERFORM CLOSE-FILES.


      ****************************************
      *    PL UNEARNED REPORT
      ****************************************
       PL-UNEARNED-REPORT SECTION.
           MOVE "N" TO SUMMARY-FG.
           MOVE "Y" TO FIRST-TIME.
           PERFORM SET-PRINT-PL-D.
           MOVE PRINT-PATH TO PRU-FILE.
           PERFORM OPEN-WT1-FILE.

           PERFORM CLEAR-TOTALS.

           MOVE MONTH-END-DATE     TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-ALPDATE.
           MOVE ALP-DATE           TO H-ENDDATE.
           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN1-FILE.

           PERFORM OPEN-WX1-FILE.
           MOVE "P" TO WX-TYPE.
           MOVE 0 TO WX-OWNBR WX-CLASS WX-ACCTNO.
           PERFORM START-WX1-FILE.

           MOVE "       P&&L UNEARNED INCOME REPORT" TO MESS.
           PERFORM SEND-LEGEND.

       PL-UNEARNED-REPORT-NEXT.
           PERFORM READ-WX1-FILE-NEXT.
           IF IO-BAD
            OR WX-TYPE NOT = "P"
               GO TO PL-UNEARNED-REPORT-END.

           MOVE WX-EARNINGS-WORKERS TO EARNINGS-WORKERS.
           MOVE WX-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

      * SKIP LOANS IN RESCISSION AND LOANS THAT HAVE BEEN RESCINDED
           IF LN-POCD-INRESCISSION OR LN-POCD-RESCIND
              GO TO PL-UNEARNED-REPORT-NEXT.

           MOVE LN-CLASS TO LC-CODE.
           PERFORM GET-LC.

           IF FIRST-TIME = "Y"
              MOVE LN-CLASS TO HOLD-LN-CLASS
              PERFORM SET-CLASS-HEADINGS
              PERFORM SET-BRANCH-HEADINGS.
           IF LN-CLASS NOT = HOLD-LN-CLASS
              PERFORM CLASS-TOTALS
              MOVE LN-CLASS TO HOLD-LN-CLASS
              PERFORM SET-CLASS-HEADINGS.

      *M     PERFORM SEND-ACCOUNT.

           PERFORM PL-UNEARNED-PROCESS.
           GO TO PL-UNEARNED-REPORT-NEXT.


       PL-UNEARNED-REPORT-END.
           MOVE "Y" TO BR-TOTALS.
           PERFORM CLASS-TOTALS.
           PERFORM BRANCH-TOTALS.

      * TOTALS ARE READ BY ACRERN.C AND PRINTED THERE.
           PERFORM CLOSE-FILES.


      ***********************************
      *   I N I T I A L I Z A T I O N
      ***********************************
       INITIALIZATION SECTION.
           MOVE " " TO MESS.
           PERFORM SEND-LEGEND.

           MOVE END-MONTH TO DATE-MMDDYY-MM.
           MOVE END-YEAR  TO DATE-MMDDYY-YY.
           MOVE 50        TO DATE-MMDDYY-DD.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NDTE-DATE.
           PERFORM NEW-DATE-CALCULATION.
           MOVE NDTE-DATE TO MONTH-END-DATE.
           MOVE " " TO EXT-EOBUF-ERRCD.

           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-LC1-FILE.

      ***********************************
      *    NON ACCRUAL PROCESS
      ***********************************
       NON-ACCRUAL-PROCESS SECTION.
           IF FIRST-TIME = "Y"
              MOVE 1 TO LN-FEED
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE "N" TO FIRST-TIME.

           PERFORM GET-SP1-FILE.


      * COMPUTE NEW ACCRUAL EARNINGS NUMBERS:
           PERFORM FIND-NON-ACCRUAL-EARNINGS.

           MOVE WX-LN-PRIOR-UNEARNED TO UNEARN.

           PERFORM PROCESS-BORROWER.
           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO W-LOAN-NUM.
           MOVE LOAN-NUM TO D-LOAN-NUM.
           IF LN-POCD = "**"
              MOVE "(VOID)" TO D-VOID.
           MOVE NON-ACCRUAL-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-DATE.
           MOVE UNEARN TO D-UNEARN.
           MOVE WS-INT TO D-INT.
           MOVE ADDON TO D-ADDON.
           MOVE SRV TO D-SRV.
           MOVE MFEE TO D-MFEE.
           MOVE WS-DISC TO D-DISC.
           MOVE W-OTHER TO D-OTHER.
           IF LN-POCD = "**"
              MOVE 0 TO ACCRUED ENDING
           ELSE
              MOVE EARN-TOT-UNEARN TO ENDING
              PERFORM COMPUTE-EARNINGS
              COMPUTE ACCRUED = ENDING - EARN-TOT-UNEARN
              MOVE EARN-TOT-UNEARN TO ENDING.
           MOVE ACCRUED TO D-ACCRUED.
           MOVE ENDING TO D-ENDING.
           PERFORM WRITE-DETAIL-LINE.

           IF INT-RB = 0 AND SRV-RB = 0 AND MFEE-RB = 0
              NEXT SENTENCE
           ELSE
              MOVE INT-RB TO D-INT
              MOVE SRV-RB TO D-SRV
              MOVE MFEE-RB TO D-MFEE
              PERFORM SET-D-R
              PERFORM WRITE-DETAIL-LINE.

           PERFORM UPDATE-WT-FILE.
           ADD 1 TO T-NOACCTS(1) T-NOACCTS(2).
           ADD UNEARN TO T-UNEARN(1) T-UNEARN(2).
           ADD WS-INT TO T-INT(1) T-INT(2).
           ADD INT-RB TO T-INT-RB(1) T-INT-RB(2).
           ADD ADDON TO T-ADDON(1) T-ADDON(2).
           ADD SRV TO T-SRV(1) T-SRV(2).
           ADD SRV-RB TO T-SRV-RB(1) T-SRV-RB(2).
           ADD MFEE TO T-MFEE(1) T-MFEE(2).
           ADD MFEE-RB TO T-MFEE-RB(1) T-MFEE-RB(2).
           ADD WS-DISC TO T-DISC(1) T-DISC(2).
           ADD W-OTHER TO T-OTHER(1) T-OTHER(2).
           ADD ACCRUED TO T-ACCRUED(1) T-ACCRUED(2).
           ADD ENDING TO T-ENDING(1) T-ENDING(2).

      *******************************
       SET-D-R SECTION.
      *******************************
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
       PL-UNEARNED-PROCESS SECTION.
      *******************************
           IF FIRST-TIME = "Y"
              MOVE 1 TO LN-FEED
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE "N" TO FIRST-TIME.

           PERFORM GET-SP1-FILE.

           MOVE EARN-UNEARN(1) TO WS-INT.
           MOVE EARN-UNEARN(2) TO ADDON.
           MOVE EARN-UNEARN(3) TO SRV.
           MOVE EARN-UNEARN(4) TO MFEE.
           MOVE EARN-UNEARN(5) TO WS-DISC.
           MOVE EARN-UNEARN(6) TO W-OTHER.
           MOVE WX-LN-PRIOR-UNEARNED TO UNEARN.
           MOVE 0 TO ENDING.
           MOVE 0 TO INT-RB SRV-RB MFEE-RB.

           PERFORM PROCESS-BORROWER.
           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO W-LOAN-NUM.
           MOVE LOAN-NUM TO D-LOAN-NUM.
           MOVE LN-PLDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-DATE.
           MOVE UNEARN TO D-UNEARN.
           MOVE WS-INT TO D-INT.
           MOVE ADDON TO D-ADDON.
           MOVE SRV TO D-SRV.
           MOVE MFEE TO D-MFEE.
           MOVE WS-DISC TO D-DISC.
           MOVE W-OTHER TO D-OTHER.
           MOVE WX-LN-NONACRERN TO D-ACCRUED.
           MOVE ENDING TO D-ENDING.

           PERFORM WRITE-DETAIL-LINE.

           PERFORM UPDATE-WT-FILE.
           ADD 1 TO T-NOACCTS(1) T-NOACCTS(2).
           ADD UNEARN TO T-UNEARN(1) T-UNEARN(2).
           ADD WS-INT TO T-INT(1) T-INT(2).
           ADD INT-RB TO T-INT-RB(1) T-INT-RB(2).
           ADD ADDON TO T-ADDON(1) T-ADDON(2).
           ADD SRV TO T-SRV(1) T-SRV(2).
           ADD SRV-RB TO T-SRV-RB(1) T-SRV-RB(2).
           ADD MFEE TO T-MFEE(1) T-MFEE(2).
           ADD MFEE-RB TO T-MFEE-RB(1) T-MFEE-RB(2).
           ADD WS-DISC TO T-DISC(1) T-DISC(2).
           ADD W-OTHER TO T-OTHER(1) T-OTHER(2).
           ADD WX-LN-NONACRERN TO T-ACCRUED(1) T-ACCRUED(2).
           ADD ENDING TO T-ENDING(1) T-ENDING(2).

      *****************************************
       UPDATE-WT-FILE SECTION.
      *****************************************
       UPDATE-WT.
           MOVE LN-OWNBR TO WT-BRANCH.
           MOVE LN-CLASS TO WT-CLASS.
           PERFORM READ-WT1-FILE.
           IF IO-GOOD
              GO TO UPDATE-WT-FIELDS.

           MOVE " " TO WT-REC.
           MOVE LN-OWNBR TO WT-BRANCH.
           MOVE LN-CLASS TO WT-CLASS.
           MOVE 0 TO WT-NOACCTS(1) WT-UNEARN(1) WT-INT(1)
                     WT-INT-RB(1) WT-ADDON(1)
                     WT-SRV(1) WT-SRV-RB(1) WT-MFEE(1)
                     WT-MFEE-RB(1) WT-DISC(1)
                     WT-OTHER(1) WT-ACCRUED(1) WT-ENDING(1).

           MOVE 0 TO WT-NOACCTS(2) WT-UNEARN(2) WT-INT(2)
                     WT-INT-RB(2) WT-ADDON(2)
                     WT-SRV(2) WT-SRV-RB(2) WT-MFEE(2)
                     WT-MFEE-RB(2) WT-DISC(2)
                     WT-OTHER(2) WT-ACCRUED(2) WT-ENDING(2).
           PERFORM WRITE-WT1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.
           GO TO UPDATE-WT.

       UPDATE-WT-FIELDS.
           ADD 1 TO WT-NOACCTS(LEV1).
           ADD UNEARN TO WT-UNEARN(LEV1).
           ADD WS-INT TO WT-INT(LEV1).
           ADD INT-RB TO WT-INT-RB(LEV1).
           ADD ADDON TO WT-ADDON(LEV1).
           ADD SRV TO WT-SRV(LEV1).
           ADD SRV-RB TO WT-SRV-RB(LEV1).
           ADD MFEE TO WT-MFEE(LEV1).
           ADD MFEE-RB TO WT-MFEE-RB(LEV1).
           ADD WS-DISC TO WT-DISC(LEV1).
           ADD W-OTHER TO WT-OTHER(LEV1).
           IF WX-TYPE = "N"
              ADD ACCRUED TO WT-ACCRUED(LEV1)
           ELSE
              ADD WX-LN-NONACRERN TO WT-ACCRUED(LEV1).
           ADD ENDING TO WT-ENDING(LEV1).
           PERFORM REWRITE-WT1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.
       END-UPDATE-WT-FILE.
           EXIT.

      **************************************
       PROCESS-BORROWER SECTION.
      **************************************
           MOVE LN-SSNO(1) TO BW-SSNO.          
           PERFORM READ-BW1-FILE.
           IF NOT IO-GOOD
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
           ADD LN-FEED TO LN-COUNT.
           IF LN-COUNT > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC
           PERFORM WRITE-PR-REC.
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
       PRINT-TOTALS SECTION.
      ****************************************
           MOVE T-NOACCTS(SUB) TO D-T-NOACCTS-T.
           MOVE "#" TO D-T-TAG-T.
           MOVE T-UNEARN(SUB) TO D-T-UNEARN-T.
           MOVE T-ADDON(SUB) TO D-T-ADDON-T.
           MOVE T-MFEE(SUB) TO D-T-MFEE-T.
           MOVE T-OTHER(SUB) TO D-T-OTHER-T.
           MOVE T-ENDING(SUB) TO D-T-ENDING-T.
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
           MOVE T-ACCRUED(SUB) TO D-T-ACCRUED-T.
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
           MOVE T-UNEARN(SUB) TO D-T-UNEARN.
           MOVE T-ADDON(SUB) TO D-T-ADDON.
           MOVE T-MFEE(SUB) TO D-T-MFEE.
           MOVE T-OTHER(SUB) TO D-T-OTHER.
           MOVE T-INT(SUB) TO D-T-INT.
           MOVE T-SRV(SUB) TO D-T-SRV.
           MOVE T-DISC(SUB) TO D-T-DISC.
           MOVE T-ACCRUED(SUB) TO D-T-ACCRUED.
           MOVE T-ENDING(SUB) TO D-T-ENDING.
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
      *    FIND NON ACCRUAL EARNINGS
      *      AND AMOUNT OF RI, RS AND RM
      ****************************************
       FIND-NON-ACCRUAL-EARNINGS SECTION.
           MOVE 0 TO EARN-WORK-TAB(1) EARN-WORK-TAB(2)
                     EARN-WORK-TAB(3) EARN-WORK-TAB(4)
                     EARN-WORK-TAB(5) EARN-WORK-TAB(6).
           MOVE 0 TO INT-RB SRV-RB MFEE-RB.
           MOVE 0 TO NON-ACCRUAL-DATE.


           IF LN-POCD = "**"
              MOVE LN-POFFDATE TO NON-ACCRUAL-DATE
              MULTIPLY -1 BY LN-ACCERN(1) GIVING EARN-WORK-TAB(1)
              MULTIPLY -1 BY LN-ACCERN(2) GIVING EARN-WORK-TAB(2)
              MULTIPLY -1 BY LN-ACCERN(3) GIVING EARN-WORK-TAB(3)
              MULTIPLY -1 BY LN-ACCERN(4) GIVING EARN-WORK-TAB(4)
              MULTIPLY -1 BY LN-ACCERN(5) GIVING EARN-WORK-TAB(5)
              MULTIPLY -1 BY LN-ACCERN(6) GIVING EARN-WORK-TAB(6)
              GO TO END-FIND-NON-ACCRUAL-EARNINGS.

           MOVE 1 TO ONE.
           PERFORM OPEN-LP1-FILE.
           PERFORM OPEN-LXE1-FILE.

           MOVE LN-OWNBR      TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE ALL "9"       TO LP-SEQNO
                                 QLP1-WEND-SEQNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.

           PERFORM START-LP1-FILE-NOT-GREATER.

       FIND-NON-ACCRUAL-NEXT.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF IO-BAD OR
               (LP-BRNO NOT = LN-OWNBR) OR
                (LP-ACCTNO NOT = LN-ACCTNO) OR
                 (LP-TRCD = "BF")
              GO TO END-FIND-NON-ACCRUAL-EARNINGS.

           MOVE MONTH-END-DATE TO NUM-DATE.
           MOVE LP-TRDATE      TO SYS-DATE.
           IF SYS-DATE > NUM-DATE
              GO TO FIND-NON-ACCRUAL-NEXT
           ELSE
              MOVE 1 TO NUM-DA
              IF SYS-DATE < NUM-DATE
                 GO TO END-FIND-NON-ACCRUAL-EARNINGS.

           IF LP-TRCD = "RI" OR
                (LP-TRCD = "RV" AND LP-REFNO = "RI")
              ADD LP-TRAMT TO INT-RB
           ELSE
           IF LP-TRCD = "RS"
                   OR (LP-TRCD = "RV" AND LP-REFNO = "RS")
              ADD LP-TRAMT TO SRV-RB
           ELSE
           IF LP-TRCD = "RM"
                   OR (LP-TRCD = "RV" AND LP-REFNO = "RM")
              ADD LP-TRAMT TO MFEE-RB.

           MOVE LP-BRNO   TO LXE-BRNO.
           MOVE LP-ACCTNO TO LXE-ACCTNO.
           MOVE LP-SEQNO  TO LXE-SEQNO.
           PERFORM READ-LXE1-FILE.
           IF IO-BAD
              MOVE 0 TO LXE-EARN(1) LXE-EARN(2) LXE-EARN(3).

           MOVE 1 TO SUB.
           PERFORM ACCUMULATE-EARNED.
           MOVE LP-TRDATE TO NON-ACCRUAL-DATE.
           GO TO FIND-NON-ACCRUAL-NEXT.

       END-FIND-NON-ACCRUAL-EARNINGS.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LXE1-FILE.


      ****************************************
       ACCUMULATE-EARNED SECTION.
      ****************************************
           IF LN-EARN-ORDER(SUB) = 1
              COMPUTE EARN-WORK-TAB(SUB) =
                        EARN-WORK-TAB(SUB) + (LP-EARNED(1) * ONE)
           ELSE
           IF LN-EARN-ORDER(SUB) = 2
              COMPUTE EARN-WORK-TAB(SUB) =
                        EARN-WORK-TAB(SUB) + (LP-EARNED(2) * ONE)
           ELSE
           IF LN-EARN-ORDER(SUB) = 3
              COMPUTE EARN-WORK-TAB(SUB) =
                        EARN-WORK-TAB(SUB) + (LP-EARNED(3) * ONE)
           ELSE
           IF LN-EARN-ORDER(SUB) = 4
              COMPUTE EARN-WORK-TAB(SUB) =
                        EARN-WORK-TAB(SUB) + (LXE-EARN(1) * ONE)
           ELSE
           IF LN-EARN-ORDER(SUB) = 5
              COMPUTE EARN-WORK-TAB(SUB) =
                        EARN-WORK-TAB(SUB) + (LXE-EARN(2) * ONE)
           ELSE
           IF LN-EARN-ORDER(SUB) = 6
              COMPUTE EARN-WORK-TAB(SUB) =
                        EARN-WORK-TAB(SUB) + (LXE-EARN(3) * ONE).

           ADD 1 TO SUB.
           IF SUB < 7
              GO TO ACCUMULATE-EARNED.


      ***************************************************
      *    LOG ERROR IN GLOBAL LG DIRECTORY
      ***************************************************
       LOG-ERROR SECTION.
           MOVE "A" TO ERRLOGW-ACTION.
           MOVE BR-NO TO ERRLOGW-BRNO.
           MOVE FORM-PATHNAME TO ERRLOGW-PROGX.
           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBLP/LPEARN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *********************************************
       MISC-PARAGRAPHS SECTION.
      *********************************************
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETPR.CPY".
       SETUP-LINK-INFO.
       FORM-RESET.
       VDU-CONVERT-FIELD.
       GET-SP1-FILE.
           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE
              IF NOT IO-GOOD
                 GO TO IO-ERROR.

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
       SEND-ACCOUNT.
           MOVE LN-OWNBR TO DISPLAY-MESS-BR.
           MOVE LN-CLASS TO DISPLAY-MESS-CL.
           MOVE LN-ACCTNO TO DISPLAY-MESS-LN.
           MOVE DISPLAY-MESS TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       GET-LC.
           IF LC-CODE NOT = LAST-CLASS-READ
              PERFORM READ-LC1-FILE
              IF IO-BAD
                 MOVE "*** NOT FOUND ***" TO LC-DESC.
           MOVE LC-CODE TO LAST-CLASS-READ.
       SET-BRANCH-HEADINGS.
      *M     MOVE HOLD-BRNO TO H-BRNO.
      *M     PERFORM GET-BR.
           MOVE BR-NO TO H-BRNO.
           MOVE BR-NAME TO H-BRDESC.
       SET-CLASS-HEADINGS.
           MOVE HOLD-LN-CLASS TO H-LCNO LC-CODE.
           PERFORM GET-LC.
           MOVE LC-DESC TO H-LCDESC.
       SET-PL-BRANCH-HEADINGS.
      *M     MOVE HOLD-BRNO TO H-BRNO.
      *M     PERFORM GET-BR.
           MOVE BR-NO TO H-BRNO.
           MOVE BR-NAME TO H-BRDESC.
       SET-PL-CLASS-HEADINGS.
           MOVE HOLD-LN-CLASS TO H-LCNO LC-CODE.
           PERFORM GET-LC.
           MOVE LC-DESC TO H-LCDESC.

       SET-PRINT-D.
           MOVE "/ED/AC2" TO PRINT-ALL-PATH.
           MOVE END-DATE-MMYY TO PRINT-ENDDATE.
           MOVE "D" TO PRINT-ID.
           MOVE "NONACCRU" TO PRINT-DESC.

       SET-PRINT-S.
           MOVE "/ED/AC2" TO PRINT-ALL-PATH.
           MOVE END-DATE-MMYY TO PRINT-ENDDATE.
           MOVE "S" TO PRINT-ID.
           MOVE "NONACCRU" TO PRINT-DESC.

       SET-PRINT-PL-D.
           MOVE "/ED/AC2" TO PRINT-ALL-PATH.
           MOVE END-DATE-MMYY TO PRINT-ENDDATE.
           MOVE "R" TO PRINT-ID.
           MOVE "PLUNEARN" TO PRINT-DESC.
           MOVE " " TO H-RMEMO.
           MOVE
           "P&L UNEARNED INCOME REPORT          MONTH ENDING "
                                                  TO H-TITLE.
           MOVE HEAD5-2PL TO HEAD5-2.
           MOVE " " TO HEAD5-3 HEAD6-3.

       SET-PRINT-PL-S.
           MOVE "/ED/AC2" TO PRINT-ALL-PATH.
           MOVE END-DATE-MMYY TO PRINT-ENDDATE.
           MOVE "T" TO PRINT-ID.
           MOVE "PLUNEARN" TO PRINT-DESC.
           MOVE SUMMARY2-2PL TO SUMMARY2-2.
           MOVE SUMMARY2-3PL TO SUMMARY2-3.
           MOVE " " TO SUMMARY2-4 SUMMARY3-1.

       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************
       IO-ROUTINE SECTION.
      ****************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLXEGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPLP1RN.CPY".
       COPY "LIBLP/LPLXE1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       OPEN-WT1-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WT1-IFN TO E-FILE.
           OPEN OUTPUT WT-FILE.
           IF IO-LOCKED
             GO TO OPEN-WT1-FILE-OUTPUT.
           IF IO-ALREADY-OPEN
             CLOSE WT-FILE
             GO TO OPEN-WT1-FILE-OUTPUT.
           UNLOCK WT-FILE.
       OPEN-WT1-FILE.
           PERFORM OPEN-IT.
           MOVE WT1-IFN TO E-FILE.
           OPEN I-O WT-FILE.
           IF IO-LOCKED
             GO TO OPEN-WT1-FILE.
           IF IO-ALREADY-OPEN
             CLOSE WT-FILE
             GO TO OPEN-WT1-FILE.
           UNLOCK WT-FILE.
       READ-WT1-FILE.
           PERFORM READ-IT.
           MOVE WT1-IFN TO E-FILE.
           MOVE WT1-KEY TO E-KEYX.
           READ WT-FILE.
           IF IO-LOCKED
             GO TO READ-WT1-FILE.
       READ-WT1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE WT1-IFN TO E-FILE.
           MOVE WT1-KEY TO E-KEYX.
           READ WT-FILE NEXT.
           IF IO-LOCKED
             GO TO READ-WT1-FILE-NEXT.
       START-WT1-FILE.
           PERFORM START-IT.
           MOVE WT1-IFN TO E-FILE.
           MOVE WT1-KEY TO E-KEYX.
           START WT-FILE KEY NOT < WT1-KEY.
           UNLOCK WT-FILE.
       WRITE-WT1-FILE.
           PERFORM WRITE-IT.
           MOVE WT1-IFN TO E-FILE.
           MOVE WT1-KEY TO E-KEYX.
           WRITE WT-REC.
           IF IO-LOCKED
             GO TO WRITE-WT1-FILE.
           UNLOCK WT-FILE.
       REWRITE-WT1-FILE.
           PERFORM REWRITE-IT.
           MOVE WT1-IFN TO E-FILE.
           MOVE WT1-KEY TO E-KEYX.
           REWRITE WT-REC.
           IF IO-LOCKED
             GO TO REWRITE-WT1-FILE.
           UNLOCK WT-FILE.
       CLOSE-WT1-FILE.
           PERFORM CLOSE-IT.
           CLOSE WT-FILE.
       OPEN-WT2-FILE.
           PERFORM OPEN-IT.
           MOVE WT2-IFN TO E-FILE.
           OPEN I-O WT-FILE.
           IF IO-LOCKED
             GO TO OPEN-WT2-FILE.
           IF IO-ALREADY-OPEN
             CLOSE WT-FILE
             GO TO OPEN-WT2-FILE.
           UNLOCK WT-FILE.
       READ-WT2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE WT2-IFN TO E-FILE.
           MOVE WT2-KEY TO E-KEYX.
           READ WT-FILE NEXT.
           IF IO-LOCKED
             GO TO READ-WT2-FILE-NEXT.
       START-WT2-FILE.
           PERFORM START-IT.
           MOVE WT2-IFN TO E-FILE.
           MOVE WT2-KEY TO E-KEYX.
           START WT-FILE KEY NOT < WT2-KEY.
           UNLOCK WT-FILE.
       CLOSE-WT2-FILE.
           PERFORM CLOSE-IT.
           CLOSE WT-FILE.

       OPEN-WX1-FILE.
           PERFORM OPEN-IT.
           MOVE WX1-IFN TO E-FILE.
           OPEN I-O WX-FILE.
           IF IO-LOCKED
              GO TO OPEN-WX1-FILE.
           IF IO-ALREADY-OPEN
              CLOSE WX-FILE
              GO TO OPEN-WX1-FILE.
           UNLOCK WX-FILE.
       READ-WX1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE WX1-IFN TO E-FILE.
           MOVE WX1-KEY TO E-KEYX.
           READ WX-FILE NEXT.
           IF IO-LOCKED
              GO TO READ-WX1-FILE-NEXT.
           UNLOCK WX-FILE.
       START-WX1-FILE.
           PERFORM START-IT.
           MOVE WX1-IFN TO E-FILE.
           MOVE WX1-KEY TO E-KEYX.
           START WX-FILE KEY NOT < WX1-KEY.
           UNLOCK WX-FILE.
       CLOSE-WX1-FILE.
           PERFORM CLOSE-IT.
           CLOSE WX-FILE.

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ****************************************
       END-PROGRAM SECTION.
      ****************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
