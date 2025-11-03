       IDENTIFICATION DIVISION.
       PROGRAM-ID.    AC2DAY.
       AUTHOR.         CS.
       DATE-WRITTEN.  09-11-2017.
      ******************************************************************
      *
      * IDENTIFICATION:  LP/AC2DAY.C
      *                 (TAKEN FROM LP/AC1DAY)
      *
      * DESCRIPTION   :  MONTHLY EARNINGS ACCRUAL
      *                  IB EXCEPTIONS,PRECOMP,PRECOMP EXCEPTIONS.
      *
      *                 RUNS IN BATCH WITH ABILITY TO LOCAL SPOOL
      *                 (SP/EOEXEC), HAS GROUP LOGIC BUT NO GROUP TOTALS.
      *
      *                 PRINT ORDER FOR GROUP FROM MENU (LPR3MU)
      *                 IB FOR ALL BRANCHES, IB EXCEPTIONS FOR ALL BRANCHES
      *                 PRECOMP FOR ALL BRANCHES, PRECOMP EXCEPTIONS FOR
      *                 ALL BRANCHES IF ALL LOAN TYPE PRINTS
      *                 REQUESTED.  WORLD PR#1271
      * 
      * NOTE: THIS PROGRAM CREATES EXTRACTS OF THE DETAIL REPORTS
      *
      * EXPLANATION FROM CINDY;
      *
      * SEPARATE SECTIONS FOR PRECOMP AND IB.
      * PRECOMP USES THE ACCRUAL DATE AND COMPUTES EARNINGS(LPAERN)
      *   TAKES INTO ACCOUNT LN-ERNDELQ-STOP FOLLOWING ALL OTHER PARAMETERS.
      *   SKIPS P&L
      * IB COPUTES A MAX ACCRUAL BASED ON SP-MAX-IB-ACR-DAYS. IF THIS IS 0,
      *   ITS DONE WITH THIS LOAN. ALSO USES ACCRUAL DATE AND SET THAT AS
      *   THE RATE DATE AND PERFORMS SET-PY-RATE.
      *   THEN GENERATES THE IBX SETION WHICH ARE IB EXCEPTIONS - LOANS THAT
      *   THE TRUE ACCRUE EXCEEDS MAX IB INTEREST.
      * ALL THE ABOVE IS THEN LOADED INTO A REPORT FILE ONLY. NO UPDATES.
      *
      *=================================================================
      * REV:
      * BAH 2017-0316 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 2017-0316 ADDED LTR-REC TO RVCALC LINKAGE LIKE AC1ERN
      *  CS 2017-0912 ACCRUAL DATE WAS DEFAULTING TO 00/00/00 INSTEAD
      *            OF SYSTEM DATE. MOVE ACCRUAL-DATE TO
      *            VDU-DATE-YYYYMMDD.
      * BAH 2018-1002 GLOBAL BWFILE
      * BAH 2018-1010 GLOBAL DLFILE
      * BAH 2018-1203 GLOBAL LNFILE
      *  CS 2019-0103 GLOBAL LTIFILE
      *          COMMENT LTI-LOCAL-PATH/LTP-LOCAL-PATH
      *          KEN AND I COULDNT FIND IN USE
      *  CS 2019-0111 GLOBAL LTIFILE
      * KEC 2019-0131 CC32 - REMOVED THE REVOLVING CREDIT USAGE.
      * BAH 2019-0306 BRANCH RANGES WERE GETTING WIPED OUT AFTER RUNNING
      *          IT IN GROUP BATCH, WORLD PR#1362
      * BAH 2019-0424 WAS PRINTING LNAME, LNAME INSTEAD OF FNAME, QA0010
      * BAH 2019-0703 REMOVED LTR PT0150
      * BAH 2019-0922 REMOVED ACCESS-CALL LNFILE, BW, LTI AND LP
      * JKC 2020-1130 PR#1477 ADD FLAG TO SCREEN TO CREATE EXPORT FILE
      *          (Y/N/O), DEFAULT TO "N".  DEPENDING ON SECTIONS
      *          REQUESTED, UP TO 4 DIFFERENT DATA FILES WILL BE CREATED
      *          WITH ACCOUNT DETAIL : IB ACCRUAL, IB EXCEPTIONS,
      *          PRE-COMPUTE ACCRUAL, PRE-COMPUTE EXCEPTIONS (CURRENTLY
      *          RUN FOR PRE-COMPUTE EXCEPTIONS ONLY).  ADD PASSWORD
      *          SEQUENCE SO THAT END-USERS CANNOT CHANGE THE FLAG FROM
      *          "N", WE WILL LIMIT OPTION TO ADMIN LOGINS.
      * JKC 2021-0111 PR#1477 IN QA TESTING, IT WAS FOUND THAT THE
      *          OPTION OF EXPORT FILE (O)NLY DID NOT WORK UNLIKE THE
      *          OPTION TO INCLUDE THE EXPORT FILE WITH THE REPORT.
      *          HAD TO INCLUDE ADDITIONAL LOGIC TO PROCESS DOING AN
      *          OPTION OF EXPORT FILE ONLY RIGHT AFTER THE READ-NEXT
      *          OF THE REPORT FILE AND BEFORE THE CHECK IF THE RECORD
      *          IS A TOTALS RECORD AND TO SKIP IF NOT PRINTING THOSE
      *          TYPES OF RECORDS.
      * JKC 2021-0210 PR#1477A NEED ADDITIONAL INFORMATION ON EACH LINE.
      *          THE EXPORT FILE RECORDS (LINES) NEEDS BRANCH NUMBER,
      *          LOAN CLASS, AND SPLIT ACCOUNT NUMBER AND STATUS FLAGS
      *          INTO SEPERATE FIELDS.  SO LINES BEGAN AS:
      *             BRANCH/ACCOUNT NUMBER/STATUS FLAGS/LOAN CLASS
      *          REPLACED DISPLAY-ACCTNO AND DISPLAY-LNNO WITH
      *          WSS-LPS-DISPLAY-LNNO WHICH IS A REDEFINES FOR DIFFERENT
      *          FORMATS NEEDED IN POPULATING THE EXPORT FIELDS.
      * JKC 2021-0224 PR#1477A PER EMAIL OF ANDRIA SINCLAIR AND
      *          KEVIN MULLEN, ADD A SPACE BEFORE AC2DAY-IB-MAX.
      * JKC 2022-0809 PR#1565 ADDED LN-ACTIONCD2 TO THE END OF ALL FOUR
      *          EXTRACTION FILES.
      * MB 2023-0807 PERFORMANCE TUNING - ADDED STORED PROCEDURE TO RETRIEVE 
      *               LN, BW AND LC DATA IN ONE QUERY.
      *               ADDED STATE PARAMETER ARRAY PROCESSING
      * BAH 2024.0801 MOVED ARRAYSP-LOAD TO BE AFTER EOINIT, SCREEN WAS
      *               BLANK WHILE LOADING, THINK U ARE STUCK S35Q-106
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".


      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           SELECT AC2DAY-IB-FILE ASSIGN AC2DAY-IB-PATH
                  ORGANIZATION RELATIVE
                  ACCESS MODE DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RELATIVE KEY AC2DAY-IB-KEY
                  FILE STATUS FILE-STAT.

           SELECT AC2DAY-IBX-FILE ASSIGN AC2DAY-IBX-PATH
                  ORGANIZATION RELATIVE
                  ACCESS MODE DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RELATIVE KEY AC2DAY-IBX-KEY
                  FILE STATUS FILE-STAT.

           SELECT AC2DAY-PC-FILE ASSIGN AC2DAY-PC-PATH
                  ORGANIZATION RELATIVE
                  ACCESS MODE DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RELATIVE KEY AC2DAY-PC-KEY
                  FILE STATUS FILE-STAT.

           SELECT AC2DAY-PCX-FILE ASSIGN AC2DAY-PCX-PATH
                  ORGANIZATION RELATIVE
                  ACCESS MODE DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RELATIVE KEY AC2DAY-PCX-KEY
                  FILE STATUS FILE-STAT.

           SELECT REPORT-FILE ASSIGN REPORT-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY REPORT-KEY
                  FILE STATUS FILE-STAT.

      *-----------------------------------------------------------------
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".


      ******************************************************************
      *
      *    A C 2 D A Y - I B - F I L E
      *
      ******************************************************************
       FD  AC2DAY-IB-FILE
           LABEL RECORDS ARE STANDARD.
       01  AC2DAY-IB-REC.
           03  AC2DAY-IB-BRNO                    PIC 9(4).
           03  AC2DAY-IB-ACCTNO                  PIC 9(6).
           03  AC2DAY-IB-7STATUS                 PIC X(7).
           03  AC2DAY-IB-CLASS                   PIC 9(2).
           03  AC2DAY-IB-NAME                    PIC X(13).
           03  AC2DAY-IB-NUMBER-ACCTS            PIC 9(3).
           03  AC2DAY-IB-FROM-DATE               PIC 9(8).
           03  AC2DAY-IB-INT-SINCE-LSTPY         PIC 9(7).99.
           03  AC2DAY-IB-INTBAL                  PIC 9(7).99.
           03  AC2DAY-IB-TRUE-ACCRUE             PIC 9(7).99.
           03  AC2DAY-IB-ACCRUED                 PIC 9(6).99.
           03  FILLER                            PIC X(1).
           03  AC2DAY-IB-MAX                     PIC X(1).
           03  AC2DAY-IB-ACTIONCD2               PIC X(4).
           03  AC2DAY-IB-CR                      PIC X(1).
           03  AC2DAY-IB-LF                      PIC X(1).

      ******************************************************************
      *
      *    A C 2 D A Y - I B X - F I L E
      *
      ******************************************************************
       FD  AC2DAY-IBX-FILE
           LABEL RECORDS ARE STANDARD.
       01  AC2DAY-IBX-REC.
           03  AC2DAY-IBX-BRNO                   PIC 9(4).
           03  AC2DAY-IBX-ACCTNO                 PIC 9(6).
           03  AC2DAY-IBX-7STATUS                PIC X(7).
           03  AC2DAY-IBX-CLASS                  PIC 9(2).
           03  AC2DAY-IBX-NAME                   PIC X(13).
           03  AC2DAY-IBX-NUMBER-ACCTS           PIC 9(3).
           03  AC2DAY-IBX-INT-PDTH-DATE          PIC 9(8).
           03  AC2DAY-IBX-INT-SINCE-LSTPY        PIC 9(7).99.
           03  AC2DAY-IBX-INT-OWED               PIC 9(7).99.
           03  AC2DAY-IBX-TRUE-ACCRUE            PIC 9(7).99.
           03  AC2DAY-IBX-INCOME-ACCRUE          PIC 9(6).99.
           03  AC2DAY-IBX-BALANCE                PIC 9(7).99.
           03  AC2DAY-IBX-CONT-PDTH-DATE         PIC 9(8).
           03  AC2DAY-IBX-CONT-STATUS            PIC 9(3).
           03  AC2DAY-IBX-ACTIONCD2              PIC X(4).
           03  AC2DAY-IBX-CR                     PIC X(1).
           03  AC2DAY-IBX-LF                     PIC X(1).

      ******************************************************************
      *
      *    A C 2 D A Y - P C - F I L E
      *
      ******************************************************************
       FD  AC2DAY-PC-FILE
           LABEL RECORDS ARE STANDARD.
       01  AC2DAY-PC-REC.
           03  AC2DAY-PC-BRNO                    PIC 9(4).
           03  AC2DAY-PC-ACCTNO                  PIC 9(6).
           03  AC2DAY-PC-7STATUS                 PIC X(7).
           03  AC2DAY-PC-CLASS                   PIC 9(2).
           03  AC2DAY-PC-NAME                    PIC X(13).
           03  AC2DAY-PC-ANTERM                  PIC 9(3).
           03  AC2DAY-PC-ANTIC                   PIC +9(06).99.
           03  AC2DAY-PC-UNEARN                  PIC +9(06).99.
           03  AC2DAY-PC-INT                     PIC +9(06).99.
           03  AC2DAY-PC-ADDON                   PIC +9(06).99.
           03  AC2DAY-PC-SRV                     PIC +9(06).99.
           03  AC2DAY-PC-MFEE                    PIC +9(06).99.
           03  AC2DAY-PC-DISC                    PIC +9(06).99.
           03  AC2DAY-PC-OTHER                   PIC +9(06).99.
           03  AC2DAY-PC-CASH-EARN               PIC +9(06).99.
           03  AC2DAY-PC-ENDING                  PIC +9(06).99.
           03  AC2DAY-PC-ACTIONCD2               PIC X(4).
           03  AC2DAY-PC-CR                      PIC X(1).
           03  AC2DAY-PC-LF                      PIC X(1).

      ******************************************************************
      *
      *    A C 2 D A Y - P C X - F I L E
      *
      ******************************************************************
       FD  AC2DAY-PCX-FILE
           LABEL RECORDS ARE STANDARD.
       01  AC2DAY-PCX-REC.
           03  AC2DAY-PCX-BRNO                   PIC 9(4).
           03  AC2DAY-PCX-ACCTNO                 PIC 9(6).
           03  AC2DAY-PCX-7STATUS                PIC X(7).
           03  AC2DAY-PCX-CLASS                  PIC 9(2).
           03  AC2DAY-PCX-NAME                   PIC X(13).
           03  AC2DAY-PCX-ANTERM                 PIC 9(3).
           03  AC2DAY-PCX-ANTIC                  PIC +9(06)9.99-.
           03  AC2DAY-PCX-UNEARN                 PIC +9(06)9.99-.
           03  AC2DAY-PCX-INT                    PIC +9(06)9.99-.
           03  AC2DAY-PCX-ADDON                  PIC +9(06)9.99-.
           03  AC2DAY-PCX-SRV                    PIC +9(06)9.99-.
           03  AC2DAY-PCX-MFEE                   PIC +9(06)9.99-.
           03  AC2DAY-PCX-DISC                   PIC +9(06)9.99-.
           03  AC2DAY-PCX-OTHER                  PIC +9(06)9.99-.
           03  AC2DAY-PCX-ROLLBACK               PIC +9(06)9.99-.
           03  AC2DAY-PCX-CONTRACTUAL            PIC 9(04).
           03  AC2DAY-PCX-ACTIONCD2              PIC X(4).
           03  AC2DAY-PCX-CR                     PIC X(1).
           03  AC2DAY-PCX-LF                     PIC X(1).

      ******************************************************************
      *
      *    R E P O R T - F I L E
      *
      *-----------------------------------------------------------------
      *    FILE USED TO PRINT A REPORT
      ******************************************************************
       FD  REPORT-FILE
           LABEL RECORDS ARE STANDARD.

      *=================================================================
       01  REPORT-REC.
           03  REPORT-KEY.
               05  REPORT-TYPE                       PIC X(03).
                   88  REPORT-TYPE-IB                    VALUE "IB ".
                   88  REPORT-TYPE-IBX                   VALUE "IBX".
                   88  REPORT-TYPE-PC                    VALUE "PC ".
                   88  REPORT-TYPE-PCX                   VALUE "PCX".

               05  REPORT-BRANCH                     PIC 9(05).
                   88  REPORT-GRAND-TOTAL                VALUE 99999.

               05  REPORT-POOL                       PIC 9(06).
                   88  REPORT-BRANCH-TOTAL               VALUE 999999.

               05  REPORT-CLASS                      PIC 9(04).
                   88  REPORT-POOL-TOTAL                 VALUE 9999.

               05  REPORT-ACCTNO                     PIC 9(08).
                   88  REPORT-CLASS-TOTAL                VALUE 99999999.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           03  REPORT-REC-INFO                       PIC X(180).

      *-------------------------------------------------{ INT. BEARING }
           03  REPORT-IB-REC-INFO          REDEFINES REPORT-REC-INFO.
               05  REPORT-IB-DISPLAY-LNNO            PIC X(16).
               05  REPORT-IB-LNAME                   PIC X(16).
               05  REPORT-IB-FNAME                   PIC X(16).
               05  REPORT-IB-FROM-DATE               PIC 9(08).
               05  REPORT-IB-INT-SINCE-LSTPY         PIC S9(9)V99.
               05  REPORT-IB-INTBAL                  PIC S9(9)V99.
               05  REPORT-IB-TRUE-ACCRUE             PIC S9(9)V99.
               05  REPORT-IB-ACCRUED                 PIC S9(9)V99.
               05  REPORT-IB-MAX                     PIC X(01).
               05  REPORT-IB-NUMBER-ACCTS            PIC 9(07).
               05  REPORT-IB-ACTIONCD2               PIC X(04).
               05  FILLER                            PIC X(68).

      *-------------------------------------{ INT. BERARING EXCEPTIONS }
           03  REPORT-IBX-REC-INFO         REDEFINES REPORT-REC-INFO.
               05  REPORT-IBX-DISPLAY-LNNO           PIC X(16).
               05  REPORT-IBX-LNAME                  PIC X(16).
               05  REPORT-IBX-FNAME                  PIC X(16).
               05  REPORT-IBX-INT-PDTH-DATE          PIC 9(8).
               05  REPORT-IBX-INT-SINCE-LSTPY        PIC S9(9)V99.
               05  REPORT-IBX-INT-OWED               PIC S9(9)V99.
               05  REPORT-IBX-TRUE-ACCRUE            PIC S9(9)V99.
               05  REPORT-IBX-INCOME-ACCRUE          PIC S9(9)V99.
               05  REPORT-IBX-BALANCE                PIC S9(9)V99.
               05  REPORT-IBX-CONTR-PDTH-DATE        PIC 9(8).
               05  REPORT-IBX-CONTR-STATUS           PIC 9(03).
               05  REPORT-IBX-NUMBER-ACCTS           PIC 9(07).
               05  REPORT-IBX-ACTIONCD2              PIC X(04).
               05  FILLER                            PIC X(47).

      *---------------------------------------------------{ PRECOMPUTE }
           03  REPORT-PC-REC-INFO          REDEFINES REPORT-REC-INFO.
               05  REPORT-PC-DISPLAY-LNNO            PIC X(16).
               05  REPORT-PC-LNAME                   PIC X(16).
               05  REPORT-PC-FNAME                   PIC X(16).
               05  REPORT-PC-ANTERM                  PIC 9(03).
               05  REPORT-PC-ANTIC                   PIC S9(9)V99.
               05  REPORT-PC-UNEARN                  PIC S9(9)V99.
               05  REPORT-PC-INT                     PIC S9(9)V99.
               05  REPORT-PC-ADDON                   PIC S9(9)V99.
               05  REPORT-PC-SRV                     PIC S9(9)V99.
               05  REPORT-PC-MFEE                    PIC S9(9)V99.
               05  REPORT-PC-DISC                    PIC S9(9)V99.
               05  REPORT-PC-OTHER                   PIC S9(9)V99.
               05  REPORT-PC-CASH-EARN               PIC S9(9)V99.
               05  REPORT-PC-ENDING                  PIC S9(9)V99.
               05  REPORT-PC-NUMBER-ACCTS            PIC 9(07).
               05  REPORT-PC-ACTIONCD2               PIC X(04).
               05  FILLER                            PIC X(08).

      *---------------------------------------{PRECOMP EXCEPTIONS      }
           03  REPORT-PCX-REC-INFO         REDEFINES REPORT-REC-INFO.
               05  REPORT-PCX-DISPLAY-LNNO           PIC X(16). 
               05  REPORT-PCX-LNAME                  PIC X(16). 
               05  REPORT-PCX-FNAME                  PIC X(16). 
               05  REPORT-PCX-ANTERM                 PIC 9(03).
               05  REPORT-PCX-ANTIC                  PIC S9(9)V99.
               05  REPORT-PCX-UNEARN                 PIC S9(9)V99.
               05  REPORT-PCX-INT                    PIC S9(9)V99.
               05  REPORT-PCX-ADDON                  PIC S9(9)V99.
               05  REPORT-PCX-SRV                    PIC S9(9)V99.
               05  REPORT-PCX-MFEE                   PIC S9(9)V99.
               05  REPORT-PCX-DISC                   PIC S9(9)V99.
               05  REPORT-PCX-OTHER                  PIC S9(9)V99.
               05  REPORT-PCX-ROLLBACK               PIC S9(9)V99.
               05  REPORT-PCX-CONTRACTUAL            PIC  9(4).
               05  REPORT-PCX-NUMBER-ACCTS           PIC 9(07).
               05  REPORT-PCX-ACTIONCD2              PIC X(04).
               05  FILLER                            PIC X(08).

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/AC2DAY.CBL S35 04/22/25-12:15:11 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".

       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LPWSDL.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LP01LTI_SQL.CPY".
       COPY "LIBLP/LPWSLTI.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".

       01  LN1-CONN-OPEN-SW            PIC X           VALUE "N".

      *-----------------------------------------------------------------
       01  REPORT-IFN                  PIC X(06)     VALUE "REPORT".
       01  REPORT-PATH                 PIC X(35)     VALUE SPACES.

      *=================================================================
      *01  LOCAL-FILE-PATHS.
      *    03  LOCAL-BW-PATH              PIC X(35).
      *    03  LOCAL-DL-PATH              PIC X(35).
      *    03  LOCAL-LP-PATH              PIC X(35).

      *=================================================================
       01  AC2DAY-IB-KEY                PIC 9(09)     VALUE ZEROES.
       01  AC2DAY-IBX-KEY               PIC 9(09)     VALUE ZEROES.
       01  AC2DAY-PC-KEY                PIC 9(09)     VALUE ZEROES.
       01  AC2DAY-PCX-KEY               PIC 9(09)     VALUE ZEROES.

       01  AC2DAY-IB-PATH               PIC X(35)     VALUE SPACES.
       01  FILLER                       REDEFINES AC2DAY-IB-PATH.
           03  AC2DAY-IB-PATH-SLASH     PIC X(01).
           03  AC2DAY-IB-PATH-FIL       PIC X(12).
           03  AC2DAY-IB-PATH-FTP       PIC X(04).
           03  AC2DAY-IB-PATH-FILENAME  PIC X(18).

       01  AC2DAY-IBX-PATH              PIC X(35)     VALUE SPACES.
       01  FILLER                       REDEFINES AC2DAY-IBX-PATH.
           03  AC2DAY-IBX-PATH-SLASH    PIC X(01).
           03  AC2DAY-IBX-PATH-FIL      PIC X(12).
           03  AC2DAY-IBX-PATH-FTP      PIC X(04).
           03  AC2DAY-IBX-PATH-FILENAME PIC X(18).

       01  AC2DAY-PC-PATH               PIC X(35)     VALUE SPACES.
       01  FILLER                       REDEFINES AC2DAY-PC-PATH.
           03  AC2DAY-PC-PATH-SLASH     PIC X(01).
           03  AC2DAY-PC-PATH-FIL       PIC X(12).
           03  AC2DAY-PC-PATH-FTP       PIC X(04).
           03  AC2DAY-PC-PATH-FILENAME  PIC X(18).

       01  AC2DAY-PCX-PATH              PIC X(35)     VALUE SPACES.
       01  FILLER                       REDEFINES AC2DAY-PCX-PATH.
           03  AC2DAY-PCX-PATH-SLASH    PIC X(01).
           03  AC2DAY-PCX-PATH-FIL      PIC X(12).
           03  AC2DAY-PCX-PATH-FTP      PIC X(04).
           03  AC2DAY-PCX-PATH-FILENAME PIC X(18).

      ******************************************************************
      *
      *    L O A D - W O R K E R S
      *
      *-----------------------------------------------------------------
      *    WORKERS USED TO LOAD THE REPORT-FILE
      ******************************************************************

      *-------------------------------------------------{ INT. BEARING }
       01  LOAD-REPORT-IB-WORKERS.
           03  LOAD-IB-DISPLAY-LNNO                  PIC X(16).
           03  LOAD-IB-LNAME                         PIC X(16).
           03  LOAD-IB-FNAME                         PIC X(16).
           03  LOAD-IB-FROM-DATE                     PIC 9(08).
           03  LOAD-IB-INT-SINCE-LSTPY               PIC S9(9)V99.
           03  LOAD-IB-INTBAL                        PIC S9(9)V99.
           03  LOAD-IB-TRUE-ACCRUE                   PIC S9(9)V99.
           03  LOAD-IB-ACCRUED                       PIC S9(9)V99.
           03  LOAD-IB-MAX                           PIC X(01).
           03  LOAD-IB-ACTIONCD2                     PIC X(04).

      *-------------------------------------{ INT. BERARING EXCEPTIONS }
       01  LOAD-REPORT-IBX-WORKERS.
           03  LOAD-IBX-DISPLAY-LNNO                 PIC X(16).
           03  LOAD-IBX-LNAME                        PIC X(16).
           03  LOAD-IBX-FNAME                        PIC X(16).
           03  LOAD-IBX-INT-PDTH-DATE                PIC 9(8).
           03  LOAD-IBX-INT-SINCE-LSTPY              PIC S9(9)V99.
           03  LOAD-IBX-INT-OWED                     PIC S9(9)V99.
           03  LOAD-IBX-TRUE-ACCRUE                  PIC S9(9)V99.
           03  LOAD-IBX-INCOME-ACCRUE                PIC S9(9)V99.
           03  LOAD-IBX-BALANCE                      PIC S9(9)V99.
           03  LOAD-IBX-CONTR-PDTH-DATE              PIC 9(8).
           03  LOAD-IBX-CONTR-STATUS                 PIC 9(03).
           03  LOAD-IBX-ACTIONCD2                    PIC X(04).

      *---------------------------------------------------{ PRECOMPUTE }
       01  LOAD-REPORT-PC-WORKERS.
           03  LOAD-PC-DISPLAY-LNNO                  PIC X(16).
           03  LOAD-PC-LNAME                         PIC X(16).
           03  LOAD-PC-FNAME                         PIC X(16).
           03  LOAD-PC-ANTERM                        PIC 9(03).
           03  LOAD-PC-ANTIC                         PIC S9(9)V99.
           03  LOAD-PC-UNEARN                        PIC S9(9)V99.
           03  LOAD-PC-INT                           PIC S9(9)V99.
           03  LOAD-PC-ADDON                         PIC S9(9)V99.
           03  LOAD-PC-SRV                           PIC S9(9)V99.
           03  LOAD-PC-MFEE                          PIC S9(9)V99.
           03  LOAD-PC-DISC                          PIC S9(9)V99.
           03  LOAD-PC-OTHER                         PIC S9(9)V99.
           03  LOAD-PC-CASH-EARN                     PIC S9(9)V99.
           03  LOAD-PC-ENDING                        PIC S9(9)V99.
           03  LOAD-PC-MAX                           PIC X.
           03  LOAD-PC-ACTIONCD2                     PIC X(04).

       01  LOAD-REPORT-PCX-WORKERS.
           03  LOAD-PCX-DISPLAY-LNNO                 PIC X(16). 
           03  LOAD-PCX-LNAME                        PIC X(16). 
           03  LOAD-PCX-FNAME                        PIC X(16). 
           03  LOAD-PCX-ANTERM                       PIC 9(03).
           03  LOAD-PCX-ANTIC                        PIC S9(9)V99.
           03  LOAD-PCX-UNEARN                       PIC S9(9)V99.
           03  LOAD-PCX-INT                          PIC S9(9)V99.
           03  LOAD-PCX-ADDON                        PIC S9(9)V99.
           03  LOAD-PCX-SRV                          PIC S9(9)V99.
           03  LOAD-PCX-MFEE                         PIC S9(9)V99.
           03  LOAD-PCX-DISC                         PIC S9(9)V99.
           03  LOAD-PCX-OTHER                        PIC S9(9)V99.
           03  LOAD-PCX-ROLLBACK                     PIC S9(9)V99.
           03  LOAD-PCX-CONTRACTUAL                  PIC  9(4).
           03  LOAD-PCX-ACTIONCD2                    PIC X(04).
 
      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  CALCUALTION-WORKERS.
           03  CALC-ADDON-EARNINGS     PIC S9(7)V99  COMP VALUE ZEROES.
           03  CALC-INTPDTH-DATE       PIC 9(8)     VALUE ZEROES.


       01  CURRENT-REPORT-TYPE         PIC X(03)     VALUE SPACES.
           88  CURRENT-REPORT-TYPE-IB                VALUE "IB ".
           88  CURRENT-REPORT-TYPE-IBX               VALUE "IBX".
           88  CURRENT-REPORT-TYPE-PC                VALUE "PC ".
           88  CURRENT-REPORT-TYPE-PCX               VALUE "PCX".

       01  WSS-LPS-DISPLAY-LNNO        VALUE SPACES.
           03  WSS-LPS-CLASS           PIC 99.
           03  WSS-LPS-DASH            PIC X.
           03  WSS-LPS-NUMBER          PIC 9(6).
           03  WSS-LPS-7STATUS         PIC X(7).
       01  FILLER                      REDEFINES WSS-LPS-DISPLAY-LNNO.
           03  FILLER                  PIC X(3).
           03  WSS-LPS-NUMBER-7STATUS  PIC X(13).

       01  HOLD-BRANCH                 PIC 9(05)     VALUE ZEROES.
       01  HOLD-CLASS                  PIC 9(03)     VALUE ZEROES.
       01  HOLD-POOL                   PIC 9(06)     VALUE ZEROES.

       01  HOLD-LC1-KEY.
           03  HOLD-LC-CODE            PIC 9(3)      VALUE ZEROES.

       01  HOLD-REPORT-KEY.
           03  HOLD-REPORT-TYPE        PIC X(03).
               88  HOLD-REPORT-TYPE-IB               VALUE "IB ".
               88  HOLD-REPORT-TYPE-IBX              VALUE "IBX".
               88  HOLD-REPORT-TYPE-PC               VALUE "PC ".
               88  HOLD-REPORT-TYPE-PCX              VALUE "PCX".

           03  HOLD-REPORT-BRANCH      PIC 9(05).
               88  HOLD-REPORT-GRAND-TOTAL           VALUE 99999.
           03  HOLD-REPORT-POOL        PIC 9(06).
               88  HOLD-REPORT-BRANCH-TOTAL          VALUE 999999.
           03  HOLD-REPORT-CLASS       PIC 9(04).
               88  HOLD-REPORT-POOL-TOTAL            VALUE 9999.
           03  HOLD-REPORT-ACCTNO      PIC 9(08).
               88  HOLD-REPORT-CLASS-TOTAL           VALUE 99999999.

       01  LEGEND                      PIC X(79)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(09)     VALUE "LEGEND:B.".
       01  MAX-IB-INTEREST             PIC S9(9)V99  COMP  VALUE ZEROES.
       01  ONE                         PIC S9(1)     VALUE ZEROES.

       01  PROCESS-ACCTNO-STAT         PIC X(02)     VALUE SPACES.
           88  PROCESS-ACCTNO-STAT-CONTINUE          VALUE SPACES.
           88  PROCESS-ACCTNO-STAT-COMPLETED         VALUE "XX".

       01  LN-COUNT                PIC 9(03)     VALUE 99.
       01  LN-FEED                 PIC 9(02)     VALUE ZEROES.
       01  PAGE-NUMBER-TOTAL       PIC 9(04)     VALUE ZEROES.
       01  PAGE-NUMBER-TYPE        PIC 9(04)     VALUE ZEROES.

       01  RECORDS-FOUND               PIC X(01)     VALUE "X".
           88  NO-RECORDS-FOUND                      VALUE "N".
           88  EXIT-PRIOR-TO-EXECUTION               VALUE "X".

       01  STARTING-TIME               PIC X(08)     VALUE SPACES.
       01  SUB-REPORT-TYPE             PIC 9(4)      VALUE ZEROS.

      ******************************************************************
      *
      *    S C R E E N - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-BRANCH                         PIC 9(04).
           03  END-BRANCH                         PIC 9(04).

           03  ACCRUAL-DATE                       PIC 9(08).
           03  FILLER                   REDEFINES ACCRUAL-DATE.
               05  ACCRUAL-DATE-CC                PIC 9(02).
               05  ACCRUAL-DATE-YY                PIC 9(02).
               05  ACCRUAL-DATE-MM                PIC 9(02).
               05  ACCRUAL-DATE-DD                PIC 9(02).

           03  GROUP-FLAG                     PIC X(01).
           03  ENT-GROUP                      PIC X(04).
           03  LOCAL-PRINT-FG                 PIC X(01).

           03  FILLER                             PIC X(01).

           03  BEG-POOL                       PIC 9(05).
           03  END-POOL                       PIC 9(05).

           03  REPORT-BREAKDOWN-OPTIONS.
               05  PRINT-BY-CLASS                 PIC X(01).
               05  PRINT-BY-POOL                  PIC X(01).

           03  REPORT-LOAN-TYPE-OPTIONS.
               05  PRINT-LOAN-TYPE-IB             PIC X(01).
               05  PRINT-LOAN-TYPE-IBX            PIC X(01).
               05  PRINT-LOAN-TYPE-PC             PIC X(01).
               05  PRINT-LOAN-TYPE-PCX            PIC X(01).

           03  REPORT-PRINT-OPTIONS.
               05  PRINT-BRANCH-TOTALS            PIC X(01).
               05  PRINT-CLASS-TOTALS             PIC X(01).
               05  PRINT-DETAIL-ANALYSIS          PIC X(01).
               05  PRINT-GRAND-TOTALS             PIC X(01).
               05  PRINT-POOL-TOTALS              PIC X(01).

           03  EXPORT-FILE-YNO                PIC X(01).

      *-----------------------------------------------------------------
       01  VERSION-CONTROL             PIC X(01)     VALUE "E".

       01  SAVE-SCREEN-RANGES.
           03  SV-BEG-BRANCH           PIC 9(04)     VALUE 9999.
           03  SV-END-BRANCH           PIC 9(04)     VALUE 9999.

           03  SV-GROUP-OPTION.
               05  SV-GROUP-FLAG       PIC X(01).
               05  SV-ENT-GROUP        PIC X(04).
               05  SV-LOCAL-PRINT-FG   PIC X(01).

      ******************************************************************
      *
      *    H E A D E R - W O R K E R S
      *
      ******************************************************************
       01  HEADER-LINE-01.
           03  H01-DATE           PIC X(08) VALUE SPACES.
           03  FILLER             PIC X(02) VALUE SPACES.
           03  H01-HOUR           PIC 9(02) VALUE ZEROES.
           03  FILLER             PIC X(01) VALUE ":".
           03  H01-MINUTE         PIC 9(02) VALUE ZEROES.
           03  FILLER             PIC X(30) VALUE SPACES.
           03  H01-NAME           PIC X(40) VALUE SPACES.
           03  FILLER             PIC X(20) VALUE SPACES.
           03  FILLER             PIC X(18) VALUE "PAGE (TYPE/TOTAL) ".
           03  H01-PAGE-TYPE      PIC 9(04).
           03  FILLER             PIC X(01) VALUE "/".
           03  H01-PAGE-TOTAL     PIC 9(04).

      *-----------------------------------------------------------------
       01  HEADER-LINE-02.
           03  FILLER             PIC X(10) VALUE "USER ID:".
           03  H02-USERID         PIC X(10) VALUE SPACES.
           03  FILLER             PIC X(18) VALUE SPACES.
           03  H02-TITLE          PIC X(92) VALUE SPACES.
           03  FILLER             PIC X(02) VALUE SPACES.
           03  FILLER             PIC X(12) VALUE "DAY ENDING: ".
           03  H02-ACCRUAL-DATE   PIC 99/99/99.

      *-----------------------------------------------------------------
       01  HEADER-LINE-03.
           03  FILLER             PIC X(08) VALUE "BRANCH :".
           03  FILLER             PIC X(02) VALUE SPACES.
           03  H03-BRANCH         PIC 9(04) VALUE ZEROES.
           03  H03-BRANCHX        REDEFINES H03-BRANCH
                                  PIC X(04).
           03  FILLER             PIC X(02) VALUE SPACES.
           03  H03-BRANCH-DESC    PIC X(40) VALUE SPACES.
           03  FILLER             PIC X(76) VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEADER-LINE-04.
           03  FILLER             PIC X(08) VALUE "POOL ID:".
           03  FILLER             PIC X(02) VALUE SPACES.
           03  H04-POOL           PIC 9(05) VALUE ZEROES.
           03  H04-POOLX          REDEFINES H04-POOL
                                  PIC X(05).
           03  FILLER             PIC X(01) VALUE SPACES.
           03  H04-POOL-DESC      PIC X(40) VALUE SPACES.
           03  FILLER             PIC X(76) VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEADER-LINE-05.
           03  FILLER             PIC X(08) VALUE "CLASS  :".
           03  FILLER             PIC X(02) VALUE SPACES.
           03  H05-CLASS          PIC 9(02) VALUE ZEROES.
           03  H05-CLASSX         REDEFINES H05-CLASS
                                  PIC X(02).
           03  FILLER             PIC X(04) VALUE SPACES.
           03  H05-CLASS-DESC     PIC X(40) VALUE SPACES.
           03  FILLER             PIC X(56) VALUE SPACES.
           03  H05-MTD-NON        PIC X(07) VALUE "MTD-NON".
           03  FILLER             PIC X(11) VALUE SPACES.

      *-------------------------------------------{ 'INTEREST BEARING' }
       01  HEADER-LINE-IB-01.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE "              ACCRUED     ACCR".
           03  FILLER PIC X(30) VALUE "UED FROM    +    INTEREST     ".
           03  FILLER PIC X(30) VALUE "=       TRUE        ACCRUED   ".
           03  FILLER PIC X(12) VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEADER-LINE-IB-02.
           03  FILLER PIC X(30) VALUE "ACCOUNT NO.   BORROWER        ".
           03  FILLER PIC X(30) VALUE "                 FROM    LAST ".
           03  FILLER PIC X(30) VALUE "ASSESSED             OWED     ".
           03  FILLER PIC X(30) VALUE "    ASSESSED         AMOUNT   ".
           03  FILLER PIC X(12) VALUE SPACES.

      *--------------------------------{ 'INTEREST BEARING EXCEPTIONS' }
       01  HEADER-LINE-IBX-01.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE "        ACRRUED    ACCRD FROM ".
           03  FILLER PIC X(30) VALUE "    INTEREST         TRUE     ".
           03  FILLER PIC X(30) VALUE " ACCRUED                CONTRA".
           03  FILLER PIC X(12) VALUE "CT    CONT  ".

      *-----------------------------------------------------------------
       01  HEADER-LINE-IBX-02.
           03  FILLER PIC X(30) VALUE "ACCOUNT NO.   BORROWER        ".
           03  FILLER PIC X(30) VALUE "        FROM        LAST ASSD ".
           03  FILLER PIC X(30) VALUE "        OWED         ASSD     ".
           03  FILLER PIC X(30) VALUE "  AMOUNT      BALANCE  PAID TH".
           03  FILLER PIC X(12) VALUE "RU  STATUS  ".

      *-------------------------------------------------{ 'PRECOMPUTE' }
       01  HEADER-LINE-PC-01.
           03  FILLER PIC X(30) VALUE "                             A".
           03  FILLER PIC X(30) VALUE "NTICIPATED      BEGIN   ------".
           03  FILLER PIC X(30) VALUE "---------- DAILY ACCRUAL EARNI".
           03  FILLER PIC X(30) VALUE "NGS ----------------    ACCRUA".
           03  FILLER PIC X(12) VALUE "L    ENDING ".

      *-----------------------------------------------------------------
       01  HEADER-LINE-PC-02.
           03  FILLER PIC X(30) VALUE "ACCOUNT NO.   BORROWER      TE".
           03  FILLER PIC X(30) VALUE "RM EARNINGS  UNEARNED  INTERES".
           03  FILLER PIC X(30) VALUE "T    ADDONS    SERCHG  MAINTFE".
           03  FILLER PIC X(30) VALUE "E   DL DISC     OTHER  EARNING".
           03  FILLER PIC X(12) VALUE "S  UNEARNED ".

      *-----------------------------------------------{'PRECOMP EXCEPTION'}
       01  HEADER-LINE-PCX-01.
           03  FILLER PIC X(30) VALUE "                             A".
           03  FILLER PIC X(30) VALUE "NTICIPATED      BEGIN   ------".
           03  FILLER PIC X(30) VALUE "---------- DAILY ACCRUAL EARNI".
           03  FILLER PIC X(30) VALUE "NGS ----------------    EARNIN".
           03  FILLER PIC X(12) VALUE "GS          ".
 
      *-----------------------------------------------{'PRECOMP EXCEPTION'}
       01  HEADER-LINE-PCX-02.
           03  FILLER PIC X(30) VALUE "ACCOUNT NO.   BORROWER      TE".
           03  FILLER PIC X(30) VALUE "RM EARNINGS  UNEARNED  INTERES".
           03  FILLER PIC X(30) VALUE "T    ADDONS    SERCHG  MAINTFE".
           03  FILLER PIC X(30) VALUE "E   DL DISC     OTHER   ROLLBA".
           03  FILLER PIC X(12) VALUE "CK CONTRACT.".

      *---------------------------------{ 'INTEREST BEARING' (SUMMARY) }
       01  HEADER-LINE-IB-01-SUMMARY.
           03  FILLER PIC X(30) VALUE "* SUMMARY *                   ".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE "                              ".
           03  FILLER PIC X(12) VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEADER-LINE-IB-02-SUMMARY.
           03  FILLER PIC X(30) VALUE "                              ".
           03  FILLER PIC X(30) VALUE "        TOTAL ACCRUED         ".
           03  FILLER PIC X(30) VALUE "   TOTAL       TOTAL TRUE     ".
           03  FILLER PIC X(30) VALUE " TOTAL ACCRUED                ".
           03  FILLER PIC X(12) VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEADER-LINE-IB-03-SUMMARY.
           03  FILLER PIC X(30) VALUE " BR# POOL CL        ACCTS     ".
           03  FILLER PIC X(30) VALUE "   FROM LAST ASSESSED         ".
           03  FILLER PIC X(30) VALUE "INT OWED         ASSESSED     ".
           03  FILLER PIC X(30) VALUE "        AMOUNT                ".
           03  FILLER PIC X(12) VALUE SPACES.

      *-----------------------{ 'INTEREST BEARING EXCEPTION' (SUMMARY) }
       01  HEADER-LINE-IBX-01-SUMMARY.
           03  FILLER PIC X(30) VALUE "* SUMMARY *                   ".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE "                              ".
           03  FILLER PIC X(12) VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEADER-LINE-IBX-02-SUMMARY.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE "                   ACCRD FROM ".
           03  FILLER PIC X(30) VALUE "    INTEREST         TRUE     ".
           03  FILLER PIC X(30) VALUE " ACCRUED                      ".
           03  FILLER PIC X(12) VALUE "            ".

      *-----------------------------------------------------------------
       01  HEADER-LINE-IBX-03-SUMMARY.
           03  FILLER PIC X(30) VALUE " BR# POOL CL                  ".
           03  FILLER PIC X(30) VALUE "                    LAST ASSD ".
           03  FILLER PIC X(30) VALUE "        OWED         ASSD     ".
           03  FILLER PIC X(30) VALUE "  AMOUNT      BALANCE         ".
           03  FILLER PIC X(12) VALUE "            ".

      *---------------------------------------{ 'PRECOMPUTE' (SUMMARY) }
       01  HEADER-LINE-PC-01-SUMMARY.
           03  FILLER PIC X(30) VALUE "* SUMMARY *                   ".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(21) VALUE SPACES.
           03  FILLER PIC X(07) VALUE "MTD-NON".
           03  FILLER PIC X(14) VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEADER-LINE-PC-02-SUMMARY.
           03  FILLER PIC X(30) VALUE "                   NO      BEG".
           03  FILLER PIC X(30) VALUE "IN       ---------------------".
           03  FILLER PIC X(30) VALUE " DAILY ACCRUAL EARNINGS ------".
           03  FILLER PIC X(30) VALUE "---------------       ACCRUAL ".
           03  FILLER PIC X(12) VALUE "     ENDING ".

      *-----------------------------------------------------------------
       01  HEADER-LINE-PC-03-SUMMARY.
           03  FILLER PIC X(30) VALUE " BR# POOL CL    ACCTS   UNEARN".
           03  FILLER PIC X(30) VALUE "ED     INTEREST      ADDONS   ".
           03  FILLER PIC X(30) VALUE "   SERCHG    MAINTFEE      DL ".
           03  FILLER PIC X(30) VALUE "DISC       OTHER    EARNINGS  ".
           03  FILLER PIC X(12) VALUE "   UNEARNED ".

    
      *---------------------------------------{ 'PRECOMPUTE' (SUMMARY) }
      *                                         'EXCEPTIONS'
       01  HEADER-LINE-PCX-01-SUMMARY.
           03  FILLER PIC X(30) VALUE "* SUMMARY *                   ".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(21) VALUE SPACES.
           03  FILLER PIC X(07) VALUE "MTD-NON".
           03  FILLER PIC X(14) VALUE SPACES.
 
      *-----------------------------------------------------------------
       01  HEADER-LINE-PCX-02-SUMMARY.
           03  FILLER PIC X(30) VALUE "                   NO      BEG".
           03  FILLER PIC X(30) VALUE "IN       ---------------------".
           03  FILLER PIC X(30) VALUE " DAILY ACCRUAL EARNINGS ------".
           03  FILLER PIC X(30) VALUE "---------------       ACCRUAL ".
           03  FILLER PIC X(12) VALUE "     ENDING ".

      *-----------------------------------------------------------------
       01  HEADER-LINE-PCX-03-SUMMARY.
           03  FILLER PIC X(30) VALUE " BR# POOL CL    ACCTS   UNEARN".
           03  FILLER PIC X(30) VALUE "ED     INTEREST      ADDONS   ".
           03  FILLER PIC X(30) VALUE "   SERCHG    MAINTFEE      DL ".
           03  FILLER PIC X(30) VALUE "DISC       OTHER    EARNINGS  ".
           03  FILLER PIC X(12) VALUE "   UNEARNED ".

      *-----------------------------------------------------------------
       01  HEADER-TITLE-TYPE-IB.
           03  FILLER PIC X(31) VALUE "ACCRUED INTEREST FOR I/B LOANS ".
           03  FILLER PIC X(31) VALUE "  (* - EXCEEDS MAX ACCRUE DAYS)".
           03  FILLER PIC X(30) VALUE "                              ".

      *-----------------------------------------------------------------
       01  HEADER-TITLE-TYPE-IBX.
           03  FILLER PIC X(31) VALUE "ACCRUED INTEREST FOR I/B LOANS ".
           03  FILLER PIC X(31) VALUE "EXCEEDING MAX ACCRUE DAYS      ".
           03  FILLER PIC X(30) VALUE "                              ".

      *-----------------------------------------------------------------
       01  HEADER-TITLE-TYPE-PC.
           03  FILLER PIC X(31) VALUE "UNEARNED / EARNED ACCRUAL REPOR".
           03  FILLER PIC X(31) VALUE "T (PRECOMPUTE)                 ".
           03  FILLER PIC X(30) VALUE "                              ".

      *-----------------------------------------------------------------
       01  HEADER-TITLE-TYPE-PCX.
           03  FILLER PIC X(31) VALUE "UNEARNED / EXCEPTIONS     REPOR".
           03  FILLER PIC X(31) VALUE "T (PRECOMPUTE)                 ".
           03  FILLER PIC X(30) VALUE "                              ".
 
      ******************************************************************
      *
      *    D E T A I L - L I N E
      *
      ******************************************************************
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------{ 'INT. BEARING' }
       01  DETAIL-LINE-01-IB                  REDEFINES DETAIL-LINE.
           03  D01-IB-DISPLAY-LNNO            PIC X(13).
           03  FILLER                         PIC X(01).
           03  D01-IB-NAME                    PIC X(13).
           03  FILLER                         PIC X(16).
           03  D01-IB-FROM                    PIC 99/99/99.
           03  FILLER                         PIC X(05).
           03  D01-IB-INT-SINCE-LSTPY         PIC Z,ZZZ,ZZ9.99.
           03  FILLER                         PIC X(05).
           03  D01-IB-INTBAL                  PIC Z,ZZZ,ZZ9.99.
           03  FILLER                         PIC X(05).
           03  D01-IB-TRUE-ACCRUE             PIC Z,ZZZ,ZZ9.99.
           03  FILLER                         PIC X(05).
           03  D01-IB-ACCRUED                 PIC ZZZ,ZZ9.99.
           03  FILLER                         PIC X(03).
           03  D01-IB-MAX                     PIC X(01).

      *------------------------------------{ 'INT. BEARING EXCEPTIONS' }
       01  DETAIL-LINE-02-IBX                 REDEFINES DETAIL-LINE.
           03  D02-IBX-DISPLAY-LNNO           PIC X(13).
           03  FILLER                         PIC X(01).
           03  D02-IBX-NAME                   PIC X(13).
           03  FILLER                         PIC X(11).
           03  D02-IBX-INT-PDTH               PIC 99/99/99.
           03  FILLER                         PIC X(01).
           03  D02-IBX-INT-SINCE-LSTPY        PIC Z,ZZZ,ZZ9.99.
           03  FILLER                         PIC X(01).
           03  D02-IBX-INT-OWED               PIC Z,ZZZ,ZZ9.99.
           03  FILLER                         PIC X(01).
           03  D02-IBX-TRUE-ACCRUE            PIC Z,ZZZ,ZZ9.99.
           03  FILLER                         PIC X(03).
           03  D02-IBX-INCOME-ACCRUE          PIC ZZZ,ZZ9.99.
           03  FILLER                         PIC X(01).
           03  D02-IBX-BALANCE                PIC Z,ZZZ,ZZ9.99.
           03  FILLER                         PIC X(03).
           03  D02-IBX-CONT-PDTH              PIC 99/99/99.
           03  FILLER                         PIC X(05).
           03  D02-IBX-CONT-STATUS            PIC ZZ9.
           03  FILLER                         PIC X(02).

      *-------------------------------------------------{ 'PRECOMPUTE' }
       01  DETAIL-LINE-03-PC                  REDEFINES DETAIL-LINE.
           03  D03-PC-DISPLAY-LNNO            PIC X(13).
           03  FILLER                         PIC X(01).
           03  D03-PC-NAME                    PIC X(13).
           03  FILLER                         PIC X(01).
           03  D03-PC-ANTERM                  PIC ZZ9        BLANK ZERO.
           03  FILLER                         PIC X(01).
           03  D03-PC-ANTIC                   PIC Z(05)9.99- BLANK ZERO.
           03  D03-PC-UNEARN                  PIC Z(05)9.99- BLANK ZERO.
           03  D03-PC-INT                     PIC Z(05)9.99- BLANK ZERO.
           03  D03-PC-ADDON                   PIC Z(05)9.99- BLANK ZERO.
           03  D03-PC-SRV                     PIC Z(05)9.99- BLANK ZERO.
           03  D03-PC-MFEE                    PIC Z(05)9.99- BLANK ZERO.
           03  D03-PC-DISC                    PIC Z(05)9.99- BLANK ZERO.
           03  D03-PC-OTHER                   PIC Z(05)9.99- BLANK ZERO.
           03  D03-PC-CASH-EARN               PIC Z(05)9.99- BLANK ZERO.
           03  D03-PC-ENDING                  PIC Z(05)9.99- BLANK ZERO.


      *-------------------------------------------------{ 'PRECOMPUTE' }
      *                                                   'EXCEPTIONS' 
       01  DETAIL-LINE-04-PCX                REDEFINES DETAIL-LINE.
           03  D04-PCX-DISPLAY-LNNO           PIC X(13).
           03  FILLER                         PIC X(01).
           03  D04-PCX-NAME                   PIC X(13).
           03  FILLER                         PIC X(01).
           03  D04-PCX-ANTERM                 PIC ZZ9        BLANK ZERO.
           03  FILLER                         PIC X(01).
           03  D04-PCX-ANTIC                  PIC Z(05)9.99- BLANK ZERO.
           03  D04-PCX-UNEARN                 PIC Z(05)9.99- BLANK ZERO.
           03  D04-PCX-INT                    PIC Z(05)9.99- BLANK ZERO.
           03  D04-PCX-ADDON                  PIC Z(05)9.99- BLANK ZERO.
           03  D04-PCX-SRV                    PIC Z(05)9.99- BLANK ZERO.
           03  D04-PCX-MFEE                   PIC Z(05)9.99- BLANK ZERO.
           03  D04-PCX-DISC                   PIC Z(05)9.99- BLANK ZERO.
           03  D04-PCX-OTHER                  PIC Z(05)9.99- BLANK ZERO.
           03  D04-PCX-ROLLBACK               PIC Z(05)9.99- BLANK ZERO.
           03  FILLER                         PIC X(3).
           03  D04-PCX-CONTRACTUAL            PIC 9(04)      BLANK ZERO.

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

      *--------------------------------{ 'INT. BEARING' SUMMARY TOTALS }
       01  SUMMARY-LINE-01-IB                 REDEFINES DETAIL-LINE.
           03  S01-IB-BRANCH                  PIC Z(03)9.
           03  S01-IB-BRANCHX                 REDEFINES S01-IB-BRANCH
                                              PIC X(04).
           03  FILLER                         PIC X(01).
           03  S01-IB-POOL                    PIC Z(03)9.
           03  S01-IB-POOLX                   REDEFINES S01-IB-POOL
                                              PIC X(04).
           03  FILLER                         PIC X(01).
           03  S01-IB-CLASS                   PIC 9(02).
           03  S01-IB-CLASSX                  REDEFINES S01-IB-CLASS
                                              PIC X(02).
           03  FILLER                         PIC X(05).
           03  S01-IB-NUMBER-ACCTS            PIC Z(05)9.
           03  S01-IB-NUMBER-ACCTS-FG         PIC X(01).
           03  FILLER                         PIC X(13).
           03  S01-IB-INT-SINCE-LSTPY         PIC ZZZ,ZZZ,ZZ9.99.
           03  FILLER                         PIC X(03).
           03  S01-IB-INTBAL                  PIC ZZZ,ZZZ,ZZ9.99.
           03  FILLER                         PIC X(03).
           03  S01-IB-TRUE-ACCRUE             PIC ZZZ,ZZZ,ZZ9.99.
           03  FILLER                         PIC X(05).
           03  S01-IB-ACCRUED                 PIC ZZZ,ZZZ,ZZ9.99.

      *-------------------------{ 'INT. BEARING EXECPT' SUMMARY TOTALS }
       01  SUMMARY-LINE-02-IBX                REDEFINES DETAIL-LINE.
           03  S02-IBX-BRANCH                 PIC Z(03)9.
           03  S02-IBX-BRANCHX                REDEFINES S02-IBX-BRANCH
                                              PIC X(04).
           03  FILLER                         PIC X(01).
           03  S02-IBX-POOL                   PIC Z(03)9.
           03  S02-IBX-POOLX                  REDEFINES S02-IBX-POOL
                                              PIC X(04).
           03  FILLER                         PIC X(01).
           03  S02-IBX-CLASS                  PIC 9(02).
           03  S02-IBX-CLASSX                 REDEFINES S02-IBX-CLASS
                                              PIC X(02).
           03  FILLER                         PIC X(05).
           03  S02-IBX-NUMBER-ACCTS           PIC Z(05)9.
           03  S02-IBX-NUMBER-ACCTS-FG        PIC X(01).
           03  FILLER                         PIC X(23).
           03  S02-IBX-INT-SINCE-LSTPY        PIC Z,ZZZ,ZZ9.99.
           03  FILLER                         PIC X(01).
           03  S02-IBX-INT-OWED               PIC Z,ZZZ,ZZ9.99.
           03  FILLER                         PIC X(1).
           03  S02-IBX-TRUE-ACCRUE            PIC Z,ZZZ,ZZ9.99.
           03  FILLER                         PIC X(1).
           03  S02-IBX-INCOME-ACCRUE          PIC Z,ZZZ,ZZ9.99.
           03  FILLER                         PIC X(1).
           03  S02-IBX-BALANCE                PIC Z,ZZZ,ZZ9.99.

      *----------------------------------{ 'PRECOMPUTE' SUMMARY TOTALS }
       01  SUMMARY-LINE-03-PC                 REDEFINES DETAIL-LINE.
           03  S03-PC-BRANCH                  PIC Z(03)9.
           03  S03-PC-BRANCHX                 REDEFINES S03-PC-BRANCH
                                              PIC X(04).
           03  FILLER                         PIC X(01).
           03  S03-PC-POOL                    PIC Z(03)9.
           03  S03-PC-POOLX                   REDEFINES S03-PC-POOL
                                              PIC X(04).
           03  FILLER                         PIC X(01).
           03  S03-PC-CLASS                   PIC 9(02).
           03  S03-PC-CLASSX                  REDEFINES S03-PC-CLASS
                                              PIC X(02).
           03  FILLER                         PIC X(01).
           03  S03-PC-NUMBER-ACCTS            PIC Z(05)9.
           03  S03-PC-NUMBER-ACCTS-FG         PIC X(01).
           03  S03-PC-UNEARN                  PIC Z(08)9.99- BLANK ZERO.
           03  S03-PC-INT                     PIC Z(08)9.99- BLANK ZERO.
           03  S03-PC-ADDON                   PIC Z(07)9.99- BLANK ZERO.
           03  S03-PC-SRV                     PIC Z(07)9.99- BLANK ZERO.
           03  S03-PC-MFEE                    PIC Z(07)9.99- BLANK ZERO.
           03  S03-PC-DISC                    PIC Z(08)9.99- BLANK ZERO.
           03  S03-PC-OTHER                   PIC Z(07)9.99- BLANK ZERO.
           03  S03-PC-CASH-EARN               PIC Z(07)9.99- BLANK ZERO.
           03  S03-PC-ENDING                  PIC Z(08)9.99- BLANK ZERO.

       01  SUMMARY-LINE-04-PCX                 REDEFINES DETAIL-LINE.
           03  S04-PCX-BRANCH                  PIC Z(03)9.
           03  S04-PCX-BRANCHX                 REDEFINES S04-PCX-BRANCH
                                              PIC X(04).
           03  FILLER                         PIC X(01).
           03  S04-PCX-POOL                    PIC Z(03)9.
           03  S04-PCX-POOLX                   REDEFINES S04-PCX-POOL
                                              PIC X(04).
           03  FILLER                         PIC X(01).
           03  S04-PCX-CLASS                   PIC 9(02).
           03  S04-PCX-CLASSX                  REDEFINES S04-PCX-CLASS
                                              PIC X(02).
           03  FILLER                         PIC X(01).
           03  S04-PCX-NUMBER-ACCTS            PIC Z(05)9.
           03  S04-PCX-NUMBER-ACCTS-FG         PIC X(01).
           03  S04-PCX-UNEARN                 PIC Z(08)9.99- BLANK ZERO.
           03  S04-PCX-INT                    PIC Z(08)9.99- BLANK ZERO.
           03  S04-PCX-ADDON                  PIC Z(07)9.99- BLANK ZERO.
           03  S04-PCX-SRV                    PIC Z(07)9.99- BLANK ZERO.
           03  S04-PCX-MFEE                   PIC Z(07)9.99- BLANK ZERO.
           03  S04-PCX-DISC                   PIC Z(08)9.99- BLANK ZERO.
           03  S04-PCX-OTHER                  PIC Z(07)9.99- BLANK ZERO.
           03  S04-PCX-ROLLBACK               PIC Z(07)9.99- BLANK ZERO.


      *-------------------------------{ 'INT. BEARING' STANDARD TOTALS }
       01  TOTAL-LINE-01-IB                   REDEFINES DETAIL-LINE.
           03  T01-IB-PROMPT                  PIC X(25).
           03  T01-IB-NUMBER-ACCTS            PIC Z(05)9.
           03  T01-IB-NUMBER-ACCTS-FG         PIC X(01).
           03  FILLER                         PIC X(23).
           03  T01-IB-INT-SINCE-LSTPY         PIC ZZ,ZZZ,ZZ9.99.
           03  FILLER                         PIC X(04).
           03  T01-IB-INTBAL                  PIC ZZ,ZZZ,ZZ9.99.
           03  FILLER                         PIC X(04).
           03  T01-IB-TRUE-ACCRUE             PIC ZZ,ZZZ,ZZ9.99.
           03  FILLER                         PIC X(01).
           03  T01-IB-ACCRUED                 PIC ZZZ,ZZZ,ZZ9.99-.

      *------------------------{ 'INT. BEARING EXECPT' STANDARD TOTALS }
       01  TOTAL-LINE-02-IBX                  REDEFINES DETAIL-LINE.
           03  T02-IBX-PROMPT                 PIC X(25).
           03  T02-IBX-NUMBER-ACCTS           PIC Z(05)9.
           03  T02-IBX-NUMBER-ACCTS-FG        PIC X(01).
           03  FILLER                         PIC X(15).
           03  T02-IBX-INT-SINCE-LSTPY        PIC Z,ZZZ,ZZ9.99.
           03  FILLER                         PIC X(01).
           03  T02-IBX-INT-OWED               PIC Z,ZZZ,ZZ9.99.
           03  FILLER                         PIC X(1).
           03  T02-IBX-TRUE-ACCRUE            PIC Z,ZZZ,ZZ9.99.
           03  FILLER                         PIC X(1).
           03  T02-IBX-INCOME-ACCRUE          PIC Z,ZZZ,ZZ9.99.
           03  FILLER                         PIC X(1).
           03  T02-IBX-BALANCE                PIC Z,ZZZ,ZZ9.99.

      *---------------------------------{ 'PRECOMPUTE' STANDARD TOTALS }
       01  TOTAL-LINE-03-PC                   REDEFINES DETAIL-LINE.
           03  T03-PC-PROMPT                  PIC X(25).
           03  T03-PC-NUMBER-ACCTS            PIC Z(05)9.
           03  T03-PC-NUMBER-ACCTS-FG         PIC X(01).
           03  FILLER                         PIC X(07).
           03  T03-PC-UNEARN                  PIC Z(08)9.99- BLANK ZERO.
           03  FILLER                         PIC X(07).
           03  T03-PC-ADDON                   PIC Z(08)9.99- BLANK ZERO.
           03  FILLER                         PIC X(07).
           03  T03-PC-MFEE                    PIC Z(08)9.99- BLANK ZERO.
           03  FILLER                         PIC X(07).
           03  T03-PC-OTHER                   PIC Z(08)9.99- BLANK ZERO.
           03  FILLER                         PIC X(07).
           03  T03-PC-ENDING                  PIC Z(08)9.99- BLANK ZERO.

      *-----------------------------------------------------------------
       01  TOTAL-LINE-04-PC                   REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(49).
           03  T04-PC-INT                     PIC Z(08)9.99- BLANK ZERO.
           03  FILLER                         PIC X(07).
           03  T04-PC-SRV                     PIC Z(08)9.99- BLANK ZERO.
           03  FILLER                         PIC X(07).
           03  T04-PC-DISC                    PIC Z(08)9.99- BLANK ZERO.
           03  FILLER                         PIC X(07).
           03  T04-PC-CASH-EARN               PIC Z(08)9.99- BLANK ZERO.


      *---------------------------------{ 'PRECOMPUTE' EXCEPTIONS      }
       01  TOTAL-LINE-03-PCX                  REDEFINES DETAIL-LINE.
           03  T03-PCX-PROMPT                 PIC X(25).
           03  T03-PCX-NUMBER-ACCTS           PIC Z(05)9.
           03  T03-PCX-NUMBER-ACCTS-FG        PIC X(01).
           03  FILLER                         PIC X(07).
           03  T03-PCX-UNEARN                 PIC Z(08)9.99- BLANK ZERO.
           03  FILLER                         PIC X(07).
           03  T03-PCX-ADDON                  PIC Z(08)9.99- BLANK ZERO.
           03  FILLER                         PIC X(07).
           03  T03-PCX-MFEE                   PIC Z(08)9.99- BLANK ZERO.
           03  FILLER                         PIC X(07).
           03  T03-PCX-OTHER                  PIC Z(08)9.99- BLANK ZERO.
           03  FILLER                         PIC X(07).

      *-----------------------------------------------------------------
       01  TOTAL-LINE-04-PCX                  REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(49).
           03  T04-PCX-INT                    PIC Z(08)9.99- BLANK ZERO.
           03  FILLER                         PIC X(07).
           03  T04-PCX-SRV                    PIC Z(08)9.99- BLANK ZERO.
           03  FILLER                         PIC X(07).
           03  T04-PCX-DISC                   PIC Z(08)9.99- BLANK ZERO.
           03  FILLER                         PIC X(07).
           03  T04-PCX-ROLLBACK               PIC Z(08)9.99- BLANK ZERO.


      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/PROGIDW.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       01  EXPORT-FILE-SEQ             PIC 9(2)      VALUE 01.

       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBGB/SYSTEMW.CPY".
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       COPY "LIBLP/AC9DAY_CMD.CPY".
       COPY "LIBLP/LPAERNW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBLP/LPCERNW.CPY".
       COPY "LIBLP/LPEADJW.CPY".
       COPY "LIBLP/LPEARNW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBLP/LPRATEW.CPY".
       COPY "LIBLP/LPREGZW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPUERNW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/AC2DAY_DEF.CPY".
       COPY "LIBLP/AC2DAY_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBLP/ARRAYSPW.CPY". 

       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/AC2DAY_SCN.CPY".

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
                 AC2DAY-IB-FILE AC2DAY-IBX-FILE
                 AC2DAY-PC-FILE AC2DAY-PCX-FILE
                 REPORT-FILE PR-FILE.

       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           PERFORM CLOSE-LN1-CONNECTION.
           CLOSE 
                 AC2DAY-IB-FILE AC2DAY-IBX-FILE
                 AC2DAY-PC-FILE AC2DAY-PCX-FILE
                 REPORT-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           PERFORM INITIALIZATION.

           IF ( EXT-EOBUF-ERRCD NOT = SPACES )
              GO TO MAIN-PROGRAM-EXIT.

           PERFORM EXECUTION-MODULE.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

           MOVE "X"           TO RECORDS-FOUND.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "AC2DAY"      TO FORM-PROG VDU-FORM-NAME.

           MOVE ZEROES TO ACCRUAL-DATE.

           MOVE AC2DAY-QUEUE-POS  TO Q-POS.
           MOVE AC2DAY-COPIES-POS TO C-POS.
           MOVE AC2DAY-PRU-POS    TO P-POS.
           MOVE SPACES            TO Q-BUF C-BUF P-BUF.


       COPY "LIBGB/EOINIT.CPY".

           IF EXT-ROUTE-BUF = "00"
              MOVE PROGID-TEXT-PROCESSING TO LEGEND
              PERFORM SEND-LEGEND.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE "S  A010ACCEPT." TO VDU
              MOVE SPACES           TO VDUBUF
              PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( BEG-BRANCH = 0 AND END-BRANCH = 0 )
              MOVE BEG-BRANCH TO SV-BEG-BRANCH
              MOVE END-BRANCH TO SV-END-BRANCH
              MOVE EXT-CONAME-USER-LOC  TO BEG-BRANCH
                                           END-BRANCH.

           IF ( EXT-EOBUF-GROUP-FG = "Y" )
              MOVE BEG-BRANCH         TO SV-BEG-BRANCH
              MOVE END-BRANCH         TO SV-END-BRANCH
              MOVE GROUP-FLAG         TO SV-GROUP-FLAG
              MOVE ENT-GROUP          TO SV-ENT-GROUP
              MOVE EXT-EOBUF-GROUP        TO ENT-GROUP
              MOVE ZEROES             TO BEG-BRANCH
              MOVE 9999               TO END-BRANCH
              MOVE "Y"                TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - -{ MAKE PATHS }
           IF ( EXPORT-FILE-YNO = "N" )
              GO TO INITIALIZATION-SKIP-EXPORT.

           MOVE "FIL" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF ( STAT-BAD )
              MOVE "ERROR: ENVIRONMENT 'FIL' NOT FOUND" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO EXT-EOBUF-ERRCD
              GO TO INITIALIZATION-EXIT.

           MOVE SPACES           TO AC2DAY-IB-PATH
                                    AC2DAY-IBX-PATH
                                    AC2DAY-PC-PATH
                                    AC2DAY-PCX-PATH.

           MOVE "/"              TO AC2DAY-IB-PATH-SLASH
                                    AC2DAY-IBX-PATH-SLASH
                                    AC2DAY-PC-PATH-SLASH
                                    AC2DAY-PCX-PATH-SLASH.

           MOVE GETENV-BUF       TO AC2DAY-IB-PATH-FIL
                                    AC2DAY-IBX-PATH-FIL
                                    AC2DAY-PC-PATH-FIL
                                    AC2DAY-PCX-PATH-FIL.

           MOVE "FTP/"           TO MESS.
           CALL "C$TOLOWER" USING MESS, VALUE 4.
           MOVE MESS             TO AC2DAY-IB-PATH-FTP
                                    AC2DAY-IBX-PATH-FTP
                                    AC2DAY-PC-PATH-FTP
                                    AC2DAY-PCX-PATH-FTP.

           MOVE "AC2DAY_IB.TXT"  TO AC2DAY-IB-PATH-FILENAME.
           MOVE "AC2DAY_IBX.TXT" TO AC2DAY-IBX-PATH-FILENAME.
           MOVE "AC2DAY_PC.TXT"  TO AC2DAY-PC-PATH-FILENAME.
           MOVE "AC2DAY_PCX.TXT" TO AC2DAY-PCX-PATH-FILENAME.

           MOVE ZEROES           TO AC2DAY-IB-KEY
                                    AC2DAY-IBX-KEY
                                    AC2DAY-PC-KEY
                                    AC2DAY-PCX-KEY.

           PERFORM OPEN-AC2DAY-IB-FILE-OUTPUT.
           PERFORM OPEN-AC2DAY-IBX-FILE-OUTPUT.
           PERFORM OPEN-AC2DAY-PC-FILE-OUTPUT.
           PERFORM OPEN-AC2DAY-PCX-FILE-OUTPUT.

      *-----------------------------------------------------------------
       INITIALIZATION-SKIP-EXPORT.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF     TO REPORT-PATH.
           PERFORM OPEN-REPORT-FILE-OUTPUT.
           PERFORM CLOSE-REPORT-FILE.
           PERFORM OPEN-REPORT-FILE.

           PERFORM ARRAYSP-LOAD.
      
           PERFORM GET-GPENV.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-CD1-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-LC1-FILE.
           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-SP1-FILE.

           PERFORM INITIAL-PROGRESS-INDICATOR.
           PERFORM GET-TIME.
           MOVE "N"       TO RECORDS-FOUND.
           MOVE TIME-EDIT TO STARTING-TIME.

       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************

           IF ( UP-KEY )
              IF ( EXPORT-FILE-YNO = "N" )
                 GO TO ENTER-PRINT-GRAND-TOTALS
              ELSE
                 GO TO ENTER-EXPORT-FILE-YNO.

       ENTER-BEG-BRANCH.

           MOVE LEGEND-BEG-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A000BEG-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-BRANCH.
           IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-BRANCH.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF ( WGR-WORD-ZERO )
              MOVE ZEROES TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.

           MOVE ZEROES TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF ( WGR-ERR > ZEROES )
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.

           IF ( WGR-TYPE = "A" )
              MOVE "S  A000END-BRANCH." TO VDU
              MOVE SPACES               TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y"                  TO GROUP-FLAG
              MOVE ZEROES               TO BEG-BRANCH
              MOVE 9999                 TO END-BRANCH
              GO TO CHECK-BR-SECURITY.

       BRANCH-IS-NUMERIC.

           MOVE "N"           TO GROUP-FLAG.
           MOVE WGR-SRESULT   TO BEG-BRANCH.

           MOVE "SN N040BEG-BRANCH." TO VDU.
           MOVE BEG-BRANCH           TO VDUNUM0.
           PERFORM SCREENIO.

       ENTER-END-BRANCH.

           MOVE LEGEND-END-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040END-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-BRANCH.

           MOVE VDUNUM0 TO END-BRANCH.

           IF ( BEG-BRANCH > END-BRANCH )
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.

           IF (( BEG-BRANCH = ZEROES ) AND ( END-BRANCH = ZEROES ))
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY"     TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES                 TO VDUBUF
              PERFORM SCREENIO.

           GO TO CHECK-BR-SECURITY.

       ENTER-ALL-BRANCH.

           MOVE "SNNN040BEG-BRANCH." TO VDU.
           MOVE ZEROES               TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040END-BRANCH." TO VDU.
           MOVE 9999                 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "N"                    TO GROUP-FLAG.
           MOVE "S  A160DISPLAY-USER." TO VDU.
           MOVE SPACES                 TO VDUBUF.
           PERFORM SCREENIO.

       CHECK-BR-SECURITY.

           PERFORM BR-RANGE-SECURITY.

           IF ( BR-SECURITY-OKAY = "N" ) GO TO ENTER-BEG-BRANCH.

       ENTER-BEG-POOL.

           MOVE LEGEND-BEG-POOL TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN050BEG-POOL." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-POOL.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-POOL.

           MOVE VDUNUM0 TO BEG-POOL.

       ENTER-END-POOL.

           MOVE LEGEND-END-POOL TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN050END-POOL." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-POOL.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-POOL.

           MOVE VDUNUM0 TO END-POOL.

           IF ( BEG-POOL > END-POOL )
              MOVE "INVALID RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-POOL.

           GO TO ENTER-ACCRUAL-DATE.

       ENTER-ALL-POOL.

           MOVE "SNNN050BEG-POOL." TO VDU.
           MOVE ZEROES             TO VDUNUM0 BEG-POOL.
           PERFORM SCREENIO.

           MOVE "SNNN050END-POOL." TO VDU.
           MOVE 99999              TO VDUNUM0 END-POOL.
           PERFORM SCREENIO.

       ENTER-ACCRUAL-DATE.
           IF ( EXT-ROUTE-BUF NOT = "00" )
              IF ( UP-KEY )
                 GO TO ENTER-END-POOL
              ELSE
                 GO TO ENTER-EXPORT-FILE-YNO.

           PERFORM DISPLAY-ACCRUAL-DATE.

           MOVE LEGEND-ACCRUAL-DATE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ERNM060ACCRUAL-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-POOL.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF NOT ( ENTER-KEY ) GO TO ENTER-ACCRUAL-DATE.

           MOVE VDU-DATE-YYYYMMDD TO ACCRUAL-DATE.

       ENTER-EXPORT-FILE-YNO.

           MOVE LEGEND-EXPORT-FILE-YNO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010EXPORT-YNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-ACCRUAL-DATE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-EXPORT-FILE-YNO.

           IF ( VDUBUF = "Y" OR "N" OR "O" )
              MOVE VDUBUF TO EXPORT-FILE-YNO
           ELSE
              MOVE "ERROR: (Y)ES, (N)O OR (O)NLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-EXPORT-FILE-YNO.

           IF ( EXPORT-FILE-YNO NOT = "N" )
              MOVE EXPORT-FILE-SEQ               TO PASSWDW-SEQ
              MOVE "FOR CREATING A EXPORT FILE:" TO PASSWDW-PROMPT2
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 GO TO ENTER-EXPORT-FILE-YNO.

           IF ( EXPORT-FILE-YNO NOT = "O" )
              GO TO ENTER-PRINT-BY-CLASS.

           MOVE "N" TO PRINT-BY-CLASS
                       PRINT-BY-POOL
                       PRINT-LOAN-TYPE-IB
                       PRINT-LOAN-TYPE-IBX
                       PRINT-LOAN-TYPE-PC
                       PRINT-LOAN-TYPE-PCX
                       PRINT-BRANCH-TOTALS
                       PRINT-CLASS-TOTALS
                       PRINT-DETAIL-ANALYSIS
                       PRINT-GRAND-TOTALS
                       PRINT-POOL-TOTALS.

           PERFORM DISPLAY-PARAMETERS.
           MOVE "N" TO LOCAL-PRINT-FG
           GO TO ENTER-ACCEPT-YN.

       ENTER-PRINT-BY-CLASS.

           IF ( UP-KEY ) GO TO ENTER-EXPORT-FILE-YNO.

           MOVE "S  A010PRT-CLASS." TO VDU.
           MOVE "Y"                 TO VDUBUF   PRINT-BY-CLASS.
           PERFORM SCREENIO.

       ENTER-PRINT-BY-POOL.

           MOVE LEGEND-PRINT-BY-POOL TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010PRT-POOL." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PRINT-BY-CLASS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PRINT-BY-POOL.

           MOVE VDUBUF TO PRINT-BY-POOL.

           IF NOT ( PRINT-BY-POOL = "Y" OR "N" )
              MOVE "ENTER (Y)ES OR (N)O ONLY!!!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PRINT-BY-POOL.

       ENTER-PRINT-LOAN-TYPE-IB.

           MOVE LEGEND-PRINT-LOAN-TYPE-IB TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010PRT-IB." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PRINT-BY-POOL.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PRINT-LOAN-TYPE-IB.

           MOVE VDUBUF TO PRINT-LOAN-TYPE-IB.

           IF NOT ( PRINT-LOAN-TYPE-IB = "Y" OR "N" )
              MOVE "ENTER (Y)ES OR (N)O ONLY!!!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PRINT-LOAN-TYPE-IB.

       ENTER-PRINT-LOAN-TYPE-IBX.

           MOVE LEGEND-PRINT-LOAN-TYPE-IBX TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010PRT-IBX." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PRINT-LOAN-TYPE-IB.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PRINT-LOAN-TYPE-IBX.

           MOVE VDUBUF TO PRINT-LOAN-TYPE-IBX.

           IF NOT ( PRINT-LOAN-TYPE-IBX = "Y" OR "N" )
              MOVE "ENTER (Y)ES OR (N)O ONLY!!!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PRINT-LOAN-TYPE-IBX.

       ENTER-PRINT-LOAN-TYPE-PC.

           MOVE LEGEND-PRINT-LOAN-TYPE-PC TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010PRT-PC." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PRINT-LOAN-TYPE-IBX.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PRINT-LOAN-TYPE-PC.

           MOVE VDUBUF TO PRINT-LOAN-TYPE-PC.

           IF NOT ( PRINT-LOAN-TYPE-PC = "Y" OR "N" )
              MOVE "ENTER (Y)ES OR (N)O ONLY!!!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PRINT-LOAN-TYPE-PC.

       ENTER-PRINT-LOAN-TYPE-PCX.
           MOVE LEGEND-PRINT-LOAN-TYPE-PCX TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010PRT-PCX." TO VDU.
           PERFORM SCREENIO.
   
           IF (        UP-KEY ) GO TO ENTER-PRINT-LOAN-TYPE-PC.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PRINT-LOAN-TYPE-PCX.

           MOVE VDUBUF TO PRINT-LOAN-TYPE-PCX.

           IF NOT ( PRINT-LOAN-TYPE-PCX = "Y" OR "N" )
              MOVE "ENTER (Y)ES OR (N)O ONLY!!!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PRINT-LOAN-TYPE-PCX.

       ENTER-PRINT-DETAIL-ANALYSI.

           MOVE LEGEND-PRINT-DETAIL-ANALYSIS TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010PRT-DETAIL." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PRINT-LOAN-TYPE-PC.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PRINT-DETAIL-ANALYSI.

           MOVE VDUBUF TO PRINT-DETAIL-ANALYSIS.

           IF NOT ( PRINT-DETAIL-ANALYSIS = "Y" OR "N" )
              MOVE "ENTER (Y)ES OR (N)O ONLY!!!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PRINT-DETAIL-ANALYSI.

       ENTER-PRINT-CLASS-TOTALS.

           MOVE LEGEND-PRINT-CLASS-TOTALS TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010PRT-CLASST." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PRINT-DETAIL-ANALYSI.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PRINT-CLASS-TOTALS.

           MOVE VDUBUF TO PRINT-CLASS-TOTALS.

           IF NOT ( PRINT-CLASS-TOTALS = "Y" OR "N" )
              MOVE "ENTER (Y)ES OR (N)O ONLY!!!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PRINT-CLASS-TOTALS.

       ENTER-PRINT-POOL-TOTALS.

           IF ( PRINT-BY-POOL = "N" )
              IF ( UP-KEY )
                 GO TO ENTER-PRINT-CLASS-TOTALS
              ELSE
                 MOVE "S  A010PRT-POOLT." TO VDU
                 MOVE "N"                 TO VDUBUF   PRINT-POOL-TOTALS
                 PERFORM SCREENIO
                 GO TO ENTER-PRINT-BRANCH-TOTALS.

           MOVE LEGEND-PRINT-POOL-TOTALS TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010PRT-POOLT." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PRINT-CLASS-TOTALS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PRINT-POOL-TOTALS.

           MOVE VDUBUF TO PRINT-POOL-TOTALS.

           IF NOT ( PRINT-POOL-TOTALS = "Y" OR "N" )
              MOVE "ENTER (Y)ES OR (N)O ONLY!!!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PRINT-POOL-TOTALS.

       ENTER-PRINT-BRANCH-TOTALS.

           MOVE LEGEND-PRINT-BRANCH-TOTALS TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010PRT-BRANCHT." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PRINT-POOL-TOTALS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PRINT-BRANCH-TOTALS.

           MOVE VDUBUF TO PRINT-BRANCH-TOTALS.

           IF NOT ( PRINT-BRANCH-TOTALS = "Y" OR "N" )
              MOVE "ENTER (Y)ES OR (N)O ONLY!!!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PRINT-BRANCH-TOTALS.

       ENTER-PRINT-GRAND-TOTALS.

           MOVE LEGEND-PRINT-GRAND-TOTALS TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010PRT-GRANDT." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PRINT-BRANCH-TOTALS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PRINT-GRAND-TOTALS.

           MOVE VDUBUF TO PRINT-GRAND-TOTALS.

           IF NOT ( PRINT-GRAND-TOTALS = "Y" OR "N" )
              MOVE "ENTER (Y)ES OR (N)O ONLY!!!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PRINT-GRAND-TOTALS.

       LOCAL-PRINT.
           IF EXT-ROUTE-BUF NOT = "40"
              IF EXT-ROUTE-BUF NOT = "50"
                 MOVE "N" TO LOCAL-PRINT-FG
                 GO TO ENTER-ACCEPT-YN. 

           IF BEG-BRANCH = END-BRANCH
              MOVE "N" TO LOCAL-PRINT-FG
              GO TO ENTER-ACCEPT-YN. 

       ASK-LOCAL.
           MOVE "S  A220SPOOL-PROMPT." TO VDU.
           MOVE "SPOOL TO LOCAL       :"    TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "SPOOL TO LOCAL DIRECTORY? (Y/N)" TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "S  A050YES-NO." TO VDU.
           MOVE "(Y/N)"          TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "E  A010LOCAL-SPOOL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO ENTER-PARAMETERS-EXIT. 
           IF VDUSTATUS = "1U" GO TO ENTER-PRINT-GRAND-TOTALS.
           IF VDUSTATUS = "1>" GO TO ENTER-PARAMETERS-EXIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ASK-LOCAL.

           IF VDUBUF NOT = "Y"
              IF VDUBUF NOT = "N"
                 GO TO ASK-LOCAL.

           MOVE VDUBUF TO LOCAL-PRINT-FG.

       ENTER-ACCEPT-YN.

           MOVE LEGEND-ACCEPT-YN TO LEGEND.
           PERFORM SEND-LEGEND.

       ENTER-PARAMETERS-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-ACCRUAL-DATE SECTION.
      ******************************************************************

           IF ( ACCRUAL-DATE NOT = ZEROES )
              GO TO DISPLAY-ACCRUAL-DATE-EXIT.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO ACCRUAL-DATE.


           MOVE "S  8000ACCRUAL-DATE." TO VDU.
           MOVE ACCRUAL-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO. 

       DISPLAY-ACCRUAL-DATE-EXIT.
           EXIT.

      *****************************************************************
       DISPLAY-PARAMETERS SECTION.
      *****************************************************************

           IF ( GROUP-FLAG = "Y" )
              MOVE "S  A000BEG-BRANCH."   TO VDU
              MOVE ENT-GROUP              TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000END-BRANCH."   TO VDU
              MOVE SPACES                 TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BEG-BRANCH."   TO VDU
              MOVE BEG-BRANCH             TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040END-BRANCH."   TO VDU
              MOVE END-BRANCH             TO VDUNUM0
              PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF (( BEG-BRANCH = ZEROES ) AND ( END-BRANCH = ZEROES ))
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY"     TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES                 TO VDUBUF
              PERFORM SCREENIO.

           MOVE "SNNN050BEG-POOL."        TO VDU.
           MOVE BEG-POOL                  TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN050END-POOL."        TO VDU.
           MOVE END-POOL                  TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A010EXPORT-YNO."      TO VDU.
           MOVE EXPORT-FILE-YNO           TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010PRT-CLASS."       TO VDU.
           MOVE PRINT-BY-CLASS            TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010PRT-POOL."        TO VDU.
           MOVE PRINT-BY-POOL             TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010PRT-IB."          TO VDU.
           MOVE PRINT-LOAN-TYPE-IB        TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010PRT-IBX."         TO VDU.
           MOVE PRINT-LOAN-TYPE-IBX       TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010PRT-PC."          TO VDU.
           MOVE PRINT-LOAN-TYPE-PC        TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010PRT-PCX."          TO VDU.
           MOVE PRINT-LOAN-TYPE-PCX        TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010PRT-DETAIL."      TO VDU.
           MOVE PRINT-DETAIL-ANALYSIS     TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010PRT-CLASST."      TO VDU.
           MOVE PRINT-CLASS-TOTALS        TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010PRT-POOLT."       TO VDU.
           MOVE PRINT-POOL-TOTALS         TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010PRT-BRANCHT."     TO VDU.
           MOVE PRINT-BRANCH-TOTALS       TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010PRT-GRANDT."      TO VDU.
           MOVE PRINT-GRAND-TOTALS        TO VDUBUF.
           PERFORM SCREENIO.

       DISPLAY-PARAMETERS-EXIT.
           EXIT.

      *****************************************************************
       EO-EXEC-AUDIT SECTION.
      *****************************************************************

           MOVE EXT-EOBUF-DATE2 TO ACCRUAL-DATE.

       EO-EXEC-AUDIT-EXIT.
           EXIT.

      ******************************************************************
       SCAN-GR1-FILE SECTION.
      ******************************************************************

           MOVE "WI/GRSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF ( GRSCAN-STATUS NOT = "00"   ) OR
              ( GRSCAN-KEY      = SPACES )
              MOVE "XX" TO VDUSTATUS
              GO TO SCAN-GR1-FILE-EXIT.

           MOVE "S  A000BEG-BRANCH." TO VDU.
           MOVE GRSCAN-KEY           TO VDUBUF.
           PERFORM SCREENIO.

           MOVE GRSCAN-KEY TO VDUBUF.
           MOVE "00"       TO VDUSTATUS.

       SCAN-GR1-FILE-EXIT.
           EXIT.

      ******************************************************************
       EXECUTION-MODULE SECTION.
      ******************************************************************

           IF ( GROUP-FLAG = "N" )
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO EXECUTION-NEXT-BRANCH.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

       EXECUTION-NEXT-GROUP.

           IF ( GROUP-FLAG = "N" )
              GO TO EXECUTION-NEXT-BRANCH.

           PERFORM SEARCH-GR-NEXT.
           IF ( WGR-SRESULT = ZEROES )
              GO TO EXECUTION-MODULE-END.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO EXECUTION-PROCESS-BRANCH.

       EXECUTION-NEXT-BRANCH.

           PERFORM READ-BR-FILE-NEXT.

           IF ( IO-BAD             ) OR
              ( BR-NO > END-BRANCH )
              GO TO EXECUTION-MODULE-END.

       EXECUTION-PROCESS-BRANCH.

           PERFORM PROCESS-BRANCH-MODULE.

           GO TO EXECUTION-NEXT-GROUP.

       EXECUTION-MODULE-END.

           PERFORM PRINT-REPORT-MODULE.

       EXECUTION-MODULE-EXIT.
           EXIT.

      ******************************************************************
       PROCESS-BRANCH-MODULE SECTION.
      ******************************************************************

           MOVE 9999 TO PROGID-SLEEP-COUNTER.
           IF EXT-ROUTE-BUF = "00"
              PERFORM SEND-PROGRESS-INDICATOR.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              LTI-PATH-OVERRIDE
                                              LTP-PATH-OVERRIDE.

           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-LTI1-FILE.

           MOVE ZEROES TO HOLD-LC-CODE.

           MOVE LN-PATH-OWNBR TO LN-OWNBR
                                 QLN1-WBEG-OWNBR.
      *     MOVE 0             TO LN-ACCTNO.

           PERFORM START-LN1-FILE.   *> SSP LN BW LC

       PROCESS-BRANCH-NEXT-LN.

           PERFORM READ-LN1-FILE-NEXT.  *> EMBEDDED LN BW LC
           IF ( IO-BAD ) OR ( LN-OWNBR NOT = LN-PATH-OWNBR )
              GO TO PROCESS-BRANCH-MODULE-END.

           IF EXT-ROUTE-BUF = "00"
              PERFORM SEND-PROGRESS-INDICATOR.

           IF ( LN-DLPOOL < BEG-POOL ) OR ( LN-DLPOOL > END-POOL )
              GO TO PROCESS-BRANCH-NEXT-LN.

           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO WSS-LPS-DISPLAY-LNNO.

      *     MOVE LN-SSNO(1) TO BW-SSNO.
      *     PERFORM READ-BW1-FILE.
      *     IF ( IO-BAD )
            IF BW-SSNO NOT = LN-SSNO(1)
              MOVE "*NOT ON FILE*" TO BW-LNAME
              MOVE SPACES          TO BW-FNAME.

      *     MOVE LN-CLASS TO LC-CODE.
      *     IF ( LC1-KEY NOT = HOLD-LC1-KEY )
      *        MOVE LC1-KEY TO HOLD-LC1-KEY
      *        PERFORM READ-LC1-FILE
      *        IF ( IO-BAD )
            IF LC-CODE NOT = LN-CLASS
                 GO TO IO-ERROR.

           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
      *     PERFORM READ-SP1-FILE.
           PERFORM ARRAYSP-READ.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           MOVE SPACES TO PROCESS-ACCTNO-STAT.

           PERFORM PROCESS-ACCTNO-PC.
           PERFORM PROCESS-ACCTNO-IB.

           GO TO PROCESS-BRANCH-NEXT-LN.

       PROCESS-BRANCH-MODULE-END.

      *     PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LTI1-FILE.

       PROCESS-BRANCH-MODULE-EXIT.
           EXIT.

      ******************************************************************
       PROCESS-ACCTNO-IB SECTION.
      ******************************************************************

           IF ( GP-IB-ACCRUALS-DONE NOT = "Y" )
              GO TO PROCESS-ACCTNO-IB-EXIT.

           IF ( LC-SERVACCT = "Y" )
              GO TO PROCESS-ACCTNO-IB-EXIT.

      **************************************************
      *    SKIP LOANS PAIDOFF
      *    OR IN P&L
      *    OR IN RESCISSION
      *    OR LOANS THAT HAVE BEEN RESCINDED
      *    OR VOIDED LOANS
      **************************************************
      
           IF ( LN-CURBAL     = 0      ) OR
              ( LN-PLDATE NOT = 0      ) OR
              ( LN-PLCD NOT   = SPACES ) OR
              ( LN-POCD-INRESCISSION   ) OR
              ( LN-POCD-RESCIND        ) OR
              ( LN-POCD       = "**"   )
              GO TO PROCESS-ACCTNO-IB-EXIT.

           IF ( SP-MAX-IB-ACR-DAYS = 0 )
              GO TO PROCESS-ACCTNO-IB-EXIT.

      **************************************************
      *    USED TO ALWAYS SKIP REPO ACCOUNTS ON IB ACCRUAL, FROM A60
      *    FORWARD.  AUTOMOTIVE CREDIT WANTED TO TAKE EARNINGS TILL
      *    ACCOUNT CHARGED OFF MERCURY, SKIP REPO ACCOUNTS:
      **************************************************

           IF ( GP-IB-ACCRUE-EARN-ON-REPO NOT = "Y"        ) AND
              ( LN-REPOCD                     = "X" OR "S" )
              GO TO PROCESS-ACCTNO-IB-EXIT.

           MOVE ACCRUAL-DATE TO IBPC-DATE.
           PERFORM IBPC-TEST.

           IF (  IBPC-FG           = "I"  ) OR
              (( IBPC-FG           = "P" )  AND
               ( IBPC-EXPIRED-FLAG = "E" ))
              NEXT SENTENCE
           ELSE
              GO TO PROCESS-ACCTNO-IB-EXIT.

      * VALID RECORD TO PROCESS  

           MOVE "XX" TO PROCESS-ACCTNO-STAT.
           MOVE "IB" TO CURRENT-REPORT-TYPE.
           PERFORM LOAD-REPORT-IB-FIELDS.
           PERFORM UPDATE-REPORT-FILE.

      * CHECK FOR IB EXCEPTIONS  

           IF ( LOAD-IB-MAX NOT = SPACES )
              MOVE "IBX" TO CURRENT-REPORT-TYPE
              PERFORM LOAD-REPORT-IBX-FIELDS
              PERFORM UPDATE-REPORT-FILE.

       PROCESS-ACCTNO-IB-EXIT.
           EXIT.

      ******************************************************************
       PROCESS-ACCTNO-PC SECTION.
      ******************************************************************

      * SKIP LOANS IN RESCISSION AND LOANS THAT HAVE BEEN RESCINDED

           IF ( LN-POCD-INRESCISSION ) OR
              ( LN-POCD-RESCIND      )
              GO TO PROCESS-ACCTNO-PC-EXIT.

           IF ( LN-POCD = "**" )
              MOVE LN-POFFDATE TO NUM-DATE
              IF ( LN-SALEFIN = "Y" )
                 MOVE LN-PURDATE  TO SYS-DATE
              ELSE
                 MOVE LN-LOANDATE TO SYS-DATE.

      * SKIP LOAN IF NOT DELETED(VOIDED) THIS MONTH:

           IF (  LN-POCD = "**"                )  AND
              (( NUM-YR NOT = ACCRUAL-DATE-YY )  OR
               ( NUM-MO NOT = ACCRUAL-DATE-MM ))
              GO TO PROCESS-ACCTNO-PC-EXIT.

      * SKIP LOAN IF BOOKED AND DELETED(VOIDED) THIS MONTH:

           IF ( LN-POCD = "**" ) AND
              ( NUM-YR = S-YY  ) AND
              ( NUM-MO = S-MM  )
              GO TO PROCESS-ACCTNO-PC-EXIT.

           IF ( LN-SERVACCT       = "Y"    ) AND
              ( LN-PRIOR-UNEARNED = ZEROES ) AND
              ( LN-NONACRERN      = ZEROES )
              GO TO PROCESS-ACCTNO-PC-EXIT.

           IF ( LN-PLDATE     NOT = ZEROES ) AND
              ( LN-PRIOR-UNEARNED = ZEROES ) AND
              ( LN-NONACRERN      = ZEROES )
              GO TO PROCESS-ACCTNO-PC-EXIT.

           IF ( LN-CURBAL         = ZEROES ) AND
              ( LN-PRIOR-UNEARNED = ZEROES ) AND
              ( LN-NONACRERN      = ZEROES )
              GO TO PROCESS-ACCTNO-PC-EXIT.

      *************************************************************
      * JTG CORRECTED LOGIC ABOVE FOR GP-INCL-OTH-IN-EARNED FLAG.
      *     NEEDED ADDITIONAL TEST FOR WHEN ACCOUNT HAS
      *     LN-ANTICERN(6) AND GP-INCL-OTH-IN-EARNED = "N", STILL
      *     NEED TO TAKE AN ACCRUAL OF LOAN COSTS.
      *************************************************************

           PERFORM COMPUTE-EARNINGS.

           IF ( LN-LOANTYPE       = "I"  ) AND
              ( LN-POCD       NOT = "**" ) AND
              ( EARN-TOT-ANTICERN = 0    ) AND
              ( EARN-TOT-ANTICADJ = 0    )
              IF ( GP-INCL-OTH-IN-EARNED = "Y" )
                 GO TO PROCESS-ACCTNO-PC-EXIT
              ELSE
              IF ( LN-ANTICERN(6) = 0  AND LN-ANTICERN(4) = 0)
                 GO TO PROCESS-ACCTNO-PC-EXIT.

      * VALID RECORD TO PROCESS  

           MOVE "XX" TO PROCESS-ACCTNO-STAT.
           MOVE "PC" TO CURRENT-REPORT-TYPE.
           PERFORM LOAD-REPORT-PC-FIELDS.
           PERFORM UPDATE-REPORT-FILE.
      
           IF LOAD-PC-MAX NOT = SPACES
              MOVE "PCX" TO CURRENT-REPORT-TYPE
              PERFORM LOAD-REPORT-PCX-FIELDS
              PERFORM UPDATE-REPORT-FILE.

       PROCESS-ACCTNO-PC-EXIT.
           EXIT.

      ******************************************************************
       LOAD-CALC-ADDON-EARNINGS SECTION.
      ******************************************************************

           MOVE ZEROES TO CALC-ADDON-EARNINGS.

           MOVE LN-OWNBR        TO LTI-BRNO
                                   QLTI1-WBEG-BRNO
                                   QLTI1-WEND-BRNO.
           MOVE LN-ACCTNO       TO LTI-ACCTNO
                                   QLTI1-WBEG-ACCTNO
                                   QLTI1-WEND-ACCTNO.
           MOVE 0               TO LTI-SEQNO
                                   QLTI1-WBEG-SEQNO.
           MOVE ALL "9"         TO QLTI1-WEND-SEQNO.

           PERFORM START-LTI1-FILE.

       LOAD-CALC-ADDON-EARN-NEXT.

           PERFORM READ-LTI1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO LOAD-CALC-ADDON-EARN-EXIT.

           IF (LTI-BRNO NOT = LTI-PATH-OWNBR) OR
               (LTI-ACCTNO NOT = LN-ACCTNO)
              GO TO LOAD-CALC-ADDON-EARN-EXIT.

           IF NOT (LTI-INT-REBATE = SPACES OR "C")
              GO TO LOAD-CALC-ADDON-EARN-NEXT.

           IF ( LTI-INSPREM < 0 )
              MOVE -1 TO ONE
           ELSE
              MOVE +1 TO ONE.

           MOVE LTI-ORIG-1STPYDATE   TO AERN-LN-ORIG-1STPYDATE.
           MOVE LTI-ANTIC-START-DATE TO AERN-LN-LOANDATE
                                        AERN-LN-PURDATE
                                        AERN-LN-INTDATE.

           SUBTRACT LTI-PRIOR-NODEF FROM LN-TOTNODEF
                                 GIVING AERN-LN-TOTNODEF.

           MOVE LTI-ANTICTERM  TO AERN-LN-ANTICTERM.
           MOVE "N"           TO AERN-LN-SALEFIN.
           MOVE LTI-INT-REBATE TO AERN-LN-REBATE.

           MULTIPLY ONE BY LTI-ANTICERN(1) GIVING AERN-LN-ANTICERN.
           MULTIPLY ONE BY LTI-ANTICADJ(1) GIVING AERN-LN-ANTICADJ.
           MULTIPLY ONE BY LTI-UFERN   (1) GIVING AERN-LN-UFERN.
           MULTIPLY ONE BY LTI-ACCERN  (1) GIVING AERN-LN-ACCERN.

           MOVE ACCRUAL-DATE         TO AERN-ENDDATE.
           MOVE LN-LAST-ACCRUAL-DATE TO AERN-LAST-ACCRUAL-DATE.
           MOVE "Y"                  TO AERN-ADDON-FG.
           MOVE 2                    TO AERN.

           PERFORM COMPUTE-AERNINGS.

           MULTIPLY ONE BY AERN-EARNINGS(2).
           ADD AERN-EARNINGS(2) TO CALC-ADDON-EARNINGS
                                   LTI-ACCERN(1).

      *KC->MOVE AERN-EARNINGS(2) TO LTI-INT-LAST-EARNED.
      *KC->PERFORM REWRITE-LTI1-FILE.

           GO TO LOAD-CALC-ADDON-EARN-NEXT.

       LOAD-CALC-ADDON-EARN-EXIT.
           EXIT.

      ******************************************************************
       LOAD-REPORT-IB-FIELDS SECTION.
      ******************************************************************

           MOVE SPACES TO
                LOAD-IB-DISPLAY-LNNO        LOAD-IB-LNAME
                LOAD-IB-FNAME               LOAD-IB-MAX.

           MOVE ZEROES TO
                LOAD-IB-FROM-DATE           LOAD-IB-INT-SINCE-LSTPY
                LOAD-IB-INTBAL              LOAD-IB-TRUE-ACCRUE
                LOAD-IB-ACCRUED.

           MOVE ZEROES TO MAX-IB-INTEREST.

           MOVE WSS-LPS-DISPLAY-LNNO TO LOAD-IB-DISPLAY-LNNO.
           MOVE BW-LNAME             TO LOAD-IB-LNAME.
           MOVE BW-FNAME             TO LOAD-IB-FNAME.

           PERFORM LOAD-REPORT-IB-FIELDS-STD.      

       LOAD-REPORT-IB-FIELDS-EXIT.
           EXIT.

      ******************************************************************
       LOAD-REPORT-IB-FIELDS-STD SECTION.
      ******************************************************************

           IF ( LN-INTPDTH-DATE = 0      )
              MOVE LN-INTDATE          TO LOAD-IB-FROM-DATE
           ELSE
              MOVE LN-INTPDTH-DATE     TO LOAD-IB-FROM-DATE.

      * THE IB ACCRUAL(AC1ERN) HAS THIS LOGIC TO STOP THE IB ACCRUAL
      * AND THIS REPORT NEEDS TO MATCH.

      * #1271
           IF SP-DELQ-IB-AGE-ALP NOT = " "
              IF SP-DELQ-IB-AGE NOT = 0
                 MOVE ACCRUAL-DATE TO AGEING-DATE
                 PERFORM COMPUTE-CONTRACTUAL
                 IF CONTRACTUAL >= SP-DELQ-IB-AGE
                    MOVE SP-DELQ-IB-MAX-DAYS TO SP-MAX-IB-ACR-DAYS.

      *************************************************************
      * THIS ROUTINE WILL ESTABLISH THE MAXIMUM INTEREST ASSESS
      * FROM THE LAST ACCRUED DATE, PER SP-MAX-IB-ACR-DAYS:
      *
      *         RE: MOVE 1 TO INDU-MAX-ACCRUE
      *************************************************************

           IF ( SP-MAX-IB-ACR-DAYS NOT = 0      )
              IF ( LN-INTPDTH-DATE = 0      )
                 MOVE LN-INTDATE      TO NDTE-DATE CALC-INTPDTH-DATE
              ELSE
                 MOVE LN-INTPDTH-DATE TO NDTE-DATE CALC-INTPDTH-DATE
              END-IF
              MOVE SP-MAX-IB-ACR-DAYS  TO NDTE-HOLD
              PERFORM INCREMENT-DAYS
              MOVE NDTE-DATE           TO RATE-DATE
              MOVE LN-MAKERCD(1)       TO RATE-MAKERCD
              MOVE "PY"                TO RATE-LPTRCD
              MOVE 1                   TO INDU-MAX-ACCRUE
              PERFORM SET-PY-RATE
              MOVE INDU-INTEREST       TO MAX-IB-INTEREST.

      *************************************************************
      *    THIS ROUTINE WILL ESTABLISH THE ACCRUED INTEREST ASSESS
      *    FROM THE LAST ACCRUED DATE
      *
      *         RE: MOVE 0 TO INDU-MAX-ACCRUE
      *************************************************************

           MOVE ACCRUAL-DATE           TO RATE-DATE.
           MOVE LN-MAKERCD(1)          TO RATE-MAKERCD.
           MOVE "PY"                   TO RATE-LPTRCD.
           MOVE ZEROES                 TO INDU-MAX-ACCRUE.
           PERFORM SET-PY-RATE.
           MOVE INDU-INTEREST          TO LOAD-IB-INT-SINCE-LSTPY.
           MOVE LN-INTBAL              TO LOAD-IB-INTBAL.

           COMPUTE LOAD-IB-TRUE-ACCRUE = LN-INTBAL + INDU-INTEREST.

           IF ( SP-MAX-IB-ACR-DAYS NOT = 999             ) AND
              ( LOAD-IB-TRUE-ACCRUE    > MAX-IB-INTEREST )
              MOVE MAX-IB-INTEREST     TO LOAD-IB-ACCRUED
              MOVE "*"                 TO LOAD-IB-MAX
           ELSE
              MOVE LOAD-IB-TRUE-ACCRUE TO LOAD-IB-ACCRUED.

           MOVE LN-ACTIONCD2           TO LOAD-IB-ACTIONCD2.

       LOAD-REPORT-IB-FD-STD-EXIT.
           EXIT.

      ******************************************************************
       LOAD-REPORT-IBX-FIELDS SECTION.
      ******************************************************************

      * NORMAL ACCOUNT IB EXCEPTION SETUP FOR ACCRUAL

           MOVE SPACES TO
                LOAD-IBX-DISPLAY-LNNO       LOAD-IBX-LNAME
                LOAD-IBX-FNAME.

           MOVE SPACES TO
                LOAD-IBX-INT-PDTH-DATE      LOAD-IBX-INT-SINCE-LSTPY
                LOAD-IBX-INT-OWED           LOAD-IBX-TRUE-ACCRUE
                LOAD-IBX-INCOME-ACCRUE      LOAD-IBX-BALANCE
                LOAD-IBX-CONTR-PDTH-DATE    LOAD-IBX-CONTR-STATUS.

           MOVE WSS-LPS-DISPLAY-LNNO    TO LOAD-IBX-DISPLAY-LNNO.
           MOVE BW-LNAME                TO LOAD-IBX-LNAME.
           MOVE BW-FNAME                TO LOAD-IBX-FNAME.
           MOVE CALC-INTPDTH-DATE       TO LOAD-IBX-INT-PDTH-DATE.
           MOVE LOAD-IB-INT-SINCE-LSTPY TO LOAD-IBX-INT-SINCE-LSTPY.
           MOVE LOAD-IB-INTBAL          TO LOAD-IBX-INT-OWED.
           MOVE LOAD-IB-TRUE-ACCRUE     TO LOAD-IBX-TRUE-ACCRUE.
           MOVE LOAD-IB-ACCRUED         TO LOAD-IBX-INCOME-ACCRUE.
           MOVE LN-CURBAL               TO LOAD-IBX-BALANCE.

           MOVE LN-1STPYDATE            TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-DATE-FULL          TO LOAD-IBX-CONTR-PDTH-DATE.

           MOVE ACCRUAL-DATE            TO AGEING-DATE.
           PERFORM COMPUTE-CONTRACTUAL.
           MOVE CONTRACTUAL             TO LOAD-IBX-CONTR-STATUS.
           MOVE LN-ACTIONCD2            TO LOAD-IBX-ACTIONCD2.

       LOAD-REPORT-IBX-FLDS-EXIT.
           EXIT.

      ******************************************************************
       LOAD-REPORT-PC-FIELDS SECTION.
      ******************************************************************

           MOVE SPACES TO
                LOAD-PC-DISPLAY-LNNO        LOAD-PC-LNAME
                LOAD-PC-FNAME               LOAD-PC-MAX.

           MOVE ZEROES TO
                LOAD-PC-ANTERM              LOAD-PC-ANTIC
                LOAD-PC-UNEARN              LOAD-PC-INT
                LOAD-PC-ADDON               LOAD-PC-SRV
                LOAD-PC-MFEE                LOAD-PC-DISC
                LOAD-PC-OTHER               LOAD-PC-CASH-EARN
                LOAD-PC-ENDING.

           MOVE WSS-LPS-DISPLAY-LNNO    TO LOAD-PC-DISPLAY-LNNO.
           MOVE BW-LNAME                TO LOAD-PC-LNAME.
           MOVE BW-FNAME                TO LOAD-PC-FNAME.

      * COMPUTE ACCRUAL EARNINGS 

           IF ( LN-POCD = "**" )
              MOVE ZEROES               TO EARN-TOT-UNEARN
           ELSE
              MOVE ACCRUAL-DATE         TO AERN-ENDDATE
              MOVE LN-LAST-ACCRUAL-DATE TO AERN-LAST-ACCRUAL-DATE
              PERFORM COMPUTE-AERNINGS
              IF AERN-ERNDELQ-STOP = "R"
                 MOVE "R" TO LOAD-PC-MAX
              END-IF
              MOVE AERN-EARNINGS(1)     TO LOAD-PC-INT
              MOVE AERN-EARNINGS(3)     TO LOAD-PC-SRV
              MOVE AERN-EARNINGS(4)     TO LOAD-PC-MFEE
              MOVE AERN-EARNINGS(5)     TO LOAD-PC-DISC
              MOVE AERN-EARNINGS(6)     TO LOAD-PC-OTHER
              MOVE 0                    TO LOAD-PC-ADDON
              IF ( LN-ADDON-FG   = "Y"           ) AND
                 ( LN-INT-REBATE = SPACES OR "C" )
                 PERFORM LOAD-CALC-ADDON-EARNINGS
                 MOVE CALC-ADDON-EARNINGS TO LOAD-PC-ADDON.

           MOVE LN-ANTICTERM            TO LOAD-PC-ANTERM.
           MOVE LN-NONACRERN            TO LOAD-PC-CASH-EARN.
           MOVE LN-PRIOR-UNEARNED       TO LOAD-PC-UNEARN.

           COMPUTE LOAD-PC-ANTIC         = EARN-TOT-ANTICADJ
                                         + EARN-TOT-ANTICERN.

           COMPUTE LOAD-PC-ENDING        = EARN-TOT-UNEARN
                                         - LOAD-PC-INT
                                         - LOAD-PC-ADDON
                                         - LOAD-PC-SRV
                                         - LOAD-PC-DISC.

           IF NOT (GP-INCL-OTH-IN-EARNED-N OR GP-INCL-OTH-IN-EARNED-R)
              SUBTRACT LOAD-PC-OTHER FROM LOAD-PC-ENDING.

      * REGACC DOES NOT WANT TO INCLUDE MFEE (THEY USE TO AMORTIZE
      * PARTICIPATION EXPENSE
           IF NOT (GP-INCL-OTH-IN-EARNED-R)
              SUBTRACT LOAD-PC-MFEE FROM LOAD-PC-ENDING.

           MOVE LN-ACTIONCD2            TO LOAD-PC-ACTIONCD2.

       LOAD-REPORT-PC-FIELDS-EXIT.
           EXIT.

      ******************************************************************
       LOAD-REPORT-PCX-FIELDS SECTION.
      ******************************************************************

           MOVE SPACES TO
                LOAD-PCX-DISPLAY-LNNO        LOAD-PCX-LNAME
                LOAD-PCX-FNAME.           

           MOVE ZEROES TO
                LOAD-PCX-ANTERM              LOAD-PCX-ANTIC
                LOAD-PCX-UNEARN              LOAD-PCX-INT
                LOAD-PCX-ADDON               LOAD-PCX-SRV
                LOAD-PCX-MFEE                LOAD-PCX-DISC
                LOAD-PCX-OTHER               LOAD-PCX-ROLLBACK   
                LOAD-PCX-CONTRACTUAL.
      
           MOVE WSS-LPS-DISPLAY-LNNO    TO LOAD-PCX-DISPLAY-LNNO.
           MOVE BW-LNAME                TO LOAD-PCX-LNAME.
           MOVE BW-FNAME                TO LOAD-PCX-FNAME.

      * COMPUTE ACCRUAL EARNINGS 

           IF ( LN-POCD = "**" )
              MOVE ZEROES               TO EARN-TOT-UNEARN
           ELSE
              MOVE ACCRUAL-DATE         TO AERN-ENDDATE
              MOVE LN-LAST-ACCRUAL-DATE TO AERN-LAST-ACCRUAL-DATE
              PERFORM COMPUTE-AERNINGS
              MOVE AERN-EARNINGS(1)     TO LOAD-PCX-INT
              MOVE AERN-EARNINGS(3)     TO LOAD-PCX-SRV
              MOVE AERN-EARNINGS(4)     TO LOAD-PCX-MFEE
              MOVE AERN-EARNINGS(5)     TO LOAD-PCX-DISC
              MOVE AERN-EARNINGS(6)     TO LOAD-PCX-OTHER
              MOVE 0                    TO LOAD-PCX-ADDON
              IF ( LN-ADDON-FG   = "Y"           ) AND
                 ( LN-INT-REBATE = SPACES OR "C" )
                   PERFORM LOAD-CALC-ADDON-EARNINGS
                   MOVE CALC-ADDON-EARNINGS TO LOAD-PCX-ADDON.

           MOVE LN-ANTICTERM            TO LOAD-PCX-ANTERM.
           MOVE CONTRACTUAL             TO LOAD-PCX-CONTRACTUAL.

           COMPUTE LOAD-PCX-ANTIC         = EARN-TOT-ANTICADJ
                                         + EARN-TOT-ANTICERN.

           COMPUTE LOAD-PCX-ROLLBACK = LOAD-PCX-INT + 
                                       LOAD-PCX-SRV +
                                       LOAD-PCX-MFEE +
                                       LOAD-PCX-DISC +
                                       LOAD-PCX-OTHER.

           IF (GP-INCL-OTH-IN-EARNED-N OR GP-INCL-OTH-IN-EARNED-R)
             SUBTRACT LOAD-PCX-OTHER FROM   LOAD-PCX-ROLLBACK.

      * REGACC DOES NOT WANT TO INCLUDE MFEE (THEY USE TO AMORTIZE
      * PARTICIPATION EXPENSE 
           IF (GP-INCL-OTH-IN-EARNED-R)                         
              SUBTRACT LOAD-PCX-MFEE FROM LOAD-PCX-ROLLBACK.

           MOVE LN-ACTIONCD2            TO LOAD-PCX-ACTIONCD2.

       LOAD-REPORT-PCX-FIELDS-EXIT.
           EXIT.

      ******************************************************************
       PRINT-REPORT-MODULE SECTION.
      ******************************************************************
           IF (LOCAL-PRINT-FG = "N")
              GO TO PRM-BRANCH-ROUTINE.

           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-BR-FILE.

           IF ( GROUP-FLAG = "N" )
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO PRM-EXECUTION-NEXT-BRANCH.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

       PRM-EXECUTION-NEXT-GROUP.

           IF ( GROUP-FLAG = "N" )
              GO TO PRM-EXECUTION-NEXT-BRANCH.

           PERFORM SEARCH-GR-NEXT.
           IF ( WGR-SRESULT = ZEROES )
              GO TO PRINT-REPORT-MODULE-END.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO PRM-EXECUTION-PROCESS-BRANCH.

       PRM-EXECUTION-NEXT-BRANCH.

           PERFORM READ-BR-FILE-NEXT.
           IF ( IO-BAD             ) OR
              ( BR-NO > END-BRANCH )
              GO TO PRINT-REPORT-MODULE-END.

       PRM-EXECUTION-PROCESS-BRANCH.
           PERFORM CLOSE-PR-FILE.
           PERFORM CREATE-SPOOL-DIR.
           IF ( NOT STAT-VALID )
              GO TO PRM-EXECUTION-NEXT-GROUP.

      * SUB-REPORT-TYPE IS 1 THRU 4

           MOVE ZEROES TO SUB-REPORT-TYPE.

       PRM-BRANCH-NEXT-TYPE.

           ADD 01 TO SUB-REPORT-TYPE.
           IF ( SUB-REPORT-TYPE > 4 )
              GO TO PRM-EXECUTION-NEXT-GROUP.

       PRM-BRANCH-ROUTINE.

           MOVE SPACES               TO CURRENT-REPORT-TYPE.
           MOVE ZEROES               TO HOLD-BRANCH.
           MOVE ZEROES               TO HOLD-CLASS.
           MOVE ZEROES               TO PAGE-NUMBER-TOTAL.
           MOVE ZEROES               TO PAGE-NUMBER-TYPE.
           MOVE PROGID-TEXT-PRINTING TO PROGID-TEXT.
           MOVE 9999                 TO PROGID-SLEEP-COUNTER.

      * START 'REPORT' FILE  

           MOVE SPACES TO REPORT-TYPE     HOLD-REPORT-TYPE.
           MOVE ZEROES TO REPORT-BRANCH   HOLD-REPORT-BRANCH.
           MOVE ZEROES TO REPORT-POOL     HOLD-REPORT-POOL.
           MOVE ZEROES TO REPORT-CLASS    HOLD-REPORT-CLASS.
           MOVE ZEROES TO REPORT-ACCTNO   HOLD-REPORT-ACCTNO.

           IF ( LOCAL-PRINT-FG = "Y" )
              IF ( SUB-REPORT-TYPE = 1 )
                 MOVE "IB"  TO REPORT-TYPE   HOLD-REPORT-TYPE
                 MOVE BR-NO TO REPORT-BRANCH HOLD-REPORT-BRANCH
              ELSE
              IF ( SUB-REPORT-TYPE = 2 )
                 MOVE "IBX" TO REPORT-TYPE   HOLD-REPORT-TYPE
                 MOVE BR-NO TO REPORT-BRANCH HOLD-REPORT-BRANCH
              ELSE
              IF ( SUB-REPORT-TYPE = 3 )
                 MOVE "PC"  TO REPORT-TYPE   HOLD-REPORT-TYPE
                 MOVE BR-NO TO REPORT-BRANCH HOLD-REPORT-BRANCH
              ELSE
                 MOVE "PCX" TO REPORT-TYPE   HOLD-REPORT-TYPE
                 MOVE BR-NO TO REPORT-BRANCH HOLD-REPORT-BRANCH.

           PERFORM START-REPORT-FILE.

       PRINT-REPORT-RECORD-NEXT.

           PERFORM READ-REPORT-FILE-NEXT.
           IF ( IO-BAD )
              IF (LOCAL-PRINT-FG = "Y")
                 GO TO PRM-EXECUTION-NEXT-GROUP
              ELSE
                 GO TO PRINT-REPORT-MODULE-END.

      *****************************************************************
      *    FOR DOING EXPORT FILE ONLY!
      *    FOR DOING EXPORT FILE WITH REPORT ("Y") IS DONE FURTHER
      *    BELOW IN THE AREA OF PRINTING DETAIL ANALYSIS.
      *****************************************************************

      * DETAIL RECORDS
           IF ( EXPORT-FILE-YNO = "O" ) 
              IF ( REPORT-GRAND-TOTAL  ) OR
                 ( REPORT-BRANCH-TOTAL ) OR
                 ( REPORT-POOL-TOTAL   ) OR
                 ( REPORT-CLASS-TOTAL  )
                 GO TO PRINT-REPORT-RECORD-NEXT
              ELSE
                 MOVE "Y" TO RECORDS-FOUND
                 PERFORM UPDATE-EXPORT-FILE
                 GO TO PRINT-REPORT-RECORD-NEXT.

           IF ( LOCAL-PRINT-FG = "Y" )
              IF ( REPORT-BRANCH NOT = BR-NO )
                 GO TO PRM-BRANCH-NEXT-TYPE       
              ELSE
              IF ( REPORT-TYPE = "IB"  AND SUB-REPORT-TYPE NOT = 1 ) OR
                 ( REPORT-TYPE = "IBX" AND SUB-REPORT-TYPE NOT = 2 ) OR
                 ( REPORT-TYPE = "PC"  AND SUB-REPORT-TYPE NOT = 3 ) OR
                 ( REPORT-TYPE = "PCX" AND SUB-REPORT-TYPE NOT = 4 )
                 GO TO PRM-BRANCH-NEXT-TYPE.

           MOVE REPORT-BRANCH TO BR-NO.
           PERFORM SEND-PROGRESS-INDICATOR.

      * CHECK REPORT TYPE  

           IF (( PRINT-LOAN-TYPE-IB  = "N"  ) AND ( REPORT-TYPE-IB )) OR
              (( PRINT-LOAN-TYPE-IBX = "N"  ) AND ( REPORT-TYPE-IBX)) OR
              (( PRINT-LOAN-TYPE-PC  = "N"  ) AND ( REPORT-TYPE-PC )) OR
              (( PRINT-LOAN-TYPE-PCX = "N"  ) AND ( REPORT-TYPE-PCX)) 
              GO TO PRINT-REPORT-RECORD-NEXT.

      * CHECK REPORT OPTIONS 

           IF ( REPORT-GRAND-TOTAL )
              IF ( PRINT-GRAND-TOTALS = "Y" )
                 GO TO PRINT-REPORT-RECORD-VALID
              ELSE
                 GO TO PRINT-REPORT-RECORD-NEXT.

           IF ( REPORT-BRANCH-TOTAL )
              IF ( PRINT-BRANCH-TOTALS = "Y" )
                 GO TO PRINT-REPORT-RECORD-VALID
              ELSE
                 GO TO PRINT-REPORT-RECORD-NEXT.

           IF ( REPORT-POOL-TOTAL )
              IF ( PRINT-POOL-TOTALS = "Y" )
                 GO TO PRINT-REPORT-RECORD-VALID
              ELSE
                 GO TO PRINT-REPORT-RECORD-NEXT.

           IF ( REPORT-CLASS-TOTAL )
              IF ( PRINT-CLASS-TOTALS = "Y" )
                 GO TO PRINT-REPORT-RECORD-VALID
              ELSE
                 GO TO PRINT-REPORT-RECORD-NEXT.

      * FOR DETAIL RECORDS
           IF ( EXPORT-FILE-YNO = "Y" OR "O" )
              MOVE "Y" TO RECORDS-FOUND
              PERFORM UPDATE-EXPORT-FILE.

           IF ( PRINT-DETAIL-ANALYSIS = "Y"  )
              GO TO PRINT-REPORT-RECORD-VALID.

           GO TO PRINT-REPORT-RECORD-NEXT.

       PRINT-REPORT-RECORD-VALID.

           PERFORM PRINT-REPORT-PAGE-BREAK.

           IF ( REPORT-TYPE-IB  ) PERFORM PRINT-REPORT-IB.
           IF ( REPORT-TYPE-IBX ) PERFORM PRINT-REPORT-IBX.
           IF ( REPORT-TYPE-PC  ) PERFORM PRINT-REPORT-PC.
           IF ( REPORT-TYPE-PCX ) PERFORM PRINT-REPORT-PCX.

           MOVE "Y" TO RECORDS-FOUND.

           GO TO PRINT-REPORT-RECORD-NEXT.

       PRINT-REPORT-MODULE-END.

           IF (LOCAL-PRINT-FG = "N")
               PERFORM PRINT-REPORT-RANGES.

       PRINT-REPORT-MODULE-EXIT.
           EXIT.

      ******************************************************************
       PRINT-REPORT-PAGE-BREAK SECTION.
      ******************************************************************

      * PAGE BREAK ONLY WHEN GOING TO PRINT A 'DETAIL' RECORD AFTER
      * JUST PRINTING A 'TOTAL' RECORD; OR IF A CHANGE OF REPORT TYPE

           IF (( NOT CURRENT-REPORT-TYPE-IB ) AND ( REPORT-TYPE-IB )) OR
              (( NOT CURRENT-REPORT-TYPE-IBX) AND ( REPORT-TYPE-IBX)) OR
              (( NOT CURRENT-REPORT-TYPE-PC ) AND ( REPORT-TYPE-PC )) OR
              (( NOT CURRENT-REPORT-TYPE-PCX) AND (REPORT-TYPE-PCX))

              MOVE REPORT-TYPE TO CURRENT-REPORT-TYPE
              MOVE 99          TO LN-COUNT
              MOVE ZEROES      TO PAGE-NUMBER-TYPE
              GO TO PRINT-REPORT-PG-BREAK-END.

      ******************************************************************
      *  DO NOT CHANGE LN-COUNT IF PRINTING A 'TOTAL' RECORD.
      *  ALLOW TOTALS TO PRINT ON SAME PAGE.
      ******************************************************************
      
           IF ( REPORT-GRAND-TOTAL  ) OR
              ( REPORT-BRANCH-TOTAL ) OR
              ( REPORT-POOL-TOTAL   ) OR
              ( REPORT-CLASS-TOTAL  )
              GO TO PRINT-REPORT-PG-BREAK-END.

      ******************************************************************
      * AT THIS POINT THE CURRENT RECORD IS FOR A 'DETAIL' RECORD.
      * IF THE PREVOIUS RECORD WAS A 'TOTAL' RECORD, DO A PAGE BREAK.
      ******************************************************************
      
           IF ( HOLD-REPORT-GRAND-TOTAL  ) OR
              ( HOLD-REPORT-BRANCH-TOTAL ) OR
              ( HOLD-REPORT-POOL-TOTAL   ) OR
              ( HOLD-REPORT-CLASS-TOTAL  )
              MOVE 99 TO LN-COUNT.

       PRINT-REPORT-PG-BREAK-END.

           MOVE REPORT-KEY TO HOLD-REPORT-KEY.

       PRINT-REPORT-PG-BREAK-EXIT.
           EXIT.

      ******************************************************************
       PRINT-REPORT-RANGES SECTION.
      ******************************************************************

           IF ( NO-RECORDS-FOUND )
              GO TO PRINT-REPORT-RANGES-EXIT.

           MOVE 99     TO LN-COUNT.
           MOVE 1      TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

      
           IF ( GROUP-FLAG = "Y" )
              MOVE "GROUP ID NAME                 : " TO R01-PROMPT
              MOVE ENT-GROUP                          TO R01-TEXT
              MOVE 2                                  TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEGINNING BRANCH NUMBER       : " TO R01-PROMPT
              MOVE BEG-BRANCH                         TO R01-NUMERIC
              MOVE 2                                  TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING    BRANCH NUMBER       : " TO R01-PROMPT
              MOVE END-BRANCH                         TO R01-NUMERIC
              MOVE 1                                  TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING POOL ID             : "    TO R01-PROMPT.
           MOVE BEG-POOL                              TO R01-NUMERIC.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    POOL ID             : "    TO R01-PROMPT.
           MOVE BEG-POOL                              TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ACCRUAL DATE (MM/DD/YY)       : "    TO R01-PROMPT.
           MOVE ACCRUAL-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                           TO R01-DATE.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "REPORT BREAKDOWN BY CLASS     : "    TO R01-PROMPT.
           MOVE PRINT-BY-CLASS                        TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "REPORT BREAKDOWN BY POOL      : "    TO R01-PROMPT.
           MOVE PRINT-BY-POOL                         TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "REPORT INTEREST BEARING       : "    TO R01-PROMPT.
           MOVE PRINT-LOAN-TYPE-IB                    TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "REPORT INTEREST BEARING EXCEPT: "    TO R01-PROMPT.
           MOVE PRINT-LOAN-TYPE-IBX                   TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "REPORT PRECOMPUTE             : "    TO R01-PROMPT.
           MOVE PRINT-LOAN-TYPE-PC                    TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "REPORT PRECOMPUTE EXCEPT:     : "    TO R01-PROMPT.
           MOVE PRINT-LOAN-TYPE-PCX                   TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PRINT DETAIL ANALYSIS         : "    TO R01-PROMPT.
           MOVE PRINT-DETAIL-ANALYSIS                 TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PRINT CLASS  TOTALS           : "    TO R01-PROMPT.
           MOVE PRINT-CLASS-TOTALS                    TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PRINT POOL   TOTALS           : "    TO R01-PROMPT.
           MOVE PRINT-POOL-TOTALS                     TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PRINT BRANCH TOTALS           : "    TO R01-PROMPT.
           MOVE PRINT-BRANCH-TOTALS                   TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PRINT GRAND  TOTALS           : "    TO R01-PROMPT.
           MOVE PRINT-GRAND-TOTALS                    TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "STARTING TIME                 : "    TO R01-PROMPT.
           MOVE STARTING-TIME                         TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING   TIME                 : "    TO R01-PROMPT.
           PERFORM GET-TIME.
           MOVE TIME-EDIT                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-REPORT-RANGES-EXIT.
           EXIT.

      ******************************************************************
       PRINT-REPORT-IB SECTION.
      ******************************************************************

           IF ( REPORT-GRAND-TOTAL  )  OR
              ( REPORT-BRANCH-TOTAL )  OR
              ( REPORT-POOL-TOTAL   )  OR
              ( REPORT-CLASS-TOTAL  )
              IF ( PRINT-DETAIL-ANALYSIS = "Y" )
                 GO TO PRINT-REPORT-IB-TOTAL
              ELSE
                 GO TO PRINT-REPORT-IB-SUMMARY.

       PRINT-REPORT-IB-DETAIL.

           MOVE REPORT-IB-DISPLAY-LNNO     TO WSS-LPS-DISPLAY-LNNO.
           MOVE WSS-LPS-NUMBER-7STATUS     TO D01-IB-DISPLAY-LNNO.

           STRING REPORT-IB-LNAME, ", ", REPORT-IB-FNAME
                  DELIMITED BY "   "      INTO D01-IB-NAME.

           MOVE REPORT-IB-FROM-DATE        TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                TO D01-IB-FROM.
           MOVE REPORT-IB-INT-SINCE-LSTPY  TO D01-IB-INT-SINCE-LSTPY.
           MOVE REPORT-IB-INTBAL           TO D01-IB-INTBAL.
           MOVE REPORT-IB-TRUE-ACCRUE      TO D01-IB-TRUE-ACCRUE.
           MOVE REPORT-IB-ACCRUED          TO D01-IB-ACCRUED.
           MOVE REPORT-IB-MAX              TO D01-IB-MAX.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           GO TO PRINT-REPORT-IB-EXIT.

       PRINT-REPORT-IB-SUMMARY.

      * TOTAL SUMMARY ONLY 

           IF ( REPORT-GRAND-TOTAL )
              MOVE "****"                  TO S01-IB-BRANCHX
           ELSE
              MOVE REPORT-BRANCH           TO S01-IB-BRANCH.

           IF ( REPORT-BRANCH-TOTAL )
              MOVE "****"                  TO S01-IB-POOLX
           ELSE
              MOVE REPORT-POOL             TO S01-IB-POOL.

           IF ( REPORT-POOL-TOTAL )
              MOVE "**"                    TO S01-IB-CLASSX
           ELSE
              MOVE REPORT-CLASS            TO S01-IB-CLASS.

           MOVE REPORT-IB-NUMBER-ACCTS     TO S01-IB-NUMBER-ACCTS.
           MOVE "#"                        TO S01-IB-NUMBER-ACCTS-FG.
           MOVE REPORT-IB-INT-SINCE-LSTPY  TO S01-IB-INT-SINCE-LSTPY.
           MOVE REPORT-IB-INTBAL           TO S01-IB-INTBAL.
           MOVE REPORT-IB-TRUE-ACCRUE      TO S01-IB-TRUE-ACCRUE.
           MOVE REPORT-IB-ACCRUED          TO S01-IB-ACCRUED.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           GO TO PRINT-REPORT-IB-EXIT.

       PRINT-REPORT-IB-TOTAL.

           IF ( REPORT-GRAND-TOTAL )
              MOVE "GRAND TOTALS         " TO T01-IB-PROMPT
           ELSE
           IF ( REPORT-BRANCH-TOTAL )
              MOVE "BRANCH TOTALS        " TO T01-IB-PROMPT
           ELSE
           IF ( REPORT-POOL-TOTAL )
              MOVE "POOL TOTALS          " TO T01-IB-PROMPT
           ELSE
              MOVE "CLASS TOTALS         " TO T01-IB-PROMPT.

           MOVE REPORT-IB-NUMBER-ACCTS     TO T01-IB-NUMBER-ACCTS.
           MOVE "#"                        TO T01-IB-NUMBER-ACCTS-FG.
           MOVE REPORT-IB-INT-SINCE-LSTPY  TO T01-IB-INT-SINCE-LSTPY.
           MOVE REPORT-IB-INTBAL           TO T01-IB-INTBAL.
           MOVE REPORT-IB-TRUE-ACCRUE      TO T01-IB-TRUE-ACCRUE.
           MOVE REPORT-IB-ACCRUED          TO T01-IB-ACCRUED.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           GO TO PRINT-REPORT-IB-EXIT.

       PRINT-REPORT-IB-EXIT.
           EXIT.

      ******************************************************************
       PRINT-REPORT-IBX SECTION.
      ******************************************************************

           IF ( REPORT-GRAND-TOTAL  )  OR
              ( REPORT-BRANCH-TOTAL )  OR
              ( REPORT-POOL-TOTAL   )  OR
              ( REPORT-CLASS-TOTAL  )
              IF ( PRINT-DETAIL-ANALYSIS = "Y" )
                 GO TO PRINT-REPORT-IBX-TOTAL
              ELSE
                 GO TO PRINT-REPORT-IBX-SUMMARY.

       PRINT-REPORT-IBX-DETAIL.

           MOVE REPORT-IBX-DISPLAY-LNNO    TO WSS-LPS-DISPLAY-LNNO.
           MOVE WSS-LPS-NUMBER-7STATUS     TO D02-IBX-DISPLAY-LNNO.

           STRING REPORT-IBX-LNAME, ", ", REPORT-IBX-FNAME
                  DELIMITED BY "   "      INTO D02-IBX-NAME.

           MOVE REPORT-IBX-INT-PDTH-DATE   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                TO D02-IBX-INT-PDTH.
           MOVE REPORT-IBX-INT-SINCE-LSTPY TO D02-IBX-INT-SINCE-LSTPY.
           MOVE REPORT-IBX-INT-OWED        TO D02-IBX-INT-OWED.
           MOVE REPORT-IBX-TRUE-ACCRUE     TO D02-IBX-TRUE-ACCRUE.
           MOVE REPORT-IBX-INCOME-ACCRUE   TO D02-IBX-INCOME-ACCRUE.
           MOVE REPORT-IBX-BALANCE         TO D02-IBX-BALANCE.
           MOVE REPORT-IBX-CONTR-PDTH-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                TO D02-IBX-CONT-PDTH.
           MOVE REPORT-IBX-CONTR-STATUS    TO D02-IBX-CONT-STATUS.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           GO TO PRINT-REPORT-IBX-EXIT.

       PRINT-REPORT-IBX-SUMMARY.

      * TOTAL SUMMARY ONLY 

           IF ( REPORT-GRAND-TOTAL )
              MOVE "****"                  TO S02-IBX-BRANCHX
           ELSE
              MOVE REPORT-BRANCH           TO S02-IBX-BRANCH.

           IF ( REPORT-BRANCH-TOTAL )
              MOVE "****"                  TO S02-IBX-POOLX
           ELSE
              MOVE REPORT-POOL             TO S02-IBX-POOL.

           IF ( REPORT-POOL-TOTAL )
              MOVE "**"                    TO S02-IBX-CLASSX
           ELSE
              MOVE REPORT-CLASS            TO S02-IBX-CLASS.

           MOVE REPORT-IBX-NUMBER-ACCTS    TO S02-IBX-NUMBER-ACCTS.
           MOVE "#"                        TO S02-IBX-NUMBER-ACCTS-FG.
           MOVE REPORT-IBX-INT-SINCE-LSTPY TO S02-IBX-INT-SINCE-LSTPY.
           MOVE REPORT-IBX-INT-OWED        TO S02-IBX-INT-OWED.
           MOVE REPORT-IBX-TRUE-ACCRUE     TO S02-IBX-TRUE-ACCRUE.
           MOVE REPORT-IBX-INCOME-ACCRUE   TO S02-IBX-INCOME-ACCRUE.
           MOVE REPORT-IBX-BALANCE         TO S02-IBX-BALANCE.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           GO TO PRINT-REPORT-IBX-EXIT.

       PRINT-REPORT-IBX-TOTAL.

           IF ( REPORT-GRAND-TOTAL )
              MOVE "GRAND TOTALS          " TO T02-IBX-PROMPT
           ELSE
           IF ( REPORT-BRANCH-TOTAL )
              MOVE "BRANCH TOTALS         " TO T02-IBX-PROMPT
           ELSE
           IF ( REPORT-POOL-TOTAL )
              MOVE "POOL TOTALS           " TO T02-IBX-PROMPT
           ELSE
              MOVE "CLASS TOTALS          " TO T02-IBX-PROMPT.

           MOVE REPORT-IBX-NUMBER-ACCTS     TO T02-IBX-NUMBER-ACCTS.
           MOVE "#"                         TO T02-IBX-NUMBER-ACCTS-FG.
           MOVE REPORT-IBX-INT-SINCE-LSTPY  TO T02-IBX-INT-SINCE-LSTPY.
           MOVE REPORT-IBX-INT-OWED         TO T02-IBX-INT-OWED.
           MOVE REPORT-IBX-TRUE-ACCRUE      TO T02-IBX-TRUE-ACCRUE.
           MOVE REPORT-IBX-INCOME-ACCRUE    TO T02-IBX-INCOME-ACCRUE.
           MOVE REPORT-IBX-BALANCE          TO T02-IBX-BALANCE.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           GO TO PRINT-REPORT-IBX-EXIT.

       PRINT-REPORT-IBX-EXIT.
           EXIT.

      ******************************************************************
       PRINT-REPORT-PC SECTION.
      ******************************************************************

           IF ( REPORT-GRAND-TOTAL  )  OR
              ( REPORT-BRANCH-TOTAL )  OR
              ( REPORT-POOL-TOTAL   )  OR
              ( REPORT-CLASS-TOTAL  )
              IF ( PRINT-DETAIL-ANALYSIS = "Y" )
                 GO TO PRINT-REPORT-PC-TOTAL
              ELSE
                 GO TO PRINT-REPORT-PC-SUMMARY.

       PRINT-REPORT-PC-DETAIL.

           MOVE REPORT-PC-DISPLAY-LNNO     TO WSS-LPS-DISPLAY-LNNO.
           MOVE WSS-LPS-NUMBER-7STATUS     TO D03-PC-DISPLAY-LNNO.

           STRING REPORT-PC-LNAME, ", ", REPORT-PC-FNAME
                  DELIMITED BY "   "      INTO D03-PC-NAME.

           MOVE REPORT-PC-ANTERM           TO D03-PC-ANTERM.
           MOVE REPORT-PC-ANTIC            TO D03-PC-ANTIC.
           MOVE REPORT-PC-UNEARN           TO D03-PC-UNEARN.
           MOVE REPORT-PC-INT              TO D03-PC-INT.
           MOVE REPORT-PC-ADDON            TO D03-PC-ADDON.
           MOVE REPORT-PC-SRV              TO D03-PC-SRV.
           MOVE REPORT-PC-MFEE             TO D03-PC-MFEE.
           MOVE REPORT-PC-DISC             TO D03-PC-DISC.
           MOVE REPORT-PC-OTHER            TO D03-PC-OTHER.
           MOVE REPORT-PC-CASH-EARN        TO D03-PC-CASH-EARN.
           MOVE REPORT-PC-ENDING           TO D03-PC-ENDING.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           GO TO PRINT-REPORT-PC-EXIT.

       PRINT-REPORT-PC-SUMMARY.

      * TOTAL SUMMARY ONLY 

           IF ( REPORT-GRAND-TOTAL )
              MOVE "****"                  TO S03-PC-BRANCHX
           ELSE
              MOVE REPORT-BRANCH           TO S03-PC-BRANCH.

           IF ( REPORT-BRANCH-TOTAL )
              MOVE "****"                  TO S03-PC-POOLX
           ELSE
              MOVE REPORT-POOL             TO S03-PC-POOL.

           IF ( REPORT-POOL-TOTAL )
              MOVE "**"                    TO S03-PC-CLASSX
           ELSE
              MOVE REPORT-CLASS            TO S03-PC-CLASS.

           MOVE REPORT-PC-NUMBER-ACCTS     TO S03-PC-NUMBER-ACCTS.
           MOVE "#"                        TO S03-PC-NUMBER-ACCTS-FG.
           MOVE REPORT-PC-UNEARN           TO S03-PC-UNEARN.
           MOVE REPORT-PC-INT              TO S03-PC-INT.
           MOVE REPORT-PC-ADDON            TO S03-PC-ADDON.
           MOVE REPORT-PC-SRV              TO S03-PC-SRV.
           MOVE REPORT-PC-MFEE             TO S03-PC-MFEE.
           MOVE REPORT-PC-DISC             TO S03-PC-DISC.
           MOVE REPORT-PC-OTHER            TO S03-PC-OTHER.
           MOVE REPORT-PC-CASH-EARN        TO S03-PC-CASH-EARN.
           MOVE REPORT-PC-ENDING           TO S03-PC-ENDING.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           GO TO PRINT-REPORT-PC-EXIT.

       PRINT-REPORT-PC-TOTAL.

           IF ( REPORT-GRAND-TOTAL )
              MOVE "GRAND TOTALS          " TO T03-PC-PROMPT
           ELSE
           IF ( REPORT-BRANCH-TOTAL )
              MOVE "BRANCH TOTALS         " TO T03-PC-PROMPT
           ELSE
           IF ( REPORT-POOL-TOTAL )
              MOVE "POOL TOTALS           " TO T03-PC-PROMPT
           ELSE
              MOVE "CLASS TOTALS          " TO T03-PC-PROMPT.

           MOVE REPORT-PC-NUMBER-ACCTS     TO T03-PC-NUMBER-ACCTS.
           MOVE "#"                        TO T03-PC-NUMBER-ACCTS-FG.
           MOVE REPORT-PC-UNEARN           TO T03-PC-UNEARN.
           MOVE REPORT-PC-ADDON            TO T03-PC-ADDON.
           MOVE REPORT-PC-MFEE             TO T03-PC-MFEE.
           MOVE REPORT-PC-OTHER            TO T03-PC-OTHER.
           MOVE REPORT-PC-ENDING           TO T03-PC-ENDING.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE REPORT-PC-INT              TO T04-PC-INT.
           MOVE REPORT-PC-SRV              TO T04-PC-SRV.
           MOVE REPORT-PC-DISC             TO T04-PC-DISC.
           MOVE REPORT-PC-CASH-EARN        TO T04-PC-CASH-EARN.

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           GO TO PRINT-REPORT-PC-EXIT.

       PRINT-REPORT-PC-EXIT.
           EXIT.

      ******************************************************************
       PRINT-REPORT-PCX SECTION.
      ******************************************************************

           IF ( REPORT-GRAND-TOTAL  )  OR
              ( REPORT-BRANCH-TOTAL )  OR
              ( REPORT-POOL-TOTAL   )  OR
              ( REPORT-CLASS-TOTAL  )
              IF ( PRINT-DETAIL-ANALYSIS = "Y" )
                 GO TO PRINT-REPORT-PCX-TOTAL
              ELSE
                 GO TO PRINT-REPORT-PCX-SUMMARY.

       PRINT-REPORT-PCX-DETAIL.

           MOVE REPORT-PCX-DISPLAY-LNNO     TO WSS-LPS-DISPLAY-LNNO.
           MOVE WSS-LPS-NUMBER-7STATUS      TO D04-PCX-DISPLAY-LNNO.

           STRING REPORT-PCX-LNAME, ", ", REPORT-PCX-FNAME
                  DELIMITED BY "   "      INTO D04-PCX-NAME.

           MOVE REPORT-PCX-ANTERM           TO D04-PCX-ANTERM.
           MOVE REPORT-PCX-ANTIC            TO D04-PCX-ANTIC.
           MOVE REPORT-PCX-UNEARN           TO D04-PCX-UNEARN.
           MOVE REPORT-PCX-INT              TO D04-PCX-INT.
           MOVE REPORT-PCX-ADDON            TO D04-PCX-ADDON.
           MOVE REPORT-PCX-SRV              TO D04-PCX-SRV.
           MOVE REPORT-PCX-MFEE             TO D04-PCX-MFEE.
           MOVE REPORT-PCX-DISC             TO D04-PCX-DISC.
           MOVE REPORT-PCX-OTHER            TO D04-PCX-OTHER.
           MOVE REPORT-PCX-ROLLBACK         TO D04-PCX-ROLLBACK.  
           MOVE REPORT-PCX-CONTRACTUAL      TO D04-PCX-CONTRACTUAL.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           GO TO PRINT-REPORT-PCX-EXIT.

       PRINT-REPORT-PCX-SUMMARY.

      * TOTAL SUMMARY ONLY 

           IF ( REPORT-GRAND-TOTAL )
              MOVE "****"                  TO S04-PCX-BRANCHX
           ELSE
              MOVE REPORT-BRANCH           TO S04-PCX-BRANCH.

           IF ( REPORT-BRANCH-TOTAL )
              MOVE "****"                  TO S04-PCX-POOLX
           ELSE
              MOVE REPORT-POOL             TO S04-PCX-POOL.

           IF ( REPORT-POOL-TOTAL )
              MOVE "**"                    TO S04-PCX-CLASSX
           ELSE
              MOVE REPORT-CLASS            TO S04-PCX-CLASS.

           MOVE REPORT-PCX-NUMBER-ACCTS    TO S04-PCX-NUMBER-ACCTS.
           MOVE "#"                        TO S04-PCX-NUMBER-ACCTS-FG.
           MOVE REPORT-PCX-UNEARN          TO S04-PCX-UNEARN.
           MOVE REPORT-PCX-INT             TO S04-PCX-INT.
           MOVE REPORT-PCX-ADDON           TO S04-PCX-ADDON.
           MOVE REPORT-PCX-SRV             TO S04-PCX-SRV.
           MOVE REPORT-PCX-MFEE            TO S04-PCX-MFEE.
           MOVE REPORT-PCX-DISC            TO S04-PCX-DISC.
           MOVE REPORT-PCX-OTHER           TO S04-PCX-OTHER.
           MOVE REPORT-PCX-ROLLBACK        TO S04-PCX-ROLLBACK.   

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           GO TO PRINT-REPORT-PCX-EXIT.

       PRINT-REPORT-PCX-TOTAL.

           IF ( REPORT-GRAND-TOTAL )
              MOVE "GRAND TOTALS          " TO T03-PCX-PROMPT
           ELSE
           IF ( REPORT-BRANCH-TOTAL )
              MOVE "BRANCH TOTALS         " TO T03-PCX-PROMPT
           ELSE
           IF ( REPORT-POOL-TOTAL )
              MOVE "POOL TOTALS           " TO T03-PCX-PROMPT
           ELSE
              MOVE "CLASS TOTALS          " TO T03-PCX-PROMPT.

           MOVE REPORT-PCX-NUMBER-ACCTS   TO T03-PCX-NUMBER-ACCTS.
           MOVE "#"                       TO T03-PCX-NUMBER-ACCTS-FG.
           MOVE REPORT-PCX-UNEARN         TO T03-PCX-UNEARN.
           MOVE REPORT-PCX-ADDON          TO T03-PCX-ADDON.
           MOVE REPORT-PCX-MFEE           TO T03-PCX-MFEE.
           MOVE REPORT-PCX-OTHER          TO T03-PCX-OTHER.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE REPORT-PCX-INT              TO T04-PCX-INT.
           MOVE REPORT-PCX-SRV              TO T04-PCX-SRV.
           MOVE REPORT-PCX-DISC             TO T04-PCX-DISC.
           MOVE REPORT-PCX-ROLLBACK         TO T04-PCX-ROLLBACK.  

           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           GO TO PRINT-REPORT-PCX-EXIT.

       PRINT-REPORT-PCX-EXIT. 
           EXIT.

      ******************************************************************
      *
      *    U P D A T E - E X P O R T - F I L E
      *
      * IN  : REPORT-REC
      * FROM: PRINT-REPORT-RECORD-NEXT.
      * DESC: USING REPORT-REC, WHICH ARE DETAIL ACCOUNT RECORDS, WILL
      *       CREATE EXPORT FILES.
      ******************************************************************
       UPDATE-EXPORT-FILE SECTION.

           IF NOT ( REPORT-TYPE-IB  )
              GO TO UPDATE-EXPORT-FILE-IBX.

           MOVE SPACES                     TO AC2DAY-IB-REC.
           MOVE REPORT-BRANCH              TO AC2DAY-IB-BRNO.

           MOVE REPORT-IB-DISPLAY-LNNO     TO WSS-LPS-DISPLAY-LNNO.
           MOVE WSS-LPS-NUMBER             TO AC2DAY-IB-ACCTNO.
           MOVE WSS-LPS-7STATUS            TO AC2DAY-IB-7STATUS.
           MOVE WSS-LPS-CLASS              TO AC2DAY-IB-CLASS.

           STRING REPORT-IB-LNAME, ", ", REPORT-IB-FNAME
                  DELIMITED BY "   "      INTO AC2DAY-IB-NAME.

           MOVE REPORT-IB-FROM-DATE        TO AC2DAY-IB-FROM-DATE.
           MOVE REPORT-IB-INT-SINCE-LSTPY  TO AC2DAY-IB-INT-SINCE-LSTPY.
           MOVE REPORT-IB-INTBAL           TO AC2DAY-IB-INTBAL.
           MOVE REPORT-IB-TRUE-ACCRUE      TO AC2DAY-IB-TRUE-ACCRUE.
           MOVE REPORT-IB-ACCRUED          TO AC2DAY-IB-ACCRUED.
           MOVE REPORT-IB-MAX              TO AC2DAY-IB-MAX.
           MOVE REPORT-IB-ACTIONCD2        TO AC2DAY-IB-ACTIONCD2.
           MOVE X"0D"                      TO AC2DAY-IB-CR.
           MOVE X"0A"                      TO AC2DAY-IB-LF.

           ADD 1 TO AC2DAY-IB-KEY.
           PERFORM WRITE-AC2DAY-IB-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

       UPDATE-EXPORT-FILE-IBX.

           IF NOT ( REPORT-TYPE-IBX  )
              GO TO UPDATE-EXPORT-FILE-PC.

           MOVE SPACES                     TO AC2DAY-IBX-REC.
           MOVE REPORT-BRANCH              TO AC2DAY-IBX-BRNO.

           MOVE REPORT-IB-DISPLAY-LNNO     TO WSS-LPS-DISPLAY-LNNO.
           MOVE WSS-LPS-NUMBER             TO AC2DAY-IBX-ACCTNO.
           MOVE WSS-LPS-7STATUS            TO AC2DAY-IBX-7STATUS.
           MOVE WSS-LPS-CLASS              TO AC2DAY-IBX-CLASS.

           STRING REPORT-IBX-LNAME, ", ", REPORT-IBX-FNAME
                  DELIMITED BY "   "      INTO AC2DAY-IBX-NAME.

           MOVE REPORT-IBX-INT-PDTH-DATE   TO AC2DAY-IBX-INT-PDTH-DATE.
           MOVE REPORT-IBX-INT-SINCE-LSTPY TO
                                             AC2DAY-IBX-INT-SINCE-LSTPY.
           MOVE REPORT-IBX-INT-OWED        TO AC2DAY-IBX-INT-OWED.
           MOVE REPORT-IBX-TRUE-ACCRUE     TO AC2DAY-IBX-TRUE-ACCRUE.
           MOVE REPORT-IBX-INCOME-ACCRUE   TO AC2DAY-IBX-INCOME-ACCRUE.
           MOVE REPORT-IBX-BALANCE         TO AC2DAY-IBX-BALANCE.
           MOVE REPORT-IBX-CONTR-PDTH-DATE TO AC2DAY-IBX-CONT-PDTH-DATE.
           MOVE REPORT-IBX-CONTR-STATUS    TO AC2DAY-IBX-CONT-STATUS.
           MOVE REPORT-IBX-ACTIONCD2       TO AC2DAY-IBX-ACTIONCD2.
           MOVE X"0D"                      TO AC2DAY-IBX-CR.
           MOVE X"0A"                      TO AC2DAY-IBX-LF.

           ADD 1 TO AC2DAY-IBX-KEY.
           PERFORM WRITE-AC2DAY-IBX-FILE.

           IF ( IO-BAD )
              GO TO IO-ERROR.

       UPDATE-EXPORT-FILE-PC.

           IF NOT ( REPORT-TYPE-PC  )
              GO TO UPDATE-EXPORT-FILE-PCX.

           MOVE SPACES                     TO AC2DAY-PC-REC.
           MOVE REPORT-BRANCH              TO AC2DAY-PC-BRNO.

           MOVE REPORT-IB-DISPLAY-LNNO     TO WSS-LPS-DISPLAY-LNNO.
           MOVE WSS-LPS-NUMBER             TO AC2DAY-PC-ACCTNO.
           MOVE WSS-LPS-7STATUS            TO AC2DAY-PC-7STATUS.
           MOVE WSS-LPS-CLASS              TO AC2DAY-PC-CLASS.

           STRING REPORT-PC-LNAME, ", ", REPORT-PC-FNAME
                  DELIMITED BY "   "      INTO AC2DAY-PC-NAME.

           MOVE REPORT-PC-ANTERM           TO AC2DAY-PC-ANTERM.
           MOVE REPORT-PC-ANTIC            TO AC2DAY-PC-ANTIC.
           MOVE REPORT-PC-UNEARN           TO AC2DAY-PC-UNEARN.
           MOVE REPORT-PC-INT              TO AC2DAY-PC-INT.
           MOVE REPORT-PC-ADDON            TO AC2DAY-PC-ADDON.
           MOVE REPORT-PC-SRV              TO AC2DAY-PC-SRV.
           MOVE REPORT-PC-MFEE             TO AC2DAY-PC-MFEE.
           MOVE REPORT-PC-DISC             TO AC2DAY-PC-DISC.
           MOVE REPORT-PC-OTHER            TO AC2DAY-PC-OTHER.
           MOVE REPORT-PC-CASH-EARN        TO AC2DAY-PC-CASH-EARN.
           MOVE REPORT-PC-ENDING           TO AC2DAY-PC-ENDING.
           MOVE REPORT-PC-ACTIONCD2        TO AC2DAY-PC-ACTIONCD2.
           MOVE X"0D"                      TO AC2DAY-PC-CR.
           MOVE X"0A"                      TO AC2DAY-PC-LF.

           ADD 1 TO AC2DAY-PC-KEY.
           PERFORM WRITE-AC2DAY-PC-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

       UPDATE-EXPORT-FILE-PCX.

           IF NOT ( REPORT-TYPE-PCX  )
              GO TO UPDATE-EXPORT-FILE-EXIT.

           MOVE SPACES                     TO AC2DAY-PCX-REC.
           MOVE REPORT-BRANCH              TO AC2DAY-PCX-BRNO.

           MOVE REPORT-IB-DISPLAY-LNNO     TO WSS-LPS-DISPLAY-LNNO.
           MOVE WSS-LPS-NUMBER             TO AC2DAY-PCX-ACCTNO.
           MOVE WSS-LPS-7STATUS            TO AC2DAY-PCX-7STATUS.
           MOVE WSS-LPS-CLASS              TO AC2DAY-PCX-CLASS.

           STRING REPORT-PCX-LNAME, ", ", REPORT-PCX-FNAME
                  DELIMITED BY "   "      INTO AC2DAY-PCX-NAME.

           MOVE REPORT-PCX-ANTERM          TO AC2DAY-PCX-ANTERM.
           MOVE REPORT-PCX-ANTIC           TO AC2DAY-PCX-ANTIC.
           MOVE REPORT-PCX-UNEARN          TO AC2DAY-PCX-UNEARN.
           MOVE REPORT-PCX-INT             TO AC2DAY-PCX-INT.
           MOVE REPORT-PCX-ADDON           TO AC2DAY-PCX-ADDON.
           MOVE REPORT-PCX-SRV             TO AC2DAY-PCX-SRV.
           MOVE REPORT-PCX-MFEE            TO AC2DAY-PCX-MFEE.
           MOVE REPORT-PCX-DISC            TO AC2DAY-PCX-DISC.
           MOVE REPORT-PCX-OTHER           TO AC2DAY-PCX-OTHER.
           MOVE REPORT-PCX-ROLLBACK        TO AC2DAY-PCX-ROLLBACK.
           MOVE REPORT-PCX-CONTRACTUAL     TO AC2DAY-PCX-CONTRACTUAL.
           MOVE REPORT-PCX-ACTIONCD2       TO AC2DAY-PCX-ACTIONCD2.
           MOVE X"0D"                      TO AC2DAY-PCX-CR.
           MOVE X"0A"                      TO AC2DAY-PCX-LF.

           ADD 1 TO AC2DAY-PCX-KEY.
           PERFORM WRITE-AC2DAY-PCX-FILE.

           IF ( IO-BAD )
              GO TO IO-ERROR.

       UPDATE-EXPORT-FILE-EXIT.
           EXIT.

      ******************************************************************
       UPDATE-REPORT-FILE SECTION.
      ******************************************************************

           MOVE CURRENT-REPORT-TYPE TO HOLD-REPORT-TYPE.
           MOVE LN-OWNBR            TO HOLD-REPORT-BRANCH.
           MOVE ZEROES              TO HOLD-REPORT-POOL.
           MOVE LN-CLASS            TO HOLD-REPORT-CLASS.
           MOVE LN-ACCTNO           TO HOLD-REPORT-ACCTNO.

           IF ( PRINT-BY-POOL = "Y" )
              MOVE LN-DLPOOL      TO HOLD-REPORT-POOL.

      * UPDATE: DETAIL 

           PERFORM UPDATE-REPORT-RECORD.

      * UPDATE: CLASS  

           MOVE 99999999 TO HOLD-REPORT-ACCTNO.
           PERFORM UPDATE-REPORT-RECORD.

      * UPDATE: POOL 

           MOVE 9999     TO HOLD-REPORT-CLASS.

           IF ( PRINT-BY-POOL = "Y" )
              PERFORM UPDATE-REPORT-RECORD.

      * UPDATE: BRANCH 

           MOVE 999999   TO HOLD-REPORT-POOL.
           PERFORM UPDATE-REPORT-RECORD.

      * UPDATE: GRAND  

           MOVE 99999    TO HOLD-REPORT-BRANCH.
           PERFORM UPDATE-REPORT-RECORD.

       UPDATE-REPORT-FILE-EXIT.
           EXIT.

      ******************************************************************
       UPDATE-REPORT-RECORD SECTION.
      ******************************************************************

           MOVE HOLD-REPORT-KEY TO REPORT-KEY.
           PERFORM READ-REPORT-FILE.
           IF ( IO-BAD )
              PERFORM UPDATE-REPORT-RECORD-CLEAR
              PERFORM WRITE-REPORT-FILE
              IF ( IO-BAD )
                 GO TO IO-ERROR.

           IF ( REPORT-TYPE-IB   ) PERFORM UPDATE-REPORT-RECORD-IB.
           IF ( REPORT-TYPE-IBX  ) PERFORM UPDATE-REPORT-RECORD-IBX.
           IF ( REPORT-TYPE-PC   ) PERFORM UPDATE-REPORT-RECORD-PC.
           IF ( REPORT-TYPE-PCX  ) PERFORM UPDATE-REPORT-RECORD-PCX.

       UPDATE-REPORT-RECORD-EXIT.
           EXIT.

      ******************************************************************
       UPDATE-REPORT-RECORD-CLEAR SECTION.
      ******************************************************************

           MOVE SPACES          TO REPORT-REC.
           MOVE HOLD-REPORT-KEY TO REPORT-KEY.

           IF ( REPORT-TYPE-IB )
              MOVE ZEROES TO
                   REPORT-IB-FROM-DATE       REPORT-IB-INT-SINCE-LSTPY
                   REPORT-IB-INTBAL          REPORT-IB-TRUE-ACCRUE
                   REPORT-IB-ACCRUED         REPORT-IB-MAX
                   REPORT-IB-NUMBER-ACCTS.

           IF ( REPORT-TYPE-IBX )
              MOVE ZEROES TO
                   REPORT-IBX-INT-PDTH-DATE
                   REPORT-IBX-INT-SINCE-LSTPY
                   REPORT-IBX-INT-OWED
                   REPORT-IBX-TRUE-ACCRUE
                   REPORT-IBX-INCOME-ACCRUE
                   REPORT-IBX-BALANCE
                   REPORT-IBX-CONTR-PDTH-DATE
                   REPORT-IBX-CONTR-STATUS  REPORT-IBX-NUMBER-ACCTS.

           IF ( REPORT-TYPE-PC )
              MOVE ZEROES TO
                   REPORT-PC-ANTERM          REPORT-PC-ANTIC
                   REPORT-PC-UNEARN          REPORT-PC-INT
                   REPORT-PC-ADDON           REPORT-PC-SRV
                   REPORT-PC-MFEE            REPORT-PC-DISC
                   REPORT-PC-OTHER           REPORT-PC-CASH-EARN
                   REPORT-PC-ENDING          REPORT-PC-NUMBER-ACCTS.

           IF ( REPORT-TYPE-PCX )
              MOVE ZEROES TO 
                   REPORT-PCX-ANTERM          REPORT-PCX-ANTIC
                   REPORT-PCX-UNEARN          REPORT-PCX-INT
                   REPORT-PCX-ADDON           REPORT-PCX-SRV
                   REPORT-PCX-MFEE            REPORT-PCX-DISC
                   REPORT-PCX-OTHER           REPORT-PCX-ROLLBACK 
                   REPORT-PCX-NUMBER-ACCTS.

       UPDATE-REPORT-REC-CLR-EXIT.
           EXIT.

      ******************************************************************
       UPDATE-REPORT-RECORD-IB SECTION.
      ******************************************************************

           IF ( REPORT-GRAND-TOTAL  ) OR
              ( REPORT-BRANCH-TOTAL ) OR
              ( REPORT-POOL-TOTAL ) OR
              ( REPORT-CLASS-TOTAL  )
              GO TO UPDATE-REPORT-RECORD-IB-SKIP.

           MOVE LOAD-IB-DISPLAY-LNNO     TO REPORT-IB-DISPLAY-LNNO.
           MOVE LOAD-IB-LNAME            TO REPORT-IB-LNAME.
           MOVE LOAD-IB-FNAME            TO REPORT-IB-FNAME.
           MOVE LOAD-IB-FROM-DATE        TO REPORT-IB-FROM-DATE.
           MOVE LOAD-IB-MAX              TO REPORT-IB-MAX.
           MOVE LOAD-IB-ACTIONCD2        TO REPORT-IB-ACTIONCD2.

       UPDATE-REPORT-RECORD-IB-SKIP.

           ADD  LOAD-IB-INT-SINCE-LSTPY  TO REPORT-IB-INT-SINCE-LSTPY.
           ADD  LOAD-IB-INTBAL           TO REPORT-IB-INTBAL.
           ADD  LOAD-IB-TRUE-ACCRUE      TO REPORT-IB-TRUE-ACCRUE.
           ADD  LOAD-IB-ACCRUED          TO REPORT-IB-ACCRUED.
           ADD  01                       TO REPORT-IB-NUMBER-ACCTS.

      * UPDATING TO 'REPORT' FILE  

           PERFORM REWRITE-REPORT-FILE.

       UPDATE-REPORT-RECORD-IB-EXIT.
           EXIT.

      ******************************************************************
       UPDATE-REPORT-RECORD-IBX SECTION.
      ******************************************************************

           IF ( REPORT-GRAND-TOTAL  ) OR
              ( REPORT-BRANCH-TOTAL ) OR
              ( REPORT-POOL-TOTAL ) OR
              ( REPORT-CLASS-TOTAL  )
              GO TO UPDATE-REPORT-RC-IBX-SKIP.

           MOVE LOAD-IBX-DISPLAY-LNNO    TO REPORT-IBX-DISPLAY-LNNO.
           MOVE LOAD-IBX-LNAME           TO REPORT-IBX-LNAME.
           MOVE LOAD-IBX-FNAME           TO REPORT-IBX-FNAME.
           MOVE LOAD-IBX-INT-PDTH-DATE   TO REPORT-IBX-INT-PDTH-DATE.
           MOVE LOAD-IBX-CONTR-PDTH-DATE TO REPORT-IBX-CONTR-PDTH-DATE.
           MOVE LOAD-IBX-CONTR-STATUS    TO REPORT-IBX-CONTR-STATUS.
           MOVE LOAD-IBX-ACTIONCD2       TO REPORT-IBX-ACTIONCD2.

       UPDATE-REPORT-RC-IBX-SKIP.

           ADD LOAD-IBX-INT-SINCE-LSTPY TO REPORT-IBX-INT-SINCE-LSTPY.
           ADD LOAD-IBX-INT-OWED        TO REPORT-IBX-INT-OWED.
           ADD LOAD-IBX-TRUE-ACCRUE     TO REPORT-IBX-TRUE-ACCRUE.
           ADD LOAD-IBX-INCOME-ACCRUE   TO REPORT-IBX-INCOME-ACCRUE.
           ADD LOAD-IBX-BALANCE         TO REPORT-IBX-BALANCE.

           ADD  01 TO REPORT-IBX-NUMBER-ACCTS.

      * UPDATING TO 'REPORT' FILE  

           PERFORM REWRITE-REPORT-FILE.

       UPDATE-REPORT-RC-IBX-EXIT.
           EXIT.

      ******************************************************************
       UPDATE-REPORT-RECORD-PC SECTION.
      ******************************************************************

           IF ( REPORT-GRAND-TOTAL  ) OR
              ( REPORT-BRANCH-TOTAL ) OR
              ( REPORT-POOL-TOTAL   ) OR
              ( REPORT-CLASS-TOTAL  )
              GO TO UPDATE-REPORT-REC-PC-SKIP.

           MOVE LOAD-PC-DISPLAY-LNNO      TO REPORT-PC-DISPLAY-LNNO.
           MOVE LOAD-PC-LNAME             TO REPORT-PC-LNAME.
           MOVE LOAD-PC-FNAME             TO REPORT-PC-FNAME.
           MOVE LOAD-PC-ANTERM            TO REPORT-PC-ANTERM.
           MOVE LOAD-PC-ACTIONCD2         TO REPORT-PC-ACTIONCD2.

       UPDATE-REPORT-REC-PC-SKIP.

           ADD  LOAD-PC-ANTIC             TO REPORT-PC-ANTIC.
           ADD  LOAD-PC-UNEARN            TO REPORT-PC-UNEARN.
           ADD  LOAD-PC-INT               TO REPORT-PC-INT.
           ADD  LOAD-PC-ADDON             TO REPORT-PC-ADDON.
           ADD  LOAD-PC-SRV               TO REPORT-PC-SRV.
           ADD  LOAD-PC-MFEE              TO REPORT-PC-MFEE.
           ADD  LOAD-PC-DISC              TO REPORT-PC-DISC.
           ADD  LOAD-PC-OTHER             TO REPORT-PC-OTHER.
           ADD  LOAD-PC-CASH-EARN         TO REPORT-PC-CASH-EARN.
           ADD  LOAD-PC-ENDING            TO REPORT-PC-ENDING.
           ADD  01                        TO REPORT-PC-NUMBER-ACCTS.

      * UPDATING TO 'REPORT' FILE  

           PERFORM REWRITE-REPORT-FILE.

       UPDATE-REPORT-REC-PC-EXIT.
           EXIT.

      ******************************************************************
       UPDATE-REPORT-RECORD-PCX SECTION.
      ******************************************************************

           IF ( REPORT-GRAND-TOTAL  ) OR
              ( REPORT-BRANCH-TOTAL ) OR
              ( REPORT-POOL-TOTAL   ) OR
              ( REPORT-CLASS-TOTAL  )
              GO TO UPDATE-REPORT-REC-PCX-SKIP.

           MOVE LOAD-PCX-DISPLAY-LNNO    TO REPORT-PCX-DISPLAY-LNNO.
           MOVE LOAD-PCX-LNAME           TO REPORT-PCX-LNAME.
           MOVE LOAD-PCX-FNAME           TO REPORT-PCX-FNAME.
           MOVE LOAD-PCX-ANTERM          TO REPORT-PCX-ANTERM.
           MOVE LOAD-PCX-CONTRACTUAL     TO REPORT-PCX-CONTRACTUAL.
           MOVE LOAD-PCX-ACTIONCD2       TO REPORT-PCX-ACTIONCD2.

       UPDATE-REPORT-REC-PCX-SKIP.

           ADD  LOAD-PCX-ANTIC             TO REPORT-PCX-ANTIC.
           ADD  LOAD-PCX-UNEARN            TO REPORT-PCX-UNEARN.
           ADD  LOAD-PCX-INT               TO REPORT-PCX-INT.
           ADD  LOAD-PCX-ADDON             TO REPORT-PCX-ADDON.
           ADD  LOAD-PCX-SRV               TO REPORT-PCX-SRV.
           ADD  LOAD-PCX-MFEE              TO REPORT-PCX-MFEE.
           ADD  LOAD-PCX-DISC              TO REPORT-PCX-DISC.
           ADD  LOAD-PCX-OTHER             TO REPORT-PCX-OTHER.
           ADD  LOAD-PCX-ROLLBACK          TO REPORT-PCX-ROLLBACK.
           ADD  01                        TO REPORT-PCX-NUMBER-ACCTS.

      * UPDATING TO 'REPORT' FILE  

           PERFORM REWRITE-REPORT-FILE.

       UPDATE-REPORT-REC-PCX-EXIT.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************

           ADD LN-FEED TO LN-COUNT.

           IF ( LN-COUNT > EXT-CONAME-RPT-LINES )
              PERFORM WRITE-PAGE-HEADER-LINES.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.

           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

       WRITE-DETAIL-LINE-EXIT.
           EXIT.

      ******************************************************************
       WRITE-PAGE-HEADER-LINES SECTION.
      ******************************************************************

           MOVE 01 TO LN-COUNT.
           ADD  01 TO PAGE-NUMBER-TOTAL.
           ADD  01 TO PAGE-NUMBER-TYPE
           PERFORM GET-TIME.

           IF ( PRINT-DETAIL-ANALYSIS = "N" )
              GO TO WRITE-PAGE-HEADER-CONTINUE.

           IF ( REPORT-BRANCH NOT = HOLD-BRANCH ) AND
              ( NOT REPORT-GRAND-TOTAL          )
              MOVE REPORT-BRANCH TO BR-NO HOLD-BRANCH
              PERFORM READ-BR-FILE
              IF ( IO-BAD )
                 GO TO IO-ERROR.

           IF ( REPORT-POOL = ZEROES )
              MOVE "* NO POOL ID FOR LOANS *"    TO CD-DESC
           ELSE
              IF ( REPORT-POOL NOT = HOLD-POOL ) AND
                 ( NOT REPORT-BRANCH-TOTAL     )
                 MOVE "PL"                          TO CD-TYPE
                 MOVE REPORT-POOL                   TO CD-PL-POOLID
                 MOVE SPACES                        TO CD-PL-FILLER
                 PERFORM READ-CD1-FILE
                 IF ( IO-BAD )
                    MOVE "* NOT ON FILE *         " TO CD-DESC.

           IF ( REPORT-CLASS NOT = HOLD-CLASS ) AND
              ( NOT REPORT-POOL-TOTAL         )
              MOVE REPORT-CLASS TO LC-CODE HOLD-CLASS
              PERFORM READ-LC1-FILE
              IF ( IO-BAD )
                 GO TO IO-ERROR.

       WRITE-PAGE-HEADER-CONTINUE.

      * 01
           MOVE EXT-CONAME-SYSDATE               TO H01-DATE.
           MOVE T-HH                             TO H01-HOUR.
           MOVE T-MM                             TO H01-MINUTE.
           MOVE EXT-CONAME-CONAME                TO H01-NAME.
           CALL "C$JUSTIFY"                   USING H01-NAME "C".
           MOVE PAGE-NUMBER-TYPE                 TO H01-PAGE-TYPE.
           MOVE PAGE-NUMBER-TOTAL                TO H01-PAGE-TOTAL.

           MOVE HEADER-LINE-01                   TO PR-REC.
           MOVE 99                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      * 02
           MOVE EXT-CONAME-USER-ID               TO H02-USERID.
           MOVE ACCRUAL-DATE                     TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY                      TO H02-ACCRUAL-DATE.

           IF ( CURRENT-REPORT-TYPE-IB )
              MOVE HEADER-TITLE-TYPE-IB          TO H02-TITLE
           ELSE
           IF ( CURRENT-REPORT-TYPE-IBX )
              MOVE HEADER-TITLE-TYPE-IBX         TO H02-TITLE
           ELSE
           IF ( CURRENT-REPORT-TYPE-PC )
              MOVE HEADER-TITLE-TYPE-PC          TO H02-TITLE
           ELSE
           IF ( CURRENT-REPORT-TYPE-PCX )
              MOVE HEADER-TITLE-TYPE-PCX         TO H02-TITLE
           ELSE
              MOVE SPACES                        TO H02-TITLE.

           CALL "C$JUSTIFY"                   USING H02-TITLE "C".

           MOVE HEADER-LINE-02                   TO PR-REC.
           ADD  01                               TO LN-COUNT.
           MOVE 01                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      * 03
           IF ( PRINT-DETAIL-ANALYSIS = "Y" )
              MOVE BR-NO                         TO H03-BRANCH
              MOVE BR-NAME                       TO H03-BRANCH-DESC
           ELSE
              MOVE "****"                        TO H03-BRANCHX
              MOVE "*ALL BRANCHES*"              TO H03-BRANCH-DESC.

           MOVE HEADER-LINE-03                   TO PR-REC.
           ADD  01                               TO LN-COUNT.
           MOVE 01                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      * 04
           IF ( PRINT-DETAIL-ANALYSIS = "Y" )
              MOVE REPORT-POOL                   TO H04-POOL
              MOVE CD-DESC                       TO H04-POOL-DESC
           ELSE
              MOVE "*****"                       TO H04-POOLX
              MOVE "*ALL POOLS*"                 TO H04-POOL-DESC.

           IF ( PRINT-BY-POOL = "Y" )
               MOVE HEADER-LINE-04               TO PR-REC
               ADD  01                           TO LN-COUNT
               MOVE 01                           TO LN-FEED
               PERFORM WRITE-PR-REC.

      * 05
           IF ( PRINT-DETAIL-ANALYSIS = "Y" )
              MOVE REPORT-CLASS                  TO H05-CLASS
              MOVE LC-DESC                       TO H05-CLASS-DESC
           ELSE
              MOVE "**"                          TO H05-CLASSX
              MOVE "*ALL CLASSES*"               TO H05-CLASS-DESC.

           MOVE SPACES                           TO H05-MTD-NON.

           IF ( CURRENT-REPORT-TYPE-PC      ) AND
              ( PRINT-DETAIL-ANALYSIS = "Y" )
              MOVE "MTD-NON"                     TO H05-MTD-NON.

           MOVE HEADER-LINE-05                   TO PR-REC.
           ADD  01                               TO LN-COUNT.
           MOVE 01                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      * 06
           IF ( REPORT-TYPE-IB )
              IF ( PRINT-DETAIL-ANALYSIS = "Y" )
                 MOVE HEADER-LINE-IB-01          TO PR-REC
              ELSE
                 MOVE HEADER-LINE-IB-01-SUMMARY  TO PR-REC
           ELSE
           IF ( REPORT-TYPE-IBX )
              IF ( PRINT-DETAIL-ANALYSIS = "Y" )
                 MOVE HEADER-LINE-IBX-01         TO PR-REC
              ELSE
                 MOVE HEADER-LINE-IBX-01-SUMMARY TO PR-REC
           ELSE
           IF ( REPORT-TYPE-PC )
              IF ( PRINT-DETAIL-ANALYSIS = "Y" )
                 MOVE HEADER-LINE-PC-01          TO PR-REC
              ELSE
                 MOVE HEADER-LINE-PC-01-SUMMARY  TO PR-REC 
           ELSE
           IF ( REPORT-TYPE-PCX )
              IF ( PRINT-DETAIL-ANALYSIS = "Y" )
                 MOVE HEADER-LINE-PCX-01          TO PR-REC
              ELSE
                 MOVE HEADER-LINE-PCX-01-SUMMARY  TO PR-REC.

           ADD  01 TO LN-COUNT.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-PR-REC.

      * 07
           IF ( REPORT-TYPE-IB )
              IF ( PRINT-DETAIL-ANALYSIS = "Y" )
                 MOVE HEADER-LINE-IB-02          TO PR-REC
              ELSE
                 MOVE HEADER-LINE-IB-02-SUMMARY  TO PR-REC
           ELSE
           IF ( REPORT-TYPE-IBX )
              IF ( PRINT-DETAIL-ANALYSIS = "Y" )
                 MOVE HEADER-LINE-IBX-02         TO PR-REC
              ELSE
                 MOVE HEADER-LINE-IBX-02-SUMMARY TO PR-REC
           ELSE
           IF ( REPORT-TYPE-PC )
              IF ( PRINT-DETAIL-ANALYSIS = "Y" )
                 MOVE HEADER-LINE-PC-02          TO PR-REC
              ELSE
                 MOVE HEADER-LINE-PC-02-SUMMARY  TO PR-REC 
           ELSE
           IF ( REPORT-TYPE-PCX )
              IF ( PRINT-DETAIL-ANALYSIS = "Y" )
                 MOVE HEADER-LINE-PCX-02          TO PR-REC
              ELSE
                 MOVE HEADER-LINE-PCX-02-SUMMARY  TO PR-REC.

           ADD  01 TO LN-COUNT.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-PR-REC.

      * 08
           IF ( REPORT-TYPE-IB )
              IF ( PRINT-DETAIL-ANALYSIS = "Y" )
                 MOVE ALL "="                    TO PR-REC
              ELSE
                 MOVE HEADER-LINE-IB-03-SUMMARY  TO PR-REC
           ELSE
           IF ( REPORT-TYPE-IBX )
              IF ( PRINT-DETAIL-ANALYSIS = "Y" )
                 MOVE ALL "="                    TO PR-REC
              ELSE
                 MOVE HEADER-LINE-IBX-03-SUMMARY TO PR-REC
           ELSE
           IF ( REPORT-TYPE-PC )
              IF ( PRINT-DETAIL-ANALYSIS = "Y" )
                 MOVE ALL "="                    TO PR-REC
              ELSE
                 MOVE HEADER-LINE-PC-03-SUMMARY  TO PR-REC 
           ELSE
           IF ( REPORT-TYPE-PCX )
              IF ( PRINT-DETAIL-ANALYSIS = "Y" )
                 MOVE ALL "="                    TO PR-REC
              ELSE
                 MOVE HEADER-LINE-PCX-03-SUMMARY  TO PR-REC.

           ADD  01 TO LN-COUNT.
           MOVE 01 TO LN-FEED.
           PERFORM WRITE-PR-REC.

      * 09
           IF ( REPORT-TYPE-IB )
              IF ( PRINT-DETAIL-ANALYSIS = "Y" )
                 MOVE SPACES                     TO PR-REC
              ELSE
                 MOVE ALL "="                    TO PR-REC
           ELSE
           IF ( REPORT-TYPE-IBX )
              IF ( PRINT-DETAIL-ANALYSIS = "Y" )
                 MOVE SPACES                     TO PR-REC
              ELSE
                 MOVE ALL "="                    TO PR-REC
           ELSE
           IF ( REPORT-TYPE-PC )
              IF ( PRINT-DETAIL-ANALYSIS = "Y" )
                 MOVE SPACES                     TO PR-REC
              ELSE
                 MOVE ALL "="                    TO PR-REC 
           ELSE
           IF ( REPORT-TYPE-PCX )
              IF ( PRINT-DETAIL-ANALYSIS = "Y" )
                 MOVE SPACES                     TO PR-REC
              ELSE
                 MOVE ALL "="                    TO PR-REC.

           IF ( PR-REC NOT = SPACES )
              ADD  01 TO LN-COUNT
              MOVE 01 TO LN-FEED
              PERFORM WRITE-PR-REC.

       WRITE-PAGE-HEADER-LN-END.

           MOVE 01 TO LN-FEED.

       WRITE-PAGE-HEADER-LN-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/EOSPOOL.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/PROGID.CPY".
       COPY "LIBGB/REBATE.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBGB/TIMEIO.CPY".

       COPY "LIBLP/LPAERN.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBLP/LPAPRS.CPY".
       COPY "LIBLP/LPAPRZ.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBLP/LPCERN.CPY".
       COPY "LIBLP/LPEADJ.CPY".
       COPY "LIBLP/LPEARN.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBLP/LPITRM.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBLP/LPRATE.CPY".
       COPY "LIBLP/LPREGZ.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPUERN.CPY".
       COPY "LIBLP/LPUPER.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBGB/RBACT.CPY".
       COPY "LIBLP/SETCTBL.CPY".

       COPY "LIBLP/ARRAYSP.CPY".

      *********************************************
       FORM-RESET SECTION.
      *********************************************
           MOVE EXT-CONAME-CONAME TO AC2DAY-CONAME.
           MOVE EXT-CONAME-SYSDATE TO AC2DAY-SYSDATE.
           DISPLAY AC2DAY-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE AC2DAY-FORM-FIELDS TO WR-BUF.
           MOVE AC2DAY-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           MOVE AC2DAY-HELP--FILE TO HELP-LINK-FILE.
           MOVE AC2DAY-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/AC2DAY_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/RMFILE.CPY".
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       INITIAL-PROGRESS-INDICATOR.
           MOVE ZEROES                 TO PROGID-BRANCH.
           MOVE "LEGEND:B."            TO PROGID-LINE-LIST.
           MOVE PRU-LP                 TO PROGID-PRU-LP.
           MOVE EXT-ROUTE-BUF          TO PROGID-ROUTE-BUF.
           MOVE 9999                   TO PROGID-SLEEP-COUNTER.
           MOVE 250                    TO PROGID-SLEEP-COUNTER-MAX.

           PERFORM GET-TIME.
           MOVE TIME-EDIT              TO PROGID-TIME.

           MOVE "B"                    TO PROGID-TYPE.
           MOVE PROGID-TEXT-PROCESSING TO PROGID-TEXT.

       SEND-PROGRESS-INDICATOR.
           MOVE BR-NO     TO PROGID-BRANCH.
           PERFORM GET-TIME.
           MOVE TIME-EDIT TO PROGID-TIME.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PROGID-DISPLAY.
       VERIFY-PASSWORD.
           MOVE " "                          TO PASSWDW-STAT.
           MOVE "AC2DAY"                     TO PASSWDW-PGM.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD"                  TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PATH.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".

       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPLTI1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".

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

      *-----------------------------------------------------------------
       OPEN-LN1-FILE.
           PERFORM LOAD-LN1-FILE.
           PERFORM OPEN-IT.
           MOVE LN-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           IF LN1-CONN-OPEN-SW = "N" 
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS LN1_CONNECT
                    USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               MOVE "Y" TO LN1-CONN-OPEN-SW
               EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-LN1-FILE.  *> SSP LN BW LC
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE LN1-KEY        TO E-KEYX.

           IF ( LN1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LN1-FILE.

           EXEC SQL SET CONNECTION LN1_CONNECT END-EXEC.

      ** --FIRST SELECT FOR PC LOANS, SECOND FOR IB (INTEREST BEARING ) LOANS

      *     SELECT LNFILE.LN_OWNBR,
      *     LNFILE.LN_ACCTNO,
      *     LNFILE.LN_CLASS,
      *     LNFILE.LN_SUBCLASS,
      *     LNFILE.LN_LAWCODE,
      *     LNFILE.LN_SSNO1,
      *     LNFILE.LN_MAKERCD1, 
      *     LNFILE.LN_SSNO2,
      *     LNFILE.LN_MAKERCD2, 
      *     LNFILE.LN_SSNO3,
      *     LNFILE.LN_MAKERCD3, 
      *     LNFILE.LN_SSNO4,
      *     LNFILE.LN_MAKERCD4,
      *     LNFILE.LN_ORGST,
      *     LNFILE.LN_SPRCLASS,
      *     LNFILE.LN_CONVERCD, 
      *     LNFILE.LN_POCD, 
      *     LNFILE.LN_PLCD, 
      *     LNFILE.LN_STATUSFG, 
      *     LNFILE.LN_SALEFIN, 
      *     LNFILE.LN_SERVACCT, 
      *     LNFILE.LN_LOANTYPE, 
      *     LNFILE.LN_ADDON_FG, 
      *     LNFILE.LN_PAY_SCHLD_FG, 
      *     LNFILE.LN_STATFLAGS, 
      *     CAST(LNFILE.LN_ENTDATE           AS VARCHAR(10)), 
      *     CAST(LNFILE.LN_LOANDATE          AS VARCHAR(10)), 
      *     CAST(LNFILE.LN_PURDATE           AS VARCHAR(10)), 
      *     CAST(LNFILE.LN_INTDATE           AS VARCHAR(10)), 
      *     CAST(LNFILE.LN_CASHDATE          AS VARCHAR(10)), 
      *     CAST(LNFILE.LN_ORIG_1STPYDATE    AS VARCHAR(10)), 
      *     CAST(LNFILE.LN_1STPYDATE         AS VARCHAR(10)), 
      *     CAST(LNFILE.LN_JDDATE            AS VARCHAR(10)), 
      *     CAST(LNFILE.LN_PLDATE            AS VARCHAR(10)), 
      *     CAST(LNFILE.LN_ACCELDATE         AS VARCHAR(10)), 
      *     CAST(LNFILE.LN_REDUDATE          AS VARCHAR(10)), 
      *     CAST(LNFILE.LN_POFFDATE          AS VARCHAR(10)), 
      *     CAST(LNFILE.LN_LAST_ACCRUAL_DATE AS VARCHAR(10)), 
      *     LNFILE.LN_APRATE,
      *     LNFILE.LN_EFFRATE,
      *     LNFILE.LN_SMPRATE,
      *     LNFILE.LN_OVRDRATE,
      *     LNFILE.LN_ENDORATE,
      *     LNFILE.LN_JDRATE,
      *     LNFILE.LN_ACCRATE,
      *     LNFILE.LN_REDURATE,
      *     LNFILE.LN_DOWNPY,
      *     LNFILE.LN_LNAMT,
      *     LNFILE.LN_FINCHG,
      *     LNFILE.LN_INTCHG,
      *     LNFILE.LN_SERCHG,
      *     LNFILE.LN_EXTCHG,
      *     LNFILE.LN_MAINTFEE,
      *     LNFILE.LN_POINTS,
      *     LNFILE.LN_ORGTERM,
      *     LNFILE.LN_1STPYAMT,
      *     LNFILE.LN_NOREGPY,
      *     LNFILE.LN_REGPYAMT,
      *     LNFILE.LN_LASTPYAMT,
      *     LNFILE.LN_FEECODE_1, 
      *     LNFILE.LN_FEECODE_2, 
      *     LNFILE.LN_FEECODE_3, 
      *     LNFILE.LN_FEECODE_4, 
      *     LNFILE.LN_FEECODE_5, 
      *     LNFILE.LN_FEECODE_6, 
      *     LNFILE.LN_FEECODE_7, 
      *     LNFILE.LN_FEECODE_8, 
      *     LNFILE.LN_FEECODE_9, 
      *     LNFILE.LN_FEECODE_10, 
      *     LNFILE.LN_FEECODE_11, 
      *     LNFILE.LN_FEECODE_12, 
      *     LNFILE.LN_FEECODE_13, 
      *     LNFILE.LN_FEECODE_14, 
      *     LNFILE.LN_FEECODE_15, 
      *     LNFILE.LN_FEECODE_16, 
      *     LNFILE.LN_FEECODE_17, 
      *     LNFILE.LN_FEECODE_18, 
      *     LNFILE.LN_FEECODE_19, 
      *     LNFILE.LN_FEECODE_20, 
      *     LNFILE.LN_FEEAMT_1,
      *     LNFILE.LN_FEEAMT_2,
      *     LNFILE.LN_FEEAMT_3,
      *     LNFILE.LN_FEEAMT_4,
      *     LNFILE.LN_FEEAMT_5,
      *     LNFILE.LN_FEEAMT_6,
      *     LNFILE.LN_FEEAMT_7,
      *     LNFILE.LN_FEEAMT_8,
      *     LNFILE.LN_FEEAMT_9,
      *     LNFILE.LN_FEEAMT_10,
      *     LNFILE.LN_FEEAMT_11,
      *     LNFILE.LN_FEEAMT_12,
      *     LNFILE.LN_FEEAMT_13,
      *     LNFILE.LN_FEEAMT_14,
      *     LNFILE.LN_FEEAMT_15,
      *     LNFILE.LN_FEEAMT_16,
      *     LNFILE.LN_FEEAMT_17,
      *     LNFILE.LN_FEEAMT_18,
      *     LNFILE.LN_FEEAMT_19,
      *     LNFILE.LN_FEEAMT_20,
      *     LNFILE.LN_FEEOURS_1, 
      *     LNFILE.LN_FEEOURS_2, 
      *     LNFILE.LN_FEEOURS_3, 
      *     LNFILE.LN_FEEOURS_4, 
      *     LNFILE.LN_FEEOURS_5, 
      *     LNFILE.LN_FEEOURS_6, 
      *     LNFILE.LN_FEEOURS_7, 
      *     LNFILE.LN_FEEOURS_8, 
      *     LNFILE.LN_FEEOURS_9, 
      *     LNFILE.LN_FEEOURS_10,
      *     LNFILE.LN_FEEOURS_11,
      *     LNFILE.LN_FEEOURS_12,
      *     LNFILE.LN_FEEOURS_13,
      *     LNFILE.LN_FEEOURS_14,
      *     LNFILE.LN_FEEOURS_15,
      *     LNFILE.LN_FEEOURS_16,
      *     LNFILE.LN_FEEOURS_17,
      *     LNFILE.LN_FEEOURS_18,
      *     LNFILE.LN_FEEOURS_19,
      *     LNFILE.LN_FEEOURS_20,
      *     LNFILE.LN_FEEFINCHG_1,
      *     LNFILE.LN_FEEFINCHG_2,
      *     LNFILE.LN_FEEFINCHG_3,
      *     LNFILE.LN_FEEFINCHG_4,
      *     LNFILE.LN_FEEFINCHG_5,
      *     LNFILE.LN_FEEFINCHG_6,
      *     LNFILE.LN_FEEFINCHG_7,
      *     LNFILE.LN_FEEFINCHG_8,
      *     LNFILE.LN_FEEFINCHG_9,
      *     LNFILE.LN_FEEFINCHG_10,
      *     LNFILE.LN_FEEFINCHG_11,
      *     LNFILE.LN_FEEFINCHG_12,
      *     LNFILE.LN_FEEFINCHG_13,
      *     LNFILE.LN_FEEFINCHG_14,
      *     LNFILE.LN_FEEFINCHG_15,
      *     LNFILE.LN_FEEFINCHG_16,
      *     LNFILE.LN_FEEFINCHG_17,
      *     LNFILE.LN_FEEFINCHG_18,
      *     LNFILE.LN_FEEFINCHG_19,
      *     LNFILE.LN_FEEFINCHG_20,
      *     LNFILE.LN_INSURED,
      *     LNFILE.LN_INSTYPES,
      *     LNFILE.LN_INSCOMP_1,
      *     LNFILE.LN_INSCOMP_2,
      *     LNFILE.LN_INSCOMP_3,
      *     LNFILE.LN_INSCOMP_4,
      *     LNFILE.LN_INSCOMP_5,
      *     LNFILE.LN_INSCOMP_6,
      *     LNFILE.LN_INSCOMP_7,
      *     LNFILE.LN_INSCOMP_8,
      *     LNFILE.LN_INSCOMP_9,
      *     LNFILE.LN_INSCOMP_10,
      *     LNFILE.LN_INSOURS_1,
      *     LNFILE.LN_INSOURS_2,
      *     LNFILE.LN_INSOURS_3,
      *     LNFILE.LN_INSOURS_4,
      *     LNFILE.LN_INSOURS_5,
      *     LNFILE.LN_INSOURS_6,
      *     LNFILE.LN_INSOURS_7,
      *     LNFILE.LN_INSOURS_8,
      *     LNFILE.LN_INSOURS_9,
      *     LNFILE.LN_INSOURS_10,
      *     CAST(LNFILE.LN_INSEFF_DATE_1  AS VARCHAR(10)),
      *     CAST(LNFILE.LN_INSEFF_DATE_2  AS VARCHAR(10)),
      *     CAST(LNFILE.LN_INSEFF_DATE_3  AS VARCHAR(10)),
      *     CAST(LNFILE.LN_INSEFF_DATE_4  AS VARCHAR(10)),
      *     CAST(LNFILE.LN_INSEFF_DATE_5  AS VARCHAR(10)),
      *     CAST(LNFILE.LN_INSEFF_DATE_6  AS VARCHAR(10)),
      *     CAST(LNFILE.LN_INSEFF_DATE_7  AS VARCHAR(10)),
      *     CAST(LNFILE.LN_INSEFF_DATE_8  AS VARCHAR(10)),
      *     CAST(LNFILE.LN_INSEFF_DATE_9  AS VARCHAR(10)),
      *     CAST(LNFILE.LN_INSEFF_DATE_10 AS VARCHAR(10)),
      *     CAST(LNFILE.LN_INSEXP_DATE_1  AS VARCHAR(10)),
      *     CAST(LNFILE.LN_INSEXP_DATE_2  AS VARCHAR(10)),
      *     CAST(LNFILE.LN_INSEXP_DATE_3  AS VARCHAR(10)),
      *     CAST(LNFILE.LN_INSEXP_DATE_4  AS VARCHAR(10)),
      *     CAST(LNFILE.LN_INSEXP_DATE_5  AS VARCHAR(10)),
      *     CAST(LNFILE.LN_INSEXP_DATE_6  AS VARCHAR(10)),
      *     CAST(LNFILE.LN_INSEXP_DATE_7  AS VARCHAR(10)),
      *     CAST(LNFILE.LN_INSEXP_DATE_8  AS VARCHAR(10)),
      *     CAST(LNFILE.LN_INSEXP_DATE_9  AS VARCHAR(10)),
      *     CAST(LNFILE.LN_INSEXP_DATE_10 AS VARCHAR(10)),
      *     LNFILE.LN_INSPREM_1,
      *     LNFILE.LN_INSPREM_2,
      *     LNFILE.LN_INSPREM_3,
      *     LNFILE.LN_INSPREM_4,
      *     LNFILE.LN_INSPREM_5,
      *     LNFILE.LN_INSPREM_6,
      *     LNFILE.LN_INSPREM_7,
      *     LNFILE.LN_INSPREM_8,
      *     LNFILE.LN_INSPREM_9,
      *     LNFILE.LN_INSPREM_10,
      *     LNFILE.LN_INSCOVR_1,
      *     LNFILE.LN_INSCOVR_2,
      *     LNFILE.LN_INSCOVR_3,
      *     LNFILE.LN_INSCOVR_4,
      *     LNFILE.LN_INSCOVR_5,
      *     LNFILE.LN_INSCOVR_6,
      *     LNFILE.LN_INSCOVR_7,
      *     LNFILE.LN_INSCOVR_8,
      *     LNFILE.LN_INSCOVR_9,
      *     LNFILE.LN_INSCOVR_10,
      *     LNFILE.LN_INSFINCHG_1,
      *     LNFILE.LN_INSFINCHG_2,
      *     LNFILE.LN_INSFINCHG_3,
      *     LNFILE.LN_INSFINCHG_4,
      *     LNFILE.LN_INSFINCHG_5,
      *     LNFILE.LN_INSFINCHG_6,
      *     LNFILE.LN_INSFINCHG_7,
      *     LNFILE.LN_INSFINCHG_8,
      *     LNFILE.LN_INSFINCHG_9,
      *     LNFILE.LN_INSFINCHG_10,
      *     LNFILE.LN_PY_YYYYMM_RECDEL,
      *     LNFILE.LN_MTD_PAYS_RECDEL,
      *     LNFILE.LN_DLPOOL,
      *     LNFILE.LN_ORIGPURAMT,
      *     LNFILE.LN_PURBAL,
      *     LNFILE.LN_DLDISC,
      *     CAST(LNFILE.LN_INTPDTH_DATE AS VARCHAR(10)), 
      *     LNFILE.LN_CURBAL,
      *     LNFILE.LN_INTBAL,
      *     LNFILE.LN_OT2BAL,
      *     LNFILE.LN_ANTICTERM,
      *     LNFILE.LN_ANTICERN_1,
      *     LNFILE.LN_ANTICERN_2,
      *     LNFILE.LN_ANTICERN_3,
      *     LNFILE.LN_ANTICERN_4,
      *     LNFILE.LN_ANTICERN_5,
      *     LNFILE.LN_ANTICERN_6,
      *     LNFILE.LN_ANTICERN_7,
      *     LNFILE.LN_ANTICERN_8,
      *     LNFILE.LN_ANTICERN_9,
      *     LNFILE.LN_ANTICERN_10,
      *     LNFILE.LN_ANTICADJ_1,
      *     LNFILE.LN_ANTICADJ_2,
      *     LNFILE.LN_ANTICADJ_3,
      *     LNFILE.LN_ANTICADJ_4,
      *     LNFILE.LN_ANTICADJ_5,
      *     LNFILE.LN_ANTICADJ_6,
      *     LNFILE.LN_ANTICADJ_7,
      *     LNFILE.LN_ANTICADJ_8,
      *     LNFILE.LN_ANTICADJ_9,
      *     LNFILE.LN_ANTICADJ_10,
      *     LNFILE.LN_UFERN_1,
      *     LNFILE.LN_UFERN_2,
      *     LNFILE.LN_UFERN_3,
      *     LNFILE.LN_UFERN_4,
      *     LNFILE.LN_UFERN_5,
      *     LNFILE.LN_UFERN_6,
      *     LNFILE.LN_UFERN_7,
      *     LNFILE.LN_UFERN_8,
      *     LNFILE.LN_UFERN_9,
      *     LNFILE.LN_UFERN_10,
      *     LNFILE.LN_ACCERN_1,
      *     LNFILE.LN_ACCERN_2,
      *     LNFILE.LN_ACCERN_3,
      *     LNFILE.LN_ACCERN_4,
      *     LNFILE.LN_ACCERN_5,
      *     LNFILE.LN_ACCERN_6,
      *     LNFILE.LN_ACCERN_7,
      *     LNFILE.LN_ACCERN_8,
      *     LNFILE.LN_ACCERN_9,
      *     LNFILE.LN_ACCERN_10,
      *     LNFILE.LN_PRIOR_UNEARNED,
      *     LNFILE.LN_ERNDELQ_STOP,
      *     LNFILE.LN_NONACRERN,
      *     CAST(LNFILE.LN_ERN_IB_PDTH_DATE AS VARCHAR(10)),
      *     LNFILE.LN_ERN_IB_PRINBAL,
      *     LNFILE.LN_ERN_IB_INTOWE,
      *     LNFILE.LN_TOTINT,
      *     LNFILE.LN_TOTLCHG,
      *     LNFILE.LN_TOTDEF,
      *     LNFILE.LN_TOTPAYMNTD,
      *     LNFILE.LN_TOTEXCPAYMNTD,
      *     LNFILE.LN_TOTNODEF,
      *     LNFILE.LN_TOTNOALDEL,
      *     LNFILE.LN_CL_REBATE,
      *     LNFILE.LN_AH_REBATE,
      *     LNFILE.LN_PP_REBATE,
      *     LNFILE.LN_INT_REBATE,
      *     LNFILE.LN_SC_REBATE,
      *     LNFILE.LN_MF_REBATE,
      *     LNFILE.LN_DF_REBATE,
      *     LNFILE.LN_EX_REBATE,
      *     LNFILE.LN_O1_REBATE,
      *     LNFILE.LN_O2_REBATE,
      *     LNFILE.LN_O3_REBATE,
      *     LNFILE.LN_O4_REBATE,
      *     LNFILE.LN_O5_REBATE,
      *     LNFILE.LN_O6_REBATE,
      *     LNFILE.LN_O7_REBATE,
      *     LNFILE.LN_UN_O7_REB,
      *     LNFILE.LN_UN_O8_REB,
      *     LNFILE.LN_UN_O9_REB,
      *     LNFILE.LN_UN_O10_REB,
      *     LNFILE.LN_ACTIONCD2,
      *     CAST(LNFILE.LN_DATE_PAID_LAST AS VARCHAR(10)),
      *     LNFILE.LN_CREDITLIM,
      *     LNFILE.LN_OLDACCTNO,
      *     LNFILE.LN_PRLNNO,
      *     LNFILE.LN_SUM_LNAMT,
      *     LNFILE.LN_SUM_FINCHG,
      *     LNFILE.LN_SUM_INTCHG,
      *     LNFILE.LN_SUM_ORGTERM,
      *     LNFILE.LN_SUM_EXTCHG,
      *     LNFILE.LN_UNITPER_CD,
      *     LNFILE.LN_UNITPER_FREQ,
      *     LNFILE.LN_REPOCD,
      *     LNFILE.LN_CONPROFILE_1,
      *     LNFILE.LN_CONPROFILE_2,
      *     LNFILE.LN_CONPROFILE_3,
      *     LNFILE.LN_CONPROFILE_4,
      *     LNFILE.LN_CONPROFILE_5,
      *     ISNULL(BWFILE.BW_OWNBR,0),
      *     ISNULL(BWFILE.BW_SSNO,0),
      *     ISNULL(BWFILE.BW_LNAME,' '),
      *     ISNULL(BWFILE.BW_FNAME,' '),
      *     ISNULL(BWFILE.BW_SOLICIT,' '),
      *     ISNULL(LCFILE.LC_CODE,0),
      *     ISNULL(LCFILE.LC_DESC,' '),
      *     ISNULL(LCFILE.LC_SERVACCT,' ')
      * FROM DBO.LNFILE LNFILE
      *  LEFT OUTER JOIN DBO.BWFILE BWFILE 
      *    ON LNFILE.LN_OWNBR = BWFILE.BW_OWNBR	
      *    AND LNFILE.LN_SSNO1 = BWFILE.BW_SSNO  
      *  LEFT OUTER JOIN DBO.LCFILE LCFILE 
      *    ON LNFILE.LN_CLASS = LCFILE.LC_CODE  
      *  WHERE LNFILE.LN_OWNBR = @QLN1_WBEG_OWNBR
      *     AND LNFILE.LN_POCD NOT IN ('*I','*R')  
      *     AND (LNFILE.LN_CURBAL != 0 OR LNFILE.LN_PRIOR_UNEARNED != 0 OR LNFILE.LN_NONACRERN  != 0)
      * AND (LNFILE.LN_PLDATE = '1900-01-01' OR LNFILE.LN_PRIOR_UNEARNED != 0 OR LNFILE.LN_NONACRERN != 0 )
      *
      * UNION
      *
      *  SELECT LNFILE.LN_OWNBR,
      *    LNFILE.LN_ACCTNO,
      *    LNFILE.LN_CLASS,
      *    LNFILE.LN_SUBCLASS,
      *    LNFILE.LN_LAWCODE,
      *    LNFILE.LN_SSNO1,
      *    LNFILE.LN_MAKERCD1, 
      *    LNFILE.LN_SSNO2,
      *    LNFILE.LN_MAKERCD2, 
      *    LNFILE.LN_SSNO3,
      *    LNFILE.LN_MAKERCD3, 
      *    LNFILE.LN_SSNO4,
      *    LNFILE.LN_MAKERCD4,
      *    LNFILE.LN_ORGST,
      *    LNFILE.LN_SPRCLASS,
      *    LNFILE.LN_CONVERCD, 
      *    LNFILE.LN_POCD, 
      *    LNFILE.LN_PLCD, 
      *    LNFILE.LN_STATUSFG, 
      *    LNFILE.LN_SALEFIN, 
      *    LNFILE.LN_SERVACCT, 
      *    LNFILE.LN_LOANTYPE, 
      *    LNFILE.LN_ADDON_FG, 
      *    LNFILE.LN_PAY_SCHLD_FG, 
      *    LNFILE.LN_STATFLAGS, 
      *    CAST(LNFILE.LN_ENTDATE           AS VARCHAR(10)), 
      *    CAST(LNFILE.LN_LOANDATE          AS VARCHAR(10)), 
      *    CAST(LNFILE.LN_PURDATE           AS VARCHAR(10)), 
      *    CAST(LNFILE.LN_INTDATE           AS VARCHAR(10)), 
      *    CAST(LNFILE.LN_CASHDATE          AS VARCHAR(10)), 
      *    CAST(LNFILE.LN_ORIG_1STPYDATE    AS VARCHAR(10)), 
      *    CAST(LNFILE.LN_1STPYDATE         AS VARCHAR(10)), 
      *    CAST(LNFILE.LN_JDDATE            AS VARCHAR(10)), 
      *    CAST(LNFILE.LN_PLDATE            AS VARCHAR(10)), 
      *    CAST(LNFILE.LN_ACCELDATE         AS VARCHAR(10)), 
      *    CAST(LNFILE.LN_REDUDATE          AS VARCHAR(10)), 
      *    CAST(LNFILE.LN_POFFDATE          AS VARCHAR(10)), 
      *    CAST(LNFILE.LN_LAST_ACCRUAL_DATE AS VARCHAR(10)), 
      *    LNFILE.LN_APRATE,
      *    LNFILE.LN_EFFRATE,
      *    LNFILE.LN_SMPRATE,
      *    LNFILE.LN_OVRDRATE,
      *    LNFILE.LN_ENDORATE,
      *    LNFILE.LN_JDRATE,
      *    LNFILE.LN_ACCRATE,
      *    LNFILE.LN_REDURATE,
      *    LNFILE.LN_DOWNPY,
      *    LNFILE.LN_LNAMT,
      *    LNFILE.LN_FINCHG,
      *    LNFILE.LN_INTCHG,
      *    LNFILE.LN_SERCHG,
      *    LNFILE.LN_EXTCHG,
      *    LNFILE.LN_MAINTFEE,
      *    LNFILE.LN_POINTS,
      *    LNFILE.LN_ORGTERM,
      *    LNFILE.LN_1STPYAMT,
      *    LNFILE.LN_NOREGPY,
      *    LNFILE.LN_REGPYAMT,
      *    LNFILE.LN_LASTPYAMT,
      *    LNFILE.LN_FEECODE_1, 
      *    LNFILE.LN_FEECODE_2, 
      *    LNFILE.LN_FEECODE_3, 
      *    LNFILE.LN_FEECODE_4, 
      *    LNFILE.LN_FEECODE_5, 
      *    LNFILE.LN_FEECODE_6, 
      *    LNFILE.LN_FEECODE_7, 
      *    LNFILE.LN_FEECODE_8, 
      *    LNFILE.LN_FEECODE_9, 
      *    LNFILE.LN_FEECODE_10, 
      *    LNFILE.LN_FEECODE_11, 
      *    LNFILE.LN_FEECODE_12, 
      *    LNFILE.LN_FEECODE_13, 
      *    LNFILE.LN_FEECODE_14, 
      *    LNFILE.LN_FEECODE_15, 
      *    LNFILE.LN_FEECODE_16, 
      *    LNFILE.LN_FEECODE_17, 
      *    LNFILE.LN_FEECODE_18, 
      *    LNFILE.LN_FEECODE_19, 
      *    LNFILE.LN_FEECODE_20, 
      *    LNFILE.LN_FEEAMT_1,
      *    LNFILE.LN_FEEAMT_2,
      *    LNFILE.LN_FEEAMT_3,
      *    LNFILE.LN_FEEAMT_4,
      *    LNFILE.LN_FEEAMT_5,
      *    LNFILE.LN_FEEAMT_6,
      *    LNFILE.LN_FEEAMT_7,
      *    LNFILE.LN_FEEAMT_8,
      *    LNFILE.LN_FEEAMT_9,
      *    LNFILE.LN_FEEAMT_10,
      *    LNFILE.LN_FEEAMT_11,
      *    LNFILE.LN_FEEAMT_12,
      *    LNFILE.LN_FEEAMT_13,
      *    LNFILE.LN_FEEAMT_14,
      *    LNFILE.LN_FEEAMT_15,
      *    LNFILE.LN_FEEAMT_16,
      *    LNFILE.LN_FEEAMT_17,
      *    LNFILE.LN_FEEAMT_18,
      *    LNFILE.LN_FEEAMT_19,
      *    LNFILE.LN_FEEAMT_20,
      *    LNFILE.LN_FEEOURS_1, 
      *    LNFILE.LN_FEEOURS_2, 
      *    LNFILE.LN_FEEOURS_3, 
      *    LNFILE.LN_FEEOURS_4, 
      *    LNFILE.LN_FEEOURS_5, 
      *    LNFILE.LN_FEEOURS_6, 
      *    LNFILE.LN_FEEOURS_7, 
      *    LNFILE.LN_FEEOURS_8, 
      *    LNFILE.LN_FEEOURS_9, 
      *    LNFILE.LN_FEEOURS_10,
      *    LNFILE.LN_FEEOURS_11,
      *    LNFILE.LN_FEEOURS_12,
      *    LNFILE.LN_FEEOURS_13,
      *    LNFILE.LN_FEEOURS_14,
      *    LNFILE.LN_FEEOURS_15,
      *    LNFILE.LN_FEEOURS_16,
      *    LNFILE.LN_FEEOURS_17,
      *    LNFILE.LN_FEEOURS_18,
      *    LNFILE.LN_FEEOURS_19,
      *    LNFILE.LN_FEEOURS_20,
      *    LNFILE.LN_FEEFINCHG_1,
      *    LNFILE.LN_FEEFINCHG_2,
      *    LNFILE.LN_FEEFINCHG_3,
      *    LNFILE.LN_FEEFINCHG_4,
      *    LNFILE.LN_FEEFINCHG_5,
      *    LNFILE.LN_FEEFINCHG_6,
      *    LNFILE.LN_FEEFINCHG_7,
      *    LNFILE.LN_FEEFINCHG_8,
      *    LNFILE.LN_FEEFINCHG_9,
      *    LNFILE.LN_FEEFINCHG_10,
      *    LNFILE.LN_FEEFINCHG_11,
      *    LNFILE.LN_FEEFINCHG_12,
      *    LNFILE.LN_FEEFINCHG_13,
      *    LNFILE.LN_FEEFINCHG_14,
      *    LNFILE.LN_FEEFINCHG_15,
      *    LNFILE.LN_FEEFINCHG_16,
      *    LNFILE.LN_FEEFINCHG_17,
      *    LNFILE.LN_FEEFINCHG_18,
      *    LNFILE.LN_FEEFINCHG_19,
      *    LNFILE.LN_FEEFINCHG_20,
      *    LNFILE.LN_INSURED,
      *    LNFILE.LN_INSTYPES,
      *    LNFILE.LN_INSCOMP_1,
      *    LNFILE.LN_INSCOMP_2,
      *    LNFILE.LN_INSCOMP_3,
      *    LNFILE.LN_INSCOMP_4,
      *    LNFILE.LN_INSCOMP_5,
      *    LNFILE.LN_INSCOMP_6,
      *    LNFILE.LN_INSCOMP_7,
      *    LNFILE.LN_INSCOMP_8,
      *    LNFILE.LN_INSCOMP_9,
      *    LNFILE.LN_INSCOMP_10,
      *    LNFILE.LN_INSOURS_1,
      *    LNFILE.LN_INSOURS_2,
      *    LNFILE.LN_INSOURS_3,
      *    LNFILE.LN_INSOURS_4,
      *    LNFILE.LN_INSOURS_5,
      *    LNFILE.LN_INSOURS_6,
      *    LNFILE.LN_INSOURS_7,
      *    LNFILE.LN_INSOURS_8,
      *    LNFILE.LN_INSOURS_9,
      *    LNFILE.LN_INSOURS_10,
      *    CAST(LNFILE.LN_INSEFF_DATE_1  AS VARCHAR(10)),
      *    CAST(LNFILE.LN_INSEFF_DATE_2  AS VARCHAR(10)),
      *    CAST(LNFILE.LN_INSEFF_DATE_3  AS VARCHAR(10)),
      *    CAST(LNFILE.LN_INSEFF_DATE_4  AS VARCHAR(10)),
      *    CAST(LNFILE.LN_INSEFF_DATE_5  AS VARCHAR(10)),
      *    CAST(LNFILE.LN_INSEFF_DATE_6  AS VARCHAR(10)),
      *    CAST(LNFILE.LN_INSEFF_DATE_7  AS VARCHAR(10)),
      *    CAST(LNFILE.LN_INSEFF_DATE_8  AS VARCHAR(10)),
      *    CAST(LNFILE.LN_INSEFF_DATE_9  AS VARCHAR(10)),
      *    CAST(LNFILE.LN_INSEFF_DATE_10 AS VARCHAR(10)),
      *    CAST(LNFILE.LN_INSEXP_DATE_1  AS VARCHAR(10)),
      *    CAST(LNFILE.LN_INSEXP_DATE_2  AS VARCHAR(10)),
      *    CAST(LNFILE.LN_INSEXP_DATE_3  AS VARCHAR(10)),
      *    CAST(LNFILE.LN_INSEXP_DATE_4  AS VARCHAR(10)),
      *    CAST(LNFILE.LN_INSEXP_DATE_5  AS VARCHAR(10)),
      *    CAST(LNFILE.LN_INSEXP_DATE_6  AS VARCHAR(10)),
      *    CAST(LNFILE.LN_INSEXP_DATE_7  AS VARCHAR(10)),
      *    CAST(LNFILE.LN_INSEXP_DATE_8  AS VARCHAR(10)),
      *    CAST(LNFILE.LN_INSEXP_DATE_9  AS VARCHAR(10)),
      *    CAST(LNFILE.LN_INSEXP_DATE_10 AS VARCHAR(10)),
      *    LNFILE.LN_INSPREM_1,
      *    LNFILE.LN_INSPREM_2,
      *    LNFILE.LN_INSPREM_3,
      *    LNFILE.LN_INSPREM_4,
      *    LNFILE.LN_INSPREM_5,
      *    LNFILE.LN_INSPREM_6,
      *    LNFILE.LN_INSPREM_7,
      *    LNFILE.LN_INSPREM_8,
      *    LNFILE.LN_INSPREM_9,
      *    LNFILE.LN_INSPREM_10,
      *    LNFILE.LN_INSCOVR_1,
      *    LNFILE.LN_INSCOVR_2,
      *    LNFILE.LN_INSCOVR_3,
      *    LNFILE.LN_INSCOVR_4,
      *    LNFILE.LN_INSCOVR_5,
      *    LNFILE.LN_INSCOVR_6,
      *    LNFILE.LN_INSCOVR_7,
      *    LNFILE.LN_INSCOVR_8,
      *    LNFILE.LN_INSCOVR_9,
      *    LNFILE.LN_INSCOVR_10,
      *    LNFILE.LN_INSFINCHG_1,
      *    LNFILE.LN_INSFINCHG_2,
      *    LNFILE.LN_INSFINCHG_3,
      *    LNFILE.LN_INSFINCHG_4,
      *    LNFILE.LN_INSFINCHG_5,
      *    LNFILE.LN_INSFINCHG_6,
      *    LNFILE.LN_INSFINCHG_7,
      *    LNFILE.LN_INSFINCHG_8,
      *    LNFILE.LN_INSFINCHG_9,
      *    LNFILE.LN_INSFINCHG_10,
      *    LNFILE.LN_PY_YYYYMM_RECDEL,
      *    LNFILE.LN_MTD_PAYS_RECDEL,
      *    LNFILE.LN_DLPOOL,
      *    LNFILE.LN_ORIGPURAMT,
      *    LNFILE.LN_PURBAL,
      *    LNFILE.LN_DLDISC,
      *    CAST(LNFILE.LN_INTPDTH_DATE AS VARCHAR(10)), 
      *    LNFILE.LN_CURBAL,
      *    LNFILE.LN_INTBAL,
      *    LNFILE.LN_OT2BAL,
      *    LNFILE.LN_ANTICTERM,
      *    LNFILE.LN_ANTICERN_1,
      *    LNFILE.LN_ANTICERN_2,
      *    LNFILE.LN_ANTICERN_3,
      *    LNFILE.LN_ANTICERN_4,
      *    LNFILE.LN_ANTICERN_5,
      *    LNFILE.LN_ANTICERN_6,
      *    LNFILE.LN_ANTICERN_7,
      *    LNFILE.LN_ANTICERN_8,
      *    LNFILE.LN_ANTICERN_9,
      *    LNFILE.LN_ANTICERN_10,
      *    LNFILE.LN_ANTICADJ_1,
      *    LNFILE.LN_ANTICADJ_2,
      *    LNFILE.LN_ANTICADJ_3,
      *    LNFILE.LN_ANTICADJ_4,
      *    LNFILE.LN_ANTICADJ_5,
      *    LNFILE.LN_ANTICADJ_6,
      *    LNFILE.LN_ANTICADJ_7,
      *    LNFILE.LN_ANTICADJ_8,
      *    LNFILE.LN_ANTICADJ_9,
      *    LNFILE.LN_ANTICADJ_10,
      *    LNFILE.LN_UFERN_1,
      *    LNFILE.LN_UFERN_2,
      *    LNFILE.LN_UFERN_3,
      *    LNFILE.LN_UFERN_4,
      *    LNFILE.LN_UFERN_5,
      *    LNFILE.LN_UFERN_6,
      *    LNFILE.LN_UFERN_7,
      *    LNFILE.LN_UFERN_8,
      *    LNFILE.LN_UFERN_9,
      *    LNFILE.LN_UFERN_10,
      *    LNFILE.LN_ACCERN_1,
      *    LNFILE.LN_ACCERN_2,
      *    LNFILE.LN_ACCERN_3,
      *    LNFILE.LN_ACCERN_4,
      *    LNFILE.LN_ACCERN_5,
      *    LNFILE.LN_ACCERN_6,
      *    LNFILE.LN_ACCERN_7,
      *    LNFILE.LN_ACCERN_8,
      *    LNFILE.LN_ACCERN_9,
      *    LNFILE.LN_ACCERN_10,
      *    LNFILE.LN_PRIOR_UNEARNED,
      *    LNFILE.LN_ERNDELQ_STOP,
      *    LNFILE.LN_NONACRERN,
      *    CAST(LNFILE.LN_ERN_IB_PDTH_DATE AS VARCHAR(10)),
      *    LNFILE.LN_ERN_IB_PRINBAL,
      *    LNFILE.LN_ERN_IB_INTOWE,
      *    LNFILE.LN_TOTINT,
      *    LNFILE.LN_TOTLCHG,
      *    LNFILE.LN_TOTDEF,
      *    LNFILE.LN_TOTPAYMNTD,
      *    LNFILE.LN_TOTEXCPAYMNTD,
      *    LNFILE.LN_TOTNODEF,
      *    LNFILE.LN_TOTNOALDEL,
      *    LNFILE.LN_CL_REBATE,
      *    LNFILE.LN_AH_REBATE,
      *    LNFILE.LN_PP_REBATE,
      *    LNFILE.LN_INT_REBATE,
      *    LNFILE.LN_SC_REBATE,
      *    LNFILE.LN_MF_REBATE,
      *    LNFILE.LN_DF_REBATE,
      *    LNFILE.LN_EX_REBATE,
      *    LNFILE.LN_O1_REBATE,
      *    LNFILE.LN_O2_REBATE,
      *    LNFILE.LN_O3_REBATE,
      *    LNFILE.LN_O4_REBATE,
      *    LNFILE.LN_O5_REBATE,
      *    LNFILE.LN_O6_REBATE,
      *    LNFILE.LN_O7_REBATE,
      *    LNFILE.LN_UN_O7_REB,
      *    LNFILE.LN_UN_O8_REB,
      *    LNFILE.LN_UN_O9_REB,
      *    LNFILE.LN_UN_O10_REB,
      *    LNFILE.LN_ACTIONCD2,
      *    CAST(LNFILE.LN_DATE_PAID_LAST AS VARCHAR(10)),
      *    LNFILE.LN_CREDITLIM,
      *    LNFILE.LN_OLDACCTNO,
      *    LNFILE.LN_PRLNNO,
      *    LNFILE.LN_SUM_LNAMT,
      *    LNFILE.LN_SUM_FINCHG,
      *    LNFILE.LN_SUM_INTCHG,
      *    LNFILE.LN_SUM_ORGTERM,
      *    LNFILE.LN_SUM_EXTCHG,
      *    LNFILE.LN_UNITPER_CD,
      *    LNFILE.LN_UNITPER_FREQ,
      *    LNFILE.LN_REPOCD,
      *    LNFILE.LN_CONPROFILE_1,
      *    LNFILE.LN_CONPROFILE_2,
      *    LNFILE.LN_CONPROFILE_3,
      *    LNFILE.LN_CONPROFILE_4,
      *    LNFILE.LN_CONPROFILE_5,
      *    ISNULL(BWFILE.BW_OWNBR,0),
      *    ISNULL(BWFILE.BW_SSNO,0),
      *    ISNULL(BWFILE.BW_LNAME,' '),
      *    ISNULL(BWFILE.BW_FNAME,' '),
      *    ISNULL(BWFILE.BW_SOLICIT,' '),
      *    ISNULL(LCFILE.LC_CODE,0),
      *    ISNULL(LCFILE.LC_DESC,' '),
      *    ISNULL(LCFILE.LC_SERVACCT,' ')
      * FROM DBO.LNFILE LNFILE
      * LEFT OUTER JOIN DBO.BWFILE BWFILE 
      *   ON LNFILE.LN_OWNBR = BWFILE.BW_OWNBR	
      *  AND LNFILE.LN_SSNO1 = BWFILE.BW_SSNO  
      * LEFT OUTER JOIN DBO.LCFILE LCFILE 
      *   ON LNFILE.LN_CLASS = LCFILE.LC_CODE  
      * WHERE LNFILE.LN_OWNBR   = @QLN1_WBEG_OWNBR
      *  AND LNFILE.LN_CURBAL   > 0
      *  AND LNFILE.LN_PLDATE   = '1900-01-01'
      *  AND LNFILE.LN_PLCD     = ' ' 
      *  AND LNFILE.LN_POCD     NOT IN ('*I','*R', '**') 
      * ORDER BY 1,2

           EXEC SQL
            DECLARE LN1_CURSOR CURSOR FOR
                :LN1-RETURN-CODE = 
                 EXEC SSP_AC2DAY_LNFILE_SELECT
                  ( :QLN1-WBEG-OWNBR)
                END-EXEC.

            EXEC SQL
               OPEN LN1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           MOVE ZEROES TO QLN1-WBEG-OWNBR.
 
           MOVE STAT---GOOD TO LN1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LN1-FILE-NEXT.   *> EMBEDDED LN BW LC
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE LN1-KEY        TO E-KEYX.
           INITIALIZE QLN-REC QBW-REC QLC-REC.

           EXEC SQL SET CONNECTION LN1_CONNECT END-EXEC.

           IF ( LN1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LN1_CURSOR INTO 
                     :QLN-OWNBR,
                     :QLN-ACCTNO,
                     :QLN-CLASS,
                     :QLN-SUBCLASS,
                     :QLN-LAWCODE,
                     :QLN-SSNO1,
                     :QLN-MAKERCD1,
                     :QLN-SSNO2,
                     :QLN-MAKERCD2,
                     :QLN-SSNO3,
                     :QLN-MAKERCD3,
                     :QLN-SSNO4,
                     :QLN-MAKERCD4,
                     :QLN-ORGST,
                     :QLN-SPRCLASS,
                     :QLN-CONVERCD, 
                     :QLN-POCD,
                     :QLN-PLCD,
                     :QLN-STATUSFG,
                     :QLN-SALEFIN, 
                     :QLN-SERVACCT, 
                     :QLN-LOANTYPE,
                     :QLN-ADDON-FG,
                     :QLN-PAY-SCHLD-FG,
                     :QLN-STATFLAGS,
                     :QLN-ENTDATE,
                     :QLN-LOANDATE,
                     :QLN-PURDATE,
                     :QLN-INTDATE,
                     :QLN-CASHDATE,
                     :QLN-ORIG-1STPYDATE,
                     :QLN-1STPYDATE,
                     :QLN-JDDATE,
                     :QLN-PLDATE,
                     :QLN-ACCELDATE,
                     :QLN-REDUDATE,
                     :QLN-POFFDATE,
                     :QLN-LAST-ACCRUAL-DATE,
                     :QLN-APRATE,
                     :QLN-EFFRATE,
                     :QLN-SMPRATE,
                     :QLN-OVRDRATE,
                     :QLN-ENDORATE,
                     :QLN-JDRATE,
                     :QLN-ACCRATE,
                     :QLN-REDURATE,
                     :QLN-DOWNPY,
                     :QLN-LNAMT,
                     :QLN-FINCHG,
                     :QLN-INTCHG,
                     :QLN-SERCHG,
                     :QLN-EXTCHG,
                     :QLN-MAINTFEE,
                     :QLN-POINTS,
                     :QLN-ORGTERM,
                     :QLN-1STPYAMT,
                     :QLN-NOREGPY,
                     :QLN-REGPYAMT,
                     :QLN-LASTPYAMT,
                     :QLN-FEECODE-1, 
                     :QLN-FEECODE-2, 
                     :QLN-FEECODE-3, 
                     :QLN-FEECODE-4, 
                     :QLN-FEECODE-5, 
                     :QLN-FEECODE-6, 
                     :QLN-FEECODE-7, 
                     :QLN-FEECODE-8, 
                     :QLN-FEECODE-9, 
                     :QLN-FEECODE-10, 
                     :QLN-FEECODE-11, 
                     :QLN-FEECODE-12, 
                     :QLN-FEECODE-13, 
                     :QLN-FEECODE-14, 
                     :QLN-FEECODE-15, 
                     :QLN-FEECODE-16, 
                     :QLN-FEECODE-17, 
                     :QLN-FEECODE-18, 
                     :QLN-FEECODE-19, 
                     :QLN-FEECODE-20, 
                     :QLN-FEEAMT-1,
                     :QLN-FEEAMT-2,
                     :QLN-FEEAMT-3,
                     :QLN-FEEAMT-4,
                     :QLN-FEEAMT-5,
                     :QLN-FEEAMT-6,
                     :QLN-FEEAMT-7,
                     :QLN-FEEAMT-8,
                     :QLN-FEEAMT-9,
                     :QLN-FEEAMT-10,
                     :QLN-FEEAMT-11,
                     :QLN-FEEAMT-12,
                     :QLN-FEEAMT-13,
                     :QLN-FEEAMT-14,
                     :QLN-FEEAMT-15,
                     :QLN-FEEAMT-16,
                     :QLN-FEEAMT-17,
                     :QLN-FEEAMT-18,
                     :QLN-FEEAMT-19,
                     :QLN-FEEAMT-20,
                     :QLN-FEEOURS-1, 
                     :QLN-FEEOURS-2, 
                     :QLN-FEEOURS-3, 
                     :QLN-FEEOURS-4, 
                     :QLN-FEEOURS-5, 
                     :QLN-FEEOURS-6, 
                     :QLN-FEEOURS-7, 
                     :QLN-FEEOURS-8, 
                     :QLN-FEEOURS-9, 
                     :QLN-FEEOURS-10, 
                     :QLN-FEEOURS-11, 
                     :QLN-FEEOURS-12, 
                     :QLN-FEEOURS-13, 
                     :QLN-FEEOURS-14, 
                     :QLN-FEEOURS-15, 
                     :QLN-FEEOURS-16, 
                     :QLN-FEEOURS-17, 
                     :QLN-FEEOURS-18, 
                     :QLN-FEEOURS-19, 
                     :QLN-FEEOURS-20, 
                     :QLN-FEEFINCHG-1, 
                     :QLN-FEEFINCHG-2, 
                     :QLN-FEEFINCHG-3, 
                     :QLN-FEEFINCHG-4, 
                     :QLN-FEEFINCHG-5, 
                     :QLN-FEEFINCHG-6, 
                     :QLN-FEEFINCHG-7, 
                     :QLN-FEEFINCHG-8, 
                     :QLN-FEEFINCHG-9, 
                     :QLN-FEEFINCHG-10, 
                     :QLN-FEEFINCHG-11, 
                     :QLN-FEEFINCHG-12, 
                     :QLN-FEEFINCHG-13, 
                     :QLN-FEEFINCHG-14, 
                     :QLN-FEEFINCHG-15, 
                     :QLN-FEEFINCHG-16, 
                     :QLN-FEEFINCHG-17, 
                     :QLN-FEEFINCHG-18, 
                     :QLN-FEEFINCHG-19, 
                     :QLN-FEEFINCHG-20, 
                     :QLN-INSURED,
                     :QLN-INSTYPES,
                     :QLN-INSCOMP-1,
                     :QLN-INSCOMP-2,
                     :QLN-INSCOMP-3,
                     :QLN-INSCOMP-4,
                     :QLN-INSCOMP-5,
                     :QLN-INSCOMP-6,
                     :QLN-INSCOMP-7,
                     :QLN-INSCOMP-8,
                     :QLN-INSCOMP-9,
                     :QLN-INSCOMP-10,
                     :QLN-INSOURS-1, 
                     :QLN-INSOURS-2, 
                     :QLN-INSOURS-3, 
                     :QLN-INSOURS-4, 
                     :QLN-INSOURS-5, 
                     :QLN-INSOURS-6, 
                     :QLN-INSOURS-7, 
                     :QLN-INSOURS-8, 
                     :QLN-INSOURS-9, 
                     :QLN-INSOURS-10,
                     :QLN-INSEFF-DATE-1,
                     :QLN-INSEFF-DATE-2,
                     :QLN-INSEFF-DATE-3,
                     :QLN-INSEFF-DATE-4,
                     :QLN-INSEFF-DATE-5,
                     :QLN-INSEFF-DATE-6,
                     :QLN-INSEFF-DATE-7,
                     :QLN-INSEFF-DATE-8,
                     :QLN-INSEFF-DATE-9,
                     :QLN-INSEFF-DATE-10,
                     :QLN-INSEXP-DATE-1,
                     :QLN-INSEXP-DATE-2,
                     :QLN-INSEXP-DATE-3,
                     :QLN-INSEXP-DATE-4,
                     :QLN-INSEXP-DATE-5,
                     :QLN-INSEXP-DATE-6,
                     :QLN-INSEXP-DATE-7,
                     :QLN-INSEXP-DATE-8,
                     :QLN-INSEXP-DATE-9,
                     :QLN-INSEXP-DATE-10,
                     :QLN-INSPREM-1,
                     :QLN-INSPREM-2,
                     :QLN-INSPREM-3,
                     :QLN-INSPREM-4,
                     :QLN-INSPREM-5,
                     :QLN-INSPREM-6,
                     :QLN-INSPREM-7,
                     :QLN-INSPREM-8,
                     :QLN-INSPREM-9,
                     :QLN-INSPREM-10,
                     :QLN-INSCOVR-1,
                     :QLN-INSCOVR-2,
                     :QLN-INSCOVR-3,
                     :QLN-INSCOVR-4,
                     :QLN-INSCOVR-5,
                     :QLN-INSCOVR-6,
                     :QLN-INSCOVR-7,
                     :QLN-INSCOVR-8,
                     :QLN-INSCOVR-9,
                     :QLN-INSCOVR-10,
                     :QLN-INSFINCHG-1,
                     :QLN-INSFINCHG-2,
                     :QLN-INSFINCHG-3,
                     :QLN-INSFINCHG-4,
                     :QLN-INSFINCHG-5,
                     :QLN-INSFINCHG-6,
                     :QLN-INSFINCHG-7,
                     :QLN-INSFINCHG-8,
                     :QLN-INSFINCHG-9,
                     :QLN-INSFINCHG-10, 
                     :QLN-PY-YYYYMM-RECDEL,
                     :QLN-MTD-PAYS-RECDEL,
                     :QLN-DLPOOL,
                     :QLN-ORIGPURAMT,
                     :QLN-PURBAL,
                     :QLN-DLDISC,
                     :QLN-INTPDTH-DATE,
                     :QLN-CURBAL,
                     :QLN-INTBAL,
                     :QLN-OT2BAL,
                     :QLN-ANTICTERM,
                     :QLN-ANTICERN-1,
                     :QLN-ANTICERN-2,
                     :QLN-ANTICERN-3,
                     :QLN-ANTICERN-4,
                     :QLN-ANTICERN-5,
                     :QLN-ANTICERN-6,
                     :QLN-ANTICERN-7,
                     :QLN-ANTICERN-8,
                     :QLN-ANTICERN-9,
                     :QLN-ANTICERN-10,
                     :QLN-ANTICADJ-1,
                     :QLN-ANTICADJ-2,
                     :QLN-ANTICADJ-3,
                     :QLN-ANTICADJ-4,
                     :QLN-ANTICADJ-5,
                     :QLN-ANTICADJ-6,
                     :QLN-ANTICADJ-7,
                     :QLN-ANTICADJ-8,
                     :QLN-ANTICADJ-9,
                     :QLN-ANTICADJ-10,
                     :QLN-UFERN-1,
                     :QLN-UFERN-2,
                     :QLN-UFERN-3,
                     :QLN-UFERN-4,
                     :QLN-UFERN-5,
                     :QLN-UFERN-6,
                     :QLN-UFERN-7,
                     :QLN-UFERN-8,
                     :QLN-UFERN-9,
                     :QLN-UFERN-10,
                     :QLN-ACCERN-1,
                     :QLN-ACCERN-2,
                     :QLN-ACCERN-3,
                     :QLN-ACCERN-4,
                     :QLN-ACCERN-5,
                     :QLN-ACCERN-6,
                     :QLN-ACCERN-7,
                     :QLN-ACCERN-8,
                     :QLN-ACCERN-9,
                     :QLN-ACCERN-10,
                     :QLN-PRIOR-UNEARNED,
                     :QLN-ERNDELQ-STOP,
                     :QLN-NONACRERN,
                     :QLN-ERN-IB-PDTH-DATE,
                     :QLN-ERN-IB-PRINBAL,
                     :QLN-ERN-IB-INTOWE,
                     :QLN-TOTINT,
                     :QLN-TOTLCHG,
                     :QLN-TOTDEF,
                     :QLN-TOTPAYMNTD,
                     :QLN-TOTEXCPAYMNTD,
                     :QLN-TOTNODEF,
                     :QLN-TOTNOALDEL,
                     :QLN-CL-REBATE, 
                     :QLN-AH-REBATE, 
                     :QLN-PP-REBATE, 
                     :QLN-INT-REBATE, 
                     :QLN-SC-REBATE, 
                     :QLN-MF-REBATE, 
                     :QLN-DF-REBATE, 
                     :QLN-EX-REBATE, 
                     :QLN-O1-REBATE, 
                     :QLN-O2-REBATE, 
                     :QLN-O3-REBATE, 
                     :QLN-O4-REBATE, 
                     :QLN-O5-REBATE, 
                     :QLN-O6-REBATE, 
                     :QLN-O7-REBATE, 
                     :QLN-UN-O7-REB, 
                     :QLN-UN-O8-REB, 
                     :QLN-UN-O9-REB, 
                     :QLN-UN-O10-REB, 
                     :QLN-ACTIONCD2, 
                     :QLN-DATE-PAID-LAST,
                     :QLN-CREDITLIM,
                     :QLN-OLDACCTNO,
                     :QLN-PRLNNO,
                     :QLN-SUM-LNAMT,
                     :QLN-SUM-FINCHG,
                     :QLN-SUM-INTCHG,
                     :QLN-SUM-ORGTERM,
                     :QLN-SUM-EXTCHG,
                     :QLN-UNITPER-CD,
                     :QLN-UNITPER-FREQ,
                     :QLN-REPOCD,
                     :QLN-CONPROFILE-1,
                     :QLN-CONPROFILE-2,
                     :QLN-CONPROFILE-3,
                     :QLN-CONPROFILE-4,
                     :QLN-CONPROFILE-5,
                     :QBW-OWNBR,
                     :QBW-SSNO,
                     :QBW-LNAME,
                     :QBW-FNAME,
                     :QBW-SOLICIT,
                     :QLC-CODE,
                     :QLC-DESC,
                     :QLC-SERVACCT
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS
              PERFORM GET-BW-FIELDS
              PERFORM GET-LC-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LN1-FILE.

           PERFORM CLOSE-IT.

           IF ( LN1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION LN1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE LN1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO LN1-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-LN1-CONNECTION.

           IF LN1-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION LN1_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT LN1_CONNECT END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE "N" TO LN1-CONN-OPEN-SW.
      *-----------------------------------------------------------------


       OPEN-AC2DAY-IB-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE AC2DAY-IB-PATH TO E-FILE.
           OPEN OUTPUT AC2DAY-IB-FILE.
           IF IO-FG = 8
              GO TO OPEN-AC2DAY-IB-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE AC2DAY-IB-FILE
              GO TO OPEN-AC2DAY-IB-FILE-OUTPUT.
       WRITE-AC2DAY-IB-FILE.
           PERFORM WRITE-IT.
           MOVE AC2DAY-IB-PATH TO E-FILE.
           WRITE AC2DAY-IB-REC.
           IF IO-FG = 8
             GO TO WRITE-AC2DAY-IB-FILE.
       CLOSE-AC2DAY-IB-FILE.
           PERFORM CLOSE-IT.
           CLOSE AC2DAY-IB-FILE.

       OPEN-AC2DAY-IBX-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE AC2DAY-IBX-PATH TO E-FILE.
           OPEN OUTPUT AC2DAY-IBX-FILE.
           IF IO-FG = 8
              GO TO OPEN-AC2DAY-IBX-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE AC2DAY-IBX-FILE
              GO TO OPEN-AC2DAY-IBX-FILE-OUTPUT.
       WRITE-AC2DAY-IBX-FILE.
           PERFORM WRITE-IT.
           MOVE AC2DAY-IBX-PATH TO E-FILE.
           WRITE AC2DAY-IBX-REC.
           IF IO-FG = 8
             GO TO WRITE-AC2DAY-IBX-FILE.
       CLOSE-AC2DAY-IBX-FILE.
           PERFORM CLOSE-IT.
           CLOSE AC2DAY-IBX-FILE.

       OPEN-AC2DAY-PC-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE AC2DAY-PC-PATH TO E-FILE.
           OPEN OUTPUT AC2DAY-PC-FILE.
           IF IO-FG = 8
              GO TO OPEN-AC2DAY-PC-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE AC2DAY-PC-FILE
              GO TO OPEN-AC2DAY-PC-FILE-OUTPUT.
       WRITE-AC2DAY-PC-FILE.
           PERFORM WRITE-IT.
           MOVE AC2DAY-PC-PATH TO E-FILE.
           WRITE AC2DAY-PC-REC.
           IF IO-FG = 8
             GO TO WRITE-AC2DAY-PC-FILE.
       CLOSE-AC2DAY-PC-FILE.
           PERFORM CLOSE-IT.
           CLOSE AC2DAY-PC-FILE.

       OPEN-AC2DAY-PCX-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE AC2DAY-PCX-PATH TO E-FILE.
           OPEN OUTPUT AC2DAY-PCX-FILE.
           IF IO-FG = 8
              GO TO OPEN-AC2DAY-PCX-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE AC2DAY-PCX-FILE
              GO TO OPEN-AC2DAY-PCX-FILE-OUTPUT.
       WRITE-AC2DAY-PCX-FILE.
           PERFORM WRITE-IT.
           MOVE AC2DAY-PCX-PATH TO E-FILE.
           WRITE AC2DAY-PCX-REC.
           IF IO-FG = 8
             GO TO WRITE-AC2DAY-PCX-FILE.
       CLOSE-AC2DAY-PCX-FILE.
           PERFORM CLOSE-IT.
           CLOSE AC2DAY-PCX-FILE.

       OPEN-REPORT-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE REPORT-IFN TO E-FILE.
           OPEN OUTPUT REPORT-FILE.
           IF ( IO-LOCKED )
              GO TO OPEN-REPORT-FILE-OUTPUT.
           IF ( IO-ALREADY-OPEN )
              CLOSE REPORT-FILE
              GO TO OPEN-REPORT-FILE-OUTPUT.
           UNLOCK REPORT-FILE.
       OPEN-REPORT-FILE.
           PERFORM OPEN-IT.
           MOVE REPORT-IFN TO E-FILE.
           OPEN I-O REPORT-FILE.
           IF ( IO-LOCKED )
              GO TO OPEN-REPORT-FILE.
           IF ( IO-ALREADY-OPEN )
              CLOSE REPORT-FILE
              GO TO OPEN-REPORT-FILE.
           UNLOCK REPORT-FILE.
       READ-REPORT-FILE.
           PERFORM READ-IT.
           MOVE REPORT-IFN TO E-FILE.
           MOVE REPORT-KEY TO E-KEYX.
           READ REPORT-FILE.
           IF ( IO-LOCKED )
              GO TO READ-REPORT-FILE.
       READ-REPORT-FILE-NEXT.
           PERFORM READ-IT.
           MOVE REPORT-IFN TO E-FILE.
           MOVE REPORT-KEY TO E-KEYX.
           READ REPORT-FILE NEXT.
           IF ( IO-LOCKED )
              GO TO READ-REPORT-FILE-NEXT.
       START-REPORT-FILE.
           PERFORM START-IT.
           MOVE REPORT-IFN TO E-FILE.
           MOVE REPORT-KEY TO E-KEYX.
           START REPORT-FILE KEY NOT < REPORT-KEY.
           UNLOCK REPORT-FILE.
       WRITE-REPORT-FILE.
           PERFORM WRITE-IT.
           MOVE REPORT-IFN TO E-FILE.
           MOVE REPORT-KEY TO E-KEYX.
           WRITE REPORT-REC.
           IF ( IO-LOCKED )
              GO TO WRITE-REPORT-FILE.
           UNLOCK REPORT-FILE.
       REWRITE-REPORT-FILE.
           PERFORM REWRITE-IT.
           MOVE REPORT-IFN TO E-FILE.
           MOVE REPORT-KEY TO E-KEYX.
           REWRITE REPORT-REC.
           IF ( IO-LOCKED )
              GO TO REWRITE-REPORT-FILE.
           UNLOCK REPORT-FILE.
       CLOSE-REPORT-FILE.
           PERFORM CLOSE-IT.
           CLOSE REPORT-FILE.
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.

           PERFORM CLOSE-FILES.

           MOVE REPORT-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

           IF ( EXIT-PRIOR-TO-EXECUTION )
              GO TO EXIT-PROG.

           IF ( EXT-ROUTE-BUF = "00" )
              PERFORM PRU-OUT.

           IF ( NO-RECORDS-FOUND         ) AND
              ( EXT-ROUTE-BUF = "00" OR "70" )
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

       EXIT-PROG.

           IF ( SV-BEG-BRANCH NOT = 9999 )
              IF ( EXT-EOBUF-GROUP-FG = "Y"  )
                 MOVE SV-BEG-BRANCH     TO BEG-BRANCH
                 MOVE SV-END-BRANCH     TO END-BRANCH
                 MOVE SV-GROUP-FLAG     TO GROUP-FLAG
                 MOVE SV-ENT-GROUP      TO ENT-GROUP
                 MOVE SV-LOCAL-PRINT-FG TO LOCAL-PRINT-FG
              ELSE
                 MOVE SV-BEG-BRANCH TO BEG-BRANCH
                 MOVE SV-END-BRANCH TO END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH  TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY AC2DAY-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
