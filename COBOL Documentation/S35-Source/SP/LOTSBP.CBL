       IDENTIFICATION   DIVISION.
       PROGRAM-ID.        LOTSBP.
       AUTHOR.               BLF.
       DATE-WRITTEN.  2/28/08.
      **************************************************************************
      *
      *                   ( SP/LOTSBP )
      *
      *                  WORLD LIFE OF THE SOUTH
      *                  CREATE BPFILES (COPIED FROM BPREF)
      *                  WORLD PR# 573
      *
      *     DESCRIPTION:  BREAK OUT THE GLOBAL BATCH FILE FROM
      *                   WEEKLY CLAIM PMT FILE INTO EACH INDIVIDUAL
      *                   BRANCH FILE
      * REV.
      * BLM 080519 USE FROM DATE, NOT TO DATE, PER JIM
      * BLM 080521 CHANGE TO CREATE BP IN SITUATIONS WHERE LOAN IS PAID OFF OR
      *            PMT WILL NOT POST, NEED ALL TO REJECT TOGETHER IN 1 REPORT
      *            WHEN LONPW9 IS RUN
      * BLM 080522 ADD TO WRITTEN, NOT SKIPPED WHEN PMT EXCEEDS BALANCE
      *            SITUATIONS, CHANGE TO SAY PAYOFF OR PAYMENT INSTEAD OF
      *            "WOULD CREATE BP"
      * BLM 080605 ADDED BP-CLAIM-NUMBER & BP-PMT-TYPE TO BPFILE FOR EXCEPTION
      *            REPORT PRINTED BY LONPW9
      * BLM 080714 FIX ERROR MESSAGES, COMPUTE VALID-POSTING-NOPOFF,
      *            WORLD PR# 583
      * BLM 080714 FIX ZEROBAL CHECKS TO CORRESPOND TO LONPW9, SKIP PURBAL
      *            CHECKS, WORLD PR# 583
      * BLM 110615 ADD COVERAGE CODE 551 TO VALID TYPE FOR TEXAS IUI,
      *            WORLD PR# 756
      * BLM 110628 ADD COVERAGE CODES 523 & 537 TO VALID TYPE FOR PROPERTY
      *            WORLD PR# 757
      * BLM 130110 ADD COVERAGE CODES 556 TO VALID TYPE FOR IUI, CHANGE
      *            537 FROM PROPERTY TO AUTO, WORLD PR# 842
      *  CS 140603 REPLACED MKDIR-CMD WITH C$MAKEDIR
      * BLM 151119 ADD COVERAGE CODES 1 & 2 TO VALID TYPE FOR LIFE,
      *            WORLD PR# 1121
      * BAH 160128 SPLIT OUT LTFILE, PD0003
      * KEC 2016.0325 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CDSCAN-WINDOW-BUF TO HAVE MATCHING CD1-KEY
      *         LAYOUTS (FOR EACH CODE TYPE; CD-TYPE).
      *         CDSCAN-CD1-KEY IS THE NEW (PRIMARY) WITH REDEFINES OF
      *         CDSCAN-??-KEY (NEW).
      * BLM 161019 FIX IO ON BPFILE FOR BP-SEQNO & 8 DIGIT LOAN #
      * BLM 170425 ACTIVATE 8 DIGIT DATES
      *
      * KEC 2017.0912 {PR#01246} WORLD <A30> [JAY CISZEWSKI]
      *          WHEN ADDING THIS PR# TO A30, I NOTICED THAT A COPY
      *          MEMBER WAS NOT PROPERLY SETUP.  TO FIX IT I DID:
      *          - COPIED LIBLP/LPBP.CPY TO ../SV/LPBP.NO
      *          - MOVED  LIBLP/LPBP.CPY TO LIBLP/LPBP_CMD.CPY
      *          - EDITED LIBLP/LPBP_CMD.CPY TO LOWERCASE WHEN NEEDED
      *
      * BAH 181004 GLOBAL BWFILE
      * BAH 181214 GLOBAL LNFILE
      * BAH 20190930 REMOVED ACCESS-CALL LNFILE
      * BAH 20210205 DO NOT ALLOW CD-BR-FILE-TYPE "C", UMC DEBIT CARD PR#1490
      * BAH 20210323 ADDED AUTO-TYPE 636 FOR "UT" FORTEGRA LIMITED PHYSICAL
      *              DAMAGE #1493
      * BAH 20220720 ADDED LIFE-TYPE 112/113 FOR SINGLE/JOINT DECR LIFE
      *              WITH DISMEMBERMENT (NONGILA) AND 227/374 FOR
      *              SINGLE/JOINT DECR LIFE (GILA)  FORTEGRA       #1562
      * 2022.0726 BAH ADDED REMOVE-WORKFILE IN END-ROUTINE. 
      * 2022.0818 BAH REMOVED GLOBRD, READ GB-FILE INSTEAD
      * 2024.0212 BAH ADD 545 TO UNEMP-TYPE #1642
      * 2024.0214 BAH BP-ALL-PATH NEED TO GET MOVED TO BP-PATH
      * 2024.0520 BAH ADDED COVERAGE CODE 390 TO VALID TYPES A&H #1656 S35Q-29
      * 2024.0815 BAH PRINT LAST 4 OF SSNO #1673 S35Q-118
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
      * WORLD INSURANCE ALLOTMENT BATCH TAPE FILE
           SELECT IA-FILE ASSIGN TO IA-PATH
                          ORGANIZATION LINE SEQUENTIAL
                          ACCESS SEQUENTIAL
                          LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                          FILE STATUS FILE-STAT.
       COPY "LIBLP/LPFSBP.CPY".
       COPY "LIBGB/GBFSWKI.CPY".
      *-----------------------------------------------------------------
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      * RSZ 230 (INCLUDING 0A)
       FD  IA-FILE
           LABEL RECORDS ARE STANDARD.
       01  IA-REC.
           03  IA-COMPANY-CODE           PIC X(2).
           03  IA-INS-TYPE               PIC X(5).
           03  IA-AGENT-NO.
               05  FILLER                PIC X(6).
               05  IA-BRANCH             PIC 9(4).
           03  IA-AGENT-NAME             PIC X(32).
           03  IA-LNAME                  PIC X(35).
           03  IA-FNAME                  PIC X(25).
           03  IA-LOAN-NUMBER.
               05  IA-NUMBER             PIC 9(10).
               05  FILLER                PIC X(10).
           03  IA-PMT-AMOUNT             PIC S9(9)V99.
           03  IA-CLAIM-NUMBER           PIC 9(8).
           03  IA-PMT-TYPE               PIC X(10).
           03  IA-DAYS-PAID              PIC 9(3).
           03  IA-FROM-DATE              PIC 9(8).
           03  IA-TO-DATE                PIC 9(8).
           03  IA-CHECK-DATE             PIC 9(8).
           03  IA-COVERAGE-CODE          PIC 9(3).
           03  IA-CR                     PIC X.
           03  IA-LF                     PIC X.
       01  IA-TOTAL-REC.
           03  FILLER                    PIC X(129).
           03  IA-TOTAL-AMT              PIC 9(9)V99.
           03  IA-TOTAL-COUNT            PIC 9(8).
           03  FILLER                    PIC X(40).

       COPY "LIBLP/LPFDBP.CPY".
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-BRNO           PIC 9(5).
               05  WK-REFCD          PIC X(5).
           03  WK-AMT                PIC S9(9)V99 COMP.
           03  WK-COUNT              PIC S9(6)    COMP.

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/LOTSBP.CBL S35 02/22/24-07:37:52 >".

      ******************************************************************
      *
      *     P R O G R A M - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBGB/GB01GP.CPY".
       COPY "LIBGB/GB01GP_SQL.CPY".
       COPY "LIBGB/GBWSGP.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       01  WK-IFN                  PIC XX VALUE "WK".

      *********************************************************
      * PATH OF DATED INSURANCE ALLOTMENT FILE FOR LOTS INSURANACE CLAIMS
      * FILE IS LOADED BY WORLD, FOR EXAMPLE:
      * /USR/DATA/R1/ED/IA.LOTS.080131
      *****************************************************************
       01  IA-PATH.
           03  IA-PATH-BASE          PIC X(09).
           03  FILLER                PIC X(01)   VALUE "/".
           03  IA-PATH-MACHINE       PIC X(02).
           03  FILLER                PIC X(04)   VALUE "/ED/".
           03  IA-PATH-FILE-NAME     PIC X(06).
           03  FILLER                PIC X(01)   VALUE ".".
           03  IA-PATH-DATE          PIC 9(06).
           03  FILLER                REDEFINES IA-PATH-DATE.
               05  IA-PATH-DATE-YY   PIC 9(02).
               05  IA-PATH-DATE-MM   PIC 9(02).
               05  IA-PATH-DATE-DD   PIC 9(02).
       01  IA-PATH-SHORT             REDEFINES IA-PATH.
           03  IA-PATH-SHORT-NAME    PIC X(21).
           03  FILLER                PIC X(19).

       01  BRANCH-PATH.
           03  BRANCH-PATH-BASE          PIC X(09).
           03  FILLER                   PIC X(01)   VALUE "/".
           03  BRANCH-PATH-MACHINE       PIC X(02).
           03  FILLER                   PIC X(01)   VALUE "/".
           03  BRANCH-PATH-DATA          PIC X(06).

      *-----------------------------------------------------------------
      *    /USR/DATA/R#/XX####/ED/B####MMDD???REFCD
      *-----------------------------------------------------------------
       01  LOCAL-PATH.
           03  LOCAL-PATH-BASE          PIC X(09).
           03  FILLER                   PIC X(01)   VALUE "/".
           03  LOCAL-PATH-MACHINE       PIC X(02).
           03  FILLER                   PIC X(01)   VALUE "/".
           03  LOCAL-PATH-DATA          PIC X(06).
           03  FILLER                   PIC X(05)   VALUE "/ED/B".
           03  LOCAL-PATH-BRANCH.
               05  LOCAL-PATH-BRNO      PIC 9(04).
           03  LOCAL-PATH-DATE          PIC 9(04).
           03  LOCAL-PATH-BATCH         PIC 9(03).
           03  LOCAL-PATH-REFCD         PIC X(05).
       01  LOCAL-PATH-DISPLAYX          REDEFINES LOCAL-PATH.
           03  FILLER                   PIC X(20).
           03  LOCAL-PATH-DISPLAY       PIC X(30).

      *-----------------------------------------------------------------
       01  PR-IFN               PIC X(02) VALUE "PR".
       01  BP1-IFN              PIC X(03) VALUE "BP1".


      *=================================================================
      *
      *    L I N E - W O R K E R S
      *
      *=================================================================
       01  LN-FEED         PIC 9(02)  VALUE 1.
       01  LN-COUNT        PIC 9(03)  VALUE 99.
       01  PAGE-NUMBER         PIC 9(04)  VALUE 0.

      *=================================================================
      *
      *    M I S C - W O R K E R S
      *
      *=================================================================
       01  SUB                  PIC 9(02).
       01  MESS-LIST            PIC X(05)      VALUE "MESS.".
       01  LEGEND               PIC X(45).

       01  NAME-WORKER          PIC X(34).
       01  FIRST-NAME           PIC X(16).
       01  LAST-NAME            PIC X(16).

      *-----------------------------------------------------------------
      * SYSTEM DATE: ACCEPT SYSTEM DATE USIING TODAYS DATE,
      *   AND CONVERT FROM YY/MM/DD TO MM/DD/YY.
      *-----------------------------------------------------------------
       01  TODAYS-DATE          PIC 9(8).

       01  BATCH-NO             PIC 9(03).
       01  BATCH-DATE           PIC 9(04).
       01  FILLER REDEFINES BATCH-DATE.
           03  BATCH-MM         PIC 9(02).
           03  BATCH-DD         PIC 9(02).

       01  DUP-FLAG             PIC  X     VALUE "N".
       01  VALID-FLAG           PIC  X     VALUE "N".
       01  VALID-BRANCH-FG      PIC  X     VALUE "N".
       01  BP-OPEN-FG           PIC  X     VALUE "N".
       01  TEST-AMT             PIC S9(9)V99  COMP   VALUE 0.
       01  NET-POFF-PRIN        PIC S9(9)V99  COMP   VALUE 0.
       01  WILL-POFF-ACCOUNT    PIC S9(9)V99  COMP   VALUE 0.
       01  VALID-POSTING-NOPOFF PIC S9(9)V99  COMP   VALUE 0.
       01  TOT-POFF-REBATES     PIC S9(9)V99 COMP.
       01  PAYOFF-TRANS-FG      PIC X        VALUE " ".
       01  HOLD-BATCH-MULTIPLE-REFCDS PIC X VALUE " ".
       01  HOLD-CD-BR-PAYOFF-FG            PIC X VALUE " ".
       01  HOLD-BP-PAYOFF-NONCASH          PIC X VALUE " ".
       01  HOLD-LBOX-PAYOFF-NONCASH        PIC X VALUE " ".
       01  HOLD-BP-ALLOW-PMT-ACCT-OTHBAL   PIC X VALUE " ".
       01  HOLD-LBOX-ALLOW-PMT-ACCT-OTHBAL PIC X VALUE " ".
       01  HOLD-CD-BR-POST-SS-TRCD         PIC X VALUE " ".
       01  HOLD-BP-TRCD                    PIC XX VALUE "  ".

       01  TEST-COVERAGE-CODE PIC 9(3).
           88  LIFE-TYPE      VALUE 001 002 100 101 102 103 104 105
                                    112 113 168 169 170 171 227 374.
           88  AH-TYPE        VALUE 200 201 203 248 390.
           88  PROPERTY-TYPE  VALUE 511 512 517 522 523.
           88  AUTO-TYPE      VALUE 531 537 636.
           88  UNEMP-TYPE     VALUE 545 550 551 554 556 557.
       01  HOLD-REFCD.
           03  HOLD-INS-CO    PIC XX.
           03  HOLD-TYPE      PIC X(3).
       01  HOLD-SSNO          PIC 9(9) VALUE 0.
       01  HOLD-MESS          PIC X(40) VALUE SPACES.
       01  HOLD-BP-REC          PIC X(BP-SIZE).
       01  HOLD-BP-SEQNO        PIC 99.

      *-----------------------------------------------------------------
      *TOTAL COUNTS 1=BRANCH, 2=GRAND TOTALS
      *-----------------------------------------------------------------
       01  TOTAL-READ           OCCURS 2 PIC  9(09)    VALUE 0.
       01  TOTAL-AMT-READ       OCCURS 2 PIC S9(09)V99 VALUE 0.
       01  TOTAL-OM             OCCURS 2 PIC  9(09)    VALUE 0.
       01  TOTAL-AMT-OM         OCCURS 2 PIC S9(09)V99 VALUE 0.
       01  TOTAL-INVALID        OCCURS 2 PIC  9(09)    VALUE 0.
       01  TOTAL-AMT-INVALID    OCCURS 2 PIC S9(09)V99 VALUE 0.
       01  TOTAL-BADACCT        OCCURS 2 PIC  9(09)    VALUE 0.
       01  TOTAL-AMT-BADACCT    OCCURS 2 PIC S9(09)V99 VALUE 0.
       01  TOTAL-SKIP           OCCURS 2 PIC  9(09)    VALUE 0.
       01  TOTAL-AMT-SKIP       OCCURS 2 PIC S9(09)V99 VALUE 0.
       01  TOTAL-BADREF         OCCURS 2 PIC  9(09)    VALUE 0.
       01  TOTAL-AMT-BADREF     OCCURS 2 PIC S9(09)V99 VALUE 0.
       01  TOTAL-PAYOFF         OCCURS 2 PIC  9(09)    VALUE 0.
       01  TOTAL-AMT-PAYOFF     OCCURS 2 PIC S9(09)V99 VALUE 0.
       01  TOTAL-WRITTEN        OCCURS 2 PIC  9(09)    VALUE 0.
       01  TOTAL-AMT-WRITTEN    OCCURS 2 PIC S9(09)V99 VALUE 0.

       01  HOLD-BRANCH          PIC 9(05) VALUE 10000.

      *-----------------------------------------------------------------
      * SPOOL DIRECTORY/FILENAME FOR EXCEPTION REPORT
      *-----------------------------------------------------------------
       01  EX-SPOOL.
           03  EX-SPOOL-BASE          PIC X(09).
           03  FILLER                 PIC X(01)   VALUE "/".
           03  EX-SPOOL-MACHINE       PIC X(02).
           03  FILLER                 PIC X(01)   VALUE "/".
           03  EX-SPOOL-DATA          PIC X(06).
           03  FILLER                 PIC X(04)   VALUE "/EO/".
           03  FILLER                 PIC X(03)   VALUE "REF".
           03  EX-SPOOL-DATE          PIC 9(06).
           03  EX-SPOOL-FILE.
               05  FILLER             PIC X(01).
               05  EX-SPOOL-STEP      PIC 9(04).
               05  EX-SPOOL-NAME      PIC X(06).
               05  EX-SPOOL-SUF       PIC X(02).

       01  STEP-COUNT             PIC 9999 VALUE 1.


      *=================================================================
      *
      *    S C R E E N - F I E L D S
      *
      *=================================================================
       01  ERRCD            PIC X(01).


      ******  PARAMETERS INPUT:
       01  SV-BP-WK-REFCD       PIC X(5)  VALUE SPACES.
       01  TRANS-DATE           PIC 9(8).
       01  FILLER REDEFINES TRANS-DATE.
           03  TRANS-CC         PIC 99.
           03  TRANS-YY         PIC 99.
           03  TRANS-MM         PIC 99.
           03  TRANS-DD         PIC 99.
       01  REPORT-ONLY-FG       PIC X.
       01  BRANCH-PAGE-FG       PIC X.
       01  REJECTS-ONLY-FG      PIC X.

      *=================================================================
      *
      *    P R I N T - B U F F E R S
      *
      *=================================================================
       01  HEAD1.
           03  H-DATE             PIC X(08)   VALUE " ".
           03  FILLER             PIC X(02)   VALUE " ".
           03  H-HR               PIC 9(02)   VALUE 0.
           03  FILLER             PIC X(01)   VALUE ":".
           03  H-MIN              PIC 9(02)   VALUE 0.
           03  FILLER             PIC X(16)   VALUE " ".
           03  H-NAME             PIC X(40)   VALUE " ".
           03  FILLER             PIC X(21)   VALUE " ".
           03  FILLER             PIC X(30)   VALUE " ".
           03  FILLER             PIC X(05)   VALUE "PAGE ".
           03  H-PAGE-NO          PIC ZZZZ9.

      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER             PIC X(10)   VALUE "USER ID:".
           03  H-USERID           PIC X(10).
           03  FILLER             PIC X(11)   VALUE " ".
           03  FILLER             PIC X(38)   VALUE
               " - BATCH PAYMENTS FOR REFERENCE CODE: ".
           03  H-TITLE-REFCD      PIC X(5).
           03  FILLER             PIC X(2)    VALUE " -".
           03  FILLER             PIC X(11)   VALUE " ".
           03  H-TITLE-PAYOFF     PIC X(12)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEAD3.
           03  FILLER             PIC X(31)   VALUE " ".
           03  FILLER             PIC X(22)   VALUE
               "   BRANCH BATCH FILE: ".
           03  H-TITLE-LOCAL      PIC X(23).

      *-----------------------------------------------------------------
       01  HEAD4.
           03  FILLER             PIC X(37)   VALUE
               "BRNO  ACCOUNT #         SSNO         ".

           03  FILLER             PIC X(33)   VALUE
               "NAME                       REFCD ".
           03  FILLER             PIC X(20)   VALUE
               "    AMOUNT    DATE  ".
           03  FILLER             PIC X(40)   VALUE
               "  CLAIM#  PMT TYPE  MESSAGE             ".

      *=================================================================
      *
      *    D E T A I L - L I N E
      *
      *=================================================================
       01  DETAIL-LINE                   PIC X(132) VALUE SPACES.
       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-BRNO                    PIC ZZ99.
           03  FILLER                    PIC X(01).
           03  D-DUP                     PIC X(01).
           03  D-ACCTNO                  PIC X(17).
           03  FILLER                    PIC X(1).
           03  D-SSNO5                   PIC X(7).
           03  D-SSNO                    PIC 9999.
           03  FILLER                    PIC X(2).
           03  D-NAME                    PIC X(25).
           03  FILLER                    PIC X(2).
           03  D-REFCD                   PIC X(05).
           03  D-AMOUNT                  PIC ZZZZ,ZZ9.99.
           03  FILLER                    PIC X(2).
           03  D-DATE                    PIC 99/99/99.
           03  FILLER                    PIC X(1).
           03  D-CLAIM-NUMBER            PIC ZZZZZZZ9.
           03  FILLER                    PIC X(1).
           03  D-PMT-TYPE                PIC X(10).
           03  FILLER                    PIC X(2).
           03  D-MESS                    PIC X(20).
       01  D-LINE2 REDEFINES DETAIL-LINE.
           03  D-TOT-PROMPT              PIC X(21).
           03  FILLER                    PIC X(2).
           03  D-TOT-TYPE                PIC X(30).
           03  FILLER                    PIC X(5).
           03  D-TOT-COUNT               PIC ZZZ,ZZ9.
           03  D-TOT-AMT                 PIC ZZZZ,ZZZ,ZZ9.99.
           03  FILLER                    PIC X(52).

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/CDSCANW.CPY".
       COPY "LIBLP/LPBP_CMD.CPY".
       COPY "LIBLP/LPBPPATH.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBLP/LPLCAPW.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBLP/LPSCHDW.CPY".
       COPY "LIBLP/LPLCASW.CPY".
       COPY "LIBLP/LPLCHGW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBCL/CRNOR2W.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       01  LOTSBP-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/LOTSBP_DEF.CPY".
       COPY "LIBSP/LOTSBP_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/LOTSBP_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE BP-FILE WK-FILE IA-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GP-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-GB-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE
               BP-FILE WK-FILE IA-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ******************************************************************
      *
      *    M A I N - M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.
           PERFORM SQL-CONNECT.
      D    STOP MESS.
       COPY "LIBGB/CHKSEC.CPY".
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LOTSBP" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
      *             WITH NO SCROLL
      *             WITH NO WRAP
                   HANDLE IS LOTSBP-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

       NEXT-IA.
           PERFORM READ-IA-FILE-NEXT.
           IF IO-BAD
              GO TO END-ROUTINE.

           IF IA-AGENT-NO = "TOTAL"
              PERFORM PRINT-TOTAL-TRAILER
              GO TO END-ROUTINE.

           MOVE IA-COMPANY-CODE TO HOLD-INS-CO.
           MOVE IA-COVERAGE-CODE TO TEST-COVERAGE-CODE.
           IF LIFE-TYPE MOVE "LIF" TO HOLD-TYPE
            ELSE
             IF AH-TYPE MOVE "A&H" TO HOLD-TYPE
              ELSE
              IF PROPERTY-TYPE MOVE "PRP" TO HOLD-TYPE
                ELSE
                 IF AUTO-TYPE MOVE "VSI" TO HOLD-TYPE
                  ELSE
                     IF UNEMP-TYPE MOVE "IUI" TO HOLD-TYPE
                      ELSE
                         MOVE "INV" TO HOLD-TYPE.

           MOVE 0 TO HOLD-SSNO.
           MOVE "N" TO VALID-FLAG.

           IF IA-BRANCH NOT = HOLD-BRANCH
              PERFORM WRITE-BP-TRAILER
              PERFORM CLOSE-BP1-FILE
              MOVE "N" TO BP-OPEN-FG
              MOVE 1 TO SUB
              PERFORM TOTAL-ROUTINE
              MOVE IA-BRANCH TO HOLD-BRANCH
              IF REPORT-ONLY-FG = "Y"
                 MOVE "BATCH BRANCH REPORT IN PROGRESS BR# &&&&"
                    TO LEGEND
              ELSE
                 MOVE "BATCH BRANCH FILE CREATE IN PROGRESS BR# &&&&"
                    TO LEGEND
              END-IF
              INSPECT LEGEND REPLACING FIRST "&&&&" BY IA-BRANCH
              PERFORM SEND-LEGEND
              PERFORM GET-BR
              IF IO-FG NOT = 0
                 PERFORM INVALID-BRANCH
                 GO TO NEXT-IA
              END-IF
      * IF BRANCH IS NOT ON THIS MACHINE DON'T BOTHER PRINTING ON
      * EXCEPTION, IT WILL SHOW UP ON THE OTHER MACHINES REPORT
              IF BR-MACHINE NOT = EXT-FILPATH-MACHINE
                 PERFORM OTHER-MACHINE-BRANCH
                 GO TO NEXT-IA
              END-IF
      * SETUP BRANCH FILE PATHS
              MOVE EXT-FILPATH-BASE TO FILPATH-BASE
              MOVE BR-MACHINE       TO FILPATH-MACHINE
              MOVE BR-DATA-PATH     TO FILPATH-DATA

              MOVE "B"   TO LN-PATH-OVERRIDE
                            BW-PATH-OVERRIDE
                            LTP-PATH-OVERRIDE
                            GB-PATH-OVERRIDE

              PERFORM SETUP-BP-PATH
      * B - BRANCH DIRECTORY DOES NOT EXIST
              IF VALID-BRANCH-FG NOT = "B"
                 MOVE "Y" TO VALID-BRANCH-FG.

           IF VALID-BRANCH-FG = "B"
              PERFORM INVALID-BRANCH
              GO TO NEXT-IA.

           IF VALID-BRANCH-FG = "O"
              PERFORM OTHER-MACHINE-BRANCH
              GO TO NEXT-IA.

           IF VALID-BRANCH-FG = "L"
              PERFORM INVALID-LOAN-PATH
              GO TO NEXT-IA.

           IF HOLD-BATCH-MULTIPLE-REFCDS = "Y"
              MOVE "BR"       TO CD-TYPE
              MOVE HOLD-REFCD TO CD-CODE
              PERFORM READ-CD1-FILE
              IF (IO-BAD                ) OR
                 (CD-BR-FILE-TYPE = "LC" OR "C")
                 MOVE "INVALID REFCD: &&&&&" TO HOLD-MESS
                 INSPECT HOLD-MESS REPLACING FIRST "&&&&&" BY
                                                HOLD-REFCD
                 PERFORM DETAIL-LINE-PROCESS
                 ADD 1 TO TOTAL-BADREF(1) TOTAL-BADREF(2)
                          TOTAL-READ(1)   TOTAL-READ(2)
                 ADD IA-PMT-AMOUNT TO TOTAL-AMT-BADREF(1)
                                      TOTAL-AMT-BADREF(2)
                                      TOTAL-AMT-READ(1)
                                      TOTAL-AMT-READ(2)
                 GO TO NEXT-IA.

           ADD 1 TO TOTAL-READ(1)    TOTAL-READ(2).
           ADD IA-PMT-AMOUNT TO TOTAL-AMT-READ(1)
                                TOTAL-AMT-READ(2).
      *    -------------------------------------------------------------
      *    GET ACCOUNT INFO NOT SUPPLIED FROM MELLON BANK (NAME & SSNO)
      *    MOVE C TO VALID-FLAG - WE WILL CREATE BP RECORD - USED IN
      *    DECIDING WHETHER TO PRINT REJECTS
      *    -------------------------------------------------------------
           MOVE "C" TO VALID-FLAG.

      * NEED FOR LCAS ROUTINES (REPLACES GLOBRD)
           PERFORM OPEN-GB-FILE.
           MOVE GB-PATH-OWNBR TO GB-BRNO.
           MOVE 1             TO GB-NO.
           PERFORM READ-GB-FILE.
           PERFORM CLOSE-GB-FILE.
           IF IO-BAD
              MOVE "INVALID GLOBAL FILE!" TO HOLD-MESS
              PERFORM DETAIL-LINE-PROCESS
              ADD 1             TO TOTAL-BADACCT(1)
                                   TOTAL-BADACCT(2)
              ADD IA-PMT-AMOUNT TO TOTAL-AMT-BADACCT(1)
                                   TOTAL-AMT-BADACCT(2)
              MOVE 0 TO LN-SSNO(1)
              GO TO CREATE-BP.

           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-BW1-FILE.

           MOVE IA-NUMBER TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-BAD
              MOVE "INVALID ACCOUNT!!" TO HOLD-MESS
              PERFORM DETAIL-LINE-PROCESS
              ADD 1             TO TOTAL-BADACCT(1)
                                   TOTAL-BADACCT(2)
              ADD IA-PMT-AMOUNT TO TOTAL-AMT-BADACCT(1)
                                   TOTAL-AMT-BADACCT(2)
              MOVE 0 TO LN-SSNO(1)
              GO TO CREATE-BP.

           MOVE "Y" TO VALID-FLAG.
           MOVE LN-SSNO(1)   TO BW-SSNO HOLD-SSNO.
           PERFORM PROCESS-BORROWER.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.

           PERFORM GET-SP.
           IF IO-BAD
              MOVE "BAD SPR: &&&&&&&&&!"  TO HOLD-MESS
              INSPECT HOLD-MESS REPLACING FIRST "&&&&&&&&&"
                                             BY SP1-KEY
              PERFORM DETAIL-LINE-PROCESS
              ADD 1             TO TOTAL-INVALID(1)
                                   TOTAL-INVALID(2)
              ADD IA-PMT-AMOUNT TO TOTAL-AMT-INVALID(1)
                                   TOTAL-AMT-INVALID(2)
              GO TO CREATE-BP.

           MOVE SPACES TO HOLD-BP-TRCD.

      *    -------------------------------------------------------------
      *    PERMANENT;
      *     DON'T ALLOW POSTING WHEN LOAN HAS ZERO BALANCE
      *    -------------------------------------------------------------
           IF LN-CURBAL = 0
              MOVE "ALREADY ZERO BALANCE" TO HOLD-MESS
                 GO TO CREATE-BP.

           PERFORM GET-PAYOFF.

           COMPUTE NET-POFF-PRIN =
              LN-CURBAL - LN-OT2BAL + LN-OTHBAL - TOT-POFF-REBATES.

      *    DETERMINE AMOUNT THAT WILL PAYOFF ACCOUNT:
           COMPUTE WILL-POFF-ACCOUNT =
              NET-POFF-PRIN + POFF-INTDUE + POFF-LCDUE.

      *    DETERMINE TRAMT THAT CAN BE POSTED LEAVING $0.01 IN PRINCIPAL:
           COMPUTE VALID-POSTING-NOPOFF = WILL-POFF-ACCOUNT - 0.01.
      *    -------------------------------------------------------------
      *    PERMANENT;
      *    TEST IF PAYMENT WILL BRING BALANCE BELOW A NET PAYOFF:
      *    -------------------------------------------------------------
           IF IA-PMT-AMOUNT > POFF-NETDUE
              IF LN-PLDATE NOT = 0
                 IF ( CD-BR-ALLOT-OPTION = "Y" AND
                      BR-BP-PL = "P"
                    )
                    OR
                    ( CD-BR-LBOX-OPTION = "Y" AND
                      BR-LBOX-PL = "P"
                    )
                    MOVE "PMT EXCEEDS PAYOFF" TO HOLD-MESS
                    GO TO CREATE-BP.

      *    -------------------------------------------------------------
      *    PERMANENT;
      *    TEST IF PAYMENT WILL BRING BALANCE BELOW THE CURRENT LOAN
      *    BALANCE
      *    -------------------------------------------------------------
           IF IA-PMT-AMOUNT > (LN-CURBAL - LN-OT2BAL) - .01
              IF LN-PLDATE NOT = 0
                 IF ( CD-BR-ALLOT-OPTION = "Y" AND
                      BR-BP-PL = "P"
                    )
                    OR
                    ( CD-BR-LBOX-OPTION = "Y" AND
                      BR-LBOX-PL = "P"
                    )
                    MOVE "PMT EXCEEDS PRINCIPL " TO HOLD-MESS
                    GO TO CREATE-BP.

           IF IA-PMT-AMOUNT > POFF-NETDUE
              IF ( (CD-BR-ALLOT-OPTION = "Y"    ) AND
                   (HOLD-BP-PAYOFF-NONCASH = "Y"  )
                 )
                 OR
                 ( (CD-BR-LBOX-OPTION = "Y"     ) AND
                   (HOLD-LBOX-PAYOFF-NONCASH = "Y")
                 )
                 MOVE "99" TO HOLD-BP-TRCD
              ELSE
                 MOVE "PMT EXCEEDS PAYOFF" TO HOLD-MESS
                 GO TO CREATE-BP.

      *    -------------------------------------------------------------
      *    PERMANENT;
      *    TEST IF PAYMENT WILL PAYOFF LOAN PREMATURLY:
      *
      *     N O T E:   THIS TEST IS NOT CORRECT, IT DOESN'T CONSIDER
      *                 LN-LASTPYAMT OR PAYMENT SCHEDULES
      *    7/14/08 - STARRED OFF, WORLD RUNS WITH BP-PAYOFF-NONCASH NO,
      *              BUT WANTS EXACT PAYOFF TO GO THRU
      *    -------------------------------------------------------------
           MOVE "N" TO PAYOFF-TRANS-FG.

      *    IF IA-PMT-AMOUNT = POFF-NETDUE
      *       MOVE "Y" TO PAYOFF-TRANS-FG
      *       IF IA-PMT-AMOUNT NOT = LN-REGPYAMT
      *          IF ( (CD-BR-ALLOT-OPTION = "Y"    ) AND
      *               (HOLD-BP-PAYOFF-NONCASH = "Y"  )
      *             )
      *             OR
      *             ( (CD-BR-LBOX-OPTION = "Y"     ) AND
      *               (HOLD-LBOX-PAYOFF-NONCASH = "Y")
      *             )
      *             MOVE "99" TO HOLD-BP-TRCD
      *          ELSE
      *             MOVE "NO EARLY PAYOFF" TO HOLD-MESS
      *             GO TO CREATE-BP.

           IF IA-PMT-AMOUNT = POFF-NETDUE
              MOVE "Y" TO PAYOFF-TRANS-FG
                 MOVE "99" TO HOLD-BP-TRCD.
      *    -------------------------------------------------------------
      *    OPTIONAL;
      *    WHEN BR-BP/LBOX-ALLOW-PMT-ACCT-OTHBAL = "Y"
      *         THAN ALLOW THE BATCH PAYMENT TO BE APPLIED
      *         TO THE ACCOUNT BUT NOT TO OTHER BALANCE
      *
      * 4/12/07  WE THINK THE CHECK BELOW MAKES SENSE IF THERE IS
      *          AN OTHER BAL ON THE ACCOUNT.  IF THERE IS NO OTHER
      *          BAL, THIS STOPS A PAYOFF FROM POSTING AND GIVE
      *          STUPID CUTOFF MESSAGE.   SO WE WILL ONLY DO THIS
      *          CHECK IF THERE IS AN OTHER BAL.  BARB & CINDY
      *
           IF LN-OTHBAL = 0
              GO TO TEST-PAYOFF-ALLOWED.

           IF ( ( CD-BR-ALLOT-OPTION = "Y"          ) AND
                ( HOLD-BP-ALLOW-PMT-ACCT-OTHBAL = "Y" )
              )
              OR
              (
                ( CD-BR-LBOX-OPTION = "Y"             ) AND
                ( HOLD-LBOX-ALLOW-PMT-ACCT-OTHBAL = "Y" )
              )
              IF BP-TRAMT > (VALID-POSTING-NOPOFF - LN-OTHBAL)
                 MOVE "PMT > POFF- .01 +OTH" TO HOLD-MESS
                 GO TO CREATE-BP.

      *    -------------------------------------------------------------
      *    OPTIONAL;
      *    TEST IF PAYOFF IS ALLOWED:
      *    -------------------------------------------------------------
       TEST-PAYOFF-ALLOWED.
           IF ( (CD-BR-ALLOT-OPTION = "Y"  ) AND
                (BR-BP-NOPAYOFF NOT = "Y"  )
              )
              OR
              ( (CD-BR-LBOX-OPTION = "Y"   ) AND
                (BR-LBOX-NOPAYOFF NOT = "Y")
              )
              IF IA-PMT-AMOUNT = POFF-NETDUE
                 MOVE "PAYOFF NOT ALLOWED" TO HOLD-MESS
                 GO TO CREATE-BP.

      *    -------------------------------------------------------------
      *    OPTIONAL;
      *    -------------------------------------------------------------
           IF ( (CD-BR-ALLOT-OPTION = "Y") AND
                (BR-BP-LC NOT = "Y"      )
              )
              OR
              ( (CD-BR-LBOX-OPTION = "Y" ) AND
                (BR-LBOX-LC NOT = "Y"    )
              )
              MOVE IA-FROM-DATE TO IBPC-DATE
              PERFORM IBPC-TEST
              IF (IBPC-FG = "P" OR LN-LOANTYPE = "I")
                                        AND (LN-JDDATE = 0)
                                         AND (LN-ACCELDATE = 0)
                 MOVE IA-PMT-AMOUNT TO LCAP-TRAMT
                 MOVE IA-FROM-DATE TO LCAP-PAYDATE
                 MOVE LN-1STPYDATE  TO LCAP-1STPYDATE
                 MOVE "PY"          TO LCAP-LPTRCD
                 MOVE 0             TO LCAP-LPAPLC LCAP-LPAPINT
                 MOVE CD-CODE       TO LCAP-BATCH-REFCD
                 MOVE CD-BR-LC-FG   TO LCAP-BATCH-REFCD-LC-FG
                 PERFORM LATE-CHARGE-APPLY
                 COMPUTE TEST-AMT  = LCAP-APP + LCAP-OWE - LN-LCBAL
                 IF TEST-AMT NOT = 0
                    MOVE "LATE CHARGE REQUIRED" TO HOLD-MESS
                    GO TO CREATE-BP.

      *    -------------------------------------------------------------
      *    OPTIONAL;
      *     TEST IF PAYMENT WILL ONLY COVER LC AND INTEREST:
      *    -------------------------------------------------------------
           IF ( (CD-BR-ALLOT-OPTION = "Y") AND
                (BR-BP-NOPRIN NOT = "Y"  )
              )
              OR
              ( (CD-BR-LBOX-OPTION = "Y" ) AND
                (BR-LBOX-NOPRIN NOT = "Y")
              )
              COMPUTE TEST-AMT = POFF-INTDUE + POFF-LCDUE
              IF IA-PMT-AMOUNT NOT > TEST-AMT
                 MOVE "PMT APPLIES- NO PRIN" TO HOLD-MESS
                 GO TO CREATE-BP.

      *    -------------------------------------------------------------
      *    PERMANENT;
      *     TEST IF PAYMENT WILL CAUSE NEGATIVE PURCHASE BALANCE:
      *
      *     N O T E:   THIS TEST IS NOT CORRECT, IT DOESN'T CONSIDER
      *                 REFUNDS, ETC
      *    -------------------------------------------------------------
           GO TO SKIP-PURBAL-CHECK.
           COMPUTE TEST-AMT = IA-PMT-AMOUNT - POFF-INTDUE - POFF-LCDUE.
              IF TEST-AMT > LN-PURBAL
                 IF ( (CD-BR-ALLOT-OPTION = "Y"    ) AND
                      (HOLD-BP-PAYOFF-NONCASH = "Y"  )
                    )
                    OR
                    ( (CD-BR-LBOX-OPTION = "Y"     ) AND
                      (HOLD-LBOX-PAYOFF-NONCASH = "Y")
                    )
                    MOVE "99" TO HOLD-BP-TRCD
                 ELSE
                    MOVE "PMT EXCEEDS PURCHBAL" TO HOLD-MESS
                    GO TO CREATE-BP.

      *    -------------------------------------------------------------
      *    PERMANENT;
      *     TEST IF PAYMENT WILL CAUSE NEGATIVE CURRENT BALANCE:
      *
      *     WHEN OT2BAL EXISTS, INSURE AT LEAST ($0.01 + LN-OT2BAL)
      *     REMAINS IN LN-CURBAL:
      *
      *    -------------------------------------------------------------
       SKIP-PURBAL-CHECK.
           COMPUTE TEST-AMT = IA-PMT-AMOUNT - POFF-INTDUE - POFF-LCDUE.
           IF LN-OT2BAL NOT = 0
              IF TEST-AMT >= (LN-CURBAL - LN-OT2BAL - TOT-POFF-REBATES)
                 IF ( (CD-BR-ALLOT-OPTION = "Y"    ) AND
                      (HOLD-BP-PAYOFF-NONCASH = "Y"  )
                    )
                    OR
                    ( (CD-BR-LBOX-OPTION = "Y"     ) AND
                      (HOLD-LBOX-PAYOFF-NONCASH = "Y")
                    )
                    MOVE "99" TO HOLD-BP-TRCD
                 ELSE
                    MOVE "PMT EXCEEDS O2 BAL." TO HOLD-MESS
                    GO TO CREATE-BP
                 END-IF
              END-IF
           ELSE
      *       WHEN NO OT2BAL, DON'T LET LN-CURBAL GO NEGATIVE:
              IF TEST-AMT > LN-CURBAL
                 IF ( (CD-BR-ALLOT-OPTION = "Y"    ) AND
                      (HOLD-BP-PAYOFF-NONCASH = "Y"  )
                    )
                    OR
                    ( (CD-BR-LBOX-OPTION = "Y"     ) AND
                      (HOLD-LBOX-PAYOFF-NONCASH = "Y")
                    )
                    MOVE "99" TO HOLD-BP-TRCD
                 ELSE
                    MOVE "PAY EXCEEDS CURBAL" TO HOLD-MESS
                    GO TO CREATE-BP.

      *    -------------------------------------------------------------
      *    PERMANENT;
      *     DON'T ALLOW POSTING WHEN LOAN HAS ZERO BALANCE
      *    -------------------------------------------------------------
           IF LN-CURBAL = 0
              MOVE "ALREADY ZERO BALANCE" TO HOLD-MESS
                 GO TO CREATE-BP.

      *    -------------------------------------------------------------
      *    VALID ACCOUNT NUMBER
      *    -------------------------------------------------------------

           IF HOLD-CD-BR-POST-SS-TRCD = "Y"
              IF LN-REPOCD NOT = "X"
                  MOVE "NON REPO BATCH REC!"  TO HOLD-MESS
                  PERFORM DETAIL-LINE-PROCESS
                    ADD 1             TO TOTAL-SKIP(1)
                                         TOTAL-SKIP(2)
                    ADD IA-PMT-AMOUNT TO TOTAL-AMT-SKIP(1)
                                         TOTAL-AMT-SKIP(2)
                  GO TO NEXT-IA.

           IF HOLD-BP-TRCD = "99"
              MOVE "PAYOFF" TO HOLD-MESS
           ELSE
              MOVE "PAYMENT " TO HOLD-MESS.

       CREATE-BP.
           PERFORM WRITE-BP-RECORDS.
           PERFORM UPDATE-REFCD-TOTALS.
           ADD 1             TO TOTAL-WRITTEN(1)
                                TOTAL-WRITTEN(2)
           ADD IA-PMT-AMOUNT TO TOTAL-AMT-WRITTEN(1)
                                TOTAL-AMT-WRITTEN(2).

      *    -------------------------------------------------------------
      *    PRINT CREATED STATUS IN EXCEPTION REPORT
      *    -------------------------------------------------------------
           PERFORM DETAIL-LINE-PROCESS.

           GO TO NEXT-IA.

      *-----------------------------------------------------------------
       END-ROUTINE.
           PERFORM WRITE-BP-TRAILER.
           PERFORM CLOSE-BP1-FILE.
           PERFORM CLOSE-BR-FILE.

           MOVE 1 TO SUB.
           PERFORM TOTAL-ROUTINE.

           MOVE 2 TO SUB.
           PERFORM TOTAL-ROUTINE.

           IF REPORT-ONLY-FG = "Y"
              GO TO END-PROGRAM.

      *    -------------------------------------------------------------
      *    RENAME BTFILE TO BTFILE{BATCH DATE}{BATCH NO}
      *    LONPW1 HAS BEEN CHANGED TO GET THE GLOBAL BATCH DATE/NO HERE
      *           " /USR/DATA/R#/ED/CCFILE  /USR/DATA/R#/ED/CCFIL"
      *    -------------------------------------------------------------
           MOVE SPACES TO SYSTEM-BUF.
           MOVE IA-PATH-SHORT-NAME TO MESS.
           INSPECT MESS REPLACING FIRST "  "   BY "+ ".
           INSPECT MESS REPLACING FIRST "+   " BY BATCH-DATE.
           INSPECT MESS REPLACING FIRST "  "   BY "+ ".
           INSPECT MESS REPLACING FIRST "+  "  BY BATCH-NO.
           STRING MV-CMD
                  " "
                  IA-PATH
                  " "
                  MESS
                  DELIMITED BY SIZE INTO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.
           GO TO END-PROGRAM.

      ******************************************************************
      *
      *    C R E A T E - B P - F I L E
      *
      *       /USR/DATA/R1/XX####/ED/B####MMDD???REFCD    LOCAL-PATH
      *
      ******************************************************************
       SETUP-BP-PATH SECTION.
           IF REPORT-ONLY-FG = "Y"
              GO TO EXIT-SETUP-BP-PATH.

           MOVE "Y" TO BP-PATH-OVERRIDE.
           MOVE EXT-FILPATH-BASE TO BP-ALL-BASE
                                    BRANCH-PATH-BASE.
           MOVE BR-MACHINE       TO BP-ALL-MACHINE
                                    BRANCH-PATH-MACHINE.
           MOVE BR-DATA-PATH     TO BP-ALL-DATA
                                    BRANCH-PATH-DATA.
           MOVE BR-NO            TO BP-ALL-BRANCH.
           MOVE BATCH-DATE       TO BP-ALL-DATE.
           MOVE BATCH-NO         TO BP-ALL-BATCH-NO.
           MOVE SV-BP-WK-REFCD   TO BP-ALL-REFCD.

           MOVE BP-ALL-PATH TO BP-PATH.

           PERFORM LOAD-BP1-FILE.

           MOVE BP-ALL-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE BRANCH-PATH TO DIR-ACCESS-BUF
              PERFORM DIR-ACCESS-CALL
              IF STAT NOT = "00"
                 MOVE "B" TO VALID-BRANCH-FG 
              ELSE
                 PERFORM OPEN-BP1-FILE-OUTPUT
                 PERFORM CLOSE-BP1-FILE
                 MOVE "N" TO BP-OPEN-FG.

       EXIT-SETUP-BP-PATH.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - B P - R E C O R D S
      *
      ******************************************************************
       WRITE-BP-RECORDS SECTION.
           IF REPORT-ONLY-FG = "Y"
              GO TO EXIT-WRITE-BP-RECORDS.
           IF BP-OPEN-FG = "N"
              PERFORM OPEN-BP1-FILE
              MOVE "Y" TO BP-OPEN-FG.
           MOVE SPACES              TO BP-REC.
           MOVE "N"                 TO DUP-FLAG.
           MOVE IA-NUMBER           TO BP-LNNO.
           MOVE LN-SSNO(1)          TO BP-SSNO
           MOVE "PA"                TO BP-TRCD.
           MOVE 4                   TO BP-SIGN.
           MOVE IA-PMT-AMOUNT       TO BP-TRAMT.
           MOVE HOLD-REFCD          TO BP-REFCD.
           MOVE IA-FROM-DATE        TO BP-PAYDATE.
           MOVE IA-CLAIM-NUMBER     TO BP-CLAIM-NUMBER.
           MOVE IA-PMT-TYPE         TO BP-PMT-TYPE.

           MOVE 0                   TO BP-AUCTION-FEES.
           MOVE SPACES              TO BP-AUCTION-NAME.

           PERFORM WRITE-THE-BP-RECORD.
       EXIT-WRITE-BP-RECORDS.
           EXIT.

      ******************************************************************
       WRITE-THE-BP-RECORD SECTION.
      ******************************************************************
           MOVE BP-REC TO HOLD-BP-REC.
           MOVE 1 TO HOLD-BP-SEQNO.
       TRY-TO-WRITE-BP.
           MOVE HOLD-BP-REC TO BP-REC.
           MOVE HOLD-BP-SEQNO TO BP-SEQNO.
           PERFORM WRITE-BP1-FILE.
           IF IO-FG NOT = 0
              IF E-CODE NOT = "22"
                 GO TO IO-ERROR
              ELSE
                 IF HOLD-BP-SEQNO < 99
                    ADD 1 TO HOLD-BP-SEQNO
                    GO TO TRY-TO-WRITE-BP
                 ELSE
                    MOVE "MORE THAN 99 PAYMNTS" TO HOLD-MESS
                    PERFORM DETAIL-LINE-PROCESS
                    ADD 1             TO TOTAL-SKIP(1)
                                         TOTAL-SKIP(2)
                    ADD IA-PMT-AMOUNT TO TOTAL-AMT-SKIP(1)
                                         TOTAL-AMT-SKIP(2).

      ******************************************************************
      *
      *    W R I T E - B P - T R A I L E R
      *
      ******************************************************************
       WRITE-BP-TRAILER SECTION.
           IF REPORT-ONLY-FG = "Y"
              GO TO EXIT-WRITE-BP-TRAILER.

           IF TOTAL-WRITTEN(1) = 0
              GO TO EXIT-WRITE-BP-TRAILER.

           IF BP-OPEN-FG = "N"
              PERFORM OPEN-BP1-FILE
              MOVE "Y" TO BP-OPEN-FG.

           MOVE SPACES               TO BP-REC.
           MOVE 0                    TO BP-LNNO.
           MOVE 1                    TO BP-SEQNO.
           MOVE "M"                  TO BP-TYPE.
           MOVE TOTAL-WRITTEN(1)     TO BP-TOTAL-COUNT.
           MOVE TOTAL-AMT-WRITTEN(1) TO BP-TOTAL-AMOUNT.
           PERFORM WRITE-BP1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.
       EXIT-WRITE-BP-TRAILER.
           EXIT.

      ******************************************************************
      * UPDATE TOTALS BY REFCD BY BRANCH & GRAND (WK-BRNO = 10000)
      * NOT DOING REFCD TOTALS FOR EACH BRANCH FOR NOW. BLM
      ******************************************************************
       UPDATE-REFCD-TOTALS SECTION.
      *    MOVE IA-BRANCH   TO WK-BRNO.
      *    MOVE HOLD-REFCD TO WK-REFCD.
      *    PERFORM READ-WK-FILE.
      *    IF IO-BAD
      *       MOVE IA-BRANCH   TO WK-BRNO
      *       MOVE HOLD-REFCD TO WK-REFCD
      *       MOVE 0 TO WK-AMT WK-COUNT
      *       PERFORM WRITE-WK-FILE
      *          IF IO-BAD
      *             GO TO EXIT-UPDATE-REFCD-TOTALS.
      *    ADD IA-PMT-AMOUNT TO WK-AMT.
      *    ADD 1 TO WK-COUNT.
      *    PERFORM REWRITE-WK-FILE.

           MOVE 10000      TO WK-BRNO.
           MOVE HOLD-REFCD TO WK-REFCD.
           PERFORM READ-WK-FILE.
           IF IO-BAD
              MOVE IA-BRANCH TO WK-BRNO
              MOVE HOLD-REFCD TO WK-REFCD
              MOVE 0 TO WK-AMT WK-COUNT
              PERFORM WRITE-WK-FILE
              IF IO-BAD
                 GO TO EXIT-UPDATE-REFCD-TOTALS.

           ADD IA-PMT-AMOUNT TO WK-AMT.
           ADD 1 TO WK-COUNT.
           PERFORM REWRITE-WK-FILE.
       EXIT-UPDATE-REFCD-TOTALS.
           EXIT.

      ******************************************************************
      *
      *    T O T A L - R O U T I N E
      *
      ******************************************************************
       TOTAL-ROUTINE SECTION.
           IF TOTAL-READ(SUB) = 0
              GO TO EXIT-TOTAL-ROUTINE.
           IF SUB = 1
              IF TOTAL-READ(1) = TOTAL-OM(1)
                 GO TO EXIT-TOTAL-ROUTINE.
           IF SUB = 1
              MOVE "TOTALS BRANCH #&&&&& " TO D-TOT-PROMPT
              INSPECT D-TOT-PROMPT REPLACING FIRST "&&&&&"
                                             BY HOLD-BRANCH
           ELSE
              MOVE "GRAND TOTALS         "   TO D-TOT-PROMPT.
           MOVE "TOTAL RECORDS READ           :" TO D-TOT-TYPE.
           MOVE TOTAL-READ(SUB)                  TO D-TOT-COUNT.
           MOVE TOTAL-AMT-READ(SUB)              TO D-TOT-AMT.
           MOVE 2                                TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF TOTAL-OM(SUB) NOT = 0
              MOVE "TOTAL ON OTHER MACHINE       :" TO D-TOT-TYPE
              MOVE TOTAL-OM(SUB)                    TO D-TOT-COUNT
              MOVE TOTAL-AMT-OM(SUB)                TO D-TOT-AMT
              MOVE 1                                TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

           IF TOTAL-INVALID(SUB) NOT = 0
              MOVE "TOTAL INVALID BRANCH/PATH/SPR:" TO D-TOT-TYPE
              MOVE TOTAL-INVALID(SUB)               TO D-TOT-COUNT
              MOVE TOTAL-AMT-INVALID(SUB)           TO D-TOT-AMT
              MOVE 1                                TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

           IF TOTAL-BADACCT(SUB) NOT = 0
              MOVE "TOTAL INVALID ACCOUNT #      :" TO D-TOT-TYPE
              MOVE TOTAL-BADACCT(SUB)               TO D-TOT-COUNT
              MOVE TOTAL-AMT-BADACCT(SUB)           TO D-TOT-AMT
              MOVE 1                                TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

           IF TOTAL-BADREF(SUB) NOT = 0
              MOVE "TOTAL INVALID REFCD          :" TO D-TOT-TYPE
              MOVE TOTAL-BADREF(SUB)                TO D-TOT-COUNT
              MOVE TOTAL-AMT-BADREF(SUB)            TO D-TOT-AMT
              MOVE 1                                TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

           IF TOTAL-SKIP(SUB) NOT = 0
              MOVE "TOTAL ACCTS SKIPPED          :" TO D-TOT-TYPE
              MOVE TOTAL-SKIP(SUB)                  TO D-TOT-COUNT
              MOVE TOTAL-AMT-SKIP(SUB)              TO D-TOT-AMT
              MOVE 1                                TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

           IF REPORT-ONLY-FG = "Y"
              MOVE "TOTAL BP'S TO BE CREATED     :" TO D-TOT-TYPE
           ELSE
              MOVE "TOTAL BP'S CREATED           :" TO D-TOT-TYPE.
           MOVE TOTAL-WRITTEN(SUB)                  TO D-TOT-COUNT.
           MOVE TOTAL-AMT-WRITTEN(SUB)              TO D-TOT-AMT.
           MOVE 1                                   TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF TOTAL-PAYOFF(SUB) NOT = 0
              MOVE "TOTAL PAYOFF RECORDS      :" TO D-TOT-TYPE
              MOVE TOTAL-PAYOFF(SUB)             TO D-TOT-COUNT
              MOVE TOTAL-AMT-PAYOFF(SUB)         TO D-TOT-AMT
              MOVE 1                             TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

      *    PERFORM PRINT-REFCD-TOTALS.

           IF SUB = 2
              AND (TOTAL-INVALID(2) NOT = 0 OR
                   TOTAL-BADACCT(2) NOT = 0 OR
                   TOTAL-BADREF(2) NOT = 0)
                   MOVE "ERRORS EXIST; CHECK REPORT" TO  MESS
                   PERFORM SEND-MESS.

           IF SUB = 1
              MOVE 2 TO LN-FEED
              GO TO EXIT-TOTAL-ROUTINE.

           PERFORM PRINT-REFCD-TOTALS.

           MOVE 99 TO LN-COUNT.

       EXIT-TOTAL-ROUTINE.
           MOVE 0  TO TOTAL-READ(1)    TOTAL-AMT-READ(1)
                      TOTAL-OM(1)      TOTAL-AMT-OM(1)
                      TOTAL-INVALID(1) TOTAL-AMT-INVALID(1)
                      TOTAL-BADACCT(1) TOTAL-AMT-BADACCT(1)
                      TOTAL-SKIP(1)    TOTAL-AMT-SKIP(1)
                      TOTAL-BADREF(1)  TOTAL-AMT-BADREF(1)
                      TOTAL-PAYOFF(1)  TOTAL-AMT-PAYOFF(1)
                      TOTAL-WRITTEN(1) TOTAL-AMT-WRITTEN(1).

      ******************************************************************
       PRINT-REFCD-TOTALS SECTION.
      ******************************************************************
           MOVE "REF CODE TOTALS      :" TO D-TOT-PROMPT.
           MOVE 2 TO LN-FEED.
           MOVE 10000 TO WK-BRNO.
           MOVE SPACES TO WK-REFCD.
           PERFORM START-WK-FILE.
       NEXT-REFCD-TOTAL.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-BAD
              GO TO EXIT-PRINT-REFCD-TOTALS.
           MOVE "TOTAL REFCD &&&&&   :" TO D-TOT-TYPE.
           INSPECT D-TOT-TYPE REPLACING FIRST "&&&&&" BY WK-REFCD.
           MOVE WK-COUNT                TO D-TOT-COUNT.
           MOVE WK-AMT                  TO D-TOT-AMT.
           PERFORM WRITE-DETAIL-LINE.
           GO TO NEXT-REFCD-TOTAL.

       EXIT-PRINT-REFCD-TOTALS.
           MOVE SPACES TO DETAIL-LINE.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

           MOVE " " TO ERRCD.

      * LEAVE THIS HERE, IT UPDATES GP RECORD
           PERFORM OPEN-GP-FILE.
           MOVE 1 TO GP-KEY.
           PERFORM READ-GP-FILE.
           PERFORM CLOSE-GP-FILE.

       ENTER-REFCD.
           MOVE "E  A050REFCD." TO VDU.
           PERFORM SCREENIO.
           IF F6-KEY
              PERFORM CODE-SCAN
              IF IO-FG NOT = 0
                 GO TO ENTER-REFCD
              ELSE
                 MOVE "00" TO VDUSTATUS.
           IF F7-KEY
              GO TO ENDIT.
           IF NOT ENTER-KEY
              GO TO ENTER-REFCD.

           PERFORM OPEN-CD1-FILE.
           MOVE "BR"   TO CD-TYPE.
           MOVE VDUBUF TO CD-CODE
                          SV-BP-WK-REFCD
                          H-TITLE-REFCD.
           PERFORM READ-CD1-FILE.
           IF (IO-BAD) OR (CD-BR-FILE-TYPE = "LC" OR "C")
              MOVE "REFERENCE CODE NOT ON FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-REFCD
           ELSE
              MOVE CD-BR-MULTIPLE-REFCDS TO HOLD-BATCH-MULTIPLE-REFCDS
              MOVE CD-BR-PAYOFF-FG       TO HOLD-CD-BR-PAYOFF-FG
              MOVE CD-BR-POST-SS-TRCD    TO HOLD-CD-BR-POST-SS-TRCD.

       DEFAULT-TODAY-TRANS-DATE.

      * ACCEPT SYSTEM DATE AS YY/MM/DD AND CONVERT TO MM/DD/YY
           ACCEPT TODAYS-DATE FROM CENTURY-DATE.
           MOVE TODAYS-DATE TO TRANS-DATE.
           MOVE "S  8000TRDATE." TO VDU.
           MOVE TRANS-DATE       TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           GO TO CHECK-IA-FILE-EXISTS.

       ENTER-TRANS-DATE.
           MOVE "ENTER TRANSACTION DATE" TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ERNM060TRDATE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO ENTER-REFCD.
           IF VDUSTATUS = "1U" GO TO ENTER-REFCD.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-TRANS-DATE.
           MOVE VDU-DATE-YYYYMMDD TO TRANS-DATE.

           IF TRANS-DATE = ZERO
              MOVE "S  8000TRDATE." TO VDU
              MOVE TODAYS-DATE TO TRANS-DATE VDU-DATE-YYYYMMDD
              PERFORM SCREENIO.

           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.

       CHECK-IA-FILE-EXISTS.
           MOVE EXT-FILPATH-BASE    TO IA-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE TO IA-PATH-MACHINE.
           MOVE CD-BR-FILE-NAME     TO IA-PATH-FILE-NAME.
           MOVE TRANS-YY            TO IA-PATH-DATE-YY.
           MOVE TRANS-MM            TO IA-PATH-DATE-MM.
           MOVE TRANS-DD            TO IA-PATH-DATE-DD.

           MOVE "S  A290GLOBALFILE." TO VDU.
           MOVE IA-PATH TO VDUBUF ACCESS-BUF.
           PERFORM SCREENIO.

           PERFORM ACCESS-CALL.
           IF STAT-BAD
              MOVE "INPUT FILE ############################# NOT FOUND!"
                 TO MESS
              INSPECT MESS
                      REPLACING FIRST "#############################"
                 BY IA-PATH
              PERFORM SEND-MESS
              GO TO ENTER-TRANS-DATE.

       REPORT-ONLY.
           MOVE "PRINT REPORT ONLY (Y/N)" TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E NA010REPORTONLY."    TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO ENTER-REFCD.
           IF VDUSTATUS = "1U" GO TO ENTER-TRANS-DATE.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO REPORT-ONLY.
           IF VDUBUF NOT = "Y" AND VDUBUF NOT = "N"
              GO TO REPORT-ONLY.
           MOVE VDUBUF TO REPORT-ONLY-FG.

      * GET NEXT BATCH NUMBER FROM GPFILE AND UPDATE GPFILE
           PERFORM OPEN-GP-FILE.
           MOVE 1 TO GP-KEY.
           PERFORM READ-GP-FILE.
           IF NOT GP-REFCD-NEXT-BATCH-VALID
              MOVE GP-REFCD-NEXT-BATCH-START TO GP-REFCD-NEXT-BATCH.

           IF REPORT-ONLY-FG = "N"
              PERFORM REWRITE-GP-FILE.

           MOVE "SNNN030BATCHID." TO VDU.
           MOVE GP-REFCD-NEXT-BATCH TO VDUNUM0
                                       BATCH-NO.
           PERFORM SCREENIO.

      * SET UP LOCAL BRANCH BATCH FILE PATH
      *  /USR/DATA/R#/XX####/ED/B####MMDD???REFCD
           MOVE EXT-FILPATH-BASE    TO LOCAL-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE TO LOCAL-PATH-MACHINE.
           MOVE EXT-FILPATH-DATA    TO LOCAL-PATH-DATA.
           MOVE "####"              TO LOCAL-PATH-BRANCH.
           MOVE TRANS-MM            TO BATCH-MM.
           MOVE TRANS-DD            TO BATCH-DD.
           MOVE BATCH-DATE          TO LOCAL-PATH-DATE.
           MOVE BATCH-NO            TO LOCAL-PATH-BATCH.
           MOVE CD-CODE             TO LOCAL-PATH-REFCD.
           MOVE "S  A200LOCALFILE."  TO VDU.
           MOVE LOCAL-PATH-DISPLAY   TO VDUBUF
                                        H-TITLE-LOCAL.
           PERFORM SCREENIO.

      * CHECK THAT NO B####MMDD???REFCD FILES EXIST
      * /$REL/SH/BPLIST_REF.SH

           MOVE SPACES TO SYSTEM-BUF.
           MOVE "REL" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           MOVE GETENV-BUF TO LIST-REF-REL LIST-REF-REL2.
           CALL "C$TOLOWER" USING LIST-REF-EC, VALUE 3.
           CALL "C$TOLOWER" USING LIST-REF-SHELL, VALUE 14.
           MOVE LOCAL-PATH-DATE   TO LIST-REF-DATE.
           MOVE LOCAL-PATH-REFCD  TO LIST-REF-REFCD.
           MOVE EXT-FILPATH-FIL TO LIST-REF-DATA.
           MOVE LIST-REF-CMD      TO SYSTEM-BUF.
           PERFORM SYSTEM-CALL.

           MOVE LIST-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT = "00"
              MOVE "BRANCH BATCH FILES EXIST FOR TODAY !!!" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

      * GET NEXT BATCH NUMBER FROM GPFILE AND UPDATE GPFILE
           PERFORM OPEN-GP-FILE.
           MOVE 1 TO GP-KEY.
           PERFORM READ-GP-FILE.
           IF NOT GP-REFCD-NEXT-BATCH-VALID
              MOVE GP-REFCD-NEXT-BATCH-START TO GP-REFCD-NEXT-BATCH.

           IF REPORT-ONLY-FG = "N"
              PERFORM REWRITE-GP-FILE.

           MOVE "SNNN030BATCHID." TO VDU.
           MOVE GP-REFCD-NEXT-BATCH TO VDUNUM0 BATCH-NO.
           PERFORM SCREENIO.

       PAGE-ON-BRANCH-YN.
           MOVE "E  A010PAGEFG." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO ENTER-REFCD.
           IF VDUSTATUS = "1U" GO TO REPORT-ONLY.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO PAGE-ON-BRANCH-YN.
           MOVE VDUBUF TO BRANCH-PAGE-FG.
           IF BRANCH-PAGE-FG NOT = "Y"
              IF BRANCH-PAGE-FG NOT = "N"
                 MOVE "PLEASE ENTER Y OR N!" TO MESS
                 PERFORM SEND-MESS
                 GO TO PAGE-ON-BRANCH-YN.

       REJECTS-ONLY-YN.
           MOVE "E  A010REJECTS." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO ENTER-REFCD.
           IF VDUSTATUS = "1U" GO TO PAGE-ON-BRANCH-YN.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO REJECTS-ONLY-YN.
           MOVE VDUBUF TO REJECTS-ONLY-FG.
           IF REJECTS-ONLY-FG NOT = "Y"
              IF REJECTS-ONLY-FG NOT = "N"
                 MOVE "PLEASE ENTER Y OR N!" TO MESS
                 PERFORM SEND-MESS
                 GO TO REJECTS-ONLY-YN.

       ACCEPT-YN.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" GO TO ACCEPT-YN.
           IF NOT (VDUBUF = "Y" OR "N" )
              GO TO ACCEPT-YN.
           IF VDUBUF = "N"
              PERFORM CLOSE-GP-FILE
              GO TO ENDIT.

           MOVE "BATCH BRANCH FILE CREATE IN PROGRESS....." TO LEGEND.
           PERFORM SEND-LEGEND.

      * SETUP TRANS-DATE, USED TO SETUP SPOOL DIRECTORY

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO TRANS-DATE.
           MOVE EXT-CONAME-USER-ID  TO H-USERID.

      * CREATE SPOOL DIRECTORY AND OPEN SPOOL FILE IN 'EO'
      
           PERFORM CREATE-SPOOL-DIR.
           IF ERRCD NOT = " "
              GO TO ENDIT.
           MOVE EX-SPOOL TO PRU-FILE.
           PERFORM OPEN-PR-FILE.
           IF IO-FG NOT = 0
              GO TO ENDIT.

           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.

           PERFORM OPEN-IA-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

      * REWRITE GP FILE HERE, IF WE DO NOT ACCEPT THEN WE DO NOT WANT
      * TO INCREMENT THE NEXT BATCH NUMBER
      
           ADD 1 TO GP-REFCD-NEXT-BATCH.
           IF NOT GP-REFCD-NEXT-BATCH-VALID
              MOVE GP-REFCD-NEXT-BATCH-START TO GP-REFCD-NEXT-BATCH.

           IF REPORT-ONLY-FG = "N"
              PERFORM REWRITE-GP-FILE.

           PERFORM CLOSE-GP-FILE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-SP1-FILE.
           MOVE 10000 TO HOLD-BRANCH.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       DETAIL-LINE-PROCESS SECTION.
      ******************************************************************
           IF REJECTS-ONLY-FG = "Y"
              IF VALID-FLAG NOT = "N"
                 GO TO EXIT-DETAIL-LINE-PROCESS.

           MOVE IA-BRANCH      TO D-BRNO.
           IF VALID-FLAG = "Y"
              PERFORM LPSTAT-DISPLAY
              MOVE LPS-DISPLAY-LNNO TO D-ACCTNO
           ELSE
              MOVE IA-NUMBER    TO D-ACCTNO.
      *    -------------------------------------------------------------
      *    IO-FG FROM THE WRITE-BP1 OR READ-BR
      *    MJD 990614 REPLACED TEST BELOW. IF IO-FG IS SET DURING WRITE
      *      DUP-FLAG IS SET TO "Y" WHICH ALLOWS DUPLICATE TRANS ON THE
      *      BP-FILE BUT STILL FLAGS THEM (*) ON BATCH LISTING. (TFC)
      *    -------------------------------------------------------------
      *--->IF IO-FG NOT = 0 OR IA-NUMBER = 0
           IF DUP-FLAG = "Y" OR IA-NUMBER = 0
              MOVE "*"          TO D-DUP.
      * PRINT LAST 4 OF SSNO
           MOVE "XXX-XX-"       TO D-SSNO5.
           MOVE HOLD-SSNO       TO D-SSNO.
           MOVE IA-PMT-AMOUNT   TO D-AMOUNT.
           MOVE HOLD-REFCD      TO D-REFCD.
           MOVE IA-FROM-DATE    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY     TO D-DATE.
           IF HOLD-SSNO = 0
              MOVE IA-FNAME     TO FIRST-NAME
              MOVE IA-LNAME     TO LAST-NAME
              PERFORM FORMAT-NAME.
           MOVE NAME-WORKER     TO D-NAME.
           MOVE IA-CLAIM-NUMBER TO D-CLAIM-NUMBER.
           MOVE IA-PMT-TYPE     TO D-PMT-TYPE.
           MOVE HOLD-MESS       TO D-MESS.

           PERFORM WRITE-DETAIL-LINE.
           MOVE SPACES TO HOLD-MESS.
       EXIT-DETAIL-LINE-PROCESS.
           EXIT.

      ******************************************************************
       PRINT-TOTAL-TRAILER SECTION.
      ******************************************************************
           MOVE 2 TO LN-FEED.
           MOVE "TOTAL TRAILER: COUNT/AMOUNT:" TO DETAIL-LINE.
           MOVE IA-TOTAL-COUNT TO D-TOT-COUNT.
           MOVE IA-TOTAL-AMT TO D-TOT-AMT.
           PERFORM WRITE-DETAIL-LINE.
           MOVE SPACES TO HOLD-MESS.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************
           ADD LN-FEED TO LN-COUNT.
           IF LN-COUNT > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************
           ADD 1            TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH        TO H-HR.
           MOVE T-MM        TO H-MIN.
           MOVE HEAD1       TO PR-REC.
           MOVE 99          TO LN-FEED.
           PERFORM WRITE-PR-REC.
           IF HOLD-CD-BR-PAYOFF-FG = "Y"
              MOVE "PAYOFF BATCH" TO H-TITLE-PAYOFF
           ELSE
              MOVE SPACES TO H-TITLE-PAYOFF.
           MOVE HEAD2       TO PR-REC.
           MOVE 1           TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE HEAD3       TO PR-REC.
           MOVE 1           TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE HEAD4       TO PR-REC.
           MOVE 2           TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 4           TO LN-COUNT.
           MOVE 2           TO LN-FEED.

      ******************************************************************
      *
      *  CREATE SPOOL DIRECTORY IN /XXX/XXXX/R#/EO
      *
      * STORE SPOOL FILE NAME AS /XXX/XXXX/R$/EO/....
      * WE NEED A FULL PATH NAME HERE ELSE DIR WILL CHANGE WHEN WE
      * CHANGE DIRECTORIES TO ANOTHER BRANCH.
      *
      * BEING ORIG DATA PATH, FULL PATH TOO LONG FOR ACCESS-BUF
      ******************************************************************
       CREATE-SPOOL-DIR SECTION.
           MOVE " " TO ERRCD.

           MOVE EXT-FILPATH-BASE    TO EX-SPOOL-BASE.
           MOVE EXT-FILPATH-MACHINE TO EX-SPOOL-MACHINE.
           MOVE EXT-FILPATH-DATA    TO EX-SPOOL-DATA.
           MOVE TRANS-DATE          TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO EX-SPOOL-DATE.
           MOVE " "                 TO EX-SPOOL-FILE.
           MOVE EX-SPOOL TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF STAT NOT = "00"
              CALL "C$MAKEDIR" USING DIR-ACCESS-BUF GIVING STAT-99
              IF STAT NOT = "00"
                 MOVE "CANNOT CREATE SPOOL DIRECTORY" TO MESS
                 PERFORM SEND-MESS
                 MOVE DIR-ACCESS-BUF TO MESS
                 PERFORM SEND-MESS
                 MOVE "E" TO ERRCD.

           MOVE "/"      TO EX-SPOOL-FILE.
           MOVE "LOTSBP" TO EX-SPOOL-NAME.
           MOVE "  "     TO EX-SPOOL-SUF.

      *****************************************************************
      * THEY ARE ALLOWED TO RUN THIS  MORE THAN ONCE ON THE
      * SAME TRANSACTION DATE, SO CHECK TO SEE IF REPORT EXISTS, IF
      * SO, INCREMENT THE STEP # SO WE DON'T WIPE OUT PRIOR REPORTS
      *****************************************************************
           MOVE 1 TO STEP-COUNT.
       TRY-NEXT-STEP-NO.
           MOVE STEP-COUNT TO EX-SPOOL-STEP.
           MOVE EX-SPOOL TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT = "00"
              IF STEP-COUNT < 999
                 ADD 1 TO STEP-COUNT
                 GO TO TRY-NEXT-STEP-NO.

       EXIT-CREATE-SPOOL-DIR.
           EXIT.

      ******************************************************************
       PROCESS-BORROWER SECTION.
      ******************************************************************
           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-BAD
               MOVE "*** BORROWER NOT ON FILE ***" TO NAME-WORKER
               GO TO PROCESS-BORROWER-EXIT.

           MOVE BW-LNAME TO LAST-NAME.
           MOVE BW-FNAME TO FIRST-NAME.
           PERFORM FORMAT-NAME.

       PROCESS-BORROWER-EXIT.
           EXIT.

      ******************************************************************
       FORMAT-NAME SECTION.
      ******************************************************************
           MOVE LAST-NAME TO NAME-WORKER.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?".
           INSPECT NAME-WORKER REPLACING FIRST "                "
                   BY FIRST-NAME.
           INSPECT NAME-WORKER REPLACING ALL "?" BY " ".

      ******************************************************************
       CODE-SCAN SECTION.
      ******************************************************************

           MOVE "BR"   TO CDSCAN-TYPE.
           MOVE SPACES TO CDSCAN-CODE.

           MOVE "WI/CDSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CDSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF CDSCAN-STATUS NOT = "00" OR CDSCAN-CODE = SPACES
              MOVE 9 TO IO-FG
              GO TO EXIT-CODE-SCAN.

           MOVE 0 TO IO-FG.

           MOVE "S  A050REFCD." TO VDU.
           MOVE CDSCAN-CODE TO VDUBUF.
           PERFORM SCREENIO.

           MOVE CDSCAN-CODE TO VDUBUF.

       EXIT-CODE-SCAN.
           EXIT.

      ******************************************
       GET-PAYOFF SECTION.
      ****************************************
           MOVE LN-ACCTNO    TO CRNOR2-ACCTNO.
           MOVE IA-FROM-DATE TO CRNOR2-SYSDATE.
           MOVE "PO"         TO CRNOR2-LPTRCD.
           MOVE "Y"          TO CRNOR2-GET-PAYOFF-ONLY.
           MOVE "CL/CRNOR2" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CRNOR2-LINK
                                 NO-REC POFF-WORKERS LN-REC.
           CANCEL FORM-PROGX.

      ******************************************
       INVALID-BRANCH SECTION.
      ****************************************
           MOVE "INVALID BRANCH REC!" TO HOLD-MESS.
           PERFORM DETAIL-LINE-PROCESS.
           ADD 1 TO TOTAL-INVALID(1) TOTAL-INVALID(2)
                    TOTAL-READ(1)    TOTAL-READ(2).
           ADD IA-PMT-AMOUNT TO TOTAL-AMT-INVALID(1)
                                TOTAL-AMT-INVALID(2)
                                TOTAL-AMT-READ(1)
                                TOTAL-AMT-READ(2).
           MOVE "B" TO VALID-BRANCH-FG.

      ******************************************
       OTHER-MACHINE-BRANCH SECTION.
      ****************************************
           ADD 1 TO TOTAL-OM(1)   TOTAL-OM(2)
                    TOTAL-READ(1) TOTAL-READ(2).
           ADD IA-PMT-AMOUNT TO TOTAL-AMT-OM(1)
                                TOTAL-AMT-OM(2)
                                TOTAL-AMT-READ(1)
                                TOTAL-AMT-READ(2).
           MOVE "O" TO VALID-BRANCH-FG.

      ******************************************
       INVALID-LOAN-PATH SECTION.
      ****************************************
           MOVE "NO FILES ON SYSTEM" TO HOLD-MESS.
           PERFORM DETAIL-LINE-PROCESS.
           ADD 1 TO TOTAL-INVALID(1) TOTAL-INVALID(2)
                    TOTAL-READ(1)    TOTAL-READ(2).
           ADD IA-PMT-AMOUNT TO TOTAL-AMT-INVALID(1)
                                TOTAL-AMT-INVALID(2)
                                TOTAL-AMT-READ(1)
                                TOTAL-AMT-READ(2).
           MOVE "L" TO VALID-BRANCH-FG.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBLP/LPLCAP.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBLP/LPSCHD.CPY".
       COPY "LIBLP/LPLCAS.CPY".
       COPY "LIBLP/LPLCHG.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBLP/LPSTAT.CPY".


      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO LOTSBP-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LOTSBP-SYSDATE.
           DISPLAY LOTSBP-SCREEN UPON LOTSBP-WINDOW-HANDLE.
           MOVE LOTSBP-FORM-FIELDS TO WR-BUF.
           MOVE LOTSBP-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE LOTSBP-HELP--FILE TO HELP-LINK-FILE.
           MOVE LOTSBP-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/LOTSBP_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/MKTEMP.CPY".
      *-----------------------------------------------------------------
       GET-BR.
           MOVE IA-BRANCH TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-GOOD
              IF HOLD-CD-BR-PAYOFF-FG = "Y"
                 MOVE "N" TO HOLD-BP-ALLOW-PMT-ACCT-OTHBAL
                             HOLD-LBOX-ALLOW-PMT-ACCT-OTHBAL
                 MOVE "Y" TO HOLD-BP-PAYOFF-NONCASH
                             HOLD-LBOX-PAYOFF-NONCASH
              ELSE
                 MOVE BR-BP-ALLOW-PMT-ACCT-OTHBAL TO
                    HOLD-BP-ALLOW-PMT-ACCT-OTHBAL
                 MOVE BR-LBOX-ALLOW-PMT-ACCT-OTHBAL TO
                    HOLD-LBOX-ALLOW-PMT-ACCT-OTHBAL
                 MOVE BR-BP-PAYOFF-NONCASH TO
                    HOLD-BP-PAYOFF-NONCASH
                 MOVE BR-LBOX-PAYOFF-NONCASH TO
                    HOLD-LBOX-PAYOFF-NONCASH.

      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       GET-SP.
           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.

           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE
              IF IO-BAD
                 GO TO IO-ERROR.


      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGPGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGBGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPBP1IN.CPY".
       COPY "LIBGB/GBBRIN.CPY".
       COPY "LIBGB/GBGBRN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBGB/GBGPIN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".

      *=================================================================
       OPEN-IA-FILE.
           PERFORM OPEN-IT.
           MOVE IA-PATH TO E-FILE.
           OPEN INPUT IA-FILE.
           IF IO-FG = 8
              GO TO OPEN-IA-FILE.
           IF IO-FG = 7
              CLOSE IA-FILE
              GO TO OPEN-IA-FILE.
       READ-IA-FILE-NEXT.
           PERFORM READ-IT.
           MOVE IA-PATH TO E-FILE.
           READ IA-FILE.
           IF IO-FG = 8
              GO TO READ-IA-FILE-NEXT.
       CLOSE-IA-FILE.
           PERFORM CLOSE-IT.
           CLOSE IA-FILE.

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
       EXIT-PROG.
           MOVE EXIT-PATH TO FORM-NAM.
           MOVE FORM-PATH   TO FORM-PATHNAME.
       STOP-RUN.
           DESTROY LOTSBP-WINDOW-HANDLE.
           EXIT PROGRAM.
