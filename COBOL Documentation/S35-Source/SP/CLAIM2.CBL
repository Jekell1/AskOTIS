       IDENTIFICATION DIVISION.
       PROGRAM-ID. CLAIM2.
      *****************************************************************
      *                   (SP)
      *         INSURANCE CLAIMS REGISTER AND UPDATE
      *         CREATED FROM A FILE RECEIVED FROM LIFE OF THE SOUTH
      *
      *         CREATED 8/27/04 - WORLD CHANGING FROM VOYAGER TO
      *         LIFE OF THE SOUTH, WILL CONTINUE TO GET CLAIMS FILES
      *         FROM VOYAGER.  WILL GET VOYAGER FIRST (CUTS OFF
      *         AROUND 25TH OF MONTH), THEN LIFE OF THE SOUTH WHICH
      *         CUTS OFF LAST FRIDAY OF MONTH & THEY GET FILE THE
      *         FOLLOWING MONDAY.  VOYAGER UPDATE (CLAIMS.C) MOVES
      *         STATS TO FIELDS IN RATING SHEET, THIE UPDATE WILL
      *         ADD SINCE IT WILL BE RUN SECOND.  SO IF THEY NEED
      *         TO RE-RUN THIS UPDATE, THEY NEED TO FIRST RERUN
      *         VOYAGER AND THEN RE-REUN THIS UPDATE!
      *         NO, THAT WAS A STUPID STATEMENT, BECAUSE PROBABLY
      *         WILL GET FEWER & FEWER CLAIMS FROM VOYAGER & IF
      *         YOU RE-RAN THIS, NEWER BRANCHES WOULD STILL GET
      *         DOUBLED UP.  SO IF YOU HAVE TO RE-RUN, WILL HAVE
      *         TO GET WRFILE FROM A BACKUP OR SOMETHING & RE-RUN
      *         CREATE NEW MONTH'S RATING SHEET, ETC.
      *
      *
      *  FILE MUST BE IN /USR/DATA/R1/ED/CLM.LOS.YYMMDD RSZ ???
      *                                       (INCLUDES LF(0A))
      *    UPDATE RENAMES FILE TO .UPD
      *    REMOTE COPIES FILE (.UPD) TO RISC2 FOR REPORTING ONLY
      *
      *   CREATES GT'S BY BRANCH PER TYPE - DEBIT
      *   CREATES GT FOR 9990 BY BRANCH PER TYPE - CREDIT
      *
      * UPDATES WRFILE FOR NO CLAIMS AND $ AMOUNT
      *
      * NOTE!!! NONFILE CLAIMS WILL COME UNDER TYPE 538 - THIS IS BOTH
      *         BANKRUPT & NON-BANKRUPT.  THEY USED TO KEYPUNCH ONE
      *         OF THEM, NOW WE HAVE NO WAY TO DISTINGUISH....SO
      *         EVEN THOUGH THERE IS LOGIC HERE FROM TYPE 536 IT
      *         WILL BE 0, NOTHING WILL COME WITH TYPE 536
      *
      ******************************************************************
      * REV:
      *  040913 BLV STOP UPDATE IF TRYING TO RUN ON RISC2!
      *  040913 BLV STOP IF DATE ENTERED ALREADY EXISTS WITH .UPD OR
      *             .UPDTR, TO STOP ACCIDENTALLY DOWNLOADING & UPDATING
      *             AGAIN
      *  040831 BLF FORCE REPORT TO BE OUTPUT TO A SPOOL DIRECTORY SO IT WILL
      *             NOT BE DELETED FROM TMP.  WILL BE STORED UNDER PROCEDURE
      *             CODE OF WAC, DATED THE LAST DAY OF THE MONTH FOR THE
      *             MONTH/YEAR SELECTED.  WILL ONLY DO THIS IF RUNNING FROM
      *             AND MENU & ANSWERING "N" TO REPORT ONLY.  IF RUN WITH
      *             UPDATE MORE THAN ONCE, STEP NUMBER WILL BE INCREMENTED:
      *             /USR/DATA/R1/SC9990/EO/WAC093004/0001CLAIM2,
      *             /USR/DATA/R1/SC9990/EO/WAC093004/0002CLAIM2
      *  041118 BLF CHANGE TO UPDATE LOTS NON-FILE CLAIMS FIELDS IN WRFILE
      *  041216 BLF ADD SIGN TO CLAIM-AMOUNT
      *  050105 BLF FIX SYSTEM CALL TO GET CORRECT SHELL TO REMOTE COPY
      *             CLAIMS FILE TO RISC 2
      *  050203 BLF ROUTINE TO PRINT INVALID RECORDS WAS NOT TRANSLATING
      *             WORLD COVERAGE CODE, DIDN'T PRINT INVALID CODE,
      *             AND INVALID CODES WERE GETTING THROUGH MAIN
      *             ROUTINE TO PRINT & PROCESS WITH INCORRECT TYPE
      *  050203 BLF ADD 554 TO VALID COVERAGE CODE TABLE FOR IUI
      *  050214 BLF FIX TO SUBTRACT FROM WK-COUNT IF CLAIM-AMOUNT IS NEGATIVE,
      *             NEGATIVE CLAIMS WERE ADDING TO COUNT IN RATING SHEET FILE,
      *             WORLD PR# 379
      *  050214 BLF FIX COMPUTATION OF WR-YTD-PCT-NF-BNK-LEDGER
      *  050411 BLF FIX IF F7 AFTER CLAIM DATE & REPORT-YN ENTERED, FILE
      *             WAS RENAMED & LOG REFLECTED UPDATE DONE EVEN THOUGH
      *             THERE WAS NO UPDATE, WORLD PR# 400
      *  050818 BLF IF BRANCH # NOT NUMERIC OR INVALID, CREATE ERROR LINE
      *             & DON'T PROCESS CLAIM, IF INVALID GL ACCOUNT, CREATE
      *             ERROR LINE & DON'T PROCESS CLAIM, WORLD PR# 419
      *  060413 BLF CHECK IF CREATE NEW MONTHS RATING SHEET HAS BEEN RUN BY
      *             READING WRFILE; IF NO RECORDS FOR THIS MONTH, SEND A
      *             MESSAGE & GET OUT, WORLD PR# 467
      * BLF 070119 ADD UPDATE OF GT-POST-USERID, GT-POST-DATE,
      *            GT-POST-TIME, SUPPORT PR# 2780
      * BLM 090316 ADDED CLAIM-DAYS-PAID & CLAIM-PMT-TYPE TO INPUT FILE,
      *            JUST ADDED FIELDS HERE SO INPUT FILE WILL BE CORRECT
      *            SIZE, WILL UPDATE NEW FIELDS TO TRFILE IN CLUPT2,
      *            WORLD PR# 631
      * BLM 110615 ADDED COVERAGE CODE 551 TO VALID TYPES, FOR TEXAS IUI,
      *            WORLD PR# 756
      * BLM 110627 ADDED COVERAGE CODE 248 TO VALID TYPES A&H,
      *            WORLD PR# 756
      * BLM 110628 ADDED COVERAGE CODES: 168, 169, 170, 171 FOR LIFE,
      *                                  523 & 527 FOR PROPERTY
      *            WORLD PR# 757
      * BAH 130109 MOVED COVERAGE CODE 537 FROM PROPERTY TO AUTO
      *            ADDED COVERAGE CODE 557 FOR "IN" (2XXX) INDIANA IUI
      *                                                      WORLD PR# 842
      * BLM 131231 ADDED 1 DIGIT TO D-528-CLAIM, GRAND TOTAL OF NONFILE
      *            WAS NOT PRINTING MILLIONS.  NONE OF THE OTHER TYPES
      *            WERE EVEN CLOSE TO A MILLION, SO JUST INCREASED
      *            NONFILE.  WORLD PR# 956
      *  CS 140603 REPLACED MKDIR-CMD WITH C$MAKEDIR
      * BLM 151119 ADDED COVERAGE CODES 001 & 002 FOR LIFE, WORLD PR# 1121
      * KEC 2016.1201 CHANGED GT-DATE1 GT-DATE2 GT-DATE3 AS ACTIVE CCYYMMDD.
      * BAH 170815 ACTIVATE 8 DIGIT DATES, PD0006
      * BLM 180223 FIX SPOOL DIRECTORY NOT BEING CREATED, NEEDED TO
      *            MOVE EO-SPOOL TO DIR-ACCESS-BUF BEFORE DIR-ACCESS-CALL
      * BAH 2019.04.01 FIX DEBIT/CREDIT FLAG TO BE SET BASED ON GT-AMT
      *                NEGATIVE AMOUNTS WERE CREATED AS "D" DEBITS, #1366
      * BAH 2019.0711 ADDED GLGTCL, FUTURE FIELDS WERE NOT CLEARED!
      * BAH 2020.0402 REMOVED CLAIMS-R2, AND REMOTE COPY,NO MORE RISC2 SERVER
      * BAH 2021.0323 ADDED AUTO-TYPE 636 FOR "UT" FORTEGRA LIMITED PHYSICAL
      *               DAMAGE #1493
      * BAH 2022.0720 ADDED LIFE-TYPE 112/113 FOR SINGLE/JOINT DECR LIFE
      *               WITH DISMEMBERMENT (NONGILA) AND 227/374 FOR
      *               SINGLE/JOINT DECR LIFE (GILA)  FORTEGRA       #1562
      * BAH 2023.1208 VALIDATE NON NUMERIC IMPORT FIELDS
      * BAH 2024.0212 ADD 545 TO UNEMP-TYPE #1642
      * 2024.0520 BAH ADDED COVERAGE CODE 390 TO VALID TYPES A&H #1656 S35Q-29
      *******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLAIMS-FILE ASSIGN TO CLAIMS-PATH
                  ORGANIZATION RELATIVE
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RELATIVE KEY CLAIMS-KEY
                  FILE STATUS FILE-STAT.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBGB/GBFSWKI.CPY".
      * WORK FILE FOR INVALID BRANCH/PRODUCER
           SELECT WK2-FILE ASSIGN WK2PATH
                  ORGANIZATION LINE SEQUENTIAL
                  ACCESS SEQUENTIAL
                  FILE STATUS FILE-STAT.

       DATA DIVISION.
       FILE SECTION.
      * RSZ 230 (INCLUDING 0A)
       FD  CLAIMS-FILE
           LABEL RECORDS ARE STANDARD.
       01  CLAIMS-REC.
           03  CLAIM-WAIS                 PIC X(4).
           03  CLAIM-BRNO-ALP.
               05  CLAIM-BRNO             PIC 9(4).
           03  CLAIM-COMPANY-CODE         PIC X(2).
           03  CLAIM-AGENT-NO             PIC X(10).
           03  CLAIM-AGENT-NAME           PIC X(32).
           03  CLAIM-POLICY-NO            PIC X(10).
           03  CLAIM-POLICY-SUFFIX        PIC 9(3).
           03  CLAIM-NUMBER               PIC 9(8).
           03  CLAIM-TYPE                 PIC X.
           03  CLAIM-COVERAGE-CODE        PIC 9(3).
           03  CLAIM-CHECK-NO             PIC 9(8).
           03  CLAIM-CHECK-DATE           PIC 9(8).
           03  CLAIM-AMOUNT               PIC S9(9)V99.
           03  CLAIM-POSTED-DATE          PIC 9(8).
           03  CLAIM-LOAN-NUMBER          PIC X(20).
           03  CLAIM-LNAME                PIC X(35).
           03  CLAIM-FNAME                PIC X(25).
           03  CLAIM-ID-NUMBER            PIC X(20).
           03  CLAIM-FROM-DATE            PIC 9(8).
           03  CLAIM-TO-DATE              PIC 9(8).
           03  CLAIM-PMT-TYPE             PIC X(10).
           03  CLAIM-DAYS-PAID            PIC 9(5).
           03  CLAIM-CR                   PIC X.
           03  CLAIM-LF                   PIC X.

       COPY "LIBGB/GBFDPR.CPY".
       FD  WK2-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK2-REC.
           05  WK2-CLAIMS-REC            PIC X(230).
           05  WK2-ERROR-FG              PIC X.
           05  WK2-BAD-GL-BRANCH         PIC 9(4).
           05  WK2-BAD-GL-ACCOUNT        PIC 9(7).

       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-BRNO                PIC 9(4).
               05  WK-TYPE                PIC 9(4).
           03  WK-AMOUNT                  PIC S9(7)V99 COMP-3.
           03  WK-COUNT                   PIC S9(7)    COMP-3.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/CLAIM2.CBL S35 12/08/23-07:43:40 >".

      *****************************************************************
      *  PATH OF DATED CLAIMS FILE (FOR VOYAGER INSURNACE CLAIMS)
      * FILE IS LOADED BY WORLD
      *****************************************************************
       01  CLAIMS-PATH.
           03  CLAIMS-PATH-BASE          PIC X(09).
           03  FILLER                    PIC X(01) VALUE "/".
           03  CLAIMS-PATH-MACHINE       PIC X(02).
           03  FILLER                    PIC X(12) VALUE "/ED/CLM.LOS.".
           03  CLAIMS-PATH-DATE          PIC 9(06).
           03  CLAIMS-P-DATE REDEFINES CLAIMS-PATH-DATE.
               05  CLAIMS-PATH-YY        PIC 9(02).
               05  CLAIMS-PATH-MM        PIC 9(02).
               05  CLAIMS-PATH-DD        PIC 9(02).

       01  CLAIMS-UPD-PATH.
           03  CLAIMS-PATH-UPD           PIC X(30).
           03  CLAIMS-PATH-UPD-TAG       PIC X(4)   VALUE ".UPD".

       01  CLAIMS-UPDTR-PATH.
           03  CLAIMS-PATH-UPDTR         PIC X(30).
           03  CLAIMS-PATH-UPDTR-TAG     PIC X(6)   VALUE ".UPDTR".

       COPY "LIBGL/GL01GT.CPY".
       COPY "LIBGL/GL01GT_SQL.CPY".
       COPY "LIBGL/GLWSGT.CPY".
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBSP/SP01WR.CPY".
       COPY "LIBSP/SP01WR_SQL.CPY".
       COPY "LIBSP/SPWSWR.CPY".

      * LAST MONTH RATING SHEET RECORD
       COPY "LIBSP/SP01WR.CPY" REPLACING LEADING "WR" BY "WRLM".

       01  CLAIMS-KEY         PIC 9(7)   VALUE 1.
       01  PR-IFN             PIC X(3)   VALUE "PR".
       01  WK-IFN             PIC X(3)   VALUE "WK".
       01  WK2-IFN            PIC X(3)   VALUE "WK2".
       01  WK2PATH            PIC X(30).

       01  LN-WORKERS.
           03  LN-FEED        PIC 999   VALUE 0.
           03  LN-COUNT       PIC 999   VALUE 99.
           03  LN-WK          PIC 999   VALUE 0.
       01  PAGE-NUMBER        PIC 999   VALUE 0.

       01  HOLD-BRANCH        PIC 9(4)  VALUE 0.

       01  HOLD-WR1-KEY.
           03  HOLD-WR-YYYYMM    PIC 9(6).
           03  HOLD-WR-BRNO      PIC 9(4).

       01  PROCESS-YYYYMM       PIC 9(6).
       01  LAST-MO-YYYYMM       PIC 9(6).
       01  FILLER REDEFINES LAST-MO-YYYYMM.
           03  LAST-MO-YYYY    PIC 9(4).
           03  LAST-MO-MM      PIC 99.

       01  WORLD-CLAIM-TYPE   PIC 9(3).
       01  TEST-COVERAGE-CODE PIC 9(3).
           88  LIFE-TYPE      VALUE 001 002 100 101 102 103 104 105
                                    112 113 168 169 170 171 227 374.
           88  AH-TYPE        VALUE 200 201 203 248 390.
           88  FIRE-TYPE      VALUE 511 512 517 522 523.
           88  AUTO-TYPE      VALUE 531 537 636.
           88  NONFILE-TYPE   VALUE 520.
           88  UNEMP-TYPE     VALUE 545 550 551 554 556.
           88  UNEMP-IN-TYPE  VALUE 557.
           88  ADD-TYPE       VALUE 570.

       01  WS-MTD-PERCENT     PIC S9(3)V99  VALUE 0.

       77  ACCUM-TOT-DEBIT    PIC S9(10)V99 COMP-3 VALUE +0.
       77  ACCUM-TOT-CREDIT   PIC S9(10)V99 COMP-3 VALUE +0.

       77  GRAND-510-TOTAL    PIC S9(10)V99 COMP-3 VALUE +0.
       77  GRAND-511-TOTAL    PIC S9(10)V99 COMP-3 VALUE +0.
       77  GRAND-527-TOTAL    PIC S9(10)V99 COMP-3 VALUE +0.
       77  GRAND-528-TOTAL    PIC S9(10)V99 COMP-3 VALUE +0.
       77  GRAND-536-TOTAL    PIC S9(10)V99 COMP-3 VALUE +0.
       77  GRAND-538-TOTAL    PIC S9(10)V99 COMP-3 VALUE +0.
       77  GRAND-454-TOTAL    PIC S9(10)V99 COMP-3 VALUE +0.
       77  GRAND-459-TOTAL    PIC S9(10)V99 COMP-3 VALUE +0.
       77  GRAND-463-TOTAL    PIC S9(10)V99 COMP-3 VALUE +0.
       77  GRAND-470-TOTAL    PIC S9(10)V99 COMP-3 VALUE +0.


       77  BRANCH-510-TOTAL    PIC S9(10)V99 COMP-3 VALUE +0.
       77  BRANCH-511-TOTAL    PIC S9(10)V99 COMP-3 VALUE +0.
       77  BRANCH-527-TOTAL    PIC S9(10)V99 COMP-3 VALUE +0.
       77  BRANCH-528-TOTAL    PIC S9(10)V99 COMP-3 VALUE +0.
       77  BRANCH-536-TOTAL    PIC S9(10)V99 COMP-3 VALUE +0.
       77  BRANCH-538-TOTAL    PIC S9(10)V99 COMP-3 VALUE +0.
       77  BRANCH-454-TOTAL    PIC S9(10)V99 COMP-3 VALUE +0.
       77  BRANCH-459-TOTAL    PIC S9(10)V99 COMP-3 VALUE +0.
       77  BRANCH-463-TOTAL    PIC S9(10)V99 COMP-3 VALUE +0.
       77  BRANCH-470-TOTAL    PIC S9(10)V99 COMP-3 VALUE +0.

       01  SAVE-GT-LINE       PIC 9(6)  VALUE 0.
       01  REF-WK.
           03  FILLER         PIC X(3) VALUE "LP-".
           03  REF-DATE       PIC 9(6).
           03  FILLER         PIC X(1) VALUE "-".
           03  REF-BR         PIC 9(4).

       01  FORMAT-ACCT        PIC 9(7).
       01  FILLER REDEFINES FORMAT-ACCT.
           03  ACCT-FIRST-4   PIC 9(4).
           03  ACCT-LAST-3    PIC 9(3).

       01  ERROR-FLAG         PIC X    VALUE "N".
       01  CONTINUE-UPDATE    PIC X    VALUE "Y".
       01  WK2-FLAG           PIC X    VALUE "N".
       01  FIRST-TIME         PIC X VALUE "Y".
       01  FIRST-WK           PIC X VALUE "Y".
       01  RECORDS-FOUND      PIC X VALUE " ".
           88  NO-RECORDS-FOUND     VALUE "N".

       01  MESS-LIST         PIC X(5) VALUE "MESS.".
       01  MSEL-LIST         PIC X(5) VALUE "MSEL.".
       01  LEGEND            PIC X(70).
       01  STEP-COUNT        PIC 9(4) VALUE 1.

       01  WORK-TIME          PIC 9(6).
       01  FILLER REDEFINES WORK-TIME.
           04  TIME-HHMM      PIC 9(4).
           04  TIME-SS        PIC 9(2).

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  CLAIMS-DATE     PIC 9(8).
           03  TRANS-DATE      PIC 9(8).
           03  REPORT-ONLY-FG  PIC X.

       01  VERSION-CONTROL    PIC X VALUE "A".

       01  LOG-CHANGE-MSG.
           03  LOG-BRNOX      PIC X(4).
           03  LOG-BRNO REDEFINES LOG-BRNOX
                              PIC 9(4).
           03  FILLER         PIC X     VALUE SPACES.
           03  LOG-MESS.
               05  LOG-MESS1      PIC X(25) VALUE SPACES.
               05  FILLER         PIC X(6)  VALUE "DATE: ".
               05  LOG-DATE       PIC 99/99/99.

       01  HEAD-1.
           03  HD-DATE        PIC X(8).
           03  FILLER         PIC X(3)    VALUE SPACES.
           03  HD-TIME        PIC X(8)    VALUE SPACES.
           03  FILLER         PIC X(30)   VALUE SPACES.
           03  HD-CONAME      PIC X(40).
           03  FILLER         PIC X(34)   VALUE SPACES.
           03  FILLER         PIC X(5)    VALUE "PAGE ".
           03  HD-PAGE        PIC ZZ9.

       01  HEAD-2.
           03  FILLER         PIC X(9)    VALUE "USERID:".
           03  HD-USERID      PIC X(10)   VALUE SPACES.
           03  FILLER         PIC X(25)   VALUE SPACES.
           03  FILLER         PIC X(43)   VALUE
               "INSURANCE CLAIMS UPDATE (LIFE OF THE SOUTH)".

       01  HEAD-3.
           03  FILLER        PIC X(8) VALUE "BRANCH: ".
           03  HD-BRNO       PIC Z(4).
           03  FILLER        PIC X(13) VALUE SPACES.
           03  FILLER        PIC X(37) VALUE SPACES.
           03  HD-TRANS-DATE PIC 99/99/99.

       01  HEAD-4.
           03  FILLER         PIC X(23)   VALUE SPACES.
           03  FILLER         PIC X(45)   VALUE
               "               510       511       527".
           03  FILLER         PIC X(48)   VALUE
               " 528   BK  536       538       454       459".
           03  FILLER         PIC X(16)   VALUE
               "   470       463".

       01  HEAD-5.
           03  FILLER         PIC X(23)   VALUE
               "  PAID   NAME       ".
           03  FILLER         PIC X(44)   VALUE
               "CHECK #      LIFE       A&H      FIRE".
           03  FILLER         PIC X(49)   VALUE
               "AUTO   NONFILE    NONFILE     UNEMP      AD&D".
           03  FILLER         PIC X(16)   VALUE
               "L.PROT     ROADG".


       01  HEAD-GRAND.
           03  FILLER         PIC X(23)   VALUE SPACES.
           03  FILLER         PIC X(42)   VALUE
               "            (510)     (511)     (527)".
           03  FILLER         PIC X(49)   VALUE
               "(528)     (536)      (538)     (454)     (459)".
           03  FILLER         PIC X(18)   VALUE
               "  (470)     (463)".

       01  HEAD-GT.
           03  FILLER        PIC X(67) VALUE "GENERAL LEDGER ENTRIES".
           03  FILLER        PIC X(10) VALUE "# CLAIMS".

       01  DETAIL-LINE        VALUE SPACES.
           03  D-DATE         PIC 99/99/99.
           03  FILLER         PIC X.
           03  D-NAME         PIC X(13).
           03  FILLER         PIC X.
           03  D-CHECK-NO     PIC ZZZ99999.
           03  D-CL-CLAIM     PIC ZZZZZ9.99-.
           03  D-AH-CLAIM     PIC ZZZZZ9.99-.
           03  D-PP-CLAIM     PIC ZZZZZ9.99-.
           03  D-AUTO-CLAIM   PIC ZZZZZ9.99-.
           03  D-536-CLAIM    PIC ZZZZZ9.99-.
           03  D-538-CLAIM    PIC ZZZZZZ9.99-.
           03  D-UNEMP-CLAIM  PIC ZZZZZ9.99-.
           03  D-AD-D-CLAIM   PIC ZZZZZ9.99-.
           03  D-470-CLAIM    PIC ZZZZZ9.99-.
           03  D-ROADG-CLAIM  PIC ZZZZZ9.99-.

       01  DETAIL-LINE2 REDEFINES DETAIL-LINE.
           03  D2-BRNO         PIC ZZZ9.
           03  FILLER          PIC X(4).
           03  D2-DATE         PIC 99/99/99.
           03  FILLER          PIC X.
           03  D2-NAME         PIC X(13).
           03  FILLER          PIC X.
           03  D2-CHECK-NO     PIC 9(8).
           03  FILLER          PIC X.
           03  D2-CLAIM        PIC ZZZZZ9.99-.
           03  FILLER          PIC X.
           03  D2-TYPE         PIC 9(3).
           03  FILLER          PIC X(3).
           03  D2-DESC         PIC X(10).
           03  FILLER          PIC X(3).
           03  D2-ERROR-DESC   PIC X(35).
           03  D2-BAD-GL-BR    PIC 9(4).
           03  D2-BAD-GL-DASH  PIC X.
           03  D2-BAD-GL-ACCT  PIC 9(7).

       01  DETAIL-GT-LINE REDEFINES DETAIL-LINE.
           03  D-GT-BRNO      PIC 9(4).
           03  FILLER         PIC X.
           03  D-GT-ACCTNO    PIC 9(7).
           03  FILLER         PIC XX.
           03  D-GT-EXPL      PIC X(32).
           03  FILLER         PIC XX.
           03  D-GT-DRCRFG    PIC X.
           03  FILLER         PIC XX.
           03  D-GT-AMOUNT    PIC ZZZ,ZZZ,ZZ9.99-.
           03  FILLER         PIC X(3).
           03  D-GT-COUNT     PIC Z(6).
           03  FILLER         PIC X(3).
           03  D-GT-ERROR     PIC X(25).

       01  TOTAL-LINE.
           03  FILLER         PIC X(36)   VALUE SPACES.
           03  TOT-TAG        PIC X(14).
           03  TOT-AMT        PIC ZZZZ,ZZZ,ZZ9.99-.
           03  FILLER         PIC XX.

       COPY "LIBGB/SYSTEMW.CPY".

       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBLP/POSTDATEW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/CLAIM2_DEF.CPY".
       COPY "LIBSP/CLAIM2_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/CLAIM2_SCN.CPY".

      *----------------------------------------------------------------
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON 
                   CLAIMS-FILE PR-FILE WK-FILE WK2-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-WR1-FILE.
           PERFORM CLOSE-GT1-FILE.
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE CLAIMS-FILE WK-FILE WK2-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ********************************************
       MAIN-PROGRAM SECTION.
      ********************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CLAIM2" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              MOVE "N" TO CONTINUE-UPDATE
              GO TO END-PROGRAM.

           IF REPORT-ONLY-FG = "Y"
              GO TO SKIP-WREOM1-CHECK.

           PERFORM OPEN-WR1-FILE.
           MOVE PROCESS-YYYYMM     TO WR-YYYYMM
                                      QWR1-WBEG-YYYYMM
                                      QWR1-WEND-YYYYMM.
           MOVE 0                  TO WR-BRNO
                                      QWR1-WBEG-BRNO.
           MOVE ALL "9"            TO QWR1-WEND-BRNO.

           PERFORM START-WR1-FILE.
           PERFORM READ-WR1-FILE-NEXT.
           IF IO-FG = 9 OR (WR-YYYYMM NOT = PROCESS-YYYYMM)
              MOVE SPACES TO MESS
              MOVE CLAIMS-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-DATE
              STRING "CREATE NEW MONTHS RATING SHEET NOT RUN FOR "
                       D-DATE DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              PERFORM CLOSE-WR1-FILE
              MOVE "N" TO CONTINUE-UPDATE
              GO TO END-PROGRAM.

           PERFORM CLOSE-WR1-FILE.

       SKIP-WREOM1-CHECK.
           MOVE 0 TO HOLD-BRANCH.
           MOVE 0 TO CLAIMS-KEY.
           PERFORM START-CLAIMS-FILE.

       NEXT-CLAIM.
           PERFORM READ-CLAIMS-FILE-NEXT.
           IF IO-BAD
              PERFORM BRANCH-TOTALS
              GO TO PROCESS-CLAIMS.

      *    COVERAGE CODES:
      *      LIFE:         001 002 100 101 102 103 104 105
      *                    112 113 168 169 170 171 227 374
      *      A&H:          200 201 203 248 390
      *      FIRE:         511 512 517 522 523
      *      AUTO:         531 537
      *      NONFILE:      520
      *      UNEMP:        545 550 551 554 556
      *      UNEMP "IN":   557
      *      AD&D:         570

           MOVE CLAIM-COVERAGE-CODE TO TEST-COVERAGE-CODE.
           IF (CLAIM-BRNO > 1999 AND < 3000) AND UNEMP-IN-TYPE
              MOVE 454 TO WORLD-CLAIM-TYPE
           ELSE
            IF LIFE-TYPE MOVE 510 TO WORLD-CLAIM-TYPE
             ELSE
              IF AH-TYPE MOVE 511 TO WORLD-CLAIM-TYPE
               ELSE
               IF FIRE-TYPE MOVE 527 TO WORLD-CLAIM-TYPE
                 ELSE
                  IF AUTO-TYPE MOVE 528 TO WORLD-CLAIM-TYPE
                   ELSE
                    IF NONFILE-TYPE MOVE 538 TO WORLD-CLAIM-TYPE
                     ELSE
                      IF UNEMP-TYPE MOVE 454 TO WORLD-CLAIM-TYPE
                       ELSE
                        IF ADD-TYPE MOVE 459 TO WORLD-CLAIM-TYPE
                         ELSE
                          MOVE CLAIM-COVERAGE-CODE TO WORLD-CLAIM-TYPE
                          MOVE "Y" TO WK2-FLAG
                          MOVE " " TO CLAIM-CR CLAIM-LF
                          MOVE CLAIMS-REC TO WK2-CLAIMS-REC
                          MOVE "T" TO WK2-ERROR-FG
                          PERFORM WRITE-WK2-FILE
                          IF IO-FG NOT = 0
                             GO TO IO-ERROR.

           IF CLAIM-BRNO-ALP NOT NUMERIC OR
              CLAIM-AMOUNT NOT NUMERIC 
              MOVE "Y" TO WK2-FLAG
              MOVE " " TO CLAIM-CR CLAIM-LF
              MOVE CLAIMS-REC TO WK2-CLAIMS-REC
              MOVE "N" TO WK2-ERROR-FG
              PERFORM WRITE-WK2-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR
              ELSE
                 GO TO NEXT-CLAIM.

           MOVE CLAIM-BRNO TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-BAD
              MOVE "Y" TO WK2-FLAG
              MOVE " " TO CLAIM-CR CLAIM-LF
              MOVE CLAIMS-REC TO WK2-CLAIMS-REC
              MOVE "I" TO WK2-ERROR-FG
              PERFORM WRITE-WK2-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR
              ELSE
                 GO TO NEXT-CLAIM.

           IF WORLD-CLAIM-TYPE = 459 OR 536 OR 538
              GO TO SKIP-VERIFY-GL.

           IF WORLD-CLAIM-TYPE = 510
              MOVE 5136 TO ACCT-FIRST-4
           ELSE
           IF WORLD-CLAIM-TYPE = 511
              MOVE 5136 TO ACCT-FIRST-4
           ELSE
           IF WORLD-CLAIM-TYPE = 527
              MOVE 5146 TO ACCT-FIRST-4
           ELSE
           IF WORLD-CLAIM-TYPE = 528
              MOVE 5146 TO ACCT-FIRST-4
           ELSE
           IF WORLD-CLAIM-TYPE = 454
              MOVE 5156 TO ACCT-FIRST-4
           ELSE
           IF WORLD-CLAIM-TYPE = 470
              MOVE 5148 TO ACCT-FIRST-4
           ELSE
           IF WORLD-CLAIM-TYPE = 463
              MOVE 5134 TO ACCT-FIRST-4.

           MOVE CLAIM-BRNO TO GL-BRANCH.
           MOVE 0 TO ACCT-LAST-3.
           MOVE FORMAT-ACCT TO GL-ACCOUNT.

           PERFORM READ-GL1-FILE.
           IF IO-BAD
              MOVE "Y" TO WK2-FLAG
              MOVE " " TO CLAIM-CR CLAIM-LF
              MOVE CLAIMS-REC TO WK2-CLAIMS-REC
              MOVE "G" TO WK2-ERROR-FG
              MOVE GL-BRANCH TO WK2-BAD-GL-BRANCH
              MOVE GL-ACCOUNT TO WK2-BAD-GL-ACCOUNT
              PERFORM WRITE-WK2-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR
              ELSE
                 GO TO NEXT-CLAIM.

           IF WK-TYPE = 510
              MOVE 3500001 TO FORMAT-ACCT
           ELSE
           IF WK-TYPE = 511
              MOVE 3500001 TO FORMAT-ACCT
           ELSE
           IF WK-TYPE = 527
              MOVE 3502001 TO FORMAT-ACCT
           ELSE
           IF WK-TYPE = 528
              MOVE 3502001 TO FORMAT-ACCT
           ELSE
           IF WK-TYPE = 454
              MOVE 3503001 TO FORMAT-ACCT
           ELSE
           IF WK-TYPE = 470
              MOVE 3502001 TO FORMAT-ACCT
           ELSE
           IF WK-TYPE = 463
              MOVE 3501001 TO FORMAT-ACCT.

           MOVE 9990 TO GL-BRANCH.
           MOVE FORMAT-ACCT TO GL-ACCOUNT.

           PERFORM READ-GL1-FILE.
           IF IO-BAD
              MOVE "Y" TO WK2-FLAG
              MOVE " " TO CLAIM-CR CLAIM-LF
              MOVE CLAIMS-REC TO WK2-CLAIMS-REC
              MOVE "G" TO WK2-ERROR-FG
              MOVE GL-BRANCH TO WK2-BAD-GL-BRANCH
              MOVE GL-ACCOUNT TO WK2-BAD-GL-ACCOUNT
              PERFORM WRITE-WK2-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR
              ELSE
                 GO TO NEXT-CLAIM.

      *    IF WORLD-CLAIM-TYPE = 538
      *       IF CLAIM-AMOUNT < 0
      *          GO TO NEXT-CLAIM.

       SKIP-VERIFY-GL.
           IF HOLD-BRANCH = 0
              MOVE CLAIM-BRNO TO HOLD-BRANCH
           ELSE
              IF HOLD-BRANCH NOT = CLAIM-BRNO
                 PERFORM BRANCH-TOTALS
                 MOVE CLAIM-BRNO TO HOLD-BRANCH.

           MOVE CLAIM-CHECK-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-DATE.

           STRING CLAIM-LNAME,", ",CLAIM-FNAME DELIMITED BY "   "
              INTO D-NAME.
           MOVE CLAIM-CHECK-NO TO D-CHECK-NO.

           IF WORLD-CLAIM-TYPE = 510
              ADD CLAIM-AMOUNT TO BRANCH-510-TOTAL
              MOVE CLAIM-AMOUNT TO D-CL-CLAIM
           ELSE
           IF WORLD-CLAIM-TYPE = 511
              ADD CLAIM-AMOUNT TO BRANCH-511-TOTAL
              MOVE CLAIM-AMOUNT TO D-AH-CLAIM
           ELSE
           IF WORLD-CLAIM-TYPE = 527
              ADD CLAIM-AMOUNT TO BRANCH-527-TOTAL
              MOVE CLAIM-AMOUNT TO D-PP-CLAIM
           ELSE
           IF WORLD-CLAIM-TYPE = 528
              ADD CLAIM-AMOUNT TO BRANCH-528-TOTAL
              MOVE CLAIM-AMOUNT TO D-AUTO-CLAIM
           ELSE
           IF WORLD-CLAIM-TYPE = 536
              ADD CLAIM-AMOUNT TO BRANCH-536-TOTAL
              MOVE CLAIM-AMOUNT TO D-536-CLAIM
           ELSE
           IF WORLD-CLAIM-TYPE = 538
              ADD CLAIM-AMOUNT TO BRANCH-538-TOTAL
              MOVE CLAIM-AMOUNT TO D-538-CLAIM
           ELSE
           IF WORLD-CLAIM-TYPE = 454
              ADD CLAIM-AMOUNT TO BRANCH-454-TOTAL
              MOVE CLAIM-AMOUNT TO D-UNEMP-CLAIM
           ELSE
           IF WORLD-CLAIM-TYPE = 459
              ADD CLAIM-AMOUNT TO BRANCH-459-TOTAL
              MOVE CLAIM-AMOUNT TO D-AD-D-CLAIM
           ELSE
           IF WORLD-CLAIM-TYPE = 470
              ADD CLAIM-AMOUNT TO BRANCH-470-TOTAL
              MOVE CLAIM-AMOUNT TO D-470-CLAIM
           ELSE
           IF WORLD-CLAIM-TYPE = 463
              ADD CLAIM-AMOUNT TO BRANCH-463-TOTAL
              MOVE CLAIM-AMOUNT TO D-ROADG-CLAIM
           ELSE
              GO TO NEXT-CLAIM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE CLAIM-BRNO TO WK-BRNO.
           MOVE WORLD-CLAIM-TYPE TO WK-TYPE.
           PERFORM READ-WK-FILE.
           IF IO-FG NOT = 0
              MOVE CLAIM-BRNO TO WK-BRNO
              MOVE WORLD-CLAIM-TYPE TO WK-TYPE
              IF CLAIM-AMOUNT < 0
                 MOVE -1 TO WK-COUNT
              ELSE
                 MOVE 1 TO WK-COUNT
              END-IF
              MOVE CLAIM-AMOUNT TO WK-AMOUNT
              PERFORM WRITE-WK-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR
              END-IF
           ELSE
              IF CLAIM-AMOUNT < 0
                 SUBTRACT 1 FROM WK-COUNT
              ELSE
                 ADD 1 TO WK-COUNT
              END-IF
              ADD CLAIM-AMOUNT TO WK-AMOUNT
              PERFORM REWRITE-WK-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR.

           GO TO NEXT-CLAIM.

       PROCESS-CLAIMS.
           PERFORM CLOSE-CLAIMS-FILE.
           PERFORM CLOSE-WK-FILE.
           PERFORM CLOSE-WK2-FILE.

           MOVE SPACES TO DETAIL-LINE.
           MOVE "GRAND TOTALS" TO D-NAME.

           MOVE GRAND-510-TOTAL TO D-CL-CLAIM.
           MOVE GRAND-511-TOTAL TO D-AH-CLAIM.
           MOVE GRAND-527-TOTAL TO D-PP-CLAIM.
           MOVE GRAND-528-TOTAL TO D-AUTO-CLAIM.
           MOVE GRAND-536-TOTAL TO D-536-CLAIM.
           MOVE GRAND-538-TOTAL TO D-538-CLAIM.
           MOVE GRAND-454-TOTAL TO D-UNEMP-CLAIM.
           MOVE GRAND-459-TOTAL TO D-AD-D-CLAIM.
           MOVE GRAND-463-TOTAL TO D-ROADG-CLAIM.
           MOVE GRAND-470-TOTAL TO D-470-CLAIM.

           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE HEAD-GRAND TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE HEAD-GT TO DETAIL-LINE.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 2 TO LN-FEED.

           PERFORM OPEN-WK-FILE.
           PERFORM OPEN-GT1-FILE.
           PERFORM OPEN-WR1-FILE.

           MOVE "Y" TO FIRST-WK.
           MOVE 0 TO WK-BRNO WK-TYPE.
           PERFORM START-WK-FILE.

       NEXT-WK-CLAIM.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG NOT = 0
              GO TO END-ROUTINE.

           MOVE 1 TO SAVE-GT-LINE.

           PERFORM CLEAR-GT-FILE.

           MOVE SPACES TO DETAIL-LINE.

           MOVE TRANS-DATE      TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYMMDD.
           MOVE DATE-YYMMDD     TO REF-DATE.

           MOVE WK-BRNO TO REF-BR.
      * ERRORS - NEED TO GO TO BRANCH 9990 (MISSING PRODUCER #)
           IF WK-BRNO = 0
              MOVE 9990 TO REF-BR.
           MOVE REF-WK TO GT-REF.

           IF WK-TYPE = 510
              MOVE "(510) LIFE INSURANCE CLAIM" TO GT-EXPL D-GT-EXPL
              MOVE 5136 TO ACCT-FIRST-4
           ELSE
           IF WK-TYPE = 511
              MOVE "(511) A&H INSURANCE CLAIM" TO GT-EXPL D-GT-EXPL
              MOVE 5136 TO ACCT-FIRST-4
           ELSE
           IF WK-TYPE = 527
              MOVE "(527) FIRE INSURANCE CLAIM" TO GT-EXPL D-GT-EXPL
              MOVE 5146 TO ACCT-FIRST-4
           ELSE
           IF WK-TYPE = 528
              MOVE "(528) AUTO INSURANCE CLAIM" TO GT-EXPL D-GT-EXPL
              MOVE 5146 TO ACCT-FIRST-4
           ELSE
           IF WK-TYPE = 536
              MOVE "(536) BANKRUPT NONFILE INS CLAIM" TO GT-EXPL
                                                         D-GT-EXPL
              MOVE 0 TO ACCT-FIRST-4
           ELSE
           IF WK-TYPE = 538
              MOVE "(538) NONFILE INSURANCE CLAIM" TO GT-EXPL D-GT-EXPL
              MOVE 0 TO ACCT-FIRST-4
           ELSE
           IF WK-TYPE = 454
              MOVE "(454) UNEMPLOYMENT INS CLAIM" TO GT-EXPL D-GT-EXPL
              MOVE 5156 TO ACCT-FIRST-4
           ELSE
           IF WK-TYPE = 459
              MOVE "(459) AD&D INSURANCE CLAIM" TO GT-EXPL D-GT-EXPL
              MOVE 0 TO ACCT-FIRST-4
           ELSE
           IF WK-TYPE = 470
              MOVE 5148 TO ACCT-FIRST-4
              MOVE "(470) L. PROT INSURANCE CLAIM" TO GT-EXPL D-GT-EXPL
           ELSE
           IF WK-TYPE = 463
              MOVE 5134 TO ACCT-FIRST-4
              MOVE "(463) ROADGARD INSURANCE CLAIM" TO GT-EXPL
                                                       D-GT-EXPL.

      * AD&D DOES NOT MAKE JOURNAL ENTRIES, ASSURANT PAYS THE CLAIMS
      * BUT STILL NEED TO UPDATE RATING SHEET
      * SKIP NON-FILE TOO?  AT LEAST FOR NOW, DON'T KNOW GL # BLV
           IF WK-TYPE = 459 OR 536 OR 538
              GO TO UPDATE-SKIP-GT.

           MOVE TRANS-DATE TO GT-DATE1 GT-DATE2 GT-DATE3.

           MOVE 3 TO GT-SOURCE.
           MOVE WK-BRNO TO GT-BRANCH GL-BRANCH D-GT-BRNO.
           MOVE 0 TO ACCT-LAST-3.
           MOVE FORMAT-ACCT TO GT-ACCTNO GL-ACCOUNT D-GT-ACCTNO.
           IF WK-BRNO = 0
              MOVE 9990 TO GT-BRANCH GL-BRANCH D-GT-BRNO
              MOVE 9999999 TO GT-ACCTNO GL-ACCOUNT D-GT-ACCTNO.

           PERFORM READ-GL1-FILE.
           IF IO-BAD
              MOVE "Y" TO ERROR-FLAG
              MOVE "** GL MASTER MISSING **" TO D-GT-ERROR.

           MOVE WK-AMOUNT TO D-GT-AMOUNT GT-AMT.

           ADD WK-AMOUNT TO ACCUM-TOT-DEBIT.
           IF GT-AMT > 0
              MOVE "1" TO GT-DRCRFG
              MOVE "D" TO D-GT-DRCRFG
           ELSE
              MOVE "2" TO GT-DRCRFG
              MOVE "C" TO D-GT-DRCRFG.

           MOVE WK-COUNT TO D-GT-COUNT.
           MOVE 0        TO GT-PERIOD.

           IF REPORT-ONLY-FG = "Y" OR GT-AMT = 0
              GO TO WRITE-REPORT-LINE.

       UPDATE-NEXT-GT1.
           PERFORM SET-POSTDATE.
           MOVE POST-DATE    TO GT-POST-DATE.
           MOVE POST-TIME    TO WORK-TIME.
           MOVE TIME-HHMM    TO GT-POST-TIME.
           MOVE POST-USERID  TO GT-POST-USERID.
           MOVE SAVE-GT-LINE TO GT-LINE.

           PERFORM WRITE-GT1-FILE.
           IF IO-BAD
              IF GT-LINE < 999999
                 ADD 1 TO SAVE-GT-LINE
                 GO TO UPDATE-NEXT-GT1.


       UPDATE-SKIP-GT.

           PERFORM UPDATE-RATING-SHEET.
           IF WK-TYPE = 459 OR 536 OR 538
              GO TO NEXT-WK-CLAIM.

       WRITE-REPORT-LINE.

           PERFORM WRITE-DETAIL-LINE.

      ******
           PERFORM UPDATE-GT-CREDIT.
      ******

           GO TO NEXT-WK-CLAIM.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM.

           MOVE "TOTAL DEBIT :" TO TOT-TAG.
           MOVE ACCUM-TOT-DEBIT  TO TOT-AMT.

           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE TOTAL-LINE TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE "TOTAL CREDIT:" TO TOT-TAG.
           MOVE ACCUM-TOT-CREDIT TO TOT-AMT.
           MOVE TOTAL-LINE TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           IF REPORT-ONLY-FG = "Y"
              MOVE "* * * * * * * * * * * * * * * * * * * " TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC
              MOVE "* * * * * REPORT ONLY OPTION * * * * *" TO PR-REC
              PERFORM WRITE-PR-REC
              MOVE "* * * * * * * * * * * * * * * * * * * " TO PR-REC
              PERFORM WRITE-PR-REC.

           IF WK2-FLAG = "Y"
              PERFORM PRINT-INVALID-RECORDS.

           MOVE SPACES TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           GO TO END-PROGRAM.


      ******************************
       PRINT-INVALID-RECORDS SECTION.
      ******************************

           MOVE SPACES TO DETAIL-LINE.
           MOVE "INVALID ACCOUNTS:" TO DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.
           PERFORM OPEN-WK2-FILE.

       PRINT-NEXT-INVALID.
           PERFORM READ-WK2-FILE-NEXT.
           IF IO-FG NOT = 0
              GO TO EXIT-PRINT-INVALID.

           MOVE WK2-CLAIMS-REC TO CLAIMS-REC.

           MOVE CLAIM-CHECK-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D2-DATE.

           STRING CLAIM-LNAME,", ",CLAIM-FNAME DELIMITED BY "   "
              INTO D2-NAME.
           MOVE CLAIM-CHECK-NO TO D2-CHECK-NO.

           MOVE CLAIM-BRNO TO D2-BRNO.

           MOVE CLAIM-AMOUNT TO D2-CLAIM.

           MOVE CLAIM-COVERAGE-CODE TO TEST-COVERAGE-CODE.
           IF LIFE-TYPE MOVE 510 TO WORLD-CLAIM-TYPE
            ELSE
             IF AH-TYPE MOVE 511 TO WORLD-CLAIM-TYPE
              ELSE
              IF FIRE-TYPE MOVE 527 TO WORLD-CLAIM-TYPE
                ELSE
                 IF AUTO-TYPE MOVE 528 TO WORLD-CLAIM-TYPE
                  ELSE
                   IF NONFILE-TYPE MOVE 538 TO WORLD-CLAIM-TYPE
                    ELSE
                     IF UNEMP-TYPE MOVE 454 TO WORLD-CLAIM-TYPE
                      ELSE
                       IF ADD-TYPE MOVE 459 TO WORLD-CLAIM-TYPE
                        ELSE
                         MOVE CLAIM-COVERAGE-CODE TO WORLD-CLAIM-TYPE.
           MOVE WORLD-CLAIM-TYPE TO D2-TYPE.

           IF WORLD-CLAIM-TYPE = 510
              MOVE "LIFE" TO D2-DESC
           ELSE
           IF WORLD-CLAIM-TYPE = 511
              MOVE "A&H" TO D2-DESC
           ELSE
           IF WORLD-CLAIM-TYPE = 527
              MOVE "FIRE" TO D2-DESC
           ELSE
           IF WORLD-CLAIM-TYPE = 528
              MOVE "AUTO" TO D2-DESC
           ELSE
           IF WORLD-CLAIM-TYPE = 536
              MOVE "BK NONFILE" TO D2-DESC
           ELSE
           IF WORLD-CLAIM-TYPE = 538
              MOVE "NONFILE" TO D2-DESC
           ELSE
           IF WORLD-CLAIM-TYPE = 454
              MOVE "UNEMP" TO D2-DESC
           ELSE
           IF WORLD-CLAIM-TYPE = 459
              MOVE "AD&D" TO D2-DESC
           ELSE
           IF WORLD-CLAIM-TYPE = 470
              MOVE "L.PROT" TO D2-DESC
           ELSE
           IF WORLD-CLAIM-TYPE = 463
              MOVE "ROADGARD" TO D2-DESC
           ELSE
              MOVE "INVALID" TO D2-DESC.

           IF WK2-ERROR-FG = "T"
              MOVE "INVALID CLAIMS TYPE" TO D2-ERROR-DESC
           ELSE IF WK2-ERROR-FG = "N"
              MOVE "NON-NUMERIC BRANCH NO" TO D2-ERROR-DESC
           ELSE IF WK2-ERROR-FG = "I"
              MOVE "INVALID BRANCH NO" TO D2-ERROR-DESC
           ELSE IF WK2-ERROR-FG = "Z"
              MOVE "ZERO BRANCH NO" TO D2-ERROR-DESC
           ELSE IF WK2-ERROR-FG = "G"
              MOVE "INVALID GL ACCOUNT:" TO D2-ERROR-DESC
              MOVE WK2-BAD-GL-BRANCH TO D2-BAD-GL-BR
              MOVE "-" TO D2-BAD-GL-DASH
              MOVE WK2-BAD-GL-ACCOUNT TO D2-BAD-GL-ACCT.

           PERFORM WRITE-DETAIL-LINE.

           GO TO PRINT-NEXT-INVALID.


       EXIT-PRINT-INVALID.
           EXIT.

      *****************************
       UPDATE-GT-CREDIT SECTION.
      *****************************

           MOVE 1 TO SAVE-GT-LINE.

           PERFORM CLEAR-GT-FILE.

           MOVE TRANS-DATE   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYMMDD.
           MOVE DATE-YYMMDD  TO REF-DATE.

           MOVE WK-BRNO TO REF-BR.
           IF WK-BRNO = 0
              MOVE 9990 TO REF-BR.
           MOVE REF-WK TO GT-REF.

           IF WK-TYPE = 510
              MOVE "(510) LIFE INSURANCE CLAIM" TO GT-EXPL D-GT-EXPL
              MOVE 3500001 TO FORMAT-ACCT
           ELSE
           IF WK-TYPE = 511
              MOVE "(511) A&H INSURANCE CLAIM" TO GT-EXPL D-GT-EXPL
              MOVE 3500001 TO FORMAT-ACCT
           ELSE
           IF WK-TYPE = 527
              MOVE "(527) FIRE INSURANCE CLAIM" TO GT-EXPL D-GT-EXPL
              MOVE 3502001 TO FORMAT-ACCT
           ELSE
           IF WK-TYPE = 528
              MOVE "(528) AUTO INSURANCE CLAIM" TO GT-EXPL D-GT-EXPL
              MOVE 3502001 TO FORMAT-ACCT
           ELSE
           IF WK-TYPE = 536
              MOVE "(536) BANKRUPT NONFILE INS CLAIM" TO GT-EXPL
                                                         D-GT-EXPL
              MOVE 0 TO FORMAT-ACCT
           ELSE
           IF WK-TYPE = 538
              MOVE "(538) NONFILE INSURANCE CLAIM" TO GT-EXPL D-GT-EXPL
              MOVE 0 TO FORMAT-ACCT
           ELSE
           IF WK-TYPE = 454
              MOVE "(454) UNEMPLOYMENT INS CLAIM" TO GT-EXPL D-GT-EXPL
              MOVE 3503001 TO FORMAT-ACCT
           ELSE
           IF WK-TYPE = 459
              MOVE "(459) AD&D INSURANCE CLAIM" TO GT-EXPL D-GT-EXPL
              MOVE 0 TO FORMAT-ACCT
           ELSE
           IF WK-TYPE = 470
              MOVE "(470) L. PROT INSURANCE CLAIM" TO GT-EXPL D-GT-EXPL
              MOVE 3502001 TO FORMAT-ACCT
           ELSE
           IF WK-TYPE = 463
              MOVE 3501001 TO FORMAT-ACCT
              MOVE "(463) ROADGARD INSURANCE CLAIM" TO GT-EXPL
                                                     D-GT-EXPL.

           MOVE TRANS-DATE TO GT-DATE1 GT-DATE2 GT-DATE3.

           MOVE 3 TO GT-SOURCE.
           MOVE 9990 TO GT-BRANCH GL-BRANCH D-GT-BRNO.
           MOVE 0 TO GT-PERIOD.
           MOVE FORMAT-ACCT TO GT-ACCTNO GL-ACCOUNT D-GT-ACCTNO.

           PERFORM READ-GL1-FILE.
           IF IO-BAD
              MOVE "Y" TO ERROR-FLAG
              MOVE "** GL MASTER MISSING **" TO D-GT-ERROR.

           MOVE WK-AMOUNT TO D-GT-AMOUNT.
           COMPUTE GT-AMT = WK-AMOUNT * -1.
           IF GT-AMT > 0
              MOVE "1" TO GT-DRCRFG
              MOVE "D" TO D-GT-DRCRFG
           ELSE
              MOVE "2" TO GT-DRCRFG
              MOVE "C" TO D-GT-DRCRFG.


      * AD&D DOES NOT MAKE JOURNAL ENTRIES, ASSURANT PAYS THE CLAIMS
      * BUT STILL NEED TO UPDATE RATING SHEET
      * SKIP NON-FILE TOO?  AT LEAST FOR NOW, DON'T KNOW GL # BLV
           IF WK-TYPE = 459 OR 536 OR 538
              GO TO EXIT-UPDATE-GT-CREDIT.

           ADD WK-AMOUNT TO ACCUM-TOT-CREDIT.

           IF REPORT-ONLY-FG = "Y" OR GT-AMT = 0
              GO TO SKIP-UPDATE-GT-CREDIT.

       UPDATE-NEXT-CREDIT.
           PERFORM SET-POSTDATE.
           MOVE POST-DATE    TO GT-POST-DATE.
           MOVE POST-TIME    TO WORK-TIME.
           MOVE TIME-HHMM    TO GT-POST-TIME.
           MOVE POST-USERID  TO GT-POST-USERID.
           MOVE SAVE-GT-LINE TO GT-LINE.

           PERFORM WRITE-GT1-FILE.
           IF IO-BAD
              IF GT-LINE < 999999
                 ADD 1 TO SAVE-GT-LINE
                 GO TO UPDATE-NEXT-CREDIT.

       SKIP-UPDATE-GT-CREDIT.
           PERFORM WRITE-DETAIL-LINE.

       EXIT-UPDATE-GT-CREDIT.
           EXIT.

      ********************************
       UPDATE-RATING-SHEET SECTION.
      ********************************
       UPDATE-RATING.
           IF REPORT-ONLY-FG = "Y"
              GO TO EXIT-UPDATE-RATING-SHEET.
           MOVE WK-BRNO TO WR-BRNO.
           IF WK-BRNO = 0
              MOVE 9990 TO WR-BRNO.
           MOVE PROCESS-YYYYMM TO WR-YYYYMM.

           PERFORM READ-WR1-FILE.
           IF IO-FG NOT = 0
              PERFORM CLEAR-WR-FILE
              MOVE WK-BRNO TO WR-BRNO
              IF WK-BRNO = 0
                 MOVE 9990 TO WR-BRNO
              END-IF
              MOVE PROCESS-YYYYMM TO WR-YYYYMM
              PERFORM WRITE-WR1-FILE
              IF IO-FG NOT = 0
                 GO TO EXIT-UPDATE-RATING-SHEET
              ELSE
                 GO TO UPDATE-RATING.

           PERFORM RETRIEVE-LAST-MONTH-WR.

           IF WK-TYPE = 510
              ADD WK-COUNT TO WR-NO-LIFE-CLAIMS(1)
              ADD WK-AMOUNT TO WR-LIFE-CLAIMS(1)
              ADD WK-COUNT TO WR-NO-LIFE-CLAIMS(2)
              ADD WK-AMOUNT TO WR-LIFE-CLAIMS(2)
           ELSE
           IF WK-TYPE = 511
              ADD WK-COUNT TO WR-NO-AH-CLAIMS(1)
              ADD WK-AMOUNT TO WR-AH-CLAIMS(1)
              ADD WK-COUNT TO WR-NO-AH-CLAIMS(2)
              ADD WK-AMOUNT TO WR-AH-CLAIMS(2)
           ELSE
           IF WK-TYPE = 527
              ADD WK-COUNT TO WR-NO-FIRE-CLAIMS(1)
              ADD WK-AMOUNT TO WR-FIRE-CLAIMS(1)
              ADD WK-COUNT TO WR-NO-FIRE-CLAIMS(2)
              ADD WK-AMOUNT TO WR-FIRE-CLAIMS(2)
           ELSE
           IF WK-TYPE = 528
              ADD WK-COUNT TO WR-NO-AUTO-CLAIMS(1)
              ADD WK-AMOUNT TO WR-AUTO-CLAIMS(1)
              ADD WK-COUNT TO WR-NO-AUTO-CLAIMS(2)
              ADD WK-AMOUNT TO WR-AUTO-CLAIMS(2)
           ELSE
           IF WK-TYPE = 536
              ADD WK-COUNT TO WR-NO-NF-BK-CLAIMS(1)
              ADD WK-AMOUNT TO WR-NF-BK-CLAIMS(1)
              ADD WK-COUNT TO WR-NO-NF-BK-CLAIMS(2)
              ADD WK-AMOUNT TO WR-NF-BK-CLAIMS(2)
              IF WR-ELM-OUTS-BALANCE NOT = 0
                 COMPUTE WS-MTD-PERCENT ROUNDED =
                 100 * WR-NF-BK-CLAIMS(1) / WR-ELM-OUTS-BALANCE
              ELSE
                 MOVE 0 TO WS-MTD-PERCENT
              END-IF
              ADD WS-MTD-PERCENT TO WR-YTD-PCT-NF-BNK-LEDGER
           ELSE
           IF WK-TYPE = 538
              ADD WK-COUNT TO WR-LOTS-NO-NF-CLAIMS(1)
              ADD WK-AMOUNT TO WR-LOTS-NF-CLAIMS(1)
              ADD WK-COUNT TO WR-LOTS-NO-NF-CLAIMS(2)
              ADD WK-AMOUNT TO WR-LOTS-NF-CLAIMS(2)
           ELSE
           IF WK-TYPE = 454
              ADD WK-COUNT TO WR-NO-UNEMP-CLAIMS(1)
              ADD WK-AMOUNT TO WR-UNEMP-CLAIMS(1)
              ADD WK-COUNT TO WR-NO-UNEMP-CLAIMS(2)
              ADD WK-AMOUNT TO WR-UNEMP-CLAIMS(2)
           ELSE
           IF WK-TYPE = 459
              ADD WK-COUNT TO WR-NO-AD-D-CLAIMS(1)
              ADD WK-AMOUNT TO WR-AD-D-CLAIMS(1)
              ADD WK-COUNT TO WR-NO-AD-D-CLAIMS(2)
              ADD WK-AMOUNT TO WR-AD-D-CLAIMS(2)
           ELSE
           IF WK-TYPE = 463
              ADD WK-COUNT TO WR-NO-ROAD-GARD-CLAIMS(1)
              ADD WK-AMOUNT TO WR-ROAD-GARD-CLAIMS(1)
              ADD WK-COUNT TO WR-NO-ROAD-GARD-CLAIMS(2)
              ADD WK-AMOUNT TO WR-ROAD-GARD-CLAIMS(2)
           ELSE
           IF WK-TYPE = 470
              ADD WK-COUNT TO WR-NO-L-PROT-CLAIMS(1)
              ADD WK-AMOUNT TO WR-L-PROT-CLAIMS(1)
              ADD WK-COUNT TO WR-NO-L-PROT-CLAIMS(2)
              ADD WK-AMOUNT TO WR-L-PROT-CLAIMS(2).
           PERFORM REWRITE-WR1-FILE.

       EXIT-UPDATE-RATING-SHEET.
           EXIT.

      *****************************************************************
      *  READ LAST MONTHS RATING SHEET, STORE FIELDS IN WRLM-REC.
      *  RESTORE CURRENT MONTH RECORD & UPDATE END OF LAST MONTH
      *  VALUES WITH FIELDS FROM WRLM-REC
      *****************************************************************
       RETRIEVE-LAST-MONTH-WR SECTION.

           MOVE WK-BRNO TO WR-BRNO.
           IF WK-BRNO = 0
              MOVE 9990 TO WR-BRNO.
           MOVE LAST-MO-YYYYMM TO WR-YYYYMM.
           PERFORM READ-WR1-FILE.
           IF IO-BAD
              PERFORM CLEAR-WR-FILE.

           MOVE WR-REC TO WRLM-REC.

           MOVE WK-BRNO TO WR-BRNO.
           IF WK-BRNO = 0
              MOVE 9990 TO WR-BRNO.
           MOVE PROCESS-YYYYMM TO WR-YYYYMM.
           PERFORM READ-WR1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

       EXIT-RETRIEVE-LAST-MO.
           EXIT.

      ************************
       BRANCH-TOTALS SECTION.
      ************************

           MOVE SPACES TO DETAIL-LINE.
           MOVE "BRANCH TOTALS" TO D-NAME.

           MOVE BRANCH-510-TOTAL TO D-CL-CLAIM.
           MOVE BRANCH-511-TOTAL TO D-AH-CLAIM.
           MOVE BRANCH-527-TOTAL TO D-PP-CLAIM.
           MOVE BRANCH-528-TOTAL TO D-AUTO-CLAIM.
           MOVE BRANCH-536-TOTAL TO D-536-CLAIM.
           MOVE BRANCH-538-TOTAL TO D-538-CLAIM.
           MOVE BRANCH-454-TOTAL TO D-UNEMP-CLAIM.
           MOVE BRANCH-459-TOTAL TO D-AD-D-CLAIM.
           MOVE BRANCH-463-TOTAL TO D-ROADG-CLAIM.
           MOVE BRANCH-470-TOTAL TO D-470-CLAIM.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 99 TO LN-FEED.

           ADD BRANCH-510-TOTAL TO GRAND-510-TOTAL.
           ADD BRANCH-511-TOTAL TO GRAND-511-TOTAL.
           ADD BRANCH-527-TOTAL TO GRAND-527-TOTAL.
           ADD BRANCH-528-TOTAL TO GRAND-528-TOTAL.
           ADD BRANCH-536-TOTAL TO GRAND-536-TOTAL.
           ADD BRANCH-538-TOTAL TO GRAND-538-TOTAL.
           ADD BRANCH-454-TOTAL TO GRAND-454-TOTAL.
           ADD BRANCH-459-TOTAL TO GRAND-459-TOTAL.
           ADD BRANCH-463-TOTAL TO GRAND-463-TOTAL.
           ADD BRANCH-470-TOTAL TO GRAND-470-TOTAL.


           MOVE 0 TO  BRANCH-510-TOTAL.
           MOVE 0 TO  BRANCH-511-TOTAL.
           MOVE 0 TO  BRANCH-527-TOTAL.
           MOVE 0 TO  BRANCH-528-TOTAL.
           MOVE 0 TO  BRANCH-536-TOTAL.
           MOVE 0 TO  BRANCH-538-TOTAL.
           MOVE 0 TO  BRANCH-454-TOTAL.
           MOVE 0 TO  BRANCH-459-TOTAL.
           MOVE 0 TO  BRANCH-463-TOTAL.
           MOVE 0 TO  BRANCH-470-TOTAL.

      *******************
       HEAD-PAGE SECTION.
      *******************
           MOVE "N" TO FIRST-TIME.
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO HD-PAGE.
           PERFORM GET-TIME.
           MOVE TIME-EDIT TO HD-TIME.

           MOVE HEAD-1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD-2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE CLAIM-BRNO TO HD-BRNO.
           MOVE HEAD-3 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD-4 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE HEAD-5 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE 6 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      ****************************
       WRITE-DETAIL-LINE SECTION.
      ****************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ****************************
       INITIALIZATION SECTION.
      ****************************
           MOVE CLAIM2-QUEUE-POS TO Q-POS.
           MOVE CLAIM2-COPIES-POS TO C-POS.
           MOVE CLAIM2-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GL1-FILE.
           PERFORM OPEN-CLAIMS-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WK2PATH.
           PERFORM OPEN-WK2-FILE-OUTPUT.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           IF REPORT-ONLY-FG = "Y"
              MOVE "REPORT IN PROGRESS..............." TO WR-BUF
           ELSE
              MOVE "UPDATE IN PROGRESS..............." TO WR-BUF.
           IF EXT-ROUTE-BUF = "00"
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           IF EXT-ROUTE-BUF = "00"
              IF REPORT-ONLY-FG = "N"
                 PERFORM CREATE-SPOOL-DIR
                 IF STAT NOT = "00"
                    MOVE "CANNOT CREATE SPOOL DIRECTORY" TO MESS
                    PERFORM SEND-MESS
                    GO TO ENDIT
                 END-IF
                 MOVE EO-SPOOL TO PRU-FILE.

           PERFORM OPEN-PR-FILE.

           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO HD-TRANS-DATE.

           MOVE CLAIMS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM TO PROCESS-YYYYMM.

           MOVE PROCESS-YYYYMM TO LAST-MO-YYYYMM.
           IF LAST-MO-MM > 1
              SUBTRACT 1 FROM LAST-MO-MM
           ELSE
              SUBTRACT 1 FROM LAST-MO-YYYY
              MOVE 12 TO LAST-MO-MM.

           MOVE EXT-CONAME-CONAME TO HD-CONAME.
           MOVE EXT-CONAME-SYSDATE TO HD-DATE.
           MOVE EXT-CONAME-USER-ID TO HD-USERID.

           IF REPORT-ONLY-FG NOT = "Y"
              MOVE CLAIMS-DATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY                 TO LOG-DATE
              MOVE "START LOS CLAIMS UPDATE  " TO LOG-MESS1
              MOVE SPACES                      TO LOG-BRNOX
              PERFORM LOG-CHANGES.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ****************************
       EO-EXEC-AUDIT SECTION.
      ****************************
           EXIT.

      ****************************
       ENTER-PARAMETERS SECTION.
      ****************************

           IF UP-KEY
              GO TO ENTER-REPORT-ONLY-YN.

        ENTER-CLAIMS-DATE.
           MOVE "ERNM060CLAIMDATE." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF F7-KEY
              MOVE "N" TO CONTINUE-UPDATE
              GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO ENTER-CLAIMS-DATE.
           MOVE VDU-DATE-YYYYMMDD TO CLAIMS-DATE.

       VERIFY-CLAIMS-PATH.
           MOVE EXT-FILPATH-BASE     TO CLAIMS-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE  TO CLAIMS-PATH-MACHINE.
           MOVE CLAIMS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYMMDD.
           MOVE DATE-YYMMDD          TO CLAIMS-PATH-DATE.
           MOVE CLAIMS-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT-BAD
              MOVE "CLAIMS INPUT FILE DOES NOT EXIST" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-CLAIMS-DATE.

           MOVE CLAIMS-PATH TO CLAIMS-PATH-UPD.
           MOVE CLAIMS-UPD-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT-GOOD
              MOVE "STOP!! CLAIMS DATE ALREADY UPDATED!!!!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-CLAIMS-DATE.

           MOVE CLAIMS-PATH TO CLAIMS-PATH-UPDTR.
           MOVE CLAIMS-UPDTR-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT-GOOD
              MOVE "STOP!! CLAIMS DATE ALREADY UPDATED!!!!" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-CLAIMS-DATE.

      *-----------------------------------------------------------------

        ENTER-TRANS-DATE.
           MOVE "ERNM060TRANSDATE." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY GO TO ENTER-CLAIMS-DATE.
           IF F1-KEY GO TO EXIT-PARM.
           IF F7-KEY
              MOVE "N" TO CONTINUE-UPDATE
              GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO ENTER-TRANS-DATE.

           MOVE VDU-DATE-YYYYMMDD TO TRANS-DATE.
           IF TRANS-DATE = 0
              GO TO ENTER-TRANS-DATE.

       ENTER-REPORT-ONLY-YN.
           MOVE "E  A010REPORTONLYFG." TO VDU.
           PERFORM SCREENIO.
           IF UP-KEY GO TO ENTER-TRANS-DATE.
           IF F1-KEY GO TO EXIT-PARM.
           IF F7-KEY
              MOVE "N" TO CONTINUE-UPDATE
              GO TO EXIT-PARM.
           IF NOT ENTER-KEY
              GO TO ENTER-REPORT-ONLY-YN.
           MOVE VDUBUF TO REPORT-ONLY-FG.
           IF REPORT-ONLY-FG NOT = "Y"
              IF REPORT-ONLY-FG NOT = "N"
                 GO TO ENTER-REPORT-ONLY-YN.

       EXIT-PARM.
           EXIT.

      ****************************
       DISPLAY-PARAMETERS SECTION.
      ****************************
           MOVE "S  8000TRANSDATE." TO VDU.
           MOVE TRANS-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  A010REPORTONLYFG." TO VDU.
           MOVE REPORT-ONLY-FG TO VDUBUF.
           PERFORM SCREENIO.

      ************************************************
       LOG-CHANGES SECTION.
      ************************************************

      * CODE TO WRITE CHANGE FILE.
           MOVE "W"              TO ERRLOGW-ACTION.

           MOVE LOG-BRNO         TO ERRLOGW-BRNO.
           MOVE LOG-MESS         TO ERRLOGW-EOM-MSG.
           MOVE FORM-PATHNAME    TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

      *****************************************************************
       CREATE-SPOOL-DIR SECTION.
      *****************************************************************
           MOVE "WAC"      TO EO-SPOOL-PCODE.
           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO EO-SPOOL-DATE.
           MOVE " " TO EO-SPOOL-FILE.
           PERFORM LOAD-EO-SPOOL.
           MOVE EO-SPOOL TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF STAT NOT = "00"
              CALL "C$MAKEDIR" USING EO-SPOOL GIVING STAT-99
              IF STAT NOT = "00"
                 GO TO EXIT-CREATE-SPOOL-DIR.

      *****************************************************************
      * THEY ARE ALLOWED TO RUN THIS UPDATE MORE THAN ONCE ON THE
      * SAME TRANSACTION DATE, SO CHECK TO SEE IF REPORT EXISTS, IF
      * SO, INCREMENT THE STEP # SO WE DON'T WIPE OUT PRIOR REPORTS
      *****************************************************************
           MOVE "/"        TO EO-SPOOL-SLASH.
           MOVE "CLAIM2"   TO EO-SPOOL-NAME.
           MOVE 1 TO STEP-COUNT.
       TRY-NEXT-STEP-NO.
           MOVE STEP-COUNT TO EO-SPOOL-STEP.
           MOVE EO-SPOOL TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "00" TO STAT
              GO TO EXIT-CREATE-SPOOL-DIR.
           IF STEP-COUNT < 999
              ADD 1 TO STEP-COUNT
              GO TO TRY-NEXT-STEP-NO.
       EXIT-CREATE-SPOOL-DIR.
           EXIT.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBSP/SPWRCL.CPY".
       COPY "LIBGL/GLGTCL.CPY".
       COPY "LIBLP/POSTDATE.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO CLAIM2-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CLAIM2-SYSDATE.
           DISPLAY CLAIM2-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE CLAIM2-FORM-FIELDS TO WR-BUF.
           MOVE CLAIM2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE CLAIM2-HELP--FILE TO HELP-LINK-FILE.
           MOVE CLAIM2-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/CLAIM2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ****************************
       MISC SECTION.
      ****************************
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/GETENV.CPY".
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *********************************
       IO-ROUTINES SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBSP/SPWRGS_SQL.CPY".
       COPY "LIBGL/GLGTGS_SQL.CPY".
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/EOSPOOLIO.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBSP/SPWR1IN.CPY".
       COPY "LIBGL/GLGT1IN.CPY".
       COPY "LIBGL/GLGL1RN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       OPEN-WK2-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE WK2-IFN TO E-FILE.
           OPEN OUTPUT WK2-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK2-FILE.
           IF IO-FG = 7
              CLOSE WK2-FILE
              GO TO OPEN-WK2-FILE.
       OPEN-WK2-FILE.
           PERFORM OPEN-IT.
           MOVE WK2-IFN TO E-FILE.
           OPEN INPUT WK2-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK2-FILE.
           IF IO-FG = 7
              CLOSE WK2-FILE
              GO TO OPEN-WK2-FILE.
       READ-WK2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE WK2-IFN TO E-FILE.
           READ WK2-FILE.
           IF IO-FG = 8
              GO TO READ-WK2-FILE-NEXT.
       WRITE-WK2-FILE.
           PERFORM WRITE-IT.
           MOVE WK2-IFN TO E-FILE.
           WRITE WK2-REC.
           IF IO-FG = 8
              GO TO WRITE-WK2-FILE.
       CLOSE-WK2-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK2-FILE.

       OPEN-CLAIMS-FILE.
           PERFORM OPEN-IT.
           MOVE CLAIMS-PATH TO E-FILE.
           OPEN INPUT CLAIMS-FILE.
           IF IO-FG = 8
              GO TO OPEN-CLAIMS-FILE.
           IF IO-FG = 7
              CLOSE CLAIMS-FILE
              GO TO OPEN-CLAIMS-FILE.
       READ-CLAIMS-FILE-NEXT.
           PERFORM READ-IT.
           MOVE CLAIMS-PATH TO E-FILE.
           MOVE CLAIMS-KEY TO E-KEYX.
           READ CLAIMS-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-CLAIMS-FILE-NEXT.
       CLOSE-CLAIMS-FILE.
           PERFORM CLOSE-IT.
           CLOSE CLAIMS-FILE.
       START-CLAIMS-FILE.
           PERFORM START-IT.
           MOVE CLAIMS-PATH TO E-FILE.
           MOVE CLAIMS-KEY TO E-KEYX.
           START CLAIMS-FILE KEY NOT < CLAIMS-KEY.
       COPY "LIBGB/FERRORS.CPY".

      **********************
       END-PROGRAM SECTION.
      **********************
           IF F7-KEY
              MOVE "N" TO CONTINUE-UPDATE.

      * RENAME CLAIMS.MMDDYY TO CLAIMS.YYMMDD.UPD
           IF REPORT-ONLY-FG = "N"
              IF CONTINUE-UPDATE = "N"
                 MOVE SPACES TO LOG-MESS
                 MOVE "LOS CLAIMS UPDATE CANCELED" TO LOG-MESS1
                 MOVE SPACES                 TO LOG-BRNOX
                 PERFORM LOG-CHANGES
              ELSE
                 MOVE "LOS CLAIMS UPDATE DONE" TO LOG-MESS1
                 MOVE SPACES                 TO LOG-BRNOX
                 PERFORM LOG-CHANGES
                 MOVE MV-CMD TO MV-BUF
                 MOVE CLAIMS-PATH TO MV-FILE1
                 MOVE CLAIMS-UPD-PATH TO MV-FILE2
                 PERFORM SYSTEM-CALL
                 IF STAT-BAD
                    MOVE "PROBLEM RENAMING CLAIMS FILE " TO MESS
                    PERFORM SEND-MESS.
      * NO MORE REMOTE COPY, NO MORE RISC2 SERVER
      *          ELSE
      *             IF EXT-FILPATH-MACHINE = CLAIMS-R1
      *                MOVE "REL" TO GETENV-BUF
      *                PERFORM GETENV-CALL
      *                MOVE GETENV-BUF TO CLAIM2-REL CLAIM2-REL2
      *                CALL "C$TOLOWER" USING CLAIM2-EC, VALUE 3
      *                CALL "C$TOLOWER" USING CLAIM2-SHELL, VALUE 10
      *                MOVE CLAIMS-PATH-DATE TO CLAIM2-COMMAND-DATE
      *                MOVE EXT-FILPATH-FIL TO CLAIM2-COMMAND-FIL
      *                MOVE RCP-CLAIM2 TO SYSTEM-BUF
      *                PERFORM SYSTEM-CALL.


           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.
           IF ERROR-FLAG = "Y"
              MOVE "INVALID GL ACCOUNTS EXISTS; CHECK JOURNAL!!"
                                                      TO MESS
              PERFORM SEND-MESS.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
           MOVE WK2PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.


       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE "SP/WRLDMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY CLAIM2-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
