       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GLDIST.
       DATE-WRITTEN.  071700
      **************************************************************************
      *   DIRECTORY  :  LP
      *   DESCRIPTION:  LOAN GL DISTRIBUTION REPORT BY GL ACCOUNT #
      *                 USES GROUP LOGIC
      *
      *                 LOOPS THRU ALL TRFILES - L,S,P,X,C,D,B,J,K USING
      *                                          DATE RANGE
      *                 READS GIFILE AND WRITES WORK FILE (LPEOD5TR.CPY)
      *                 END OF BRANCH, PRINTS GL DETAIL ENTRIES
      *                 END OF PROGRAM, PRINTS TOTAL DEBITS AND CREDITS.
      *
      *   R E M E M B E R:    CHANGE LP/LPEOD5.C & LP/RECAPJ.C
      *
      * REV:
      * BLV 020214 FIX BUG, IF GROUP IS SELECTED, PROGRAM SKIPPED ACCOUNT
      *            NUMBER RANGE ENTRY
      * BAH 021014 ADDED NEW TRANSACTION CODE OF "IN" INSURANCE LOSS, EXACTLY
      *            LIKE "PE", REGACC PR# 0007
      * JTG 021121 ADDED NEW CODE FILE TYPE 'BR' -BATCH PAYMENT REFERENCE CODES
      *            BATCH PAYMENT REVISIONS                         REGACC #0006
      * GLB 030508 WORLD PR#323 ADDED NEW "CT-FILE" FOR LIBLP/LPEOD5TR
      *            CODE TRAILER G/L INTERFACE TABLE & AMOUNT
      *            ONLY OCCURS IF CD-GC-FEE-GIFTNET = "Y"
      * CS  031111 ADDED FEE-COMMISSION (COMMISSION ON FEE CODES WRLD PR#331)
      * JTG 040330 ADDED LOGIC FOR POSTING BATCH PAYMENTS TO ACCOUNT'S
      *            WITH 'LN-OTBAL'                                 CENTRIX #2253
      *            ADDED BATCH-REFCD-OT-GLNO
      * JTG 040722 CREATED NEW TRCD 'ZX' WHAT WORKS LIKE 'SX'     CENTRIX #2314
      * BLF 070822 BARB & CINDY.  IF I-AM-WORLD & PMT POSTED BY COLLECTION
      *            BRANCH (BR# 197) ON P&L ACCOUNT, ALTER GL UPDATE SO POSTING
      *            BRANCH GETS P&L RECOVERIES POSTED TO THEIR BRANCH,
      *            WORLD PR# 551
      * BLF 070830 UNDID WORLD PR# 551
      *  CS 081216 ADDED BR-FILE, BR-USE-PARTIC-FLAT-TABLE IS TESTED IN
      *            LPEOD5TR  PR#232
      *  CS 150814 IF GP-I-AM-EXCEL, TEST THE FEECODE AMOUNT AGAINST CD-
      *            GC-BRANCH-COMM-AMOUNT  EXCEL PR#4821
      *  CS 150922 CHANGE BRANCH-COMM-AMOUNT TO S9(6) PR#4821 EXCEL
      * KEC 2016.0315 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-FEE???? TO CD-GC-????.
      *  BLM 160726 IF GP-I-AM-WORLD & CD-COMM-PERCENT = 99.99, FORCE THE
      *             PAYABLE PERCENT TO 100, FOR LIVE CHECKS, WORLD PR# 1170
      *  BAH 170602 ACTIVATE 8 DIGIT DATES, PD0006
      *   CS 181213 IF WORLD, CHANGE BANK TRANSACTIONS ID, IW, & BK TO
      *             UPDATE TO DEPOSIT BUCKET IN ED-FILE, SO THEY WILL
      *             BE UPDATED TO GTFILE WITH EXPLANATION OF "EOD-DEPOSIT",
      *             WORLD PR# 1353
      *             RECAPJ,GLRCAP,GLDIST,CL/CK1497 WERE SKIPPED IN
      *             ORIGINAL CHANGE
      *  BAH 20190925 REMOVED ACCESS-CALL TRP, TRS, TRL, TRX, TRB, TRJ, TRK
      *                                   TRC, TRD
      *  BAH 20190927 CD-FILE WAS MISSING FROM DECLARATIVES, USED IN LPEOD5TR
      *  BAH 20200625 ADDED OPEN-CDB1-FILE, OPEN-CD1-FILE, OPEN-LC1-FILE,
      *               OPEN-GI1-FILE, REMOVED THEM FROM LPEOD5TR, #TST001
      *  BAH 20210422 ADDED TR-POSTING-BR AND TR-OTHBR-FG FOR GLRCAP EXTRACT
      *                                                          #1496
      *  BLM 20210810 REMOVE TP-CODE, TP-POOLID NO LONGER IN KEY
      *  BAH 20220602 ADDED TR-ACTIONCD2 FOR GLRCAP EXTRACT, #1555
      *  MB  20230821 PERFORMANCE TUNING - ADDED STORED PROCEDURES
      *               EMBEDDED SQL QUERIES
      *               USES BRANCHLIST PROCESSING
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBLP/LPFSED.CPY".
       COPY "LIBLP/LPFSER.CPY".
       COPY "LIBGB/GBFSPR.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
      **************************************************
      *      E.O.D. REPORT WORK FILE                   *
      *      (EACH REPORT TO DEFINE ED-REC)            *
      **************************************************
       FD  ED-FILE
           LABEL RECORDS ARE STANDARD.
       01  ED-REC.
           03  ED-KEY.
               05  ED-NO              PIC 9(11).
               05  ED-TR-DATE         PIC 9(8).
               05  ED-CODE-ACCT.
                   07  ED-TR-CODE     PIC X.
                   07  ED-TR-ACCT     PIC 9(8).
               05  ED-TR-SUBKEY       PIC X(17).
               05  ED-TR-SEQNO        PIC 9(3).

           03  ED-AMT                 PIC S9(9)V99 COMP-3.
           03  ED-TR-TYPE             PIC XX.
           03  ED-TR-CLASS            PIC 9(3).
           03  ED-DESC                PIC X(30).
           03  FILLER                 PIC X(8).

      * ED-AMT     - TRANSACTION CODES (TRP)
      * ED-AMT     - SALES FINANCE LOANS (TRS)
      * ED-AMT     - DIRECT LOANS (TRL)
      * ED-AMT     - TRANSFERS (TRX)
      * ED-AMT     - CHECKS (TRC)
      * ED-AMT     - DEPOSITS (TRD)
      * ED-AMT     - BRANCH (BULK) TRANSFERS (TRB)
      * ED-AMT     - BRANCH (CLASS) TRANSFERS (TRJ)
      * ED-AMT     - BRANCH (CLASS) TRANSFERS (TRJ)

       COPY "LIBLP/LPFDER.CPY".
       COPY "LIBGB/GBFDPR.CPY".

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)LP/GLDIST S35 12/04/23-09:58:26 >".
      ****************************
      *     F I L E   I F N      *
      ****************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01TRL.CPY".
       COPY "LIBLP/LP01TRL_SQL.CPY".
       COPY "LIBLP/LPWSTRL.CPY".
       COPY "LIBLP/LP01TRS.CPY".
       COPY "LIBLP/LP01TRS_SQL.CPY".
       COPY "LIBLP/LPWSTRS.CPY".
       COPY "LIBLP/LP01TRP.CPY".
       COPY "LIBLP/LP01TRP_SQL.CPY".
       COPY "LIBLP/LPWSTRP.CPY".
       COPY "LIBLP/LP01TRX.CPY".
       COPY "LIBLP/LP01TRX_SQL.CPY".
       COPY "LIBLP/LPWSTRX.CPY".
       COPY "LIBLP/LP01TRC.CPY".
       COPY "LIBLP/LP01TRC_SQL.CPY".
       COPY "LIBLP/LPWSTRC.CPY".
       COPY "LIBLP/LP01TRD.CPY".
       COPY "LIBLP/LP01TRD_SQL.CPY".
       COPY "LIBLP/LPWSTRD.CPY".
       COPY "LIBLP/LP01TRB.CPY".
       COPY "LIBLP/LP01TRB_SQL.CPY".
       COPY "LIBLP/LPWSTRB.CPY".
       COPY "LIBLP/LP01TRJ.CPY".
       COPY "LIBLP/LP01TRJ_SQL.CPY".
       COPY "LIBLP/LPWSTRJ.CPY".
       COPY "LIBLP/LP01TRK.CPY".
       COPY "LIBLP/LP01TRK_SQL.CPY".
       COPY "LIBLP/LPWSTRK.CPY".
       COPY "LIBLP/LPWSER.CPY".
       COPY "LIBGL/GL01GI.CPY".
       COPY "LIBGL/GL01GI_SQL.CPY".
       COPY "LIBGL/GLWSGI.CPY".
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".

       COPY "LIBGB/GRSSPW.CPY".
       COPY "LIBGB/GRBRLSW.CPY".
       COPY "LIBGB/ARRAYBRW.CPY".
       COPY "LIBLP/ARRAYLCW.CPY".
       COPY "LIBGL/ARRAYGIW.CPY".

      *----------------------------------------------------------------

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  PASS-WBEG-DATE                PIC X(10).
       01  PASS-WEND-DATE                PIC X(10).
       01  PASS-SQL                      PIC X(7500).
       01  WS-TRBSW                      PIC X.
       01  WS-TRCSW                      PIC X.
       01  WS-TRDSW                      PIC X.
       01  WS-TRJSW                      PIC X.
       01  WS-TRKSW                      PIC X.
       01  WS-TRLSW                      PIC X.
       01  WS-TRPSW                      PIC X.
       01  WS-TRSSW                      PIC X.
       01  WS-TRXSW                      PIC X.
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  ORIG-BR-REC                    PIC X(BR-SIZE).
       01  WS-SELECT                     PIC X(7100).
       01  WS-WHERE                      PIC X(200).
       01  WS-ORDERBY                    PIC X(200).

       01  MISC-CONN-OPEN-SW             PIC X         VALUE "N".
       01  BR2-CURSOR-STAT               PIC X(2)      VALUE SPACES.
           88  BR2-CURSOR-STAT-GOOD                    VALUE "00".
           88  BR2-CURSOR-STAT-BAD             VALUE "  " THRU "//"
                                                     "01" THRU "~~".
       01  BR2-CONN-OPEN-SW            PIC X           VALUE "N".
       01  PROC-LOOP-SUB               PIC 9(4)        VALUE 0.

       01  ED1-IFN              PIC X(3)     VALUE "ED1".
       01  PR-IFN               PIC X(2)     VALUE "PR".

      * BASE ELEMENT NUMBERS FOR UNEARNED AND EARNINGS
      * GL ACCOUNTS:
      *01  UNSUB                PIC 99       VALUE 13.
      *01  ERSUB                PIC 999      VALUE 63.
      *01  ORSUB                PIC 999      VALUE 79.
      *01  OROFFSUB             PIC 999      VALUE 92.
      *01  RFSUB                PIC 999      VALUE 105.
      *01  RFOFFSUB             PIC 999      VALUE 118.
      *01  GISUB                PIC 999.
      *01  SUBR                 PIC 9.

       01  FUNK-SUB             PIC 999.
      *PLSUB MUST BE PIC 9:
       01  PLSUBX.
           03  PLSUB            PIC 9.

       01  SAVE-ED-TR-SEQNO     PIC 9(3).
       01  SAVE-ED-CODE-ACCT    PIC X(9).
       01  SAVE-ED-TR-SUBKEY    PIC X(17).
       01  SAVE-ED-NO           PIC 9(11).
       01  SAVE-ED-DATE         PIC 9(8).

       01  DR-CR-FG             PIC X.

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  ER-FEED              PIC 9        VALUE 1.
       01  LEGEND               PIC X(60)    VALUE SPACES.

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  END-DATE.
               05  END-MONTH   PIC 99.
               05  END-YEAR    PIC 99.
           03  EDATE           REDEFINES END-DATE
                               PIC 9(4).
           03  DAY-OF-MONTH    PIC 9(2).
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).
           03  GLBR1           PIC 9(4).
           03  GLBR2           PIC 9(4).
           03  ACCOUNT1        PIC 9(7).
           03  ACCOUNT2        PIC 9(7).
           03  TRANS-TYPE      PIC XXX.

       01  VERSION-CONTROL     PIC X VALUE "B".
       01  BEGINNING-DATE       PIC 9(8).
       01  ENDING-DATE          PIC 9(8).

       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.

      **********************
      *   MISC WORKERS     *
      **********************
       01  SUB                  PIC 9(6) COMP.
       01  L1                   PIC 9(6) COMP.
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  CLEARING-WK          PIC S9(10)V99  COMP.
       01  FEE-COMMISSION       PIC S9(4)V99.

       01  HOLD-BRNO            PIC 9(4).

       01  DF-TEST              PIC X        VALUE " ".
           88  DF-POSTING                    VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
      ****************************************************
      * TOTALS TABLE:  LEVEL 1 = 1 DAY FOR 1 GL ACCOUNT
      *                LEVEL 2 = GL ACCOUNT TOTALS
      *                LEVEL 3 = BRANCH
      *                LEVEL 4 = GRAND
      ****************************************************
       01  TOTALS-TABLE         OCCURS 4.
           03  TOT-DR           PIC S9(9)V99 COMP VALUE 0.
           03  TOT-CR           PIC S9(9)V99 COMP VALUE 0.
       01  LEVEL                PIC 9.

      * LPEOD5TR
       01  TR-DATE              PIC 9(8).
       01  TR-CODE              PIC X.
       01  TR-ACCT              PIC 9(8).
       01  TR-TYPE-TRCD         PIC XX.
       01  TR-REASON            PIC X(30).
       01  TR-CLASS             PIC 9(3).
       01  TR-REFNO             PIC X(5).
       01  TR-PLTYPE            PIC X(1).
       01  TR-POSTING-BR        PIC 9(4).
       01  TR-OTHBR-FG          PIC X(1).
       01  TR-ACTIONCD2         PIC X(4).

       01  TR-SUBKEY            PIC X(17).
       01  TRP-SUBKEY REDEFINES TR-SUBKEY.
           03  TRP-SUBKEY-ACCTNO       PIC 9(8).
           03  TRP-SUBKEY-LINE         PIC 9(3).
       01  OTHER-SUBKEY REDEFINES TR-SUBKEY.
           03  OTHER-SUBKEY-ACCTNO       PIC 9(8).
           03  OTHER-SUBKEY-LAWCODE      PIC 9(2).
           03  OTHER-SUBKEY-LINE         PIC 9(3).

       01  BATCH-REFCD          PIC X(5)      VALUE LOW-VALUES.
       01  BATCH-REFCD-GLNO     PIC 9(11)     VALUE 0.
       01  BATCH-REFCD-OT-GLNO  PIC 9(11)     VALUE 0.
       01  GLNO                 PIC 9(11)     VALUE 0.
       01  GLNOX               REDEFINES GLNO.
           03  FILLER           PIC X(4).
           03  FILLER           PIC X(3).
           03  GLOTHBR          PIC 9999.
       01  GLNOZ               REDEFINES GLNO.
           03  GLNO-BR          PIC 9999.
           03  GLNO-NUMBER      PIC 9(7).
       01  FILLER               REDEFINES GLNO.
           03  GLNO-BRANCH      PIC 9(4).
           03  GLNO-ACCT        PIC 9(7).

       01  GLAMT                PIC S9(9)V99 VALUE 0.
       01  GLWK                 PIC S9(9)V99 VALUE 0.
       01  ERROR-FG             PIC X        VALUE "N".
       01  HOLD-COMM-PERCENT    PIC 999V99   VALUE 0.
       01  HOLD-PL-EXP          PIC S9(9)V99 VALUE 0.

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
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
           03  H-USERID         PIC X(12).
           03  FILLER           PIC X(24)    VALUE " ".
           03  FILLER           PIC X(41)    VALUE
               " LOAN PROCESSING GL DISTRIBUTION JOURNAL ".

       01  HEAD3.
           03  FILLER           PIC X(10) VALUE "BRANCH :".
           03  H-BRNO           PIC ZZ99.
           03  FILLER           PIC X(21) VALUE SPACES.
           03  FILLER           PIC X(27) VALUE " ".

       01  HEAD4.
           03  FILLER           PIC X(50)   VALUE
           "G/L ACCOUNT   DESCRIPTION                         ".
           03  FILLER           PIC X(12)   VALUE "TRANS DATE".
           03  FILLER           PIC X(33)   VALUE
               "SOURCE/TRANSACTION INFORMATION ".
           03  FILLER           PIC X(17)   VALUE "ACCT/CK #".
           03  FILLER           PIC X(14)   VALUE "DEBIT         ".
           03  FILLER           PIC X(6)    VALUE "CREDIT".

       01  DETAIL-LINE          PIC X(132) VALUE " ".

       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  D-G-BRNO         PIC Z9(3).
           03  D-G-HYPHEN       PIC X.
           03  D-G-GLNO         PIC 9(7).
           03  FILLER           PIC X(2).
           03  D-G-DESCRIPTION  PIC X(38).
           03  D-TR-DATE        PIC 99/99/99.
           03  FILLER           PIC X(2).
           03  D-SOURCE-DESC    PIC X(31).
           03  D-TRANS-DESC REDEFINES D-SOURCE-DESC.
               05  D-TR-CODE2   PIC X(3).
               05  D-TR-DESC    PIC X(28).
           03  FILLER           PIC XX.
           03  D-TR-ACCT        PIC ZZZZZZ.
           03  FILLER           PIC X.
           03  D-G-DR           PIC ZZZ,ZZZ,ZZ9.99- BLANK ZERO.
           03  D-STAR-DR REDEFINES D-G-DR
                                PIC **********9.99-.
           03  D-G-CR           PIC ZZZ,ZZZ,ZZ9.99- BLANK ZERO.
           03  D-STAR-CR REDEFINES D-G-CR
                                PIC **********9.99-.

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(35).
           03  RANGE-SELECTION.
               04  R-NUM        PIC 9(10).

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBLP/LPEDPATH.CPY".
       COPY "LIBLP/EMSFRG_CMD.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBLP/LPSETTYW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/GRWORKW.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/GLDIST_DEF.CPY".
       COPY "LIBLP/GLDIST_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ****************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/GLDIST_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               ED-FILE PR-FILE ER-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TRX1-FILE.
           PERFORM CLOSE-TRS1-FILE.
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-TRL1-FILE.
           PERFORM CLOSE-TRK1-FILE.
           PERFORM CLOSE-TRJ1-FILE.
           PERFORM CLOSE-TRD1-FILE.
           PERFORM CLOSE-TRC1-FILE.
           PERFORM CLOSE-TRB1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-GI1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           PERFORM CLOSE-BR2-CONNECTION.
           PERFORM CLOSE-MISC-CONNECTION.
           CLOSE
               ED-FILE ER-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ***********************************************
       MAIN-MODULE SECTION.
      ***********************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "GLDIST" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.

      *     IF GROUP-FLAG = "N"
      *        MOVE BEG-BRANCH TO BR-NO HOLD-BRNO
      *        PERFORM START-BR-FILE
      *        GO TO NEXT-BRANCH-NO.

      *     MOVE ENT-GROUP TO GR-ID.
      *     PERFORM SEARCH-GR-INIT.
      ** NOTE THIS BRANCH IO LOOKS A LITTLE WIERD - IT'S BECAUSE COPY
      ** MEMBER LPEOD5TR READS & CLOSES BR-FILE...SO WE HAVE TO SAVE
      ** BRNO, REOPEN FILE, START & DO AN EXTRA READ NEXT

      * LOAD BRANCHES TO PROCESS USING STORED PROCEDURE SSP_TCLP_BRANCH_GROUP_LOGIC_SELECT
      *   INTO BGLS-BRANCH-TBL
           IF GROUP-FLAG = "Y"
               MOVE ENT-GROUP TO BGLS-PASS-BEG-BRGRP
               MOVE SPACES    TO BGLS-PASS-END-BRGRP
           ELSE
               MOVE BEG-BRANCH           TO BGLS-WORK-BRNO
               MOVE BGLS-WORK-BRNO-TEXT  TO BGLS-PASS-BEG-BRGRP
               MOVE END-BRANCH           TO BGLS-WORK-BRNO
               MOVE BGLS-WORK-BRNO       TO BGLS-PASS-END-BRGRP.

           PERFORM LOAD-BGLS-BRANCH-LIST.

      * SET BRANCHLIST-PGM-PROC-MAX TO NUMBER OF BRANCHES TO PROCESS PER STORED PROCEDURE CURSOR
           MOVE 300 TO BRANCHLIST-PGM-PROC-MAX.

      * LOOP THRU BGLS-BRANCH-TBL (LOADED BY RETRIEVE-BGLS-BRANCH-LIST)
      *  BGLS-BRANCHES-RETR-CTR = TOTAL BRANCHES LOADED IN LOADED BY RETRIEVE-BGLS-BRANCH-LIST
      *  BRANCHLIST-PGM-PROC-MAX - MAX BRANCHES TO PROCESS AT ONE TIME
       PROC-LOOP-INIT.
           INITIALIZE BRANCHLIST-TBL
                      BRANCHLIST-SUB
                      BRANCHLIST-PROC-BRANCH-CTR 
                      PASS-BRANCHLIST.

      * RETRIEVE BRANCHES WE WANT TO PROCESS FROM BGLS-BRANCH-TBL
      * WILL PROCESS UNTIL END OF BGLS-BRANCH-TBL OR BRANCHLIST-PGM-PROC-MAX
       PROC-LOOP.
           ADD 1 TO PROC-LOOP-SUB.

           IF PROC-LOOP-SUB > BGLS-BRANCHES-RETR-CTR
              GO TO PROC-LOOP-CONT.

           ADD 1 TO BRANCHLIST-PROC-BRANCH-CTR BRANCHLIST-SUB.

           MOVE BGLS-BRANCH-BRNO(PROC-LOOP-SUB) TO 
                            BRANCHLIST-BRNO(BRANCHLIST-SUB).

           IF BRANCHLIST-SUB > 1
              MOVE ","   TO BRANCHLIST-COMMA(BRANCHLIST-SUB).

           IF BRANCHLIST-PROC-BRANCH-CTR < BRANCHLIST-PGM-PROC-MAX 
               GO TO PROC-LOOP.
 
       PROC-LOOP-CONT.
           IF BRANCHLIST-SUB = 0
              MOVE 4 TO LEVEL
              PERFORM PRINT-TOTALS
              GO TO END-PROGRAM.

           MOVE BEGINNING-DATE      TO TP-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO PASS-WBEG-DATE.

           MOVE ENDING-DATE         TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO PASS-WEND-DATE.
           MOVE BRANCHLIST-TBL TO PASS-BRANCHLIST.

           PERFORM START-BR2-FILE.

       NEXT-GROUP-LOC.
           PERFORM READ-BR2-FILE-NEXT.
           IF IO-BAD 
              PERFORM CLOSE-BR2-FILE
              GO TO PROC-LOOP-INIT.

      *     PERFORM OPEN-BR-FILE.
      *     PERFORM READ-BR-FILE.
           MOVE BR-NO TO BR-KEY.
           PERFORM ARRAYBR-READ.
           IF IO-BAD
              GO TO IO-ERROR.

           MOVE BR-REC TO ORIG-BR-REC.

      *    PERFORM OPEN-BR-FILE.
      *    IF GROUP-FLAG NOT = "Y"
      *       GO TO START-NEXT-BRANCH-NO.
      *
      *    PERFORM SEARCH-GR-NEXT.
      *    IF WGR-SRESULT = 0
      *       MOVE 4 TO LEVEL
      *       PERFORM PRINT-TOTALS
      *       GO TO END-PROGRAM.
      *
      *    MOVE WGR-SRESULT TO BR-NO.
      *    PERFORM READ-BR-FILE.
      *    IF IO-FG NOT = 0
      *       GO TO IO-ERROR.
      *
      *    GO TO MOVE-BRANCH-PATH-INFO.
      *
      *START-NEXT-BRANCH-NO.
      *    MOVE HOLD-BRNO TO BR-NO.
      *    PERFORM START-BR-FILE.
      *    PERFORM READ-BR-FILE-NEXT.
      *
      *NEXT-BRANCH-NO.
      *    PERFORM READ-BR-FILE-NEXT.
      *    IF IO-FG = 9 OR BR-NO > END-BRANCH
      *       MOVE 4 TO LEVEL
      *       PERFORM PRINT-TOTALS
      *       GO TO END-PROGRAM.
      *
      *    MOVE BR-NO TO HOLD-BRNO.

       MOVE-BRANCH-PATH-INFO.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO TRL-PATH-OVERRIDE
                                              TRS-PATH-OVERRIDE
                                              TRP-PATH-OVERRIDE
                                              TRX-PATH-OVERRIDE
                                              TRC-PATH-OVERRIDE
                                              TRD-PATH-OVERRIDE
                                              TRB-PATH-OVERRIDE
                                              TRJ-PATH-OVERRIDE
                                              TRK-PATH-OVERRIDE.

           MOVE 99  TO LN-COUNT.
           MOVE BR-NO TO H-BRNO.

      * RELOAD GI TABLE AT BRANCH BREAKS.
           MOVE BR-NO TO ARRAYGI-GI-BRANCH.
           MOVE ZEROS TO ARRAYGI-GI-CLASS.
           PERFORM ARRAYGI-LOAD.

      * MUST HOLD ORIG-BR-REC TO LPEOD5TR BECAUSE LPEOD5TF CHANGES BR VALUES !!!

           IF PRU-LP NOT = "VDU" AND NOT = "PRU"
               STRING "GL DETAIL BY ACCOUNT IN PROGRESS...", BR-NO
                     DELIMITED "  " INTO MESS
              PERFORM PROGRESS-MSG.

           IF TRANS-TYPE = "ALL" OR "B"
              IF WS-TRBSW = 'Y' 
                 PERFORM PROCESS-TRB-FILE.

           IF TRANS-TYPE = "ALL" OR "C"
              IF WS-TRCSW = 'Y'
                 PERFORM PROCESS-TRC-FILE.

           IF TRANS-TYPE = "ALL" OR "D"
              IF WS-TRDSW = 'Y'
                 PERFORM PROCESS-TRD-FILE.

           IF TRANS-TYPE = "ALL" OR "J"
              IF WS-TRJSW = 'Y'
                 PERFORM PROCESS-TRJ-FILE.

           IF TRANS-TYPE = "ALL" OR "K"
              IF WS-TRKSW = 'Y'
                 PERFORM PROCESS-TRK-FILE.

           IF TRANS-TYPE = "ALL" OR "L"
              IF WS-TRLSW = 'Y'
                 PERFORM PROCESS-TRL-FILE.

           IF TRANS-TYPE = "ALL" OR "P"
              IF WS-TRPSW = 'Y'
                 PERFORM PROCESS-TRP-FILE.

           IF TRANS-TYPE = "ALL" OR "S"
              IF WS-TRSSW = 'Y'
                 PERFORM PROCESS-TRS-FILE.

           IF TRANS-TYPE = "ALL" OR "X"
              IF WS-TRXSW = 'Y'
                 PERFORM PROCESS-TRX-FILE.

           MOVE ORIG-BR-REC TO BR-REC.

       GL-SUMMARY.

           IF ERROR-FG = "Y"
              GO TO TODAYS-SUMMARY.

           PERFORM CLOSE-ED1-FILE.
           PERFORM OPEN-ED1-FILE.

           MOVE LOW-VALUES TO ED-KEY SAVE-ED-CODE-ACCT SAVE-ED-TR-SUBKEY.
           MOVE 0 TO SAVE-ED-NO SAVE-ED-DATE.
           MOVE 0 TO ED-NO ED-TR-DATE ED-TR-ACCT ED-TR-SEQNO.

           PERFORM START-ED1-FILE.

       NEXT-GL-SUMMARY.
           PERFORM READ-ED1-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-GL-SUMMARY.

           PERFORM PRINT-GL-DETAIL.

           GO TO NEXT-GL-SUMMARY.

       END-GL-SUMMARY.
           MOVE 3 TO LEVEL.
           PERFORM PRINT-TOTALS.

       TODAYS-SUMMARY.
           IF ERROR-FG = "Y"
              MOVE "E" TO EXT-EOBUF-ERRCD.

           PERFORM CLOSE-ED1-FILE.
           PERFORM OPEN-ED1-FILE-OUTPUT.
           PERFORM CLOSE-ED1-FILE.
           PERFORM OPEN-ED1-FILE.
           MOVE 99 TO LN-COUNT.

           GO TO NEXT-GROUP-LOC.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE GLDIST-QUEUE-POS TO Q-POS.
           MOVE GLDIST-COPIES-POS TO C-POS.
           MOVE GLDIST-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO ED-PATH.
           OPEN OUTPUT ED-FILE.
           IF FILE-STAT NOT = "00"
              MOVE "CAN'T CREATE JOURNAL WORK FILE" TO MESS
              PERFORM SEND-MESS
              GO TO END-PROGRAM.
           CLOSE ED-FILE.

      * GET GLOBAL PARAMETER RECORD (FROM GPENV):
           PERFORM GET-GPENV.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF BEG-BRANCH = 0
              IF END-BRANCH = 0
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH
                                             END-BRANCH.

           MOVE 0 TO TOT-DR(1) TOT-CR(1)
                     TOT-DR(2) TOT-CR(2)
                     TOT-DR(3) TOT-CR(3)
                     TOT-DR(4) TOT-CR(4).

           IF EXT-ROUTE-BUF = "00"
              MOVE "PROGRAM IN PROGRESS..............." TO MESS
              PERFORM PROGRESS-MSG.

      * SET DATE RANGE FOR TESTING TR FILES
           MOVE END-MONTH TO DATE-MMDDYY-MM.
           MOVE END-YEAR  TO DATE-MMDDYY-YY.
           IF DAY-OF-MONTH = 99
              MOVE 01           TO DATE-MMDDYY-DD
           ELSE
              MOVE DAY-OF-MONTH TO DATE-MMDDYY-DD.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO BEGINNING-DATE.

           MOVE END-MONTH TO DATE-MMDDYY-MM.
           MOVE END-YEAR  TO DATE-MMDDYY-YY.
           IF DAY-OF-MONTH = 99
              MOVE 31           TO DATE-MMDDYY-DD
           ELSE
              MOVE DAY-OF-MONTH TO DATE-MMDDYY-DD.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NDTE-DATE.
           PERFORM NEW-DATE-CALCULATION.
           MOVE NDTE-DATE TO ENDING-DATE.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-ED1-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-CDB1-FILE.
           PERFORM OPEN-CD1-FILE.
           PERFORM OPEN-LC1-FILE.
           PERFORM OPEN-GI1-FILE.

           PERFORM OPEN-MISC-CONNECTION.

           PERFORM ARRAYBR-LOAD.
           PERFORM ARRAYLC-LOAD.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ********************************************
       EO-EXEC-AUDIT SECTION.
      ********************************************
           MOVE EXT-EOBUF-DATE2 TO SYS-DATE.
           MOVE S-MM TO END-MONTH.
           MOVE S-YY TO END-YEAR.

           IF EXT-EOBUF-DATE1 NOT = EXT-EOBUF-DATE2
              MOVE 99 TO DAY-OF-MONTH
           ELSE
              MOVE S-DD TO DAY-OF-MONTH.

      ********************************************
       ENTER-PARAMETERS SECTION.
      ********************************************
       ENTER-PARM.
           IF VDUSTATUS = "1U"
              IF EXT-ROUTE-BUF = "00"
                 GO TO E-TRANS-TYPE
              ELSE
                 GO TO BR-01.

      * DEFAULT TO USER BRANCH
      *    MOVE "SNNN040BR1." TO VDU.
      *    MOVE 0 TO BEG-BRANCH VDUNUM0.
      *    PERFORM SCREENIO.
      *    MOVE "SNNN040BR2." TO VDU.
      *    MOVE 0 TO END-BRANCH VDUNUM0.
      *    PERFORM SCREENIO.

       BR-01.
           MOVE LEGEND-BR-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "R  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1U" GO TO BR-01.
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
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-01.

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
              GO TO DTE.
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

           GO TO DTE.
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

       DTE.
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO DA-01.
           MOVE LEGEND-DTE TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040ENDDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DTE.
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
              GO TO E-GL-BR1.

           IF ( UP-KEY )
              IF ( EXT-ROUTE-BUF = "00" )
                 GO TO DTE
              ELSE
                 GO TO BR-02.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DTE.
           MOVE VDUNUM0 TO DAY-OF-MONTH.

           IF DAY-OF-MONTH > 31  OR
              (DAY-OF-MONTH > 30 AND
              (END-MONTH = 2 OR 4 OR 6 OR 9 OR 11)) OR
              (DAY-OF-MONTH > 29 AND END-MONTH = 2)
              MOVE "DAY IS PAST LAST DAY OF MONTH" TO MESS
              PERFORM SEND-MESS
              GO TO DA-01.

      *-----------------------------------------------------------------
       E-GL-BR1.
           MOVE "EN N040GLBR1." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY GO TO EXIT-PARM.
           IF F3-KEY GO TO ALL-GL-BRS.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY
              IF EXT-ROUTE-BUF = "00"
                 GO TO E-TRANS-TYPE
              ELSE
                 GO TO DA-01.
           IF NOT ENTER-KEY GO TO E-GL-BR1.
           MOVE VDUNUM0 TO GLBR1.
       E-GL-BR2.
           MOVE "EN N040GLBR2." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY GO TO EXIT-PARM.
           IF F3-KEY GO TO ALL-GL-BRS.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO E-GL-BR1.
           IF NOT ENTER-KEY GO TO E-GL-BR2.
           MOVE VDUNUM0 TO GLBR2.
           GO TO E-ACCT1.
       ALL-GL-BRS.
           MOVE "SN N040GLBR1." TO VDU.
           MOVE 0 TO GLBR1 VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N040GLBR2." TO VDU.
           MOVE 9999 TO GLBR2 VDUNUM0.
           PERFORM SCREENIO.
       E-ACCT1.
           MOVE "EN N070ACCT1." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY GO TO EXIT-PARM.
           IF F3-KEY GO TO ALL-ACCTS.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO E-GL-BR1.
           IF NOT ENTER-KEY GO TO E-ACCT1.
           MOVE VDUNUM0 TO ACCOUNT1.
       E-ACCT2.
           MOVE "EN N070ACCT2." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY GO TO EXIT-PARM.
           IF F3-KEY GO TO ALL-ACCTS.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO E-ACCT1.
           IF NOT ENTER-KEY GO TO E-ACCT2.
           MOVE VDUNUM0 TO ACCOUNT2.
           GO TO E-TRANS-TYPE.
       ALL-ACCTS.
           MOVE "SN N070ACCT1." TO VDU.
           MOVE 0 TO ACCOUNT1 VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N070ACCT2." TO VDU.
           MOVE 9999999 TO ACCOUNT2 VDUNUM0.
           PERFORM SCREENIO.
       E-TRANS-TYPE.
           MOVE "E  A010TRANSTYPE." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY GO TO EXIT-PARM.
           IF F3-KEY GO TO ALL-TRANS-TYPES.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO E-ACCT2.
           IF NOT ENTER-KEY GO TO E-TRANS-TYPE.
           MOVE VDUBUF TO TRANS-TYPE.
           IF NOT (TRANS-TYPE = "P" OR "S" OR "L" OR "X" OR "C" OR "D"
                                    OR "B" OR "J" OR "K")
              MOVE "PLEASE ENTER P,S,L,X,C,D,B,J,K, OR F3 FOR ALL"
                 TO MESS
              PERFORM SEND-MESS
              GO TO E-TRANS-TYPE.
           GO TO EXIT-PARM.
       ALL-TRANS-TYPES.
           MOVE "S  A030TRANSTYPE." TO VDU.
           MOVE "ALL" TO TRANS-TYPE VDUBUF.
           PERFORM SCREENIO.
       END-PARM.
           MOVE LEGEND-ACCEPT TO LEGEND.
           PERFORM SEND-LEGEND.
       EXIT-PARM.
           EXIT.

      ****************************************
       DISPLAY-PARAMETERS SECTION.
      ****************************************

           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF
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

           MOVE "SNNN020DA1." TO VDU.
           MOVE DAY-OF-MONTH TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040GLBR1." TO VDU.
           MOVE GLBR1 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040GLBR2." TO VDU.
           MOVE GLBR2 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN070ACCT1." TO VDU.
           MOVE ACCOUNT1 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN070ACCT2." TO VDU.
           MOVE ACCOUNT2 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A010TRANSTYPE." TO VDU.
           MOVE TRANS-TYPE TO VDUBUF.
           PERFORM SCREENIO.

      *****************************************
       PROCESS-TRP-FILE SECTION.
      *****************************************
           PERFORM OPEN-TRP1-FILE.

           MOVE BR-NO               TO TP-BRNO
                                       QTRP1-WBEG-BRNO
                                       QTRP1-WEND-BRNO.

           MOVE BEGINNING-DATE      TO TP-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRP1-WBEG-DATE.
           MOVE ENDING-DATE         TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRP1-WEND-DATE.

           PERFORM START-TRP1-FILE.  *> SSP - TRP

       PROCESS-TRP-NEXT.
           PERFORM READ-TRP1-FILE-NEXT.   *> SSP - TRP
           IF (IO-FG NOT = 0)              *> OR (TRP-PATH-OWNBR NOT = TP-BRNO)
              GO TO EXIT-PROCESS-TRP-FILE.

           IF TP-DATE > ENDING-DATE
              GO TO EXIT-PROCESS-TRP-FILE.

           PERFORM TRP1-DETAIL.

           GO TO PROCESS-TRP-NEXT.

       EXIT-PROCESS-TRP-FILE.
           PERFORM CLOSE-TRP1-FILE.

      *****************************************
       PROCESS-TRS-FILE SECTION.
      *****************************************
           PERFORM OPEN-TRS1-FILE.   

           MOVE BR-NO               TO TS-BRNO
                                       QTRS1-WBEG-BRNO
                                       QTRS1-WEND-BRNO.

           MOVE BEGINNING-DATE      TO TS-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRS1-WBEG-DATE.
           MOVE ENDING-DATE         TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRS1-WEND-DATE.

           PERFORM START-TRS1-FILE.   *> SSP - TRS

       PROCESS-TRS-NEXT.
           PERFORM READ-TRS1-FILE-NEXT.  *> SSP - TRS
           IF (IO-FG NOT = 0) OR (TRS-PATH-OWNBR NOT = TS-BRNO)
              GO TO EXIT-PROCESS-TRS-FILE.

           IF TS-DATE > ENDING-DATE
              GO TO EXIT-PROCESS-TRS-FILE.

           PERFORM TRS1-DETAIL.

           GO TO PROCESS-TRS-NEXT.

       EXIT-PROCESS-TRS-FILE.
           PERFORM CLOSE-TRS1-FILE.

      *****************************************
       PROCESS-TRL-FILE SECTION.
      *****************************************
           PERFORM OPEN-TRL1-FILE.

           MOVE BR-NO               TO TL-BRNO
                                       QTRL1-WBEG-BRNO
                                       QTRL1-WEND-BRNO.

           MOVE BEGINNING-DATE      TO TL-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRL1-WBEG-DATE.
           MOVE ENDING-DATE         TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRL1-WEND-DATE.

           PERFORM START-TRL1-FILE.   *> SSP - TRL

       PROCESS-TRL-NEXT.
           PERFORM READ-TRL1-FILE-NEXT.  *> SSP - TRL
           IF (IO-FG NOT = 0) OR (TRL-PATH-OWNBR NOT = TL-BRNO)
              GO TO EXIT-PROCESS-TRL-FILE.

           IF TL-DATE > ENDING-DATE
              GO TO EXIT-PROCESS-TRL-FILE.

           PERFORM TRL1-DETAIL.

           GO TO PROCESS-TRL-NEXT.

       EXIT-PROCESS-TRL-FILE.
           PERFORM CLOSE-TRL1-FILE.

      *****************************************
       PROCESS-TRX-FILE SECTION.
      *****************************************
           PERFORM OPEN-TRX1-FILE.

           MOVE BR-NO               TO TX-BRNO
                                       QTRX1-WBEG-BRNO
                                       QTRX1-WEND-BRNO.

           MOVE BEGINNING-DATE      TO TX-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRX1-WBEG-DATE.
           MOVE ENDING-DATE         TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRX1-WEND-DATE.

           PERFORM START-TRX1-FILE.   *> SSP - TRX

       PROCESS-TRX-NEXT.
           PERFORM READ-TRX1-FILE-NEXT.    *> SSP - TRX
           IF (IO-FG NOT = 0) OR (TRX-PATH-OWNBR NOT = TX-BRNO)
              GO TO EXIT-PROCESS-TRX-FILE.

           IF TX-DATE > ENDING-DATE
              GO TO EXIT-PROCESS-TRX-FILE.

           PERFORM TRX1-DETAIL.

           GO TO PROCESS-TRX-NEXT.

       EXIT-PROCESS-TRX-FILE.
           PERFORM CLOSE-TRX1-FILE.

      *****************************************
       PROCESS-TRB-FILE SECTION.
      *****************************************
           PERFORM OPEN-TRB1-FILE.

           MOVE BR-NO               TO TB-BRNO
                                       QTRB1-WBEG-BRNO
                                       QTRB1-WEND-BRNO.

           MOVE BEGINNING-DATE      TO TB-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRB1-WBEG-DATE.
           MOVE ENDING-DATE         TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRB1-WEND-DATE.

           PERFORM START-TRB1-FILE.   *> SSP - TRB

       PROCESS-TRB-NEXT.
           PERFORM READ-TRB1-FILE-NEXT.   *> SSP - TRB
           IF (IO-FG NOT = 0)        *> OR (TRB-PATH-OWNBR NOT = TB-BRNO)
              GO TO EXIT-PROCESS-TRB-FILE.

           IF TB-DATE > ENDING-DATE
              GO TO EXIT-PROCESS-TRB-FILE.

           PERFORM TRB1-DETAIL.

           GO TO PROCESS-TRB-NEXT.

       EXIT-PROCESS-TRB-FILE.
           PERFORM CLOSE-TRB1-FILE.

      *****************************************
       PROCESS-TRJ-FILE SECTION.
      *****************************************
           PERFORM OPEN-TRJ1-FILE.

           MOVE BR-NO               TO TJ-BRNO
                                       QTRJ1-WBEG-BRNO
                                       QTRJ1-WEND-BRNO.

           MOVE BEGINNING-DATE      TO TJ-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRJ1-WBEG-DATE.
           MOVE ENDING-DATE         TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRJ1-WEND-DATE.

           PERFORM START-TRJ1-FILE.   *> SSP - TRJ

       PROCESS-TRJ-NEXT.
           PERFORM READ-TRJ1-FILE-NEXT.   *> SSP - TRJ
           IF (IO-FG NOT = 0) OR (TRJ-PATH-OWNBR NOT = TJ-BRNO)
              GO TO EXIT-PROCESS-TRJ-FILE.

           IF TJ-DATE > ENDING-DATE
              GO TO EXIT-PROCESS-TRJ-FILE.

           PERFORM TRJ1-DETAIL.

           GO TO PROCESS-TRJ-NEXT.

       EXIT-PROCESS-TRJ-FILE.
           PERFORM CLOSE-TRJ1-FILE.

      *****************************************
       PROCESS-TRK-FILE SECTION.
      *****************************************
           PERFORM OPEN-TRK1-FILE.

           MOVE BR-NO               TO TK-BRNO
                                       QTRK1-WBEG-BRNO
                                       QTRK1-WEND-BRNO.
 
           MOVE BEGINNING-DATE      TO TK-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRK1-WBEG-DATE.
           MOVE ENDING-DATE         TO  SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRK1-WEND-DATE.

           PERFORM START-TRK1-FILE.  *> SSP - TRK

       PROCESS-TRK-NEXT.
           PERFORM READ-TRK1-FILE-NEXT.  *> SSP - TRK
           IF (IO-FG NOT = 0) OR (TRK-PATH-OWNBR NOT = TK-BRNO)
              GO TO EXIT-PROCESS-TRK-FILE.

           IF TK-DATE > ENDING-DATE
              GO TO EXIT-PROCESS-TRK-FILE.

           PERFORM TRK1-DETAIL.

           GO TO PROCESS-TRK-NEXT.

       EXIT-PROCESS-TRK-FILE.
           PERFORM CLOSE-TRK1-FILE.

      *****************************************
       PROCESS-TRC-FILE SECTION.
      *****************************************
           PERFORM OPEN-TRC1-FILE.

           MOVE BR-NO       TO TC-BRNO
                               QTRC1-WBEG-BRNO
                               QTRC1-WEND-BRNO.

           MOVE BEGINNING-DATE TO TC-KEY-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRC1-WBEG-KEY-DATE
           MOVE ENDING-DATE TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRC1-WEND-KEY-DATE.

           PERFORM START-TRC1-FILE.   *> SSP - TRC

       PROCESS-TRC-NEXT.
           PERFORM READ-TRC1-FILE-NEXT.   *> SSP - TRC
           IF (IO-FG NOT = 0) OR (TRC-PATH-OWNBR NOT = TC-BRNO)
              GO TO EXIT-PROCESS-TRC-FILE.

           IF TC-KEY-DATE > ENDING-DATE
              GO TO EXIT-PROCESS-TRC-FILE.

           PERFORM TRC1-DETAIL.

           GO TO PROCESS-TRC-NEXT.

       EXIT-PROCESS-TRC-FILE.
           PERFORM CLOSE-TRC1-FILE.

      *****************************************
       PROCESS-TRD-FILE SECTION.
      *****************************************
           PERFORM OPEN-TRD1-FILE.

           MOVE BR-NO             TO TD-BRNO
                                     QTRD1-WBEG-BRNO
                                     QTRD1-WEND-BRNO.

           MOVE BEGINNING-DATE    TO TD-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QTRD1-WBEG-DATE.
           MOVE ENDING-DATE       TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QTRD1-WEND-DATE.

           PERFORM START-TRD1-FILE.   *> SSP - TRD

       PROCESS-TRD-NEXT.
           PERFORM READ-TRD1-FILE-NEXT.   *> SSP - TRD
           IF (IO-FG NOT = 0) OR (TRD-PATH-OWNBR NOT = TD-BRNO)
              GO TO EXIT-PROCESS-TRD-FILE.

           IF TD-DATE > ENDING-DATE
              GO TO EXIT-PROCESS-TRD-FILE.

           PERFORM TRD1-DETAIL.

           GO TO PROCESS-TRD-NEXT.

       EXIT-PROCESS-TRD-FILE.
           PERFORM CLOSE-TRD1-FILE.

      *******************************
       PRINT-RANGE-LINES SECTION.
      *******************************
           MOVE 3 TO LN-FEED.
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
           MOVE "MONTH-END DATE" TO RANGE-LINE.
           MOVE EDATE TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "DAY OF MONTH" TO RANGE-LINE.
           MOVE DAY-OF-MONTH TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "BEG GL BRANCH" TO RANGE-LINE.
           MOVE GLBR1 TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "END GL BRANCH" TO RANGE-LINE.
           MOVE GLBR2 TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "BEG GL ACCOUNT" TO RANGE-LINE.
           MOVE ACCOUNT1 TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "END GL ACCOUNT" TO RANGE-LINE.
           MOVE ACCOUNT2 TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "TRANSACTION TYPE" TO RANGE-LINE.
           MOVE TRANS-TYPE TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

       END-RANGE.
           EXIT.

      ****************************
       PRINT-GL-DETAIL SECTION.
      ****************************
           IF ED-TR-SEQNO = 0
              PERFORM HEAD-UP-GL-ACCT
              GO TO EXIT-PRINT-GL-DETAIL.
           IF ED-NO NOT = SAVE-ED-NO
              MOVE ED-NO TO SAVE-ED-NO
              MOVE ED-TR-DATE TO SAVE-ED-DATE.
            IF ED-TR-CODE NOT = "C"
               IF ED-CODE-ACCT = SAVE-ED-CODE-ACCT
                 IF ED-TR-SUBKEY = SAVE-ED-TR-SUBKEY
                    MOVE 1 TO LN-FEED
                    GO TO PRINT-GL-TRANS.
           MOVE ED-CODE-ACCT TO SAVE-ED-CODE-ACCT.
           MOVE ED-TR-SUBKEY TO SAVE-ED-TR-SUBKEY.
           MOVE ED-TR-ACCT TO D-TR-ACCT.

       PRINT-GL-TRANS.
           MOVE 1 TO LN-FEED.
           IF ED-TR-CODE NOT = "P"
              GO TO PRINT-GL-SFREG.
           MOVE ED-TR-TYPE TO D-TR-CODE2.
           IF ED-TR-TYPE = "PY"
              MOVE "PAYMENT" TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "PA"
              MOVE "ALLOTMENT PAYMENT" TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "PR"
              MOVE "RESERVE PAYMENT" TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "PE"
              MOVE "EXCEPTION PAYMENT" TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "IN"
              MOVE "INSURANCE LOSS" TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "DF"
              MOVE "DEFERMENT CHARGE" TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "D2"
              MOVE "MULTIPLE DEFERMENT" TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "PL"
              MOVE "CHARGE OFF" TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "AL"
              MOVE "ASSESS LATE CHARGE " TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "AI"
              MOVE "ASSESS INTEREST " TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "OT"
              MOVE "ASSESS OTHER CHARGE " TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "WL"
              MOVE "WAIVE LATE CHARGE " TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "WI"
              MOVE "WAIVE INTEREST " TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "WO"
              MOVE "WAIVE OTHER CHARGE " TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "JD"
              MOVE "JUDGEMENT " TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "PX"
              MOVE "PARTIAL CHARGE OFF " TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "PI"
              MOVE "P&L TO INACTIVE" TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "PW"
              MOVE "REMOVE P&L SYS" TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "SP"
              MOVE "SET POOL" TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "AC"
              MOVE "ACCELERATE ACCOUNT" TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "IR"
              MOVE "INTEREST REDUCTION" TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "DC"
              MOVE "DUE DATE CHANGE" TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "RN"
              MOVE "RENEWAL" TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "RB"
              MOVE "RENEWAL-BANKRUPT" TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "RO"
              MOVE "RENEWAL-OTHER" TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "SC"
              MOVE "SALES CONVERSION" TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "TS"
              MOVE "TRANSFER SERVICE" TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "TP"
              MOVE "TRANSFER PURCHASE" TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "PO"
              MOVE "PAYOFF" TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "PB"
              MOVE "RENEWAL (ADDON)" TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "RC"
              MOVE "REBATE CL INSURANCE" TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "RA"
              MOVE "REBATE AH INSURANCE" TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "RP"
              MOVE "REBATE PP INSURANCE" TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "R1"
              MOVE "REBATE O1 INSURANCE" TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "R2"
              MOVE "REBATE O2 INSURANCE" TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "RI"
              MOVE "REBATE INTEREST" TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "RM"
              MOVE "REBATE MAINT FEE" TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "RD"
              MOVE "REBATE DEFERMENT" TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "RS"
              MOVE "REBATE SERVICE CHARGE" TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "BK"
              MOVE "SET BANKRUPTCY" TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "BD"
              MOVE "REMOVE BANKRUPTCY" TO D-TR-DESC
           ELSE IF (ED-TR-TYPE = "SX" OR "ZX")
              MOVE "SET REPO" TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "SS"
              MOVE "SALE OF REPO" TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "SV"
              MOVE "SET REPO REDEEM" TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "PP"
              MOVE "PAY OUT PARTICIPATING RESERVE" TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "PC"
              MOVE "PAY OUT RESERVE CHARGES" TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "AD"
              MOVE "ALLOWABLE DELINQUENCY" TO D-TR-DESC
           ELSE IF ED-TR-TYPE = "Z2"
              MOVE "PAYMENT AGAINST O2" TO D-TR-DESC.
           IF DR-CR-FG = "C"
              MOVE ED-AMT TO D-G-CR
              ADD ED-AMT TO TOT-CR(1) TOT-CR(2) TOT-CR(3) TOT-CR(4)
           ELSE
              MOVE ED-AMT TO D-G-DR
              ADD ED-AMT TO TOT-DR(1) TOT-DR(2) TOT-DR(3) TOT-DR(4).
           PERFORM WRITE-DETAIL-LINE.
       PRINT-GL-SFREG.
           IF ED-TR-CODE NOT = "S"
              GO TO PRINT-GL-DLREG.
           MOVE "NEW SALE FIN LOAN, CLASS &&&" TO D-TRANS-DESC.
           INSPECT D-TRANS-DESC REPLACING FIRST "&&&" BY ED-TR-CLASS.
           IF DR-CR-FG = "C"
              MOVE ED-AMT TO D-G-CR
              ADD ED-AMT TO TOT-CR(1) TOT-CR(2) TOT-CR(3) TOT-CR(4)
           ELSE
              MOVE ED-AMT TO D-G-DR
              ADD ED-AMT TO TOT-DR(1) TOT-DR(2) TOT-DR(3) TOT-DR(4).
           PERFORM WRITE-DETAIL-LINE.

       PRINT-GL-DLREG.
           IF ED-TR-CODE NOT = "L"
              GO TO PRINT-GL-TFJRNL.
           MOVE "NEW DIRECT LOAN, CLASS &&&" TO D-TRANS-DESC.
           INSPECT D-TRANS-DESC REPLACING FIRST "&&&" BY ED-TR-CLASS.
           IF DR-CR-FG = "C"
              MOVE ED-AMT TO D-G-CR
              ADD ED-AMT TO TOT-CR(1) TOT-CR(2) TOT-CR(3) TOT-CR(4)
           ELSE
              MOVE ED-AMT TO D-G-DR
              ADD ED-AMT TO TOT-DR(1) TOT-DR(2) TOT-DR(3) TOT-DR(4).
           PERFORM WRITE-DETAIL-LINE.

       PRINT-GL-TFJRNL.
           IF ED-TR-CODE NOT = "X"
              GO TO PRINT-GL-CHKS.
      *    MOVE "SERV/PUR TRANSFER JOURNAL" TO D-SOURCE-DESC.
           IF DR-CR-FG = "C"
              MOVE ED-AMT TO D-G-CR
              ADD ED-AMT TO TOT-CR(1) TOT-CR(2) TOT-CR(3) TOT-CR(4)
           ELSE
              MOVE ED-AMT TO D-G-DR
              ADD ED-AMT TO TOT-DR(1) TOT-DR(2) TOT-DR(3) TOT-DR(4).
           PERFORM WRITE-DETAIL-LINE.

       PRINT-GL-CHKS.
           IF ED-TR-CODE NOT = "C"
              GO TO PRINT-GL-DEP.
           MOVE ED-TR-TYPE TO D-TR-CODE2.
           IF ED-DESC NOT = SPACES
              MOVE ED-DESC TO D-TRANS-DESC
           ELSE
           IF ED-TR-TYPE = "CA"
              IF ED-AMT < 0
                 MOVE "RECEIPT" TO D-TRANS-DESC
              ELSE
                 MOVE "DISBURSE" TO D-TRANS-DESC
              END-IF
           ELSE IF ED-TR-TYPE = "RR"
              MOVE "RESERVE FROM PRINCIPAL" TO D-TRANS-DESC
           ELSE IF ED-TR-TYPE = "RC"
              MOVE "RESERVE FROM CHARGES" TO D-TRANS-DESC
           ELSE IF ED-TR-TYPE = "RP"
              MOVE "PARTICIPATING RESERVE" TO D-TRANS-DESC
           ELSE
              MOVE "BANK" TO D-TRANS-DESC.

           IF DR-CR-FG = "C"
              MOVE ED-AMT TO D-G-CR
              ADD ED-AMT TO TOT-CR(1) TOT-CR(2) TOT-CR(3) TOT-CR(4)
           ELSE
              MOVE ED-AMT TO D-G-DR
              ADD ED-AMT TO TOT-DR(1) TOT-DR(2) TOT-DR(3) TOT-DR(4).
           PERFORM WRITE-DETAIL-LINE.

       PRINT-GL-DEP.
           IF ED-TR-CODE NOT = "D"
              GO TO PRINT-GL-BRANCH-TRANSFER.
           MOVE "DEPOSIT" TO D-TRANS-DESC.
           IF DR-CR-FG = "C"
              MOVE ED-AMT TO D-G-CR
              ADD ED-AMT TO TOT-CR(1) TOT-CR(2) TOT-CR(3) TOT-CR(4)
           ELSE
              MOVE ED-AMT TO D-G-DR
              ADD ED-AMT TO TOT-DR(1) TOT-DR(2) TOT-DR(3) TOT-DR(4).
           PERFORM WRITE-DETAIL-LINE.

       PRINT-GL-BRANCH-TRANSFER.
           IF ED-TR-CODE NOT = "B"
              GO TO PRINT-GL-CLASS-TRANSFER.
      *    MOVE "BRANCH TRANSFER JOURNAL" TO D-SOURCE-DESC.
           IF DR-CR-FG = "C"
              MOVE ED-AMT TO D-G-CR
              ADD ED-AMT TO TOT-CR(1) TOT-CR(2) TOT-CR(3) TOT-CR(4)
           ELSE
              MOVE ED-AMT TO D-G-DR
              ADD ED-AMT TO TOT-DR(1) TOT-DR(2) TOT-DR(3) TOT-DR(4).
           PERFORM WRITE-DETAIL-LINE.

       PRINT-GL-CLASS-TRANSFER.
           IF ED-TR-CODE NOT = "J"
              GO TO PRINT-GL-ACCT-CONV.
      *    MOVE "CLASS TRANSFER JOURNAL" TO D-SOURCE-DESC.
           IF DR-CR-FG = "C"
              MOVE ED-AMT TO D-G-CR
              ADD ED-AMT TO TOT-CR(1) TOT-CR(2) TOT-CR(3) TOT-CR(4)
           ELSE
              MOVE ED-AMT TO D-G-DR
              ADD ED-AMT TO TOT-DR(1) TOT-DR(2) TOT-DR(3) TOT-DR(4).
           PERFORM WRITE-DETAIL-LINE.

       PRINT-GL-ACCT-CONV.
           IF ED-TR-CODE NOT = "K"
              GO TO EXIT-PRINT-GL-DETAIL.
           IF GP-LOAN-CONVERSION-UPD-FG NOT = "Y"
              IF GP-LOAN-CONVERSION-UPD-FG NOT = "G"
                 GO TO EXIT-PRINT-GL-DETAIL.
      *    MOVE "ACCOUNT CONVERSION JOURNAL" TO D-SOURCE-DESC.
           IF DR-CR-FG = "C"
              MOVE ED-AMT TO D-G-CR
              ADD ED-AMT TO TOT-CR(1) TOT-CR(2) TOT-CR(3) TOT-CR(4)
           ELSE
              MOVE ED-AMT TO D-G-DR
              ADD ED-AMT TO TOT-DR(1) TOT-DR(2) TOT-DR(3) TOT-DR(4).
           PERFORM WRITE-DETAIL-LINE.

       EXIT-PRINT-GL-DETAIL.
           EXIT.

      ****************************
       PRINT-TOTALS SECTION.
      ****************************

           IF TOT-DR(LEVEL) = 0
              IF TOT-CR(LEVEL) = 0
                 GO TO PRINT-GL-TOTS-EXIT.
           IF LEVEL = 1
              MOVE "DATE TOTALS :" TO DETAIL-LINE
           ELSE IF LEVEL = 2
              MOVE "ACCOUNT TOTALS :" TO DETAIL-LINE
           ELSE IF LEVEL = 3
              MOVE "BRANCH TOTALS :" TO DETAIL-LINE
           ELSE
              MOVE "GRAND TOTALS :" TO DETAIL-LINE
           MOVE TOT-DR(LEVEL) TO D-G-DR.
           MOVE TOT-CR(LEVEL) TO D-G-CR.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 0 TO TOT-DR(LEVEL) TOT-CR(LEVEL).

       PRINT-GL-TOTS-EXIT.
           EXIT.

      ***********************************
       UPDATE-WORK-FILE SECTION.
      ***********************************

           IF GLAMT = 0
              GO TO EXIT-UPDATE-WORK-FILE.

           IF GLNO-BRANCH < GLBR1 OR GLNO-BRANCH > GLBR2
              GO TO EXIT-UPDATE-WORK-FILE.

           IF GLNO-ACCT < ACCOUNT1 OR GLNO-ACCT > ACCOUNT2
              GO TO EXIT-UPDATE-WORK-FILE.

           MOVE 1 TO SAVE-ED-TR-SEQNO.
       FIND-UNIQUE-ED-KEY.
           MOVE SPACES TO ED-KEY.

           MOVE GLNO          TO ED-NO.
           MOVE TR-DATE       TO ED-TR-DATE.
           MOVE TR-CODE       TO ED-TR-CODE.
           MOVE TR-ACCT       TO ED-TR-ACCT.
           MOVE TR-SUBKEY     TO ED-TR-SUBKEY.
           MOVE SAVE-ED-TR-SEQNO TO ED-TR-SEQNO.

           PERFORM READ-ED1-FILE.
           IF IO-FG = 0
              IF SAVE-ED-TR-SEQNO < 999
                 ADD 1 TO SAVE-ED-TR-SEQNO
                 GO TO FIND-UNIQUE-ED-KEY
              ELSE
                 GO TO SKIP-CREATE-WK.

           IF IO-FG = 9
              GO TO CREATE-WK.

       SKIP-CREATE-WK.
           IF TR-CODE = "P"
              ADD GLAMT TO ED-AMT
           ELSE
           IF TR-CODE = "S"
              ADD GLAMT TO ED-AMT
           ELSE
              IF TR-CODE = "L"
                 ADD GLAMT TO ED-AMT
              ELSE
              IF TR-CODE = "X"
                 ADD GLAMT TO ED-AMT
              ELSE
              IF TR-CODE = "C"
                 ADD GLAMT TO ED-AMT
              ELSE
              IF TR-CODE = "D"
                 ADD GLAMT TO ED-AMT
              ELSE
              IF TR-CODE = "B"
                 ADD GLAMT TO ED-AMT
              ELSE
              IF TR-CODE = "J"
                 ADD GLAMT TO ED-AMT
              ELSE
              IF TR-CODE = "K" AND
                       (GP-LOAN-CONVERSION-UPD-FG = "Y" OR "G")
                 ADD GLAMT TO ED-AMT.

           PERFORM REWRITE-ED1-FILE.

           GO TO UPDATE-CONTROL-RECORD.

       CREATE-WK.
           MOVE SPACES TO ED-REC.

           MOVE GLNO          TO ED-NO.
           MOVE TR-DATE       TO ED-TR-DATE.
           MOVE TR-CODE       TO ED-TR-CODE.
           MOVE TR-ACCT       TO ED-TR-ACCT.
           MOVE TR-SUBKEY     TO ED-TR-SUBKEY.
           MOVE SAVE-ED-TR-SEQNO TO ED-TR-SEQNO.

           IF TR-CODE = "P" 
              MOVE TR-TYPE-TRCD TO ED-TR-TYPE
           ELSE 
           IF TR-CODE = "C"
              MOVE TR-TYPE-TRCD TO ED-TR-TYPE.
      *A15 HAS SPACES IN ED-DESC, NEED IT TO MATCH
      *        MOVE TR-REASON    TO ED-DESC.

           MOVE 0        TO ED-AMT.
           MOVE TR-CLASS TO ED-TR-CLASS.

           IF TR-CODE = "P" OR "S" OR "L" OR "X" OR "C" OR
                         "D" OR "B" OR "J"
              ADD GLAMT TO ED-AMT
           ELSE 
           IF (TR-CODE = "K" AND GP-LOAN-CONVERSION-UPD-FG = "Y" OR "G")
               ADD GLAMT TO ED-AMT.

           PERFORM WRITE-ED1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

       UPDATE-CONTROL-RECORD.
           MOVE 0 TO ED-TR-ACCT ED-TR-SEQNO.
           MOVE LOW-VALUES TO ED-TR-SUBKEY.
           PERFORM READ-ED1-FILE.
           IF IO-FG = 0
              ADD GLAMT TO ED-AMT
              PERFORM REWRITE-ED1-FILE
              GO TO EXIT-UPDATE-WORK-FILE.

           MOVE SPACES TO ED-REC.

           MOVE GLNO         TO ED-NO.
           MOVE TR-DATE      TO ED-TR-DATE.
           MOVE LOW-VALUES   TO ED-TR-SUBKEY.
           MOVE TR-CODE      TO ED-TR-CODE.
           MOVE 0            TO ED-TR-ACCT ED-TR-SEQNO.
           MOVE GLAMT        TO ED-AMT.
           MOVE TR-TYPE-TRCD TO ED-TR-TYPE.
           PERFORM WRITE-ED1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

       EXIT-UPDATE-WORK-FILE.
           MOVE 0 TO GLAMT GLNO.

      ****************************
       HEAD-UP-GL-ACCT SECTION.
      ****************************
           MOVE ED-NO      TO GL-BR-ACCOUNT.
           MOVE GL-BRANCH  TO D-G-BRNO.
           MOVE "-"        TO D-G-HYPHEN.
           MOVE GL-ACCOUNT TO D-G-GLNO.
           PERFORM OPEN-GL1-FILE.
           PERFORM READ-GL1-FILE.
           IF IO-FG = 9
              MOVE "* *  N O T   F O U N D  * *" TO GL-DESCRIPTION.
           PERFORM CLOSE-GL1-FILE.

           MOVE GL-DESCRIPTION TO D-G-DESCRIPTION.
           MOVE ED-TR-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-TR-DATE.
           PERFORM GET-SOURCE-DESC.
           IF ED-AMT < 0
              MOVE ED-AMT TO D-STAR-CR
              MOVE "C" TO DR-CR-FG
           ELSE
              MOVE ED-AMT TO D-STAR-DR
              MOVE "D" TO DR-CR-FG.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      ****************************
       GET-SOURCE-DESC SECTION.
      ****************************
           IF ED-TR-CODE = "P"
              MOVE "TRANSACTION JOURNAL" TO D-SOURCE-DESC
           ELSE IF ED-TR-CODE = "S"
              MOVE "SALES FINANCE REGISTER" TO D-SOURCE-DESC
           ELSE IF ED-TR-CODE = "L"
              MOVE "DIRECT LOAN REGISTER" TO D-SOURCE-DESC
           ELSE IF ED-TR-CODE = "X"
              MOVE "SERV/PUR TRANSFER JOURNAL" TO D-SOURCE-DESC
           ELSE IF ED-TR-CODE = "C"
              PERFORM TC-SOURCE-DESC                   
           ELSE IF ED-TR-CODE = "D"
              MOVE "DEPOSIT" TO D-SOURCE-DESC
           ELSE IF ED-TR-CODE = "B"
              MOVE "BRANCH TRANSFER JOURNAL" TO D-SOURCE-DESC
           ELSE IF ED-TR-CODE = "J"
              MOVE "CLASS TRANSFER JOURNAL" TO D-SOURCE-DESC
           ELSE IF ED-TR-CODE = "K"
              MOVE "ACCOUNT CONVERSION JOURNAL" TO D-SOURCE-DESC
           ELSE
              MOVE ED-TR-CODE TO D-SOURCE-DESC.

      ****************************
       WRITE-DETAIL-LINE SECTION.
      ****************************

           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE " " TO DETAIL-LINE.

      *******************************
       HEAD-PAGE SECTION.
      *******************************

           MOVE "N" TO FIRST-TIME.
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
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 5 TO LN-COUNT.
           MOVE 2 TO LN-FEED.


       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/GPENV.CPY".

      *****************************************************
       FORM-RESET SECTION.
      *****************************************************
           MOVE EXT-CONAME-CONAME TO GLDIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GLDIST-SYSDATE.
           DISPLAY GLDIST-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE GLDIST-FORM-FIELDS TO WR-BUF.
           MOVE GLDIST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *****************************************************
       SETUP-LINK-INFO SECTION.
      *****************************************************
           MOVE GLDIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE GLDIST-HELP--DIR TO HELP-LINK-DIR.

      *****************************************************
       VDU-CONVERT-FIELD SECTION.
      *****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/GLDIST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.


      ******************************************************************
       OPEN-MISC-CONNECTION SECTION.
      ******************************************************************

           IF MISC-CONN-OPEN-SW = "N" 
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS MISC_CONNECT
                    USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               MOVE "Y" TO MISC-CONN-OPEN-SW
               EXEC SQL SET CONNECTION C1 END-EXEC
               PERFORM SQL-IO-VALIDATION.

      ******************************************************************
       CLOSE-MISC-CONNECTION SECTION.
      ******************************************************************

           IF MISC-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT MISC_CONNECT END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE "N" TO MISC-CONN-OPEN-SW.

      **************************************
       MISC-PARA SECTION.
      **************************************
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETPR.CPY".
      *  NOTE: CHECK-UPDATE-BR & TD-UPD-BR ARE DUMMY PARAGRAPHS
      *        BECAUSE COPY MEMBER LPEOD5TR PERFORMS THEM WHEN
      *        UPDATING.
       CHECK-UPDATE-BR.
           MOVE SPACES TO MESS.
       TD-UPD-BR.
           MOVE SPACES TO MESS.
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
      *TR1-CODE-C.
      *    IF ((GP-I-AM-WORLD) AND
      *       (ED-TR-TYPE = "BK" OR "ID" OR "IW"))
      *         MOVE "DEPOSIT" TO ED-DESC
      *    ELSE
      *         MOVE TC-REASON TO ED-DESC.
       TC-SOURCE-DESC.
           IF ED-TR-TYPE = "BK" OR "ID" OR "IW"
              MOVE "DEPOSIT" TO D-SOURCE-DESC
           ELSE
              MOVE "CHECKS" TO D-SOURCE-DESC.
       GET-CODE.
           MOVE "GC"   TO CDB-TYPE.
      *--> SET BEFORE     CDB-BRANCH.
      *--> SET BEFORE     CDB-CODE.
           PERFORM READ-CDB1-FILE.
       TEST-BR-FEE-GL-BRNO.
           IF BR-FEE-GL-BRNO NUMERIC
              IF BR-FEE-GL-BRNO NOT = 0
                 MOVE BR-FEE-GL-BRNO TO GLNO-BR.
       PROGRESS-MSG.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       CREATE-LOG.
           MOVE SPACES TO MESS.
       SAVE-ERROR-MSG.
           MOVE SPACES TO MESS.
       CONVERT-TIME.
           IF H-AND-M NOT GREATER THAN 1159
              MOVE "AM" TO T-AMPM
           ELSE
              MOVE "PM" TO T-AMPM.
           IF H-AND-M IS NOT LESS THAN 1300
              SUBTRACT 1200 FROM H-AND-M.
           IF CUR-HH = 0
              MOVE 12 TO CUR-HH.
           MOVE CUR-HH TO T-HH.
           MOVE CUR-MM TO T-MM.
           MOVE ":" TO T-COL.
           MOVE " " TO T-SPACE.

       COPY "LIBLP/LPEOD5TR.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/ARRAYBR.CPY".
       COPY "LIBLP/ARRAYLC.CPY".
       COPY "LIBGL/ARRAYGI.CPY".

      ****************************************
       IO-ROUTINES SECTION.
      ****************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRXGS_SQL.CPY".
       COPY "LIBLP/LPTRSGS_SQL.CPY".
       COPY "LIBLP/LPTRPGS_SQL.CPY".
       COPY "LIBLP/LPTRLGS_SQL.CPY".
       COPY "LIBLP/LPTRKGS_SQL.CPY".
       COPY "LIBLP/LPTRJGS_SQL.CPY".
       COPY "LIBLP/LPTRDGS_SQL.CPY".
       COPY "LIBLP/LPTRCGS_SQL.CPY".
       COPY "LIBLP/LPTRBGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGL/GLGIGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPPRINT.CPY".

       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPED1IN.CPY".
       COPY "LIBGL/GLGL1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBGL/GLGI1RN.CPY".

       COPY "LIBGB/GBBRRN.CPY".
      *-----------------------------------------------------------------
       START-BR2-FILE.

           IF BR2-CONN-OPEN-SW = "N" 
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS BR2_CONNECT
                    USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               MOVE "Y" TO BR2-CONN-OPEN-SW
               EXEC SQL SET CONNECTION C1 END-EXEC
               PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION BR2_CONNECT END-EXEC.

           EXEC SQL
            DECLARE BR2_CURSOR CURSOR FOR
                :BR1-RETURN-CODE = 
                 EXEC SSP_GLDIST_TRFILE_SELECT
                  ( :PASS-BRANCHLIST,
                    :PASS-WBEG-DATE,  :PASS-WEND-DATE)
                END-EXEC.

           EXEC SQL
               OPEN BR2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE STAT---GOOD TO BR2-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-BR2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BR-PATH-SQL TO E-FILE.
           MOVE BR-NO TO E-KEYX.
           INITIALIZE QBR-REC.

           EXEC SQL SET CONNECTION BR2_CONNECT END-EXEC.

           IF ( BR2-CURSOR-STAT-GOOD )
              EXEC SQL
                  FETCH BR2_CURSOR INTO 
                   :QBR-NO,
                   :WS-TRBSW,
                   :WS-TRCSW,
                   :WS-TRDSW,
                   :WS-TRJSW,
                   :WS-TRKSW,
                   :WS-TRLSW,
                   :WS-TRPSW,
                   :WS-TRSSW,
                   :WS-TRXSW
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO READ-BR2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BR-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-BR2-FILE.

           IF ( BR2-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION BR2_CONNECT END-EXEC
              EXEC SQL
                   CLOSE BR2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO BR2-CURSOR-STAT
              EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-BR2-CONNECTION.

           IF BR2-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION BR2_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT BR2_CONNECT END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE "N" TO BR2-CONN-OPEN-SW.


      *-----------------------------------------------------------------
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
              MOVE TRB-ALL-PATH            TO TRB-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRB-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRB-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRB-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRB-ALL-DATA
              MOVE TRB-ALL-PATH            TO TRB-PATH.

      *-----------------------------------------------------------------
       OPEN-TRB1-FILE.
           PERFORM LOAD-TRB1-FILE.
           PERFORM OPEN-IT.
           MOVE TRB-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRB1-FILE.   *> SSP - TRB 
           PERFORM START-IT.
           MOVE TRB-PATH-OWNBR  TO TB-BRNO.
           MOVE TRB-PATH-SQL    TO E-FILE.
           MOVE TRB1-KEY        TO E-KEYX.

           INITIALIZE WS-SELECT WS-WHERE WS-ORDERBY PASS-SQL.

           IF ( TRB1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRB1-FILE.

           STRING "SELECT TRBFILE.TB_BRNO," 
                  "CAST(TRBFILE.TB_DATE AS VARCHAR(10)),"
                  "TRBFILE.TB_CLASS,"
                  "TRBFILE.TB_ACCTNO,"
                  "TRBFILE.TB_LAWCODE,"
                  "TRBFILE.TB_LINE,"
                  "TRBFILE.TB_REF_FRTO," 
                  "TRBFILE.TB_REFBR,"
                  "TRBFILE.TB_SERVACCT," 
                  "TRBFILE.TB_PURBAL,"
                  "TRBFILE.TB_UNEARN_1,"
                  "TRBFILE.TB_UNEARN_2,"
                  "TRBFILE.TB_UNEARN_3,"
                  "TRBFILE.TB_UNEARN_4,"
                  "TRBFILE.TB_UNEARN_5,"
                  "TRBFILE.TB_UNEARN_6,"
                  "TRBFILE.TB_UNEARN_7,"
                  "TRBFILE.TB_UNEARN_8,"
                  "TRBFILE.TB_UNEARN_9,"
                  "TRBFILE.TB_UNEARN_10,"
                  "TRBFILE.TB_REF_ACCTNO,"
                  "TRBFILE.TB_PLCD,"
                  "TRBFILE.TB_PLTYPE," 
                  "TRBFILE.TB_REPOCD,"
                  "TRBFILE.TB_ACTIONCD2"
                  " FROM DBO.TRBFILE | " INTO WS-SELECT.

           STRING " WHERE TRBFILE.TB_BRNO = ",  
                   QTRB1-WBEG-BRNO ,  
                  " AND TRBFILE.TB_DATE BETWEEN '"  ,
                   QTRB1-WBEG-DATE  ,
                  " ' AND '" ,
                   QTRB1-WEND-DATE ,
                  "' | " INTO WS-WHERE.

           STRING " ORDER BY TRBFILE.TB_BRNO,"
                  " TRBFILE.TB_DATE,"
                  " TRBFILE.TB_CLASS,"
                  " TRBFILE.TB_ACCTNO,"
                  " TRBFILE.TB_LAWCODE,"
                  " TRBFILE.TB_LINE |" INTO WS-ORDERBY.
 
           STRING WS-SELECT DELIMITED BY "|",
                  WS-WHERE  DELIMITED BY "|",
                  WS-ORDERBY DELIMITED BY "|" INTO PASS-SQL.

           INSPECT PASS-SQL REPLACING ALL "|" BY SPACE.

           EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC.

           EXEC SQL
            DECLARE TRB1_CURSOR CURSOR FOR
                :TRB1-RETURN-CODE = 
                 EXEC SSP_TCLP_SELECT
                  ( :PASS-SQL)
                END-EXEC.

           EXEC SQL
               OPEN TRB1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE STAT---GOOD TO TRB1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-TRB1-FILE-NEXT.  *> SSP - TRB 
           PERFORM READ-IT.
           MOVE TRB-PATH-OWNBR  TO TB-BRNO.
           MOVE TRB-PATH-SQL    TO E-FILE.
           MOVE TRB1-KEY        TO E-KEYX.
           INITIALIZE QTRB-REC.

           EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC.

           IF ( TRB1-CURSOR-STAT-GOOD )
              EXEC SQL
                  FETCH TRB1_CURSOR INTO  
                   :QTB-BRNO,
                   :QTB-DATE,
                   :QTB-CLASS,
                   :QTB-ACCTNO,
                   :QTB-LAWCODE,
                   :QTB-LINE,
                   :QTB-REF-FRTO, 
                   :QTB-REFBR,
                   :QTB-SERVACCT, 
                   :QTB-PURBAL,
                   :QTB-UNEARN-1,
                   :QTB-UNEARN-2,
                   :QTB-UNEARN-3,
                   :QTB-UNEARN-4,
                   :QTB-UNEARN-5,
                   :QTB-UNEARN-6,
                   :QTB-UNEARN-7,
                   :QTB-UNEARN-8,
                   :QTB-UNEARN-9,
                   :QTB-UNEARN-10,
                   :QTB-REF-ACCTNO,
                   :QTB-PLCD, 
                   :QTB-PLTYPE, 
                   :QTB-REPOCD, 
                   :QTB-ACTIONCD2
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRB1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRB-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TRB1-FILE.

           PERFORM CLOSE-IT.
           IF ( TRB1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC
              EXEC SQL
                   CLOSE TRB1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRB1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------

      * COPY "LIBLP/LPTRC1RN.CPY".
      * COPYMEMBER: LIBLP/LPTR1RN
       LOAD-TRC1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRC-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRC-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRC-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRC-ALL-DATA
              MOVE TRC-ALL-PATH             TO TRC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRC-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRC-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRC-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRC-ALL-DATA
              MOVE TRC-ALL-PATH            TO TRC-PATH.

      *-----------------------------------------------------------------
       OPEN-TRC1-FILE.
           PERFORM LOAD-TRC1-FILE.
           PERFORM OPEN-IT.
           MOVE TRC-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRC1-FILE.  *> SSP - TRC
           PERFORM START-IT.
           MOVE TRC-PATH-OWNBR  TO TC-BRNO.
           MOVE TRC-PATH-SQL    TO E-FILE.
           MOVE TRC1-KEY        TO E-KEYX.

           INITIALIZE WS-SELECT WS-WHERE WS-ORDERBY PASS-SQL.

           IF ( TRC1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRC1-FILE.

           STRING "SELECT TRCFILE.TC_BRNO,"
                  " CAST(TRCFILE.TC_KEY_DATE AS VARCHAR(10)),"
                  " TRCFILE.TC_CKNO,"
                  " TRCFILE.TC_CLASS,"
                  " TRCFILE.TC_SEQ,"
                  " TRCFILE.TC_TYPE,"
                  " TRCFILE.TC_AMT,"
                  " TRCFILE.TC_REASON,"
                  " TRCFILE.TC_GLNO_1,"
                  " TRCFILE.TC_GLNO_2,"
                  " TRCFILE.TC_GLNO_3,"
                  " TRCFILE.TC_GLNO_4,"
                  " TRCFILE.TC_GLNO_5,"
                  " TRCFILE.TC_GLAMT_1,"
                  " TRCFILE.TC_GLAMT_2,"
                  " TRCFILE.TC_GLAMT_3,"
                  " TRCFILE.TC_GLAMT_4,"
                  " TRCFILE.TC_GLAMT_5,"
                  " TRCFILE.TC_PGM," 
                  " TRCFILE.TC_ACH_FG," 
                  " TRCFILE.TC_DEBIT_REFCD"
                  " FROM DBO.TRCFILE | " INTO WS-SELECT.

           STRING "WHERE TRCFILE.TC_BRNO = ",
                    QTRC1-WBEG-BRNO
                   " AND TRCFILE.TC_KEY_DATE BETWEEN '",
                    QTRC1-WBEG-KEY-DATE
                   " ' AND '",
                    QTRC1-WEND-KEY-DATE,
                   "' | " INTO WS-WHERE.

           STRING " ORDER BY TRCFILE.TC_BRNO,"
                  " TRCFILE.TC_KEY_DATE,"
                  " TRCFILE.TC_CKNO,"
                  " TRCFILE.TC_CLASS,"
                  " TRCFILE.TC_SEQ |" INTO WS-ORDERBY.

           STRING WS-SELECT DELIMITED BY "|",
                  WS-WHERE  DELIMITED BY "|",
                  WS-ORDERBY DELIMITED BY "|" INTO PASS-SQL.

           EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC.

           EXEC SQL
            DECLARE TRC1_CURSOR CURSOR FOR
                :TRC1-RETURN-CODE = 
                 EXEC SSP_TCLP_SELECT
                  ( :PASS-SQL)
                END-EXEC.

           EXEC SQL
               OPEN TRC1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRC1-WBEG-BRNO.
           MOVE SPACES TO QTRC1-WBEG-KEY-DATE
                          QTRC1-WEND-KEY-DATE.

           MOVE STAT---GOOD TO TRC1-CURSOR-STAT.
 
           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-TRC1-FILE-NEXT.  *> SSP - TRC
           PERFORM READ-IT.
           MOVE TRC-PATH-OWNBR  TO TC-BRNO.
           MOVE TRC-PATH-SQL    TO E-FILE.
           MOVE TRC1-KEY        TO E-KEYX.
           INITIALIZE QTRC-REC.

           EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC.

           IF ( TRC1-CURSOR-STAT-GOOD )
              EXEC SQL
                 FETCH TRC1_CURSOR INTO  
                   :QTC-BRNO,
                   :QTC-KEY-DATE,
                   :QTC-CKNO,
                   :QTC-CLASS,
                   :QTC-SEQ,
                   :QTC-TYPE,
                   :QTC-AMT,
                   :QTC-REASON,
                   :QTC-GLNO-1,
                   :QTC-GLNO-2,
                   :QTC-GLNO-3,
                   :QTC-GLNO-4,
                   :QTC-GLNO-5,
                   :QTC-GLAMT-1,
                   :QTC-GLAMT-2,
                   :QTC-GLAMT-3,
                   :QTC-GLAMT-4,
                   :QTC-GLAMT-5,
                   :QTC-PGM,
                   :QTC-ACH-FG,
                   :QTC-DEBIT-REFCD
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRC1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRC-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TRC1-FILE.

           PERFORM CLOSE-IT.

           IF ( TRC1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC
              EXEC SQL
                   CLOSE TRC1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRC1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.


      * COPY "LIBLP/LPTRD1RN.CPY".
      * COPYMEMBER: LIBLP/LPTRD1RN
       LOAD-TRD1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRD-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRD-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRD-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRD-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRD-ALL-DATA
              MOVE TRD-ALL-PATH             TO TRD-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRD-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRD-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRD-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRD-ALL-DATA
              MOVE TRD-ALL-PATH             TO TRD-PATH.

      *-----------------------------------------------------------------
       OPEN-TRD1-FILE.
           PERFORM LOAD-TRD1-FILE.
           PERFORM OPEN-IT.
           MOVE TRD-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRD1-FILE.   *> SSP - TRD
           PERFORM START-IT.
           MOVE TRD-PATH-OWNBR  TO TD-BRNO.
           MOVE TRD-PATH-SQL TO E-FILE.
           MOVE TRD1-KEY        TO E-KEYX.

           INITIALIZE WS-SELECT WS-WHERE WS-ORDERBY PASS-SQL.

           IF ( TRD1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRD1-FILE.

           STRING "SELECT TRDFILE.TD_BRNO,"
                   "CAST(TRDFILE.TD_DATE AS VARCHAR(10)), "
                   "TRDFILE.TD_DEPOSIT,"
                   "TRDFILE.TD_CHECK_DEPOSIT,"
                   "TRDFILE.TD_BANK,"
                   "TRDFILE.TD_CASH,"
                   "TRDFILE.TD_GLOVERSHORT,"
                   "TRDFILE.TD_OVERSHORT"
                   " FROM DBO.TRDFILE | " INTO WS-SELECT.

           STRING " WHERE TRDFILE.TD_BRNO = ",
                   QTRD1-WBEG-BRNO
                  " AND TRDFILE.TD_DATE BETWEEN '",
                   QTRD1-WBEG-DATE 
                  " ' AND '"
                   QTRD1-WEND-DATE
                  "' | " INTO WS-WHERE.
            
           STRING " ORDER BY TRDFILE.TD_BRNO,"
                  " TRDFILE.TD_DATE |" INTO WS-ORDERBY.

           STRING WS-SELECT DELIMITED BY "|",
                  WS-WHERE  DELIMITED BY "|",
                  WS-ORDERBY DELIMITED BY "|" INTO PASS-SQL.

           EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC.

           EXEC SQL
            DECLARE TRD1_CURSOR CURSOR FOR
                :TRD1-RETURN-CODE = 
                 EXEC SSP_TCLP_SELECT
                  ( :PASS-SQL)
                END-EXEC.

           EXEC SQL
               OPEN TRD1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRD1-WBEG-BRNO.
           MOVE SPACES TO QTRD1-WBEG-DATE
                          QTRD1-WEND-DATE.

           MOVE STAT---GOOD TO TRD1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-TRD1-FILE-NEXT.   *> SSP - TRD
           PERFORM READ-IT.
           MOVE TRD-PATH-OWNBR  TO TD-BRNO.
           MOVE TRD-PATH-SQL    TO E-FILE.
           MOVE TRD1-KEY        TO E-KEYX.
           INITIALIZE QTRD-REC.

           EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC.

           IF ( TRD1-CURSOR-STAT-GOOD )
              EXEC SQL
                 FETCH TRD1_CURSOR INTO  
                   :QTD-BRNO,
                   :QTD-DATE,
                   :QTD-DEPOSIT,
                   :QTD-CHECK-DEPOSIT,
                   :QTD-BANK,
                   :QTD-CASH,
                   :QTD-GLOVERSHORT,
                   :QTD-OVERSHORT
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRD1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRD-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TRD1-FILE.

           PERFORM CLOSE-IT.

           IF ( TRD1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC
              EXEC SQL
                   CLOSE TRD1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRD1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      * COPY "LIBLP/LPTRJ1RN.CPY".
      * COPYMEMBER: LIBLP/LPTRJ1RN
       LOAD-TRJ1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRJ-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRJ-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRJ-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRJ-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRJ-ALL-DATA
              MOVE TRJ-ALL-PATH             TO TRJ-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRJ-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRJ-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRJ-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRJ-ALL-DATA
              MOVE TRJ-ALL-PATH             TO TRJ-PATH.

      *-----------------------------------------------------------------
       OPEN-TRJ1-FILE.
           PERFORM LOAD-TRJ1-FILE.
           PERFORM OPEN-IT.
           MOVE TRJ-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRJ1-FILE.   *> SSP - TRJ
           PERFORM START-IT.
           MOVE TRJ-PATH-OWNBR  TO TJ-BRNO.
           MOVE TRJ-PATH-SQL    TO E-FILE.
           MOVE TRJ1-KEY        TO E-KEYX.

           INITIALIZE WS-SELECT WS-WHERE WS-ORDERBY PASS-SQL.

           IF ( TRJ1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRJ1-FILE.

           STRING "SELECT TRJFILE.TJ_BRNO,"
                   "CAST(TRJFILE.TJ_DATE AS VARCHAR(10))," 
                   "TRJFILE.TJ_CLASS,"
                   "TRJFILE.TJ_ACCTNO,"
                   "TRJFILE.TJ_LAWCODE,"
                   "TRJFILE.TJ_LINE,"
                   "TRJFILE.TJ_SERVACCT," 
                   "TRJFILE.TJ_PURBAL,"
                   "TRJFILE.TJ_UNEARN_1,"
                   "TRJFILE.TJ_UNEARN_2,"
                   "TRJFILE.TJ_UNEARN_3,"
                   "TRJFILE.TJ_UNEARN_4,"
                   "TRJFILE.TJ_UNEARN_5,"
                   "TRJFILE.TJ_UNEARN_6,"
                   "TRJFILE.TJ_UNEARN_7,"
                   "TRJFILE.TJ_UNEARN_8,"
                   "TRJFILE.TJ_UNEARN_9,"
                   "TRJFILE.TJ_UNEARN_10,"
                   "TRJFILE.TJ_PLCD,"
                   "TRJFILE.TJ_PLTYPE," 
                   "TRJFILE.TJ_REPOCD"
                   " FROM DBO.TRJFILE | " INTO WS-SELECT.

           STRING " WHERE TRJFILE.TJ_BRNO = ",
                    QTRJ1-WBEG-BRNO
                  "AND TRJFILE.TJ_DATE BETWEEN '"
                    QTRJ1-WBEG-DATE
                   "' AND '",
                    QTRJ1-WEND-DATE
                   "' | " INTO WS-WHERE.

           STRING " ORDER BY TRJFILE.TJ_BRNO,"
                  " TRJFILE.TJ_DATE,"
                  " TRJFILE.TJ_CLASS,"
                  " TRJFILE.TJ_ACCTNO,"
                  " TRJFILE.TJ_LAWCODE,"
                  " TRJFILE.TJ_LINE |" INTO WS-ORDERBY.

           STRING WS-SELECT DELIMITED BY "|",
                  WS-WHERE  DELIMITED BY "|",
                  WS-ORDERBY DELIMITED BY "|" INTO PASS-SQL.

           EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC.

           EXEC SQL
            DECLARE TRJ1_CURSOR CURSOR FOR
                :TRJ1-RETURN-CODE = 
                 EXEC SSP_TCLP_SELECT
                  ( :PASS-SQL)
                END-EXEC.

           EXEC SQL
               OPEN TRJ1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRJ1-WBEG-BRNO.
           MOVE SPACES TO QTRJ1-WBEG-DATE
                          QTRJ1-WEND-DATE.

           MOVE STAT---GOOD TO TRJ1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-TRJ1-FILE-NEXT.   *> SSP - TRJ
           PERFORM READ-IT.
           MOVE TRJ-PATH-OWNBR  TO TJ-BRNO.
           MOVE TRJ-PATH-SQL    TO E-FILE.
           MOVE TRJ1-KEY        TO E-KEYX.
           INITIALIZE QTRJ-REC.

           EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC.

           IF ( TRJ1-CURSOR-STAT-GOOD )
              EXEC SQL
                  FETCH TRJ1_CURSOR INTO
                   :QTJ-BRNO,
                   :QTJ-DATE,
                   :QTJ-CLASS,
                   :QTJ-ACCTNO,
                   :QTJ-LAWCODE,
                   :QTJ-LINE,
                   :QTJ-SERVACCT, 
                   :QTJ-PURBAL,
                   :QTJ-UNEARN-1,
                   :QTJ-UNEARN-2,
                   :QTJ-UNEARN-3,
                   :QTJ-UNEARN-4,
                   :QTJ-UNEARN-5,
                   :QTJ-UNEARN-6,
                   :QTJ-UNEARN-7,
                   :QTJ-UNEARN-8,
                   :QTJ-UNEARN-9,
                   :QTJ-UNEARN-10,
                   :QTJ-PLCD, 
                   :QTJ-PLTYPE, 
                   :QTJ-REPOCD
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRJ1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRJ-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TRJ1-FILE.

           PERFORM CLOSE-IT.
           IF ( TRJ1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC
              EXEC SQL
                   CLOSE TRJ1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRJ1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.


      * COPY "LIBLP/LPTRK1RN.CPY".
      * COPYMEMBER: LIBLP/LPTRK1RN
       LOAD-TRK1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRK-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRK-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRK-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRK-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRK-ALL-DATA
              MOVE TRK-ALL-PATH             TO TRK-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRK-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRK-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRK-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRK-ALL-DATA
              MOVE TRK-ALL-PATH             TO TRK-PATH.

      *-----------------------------------------------------------------
       OPEN-TRK1-FILE.
           PERFORM LOAD-TRK1-FILE.
           PERFORM OPEN-IT.
           MOVE TRK-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRK1-FILE.   *> SSP - TRK
           PERFORM START-IT.
           MOVE TRK-PATH-OWNBR  TO TK-BRNO.
           MOVE TRK-PATH-SQL    TO E-FILE.
           MOVE TRK1-KEY        TO E-KEYX.

           INITIALIZE WS-SELECT WS-WHERE WS-ORDERBY PASS-SQL.

           IF ( TRK1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRK1-FILE.

           STRING "SELECT TRKFILE.TK_BRNO,"
                  "CAST(TRKFILE.TK_DATE AS VARCHAR(10)),"
                  "TRKFILE.TK_CLASS,"
                  "TRKFILE.TK_ACCTNO,"
                  "TRKFILE.TK_LAWCODE,"
                  "TRKFILE.TK_LINE,"
                  "TRKFILE.TK_SERVACCT,"
                  "TRKFILE.TK_REPOCD,"
                  "TRKFILE.TK_PLCD,"
                  "TRKFILE.TK_PLTYPE,"
                  "TRKFILE.TK_PURBAL,"
                  "TRKFILE.TK_UNEARN_1,"
                  "TRKFILE.TK_UNEARN_2,"
                  "TRKFILE.TK_UNEARN_3,"
                  "TRKFILE.TK_UNEARN_4,"
                  "TRKFILE.TK_UNEARN_5,"
                  "TRKFILE.TK_UNEARN_6,"
                  "TRKFILE.TK_UNEARN_7,"
                  "TRKFILE.TK_UNEARN_8,"
                  "TRKFILE.TK_UNEARN_9,"
                  "TRKFILE.TK_UNEARN_10,"
                  "TRKFILE.TK_DLRVPRIN_1,"
                  "TRKFILE.TK_DLRVPRIN_2,"
                  "TRKFILE.TK_DLRVPRIN_3,"
                  "TRKFILE.TK_EARNED_1,"
                  "TRKFILE.TK_EARNED_2,"
                  "TRKFILE.TK_EARNED_3,"
                  "TRKFILE.TK_EARNED_4,"
                  "TRKFILE.TK_EARNED_5,"
                  "TRKFILE.TK_EARNED_6,"
                  "TRKFILE.TK_EARNED_7,"
                  "TRKFILE.TK_EARNED_8,"
                  "TRKFILE.TK_EARNED_9,"
                  "TRKFILE.TK_EARNED_10"
                  " FROM DBO.TRKFILE | " INTO WS-SELECT.

           STRING " WHERE TRKFILE.TK_BRNO = ",
                   QTRK1-WBEG-BRNO
                  " AND TRKFILE.TK_DATE BETWEEN '",
                   QTRK1-WBEG-DATE 
                  "' AND '",
                   QTRK1-WEND-DATE
                  "' | " INTO WS-WHERE.

           STRING " ORDER BY TRKFILE.TK_BRNO,"
                  "   TRKFILE.TK_DATE,"
                  "   TRKFILE.TK_CLASS,"
                  "   TRKFILE.TK_ACCTNO,"
                  "   TRKFILE.TK_LAWCODE,"
                  "   TRKFILE.TK_LINE |" INTO WS-ORDERBY.

           STRING WS-SELECT DELIMITED BY "|",
                  WS-WHERE  DELIMITED BY "|",
                  WS-ORDERBY DELIMITED BY "|" INTO PASS-SQL.

           EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC.

           EXEC SQL
            DECLARE TRK1_CURSOR CURSOR FOR
                :TRK1-RETURN-CODE = 
                 EXEC SSP_TCLP_SELECT
                  ( :PASS-SQL)
                END-EXEC.

           EXEC SQL
               OPEN TRK1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRK1-WBEG-BRNO.
           MOVE SPACES TO QTRK1-WBEG-DATE
                          QTRK1-WEND-DATE.

           MOVE STAT---GOOD TO TRK1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-TRK1-FILE-NEXT.   *> SSP - TRK
           PERFORM READ-IT.
           MOVE TRK-PATH-OWNBR  TO TK-BRNO.
           MOVE TRK-PATH-SQL    TO E-FILE.
           MOVE TRK1-KEY        TO E-KEYX.
           INITIALIZE QTRK-REC.

           EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC.

           IF ( TRK1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TRK1_CURSOR INTO  
                   :QTK-BRNO,
                   :QTK-DATE,
                   :QTK-CLASS,
                   :QTK-ACCTNO,
                   :QTK-LAWCODE,
                   :QTK-LINE,
                   :QTK-SERVACCT, 
                   :QTK-REPOCD, 
                   :QTK-PLCD, 
                   :QTK-PLTYPE, 
                   :QTK-PURBAL,
                   :QTK-UNEARN-1,
                   :QTK-UNEARN-2,
                   :QTK-UNEARN-3,
                   :QTK-UNEARN-4,
                   :QTK-UNEARN-5,
                   :QTK-UNEARN-6,
                   :QTK-UNEARN-7,
                   :QTK-UNEARN-8,
                   :QTK-UNEARN-9,
                   :QTK-UNEARN-10,
                   :QTK-DLRVPRIN-1,
                   :QTK-DLRVPRIN-2,
                   :QTK-DLRVPRIN-3,
                   :QTK-EARNED-1,
                   :QTK-EARNED-2,
                   :QTK-EARNED-3,
                   :QTK-EARNED-4,
                   :QTK-EARNED-5,
                   :QTK-EARNED-6,
                   :QTK-EARNED-7,
                   :QTK-EARNED-8,
                   :QTK-EARNED-9,
                   :QTK-EARNED-10
                 END-EXEC
              ELSE
                 MOVE 9        TO SQLCODE
                 MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRK1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRK-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TRK1-FILE.

           PERFORM CLOSE-IT.

           IF ( TRK1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC
              EXEC SQL
                   CLOSE TRK1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRK1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.


      * COPY "LIBLP/LPTRL1RN.CPY".
      * COPYMEMBER: LIBLP/LPTR1RN
       LOAD-TRL1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRL-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRL-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRL-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRL-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRL-ALL-DATA
              MOVE TRL-ALL-PATH             TO TRL-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRL-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRL-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRL-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRL-ALL-DATA
              MOVE TRL-ALL-PATH             TO TRL-PATH.

      *-----------------------------------------------------------------
       OPEN-TRL1-FILE.
           PERFORM LOAD-TRL1-FILE.
           PERFORM OPEN-IT.
           MOVE TRL-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRL1-FILE.   *> SSP - TRL
           PERFORM START-IT.
           MOVE TRL-PATH-OWNBR  TO TL-BRNO.
           MOVE TRL-PATH-SQL    TO E-FILE.
           MOVE TRL1-KEY        TO E-KEYX.

           INITIALIZE WS-SELECT WS-WHERE WS-ORDERBY PASS-SQL.

           IF ( TRL1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRL1-FILE.

           STRING  "SELECT TRLFILE.TL_BRNO,"
                   "CAST(TRLFILE.TL_DATE AS VARCHAR(10)),"
                   "TRLFILE.TL_CLASS,"
                   "TRLFILE.TL_ACCTNO,"
                   "TRLFILE.TL_LAWCODE,"
                   "TRLFILE.TL_LINE,"
                   "TRLFILE.TL_SRCD,"
                   "TRLFILE.TL_PROCEEDS,"
                   "TRLFILE.TL_LNAMT,"
                   "TRLFILE.TL_PROCASH,"
                   "TRLFILE.TL_FEECODE_1,"
                   "TRLFILE.TL_FEECODE_2,"
                   "TRLFILE.TL_FEECODE_3,"
                   "TRLFILE.TL_FEECODE_4,"
                   "TRLFILE.TL_FEECODE_5,"
                   "TRLFILE.TL_FEECODE_6,"
                   "TRLFILE.TL_FEECODE_7,"
                   "TRLFILE.TL_FEECODE_8,"
                   "TRLFILE.TL_FEECODE_9,"
                   "TRLFILE.TL_FEECODE_10,"
                   "TRLFILE.TL_FEECODE_11,"
                   "TRLFILE.TL_FEECODE_12,"
                   "TRLFILE.TL_FEECODE_13,"
                   "TRLFILE.TL_FEECODE_14,"
                   "TRLFILE.TL_FEECODE_15,"
                   "TRLFILE.TL_FEECODE_16,"
                   "TRLFILE.TL_FEECODE_17,"
                   "TRLFILE.TL_FEECODE_18,"
                   "TRLFILE.TL_FEECODE_19,"
                   "TRLFILE.TL_FEECODE_20,"
                   "TRLFILE.TL_FEEAMT_1,"
                   "TRLFILE.TL_FEEAMT_2,"
                   "TRLFILE.TL_FEEAMT_3,"
                   "TRLFILE.TL_FEEAMT_4,"
                   "TRLFILE.TL_FEEAMT_5,"
                   "TRLFILE.TL_FEEAMT_6,"
                   "TRLFILE.TL_FEEAMT_7,"
                   "TRLFILE.TL_FEEAMT_8,"
                   "TRLFILE.TL_FEEAMT_9,"
                   "TRLFILE.TL_FEEAMT_10,"
                   "TRLFILE.TL_FEEAMT_11,"
                   "TRLFILE.TL_FEEAMT_12,"
                   "TRLFILE.TL_FEEAMT_13,"
                   "TRLFILE.TL_FEEAMT_14,"
                   "TRLFILE.TL_FEEAMT_15,"
                   "TRLFILE.TL_FEEAMT_16,"
                   "TRLFILE.TL_FEEAMT_17,"
                   "TRLFILE.TL_FEEAMT_18,"
                   "TRLFILE.TL_FEEAMT_19,"
                   "TRLFILE.TL_FEEAMT_20,"
                   "TRLFILE.TL_INSTYPES,"
                   "TRLFILE.TL_INSOURS_1,"
                   "TRLFILE.TL_INSOURS_2,"
                   "TRLFILE.TL_INSOURS_3,"
                   "TRLFILE.TL_INSOURS_4,"
                   "TRLFILE.TL_INSOURS_5,"
                   "TRLFILE.TL_INSOURS_6,"
                   "TRLFILE.TL_INSOURS_7,"
                   "TRLFILE.TL_INSOURS_8,"
                   "TRLFILE.TL_INSOURS_9,"
                   "TRLFILE.TL_INSOURS_10,"
                   "TRLFILE.TL_INSPREM_1,"
                   "TRLFILE.TL_INSPREM_2,"
                   "TRLFILE.TL_INSPREM_3,"
                   "TRLFILE.TL_INSPREM_4,"
                   "TRLFILE.TL_INSPREM_5,"
                   "TRLFILE.TL_INSPREM_6,"
                   "TRLFILE.TL_INSPREM_7,"
                   "TRLFILE.TL_INSPREM_8,"
                   "TRLFILE.TL_INSPREM_9,"
                   "TRLFILE.TL_INSPREM_10,"
                   "TRLFILE.TL_INSCOMM_1,"
                   "TRLFILE.TL_INSCOMM_2,"
                   "TRLFILE.TL_INSCOMM_3,"
                   "TRLFILE.TL_INSCOMM_4,"
                   "TRLFILE.TL_INSCOMM_5,"
                   "TRLFILE.TL_INSCOMM_6,"
                   "TRLFILE.TL_INSCOMM_7,"
                   "TRLFILE.TL_INSCOMM_8,"
                   "TRLFILE.TL_INSCOMM_9,"
                   "TRLFILE.TL_INSCOMM_10,"
                   "TRLFILE.TL_EARNINGS_1,"
                   "TRLFILE.TL_EARNINGS_2,"
                   "TRLFILE.TL_EARNINGS_3,"
                   "TRLFILE.TL_EARNINGS_4,"
                   "TRLFILE.TL_EARNINGS_5,"
                   "TRLFILE.TL_EARNINGS_6,"
                   "TRLFILE.TL_EARNINGS_7,"
                   "TRLFILE.TL_EARNINGS_8,"
                   "TRLFILE.TL_EARNINGS_9,"
                   "TRLFILE.TL_EARNINGS_10,"
                   "TRLFILE.TL_ANTICERN_1,"
                   "TRLFILE.TL_ANTICERN_2,"
                   "TRLFILE.TL_ANTICERN_3,"
                   "TRLFILE.TL_ANTICERN_4,"
                   "TRLFILE.TL_ANTICERN_5,"
                   "TRLFILE.TL_ANTICERN_6,"
                   "TRLFILE.TL_ANTICERN_7,"
                   "TRLFILE.TL_ANTICERN_8,"
                   "TRLFILE.TL_ANTICERN_9,"
                   "TRLFILE.TL_ANTICERN_10,"
                   "TRLFILE.TL_LPEOD5_FG,"
                   "TRLFILE.TL_STATE_WORKER,"
                   "TRLFILE.TL_SP_LNCOST_FRMLA,"
                   "TRLFILE.TL_PRDLDISC_UNEARN,"
                   "TRLFILE.TL_DEBIT_CARD_AMT,"
                   "TRLFILE.TL_ACTIONCD2"
                  " FROM DBO.TRLFILE | " INTO WS-SELECT.

           STRING " WHERE TRLFILE.TL_BRNO = ",
                    QTRL1-WBEG-BRNO,
                  " AND TRLFILE.TL_DATE BETWEEN '"
                    QTRL1-WBEG-DATE 
                  " ' AND '"
                   QTRL1-WEND-DATE
                  "' | " INTO WS-WHERE.

           STRING " ORDER BY TRLFILE.TL_BRNO,"
                   "  TRLFILE.TL_DATE,"
                   "  TRLFILE.TL_CLASS,"
                   "  TRLFILE.TL_ACCTNO,"
                   "  TRLFILE.TL_LAWCODE,"
                   "  TRLFILE.TL_LINE  |" INTO WS-ORDERBY.

           STRING WS-SELECT DELIMITED BY "|",
                  WS-WHERE  DELIMITED BY "|",
                  WS-ORDERBY DELIMITED BY "|" INTO PASS-SQL.

           EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC.

           EXEC SQL
            DECLARE TRL1_CURSOR CURSOR FOR
                :TRL1-RETURN-CODE = 
                 EXEC SSP_TCLP_SELECT
                  ( :PASS-SQL)
                END-EXEC.

           EXEC SQL
               OPEN TRL1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRL1-WBEG-BRNO.
           MOVE SPACES TO QTRL1-WBEG-DATE
                          QTRL1-WEND-DATE.

           MOVE STAT---GOOD TO TRL1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-TRL1-FILE-NEXT.   *> SSP - TRL
           PERFORM READ-IT.
           MOVE TRL-PATH-OWNBR  TO TL-BRNO.
           MOVE TRL-PATH-SQL    TO E-FILE.
           MOVE TRL1-KEY        TO E-KEYX.
           INITIALIZE QTRL-REC.

           EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC.

           IF ( TRL1-CURSOR-STAT-GOOD )
              EXEC SQL
                FETCH TRL1_CURSOR INTO  
                  :QTL-BRNO,
                  :QTL-DATE ,
                  :QTL-CLASS,
                  :QTL-ACCTNO,
                  :QTL-LAWCODE,
                  :QTL-LINE,
                  :QTL-SRCD,
                  :QTL-PROCEEDS,
                  :QTL-LNAMT,
                  :QTL-PROCASH,
                  :QTL-FEECODE-1,
                  :QTL-FEECODE-2,
                  :QTL-FEECODE-3,
                  :QTL-FEECODE-4,
                  :QTL-FEECODE-5,
                  :QTL-FEECODE-6,
                  :QTL-FEECODE-7,
                  :QTL-FEECODE-8,
                  :QTL-FEECODE-9,
                  :QTL-FEECODE-10,
                  :QTL-FEECODE-11,
                  :QTL-FEECODE-12,
                  :QTL-FEECODE-13,
                  :QTL-FEECODE-14,
                  :QTL-FEECODE-15,
                  :QTL-FEECODE-16,
                  :QTL-FEECODE-17,
                  :QTL-FEECODE-18,
                  :QTL-FEECODE-19,
                  :QTL-FEECODE-20,
                  :QTL-FEEAMT-1,
                  :QTL-FEEAMT-2,
                  :QTL-FEEAMT-3,
                  :QTL-FEEAMT-4,
                  :QTL-FEEAMT-5,
                  :QTL-FEEAMT-6,
                  :QTL-FEEAMT-7,
                  :QTL-FEEAMT-8,
                  :QTL-FEEAMT-9,
                  :QTL-FEEAMT-10,
                  :QTL-FEEAMT-11,
                  :QTL-FEEAMT-12,
                  :QTL-FEEAMT-13,
                  :QTL-FEEAMT-14,
                  :QTL-FEEAMT-15,
                  :QTL-FEEAMT-16,
                  :QTL-FEEAMT-17,
                  :QTL-FEEAMT-18,
                  :QTL-FEEAMT-19,
                  :QTL-FEEAMT-20,
                  :QTL-INSTYPES,
                  :QTL-INSOURS-1,
                  :QTL-INSOURS-2,
                  :QTL-INSOURS-3,
                  :QTL-INSOURS-4,
                  :QTL-INSOURS-5,
                  :QTL-INSOURS-6,
                  :QTL-INSOURS-7,
                  :QTL-INSOURS-8,
                  :QTL-INSOURS-9,
                  :QTL-INSOURS-10,
                  :QTL-INSPREM-1,
                  :QTL-INSPREM-2,
                  :QTL-INSPREM-3,
                  :QTL-INSPREM-4,
                  :QTL-INSPREM-5,
                  :QTL-INSPREM-6,
                  :QTL-INSPREM-7,
                  :QTL-INSPREM-8,
                  :QTL-INSPREM-9,
                  :QTL-INSPREM-10,
                  :QTL-INSCOMM-1,
                  :QTL-INSCOMM-2,
                  :QTL-INSCOMM-3,
                  :QTL-INSCOMM-4,
                  :QTL-INSCOMM-5,
                  :QTL-INSCOMM-6,
                  :QTL-INSCOMM-7,
                  :QTL-INSCOMM-8,
                  :QTL-INSCOMM-9,
                  :QTL-INSCOMM-10,
                  :QTL-EARNINGS-1,
                  :QTL-EARNINGS-2,
                  :QTL-EARNINGS-3,
                  :QTL-EARNINGS-4,
                  :QTL-EARNINGS-5,
                  :QTL-EARNINGS-6,
                  :QTL-EARNINGS-7,
                  :QTL-EARNINGS-8,
                  :QTL-EARNINGS-9,
                  :QTL-EARNINGS-10,
                  :QTL-ANTICERN-1,
                  :QTL-ANTICERN-2,
                  :QTL-ANTICERN-3,
                  :QTL-ANTICERN-4,
                  :QTL-ANTICERN-5,
                  :QTL-ANTICERN-6,
                  :QTL-ANTICERN-7,
                  :QTL-ANTICERN-8,
                  :QTL-ANTICERN-9,
                  :QTL-ANTICERN-10,
                  :QTL-LPEOD5-FG,
                  :QTL-STATE-WORKER,
                  :QTL-SP-LNCOST-FRMLA,
                  :QTL-PRDLDISC-UNEARN,
                  :QTL-DEBIT-CARD-AMT,
                  :QTL-ACTIONCD2
                 END-EXEC
              ELSE 
                 MOVE 9        TO SQLCODE
                 MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRL1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRL-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TRL1-FILE.

           PERFORM CLOSE-IT.

           IF ( TRL1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC
              EXEC SQL
                   CLOSE TRL1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRL1-CURSOR-STAT.
 
            EXEC SQL SET CONNECTION C1 END-EXEC.
 

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

      *-----------------------------------------------------------------
       OPEN-TRP1-FILE.
           PERFORM LOAD-TRP1-FILE.
           PERFORM OPEN-IT.
           MOVE TRP-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRP1-FILE.   *> SSP - TRP
           PERFORM START-IT.
           MOVE TRP-PATH-OWNBR  TO TP-BRNO.
           MOVE TRP-PATH-SQL    TO E-FILE.
           MOVE TRP1-KEY        TO E-KEYX.

           INITIALIZE WS-SELECT WS-WHERE WS-ORDERBY PASS-SQL.

           IF ( TRP1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRP1-FILE.

           STRING "SELECT TRPFILE.TP_BRNO,"
                  "CAST(TRPFILE.TP_DATE AS VARCHAR(10)),"
                  "TRPFILE.TP_CLASS,"
                  "TRPFILE.TP_ACCTNO,"
                  "TRPFILE.TP_LINE,"
                  "TRPFILE.TP_PLCD,"
                  "TRPFILE.TP_IBPC,"
                  "TRPFILE.TP_TRCD,"
                  "TRPFILE.TP_REFNO,"
                  "TRPFILE.TP_TRAMT,"
                  "TRPFILE.TP_APOT2,"
                  "TRPFILE.TP_APINT,"
                  "TRPFILE.TP_APLC,"
                  "TRPFILE.TP_APCUR,"
                  "TRPFILE.TP_GLNO_1,"
                  "TRPFILE.TP_GLNO_2,"
                  "TRPFILE.TP_GLNO_3,"
                  "TRPFILE.TP_GLAMT_1,"
                  "TRPFILE.TP_GLAMT_2,"
                  "TRPFILE.TP_GLAMT_3,"
                  "TRPFILE.TP_EARNINGS_1,"
                  "TRPFILE.TP_EARNINGS_2,"
                  "TRPFILE.TP_EARNINGS_3,"
                  "TRPFILE.TP_EARNINGS_4,"
                  "TRPFILE.TP_EARNINGS_5,"
                  "TRPFILE.TP_EARNINGS_6,"
                  "TRPFILE.TP_EARNINGS_7,"
                  "TRPFILE.TP_EARNINGS_8,"
                  "TRPFILE.TP_EARNINGS_9,"
                  "TRPFILE.TP_EARNINGS_10,"
                  "TRPFILE.TP_WORKAMT_1,"
                  "TRPFILE.TP_WORKAMT_2,"
                  "TRPFILE.TP_WORKAMT_3,"
                  "TRPFILE.TP_WORKAMT_4,"
                  "TRPFILE.TP_WORKAMT_5,"
                  "TRPFILE.TP_WORKAMT_6,"
                  "TRPFILE.TP_WORKAMT_7,"
                  "TRPFILE.TP_WORKAMT_8,"
                  "TRPFILE.TP_WORKAMT_9,"
                  "TRPFILE.TP_WORKAMT_10,"
                  "TRPFILE.TP_CURBAL,"
                  "TRPFILE.TP_DLPROC,"
                  "TRPFILE.TP_SERVACCT,"
                  "TRPFILE.TP_INSOURS,"
                  "TRPFILE.TP_LOANTYPE,"
                  "TRPFILE.TP_POSTING_BR,"
                  "TRPFILE.TP_OTHBR_FG,"
                  "TRPFILE.TP_REV,"
                  "TRPFILE.TP_REVTRCD,"
                  "TRPFILE.TP_INSTYPES,"
                  "TRPFILE.TP_DLRVCHGS_ADJ_1,"
                  "TRPFILE.TP_DLRVCHGS_ADJ_2,"
                  "TRPFILE.TP_DLRVCHGS_ADJ_3,"
                  "TRPFILE.TP_DLPARVHELD_ADJ,"
                  "TRPFILE.TP_DLPARVPAID_ADJ,"
                  "TRPFILE.TP_DL_PARVHELD_CODE,"
                  "TRPFILE.TP_DL_PARVPAID_CODE,"
                  "TRPFILE.TP_INSCOMM,"
                  "TRPFILE.TP_PLTYPE,"
                  "TRPFILE.TP_PL_OTH_AR,"
                  "TRPFILE.TP_REPOCD,"
                  "TRPFILE.TP_SP_DEFFRMLA,"
                  "TRPFILE.TP_PENALTY_INT,"
                  "TRPFILE.TP_INS_TAX_RB,"
                  "TRPFILE.TP_MIP_FG,"
                  "TRPFILE.TP_MIP_CLPAID,"
                  "TRPFILE.TP_MIP_CLCOMM,"
                  "TRPFILE.TP_MIP_AHPAID,"
                  "TRPFILE.TP_MIP_AHCOMM,"
                  "TRPFILE.TP_APPLIED_TO_ESCROW,"
                  "TRPFILE.TP_ESCROW_PREPAID,"
                  "TRPFILE.TP_ESCROW_DISBURSED,"
                  "TRPFILE.TP_ACTIONCD2"
                  " FROM DBO.TRPFILE | " INTO WS-SELECT.

           STRING " WHERE  TRPFILE.TP_BRNO = ",
                    QTRP1-WBEG-BRNO  
                  " AND TRPFILE.TP_DATE BETWEEN '"
                    QTRP1-WBEG-DATE
                  " 'AND '",
                    QTRP1-WEND-DATE 
                  "' | " INTO WS-WHERE.

           STRING " ORDER BY TRPFILE.TP_BRNO,"
                    " TRPFILE.TP_DATE,"
                    " TRPFILE.TP_CLASS,"
                    " TRPFILE.TP_ACCTNO,"
                    " TRPFILE.TP_LINE |" INTO WS-ORDERBY.

           STRING WS-SELECT DELIMITED BY "|",
                  WS-WHERE  DELIMITED BY "|",
                  WS-ORDERBY DELIMITED BY "|" INTO PASS-SQL.

           EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC.

           EXEC SQL
            DECLARE TRP1_CURSOR CURSOR FOR
                :TRP1-RETURN-CODE = 
                 EXEC SSP_TCLP_SELECT
                  ( :PASS-SQL)
                END-EXEC.

           EXEC SQL
               OPEN TRP1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRP1-WBEG-BRNO.
           MOVE SPACES TO QTRP1-WBEG-DATE
                          QTRP1-WEND-DATE.

           MOVE STAT---GOOD TO TRP1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-TRP1-FILE-NEXT.  *> SSP - TRP
           PERFORM READ-IT.
           MOVE TRP-PATH-OWNBR  TO TP-BRNO.
           MOVE TRP-PATH-SQL    TO E-FILE.
           MOVE TRP1-KEY        TO E-KEYX.
           INITIALIZE QTRP-REC.

           EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC.

           IF ( TRP1-CURSOR-STAT-GOOD )
              EXEC SQL
                 FETCH TRP1_CURSOR INTO  
                   :QTP-BRNO,
                   :QTP-DATE,
                   :QTP-CLASS,
                   :QTP-ACCTNO,
                   :QTP-LINE,
                   :QTP-PLCD,
                   :QTP-IBPC,
                   :QTP-TRCD,
                   :QTP-REFNO,
                   :QTP-TRAMT,
                   :QTP-APOT2,
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
                   :QTP-WORKAMT-1,
                   :QTP-WORKAMT-2,
                   :QTP-WORKAMT-3,
                   :QTP-WORKAMT-4,
                   :QTP-WORKAMT-5,
                   :QTP-WORKAMT-6,
                   :QTP-WORKAMT-7,
                   :QTP-WORKAMT-8,
                   :QTP-WORKAMT-9,
                   :QTP-WORKAMT-10,
                   :QTP-CURBAL,
                   :QTP-DLPROC,
                   :QTP-SERVACCT,
                   :QTP-INSOURS,
                   :QTP-LOANTYPE,
                   :QTP-POSTING-BR,
                   :QTP-OTHBR-FG,
                   :QTP-REV,
                   :QTP-REVTRCD,
                   :QTP-INSTYPES,
                   :QTP-DLRVCHGS-ADJ-1,
                   :QTP-DLRVCHGS-ADJ-2,
                   :QTP-DLRVCHGS-ADJ-3,
                   :QTP-DLPARVHELD-ADJ,
                   :QTP-DLPARVPAID-ADJ,
                   :QTP-DL-PARVHELD-CODE,
                   :QTP-DL-PARVPAID-CODE,
                   :QTP-INSCOMM,
                   :QTP-PLTYPE,
                   :QTP-PL-OTH-AR,
                   :QTP-REPOCD,
                   :QTP-SP-DEFFRMLA,
                   :QTP-PENALTY-INT,
                   :QTP-INS-TAX-RB,
                   :QTP-MIP-FG,
                   :QTP-MIP-CLPAID,
                   :QTP-MIP-CLCOMM,
                   :QTP-MIP-AHPAID,
                   :QTP-MIP-AHCOMM,
                   :QTP-APPLIED-TO-ESCROW,
                   :QTP-ESCROW-PREPAID,
                   :QTP-ESCROW-DISBURSED,
                   :QTP-ACTIONCD2 
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

           IF ( TRP1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC
              EXEC SQL
                   CLOSE TRP1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRP1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.


      * COPY "LIBLP/LPTRS1RN.CPY".
      * COPYMEMBER: LIBLP/LPTR1RN
       LOAD-TRS1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRS-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRS-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRS-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRS-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRS-ALL-DATA
              MOVE TRS-ALL-PATH             TO TRS-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRS-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRS-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRS-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRS-ALL-DATA
              MOVE TRS-ALL-PATH             TO TRS-PATH.

      *-----------------------------------------------------------------
       OPEN-TRS1-FILE.
           PERFORM LOAD-TRS1-FILE.
           PERFORM OPEN-IT.
           MOVE TRS-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRS1-FILE.   *> SSP - TRS
           PERFORM START-IT.
           MOVE TRS-PATH-OWNBR  TO TS-BRNO.
           MOVE TRS-PATH-SQL TO E-FILE.
           MOVE TRS1-KEY        TO E-KEYX.

           INITIALIZE WS-SELECT WS-WHERE WS-ORDERBY PASS-SQL.

           IF ( TRS1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRS1-FILE.

           STRING "SELECT TRSFILE.TS_BRNO,"
                  "CAST(TRSFILE.TS_DATE AS VARCHAR(10))," 
                  "TRSFILE.TS_CLASS,"
                  "TRSFILE.TS_ACCTNO,"
                  "TRSFILE.TS_LAWCODE,"
                  "TRSFILE.TS_LINE,"
                  "TRSFILE.TS_SRCD,"
                  "TRSFILE.TS_SERVACCT,"
                  "TRSFILE.TS_LOANTYPE,"
                  "TRSFILE.TS_PROCEEDS,"
                  "TRSFILE.TS_LNAMT,"
                  "TRSFILE.TS_FEECODE_1,"
                  "TRSFILE.TS_FEECODE_2,"
                  "TRSFILE.TS_FEECODE_3,"
                  "TRSFILE.TS_FEECODE_4,"
                  "TRSFILE.TS_FEECODE_5,"
                  "TRSFILE.TS_FEECODE_6,"
                  "TRSFILE.TS_FEECODE_7,"
                  "TRSFILE.TS_FEECODE_8,"
                  "TRSFILE.TS_FEECODE_9,"
                  "TRSFILE.TS_FEECODE_10,"
                  "TRSFILE.TS_FEECODE_11,"
                  "TRSFILE.TS_FEECODE_12,"
                  "TRSFILE.TS_FEECODE_13,"
                  "TRSFILE.TS_FEECODE_14,"
                  "TRSFILE.TS_FEECODE_15,"
                  "TRSFILE.TS_FEECODE_16,"
                  "TRSFILE.TS_FEECODE_17,"
                  "TRSFILE.TS_FEECODE_18,"
                  "TRSFILE.TS_FEECODE_19,"
                  "TRSFILE.TS_FEECODE_20,"
                  "TRSFILE.TS_FEEAMT_1,"
                  "TRSFILE.TS_FEEAMT_2,"
                  "TRSFILE.TS_FEEAMT_3,"
                  "TRSFILE.TS_FEEAMT_4,"
                  "TRSFILE.TS_FEEAMT_5,"
                  "TRSFILE.TS_FEEAMT_6,"
                  "TRSFILE.TS_FEEAMT_7,"
                  "TRSFILE.TS_FEEAMT_8,"
                  "TRSFILE.TS_FEEAMT_9,"
                  "TRSFILE.TS_FEEAMT_10,"
                  "TRSFILE.TS_FEEAMT_11,"
                  "TRSFILE.TS_FEEAMT_12,"
                  "TRSFILE.TS_FEEAMT_13,"
                  "TRSFILE.TS_FEEAMT_14,"
                  "TRSFILE.TS_FEEAMT_15,"
                  "TRSFILE.TS_FEEAMT_16,"
                  "TRSFILE.TS_FEEAMT_17,"
                  "TRSFILE.TS_FEEAMT_18,"
                  "TRSFILE.TS_FEEAMT_19,"
                  "TRSFILE.TS_FEEAMT_20,"
                  "TRSFILE.TS_FEEOURS_1,"
                  "TRSFILE.TS_FEEOURS_2,"
                  "TRSFILE.TS_FEEOURS_3,"
                  "TRSFILE.TS_FEEOURS_4,"
                  "TRSFILE.TS_FEEOURS_5,"
                  "TRSFILE.TS_FEEOURS_6,"
                  "TRSFILE.TS_FEEOURS_7,"
                  "TRSFILE.TS_FEEOURS_8,"
                  "TRSFILE.TS_FEEOURS_9,"
                  "TRSFILE.TS_FEEOURS_10,"
                  "TRSFILE.TS_FEEOURS_11,"
                  "TRSFILE.TS_FEEOURS_12,"
                  "TRSFILE.TS_FEEOURS_13,"
                  "TRSFILE.TS_FEEOURS_14,"
                  "TRSFILE.TS_FEEOURS_15,"
                  "TRSFILE.TS_FEEOURS_16,"
                  "TRSFILE.TS_FEEOURS_17,"
                  "TRSFILE.TS_FEEOURS_18,"
                  "TRSFILE.TS_FEEOURS_19,"
                  "TRSFILE.TS_FEEOURS_20,"
                  "TRSFILE.TS_INSTYPES,"
                  "TRSFILE.TS_INSOURS_1,"
                  "TRSFILE.TS_INSOURS_2,"
                  "TRSFILE.TS_INSOURS_3,"
                  "TRSFILE.TS_INSOURS_4,"
                  "TRSFILE.TS_INSOURS_5,"
                  "TRSFILE.TS_INSOURS_6,"
                  "TRSFILE.TS_INSOURS_7,"
                  "TRSFILE.TS_INSOURS_8,"
                  "TRSFILE.TS_INSOURS_9,"
                  "TRSFILE.TS_INSOURS_10,"
                  "TRSFILE.TS_INSPREM_1,"
                  "TRSFILE.TS_INSPREM_2,"
                  "TRSFILE.TS_INSPREM_3,"
                  "TRSFILE.TS_INSPREM_4,"
                  "TRSFILE.TS_INSPREM_5,"
                  "TRSFILE.TS_INSPREM_6,"
                  "TRSFILE.TS_INSPREM_7,"
                  "TRSFILE.TS_INSPREM_8,"
                  "TRSFILE.TS_INSPREM_9,"
                  "TRSFILE.TS_INSPREM_10,"
                  "TRSFILE.TS_INSCOMM_1,"
                  "TRSFILE.TS_INSCOMM_2,"
                  "TRSFILE.TS_INSCOMM_3,"
                  "TRSFILE.TS_INSCOMM_4,"
                  "TRSFILE.TS_INSCOMM_5,"
                  "TRSFILE.TS_INSCOMM_6,"
                  "TRSFILE.TS_INSCOMM_7,"
                  "TRSFILE.TS_INSCOMM_8,"
                  "TRSFILE.TS_INSCOMM_9,"
                  "TRSFILE.TS_INSCOMM_10,"
                  "TRSFILE.TS_DLCOMM_PP_CMSI,"
                  "TRSFILE.TS_DLCOMM_O3_CMSI,"
                  "TRSFILE.TS_UNWIND_REASON_CODE,"
                  "TRSFILE.TS_DLRVPRIN_1,"
                  "TRSFILE.TS_DLRVPRIN_2,"
                  "TRSFILE.TS_DLRVPRIN_3,"
                  "TRSFILE.TS_DLRVCHGS_1,"
                  "TRSFILE.TS_DLRVCHGS_2,"
                  "TRSFILE.TS_DLRVCHGS_3,"
                  "TRSFILE.TS_DL_PARVHELD_CODE,"
                  "TRSFILE.TS_DL_PARVPAID_CODE,"
                  "TRSFILE.TS_DLPARVHELD,"
                  "TRSFILE.TS_DLPARVPAID,"
                  "TRSFILE.TS_ANTICERN_1,"
                  "TRSFILE.TS_ANTICERN_2,"
                  "TRSFILE.TS_ANTICERN_3,"
                  "TRSFILE.TS_ANTICERN_4,"
                  "TRSFILE.TS_ANTICERN_5,"
                  "TRSFILE.TS_ANTICERN_6,"
                  "TRSFILE.TS_ANTICERN_7,"
                  "TRSFILE.TS_ANTICERN_8,"
                  "TRSFILE.TS_ANTICERN_9,"
                  "TRSFILE.TS_ANTICERN_10,"
                  "TRSFILE.TS_EARNINGS_1,"
                  "TRSFILE.TS_EARNINGS_2,"
                  "TRSFILE.TS_EARNINGS_3,"
                  "TRSFILE.TS_EARNINGS_4,"
                  "TRSFILE.TS_EARNINGS_5,"
                  "TRSFILE.TS_EARNINGS_6,"
                  "TRSFILE.TS_EARNINGS_7,"
                  "TRSFILE.TS_EARNINGS_8,"
                  "TRSFILE.TS_EARNINGS_9,"
                  "TRSFILE.TS_EARNINGS_10,"
                  "TRSFILE.TS_PRDLDISC_UNEARN,"
                  "TRSFILE.TS_SP_LNCOST_FRMLA,"
                  "TRSFILE.TS_O3INS_DLPROC,"
                  "TRSFILE.TS_ACTIONCD2" 
                  " FROM DBO.TRSFILE |" INTO WS-SELECT.

           STRING " WHERE TRSFILE.TS_BRNO = ",
                    QTRS1-WBEG-BRNO
                  " AND TRSFILE.TS_DATE BETWEEN '"
                    QTRS1-WBEG-DATE 
                  "' AND '"
                    QTRS1-WEND-DATE
                  "' | " INTO WS-WHERE.

           STRING " ORDER BY TRSFILE.TS_BRNO,"
                   "  TRSFILE.TS_DATE,"
                   "  TRSFILE.TS_CLASS,"
                   "  TRSFILE.TS_ACCTNO,"
                   "  TRSFILE.TS_LAWCODE,"
                   "  TRSFILE.TS_LINE | " INTO WS-ORDERBY.

           STRING WS-SELECT DELIMITED BY "|",
                  WS-WHERE  DELIMITED BY "|",
                  WS-ORDERBY DELIMITED BY "|" INTO PASS-SQL.

           EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC.

           EXEC SQL
            DECLARE TRS1_CURSOR CURSOR FOR
                :TRS1-RETURN-CODE = 
                 EXEC SSP_TCLP_SELECT
                  ( :PASS-SQL)
                END-EXEC.

           EXEC SQL
               OPEN TRS1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRS1-WBEG-BRNO.
           MOVE SPACES TO QTRS1-WBEG-DATE
                          QTRS1-WEND-DATE.

           MOVE STAT---GOOD TO TRS1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-TRS1-FILE-NEXT.    *> SSP - TRS
           PERFORM READ-IT.
           MOVE TRS-PATH-OWNBR  TO TS-BRNO.
           MOVE TRS-PATH-SQL    TO E-FILE.
           MOVE TRS1-KEY        TO E-KEYX.
           INITIALIZE QTRS-REC.

           EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC.

           IF ( TRS1-CURSOR-STAT-GOOD )
              EXEC SQL
                  FETCH TRS1_CURSOR INTO  
                   :QTS-BRNO,
                   :QTS-DATE,
                   :QTS-CLASS,
                   :QTS-ACCTNO,
                   :QTS-LAWCODE,
                   :QTS-LINE,
                   :QTS-SRCD,
                   :QTS-SERVACCT,
                   :QTS-LOANTYPE,
                   :QTS-PROCEEDS,
                   :QTS-LNAMT,
                   :QTS-FEECODE-1,
                   :QTS-FEECODE-2,
                   :QTS-FEECODE-3,
                   :QTS-FEECODE-4,
                   :QTS-FEECODE-5,
                   :QTS-FEECODE-6,
                   :QTS-FEECODE-7,
                   :QTS-FEECODE-8,
                   :QTS-FEECODE-9,
                   :QTS-FEECODE-10,
                   :QTS-FEECODE-11,
                   :QTS-FEECODE-12,
                   :QTS-FEECODE-13,
                   :QTS-FEECODE-14,
                   :QTS-FEECODE-15,
                   :QTS-FEECODE-16,
                   :QTS-FEECODE-17,
                   :QTS-FEECODE-18,
                   :QTS-FEECODE-19,
                   :QTS-FEECODE-20,
                   :QTS-FEEAMT-1,
                   :QTS-FEEAMT-2,
                   :QTS-FEEAMT-3,
                   :QTS-FEEAMT-4,
                   :QTS-FEEAMT-5,
                   :QTS-FEEAMT-6,
                   :QTS-FEEAMT-7,
                   :QTS-FEEAMT-8,
                   :QTS-FEEAMT-9,
                   :QTS-FEEAMT-10,
                   :QTS-FEEAMT-11,
                   :QTS-FEEAMT-12,
                   :QTS-FEEAMT-13,
                   :QTS-FEEAMT-14,
                   :QTS-FEEAMT-15,
                   :QTS-FEEAMT-16,
                   :QTS-FEEAMT-17,
                   :QTS-FEEAMT-18,
                   :QTS-FEEAMT-19,
                   :QTS-FEEAMT-20,
                   :QTS-FEEOURS-1,
                   :QTS-FEEOURS-2,
                   :QTS-FEEOURS-3,
                   :QTS-FEEOURS-4,
                   :QTS-FEEOURS-5,
                   :QTS-FEEOURS-6,
                   :QTS-FEEOURS-7,
                   :QTS-FEEOURS-8,
                   :QTS-FEEOURS-9,
                   :QTS-FEEOURS-10,
                   :QTS-FEEOURS-11,
                   :QTS-FEEOURS-12,
                   :QTS-FEEOURS-13,
                   :QTS-FEEOURS-14,
                   :QTS-FEEOURS-15,
                   :QTS-FEEOURS-16,
                   :QTS-FEEOURS-17,
                   :QTS-FEEOURS-18,
                   :QTS-FEEOURS-19,
                   :QTS-FEEOURS-20,
                   :QTS-INSTYPES,
                   :QTS-INSOURS-1,
                   :QTS-INSOURS-2,
                   :QTS-INSOURS-3,
                   :QTS-INSOURS-4,
                   :QTS-INSOURS-5,
                   :QTS-INSOURS-6,
                   :QTS-INSOURS-7,
                   :QTS-INSOURS-8,
                   :QTS-INSOURS-9,
                   :QTS-INSOURS-10,
                   :QTS-INSPREM-1,
                   :QTS-INSPREM-2,
                   :QTS-INSPREM-3,
                   :QTS-INSPREM-4,
                   :QTS-INSPREM-5,
                   :QTS-INSPREM-6,
                   :QTS-INSPREM-7,
                   :QTS-INSPREM-8,
                   :QTS-INSPREM-9,
                   :QTS-INSPREM-10,
                   :QTS-INSCOMM-1,
                   :QTS-INSCOMM-2,
                   :QTS-INSCOMM-3,
                   :QTS-INSCOMM-4,
                   :QTS-INSCOMM-5,
                   :QTS-INSCOMM-6,
                   :QTS-INSCOMM-7,
                   :QTS-INSCOMM-8,
                   :QTS-INSCOMM-9,
                   :QTS-INSCOMM-10,
                   :QTS-DLCOMM-PP-CMSI,
                   :QTS-DLCOMM-O3-CMSI,
                   :QTS-UNWIND-REASON-CODE,
                   :QTS-DLRVPRIN-1,
                   :QTS-DLRVPRIN-2,
                   :QTS-DLRVPRIN-3,
                   :QTS-DLRVCHGS-1,
                   :QTS-DLRVCHGS-2,
                   :QTS-DLRVCHGS-3,
                   :QTS-DL-PARVHELD-CODE,
                   :QTS-DL-PARVPAID-CODE,
                   :QTS-DLPARVHELD,
                   :QTS-DLPARVPAID,
                   :QTS-ANTICERN-1,
                   :QTS-ANTICERN-2,
                   :QTS-ANTICERN-3,
                   :QTS-ANTICERN-4,
                   :QTS-ANTICERN-5,
                   :QTS-ANTICERN-6,
                   :QTS-ANTICERN-7,
                   :QTS-ANTICERN-8,
                   :QTS-ANTICERN-9,
                   :QTS-ANTICERN-10,
                   :QTS-EARNINGS-1,
                   :QTS-EARNINGS-2,
                   :QTS-EARNINGS-3,
                   :QTS-EARNINGS-4,
                   :QTS-EARNINGS-5,
                   :QTS-EARNINGS-6,
                   :QTS-EARNINGS-7,
                   :QTS-EARNINGS-8,
                   :QTS-EARNINGS-9,
                   :QTS-EARNINGS-10
                   :QTS-PRDLDISC-UNEARN,
                   :QTS-SP-LNCOST-FRMLA,
                   :QTS-O3INS-DLPROC,
                   :QTS-ACTIONCD2 
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRS1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRS-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TRS1-FILE.

           PERFORM CLOSE-IT.

           IF ( TRS1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC
              EXEC SQL
                   CLOSE TRS1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRS1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.


      * COPY "LIBLP/LPTRX1RN.CPY".
      * COPYMEMBER: LIBLP/LPTRX1RN
       LOAD-TRX1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRX-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRX-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRX-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRX-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRX-ALL-DATA
              MOVE TRX-ALL-PATH             TO TRX-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRX-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRX-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRX-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRX-ALL-DATA
              MOVE TRX-ALL-PATH             TO TRX-PATH.

      *-----------------------------------------------------------------
       OPEN-TRX1-FILE.
           PERFORM LOAD-TRX1-FILE.
           PERFORM OPEN-IT.
           MOVE TRX-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRX1-FILE.   *> SSP - TRX
           PERFORM START-IT.
           MOVE TRX-PATH-OWNBR  TO TX-BRNO.
           MOVE TRX-PATH-SQL    TO E-FILE.
           MOVE TRX1-KEY        TO E-KEYX.

           INITIALIZE WS-SELECT WS-WHERE WS-ORDERBY PASS-SQL.

           IF ( TRX1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRX1-FILE.

           STRING "SELECT TRXFILE.TX_BRNO,"
                   "CAST(TRXFILE.TX_DATE AS VARCHAR(10)), "
                   "TRXFILE.TX_CLASS,"
                   "TRXFILE.TX_ACCTNO,"
                   "TRXFILE.TX_LAWCODE,"
                   "TRXFILE.TX_LINE,"
                   "TRXFILE.TX_SERVACCT,"
                   "TRXFILE.TX_PROCEEDS,"
                   "TRXFILE.TX_PURBAL,"
                   "TRXFILE.TX_DLRVPRIN_1,"
                   "TRXFILE.TX_DLRVPRIN_2,"
                   "TRXFILE.TX_DLRVPRIN_3,"
                   "TRXFILE.TX_DLPARVHELD,"
                   "TRXFILE.TX_DLDISC,"
                   "TRXFILE.TX_ANTICERN_1,"
                   "TRXFILE.TX_ANTICERN_2,"
                   "TRXFILE.TX_ANTICERN_3,"
                   "TRXFILE.TX_ANTICERN_4,"
                   "TRXFILE.TX_ANTICERN_5,"
                   "TRXFILE.TX_ANTICERN_6,"
                   "TRXFILE.TX_ANTICERN_7,"
                   "TRXFILE.TX_ANTICERN_8,"
                   "TRXFILE.TX_ANTICERN_9,"
                   "TRXFILE.TX_ANTICERN_10,"
                   "TRXFILE.TX_TFRBAL"
                   " FROM DBO.TRXFILE  | " INTO WS-SELECT.

           STRING " WHERE TRXFILE.TX_BRNO = ",
                    QTRX1-WBEG-BRNO
                  " AND TRXFILE.TX_DATE BETWEEN '"
                    QTRX1-WBEG-DATE 
                  "' AND '",
                   QTRX1-WEND-DATE
                  "' | " INTO WS-WHERE.

           STRING " ORDER BY TRXFILE.TX_BRNO,"
                   "  TRXFILE.TX_DATE,"
                   "  TRXFILE.TX_CLASS,"
                   "  TRXFILE.TX_ACCTNO,"
                   "  TRXFILE.TX_LAWCODE,"
                   "  TRXFILE.TX_LINE |" INTO WS-ORDERBY.

           STRING WS-SELECT DELIMITED BY "|",
                  WS-WHERE  DELIMITED BY "|",
                  WS-ORDERBY DELIMITED BY "|" INTO PASS-SQL.

           EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC.

           EXEC SQL
            DECLARE TRX1_CURSOR CURSOR FOR
                :TRX1-RETURN-CODE = 
                 EXEC SSP_TCLP_SELECT
                  ( :PASS-SQL)
                END-EXEC.

           EXEC SQL
               OPEN TRX1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRX1-WBEG-BRNO.
           MOVE SPACES TO QTRX1-WBEG-DATE
                          QTRX1-WEND-DATE.

           MOVE STAT---GOOD TO TRX1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-TRX1-FILE-NEXT.   *> SSP - TRX
           PERFORM READ-IT.
           MOVE TRX-PATH-OWNBR  TO TX-BRNO.
           MOVE TRX-PATH-SQL    TO E-FILE.
           MOVE TRX1-KEY        TO E-KEYX.
           INITIALIZE QTRX-REC.

           EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC.

           IF ( TRX1-CURSOR-STAT-GOOD )
              EXEC SQL
                  FETCH TRX1_CURSOR INTO  
                   :QTX-BRNO,
                   :QTX-DATE,
                   :QTX-CLASS,
                   :QTX-ACCTNO,
                   :QTX-LAWCODE,
                   :QTX-LINE,
                   :QTX-SERVACCT,
                   :QTX-PROCEEDS,
                   :QTX-PURBAL,
                   :QTX-DLRVPRIN-1,
                   :QTX-DLRVPRIN-2,
                   :QTX-DLRVPRIN-3,
                   :QTX-DLPARVHELD,
                   :QTX-DLDISC,
                   :QTX-ANTICERN-1,
                   :QTX-ANTICERN-2,
                   :QTX-ANTICERN-3,
                   :QTX-ANTICERN-4,
                   :QTX-ANTICERN-5,
                   :QTX-ANTICERN-6,
                   :QTX-ANTICERN-7,
                   :QTX-ANTICERN-8,
                   :QTX-ANTICERN-9,
                   :QTX-ANTICERN-10,
                   :QTX-TFRBAL
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRX1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRX-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TRX1-FILE.

           PERFORM CLOSE-IT.

           IF ( TRX1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION MISC_CONNECT END-EXEC
              EXEC SQL
                   CLOSE TRX1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRX1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.
      *-----------------------------------------------------------------

       COPY "LIBGB/GRBRLS_SQL.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.


      **************************************
       END-PROGRAM SECTION.
      **************************************
       END-RUN.
           PERFORM PRINT-RANGE-LINES.

           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
           MOVE ED-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
       EXIT-PROG.
           IF SV-BEG-BR NOT = 9999
              MOVE SV-BEG-BR TO BEG-BRANCH
              MOVE SV-END-BR TO END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY GLDIST-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
