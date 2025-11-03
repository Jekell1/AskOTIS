       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RECAPJ.
       DATE-WRITTEN.  020394
      *************************************************************
      *   DIRECTORY  :  LP
      *   DESCRIPTION:  EOD RECAPJ (NO UPDATE)
      *
      *   R E M E M B E R:    CHANGE LP/LPEOD5.C & LP/GLRCAP.C & LP/GLDIST.C
      *
      * USES GROUP LOGIC.
      * LOOPS THRU THE CAFILE FOR THE DAY AND PRINTS DETAIL LINES.
      * LOOPS THRU ALL TRFILES - P,D,C,L,X,B,J,K,S USING DATE RANGE
      *      AND WRITES WORK FILE (LPEOD5TR.CPY)
      * END OF BRANCH, LOOPS THRU WORK FILE AND PRINTS GL DETAIL ENTRIES,
      *      LOOPS THRU TYFILE FOR TODAY AND PRINTS DAILY TOTALS.
      *
      * REV:
      *  BAH 030494 REMOVED EXTRA DASH LINES
      *  JTG 031694 ADDED LOGIC FOR BULK / BRANCH TRANSFER
      *  MJD 062395 ADDED REPO INFORMATION.
      *  MJD 081495 ADDED LOGIC FOR GP-THE-ONLY-SF-SRCD.
      *  BAH 101195 ADDED REMOVE-WORKFILE OF ED-PATH
      *  JFF 111695 REDEFINED DETAIL-LINE
      *  MJD 012496 ADDED NEW PL TABLE LOGIC.
      *  MJD 020196 ADDED TY-PL-O2COL.
      *  JTG 101096 CHANGED TO HANDLE CLASS TRANSFERS
      *  JTG 111696 CHANGED GL-DESC FOR TFR TO SERV & TFR TO PUR
      *             "   SERV/PUR TO PUR/SERV TRANSFER JOURNAL"
      *  JTG 111296 CHANGED TO HANDLE ACCOUNT CONVERSION
      *  MJD 060597 ADDED O3 REBATES.
      *  GLF 090897 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *  BLV 090997 CHANGED LP01ED; EXPANDED AMT
      *  BLV 101597 ADDED GROUP LOGIC, CROSS DATA PATHS, FIX RUNNING
      *             IN BATCH (PROGRAM HAD GETFM AS 1ST STATEMENT)
      *  MJD 051198 ADDED DETAIL LINES FOR "RB" AND "RO".
      *  BAH 062698 TILLNO IS NOW ALPHANUMERIC
      *  BLV 990302 PRINT OCCURRENCE 8 OF TY-CKWRITTEN TABLE (NOTES
      *             PAYABLE CHECKS) ON CASH SUMMARY PAGE
      *  BLV 990304 WASN'T PRINTING NOTES CASH COLUMN IN CASH
      *             DRAWER SUMMARY
      *  BLV 990820 CHANGE TO PRINT BUCKETS 4 & 5 OF REPO TABLE, NOT
      *             PLA, PLI; TFC PR#20, GREATER SUBURBAN PR#809
      *  GLF 082499 ADD ARCHIVE PATH INFO. FOR TR FILES NOT FOUND IN DATA
      *             SET FOR SELECTED MONTH AND YEAR
      *  BLV 990826 FIX ARCHIVE PATH LOGIC
      *  BLV 990920 DON'T ADD TY-O2COL TO TOTAL-PLA-RECV, IT IS ALREADY
      *             IN TY-PRIN-COL SO Z2 WAS DOUBLING RECOVERED AMT
      *  MJD 000223 ADDED PRINT OF FAILED ATTEMPTS TO BALANCE CASH DRAWER
      *             BASED ON BR-HIDE-END-BAL FLAG (WORLD PR#219)
      *  MJD 000314 FIXED BUG IN CASH DRAWER SUMMARY. WHEN I ADDED
      *             THE HIDE END BAL LOGIC I PUT EXP END BAL IN O/S.
      *  MJD 000601 ADDED LOGIC TO SKIP PRINTING OF CKWRITTEN(4) IF IT IS ZERO.
      *  JTG 000613 CHANGED UPDATE TEST ON GP-LOAN-CONVERSION-UPD-FG
      *             TO ALWAYS CREATE A TRK TRANSACTION RECORD         #1203
      *             AND TO TEST WHETHER TO CREATE A GT RECORD
      *  MJD 000929 ADDED TY-LOCKBOX (MFC PR#237)
      *  BAH 010319 ADDED BRANCH SECURITY PASSWORD TESTING (BRSECURE)
      *  BLV 010507 FIX TO WORK WITH MULTIPLE BRANCH RANGE, WAS OPENING EDFILE
      *             TO OUTPUT AFTER THE FIRST BRANCH SO FOLLOWING BRANCH INFO
      *             WAS INCORRECT; ALSO CLEARED TOTALS FOR NEW BRANCH
      *  JTG 021121 ADDED NEW CODE FILE TYPE 'BR' -BATCH PAYMENT REFERENCE CODES
      *             BATCH PAYMENT REVISIONS                         REGACC #0006
      *  GLB 030508 WORLD PR#323 ADDED NEW "CT-FILE" FOR LIBLP/LPEOD5TR
      *             CODE TRAILER G/L INTERFACE TABLE & AMOUNT
      *             ONLY OCCURS IF CD-GC-FEE-GIFTNET = "Y"
      *  BAH 030722 IF DAY NEVER OPENED OR CLOSED, NO DEPOSIT TR RECORD, IT
      *             WAS PRINTING TD-DEPOSIT WITH GARBAGE...CASHCO
      *  CS  031111 ADDED FEE-COMMISSION [WORLD PR#331]
      * JTG 040330 ADDED LOGIC FOR POSTING BATCH PAYMENTS TO ACCOUNT'S
      *            WITH 'LN-OTBAL'                                 CENTRIX #2253
      *            ADDED BATCH-REFCD-OT-GLNO
      *  BLF 050921 IF DAY HAS ALREADY BEEN UPDATED, READ RC FOR THAT DAY'S
      *             CLOSING BANK BAL, PRINT THAT AS CLOSING BAL & USE IT TO
      *             BACK INTO OPENING BAL, WORLD PR# 433
      *  BLF 070822 BARB & CINDY.  IF I-AM-WORLD & PMT POSTED BY COLLECTION
      *             BRANCH (BR# 197) ON P&L ACCOUNT, ALTER GL UPDATE SO POSTING
      *             BRANCH GETS P&L RECOVERIES POSTED TO THEIR BRANCH,
      *             WORLD PR# 551
      *  BLF 070830 UNDID WORLD PR# 551
      *  BAH 080212 ADDED "PRINT DAILY TOTALS RECAP:" FOR HAMPTON ROADS PR#3150
      *             DID NOT CHANGE VERSION AND USED "Y" OR " " AS DEFAULT
      *   CS 081216 ADD BR-FILE, BR-USE-PARTIC-FLAT-TABLE IS TESTED IN LPEOD5TR
      *                                                    REGACC PR#232
      *  BAH 110411 UNCOMP'D CKWRITTEN, MEMORY ACCESS VIOLATION IF REALLY
      *             LARGE NUMBERS, PERF EQ PL# 735
      *   CS 150819 ADD BRANCH-COMMISSION, USED IN LPEOD5TR EXCEL PR#4821
      *   CS 150922 CHANGED BRANCH-COMMISSION TO S9(6) PR#4821 EXCEL
      *
      * KEC 2015.1120 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED RC-TRANS-DATE FROM YYYMMDD TO CCYYMMDD.
      *  CS 160201 REMOVED UNSUB,ERSUB,OROFFSUB,RFSUB,RFOFFSUB,GISUB
      *            SUBR FROM WORKING STORAGE NOT USED
      *            SAVED VERSION RECAPJ.160201
      *  BLM 160311 FIX FOR NEW TY-DATE FORMAT, PD0003
      * KEC 2016.0315 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-FEE???? TO CD-GC-????.
      *  BLM 160726 IF GP-I-AM-WORLD & CD-COMM-PERCENT = 99.99, FORCE THE
      *             PAYABLE PERCENT TO 100, FOR LIVE CHECKS, WORLD PR# 1170
      *  BLM 161012 IF BR-ALLOW-SCANNED-CKS = Y, PRINT TD-CHECK-DEPOSIT ON
      *             RECAP, WORLD PR# 1188
      *
      * KEC 2017.0110 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          REMOVED TRANS-DATE-SETTT [PIC 9(7)] SINCE NOT USED.
      *
      *  BAH 170712 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 180716 DRAWER # WAS BEING TRUNCATED.
      *  BAH 181018 GLOBAL TYFILE
      *   CS 181213 IF WORLD, CHANGE BANK TRANSACTIONS ID, IW, & BK TO
      *             UPDATE TO DEPOSIT BUCKET IN ED-FILE, SO THEY WILL
      *             BE UPDATED TO GTFILE WITH EXPLANATION OF "EOD-DEPOSIT",
      *             WORLD PR# 1353
      *             RECAPJ,GLRCAP,GLDIST,CL/CK1497 WERE SKIPPED IN
      *             ORIGINAL CHANGE
      * BAH 190131 CHANGED CDFILE TO CDBFILE FOR "GC", REMOVED CTFILE
      * BAH 190925 INSTEAD OF AN ACCESS-CALL ON TRFILE LIKE A15, READ RCFILE
      *            AND SEE IF DAY HAS BEEN OPENED, IF NOT, SKIP THIS BRANCH
      * BAH 20190930 REMOVED ACCESS-CALL TRP, TRD, TRC, TRL, TRX, TRB, TRJ,
      *              TRK, TRS, TRW
      * BAH 20200625 ADDED OPEN-CDB1-FILE, OPEN-CD1-FILE, OPEN-LC1-FILE,
      *              OPEN-GI1-FILE, REMOVED THEM FROM LPEOD5TR, #TST001
      * BAH 20210422 ADDED TR-POSTING-BR AND TR-OTHBR-FG FOR GLRCAP EXTRACT
      *                                                          #1496
      * BAH 20220602 ADDED TR-ACTIONCD2 FOR GLRCAP EXTRACT, #1555
      * MB  20230920 PERFORMANCE TUNING - ADDED STORED PROCEDURES
      *              EMBEDDED SQL CALLS
      ******************************************************************
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
       COPY "LIBLP/LPFDED.CPY".

      * ED-AMT(2)  - TRANSACTION CODES (TRP)
      * ED-AMT(3)  - SALES FINANCE LOANS (TRS)
      * ED-AMT(4)  - DIRECT LOANS (TRL)
      * ED-AMT(5)  - TRANSFERS (TRX)
      * ED-AMT(6)  - CHECKS (TRC)
      * ED-AMT(7)  - DEPOSITS (TRD)
      * ED-AMT(8)  - BRANCH (BULK) TRANSFERS (TRB)
      * ED-AMT(9)  - BRANCH (CLASS) TRANSFERS (TRJ)
      * ED-AMT(10) - BRANCH (CLASS) TRANSFERS (TRJ)

       COPY "LIBLP/LPFDER.CPY".
       COPY "LIBGB/GBFDPR.CPY".

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)LP/RECAPJ S35 05/07/24-16:00:29 >".
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
       COPY "LIBLP/LP01TRW.CPY".
       COPY "LIBLP/LP01TRW_SQL.CPY".
       COPY "LIBLP/LPWSTRW.CPY".
       COPY "LIBLP/LP01CA.CPY".
       COPY "LIBLP/LP01CA_SQL.CPY".
       COPY "LIBLP/LPWSCA.CPY".
       COPY "LIBLP/LP01TY.CPY".
       COPY "LIBLP/LP01TY_SQL.CPY".
       COPY "LIBLP/LPWSTY.CPY".
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
       COPY "LIBGB/GB01RC.CPY".
       COPY "LIBGB/GB01RC_SQL.CPY".
       COPY "LIBGB/GBWSRC.CPY".
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
       COPY "LIBLP/ARRAYLCW.CPY".
       COPY "LIBGL/ARRAYGIW.CPY".
       COPY "LIBGB/ARRAYBRW.CPY".

      *----------------------------------------------------------------

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  PASS-WBEG-DATE                PIC X(10).
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

       01  WS-SELECT                     PIC X(7100).
       01  WS-WHERE                      PIC X(200).
       01  WS-ORDERBY                    PIC X(200).

       01  MISC-CONN-OPEN-SW             PIC X         VALUE "N".
       01  TR2-CURSOR-STAT               PIC X(2)      VALUE SPACES.
           88  TR2-CURSOR-STAT-GOOD                    VALUE "00".
           88  TR2-CURSOR-STAT-BAD             VALUE "  " THRU "//"
                                                     "01" THRU "~~".
       01  TR2-CONN-OPEN-SW            PIC X           VALUE "N".
       01  TOT-PROC-BRANCH-CTR         PIC 9(4)        VALUE 0.
       01  LOOP-PROC-SUB               PIC 9(4)        VALUE 0.
      *----------------------------------------------------------------

       01  ED1-IFN              PIC X(3)     VALUE "ED1".
       01  PR-IFN               PIC X(2)     VALUE "PR".

      * BASE ELEMENT NUMBERS FOR UNEARNED AND EARNINGS
      * GL ACCOUNTS:
       01  SUBR                 PIC 9.
       01  TW-SUB               PIC 9(02).

       01  FUNK-SUB             PIC 999.
      *PLSUB MUST BE PIC 9:
       01  PLSUBX.
           03  PLSUB            PIC 9.

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
           03  EOD-DATE        PIC 9(8).
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).
           03  DAILY-TOTALS    PIC X.

       01  VERSION-CONTROL     PIC X VALUE "B".

       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.

      *----------------------------------------------------------------
      * WORKERS FOR ACCOM-PL-TOTALS SECTION.
      *----------------------------------------------------------------
       01  PL-TYPE              PIC 9(6) COMP.
       01  PL-ACTY              PIC 9(6) COMP.
       01  PL-SUB3              PIC 9(6) COMP.
       01  TOTAL-NO-PL          PIC S9(9)V99 COMP.
       01  TOTAL-AMT-PL         PIC S9(9)V99 COMP.
       01  TOTAL-O2-PL          PIC S9(9)V99 COMP.
       01  TOTAL-INT-PL         PIC S9(9)V99 COMP.

       01  WS-HOLD-AMT          PIC S9(12)V99 COMP.
      **********************
      *   MISC WORKERS     *
      **********************
       01  SUB                  PIC 9(6) COMP.
       01  SUB4                 PIC 9(6) COMP.
       01  L1                   PIC 9(6) COMP.
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  CLEARING-WK          PIC S9(10)V99  COMP.
       01  TOTAL-PLA-RECV       PIC S9(9)V99 COMP.
       01  FEE-COMMISSION       PIC S9(4)V99.
       01  HOLD-GP-PROMPTX.
           03  HOLD-GP-PROMPT   PIC XX.
           03  FILLER           PIC XX       VALUE " :".

      ****************************************************
      *    THIS FLAG INDICATES THAT THE DAY WAS CLOSED
      *    AND SELECTION 1 WAS RUN
       01  DAY-WAS-CLOSED       PIC X        VALUE " ".
      ****************************************************
       01  DF-TEST              PIC X        VALUE " ".
           88  DF-POSTING                    VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  TOT-DR               PIC S9(9)V99 COMP VALUE 0.
       01  TOT-CR               PIC S9(9)V99 COMP VALUE 0.
       01  TR-CODE              PIC X.
       01  TR-DATE              PIC 9(8).
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
       01  BATCH-REFCD          PIC X(5)     VALUE LOW-VALUES.
       01  BATCH-REFCD-GLNO     PIC 9(11)    VALUE 0.
       01  BATCH-REFCD-OT-GLNO  PIC 9(11)    VALUE 0.
       01  GLNO                 PIC 9(11)    VALUE 0.
       01  GLNOX               REDEFINES GLNO.
           03  FILLER           PIC X(4).
           03  FILLER           PIC X(3).
           03  GLOTHBR          PIC 9999.
       01  GLNOZ               REDEFINES GLNO.
           03  GLNO-BR          PIC 9999.
           03  GLNO-NUMBER      PIC 9(7).

       01  GLAMT                PIC S9(9)V99 VALUE 0.
       01  GLWK                 PIC S9(9)V99 VALUE 0.
       01  HOLD-PL-EXP          PIC S9(9)V99 VALUE 0.
       01  TOTALS-TT.
           03  TOT-NO1          PIC S9(9)     COMP VALUE 0.
           03  TOT-AMT1         PIC S9(9)V99  COMP VALUE 0.
           03  TOT-NO2          PIC S9(9)     COMP VALUE 0.
           03  TOT-AMT2         PIC S9(9)V99  COMP VALUE 0.
       01  ERROR-FG             PIC X VALUE "N".

       01  TODAYS-CKWRITTEN-TAB.
           03  CKWRITTEN        PIC S9(7)V99 OCCURS 8.
       01  WK-CA-TOTALS.
           03  WK-CA-TOT        PIC S9(7)V99 COMP OCCURS 8.
       01  WK-CA                PIC S9(7)V99 COMP VALUE 0.
       01  HOLD-COMM-PERCENT    PIC 999V99 VALUE 0.
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
           03  H-TITLE          PIC X(41).

       01  HEAD3.
           03  FILLER           PIC X(10) VALUE "BRANCH :".
           03  H-BRNO           PIC ZZ99.
           03  FILLER           PIC X(21) VALUE " ".
           03  FILLER           PIC X(27) VALUE " ".
           03  H-TRDATE         PIC 99/99/99.

       01  HEAD4.
           03  FILLER           PIC X(41) VALUE
           "DRAWER/DESCRIPTION-------  TIME      TIME".
           03  FILLER           PIC X(41) VALUE
           "  BALANCED    OPENING  NET-CASH CSH-NOTES".
           03  FILLER           PIC X(50) VALUE
           " ESTIMATED      CASH    ACTUAL     CHECK      CASH".

       01  HEAD5.
           03  FILLER           PIC X(41) VALUE
           "#                        OPENED    CLOSED".
           03  FILLER           PIC X(41) VALUE
           "  BY          BALANCE  ACTIVITY  ACTIVITY".
           03  FILLER           PIC X(50) VALUE
           "   END-BAL     OV/SH   END-BAL   DEPOSIT   DEPOSIT".

       01  HEAD6.
           03  HEAD6X           PIC X(51) VALUE
           "   NET    GAIN / LOSS      NEW         VOLUME".
           03  FILLER           PIC X(29) VALUE
           "RECOVERED".
           03  FILLER           PIC X(42) VALUE
           "NET    GAIN / LOSS      NEW         VOLUME".

       01  HEAD7.
           03  FILLER           PIC X(50) VALUE
           "   -------------- P&L INACTIVE --------------".

       01  HEAD8.
           03  FILLER           PIC X(79) VALUE " ".
           03  FILLER           PIC X(43) VALUE
           "----  -------------     ----  -------------".

       01  HEAD9.
           03  FILLER           PIC X(44) VALUE " ".
           03  FILLER           PIC X(29) VALUE
           "BANK CREDITS---------------- ".
           03  FILLER           PIC X(50) VALUE
           "COLLECTED --------------- REBATES --------------".
       01  HEAD9A.
           03  FILLER           PIC X(73) VALUE " ".
           03  FILLER           PIC X(25) VALUE
           "POSTED-------------------".

       01  HEAD10.
           03  HEAD10X          PIC X(68)   VALUE
           "-----------   ------------------------------------".
           03  FILLER           PIC X(20)   VALUE
           "---------------".
           03  FILLER           PIC X(20)   VALUE
           "---------------".

       01  HEAD11.
           03  FILLER           PIC X(7)    VALUE "CLASS: ".
           03  H-LC-CODE        PIC 99.
           03  FILLER           PIC X       VALUE " ".
           03  H-LC-DESC        PIC X(16).

       01  HEAD12.
           03  FILLER           PIC X(50) VALUE
           "   --------------------- P&L ACTIVE --------------".
           03  FILLER           PIC X(23) VALUE
           "----------".
           03  FILLER           PIC X(49) VALUE
           "------------------- NORMAL ----------------------".

       01  HEAD13.
           03  FILLER           PIC X(61)   VALUE
           "G/L   #       DESCRIPTION-------------------------".
           03  FILLER           PIC X(16)    VALUE " ".
           03  FILLER           PIC X(8)    VALUE "DEBIT".
           03  FILLER           PIC X(11)   VALUE " ".
           03  FILLER           PIC X(19)   VALUE "CREDIT".

       01  HEAD14.
           03  FILLER           PIC X(68) VALUE " ".
           03  FILLER           PIC X(37) VALUE
           "---------------     ---------------".

       01  HEAD15.
           03  FILLER            PIC X(3)   VALUE SPACES.
           03  FILLER            PIC X(13)  VALUE "REPOSSESSIONS".
           03  FILLER            PIC X(5)   VALUE " --- ".
           03  FILLER            PIC X(14)  VALUE "NET    GAIN / ".
           03  FILLER            PIC X(11)  VALUE "LOSS ----- ".
           03  FILLER            PIC X(16)  VALUE "NEW       VOLUME".
           03  FILLER            PIC X(5)   VALUE " ----".

       01  HEAD16.
           03  FILLER            PIC X(19)  VALUE SPACES.
           03  FILLER            PIC X(8)   VALUE "-----   ".
           03  FILLER            PIC X(12)  VALUE "------------".
           03  FILLER            PIC X(13)  VALUE "     -----  ".
           03  FILLER            PIC X(12)  VALUE "------------".

       01  SAVE-HEAD4           PIC X(132).
       01  SAVE-HEAD5           PIC X(132).

       01  DETAIL-LINE          PIC X(132) VALUE " ".

       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  D-G-BRNO         PIC Z9(3).
           03  D-G-HYPHEN       PIC X.
           03  D-G-GLNO         PIC 9(7).
           03  FILLER           PIC X.
           03  D-G-POST         PIC X.
           03  FILLER           PIC X.
           03  D-G-DESCRIPTION  PIC X(36).
           03  FILLER           PIC X(17).
           03  D-G-DR           PIC ZZZ,ZZZ,ZZ9.99- BLANK ZERO.
           03  FILLER           PIC X(5).
           03  D-G-CR           PIC ZZZ,ZZZ,ZZ9.99- BLANK ZERO.
           03  FILLER           PIC X(5).

       01  DETAIL-LINE2 REDEFINES DETAIL-LINE.
           03  D-P-NO1          PIC ZZ,ZZ9-       BLANK ZERO.
           03  D-P-AMT1         PIC ZZZ,ZZZ,ZZ9.99-  BLANK ZERO.
           03  D-P-NO2          PIC ZZZZ,ZZ9-        BLANK ZERO.
           03  D-P-AMT2         PIC ZZZ,ZZZ,ZZ9.99-  BLANK ZERO.
           03  D-P-AMT3         PIC ZZZ,ZZZ,ZZ9.99-  BLANK ZERO.
           03  FILLER           PIC X(12).
           03  D-N-PROMPT1      PIC X(4).
           03  D-N-NO1          PIC ZZ,ZZ9-          BLANK ZERO.
           03  D-N-PROMPT2      REDEFINES D-N-NO1
                                PIC X(7).
           03  D-N-AMT1         PIC ZZZ,ZZZ,ZZ9.99-  BLANK ZERO.
           03  D-N-NO2          PIC ZZZZ,ZZ9-        BLANK ZERO.
           03  D-N-PROMPT3      REDEFINES D-N-NO2
                                PIC X(9).
           03  D-N-AMT2         PIC ZZZ,ZZZ,ZZ9.99-  BLANK ZERO.

       01  DETAIL-LINE3 REDEFINES DETAIL-LINE.
           03  D-TILLNO         PIC 99.
           03  FILLER           PIC X.
           03  D-CA-DESC        PIC X(19).
           03  FILLER           PIC X.
           03  D-OPENED         PIC X(8).
           03  FILLER           PIC XX.
           03  D-CLOSED         PIC X(8).
           03  FILLER           PIC XX.
           03  D-BALBY          PIC X(10).
           03  D-CASHBALS       OCCURS 8.
               05  D-CASHBAL    PIC ZZZZZ9.99- BLANK ZERO.

       01  DETAIL-LINE4 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(22).
           03  D-CASH-TOT       PIC ZZZ,ZZZ,ZZ9.99-.
           03  D-NOT-CLOSED     PIC X(95).
       01  DETAIL-LINE5 REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(51).
           03  D-C-PROMPT       PIC X(5).
           03  D-C-AMT1         PIC ZZZ,ZZZ,ZZ9.99-.

       01  DETAIL-LINE6 REDEFINES DETAIL-LINE.
           03  FILLER            PIC X(6).
           03  D-REPO-DESC       PIC X(12).
           03  FILLER            PIC X(1).
           03  D-REPO-NO         PIC ZZZZ9-.
           03  FILLER            PIC XX.
           03  D-REPO-AMT        PIC ZZZZZZZZ9.99-.
           03  FILLER            PIC X(4).
           03  D-REPO-NO2        PIC ZZZZ9-.
           03  FILLER            PIC XX.
           03  D-REPO-AMT2       PIC ZZZZZZZZ9.99-.
           03  FILLER            PIC X(67).

       COPY "LIBWI/GRSCANW.CPY".
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
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/RECAPJ_DEF.CPY".
       COPY "LIBLP/RECAPJ_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ****************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/RECAPJ_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               ED-FILE PR-FILE ER-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TY1-FILE.
           PERFORM CLOSE-TRX1-FILE.
           PERFORM CLOSE-TRW1-FILE.
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
           PERFORM CLOSE-CA1-FILE.
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-GI1-FILE.
           PERFORM CLOSE-RC1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           PERFORM CLOSE-MISC-CONNECTION.
           PERFORM CLOSE-TR2-CONNECTION.
           CLOSE
               ED-FILE ER-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *************************************************************
       MAIN-MODULE SECTION.
      *************************************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "RECAPJ" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.

      D    STOP MESS.

      *    IF GROUP-FLAG = "N"
      *       MOVE BEG-BRANCH TO BR-NO
      *       PERFORM START-BR-FILE
      *       GO TO NEXT-BRANCH-NO.
      *
      *    MOVE ENT-GROUP TO GR-ID.
      *    PERFORM SEARCH-GR-INIT.

      * LOAD BRANCHES TO PROCESS USING STORED PROCEDURE SSP_TCLP_BRANCH_GROUP_LOGIC_SELECT
      *   INTO BGLS-BRANCH-TBL
           IF GROUP-FLAG = "Y"
               MOVE ENT-GROUP TO BGLS-PASS-BEG-BRGRP
               MOVE SPACES    TO BGLS-PASS-END-BRGRP
           ELSE
               MOVE BEG-BRANCH                TO BRANCHLIST-WORK-BRNO
               MOVE BRANCHLIST-WORK-BRNO-TEXT TO BGLS-PASS-BEG-BRGRP
               MOVE END-BRANCH                TO BRANCHLIST-WORK-BRNO
               MOVE BRANCHLIST-WORK-BRNO-TEXT TO BGLS-PASS-END-BRGRP.

           PERFORM LOAD-BGLS-BRANCH-LIST.

      * SET BRANCHLIST-PGM-PROC-MAX TO NUMBER OF BRANCHES TO PROCESS PER STORED PROCEDURE CURSOR
           MOVE 300 TO BRANCHLIST-PGM-PROC-MAX.

      * RETRIEVE BRANCHES WE WANT TO PROCESS FROM BGLS-BRANCH-TBL
      * WILL PROCESS UNTIL END OF BGLS-BRANCH-TBL OR BRANCHLIST-PGM-PROC-MAX
       PROC-LOOP-INIT.
           INITIALIZE BRANCHLIST-TBL BRANCHLIST-SUB
                      BRANCHLIST-PROC-BRANCH-CTR 
                      PASS-BRANCHLIST. 

       PROC-LOOP.
           ADD 1 TO LOOP-PROC-SUB.

           IF LOOP-PROC-SUB > BGLS-BRANCHES-RETR-CTR
              GO TO PROC-LOOP-CONT.

           ADD 1 TO BRANCHLIST-PROC-BRANCH-CTR
                    BRANCHLIST-SUB.

           MOVE BGLS-BRANCH-BRNO(LOOP-PROC-SUB) TO 
                            BRANCHLIST-BRNO(BRANCHLIST-SUB).
           IF BRANCHLIST-SUB > 1
              MOVE ","   TO BRANCHLIST-COMMA(BRANCHLIST-SUB).

           IF BRANCHLIST-PROC-BRANCH-CTR < BRANCHLIST-PGM-PROC-MAX 
               GO TO PROC-LOOP.
 
       PROC-LOOP-CONT.
           IF BRANCHLIST-SUB = 0
               GO TO END-PROGRAM.

           MOVE BRANCHLIST-TBL TO PASS-BRANCHLIST.
           PERFORM OPEN-TR2-CURSOR.

       NEXT-GROUP-LOC.
           PERFORM READ-TR2-FILE-NEXT.
           IF IO-BAD 
              PERFORM CLOSE-TR2-CURSOR
              GO TO PROC-LOOP-INIT.

      *     PERFORM OPEN-BR-FILE.
      *     PERFORM READ-BR-FILE.
           PERFORM ARRAYBR-READ.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.


      *    IF GROUP-FLAG NOT = "Y"
      *       GO TO NEXT-BRANCH-NO.
      *
      *    PERFORM SEARCH-GR-NEXT.
      *    IF WGR-SRESULT = 0
      *       GO TO END-PROGRAM.
      *
      *    MOVE WGR-SRESULT TO BR-NO.
      *    PERFORM READ-BR-FILE.
      *    IF IO-FG NOT = 0
      *       GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

      * NEXT-BRANCH-NO.
      *     PERFORM READ-BR-FILE-NEXT.
      *     IF IO-FG = 9 OR BR-NO > END-BRANCH
      *        GO TO END-PROGRAM.

       MOVE-BRANCH-PATH-INFO.
           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO CA-PATH-OVERRIDE
                                              TY-PATH-OVERRIDE
                                              TRL-PATH-OVERRIDE
                                              TRS-PATH-OVERRIDE
                                              TRP-PATH-OVERRIDE
                                              TRX-PATH-OVERRIDE
                                              TRC-PATH-OVERRIDE
                                              TRD-PATH-OVERRIDE
                                              TRB-PATH-OVERRIDE
                                              TRJ-PATH-OVERRIDE
                                              TRK-PATH-OVERRIDE
                                              TRW-PATH-OVERRIDE.

           MOVE BR-NO TO H-BRNO.

      * INSTEAD OF AN ACCESS-CALL ON TRFILE LIKE A15, READ RCFILE
      * AND SEE IF DAY HAS BEEN OPENED, IF NOT, SKIP THIS BRANCH

           PERFORM OPEN-RC1-FILE.
           MOVE BR-NO    TO RC-BRNO.
           MOVE EOD-DATE TO RC-TRANS-DATE.
           PERFORM READ-RC1-FILE.
           PERFORM CLOSE-RC1-FILE.
           IF IO-FG NOT = 0 OR RC-OPENED-DATE = 0
              GO TO NEXT-GROUP-LOC.

           MOVE BR-NO   TO H-BRNO.

           PERFORM CASH-TILL-SUMMARY.

           MOVE "  - - - DAILY TOTALS RECAP JOURNAL - - -" TO H-TITLE.

           IF PRU-LP NOT = "VDU" AND NOT = "PRU"
              STRING "DAILY TOTALS RECAP IN PROGRESS....." , BR-NO
                     DELIMITED "  " INTO MESS
              PERFORM PROGRESS-MSG.

           MOVE " " TO HEAD4 HEAD5.
           MOVE HEAD13 TO HEAD4.
           MOVE HEAD14 TO HEAD5.
           MOVE 99 TO LN-COUNT.

      * RELOAD GI TABLE AT BRANCH BREAKS.
           MOVE BR-NO TO ARRAYGI-GI-BRANCH.
           MOVE ZEROS TO ARRAYGI-GI-CLASS.
           PERFORM ARRAYGI-LOAD.

      * TRANSACTIONS
           IF WS-TRPSW = 'Y'
               PERFORM PROCESS-TRP-FILE.
      * DEPOSITS
           IF WS-TRDSW = 'Y'
              PERFORM PROCESS-TRD-FILE.
      * CHECKS
           IF WS-TRCSW = 'Y'
              PERFORM PROCESS-TRC-FILE.
      * DIRECT LOANS
           IF WS-TRLSW = 'Y'
              PERFORM PROCESS-TRL-FILE.
      * TRANSFERS
           IF WS-TRXSW = 'Y'
              PERFORM PROCESS-TRX-FILE.
      * BRANCH (BULK) TRANSFERS
           IF WS-TRBSW = 'Y' 
              PERFORM PROCESS-TRB-FILE.
      * BRANCH (CLASS) TRANSFERS
           IF WS-TRJSW = 'Y'
              PERFORM PROCESS-TRJ-FILE.
      * ACCOUNT CONVERSIONS
              PERFORM PROCESS-TRK-FILE.
      * SALES FINANCE
           IF WS-TRSSW = 'Y'
              PERFORM PROCESS-TRS-FILE.

       GL-SUMMARY.
           IF ERROR-FG = "Y"
              GO TO TODAYS-SUMMARY.

           PERFORM CLOSE-ED1-FILE.
           PERFORM OPEN-ED1-FILE.

           MOVE 0 TO ED-NO.

           PERFORM START-ED1-FILE.

       NEXT-GL-SUMMARY.
           PERFORM READ-ED1-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-GL-SUMMARY.

           PERFORM PRINT-GL-DETAIL.

           GO TO NEXT-GL-SUMMARY.

       END-GL-SUMMARY.
           PERFORM PRINT-GL-TOTALS.

       TODAYS-SUMMARY.
           PERFORM UPDATE-BYFILE.
           IF ERROR-FG = "Y"
              MOVE "E" TO EXT-EOBUF-ERRCD.

           PERFORM CLOSE-ED1-FILE.
           PERFORM OPEN-ED1-FILE-OUTPUT.
           PERFORM CLOSE-ED1-FILE.
           PERFORM OPEN-ED1-FILE.
           MOVE SAVE-HEAD4 TO HEAD4.
           MOVE SAVE-HEAD5 TO HEAD5.
           MOVE 99 TO LN-COUNT.
           MOVE 0 TO TOT-DR TOT-CR.

           GO TO NEXT-GROUP-LOC.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE RECAPJ-QUEUE-POS TO Q-POS.
           MOVE RECAPJ-COPIES-POS TO C-POS.
           MOVE RECAPJ-PRU-POS   TO P-POS.
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
           MOVE EOD-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO H-TRDATE.

           IF BEG-BRANCH = 0
              IF END-BRANCH = 0
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH
                                             END-BRANCH.

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

      *******************************************
       EO-EXEC-AUDIT SECTION.
      *******************************************
           MOVE EXT-EOBUF-DATE2 TO EOD-DATE.

      *******************************************
       ENTER-PARAMETERS SECTION.
      *******************************************
       ENTER-PARM.
           IF VDUSTATUS = "1U"
              IF EXT-ROUTE-BUF = "00"
                 GO TO ENTER-DAILY-RECAP
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
              GO TO CHECK-BRANCH-SECURITY.

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

       ENTER-DATE.
           IF EXT-ROUTE-BUF NOT = "00" GO TO ENTER-DAILY-RECAP.
           MOVE LEGEND-TRANS-DTE TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ERNM060DATE1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-DATE.
           MOVE VDU-DATE-YYYYMMDD TO EOD-DATE.

           IF EOD-DATE = 0
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD
              MOVE "S  8000DATE1." TO VDU
              MOVE DATE-YYYYMMDD TO EOD-DATE VDU-DATE-YYYYMMDD
              PERFORM SCREENIO.
       ENTER-DAILY-RECAP.
           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010DAILY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-DATE.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-DAILY-RECAP.
           MOVE VDUBUF TO DAILY-TOTALS.
           IF DAILY-TOTALS NOT = "Y" AND NOT = "N"
              GO TO ENTER-DAILY-RECAP.
       END-PARM.
           MOVE LEGEND-ACCEPT TO LEGEND.
           PERFORM SEND-LEGEND.
       EXIT-PARM.
           EXIT.

      *******************************************
       DISPLAY-PARAMETERS SECTION.
      *******************************************

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

           MOVE "S  8000DATE1." TO VDU.
           MOVE EOD-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "S  A010DAILY." TO VDU.
           MOVE DAILY-TOTALS TO VDUBUF.
           PERFORM SCREENIO.

      ****************************
       CASH-TILL-SUMMARY SECTION.
      ****************************

           MOVE "C A S H   D R A W E R   S U M M A R Y" TO H-TITLE.

           MOVE 0 TO SUB
              WK-CA-TOT(1) WK-CA-TOT(2) WK-CA-TOT(3)
              WK-CA-TOT(4) WK-CA-TOT(5) WK-CA-TOT(6)
              WK-CA-TOT(7) WK-CA-TOT(8).

           PERFORM OPEN-CA1-FILE.

           MOVE BR-NO    TO CA-BRNO
                            QCA1-WBEG-BRNO
                            QCA1-WEND-BRNO.
           MOVE EOD-DATE TO CA-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QCA1-WBEG-DATE
                                       QCA1-WEND-DATE.
           MOVE 0        TO CA-TILLNO
                            QCA1-WBEG-TILLNO
           MOVE ALL "9"  TO QCA1-WEND-TILLNO.

           PERFORM START-CA1-FILE.
       NEXT-CASH-TILL.
           PERFORM READ-CA1-FILE-NEXT.
           IF IO-FG = 9 OR (CA-BRNO NOT = CA-PATH-OWNBR)
              PERFORM CLOSE-CA1-FILE
              GO TO CASH-TILL-SUMMARY-CHECKS.

           IF CA-DATE NOT = EOD-DATE
              PERFORM CLOSE-CA1-FILE
              GO TO CASH-TILL-SUMMARY-CHECKS.

           MOVE CA-TILLNO      TO D-TILLNO.
           MOVE CA-DESC        TO D-CA-DESC.
           MOVE CA-OPENED-HHMM TO H-AND-M.
           PERFORM CONVERT-TIME.
           MOVE TIME-EDIT      TO D-OPENED.
           MOVE CA-CLOSED-HHMM TO H-AND-M.
           PERFORM CONVERT-TIME.
           MOVE TIME-EDIT      TO D-CLOSED.
           MOVE CA-BALBY       TO D-BALBY.

           MOVE CA-BEGBAL         TO D-CASHBAL(1).
           ADD CA-BEGBAL          TO WK-CA-TOT(1).
           MOVE CA-RUNNING        TO D-CASHBAL(2).
           ADD CA-RUNNING         TO WK-CA-TOT(2).
           MOVE CA-NET-CASH-NOTES TO D-CASHBAL(3).
           ADD CA-NET-CASH-NOTES  TO WK-CA-TOT(3).
           ADD CA-BEGBAL CA-RUNNING CA-NET-CASH-NOTES GIVING WK-CA.
           MOVE WK-CA             TO D-CASHBAL(4).
           ADD WK-CA              TO WK-CA-TOT(4).
           IF CA-CLOSED-HHMM NOT = 0
              SUBTRACT WK-CA FROM CA-ENDBAL GIVING WK-CA
              MOVE WK-CA TO D-CASHBAL(5)
              ADD WK-CA TO WK-CA-TOT(5).

           MOVE CA-ENDBAL           TO D-CASHBAL(6).
           ADD CA-ENDBAL            TO WK-CA-TOT(6).
           IF BR-ALLOW-SCANNED-CKS = "Y"
              MOVE CA-CHECK-DEPOSIT TO D-CASHBAL(7)
              ADD CA-CHECK-DEPOSIT  TO WK-CA-TOT(7).
           IF BR-AUTO-OPEN-BAL = "Y"
              MOVE CA-DEPOSIT       TO D-CASHBAL(8)
              ADD CA-DEPOSIT        TO WK-CA-TOT(8).
           IF CA-BALBY = " "
              IF BR-HIDE-END-BAL = "Y"
                 MOVE " ********** " TO D-CASHBALS(4)
                                        D-CASHBALS(3)
                                        D-CASHBALS(4)
                                        D-CASHBALS(5).
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF BR-HIDE-END-BAL = "Y"
              PERFORM PRINT-TRW-RECORDS.

           GO TO NEXT-CASH-TILL.
       CASH-TILL-SUMMARY-CHECKS.

           ADD 1 TO SUB.
           MOVE WK-CA-TOT(SUB) TO D-CASHBAL(SUB).
           IF SUB < 8
              GO TO CASH-TILL-SUMMARY-CHECKS.

           MOVE "CASH DRAWER TOTALS..........." TO D-CA-DESC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 0 TO CKWRITTEN(1) CKWRITTEN(2)
              CKWRITTEN(3) CKWRITTEN(4) CKWRITTEN(5) CKWRITTEN(6)
              CKWRITTEN(7) CKWRITTEN(8).

           PERFORM GET-TY-CHECK-SUMMARY.

           PERFORM OPEN-TRD1-FILE.

           MOVE BR-NO    TO TD-BRNO.
           MOVE EOD-DATE TO TD-DATE.

           PERFORM READ-TRD1-FILE.
           IF IO-FG = 0
              MOVE "Y" TO DAY-WAS-CLOSED
           ELSE
              MOVE "N" TO TD-GLFG
              MOVE 0   TO TD-DEPOSIT.

           IF TD-GLFG = "Y"
              PERFORM OPEN-RC1-FILE
              MOVE BR-NO    TO RC-BRNO
              MOVE EOD-DATE TO RC-TRANS-DATE
              PERFORM READ-RC1-FILE
              IF IO-BAD
                 MOVE 0 TO RC-BANK-BAL
              ELSE
                 IF RC-BANK-BAL = 202020202.02
                    MOVE 0 TO RC-BANK-BAL.

           MOVE 4 TO LN-FEED.
           IF GP-BANK-GL = 0
              MOVE "OPENING BANK BALANCE:" TO DETAIL-LINE
              IF TD-GLFG = "Y"
                 MOVE "UPDATE PREVIOUSLY RUN"  TO D-NOT-CLOSED
                 COMPUTE D-CASH-TOT =
                    RC-BANK-BAL - TD-DEPOSIT
                          + CKWRITTEN(1) + CKWRITTEN(2)
                           + CKWRITTEN(3) + CKWRITTEN(4)
                            - CKWRITTEN(5) + CKWRITTEN(6)
                             + CKWRITTEN(7) + CKWRITTEN(8)
                 PERFORM WRITE-DETAIL-LINE
              ELSE
                 MOVE BR-BANKBAL TO D-CASH-TOT
                 PERFORM WRITE-DETAIL-LINE.

           MOVE "+ CASH DEPOSITS     :" TO DETAIL-LINE.
           IF DAY-WAS-CLOSED = " "
              MOVE "THE DAY WAS NOT CLOSED!" TO D-NOT-CLOSED
              MOVE 0 TO TD-DEPOSIT
           ELSE
              MOVE TD-DEPOSIT TO D-CASH-TOT.
           PERFORM WRITE-DETAIL-LINE.
           IF BR-ALLOW-SCANNED-CKS = "Y"
              MOVE "+ CHECK DEPOSITS    :" TO DETAIL-LINE
              MOVE TD-CHECK-DEPOSIT TO D-CASH-TOT 
              PERFORM WRITE-DETAIL-LINE.

           MOVE "- LOAN CHECKS       :" TO DETAIL-LINE.
           MOVE CKWRITTEN(1) TO D-CASH-TOT.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "- DEALER CHECKS     :" TO DETAIL-LINE.
           MOVE CKWRITTEN(2) TO D-CASH-TOT.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "- MISC CHECK        :" TO DETAIL-LINE.
           MOVE CKWRITTEN(3) TO D-CASH-TOT.
           PERFORM WRITE-DETAIL-LINE.
           IF CKWRITTEN(4) NOT = 0
              MOVE "- OVERPAYMENT CHECKS:" TO DETAIL-LINE
              MOVE CKWRITTEN(4) TO D-CASH-TOT
              PERFORM WRITE-DETAIL-LINE.
           MOVE "- NSF CHECKS        :" TO DETAIL-LINE.
      *  DONT WANT THE - SIGN TO PRINT
           COMPUTE D-CASH-TOT = CKWRITTEN(5) * -1.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "- BANK WITHDRAWALS  :" TO DETAIL-LINE.
           MOVE CKWRITTEN(6) TO D-CASH-TOT.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "+ INTERIM DEPOSITS  :" TO DETAIL-LINE.
      *  DONT WANT THE - SIGN TO PRINT
           COMPUTE D-CASH-TOT = CKWRITTEN(7) * -1.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "- NOTES CHECKS      :" TO DETAIL-LINE.
           MOVE CKWRITTEN(8) TO D-CASH-TOT.
           PERFORM WRITE-DETAIL-LINE.
           IF GP-BANK-GL = 0
              MOVE "CLOSING BANK BALANCE:" TO DETAIL-LINE
              IF TD-GLFG = "Y"
                 MOVE RC-BANK-BAL TO D-CASH-TOT
                 PERFORM WRITE-DETAIL-LINE
              ELSE
                 COMPUTE WS-HOLD-AMT =
                    BR-BANKBAL + TD-DEPOSIT
                          - CKWRITTEN(1) - CKWRITTEN(2)
                           - CKWRITTEN(3) - CKWRITTEN(4)
                            + CKWRITTEN(5) - CKWRITTEN(6)
                             - CKWRITTEN(7) - CKWRITTEN(8)
                 MOVE WS-HOLD-AMT TO D-CASH-TOT
                 PERFORM WRITE-DETAIL-LINE.
       CASH-TILL-SUMMARY-EXIT.
           MOVE HEAD4 TO SAVE-HEAD4.
           MOVE HEAD5 TO SAVE-HEAD5.
           MOVE SPACE TO HEAD4 HEAD5.


      *****************************************
       PROCESS-TRP-FILE SECTION.
      *****************************************
           PERFORM OPEN-TRP1-FILE.

           MOVE BR-NO               TO TP-BRNO
                                       QTRP1-WBEG-BRNO
                                       QTRP1-WEND-BRNO.

           MOVE EOD-DATE      TO TP-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRP1-WBEG-DATE 
                                       QTRP1-WEND-DATE.

           PERFORM START-TRP1-FILE. *> SSP - TRP

       PROCESS-TRP-NEXT.
           PERFORM READ-TRP1-FILE-NEXT.   *> EMBEDDED - TRP
           IF (IO-FG NOT = 0)   
      *  OR (TRP-PATH-OWNBR NOT = TP-BRNO)
              GO TO EXIT-PROCESS-TRP-FILE.

           IF TP-DATE NOT = EOD-DATE
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

           MOVE EOD-DATE      TO TS-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRS1-WBEG-DATE 
                                       QTRS1-WEND-DATE.

           PERFORM START-TRS1-FILE. *> SSP  TRS

       PROCESS-TRS-NEXT.
           PERFORM READ-TRS1-FILE-NEXT.   *> EMBEDDED - TRS
           IF (IO-FG NOT = 0) 
      *  OR (TRS-PATH-OWNBR NOT = TS-BRNO)
              GO TO EXIT-PROCESS-TRS-FILE.

           IF TS-DATE NOT = EOD-DATE
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

           MOVE EOD-DATE      TO TL-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRL1-WBEG-DATE 
                                       QTRL1-WEND-DATE.

           PERFORM START-TRL1-FILE.   *> SSP - TRL

       PROCESS-TRL-NEXT.
           PERFORM READ-TRL1-FILE-NEXT.   *> EMBEDDED - TRL
           IF (IO-FG NOT = 0)
      *  OR (TRL-PATH-OWNBR NOT = TL-BRNO)
              GO TO EXIT-PROCESS-TRL-FILE.

           IF TL-DATE NOT = EOD-DATE
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

           MOVE EOD-DATE      TO TX-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRX1-WBEG-DATE 
                                       QTRX1-WEND-DATE.

           PERFORM START-TRX1-FILE.   *> SSP - TRX

       PROCESS-TRX-NEXT.
           PERFORM READ-TRX1-FILE-NEXT.   *> EMBEDDED - TRX
           IF (IO-FG NOT = 0)
      *  OR (TRX-PATH-OWNBR NOT = TX-BRNO)
              GO TO EXIT-PROCESS-TRX-FILE.

           IF TX-DATE NOT = EOD-DATE
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

           MOVE EOD-DATE      TO TB-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRB1-WBEG-DATE 
                                       QTRB1-WEND-DATE.

           PERFORM START-TRB1-FILE.   *> SSP - TRB

       PROCESS-TRB-NEXT.
           PERFORM READ-TRB1-FILE-NEXT.   *> EMBEDDED - TRB
           IF (IO-FG NOT = 0)
      *  OR (TRB-PATH-OWNBR NOT = TB-BRNO)
              GO TO EXIT-PROCESS-TRB-FILE.

           IF TB-DATE NOT = EOD-DATE
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

           MOVE EOD-DATE      TO TJ-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRJ1-WBEG-DATE 
                                       QTRJ1-WEND-DATE.

           PERFORM START-TRJ1-FILE.   *> SSP - TRJ

       PROCESS-TRJ-NEXT.
           PERFORM READ-TRJ1-FILE-NEXT.   *> EMBEDDED - TRJ
           IF (IO-FG NOT = 0)
      *  OR (TRJ-PATH-OWNBR NOT = TJ-BRNO)
              GO TO EXIT-PROCESS-TRJ-FILE.

           IF TJ-DATE NOT = EOD-DATE
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
 
           MOVE EOD-DATE      TO TK-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRK1-WBEG-DATE 
                                       QTRK1-WEND-DATE.

           PERFORM START-TRK1-FILE.   *> SSP - TRK

       PROCESS-TRK-NEXT.
           PERFORM READ-TRK1-FILE-NEXT.   *> EMBEDDED - TRK
           IF (IO-FG NOT = 0)
      *  OR (TRK-PATH-OWNBR NOT = TK-BRNO)
              GO TO EXIT-PROCESS-TRK-FILE.

           IF TK-DATE NOT = EOD-DATE
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

           MOVE EOD-DATE TO TC-KEY-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRC1-WBEG-KEY-DATE 
                                       QTRC1-WEND-KEY-DATE.

           PERFORM START-TRC1-FILE.   *> SSP - TRC

       PROCESS-TRC-NEXT.
           PERFORM READ-TRC1-FILE-NEXT.   *> EMBEDDED - TRC
           IF (IO-FG NOT = 0)
      *  OR (TRC-PATH-OWNBR NOT = TC-BRNO)
              GO TO EXIT-PROCESS-TRC-FILE.

           IF TC-KEY-DATE NOT = EOD-DATE
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

           MOVE EOD-DATE    TO TD-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QTRD1-WBEG-DATE 
                                        QTRD1-WEND-DATE.

           PERFORM START-TRD1-FILE.   *> SSP - TRD

       PROCESS-TRD-NEXT.
           PERFORM READ-TRD1-FILE-NEXT.   *> EMBEDDED - TRD
           IF (IO-FG NOT = 0) OR (TRD-PATH-OWNBR NOT = TD-BRNO)
              GO TO EXIT-PROCESS-TRD-FILE.

           IF TD-DATE NOT = EOD-DATE
              GO TO EXIT-PROCESS-TRD-FILE.

           PERFORM TRD1-DETAIL.

           GO TO PROCESS-TRD-NEXT.

       EXIT-PROCESS-TRD-FILE.
           PERFORM CLOSE-TRD1-FILE.


      *******************************************
       GET-TY-CHECK-SUMMARY SECTION.
      *******************************************

           PERFORM OPEN-TY1-FILE.

           MOVE BR-NO               TO TY-BRNO
                                       QTY1-WBEG-BRNO
                                       QTY1-WEND-BRNO.
           MOVE EOD-DATE            TO TY-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTY1-WBEG-DATE
                                       QTY1-WEND-DATE.

           MOVE 0                   TO TY-CLASS
                                       QTY1-WBEG-CLASS.
           MOVE ALL "9"             TO QTY1-WEND-CLASS.

           PERFORM START-TY1-FILE.

       GET-TY-CHECK-SUMMARY-NEXT.
           PERFORM READ-TY1-FILE-NEXT.
           IF (IO-FG = 9) OR (TY-BRNO NOT = TY-PATH-OWNBR)   
              GO TO GET-TY-CHECK-SUMMARY-EXIT.

           IF EOD-DATE NOT = TY-DATE
              GO TO GET-TY-CHECK-SUMMARY-EXIT.

           ADD TY-CKWRITTEN(1) TO CKWRITTEN(1).
           ADD TY-CKWRITTEN(2) TO CKWRITTEN(2).
           ADD TY-CKWRITTEN(3) TO CKWRITTEN(3).
           ADD TY-CKWRITTEN(4) TO CKWRITTEN(4).
           ADD TY-CKWRITTEN(5) TO CKWRITTEN(5).
           ADD TY-CKWRITTEN(6) TO CKWRITTEN(6).
           ADD TY-CKWRITTEN(7) TO CKWRITTEN(7).
           ADD TY-CKWRITTEN(8) TO CKWRITTEN(8).
           GO TO GET-TY-CHECK-SUMMARY-NEXT.

       GET-TY-CHECK-SUMMARY-EXIT.
           PERFORM CLOSE-TY1-FILE.

      ****************************
       PRINT-GL-DETAIL SECTION.
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
           MOVE ED-UPDATED TO D-G-POST.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-GL-TRANS.
           IF ED-AMT(2) = 0
              GO TO PRINT-GL-SFREG.
           MOVE "   TRANSACTION JOURNAL" TO D-G-DESCRIPTION.
           IF ED-AMT(2) < 0
              MOVE ED-AMT(2) TO D-G-CR
              ADD ED-AMT(2) TO TOT-CR
           ELSE
              MOVE ED-AMT(2) TO D-G-DR
              ADD ED-AMT(2) TO TOT-DR.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
       PRINT-GL-SFREG.
           IF ED-AMT(3) = 0
              GO TO PRINT-GL-DLREG.
           MOVE "   SALES FINANCE REGISTER" TO D-G-DESCRIPTION.
           IF ED-AMT(3) < 0
              MOVE ED-AMT(3) TO D-G-CR
              ADD ED-AMT(3) TO TOT-CR
           ELSE
              MOVE ED-AMT(3) TO D-G-DR
              ADD ED-AMT(3) TO TOT-DR.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-GL-DLREG.
           IF ED-AMT(4) = 0
              GO TO PRINT-GL-TFJRNL.
           MOVE "   DIRECT LOAN REGISTER" TO D-G-DESCRIPTION.
           IF ED-AMT(4) < 0
              MOVE ED-AMT(4) TO D-G-CR
              ADD ED-AMT(4) TO TOT-CR
           ELSE
              MOVE ED-AMT(4) TO D-G-DR
              ADD ED-AMT(4) TO TOT-DR.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-GL-TFJRNL.
           IF ED-AMT(5) = 0
              GO TO PRINT-GL-CHKS.
           MOVE "   SERV/PUR TO PUR/SERV TRANSFER JOURNAL"
                                            TO D-G-DESCRIPTION.
           IF ED-AMT(5) < 0
              MOVE ED-AMT(5) TO D-G-CR
              ADD ED-AMT(5) TO TOT-CR
           ELSE
              MOVE ED-AMT(5) TO D-G-DR
              ADD ED-AMT(5) TO TOT-DR.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-GL-CHKS.
           IF ED-AMT(6) = 0
              GO TO PRINT-GL-DEP.
           MOVE "   CHECKS" TO D-G-DESCRIPTION.
           IF ED-AMT(6) < 0
              MOVE ED-AMT(6) TO D-G-CR
              ADD ED-AMT(6) TO TOT-CR
           ELSE
              MOVE ED-AMT(6) TO D-G-DR
              ADD ED-AMT(6) TO TOT-DR.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-GL-DEP.
           IF ED-AMT(7) = 0
              GO TO PRINT-GL-BRANCH-TRANSFER.
           MOVE "   DEPOSIT" TO D-G-DESCRIPTION.
           IF ED-AMT(7) < 0
              MOVE ED-AMT(7) TO D-G-CR
              ADD ED-AMT(7) TO TOT-CR
           ELSE
              MOVE ED-AMT(7) TO D-G-DR
              ADD ED-AMT(7) TO TOT-DR.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-GL-BRANCH-TRANSFER.
           IF ED-AMT(8) = 0
              GO TO PRINT-GL-CLASS-TRANSFER.
           MOVE " BRANCH (BULK) TRANSFER JOURNAL"
                                       TO D-G-DESCRIPTION.
           IF ED-AMT(8) < 0
              MOVE ED-AMT(8) TO D-G-CR
              ADD ED-AMT(8) TO TOT-CR
           ELSE
              MOVE ED-AMT(8) TO D-G-DR
              ADD ED-AMT(8) TO TOT-DR.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-GL-CLASS-TRANSFER.
           IF ED-AMT(9) = 0
              GO TO PRINT-GL-ACCT-CONV.
           MOVE " BRANCH (CLASS) TRANSFER JOURNAL"
                                       TO D-G-DESCRIPTION.
           IF ED-AMT(9) < 0
              MOVE ED-AMT(9) TO D-G-CR
              ADD ED-AMT(9) TO TOT-CR
           ELSE
              MOVE ED-AMT(9) TO D-G-DR
              ADD ED-AMT(9) TO TOT-DR.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-GL-ACCT-CONV.
           IF ED-AMT(10) = 0
              GO TO EXIT-PRINT-GL-DETAIL.
           MOVE " ACCOUNT CONVERSION JOURNAL" TO D-G-DESCRIPTION.
           IF ED-AMT(10) < 0
              MOVE ED-AMT(10) TO D-G-CR
              ADD ED-AMT(10) TO TOT-CR
           ELSE
              MOVE ED-AMT(10) TO D-G-DR
              ADD ED-AMT(10) TO TOT-DR.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       EXIT-PRINT-GL-DETAIL.
           EXIT.

      ****************************
       PRINT-GL-TOTALS SECTION.
      ****************************

           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "RECAP TOTALS :" TO DETAIL-LINE.
           MOVE TOT-DR TO D-G-DR.
           MOVE TOT-CR TO D-G-CR.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *    MOVE "GRAND TOTALS :" TO HEAD10X.
      *    MOVE HEAD10 TO DETAIL-LINE.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE " " TO HEAD10X.
           MOVE HEAD10 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM WRITE-DETAIL-LINE.

           GO TO PRINT-GL-TOTS-EXIT.

       GL-TOTS-WR-DUMMY.
           MOVE HEAD10 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 2 TO LN-FEED.
       PRINT-GL-TOTS-EXIT.
           EXIT.

      ***********************************
       UPDATE-WORK-FILE SECTION.
      ***********************************

           IF GLAMT = 0
              GO TO EXIT-UPDATE-WORK-FILE.

           IF GLNO = 0
              MOVE SPACES TO MESS.

           MOVE " " TO ED-KEY.
           MOVE "G" TO ED-ID.
           MOVE GLNO TO ED-NO.
           PERFORM READ-ED1-FILE.
           IF IO-FG = 9
              GO TO CREATE-WK.

      * TR-CODE IS SET IN LPEOD5TR COPY MEMBER
           IF TR-CODE = "P"
              ADD GLAMT TO ED-AMT(2)
           ELSE
              IF TR-CODE = "S"
                 ADD GLAMT TO ED-AMT(3)
              ELSE
              IF TR-CODE = "L"
                 ADD GLAMT TO ED-AMT(4)
              ELSE
              IF TR-CODE = "X"
                 ADD GLAMT TO ED-AMT(5)
              ELSE
              IF TR-CODE = "C"
                 PERFORM TR-CODE-C     
              ELSE
              IF TR-CODE = "D"
                 ADD GLAMT TO ED-AMT(7)
              ELSE
              IF TR-CODE = "B"
                 ADD GLAMT TO ED-AMT(8)
              ELSE
              IF TR-CODE = "J"
                 ADD GLAMT TO ED-AMT(9)
              ELSE
              IF TR-CODE = "K" AND
                 GP-LOAN-CONVERSION-UPD-FG = "Y" OR "G"
                 ADD GLAMT TO ED-AMT(10).

           PERFORM REWRITE-ED1-FILE.
           GO TO EXIT-UPDATE-WORK-FILE.
       CREATE-WK.
           MOVE " " TO ED-REC.
           MOVE 0 TO ED-AMT(2) ED-AMT(3) ED-AMT(4)
                     ED-AMT(5) ED-AMT(6) ED-AMT(7)
                     ED-AMT(8) ED-AMT(9) ED-AMT(10).
           MOVE "G" TO ED-ID.
           MOVE GLNO TO ED-NO.

           IF TR-CODE = "P"
              ADD GLAMT TO ED-AMT(2)
           ELSE
           IF TR-CODE = "S"
              ADD GLAMT TO ED-AMT(3)
           ELSE
              IF TR-CODE = "L"
                 ADD GLAMT TO ED-AMT(4)
              ELSE
              IF TR-CODE = "X"
                 ADD GLAMT TO ED-AMT(5)
              ELSE
              IF TR-CODE = "C"
                 PERFORM TR-CODE-C         
              ELSE
              IF TR-CODE = "D"
                 ADD GLAMT TO ED-AMT(7)
              ELSE
              IF TR-CODE = "B"
                 ADD GLAMT TO ED-AMT(8)
              ELSE
              IF TR-CODE = "J"
                 ADD GLAMT TO ED-AMT(9)
              ELSE
              IF TR-CODE = "K" AND
                 GP-LOAN-CONVERSION-UPD-FG = "Y" OR "G"
                 ADD GLAMT TO ED-AMT(10).

           PERFORM WRITE-ED1-FILE.
       EXIT-UPDATE-WORK-FILE.
           MOVE 0 TO GLAMT GLNO.

      **************************
       UPDATE-BYFILE SECTION.
      **************************

           MOVE " " TO HEAD4.
           MOVE "  - - - DAILY TOTALS RECAP JOURNAL - - -" TO H-TITLE.
           PERFORM PROGRESS-MSG.
           PERFORM OPEN-TY1-FILE.

           MOVE BR-NO               TO TY-BRNO
                                       QTY1-WBEG-BRNO
                                       QTY1-WEND-BRNO.
           MOVE EOD-DATE            TO TY-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTY1-WBEG-DATE
                                       QTY1-WEND-DATE.

           MOVE 0                   TO TY-CLASS
                                       QTY1-WBEG-CLASS.
           MOVE ALL "9"             TO QTY1-WEND-CLASS.

           PERFORM START-TY1-FILE.
       NEXT-TY-TO-BY.
           PERFORM READ-TY1-FILE-NEXT.
           IF IO-FG = 9 OR (TY-BRNO NOT = TY-PATH-OWNBR)
              GO TO UPDATE-BYFILE-EXIT.

           IF EOD-DATE NOT = TY-DATE
              GO TO UPDATE-BYFILE-EXIT.

       UPDATE-BY-TEST-TYPRINT.
           IF TY-CLASS NOT = 0 OR TY-CKWRITTEN(6) NOT = 0
      * DEFAULT IS "Y" OR " ", SO LEAVE THIS TEST ALONE
              IF DAILY-TOTALS NOT = "N"
                 PERFORM TY-TODAYS-TOTAL-PRINT.
           GO TO NEXT-TY-TO-BY.

       UPDATE-BYFILE-EXIT.
           PERFORM CLOSE-TY1-FILE.

      *****************************************
       TY-TODAYS-TOTAL-PRINT SECTION.
      *****************************************

           MOVE 99 TO LN-COUNT.

           MOVE TY-CLASS TO H-LC-CODE LC-CODE.
           PERFORM READ-LC1-FILE.
           IF IO-FG = 9
              MOVE "* INVALID *" TO LC-DESC.
           MOVE LC-DESC TO H-LC-DESC.
           MOVE HEAD11 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 3 TO LN-FEED.

           MOVE 0 TO TOT-NO1 TOT-AMT1 TOT-NO2 TOT-AMT2.
           MOVE HEAD12 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE HEAD6 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.

           MOVE 1 TO PL-TYPE.
           MOVE 1 TO PL-ACTY.
           PERFORM ACCUM-PL-TOTALS.
           MOVE TOTAL-NO-PL TO D-P-NO1.
           MOVE TOTAL-AMT-PL TO D-P-AMT1.
      *     MOVE TY-NO(1 10) TO D-P-NO1.
      *     MOVE TY-AMT(1 10) TO D-P-AMT1.

           MOVE 1 TO PL-TYPE.
           MOVE 2 TO PL-ACTY.
           PERFORM ACCUM-PL-TOTALS.
           MOVE TOTAL-NO-PL TO D-P-NO2.
           MOVE TOTAL-AMT-PL TO D-P-AMT2.
      *     MOVE TY-NO(2 10) TO D-P-NO2.
      *     MOVE TY-AMT(2 10) TO D-P-AMT2.

           MOVE 0 TO TOTAL-PLA-RECV.
           PERFORM VARYING SUB4 FROM 1 BY 1 UNTIL SUB4 > 5
             ADD TY-PL-PRINCOL(SUB4) TO TOTAL-PLA-RECV
             ADD TY-PL-INTCOL(SUB4) TO TOTAL-PLA-RECV
             ADD TY-PL-LATECOL(SUB4) TO TOTAL-PLA-RECV
      *      ADD TY-PL-O2COL(SUB4) TO TOTAL-PLA-RECV
           END-PERFORM.
           MOVE TOTAL-PLA-RECV TO D-P-AMT3.
      *     MOVE TY-PLA-RECV TO D-P-AMT3.
           MOVE "NB :" TO D-N-PROMPT1.
           MOVE TY-NO(1 1) TO D-N-NO1.
           MOVE TY-AMT(1 1) TO D-N-AMT1.
           MOVE TY-NO(2 1) TO D-N-NO2.
           MOVE TY-AMT(2 1) TO D-N-AMT2.
           PERFORM WRITE-DETAIL-LINE.
           ADD TY-NO(1 1) TO TOT-NO1.
           ADD TY-AMT(1 1) TO TOT-AMT1.
           ADD TY-NO(2 1) TO TOT-NO2.
           ADD TY-AMT(2 1) TO TOT-AMT2.

           IF GP-THE-ONLY-SF-SRCD NOT = " "
              MOVE GP-THE-ONLY-SF-SRCD TO HOLD-GP-PROMPT
              MOVE HOLD-GP-PROMPTX TO D-N-PROMPT1
           ELSE
              MOVE "RF :" TO D-N-PROMPT1.

           MOVE TY-NO(1 2) TO D-N-NO1.
           MOVE TY-AMT(1 2) TO D-N-AMT1.
           MOVE TY-NO(2 2) TO D-N-NO2.
           MOVE TY-AMT(2 2) TO D-N-AMT2.
           PERFORM WRITE-DETAIL-LINE.
           ADD TY-NO(1 2) TO TOT-NO1.
           ADD TY-AMT(1 2) TO TOT-AMT1.
           ADD TY-NO(2 2) TO TOT-NO2.
           ADD TY-AMT(2 2) TO TOT-AMT2.

           MOVE "FB :" TO D-N-PROMPT1.
           MOVE TY-NO(1 3) TO D-N-NO1.
           MOVE TY-AMT(1 3) TO D-N-AMT1.
           MOVE TY-NO(2 3) TO D-N-NO2.
           MOVE TY-AMT(2 3) TO D-N-AMT2.
           PERFORM WRITE-DETAIL-LINE.

           ADD TY-NO(1 3) TO TOT-NO1.
           ADD TY-AMT(1 3) TO TOT-AMT1.
           ADD TY-NO(2 3) TO TOT-NO2.
           ADD TY-AMT(2 3) TO TOT-AMT2.

           MOVE "PB :" TO D-N-PROMPT1.
           MOVE TY-NO(1 4) TO D-N-NO1.
           MOVE TY-AMT(1 4) TO D-N-AMT1.
           MOVE TY-NO(2 4) TO D-N-NO2.
           MOVE TY-AMT(2 4) TO D-N-AMT2.
           PERFORM WRITE-DETAIL-LINE.
           ADD TY-NO(1 4) TO TOT-NO1.
           ADD TY-AMT(1 4) TO TOT-AMT1.
           ADD TY-NO(2 4) TO TOT-NO2.
           ADD TY-AMT(2 4) TO TOT-AMT2.

           MOVE HEAD7 TO DETAIL-LINE.
           MOVE "PBD:" TO D-N-PROMPT1.
           MOVE TY-NO(1 5) TO D-N-NO1.
           MOVE TY-AMT(1 5) TO D-N-AMT1.
           MOVE TY-NO(2 5) TO D-N-NO2.
           MOVE TY-AMT(2 5) TO D-N-AMT2.
           PERFORM WRITE-DETAIL-LINE.
           ADD TY-NO(1 5) TO TOT-NO1.
           ADD TY-AMT(1 5) TO TOT-AMT1.
           ADD TY-NO(2 5) TO TOT-NO2.
           ADD TY-AMT(2 5) TO TOT-AMT2.

           MOVE HEAD6X TO DETAIL-LINE.
           MOVE "PBI:" TO D-N-PROMPT1.
           MOVE TY-NO(1 6) TO D-N-NO1.
           MOVE TY-AMT(1 6) TO D-N-AMT1.
           MOVE TY-NO(2 6) TO D-N-NO2.
           MOVE TY-AMT(2 6) TO D-N-AMT2.
           PERFORM WRITE-DETAIL-LINE.
           ADD TY-NO(1 6) TO TOT-NO1.
           ADD TY-AMT(1 6) TO TOT-AMT1.
           ADD TY-NO(2 6) TO TOT-NO2.
           ADD TY-AMT(2 6) TO TOT-AMT2.

           MOVE "RN :" TO D-N-PROMPT1.
           MOVE TY-NO(1 7) TO D-N-NO1.
           MOVE TY-AMT(1 7) TO D-N-AMT1.
           MOVE TY-NO(2 7) TO D-N-NO2.
           MOVE TY-AMT(2 7) TO D-N-AMT2.
           PERFORM WRITE-DETAIL-LINE.
           ADD TY-NO(1 7) TO TOT-NO1.
           ADD TY-AMT(1 7) TO TOT-AMT1.
           ADD TY-NO(2 7) TO TOT-NO2.
           ADD TY-AMT(2 7) TO TOT-AMT2.

           MOVE 2 TO PL-TYPE.
           MOVE 1 TO PL-ACTY.
           PERFORM ACCUM-PL-TOTALS.
           MOVE TOTAL-NO-PL TO D-P-NO1.
           MOVE TOTAL-AMT-PL TO D-P-AMT1.
      *     MOVE TY-NO(1 11) TO D-P-NO1.
      *     MOVE TY-AMT(1 11) TO D-P-AMT1.
           MOVE 2 TO PL-TYPE PL-ACTY.
           PERFORM ACCUM-PL-TOTALS.
           MOVE TOTAL-NO-PL TO D-P-NO2.
           MOVE TOTAL-AMT-PL TO D-P-AMT2.
      *     MOVE TY-NO(2 11) TO D-P-NO2.
      *     MOVE TY-AMT(2 11) TO D-P-AMT2.
      *     MOVE TY-PLI-RECV TO D-P-AMT3.
           MOVE "SC :" TO D-N-PROMPT1.
           MOVE TY-NO(1 8) TO D-N-NO1.
           MOVE TY-AMT(1 8) TO D-N-AMT1.
           MOVE TY-NO(2 8) TO D-N-NO2.
           MOVE TY-AMT(2 8) TO D-N-AMT2.
           PERFORM WRITE-DETAIL-LINE.
           ADD TY-NO(1 8) TO TOT-NO1.
           ADD TY-AMT(1 8) TO TOT-AMT1.
           ADD TY-NO(2 8) TO TOT-NO2.
           ADD TY-AMT(2 8) TO TOT-AMT2.

           MOVE "OT :" TO D-N-PROMPT1.
           MOVE TY-NO(1 9) TO D-N-NO1.
           MOVE TY-AMT(1 9) TO D-N-AMT1.
           MOVE TY-NO(2 9) TO D-N-NO2.
           MOVE TY-AMT(2 9) TO D-N-AMT2.
           PERFORM WRITE-DETAIL-LINE.
           ADD TY-NO(1 9) TO TOT-NO1.
           ADD TY-AMT(1 9) TO TOT-AMT1.
           ADD TY-NO(2 9) TO TOT-NO2.
           ADD TY-AMT(2 9) TO TOT-AMT2.

           MOVE "RB :" TO D-N-PROMPT1.
           MOVE TY-NO(1 10) TO D-N-NO1.
           MOVE TY-AMT(1 10) TO D-N-AMT1.
           MOVE TY-NO(2 10) TO D-N-NO2.
           MOVE TY-AMT(2 10) TO D-N-AMT2.
           PERFORM WRITE-DETAIL-LINE.
           ADD TY-NO(1 10) TO TOT-NO1.
           ADD TY-AMT(1 10) TO TOT-AMT1.
           ADD TY-NO(2 10) TO TOT-NO2.
           ADD TY-AMT(2 10) TO TOT-AMT2.

           MOVE "RO :" TO D-N-PROMPT1.
           MOVE TY-NO(1 11) TO D-N-NO1.
           MOVE TY-AMT(1 11) TO D-N-AMT1.
           MOVE TY-NO(2 11) TO D-N-NO2.
           MOVE TY-AMT(2 11) TO D-N-AMT2.
           PERFORM WRITE-DETAIL-LINE.
           ADD TY-NO(1 11) TO TOT-NO1.
           ADD TY-AMT(1 11) TO TOT-AMT1.
           ADD TY-NO(2 11) TO TOT-NO2.
           ADD TY-AMT(2 11) TO TOT-AMT2.

           MOVE HEAD8 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOT:" TO D-N-PROMPT1.
           MOVE TOT-NO1 TO D-N-NO1.
           MOVE TOT-AMT1 TO D-N-AMT1.
           MOVE TOT-NO2 TO D-N-NO2.
           MOVE TOT-AMT2 TO D-N-AMT2.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 3 TO LN-FEED.
           MOVE HEAD9 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.
           MOVE "LNCK:" TO D-C-PROMPT.
           MOVE TY-CKWRITTEN(1) TO D-C-AMT1.
           MOVE "OTH :" TO D-N-PROMPT2.
           MOVE TY-OTHCOL TO D-N-AMT1.
           MOVE "   CL1:" TO D-N-PROMPT3.
           MOVE TY-CLREB1 TO D-N-AMT2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DLCK:" TO D-C-PROMPT.
           MOVE TY-CKWRITTEN(2) TO D-C-AMT1.
           MOVE "INT :" TO D-N-PROMPT2.
           MOVE TY-INTCOL TO D-N-AMT1.
           MOVE "   CL2:" TO D-N-PROMPT3.
           MOVE TY-CLREB2 TO D-N-AMT2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MISC:" TO D-C-PROMPT.
           MOVE TY-CKWRITTEN(3) TO D-C-AMT1.
           MOVE "LC  :" TO D-N-PROMPT2.
           MOVE TY-LATECOL TO D-N-AMT1.
           MOVE "   CL3:" TO D-N-PROMPT3.
           MOVE TY-CLREB3 TO D-N-AMT2.
           PERFORM WRITE-DETAIL-LINE.

           IF TY-CKWRITTEN(4) = 0
              MOVE "NSF :" TO D-C-PROMPT
              MOVE TY-CKWRITTEN(5) TO D-C-AMT1
           ELSE
              MOVE "OVPY:" TO D-C-PROMPT
              MOVE TY-CKWRITTEN(4) TO D-C-AMT1.
           MOVE "PRIN:" TO D-N-PROMPT2.
           MOVE TY-PRINCOL TO D-N-AMT1.
           MOVE "   CL4:" TO D-N-PROMPT3.
           MOVE TY-CLREB4 TO D-N-AMT2.
           PERFORM WRITE-DETAIL-LINE.

           IF TY-CKWRITTEN(4) = 0
              MOVE "WDRL:" TO D-C-PROMPT
              MOVE TY-CKWRITTEN(6) TO D-C-AMT1
           ELSE
              MOVE "NSF :" TO D-C-PROMPT
              MOVE TY-CKWRITTEN(5) TO D-C-AMT1.
           MOVE "DEF :" TO D-N-PROMPT2.
           MOVE TY-DEFCOL TO D-N-AMT1.
           MOVE "   AH :" TO D-N-PROMPT3.
           MOVE TY-AHREB TO D-N-AMT2.
           PERFORM WRITE-DETAIL-LINE.

           IF TY-CKWRITTEN(4) = 0
              MOVE "DEP :" TO D-C-PROMPT
              MOVE TY-CKWRITTEN(7) TO D-C-AMT1
           ELSE
              MOVE "WDRL:" TO D-C-PROMPT
              MOVE TY-CKWRITTEN(6) TO D-C-AMT1.
           MOVE "   PP :" TO D-N-PROMPT3.
           MOVE TY-PPREB TO D-N-AMT2.
           PERFORM WRITE-DETAIL-LINE.

           IF TY-CKWRITTEN(4) NOT = 0
              MOVE "DEP :" TO D-C-PROMPT
              MOVE TY-CKWRITTEN(7) TO D-C-AMT1.
           MOVE "   OT1:" TO D-N-PROMPT3.
           MOVE TY-O1REB TO D-N-AMT2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "   OT2:" TO D-N-PROMPT3.
           MOVE TY-O2REB TO D-N-AMT2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "   OT3:" TO D-N-PROMPT3.
           MOVE TY-O3REB TO D-N-AMT2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "   INT:" TO D-N-PROMPT3.
           MOVE TY-INTREB TO D-N-AMT2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE HEAD9A TO DETAIL-LINE.
           MOVE "   SRV:" TO D-N-PROMPT3.
           MOVE TY-SCREB TO D-N-AMT2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "   FEE:" TO D-N-PROMPT3.
           MOVE TY-MFREB TO D-N-AMT2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ALLO:" TO D-N-PROMPT2.
           MOVE TY-ALLOTMENTS TO D-N-AMT1.
           MOVE "   DEF:" TO D-N-PROMPT3.
           MOVE TY-DFREB TO D-N-AMT2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "RENW:" TO D-N-PROMPT2.
           MOVE TY-RNPRIN TO D-N-AMT1.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "LBOX:" TO D-N-PROMPT2.
           MOVE TY-LOCKBOX TO D-N-AMT1.
           PERFORM WRITE-DETAIL-LINE.
      *---PRINT REPOSSESSIONS----------------------------------


           MOVE HEAD15 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           PERFORM PRINT-REPO-LINES VARYING SUBR FROM 1 BY 1
                                            UNTIL SUBR > 5.
           GO TO PRINT-REPO-TRAILER.
       PRINT-REPO-LINES.
           IF SUBR = 1
                   MOVE "ON HAND    :" TO D-REPO-DESC
           ELSE
           IF SUBR = 2
                   MOVE "REDEEMED   :" TO D-REPO-DESC
           ELSE
           IF SUBR = 3
                   MOVE "SOLD       :" TO D-REPO-DESC
           ELSE
           IF SUBR = 4
                   MOVE "ON HAND P&L:" TO D-REPO-DESC
           ELSE
                   MOVE "SOLD P&L   :" TO D-REPO-DESC.

           MOVE TY-NO-REPO  (1 SUBR) TO D-REPO-NO.
           MOVE TY-AMT-REPO (1 SUBR) TO D-REPO-AMT.
           MOVE TY-NO-REPO  (2 SUBR) TO D-REPO-NO2.
           MOVE TY-AMT-REPO (2 SUBR) TO D-REPO-AMT2.

           PERFORM WRITE-DETAIL-LINE.

       PRINT-REPO-TRAILER.

           MOVE HEAD16 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *****************************************
       WRITE-DETAIL-LINE SECTION.
      *****************************************

           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE " " TO DETAIL-LINE.

      *****************************************
       HEAD-PAGE SECTION.
      *****************************************

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

           MOVE HEAD5 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 6 TO LN-COUNT.
           MOVE 2 TO LN-FEED.


      *****************************************************************
      *  ACCUM P&L TOTALS SECTION:
      *
      *  INPUT :  PL-TYPE
      *           PL-PERIOD   (BY ONLY, NOT NEEDED FOR TY).
      *           PL-ACTY
      *
      *  OUTOUT : TOTAL-NO-PL
      *           TOTAL-AMT-PL
      *           TOTAL-O2-PL
      *           TOTAL-INT-PL
      *
      *  PL-TYPE = 1 - TY ACTIVE P&L TOTALS.
      *            2 - TY INACTIVE P&L TOTALS.
      *            3 - BY ACTIVE P&L TOTALS.
      *            4 - BY INACTIVE P&L TOTALS.
      *            5 - TY AND BY ACTIVE P&L TOTALS.
      *            6 - TY AND BY INACTIVE P&L TOTALS.
      *
      *            --------------------------------------
      *  PL-PERIOD 1 - CURRENT         |  1 - MTD
      *            2 - END LAST MONTH  |  2 - YTD
      *            3 - END-LAST YEAR   |  3 - LYR
      *            --------------------------------------
      *  PL-ACTY   1 - NET GAIN/LOSS   |  2 - NEW VOLUME
      *            --------------------------------------
      *
      *  PL-SUB3   1 - P1 ACTIVE          2 - P1 INACTIVE
      *            3 - P2 ACTIVE          4 - P2 INACTIVE
      *            5 - P3 ACTIVE          6 - P3 INACTIVE
      *            7 - P4 ACTIVE          8 - P4 INACTIVE
      *            9 - P5 ACTIVE         10 - P5 INACTIVE
      *
      *****************************************************************
       ACCUM-PL-TOTALS SECTION.
           MOVE 0 TO TOTAL-NO-PL
                     TOTAL-AMT-PL
                     TOTAL-O2-PL
                     TOTAL-INT-PL.

           IF PL-TYPE = 1 OR 5
              PERFORM ACCUM-PL-TY
                    VARYING PL-SUB3 FROM 1 BY 2 UNTIL PL-SUB3 > 9.

           IF PL-TYPE = 2 OR 6
              PERFORM ACCUM-PL-TY
                    VARYING PL-SUB3 FROM 2 BY 2 UNTIL PL-SUB3 > 10.

           GO TO EXIT-ACCUM-PL-TOTALS.

       ACCUM-PL-TY.
           ADD TY-NO-PL(PL-ACTY PL-SUB3) TO TOTAL-NO-PL.
           ADD TY-AMT-PL(PL-ACTY PL-SUB3) TO TOTAL-AMT-PL.
           ADD TY-O2-PL(PL-ACTY PL-SUB3) TO TOTAL-O2-PL.
           ADD TY-INT-PL(PL-ACTY PL-SUB3) TO TOTAL-INT-PL.

       EXIT-ACCUM-PL-TOTALS.
           EXIT.

      ******************************************************************
       PRINT-TRW-RECORDS SECTION.
      ******************************************************************

           PERFORM OPEN-TRW1-FILE.

      * LIST EVERT ATTEMPT TO CLOSE THE CASH DRAWER IN WHICH THE
      * OVER/SHORT AMOUNT DID NOT = ZERO.

           MOVE CA-BRNO   TO TW-BRNO.
           MOVE CA-DATE   TO TW-DATE.
           MOVE CA-TILLNO TO TW-TILLNO.
           PERFORM READ-TRW1-FILE.
           IF IO-BAD
              GO TO PRINT-TRW-RECORDS-EXIT.

           PERFORM VARYING TW-SUB FROM 1 BY 1
                   UNTIL (TW-SUB > 20 OR TW-TIME(TW-SUB) = 0)
              STRING "CLOSING ATTEMP # " TW-SUB
                     DELIMITED BY SIZE INTO D-CA-DESC
              MOVE TW-TIME(TW-SUB)      TO H-AND-M
              PERFORM CONVERT-TIME
              MOVE TIME-EDIT            TO D-CLOSED
              MOVE TW-POSTNAME(TW-SUB)  TO D-BALBY
              MOVE TW-AMOUNT(TW-SUB)    TO D-CASHBAL(6)
              MOVE TW-OVERSHORT(TW-SUB) TO D-CASHBAL(5)
              PERFORM WRITE-DETAIL-LINE
           END-PERFORM.

       PRINT-TRW-RECORDS-EXIT.
           EXIT.

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/BRSECURE.CPY".
 
      *****************************************************
       FORM-RESET SECTION.
      *****************************************************
           MOVE EXT-CONAME-CONAME TO RECAPJ-CONAME.
           MOVE EXT-CONAME-SYSDATE TO RECAPJ-SYSDATE.
           DISPLAY RECAPJ-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE RECAPJ-FORM-FIELDS TO WR-BUF.
           MOVE RECAPJ-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.


      *****************************************************
       SETUP-LINK-INFO SECTION.
      *****************************************************
           MOVE RECAPJ-HELP--FILE TO HELP-LINK-FILE.
           MOVE RECAPJ-HELP--DIR TO HELP-LINK-DIR.

      *****************************************************
       VDU-CONVERT-FIELD SECTION.
      *****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/RECAPJ_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       OPEN-TR2-CURSOR SECTION.
      ******************************************************************

           MOVE EOD-DATE      TO TP-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO PASS-WBEG-DATE.

           IF TR2-CONN-OPEN-SW = "N" 
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS TR2_CONNECT
                    USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               MOVE "Y" TO TR2-CONN-OPEN-SW
               EXEC SQL SET CONNECTION C1 END-EXEC
               PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION TR2_CONNECT END-EXEC.

           EXEC SQL
            DECLARE TR2_CURSOR CURSOR FOR
                :BR1-RETURN-CODE = 
                 EXEC SSP_RECAPJ_TRFILE_SELECT
                  ( :PASS-BRANCHLIST, :PASS-WBEG-DATE)
                END-EXEC.

           EXEC SQL
               OPEN TR2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE STAT---GOOD TO TR2-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

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
      * NOTE: CHECK-UPDATE-BR & TD-UPD-BR ARE DUMMY PARAGRAPHS
      *       BECAUSE COPY MEMBER LPEOD5TR PERFORMS THEM WHEN
      *       UPDATING.
       CHECK-UPDATE-BR.
           MOVE SPACES TO MESS.
       TD-UPD-BR.
           MOVE SPACES TO MESS.
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       TR-CODE-C.
           IF (TC-TYPE = "BK" OR "ID" OR "IW")
              ADD GLAMT TO ED-AMT(7)
           ELSE
              ADD GLAMT TO ED-AMT(6).
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
      * USED IN COPY MEMBER LPEOD5TR
      *    MOVE ER-PATH TO ACCESS-BUF.
      *    PERFORM ACCESS-CALL.
      *    IF STAT NOT = "00"
      *       OPEN OUTPUT ER-FILE
      *       CLOSE ER-FILE.
      *    MOVE E-FILE TO E-FILEX.
      *    MOVE FILE-STAT TO E-CODE.
      *    MOVE ERROR-MSG TO MESS.
      *    PERFORM OPEN-ER-FILE.
      *    MOVE MESS TO ER-REC.
      *    MOVE "PROGRAM =" TO ER-PROGRAM.
      *    MOVE "LPEOD5" TO ER-NAME.
      *    MOVE EXT-CONAME-SYSDATE TO ER-DATE.
      *    PERFORM WRITE-ER-REC.
      *    PERFORM CLOSE-ER-FILE.
       SAVE-ERROR-MSG.
           EXIT.
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
      *----------------------------------------------------------------

       COPY "LIBLP/LPEOD5TR.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/ARRAYBR.CPY".
       COPY "LIBLP/ARRAYLC.CPY".
       COPY "LIBGL/ARRAYGI.CPY".

      ************************************
       IO-ROUTINES SECTION.
      ************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTYGS_SQL.CPY".
       COPY "LIBLP/LPTRXGS_SQL.CPY".
       COPY "LIBLP/LPTRWGS_SQL.CPY".
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
       COPY "LIBLP/LPCAGS_SQL.CPY".
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGL/GLGIGS_SQL.CPY".
       COPY "LIBGB/GBRCGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPPRINT.CPY".
       COPY "LIBGB/GRBRLS_SQL.CPY".

       COPY "LIBGL/GLGI1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPTY1RN.CPY".
       COPY "LIBLP/LPED1IN.CPY".
       COPY "LIBLP/LPTRW1RN.CPY".
       COPY "LIBGL/GLGL1RN.CPY".
       COPY "LIBLP/LPCA1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBGB/GBRC1RN.CPY".

       COPY "LIBGB/GBBRRN.CPY".
      *-----------------------------------------------------------------
       READ-TR2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BR-PATH-SQL TO E-FILE.
           MOVE BR-NO TO E-KEYX.
           INITIALIZE QBR-REC.

           EXEC SQL SET CONNECTION TR2_CONNECT END-EXEC.

           IF ( TR2-CURSOR-STAT-GOOD )
              EXEC SQL
                  FETCH TR2_CURSOR INTO 
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
              GO TO READ-TR2-FILE-NEXT.

           IF ( IO-FG = 0 )
              MOVE QBR-NO TO BR-NO.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TR2-CURSOR.

           IF ( TR2-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION TR2_CONNECT END-EXEC
              EXEC SQL
                   CLOSE TR2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TR2-CURSOR-STAT
              EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TR2-CONNECTION.
           IF TR2-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION TR2_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT TR2_CONNECT END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE "N" TO TR2-CONN-OPEN-SW.
      *-----------------------------------------------------------------


      * COPY "LIBLP/LPTRB1RN.CPY".
      * COPYMEMBER: LIBLP/LPTRB1RN
       LOAD-TRB1-FILE.
      * - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
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
       READ-TRB1-FILE-NEXT.  *> EMBEDDED - TRB
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
              MOVE TRC-ALL-PATH             TO TRC-PATH.

      *-----------------------------------------------------------------
       OPEN-TRC1-FILE.
           PERFORM LOAD-TRC1-FILE.
           PERFORM OPEN-IT.
           MOVE TRC-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRC1-FILE.   *> SSP - TRC
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
       READ-TRC1-FILE-NEXT.   *> EMBEDDED - TRC
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
           MOVE TRD-PATH-SQL    TO E-FILE.
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
       READ-TRD1-FILE.   *> EMBEDDED - TRD
           PERFORM READ-IT.
           MOVE TRD-PATH-OWNBR  TO TD-BRNO. 
           MOVE TRD-PATH-SQL    TO E-FILE.
           MOVE TRD1-KEY        TO E-KEYX.
           INITIALIZE QTRD-REC.

           MOVE TD-BRNO   TO QTD-BRNO.
           MOVE TD-DATE   TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTD-DATE.

           EXEC SQL
            SELECT TRDFILE.TD_BRNO,
                   CAST(TRDFILE.TD_DATE AS VARCHAR(10)),
                   CAST(TRDFILE.TD_LTOUCH_DATE AS VARCHAR(10)),
                   TRDFILE.TD_DEPOSIT,
                   TRDFILE.TD_CHECK_DEPOSIT,
                   TRDFILE.TD_BANK,
                   TRDFILE.TD_CASH,
                   TRDFILE.TD_GLFG,
                   TRDFILE.TD_GLOVERSHORT,
                   TRDFILE.TD_OVERSHORT,
                   TRDFILE.TD_USERID,
                   CAST(TRDFILE.TD_POSTDATE AS VARCHAR(10)),
                   TRDFILE.TD_POSTTIME
            INTO :QTRD-REC
            FROM DBO.TRDFILE
            WHERE TRDFILE.TD_BRNO = :QTD-BRNO
              AND TRDFILE.TD_DATE = :QTD-DATE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRD1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-TRD-FIELDS.


      *-----------------------------------------------------------------
       READ-TRD1-FILE-NEXT.  *> EMBEDDED - TRD
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
       READ-TRJ1-FILE-NEXT.   *> EMBEDDED - TRJ
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
       READ-TRK1-FILE-NEXT.   *> EMBEDDED - TRK
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
       READ-TRL1-FILE-NEXT.   *> EMBEDDED - TRL
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
       READ-TRP1-FILE-NEXT.  *> EMBEDDED - TRP
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
           MOVE TRS-PATH-SQL    TO E-FILE.
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
       READ-TRS1-FILE-NEXT.   *> EMBEDDED - TRS
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
       READ-TRX1-FILE-NEXT.   *> EMBEDDED - TRX
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

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.


      **************************************
       END-PROGRAM SECTION.
      **************************************
       END-RUN.
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

           IF EXT-ROUTE-BUF  = "00"
              MOVE "LP/LPEDMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY RECAPJ-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
