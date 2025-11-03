       IDENTIFICATION DIVISION.
       PROGRAM-ID.    LPEOD5.
       DATE-WRITTEN.  042084
      ******************************************************************
      *   DIRECTORY  :  LP
      *   DESCRIPTION:  EOD G/L & BRANCH FILE UPDATE
      *                 CASH DRAWER SUMMARY REPORT
      *                 EOD G/L & BRANCH SUMMARY REPORT
      *   CALLED FROM LONPR0.C
      *
      *   R E M E M B E R:    CHANGE LP/LPEOD5.C & LP/RECAPJ.C
      *   R E M E M B E R:    IF YOU CHANGE SOMTHING IN THE UPDATE OF
      *                       TY TO BY, WOULD YOU ALSO UPDATE FX/FXBYTY?
      *                       THIS IS A HANDY FIX & THE UPDATE SECTION
      *                       IS IDENTICAL TO THIS ONE.  THANKS!!! BARB
      *   NOTE:
      *        IF THE GI-RECORD CHANGES CHANGE UNSUB AND ERSUB!!!!!
      *
      * REV:
      *  073095 JTG CORRECTED UPDATE OF BY-FILE RE: REPO
      *  081495 MJD ADDED LOGIC FOR GP-THE-ONLY-SF-SRCD.
      *  103195 BLL UNSTAR LOGIC SO WE CHECK TY-BRFG Y & DON'T
      *             RE-UPDATE
      *  111895 JTG CHANGED INDEX TO BY-UNITS FROM 18 TO 10
      *  012496 BLL CHANGED FOR RECORD CHANGES - PL, RECOVERED, ETC.
      *  012596 MJD CHANGED PRINT FOR RECORD CHANGES - PL, RECOVERED, ETC.
      *  012696 BLL BY DLRV TABLE CHANGED, PL-O2COL ADDED
      *  092396 KEC ADDED LOGIC TO REMOVE WORKFILE FOR ED-FILE.
      *  101096 JTG CHANGED TO HANDLE CLASS TRANSFERS
      *  111696 JTG CHANGED GL-DESC FOR TFR TO SERV & TFR TO PUR
      *             "   SERV/PUR TO PUR/SERV TRANSFER JOURNAL"
      *  111296 JTG CHANGED TO HANDLE ACCOUNT CONVERSION
      *  030697 KEC CHANGED TO HANDLE NEW APPLICATIONS STATISTICS IN
      *              BY-FILE AND TY-FILE.
      *  042597 BLV ADD BY-NO-TURNUSDOWN FOR APPLICATION STATS
      *  051297 KEC ADD BY-NO-MADE FOR APPLICATION STATS
      *  060697 MJD ADDED PRINT OF O3 REBATED.
      *  070297 MJD ADDED UPDATE OF BY-DLDISC(1-2)
      *  090297 GLF REMOVED ALL USE OF GLOBAL
      *  090897 GLF REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *  091097 BLV CHANGED LP01ED, EXPANDED AMT, PUT FIELDS IN COPY MEMBER
      *  122997 JTG EXPANED AMOUNTS TO 9(9) RE: BULK TRANSFER FULL BRANCH
      *  020698 BLV IF TD-GLFG IS Y, DON'T UPDATE GT AGAIN
      *  032098 MJD ADDED GETENV(W) FOR USE IN LIBLP/POSTDATE
      *  033098 BLV WORK ON ERROR LOGGING PROBLEMS; FIX MSG
      *             "EOD SUCCESSFUL" WHEN IT IS NOT
      *  040898 BAH ADDED REPO INFO THAT MJD ADDED TO RECAPJ 6/23/95, PL# 179
      *  051198 MJD ADDED DETAIL LINES FOR "RB" AND "RO".
      *  051998 MJD CHANGED BY-EXREB TO BE BY-PART-CHGOFF; REMOVED BY-PASTDUE;
      *             ADDED BANKRUPT DELINQUENCY
      *  BAH 062698 TILLNO IS NOW ALPHANUMERIC
      *  080698 BLV WORK ON ERROR ROUTINES; CHANGE ERROR-FG TO DIFFERENT
      *             CODE TO REFLECT ERRORS ON GL & GI, CHANGES TO LONPR0,
      *             DON'T UPDATE STATS IF INVALID GI
      *  990302 BLV PRINT OCCURRENCE 8 OF TY-CKWRITTEN (NOTES PAYABLE
      *             CHECKS & UPDATE BR-BANKBAL BY SUBTRACTING NOTES
      *             PAYABLE CHECK WRITTEN TOTAL FOR BRANCH
      *  990511 BLV DON'T READ COFILE FOR GL/GLMENU - ALWAYS UPDATE GL.
      *             TOO MANY NEW CUSTOMERS DON'T HAVE SECURITY SET UP &
      *             EVERYBODY WANTS TO UPDATE GL...IF WE REALLY NEED TO
      *             STOP CUSTOMER GL UPDATE - ADD A GP FLAG!  BARB & BETH
      *  BLV 990820 CHANGE TO PRINT BUCKETS 4 & 5 OF REPO TABLE, NOT
      *             PLA, PLI; TFC PR#20, GREATER SUBURBAN PR#809
      *  BLV 990920 DON'T ADD TY-O2COL TO TOTAL-PLA-RECV, IT IS ALREADY
      *             IN TY-PRIN-COL SO Z2 WAS DOUBLING RECOVERED AMT
      *  MJD 000313 ADDED PRINT OF FAILED ATTEMPTS TO BALANCE CASH DRAWER
      *             BASED ON BR-HIDE-END-BAL FLAG (WORLD PR#219)
      *  JTG 000322 MODIFIED TO UPDATE UNEARNED STATISTICS, MERCURY #223
      *  MJD 000406 ADDED UPDATE OF TY-PROCEEDS TO BY-PROCEEDS (MERC PR#223)
      *  MJD 000601 ADDED LOGIC TO SKIP PRINTING OF CKWRITTEN(4) IF IT IS
      *             ZERO.
      *  JTG 000613 CHANGED UPDATE TEST ON GP-LOAN-CONVERSION-UPD-FG
      *             TO ALWAYS CREATE A TRK TRANSACTION RECORD         #1203
      *             AND TO TEST WHETHER TO CREATE A GT RECORD
      *  MJD 000929 ADDED TY-LOCKBOX/BY-LOCKBOX (MFC PR#237)
      *  JTG 021121 ADDED NEW CODE FILE TYPE 'BR' -BATCH PAYMENT REFERENCE CODES
      *             BATCH PAYMENT REVISIONS                         REGACC #0006
      *  GLB 030508 WORLD PR#323 ADDED NEW "CT-FILE" FOR LIBLP/LPEOD5TR
      *             CODE TRAILER G/L INTERFACE TABLE & AMOUNT
      *             ONLY OCCURS IF CD-FEE-GIFTNET = "Y"
      *  CS  031111 ADDED FEE-COMMISSION TO WORKING STORAGE [WORLD PR#331]
      * JTG 040404 ADDED LOGIC FOR POSTING BATCH PAYMENTS TO ACCOUNT'S
      *            WITH 'LN-OTBAL'                                 CENTRIX #2253
      *            ADDED BATCH-REFCD-OT-GLNO
      *  BLF 050218 IF BAD GI, SKIP UPDATING GT & BY, BUT BRANCH BANK BALANCE
      *             IS UPDATED & WILL GET UPDATED AGAIN WHEN GI FIXED & EOD
      *             IS RE-RUN.  SINCE BANK BAL IS AFFECTED THROUGHOUT
      *             TR1-DETAIL, SAVED BR-BANKBAL AT BEGINNING OF PROGRAM
      *             (IN CASH SUMMARY RUN IN INITIALIZATION), THEN IF ERROR
      *             PERFORM RESTORE-BRANCH-BANK-BAL, PL# 464
      *  BLF 050920 UPDATE BRANCH BRANK BALANCE TO RC-BANK-BAL, WORLD PR# 433
      *  BLF 070119 ADD UPDATE OF GT-POST-USERID, GT-POST-DATE,
      *             GT-POST-TIME, SUPPORT PR# 2780
      *  BLF 070830 UNDID WORLD PR# 551
      *  BAH 080212 ADDED GP-RECAP-CLASS-TOTALS TO NOT PRINT CLASS TY
      *             RECAP TOTALS FOR HAMPTON ROADS PR#3150, DEFAULT AS "Y" " "
      *   CS 081608 ADDED BR-FILE FOR CHANGES MADE TO LPEOD5TR. BR-USE-PARTIC-
      *             FLAT-RATE-TABLE IS TESTED.  PR#232
      *  BAH 110411 UNCOMP'D CKWRITTEN, MEMORY ACCESS VIOLATION IF REALLY
      *             LARGE NUMBERS, PERF EQ PL# 735
      *  BAH 130412 ADDED LPEOD5-ERRCD TO PASS BACK TO LONPR0 SINCE FLDVAL
      *             AND XFER ARE GONE
      *  BLM 130104 REPLACE OLD LOGIC FOR WRITING GTFILE (USED TO START WITH
      *             LINE 1, WRITE, IF BAD ADD 1, ETC.  THIS COULD MEAN THAT
      *             FOR WORLD, WE WOULD ATTEMPT HUNDREDS OF WRITES TO 9990
      *             ACCOUNTS NEAR THE SAME TIME).  NOW WILL START GT WITH
      *             LINE OF 999999 & READ PREVIOUS, THEN START ATTEMPTING
      *             WRITES WITH THAT LINE #.  WORLD PR# 836
      *  CS 150814 ADD BRANCH-COMMISSION, USED IN LPEOD5TR  EXCEL PR#4821
      *
      * KEC 2015.1120 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED RC-TRANS-DATE FROM YYYMMDD TO CCYYMMDD.
      *  CS    160202 REMOVED UNSUB,ERSUB,GISUB FROM WORKING STORAGE, NOT
      *               USED SAVED VERSION LPEOD5.160202
      * BLM 160311 FIX FOR NEW TY-DATE FORMAT, PD0003
      * BLM 160314 FIX FOR NEW BY-DATE FORMAT, PD0003
      * KEC 2016.0315 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-FEE???? TO CD-GC-????.
      *  BLM 160726 IF GP-I-AM-WORLD & CD-COMM-PERCENT = 99.99, FORCE THE
      *             PAYABLE PERCENT TO 100, FOR LIVE CHECKS, WORLD PR# 1170
      * KEC 2016.0929 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED BY-DATE TO BY-DATE-CCYYMM FOR CONSISTENCY.
      *  BLM 160928 IF BR-ALLOW-SCANNED-CKS = Y, PRINT TD-CHECK-DEPOSIT ON
      *             RECAP, WORLD PR# 1188
      *
      * KEC 2016.1201 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED GT-DATE1 GT-DATE2 GT-DATE3 AS ACTIVE CCYYMMDD.
      *
      * KEC 2016.1206 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          REMOVED CO-FILE (LIBGB/GB..CO).
      * 170313 BAH ACTIVATE 8 DIGIT DATES, PD0006
      * 170321 BLM FIX GT-REF DATE TO BE YYMMDD, I THINK THIS HAS BEEN
      *            WRONG FOR A FEW MONTHS?
      * 180228 BLM ADD UPDATE OF TY-TYPE TABLE FOR NONFILE, AD&D AND
      *            ROADGARD
      * 180716 BAH CASH DRAWER WAS TRUNCATED
      *  BLM 181207 IF WORLD, CHANGE BANK TRANSACTIONS ID, IW, & BK TO
      *             UPDATE TO DEPOSIT BUCKET IN ED-FILE, SO THEY WILL
      *             BE UPDATED TO GTFILE WITH EXPLANATION OF "EOD-DEPOSIT",
      *             WORLD PR# 1353
      * KEC 190212 A32 F2SL - REVISED FOR SPLITTING THE BYFILE.
      *            REMOVED LOAN APP FIELDS IN BY & TY.
      * BAH 20190710 ADDED GLGTCL, FUTURE FIELDS WERE NOT CLEARED!
      * BAH 20190919 REMOVED ACCESS-CALL ON ALL TR FILES
      * BAH 20200625 ADDED OPEN-CDB1-FILE, OPEN-CD1-FILE, OPEN-LC1-FILE,
      *              OPEN-GI1-FILE, REMOVED THEM FROM LPEOD5TR, #TST001
      *  BAH 20210422 ADDED TR-POSTING-BR AND TR-OTHBR-FG FOR GLRCAP EXTRACT
      *  BLM 20210810 REMOVE TP-CODE, TP-POOLID NO LONGER IN KEY
      *  BAH 20220602 ADDED TR-ACTIONCD2 FOR GLRCAP EXTRACT, #1555
      *  RMZ 20241215 PERFORMANCE TUNING.  ADDED STORED PROCEDURE TO 
      *               RETREIVE GTFILE.
      *  MB  20240205 ADDED MOVE "N" TO ARRAYBR-LOAD-ALL-SW.
      *          THIS TELLS ARRAYBR NOT TO LOAD ALL BRANCHES UP FRONT
      *          WILL LOAD INDIVIDUAL BRANCH INFO INTO ARRAY WHEN ARRAYBR-READ
      *          IS PERFORMED FOR A BRANCH 
      *  RMZ 20240613 LOCKING ISSUE - ADDED STORED PROCEDURE/TEMP TABLE
      *               PROCESS, TO AVOID SQL DATABASE LOCK. (S35Q-67)
      *  MB  20250403 REMOVED USE OF ARRAYBR - CAN NOT USE STORED BRFILE DATA
      *               BECAUSE LPEOD5 UPDATES BRFILE.  S35Q-167
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
      * ED-AMT(5)  - TRANSFERS SERV-TO-PUR/PUR-TO-SERV (TRX)
      * ED-AMT(6)  - CHECKS (TRC)
      * ED-AMT(7)  - DEPOSITS (TRD)
      * ED-AMT(8)  - BULK TRANSFERS (TRB)
      * ED-AMT(9)  - CLASS TRANSFERS (TRJ)
      * ED-AMT(10) - ACCOUNT CONVERSIONS (TRK)

       COPY "LIBLP/LPFDER.CPY".
       COPY "LIBGB/GBFDPR.CPY".

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)LP/LPEOD5 S35 04/03/25-07:27:58 >".

      ******************************************************************
      *
      *    F I L E - R E C O R D - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01BY.CPY".
       COPY "LIBLP/LP01BYA.CPY".
       COPY "LIBLP/LP01BYB.CPY".
       COPY "LIBLP/LP01BY_SQL.CPY".
       COPY "LIBLP/LPWSBY.CPY".
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
       COPY "LIBGL/GL01GT.CPY".
       COPY "LIBGL/GL01GT_SQL.CPY".
       COPY "LIBGL/GLWSGT.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".
       COPY "LIBGB/GB01RC.CPY".
       COPY "LIBGB/GB01RC_SQL.CPY".
       COPY "LIBGB/GBWSRC.CPY".
       COPY "LIBLP/LP01TRP.CPY".
       COPY "LIBLP/LP01TRP_SQL.CPY".
       COPY "LIBLP/LPWSTRP.CPY".
       COPY "LIBLP/LP01TRD.CPY".
       COPY "LIBLP/LP01TRD_SQL.CPY".
       COPY "LIBLP/LPWSTRD.CPY".
       COPY "LIBLP/LP01TRC.CPY".
       COPY "LIBLP/LP01TRC_SQL.CPY".
       COPY "LIBLP/LPWSTRC.CPY".
       COPY "LIBLP/LP01TRL.CPY".
       COPY "LIBLP/LP01TRL_SQL.CPY".
       COPY "LIBLP/LPWSTRL.CPY".
       COPY "LIBLP/LP01TRX.CPY".
       COPY "LIBLP/LP01TRX_SQL.CPY".
       COPY "LIBLP/LPWSTRX.CPY".
       COPY "LIBLP/LP01TRB.CPY".
       COPY "LIBLP/LP01TRB_SQL.CPY".
       COPY "LIBLP/LPWSTRB.CPY".
       COPY "LIBLP/LP01TRJ.CPY".
       COPY "LIBLP/LP01TRJ_SQL.CPY".
       COPY "LIBLP/LPWSTRJ.CPY".
       COPY "LIBLP/LP01TRK.CPY".
       COPY "LIBLP/LP01TRK_SQL.CPY".
       COPY "LIBLP/LPWSTRK.CPY".
       COPY "LIBLP/LP01TRS.CPY".
       COPY "LIBLP/LP01TRS_SQL.CPY".
       COPY "LIBLP/LPWSTRS.CPY".
       COPY "LIBLP/LP01TRW.CPY".
       COPY "LIBLP/LP01TRW_SQL.CPY".
       COPY "LIBLP/LPWSTRW.CPY".

      *=================================================================
      *   HOLD-GT-REC FROM 'REPLACING LEADING' ON GT-REC.
      *=================================================================
       COPY "LIBGL/GL01GT.CPY" REPLACING LEADING "GT" BY "HOLD-GT".

      *=================================================================
       01  ED1-IFN              PIC X(3)     VALUE "ED1".
       01  PR-IFN               PIC X(2)     VALUE "PR".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.            
       01  QWS-GT-SOURCE        PIC 9.
       01  QWS-GT-DATE2         PIC X(10).
       01  QWS-GT-BRANCH        PIC 9(4).
       01  QWS-GT-ACCTNO        PIC 9(8).
       01  QWS-GT-DATE3         PIC X(10).
       01  QWS-GT-DRCRFG        PIC X.
       EXEC SQL END DECLARE SECTION END-EXEC.


       01  GT1-CONN-OPEN-SW     PIC X     VALUE "N".
       01  TY1-CONN-OPEN-SW     PIC X     VALUE "N".
       01  TRD1-CONN-OPEN-SW    PIC X     VALUE "N".
       01  BY1-CONN-OPEN-SW     PIC X     VALUE "N".
       01  RC1-CONN-OPEN-SW     PIC X     VALUE "N".

      ******************************************************************
      * BASE ELEMENT NUMBERS FOR UNEARNED AND EARNINGS
      * GL ACCOUNTS:
       01  SUBR                 PIC 9.
       01  TW-SUB               PIC 9(2).

       01  FUNK-SUB             PIC 999.
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
       01  HOLD-ER-MESSAGE.
           07  HOLD-ER-TYPE     PIC X(4).
           07  FILLER           PIC X.
           07  HOLD-ER-CODE     PIC XX.
           07  HOLD-ER-COMMA    PIC X.
           07  HOLD-ER-SUB-CODE PIC XX.
           07  HOLD-ER-E-PATH   PIC X(34).
           07  FILLER           PIC X.
           07  HOLD-ER-E-KEY    PIC X(44).
      **********************
      *   MISC WORKERS     *
      **********************
       01  TRANS-DATE-CCYYMMDD  PIC 9(8) COMP.
       01  SUB                  PIC 9(6) COMP.
       01  SUB2                 PIC 9(6) COMP.
       01  SUB3                 PIC 9(6) COMP.
       01  SUB4                 PIC 9(6)     COMP.
       01  TOTAL-PLA-RECV       PIC S9(9)V99 COMP.
       01  BYSUB1               PIC 9(6) COMP.
       01  BYSUB2               PIC 9(6) COMP.
       01  BYSUB3               PIC 9(6) COMP.
       01  ACT                  PIC 9(6) COMP.
       01  TYP                  PIC 9(6) COMP.
       01  L1                   PIC 9(6) COMP.
       01  FIRST-TIME           PIC X         VALUE "Y".
       01  FEE-COMMISSION       PIC S9(4)V99.

       01  HOLD-GP-PROMPTX.
           03  HOLD-GP-PROMPT   PIC XX.
           03  FILLER           PIC XX VALUE " :".

       01  CLEARING-WK          PIC S9(10)V99 COMP.
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
       01  BATCH-REFCD          PIC X(5)      VALUE LOW-VALUES.
       01  BATCH-REFCD-GLNO     PIC 9(11)     VALUE 0.
       01  BATCH-REFCD-OT-GLNO  PIC 9(11)     VALUE 0.
       01  GLNO                 PIC 9(11)     VALUE 0.
       01  GLNOX               REDEFINES GLNO.
           03  FILLER           PIC 9(4).
           03  FILLER           PIC 9(3).
           03  GLOTHBR          PIC 9(4).
       01  GLNOZ               REDEFINES GLNO.
           03  GLNO-BR          PIC 9(4).
           03  GLNO-NUMBER      PIC 9(7).
       01  GLAMT                PIC S9(9)V99 VALUE 0.
       01  GLWK                 PIC S9(9)V99 VALUE 0.
       01  HOLD-PL-EXP          PIC S9(9)V99 VALUE 0.
       01  HOLD-NP-CK-WRITTEN   PIC S9(9)V99 VALUE 0.
       01  TOTALS-TT.
           03  TOT-NO1          PIC S9(9)     COMP VALUE 0.
           03  TOT-AMT1         PIC S9(9)V99  COMP VALUE 0.
           03  TOT-NO2          PIC S9(9)     COMP VALUE 0.
           03  TOT-AMT2         PIC S9(9)V99  COMP VALUE 0.
       01  REF-WK.
           03  FILLER           PIC X(3) VALUE "LP-".
           03  REF-DATE-YYMMDD  PIC 9(6).
           03  FILLER           PIC X(1) VALUE "-".
           03  REF-BR           PIC 9(4).
       01  EXPL-WK.
           03  FILLER           PIC X(4) VALUE "EOD-".
           03  EXPL-DESC        PIC X(25).
           03  FILLER           PIC X(3).
       01  SAVE-GT-LINE         PIC 9(6) COMP.
       01  ERROR-FG             PIC X VALUE "N".
       01  WORK-TIME            PIC 9(6).
       01  FILLER REDEFINES WORK-TIME.
           04  TIME-HHMM        PIC 9(4).
           04  FILLER           PIC X(2).

       01  TODAYS-CKWRITTEN-TAB.
           03  CKWRITTEN        PIC S9(7)V99 OCCURS 8.
       01  WK-CA-TOTALS.
           03  WK-CA-TOT        PIC S9(7)V99 COMP OCCURS 8.
       01  WK-CA                PIC S9(7)V99 COMP VALUE 0.
       01  SHOULD-WE-UPDATE-GT  PIC X(3).
           88  WE-SHOULD-UPDATE-GT      VALUE "YES".
           88  WE-SHOULD-NOT-UPDATE-GT  VALUE "NO ".
       01  SAVE-TD-GLFG         PIC X   VALUE "Y".
       01  SAVE-BR-BANKBAL      PIC S9(9)V99 VALUE 0.
       01  HOLD-COMM-PERCENT    PIC 999V99 VALUE 0.
      *----------------------------------------------------------------
      * WORKERS FOR ACCOM-PL-TOTALS SECTION.
      *----------------------------------------------------------------
       01  PL-TYPE              PIC 9(6) COMP.
      * 01  PL-PERIOD            PIC 9(6) COMP.
       01  PL-ACTY              PIC 9(6) COMP.
       01  PL-SUB3              PIC 9(6) COMP.
       01  TOTAL-NO-PL          PIC S9(9)V99.
       01  TOTAL-AMT-PL         PIC S9(9)V99.
       01  TOTAL-O2-PL          PIC S9(9)V99.
       01  TOTAL-INT-PL         PIC S9(9)V99.

       01  ERRCD                PIC X VALUE SPACES.

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
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(26)    VALUE " ".
           03  H-TITLE          PIC X(41).

       01  HEAD3.
           03  FILLER           PIC X(10) VALUE "BRANCH :".
           03  H-BRNO           PIC 9(4).
           03  FILLER           PIC X VALUE " ".
           03  H-BRNAME         PIC X(20).
           03  FILLER           PIC X(29) VALUE " ".
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
           03  FILLER           PIC X(79) VALUE SPACES.
           03  FILLER           PIC X(43) VALUE
           "----  -------------     ----  -------------".

       01  HEAD9.
           03  FILLER           PIC X(44) VALUE SPACES.
           03  FILLER           PIC X(29) VALUE
           "BANK CREDITS---------------- ".
           03  FILLER           PIC X(50) VALUE
           "COLLECTED --------------- REBATES --------------".
       01  HEAD9A.
           03  FILLER           PIC X(73) VALUE SPACES.
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
           03  FILLER           PIC X(68) VALUE SPACES.
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

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  D-G-BRNO         PIC 9(4).
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

       COPY "LIBGL/ARRAYGIW.CPY".
       COPY "LIBLP/ARRAYLCW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBLP/LPEDPATH.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/POSTDATEW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBLP/LPSETTYW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       88  LPEOD5-PRU-88           VALUE "PRU.".
       78  LPEOD5-PRU-POS          VALUE 000000.
       78  LPEOD5-PRU-SIZE         VALUE 02.
       78  LPEOD5-PRU-ATR          VALUE "YYYNYNNNNNY".
       88  LPEOD5-QUEUE-88           VALUE "QUEUE.".
       78  LPEOD5-QUEUE-POS          VALUE 000000.
       78  LPEOD5-QUEUE-SIZE         VALUE 01.
       78  LPEOD5-QUEUE-ATR          VALUE "YNYNNNNNNNY".
       78  LPEOD5-COPIES-POS          VALUE 000000.
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      /***************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
      * MUST MATCH LONPR0
       01  LPEOD5-BUF.
           03  LPEOD5-BRNAME     PIC X(20).
           03  LPEOD5-PCODE      PIC XXX.
           03  LPEOD5-CONAME     PIC X(20).
           03  LPEOD5-BRANCH     PIC 9(4).
           03  LPEOD5-TRANS-DATE PIC 9(8).
           03  LPEOD5-SYSDATE    PIC X(8).
           03  LPEOD5-SEL        PIC 99.
           03  LPEOD5-USERID     PIC X(10).
           03  LPEOD5-ERRCD      PIC X.
       01  PASS-ED-PATH        PIC X(20).

       PROCEDURE DIVISION
            USING FORM-PATHNAME LPEOD5-BUF PASS-ED-PATH EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               ED-FILE PR-FILE ER-FILE.
       COPY "LIBGB/DECLAREOD.CPY".
           PERFORM CLOSE-PR-FILE.
           PERFORM CLOSE-GT1-CONNECTION-SSP.
           PERFORM CLOSE-TRD1-CONNECTION.
           PERFORM CLOSE-TY1-CONNECTION.
           PERFORM CLOSE-BY1-CONNECTION.
           CLOSE ED-FILE ER-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ****************************************
       MAIN-MODULE SECTION.
      ****************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.

           PERFORM INITIALIZATION.

           IF ERRCD = "R"
              GO TO GL-SUMMARY.

           IF ERRCD NOT = " "
              GO TO END-PROGRAM.


      * TRANSACTIONS
           PERFORM PROCESS-TRP-FILE.
      * DEPOSITS
           PERFORM PROCESS-TRD-FILE.
      * CHECKS
           PERFORM PROCESS-TRC-FILE.
      * DIRECT LOANS
           PERFORM PROCESS-TRL-FILE.
      * TRANSFERS
           PERFORM PROCESS-TRX-FILE.
      * BRANCH (BULK) TRANSFERS
           PERFORM PROCESS-TRB-FILE.
      * BRANCH (CLASS) TRANSFERS
           PERFORM PROCESS-TRJ-FILE.
      * ACCOUNT CONVERSIONS
           PERFORM PROCESS-TRK-FILE.
      * SALES FINANCE
           PERFORM PROCESS-TRS-FILE.

       GL-SUMMARY.
           IF ERROR-FG NOT = "N"
              PERFORM RESTORE-BRANCH-BANK-BAL
              GO TO TODAYS-SUMMARY.

           PERFORM CLOSE-ED1-FILE.
           PERFORM OPEN-ED1-FILE.
           PERFORM OPEN-GL1-FILE.
       NEXT-GL-SUMMARY.
           PERFORM READ-ED1-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-ROUTINE.

           PERFORM PRINT-GL-DETAIL.

           GO TO NEXT-GL-SUMMARY.

       END-ROUTINE.
           PERFORM CLOSE-GL1-FILE.
           PERFORM PRINT-GL-TOTALS.

           IF LPEOD5-SEL = 1
              GO TO TODAYS-SUMMARY.

           IF ERROR-FG = "Y" OR ERROR-FG = "I"
              GO TO TODAYS-SUMMARY.

           PERFORM OPEN-TRD1-FILE.

           MOVE LPEOD5-BRANCH     TO TD-BRNO.
           MOVE LPEOD5-TRANS-DATE TO TD-DATE.
           PERFORM READ-TRD1-FILE. *> EMBEDDED - TRDFILE
           IF IO-FG = 9
              MOVE "Y" TO ERROR-FG
              PERFORM SAVE-ERROR-MSG
              PERFORM CREATE-LOG
              GO TO TODAYS-SUMMARY.

           MOVE "Y"               TO TD-GLFG.
           PERFORM REWRITE-TRD1-FILE. *> EMBEDDED - TRDFILE
           PERFORM CLOSE-TRD1-FILE.

           PERFORM UPDATE-RC1-GL-IN-BAL.

       TODAYS-SUMMARY.
           IF ERROR-FG NOT = "I"
              PERFORM UPDATE-BYFILE.

           IF ERROR-FG NOT = "N"
              MOVE ERROR-FG TO ERRCD.

           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************

           PERFORM GET-GPENV.

           MOVE "YES" TO SHOULD-WE-UPDATE-GT.

      * WORK FILE CREATED IN LPEOD.C
           MOVE PASS-ED-PATH TO ED-PATH.

           MOVE " "          TO EO-SPOOL-FILE.
           MOVE LPEOD5-PCODE TO EO-SPOOL-PCODE.
           MOVE LPEOD5-TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO EO-SPOOL-DATE-NAME.
           MOVE "/0000RECAP" TO EO-SPOOL-FILE.

           PERFORM LOAD-EO-SPOOL.
           MOVE EO-SPOOL TO PRU-FILE.

           MOVE EO-SPOOL-PCODE     TO ER-ALL-SPOOL-PCODE.
           MOVE EO-SPOOL-DATE-NAME TO ER-ALL-SPOOL-DATE.
           PERFORM LOAD-ER-FILE.

           MOVE LPEOD5-QUEUE-POS  TO Q-POS.
           MOVE LPEOD5-COPIES-POS TO C-POS.
           MOVE LPEOD5-PRU-POS    TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

           PERFORM OPEN-PR-FILE.

           MOVE LPEOD5-TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY   TO H-TRDATE.
           MOVE LPEOD5-CONAME TO H-NAME.
           MOVE LPEOD5-BRNAME TO H-BRNAME.
           MOVE LPEOD5-USERID TO H-USERID.
           MOVE LPEOD5-BRANCH TO H-BRNO REF-BR.

           MOVE LPEOD5-SYSDATE TO H-DATE ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYMMDD.
           MOVE DATE-YYMMDD TO REF-DATE-YYMMDD.

           MOVE LPEOD5-TRANS-DATE TO TRANS-DATE-CCYYMMDD.

           PERFORM CASH-TILL-SUMMARY.

           IF LPEOD5-SEL NOT = 1
             MOVE "DAILY TOTALS G/L RECAP SUMMARY UPDATE" TO H-TITLE
             MOVE "G/L RECAP PRINT & UPDATE IN PROGRESS......." TO MESS
           ELSE
             MOVE "DAILY TOTALS G/L RECAP SUMMARY PRINT ONLY" TO H-TITLE
             MOVE "G/L RECAP PRINT ONLY IN PROGRESS...." TO MESS.
           PERFORM PROGRESS-MSG.

           MOVE " " TO HEAD4 HEAD5.
           MOVE HEAD13 TO HEAD4.
           MOVE HEAD14 TO HEAD5.
           MOVE 99 TO LN-COUNT.
           IF ERRCD = "R"
              PERFORM OPEN-ED1-FILE
              GO TO EXIT-INITIALIZE.

           MOVE " " TO ERRCD.

           PERFORM OPEN-ED1-FILE-OUTPUT.
           PERFORM CLOSE-ED1-FILE.
           PERFORM OPEN-ED1-FILE.

           PERFORM OPEN-CDB1-FILE.
           PERFORM OPEN-CD1-FILE.
           PERFORM OPEN-LC1-FILE.
           PERFORM OPEN-GI1-FILE.

           PERFORM ARRAYLC-LOAD.
           MOVE LPEOD5-BRANCH TO ARRAYGI-GI-BRANCH.
           PERFORM ARRAYGI-LOAD.

       EXIT-INITIALIZE.
           EXIT.

      *****************************************
       PROCESS-TRP-FILE SECTION.
      *****************************************
           PERFORM OPEN-TRP1-FILE.

           MOVE LPEOD5-BRANCH     TO TP-BRNO
                                     QTRP1-WBEG-BRNO
                                     QTRP1-WEND-BRNO.
           MOVE LPEOD5-TRANS-DATE TO TP-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRP1-WBEG-DATE
                                       QTRP1-WEND-DATE.

           MOVE 0                   TO TP-CLASS TP-ACCTNO TP-LINE
                                       QTRP1-WBEG-CLASS
                                       QTRP1-WBEG-ACCTNO
                                       QTRP1-WBEG-LINE.
           MOVE ALL "9"             TO QTRP1-WEND-CLASS
                                       QTRP1-WEND-ACCTNO
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

       PROCESS-TRP-NEXT.
           PERFORM READ-TRP1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRP-PATH-OWNBR NOT = TP-BRNO)
              GO TO EXIT-PROCESS-TRP-FILE.

           IF TP-DATE NOT = LPEOD5-TRANS-DATE
              GO TO EXIT-PROCESS-TRP-FILE.

           PERFORM TRP1-DETAIL.

           GO TO PROCESS-TRP-NEXT.

       EXIT-PROCESS-TRP-FILE.
           PERFORM CLOSE-TRP1-FILE.

      *****************************************
       PROCESS-TRD-FILE SECTION.
      *****************************************
           PERFORM OPEN-TRD1-FILE.

           MOVE LPEOD5-BRANCH     TO TD-BRNO
                                     QTRD1-WBEG-BRNO
                                     QTRD1-WEND-BRNO.

           MOVE LPEOD5-TRANS-DATE TO TD-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QTRD1-WBEG-DATE
                                        QTRD1-WEND-DATE.
 
           PERFORM START-TRD1-FILE. *> SSP - TRDFILE

       PROCESS-TRD-NEXT.
           PERFORM READ-TRD1-FILE-NEXT. *> EMBEDDED - TRDFILE
           IF (IO-FG NOT = 0) OR (TRD-PATH-OWNBR NOT = TD-BRNO)
              GO TO EXIT-PROCESS-TRD-FILE.

      *    UNLOCK TRD-FILE.
           IF TD-DATE NOT = LPEOD5-TRANS-DATE
              GO TO EXIT-PROCESS-TRD-FILE.

           PERFORM TRD1-DETAIL.

           GO TO PROCESS-TRD-NEXT.

       EXIT-PROCESS-TRD-FILE.
           PERFORM CLOSE-TRD1-FILE.

      *****************************************
       PROCESS-TRC-FILE SECTION.
      *****************************************
           PERFORM OPEN-TRC1-FILE.

           MOVE LPEOD5-BRANCH       TO TC-BRNO
                                       QTRC1-WBEG-BRNO
                                       QTRC1-WEND-BRNO.
           MOVE LPEOD5-TRANS-DATE TO TC-KEY-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRC1-WBEG-KEY-DATE
                                       QTRC1-WEND-KEY-DATE.
           MOVE 0                   TO TC-CKNO TC-CLASS TC-SEQ
                                       QTRC1-WBEG-CKNO
                                       QTRC1-WBEG-CLASS
                                       QTRC1-WBEG-SEQ.
           MOVE ALL "9"             TO QTRC1-WEND-CKNO
                                       QTRC1-WEND-CLASS
                                       QTRC1-WEND-SEQ.

           PERFORM START-TRC1-FILE.
       PROCESS-TRC-NEXT.
           PERFORM READ-TRC1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRC-PATH-OWNBR NOT = TC-BRNO)
              GO TO EXIT-PROCESS-TRC-FILE.

           IF TC-KEY-DATE NOT = LPEOD5-TRANS-DATE
              GO TO EXIT-PROCESS-TRC-FILE.

           PERFORM TRC1-DETAIL.

           GO TO PROCESS-TRC-NEXT.

       EXIT-PROCESS-TRC-FILE.
           PERFORM CLOSE-TRC1-FILE.

      *****************************************
       PROCESS-TRL-FILE SECTION.
      *****************************************
           PERFORM OPEN-TRL1-FILE.

           MOVE LPEOD5-BRANCH     TO TL-BRNO
                                     QTRL1-WBEG-BRNO
                                     QTRL1-WEND-BRNO.
           MOVE LPEOD5-TRANS-DATE TO TL-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRL1-WBEG-DATE
                                       QTRL1-WEND-DATE.
           MOVE 0                   TO TL-CLASS TL-ACCTNO
                                       TL-LAWCODE TL-LINE
                                       QTRL1-WBEG-CLASS
                                       QTRL1-WBEG-ACCTNO
                                       QTRL1-WBEG-LAWCODE
                                       QTRL1-WBEG-LINE.
           MOVE ALL "9"             TO QTRL1-WEND-CLASS
                                       QTRL1-WEND-ACCTNO
                                       QTRL1-WEND-LAWCODE
                                       QTRL1-WEND-LINE.

           PERFORM START-TRL1-FILE.
       PROCESS-TRL-NEXT.
           PERFORM READ-TRL1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRL-PATH-OWNBR NOT = TL-BRNO)
              GO TO EXIT-PROCESS-TRL-FILE.

           IF TL-DATE NOT = LPEOD5-TRANS-DATE
              GO TO EXIT-PROCESS-TRL-FILE.

           PERFORM TRL1-DETAIL.

           GO TO PROCESS-TRL-NEXT.

       EXIT-PROCESS-TRL-FILE.
           PERFORM CLOSE-TRL1-FILE.

      *****************************************
       PROCESS-TRX-FILE SECTION.
      *****************************************
           PERFORM OPEN-TRX1-FILE.

           MOVE LPEOD5-BRANCH     TO TX-BRNO
                                     QTRX1-WBEG-BRNO
                                     QTRX1-WEND-BRNO.
           MOVE LPEOD5-TRANS-DATE TO TX-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRX1-WBEG-DATE
                                       QTRX1-WEND-DATE.
           MOVE 0                   TO TX-CLASS TX-ACCTNO
                                       TX-LAWCODE TX-LINE
                                       QTRX1-WBEG-CLASS
                                       QTRX1-WBEG-ACCTNO
                                       QTRX1-WBEG-LAWCODE
                                       QTRX1-WBEG-LINE.
           MOVE ALL "9"             TO QTRX1-WEND-CLASS
                                       QTRX1-WEND-ACCTNO
                                       QTRX1-WEND-LAWCODE
                                       QTRX1-WEND-LINE.

           PERFORM START-TRX1-FILE.
       PROCESS-TRX-NEXT.
           PERFORM READ-TRX1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRX-PATH-OWNBR NOT = TX-BRNO)
              GO TO EXIT-PROCESS-TRX-FILE.

           IF TX-DATE NOT = LPEOD5-TRANS-DATE
              GO TO EXIT-PROCESS-TRX-FILE.

           PERFORM TRX1-DETAIL.

           GO TO PROCESS-TRX-NEXT.

       EXIT-PROCESS-TRX-FILE.
           PERFORM CLOSE-TRX1-FILE.

      *****************************************
       PROCESS-TRB-FILE SECTION.
      *****************************************
           PERFORM OPEN-TRB1-FILE.

           MOVE LPEOD5-BRANCH     TO TB-BRNO
                                     QTRB1-WBEG-BRNO
                                     QTRB1-WEND-BRNO.
           MOVE LPEOD5-TRANS-DATE TO TB-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRB1-WBEG-DATE
                                       QTRB1-WEND-DATE.
           MOVE 0                   TO TB-CLASS TB-ACCTNO
                                       TB-LAWCODE TB-LINE
                                       QTRB1-WBEG-CLASS
                                       QTRB1-WBEG-ACCTNO
                                       QTRB1-WBEG-LAWCODE
                                       QTRB1-WBEG-LINE.
           MOVE ALL "9"             TO QTRB1-WEND-CLASS
                                       QTRB1-WEND-ACCTNO
                                       QTRB1-WEND-LAWCODE
                                       QTRB1-WEND-LINE.

           PERFORM START-TRB1-FILE.
       PROCESS-TRB-NEXT.
           PERFORM READ-TRB1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRB-PATH-OWNBR NOT = TB-BRNO)
              GO TO EXIT-PROCESS-TRB-FILE.

           IF TB-DATE NOT = LPEOD5-TRANS-DATE
              GO TO EXIT-PROCESS-TRB-FILE.

           PERFORM TRB1-DETAIL.

           GO TO PROCESS-TRB-NEXT.

       EXIT-PROCESS-TRB-FILE.
           PERFORM CLOSE-TRB1-FILE.

      *****************************************
       PROCESS-TRK-FILE SECTION.
      *****************************************
           PERFORM OPEN-TRK1-FILE.

           MOVE LPEOD5-BRANCH     TO TK-BRNO
                                     QTRK1-WBEG-BRNO
                                     QTRK1-WEND-BRNO.
           MOVE LPEOD5-TRANS-DATE TO TK-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRK1-WBEG-DATE
                                       QTRK1-WEND-DATE.
           MOVE 0                   TO TK-CLASS TK-ACCTNO
                                       TK-LAWCODE TK-LINE
                                       QTRK1-WBEG-CLASS
                                       QTRK1-WBEG-ACCTNO
                                       QTRK1-WBEG-LAWCODE
                                       QTRK1-WBEG-LINE.
           MOVE ALL "9"             TO QTRK1-WEND-CLASS
                                       QTRK1-WEND-ACCTNO
                                       QTRK1-WEND-LAWCODE
                                       QTRK1-WEND-LINE.

           PERFORM START-TRK1-FILE.
       PROCESS-TRK-NEXT.
           PERFORM READ-TRK1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRK-PATH-OWNBR NOT = TK-BRNO)
              GO TO EXIT-PROCESS-TRK-FILE.

           IF TK-DATE NOT = LPEOD5-TRANS-DATE
              GO TO EXIT-PROCESS-TRK-FILE.

           PERFORM TRK1-DETAIL.

           GO TO PROCESS-TRK-NEXT.

       EXIT-PROCESS-TRK-FILE.
           PERFORM CLOSE-TRK1-FILE.

      *****************************************
       PROCESS-TRJ-FILE SECTION.
      *****************************************
           PERFORM OPEN-TRJ1-FILE.

           MOVE LPEOD5-BRANCH     TO TJ-BRNO
                                     QTRJ1-WBEG-BRNO
                                     QTRJ1-WEND-BRNO.
           MOVE LPEOD5-TRANS-DATE TO TJ-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRJ1-WBEG-DATE
                                       QTRJ1-WEND-DATE.
           MOVE 0                   TO TJ-CLASS TJ-ACCTNO
                                       TJ-LAWCODE TJ-LINE
                                       QTRJ1-WBEG-CLASS
                                       QTRJ1-WBEG-ACCTNO
                                       QTRJ1-WBEG-LAWCODE
                                       QTRJ1-WBEG-LINE.
           MOVE ALL "9"             TO QTRJ1-WEND-CLASS
                                       QTRJ1-WEND-ACCTNO
                                       QTRJ1-WEND-LAWCODE
                                       QTRJ1-WEND-LINE.

           PERFORM START-TRJ1-FILE.
       PROCESS-TRJ-NEXT.
           PERFORM READ-TRJ1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRJ-PATH-OWNBR NOT = TJ-BRNO)
              GO TO EXIT-PROCESS-TRJ-FILE.

           IF TJ-DATE NOT = LPEOD5-TRANS-DATE
              GO TO EXIT-PROCESS-TRJ-FILE.

           PERFORM TRJ1-DETAIL.

           GO TO PROCESS-TRJ-NEXT.

       EXIT-PROCESS-TRJ-FILE.
           PERFORM CLOSE-TRJ1-FILE.

      *****************************************
       PROCESS-TRS-FILE SECTION.
      *****************************************
           PERFORM OPEN-TRS1-FILE.

           MOVE LPEOD5-BRANCH     TO TS-BRNO
                                     QTRS1-WBEG-BRNO
                                     QTRS1-WEND-BRNO.
           MOVE LPEOD5-TRANS-DATE TO TS-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTRS1-WBEG-DATE
                                       QTRS1-WEND-DATE.
           MOVE 0                   TO TS-CLASS TS-ACCTNO
                                       TS-LAWCODE TS-LINE
                                       QTRS1-WBEG-CLASS
                                       QTRS1-WBEG-ACCTNO
                                       QTRS1-WBEG-LAWCODE
                                       QTRS1-WBEG-LINE.
           MOVE ALL "9"             TO QTRS1-WEND-CLASS
                                       QTRS1-WEND-ACCTNO
                                       QTRS1-WEND-LAWCODE
                                       QTRS1-WEND-LINE.

           PERFORM START-TRS1-FILE.
       PROCESS-TRS-NEXT.
           PERFORM READ-TRS1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRS-PATH-OWNBR NOT = TS-BRNO)
              GO TO EXIT-PROCESS-TRS-FILE.

           IF TS-DATE NOT = LPEOD5-TRANS-DATE
              GO TO EXIT-PROCESS-TRS-FILE.

           PERFORM TRS1-DETAIL.

           GO TO PROCESS-TRS-NEXT.

       EXIT-PROCESS-TRS-FILE.
           PERFORM CLOSE-TRS1-FILE.

      ****************************
       CASH-TILL-SUMMARY SECTION.
      ****************************

           MOVE LPEOD5-BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG = 9
              MOVE "Y" TO ERROR-FG
              PERFORM SAVE-ERROR-MSG
              PERFORM CREATE-LOG
              GO TO CASH-TILL-SUMMARY-EXIT.

           MOVE BR-BANKBAL TO SAVE-BR-BANKBAL.

           MOVE "C A S H   D R A W E R   S U M M A R Y" TO H-TITLE.
           MOVE "CASH DRAWER SUMMARY IN PROGRESS........." TO MESS.
           PERFORM PROGRESS-MSG.

           MOVE 0 TO SUB
              WK-CA-TOT(1) WK-CA-TOT(2) WK-CA-TOT(3)
              WK-CA-TOT(4) WK-CA-TOT(5) WK-CA-TOT(6)
              WK-CA-TOT(7) WK-CA-TOT(8).

           PERFORM OPEN-CA1-FILE.

           MOVE LPEOD5-BRANCH        TO CA-BRNO
                                        QCA1-WBEG-BRNO
                                        QCA1-WEND-BRNO.
           MOVE LPEOD5-TRANS-DATE    TO CA-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QCA1-WBEG-DATE
                                        QCA1-WEND-DATE.

           MOVE 0                    TO CA-TILLNO
                                        QCA1-WBEG-TILLNO.
           MOVE ALL "9"              TO QCA1-WEND-TILLNO.

           PERFORM START-CA1-FILE.
       NEXT-CASH-TILL.
           PERFORM READ-CA1-FILE-NEXT.
           IF IO-FG = 9
              PERFORM CLOSE-CA1-FILE
              GO TO CASH-TILL-SUMMARY-CHECKS.

           IF CA-BRNO NOT = LPEOD5-BRANCH OR
                 CA-DATE NOT = LPEOD5-TRANS-DATE
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
           SUBTRACT WK-CA FROM CA-ENDBAL GIVING WK-CA.
           MOVE WK-CA             TO D-CASHBAL(5).
           ADD WK-CA              TO WK-CA-TOT(5).
           MOVE CA-ENDBAL         TO D-CASHBAL(6).
           ADD CA-ENDBAL          TO WK-CA-TOT(6).
           IF BR-ALLOW-SCANNED-CKS = "Y"
              MOVE CA-CHECK-DEPOSIT TO D-CASHBAL(7)
              ADD CA-CHECK-DEPOSIT TO WK-CA-TOT(7).

           IF BR-AUTO-OPEN-BAL = "Y"
              MOVE CA-DEPOSIT TO D-CASHBAL(8)
              ADD CA-DEPOSIT  TO WK-CA-TOT(8).

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

      * THIS SECTION IS PERFORMED FROM INITIALIZATION AND TRDFILE
      * HAS NOT BEEN SET UP YET.
           PERFORM LOAD-TRD1-FILE.

           PERFORM OPEN-TRD1-FILE.

           MOVE LPEOD5-TRANS-DATE TO TD-DATE.
           MOVE LPEOD5-BRANCH     TO TD-BRNO.
           PERFORM READ-TRD1-FILE.
           IF IO-FG = 9
              IF LPEOD5-SEL NOT = 1
                 MOVE "Y" TO ERROR-FG
                 PERFORM SAVE-ERROR-MSG
                 PERFORM CREATE-LOG
                 GO TO CASH-TILL-SUMMARY-EXIT.

           IF IO-FG NOT = 9
              IF LPEOD5-SEL = 1
                 MOVE "Y" TO DAY-WAS-CLOSED.

           IF IO-FG = 0
              MOVE TD-GLFG TO SAVE-TD-GLFG
           ELSE
              MOVE "Y" TO SAVE-TD-GLFG.

           MOVE 4 TO LN-FEED.
           IF GP-BANK-GL = 0
              MOVE "OPENING BANK BALANCE:" TO DETAIL-LINE
              MOVE BR-BANKBAL TO D-CASH-TOT
              PERFORM WRITE-DETAIL-LINE.
           MOVE "+ CASH DEPOSITS     :" TO DETAIL-LINE.
           IF LPEOD5-SEL = 1
              IF DAY-WAS-CLOSED = " "
                 MOVE "THE DAY WAS NOT CLOSED!" TO D-NOT-CLOSED
                 MOVE 0 TO TD-DEPOSIT
              ELSE
                 MOVE TD-DEPOSIT TO D-CASH-TOT
                 IF TD-GLFG = "Y"
                    MOVE "UPDATE PREVIOUSLY RUN"  TO D-NOT-CLOSED
                 ELSE
                    NEXT SENTENCE
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
              COMPUTE D-CASH-TOT =
                  BR-BANKBAL + TD-DEPOSIT
                         - CKWRITTEN(1) - CKWRITTEN(2)
                          - CKWRITTEN(3) - CKWRITTEN(4)
                           + CKWRITTEN(5) - CKWRITTEN(6)
                            - CKWRITTEN(7) - CKWRITTEN(8)
              PERFORM WRITE-DETAIL-LINE.
       CASH-TILL-SUMMARY-EXIT.
           MOVE SPACE TO HEAD4 HEAD5.

      ************************************************
       TD-UPD-BR SECTION.
      ************************************************
           MOVE TD-BRNO TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           IF IO-FG = 9
              MOVE "Y" TO ERROR-FG
              PERFORM SAVE-ERROR-MSG
              PERFORM CREATE-LOG
              GO TO EXIT-TD-UPD-BR.

           IF GP-BANK-GL = 0
              ADD TD-DEPOSIT TO BR-BANKBAL.

           SUBTRACT HOLD-NP-CK-WRITTEN FROM BR-BANKBAL.

           PERFORM CHECK-UPDATE-BR.
           PERFORM CLOSE-BR-FILE.

       EXIT-TD-UPD-BR.
           EXIT. 

      ******************************************
       RESTORE-BRANCH-BANK-BAL SECTION.
      ******************************************

           MOVE LPEOD5-BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE. *> EMBEDDED - BRFILE
           IF IO-FG = 9
              MOVE "Y" TO ERROR-FG
              PERFORM SAVE-ERROR-MSG
              PERFORM CREATE-LOG
           ELSE
              MOVE SAVE-BR-BANKBAL TO BR-BANKBAL
              PERFORM REWRITE-BR-FILE. *> EMBEDDED - BRFILE
           PERFORM CLOSE-BR-FILE.

      *******************************************
       GET-TY-CHECK-SUMMARY SECTION.
      *******************************************
           PERFORM OPEN-TY1-FILE.

           MOVE LPEOD5-BRANCH       TO TY-BRNO
                                       QTY1-WBEG-BRNO
                                       QTY1-WEND-BRNO.
           MOVE LPEOD5-TRANS-DATE   TO TY-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTY1-WBEG-DATE
                                       QTY1-WEND-DATE.

           MOVE 0                   TO TY-CLASS
                                       QTY1-WBEG-CLASS.
           MOVE ALL "9"             TO QTY1-WEND-CLASS.

           PERFORM START-TY1-FILE. *> SSP - TYFILE
       GET-TY-CHECK-SUMMARY-NEXT.
           PERFORM READ-TY1-FILE-NEXT. *> EMBEDDED - TYFILE
           IF IO-FG = 9
              GO TO GET-TY-CHECK-SUMMARY-EXIT.

           IF TY-BRNO NOT = LPEOD5-BRANCH
              GO TO GET-TY-CHECK-SUMMARY-EXIT.

           IF LPEOD5-TRANS-DATE NOT = TY-DATE
              GO TO GET-TY-CHECK-SUMMARY-EXIT.

           ADD TY-CKWRITTEN(1) TO CKWRITTEN(1).
           ADD TY-CKWRITTEN(2) TO CKWRITTEN(2).
           ADD TY-CKWRITTEN(3) TO CKWRITTEN(3).
           ADD TY-CKWRITTEN(4) TO CKWRITTEN(4).
           ADD TY-CKWRITTEN(5) TO CKWRITTEN(5).
           ADD TY-CKWRITTEN(6) TO CKWRITTEN(6).
           ADD TY-CKWRITTEN(7) TO CKWRITTEN(7).
           ADD TY-CKWRITTEN(8) TO CKWRITTEN(8).

           IF TY-CLASS = 0
              MOVE TY-CKWRITTEN(8) TO HOLD-NP-CK-WRITTEN.

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
           PERFORM READ-GL1-FILE.
           IF IO-FG = 9
              MOVE "G" TO ERROR-FG
              PERFORM SAVE-ERROR-MSG
              PERFORM CREATE-LOG
              MOVE "* *  N O T   F O U N D  * *" TO GL-DESCRIPTION.

           MOVE GL-DESCRIPTION TO D-G-DESCRIPTION.
           MOVE ED-UPDATED     TO D-G-POST.
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
              GO TO PRINT-GL-BTJRNL.
           MOVE "   DEPOSIT" TO D-G-DESCRIPTION.
           IF ED-AMT(7) < 0
              MOVE ED-AMT(7) TO D-G-CR
              ADD ED-AMT(7) TO TOT-CR
           ELSE
              MOVE ED-AMT(7) TO D-G-DR
              ADD ED-AMT(7) TO TOT-DR.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-GL-BTJRNL.
           IF ED-AMT(8) = 0
              GO TO PRINT-GL-CTJRNL.
           MOVE "  BRANCH TRANSFER JOURNAL" TO D-G-DESCRIPTION.
           IF ED-AMT(8) < 0
              MOVE ED-AMT(8) TO D-G-CR
              ADD ED-AMT(8) TO TOT-CR
           ELSE
              MOVE ED-AMT(8) TO D-G-DR
              ADD ED-AMT(8) TO TOT-DR.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-GL-CTJRNL.
           IF ED-AMT(9) = 0
              GO TO PRINT-GL-CONVJRNL.
           MOVE "  CLASS TRANSFER JOURNAL" TO D-G-DESCRIPTION.
           IF ED-AMT(9) < 0
              MOVE ED-AMT(9) TO D-G-CR
              ADD ED-AMT(9) TO TOT-CR
           ELSE
              MOVE ED-AMT(9) TO D-G-DR
              ADD ED-AMT(9) TO TOT-DR.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-GL-CONVJRNL.
           IF ED-AMT(10) = 0
              GO TO PRINT-GL-CONT.
           MOVE "  ACCOUNT CONVERSION JOURNAL" TO D-G-DESCRIPTION.
           IF ED-AMT(10) < 0
              MOVE ED-AMT(10) TO D-G-CR
              ADD ED-AMT(10) TO TOT-CR
           ELSE
              MOVE ED-AMT(10) TO D-G-DR
              ADD ED-AMT(10) TO TOT-DR.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       PRINT-GL-CONT.
           IF LPEOD5-SEL = 1
              GO TO EXIT-PRINT-GL-DETAIL.

           IF ED-UPDATED = "Y"
              GO TO EXIT-PRINT-GL-DETAIL.
           IF SAVE-TD-GLFG = "Y"
              GO TO EXIT-PRINT-GL-DETAIL.
           IF WE-SHOULD-UPDATE-GT
              MOVE "TRANSACTION JOURNAL" TO EXPL-DESC
              MOVE ED-AMT(2) TO GLAMT
              PERFORM CREATE-GT1-DETAIL
              MOVE "SALES FINANCE REGISTER" TO EXPL-DESC
              MOVE ED-AMT(3) TO GLAMT
              PERFORM CREATE-GT1-DETAIL
              MOVE "DIRECT LOAN REGISTER" TO EXPL-DESC
              MOVE ED-AMT(4) TO GLAMT
              PERFORM CREATE-GT1-DETAIL
              MOVE "CLASS TRANSFER JOURNAL" TO EXPL-DESC
              MOVE ED-AMT(5) TO GLAMT
              PERFORM CREATE-GT1-DETAIL
              MOVE "CHECKS" TO EXPL-DESC
              MOVE ED-AMT(6) TO GLAMT
              PERFORM CREATE-GT1-DETAIL
              MOVE "DEPOSIT" TO EXPL-DESC
              MOVE ED-AMT(7) TO GLAMT
              PERFORM CREATE-GT1-DETAIL
              MOVE "BRANCH TRANSFER JOURNAL" TO EXPL-DESC
              MOVE ED-AMT(8) TO GLAMT
              PERFORM CREATE-GT1-DETAIL
              MOVE "CLASS TRANSFER JOURNAL" TO EXPL-DESC
              MOVE ED-AMT(9) TO GLAMT
              PERFORM CREATE-GT1-DETAIL
              MOVE "ACCOUNT CONVERSION JOURNAL" TO EXPL-DESC
              MOVE ED-AMT(10) TO GLAMT
              PERFORM CREATE-GT1-DETAIL.
           MOVE "Y" TO ED-UPDATED.
           PERFORM REWRITE-ED1-FILE.
       EXIT-PRINT-GL-DETAIL.
           EXIT.

      ****************************
       PRINT-GL-TOTALS SECTION.
      ****************************

           MOVE 3 TO LN-FEED.
           PERFORM GL-TOTS-WR-DUMMY 5 TIMES.

           MOVE "MANUAL TOTALS:" TO HEAD10X.
           MOVE HEAD10 TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "RECAP TOTALS :" TO DETAIL-LINE.
           MOVE TOT-DR TO D-G-DR.
           MOVE TOT-CR TO D-G-CR.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "GRAND TOTALS :" TO HEAD10X.
           MOVE HEAD10 TO DETAIL-LINE.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE SPACES TO HEAD10X.
           MOVE HEAD10 TO DETAIL-LINE.
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

           MOVE " "  TO ED-KEY.
           MOVE "G"  TO ED-ID.
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
           IF IO-FG = 9
              MOVE "Y" TO ERROR-FG
              PERFORM SAVE-ERROR-MSG
              PERFORM CREATE-LOG.
       EXIT-UPDATE-WORK-FILE.
           MOVE 0 TO GLAMT GLNO.

      ***********************************
       CREATE-GT1-DETAIL SECTION.
      ***********************************
           PERFORM OPEN-GT1-FILE.
           PERFORM OPEN-GT1-FILE-SSP.
           IF GLAMT = 0
              GO TO EXIT-CREATE-GT1-DETAIL.

           PERFORM CLEAR-GT-FILE.

           MOVE 3             TO GT-SOURCE.
           MOVE REF-WK        TO GT-REF.
      * WILL NEVER BE 0
           MOVE TRANS-DATE-CCYYMMDD TO GT-DATE1 GT-DATE2 GT-DATE3.
           MOVE ED-NO         TO GL-BR-ACCOUNT.
           MOVE GL-BRANCH     TO GT-BRANCH.
           MOVE GL-ACCOUNT    TO GT-ACCTNO.
           IF GLAMT > 0
              MOVE "1"        TO GT-DRCRFG
           ELSE
              MOVE "2"        TO GT-DRCRFG.
           MOVE GLAMT         TO GT-AMT.
           MOVE EXPL-WK       TO GT-EXPL.
           MOVE 0             TO GT-PERIOD.
           PERFORM SET-POSTDATE.
           MOVE POST-DATE     TO GT-POST-DATE.
           MOVE POST-TIME     TO WORK-TIME.
           MOVE TIME-HHMM     TO GT-POST-TIME.
           MOVE LPEOD5-USERID TO GT-POST-USERID.

      ****************************************************************
      *     WRITE NEW GT RECORD                                      *
      *  ROUTINE STARTS NOT > & READS PREVIOUS TO FIND HIGHEST       *
      *  CURRENT LINE # FOR GIVEN KEY; HUNDREDS OF BRANCHES ARE      *
      *  COMPETING TO WRITE GT'S TO THE SAME CORPORATE ACCOUNTS.     *
      *  SO IF WE FIND EXISTING GT FOR SAME KEY1, ADD 1 TO THAT      *
      *  LINE & TRY TO WRITE, OTHERWISE START WITH LINE 1
      ****************************************************************

      *    MOVE GT-REC    TO HOLD-GT-REC.

      *    MOVE 1 TO SAVE-GT-LINE.

      *    MOVE 999999 TO GT-LINE.

      *    MOVE GT-SOURCE            TO QGT1-WEND-SOURCE
      *                                 QGT1-WBEG-SOURCE.

      *    MOVE GT-DATE2             TO SQL-DATE-YYYYMMDD.
      *    PERFORM SQL-SET-DATE.
      *    MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WEND-DATE2
      *                                 QGT1-WBEG-DATE2.

      *    MOVE GT-BRANCH            TO QGT1-WEND-BRANCH
      *                                 QGT1-WBEG-BRANCH.

      *    MOVE GT-ACCTNO            TO QGT1-WEND-ACCTNO
      *                                 QGT1-WBEG-ACCTNO.

      *    MOVE GT-DATE3             TO SQL-DATE-YYYYMMDD.
      *    PERFORM SQL-SET-DATE.
      *    MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WEND-DATE3
      *                                 QGT1-WBEG-DATE3.

      *    MOVE GT-DRCRFG            TO QGT1-WEND-DRCRFG
      *                                 QGT1-WBEG-DRCRFG.

      *    MOVE ALL "0"              TO QGT1-WBEG-LINE.
      *    MOVE GT-LINE              TO QGT1-WEND-LINE.

      *-   -   -{ SUBTRACT ONE TO WHERE SCAN BEG FIELDS }
      *    IF ( GT-SOURCE = 0 )
      *       MOVE GT-SOURCE         TO QGT1-WSBEG-SOURCE
      *    ELSE
      *       COMPUTE QGT1-WSBEG-SOURCE = GT-SOURCE - 1.

      *    MOVE GT-DATE2             TO NDTE-DATE
      *    MOVE -1                   TO NDTE-HOLD
      *    PERFORM INCREMENT-DAYS
      *    MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD.
      *    PERFORM SQL-SET-DATE
      *    MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WSBEG-DATE2.

      *    IF ( GT-BRANCH = 0 )
      *       MOVE GT-BRANCH         TO QGT1-WSBEG-BRANCH
      *    ELSE
      *       COMPUTE QGT1-WSBEG-BRANCH = GT-BRANCH - 1.

      *    IF ( GT-ACCTNO = 0 )
      *       MOVE GT-ACCTNO         TO QGT1-WSBEG-ACCTNO
      *    ELSE
      *       COMPUTE QGT1-WSBEG-ACCTNO = GT-ACCTNO - 1.

      *    MOVE GT-DATE3             TO NDTE-DATE
      *    MOVE -1                   TO NDTE-HOLD
      *    PERFORM INCREMENT-DAYS.
      *    MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
      *    PERFORM SQL-SET-DATE
      *    MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WSBEG-DATE3.

      *    MOVE GT-DRCRFG            TO SQL-TEXT.
      *    PERFORM SQL-TEXT-DECREMENT.
      *    MOVE SQL-TEXT             TO QGT1-WSBEG-DRCRFG.

      *    PERFORM START-GT1-FILE-NOT-GREATER.
      *    PERFORM READ-GT1-FILE-PREVIOUS.
      **    UNLOCK GT-FILE.
      *    IF IO-BAD       
      *       GO TO WRITE-GT-LOOP. 

      *    IF GT-SOURCE = HOLD-GT-SOURCE
      *       IF GT-DATE2 = HOLD-GT-DATE2
      *          IF GT-BRANCH = HOLD-GT-BRANCH
      *             IF GT-ACCTNO = HOLD-GT-ACCTNO
      *                IF GT-DATE3 = HOLD-GT-DATE3
      *                   IF GT-DRCRFG = HOLD-GT-DRCRFG
      *                      MOVE GT-LINE TO SAVE-GT-LINE
      *                      ADD 1 TO SAVE-GT-LINE.

      * CALL SQL STORED PROCEDURE TO FIND THE LAST GT-LINE OF GT1-KEY
      
      D    STOP MESS.
           MOVE GT-REC    TO HOLD-GT-REC.
           PERFORM START-GT1-FILE-SSP.      *>  SSP GTFILE
           PERFORM READ-GT1-FILE-NEXT-SSP.  *>  EMBEDDED GTFILE

           IF ( IO-FG = 9 )
               MOVE 0 TO GT-LINE.

           PERFORM CLOSE-GT1-FILE-SSP.

           COMPUTE SAVE-GT-LINE = GT-LINE + 1.

       WRITE-GT-LOOP.
           MOVE HOLD-GT-REC TO GT-REC.
           MOVE SAVE-GT-LINE TO GT-LINE.
           PERFORM WRITE-GT1-FILE. *> EMBEDDED - GTFILE
           IF IO-BAD       
              IF GT-LINE < 999999
                 ADD 1 TO SAVE-GT-LINE
                 GO TO WRITE-GT-LOOP
              ELSE
                 PERFORM SAVE-ERROR-MSG
                 PERFORM CREATE-LOG
                 MOVE "Y" TO ERROR-FG.
       EXIT-CREATE-GT1-DETAIL.
           PERFORM CLOSE-GT1-FILE.
           PERFORM CLOSE-GT1-FILE-SSP.

      *********************************
       UPDATE-BYFILE SECTION.
      *********************************

           MOVE " " TO HEAD4.
           MOVE "  - - - DAILY TOTALS RECAP JOURNAL - - -" TO H-TITLE.
           PERFORM PROGRESS-MSG.
           PERFORM OPEN-BY1-FILE.
           PERFORM OPEN-TY1-FILE.

           MOVE LPEOD5-BRANCH       TO TY-BRNO
                                       QTY1-WBEG-BRNO
                                       QTY1-WEND-BRNO.
           MOVE LPEOD5-TRANS-DATE   TO TY-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTY1-WBEG-DATE
                                       QTY1-WEND-DATE.

           MOVE 0                   TO TY-CLASS
                                       QTY1-WBEG-CLASS.
           MOVE ALL "9"             TO QTY1-WEND-CLASS.

           PERFORM START-TY1-FILE. *> SSP - TYFILE
       NEXT-TY-TO-BY.
           PERFORM READ-TY1-FILE-NEXT. *> EMBEDDED - TYFILE
           IF IO-FG = 9
              GO TO UPDATE-BYFILE-EXIT.

           IF TY-BRNO NOT = LPEOD5-BRANCH
              GO TO UPDATE-BYFILE-EXIT.

           IF LPEOD5-TRANS-DATE NOT = TY-DATE
              GO TO UPDATE-BYFILE-EXIT.

           IF TY-BRFG = "Y"
      *       UNLOCK TY-FILE
              GO TO UPDATE-BY-TEST-TYPRINT.
           IF TY-CLASS = 0
              GO TO UPDATE-BY-END.
           IF LPEOD5-SEL = 1
      *       UNLOCK TY-FILE
              GO TO UPDATE-BY-TEST-TYPRINT.

           MOVE LPEOD5-TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM TO BY-DATE-CCYYMM.
           MOVE TY-BRNO  TO BY-BRNO.
           MOVE TY-CLASS TO BY-CLASS.
           PERFORM READ-BY1-FILE. *> EMBEDDED - BYFILE
           IF IO-FG = 9
              PERFORM CREATE-BY1-REC
              IF IO-FG = 9
                 GO TO UPDATE-BYFILE-EXIT.

           ADD TY-DLPARVHELD(1) TO BY-DLPARVHELD(1 1).
           ADD TY-DLPARVHELD(2) TO BY-DLPARVHELD(1 2)
                                   BY-DLPARVHELD(2 2).
           ADD TY-DLDISC      TO BY-DLDISC(1) BY-DLDISC(2).
           ADD TY-AMT-SOLD    TO BY-AMT-SOLD(1) BY-AMT-SOLD(2).
           ADD TY-AMT-TRANS   TO BY-AMT-TRANS(1) BY-AMT-TRANS(2).
           ADD TY-AMT-PURCH   TO BY-AMT-PURCH(1) BY-AMT-PURCH(2).
           ADD TY-NO-SOLD  TO BY-NO-SOLD(1) BY-NO-SOLD(2).
           ADD TY-NO-TRANS TO BY-NO-TRANS(1) BY-NO-TRANS(2).
           ADD TY-NO-PURCH TO BY-NO-PURCH(1) BY-NO-PURCH(2).
           ADD TY-OTHCOL   TO BY-OTHCOL(1) BY-OTHCOL(2).
           ADD TY-OT2COL   TO BY-OT2COL(1) BY-OT2COL(2).
           ADD TY-INTCOL   TO BY-INTCOL(1) BY-INTCOL(2).
           ADD TY-LATECOL  TO BY-LATECOL(1) BY-LATECOL(2).
           ADD TY-PRINCOL  TO BY-PRINCOL(1) BY-PRINCOL(2).
           ADD TY-DEFCOL   TO BY-DEFCOL(1) BY-DEFCOL(2).
           ADD TY-RNPRIN   TO BY-RNPRIN(1) BY-RNPRIN(2).
           ADD TY-INTREB   TO BY-INTREB(1) BY-INTREB(2).
           ADD TY-CLREB1   TO BY-CLREB1(1) BY-CLREB1(2).
           ADD TY-CLREB2   TO BY-CLREB2(1) BY-CLREB2(2).
           ADD TY-CLREB3   TO BY-CLREB3(1) BY-CLREB3(2).
           ADD TY-CLREB4   TO BY-CLREB4(1) BY-CLREB4(2).
           ADD TY-AHREB    TO BY-AHREB(1) BY-AHREB(2).
           ADD TY-PPREB    TO BY-PPREB(1) BY-PPREB(2).
           ADD TY-O1REB    TO BY-O1REB(1) BY-O1REB(2).
           ADD TY-O2REB    TO BY-O2REB(1) BY-O2REB(2).
           ADD TY-O3REB    TO BY-O3REB(1) BY-O3REB(2).
           ADD TY-O4REB    TO BY-O4REB(1) BY-O4REB(2).
           ADD TY-O5REB    TO BY-O5REB(1) BY-O5REB(2).
           ADD TY-O6REB    TO BY-O6REB(1) BY-O6REB(2).
           ADD TY-O7REB    TO BY-O7REB(1) BY-O7REB(2).
           ADD TY-SCREB    TO BY-SCREB(1) BY-SCREB(2).
           ADD TY-MFREB    TO BY-MFREB(1) BY-MFREB(2).
           ADD TY-DFREB    TO BY-DFREB(1) BY-DFREB(2).
           ADD TY-PART-CHGOFF TO BY-PART-CHGOFF(1)
                                 BY-PART-CHGOFF(2).
           ADD TY-ALLOTMENTS TO BY-ALLOTMENTS(1)
                                BY-ALLOTMENTS(2).
           ADD TY-LOCKBOX    TO BY-LOCKBOX(1)
                                BY-LOCKBOX(2).
           ADD TY-SUMWGTTM TO BY-SUMWGTTM.
           ADD TY-SUMWGTAV TO BY-SUMWGTAV.
           ADD TY-SUMWGTTA TO BY-SUMWGTTA.

           ADD TY-OTH-NO-COL TO BY-OTH-NO-COL(1)
                                BY-OTH-NO-COL(2).
           ADD TY-OT2-NO-COL TO BY-OT2-NO-COL(1)
                                BY-OT2-NO-COL(2).
           ADD TY-INT-NO-COL TO BY-INT-NO-COL(1)
                                BY-INT-NO-COL(2).
           ADD TY-LATE-NO-COL TO BY-LATE-NO-COL(1)
                                 BY-LATE-NO-COL(2).

           ADD TY-OTH-NO-WAIVED TO BY-OTH-NO-WAIVED(1)
                                   BY-OTH-NO-WAIVED(2).
           ADD TY-OT2-NO-WAIVED TO BY-OT2-NO-WAIVED(1)
                                   BY-OT2-NO-WAIVED(2).
           ADD TY-INT-NO-WAIVED TO BY-INT-NO-WAIVED(1)
                                   BY-INT-NO-WAIVED(2).
           ADD TY-LATE-NO-WAIVED TO BY-LATE-NO-WAIVED(1)
                                    BY-LATE-NO-WAIVED(2).

           ADD TY-OTH-AMT-WAIVED TO BY-OTH-AMT-WAIVED(1)
                                    BY-OTH-AMT-WAIVED(2).
           ADD TY-OT2-AMT-WAIVED TO BY-OT2-AMT-WAIVED(1)
                                    BY-OT2-AMT-WAIVED(2).
           ADD TY-INT-AMT-WAIVED TO BY-INT-AMT-WAIVED(1)
                                    BY-INT-AMT-WAIVED(2).
           ADD TY-LATE-AMT-WAIVED TO BY-LATE-AMT-WAIVED(1)
                                     BY-LATE-AMT-WAIVED(2).

           IF GP-UNEARNED-STATS = "Y"
              ADD TY-PROCEEDS TO BY-PROCEEDS(1)
                                 BY-PROCEEDS(2).

           PERFORM UPDATE-BY-PL-RECOVERED VARYING
              SUB FROM 1 BY 1 UNTIL SUB > 5.

           PERFORM UPDATE-BY-INS-TYPE VARYING
              SUB FROM 1 BY 1 UNTIL SUB > 10.

           PERFORM UPDATE-BY-FEE-TYPE VARYING
              SUB FROM 1 BY 1 UNTIL SUB > 3.

           PERFORM UPDATE-BY-ACTIVITY VARYING
              ACT FROM 1 BY 1 UNTIL ACT > 2.

           PERFORM UPDATE-BY-DEL
              VARYING SUB3 FROM 1 BY 1 UNTIL SUB3 > 10
                AFTER SUB2 FROM 1 BY 1 UNTIL SUB2 > 2.

      * UPDATE UNEARNED STATISTICS NET AND VOLUME
      
           IF GP-UNEARNED-STATS = "Y"
              PERFORM VARYING BYSUB3 FROM 1 BY 1 UNTIL BYSUB3 > 6
                        AFTER BYSUB2 FROM 1 BY 1 UNTIL BYSUB2 > 2
                  IF BYSUB2 = 1
                     ADD TY-UNEARNED-AMT(BYSUB2 BYSUB3)
                              TO BY-UNEARNED-AMT(1 BYSUB2 BYSUB3)
                  ELSE
                     ADD TY-UNEARNED-AMT(BYSUB2 BYSUB3)
                              TO BY-UNEARNED-AMT(1 BYSUB2 BYSUB3)
                                 BY-UNEARNED-AMT(2 BYSUB2 BYSUB3)
                  END-IF
              END-PERFORM.

           PERFORM REWRITE-BY1-FILE. *> EMBEDDED - BYFILE
           GO TO UPDATE-BY-END.

       UPDATE-BY-PL-RECOVERED.
           ADD TY-PL-PRINCOL(SUB) TO BY-PL-PRINCOL(1 SUB)
                                     BY-PL-PRINCOL(2 SUB).
           ADD TY-PL-INTCOL(SUB) TO BY-PL-INTCOL(1 SUB)
                                     BY-PL-INTCOL(2 SUB).
           ADD TY-PL-LATECOL(SUB) TO BY-PL-LATECOL(1 SUB)
                                     BY-PL-LATECOL(2 SUB).
           ADD TY-PL-O2COL(SUB) TO BY-PL-O2COL(1 SUB)
                                     BY-PL-O2COL(2 SUB).

       UPDATE-BY-INS-TYPE.
           ADD TY-INS-NO-ELIG(SUB) TO BY-INS-NO-ELIG(1 SUB)
                                      BY-INS-NO-ELIG(2 SUB).
           ADD TY-INS-NO-WROTE(SUB) TO BY-INS-NO-WROTE(1 SUB)
                                       BY-INS-NO-WROTE(2 SUB).
           ADD TY-INS-NO-NOTWROTE(SUB) TO BY-INS-NO-NOTWROTE(1 SUB)
                                          BY-INS-NO-NOTWROTE(2 SUB).
           ADD TY-INS-AMT-WROTE(SUB) TO BY-INS-AMT-WROTE(1 SUB)
                                        BY-INS-AMT-WROTE(2 SUB).

       UPDATE-BY-FEE-TYPE.
           ADD TY-FEE-NO-ELIG(SUB) TO BY-FEE-NO-ELIG(1 SUB)
                                      BY-FEE-NO-ELIG(2 SUB).
           ADD TY-FEE-NO-WROTE(SUB) TO BY-FEE-NO-WROTE(1 SUB)
                                       BY-FEE-NO-WROTE(2 SUB).
           ADD TY-FEE-NO-NOTWROTE(SUB) TO BY-FEE-NO-NOTWROTE(1 SUB)
                                          BY-FEE-NO-NOTWROTE(2 SUB).
           ADD TY-FEE-AMT-WROTE(SUB) TO BY-FEE-AMT-WROTE(1 SUB)
                                        BY-FEE-AMT-WROTE(2 SUB).

       UPDATE-BY-ACTIVITY.
           PERFORM UPDATE-BY-TYPE VARYING
              TYP FROM 1 BY 1 UNTIL TYP > 11.

       UPDATE-BY-TYPE.
           ADD TY-NO(ACT TYP) TO BY-NO(1 ACT TYP).
           ADD TY-AMT(ACT TYP) TO BY-AMT(1 ACT TYP).
           ADD TY-O2(ACT TYP) TO BY-O2(1 ACT TYP).
           ADD TY-INT(ACT TYP) TO BY-INT(1 ACT TYP).

           IF TYP < 11
              ADD TY-NO-PL(ACT TYP) TO BY-NO-PL(1 ACT TYP)
              ADD TY-AMT-PL(ACT TYP) TO BY-AMT-PL(1 ACT TYP)
              ADD TY-O2-PL(ACT TYP) TO BY-O2-PL(1 ACT TYP)
              ADD TY-INT-PL(ACT TYP) TO BY-INT-PL(1 ACT TYP).
           IF TYP < 6
              ADD TY-NO-REPO(ACT TYP) TO BY-NO-REPO(1 ACT TYP)
              ADD TY-AMT-REPO(ACT TYP) TO BY-AMT-REPO(1 ACT TYP)
              ADD TY-O2-REPO(ACT TYP) TO BY-O2-REPO(1 ACT TYP)
              ADD TY-INT-REPO(ACT TYP) TO BY-INT-REPO(1 ACT TYP).
           IF TYP < 4
              ADD TY-DLRVPRIN(ACT TYP) TO BY-DLRVPRIN(1 ACT TYP)
              ADD TY-DLRVCHGS(ACT TYP) TO BY-DLRVCHGS(1 ACT TYP).

           IF ACT = 2
              ADD TY-NO(ACT TYP) TO BY-NO(2 ACT TYP)
              ADD TY-AMT(ACT TYP) TO BY-AMT(2 ACT TYP)
              ADD TY-O2(ACT TYP) TO BY-O2(2 ACT TYP)
              ADD TY-INT(ACT TYP) TO BY-INT(2 ACT TYP)
              IF TYP < 11
                 ADD TY-NO-PL(ACT TYP) TO BY-NO-PL(2 ACT TYP)
                 ADD TY-AMT-PL(ACT TYP) TO BY-AMT-PL(2 ACT TYP)
                 ADD TY-O2-PL(ACT TYP) TO BY-O2-PL(2 ACT TYP)
                 ADD TY-INT-PL(ACT TYP) TO BY-INT-PL(2 ACT TYP)
              END-IF
              IF TYP < 6
                 ADD TY-NO-REPO(ACT TYP) TO BY-NO-REPO(2 ACT TYP)
                 ADD TY-AMT-REPO(ACT TYP) TO BY-AMT-REPO(2 ACT TYP)
                 ADD TY-O2-REPO(ACT TYP) TO BY-O2-REPO(2 ACT TYP)
                 ADD TY-INT-REPO(ACT TYP) TO BY-INT-REPO(2 ACT TYP)
              END-IF
              IF TYP < 4
                 ADD TY-DLRVPRIN(ACT TYP) TO BY-DLRVPRIN(2 ACT TYP)
                 ADD TY-DLRVCHGS(ACT TYP) TO BY-DLRVCHGS(2 ACT TYP).

       UPDATE-BY-DEL.
           ADD TY-UNITS(SUB2 SUB3) TO BY-UNITS(1 SUB2 SUB3).
           ADD TY-OUTBAL(SUB2 SUB3) TO BY-OUTBAL(1 SUB2 SUB3).
           ADD TY-UNITS-BNKRPT(SUB2 SUB3)
                               TO BY-UNITS-BNKRPT(1 SUB2 SUB3).
           ADD TY-OUTBAL-BNKRPT(SUB2 SUB3)
                               TO BY-OUTBAL-BNKRPT(1 SUB2 SUB3).

       UPDATE-BY-END.
           IF LPEOD5-SEL NOT = 1
              MOVE "Y" TO TY-BRFG
              PERFORM REWRITE-TY1-FILE. *> EMBEDDED - TYFILE

       UPDATE-BY-TEST-TYPRINT.
           IF TY-CLASS NOT = 0 OR TY-CKWRITTEN(6) NOT = 0
              IF GP-RECAP-CLASS-TOTALS NOT = "N"
                 PERFORM TY-TODAYS-TOTAL-PRINT.
           GO TO NEXT-TY-TO-BY.

       UPDATE-BYFILE-EXIT.
           PERFORM CLOSE-TY1-FILE.
           PERFORM CLOSE-BY1-FILE.

      ****************************
       CREATE-BY1-REC SECTION.
      ****************************
           PERFORM CLEAR-BY-FILE.
           MOVE LPEOD5-TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM TO BY-DATE-CCYYMM.
           MOVE TY-BRNO  TO BY-BRNO.
           MOVE TY-CLASS TO BY-CLASS.
           PERFORM WRITE-BY1-FILE.
           IF IO-FG = 9
              MOVE "Y" TO ERROR-FG
              PERFORM SAVE-ERROR-MSG
              PERFORM CREATE-LOG
              MOVE 9 TO IO-FG.

      ****************************
       UPDATE-RC1-GL-IN-BAL SECTION.
      ****************************
           PERFORM OPEN-RC1-FILE.
           PERFORM SET-RC1-KEY.
           PERFORM READ-RC1-FILE. *> EMBEDDED - RCFILE
           IF IO-FG = 9
              GO TO IO-ERROR.
           IF (TOT-DR + TOT-CR = 0)
              MOVE "Y" TO RC-GL-FG
           ELSE
              MOVE "N" TO RC-GL-FG.
           MOVE BR-BANKBAL TO RC-BANK-BAL.
           PERFORM REWRITE-RC1-FILE. *> EMBEDDED - RCFILE
           PERFORM CLOSE-RC1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

      *********************************
       TY-TODAYS-TOTAL-PRINT SECTION.
      *********************************

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
           MOVE 1 TO PL-TYPE PL-ACTY.
           PERFORM ACCUM-PL-TOTALS.
           MOVE TOTAL-NO-PL TO D-P-NO1.
           MOVE TOTAL-AMT-PL TO D-P-AMT1.
           MOVE 1 TO PL-TYPE.
           MOVE 2 TO PL-ACTY.
           PERFORM ACCUM-PL-TOTALS.
           MOVE TOTAL-NO-PL TO D-P-NO2.
           MOVE TOTAL-AMT-PL TO D-P-AMT2.

           MOVE 0 TO TOTAL-PLA-RECV.
           PERFORM VARYING SUB4 FROM 1 BY 1 UNTIL SUB4 > 5
             ADD TY-PL-PRINCOL(SUB4) TO TOTAL-PLA-RECV
             ADD TY-PL-INTCOL(SUB4) TO TOTAL-PLA-RECV
             ADD TY-PL-LATECOL(SUB4) TO TOTAL-PLA-RECV
           END-PERFORM.

           MOVE TOTAL-PLA-RECV TO D-P-AMT3.
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

      * PRINT REPOSSESSIONS

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

      *********************************
       WRITE-DETAIL-LINE SECTION.
      *********************************

           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *********************************
       HEAD-PAGE SECTION.
      *********************************

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

      *********************************
       CREATE-LOG SECTION.
      *********************************
           PERFORM LOAD-ER-FILE.
           MOVE ER-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "ER" TO E-FILE
              OPEN OUTPUT ER-FILE
              CLOSE ER-FILE.
           MOVE E-FILE TO E-FILEX.
           PERFORM OPEN-ER-FILE.

           PERFORM SET-POSTDATE.
           MOVE POST-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO ER-DATE.
           MOVE POST-TIME TO ER-TIME.
           INSPECT ER-TIME REPLACING ALL "/" BY ":".
           MOVE LPEOD5-USERID TO ER-USER.
           MOVE "LP" TO ER-DIR.
           MOVE "/" TO ER-SLASH.
           MOVE "LPEOD5" TO ER-PROGRAM.
           MOVE HOLD-ER-MESSAGE TO ER-MESSAGE.
           PERFORM WRITE-ER-REC.
           PERFORM CLOSE-ER-FILE.

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

      * IF BR-HIDE-END-BAL = "Y" THEN LIST EVERT ATTEMPT TO CLOSE THE
      *   CASH DRAWER IN WHICH THE OVER/SHORT AMOUNT DID NOT = ZERO.

           PERFORM OPEN-TRW1-FILE.

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

       COPY "LIBGL/ARRAYGI.CPY".
       COPY "LIBLP/ARRAYLC.CPY".
       COPY "LIBGL/GLGTCL.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBLP/LPBYCL.CPY".
       COPY "LIBLP/POSTDATE.CPY".
       COPY "LIBLP/LPEOD5TR.CPY".
       COPY "LIBGB/SCREEN.CPY".


      **************************************
       MISC-PARA SECTION.
      **************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".
       FORM-RESET.
       SETUP-LINK-INFO.
       VDU-CONVERT-FIELD.
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
       TR-CODE-C.
      * ED-AMT(6)  - CHECKS (TRC)
      * ED-AMT(7)  - DEPOSITS (TRD)
           IF (TC-TYPE = "BK" OR "ID" OR "IW")
              ADD GLAMT TO ED-AMT(7)
           ELSE
              ADD GLAMT TO ED-AMT(6).
       SET-RC1-KEY.
           MOVE LPEOD5-BRANCH TO RC-BRNO.
           MOVE LPEOD5-TRANS-DATE TO RC-TRANS-DATE.
       GET-CODE.
           MOVE "GC"    TO CDB-TYPE.
      * SET IN LPEOD5TR    CDB-BRANCH.
      * SET IN LPEOD5TR    CDB-CODE.
           PERFORM READ-CDB1-FILE.
       TEST-BR-FEE-GL-BRNO.
           IF BR-FEE-GL-BRNO NUMERIC
              IF BR-FEE-GL-BRNO NOT = 0
                 MOVE BR-FEE-GL-BRNO TO GLNO-BR.
       PROGRESS-MSG.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       CHECK-UPDATE-BR.
           IF LPEOD5-SEL NOT = 1
              PERFORM REWRITE-BR-FILE.
      *    ELSE
      *       UNLOCK BR-FILE.
       SAVE-ERROR-MSG.
           MOVE E-MSG TO HOLD-ER-TYPE.
           MOVE E-CODE TO HOLD-ER-CODE.
           MOVE "," TO HOLD-ER-COMMA.
           MOVE E-SUBCODE TO HOLD-ER-SUB-CODE.
           MOVE E-FILE TO HOLD-ER-E-PATH.
           MOVE E-KEYX TO HOLD-ER-E-KEY.
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
           MOVE SPACE TO T-SPACE.

      **************************************************
       IO-ROUTINES SECTION.
      **************************************************
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
       COPY "LIBLP/LPBYGS_SQL.CPY".
       COPY "LIBGL/GLGTGS_SQL.CPY".
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGL/GLGIGS_SQL.CPY".
       COPY "LIBGB/GBRCGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/EOSPOOLIO.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPPRINT.CPY".
      * COPY "LIBGB/GBBRIN.CPY".
      * COPYMEMBER: LIBGB/GBBRIN
      * BAH 2022-0923 BAH ADDED BR-VERITEC-LICENSE
      * BAH 2023-0419 BAH ADDED BR-TEST-PP-AUTO-INSCOVR #1594A
      * BAH 2023-0713 BAH ADDED BR-USE-NEW-WAFRGO-ACCT #1616
      * BAH 2023-1113 BAH ADDED BR-ALLOW-F3-ALL-DOCS #1630
      * JKC 2024-0124 ADDED OTIS TRACE LOGGING LOGIC
      *
       LOAD-BR-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BR-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BR-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BR-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BR-ALL-MACHINE
              MOVE BR-ALL-PATH             TO BR-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BR-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BR-ALL-BASE
              MOVE FILPATH-MACHINE         TO BR-ALL-MACHINE
              MOVE BR-ALL-PATH             TO BR-PATH.

      *-----------------------------------------------------------------
       OPEN-BR-FILE.
           PERFORM LOAD-BR-FILE.
           PERFORM OPEN-IT.
           MOVE BR-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       COUNT-BR-FILE.
           PERFORM COUNT-IT.
           MOVE BR-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.

           EXEC SQL
            SELECT COUNT(*)
            INTO :BR1-ROW-COUNT
            FROM DBO.BRFILE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO COUNT-BR-FILE.

      *-----------------------------------------------------------------
       START-BR-FILE.
           PERFORM START-IT.
           MOVE BR-PATH-SQL TO E-FILE.
           MOVE BR-NO TO E-KEYX.

           IF ( EXT-TCLP-TRACE-BRANCH )
              STRING "START-BR-FILE | ", BR-NO
                     DELIMITED BY SIZE INTO EXT-TCLP-TRACE-MESS
              CALL "TRACE" USING FORM-PATHNAME
              CANCEL "TRACE".

           IF ( BR-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BR-FILE.

           IF ( QBR-WEND-NO = ZEROES    ) OR
              ( QBR-WEND-NO NOT NUMERIC )
              MOVE BR-NO                TO QBR-WBEG-NO
              MOVE ALL "9"              TO QBR-WEND-NO.

           EXEC SQL
            DECLARE BR_CURSOR CURSOR FOR
            SELECT BRFILE.BR_NO,
                   CAST(BRFILE.BR_LTOUCH_DATE AS VARCHAR(10)),
                   BRFILE.BR_NAME,
                   BRFILE.BR_ADR1,
                   BRFILE.BR_ADR2,
                   BRFILE.BR_CITY,
                   BRFILE.BR_STATE,
                   BRFILE.BR_ZIP,
                   BRFILE.BR_PHONE,
                   BRFILE.BR_RETAIN,
                   BRFILE.BR_NEXT_CHECK,
                   BRFILE.BR_PRINTER_1,
                   BRFILE.BR_PRINTER_2,
                   BRFILE.BR_PRINTER_3,
                   BRFILE.BR_PRINTER_4,
                   BRFILE.BR_FEE_GL_BRNO,
                   BRFILE.BR_REMIT1,
                   BRFILE.BR_REMIT2,
                   BRFILE.BR_REMIT3,
                   BRFILE.BR_STMNT1,
                   BRFILE.BR_STMNT2,
                   BRFILE.BR_INS_PRODUCER_NUM,
                   BRFILE.BR_DLRCHK,
                   BRFILE.BR_FRONT_END_FG,
                   BRFILE.BR_EMLNRG_PHONE,
                   BRFILE.BR_POST_GL,
                   BRFILE.BR_ALLOTMNTS,
                   BRFILE.BR_EXTRACT_AUTO_UPD,
                   BRFILE.BR_PCNI_LEDGER,
                   BRFILE.BR_PRINTERFG_1,
                   BRFILE.BR_PRINTERFG_2,
                   BRFILE.BR_PRINTERFG_3,
                   BRFILE.BR_PRINTERFG_4,
                   BRFILE.BR_NEGDLRSV,
                   BRFILE.BR_CPCORP_ORDER,
                   BRFILE.BR_PRIO_MEMO_FLAG,
                   BRFILE.BR_PREBILL_PL_JD_OPT,
                   BRFILE.BR_LOAN_NUM_FLAG,
                   BRFILE.BR_DOWNPY_FLAG,
                   BRFILE.BR_AUTO_INS_EARN,
                   BRFILE.BR_OTH_INS_LEVELS,
                   BRFILE.BR_ALLOW_INTERBR,
                   BRFILE.BR_UNER_DLANRP,
                   BRFILE.BR_SECOND_PROMISE,
                   BRFILE.BR_AVAIL_CREDIT,
                   BRFILE.BR_RECP1,
                   BRFILE.BR_RECP2,
                   BRFILE.BR_INC_WAIVERS,
                   BRFILE.BR_INC_INS,
                   BRFILE.BR_BP_NEXT_BATCH,
                   BRFILE.BR_BP_NOPAYOFF,
                   BRFILE.BR_BP_ODDPAY,
                   BRFILE.BR_BP_LC,
                   BRFILE.BR_BP_NOPRIN,
                   BRFILE.BR_BP_FLASH,
                   BRFILE.BR_BP_ACTIONCD,
                   BRFILE.BR_BP_MAXCON,
                   BRFILE.BR_BP_MAXREC,
                   BRFILE.BR_BP_PL,
                   BRFILE.BR_BP_JD,
                   BRFILE.BR_BP_BRANCH,
                   BRFILE.BR_AGE_ERN,
                   BRFILE.BR_OTHER_BRANCH_GL,
                   BRFILE.BR_SALES_SMALL,
                   BRFILE.BR_NEW_COLL_INQUIRY,
                   BRFILE.BR_CHANGE_MEMO,
                   BRFILE.BR_INS_ADDON,
                   BRFILE.BR_TRAILER_REC,
                   BRFILE.BR_ALDEL_FLAG,
                   BRFILE.BR_IB_LCHG_MESS,
                   BRFILE.BR_COL_QUEUE_PAST_DUE,
                   BRFILE.BR_LI_DLCOMM_CMSI_PP_MAX,
                   BRFILE.BR_ADDON_FEECD_O2_1,
                   BRFILE.BR_ADDON_FEECD_O2_2,
                   BRFILE.BR_AUTO_CHECK,
                   BRFILE.BR_RECEIPT_PROMDATE,
                   BRFILE.BR_RECEIPT_INTPD,
                   BRFILE.BR_COLL_ID_SCAN,
                   BRFILE.BR_STOP_ON_COLL,
                   BRFILE.BR_DUEDATE_CHANGE_CD,
                   BRFILE.BR_CHECK_EOM_FG,
                   BRFILE.BR_VOLUME_DLANRP,
                   BRFILE.BR_PDTH_NXDUE,
                   BRFILE.BR_LC_CODE,
                   BRFILE.BR_VALIDATE_PRUS,
                   BRFILE.BR_AUTO_OPEN_BAL,
                   BRFILE.BR_OPEN_BAL_1,
                   BRFILE.BR_OPEN_BAL_2,
                   BRFILE.BR_OPEN_BAL_3,
                   BRFILE.BR_OPEN_BAL_4,
                   BRFILE.BR_OPEN_BAL_5,
                   BRFILE.BR_OPEN_BAL_6,
                   BRFILE.BR_OPEN_BAL_7,
                   BRFILE.BR_OPEN_BAL_8,
                   BRFILE.BR_OPEN_BAL_9,
                   BRFILE.BR_OPEN_BAL_10,
                   BRFILE.BR_OPEN_BAL_11,
                   BRFILE.BR_OPEN_BAL_12,
                   BRFILE.BR_OPEN_BAL_13,
                   BRFILE.BR_OPEN_BAL_14,
                   BRFILE.BR_OPEN_BAL_15,
                   BRFILE.BR_OPEN_BAL_16,
                   BRFILE.BR_OPEN_BAL_17,
                   BRFILE.BR_OPEN_BAL_18,
                   BRFILE.BR_OPEN_BAL_19,
                   BRFILE.BR_OPEN_BAL_20,
                   BRFILE.BR_TENDER_CD_1,
                   BRFILE.BR_TENDER_CD_2,
                   BRFILE.BR_TENDER_CD_3,
                   BRFILE.BR_DLPARV_ADJ_FG,
                   BRFILE.BR_OVERRIDE_BR_STATE,
                   BRFILE.BR_DEFAULT_SC_CLASS,
                   BRFILE.BR_RECDEL_NOT_REC,
                   BRFILE.BR_POST_AHEAD_FG,
                   BRFILE.BR_CAN_LOANS_FG,
                   BRFILE.BR_NON_MONTHLY_CAN_LOANS,
                   BRFILE.BR_BP_ALLOW_PMT_ACCT_OTHBAL,
                   BRFILE.BR_LBOX_ALLOW_PMT_ACCT_OTHBAL,
                   BRFILE.BR_REGION_NUMBER,
                   BRFILE.BR_OUR_FEE,
                   BRFILE.BR_REB_METHOD_1,
                   BRFILE.BR_REB_METHOD_2,
                   BRFILE.BR_REB_METHOD_3,
                   BRFILE.BR_REB_METHOD_4,
                   BRFILE.BR_REB_METHOD_5,
                   BRFILE.BR_REB_METHOD_6,
                   BRFILE.BR_REB_METHOD_7,
                   BRFILE.BR_REB_METHOD_8,
                   BRFILE.BR_REB_METHOD_9,
                   BRFILE.BR_REB_METHOD_10,
                   BRFILE.BR_REB_METHOD_11,
                   BRFILE.BR_REBDAYS_MONTH_1,
                   BRFILE.BR_REBDAYS_MONTH_2,
                   BRFILE.BR_REBDAYS_MONTH_3,
                   BRFILE.BR_REBDAYS_MONTH_4,
                   BRFILE.BR_REBDAYS_MONTH_5,
                   BRFILE.BR_REBDAYS_MONTH_6,
                   BRFILE.BR_REBDAYS_MONTH_7,
                   BRFILE.BR_REBDAYS_MONTH_8,
                   BRFILE.BR_REBDAYS_MONTH_9,
                   BRFILE.BR_REBDAYS_MONTH_10,
                   BRFILE.BR_REBDAYS_MONTH_11,
                   BRFILE.BR_ACCRUAL_MONTHS_1,
                   BRFILE.BR_ACCRUAL_MONTHS_2,
                   BRFILE.BR_ACCRUAL_MONTHS_3,
                   BRFILE.BR_ACCRUAL_MONTHS_4,
                   BRFILE.BR_ACCRUAL_MONTHS_5,
                   BRFILE.BR_ACCRUAL_MONTHS_6,
                   BRFILE.BR_ACCRUAL_MONTHS_7,
                   BRFILE.BR_ACCRUAL_MONTHS_8,
                   BRFILE.BR_ACCRUAL_MONTHS_9,
                   BRFILE.BR_ACCRUAL_MONTHS_10,
                   BRFILE.BR_ACCRUAL_MONTHS_11,
                   BRFILE.BR_GLOVERSHORT,
                   BRFILE.BR_GLBANK,
                   BRFILE.BR_GLONHAND,
                   BRFILE.BR_BANKBAL,
                   BRFILE.BR_RETAIN_AMT,
                   BRFILE.BR_LCARDFG,
                   BRFILE.BR_DATA_PATH,
                   BRFILE.BR_MACHINE,
                   BRFILE.BR_MANAGER,
                   BRFILE.BR_EOD_PROCEDURE,
                   BRFILE.BR_BP_PAYOFF_NONCASH,
                   BRFILE.BR_BP_PAYOFF_OV,
                   BRFILE.BR_LI_DLCOMM_CMSI_O3_MAX,
                   BRFILE.BR_LR_GROUP_COLLID,
                   BRFILE.BR_UNITPER_FG,
                   BRFILE.BR_UNITPER_Y_D_FG,
                   BRFILE.BR_NO_PROMPT_FOR_OTHBR,
                   BRFILE.BR_WORLD_BRANCH_TYPE,
                   BRFILE.BR_COL_QUEUE_BEHAVIOR_FG,
                   BRFILE.BR_CONVERT_AUTO_ACCTNO,
                   BRFILE.BR_TS_REPO_INS_FG,
                   BRFILE.BR_BOOK_SF_BR_ONLY,
                   BRFILE.BR_FORCE_VALID_CITY_COUNTY,
                   BRFILE.BR_SET_COLLCD_DL_MFGCD,
                   BRFILE.BR_DEFAULT_SF_UNITPER_CD,
                   BRFILE.BR_DEFAULT_SF_PURCD,
                   BRFILE.BR_DEFAULT_LN_UNITPER_CD,
                   BRFILE.BR_DEFAULT_LN_PURCD,
                   BRFILE.BR_TAP_OTHER_DL,
                   BRFILE.BR_AH_AND_CL,
                   BRFILE.BR_NO_CASH_PROCEEDS,
                   BRFILE.BR_RI_BEFORE_REPO,
                   BRFILE.BR_RI_BEFORE_P4P5,
                   BRFILE.BR_NOT_OURS_REFUND,
                   BRFILE.BR_SFDLO3_WINDOW_FG,
                   BRFILE.BR_CALC_INS_ADJ,
                   BRFILE.BR_AC_BEFORE_REPO,
                   BRFILE.BR_AC_BEFORE_P4P5,
                   BRFILE.BR_BANK_ROUTING_NO,
                   BRFILE.BR_BANK_CLEARING_GL,
                   BRFILE.BR_GLBANK_DEPOSIT,
                   BRFILE.BR_BANK_ACCTNO,
                   BRFILE.BR_DEFAULT_SC_SUBCLASS,
                   BRFILE.BR_NO_OUT_DLR_RSV,
                   BRFILE.BR_COLLECTOR_QUEUE,
                   BRFILE.BR_COLL_QUEUE_CON_REC,
                   BRFILE.BR_JE_ALLOCATION_SKIP,
                   BRFILE.BR_EXPAND_APP_INPUT,
                   BRFILE.BR_A50_CPCORP,
                   BRFILE.BR_INBR_BILNSC_FG,
                   BRFILE.BR_INBR_BRIEF_FG,
                   BRFILE.BR_LEDGER_JUST_INTCHG,
                   BRFILE.BR_BYINQ_START_PAGE,
                   BRFILE.BR_DSINQ_START_PAGE,
                   BRFILE.BR_APP_REQUIRED_FOR_INPUT,
                   BRFILE.BR_CREDIT_SCORING_FG,
                   BRFILE.BR_COL_QUEUE_LOW_TO_HI,
                   BRFILE.BR_CAN_GROUP_ID,
                   BRFILE.BR_AUTO_LNSTAT_WINDOW,
                   BRFILE.BR_TIME_ZONE_FACTOR,
                   BRFILE.BR_LONPF_OPT_DISPLAY,
                   BRFILE.BR_GIFTNET,
                   BRFILE.BR_GIFTNET_LEDGER_LINE,
                   BRFILE.BR_NO_FT_EMP,
                   BRFILE.BR_NO_PT_EMP,
                   BRFILE.BR_BALLOON_LOANS_FG,
                   BRFILE.BR_GL_TYPE_ALLOWED,
                   BRFILE.BR_GL_ENTRY_DEFAULT,
                   BRFILE.BR_MIDSTREAM_1STPAY_FG,
                   BRFILE.BR_NODEF_1ST_X_MONTHS,
                   BRFILE.BR_ADD_INS_FEECODE,
                   BRFILE.BR_ROAD_G_SGL_FEECODE,
                   BRFILE.BR_ROAD_G_FAM_FEECODE,
                   BRFILE.BR_NODEF_IF_ZERO_AMT,
                   BRFILE.BR_ASSESS_LATE_CHGS,
                   BRFILE.BR_NEXT_SOLICIT_NO,
                   BRFILE.BR_ADDON_FEECD_O3_1,
                   BRFILE.BR_ADDON_FEECD_O3_2,
                   BRFILE.BR_IMMEDIATE_RECEIPT,
                   BRFILE.BR_IMMEDIATE_DOC_PRINT,
                   BRFILE.BR_AUTO_PL_REF_AC,
                   BRFILE.BR_FAX_NUMBER,
                   BRFILE.BR_ALPHA_DRAWERS,
                   BRFILE.BR_LATECHG_ON_ALLOTMENTS,
                   BRFILE.BR_ALLOW_SYS_AS_EFFDATE,
                   BRFILE.BR_TAX_PREBOOK_CUST,
                   BRFILE.BR_TAX_PREBOOK_NONCUST,
                   BRFILE.BR_PASSWORD_DRAWER,
                   BRFILE.BR_ONESTEP_RN_FG,
                   BRFILE.BR_LI_LISCAN_FG,
                   BRFILE.BR_HIDE_END_BAL,
                   BRFILE.BR_LBOX_NOPAYOFF,
                   BRFILE.BR_LBOX_ODDPAY,
                   BRFILE.BR_LBOX_LC,
                   BRFILE.BR_LBOX_NOPRIN,
                   BRFILE.BR_LBOX_FLASH,
                   BRFILE.BR_LBOX_ACTIONCD,
                   BRFILE.BR_LBOX_MAXCON,
                   BRFILE.BR_LBOX_MAXREC,
                   BRFILE.BR_LBOX_PL,
                   BRFILE.BR_LBOX_JD,
                   BRFILE.BR_LBOX,
                   BRFILE.BR_LATECHG_ON_LBOX,
                   BRFILE.BR_ALLOW_OV_TRCD,
                   BRFILE.BR_GLOBAL_QUEUE,
                   BRFILE.BR_LBOX_PAYOFF_NONCASH,
                   BRFILE.BR_LBOX_PAYOFF_OV,
                   BRFILE.BR_COLLECT_MANAGER,
                   BRFILE.BR_COLLECT_PHONE,
                   BRFILE.BR_COLLECT_EXT,
                   BRFILE.BR_ALT_INTO_THIS_BRANCH,
                   BRFILE.BR_POFF_TITLE_WINDOW,
                   BRFILE.BR_COUPON_STATEMENT,
                   BRFILE.BR_SUPERVISOR,
                   BRFILE.BR_AREA_MANAGER,
                   BRFILE.BR_MIN_MONTHS_DEF,
                   BRFILE.BR_CC_FLAG,
                   BRFILE.BR_CC_ADR1,
                   BRFILE.BR_CC_CITY,
                   BRFILE.BR_CC_STATE,
                   BRFILE.BR_CC_ZIP,
                   BRFILE.BR_MIN_MONTHS_D2,
                   BRFILE.BR_SKIP_MIDSTREAM_TRW,
                   BRFILE.BR_GL_ESCROW_BALANCE,
                   BRFILE.BR_GL_ESCROW_PAYABLE,
                   BRFILE.BR_ALLOW_FORCE_OPEN_FG,
                   BRFILE.BR_ACH_BANK_NO,
                   BRFILE.BR_WIMENU_CHECK_WRITING,
                   BRFILE.BR_ADP_PAY_GROUP,
                   BRFILE.BR_ALLOW_SCANNED_CKS,
                   BRFILE.BR_CHECK_DEPOSIT_GL,
                   BRFILE.BR_OPTION_PST_LASER,
                   BRFILE.BR_USE_DEBIT_CARDS,
                   BRFILE.BR_GL_DEBIT_CARD_PROCEEDS,
                   BRFILE.BR_WKDAY_BANKID,
                   BRFILE.BR_PURGE_NOW_FLAG,
                   BRFILE.BR_EMAIL,
                   BRFILE.BR_MANAGER_EMAIL,
                   BRFILE.BR_VERITEC_LICENSE,
                   BRFILE.BR_TEST_PP_AUTO_INSCOVR,
                   BRFILE.BR_USE_NEW_WFARGO_ACCT,
                   BRFILE.BR_BRANCH_CLOSED,
                   BRFILE.BR_ALLOW_F3_ALL_DOCS
            FROM DBO.BRFILE
            WHERE BRFILE.BR_NO >= :QBR-WBEG-NO
              AND BRFILE.BR_NO <= :QBR-WEND-NO
            ORDER BY BR_NO
           END-EXEC.

           EXEC SQL
               OPEN BR_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QBR-WBEG-NO
                          QBR-WEND-NO.

           MOVE STAT---GOOD TO BR-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-BR-FILE.
           PERFORM READ-IT.
           MOVE BR-PATH-SQL TO E-FILE.
           MOVE BR-NO TO E-KEYX.

           IF ( EXT-TCLP-TRACE-BRANCH )
              STRING "READ-BR-FILE | ", BR-NO
                     DELIMITED BY SIZE INTO EXT-TCLP-TRACE-MESS
              CALL "TRACE" USING FORM-PATHNAME
              CANCEL "TRACE".

           INITIALIZE QBR-REC.

           MOVE BR-NO TO QBR-NO.

           EXEC SQL
            SELECT BRFILE.BR_NO,
                   CAST(BRFILE.BR_LTOUCH_DATE AS VARCHAR(10)),
                   BRFILE.BR_NAME,
                   BRFILE.BR_ADR1,
                   BRFILE.BR_ADR2,
                   BRFILE.BR_CITY,
                   BRFILE.BR_STATE,
                   BRFILE.BR_ZIP,
                   BRFILE.BR_PHONE,
                   BRFILE.BR_RETAIN,
                   BRFILE.BR_NEXT_CHECK,
                   BRFILE.BR_PRINTER_1,
                   BRFILE.BR_PRINTER_2,
                   BRFILE.BR_PRINTER_3,
                   BRFILE.BR_PRINTER_4,
                   BRFILE.BR_FEE_GL_BRNO,
                   BRFILE.BR_REMIT1,
                   BRFILE.BR_REMIT2,
                   BRFILE.BR_REMIT3,
                   BRFILE.BR_STMNT1,
                   BRFILE.BR_STMNT2,
                   BRFILE.BR_INS_PRODUCER_NUM,
                   BRFILE.BR_DLRCHK,
                   BRFILE.BR_FRONT_END_FG,
                   BRFILE.BR_EMLNRG_PHONE,
                   BRFILE.BR_POST_GL,
                   BRFILE.BR_ALLOTMNTS,
                   BRFILE.BR_EXTRACT_AUTO_UPD,
                   BRFILE.BR_PCNI_LEDGER,
                   BRFILE.BR_PRINTERFG_1,
                   BRFILE.BR_PRINTERFG_2,
                   BRFILE.BR_PRINTERFG_3,
                   BRFILE.BR_PRINTERFG_4,
                   BRFILE.BR_NEGDLRSV,
                   BRFILE.BR_CPCORP_ORDER,
                   BRFILE.BR_PRIO_MEMO_FLAG,
                   BRFILE.BR_PREBILL_PL_JD_OPT,
                   BRFILE.BR_LOAN_NUM_FLAG,
                   BRFILE.BR_DOWNPY_FLAG,
                   BRFILE.BR_AUTO_INS_EARN,
                   BRFILE.BR_OTH_INS_LEVELS,
                   BRFILE.BR_ALLOW_INTERBR,
                   BRFILE.BR_UNER_DLANRP,
                   BRFILE.BR_SECOND_PROMISE,
                   BRFILE.BR_AVAIL_CREDIT,
                   BRFILE.BR_RECP1,
                   BRFILE.BR_RECP2,
                   BRFILE.BR_INC_WAIVERS,
                   BRFILE.BR_INC_INS,
                   BRFILE.BR_BP_NEXT_BATCH,
                   BRFILE.BR_BP_NOPAYOFF,
                   BRFILE.BR_BP_ODDPAY,
                   BRFILE.BR_BP_LC,
                   BRFILE.BR_BP_NOPRIN,
                   BRFILE.BR_BP_FLASH,
                   BRFILE.BR_BP_ACTIONCD,
                   BRFILE.BR_BP_MAXCON,
                   BRFILE.BR_BP_MAXREC,
                   BRFILE.BR_BP_PL,
                   BRFILE.BR_BP_JD,
                   BRFILE.BR_BP_BRANCH,
                   BRFILE.BR_AGE_ERN,
                   BRFILE.BR_OTHER_BRANCH_GL,
                   BRFILE.BR_SALES_SMALL,
                   BRFILE.BR_NEW_COLL_INQUIRY,
                   BRFILE.BR_CHANGE_MEMO,
                   BRFILE.BR_INS_ADDON,
                   BRFILE.BR_TRAILER_REC,
                   BRFILE.BR_ALDEL_FLAG,
                   BRFILE.BR_IB_LCHG_MESS,
                   BRFILE.BR_COL_QUEUE_PAST_DUE,
                   BRFILE.BR_LI_DLCOMM_CMSI_PP_MAX,
                   BRFILE.BR_ADDON_FEECD_O2_1,
                   BRFILE.BR_ADDON_FEECD_O2_2,
                   BRFILE.BR_AUTO_CHECK,
                   BRFILE.BR_RECEIPT_PROMDATE,
                   BRFILE.BR_RECEIPT_INTPD,
                   BRFILE.BR_COLL_ID_SCAN,
                   BRFILE.BR_STOP_ON_COLL,
                   BRFILE.BR_DUEDATE_CHANGE_CD,
                   BRFILE.BR_CHECK_EOM_FG,
                   BRFILE.BR_VOLUME_DLANRP,
                   BRFILE.BR_PDTH_NXDUE,
                   BRFILE.BR_LC_CODE,
                   BRFILE.BR_VALIDATE_PRUS,
                   BRFILE.BR_AUTO_OPEN_BAL,
                   BRFILE.BR_OPEN_BAL_1,
                   BRFILE.BR_OPEN_BAL_2,
                   BRFILE.BR_OPEN_BAL_3,
                   BRFILE.BR_OPEN_BAL_4,
                   BRFILE.BR_OPEN_BAL_5,
                   BRFILE.BR_OPEN_BAL_6,
                   BRFILE.BR_OPEN_BAL_7,
                   BRFILE.BR_OPEN_BAL_8,
                   BRFILE.BR_OPEN_BAL_9,
                   BRFILE.BR_OPEN_BAL_10,
                   BRFILE.BR_OPEN_BAL_11,
                   BRFILE.BR_OPEN_BAL_12,
                   BRFILE.BR_OPEN_BAL_13,
                   BRFILE.BR_OPEN_BAL_14,
                   BRFILE.BR_OPEN_BAL_15,
                   BRFILE.BR_OPEN_BAL_16,
                   BRFILE.BR_OPEN_BAL_17,
                   BRFILE.BR_OPEN_BAL_18,
                   BRFILE.BR_OPEN_BAL_19,
                   BRFILE.BR_OPEN_BAL_20,
                   BRFILE.BR_TENDER_CD_1,
                   BRFILE.BR_TENDER_CD_2,
                   BRFILE.BR_TENDER_CD_3,
                   BRFILE.BR_DLPARV_ADJ_FG,
                   BRFILE.BR_OVERRIDE_BR_STATE,
                   BRFILE.BR_DEFAULT_SC_CLASS,
                   BRFILE.BR_RECDEL_NOT_REC,
                   BRFILE.BR_POST_AHEAD_FG,
                   BRFILE.BR_CAN_LOANS_FG,
                   BRFILE.BR_NON_MONTHLY_CAN_LOANS,
                   BRFILE.BR_BP_ALLOW_PMT_ACCT_OTHBAL,
                   BRFILE.BR_LBOX_ALLOW_PMT_ACCT_OTHBAL,
                   BRFILE.BR_REGION_NUMBER,
                   BRFILE.BR_OUR_FEE,
                   BRFILE.BR_REB_METHOD_1,
                   BRFILE.BR_REB_METHOD_2,
                   BRFILE.BR_REB_METHOD_3,
                   BRFILE.BR_REB_METHOD_4,
                   BRFILE.BR_REB_METHOD_5,
                   BRFILE.BR_REB_METHOD_6,
                   BRFILE.BR_REB_METHOD_7,
                   BRFILE.BR_REB_METHOD_8,
                   BRFILE.BR_REB_METHOD_9,
                   BRFILE.BR_REB_METHOD_10,
                   BRFILE.BR_REB_METHOD_11,
                   BRFILE.BR_REBDAYS_MONTH_1,
                   BRFILE.BR_REBDAYS_MONTH_2,
                   BRFILE.BR_REBDAYS_MONTH_3,
                   BRFILE.BR_REBDAYS_MONTH_4,
                   BRFILE.BR_REBDAYS_MONTH_5,
                   BRFILE.BR_REBDAYS_MONTH_6,
                   BRFILE.BR_REBDAYS_MONTH_7,
                   BRFILE.BR_REBDAYS_MONTH_8,
                   BRFILE.BR_REBDAYS_MONTH_9,
                   BRFILE.BR_REBDAYS_MONTH_10,
                   BRFILE.BR_REBDAYS_MONTH_11,
                   BRFILE.BR_ACCRUAL_MONTHS_1,
                   BRFILE.BR_ACCRUAL_MONTHS_2,
                   BRFILE.BR_ACCRUAL_MONTHS_3,
                   BRFILE.BR_ACCRUAL_MONTHS_4,
                   BRFILE.BR_ACCRUAL_MONTHS_5,
                   BRFILE.BR_ACCRUAL_MONTHS_6,
                   BRFILE.BR_ACCRUAL_MONTHS_7,
                   BRFILE.BR_ACCRUAL_MONTHS_8,
                   BRFILE.BR_ACCRUAL_MONTHS_9,
                   BRFILE.BR_ACCRUAL_MONTHS_10,
                   BRFILE.BR_ACCRUAL_MONTHS_11,
                   BRFILE.BR_GLOVERSHORT,
                   BRFILE.BR_GLBANK,
                   BRFILE.BR_GLONHAND,
                   BRFILE.BR_BANKBAL,
                   BRFILE.BR_RETAIN_AMT,
                   BRFILE.BR_LCARDFG,
                   BRFILE.BR_DATA_PATH,
                   BRFILE.BR_MACHINE,
                   BRFILE.BR_MANAGER,
                   BRFILE.BR_EOD_PROCEDURE,
                   BRFILE.BR_BP_PAYOFF_NONCASH,
                   BRFILE.BR_BP_PAYOFF_OV,
                   BRFILE.BR_LI_DLCOMM_CMSI_O3_MAX,
                   BRFILE.BR_LR_GROUP_COLLID,
                   BRFILE.BR_UNITPER_FG,
                   BRFILE.BR_UNITPER_Y_D_FG,
                   BRFILE.BR_NO_PROMPT_FOR_OTHBR,
                   BRFILE.BR_WORLD_BRANCH_TYPE,
                   BRFILE.BR_COL_QUEUE_BEHAVIOR_FG,
                   BRFILE.BR_CONVERT_AUTO_ACCTNO,
                   BRFILE.BR_TS_REPO_INS_FG,
                   BRFILE.BR_BOOK_SF_BR_ONLY,
                   BRFILE.BR_FORCE_VALID_CITY_COUNTY,
                   BRFILE.BR_SET_COLLCD_DL_MFGCD,
                   BRFILE.BR_DEFAULT_SF_UNITPER_CD,
                   BRFILE.BR_DEFAULT_SF_PURCD,
                   BRFILE.BR_DEFAULT_LN_UNITPER_CD,
                   BRFILE.BR_DEFAULT_LN_PURCD,
                   BRFILE.BR_TAP_OTHER_DL,
                   BRFILE.BR_AH_AND_CL,
                   BRFILE.BR_NO_CASH_PROCEEDS,
                   BRFILE.BR_RI_BEFORE_REPO,
                   BRFILE.BR_RI_BEFORE_P4P5,
                   BRFILE.BR_NOT_OURS_REFUND,
                   BRFILE.BR_SFDLO3_WINDOW_FG,
                   BRFILE.BR_CALC_INS_ADJ,
                   BRFILE.BR_AC_BEFORE_REPO,
                   BRFILE.BR_AC_BEFORE_P4P5,
                   BRFILE.BR_BANK_ROUTING_NO,
                   BRFILE.BR_BANK_CLEARING_GL,
                   BRFILE.BR_GLBANK_DEPOSIT,
                   BRFILE.BR_BANK_ACCTNO,
                   BRFILE.BR_DEFAULT_SC_SUBCLASS,
                   BRFILE.BR_NO_OUT_DLR_RSV,
                   BRFILE.BR_COLLECTOR_QUEUE,
                   BRFILE.BR_COLL_QUEUE_CON_REC,
                   BRFILE.BR_JE_ALLOCATION_SKIP,
                   BRFILE.BR_EXPAND_APP_INPUT,
                   BRFILE.BR_A50_CPCORP,
                   BRFILE.BR_INBR_BILNSC_FG,
                   BRFILE.BR_INBR_BRIEF_FG,
                   BRFILE.BR_LEDGER_JUST_INTCHG,
                   BRFILE.BR_BYINQ_START_PAGE,
                   BRFILE.BR_DSINQ_START_PAGE,
                   BRFILE.BR_APP_REQUIRED_FOR_INPUT,
                   BRFILE.BR_CREDIT_SCORING_FG,
                   BRFILE.BR_COL_QUEUE_LOW_TO_HI,
                   BRFILE.BR_CAN_GROUP_ID,
                   BRFILE.BR_AUTO_LNSTAT_WINDOW,
                   BRFILE.BR_TIME_ZONE_FACTOR,
                   BRFILE.BR_LONPF_OPT_DISPLAY,
                   BRFILE.BR_GIFTNET,
                   BRFILE.BR_GIFTNET_LEDGER_LINE,
                   BRFILE.BR_NO_FT_EMP,
                   BRFILE.BR_NO_PT_EMP,
                   BRFILE.BR_BALLOON_LOANS_FG,
                   BRFILE.BR_GL_TYPE_ALLOWED,
                   BRFILE.BR_GL_ENTRY_DEFAULT,
                   BRFILE.BR_MIDSTREAM_1STPAY_FG,
                   BRFILE.BR_NODEF_1ST_X_MONTHS,
                   BRFILE.BR_ADD_INS_FEECODE,
                   BRFILE.BR_ROAD_G_SGL_FEECODE,
                   BRFILE.BR_ROAD_G_FAM_FEECODE,
                   BRFILE.BR_NODEF_IF_ZERO_AMT,
                   BRFILE.BR_ASSESS_LATE_CHGS,
                   BRFILE.BR_NEXT_SOLICIT_NO,
                   BRFILE.BR_ADDON_FEECD_O3_1,
                   BRFILE.BR_ADDON_FEECD_O3_2,
                   BRFILE.BR_IMMEDIATE_RECEIPT,
                   BRFILE.BR_IMMEDIATE_DOC_PRINT,
                   BRFILE.BR_AUTO_PL_REF_AC,
                   BRFILE.BR_FAX_NUMBER,
                   BRFILE.BR_ALPHA_DRAWERS,
                   BRFILE.BR_LATECHG_ON_ALLOTMENTS,
                   BRFILE.BR_ALLOW_SYS_AS_EFFDATE,
                   BRFILE.BR_TAX_PREBOOK_CUST,
                   BRFILE.BR_TAX_PREBOOK_NONCUST,
                   BRFILE.BR_PASSWORD_DRAWER,
                   BRFILE.BR_ONESTEP_RN_FG,
                   BRFILE.BR_LI_LISCAN_FG,
                   BRFILE.BR_HIDE_END_BAL,
                   BRFILE.BR_LBOX_NOPAYOFF,
                   BRFILE.BR_LBOX_ODDPAY,
                   BRFILE.BR_LBOX_LC,
                   BRFILE.BR_LBOX_NOPRIN,
                   BRFILE.BR_LBOX_FLASH,
                   BRFILE.BR_LBOX_ACTIONCD,
                   BRFILE.BR_LBOX_MAXCON,
                   BRFILE.BR_LBOX_MAXREC,
                   BRFILE.BR_LBOX_PL,
                   BRFILE.BR_LBOX_JD,
                   BRFILE.BR_LBOX,
                   BRFILE.BR_LATECHG_ON_LBOX,
                   BRFILE.BR_ALLOW_OV_TRCD,
                   BRFILE.BR_GLOBAL_QUEUE,
                   BRFILE.BR_LBOX_PAYOFF_NONCASH,
                   BRFILE.BR_LBOX_PAYOFF_OV,
                   BRFILE.BR_COLLECT_MANAGER,
                   BRFILE.BR_COLLECT_PHONE,
                   BRFILE.BR_COLLECT_EXT,
                   BRFILE.BR_ALT_INTO_THIS_BRANCH,
                   BRFILE.BR_POFF_TITLE_WINDOW,
                   BRFILE.BR_COUPON_STATEMENT,
                   BRFILE.BR_SUPERVISOR,
                   BRFILE.BR_AREA_MANAGER,
                   BRFILE.BR_MIN_MONTHS_DEF,
                   BRFILE.BR_CC_FLAG,
                   BRFILE.BR_CC_ADR1,
                   BRFILE.BR_CC_CITY,
                   BRFILE.BR_CC_STATE,
                   BRFILE.BR_CC_ZIP,
                   BRFILE.BR_MIN_MONTHS_D2,
                   BRFILE.BR_SKIP_MIDSTREAM_TRW,
                   BRFILE.BR_GL_ESCROW_BALANCE,
                   BRFILE.BR_GL_ESCROW_PAYABLE,
                   BRFILE.BR_ALLOW_FORCE_OPEN_FG,
                   BRFILE.BR_ACH_BANK_NO,
                   BRFILE.BR_WIMENU_CHECK_WRITING,
                   BRFILE.BR_ADP_PAY_GROUP,
                   BRFILE.BR_ALLOW_SCANNED_CKS,
                   BRFILE.BR_CHECK_DEPOSIT_GL,
                   BRFILE.BR_OPTION_PST_LASER,
                   BRFILE.BR_USE_DEBIT_CARDS,
                   BRFILE.BR_GL_DEBIT_CARD_PROCEEDS,
                   BRFILE.BR_WKDAY_BANKID,
                   BRFILE.BR_PURGE_NOW_FLAG,
                   BRFILE.BR_EMAIL,
                   BRFILE.BR_MANAGER_EMAIL,
                   BRFILE.BR_VERITEC_LICENSE,
                   BRFILE.BR_TEST_PP_AUTO_INSCOVR,
                   BRFILE.BR_USE_NEW_WFARGO_ACCT,
                   BRFILE.BR_BRANCH_CLOSED,
                   BRFILE.BR_ALLOW_F3_ALL_DOCS
            INTO :QBR-REC
            FROM DBO.BRFILE
            WHERE BRFILE.BR_NO = :QBR-NO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO READ-BR-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-BR-FIELDS.

      *-----------------------------------------------------------------
       READ-BR-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BR-PATH-SQL TO E-FILE.
           MOVE BR-NO TO E-KEYX.

           IF ( EXT-TCLP-TRACE-BRANCH )
              STRING "READ-BR-FILE-NEXT | ", BR-NO
                     DELIMITED BY SIZE INTO EXT-TCLP-TRACE-MESS
              CALL "TRACE" USING FORM-PATHNAME
              CANCEL "TRACE".

           INITIALIZE QBR-REC.

           IF ( BR-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BR_CURSOR INTO :QBR-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO READ-BR-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BR-FIELDS.

      *-----------------------------------------------------------------
       WRITE-BR-FILE.

           ACCEPT BR-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE BR-PATH-SQL TO E-FILE.
           MOVE BR-NO TO E-KEYX.
           PERFORM SET-BR-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            INSERT INTO DBO.BRFILE
             (BRFILE.BR_NO,
              BRFILE.BR_LTOUCH_DATE,
              BRFILE.BR_NAME,
              BRFILE.BR_ADR1,
              BRFILE.BR_ADR2,
              BRFILE.BR_CITY,
              BRFILE.BR_STATE,
              BRFILE.BR_ZIP,
              BRFILE.BR_PHONE,
              BRFILE.BR_RETAIN,
              BRFILE.BR_NEXT_CHECK,
              BRFILE.BR_PRINTER_1,
              BRFILE.BR_PRINTER_2,
              BRFILE.BR_PRINTER_3,
              BRFILE.BR_PRINTER_4,
              BRFILE.BR_FEE_GL_BRNO,
              BRFILE.BR_REMIT1,
              BRFILE.BR_REMIT2,
              BRFILE.BR_REMIT3,
              BRFILE.BR_STMNT1,
              BRFILE.BR_STMNT2,
              BRFILE.BR_INS_PRODUCER_NUM,
              BRFILE.BR_DLRCHK,
              BRFILE.BR_FRONT_END_FG,
              BRFILE.BR_EMLNRG_PHONE,
              BRFILE.BR_POST_GL,
              BRFILE.BR_ALLOTMNTS,
              BRFILE.BR_EXTRACT_AUTO_UPD,
              BRFILE.BR_PCNI_LEDGER,
              BRFILE.BR_PRINTERFG_1,
              BRFILE.BR_PRINTERFG_2,
              BRFILE.BR_PRINTERFG_3,
              BRFILE.BR_PRINTERFG_4,
              BRFILE.BR_NEGDLRSV,
              BRFILE.BR_CPCORP_ORDER,
              BRFILE.BR_PRIO_MEMO_FLAG,
              BRFILE.BR_PREBILL_PL_JD_OPT,
              BRFILE.BR_LOAN_NUM_FLAG,
              BRFILE.BR_DOWNPY_FLAG,
              BRFILE.BR_AUTO_INS_EARN,
              BRFILE.BR_OTH_INS_LEVELS,
              BRFILE.BR_ALLOW_INTERBR,
              BRFILE.BR_UNER_DLANRP,
              BRFILE.BR_SECOND_PROMISE,
              BRFILE.BR_AVAIL_CREDIT,
              BRFILE.BR_RECP1,
              BRFILE.BR_RECP2,
              BRFILE.BR_INC_WAIVERS,
              BRFILE.BR_INC_INS,
              BRFILE.BR_BP_NEXT_BATCH,
              BRFILE.BR_BP_NOPAYOFF,
              BRFILE.BR_BP_ODDPAY,
              BRFILE.BR_BP_LC,
              BRFILE.BR_BP_NOPRIN,
              BRFILE.BR_BP_FLASH,
              BRFILE.BR_BP_ACTIONCD,
              BRFILE.BR_BP_MAXCON,
              BRFILE.BR_BP_MAXREC,
              BRFILE.BR_BP_PL,
              BRFILE.BR_BP_JD,
              BRFILE.BR_BP_BRANCH,
              BRFILE.BR_AGE_ERN,
              BRFILE.BR_OTHER_BRANCH_GL,
              BRFILE.BR_SALES_SMALL,
              BRFILE.BR_NEW_COLL_INQUIRY,
              BRFILE.BR_CHANGE_MEMO,
              BRFILE.BR_INS_ADDON,
              BRFILE.BR_TRAILER_REC,
              BRFILE.BR_ALDEL_FLAG,
              BRFILE.BR_IB_LCHG_MESS,
              BRFILE.BR_COL_QUEUE_PAST_DUE,
              BRFILE.BR_LI_DLCOMM_CMSI_PP_MAX,
              BRFILE.BR_ADDON_FEECD_O2_1,
              BRFILE.BR_ADDON_FEECD_O2_2,
              BRFILE.BR_AUTO_CHECK,
              BRFILE.BR_RECEIPT_PROMDATE,
              BRFILE.BR_RECEIPT_INTPD,
              BRFILE.BR_COLL_ID_SCAN,
              BRFILE.BR_STOP_ON_COLL,
              BRFILE.BR_DUEDATE_CHANGE_CD,
              BRFILE.BR_CHECK_EOM_FG,
              BRFILE.BR_VOLUME_DLANRP,
              BRFILE.BR_PDTH_NXDUE,
              BRFILE.BR_LC_CODE,
              BRFILE.BR_VALIDATE_PRUS,
              BRFILE.BR_AUTO_OPEN_BAL,
              BRFILE.BR_OPEN_BAL_1,
              BRFILE.BR_OPEN_BAL_2,
              BRFILE.BR_OPEN_BAL_3,
              BRFILE.BR_OPEN_BAL_4,
              BRFILE.BR_OPEN_BAL_5,
              BRFILE.BR_OPEN_BAL_6,
              BRFILE.BR_OPEN_BAL_7,
              BRFILE.BR_OPEN_BAL_8,
              BRFILE.BR_OPEN_BAL_9,
              BRFILE.BR_OPEN_BAL_10,
              BRFILE.BR_OPEN_BAL_11,
              BRFILE.BR_OPEN_BAL_12,
              BRFILE.BR_OPEN_BAL_13,
              BRFILE.BR_OPEN_BAL_14,
              BRFILE.BR_OPEN_BAL_15,
              BRFILE.BR_OPEN_BAL_16,
              BRFILE.BR_OPEN_BAL_17,
              BRFILE.BR_OPEN_BAL_18,
              BRFILE.BR_OPEN_BAL_19,
              BRFILE.BR_OPEN_BAL_20,
              BRFILE.BR_TENDER_CD_1,
              BRFILE.BR_TENDER_CD_2,
              BRFILE.BR_TENDER_CD_3,
              BRFILE.BR_DLPARV_ADJ_FG,
              BRFILE.BR_OVERRIDE_BR_STATE,
              BRFILE.BR_DEFAULT_SC_CLASS,
              BRFILE.BR_RECDEL_NOT_REC,
              BRFILE.BR_POST_AHEAD_FG,
              BRFILE.BR_CAN_LOANS_FG,
              BRFILE.BR_NON_MONTHLY_CAN_LOANS,
              BRFILE.BR_BP_ALLOW_PMT_ACCT_OTHBAL,
              BRFILE.BR_LBOX_ALLOW_PMT_ACCT_OTHBAL,
              BRFILE.BR_REGION_NUMBER,
              BRFILE.BR_OUR_FEE,
              BRFILE.BR_REB_METHOD_1,
              BRFILE.BR_REB_METHOD_2,
              BRFILE.BR_REB_METHOD_3,
              BRFILE.BR_REB_METHOD_4,
              BRFILE.BR_REB_METHOD_5,
              BRFILE.BR_REB_METHOD_6,
              BRFILE.BR_REB_METHOD_7,
              BRFILE.BR_REB_METHOD_8,
              BRFILE.BR_REB_METHOD_9,
              BRFILE.BR_REB_METHOD_10,
              BRFILE.BR_REB_METHOD_11,
              BRFILE.BR_REBDAYS_MONTH_1,
              BRFILE.BR_REBDAYS_MONTH_2,
              BRFILE.BR_REBDAYS_MONTH_3,
              BRFILE.BR_REBDAYS_MONTH_4,
              BRFILE.BR_REBDAYS_MONTH_5,
              BRFILE.BR_REBDAYS_MONTH_6,
              BRFILE.BR_REBDAYS_MONTH_7,
              BRFILE.BR_REBDAYS_MONTH_8,
              BRFILE.BR_REBDAYS_MONTH_9,
              BRFILE.BR_REBDAYS_MONTH_10,
              BRFILE.BR_REBDAYS_MONTH_11,
              BRFILE.BR_ACCRUAL_MONTHS_1,
              BRFILE.BR_ACCRUAL_MONTHS_2,
              BRFILE.BR_ACCRUAL_MONTHS_3,
              BRFILE.BR_ACCRUAL_MONTHS_4,
              BRFILE.BR_ACCRUAL_MONTHS_5,
              BRFILE.BR_ACCRUAL_MONTHS_6,
              BRFILE.BR_ACCRUAL_MONTHS_7,
              BRFILE.BR_ACCRUAL_MONTHS_8,
              BRFILE.BR_ACCRUAL_MONTHS_9,
              BRFILE.BR_ACCRUAL_MONTHS_10,
              BRFILE.BR_ACCRUAL_MONTHS_11,
              BRFILE.BR_GLOVERSHORT,
              BRFILE.BR_GLBANK,
              BRFILE.BR_GLONHAND,
              BRFILE.BR_BANKBAL,
              BRFILE.BR_RETAIN_AMT,
              BRFILE.BR_LCARDFG,
              BRFILE.BR_DATA_PATH,
              BRFILE.BR_MACHINE,
              BRFILE.BR_MANAGER,
              BRFILE.BR_EOD_PROCEDURE,
              BRFILE.BR_BP_PAYOFF_NONCASH,
              BRFILE.BR_BP_PAYOFF_OV,
              BRFILE.BR_LI_DLCOMM_CMSI_O3_MAX,
              BRFILE.BR_LR_GROUP_COLLID,
              BRFILE.BR_UNITPER_FG,
              BRFILE.BR_UNITPER_Y_D_FG,
              BRFILE.BR_NO_PROMPT_FOR_OTHBR,
              BRFILE.BR_WORLD_BRANCH_TYPE,
              BRFILE.BR_COL_QUEUE_BEHAVIOR_FG,
              BRFILE.BR_CONVERT_AUTO_ACCTNO,
              BRFILE.BR_TS_REPO_INS_FG,
              BRFILE.BR_BOOK_SF_BR_ONLY,
              BRFILE.BR_FORCE_VALID_CITY_COUNTY,
              BRFILE.BR_SET_COLLCD_DL_MFGCD,
              BRFILE.BR_DEFAULT_SF_UNITPER_CD,
              BRFILE.BR_DEFAULT_SF_PURCD,
              BRFILE.BR_DEFAULT_LN_UNITPER_CD,
              BRFILE.BR_DEFAULT_LN_PURCD,
              BRFILE.BR_TAP_OTHER_DL,
              BRFILE.BR_AH_AND_CL,
              BRFILE.BR_NO_CASH_PROCEEDS,
              BRFILE.BR_RI_BEFORE_REPO,
              BRFILE.BR_RI_BEFORE_P4P5,
              BRFILE.BR_NOT_OURS_REFUND,
              BRFILE.BR_SFDLO3_WINDOW_FG,
              BRFILE.BR_CALC_INS_ADJ,
              BRFILE.BR_AC_BEFORE_REPO,
              BRFILE.BR_AC_BEFORE_P4P5,
              BRFILE.BR_BANK_ROUTING_NO,
              BRFILE.BR_BANK_CLEARING_GL,
              BRFILE.BR_GLBANK_DEPOSIT,
              BRFILE.BR_BANK_ACCTNO,
              BRFILE.BR_DEFAULT_SC_SUBCLASS,
              BRFILE.BR_NO_OUT_DLR_RSV,
              BRFILE.BR_COLLECTOR_QUEUE,
              BRFILE.BR_COLL_QUEUE_CON_REC,
              BRFILE.BR_JE_ALLOCATION_SKIP,
              BRFILE.BR_EXPAND_APP_INPUT,
              BRFILE.BR_A50_CPCORP,
              BRFILE.BR_INBR_BILNSC_FG,
              BRFILE.BR_INBR_BRIEF_FG,
              BRFILE.BR_LEDGER_JUST_INTCHG,
              BRFILE.BR_BYINQ_START_PAGE,
              BRFILE.BR_DSINQ_START_PAGE,
              BRFILE.BR_APP_REQUIRED_FOR_INPUT,
              BRFILE.BR_CREDIT_SCORING_FG,
              BRFILE.BR_COL_QUEUE_LOW_TO_HI,
              BRFILE.BR_CAN_GROUP_ID,
              BRFILE.BR_AUTO_LNSTAT_WINDOW,
              BRFILE.BR_TIME_ZONE_FACTOR,
              BRFILE.BR_LONPF_OPT_DISPLAY,
              BRFILE.BR_GIFTNET,
              BRFILE.BR_GIFTNET_LEDGER_LINE,
              BRFILE.BR_NO_FT_EMP,
              BRFILE.BR_NO_PT_EMP,
              BRFILE.BR_BALLOON_LOANS_FG,
              BRFILE.BR_GL_TYPE_ALLOWED,
              BRFILE.BR_GL_ENTRY_DEFAULT,
              BRFILE.BR_MIDSTREAM_1STPAY_FG,
              BRFILE.BR_NODEF_1ST_X_MONTHS,
              BRFILE.BR_ADD_INS_FEECODE,
              BRFILE.BR_ROAD_G_SGL_FEECODE,
              BRFILE.BR_ROAD_G_FAM_FEECODE,
              BRFILE.BR_NODEF_IF_ZERO_AMT,
              BRFILE.BR_ASSESS_LATE_CHGS,
              BRFILE.BR_NEXT_SOLICIT_NO,
              BRFILE.BR_ADDON_FEECD_O3_1,
              BRFILE.BR_ADDON_FEECD_O3_2,
              BRFILE.BR_IMMEDIATE_RECEIPT,
              BRFILE.BR_IMMEDIATE_DOC_PRINT,
              BRFILE.BR_AUTO_PL_REF_AC,
              BRFILE.BR_FAX_NUMBER,
              BRFILE.BR_ALPHA_DRAWERS,
              BRFILE.BR_LATECHG_ON_ALLOTMENTS,
              BRFILE.BR_ALLOW_SYS_AS_EFFDATE,
              BRFILE.BR_TAX_PREBOOK_CUST,
              BRFILE.BR_TAX_PREBOOK_NONCUST,
              BRFILE.BR_PASSWORD_DRAWER,
              BRFILE.BR_ONESTEP_RN_FG,
              BRFILE.BR_LI_LISCAN_FG,
              BRFILE.BR_HIDE_END_BAL,
              BRFILE.BR_LBOX_NOPAYOFF,
              BRFILE.BR_LBOX_ODDPAY,
              BRFILE.BR_LBOX_LC,
              BRFILE.BR_LBOX_NOPRIN,
              BRFILE.BR_LBOX_FLASH,
              BRFILE.BR_LBOX_ACTIONCD,
              BRFILE.BR_LBOX_MAXCON,
              BRFILE.BR_LBOX_MAXREC,
              BRFILE.BR_LBOX_PL,
              BRFILE.BR_LBOX_JD,
              BRFILE.BR_LBOX,
              BRFILE.BR_LATECHG_ON_LBOX,
              BRFILE.BR_ALLOW_OV_TRCD,
              BRFILE.BR_GLOBAL_QUEUE,
              BRFILE.BR_LBOX_PAYOFF_NONCASH,
              BRFILE.BR_LBOX_PAYOFF_OV,
              BRFILE.BR_COLLECT_MANAGER,
              BRFILE.BR_COLLECT_PHONE,
              BRFILE.BR_COLLECT_EXT,
              BRFILE.BR_ALT_INTO_THIS_BRANCH,
              BRFILE.BR_POFF_TITLE_WINDOW,
              BRFILE.BR_COUPON_STATEMENT,
              BRFILE.BR_SUPERVISOR,
              BRFILE.BR_AREA_MANAGER,
              BRFILE.BR_MIN_MONTHS_DEF,
              BRFILE.BR_CC_FLAG,
              BRFILE.BR_CC_ADR1,
              BRFILE.BR_CC_CITY,
              BRFILE.BR_CC_STATE,
              BRFILE.BR_CC_ZIP,
              BRFILE.BR_MIN_MONTHS_D2,
              BRFILE.BR_SKIP_MIDSTREAM_TRW,
              BRFILE.BR_GL_ESCROW_BALANCE,
              BRFILE.BR_GL_ESCROW_PAYABLE,
              BRFILE.BR_ALLOW_FORCE_OPEN_FG,
              BRFILE.BR_ACH_BANK_NO,
              BRFILE.BR_WIMENU_CHECK_WRITING,
              BRFILE.BR_ADP_PAY_GROUP,
              BRFILE.BR_ALLOW_SCANNED_CKS,
              BRFILE.BR_CHECK_DEPOSIT_GL,
              BRFILE.BR_OPTION_PST_LASER,
              BRFILE.BR_USE_DEBIT_CARDS,
              BRFILE.BR_GL_DEBIT_CARD_PROCEEDS,
              BRFILE.BR_WKDAY_BANKID,
              BRFILE.BR_PURGE_NOW_FLAG,
              BRFILE.BR_EMAIL,
              BRFILE.BR_MANAGER_EMAIL,
              BRFILE.BR_VERITEC_LICENSE,
              BRFILE.BR_TEST_PP_AUTO_INSCOVR,
              BRFILE.BR_USE_NEW_WFARGO_ACCT,
              BRFILE.BR_BRANCH_CLOSED,
              BRFILE.BR_ALLOW_F3_ALL_DOCS)
            VALUES
             (:QBR-NO,
              :QBR-LTOUCH-DATE,
              :QBR-NAME,
              :QBR-ADR1,
              :QBR-ADR2,
              :QBR-CITY,
              :QBR-STATE,
              :QBR-ZIP,
              :QBR-PHONE,
              :QBR-RETAIN,
              :QBR-NEXT-CHECK,
              :QBR-PRINTER-1,
              :QBR-PRINTER-2,
              :QBR-PRINTER-3,
              :QBR-PRINTER-4,
              :QBR-FEE-GL-BRNO,
              :QBR-REMIT1,
              :QBR-REMIT2,
              :QBR-REMIT3,
              :QBR-STMNT1,
              :QBR-STMNT2,
              :QBR-INS-PRODUCER-NUM,
              :QBR-DLRCHK,
              :QBR-FRONT-END-FG,
              :QBR-EMLNRG-PHONE,
              :QBR-POST-GL,
              :QBR-ALLOTMNTS,
              :QBR-EXTRACT-AUTO-UPD,
              :QBR-PCNI-LEDGER,
              :QBR-PRINTERFG-1,
              :QBR-PRINTERFG-2,
              :QBR-PRINTERFG-3,
              :QBR-PRINTERFG-4,
              :QBR-NEGDLRSV,
              :QBR-CPCORP-ORDER,
              :QBR-PRIO-MEMO-FLAG,
              :QBR-PREBILL-PL-JD-OPT,
              :QBR-LOAN-NUM-FLAG,
              :QBR-DOWNPY-FLAG,
              :QBR-AUTO-INS-EARN,
              :QBR-OTH-INS-LEVELS,
              :QBR-ALLOW-INTERBR,
              :QBR-UNER-DLANRP,
              :QBR-SECOND-PROMISE,
              :QBR-AVAIL-CREDIT,
              :QBR-RECP1,
              :QBR-RECP2,
              :QBR-INC-WAIVERS,
              :QBR-INC-INS,
              :QBR-BP-NEXT-BATCH,
              :QBR-BP-NOPAYOFF,
              :QBR-BP-ODDPAY,
              :QBR-BP-LC,
              :QBR-BP-NOPRIN,
              :QBR-BP-FLASH,
              :QBR-BP-ACTIONCD,
              :QBR-BP-MAXCON,
              :QBR-BP-MAXREC,
              :QBR-BP-PL,
              :QBR-BP-JD,
              :QBR-BP-BRANCH,
              :QBR-AGE-ERN,
              :QBR-OTHER-BRANCH-GL,
              :QBR-SALES-SMALL,
              :QBR-NEW-COLL-INQUIRY,
              :QBR-CHANGE-MEMO,
              :QBR-INS-ADDON,
              :QBR-TRAILER-REC,
              :QBR-ALDEL-FLAG,
              :QBR-IB-LCHG-MESS,
              :QBR-COL-QUEUE-PAST-DUE,
              :QBR-LI-DLCOMM-CMSI-PP-MAX,
              :QBR-ADDON-FEECD-O2-1,
              :QBR-ADDON-FEECD-O2-2,
              :QBR-AUTO-CHECK,
              :QBR-RECEIPT-PROMDATE,
              :QBR-RECEIPT-INTPD,
              :QBR-COLL-ID-SCAN,
              :QBR-STOP-ON-COLL,
              :QBR-DUEDATE-CHANGE-CD,
              :QBR-CHECK-EOM-FG,
              :QBR-VOLUME-DLANRP,
              :QBR-PDTH-NXDUE,
              :QBR-LC-CODE,
              :QBR-VALIDATE-PRUS,
              :QBR-AUTO-OPEN-BAL,
              :QBR-OPEN-BAL-1,
              :QBR-OPEN-BAL-2,
              :QBR-OPEN-BAL-3,
              :QBR-OPEN-BAL-4,
              :QBR-OPEN-BAL-5,
              :QBR-OPEN-BAL-6,
              :QBR-OPEN-BAL-7,
              :QBR-OPEN-BAL-8,
              :QBR-OPEN-BAL-9,
              :QBR-OPEN-BAL-10,
              :QBR-OPEN-BAL-11,
              :QBR-OPEN-BAL-12,
              :QBR-OPEN-BAL-13,
              :QBR-OPEN-BAL-14,
              :QBR-OPEN-BAL-15,
              :QBR-OPEN-BAL-16,
              :QBR-OPEN-BAL-17,
              :QBR-OPEN-BAL-18,
              :QBR-OPEN-BAL-19,
              :QBR-OPEN-BAL-20,
              :QBR-TENDER-CD-1,
              :QBR-TENDER-CD-2,
              :QBR-TENDER-CD-3,
              :QBR-DLPARV-ADJ-FG,
              :QBR-OVERRIDE-BR-STATE,
              :QBR-DEFAULT-SC-CLASS,
              :QBR-RECDEL-NOT-REC,
              :QBR-POST-AHEAD-FG,
              :QBR-CAN-LOANS-FG,
              :QBR-NON-MONTHLY-CAN-LOANS,
              :QBR-BP-ALLOW-PMT-ACCT-OTHBAL,
              :QBR-LBOX-ALLOW-PMT-ACCT-OTHBAL,
              :QBR-REGION-NUMBER,
              :QBR-OUR-FEE,
              :QBR-REB-METHOD-1,
              :QBR-REB-METHOD-2,
              :QBR-REB-METHOD-3,
              :QBR-REB-METHOD-4,
              :QBR-REB-METHOD-5,
              :QBR-REB-METHOD-6,
              :QBR-REB-METHOD-7,
              :QBR-REB-METHOD-8,
              :QBR-REB-METHOD-9,
              :QBR-REB-METHOD-10,
              :QBR-REB-METHOD-11,
              :QBR-REBDAYS-MONTH-1,
              :QBR-REBDAYS-MONTH-2,
              :QBR-REBDAYS-MONTH-3,
              :QBR-REBDAYS-MONTH-4,
              :QBR-REBDAYS-MONTH-5,
              :QBR-REBDAYS-MONTH-6,
              :QBR-REBDAYS-MONTH-7,
              :QBR-REBDAYS-MONTH-8,
              :QBR-REBDAYS-MONTH-9,
              :QBR-REBDAYS-MONTH-10,
              :QBR-REBDAYS-MONTH-11,
              :QBR-ACCRUAL-MONTHS-1,
              :QBR-ACCRUAL-MONTHS-2,
              :QBR-ACCRUAL-MONTHS-3,
              :QBR-ACCRUAL-MONTHS-4,
              :QBR-ACCRUAL-MONTHS-5,
              :QBR-ACCRUAL-MONTHS-6,
              :QBR-ACCRUAL-MONTHS-7,
              :QBR-ACCRUAL-MONTHS-8,
              :QBR-ACCRUAL-MONTHS-9,
              :QBR-ACCRUAL-MONTHS-10,
              :QBR-ACCRUAL-MONTHS-11,
              :QBR-GLOVERSHORT,
              :QBR-GLBANK,
              :QBR-GLONHAND,
              :QBR-BANKBAL,
              :QBR-RETAIN-AMT,
              :QBR-LCARDFG,
              :QBR-DATA-PATH,
              :QBR-MACHINE,
              :QBR-MANAGER,
              :QBR-EOD-PROCEDURE,
              :QBR-BP-PAYOFF-NONCASH,
              :QBR-BP-PAYOFF-OV,
              :QBR-LI-DLCOMM-CMSI-O3-MAX,
              :QBR-LR-GROUP-COLLID,
              :QBR-UNITPER-FG,
              :QBR-UNITPER-Y-D-FG,
              :QBR-NO-PROMPT-FOR-OTHBR,
              :QBR-WORLD-BRANCH-TYPE,
              :QBR-COL-QUEUE-BEHAVIOR-FG,
              :QBR-CONVERT-AUTO-ACCTNO,
              :QBR-TS-REPO-INS-FG,
              :QBR-BOOK-SF-BR-ONLY,
              :QBR-FORCE-VALID-CITY-COUNTY,
              :QBR-SET-COLLCD-DL-MFGCD,
              :QBR-DEFAULT-SF-UNITPER-CD,
              :QBR-DEFAULT-SF-PURCD,
              :QBR-DEFAULT-LN-UNITPER-CD,
              :QBR-DEFAULT-LN-PURCD,
              :QBR-TAP-OTHER-DL,
              :QBR-AH-AND-CL,
              :QBR-NO-CASH-PROCEEDS,
              :QBR-RI-BEFORE-REPO,
              :QBR-RI-BEFORE-P4P5,
              :QBR-NOT-OURS-REFUND,
              :QBR-SFDLO3-WINDOW-FG,
              :QBR-CALC-INS-ADJ,
              :QBR-AC-BEFORE-REPO,
              :QBR-AC-BEFORE-P4P5,
              :QBR-BANK-ROUTING-NO,
              :QBR-BANK-CLEARING-GL,
              :QBR-GLBANK-DEPOSIT,
              :QBR-BANK-ACCTNO,
              :QBR-DEFAULT-SC-SUBCLASS,
              :QBR-NO-OUT-DLR-RSV,
              :QBR-COLLECTOR-QUEUE,
              :QBR-COLL-QUEUE-CON-REC,
              :QBR-JE-ALLOCATION-SKIP,
              :QBR-EXPAND-APP-INPUT,
              :QBR-A50-CPCORP,
              :QBR-INBR-BILNSC-FG,
              :QBR-INBR-BRIEF-FG,
              :QBR-LEDGER-JUST-INTCHG,
              :QBR-BYINQ-START-PAGE,
              :QBR-DSINQ-START-PAGE,
              :QBR-APP-REQUIRED-FOR-INPUT,
              :QBR-CREDIT-SCORING-FG,
              :QBR-COL-QUEUE-LOW-TO-HI,
              :QBR-CAN-GROUP-ID,
              :QBR-AUTO-LNSTAT-WINDOW,
              :QBR-TIME-ZONE-FACTOR,
              :QBR-LONPF-OPT-DISPLAY,
              :QBR-GIFTNET,
              :QBR-GIFTNET-LEDGER-LINE,
              :QBR-NO-FT-EMP,
              :QBR-NO-PT-EMP,
              :QBR-BALLOON-LOANS-FG,
              :QBR-GL-TYPE-ALLOWED,
              :QBR-GL-ENTRY-DEFAULT,
              :QBR-MIDSTREAM-1STPAY-FG,
              :QBR-NODEF-1ST-X-MONTHS,
              :QBR-ADD-INS-FEECODE,
              :QBR-ROAD-G-SGL-FEECODE,
              :QBR-ROAD-G-FAM-FEECODE,
              :QBR-NODEF-IF-ZERO-AMT,
              :QBR-ASSESS-LATE-CHGS,
              :QBR-NEXT-SOLICIT-NO,
              :QBR-ADDON-FEECD-O3-1,
              :QBR-ADDON-FEECD-O3-2,
              :QBR-IMMEDIATE-RECEIPT,
              :QBR-IMMEDIATE-DOC-PRINT,
              :QBR-AUTO-PL-REF-AC,
              :QBR-FAX-NUMBER,
              :QBR-ALPHA-DRAWERS,
              :QBR-LATECHG-ON-ALLOTMENTS,
              :QBR-ALLOW-SYS-AS-EFFDATE,
              :QBR-TAX-PREBOOK-CUST,
              :QBR-TAX-PREBOOK-NONCUST,
              :QBR-PASSWORD-DRAWER,
              :QBR-ONESTEP-RN-FG,
              :QBR-LI-LISCAN-FG,
              :QBR-HIDE-END-BAL,
              :QBR-LBOX-NOPAYOFF,
              :QBR-LBOX-ODDPAY,
              :QBR-LBOX-LC,
              :QBR-LBOX-NOPRIN,
              :QBR-LBOX-FLASH,
              :QBR-LBOX-ACTIONCD,
              :QBR-LBOX-MAXCON,
              :QBR-LBOX-MAXREC,
              :QBR-LBOX-PL,
              :QBR-LBOX-JD,
              :QBR-LBOX,
              :QBR-LATECHG-ON-LBOX,
              :QBR-ALLOW-OV-TRCD,
              :QBR-GLOBAL-QUEUE,
              :QBR-LBOX-PAYOFF-NONCASH,
              :QBR-LBOX-PAYOFF-OV,
              :QBR-COLLECT-MANAGER,
              :QBR-COLLECT-PHONE,
              :QBR-COLLECT-EXT,
              :QBR-ALT-INTO-THIS-BRANCH,
              :QBR-POFF-TITLE-WINDOW,
              :QBR-COUPON-STATEMENT,
              :QBR-SUPERVISOR,
              :QBR-AREA-MANAGER,
              :QBR-MIN-MONTHS-DEF,
              :QBR-CC-FLAG,
              :QBR-CC-ADR1,
              :QBR-CC-CITY,
              :QBR-CC-STATE,
              :QBR-CC-ZIP,
              :QBR-MIN-MONTHS-D2,
              :QBR-SKIP-MIDSTREAM-TRW,
              :QBR-GL-ESCROW-BALANCE,
              :QBR-GL-ESCROW-PAYABLE,
              :QBR-ALLOW-FORCE-OPEN-FG,
              :QBR-ACH-BANK-NO,
              :QBR-WIMENU-CHECK-WRITING,
              :QBR-ADP-PAY-GROUP,
              :QBR-ALLOW-SCANNED-CKS,
              :QBR-CHECK-DEPOSIT-GL,
              :QBR-OPTION-PST-LASER,
              :QBR-USE-DEBIT-CARDS,
              :QBR-GL-DEBIT-CARD-PROCEEDS,
              :QBR-WKDAY-BANKID,
              :QBR-PURGE-NOW-FLAG,
              :QBR-EMAIL,
              :QBR-MANAGER-EMAIL,
              :QBR-VERITEC-LICENSE,
              :QBR-TEST-PP-AUTO-INSCOVR,
              :QBR-USE-NEW-WFARGO-ACCT,
              :QBR-BRANCH-CLOSED,
              :QBR-ALLOW-F3-ALL-DOCS)
           END-EXEC.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-BR-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

           MOVE '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       REWRITE-BR-FILE.

           ACCEPT BR-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE BR-PATH-SQL TO E-FILE.
           MOVE BR-NO TO E-KEYX.
           PERFORM SET-BR-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            UPDATE DBO.BRFILE
             SET
              BRFILE.BR_NO = :QBR-NO,
              BRFILE.BR_LTOUCH_DATE = :QBR-LTOUCH-DATE,
              BRFILE.BR_NAME = :QBR-NAME,
              BRFILE.BR_ADR1 = :QBR-ADR1,
              BRFILE.BR_ADR2 = :QBR-ADR2,
              BRFILE.BR_CITY = :QBR-CITY,
              BRFILE.BR_STATE = :QBR-STATE,
              BRFILE.BR_ZIP = :QBR-ZIP,
              BRFILE.BR_PHONE = :QBR-PHONE,
              BRFILE.BR_RETAIN = :QBR-RETAIN,
              BRFILE.BR_NEXT_CHECK = :QBR-NEXT-CHECK,
              BRFILE.BR_PRINTER_1 = :QBR-PRINTER-1,
              BRFILE.BR_PRINTER_2 = :QBR-PRINTER-2,
              BRFILE.BR_PRINTER_3 = :QBR-PRINTER-3,
              BRFILE.BR_PRINTER_4 = :QBR-PRINTER-4,
              BRFILE.BR_FEE_GL_BRNO = :QBR-FEE-GL-BRNO,
              BRFILE.BR_REMIT1 = :QBR-REMIT1,
              BRFILE.BR_REMIT2 = :QBR-REMIT2,
              BRFILE.BR_REMIT3 = :QBR-REMIT3,
              BRFILE.BR_STMNT1 = :QBR-STMNT1,
              BRFILE.BR_STMNT2 = :QBR-STMNT2,
              BRFILE.BR_INS_PRODUCER_NUM = :QBR-INS-PRODUCER-NUM,
              BRFILE.BR_DLRCHK = :QBR-DLRCHK,
              BRFILE.BR_FRONT_END_FG = :QBR-FRONT-END-FG,
              BRFILE.BR_EMLNRG_PHONE = :QBR-EMLNRG-PHONE,
              BRFILE.BR_POST_GL = :QBR-POST-GL,
              BRFILE.BR_ALLOTMNTS = :QBR-ALLOTMNTS,
              BRFILE.BR_EXTRACT_AUTO_UPD = :QBR-EXTRACT-AUTO-UPD,
              BRFILE.BR_PCNI_LEDGER = :QBR-PCNI-LEDGER,
              BRFILE.BR_PRINTERFG_1 = :QBR-PRINTERFG-1,
              BRFILE.BR_PRINTERFG_2 = :QBR-PRINTERFG-2,
              BRFILE.BR_PRINTERFG_3 = :QBR-PRINTERFG-3,
              BRFILE.BR_PRINTERFG_4 = :QBR-PRINTERFG-4,
              BRFILE.BR_NEGDLRSV = :QBR-NEGDLRSV,
              BRFILE.BR_CPCORP_ORDER = :QBR-CPCORP-ORDER,
              BRFILE.BR_PRIO_MEMO_FLAG = :QBR-PRIO-MEMO-FLAG,
              BRFILE.BR_PREBILL_PL_JD_OPT =
                   :QBR-PREBILL-PL-JD-OPT,
              BRFILE.BR_LOAN_NUM_FLAG = :QBR-LOAN-NUM-FLAG,
              BRFILE.BR_DOWNPY_FLAG = :QBR-DOWNPY-FLAG,
              BRFILE.BR_AUTO_INS_EARN = :QBR-AUTO-INS-EARN,
              BRFILE.BR_OTH_INS_LEVELS = :QBR-OTH-INS-LEVELS,
              BRFILE.BR_ALLOW_INTERBR = :QBR-ALLOW-INTERBR,
              BRFILE.BR_UNER_DLANRP = :QBR-UNER-DLANRP,
              BRFILE.BR_SECOND_PROMISE = :QBR-SECOND-PROMISE,
              BRFILE.BR_AVAIL_CREDIT = :QBR-AVAIL-CREDIT,
              BRFILE.BR_RECP1 = :QBR-RECP1,
              BRFILE.BR_RECP2 = :QBR-RECP2,
              BRFILE.BR_INC_WAIVERS = :QBR-INC-WAIVERS,
              BRFILE.BR_INC_INS = :QBR-INC-INS,
              BRFILE.BR_BP_NEXT_BATCH = :QBR-BP-NEXT-BATCH,
              BRFILE.BR_BP_NOPAYOFF = :QBR-BP-NOPAYOFF,
              BRFILE.BR_BP_ODDPAY = :QBR-BP-ODDPAY,
              BRFILE.BR_BP_LC = :QBR-BP-LC,
              BRFILE.BR_BP_NOPRIN = :QBR-BP-NOPRIN,
              BRFILE.BR_BP_FLASH = :QBR-BP-FLASH,
              BRFILE.BR_BP_ACTIONCD = :QBR-BP-ACTIONCD,
              BRFILE.BR_BP_MAXCON = :QBR-BP-MAXCON,
              BRFILE.BR_BP_MAXREC = :QBR-BP-MAXREC,
              BRFILE.BR_BP_PL = :QBR-BP-PL,
              BRFILE.BR_BP_JD = :QBR-BP-JD,
              BRFILE.BR_BP_BRANCH = :QBR-BP-BRANCH,
              BRFILE.BR_AGE_ERN = :QBR-AGE-ERN,
              BRFILE.BR_OTHER_BRANCH_GL = :QBR-OTHER-BRANCH-GL,
              BRFILE.BR_SALES_SMALL = :QBR-SALES-SMALL,
              BRFILE.BR_NEW_COLL_INQUIRY = :QBR-NEW-COLL-INQUIRY,
              BRFILE.BR_CHANGE_MEMO = :QBR-CHANGE-MEMO,
              BRFILE.BR_INS_ADDON = :QBR-INS-ADDON,
              BRFILE.BR_TRAILER_REC = :QBR-TRAILER-REC,
              BRFILE.BR_ALDEL_FLAG = :QBR-ALDEL-FLAG,
              BRFILE.BR_IB_LCHG_MESS = :QBR-IB-LCHG-MESS,
              BRFILE.BR_COL_QUEUE_PAST_DUE =
                   :QBR-COL-QUEUE-PAST-DUE,
              BRFILE.BR_LI_DLCOMM_CMSI_PP_MAX =
                   :QBR-LI-DLCOMM-CMSI-PP-MAX,
              BRFILE.BR_ADDON_FEECD_O2_1 = :QBR-ADDON-FEECD-O2-1,
              BRFILE.BR_ADDON_FEECD_O2_2 = :QBR-ADDON-FEECD-O2-2,
              BRFILE.BR_AUTO_CHECK = :QBR-AUTO-CHECK,
              BRFILE.BR_RECEIPT_PROMDATE = :QBR-RECEIPT-PROMDATE,
              BRFILE.BR_RECEIPT_INTPD = :QBR-RECEIPT-INTPD,
              BRFILE.BR_COLL_ID_SCAN = :QBR-COLL-ID-SCAN,
              BRFILE.BR_STOP_ON_COLL = :QBR-STOP-ON-COLL,
              BRFILE.BR_DUEDATE_CHANGE_CD =
                   :QBR-DUEDATE-CHANGE-CD,
              BRFILE.BR_CHECK_EOM_FG = :QBR-CHECK-EOM-FG,
              BRFILE.BR_VOLUME_DLANRP = :QBR-VOLUME-DLANRP,
              BRFILE.BR_PDTH_NXDUE = :QBR-PDTH-NXDUE,
              BRFILE.BR_LC_CODE = :QBR-LC-CODE,
              BRFILE.BR_VALIDATE_PRUS = :QBR-VALIDATE-PRUS,
              BRFILE.BR_AUTO_OPEN_BAL = :QBR-AUTO-OPEN-BAL,
              BRFILE.BR_OPEN_BAL_1 = :QBR-OPEN-BAL-1,
              BRFILE.BR_OPEN_BAL_2 = :QBR-OPEN-BAL-2,
              BRFILE.BR_OPEN_BAL_3 = :QBR-OPEN-BAL-3,
              BRFILE.BR_OPEN_BAL_4 = :QBR-OPEN-BAL-4,
              BRFILE.BR_OPEN_BAL_5 = :QBR-OPEN-BAL-5,
              BRFILE.BR_OPEN_BAL_6 = :QBR-OPEN-BAL-6,
              BRFILE.BR_OPEN_BAL_7 = :QBR-OPEN-BAL-7,
              BRFILE.BR_OPEN_BAL_8 = :QBR-OPEN-BAL-8,
              BRFILE.BR_OPEN_BAL_9 = :QBR-OPEN-BAL-9,
              BRFILE.BR_OPEN_BAL_10 = :QBR-OPEN-BAL-10,
              BRFILE.BR_OPEN_BAL_11 = :QBR-OPEN-BAL-11,
              BRFILE.BR_OPEN_BAL_12 = :QBR-OPEN-BAL-12,
              BRFILE.BR_OPEN_BAL_13 = :QBR-OPEN-BAL-13,
              BRFILE.BR_OPEN_BAL_14 = :QBR-OPEN-BAL-14,
              BRFILE.BR_OPEN_BAL_15 = :QBR-OPEN-BAL-15,
              BRFILE.BR_OPEN_BAL_16 = :QBR-OPEN-BAL-16,
              BRFILE.BR_OPEN_BAL_17 = :QBR-OPEN-BAL-17,
              BRFILE.BR_OPEN_BAL_18 = :QBR-OPEN-BAL-18,
              BRFILE.BR_OPEN_BAL_19 = :QBR-OPEN-BAL-19,
              BRFILE.BR_OPEN_BAL_20 = :QBR-OPEN-BAL-20,
              BRFILE.BR_TENDER_CD_1 = :QBR-TENDER-CD-1,
              BRFILE.BR_TENDER_CD_2 = :QBR-TENDER-CD-2,
              BRFILE.BR_TENDER_CD_3 = :QBR-TENDER-CD-3,
              BRFILE.BR_DLPARV_ADJ_FG = :QBR-DLPARV-ADJ-FG,
              BRFILE.BR_OVERRIDE_BR_STATE =
                   :QBR-OVERRIDE-BR-STATE,
              BRFILE.BR_DEFAULT_SC_CLASS = :QBR-DEFAULT-SC-CLASS,
              BRFILE.BR_RECDEL_NOT_REC = :QBR-RECDEL-NOT-REC,
              BRFILE.BR_POST_AHEAD_FG = :QBR-POST-AHEAD-FG,
              BRFILE.BR_CAN_LOANS_FG = :QBR-CAN-LOANS-FG,
              BRFILE.BR_NON_MONTHLY_CAN_LOANS =
                   :QBR-NON-MONTHLY-CAN-LOANS,
              BRFILE.BR_BP_ALLOW_PMT_ACCT_OTHBAL =
                   :QBR-BP-ALLOW-PMT-ACCT-OTHBAL,
              BRFILE.BR_LBOX_ALLOW_PMT_ACCT_OTHBAL =
                   :QBR-LBOX-ALLOW-PMT-ACCT-OTHBAL,
              BRFILE.BR_REGION_NUMBER = :QBR-REGION-NUMBER,
              BRFILE.BR_OUR_FEE = :QBR-OUR-FEE,
              BRFILE.BR_REB_METHOD_1 = :QBR-REB-METHOD-1,
              BRFILE.BR_REB_METHOD_2 = :QBR-REB-METHOD-2,
              BRFILE.BR_REB_METHOD_3 = :QBR-REB-METHOD-3,
              BRFILE.BR_REB_METHOD_4 = :QBR-REB-METHOD-4,
              BRFILE.BR_REB_METHOD_5 = :QBR-REB-METHOD-5,
              BRFILE.BR_REB_METHOD_6 = :QBR-REB-METHOD-6,
              BRFILE.BR_REB_METHOD_7 = :QBR-REB-METHOD-7,
              BRFILE.BR_REB_METHOD_8 = :QBR-REB-METHOD-8,
              BRFILE.BR_REB_METHOD_9 = :QBR-REB-METHOD-9,
              BRFILE.BR_REB_METHOD_10 = :QBR-REB-METHOD-10,
              BRFILE.BR_REB_METHOD_11 = :QBR-REB-METHOD-11,
              BRFILE.BR_REBDAYS_MONTH_1 = :QBR-REBDAYS-MONTH-1,
              BRFILE.BR_REBDAYS_MONTH_2 = :QBR-REBDAYS-MONTH-2,
              BRFILE.BR_REBDAYS_MONTH_3 = :QBR-REBDAYS-MONTH-3,
              BRFILE.BR_REBDAYS_MONTH_4 = :QBR-REBDAYS-MONTH-4,
              BRFILE.BR_REBDAYS_MONTH_5 = :QBR-REBDAYS-MONTH-5,
              BRFILE.BR_REBDAYS_MONTH_6 = :QBR-REBDAYS-MONTH-6,
              BRFILE.BR_REBDAYS_MONTH_7 = :QBR-REBDAYS-MONTH-7,
              BRFILE.BR_REBDAYS_MONTH_8 = :QBR-REBDAYS-MONTH-8,
              BRFILE.BR_REBDAYS_MONTH_9 = :QBR-REBDAYS-MONTH-9,
              BRFILE.BR_REBDAYS_MONTH_10 = :QBR-REBDAYS-MONTH-10,
              BRFILE.BR_REBDAYS_MONTH_11 = :QBR-REBDAYS-MONTH-11,
              BRFILE.BR_ACCRUAL_MONTHS_1 = :QBR-ACCRUAL-MONTHS-1,
              BRFILE.BR_ACCRUAL_MONTHS_2 = :QBR-ACCRUAL-MONTHS-2,
              BRFILE.BR_ACCRUAL_MONTHS_3 = :QBR-ACCRUAL-MONTHS-3,
              BRFILE.BR_ACCRUAL_MONTHS_4 = :QBR-ACCRUAL-MONTHS-4,
              BRFILE.BR_ACCRUAL_MONTHS_5 = :QBR-ACCRUAL-MONTHS-5,
              BRFILE.BR_ACCRUAL_MONTHS_6 = :QBR-ACCRUAL-MONTHS-6,
              BRFILE.BR_ACCRUAL_MONTHS_7 = :QBR-ACCRUAL-MONTHS-7,
              BRFILE.BR_ACCRUAL_MONTHS_8 = :QBR-ACCRUAL-MONTHS-8,
              BRFILE.BR_ACCRUAL_MONTHS_9 = :QBR-ACCRUAL-MONTHS-9,
              BRFILE.BR_ACCRUAL_MONTHS_10 =
                   :QBR-ACCRUAL-MONTHS-10,
              BRFILE.BR_ACCRUAL_MONTHS_11 =
                   :QBR-ACCRUAL-MONTHS-11,
              BRFILE.BR_GLOVERSHORT = :QBR-GLOVERSHORT,
              BRFILE.BR_GLBANK = :QBR-GLBANK,
              BRFILE.BR_GLONHAND = :QBR-GLONHAND,
              BRFILE.BR_BANKBAL = :QBR-BANKBAL,
              BRFILE.BR_RETAIN_AMT = :QBR-RETAIN-AMT,
              BRFILE.BR_LCARDFG = :QBR-LCARDFG,
              BRFILE.BR_DATA_PATH = :QBR-DATA-PATH,
              BRFILE.BR_MACHINE = :QBR-MACHINE,
              BRFILE.BR_MANAGER = :QBR-MANAGER,
              BRFILE.BR_EOD_PROCEDURE = :QBR-EOD-PROCEDURE,
              BRFILE.BR_BP_PAYOFF_NONCASH =
                   :QBR-BP-PAYOFF-NONCASH,
              BRFILE.BR_BP_PAYOFF_OV = :QBR-BP-PAYOFF-OV,
              BRFILE.BR_LI_DLCOMM_CMSI_O3_MAX =
                   :QBR-LI-DLCOMM-CMSI-O3-MAX,
              BRFILE.BR_LR_GROUP_COLLID = :QBR-LR-GROUP-COLLID,
              BRFILE.BR_UNITPER_FG = :QBR-UNITPER-FG,
              BRFILE.BR_UNITPER_Y_D_FG = :QBR-UNITPER-Y-D-FG,
              BRFILE.BR_NO_PROMPT_FOR_OTHBR =
                   :QBR-NO-PROMPT-FOR-OTHBR,
              BRFILE.BR_WORLD_BRANCH_TYPE =
                   :QBR-WORLD-BRANCH-TYPE,
              BRFILE.BR_COL_QUEUE_BEHAVIOR_FG =
                   :QBR-COL-QUEUE-BEHAVIOR-FG,
              BRFILE.BR_CONVERT_AUTO_ACCTNO =
                   :QBR-CONVERT-AUTO-ACCTNO,
              BRFILE.BR_TS_REPO_INS_FG = :QBR-TS-REPO-INS-FG,
              BRFILE.BR_BOOK_SF_BR_ONLY = :QBR-BOOK-SF-BR-ONLY,
              BRFILE.BR_FORCE_VALID_CITY_COUNTY =
                   :QBR-FORCE-VALID-CITY-COUNTY,
              BRFILE.BR_SET_COLLCD_DL_MFGCD =
                   :QBR-SET-COLLCD-DL-MFGCD,
              BRFILE.BR_DEFAULT_SF_UNITPER_CD =
                   :QBR-DEFAULT-SF-UNITPER-CD,
              BRFILE.BR_DEFAULT_SF_PURCD = :QBR-DEFAULT-SF-PURCD,
              BRFILE.BR_DEFAULT_LN_UNITPER_CD =
                   :QBR-DEFAULT-LN-UNITPER-CD,
              BRFILE.BR_DEFAULT_LN_PURCD = :QBR-DEFAULT-LN-PURCD,
              BRFILE.BR_TAP_OTHER_DL = :QBR-TAP-OTHER-DL,
              BRFILE.BR_AH_AND_CL = :QBR-AH-AND-CL,
              BRFILE.BR_NO_CASH_PROCEEDS = :QBR-NO-CASH-PROCEEDS,
              BRFILE.BR_RI_BEFORE_REPO = :QBR-RI-BEFORE-REPO,
              BRFILE.BR_RI_BEFORE_P4P5 = :QBR-RI-BEFORE-P4P5,
              BRFILE.BR_NOT_OURS_REFUND = :QBR-NOT-OURS-REFUND,
              BRFILE.BR_SFDLO3_WINDOW_FG = :QBR-SFDLO3-WINDOW-FG,
              BRFILE.BR_CALC_INS_ADJ = :QBR-CALC-INS-ADJ,
              BRFILE.BR_AC_BEFORE_REPO = :QBR-AC-BEFORE-REPO,
              BRFILE.BR_AC_BEFORE_P4P5 = :QBR-AC-BEFORE-P4P5,
              BRFILE.BR_BANK_ROUTING_NO = :QBR-BANK-ROUTING-NO,
              BRFILE.BR_BANK_CLEARING_GL = :QBR-BANK-CLEARING-GL,
              BRFILE.BR_GLBANK_DEPOSIT = :QBR-GLBANK-DEPOSIT,
              BRFILE.BR_BANK_ACCTNO = :QBR-BANK-ACCTNO,
              BRFILE.BR_DEFAULT_SC_SUBCLASS =
                   :QBR-DEFAULT-SC-SUBCLASS,
              BRFILE.BR_NO_OUT_DLR_RSV = :QBR-NO-OUT-DLR-RSV,
              BRFILE.BR_COLLECTOR_QUEUE = :QBR-COLLECTOR-QUEUE,
              BRFILE.BR_COLL_QUEUE_CON_REC =
                   :QBR-COLL-QUEUE-CON-REC,
              BRFILE.BR_JE_ALLOCATION_SKIP =
                   :QBR-JE-ALLOCATION-SKIP,
              BRFILE.BR_EXPAND_APP_INPUT = :QBR-EXPAND-APP-INPUT,
              BRFILE.BR_A50_CPCORP = :QBR-A50-CPCORP,
              BRFILE.BR_INBR_BILNSC_FG = :QBR-INBR-BILNSC-FG,
              BRFILE.BR_INBR_BRIEF_FG = :QBR-INBR-BRIEF-FG,
              BRFILE.BR_LEDGER_JUST_INTCHG =
                   :QBR-LEDGER-JUST-INTCHG,
              BRFILE.BR_BYINQ_START_PAGE = :QBR-BYINQ-START-PAGE,
              BRFILE.BR_DSINQ_START_PAGE = :QBR-DSINQ-START-PAGE,
              BRFILE.BR_APP_REQUIRED_FOR_INPUT =
                   :QBR-APP-REQUIRED-FOR-INPUT,
              BRFILE.BR_CREDIT_SCORING_FG =
                   :QBR-CREDIT-SCORING-FG,
              BRFILE.BR_COL_QUEUE_LOW_TO_HI =
                   :QBR-COL-QUEUE-LOW-TO-HI,
              BRFILE.BR_CAN_GROUP_ID = :QBR-CAN-GROUP-ID,
              BRFILE.BR_AUTO_LNSTAT_WINDOW =
                   :QBR-AUTO-LNSTAT-WINDOW,
              BRFILE.BR_TIME_ZONE_FACTOR = :QBR-TIME-ZONE-FACTOR,
              BRFILE.BR_LONPF_OPT_DISPLAY =
                   :QBR-LONPF-OPT-DISPLAY,
              BRFILE.BR_GIFTNET = :QBR-GIFTNET,
              BRFILE.BR_GIFTNET_LEDGER_LINE =
                   :QBR-GIFTNET-LEDGER-LINE,
              BRFILE.BR_NO_FT_EMP = :QBR-NO-FT-EMP,
              BRFILE.BR_NO_PT_EMP = :QBR-NO-PT-EMP,
              BRFILE.BR_BALLOON_LOANS_FG = :QBR-BALLOON-LOANS-FG,
              BRFILE.BR_GL_TYPE_ALLOWED = :QBR-GL-TYPE-ALLOWED,
              BRFILE.BR_GL_ENTRY_DEFAULT = :QBR-GL-ENTRY-DEFAULT,
              BRFILE.BR_MIDSTREAM_1STPAY_FG =
                   :QBR-MIDSTREAM-1STPAY-FG,
              BRFILE.BR_NODEF_1ST_X_MONTHS =
                   :QBR-NODEF-1ST-X-MONTHS,
              BRFILE.BR_ADD_INS_FEECODE = :QBR-ADD-INS-FEECODE,
              BRFILE.BR_ROAD_G_SGL_FEECODE =
                   :QBR-ROAD-G-SGL-FEECODE,
              BRFILE.BR_ROAD_G_FAM_FEECODE =
                   :QBR-ROAD-G-FAM-FEECODE,
              BRFILE.BR_NODEF_IF_ZERO_AMT =
                   :QBR-NODEF-IF-ZERO-AMT,
              BRFILE.BR_ASSESS_LATE_CHGS = :QBR-ASSESS-LATE-CHGS,
              BRFILE.BR_NEXT_SOLICIT_NO = :QBR-NEXT-SOLICIT-NO,
              BRFILE.BR_ADDON_FEECD_O3_1 = :QBR-ADDON-FEECD-O3-1,
              BRFILE.BR_ADDON_FEECD_O3_2 = :QBR-ADDON-FEECD-O3-2,
              BRFILE.BR_IMMEDIATE_RECEIPT =
                   :QBR-IMMEDIATE-RECEIPT,
              BRFILE.BR_IMMEDIATE_DOC_PRINT =
                   :QBR-IMMEDIATE-DOC-PRINT,
              BRFILE.BR_AUTO_PL_REF_AC = :QBR-AUTO-PL-REF-AC,
              BRFILE.BR_FAX_NUMBER = :QBR-FAX-NUMBER,
              BRFILE.BR_ALPHA_DRAWERS = :QBR-ALPHA-DRAWERS,
              BRFILE.BR_LATECHG_ON_ALLOTMENTS =
                   :QBR-LATECHG-ON-ALLOTMENTS,
              BRFILE.BR_ALLOW_SYS_AS_EFFDATE =
                   :QBR-ALLOW-SYS-AS-EFFDATE,
              BRFILE.BR_TAX_PREBOOK_CUST = :QBR-TAX-PREBOOK-CUST,
              BRFILE.BR_TAX_PREBOOK_NONCUST =
                   :QBR-TAX-PREBOOK-NONCUST,
              BRFILE.BR_PASSWORD_DRAWER = :QBR-PASSWORD-DRAWER,
              BRFILE.BR_ONESTEP_RN_FG = :QBR-ONESTEP-RN-FG,
              BRFILE.BR_LI_LISCAN_FG = :QBR-LI-LISCAN-FG,
              BRFILE.BR_HIDE_END_BAL = :QBR-HIDE-END-BAL,
              BRFILE.BR_LBOX_NOPAYOFF = :QBR-LBOX-NOPAYOFF,
              BRFILE.BR_LBOX_ODDPAY = :QBR-LBOX-ODDPAY,
              BRFILE.BR_LBOX_LC = :QBR-LBOX-LC,
              BRFILE.BR_LBOX_NOPRIN = :QBR-LBOX-NOPRIN,
              BRFILE.BR_LBOX_FLASH = :QBR-LBOX-FLASH,
              BRFILE.BR_LBOX_ACTIONCD = :QBR-LBOX-ACTIONCD,
              BRFILE.BR_LBOX_MAXCON = :QBR-LBOX-MAXCON,
              BRFILE.BR_LBOX_MAXREC = :QBR-LBOX-MAXREC,
              BRFILE.BR_LBOX_PL = :QBR-LBOX-PL,
              BRFILE.BR_LBOX_JD = :QBR-LBOX-JD,
              BRFILE.BR_LBOX = :QBR-LBOX,
              BRFILE.BR_LATECHG_ON_LBOX = :QBR-LATECHG-ON-LBOX,
              BRFILE.BR_ALLOW_OV_TRCD = :QBR-ALLOW-OV-TRCD,
              BRFILE.BR_GLOBAL_QUEUE = :QBR-GLOBAL-QUEUE,
              BRFILE.BR_LBOX_PAYOFF_NONCASH =
                   :QBR-LBOX-PAYOFF-NONCASH,
              BRFILE.BR_LBOX_PAYOFF_OV = :QBR-LBOX-PAYOFF-OV,
              BRFILE.BR_COLLECT_MANAGER = :QBR-COLLECT-MANAGER,
              BRFILE.BR_COLLECT_PHONE = :QBR-COLLECT-PHONE,
              BRFILE.BR_COLLECT_EXT = :QBR-COLLECT-EXT,
              BRFILE.BR_ALT_INTO_THIS_BRANCH =
                   :QBR-ALT-INTO-THIS-BRANCH,
              BRFILE.BR_POFF_TITLE_WINDOW =
                   :QBR-POFF-TITLE-WINDOW,
              BRFILE.BR_COUPON_STATEMENT = :QBR-COUPON-STATEMENT,
              BRFILE.BR_SUPERVISOR = :QBR-SUPERVISOR,
              BRFILE.BR_AREA_MANAGER = :QBR-AREA-MANAGER,
              BRFILE.BR_MIN_MONTHS_DEF = :QBR-MIN-MONTHS-DEF,
              BRFILE.BR_CC_FLAG = :QBR-CC-FLAG,
              BRFILE.BR_CC_ADR1 = :QBR-CC-ADR1,
              BRFILE.BR_CC_CITY = :QBR-CC-CITY,
              BRFILE.BR_CC_STATE = :QBR-CC-STATE,
              BRFILE.BR_CC_ZIP = :QBR-CC-ZIP,
              BRFILE.BR_MIN_MONTHS_D2 = :QBR-MIN-MONTHS-D2,
              BRFILE.BR_SKIP_MIDSTREAM_TRW =
                   :QBR-SKIP-MIDSTREAM-TRW,
              BRFILE.BR_GL_ESCROW_BALANCE =
                   :QBR-GL-ESCROW-BALANCE,
              BRFILE.BR_GL_ESCROW_PAYABLE =
                   :QBR-GL-ESCROW-PAYABLE,
              BRFILE.BR_ALLOW_FORCE_OPEN_FG =
                   :QBR-ALLOW-FORCE-OPEN-FG,
              BRFILE.BR_ACH_BANK_NO = :QBR-ACH-BANK-NO,
              BRFILE.BR_WIMENU_CHECK_WRITING =
                   :QBR-WIMENU-CHECK-WRITING,
              BRFILE.BR_ADP_PAY_GROUP = :QBR-ADP-PAY-GROUP,
              BRFILE.BR_ALLOW_SCANNED_CKS =
                   :QBR-ALLOW-SCANNED-CKS,
              BRFILE.BR_CHECK_DEPOSIT_GL = :QBR-CHECK-DEPOSIT-GL,
              BRFILE.BR_OPTION_PST_LASER = :QBR-OPTION-PST-LASER,
              BRFILE.BR_USE_DEBIT_CARDS = :QBR-USE-DEBIT-CARDS,
              BRFILE.BR_GL_DEBIT_CARD_PROCEEDS =
                   :QBR-GL-DEBIT-CARD-PROCEEDS,
              BRFILE.BR_WKDAY_BANKID = :QBR-WKDAY-BANKID,
              BRFILE.BR_PURGE_NOW_FLAG = :QBR-PURGE-NOW-FLAG,
              BRFILE.BR_EMAIL = :QBR-EMAIL,
              BRFILE.BR_MANAGER_EMAIL = :QBR-MANAGER-EMAIL,
              BRFILE.BR_VERITEC_LICENSE = :QBR-VERITEC-LICENSE,
            BRFILE.BR_TEST_PP_AUTO_INSCOVR = :QBR-TEST-PP-AUTO-INSCOVR,
            BRFILE.BR_USE_NEW_WFARGO_ACCT = :QBR-USE-NEW-WFARGO-ACCT,
            BRFILE.BR_BRANCH_CLOSED = :QBR-BRANCH-CLOSED,
            BRFILE.BR_ALLOW_F3_ALL_DOCS = :QBR-ALLOW-F3-ALL-DOCS
             WHERE BRFILE.BR_NO = :QBR-NO
           END-EXEC.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-BR-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

           MOVE '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       DELETE-BR-FILE.

           PERFORM DELETE-IT.
           MOVE BR-PATH-SQL TO E-FILE.
           MOVE BR-NO TO E-KEYX.
           PERFORM SET-BR-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            DELETE FROM DBO.BRFILE
             WHERE BRFILE.BR_NO = :QBR-NO
           END-EXEC.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-BR-FILE.

           MOVE '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       CLOSE-BR-FILE.

           PERFORM CLOSE-IT.

           IF ( BR-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE BR_CURSOR
              END-EXEC
              MOVE STAT---BAD TO BR-CURSOR-STAT.

      *-----------------------------------------------------------------
   
      * COPY "LIBLP/LPTY1IN.CPY".
      * COPYMEMBER: LIBLP/LPTY1IN
       LOAD-TY1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TY-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TY-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TY-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TY-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TY-ALL-DATA
              MOVE TY-ALL-PATH             TO TY-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TY-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TY-ALL-BASE
              MOVE FILPATH-MACHINE         TO TY-ALL-MACHINE
              MOVE FILPATH-DATA            TO TY-ALL-DATA
              MOVE TY-ALL-PATH             TO TY-PATH.

      *-----------------------------------------------------------------
       OPEN-TY1-FILE.
           PERFORM LOAD-TY1-FILE.
           PERFORM OPEN-IT.
           MOVE TY-PATH-SQL TO E-FILE.

           IF TY1-CONN-OPEN-SW = "N" 
              MOVE "Y"         TO TY1-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS TY1_CONNECT 
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-TY1-FILE.
           PERFORM START-IT.
           MOVE TY-PATH-OWNBR  TO TY-BRNO.
           MOVE TY-PATH-SQL    TO E-FILE.
           MOVE TY1-KEY        TO E-KEYX.

           IF ( TY1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TY1-FILE.

           IF ( QTY1-WEND-CLASS = ZEROES    ) OR
              ( QTY1-WEND-CLASS NOT NUMERIC )
              MOVE TY-PATH-OWNBR        TO QTY1-WBEG-BRNO
              MOVE TY-PATH-OWNBR        TO QTY1-WEND-BRNO
              MOVE TY-DATE              TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTY1-WBEG-DATE
              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTY1-WEND-DATE
              MOVE TY-CLASS             TO QTY1-WBEG-CLASS
              MOVE ALL "9"              TO QTY1-WEND-CLASS.

      *  STORED PROCEDURE:
      *
      *  SELECT TYFILE.TY_BRNO,
      *         CAST(TYFILE.TY_DATE        AS VARCHAR(10)) AS TY_DATE,
      *         TYFILE.TY_CLASS,
      *         CAST(TYFILE.TY_LTOUCH_DATE AS VARCHAR(10)) AS TY_LTOUCH_DATE,
      *         TYFILE.TY_NO_1_1,
      *         TYFILE.TY_AMT_1_1,
      *         TYFILE.TY_O2_1_1,
      *         TYFILE.TY_INT_1_1,
      *         TYFILE.TY_NO_1_2,
      *         TYFILE.TY_AMT_1_2,
      *         TYFILE.TY_O2_1_2,
      *         TYFILE.TY_INT_1_2,
      *         TYFILE.TY_NO_1_3,
      *         TYFILE.TY_AMT_1_3,
      *         TYFILE.TY_O2_1_3,
      *         TYFILE.TY_INT_1_3,
      *         TYFILE.TY_NO_1_4,
      *         TYFILE.TY_AMT_1_4,
      *         TYFILE.TY_O2_1_4,
      *         TYFILE.TY_INT_1_4,
      *         TYFILE.TY_NO_1_5,
      *         TYFILE.TY_AMT_1_5,
      *         TYFILE.TY_O2_1_5,
      *         TYFILE.TY_INT_1_5,
      *         TYFILE.TY_NO_1_6,
      *         TYFILE.TY_AMT_1_6,
      *         TYFILE.TY_O2_1_6,
      *         TYFILE.TY_INT_1_6,
      *         TYFILE.TY_NO_1_7,
      *         TYFILE.TY_AMT_1_7,
      *         TYFILE.TY_O2_1_7,
      *         TYFILE.TY_INT_1_7,
      *         TYFILE.TY_NO_1_8,
      *         TYFILE.TY_AMT_1_8,
      *         TYFILE.TY_O2_1_8,
      *         TYFILE.TY_INT_1_8,
      *         TYFILE.TY_NO_1_9,
      *         TYFILE.TY_AMT_1_9,
      *         TYFILE.TY_O2_1_9,
      *         TYFILE.TY_INT_1_9,
      *         TYFILE.TY_NO_1_10,
      *         TYFILE.TY_AMT_1_10,
      *         TYFILE.TY_O2_1_10,
      *         TYFILE.TY_INT_1_10,
      *         TYFILE.TY_NO_1_11,
      *         TYFILE.TY_AMT_1_11,
      *         TYFILE.TY_O2_1_11,
      *         TYFILE.TY_INT_1_11,
      *         TYFILE.TY_NO_2_1,
      *         TYFILE.TY_AMT_2_1,
      *         TYFILE.TY_O2_2_1,
      *         TYFILE.TY_INT_2_1,
      *         TYFILE.TY_NO_2_2,
      *         TYFILE.TY_AMT_2_2,
      *         TYFILE.TY_O2_2_2,
      *         TYFILE.TY_INT_2_2,
      *         TYFILE.TY_NO_2_3,
      *         TYFILE.TY_AMT_2_3,
      *         TYFILE.TY_O2_2_3,
      *         TYFILE.TY_INT_2_3,
      *         TYFILE.TY_NO_2_4,
      *         TYFILE.TY_AMT_2_4,
      *         TYFILE.TY_O2_2_4,
      *         TYFILE.TY_INT_2_4,
      *         TYFILE.TY_NO_2_5,
      *         TYFILE.TY_AMT_2_5,
      *         TYFILE.TY_O2_2_5,
      *         TYFILE.TY_INT_2_5,
      *         TYFILE.TY_NO_2_6,
      *         TYFILE.TY_AMT_2_6,
      *         TYFILE.TY_O2_2_6,
      *         TYFILE.TY_INT_2_6,
      *         TYFILE.TY_NO_2_7,
      *         TYFILE.TY_AMT_2_7,
      *         TYFILE.TY_O2_2_7,
      *         TYFILE.TY_INT_2_7,
      *         TYFILE.TY_NO_2_8,
      *         TYFILE.TY_AMT_2_8,
      *         TYFILE.TY_O2_2_8,
      *         TYFILE.TY_INT_2_8,
      *         TYFILE.TY_NO_2_9,
      *         TYFILE.TY_AMT_2_9,
      *         TYFILE.TY_O2_2_9,
      *         TYFILE.TY_INT_2_9,
      *         TYFILE.TY_NO_2_10,
      *         TYFILE.TY_AMT_2_10,
      *         TYFILE.TY_O2_2_10,
      *         TYFILE.TY_INT_2_10,
      *         TYFILE.TY_NO_2_11,
      *         TYFILE.TY_AMT_2_11,
      *         TYFILE.TY_O2_2_11,
      *         TYFILE.TY_INT_2_11,
      *         TYFILE.TY_NO_REPO_1_1,
      *         TYFILE.TY_AMT_REPO_1_1,
      *         TYFILE.TY_O2_REPO_1_1,
      *         TYFILE.TY_INT_REPO_1_1,
      *         TYFILE.TY_NO_REPO_1_2,
      *         TYFILE.TY_AMT_REPO_1_2,
      *         TYFILE.TY_O2_REPO_1_2,
      *         TYFILE.TY_INT_REPO_1_2,
      *         TYFILE.TY_NO_REPO_1_3,
      *         TYFILE.TY_AMT_REPO_1_3,
      *         TYFILE.TY_O2_REPO_1_3,
      *         TYFILE.TY_INT_REPO_1_3,
      *         TYFILE.TY_NO_REPO_1_4,
      *         TYFILE.TY_AMT_REPO_1_4,
      *         TYFILE.TY_O2_REPO_1_4,
      *         TYFILE.TY_INT_REPO_1_4,
      *         TYFILE.TY_NO_REPO_1_5,
      *         TYFILE.TY_AMT_REPO_1_5,
      *         TYFILE.TY_O2_REPO_1_5,
      *         TYFILE.TY_INT_REPO_1_5,
      *         TYFILE.TY_NO_REPO_2_1,
      *         TYFILE.TY_AMT_REPO_2_1,
      *         TYFILE.TY_O2_REPO_2_1,
      *         TYFILE.TY_INT_REPO_2_1,
      *         TYFILE.TY_NO_REPO_2_2,
      *         TYFILE.TY_AMT_REPO_2_2,
      *         TYFILE.TY_O2_REPO_2_2,
      *         TYFILE.TY_INT_REPO_2_2,
      *         TYFILE.TY_NO_REPO_2_3,
      *         TYFILE.TY_AMT_REPO_2_3,
      *         TYFILE.TY_O2_REPO_2_3,
      *         TYFILE.TY_INT_REPO_2_3,
      *         TYFILE.TY_NO_REPO_2_4,
      *         TYFILE.TY_AMT_REPO_2_4,
      *         TYFILE.TY_O2_REPO_2_4,
      *         TYFILE.TY_INT_REPO_2_4,
      *         TYFILE.TY_NO_REPO_2_5,
      *         TYFILE.TY_AMT_REPO_2_5,
      *         TYFILE.TY_O2_REPO_2_5,
      *         TYFILE.TY_INT_REPO_2_5,
      *         TYFILE.TY_NO_PL_1_1,
      *         TYFILE.TY_AMT_PL_1_1,
      *         TYFILE.TY_O2_PL_1_1,
      *         TYFILE.TY_INT_PL_1_1,
      *         TYFILE.TY_NO_PL_1_2,
      *         TYFILE.TY_AMT_PL_1_2,
      *         TYFILE.TY_O2_PL_1_2,
      *         TYFILE.TY_INT_PL_1_2,
      *         TYFILE.TY_NO_PL_1_3,
      *         TYFILE.TY_AMT_PL_1_3,
      *         TYFILE.TY_O2_PL_1_3,
      *         TYFILE.TY_INT_PL_1_3,
      *         TYFILE.TY_NO_PL_1_4,
      *         TYFILE.TY_AMT_PL_1_4,
      *         TYFILE.TY_O2_PL_1_4,
      *         TYFILE.TY_INT_PL_1_4,
      *         TYFILE.TY_NO_PL_1_5,
      *         TYFILE.TY_AMT_PL_1_5,
      *         TYFILE.TY_O2_PL_1_5,
      *         TYFILE.TY_INT_PL_1_5,
      *         TYFILE.TY_NO_PL_1_6,
      *         TYFILE.TY_AMT_PL_1_6,
      *         TYFILE.TY_O2_PL_1_6,
      *         TYFILE.TY_INT_PL_1_6,
      *         TYFILE.TY_NO_PL_1_7,
      *         TYFILE.TY_AMT_PL_1_7,
      *         TYFILE.TY_O2_PL_1_7,
      *         TYFILE.TY_INT_PL_1_7,
      *         TYFILE.TY_NO_PL_1_8,
      *         TYFILE.TY_AMT_PL_1_8,
      *         TYFILE.TY_O2_PL_1_8,
      *         TYFILE.TY_INT_PL_1_8,
      *         TYFILE.TY_NO_PL_1_9,
      *         TYFILE.TY_AMT_PL_1_9,
      *         TYFILE.TY_O2_PL_1_9,
      *         TYFILE.TY_INT_PL_1_9,
      *         TYFILE.TY_NO_PL_1_10,
      *         TYFILE.TY_AMT_PL_1_10,
      *         TYFILE.TY_O2_PL_1_10,
      *         TYFILE.TY_INT_PL_1_10,
      *         TYFILE.TY_NO_PL_2_1,
      *         TYFILE.TY_AMT_PL_2_1,
      *         TYFILE.TY_O2_PL_2_1,
      *         TYFILE.TY_INT_PL_2_1,
      *         TYFILE.TY_NO_PL_2_2,
      *         TYFILE.TY_AMT_PL_2_2,
      *         TYFILE.TY_O2_PL_2_2,
      *         TYFILE.TY_INT_PL_2_2,
      *         TYFILE.TY_NO_PL_2_3,
      *         TYFILE.TY_AMT_PL_2_3,
      *         TYFILE.TY_O2_PL_2_3,
      *         TYFILE.TY_INT_PL_2_3,
      *         TYFILE.TY_NO_PL_2_4,
      *         TYFILE.TY_AMT_PL_2_4,
      *         TYFILE.TY_O2_PL_2_4,
      *         TYFILE.TY_INT_PL_2_4,
      *         TYFILE.TY_NO_PL_2_5,
      *         TYFILE.TY_AMT_PL_2_5,
      *         TYFILE.TY_O2_PL_2_5,
      *         TYFILE.TY_INT_PL_2_5,
      *         TYFILE.TY_NO_PL_2_6,
      *         TYFILE.TY_AMT_PL_2_6,
      *         TYFILE.TY_O2_PL_2_6,
      *         TYFILE.TY_INT_PL_2_6,
      *         TYFILE.TY_NO_PL_2_7,
      *         TYFILE.TY_AMT_PL_2_7,
      *         TYFILE.TY_O2_PL_2_7,
      *         TYFILE.TY_INT_PL_2_7,
      *         TYFILE.TY_NO_PL_2_8,
      *         TYFILE.TY_AMT_PL_2_8,
      *         TYFILE.TY_O2_PL_2_8,
      *         TYFILE.TY_INT_PL_2_8,
      *         TYFILE.TY_NO_PL_2_9,
      *         TYFILE.TY_AMT_PL_2_9,
      *         TYFILE.TY_O2_PL_2_9,
      *         TYFILE.TY_INT_PL_2_9,
      *         TYFILE.TY_NO_PL_2_10,
      *         TYFILE.TY_AMT_PL_2_10,
      *         TYFILE.TY_O2_PL_2_10,
      *         TYFILE.TY_INT_PL_2_10,
      *         TYFILE.TY_DLRVPRIN_1_1,
      *         TYFILE.TY_DLRVPRIN_1_2,
      *         TYFILE.TY_DLRVPRIN_1_3,
      *         TYFILE.TY_DLRVCHGS_1_1,
      *         TYFILE.TY_DLRVCHGS_1_2,
      *         TYFILE.TY_DLRVCHGS_1_3,
      *         TYFILE.TY_DLPARVHELD_1,
      *         TYFILE.TY_DLRVPRIN_2_1,
      *         TYFILE.TY_DLRVPRIN_2_2,
      *         TYFILE.TY_DLRVPRIN_2_3,
      *         TYFILE.TY_DLRVCHGS_2_1,
      *         TYFILE.TY_DLRVCHGS_2_2,
      *         TYFILE.TY_DLRVCHGS_2_3,
      *         TYFILE.TY_DLPARVHELD_2,
      *         TYFILE.TY_PL_PRINCOL_1,
      *         TYFILE.TY_PL_INTCOL_1,
      *         TYFILE.TY_PL_LATECOL_1,
      *         TYFILE.TY_PL_O2COL_1,
      *         TYFILE.TY_PL_PRINCOL_2,
      *         TYFILE.TY_PL_INTCOL_2,
      *         TYFILE.TY_PL_LATECOL_2,
      *         TYFILE.TY_PL_O2COL_2,
      *         TYFILE.TY_PL_PRINCOL_3,
      *         TYFILE.TY_PL_INTCOL_3,
      *         TYFILE.TY_PL_LATECOL_3,
      *         TYFILE.TY_PL_O2COL_3,
      *         TYFILE.TY_PL_PRINCOL_4,
      *         TYFILE.TY_PL_INTCOL_4,
      *         TYFILE.TY_PL_LATECOL_4,
      *         TYFILE.TY_PL_O2COL_4,
      *         TYFILE.TY_PL_PRINCOL_5,
      *         TYFILE.TY_PL_INTCOL_5,
      *         TYFILE.TY_PL_LATECOL_5,
      *         TYFILE.TY_PL_O2COL_5,
      *         TYFILE.TY_AMT_SOLD,
      *         TYFILE.TY_AMT_TRANS,
      *         TYFILE.TY_AMT_PURCH,
      *         TYFILE.TY_NO_SOLD,
      *         TYFILE.TY_NO_TRANS,
      *         TYFILE.TY_NO_PURCH,
      *         TYFILE.TY_OTHCOL,
      *         TYFILE.TY_OT2COL,
      *         TYFILE.TY_INTCOL,
      *         TYFILE.TY_LATECOL,
      *         TYFILE.TY_PRINCOL,
      *         TYFILE.TY_DEFCOL,
      *         TYFILE.TY_RNPRIN,
      *         TYFILE.TY_INTREB,
      *         TYFILE.TY_CLREB1,
      *         TYFILE.TY_CLREB2,
      *         TYFILE.TY_CLREB3,
      *         TYFILE.TY_CLREB4,
      *         TYFILE.TY_AHREB,
      *         TYFILE.TY_PPREB,
      *         TYFILE.TY_O1REB,
      *         TYFILE.TY_O2REB,
      *         TYFILE.TY_O3REB,
      *         TYFILE.TY_O4REB,
      *         TYFILE.TY_O5REB,
      *         TYFILE.TY_O6REB,
      *         TYFILE.TY_O7REB,
      *         TYFILE.TY_SCREB,
      *         TYFILE.TY_MFREB,
      *         TYFILE.TY_DFREB,
      *         TYFILE.TY_UN7_REB,
      *         TYFILE.TY_UN8_REB,
      *         TYFILE.TY_UN9_REB,
      *         TYFILE.TY_UN10_REB,
      *         TYFILE.TY_PART_CHGOFF,
      *         TYFILE.TY_INS_NO_ELIG_1,
      *         TYFILE.TY_INS_NO_WROTE_1,
      *         TYFILE.TY_INS_NO_NOTWROTE_1,
      *         TYFILE.TY_INS_AMT_WROTE_1,
      *         TYFILE.TY_INS_NO_ELIG_2,
      *         TYFILE.TY_INS_NO_WROTE_2,
      *         TYFILE.TY_INS_NO_NOTWROTE_2,
      *         TYFILE.TY_INS_AMT_WROTE_2,
      *         TYFILE.TY_INS_NO_ELIG_3,
      *         TYFILE.TY_INS_NO_WROTE_3,
      *         TYFILE.TY_INS_NO_NOTWROTE_3,
      *         TYFILE.TY_INS_AMT_WROTE_3,
      *         TYFILE.TY_INS_NO_ELIG_4,
      *         TYFILE.TY_INS_NO_WROTE_4,
      *         TYFILE.TY_INS_NO_NOTWROTE_4,
      *         TYFILE.TY_INS_AMT_WROTE_4,
      *         TYFILE.TY_INS_NO_ELIG_5,
      *         TYFILE.TY_INS_NO_WROTE_5,
      *         TYFILE.TY_INS_NO_NOTWROTE_5,
      *         TYFILE.TY_INS_AMT_WROTE_5,
      *         TYFILE.TY_INS_NO_ELIG_6,
      *         TYFILE.TY_INS_NO_WROTE_6,
      *         TYFILE.TY_INS_NO_NOTWROTE_6,
      *         TYFILE.TY_INS_AMT_WROTE_6,
      *         TYFILE.TY_INS_NO_ELIG_7,
      *         TYFILE.TY_INS_NO_WROTE_7,
      *         TYFILE.TY_INS_NO_NOTWROTE_7,
      *         TYFILE.TY_INS_AMT_WROTE_7,
      *         TYFILE.TY_INS_NO_ELIG_8,
      *         TYFILE.TY_INS_NO_WROTE_8,
      *         TYFILE.TY_INS_NO_NOTWROTE_8,
      *         TYFILE.TY_INS_AMT_WROTE_8,
      *         TYFILE.TY_INS_NO_ELIG_9,
      *         TYFILE.TY_INS_NO_WROTE_9,
      *         TYFILE.TY_INS_NO_NOTWROTE_9,
      *         TYFILE.TY_INS_AMT_WROTE_9,
      *         TYFILE.TY_INS_NO_ELIG_10,
      *         TYFILE.TY_INS_NO_WROTE_10,
      *         TYFILE.TY_INS_NO_NOTWROTE_10,
      *         TYFILE.TY_INS_AMT_WROTE_10,
      *         TYFILE.TY_FEE_NO_ELIG_1,
      *         TYFILE.TY_FEE_NO_WROTE_1,
      *         TYFILE.TY_FEE_NO_NOTWROTE_1,
      *         TYFILE.TY_FEE_AMT_WROTE_1,
      *         TYFILE.TY_FEE_NO_ELIG_2,
      *         TYFILE.TY_FEE_NO_WROTE_2,
      *         TYFILE.TY_FEE_NO_NOTWROTE_2,
      *         TYFILE.TY_FEE_AMT_WROTE_2,
      *         TYFILE.TY_FEE_NO_ELIG_3,
      *         TYFILE.TY_FEE_NO_WROTE_3,
      *         TYFILE.TY_FEE_NO_NOTWROTE_3,
      *         TYFILE.TY_FEE_AMT_WROTE_3,
      *         TYFILE.TY_OTH_NO_COL,
      *         TYFILE.TY_OT2_NO_COL,
      *         TYFILE.TY_INT_NO_COL,
      *         TYFILE.TY_LATE_NO_COL,
      *         TYFILE.TY_OTH_NO_WAIVED,
      *         TYFILE.TY_OT2_NO_WAIVED,
      *         TYFILE.TY_INT_NO_WAIVED,
      *         TYFILE.TY_LATE_NO_WAIVED,
      *         TYFILE.TY_OTH_AMT_WAIVED,
      *         TYFILE.TY_OT2_AMT_WAIVED,
      *         TYFILE.TY_INT_AMT_WAIVED,
      *         TYFILE.TY_LATE_AMT_WAIVED,
      *         TYFILE.TY_CKWRITTEN_1,
      *         TYFILE.TY_CKWRITTEN_2,
      *         TYFILE.TY_CKWRITTEN_3,
      *         TYFILE.TY_CKWRITTEN_4,
      *         TYFILE.TY_CKWRITTEN_5,
      *         TYFILE.TY_CKWRITTEN_6,
      *         TYFILE.TY_CKWRITTEN_7,
      *         TYFILE.TY_CKWRITTEN_8,
      *         TYFILE.TY_CKWRITTEN_9,
      *         TYFILE.TY_CKWRITTEN_10,
      *         TYFILE.TY_CKWRITTEN_11,
      *         TYFILE.TY_CKWRITTEN_12,
      *         TYFILE.TY_CKWRITTEN_13,
      *         TYFILE.TY_CKWRITTEN_14,
      *         TYFILE.TY_CKWRITTEN_15,
      *         TYFILE.TY_CKWRITTEN_16,
      *         TYFILE.TY_UNITS_1_1,
      *         TYFILE.TY_OUTBAL_1_1,
      *         TYFILE.TY_UNITS_1_2,
      *         TYFILE.TY_OUTBAL_1_2,
      *         TYFILE.TY_UNITS_1_3,
      *         TYFILE.TY_OUTBAL_1_3,
      *         TYFILE.TY_UNITS_1_4,
      *         TYFILE.TY_OUTBAL_1_4,
      *         TYFILE.TY_UNITS_1_5,
      *         TYFILE.TY_OUTBAL_1_5,
      *         TYFILE.TY_UNITS_1_6,
      *         TYFILE.TY_OUTBAL_1_6,
      *         TYFILE.TY_UNITS_1_7,
      *         TYFILE.TY_OUTBAL_1_7,
      *         TYFILE.TY_UNITS_1_8,
      *         TYFILE.TY_OUTBAL_1_8,
      *         TYFILE.TY_UNITS_1_9,
      *         TYFILE.TY_OUTBAL_1_9,
      *         TYFILE.TY_UNITS_1_10,
      *         TYFILE.TY_OUTBAL_1_10,
      *         TYFILE.TY_UNITS_2_1,
      *         TYFILE.TY_OUTBAL_2_1,
      *         TYFILE.TY_UNITS_2_2,
      *         TYFILE.TY_OUTBAL_2_2,
      *         TYFILE.TY_UNITS_2_3,
      *         TYFILE.TY_OUTBAL_2_3,
      *         TYFILE.TY_UNITS_2_4,
      *         TYFILE.TY_OUTBAL_2_4,
      *         TYFILE.TY_UNITS_2_5,
      *         TYFILE.TY_OUTBAL_2_5,
      *         TYFILE.TY_UNITS_2_6,
      *         TYFILE.TY_OUTBAL_2_6,
      *         TYFILE.TY_UNITS_2_7,
      *         TYFILE.TY_OUTBAL_2_7,
      *         TYFILE.TY_UNITS_2_8,
      *         TYFILE.TY_OUTBAL_2_8,
      *         TYFILE.TY_UNITS_2_9,
      *         TYFILE.TY_OUTBAL_2_9,
      *         TYFILE.TY_UNITS_2_10,
      *         TYFILE.TY_OUTBAL_2_10,
      *         TYFILE.TY_UNITS_BNKRPT_1_1,
      *         TYFILE.TY_OUTBAL_BNKRPT_1_1,
      *         TYFILE.TY_UNITS_BNKRPT_1_2,
      *         TYFILE.TY_OUTBAL_BNKRPT_1_2,
      *         TYFILE.TY_UNITS_BNKRPT_1_3,
      *         TYFILE.TY_OUTBAL_BNKRPT_1_3,
      *         TYFILE.TY_UNITS_BNKRPT_1_4,
      *         TYFILE.TY_OUTBAL_BNKRPT_1_4,
      *         TYFILE.TY_UNITS_BNKRPT_1_5,
      *         TYFILE.TY_OUTBAL_BNKRPT_1_5,
      *         TYFILE.TY_UNITS_BNKRPT_1_6,
      *         TYFILE.TY_OUTBAL_BNKRPT_1_6,
      *         TYFILE.TY_UNITS_BNKRPT_1_7,
      *         TYFILE.TY_OUTBAL_BNKRPT_1_7,
      *         TYFILE.TY_UNITS_BNKRPT_1_8,
      *         TYFILE.TY_OUTBAL_BNKRPT_1_8,
      *         TYFILE.TY_UNITS_BNKRPT_1_9,
      *         TYFILE.TY_OUTBAL_BNKRPT_1_9,
      *         TYFILE.TY_UNITS_BNKRPT_1_10,
      *         TYFILE.TY_OUTBAL_BNKRPT_1_10,
      *         TYFILE.TY_UNITS_BNKRPT_2_1,
      *         TYFILE.TY_OUTBAL_BNKRPT_2_1,
      *         TYFILE.TY_UNITS_BNKRPT_2_2,
      *         TYFILE.TY_OUTBAL_BNKRPT_2_2,
      *         TYFILE.TY_UNITS_BNKRPT_2_3,
      *         TYFILE.TY_OUTBAL_BNKRPT_2_3,
      *         TYFILE.TY_UNITS_BNKRPT_2_4,
      *         TYFILE.TY_OUTBAL_BNKRPT_2_4,
      *         TYFILE.TY_UNITS_BNKRPT_2_5,
      *         TYFILE.TY_OUTBAL_BNKRPT_2_5,
      *         TYFILE.TY_UNITS_BNKRPT_2_6,
      *         TYFILE.TY_OUTBAL_BNKRPT_2_6,
      *         TYFILE.TY_UNITS_BNKRPT_2_7,
      *         TYFILE.TY_OUTBAL_BNKRPT_2_7,
      *         TYFILE.TY_UNITS_BNKRPT_2_8,
      *         TYFILE.TY_OUTBAL_BNKRPT_2_8,
      *         TYFILE.TY_UNITS_BNKRPT_2_9,
      *         TYFILE.TY_OUTBAL_BNKRPT_2_9,
      *         TYFILE.TY_UNITS_BNKRPT_2_10,
      *         TYFILE.TY_OUTBAL_BNKRPT_2_10,
      *         TYFILE.TY_SUMWGTTM,
      *         TYFILE.TY_SUMWGTAV,
      *         TYFILE.TY_SUMWGTTA,
      *         TYFILE.TY_ALLOTMENTS,
      *         TYFILE.TY_BRFG,
      *         TYFILE.TY_DLDISC,
      *         TYFILE.TY_UNEARNED_AMT_1_1,
      *         TYFILE.TY_UNEARNED_AMT_1_2,
      *         TYFILE.TY_UNEARNED_AMT_1_3,
      *         TYFILE.TY_UNEARNED_AMT_1_4,
      *         TYFILE.TY_UNEARNED_AMT_1_5,
      *         TYFILE.TY_UNEARNED_AMT_1_6,
      *         TYFILE.TY_UNEARNED_AMT_1_7,
      *         TYFILE.TY_UNEARNED_AMT_1_8,
      *         TYFILE.TY_UNEARNED_AMT_1_9,
      *         TYFILE.TY_UNEARNED_AMT_1_10,
      *         TYFILE.TY_UNEARNED_AMT_2_1,
      *         TYFILE.TY_UNEARNED_AMT_2_2,
      *         TYFILE.TY_UNEARNED_AMT_2_3,
      *         TYFILE.TY_UNEARNED_AMT_2_4,
      *         TYFILE.TY_UNEARNED_AMT_2_5,
      *         TYFILE.TY_UNEARNED_AMT_2_6,
      *         TYFILE.TY_UNEARNED_AMT_2_7,
      *         TYFILE.TY_UNEARNED_AMT_2_8,
      *         TYFILE.TY_UNEARNED_AMT_2_9,
      *         TYFILE.TY_UNEARNED_AMT_2_10,
      *         TYFILE.TY_PROCEEDS,
      *         TYFILE.TY_LOCKBOX
      *
      *    INTO #TEMP_LPEOD5_TYFILE
      *
      *    FROM DBO.TYFILE
      *
      *   WHERE TYFILE.TY_BRNO        = @QTY1_WBEG_BRNO
      *     AND TYFILE.TY_DATE  BETWEEN @QTY1_WBEG_DATE 
      *                             AND @QTY1_WEND_DATE
      *     AND TYFILE.TY_CLASS BETWEEN @QTY1_WBEG_CLASS
      *                             AND @QTY1_WEND_CLASS; 


           EXEC SQL SET CONNECTION TY1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE TY1_CURSOR CURSOR FOR
                :TY1-RETURN-CODE = EXEC SSP_LPEOD5_TYFILE_SELECT 
              ( :QTY1-WBEG-BRNO, 
                :QTY1-WBEG-DATE,  
                :QTY1-WEND-DATE, 
                :QTY1-WBEG-CLASS,
                :QTY1-WEND-CLASS )
           END-EXEC.

           EXEC SQL
               OPEN TY1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTY1-WBEG-BRNO
                          QTY1-WEND-BRNO
                          QTY1-WBEG-CLASS
                          QTY1-WEND-CLASS.

           MOVE SPACES TO QTY1-WBEG-DATE
                          QTY1-WEND-DATE.

           MOVE STAT---GOOD TO TY1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-TY1-FILE.
           PERFORM READ-IT.
           MOVE TY-PATH-OWNBR  TO TY-BRNO. 
           MOVE TY-PATH-SQL    TO E-FILE.
           MOVE TY1-KEY        TO E-KEYX.
           INITIALIZE QTY-REC. 

           MOVE TY-BRNO             TO QTY-BRNO.
           MOVE TY-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTY-DATE.
           MOVE TY-CLASS           TO QTY-CLASS.

           EXEC SQL
            SELECT TYFILE.TY_BRNO,
                   CAST(TYFILE.TY_DATE AS VARCHAR(10)),
                   TYFILE.TY_CLASS,
                   CAST(TYFILE.TY_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   TYFILE.TY_NO_1_1,
                   TYFILE.TY_AMT_1_1,
                   TYFILE.TY_O2_1_1,
                   TYFILE.TY_INT_1_1,
                   TYFILE.TY_NO_1_2,
                   TYFILE.TY_AMT_1_2,
                   TYFILE.TY_O2_1_2,
                   TYFILE.TY_INT_1_2,
                   TYFILE.TY_NO_1_3,
                   TYFILE.TY_AMT_1_3,
                   TYFILE.TY_O2_1_3,
                   TYFILE.TY_INT_1_3,
                   TYFILE.TY_NO_1_4,
                   TYFILE.TY_AMT_1_4,
                   TYFILE.TY_O2_1_4,
                   TYFILE.TY_INT_1_4,
                   TYFILE.TY_NO_1_5,
                   TYFILE.TY_AMT_1_5,
                   TYFILE.TY_O2_1_5,
                   TYFILE.TY_INT_1_5,
                   TYFILE.TY_NO_1_6,
                   TYFILE.TY_AMT_1_6,
                   TYFILE.TY_O2_1_6,
                   TYFILE.TY_INT_1_6,
                   TYFILE.TY_NO_1_7,
                   TYFILE.TY_AMT_1_7,
                   TYFILE.TY_O2_1_7,
                   TYFILE.TY_INT_1_7,
                   TYFILE.TY_NO_1_8,
                   TYFILE.TY_AMT_1_8,
                   TYFILE.TY_O2_1_8,
                   TYFILE.TY_INT_1_8,
                   TYFILE.TY_NO_1_9,
                   TYFILE.TY_AMT_1_9,
                   TYFILE.TY_O2_1_9,
                   TYFILE.TY_INT_1_9,
                   TYFILE.TY_NO_1_10,
                   TYFILE.TY_AMT_1_10,
                   TYFILE.TY_O2_1_10,
                   TYFILE.TY_INT_1_10,
                   TYFILE.TY_NO_1_11,
                   TYFILE.TY_AMT_1_11,
                   TYFILE.TY_O2_1_11,
                   TYFILE.TY_INT_1_11,
                   TYFILE.TY_NO_2_1,
                   TYFILE.TY_AMT_2_1,
                   TYFILE.TY_O2_2_1,
                   TYFILE.TY_INT_2_1,
                   TYFILE.TY_NO_2_2,
                   TYFILE.TY_AMT_2_2,
                   TYFILE.TY_O2_2_2,
                   TYFILE.TY_INT_2_2,
                   TYFILE.TY_NO_2_3,
                   TYFILE.TY_AMT_2_3,
                   TYFILE.TY_O2_2_3,
                   TYFILE.TY_INT_2_3,
                   TYFILE.TY_NO_2_4,
                   TYFILE.TY_AMT_2_4,
                   TYFILE.TY_O2_2_4,
                   TYFILE.TY_INT_2_4,
                   TYFILE.TY_NO_2_5,
                   TYFILE.TY_AMT_2_5,
                   TYFILE.TY_O2_2_5,
                   TYFILE.TY_INT_2_5,
                   TYFILE.TY_NO_2_6,
                   TYFILE.TY_AMT_2_6,
                   TYFILE.TY_O2_2_6,
                   TYFILE.TY_INT_2_6,
                   TYFILE.TY_NO_2_7,
                   TYFILE.TY_AMT_2_7,
                   TYFILE.TY_O2_2_7,
                   TYFILE.TY_INT_2_7,
                   TYFILE.TY_NO_2_8,
                   TYFILE.TY_AMT_2_8,
                   TYFILE.TY_O2_2_8,
                   TYFILE.TY_INT_2_8,
                   TYFILE.TY_NO_2_9,
                   TYFILE.TY_AMT_2_9,
                   TYFILE.TY_O2_2_9,
                   TYFILE.TY_INT_2_9,
                   TYFILE.TY_NO_2_10,
                   TYFILE.TY_AMT_2_10,
                   TYFILE.TY_O2_2_10,
                   TYFILE.TY_INT_2_10,
                   TYFILE.TY_NO_2_11,
                   TYFILE.TY_AMT_2_11,
                   TYFILE.TY_O2_2_11,
                   TYFILE.TY_INT_2_11,
                   TYFILE.TY_NO_REPO_1_1,
                   TYFILE.TY_AMT_REPO_1_1,
                   TYFILE.TY_O2_REPO_1_1,
                   TYFILE.TY_INT_REPO_1_1,
                   TYFILE.TY_NO_REPO_1_2,
                   TYFILE.TY_AMT_REPO_1_2,
                   TYFILE.TY_O2_REPO_1_2,
                   TYFILE.TY_INT_REPO_1_2,
                   TYFILE.TY_NO_REPO_1_3,
                   TYFILE.TY_AMT_REPO_1_3,
                   TYFILE.TY_O2_REPO_1_3,
                   TYFILE.TY_INT_REPO_1_3,
                   TYFILE.TY_NO_REPO_1_4,
                   TYFILE.TY_AMT_REPO_1_4,
                   TYFILE.TY_O2_REPO_1_4,
                   TYFILE.TY_INT_REPO_1_4,
                   TYFILE.TY_NO_REPO_1_5,
                   TYFILE.TY_AMT_REPO_1_5,
                   TYFILE.TY_O2_REPO_1_5,
                   TYFILE.TY_INT_REPO_1_5,
                   TYFILE.TY_NO_REPO_2_1,
                   TYFILE.TY_AMT_REPO_2_1,
                   TYFILE.TY_O2_REPO_2_1,
                   TYFILE.TY_INT_REPO_2_1,
                   TYFILE.TY_NO_REPO_2_2,
                   TYFILE.TY_AMT_REPO_2_2,
                   TYFILE.TY_O2_REPO_2_2,
                   TYFILE.TY_INT_REPO_2_2,
                   TYFILE.TY_NO_REPO_2_3,
                   TYFILE.TY_AMT_REPO_2_3,
                   TYFILE.TY_O2_REPO_2_3,
                   TYFILE.TY_INT_REPO_2_3,
                   TYFILE.TY_NO_REPO_2_4,
                   TYFILE.TY_AMT_REPO_2_4,
                   TYFILE.TY_O2_REPO_2_4,
                   TYFILE.TY_INT_REPO_2_4,
                   TYFILE.TY_NO_REPO_2_5,
                   TYFILE.TY_AMT_REPO_2_5,
                   TYFILE.TY_O2_REPO_2_5,
                   TYFILE.TY_INT_REPO_2_5,
                   TYFILE.TY_NO_PL_1_1,
                   TYFILE.TY_AMT_PL_1_1,
                   TYFILE.TY_O2_PL_1_1,
                   TYFILE.TY_INT_PL_1_1,
                   TYFILE.TY_NO_PL_1_2,
                   TYFILE.TY_AMT_PL_1_2,
                   TYFILE.TY_O2_PL_1_2,
                   TYFILE.TY_INT_PL_1_2,
                   TYFILE.TY_NO_PL_1_3,
                   TYFILE.TY_AMT_PL_1_3,
                   TYFILE.TY_O2_PL_1_3,
                   TYFILE.TY_INT_PL_1_3,
                   TYFILE.TY_NO_PL_1_4,
                   TYFILE.TY_AMT_PL_1_4,
                   TYFILE.TY_O2_PL_1_4,
                   TYFILE.TY_INT_PL_1_4,
                   TYFILE.TY_NO_PL_1_5,
                   TYFILE.TY_AMT_PL_1_5,
                   TYFILE.TY_O2_PL_1_5,
                   TYFILE.TY_INT_PL_1_5,
                   TYFILE.TY_NO_PL_1_6,
                   TYFILE.TY_AMT_PL_1_6,
                   TYFILE.TY_O2_PL_1_6,
                   TYFILE.TY_INT_PL_1_6,
                   TYFILE.TY_NO_PL_1_7,
                   TYFILE.TY_AMT_PL_1_7,
                   TYFILE.TY_O2_PL_1_7,
                   TYFILE.TY_INT_PL_1_7,
                   TYFILE.TY_NO_PL_1_8,
                   TYFILE.TY_AMT_PL_1_8,
                   TYFILE.TY_O2_PL_1_8,
                   TYFILE.TY_INT_PL_1_8,
                   TYFILE.TY_NO_PL_1_9,
                   TYFILE.TY_AMT_PL_1_9,
                   TYFILE.TY_O2_PL_1_9,
                   TYFILE.TY_INT_PL_1_9,
                   TYFILE.TY_NO_PL_1_10,
                   TYFILE.TY_AMT_PL_1_10,
                   TYFILE.TY_O2_PL_1_10,
                   TYFILE.TY_INT_PL_1_10,
                   TYFILE.TY_NO_PL_2_1,
                   TYFILE.TY_AMT_PL_2_1,
                   TYFILE.TY_O2_PL_2_1,
                   TYFILE.TY_INT_PL_2_1,
                   TYFILE.TY_NO_PL_2_2,
                   TYFILE.TY_AMT_PL_2_2,
                   TYFILE.TY_O2_PL_2_2,
                   TYFILE.TY_INT_PL_2_2,
                   TYFILE.TY_NO_PL_2_3,
                   TYFILE.TY_AMT_PL_2_3,
                   TYFILE.TY_O2_PL_2_3,
                   TYFILE.TY_INT_PL_2_3,
                   TYFILE.TY_NO_PL_2_4,
                   TYFILE.TY_AMT_PL_2_4,
                   TYFILE.TY_O2_PL_2_4,
                   TYFILE.TY_INT_PL_2_4,
                   TYFILE.TY_NO_PL_2_5,
                   TYFILE.TY_AMT_PL_2_5,
                   TYFILE.TY_O2_PL_2_5,
                   TYFILE.TY_INT_PL_2_5,
                   TYFILE.TY_NO_PL_2_6,
                   TYFILE.TY_AMT_PL_2_6,
                   TYFILE.TY_O2_PL_2_6,
                   TYFILE.TY_INT_PL_2_6,
                   TYFILE.TY_NO_PL_2_7,
                   TYFILE.TY_AMT_PL_2_7,
                   TYFILE.TY_O2_PL_2_7,
                   TYFILE.TY_INT_PL_2_7,
                   TYFILE.TY_NO_PL_2_8,
                   TYFILE.TY_AMT_PL_2_8,
                   TYFILE.TY_O2_PL_2_8,
                   TYFILE.TY_INT_PL_2_8,
                   TYFILE.TY_NO_PL_2_9,
                   TYFILE.TY_AMT_PL_2_9,
                   TYFILE.TY_O2_PL_2_9,
                   TYFILE.TY_INT_PL_2_9,
                   TYFILE.TY_NO_PL_2_10,
                   TYFILE.TY_AMT_PL_2_10,
                   TYFILE.TY_O2_PL_2_10,
                   TYFILE.TY_INT_PL_2_10,
                   TYFILE.TY_DLRVPRIN_1_1,
                   TYFILE.TY_DLRVPRIN_1_2,
                   TYFILE.TY_DLRVPRIN_1_3,
                   TYFILE.TY_DLRVCHGS_1_1,
                   TYFILE.TY_DLRVCHGS_1_2,
                   TYFILE.TY_DLRVCHGS_1_3,
                   TYFILE.TY_DLPARVHELD_1,
                   TYFILE.TY_DLRVPRIN_2_1,
                   TYFILE.TY_DLRVPRIN_2_2,
                   TYFILE.TY_DLRVPRIN_2_3,
                   TYFILE.TY_DLRVCHGS_2_1,
                   TYFILE.TY_DLRVCHGS_2_2,
                   TYFILE.TY_DLRVCHGS_2_3,
                   TYFILE.TY_DLPARVHELD_2,
                   TYFILE.TY_PL_PRINCOL_1,
                   TYFILE.TY_PL_INTCOL_1,
                   TYFILE.TY_PL_LATECOL_1,
                   TYFILE.TY_PL_O2COL_1,
                   TYFILE.TY_PL_PRINCOL_2,
                   TYFILE.TY_PL_INTCOL_2,
                   TYFILE.TY_PL_LATECOL_2,
                   TYFILE.TY_PL_O2COL_2,
                   TYFILE.TY_PL_PRINCOL_3,
                   TYFILE.TY_PL_INTCOL_3,
                   TYFILE.TY_PL_LATECOL_3,
                   TYFILE.TY_PL_O2COL_3,
                   TYFILE.TY_PL_PRINCOL_4,
                   TYFILE.TY_PL_INTCOL_4,
                   TYFILE.TY_PL_LATECOL_4,
                   TYFILE.TY_PL_O2COL_4,
                   TYFILE.TY_PL_PRINCOL_5,
                   TYFILE.TY_PL_INTCOL_5,
                   TYFILE.TY_PL_LATECOL_5,
                   TYFILE.TY_PL_O2COL_5,
                   TYFILE.TY_AMT_SOLD,
                   TYFILE.TY_AMT_TRANS,
                   TYFILE.TY_AMT_PURCH,
                   TYFILE.TY_NO_SOLD,
                   TYFILE.TY_NO_TRANS,
                   TYFILE.TY_NO_PURCH,
                   TYFILE.TY_OTHCOL,
                   TYFILE.TY_OT2COL,
                   TYFILE.TY_INTCOL,
                   TYFILE.TY_LATECOL,
                   TYFILE.TY_PRINCOL,
                   TYFILE.TY_DEFCOL,
                   TYFILE.TY_RNPRIN,
                   TYFILE.TY_INTREB,
                   TYFILE.TY_CLREB1,
                   TYFILE.TY_CLREB2,
                   TYFILE.TY_CLREB3,
                   TYFILE.TY_CLREB4,
                   TYFILE.TY_AHREB,
                   TYFILE.TY_PPREB,
                   TYFILE.TY_O1REB,
                   TYFILE.TY_O2REB,
                   TYFILE.TY_O3REB,
                   TYFILE.TY_O4REB,
                   TYFILE.TY_O5REB,
                   TYFILE.TY_O6REB,
                   TYFILE.TY_O7REB,
                   TYFILE.TY_SCREB,
                   TYFILE.TY_MFREB,
                   TYFILE.TY_DFREB,
                   TYFILE.TY_UN7_REB,
                   TYFILE.TY_UN8_REB,
                   TYFILE.TY_UN9_REB,
                   TYFILE.TY_UN10_REB,
                   TYFILE.TY_PART_CHGOFF,
                   TYFILE.TY_INS_NO_ELIG_1,
                   TYFILE.TY_INS_NO_WROTE_1,
                   TYFILE.TY_INS_NO_NOTWROTE_1,
                   TYFILE.TY_INS_AMT_WROTE_1,
                   TYFILE.TY_INS_NO_ELIG_2,
                   TYFILE.TY_INS_NO_WROTE_2,
                   TYFILE.TY_INS_NO_NOTWROTE_2,
                   TYFILE.TY_INS_AMT_WROTE_2,
                   TYFILE.TY_INS_NO_ELIG_3,
                   TYFILE.TY_INS_NO_WROTE_3,
                   TYFILE.TY_INS_NO_NOTWROTE_3,
                   TYFILE.TY_INS_AMT_WROTE_3,
                   TYFILE.TY_INS_NO_ELIG_4,
                   TYFILE.TY_INS_NO_WROTE_4,
                   TYFILE.TY_INS_NO_NOTWROTE_4,
                   TYFILE.TY_INS_AMT_WROTE_4,
                   TYFILE.TY_INS_NO_ELIG_5,
                   TYFILE.TY_INS_NO_WROTE_5,
                   TYFILE.TY_INS_NO_NOTWROTE_5,
                   TYFILE.TY_INS_AMT_WROTE_5,
                   TYFILE.TY_INS_NO_ELIG_6,
                   TYFILE.TY_INS_NO_WROTE_6,
                   TYFILE.TY_INS_NO_NOTWROTE_6,
                   TYFILE.TY_INS_AMT_WROTE_6,
                   TYFILE.TY_INS_NO_ELIG_7,
                   TYFILE.TY_INS_NO_WROTE_7,
                   TYFILE.TY_INS_NO_NOTWROTE_7,
                   TYFILE.TY_INS_AMT_WROTE_7,
                   TYFILE.TY_INS_NO_ELIG_8,
                   TYFILE.TY_INS_NO_WROTE_8,
                   TYFILE.TY_INS_NO_NOTWROTE_8,
                   TYFILE.TY_INS_AMT_WROTE_8,
                   TYFILE.TY_INS_NO_ELIG_9,
                   TYFILE.TY_INS_NO_WROTE_9,
                   TYFILE.TY_INS_NO_NOTWROTE_9,
                   TYFILE.TY_INS_AMT_WROTE_9,
                   TYFILE.TY_INS_NO_ELIG_10,
                   TYFILE.TY_INS_NO_WROTE_10,
                   TYFILE.TY_INS_NO_NOTWROTE_10,
                   TYFILE.TY_INS_AMT_WROTE_10,
                   TYFILE.TY_FEE_NO_ELIG_1,
                   TYFILE.TY_FEE_NO_WROTE_1,
                   TYFILE.TY_FEE_NO_NOTWROTE_1,
                   TYFILE.TY_FEE_AMT_WROTE_1,
                   TYFILE.TY_FEE_NO_ELIG_2,
                   TYFILE.TY_FEE_NO_WROTE_2,
                   TYFILE.TY_FEE_NO_NOTWROTE_2,
                   TYFILE.TY_FEE_AMT_WROTE_2,
                   TYFILE.TY_FEE_NO_ELIG_3,
                   TYFILE.TY_FEE_NO_WROTE_3,
                   TYFILE.TY_FEE_NO_NOTWROTE_3,
                   TYFILE.TY_FEE_AMT_WROTE_3,
                   TYFILE.TY_OTH_NO_COL,
                   TYFILE.TY_OT2_NO_COL,
                   TYFILE.TY_INT_NO_COL,
                   TYFILE.TY_LATE_NO_COL,
                   TYFILE.TY_OTH_NO_WAIVED,
                   TYFILE.TY_OT2_NO_WAIVED,
                   TYFILE.TY_INT_NO_WAIVED,
                   TYFILE.TY_LATE_NO_WAIVED,
                   TYFILE.TY_OTH_AMT_WAIVED,
                   TYFILE.TY_OT2_AMT_WAIVED,
                   TYFILE.TY_INT_AMT_WAIVED,
                   TYFILE.TY_LATE_AMT_WAIVED,
                   TYFILE.TY_CKWRITTEN_1,
                   TYFILE.TY_CKWRITTEN_2,
                   TYFILE.TY_CKWRITTEN_3,
                   TYFILE.TY_CKWRITTEN_4,
                   TYFILE.TY_CKWRITTEN_5,
                   TYFILE.TY_CKWRITTEN_6,
                   TYFILE.TY_CKWRITTEN_7,
                   TYFILE.TY_CKWRITTEN_8,
                   TYFILE.TY_CKWRITTEN_9,
                   TYFILE.TY_CKWRITTEN_10,
                   TYFILE.TY_CKWRITTEN_11,
                   TYFILE.TY_CKWRITTEN_12,
                   TYFILE.TY_CKWRITTEN_13,
                   TYFILE.TY_CKWRITTEN_14,
                   TYFILE.TY_CKWRITTEN_15,
                   TYFILE.TY_CKWRITTEN_16,
                   TYFILE.TY_UNITS_1_1,
                   TYFILE.TY_OUTBAL_1_1,
                   TYFILE.TY_UNITS_1_2,
                   TYFILE.TY_OUTBAL_1_2,
                   TYFILE.TY_UNITS_1_3,
                   TYFILE.TY_OUTBAL_1_3,
                   TYFILE.TY_UNITS_1_4,
                   TYFILE.TY_OUTBAL_1_4,
                   TYFILE.TY_UNITS_1_5,
                   TYFILE.TY_OUTBAL_1_5,
                   TYFILE.TY_UNITS_1_6,
                   TYFILE.TY_OUTBAL_1_6,
                   TYFILE.TY_UNITS_1_7,
                   TYFILE.TY_OUTBAL_1_7,
                   TYFILE.TY_UNITS_1_8,
                   TYFILE.TY_OUTBAL_1_8,
                   TYFILE.TY_UNITS_1_9,
                   TYFILE.TY_OUTBAL_1_9,
                   TYFILE.TY_UNITS_1_10,
                   TYFILE.TY_OUTBAL_1_10,
                   TYFILE.TY_UNITS_2_1,
                   TYFILE.TY_OUTBAL_2_1,
                   TYFILE.TY_UNITS_2_2,
                   TYFILE.TY_OUTBAL_2_2,
                   TYFILE.TY_UNITS_2_3,
                   TYFILE.TY_OUTBAL_2_3,
                   TYFILE.TY_UNITS_2_4,
                   TYFILE.TY_OUTBAL_2_4,
                   TYFILE.TY_UNITS_2_5,
                   TYFILE.TY_OUTBAL_2_5,
                   TYFILE.TY_UNITS_2_6,
                   TYFILE.TY_OUTBAL_2_6,
                   TYFILE.TY_UNITS_2_7,
                   TYFILE.TY_OUTBAL_2_7,
                   TYFILE.TY_UNITS_2_8,
                   TYFILE.TY_OUTBAL_2_8,
                   TYFILE.TY_UNITS_2_9,
                   TYFILE.TY_OUTBAL_2_9,
                   TYFILE.TY_UNITS_2_10,
                   TYFILE.TY_OUTBAL_2_10,
                   TYFILE.TY_UNITS_BNKRPT_1_1,
                   TYFILE.TY_OUTBAL_BNKRPT_1_1,
                   TYFILE.TY_UNITS_BNKRPT_1_2,
                   TYFILE.TY_OUTBAL_BNKRPT_1_2,
                   TYFILE.TY_UNITS_BNKRPT_1_3,
                   TYFILE.TY_OUTBAL_BNKRPT_1_3,
                   TYFILE.TY_UNITS_BNKRPT_1_4,
                   TYFILE.TY_OUTBAL_BNKRPT_1_4,
                   TYFILE.TY_UNITS_BNKRPT_1_5,
                   TYFILE.TY_OUTBAL_BNKRPT_1_5,
                   TYFILE.TY_UNITS_BNKRPT_1_6,
                   TYFILE.TY_OUTBAL_BNKRPT_1_6,
                   TYFILE.TY_UNITS_BNKRPT_1_7,
                   TYFILE.TY_OUTBAL_BNKRPT_1_7,
                   TYFILE.TY_UNITS_BNKRPT_1_8,
                   TYFILE.TY_OUTBAL_BNKRPT_1_8,
                   TYFILE.TY_UNITS_BNKRPT_1_9,
                   TYFILE.TY_OUTBAL_BNKRPT_1_9,
                   TYFILE.TY_UNITS_BNKRPT_1_10,
                   TYFILE.TY_OUTBAL_BNKRPT_1_10,
                   TYFILE.TY_UNITS_BNKRPT_2_1,
                   TYFILE.TY_OUTBAL_BNKRPT_2_1,
                   TYFILE.TY_UNITS_BNKRPT_2_2,
                   TYFILE.TY_OUTBAL_BNKRPT_2_2,
                   TYFILE.TY_UNITS_BNKRPT_2_3,
                   TYFILE.TY_OUTBAL_BNKRPT_2_3,
                   TYFILE.TY_UNITS_BNKRPT_2_4,
                   TYFILE.TY_OUTBAL_BNKRPT_2_4,
                   TYFILE.TY_UNITS_BNKRPT_2_5,
                   TYFILE.TY_OUTBAL_BNKRPT_2_5,
                   TYFILE.TY_UNITS_BNKRPT_2_6,
                   TYFILE.TY_OUTBAL_BNKRPT_2_6,
                   TYFILE.TY_UNITS_BNKRPT_2_7,
                   TYFILE.TY_OUTBAL_BNKRPT_2_7,
                   TYFILE.TY_UNITS_BNKRPT_2_8,
                   TYFILE.TY_OUTBAL_BNKRPT_2_8,
                   TYFILE.TY_UNITS_BNKRPT_2_9,
                   TYFILE.TY_OUTBAL_BNKRPT_2_9,
                   TYFILE.TY_UNITS_BNKRPT_2_10,
                   TYFILE.TY_OUTBAL_BNKRPT_2_10,
                   TYFILE.TY_SUMWGTTM,
                   TYFILE.TY_SUMWGTAV,
                   TYFILE.TY_SUMWGTTA,
                   TYFILE.TY_ALLOTMENTS,
                   TYFILE.TY_BRFG,
                   TYFILE.TY_DLDISC,
                   TYFILE.TY_UNEARNED_AMT_1_1,
                   TYFILE.TY_UNEARNED_AMT_1_2,
                   TYFILE.TY_UNEARNED_AMT_1_3,
                   TYFILE.TY_UNEARNED_AMT_1_4,
                   TYFILE.TY_UNEARNED_AMT_1_5,
                   TYFILE.TY_UNEARNED_AMT_1_6,
                   TYFILE.TY_UNEARNED_AMT_1_7,
                   TYFILE.TY_UNEARNED_AMT_1_8,
                   TYFILE.TY_UNEARNED_AMT_1_9,
                   TYFILE.TY_UNEARNED_AMT_1_10,
                   TYFILE.TY_UNEARNED_AMT_2_1,
                   TYFILE.TY_UNEARNED_AMT_2_2,
                   TYFILE.TY_UNEARNED_AMT_2_3,
                   TYFILE.TY_UNEARNED_AMT_2_4,
                   TYFILE.TY_UNEARNED_AMT_2_5,
                   TYFILE.TY_UNEARNED_AMT_2_6,
                   TYFILE.TY_UNEARNED_AMT_2_7,
                   TYFILE.TY_UNEARNED_AMT_2_8,
                   TYFILE.TY_UNEARNED_AMT_2_9,
                   TYFILE.TY_UNEARNED_AMT_2_10,
                   TYFILE.TY_PROCEEDS,
                   TYFILE.TY_LOCKBOX
            INTO :QTY-REC
            FROM DBO.TYFILE
            WHERE TYFILE.TY_BRNO = :QTY-BRNO
              AND TYFILE.TY_DATE = :QTY-DATE
              AND TYFILE.TY_CLASS = :QTY-CLASS
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TY1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-TY-FIELDS.

      *-----------------------------------------------------------------
       READ-TY1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE TY-PATH-OWNBR  TO TY-BRNO.
           MOVE TY-PATH-SQL    TO E-FILE.
           MOVE TY1-KEY        TO E-KEYX.
           INITIALIZE QTY-REC.

           EXEC SQL SET CONNECTION TY1_CONNECT END-EXEC.

           IF ( TY1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TY1_CURSOR INTO :QTY-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TY1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TY-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       WRITE-TY1-FILE. 

           MOVE TY-PATH-OWNBR  TO TY-BRNO.
           ACCEPT TY-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE TY-PATH-SQL    TO E-FILE.
           MOVE TY1-KEY        TO E-KEYX.
           PERFORM SET-TY-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            INSERT INTO DBO.TYFILE
             (TYFILE.TY_BRNO,
              TYFILE.TY_DATE,
              TYFILE.TY_CLASS,
              TYFILE.TY_LTOUCH_DATE,
              TYFILE.TY_NO_1_1,
              TYFILE.TY_AMT_1_1,
              TYFILE.TY_O2_1_1,
              TYFILE.TY_INT_1_1,
              TYFILE.TY_NO_1_2,
              TYFILE.TY_AMT_1_2,
              TYFILE.TY_O2_1_2,
              TYFILE.TY_INT_1_2,
              TYFILE.TY_NO_1_3,
              TYFILE.TY_AMT_1_3,
              TYFILE.TY_O2_1_3,
              TYFILE.TY_INT_1_3,
              TYFILE.TY_NO_1_4,
              TYFILE.TY_AMT_1_4,
              TYFILE.TY_O2_1_4,
              TYFILE.TY_INT_1_4,
              TYFILE.TY_NO_1_5,
              TYFILE.TY_AMT_1_5,
              TYFILE.TY_O2_1_5,
              TYFILE.TY_INT_1_5,
              TYFILE.TY_NO_1_6,
              TYFILE.TY_AMT_1_6,
              TYFILE.TY_O2_1_6,
              TYFILE.TY_INT_1_6,
              TYFILE.TY_NO_1_7,
              TYFILE.TY_AMT_1_7,
              TYFILE.TY_O2_1_7,
              TYFILE.TY_INT_1_7,
              TYFILE.TY_NO_1_8,
              TYFILE.TY_AMT_1_8,
              TYFILE.TY_O2_1_8,
              TYFILE.TY_INT_1_8,
              TYFILE.TY_NO_1_9,
              TYFILE.TY_AMT_1_9,
              TYFILE.TY_O2_1_9,
              TYFILE.TY_INT_1_9,
              TYFILE.TY_NO_1_10,
              TYFILE.TY_AMT_1_10,
              TYFILE.TY_O2_1_10,
              TYFILE.TY_INT_1_10,
              TYFILE.TY_NO_1_11,
              TYFILE.TY_AMT_1_11,
              TYFILE.TY_O2_1_11,
              TYFILE.TY_INT_1_11,
              TYFILE.TY_NO_2_1,
              TYFILE.TY_AMT_2_1,
              TYFILE.TY_O2_2_1,
              TYFILE.TY_INT_2_1,
              TYFILE.TY_NO_2_2,
              TYFILE.TY_AMT_2_2,
              TYFILE.TY_O2_2_2,
              TYFILE.TY_INT_2_2,
              TYFILE.TY_NO_2_3,
              TYFILE.TY_AMT_2_3,
              TYFILE.TY_O2_2_3,
              TYFILE.TY_INT_2_3,
              TYFILE.TY_NO_2_4,
              TYFILE.TY_AMT_2_4,
              TYFILE.TY_O2_2_4,
              TYFILE.TY_INT_2_4,
              TYFILE.TY_NO_2_5,
              TYFILE.TY_AMT_2_5,
              TYFILE.TY_O2_2_5,
              TYFILE.TY_INT_2_5,
              TYFILE.TY_NO_2_6,
              TYFILE.TY_AMT_2_6,
              TYFILE.TY_O2_2_6,
              TYFILE.TY_INT_2_6,
              TYFILE.TY_NO_2_7,
              TYFILE.TY_AMT_2_7,
              TYFILE.TY_O2_2_7,
              TYFILE.TY_INT_2_7,
              TYFILE.TY_NO_2_8,
              TYFILE.TY_AMT_2_8,
              TYFILE.TY_O2_2_8,
              TYFILE.TY_INT_2_8,
              TYFILE.TY_NO_2_9,
              TYFILE.TY_AMT_2_9,
              TYFILE.TY_O2_2_9,
              TYFILE.TY_INT_2_9,
              TYFILE.TY_NO_2_10,
              TYFILE.TY_AMT_2_10,
              TYFILE.TY_O2_2_10,
              TYFILE.TY_INT_2_10,
              TYFILE.TY_NO_2_11,
              TYFILE.TY_AMT_2_11,
              TYFILE.TY_O2_2_11,
              TYFILE.TY_INT_2_11,
              TYFILE.TY_NO_REPO_1_1,
              TYFILE.TY_AMT_REPO_1_1,
              TYFILE.TY_O2_REPO_1_1,
              TYFILE.TY_INT_REPO_1_1,
              TYFILE.TY_NO_REPO_1_2,
              TYFILE.TY_AMT_REPO_1_2,
              TYFILE.TY_O2_REPO_1_2,
              TYFILE.TY_INT_REPO_1_2,
              TYFILE.TY_NO_REPO_1_3,
              TYFILE.TY_AMT_REPO_1_3,
              TYFILE.TY_O2_REPO_1_3,
              TYFILE.TY_INT_REPO_1_3,
              TYFILE.TY_NO_REPO_1_4,
              TYFILE.TY_AMT_REPO_1_4,
              TYFILE.TY_O2_REPO_1_4,
              TYFILE.TY_INT_REPO_1_4,
              TYFILE.TY_NO_REPO_1_5,
              TYFILE.TY_AMT_REPO_1_5,
              TYFILE.TY_O2_REPO_1_5,
              TYFILE.TY_INT_REPO_1_5,
              TYFILE.TY_NO_REPO_2_1,
              TYFILE.TY_AMT_REPO_2_1,
              TYFILE.TY_O2_REPO_2_1,
              TYFILE.TY_INT_REPO_2_1,
              TYFILE.TY_NO_REPO_2_2,
              TYFILE.TY_AMT_REPO_2_2,
              TYFILE.TY_O2_REPO_2_2,
              TYFILE.TY_INT_REPO_2_2,
              TYFILE.TY_NO_REPO_2_3,
              TYFILE.TY_AMT_REPO_2_3,
              TYFILE.TY_O2_REPO_2_3,
              TYFILE.TY_INT_REPO_2_3,
              TYFILE.TY_NO_REPO_2_4,
              TYFILE.TY_AMT_REPO_2_4,
              TYFILE.TY_O2_REPO_2_4,
              TYFILE.TY_INT_REPO_2_4,
              TYFILE.TY_NO_REPO_2_5,
              TYFILE.TY_AMT_REPO_2_5,
              TYFILE.TY_O2_REPO_2_5,
              TYFILE.TY_INT_REPO_2_5,
              TYFILE.TY_NO_PL_1_1,
              TYFILE.TY_AMT_PL_1_1,
              TYFILE.TY_O2_PL_1_1,
              TYFILE.TY_INT_PL_1_1,
              TYFILE.TY_NO_PL_1_2,
              TYFILE.TY_AMT_PL_1_2,
              TYFILE.TY_O2_PL_1_2,
              TYFILE.TY_INT_PL_1_2,
              TYFILE.TY_NO_PL_1_3,
              TYFILE.TY_AMT_PL_1_3,
              TYFILE.TY_O2_PL_1_3,
              TYFILE.TY_INT_PL_1_3,
              TYFILE.TY_NO_PL_1_4,
              TYFILE.TY_AMT_PL_1_4,
              TYFILE.TY_O2_PL_1_4,
              TYFILE.TY_INT_PL_1_4,
              TYFILE.TY_NO_PL_1_5,
              TYFILE.TY_AMT_PL_1_5,
              TYFILE.TY_O2_PL_1_5,
              TYFILE.TY_INT_PL_1_5,
              TYFILE.TY_NO_PL_1_6,
              TYFILE.TY_AMT_PL_1_6,
              TYFILE.TY_O2_PL_1_6,
              TYFILE.TY_INT_PL_1_6,
              TYFILE.TY_NO_PL_1_7,
              TYFILE.TY_AMT_PL_1_7,
              TYFILE.TY_O2_PL_1_7,
              TYFILE.TY_INT_PL_1_7,
              TYFILE.TY_NO_PL_1_8,
              TYFILE.TY_AMT_PL_1_8,
              TYFILE.TY_O2_PL_1_8,
              TYFILE.TY_INT_PL_1_8,
              TYFILE.TY_NO_PL_1_9,
              TYFILE.TY_AMT_PL_1_9,
              TYFILE.TY_O2_PL_1_9,
              TYFILE.TY_INT_PL_1_9,
              TYFILE.TY_NO_PL_1_10,
              TYFILE.TY_AMT_PL_1_10,
              TYFILE.TY_O2_PL_1_10,
              TYFILE.TY_INT_PL_1_10,
              TYFILE.TY_NO_PL_2_1,
              TYFILE.TY_AMT_PL_2_1,
              TYFILE.TY_O2_PL_2_1,
              TYFILE.TY_INT_PL_2_1,
              TYFILE.TY_NO_PL_2_2,
              TYFILE.TY_AMT_PL_2_2,
              TYFILE.TY_O2_PL_2_2,
              TYFILE.TY_INT_PL_2_2,
              TYFILE.TY_NO_PL_2_3,
              TYFILE.TY_AMT_PL_2_3,
              TYFILE.TY_O2_PL_2_3,
              TYFILE.TY_INT_PL_2_3,
              TYFILE.TY_NO_PL_2_4,
              TYFILE.TY_AMT_PL_2_4,
              TYFILE.TY_O2_PL_2_4,
              TYFILE.TY_INT_PL_2_4,
              TYFILE.TY_NO_PL_2_5,
              TYFILE.TY_AMT_PL_2_5,
              TYFILE.TY_O2_PL_2_5,
              TYFILE.TY_INT_PL_2_5,
              TYFILE.TY_NO_PL_2_6,
              TYFILE.TY_AMT_PL_2_6,
              TYFILE.TY_O2_PL_2_6,
              TYFILE.TY_INT_PL_2_6,
              TYFILE.TY_NO_PL_2_7,
              TYFILE.TY_AMT_PL_2_7,
              TYFILE.TY_O2_PL_2_7,
              TYFILE.TY_INT_PL_2_7,
              TYFILE.TY_NO_PL_2_8,
              TYFILE.TY_AMT_PL_2_8,
              TYFILE.TY_O2_PL_2_8,
              TYFILE.TY_INT_PL_2_8,
              TYFILE.TY_NO_PL_2_9,
              TYFILE.TY_AMT_PL_2_9,
              TYFILE.TY_O2_PL_2_9,
              TYFILE.TY_INT_PL_2_9,
              TYFILE.TY_NO_PL_2_10,
              TYFILE.TY_AMT_PL_2_10,
              TYFILE.TY_O2_PL_2_10,
              TYFILE.TY_INT_PL_2_10,
              TYFILE.TY_DLRVPRIN_1_1,
              TYFILE.TY_DLRVPRIN_1_2,
              TYFILE.TY_DLRVPRIN_1_3,
              TYFILE.TY_DLRVCHGS_1_1,
              TYFILE.TY_DLRVCHGS_1_2,
              TYFILE.TY_DLRVCHGS_1_3,
              TYFILE.TY_DLPARVHELD_1,
              TYFILE.TY_DLRVPRIN_2_1,
              TYFILE.TY_DLRVPRIN_2_2,
              TYFILE.TY_DLRVPRIN_2_3,
              TYFILE.TY_DLRVCHGS_2_1,
              TYFILE.TY_DLRVCHGS_2_2,
              TYFILE.TY_DLRVCHGS_2_3,
              TYFILE.TY_DLPARVHELD_2,
              TYFILE.TY_PL_PRINCOL_1,
              TYFILE.TY_PL_INTCOL_1,
              TYFILE.TY_PL_LATECOL_1,
              TYFILE.TY_PL_O2COL_1,
              TYFILE.TY_PL_PRINCOL_2,
              TYFILE.TY_PL_INTCOL_2,
              TYFILE.TY_PL_LATECOL_2,
              TYFILE.TY_PL_O2COL_2,
              TYFILE.TY_PL_PRINCOL_3,
              TYFILE.TY_PL_INTCOL_3,
              TYFILE.TY_PL_LATECOL_3,
              TYFILE.TY_PL_O2COL_3,
              TYFILE.TY_PL_PRINCOL_4,
              TYFILE.TY_PL_INTCOL_4,
              TYFILE.TY_PL_LATECOL_4,
              TYFILE.TY_PL_O2COL_4,
              TYFILE.TY_PL_PRINCOL_5,
              TYFILE.TY_PL_INTCOL_5,
              TYFILE.TY_PL_LATECOL_5,
              TYFILE.TY_PL_O2COL_5,
              TYFILE.TY_AMT_SOLD,
              TYFILE.TY_AMT_TRANS,
              TYFILE.TY_AMT_PURCH,
              TYFILE.TY_NO_SOLD,
              TYFILE.TY_NO_TRANS,
              TYFILE.TY_NO_PURCH,
              TYFILE.TY_OTHCOL,
              TYFILE.TY_OT2COL,
              TYFILE.TY_INTCOL,
              TYFILE.TY_LATECOL,
              TYFILE.TY_PRINCOL,
              TYFILE.TY_DEFCOL,
              TYFILE.TY_RNPRIN,
              TYFILE.TY_INTREB,
              TYFILE.TY_CLREB1,
              TYFILE.TY_CLREB2,
              TYFILE.TY_CLREB3,
              TYFILE.TY_CLREB4,
              TYFILE.TY_AHREB,
              TYFILE.TY_PPREB,
              TYFILE.TY_O1REB,
              TYFILE.TY_O2REB,
              TYFILE.TY_O3REB,
              TYFILE.TY_O4REB,
              TYFILE.TY_O5REB,
              TYFILE.TY_O6REB,
              TYFILE.TY_O7REB,
              TYFILE.TY_SCREB,
              TYFILE.TY_MFREB,
              TYFILE.TY_DFREB,
              TYFILE.TY_UN7_REB,
              TYFILE.TY_UN8_REB,
              TYFILE.TY_UN9_REB,
              TYFILE.TY_UN10_REB,
              TYFILE.TY_PART_CHGOFF,
              TYFILE.TY_INS_NO_ELIG_1,
              TYFILE.TY_INS_NO_WROTE_1,
              TYFILE.TY_INS_NO_NOTWROTE_1,
              TYFILE.TY_INS_AMT_WROTE_1,
              TYFILE.TY_INS_NO_ELIG_2,
              TYFILE.TY_INS_NO_WROTE_2,
              TYFILE.TY_INS_NO_NOTWROTE_2,
              TYFILE.TY_INS_AMT_WROTE_2,
              TYFILE.TY_INS_NO_ELIG_3,
              TYFILE.TY_INS_NO_WROTE_3,
              TYFILE.TY_INS_NO_NOTWROTE_3,
              TYFILE.TY_INS_AMT_WROTE_3,
              TYFILE.TY_INS_NO_ELIG_4,
              TYFILE.TY_INS_NO_WROTE_4,
              TYFILE.TY_INS_NO_NOTWROTE_4,
              TYFILE.TY_INS_AMT_WROTE_4,
              TYFILE.TY_INS_NO_ELIG_5,
              TYFILE.TY_INS_NO_WROTE_5,
              TYFILE.TY_INS_NO_NOTWROTE_5,
              TYFILE.TY_INS_AMT_WROTE_5,
              TYFILE.TY_INS_NO_ELIG_6,
              TYFILE.TY_INS_NO_WROTE_6,
              TYFILE.TY_INS_NO_NOTWROTE_6,
              TYFILE.TY_INS_AMT_WROTE_6,
              TYFILE.TY_INS_NO_ELIG_7,
              TYFILE.TY_INS_NO_WROTE_7,
              TYFILE.TY_INS_NO_NOTWROTE_7,
              TYFILE.TY_INS_AMT_WROTE_7,
              TYFILE.TY_INS_NO_ELIG_8,
              TYFILE.TY_INS_NO_WROTE_8,
              TYFILE.TY_INS_NO_NOTWROTE_8,
              TYFILE.TY_INS_AMT_WROTE_8,
              TYFILE.TY_INS_NO_ELIG_9,
              TYFILE.TY_INS_NO_WROTE_9,
              TYFILE.TY_INS_NO_NOTWROTE_9,
              TYFILE.TY_INS_AMT_WROTE_9,
              TYFILE.TY_INS_NO_ELIG_10,
              TYFILE.TY_INS_NO_WROTE_10,
              TYFILE.TY_INS_NO_NOTWROTE_10,
              TYFILE.TY_INS_AMT_WROTE_10,
              TYFILE.TY_FEE_NO_ELIG_1,
              TYFILE.TY_FEE_NO_WROTE_1,
              TYFILE.TY_FEE_NO_NOTWROTE_1,
              TYFILE.TY_FEE_AMT_WROTE_1,
              TYFILE.TY_FEE_NO_ELIG_2,
              TYFILE.TY_FEE_NO_WROTE_2,
              TYFILE.TY_FEE_NO_NOTWROTE_2,
              TYFILE.TY_FEE_AMT_WROTE_2,
              TYFILE.TY_FEE_NO_ELIG_3,
              TYFILE.TY_FEE_NO_WROTE_3,
              TYFILE.TY_FEE_NO_NOTWROTE_3,
              TYFILE.TY_FEE_AMT_WROTE_3,
              TYFILE.TY_OTH_NO_COL,
              TYFILE.TY_OT2_NO_COL,
              TYFILE.TY_INT_NO_COL,
              TYFILE.TY_LATE_NO_COL,
              TYFILE.TY_OTH_NO_WAIVED,
              TYFILE.TY_OT2_NO_WAIVED,
              TYFILE.TY_INT_NO_WAIVED,
              TYFILE.TY_LATE_NO_WAIVED,
              TYFILE.TY_OTH_AMT_WAIVED,
              TYFILE.TY_OT2_AMT_WAIVED,
              TYFILE.TY_INT_AMT_WAIVED,
              TYFILE.TY_LATE_AMT_WAIVED,
              TYFILE.TY_CKWRITTEN_1,
              TYFILE.TY_CKWRITTEN_2,
              TYFILE.TY_CKWRITTEN_3,
              TYFILE.TY_CKWRITTEN_4,
              TYFILE.TY_CKWRITTEN_5,
              TYFILE.TY_CKWRITTEN_6,
              TYFILE.TY_CKWRITTEN_7,
              TYFILE.TY_CKWRITTEN_8,
              TYFILE.TY_CKWRITTEN_9,
              TYFILE.TY_CKWRITTEN_10,
              TYFILE.TY_CKWRITTEN_11,
              TYFILE.TY_CKWRITTEN_12,
              TYFILE.TY_CKWRITTEN_13,
              TYFILE.TY_CKWRITTEN_14,
              TYFILE.TY_CKWRITTEN_15,
              TYFILE.TY_CKWRITTEN_16,
              TYFILE.TY_UNITS_1_1,
              TYFILE.TY_OUTBAL_1_1,
              TYFILE.TY_UNITS_1_2,
              TYFILE.TY_OUTBAL_1_2,
              TYFILE.TY_UNITS_1_3,
              TYFILE.TY_OUTBAL_1_3,
              TYFILE.TY_UNITS_1_4,
              TYFILE.TY_OUTBAL_1_4,
              TYFILE.TY_UNITS_1_5,
              TYFILE.TY_OUTBAL_1_5,
              TYFILE.TY_UNITS_1_6,
              TYFILE.TY_OUTBAL_1_6,
              TYFILE.TY_UNITS_1_7,
              TYFILE.TY_OUTBAL_1_7,
              TYFILE.TY_UNITS_1_8,
              TYFILE.TY_OUTBAL_1_8,
              TYFILE.TY_UNITS_1_9,
              TYFILE.TY_OUTBAL_1_9,
              TYFILE.TY_UNITS_1_10,
              TYFILE.TY_OUTBAL_1_10,
              TYFILE.TY_UNITS_2_1,
              TYFILE.TY_OUTBAL_2_1,
              TYFILE.TY_UNITS_2_2,
              TYFILE.TY_OUTBAL_2_2,
              TYFILE.TY_UNITS_2_3,
              TYFILE.TY_OUTBAL_2_3,
              TYFILE.TY_UNITS_2_4,
              TYFILE.TY_OUTBAL_2_4,
              TYFILE.TY_UNITS_2_5,
              TYFILE.TY_OUTBAL_2_5,
              TYFILE.TY_UNITS_2_6,
              TYFILE.TY_OUTBAL_2_6,
              TYFILE.TY_UNITS_2_7,
              TYFILE.TY_OUTBAL_2_7,
              TYFILE.TY_UNITS_2_8,
              TYFILE.TY_OUTBAL_2_8,
              TYFILE.TY_UNITS_2_9,
              TYFILE.TY_OUTBAL_2_9,
              TYFILE.TY_UNITS_2_10,
              TYFILE.TY_OUTBAL_2_10,
              TYFILE.TY_UNITS_BNKRPT_1_1,
              TYFILE.TY_OUTBAL_BNKRPT_1_1,
              TYFILE.TY_UNITS_BNKRPT_1_2,
              TYFILE.TY_OUTBAL_BNKRPT_1_2,
              TYFILE.TY_UNITS_BNKRPT_1_3,
              TYFILE.TY_OUTBAL_BNKRPT_1_3,
              TYFILE.TY_UNITS_BNKRPT_1_4,
              TYFILE.TY_OUTBAL_BNKRPT_1_4,
              TYFILE.TY_UNITS_BNKRPT_1_5,
              TYFILE.TY_OUTBAL_BNKRPT_1_5,
              TYFILE.TY_UNITS_BNKRPT_1_6,
              TYFILE.TY_OUTBAL_BNKRPT_1_6,
              TYFILE.TY_UNITS_BNKRPT_1_7,
              TYFILE.TY_OUTBAL_BNKRPT_1_7,
              TYFILE.TY_UNITS_BNKRPT_1_8,
              TYFILE.TY_OUTBAL_BNKRPT_1_8,
              TYFILE.TY_UNITS_BNKRPT_1_9,
              TYFILE.TY_OUTBAL_BNKRPT_1_9,
              TYFILE.TY_UNITS_BNKRPT_1_10,
              TYFILE.TY_OUTBAL_BNKRPT_1_10,
              TYFILE.TY_UNITS_BNKRPT_2_1,
              TYFILE.TY_OUTBAL_BNKRPT_2_1,
              TYFILE.TY_UNITS_BNKRPT_2_2,
              TYFILE.TY_OUTBAL_BNKRPT_2_2,
              TYFILE.TY_UNITS_BNKRPT_2_3,
              TYFILE.TY_OUTBAL_BNKRPT_2_3,
              TYFILE.TY_UNITS_BNKRPT_2_4,
              TYFILE.TY_OUTBAL_BNKRPT_2_4,
              TYFILE.TY_UNITS_BNKRPT_2_5,
              TYFILE.TY_OUTBAL_BNKRPT_2_5,
              TYFILE.TY_UNITS_BNKRPT_2_6,
              TYFILE.TY_OUTBAL_BNKRPT_2_6,
              TYFILE.TY_UNITS_BNKRPT_2_7,
              TYFILE.TY_OUTBAL_BNKRPT_2_7,
              TYFILE.TY_UNITS_BNKRPT_2_8,
              TYFILE.TY_OUTBAL_BNKRPT_2_8,
              TYFILE.TY_UNITS_BNKRPT_2_9,
              TYFILE.TY_OUTBAL_BNKRPT_2_9,
              TYFILE.TY_UNITS_BNKRPT_2_10,
              TYFILE.TY_OUTBAL_BNKRPT_2_10,
              TYFILE.TY_SUMWGTTM,
              TYFILE.TY_SUMWGTAV,
              TYFILE.TY_SUMWGTTA,
              TYFILE.TY_ALLOTMENTS,
              TYFILE.TY_BRFG,
              TYFILE.TY_DLDISC,
              TYFILE.TY_UNEARNED_AMT_1_1,
              TYFILE.TY_UNEARNED_AMT_1_2,
              TYFILE.TY_UNEARNED_AMT_1_3,
              TYFILE.TY_UNEARNED_AMT_1_4,
              TYFILE.TY_UNEARNED_AMT_1_5,
              TYFILE.TY_UNEARNED_AMT_1_6,
              TYFILE.TY_UNEARNED_AMT_1_7,
              TYFILE.TY_UNEARNED_AMT_1_8,
              TYFILE.TY_UNEARNED_AMT_1_9,
              TYFILE.TY_UNEARNED_AMT_1_10,
              TYFILE.TY_UNEARNED_AMT_2_1,
              TYFILE.TY_UNEARNED_AMT_2_2,
              TYFILE.TY_UNEARNED_AMT_2_3,
              TYFILE.TY_UNEARNED_AMT_2_4,
              TYFILE.TY_UNEARNED_AMT_2_5,
              TYFILE.TY_UNEARNED_AMT_2_6,
              TYFILE.TY_UNEARNED_AMT_2_7,
              TYFILE.TY_UNEARNED_AMT_2_8,
              TYFILE.TY_UNEARNED_AMT_2_9,
              TYFILE.TY_UNEARNED_AMT_2_10,
              TYFILE.TY_PROCEEDS,
              TYFILE.TY_LOCKBOX)
            VALUES
             (:QTY-BRNO,
              :QTY-DATE,
              :QTY-CLASS,
              :QTY-LTOUCH-DATE,
              :QTY-NO-1-1,
              :QTY-AMT-1-1,
              :QTY-O2-1-1,
              :QTY-INT-1-1,
              :QTY-NO-1-2,
              :QTY-AMT-1-2,
              :QTY-O2-1-2,
              :QTY-INT-1-2,
              :QTY-NO-1-3,
              :QTY-AMT-1-3,
              :QTY-O2-1-3,
              :QTY-INT-1-3,
              :QTY-NO-1-4,
              :QTY-AMT-1-4,
              :QTY-O2-1-4,
              :QTY-INT-1-4,
              :QTY-NO-1-5,
              :QTY-AMT-1-5,
              :QTY-O2-1-5,
              :QTY-INT-1-5,
              :QTY-NO-1-6,
              :QTY-AMT-1-6,
              :QTY-O2-1-6,
              :QTY-INT-1-6,
              :QTY-NO-1-7,
              :QTY-AMT-1-7,
              :QTY-O2-1-7,
              :QTY-INT-1-7,
              :QTY-NO-1-8,
              :QTY-AMT-1-8,
              :QTY-O2-1-8,
              :QTY-INT-1-8,
              :QTY-NO-1-9,
              :QTY-AMT-1-9,
              :QTY-O2-1-9,
              :QTY-INT-1-9,
              :QTY-NO-1-10,
              :QTY-AMT-1-10,
              :QTY-O2-1-10,
              :QTY-INT-1-10,
              :QTY-NO-1-11,
              :QTY-AMT-1-11,
              :QTY-O2-1-11,
              :QTY-INT-1-11,
              :QTY-NO-2-1,
              :QTY-AMT-2-1,
              :QTY-O2-2-1,
              :QTY-INT-2-1,
              :QTY-NO-2-2,
              :QTY-AMT-2-2,
              :QTY-O2-2-2,
              :QTY-INT-2-2,
              :QTY-NO-2-3,
              :QTY-AMT-2-3,
              :QTY-O2-2-3,
              :QTY-INT-2-3,
              :QTY-NO-2-4,
              :QTY-AMT-2-4,
              :QTY-O2-2-4,
              :QTY-INT-2-4,
              :QTY-NO-2-5,
              :QTY-AMT-2-5,
              :QTY-O2-2-5,
              :QTY-INT-2-5,
              :QTY-NO-2-6,
              :QTY-AMT-2-6,
              :QTY-O2-2-6,
              :QTY-INT-2-6,
              :QTY-NO-2-7,
              :QTY-AMT-2-7,
              :QTY-O2-2-7,
              :QTY-INT-2-7,
              :QTY-NO-2-8,
              :QTY-AMT-2-8,
              :QTY-O2-2-8,
              :QTY-INT-2-8,
              :QTY-NO-2-9,
              :QTY-AMT-2-9,
              :QTY-O2-2-9,
              :QTY-INT-2-9,
              :QTY-NO-2-10,
              :QTY-AMT-2-10,
              :QTY-O2-2-10,
              :QTY-INT-2-10,
              :QTY-NO-2-11,
              :QTY-AMT-2-11,
              :QTY-O2-2-11,
              :QTY-INT-2-11,
              :QTY-NO-REPO-1-1,
              :QTY-AMT-REPO-1-1,
              :QTY-O2-REPO-1-1,
              :QTY-INT-REPO-1-1,
              :QTY-NO-REPO-1-2,
              :QTY-AMT-REPO-1-2,
              :QTY-O2-REPO-1-2,
              :QTY-INT-REPO-1-2,
              :QTY-NO-REPO-1-3,
              :QTY-AMT-REPO-1-3,
              :QTY-O2-REPO-1-3,
              :QTY-INT-REPO-1-3,
              :QTY-NO-REPO-1-4,
              :QTY-AMT-REPO-1-4,
              :QTY-O2-REPO-1-4,
              :QTY-INT-REPO-1-4,
              :QTY-NO-REPO-1-5,
              :QTY-AMT-REPO-1-5,
              :QTY-O2-REPO-1-5,
              :QTY-INT-REPO-1-5,
              :QTY-NO-REPO-2-1,
              :QTY-AMT-REPO-2-1,
              :QTY-O2-REPO-2-1,
              :QTY-INT-REPO-2-1,
              :QTY-NO-REPO-2-2,
              :QTY-AMT-REPO-2-2,
              :QTY-O2-REPO-2-2,
              :QTY-INT-REPO-2-2,
              :QTY-NO-REPO-2-3,
              :QTY-AMT-REPO-2-3,
              :QTY-O2-REPO-2-3,
              :QTY-INT-REPO-2-3,
              :QTY-NO-REPO-2-4,
              :QTY-AMT-REPO-2-4,
              :QTY-O2-REPO-2-4,
              :QTY-INT-REPO-2-4,
              :QTY-NO-REPO-2-5,
              :QTY-AMT-REPO-2-5,
              :QTY-O2-REPO-2-5,
              :QTY-INT-REPO-2-5,
              :QTY-NO-PL-1-1,
              :QTY-AMT-PL-1-1,
              :QTY-O2-PL-1-1,
              :QTY-INT-PL-1-1,
              :QTY-NO-PL-1-2,
              :QTY-AMT-PL-1-2,
              :QTY-O2-PL-1-2,
              :QTY-INT-PL-1-2,
              :QTY-NO-PL-1-3,
              :QTY-AMT-PL-1-3,
              :QTY-O2-PL-1-3,
              :QTY-INT-PL-1-3,
              :QTY-NO-PL-1-4,
              :QTY-AMT-PL-1-4,
              :QTY-O2-PL-1-4,
              :QTY-INT-PL-1-4,
              :QTY-NO-PL-1-5,
              :QTY-AMT-PL-1-5,
              :QTY-O2-PL-1-5,
              :QTY-INT-PL-1-5,
              :QTY-NO-PL-1-6,
              :QTY-AMT-PL-1-6,
              :QTY-O2-PL-1-6,
              :QTY-INT-PL-1-6,
              :QTY-NO-PL-1-7,
              :QTY-AMT-PL-1-7,
              :QTY-O2-PL-1-7,
              :QTY-INT-PL-1-7,
              :QTY-NO-PL-1-8,
              :QTY-AMT-PL-1-8,
              :QTY-O2-PL-1-8,
              :QTY-INT-PL-1-8,
              :QTY-NO-PL-1-9,
              :QTY-AMT-PL-1-9,
              :QTY-O2-PL-1-9,
              :QTY-INT-PL-1-9,
              :QTY-NO-PL-1-10,
              :QTY-AMT-PL-1-10,
              :QTY-O2-PL-1-10,
              :QTY-INT-PL-1-10,
              :QTY-NO-PL-2-1,
              :QTY-AMT-PL-2-1,
              :QTY-O2-PL-2-1,
              :QTY-INT-PL-2-1,
              :QTY-NO-PL-2-2,
              :QTY-AMT-PL-2-2,
              :QTY-O2-PL-2-2,
              :QTY-INT-PL-2-2,
              :QTY-NO-PL-2-3,
              :QTY-AMT-PL-2-3,
              :QTY-O2-PL-2-3,
              :QTY-INT-PL-2-3,
              :QTY-NO-PL-2-4,
              :QTY-AMT-PL-2-4,
              :QTY-O2-PL-2-4,
              :QTY-INT-PL-2-4,
              :QTY-NO-PL-2-5,
              :QTY-AMT-PL-2-5,
              :QTY-O2-PL-2-5,
              :QTY-INT-PL-2-5,
              :QTY-NO-PL-2-6,
              :QTY-AMT-PL-2-6,
              :QTY-O2-PL-2-6,
              :QTY-INT-PL-2-6,
              :QTY-NO-PL-2-7,
              :QTY-AMT-PL-2-7,
              :QTY-O2-PL-2-7,
              :QTY-INT-PL-2-7,
              :QTY-NO-PL-2-8,
              :QTY-AMT-PL-2-8,
              :QTY-O2-PL-2-8,
              :QTY-INT-PL-2-8,
              :QTY-NO-PL-2-9,
              :QTY-AMT-PL-2-9,
              :QTY-O2-PL-2-9,
              :QTY-INT-PL-2-9,
              :QTY-NO-PL-2-10,
              :QTY-AMT-PL-2-10,
              :QTY-O2-PL-2-10,
              :QTY-INT-PL-2-10,
              :QTY-DLRVPRIN-1-1,
              :QTY-DLRVPRIN-1-2,
              :QTY-DLRVPRIN-1-3,
              :QTY-DLRVCHGS-1-1,
              :QTY-DLRVCHGS-1-2,
              :QTY-DLRVCHGS-1-3,
              :QTY-DLPARVHELD-1,
              :QTY-DLRVPRIN-2-1,
              :QTY-DLRVPRIN-2-2,
              :QTY-DLRVPRIN-2-3,
              :QTY-DLRVCHGS-2-1,
              :QTY-DLRVCHGS-2-2,
              :QTY-DLRVCHGS-2-3,
              :QTY-DLPARVHELD-2,
              :QTY-PL-PRINCOL-1,
              :QTY-PL-INTCOL-1,
              :QTY-PL-LATECOL-1,
              :QTY-PL-O2COL-1,
              :QTY-PL-PRINCOL-2,
              :QTY-PL-INTCOL-2,
              :QTY-PL-LATECOL-2,
              :QTY-PL-O2COL-2,
              :QTY-PL-PRINCOL-3,
              :QTY-PL-INTCOL-3,
              :QTY-PL-LATECOL-3,
              :QTY-PL-O2COL-3,
              :QTY-PL-PRINCOL-4,
              :QTY-PL-INTCOL-4,
              :QTY-PL-LATECOL-4,
              :QTY-PL-O2COL-4,
              :QTY-PL-PRINCOL-5,
              :QTY-PL-INTCOL-5,
              :QTY-PL-LATECOL-5,
              :QTY-PL-O2COL-5,
              :QTY-AMT-SOLD,
              :QTY-AMT-TRANS,
              :QTY-AMT-PURCH,
              :QTY-NO-SOLD,
              :QTY-NO-TRANS,
              :QTY-NO-PURCH,
              :QTY-OTHCOL,
              :QTY-OT2COL,
              :QTY-INTCOL,
              :QTY-LATECOL,
              :QTY-PRINCOL,
              :QTY-DEFCOL,
              :QTY-RNPRIN,
              :QTY-INTREB,
              :QTY-CLREB1,
              :QTY-CLREB2,
              :QTY-CLREB3,
              :QTY-CLREB4,
              :QTY-AHREB,
              :QTY-PPREB,
              :QTY-O1REB,
              :QTY-O2REB,
              :QTY-O3REB,
              :QTY-O4REB,
              :QTY-O5REB,
              :QTY-O6REB,
              :QTY-O7REB,
              :QTY-SCREB,
              :QTY-MFREB,
              :QTY-DFREB,
              :QTY-UN7-REB,
              :QTY-UN8-REB,
              :QTY-UN9-REB,
              :QTY-UN10-REB,
              :QTY-PART-CHGOFF,
              :QTY-INS-NO-ELIG-1,
              :QTY-INS-NO-WROTE-1,
              :QTY-INS-NO-NOTWROTE-1,
              :QTY-INS-AMT-WROTE-1,
              :QTY-INS-NO-ELIG-2,
              :QTY-INS-NO-WROTE-2,
              :QTY-INS-NO-NOTWROTE-2,
              :QTY-INS-AMT-WROTE-2,
              :QTY-INS-NO-ELIG-3,
              :QTY-INS-NO-WROTE-3,
              :QTY-INS-NO-NOTWROTE-3,
              :QTY-INS-AMT-WROTE-3,
              :QTY-INS-NO-ELIG-4,
              :QTY-INS-NO-WROTE-4,
              :QTY-INS-NO-NOTWROTE-4,
              :QTY-INS-AMT-WROTE-4,
              :QTY-INS-NO-ELIG-5,
              :QTY-INS-NO-WROTE-5,
              :QTY-INS-NO-NOTWROTE-5,
              :QTY-INS-AMT-WROTE-5,
              :QTY-INS-NO-ELIG-6,
              :QTY-INS-NO-WROTE-6,
              :QTY-INS-NO-NOTWROTE-6,
              :QTY-INS-AMT-WROTE-6,
              :QTY-INS-NO-ELIG-7,
              :QTY-INS-NO-WROTE-7,
              :QTY-INS-NO-NOTWROTE-7,
              :QTY-INS-AMT-WROTE-7,
              :QTY-INS-NO-ELIG-8,
              :QTY-INS-NO-WROTE-8,
              :QTY-INS-NO-NOTWROTE-8,
              :QTY-INS-AMT-WROTE-8,
              :QTY-INS-NO-ELIG-9,
              :QTY-INS-NO-WROTE-9,
              :QTY-INS-NO-NOTWROTE-9,
              :QTY-INS-AMT-WROTE-9,
              :QTY-INS-NO-ELIG-10,
              :QTY-INS-NO-WROTE-10,
              :QTY-INS-NO-NOTWROTE-10,
              :QTY-INS-AMT-WROTE-10,
              :QTY-FEE-NO-ELIG-1,
              :QTY-FEE-NO-WROTE-1,
              :QTY-FEE-NO-NOTWROTE-1,
              :QTY-FEE-AMT-WROTE-1,
              :QTY-FEE-NO-ELIG-2,
              :QTY-FEE-NO-WROTE-2,
              :QTY-FEE-NO-NOTWROTE-2,
              :QTY-FEE-AMT-WROTE-2,
              :QTY-FEE-NO-ELIG-3,
              :QTY-FEE-NO-WROTE-3,
              :QTY-FEE-NO-NOTWROTE-3,
              :QTY-FEE-AMT-WROTE-3,
              :QTY-OTH-NO-COL,
              :QTY-OT2-NO-COL,
              :QTY-INT-NO-COL,
              :QTY-LATE-NO-COL,
              :QTY-OTH-NO-WAIVED,
              :QTY-OT2-NO-WAIVED,
              :QTY-INT-NO-WAIVED,
              :QTY-LATE-NO-WAIVED,
              :QTY-OTH-AMT-WAIVED,
              :QTY-OT2-AMT-WAIVED,
              :QTY-INT-AMT-WAIVED,
              :QTY-LATE-AMT-WAIVED,
              :QTY-CKWRITTEN-1,
              :QTY-CKWRITTEN-2,
              :QTY-CKWRITTEN-3,
              :QTY-CKWRITTEN-4,
              :QTY-CKWRITTEN-5,
              :QTY-CKWRITTEN-6,
              :QTY-CKWRITTEN-7,
              :QTY-CKWRITTEN-8,
              :QTY-CKWRITTEN-9,
              :QTY-CKWRITTEN-10,
              :QTY-CKWRITTEN-11,
              :QTY-CKWRITTEN-12,
              :QTY-CKWRITTEN-13,
              :QTY-CKWRITTEN-14,
              :QTY-CKWRITTEN-15,
              :QTY-CKWRITTEN-16,
              :QTY-UNITS-1-1,
              :QTY-OUTBAL-1-1,
              :QTY-UNITS-1-2,
              :QTY-OUTBAL-1-2,
              :QTY-UNITS-1-3,
              :QTY-OUTBAL-1-3,
              :QTY-UNITS-1-4,
              :QTY-OUTBAL-1-4,
              :QTY-UNITS-1-5,
              :QTY-OUTBAL-1-5,
              :QTY-UNITS-1-6,
              :QTY-OUTBAL-1-6,
              :QTY-UNITS-1-7,
              :QTY-OUTBAL-1-7,
              :QTY-UNITS-1-8,
              :QTY-OUTBAL-1-8,
              :QTY-UNITS-1-9,
              :QTY-OUTBAL-1-9,
              :QTY-UNITS-1-10,
              :QTY-OUTBAL-1-10,
              :QTY-UNITS-2-1,
              :QTY-OUTBAL-2-1,
              :QTY-UNITS-2-2,
              :QTY-OUTBAL-2-2,
              :QTY-UNITS-2-3,
              :QTY-OUTBAL-2-3,
              :QTY-UNITS-2-4,
              :QTY-OUTBAL-2-4,
              :QTY-UNITS-2-5,
              :QTY-OUTBAL-2-5,
              :QTY-UNITS-2-6,
              :QTY-OUTBAL-2-6,
              :QTY-UNITS-2-7,
              :QTY-OUTBAL-2-7,
              :QTY-UNITS-2-8,
              :QTY-OUTBAL-2-8,
              :QTY-UNITS-2-9,
              :QTY-OUTBAL-2-9,
              :QTY-UNITS-2-10,
              :QTY-OUTBAL-2-10,
              :QTY-UNITS-BNKRPT-1-1,
              :QTY-OUTBAL-BNKRPT-1-1,
              :QTY-UNITS-BNKRPT-1-2,
              :QTY-OUTBAL-BNKRPT-1-2,
              :QTY-UNITS-BNKRPT-1-3,
              :QTY-OUTBAL-BNKRPT-1-3,
              :QTY-UNITS-BNKRPT-1-4,
              :QTY-OUTBAL-BNKRPT-1-4,
              :QTY-UNITS-BNKRPT-1-5,
              :QTY-OUTBAL-BNKRPT-1-5,
              :QTY-UNITS-BNKRPT-1-6,
              :QTY-OUTBAL-BNKRPT-1-6,
              :QTY-UNITS-BNKRPT-1-7,
              :QTY-OUTBAL-BNKRPT-1-7,
              :QTY-UNITS-BNKRPT-1-8,
              :QTY-OUTBAL-BNKRPT-1-8,
              :QTY-UNITS-BNKRPT-1-9,
              :QTY-OUTBAL-BNKRPT-1-9,
              :QTY-UNITS-BNKRPT-1-10,
              :QTY-OUTBAL-BNKRPT-1-10,
              :QTY-UNITS-BNKRPT-2-1,
              :QTY-OUTBAL-BNKRPT-2-1,
              :QTY-UNITS-BNKRPT-2-2,
              :QTY-OUTBAL-BNKRPT-2-2,
              :QTY-UNITS-BNKRPT-2-3,
              :QTY-OUTBAL-BNKRPT-2-3,
              :QTY-UNITS-BNKRPT-2-4,
              :QTY-OUTBAL-BNKRPT-2-4,
              :QTY-UNITS-BNKRPT-2-5,
              :QTY-OUTBAL-BNKRPT-2-5,
              :QTY-UNITS-BNKRPT-2-6,
              :QTY-OUTBAL-BNKRPT-2-6,
              :QTY-UNITS-BNKRPT-2-7,
              :QTY-OUTBAL-BNKRPT-2-7,
              :QTY-UNITS-BNKRPT-2-8,
              :QTY-OUTBAL-BNKRPT-2-8,
              :QTY-UNITS-BNKRPT-2-9,
              :QTY-OUTBAL-BNKRPT-2-9,
              :QTY-UNITS-BNKRPT-2-10,
              :QTY-OUTBAL-BNKRPT-2-10,
              :QTY-SUMWGTTM,
              :QTY-SUMWGTAV,
              :QTY-SUMWGTTA,
              :QTY-ALLOTMENTS,
              :QTY-BRFG,
              :QTY-DLDISC,
              :QTY-UNEARNED-AMT-1-1,
              :QTY-UNEARNED-AMT-1-2,
              :QTY-UNEARNED-AMT-1-3,
              :QTY-UNEARNED-AMT-1-4,
              :QTY-UNEARNED-AMT-1-5,
              :QTY-UNEARNED-AMT-1-6,
              :QTY-UNEARNED-AMT-1-7,
              :QTY-UNEARNED-AMT-1-8,
              :QTY-UNEARNED-AMT-1-9,
              :QTY-UNEARNED-AMT-1-10,
              :QTY-UNEARNED-AMT-2-1,
              :QTY-UNEARNED-AMT-2-2,
              :QTY-UNEARNED-AMT-2-3,
              :QTY-UNEARNED-AMT-2-4,
              :QTY-UNEARNED-AMT-2-5,
              :QTY-UNEARNED-AMT-2-6,
              :QTY-UNEARNED-AMT-2-7,
              :QTY-UNEARNED-AMT-2-8,
              :QTY-UNEARNED-AMT-2-9,
              :QTY-UNEARNED-AMT-2-10,
              :QTY-PROCEEDS,
              :QTY-LOCKBOX)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-TY1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

           MOVE '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.


      *-----------------------------------------------------------------
       REWRITE-TY1-FILE.

           MOVE TY-PATH-OWNBR  TO TY-BRNO.
           ACCEPT TY-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE TY-PATH-SQL    TO E-FILE.
           MOVE TY1-KEY        TO E-KEYX.
           PERFORM SET-TY-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            UPDATE DBO.TYFILE
             SET
              TYFILE.TY_BRNO = :QTY-BRNO,
              TYFILE.TY_DATE = :QTY-DATE,
              TYFILE.TY_CLASS = :QTY-CLASS,
              TYFILE.TY_LTOUCH_DATE = :QTY-LTOUCH-DATE,
              TYFILE.TY_NO_1_1 = :QTY-NO-1-1,
              TYFILE.TY_AMT_1_1 = :QTY-AMT-1-1,
              TYFILE.TY_O2_1_1 = :QTY-O2-1-1,
              TYFILE.TY_INT_1_1 = :QTY-INT-1-1,
              TYFILE.TY_NO_1_2 = :QTY-NO-1-2,
              TYFILE.TY_AMT_1_2 = :QTY-AMT-1-2,
              TYFILE.TY_O2_1_2 = :QTY-O2-1-2,
              TYFILE.TY_INT_1_2 = :QTY-INT-1-2,
              TYFILE.TY_NO_1_3 = :QTY-NO-1-3,
              TYFILE.TY_AMT_1_3 = :QTY-AMT-1-3,
              TYFILE.TY_O2_1_3 = :QTY-O2-1-3,
              TYFILE.TY_INT_1_3 = :QTY-INT-1-3,
              TYFILE.TY_NO_1_4 = :QTY-NO-1-4,
              TYFILE.TY_AMT_1_4 = :QTY-AMT-1-4,
              TYFILE.TY_O2_1_4 = :QTY-O2-1-4,
              TYFILE.TY_INT_1_4 = :QTY-INT-1-4,
              TYFILE.TY_NO_1_5 = :QTY-NO-1-5,
              TYFILE.TY_AMT_1_5 = :QTY-AMT-1-5,
              TYFILE.TY_O2_1_5 = :QTY-O2-1-5,
              TYFILE.TY_INT_1_5 = :QTY-INT-1-5,
              TYFILE.TY_NO_1_6 = :QTY-NO-1-6,
              TYFILE.TY_AMT_1_6 = :QTY-AMT-1-6,
              TYFILE.TY_O2_1_6 = :QTY-O2-1-6,
              TYFILE.TY_INT_1_6 = :QTY-INT-1-6,
              TYFILE.TY_NO_1_7 = :QTY-NO-1-7,
              TYFILE.TY_AMT_1_7 = :QTY-AMT-1-7,
              TYFILE.TY_O2_1_7 = :QTY-O2-1-7,
              TYFILE.TY_INT_1_7 = :QTY-INT-1-7,
              TYFILE.TY_NO_1_8 = :QTY-NO-1-8,
              TYFILE.TY_AMT_1_8 = :QTY-AMT-1-8,
              TYFILE.TY_O2_1_8 = :QTY-O2-1-8,
              TYFILE.TY_INT_1_8 = :QTY-INT-1-8,
              TYFILE.TY_NO_1_9 = :QTY-NO-1-9,
              TYFILE.TY_AMT_1_9 = :QTY-AMT-1-9,
              TYFILE.TY_O2_1_9 = :QTY-O2-1-9,
              TYFILE.TY_INT_1_9 = :QTY-INT-1-9,
              TYFILE.TY_NO_1_10 = :QTY-NO-1-10,
              TYFILE.TY_AMT_1_10 = :QTY-AMT-1-10,
              TYFILE.TY_O2_1_10 = :QTY-O2-1-10,
              TYFILE.TY_INT_1_10 = :QTY-INT-1-10,
              TYFILE.TY_NO_1_11 = :QTY-NO-1-11,
              TYFILE.TY_AMT_1_11 = :QTY-AMT-1-11,
              TYFILE.TY_O2_1_11 = :QTY-O2-1-11,
              TYFILE.TY_INT_1_11 = :QTY-INT-1-11,
              TYFILE.TY_NO_2_1 = :QTY-NO-2-1,
              TYFILE.TY_AMT_2_1 = :QTY-AMT-2-1,
              TYFILE.TY_O2_2_1 = :QTY-O2-2-1,
              TYFILE.TY_INT_2_1 = :QTY-INT-2-1,
              TYFILE.TY_NO_2_2 = :QTY-NO-2-2,
              TYFILE.TY_AMT_2_2 = :QTY-AMT-2-2,
              TYFILE.TY_O2_2_2 = :QTY-O2-2-2,
              TYFILE.TY_INT_2_2 = :QTY-INT-2-2,
              TYFILE.TY_NO_2_3 = :QTY-NO-2-3,
              TYFILE.TY_AMT_2_3 = :QTY-AMT-2-3,
              TYFILE.TY_O2_2_3 = :QTY-O2-2-3,
              TYFILE.TY_INT_2_3 = :QTY-INT-2-3,
              TYFILE.TY_NO_2_4 = :QTY-NO-2-4,
              TYFILE.TY_AMT_2_4 = :QTY-AMT-2-4,
              TYFILE.TY_O2_2_4 = :QTY-O2-2-4,
              TYFILE.TY_INT_2_4 = :QTY-INT-2-4,
              TYFILE.TY_NO_2_5 = :QTY-NO-2-5,
              TYFILE.TY_AMT_2_5 = :QTY-AMT-2-5,
              TYFILE.TY_O2_2_5 = :QTY-O2-2-5,
              TYFILE.TY_INT_2_5 = :QTY-INT-2-5,
              TYFILE.TY_NO_2_6 = :QTY-NO-2-6,
              TYFILE.TY_AMT_2_6 = :QTY-AMT-2-6,
              TYFILE.TY_O2_2_6 = :QTY-O2-2-6,
              TYFILE.TY_INT_2_6 = :QTY-INT-2-6,
              TYFILE.TY_NO_2_7 = :QTY-NO-2-7,
              TYFILE.TY_AMT_2_7 = :QTY-AMT-2-7,
              TYFILE.TY_O2_2_7 = :QTY-O2-2-7,
              TYFILE.TY_INT_2_7 = :QTY-INT-2-7,
              TYFILE.TY_NO_2_8 = :QTY-NO-2-8,
              TYFILE.TY_AMT_2_8 = :QTY-AMT-2-8,
              TYFILE.TY_O2_2_8 = :QTY-O2-2-8,
              TYFILE.TY_INT_2_8 = :QTY-INT-2-8,
              TYFILE.TY_NO_2_9 = :QTY-NO-2-9,
              TYFILE.TY_AMT_2_9 = :QTY-AMT-2-9,
              TYFILE.TY_O2_2_9 = :QTY-O2-2-9,
              TYFILE.TY_INT_2_9 = :QTY-INT-2-9,
              TYFILE.TY_NO_2_10 = :QTY-NO-2-10,
              TYFILE.TY_AMT_2_10 = :QTY-AMT-2-10,
              TYFILE.TY_O2_2_10 = :QTY-O2-2-10,
              TYFILE.TY_INT_2_10 = :QTY-INT-2-10,
              TYFILE.TY_NO_2_11 = :QTY-NO-2-11,
              TYFILE.TY_AMT_2_11 = :QTY-AMT-2-11,
              TYFILE.TY_O2_2_11 = :QTY-O2-2-11,
              TYFILE.TY_INT_2_11 = :QTY-INT-2-11,
              TYFILE.TY_NO_REPO_1_1 = :QTY-NO-REPO-1-1,
              TYFILE.TY_AMT_REPO_1_1 = :QTY-AMT-REPO-1-1,
              TYFILE.TY_O2_REPO_1_1 = :QTY-O2-REPO-1-1,
              TYFILE.TY_INT_REPO_1_1 = :QTY-INT-REPO-1-1,
              TYFILE.TY_NO_REPO_1_2 = :QTY-NO-REPO-1-2,
              TYFILE.TY_AMT_REPO_1_2 = :QTY-AMT-REPO-1-2,
              TYFILE.TY_O2_REPO_1_2 = :QTY-O2-REPO-1-2,
              TYFILE.TY_INT_REPO_1_2 = :QTY-INT-REPO-1-2,
              TYFILE.TY_NO_REPO_1_3 = :QTY-NO-REPO-1-3,
              TYFILE.TY_AMT_REPO_1_3 = :QTY-AMT-REPO-1-3,
              TYFILE.TY_O2_REPO_1_3 = :QTY-O2-REPO-1-3,
              TYFILE.TY_INT_REPO_1_3 = :QTY-INT-REPO-1-3,
              TYFILE.TY_NO_REPO_1_4 = :QTY-NO-REPO-1-4,
              TYFILE.TY_AMT_REPO_1_4 = :QTY-AMT-REPO-1-4,
              TYFILE.TY_O2_REPO_1_4 = :QTY-O2-REPO-1-4,
              TYFILE.TY_INT_REPO_1_4 = :QTY-INT-REPO-1-4,
              TYFILE.TY_NO_REPO_1_5 = :QTY-NO-REPO-1-5,
              TYFILE.TY_AMT_REPO_1_5 = :QTY-AMT-REPO-1-5,
              TYFILE.TY_O2_REPO_1_5 = :QTY-O2-REPO-1-5,
              TYFILE.TY_INT_REPO_1_5 = :QTY-INT-REPO-1-5,
              TYFILE.TY_NO_REPO_2_1 = :QTY-NO-REPO-2-1,
              TYFILE.TY_AMT_REPO_2_1 = :QTY-AMT-REPO-2-1,
              TYFILE.TY_O2_REPO_2_1 = :QTY-O2-REPO-2-1,
              TYFILE.TY_INT_REPO_2_1 = :QTY-INT-REPO-2-1,
              TYFILE.TY_NO_REPO_2_2 = :QTY-NO-REPO-2-2,
              TYFILE.TY_AMT_REPO_2_2 = :QTY-AMT-REPO-2-2,
              TYFILE.TY_O2_REPO_2_2 = :QTY-O2-REPO-2-2,
              TYFILE.TY_INT_REPO_2_2 = :QTY-INT-REPO-2-2,
              TYFILE.TY_NO_REPO_2_3 = :QTY-NO-REPO-2-3,
              TYFILE.TY_AMT_REPO_2_3 = :QTY-AMT-REPO-2-3,
              TYFILE.TY_O2_REPO_2_3 = :QTY-O2-REPO-2-3,
              TYFILE.TY_INT_REPO_2_3 = :QTY-INT-REPO-2-3,
              TYFILE.TY_NO_REPO_2_4 = :QTY-NO-REPO-2-4,
              TYFILE.TY_AMT_REPO_2_4 = :QTY-AMT-REPO-2-4,
              TYFILE.TY_O2_REPO_2_4 = :QTY-O2-REPO-2-4,
              TYFILE.TY_INT_REPO_2_4 = :QTY-INT-REPO-2-4,
              TYFILE.TY_NO_REPO_2_5 = :QTY-NO-REPO-2-5,
              TYFILE.TY_AMT_REPO_2_5 = :QTY-AMT-REPO-2-5,
              TYFILE.TY_O2_REPO_2_5 = :QTY-O2-REPO-2-5,
              TYFILE.TY_INT_REPO_2_5 = :QTY-INT-REPO-2-5,
              TYFILE.TY_NO_PL_1_1 = :QTY-NO-PL-1-1,
              TYFILE.TY_AMT_PL_1_1 = :QTY-AMT-PL-1-1,
              TYFILE.TY_O2_PL_1_1 = :QTY-O2-PL-1-1,
              TYFILE.TY_INT_PL_1_1 = :QTY-INT-PL-1-1,
              TYFILE.TY_NO_PL_1_2 = :QTY-NO-PL-1-2,
              TYFILE.TY_AMT_PL_1_2 = :QTY-AMT-PL-1-2,
              TYFILE.TY_O2_PL_1_2 = :QTY-O2-PL-1-2,
              TYFILE.TY_INT_PL_1_2 = :QTY-INT-PL-1-2,
              TYFILE.TY_NO_PL_1_3 = :QTY-NO-PL-1-3,
              TYFILE.TY_AMT_PL_1_3 = :QTY-AMT-PL-1-3,
              TYFILE.TY_O2_PL_1_3 = :QTY-O2-PL-1-3,
              TYFILE.TY_INT_PL_1_3 = :QTY-INT-PL-1-3,
              TYFILE.TY_NO_PL_1_4 = :QTY-NO-PL-1-4,
              TYFILE.TY_AMT_PL_1_4 = :QTY-AMT-PL-1-4,
              TYFILE.TY_O2_PL_1_4 = :QTY-O2-PL-1-4,
              TYFILE.TY_INT_PL_1_4 = :QTY-INT-PL-1-4,
              TYFILE.TY_NO_PL_1_5 = :QTY-NO-PL-1-5,
              TYFILE.TY_AMT_PL_1_5 = :QTY-AMT-PL-1-5,
              TYFILE.TY_O2_PL_1_5 = :QTY-O2-PL-1-5,
              TYFILE.TY_INT_PL_1_5 = :QTY-INT-PL-1-5,
              TYFILE.TY_NO_PL_1_6 = :QTY-NO-PL-1-6,
              TYFILE.TY_AMT_PL_1_6 = :QTY-AMT-PL-1-6,
              TYFILE.TY_O2_PL_1_6 = :QTY-O2-PL-1-6,
              TYFILE.TY_INT_PL_1_6 = :QTY-INT-PL-1-6,
              TYFILE.TY_NO_PL_1_7 = :QTY-NO-PL-1-7,
              TYFILE.TY_AMT_PL_1_7 = :QTY-AMT-PL-1-7,
              TYFILE.TY_O2_PL_1_7 = :QTY-O2-PL-1-7,
              TYFILE.TY_INT_PL_1_7 = :QTY-INT-PL-1-7,
              TYFILE.TY_NO_PL_1_8 = :QTY-NO-PL-1-8,
              TYFILE.TY_AMT_PL_1_8 = :QTY-AMT-PL-1-8,
              TYFILE.TY_O2_PL_1_8 = :QTY-O2-PL-1-8,
              TYFILE.TY_INT_PL_1_8 = :QTY-INT-PL-1-8,
              TYFILE.TY_NO_PL_1_9 = :QTY-NO-PL-1-9,
              TYFILE.TY_AMT_PL_1_9 = :QTY-AMT-PL-1-9,
              TYFILE.TY_O2_PL_1_9 = :QTY-O2-PL-1-9,
              TYFILE.TY_INT_PL_1_9 = :QTY-INT-PL-1-9,
              TYFILE.TY_NO_PL_1_10 = :QTY-NO-PL-1-10,
              TYFILE.TY_AMT_PL_1_10 = :QTY-AMT-PL-1-10,
              TYFILE.TY_O2_PL_1_10 = :QTY-O2-PL-1-10,
              TYFILE.TY_INT_PL_1_10 = :QTY-INT-PL-1-10,
              TYFILE.TY_NO_PL_2_1 = :QTY-NO-PL-2-1,
              TYFILE.TY_AMT_PL_2_1 = :QTY-AMT-PL-2-1,
              TYFILE.TY_O2_PL_2_1 = :QTY-O2-PL-2-1,
              TYFILE.TY_INT_PL_2_1 = :QTY-INT-PL-2-1,
              TYFILE.TY_NO_PL_2_2 = :QTY-NO-PL-2-2,
              TYFILE.TY_AMT_PL_2_2 = :QTY-AMT-PL-2-2,
              TYFILE.TY_O2_PL_2_2 = :QTY-O2-PL-2-2,
              TYFILE.TY_INT_PL_2_2 = :QTY-INT-PL-2-2,
              TYFILE.TY_NO_PL_2_3 = :QTY-NO-PL-2-3,
              TYFILE.TY_AMT_PL_2_3 = :QTY-AMT-PL-2-3,
              TYFILE.TY_O2_PL_2_3 = :QTY-O2-PL-2-3,
              TYFILE.TY_INT_PL_2_3 = :QTY-INT-PL-2-3,
              TYFILE.TY_NO_PL_2_4 = :QTY-NO-PL-2-4,
              TYFILE.TY_AMT_PL_2_4 = :QTY-AMT-PL-2-4,
              TYFILE.TY_O2_PL_2_4 = :QTY-O2-PL-2-4,
              TYFILE.TY_INT_PL_2_4 = :QTY-INT-PL-2-4,
              TYFILE.TY_NO_PL_2_5 = :QTY-NO-PL-2-5,
              TYFILE.TY_AMT_PL_2_5 = :QTY-AMT-PL-2-5,
              TYFILE.TY_O2_PL_2_5 = :QTY-O2-PL-2-5,
              TYFILE.TY_INT_PL_2_5 = :QTY-INT-PL-2-5,
              TYFILE.TY_NO_PL_2_6 = :QTY-NO-PL-2-6,
              TYFILE.TY_AMT_PL_2_6 = :QTY-AMT-PL-2-6,
              TYFILE.TY_O2_PL_2_6 = :QTY-O2-PL-2-6,
              TYFILE.TY_INT_PL_2_6 = :QTY-INT-PL-2-6,
              TYFILE.TY_NO_PL_2_7 = :QTY-NO-PL-2-7,
              TYFILE.TY_AMT_PL_2_7 = :QTY-AMT-PL-2-7,
              TYFILE.TY_O2_PL_2_7 = :QTY-O2-PL-2-7,
              TYFILE.TY_INT_PL_2_7 = :QTY-INT-PL-2-7,
              TYFILE.TY_NO_PL_2_8 = :QTY-NO-PL-2-8,
              TYFILE.TY_AMT_PL_2_8 = :QTY-AMT-PL-2-8,
              TYFILE.TY_O2_PL_2_8 = :QTY-O2-PL-2-8,
              TYFILE.TY_INT_PL_2_8 = :QTY-INT-PL-2-8,
              TYFILE.TY_NO_PL_2_9 = :QTY-NO-PL-2-9,
              TYFILE.TY_AMT_PL_2_9 = :QTY-AMT-PL-2-9,
              TYFILE.TY_O2_PL_2_9 = :QTY-O2-PL-2-9,
              TYFILE.TY_INT_PL_2_9 = :QTY-INT-PL-2-9,
              TYFILE.TY_NO_PL_2_10 = :QTY-NO-PL-2-10,
              TYFILE.TY_AMT_PL_2_10 = :QTY-AMT-PL-2-10,
              TYFILE.TY_O2_PL_2_10 = :QTY-O2-PL-2-10,
              TYFILE.TY_INT_PL_2_10 = :QTY-INT-PL-2-10,
              TYFILE.TY_DLRVPRIN_1_1 = :QTY-DLRVPRIN-1-1,
              TYFILE.TY_DLRVPRIN_1_2 = :QTY-DLRVPRIN-1-2,
              TYFILE.TY_DLRVPRIN_1_3 = :QTY-DLRVPRIN-1-3,
              TYFILE.TY_DLRVCHGS_1_1 = :QTY-DLRVCHGS-1-1,
              TYFILE.TY_DLRVCHGS_1_2 = :QTY-DLRVCHGS-1-2,
              TYFILE.TY_DLRVCHGS_1_3 = :QTY-DLRVCHGS-1-3,
              TYFILE.TY_DLPARVHELD_1 = :QTY-DLPARVHELD-1,
              TYFILE.TY_DLRVPRIN_2_1 = :QTY-DLRVPRIN-2-1,
              TYFILE.TY_DLRVPRIN_2_2 = :QTY-DLRVPRIN-2-2,
              TYFILE.TY_DLRVPRIN_2_3 = :QTY-DLRVPRIN-2-3,
              TYFILE.TY_DLRVCHGS_2_1 = :QTY-DLRVCHGS-2-1,
              TYFILE.TY_DLRVCHGS_2_2 = :QTY-DLRVCHGS-2-2,
              TYFILE.TY_DLRVCHGS_2_3 = :QTY-DLRVCHGS-2-3,
              TYFILE.TY_DLPARVHELD_2 = :QTY-DLPARVHELD-2,
              TYFILE.TY_PL_PRINCOL_1 = :QTY-PL-PRINCOL-1,
              TYFILE.TY_PL_INTCOL_1 = :QTY-PL-INTCOL-1,
              TYFILE.TY_PL_LATECOL_1 = :QTY-PL-LATECOL-1,
              TYFILE.TY_PL_O2COL_1 = :QTY-PL-O2COL-1,
              TYFILE.TY_PL_PRINCOL_2 = :QTY-PL-PRINCOL-2,
              TYFILE.TY_PL_INTCOL_2 = :QTY-PL-INTCOL-2,
              TYFILE.TY_PL_LATECOL_2 = :QTY-PL-LATECOL-2,
              TYFILE.TY_PL_O2COL_2 = :QTY-PL-O2COL-2,
              TYFILE.TY_PL_PRINCOL_3 = :QTY-PL-PRINCOL-3,
              TYFILE.TY_PL_INTCOL_3 = :QTY-PL-INTCOL-3,
              TYFILE.TY_PL_LATECOL_3 = :QTY-PL-LATECOL-3,
              TYFILE.TY_PL_O2COL_3 = :QTY-PL-O2COL-3,
              TYFILE.TY_PL_PRINCOL_4 = :QTY-PL-PRINCOL-4,
              TYFILE.TY_PL_INTCOL_4 = :QTY-PL-INTCOL-4,
              TYFILE.TY_PL_LATECOL_4 = :QTY-PL-LATECOL-4,
              TYFILE.TY_PL_O2COL_4 = :QTY-PL-O2COL-4,
              TYFILE.TY_PL_PRINCOL_5 = :QTY-PL-PRINCOL-5,
              TYFILE.TY_PL_INTCOL_5 = :QTY-PL-INTCOL-5,
              TYFILE.TY_PL_LATECOL_5 = :QTY-PL-LATECOL-5,
              TYFILE.TY_PL_O2COL_5 = :QTY-PL-O2COL-5,
              TYFILE.TY_AMT_SOLD = :QTY-AMT-SOLD,
              TYFILE.TY_AMT_TRANS = :QTY-AMT-TRANS,
              TYFILE.TY_AMT_PURCH = :QTY-AMT-PURCH,
              TYFILE.TY_NO_SOLD = :QTY-NO-SOLD,
              TYFILE.TY_NO_TRANS = :QTY-NO-TRANS,
              TYFILE.TY_NO_PURCH = :QTY-NO-PURCH,
              TYFILE.TY_OTHCOL = :QTY-OTHCOL,
              TYFILE.TY_OT2COL = :QTY-OT2COL,
              TYFILE.TY_INTCOL = :QTY-INTCOL,
              TYFILE.TY_LATECOL = :QTY-LATECOL,
              TYFILE.TY_PRINCOL = :QTY-PRINCOL,
              TYFILE.TY_DEFCOL = :QTY-DEFCOL,
              TYFILE.TY_RNPRIN = :QTY-RNPRIN,
              TYFILE.TY_INTREB = :QTY-INTREB,
              TYFILE.TY_CLREB1 = :QTY-CLREB1,
              TYFILE.TY_CLREB2 = :QTY-CLREB2,
              TYFILE.TY_CLREB3 = :QTY-CLREB3,
              TYFILE.TY_CLREB4 = :QTY-CLREB4,
              TYFILE.TY_AHREB = :QTY-AHREB,
              TYFILE.TY_PPREB = :QTY-PPREB,
              TYFILE.TY_O1REB = :QTY-O1REB,
              TYFILE.TY_O2REB = :QTY-O2REB,
              TYFILE.TY_O3REB = :QTY-O3REB,
              TYFILE.TY_O4REB = :QTY-O4REB,
              TYFILE.TY_O5REB = :QTY-O5REB,
              TYFILE.TY_O6REB = :QTY-O6REB,
              TYFILE.TY_O7REB = :QTY-O7REB,
              TYFILE.TY_SCREB = :QTY-SCREB,
              TYFILE.TY_MFREB = :QTY-MFREB,
              TYFILE.TY_DFREB = :QTY-DFREB,
              TYFILE.TY_UN7_REB = :QTY-UN7-REB,
              TYFILE.TY_UN8_REB = :QTY-UN8-REB,
              TYFILE.TY_UN9_REB = :QTY-UN9-REB,
              TYFILE.TY_UN10_REB = :QTY-UN10-REB,
              TYFILE.TY_PART_CHGOFF = :QTY-PART-CHGOFF,
              TYFILE.TY_INS_NO_ELIG_1 = :QTY-INS-NO-ELIG-1,
              TYFILE.TY_INS_NO_WROTE_1 = :QTY-INS-NO-WROTE-1,
              TYFILE.TY_INS_NO_NOTWROTE_1 =
                   :QTY-INS-NO-NOTWROTE-1,
              TYFILE.TY_INS_AMT_WROTE_1 = :QTY-INS-AMT-WROTE-1,
              TYFILE.TY_INS_NO_ELIG_2 = :QTY-INS-NO-ELIG-2,
              TYFILE.TY_INS_NO_WROTE_2 = :QTY-INS-NO-WROTE-2,
              TYFILE.TY_INS_NO_NOTWROTE_2 =
                   :QTY-INS-NO-NOTWROTE-2,
              TYFILE.TY_INS_AMT_WROTE_2 = :QTY-INS-AMT-WROTE-2,
              TYFILE.TY_INS_NO_ELIG_3 = :QTY-INS-NO-ELIG-3,
              TYFILE.TY_INS_NO_WROTE_3 = :QTY-INS-NO-WROTE-3,
              TYFILE.TY_INS_NO_NOTWROTE_3 =
                   :QTY-INS-NO-NOTWROTE-3,
              TYFILE.TY_INS_AMT_WROTE_3 = :QTY-INS-AMT-WROTE-3,
              TYFILE.TY_INS_NO_ELIG_4 = :QTY-INS-NO-ELIG-4,
              TYFILE.TY_INS_NO_WROTE_4 = :QTY-INS-NO-WROTE-4,
              TYFILE.TY_INS_NO_NOTWROTE_4 =
                   :QTY-INS-NO-NOTWROTE-4,
              TYFILE.TY_INS_AMT_WROTE_4 = :QTY-INS-AMT-WROTE-4,
              TYFILE.TY_INS_NO_ELIG_5 = :QTY-INS-NO-ELIG-5,
              TYFILE.TY_INS_NO_WROTE_5 = :QTY-INS-NO-WROTE-5,
              TYFILE.TY_INS_NO_NOTWROTE_5 =
                   :QTY-INS-NO-NOTWROTE-5,
              TYFILE.TY_INS_AMT_WROTE_5 = :QTY-INS-AMT-WROTE-5,
              TYFILE.TY_INS_NO_ELIG_6 = :QTY-INS-NO-ELIG-6,
              TYFILE.TY_INS_NO_WROTE_6 = :QTY-INS-NO-WROTE-6,
              TYFILE.TY_INS_NO_NOTWROTE_6 =
                   :QTY-INS-NO-NOTWROTE-6,
              TYFILE.TY_INS_AMT_WROTE_6 = :QTY-INS-AMT-WROTE-6,
              TYFILE.TY_INS_NO_ELIG_7 = :QTY-INS-NO-ELIG-7,
              TYFILE.TY_INS_NO_WROTE_7 = :QTY-INS-NO-WROTE-7,
              TYFILE.TY_INS_NO_NOTWROTE_7 =
                   :QTY-INS-NO-NOTWROTE-7,
              TYFILE.TY_INS_AMT_WROTE_7 = :QTY-INS-AMT-WROTE-7,
              TYFILE.TY_INS_NO_ELIG_8 = :QTY-INS-NO-ELIG-8,
              TYFILE.TY_INS_NO_WROTE_8 = :QTY-INS-NO-WROTE-8,
              TYFILE.TY_INS_NO_NOTWROTE_8 =
                   :QTY-INS-NO-NOTWROTE-8,
              TYFILE.TY_INS_AMT_WROTE_8 = :QTY-INS-AMT-WROTE-8,
              TYFILE.TY_INS_NO_ELIG_9 = :QTY-INS-NO-ELIG-9,
              TYFILE.TY_INS_NO_WROTE_9 = :QTY-INS-NO-WROTE-9,
              TYFILE.TY_INS_NO_NOTWROTE_9 =
                   :QTY-INS-NO-NOTWROTE-9,
              TYFILE.TY_INS_AMT_WROTE_9 = :QTY-INS-AMT-WROTE-9,
              TYFILE.TY_INS_NO_ELIG_10 = :QTY-INS-NO-ELIG-10,
              TYFILE.TY_INS_NO_WROTE_10 = :QTY-INS-NO-WROTE-10,
              TYFILE.TY_INS_NO_NOTWROTE_10 =
                   :QTY-INS-NO-NOTWROTE-10,
              TYFILE.TY_INS_AMT_WROTE_10 = :QTY-INS-AMT-WROTE-10,
              TYFILE.TY_FEE_NO_ELIG_1 = :QTY-FEE-NO-ELIG-1,
              TYFILE.TY_FEE_NO_WROTE_1 = :QTY-FEE-NO-WROTE-1,
              TYFILE.TY_FEE_NO_NOTWROTE_1 =
                   :QTY-FEE-NO-NOTWROTE-1,
              TYFILE.TY_FEE_AMT_WROTE_1 = :QTY-FEE-AMT-WROTE-1,
              TYFILE.TY_FEE_NO_ELIG_2 = :QTY-FEE-NO-ELIG-2,
              TYFILE.TY_FEE_NO_WROTE_2 = :QTY-FEE-NO-WROTE-2,
              TYFILE.TY_FEE_NO_NOTWROTE_2 =
                   :QTY-FEE-NO-NOTWROTE-2,
              TYFILE.TY_FEE_AMT_WROTE_2 = :QTY-FEE-AMT-WROTE-2,
              TYFILE.TY_FEE_NO_ELIG_3 = :QTY-FEE-NO-ELIG-3,
              TYFILE.TY_FEE_NO_WROTE_3 = :QTY-FEE-NO-WROTE-3,
              TYFILE.TY_FEE_NO_NOTWROTE_3 =
                   :QTY-FEE-NO-NOTWROTE-3,
              TYFILE.TY_FEE_AMT_WROTE_3 = :QTY-FEE-AMT-WROTE-3,
              TYFILE.TY_OTH_NO_COL = :QTY-OTH-NO-COL,
              TYFILE.TY_OT2_NO_COL = :QTY-OT2-NO-COL,
              TYFILE.TY_INT_NO_COL = :QTY-INT-NO-COL,
              TYFILE.TY_LATE_NO_COL = :QTY-LATE-NO-COL,
              TYFILE.TY_OTH_NO_WAIVED = :QTY-OTH-NO-WAIVED,
              TYFILE.TY_OT2_NO_WAIVED = :QTY-OT2-NO-WAIVED,
              TYFILE.TY_INT_NO_WAIVED = :QTY-INT-NO-WAIVED,
              TYFILE.TY_LATE_NO_WAIVED = :QTY-LATE-NO-WAIVED,
              TYFILE.TY_OTH_AMT_WAIVED = :QTY-OTH-AMT-WAIVED,
              TYFILE.TY_OT2_AMT_WAIVED = :QTY-OT2-AMT-WAIVED,
              TYFILE.TY_INT_AMT_WAIVED = :QTY-INT-AMT-WAIVED,
              TYFILE.TY_LATE_AMT_WAIVED = :QTY-LATE-AMT-WAIVED,
              TYFILE.TY_CKWRITTEN_1 = :QTY-CKWRITTEN-1,
              TYFILE.TY_CKWRITTEN_2 = :QTY-CKWRITTEN-2,
              TYFILE.TY_CKWRITTEN_3 = :QTY-CKWRITTEN-3,
              TYFILE.TY_CKWRITTEN_4 = :QTY-CKWRITTEN-4,
              TYFILE.TY_CKWRITTEN_5 = :QTY-CKWRITTEN-5,
              TYFILE.TY_CKWRITTEN_6 = :QTY-CKWRITTEN-6,
              TYFILE.TY_CKWRITTEN_7 = :QTY-CKWRITTEN-7,
              TYFILE.TY_CKWRITTEN_8 = :QTY-CKWRITTEN-8,
              TYFILE.TY_CKWRITTEN_9 = :QTY-CKWRITTEN-9,
              TYFILE.TY_CKWRITTEN_10 = :QTY-CKWRITTEN-10,
              TYFILE.TY_CKWRITTEN_11 = :QTY-CKWRITTEN-11,
              TYFILE.TY_CKWRITTEN_12 = :QTY-CKWRITTEN-12,
              TYFILE.TY_CKWRITTEN_13 = :QTY-CKWRITTEN-13,
              TYFILE.TY_CKWRITTEN_14 = :QTY-CKWRITTEN-14,
              TYFILE.TY_CKWRITTEN_15 = :QTY-CKWRITTEN-15,
              TYFILE.TY_CKWRITTEN_16 = :QTY-CKWRITTEN-16,
              TYFILE.TY_UNITS_1_1 = :QTY-UNITS-1-1,
              TYFILE.TY_OUTBAL_1_1 = :QTY-OUTBAL-1-1,
              TYFILE.TY_UNITS_1_2 = :QTY-UNITS-1-2,
              TYFILE.TY_OUTBAL_1_2 = :QTY-OUTBAL-1-2,
              TYFILE.TY_UNITS_1_3 = :QTY-UNITS-1-3,
              TYFILE.TY_OUTBAL_1_3 = :QTY-OUTBAL-1-3,
              TYFILE.TY_UNITS_1_4 = :QTY-UNITS-1-4,
              TYFILE.TY_OUTBAL_1_4 = :QTY-OUTBAL-1-4,
              TYFILE.TY_UNITS_1_5 = :QTY-UNITS-1-5,
              TYFILE.TY_OUTBAL_1_5 = :QTY-OUTBAL-1-5,
              TYFILE.TY_UNITS_1_6 = :QTY-UNITS-1-6,
              TYFILE.TY_OUTBAL_1_6 = :QTY-OUTBAL-1-6,
              TYFILE.TY_UNITS_1_7 = :QTY-UNITS-1-7,
              TYFILE.TY_OUTBAL_1_7 = :QTY-OUTBAL-1-7,
              TYFILE.TY_UNITS_1_8 = :QTY-UNITS-1-8,
              TYFILE.TY_OUTBAL_1_8 = :QTY-OUTBAL-1-8,
              TYFILE.TY_UNITS_1_9 = :QTY-UNITS-1-9,
              TYFILE.TY_OUTBAL_1_9 = :QTY-OUTBAL-1-9,
              TYFILE.TY_UNITS_1_10 = :QTY-UNITS-1-10,
              TYFILE.TY_OUTBAL_1_10 = :QTY-OUTBAL-1-10,
              TYFILE.TY_UNITS_2_1 = :QTY-UNITS-2-1,
              TYFILE.TY_OUTBAL_2_1 = :QTY-OUTBAL-2-1,
              TYFILE.TY_UNITS_2_2 = :QTY-UNITS-2-2,
              TYFILE.TY_OUTBAL_2_2 = :QTY-OUTBAL-2-2,
              TYFILE.TY_UNITS_2_3 = :QTY-UNITS-2-3,
              TYFILE.TY_OUTBAL_2_3 = :QTY-OUTBAL-2-3,
              TYFILE.TY_UNITS_2_4 = :QTY-UNITS-2-4,
              TYFILE.TY_OUTBAL_2_4 = :QTY-OUTBAL-2-4,
              TYFILE.TY_UNITS_2_5 = :QTY-UNITS-2-5,
              TYFILE.TY_OUTBAL_2_5 = :QTY-OUTBAL-2-5,
              TYFILE.TY_UNITS_2_6 = :QTY-UNITS-2-6,
              TYFILE.TY_OUTBAL_2_6 = :QTY-OUTBAL-2-6,
              TYFILE.TY_UNITS_2_7 = :QTY-UNITS-2-7,
              TYFILE.TY_OUTBAL_2_7 = :QTY-OUTBAL-2-7,
              TYFILE.TY_UNITS_2_8 = :QTY-UNITS-2-8,
              TYFILE.TY_OUTBAL_2_8 = :QTY-OUTBAL-2-8,
              TYFILE.TY_UNITS_2_9 = :QTY-UNITS-2-9,
              TYFILE.TY_OUTBAL_2_9 = :QTY-OUTBAL-2-9,
              TYFILE.TY_UNITS_2_10 = :QTY-UNITS-2-10,
              TYFILE.TY_OUTBAL_2_10 = :QTY-OUTBAL-2-10,
              TYFILE.TY_UNITS_BNKRPT_1_1 = :QTY-UNITS-BNKRPT-1-1,
              TYFILE.TY_OUTBAL_BNKRPT_1_1 =
                   :QTY-OUTBAL-BNKRPT-1-1,
              TYFILE.TY_UNITS_BNKRPT_1_2 = :QTY-UNITS-BNKRPT-1-2,
              TYFILE.TY_OUTBAL_BNKRPT_1_2 =
                   :QTY-OUTBAL-BNKRPT-1-2,
              TYFILE.TY_UNITS_BNKRPT_1_3 = :QTY-UNITS-BNKRPT-1-3,
              TYFILE.TY_OUTBAL_BNKRPT_1_3 =
                   :QTY-OUTBAL-BNKRPT-1-3,
              TYFILE.TY_UNITS_BNKRPT_1_4 = :QTY-UNITS-BNKRPT-1-4,
              TYFILE.TY_OUTBAL_BNKRPT_1_4 =
                   :QTY-OUTBAL-BNKRPT-1-4,
              TYFILE.TY_UNITS_BNKRPT_1_5 = :QTY-UNITS-BNKRPT-1-5,
              TYFILE.TY_OUTBAL_BNKRPT_1_5 =
                   :QTY-OUTBAL-BNKRPT-1-5,
              TYFILE.TY_UNITS_BNKRPT_1_6 = :QTY-UNITS-BNKRPT-1-6,
              TYFILE.TY_OUTBAL_BNKRPT_1_6 =
                   :QTY-OUTBAL-BNKRPT-1-6,
              TYFILE.TY_UNITS_BNKRPT_1_7 = :QTY-UNITS-BNKRPT-1-7,
              TYFILE.TY_OUTBAL_BNKRPT_1_7 =
                   :QTY-OUTBAL-BNKRPT-1-7,
              TYFILE.TY_UNITS_BNKRPT_1_8 = :QTY-UNITS-BNKRPT-1-8,
              TYFILE.TY_OUTBAL_BNKRPT_1_8 =
                   :QTY-OUTBAL-BNKRPT-1-8,
              TYFILE.TY_UNITS_BNKRPT_1_9 = :QTY-UNITS-BNKRPT-1-9,
              TYFILE.TY_OUTBAL_BNKRPT_1_9 =
                   :QTY-OUTBAL-BNKRPT-1-9,
              TYFILE.TY_UNITS_BNKRPT_1_10 =
                   :QTY-UNITS-BNKRPT-1-10,
              TYFILE.TY_OUTBAL_BNKRPT_1_10 =
                   :QTY-OUTBAL-BNKRPT-1-10,
              TYFILE.TY_UNITS_BNKRPT_2_1 = :QTY-UNITS-BNKRPT-2-1,
              TYFILE.TY_OUTBAL_BNKRPT_2_1 =
                   :QTY-OUTBAL-BNKRPT-2-1,
              TYFILE.TY_UNITS_BNKRPT_2_2 = :QTY-UNITS-BNKRPT-2-2,
              TYFILE.TY_OUTBAL_BNKRPT_2_2 =
                   :QTY-OUTBAL-BNKRPT-2-2,
              TYFILE.TY_UNITS_BNKRPT_2_3 = :QTY-UNITS-BNKRPT-2-3,
              TYFILE.TY_OUTBAL_BNKRPT_2_3 =
                   :QTY-OUTBAL-BNKRPT-2-3,
              TYFILE.TY_UNITS_BNKRPT_2_4 = :QTY-UNITS-BNKRPT-2-4,
              TYFILE.TY_OUTBAL_BNKRPT_2_4 =
                   :QTY-OUTBAL-BNKRPT-2-4,
              TYFILE.TY_UNITS_BNKRPT_2_5 = :QTY-UNITS-BNKRPT-2-5,
              TYFILE.TY_OUTBAL_BNKRPT_2_5 =
                   :QTY-OUTBAL-BNKRPT-2-5,
              TYFILE.TY_UNITS_BNKRPT_2_6 = :QTY-UNITS-BNKRPT-2-6,
              TYFILE.TY_OUTBAL_BNKRPT_2_6 =
                   :QTY-OUTBAL-BNKRPT-2-6,
              TYFILE.TY_UNITS_BNKRPT_2_7 = :QTY-UNITS-BNKRPT-2-7,
              TYFILE.TY_OUTBAL_BNKRPT_2_7 =
                   :QTY-OUTBAL-BNKRPT-2-7,
              TYFILE.TY_UNITS_BNKRPT_2_8 = :QTY-UNITS-BNKRPT-2-8,
              TYFILE.TY_OUTBAL_BNKRPT_2_8 =
                   :QTY-OUTBAL-BNKRPT-2-8,
              TYFILE.TY_UNITS_BNKRPT_2_9 = :QTY-UNITS-BNKRPT-2-9,
              TYFILE.TY_OUTBAL_BNKRPT_2_9 =
                   :QTY-OUTBAL-BNKRPT-2-9,
              TYFILE.TY_UNITS_BNKRPT_2_10 =
                   :QTY-UNITS-BNKRPT-2-10,
              TYFILE.TY_OUTBAL_BNKRPT_2_10 =
                   :QTY-OUTBAL-BNKRPT-2-10,
              TYFILE.TY_SUMWGTTM = :QTY-SUMWGTTM,
              TYFILE.TY_SUMWGTAV = :QTY-SUMWGTAV,
              TYFILE.TY_SUMWGTTA = :QTY-SUMWGTTA,
              TYFILE.TY_ALLOTMENTS = :QTY-ALLOTMENTS,
              TYFILE.TY_BRFG = :QTY-BRFG,
              TYFILE.TY_DLDISC = :QTY-DLDISC,
              TYFILE.TY_UNEARNED_AMT_1_1 = :QTY-UNEARNED-AMT-1-1,
              TYFILE.TY_UNEARNED_AMT_1_2 = :QTY-UNEARNED-AMT-1-2,
              TYFILE.TY_UNEARNED_AMT_1_3 = :QTY-UNEARNED-AMT-1-3,
              TYFILE.TY_UNEARNED_AMT_1_4 = :QTY-UNEARNED-AMT-1-4,
              TYFILE.TY_UNEARNED_AMT_1_5 = :QTY-UNEARNED-AMT-1-5,
              TYFILE.TY_UNEARNED_AMT_1_6 = :QTY-UNEARNED-AMT-1-6,
              TYFILE.TY_UNEARNED_AMT_1_7 = :QTY-UNEARNED-AMT-1-7,
              TYFILE.TY_UNEARNED_AMT_1_8 = :QTY-UNEARNED-AMT-1-8,
              TYFILE.TY_UNEARNED_AMT_1_9 = :QTY-UNEARNED-AMT-1-9,
              TYFILE.TY_UNEARNED_AMT_1_10 =
                   :QTY-UNEARNED-AMT-1-10,
              TYFILE.TY_UNEARNED_AMT_2_1 = :QTY-UNEARNED-AMT-2-1,
              TYFILE.TY_UNEARNED_AMT_2_2 = :QTY-UNEARNED-AMT-2-2,
              TYFILE.TY_UNEARNED_AMT_2_3 = :QTY-UNEARNED-AMT-2-3,
              TYFILE.TY_UNEARNED_AMT_2_4 = :QTY-UNEARNED-AMT-2-4,
              TYFILE.TY_UNEARNED_AMT_2_5 = :QTY-UNEARNED-AMT-2-5,
              TYFILE.TY_UNEARNED_AMT_2_6 = :QTY-UNEARNED-AMT-2-6,
              TYFILE.TY_UNEARNED_AMT_2_7 = :QTY-UNEARNED-AMT-2-7,
              TYFILE.TY_UNEARNED_AMT_2_8 = :QTY-UNEARNED-AMT-2-8,
              TYFILE.TY_UNEARNED_AMT_2_9 = :QTY-UNEARNED-AMT-2-9,
              TYFILE.TY_UNEARNED_AMT_2_10 =
                   :QTY-UNEARNED-AMT-2-10,
              TYFILE.TY_PROCEEDS = :QTY-PROCEEDS,
              TYFILE.TY_LOCKBOX = :QTY-LOCKBOX
            WHERE TYFILE.TY_BRNO = :QTY-BRNO
              AND TYFILE.TY_DATE = :QTY-DATE
              AND TYFILE.TY_CLASS = :QTY-CLASS
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-TY1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

           MOVE '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       DELETE-TY1-FILE.

           MOVE TY-PATH-OWNBR  TO TY-BRNO.
           PERFORM DELETE-IT.
           MOVE TY-PATH-SQL    TO E-FILE.
           MOVE TY1-KEY        TO E-KEYX.
           PERFORM SET-TY-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            DELETE FROM DBO.TYFILE
            WHERE TYFILE.TY_BRNO = :QTY-BRNO
              AND TYFILE.TY_DATE = :QTY-DATE
              AND TYFILE.TY_CLASS = :QTY-CLASS
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-TY1-FILE.

           MOVE '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       CLOSE-TY1-FILE.

           PERFORM CLOSE-IT.

           IF ( TY1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION TY1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE TY1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO TY1-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-TY1-CONNECTION.

           IF TY1-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION TY1_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT TY1_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO TY1-CONN-OPEN-SW.

      *-----------------------------------------------------------------


       COPY "LIBLP/LPED1IN.CPY".
       COPY "LIBLP/LPTRP1RN.CPY".
   
      * COPY "LIBLP/LPTRD1IN.CPY".
      * COPYMEMBER: LIBLP/LPTRD1IN
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

           IF TRD1-CONN-OPEN-SW = "N" 
              MOVE "Y"         TO TRD1-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS TRD1_CONNECT 
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-TRD1-FILE.
           PERFORM START-IT.
           MOVE TRD-PATH-OWNBR  TO TD-BRNO.
           MOVE TRD-PATH-SQL    TO E-FILE.
           MOVE TRD1-KEY        TO E-KEYX.

           IF ( TRD1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRD1-FILE.

           IF ( QTRD1-WEND-BRNO = ZEROES    ) OR
              ( QTRD1-WEND-BRNO NOT NUMERIC )
              MOVE TRD-PATH-OWNBR       TO QTRD1-WBEG-BRNO
              MOVE TRD-PATH-OWNBR       TO QTRD1-WEND-BRNO
              MOVE TD-DATE              TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRD1-WBEG-DATE
              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRD1-WEND-DATE.

      *  STORED PROCEDURE:
      *
      *  SELECT TRDFILE.TD_BRNO,
      *         CAST(TRDFILE.TD_DATE        AS VARCHAR(10)) AS TD_DATE,
      *         CAST(TRDFILE.TD_LTOUCH_DATE AS VARCHAR(10)) AS TD_LTOUCH_DATE,
      *         TRDFILE.TD_DEPOSIT,
      *         TRDFILE.TD_CHECK_DEPOSIT,
      *         TRDFILE.TD_BANK,
      *         TRDFILE.TD_CASH,
      *         TRDFILE.TD_GLFG,
      *         TRDFILE.TD_GLOVERSHORT,
      *         TRDFILE.TD_OVERSHORT,
      *         TRDFILE.TD_USERID,
      *         CAST(TRDFILE.TD_POSTDATE    AS VARCHAR(10)) AS TD_POSTDATE,
      *         TRDFILE.TD_POSTTIME
      *
      *    INTO #TEMP_LPEOD5_TRDFILE
      *
      *    FROM DBO.TRDFILE
      *
      *   WHERE TRDFILE.TD_BRNO       = @QTRD1_WBEG_BRNO
      *     AND TRDFILE.TD_DATE BETWEEN @QTRD1_WBEG_DATE
      *                             AND @QTRD1_WEND_DATE;


           EXEC SQL SET CONNECTION TRD1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE TRD1_CURSOR CURSOR FOR
                :TRD1-RETURN-CODE = EXEC SSP_LPEOD5_TRDFILE_SELECT 
              ( :QTRD1-WBEG-BRNO,  
                :QTRD1-WBEG-DATE,  
                :QTRD1-WEND-DATE )      
           END-EXEC.
 
           EXEC SQL
               OPEN TRD1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRD1-WBEG-BRNO 
                          QTRD1-WEND-BRNO.
 
           MOVE SPACES TO QTRD1-WBEG-DATE
                          QTRD1-WEND-DATE.

           MOVE STAT---GOOD TO TRD1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-TRD1-FILE.
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
                   CAST(TRDFILE.TD_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   TRDFILE.TD_DEPOSIT,
                   TRDFILE.TD_CHECK_DEPOSIT,
                   TRDFILE.TD_BANK,
                   TRDFILE.TD_CASH,
                   TRDFILE.TD_GLFG,
                   TRDFILE.TD_GLOVERSHORT,
                   TRDFILE.TD_OVERSHORT,
                   TRDFILE.TD_USERID,
                   CAST(TRDFILE.TD_POSTDATE
                                                  AS VARCHAR(10)),
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
       READ-TRD1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE TRD-PATH-OWNBR  TO TD-BRNO.
           MOVE TRD-PATH-SQL    TO E-FILE.
           MOVE TRD1-KEY        TO E-KEYX.
           INITIALIZE QTRD-REC.

           EXEC SQL SET CONNECTION TRD1_CONNECT END-EXEC.

           IF ( TRD1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TRD1_CURSOR INTO :QTRD-REC
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
       WRITE-TRD1-FILE.

           MOVE TRD-PATH-OWNBR  TO TD-BRNO.
           ACCEPT TD-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE TRD-PATH-SQL    TO E-FILE.
           MOVE TRD1-KEY        TO E-KEYX.
           PERFORM SET-TRD-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            INSERT INTO DBO.TRDFILE
             (TRDFILE.TD_BRNO,
              TRDFILE.TD_DATE,
              TRDFILE.TD_LTOUCH_DATE,
              TRDFILE.TD_DEPOSIT,
              TRDFILE.TD_CHECK_DEPOSIT,
              TRDFILE.TD_BANK,
              TRDFILE.TD_CASH,
              TRDFILE.TD_GLFG,
              TRDFILE.TD_GLOVERSHORT,
              TRDFILE.TD_OVERSHORT,
              TRDFILE.TD_USERID,
              TRDFILE.TD_POSTDATE,
              TRDFILE.TD_POSTTIME)
            VALUES
             (:QTD-BRNO,
              :QTD-DATE,
              :QTD-LTOUCH-DATE,
              :QTD-DEPOSIT,
              :QTD-CHECK-DEPOSIT,
              :QTD-BANK,
              :QTD-CASH,
              :QTD-GLFG,
              :QTD-GLOVERSHORT,
              :QTD-OVERSHORT,
              :QTD-USERID,
              :QTD-POSTDATE,
              :QTD-POSTTIME)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-TRD1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

           MOVE '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       REWRITE-TRD1-FILE.

           MOVE TRD-PATH-OWNBR  TO TD-BRNO.
           ACCEPT TD-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE TRD-PATH-SQL    TO E-FILE.
           MOVE TRD1-KEY        TO E-KEYX.
           PERFORM SET-TRD-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            UPDATE DBO.TRDFILE
             SET
              TRDFILE.TD_BRNO = :QTD-BRNO,
              TRDFILE.TD_DATE =
                 :QTD-DATE,
              TRDFILE.TD_LTOUCH_DATE =
                 :QTD-LTOUCH-DATE,
              TRDFILE.TD_DEPOSIT = :QTD-DEPOSIT,
              TRDFILE.TD_CHECK_DEPOSIT = :QTD-CHECK-DEPOSIT,
              TRDFILE.TD_BANK = :QTD-BANK,
              TRDFILE.TD_CASH = :QTD-CASH,
              TRDFILE.TD_GLFG = :QTD-GLFG,
              TRDFILE.TD_GLOVERSHORT = :QTD-GLOVERSHORT,
              TRDFILE.TD_OVERSHORT = :QTD-OVERSHORT,
              TRDFILE.TD_USERID = :QTD-USERID,
              TRDFILE.TD_POSTDATE = :QTD-POSTDATE,
              TRDFILE.TD_POSTTIME = :QTD-POSTTIME
            WHERE TRDFILE.TD_BRNO = :QTD-BRNO
              AND TRDFILE.TD_DATE = :QTD-DATE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-TRD1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

           MOVE '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       DELETE-TRD1-FILE.

           MOVE TRD-PATH-OWNBR  TO TD-BRNO.
           PERFORM DELETE-IT.
           MOVE TRD-PATH-SQL    TO E-FILE.
           MOVE TRD1-KEY        TO E-KEYX.
           PERFORM SET-TRD-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            DELETE FROM DBO.TRDFILE
            WHERE TRDFILE.TD_BRNO = :QTD-BRNO
              AND TRDFILE.TD_DATE = :QTD-DATE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-TRD1-FILE.

           MOVE '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       CLOSE-TRD1-FILE.

           PERFORM CLOSE-IT.

           IF ( TRD1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION TRD1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE TRD1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO TRD1-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-TRD1-CONNECTION.

           IF TRD1-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION TRD1_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT TRD1_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO TRD1-CONN-OPEN-SW.

      *-----------------------------------------------------------------


       COPY "LIBLP/LPTRL1RN.CPY".
       COPY "LIBLP/LPTRS1RN.CPY".
       COPY "LIBLP/LPTRB1RN.CPY".
       COPY "LIBLP/LPTRX1RN.CPY".
       COPY "LIBLP/LPTRJ1RN.CPY".
       COPY "LIBLP/LPTRC1RN.CPY".
       COPY "LIBLP/LPTRW1RN.CPY".
       COPY "LIBLP/LPTRK1RN.CPY".
       COPY "LIBGL/GLGI1RN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBGL/GLGL1RN.CPY".
      * COPY "LIBGL/GLGT1IN.CPY".
      * COPYMEMBER: LIBGL/GLGT1IN
      ******************************************************************
      * REV:
      * JKC 2023-1019 COMMENTED OUT START-GT1-FILE-GREATER; ONE PROGRAM
      *          (GL/DELDUP) ONLY USES IT; DUE TO THE COMPLEX GT1-KEY I
      *          DO NOT BELIEVE IT IS PROPERLY SETUP FOR IT TO WORK
      *          CORRECTLY; AFTER DISCUSSING WITH KEVIN MULLEN, DELDUP
      *          WAS ONLY USED ONCE BUT WOULD BE NICE TO HAVE IN CASE
      *          NEEDED FOR AN EMERGENCY; RON ZAMISKA WAS ABLE TO
      *          ACCOMPLISH WHAT DELDUP DOES WITHIN A SQL CODE; WE
      *          ARCHIVED DELDUP FROM USE SO THE USE OF
      *          START-GT1-FILE-GREATER IS NO LONGER NEEDED.
      ******************************************************************
       LOAD-GT1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( GT-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( GT-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO GT-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO GT-ALL-MACHINE
              MOVE GT-ALL-PATH             TO GT-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( GT-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO GT-ALL-BASE
              MOVE FILPATH-MACHINE         TO GT-ALL-MACHINE
              MOVE GT-ALL-PATH             TO GT-PATH.

      *-----------------------------------------------------------------
       OPEN-GT1-FILE.
           PERFORM LOAD-GT1-FILE.
           PERFORM OPEN-IT.
           MOVE GT-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-GT1-FILE.
           PERFORM START-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT1-KEY TO E-KEYX.
           IF ( GT1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-GT1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  DEFAULT THE WBEG AND WEND; TO GET A START TO WORK IN SQL LIKE
      *  IT DOES IN VISION FILES FOR MULTIPLE SCAN PAGES, NEED TO ADD
      *  ONE TO THE WHERE SCAN BEG (WSBEG).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( QGT1-WEND-LINE = ZEROES    ) OR
              ( QGT1-WEND-LINE NOT NUMERIC )
              MOVE GT-SOURCE            TO QGT1-WBEG-SOURCE
              MOVE 9                    TO QGT1-WEND-SOURCE

              MOVE GT-DATE2             TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WBEG-DATE2
              MOVE GT-DATE3             TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WBEG-DATE3

              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WEND-DATE2
                                           QGT1-WEND-DATE3

              MOVE GT-BRANCH            TO QGT1-WBEG-BRANCH
              MOVE ALL "9"              TO QGT1-WEND-BRANCH
              MOVE GT-ACCTNO            TO QGT1-WBEG-ACCTNO
              MOVE ALL "9"              TO QGT1-WEND-ACCTNO
              MOVE GT-DRCRFG            TO QGT1-WBEG-DRCRFG
              MOVE ALL "Z"              TO QGT1-WEND-DRCRFG
              MOVE GT-LINE              TO QGT1-WBEG-LINE
              MOVE ALL "9"              TO QGT1-WEND-LINE
      *-   -   -{ ADD ONE TO WHERE SCAN BEG FIELDS }
              IF ( GT-SOURCE = 9 )
                 MOVE GT-SOURCE         TO QGT1-WSBEG-SOURCE
              ELSE
                 COMPUTE QGT1-WSBEG-SOURCE = GT-SOURCE + 1
              END-IF

              MOVE GT-DATE2             TO NDTE-DATE
              MOVE 1                    TO NDTE-HOLD
              PERFORM INCREMENT-DAYS
              MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WSBEG-DATE2

              IF ( GT-BRANCH = 9999 )
                 MOVE GT-BRANCH         TO QGT1-WSBEG-BRANCH
              ELSE
                 COMPUTE QGT1-WSBEG-BRANCH = GT-BRANCH + 1
              END-IF

              IF ( GT-ACCTNO = 99999999 )
                 MOVE GT-ACCTNO         TO QGT1-WSBEG-ACCTNO
              ELSE
                 COMPUTE QGT1-WSBEG-ACCTNO = GT-ACCTNO + 1
              END-IF

              MOVE GT-DATE3             TO NDTE-DATE
              MOVE 1                    TO NDTE-HOLD
              PERFORM INCREMENT-DAYS
              MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WSBEG-DATE3

              MOVE 01                   TO SQL-TEXT-SIZE
              MOVE GT-DRCRFG            TO SQL-TEXT
              PERFORM SQL-TEXT-INCREMENT
              MOVE SQL-TEXT             TO QGT1-WSBEG-DRCRFG.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           EXEC SQL
            DECLARE GT1_CURSOR CURSOR FOR
            SELECT GTFILE.GT_FLAG,
                   GTFILE.GT_REF,
                   CAST(GTFILE.GT_DATE1 AS VARCHAR(10)),
                   GTFILE.GT_SOURCE,
                   CAST(GTFILE.GT_DATE2 AS VARCHAR(10)),
                   GTFILE.GT_BRANCH,
                   GTFILE.GT_ACCTNO,
                   CAST(GTFILE.GT_DATE3 AS VARCHAR(10)),
                   GTFILE.GT_DRCRFG,
                   GTFILE.GT_LINE,
                   CAST(GTFILE.GT_LTOUCH_DATE AS VARCHAR(10)),
                   GTFILE.GT_AMT,
                   GTFILE.GT_EXPL,
                   GTFILE.GT_PERIOD,
                   GTFILE.GT_SMITH_SENT,
                   GTFILE.GT_POST_USERID,
                   CAST(GTFILE.GT_POST_DATE
                                               AS VARCHAR(10)),
                   GTFILE.GT_POST_TIME
            FROM DBO.GTFILE
            WHERE (( GTFILE.GT_SOURCE     >= :QGT1-WBEG-SOURCE
              AND    GTFILE.GT_DATE2      >= :QGT1-WBEG-DATE2 
              AND    GTFILE.GT_BRANCH     >= :QGT1-WBEG-BRANCH
              AND    GTFILE.GT_ACCTNO     >= :QGT1-WBEG-ACCTNO
              AND    GTFILE.GT_DATE3      >= :QGT1-WBEG-DATE3
              AND    GTFILE.GT_DRCRFG     >= :QGT1-WBEG-DRCRFG
              AND    GTFILE.GT_LINE       >= :QGT1-WBEG-LINE )
               OR  ( GTFILE.GT_SOURCE     >= :QGT1-WBEG-SOURCE
              AND    GTFILE.GT_DATE2      >= :QGT1-WBEG-DATE2
              AND    GTFILE.GT_BRANCH     >= :QGT1-WBEG-BRANCH
              AND    GTFILE.GT_ACCTNO     >= :QGT1-WBEG-ACCTNO
              AND    GTFILE.GT_DATE3      >= :QGT1-WBEG-DATE3 
              AND    GTFILE.GT_DRCRFG     >= :QGT1-WSBEG-DRCRFG
              AND    GTFILE.GT_LINE       >= 0 )
               OR  ( GTFILE.GT_SOURCE     >= :QGT1-WBEG-SOURCE
              AND    GTFILE.GT_DATE2      >= :QGT1-WBEG-DATE2
              AND    GTFILE.GT_BRANCH     >= :QGT1-WBEG-BRANCH
              AND    GTFILE.GT_ACCTNO     >= :QGT1-WBEG-ACCTNO
              AND    GTFILE.GT_DATE3      >= :QGT1-WSBEG-DATE3 
              AND    GTFILE.GT_DRCRFG     >= ''
              AND    GTFILE.GT_LINE       >= 0 )
               OR  ( GTFILE.GT_SOURCE     >= :QGT1-WBEG-SOURCE
              AND    GTFILE.GT_DATE2      >= :QGT1-WBEG-DATE2
              AND    GTFILE.GT_BRANCH     >= :QGT1-WBEG-BRANCH
              AND    GTFILE.GT_ACCTNO     >= :QGT1-WSBEG-ACCTNO
              AND    GTFILE.GT_DATE3      >= '1900-01-01' 
              AND    GTFILE.GT_DRCRFG     >= ''
              AND    GTFILE.GT_LINE       >= 0 )
               OR  ( GTFILE.GT_SOURCE     >= :QGT1-WBEG-SOURCE
              AND    GTFILE.GT_DATE2      >= :QGT1-WBEG-DATE2 
              AND    GTFILE.GT_BRANCH     >= :QGT1-WSBEG-BRANCH
              AND    GTFILE.GT_ACCTNO     >= 0
              AND    GTFILE.GT_DATE3      >= '1900-01-01' 
              AND    GTFILE.GT_DRCRFG     >= ''
              AND    GTFILE.GT_LINE       >= 0 )
               OR  ( GTFILE.GT_SOURCE     >= :QGT1-WBEG-SOURCE
              AND    GTFILE.GT_DATE2 
                                          >= :QGT1-WSBEG-DATE2
              AND    GTFILE.GT_BRANCH     >= 0
              AND    GTFILE.GT_ACCTNO     >= 0
              AND    GTFILE.GT_DATE3      >= '1900-01-01' 
              AND    GTFILE.GT_DRCRFG     >= ''
              AND    GTFILE.GT_LINE       >= 0 )
               OR  ( GTFILE.GT_SOURCE     >= :QGT1-WSBEG-SOURCE
              AND    GTFILE.GT_DATE2      >= '1900-01-01' 
              AND    GTFILE.GT_BRANCH     >= 0
              AND    GTFILE.GT_ACCTNO     >= 0
              AND    GTFILE.GT_DATE3      >= '1900-01-01' 
              AND    GTFILE.GT_DRCRFG     >= ''
              AND    GTFILE.GT_LINE       >= 0 ))
              AND (  GTFILE.GT_SOURCE     <= :QGT1-WEND-SOURCE
              AND    GTFILE.GT_DATE2      <= :QGT1-WEND-DATE2 
              AND    GTFILE.GT_BRANCH     <= :QGT1-WEND-BRANCH
              AND    GTFILE.GT_ACCTNO     <= :QGT1-WEND-ACCTNO
              AND    GTFILE.GT_DATE3      <= :QGT1-WEND-DATE3 
              AND    GTFILE.GT_DRCRFG     <= :QGT1-WEND-DRCRFG
              AND    GTFILE.GT_LINE       <= :QGT1-WEND-LINE )

            ORDER BY GTFILE.GT_SOURCE,
                     GTFILE.GT_DATE2,
                     GTFILE.GT_BRANCH,
                     GTFILE.GT_ACCTNO,
                     GTFILE.GT_DATE3,
                     GTFILE.GT_DRCRFG,
                     GTFILE.GT_LINE
           END-EXEC.

           EXEC SQL
               OPEN GT1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QGT1-WBEG-DRCRFG
                          QGT1-WEND-DRCRFG
                          QGT1-WBEG-DATE2
                          QGT1-WEND-DATE2
                          QGT1-WBEG-DATE3
                          QGT1-WEND-DATE3
                          QGT1-WSBEG-DATE2
                          QGT1-WSBEG-DATE3
                          QGT1-WSBEG-DRCRFG.

           MOVE ZEROES TO QGT1-WBEG-BRANCH
                          QGT1-WEND-BRANCH
                          QGT1-WBEG-ACCTNO
                          QGT1-WEND-ACCTNO
                          QGT1-WBEG-LINE
                          QGT1-WEND-LINE
                          QGT1-WBEG-SOURCE
                          QGT1-WEND-SOURCE
                          QGT1-WSBEG-SOURCE
                          QGT1-WSBEG-BRANCH
                          QGT1-WSBEG-ACCTNO.

           MOVE STAT---GOOD TO GT1-CURSOR-STAT.

      *-----------------------------------------------------------------
      * START-GT1-FILE-GREATER.
      *     PERFORM START-IT.
      *     MOVE GT-PATH-SQL TO E-FILE.
      *     MOVE GT1-KEY TO E-KEYX.

      *     IF ( GT1-CURSOR-GR-STAT-GOOD )
      *        PERFORM CLOSE-GT1-FILE.

      **- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      **  DEFAULT THE WBEG AND WEND; TO GET A START TO WORK IN SQL LIKE
      **  IT DOES IN VISION FILES FOR MULTIPLE SCAN PAGES, NEED TO ADD
      **  ONE TO THE WHERE SCAN BEG (WSBEG).
      **- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      *     IF ( QGT1-WEND-LINE = ZEROES    ) OR
      *        ( QGT1-WEND-LINE NOT NUMERIC )
      *        MOVE GT-SOURCE            TO QGT1-WBEG-SOURCE
      *        MOVE 9                    TO QGT1-WEND-SOURCE

      *        MOVE GT-DATE2             TO SQL-DATE-YYYYMMDD
      *        PERFORM SQL-SET-DATE
      *        MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WBEG-DATE2
      *        MOVE GT-DATE3             TO SQL-DATE-YYYYMMDD
      *        PERFORM SQL-SET-DATE
      *        MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WBEG-DATE3

      *        MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
      *        PERFORM SQL-SET-DATE
      *        MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WEND-DATE2
      *                                     QGT1-WEND-DATE3

      *        MOVE GT-BRANCH            TO QGT1-WBEG-BRANCH
      *        MOVE ALL "9"              TO QGT1-WEND-BRANCH
      *        MOVE GT-ACCTNO            TO QGT1-WBEG-ACCTNO
      *        MOVE ALL "9"              TO QGT1-WEND-ACCTNO
      *        MOVE GT-DRCRFG            TO QGT1-WBEG-DRCRFG
      *        MOVE ALL "Z"              TO QGT1-WEND-DRCRFG
      *        MOVE GT-LINE              TO QGT1-WBEG-LINE
      *        MOVE ALL "9"              TO QGT1-WEND-LINE
      **-   -   -{ ADD ONE TO WHERE SCAN BEG FIELDS }
      *        IF ( GT-SOURCE = 9 )
      *           MOVE GT-SOURCE         TO QGT1-WSBEG-SOURCE
      *        ELSE
      *           COMPUTE QGT1-WSBEG-SOURCE = GT-SOURCE + 1
      *        END-IF

      *        MOVE GT-DATE2             TO NDTE-DATE
      *        MOVE 1                    TO NDTE-HOLD
      *        PERFORM INCREMENT-DAYS
      *        MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
      *        PERFORM SQL-SET-DATE
      *        MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WSBEG-DATE2

      *        IF ( GT-BRANCH = 9999 )
      *           MOVE GT-BRANCH         TO QGT1-WSBEG-BRANCH
      *        ELSE
      *           COMPUTE QGT1-WSBEG-BRANCH = GT-BRANCH + 1
      *        END-IF

      *        IF ( GT-ACCTNO = 99999999 )
      *           MOVE GT-ACCTNO         TO QGT1-WSBEG-ACCTNO
      *        ELSE
      *           COMPUTE QGT1-WSBEG-ACCTNO = GT-ACCTNO + 1
      *        END-IF

      *        MOVE GT-DATE3             TO NDTE-DATE
      *        MOVE 1                    TO NDTE-HOLD
      *        PERFORM INCREMENT-DAYS
      *        MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
      *        PERFORM SQL-SET-DATE
      *        MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WSBEG-DATE3

      *        MOVE 01                   TO SQL-TEXT-SIZE
      *        MOVE GT-DRCRFG            TO SQL-TEXT
      *        PERFORM SQL-TEXT-INCREMENT
      *        MOVE SQL-TEXT             TO QGT1-WSBEG-DRCRFG.

      **- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      *     EXEC SQL
      *      DECLARE GT1_CURSOR_GR CURSOR FOR
      *      SELECT GTFILE.GT_FLAG,
      *             GTFILE.GT_REF,
      *             CAST(GTFILE.GT_DATE1 AS VARCHAR(10)),
      *             GTFILE.GT_SOURCE,
      *             CAST(GTFILE.GT_DATE2 AS VARCHAR(10)),
      *             GTFILE.GT_BRANCH,
      *             GTFILE.GT_ACCTNO,
      *             CAST(GTFILE.GT_DATE3 AS VARCHAR(10)),
      *             GTFILE.GT_DRCRFG,
      *             GTFILE.GT_LINE,
      *             CAST(GTFILE.GT_LTOUCH_DATE AS VARCHAR(10)),
      *             GTFILE.GT_AMT,
      *             GTFILE.GT_EXPL,
      *             GTFILE.GT_PERIOD,
      *             GTFILE.GT_SMITH_SENT,
      *             GTFILE.GT_POST_USERID,
      *             CAST(GTFILE.GT_POST_DATE AS VARCHAR(10)),
      *             GTFILE.GT_POST_TIME
      *      FROM DBO.GTFILE
      *      WHERE (( GTFILE.GT_SOURCE     >  :QGT1-WBEG-SOURCE
      *        AND    GTFILE.GT_DATE2      >  :QGT1-WBEG-DATE2 
      *        AND    GTFILE.GT_BRANCH     >  :QGT1-WBEG-BRANCH
      *        AND    GTFILE.GT_ACCTNO     >  :QGT1-WBEG-ACCTNO
      *        AND    GTFILE.GT_DATE3      >  :QGT1-WBEG-DATE3 
      *        AND    GTFILE.GT_DRCRFG     >  :QGT1-WBEG-DRCRFG
      *        AND    GTFILE.GT_LINE       >  :QGT1-WBEG-LINE )
      *         OR  ( GTFILE.GT_SOURCE     >  :QGT1-WBEG-SOURCE
      *        AND    GTFILE.GT_DATE2      >  :QGT1-WBEG-DATE2 
      *        AND    GTFILE.GT_BRANCH     >  :QGT1-WBEG-BRANCH
      *        AND    GTFILE.GT_ACCTNO     >  :QGT1-WBEG-ACCTNO
      *        AND    GTFILE.GT_DATE3      >  :QGT1-WBEG-DATE3 
      *        AND    GTFILE.GT_DRCRFG     >  :QGT1-WSBEG-DRCRFG
      *        AND    GTFILE.GT_LINE       >  0 )
      *         OR  ( GTFILE.GT_SOURCE     >  :QGT1-WBEG-SOURCE
      *        AND    GTFILE.GT_DATE2      >  :QGT1-WBEG-DATE2 
      *        AND    GTFILE.GT_BRANCH     >  :QGT1-WBEG-BRANCH
      *        AND    GTFILE.GT_ACCTNO     >  :QGT1-WBEG-ACCTNO
      *        AND    GTFILE.GT_DATE3 
      *                                    >  :QGT1-WSBEG-DATE3
      *        AND    GTFILE.GT_DRCRFG     >  ''
      *        AND    GTFILE.GT_LINE       >  0 )
      *         OR  ( GTFILE.GT_SOURCE     >  :QGT1-WBEG-SOURCE
      *        AND    GTFILE.GT_DATE2      >  :QGT1-WBEG-DATE2 
      *        AND    GTFILE.GT_BRANCH     >  :QGT1-WBEG-BRANCH
      *        AND    GTFILE.GT_ACCTNO     >  :QGT1-WSBEG-ACCTNO
      *        AND    GTFILE.GT_DATE3      >  '1900-01-01' 
      *        AND    GTFILE.GT_DRCRFG     >  ''
      *        AND    GTFILE.GT_LINE       >  0 )
      *         OR  ( GTFILE.GT_SOURCE     >  :QGT1-WBEG-SOURCE
      *        AND    GTFILE.GT_DATE2      >  :QGT1-WBEG-DATE2 
      *        AND    GTFILE.GT_BRANCH     >  :QGT1-WSBEG-BRANCH
      *        AND    GTFILE.GT_ACCTNO     >  0
      *        AND    GTFILE.GT_DATE3      >  '1900-01-01' 
      *        AND    GTFILE.GT_DRCRFG     >  ''
      *        AND    GTFILE.GT_LINE       >  0 )
      *         OR  ( GTFILE.GT_SOURCE     >  :QGT1-WBEG-SOURCE
      *        AND    GTFILE.GT_DATE2      >  :QGT1-WSBEG-DATE2 
      *        AND    GTFILE.GT_BRANCH     >  0
      *        AND    GTFILE.GT_ACCTNO     >  0
      *        AND    GTFILE.GT_DATE3      >  '1900-01-01' 
      *        AND    GTFILE.GT_DRCRFG     >  ''
      *        AND    GTFILE.GT_LINE       >  0 )
      *         OR  ( GTFILE.GT_SOURCE     >  :QGT1-WSBEG-SOURCE
      *        AND    GTFILE.GT_DATE2      >  '1900-01-01'
      *        AND    GTFILE.GT_BRANCH     >  0
      *        AND    GTFILE.GT_ACCTNO     >  0
      *        AND    GTFILE.GT_DATE3      >  '1900-01-01'
      *        AND    GTFILE.GT_DRCRFG     >  ''
      *        AND    GTFILE.GT_LINE       >  0 ))
      *        AND (  GTFILE.GT_SOURCE     <= :QGT1-WEND-SOURCE
      *        AND    GTFILE.GT_DATE2      <= :QGT1-WEND-DATE2
      *        AND    GTFILE.GT_BRANCH     <= :QGT1-WEND-BRANCH
      *        AND    GTFILE.GT_ACCTNO     <= :QGT1-WEND-ACCTNO
      *        AND    GTFILE.GT_DATE3      <= :QGT1-WEND-DATE3 
      *        AND    GTFILE.GT_DRCRFG     <= :QGT1-WEND-DRCRFG
      *        AND    GTFILE.GT_LINE       <= :QGT1-WEND-LINE )
      *      ORDER BY GTFILE.GT_SOURCE,
      *               GTFILE.GT_DATE2,
      *               GTFILE.GT_BRANCH,
      *               GTFILE.GT_ACCTNO,
      *               GTFILE.GT_DATE3,
      *               GTFILE.GT_DRCRFG,
      *               GTFILE.GT_LINE
      *     END-EXEC.

      *     EXEC SQL
      *         OPEN GT1_CURSOR_GR
      *     END-EXEC.

      *     PERFORM SQL-IO-VALIDATION.

      *     MOVE SPACES TO QGT1-WBEG-DRCRFG
      *                    QGT1-WEND-DRCRFG
      *                    QGT1-WBEG-DATE2
      *                    QGT1-WEND-DATE2
      *                    QGT1-WBEG-DATE3
      *                    QGT1-WEND-DATE3
      *                    QGT1-WSBEG-DATE2
      *                    QGT1-WSBEG-DATE3
      *                    QGT1-WSBEG-DRCRFG.

      *     MOVE ZEROES TO QGT1-WBEG-BRANCH
      *                    QGT1-WEND-BRANCH
      *                    QGT1-WBEG-ACCTNO
      *                    QGT1-WEND-ACCTNO
      *                    QGT1-WBEG-LINE
      *                    QGT1-WEND-LINE
      *                    QGT1-WBEG-SOURCE
      *                    QGT1-WEND-SOURCE
      *                    QGT1-WSBEG-SOURCE
      *                    QGT1-WSBEG-BRANCH
      *                    QGT1-WSBEG-ACCTNO.

      *     MOVE STAT---GOOD TO GT1-CURSOR-GR-STAT.

      *-----------------------------------------------------------------
       START-GT1-FILE-NOT-GREATER.
           PERFORM START-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT1-KEY TO E-KEYX.

           IF ( GT1-CURSOR-NG-STAT-GOOD )
              PERFORM CLOSE-GT1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  DEFAULT THE WBEG AND WEND; TO GET A START TO WORK IN SQL LIKE
      *  IT DOES IN VISION FILES FOR MULTIPLE SCAN PAGES, NEED TO 
      *  !!SUBTRACT!! ONE TO THE WHERE SCAN BEG (WSBEG).
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( QGT1-WEND-LINE = ZEROES    ) OR
              ( QGT1-WEND-LINE NOT NUMERIC )
              MOVE 0                    TO QGT1-WBEG-SOURCE
              MOVE GT-SOURCE            TO QGT1-WEND-SOURCE

              MOVE EXT-JULIAN-BEG       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WBEG-DATE2
                                           QGT1-WBEG-DATE3
              MOVE GT-DATE2             TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WEND-DATE2
              MOVE GT-DATE3             TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WEND-DATE3

              MOVE ALL "0"              TO QGT1-WBEG-BRANCH
              MOVE GT-BRANCH            TO QGT1-WEND-BRANCH
              MOVE ALL "0"              TO QGT1-WBEG-ACCTNO
              MOVE GT-ACCTNO            TO QGT1-WEND-ACCTNO
              MOVE ALL "A"              TO QGT1-WBEG-DRCRFG
              MOVE GT-DRCRFG            TO QGT1-WEND-DRCRFG
              MOVE ALL "0"              TO QGT1-WBEG-LINE
              MOVE GT-LINE              TO QGT1-WEND-LINE.
      *-   -   -{ SUBTRACT ONE TO WHERE SCAN BEG FIELDS }
              IF ( GT-SOURCE = 0 )
                 MOVE GT-SOURCE         TO QGT1-WSBEG-SOURCE
              ELSE
                 COMPUTE QGT1-WSBEG-SOURCE = GT-SOURCE - 1
              END-IF

              MOVE GT-DATE2             TO NDTE-DATE
              MOVE -1                   TO NDTE-HOLD
              PERFORM INCREMENT-DAYS
              MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WSBEG-DATE2

              IF ( GT-BRANCH = 0 )
                 MOVE GT-BRANCH         TO QGT1-WSBEG-BRANCH
              ELSE
                 COMPUTE QGT1-WSBEG-BRANCH = GT-BRANCH - 1
              END-IF

              IF ( GT-ACCTNO = 0 )
                 MOVE GT-ACCTNO         TO QGT1-WSBEG-ACCTNO
              ELSE
                 COMPUTE QGT1-WSBEG-ACCTNO = GT-ACCTNO - 1
              END-IF

              MOVE GT-DATE3             TO NDTE-DATE
              MOVE -1                   TO NDTE-HOLD
              PERFORM INCREMENT-DAYS
              MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WSBEG-DATE3

              MOVE GT-DRCRFG            TO SQL-TEXT
              PERFORM SQL-TEXT-DECREMENT
              MOVE SQL-TEXT             TO QGT1-WSBEG-DRCRFG.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           EXEC SQL
            DECLARE GT1_CURSOR_NG CURSOR FOR
            SELECT GTFILE.GT_FLAG,
                   GTFILE.GT_REF,
                   CAST(GTFILE.GT_DATE1 AS VARCHAR(10)),
                   GTFILE.GT_SOURCE,
                   CAST(GTFILE.GT_DATE2 AS VARCHAR(10)),
                   GTFILE.GT_BRANCH,
                   GTFILE.GT_ACCTNO,
                   CAST(GTFILE.GT_DATE3 AS VARCHAR(10)),
                   GTFILE.GT_DRCRFG,
                   GTFILE.GT_LINE,
                   CAST(GTFILE.GT_LTOUCH_DATE AS VARCHAR(10)),
                   GTFILE.GT_AMT,
                   GTFILE.GT_EXPL,
                   GTFILE.GT_PERIOD,
                   GTFILE.GT_SMITH_SENT,
                   GTFILE.GT_POST_USERID,
                   CAST(GTFILE.GT_POST_DATE AS VARCHAR(10)),
                   GTFILE.GT_POST_TIME
            FROM DBO.GTFILE
            WHERE (( GTFILE.GT_SOURCE     >= :QGT1-WBEG-SOURCE
              AND    GTFILE.GT_DATE2      >= :QGT1-WBEG-DATE2
              AND    GTFILE.GT_BRANCH     >= :QGT1-WBEG-BRANCH
              AND    GTFILE.GT_ACCTNO     >= :QGT1-WBEG-ACCTNO
              AND    GTFILE.GT_DATE3      >= :QGT1-WBEG-DATE3 
              AND    GTFILE.GT_DRCRFG     >= :QGT1-WBEG-DRCRFG
              AND    GTFILE.GT_LINE       >= :QGT1-WBEG-LINE )
               OR  ( GTFILE.GT_SOURCE     >= :QGT1-WBEG-SOURCE
              AND    GTFILE.GT_DATE2      >= :QGT1-WBEG-DATE2 
              AND    GTFILE.GT_BRANCH     >= :QGT1-WBEG-BRANCH
              AND    GTFILE.GT_ACCTNO     >= :QGT1-WBEG-ACCTNO
              AND    GTFILE.GT_DATE3      >= :QGT1-WBEG-DATE3
              AND    GTFILE.GT_DRCRFG     >= :QGT1-WSBEG-DRCRFG
              AND    GTFILE.GT_LINE       >= 0 )
               OR  ( GTFILE.GT_SOURCE     >= :QGT1-WBEG-SOURCE
              AND    GTFILE.GT_DATE2      >= :QGT1-WBEG-DATE2 
              AND    GTFILE.GT_BRANCH     >= :QGT1-WBEG-BRANCH
              AND    GTFILE.GT_ACCTNO     >= :QGT1-WBEG-ACCTNO
              AND    GTFILE.GT_DATE3      >= :QGT1-WSBEG-DATE3 
              AND    GTFILE.GT_DRCRFG     >= ''
              AND    GTFILE.GT_LINE       >= 0 )
               OR  ( GTFILE.GT_SOURCE     >= :QGT1-WBEG-SOURCE
              AND    GTFILE.GT_DATE2      >= :QGT1-WBEG-DATE2
              AND    GTFILE.GT_BRANCH     >= :QGT1-WBEG-BRANCH
              AND    GTFILE.GT_ACCTNO     >= :QGT1-WSBEG-ACCTNO
              AND    GTFILE.GT_DATE3      >= '1900-01-01' 
              AND    GTFILE.GT_DRCRFG     >= ''
              AND    GTFILE.GT_LINE       >= 0 )
               OR  ( GTFILE.GT_SOURCE     >= :QGT1-WBEG-SOURCE
              AND    GTFILE.GT_DATE2      >= :QGT1-WBEG-DATE2 
              AND    GTFILE.GT_BRANCH     >= :QGT1-WSBEG-BRANCH
              AND    GTFILE.GT_ACCTNO     >= 0
              AND    GTFILE.GT_DATE3      >= '1900-01-01'
              AND    GTFILE.GT_DRCRFG     >= ''
              AND    GTFILE.GT_LINE       >= 0 )
               OR  ( GTFILE.GT_SOURCE     >= :QGT1-WBEG-SOURCE
              AND    GTFILE.GT_DATE2 
                                          >= :QGT1-WSBEG-DATE2
              AND    GTFILE.GT_BRANCH     >= 0
              AND    GTFILE.GT_ACCTNO     >= 0
              AND    GTFILE.GT_DATE3      >= '1900-01-01' 
              AND    GTFILE.GT_DRCRFG     >= ''
              AND    GTFILE.GT_LINE       >= 0 )
               OR  ( GTFILE.GT_SOURCE     >= :QGT1-WSBEG-SOURCE
              AND    GTFILE.GT_DATE2      >= '1900-01-01'
              AND    GTFILE.GT_BRANCH     >= 0
              AND    GTFILE.GT_ACCTNO     >= 0
              AND    GTFILE.GT_DATE3      >= '1900-01-01'
              AND    GTFILE.GT_DRCRFG     >= ''
              AND    GTFILE.GT_LINE       >= 0 ))
              AND (  GTFILE.GT_SOURCE     <= :QGT1-WEND-SOURCE
              AND    GTFILE.GT_DATE2      <= :QGT1-WEND-DATE2 
              AND    GTFILE.GT_BRANCH     <= :QGT1-WEND-BRANCH
              AND    GTFILE.GT_ACCTNO     <= :QGT1-WEND-ACCTNO
              AND    GTFILE.GT_DATE3      <= :QGT1-WEND-DATE3 
              AND    GTFILE.GT_DRCRFG     <= :QGT1-WEND-DRCRFG
              AND    GTFILE.GT_LINE       <= :QGT1-WEND-LINE )
           ORDER BY GTFILE.GT_SOURCE DESC,
                    GTFILE.GT_DATE2 DESC,
                    GTFILE.GT_BRANCH DESC,
                    GTFILE.GT_ACCTNO DESC,
                    GTFILE.GT_DATE3 DESC,
                    GTFILE.GT_DRCRFG DESC,
                    GTFILE.GT_LINE DESC
           END-EXEC.

           EXEC SQL
               OPEN GT1_CURSOR_NG
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QGT1-WBEG-DRCRFG
                          QGT1-WEND-DRCRFG
                          QGT1-WBEG-DATE2
                          QGT1-WEND-DATE2
                          QGT1-WBEG-DATE3
                          QGT1-WEND-DATE3
                          QGT1-WSBEG-DATE2
                          QGT1-WSBEG-DATE3
                          QGT1-WSBEG-DRCRFG.

           MOVE ZEROES TO QGT1-WBEG-BRANCH
                          QGT1-WEND-BRANCH
                          QGT1-WBEG-ACCTNO
                          QGT1-WEND-ACCTNO
                          QGT1-WBEG-LINE
                          QGT1-WEND-LINE
                          QGT1-WBEG-SOURCE
                          QGT1-WEND-SOURCE
                          QGT1-WSBEG-SOURCE
                          QGT1-WSBEG-BRANCH
                          QGT1-WSBEG-ACCTNO.

           MOVE STAT---GOOD TO GT1-CURSOR-NG-STAT.

      *-----------------------------------------------------------------
       READ-GT1-FILE.
           PERFORM READ-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT1-KEY TO E-KEYX.
           INITIALIZE QGT-REC.

           MOVE GT-SOURCE           TO QGT-SOURCE.
           MOVE GT-DATE2            TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QGT-DATE2.
           MOVE GT-BRANCH           TO QGT-BRANCH.
           MOVE GT-ACCTNO           TO QGT-ACCTNO.
           MOVE GT-DATE3            TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QGT-DATE3.
           MOVE GT-DRCRFG           TO QGT-DRCRFG.
           MOVE GT-LINE             TO QGT-LINE.

           EXEC SQL
            SELECT GTFILE.GT_FLAG,
                   GTFILE.GT_REF,
                   CAST(GTFILE.GT_DATE1 AS VARCHAR(10)),
                   GTFILE.GT_SOURCE,
                   CAST(GTFILE.GT_DATE2 AS VARCHAR(10)),
                   GTFILE.GT_BRANCH,
                   GTFILE.GT_ACCTNO,
                   CAST(GTFILE.GT_DATE3 AS VARCHAR(10)),
                   GTFILE.GT_DRCRFG,
                   GTFILE.GT_LINE,
                   CAST(GTFILE.GT_LTOUCH_DATE AS VARCHAR(10)),
                   GTFILE.GT_AMT,
                   GTFILE.GT_EXPL,
                   GTFILE.GT_PERIOD,
                   GTFILE.GT_SMITH_SENT,
                   GTFILE.GT_POST_USERID,
                   CAST(GTFILE.GT_POST_DATE AS VARCHAR(10)),
                   GTFILE.GT_POST_TIME
            INTO :QGT-REC
            FROM DBO.GTFILE
            WHERE GTFILE.GT_SOURCE = :QGT-SOURCE
              AND GTFILE.GT_DATE2 = :QGT-DATE2
              AND GTFILE.GT_BRANCH = :QGT-BRANCH
              AND GTFILE.GT_ACCTNO = :QGT-ACCTNO
              AND GTFILE.GT_DATE3 = :QGT-DATE3
              AND GTFILE.GT_DRCRFG = :QGT-DRCRFG
              AND GTFILE.GT_LINE = :QGT-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GT1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-GT-FIELDS.

      *-----------------------------------------------------------------
       READ-GT1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT1-KEY TO E-KEYX.
           INITIALIZE QGT-REC.

           IF ( GT1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH GT1_CURSOR INTO :QGT-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GT1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-GT-FIELDS.


      *-----------------------------------------------------------------
       READ-GT1-FILE-PREVIOUS.
           PERFORM READ-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT1-KEY TO E-KEYX.
           INITIALIZE QGT-REC.

           IF ( GT1-CURSOR-NG-STAT-GOOD )
              EXEC SQL
                   FETCH GT1_CURSOR_NG INTO :QGT-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDPVCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GT1-FILE-PREVIOUS.

           IF ( IO-FG = 0 )
              PERFORM GET-GT-FIELDS.

      *-----------------------------------------------------------------
       WRITE-GT1-FILE.

           ACCEPT GT-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT1-KEY TO E-KEYX.
           PERFORM SET-GT-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            INSERT INTO DBO.GTFILE
             (GTFILE.GT_FLAG,
              GTFILE.GT_REF,
              GTFILE.GT_DATE1,
              GTFILE.GT_SOURCE,
              GTFILE.GT_DATE2,
              GTFILE.GT_BRANCH,
              GTFILE.GT_ACCTNO,
              GTFILE.GT_DATE3,
              GTFILE.GT_DRCRFG,
              GTFILE.GT_LINE,
              GTFILE.GT_LTOUCH_DATE,
              GTFILE.GT_AMT,
              GTFILE.GT_EXPL,
              GTFILE.GT_PERIOD,
              GTFILE.GT_SMITH_SENT,
              GTFILE.GT_POST_USERID,
              GTFILE.GT_POST_DATE,
              GTFILE.GT_POST_TIME)
            VALUES
             (:QGT-FLAG,
              :QGT-REF,
              :QGT-DATE1,
              :QGT-SOURCE,
              :QGT-DATE2,
              :QGT-BRANCH,
              :QGT-ACCTNO,
              :QGT-DATE3,
              :QGT-DRCRFG,
              :QGT-LINE,
              :QGT-LTOUCH-DATE,
              :QGT-AMT,
              :QGT-EXPL,
              :QGT-PERIOD,
              :QGT-SMITH-SENT,
              :QGT-POST-USERID,
              :QGT-POST-DATE,
              :QGT-POST-TIME)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-GT1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

           MOVE '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       REWRITE-GT1-FILE.

           ACCEPT GT-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT1-KEY TO E-KEYX.
           PERFORM SET-GT-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            UPDATE DBO.GTFILE
             SET
              GTFILE.GT_FLAG = :QGT-FLAG,
              GTFILE.GT_REF = :QGT-REF,
              GTFILE.GT_DATE1 = :QGT-DATE1,
              GTFILE.GT_SOURCE = :QGT-SOURCE,
              GTFILE.GT_DATE2 = :QGT-DATE2,
              GTFILE.GT_BRANCH = :QGT-BRANCH,
              GTFILE.GT_ACCTNO = :QGT-ACCTNO,
              GTFILE.GT_DATE3 = :QGT-DATE3,
              GTFILE.GT_DRCRFG = :QGT-DRCRFG,
              GTFILE.GT_LINE = :QGT-LINE,
              GTFILE.GT_LTOUCH_DATE = :QGT-LTOUCH-DATE,
              GTFILE.GT_AMT = :QGT-AMT,
              GTFILE.GT_EXPL = :QGT-EXPL,
              GTFILE.GT_PERIOD = :QGT-PERIOD,
              GTFILE.GT_SMITH_SENT = :QGT-SMITH-SENT,
              GTFILE.GT_POST_USERID = :QGT-POST-USERID,
              GTFILE.GT_POST_DATE = :QGT-POST-DATE,
              GTFILE.GT_POST_TIME = :QGT-POST-TIME
            WHERE GTFILE.GT_SOURCE = :QGT-SOURCE
              AND GTFILE.GT_DATE2 = :QGT-DATE2
              AND GTFILE.GT_BRANCH = :QGT-BRANCH
              AND GTFILE.GT_ACCTNO = :QGT-ACCTNO
              AND GTFILE.GT_DATE3 = :QGT-DATE3
              AND GTFILE.GT_DRCRFG = :QGT-DRCRFG
              AND GTFILE.GT_LINE = :QGT-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-GT1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

           MOVE '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       DELETE-GT1-FILE.

           PERFORM DELETE-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT1-KEY TO E-KEYX.
           PERFORM SET-GT-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            DELETE FROM DBO.GTFILE
            WHERE GTFILE.GT_SOURCE = :QGT-SOURCE
              AND GTFILE.GT_DATE2 = :QGT-DATE2
              AND GTFILE.GT_BRANCH = :QGT-BRANCH
              AND GTFILE.GT_ACCTNO = :QGT-ACCTNO
              AND GTFILE.GT_DATE3 = :QGT-DATE3
              AND GTFILE.GT_DRCRFG = :QGT-DRCRFG
              AND GTFILE.GT_LINE = :QGT-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-GT1-FILE.

           MOVE '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       CLOSE-GT1-FILE.

           PERFORM CLOSE-IT.

           IF ( GT1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE GT1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO GT1-CURSOR-STAT.

           IF ( GT1-CURSOR-NG-STAT-GOOD )
              EXEC SQL
                   CLOSE GT1_CURSOR_NG
              END-EXEC
              MOVE STAT---BAD TO GT1-CURSOR-NG-STAT.

      *    IF ( GT1-CURSOR-GR-STAT-GOOD )
      *       EXEC SQL
      *            CLOSE GT1_CURSOR_GR
      *       END-EXEC
      *       MOVE STAT---BAD TO GT1-CURSOR-GR-STAT.

      *-----------------------------------------------------------------
       OPEN-GT1-FILE-SSP.    
           PERFORM LOAD-GT1-FILE.
           PERFORM OPEN-IT.
           MOVE GT-PATH-SQL TO E-FILE.

           IF GT1-CONN-OPEN-SW = "N" 
              MOVE "Y"         TO GT1-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS GT1_SP_CONNECT 
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.   
 
      *-----------------------------------------------------------------
       START-GT1-FILE-SSP.  *>  SSP GTFILE
           PERFORM START-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT1-KEY TO E-KEYX.
           IF ( GT1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-GT1-FILE-SSP. 

      *  STORED PROCEDURE:
      *
      *  SELECT GTFILE.GT_SOURCE,
      *         CAST(GTFILE.GT_DATE2       AS VARCHAR(10)),
      *         GTFILE.GT_BRANCH,
      *         GTFILE.GT_ACCTNO,
      *         CAST(GTFILE.GT_DATE3       AS VARCHAR(10)),
      *         GTFILE.GT_DRCRFG,
      *         GTFILE.GT_LINE,
      *         GTFILE.GT_FLAG,
      *         GTFILE.GT_REF,
      *         CAST(GTFILE.GT_DATE1       AS VARCHAR(10)),
      *         CAST(GTFILE.GT_LTOUCH_DATE AS VARCHAR(10)),
      *         GTFILE.GT_AMT,
      *         GTFILE.GT_EXPL,
      *         GTFILE.GT_PERIOD,
      *         GTFILE.GT_SMITH_SENT,
      *         GTFILE.GT_POST_USERID,
      *         CAST(GTFILE.GT_POST_DATE   AS VARCHAR(10)),
      *         GTFILE.GT_POST_TIME
      *    INTO #TEMP_LPEOD5_GTFILE
      *    FROM DBO.GTFILE GTFILE
      *   WHERE GT_SOURCE = @QGT_SOURCE
      *     AND GT_DATE2  = @QGT_DATE2 
      *     AND GT_BRANCH = @QGT_BRANCH
      *     AND GT_ACCTNO = @QGT_ACCTNO
      *     AND GT_DATE3  = @QGT_DATE3 
      *     AND GT_DRCRFG = @QGT_DRCRFG
      *     AND GT_LINE = ( SELECT MAX(GT_LINE) 
      *                       FROM GTFILE
      *                      WHERE GT_SOURCE = @QGT_SOURCE
      *                        AND GT_DATE2  = @QGT_DATE2 
      *                        AND GT_BRANCH = @QGT_BRANCH
      *                        AND GT_ACCTNO = @QGT_ACCTNO
      *                        AND GT_DATE3  = @QGT_DATE3 
      *                        AND GT_DRCRFG = @QGT_DRCRFG ); 


           MOVE GT-SOURCE            TO QWS-GT-SOURCE.
           MOVE GT-DATE2             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QWS-GT-DATE2
           MOVE GT-BRANCH            TO QWS-GT-BRANCH.
           MOVE GT-ACCTNO            TO QWS-GT-ACCTNO.
           MOVE GT-DATE3             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QWS-GT-DATE3.    
           MOVE GT-DRCRFG            TO QWS-GT-DRCRFG.
                
           EXEC SQL SET CONNECTION GT1_SP_CONNECT END-EXEC.

           EXEC SQL
            DECLARE GT1_SP_CURSOR CURSOR FOR
                :GT1-RETURN-CODE = EXEC SSP_LPEOD5_GTFILE_SELECT 
              ( :QWS-GT-SOURCE, 
                :QWS-GT-DATE2,  
                :QWS-GT-BRANCH,
                :QWS-GT-ACCTNO, 
                :QWS-GT-DATE3, 
                :QWS-GT-DRCRFG )
           END-EXEC.

           EXEC SQL
               OPEN GT1_SP_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.  

           MOVE STAT---GOOD TO GT1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-GT1-FILE-NEXT-SSP.  *>  SSP GTFILE
           PERFORM READ-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT1-KEY TO E-KEYX.
           INITIALIZE QGT-REC.

           EXEC SQL SET CONNECTION GT1_SP_CONNECT END-EXEC.

           IF ( GT1-CURSOR-STAT-GOOD )
              EXEC SQL
                FETCH GT1_SP_CURSOR 
                  INTO 
                    :QGT-SOURCE,
                    :QGT-DATE2,
                    :QGT-BRANCH,
                    :QGT-ACCTNO,
                    :QGT-DATE3,
                    :QGT-DRCRFG,
                    :QGT-LINE,
                    :QGT-FLAG,
                    :QGT-REF,
                    :QGT-DATE1,       
                    :QGT-LTOUCH-DATE, 
                    :QGT-AMT,
                    :QGT-EXPL,
                    :QGT-PERIOD,
                    :QGT-SMITH-SENT,
                    :QGT-POST-USERID,
                    :QGT-POST-DATE,   
                    :QGT-POST-TIME
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GT1-FILE-NEXT-SSP.

           IF ( IO-FG = 0 )
              PERFORM GET-GT-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-GT1-FILE-SSP.
           PERFORM CLOSE-IT.
           IF ( GT1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION GT1_SP_CONNECT END-EXEC
              EXEC SQL
                   CLOSE GT1_SP_CURSOR
              END-EXEC
              MOVE STAT---BAD TO GT1-CURSOR-STAT
              EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-GT1-CONNECTION-SSP.

           IF GT1-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION GT1_SP_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT GT1_SP_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO GT1-CONN-OPEN-SW.

      *-----------------------------------------------------------------
   
      * COPY "LIBLP/LPBY1IN.CPY".
      * COPYMEMBER: LIBLP/LPBY1IN
      ******************************************************************
      *
      * DESCRIPTION: BRANCH STATISTICS FILE - IO ROUTINES - A THRU B
      *
      * FILE KEY   : #1 = DATE-CCYYMM, BRNO, CLASS
      *
      * FILE PATH  : /USR/DATA/R#/LP/BYAFILE
      *              /USR/DATA/R#/LP/BYBFILE
      *
      * ATTENTION  : BY-PATH-OVERRIDE CONTROLS ALL FILES - A THRU B
      *              ^^^^^^^^^^^^^^^^
      *              THERE IS NO BYA-PATH-OVERRIDE OR BYB-PATH-OVERRIDE
      *
      *=================================================================
      * REV:
      * KEC 2019-0205 ***NEW*** TO A32; MODIFIED FROM A30
      * KEC 2021-0524 ADDED ACUSQL VERSION
      ******************************************************************
       SET-ALL-TO-BY1.
           MOVE BYA1-KEY   TO BY1-KEY.
           MOVE BYA-AREA-A TO BY-AREA-A.
           MOVE BYB-AREA-B TO BY-AREA-B.

      *-----------------------------------------------------------------
       SET-BY1-TO-ALL.
           MOVE BY1-KEY    TO BYA1-KEY BYB1-KEY.
           MOVE BY-AREA-A  TO BYA-AREA-A.
           MOVE BY-AREA-B  TO BYB-AREA-B.

      *-----------------------------------------------------------------
       LOAD-BY1-FILE.
           PERFORM LOAD-BYA1-FILE.
           PERFORM LOAD-BYB1-FILE.

      *-----------------------------------------------------------------
       OPEN-BY1-FILE.
           PERFORM OPEN-BYA1-FILE.
           IF ( IO-FG = 0 )
              PERFORM OPEN-BYB1-FILE
              IF ( IO-FG = 0 )
                 MOVE " " TO IO-MULTI-FG
              ELSE
                 MOVE "B" TO IO-MULTI-FG
           ELSE
              MOVE "A" TO IO-MULTI-FG.

      *-----------------------------------------------------------------
       START-BY1-FILE.
           PERFORM SET-BY1-TO-ALL.
           PERFORM START-BYA1-FILE. *> SSP - BYAFILE
           IF ( IO-FG NOT = 0 )
              MOVE "A" TO IO-MULTI-FG.
             
      *-----------------------------------------------------------------
       READ-BY1-FILE.
           PERFORM SET-BY1-TO-ALL.
           PERFORM READ-BYA1-FILE.
           IF ( IO-FG = 0 )
              PERFORM READ-BYB1-FILE
              IF ( IO-FG = 0 )
                 PERFORM SET-ALL-TO-BY1
               ELSE
                 MOVE "B" TO IO-MULTI-FG
           ELSE
              MOVE "A" TO IO-MULTI-FG.
             
      *-----------------------------------------------------------------
       READ-BY1-FILE-NEXT.
           PERFORM SET-BY1-TO-ALL.
           PERFORM READ-BYA1-FILE-NEXT.
           IF ( IO-FG = 0 )
              MOVE BYA1-KEY TO BYB1-KEY
              PERFORM READ-BYB1-FILE
              IF ( IO-FG = 0 )
                 PERFORM SET-ALL-TO-BY1
               ELSE
                 MOVE "B" TO IO-MULTI-FG
           ELSE
              MOVE "A" TO IO-MULTI-FG.
             
      *-----------------------------------------------------------------
       WRITE-BY1-FILE.
           PERFORM SET-BY1-TO-ALL.
           PERFORM WRITE-BYA1-FILE.
           IF ( IO-FG = 0 )
              PERFORM WRITE-BYB1-FILE
              IF ( IO-FG = 0 )
                 PERFORM SET-ALL-TO-BY1
               ELSE
                 MOVE "B" TO IO-MULTI-FG
           ELSE
              MOVE "A" TO IO-MULTI-FG.

      *-----------------------------------------------------------------
       REWRITE-BY1-FILE.
           PERFORM SET-BY1-TO-ALL.
           PERFORM REWRITE-BYA1-FILE.
           IF ( IO-FG = 0 )
              PERFORM REWRITE-BYB1-FILE
              IF ( IO-FG = 0 )
                 PERFORM SET-ALL-TO-BY1
               ELSE
                 MOVE "B" TO IO-MULTI-FG
           ELSE
              MOVE "A" TO IO-MULTI-FG.

      *-----------------------------------------------------------------
       DELETE-BY1-FILE.
           PERFORM SET-BY1-TO-ALL.
           PERFORM DELETE-BYA1-FILE.
           IF ( IO-FG = 0 )
              PERFORM DELETE-BYB1-FILE
              IF ( IO-FG = 0 )
                 PERFORM SET-ALL-TO-BY1
               ELSE
                 MOVE "B" TO IO-MULTI-FG
           ELSE
              MOVE "A" TO IO-MULTI-FG.

      *-----------------------------------------------------------------
       CLOSE-BY1-FILE.

           PERFORM CLOSE-IT.
           PERFORM CLOSE-BYA1-FILE.
           PERFORM CLOSE-BYB1-FILE.

      *=================================================================
       LOAD-BYA1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BY-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BY-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BYA-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BYA-ALL-MACHINE
              MOVE BYA-ALL-PATH            TO BYA-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BY-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BYA-ALL-BASE
              MOVE FILPATH-MACHINE         TO BYA-ALL-MACHINE
              MOVE BYA-ALL-PATH            TO BYA-PATH.

      *-----------------------------------------------------------------
       OPEN-BYA1-FILE.
           PERFORM LOAD-BYA1-FILE.
           PERFORM OPEN-IT.
           MOVE BYA-PATH-SQL TO E-FILE.

           IF BY1-CONN-OPEN-SW = "N" 
              MOVE "Y"         TO BY1-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS BY1_CONNECT 
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-BYA1-FILE. *> SSP - BYAFILE
           PERFORM START-IT.
           MOVE BYA-PATH-SQL TO E-FILE.
           MOVE BYA1-KEY TO E-KEYX.

           IF ( BYA1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BYA1-FILE.

           IF ( QBY1-WEND-DATE-CCYYMM = ZEROES ) OR
              ( QBY1-WEND-DATE-CCYYMM NOT NUMERIC )
              MOVE BY-DATE-CCYYMM       TO QBY1-WBEG-DATE-CCYYMM
              MOVE ALL "9"              TO QBY1-WEND-DATE-CCYYMM 
              MOVE BY-BRNO              TO QBY1-WBEG-BRNO
              MOVE ALL "9"              TO QBY1-WEND-BRNO
              MOVE BY-CLASS             TO QBY1-WBEG-CLASS
              MOVE ALL "9"              TO QBY1-WEND-CLASS.

      *  STORED PROCEDURE:
      *
      *  SELECT BYAFILE.BYA_DATE_CCYYMM,
      *         BYAFILE.BYA_BRNO,
      *         BYAFILE.BYA_CLASS,
      *         CAST(BYAFILE.BYA_LTOUCH_DATE AS VARCHAR(10)) AS BYA_LTOUCH_DATE,
      *         BYAFILE.BYA_NO_1_1_1,
      *         BYAFILE.BYA_AMT_1_1_1,
      *         BYAFILE.BYA_O2_1_1_1,
      *         BYAFILE.BYA_INT_1_1_1,
      *         BYAFILE.BYA_NO_1_1_2,
      *         BYAFILE.BYA_AMT_1_1_2,
      *         BYAFILE.BYA_O2_1_1_2,
      *         BYAFILE.BYA_INT_1_1_2,
      *         BYAFILE.BYA_NO_1_1_3,
      *         BYAFILE.BYA_AMT_1_1_3,
      *         BYAFILE.BYA_O2_1_1_3,
      *         BYAFILE.BYA_INT_1_1_3,
      *         BYAFILE.BYA_NO_1_1_4,
      *         BYAFILE.BYA_AMT_1_1_4,
      *         BYAFILE.BYA_O2_1_1_4,
      *         BYAFILE.BYA_INT_1_1_4,
      *         BYAFILE.BYA_NO_1_1_5,
      *         BYAFILE.BYA_AMT_1_1_5,
      *         BYAFILE.BYA_O2_1_1_5,
      *         BYAFILE.BYA_INT_1_1_5,
      *         BYAFILE.BYA_NO_1_1_6,
      *         BYAFILE.BYA_AMT_1_1_6,
      *         BYAFILE.BYA_O2_1_1_6,
      *         BYAFILE.BYA_INT_1_1_6,
      *         BYAFILE.BYA_NO_1_1_7,
      *         BYAFILE.BYA_AMT_1_1_7,
      *         BYAFILE.BYA_O2_1_1_7,
      *         BYAFILE.BYA_INT_1_1_7,
      *         BYAFILE.BYA_NO_1_1_8,
      *         BYAFILE.BYA_AMT_1_1_8,
      *         BYAFILE.BYA_O2_1_1_8,
      *         BYAFILE.BYA_INT_1_1_8,
      *         BYAFILE.BYA_NO_1_1_9,
      *         BYAFILE.BYA_AMT_1_1_9,
      *         BYAFILE.BYA_O2_1_1_9,
      *         BYAFILE.BYA_INT_1_1_9,
      *         BYAFILE.BYA_NO_1_1_10,
      *         BYAFILE.BYA_AMT_1_1_10,
      *         BYAFILE.BYA_O2_1_1_10,
      *         BYAFILE.BYA_INT_1_1_10,
      *         BYAFILE.BYA_NO_1_1_11,
      *         BYAFILE.BYA_AMT_1_1_11,
      *         BYAFILE.BYA_O2_1_1_11,
      *         BYAFILE.BYA_INT_1_1_11,
      *         BYAFILE.BYA_NO_1_2_1,
      *         BYAFILE.BYA_AMT_1_2_1,
      *         BYAFILE.BYA_O2_1_2_1,
      *         BYAFILE.BYA_INT_1_2_1,
      *         BYAFILE.BYA_NO_1_2_2,
      *         BYAFILE.BYA_AMT_1_2_2,
      *         BYAFILE.BYA_O2_1_2_2,
      *         BYAFILE.BYA_INT_1_2_2,
      *         BYAFILE.BYA_NO_1_2_3,
      *         BYAFILE.BYA_AMT_1_2_3,
      *         BYAFILE.BYA_O2_1_2_3,
      *         BYAFILE.BYA_INT_1_2_3,
      *         BYAFILE.BYA_NO_1_2_4,
      *         BYAFILE.BYA_AMT_1_2_4,
      *         BYAFILE.BYA_O2_1_2_4,
      *         BYAFILE.BYA_INT_1_2_4,
      *         BYAFILE.BYA_NO_1_2_5,
      *         BYAFILE.BYA_AMT_1_2_5,
      *         BYAFILE.BYA_O2_1_2_5,
      *         BYAFILE.BYA_INT_1_2_5,
      *         BYAFILE.BYA_NO_1_2_6,
      *         BYAFILE.BYA_AMT_1_2_6,
      *         BYAFILE.BYA_O2_1_2_6,
      *         BYAFILE.BYA_INT_1_2_6,
      *         BYAFILE.BYA_NO_1_2_7,
      *         BYAFILE.BYA_AMT_1_2_7,
      *         BYAFILE.BYA_O2_1_2_7,
      *         BYAFILE.BYA_INT_1_2_7,
      *         BYAFILE.BYA_NO_1_2_8,
      *         BYAFILE.BYA_AMT_1_2_8,
      *         BYAFILE.BYA_O2_1_2_8,
      *         BYAFILE.BYA_INT_1_2_8,
      *         BYAFILE.BYA_NO_1_2_9,
      *         BYAFILE.BYA_AMT_1_2_9,
      *         BYAFILE.BYA_O2_1_2_9,
      *         BYAFILE.BYA_INT_1_2_9,
      *         BYAFILE.BYA_NO_1_2_10,
      *         BYAFILE.BYA_AMT_1_2_10,
      *         BYAFILE.BYA_O2_1_2_10,
      *         BYAFILE.BYA_INT_1_2_10,
      *         BYAFILE.BYA_NO_1_2_11,
      *         BYAFILE.BYA_AMT_1_2_11,
      *         BYAFILE.BYA_O2_1_2_11,
      *         BYAFILE.BYA_INT_1_2_11,
      *         BYAFILE.BYA_NO_2_1_1,
      *         BYAFILE.BYA_AMT_2_1_1,
      *         BYAFILE.BYA_O2_2_1_1,
      *         BYAFILE.BYA_INT_2_1_1,
      *         BYAFILE.BYA_NO_2_1_2,
      *         BYAFILE.BYA_AMT_2_1_2,
      *         BYAFILE.BYA_O2_2_1_2,
      *         BYAFILE.BYA_INT_2_1_2,
      *         BYAFILE.BYA_NO_2_1_3,
      *         BYAFILE.BYA_AMT_2_1_3,
      *         BYAFILE.BYA_O2_2_1_3,
      *         BYAFILE.BYA_INT_2_1_3,
      *         BYAFILE.BYA_NO_2_1_4,
      *         BYAFILE.BYA_AMT_2_1_4,
      *         BYAFILE.BYA_O2_2_1_4,
      *         BYAFILE.BYA_INT_2_1_4,
      *         BYAFILE.BYA_NO_2_1_5,
      *         BYAFILE.BYA_AMT_2_1_5,
      *         BYAFILE.BYA_O2_2_1_5,
      *         BYAFILE.BYA_INT_2_1_5,
      *         BYAFILE.BYA_NO_2_1_6,
      *         BYAFILE.BYA_AMT_2_1_6,
      *         BYAFILE.BYA_O2_2_1_6,
      *         BYAFILE.BYA_INT_2_1_6,
      *         BYAFILE.BYA_NO_2_1_7,
      *         BYAFILE.BYA_AMT_2_1_7,
      *         BYAFILE.BYA_O2_2_1_7,
      *         BYAFILE.BYA_INT_2_1_7,
      *         BYAFILE.BYA_NO_2_1_8,
      *         BYAFILE.BYA_AMT_2_1_8,
      *         BYAFILE.BYA_O2_2_1_8,
      *         BYAFILE.BYA_INT_2_1_8,
      *         BYAFILE.BYA_NO_2_1_9,
      *         BYAFILE.BYA_AMT_2_1_9,
      *         BYAFILE.BYA_O2_2_1_9,
      *         BYAFILE.BYA_INT_2_1_9,
      *         BYAFILE.BYA_NO_2_1_10,
      *         BYAFILE.BYA_AMT_2_1_10,
      *         BYAFILE.BYA_O2_2_1_10,
      *         BYAFILE.BYA_INT_2_1_10,
      *         BYAFILE.BYA_NO_2_1_11,
      *         BYAFILE.BYA_AMT_2_1_11,
      *         BYAFILE.BYA_O2_2_1_11,
      *         BYAFILE.BYA_INT_2_1_11,
      *         BYAFILE.BYA_NO_2_2_1,
      *         BYAFILE.BYA_AMT_2_2_1,
      *         BYAFILE.BYA_O2_2_2_1,
      *         BYAFILE.BYA_INT_2_2_1,
      *         BYAFILE.BYA_NO_2_2_2,
      *         BYAFILE.BYA_AMT_2_2_2,
      *         BYAFILE.BYA_O2_2_2_2,
      *         BYAFILE.BYA_INT_2_2_2,
      *         BYAFILE.BYA_NO_2_2_3,
      *         BYAFILE.BYA_AMT_2_2_3,
      *         BYAFILE.BYA_O2_2_2_3,
      *         BYAFILE.BYA_INT_2_2_3,
      *         BYAFILE.BYA_NO_2_2_4,
      *         BYAFILE.BYA_AMT_2_2_4,
      *         BYAFILE.BYA_O2_2_2_4,
      *         BYAFILE.BYA_INT_2_2_4,
      *         BYAFILE.BYA_NO_2_2_5,
      *         BYAFILE.BYA_AMT_2_2_5,
      *         BYAFILE.BYA_O2_2_2_5,
      *         BYAFILE.BYA_INT_2_2_5,
      *         BYAFILE.BYA_NO_2_2_6,
      *         BYAFILE.BYA_AMT_2_2_6,
      *         BYAFILE.BYA_O2_2_2_6,
      *         BYAFILE.BYA_INT_2_2_6,
      *         BYAFILE.BYA_NO_2_2_7,
      *         BYAFILE.BYA_AMT_2_2_7,
      *         BYAFILE.BYA_O2_2_2_7,
      *         BYAFILE.BYA_INT_2_2_7,
      *         BYAFILE.BYA_NO_2_2_8,
      *         BYAFILE.BYA_AMT_2_2_8,
      *         BYAFILE.BYA_O2_2_2_8,
      *         BYAFILE.BYA_INT_2_2_8,
      *         BYAFILE.BYA_NO_2_2_9,
      *         BYAFILE.BYA_AMT_2_2_9,
      *         BYAFILE.BYA_O2_2_2_9,
      *         BYAFILE.BYA_INT_2_2_9,
      *         BYAFILE.BYA_NO_2_2_10,
      *         BYAFILE.BYA_AMT_2_2_10,
      *         BYAFILE.BYA_O2_2_2_10,
      *         BYAFILE.BYA_INT_2_2_10,
      *         BYAFILE.BYA_NO_2_2_11,
      *         BYAFILE.BYA_AMT_2_2_11,
      *         BYAFILE.BYA_O2_2_2_11,
      *         BYAFILE.BYA_INT_2_2_11,
      *         BYAFILE.BYA_NO_3_1_1,
      *         BYAFILE.BYA_AMT_3_1_1,
      *         BYAFILE.BYA_O2_3_1_1,
      *         BYAFILE.BYA_INT_3_1_1,
      *         BYAFILE.BYA_NO_3_1_2,
      *         BYAFILE.BYA_AMT_3_1_2,
      *         BYAFILE.BYA_O2_3_1_2,
      *         BYAFILE.BYA_INT_3_1_2,
      *         BYAFILE.BYA_NO_3_1_3,
      *         BYAFILE.BYA_AMT_3_1_3,
      *         BYAFILE.BYA_O2_3_1_3,
      *         BYAFILE.BYA_INT_3_1_3,
      *         BYAFILE.BYA_NO_3_1_4,
      *         BYAFILE.BYA_AMT_3_1_4,
      *         BYAFILE.BYA_O2_3_1_4,
      *         BYAFILE.BYA_INT_3_1_4,
      *         BYAFILE.BYA_NO_3_1_5,
      *         BYAFILE.BYA_AMT_3_1_5,
      *         BYAFILE.BYA_O2_3_1_5,
      *         BYAFILE.BYA_INT_3_1_5,
      *         BYAFILE.BYA_NO_3_1_6,
      *         BYAFILE.BYA_AMT_3_1_6,
      *         BYAFILE.BYA_O2_3_1_6,
      *         BYAFILE.BYA_INT_3_1_6,
      *         BYAFILE.BYA_NO_3_1_7,
      *         BYAFILE.BYA_AMT_3_1_7,
      *         BYAFILE.BYA_O2_3_1_7,
      *         BYAFILE.BYA_INT_3_1_7,
      *         BYAFILE.BYA_NO_3_1_8,
      *         BYAFILE.BYA_AMT_3_1_8,
      *         BYAFILE.BYA_O2_3_1_8,
      *         BYAFILE.BYA_INT_3_1_8,
      *         BYAFILE.BYA_NO_3_1_9,
      *         BYAFILE.BYA_AMT_3_1_9,
      *         BYAFILE.BYA_O2_3_1_9,
      *         BYAFILE.BYA_INT_3_1_9,
      *         BYAFILE.BYA_NO_3_1_10,
      *         BYAFILE.BYA_AMT_3_1_10,
      *         BYAFILE.BYA_O2_3_1_10,
      *         BYAFILE.BYA_INT_3_1_10,
      *         BYAFILE.BYA_NO_3_1_11,
      *         BYAFILE.BYA_AMT_3_1_11,
      *         BYAFILE.BYA_O2_3_1_11,
      *         BYAFILE.BYA_INT_3_1_11,
      *         BYAFILE.BYA_NO_3_2_1,
      *         BYAFILE.BYA_AMT_3_2_1,
      *         BYAFILE.BYA_O2_3_2_1,
      *         BYAFILE.BYA_INT_3_2_1,
      *         BYAFILE.BYA_NO_3_2_2,
      *         BYAFILE.BYA_AMT_3_2_2,
      *         BYAFILE.BYA_O2_3_2_2,
      *         BYAFILE.BYA_INT_3_2_2,
      *         BYAFILE.BYA_NO_3_2_3,
      *         BYAFILE.BYA_AMT_3_2_3,
      *         BYAFILE.BYA_O2_3_2_3,
      *         BYAFILE.BYA_INT_3_2_3,
      *         BYAFILE.BYA_NO_3_2_4,
      *         BYAFILE.BYA_AMT_3_2_4,
      *         BYAFILE.BYA_O2_3_2_4,
      *         BYAFILE.BYA_INT_3_2_4,
      *         BYAFILE.BYA_NO_3_2_5,
      *         BYAFILE.BYA_AMT_3_2_5,
      *         BYAFILE.BYA_O2_3_2_5,
      *         BYAFILE.BYA_INT_3_2_5,
      *         BYAFILE.BYA_NO_3_2_6,
      *         BYAFILE.BYA_AMT_3_2_6,
      *         BYAFILE.BYA_O2_3_2_6,
      *         BYAFILE.BYA_INT_3_2_6,
      *         BYAFILE.BYA_NO_3_2_7,
      *         BYAFILE.BYA_AMT_3_2_7,
      *         BYAFILE.BYA_O2_3_2_7,
      *         BYAFILE.BYA_INT_3_2_7,
      *         BYAFILE.BYA_NO_3_2_8,
      *         BYAFILE.BYA_AMT_3_2_8,
      *         BYAFILE.BYA_O2_3_2_8,
      *         BYAFILE.BYA_INT_3_2_8,
      *         BYAFILE.BYA_NO_3_2_9,
      *         BYAFILE.BYA_AMT_3_2_9,
      *         BYAFILE.BYA_O2_3_2_9,
      *         BYAFILE.BYA_INT_3_2_9,
      *         BYAFILE.BYA_NO_3_2_10,
      *         BYAFILE.BYA_AMT_3_2_10,
      *         BYAFILE.BYA_O2_3_2_10,
      *         BYAFILE.BYA_INT_3_2_10,
      *         BYAFILE.BYA_NO_3_2_11,
      *         BYAFILE.BYA_AMT_3_2_11,
      *         BYAFILE.BYA_O2_3_2_11,
      *         BYAFILE.BYA_INT_3_2_11,
      *         BYAFILE.BYA_NO_REPO_1_1_1,
      *         BYAFILE.BYA_AMT_REPO_1_1_1,
      *         BYAFILE.BYA_O2_REPO_1_1_1,
      *         BYAFILE.BYA_INT_REPO_1_1_1,
      *         BYAFILE.BYA_NO_REPO_1_1_2,
      *         BYAFILE.BYA_AMT_REPO_1_1_2,
      *         BYAFILE.BYA_O2_REPO_1_1_2,
      *         BYAFILE.BYA_INT_REPO_1_1_2,
      *         BYAFILE.BYA_NO_REPO_1_1_3,
      *         BYAFILE.BYA_AMT_REPO_1_1_3,
      *         BYAFILE.BYA_O2_REPO_1_1_3,
      *         BYAFILE.BYA_INT_REPO_1_1_3,
      *         BYAFILE.BYA_NO_REPO_1_1_4,
      *         BYAFILE.BYA_AMT_REPO_1_1_4,
      *         BYAFILE.BYA_O2_REPO_1_1_4,
      *         BYAFILE.BYA_INT_REPO_1_1_4,
      *         BYAFILE.BYA_NO_REPO_1_1_5,
      *         BYAFILE.BYA_AMT_REPO_1_1_5,
      *         BYAFILE.BYA_O2_REPO_1_1_5,
      *         BYAFILE.BYA_INT_REPO_1_1_5,
      *         BYAFILE.BYA_NO_REPO_1_2_1,
      *         BYAFILE.BYA_AMT_REPO_1_2_1,
      *         BYAFILE.BYA_O2_REPO_1_2_1,
      *         BYAFILE.BYA_INT_REPO_1_2_1,
      *         BYAFILE.BYA_NO_REPO_1_2_2,
      *         BYAFILE.BYA_AMT_REPO_1_2_2,
      *         BYAFILE.BYA_O2_REPO_1_2_2,
      *         BYAFILE.BYA_INT_REPO_1_2_2,
      *         BYAFILE.BYA_NO_REPO_1_2_3,
      *         BYAFILE.BYA_AMT_REPO_1_2_3,
      *         BYAFILE.BYA_O2_REPO_1_2_3,
      *         BYAFILE.BYA_INT_REPO_1_2_3,
      *         BYAFILE.BYA_NO_REPO_1_2_4,
      *         BYAFILE.BYA_AMT_REPO_1_2_4,
      *         BYAFILE.BYA_O2_REPO_1_2_4,
      *         BYAFILE.BYA_INT_REPO_1_2_4,
      *         BYAFILE.BYA_NO_REPO_1_2_5,
      *         BYAFILE.BYA_AMT_REPO_1_2_5,
      *         BYAFILE.BYA_O2_REPO_1_2_5,
      *         BYAFILE.BYA_INT_REPO_1_2_5,
      *         BYAFILE.BYA_NO_REPO_2_1_1,
      *         BYAFILE.BYA_AMT_REPO_2_1_1,
      *         BYAFILE.BYA_O2_REPO_2_1_1,
      *         BYAFILE.BYA_INT_REPO_2_1_1,
      *         BYAFILE.BYA_NO_REPO_2_1_2,
      *         BYAFILE.BYA_AMT_REPO_2_1_2,
      *         BYAFILE.BYA_O2_REPO_2_1_2,
      *         BYAFILE.BYA_INT_REPO_2_1_2,
      *         BYAFILE.BYA_NO_REPO_2_1_3,
      *         BYAFILE.BYA_AMT_REPO_2_1_3,
      *         BYAFILE.BYA_O2_REPO_2_1_3,
      *         BYAFILE.BYA_INT_REPO_2_1_3,
      *         BYAFILE.BYA_NO_REPO_2_1_4,
      *         BYAFILE.BYA_AMT_REPO_2_1_4,
      *         BYAFILE.BYA_O2_REPO_2_1_4,
      *         BYAFILE.BYA_INT_REPO_2_1_4,
      *         BYAFILE.BYA_NO_REPO_2_1_5,
      *         BYAFILE.BYA_AMT_REPO_2_1_5,
      *         BYAFILE.BYA_O2_REPO_2_1_5,
      *         BYAFILE.BYA_INT_REPO_2_1_5,
      *         BYAFILE.BYA_NO_REPO_2_2_1,
      *         BYAFILE.BYA_AMT_REPO_2_2_1,
      *         BYAFILE.BYA_O2_REPO_2_2_1,
      *         BYAFILE.BYA_INT_REPO_2_2_1,
      *         BYAFILE.BYA_NO_REPO_2_2_2,
      *         BYAFILE.BYA_AMT_REPO_2_2_2,
      *         BYAFILE.BYA_O2_REPO_2_2_2,
      *         BYAFILE.BYA_INT_REPO_2_2_2,
      *         BYAFILE.BYA_NO_REPO_2_2_3,
      *         BYAFILE.BYA_AMT_REPO_2_2_3,
      *         BYAFILE.BYA_O2_REPO_2_2_3,
      *         BYAFILE.BYA_INT_REPO_2_2_3,
      *         BYAFILE.BYA_NO_REPO_2_2_4,
      *         BYAFILE.BYA_AMT_REPO_2_2_4,
      *         BYAFILE.BYA_O2_REPO_2_2_4,
      *         BYAFILE.BYA_INT_REPO_2_2_4,
      *         BYAFILE.BYA_NO_REPO_2_2_5,
      *         BYAFILE.BYA_AMT_REPO_2_2_5,
      *         BYAFILE.BYA_O2_REPO_2_2_5,
      *         BYAFILE.BYA_INT_REPO_2_2_5,
      *         BYAFILE.BYA_NO_REPO_3_1_1,
      *         BYAFILE.BYA_AMT_REPO_3_1_1,
      *         BYAFILE.BYA_O2_REPO_3_1_1,
      *         BYAFILE.BYA_INT_REPO_3_1_1,
      *         BYAFILE.BYA_NO_REPO_3_1_2,
      *         BYAFILE.BYA_AMT_REPO_3_1_2,
      *         BYAFILE.BYA_O2_REPO_3_1_2,
      *         BYAFILE.BYA_INT_REPO_3_1_2,
      *         BYAFILE.BYA_NO_REPO_3_1_3,
      *         BYAFILE.BYA_AMT_REPO_3_1_3,
      *         BYAFILE.BYA_O2_REPO_3_1_3,
      *         BYAFILE.BYA_INT_REPO_3_1_3,
      *         BYAFILE.BYA_NO_REPO_3_1_4,
      *         BYAFILE.BYA_AMT_REPO_3_1_4,
      *         BYAFILE.BYA_O2_REPO_3_1_4,
      *         BYAFILE.BYA_INT_REPO_3_1_4,
      *         BYAFILE.BYA_NO_REPO_3_1_5,
      *         BYAFILE.BYA_AMT_REPO_3_1_5,
      *         BYAFILE.BYA_O2_REPO_3_1_5,
      *         BYAFILE.BYA_INT_REPO_3_1_5,
      *         BYAFILE.BYA_NO_REPO_3_2_1,
      *         BYAFILE.BYA_AMT_REPO_3_2_1,
      *         BYAFILE.BYA_O2_REPO_3_2_1,
      *         BYAFILE.BYA_INT_REPO_3_2_1,
      *         BYAFILE.BYA_NO_REPO_3_2_2,
      *         BYAFILE.BYA_AMT_REPO_3_2_2,
      *         BYAFILE.BYA_O2_REPO_3_2_2,
      *         BYAFILE.BYA_INT_REPO_3_2_2,
      *         BYAFILE.BYA_NO_REPO_3_2_3,
      *         BYAFILE.BYA_AMT_REPO_3_2_3,
      *         BYAFILE.BYA_O2_REPO_3_2_3,
      *         BYAFILE.BYA_INT_REPO_3_2_3,
      *         BYAFILE.BYA_NO_REPO_3_2_4,
      *         BYAFILE.BYA_AMT_REPO_3_2_4,
      *         BYAFILE.BYA_O2_REPO_3_2_4,
      *         BYAFILE.BYA_INT_REPO_3_2_4,
      *         BYAFILE.BYA_NO_REPO_3_2_5,
      *         BYAFILE.BYA_AMT_REPO_3_2_5,
      *         BYAFILE.BYA_O2_REPO_3_2_5,
      *         BYAFILE.BYA_INT_REPO_3_2_5,
      *         BYAFILE.BYA_NO_PL_1_1_1,
      *         BYAFILE.BYA_AMT_PL_1_1_1,
      *         BYAFILE.BYA_O2_PL_1_1_1,
      *         BYAFILE.BYA_INT_PL_1_1_1,
      *         BYAFILE.BYA_NO_PL_1_1_2,
      *         BYAFILE.BYA_AMT_PL_1_1_2,
      *         BYAFILE.BYA_O2_PL_1_1_2,
      *         BYAFILE.BYA_INT_PL_1_1_2,
      *         BYAFILE.BYA_NO_PL_1_1_3,
      *         BYAFILE.BYA_AMT_PL_1_1_3,
      *         BYAFILE.BYA_O2_PL_1_1_3,
      *         BYAFILE.BYA_INT_PL_1_1_3,
      *         BYAFILE.BYA_NO_PL_1_1_4,
      *         BYAFILE.BYA_AMT_PL_1_1_4,
      *         BYAFILE.BYA_O2_PL_1_1_4,
      *         BYAFILE.BYA_INT_PL_1_1_4,
      *         BYAFILE.BYA_NO_PL_1_1_5,
      *         BYAFILE.BYA_AMT_PL_1_1_5,
      *         BYAFILE.BYA_O2_PL_1_1_5,
      *         BYAFILE.BYA_INT_PL_1_1_5,
      *         BYAFILE.BYA_NO_PL_1_1_6,
      *         BYAFILE.BYA_AMT_PL_1_1_6,
      *         BYAFILE.BYA_O2_PL_1_1_6,
      *         BYAFILE.BYA_INT_PL_1_1_6,
      *         BYAFILE.BYA_NO_PL_1_1_7,
      *         BYAFILE.BYA_AMT_PL_1_1_7,
      *         BYAFILE.BYA_O2_PL_1_1_7,
      *         BYAFILE.BYA_INT_PL_1_1_7,
      *         BYAFILE.BYA_NO_PL_1_1_8,
      *         BYAFILE.BYA_AMT_PL_1_1_8,
      *         BYAFILE.BYA_O2_PL_1_1_8,
      *         BYAFILE.BYA_INT_PL_1_1_8,
      *         BYAFILE.BYA_NO_PL_1_1_9,
      *         BYAFILE.BYA_AMT_PL_1_1_9,
      *         BYAFILE.BYA_O2_PL_1_1_9,
      *         BYAFILE.BYA_INT_PL_1_1_9,
      *         BYAFILE.BYA_NO_PL_1_1_10,
      *         BYAFILE.BYA_AMT_PL_1_1_10,
      *         BYAFILE.BYA_O2_PL_1_1_10,
      *         BYAFILE.BYA_INT_PL_1_1_10,
      *         BYAFILE.BYA_NO_PL_1_2_1,
      *         BYAFILE.BYA_AMT_PL_1_2_1,
      *         BYAFILE.BYA_O2_PL_1_2_1,
      *         BYAFILE.BYA_INT_PL_1_2_1,
      *         BYAFILE.BYA_NO_PL_1_2_2,
      *         BYAFILE.BYA_AMT_PL_1_2_2,
      *         BYAFILE.BYA_O2_PL_1_2_2,
      *         BYAFILE.BYA_INT_PL_1_2_2,
      *         BYAFILE.BYA_NO_PL_1_2_3,
      *         BYAFILE.BYA_AMT_PL_1_2_3,
      *         BYAFILE.BYA_O2_PL_1_2_3,
      *         BYAFILE.BYA_INT_PL_1_2_3,
      *         BYAFILE.BYA_NO_PL_1_2_4,
      *         BYAFILE.BYA_AMT_PL_1_2_4,
      *         BYAFILE.BYA_O2_PL_1_2_4,
      *         BYAFILE.BYA_INT_PL_1_2_4,
      *         BYAFILE.BYA_NO_PL_1_2_5,
      *         BYAFILE.BYA_AMT_PL_1_2_5,
      *         BYAFILE.BYA_O2_PL_1_2_5,
      *         BYAFILE.BYA_INT_PL_1_2_5,
      *         BYAFILE.BYA_NO_PL_1_2_6,
      *         BYAFILE.BYA_AMT_PL_1_2_6,
      *         BYAFILE.BYA_O2_PL_1_2_6,
      *         BYAFILE.BYA_INT_PL_1_2_6,
      *         BYAFILE.BYA_NO_PL_1_2_7,
      *         BYAFILE.BYA_AMT_PL_1_2_7,
      *         BYAFILE.BYA_O2_PL_1_2_7,
      *         BYAFILE.BYA_INT_PL_1_2_7,
      *         BYAFILE.BYA_NO_PL_1_2_8,
      *         BYAFILE.BYA_AMT_PL_1_2_8,
      *         BYAFILE.BYA_O2_PL_1_2_8,
      *         BYAFILE.BYA_INT_PL_1_2_8,
      *         BYAFILE.BYA_NO_PL_1_2_9,
      *         BYAFILE.BYA_AMT_PL_1_2_9,
      *         BYAFILE.BYA_O2_PL_1_2_9,
      *         BYAFILE.BYA_INT_PL_1_2_9,
      *         BYAFILE.BYA_NO_PL_1_2_10,
      *         BYAFILE.BYA_AMT_PL_1_2_10,
      *         BYAFILE.BYA_O2_PL_1_2_10,
      *         BYAFILE.BYA_INT_PL_1_2_10,
      *         BYAFILE.BYA_NO_PL_2_1_1,
      *         BYAFILE.BYA_AMT_PL_2_1_1,
      *         BYAFILE.BYA_O2_PL_2_1_1,
      *         BYAFILE.BYA_INT_PL_2_1_1,
      *         BYAFILE.BYA_NO_PL_2_1_2,
      *         BYAFILE.BYA_AMT_PL_2_1_2,
      *         BYAFILE.BYA_O2_PL_2_1_2,
      *         BYAFILE.BYA_INT_PL_2_1_2,
      *         BYAFILE.BYA_NO_PL_2_1_3,
      *         BYAFILE.BYA_AMT_PL_2_1_3,
      *         BYAFILE.BYA_O2_PL_2_1_3,
      *         BYAFILE.BYA_INT_PL_2_1_3,
      *         BYAFILE.BYA_NO_PL_2_1_4,
      *         BYAFILE.BYA_AMT_PL_2_1_4,
      *         BYAFILE.BYA_O2_PL_2_1_4,
      *         BYAFILE.BYA_INT_PL_2_1_4,
      *         BYAFILE.BYA_NO_PL_2_1_5,
      *         BYAFILE.BYA_AMT_PL_2_1_5,
      *         BYAFILE.BYA_O2_PL_2_1_5,
      *         BYAFILE.BYA_INT_PL_2_1_5,
      *         BYAFILE.BYA_NO_PL_2_1_6,
      *         BYAFILE.BYA_AMT_PL_2_1_6,
      *         BYAFILE.BYA_O2_PL_2_1_6,
      *         BYAFILE.BYA_INT_PL_2_1_6,
      *         BYAFILE.BYA_NO_PL_2_1_7,
      *         BYAFILE.BYA_AMT_PL_2_1_7,
      *         BYAFILE.BYA_O2_PL_2_1_7,
      *         BYAFILE.BYA_INT_PL_2_1_7,
      *         BYAFILE.BYA_NO_PL_2_1_8,
      *         BYAFILE.BYA_AMT_PL_2_1_8,
      *         BYAFILE.BYA_O2_PL_2_1_8,
      *         BYAFILE.BYA_INT_PL_2_1_8,
      *         BYAFILE.BYA_NO_PL_2_1_9,
      *         BYAFILE.BYA_AMT_PL_2_1_9,
      *         BYAFILE.BYA_O2_PL_2_1_9,
      *         BYAFILE.BYA_INT_PL_2_1_9,
      *         BYAFILE.BYA_NO_PL_2_1_10,
      *         BYAFILE.BYA_AMT_PL_2_1_10,
      *         BYAFILE.BYA_O2_PL_2_1_10,
      *         BYAFILE.BYA_INT_PL_2_1_10,
      *         BYAFILE.BYA_NO_PL_2_2_1,
      *         BYAFILE.BYA_AMT_PL_2_2_1,
      *         BYAFILE.BYA_O2_PL_2_2_1,
      *         BYAFILE.BYA_INT_PL_2_2_1,
      *         BYAFILE.BYA_NO_PL_2_2_2,
      *         BYAFILE.BYA_AMT_PL_2_2_2,
      *         BYAFILE.BYA_O2_PL_2_2_2,
      *         BYAFILE.BYA_INT_PL_2_2_2,
      *         BYAFILE.BYA_NO_PL_2_2_3,
      *         BYAFILE.BYA_AMT_PL_2_2_3,
      *         BYAFILE.BYA_O2_PL_2_2_3,
      *         BYAFILE.BYA_INT_PL_2_2_3,
      *         BYAFILE.BYA_NO_PL_2_2_4,
      *         BYAFILE.BYA_AMT_PL_2_2_4,
      *         BYAFILE.BYA_O2_PL_2_2_4,
      *         BYAFILE.BYA_INT_PL_2_2_4,
      *         BYAFILE.BYA_NO_PL_2_2_5,
      *         BYAFILE.BYA_AMT_PL_2_2_5,
      *         BYAFILE.BYA_O2_PL_2_2_5,
      *         BYAFILE.BYA_INT_PL_2_2_5,
      *         BYAFILE.BYA_NO_PL_2_2_6,
      *         BYAFILE.BYA_AMT_PL_2_2_6,
      *         BYAFILE.BYA_O2_PL_2_2_6,
      *         BYAFILE.BYA_INT_PL_2_2_6,
      *         BYAFILE.BYA_NO_PL_2_2_7,
      *         BYAFILE.BYA_AMT_PL_2_2_7,
      *         BYAFILE.BYA_O2_PL_2_2_7,
      *         BYAFILE.BYA_INT_PL_2_2_7,
      *         BYAFILE.BYA_NO_PL_2_2_8,
      *         BYAFILE.BYA_AMT_PL_2_2_8,
      *         BYAFILE.BYA_O2_PL_2_2_8,
      *         BYAFILE.BYA_INT_PL_2_2_8,
      *         BYAFILE.BYA_NO_PL_2_2_9,
      *         BYAFILE.BYA_AMT_PL_2_2_9,
      *         BYAFILE.BYA_O2_PL_2_2_9,
      *         BYAFILE.BYA_INT_PL_2_2_9,
      *         BYAFILE.BYA_NO_PL_2_2_10,
      *         BYAFILE.BYA_AMT_PL_2_2_10,
      *         BYAFILE.BYA_O2_PL_2_2_10,
      *         BYAFILE.BYA_INT_PL_2_2_10,
      *         BYAFILE.BYA_NO_PL_3_1_1,
      *         BYAFILE.BYA_AMT_PL_3_1_1,
      *         BYAFILE.BYA_O2_PL_3_1_1,
      *         BYAFILE.BYA_INT_PL_3_1_1,
      *         BYAFILE.BYA_NO_PL_3_1_2,
      *         BYAFILE.BYA_AMT_PL_3_1_2,
      *         BYAFILE.BYA_O2_PL_3_1_2,
      *         BYAFILE.BYA_INT_PL_3_1_2,
      *         BYAFILE.BYA_NO_PL_3_1_3,
      *         BYAFILE.BYA_AMT_PL_3_1_3,
      *         BYAFILE.BYA_O2_PL_3_1_3,
      *         BYAFILE.BYA_INT_PL_3_1_3,
      *         BYAFILE.BYA_NO_PL_3_1_4,
      *         BYAFILE.BYA_AMT_PL_3_1_4,
      *         BYAFILE.BYA_O2_PL_3_1_4,
      *         BYAFILE.BYA_INT_PL_3_1_4,
      *         BYAFILE.BYA_NO_PL_3_1_5,
      *         BYAFILE.BYA_AMT_PL_3_1_5,
      *         BYAFILE.BYA_O2_PL_3_1_5,
      *         BYAFILE.BYA_INT_PL_3_1_5,
      *         BYAFILE.BYA_NO_PL_3_1_6,
      *         BYAFILE.BYA_AMT_PL_3_1_6,
      *         BYAFILE.BYA_O2_PL_3_1_6,
      *         BYAFILE.BYA_INT_PL_3_1_6,
      *         BYAFILE.BYA_NO_PL_3_1_7,
      *         BYAFILE.BYA_AMT_PL_3_1_7,
      *         BYAFILE.BYA_O2_PL_3_1_7,
      *         BYAFILE.BYA_INT_PL_3_1_7,
      *         BYAFILE.BYA_NO_PL_3_1_8,
      *         BYAFILE.BYA_AMT_PL_3_1_8,
      *         BYAFILE.BYA_O2_PL_3_1_8,
      *         BYAFILE.BYA_INT_PL_3_1_8,
      *         BYAFILE.BYA_NO_PL_3_1_9,
      *         BYAFILE.BYA_AMT_PL_3_1_9,
      *         BYAFILE.BYA_O2_PL_3_1_9,
      *         BYAFILE.BYA_INT_PL_3_1_9,
      *         BYAFILE.BYA_NO_PL_3_1_10,
      *         BYAFILE.BYA_AMT_PL_3_1_10,
      *         BYAFILE.BYA_O2_PL_3_1_10,
      *         BYAFILE.BYA_INT_PL_3_1_10,
      *         BYAFILE.BYA_NO_PL_3_2_1,
      *         BYAFILE.BYA_AMT_PL_3_2_1,
      *         BYAFILE.BYA_O2_PL_3_2_1,
      *         BYAFILE.BYA_INT_PL_3_2_1,
      *         BYAFILE.BYA_NO_PL_3_2_2,
      *         BYAFILE.BYA_AMT_PL_3_2_2,
      *         BYAFILE.BYA_O2_PL_3_2_2,
      *         BYAFILE.BYA_INT_PL_3_2_2,
      *         BYAFILE.BYA_NO_PL_3_2_3,
      *         BYAFILE.BYA_AMT_PL_3_2_3,
      *         BYAFILE.BYA_O2_PL_3_2_3,
      *         BYAFILE.BYA_INT_PL_3_2_3,
      *         BYAFILE.BYA_NO_PL_3_2_4,
      *         BYAFILE.BYA_AMT_PL_3_2_4,
      *         BYAFILE.BYA_O2_PL_3_2_4,
      *         BYAFILE.BYA_INT_PL_3_2_4,
      *         BYAFILE.BYA_NO_PL_3_2_5,
      *         BYAFILE.BYA_AMT_PL_3_2_5,
      *         BYAFILE.BYA_O2_PL_3_2_5,
      *         BYAFILE.BYA_INT_PL_3_2_5,
      *         BYAFILE.BYA_NO_PL_3_2_6,
      *         BYAFILE.BYA_AMT_PL_3_2_6,
      *         BYAFILE.BYA_O2_PL_3_2_6,
      *         BYAFILE.BYA_INT_PL_3_2_6,
      *         BYAFILE.BYA_NO_PL_3_2_7,
      *         BYAFILE.BYA_AMT_PL_3_2_7,
      *         BYAFILE.BYA_O2_PL_3_2_7,
      *         BYAFILE.BYA_INT_PL_3_2_7,
      *         BYAFILE.BYA_NO_PL_3_2_8,
      *         BYAFILE.BYA_AMT_PL_3_2_8,
      *         BYAFILE.BYA_O2_PL_3_2_8,
      *         BYAFILE.BYA_INT_PL_3_2_8,
      *         BYAFILE.BYA_NO_PL_3_2_9,
      *         BYAFILE.BYA_AMT_PL_3_2_9,
      *         BYAFILE.BYA_O2_PL_3_2_9,
      *         BYAFILE.BYA_INT_PL_3_2_9,
      *         BYAFILE.BYA_NO_PL_3_2_10,
      *         BYAFILE.BYA_AMT_PL_3_2_10,
      *         BYAFILE.BYA_O2_PL_3_2_10,
      *         BYAFILE.BYA_INT_PL_3_2_10,
      *         BYAFILE.BYA_DLRVPRIN_1_1_1,
      *         BYAFILE.BYA_DLRVPRIN_1_1_2,
      *         BYAFILE.BYA_DLRVPRIN_1_1_3,
      *         BYAFILE.BYA_DLRVCHGS_1_1_1,
      *         BYAFILE.BYA_DLRVCHGS_1_1_2,
      *         BYAFILE.BYA_DLRVCHGS_1_1_3,
      *         BYAFILE.BYA_DLPARVHELD_1_1,
      *         BYAFILE.BYA_DLRVPRIN_1_2_1,
      *         BYAFILE.BYA_DLRVPRIN_1_2_2,
      *         BYAFILE.BYA_DLRVPRIN_1_2_3,
      *         BYAFILE.BYA_DLRVCHGS_1_2_1,
      *         BYAFILE.BYA_DLRVCHGS_1_2_2,
      *         BYAFILE.BYA_DLRVCHGS_1_2_3,
      *         BYAFILE.BYA_DLPARVHELD_1_2,
      *         BYAFILE.BYA_DLRVPRIN_2_1_1,
      *         BYAFILE.BYA_DLRVPRIN_2_1_2,
      *         BYAFILE.BYA_DLRVPRIN_2_1_3,
      *         BYAFILE.BYA_DLRVCHGS_2_1_1,
      *         BYAFILE.BYA_DLRVCHGS_2_1_2,
      *         BYAFILE.BYA_DLRVCHGS_2_1_3,
      *         BYAFILE.BYA_DLPARVHELD_2_1,
      *         BYAFILE.BYA_DLRVPRIN_2_2_1,
      *         BYAFILE.BYA_DLRVPRIN_2_2_2,
      *         BYAFILE.BYA_DLRVPRIN_2_2_3,
      *         BYAFILE.BYA_DLRVCHGS_2_2_1,
      *         BYAFILE.BYA_DLRVCHGS_2_2_2,
      *         BYAFILE.BYA_DLRVCHGS_2_2_3,
      *         BYAFILE.BYA_DLPARVHELD_2_2,
      *         BYAFILE.BYA_DLRVPRIN_3_1_1,
      *         BYAFILE.BYA_DLRVPRIN_3_1_2,
      *         BYAFILE.BYA_DLRVPRIN_3_1_3,
      *         BYAFILE.BYA_DLRVCHGS_3_1_1,
      *         BYAFILE.BYA_DLRVCHGS_3_1_2,
      *         BYAFILE.BYA_DLRVCHGS_3_1_3,
      *         BYAFILE.BYA_DLPARVHELD_3_1,
      *         BYAFILE.BYA_DLRVPRIN_3_2_1,
      *         BYAFILE.BYA_DLRVPRIN_3_2_2,
      *         BYAFILE.BYA_DLRVPRIN_3_2_3,
      *         BYAFILE.BYA_DLRVCHGS_3_2_1,
      *         BYAFILE.BYA_DLRVCHGS_3_2_2,
      *         BYAFILE.BYA_DLRVCHGS_3_2_3,
      *         BYAFILE.BYA_DLPARVHELD_3_2
      *  
      *    INTO #TEMP_LPEOD5_BYAFILE
      *
      *    FROM DBO.BYAFILE 
      *   
      *   WHERE BYAFILE.BYA_DATE_CCYYMM BETWEEN @QBY1_WBEG_DATE  
      *                                     AND @QBY1_WEND_DATE
      *     AND BYAFILE.BYA_BRNO        BETWEEN @QBY1_WBEG_BRNO  
      *                                     AND @QBY1_WEND_BRNO
      *     AND BYAFILE.BYA_CLASS       BETWEEN @QBY1_WBEG_CLASS 
      *                                     AND @QBY1_WEND_CLASS;

           EXEC SQL SET CONNECTION BY1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE BYA1_CURSOR CURSOR FOR
                :BY1-RETURN-CODE = EXEC SSP_LPEOD5_BYAFILE_SELECT 
              ( :QBY1-WBEG-DATE-CCYYMM,
                :QBY1-WEND-DATE-CCYYMM,
                :QBY1-WBEG-BRNO,
                :QBY1-WEND-BRNO, 
                :QBY1-WBEG-CLASS,      
                :QBY1-WEND-CLASS )     
           END-EXEC.

           EXEC SQL
               OPEN BYA1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QBY1-WBEG-DATE-CCYYMM
                          QBY1-WEND-DATE-CCYYMM
                          QBY1-WBEG-BRNO
                          QBY1-WEND-BRNO
                          QBY1-WBEG-CLASS
                          QBY1-WEND-CLASS.

           MOVE STAT---GOOD TO BYA1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-BYA1-FILE.
           PERFORM READ-IT.
           MOVE BYA-PATH-SQL TO E-FILE.
           MOVE BYA1-KEY TO E-KEYX.
           INITIALIZE QBYA-REC.

           MOVE BYA-DATE-CCYYMM      TO QBYA-DATE-CCYYMM.
           MOVE BYA-BRNO             TO QBYA-BRNO.
           MOVE BYA-CLASS            TO QBYA-CLASS.

           EXEC SQL
            SELECT BYAFILE.BYA_DATE_CCYYMM,
                   BYAFILE.BYA_BRNO,
                   BYAFILE.BYA_CLASS,
                   CAST(BYAFILE.BYA_LTOUCH_DATE AS VARCHAR(10)),
                   BYAFILE.BYA_NO_1_1_1,
                   BYAFILE.BYA_AMT_1_1_1,
                   BYAFILE.BYA_O2_1_1_1,
                   BYAFILE.BYA_INT_1_1_1,
                   BYAFILE.BYA_NO_1_1_2,
                   BYAFILE.BYA_AMT_1_1_2,
                   BYAFILE.BYA_O2_1_1_2,
                   BYAFILE.BYA_INT_1_1_2,
                   BYAFILE.BYA_NO_1_1_3,
                   BYAFILE.BYA_AMT_1_1_3,
                   BYAFILE.BYA_O2_1_1_3,
                   BYAFILE.BYA_INT_1_1_3,
                   BYAFILE.BYA_NO_1_1_4,
                   BYAFILE.BYA_AMT_1_1_4,
                   BYAFILE.BYA_O2_1_1_4,
                   BYAFILE.BYA_INT_1_1_4,
                   BYAFILE.BYA_NO_1_1_5,
                   BYAFILE.BYA_AMT_1_1_5,
                   BYAFILE.BYA_O2_1_1_5,
                   BYAFILE.BYA_INT_1_1_5,
                   BYAFILE.BYA_NO_1_1_6,
                   BYAFILE.BYA_AMT_1_1_6,
                   BYAFILE.BYA_O2_1_1_6,
                   BYAFILE.BYA_INT_1_1_6,
                   BYAFILE.BYA_NO_1_1_7,
                   BYAFILE.BYA_AMT_1_1_7,
                   BYAFILE.BYA_O2_1_1_7,
                   BYAFILE.BYA_INT_1_1_7,
                   BYAFILE.BYA_NO_1_1_8,
                   BYAFILE.BYA_AMT_1_1_8,
                   BYAFILE.BYA_O2_1_1_8,
                   BYAFILE.BYA_INT_1_1_8,
                   BYAFILE.BYA_NO_1_1_9,
                   BYAFILE.BYA_AMT_1_1_9,
                   BYAFILE.BYA_O2_1_1_9,
                   BYAFILE.BYA_INT_1_1_9,
                   BYAFILE.BYA_NO_1_1_10,
                   BYAFILE.BYA_AMT_1_1_10,
                   BYAFILE.BYA_O2_1_1_10,
                   BYAFILE.BYA_INT_1_1_10,
                   BYAFILE.BYA_NO_1_1_11,
                   BYAFILE.BYA_AMT_1_1_11,
                   BYAFILE.BYA_O2_1_1_11,
                   BYAFILE.BYA_INT_1_1_11,
                   BYAFILE.BYA_NO_1_2_1,
                   BYAFILE.BYA_AMT_1_2_1,
                   BYAFILE.BYA_O2_1_2_1,
                   BYAFILE.BYA_INT_1_2_1,
                   BYAFILE.BYA_NO_1_2_2,
                   BYAFILE.BYA_AMT_1_2_2,
                   BYAFILE.BYA_O2_1_2_2,
                   BYAFILE.BYA_INT_1_2_2,
                   BYAFILE.BYA_NO_1_2_3,
                   BYAFILE.BYA_AMT_1_2_3,
                   BYAFILE.BYA_O2_1_2_3,
                   BYAFILE.BYA_INT_1_2_3,
                   BYAFILE.BYA_NO_1_2_4,
                   BYAFILE.BYA_AMT_1_2_4,
                   BYAFILE.BYA_O2_1_2_4,
                   BYAFILE.BYA_INT_1_2_4,
                   BYAFILE.BYA_NO_1_2_5,
                   BYAFILE.BYA_AMT_1_2_5,
                   BYAFILE.BYA_O2_1_2_5,
                   BYAFILE.BYA_INT_1_2_5,
                   BYAFILE.BYA_NO_1_2_6,
                   BYAFILE.BYA_AMT_1_2_6,
                   BYAFILE.BYA_O2_1_2_6,
                   BYAFILE.BYA_INT_1_2_6,
                   BYAFILE.BYA_NO_1_2_7,
                   BYAFILE.BYA_AMT_1_2_7,
                   BYAFILE.BYA_O2_1_2_7,
                   BYAFILE.BYA_INT_1_2_7,
                   BYAFILE.BYA_NO_1_2_8,
                   BYAFILE.BYA_AMT_1_2_8,
                   BYAFILE.BYA_O2_1_2_8,
                   BYAFILE.BYA_INT_1_2_8,
                   BYAFILE.BYA_NO_1_2_9,
                   BYAFILE.BYA_AMT_1_2_9,
                   BYAFILE.BYA_O2_1_2_9,
                   BYAFILE.BYA_INT_1_2_9,
                   BYAFILE.BYA_NO_1_2_10,
                   BYAFILE.BYA_AMT_1_2_10,
                   BYAFILE.BYA_O2_1_2_10,
                   BYAFILE.BYA_INT_1_2_10,
                   BYAFILE.BYA_NO_1_2_11,
                   BYAFILE.BYA_AMT_1_2_11,
                   BYAFILE.BYA_O2_1_2_11,
                   BYAFILE.BYA_INT_1_2_11,
                   BYAFILE.BYA_NO_2_1_1,
                   BYAFILE.BYA_AMT_2_1_1,
                   BYAFILE.BYA_O2_2_1_1,
                   BYAFILE.BYA_INT_2_1_1,
                   BYAFILE.BYA_NO_2_1_2,
                   BYAFILE.BYA_AMT_2_1_2,
                   BYAFILE.BYA_O2_2_1_2,
                   BYAFILE.BYA_INT_2_1_2,
                   BYAFILE.BYA_NO_2_1_3,
                   BYAFILE.BYA_AMT_2_1_3,
                   BYAFILE.BYA_O2_2_1_3,
                   BYAFILE.BYA_INT_2_1_3,
                   BYAFILE.BYA_NO_2_1_4,
                   BYAFILE.BYA_AMT_2_1_4,
                   BYAFILE.BYA_O2_2_1_4,
                   BYAFILE.BYA_INT_2_1_4,
                   BYAFILE.BYA_NO_2_1_5,
                   BYAFILE.BYA_AMT_2_1_5,
                   BYAFILE.BYA_O2_2_1_5,
                   BYAFILE.BYA_INT_2_1_5,
                   BYAFILE.BYA_NO_2_1_6,
                   BYAFILE.BYA_AMT_2_1_6,
                   BYAFILE.BYA_O2_2_1_6,
                   BYAFILE.BYA_INT_2_1_6,
                   BYAFILE.BYA_NO_2_1_7,
                   BYAFILE.BYA_AMT_2_1_7,
                   BYAFILE.BYA_O2_2_1_7,
                   BYAFILE.BYA_INT_2_1_7,
                   BYAFILE.BYA_NO_2_1_8,
                   BYAFILE.BYA_AMT_2_1_8,
                   BYAFILE.BYA_O2_2_1_8,
                   BYAFILE.BYA_INT_2_1_8,
                   BYAFILE.BYA_NO_2_1_9,
                   BYAFILE.BYA_AMT_2_1_9,
                   BYAFILE.BYA_O2_2_1_9,
                   BYAFILE.BYA_INT_2_1_9,
                   BYAFILE.BYA_NO_2_1_10,
                   BYAFILE.BYA_AMT_2_1_10,
                   BYAFILE.BYA_O2_2_1_10,
                   BYAFILE.BYA_INT_2_1_10,
                   BYAFILE.BYA_NO_2_1_11,
                   BYAFILE.BYA_AMT_2_1_11,
                   BYAFILE.BYA_O2_2_1_11,
                   BYAFILE.BYA_INT_2_1_11,
                   BYAFILE.BYA_NO_2_2_1,
                   BYAFILE.BYA_AMT_2_2_1,
                   BYAFILE.BYA_O2_2_2_1,
                   BYAFILE.BYA_INT_2_2_1,
                   BYAFILE.BYA_NO_2_2_2,
                   BYAFILE.BYA_AMT_2_2_2,
                   BYAFILE.BYA_O2_2_2_2,
                   BYAFILE.BYA_INT_2_2_2,
                   BYAFILE.BYA_NO_2_2_3,
                   BYAFILE.BYA_AMT_2_2_3,
                   BYAFILE.BYA_O2_2_2_3,
                   BYAFILE.BYA_INT_2_2_3,
                   BYAFILE.BYA_NO_2_2_4,
                   BYAFILE.BYA_AMT_2_2_4,
                   BYAFILE.BYA_O2_2_2_4,
                   BYAFILE.BYA_INT_2_2_4,
                   BYAFILE.BYA_NO_2_2_5,
                   BYAFILE.BYA_AMT_2_2_5,
                   BYAFILE.BYA_O2_2_2_5,
                   BYAFILE.BYA_INT_2_2_5,
                   BYAFILE.BYA_NO_2_2_6,
                   BYAFILE.BYA_AMT_2_2_6,
                   BYAFILE.BYA_O2_2_2_6,
                   BYAFILE.BYA_INT_2_2_6,
                   BYAFILE.BYA_NO_2_2_7,
                   BYAFILE.BYA_AMT_2_2_7,
                   BYAFILE.BYA_O2_2_2_7,
                   BYAFILE.BYA_INT_2_2_7,
                   BYAFILE.BYA_NO_2_2_8,
                   BYAFILE.BYA_AMT_2_2_8,
                   BYAFILE.BYA_O2_2_2_8,
                   BYAFILE.BYA_INT_2_2_8,
                   BYAFILE.BYA_NO_2_2_9,
                   BYAFILE.BYA_AMT_2_2_9,
                   BYAFILE.BYA_O2_2_2_9,
                   BYAFILE.BYA_INT_2_2_9,
                   BYAFILE.BYA_NO_2_2_10,
                   BYAFILE.BYA_AMT_2_2_10,
                   BYAFILE.BYA_O2_2_2_10,
                   BYAFILE.BYA_INT_2_2_10,
                   BYAFILE.BYA_NO_2_2_11,
                   BYAFILE.BYA_AMT_2_2_11,
                   BYAFILE.BYA_O2_2_2_11,
                   BYAFILE.BYA_INT_2_2_11,
                   BYAFILE.BYA_NO_3_1_1,
                   BYAFILE.BYA_AMT_3_1_1,
                   BYAFILE.BYA_O2_3_1_1,
                   BYAFILE.BYA_INT_3_1_1,
                   BYAFILE.BYA_NO_3_1_2,
                   BYAFILE.BYA_AMT_3_1_2,
                   BYAFILE.BYA_O2_3_1_2,
                   BYAFILE.BYA_INT_3_1_2,
                   BYAFILE.BYA_NO_3_1_3,
                   BYAFILE.BYA_AMT_3_1_3,
                   BYAFILE.BYA_O2_3_1_3,
                   BYAFILE.BYA_INT_3_1_3,
                   BYAFILE.BYA_NO_3_1_4,
                   BYAFILE.BYA_AMT_3_1_4,
                   BYAFILE.BYA_O2_3_1_4,
                   BYAFILE.BYA_INT_3_1_4,
                   BYAFILE.BYA_NO_3_1_5,
                   BYAFILE.BYA_AMT_3_1_5,
                   BYAFILE.BYA_O2_3_1_5,
                   BYAFILE.BYA_INT_3_1_5,
                   BYAFILE.BYA_NO_3_1_6,
                   BYAFILE.BYA_AMT_3_1_6,
                   BYAFILE.BYA_O2_3_1_6,
                   BYAFILE.BYA_INT_3_1_6,
                   BYAFILE.BYA_NO_3_1_7,
                   BYAFILE.BYA_AMT_3_1_7,
                   BYAFILE.BYA_O2_3_1_7,
                   BYAFILE.BYA_INT_3_1_7,
                   BYAFILE.BYA_NO_3_1_8,
                   BYAFILE.BYA_AMT_3_1_8,
                   BYAFILE.BYA_O2_3_1_8,
                   BYAFILE.BYA_INT_3_1_8,
                   BYAFILE.BYA_NO_3_1_9,
                   BYAFILE.BYA_AMT_3_1_9,
                   BYAFILE.BYA_O2_3_1_9,
                   BYAFILE.BYA_INT_3_1_9,
                   BYAFILE.BYA_NO_3_1_10,
                   BYAFILE.BYA_AMT_3_1_10,
                   BYAFILE.BYA_O2_3_1_10,
                   BYAFILE.BYA_INT_3_1_10,
                   BYAFILE.BYA_NO_3_1_11,
                   BYAFILE.BYA_AMT_3_1_11,
                   BYAFILE.BYA_O2_3_1_11,
                   BYAFILE.BYA_INT_3_1_11,
                   BYAFILE.BYA_NO_3_2_1,
                   BYAFILE.BYA_AMT_3_2_1,
                   BYAFILE.BYA_O2_3_2_1,
                   BYAFILE.BYA_INT_3_2_1,
                   BYAFILE.BYA_NO_3_2_2,
                   BYAFILE.BYA_AMT_3_2_2,
                   BYAFILE.BYA_O2_3_2_2,
                   BYAFILE.BYA_INT_3_2_2,
                   BYAFILE.BYA_NO_3_2_3,
                   BYAFILE.BYA_AMT_3_2_3,
                   BYAFILE.BYA_O2_3_2_3,
                   BYAFILE.BYA_INT_3_2_3,
                   BYAFILE.BYA_NO_3_2_4,
                   BYAFILE.BYA_AMT_3_2_4,
                   BYAFILE.BYA_O2_3_2_4,
                   BYAFILE.BYA_INT_3_2_4,
                   BYAFILE.BYA_NO_3_2_5,
                   BYAFILE.BYA_AMT_3_2_5,
                   BYAFILE.BYA_O2_3_2_5,
                   BYAFILE.BYA_INT_3_2_5,
                   BYAFILE.BYA_NO_3_2_6,
                   BYAFILE.BYA_AMT_3_2_6,
                   BYAFILE.BYA_O2_3_2_6,
                   BYAFILE.BYA_INT_3_2_6,
                   BYAFILE.BYA_NO_3_2_7,
                   BYAFILE.BYA_AMT_3_2_7,
                   BYAFILE.BYA_O2_3_2_7,
                   BYAFILE.BYA_INT_3_2_7,
                   BYAFILE.BYA_NO_3_2_8,
                   BYAFILE.BYA_AMT_3_2_8,
                   BYAFILE.BYA_O2_3_2_8,
                   BYAFILE.BYA_INT_3_2_8,
                   BYAFILE.BYA_NO_3_2_9,
                   BYAFILE.BYA_AMT_3_2_9,
                   BYAFILE.BYA_O2_3_2_9,
                   BYAFILE.BYA_INT_3_2_9,
                   BYAFILE.BYA_NO_3_2_10,
                   BYAFILE.BYA_AMT_3_2_10,
                   BYAFILE.BYA_O2_3_2_10,
                   BYAFILE.BYA_INT_3_2_10,
                   BYAFILE.BYA_NO_3_2_11,
                   BYAFILE.BYA_AMT_3_2_11,
                   BYAFILE.BYA_O2_3_2_11,
                   BYAFILE.BYA_INT_3_2_11,
                   BYAFILE.BYA_NO_REPO_1_1_1,
                   BYAFILE.BYA_AMT_REPO_1_1_1,
                   BYAFILE.BYA_O2_REPO_1_1_1,
                   BYAFILE.BYA_INT_REPO_1_1_1,
                   BYAFILE.BYA_NO_REPO_1_1_2,
                   BYAFILE.BYA_AMT_REPO_1_1_2,
                   BYAFILE.BYA_O2_REPO_1_1_2,
                   BYAFILE.BYA_INT_REPO_1_1_2,
                   BYAFILE.BYA_NO_REPO_1_1_3,
                   BYAFILE.BYA_AMT_REPO_1_1_3,
                   BYAFILE.BYA_O2_REPO_1_1_3,
                   BYAFILE.BYA_INT_REPO_1_1_3,
                   BYAFILE.BYA_NO_REPO_1_1_4,
                   BYAFILE.BYA_AMT_REPO_1_1_4,
                   BYAFILE.BYA_O2_REPO_1_1_4,
                   BYAFILE.BYA_INT_REPO_1_1_4,
                   BYAFILE.BYA_NO_REPO_1_1_5,
                   BYAFILE.BYA_AMT_REPO_1_1_5,
                   BYAFILE.BYA_O2_REPO_1_1_5,
                   BYAFILE.BYA_INT_REPO_1_1_5,
                   BYAFILE.BYA_NO_REPO_1_2_1,
                   BYAFILE.BYA_AMT_REPO_1_2_1,
                   BYAFILE.BYA_O2_REPO_1_2_1,
                   BYAFILE.BYA_INT_REPO_1_2_1,
                   BYAFILE.BYA_NO_REPO_1_2_2,
                   BYAFILE.BYA_AMT_REPO_1_2_2,
                   BYAFILE.BYA_O2_REPO_1_2_2,
                   BYAFILE.BYA_INT_REPO_1_2_2,
                   BYAFILE.BYA_NO_REPO_1_2_3,
                   BYAFILE.BYA_AMT_REPO_1_2_3,
                   BYAFILE.BYA_O2_REPO_1_2_3,
                   BYAFILE.BYA_INT_REPO_1_2_3,
                   BYAFILE.BYA_NO_REPO_1_2_4,
                   BYAFILE.BYA_AMT_REPO_1_2_4,
                   BYAFILE.BYA_O2_REPO_1_2_4,
                   BYAFILE.BYA_INT_REPO_1_2_4,
                   BYAFILE.BYA_NO_REPO_1_2_5,
                   BYAFILE.BYA_AMT_REPO_1_2_5,
                   BYAFILE.BYA_O2_REPO_1_2_5,
                   BYAFILE.BYA_INT_REPO_1_2_5,
                   BYAFILE.BYA_NO_REPO_2_1_1,
                   BYAFILE.BYA_AMT_REPO_2_1_1,
                   BYAFILE.BYA_O2_REPO_2_1_1,
                   BYAFILE.BYA_INT_REPO_2_1_1,
                   BYAFILE.BYA_NO_REPO_2_1_2,
                   BYAFILE.BYA_AMT_REPO_2_1_2,
                   BYAFILE.BYA_O2_REPO_2_1_2,
                   BYAFILE.BYA_INT_REPO_2_1_2,
                   BYAFILE.BYA_NO_REPO_2_1_3,
                   BYAFILE.BYA_AMT_REPO_2_1_3,
                   BYAFILE.BYA_O2_REPO_2_1_3,
                   BYAFILE.BYA_INT_REPO_2_1_3,
                   BYAFILE.BYA_NO_REPO_2_1_4,
                   BYAFILE.BYA_AMT_REPO_2_1_4,
                   BYAFILE.BYA_O2_REPO_2_1_4,
                   BYAFILE.BYA_INT_REPO_2_1_4,
                   BYAFILE.BYA_NO_REPO_2_1_5,
                   BYAFILE.BYA_AMT_REPO_2_1_5,
                   BYAFILE.BYA_O2_REPO_2_1_5,
                   BYAFILE.BYA_INT_REPO_2_1_5,
                   BYAFILE.BYA_NO_REPO_2_2_1,
                   BYAFILE.BYA_AMT_REPO_2_2_1,
                   BYAFILE.BYA_O2_REPO_2_2_1,
                   BYAFILE.BYA_INT_REPO_2_2_1,
                   BYAFILE.BYA_NO_REPO_2_2_2,
                   BYAFILE.BYA_AMT_REPO_2_2_2,
                   BYAFILE.BYA_O2_REPO_2_2_2,
                   BYAFILE.BYA_INT_REPO_2_2_2,
                   BYAFILE.BYA_NO_REPO_2_2_3,
                   BYAFILE.BYA_AMT_REPO_2_2_3,
                   BYAFILE.BYA_O2_REPO_2_2_3,
                   BYAFILE.BYA_INT_REPO_2_2_3,
                   BYAFILE.BYA_NO_REPO_2_2_4,
                   BYAFILE.BYA_AMT_REPO_2_2_4,
                   BYAFILE.BYA_O2_REPO_2_2_4,
                   BYAFILE.BYA_INT_REPO_2_2_4,
                   BYAFILE.BYA_NO_REPO_2_2_5,
                   BYAFILE.BYA_AMT_REPO_2_2_5,
                   BYAFILE.BYA_O2_REPO_2_2_5,
                   BYAFILE.BYA_INT_REPO_2_2_5,
                   BYAFILE.BYA_NO_REPO_3_1_1,
                   BYAFILE.BYA_AMT_REPO_3_1_1,
                   BYAFILE.BYA_O2_REPO_3_1_1,
                   BYAFILE.BYA_INT_REPO_3_1_1,
                   BYAFILE.BYA_NO_REPO_3_1_2,
                   BYAFILE.BYA_AMT_REPO_3_1_2,
                   BYAFILE.BYA_O2_REPO_3_1_2,
                   BYAFILE.BYA_INT_REPO_3_1_2,
                   BYAFILE.BYA_NO_REPO_3_1_3,
                   BYAFILE.BYA_AMT_REPO_3_1_3,
                   BYAFILE.BYA_O2_REPO_3_1_3,
                   BYAFILE.BYA_INT_REPO_3_1_3,
                   BYAFILE.BYA_NO_REPO_3_1_4,
                   BYAFILE.BYA_AMT_REPO_3_1_4,
                   BYAFILE.BYA_O2_REPO_3_1_4,
                   BYAFILE.BYA_INT_REPO_3_1_4,
                   BYAFILE.BYA_NO_REPO_3_1_5,
                   BYAFILE.BYA_AMT_REPO_3_1_5,
                   BYAFILE.BYA_O2_REPO_3_1_5,
                   BYAFILE.BYA_INT_REPO_3_1_5,
                   BYAFILE.BYA_NO_REPO_3_2_1,
                   BYAFILE.BYA_AMT_REPO_3_2_1,
                   BYAFILE.BYA_O2_REPO_3_2_1,
                   BYAFILE.BYA_INT_REPO_3_2_1,
                   BYAFILE.BYA_NO_REPO_3_2_2,
                   BYAFILE.BYA_AMT_REPO_3_2_2,
                   BYAFILE.BYA_O2_REPO_3_2_2,
                   BYAFILE.BYA_INT_REPO_3_2_2,
                   BYAFILE.BYA_NO_REPO_3_2_3,
                   BYAFILE.BYA_AMT_REPO_3_2_3,
                   BYAFILE.BYA_O2_REPO_3_2_3,
                   BYAFILE.BYA_INT_REPO_3_2_3,
                   BYAFILE.BYA_NO_REPO_3_2_4,
                   BYAFILE.BYA_AMT_REPO_3_2_4,
                   BYAFILE.BYA_O2_REPO_3_2_4,
                   BYAFILE.BYA_INT_REPO_3_2_4,
                   BYAFILE.BYA_NO_REPO_3_2_5,
                   BYAFILE.BYA_AMT_REPO_3_2_5,
                   BYAFILE.BYA_O2_REPO_3_2_5,
                   BYAFILE.BYA_INT_REPO_3_2_5,
                   BYAFILE.BYA_NO_PL_1_1_1,
                   BYAFILE.BYA_AMT_PL_1_1_1,
                   BYAFILE.BYA_O2_PL_1_1_1,
                   BYAFILE.BYA_INT_PL_1_1_1,
                   BYAFILE.BYA_NO_PL_1_1_2,
                   BYAFILE.BYA_AMT_PL_1_1_2,
                   BYAFILE.BYA_O2_PL_1_1_2,
                   BYAFILE.BYA_INT_PL_1_1_2,
                   BYAFILE.BYA_NO_PL_1_1_3,
                   BYAFILE.BYA_AMT_PL_1_1_3,
                   BYAFILE.BYA_O2_PL_1_1_3,
                   BYAFILE.BYA_INT_PL_1_1_3,
                   BYAFILE.BYA_NO_PL_1_1_4,
                   BYAFILE.BYA_AMT_PL_1_1_4,
                   BYAFILE.BYA_O2_PL_1_1_4,
                   BYAFILE.BYA_INT_PL_1_1_4,
                   BYAFILE.BYA_NO_PL_1_1_5,
                   BYAFILE.BYA_AMT_PL_1_1_5,
                   BYAFILE.BYA_O2_PL_1_1_5,
                   BYAFILE.BYA_INT_PL_1_1_5,
                   BYAFILE.BYA_NO_PL_1_1_6,
                   BYAFILE.BYA_AMT_PL_1_1_6,
                   BYAFILE.BYA_O2_PL_1_1_6,
                   BYAFILE.BYA_INT_PL_1_1_6,
                   BYAFILE.BYA_NO_PL_1_1_7,
                   BYAFILE.BYA_AMT_PL_1_1_7,
                   BYAFILE.BYA_O2_PL_1_1_7,
                   BYAFILE.BYA_INT_PL_1_1_7,
                   BYAFILE.BYA_NO_PL_1_1_8,
                   BYAFILE.BYA_AMT_PL_1_1_8,
                   BYAFILE.BYA_O2_PL_1_1_8,
                   BYAFILE.BYA_INT_PL_1_1_8,
                   BYAFILE.BYA_NO_PL_1_1_9,
                   BYAFILE.BYA_AMT_PL_1_1_9,
                   BYAFILE.BYA_O2_PL_1_1_9,
                   BYAFILE.BYA_INT_PL_1_1_9,
                   BYAFILE.BYA_NO_PL_1_1_10,
                   BYAFILE.BYA_AMT_PL_1_1_10,
                   BYAFILE.BYA_O2_PL_1_1_10,
                   BYAFILE.BYA_INT_PL_1_1_10,
                   BYAFILE.BYA_NO_PL_1_2_1,
                   BYAFILE.BYA_AMT_PL_1_2_1,
                   BYAFILE.BYA_O2_PL_1_2_1,
                   BYAFILE.BYA_INT_PL_1_2_1,
                   BYAFILE.BYA_NO_PL_1_2_2,
                   BYAFILE.BYA_AMT_PL_1_2_2,
                   BYAFILE.BYA_O2_PL_1_2_2,
                   BYAFILE.BYA_INT_PL_1_2_2,
                   BYAFILE.BYA_NO_PL_1_2_3,
                   BYAFILE.BYA_AMT_PL_1_2_3,
                   BYAFILE.BYA_O2_PL_1_2_3,
                   BYAFILE.BYA_INT_PL_1_2_3,
                   BYAFILE.BYA_NO_PL_1_2_4,
                   BYAFILE.BYA_AMT_PL_1_2_4,
                   BYAFILE.BYA_O2_PL_1_2_4,
                   BYAFILE.BYA_INT_PL_1_2_4,
                   BYAFILE.BYA_NO_PL_1_2_5,
                   BYAFILE.BYA_AMT_PL_1_2_5,
                   BYAFILE.BYA_O2_PL_1_2_5,
                   BYAFILE.BYA_INT_PL_1_2_5,
                   BYAFILE.BYA_NO_PL_1_2_6,
                   BYAFILE.BYA_AMT_PL_1_2_6,
                   BYAFILE.BYA_O2_PL_1_2_6,
                   BYAFILE.BYA_INT_PL_1_2_6,
                   BYAFILE.BYA_NO_PL_1_2_7,
                   BYAFILE.BYA_AMT_PL_1_2_7,
                   BYAFILE.BYA_O2_PL_1_2_7,
                   BYAFILE.BYA_INT_PL_1_2_7,
                   BYAFILE.BYA_NO_PL_1_2_8,
                   BYAFILE.BYA_AMT_PL_1_2_8,
                   BYAFILE.BYA_O2_PL_1_2_8,
                   BYAFILE.BYA_INT_PL_1_2_8,
                   BYAFILE.BYA_NO_PL_1_2_9,
                   BYAFILE.BYA_AMT_PL_1_2_9,
                   BYAFILE.BYA_O2_PL_1_2_9,
                   BYAFILE.BYA_INT_PL_1_2_9,
                   BYAFILE.BYA_NO_PL_1_2_10,
                   BYAFILE.BYA_AMT_PL_1_2_10,
                   BYAFILE.BYA_O2_PL_1_2_10,
                   BYAFILE.BYA_INT_PL_1_2_10,
                   BYAFILE.BYA_NO_PL_2_1_1,
                   BYAFILE.BYA_AMT_PL_2_1_1,
                   BYAFILE.BYA_O2_PL_2_1_1,
                   BYAFILE.BYA_INT_PL_2_1_1,
                   BYAFILE.BYA_NO_PL_2_1_2,
                   BYAFILE.BYA_AMT_PL_2_1_2,
                   BYAFILE.BYA_O2_PL_2_1_2,
                   BYAFILE.BYA_INT_PL_2_1_2,
                   BYAFILE.BYA_NO_PL_2_1_3,
                   BYAFILE.BYA_AMT_PL_2_1_3,
                   BYAFILE.BYA_O2_PL_2_1_3,
                   BYAFILE.BYA_INT_PL_2_1_3,
                   BYAFILE.BYA_NO_PL_2_1_4,
                   BYAFILE.BYA_AMT_PL_2_1_4,
                   BYAFILE.BYA_O2_PL_2_1_4,
                   BYAFILE.BYA_INT_PL_2_1_4,
                   BYAFILE.BYA_NO_PL_2_1_5,
                   BYAFILE.BYA_AMT_PL_2_1_5,
                   BYAFILE.BYA_O2_PL_2_1_5,
                   BYAFILE.BYA_INT_PL_2_1_5,
                   BYAFILE.BYA_NO_PL_2_1_6,
                   BYAFILE.BYA_AMT_PL_2_1_6,
                   BYAFILE.BYA_O2_PL_2_1_6,
                   BYAFILE.BYA_INT_PL_2_1_6,
                   BYAFILE.BYA_NO_PL_2_1_7,
                   BYAFILE.BYA_AMT_PL_2_1_7,
                   BYAFILE.BYA_O2_PL_2_1_7,
                   BYAFILE.BYA_INT_PL_2_1_7,
                   BYAFILE.BYA_NO_PL_2_1_8,
                   BYAFILE.BYA_AMT_PL_2_1_8,
                   BYAFILE.BYA_O2_PL_2_1_8,
                   BYAFILE.BYA_INT_PL_2_1_8,
                   BYAFILE.BYA_NO_PL_2_1_9,
                   BYAFILE.BYA_AMT_PL_2_1_9,
                   BYAFILE.BYA_O2_PL_2_1_9,
                   BYAFILE.BYA_INT_PL_2_1_9,
                   BYAFILE.BYA_NO_PL_2_1_10,
                   BYAFILE.BYA_AMT_PL_2_1_10,
                   BYAFILE.BYA_O2_PL_2_1_10,
                   BYAFILE.BYA_INT_PL_2_1_10,
                   BYAFILE.BYA_NO_PL_2_2_1,
                   BYAFILE.BYA_AMT_PL_2_2_1,
                   BYAFILE.BYA_O2_PL_2_2_1,
                   BYAFILE.BYA_INT_PL_2_2_1,
                   BYAFILE.BYA_NO_PL_2_2_2,
                   BYAFILE.BYA_AMT_PL_2_2_2,
                   BYAFILE.BYA_O2_PL_2_2_2,
                   BYAFILE.BYA_INT_PL_2_2_2,
                   BYAFILE.BYA_NO_PL_2_2_3,
                   BYAFILE.BYA_AMT_PL_2_2_3,
                   BYAFILE.BYA_O2_PL_2_2_3,
                   BYAFILE.BYA_INT_PL_2_2_3,
                   BYAFILE.BYA_NO_PL_2_2_4,
                   BYAFILE.BYA_AMT_PL_2_2_4,
                   BYAFILE.BYA_O2_PL_2_2_4,
                   BYAFILE.BYA_INT_PL_2_2_4,
                   BYAFILE.BYA_NO_PL_2_2_5,
                   BYAFILE.BYA_AMT_PL_2_2_5,
                   BYAFILE.BYA_O2_PL_2_2_5,
                   BYAFILE.BYA_INT_PL_2_2_5,
                   BYAFILE.BYA_NO_PL_2_2_6,
                   BYAFILE.BYA_AMT_PL_2_2_6,
                   BYAFILE.BYA_O2_PL_2_2_6,
                   BYAFILE.BYA_INT_PL_2_2_6,
                   BYAFILE.BYA_NO_PL_2_2_7,
                   BYAFILE.BYA_AMT_PL_2_2_7,
                   BYAFILE.BYA_O2_PL_2_2_7,
                   BYAFILE.BYA_INT_PL_2_2_7,
                   BYAFILE.BYA_NO_PL_2_2_8,
                   BYAFILE.BYA_AMT_PL_2_2_8,
                   BYAFILE.BYA_O2_PL_2_2_8,
                   BYAFILE.BYA_INT_PL_2_2_8,
                   BYAFILE.BYA_NO_PL_2_2_9,
                   BYAFILE.BYA_AMT_PL_2_2_9,
                   BYAFILE.BYA_O2_PL_2_2_9,
                   BYAFILE.BYA_INT_PL_2_2_9,
                   BYAFILE.BYA_NO_PL_2_2_10,
                   BYAFILE.BYA_AMT_PL_2_2_10,
                   BYAFILE.BYA_O2_PL_2_2_10,
                   BYAFILE.BYA_INT_PL_2_2_10,
                   BYAFILE.BYA_NO_PL_3_1_1,
                   BYAFILE.BYA_AMT_PL_3_1_1,
                   BYAFILE.BYA_O2_PL_3_1_1,
                   BYAFILE.BYA_INT_PL_3_1_1,
                   BYAFILE.BYA_NO_PL_3_1_2,
                   BYAFILE.BYA_AMT_PL_3_1_2,
                   BYAFILE.BYA_O2_PL_3_1_2,
                   BYAFILE.BYA_INT_PL_3_1_2,
                   BYAFILE.BYA_NO_PL_3_1_3,
                   BYAFILE.BYA_AMT_PL_3_1_3,
                   BYAFILE.BYA_O2_PL_3_1_3,
                   BYAFILE.BYA_INT_PL_3_1_3,
                   BYAFILE.BYA_NO_PL_3_1_4,
                   BYAFILE.BYA_AMT_PL_3_1_4,
                   BYAFILE.BYA_O2_PL_3_1_4,
                   BYAFILE.BYA_INT_PL_3_1_4,
                   BYAFILE.BYA_NO_PL_3_1_5,
                   BYAFILE.BYA_AMT_PL_3_1_5,
                   BYAFILE.BYA_O2_PL_3_1_5,
                   BYAFILE.BYA_INT_PL_3_1_5,
                   BYAFILE.BYA_NO_PL_3_1_6,
                   BYAFILE.BYA_AMT_PL_3_1_6,
                   BYAFILE.BYA_O2_PL_3_1_6,
                   BYAFILE.BYA_INT_PL_3_1_6,
                   BYAFILE.BYA_NO_PL_3_1_7,
                   BYAFILE.BYA_AMT_PL_3_1_7,
                   BYAFILE.BYA_O2_PL_3_1_7,
                   BYAFILE.BYA_INT_PL_3_1_7,
                   BYAFILE.BYA_NO_PL_3_1_8,
                   BYAFILE.BYA_AMT_PL_3_1_8,
                   BYAFILE.BYA_O2_PL_3_1_8,
                   BYAFILE.BYA_INT_PL_3_1_8,
                   BYAFILE.BYA_NO_PL_3_1_9,
                   BYAFILE.BYA_AMT_PL_3_1_9,
                   BYAFILE.BYA_O2_PL_3_1_9,
                   BYAFILE.BYA_INT_PL_3_1_9,
                   BYAFILE.BYA_NO_PL_3_1_10,
                   BYAFILE.BYA_AMT_PL_3_1_10,
                   BYAFILE.BYA_O2_PL_3_1_10,
                   BYAFILE.BYA_INT_PL_3_1_10,
                   BYAFILE.BYA_NO_PL_3_2_1,
                   BYAFILE.BYA_AMT_PL_3_2_1,
                   BYAFILE.BYA_O2_PL_3_2_1,
                   BYAFILE.BYA_INT_PL_3_2_1,
                   BYAFILE.BYA_NO_PL_3_2_2,
                   BYAFILE.BYA_AMT_PL_3_2_2,
                   BYAFILE.BYA_O2_PL_3_2_2,
                   BYAFILE.BYA_INT_PL_3_2_2,
                   BYAFILE.BYA_NO_PL_3_2_3,
                   BYAFILE.BYA_AMT_PL_3_2_3,
                   BYAFILE.BYA_O2_PL_3_2_3,
                   BYAFILE.BYA_INT_PL_3_2_3,
                   BYAFILE.BYA_NO_PL_3_2_4,
                   BYAFILE.BYA_AMT_PL_3_2_4,
                   BYAFILE.BYA_O2_PL_3_2_4,
                   BYAFILE.BYA_INT_PL_3_2_4,
                   BYAFILE.BYA_NO_PL_3_2_5,
                   BYAFILE.BYA_AMT_PL_3_2_5,
                   BYAFILE.BYA_O2_PL_3_2_5,
                   BYAFILE.BYA_INT_PL_3_2_5,
                   BYAFILE.BYA_NO_PL_3_2_6,
                   BYAFILE.BYA_AMT_PL_3_2_6,
                   BYAFILE.BYA_O2_PL_3_2_6,
                   BYAFILE.BYA_INT_PL_3_2_6,
                   BYAFILE.BYA_NO_PL_3_2_7,
                   BYAFILE.BYA_AMT_PL_3_2_7,
                   BYAFILE.BYA_O2_PL_3_2_7,
                   BYAFILE.BYA_INT_PL_3_2_7,
                   BYAFILE.BYA_NO_PL_3_2_8,
                   BYAFILE.BYA_AMT_PL_3_2_8,
                   BYAFILE.BYA_O2_PL_3_2_8,
                   BYAFILE.BYA_INT_PL_3_2_8,
                   BYAFILE.BYA_NO_PL_3_2_9,
                   BYAFILE.BYA_AMT_PL_3_2_9,
                   BYAFILE.BYA_O2_PL_3_2_9,
                   BYAFILE.BYA_INT_PL_3_2_9,
                   BYAFILE.BYA_NO_PL_3_2_10,
                   BYAFILE.BYA_AMT_PL_3_2_10,
                   BYAFILE.BYA_O2_PL_3_2_10,
                   BYAFILE.BYA_INT_PL_3_2_10,
                   BYAFILE.BYA_DLRVPRIN_1_1_1,
                   BYAFILE.BYA_DLRVPRIN_1_1_2,
                   BYAFILE.BYA_DLRVPRIN_1_1_3,
                   BYAFILE.BYA_DLRVCHGS_1_1_1,
                   BYAFILE.BYA_DLRVCHGS_1_1_2,
                   BYAFILE.BYA_DLRVCHGS_1_1_3,
                   BYAFILE.BYA_DLPARVHELD_1_1,
                   BYAFILE.BYA_DLRVPRIN_1_2_1,
                   BYAFILE.BYA_DLRVPRIN_1_2_2,
                   BYAFILE.BYA_DLRVPRIN_1_2_3,
                   BYAFILE.BYA_DLRVCHGS_1_2_1,
                   BYAFILE.BYA_DLRVCHGS_1_2_2,
                   BYAFILE.BYA_DLRVCHGS_1_2_3,
                   BYAFILE.BYA_DLPARVHELD_1_2,
                   BYAFILE.BYA_DLRVPRIN_2_1_1,
                   BYAFILE.BYA_DLRVPRIN_2_1_2,
                   BYAFILE.BYA_DLRVPRIN_2_1_3,
                   BYAFILE.BYA_DLRVCHGS_2_1_1,
                   BYAFILE.BYA_DLRVCHGS_2_1_2,
                   BYAFILE.BYA_DLRVCHGS_2_1_3,
                   BYAFILE.BYA_DLPARVHELD_2_1,
                   BYAFILE.BYA_DLRVPRIN_2_2_1,
                   BYAFILE.BYA_DLRVPRIN_2_2_2,
                   BYAFILE.BYA_DLRVPRIN_2_2_3,
                   BYAFILE.BYA_DLRVCHGS_2_2_1,
                   BYAFILE.BYA_DLRVCHGS_2_2_2,
                   BYAFILE.BYA_DLRVCHGS_2_2_3,
                   BYAFILE.BYA_DLPARVHELD_2_2,
                   BYAFILE.BYA_DLRVPRIN_3_1_1,
                   BYAFILE.BYA_DLRVPRIN_3_1_2,
                   BYAFILE.BYA_DLRVPRIN_3_1_3,
                   BYAFILE.BYA_DLRVCHGS_3_1_1,
                   BYAFILE.BYA_DLRVCHGS_3_1_2,
                   BYAFILE.BYA_DLRVCHGS_3_1_3,
                   BYAFILE.BYA_DLPARVHELD_3_1,
                   BYAFILE.BYA_DLRVPRIN_3_2_1,
                   BYAFILE.BYA_DLRVPRIN_3_2_2,
                   BYAFILE.BYA_DLRVPRIN_3_2_3,
                   BYAFILE.BYA_DLRVCHGS_3_2_1,
                   BYAFILE.BYA_DLRVCHGS_3_2_2,
                   BYAFILE.BYA_DLRVCHGS_3_2_3,
                   BYAFILE.BYA_DLPARVHELD_3_2
            INTO :QBYA-REC
            FROM DBO.BYAFILE
            WHERE BYAFILE.BYA_DATE_CCYYMM = :QBYA-DATE-CCYYMM
              AND BYAFILE.BYA_BRNO = :QBYA-BRNO 
              AND BYAFILE.BYA_CLASS = :QBYA-CLASS
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BYA1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-BYA-FIELDS.

      *-----------------------------------------------------------------
       READ-BYA1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BYA-PATH-SQL TO E-FILE.
           MOVE BYA1-KEY TO E-KEYX.
           INITIALIZE QBYA-REC.

           EXEC SQL SET CONNECTION BY1_CONNECT END-EXEC.

           IF ( BYA1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BYA1_CURSOR INTO :QBYA-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BYA1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BYA-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       WRITE-BYA1-FILE.

           ACCEPT BYA-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE BYA-PATH-SQL TO E-FILE.
           MOVE BYA1-KEY TO E-KEYX.
           PERFORM SET-BYA-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            INSERT INTO DBO.BYAFILE
             (BYAFILE.BYA_DATE_CCYYMM,
              BYAFILE.BYA_BRNO,
              BYAFILE.BYA_CLASS,
              BYAFILE.BYA_LTOUCH_DATE,
              BYAFILE.BYA_NO_1_1_1,
              BYAFILE.BYA_AMT_1_1_1,
              BYAFILE.BYA_O2_1_1_1,
              BYAFILE.BYA_INT_1_1_1,
              BYAFILE.BYA_NO_1_1_2,
              BYAFILE.BYA_AMT_1_1_2,
              BYAFILE.BYA_O2_1_1_2,
              BYAFILE.BYA_INT_1_1_2,
              BYAFILE.BYA_NO_1_1_3,
              BYAFILE.BYA_AMT_1_1_3,
              BYAFILE.BYA_O2_1_1_3,
              BYAFILE.BYA_INT_1_1_3,
              BYAFILE.BYA_NO_1_1_4,
              BYAFILE.BYA_AMT_1_1_4,
              BYAFILE.BYA_O2_1_1_4,
              BYAFILE.BYA_INT_1_1_4,
              BYAFILE.BYA_NO_1_1_5,
              BYAFILE.BYA_AMT_1_1_5,
              BYAFILE.BYA_O2_1_1_5,
              BYAFILE.BYA_INT_1_1_5,
              BYAFILE.BYA_NO_1_1_6,
              BYAFILE.BYA_AMT_1_1_6,
              BYAFILE.BYA_O2_1_1_6,
              BYAFILE.BYA_INT_1_1_6,
              BYAFILE.BYA_NO_1_1_7,
              BYAFILE.BYA_AMT_1_1_7,
              BYAFILE.BYA_O2_1_1_7,
              BYAFILE.BYA_INT_1_1_7,
              BYAFILE.BYA_NO_1_1_8,
              BYAFILE.BYA_AMT_1_1_8,
              BYAFILE.BYA_O2_1_1_8,
              BYAFILE.BYA_INT_1_1_8,
              BYAFILE.BYA_NO_1_1_9,
              BYAFILE.BYA_AMT_1_1_9,
              BYAFILE.BYA_O2_1_1_9,
              BYAFILE.BYA_INT_1_1_9,
              BYAFILE.BYA_NO_1_1_10,
              BYAFILE.BYA_AMT_1_1_10,
              BYAFILE.BYA_O2_1_1_10,
              BYAFILE.BYA_INT_1_1_10,
              BYAFILE.BYA_NO_1_1_11,
              BYAFILE.BYA_AMT_1_1_11,
              BYAFILE.BYA_O2_1_1_11,
              BYAFILE.BYA_INT_1_1_11,
              BYAFILE.BYA_NO_1_2_1,
              BYAFILE.BYA_AMT_1_2_1,
              BYAFILE.BYA_O2_1_2_1,
              BYAFILE.BYA_INT_1_2_1,
              BYAFILE.BYA_NO_1_2_2,
              BYAFILE.BYA_AMT_1_2_2,
              BYAFILE.BYA_O2_1_2_2,
              BYAFILE.BYA_INT_1_2_2,
              BYAFILE.BYA_NO_1_2_3,
              BYAFILE.BYA_AMT_1_2_3,
              BYAFILE.BYA_O2_1_2_3,
              BYAFILE.BYA_INT_1_2_3,
              BYAFILE.BYA_NO_1_2_4,
              BYAFILE.BYA_AMT_1_2_4,
              BYAFILE.BYA_O2_1_2_4,
              BYAFILE.BYA_INT_1_2_4,
              BYAFILE.BYA_NO_1_2_5,
              BYAFILE.BYA_AMT_1_2_5,
              BYAFILE.BYA_O2_1_2_5,
              BYAFILE.BYA_INT_1_2_5,
              BYAFILE.BYA_NO_1_2_6,
              BYAFILE.BYA_AMT_1_2_6,
              BYAFILE.BYA_O2_1_2_6,
              BYAFILE.BYA_INT_1_2_6,
              BYAFILE.BYA_NO_1_2_7,
              BYAFILE.BYA_AMT_1_2_7,
              BYAFILE.BYA_O2_1_2_7,
              BYAFILE.BYA_INT_1_2_7,
              BYAFILE.BYA_NO_1_2_8,
              BYAFILE.BYA_AMT_1_2_8,
              BYAFILE.BYA_O2_1_2_8,
              BYAFILE.BYA_INT_1_2_8,
              BYAFILE.BYA_NO_1_2_9,
              BYAFILE.BYA_AMT_1_2_9,
              BYAFILE.BYA_O2_1_2_9,
              BYAFILE.BYA_INT_1_2_9,
              BYAFILE.BYA_NO_1_2_10,
              BYAFILE.BYA_AMT_1_2_10,
              BYAFILE.BYA_O2_1_2_10,
              BYAFILE.BYA_INT_1_2_10,
              BYAFILE.BYA_NO_1_2_11,
              BYAFILE.BYA_AMT_1_2_11,
              BYAFILE.BYA_O2_1_2_11,
              BYAFILE.BYA_INT_1_2_11,
              BYAFILE.BYA_NO_2_1_1,
              BYAFILE.BYA_AMT_2_1_1,
              BYAFILE.BYA_O2_2_1_1,
              BYAFILE.BYA_INT_2_1_1,
              BYAFILE.BYA_NO_2_1_2,
              BYAFILE.BYA_AMT_2_1_2,
              BYAFILE.BYA_O2_2_1_2,
              BYAFILE.BYA_INT_2_1_2,
              BYAFILE.BYA_NO_2_1_3,
              BYAFILE.BYA_AMT_2_1_3,
              BYAFILE.BYA_O2_2_1_3,
              BYAFILE.BYA_INT_2_1_3,
              BYAFILE.BYA_NO_2_1_4,
              BYAFILE.BYA_AMT_2_1_4,
              BYAFILE.BYA_O2_2_1_4,
              BYAFILE.BYA_INT_2_1_4,
              BYAFILE.BYA_NO_2_1_5,
              BYAFILE.BYA_AMT_2_1_5,
              BYAFILE.BYA_O2_2_1_5,
              BYAFILE.BYA_INT_2_1_5,
              BYAFILE.BYA_NO_2_1_6,
              BYAFILE.BYA_AMT_2_1_6,
              BYAFILE.BYA_O2_2_1_6,
              BYAFILE.BYA_INT_2_1_6,
              BYAFILE.BYA_NO_2_1_7,
              BYAFILE.BYA_AMT_2_1_7,
              BYAFILE.BYA_O2_2_1_7,
              BYAFILE.BYA_INT_2_1_7,
              BYAFILE.BYA_NO_2_1_8,
              BYAFILE.BYA_AMT_2_1_8,
              BYAFILE.BYA_O2_2_1_8,
              BYAFILE.BYA_INT_2_1_8,
              BYAFILE.BYA_NO_2_1_9,
              BYAFILE.BYA_AMT_2_1_9,
              BYAFILE.BYA_O2_2_1_9,
              BYAFILE.BYA_INT_2_1_9,
              BYAFILE.BYA_NO_2_1_10,
              BYAFILE.BYA_AMT_2_1_10,
              BYAFILE.BYA_O2_2_1_10,
              BYAFILE.BYA_INT_2_1_10,
              BYAFILE.BYA_NO_2_1_11,
              BYAFILE.BYA_AMT_2_1_11,
              BYAFILE.BYA_O2_2_1_11,
              BYAFILE.BYA_INT_2_1_11,
              BYAFILE.BYA_NO_2_2_1,
              BYAFILE.BYA_AMT_2_2_1,
              BYAFILE.BYA_O2_2_2_1,
              BYAFILE.BYA_INT_2_2_1,
              BYAFILE.BYA_NO_2_2_2,
              BYAFILE.BYA_AMT_2_2_2,
              BYAFILE.BYA_O2_2_2_2,
              BYAFILE.BYA_INT_2_2_2,
              BYAFILE.BYA_NO_2_2_3,
              BYAFILE.BYA_AMT_2_2_3,
              BYAFILE.BYA_O2_2_2_3,
              BYAFILE.BYA_INT_2_2_3,
              BYAFILE.BYA_NO_2_2_4,
              BYAFILE.BYA_AMT_2_2_4,
              BYAFILE.BYA_O2_2_2_4,
              BYAFILE.BYA_INT_2_2_4,
              BYAFILE.BYA_NO_2_2_5,
              BYAFILE.BYA_AMT_2_2_5,
              BYAFILE.BYA_O2_2_2_5,
              BYAFILE.BYA_INT_2_2_5,
              BYAFILE.BYA_NO_2_2_6,
              BYAFILE.BYA_AMT_2_2_6,
              BYAFILE.BYA_O2_2_2_6,
              BYAFILE.BYA_INT_2_2_6,
              BYAFILE.BYA_NO_2_2_7,
              BYAFILE.BYA_AMT_2_2_7,
              BYAFILE.BYA_O2_2_2_7,
              BYAFILE.BYA_INT_2_2_7,
              BYAFILE.BYA_NO_2_2_8,
              BYAFILE.BYA_AMT_2_2_8,
              BYAFILE.BYA_O2_2_2_8,
              BYAFILE.BYA_INT_2_2_8,
              BYAFILE.BYA_NO_2_2_9,
              BYAFILE.BYA_AMT_2_2_9,
              BYAFILE.BYA_O2_2_2_9,
              BYAFILE.BYA_INT_2_2_9,
              BYAFILE.BYA_NO_2_2_10,
              BYAFILE.BYA_AMT_2_2_10,
              BYAFILE.BYA_O2_2_2_10,
              BYAFILE.BYA_INT_2_2_10,
              BYAFILE.BYA_NO_2_2_11,
              BYAFILE.BYA_AMT_2_2_11,
              BYAFILE.BYA_O2_2_2_11,
              BYAFILE.BYA_INT_2_2_11,
              BYAFILE.BYA_NO_3_1_1,
              BYAFILE.BYA_AMT_3_1_1,
              BYAFILE.BYA_O2_3_1_1,
              BYAFILE.BYA_INT_3_1_1,
              BYAFILE.BYA_NO_3_1_2,
              BYAFILE.BYA_AMT_3_1_2,
              BYAFILE.BYA_O2_3_1_2,
              BYAFILE.BYA_INT_3_1_2,
              BYAFILE.BYA_NO_3_1_3,
              BYAFILE.BYA_AMT_3_1_3,
              BYAFILE.BYA_O2_3_1_3,
              BYAFILE.BYA_INT_3_1_3,
              BYAFILE.BYA_NO_3_1_4,
              BYAFILE.BYA_AMT_3_1_4,
              BYAFILE.BYA_O2_3_1_4,
              BYAFILE.BYA_INT_3_1_4,
              BYAFILE.BYA_NO_3_1_5,
              BYAFILE.BYA_AMT_3_1_5,
              BYAFILE.BYA_O2_3_1_5,
              BYAFILE.BYA_INT_3_1_5,
              BYAFILE.BYA_NO_3_1_6,
              BYAFILE.BYA_AMT_3_1_6,
              BYAFILE.BYA_O2_3_1_6,
              BYAFILE.BYA_INT_3_1_6,
              BYAFILE.BYA_NO_3_1_7,
              BYAFILE.BYA_AMT_3_1_7,
              BYAFILE.BYA_O2_3_1_7,
              BYAFILE.BYA_INT_3_1_7,
              BYAFILE.BYA_NO_3_1_8,
              BYAFILE.BYA_AMT_3_1_8,
              BYAFILE.BYA_O2_3_1_8,
              BYAFILE.BYA_INT_3_1_8,
              BYAFILE.BYA_NO_3_1_9,
              BYAFILE.BYA_AMT_3_1_9,
              BYAFILE.BYA_O2_3_1_9,
              BYAFILE.BYA_INT_3_1_9,
              BYAFILE.BYA_NO_3_1_10,
              BYAFILE.BYA_AMT_3_1_10,
              BYAFILE.BYA_O2_3_1_10,
              BYAFILE.BYA_INT_3_1_10,
              BYAFILE.BYA_NO_3_1_11,
              BYAFILE.BYA_AMT_3_1_11,
              BYAFILE.BYA_O2_3_1_11,
              BYAFILE.BYA_INT_3_1_11,
              BYAFILE.BYA_NO_3_2_1,
              BYAFILE.BYA_AMT_3_2_1,
              BYAFILE.BYA_O2_3_2_1,
              BYAFILE.BYA_INT_3_2_1,
              BYAFILE.BYA_NO_3_2_2,
              BYAFILE.BYA_AMT_3_2_2,
              BYAFILE.BYA_O2_3_2_2,
              BYAFILE.BYA_INT_3_2_2,
              BYAFILE.BYA_NO_3_2_3,
              BYAFILE.BYA_AMT_3_2_3,
              BYAFILE.BYA_O2_3_2_3,
              BYAFILE.BYA_INT_3_2_3,
              BYAFILE.BYA_NO_3_2_4,
              BYAFILE.BYA_AMT_3_2_4,
              BYAFILE.BYA_O2_3_2_4,
              BYAFILE.BYA_INT_3_2_4,
              BYAFILE.BYA_NO_3_2_5,
              BYAFILE.BYA_AMT_3_2_5,
              BYAFILE.BYA_O2_3_2_5,
              BYAFILE.BYA_INT_3_2_5,
              BYAFILE.BYA_NO_3_2_6,
              BYAFILE.BYA_AMT_3_2_6,
              BYAFILE.BYA_O2_3_2_6,
              BYAFILE.BYA_INT_3_2_6,
              BYAFILE.BYA_NO_3_2_7,
              BYAFILE.BYA_AMT_3_2_7,
              BYAFILE.BYA_O2_3_2_7,
              BYAFILE.BYA_INT_3_2_7,
              BYAFILE.BYA_NO_3_2_8,
              BYAFILE.BYA_AMT_3_2_8,
              BYAFILE.BYA_O2_3_2_8,
              BYAFILE.BYA_INT_3_2_8,
              BYAFILE.BYA_NO_3_2_9,
              BYAFILE.BYA_AMT_3_2_9,
              BYAFILE.BYA_O2_3_2_9,
              BYAFILE.BYA_INT_3_2_9,
              BYAFILE.BYA_NO_3_2_10,
              BYAFILE.BYA_AMT_3_2_10,
              BYAFILE.BYA_O2_3_2_10,
              BYAFILE.BYA_INT_3_2_10,
              BYAFILE.BYA_NO_3_2_11,
              BYAFILE.BYA_AMT_3_2_11,
              BYAFILE.BYA_O2_3_2_11,
              BYAFILE.BYA_INT_3_2_11,
              BYAFILE.BYA_NO_REPO_1_1_1,
              BYAFILE.BYA_AMT_REPO_1_1_1,
              BYAFILE.BYA_O2_REPO_1_1_1,
              BYAFILE.BYA_INT_REPO_1_1_1,
              BYAFILE.BYA_NO_REPO_1_1_2,
              BYAFILE.BYA_AMT_REPO_1_1_2,
              BYAFILE.BYA_O2_REPO_1_1_2,
              BYAFILE.BYA_INT_REPO_1_1_2,
              BYAFILE.BYA_NO_REPO_1_1_3,
              BYAFILE.BYA_AMT_REPO_1_1_3,
              BYAFILE.BYA_O2_REPO_1_1_3,
              BYAFILE.BYA_INT_REPO_1_1_3,
              BYAFILE.BYA_NO_REPO_1_1_4,
              BYAFILE.BYA_AMT_REPO_1_1_4,
              BYAFILE.BYA_O2_REPO_1_1_4,
              BYAFILE.BYA_INT_REPO_1_1_4,
              BYAFILE.BYA_NO_REPO_1_1_5,
              BYAFILE.BYA_AMT_REPO_1_1_5,
              BYAFILE.BYA_O2_REPO_1_1_5,
              BYAFILE.BYA_INT_REPO_1_1_5,
              BYAFILE.BYA_NO_REPO_1_2_1,
              BYAFILE.BYA_AMT_REPO_1_2_1,
              BYAFILE.BYA_O2_REPO_1_2_1,
              BYAFILE.BYA_INT_REPO_1_2_1,
              BYAFILE.BYA_NO_REPO_1_2_2,
              BYAFILE.BYA_AMT_REPO_1_2_2,
              BYAFILE.BYA_O2_REPO_1_2_2,
              BYAFILE.BYA_INT_REPO_1_2_2,
              BYAFILE.BYA_NO_REPO_1_2_3,
              BYAFILE.BYA_AMT_REPO_1_2_3,
              BYAFILE.BYA_O2_REPO_1_2_3,
              BYAFILE.BYA_INT_REPO_1_2_3,
              BYAFILE.BYA_NO_REPO_1_2_4,
              BYAFILE.BYA_AMT_REPO_1_2_4,
              BYAFILE.BYA_O2_REPO_1_2_4,
              BYAFILE.BYA_INT_REPO_1_2_4,
              BYAFILE.BYA_NO_REPO_1_2_5,
              BYAFILE.BYA_AMT_REPO_1_2_5,
              BYAFILE.BYA_O2_REPO_1_2_5,
              BYAFILE.BYA_INT_REPO_1_2_5,
              BYAFILE.BYA_NO_REPO_2_1_1,
              BYAFILE.BYA_AMT_REPO_2_1_1,
              BYAFILE.BYA_O2_REPO_2_1_1,
              BYAFILE.BYA_INT_REPO_2_1_1,
              BYAFILE.BYA_NO_REPO_2_1_2,
              BYAFILE.BYA_AMT_REPO_2_1_2,
              BYAFILE.BYA_O2_REPO_2_1_2,
              BYAFILE.BYA_INT_REPO_2_1_2,
              BYAFILE.BYA_NO_REPO_2_1_3,
              BYAFILE.BYA_AMT_REPO_2_1_3,
              BYAFILE.BYA_O2_REPO_2_1_3,
              BYAFILE.BYA_INT_REPO_2_1_3,
              BYAFILE.BYA_NO_REPO_2_1_4,
              BYAFILE.BYA_AMT_REPO_2_1_4,
              BYAFILE.BYA_O2_REPO_2_1_4,
              BYAFILE.BYA_INT_REPO_2_1_4,
              BYAFILE.BYA_NO_REPO_2_1_5,
              BYAFILE.BYA_AMT_REPO_2_1_5,
              BYAFILE.BYA_O2_REPO_2_1_5,
              BYAFILE.BYA_INT_REPO_2_1_5,
              BYAFILE.BYA_NO_REPO_2_2_1,
              BYAFILE.BYA_AMT_REPO_2_2_1,
              BYAFILE.BYA_O2_REPO_2_2_1,
              BYAFILE.BYA_INT_REPO_2_2_1,
              BYAFILE.BYA_NO_REPO_2_2_2,
              BYAFILE.BYA_AMT_REPO_2_2_2,
              BYAFILE.BYA_O2_REPO_2_2_2,
              BYAFILE.BYA_INT_REPO_2_2_2,
              BYAFILE.BYA_NO_REPO_2_2_3,
              BYAFILE.BYA_AMT_REPO_2_2_3,
              BYAFILE.BYA_O2_REPO_2_2_3,
              BYAFILE.BYA_INT_REPO_2_2_3,
              BYAFILE.BYA_NO_REPO_2_2_4,
              BYAFILE.BYA_AMT_REPO_2_2_4,
              BYAFILE.BYA_O2_REPO_2_2_4,
              BYAFILE.BYA_INT_REPO_2_2_4,
              BYAFILE.BYA_NO_REPO_2_2_5,
              BYAFILE.BYA_AMT_REPO_2_2_5,
              BYAFILE.BYA_O2_REPO_2_2_5,
              BYAFILE.BYA_INT_REPO_2_2_5,
              BYAFILE.BYA_NO_REPO_3_1_1,
              BYAFILE.BYA_AMT_REPO_3_1_1,
              BYAFILE.BYA_O2_REPO_3_1_1,
              BYAFILE.BYA_INT_REPO_3_1_1,
              BYAFILE.BYA_NO_REPO_3_1_2,
              BYAFILE.BYA_AMT_REPO_3_1_2,
              BYAFILE.BYA_O2_REPO_3_1_2,
              BYAFILE.BYA_INT_REPO_3_1_2,
              BYAFILE.BYA_NO_REPO_3_1_3,
              BYAFILE.BYA_AMT_REPO_3_1_3,
              BYAFILE.BYA_O2_REPO_3_1_3,
              BYAFILE.BYA_INT_REPO_3_1_3,
              BYAFILE.BYA_NO_REPO_3_1_4,
              BYAFILE.BYA_AMT_REPO_3_1_4,
              BYAFILE.BYA_O2_REPO_3_1_4,
              BYAFILE.BYA_INT_REPO_3_1_4,
              BYAFILE.BYA_NO_REPO_3_1_5,
              BYAFILE.BYA_AMT_REPO_3_1_5,
              BYAFILE.BYA_O2_REPO_3_1_5,
              BYAFILE.BYA_INT_REPO_3_1_5,
              BYAFILE.BYA_NO_REPO_3_2_1,
              BYAFILE.BYA_AMT_REPO_3_2_1,
              BYAFILE.BYA_O2_REPO_3_2_1,
              BYAFILE.BYA_INT_REPO_3_2_1,
              BYAFILE.BYA_NO_REPO_3_2_2,
              BYAFILE.BYA_AMT_REPO_3_2_2,
              BYAFILE.BYA_O2_REPO_3_2_2,
              BYAFILE.BYA_INT_REPO_3_2_2,
              BYAFILE.BYA_NO_REPO_3_2_3,
              BYAFILE.BYA_AMT_REPO_3_2_3,
              BYAFILE.BYA_O2_REPO_3_2_3,
              BYAFILE.BYA_INT_REPO_3_2_3,
              BYAFILE.BYA_NO_REPO_3_2_4,
              BYAFILE.BYA_AMT_REPO_3_2_4,
              BYAFILE.BYA_O2_REPO_3_2_4,
              BYAFILE.BYA_INT_REPO_3_2_4,
              BYAFILE.BYA_NO_REPO_3_2_5,
              BYAFILE.BYA_AMT_REPO_3_2_5,
              BYAFILE.BYA_O2_REPO_3_2_5,
              BYAFILE.BYA_INT_REPO_3_2_5,
              BYAFILE.BYA_NO_PL_1_1_1,
              BYAFILE.BYA_AMT_PL_1_1_1,
              BYAFILE.BYA_O2_PL_1_1_1,
              BYAFILE.BYA_INT_PL_1_1_1,
              BYAFILE.BYA_NO_PL_1_1_2,
              BYAFILE.BYA_AMT_PL_1_1_2,
              BYAFILE.BYA_O2_PL_1_1_2,
              BYAFILE.BYA_INT_PL_1_1_2,
              BYAFILE.BYA_NO_PL_1_1_3,
              BYAFILE.BYA_AMT_PL_1_1_3,
              BYAFILE.BYA_O2_PL_1_1_3,
              BYAFILE.BYA_INT_PL_1_1_3,
              BYAFILE.BYA_NO_PL_1_1_4,
              BYAFILE.BYA_AMT_PL_1_1_4,
              BYAFILE.BYA_O2_PL_1_1_4,
              BYAFILE.BYA_INT_PL_1_1_4,
              BYAFILE.BYA_NO_PL_1_1_5,
              BYAFILE.BYA_AMT_PL_1_1_5,
              BYAFILE.BYA_O2_PL_1_1_5,
              BYAFILE.BYA_INT_PL_1_1_5,
              BYAFILE.BYA_NO_PL_1_1_6,
              BYAFILE.BYA_AMT_PL_1_1_6,
              BYAFILE.BYA_O2_PL_1_1_6,
              BYAFILE.BYA_INT_PL_1_1_6,
              BYAFILE.BYA_NO_PL_1_1_7,
              BYAFILE.BYA_AMT_PL_1_1_7,
              BYAFILE.BYA_O2_PL_1_1_7,
              BYAFILE.BYA_INT_PL_1_1_7,
              BYAFILE.BYA_NO_PL_1_1_8,
              BYAFILE.BYA_AMT_PL_1_1_8,
              BYAFILE.BYA_O2_PL_1_1_8,
              BYAFILE.BYA_INT_PL_1_1_8,
              BYAFILE.BYA_NO_PL_1_1_9,
              BYAFILE.BYA_AMT_PL_1_1_9,
              BYAFILE.BYA_O2_PL_1_1_9,
              BYAFILE.BYA_INT_PL_1_1_9,
              BYAFILE.BYA_NO_PL_1_1_10,
              BYAFILE.BYA_AMT_PL_1_1_10,
              BYAFILE.BYA_O2_PL_1_1_10,
              BYAFILE.BYA_INT_PL_1_1_10,
              BYAFILE.BYA_NO_PL_1_2_1,
              BYAFILE.BYA_AMT_PL_1_2_1,
              BYAFILE.BYA_O2_PL_1_2_1,
              BYAFILE.BYA_INT_PL_1_2_1,
              BYAFILE.BYA_NO_PL_1_2_2,
              BYAFILE.BYA_AMT_PL_1_2_2,
              BYAFILE.BYA_O2_PL_1_2_2,
              BYAFILE.BYA_INT_PL_1_2_2,
              BYAFILE.BYA_NO_PL_1_2_3,
              BYAFILE.BYA_AMT_PL_1_2_3,
              BYAFILE.BYA_O2_PL_1_2_3,
              BYAFILE.BYA_INT_PL_1_2_3,
              BYAFILE.BYA_NO_PL_1_2_4,
              BYAFILE.BYA_AMT_PL_1_2_4,
              BYAFILE.BYA_O2_PL_1_2_4,
              BYAFILE.BYA_INT_PL_1_2_4,
              BYAFILE.BYA_NO_PL_1_2_5,
              BYAFILE.BYA_AMT_PL_1_2_5,
              BYAFILE.BYA_O2_PL_1_2_5,
              BYAFILE.BYA_INT_PL_1_2_5,
              BYAFILE.BYA_NO_PL_1_2_6,
              BYAFILE.BYA_AMT_PL_1_2_6,
              BYAFILE.BYA_O2_PL_1_2_6,
              BYAFILE.BYA_INT_PL_1_2_6,
              BYAFILE.BYA_NO_PL_1_2_7,
              BYAFILE.BYA_AMT_PL_1_2_7,
              BYAFILE.BYA_O2_PL_1_2_7,
              BYAFILE.BYA_INT_PL_1_2_7,
              BYAFILE.BYA_NO_PL_1_2_8,
              BYAFILE.BYA_AMT_PL_1_2_8,
              BYAFILE.BYA_O2_PL_1_2_8,
              BYAFILE.BYA_INT_PL_1_2_8,
              BYAFILE.BYA_NO_PL_1_2_9,
              BYAFILE.BYA_AMT_PL_1_2_9,
              BYAFILE.BYA_O2_PL_1_2_9,
              BYAFILE.BYA_INT_PL_1_2_9,
              BYAFILE.BYA_NO_PL_1_2_10,
              BYAFILE.BYA_AMT_PL_1_2_10,
              BYAFILE.BYA_O2_PL_1_2_10,
              BYAFILE.BYA_INT_PL_1_2_10,
              BYAFILE.BYA_NO_PL_2_1_1,
              BYAFILE.BYA_AMT_PL_2_1_1,
              BYAFILE.BYA_O2_PL_2_1_1,
              BYAFILE.BYA_INT_PL_2_1_1,
              BYAFILE.BYA_NO_PL_2_1_2,
              BYAFILE.BYA_AMT_PL_2_1_2,
              BYAFILE.BYA_O2_PL_2_1_2,
              BYAFILE.BYA_INT_PL_2_1_2,
              BYAFILE.BYA_NO_PL_2_1_3,
              BYAFILE.BYA_AMT_PL_2_1_3,
              BYAFILE.BYA_O2_PL_2_1_3,
              BYAFILE.BYA_INT_PL_2_1_3,
              BYAFILE.BYA_NO_PL_2_1_4,
              BYAFILE.BYA_AMT_PL_2_1_4,
              BYAFILE.BYA_O2_PL_2_1_4,
              BYAFILE.BYA_INT_PL_2_1_4,
              BYAFILE.BYA_NO_PL_2_1_5,
              BYAFILE.BYA_AMT_PL_2_1_5,
              BYAFILE.BYA_O2_PL_2_1_5,
              BYAFILE.BYA_INT_PL_2_1_5,
              BYAFILE.BYA_NO_PL_2_1_6,
              BYAFILE.BYA_AMT_PL_2_1_6,
              BYAFILE.BYA_O2_PL_2_1_6,
              BYAFILE.BYA_INT_PL_2_1_6,
              BYAFILE.BYA_NO_PL_2_1_7,
              BYAFILE.BYA_AMT_PL_2_1_7,
              BYAFILE.BYA_O2_PL_2_1_7,
              BYAFILE.BYA_INT_PL_2_1_7,
              BYAFILE.BYA_NO_PL_2_1_8,
              BYAFILE.BYA_AMT_PL_2_1_8,
              BYAFILE.BYA_O2_PL_2_1_8,
              BYAFILE.BYA_INT_PL_2_1_8,
              BYAFILE.BYA_NO_PL_2_1_9,
              BYAFILE.BYA_AMT_PL_2_1_9,
              BYAFILE.BYA_O2_PL_2_1_9,
              BYAFILE.BYA_INT_PL_2_1_9,
              BYAFILE.BYA_NO_PL_2_1_10,
              BYAFILE.BYA_AMT_PL_2_1_10,
              BYAFILE.BYA_O2_PL_2_1_10,
              BYAFILE.BYA_INT_PL_2_1_10,
              BYAFILE.BYA_NO_PL_2_2_1,
              BYAFILE.BYA_AMT_PL_2_2_1,
              BYAFILE.BYA_O2_PL_2_2_1,
              BYAFILE.BYA_INT_PL_2_2_1,
              BYAFILE.BYA_NO_PL_2_2_2,
              BYAFILE.BYA_AMT_PL_2_2_2,
              BYAFILE.BYA_O2_PL_2_2_2,
              BYAFILE.BYA_INT_PL_2_2_2,
              BYAFILE.BYA_NO_PL_2_2_3,
              BYAFILE.BYA_AMT_PL_2_2_3,
              BYAFILE.BYA_O2_PL_2_2_3,
              BYAFILE.BYA_INT_PL_2_2_3,
              BYAFILE.BYA_NO_PL_2_2_4,
              BYAFILE.BYA_AMT_PL_2_2_4,
              BYAFILE.BYA_O2_PL_2_2_4,
              BYAFILE.BYA_INT_PL_2_2_4,
              BYAFILE.BYA_NO_PL_2_2_5,
              BYAFILE.BYA_AMT_PL_2_2_5,
              BYAFILE.BYA_O2_PL_2_2_5,
              BYAFILE.BYA_INT_PL_2_2_5,
              BYAFILE.BYA_NO_PL_2_2_6,
              BYAFILE.BYA_AMT_PL_2_2_6,
              BYAFILE.BYA_O2_PL_2_2_6,
              BYAFILE.BYA_INT_PL_2_2_6,
              BYAFILE.BYA_NO_PL_2_2_7,
              BYAFILE.BYA_AMT_PL_2_2_7,
              BYAFILE.BYA_O2_PL_2_2_7,
              BYAFILE.BYA_INT_PL_2_2_7,
              BYAFILE.BYA_NO_PL_2_2_8,
              BYAFILE.BYA_AMT_PL_2_2_8,
              BYAFILE.BYA_O2_PL_2_2_8,
              BYAFILE.BYA_INT_PL_2_2_8,
              BYAFILE.BYA_NO_PL_2_2_9,
              BYAFILE.BYA_AMT_PL_2_2_9,
              BYAFILE.BYA_O2_PL_2_2_9,
              BYAFILE.BYA_INT_PL_2_2_9,
              BYAFILE.BYA_NO_PL_2_2_10,
              BYAFILE.BYA_AMT_PL_2_2_10,
              BYAFILE.BYA_O2_PL_2_2_10,
              BYAFILE.BYA_INT_PL_2_2_10,
              BYAFILE.BYA_NO_PL_3_1_1,
              BYAFILE.BYA_AMT_PL_3_1_1,
              BYAFILE.BYA_O2_PL_3_1_1,
              BYAFILE.BYA_INT_PL_3_1_1,
              BYAFILE.BYA_NO_PL_3_1_2,
              BYAFILE.BYA_AMT_PL_3_1_2,
              BYAFILE.BYA_O2_PL_3_1_2,
              BYAFILE.BYA_INT_PL_3_1_2,
              BYAFILE.BYA_NO_PL_3_1_3,
              BYAFILE.BYA_AMT_PL_3_1_3,
              BYAFILE.BYA_O2_PL_3_1_3,
              BYAFILE.BYA_INT_PL_3_1_3,
              BYAFILE.BYA_NO_PL_3_1_4,
              BYAFILE.BYA_AMT_PL_3_1_4,
              BYAFILE.BYA_O2_PL_3_1_4,
              BYAFILE.BYA_INT_PL_3_1_4,
              BYAFILE.BYA_NO_PL_3_1_5,
              BYAFILE.BYA_AMT_PL_3_1_5,
              BYAFILE.BYA_O2_PL_3_1_5,
              BYAFILE.BYA_INT_PL_3_1_5,
              BYAFILE.BYA_NO_PL_3_1_6,
              BYAFILE.BYA_AMT_PL_3_1_6,
              BYAFILE.BYA_O2_PL_3_1_6,
              BYAFILE.BYA_INT_PL_3_1_6,
              BYAFILE.BYA_NO_PL_3_1_7,
              BYAFILE.BYA_AMT_PL_3_1_7,
              BYAFILE.BYA_O2_PL_3_1_7,
              BYAFILE.BYA_INT_PL_3_1_7,
              BYAFILE.BYA_NO_PL_3_1_8,
              BYAFILE.BYA_AMT_PL_3_1_8,
              BYAFILE.BYA_O2_PL_3_1_8,
              BYAFILE.BYA_INT_PL_3_1_8,
              BYAFILE.BYA_NO_PL_3_1_9,
              BYAFILE.BYA_AMT_PL_3_1_9,
              BYAFILE.BYA_O2_PL_3_1_9,
              BYAFILE.BYA_INT_PL_3_1_9,
              BYAFILE.BYA_NO_PL_3_1_10,
              BYAFILE.BYA_AMT_PL_3_1_10,
              BYAFILE.BYA_O2_PL_3_1_10,
              BYAFILE.BYA_INT_PL_3_1_10,
              BYAFILE.BYA_NO_PL_3_2_1,
              BYAFILE.BYA_AMT_PL_3_2_1,
              BYAFILE.BYA_O2_PL_3_2_1,
              BYAFILE.BYA_INT_PL_3_2_1,
              BYAFILE.BYA_NO_PL_3_2_2,
              BYAFILE.BYA_AMT_PL_3_2_2,
              BYAFILE.BYA_O2_PL_3_2_2,
              BYAFILE.BYA_INT_PL_3_2_2,
              BYAFILE.BYA_NO_PL_3_2_3,
              BYAFILE.BYA_AMT_PL_3_2_3,
              BYAFILE.BYA_O2_PL_3_2_3,
              BYAFILE.BYA_INT_PL_3_2_3,
              BYAFILE.BYA_NO_PL_3_2_4,
              BYAFILE.BYA_AMT_PL_3_2_4,
              BYAFILE.BYA_O2_PL_3_2_4,
              BYAFILE.BYA_INT_PL_3_2_4,
              BYAFILE.BYA_NO_PL_3_2_5,
              BYAFILE.BYA_AMT_PL_3_2_5,
              BYAFILE.BYA_O2_PL_3_2_5,
              BYAFILE.BYA_INT_PL_3_2_5,
              BYAFILE.BYA_NO_PL_3_2_6,
              BYAFILE.BYA_AMT_PL_3_2_6,
              BYAFILE.BYA_O2_PL_3_2_6,
              BYAFILE.BYA_INT_PL_3_2_6,
              BYAFILE.BYA_NO_PL_3_2_7,
              BYAFILE.BYA_AMT_PL_3_2_7,
              BYAFILE.BYA_O2_PL_3_2_7,
              BYAFILE.BYA_INT_PL_3_2_7,
              BYAFILE.BYA_NO_PL_3_2_8,
              BYAFILE.BYA_AMT_PL_3_2_8,
              BYAFILE.BYA_O2_PL_3_2_8,
              BYAFILE.BYA_INT_PL_3_2_8,
              BYAFILE.BYA_NO_PL_3_2_9,
              BYAFILE.BYA_AMT_PL_3_2_9,
              BYAFILE.BYA_O2_PL_3_2_9,
              BYAFILE.BYA_INT_PL_3_2_9,
              BYAFILE.BYA_NO_PL_3_2_10,
              BYAFILE.BYA_AMT_PL_3_2_10,
              BYAFILE.BYA_O2_PL_3_2_10,
              BYAFILE.BYA_INT_PL_3_2_10,
              BYAFILE.BYA_DLRVPRIN_1_1_1,
              BYAFILE.BYA_DLRVPRIN_1_1_2,
              BYAFILE.BYA_DLRVPRIN_1_1_3,
              BYAFILE.BYA_DLRVCHGS_1_1_1,
              BYAFILE.BYA_DLRVCHGS_1_1_2,
              BYAFILE.BYA_DLRVCHGS_1_1_3,
              BYAFILE.BYA_DLPARVHELD_1_1,
              BYAFILE.BYA_DLRVPRIN_1_2_1,
              BYAFILE.BYA_DLRVPRIN_1_2_2,
              BYAFILE.BYA_DLRVPRIN_1_2_3,
              BYAFILE.BYA_DLRVCHGS_1_2_1,
              BYAFILE.BYA_DLRVCHGS_1_2_2,
              BYAFILE.BYA_DLRVCHGS_1_2_3,
              BYAFILE.BYA_DLPARVHELD_1_2,
              BYAFILE.BYA_DLRVPRIN_2_1_1,
              BYAFILE.BYA_DLRVPRIN_2_1_2,
              BYAFILE.BYA_DLRVPRIN_2_1_3,
              BYAFILE.BYA_DLRVCHGS_2_1_1,
              BYAFILE.BYA_DLRVCHGS_2_1_2,
              BYAFILE.BYA_DLRVCHGS_2_1_3,
              BYAFILE.BYA_DLPARVHELD_2_1,
              BYAFILE.BYA_DLRVPRIN_2_2_1,
              BYAFILE.BYA_DLRVPRIN_2_2_2,
              BYAFILE.BYA_DLRVPRIN_2_2_3,
              BYAFILE.BYA_DLRVCHGS_2_2_1,
              BYAFILE.BYA_DLRVCHGS_2_2_2,
              BYAFILE.BYA_DLRVCHGS_2_2_3,
              BYAFILE.BYA_DLPARVHELD_2_2,
              BYAFILE.BYA_DLRVPRIN_3_1_1,
              BYAFILE.BYA_DLRVPRIN_3_1_2,
              BYAFILE.BYA_DLRVPRIN_3_1_3,
              BYAFILE.BYA_DLRVCHGS_3_1_1,
              BYAFILE.BYA_DLRVCHGS_3_1_2,
              BYAFILE.BYA_DLRVCHGS_3_1_3,
              BYAFILE.BYA_DLPARVHELD_3_1,
              BYAFILE.BYA_DLRVPRIN_3_2_1,
              BYAFILE.BYA_DLRVPRIN_3_2_2,
              BYAFILE.BYA_DLRVPRIN_3_2_3,
              BYAFILE.BYA_DLRVCHGS_3_2_1,
              BYAFILE.BYA_DLRVCHGS_3_2_2,
              BYAFILE.BYA_DLRVCHGS_3_2_3,
              BYAFILE.BYA_DLPARVHELD_3_2)
            VALUES
             (:QBYA-DATE-CCYYMM,
              :QBYA-BRNO,
              :QBYA-CLASS,
              :QBYA-LTOUCH-DATE,
              :QBYA-NO-1-1-1,
              :QBYA-AMT-1-1-1,
              :QBYA-O2-1-1-1,
              :QBYA-INT-1-1-1,
              :QBYA-NO-1-1-2,
              :QBYA-AMT-1-1-2,
              :QBYA-O2-1-1-2,
              :QBYA-INT-1-1-2,
              :QBYA-NO-1-1-3,
              :QBYA-AMT-1-1-3,
              :QBYA-O2-1-1-3,
              :QBYA-INT-1-1-3,
              :QBYA-NO-1-1-4,
              :QBYA-AMT-1-1-4,
              :QBYA-O2-1-1-4,
              :QBYA-INT-1-1-4,
              :QBYA-NO-1-1-5,
              :QBYA-AMT-1-1-5,
              :QBYA-O2-1-1-5,
              :QBYA-INT-1-1-5,
              :QBYA-NO-1-1-6,
              :QBYA-AMT-1-1-6,
              :QBYA-O2-1-1-6,
              :QBYA-INT-1-1-6,
              :QBYA-NO-1-1-7,
              :QBYA-AMT-1-1-7,
              :QBYA-O2-1-1-7,
              :QBYA-INT-1-1-7,
              :QBYA-NO-1-1-8,
              :QBYA-AMT-1-1-8,
              :QBYA-O2-1-1-8,
              :QBYA-INT-1-1-8,
              :QBYA-NO-1-1-9,
              :QBYA-AMT-1-1-9,
              :QBYA-O2-1-1-9,
              :QBYA-INT-1-1-9,
              :QBYA-NO-1-1-10,
              :QBYA-AMT-1-1-10,
              :QBYA-O2-1-1-10,
              :QBYA-INT-1-1-10,
              :QBYA-NO-1-1-11,
              :QBYA-AMT-1-1-11,
              :QBYA-O2-1-1-11,
              :QBYA-INT-1-1-11,
              :QBYA-NO-1-2-1,
              :QBYA-AMT-1-2-1,
              :QBYA-O2-1-2-1,
              :QBYA-INT-1-2-1,
              :QBYA-NO-1-2-2,
              :QBYA-AMT-1-2-2,
              :QBYA-O2-1-2-2,
              :QBYA-INT-1-2-2,
              :QBYA-NO-1-2-3,
              :QBYA-AMT-1-2-3,
              :QBYA-O2-1-2-3,
              :QBYA-INT-1-2-3,
              :QBYA-NO-1-2-4,
              :QBYA-AMT-1-2-4,
              :QBYA-O2-1-2-4,
              :QBYA-INT-1-2-4,
              :QBYA-NO-1-2-5,
              :QBYA-AMT-1-2-5,
              :QBYA-O2-1-2-5,
              :QBYA-INT-1-2-5,
              :QBYA-NO-1-2-6,
              :QBYA-AMT-1-2-6,
              :QBYA-O2-1-2-6,
              :QBYA-INT-1-2-6,
              :QBYA-NO-1-2-7,
              :QBYA-AMT-1-2-7,
              :QBYA-O2-1-2-7,
              :QBYA-INT-1-2-7,
              :QBYA-NO-1-2-8,
              :QBYA-AMT-1-2-8,
              :QBYA-O2-1-2-8,
              :QBYA-INT-1-2-8,
              :QBYA-NO-1-2-9,
              :QBYA-AMT-1-2-9,
              :QBYA-O2-1-2-9,
              :QBYA-INT-1-2-9,
              :QBYA-NO-1-2-10,
              :QBYA-AMT-1-2-10,
              :QBYA-O2-1-2-10,
              :QBYA-INT-1-2-10,
              :QBYA-NO-1-2-11,
              :QBYA-AMT-1-2-11,
              :QBYA-O2-1-2-11,
              :QBYA-INT-1-2-11,
              :QBYA-NO-2-1-1,
              :QBYA-AMT-2-1-1,
              :QBYA-O2-2-1-1,
              :QBYA-INT-2-1-1,
              :QBYA-NO-2-1-2,
              :QBYA-AMT-2-1-2,
              :QBYA-O2-2-1-2,
              :QBYA-INT-2-1-2,
              :QBYA-NO-2-1-3,
              :QBYA-AMT-2-1-3,
              :QBYA-O2-2-1-3,
              :QBYA-INT-2-1-3,
              :QBYA-NO-2-1-4,
              :QBYA-AMT-2-1-4,
              :QBYA-O2-2-1-4,
              :QBYA-INT-2-1-4,
              :QBYA-NO-2-1-5,
              :QBYA-AMT-2-1-5,
              :QBYA-O2-2-1-5,
              :QBYA-INT-2-1-5,
              :QBYA-NO-2-1-6,
              :QBYA-AMT-2-1-6,
              :QBYA-O2-2-1-6,
              :QBYA-INT-2-1-6,
              :QBYA-NO-2-1-7,
              :QBYA-AMT-2-1-7,
              :QBYA-O2-2-1-7,
              :QBYA-INT-2-1-7,
              :QBYA-NO-2-1-8,
              :QBYA-AMT-2-1-8,
              :QBYA-O2-2-1-8,
              :QBYA-INT-2-1-8,
              :QBYA-NO-2-1-9,
              :QBYA-AMT-2-1-9,
              :QBYA-O2-2-1-9,
              :QBYA-INT-2-1-9,
              :QBYA-NO-2-1-10,
              :QBYA-AMT-2-1-10,
              :QBYA-O2-2-1-10,
              :QBYA-INT-2-1-10,
              :QBYA-NO-2-1-11,
              :QBYA-AMT-2-1-11,
              :QBYA-O2-2-1-11,
              :QBYA-INT-2-1-11,
              :QBYA-NO-2-2-1,
              :QBYA-AMT-2-2-1,
              :QBYA-O2-2-2-1,
              :QBYA-INT-2-2-1,
              :QBYA-NO-2-2-2,
              :QBYA-AMT-2-2-2,
              :QBYA-O2-2-2-2,
              :QBYA-INT-2-2-2,
              :QBYA-NO-2-2-3,
              :QBYA-AMT-2-2-3,
              :QBYA-O2-2-2-3,
              :QBYA-INT-2-2-3,
              :QBYA-NO-2-2-4,
              :QBYA-AMT-2-2-4,
              :QBYA-O2-2-2-4,
              :QBYA-INT-2-2-4,
              :QBYA-NO-2-2-5,
              :QBYA-AMT-2-2-5,
              :QBYA-O2-2-2-5,
              :QBYA-INT-2-2-5,
              :QBYA-NO-2-2-6,
              :QBYA-AMT-2-2-6,
              :QBYA-O2-2-2-6,
              :QBYA-INT-2-2-6,
              :QBYA-NO-2-2-7,
              :QBYA-AMT-2-2-7,
              :QBYA-O2-2-2-7,
              :QBYA-INT-2-2-7,
              :QBYA-NO-2-2-8,
              :QBYA-AMT-2-2-8,
              :QBYA-O2-2-2-8,
              :QBYA-INT-2-2-8,
              :QBYA-NO-2-2-9,
              :QBYA-AMT-2-2-9,
              :QBYA-O2-2-2-9,
              :QBYA-INT-2-2-9,
              :QBYA-NO-2-2-10,
              :QBYA-AMT-2-2-10,
              :QBYA-O2-2-2-10,
              :QBYA-INT-2-2-10,
              :QBYA-NO-2-2-11,
              :QBYA-AMT-2-2-11,
              :QBYA-O2-2-2-11,
              :QBYA-INT-2-2-11,
              :QBYA-NO-3-1-1,
              :QBYA-AMT-3-1-1,
              :QBYA-O2-3-1-1,
              :QBYA-INT-3-1-1,
              :QBYA-NO-3-1-2,
              :QBYA-AMT-3-1-2,
              :QBYA-O2-3-1-2,
              :QBYA-INT-3-1-2,
              :QBYA-NO-3-1-3,
              :QBYA-AMT-3-1-3,
              :QBYA-O2-3-1-3,
              :QBYA-INT-3-1-3,
              :QBYA-NO-3-1-4,
              :QBYA-AMT-3-1-4,
              :QBYA-O2-3-1-4,
              :QBYA-INT-3-1-4,
              :QBYA-NO-3-1-5,
              :QBYA-AMT-3-1-5,
              :QBYA-O2-3-1-5,
              :QBYA-INT-3-1-5,
              :QBYA-NO-3-1-6,
              :QBYA-AMT-3-1-6,
              :QBYA-O2-3-1-6,
              :QBYA-INT-3-1-6,
              :QBYA-NO-3-1-7,
              :QBYA-AMT-3-1-7,
              :QBYA-O2-3-1-7,
              :QBYA-INT-3-1-7,
              :QBYA-NO-3-1-8,
              :QBYA-AMT-3-1-8,
              :QBYA-O2-3-1-8,
              :QBYA-INT-3-1-8,
              :QBYA-NO-3-1-9,
              :QBYA-AMT-3-1-9,
              :QBYA-O2-3-1-9,
              :QBYA-INT-3-1-9,
              :QBYA-NO-3-1-10,
              :QBYA-AMT-3-1-10,
              :QBYA-O2-3-1-10,
              :QBYA-INT-3-1-10,
              :QBYA-NO-3-1-11,
              :QBYA-AMT-3-1-11,
              :QBYA-O2-3-1-11,
              :QBYA-INT-3-1-11,
              :QBYA-NO-3-2-1,
              :QBYA-AMT-3-2-1,
              :QBYA-O2-3-2-1,
              :QBYA-INT-3-2-1,
              :QBYA-NO-3-2-2,
              :QBYA-AMT-3-2-2,
              :QBYA-O2-3-2-2,
              :QBYA-INT-3-2-2,
              :QBYA-NO-3-2-3,
              :QBYA-AMT-3-2-3,
              :QBYA-O2-3-2-3,
              :QBYA-INT-3-2-3,
              :QBYA-NO-3-2-4,
              :QBYA-AMT-3-2-4,
              :QBYA-O2-3-2-4,
              :QBYA-INT-3-2-4,
              :QBYA-NO-3-2-5,
              :QBYA-AMT-3-2-5,
              :QBYA-O2-3-2-5,
              :QBYA-INT-3-2-5,
              :QBYA-NO-3-2-6,
              :QBYA-AMT-3-2-6,
              :QBYA-O2-3-2-6,
              :QBYA-INT-3-2-6,
              :QBYA-NO-3-2-7,
              :QBYA-AMT-3-2-7,
              :QBYA-O2-3-2-7,
              :QBYA-INT-3-2-7,
              :QBYA-NO-3-2-8,
              :QBYA-AMT-3-2-8,
              :QBYA-O2-3-2-8,
              :QBYA-INT-3-2-8,
              :QBYA-NO-3-2-9,
              :QBYA-AMT-3-2-9,
              :QBYA-O2-3-2-9,
              :QBYA-INT-3-2-9,
              :QBYA-NO-3-2-10,
              :QBYA-AMT-3-2-10,
              :QBYA-O2-3-2-10,
              :QBYA-INT-3-2-10,
              :QBYA-NO-3-2-11,
              :QBYA-AMT-3-2-11,
              :QBYA-O2-3-2-11,
              :QBYA-INT-3-2-11,
              :QBYA-NO-REPO-1-1-1,
              :QBYA-AMT-REPO-1-1-1,
              :QBYA-O2-REPO-1-1-1,
              :QBYA-INT-REPO-1-1-1,
              :QBYA-NO-REPO-1-1-2,
              :QBYA-AMT-REPO-1-1-2,
              :QBYA-O2-REPO-1-1-2,
              :QBYA-INT-REPO-1-1-2,
              :QBYA-NO-REPO-1-1-3,
              :QBYA-AMT-REPO-1-1-3,
              :QBYA-O2-REPO-1-1-3,
              :QBYA-INT-REPO-1-1-3,
              :QBYA-NO-REPO-1-1-4,
              :QBYA-AMT-REPO-1-1-4,
              :QBYA-O2-REPO-1-1-4,
              :QBYA-INT-REPO-1-1-4,
              :QBYA-NO-REPO-1-1-5,
              :QBYA-AMT-REPO-1-1-5,
              :QBYA-O2-REPO-1-1-5,
              :QBYA-INT-REPO-1-1-5,
              :QBYA-NO-REPO-1-2-1,
              :QBYA-AMT-REPO-1-2-1,
              :QBYA-O2-REPO-1-2-1,
              :QBYA-INT-REPO-1-2-1,
              :QBYA-NO-REPO-1-2-2,
              :QBYA-AMT-REPO-1-2-2,
              :QBYA-O2-REPO-1-2-2,
              :QBYA-INT-REPO-1-2-2,
              :QBYA-NO-REPO-1-2-3,
              :QBYA-AMT-REPO-1-2-3,
              :QBYA-O2-REPO-1-2-3,
              :QBYA-INT-REPO-1-2-3,
              :QBYA-NO-REPO-1-2-4,
              :QBYA-AMT-REPO-1-2-4,
              :QBYA-O2-REPO-1-2-4,
              :QBYA-INT-REPO-1-2-4,
              :QBYA-NO-REPO-1-2-5,
              :QBYA-AMT-REPO-1-2-5,
              :QBYA-O2-REPO-1-2-5,
              :QBYA-INT-REPO-1-2-5,
              :QBYA-NO-REPO-2-1-1,
              :QBYA-AMT-REPO-2-1-1,
              :QBYA-O2-REPO-2-1-1,
              :QBYA-INT-REPO-2-1-1,
              :QBYA-NO-REPO-2-1-2,
              :QBYA-AMT-REPO-2-1-2,
              :QBYA-O2-REPO-2-1-2,
              :QBYA-INT-REPO-2-1-2,
              :QBYA-NO-REPO-2-1-3,
              :QBYA-AMT-REPO-2-1-3,
              :QBYA-O2-REPO-2-1-3,
              :QBYA-INT-REPO-2-1-3,
              :QBYA-NO-REPO-2-1-4,
              :QBYA-AMT-REPO-2-1-4,
              :QBYA-O2-REPO-2-1-4,
              :QBYA-INT-REPO-2-1-4,
              :QBYA-NO-REPO-2-1-5,
              :QBYA-AMT-REPO-2-1-5,
              :QBYA-O2-REPO-2-1-5,
              :QBYA-INT-REPO-2-1-5,
              :QBYA-NO-REPO-2-2-1,
              :QBYA-AMT-REPO-2-2-1,
              :QBYA-O2-REPO-2-2-1,
              :QBYA-INT-REPO-2-2-1,
              :QBYA-NO-REPO-2-2-2,
              :QBYA-AMT-REPO-2-2-2,
              :QBYA-O2-REPO-2-2-2,
              :QBYA-INT-REPO-2-2-2,
              :QBYA-NO-REPO-2-2-3,
              :QBYA-AMT-REPO-2-2-3,
              :QBYA-O2-REPO-2-2-3,
              :QBYA-INT-REPO-2-2-3,
              :QBYA-NO-REPO-2-2-4,
              :QBYA-AMT-REPO-2-2-4,
              :QBYA-O2-REPO-2-2-4,
              :QBYA-INT-REPO-2-2-4,
              :QBYA-NO-REPO-2-2-5,
              :QBYA-AMT-REPO-2-2-5,
              :QBYA-O2-REPO-2-2-5,
              :QBYA-INT-REPO-2-2-5,
              :QBYA-NO-REPO-3-1-1,
              :QBYA-AMT-REPO-3-1-1,
              :QBYA-O2-REPO-3-1-1,
              :QBYA-INT-REPO-3-1-1,
              :QBYA-NO-REPO-3-1-2,
              :QBYA-AMT-REPO-3-1-2,
              :QBYA-O2-REPO-3-1-2,
              :QBYA-INT-REPO-3-1-2,
              :QBYA-NO-REPO-3-1-3,
              :QBYA-AMT-REPO-3-1-3,
              :QBYA-O2-REPO-3-1-3,
              :QBYA-INT-REPO-3-1-3,
              :QBYA-NO-REPO-3-1-4,
              :QBYA-AMT-REPO-3-1-4,
              :QBYA-O2-REPO-3-1-4,
              :QBYA-INT-REPO-3-1-4,
              :QBYA-NO-REPO-3-1-5,
              :QBYA-AMT-REPO-3-1-5,
              :QBYA-O2-REPO-3-1-5,
              :QBYA-INT-REPO-3-1-5,
              :QBYA-NO-REPO-3-2-1,
              :QBYA-AMT-REPO-3-2-1,
              :QBYA-O2-REPO-3-2-1,
              :QBYA-INT-REPO-3-2-1,
              :QBYA-NO-REPO-3-2-2,
              :QBYA-AMT-REPO-3-2-2,
              :QBYA-O2-REPO-3-2-2,
              :QBYA-INT-REPO-3-2-2,
              :QBYA-NO-REPO-3-2-3,
              :QBYA-AMT-REPO-3-2-3,
              :QBYA-O2-REPO-3-2-3,
              :QBYA-INT-REPO-3-2-3,
              :QBYA-NO-REPO-3-2-4,
              :QBYA-AMT-REPO-3-2-4,
              :QBYA-O2-REPO-3-2-4,
              :QBYA-INT-REPO-3-2-4,
              :QBYA-NO-REPO-3-2-5,
              :QBYA-AMT-REPO-3-2-5,
              :QBYA-O2-REPO-3-2-5,
              :QBYA-INT-REPO-3-2-5,
              :QBYA-NO-PL-1-1-1,
              :QBYA-AMT-PL-1-1-1,
              :QBYA-O2-PL-1-1-1,
              :QBYA-INT-PL-1-1-1,
              :QBYA-NO-PL-1-1-2,
              :QBYA-AMT-PL-1-1-2,
              :QBYA-O2-PL-1-1-2,
              :QBYA-INT-PL-1-1-2,
              :QBYA-NO-PL-1-1-3,
              :QBYA-AMT-PL-1-1-3,
              :QBYA-O2-PL-1-1-3,
              :QBYA-INT-PL-1-1-3,
              :QBYA-NO-PL-1-1-4,
              :QBYA-AMT-PL-1-1-4,
              :QBYA-O2-PL-1-1-4,
              :QBYA-INT-PL-1-1-4,
              :QBYA-NO-PL-1-1-5,
              :QBYA-AMT-PL-1-1-5,
              :QBYA-O2-PL-1-1-5,
              :QBYA-INT-PL-1-1-5,
              :QBYA-NO-PL-1-1-6,
              :QBYA-AMT-PL-1-1-6,
              :QBYA-O2-PL-1-1-6,
              :QBYA-INT-PL-1-1-6,
              :QBYA-NO-PL-1-1-7,
              :QBYA-AMT-PL-1-1-7,
              :QBYA-O2-PL-1-1-7,
              :QBYA-INT-PL-1-1-7,
              :QBYA-NO-PL-1-1-8,
              :QBYA-AMT-PL-1-1-8,
              :QBYA-O2-PL-1-1-8,
              :QBYA-INT-PL-1-1-8,
              :QBYA-NO-PL-1-1-9,
              :QBYA-AMT-PL-1-1-9,
              :QBYA-O2-PL-1-1-9,
              :QBYA-INT-PL-1-1-9,
              :QBYA-NO-PL-1-1-10,
              :QBYA-AMT-PL-1-1-10,
              :QBYA-O2-PL-1-1-10,
              :QBYA-INT-PL-1-1-10,
              :QBYA-NO-PL-1-2-1,
              :QBYA-AMT-PL-1-2-1,
              :QBYA-O2-PL-1-2-1,
              :QBYA-INT-PL-1-2-1,
              :QBYA-NO-PL-1-2-2,
              :QBYA-AMT-PL-1-2-2,
              :QBYA-O2-PL-1-2-2,
              :QBYA-INT-PL-1-2-2,
              :QBYA-NO-PL-1-2-3,
              :QBYA-AMT-PL-1-2-3,
              :QBYA-O2-PL-1-2-3,
              :QBYA-INT-PL-1-2-3,
              :QBYA-NO-PL-1-2-4,
              :QBYA-AMT-PL-1-2-4,
              :QBYA-O2-PL-1-2-4,
              :QBYA-INT-PL-1-2-4,
              :QBYA-NO-PL-1-2-5,
              :QBYA-AMT-PL-1-2-5,
              :QBYA-O2-PL-1-2-5,
              :QBYA-INT-PL-1-2-5,
              :QBYA-NO-PL-1-2-6,
              :QBYA-AMT-PL-1-2-6,
              :QBYA-O2-PL-1-2-6,
              :QBYA-INT-PL-1-2-6,
              :QBYA-NO-PL-1-2-7,
              :QBYA-AMT-PL-1-2-7,
              :QBYA-O2-PL-1-2-7,
              :QBYA-INT-PL-1-2-7,
              :QBYA-NO-PL-1-2-8,
              :QBYA-AMT-PL-1-2-8,
              :QBYA-O2-PL-1-2-8,
              :QBYA-INT-PL-1-2-8,
              :QBYA-NO-PL-1-2-9,
              :QBYA-AMT-PL-1-2-9,
              :QBYA-O2-PL-1-2-9,
              :QBYA-INT-PL-1-2-9,
              :QBYA-NO-PL-1-2-10,
              :QBYA-AMT-PL-1-2-10,
              :QBYA-O2-PL-1-2-10,
              :QBYA-INT-PL-1-2-10,
              :QBYA-NO-PL-2-1-1,
              :QBYA-AMT-PL-2-1-1,
              :QBYA-O2-PL-2-1-1,
              :QBYA-INT-PL-2-1-1,
              :QBYA-NO-PL-2-1-2,
              :QBYA-AMT-PL-2-1-2,
              :QBYA-O2-PL-2-1-2,
              :QBYA-INT-PL-2-1-2,
              :QBYA-NO-PL-2-1-3,
              :QBYA-AMT-PL-2-1-3,
              :QBYA-O2-PL-2-1-3,
              :QBYA-INT-PL-2-1-3,
              :QBYA-NO-PL-2-1-4,
              :QBYA-AMT-PL-2-1-4,
              :QBYA-O2-PL-2-1-4,
              :QBYA-INT-PL-2-1-4,
              :QBYA-NO-PL-2-1-5,
              :QBYA-AMT-PL-2-1-5,
              :QBYA-O2-PL-2-1-5,
              :QBYA-INT-PL-2-1-5,
              :QBYA-NO-PL-2-1-6,
              :QBYA-AMT-PL-2-1-6,
              :QBYA-O2-PL-2-1-6,
              :QBYA-INT-PL-2-1-6,
              :QBYA-NO-PL-2-1-7,
              :QBYA-AMT-PL-2-1-7,
              :QBYA-O2-PL-2-1-7,
              :QBYA-INT-PL-2-1-7,
              :QBYA-NO-PL-2-1-8,
              :QBYA-AMT-PL-2-1-8,
              :QBYA-O2-PL-2-1-8,
              :QBYA-INT-PL-2-1-8,
              :QBYA-NO-PL-2-1-9,
              :QBYA-AMT-PL-2-1-9,
              :QBYA-O2-PL-2-1-9,
              :QBYA-INT-PL-2-1-9,
              :QBYA-NO-PL-2-1-10,
              :QBYA-AMT-PL-2-1-10,
              :QBYA-O2-PL-2-1-10,
              :QBYA-INT-PL-2-1-10,
              :QBYA-NO-PL-2-2-1,
              :QBYA-AMT-PL-2-2-1,
              :QBYA-O2-PL-2-2-1,
              :QBYA-INT-PL-2-2-1,
              :QBYA-NO-PL-2-2-2,
              :QBYA-AMT-PL-2-2-2,
              :QBYA-O2-PL-2-2-2,
              :QBYA-INT-PL-2-2-2,
              :QBYA-NO-PL-2-2-3,
              :QBYA-AMT-PL-2-2-3,
              :QBYA-O2-PL-2-2-3,
              :QBYA-INT-PL-2-2-3,
              :QBYA-NO-PL-2-2-4,
              :QBYA-AMT-PL-2-2-4,
              :QBYA-O2-PL-2-2-4,
              :QBYA-INT-PL-2-2-4,
              :QBYA-NO-PL-2-2-5,
              :QBYA-AMT-PL-2-2-5,
              :QBYA-O2-PL-2-2-5,
              :QBYA-INT-PL-2-2-5,
              :QBYA-NO-PL-2-2-6,
              :QBYA-AMT-PL-2-2-6,
              :QBYA-O2-PL-2-2-6,
              :QBYA-INT-PL-2-2-6,
              :QBYA-NO-PL-2-2-7,
              :QBYA-AMT-PL-2-2-7,
              :QBYA-O2-PL-2-2-7,
              :QBYA-INT-PL-2-2-7,
              :QBYA-NO-PL-2-2-8,
              :QBYA-AMT-PL-2-2-8,
              :QBYA-O2-PL-2-2-8,
              :QBYA-INT-PL-2-2-8,
              :QBYA-NO-PL-2-2-9,
              :QBYA-AMT-PL-2-2-9,
              :QBYA-O2-PL-2-2-9,
              :QBYA-INT-PL-2-2-9,
              :QBYA-NO-PL-2-2-10,
              :QBYA-AMT-PL-2-2-10,
              :QBYA-O2-PL-2-2-10,
              :QBYA-INT-PL-2-2-10,
              :QBYA-NO-PL-3-1-1,
              :QBYA-AMT-PL-3-1-1,
              :QBYA-O2-PL-3-1-1,
              :QBYA-INT-PL-3-1-1,
              :QBYA-NO-PL-3-1-2,
              :QBYA-AMT-PL-3-1-2,
              :QBYA-O2-PL-3-1-2,
              :QBYA-INT-PL-3-1-2,
              :QBYA-NO-PL-3-1-3,
              :QBYA-AMT-PL-3-1-3,
              :QBYA-O2-PL-3-1-3,
              :QBYA-INT-PL-3-1-3,
              :QBYA-NO-PL-3-1-4,
              :QBYA-AMT-PL-3-1-4,
              :QBYA-O2-PL-3-1-4,
              :QBYA-INT-PL-3-1-4,
              :QBYA-NO-PL-3-1-5,
              :QBYA-AMT-PL-3-1-5,
              :QBYA-O2-PL-3-1-5,
              :QBYA-INT-PL-3-1-5,
              :QBYA-NO-PL-3-1-6,
              :QBYA-AMT-PL-3-1-6,
              :QBYA-O2-PL-3-1-6,
              :QBYA-INT-PL-3-1-6,
              :QBYA-NO-PL-3-1-7,
              :QBYA-AMT-PL-3-1-7,
              :QBYA-O2-PL-3-1-7,
              :QBYA-INT-PL-3-1-7,
              :QBYA-NO-PL-3-1-8,
              :QBYA-AMT-PL-3-1-8,
              :QBYA-O2-PL-3-1-8,
              :QBYA-INT-PL-3-1-8,
              :QBYA-NO-PL-3-1-9,
              :QBYA-AMT-PL-3-1-9,
              :QBYA-O2-PL-3-1-9,
              :QBYA-INT-PL-3-1-9,
              :QBYA-NO-PL-3-1-10,
              :QBYA-AMT-PL-3-1-10,
              :QBYA-O2-PL-3-1-10,
              :QBYA-INT-PL-3-1-10,
              :QBYA-NO-PL-3-2-1,
              :QBYA-AMT-PL-3-2-1,
              :QBYA-O2-PL-3-2-1,
              :QBYA-INT-PL-3-2-1,
              :QBYA-NO-PL-3-2-2,
              :QBYA-AMT-PL-3-2-2,
              :QBYA-O2-PL-3-2-2,
              :QBYA-INT-PL-3-2-2,
              :QBYA-NO-PL-3-2-3,
              :QBYA-AMT-PL-3-2-3,
              :QBYA-O2-PL-3-2-3,
              :QBYA-INT-PL-3-2-3,
              :QBYA-NO-PL-3-2-4,
              :QBYA-AMT-PL-3-2-4,
              :QBYA-O2-PL-3-2-4,
              :QBYA-INT-PL-3-2-4,
              :QBYA-NO-PL-3-2-5,
              :QBYA-AMT-PL-3-2-5,
              :QBYA-O2-PL-3-2-5,
              :QBYA-INT-PL-3-2-5,
              :QBYA-NO-PL-3-2-6,
              :QBYA-AMT-PL-3-2-6,
              :QBYA-O2-PL-3-2-6,
              :QBYA-INT-PL-3-2-6,
              :QBYA-NO-PL-3-2-7,
              :QBYA-AMT-PL-3-2-7,
              :QBYA-O2-PL-3-2-7,
              :QBYA-INT-PL-3-2-7,
              :QBYA-NO-PL-3-2-8,
              :QBYA-AMT-PL-3-2-8,
              :QBYA-O2-PL-3-2-8,
              :QBYA-INT-PL-3-2-8,
              :QBYA-NO-PL-3-2-9,
              :QBYA-AMT-PL-3-2-9,
              :QBYA-O2-PL-3-2-9,
              :QBYA-INT-PL-3-2-9,
              :QBYA-NO-PL-3-2-10,
              :QBYA-AMT-PL-3-2-10,
              :QBYA-O2-PL-3-2-10,
              :QBYA-INT-PL-3-2-10,
              :QBYA-DLRVPRIN-1-1-1,
              :QBYA-DLRVPRIN-1-1-2,
              :QBYA-DLRVPRIN-1-1-3,
              :QBYA-DLRVCHGS-1-1-1,
              :QBYA-DLRVCHGS-1-1-2,
              :QBYA-DLRVCHGS-1-1-3,
              :QBYA-DLPARVHELD-1-1,
              :QBYA-DLRVPRIN-1-2-1,
              :QBYA-DLRVPRIN-1-2-2,
              :QBYA-DLRVPRIN-1-2-3,
              :QBYA-DLRVCHGS-1-2-1,
              :QBYA-DLRVCHGS-1-2-2,
              :QBYA-DLRVCHGS-1-2-3,
              :QBYA-DLPARVHELD-1-2,
              :QBYA-DLRVPRIN-2-1-1,
              :QBYA-DLRVPRIN-2-1-2,
              :QBYA-DLRVPRIN-2-1-3,
              :QBYA-DLRVCHGS-2-1-1,
              :QBYA-DLRVCHGS-2-1-2,
              :QBYA-DLRVCHGS-2-1-3,
              :QBYA-DLPARVHELD-2-1,
              :QBYA-DLRVPRIN-2-2-1,
              :QBYA-DLRVPRIN-2-2-2,
              :QBYA-DLRVPRIN-2-2-3,
              :QBYA-DLRVCHGS-2-2-1,
              :QBYA-DLRVCHGS-2-2-2,
              :QBYA-DLRVCHGS-2-2-3,
              :QBYA-DLPARVHELD-2-2,
              :QBYA-DLRVPRIN-3-1-1,
              :QBYA-DLRVPRIN-3-1-2,
              :QBYA-DLRVPRIN-3-1-3,
              :QBYA-DLRVCHGS-3-1-1,
              :QBYA-DLRVCHGS-3-1-2,
              :QBYA-DLRVCHGS-3-1-3,
              :QBYA-DLPARVHELD-3-1,
              :QBYA-DLRVPRIN-3-2-1,
              :QBYA-DLRVPRIN-3-2-2,
              :QBYA-DLRVPRIN-3-2-3,
              :QBYA-DLRVCHGS-3-2-1,
              :QBYA-DLRVCHGS-3-2-2,
              :QBYA-DLRVCHGS-3-2-3,
              :QBYA-DLPARVHELD-3-2)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-BYA1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

           MOVE '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       REWRITE-BYA1-FILE.

           ACCEPT BYA-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE BYA-PATH-SQL TO E-FILE.
           MOVE BYA1-KEY TO E-KEYX.
           PERFORM SET-BYA-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            UPDATE DBO.BYAFILE
             SET
              BYAFILE.BYA_DATE_CCYYMM = :QBYA-DATE-CCYYMM,
              BYAFILE.BYA_BRNO = :QBYA-BRNO,
              BYAFILE.BYA_CLASS = :QBYA-CLASS,
              BYAFILE.BYA_LTOUCH_DATE = :QBYA-LTOUCH-DATE,
              BYAFILE.BYA_NO_1_1_1 = :QBYA-NO-1-1-1,
              BYAFILE.BYA_AMT_1_1_1 = :QBYA-AMT-1-1-1,
              BYAFILE.BYA_O2_1_1_1 = :QBYA-O2-1-1-1,
              BYAFILE.BYA_INT_1_1_1 = :QBYA-INT-1-1-1,
              BYAFILE.BYA_NO_1_1_2 = :QBYA-NO-1-1-2,
              BYAFILE.BYA_AMT_1_1_2 = :QBYA-AMT-1-1-2,
              BYAFILE.BYA_O2_1_1_2 = :QBYA-O2-1-1-2,
              BYAFILE.BYA_INT_1_1_2 = :QBYA-INT-1-1-2,
              BYAFILE.BYA_NO_1_1_3 = :QBYA-NO-1-1-3,
              BYAFILE.BYA_AMT_1_1_3 = :QBYA-AMT-1-1-3,
              BYAFILE.BYA_O2_1_1_3 = :QBYA-O2-1-1-3,
              BYAFILE.BYA_INT_1_1_3 = :QBYA-INT-1-1-3,
              BYAFILE.BYA_NO_1_1_4 = :QBYA-NO-1-1-4,
              BYAFILE.BYA_AMT_1_1_4 = :QBYA-AMT-1-1-4,
              BYAFILE.BYA_O2_1_1_4 = :QBYA-O2-1-1-4,
              BYAFILE.BYA_INT_1_1_4 = :QBYA-INT-1-1-4,
              BYAFILE.BYA_NO_1_1_5 = :QBYA-NO-1-1-5,
              BYAFILE.BYA_AMT_1_1_5 = :QBYA-AMT-1-1-5,
              BYAFILE.BYA_O2_1_1_5 = :QBYA-O2-1-1-5,
              BYAFILE.BYA_INT_1_1_5 = :QBYA-INT-1-1-5,
              BYAFILE.BYA_NO_1_1_6 = :QBYA-NO-1-1-6,
              BYAFILE.BYA_AMT_1_1_6 = :QBYA-AMT-1-1-6,
              BYAFILE.BYA_O2_1_1_6 = :QBYA-O2-1-1-6,
              BYAFILE.BYA_INT_1_1_6 = :QBYA-INT-1-1-6,
              BYAFILE.BYA_NO_1_1_7 = :QBYA-NO-1-1-7,
              BYAFILE.BYA_AMT_1_1_7 = :QBYA-AMT-1-1-7,
              BYAFILE.BYA_O2_1_1_7 = :QBYA-O2-1-1-7,
              BYAFILE.BYA_INT_1_1_7 = :QBYA-INT-1-1-7,
              BYAFILE.BYA_NO_1_1_8 = :QBYA-NO-1-1-8,
              BYAFILE.BYA_AMT_1_1_8 = :QBYA-AMT-1-1-8,
              BYAFILE.BYA_O2_1_1_8 = :QBYA-O2-1-1-8,
              BYAFILE.BYA_INT_1_1_8 = :QBYA-INT-1-1-8,
              BYAFILE.BYA_NO_1_1_9 = :QBYA-NO-1-1-9,
              BYAFILE.BYA_AMT_1_1_9 = :QBYA-AMT-1-1-9,
              BYAFILE.BYA_O2_1_1_9 = :QBYA-O2-1-1-9,
              BYAFILE.BYA_INT_1_1_9 = :QBYA-INT-1-1-9,
              BYAFILE.BYA_NO_1_1_10 = :QBYA-NO-1-1-10,
              BYAFILE.BYA_AMT_1_1_10 = :QBYA-AMT-1-1-10,
              BYAFILE.BYA_O2_1_1_10 = :QBYA-O2-1-1-10,
              BYAFILE.BYA_INT_1_1_10 = :QBYA-INT-1-1-10,
              BYAFILE.BYA_NO_1_1_11 = :QBYA-NO-1-1-11,
              BYAFILE.BYA_AMT_1_1_11 = :QBYA-AMT-1-1-11,
              BYAFILE.BYA_O2_1_1_11 = :QBYA-O2-1-1-11,
              BYAFILE.BYA_INT_1_1_11 = :QBYA-INT-1-1-11,
              BYAFILE.BYA_NO_1_2_1 = :QBYA-NO-1-2-1,
              BYAFILE.BYA_AMT_1_2_1 = :QBYA-AMT-1-2-1,
              BYAFILE.BYA_O2_1_2_1 = :QBYA-O2-1-2-1,
              BYAFILE.BYA_INT_1_2_1 = :QBYA-INT-1-2-1,
              BYAFILE.BYA_NO_1_2_2 = :QBYA-NO-1-2-2,
              BYAFILE.BYA_AMT_1_2_2 = :QBYA-AMT-1-2-2,
              BYAFILE.BYA_O2_1_2_2 = :QBYA-O2-1-2-2,
              BYAFILE.BYA_INT_1_2_2 = :QBYA-INT-1-2-2,
              BYAFILE.BYA_NO_1_2_3 = :QBYA-NO-1-2-3,
              BYAFILE.BYA_AMT_1_2_3 = :QBYA-AMT-1-2-3,
              BYAFILE.BYA_O2_1_2_3 = :QBYA-O2-1-2-3,
              BYAFILE.BYA_INT_1_2_3 = :QBYA-INT-1-2-3,
              BYAFILE.BYA_NO_1_2_4 = :QBYA-NO-1-2-4,
              BYAFILE.BYA_AMT_1_2_4 = :QBYA-AMT-1-2-4,
              BYAFILE.BYA_O2_1_2_4 = :QBYA-O2-1-2-4,
              BYAFILE.BYA_INT_1_2_4 = :QBYA-INT-1-2-4,
              BYAFILE.BYA_NO_1_2_5 = :QBYA-NO-1-2-5,
              BYAFILE.BYA_AMT_1_2_5 = :QBYA-AMT-1-2-5,
              BYAFILE.BYA_O2_1_2_5 = :QBYA-O2-1-2-5,
              BYAFILE.BYA_INT_1_2_5 = :QBYA-INT-1-2-5,
              BYAFILE.BYA_NO_1_2_6 = :QBYA-NO-1-2-6,
              BYAFILE.BYA_AMT_1_2_6 = :QBYA-AMT-1-2-6,
              BYAFILE.BYA_O2_1_2_6 = :QBYA-O2-1-2-6,
              BYAFILE.BYA_INT_1_2_6 = :QBYA-INT-1-2-6,
              BYAFILE.BYA_NO_1_2_7 = :QBYA-NO-1-2-7,
              BYAFILE.BYA_AMT_1_2_7 = :QBYA-AMT-1-2-7,
              BYAFILE.BYA_O2_1_2_7 = :QBYA-O2-1-2-7,
              BYAFILE.BYA_INT_1_2_7 = :QBYA-INT-1-2-7,
              BYAFILE.BYA_NO_1_2_8 = :QBYA-NO-1-2-8,
              BYAFILE.BYA_AMT_1_2_8 = :QBYA-AMT-1-2-8,
              BYAFILE.BYA_O2_1_2_8 = :QBYA-O2-1-2-8,
              BYAFILE.BYA_INT_1_2_8 = :QBYA-INT-1-2-8,
              BYAFILE.BYA_NO_1_2_9 = :QBYA-NO-1-2-9,
              BYAFILE.BYA_AMT_1_2_9 = :QBYA-AMT-1-2-9,
              BYAFILE.BYA_O2_1_2_9 = :QBYA-O2-1-2-9,
              BYAFILE.BYA_INT_1_2_9 = :QBYA-INT-1-2-9,
              BYAFILE.BYA_NO_1_2_10 = :QBYA-NO-1-2-10,
              BYAFILE.BYA_AMT_1_2_10 = :QBYA-AMT-1-2-10,
              BYAFILE.BYA_O2_1_2_10 = :QBYA-O2-1-2-10,
              BYAFILE.BYA_INT_1_2_10 = :QBYA-INT-1-2-10,
              BYAFILE.BYA_NO_1_2_11 = :QBYA-NO-1-2-11,
              BYAFILE.BYA_AMT_1_2_11 = :QBYA-AMT-1-2-11,
              BYAFILE.BYA_O2_1_2_11 = :QBYA-O2-1-2-11,
              BYAFILE.BYA_INT_1_2_11 = :QBYA-INT-1-2-11,
              BYAFILE.BYA_NO_2_1_1 = :QBYA-NO-2-1-1,
              BYAFILE.BYA_AMT_2_1_1 = :QBYA-AMT-2-1-1,
              BYAFILE.BYA_O2_2_1_1 = :QBYA-O2-2-1-1,
              BYAFILE.BYA_INT_2_1_1 = :QBYA-INT-2-1-1,
              BYAFILE.BYA_NO_2_1_2 = :QBYA-NO-2-1-2,
              BYAFILE.BYA_AMT_2_1_2 = :QBYA-AMT-2-1-2,
              BYAFILE.BYA_O2_2_1_2 = :QBYA-O2-2-1-2,
              BYAFILE.BYA_INT_2_1_2 = :QBYA-INT-2-1-2,
              BYAFILE.BYA_NO_2_1_3 = :QBYA-NO-2-1-3,
              BYAFILE.BYA_AMT_2_1_3 = :QBYA-AMT-2-1-3,
              BYAFILE.BYA_O2_2_1_3 = :QBYA-O2-2-1-3,
              BYAFILE.BYA_INT_2_1_3 = :QBYA-INT-2-1-3,
              BYAFILE.BYA_NO_2_1_4 = :QBYA-NO-2-1-4,
              BYAFILE.BYA_AMT_2_1_4 = :QBYA-AMT-2-1-4,
              BYAFILE.BYA_O2_2_1_4 = :QBYA-O2-2-1-4,
              BYAFILE.BYA_INT_2_1_4 = :QBYA-INT-2-1-4,
              BYAFILE.BYA_NO_2_1_5 = :QBYA-NO-2-1-5,
              BYAFILE.BYA_AMT_2_1_5 = :QBYA-AMT-2-1-5,
              BYAFILE.BYA_O2_2_1_5 = :QBYA-O2-2-1-5,
              BYAFILE.BYA_INT_2_1_5 = :QBYA-INT-2-1-5,
              BYAFILE.BYA_NO_2_1_6 = :QBYA-NO-2-1-6,
              BYAFILE.BYA_AMT_2_1_6 = :QBYA-AMT-2-1-6,
              BYAFILE.BYA_O2_2_1_6 = :QBYA-O2-2-1-6,
              BYAFILE.BYA_INT_2_1_6 = :QBYA-INT-2-1-6,
              BYAFILE.BYA_NO_2_1_7 = :QBYA-NO-2-1-7,
              BYAFILE.BYA_AMT_2_1_7 = :QBYA-AMT-2-1-7,
              BYAFILE.BYA_O2_2_1_7 = :QBYA-O2-2-1-7,
              BYAFILE.BYA_INT_2_1_7 = :QBYA-INT-2-1-7,
              BYAFILE.BYA_NO_2_1_8 = :QBYA-NO-2-1-8,
              BYAFILE.BYA_AMT_2_1_8 = :QBYA-AMT-2-1-8,
              BYAFILE.BYA_O2_2_1_8 = :QBYA-O2-2-1-8,
              BYAFILE.BYA_INT_2_1_8 = :QBYA-INT-2-1-8,
              BYAFILE.BYA_NO_2_1_9 = :QBYA-NO-2-1-9,
              BYAFILE.BYA_AMT_2_1_9 = :QBYA-AMT-2-1-9,
              BYAFILE.BYA_O2_2_1_9 = :QBYA-O2-2-1-9,
              BYAFILE.BYA_INT_2_1_9 = :QBYA-INT-2-1-9,
              BYAFILE.BYA_NO_2_1_10 = :QBYA-NO-2-1-10,
              BYAFILE.BYA_AMT_2_1_10 = :QBYA-AMT-2-1-10,
              BYAFILE.BYA_O2_2_1_10 = :QBYA-O2-2-1-10,
              BYAFILE.BYA_INT_2_1_10 = :QBYA-INT-2-1-10,
              BYAFILE.BYA_NO_2_1_11 = :QBYA-NO-2-1-11,
              BYAFILE.BYA_AMT_2_1_11 = :QBYA-AMT-2-1-11,
              BYAFILE.BYA_O2_2_1_11 = :QBYA-O2-2-1-11,
              BYAFILE.BYA_INT_2_1_11 = :QBYA-INT-2-1-11,
              BYAFILE.BYA_NO_2_2_1 = :QBYA-NO-2-2-1,
              BYAFILE.BYA_AMT_2_2_1 = :QBYA-AMT-2-2-1,
              BYAFILE.BYA_O2_2_2_1 = :QBYA-O2-2-2-1,
              BYAFILE.BYA_INT_2_2_1 = :QBYA-INT-2-2-1,
              BYAFILE.BYA_NO_2_2_2 = :QBYA-NO-2-2-2,
              BYAFILE.BYA_AMT_2_2_2 = :QBYA-AMT-2-2-2,
              BYAFILE.BYA_O2_2_2_2 = :QBYA-O2-2-2-2,
              BYAFILE.BYA_INT_2_2_2 = :QBYA-INT-2-2-2,
              BYAFILE.BYA_NO_2_2_3 = :QBYA-NO-2-2-3,
              BYAFILE.BYA_AMT_2_2_3 = :QBYA-AMT-2-2-3,
              BYAFILE.BYA_O2_2_2_3 = :QBYA-O2-2-2-3,
              BYAFILE.BYA_INT_2_2_3 = :QBYA-INT-2-2-3,
              BYAFILE.BYA_NO_2_2_4 = :QBYA-NO-2-2-4,
              BYAFILE.BYA_AMT_2_2_4 = :QBYA-AMT-2-2-4,
              BYAFILE.BYA_O2_2_2_4 = :QBYA-O2-2-2-4,
              BYAFILE.BYA_INT_2_2_4 = :QBYA-INT-2-2-4,
              BYAFILE.BYA_NO_2_2_5 = :QBYA-NO-2-2-5,
              BYAFILE.BYA_AMT_2_2_5 = :QBYA-AMT-2-2-5,
              BYAFILE.BYA_O2_2_2_5 = :QBYA-O2-2-2-5,
              BYAFILE.BYA_INT_2_2_5 = :QBYA-INT-2-2-5,
              BYAFILE.BYA_NO_2_2_6 = :QBYA-NO-2-2-6,
              BYAFILE.BYA_AMT_2_2_6 = :QBYA-AMT-2-2-6,
              BYAFILE.BYA_O2_2_2_6 = :QBYA-O2-2-2-6,
              BYAFILE.BYA_INT_2_2_6 = :QBYA-INT-2-2-6,
              BYAFILE.BYA_NO_2_2_7 = :QBYA-NO-2-2-7,
              BYAFILE.BYA_AMT_2_2_7 = :QBYA-AMT-2-2-7,
              BYAFILE.BYA_O2_2_2_7 = :QBYA-O2-2-2-7,
              BYAFILE.BYA_INT_2_2_7 = :QBYA-INT-2-2-7,
              BYAFILE.BYA_NO_2_2_8 = :QBYA-NO-2-2-8,
              BYAFILE.BYA_AMT_2_2_8 = :QBYA-AMT-2-2-8,
              BYAFILE.BYA_O2_2_2_8 = :QBYA-O2-2-2-8,
              BYAFILE.BYA_INT_2_2_8 = :QBYA-INT-2-2-8,
              BYAFILE.BYA_NO_2_2_9 = :QBYA-NO-2-2-9,
              BYAFILE.BYA_AMT_2_2_9 = :QBYA-AMT-2-2-9,
              BYAFILE.BYA_O2_2_2_9 = :QBYA-O2-2-2-9,
              BYAFILE.BYA_INT_2_2_9 = :QBYA-INT-2-2-9,
              BYAFILE.BYA_NO_2_2_10 = :QBYA-NO-2-2-10,
              BYAFILE.BYA_AMT_2_2_10 = :QBYA-AMT-2-2-10,
              BYAFILE.BYA_O2_2_2_10 = :QBYA-O2-2-2-10,
              BYAFILE.BYA_INT_2_2_10 = :QBYA-INT-2-2-10,
              BYAFILE.BYA_NO_2_2_11 = :QBYA-NO-2-2-11,
              BYAFILE.BYA_AMT_2_2_11 = :QBYA-AMT-2-2-11,
              BYAFILE.BYA_O2_2_2_11 = :QBYA-O2-2-2-11,
              BYAFILE.BYA_INT_2_2_11 = :QBYA-INT-2-2-11,
              BYAFILE.BYA_NO_3_1_1 = :QBYA-NO-3-1-1,
              BYAFILE.BYA_AMT_3_1_1 = :QBYA-AMT-3-1-1,
              BYAFILE.BYA_O2_3_1_1 = :QBYA-O2-3-1-1,
              BYAFILE.BYA_INT_3_1_1 = :QBYA-INT-3-1-1,
              BYAFILE.BYA_NO_3_1_2 = :QBYA-NO-3-1-2,
              BYAFILE.BYA_AMT_3_1_2 = :QBYA-AMT-3-1-2,
              BYAFILE.BYA_O2_3_1_2 = :QBYA-O2-3-1-2,
              BYAFILE.BYA_INT_3_1_2 = :QBYA-INT-3-1-2,
              BYAFILE.BYA_NO_3_1_3 = :QBYA-NO-3-1-3,
              BYAFILE.BYA_AMT_3_1_3 = :QBYA-AMT-3-1-3,
              BYAFILE.BYA_O2_3_1_3 = :QBYA-O2-3-1-3,
              BYAFILE.BYA_INT_3_1_3 = :QBYA-INT-3-1-3,
              BYAFILE.BYA_NO_3_1_4 = :QBYA-NO-3-1-4,
              BYAFILE.BYA_AMT_3_1_4 = :QBYA-AMT-3-1-4,
              BYAFILE.BYA_O2_3_1_4 = :QBYA-O2-3-1-4,
              BYAFILE.BYA_INT_3_1_4 = :QBYA-INT-3-1-4,
              BYAFILE.BYA_NO_3_1_5 = :QBYA-NO-3-1-5,
              BYAFILE.BYA_AMT_3_1_5 = :QBYA-AMT-3-1-5,
              BYAFILE.BYA_O2_3_1_5 = :QBYA-O2-3-1-5,
              BYAFILE.BYA_INT_3_1_5 = :QBYA-INT-3-1-5,
              BYAFILE.BYA_NO_3_1_6 = :QBYA-NO-3-1-6,
              BYAFILE.BYA_AMT_3_1_6 = :QBYA-AMT-3-1-6,
              BYAFILE.BYA_O2_3_1_6 = :QBYA-O2-3-1-6,
              BYAFILE.BYA_INT_3_1_6 = :QBYA-INT-3-1-6,
              BYAFILE.BYA_NO_3_1_7 = :QBYA-NO-3-1-7,
              BYAFILE.BYA_AMT_3_1_7 = :QBYA-AMT-3-1-7,
              BYAFILE.BYA_O2_3_1_7 = :QBYA-O2-3-1-7,
              BYAFILE.BYA_INT_3_1_7 = :QBYA-INT-3-1-7,
              BYAFILE.BYA_NO_3_1_8 = :QBYA-NO-3-1-8,
              BYAFILE.BYA_AMT_3_1_8 = :QBYA-AMT-3-1-8,
              BYAFILE.BYA_O2_3_1_8 = :QBYA-O2-3-1-8,
              BYAFILE.BYA_INT_3_1_8 = :QBYA-INT-3-1-8,
              BYAFILE.BYA_NO_3_1_9 = :QBYA-NO-3-1-9,
              BYAFILE.BYA_AMT_3_1_9 = :QBYA-AMT-3-1-9,
              BYAFILE.BYA_O2_3_1_9 = :QBYA-O2-3-1-9,
              BYAFILE.BYA_INT_3_1_9 = :QBYA-INT-3-1-9,
              BYAFILE.BYA_NO_3_1_10 = :QBYA-NO-3-1-10,
              BYAFILE.BYA_AMT_3_1_10 = :QBYA-AMT-3-1-10,
              BYAFILE.BYA_O2_3_1_10 = :QBYA-O2-3-1-10,
              BYAFILE.BYA_INT_3_1_10 = :QBYA-INT-3-1-10,
              BYAFILE.BYA_NO_3_1_11 = :QBYA-NO-3-1-11,
              BYAFILE.BYA_AMT_3_1_11 = :QBYA-AMT-3-1-11,
              BYAFILE.BYA_O2_3_1_11 = :QBYA-O2-3-1-11,
              BYAFILE.BYA_INT_3_1_11 = :QBYA-INT-3-1-11,
              BYAFILE.BYA_NO_3_2_1 = :QBYA-NO-3-2-1,
              BYAFILE.BYA_AMT_3_2_1 = :QBYA-AMT-3-2-1,
              BYAFILE.BYA_O2_3_2_1 = :QBYA-O2-3-2-1,
              BYAFILE.BYA_INT_3_2_1 = :QBYA-INT-3-2-1,
              BYAFILE.BYA_NO_3_2_2 = :QBYA-NO-3-2-2,
              BYAFILE.BYA_AMT_3_2_2 = :QBYA-AMT-3-2-2,
              BYAFILE.BYA_O2_3_2_2 = :QBYA-O2-3-2-2,
              BYAFILE.BYA_INT_3_2_2 = :QBYA-INT-3-2-2,
              BYAFILE.BYA_NO_3_2_3 = :QBYA-NO-3-2-3,
              BYAFILE.BYA_AMT_3_2_3 = :QBYA-AMT-3-2-3,
              BYAFILE.BYA_O2_3_2_3 = :QBYA-O2-3-2-3,
              BYAFILE.BYA_INT_3_2_3 = :QBYA-INT-3-2-3,
              BYAFILE.BYA_NO_3_2_4 = :QBYA-NO-3-2-4,
              BYAFILE.BYA_AMT_3_2_4 = :QBYA-AMT-3-2-4,
              BYAFILE.BYA_O2_3_2_4 = :QBYA-O2-3-2-4,
              BYAFILE.BYA_INT_3_2_4 = :QBYA-INT-3-2-4,
              BYAFILE.BYA_NO_3_2_5 = :QBYA-NO-3-2-5,
              BYAFILE.BYA_AMT_3_2_5 = :QBYA-AMT-3-2-5,
              BYAFILE.BYA_O2_3_2_5 = :QBYA-O2-3-2-5,
              BYAFILE.BYA_INT_3_2_5 = :QBYA-INT-3-2-5,
              BYAFILE.BYA_NO_3_2_6 = :QBYA-NO-3-2-6,
              BYAFILE.BYA_AMT_3_2_6 = :QBYA-AMT-3-2-6,
              BYAFILE.BYA_O2_3_2_6 = :QBYA-O2-3-2-6,
              BYAFILE.BYA_INT_3_2_6 = :QBYA-INT-3-2-6,
              BYAFILE.BYA_NO_3_2_7 = :QBYA-NO-3-2-7,
              BYAFILE.BYA_AMT_3_2_7 = :QBYA-AMT-3-2-7,
              BYAFILE.BYA_O2_3_2_7 = :QBYA-O2-3-2-7,
              BYAFILE.BYA_INT_3_2_7 = :QBYA-INT-3-2-7,
              BYAFILE.BYA_NO_3_2_8 = :QBYA-NO-3-2-8,
              BYAFILE.BYA_AMT_3_2_8 = :QBYA-AMT-3-2-8,
              BYAFILE.BYA_O2_3_2_8 = :QBYA-O2-3-2-8,
              BYAFILE.BYA_INT_3_2_8 = :QBYA-INT-3-2-8,
              BYAFILE.BYA_NO_3_2_9 = :QBYA-NO-3-2-9,
              BYAFILE.BYA_AMT_3_2_9 = :QBYA-AMT-3-2-9,
              BYAFILE.BYA_O2_3_2_9 = :QBYA-O2-3-2-9,
              BYAFILE.BYA_INT_3_2_9 = :QBYA-INT-3-2-9,
              BYAFILE.BYA_NO_3_2_10 = :QBYA-NO-3-2-10,
              BYAFILE.BYA_AMT_3_2_10 = :QBYA-AMT-3-2-10,
              BYAFILE.BYA_O2_3_2_10 = :QBYA-O2-3-2-10,
              BYAFILE.BYA_INT_3_2_10 = :QBYA-INT-3-2-10,
              BYAFILE.BYA_NO_3_2_11 = :QBYA-NO-3-2-11,
              BYAFILE.BYA_AMT_3_2_11 = :QBYA-AMT-3-2-11,
              BYAFILE.BYA_O2_3_2_11 = :QBYA-O2-3-2-11,
              BYAFILE.BYA_INT_3_2_11 = :QBYA-INT-3-2-11,
              BYAFILE.BYA_NO_REPO_1_1_1 = :QBYA-NO-REPO-1-1-1,
              BYAFILE.BYA_AMT_REPO_1_1_1 = :QBYA-AMT-REPO-1-1-1,
              BYAFILE.BYA_O2_REPO_1_1_1 = :QBYA-O2-REPO-1-1-1,
              BYAFILE.BYA_INT_REPO_1_1_1 = :QBYA-INT-REPO-1-1-1,
              BYAFILE.BYA_NO_REPO_1_1_2 = :QBYA-NO-REPO-1-1-2,
              BYAFILE.BYA_AMT_REPO_1_1_2 = :QBYA-AMT-REPO-1-1-2,
              BYAFILE.BYA_O2_REPO_1_1_2 = :QBYA-O2-REPO-1-1-2,
              BYAFILE.BYA_INT_REPO_1_1_2 = :QBYA-INT-REPO-1-1-2,
              BYAFILE.BYA_NO_REPO_1_1_3 = :QBYA-NO-REPO-1-1-3,
              BYAFILE.BYA_AMT_REPO_1_1_3 = :QBYA-AMT-REPO-1-1-3,
              BYAFILE.BYA_O2_REPO_1_1_3 = :QBYA-O2-REPO-1-1-3,
              BYAFILE.BYA_INT_REPO_1_1_3 = :QBYA-INT-REPO-1-1-3,
              BYAFILE.BYA_NO_REPO_1_1_4 = :QBYA-NO-REPO-1-1-4,
              BYAFILE.BYA_AMT_REPO_1_1_4 = :QBYA-AMT-REPO-1-1-4,
              BYAFILE.BYA_O2_REPO_1_1_4 = :QBYA-O2-REPO-1-1-4,
              BYAFILE.BYA_INT_REPO_1_1_4 = :QBYA-INT-REPO-1-1-4,
              BYAFILE.BYA_NO_REPO_1_1_5 = :QBYA-NO-REPO-1-1-5,
              BYAFILE.BYA_AMT_REPO_1_1_5 = :QBYA-AMT-REPO-1-1-5,
              BYAFILE.BYA_O2_REPO_1_1_5 = :QBYA-O2-REPO-1-1-5,
              BYAFILE.BYA_INT_REPO_1_1_5 = :QBYA-INT-REPO-1-1-5,
              BYAFILE.BYA_NO_REPO_1_2_1 = :QBYA-NO-REPO-1-2-1,
              BYAFILE.BYA_AMT_REPO_1_2_1 = :QBYA-AMT-REPO-1-2-1,
              BYAFILE.BYA_O2_REPO_1_2_1 = :QBYA-O2-REPO-1-2-1,
              BYAFILE.BYA_INT_REPO_1_2_1 = :QBYA-INT-REPO-1-2-1,
              BYAFILE.BYA_NO_REPO_1_2_2 = :QBYA-NO-REPO-1-2-2,
              BYAFILE.BYA_AMT_REPO_1_2_2 = :QBYA-AMT-REPO-1-2-2,
              BYAFILE.BYA_O2_REPO_1_2_2 = :QBYA-O2-REPO-1-2-2,
              BYAFILE.BYA_INT_REPO_1_2_2 = :QBYA-INT-REPO-1-2-2,
              BYAFILE.BYA_NO_REPO_1_2_3 = :QBYA-NO-REPO-1-2-3,
              BYAFILE.BYA_AMT_REPO_1_2_3 = :QBYA-AMT-REPO-1-2-3,
              BYAFILE.BYA_O2_REPO_1_2_3 = :QBYA-O2-REPO-1-2-3,
              BYAFILE.BYA_INT_REPO_1_2_3 = :QBYA-INT-REPO-1-2-3,
              BYAFILE.BYA_NO_REPO_1_2_4 = :QBYA-NO-REPO-1-2-4,
              BYAFILE.BYA_AMT_REPO_1_2_4 = :QBYA-AMT-REPO-1-2-4,
              BYAFILE.BYA_O2_REPO_1_2_4 = :QBYA-O2-REPO-1-2-4,
              BYAFILE.BYA_INT_REPO_1_2_4 = :QBYA-INT-REPO-1-2-4,
              BYAFILE.BYA_NO_REPO_1_2_5 = :QBYA-NO-REPO-1-2-5,
              BYAFILE.BYA_AMT_REPO_1_2_5 = :QBYA-AMT-REPO-1-2-5,
              BYAFILE.BYA_O2_REPO_1_2_5 = :QBYA-O2-REPO-1-2-5,
              BYAFILE.BYA_INT_REPO_1_2_5 = :QBYA-INT-REPO-1-2-5,
              BYAFILE.BYA_NO_REPO_2_1_1 = :QBYA-NO-REPO-2-1-1,
              BYAFILE.BYA_AMT_REPO_2_1_1 = :QBYA-AMT-REPO-2-1-1,
              BYAFILE.BYA_O2_REPO_2_1_1 = :QBYA-O2-REPO-2-1-1,
              BYAFILE.BYA_INT_REPO_2_1_1 = :QBYA-INT-REPO-2-1-1,
              BYAFILE.BYA_NO_REPO_2_1_2 = :QBYA-NO-REPO-2-1-2,
              BYAFILE.BYA_AMT_REPO_2_1_2 = :QBYA-AMT-REPO-2-1-2,
              BYAFILE.BYA_O2_REPO_2_1_2 = :QBYA-O2-REPO-2-1-2,
              BYAFILE.BYA_INT_REPO_2_1_2 = :QBYA-INT-REPO-2-1-2,
              BYAFILE.BYA_NO_REPO_2_1_3 = :QBYA-NO-REPO-2-1-3,
              BYAFILE.BYA_AMT_REPO_2_1_3 = :QBYA-AMT-REPO-2-1-3,
              BYAFILE.BYA_O2_REPO_2_1_3 = :QBYA-O2-REPO-2-1-3,
              BYAFILE.BYA_INT_REPO_2_1_3 = :QBYA-INT-REPO-2-1-3,
              BYAFILE.BYA_NO_REPO_2_1_4 = :QBYA-NO-REPO-2-1-4,
              BYAFILE.BYA_AMT_REPO_2_1_4 = :QBYA-AMT-REPO-2-1-4,
              BYAFILE.BYA_O2_REPO_2_1_4 = :QBYA-O2-REPO-2-1-4,
              BYAFILE.BYA_INT_REPO_2_1_4 = :QBYA-INT-REPO-2-1-4,
              BYAFILE.BYA_NO_REPO_2_1_5 = :QBYA-NO-REPO-2-1-5,
              BYAFILE.BYA_AMT_REPO_2_1_5 = :QBYA-AMT-REPO-2-1-5,
              BYAFILE.BYA_O2_REPO_2_1_5 = :QBYA-O2-REPO-2-1-5,
              BYAFILE.BYA_INT_REPO_2_1_5 = :QBYA-INT-REPO-2-1-5,
              BYAFILE.BYA_NO_REPO_2_2_1 = :QBYA-NO-REPO-2-2-1,
              BYAFILE.BYA_AMT_REPO_2_2_1 = :QBYA-AMT-REPO-2-2-1,
              BYAFILE.BYA_O2_REPO_2_2_1 = :QBYA-O2-REPO-2-2-1,
              BYAFILE.BYA_INT_REPO_2_2_1 = :QBYA-INT-REPO-2-2-1,
              BYAFILE.BYA_NO_REPO_2_2_2 = :QBYA-NO-REPO-2-2-2,
              BYAFILE.BYA_AMT_REPO_2_2_2 = :QBYA-AMT-REPO-2-2-2,
              BYAFILE.BYA_O2_REPO_2_2_2 = :QBYA-O2-REPO-2-2-2,
              BYAFILE.BYA_INT_REPO_2_2_2 = :QBYA-INT-REPO-2-2-2,
              BYAFILE.BYA_NO_REPO_2_2_3 = :QBYA-NO-REPO-2-2-3,
              BYAFILE.BYA_AMT_REPO_2_2_3 = :QBYA-AMT-REPO-2-2-3,
              BYAFILE.BYA_O2_REPO_2_2_3 = :QBYA-O2-REPO-2-2-3,
              BYAFILE.BYA_INT_REPO_2_2_3 = :QBYA-INT-REPO-2-2-3,
              BYAFILE.BYA_NO_REPO_2_2_4 = :QBYA-NO-REPO-2-2-4,
              BYAFILE.BYA_AMT_REPO_2_2_4 = :QBYA-AMT-REPO-2-2-4,
              BYAFILE.BYA_O2_REPO_2_2_4 = :QBYA-O2-REPO-2-2-4,
              BYAFILE.BYA_INT_REPO_2_2_4 = :QBYA-INT-REPO-2-2-4,
              BYAFILE.BYA_NO_REPO_2_2_5 = :QBYA-NO-REPO-2-2-5,
              BYAFILE.BYA_AMT_REPO_2_2_5 = :QBYA-AMT-REPO-2-2-5,
              BYAFILE.BYA_O2_REPO_2_2_5 = :QBYA-O2-REPO-2-2-5,
              BYAFILE.BYA_INT_REPO_2_2_5 = :QBYA-INT-REPO-2-2-5,
              BYAFILE.BYA_NO_REPO_3_1_1 = :QBYA-NO-REPO-3-1-1,
              BYAFILE.BYA_AMT_REPO_3_1_1 = :QBYA-AMT-REPO-3-1-1,
              BYAFILE.BYA_O2_REPO_3_1_1 = :QBYA-O2-REPO-3-1-1,
              BYAFILE.BYA_INT_REPO_3_1_1 = :QBYA-INT-REPO-3-1-1,
              BYAFILE.BYA_NO_REPO_3_1_2 = :QBYA-NO-REPO-3-1-2,
              BYAFILE.BYA_AMT_REPO_3_1_2 = :QBYA-AMT-REPO-3-1-2,
              BYAFILE.BYA_O2_REPO_3_1_2 = :QBYA-O2-REPO-3-1-2,
              BYAFILE.BYA_INT_REPO_3_1_2 = :QBYA-INT-REPO-3-1-2,
              BYAFILE.BYA_NO_REPO_3_1_3 = :QBYA-NO-REPO-3-1-3,
              BYAFILE.BYA_AMT_REPO_3_1_3 = :QBYA-AMT-REPO-3-1-3,
              BYAFILE.BYA_O2_REPO_3_1_3 = :QBYA-O2-REPO-3-1-3,
              BYAFILE.BYA_INT_REPO_3_1_3 = :QBYA-INT-REPO-3-1-3,
              BYAFILE.BYA_NO_REPO_3_1_4 = :QBYA-NO-REPO-3-1-4,
              BYAFILE.BYA_AMT_REPO_3_1_4 = :QBYA-AMT-REPO-3-1-4,
              BYAFILE.BYA_O2_REPO_3_1_4 = :QBYA-O2-REPO-3-1-4,
              BYAFILE.BYA_INT_REPO_3_1_4 = :QBYA-INT-REPO-3-1-4,
              BYAFILE.BYA_NO_REPO_3_1_5 = :QBYA-NO-REPO-3-1-5,
              BYAFILE.BYA_AMT_REPO_3_1_5 = :QBYA-AMT-REPO-3-1-5,
              BYAFILE.BYA_O2_REPO_3_1_5 = :QBYA-O2-REPO-3-1-5,
              BYAFILE.BYA_INT_REPO_3_1_5 = :QBYA-INT-REPO-3-1-5,
              BYAFILE.BYA_NO_REPO_3_2_1 = :QBYA-NO-REPO-3-2-1,
              BYAFILE.BYA_AMT_REPO_3_2_1 = :QBYA-AMT-REPO-3-2-1,
              BYAFILE.BYA_O2_REPO_3_2_1 = :QBYA-O2-REPO-3-2-1,
              BYAFILE.BYA_INT_REPO_3_2_1 = :QBYA-INT-REPO-3-2-1,
              BYAFILE.BYA_NO_REPO_3_2_2 = :QBYA-NO-REPO-3-2-2,
              BYAFILE.BYA_AMT_REPO_3_2_2 = :QBYA-AMT-REPO-3-2-2,
              BYAFILE.BYA_O2_REPO_3_2_2 = :QBYA-O2-REPO-3-2-2,
              BYAFILE.BYA_INT_REPO_3_2_2 = :QBYA-INT-REPO-3-2-2,
              BYAFILE.BYA_NO_REPO_3_2_3 = :QBYA-NO-REPO-3-2-3,
              BYAFILE.BYA_AMT_REPO_3_2_3 = :QBYA-AMT-REPO-3-2-3,
              BYAFILE.BYA_O2_REPO_3_2_3 = :QBYA-O2-REPO-3-2-3,
              BYAFILE.BYA_INT_REPO_3_2_3 = :QBYA-INT-REPO-3-2-3,
              BYAFILE.BYA_NO_REPO_3_2_4 = :QBYA-NO-REPO-3-2-4,
              BYAFILE.BYA_AMT_REPO_3_2_4 = :QBYA-AMT-REPO-3-2-4,
              BYAFILE.BYA_O2_REPO_3_2_4 = :QBYA-O2-REPO-3-2-4,
              BYAFILE.BYA_INT_REPO_3_2_4 = :QBYA-INT-REPO-3-2-4,
              BYAFILE.BYA_NO_REPO_3_2_5 = :QBYA-NO-REPO-3-2-5,
              BYAFILE.BYA_AMT_REPO_3_2_5 = :QBYA-AMT-REPO-3-2-5,
              BYAFILE.BYA_O2_REPO_3_2_5 = :QBYA-O2-REPO-3-2-5,
              BYAFILE.BYA_INT_REPO_3_2_5 = :QBYA-INT-REPO-3-2-5,
              BYAFILE.BYA_NO_PL_1_1_1 = :QBYA-NO-PL-1-1-1,
              BYAFILE.BYA_AMT_PL_1_1_1 = :QBYA-AMT-PL-1-1-1,
              BYAFILE.BYA_O2_PL_1_1_1 = :QBYA-O2-PL-1-1-1,
              BYAFILE.BYA_INT_PL_1_1_1 = :QBYA-INT-PL-1-1-1,
              BYAFILE.BYA_NO_PL_1_1_2 = :QBYA-NO-PL-1-1-2,
              BYAFILE.BYA_AMT_PL_1_1_2 = :QBYA-AMT-PL-1-1-2,
              BYAFILE.BYA_O2_PL_1_1_2 = :QBYA-O2-PL-1-1-2,
              BYAFILE.BYA_INT_PL_1_1_2 = :QBYA-INT-PL-1-1-2,
              BYAFILE.BYA_NO_PL_1_1_3 = :QBYA-NO-PL-1-1-3,
              BYAFILE.BYA_AMT_PL_1_1_3 = :QBYA-AMT-PL-1-1-3,
              BYAFILE.BYA_O2_PL_1_1_3 = :QBYA-O2-PL-1-1-3,
              BYAFILE.BYA_INT_PL_1_1_3 = :QBYA-INT-PL-1-1-3,
              BYAFILE.BYA_NO_PL_1_1_4 = :QBYA-NO-PL-1-1-4,
              BYAFILE.BYA_AMT_PL_1_1_4 = :QBYA-AMT-PL-1-1-4,
              BYAFILE.BYA_O2_PL_1_1_4 = :QBYA-O2-PL-1-1-4,
              BYAFILE.BYA_INT_PL_1_1_4 = :QBYA-INT-PL-1-1-4,
              BYAFILE.BYA_NO_PL_1_1_5 = :QBYA-NO-PL-1-1-5,
              BYAFILE.BYA_AMT_PL_1_1_5 = :QBYA-AMT-PL-1-1-5,
              BYAFILE.BYA_O2_PL_1_1_5 = :QBYA-O2-PL-1-1-5,
              BYAFILE.BYA_INT_PL_1_1_5 = :QBYA-INT-PL-1-1-5,
              BYAFILE.BYA_NO_PL_1_1_6 = :QBYA-NO-PL-1-1-6,
              BYAFILE.BYA_AMT_PL_1_1_6 = :QBYA-AMT-PL-1-1-6,
              BYAFILE.BYA_O2_PL_1_1_6 = :QBYA-O2-PL-1-1-6,
              BYAFILE.BYA_INT_PL_1_1_6 = :QBYA-INT-PL-1-1-6,
              BYAFILE.BYA_NO_PL_1_1_7 = :QBYA-NO-PL-1-1-7,
              BYAFILE.BYA_AMT_PL_1_1_7 = :QBYA-AMT-PL-1-1-7,
              BYAFILE.BYA_O2_PL_1_1_7 = :QBYA-O2-PL-1-1-7,
              BYAFILE.BYA_INT_PL_1_1_7 = :QBYA-INT-PL-1-1-7,
              BYAFILE.BYA_NO_PL_1_1_8 = :QBYA-NO-PL-1-1-8,
              BYAFILE.BYA_AMT_PL_1_1_8 = :QBYA-AMT-PL-1-1-8,
              BYAFILE.BYA_O2_PL_1_1_8 = :QBYA-O2-PL-1-1-8,
              BYAFILE.BYA_INT_PL_1_1_8 = :QBYA-INT-PL-1-1-8,
              BYAFILE.BYA_NO_PL_1_1_9 = :QBYA-NO-PL-1-1-9,
              BYAFILE.BYA_AMT_PL_1_1_9 = :QBYA-AMT-PL-1-1-9,
              BYAFILE.BYA_O2_PL_1_1_9 = :QBYA-O2-PL-1-1-9,
              BYAFILE.BYA_INT_PL_1_1_9 = :QBYA-INT-PL-1-1-9,
              BYAFILE.BYA_NO_PL_1_1_10 = :QBYA-NO-PL-1-1-10,
              BYAFILE.BYA_AMT_PL_1_1_10 = :QBYA-AMT-PL-1-1-10,
              BYAFILE.BYA_O2_PL_1_1_10 = :QBYA-O2-PL-1-1-10,
              BYAFILE.BYA_INT_PL_1_1_10 = :QBYA-INT-PL-1-1-10,
              BYAFILE.BYA_NO_PL_1_2_1 = :QBYA-NO-PL-1-2-1,
              BYAFILE.BYA_AMT_PL_1_2_1 = :QBYA-AMT-PL-1-2-1,
              BYAFILE.BYA_O2_PL_1_2_1 = :QBYA-O2-PL-1-2-1,
              BYAFILE.BYA_INT_PL_1_2_1 = :QBYA-INT-PL-1-2-1,
              BYAFILE.BYA_NO_PL_1_2_2 = :QBYA-NO-PL-1-2-2,
              BYAFILE.BYA_AMT_PL_1_2_2 = :QBYA-AMT-PL-1-2-2,
              BYAFILE.BYA_O2_PL_1_2_2 = :QBYA-O2-PL-1-2-2,
              BYAFILE.BYA_INT_PL_1_2_2 = :QBYA-INT-PL-1-2-2,
              BYAFILE.BYA_NO_PL_1_2_3 = :QBYA-NO-PL-1-2-3,
              BYAFILE.BYA_AMT_PL_1_2_3 = :QBYA-AMT-PL-1-2-3,
              BYAFILE.BYA_O2_PL_1_2_3 = :QBYA-O2-PL-1-2-3,
              BYAFILE.BYA_INT_PL_1_2_3 = :QBYA-INT-PL-1-2-3,
              BYAFILE.BYA_NO_PL_1_2_4 = :QBYA-NO-PL-1-2-4,
              BYAFILE.BYA_AMT_PL_1_2_4 = :QBYA-AMT-PL-1-2-4,
              BYAFILE.BYA_O2_PL_1_2_4 = :QBYA-O2-PL-1-2-4,
              BYAFILE.BYA_INT_PL_1_2_4 = :QBYA-INT-PL-1-2-4,
              BYAFILE.BYA_NO_PL_1_2_5 = :QBYA-NO-PL-1-2-5,
              BYAFILE.BYA_AMT_PL_1_2_5 = :QBYA-AMT-PL-1-2-5,
              BYAFILE.BYA_O2_PL_1_2_5 = :QBYA-O2-PL-1-2-5,
              BYAFILE.BYA_INT_PL_1_2_5 = :QBYA-INT-PL-1-2-5,
              BYAFILE.BYA_NO_PL_1_2_6 = :QBYA-NO-PL-1-2-6,
              BYAFILE.BYA_AMT_PL_1_2_6 = :QBYA-AMT-PL-1-2-6,
              BYAFILE.BYA_O2_PL_1_2_6 = :QBYA-O2-PL-1-2-6,
              BYAFILE.BYA_INT_PL_1_2_6 = :QBYA-INT-PL-1-2-6,
              BYAFILE.BYA_NO_PL_1_2_7 = :QBYA-NO-PL-1-2-7,
              BYAFILE.BYA_AMT_PL_1_2_7 = :QBYA-AMT-PL-1-2-7,
              BYAFILE.BYA_O2_PL_1_2_7 = :QBYA-O2-PL-1-2-7,
              BYAFILE.BYA_INT_PL_1_2_7 = :QBYA-INT-PL-1-2-7,
              BYAFILE.BYA_NO_PL_1_2_8 = :QBYA-NO-PL-1-2-8,
              BYAFILE.BYA_AMT_PL_1_2_8 = :QBYA-AMT-PL-1-2-8,
              BYAFILE.BYA_O2_PL_1_2_8 = :QBYA-O2-PL-1-2-8,
              BYAFILE.BYA_INT_PL_1_2_8 = :QBYA-INT-PL-1-2-8,
              BYAFILE.BYA_NO_PL_1_2_9 = :QBYA-NO-PL-1-2-9,
              BYAFILE.BYA_AMT_PL_1_2_9 = :QBYA-AMT-PL-1-2-9,
              BYAFILE.BYA_O2_PL_1_2_9 = :QBYA-O2-PL-1-2-9,
              BYAFILE.BYA_INT_PL_1_2_9 = :QBYA-INT-PL-1-2-9,
              BYAFILE.BYA_NO_PL_1_2_10 = :QBYA-NO-PL-1-2-10,
              BYAFILE.BYA_AMT_PL_1_2_10 = :QBYA-AMT-PL-1-2-10,
              BYAFILE.BYA_O2_PL_1_2_10 = :QBYA-O2-PL-1-2-10,
              BYAFILE.BYA_INT_PL_1_2_10 = :QBYA-INT-PL-1-2-10,
              BYAFILE.BYA_NO_PL_2_1_1 = :QBYA-NO-PL-2-1-1,
              BYAFILE.BYA_AMT_PL_2_1_1 = :QBYA-AMT-PL-2-1-1,
              BYAFILE.BYA_O2_PL_2_1_1 = :QBYA-O2-PL-2-1-1,
              BYAFILE.BYA_INT_PL_2_1_1 = :QBYA-INT-PL-2-1-1,
              BYAFILE.BYA_NO_PL_2_1_2 = :QBYA-NO-PL-2-1-2,
              BYAFILE.BYA_AMT_PL_2_1_2 = :QBYA-AMT-PL-2-1-2,
              BYAFILE.BYA_O2_PL_2_1_2 = :QBYA-O2-PL-2-1-2,
              BYAFILE.BYA_INT_PL_2_1_2 = :QBYA-INT-PL-2-1-2,
              BYAFILE.BYA_NO_PL_2_1_3 = :QBYA-NO-PL-2-1-3,
              BYAFILE.BYA_AMT_PL_2_1_3 = :QBYA-AMT-PL-2-1-3,
              BYAFILE.BYA_O2_PL_2_1_3 = :QBYA-O2-PL-2-1-3,
              BYAFILE.BYA_INT_PL_2_1_3 = :QBYA-INT-PL-2-1-3,
              BYAFILE.BYA_NO_PL_2_1_4 = :QBYA-NO-PL-2-1-4,
              BYAFILE.BYA_AMT_PL_2_1_4 = :QBYA-AMT-PL-2-1-4,
              BYAFILE.BYA_O2_PL_2_1_4 = :QBYA-O2-PL-2-1-4,
              BYAFILE.BYA_INT_PL_2_1_4 = :QBYA-INT-PL-2-1-4,
              BYAFILE.BYA_NO_PL_2_1_5 = :QBYA-NO-PL-2-1-5,
              BYAFILE.BYA_AMT_PL_2_1_5 = :QBYA-AMT-PL-2-1-5,
              BYAFILE.BYA_O2_PL_2_1_5 = :QBYA-O2-PL-2-1-5,
              BYAFILE.BYA_INT_PL_2_1_5 = :QBYA-INT-PL-2-1-5,
              BYAFILE.BYA_NO_PL_2_1_6 = :QBYA-NO-PL-2-1-6,
              BYAFILE.BYA_AMT_PL_2_1_6 = :QBYA-AMT-PL-2-1-6,
              BYAFILE.BYA_O2_PL_2_1_6 = :QBYA-O2-PL-2-1-6,
              BYAFILE.BYA_INT_PL_2_1_6 = :QBYA-INT-PL-2-1-6,
              BYAFILE.BYA_NO_PL_2_1_7 = :QBYA-NO-PL-2-1-7,
              BYAFILE.BYA_AMT_PL_2_1_7 = :QBYA-AMT-PL-2-1-7,
              BYAFILE.BYA_O2_PL_2_1_7 = :QBYA-O2-PL-2-1-7,
              BYAFILE.BYA_INT_PL_2_1_7 = :QBYA-INT-PL-2-1-7,
              BYAFILE.BYA_NO_PL_2_1_8 = :QBYA-NO-PL-2-1-8,
              BYAFILE.BYA_AMT_PL_2_1_8 = :QBYA-AMT-PL-2-1-8,
              BYAFILE.BYA_O2_PL_2_1_8 = :QBYA-O2-PL-2-1-8,
              BYAFILE.BYA_INT_PL_2_1_8 = :QBYA-INT-PL-2-1-8,
              BYAFILE.BYA_NO_PL_2_1_9 = :QBYA-NO-PL-2-1-9,
              BYAFILE.BYA_AMT_PL_2_1_9 = :QBYA-AMT-PL-2-1-9,
              BYAFILE.BYA_O2_PL_2_1_9 = :QBYA-O2-PL-2-1-9,
              BYAFILE.BYA_INT_PL_2_1_9 = :QBYA-INT-PL-2-1-9,
              BYAFILE.BYA_NO_PL_2_1_10 = :QBYA-NO-PL-2-1-10,
              BYAFILE.BYA_AMT_PL_2_1_10 = :QBYA-AMT-PL-2-1-10,
              BYAFILE.BYA_O2_PL_2_1_10 = :QBYA-O2-PL-2-1-10,
              BYAFILE.BYA_INT_PL_2_1_10 = :QBYA-INT-PL-2-1-10,
              BYAFILE.BYA_NO_PL_2_2_1 = :QBYA-NO-PL-2-2-1,
              BYAFILE.BYA_AMT_PL_2_2_1 = :QBYA-AMT-PL-2-2-1,
              BYAFILE.BYA_O2_PL_2_2_1 = :QBYA-O2-PL-2-2-1,
              BYAFILE.BYA_INT_PL_2_2_1 = :QBYA-INT-PL-2-2-1,
              BYAFILE.BYA_NO_PL_2_2_2 = :QBYA-NO-PL-2-2-2,
              BYAFILE.BYA_AMT_PL_2_2_2 = :QBYA-AMT-PL-2-2-2,
              BYAFILE.BYA_O2_PL_2_2_2 = :QBYA-O2-PL-2-2-2,
              BYAFILE.BYA_INT_PL_2_2_2 = :QBYA-INT-PL-2-2-2,
              BYAFILE.BYA_NO_PL_2_2_3 = :QBYA-NO-PL-2-2-3,
              BYAFILE.BYA_AMT_PL_2_2_3 = :QBYA-AMT-PL-2-2-3,
              BYAFILE.BYA_O2_PL_2_2_3 = :QBYA-O2-PL-2-2-3,
              BYAFILE.BYA_INT_PL_2_2_3 = :QBYA-INT-PL-2-2-3,
              BYAFILE.BYA_NO_PL_2_2_4 = :QBYA-NO-PL-2-2-4,
              BYAFILE.BYA_AMT_PL_2_2_4 = :QBYA-AMT-PL-2-2-4,
              BYAFILE.BYA_O2_PL_2_2_4 = :QBYA-O2-PL-2-2-4,
              BYAFILE.BYA_INT_PL_2_2_4 = :QBYA-INT-PL-2-2-4,
              BYAFILE.BYA_NO_PL_2_2_5 = :QBYA-NO-PL-2-2-5,
              BYAFILE.BYA_AMT_PL_2_2_5 = :QBYA-AMT-PL-2-2-5,
              BYAFILE.BYA_O2_PL_2_2_5 = :QBYA-O2-PL-2-2-5,
              BYAFILE.BYA_INT_PL_2_2_5 = :QBYA-INT-PL-2-2-5,
              BYAFILE.BYA_NO_PL_2_2_6 = :QBYA-NO-PL-2-2-6,
              BYAFILE.BYA_AMT_PL_2_2_6 = :QBYA-AMT-PL-2-2-6,
              BYAFILE.BYA_O2_PL_2_2_6 = :QBYA-O2-PL-2-2-6,
              BYAFILE.BYA_INT_PL_2_2_6 = :QBYA-INT-PL-2-2-6,
              BYAFILE.BYA_NO_PL_2_2_7 = :QBYA-NO-PL-2-2-7,
              BYAFILE.BYA_AMT_PL_2_2_7 = :QBYA-AMT-PL-2-2-7,
              BYAFILE.BYA_O2_PL_2_2_7 = :QBYA-O2-PL-2-2-7,
              BYAFILE.BYA_INT_PL_2_2_7 = :QBYA-INT-PL-2-2-7,
              BYAFILE.BYA_NO_PL_2_2_8 = :QBYA-NO-PL-2-2-8,
              BYAFILE.BYA_AMT_PL_2_2_8 = :QBYA-AMT-PL-2-2-8,
              BYAFILE.BYA_O2_PL_2_2_8 = :QBYA-O2-PL-2-2-8,
              BYAFILE.BYA_INT_PL_2_2_8 = :QBYA-INT-PL-2-2-8,
              BYAFILE.BYA_NO_PL_2_2_9 = :QBYA-NO-PL-2-2-9,
              BYAFILE.BYA_AMT_PL_2_2_9 = :QBYA-AMT-PL-2-2-9,
              BYAFILE.BYA_O2_PL_2_2_9 = :QBYA-O2-PL-2-2-9,
              BYAFILE.BYA_INT_PL_2_2_9 = :QBYA-INT-PL-2-2-9,
              BYAFILE.BYA_NO_PL_2_2_10 = :QBYA-NO-PL-2-2-10,
              BYAFILE.BYA_AMT_PL_2_2_10 = :QBYA-AMT-PL-2-2-10,
              BYAFILE.BYA_O2_PL_2_2_10 = :QBYA-O2-PL-2-2-10,
              BYAFILE.BYA_INT_PL_2_2_10 = :QBYA-INT-PL-2-2-10,
              BYAFILE.BYA_NO_PL_3_1_1 = :QBYA-NO-PL-3-1-1,
              BYAFILE.BYA_AMT_PL_3_1_1 = :QBYA-AMT-PL-3-1-1,
              BYAFILE.BYA_O2_PL_3_1_1 = :QBYA-O2-PL-3-1-1,
              BYAFILE.BYA_INT_PL_3_1_1 = :QBYA-INT-PL-3-1-1,
              BYAFILE.BYA_NO_PL_3_1_2 = :QBYA-NO-PL-3-1-2,
              BYAFILE.BYA_AMT_PL_3_1_2 = :QBYA-AMT-PL-3-1-2,
              BYAFILE.BYA_O2_PL_3_1_2 = :QBYA-O2-PL-3-1-2,
              BYAFILE.BYA_INT_PL_3_1_2 = :QBYA-INT-PL-3-1-2,
              BYAFILE.BYA_NO_PL_3_1_3 = :QBYA-NO-PL-3-1-3,
              BYAFILE.BYA_AMT_PL_3_1_3 = :QBYA-AMT-PL-3-1-3,
              BYAFILE.BYA_O2_PL_3_1_3 = :QBYA-O2-PL-3-1-3,
              BYAFILE.BYA_INT_PL_3_1_3 = :QBYA-INT-PL-3-1-3,
              BYAFILE.BYA_NO_PL_3_1_4 = :QBYA-NO-PL-3-1-4,
              BYAFILE.BYA_AMT_PL_3_1_4 = :QBYA-AMT-PL-3-1-4,
              BYAFILE.BYA_O2_PL_3_1_4 = :QBYA-O2-PL-3-1-4,
              BYAFILE.BYA_INT_PL_3_1_4 = :QBYA-INT-PL-3-1-4,
              BYAFILE.BYA_NO_PL_3_1_5 = :QBYA-NO-PL-3-1-5,
              BYAFILE.BYA_AMT_PL_3_1_5 = :QBYA-AMT-PL-3-1-5,
              BYAFILE.BYA_O2_PL_3_1_5 = :QBYA-O2-PL-3-1-5,
              BYAFILE.BYA_INT_PL_3_1_5 = :QBYA-INT-PL-3-1-5,
              BYAFILE.BYA_NO_PL_3_1_6 = :QBYA-NO-PL-3-1-6,
              BYAFILE.BYA_AMT_PL_3_1_6 = :QBYA-AMT-PL-3-1-6,
              BYAFILE.BYA_O2_PL_3_1_6 = :QBYA-O2-PL-3-1-6,
              BYAFILE.BYA_INT_PL_3_1_6 = :QBYA-INT-PL-3-1-6,
              BYAFILE.BYA_NO_PL_3_1_7 = :QBYA-NO-PL-3-1-7,
              BYAFILE.BYA_AMT_PL_3_1_7 = :QBYA-AMT-PL-3-1-7,
              BYAFILE.BYA_O2_PL_3_1_7 = :QBYA-O2-PL-3-1-7,
              BYAFILE.BYA_INT_PL_3_1_7 = :QBYA-INT-PL-3-1-7,
              BYAFILE.BYA_NO_PL_3_1_8 = :QBYA-NO-PL-3-1-8,
              BYAFILE.BYA_AMT_PL_3_1_8 = :QBYA-AMT-PL-3-1-8,
              BYAFILE.BYA_O2_PL_3_1_8 = :QBYA-O2-PL-3-1-8,
              BYAFILE.BYA_INT_PL_3_1_8 = :QBYA-INT-PL-3-1-8,
              BYAFILE.BYA_NO_PL_3_1_9 = :QBYA-NO-PL-3-1-9,
              BYAFILE.BYA_AMT_PL_3_1_9 = :QBYA-AMT-PL-3-1-9,
              BYAFILE.BYA_O2_PL_3_1_9 = :QBYA-O2-PL-3-1-9,
              BYAFILE.BYA_INT_PL_3_1_9 = :QBYA-INT-PL-3-1-9,
              BYAFILE.BYA_NO_PL_3_1_10 = :QBYA-NO-PL-3-1-10,
              BYAFILE.BYA_AMT_PL_3_1_10 = :QBYA-AMT-PL-3-1-10,
              BYAFILE.BYA_O2_PL_3_1_10 = :QBYA-O2-PL-3-1-10,
              BYAFILE.BYA_INT_PL_3_1_10 = :QBYA-INT-PL-3-1-10,
              BYAFILE.BYA_NO_PL_3_2_1 = :QBYA-NO-PL-3-2-1,
              BYAFILE.BYA_AMT_PL_3_2_1 = :QBYA-AMT-PL-3-2-1,
              BYAFILE.BYA_O2_PL_3_2_1 = :QBYA-O2-PL-3-2-1,
              BYAFILE.BYA_INT_PL_3_2_1 = :QBYA-INT-PL-3-2-1,
              BYAFILE.BYA_NO_PL_3_2_2 = :QBYA-NO-PL-3-2-2,
              BYAFILE.BYA_AMT_PL_3_2_2 = :QBYA-AMT-PL-3-2-2,
              BYAFILE.BYA_O2_PL_3_2_2 = :QBYA-O2-PL-3-2-2,
              BYAFILE.BYA_INT_PL_3_2_2 = :QBYA-INT-PL-3-2-2,
              BYAFILE.BYA_NO_PL_3_2_3 = :QBYA-NO-PL-3-2-3,
              BYAFILE.BYA_AMT_PL_3_2_3 = :QBYA-AMT-PL-3-2-3,
              BYAFILE.BYA_O2_PL_3_2_3 = :QBYA-O2-PL-3-2-3,
              BYAFILE.BYA_INT_PL_3_2_3 = :QBYA-INT-PL-3-2-3,
              BYAFILE.BYA_NO_PL_3_2_4 = :QBYA-NO-PL-3-2-4,
              BYAFILE.BYA_AMT_PL_3_2_4 = :QBYA-AMT-PL-3-2-4,
              BYAFILE.BYA_O2_PL_3_2_4 = :QBYA-O2-PL-3-2-4,
              BYAFILE.BYA_INT_PL_3_2_4 = :QBYA-INT-PL-3-2-4,
              BYAFILE.BYA_NO_PL_3_2_5 = :QBYA-NO-PL-3-2-5,
              BYAFILE.BYA_AMT_PL_3_2_5 = :QBYA-AMT-PL-3-2-5,
              BYAFILE.BYA_O2_PL_3_2_5 = :QBYA-O2-PL-3-2-5,
              BYAFILE.BYA_INT_PL_3_2_5 = :QBYA-INT-PL-3-2-5,
              BYAFILE.BYA_NO_PL_3_2_6 = :QBYA-NO-PL-3-2-6,
              BYAFILE.BYA_AMT_PL_3_2_6 = :QBYA-AMT-PL-3-2-6,
              BYAFILE.BYA_O2_PL_3_2_6 = :QBYA-O2-PL-3-2-6,
              BYAFILE.BYA_INT_PL_3_2_6 = :QBYA-INT-PL-3-2-6,
              BYAFILE.BYA_NO_PL_3_2_7 = :QBYA-NO-PL-3-2-7,
              BYAFILE.BYA_AMT_PL_3_2_7 = :QBYA-AMT-PL-3-2-7,
              BYAFILE.BYA_O2_PL_3_2_7 = :QBYA-O2-PL-3-2-7,
              BYAFILE.BYA_INT_PL_3_2_7 = :QBYA-INT-PL-3-2-7,
              BYAFILE.BYA_NO_PL_3_2_8 = :QBYA-NO-PL-3-2-8,
              BYAFILE.BYA_AMT_PL_3_2_8 = :QBYA-AMT-PL-3-2-8,
              BYAFILE.BYA_O2_PL_3_2_8 = :QBYA-O2-PL-3-2-8,
              BYAFILE.BYA_INT_PL_3_2_8 = :QBYA-INT-PL-3-2-8,
              BYAFILE.BYA_NO_PL_3_2_9 = :QBYA-NO-PL-3-2-9,
              BYAFILE.BYA_AMT_PL_3_2_9 = :QBYA-AMT-PL-3-2-9,
              BYAFILE.BYA_O2_PL_3_2_9 = :QBYA-O2-PL-3-2-9,
              BYAFILE.BYA_INT_PL_3_2_9 = :QBYA-INT-PL-3-2-9,
              BYAFILE.BYA_NO_PL_3_2_10 = :QBYA-NO-PL-3-2-10,
              BYAFILE.BYA_AMT_PL_3_2_10 = :QBYA-AMT-PL-3-2-10,
              BYAFILE.BYA_O2_PL_3_2_10 = :QBYA-O2-PL-3-2-10,
              BYAFILE.BYA_INT_PL_3_2_10 = :QBYA-INT-PL-3-2-10,
              BYAFILE.BYA_DLRVPRIN_1_1_1 = :QBYA-DLRVPRIN-1-1-1,
              BYAFILE.BYA_DLRVPRIN_1_1_2 = :QBYA-DLRVPRIN-1-1-2,
              BYAFILE.BYA_DLRVPRIN_1_1_3 = :QBYA-DLRVPRIN-1-1-3,
              BYAFILE.BYA_DLRVCHGS_1_1_1 = :QBYA-DLRVCHGS-1-1-1,
              BYAFILE.BYA_DLRVCHGS_1_1_2 = :QBYA-DLRVCHGS-1-1-2,
              BYAFILE.BYA_DLRVCHGS_1_1_3 = :QBYA-DLRVCHGS-1-1-3,
              BYAFILE.BYA_DLPARVHELD_1_1 = :QBYA-DLPARVHELD-1-1,
              BYAFILE.BYA_DLRVPRIN_1_2_1 = :QBYA-DLRVPRIN-1-2-1,
              BYAFILE.BYA_DLRVPRIN_1_2_2 = :QBYA-DLRVPRIN-1-2-2,
              BYAFILE.BYA_DLRVPRIN_1_2_3 = :QBYA-DLRVPRIN-1-2-3,
              BYAFILE.BYA_DLRVCHGS_1_2_1 = :QBYA-DLRVCHGS-1-2-1,
              BYAFILE.BYA_DLRVCHGS_1_2_2 = :QBYA-DLRVCHGS-1-2-2,
              BYAFILE.BYA_DLRVCHGS_1_2_3 = :QBYA-DLRVCHGS-1-2-3,
              BYAFILE.BYA_DLPARVHELD_1_2 = :QBYA-DLPARVHELD-1-2,
              BYAFILE.BYA_DLRVPRIN_2_1_1 = :QBYA-DLRVPRIN-2-1-1,
              BYAFILE.BYA_DLRVPRIN_2_1_2 = :QBYA-DLRVPRIN-2-1-2,
              BYAFILE.BYA_DLRVPRIN_2_1_3 = :QBYA-DLRVPRIN-2-1-3,
              BYAFILE.BYA_DLRVCHGS_2_1_1 = :QBYA-DLRVCHGS-2-1-1,
              BYAFILE.BYA_DLRVCHGS_2_1_2 = :QBYA-DLRVCHGS-2-1-2,
              BYAFILE.BYA_DLRVCHGS_2_1_3 = :QBYA-DLRVCHGS-2-1-3,
              BYAFILE.BYA_DLPARVHELD_2_1 = :QBYA-DLPARVHELD-2-1,
              BYAFILE.BYA_DLRVPRIN_2_2_1 = :QBYA-DLRVPRIN-2-2-1,
              BYAFILE.BYA_DLRVPRIN_2_2_2 = :QBYA-DLRVPRIN-2-2-2,
              BYAFILE.BYA_DLRVPRIN_2_2_3 = :QBYA-DLRVPRIN-2-2-3,
              BYAFILE.BYA_DLRVCHGS_2_2_1 = :QBYA-DLRVCHGS-2-2-1,
              BYAFILE.BYA_DLRVCHGS_2_2_2 = :QBYA-DLRVCHGS-2-2-2,
              BYAFILE.BYA_DLRVCHGS_2_2_3 = :QBYA-DLRVCHGS-2-2-3,
              BYAFILE.BYA_DLPARVHELD_2_2 = :QBYA-DLPARVHELD-2-2,
              BYAFILE.BYA_DLRVPRIN_3_1_1 = :QBYA-DLRVPRIN-3-1-1,
              BYAFILE.BYA_DLRVPRIN_3_1_2 = :QBYA-DLRVPRIN-3-1-2,
              BYAFILE.BYA_DLRVPRIN_3_1_3 = :QBYA-DLRVPRIN-3-1-3,
              BYAFILE.BYA_DLRVCHGS_3_1_1 = :QBYA-DLRVCHGS-3-1-1,
              BYAFILE.BYA_DLRVCHGS_3_1_2 = :QBYA-DLRVCHGS-3-1-2,
              BYAFILE.BYA_DLRVCHGS_3_1_3 = :QBYA-DLRVCHGS-3-1-3,
              BYAFILE.BYA_DLPARVHELD_3_1 = :QBYA-DLPARVHELD-3-1,
              BYAFILE.BYA_DLRVPRIN_3_2_1 = :QBYA-DLRVPRIN-3-2-1,
              BYAFILE.BYA_DLRVPRIN_3_2_2 = :QBYA-DLRVPRIN-3-2-2,
              BYAFILE.BYA_DLRVPRIN_3_2_3 = :QBYA-DLRVPRIN-3-2-3,
              BYAFILE.BYA_DLRVCHGS_3_2_1 = :QBYA-DLRVCHGS-3-2-1,
              BYAFILE.BYA_DLRVCHGS_3_2_2 = :QBYA-DLRVCHGS-3-2-2,
              BYAFILE.BYA_DLRVCHGS_3_2_3 = :QBYA-DLRVCHGS-3-2-3,
              BYAFILE.BYA_DLPARVHELD_3_2 = :QBYA-DLPARVHELD-3-2
             WHERE BYAFILE.BYA_DATE_CCYYMM = :QBYA-DATE-CCYYMM
               AND BYAFILE.BYA_BRNO = :QBYA-BRNO 
               AND BYAFILE.BYA_CLASS = :QBYA-CLASS
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-BYA1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

           MOVE '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       DELETE-BYA1-FILE.

           PERFORM DELETE-IT.
           MOVE BYA-PATH-SQL TO E-FILE.
           MOVE BYA1-KEY TO E-KEYX.
           PERFORM SET-BYA-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            DELETE FROM DBO.BYAFILE
             WHERE BYAFILE.BYA_DATE_CCYYMM = :QBYA-DATE-CCYYMM
               AND BYAFILE.BYA_BRNO = :QBYA-BRNO 
               AND BYAFILE.BYA_CLASS = :QBYA-CLASS
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-BYA1-FILE.

           MOVE '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       CLOSE-BYA1-FILE.

           IF ( BYA1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION BY1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE BYA1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO BYA1-CURSOR-STAT.

      *=================================================================
       LOAD-BYB1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BY-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BY-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BYB-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BYB-ALL-MACHINE
              MOVE BYB-ALL-PATH            TO BYB-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BY-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BYB-ALL-BASE
              MOVE FILPATH-MACHINE         TO BYB-ALL-MACHINE
              MOVE BYB-ALL-PATH            TO BYB-PATH.

      *-----------------------------------------------------------------
       OPEN-BYB1-FILE.
           PERFORM LOAD-BYB1-FILE.
           PERFORM OPEN-IT.
           MOVE BYB-PATH-SQL TO E-FILE.

      *-----------------------------------------------------------------
       READ-BYB1-FILE.
           PERFORM READ-IT.
           MOVE BYB-PATH-SQL TO E-FILE.
           MOVE BYB1-KEY TO E-KEYX.
           INITIALIZE QBYB-REC.

           MOVE BYB-DATE-CCYYMM      TO QBYB-DATE-CCYYMM.
           MOVE BYB-BRNO             TO QBYB-BRNO.
           MOVE BYB-CLASS            TO QBYB-CLASS.

           EXEC SQL
            SELECT BYBFILE.BYB_DATE_CCYYMM,
                   BYBFILE.BYB_BRNO,
                   BYBFILE.BYB_CLASS,
                   BYBFILE.BYB_PL_PRINCOL_1_1,
                   BYBFILE.BYB_PL_INTCOL_1_1,
                   BYBFILE.BYB_PL_LATECOL_1_1,
                   BYBFILE.BYB_PL_O2COL_1_1,
                   BYBFILE.BYB_PL_PRINCOL_1_2,
                   BYBFILE.BYB_PL_INTCOL_1_2,
                   BYBFILE.BYB_PL_LATECOL_1_2,
                   BYBFILE.BYB_PL_O2COL_1_2,
                   BYBFILE.BYB_PL_PRINCOL_1_3,
                   BYBFILE.BYB_PL_INTCOL_1_3,
                   BYBFILE.BYB_PL_LATECOL_1_3,
                   BYBFILE.BYB_PL_O2COL_1_3,
                   BYBFILE.BYB_PL_PRINCOL_1_4,
                   BYBFILE.BYB_PL_INTCOL_1_4,
                   BYBFILE.BYB_PL_LATECOL_1_4,
                   BYBFILE.BYB_PL_O2COL_1_4,
                   BYBFILE.BYB_PL_PRINCOL_1_5,
                   BYBFILE.BYB_PL_INTCOL_1_5,
                   BYBFILE.BYB_PL_LATECOL_1_5,
                   BYBFILE.BYB_PL_O2COL_1_5,
                   BYBFILE.BYB_AMT_SOLD_1,
                   BYBFILE.BYB_AMT_TRANS_1,
                   BYBFILE.BYB_AMT_PURCH_1,
                   BYBFILE.BYB_NO_SOLD_1,
                   BYBFILE.BYB_NO_TRANS_1,
                   BYBFILE.BYB_NO_PURCH_1,
                   BYBFILE.BYB_OTHCOL_1,
                   BYBFILE.BYB_OT2COL_1,
                   BYBFILE.BYB_INTCOL_1,
                   BYBFILE.BYB_LATECOL_1,
                   BYBFILE.BYB_PRINCOL_1,
                   BYBFILE.BYB_DEFCOL_1,
                   BYBFILE.BYB_RNPRIN_1,
                   BYBFILE.BYB_INTREB_1,
                   BYBFILE.BYB_CLREB1_1,
                   BYBFILE.BYB_CLREB2_1,
                   BYBFILE.BYB_CLREB3_1,
                   BYBFILE.BYB_CLREB4_1,
                   BYBFILE.BYB_AHREB_1,
                   BYBFILE.BYB_PPREB_1,
                   BYBFILE.BYB_O1REB_1,
                   BYBFILE.BYB_O2REB_1,
                   BYBFILE.BYB_O3REB_1,
                   BYBFILE.BYB_O4REB_1,
                   BYBFILE.BYB_O5REB_1,
                   BYBFILE.BYB_O6REB_1,
                   BYBFILE.BYB_O7REB_1,
                   BYBFILE.BYB_SCREB_1,
                   BYBFILE.BYB_MFREB_1,
                   BYBFILE.BYB_DFREB_1,
                   BYBFILE.BYB_UN7_REB_1,
                   BYBFILE.BYB_UN8_REB_1,
                   BYBFILE.BYB_UN9_REB_1,
                   BYBFILE.BYB_UN10_REB_1,
                   BYBFILE.BYB_PART_CHGOFF_1,
                   BYBFILE.BYB_ALLOTMENTS_1,
                   BYBFILE.BYB_INS_NO_ELIG_1_1,
                   BYBFILE.BYB_INS_NO_WROTE_1_1,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_1,
                   BYBFILE.BYB_INS_AMT_WROTE_1_1,
                   BYBFILE.BYB_INS_NO_ELIG_1_2,
                   BYBFILE.BYB_INS_NO_WROTE_1_2,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_2,
                   BYBFILE.BYB_INS_AMT_WROTE_1_2,
                   BYBFILE.BYB_INS_NO_ELIG_1_3,
                   BYBFILE.BYB_INS_NO_WROTE_1_3,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_3,
                   BYBFILE.BYB_INS_AMT_WROTE_1_3,
                   BYBFILE.BYB_INS_NO_ELIG_1_4,
                   BYBFILE.BYB_INS_NO_WROTE_1_4,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_4,
                   BYBFILE.BYB_INS_AMT_WROTE_1_4,
                   BYBFILE.BYB_INS_NO_ELIG_1_5,
                   BYBFILE.BYB_INS_NO_WROTE_1_5,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_5,
                   BYBFILE.BYB_INS_AMT_WROTE_1_5,
                   BYBFILE.BYB_INS_NO_ELIG_1_6,
                   BYBFILE.BYB_INS_NO_WROTE_1_6,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_6,
                   BYBFILE.BYB_INS_AMT_WROTE_1_6,
                   BYBFILE.BYB_INS_NO_ELIG_1_7,
                   BYBFILE.BYB_INS_NO_WROTE_1_7,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_7,
                   BYBFILE.BYB_INS_AMT_WROTE_1_7,
                   BYBFILE.BYB_INS_NO_ELIG_1_8,
                   BYBFILE.BYB_INS_NO_WROTE_1_8,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_8,
                   BYBFILE.BYB_INS_AMT_WROTE_1_8,
                   BYBFILE.BYB_INS_NO_ELIG_1_9,
                   BYBFILE.BYB_INS_NO_WROTE_1_9,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_9,
                   BYBFILE.BYB_INS_AMT_WROTE_1_9,
                   BYBFILE.BYB_INS_NO_ELIG_1_10,
                   BYBFILE.BYB_INS_NO_WROTE_1_10,
                   BYBFILE.BYB_INS_NO_NOTWROTE_1_10,
                   BYBFILE.BYB_INS_AMT_WROTE_1_10,
                   BYBFILE.BYB_FEE_NO_ELIG_1_1,
                   BYBFILE.BYB_FEE_NO_WROTE_1_1,
                   BYBFILE.BYB_FEE_NO_NOTWROTE_1_1,
                   BYBFILE.BYB_FEE_AMT_WROTE_1_1,
                   BYBFILE.BYB_FEE_NO_ELIG_1_2,
                   BYBFILE.BYB_FEE_NO_WROTE_1_2,
                   BYBFILE.BYB_FEE_NO_NOTWROTE_1_2,
                   BYBFILE.BYB_FEE_AMT_WROTE_1_2,
                   BYBFILE.BYB_FEE_NO_ELIG_1_3,
                   BYBFILE.BYB_FEE_NO_WROTE_1_3,
                   BYBFILE.BYB_FEE_NO_NOTWROTE_1_3,
                   BYBFILE.BYB_FEE_AMT_WROTE_1_3,
                   BYBFILE.BYB_OTH_NO_COL_1,
                   BYBFILE.BYB_OT2_NO_COL_1,
                   BYBFILE.BYB_INT_NO_COL_1,
                   BYBFILE.BYB_LATE_NO_COL_1,
                   BYBFILE.BYB_OTH_NO_WAIVED_1,
                   BYBFILE.BYB_OT2_NO_WAIVED_1,
                   BYBFILE.BYB_INT_NO_WAIVED_1,
                   BYBFILE.BYB_LATE_NO_WAIVED_1,
                   BYBFILE.BYB_OTH_AMT_WAIVED_1,
                   BYBFILE.BYB_OT2_AMT_WAIVED_1,
                   BYBFILE.BYB_INT_AMT_WAIVED_1,
                   BYBFILE.BYB_LATE_AMT_WAIVED_1,
                   BYBFILE.BYB_PL_PRINCOL_2_1,
                   BYBFILE.BYB_PL_INTCOL_2_1,
                   BYBFILE.BYB_PL_LATECOL_2_1,
                   BYBFILE.BYB_PL_O2COL_2_1,
                   BYBFILE.BYB_PL_PRINCOL_2_2,
                   BYBFILE.BYB_PL_INTCOL_2_2,
                   BYBFILE.BYB_PL_LATECOL_2_2,
                   BYBFILE.BYB_PL_O2COL_2_2,
                   BYBFILE.BYB_PL_PRINCOL_2_3,
                   BYBFILE.BYB_PL_INTCOL_2_3,
                   BYBFILE.BYB_PL_LATECOL_2_3,
                   BYBFILE.BYB_PL_O2COL_2_3,
                   BYBFILE.BYB_PL_PRINCOL_2_4,
                   BYBFILE.BYB_PL_INTCOL_2_4,
                   BYBFILE.BYB_PL_LATECOL_2_4,
                   BYBFILE.BYB_PL_O2COL_2_4,
                   BYBFILE.BYB_PL_PRINCOL_2_5,
                   BYBFILE.BYB_PL_INTCOL_2_5,
                   BYBFILE.BYB_PL_LATECOL_2_5,
                   BYBFILE.BYB_PL_O2COL_2_5,
                   BYBFILE.BYB_AMT_SOLD_2,
                   BYBFILE.BYB_AMT_TRANS_2,
                   BYBFILE.BYB_AMT_PURCH_2,
                   BYBFILE.BYB_NO_SOLD_2,
                   BYBFILE.BYB_NO_TRANS_2,
                   BYBFILE.BYB_NO_PURCH_2,
                   BYBFILE.BYB_OTHCOL_2,
                   BYBFILE.BYB_OT2COL_2,
                   BYBFILE.BYB_INTCOL_2,
                   BYBFILE.BYB_LATECOL_2,
                   BYBFILE.BYB_PRINCOL_2,
                   BYBFILE.BYB_DEFCOL_2,
                   BYBFILE.BYB_RNPRIN_2,
                   BYBFILE.BYB_INTREB_2,
                   BYBFILE.BYB_CLREB1_2,
                   BYBFILE.BYB_CLREB2_2,
                   BYBFILE.BYB_CLREB3_2,
                   BYBFILE.BYB_CLREB4_2,
                   BYBFILE.BYB_AHREB_2,
                   BYBFILE.BYB_PPREB_2,
                   BYBFILE.BYB_O1REB_2,
                   BYBFILE.BYB_O2REB_2,
                   BYBFILE.BYB_O3REB_2,
                   BYBFILE.BYB_O4REB_2,
                   BYBFILE.BYB_O5REB_2,
                   BYBFILE.BYB_O6REB_2,
                   BYBFILE.BYB_O7REB_2,
                   BYBFILE.BYB_SCREB_2,
                   BYBFILE.BYB_MFREB_2,
                   BYBFILE.BYB_DFREB_2,
                   BYBFILE.BYB_UN7_REB_2,
                   BYBFILE.BYB_UN8_REB_2,
                   BYBFILE.BYB_UN9_REB_2,
                   BYBFILE.BYB_UN10_REB_2,
                   BYBFILE.BYB_PART_CHGOFF_2,
                   BYBFILE.BYB_ALLOTMENTS_2,
                   BYBFILE.BYB_INS_NO_ELIG_2_1,
                   BYBFILE.BYB_INS_NO_WROTE_2_1,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_1,
                   BYBFILE.BYB_INS_AMT_WROTE_2_1,
                   BYBFILE.BYB_INS_NO_ELIG_2_2,
                   BYBFILE.BYB_INS_NO_WROTE_2_2,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_2,
                   BYBFILE.BYB_INS_AMT_WROTE_2_2,
                   BYBFILE.BYB_INS_NO_ELIG_2_3,
                   BYBFILE.BYB_INS_NO_WROTE_2_3,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_3,
                   BYBFILE.BYB_INS_AMT_WROTE_2_3,
                   BYBFILE.BYB_INS_NO_ELIG_2_4,
                   BYBFILE.BYB_INS_NO_WROTE_2_4,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_4,
                   BYBFILE.BYB_INS_AMT_WROTE_2_4,
                   BYBFILE.BYB_INS_NO_ELIG_2_5,
                   BYBFILE.BYB_INS_NO_WROTE_2_5,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_5,
                   BYBFILE.BYB_INS_AMT_WROTE_2_5,
                   BYBFILE.BYB_INS_NO_ELIG_2_6,
                   BYBFILE.BYB_INS_NO_WROTE_2_6,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_6,
                   BYBFILE.BYB_INS_AMT_WROTE_2_6,
                   BYBFILE.BYB_INS_NO_ELIG_2_7,
                   BYBFILE.BYB_INS_NO_WROTE_2_7,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_7,
                   BYBFILE.BYB_INS_AMT_WROTE_2_7,
                   BYBFILE.BYB_INS_NO_ELIG_2_8,
                   BYBFILE.BYB_INS_NO_WROTE_2_8,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_8,
                   BYBFILE.BYB_INS_AMT_WROTE_2_8,
                   BYBFILE.BYB_INS_NO_ELIG_2_9,
                   BYBFILE.BYB_INS_NO_WROTE_2_9,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_9,
                   BYBFILE.BYB_INS_AMT_WROTE_2_9,
                   BYBFILE.BYB_INS_NO_ELIG_2_10,
                   BYBFILE.BYB_INS_NO_WROTE_2_10,
                   BYBFILE.BYB_INS_NO_NOTWROTE_2_10,
                   BYBFILE.BYB_INS_AMT_WROTE_2_10,
                   BYBFILE.BYB_FEE_NO_ELIG_2_1,
                   BYBFILE.BYB_FEE_NO_WROTE_2_1,
                   BYBFILE.BYB_FEE_NO_NOTWROTE_2_1,
                   BYBFILE.BYB_FEE_AMT_WROTE_2_1,
                   BYBFILE.BYB_FEE_NO_ELIG_2_2,
                   BYBFILE.BYB_FEE_NO_WROTE_2_2,
                   BYBFILE.BYB_FEE_NO_NOTWROTE_2_2,
                   BYBFILE.BYB_FEE_AMT_WROTE_2_2,
                   BYBFILE.BYB_FEE_NO_ELIG_2_3,
                   BYBFILE.BYB_FEE_NO_WROTE_2_3,
                   BYBFILE.BYB_FEE_NO_NOTWROTE_2_3,
                   BYBFILE.BYB_FEE_AMT_WROTE_2_3,
                   BYBFILE.BYB_OTH_NO_COL_2,
                   BYBFILE.BYB_OT2_NO_COL_2,
                   BYBFILE.BYB_INT_NO_COL_2,
                   BYBFILE.BYB_LATE_NO_COL_2,
                   BYBFILE.BYB_OTH_NO_WAIVED_2,
                   BYBFILE.BYB_OT2_NO_WAIVED_2,
                   BYBFILE.BYB_INT_NO_WAIVED_2,
                   BYBFILE.BYB_LATE_NO_WAIVED_2,
                   BYBFILE.BYB_OTH_AMT_WAIVED_2,
                   BYBFILE.BYB_OT2_AMT_WAIVED_2,
                   BYBFILE.BYB_INT_AMT_WAIVED_2,
                   BYBFILE.BYB_LATE_AMT_WAIVED_2,
                   BYBFILE.BYB_UNITS_1_1_1,
                   BYBFILE.BYB_OUTBAL_1_1_1,
                   BYBFILE.BYB_UNITS_1_1_2,
                   BYBFILE.BYB_OUTBAL_1_1_2,
                   BYBFILE.BYB_UNITS_1_1_3,
                   BYBFILE.BYB_OUTBAL_1_1_3,
                   BYBFILE.BYB_UNITS_1_1_4,
                   BYBFILE.BYB_OUTBAL_1_1_4,
                   BYBFILE.BYB_UNITS_1_1_5,
                   BYBFILE.BYB_OUTBAL_1_1_5,
                   BYBFILE.BYB_UNITS_1_1_6,
                   BYBFILE.BYB_OUTBAL_1_1_6,
                   BYBFILE.BYB_UNITS_1_1_7,
                   BYBFILE.BYB_OUTBAL_1_1_7,
                   BYBFILE.BYB_UNITS_1_1_8,
                   BYBFILE.BYB_OUTBAL_1_1_8,
                   BYBFILE.BYB_UNITS_1_1_9,
                   BYBFILE.BYB_OUTBAL_1_1_9,
                   BYBFILE.BYB_UNITS_1_1_10,
                   BYBFILE.BYB_OUTBAL_1_1_10,
                   BYBFILE.BYB_UNITS_1_2_1,
                   BYBFILE.BYB_OUTBAL_1_2_1,
                   BYBFILE.BYB_UNITS_1_2_2,
                   BYBFILE.BYB_OUTBAL_1_2_2,
                   BYBFILE.BYB_UNITS_1_2_3,
                   BYBFILE.BYB_OUTBAL_1_2_3,
                   BYBFILE.BYB_UNITS_1_2_4,
                   BYBFILE.BYB_OUTBAL_1_2_4,
                   BYBFILE.BYB_UNITS_1_2_5,
                   BYBFILE.BYB_OUTBAL_1_2_5,
                   BYBFILE.BYB_UNITS_1_2_6,
                   BYBFILE.BYB_OUTBAL_1_2_6,
                   BYBFILE.BYB_UNITS_1_2_7,
                   BYBFILE.BYB_OUTBAL_1_2_7,
                   BYBFILE.BYB_UNITS_1_2_8,
                   BYBFILE.BYB_OUTBAL_1_2_8,
                   BYBFILE.BYB_UNITS_1_2_9,
                   BYBFILE.BYB_OUTBAL_1_2_9,
                   BYBFILE.BYB_UNITS_1_2_10,
                   BYBFILE.BYB_OUTBAL_1_2_10,
                   BYBFILE.BYB_UNITS_2_1_1,
                   BYBFILE.BYB_OUTBAL_2_1_1,
                   BYBFILE.BYB_UNITS_2_1_2,
                   BYBFILE.BYB_OUTBAL_2_1_2,
                   BYBFILE.BYB_UNITS_2_1_3,
                   BYBFILE.BYB_OUTBAL_2_1_3,
                   BYBFILE.BYB_UNITS_2_1_4,
                   BYBFILE.BYB_OUTBAL_2_1_4,
                   BYBFILE.BYB_UNITS_2_1_5,
                   BYBFILE.BYB_OUTBAL_2_1_5,
                   BYBFILE.BYB_UNITS_2_1_6,
                   BYBFILE.BYB_OUTBAL_2_1_6,
                   BYBFILE.BYB_UNITS_2_1_7,
                   BYBFILE.BYB_OUTBAL_2_1_7,
                   BYBFILE.BYB_UNITS_2_1_8,
                   BYBFILE.BYB_OUTBAL_2_1_8,
                   BYBFILE.BYB_UNITS_2_1_9,
                   BYBFILE.BYB_OUTBAL_2_1_9,
                   BYBFILE.BYB_UNITS_2_1_10,
                   BYBFILE.BYB_OUTBAL_2_1_10,
                   BYBFILE.BYB_UNITS_2_2_1,
                   BYBFILE.BYB_OUTBAL_2_2_1,
                   BYBFILE.BYB_UNITS_2_2_2,
                   BYBFILE.BYB_OUTBAL_2_2_2,
                   BYBFILE.BYB_UNITS_2_2_3,
                   BYBFILE.BYB_OUTBAL_2_2_3,
                   BYBFILE.BYB_UNITS_2_2_4,
                   BYBFILE.BYB_OUTBAL_2_2_4,
                   BYBFILE.BYB_UNITS_2_2_5,
                   BYBFILE.BYB_OUTBAL_2_2_5,
                   BYBFILE.BYB_UNITS_2_2_6,
                   BYBFILE.BYB_OUTBAL_2_2_6,
                   BYBFILE.BYB_UNITS_2_2_7,
                   BYBFILE.BYB_OUTBAL_2_2_7,
                   BYBFILE.BYB_UNITS_2_2_8,
                   BYBFILE.BYB_OUTBAL_2_2_8,
                   BYBFILE.BYB_UNITS_2_2_9,
                   BYBFILE.BYB_OUTBAL_2_2_9,
                   BYBFILE.BYB_UNITS_2_2_10,
                   BYBFILE.BYB_OUTBAL_2_2_10,
                   BYBFILE.BYB_UNITS_3_1_1,
                   BYBFILE.BYB_OUTBAL_3_1_1,
                   BYBFILE.BYB_UNITS_3_1_2,
                   BYBFILE.BYB_OUTBAL_3_1_2,
                   BYBFILE.BYB_UNITS_3_1_3,
                   BYBFILE.BYB_OUTBAL_3_1_3,
                   BYBFILE.BYB_UNITS_3_1_4,
                   BYBFILE.BYB_OUTBAL_3_1_4,
                   BYBFILE.BYB_UNITS_3_1_5,
                   BYBFILE.BYB_OUTBAL_3_1_5,
                   BYBFILE.BYB_UNITS_3_1_6,
                   BYBFILE.BYB_OUTBAL_3_1_6,
                   BYBFILE.BYB_UNITS_3_1_7,
                   BYBFILE.BYB_OUTBAL_3_1_7,
                   BYBFILE.BYB_UNITS_3_1_8,
                   BYBFILE.BYB_OUTBAL_3_1_8,
                   BYBFILE.BYB_UNITS_3_1_9,
                   BYBFILE.BYB_OUTBAL_3_1_9,
                   BYBFILE.BYB_UNITS_3_1_10,
                   BYBFILE.BYB_OUTBAL_3_1_10,
                   BYBFILE.BYB_UNITS_3_2_1,
                   BYBFILE.BYB_OUTBAL_3_2_1,
                   BYBFILE.BYB_UNITS_3_2_2,
                   BYBFILE.BYB_OUTBAL_3_2_2,
                   BYBFILE.BYB_UNITS_3_2_3,
                   BYBFILE.BYB_OUTBAL_3_2_3,
                   BYBFILE.BYB_UNITS_3_2_4,
                   BYBFILE.BYB_OUTBAL_3_2_4,
                   BYBFILE.BYB_UNITS_3_2_5,
                   BYBFILE.BYB_OUTBAL_3_2_5,
                   BYBFILE.BYB_UNITS_3_2_6,
                   BYBFILE.BYB_OUTBAL_3_2_6,
                   BYBFILE.BYB_UNITS_3_2_7,
                   BYBFILE.BYB_OUTBAL_3_2_7,
                   BYBFILE.BYB_UNITS_3_2_8,
                   BYBFILE.BYB_OUTBAL_3_2_8,
                   BYBFILE.BYB_UNITS_3_2_9,
                   BYBFILE.BYB_OUTBAL_3_2_9,
                   BYBFILE.BYB_UNITS_3_2_10,
                   BYBFILE.BYB_OUTBAL_3_2_10,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_1,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_1,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_2,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_2,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_3,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_3,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_4,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_4,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_5,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_5,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_6,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_6,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_7,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_7,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_8,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_8,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_9,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_9,
                   BYBFILE.BYB_UNITS_BNKRPT_1_1_10,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_1_10,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_1,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_1,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_2,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_2,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_3,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_3,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_4,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_4,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_5,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_5,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_6,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_6,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_7,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_7,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_8,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_8,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_9,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_9,
                   BYBFILE.BYB_UNITS_BNKRPT_1_2_10,
                   BYBFILE.BYB_OUTBAL_BNKRPT_1_2_10,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_1,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_1,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_2,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_2,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_3,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_3,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_4,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_4,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_5,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_5,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_6,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_6,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_7,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_7,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_8,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_8,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_9,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_9,
                   BYBFILE.BYB_UNITS_BNKRPT_2_1_10,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_1_10,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_1,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_1,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_2,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_2,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_3,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_3,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_4,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_4,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_5,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_5,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_6,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_6,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_7,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_7,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_8,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_8,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_9,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_9,
                   BYBFILE.BYB_UNITS_BNKRPT_2_2_10,
                   BYBFILE.BYB_OUTBAL_BNKRPT_2_2_10,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_1,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_1,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_2,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_2,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_3,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_3,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_4,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_4,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_5,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_5,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_6,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_6,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_7,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_7,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_8,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_8,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_9,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_9,
                   BYBFILE.BYB_UNITS_BNKRPT_3_1_10,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_1_10,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_1,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_1,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_2,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_2,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_3,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_3,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_4,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_4,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_5,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_5,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_6,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_6,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_7,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_7,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_8,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_8,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_9,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_9,
                   BYBFILE.BYB_UNITS_BNKRPT_3_2_10,
                   BYBFILE.BYB_OUTBAL_BNKRPT_3_2_10,
                   BYBFILE.BYB_SUMWGTTM,
                   BYBFILE.BYB_SUMWGTAV,
                   BYBFILE.BYB_SUMWGTTA,
                   BYBFILE.BYB_IB_ACCRUED,
                   BYBFILE.BYB_DLDISC_1,
                   BYBFILE.BYB_DLDISC_2,
                   BYBFILE.BYB_DLDISC_3,
                   BYBFILE.BYB_STD_POS,
                   BYBFILE.BYB_30_DAY_MOVED,
                   BYBFILE.BYB_PROJ_INCREASE,
                   BYBFILE.BYB_PROJ_PROFIT,
                   BYBFILE.BYB_PROCEEDS_1,
                   BYBFILE.BYB_PROCEEDS_2,
                   BYBFILE.BYB_LOCKBOX_1,
                   BYBFILE.BYB_LOCKBOX_2,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_1,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_2,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_3,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_4,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_5,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_6,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_7,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_8,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_9,
                   BYBFILE.BYB_UNEARNED_AMT_1_1_10,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_1,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_2,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_3,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_4,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_5,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_6,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_7,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_8,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_9,
                   BYBFILE.BYB_UNEARNED_AMT_1_2_10,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_1,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_2,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_3,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_4,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_5,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_6,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_7,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_8,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_9,
                   BYBFILE.BYB_UNEARNED_AMT_2_1_10,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_1,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_2,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_3,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_4,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_5,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_6,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_7,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_8,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_9,
                   BYBFILE.BYB_UNEARNED_AMT_2_2_10,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_1,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_2,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_3,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_4,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_5,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_6,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_7,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_8,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_9,
                   BYBFILE.BYB_UNEARNED_AMT_3_1_10,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_1,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_2,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_3,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_4,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_5,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_6,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_7,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_8,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_9,
                   BYBFILE.BYB_UNEARNED_AMT_3_2_10
            INTO :QBYB-REC
            FROM DBO.BYBFILE
            WHERE BYBFILE.BYB_DATE_CCYYMM = :QBYB-DATE-CCYYMM
              AND BYBFILE.BYB_BRNO = :QBYB-BRNO
              AND BYBFILE.BYB_CLASS = :QBYB-CLASS
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BYB1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-BYB-FIELDS.

      *-----------------------------------------------------------------
       WRITE-BYB1-FILE.

           PERFORM WRITE-IT.
           MOVE BYB-PATH-SQL TO E-FILE.
           MOVE BYB1-KEY TO E-KEYX.
           PERFORM SET-BYB-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            INSERT INTO DBO.BYBFILE
             (BYBFILE.BYB_DATE_CCYYMM,
              BYBFILE.BYB_BRNO,
              BYBFILE.BYB_CLASS,
              BYBFILE.BYB_PL_PRINCOL_1_1,
              BYBFILE.BYB_PL_INTCOL_1_1,
              BYBFILE.BYB_PL_LATECOL_1_1,
              BYBFILE.BYB_PL_O2COL_1_1,
              BYBFILE.BYB_PL_PRINCOL_1_2,
              BYBFILE.BYB_PL_INTCOL_1_2,
              BYBFILE.BYB_PL_LATECOL_1_2,
              BYBFILE.BYB_PL_O2COL_1_2,
              BYBFILE.BYB_PL_PRINCOL_1_3,
              BYBFILE.BYB_PL_INTCOL_1_3,
              BYBFILE.BYB_PL_LATECOL_1_3,
              BYBFILE.BYB_PL_O2COL_1_3,
              BYBFILE.BYB_PL_PRINCOL_1_4,
              BYBFILE.BYB_PL_INTCOL_1_4,
              BYBFILE.BYB_PL_LATECOL_1_4,
              BYBFILE.BYB_PL_O2COL_1_4,
              BYBFILE.BYB_PL_PRINCOL_1_5,
              BYBFILE.BYB_PL_INTCOL_1_5,
              BYBFILE.BYB_PL_LATECOL_1_5,
              BYBFILE.BYB_PL_O2COL_1_5,
              BYBFILE.BYB_AMT_SOLD_1,
              BYBFILE.BYB_AMT_TRANS_1,
              BYBFILE.BYB_AMT_PURCH_1,
              BYBFILE.BYB_NO_SOLD_1,
              BYBFILE.BYB_NO_TRANS_1,
              BYBFILE.BYB_NO_PURCH_1,
              BYBFILE.BYB_OTHCOL_1,
              BYBFILE.BYB_OT2COL_1,
              BYBFILE.BYB_INTCOL_1,
              BYBFILE.BYB_LATECOL_1,
              BYBFILE.BYB_PRINCOL_1,
              BYBFILE.BYB_DEFCOL_1,
              BYBFILE.BYB_RNPRIN_1,
              BYBFILE.BYB_INTREB_1,
              BYBFILE.BYB_CLREB1_1,
              BYBFILE.BYB_CLREB2_1,
              BYBFILE.BYB_CLREB3_1,
              BYBFILE.BYB_CLREB4_1,
              BYBFILE.BYB_AHREB_1,
              BYBFILE.BYB_PPREB_1,
              BYBFILE.BYB_O1REB_1,
              BYBFILE.BYB_O2REB_1,
              BYBFILE.BYB_O3REB_1,
              BYBFILE.BYB_O4REB_1,
              BYBFILE.BYB_O5REB_1,
              BYBFILE.BYB_O6REB_1,
              BYBFILE.BYB_O7REB_1,
              BYBFILE.BYB_SCREB_1,
              BYBFILE.BYB_MFREB_1,
              BYBFILE.BYB_DFREB_1,
              BYBFILE.BYB_UN7_REB_1,
              BYBFILE.BYB_UN8_REB_1,
              BYBFILE.BYB_UN9_REB_1,
              BYBFILE.BYB_UN10_REB_1,
              BYBFILE.BYB_PART_CHGOFF_1,
              BYBFILE.BYB_ALLOTMENTS_1,
              BYBFILE.BYB_INS_NO_ELIG_1_1,
              BYBFILE.BYB_INS_NO_WROTE_1_1,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_1,
              BYBFILE.BYB_INS_AMT_WROTE_1_1,
              BYBFILE.BYB_INS_NO_ELIG_1_2,
              BYBFILE.BYB_INS_NO_WROTE_1_2,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_2,
              BYBFILE.BYB_INS_AMT_WROTE_1_2,
              BYBFILE.BYB_INS_NO_ELIG_1_3,
              BYBFILE.BYB_INS_NO_WROTE_1_3,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_3,
              BYBFILE.BYB_INS_AMT_WROTE_1_3,
              BYBFILE.BYB_INS_NO_ELIG_1_4,
              BYBFILE.BYB_INS_NO_WROTE_1_4,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_4,
              BYBFILE.BYB_INS_AMT_WROTE_1_4,
              BYBFILE.BYB_INS_NO_ELIG_1_5,
              BYBFILE.BYB_INS_NO_WROTE_1_5,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_5,
              BYBFILE.BYB_INS_AMT_WROTE_1_5,
              BYBFILE.BYB_INS_NO_ELIG_1_6,
              BYBFILE.BYB_INS_NO_WROTE_1_6,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_6,
              BYBFILE.BYB_INS_AMT_WROTE_1_6,
              BYBFILE.BYB_INS_NO_ELIG_1_7,
              BYBFILE.BYB_INS_NO_WROTE_1_7,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_7,
              BYBFILE.BYB_INS_AMT_WROTE_1_7,
              BYBFILE.BYB_INS_NO_ELIG_1_8,
              BYBFILE.BYB_INS_NO_WROTE_1_8,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_8,
              BYBFILE.BYB_INS_AMT_WROTE_1_8,
              BYBFILE.BYB_INS_NO_ELIG_1_9,
              BYBFILE.BYB_INS_NO_WROTE_1_9,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_9,
              BYBFILE.BYB_INS_AMT_WROTE_1_9,
              BYBFILE.BYB_INS_NO_ELIG_1_10,
              BYBFILE.BYB_INS_NO_WROTE_1_10,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_10,
              BYBFILE.BYB_INS_AMT_WROTE_1_10,
              BYBFILE.BYB_FEE_NO_ELIG_1_1,
              BYBFILE.BYB_FEE_NO_WROTE_1_1,
              BYBFILE.BYB_FEE_NO_NOTWROTE_1_1,
              BYBFILE.BYB_FEE_AMT_WROTE_1_1,
              BYBFILE.BYB_FEE_NO_ELIG_1_2,
              BYBFILE.BYB_FEE_NO_WROTE_1_2,
              BYBFILE.BYB_FEE_NO_NOTWROTE_1_2,
              BYBFILE.BYB_FEE_AMT_WROTE_1_2,
              BYBFILE.BYB_FEE_NO_ELIG_1_3,
              BYBFILE.BYB_FEE_NO_WROTE_1_3,
              BYBFILE.BYB_FEE_NO_NOTWROTE_1_3,
              BYBFILE.BYB_FEE_AMT_WROTE_1_3,
              BYBFILE.BYB_OTH_NO_COL_1,
              BYBFILE.BYB_OT2_NO_COL_1,
              BYBFILE.BYB_INT_NO_COL_1,
              BYBFILE.BYB_LATE_NO_COL_1,
              BYBFILE.BYB_OTH_NO_WAIVED_1,
              BYBFILE.BYB_OT2_NO_WAIVED_1,
              BYBFILE.BYB_INT_NO_WAIVED_1,
              BYBFILE.BYB_LATE_NO_WAIVED_1,
              BYBFILE.BYB_OTH_AMT_WAIVED_1,
              BYBFILE.BYB_OT2_AMT_WAIVED_1,
              BYBFILE.BYB_INT_AMT_WAIVED_1,
              BYBFILE.BYB_LATE_AMT_WAIVED_1,
              BYBFILE.BYB_PL_PRINCOL_2_1,
              BYBFILE.BYB_PL_INTCOL_2_1,
              BYBFILE.BYB_PL_LATECOL_2_1,
              BYBFILE.BYB_PL_O2COL_2_1,
              BYBFILE.BYB_PL_PRINCOL_2_2,
              BYBFILE.BYB_PL_INTCOL_2_2,
              BYBFILE.BYB_PL_LATECOL_2_2,
              BYBFILE.BYB_PL_O2COL_2_2,
              BYBFILE.BYB_PL_PRINCOL_2_3,
              BYBFILE.BYB_PL_INTCOL_2_3,
              BYBFILE.BYB_PL_LATECOL_2_3,
              BYBFILE.BYB_PL_O2COL_2_3,
              BYBFILE.BYB_PL_PRINCOL_2_4,
              BYBFILE.BYB_PL_INTCOL_2_4,
              BYBFILE.BYB_PL_LATECOL_2_4,
              BYBFILE.BYB_PL_O2COL_2_4,
              BYBFILE.BYB_PL_PRINCOL_2_5,
              BYBFILE.BYB_PL_INTCOL_2_5,
              BYBFILE.BYB_PL_LATECOL_2_5,
              BYBFILE.BYB_PL_O2COL_2_5,
              BYBFILE.BYB_AMT_SOLD_2,
              BYBFILE.BYB_AMT_TRANS_2,
              BYBFILE.BYB_AMT_PURCH_2,
              BYBFILE.BYB_NO_SOLD_2,
              BYBFILE.BYB_NO_TRANS_2,
              BYBFILE.BYB_NO_PURCH_2,
              BYBFILE.BYB_OTHCOL_2,
              BYBFILE.BYB_OT2COL_2,
              BYBFILE.BYB_INTCOL_2,
              BYBFILE.BYB_LATECOL_2,
              BYBFILE.BYB_PRINCOL_2,
              BYBFILE.BYB_DEFCOL_2,
              BYBFILE.BYB_RNPRIN_2,
              BYBFILE.BYB_INTREB_2,
              BYBFILE.BYB_CLREB1_2,
              BYBFILE.BYB_CLREB2_2,
              BYBFILE.BYB_CLREB3_2,
              BYBFILE.BYB_CLREB4_2,
              BYBFILE.BYB_AHREB_2,
              BYBFILE.BYB_PPREB_2,
              BYBFILE.BYB_O1REB_2,
              BYBFILE.BYB_O2REB_2,
              BYBFILE.BYB_O3REB_2,
              BYBFILE.BYB_O4REB_2,
              BYBFILE.BYB_O5REB_2,
              BYBFILE.BYB_O6REB_2,
              BYBFILE.BYB_O7REB_2,
              BYBFILE.BYB_SCREB_2,
              BYBFILE.BYB_MFREB_2,
              BYBFILE.BYB_DFREB_2,
              BYBFILE.BYB_UN7_REB_2,
              BYBFILE.BYB_UN8_REB_2,
              BYBFILE.BYB_UN9_REB_2,
              BYBFILE.BYB_UN10_REB_2,
              BYBFILE.BYB_PART_CHGOFF_2,
              BYBFILE.BYB_ALLOTMENTS_2,
              BYBFILE.BYB_INS_NO_ELIG_2_1,
              BYBFILE.BYB_INS_NO_WROTE_2_1,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_1,
              BYBFILE.BYB_INS_AMT_WROTE_2_1,
              BYBFILE.BYB_INS_NO_ELIG_2_2,
              BYBFILE.BYB_INS_NO_WROTE_2_2,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_2,
              BYBFILE.BYB_INS_AMT_WROTE_2_2,
              BYBFILE.BYB_INS_NO_ELIG_2_3,
              BYBFILE.BYB_INS_NO_WROTE_2_3,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_3,
              BYBFILE.BYB_INS_AMT_WROTE_2_3,
              BYBFILE.BYB_INS_NO_ELIG_2_4,
              BYBFILE.BYB_INS_NO_WROTE_2_4,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_4,
              BYBFILE.BYB_INS_AMT_WROTE_2_4,
              BYBFILE.BYB_INS_NO_ELIG_2_5,
              BYBFILE.BYB_INS_NO_WROTE_2_5,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_5,
              BYBFILE.BYB_INS_AMT_WROTE_2_5,
              BYBFILE.BYB_INS_NO_ELIG_2_6,
              BYBFILE.BYB_INS_NO_WROTE_2_6,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_6,
              BYBFILE.BYB_INS_AMT_WROTE_2_6,
              BYBFILE.BYB_INS_NO_ELIG_2_7,
              BYBFILE.BYB_INS_NO_WROTE_2_7,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_7,
              BYBFILE.BYB_INS_AMT_WROTE_2_7,
              BYBFILE.BYB_INS_NO_ELIG_2_8,
              BYBFILE.BYB_INS_NO_WROTE_2_8,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_8,
              BYBFILE.BYB_INS_AMT_WROTE_2_8,
              BYBFILE.BYB_INS_NO_ELIG_2_9,
              BYBFILE.BYB_INS_NO_WROTE_2_9,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_9,
              BYBFILE.BYB_INS_AMT_WROTE_2_9,
              BYBFILE.BYB_INS_NO_ELIG_2_10,
              BYBFILE.BYB_INS_NO_WROTE_2_10,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_10,
              BYBFILE.BYB_INS_AMT_WROTE_2_10,
              BYBFILE.BYB_FEE_NO_ELIG_2_1,
              BYBFILE.BYB_FEE_NO_WROTE_2_1,
              BYBFILE.BYB_FEE_NO_NOTWROTE_2_1,
              BYBFILE.BYB_FEE_AMT_WROTE_2_1,
              BYBFILE.BYB_FEE_NO_ELIG_2_2,
              BYBFILE.BYB_FEE_NO_WROTE_2_2,
              BYBFILE.BYB_FEE_NO_NOTWROTE_2_2,
              BYBFILE.BYB_FEE_AMT_WROTE_2_2,
              BYBFILE.BYB_FEE_NO_ELIG_2_3,
              BYBFILE.BYB_FEE_NO_WROTE_2_3,
              BYBFILE.BYB_FEE_NO_NOTWROTE_2_3,
              BYBFILE.BYB_FEE_AMT_WROTE_2_3,
              BYBFILE.BYB_OTH_NO_COL_2,
              BYBFILE.BYB_OT2_NO_COL_2,
              BYBFILE.BYB_INT_NO_COL_2,
              BYBFILE.BYB_LATE_NO_COL_2,
              BYBFILE.BYB_OTH_NO_WAIVED_2,
              BYBFILE.BYB_OT2_NO_WAIVED_2,
              BYBFILE.BYB_INT_NO_WAIVED_2,
              BYBFILE.BYB_LATE_NO_WAIVED_2,
              BYBFILE.BYB_OTH_AMT_WAIVED_2,
              BYBFILE.BYB_OT2_AMT_WAIVED_2,
              BYBFILE.BYB_INT_AMT_WAIVED_2,
              BYBFILE.BYB_LATE_AMT_WAIVED_2,
              BYBFILE.BYB_UNITS_1_1_1,
              BYBFILE.BYB_OUTBAL_1_1_1,
              BYBFILE.BYB_UNITS_1_1_2,
              BYBFILE.BYB_OUTBAL_1_1_2,
              BYBFILE.BYB_UNITS_1_1_3,
              BYBFILE.BYB_OUTBAL_1_1_3,
              BYBFILE.BYB_UNITS_1_1_4,
              BYBFILE.BYB_OUTBAL_1_1_4,
              BYBFILE.BYB_UNITS_1_1_5,
              BYBFILE.BYB_OUTBAL_1_1_5,
              BYBFILE.BYB_UNITS_1_1_6,
              BYBFILE.BYB_OUTBAL_1_1_6,
              BYBFILE.BYB_UNITS_1_1_7,
              BYBFILE.BYB_OUTBAL_1_1_7,
              BYBFILE.BYB_UNITS_1_1_8,
              BYBFILE.BYB_OUTBAL_1_1_8,
              BYBFILE.BYB_UNITS_1_1_9,
              BYBFILE.BYB_OUTBAL_1_1_9,
              BYBFILE.BYB_UNITS_1_1_10,
              BYBFILE.BYB_OUTBAL_1_1_10,
              BYBFILE.BYB_UNITS_1_2_1,
              BYBFILE.BYB_OUTBAL_1_2_1,
              BYBFILE.BYB_UNITS_1_2_2,
              BYBFILE.BYB_OUTBAL_1_2_2,
              BYBFILE.BYB_UNITS_1_2_3,
              BYBFILE.BYB_OUTBAL_1_2_3,
              BYBFILE.BYB_UNITS_1_2_4,
              BYBFILE.BYB_OUTBAL_1_2_4,
              BYBFILE.BYB_UNITS_1_2_5,
              BYBFILE.BYB_OUTBAL_1_2_5,
              BYBFILE.BYB_UNITS_1_2_6,
              BYBFILE.BYB_OUTBAL_1_2_6,
              BYBFILE.BYB_UNITS_1_2_7,
              BYBFILE.BYB_OUTBAL_1_2_7,
              BYBFILE.BYB_UNITS_1_2_8,
              BYBFILE.BYB_OUTBAL_1_2_8,
              BYBFILE.BYB_UNITS_1_2_9,
              BYBFILE.BYB_OUTBAL_1_2_9,
              BYBFILE.BYB_UNITS_1_2_10,
              BYBFILE.BYB_OUTBAL_1_2_10,
              BYBFILE.BYB_UNITS_2_1_1,
              BYBFILE.BYB_OUTBAL_2_1_1,
              BYBFILE.BYB_UNITS_2_1_2,
              BYBFILE.BYB_OUTBAL_2_1_2,
              BYBFILE.BYB_UNITS_2_1_3,
              BYBFILE.BYB_OUTBAL_2_1_3,
              BYBFILE.BYB_UNITS_2_1_4,
              BYBFILE.BYB_OUTBAL_2_1_4,
              BYBFILE.BYB_UNITS_2_1_5,
              BYBFILE.BYB_OUTBAL_2_1_5,
              BYBFILE.BYB_UNITS_2_1_6,
              BYBFILE.BYB_OUTBAL_2_1_6,
              BYBFILE.BYB_UNITS_2_1_7,
              BYBFILE.BYB_OUTBAL_2_1_7,
              BYBFILE.BYB_UNITS_2_1_8,
              BYBFILE.BYB_OUTBAL_2_1_8,
              BYBFILE.BYB_UNITS_2_1_9,
              BYBFILE.BYB_OUTBAL_2_1_9,
              BYBFILE.BYB_UNITS_2_1_10,
              BYBFILE.BYB_OUTBAL_2_1_10,
              BYBFILE.BYB_UNITS_2_2_1,
              BYBFILE.BYB_OUTBAL_2_2_1,
              BYBFILE.BYB_UNITS_2_2_2,
              BYBFILE.BYB_OUTBAL_2_2_2,
              BYBFILE.BYB_UNITS_2_2_3,
              BYBFILE.BYB_OUTBAL_2_2_3,
              BYBFILE.BYB_UNITS_2_2_4,
              BYBFILE.BYB_OUTBAL_2_2_4,
              BYBFILE.BYB_UNITS_2_2_5,
              BYBFILE.BYB_OUTBAL_2_2_5,
              BYBFILE.BYB_UNITS_2_2_6,
              BYBFILE.BYB_OUTBAL_2_2_6,
              BYBFILE.BYB_UNITS_2_2_7,
              BYBFILE.BYB_OUTBAL_2_2_7,
              BYBFILE.BYB_UNITS_2_2_8,
              BYBFILE.BYB_OUTBAL_2_2_8,
              BYBFILE.BYB_UNITS_2_2_9,
              BYBFILE.BYB_OUTBAL_2_2_9,
              BYBFILE.BYB_UNITS_2_2_10,
              BYBFILE.BYB_OUTBAL_2_2_10,
              BYBFILE.BYB_UNITS_3_1_1,
              BYBFILE.BYB_OUTBAL_3_1_1,
              BYBFILE.BYB_UNITS_3_1_2,
              BYBFILE.BYB_OUTBAL_3_1_2,
              BYBFILE.BYB_UNITS_3_1_3,
              BYBFILE.BYB_OUTBAL_3_1_3,
              BYBFILE.BYB_UNITS_3_1_4,
              BYBFILE.BYB_OUTBAL_3_1_4,
              BYBFILE.BYB_UNITS_3_1_5,
              BYBFILE.BYB_OUTBAL_3_1_5,
              BYBFILE.BYB_UNITS_3_1_6,
              BYBFILE.BYB_OUTBAL_3_1_6,
              BYBFILE.BYB_UNITS_3_1_7,
              BYBFILE.BYB_OUTBAL_3_1_7,
              BYBFILE.BYB_UNITS_3_1_8,
              BYBFILE.BYB_OUTBAL_3_1_8,
              BYBFILE.BYB_UNITS_3_1_9,
              BYBFILE.BYB_OUTBAL_3_1_9,
              BYBFILE.BYB_UNITS_3_1_10,
              BYBFILE.BYB_OUTBAL_3_1_10,
              BYBFILE.BYB_UNITS_3_2_1,
              BYBFILE.BYB_OUTBAL_3_2_1,
              BYBFILE.BYB_UNITS_3_2_2,
              BYBFILE.BYB_OUTBAL_3_2_2,
              BYBFILE.BYB_UNITS_3_2_3,
              BYBFILE.BYB_OUTBAL_3_2_3,
              BYBFILE.BYB_UNITS_3_2_4,
              BYBFILE.BYB_OUTBAL_3_2_4,
              BYBFILE.BYB_UNITS_3_2_5,
              BYBFILE.BYB_OUTBAL_3_2_5,
              BYBFILE.BYB_UNITS_3_2_6,
              BYBFILE.BYB_OUTBAL_3_2_6,
              BYBFILE.BYB_UNITS_3_2_7,
              BYBFILE.BYB_OUTBAL_3_2_7,
              BYBFILE.BYB_UNITS_3_2_8,
              BYBFILE.BYB_OUTBAL_3_2_8,
              BYBFILE.BYB_UNITS_3_2_9,
              BYBFILE.BYB_OUTBAL_3_2_9,
              BYBFILE.BYB_UNITS_3_2_10,
              BYBFILE.BYB_OUTBAL_3_2_10,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_1,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_1,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_2,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_2,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_3,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_3,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_4,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_4,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_5,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_5,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_6,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_6,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_7,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_7,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_8,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_8,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_9,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_9,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_10,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_10,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_1,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_1,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_2,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_2,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_3,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_3,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_4,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_4,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_5,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_5,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_6,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_6,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_7,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_7,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_8,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_8,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_9,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_9,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_10,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_10,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_1,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_1,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_2,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_2,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_3,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_3,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_4,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_4,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_5,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_5,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_6,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_6,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_7,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_7,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_8,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_8,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_9,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_9,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_10,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_10,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_1,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_1,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_2,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_2,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_3,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_3,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_4,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_4,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_5,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_5,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_6,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_6,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_7,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_7,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_8,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_8,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_9,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_9,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_10,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_10,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_1,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_1,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_2,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_2,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_3,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_3,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_4,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_4,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_5,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_5,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_6,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_6,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_7,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_7,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_8,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_8,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_9,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_9,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_10,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_10,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_1,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_1,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_2,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_2,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_3,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_3,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_4,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_4,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_5,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_5,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_6,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_6,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_7,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_7,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_8,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_8,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_9,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_9,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_10,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_10,
              BYBFILE.BYB_SUMWGTTM,
              BYBFILE.BYB_SUMWGTAV,
              BYBFILE.BYB_SUMWGTTA,
              BYBFILE.BYB_IB_ACCRUED,
              BYBFILE.BYB_DLDISC_1,
              BYBFILE.BYB_DLDISC_2,
              BYBFILE.BYB_DLDISC_3,
              BYBFILE.BYB_STD_POS,
              BYBFILE.BYB_30_DAY_MOVED,
              BYBFILE.BYB_PROJ_INCREASE,
              BYBFILE.BYB_PROJ_PROFIT,
              BYBFILE.BYB_PROCEEDS_1,
              BYBFILE.BYB_PROCEEDS_2,
              BYBFILE.BYB_LOCKBOX_1,
              BYBFILE.BYB_LOCKBOX_2,
              BYBFILE.BYB_UNEARNED_AMT_1_1_1,
              BYBFILE.BYB_UNEARNED_AMT_1_1_2,
              BYBFILE.BYB_UNEARNED_AMT_1_1_3,
              BYBFILE.BYB_UNEARNED_AMT_1_1_4,
              BYBFILE.BYB_UNEARNED_AMT_1_1_5,
              BYBFILE.BYB_UNEARNED_AMT_1_1_6,
              BYBFILE.BYB_UNEARNED_AMT_1_1_7,
              BYBFILE.BYB_UNEARNED_AMT_1_1_8,
              BYBFILE.BYB_UNEARNED_AMT_1_1_9,
              BYBFILE.BYB_UNEARNED_AMT_1_1_10,
              BYBFILE.BYB_UNEARNED_AMT_1_2_1,
              BYBFILE.BYB_UNEARNED_AMT_1_2_2,
              BYBFILE.BYB_UNEARNED_AMT_1_2_3,
              BYBFILE.BYB_UNEARNED_AMT_1_2_4,
              BYBFILE.BYB_UNEARNED_AMT_1_2_5,
              BYBFILE.BYB_UNEARNED_AMT_1_2_6,
              BYBFILE.BYB_UNEARNED_AMT_1_2_7,
              BYBFILE.BYB_UNEARNED_AMT_1_2_8,
              BYBFILE.BYB_UNEARNED_AMT_1_2_9,
              BYBFILE.BYB_UNEARNED_AMT_1_2_10,
              BYBFILE.BYB_UNEARNED_AMT_2_1_1,
              BYBFILE.BYB_UNEARNED_AMT_2_1_2,
              BYBFILE.BYB_UNEARNED_AMT_2_1_3,
              BYBFILE.BYB_UNEARNED_AMT_2_1_4,
              BYBFILE.BYB_UNEARNED_AMT_2_1_5,
              BYBFILE.BYB_UNEARNED_AMT_2_1_6,
              BYBFILE.BYB_UNEARNED_AMT_2_1_7,
              BYBFILE.BYB_UNEARNED_AMT_2_1_8,
              BYBFILE.BYB_UNEARNED_AMT_2_1_9,
              BYBFILE.BYB_UNEARNED_AMT_2_1_10,
              BYBFILE.BYB_UNEARNED_AMT_2_2_1,
              BYBFILE.BYB_UNEARNED_AMT_2_2_2,
              BYBFILE.BYB_UNEARNED_AMT_2_2_3,
              BYBFILE.BYB_UNEARNED_AMT_2_2_4,
              BYBFILE.BYB_UNEARNED_AMT_2_2_5,
              BYBFILE.BYB_UNEARNED_AMT_2_2_6,
              BYBFILE.BYB_UNEARNED_AMT_2_2_7,
              BYBFILE.BYB_UNEARNED_AMT_2_2_8,
              BYBFILE.BYB_UNEARNED_AMT_2_2_9,
              BYBFILE.BYB_UNEARNED_AMT_2_2_10,
              BYBFILE.BYB_UNEARNED_AMT_3_1_1,
              BYBFILE.BYB_UNEARNED_AMT_3_1_2,
              BYBFILE.BYB_UNEARNED_AMT_3_1_3,
              BYBFILE.BYB_UNEARNED_AMT_3_1_4,
              BYBFILE.BYB_UNEARNED_AMT_3_1_5,
              BYBFILE.BYB_UNEARNED_AMT_3_1_6,
              BYBFILE.BYB_UNEARNED_AMT_3_1_7,
              BYBFILE.BYB_UNEARNED_AMT_3_1_8,
              BYBFILE.BYB_UNEARNED_AMT_3_1_9,
              BYBFILE.BYB_UNEARNED_AMT_3_1_10,
              BYBFILE.BYB_UNEARNED_AMT_3_2_1,
              BYBFILE.BYB_UNEARNED_AMT_3_2_2,
              BYBFILE.BYB_UNEARNED_AMT_3_2_3,
              BYBFILE.BYB_UNEARNED_AMT_3_2_4,
              BYBFILE.BYB_UNEARNED_AMT_3_2_5,
              BYBFILE.BYB_UNEARNED_AMT_3_2_6,
              BYBFILE.BYB_UNEARNED_AMT_3_2_7,
              BYBFILE.BYB_UNEARNED_AMT_3_2_8,
              BYBFILE.BYB_UNEARNED_AMT_3_2_9,
              BYBFILE.BYB_UNEARNED_AMT_3_2_10)
            VALUES
             (:QBYB-DATE-CCYYMM,
              :QBYB-BRNO,
              :QBYB-CLASS,
              :QBYB-PL-PRINCOL-1-1,
              :QBYB-PL-INTCOL-1-1,
              :QBYB-PL-LATECOL-1-1,
              :QBYB-PL-O2COL-1-1,
              :QBYB-PL-PRINCOL-1-2,
              :QBYB-PL-INTCOL-1-2,
              :QBYB-PL-LATECOL-1-2,
              :QBYB-PL-O2COL-1-2,
              :QBYB-PL-PRINCOL-1-3,
              :QBYB-PL-INTCOL-1-3,
              :QBYB-PL-LATECOL-1-3,
              :QBYB-PL-O2COL-1-3,
              :QBYB-PL-PRINCOL-1-4,
              :QBYB-PL-INTCOL-1-4,
              :QBYB-PL-LATECOL-1-4,
              :QBYB-PL-O2COL-1-4,
              :QBYB-PL-PRINCOL-1-5,
              :QBYB-PL-INTCOL-1-5,
              :QBYB-PL-LATECOL-1-5,
              :QBYB-PL-O2COL-1-5,
              :QBYB-AMT-SOLD-1,
              :QBYB-AMT-TRANS-1,
              :QBYB-AMT-PURCH-1,
              :QBYB-NO-SOLD-1,
              :QBYB-NO-TRANS-1,
              :QBYB-NO-PURCH-1,
              :QBYB-OTHCOL-1,
              :QBYB-OT2COL-1,
              :QBYB-INTCOL-1,
              :QBYB-LATECOL-1,
              :QBYB-PRINCOL-1,
              :QBYB-DEFCOL-1,
              :QBYB-RNPRIN-1,
              :QBYB-INTREB-1,
              :QBYB-CLREB1-1,
              :QBYB-CLREB2-1,
              :QBYB-CLREB3-1,
              :QBYB-CLREB4-1,
              :QBYB-AHREB-1,
              :QBYB-PPREB-1,
              :QBYB-O1REB-1,
              :QBYB-O2REB-1,
              :QBYB-O3REB-1,
              :QBYB-O4REB-1,
              :QBYB-O5REB-1,
              :QBYB-O6REB-1,
              :QBYB-O7REB-1,
              :QBYB-SCREB-1,
              :QBYB-MFREB-1,
              :QBYB-DFREB-1,
              :QBYB-UN7-REB-1,
              :QBYB-UN8-REB-1,
              :QBYB-UN9-REB-1,
              :QBYB-UN10-REB-1,
              :QBYB-PART-CHGOFF-1,
              :QBYB-ALLOTMENTS-1,
              :QBYB-INS-NO-ELIG-1-1,
              :QBYB-INS-NO-WROTE-1-1,
              :QBYB-INS-NO-NOTWROTE-1-1,
              :QBYB-INS-AMT-WROTE-1-1,
              :QBYB-INS-NO-ELIG-1-2,
              :QBYB-INS-NO-WROTE-1-2,
              :QBYB-INS-NO-NOTWROTE-1-2,
              :QBYB-INS-AMT-WROTE-1-2,
              :QBYB-INS-NO-ELIG-1-3,
              :QBYB-INS-NO-WROTE-1-3,
              :QBYB-INS-NO-NOTWROTE-1-3,
              :QBYB-INS-AMT-WROTE-1-3,
              :QBYB-INS-NO-ELIG-1-4,
              :QBYB-INS-NO-WROTE-1-4,
              :QBYB-INS-NO-NOTWROTE-1-4,
              :QBYB-INS-AMT-WROTE-1-4,
              :QBYB-INS-NO-ELIG-1-5,
              :QBYB-INS-NO-WROTE-1-5,
              :QBYB-INS-NO-NOTWROTE-1-5,
              :QBYB-INS-AMT-WROTE-1-5,
              :QBYB-INS-NO-ELIG-1-6,
              :QBYB-INS-NO-WROTE-1-6,
              :QBYB-INS-NO-NOTWROTE-1-6,
              :QBYB-INS-AMT-WROTE-1-6,
              :QBYB-INS-NO-ELIG-1-7,
              :QBYB-INS-NO-WROTE-1-7,
              :QBYB-INS-NO-NOTWROTE-1-7,
              :QBYB-INS-AMT-WROTE-1-7,
              :QBYB-INS-NO-ELIG-1-8,
              :QBYB-INS-NO-WROTE-1-8,
              :QBYB-INS-NO-NOTWROTE-1-8,
              :QBYB-INS-AMT-WROTE-1-8,
              :QBYB-INS-NO-ELIG-1-9,
              :QBYB-INS-NO-WROTE-1-9,
              :QBYB-INS-NO-NOTWROTE-1-9,
              :QBYB-INS-AMT-WROTE-1-9,
              :QBYB-INS-NO-ELIG-1-10,
              :QBYB-INS-NO-WROTE-1-10,
              :QBYB-INS-NO-NOTWROTE-1-10,
              :QBYB-INS-AMT-WROTE-1-10,
              :QBYB-FEE-NO-ELIG-1-1,
              :QBYB-FEE-NO-WROTE-1-1,
              :QBYB-FEE-NO-NOTWROTE-1-1,
              :QBYB-FEE-AMT-WROTE-1-1,
              :QBYB-FEE-NO-ELIG-1-2,
              :QBYB-FEE-NO-WROTE-1-2,
              :QBYB-FEE-NO-NOTWROTE-1-2,
              :QBYB-FEE-AMT-WROTE-1-2,
              :QBYB-FEE-NO-ELIG-1-3,
              :QBYB-FEE-NO-WROTE-1-3,
              :QBYB-FEE-NO-NOTWROTE-1-3,
              :QBYB-FEE-AMT-WROTE-1-3,
              :QBYB-OTH-NO-COL-1,
              :QBYB-OT2-NO-COL-1,
              :QBYB-INT-NO-COL-1,
              :QBYB-LATE-NO-COL-1,
              :QBYB-OTH-NO-WAIVED-1,
              :QBYB-OT2-NO-WAIVED-1,
              :QBYB-INT-NO-WAIVED-1,
              :QBYB-LATE-NO-WAIVED-1,
              :QBYB-OTH-AMT-WAIVED-1,
              :QBYB-OT2-AMT-WAIVED-1,
              :QBYB-INT-AMT-WAIVED-1,
              :QBYB-LATE-AMT-WAIVED-1,
              :QBYB-PL-PRINCOL-2-1,
              :QBYB-PL-INTCOL-2-1,
              :QBYB-PL-LATECOL-2-1,
              :QBYB-PL-O2COL-2-1,
              :QBYB-PL-PRINCOL-2-2,
              :QBYB-PL-INTCOL-2-2,
              :QBYB-PL-LATECOL-2-2,
              :QBYB-PL-O2COL-2-2,
              :QBYB-PL-PRINCOL-2-3,
              :QBYB-PL-INTCOL-2-3,
              :QBYB-PL-LATECOL-2-3,
              :QBYB-PL-O2COL-2-3,
              :QBYB-PL-PRINCOL-2-4,
              :QBYB-PL-INTCOL-2-4,
              :QBYB-PL-LATECOL-2-4,
              :QBYB-PL-O2COL-2-4,
              :QBYB-PL-PRINCOL-2-5,
              :QBYB-PL-INTCOL-2-5,
              :QBYB-PL-LATECOL-2-5,
              :QBYB-PL-O2COL-2-5,
              :QBYB-AMT-SOLD-2,
              :QBYB-AMT-TRANS-2,
              :QBYB-AMT-PURCH-2,
              :QBYB-NO-SOLD-2,
              :QBYB-NO-TRANS-2,
              :QBYB-NO-PURCH-2,
              :QBYB-OTHCOL-2,
              :QBYB-OT2COL-2,
              :QBYB-INTCOL-2,
              :QBYB-LATECOL-2,
              :QBYB-PRINCOL-2,
              :QBYB-DEFCOL-2,
              :QBYB-RNPRIN-2,
              :QBYB-INTREB-2,
              :QBYB-CLREB1-2,
              :QBYB-CLREB2-2,
              :QBYB-CLREB3-2,
              :QBYB-CLREB4-2,
              :QBYB-AHREB-2,
              :QBYB-PPREB-2,
              :QBYB-O1REB-2,
              :QBYB-O2REB-2,
              :QBYB-O3REB-2,
              :QBYB-O4REB-2,
              :QBYB-O5REB-2,
              :QBYB-O6REB-2,
              :QBYB-O7REB-2,
              :QBYB-SCREB-2,
              :QBYB-MFREB-2,
              :QBYB-DFREB-2,
              :QBYB-UN7-REB-2,
              :QBYB-UN8-REB-2,
              :QBYB-UN9-REB-2,
              :QBYB-UN10-REB-2,
              :QBYB-PART-CHGOFF-2,
              :QBYB-ALLOTMENTS-2,
              :QBYB-INS-NO-ELIG-2-1,
              :QBYB-INS-NO-WROTE-2-1,
              :QBYB-INS-NO-NOTWROTE-2-1,
              :QBYB-INS-AMT-WROTE-2-1,
              :QBYB-INS-NO-ELIG-2-2,
              :QBYB-INS-NO-WROTE-2-2,
              :QBYB-INS-NO-NOTWROTE-2-2,
              :QBYB-INS-AMT-WROTE-2-2,
              :QBYB-INS-NO-ELIG-2-3,
              :QBYB-INS-NO-WROTE-2-3,
              :QBYB-INS-NO-NOTWROTE-2-3,
              :QBYB-INS-AMT-WROTE-2-3,
              :QBYB-INS-NO-ELIG-2-4,
              :QBYB-INS-NO-WROTE-2-4,
              :QBYB-INS-NO-NOTWROTE-2-4,
              :QBYB-INS-AMT-WROTE-2-4,
              :QBYB-INS-NO-ELIG-2-5,
              :QBYB-INS-NO-WROTE-2-5,
              :QBYB-INS-NO-NOTWROTE-2-5,
              :QBYB-INS-AMT-WROTE-2-5,
              :QBYB-INS-NO-ELIG-2-6,
              :QBYB-INS-NO-WROTE-2-6,
              :QBYB-INS-NO-NOTWROTE-2-6,
              :QBYB-INS-AMT-WROTE-2-6,
              :QBYB-INS-NO-ELIG-2-7,
              :QBYB-INS-NO-WROTE-2-7,
              :QBYB-INS-NO-NOTWROTE-2-7,
              :QBYB-INS-AMT-WROTE-2-7,
              :QBYB-INS-NO-ELIG-2-8,
              :QBYB-INS-NO-WROTE-2-8,
              :QBYB-INS-NO-NOTWROTE-2-8,
              :QBYB-INS-AMT-WROTE-2-8,
              :QBYB-INS-NO-ELIG-2-9,
              :QBYB-INS-NO-WROTE-2-9,
              :QBYB-INS-NO-NOTWROTE-2-9,
              :QBYB-INS-AMT-WROTE-2-9,
              :QBYB-INS-NO-ELIG-2-10,
              :QBYB-INS-NO-WROTE-2-10,
              :QBYB-INS-NO-NOTWROTE-2-10,
              :QBYB-INS-AMT-WROTE-2-10,
              :QBYB-FEE-NO-ELIG-2-1,
              :QBYB-FEE-NO-WROTE-2-1,
              :QBYB-FEE-NO-NOTWROTE-2-1,
              :QBYB-FEE-AMT-WROTE-2-1,
              :QBYB-FEE-NO-ELIG-2-2,
              :QBYB-FEE-NO-WROTE-2-2,
              :QBYB-FEE-NO-NOTWROTE-2-2,
              :QBYB-FEE-AMT-WROTE-2-2,
              :QBYB-FEE-NO-ELIG-2-3,
              :QBYB-FEE-NO-WROTE-2-3,
              :QBYB-FEE-NO-NOTWROTE-2-3,
              :QBYB-FEE-AMT-WROTE-2-3,
              :QBYB-OTH-NO-COL-2,
              :QBYB-OT2-NO-COL-2,
              :QBYB-INT-NO-COL-2,
              :QBYB-LATE-NO-COL-2,
              :QBYB-OTH-NO-WAIVED-2,
              :QBYB-OT2-NO-WAIVED-2,
              :QBYB-INT-NO-WAIVED-2,
              :QBYB-LATE-NO-WAIVED-2,
              :QBYB-OTH-AMT-WAIVED-2,
              :QBYB-OT2-AMT-WAIVED-2,
              :QBYB-INT-AMT-WAIVED-2,
              :QBYB-LATE-AMT-WAIVED-2,
              :QBYB-UNITS-1-1-1,
              :QBYB-OUTBAL-1-1-1,
              :QBYB-UNITS-1-1-2,
              :QBYB-OUTBAL-1-1-2,
              :QBYB-UNITS-1-1-3,
              :QBYB-OUTBAL-1-1-3,
              :QBYB-UNITS-1-1-4,
              :QBYB-OUTBAL-1-1-4,
              :QBYB-UNITS-1-1-5,
              :QBYB-OUTBAL-1-1-5,
              :QBYB-UNITS-1-1-6,
              :QBYB-OUTBAL-1-1-6,
              :QBYB-UNITS-1-1-7,
              :QBYB-OUTBAL-1-1-7,
              :QBYB-UNITS-1-1-8,
              :QBYB-OUTBAL-1-1-8,
              :QBYB-UNITS-1-1-9,
              :QBYB-OUTBAL-1-1-9,
              :QBYB-UNITS-1-1-10,
              :QBYB-OUTBAL-1-1-10,
              :QBYB-UNITS-1-2-1,
              :QBYB-OUTBAL-1-2-1,
              :QBYB-UNITS-1-2-2,
              :QBYB-OUTBAL-1-2-2,
              :QBYB-UNITS-1-2-3,
              :QBYB-OUTBAL-1-2-3,
              :QBYB-UNITS-1-2-4,
              :QBYB-OUTBAL-1-2-4,
              :QBYB-UNITS-1-2-5,
              :QBYB-OUTBAL-1-2-5,
              :QBYB-UNITS-1-2-6,
              :QBYB-OUTBAL-1-2-6,
              :QBYB-UNITS-1-2-7,
              :QBYB-OUTBAL-1-2-7,
              :QBYB-UNITS-1-2-8,
              :QBYB-OUTBAL-1-2-8,
              :QBYB-UNITS-1-2-9,
              :QBYB-OUTBAL-1-2-9,
              :QBYB-UNITS-1-2-10,
              :QBYB-OUTBAL-1-2-10,
              :QBYB-UNITS-2-1-1,
              :QBYB-OUTBAL-2-1-1,
              :QBYB-UNITS-2-1-2,
              :QBYB-OUTBAL-2-1-2,
              :QBYB-UNITS-2-1-3,
              :QBYB-OUTBAL-2-1-3,
              :QBYB-UNITS-2-1-4,
              :QBYB-OUTBAL-2-1-4,
              :QBYB-UNITS-2-1-5,
              :QBYB-OUTBAL-2-1-5,
              :QBYB-UNITS-2-1-6,
              :QBYB-OUTBAL-2-1-6,
              :QBYB-UNITS-2-1-7,
              :QBYB-OUTBAL-2-1-7,
              :QBYB-UNITS-2-1-8,
              :QBYB-OUTBAL-2-1-8,
              :QBYB-UNITS-2-1-9,
              :QBYB-OUTBAL-2-1-9,
              :QBYB-UNITS-2-1-10,
              :QBYB-OUTBAL-2-1-10,
              :QBYB-UNITS-2-2-1,
              :QBYB-OUTBAL-2-2-1,
              :QBYB-UNITS-2-2-2,
              :QBYB-OUTBAL-2-2-2,
              :QBYB-UNITS-2-2-3,
              :QBYB-OUTBAL-2-2-3,
              :QBYB-UNITS-2-2-4,
              :QBYB-OUTBAL-2-2-4,
              :QBYB-UNITS-2-2-5,
              :QBYB-OUTBAL-2-2-5,
              :QBYB-UNITS-2-2-6,
              :QBYB-OUTBAL-2-2-6,
              :QBYB-UNITS-2-2-7,
              :QBYB-OUTBAL-2-2-7,
              :QBYB-UNITS-2-2-8,
              :QBYB-OUTBAL-2-2-8,
              :QBYB-UNITS-2-2-9,
              :QBYB-OUTBAL-2-2-9,
              :QBYB-UNITS-2-2-10,
              :QBYB-OUTBAL-2-2-10,
              :QBYB-UNITS-3-1-1,
              :QBYB-OUTBAL-3-1-1,
              :QBYB-UNITS-3-1-2,
              :QBYB-OUTBAL-3-1-2,
              :QBYB-UNITS-3-1-3,
              :QBYB-OUTBAL-3-1-3,
              :QBYB-UNITS-3-1-4,
              :QBYB-OUTBAL-3-1-4,
              :QBYB-UNITS-3-1-5,
              :QBYB-OUTBAL-3-1-5,
              :QBYB-UNITS-3-1-6,
              :QBYB-OUTBAL-3-1-6,
              :QBYB-UNITS-3-1-7,
              :QBYB-OUTBAL-3-1-7,
              :QBYB-UNITS-3-1-8,
              :QBYB-OUTBAL-3-1-8,
              :QBYB-UNITS-3-1-9,
              :QBYB-OUTBAL-3-1-9,
              :QBYB-UNITS-3-1-10,
              :QBYB-OUTBAL-3-1-10,
              :QBYB-UNITS-3-2-1,
              :QBYB-OUTBAL-3-2-1,
              :QBYB-UNITS-3-2-2,
              :QBYB-OUTBAL-3-2-2,
              :QBYB-UNITS-3-2-3,
              :QBYB-OUTBAL-3-2-3,
              :QBYB-UNITS-3-2-4,
              :QBYB-OUTBAL-3-2-4,
              :QBYB-UNITS-3-2-5,
              :QBYB-OUTBAL-3-2-5,
              :QBYB-UNITS-3-2-6,
              :QBYB-OUTBAL-3-2-6,
              :QBYB-UNITS-3-2-7,
              :QBYB-OUTBAL-3-2-7,
              :QBYB-UNITS-3-2-8,
              :QBYB-OUTBAL-3-2-8,
              :QBYB-UNITS-3-2-9,
              :QBYB-OUTBAL-3-2-9,
              :QBYB-UNITS-3-2-10,
              :QBYB-OUTBAL-3-2-10,
              :QBYB-UNITS-BNKRPT-1-1-1,
              :QBYB-OUTBAL-BNKRPT-1-1-1,
              :QBYB-UNITS-BNKRPT-1-1-2,
              :QBYB-OUTBAL-BNKRPT-1-1-2,
              :QBYB-UNITS-BNKRPT-1-1-3,
              :QBYB-OUTBAL-BNKRPT-1-1-3,
              :QBYB-UNITS-BNKRPT-1-1-4,
              :QBYB-OUTBAL-BNKRPT-1-1-4,
              :QBYB-UNITS-BNKRPT-1-1-5,
              :QBYB-OUTBAL-BNKRPT-1-1-5,
              :QBYB-UNITS-BNKRPT-1-1-6,
              :QBYB-OUTBAL-BNKRPT-1-1-6,
              :QBYB-UNITS-BNKRPT-1-1-7,
              :QBYB-OUTBAL-BNKRPT-1-1-7,
              :QBYB-UNITS-BNKRPT-1-1-8,
              :QBYB-OUTBAL-BNKRPT-1-1-8,
              :QBYB-UNITS-BNKRPT-1-1-9,
              :QBYB-OUTBAL-BNKRPT-1-1-9,
              :QBYB-UNITS-BNKRPT-1-1-10,
              :QBYB-OUTBAL-BNKRPT-1-1-10,
              :QBYB-UNITS-BNKRPT-1-2-1,
              :QBYB-OUTBAL-BNKRPT-1-2-1,
              :QBYB-UNITS-BNKRPT-1-2-2,
              :QBYB-OUTBAL-BNKRPT-1-2-2,
              :QBYB-UNITS-BNKRPT-1-2-3,
              :QBYB-OUTBAL-BNKRPT-1-2-3,
              :QBYB-UNITS-BNKRPT-1-2-4,
              :QBYB-OUTBAL-BNKRPT-1-2-4,
              :QBYB-UNITS-BNKRPT-1-2-5,
              :QBYB-OUTBAL-BNKRPT-1-2-5,
              :QBYB-UNITS-BNKRPT-1-2-6,
              :QBYB-OUTBAL-BNKRPT-1-2-6,
              :QBYB-UNITS-BNKRPT-1-2-7,
              :QBYB-OUTBAL-BNKRPT-1-2-7,
              :QBYB-UNITS-BNKRPT-1-2-8,
              :QBYB-OUTBAL-BNKRPT-1-2-8,
              :QBYB-UNITS-BNKRPT-1-2-9,
              :QBYB-OUTBAL-BNKRPT-1-2-9,
              :QBYB-UNITS-BNKRPT-1-2-10,
              :QBYB-OUTBAL-BNKRPT-1-2-10,
              :QBYB-UNITS-BNKRPT-2-1-1,
              :QBYB-OUTBAL-BNKRPT-2-1-1,
              :QBYB-UNITS-BNKRPT-2-1-2,
              :QBYB-OUTBAL-BNKRPT-2-1-2,
              :QBYB-UNITS-BNKRPT-2-1-3,
              :QBYB-OUTBAL-BNKRPT-2-1-3,
              :QBYB-UNITS-BNKRPT-2-1-4,
              :QBYB-OUTBAL-BNKRPT-2-1-4,
              :QBYB-UNITS-BNKRPT-2-1-5,
              :QBYB-OUTBAL-BNKRPT-2-1-5,
              :QBYB-UNITS-BNKRPT-2-1-6,
              :QBYB-OUTBAL-BNKRPT-2-1-6,
              :QBYB-UNITS-BNKRPT-2-1-7,
              :QBYB-OUTBAL-BNKRPT-2-1-7,
              :QBYB-UNITS-BNKRPT-2-1-8,
              :QBYB-OUTBAL-BNKRPT-2-1-8,
              :QBYB-UNITS-BNKRPT-2-1-9,
              :QBYB-OUTBAL-BNKRPT-2-1-9,
              :QBYB-UNITS-BNKRPT-2-1-10,
              :QBYB-OUTBAL-BNKRPT-2-1-10,
              :QBYB-UNITS-BNKRPT-2-2-1,
              :QBYB-OUTBAL-BNKRPT-2-2-1,
              :QBYB-UNITS-BNKRPT-2-2-2,
              :QBYB-OUTBAL-BNKRPT-2-2-2,
              :QBYB-UNITS-BNKRPT-2-2-3,
              :QBYB-OUTBAL-BNKRPT-2-2-3,
              :QBYB-UNITS-BNKRPT-2-2-4,
              :QBYB-OUTBAL-BNKRPT-2-2-4,
              :QBYB-UNITS-BNKRPT-2-2-5,
              :QBYB-OUTBAL-BNKRPT-2-2-5,
              :QBYB-UNITS-BNKRPT-2-2-6,
              :QBYB-OUTBAL-BNKRPT-2-2-6,
              :QBYB-UNITS-BNKRPT-2-2-7,
              :QBYB-OUTBAL-BNKRPT-2-2-7,
              :QBYB-UNITS-BNKRPT-2-2-8,
              :QBYB-OUTBAL-BNKRPT-2-2-8,
              :QBYB-UNITS-BNKRPT-2-2-9,
              :QBYB-OUTBAL-BNKRPT-2-2-9,
              :QBYB-UNITS-BNKRPT-2-2-10,
              :QBYB-OUTBAL-BNKRPT-2-2-10,
              :QBYB-UNITS-BNKRPT-3-1-1,
              :QBYB-OUTBAL-BNKRPT-3-1-1,
              :QBYB-UNITS-BNKRPT-3-1-2,
              :QBYB-OUTBAL-BNKRPT-3-1-2,
              :QBYB-UNITS-BNKRPT-3-1-3,
              :QBYB-OUTBAL-BNKRPT-3-1-3,
              :QBYB-UNITS-BNKRPT-3-1-4,
              :QBYB-OUTBAL-BNKRPT-3-1-4,
              :QBYB-UNITS-BNKRPT-3-1-5,
              :QBYB-OUTBAL-BNKRPT-3-1-5,
              :QBYB-UNITS-BNKRPT-3-1-6,
              :QBYB-OUTBAL-BNKRPT-3-1-6,
              :QBYB-UNITS-BNKRPT-3-1-7,
              :QBYB-OUTBAL-BNKRPT-3-1-7,
              :QBYB-UNITS-BNKRPT-3-1-8,
              :QBYB-OUTBAL-BNKRPT-3-1-8,
              :QBYB-UNITS-BNKRPT-3-1-9,
              :QBYB-OUTBAL-BNKRPT-3-1-9,
              :QBYB-UNITS-BNKRPT-3-1-10,
              :QBYB-OUTBAL-BNKRPT-3-1-10,
              :QBYB-UNITS-BNKRPT-3-2-1,
              :QBYB-OUTBAL-BNKRPT-3-2-1,
              :QBYB-UNITS-BNKRPT-3-2-2,
              :QBYB-OUTBAL-BNKRPT-3-2-2,
              :QBYB-UNITS-BNKRPT-3-2-3,
              :QBYB-OUTBAL-BNKRPT-3-2-3,
              :QBYB-UNITS-BNKRPT-3-2-4,
              :QBYB-OUTBAL-BNKRPT-3-2-4,
              :QBYB-UNITS-BNKRPT-3-2-5,
              :QBYB-OUTBAL-BNKRPT-3-2-5,
              :QBYB-UNITS-BNKRPT-3-2-6,
              :QBYB-OUTBAL-BNKRPT-3-2-6,
              :QBYB-UNITS-BNKRPT-3-2-7,
              :QBYB-OUTBAL-BNKRPT-3-2-7,
              :QBYB-UNITS-BNKRPT-3-2-8,
              :QBYB-OUTBAL-BNKRPT-3-2-8,
              :QBYB-UNITS-BNKRPT-3-2-9,
              :QBYB-OUTBAL-BNKRPT-3-2-9,
              :QBYB-UNITS-BNKRPT-3-2-10,
              :QBYB-OUTBAL-BNKRPT-3-2-10,
              :QBYB-SUMWGTTM,
              :QBYB-SUMWGTAV,
              :QBYB-SUMWGTTA,
              :QBYB-IB-ACCRUED,
              :QBYB-DLDISC-1,
              :QBYB-DLDISC-2,
              :QBYB-DLDISC-3,
              :QBYB-STD-POS,
              :QBYB-30-DAY-MOVED,
              :QBYB-PROJ-INCREASE,
              :QBYB-PROJ-PROFIT,
              :QBYB-PROCEEDS-1,
              :QBYB-PROCEEDS-2,
              :QBYB-LOCKBOX-1,
              :QBYB-LOCKBOX-2,
              :QBYB-UNEARNED-AMT-1-1-1,
              :QBYB-UNEARNED-AMT-1-1-2,
              :QBYB-UNEARNED-AMT-1-1-3,
              :QBYB-UNEARNED-AMT-1-1-4,
              :QBYB-UNEARNED-AMT-1-1-5,
              :QBYB-UNEARNED-AMT-1-1-6,
              :QBYB-UNEARNED-AMT-1-1-7,
              :QBYB-UNEARNED-AMT-1-1-8,
              :QBYB-UNEARNED-AMT-1-1-9,
              :QBYB-UNEARNED-AMT-1-1-10,
              :QBYB-UNEARNED-AMT-1-2-1,
              :QBYB-UNEARNED-AMT-1-2-2,
              :QBYB-UNEARNED-AMT-1-2-3,
              :QBYB-UNEARNED-AMT-1-2-4,
              :QBYB-UNEARNED-AMT-1-2-5,
              :QBYB-UNEARNED-AMT-1-2-6,
              :QBYB-UNEARNED-AMT-1-2-7,
              :QBYB-UNEARNED-AMT-1-2-8,
              :QBYB-UNEARNED-AMT-1-2-9,
              :QBYB-UNEARNED-AMT-1-2-10,
              :QBYB-UNEARNED-AMT-2-1-1,
              :QBYB-UNEARNED-AMT-2-1-2,
              :QBYB-UNEARNED-AMT-2-1-3,
              :QBYB-UNEARNED-AMT-2-1-4,
              :QBYB-UNEARNED-AMT-2-1-5,
              :QBYB-UNEARNED-AMT-2-1-6,
              :QBYB-UNEARNED-AMT-2-1-7,
              :QBYB-UNEARNED-AMT-2-1-8,
              :QBYB-UNEARNED-AMT-2-1-9,
              :QBYB-UNEARNED-AMT-2-1-10,
              :QBYB-UNEARNED-AMT-2-2-1,
              :QBYB-UNEARNED-AMT-2-2-2,
              :QBYB-UNEARNED-AMT-2-2-3,
              :QBYB-UNEARNED-AMT-2-2-4,
              :QBYB-UNEARNED-AMT-2-2-5,
              :QBYB-UNEARNED-AMT-2-2-6,
              :QBYB-UNEARNED-AMT-2-2-7,
              :QBYB-UNEARNED-AMT-2-2-8,
              :QBYB-UNEARNED-AMT-2-2-9,
              :QBYB-UNEARNED-AMT-2-2-10,
              :QBYB-UNEARNED-AMT-3-1-1,
              :QBYB-UNEARNED-AMT-3-1-2,
              :QBYB-UNEARNED-AMT-3-1-3,
              :QBYB-UNEARNED-AMT-3-1-4,
              :QBYB-UNEARNED-AMT-3-1-5,
              :QBYB-UNEARNED-AMT-3-1-6,
              :QBYB-UNEARNED-AMT-3-1-7,
              :QBYB-UNEARNED-AMT-3-1-8,
              :QBYB-UNEARNED-AMT-3-1-9,
              :QBYB-UNEARNED-AMT-3-1-10,
              :QBYB-UNEARNED-AMT-3-2-1,
              :QBYB-UNEARNED-AMT-3-2-2,
              :QBYB-UNEARNED-AMT-3-2-3,
              :QBYB-UNEARNED-AMT-3-2-4,
              :QBYB-UNEARNED-AMT-3-2-5,
              :QBYB-UNEARNED-AMT-3-2-6,
              :QBYB-UNEARNED-AMT-3-2-7,
              :QBYB-UNEARNED-AMT-3-2-8,
              :QBYB-UNEARNED-AMT-3-2-9,
              :QBYB-UNEARNED-AMT-3-2-10)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-BYB1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

           MOVE '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       REWRITE-BYB1-FILE.

           PERFORM REWRITE-IT.
           MOVE BYB-PATH-SQL TO E-FILE.
           MOVE BYB1-KEY TO E-KEYX.
           PERFORM SET-BYB-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            UPDATE DBO.BYBFILE
             SET
              BYBFILE.BYB_DATE_CCYYMM = :QBYB-DATE-CCYYMM,
              BYBFILE.BYB_BRNO = :QBYB-BRNO,
              BYBFILE.BYB_CLASS = :QBYB-CLASS,
              BYBFILE.BYB_PL_PRINCOL_1_1 = :QBYB-PL-PRINCOL-1-1,
              BYBFILE.BYB_PL_INTCOL_1_1 = :QBYB-PL-INTCOL-1-1,
              BYBFILE.BYB_PL_LATECOL_1_1 = :QBYB-PL-LATECOL-1-1,
              BYBFILE.BYB_PL_O2COL_1_1 = :QBYB-PL-O2COL-1-1,
              BYBFILE.BYB_PL_PRINCOL_1_2 = :QBYB-PL-PRINCOL-1-2,
              BYBFILE.BYB_PL_INTCOL_1_2 = :QBYB-PL-INTCOL-1-2,
              BYBFILE.BYB_PL_LATECOL_1_2 = :QBYB-PL-LATECOL-1-2,
              BYBFILE.BYB_PL_O2COL_1_2 = :QBYB-PL-O2COL-1-2,
              BYBFILE.BYB_PL_PRINCOL_1_3 = :QBYB-PL-PRINCOL-1-3,
              BYBFILE.BYB_PL_INTCOL_1_3 = :QBYB-PL-INTCOL-1-3,
              BYBFILE.BYB_PL_LATECOL_1_3 = :QBYB-PL-LATECOL-1-3,
              BYBFILE.BYB_PL_O2COL_1_3 = :QBYB-PL-O2COL-1-3,
              BYBFILE.BYB_PL_PRINCOL_1_4 = :QBYB-PL-PRINCOL-1-4,
              BYBFILE.BYB_PL_INTCOL_1_4 = :QBYB-PL-INTCOL-1-4,
              BYBFILE.BYB_PL_LATECOL_1_4 = :QBYB-PL-LATECOL-1-4,
              BYBFILE.BYB_PL_O2COL_1_4 = :QBYB-PL-O2COL-1-4,
              BYBFILE.BYB_PL_PRINCOL_1_5 = :QBYB-PL-PRINCOL-1-5,
              BYBFILE.BYB_PL_INTCOL_1_5 = :QBYB-PL-INTCOL-1-5,
              BYBFILE.BYB_PL_LATECOL_1_5 = :QBYB-PL-LATECOL-1-5,
              BYBFILE.BYB_PL_O2COL_1_5 = :QBYB-PL-O2COL-1-5,
              BYBFILE.BYB_AMT_SOLD_1 = :QBYB-AMT-SOLD-1,
              BYBFILE.BYB_AMT_TRANS_1 = :QBYB-AMT-TRANS-1,
              BYBFILE.BYB_AMT_PURCH_1 = :QBYB-AMT-PURCH-1,
              BYBFILE.BYB_NO_SOLD_1 = :QBYB-NO-SOLD-1,
              BYBFILE.BYB_NO_TRANS_1 = :QBYB-NO-TRANS-1,
              BYBFILE.BYB_NO_PURCH_1 = :QBYB-NO-PURCH-1,
              BYBFILE.BYB_OTHCOL_1 = :QBYB-OTHCOL-1,
              BYBFILE.BYB_OT2COL_1 = :QBYB-OT2COL-1,
              BYBFILE.BYB_INTCOL_1 = :QBYB-INTCOL-1,
              BYBFILE.BYB_LATECOL_1 = :QBYB-LATECOL-1,
              BYBFILE.BYB_PRINCOL_1 = :QBYB-PRINCOL-1,
              BYBFILE.BYB_DEFCOL_1 = :QBYB-DEFCOL-1,
              BYBFILE.BYB_RNPRIN_1 = :QBYB-RNPRIN-1,
              BYBFILE.BYB_INTREB_1 = :QBYB-INTREB-1,
              BYBFILE.BYB_CLREB1_1 = :QBYB-CLREB1-1,
              BYBFILE.BYB_CLREB2_1 = :QBYB-CLREB2-1,
              BYBFILE.BYB_CLREB3_1 = :QBYB-CLREB3-1,
              BYBFILE.BYB_CLREB4_1 = :QBYB-CLREB4-1,
              BYBFILE.BYB_AHREB_1 = :QBYB-AHREB-1,
              BYBFILE.BYB_PPREB_1 = :QBYB-PPREB-1,
              BYBFILE.BYB_O1REB_1 = :QBYB-O1REB-1,
              BYBFILE.BYB_O2REB_1 = :QBYB-O2REB-1,
              BYBFILE.BYB_O3REB_1 = :QBYB-O3REB-1,
              BYBFILE.BYB_O4REB_1 = :QBYB-O4REB-1,
              BYBFILE.BYB_O5REB_1 = :QBYB-O5REB-1,
              BYBFILE.BYB_O6REB_1 = :QBYB-O6REB-1,
              BYBFILE.BYB_O7REB_1 = :QBYB-O7REB-1,
              BYBFILE.BYB_SCREB_1 = :QBYB-SCREB-1,
              BYBFILE.BYB_MFREB_1 = :QBYB-MFREB-1,
              BYBFILE.BYB_DFREB_1 = :QBYB-DFREB-1,
              BYBFILE.BYB_UN7_REB_1 = :QBYB-UN7-REB-1,
              BYBFILE.BYB_UN8_REB_1 = :QBYB-UN8-REB-1,
              BYBFILE.BYB_UN9_REB_1 = :QBYB-UN9-REB-1,
              BYBFILE.BYB_UN10_REB_1 = :QBYB-UN10-REB-1,
              BYBFILE.BYB_PART_CHGOFF_1 = :QBYB-PART-CHGOFF-1,
              BYBFILE.BYB_ALLOTMENTS_1 = :QBYB-ALLOTMENTS-1,
              BYBFILE.BYB_INS_NO_ELIG_1_1 = :QBYB-INS-NO-ELIG-1-1,
              BYBFILE.BYB_INS_NO_WROTE_1_1 =
                   :QBYB-INS-NO-WROTE-1-1,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_1 =
                   :QBYB-INS-NO-NOTWROTE-1-1,
              BYBFILE.BYB_INS_AMT_WROTE_1_1 =
                   :QBYB-INS-AMT-WROTE-1-1,
              BYBFILE.BYB_INS_NO_ELIG_1_2 = :QBYB-INS-NO-ELIG-1-2,
              BYBFILE.BYB_INS_NO_WROTE_1_2 =
                   :QBYB-INS-NO-WROTE-1-2,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_2 =
                   :QBYB-INS-NO-NOTWROTE-1-2,
              BYBFILE.BYB_INS_AMT_WROTE_1_2 =
                   :QBYB-INS-AMT-WROTE-1-2,
              BYBFILE.BYB_INS_NO_ELIG_1_3 = :QBYB-INS-NO-ELIG-1-3,
              BYBFILE.BYB_INS_NO_WROTE_1_3 =
                   :QBYB-INS-NO-WROTE-1-3,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_3 =
                   :QBYB-INS-NO-NOTWROTE-1-3,
              BYBFILE.BYB_INS_AMT_WROTE_1_3 =
                   :QBYB-INS-AMT-WROTE-1-3,
              BYBFILE.BYB_INS_NO_ELIG_1_4 = :QBYB-INS-NO-ELIG-1-4,
              BYBFILE.BYB_INS_NO_WROTE_1_4 =
                   :QBYB-INS-NO-WROTE-1-4,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_4 =
                   :QBYB-INS-NO-NOTWROTE-1-4,
              BYBFILE.BYB_INS_AMT_WROTE_1_4 =
                   :QBYB-INS-AMT-WROTE-1-4,
              BYBFILE.BYB_INS_NO_ELIG_1_5 = :QBYB-INS-NO-ELIG-1-5,
              BYBFILE.BYB_INS_NO_WROTE_1_5 =
                   :QBYB-INS-NO-WROTE-1-5,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_5 =
                   :QBYB-INS-NO-NOTWROTE-1-5,
              BYBFILE.BYB_INS_AMT_WROTE_1_5 =
                   :QBYB-INS-AMT-WROTE-1-5,
              BYBFILE.BYB_INS_NO_ELIG_1_6 = :QBYB-INS-NO-ELIG-1-6,
              BYBFILE.BYB_INS_NO_WROTE_1_6 =
                   :QBYB-INS-NO-WROTE-1-6,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_6 =
                   :QBYB-INS-NO-NOTWROTE-1-6,
              BYBFILE.BYB_INS_AMT_WROTE_1_6 =
                   :QBYB-INS-AMT-WROTE-1-6,
              BYBFILE.BYB_INS_NO_ELIG_1_7 = :QBYB-INS-NO-ELIG-1-7,
              BYBFILE.BYB_INS_NO_WROTE_1_7 =
                   :QBYB-INS-NO-WROTE-1-7,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_7 =
                   :QBYB-INS-NO-NOTWROTE-1-7,
              BYBFILE.BYB_INS_AMT_WROTE_1_7 =
                   :QBYB-INS-AMT-WROTE-1-7,
              BYBFILE.BYB_INS_NO_ELIG_1_8 = :QBYB-INS-NO-ELIG-1-8,
              BYBFILE.BYB_INS_NO_WROTE_1_8 =
                   :QBYB-INS-NO-WROTE-1-8,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_8 =
                   :QBYB-INS-NO-NOTWROTE-1-8,
              BYBFILE.BYB_INS_AMT_WROTE_1_8 =
                   :QBYB-INS-AMT-WROTE-1-8,
              BYBFILE.BYB_INS_NO_ELIG_1_9 = :QBYB-INS-NO-ELIG-1-9,
              BYBFILE.BYB_INS_NO_WROTE_1_9 =
                   :QBYB-INS-NO-WROTE-1-9,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_9 =
                   :QBYB-INS-NO-NOTWROTE-1-9,
              BYBFILE.BYB_INS_AMT_WROTE_1_9 =
                   :QBYB-INS-AMT-WROTE-1-9,
              BYBFILE.BYB_INS_NO_ELIG_1_10 =
                   :QBYB-INS-NO-ELIG-1-10,
              BYBFILE.BYB_INS_NO_WROTE_1_10 =
                   :QBYB-INS-NO-WROTE-1-10,
              BYBFILE.BYB_INS_NO_NOTWROTE_1_10 =
                   :QBYB-INS-NO-NOTWROTE-1-10,
              BYBFILE.BYB_INS_AMT_WROTE_1_10 =
                   :QBYB-INS-AMT-WROTE-1-10,
              BYBFILE.BYB_FEE_NO_ELIG_1_1 = :QBYB-FEE-NO-ELIG-1-1,
              BYBFILE.BYB_FEE_NO_WROTE_1_1 =
                   :QBYB-FEE-NO-WROTE-1-1,
              BYBFILE.BYB_FEE_NO_NOTWROTE_1_1 =
                   :QBYB-FEE-NO-NOTWROTE-1-1,
              BYBFILE.BYB_FEE_AMT_WROTE_1_1 =
                   :QBYB-FEE-AMT-WROTE-1-1,
              BYBFILE.BYB_FEE_NO_ELIG_1_2 = :QBYB-FEE-NO-ELIG-1-2,
              BYBFILE.BYB_FEE_NO_WROTE_1_2 =
                   :QBYB-FEE-NO-WROTE-1-2,
              BYBFILE.BYB_FEE_NO_NOTWROTE_1_2 =
                   :QBYB-FEE-NO-NOTWROTE-1-2,
              BYBFILE.BYB_FEE_AMT_WROTE_1_2 =
                   :QBYB-FEE-AMT-WROTE-1-2,
              BYBFILE.BYB_FEE_NO_ELIG_1_3 = :QBYB-FEE-NO-ELIG-1-3,
              BYBFILE.BYB_FEE_NO_WROTE_1_3 =
                   :QBYB-FEE-NO-WROTE-1-3,
              BYBFILE.BYB_FEE_NO_NOTWROTE_1_3 =
                   :QBYB-FEE-NO-NOTWROTE-1-3,
              BYBFILE.BYB_FEE_AMT_WROTE_1_3 =
                   :QBYB-FEE-AMT-WROTE-1-3,
              BYBFILE.BYB_OTH_NO_COL_1 = :QBYB-OTH-NO-COL-1,
              BYBFILE.BYB_OT2_NO_COL_1 = :QBYB-OT2-NO-COL-1,
              BYBFILE.BYB_INT_NO_COL_1 = :QBYB-INT-NO-COL-1,
              BYBFILE.BYB_LATE_NO_COL_1 = :QBYB-LATE-NO-COL-1,
              BYBFILE.BYB_OTH_NO_WAIVED_1 = :QBYB-OTH-NO-WAIVED-1,
              BYBFILE.BYB_OT2_NO_WAIVED_1 = :QBYB-OT2-NO-WAIVED-1,
              BYBFILE.BYB_INT_NO_WAIVED_1 = :QBYB-INT-NO-WAIVED-1,
              BYBFILE.BYB_LATE_NO_WAIVED_1 =
                   :QBYB-LATE-NO-WAIVED-1,
              BYBFILE.BYB_OTH_AMT_WAIVED_1 =
                   :QBYB-OTH-AMT-WAIVED-1,
              BYBFILE.BYB_OT2_AMT_WAIVED_1 =
                   :QBYB-OT2-AMT-WAIVED-1,
              BYBFILE.BYB_INT_AMT_WAIVED_1 =
                   :QBYB-INT-AMT-WAIVED-1,
              BYBFILE.BYB_LATE_AMT_WAIVED_1 =
                   :QBYB-LATE-AMT-WAIVED-1,
              BYBFILE.BYB_PL_PRINCOL_2_1 = :QBYB-PL-PRINCOL-2-1,
              BYBFILE.BYB_PL_INTCOL_2_1 = :QBYB-PL-INTCOL-2-1,
              BYBFILE.BYB_PL_LATECOL_2_1 = :QBYB-PL-LATECOL-2-1,
              BYBFILE.BYB_PL_O2COL_2_1 = :QBYB-PL-O2COL-2-1,
              BYBFILE.BYB_PL_PRINCOL_2_2 = :QBYB-PL-PRINCOL-2-2,
              BYBFILE.BYB_PL_INTCOL_2_2 = :QBYB-PL-INTCOL-2-2,
              BYBFILE.BYB_PL_LATECOL_2_2 = :QBYB-PL-LATECOL-2-2,
              BYBFILE.BYB_PL_O2COL_2_2 = :QBYB-PL-O2COL-2-2,
              BYBFILE.BYB_PL_PRINCOL_2_3 = :QBYB-PL-PRINCOL-2-3,
              BYBFILE.BYB_PL_INTCOL_2_3 = :QBYB-PL-INTCOL-2-3,
              BYBFILE.BYB_PL_LATECOL_2_3 = :QBYB-PL-LATECOL-2-3,
              BYBFILE.BYB_PL_O2COL_2_3 = :QBYB-PL-O2COL-2-3,
              BYBFILE.BYB_PL_PRINCOL_2_4 = :QBYB-PL-PRINCOL-2-4,
              BYBFILE.BYB_PL_INTCOL_2_4 = :QBYB-PL-INTCOL-2-4,
              BYBFILE.BYB_PL_LATECOL_2_4 = :QBYB-PL-LATECOL-2-4,
              BYBFILE.BYB_PL_O2COL_2_4 = :QBYB-PL-O2COL-2-4,
              BYBFILE.BYB_PL_PRINCOL_2_5 = :QBYB-PL-PRINCOL-2-5,
              BYBFILE.BYB_PL_INTCOL_2_5 = :QBYB-PL-INTCOL-2-5,
              BYBFILE.BYB_PL_LATECOL_2_5 = :QBYB-PL-LATECOL-2-5,
              BYBFILE.BYB_PL_O2COL_2_5 = :QBYB-PL-O2COL-2-5,
              BYBFILE.BYB_AMT_SOLD_2 = :QBYB-AMT-SOLD-2,
              BYBFILE.BYB_AMT_TRANS_2 = :QBYB-AMT-TRANS-2,
              BYBFILE.BYB_AMT_PURCH_2 = :QBYB-AMT-PURCH-2,
              BYBFILE.BYB_NO_SOLD_2 = :QBYB-NO-SOLD-2,
              BYBFILE.BYB_NO_TRANS_2 = :QBYB-NO-TRANS-2,
              BYBFILE.BYB_NO_PURCH_2 = :QBYB-NO-PURCH-2,
              BYBFILE.BYB_OTHCOL_2 = :QBYB-OTHCOL-2,
              BYBFILE.BYB_OT2COL_2 = :QBYB-OT2COL-2,
              BYBFILE.BYB_INTCOL_2 = :QBYB-INTCOL-2,
              BYBFILE.BYB_LATECOL_2 = :QBYB-LATECOL-2,
              BYBFILE.BYB_PRINCOL_2 = :QBYB-PRINCOL-2,
              BYBFILE.BYB_DEFCOL_2 = :QBYB-DEFCOL-2,
              BYBFILE.BYB_RNPRIN_2 = :QBYB-RNPRIN-2,
              BYBFILE.BYB_INTREB_2 = :QBYB-INTREB-2,
              BYBFILE.BYB_CLREB1_2 = :QBYB-CLREB1-2,
              BYBFILE.BYB_CLREB2_2 = :QBYB-CLREB2-2,
              BYBFILE.BYB_CLREB3_2 = :QBYB-CLREB3-2,
              BYBFILE.BYB_CLREB4_2 = :QBYB-CLREB4-2,
              BYBFILE.BYB_AHREB_2 = :QBYB-AHREB-2,
              BYBFILE.BYB_PPREB_2 = :QBYB-PPREB-2,
              BYBFILE.BYB_O1REB_2 = :QBYB-O1REB-2,
              BYBFILE.BYB_O2REB_2 = :QBYB-O2REB-2,
              BYBFILE.BYB_O3REB_2 = :QBYB-O3REB-2,
              BYBFILE.BYB_O4REB_2 = :QBYB-O4REB-2,
              BYBFILE.BYB_O5REB_2 = :QBYB-O5REB-2,
              BYBFILE.BYB_O6REB_2 = :QBYB-O6REB-2,
              BYBFILE.BYB_O7REB_2 = :QBYB-O7REB-2,
              BYBFILE.BYB_SCREB_2 = :QBYB-SCREB-2,
              BYBFILE.BYB_MFREB_2 = :QBYB-MFREB-2,
              BYBFILE.BYB_DFREB_2 = :QBYB-DFREB-2,
              BYBFILE.BYB_UN7_REB_2 = :QBYB-UN7-REB-2,
              BYBFILE.BYB_UN8_REB_2 = :QBYB-UN8-REB-2,
              BYBFILE.BYB_UN9_REB_2 = :QBYB-UN9-REB-2,
              BYBFILE.BYB_UN10_REB_2 = :QBYB-UN10-REB-2,
              BYBFILE.BYB_PART_CHGOFF_2 = :QBYB-PART-CHGOFF-2,
              BYBFILE.BYB_ALLOTMENTS_2 = :QBYB-ALLOTMENTS-2,
              BYBFILE.BYB_INS_NO_ELIG_2_1 = :QBYB-INS-NO-ELIG-2-1,
              BYBFILE.BYB_INS_NO_WROTE_2_1 =
                   :QBYB-INS-NO-WROTE-2-1,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_1 =
                   :QBYB-INS-NO-NOTWROTE-2-1,
              BYBFILE.BYB_INS_AMT_WROTE_2_1 =
                   :QBYB-INS-AMT-WROTE-2-1,
              BYBFILE.BYB_INS_NO_ELIG_2_2 = :QBYB-INS-NO-ELIG-2-2,
              BYBFILE.BYB_INS_NO_WROTE_2_2 =
                   :QBYB-INS-NO-WROTE-2-2,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_2 =
                   :QBYB-INS-NO-NOTWROTE-2-2,
              BYBFILE.BYB_INS_AMT_WROTE_2_2 =
                   :QBYB-INS-AMT-WROTE-2-2,
              BYBFILE.BYB_INS_NO_ELIG_2_3 = :QBYB-INS-NO-ELIG-2-3,
              BYBFILE.BYB_INS_NO_WROTE_2_3 =
                   :QBYB-INS-NO-WROTE-2-3,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_3 =
                   :QBYB-INS-NO-NOTWROTE-2-3,
              BYBFILE.BYB_INS_AMT_WROTE_2_3 =
                   :QBYB-INS-AMT-WROTE-2-3,
              BYBFILE.BYB_INS_NO_ELIG_2_4 = :QBYB-INS-NO-ELIG-2-4,
              BYBFILE.BYB_INS_NO_WROTE_2_4 =
                   :QBYB-INS-NO-WROTE-2-4,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_4 =
                   :QBYB-INS-NO-NOTWROTE-2-4,
              BYBFILE.BYB_INS_AMT_WROTE_2_4 =
                   :QBYB-INS-AMT-WROTE-2-4,
              BYBFILE.BYB_INS_NO_ELIG_2_5 = :QBYB-INS-NO-ELIG-2-5,
              BYBFILE.BYB_INS_NO_WROTE_2_5 =
                   :QBYB-INS-NO-WROTE-2-5,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_5 =
                   :QBYB-INS-NO-NOTWROTE-2-5,
              BYBFILE.BYB_INS_AMT_WROTE_2_5 =
                   :QBYB-INS-AMT-WROTE-2-5,
              BYBFILE.BYB_INS_NO_ELIG_2_6 = :QBYB-INS-NO-ELIG-2-6,
              BYBFILE.BYB_INS_NO_WROTE_2_6 =
                   :QBYB-INS-NO-WROTE-2-6,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_6 =
                   :QBYB-INS-NO-NOTWROTE-2-6,
              BYBFILE.BYB_INS_AMT_WROTE_2_6 =
                   :QBYB-INS-AMT-WROTE-2-6,
              BYBFILE.BYB_INS_NO_ELIG_2_7 = :QBYB-INS-NO-ELIG-2-7,
              BYBFILE.BYB_INS_NO_WROTE_2_7 =
                   :QBYB-INS-NO-WROTE-2-7,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_7 =
                   :QBYB-INS-NO-NOTWROTE-2-7,
              BYBFILE.BYB_INS_AMT_WROTE_2_7 =
                   :QBYB-INS-AMT-WROTE-2-7,
              BYBFILE.BYB_INS_NO_ELIG_2_8 = :QBYB-INS-NO-ELIG-2-8,
              BYBFILE.BYB_INS_NO_WROTE_2_8 =
                   :QBYB-INS-NO-WROTE-2-8,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_8 =
                   :QBYB-INS-NO-NOTWROTE-2-8,
              BYBFILE.BYB_INS_AMT_WROTE_2_8 =
                   :QBYB-INS-AMT-WROTE-2-8,
              BYBFILE.BYB_INS_NO_ELIG_2_9 = :QBYB-INS-NO-ELIG-2-9,
              BYBFILE.BYB_INS_NO_WROTE_2_9 =
                   :QBYB-INS-NO-WROTE-2-9,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_9 =
                   :QBYB-INS-NO-NOTWROTE-2-9,
              BYBFILE.BYB_INS_AMT_WROTE_2_9 =
                   :QBYB-INS-AMT-WROTE-2-9,
              BYBFILE.BYB_INS_NO_ELIG_2_10 =
                   :QBYB-INS-NO-ELIG-2-10,
              BYBFILE.BYB_INS_NO_WROTE_2_10 =
                   :QBYB-INS-NO-WROTE-2-10,
              BYBFILE.BYB_INS_NO_NOTWROTE_2_10 =
                   :QBYB-INS-NO-NOTWROTE-2-10,
              BYBFILE.BYB_INS_AMT_WROTE_2_10 =
                   :QBYB-INS-AMT-WROTE-2-10,
              BYBFILE.BYB_FEE_NO_ELIG_2_1 = :QBYB-FEE-NO-ELIG-2-1,
              BYBFILE.BYB_FEE_NO_WROTE_2_1 =
                   :QBYB-FEE-NO-WROTE-2-1,
              BYBFILE.BYB_FEE_NO_NOTWROTE_2_1 =
                   :QBYB-FEE-NO-NOTWROTE-2-1,
              BYBFILE.BYB_FEE_AMT_WROTE_2_1 =
                   :QBYB-FEE-AMT-WROTE-2-1,
              BYBFILE.BYB_FEE_NO_ELIG_2_2 = :QBYB-FEE-NO-ELIG-2-2,
              BYBFILE.BYB_FEE_NO_WROTE_2_2 =
                   :QBYB-FEE-NO-WROTE-2-2,
              BYBFILE.BYB_FEE_NO_NOTWROTE_2_2 =
                   :QBYB-FEE-NO-NOTWROTE-2-2,
              BYBFILE.BYB_FEE_AMT_WROTE_2_2 =
                   :QBYB-FEE-AMT-WROTE-2-2,
              BYBFILE.BYB_FEE_NO_ELIG_2_3 = :QBYB-FEE-NO-ELIG-2-3,
              BYBFILE.BYB_FEE_NO_WROTE_2_3 =
                   :QBYB-FEE-NO-WROTE-2-3,
              BYBFILE.BYB_FEE_NO_NOTWROTE_2_3 =
                   :QBYB-FEE-NO-NOTWROTE-2-3,
              BYBFILE.BYB_FEE_AMT_WROTE_2_3 =
                   :QBYB-FEE-AMT-WROTE-2-3,
              BYBFILE.BYB_OTH_NO_COL_2 = :QBYB-OTH-NO-COL-2,
              BYBFILE.BYB_OT2_NO_COL_2 = :QBYB-OT2-NO-COL-2,
              BYBFILE.BYB_INT_NO_COL_2 = :QBYB-INT-NO-COL-2,
              BYBFILE.BYB_LATE_NO_COL_2 = :QBYB-LATE-NO-COL-2,
              BYBFILE.BYB_OTH_NO_WAIVED_2 = :QBYB-OTH-NO-WAIVED-2,
              BYBFILE.BYB_OT2_NO_WAIVED_2 = :QBYB-OT2-NO-WAIVED-2,
              BYBFILE.BYB_INT_NO_WAIVED_2 = :QBYB-INT-NO-WAIVED-2,
              BYBFILE.BYB_LATE_NO_WAIVED_2 =
                   :QBYB-LATE-NO-WAIVED-2,
              BYBFILE.BYB_OTH_AMT_WAIVED_2 =
                   :QBYB-OTH-AMT-WAIVED-2,
              BYBFILE.BYB_OT2_AMT_WAIVED_2 =
                   :QBYB-OT2-AMT-WAIVED-2,
              BYBFILE.BYB_INT_AMT_WAIVED_2 =
                   :QBYB-INT-AMT-WAIVED-2,
              BYBFILE.BYB_LATE_AMT_WAIVED_2 =
                   :QBYB-LATE-AMT-WAIVED-2,
              BYBFILE.BYB_UNITS_1_1_1 = :QBYB-UNITS-1-1-1,
              BYBFILE.BYB_OUTBAL_1_1_1 = :QBYB-OUTBAL-1-1-1,
              BYBFILE.BYB_UNITS_1_1_2 = :QBYB-UNITS-1-1-2,
              BYBFILE.BYB_OUTBAL_1_1_2 = :QBYB-OUTBAL-1-1-2,
              BYBFILE.BYB_UNITS_1_1_3 = :QBYB-UNITS-1-1-3,
              BYBFILE.BYB_OUTBAL_1_1_3 = :QBYB-OUTBAL-1-1-3,
              BYBFILE.BYB_UNITS_1_1_4 = :QBYB-UNITS-1-1-4,
              BYBFILE.BYB_OUTBAL_1_1_4 = :QBYB-OUTBAL-1-1-4,
              BYBFILE.BYB_UNITS_1_1_5 = :QBYB-UNITS-1-1-5,
              BYBFILE.BYB_OUTBAL_1_1_5 = :QBYB-OUTBAL-1-1-5,
              BYBFILE.BYB_UNITS_1_1_6 = :QBYB-UNITS-1-1-6,
              BYBFILE.BYB_OUTBAL_1_1_6 = :QBYB-OUTBAL-1-1-6,
              BYBFILE.BYB_UNITS_1_1_7 = :QBYB-UNITS-1-1-7,
              BYBFILE.BYB_OUTBAL_1_1_7 = :QBYB-OUTBAL-1-1-7,
              BYBFILE.BYB_UNITS_1_1_8 = :QBYB-UNITS-1-1-8,
              BYBFILE.BYB_OUTBAL_1_1_8 = :QBYB-OUTBAL-1-1-8,
              BYBFILE.BYB_UNITS_1_1_9 = :QBYB-UNITS-1-1-9,
              BYBFILE.BYB_OUTBAL_1_1_9 = :QBYB-OUTBAL-1-1-9,
              BYBFILE.BYB_UNITS_1_1_10 = :QBYB-UNITS-1-1-10,
              BYBFILE.BYB_OUTBAL_1_1_10 = :QBYB-OUTBAL-1-1-10,
              BYBFILE.BYB_UNITS_1_2_1 = :QBYB-UNITS-1-2-1,
              BYBFILE.BYB_OUTBAL_1_2_1 = :QBYB-OUTBAL-1-2-1,
              BYBFILE.BYB_UNITS_1_2_2 = :QBYB-UNITS-1-2-2,
              BYBFILE.BYB_OUTBAL_1_2_2 = :QBYB-OUTBAL-1-2-2,
              BYBFILE.BYB_UNITS_1_2_3 = :QBYB-UNITS-1-2-3,
              BYBFILE.BYB_OUTBAL_1_2_3 = :QBYB-OUTBAL-1-2-3,
              BYBFILE.BYB_UNITS_1_2_4 = :QBYB-UNITS-1-2-4,
              BYBFILE.BYB_OUTBAL_1_2_4 = :QBYB-OUTBAL-1-2-4,
              BYBFILE.BYB_UNITS_1_2_5 = :QBYB-UNITS-1-2-5,
              BYBFILE.BYB_OUTBAL_1_2_5 = :QBYB-OUTBAL-1-2-5,
              BYBFILE.BYB_UNITS_1_2_6 = :QBYB-UNITS-1-2-6,
              BYBFILE.BYB_OUTBAL_1_2_6 = :QBYB-OUTBAL-1-2-6,
              BYBFILE.BYB_UNITS_1_2_7 = :QBYB-UNITS-1-2-7,
              BYBFILE.BYB_OUTBAL_1_2_7 = :QBYB-OUTBAL-1-2-7,
              BYBFILE.BYB_UNITS_1_2_8 = :QBYB-UNITS-1-2-8,
              BYBFILE.BYB_OUTBAL_1_2_8 = :QBYB-OUTBAL-1-2-8,
              BYBFILE.BYB_UNITS_1_2_9 = :QBYB-UNITS-1-2-9,
              BYBFILE.BYB_OUTBAL_1_2_9 = :QBYB-OUTBAL-1-2-9,
              BYBFILE.BYB_UNITS_1_2_10 = :QBYB-UNITS-1-2-10,
              BYBFILE.BYB_OUTBAL_1_2_10 = :QBYB-OUTBAL-1-2-10,
              BYBFILE.BYB_UNITS_2_1_1 = :QBYB-UNITS-2-1-1,
              BYBFILE.BYB_OUTBAL_2_1_1 = :QBYB-OUTBAL-2-1-1,
              BYBFILE.BYB_UNITS_2_1_2 = :QBYB-UNITS-2-1-2,
              BYBFILE.BYB_OUTBAL_2_1_2 = :QBYB-OUTBAL-2-1-2,
              BYBFILE.BYB_UNITS_2_1_3 = :QBYB-UNITS-2-1-3,
              BYBFILE.BYB_OUTBAL_2_1_3 = :QBYB-OUTBAL-2-1-3,
              BYBFILE.BYB_UNITS_2_1_4 = :QBYB-UNITS-2-1-4,
              BYBFILE.BYB_OUTBAL_2_1_4 = :QBYB-OUTBAL-2-1-4,
              BYBFILE.BYB_UNITS_2_1_5 = :QBYB-UNITS-2-1-5,
              BYBFILE.BYB_OUTBAL_2_1_5 = :QBYB-OUTBAL-2-1-5,
              BYBFILE.BYB_UNITS_2_1_6 = :QBYB-UNITS-2-1-6,
              BYBFILE.BYB_OUTBAL_2_1_6 = :QBYB-OUTBAL-2-1-6,
              BYBFILE.BYB_UNITS_2_1_7 = :QBYB-UNITS-2-1-7,
              BYBFILE.BYB_OUTBAL_2_1_7 = :QBYB-OUTBAL-2-1-7,
              BYBFILE.BYB_UNITS_2_1_8 = :QBYB-UNITS-2-1-8,
              BYBFILE.BYB_OUTBAL_2_1_8 = :QBYB-OUTBAL-2-1-8,
              BYBFILE.BYB_UNITS_2_1_9 = :QBYB-UNITS-2-1-9,
              BYBFILE.BYB_OUTBAL_2_1_9 = :QBYB-OUTBAL-2-1-9,
              BYBFILE.BYB_UNITS_2_1_10 = :QBYB-UNITS-2-1-10,
              BYBFILE.BYB_OUTBAL_2_1_10 = :QBYB-OUTBAL-2-1-10,
              BYBFILE.BYB_UNITS_2_2_1 = :QBYB-UNITS-2-2-1,
              BYBFILE.BYB_OUTBAL_2_2_1 = :QBYB-OUTBAL-2-2-1,
              BYBFILE.BYB_UNITS_2_2_2 = :QBYB-UNITS-2-2-2,
              BYBFILE.BYB_OUTBAL_2_2_2 = :QBYB-OUTBAL-2-2-2,
              BYBFILE.BYB_UNITS_2_2_3 = :QBYB-UNITS-2-2-3,
              BYBFILE.BYB_OUTBAL_2_2_3 = :QBYB-OUTBAL-2-2-3,
              BYBFILE.BYB_UNITS_2_2_4 = :QBYB-UNITS-2-2-4,
              BYBFILE.BYB_OUTBAL_2_2_4 = :QBYB-OUTBAL-2-2-4,
              BYBFILE.BYB_UNITS_2_2_5 = :QBYB-UNITS-2-2-5,
              BYBFILE.BYB_OUTBAL_2_2_5 = :QBYB-OUTBAL-2-2-5,
              BYBFILE.BYB_UNITS_2_2_6 = :QBYB-UNITS-2-2-6,
              BYBFILE.BYB_OUTBAL_2_2_6 = :QBYB-OUTBAL-2-2-6,
              BYBFILE.BYB_UNITS_2_2_7 = :QBYB-UNITS-2-2-7,
              BYBFILE.BYB_OUTBAL_2_2_7 = :QBYB-OUTBAL-2-2-7,
              BYBFILE.BYB_UNITS_2_2_8 = :QBYB-UNITS-2-2-8,
              BYBFILE.BYB_OUTBAL_2_2_8 = :QBYB-OUTBAL-2-2-8,
              BYBFILE.BYB_UNITS_2_2_9 = :QBYB-UNITS-2-2-9,
              BYBFILE.BYB_OUTBAL_2_2_9 = :QBYB-OUTBAL-2-2-9,
              BYBFILE.BYB_UNITS_2_2_10 = :QBYB-UNITS-2-2-10,
              BYBFILE.BYB_OUTBAL_2_2_10 = :QBYB-OUTBAL-2-2-10,
              BYBFILE.BYB_UNITS_3_1_1 = :QBYB-UNITS-3-1-1,
              BYBFILE.BYB_OUTBAL_3_1_1 = :QBYB-OUTBAL-3-1-1,
              BYBFILE.BYB_UNITS_3_1_2 = :QBYB-UNITS-3-1-2,
              BYBFILE.BYB_OUTBAL_3_1_2 = :QBYB-OUTBAL-3-1-2,
              BYBFILE.BYB_UNITS_3_1_3 = :QBYB-UNITS-3-1-3,
              BYBFILE.BYB_OUTBAL_3_1_3 = :QBYB-OUTBAL-3-1-3,
              BYBFILE.BYB_UNITS_3_1_4 = :QBYB-UNITS-3-1-4,
              BYBFILE.BYB_OUTBAL_3_1_4 = :QBYB-OUTBAL-3-1-4,
              BYBFILE.BYB_UNITS_3_1_5 = :QBYB-UNITS-3-1-5,
              BYBFILE.BYB_OUTBAL_3_1_5 = :QBYB-OUTBAL-3-1-5,
              BYBFILE.BYB_UNITS_3_1_6 = :QBYB-UNITS-3-1-6,
              BYBFILE.BYB_OUTBAL_3_1_6 = :QBYB-OUTBAL-3-1-6,
              BYBFILE.BYB_UNITS_3_1_7 = :QBYB-UNITS-3-1-7,
              BYBFILE.BYB_OUTBAL_3_1_7 = :QBYB-OUTBAL-3-1-7,
              BYBFILE.BYB_UNITS_3_1_8 = :QBYB-UNITS-3-1-8,
              BYBFILE.BYB_OUTBAL_3_1_8 = :QBYB-OUTBAL-3-1-8,
              BYBFILE.BYB_UNITS_3_1_9 = :QBYB-UNITS-3-1-9,
              BYBFILE.BYB_OUTBAL_3_1_9 = :QBYB-OUTBAL-3-1-9,
              BYBFILE.BYB_UNITS_3_1_10 = :QBYB-UNITS-3-1-10,
              BYBFILE.BYB_OUTBAL_3_1_10 = :QBYB-OUTBAL-3-1-10,
              BYBFILE.BYB_UNITS_3_2_1 = :QBYB-UNITS-3-2-1,
              BYBFILE.BYB_OUTBAL_3_2_1 = :QBYB-OUTBAL-3-2-1,
              BYBFILE.BYB_UNITS_3_2_2 = :QBYB-UNITS-3-2-2,
              BYBFILE.BYB_OUTBAL_3_2_2 = :QBYB-OUTBAL-3-2-2,
              BYBFILE.BYB_UNITS_3_2_3 = :QBYB-UNITS-3-2-3,
              BYBFILE.BYB_OUTBAL_3_2_3 = :QBYB-OUTBAL-3-2-3,
              BYBFILE.BYB_UNITS_3_2_4 = :QBYB-UNITS-3-2-4,
              BYBFILE.BYB_OUTBAL_3_2_4 = :QBYB-OUTBAL-3-2-4,
              BYBFILE.BYB_UNITS_3_2_5 = :QBYB-UNITS-3-2-5,
              BYBFILE.BYB_OUTBAL_3_2_5 = :QBYB-OUTBAL-3-2-5,
              BYBFILE.BYB_UNITS_3_2_6 = :QBYB-UNITS-3-2-6,
              BYBFILE.BYB_OUTBAL_3_2_6 = :QBYB-OUTBAL-3-2-6,
              BYBFILE.BYB_UNITS_3_2_7 = :QBYB-UNITS-3-2-7,
              BYBFILE.BYB_OUTBAL_3_2_7 = :QBYB-OUTBAL-3-2-7,
              BYBFILE.BYB_UNITS_3_2_8 = :QBYB-UNITS-3-2-8,
              BYBFILE.BYB_OUTBAL_3_2_8 = :QBYB-OUTBAL-3-2-8,
              BYBFILE.BYB_UNITS_3_2_9 = :QBYB-UNITS-3-2-9,
              BYBFILE.BYB_OUTBAL_3_2_9 = :QBYB-OUTBAL-3-2-9,
              BYBFILE.BYB_UNITS_3_2_10 = :QBYB-UNITS-3-2-10,
              BYBFILE.BYB_OUTBAL_3_2_10 = :QBYB-OUTBAL-3-2-10,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_1 =
                   :QBYB-UNITS-BNKRPT-1-1-1,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_1 =
                   :QBYB-OUTBAL-BNKRPT-1-1-1,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_2 =
                   :QBYB-UNITS-BNKRPT-1-1-2,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_2 =
                   :QBYB-OUTBAL-BNKRPT-1-1-2,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_3 =
                   :QBYB-UNITS-BNKRPT-1-1-3,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_3 =
                   :QBYB-OUTBAL-BNKRPT-1-1-3,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_4 =
                   :QBYB-UNITS-BNKRPT-1-1-4,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_4 =
                   :QBYB-OUTBAL-BNKRPT-1-1-4,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_5 =
                   :QBYB-UNITS-BNKRPT-1-1-5,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_5 =
                   :QBYB-OUTBAL-BNKRPT-1-1-5,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_6 =
                   :QBYB-UNITS-BNKRPT-1-1-6,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_6 =
                   :QBYB-OUTBAL-BNKRPT-1-1-6,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_7 =
                   :QBYB-UNITS-BNKRPT-1-1-7,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_7 =
                   :QBYB-OUTBAL-BNKRPT-1-1-7,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_8 =
                   :QBYB-UNITS-BNKRPT-1-1-8,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_8 =
                   :QBYB-OUTBAL-BNKRPT-1-1-8,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_9 =
                   :QBYB-UNITS-BNKRPT-1-1-9,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_9 =
                   :QBYB-OUTBAL-BNKRPT-1-1-9,
              BYBFILE.BYB_UNITS_BNKRPT_1_1_10 =
                   :QBYB-UNITS-BNKRPT-1-1-10,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_1_10 =
                   :QBYB-OUTBAL-BNKRPT-1-1-10,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_1 =
                   :QBYB-UNITS-BNKRPT-1-2-1,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_1 =
                   :QBYB-OUTBAL-BNKRPT-1-2-1,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_2 =
                   :QBYB-UNITS-BNKRPT-1-2-2,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_2 =
                   :QBYB-OUTBAL-BNKRPT-1-2-2,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_3 =
                   :QBYB-UNITS-BNKRPT-1-2-3,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_3 =
                   :QBYB-OUTBAL-BNKRPT-1-2-3,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_4 =
                   :QBYB-UNITS-BNKRPT-1-2-4,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_4 =
                   :QBYB-OUTBAL-BNKRPT-1-2-4,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_5 =
                   :QBYB-UNITS-BNKRPT-1-2-5,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_5 =
                   :QBYB-OUTBAL-BNKRPT-1-2-5,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_6 =
                   :QBYB-UNITS-BNKRPT-1-2-6,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_6 =
                   :QBYB-OUTBAL-BNKRPT-1-2-6,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_7 =
                   :QBYB-UNITS-BNKRPT-1-2-7,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_7 =
                   :QBYB-OUTBAL-BNKRPT-1-2-7,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_8 =
                   :QBYB-UNITS-BNKRPT-1-2-8,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_8 =
                   :QBYB-OUTBAL-BNKRPT-1-2-8,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_9 =
                   :QBYB-UNITS-BNKRPT-1-2-9,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_9 =
                   :QBYB-OUTBAL-BNKRPT-1-2-9,
              BYBFILE.BYB_UNITS_BNKRPT_1_2_10 =
                   :QBYB-UNITS-BNKRPT-1-2-10,
              BYBFILE.BYB_OUTBAL_BNKRPT_1_2_10 =
                   :QBYB-OUTBAL-BNKRPT-1-2-10,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_1 =
                   :QBYB-UNITS-BNKRPT-2-1-1,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_1 =
                   :QBYB-OUTBAL-BNKRPT-2-1-1,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_2 =
                   :QBYB-UNITS-BNKRPT-2-1-2,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_2 =
                   :QBYB-OUTBAL-BNKRPT-2-1-2,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_3 =
                   :QBYB-UNITS-BNKRPT-2-1-3,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_3 =
                   :QBYB-OUTBAL-BNKRPT-2-1-3,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_4 =
                   :QBYB-UNITS-BNKRPT-2-1-4,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_4 =
                   :QBYB-OUTBAL-BNKRPT-2-1-4,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_5 =
                   :QBYB-UNITS-BNKRPT-2-1-5,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_5 =
                   :QBYB-OUTBAL-BNKRPT-2-1-5,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_6 =
                   :QBYB-UNITS-BNKRPT-2-1-6,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_6 =
                   :QBYB-OUTBAL-BNKRPT-2-1-6,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_7 =
                   :QBYB-UNITS-BNKRPT-2-1-7,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_7 =
                   :QBYB-OUTBAL-BNKRPT-2-1-7,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_8 =
                   :QBYB-UNITS-BNKRPT-2-1-8,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_8 =
                   :QBYB-OUTBAL-BNKRPT-2-1-8,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_9 =
                   :QBYB-UNITS-BNKRPT-2-1-9,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_9 =
                   :QBYB-OUTBAL-BNKRPT-2-1-9,
              BYBFILE.BYB_UNITS_BNKRPT_2_1_10 =
                   :QBYB-UNITS-BNKRPT-2-1-10,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_1_10 =
                   :QBYB-OUTBAL-BNKRPT-2-1-10,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_1 =
                   :QBYB-UNITS-BNKRPT-2-2-1,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_1 =
                   :QBYB-OUTBAL-BNKRPT-2-2-1,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_2 =
                   :QBYB-UNITS-BNKRPT-2-2-2,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_2 =
                   :QBYB-OUTBAL-BNKRPT-2-2-2,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_3 =
                   :QBYB-UNITS-BNKRPT-2-2-3,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_3 =
                   :QBYB-OUTBAL-BNKRPT-2-2-3,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_4 =
                   :QBYB-UNITS-BNKRPT-2-2-4,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_4 =
                   :QBYB-OUTBAL-BNKRPT-2-2-4,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_5 =
                   :QBYB-UNITS-BNKRPT-2-2-5,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_5 =
                   :QBYB-OUTBAL-BNKRPT-2-2-5,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_6 =
                   :QBYB-UNITS-BNKRPT-2-2-6,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_6 =
                   :QBYB-OUTBAL-BNKRPT-2-2-6,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_7 =
                   :QBYB-UNITS-BNKRPT-2-2-7,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_7 =
                   :QBYB-OUTBAL-BNKRPT-2-2-7,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_8 =
                   :QBYB-UNITS-BNKRPT-2-2-8,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_8 =
                   :QBYB-OUTBAL-BNKRPT-2-2-8,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_9 =
                   :QBYB-UNITS-BNKRPT-2-2-9,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_9 =
                   :QBYB-OUTBAL-BNKRPT-2-2-9,
              BYBFILE.BYB_UNITS_BNKRPT_2_2_10 =
                   :QBYB-UNITS-BNKRPT-2-2-10,
              BYBFILE.BYB_OUTBAL_BNKRPT_2_2_10 =
                   :QBYB-OUTBAL-BNKRPT-2-2-10,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_1 =
                   :QBYB-UNITS-BNKRPT-3-1-1,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_1 =
                   :QBYB-OUTBAL-BNKRPT-3-1-1,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_2 =
                   :QBYB-UNITS-BNKRPT-3-1-2,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_2 =
                   :QBYB-OUTBAL-BNKRPT-3-1-2,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_3 =
                   :QBYB-UNITS-BNKRPT-3-1-3,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_3 =
                   :QBYB-OUTBAL-BNKRPT-3-1-3,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_4 =
                   :QBYB-UNITS-BNKRPT-3-1-4,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_4 =
                   :QBYB-OUTBAL-BNKRPT-3-1-4,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_5 =
                   :QBYB-UNITS-BNKRPT-3-1-5,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_5 =
                   :QBYB-OUTBAL-BNKRPT-3-1-5,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_6 =
                   :QBYB-UNITS-BNKRPT-3-1-6,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_6 =
                   :QBYB-OUTBAL-BNKRPT-3-1-6,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_7 =
                   :QBYB-UNITS-BNKRPT-3-1-7,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_7 =
                   :QBYB-OUTBAL-BNKRPT-3-1-7,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_8 =
                   :QBYB-UNITS-BNKRPT-3-1-8,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_8 =
                   :QBYB-OUTBAL-BNKRPT-3-1-8,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_9 =
                   :QBYB-UNITS-BNKRPT-3-1-9,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_9 =
                   :QBYB-OUTBAL-BNKRPT-3-1-9,
              BYBFILE.BYB_UNITS_BNKRPT_3_1_10 =
                   :QBYB-UNITS-BNKRPT-3-1-10,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_1_10 =
                   :QBYB-OUTBAL-BNKRPT-3-1-10,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_1 =
                   :QBYB-UNITS-BNKRPT-3-2-1,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_1 =
                   :QBYB-OUTBAL-BNKRPT-3-2-1,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_2 =
                   :QBYB-UNITS-BNKRPT-3-2-2,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_2 =
                   :QBYB-OUTBAL-BNKRPT-3-2-2,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_3 =
                   :QBYB-UNITS-BNKRPT-3-2-3,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_3 =
                   :QBYB-OUTBAL-BNKRPT-3-2-3,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_4 =
                   :QBYB-UNITS-BNKRPT-3-2-4,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_4 =
                   :QBYB-OUTBAL-BNKRPT-3-2-4,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_5 =
                   :QBYB-UNITS-BNKRPT-3-2-5,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_5 =
                   :QBYB-OUTBAL-BNKRPT-3-2-5,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_6 =
                   :QBYB-UNITS-BNKRPT-3-2-6,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_6 =
                   :QBYB-OUTBAL-BNKRPT-3-2-6,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_7 =
                   :QBYB-UNITS-BNKRPT-3-2-7,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_7 =
                   :QBYB-OUTBAL-BNKRPT-3-2-7,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_8 =
                   :QBYB-UNITS-BNKRPT-3-2-8,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_8 =
                   :QBYB-OUTBAL-BNKRPT-3-2-8,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_9 =
                   :QBYB-UNITS-BNKRPT-3-2-9,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_9 =
                   :QBYB-OUTBAL-BNKRPT-3-2-9,
              BYBFILE.BYB_UNITS_BNKRPT_3_2_10 =
                   :QBYB-UNITS-BNKRPT-3-2-10,
              BYBFILE.BYB_OUTBAL_BNKRPT_3_2_10 =
                   :QBYB-OUTBAL-BNKRPT-3-2-10,
              BYBFILE.BYB_SUMWGTTM = :QBYB-SUMWGTTM,
              BYBFILE.BYB_SUMWGTAV = :QBYB-SUMWGTAV,
              BYBFILE.BYB_SUMWGTTA = :QBYB-SUMWGTTA,
              BYBFILE.BYB_IB_ACCRUED = :QBYB-IB-ACCRUED,
              BYBFILE.BYB_DLDISC_1 = :QBYB-DLDISC-1,
              BYBFILE.BYB_DLDISC_2 = :QBYB-DLDISC-2,
              BYBFILE.BYB_DLDISC_3 = :QBYB-DLDISC-3,
              BYBFILE.BYB_STD_POS = :QBYB-STD-POS,
              BYBFILE.BYB_30_DAY_MOVED = :QBYB-30-DAY-MOVED,
              BYBFILE.BYB_PROJ_INCREASE = :QBYB-PROJ-INCREASE,
              BYBFILE.BYB_PROJ_PROFIT = :QBYB-PROJ-PROFIT,
              BYBFILE.BYB_PROCEEDS_1 = :QBYB-PROCEEDS-1,
              BYBFILE.BYB_PROCEEDS_2 = :QBYB-PROCEEDS-2,
              BYBFILE.BYB_LOCKBOX_1 = :QBYB-LOCKBOX-1,
              BYBFILE.BYB_LOCKBOX_2 = :QBYB-LOCKBOX-2,
              BYBFILE.BYB_UNEARNED_AMT_1_1_1 =
                   :QBYB-UNEARNED-AMT-1-1-1,
              BYBFILE.BYB_UNEARNED_AMT_1_1_2 =
                   :QBYB-UNEARNED-AMT-1-1-2,
              BYBFILE.BYB_UNEARNED_AMT_1_1_3 =
                   :QBYB-UNEARNED-AMT-1-1-3,
              BYBFILE.BYB_UNEARNED_AMT_1_1_4 =
                   :QBYB-UNEARNED-AMT-1-1-4,
              BYBFILE.BYB_UNEARNED_AMT_1_1_5 =
                   :QBYB-UNEARNED-AMT-1-1-5,
              BYBFILE.BYB_UNEARNED_AMT_1_1_6 =
                   :QBYB-UNEARNED-AMT-1-1-6,
              BYBFILE.BYB_UNEARNED_AMT_1_1_7 =
                   :QBYB-UNEARNED-AMT-1-1-7,
              BYBFILE.BYB_UNEARNED_AMT_1_1_8 =
                   :QBYB-UNEARNED-AMT-1-1-8,
              BYBFILE.BYB_UNEARNED_AMT_1_1_9 =
                   :QBYB-UNEARNED-AMT-1-1-9,
              BYBFILE.BYB_UNEARNED_AMT_1_1_10 =
                   :QBYB-UNEARNED-AMT-1-1-10,
              BYBFILE.BYB_UNEARNED_AMT_1_2_1 =
                   :QBYB-UNEARNED-AMT-1-2-1,
              BYBFILE.BYB_UNEARNED_AMT_1_2_2 =
                   :QBYB-UNEARNED-AMT-1-2-2,
              BYBFILE.BYB_UNEARNED_AMT_1_2_3 =
                   :QBYB-UNEARNED-AMT-1-2-3,
              BYBFILE.BYB_UNEARNED_AMT_1_2_4 =
                   :QBYB-UNEARNED-AMT-1-2-4,
              BYBFILE.BYB_UNEARNED_AMT_1_2_5 =
                   :QBYB-UNEARNED-AMT-1-2-5,
              BYBFILE.BYB_UNEARNED_AMT_1_2_6 =
                   :QBYB-UNEARNED-AMT-1-2-6,
              BYBFILE.BYB_UNEARNED_AMT_1_2_7 =
                   :QBYB-UNEARNED-AMT-1-2-7,
              BYBFILE.BYB_UNEARNED_AMT_1_2_8 =
                   :QBYB-UNEARNED-AMT-1-2-8,
              BYBFILE.BYB_UNEARNED_AMT_1_2_9 =
                   :QBYB-UNEARNED-AMT-1-2-9,
              BYBFILE.BYB_UNEARNED_AMT_1_2_10 =
                   :QBYB-UNEARNED-AMT-1-2-10,
              BYBFILE.BYB_UNEARNED_AMT_2_1_1 =
                   :QBYB-UNEARNED-AMT-2-1-1,
              BYBFILE.BYB_UNEARNED_AMT_2_1_2 =
                   :QBYB-UNEARNED-AMT-2-1-2,
              BYBFILE.BYB_UNEARNED_AMT_2_1_3 =
                   :QBYB-UNEARNED-AMT-2-1-3,
              BYBFILE.BYB_UNEARNED_AMT_2_1_4 =
                   :QBYB-UNEARNED-AMT-2-1-4,
              BYBFILE.BYB_UNEARNED_AMT_2_1_5 =
                   :QBYB-UNEARNED-AMT-2-1-5,
              BYBFILE.BYB_UNEARNED_AMT_2_1_6 =
                   :QBYB-UNEARNED-AMT-2-1-6,
              BYBFILE.BYB_UNEARNED_AMT_2_1_7 =
                   :QBYB-UNEARNED-AMT-2-1-7,
              BYBFILE.BYB_UNEARNED_AMT_2_1_8 =
                   :QBYB-UNEARNED-AMT-2-1-8,
              BYBFILE.BYB_UNEARNED_AMT_2_1_9 =
                   :QBYB-UNEARNED-AMT-2-1-9,
              BYBFILE.BYB_UNEARNED_AMT_2_1_10 =
                   :QBYB-UNEARNED-AMT-2-1-10,
              BYBFILE.BYB_UNEARNED_AMT_2_2_1 =
                   :QBYB-UNEARNED-AMT-2-2-1,
              BYBFILE.BYB_UNEARNED_AMT_2_2_2 =
                   :QBYB-UNEARNED-AMT-2-2-2,
              BYBFILE.BYB_UNEARNED_AMT_2_2_3 =
                   :QBYB-UNEARNED-AMT-2-2-3,
              BYBFILE.BYB_UNEARNED_AMT_2_2_4 =
                   :QBYB-UNEARNED-AMT-2-2-4,
              BYBFILE.BYB_UNEARNED_AMT_2_2_5 =
                   :QBYB-UNEARNED-AMT-2-2-5,
              BYBFILE.BYB_UNEARNED_AMT_2_2_6 =
                   :QBYB-UNEARNED-AMT-2-2-6,
              BYBFILE.BYB_UNEARNED_AMT_2_2_7 =
                   :QBYB-UNEARNED-AMT-2-2-7,
              BYBFILE.BYB_UNEARNED_AMT_2_2_8 =
                   :QBYB-UNEARNED-AMT-2-2-8,
              BYBFILE.BYB_UNEARNED_AMT_2_2_9 =
                   :QBYB-UNEARNED-AMT-2-2-9,
              BYBFILE.BYB_UNEARNED_AMT_2_2_10 =
                   :QBYB-UNEARNED-AMT-2-2-10,
              BYBFILE.BYB_UNEARNED_AMT_3_1_1 =
                   :QBYB-UNEARNED-AMT-3-1-1,
              BYBFILE.BYB_UNEARNED_AMT_3_1_2 =
                   :QBYB-UNEARNED-AMT-3-1-2,
              BYBFILE.BYB_UNEARNED_AMT_3_1_3 =
                   :QBYB-UNEARNED-AMT-3-1-3,
              BYBFILE.BYB_UNEARNED_AMT_3_1_4 =
                   :QBYB-UNEARNED-AMT-3-1-4,
              BYBFILE.BYB_UNEARNED_AMT_3_1_5 =
                   :QBYB-UNEARNED-AMT-3-1-5,
              BYBFILE.BYB_UNEARNED_AMT_3_1_6 =
                   :QBYB-UNEARNED-AMT-3-1-6,
              BYBFILE.BYB_UNEARNED_AMT_3_1_7 =
                   :QBYB-UNEARNED-AMT-3-1-7,
              BYBFILE.BYB_UNEARNED_AMT_3_1_8 =
                   :QBYB-UNEARNED-AMT-3-1-8,
              BYBFILE.BYB_UNEARNED_AMT_3_1_9 =
                   :QBYB-UNEARNED-AMT-3-1-9,
              BYBFILE.BYB_UNEARNED_AMT_3_1_10 =
                   :QBYB-UNEARNED-AMT-3-1-10,
              BYBFILE.BYB_UNEARNED_AMT_3_2_1 =
                   :QBYB-UNEARNED-AMT-3-2-1,
              BYBFILE.BYB_UNEARNED_AMT_3_2_2 =
                   :QBYB-UNEARNED-AMT-3-2-2,
              BYBFILE.BYB_UNEARNED_AMT_3_2_3 =
                   :QBYB-UNEARNED-AMT-3-2-3,
              BYBFILE.BYB_UNEARNED_AMT_3_2_4 =
                   :QBYB-UNEARNED-AMT-3-2-4,
              BYBFILE.BYB_UNEARNED_AMT_3_2_5 =
                   :QBYB-UNEARNED-AMT-3-2-5,
              BYBFILE.BYB_UNEARNED_AMT_3_2_6 =
                   :QBYB-UNEARNED-AMT-3-2-6,
              BYBFILE.BYB_UNEARNED_AMT_3_2_7 =
                   :QBYB-UNEARNED-AMT-3-2-7,
              BYBFILE.BYB_UNEARNED_AMT_3_2_8 =
                   :QBYB-UNEARNED-AMT-3-2-8,
              BYBFILE.BYB_UNEARNED_AMT_3_2_9 =
                   :QBYB-UNEARNED-AMT-3-2-9,
              BYBFILE.BYB_UNEARNED_AMT_3_2_10 =
                   :QBYB-UNEARNED-AMT-3-2-10
             WHERE BYBFILE.BYB_DATE_CCYYMM = :QBYB-DATE-CCYYMM
               AND BYBFILE.BYB_BRNO = :QBYB-BRNO
               AND BYBFILE.BYB_CLASS = :QBYB-CLASS
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-BYB1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

           MOVE '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       DELETE-BYB1-FILE.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           PERFORM DELETE-IT.
           MOVE BYB-PATH-SQL TO E-FILE.
           MOVE BYB1-KEY TO E-KEYX.
           PERFORM SET-BYB-FIELDS.

           EXEC SQL
            DELETE FROM DBO.BYBFILE
             WHERE BYBFILE.BYB_DATE_CCYYMM = :QBYB-DATE-CCYYMM
               AND BYBFILE.BYB_BRNO = :QBYB-BRNO
               AND BYBFILE.BYB_CLASS = :QBYB-CLASS
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-BYB1-FILE.

           MOVE '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       CLOSE-BYB1-FILE.

      *-----------------------------------------------------------------
       CLOSE-BY1-CONNECTION.

           IF BY1-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION BY1_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT BY1_CONNECT END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE "N" TO BY1-CONN-OPEN-SW.

      *-----------------------------------------------------------------

       COPY "LIBLP/LPCA1RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
   
      * COPY "LIBGB/GBRC1IN.CPY".
      * COPYMEMBER: LIBGB/GBRC1IN
       LOAD-RC1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( RC-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( RC-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO RC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO RC-ALL-MACHINE
              MOVE RC-ALL-PATH             TO RC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( RC-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO RC-ALL-BASE
              MOVE FILPATH-MACHINE         TO RC-ALL-MACHINE
              MOVE RC-ALL-PATH             TO RC-PATH.

      *-----------------------------------------------------------------
       OPEN-RC1-FILE.
           PERFORM LOAD-RC1-FILE.
           PERFORM OPEN-IT.
           MOVE RC-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-RC1-FILE.
           PERFORM START-IT.
           MOVE RC-PATH-SQL TO E-FILE.
           MOVE RC1-KEY TO E-KEYX.

           IF ( RC1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-RC1-FILE.

           IF ( QRC1-WEND-BRNO = ZEROES    ) OR
              ( QRC1-WEND-BRNO NOT NUMERIC )
              MOVE RC-BRNO              TO QRC1-WBEG-BRNO
              MOVE ALL "9"              TO QRC1-WEND-BRNO

              MOVE RC-TRANS-DATE        TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QRC1-WBEG-TRANS-DATE

              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QRC1-WEND-TRANS-DATE.

           EXEC SQL
            DECLARE RC1_CURSOR CURSOR FOR
            SELECT RCFILE.RC_STATUS,
                   RCFILE.RC_BRNO,
                   CAST(RCFILE.RC_TRANS_DATE AS VARCHAR(10)),
                   CAST(RCFILE.RC_LTOUCH_DATE AS VARCHAR(10)),
                   RCFILE.RC_RECAP_FG,
                   RCFILE.RC_GL_FG,
                   RCFILE.RC_EOD_BATCH_FG,
                   RCFILE.RC_NIGHTLY_FUNDS_FG,
                   RCFILE.RC_EOM1_BATCH_FG,
                   RCFILE.RC_EOM_UPDATE_FG,
                   RCFILE.RC_EOM2_BATCH_FG,
                   RCFILE.RC_BS_P2P3_INBAL_FG,
                   RCFILE.RC_DEPOSIT,
                   RCFILE.RC_OVER_SHORT,
                   RCFILE.RC_CHECKS,
                   CAST(RCFILE.RC_OPENED_DATE AS VARCHAR(10)),
                   RCFILE.RC_OPENED_TIME,
                   RCFILE.RC_OP_USERID,
                   CAST(RCFILE.RC_CLOSED_DATE AS VARCHAR(10)),
                   RCFILE.RC_CLOSED_TIME,
                   RCFILE.RC_CL_USERID,
                   CAST(RCFILE.RC_UPDATED_DATE AS VARCHAR(10)),
                   RCFILE.RC_UPDATED_TIME,
                   CAST(RCFILE.RC_UPDATED_USERID AS VARCHAR(10)),
                   CAST(RCFILE.RC_EOD_BATCH_DATE AS VARCHAR(10)),
                   RCFILE.RC_EOD_BATCH_TIME,
                   CAST(RCFILE.RC_FUNDS_DATE AS VARCHAR(10)),
                   RCFILE.RC_FUNDS_TIME,
                   CAST(RCFILE.RC_EARNINGS_DATE AS VARCHAR(10)),
                   RCFILE.RC_EARNINGS_TIME,
                   CAST(RCFILE.RC_EOM_UPDATED_DATE AS VARCHAR(10)),
                   RCFILE.RC_EOM_UPDATED_TIME,
                   CAST(RCFILE.RC_EOM2_BATCH_DATE AS VARCHAR(10)),
                   RCFILE.RC_EOM2_BATCH_TIME,
                   CAST(RCFILE.RC_CHECK_DATE AS VARCHAR(10)),
                   RCFILE.RC_CHECK_TIME,
                   RCFILE.RC_LAST_ACCRUE,
                   RCFILE.RC_ERROR_MSG,
                   RCFILE.RC_BANK_BAL,
                   RCFILE.RC_CHECK_DEPOSIT
            FROM DBO.RCFILE
            WHERE RCFILE.RC_BRNO >= :QRC1-WBEG-BRNO
              AND RCFILE.RC_BRNO <= :QRC1-WEND-BRNO
              AND RCFILE.RC_TRANS_DATE >= :QRC1-WBEG-TRANS-DATE 
              AND RCFILE.RC_TRANS_DATE <= :QRC1-WEND-TRANS-DATE
            ORDER BY RCFILE.RC_BRNO,
                     RCFILE.RC_TRANS_DATE
           END-EXEC.

           EXEC SQL
               OPEN RC1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QRC1-WBEG-BRNO
                          QRC1-WEND-BRNO.

           MOVE SPACES TO QRC1-WBEG-TRANS-DATE
                          QRC1-WEND-TRANS-DATE.

           MOVE STAT---GOOD TO RC1-CURSOR-STAT.

      *-----------------------------------------------------------------
       START-RC1-FILE-NOT-GREATER.
           PERFORM START-IT.
           MOVE RC-PATH-SQL TO E-FILE.
           MOVE RC1-KEY TO E-KEYX.

           IF ( RC1-CURSOR-NG-STAT-GOOD )
              PERFORM CLOSE-RC1-FILE.

           IF ( QRC1-WEND-BRNO = ZEROES    ) OR
              ( QRC1-WEND-BRNO NOT NUMERIC )
              MOVE ALL "0"              TO QRC1-WBEG-BRNO
              MOVE RC-BRNO              TO QRC1-WEND-BRNO

              MOVE EXT-JULIAN-BEG       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QRC1-WBEG-TRANS-DATE
              MOVE RC-TRANS-DATE        TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QRC1-WEND-TRANS-DATE.

           EXEC SQL
            DECLARE RC1_CURSOR_NG CURSOR FOR
            SELECT RCFILE.RC_STATUS,
                   RCFILE.RC_BRNO,
                   CAST(RCFILE.RC_TRANS_DATE AS VARCHAR(10)),
                   CAST(RCFILE.RC_LTOUCH_DATE AS VARCHAR(10)),
                   RCFILE.RC_RECAP_FG,
                   RCFILE.RC_GL_FG,
                   RCFILE.RC_EOD_BATCH_FG,
                   RCFILE.RC_NIGHTLY_FUNDS_FG,
                   RCFILE.RC_EOM1_BATCH_FG,
                   RCFILE.RC_EOM_UPDATE_FG,
                   RCFILE.RC_EOM2_BATCH_FG,
                   RCFILE.RC_BS_P2P3_INBAL_FG,
                   RCFILE.RC_DEPOSIT,
                   RCFILE.RC_OVER_SHORT,
                   RCFILE.RC_CHECKS,
                   CAST(RCFILE.RC_OPENED_DATE AS VARCHAR(10)),
                   RCFILE.RC_OPENED_TIME,
                   RCFILE.RC_OP_USERID,
                   CAST(RCFILE.RC_CLOSED_DATE AS VARCHAR(10)),
                   RCFILE.RC_CLOSED_TIME,
                   RCFILE.RC_CL_USERID,
                   CAST(RCFILE.RC_UPDATED_DATE AS VARCHAR(10)),
                   RCFILE.RC_UPDATED_TIME,
                   CAST(RCFILE.RC_UPDATED_USERID AS VARCHAR(10)),
                   CAST(RCFILE.RC_EOD_BATCH_DATE AS VARCHAR(10)),
                   RCFILE.RC_EOD_BATCH_TIME,
                   CAST(RCFILE.RC_FUNDS_DATE AS VARCHAR(10)),
                   RCFILE.RC_FUNDS_TIME,
                   CAST(RCFILE.RC_EARNINGS_DATE AS VARCHAR(10)),
                   RCFILE.RC_EARNINGS_TIME,
                   CAST(RCFILE.RC_EOM_UPDATED_DATE AS VARCHAR(10)),
                   RCFILE.RC_EOM_UPDATED_TIME,
                   CAST(RCFILE.RC_EOM2_BATCH_DATE AS VARCHAR(10)),
                   RCFILE.RC_EOM2_BATCH_TIME,
                   CAST(RCFILE.RC_CHECK_DATE AS VARCHAR(10)), 
                   RCFILE.RC_CHECK_TIME,
                   RCFILE.RC_LAST_ACCRUE,
                   RCFILE.RC_ERROR_MSG,
                   RCFILE.RC_BANK_BAL,
                   RCFILE.RC_CHECK_DEPOSIT
            FROM DBO.RCFILE
            WHERE RCFILE.RC_BRNO >= :QRC1-WBEG-BRNO
              AND RCFILE.RC_BRNO <= :QRC1-WEND-BRNO
              AND RCFILE.RC_TRANS_DATE >= :QRC1-WBEG-TRANS-DATE 
              AND RCFILE.RC_TRANS_DATE <= :QRC1-WEND-TRANS-DATE
            ORDER BY RCFILE.RC_BRNO DESC,
                     RCFILE.RC_TRANS_DATE DESC
           END-EXEC.

           EXEC SQL
               OPEN RC1_CURSOR_NG
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QRC1-WBEG-BRNO
                          QRC1-WEND-BRNO.

           MOVE SPACES TO QRC1-WBEG-TRANS-DATE
                          QRC1-WEND-TRANS-DATE.

           MOVE STAT---GOOD TO RC1-CURSOR-NG-STAT.

      *-----------------------------------------------------------------
       READ-RC1-FILE.
           PERFORM READ-IT.
           MOVE RC-PATH-SQL TO E-FILE.
           MOVE RC1-KEY TO E-KEYX.
           INITIALIZE QRC-REC.

           MOVE RC-BRNO          TO QRC-BRNO.  
           MOVE RC-TRANS-DATE    TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QRC-TRANS-DATE.
           MOVE RC-STATUS        TO QRC-STATUS.

           EXEC SQL
            SELECT RCFILE.RC_STATUS,
                   RCFILE.RC_BRNO,
                   CAST(RCFILE.RC_TRANS_DATE AS VARCHAR(10)),
                   CAST(RCFILE.RC_LTOUCH_DATE AS VARCHAR(10)),
                   RCFILE.RC_RECAP_FG,
                   RCFILE.RC_GL_FG,
                   RCFILE.RC_EOD_BATCH_FG,
                   RCFILE.RC_NIGHTLY_FUNDS_FG,
                   RCFILE.RC_EOM1_BATCH_FG,
                   RCFILE.RC_EOM_UPDATE_FG,
                   RCFILE.RC_EOM2_BATCH_FG,
                   RCFILE.RC_BS_P2P3_INBAL_FG,
                   RCFILE.RC_DEPOSIT,
                   RCFILE.RC_OVER_SHORT,
                   RCFILE.RC_CHECKS,
                   CAST(RCFILE.RC_OPENED_DATE AS VARCHAR(10)),
                   RCFILE.RC_OPENED_TIME,
                   RCFILE.RC_OP_USERID,
                   CAST(RCFILE.RC_CLOSED_DATE AS VARCHAR(10)),
                   RCFILE.RC_CLOSED_TIME,
                   RCFILE.RC_CL_USERID,
                   CAST(RCFILE.RC_UPDATED_DATE AS VARCHAR(10)),
                   RCFILE.RC_UPDATED_TIME,
                   CAST(RCFILE.RC_UPDATED_USERID AS VARCHAR(10)),
                   CAST(RCFILE.RC_EOD_BATCH_DATE AS VARCHAR(10)),
                   RCFILE.RC_EOD_BATCH_TIME,
                   CAST(RCFILE.RC_FUNDS_DATE AS VARCHAR(10)),
                   RCFILE.RC_FUNDS_TIME,
                   CAST(RCFILE.RC_EARNINGS_DATE AS VARCHAR(10)),
                   RCFILE.RC_EARNINGS_TIME,
                   CAST(RCFILE.RC_EOM_UPDATED_DATE AS VARCHAR(10)),
                   RCFILE.RC_EOM_UPDATED_TIME,
                   CAST(RCFILE.RC_EOM2_BATCH_DATE AS VARCHAR(10)),
                   RCFILE.RC_EOM2_BATCH_TIME,
                   CAST(RCFILE.RC_CHECK_DATE AS VARCHAR(10)),
                   RCFILE.RC_CHECK_TIME,
                   RCFILE.RC_LAST_ACCRUE,
                   RCFILE.RC_ERROR_MSG,
                   RCFILE.RC_BANK_BAL,
                   RCFILE.RC_CHECK_DEPOSIT
            INTO :QRC-REC
            FROM DBO.RCFILE
            WHERE RCFILE.RC_BRNO = :QRC-BRNO
              AND RCFILE.RC_TRANS_DATE = :QRC-TRANS-DATE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-RC1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-RC-FIELDS.

      *-----------------------------------------------------------------
       READ-RC1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE RC-PATH-SQL TO E-FILE.
           MOVE RC1-KEY TO E-KEYX.
           INITIALIZE QRC-REC.

           IF ( RC1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH RC1_CURSOR INTO :QRC-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-RC1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-RC-FIELDS.

      *-----------------------------------------------------------------
       READ-RC1-FILE-PREVIOUS.
           PERFORM READ-IT.
           MOVE RC-PATH-SQL TO E-FILE.
           MOVE RC1-KEY TO E-KEYX.
           INITIALIZE QRC-REC.

           IF ( RC1-CURSOR-NG-STAT-GOOD )
              EXEC SQL
                   FETCH RC1_CURSOR_NG INTO :QRC-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDPVCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-RC1-FILE-PREVIOUS.

           IF ( IO-FG = 0 )
              PERFORM GET-RC-FIELDS.

      *-----------------------------------------------------------------
       WRITE-RC1-FILE.

           ACCEPT RC-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE RC-PATH-SQL TO E-FILE.
           MOVE RC1-KEY TO E-KEYX.
           PERFORM SET-RC-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            INSERT INTO DBO.RCFILE
             (RCFILE.RC_STATUS,
              RCFILE.RC_BRNO,
              RCFILE.RC_TRANS_DATE,
              RCFILE.RC_LTOUCH_DATE,
              RCFILE.RC_RECAP_FG,
              RCFILE.RC_GL_FG,
              RCFILE.RC_EOD_BATCH_FG,
              RCFILE.RC_NIGHTLY_FUNDS_FG,
              RCFILE.RC_EOM1_BATCH_FG,
              RCFILE.RC_EOM_UPDATE_FG,
              RCFILE.RC_EOM2_BATCH_FG,
              RCFILE.RC_BS_P2P3_INBAL_FG,
              RCFILE.RC_DEPOSIT,
              RCFILE.RC_OVER_SHORT,
              RCFILE.RC_CHECKS,
              RCFILE.RC_OPENED_DATE,
              RCFILE.RC_OPENED_TIME,
              RCFILE.RC_OP_USERID,
              RCFILE.RC_CLOSED_DATE,
              RCFILE.RC_CLOSED_TIME,
              RCFILE.RC_CL_USERID,
              RCFILE.RC_UPDATED_DATE,
              RCFILE.RC_UPDATED_TIME,
              RCFILE.RC_UPDATED_USERID,
              RCFILE.RC_EOD_BATCH_DATE,
              RCFILE.RC_EOD_BATCH_TIME,
              RCFILE.RC_FUNDS_DATE,
              RCFILE.RC_FUNDS_TIME,
              RCFILE.RC_EARNINGS_DATE,
              RCFILE.RC_EARNINGS_TIME,
              RCFILE.RC_EOM_UPDATED_DATE,
              RCFILE.RC_EOM_UPDATED_TIME,
              RCFILE.RC_EOM2_BATCH_DATE,
              RCFILE.RC_EOM2_BATCH_TIME,
              RCFILE.RC_CHECK_DATE,
              RCFILE.RC_CHECK_TIME,
              RCFILE.RC_LAST_ACCRUE,
              RCFILE.RC_ERROR_MSG,
              RCFILE.RC_BANK_BAL,
              RCFILE.RC_CHECK_DEPOSIT)
            VALUES
             (:QRC-STATUS,
              :QRC-BRNO,
              :QRC-TRANS-DATE,
              :QRC-LTOUCH-DATE,
              :QRC-RECAP-FG,
              :QRC-GL-FG,
              :QRC-EOD-BATCH-FG,
              :QRC-NIGHTLY-FUNDS-FG,
              :QRC-EOM1-BATCH-FG,
              :QRC-EOM-UPDATE-FG,
              :QRC-EOM2-BATCH-FG,
              :QRC-BS-P2P3-INBAL-FG,
              :QRC-DEPOSIT,
              :QRC-OVER-SHORT,
              :QRC-CHECKS,
              :QRC-OPENED-DATE,
              :QRC-OPENED-TIME,
              :QRC-OP-USERID,
              :QRC-CLOSED-DATE,
              :QRC-CLOSED-TIME,
              :QRC-CL-USERID,
              :QRC-UPDATED-DATE,
              :QRC-UPDATED-TIME,
              :QRC-UPDATED-USERID,
              :QRC-EOD-BATCH-DATE,
              :QRC-EOD-BATCH-TIME,
              :QRC-FUNDS-DATE,
              :QRC-FUNDS-TIME,
              :QRC-EARNINGS-DATE,
              :QRC-EARNINGS-TIME,
              :QRC-EOM-UPDATED-DATE,
              :QRC-EOM-UPDATED-TIME,
              :QRC-EOM2-BATCH-DATE,
              :QRC-EOM2-BATCH-TIME,
              :QRC-CHECK-DATE,
              :QRC-CHECK-TIME,
              :QRC-LAST-ACCRUE,
              :QRC-ERROR-MSG,
              :QRC-BANK-BAL,
              :QRC-CHECK-DEPOSIT)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-RC1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

           MOVE '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       REWRITE-RC1-FILE.

           ACCEPT RC-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE RC-PATH-SQL TO E-FILE.
           MOVE RC1-KEY TO E-KEYX.
           PERFORM SET-RC-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            UPDATE DBO.RCFILE
             SET
              RCFILE.RC_STATUS = :QRC-STATUS,
              RCFILE.RC_BRNO = :QRC-BRNO,
              RCFILE.RC_TRANS_DATE = :QRC-TRANS-DATE,
              RCFILE.RC_LTOUCH_DATE = :QRC-LTOUCH-DATE,
              RCFILE.RC_RECAP_FG = :QRC-RECAP-FG,
              RCFILE.RC_GL_FG = :QRC-GL-FG,
              RCFILE.RC_EOD_BATCH_FG = :QRC-EOD-BATCH-FG,
              RCFILE.RC_NIGHTLY_FUNDS_FG = :QRC-NIGHTLY-FUNDS-FG,
              RCFILE.RC_EOM1_BATCH_FG = :QRC-EOM1-BATCH-FG,
              RCFILE.RC_EOM_UPDATE_FG = :QRC-EOM-UPDATE-FG,
              RCFILE.RC_EOM2_BATCH_FG = :QRC-EOM2-BATCH-FG,
              RCFILE.RC_BS_P2P3_INBAL_FG = :QRC-BS-P2P3-INBAL-FG,
              RCFILE.RC_DEPOSIT = :QRC-DEPOSIT,
              RCFILE.RC_OVER_SHORT = :QRC-OVER-SHORT,
              RCFILE.RC_CHECKS = :QRC-CHECKS,
              RCFILE.RC_OPENED_DATE = :QRC-OPENED-DATE,
              RCFILE.RC_OPENED_TIME = :QRC-OPENED-TIME,
              RCFILE.RC_OP_USERID = :QRC-OP-USERID,
              RCFILE.RC_CLOSED_DATE = :QRC-CLOSED-DATE,
              RCFILE.RC_CLOSED_TIME = :QRC-CLOSED-TIME,
              RCFILE.RC_CL_USERID = :QRC-CL-USERID,
              RCFILE.RC_UPDATED_DATE = :QRC-UPDATED-DATE,
              RCFILE.RC_UPDATED_TIME = :QRC-UPDATED-TIME,
              RCFILE.RC_UPDATED_USERID = :QRC-UPDATED-USERID,
              RCFILE.RC_EOD_BATCH_DATE = :QRC-EOD-BATCH-DATE,
              RCFILE.RC_EOD_BATCH_TIME = :QRC-EOD-BATCH-TIME,
              RCFILE.RC_FUNDS_DATE = :QRC-FUNDS-DATE,
              RCFILE.RC_FUNDS_TIME = :QRC-FUNDS-TIME,
              RCFILE.RC_EARNINGS_DATE = :QRC-EARNINGS-DATE,
              RCFILE.RC_EARNINGS_TIME = :QRC-EARNINGS-TIME,
              RCFILE.RC_EOM_UPDATED_DATE = :QRC-EOM-UPDATED-DATE,
              RCFILE.RC_EOM_UPDATED_TIME = :QRC-EOM-UPDATED-TIME,
              RCFILE.RC_EOM2_BATCH_DATE = :QRC-EOM2-BATCH-DATE,
              RCFILE.RC_EOM2_BATCH_TIME = :QRC-EOM2-BATCH-TIME,
              RCFILE.RC_CHECK_DATE = :QRC-CHECK-DATE,
              RCFILE.RC_CHECK_TIME = :QRC-CHECK-TIME,
              RCFILE.RC_LAST_ACCRUE = :QRC-LAST-ACCRUE,
              RCFILE.RC_ERROR_MSG = :QRC-ERROR-MSG,
              RCFILE.RC_BANK_BAL = :QRC-BANK-BAL,
              RCFILE.RC_CHECK_DEPOSIT = :QRC-CHECK-DEPOSIT
            WHERE RCFILE.RC_BRNO = :QRC-BRNO
              AND RCFILE.RC_TRANS_DATE = :QRC-TRANS-DATE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-RC1-FILE.

      *    IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION.

           MOVE '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       DELETE-RC1-FILE.

           PERFORM DELETE-IT.
           MOVE RC-PATH-SQL TO E-FILE.
           MOVE RC1-KEY TO E-KEYX.
           PERFORM SET-RC-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW. *> USE UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            DELETE FROM DBO.RCFILE
            WHERE RCFILE.RC_BRNO = :QRC-BRNO
              AND RCFILE.RC_TRANS_DATE = :QRC-TRANS-DATE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-RC1-FILE.

           MOVE '1' TO SQL-UPDATE-CONN-USE-SW. *> USE C1 CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

      *-----------------------------------------------------------------
       CLOSE-RC1-FILE.

           PERFORM CLOSE-IT.

           IF ( RC1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE RC1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO RC1-CURSOR-STAT.

           IF ( RC1-CURSOR-NG-STAT-GOOD )
              EXEC SQL
                   CLOSE RC1_CURSOR_NG
              END-EXEC
              MOVE STAT---BAD TO RC1-CURSOR-NG-STAT.

      *-----------------------------------------------------------------

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      **************************************************
       END-PROGRAM SECTION.
      **************************************************
       END-RUN.
           IF ERROR-FG = "Y"
              MOVE "END OF DAY UPDATE UNSUCCESSFUL"
                    TO HOLD-ER-MESSAGE
           ELSE IF ERROR-FG = "I"
              MOVE "END OF DAY UPDATE NOT RUN, MISSING GL INTERFACE"
                    TO HOLD-ER-MESSAGE
           ELSE IF ERROR-FG = "G"
            MOVE "END OF DAY UPDATE SUCCESSFUL, BUT MISSING GL MASTER!"
                    TO HOLD-ER-MESSAGE
           ELSE
              MOVE "END OF DAY UPDATE SUCCESSFUL"
                    TO HOLD-ER-MESSAGE.
           PERFORM CREATE-LOG.
           IF ERROR-FG = "N"
              GO TO SUCCESSFUL-END.
      * IF ERRORS, CREATE ERROR TAG IN SPOOL DIRECTORY - EOQRPT WILL
      * CHECK FOR THIS FILE & STOP USER FROM RELEASING JOURNALS
       CREATE-ERROR-TAG.
           MOVE "/0000ERFILE"   TO EO-SPOOL-FILE.
           MOVE EO-SPOOL        TO ER-ALL-PATH.
           PERFORM LOAD-ER-FILE.
           MOVE ER-PATH         TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT = "00"
              PERFORM OPEN-ER-FILE
           ELSE
              PERFORM OPEN-ER-FILE-OUTPUT.
      *    MOVE 9 TO ERRCD.
       SUCCESSFUL-END.
           PERFORM CLOSE-FILES.

           MOVE ED-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

       EXIT-PROG.
       STOP-RUN.
           MOVE ERRCD TO LPEOD5-ERRCD.
           EXIT PROGRAM.
