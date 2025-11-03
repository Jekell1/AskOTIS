       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LONPF5.
       DATE-WRITTEN. 012486.
      ******************************************************************
      *
      *                    (LP)
      *  DESCRIPTION:  LOAN PROCESSING
      *                TRANSACTION UPDATE
      *                PROGRAM 5 - "TS" TRCD ONLY
      *
      *=================================================================
      * REV:
      * 042995 SLC ADDED PLTYPE LOGIC.
      * 061495 MJD SET TP-USERID AND TP-POSTTIME.
      * 062795 MJD ADDED COPY MEMBERS LIBLP/POSTDATE AND POSTDATEW.
      *            COMPUTE POSTTIME, POSTDATE AND USERID FOR LP AND
      *            TR WRITES.
      * JTG 071995 ADDED LOGIC FOR LN-PRLNAMT & LN-PRPDUE-DAYS
      * JTG 072195 ADDED LOGIC FOR LPSRCD
      * JTG 072895 ADDED LOGIC FOR DELSUB-SET ROUTINE
      * MJD 081195 ADDED GP-FILE INFO FOR LIBLP/LPSRCD :
      *                  GP-THE-ONLY-SF-SRCD
      * MJD 081595 ADDED GP-THE-ONLY-SF-SRCD LOGIC FOR DS UPDATES
      * MJD 091895 ADDED DS-DATE LOGIC.
      * MJD 021196 ADDED UPDATES OF PL TABLE, LOANS SOLD,TRANS/PURCH,
      *             DLR RESERVES
      * MJD 071696 ADDED LIBLP/LPUPWK FOR CHANGES IN LIBLP/AGEING
      * MJD 071896 ADDED TP-UNITPER-CD AND TP-UNITPER-FREQ
      * JTG 101196 ADDED 'LXE-WORKER' & 'LXE-WORKER2' FOR LPCERN-- A50 VERSION
      * JTG 110696 CHANGED TO UPDATE DSFILE DELINQUENCY
      * MJD 121096 ADDED READ OF BWFILE FOR LPSTAT-DISPLAY.
      * JTG 022797 ADDED TEMPORARY READ OF SPR, UNTIL LN-SPRCLASS LOGIC
      *            IS ADDED
      * BAH 070997 SET LP-REVTRCD, LP-REVSEQNO AND TP-REVTRCD, TP-REVSEQNO
      * JTG 081497 ADDED SET OF LN-GIFTNET WORLD A90
      *               AND PASSED LC-GIFTNET TO "CL/LEDGER" CALL
      * JTG 082297 CHANGED TO SET DT-REVERSE-TRCD TO 'TX' ON A REVERSAL
      * GLF 090297 REMOVED ALL USE OF GLOBAL
      * GLF 090897 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      * MJD 091797 REMOVED DELSUB-SET LOGIC, ADDED LPPDUE(W)
      * GLF 101097 ADDED CTRL N
      * MJD 101797 MOVED WK-UPDATE-BUF TO LP01WK
      * BAH 111197 ADDED HOLD-SP1-KEY TO CL/LEDGER LINKAGE CALL
      * MJD 032098 ADDED GETENV(W) FOR USE IN LIBLP/POSTDATE
      * MJD 051998 REMOVED XY-PASTDUE.
      * JTG 061598 ADDED LOGIC TO UPDATE BANKRUPT DELINQUENT STATISTICS
      * BAH 062598 CHANGED TP-TILL PIC 9 TO TP-TILLNO PIC X
      * JTG 063098 EXPANDED DS-APP-TYPE-TBL TO 3 LEVELS
      * KEC 090498 ADDED 'MOVE LC-LOANTYPE TO LN-LOANTYPE';
      *             FOR REGENCY REVOLVING.
      * MJD 990420 CHANGED TO INCLUDE/EXCLUDE O2 IN DELINQ BASED
      *            ON GP-DELINQ-WITH-O2
      * JTG 991011 CHANGED SETUP-LINE FOR 'AP' DISPLAY REVOLVING, #47 REGENCY
      *            AND CHANGED CALL TO CL/LEDGER.C TO PASS LC-REC
      * JTG 000327 MODIFIED TO UPDATE UNEARNED STATISTICS, MERCURY #223
      * MJD 001016 ADDED UPDATE OF TP-COLLECTOR/LP-COLLECTOR (BAY PR#1317)
      * MG  010201 REMOVED "01 WK-LXE" AND ADDED LIBLP/LPWKLXE.
      * CS  151208 CHANGED HOLD-KEY 9(13), HOLD-1ST-LP 9(13) FROM 9(12) BECAUSE
      *            LP-SEQNO IS NOW 9(5), CHANGED SAVE-LP-SEQNO TO 9(5)
      *            CHANGED HOLD-KEY TO HOLD-LP-KEY
      *            SAVE COPY IS LONPF5.151208
      * BAH 160125 SPLIT OUT LTFILE, PD0003
      *
      * KEC 2016.0219 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED KEYS & RECS FOR FD-REC.
      *         CHANGED FMNO/FORMNO/NEXTFM/SETFM FROM 3 TO 4 BYTES.
      *   CS 160222 REMOVE COPY LPWKLXE, ITS AN EARNINGS COPY MEMBER THAT IS
      *             PASSED IN LOAN PROCESSING VIA LP01WK USING WK-LX2-REC. 
      *             REPLACED WK-LX2-REC WITH WK-LXE-REC IN LP01WK 
      *             MOVED LPWKLXE TO LPWKLXE.NO
      *             SAVED VERSION LONPF5.160222
      *  BAH 160304 CHANGED TILLNO TO NUMERIC BUT NO ROOM ON SCREEN FOR 2 DIGITS
      *  BLM 160310 FIX FOR NEW TY DATE FORMAT, INCREASE HOLD-TY-CLASS &
      *             HOLD-TY-DATE, PD0003
      *  BLM 160512 FIX FOR NEW DS DATE FORMAT, FIX DS-UNEARNED, PD0003
      *  CS 160830 CHANGED TEST ON SAVE-DT-SEQ < 9999 TO < 999, SAVE-DT-SEQ
      *            IS MOVED TO DT-SEQ WHICH IS ONLY 9(3), CHANGED SAVE-DT-SEQ
      *            FROM PIC 9(4) TO PIC 9(3)
      *
      * KEC 2016.1026 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED USING HOLD-TR1-KEY.
      *         ADDED LIBLP/LPWSTR (NEW) FOR LPWSTR-TR1-KEY (HOLD/SAVE).
      *
      * KEC 2017.0105 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED DS-DATE TO DS-DATE-CCYYMM FOR CONSISTENCY
      *          WITH THE DATE FIELDS IN BY & UD FILE.
      *
      * BAH 170615 ACTIVATE 8 DIGIT DATES, PD0006
      *
      * KEC 2017.0727 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED HOLD-LP-KEY TO SAVE-LP1-KEY; COULD NOT RENAME TO
      *         HOLD-LP1-KEY SINCE ALREADY USED.
      * BAH 171129 FIXED WK-LXE-REC
      * BAH 181227 GLOBAL LPFILE
      * BAH 190116 GLOBAL LXFILE
      *  CS 090606 IN TESTS ON LXE-EARN() AND LXE-EARN(4-7)
      *  CS 190611 FIXED SETUP OF LXT-EARN AND LXP-EARN. THE LXT/LXP
      *            EARNINGS COMPONENTS NOW OCCUR 20 INSTEAD OF 12 (A15),
      *            LXT-EARN(7) IS NOW LXT-EARN(11)
      * BAH 20190625 REMOVED LC-REVOLVING, PT#0153
      * BLM 20210810 REMOVE TP-CODE, TP-POOLID NO LONGER IN KEY
      * 20220524 BAH ADDED ATB-ACTIONCD2, SECURITIZATION #1551
      * BAH 20220803 HARD CODED START
      * BAH 20231019 CREATE XTRP AND XNO FOR EVERY TRANSACTION IN
      *              R1/FTP/OUT/PAYS, FOR REPAY #1629
      * BAH 20231218 CREATE XTRP AND XNO WITH OWNING BRANCH ONLY, IN THE PATH
      *              FILENAME AND ALSO INSIDE THE FILE. #1629A
      * BAH 2024.0130 CALL CRNOR2 AND WRITE NOTICE RECORD
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBLP/LPFSWK.CPY".
       COPY "LIBLP/LPFSREPAYTRP.CPY".
       COPY "LIBLP/LPFSREPAYNO.CPY".
       I-O-CONTROL.

       DATA DIVISION.
       FILE SECTION.
       COPY "LIBLP/LPFDWK.CPY".
       COPY "LIBLP/LPFDREPAYTRP.CPY".
       COPY "LIBLP/LPFDREPAYNO.CPY".

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/LONPF5.CBL S35 01/30/24-12:06:58 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01LX.CPY".
       COPY "LIBLP/LP01LX_SQL.CPY".
       COPY "LIBLP/LPWSLX.CPY".
       COPY "LIBLP/LP01LXE.CPY".
       COPY "LIBLP/LP01LXE_SQL.CPY".
       COPY "LIBLP/LPWSLXE.CPY".
       COPY "LIBLP/LP01LXG.CPY".
       COPY "LIBLP/LP01LXG_SQL.CPY".
       COPY "LIBLP/LPWSLXG.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01TY.CPY".
       COPY "LIBLP/LP01TY_SQL.CPY".
       COPY "LIBLP/LPWSTY.CPY".
       COPY "LIBLP/LP01DS.CPY".
       COPY "LIBLP/LP01DS_SQL.CPY".
       COPY "LIBLP/LPWSDS.CPY".
       COPY "LIBLP/LP01DT.CPY".
       COPY "LIBLP/LP01DT_SQL.CPY".
       COPY "LIBLP/LPWSDT.CPY".
       COPY "LIBLP/LP01TRP.CPY".
       COPY "LIBLP/LP01TRP_SQL.CPY".
       COPY "LIBLP/LPWSTRP.CPY".
       COPY "LIBLP/LP01TRX.CPY".
       COPY "LIBLP/LP01TRX_SQL.CPY".
       COPY "LIBLP/LPWSTRX.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LP01NO_SQL.CPY".
       COPY "LIBLP/LPWSNO.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBLP/LPWSREPAYTRP.CPY".
       COPY "LIBLP/LPWSREPAYNO.CPY".
       COPY "LIBEX/EX01XTRP.CPY".
       COPY "LIBEX/EX01XNO.CPY".
      *-----------------------------------------------------------------
       01  WK-IFN              PIC X(2)    VALUE "WK".

       01  MSEL-LIST           PIC X(5)     VALUE "MSEL.".
       01  WK-KEY              PIC 9(2)     VALUE 0.
       01  HOLD-TY1-KEY.
           03  HOLD-TY-BRNO    PIC 9(4).
           03  HOLD-TY-DATE    PIC 9(8).
           03  HOLD-TY-CLASS   PIC 9(3).

      *********************************************************
      *   DATA PATH FROM GETENV ON "FIL"
      *********************************************************
       01  DATA-PATH             PIC X(35).
       01  FIL-PATH              REDEFINES DATA-PATH.
      * USR/DATA/R1/
           03  FIL-BASE         PIC X(12).

       01  WS-BRACCTNO.
           03  WS-BRNO     PIC 9(4).
           03  WS-NUMBER   PIC 9(6).

       01  ORIG-LN-REPOCD            PIC X        VALUE " ".
           88  ORIG-LN-IS-REPO-AR     VALUE "X" "S".
           88  ORIG-LN-REPO-ONHAND    VALUE "X".
           88  ORIG-LN-REPO-REDEEMED  VALUE "V".
           88  ORIG-LN-REPO-SOLD      VALUE "S".
           88  ORIG-LN-IS-NOT-REPO    VALUE " " "V".
       01  REPO-AR-SUB               PIC S9(5) COMP.

       01  ORIG-RDSUB          PIC 99       COMP VALUE 0.
       01  ORIG-CSUB           PIC 99       COMP VALUE 0.
       01  ORIG-OUTBAL         PIC S9(7)V99 COMP VALUE 0.
       01  SAVE-OT2BAL         PIC S9(7)V99 COMP VALUE 0.

       01  HOLD-DS1-KEY.
           03  HOLD-DS-DATE-CCYYMM   PIC 9(6).
           03  HOLD-DS-BRANCH        PIC 9(4).
           03  HOLD-DS-DLNO          PIC 9(4)V99.
           03  HOLD-DS-CLASS         PIC 9(3).
       01  HOLD-FROM-LN-CLASS  PIC 9(3).
       01  HOLD-TO-LN-CLASS    PIC 9(3).
       01  HOLD-TP-OTHBR-FG    PIC X.
       01  HOLD-TP-POSTING-BR  PIC 9(4).
       01  HOLD-TP-BRNO        PIC 9(4).
      * DONT CONFUSE THIS WITH HOLD-LP1-KEY WHICH IS IN LPPFBUF
       01  SAVE-LP1-KEY        PIC X(17)    VALUE SPACES.

       01  HOLD-LP-TRCD        PIC XX       VALUE SPACES.
       01  HOLD-TP-WORKAMT-01.
           03  HOLD-TP-WORKAMT PIC S9(7)V99 COMP-3 OCCURS 6.
       01  RV-LP-TRCD          PIC XX       VALUE SPACES.
       01  SAVE-DT-SEQ         PIC 9(3).
       01  SAVE-LP-SEQNO       PIC 9(5).
       01  RV-LP-SEQNO         PIC 9(5)     VALUE 0.
       01  SAVE-TX-LINE        PIC 9(3).
       01  ANTIC-TAB.
           03  WK-LN-ANTICERN  PIC S9(7)V99 COMP OCCURS 6.
           03  WK-LN-ANTICADJ  PIC S9(7)V99 COMP OCCURS 6.
           03  WK-LN-UFERN     PIC S9(7)V99 COMP OCCURS 6.
           03  WK-LN-ACCERN    PIC S9(7)V99 COMP OCCURS 6.

       01  D-LNNO-LIST         PIC X(13)  VALUE "LNNO SORTKEY.".
       01  D-LNNO-BUF.
           03  D-LOAN-NUM      PIC X(16).
           03  D-LCDESC        PIC X(16).

       01  PART-RC-AMT         PIC S9(7)V99 COMP-3.
       01  ELE                 PIC 99       VALUE 0.
       01  TS-ONE              PIC S9.
       01  SUB                 PIC 9(6) COMP-3       VALUE 0.
       01  SUB2                PIC 9(6) COMP-3       VALUE 0.
       01  WK-SUB              PIC 9    COMP-3.
       01  REC-EXISTS          PIC X        VALUE SPACES.

       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBCL/CRNOR2W.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPCDELW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBLP/LPEARNW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPMENUW.CPY".
       COPY "LIBLP/LPSETTYW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBLP/LPDSCLW.CPY".
       COPY "LIBLP/POSTDATEW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBLP/LPSRCDW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LONPF_DEF.CPY".
       COPY "LIBLP/LONPF_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      /*************************************************
      *   C O N T R O L   M O D U L E                  *
      **************************************************
       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPPFBUF.CPY".
       COPY "LIBLP/LPLOANW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/LONPF_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME PROG-BUF UPDATE-BUF.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                     WK-FILE REPAY-TRP-FILE REPAY-NO-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-TY1-FILE.
           PERFORM CLOSE-TRX1-FILE.
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LXG1-FILE.
           PERFORM CLOSE-LXE1-FILE.
           PERFORM CLOSE-LX1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-DT1-FILE.
           PERFORM CLOSE-DS1-FILE.
           PERFORM CLOSE-BW1-FILE.
           CLOSE WK-FILE REPAY-TRP-FILE REPAY-NO-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LONPF" TO VDU-FORM-NAME.
      D    STOP MESS.

           PERFORM INITIALIZE-ROUTINE.
           IF ERRCD NOT = " "
              IF ERRCD = "Z"
                 GO TO UPDATE-EXIT
               ELSE
                 GO TO END-ROUTINE.

      **************************************************
      *  FILE UPDATE ROUTINES
      *  EXAMPLE OF "TS" CODE:
      *
      *     IF NORMAL POSTING:
      *         RV-LP-TRCD   = "  "
      *         HOLD-LP-TRCD = "TS"
      *         TS-ONE       = -1
      *         RV-LP-SEQNO  = 00000
      *     IF REVERSAL:
      *         RV-LP-TRCD   = "TS"
      *         HOLD-LP-TRCD = "TS"
      *         TS-ONE       = +1
      *         RV-LP-SEQNO  = LP-SEQNO OF THE "TS"
      **************************************************

           IF LP-TRCD = "RV"
              MOVE +1 TO TS-ONE
              PERFORM REVERSE-LP-RECORDS
              MOVE LP-TOCL TO HOLD-FROM-LN-CLASS
              MOVE LP-FROMCL TO HOLD-TO-LN-CLASS
            ELSE
              MOVE -1 TO TS-ONE
              MOVE LP-TRCD TO HOLD-LP-TRCD
              MOVE LP-FROMCL TO HOLD-FROM-LN-CLASS
              MOVE LP-TOCL TO HOLD-TO-LN-CLASS.

      **************************************************

      * CLEAR OUT PURCHASED FROM DS-FILE:
           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM        TO HOLD-DS-DATE-CCYYMM.
           MOVE LN-OWNBR           TO HOLD-DS-BRANCH.
           MOVE LN-DLNO            TO HOLD-DS-DLNO.
           MOVE HOLD-FROM-LN-CLASS TO HOLD-DS-CLASS.
           PERFORM UPDATE-DS1-FILE.

      * CLEAR OUT PURCHASED FROM TT-FILE:
           MOVE LN-OWNBR           TO HOLD-TY-BRNO.
           MOVE TRANS-DATE         TO HOLD-TY-DATE.
           MOVE HOLD-FROM-LN-CLASS TO HOLD-TY-CLASS.
           PERFORM UPDATE-TY1-FILE.

      * CLEAR OUT PURCHASED FROM TR1(TP)-FILE AND DT-FILE.
           PERFORM UPDATE-TRANS-FILE.

      * CHANGE LOAN CLASS TO SERVICE ACCOUNT:
           PERFORM UPDATE-LN1-FILE.

           PERFORM UPDATE-NOTICE-FILE.

      * GET UNEARNED STATISTICS AFTER LN UPDATE:
           IF GP-UNEARNED-STATS = "Y"
              PERFORM COMPUTE-EARNINGS.

      * SETUP NEW SERVICE IN DS-FILE:
           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM TO HOLD-DS-DATE-CCYYMM.

           MOVE LN-OWNBR TO HOLD-DS-BRANCH.
           MOVE LN-DLNO TO HOLD-DS-DLNO.
           MOVE HOLD-TO-LN-CLASS TO HOLD-DS-CLASS.
           PERFORM UPDATE-DS1-SERVICE.

      * SETUP NEW SERVICE IN TT-FILE:
           MOVE LN-OWNBR TO HOLD-TY-BRNO.
           MOVE TRANS-DATE TO HOLD-TY-DATE.
           MOVE HOLD-TO-LN-CLASS TO HOLD-TY-CLASS.
           PERFORM UPDATE-TY-SERVICE.

      * SETUP NEW SERVICE DAILY TRANS RECORD:
           PERFORM UPDATE-TR1-SERVICE.

      * SETUP NEW SERVICE DEALER TRANS RECORD:
           PERFORM UPDATE-DT1-SERVICE.

      * FTP/OUT/PAYS/NO.BRNO.NUMBER (DATA WAREHOUSE LAYOUT)
           IF GP-CREATE-PAY-XTRP-XNO = "Y"
              PERFORM UPDATE-REPAY-NO-FILE.

           MOVE "U P D A T E    C O M P L E T E " TO MESS.
           PERFORM SEND-LEGEND.
       UPDATE-EXIT.
           MOVE MENU-PROG-FORM(4) TO NEW-PROG-FORM.
           MOVE "P" TO ERRCD.

           PERFORM FULL-BRIEF-ROUTING.

           PERFORM SAVE-SCREEN.
           GO TO END-ROUTINE.

      ****************************************
       INITIALIZE-ROUTINE SECTION.
      ****************************************

           MOVE USER-PID TO TEMP-PID.

           MOVE SPACES TO ERRCD.

           PERFORM RESTORE-SCREEN.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           IF LN-REC NOT = WK-SAVE-REC
              MOVE " LOAN RECORD HAS BEEN CHANGED" TO MESS
              PERFORM SEND-MESS
              MOVE " PLEASE RE-INPUT YOUR CHANGES" TO MESS
              PERFORM SEND-MESS
              MOVE "Z" TO ERRCD
              GO TO INITIALIZE-ROUTINE-EXIT.
           IF LP-TRCD = "RV"
              PERFORM MOVE-IN-WORK-ANTIC.

      * SETUP FOR REPO PROCESSING:
           MOVE LN-REPOCD TO ORIG-LN-REPOCD.
           IF ORIG-LN-IS-REPO-AR
              IF ORIG-LN-REPO-ONHAND
                 MOVE 1 TO REPO-AR-SUB
              ELSE
                 MOVE 3 TO REPO-AR-SUB.

           IF IO-FG = 9
              GO TO IO-ERROR.

      * GET GLOBAL PARAMETER RECORD (FROM GPENV):
           PERFORM GET-GPENV.

      * TEMPORARY READ OF SPR, UNTIL LN-SPRCLASS LOGIC IS ADDED: JTG 022797
           MOVE HOLD-SP1-KEY TO SP1-KEY.
           IF LP-TRCD = "RV"
              MOVE LP-FROMCL TO HOLD-TO-LN-CLASS
            ELSE
              MOVE LP-TOCL TO HOLD-TO-LN-CLASS.
           MOVE HOLD-TO-LN-CLASS TO SP-SPRCLASS.
           PERFORM GET-SP.
           IF ERRCD NOT = " "
              GO TO INITIALIZE-ROUTINE-EXIT.

      * ESTABLISH CONTR, RECENCY & PAST DUE FOR UPDATE-TY1 & UPDATE-DS1:
           PERFORM ESTABLISH-PRIOR-CON-REC.
           IF ERRCD NOT = " "
              GO TO INITIALIZE-ROUTINE-EXIT.

      * GET UNEARNED STATISTICS PRIOR TO LN UPDATE:
           PERFORM COMPUTE-EARNINGS.

       INITIALIZE-ROUTINE-EXIT.
           EXIT.

      ************************************
       REVERSE-LP-RECORDS SECTION.
      ************************************

           MOVE "REVERSING PAYMENT FILE.................." TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-LP1-FILE.
           PERFORM READ-LP1-FILE.
           IF IO-FG NOT = 0 OR LP-REV = "Y"
              MOVE " PAYMENT REVERSAL ERROR" TO MESS
              PERFORM SEND-MESS
              GO TO IO-ERROR.
           IF RV-LP-TRCD = SPACES
              MOVE LP-TRCD TO RV-LP-TRCD HOLD-LP-TRCD
              MOVE LP-SEQNO TO RV-LP-SEQNO.
           MOVE "Y" TO LP-REV.
           PERFORM REWRITE-LP1-FILE.

           MOVE "N" TO REV-REBATE.
           MOVE WK-LP-REC TO LP-REC.
           PERFORM CLOSE-LP1-FILE.

      **************************************
       UPDATE-DS1-FILE SECTION.
      **************************************

      * DELETE PURCHASED ACCOUNT

           MOVE "UPDATING DEALER STATISTICS FILE........." TO MESS.
           PERFORM SEND-LEGEND.
           PERFORM UPDATE-DS1-READ-FILE.

           IF LN-DLRECOURSE = "F"
              MOVE 1 TO SUB
           ELSE
              IF LN-DLRECOURSE = "P"
                 MOVE 2 TO SUB
              ELSE
                 IF LN-DLRECOURSE = "N"
                    MOVE 3 TO SUB
                 ELSE
                    MOVE 0 TO SUB.

      ********************************************************
      * UPDATE RECOURSE FIGURES:
           IF SUB NOT = 0
              MOVE LP-INTDUE TO PART-RC-AMT
              PERFORM DS1-COMPUTE-PART-RC-AMT
              SUBTRACT PART-RC-AMT FROM DS-RCOURSE-AMT(SUB)
              ADD TS-ONE TO DS-RCOURSE-UNITS(SUB).

      ********************************************************
      * UPDATE WEIGHTED AVERAGES
      *          AND CURRENT BALANCES:

           IF LN-PLCD = " "
              COMPUTE DS-SUMWGTTM = DS-SUMWGTTM +
                            (LN-ORGTERM * TS-ONE)
              COMPUTE DS-SUMWGTAV = DS-SUMWGTAV +
                            (LN-LNAMT * TS-ONE)
              COMPUTE DS-SUMWGTTA = DS-SUMWGTTA +
                            (LN-ORGTERM * LN-LNAMT * TS-ONE)
              SUBTRACT LP-INTDUE FROM DS-OUTSAMT(1)
              ADD TS-ONE TO DS-OUTSUNIT(1)
            ELSE
              SUBTRACT LP-INTDUE FROM DS-RCPLAMT(1)
              ADD TS-ONE TO DS-RCPLUNIT(1)
              SUBTRACT LP-INTDUE FROM DS-PLOSAMT(1)
              ADD TS-ONE TO DS-PLOSUNIT(1).
           IF LN-JDDATE NOT = 0
              SUBTRACT LP-INTDUE FROM DS-JDOSAMT(1)
              ADD TS-ONE TO DS-JDOSUNIT(1).

      ********************************************************
      * UPDATE DEALER RESERVES:
      *    IF LP-TRCD NOT = "RV"
              PERFORM GET-TP-WORKAMT.
      *    ELSE
      *       COMPUTE TP-WORKAMT(1) = TP-WORKAMT(1) * (-1)
      *       COMPUTE TP-WORKAMT(2) = TP-WORKAMT(2) * (-1)
      *       COMPUTE TP-WORKAMT(3) = TP-WORKAMT(3) * (-1).

           COMPUTE DS-DLRVPRIN(1) = DS-DLRVPRIN(1) -
                                (LP-INTDUE -
                                (TP-WORKAMT(1) + TP-WORKAMT(2) +
                                 TP-WORKAMT(3) + TP-WORKAMT(4) +
                                 TP-WORKAMT(5) + TP-WORKAMT(6)))
           GO TO DS1-REWRITE.
      ********************************************************

       DS1-COMPUTE-PART-RC-AMT.
           IF SUB = 2
            IF PART-RC-AMT > 0
               IF LP-CURBAL NOT < LN-DLPARTRCAMT
                  MOVE 0 TO PART-RC-AMT
                ELSE
                  IF (LP-CURBAL + PART-RC-AMT) > LN-DLPARTRCAMT
                     SUBTRACT LP-CURBAL FROM LN-DLPARTRCAMT
                                     GIVING PART-RC-AMT.
           IF SUB = 2
            IF PART-RC-AMT < 0
               IF LP-CURBAL > LN-DLPARTRCAMT
                  IF (LP-CURBAL + PART-RC-AMT) NOT < LN-DLPARTRCAMT
                     MOVE 0 TO PART-RC-AMT
                   ELSE
                     COMPUTE PART-RC-AMT = LP-CURBAL -
                                   LN-DLPARTRCAMT + PART-RC-AMT.

       DS1-REWRITE.

      * DELINQUENCY UPDATE:
           PERFORM UPDATE-DS1-PRIOR-TFR.

      * REMOVE NET UNEARNED STATISTICS FROM PURCHASE CLASS STATS:
           IF GP-UNEARNED-STATS = "Y"
              PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
                  SUBTRACT EARN-UNEARN(SUB)
                             FROM DS-UNEARNED-AMT(DSNOW DSNET SUB)
              END-PERFORM.

           PERFORM REWRITE-DS1-FILE.
           PERFORM CLOSE-DS1-FILE.

      ***********************************
       UPDATE-TY1-FILE SECTION.
      ***********************************

      * DELETE PURCHASED ACCOUNT

           MOVE "UPDATING TODAY'S TOTAL FILE............." TO MESS.
           PERFORM SEND-LEGEND.
           PERFORM UPDATE-TY1-READ-FILE.

      * UPDATE-TY RECORD:
           PERFORM GET-SRCD-SUB.
           IF SRCD-NOT-PL
              COMPUTE TY-SUMWGTTM =
                  TY-SUMWGTTM + (LN-ORGTERM * TS-ONE)
              COMPUTE TY-SUMWGTAV =
                  TY-SUMWGTAV + (LN-LNAMT * TS-ONE)
              COMPUTE TY-SUMWGTTA =
                  TY-SUMWGTTA + (LN-ORGTERM * LN-LNAMT * TS-ONE)
              COMPUTE TY-PRINCOL =
                  TY-PRINCOL + LP-INTDUE + (LN-OT2BAL * TS-ONE)
              COMPUTE TY-OT2COL = TY-OT2COL - (LN-OT2BAL * TS-ONE)
              IF ORIG-LN-IS-REPO-AR
                 SUBTRACT LP-INTDUE FROM TY-AMT-REPO(1 REPO-AR-SUB)
                 ADD TS-ONE TO TY-NO-REPO(1 REPO-AR-SUB)
                 COMPUTE TY-O2-REPO(1 REPO-AR-SUB) =
                     TY-O2-REPO(1 REPO-AR-SUB) + (LN-OT2BAL * TS-ONE)
              ELSE
                 SUBTRACT LP-INTDUE FROM TY-AMT(1 SRCD)
                 ADD TS-ONE TO TY-NO(1 SRCD)
                 COMPUTE TY-O2(1 SRCD) =
                                 TY-O2(1 SRCD) + (LN-OT2BAL * TS-ONE).

      *
      *-------------------------------------------------------
      * UPDATE DEALER RESERVES:
      *    IF LP-TRCD NOT = "RV"
              PERFORM GET-TP-WORKAMT.
      *    ELSE
      *       COMPUTE TP-WORKAMT(1) = TP-WORKAMT(1) * (-1)
      *       COMPUTE TP-WORKAMT(2) = TP-WORKAMT(2) * (-1)
      *       COMPUTE TP-WORKAMT(3) = TP-WORKAMT(3) * (-1).

           COMPUTE TY-DLRVPRIN(1 1) = TY-DLRVPRIN(1 1) -
                                (LP-INTDUE -
                                (TP-WORKAMT(1) + TP-WORKAMT(2) +
                                 TP-WORKAMT(3) + TP-WORKAMT(4) +
                                 TP-WORKAMT(5) + TP-WORKAMT(6))).
      *
      * DELINQUENCY UPDATE:
           PERFORM UPDATE-TY1-PRIOR-TFR.

      * REMOVE NET UNEARNED STATISTICS FROM PURCHASE CLASS STATS:
           IF GP-UNEARNED-STATS = "Y"
              PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
                  SUBTRACT EARN-UNEARN(SUB)
                             FROM TY-UNEARNED-AMT(TYNET SUB)
              END-PERFORM.

           PERFORM REWRITE-TY1-FILE.
           PERFORM CLOSE-TY1-FILE.

      ******************************************************
       ESTABLISH-PRIOR-CON-REC SECTION.
      ******************************************************

           MOVE TRANS-DATE TO AGEING-DATE.
           PERFORM COMPUTE-CONTRACTUAL.
           PERFORM COMPUTE-RECENCY.

           MOVE LN-1STPYDATE TO CEPP1-DATE.
           MOVE TRANS-DATE TO CEPP2-DATE.
           PERFORM CONTRACTUAL-DELINQUENCY.

           MOVE CSUB TO ORIG-CSUB.
           MOVE RDSUB TO ORIG-RDSUB.
           MOVE LN-CURBAL TO ORIG-OUTBAL.

       ESTABLISH-PRIOR-CON-REC-EXIT.
           EXIT.

      *******************************************
       UPDATE-TY1-PRIOR-TFR SECTION.
      *******************************************

      * SETUP OT2BAL WORKER FOR UPDATE OF STATS
           IF GP-DELINQ-WITH-O2 = "Y"
              MOVE 0 TO SAVE-OT2BAL
           ELSE
              MOVE LN-OT2BAL TO SAVE-OT2BAL.

           IF LN-PLCD = " " AND ORIG-LN-IS-NOT-REPO
      * NORMAL DELINQUENCY:
              ADD TS-ONE TO TY-UNITS(1 ORIG-RDSUB)
                            TY-UNITS(2 ORIG-CSUB)
              COMPUTE TY-OUTBAL(1 ORIG-RDSUB) =
                   TY-OUTBAL(1 ORIG-RDSUB)
                        - LP-INTDUE - (SAVE-OT2BAL * TS-ONE)
              COMPUTE TY-OUTBAL(2 ORIG-CSUB) =
                   TY-OUTBAL(2 ORIG-CSUB)
                        - LP-INTDUE - (SAVE-OT2BAL * TS-ONE)
      * BANKRUPT DELINQUENCY:
              IF LN-BNKRPTDATE NOT = 0
                 ADD TS-ONE TO TY-UNITS-BNKRPT(1 ORIG-RDSUB)
                               TY-UNITS-BNKRPT(2 ORIG-CSUB)
                 COMPUTE TY-OUTBAL-BNKRPT(1 ORIG-RDSUB) =
                      TY-OUTBAL-BNKRPT(1 ORIG-RDSUB)
                           - LP-INTDUE - (SAVE-OT2BAL * TS-ONE)
                 COMPUTE TY-OUTBAL-BNKRPT(2 ORIG-CSUB) =
                      TY-OUTBAL-BNKRPT(2 ORIG-CSUB)
                           - LP-INTDUE - (SAVE-OT2BAL * TS-ONE).

      *******************************************
       UPDATE-DS1-PRIOR-TFR SECTION.
      *******************************************

      * SETUP OT2BAL WORKER FOR UPDATE OF STATS
           IF GP-DELINQ-WITH-O2 = "Y"
              MOVE 0 TO SAVE-OT2BAL
           ELSE
              MOVE LN-OT2BAL TO SAVE-OT2BAL.

           IF LN-PLCD = " " AND ORIG-LN-IS-NOT-REPO
      * NORMAL DELINQUENCY
              ADD TS-ONE TO DS-UNITS(DSNOW 1 ORIG-RDSUB)
                            DS-UNITS(DSNOW 2 ORIG-CSUB)
              COMPUTE DS-OUTBAL(DSNOW 1 ORIG-RDSUB) =
                   DS-OUTBAL(DSNOW 1 ORIG-RDSUB)
                        - LP-INTDUE - (SAVE-OT2BAL * TS-ONE)
              COMPUTE DS-OUTBAL(DSNOW 2 ORIG-CSUB) =
                   DS-OUTBAL(DSNOW 2 ORIG-CSUB)
                        - LP-INTDUE - (SAVE-OT2BAL * TS-ONE)
      * BANKRUPT DELINQUENCY
              IF LN-BNKRPTDATE NOT = 0
                 ADD TS-ONE TO DS-UNITS-BNKRPT(DSNOW 1 ORIG-RDSUB)
                               DS-UNITS-BNKRPT(DSNOW 2 ORIG-CSUB)
                 COMPUTE DS-OUTBAL-BNKRPT(DSNOW 1 ORIG-RDSUB) =
                      DS-OUTBAL-BNKRPT(DSNOW 1 ORIG-RDSUB)
                           - LP-INTDUE - (SAVE-OT2BAL * TS-ONE)
                 COMPUTE DS-OUTBAL-BNKRPT(DSNOW 2 ORIG-CSUB) =
                      DS-OUTBAL-BNKRPT(DSNOW 2 ORIG-CSUB)
                           - LP-INTDUE - (SAVE-OT2BAL * TS-ONE).

      ******************************************************
       ESTABLISH-AFTER-CON-REC SECTION.
      ******************************************************
           MOVE TRANS-DATE TO AGEING-DATE.
           PERFORM COMPUTE-CONTRACTUAL.
           PERFORM COMPUTE-RECENCY.

           MOVE LN-1STPYDATE TO CEPP1-DATE.
           MOVE TRANS-DATE TO CEPP2-DATE.
           PERFORM CONTRACTUAL-DELINQUENCY.

           MOVE CSUB TO CSUB.
           MOVE RDSUB TO RDSUB.

      *******************************************
       UPDATE-TY1-AFTER-TFR SECTION.
      *******************************************
           MOVE "UPDATING TODAY'S DELINQ TOTAL FILE......." TO MESS.
           PERFORM SEND-LEGEND.

      * SETUP OT2BAL WORKER FOR UPDATE OF STATS
           IF GP-DELINQ-WITH-O2 = "Y"
              MOVE 0 TO SAVE-OT2BAL
           ELSE
              MOVE LN-OT2BAL TO SAVE-OT2BAL.

           IF LN-PLCD = " " AND ORIG-LN-IS-NOT-REPO
              PERFORM ESTABLISH-AFTER-CON-REC
      * NORMAL DELINQUENCY:
              SUBTRACT TS-ONE FROM TY-UNITS(1 RDSUB)
              COMPUTE TY-OUTBAL(1 RDSUB) =
                   TY-OUTBAL(1 RDSUB)
                        + LP-INTDUE + (SAVE-OT2BAL * TS-ONE)

              SUBTRACT TS-ONE FROM TY-UNITS(2 CSUB)
              COMPUTE TY-OUTBAL(2 CSUB) =
                   TY-OUTBAL(2 CSUB)
                        + LP-INTDUE + (SAVE-OT2BAL * TS-ONE)
      * BANKRUPT DELINQUENCY:
              IF LN-BNKRPTDATE NOT = 0
                 SUBTRACT TS-ONE FROM TY-UNITS-BNKRPT(1 RDSUB)
                 COMPUTE TY-OUTBAL-BNKRPT(1 RDSUB) =
                      TY-OUTBAL-BNKRPT(1 RDSUB)
                           + LP-INTDUE + (SAVE-OT2BAL * TS-ONE)

                 SUBTRACT TS-ONE FROM TY-UNITS-BNKRPT(2 CSUB)
                 COMPUTE TY-OUTBAL-BNKRPT(2 CSUB) =
                      TY-OUTBAL-BNKRPT(2 CSUB)
                           + LP-INTDUE + (SAVE-OT2BAL * TS-ONE).

       UPDATE-TY1-AFTER-TFR-EXIT.
           EXIT.

      *******************************************
       UPDATE-DS1-AFTER-TFR SECTION.
      *******************************************

           MOVE "UPDATING DEALER STATISTICS DELINQUENCY...." TO MESS.
           PERFORM SEND-LEGEND.

      * SETUP OT2BAL WORKER FOR UPDATE OF STATS
           IF GP-DELINQ-WITH-O2 = "Y"
              MOVE 0 TO SAVE-OT2BAL
           ELSE
              MOVE LN-OT2BAL TO SAVE-OT2BAL.

           IF LN-PLCD = " " AND ORIG-LN-IS-NOT-REPO
              PERFORM ESTABLISH-AFTER-CON-REC
      * NORMAL DELINQUENCY:
              SUBTRACT TS-ONE FROM DS-UNITS(DSNOW 1 RDSUB)
              COMPUTE DS-OUTBAL(DSNOW 1 RDSUB) =
                   DS-OUTBAL(DSNOW 1 RDSUB)
                        + LP-INTDUE + (SAVE-OT2BAL * TS-ONE)

              SUBTRACT TS-ONE FROM DS-UNITS(DSNOW 2 CSUB)
              COMPUTE DS-OUTBAL(DSNOW 2 CSUB) =
                   DS-OUTBAL(DSNOW 2 CSUB)
                        + LP-INTDUE + (SAVE-OT2BAL * TS-ONE)
      * BANKRUPT DELINQUENCY:
              IF LN-BNKRPTDATE NOT = 0
                 SUBTRACT TS-ONE FROM DS-UNITS-BNKRPT(DSNOW 1 RDSUB)
                 COMPUTE DS-OUTBAL-BNKRPT(DSNOW 1 RDSUB) =
                      DS-OUTBAL-BNKRPT(DSNOW 1 RDSUB)
                           + LP-INTDUE + (SAVE-OT2BAL * TS-ONE)

                 SUBTRACT TS-ONE FROM DS-UNITS-BNKRPT(DSNOW 2 CSUB)
                 COMPUTE DS-OUTBAL-BNKRPT(DSNOW 2 CSUB) =
                      DS-OUTBAL-BNKRPT(DSNOW 2 CSUB)
                           + LP-INTDUE + (SAVE-OT2BAL * TS-ONE).

       UPDATE-DS1-AFTER-TFR-EXIT.
           EXIT.

      **************************************
       UPDATE-TRANS-FILE SECTION.
      **************************************

      *  DELETE PURCHASED ACCOUNT

           MOVE 0 TO TP-LINE.

           MOVE "UPDATING DAILY TRANSACTION FILE........." TO MESS.
           PERFORM SEND-LEGEND.

           MOVE BRANCH TO HOLD-TP-POSTING-BR.
           MOVE LN-OWNBR TO HOLD-TP-BRNO.
           IF HOLD-TP-POSTING-BR = HOLD-TP-BRNO
              MOVE SPACES TO HOLD-TP-OTHBR-FG
              PERFORM UPDATE-TR1-FILE
            ELSE
              MOVE "G" TO HOLD-TP-OTHBR-FG
              PERFORM UPDATE-TR1-FILE
              MOVE LN-OWNBR TO HOLD-TP-POSTING-BR
              MOVE BRANCH TO HOLD-TP-BRNO
              MOVE "C" TO HOLD-TP-OTHBR-FG
              PERFORM UPDATE-TR1-FILE.
      *----------------------------
           MOVE "UPDATING LOAN PAYMENT FILE.............." TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-LP1-FILE.

           MOVE LP-PATH-OWNBR TO LP-BRNO.
           MOVE LN-ACCTNO     TO LP-ACCTNO.
           MOVE 0             TO LP-SEQNO.

           MOVE LP-REC TO WK-LP-REC.
           MOVE 99999  TO LP-SEQNO.

           MOVE LP-BRNO   TO QLP1-WBEG-BRNO
                             QLP1-WEND-BRNO.
           MOVE LP-ACCTNO TO QLP1-WBEG-ACCTNO
                             QLP1-WEND-ACCTNO.
           MOVE LP-SEQNO  TO QLP1-WEND-SEQNO.
           MOVE ALL "0"   TO QLP1-WBEG-SEQNO.

           PERFORM START-LP1-FILE-NOT-GREATER.
           PERFORM READ-LP1-FILE-PREVIOUS.
           PERFORM CLOSE-LP1-FILE.
           IF IO-FG NOT = 0 OR (LP-PATH-OWNBR NOT = LP-BRNO)
                             OR (LN-ACCTNO NOT = LP-ACCTNO)
              MOVE LN-OWNBR  TO LP-BRNO
              MOVE LN-ACCTNO TO LP-ACCTNO
              MOVE 0         TO LP-SEQNO.

           PERFORM OPEN-LP1-FILE.
       NEXT-AVAIL-SEQ.
           ADD 1 TO LP-SEQNO.
           MOVE LP1-KEY TO SAVE-LP1-KEY.
           PERFORM READ-LP1-FILE.
           IF IO-FG = 0
      *       UNLOCK LP-FILE
              GO TO NEXT-AVAIL-SEQ.

           MOVE WK-LP-REC    TO LP-REC.
           MOVE SAVE-LP1-KEY TO LP1-KEY.

           PERFORM SET-POSTDATE.
           MOVE POST-USERID  TO LP-USERID.
           MOVE POST-TIME    TO LP-POSTTIME.
           MOVE POST-DATE    TO LP-POSTDATE.
           MOVE LN-COLLECTOR TO LP-COLLECTOR.

           MOVE RV-LP-TRCD  TO LP-REVTRCD.
           MOVE RV-LP-SEQNO TO LP-REVSEQNO.

           PERFORM WRITE-LP1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.
           PERFORM CLOSE-LP1-FILE.

           IF LXG-GLAMT(1) NOT = 0 OR
              LXG-GLAMT(2) NOT = 0 OR
              LXG-GLAMT(3) NOT = 0
                 PERFORM OPEN-LXG1-FILE
                 MOVE LP-BRNO   TO LXG-BRNO
                 MOVE LP-ACCTNO TO LXG-ACCTNO
                 MOVE LP-SEQNO  TO LXG-SEQNO
                 PERFORM WRITE-LXG1-FILE
                 PERFORM CLOSE-LXG1-FILE.

           IF NOT (LXE-EARN(1) = 0 AND
                    LXE-EARN(2) = 0 AND
                     LXE-EARN(3) = 0 AND
                     LXE-EARN(4) = 0 AND
                     LXE-EARN(5) = 0 AND
                     LXE-EARN(6) = 0 AND
                     LXE-EARN(7) = 0 AND
                     LXE-WORKER = 0 AND
                     LXE-WORKER2 = 0 )
              PERFORM OPEN-LXE1-FILE
              MOVE LP-BRNO   TO LXE-BRNO
              MOVE LP-ACCTNO TO LXE-ACCTNO
              MOVE LP-SEQNO  TO LXE-SEQNO
              PERFORM WRITE-LXE1-FILE
              PERFORM CLOSE-LXE1-FILE.

      * CREATE TRANSFER LOAN PAYMENT TRAILERS T & P

           PERFORM OPEN-LX1-FILE.

           MOVE LP-BRNO   TO LX-BRNO.
           MOVE LP-ACCTNO TO LX-ACCTNO.
           MOVE LP-SEQNO  TO LX-SEQNO.
           PERFORM READ-LX1-FILE.
           IF IO-FG = 0
              PERFORM MOVE-LXT-FIELDS
              PERFORM MOVE-LXP-FIELDS
              PERFORM REWRITE-LX1-FILE
           ELSE
              PERFORM CLEAR-LX-FILE
              MOVE LP-BRNO   TO LX-BRNO
              MOVE LP-ACCTNO TO LX-ACCTNO
              MOVE LP-SEQNO  TO LX-SEQNO
              PERFORM MOVE-LXT-FIELDS
              PERFORM MOVE-LXP-FIELDS
              PERFORM WRITE-LX1-FILE.

           PERFORM CLOSE-LX1-FILE.


           MOVE LP-SEQNO TO SAVE-LP-SEQNO.
           PERFORM UPDATE-DT1-FILE.

      *************************************
       UPDATE-TR1-FILE SECTION.
      *************************************

      * DELETE PURCHASED ACCOUNT

           PERFORM OPEN-TRP1-FILE.

           MOVE HOLD-TP-BRNO       TO TP-BRNO.
           MOVE TRANS-DATE         TO TP-DATE.
           MOVE HOLD-FROM-LN-CLASS TO TP-CLASS.
           MOVE LN-ACCTNO          TO TP-ACCTNO.
           MOVE 0                  TO TP-LINE.
       TR-CHECK-EXISTS.
           ADD 1                   TO TP-LINE.
           MOVE TRP1-KEY           TO LPWSTRP-TRP1-KEY.
           PERFORM READ-TRP1-FILE.
           IF IO-FG = 0
      *       UNLOCK TRP-FILE
              GO TO TR-CHECK-EXISTS.

           PERFORM CLEAR-TRP-FILE.
           MOVE LPWSTRP-TRP1-KEY TO TRP1-KEY.

       TR-UPDATE-REC.
           MOVE HOLD-TP-WORKAMT(1) TO TP-WORKAMT(1).
           MOVE HOLD-TP-WORKAMT(2) TO TP-WORKAMT(2).
           MOVE HOLD-TP-WORKAMT(3) TO TP-WORKAMT(3).
           MOVE HOLD-TP-WORKAMT(4) TO TP-WORKAMT(4).
           MOVE HOLD-TP-WORKAMT(5) TO TP-WORKAMT(5).
           MOVE HOLD-TP-WORKAMT(6) TO TP-WORKAMT(6).
           MOVE HOLD-TP-POSTING-BR TO TP-POSTING-BR.
           MOVE HOLD-TP-OTHBR-FG   TO TP-OTHBR-FG.
           MOVE LN-LAWCODE         TO TP-LAWCODE.
           IF LN-JDDATE NOT = 0
              MOVE "J"             TO TP-JDCD.
           MOVE LN-PLCD            TO TP-PLCD.
           MOVE LN-PLTYPE          TO TP-PLTYPE.
           MOVE LP-SSNO            TO TP-SSNO.
           MOVE MAKER-CD           TO TP-MAKER.
           MOVE LP-IBPC            TO TP-IBPC.
           MOVE LP-TRCD            TO TP-TRCD.
           IF RV-LP-TRCD NOT = " "
              MOVE "Y"             TO TP-REV
              MOVE RV-LP-TRCD      TO TP-TRCD.
           MOVE LP-REFNO           TO TP-REFNO.
           MOVE LP-FROMCL          TO TP-FROMCL.
           MOVE LP-TOCL            TO TP-TOCL.
           MOVE LP-PAYDATE         TO TP-PAYDATE.
           MOVE LP-PDTH-DATE       TO TP-DUEDATE.

           ADD LP-INTDUE TO TP-TRAMT.

      *    IF LP-TRCD NOT = "RV"
              PERFORM GET-TP-WORKAMT.
      *    ELSE
      *       COMPUTE TP-WORKAMT(1) = TP-WORKAMT(1) * (-1)
      *       COMPUTE TP-WORKAMT(2) = TP-WORKAMT(2) * (-1)
      *       COMPUTE TP-WORKAMT(3) = TP-WORKAMT(3) * (-1).


           MOVE LXG-GLNO(1) TO TP-GLNO(1).
           MOVE LXG-GLNO(2) TO TP-GLNO(2).
           MOVE LXG-GLNO(3) TO TP-GLNO(3).

           ADD LXG-GLAMT(1) TO TP-GLAMT(1).
           ADD LXG-GLAMT(2) TO TP-GLAMT(2).
           ADD LXG-GLAMT(3) TO TP-GLAMT(3).

           PERFORM SET-TP-EARNINGS.

           ADD LP-OT2BAL TO TP-OT2BAL.
           ADD LP-OTHBAL TO TP-OTHBAL.
           ADD LP-INTBAL TO TP-INTBAL.
           ADD LP-LCBAL TO TP-LCBAL.
           ADD LP-CURBAL TO TP-CURBAL.

           MOVE RV-LP-TRCD TO TP-REVTRCD.
           MOVE RV-LP-SEQNO TO TP-REVSEQNO.

           MOVE LP-PDTH-DATE TO TP-PDTH-DATE.
           MOVE LP-TILLNO TO TP-TILLNO.
           MOVE LP-LCPDTH-DATE TO TP-LCPDTH-DATE.
           MOVE LN-DLNO TO TP-DLNO.
           MOVE LP-DLPROC TO TP-DLPROC.
           MOVE LP-INTPAID TO TP-INTPAID.
           MOVE LP-NEWSTAT-RATE TO TP-NEWSTAT-RATE.

           MOVE "N" TO TP-SERVACCT.
           MOVE LN-SALEFIN TO TP-SALEFIN.
           MOVE LN-LOANTYPE TO TP-LOANTYPE.

           PERFORM SET-POSTDATE.
           MOVE POST-USERID TO TP-USERID.
           MOVE POST-TIME TO TP-POSTTIME.
           MOVE POST-DATE TO TP-POSTDATE.
           MOVE LN-COLLECTOR TO TP-COLLECTOR.

           MOVE LN-UNITPER-CD   TO TP-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO TP-UNITPER-FREQ.
           MOVE LN-ACTIONCD2    TO TP-ACTIONCD2.

           PERFORM WRITE-TRP1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.
           PERFORM CLOSE-TRP1-FILE.

      * ONLY CREATE XTRP FOR OWNING BRANCH

      * INTERBRANCH OWNING BRANCH (OTHBR-FG "G")
      * INTERBRANCH POSTING BRANCH (OTHBR-FG "C")
      * OWNING BRANCH POSTING FOR ITSELF (OTHBR-FG = " ")

      * FTP/OUT/PAYS/TRP.BRNO.NUMBER.SEQ (DATA WAREHOUSE LAYOUT)
           IF GP-CREATE-PAY-XTRP-XNO = "Y" AND
                                      HOLD-TP-OTHBR-FG NOT = "C"
              PERFORM UPDATE-REPAY-TRP-FILE.

      *****************************************
       UPDATE-DT1-FILE SECTION.
      *****************************************

      * DELETE PURCHASED ACCOUNT

           MOVE "UPDATING DEALER TRANSACTION FILE........" TO MESS.
           PERFORM SEND-LEGEND.
           PERFORM OPEN-DT1-FILE.
           MOVE 1 TO SAVE-DT-SEQ.
       NEXT-DT-SEQ.
           PERFORM CLEAR-DT-FILE.
           MOVE LN-OWNBR TO DT-BRNO.
           MOVE LN-DLNO TO DT-DLNO.
           MOVE HOLD-FROM-LN-CLASS TO DT-CLASS.
           MOVE LP-ACCTNO TO DT-ACCTNO.
           MOVE TRANS-DATE TO DT-DATE.
           MOVE SAVE-DT-SEQ TO DT-SEQ.
           MOVE "P" TO DT-TYPE.
           MOVE LP-SEQNO TO DT-LPSEQNO.
           MOVE LP-TRCD TO DT-TRCD.
           COMPUTE DT-DLRVPRIN(1) =
                0 - (TP-TRAMT - (TP-WORKAMT(1) + TP-WORKAMT(2)
                                 + TP-WORKAMT(3) + TP-WORKAMT(4)
                                  + TP-WORKAMT(5) + TP-WORKAMT(6))
                    ).
           MOVE LP-DLPROC TO DT-PROCEEDS.
           MOVE "B" TO DT-PAID.

           PERFORM WRITE-DT1-FILE.
           IF IO-FG = 0
              GO TO EXIT-UPDATE-DT1.
           IF SAVE-DT-SEQ < 999
              ADD 1 TO SAVE-DT-SEQ
              GO TO NEXT-DT-SEQ.

           MOVE "DEALER TRANSACTION = 9999, REC NOT CREATED !" TO MESS.
           PERFORM SEND-MESS.

       EXIT-UPDATE-DT1.
           PERFORM CLOSE-DT1-FILE.

      **************************************
       UPDATE-LN1-FILE SECTION.
      **************************************

      * SETUP SERVICE ACCOUNT

           MOVE "UPDATING LOAN MASTER FILE..............." TO MESS.
           PERFORM SEND-LEGEND.

      * READ NEW LOAN CLASS:
           IF RV-LP-TRCD NOT = " "
              MOVE HOLD-FROM-LN-CLASS TO LC-CODE
            ELSE
              MOVE HOLD-TO-LN-CLASS TO LC-CODE.

           PERFORM OPEN-LC1-FILE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
           IF IO-FG NOT = 0
              MOVE " NEW LOAN CLASS RECORD DELETED" TO MESS
              PERFORM SEND-MESS
              GO TO IO-ERROR.

      * READ LOAN RECORD:
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

      *    ADD LP-EARNED TO LN-ACCERN(1) LN-TOTERN.
           IF LP-TRCD = "RV"
              PERFORM MOVE-IN-WORK-ANTIC
           ELSE
              MOVE HOLD-LN-ACCERN(1) TO LN-ACCERN(1)
              MOVE HOLD-LN-ACCERN(2) TO LN-ACCERN(2)
              MOVE HOLD-LN-ACCERN(3) TO LN-ACCERN(3)
              MOVE HOLD-LN-ACCERN(4) TO LN-ACCERN(4)
              MOVE HOLD-LN-ACCERN(5) TO LN-ACCERN(5)
              MOVE HOLD-LN-ACCERN(6) TO LN-ACCERN(6).
           COMPUTE LN-TOTERN = LN-TOTERN    + LP-EARNED(1) +
                               LP-EARNED(2) + LP-EARNED(3) +
                               LXE-EARN(1)  + LXE-EARN(2) +
                               LXE-EARN(3).
      *                        WK-LXE-EARN(1) + WK-LXE-EARN(2) +
      *                        WK-LXE-EARN(3).

           MOVE LC-SMALLOAN TO LN-SMALLOAN.
           MOVE LC-SALEFIN  TO LN-SALEFIN.
           MOVE LC-SERVACCT TO LN-SERVACCT.
           MOVE LC-LOANTYPE TO LN-LOANTYPE.
           MOVE LC-GIFTNET  TO LN-GIFTNET.
           MOVE LC-CODE     TO LN-CLASS
                               LN-SPRCLASS.

           IF LP-TRCD NOT = "RV"
              MOVE 0 TO LN-ANTICERN(1) LN-ANTICERN(2) LN-ANTICERN(3)
                        LN-ANTICERN(4) LN-ANTICERN(5) LN-ANTICERN(6)
                        LN-ANTICADJ(1) LN-ANTICADJ(2) LN-ANTICADJ(3)
                        LN-ANTICADJ(4) LN-ANTICADJ(5) LN-ANTICADJ(6)
                        LN-ACCERN(1)   LN-ACCERN(2)   LN-ACCERN(3)
                        LN-ACCERN(4)   LN-ACCERN(5)   LN-ACCERN(6)
                        LN-UFERN(1)    LN-UFERN(2)    LN-UFERN(3)
                        LN-UFERN(4)    LN-UFERN(5)    LN-UFERN(6).

           PERFORM REWRITE-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.

      ***********************************************
       UPDATE-NOTICE-FILE SECTION.
      ***********************************************
           PERFORM OPEN-NO1-FILE.
           MOVE LN-OWNBR  TO NO-BRNO.
           MOVE LN-ACCTNO TO NO-ACCTNO CRNOR2-ACCTNO.
           PERFORM READ-NO1-FILE.

           MOVE TRANS-DATE     TO CRNOR2-SYSDATE.
           MOVE "CL/CRNOR2"    TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH CRNOR2-LINK NO-REC
                                 POFF-WORKERS LN-REC.
           CANCEL FORM-PROGX.

           MOVE LN-OWNBR  TO NO-BRNO.
           MOVE LN-ACCTNO TO NO-ACCTNO.
           IF IO-FG = 0
              PERFORM REWRITE-NO1-FILE
           ELSE
              PERFORM WRITE-NO1-FILE.

           PERFORM CLOSE-NO1-FILE.

       EXIT-UPDATE-NOTICE-FILE.
           EXIT.

      ***********************************
       UPDATE-TY-SERVICE SECTION.
      ***********************************

      * SETUP NEW SERVICE ACCOUNT

           MOVE "UPDATING TODAY'S SERVICE TOTALS........" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM UPDATE-TY1-READ-FILE.

           COMPUTE TY-SUMWGTTM = TY-SUMWGTTM -
                                     (TS-ONE * LN-ORGTERM).
           COMPUTE TY-SUMWGTAV = TY-SUMWGTAV -
                                     (TS-ONE * LN-LNAMT).
           COMPUTE TY-SUMWGTTA =
              TY-SUMWGTTA - (LN-ORGTERM * TS-ONE * LN-LNAMT).

           PERFORM GET-SRCD-SUB.
           IF SRCD-NOT-PL
              IF ORIG-LN-IS-REPO-AR
                 SUBTRACT TS-ONE FROM TY-NO-REPO(1 REPO-AR-SUB)
                                      TY-NO-REPO(2 REPO-AR-SUB)
                 ADD LP-INTDUE TO TY-AMT-REPO(1 REPO-AR-SUB)
                                  TY-AMT-REPO(2 REPO-AR-SUB)
                 COMPUTE TY-O2-REPO(1 REPO-AR-SUB) =
                    TY-O2-REPO(1 REPO-AR-SUB) - (TS-ONE * LN-OT2BAL)
                 COMPUTE TY-O2-REPO(2 REPO-AR-SUB) =
                    TY-O2-REPO(2 REPO-AR-SUB) - (TS-ONE * LN-OT2BAL)
              ELSE
                 SUBTRACT TS-ONE FROM TY-NO(1 SRCD)
                                      TY-NO(2 SRCD)
                 ADD LP-INTDUE TO TY-AMT(1 SRCD)
                                  TY-AMT(2 SRCD)
                 COMPUTE TY-O2(1 SRCD) =
                    TY-O2(1 SRCD) - (TS-ONE * LN-OT2BAL)
                 COMPUTE TY-O2(2 SRCD) =
                    TY-O2(2 SRCD) - (TS-ONE * LN-OT2BAL).

      * DELINQUENCY UPDATE:
           PERFORM UPDATE-TY1-AFTER-TFR.

      * ADD NET UNEARNED STATISTICS TO SERVICE CLASS STATS:
           IF GP-UNEARNED-STATS = "Y"
              PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
                  ADD EARN-UNEARN(SUB)
                             TO TY-UNEARNED-AMT(TYNET SUB)
              END-PERFORM.

           PERFORM REWRITE-TY1-FILE.
           PERFORM CLOSE-TY1-FILE.

      ***********************************
       UPDATE-DS1-SERVICE SECTION.
      ***********************************

      * SETUP NEW SERVICE ACCOUNT

           MOVE "UPDATING DEALER SERVICE STATISTICS........." TO MESS.
           PERFORM SEND-LEGEND.
           PERFORM UPDATE-DS1-READ-FILE.

           COMPUTE DS-SUMWGTTM = DS-SUMWGTTM -
                                     (TS-ONE * LN-ORGTERM).
           COMPUTE DS-SUMWGTAV = DS-SUMWGTAV -
                                     (TS-ONE * LN-LNAMT).
           COMPUTE DS-SUMWGTTA =
              DS-SUMWGTTA - (LN-ORGTERM * TS-ONE * LN-LNAMT).
           SUBTRACT TS-ONE FROM DS-OUTSUNIT(1).
           ADD LP-INTDUE TO DS-OUTSAMT(1).

           IF LC-SALEFIN = "Y" AND GP-THE-ONLY-SF-SRCD NOT = " "
              IF LN-SRCD = GP-THE-ONLY-SF-SRCD
                 SUBTRACT TS-ONE FROM DS-NPURUNIT(1) DS-NPURUNIT(2)
                 ADD LP-INTDUE TO DS-NPURAMT(1) DS-NPURAMT(2)
              END-IF
           ELSE
           IF LN-SRCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO"
              SUBTRACT TS-ONE FROM DS-RNEWUNIT(1) DS-RNEWUNIT(2)
              ADD LP-INTDUE TO DS-RNEWAMT(1) DS-RNEWAMT(2)
           ELSE
           IF LN-SRCD = "NB"
              SUBTRACT TS-ONE FROM DS-NPURUNIT(1) DS-NPURUNIT(2)
              ADD LP-INTDUE TO DS-NPURAMT(1) DS-NPURAMT(2)
           ELSE
              SUBTRACT TS-ONE FROM DS-OPURUNIT(1) DS-OPURUNIT(2)
              ADD LP-INTDUE TO DS-OPURAMT(1) DS-OPURAMT(2).

      * DELINQUENCY UPDATE:
           PERFORM UPDATE-DS1-AFTER-TFR.

      * ADD NET UNEARNED STATISTICS TO SERVICE CLASS STATS:
           IF GP-UNEARNED-STATS = "Y"
              PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
                  ADD EARN-UNEARN(SUB)
                             TO DS-UNEARNED-AMT(DSNOW DSNET SUB)
              END-PERFORM.

           PERFORM REWRITE-DS1-FILE.
       EXIT-UPDATE-DS1-SERVICE.
           PERFORM CLOSE-DS1-FILE.

      ***********************************
       UPDATE-DT1-SERVICE SECTION.
      ***********************************

      * SETUP NEW SERVICE ACCOUNT

           MOVE "UPDATING DEALER TRANS SERVICE FILE........" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-DT1-FILE.
           MOVE 1 TO SAVE-DT-SEQ.
       UPDATE-DT1-SERVICE-SEQ.
           PERFORM CLEAR-DT-FILE.
           MOVE BRANCH TO DT-BRNO.
           MOVE LN-DLNO TO DT-DLNO.
           MOVE HOLD-TO-LN-CLASS TO DT-CLASS.
           MOVE LN-ACCTNO TO DT-ACCTNO.
           MOVE TRANS-DATE TO DT-DATE.
           MOVE SAVE-DT-SEQ TO DT-SEQ.
           MOVE "S" TO DT-TYPE.
           MOVE SAVE-LP-SEQNO TO DT-LPSEQNO.
           MOVE "TX" TO DT-TRCD.
           IF RV-LP-TRCD NOT = " "
              MOVE "RV" TO DT-TRCD
              MOVE "TX" TO DT-REVERSE-TRCD.
           MOVE "B" TO DT-PAID.

           PERFORM WRITE-DT1-FILE.
           IF IO-FG = 0
              GO TO EXIT-UPDATE-DT1-SERVICE.
           IF SAVE-DT-SEQ < 9999
              ADD 1 TO SAVE-DT-SEQ
              GO TO UPDATE-DT1-SERVICE-SEQ.
           MOVE "DEALER TRANSACTION = 9999, REC NOT CREATED !" TO MESS.
           PERFORM SEND-MESS.
       EXIT-UPDATE-DT1-SERVICE.
           PERFORM CLOSE-DT1-FILE.

      ***************************************
       UPDATE-TR1-SERVICE SECTION.
      ***************************************

      * SETUP NEW SERVICE ACCOUNT
           MOVE "UPDATING DAILY TRANSFER TRANS FILE" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-TRX1-FILE.
           MOVE 1 TO SAVE-TX-LINE.
       UPDATE-TR1-SERVICE-NEXT.
           PERFORM CLEAR-TRX-FILE.

           MOVE BRANCH           TO TX-BRNO.
           MOVE TRANS-DATE       TO TX-DATE.
           MOVE HOLD-TO-LN-CLASS TO TX-CLASS.
           MOVE LN-ACCTNO        TO TX-ACCTNO.
           MOVE LN-LAWCODE       TO TX-LAWCODE.
           MOVE SAVE-TX-LINE     TO TX-LINE.

           MOVE LN-PURCD    TO TX-PURCD.
           MOVE LN-SRCD     TO TX-SRCD.
           MOVE LN-COLLCD   TO TX-COLLCD.
           MOVE "Y"         TO TX-SERVACCT.
           MOVE LN-LOANTYPE TO TX-LOANTYPE.
      ***********************
           MOVE LN-LOANDATE TO TX-LOANDATE.
      ***********************
           COMPUTE TX-LNAMT  = 0 - (LN-SUM-LNAMT * TS-ONE).
           COMPUTE TX-FINCHG = 0 - (LN-SUM-FINCHG * TS-ONE).
           COMPUTE TX-INTCHG = 0 - (LN-SUM-INTCHG * TS-ONE).
           COMPUTE TX-SERCHG = 0 - (LN-SUM-SERCHG * TS-ONE).
           COMPUTE TX-EXTCHG = 0 - (LN-SUM-EXTCHG * TS-ONE).
           COMPUTE TX-MAINTFEE = 0 - (LN-SUM-MAINTFEE * TS-ONE).
           MOVE LN-SUM-ORGTERM TO TX-ORGTERM.
      ***********************
           MOVE LN-INSURED    TO TX-INSURED.
      ***********************
           MOVE LN-DLNO       TO TX-DLNO.
           MOVE LN-DLRECOURSE TO TX-DLRECOURSE.
      ***********************
           MOVE LN-ANTICTERM TO TX-ANTICTERM.
           MOVE LP-INTDUE    TO TX-TFRBAL.
           MOVE 1 TO SUB.
       UPDATE-TR1-SERVICE-BORR.
           IF SUB NOT > 4
              MOVE LN-SSNO(SUB) TO TX-SSNO(SUB)
              MOVE LN-MAKERCD(SUB) TO TX-MAKERCD(SUB).
      *    IF SUB NOT > 5
      *       MOVE 0 TO TX-INSCOMP(SUB)
      *       MOVE 0 TO TX-INSPREM(SUB) TX-INSEXP(SUB)
      *       MOVE 0 TO TX-INSTERM(SUB) TX-INSCOMM(SUB)
      *       MOVE SPACES TO TX-INSOURS(SUB).
      *    IF SUB NOT > 10
      *       MOVE 0 TO TX-FEEAMT(SUB).

           IF SUB < 10
              ADD 1 TO SUB
              GO TO UPDATE-TR1-SERVICE-BORR.

           PERFORM SET-POSTDATE.
           MOVE POST-USERID TO TX-USERID.
           MOVE POST-TIME TO TX-POSTTIME.
           MOVE POST-DATE TO TX-POSTDATE.

           PERFORM WRITE-TRX1-FILE.
           IF IO-FG = 0
              GO TO EXIT-UPDATE-TR1-SERVICE.

           IF SAVE-TX-LINE < 999
              ADD 1 TO SAVE-TX-LINE
              GO TO UPDATE-TR1-SERVICE-NEXT.

           MOVE "TRANSACTION SEQ = 999, REC NOT CREATED !" TO MESS.
           PERFORM SEND-MESS.
       EXIT-UPDATE-TR1-SERVICE.
           PERFORM CLOSE-TRX1-FILE.

      ******************************************
       UPDATE-DS1-READ-FILE SECTION.
      ******************************************
           PERFORM OPEN-DS1-FILE.
           MOVE "N" TO REC-EXISTS.

       UPDATE-DS1-READ-EXISTS.
           MOVE HOLD-DS1-KEY TO DS1-KEY.
           PERFORM READ-DS1-FILE.
           IF IO-FG NOT = 0
              IF REC-EXISTS = "Y"
                 GO TO IO-ERROR
              ELSE
                 MOVE "Y" TO REC-EXISTS
                 PERFORM CLEAR-DS-FILE
                 MOVE HOLD-DS1-KEY TO DS1-KEY
                 PERFORM WRITE-DS1-FILE
                 IF IO-FG = 9
                    GO TO IO-ERROR
                 ELSE
                    GO TO UPDATE-DS1-READ-EXISTS.

       UPDATE-DS1-READ-EXIT.
           EXIT.

      ***********************************
       UPDATE-TY1-READ-FILE SECTION.
      ***********************************
           PERFORM OPEN-TY1-FILE.

           MOVE "N" TO REC-EXISTS.

       UPDATE-TY1-READ-EXISTS.
           MOVE HOLD-TY1-KEY TO TY1-KEY.
           PERFORM READ-TY1-FILE.
           IF IO-FG = 0
              GO TO UPDATE-TY1-READ-EXIT.

           IF REC-EXISTS = "Y"
              GO TO IO-ERROR.

           MOVE "Y" TO REC-EXISTS.
           PERFORM CLEAR-TY-FILE.
           MOVE HOLD-TY1-KEY TO TY1-KEY.
           PERFORM WRITE-TY1-FILE.

           IF IO-FG = 9 AND E-CODE NOT = "22"
              GO TO IO-ERROR.

           GO TO UPDATE-TY1-READ-EXISTS.

       UPDATE-TY1-READ-EXIT.
           EXIT.

      *************************************
       SAVE-SCREEN SECTION.
      *************************************
           MOVE 3 TO WK-KEY.
           PERFORM OPEN-WK-FILE.
           PERFORM READ-WK-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           MOVE ELE     TO WK-ELE.
           MOVE LN-REC  TO WK-SAVE-RECORD.
           MOVE LP-REC  TO WK-LP-REC.
           MOVE LX-REC  TO WK-LX-REC.
           MOVE LXE-REC TO WK-LXE-REC.
           MOVE LXG-REC TO WK-LXG-REC.
           PERFORM REWRITE-WK-FILE.
       EXIT-SAVE-SCREEN.
           PERFORM CLOSE-WK-FILE.

      *************************************
       RESTORE-SCREEN SECTION.
      *************************************
           MOVE 3 TO WK-KEY.
           PERFORM OPEN-WK-FILE.
           PERFORM READ-WK-FILE.
           IF NEW-FORM NOT = OLD-FORM
              MOVE OLD-PROG-FORM TO WK-OLD-PROG-FORM
              MOVE NEW-PROG-FORM TO OLD-PROG-FORM.
           IF IO-FG = 0
              GO TO RESTORE-CLEAR.

           MOVE SPACES TO WK-SAVE-RECORD.
           MOVE 0      TO WK-ELE.
           PERFORM WRITE-WK-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           GO TO EXIT-RESTORE-SCREEN.
       RESTORE-CLEAR.
           MOVE WK-SAVE-RECORD TO LN-REC.
           MOVE WK-LP-REC      TO LP-REC.
           IF LP-TRCD = "RV"
              PERFORM GET-ANTIC-TAB.
           MOVE WK-LX-REC      TO LX-REC.
           MOVE WK-LXE-REC     TO LXE-REC.
           MOVE WK-LXG-REC     TO LXG-REC.
           MOVE 0              TO WK-ELE.
           PERFORM REWRITE-WK-FILE.
       EXIT-RESTORE-SCREEN.
           PERFORM CLOSE-WK-FILE.

       COPY "LIBLP/UPDATEREPAYTRP.CPY".
       COPY "LIBLP/UPDATEREPAYNO.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPCDEL.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBLP/LPEARN.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPTYCL.CPY".
       COPY "LIBLP/LPDTCL.CPY".
       COPY "LIBLP/LPTRXCL.CPY".
       COPY "LIBLP/LPTRPCL.CPY".
       COPY "LIBLP/LPDSCL.CPY".
       COPY "LIBLP/LPLXCL.CPY".
       COPY "LIBLP/POSTDATE.CPY".
       COPY "LIBLP/LPSRCD.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBGB/SCREEN.CPY".

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE LONPF-HELP--FILE TO HELP-LINK-FILE.
           MOVE LONPF-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LONPF_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *************************************
       MISCELLANEOUS-ROUTINES SECTION.
      *************************************
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".
       FORM-RESET.
       MOVE-LXT-FIELDS.
           MOVE "T"            TO LXT-CODE.
           MOVE LN-ANTICERN(1) TO LXT-EARN(1).
           MOVE LN-ANTICERN(2) TO LXT-EARN(2).
           MOVE LN-ANTICERN(3) TO LXT-EARN(3).
           MOVE LN-ANTICERN(4) TO LXT-EARN(4).
           MOVE LN-ANTICERN(5) TO LXT-EARN(5).
           MOVE LN-ANTICERN(6) TO LXT-EARN(6).
           MOVE LN-ANTICADJ(1) TO LXT-EARN(11).
           MOVE LN-ANTICADJ(2) TO LXT-EARN(12).
           MOVE LN-ANTICADJ(3) TO LXT-EARN(13).
           MOVE LN-ANTICADJ(4) TO LXT-EARN(14).
           MOVE LN-ANTICADJ(5) TO LXT-EARN(15).
           MOVE LN-ANTICADJ(6) TO LXT-EARN(16).
       MOVE-LXP-FIELDS.
           MOVE "P"          TO LXP-CODE.
           MOVE LN-ACCERN(1) TO LXP-EARN(1).
           MOVE LN-ACCERN(2) TO LXP-EARN(2).
           MOVE LN-ACCERN(3) TO LXP-EARN(3).
           MOVE LN-ACCERN(4) TO LXP-EARN(4).
           MOVE LN-ACCERN(5) TO LXP-EARN(5).
           MOVE LN-ACCERN(6) TO LXP-EARN(6).
           MOVE LN-UFERN(1) TO LXP-EARN(11).
           MOVE LN-UFERN(2) TO LXP-EARN(12).
           MOVE LN-UFERN(3) TO LXP-EARN(13).
           MOVE LN-UFERN(4) TO LXP-EARN(14).
           MOVE LN-UFERN(5) TO LXP-EARN(15).
           MOVE LN-UFERN(6) TO LXP-EARN(16).
       GET-SP.
           PERFORM OPEN-SP1-FILE.
           PERFORM READ-SP1-FILE.
           PERFORM CLOSE-SP1-FILE.
           IF IO-FG = 9
              MOVE " INVALID STATE PARAMETER RECORD" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO ERRCD.
       MOVE-IN-WORK-ANTIC.
           MOVE WK-LN-ANTICERN(1) TO LN-ANTICERN(1).
           MOVE WK-LN-ANTICERN(2) TO LN-ANTICERN(2).
           MOVE WK-LN-ANTICERN(3) TO LN-ANTICERN(3).
           MOVE WK-LN-ANTICERN(4) TO LN-ANTICERN(4).
           MOVE WK-LN-ANTICERN(5) TO LN-ANTICERN(5).
           MOVE WK-LN-ANTICERN(6) TO LN-ANTICERN(6).
           MOVE WK-LN-ANTICADJ(1) TO LN-ANTICADJ(1).
           MOVE WK-LN-ANTICADJ(2) TO LN-ANTICADJ(2).
           MOVE WK-LN-ANTICADJ(3) TO LN-ANTICADJ(3).
           MOVE WK-LN-ANTICADJ(4) TO LN-ANTICADJ(4).
           MOVE WK-LN-ANTICADJ(5) TO LN-ANTICADJ(5).
           MOVE WK-LN-ANTICADJ(6) TO LN-ANTICADJ(6).
           MOVE WK-LN-UFERN(1) TO LN-UFERN(1).
           MOVE WK-LN-UFERN(2) TO LN-UFERN(2).
           MOVE WK-LN-UFERN(3) TO LN-UFERN(3).
           MOVE WK-LN-UFERN(4) TO LN-UFERN(4).
           MOVE WK-LN-UFERN(5) TO LN-UFERN(5).
           MOVE WK-LN-UFERN(6) TO LN-UFERN(6).
           MOVE WK-LN-ACCERN(1) TO LN-ACCERN(1).
           MOVE WK-LN-ACCERN(2) TO LN-ACCERN(2).
           MOVE WK-LN-ACCERN(3) TO LN-ACCERN(3).
           MOVE WK-LN-ACCERN(4) TO LN-ACCERN(4).
           MOVE WK-LN-ACCERN(5) TO LN-ACCERN(5).
           MOVE WK-LN-ACCERN(6) TO LN-ACCERN(6).
       GET-ANTIC-TAB.
      * GET ANTICIPATED TABLE FOR REVERSAL.
           PERFORM OPEN-LX1-FILE.

           MOVE LP-BRNO   TO LX-BRNO.
           MOVE LP-ACCTNO TO LX-ACCTNO.
           MOVE LP-SEQNO  TO LX-SEQNO.
           PERFORM READ-LX1-FILE.
           IF IO-FG = 0
              MOVE LXT-EARN(1) TO WK-LN-ANTICERN(1) LN-ANTICERN(1)
              MOVE LXT-EARN(2) TO WK-LN-ANTICERN(2) LN-ANTICERN(2)
              MOVE LXT-EARN(3) TO WK-LN-ANTICERN(3) LN-ANTICERN(3)
              MOVE LXT-EARN(4) TO WK-LN-ANTICERN(4) LN-ANTICERN(4)
              MOVE LXT-EARN(5) TO WK-LN-ANTICERN(5) LN-ANTICERN(5)
              MOVE LXT-EARN(6) TO WK-LN-ANTICERN(6) LN-ANTICERN(6)
              MOVE LXT-EARN(11) TO WK-LN-ANTICADJ(1) LN-ANTICADJ(1)
              MOVE LXT-EARN(12) TO WK-LN-ANTICADJ(2) LN-ANTICADJ(2)
              MOVE LXT-EARN(13) TO WK-LN-ANTICADJ(3) LN-ANTICADJ(3)
              MOVE LXT-EARN(14) TO WK-LN-ANTICADJ(4) LN-ANTICADJ(4)
              MOVE LXT-EARN(15) TO WK-LN-ANTICADJ(5) LN-ANTICADJ(5)
              MOVE LXT-EARN(16) TO WK-LN-ANTICADJ(6) LN-ANTICADJ(6)
              MOVE LXP-EARN(1) TO WK-LN-ACCERN(1) LN-ACCERN(1)
              MOVE LXP-EARN(2) TO WK-LN-ACCERN(2) LN-ACCERN(2)
              MOVE LXP-EARN(3) TO WK-LN-ACCERN(3) LN-ACCERN(3)
              MOVE LXP-EARN(4) TO WK-LN-ACCERN(4) LN-ACCERN(4)
              MOVE LXP-EARN(5) TO WK-LN-ACCERN(5) LN-ACCERN(5)
              MOVE LXP-EARN(6) TO WK-LN-ACCERN(6) LN-ACCERN(6)
              MOVE LXP-EARN(11) TO WK-LN-UFERN(1) LN-UFERN(1)
              MOVE LXP-EARN(12) TO WK-LN-UFERN(2) LN-UFERN(2)
              MOVE LXP-EARN(13) TO WK-LN-UFERN(3) LN-UFERN(3)
              MOVE LXP-EARN(14) TO WK-LN-UFERN(4) LN-UFERN(4)
              MOVE LXP-EARN(15) TO WK-LN-UFERN(5) LN-UFERN(5)
              MOVE LXP-EARN(16) TO WK-LN-UFERN(6) LN-UFERN(6)
           ELSE
              MOVE 0 TO WK-LN-ANTICERN(1) WK-LN-ANTICERN(2)
                        WK-LN-ANTICERN(3) WK-LN-ANTICERN(4)
                        WK-LN-ANTICERN(5) WK-LN-ANTICERN(6)
                        WK-LN-ANTICADJ(1) WK-LN-ANTICADJ(2)
                        WK-LN-ANTICADJ(3) WK-LN-ANTICADJ(4)
                        WK-LN-ANTICADJ(5) WK-LN-ANTICADJ(6)
              MOVE 0 TO WK-LN-UFERN(1) WK-LN-UFERN(2) WK-LN-UFERN(3)
                        WK-LN-UFERN(4) WK-LN-UFERN(5) WK-LN-UFERN(6)
                        WK-LN-ACCERN(1) WK-LN-ACCERN(2)
                        WK-LN-ACCERN(3) WK-LN-ACCERN(4)
                        WK-LN-ACCERN(5) WK-LN-ACCERN(6).

           PERFORM CLOSE-LX1-FILE.

       SET-TP-EARNINGS.
           MOVE LN-EARN-ORDER(1) TO WK-SUB.
           IF WK-SUB > 3
      *       MOVE WK-LXE-EARN(WK-SUB - 3) TO TP-EARNINGS(1)
              MOVE LXE-EARN(WK-SUB - 3) TO TP-EARNINGS(1)
           ELSE
              MOVE LP-EARNED(WK-SUB) TO TP-EARNINGS(1).

           MOVE LN-EARN-ORDER(2) TO WK-SUB.
           IF WK-SUB > 3
      *       MOVE WK-LXE-EARN(WK-SUB - 3) TO TP-EARNINGS(2)
              MOVE LXE-EARN(WK-SUB - 3) TO TP-EARNINGS(2)
           ELSE
              MOVE LP-EARNED(WK-SUB) TO TP-EARNINGS(2).

           MOVE LN-EARN-ORDER(3) TO WK-SUB.
           IF WK-SUB > 3
      *       MOVE WK-LXE-EARN(WK-SUB - 3) TO TP-EARNINGS(3)
              MOVE LXE-EARN(WK-SUB - 3) TO TP-EARNINGS(3)
           ELSE
              MOVE LP-EARNED(WK-SUB) TO TP-EARNINGS(3).

           MOVE LN-EARN-ORDER(4) TO WK-SUB.
           IF WK-SUB > 3
      *       MOVE WK-LXE-EARN(WK-SUB - 3) TO TP-EARNINGS(4)
              MOVE LXE-EARN(WK-SUB - 3) TO TP-EARNINGS(4)
           ELSE
              MOVE LP-EARNED(WK-SUB) TO TP-EARNINGS(4).

           MOVE LN-EARN-ORDER(5) TO WK-SUB.
           IF WK-SUB > 3
      *       MOVE WK-LXE-EARN(WK-SUB - 3) TO TP-EARNINGS(5)
              MOVE LXE-EARN(WK-SUB - 3) TO TP-EARNINGS(5)
           ELSE
              MOVE LP-EARNED(WK-SUB) TO TP-EARNINGS(5).

           MOVE LN-EARN-ORDER(6) TO WK-SUB.
           IF WK-SUB > 3
      *       MOVE WK-LXE-EARN(WK-SUB - 3) TO TP-EARNINGS(6)
              MOVE LXE-EARN(WK-SUB - 3) TO TP-EARNINGS(6)
           ELSE
              MOVE LP-EARNED(WK-SUB) TO TP-EARNINGS(6).
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FULL-BRIEF-ROUTING.
           IF FULL-DISPLAY = "B"
              MOVE 0 TO NEW-PROG-SUB
              MOVE 2 TO ROUTE-CD
            ELSE
              MOVE 1 TO NEW-PROG-SUB
              PERFORM SET-NEW-LOAN-NO
              MOVE D-LNNO-BUF TO WR-BUF
              MOVE D-LNNO-LIST TO WR-LIST
              PERFORM CALL-WRFORM.
       SET-NEW-LOAN-NO.
           MOVE " " TO D-LNNO-BUF.
           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM OPEN-BW1-FILE.
           PERFORM READ-BW1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO D-LOAN-NUM.
           MOVE LC-DESC TO D-LCDESC.
       CLEAR-H05.
           MOVE "H05." TO MSEL-LIST.
           MOVE " " TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "MSEL." TO MSEL-LIST.
       GET-TP-WORKAMT.
           MOVE 0 TO TP-WORKAMT(1) TP-WORKAMT(2) TP-WORKAMT(3)
                     TP-WORKAMT(4) TP-WORKAMT(5) TP-WORKAMT(6).

           MOVE LN-EARN-ORDER(1) TO SUB.
           MOVE 1 TO SUB2.
           PERFORM MOVE-TO-HOLD-TP-WORKAMT.

           IF LN-INT-REBATE = " " OR "C"
              PERFORM CALC-TP-WORKAMT.

           MOVE LN-EARN-ORDER(2) TO SUB.
           MOVE 2 TO SUB2.
           PERFORM MOVE-TO-HOLD-TP-WORKAMT.

           IF LN-INT-REBATE = " " OR "C"
              PERFORM CALC-TP-WORKAMT.

           MOVE LN-EARN-ORDER(3) TO SUB.
           MOVE 3 TO SUB2.
           PERFORM MOVE-TO-HOLD-TP-WORKAMT.

           IF LN-SC-REBATE = " " OR "C"
              PERFORM CALC-TP-WORKAMT.

           MOVE LN-EARN-ORDER(4) TO SUB.
           MOVE 4 TO SUB2.
           PERFORM MOVE-TO-HOLD-TP-WORKAMT.

           IF LN-MF-REBATE = " " OR "C"
              PERFORM CALC-TP-WORKAMT.

           MOVE TP-WORKAMT(1) TO HOLD-TP-WORKAMT(1).
           MOVE TP-WORKAMT(2) TO HOLD-TP-WORKAMT(2).
           MOVE TP-WORKAMT(3) TO HOLD-TP-WORKAMT(3).
           MOVE TP-WORKAMT(4) TO HOLD-TP-WORKAMT(4).
           MOVE TP-WORKAMT(5) TO HOLD-TP-WORKAMT(5).
           MOVE TP-WORKAMT(6) TO HOLD-TP-WORKAMT(6).

       MOVE-TO-HOLD-TP-WORKAMT.
           IF SUB > 3
      *       MOVE WK-LXE-EARN(SUB - 3) TO HOLD-TP-WORKAMT(SUB2)
              MOVE LXE-EARN(SUB - 3) TO HOLD-TP-WORKAMT(SUB2)
           ELSE
              MOVE LP-EARNED(SUB) TO HOLD-TP-WORKAMT(SUB2).

       CALC-TP-WORKAMT.
           ADD HOLD-TP-WORKAMT(SUB2) LN-ACCERN(SUB2)
               GIVING HOLD-TP-WORKAMT(SUB2).
           IF LN-ANTICERN(SUB2) > HOLD-TP-WORKAMT(SUB2)
              IF RV-LP-TRCD NOT = "  "
                 IF SUB > 3
                    COMPUTE TP-WORKAMT(SUB2) = HOLD-TP-WORKAMT(SUB2) -
                                      (2 * LXE-EARN(SUB - 3)) -
                                            LN-ANTICERN(SUB2)
                 ELSE
                    COMPUTE TP-WORKAMT(SUB2) = HOLD-TP-WORKAMT(SUB2) -
                                      (2 * LP-EARNED(SUB)) -
                                            LN-ANTICERN(SUB2)
              ELSE
                 SUBTRACT HOLD-TP-WORKAMT(SUB2) FROM LN-ANTICERN(SUB2)
                                      GIVING TP-WORKAMT(SUB2).

      ********************************************
       IO-ROUTINES SECTION.
      ********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTYGS_SQL.CPY".
       COPY "LIBLP/LPTRXGS_SQL.CPY".
       COPY "LIBLP/LPTRPGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLXGGS_SQL.CPY".
       COPY "LIBLP/LPLXEGS_SQL.CPY".
       COPY "LIBLP/LPLXGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPNOGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPDTGS_SQL.CPY".
       COPY "LIBLP/LPDSGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPLP1IN.CPY".
       COPY "LIBLP/LPLX1IN.CPY".
       COPY "LIBLP/LPLXE1IN.CPY".
       COPY "LIBLP/LPLXG1IN.CPY".
       COPY "LIBLP/LPNO1IN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPTY1IN.CPY".
       COPY "LIBLP/LPDS1IN.CPY".
       COPY "LIBLP/LPWKI.CPY".
       COPY "LIBLP/LPTRP1IN.CPY".
       COPY "LIBLP/LPTRX1IN.CPY".
       COPY "LIBLP/LPDT1IN.CPY".
       COPY "LIBLP/LPREPAYTRPI.CPY".
       COPY "LIBLP/LPREPAYNOI.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.
       END-ROUTINE.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
