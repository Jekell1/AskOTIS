       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LONPG6.
       DATE-WRITTEN.  08/20/90.
      ******************************************************************
      *  DESCRIPTION:  ADDON ADJUSTMENT UPDATE
      *
      * REV:
      *  061495 MJD SET UP TP-POSTTIME AND TP-USERID.
      *  062895 MJD ADDED COPY MEMBERS LIBLP/POSTDATE AND POSTDATEW.
      *             SET TP-POSTDATE.
      *  071995 JTG ADDED LOGIC FOR LN-PRPDUE-DAYS & LN-PRLNAMT
      *  072195 JTG ADDED LPSRCD LOGIC
      *  072895 JTG ADDED LOGIC FOR DELSUB-SET ROUTINE
      *  081195 MJD ADDED GP-FILE FOR LIBLP/LPSRCD :
      *                   GP-THE-ONLY-SF-SRCD.
      *  MJD 091895 ADDED DS-DATE LOGIC
      *  MJD 071296 INCREASED SAVE-PDTH-DATE TO 9(6), USE PDTH-DATE-FULL
      *             INSTEAD OF PDTH-DATE.
      *  MJD 071696 ADDED LIBLP/LPUPWK FOR CHANGES IN LIBLP/AGEING
      *  MJD 071796 ADDED LOGIC FOR 6 DIGIT PDTH DATES
      *  MJD 071896 ADDED TP-UNITPER-CD AND TP-UNITPER-FREQ
      *  MJD 021997 REMOVED LP..PD, AND PD IO ROUTINES. PROGRAM ONLY USING
      *             PD-REC AS HOLD FOR LN-REC. CHANGED UPDATES FOR TY-FILE,
      *             AND DSFILE. (USED LONPG4 AS BASIS FOR CHANGES)
      *  MJD 042597 ADDED LOADLXA(W) LOGIC TO READ SECOND ADDON RECORD LX2.
      *  MJD 060697 ADDED SETCTBL(W)
      *  MJD 051397 USE LN/PD-SMPRATE INSTEAD OF LN/PD-EFFRATE.
      *  GLF 090997 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *  MJD 091797 REMOVED DELSUB-SET LOGIC; ADDED LPPDUE(W) COPY MEMBERS
      *  MJD 101797 MOVED WK-UPDATE-BUF TO LP01WK
      *  BAH 111197 ADDED HOLD-SP1-KEY TO CL/LEDGER LINKAGE CALL
      *  MJD 052198 REMOVED XY-PASTDUE.
      *  BAH 062298 MOVE LT-ACCERN(1) TO TP-EARNINGS(2) SO LPEOD5 WILL
      *             UPDATE ADDON INTEREST INSTEAD OF INTEREST, CASTLE PL# 186
      *  BAH 062598 CHANGED TP-TILL PIC 9 TO TP-TILLNO PIC X, CHANGED LP-TILLNO
      *             TO PIC X.
      *  JTG 063098 EXPANDED DS-APP-TYPE-TBL TO 3 LEVELS
      *  MJD 990420 CHANGED TO INCLUDE/EXCLUDE O2 IN DELINQ BASED
      *             ON GP-DELINQ-WITH-O2
      *  JTG 991011 CHANGED SETUP-LINE FOR 'AP' DISPLAY REVOLVING, #47 REGENCY
      *             AND CHANGED CALL TO CL/LEDGER.C TO PASS LC-REC
      *  JTG 991109 ALLOWED ACCOUNTS TO HAVE ADDON'S WHEN IN REPO STATUS,
      *             CHANGED UPD OF DSFILE, TYFILE & LPEOD5TR, NEWPORT #244, A15
      *  MJD 000314 IF REGACC DO NOT UPDATE VOLUME STATS (REGACC PR#1106)
      *  JTG 000328 MODIFIED TO UPDATE UNEARNED STATISTICS, MERCURY #223
      *  MJD 001016 ADDED UPDATE OF TP-COLLECTOR AND LP-COLLECTOR (BAY PR#1317)
      *  BAH 010205 ADDED UPDATE OF TP-REPOCD, WRONG RECEIVABLE BEING UPDATED
      *             IF REPO !! NEWPORT, LAURA
      *  BAH 010716 SET TP-JDCD AND TP-PLCD, HAD LOW-VALUES CAUSING JUNK ON
      *             TRANSACTION PAYMENT FILE MAINTENANCE SCAN, HCRED LAURA
      *  BAH 011109 SET TP-INSCOMP, REGACC PR# 1617
      *  BAH 020722 SET TP-INSOURS, FORGOT FROM WAY BACK !
      *  BAH 030425 ADDED LC-USE-ALT-GL-FEE AND CD-GC-ALT-GLFEENO FOR
      *             LENDMARK PR# 2048
      *  BAH 040318 TP-DUEDATE WAS PAID THRU INSTEAD OF WHAT THE DUEDATE WAS
      *   CS 050510 ADJUSTMENTS WERE ALWAYS POSITIVE WHEN UPDATING TYFILE.
      *             CHANGED TO USE WK-PD AMOUNTS INSTEAD OF PD AMOUNTS.  THE
      *             WK-PD AMOUNTS ARE MULTIPLIED BY -1 IF PD-PROCASH < 0
      *             REPORTED BY MULLEN  [PR#490]
      *   CS 080918 WHEN IB LOAN ADJUST-INTEREST WHERE INTPDTH > PD-DATE WAS
      *             OVERCHARGING INTEREST FROM ADJUST-INTEREST SECTION. IT WAS
      *             A 2 STEP PROCESS ASSESSING INTEREST FROM INTPDTH TO PD-DATE.
      *             PROBLEM WHEN ADDON BACKDATED PRIOR TO INTPDTH... ASSESSING
      *             INTEREST ON CURBAL + ADDON AMOUNT. JR SAYS CHANGE TO
      *             ALWAYS ASSESS INTEREST TO TRANSACTION DATE.
      *   CS 090722 FIXED ADJUST-INTEREST, WAS IN A LOOP....
      *   CS 151222 SET LP-BRNO FROM LN-OWNBR, CHANGED HOLD-KEY X(12) TO HOLD-
      *             LP-KEY X(13), LP-SEQNO INCREASED TO 5
      *  BAH 160118 SPLIT OUT LTFILE, PD0003
      *   CS 160216 CHANGED LOADLXA TO POPULATE LXA FIELDS 1 - 9, I LEFT THE
      *             COPY MEMBER BECAUSE ITS USED IN ALL THE ADDON UPDATE
      *              PROGRAMS
      *  BLM 160310 FIX FOR NEW TY DATE FORMAT, PD0003
      *
      * KEC 2016.0315 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-FEE???? TO CD-GC-????.
      * BLM 160513 FIX FOR NEW DS-DATE FORMAT, FIX DS-UNEARNED, PD0003
      *
      * KEC 2016.1025 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED USING HOLD-TR1-KEY.
      *         ADDED LIBLP/LPWSTR (NEW) FOR LPWSTR-TR1-KEY (HOLD/SAVE).
      *
      * KEC 2017.0105 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED DS-DATE TO DS-DATE-CCYYMM FOR CONSISTENCY
      *          WITH THE DATE FIELDS IN BY & UD FILE.
      *
      * BAH 170623 ACTIVATE 8 DIGIT DATES, PD0006
      *
      * KEC 2017.0727 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED HOLD-LP-KEY TO SAVE-LP1-KEY; COULD NOT RENAME TO
      *         HOLD-LP1-KEY SINCE ALREADY USED.
      * BAH 181231 GLOBAL LPFILE
      * BAH 20190308 CHANGED LP-ADDON TO USE LPWSLP-LP-ADDON, MOVED LP-REFNO
      *              REDEFINITIONS TO LPWSLP COPY MEMBER
      * BAH 20190719 ADDED CLEAR-LTI-FILE (LPLTI) FUTURE FIELDS NOT SET
      * BLM 20210810 REMOVE TP-CODE, TP-POOLID NO LONGER IN KEY
      * BAH 20220422 UPDATE TP-ACTIONCD2, SECURITIZATION #1551
      * BAH 2024.0130 CALL CRNOR2 AND WRITE NOTICE RECORD
      * BAH 2024.0508 SET TP-ADDON-TXT AND TP-ADDON-TYP
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBLP/LPFSWK.CPY".

       I-O-CONTROL.

       DATA DIVISION.
       FILE SECTION.
       COPY "LIBLP/LPFDWK.CPY".

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./LP/LONPG6.CBL S34 08/11/21-10:33:12 >".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LP01NO_SQL.CPY".
       COPY "LIBLP/LPWSNO.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01LX.CPY".
       COPY "LIBLP/LP01LX_SQL.CPY".
       COPY "LIBLP/LPWSLX.CPY".
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
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01TY.CPY".
       COPY "LIBLP/LP01TY_SQL.CPY".
       COPY "LIBLP/LPWSTY.CPY".
       COPY "LIBLP/LP01DS.CPY".
       COPY "LIBLP/LP01DS_SQL.CPY".
       COPY "LIBLP/LPWSDS.CPY".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".
       COPY "LIBLP/LP01TRP.CPY".
       COPY "LIBLP/LP01TRP_SQL.CPY".
       COPY "LIBLP/LPWSTRP.CPY".
      *-----------------------------------------------------------------
       COPY "LIBLP/LPWKPATH.CPY".

       01  WK-KEY              PIC 99.

       01  MSEL-LIST           PIC X(5)     VALUE "MSEL.".
       01  HOLD-TP-LINE        PIC 9(3).

       01  SAVE-LP1-KEY         PIC X(17)    VALUE SPACES.

       01  PRIOR-RDSUB           PIC 99       COMP VALUE 0.
       01  PRIOR-CSUB            PIC 99       COMP VALUE 0.
       01  PRIOR-SAVE-OUTBAL     PIC S9(7)V99 COMP VALUE 0.

       01  AFTER-RDSUB           PIC 99       COMP VALUE 0.
       01  AFTER-CSUB            PIC 99       COMP VALUE 0.
       01  AFTER-SAVE-OUTBAL     PIC S9(7)V99 COMP VALUE 0.

       01  REPO-AR-SUB           PIC S9(5)    COMP.

       01  BSCHD-FG              PIC X.
       01  BSCHD-SUB             PIC S999     COMP VALUE 0.
       01  BSCHD-TERM            PIC S999     COMP VALUE 0.
       01  BSCHD-NEW-AMTDUE      PIC S9(7)V99 COMP VALUE 0.
       01  BSCHD-REGPYAMT        PIC S9(7)V99 COMP VALUE 0.
       01  BSCHD-AMT             PIC S9(7)V99 COMP VALUE 0.
       01  BSCHD-NET-CHANGE      PIC S9(7)V99 COMP VALUE 0.

       01  ASCHD-SUB             PIC S999     COMP VALUE 0.
       01  ASCHD-AMTDUE          PIC S9(7)V99 COMP VALUE 0.
       01  ADJ-INT-DUE           PIC S9(7)V99 COMP VALUE 0.
       01  HOLD-LN-CURBAL        PIC S9(7)V99 COMP VALUE 0.
       01  HOLD-LN-INTPDTH-DATE  PIC 9(8)     COMP VALUE 0.
       01  WK-PD-LNAMT           PIC S9(7)V99 COMP VALUE 0.
       01  WK-PD-REGPYAMT        PIC S9(5)V99 COMP VALUE 0.
       01  WK-PD-PROCEEDS        PIC S9(5)V99 COMP VALUE 0.
       01  WK-PD-FINCHG          PIC S9(7)V99 COMP VALUE 0.
       01  WK-PD-ANTICERN1       PIC S9(7)V99 COMP VALUE 0.
       01  WK-PD-ANTICERN3       PIC S9(7)V99 COMP VALUE 0.
       01  WK-PD-ANTICERN4       PIC S9(7)V99 COMP VALUE 0.
       01  WK-PD-FEEAMT-01.
           03  WK-PD-FEEAMT      PIC S9(5)V99 COMP OCCURS 2 TIMES.
       01  WK-PD-INTCHG          PIC S9(7)V99 COMP VALUE 0.
       01  WK-PD-SERCHG          PIC S9(7)V99 COMP VALUE 0.
       01  WK-PD-EXTCHG          PIC S9(7)V99 COMP VALUE 0.
       01  WK-PD-MAINTFEE        PIC S9(7)V99 COMP VALUE 0.
       01  GLNO                  PIC 9(11)     VALUE 0.
       01  GLNOZ REDEFINES GLNO.
           03  GLNO-BR           PIC 9(4).
           03  GLNO-NUMBER       PIC 9(7).
       01  SAVE-LT-SEQ           PIC 9(3)     COMP   VALUE 0.
       01  SAVE-CUR-SCHD-DATE    PIC 9(8)     COMP VALUE 0.
       01  SAVE-PDTH-DATE        PIC 9(8)     COMP   VALUE 0.
       01  SAVE-INDU-INTEREST    PIC S9(7)V99 COMP   VALUE 0.
       01  SAVE-LN-INTPDTH-DATE  PIC 9(8)     COMP   VALUE 0.
       01  SAVE-IBPC-FG          PIC X               VALUE " ".

      * LXA SAVE FIELDS:
       01  SAVE-PMT-LXA-TABLE.
           03  SAVE-PMT-SCHED-CHNG OCCURS 9 TIMES.
               05  SAVE-START-PMT  PIC 999      COMP-3.
               05  SAVE-END-PMT    PIC 999      COMP-3.
               05  SAVE-NET-CHANGE PIC S9(6)V99 COMP.

      *    -------------------------------------------------------------
      *    ONE =  1 IF ADJUSTMENT IS POSITIVE.
      *    ONE = -1 IF ADJUSTMENT IS NEGATIVE.
      *    -------------------------------------------------------------
       01  ONE                 PIC S9        COMP.
       01  SUB                 PIC S9(5)     COMP  VALUE +0.
       01  SUB1                PIC S9(5)     COMP  VALUE +0.
       01  INS-SUB             PIC 99        COMP  VALUE 0.
       01  REC-EXISTS          PIC X         VALUE SPACES.
       01  WORKER2             PIC S9(7)V99  COMP.

      * PD-REC USED ONLY AS HOLD RECORD FOR LN-REC.
       COPY "LIBLP/LP01PD.CPY".

       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBCL/CRNOR2W.CPY".
       COPY "LIBLP/LPIBPCW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPXDTEW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPMENUW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBLP/LPSCHDW.CPY".
       01  ST-SCHD-PAYMENT-TABLE  OCCURS 360.
           03  ST-SCHD-AMT             PIC S9(6)V99 COMP.
      * ST-SCHD-SUB HOLD START INDEX FOR LOAN TRAILER UPDATE.
       01  ST-SCHD-SUB                 PIC 999      COMP.
       01  ST-SCHD-SUB2                PIC 999      COMP.
       COPY "LIBLP/LPRATEW.CPY".
       COPY "LIBGB/REBATEW.CPY".
       COPY "LIBLP/LPSETTYW.CPY".
       COPY "LIBLP/LPDSCLW.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBLP/POSTDATEW.CPY".
       COPY "LIBLP/LPSRCDW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBLP/LOADLXAW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LONPG_DEF.CPY".
       COPY "LIBLP/LONPG_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      /*************************************************
      *   C O N T R O L   M O D U L E                  *
      **************************************************
       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPPFBUF.CPY".
       COPY "LIBLP/LPLOANW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/LONPG_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME PROG-BUF UPDATE-BUF.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               WK-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-TY1-FILE.
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LX1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LTI1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-NO1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-DS1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LONPG" TO VDU-FORM-NAME.

           MOVE USER-PID TO TEMP-PID.

           MOVE SPACE TO MESS.

           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 9
              MOVE 0 TO SAVE-START-PMT(SUB1)
                        SAVE-END-PMT(SUB1)
                        SAVE-NET-CHANGE(SUB1)
           END-PERFORM.

           MOVE 8 TO WK-KEY.
           PERFORM OPEN-WK-FILE.
           PERFORM READ-WK-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.
           MOVE WK-SAVE-REC TO PD-RECORD.

      *    -------------------------------------------------------------
      *    READ LOAN RECORD, NEEDED FOR CALC'S
      *    -------------------------------------------------------------
           MOVE PD-OWNBR  TO LN-OWNBR.
           MOVE PD-ACCTNO TO LN-ACCTNO.
           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

      * GET ORIGINAL LOAN RECORD  (KEY 9)

           MOVE 9 TO WK-KEY.
           PERFORM READ-WK-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.
           IF LN-REC NOT = WK-SAVE-REC
              MOVE
              "LOAN RECORD HAS CHANGED, RE-INPUT ADDON ENTRY!"
                                                        TO MESS
              PERFORM SEND-MESS
              GO TO END-ROUTINE.

      * SETUP FOR REPO PROCESSING:

           IF LN-IS-REPO-AR
              IF LN-REPO-ONHAND
                 MOVE 1 TO REPO-AR-SUB
              ELSE
                 MOVE 3 TO REPO-AR-SUB.

           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           PERFORM OPEN-SP1-FILE.
           PERFORM READ-SP1-FILE.
           PERFORM CLOSE-SP1-FILE.
           IF IO-FG NOT = 0
              MOVE " INVALID STATE PARAMETER RECORD" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.
           MOVE SP1-KEY TO HOLD-SP1-KEY.

           MOVE BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID BRANCH RECORD" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.

           PERFORM OPEN-LC1-FILE.
           MOVE LN-CLASS TO LC-CODE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           PERFORM GET-GPENV.

      * GET DELINQUENCY PRIOR TO LOAN UPDATE:

           PERFORM GET-CON-REC-PRIOR-LNUPD.

      * GET LOAN SOURCE CODE INDEX:

           PERFORM GET-SRCD-SUB.

           MOVE LN-1STPYDATE TO PDTH-DATE-WORK.
           PERFORM PAID-THRU-CALCULATION.
           MOVE PDTH-DATE-FULL     TO SAVE-PDTH-DATE.
           MOVE PDTH-CUR-SCHD-DATE TO SAVE-CUR-SCHD-DATE.

           MOVE PD-LOANDATE TO IBPC-DATE.
           PERFORM IBPC-TEST.
           MOVE IBPC-FG TO SAVE-IBPC-FG.

           MOVE LN-INTPDTH-DATE TO SAVE-LN-INTPDTH-DATE.

           IF SAVE-IBPC-FG = "I"
              PERFORM ADJUST-INTEREST
              MOVE ADJ-INT-DUE TO SAVE-INDU-INTEREST
           ELSE
              MOVE 0 TO SAVE-INDU-INTEREST.

      * SETUP SUBSCRIPT FROM THE VERY START !!
           MOVE PD-PROCASH TO INS-SUB.

      * ONE =  1 IF ADJUSTMENT IS POSITIVE.
      * ONE = -1 IF ADJUSTMENT IS NEGATIVE.

           IF PD-PROCASH < 0
              MOVE -1 TO ONE
              MULTIPLY -1 BY PD-LNAMT GIVING WK-PD-LNAMT
              MULTIPLY -1 BY PD-REGPYAMT GIVING WK-PD-REGPYAMT
              MULTIPLY -1 BY PD-PROCEEDS GIVING WK-PD-PROCEEDS
              MULTIPLY -1 BY PD-FINCHG GIVING WK-PD-FINCHG
              MULTIPLY -1 BY PD-ANTICERN(1) GIVING WK-PD-ANTICERN1
              MULTIPLY -1 BY PD-ANTICERN(3) GIVING WK-PD-ANTICERN3
              MULTIPLY -1 BY PD-ANTICERN(4) GIVING WK-PD-ANTICERN4
              MULTIPLY -1 BY PD-FEEAMT(1) GIVING WK-PD-FEEAMT(1)
              MULTIPLY -1 BY PD-FEEAMT(2) GIVING WK-PD-FEEAMT(2)
              MULTIPLY -1 BY PD-INTCHG GIVING WK-PD-INTCHG
              MULTIPLY -1 BY PD-SERCHG GIVING WK-PD-SERCHG
              MULTIPLY -1 BY PD-EXTCHG GIVING WK-PD-EXTCHG
              MULTIPLY -1 BY PD-MAINTFEE GIVING WK-PD-MAINTFEE
           ELSE
              MOVE 1 TO ONE
              MOVE PD-LNAMT TO WK-PD-LNAMT
              MOVE PD-REGPYAMT TO WK-PD-REGPYAMT
              MOVE PD-PROCEEDS TO WK-PD-PROCEEDS
              MOVE PD-FINCHG TO WK-PD-FINCHG
              MOVE PD-ANTICERN(1) TO WK-PD-ANTICERN1
              MOVE PD-ANTICERN(3) TO WK-PD-ANTICERN3
              MOVE PD-ANTICERN(4) TO WK-PD-ANTICERN4
              MOVE PD-FEEAMT(1) TO WK-PD-FEEAMT(1)
              MOVE PD-FEEAMT(2) TO WK-PD-FEEAMT(2)
              MOVE PD-INTCHG TO WK-PD-INTCHG
              MOVE PD-EXTCHG TO WK-PD-EXTCHG
              MOVE PD-SERCHG TO WK-PD-SERCHG
              MOVE PD-MAINTFEE TO WK-PD-MAINTFEE.

           PERFORM UPDATE-BW1-FILE.

           PERFORM UPDATE-DS1-FILE.

           PERFORM UPDATE-TY1-FILE.

           PERFORM UPDATE-LT-SCHLD.

           PERFORM UPDATE-LT-INS.

           PERFORM UPDATE-LP1-FILE.

           PERFORM UPDATE-LX1-FILE.

      *    PERFORM UPDATE-DT1-FILE.

           PERFORM UPDATE-TR1-FILE.

      * UPDATE THE ORIGINAL LOAN RECORD LAST !!!!
           PERFORM UPDATE-LN1-FILE.

           PERFORM UPDATE-NOTICE-FILE.

           PERFORM GET-CON-REC-AFTER-LNUPD.
           PERFORM UPDATE-DS1-AFTER.
           PERFORM UPDATE-TY1-AFTER.

       LOAN-UPDATE-EXIT.
           MOVE 0 TO NEW-PROG-SUB.
           MOVE "P" TO ERRCD.
           MOVE " " TO WK-SAVE-REC.
           MOVE 0 TO WK-ELE.
           MOVE 8 TO WK-KEY.
           PERFORM REWRITE-WK-FILE.
           GO TO END-ROUTINE.

      **********************************************
       UPDATE-BW1-FILE SECTION.
      **********************************************
           MOVE "UPDATING BORROWER FILE.............." TO MESS.
           PERFORM SEND-LEGEND.

           MOVE 1 TO SUB.
           PERFORM OPEN-BW1-FILE.
       UPDATE-NEXT-BW.
           MOVE PD-SSNO(SUB) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.
           IF PD-SERVACCT = "Y"
              ADD WK-PD-LNAMT TO BW-LIAOTH
           ELSE
              IF PD-SMALLOAN = "Y"
                 ADD WK-PD-LNAMT TO BW-LIASMLN
              ELSE
                 ADD WK-PD-LNAMT TO BW-LIAOTH.
           PERFORM REWRITE-BW1-FILE.
           IF SUB = 4
              GO TO EXIT-UPDATE-BW1.
           ADD 1 TO SUB.
           IF PD-SSNO(SUB) NOT = 0
              GO TO UPDATE-NEXT-BW.
       EXIT-UPDATE-BW1.
           PERFORM CLOSE-BW1-FILE.

      **********************************************
       UPDATE-DS1-FILE SECTION.
      **********************************************
           MOVE "UPDATING DEALER STATISTICS FILE........." TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM GET-DS1-FILE.

           IF LN-IS-REPO-AR
              ADD WK-PD-LNAMT TO DS-AMT-REPO(DSCUR DSNET REPO-AR-SUB)
           ELSE
              ADD WK-PD-LNAMT TO DS-OUTSAMT(1).

      *    IF NOT GP-I-AM-REGACC
              IF LN-IS-REPO-AR
                 ADD WK-PD-LNAMT TO DS-AMT-REPO(DSMTD DSVOL REPO-AR-SUB)
                                    DS-AMT-REPO(DSYTD DSVOL REPO-AR-SUB)
              ELSE
      * THE ORIGINAL LOAN'S SOURCE CODE IS IN PD-POCD:
                 IF PD-POCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO"
                    ADD WK-PD-LNAMT TO DS-RNEWAMT(1) DS-RNEWAMT(2)
                 ELSE
                 IF PD-SALEFIN = "Y" AND GP-THE-ONLY-SF-SRCD NOT = " "
                    IF PD-POCD = GP-THE-ONLY-SF-SRCD
                       ADD WK-PD-LNAMT TO DS-NPURAMT(1) DS-NPURAMT(2)
                    END-IF
                 ELSE
                 IF PD-POCD = "NB"
                    ADD WK-PD-LNAMT TO DS-NPURAMT(1) DS-NPURAMT(2)
                 ELSE
                    ADD WK-PD-LNAMT TO DS-OPURAMT(1) DS-OPURAMT(2).

      *    -------------------------------------------------------------

           IF LN-SERVACCT NOT = "Y"
              IF PD-DLRECOURSE = "F"
                 ADD WK-PD-LNAMT TO DS-RCOURSE-AMT(1)
              ELSE
                 IF PD-DLRECOURSE = "P"
                    ADD LN-DLPARTRCAMT TO DS-RCOURSE-AMT(2)
                 ELSE
                    IF LN-DLRECOURSE = "N"
                       ADD WK-PD-LNAMT TO DS-RCOURSE-AMT(3).

      * UPDATE DELINQUENCY:
           PERFORM UPDATE-DS1-PRIOR.

           PERFORM REWRITE-DS1-FILE.
           PERFORM CLOSE-DS1-FILE.

      **************************************************
       UPDATE-TY1-FILE SECTION.
      **************************************************

      * UPDATE TY-FILE    NEW STATISTICS

           MOVE "UPDATING TODAY'S TOTAL FILE............." TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM GET-TY1-FILE.

       TY-UPDATE-REC.

           IF LN-IS-REPO-AR
              ADD WK-PD-LNAMT  TO TY-AMT-REPO(1 REPO-AR-SUB)
              ADD WK-PD-INTCHG TO TY-INT-REPO(1 REPO-AR-SUB)
              ADD WK-PD-EXTCHG TO TY-INT-REPO(1 REPO-AR-SUB)
           ELSE
              ADD WK-PD-LNAMT  TO TY-AMT(1 SRCD)
              ADD WK-PD-INTCHG TO TY-INT(1 SRCD)
              ADD WK-PD-EXTCHG TO TY-INT(1 SRCD).

      *    IF NOT GP-I-AM-REGACC
              IF LN-IS-REPO-AR
                 ADD WK-PD-LNAMT  TO TY-AMT-REPO(2 REPO-AR-SUB)
                 ADD WK-PD-INTCHG TO TY-INT-REPO(2 REPO-AR-SUB)
                 ADD WK-PD-EXTCHG TO TY-INT-REPO(2 REPO-AR-SUB)
              ELSE
                 ADD WK-PD-LNAMT  TO TY-AMT(2 SRCD)
                 ADD WK-PD-INTCHG TO TY-INT(2 SRCD)
                 ADD WK-PD-EXTCHG TO TY-INT(2 SRCD)
              END-IF
      * UPDATE INSURANCES WROTE:
              MOVE WK-PD-LNAMT TO WORKER2
              IF PD-LOANTYPE = "P"
                 SUBTRACT WK-PD-FINCHG FROM WORKER2
              END-IF
              IF INS-SUB = 1 OR 2 OR 3 OR 4 OR 5 OR 6
                 ADD WORKER2 TO TY-INS-AMT-WROTE(INS-SUB).

      * UPDATE DELINQUENCY
           PERFORM UPDATE-TY1-PRIOR.

       UPDATE-TY-EXIT.
           PERFORM REWRITE-TY1-FILE.
           PERFORM CLOSE-TY1-FILE.

      ********************************************************
       UPDATE-TR1-FILE SECTION.
      ********************************************************
           MOVE "UPDATING DAILY TRANSACTION FILE........." TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-TRP1-FILE.
           MOVE 0          TO HOLD-TP-LINE.

           MOVE BRANCH     TO TP-BRNO.
           MOVE TRANS-DATE TO TP-DATE.
           MOVE PD-CLASS   TO TP-CLASS.
           MOVE PD-ACCTNO  TO TP-ACCTNO.
       TR-CHECK-EXISTS.
           ADD 1             TO HOLD-TP-LINE.
           MOVE HOLD-TP-LINE TO TP-LINE.

           MOVE TRP1-KEY TO LPWSTRP-TRP1-KEY.
           PERFORM READ-TRP1-FILE.
           IF IO-FG = 0
      *       UNLOCK TRP-FILE
              GO TO TR-CHECK-EXISTS.

           PERFORM CLEAR-TRP-FILE.

           MOVE LPWSTRP-TRP1-KEY TO TRP1-KEY.

           MOVE BRANCH         TO TP-POSTING-BR.
           MOVE PD-LAWCODE     TO TP-LAWCODE.
           MOVE PD-PLCD        TO TP-PLCD.
           IF PD-JDDATE NOT = 0
              MOVE "J"         TO TP-JDCD
           ELSE
              MOVE " "         TO TP-JDCD.
           MOVE LP-SSNO        TO TP-SSNO.
           MOVE "M"            TO TP-MAKER.
           MOVE LP-IBPC        TO TP-IBPC.
           MOVE LP-TRCD        TO TP-TRCD.

           MOVE LP-REFNO             TO TP-REFNO LPWSLP-LP-ADDON.
           MOVE LPWSLP-LP-ADDON-TEXT TO TP-ADDON-TXT.
           MOVE LPWSLP-LP-ADDON-TYPE TO TP-ADDON-TYP.
           MOVE LP-PAYDATE           TO TP-PAYDATE.
           MOVE SAVE-CUR-SCHD-DATE   TO TP-DUEDATE.

           ADD LP-TRAMT TO TP-TRAMT.

           ADD LP-APINT TO TP-APINT.
           ADD LP-APCUR TO TP-APCUR.

           ADD LP-OTHBAL TO TP-OTHBAL.
           ADD LP-OT2BAL TO TP-OT2BAL.
           ADD LP-INTBAL TO TP-INTBAL.
           ADD LP-LCBAL TO TP-LCBAL.
           ADD LP-CURBAL TO TP-CURBAL.
           MOVE LTI-INSCOMM TO TP-INSCOMM.
           MOVE LTI-INSCOMP TO TP-INSCOMP.
           MOVE LTI-INSOURS TO TP-INSOURS.

      * TP-WORKAMT(2) = ADDON INTEREST
           COMPUTE TP-WORKAMT(2) = 0 - LTI-ANTICERN(1).

      * TP-EARNINGS(2) = ADDON INTEREST
           MOVE LTI-ACCERN(1) TO TP-EARNINGS(2).

           PERFORM UPDATE-TP-GLNO.

           MOVE LP-PDTH-DATE TO TP-PDTH-DATE.
           MOVE LP-TILLNO TO TP-TILLNO.
           MOVE LP-LCPDTH-DATE TO TP-LCPDTH-DATE.
           MOVE PD-DLNO TO TP-DLNO.
           MOVE LP-DLPROC TO TP-DLPROC.
           MOVE LP-INTPAID TO TP-INTPAID.

           MOVE LP-NEWSTAT-RATE TO TP-NEWSTAT-RATE.

           MOVE PD-SERVACCT TO TP-SERVACCT.
           MOVE PD-SALEFIN TO TP-SALEFIN.
           MOVE PD-LOANTYPE TO TP-LOANTYPE.
           MOVE PD-INSURED TO TP-INSURED.
           MOVE PD-INSTYPES TO TP-INSTYPES.

           PERFORM SET-POSTDATE.
           MOVE POST-USERID  TO TP-USERID.
           MOVE POST-TIME    TO TP-POSTTIME.
           MOVE POST-DATE    TO TP-POSTDATE.
           MOVE LN-COLLECTOR TO TP-COLLECTOR.
           MOVE LN-REPOCD    TO TP-REPOCD.
           MOVE LN-ACTIONCD2 TO TP-ACTIONCD2.

           MOVE LN-UNITPER-CD TO TP-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO TP-UNITPER-FREQ.

           PERFORM WRITE-TRP1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           PERFORM CLOSE-TRP1-FILE.

      ********************************************
       UPDATE-LN1-FILE SECTION.
      ********************************************
           MOVE "UPDATING LOAN MASTER FILE..............." TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "Y" TO LN-ADDON-FG LN-PAY-SCHLD-FG.

      * UPDATE INTEREST ASSESSED THRU ADDON IF IB:
           IF IBPC-FG = "I"
              ADD SAVE-INDU-INTEREST TO LN-INTBAL
              MOVE TRANS-DATE TO LN-INTPDTH-DATE.

      * UPDATE LOAN BALANCES:
           ADD WK-PD-LNAMT TO LN-CURBAL LN-PURBAL.

      * UPDATE ADDON INSURANCE:
           ADD WK-PD-PROCEEDS TO LN-INSPREM(INS-SUB).
           ADD LTI-INSCOMM TO LN-INSCOMM(INS-SUB).

      * UPDATE INS. CONSOLIDATED INFO:
           IF PD-SRCD = "RT"
              ADD WK-PD-REGPYAMT TO LN-SUM-REGPYAMT.
           ADD WK-PD-LNAMT TO LN-SUM-LNAMT LN-SUM-ORIGPURAMT.
           ADD WK-PD-FINCHG TO LN-SUM-FINCHG.
           ADD WK-PD-INTCHG TO LN-SUM-INTCHG.
           ADD WK-PD-EXTCHG TO LN-SUM-EXTCHG.
           ADD WK-PD-SERCHG TO LN-SUM-SERCHG.
           COMPUTE LN-SUM-MAINTFEE = LN-SUM-MAINTFEE +
                                     (WK-PD-MAINTFEE * PD-ORGTERM).

           MOVE SCHD-TERM TO LN-SUM-ORGTERM.

           ADD LTI-ANTICERN(1) TO LN-ANTICERN(2).
           ADD LTI-ANTICADJ(1) TO LN-ANTICADJ(2).
           ADD LTI-UFERN(1) TO LN-UFERN(2).
           ADD LTI-ACCERN(1) TO LN-ACCERN(2).

           PERFORM REWRITE-LN1-FILE.
           PERFORM CLOSE-LN1-FILE.

      ***********************************************
       UPDATE-NOTICE-FILE SECTION.
      ***********************************************
           PERFORM OPEN-NO1-FILE.
           MOVE LN-OWNBR  TO NO-BRNO.
           MOVE LN-ACCTNO TO NO-ACCTNO CRNOR2-ACCTNO.
           PERFORM READ-NO1-FILE.

           MOVE TRANS-DATE    TO CRNOR2-SYSDATE.
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

      *****************************************
       UPDATE-LT-INS SECTION.
      *****************************************
           MOVE "UPDATING INSURANCE TRAILER FILE........" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-LTI1-FILE.

           MOVE 1 TO SAVE-LT-SEQ.
       UPDATE-LT-INS-NEXT-SEQ.
           PERFORM CLEAR-LTI-FILE.

           MOVE PD-OWNBR    TO LTI-BRNO.
           MOVE PD-ACCTNO   TO LTI-ACCTNO.
           MOVE SAVE-LT-SEQ TO LTI-SEQNO.

           MOVE INS-SUB TO LTI-INS-TYPE.
           MOVE PD-1STPYDATE TO LTI-ORIG-1STPYDATE.
           ADD LN-TOTNODEF LN-TOTNOALDEL GIVING LTI-PRIOR-NODEF.

           MOVE PD-FEECODE(1)   TO LTI-INSFEECD(1).
           MOVE WK-PD-FEEAMT(1) TO LTI-INSFEE(1).
           MOVE PD-FEECODE(2)   TO LTI-INSFEECD(2).
           MOVE WK-PD-FEEAMT(2) TO LTI-INSFEE(2).

           MOVE PD-SRCD             TO LTI-TRCD.
           MOVE LN-INSCOMP(INS-SUB) TO LTI-INSCOMP.
           MOVE LN-INSOURS(INS-SUB) TO LTI-INSOURS.
           MOVE LN-INSEFF-DATE(INS-SUB)  TO LTI-INSEFF-DATE.
           MOVE LN-INSEXP-DATE(INS-SUB)  TO LTI-INSEXP-DATE.
           MOVE LN-INSCOVR(INS-SUB) TO LTI-INSCOVR.
           MOVE WK-PD-PROCEEDS      TO LTI-INSPREM.

           MOVE WK-PD-REGPYAMT TO LTI-INS-PAYMNT-INCR.
           MOVE WK-PD-LNAMT    TO LTI-INS-TOT-ADDON.
           MOVE 0              TO LTI-INT-LAST-EARNED.

      *  ADJUSTMENT COMMISSION IS PRORATED TO ORIGINAL COMMISSION
           COMPUTE LTI-INSCOMM ROUNDED = WK-PD-PROCEEDS *
                        LN-INSCOMM(INS-SUB) / LN-INSPREM(INS-SUB).

      * NOTE:
      *  NO UPFRONT EARNINGS ARE CALCULATED AND TAKEN:
           MOVE PD-ANTICTERM TO LTI-ANTICTERM.

           PERFORM UPDATE-LT-INS-CLEAR-ANTIC VARYING
              SUB FROM 1 BY 1 UNTIL SUB > 3.

           MOVE WK-PD-ANTICERN1 TO LTI-ANTICERN(1).
           MOVE WK-PD-FINCHG    TO LTI-ANTICERN(3).
           MOVE PD-INTDATE      TO LTI-ANTIC-START-DATE.

           PERFORM WRITE-LTI1-FILE.
           IF IO-FG = 0
              GO TO UPDATE-LT-INS-EXIT.

           IF SAVE-LT-SEQ < 999
              ADD 1 TO SAVE-LT-SEQ
              GO TO UPDATE-LT-INS-NEXT-SEQ.

           MOVE "TRAILER TRANSACTION = 999, REC NOT CREATED !" TO MESS.
           PERFORM SEND-MESS.

           GO TO UPDATE-LT-INS-EXIT.

       UPDATE-LT-INS-CLEAR-ANTIC.
           MOVE 0 TO LTI-ANTICERN(SUB) LTI-ANTICADJ(SUB)
                     LTI-UFERN(SUB)    LTI-ACCERN(SUB).

       UPDATE-LT-INS-EXIT.
           PERFORM CLOSE-LTI1-FILE.

      *****************************************
       UPDATE-LT-SCHLD SECTION.
      *****************************************
           MOVE "UPDATING PAYMENT SCHEDULE TRAILER FILE..." TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-LTP1-FILE.

       UPDATE-LT-SCHLD-READ.
           MOVE PD-OWNBR  TO LTP-BRNO.
           MOVE PD-ACCTNO TO LTP-ACCTNO.
           MOVE 1         TO LTP-SEQNO.
           PERFORM READ-LTP1-FILE.
           IF IO-FG = 0
              IF LN-PAY-SCHLD-FG = "Y"
                 GO TO UPDATE-LT-SCHLD-CONT
              ELSE
                 MOVE "ERROR: PAYMENT SCHEDULE EXISTS (NO FLAG)" TO MESS
                 PERFORM SEND-MESS
                 GO TO UPDATE-LT-SCHLD-CONT
           ELSE
              IF LN-PAY-SCHLD-FG = "Y"
                 MOVE "ERROR: NO PAYMENT SCHEDULE (FLAG SET)" TO MESS
                 PERFORM SEND-MESS.

           PERFORM CLEAR-LTP-FILE.

           MOVE PD-OWNBR  TO LTP-BRNO.
           MOVE PD-ACCTNO TO LTP-ACCTNO.
           MOVE 1         TO LTP-SEQNO.

      * SET 1STPYAMT:
           MOVE 1           TO LTP-SCHLD-TERM(1).
           MOVE LN-1STPYAMT TO LTP-SCHLD-PAYMNT(1).
           MOVE 2 TO SCHD-SUB.

      * SET REGPYAMT:
           IF LN-NOREGPY NOT = 0
              MOVE LN-NOREGPY  TO LTP-SCHLD-TERM(SCHD-SUB)
              MOVE LN-REGPYAMT TO LTP-SCHLD-PAYMNT(SCHD-SUB)
              ADD 1 TO SCHD-SUB.

      * SET LASTPYAMT:
           IF LN-LASTPYAMT NOT = 0
              MOVE 1            TO LTP-SCHLD-TERM(SCHD-SUB)
              MOVE LN-LASTPYAMT TO LTP-SCHLD-PAYMNT(SCHD-SUB).

           PERFORM WRITE-LTP1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           MOVE "Y" TO LN-PAY-SCHLD-FG.

           GO TO UPDATE-LT-SCHLD-READ.

       UPDATE-LT-SCHLD-CONT.
           MOVE LOW-VALUES TO SAVE-PMT-LXA-TABLE.
           PERFORM UNPACK-PAYMENT-SCHEDULE.

           PERFORM GET-1ST-PAYMNT-INDEX.
           IF SCHD-SUB < 1 OR SCHD-SUB > 360
              MOVE "ERROR UPDATING PAYMENT SCHEDULE" TO MESS
              PERFORM SEND-MESS
              GO TO UPDATE-LT-SCHLD-EXIT.

           IF PD-SRCD = "NP" OR "LP"
              PERFORM SCHDUP-NP-LP-UPDATE
           ELSE
              IF PD-SRCD = "RT"
                 PERFORM SCHDUP-RT-UPDATE
              ELSE
                 IF PD-SRCD = "ET"
                    PERFORM SCHDUP-ET-UPDATE
                 ELSE
                    IF PD-SRCD = "ST"
                       PERFORM SCHDUP-ST-UPDATE.

           PERFORM PACK-PAYMENT-SCHEDULE.
           PERFORM REWRITE-LTP1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

       UPDATE-LT-SCHLD-EXIT.
           PERFORM CLOSE-LTP1-FILE.

      **************************************************
       GET-1ST-PAYMNT-INDEX SECTION.
      **************************************************

      * DETERMINE 1STPAYMENT INDEX TO UNPACKED TABLE

           MOVE LN-1STPYDATE TO NUM-DATE.
           MOVE PD-1STPYDATE TO SYS-DATE.
           PERFORM TIM360.

           COMPUTE SCHD-SUB = ELAPSED-MONTHS + 1
                         - LN-TOTNODEF - LN-TOTNOALDEL.

      **************************************************
       SCHDUP-NP-LP-UPDATE SECTION.
      **************************************************

      * NEXT PAYMENT (NP), PAYMENT SCHEDULE UPDATE:
      * LAST PAYMENT (LP)

           MOVE SCHD-SUB TO SAVE-START-PMT(1) SAVE-END-PMT(1).
           MOVE WK-PD-REGPYAMT TO SAVE-NET-CHANGE(1).
           ADD WK-PD-REGPYAMT TO SCHD-AMT(SCHD-SUB).

      **************************************************
       SCHDUP-RT-UPDATE SECTION.
      **************************************************

      * REMAINING TERM (RT), PAYMENT SCHEDULE UPDATE:

           MOVE SCHD-SUB TO ASCHD-SUB BSCHD-SUB.
           PERFORM ASCHD-AMOUNT-DUE.
           COMPUTE BSCHD-NEW-AMTDUE = ASCHD-AMTDUE + WK-PD-LNAMT.
           IF PD-LOANTYPE = "I"
              ADD WK-PD-FINCHG TO BSCHD-NEW-AMTDUE.
           ADD WK-PD-REGPYAMT TO PD-SUM-REGPYAMT GIVING BSCHD-REGPYAMT.
           MOVE PD-ORGTERM TO BSCHD-TERM.

           PERFORM BSCHD-BUILD-SCHEDULE.

       SCHDUP-RT-UPDATE-EXIT.
           EXIT.

      **************************************************
       SCHDUP-ST-UPDATE SECTION.
      **************************************************

      * SCHEDULE TERM (ST), PAYMENT SCHEDULE UPDATE:
      *
      * DO NOT USE WK-PD- FIELDS HERE.
      * SCHEDULE WILL BE MULTIPLIED BY +1/-1 AFTER IT IS BUILT.

           PERFORM VARYING ST-SCHD-SUB2 FROM 1 BY 1
                                   UNTIL ST-SCHD-SUB2 > 360
              MOVE 0 TO ST-SCHD-AMT(ST-SCHD-SUB2)
           END-PERFORM.

           MOVE SCHD-SUB TO ASCHD-SUB BSCHD-SUB.
           PERFORM ASCHD-AMOUNT-DUE.
           MOVE PD-LNAMT TO BSCHD-NEW-AMTDUE.
           IF PD-LOANTYPE = "I"
              ADD PD-FINCHG TO BSCHD-NEW-AMTDUE.
           MOVE PD-REGPYAMT TO  BSCHD-REGPYAMT.
           MOVE PD-ORGTERM TO BSCHD-TERM.

           PERFORM ST-BSCHD-BUILD-SCHEDULE.

       SCHDUP-ST-UPDATE-EXIT.
           EXIT.

      **************************************************
       SCHDUP-ET-UPDATE SECTION.
      **************************************************

      * EXTEND TERM (ET), PAYMENT SCHEDULE UPDATE:

           SUBTRACT 1 FROM SCHD-SUB GIVING ASCHD-SUB BSCHD-SUB.
           PERFORM ASCHD-AMOUNT-DUE.
           COMPUTE BSCHD-NEW-AMTDUE = ASCHD-AMTDUE + WK-PD-LNAMT.
           IF PD-LOANTYPE = "I"
                 ADD WK-PD-FINCHG TO BSCHD-NEW-AMTDUE.
           MOVE PD-SUM-REGPYAMT TO BSCHD-REGPYAMT.
           MOVE 360 TO BSCHD-TERM.

           PERFORM BSCHD-BUILD-SCHEDULE.

       SCHDUP-ET-UPDATE-EXIT.
           EXIT.

      *****************************************
       UPDATE-LP1-FILE SECTION.
      *****************************************
           MOVE "UPDATING LOAN PAYMENT FILE.............." TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM CLEAR-LP-FILE.
       SETUP-LP-SEQ.
           PERFORM OPEN-LP1-FILE.

           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE PD-ACCTNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE ALL "9"       TO LP-SEQNO
                                 QLP1-WEND-SEQNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.

           PERFORM START-LP1-FILE-NOT-GREATER.
           PERFORM READ-LP1-FILE-PREVIOUS.
           PERFORM CLOSE-LP1-FILE.
           IF IO-FG NOT = 0 OR (LP-PATH-OWNBR NOT = LP-BRNO) OR
                                (PD-ACCTNO NOT = LP-ACCTNO)
              MOVE LP-PATH-OWNBR TO LP-BRNO
              MOVE PD-ACCTNO     TO LP-ACCTNO
              MOVE 0             TO LP-SEQNO.

           PERFORM OPEN-LP1-FILE.
       NEXT-AVAIL-SEQ.
           ADD 1 TO LP-SEQNO.
           MOVE LP1-KEY TO SAVE-LP1-KEY.
           PERFORM READ-LP1-FILE.
           IF IO-FG = 0
      *       UNLOCK LP-FILE
              GO TO NEXT-AVAIL-SEQ.

           PERFORM CLEAR-LP-FILE.
           MOVE SAVE-LP1-KEY TO LP1-KEY.

           MOVE PD-SRCD      TO LP-TRCD.
           MOVE "ADJ"        TO LP-REFNO LPWSLP-LP-REFNO.
           IF INS-SUB = 1
              MOVE "CL"      TO LPWSLP-LP-ADDON-TYPE
           ELSE
           IF INS-SUB = 2
              MOVE "AH"      TO LPWSLP-LP-ADDON-TYPE
           ELSE
           IF INS-SUB = 3
              MOVE "PP"      TO LPWSLP-LP-ADDON-TYPE
           ELSE
           IF INS-SUB = 4
              MOVE "O1"      TO LPWSLP-LP-ADDON-TYPE
           ELSE
           IF INS-SUB = 5
              MOVE "O2"      TO LPWSLP-LP-ADDON-TYPE
           ELSE
           IF INS-SUB = 6
              MOVE "O3"      TO LPWSLP-LP-ADDON-TYPE.

           MOVE LPWSLP-LP-REFNO TO LP-REFNO.

           MOVE TRANS-DATE      TO LP-TRDATE.
           MOVE PD-LOANDATE     TO LP-PAYDATE.
           MOVE PD-SSNO(1)      TO LP-SSNO.

           IF PD-LOANTYPE = "P"
              MOVE WK-PD-LNAMT TO LP-TRAMT
              SUBTRACT WK-PD-FINCHG FROM WK-PD-LNAMT GIVING LP-APCUR
           ELSE
              ADD WK-PD-LNAMT WK-PD-FINCHG GIVING LP-TRAMT
              MOVE WK-PD-LNAMT TO LP-APCUR.

           MOVE WK-PD-FINCHG TO LP-APINT.

           MOVE SAVE-IBPC-FG TO LP-IBPC.

           MOVE SAVE-PDTH-DATE TO LP-PDTH-DATE.

      *    -------------------------------------------------------------
      *    FIGURES FROM ORIGINAL LOAN:
      *    -------------------------------------------------------------
           MOVE LN-LCPDTH-DATE TO LP-LCPDTH-DATE.
           MOVE LN-OTHBAL TO LP-OTHBAL.
           MOVE LN-OT2BAL TO LP-OT2BAL.
           MOVE LN-INTBAL TO LP-INTBAL.
           MOVE LN-LCBAL TO LP-LCBAL.
           COMPUTE LP-CURBAL = LN-CURBAL + WK-PD-LNAMT.

           MOVE 0 TO LP-APOTH LP-APOT2 LP-APLC LP-DLPROC
                     LP-EARNED(1) LP-EARNED(2) LP-EARNED(3)
                     LP-INTPAID LP-NEWSTAT-RATE LP-TILLNO.

           MOVE SAVE-LT-SEQ TO LP-LT-SEQNO.
           ADD LN-SUM-REGPYAMT WK-PD-REGPYAMT GIVING LP-LCPARTIALS.
           MOVE PD-LASTPYAMT TO LP-LCPAID.

           IF SAVE-IBPC-FG = "I"
              ADD SAVE-INDU-INTEREST TO LP-INTBAL
              MOVE SAVE-INDU-INTEREST TO LP-INTDUE
              MOVE SAVE-LN-INTPDTH-DATE TO LP-ADDON-INTPDTH-DATE
           ELSE
              MOVE 0 TO LP-ADDON-INTPDTH-DATE LP-INTDUE.

           MOVE LN-STATUSFG TO LP-STATUSFG.
           MOVE PD-INSTYPES TO LP-INSTYPES.

           PERFORM SET-POSTDATE.
           MOVE POST-USERID TO LP-USERID.
           MOVE POST-TIME TO LP-POSTTIME.
           MOVE POST-DATE TO LP-POSTDATE.
           MOVE LN-COLLECTOR TO LP-COLLECTOR.

           PERFORM WRITE-LP1-FILE.
           PERFORM CLOSE-LP1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

      *****************************************
       UPDATE-LX1-FILE SECTION.
      *****************************************
           MOVE "UPDATING LOAN PAYMENT TRAILER FILE......" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM INITIALIZE-WLXA-WORKER.
           PERFORM OPEN-LX1-FILE.
           MOVE SAVE-PMT-SCHED-CHNG(1) TO WLXA-PMT-SCHED-CHNG(1).
           MOVE SAVE-PMT-SCHED-CHNG(2) TO WLXA-PMT-SCHED-CHNG(2).
           MOVE SAVE-PMT-SCHED-CHNG(3) TO WLXA-PMT-SCHED-CHNG(3).
           MOVE SAVE-PMT-SCHED-CHNG(4) TO WLXA-PMT-SCHED-CHNG(4).
           MOVE SAVE-PMT-SCHED-CHNG(5) TO WLXA-PMT-SCHED-CHNG(5).
           MOVE SAVE-PMT-SCHED-CHNG(6) TO WLXA-PMT-SCHED-CHNG(6).
           MOVE SAVE-PMT-SCHED-CHNG(7) TO WLXA-PMT-SCHED-CHNG(7).
           MOVE SAVE-PMT-SCHED-CHNG(8) TO WLXA-PMT-SCHED-CHNG(8).
           MOVE SAVE-PMT-SCHED-CHNG(9) TO WLXA-PMT-SCHED-CHNG(9).

      *    -------------------------------------------------------------
      *    LXA-INSTBL WILL HOLD THE LAST CURRENT INSURANCE FROM LN-REC
      *    BEFORE THIS ADDON.  NEEDED FOR REVERSALS !
      *    -------------------------------------------------------------
           IF INS-SUB = 1
              MOVE LN-CL-REBATE TO WLXA-LN-INS-REBATE
           ELSE
           IF INS-SUB = 2
              MOVE LN-AH-REBATE TO WLXA-LN-INS-REBATE
           ELSE
           IF INS-SUB = 3
              MOVE LN-PP-REBATE TO WLXA-LN-INS-REBATE
           ELSE
           IF INS-SUB = 4
              MOVE LN-O1-REBATE TO WLXA-LN-INS-REBATE
           ELSE
           IF INS-SUB = 5
              MOVE LN-O2-REBATE TO WLXA-LN-INS-REBATE
           ELSE
           IF INS-SUB = 6
              MOVE LN-O2-REBATE TO WLXA-LN-INS-REBATE.

           MOVE LN-INSCOMP(INS-SUB) TO WLXA-LN-INSCOMP.
           MOVE LN-INSOURS(INS-SUB) TO WLXA-LN-INSOURS.
           MOVE LN-INSEFF-DATE(INS-SUB) TO WLXA-LN-INSEFF-DATE.
           MOVE LN-INSEXP-DATE(INS-SUB) TO WLXA-LN-INSEXP-DATE.
           MOVE LN-INSPREM(INS-SUB) TO WLXA-LN-INSPREM.
           MOVE LN-INSCOVR(INS-SUB) TO WLXA-LN-INSCOVR.
           MOVE LN-INSCOMM(INS-SUB) TO WLXA-LN-INSCOMM.

           PERFORM SAVE-WLXA-WORKER.
           PERFORM CLOSE-LX1-FILE.

      *****************************************
       UPDATE-TP-GLNO SECTION.
      *****************************************

           MOVE 1 TO SUB.
       NEXT-TP-GLNO.
           IF PD-FEECODE(SUB) NOT = SPACES
              MOVE BRANCH          TO CDB-BRANCH GLNO-BR
              MOVE PD-FEECODE(SUB) TO CDB-CODE
              PERFORM GET-CODE
              IF IO-FG = 0
                 IF LC-USE-ALT-GL-FEE = "Y"
                    MOVE CDB-GC-ALT-GLFEENO TO GLNO-NUMBER
                 ELSE
                    MOVE CDB-GC-GLFEENO TO GLNO-NUMBER
                 END-IF
                 MOVE GLNO TO TP-GLNO(SUB)
                 MOVE WK-PD-FEEAMT(SUB) TO TP-GLAMT(SUB).
            ADD 1 TO SUB.
            IF SUB < 3
               GO TO NEXT-TP-GLNO.

      *********************************************************
       ADJUST-INTEREST SECTION.
      *********************************************************

      * ADJUST INTEREST FROM EFFECTIVE DATE OF ADJUSTMENT

           MOVE "ESTABLISHING ADJUSTED INTEREST DUE......" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE LN-CURBAL TO HOLD-LN-CURBAL.
           MOVE LN-INTPDTH-DATE TO HOLD-LN-INTPDTH-DATE.
           MOVE 0 TO ADJ-INT-DUE.

       ADJUST-INTEREST-CANCEL.

      * GET INTEREST FROM LAST ASSESSMENT THRU TODAY
      * ON NEW BALANCE FOR ADJUSTMENT:

           MOVE TRANS-DATE TO RATE-DATE.
           MOVE LN-MAKERCD(1) TO RATE-MAKERCD.
           MOVE "XX" TO RATE-LPTRCD.
           PERFORM SET-PY-RATE.
           ADD INDU-INTEREST TO ADJ-INT-DUE.
           GO TO ADJUST-INTEREST-EXIT.

      *WE SHOULD ALWAYS ASSESS INTEREST THRU THE TRANSACTION DATE AS
      *WE ARE PERFORMING A TRANSACTION THAT WILL ALTER THE BALANCE. THIS
      *CAME ABOUT BECAUSE WE WERE ASSESSING INTEREST ON AN ADDON BACKDATED

       ADJUST-INTEREST-EXIT.
           MOVE HOLD-LN-CURBAL       TO LN-CURBAL.
           MOVE HOLD-LN-INTPDTH-DATE TO LN-INTPDTH-DATE.

      *********************************************************
       ASCHD-AMOUNT-DUE SECTION.
      *********************************************************

      * GET REMAINING SCHEDULED AMOUNT DUE
           MOVE 0 TO ASCHD-AMTDUE.
           IF ASCHD-SUB < 1
              GO TO ASCHD-AMOUNT-DUE-EXIT.

           PERFORM ASCHD-AMOUNT-DUE-TALLY
              UNTIL ASCHD-SUB > 360 OR
                    SCHD-AMT(ASCHD-SUB) = 0.
           GO TO ASCHD-AMOUNT-DUE-EXIT.

       ASCHD-AMOUNT-DUE-TALLY.
           ADD SCHD-AMT(ASCHD-SUB) TO ASCHD-AMTDUE.
           ADD 1 TO ASCHD-SUB.

       ASCHD-AMOUNT-DUE-EXIT.
           EXIT.

      *********************************************************
       BSCHD-BUILD-SCHEDULE SECTION.
      *********************************************************

      * BUILD NEW PAYMENT SCHEDULE AND SET LX NET CHANGES
           IF BSCHD-SUB < 1 OR BSCHD-TERM < 1
              GO TO BSCHD-BUILD-SCHEDULE-EXIT.

           MOVE 1 TO SCHD-SUB.
           MOVE " " TO BSCHD-FG.
           PERFORM BSCHD-BUILD-SCHEDULE-SET
              UNTIL BSCHD-SUB > 360
                    OR (BSCHD-NEW-AMTDUE = 0
                          AND SCHD-AMT(BSCHD-SUB) = 0).

           IF BSCHD-FG = "E"
              MOVE "PROBLEM BUILDING PMT SCHD LX REV FILE" TO MESS
              PERFORM SEND-MESS.
       BSCHD-BUILD-SCHEDULE-EXIT.
           EXIT.

      *********************************************************
       BSCHD-BUILD-SCHEDULE-SET SECTION.
      *********************************************************
           MOVE SCHD-AMT(BSCHD-SUB) TO BSCHD-AMT.
           IF BSCHD-REGPYAMT > BSCHD-NEW-AMTDUE
            OR BSCHD-TERM = 1
              MOVE BSCHD-NEW-AMTDUE TO SCHD-AMT(BSCHD-SUB)
              MOVE 0 TO BSCHD-NEW-AMTDUE
           ELSE
              MOVE BSCHD-REGPYAMT TO SCHD-AMT(BSCHD-SUB)
              SUBTRACT BSCHD-REGPYAMT FROM BSCHD-NEW-AMTDUE.

           SUBTRACT BSCHD-AMT FROM SCHD-AMT(BSCHD-SUB)
                                     GIVING BSCHD-NET-CHANGE.
           IF BSCHD-NET-CHANGE = 0
              GO TO BSCHD-BUILD-SCHEDULE-SET-CONT.

       BSCHD-BUILD-SCHEDULE-SET-AGAIN.
           IF SAVE-START-PMT(SCHD-SUB) = 0
              MOVE BSCHD-SUB TO SAVE-START-PMT(SCHD-SUB)
                                SAVE-END-PMT(SCHD-SUB)
              SUBTRACT BSCHD-AMT FROM SCHD-AMT(BSCHD-SUB)
                                 GIVING SAVE-NET-CHANGE(SCHD-SUB)
              GO TO BSCHD-BUILD-SCHEDULE-SET-CONT.

           IF BSCHD-NET-CHANGE = SAVE-NET-CHANGE(SCHD-SUB)
              SUBTRACT 1 FROM BSCHD-SUB GIVING SUB
              IF SUB = SAVE-END-PMT(SCHD-SUB)
                 MOVE BSCHD-SUB TO SAVE-END-PMT(SCHD-SUB)
                 GO TO BSCHD-BUILD-SCHEDULE-SET-CONT.

           IF SCHD-SUB < 9
              ADD 1 TO SCHD-SUB
              GO TO BSCHD-BUILD-SCHEDULE-SET-AGAIN
           ELSE
              MOVE "E" TO BSCHD-FG
              GO TO BSCHD-BUILD-SCHEDULE-SET-AGAIN.

       BSCHD-BUILD-SCHEDULE-SET-CONT.
           ADD 1 TO BSCHD-SUB.
           SUBTRACT 1 FROM BSCHD-TERM.

      *********************************************************
      *    (ST-BSCHD)
      *    BUILD NEW PAYMENT SCHEDULE AND SET LX NET CHANGES
      *    FOR "ST" SCHEDULED TERM ADDONS.
      *
      *     EXAMPLE: NEW AMTDUE =  PD-LNAMT = $60.00.
      *              TERM = PD-ORGTERM = 3
      *
      *         SCHEDULE WOULD LOOK LIKE:
      *              (1)   $25.00
      *              (2)   $25.00
      *              (3)   $10.00
      *
      *         THIS WOULD THEN BE ADDED INTO THE LT PAYMENT SCHEDULE:
      *          LT SCHD-AMT    + ST-SCHD-AMT() =  LT SCHD-AMT   NET CHANGE
      *          -------------    -------------    -----------   ----------
      *     .... (10)  $150.50     (10)  $0.00
      *          (11)  $150.50  +  (11) $25.00  =  (11) $175.50   $25.00
      *          (12)  $150.50  +  (12) $25.00  =  (12) $175.50   $25.00
      *          (13)  $150.50  +  (13) $10.00  =  (13) $160.50   $10.00
      *          (14)  $150.50     (14)  $0.00  .....
      *
      *          THE LXA RECORD WOULD CONTAIN THE FOLLOWING:
      *                  START  END  NET CHANGE
      *                  -----  ---  ----------
      *              (1)   11    12      $25.00
      *              (2)   13    13      $10.00
      *              (3)    0     0       $0.00
      *********************************************************
       ST-BSCHD-BUILD-SCHEDULE SECTION.
           IF BSCHD-SUB < 1 OR BSCHD-TERM < 1
              GO TO ST-BSCHD-BUILD-SCHEDULE-EXIT.

           MOVE BSCHD-SUB TO ST-SCHD-SUB.
           MOVE 1 TO SCHD-SUB.
           MOVE " " TO BSCHD-FG.
           PERFORM ST-BSCHD-BUILD-SCHEDULE-SET
              UNTIL BSCHD-SUB > 360
                    OR (BSCHD-NEW-AMTDUE = 0
                          AND ST-SCHD-AMT(BSCHD-SUB) = 0).

           IF BSCHD-FG = "E"
              MOVE "PROBLEM BUILDING PMT SCHD LX REV FILE" TO MESS
              PERFORM SEND-MESS.

      *    -------------------------------------------------------------
      *    ONE =  1 IF ADJUSTMENT IS POSITIVE.
      *    ONE = -1 IF ADJUSTMENT IS NEGATIVE.
      *    -------------------------------------------------------------
           PERFORM VARYING ST-SCHD-SUB2 FROM 1 BY 1
                                   UNTIL ST-SCHD-SUB2 > 360
              IF ST-SCHD-SUB2 <  10
                 MULTIPLY ONE BY SAVE-NET-CHANGE(ST-SCHD-SUB2)
              END-IF
              MULTIPLY ONE BY ST-SCHD-AMT(ST-SCHD-SUB2)
           END-PERFORM.

      *  ADD ST-SCHD-AMT() TO SCHD-AMT().
           PERFORM VARYING ST-SCHD-SUB2 FROM ST-SCHD-SUB BY 1
                   UNTIL ST-SCHD-SUB2 > (ST-SCHD-SUB + PD-ORGTERM)
               ADD ST-SCHD-AMT(ST-SCHD-SUB2) TO SCHD-AMT(ST-SCHD-SUB2)
           END-PERFORM.

       ST-BSCHD-BUILD-SCHEDULE-EXIT.
           EXIT.

      *********************************************************
       ST-BSCHD-BUILD-SCHEDULE-SET SECTION.
      *********************************************************
           MOVE ST-SCHD-AMT(BSCHD-SUB) TO BSCHD-AMT.
           IF BSCHD-REGPYAMT > BSCHD-NEW-AMTDUE
            OR BSCHD-TERM = 1
              MOVE BSCHD-NEW-AMTDUE TO ST-SCHD-AMT(BSCHD-SUB)
              MOVE 0 TO BSCHD-NEW-AMTDUE
           ELSE
              MOVE BSCHD-REGPYAMT TO ST-SCHD-AMT(BSCHD-SUB)
              SUBTRACT BSCHD-REGPYAMT FROM BSCHD-NEW-AMTDUE.

           MOVE ST-SCHD-AMT(BSCHD-SUB) TO BSCHD-NET-CHANGE.

           IF BSCHD-NET-CHANGE = 0
              GO TO ST-BSCHD-BUILD-SCHD-SET-CONT.

       ST-BSCHD-BUILD-SCHD-SET-AGAIN.
           IF SAVE-START-PMT(SCHD-SUB) = 0
              MOVE BSCHD-SUB TO SAVE-START-PMT(SCHD-SUB)
                                SAVE-END-PMT(SCHD-SUB)
      *  NET CHANGE EQUALS AMOUNT OF ADDON PAYMENT
              MOVE ST-SCHD-AMT(BSCHD-SUB) TO SAVE-NET-CHANGE(SCHD-SUB)
              GO TO ST-BSCHD-BUILD-SCHD-SET-CONT.

           IF BSCHD-NET-CHANGE = SAVE-NET-CHANGE(SCHD-SUB)
              SUBTRACT 1 FROM BSCHD-SUB GIVING SUB
              IF SUB = SAVE-END-PMT(SCHD-SUB)
                 MOVE BSCHD-SUB TO SAVE-END-PMT(SCHD-SUB)
                 GO TO ST-BSCHD-BUILD-SCHD-SET-CONT.

           IF SCHD-SUB < 9
              ADD 1 TO SCHD-SUB
              GO TO ST-BSCHD-BUILD-SCHD-SET-AGAIN
           ELSE
              MOVE "E" TO BSCHD-FG.

       ST-BSCHD-BUILD-SCHD-SET-CONT.
           ADD 1 TO BSCHD-SUB.
           SUBTRACT 1 FROM BSCHD-TERM.

      ******************************************************************
       GET-CON-REC-PRIOR-LNUPD SECTION.
      ******************************************************************

      * GET CONTRACTUAL, RECENCY, PAST DUE & SAVE-OUTBAL
      * FOR UPDATE OF TY & DS PRIOR TO LOAN UPDATE
           MOVE TRANS-DATE TO AGEING-DATE.
           PERFORM COMPUTE-CONTRACTUAL.
           PERFORM COMPUTE-RECENCY.

           MOVE CSUB TO PRIOR-CSUB.
           MOVE RDSUB TO PRIOR-RDSUB.

      * SETUP OUTBAL WORKER FOR UPDATE OF STATS
           IF GP-DELINQ-WITH-O2 = "Y"
              MOVE LN-CURBAL TO PRIOR-SAVE-OUTBAL
           ELSE
              SUBTRACT LN-OT2BAL FROM LN-CURBAL
                         GIVING PRIOR-SAVE-OUTBAL.


      *******************************************
       UPDATE-TY1-PRIOR SECTION.
      *******************************************

      * UPDATE TODAYS TOTALS FOR PRIOR TO LOAN UPDATE
           IF (NOT LN-IS-REPO-AR)
              SUBTRACT 1 FROM TY-UNITS(1 PRIOR-RDSUB)
                              TY-UNITS(2 PRIOR-CSUB)
              SUBTRACT PRIOR-SAVE-OUTBAL
                         FROM TY-OUTBAL(1 PRIOR-RDSUB)
                              TY-OUTBAL(2 PRIOR-CSUB).

       UPDATE-TY1-PRIOR-EXIT.
           EXIT.

      *********************************************************
       UPDATE-DS1-PRIOR SECTION.
      *********************************************************

      * UPDATE DEALER STATISTICS FOR TO LOAN UPDATE

           IF (NOT LN-IS-REPO-AR)
              SUBTRACT 1 FROM DS-UNITS(DSNOW DSREC PRIOR-RDSUB)
                              DS-UNITS(DSNOW DSCON PRIOR-CSUB)
              SUBTRACT PRIOR-SAVE-OUTBAL
                         FROM DS-OUTBAL(DSNOW DSREC PRIOR-RDSUB)
                              DS-OUTBAL(DSNOW DSCON PRIOR-CSUB).

       UPDATE-DS1-PRIOR-EXIT.
           EXIT.

      ******************************************************
       GET-CON-REC-AFTER-LNUPD SECTION.
      ******************************************************

      * GET CONTRACTUAL, RECENCY, PAST DUE & SAVE-OUTBAL
      * FOR UPDATE OF TY & DS AFTER LOAN UPDATE

      * SETUP OUTBAL WORKER FOR UPDATE OF STATS
           IF GP-DELINQ-WITH-O2 = "Y"
              MOVE LN-CURBAL TO AFTER-SAVE-OUTBAL
           ELSE
              SUBTRACT LN-OT2BAL FROM LN-CURBAL
                         GIVING AFTER-SAVE-OUTBAL.

           MOVE TRANS-DATE TO AGEING-DATE.
           PERFORM COMPUTE-CONTRACTUAL.
           PERFORM COMPUTE-RECENCY.

           MOVE CSUB TO AFTER-CSUB.
           MOVE RDSUB TO AFTER-RDSUB.

      *******************************************
       UPDATE-TY1-AFTER SECTION.
      *******************************************

      * UPDATE TODAYS TOTALS FOR AFTER LOAN UPDATE

           MOVE "UPDATING TODAY'S DELINQ TOTAL FILE........" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM GET-TY1-FILE.

           PERFORM POST-TY1-AFTER-UPDATE.

      * UPDATE UNEARNED STATISTICS NET AND VOLUME
           IF GP-UNEARNED-STATS = "Y"
              IF ONE = +1
                 ADD PD-FINCHG TO TY-UNEARNED-AMT(1 2)
                                  TY-UNEARNED-AMT(2 2)
              ELSE
                 SUBTRACT PD-FINCHG FROM TY-UNEARNED-AMT(1 2)
                                         TY-UNEARNED-AMT(2 2).

           PERFORM REWRITE-TY1-FILE.
           PERFORM CLOSE-TY1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

       UPDATE-TY1-AFTER-EXIT.
           EXIT.

      ********************************************
       POST-TY1-AFTER-UPDATE SECTION.
      ********************************************
           IF (NOT LN-IS-REPO-AR)
              ADD 1 TO TY-UNITS(1 AFTER-RDSUB)
                       TY-UNITS(2 AFTER-CSUB)
              ADD AFTER-SAVE-OUTBAL TO TY-OUTBAL(1 AFTER-RDSUB)
                                       TY-OUTBAL(2 AFTER-CSUB).

      *******************************************
       UPDATE-DS1-AFTER SECTION.
      *******************************************

      * UPDATE DEALER DELINQUENCY FOR AFTER LOAN UPDATE

           MOVE "UPDATING DSFILE DELINQ TOTALS............."
                                                        TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM GET-DS1-FILE.

           PERFORM POST-DS1-AFTER-UPDATE.

      * UPDATE UNEARNED STATISTICS NET AND VOLUME
           IF GP-UNEARNED-STATS = "Y"
              IF ONE = +1
                 ADD PD-FINCHG TO DS-UNEARNED-AMT(1 1 2)
                                  DS-UNEARNED-AMT(1 2 2)
                                  DS-UNEARNED-AMT(2 2 2)
              ELSE
                 SUBTRACT PD-FINCHG FROM DS-UNEARNED-AMT(1 1 2)
                                         DS-UNEARNED-AMT(1 2 2)
                                         DS-UNEARNED-AMT(2 2 2).

           PERFORM REWRITE-DS1-FILE.
           PERFORM CLOSE-DS1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

       UPDATE-DS1-AFTER-EXIT.
           EXIT.

      ********************************************
       POST-DS1-AFTER-UPDATE SECTION.
      ********************************************
           IF (NOT LN-IS-REPO-AR)
              ADD 1 TO DS-UNITS(DSNOW DSREC AFTER-RDSUB)
                       DS-UNITS(DSNOW DSCON AFTER-CSUB)
              ADD AFTER-SAVE-OUTBAL
                    TO DS-OUTBAL(DSNOW DSREC AFTER-RDSUB)
                       DS-OUTBAL(DSNOW DSCON AFTER-CSUB).

      ********************************************
       GET-TY1-FILE SECTION.
      ********************************************
           PERFORM OPEN-TY1-FILE.
           MOVE "N" TO REC-EXISTS.
       GET-TY1-FILE-CHECK-EXISTS.
           PERFORM SET-TY-KEY.
           PERFORM READ-TY1-FILE.
           IF IO-FG = 0
              GO TO GET-TY1-FILE-EXIT.
           IF REC-EXISTS = "Y"
              GO TO IO-ERROR.

           MOVE "Y" TO REC-EXISTS.
           PERFORM CLEAR-TY-FILE.
           PERFORM SET-TY-KEY.
           PERFORM WRITE-TY1-FILE.
           IF IO-FG = 9 AND E-CODE NOT = "22"
              GO TO IO-ERROR.
           GO TO GET-TY1-FILE-CHECK-EXISTS.

       GET-TY1-FILE-EXIT.
           EXIT.

      ********************************************
       GET-DS1-FILE SECTION.
      ********************************************
           PERFORM OPEN-DS1-FILE.
           MOVE "N" TO REC-EXISTS.
       GET-DS1-FILE-CHECK-EXISTS.
           PERFORM SET-DS-KEY.
           PERFORM READ-DS1-FILE.
           IF IO-FG = 0
              GO TO GET-DS1-FILE-EXIT.
           IF REC-EXISTS = "Y"
              GO TO IO-ERROR.

           MOVE "Y" TO REC-EXISTS.
           MOVE " " TO DS-REC.
           PERFORM CLEAR-DS-FILE.
           PERFORM SET-DS-KEY.
           PERFORM WRITE-DS1-FILE.
           IF IO-FG = 9 AND E-CODE NOT = "22"
              GO TO IO-ERROR.
           GO TO GET-DS1-FILE-CHECK-EXISTS.

       GET-DS1-FILE-EXIT.
           EXIT.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPXDTE.CPY".
       COPY "LIBLP/LPIBPC.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBLP/LPSCHD.CPY".
       COPY "LIBLP/LPRATE.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBLP/POSTDATE.CPY".
       COPY "LIBLP/LPSRCD.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBLP/LOADLXA.CPY".
       COPY "LIBLP/SETCTBL.CPY".


      ***************************************************
       SETUP-LINK-INFO SECTION.
      ***************************************************
           MOVE LONPG-HELP--FILE TO HELP-LINK-FILE.
           MOVE LONPG-HELP--DIR TO HELP-LINK-DIR.

      ***************************************************
       VDU-CONVERT-FIELD SECTION.
      ***************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LONPG_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *************************************
       MISCELLANEOUS-ROUTINES SECTION.
      *************************************
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/ACCESS.CPY".
       FORM-RESET.
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       SET-DS-KEY.
           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM TO DS-DATE-CCYYMM.
           MOVE LN-OWNBR TO DS-BRANCH.
           MOVE LN-DLNO  TO DS-DLNO.
           MOVE LN-CLASS TO DS-CLASS.
       SET-TY-KEY.
           MOVE LN-OWNBR   TO TY-BRNO.
           MOVE TRANS-DATE TO TY-DATE.
           MOVE LN-CLASS   TO TY-CLASS.
       GET-CODE.
           MOVE "GC"   TO CDB-TYPE.
      *--> SET BEFORE     CDB-BRANCH.
      *--> SET BEFORE     CDB-CODE.
           PERFORM OPEN-CDB1-FILE.
           PERFORM READ-CDB1-FILE.
           PERFORM CLOSE-CDB1-FILE.

      /*******************************************
      *     I N P U T  /  O U T P U T            *
      ********************************************
       COPY "LIBLP/LPLTICL.CPY".
       COPY "LIBLP/LPTYCL.CPY".
       COPY "LIBLP/LPLPCL.CPY".
       COPY "LIBLP/LPDSCL.CPY".
       COPY "LIBLP/LPTRPCL.CPY".
       COPY "LIBLP/LPLTPCL.CPY".
       COPY "LIBLP/LPLXCL.CPY".

      **********************************************
       IO-ROUTINES SECTION.
      **********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTYGS_SQL.CPY".
       COPY "LIBLP/LPTRPGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLXGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPNOGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPDSGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPNO1IN.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPLP1IN.CPY".
       COPY "LIBLP/LPLX1IN.CPY".
       COPY "LIBLP/LPLTI1IN.CPY".
       COPY "LIBLP/LPLTP1IN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPBW1IN.CPY".
       COPY "LIBLP/LPTY1IN.CPY".
       COPY "LIBLP/LPDS1IN.CPY".
       COPY "LIBLP/LPWKI.CPY".
       COPY "LIBLP/LPTRP1IN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

       END-ROUTINE.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.

