       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LONPFF.
       DATE-WRITTEN. 081690.
      ******************************************************************
      *  DESCRIPTION:  ADDON INSURANCE REVERSAL UPDATE
      * REV:
      *  061495 MJD SET UP TP-USERID AND TP-POSTTIME
      *  062895 MJD ADDED COPY MEMBERS LIBLP/POSTDATE AND POSTDATEW.
      *             SET TP-POSTDATE.
      *  071995 JTG ADDED LOGIC FOR LN-PRPDUE-DAYS & LN-PRLNAMT
      *  072195 JTG ADDED LPSRCD LOGIC
      *  081195 MJD ADDED GP-FILE INFO FOR LIBLP/LPSRCD :
      *                   GP-THE-ONLY-SF-SRCD.
      *  MJD 091895 ADDED DS-DATE LOGIC.
      *  MJD 021396 ADDED UPDATE OF PL TABLE.
      *  MJD 071796 MADE CHANGES FOR 6 DIGIT PDTH DATE.
      *  MJD 071896 ADDED TP-UNITPER-CD AND TP-UNITPER-FREQ
      *  JTG 111996 CHANGED TO UPDATE LN-NONACRERN WITH AMOUNT EARNED
      *             TODATE ON REVERSED CANCELLED PREMIUM
      *  JTG 021197 CHANGED TO SET LP-REVTRCD LP-REVSEQNO
      *                            TP-REVTRCD TP-REVSEQNO
      *  MJD 050297 ADDED LOADLXA LOGIC (ADDON TRAILER RECORD 2).
      *  MJD ADDED LOGIC TO DETERMINE THE NEED FOR DELETING THE
      *      PAYMENT SCHEDULE RECORD.
      *  MJD 050697 ADDED LIBLP/SETCTBL(W).
      *  MJD 060497 ADDED UPDATE OF LN-O3-REBATE IF INS-SUB = 6.
      *  GLF 090897 REPLACED 'IO' ON GP-FILE WITH GET 'GPENV'
      *  MJD 091797 REMOVED DELSUB-SET LOGIC; ADDED LPPDUE(W) COPY MEMBERS
      *  MJD 101797 MOVED WK-UPDATE-BUF TO LP01WK
      *  BAH 111197 ADDED HOLD-SP1-KEY,LC-GIFTNET TO CL/LEDGER LINKAGE CALL
      *  JTG 120597 CORRECTED RESET OF LN-INSTYPES WHEN O1 ADDON RV
      *  MJD 052198 REMOVED XY-PASTDUE.
      *  JTG 061598 ADDED LOGIC TO UPDATE BANKRUPT DELINQUENT STATISTICS
      *  BAH 062298 MOVE LT-ACCERN(1) TO TP-EARNINGS(2) SO LPEOD5 WILL
      *             UPDATE ADDON INTEREST INSTEAD OF INTEREST, CASTLE PL# 186
      *  BAH 062598 CHANGED TP-TILL PIC 9 TO TP-TILLNO PIC X
      *  JTG 063098 EXPANDED DS-APP-TYPE-TBL TO 3 LEVELS
      *  MJD 990420 CHANGED TO INCLUDE/EXCLUDE O2 IN DELINQ BASED
      *             ON GP-DELINQ-WITH-O2
      *  JTG 991011 CHANGED SETUP-LINE FOR 'AP' DISPLAY REVOLVING, #47 REGENCY
      *             AND CHANGED CALL TO CL/LEDGER.C TO PASS LC-REC
      *  JTG 991110 ALLOWED ACCOUNTS TO HAVE ADDON'S WHEN IN REPO STATUS,
      *             CHANGED UPD OF DSFILE, TYFILE & LPEOD5TR, NEWPORT #244, A15K
      *  MJD 000328 ADDED LOGIC TO SKIP UPDATE OF VOLUME IF REGACC
      *             (REGACC PR#1106)
      *  JTG 000328 MODIFIED TO UPDATE UNEARNED STATISTICS, MERCURY #223
      *  MJD 001016 ADDED UPDATE OF TP-COLLECTOR/LP-COLLECTOR (BAY PR#1317)
      *  BAH 011109 SET TP-INSCOMP, REGACC PR# 1617
      *  JTG 020517 CORRECTED SET OF SIGN ON TP-FEEAMT FIELDS WORLD #244
      *  BAH 020722 SET TP-INSOURS
      *  BAH 030425 ADDED LC-USE-ALT-GL-FEE AND CD-GC-ALT-GLFEENO FOR
      *             LENDMARK PR# 2048
      *  JTG 041216 THE REVERSAL OF ADDON POSTINGS WILL NOW PASS LN-REPOCD
      *             TO TP-REPOCD RE: UPDATE OF GL IN LPEOD5TR FOR
      *             DETERMINING UPDATE WITH GI-REPO-AR         NORTH STATE #0456
      * BAH 140814 ADDED TEST FOR LPSMNU IN HELP
      *  CS 151111 LP-ADDON-INTPDTH-DATE WAS REMOVED IN A30 AND A NEW FIELD
      *            LP-INT-PDTH, IN THE CASE OF ADDONS, ITS THE PRIOR INTEREST
      *            PAID THRU DATE. MAY STILL NEED THE SEPARATE FIELD FOR
      *            ADDON INT-PDTH AS ITS ONLY UDPATED FOR "IB"
      *  CS 160112 ADD MOVE OF LN-OWNBR TO LP-BRNO
      *  BAH 160125 SPLIT OUT LTFILE, PD0003
      *  BLM 160310 FIX FOR NEW TY DATE FORMAT, PD0003
      *
      * KEC 2016.0315 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         CHANGED CD-FEE???? TO CD-GC-????.
      * BLM 160513 FIX FOR NEW DS-DATE FORMAT, FIX DS-UNEARNED, PD0003
      * KEC 2016.1026 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED USING %%%%-TR1-KEY.
      *         ADDED LIBLP/LPWSTR (NEW) FOR LPWSTR-TR1-KEY (HOLD/SAVE).
      * KEC 2017.0105 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED DS-DATE TO DS-DATE-CCYYMM FOR CONSISTENCY
      *          WITH THE DATE FIELDS IN BY & UD FILE.
      *
      * KEC 2017.0509 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          REMOVED THE LOGIC OF ACCESSING LX-REC FOR
      *          LX-CODE = "2"; "2" WAS MERGED INTO "A" IN A30.
      *
      * BAH 181231 GLOBAL LPFILE
      *  CS 190103 GLOBAL LTIFILE
      * BAH 190116 GLOBAL LXFILE
      * BAH 20190308 CHANGED LP-ADDON TO USE LPWSLP-LP-ADDON, MOVED LP-REFNO
      *              REDEFINITIONS TO LPWSLP COPY MEMBER
      * BLM 20210810 REMOVE TP-CODE, TP-POOLID NO LONGER IN KEY
      * BAH 2022.0314 REMOVED EXT-SPANISH-FORMS AND ALL MX FORMS
      * BAH 20220422 UPDATE TP-ACTIONCD2, SECURITIZATION #1551
      * BAH 20220803 HARD CODED START
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
           "@(#)./LP/LONPFF.CBL S34 08/11/21-10:54:31 >".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LP01NO_SQL.CPY".
       COPY "LIBLP/LPWSNO.CPY".
       COPY "LIBLP/LP01LX.CPY".
       COPY "LIBLP/LP01LX_SQL.CPY".
       COPY "LIBLP/LPWSLX.CPY".
       COPY "LIBLP/LP01LXE.CPY".
       COPY "LIBLP/LP01LXE_SQL.CPY".
       COPY "LIBLP/LPWSLXE.CPY".
       COPY "LIBLP/LP01LXG.CPY".
       COPY "LIBLP/LP01LXG_SQL.CPY".
       COPY "LIBLP/LPWSLXG.CPY".
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
      *-----------------------------------------------------------------
       COPY "LIBLP/LP01TRP.CPY".
       COPY "LIBLP/LP01TRP_SQL.CPY".
       COPY "LIBLP/LPWSTRP.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
      *-----------------------------------------------------------------
       01  WK-IFN              PIC X(2)    VALUE "WK".

       01  WK-KEY              PIC 99.

       01  MSEL-LIST           PIC X(5)     VALUE "MSEL.".
       01  HOLD-TP-LINE        PIC 9(3).

      * HOLD-LP1-KEY
       01  SAVE-LP1-KEY          PIC X(17)    VALUE SPACES.

       01  ORIG-LN-REPOCD            PIC X        VALUE " ".
           88  ORIG-LN-IS-REPO-AR     VALUE "X" "S".
           88  ORIG-LN-REPO-ONHAND    VALUE "X".
           88  ORIG-LN-REPO-REDEEMED  VALUE "V".
           88  ORIG-LN-REPO-SOLD      VALUE "S".
           88  ORIG-LN-IS-NOT-REPO    VALUE " " "V".
       01  REPO-AR-SUB               PIC S9(5) COMP.

       01  GLNO                 PIC 9(11)     VALUE 0.
       01  GLNOZ REDEFINES GLNO.
           03  GLNO-BR          PIC 9(4).
           03  GLNO-NUMBER      PIC 9(7).
       01  HOLD-LP-REVTRCD      PIC XX.
       01  HOLD-LP-REVSEQNO     PIC 9(5)     COMP   VALUE 0.
       01  WORKER2              PIC S9(7)V99 COMP   VALUE 0.
      * LXA SAVE FIELDS:
       01  SAVE-PMT-LXA-TABLE.
           03  SAVE-PMT-SCHED-CHNG OCCURS 9 TIMES.
               05  SAVE-START-PMT  PIC 999      COMP-3.
               05  SAVE-END-PMT    PIC 999      COMP-3.
               05  SAVE-NET-CHANGE PIC S9(6)V99 COMP.

       01  ELE                 PIC 99        COMP  VALUE 0.
       01  ONE                 PIC S9        COMP.
       01  SUB                 PIC S9(5)     COMP  VALUE +0.
       01  INS-SUB             PIC 99        COMP  VALUE 0.
       01  REC-EXISTS          PIC X         VALUE SPACES.

       01  PRIOR-RDSUB          PIC 99          COMP VALUE 0.
       01  PRIOR-CSUB           PIC 99          COMP VALUE 0.
       01  PRIOR-SAVE-OUTBAL      PIC S9(7)V99    COMP VALUE 0.

       01  AFTER-RDSUB         PIC 99          COMP VALUE 0.
       01  AFTER-CSUB          PIC 99          COMP VALUE 0.
       01  AFTER-SAVE-OUTBAL     PIC S9(7)V99    COMP VALUE 0.


       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBCL/CRNOR2W.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPMENUW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBLP/LPSCHDW.CPY".
       COPY "LIBLP/LPSETTYW.CPY".
       COPY "LIBLP/LPDSCLW.CPY".
       COPY "LIBLP/POSTDATEW.CPY".
       COPY "LIBLP/LPSRCDW.CPY".
       COPY "LIBLP/LOADLXAW.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LONPF_DEF.CPY".
       COPY "LIBLP/LPSMNU_DEF.CPY".
       COPY "LIBLP/LPLNMU_DEF.CPY".
       COPY "LIBLP/LPCAMU_DEF.CPY".
       COPY "LIBLP/LPVDMU_DEF.CPY".

       COPY "LIBLP/LONPF_WKS.CPY".
       COPY "LIBLP/LPSMNU_WKS.CPY".
       COPY "LIBLP/LPLNMU_WKS.CPY".
       COPY "LIBLP/LPCAMU_WKS.CPY".
       COPY "LIBLP/LPVDMU_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBLP/LPPFBUF.CPY".

       SCREEN SECTION.
       COPY "LIBLP/LONPF_SCN.CPY".
       COPY "LIBLP/LPSMNU_SCN.CPY".
       COPY "LIBLP/LPLNMU_SCN.CPY".
       COPY "LIBLP/LPCAMU_SCN.CPY".
       COPY "LIBLP/LPVDMU_SCN.CPY".

      /*************************************************
      *   C O N T R O L   M O D U L E                  *
      **************************************************
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
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-NO1-FILE.
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
           MOVE "LONPF" TO VDU-FORM-NAME.

           MOVE USER-PID TO TEMP-PID.

           MOVE 0 TO IO-FG.
           MOVE SPACE TO MESS ERRCD.
           PERFORM RESTORE-SCREEN.

           PERFORM OPEN-LN1-FILE.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           IF LN-REC NOT = WK-SAVE-REC
              MOVE "LOAN HAS BEEN CHANGED, RE-INPUT CHANGES" TO MESS
              PERFORM SEND-MESS
              GO TO INVALID-EXIT.

      *---------------------------------------------
      *    GET REVERSED PAYMENT 'TRCD' AND 'SEQNO'
      *---------------------------------------------
           PERFORM OPEN-LP1-FILE.
           PERFORM READ-LP1-FILE.
           PERFORM CLOSE-LP1-FILE.
           IF IO-FG NOT = 0 OR LP-REV = "Y"
              MOVE "PAYMENT REVERSAL ERROR" TO MESS
              PERFORM SEND-MESS
              GO TO IO-ERROR.

           MOVE LP-TRCD   TO HOLD-LP-REVTRCD.
           MOVE LP-SEQNO  TO HOLD-LP-REVSEQNO.

           MOVE WK-LP-REC TO LP-REC.

      *-------------------------------------------
           MOVE HOLD-SP1-KEY TO SP1-KEY.
           PERFORM OPEN-SP1-FILE.
           PERFORM READ-SP1-FILE.
           PERFORM CLOSE-SP1-FILE.
           IF IO-FG NOT = 0
              MOVE " INVALID STATE PARAMETER RECORD" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.

      *-------------------------------------------
           MOVE BRANCH TO BR-NO.
           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID BRANCH RECORD" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO ERRCD
              GO TO END-ROUTINE.

      *-------------------------------------------
           PERFORM OPEN-LC1-FILE.
           MOVE LN-CLASS TO LC-CODE.
           PERFORM READ-LC1-FILE.
           PERFORM CLOSE-LC1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

      *-------------------------------------------
      * GET GLOBAL PARAMETER RECORD (FROM GPENV):
           PERFORM GET-GPENV.

      *------------------------------------------------------
      * ESTABLISH ORIGINAL REPO STATUS
           MOVE LN-REPOCD TO ORIG-LN-REPOCD.
           IF ORIG-LN-REPO-ONHAND
              MOVE 1 TO REPO-AR-SUB
           ELSE
              MOVE 3 TO REPO-AR-SUB.

      *------------------------------------------------------
      * GET DELINQUENCY PRIOR TO LOAN UPDATE:
           PERFORM GET-CON-REC-PRIOR-LNUPD.

      * GET LOAN SOURCE CODE INDEX:
           PERFORM GET-SRCD-SUB.

           MOVE LP-REFNO TO LPWSLP-LP-REFNO.
      * SETUP SUBSCRIPT FROM THE VERY START !!
           IF LPWSLP-LP-ADDON-TYPE = "CL"
              MOVE 1 TO INS-SUB
           ELSE
           IF LPWSLP-LP-ADDON-TYPE = "AH"
              MOVE 2 TO INS-SUB
           ELSE
           IF LPWSLP-LP-ADDON-TYPE = "PP"
              MOVE 3 TO INS-SUB
           ELSE
           IF LPWSLP-LP-ADDON-TYPE = "O1"
              MOVE 4 TO INS-SUB
           ELSE
           IF LPWSLP-LP-ADDON-TYPE = "O2"
              MOVE 5 TO INS-SUB
           ELSE
           IF LPWSLP-LP-ADDON-TYPE = "O3"
              MOVE 6 TO INS-SUB
           ELSE
      * FEE ONLY
              MOVE 0 TO INS-SUB.

      *-------------------------------------------
      *    GET THE PAYMENT TRAILER RECORD WITH
      *    THE SEQUENCE FROM REVERSED PAYMENT
      *-------------------------------------------
           PERFORM OPEN-LX1-FILE.
           PERFORM GET-LXA-FILE.
           IF WLXA-NO-RECORDS-FOUND
              MOVE "INVALID PAYMENT TRAILER" TO MESS
              PERFORM SEND-MESS
              GO TO INVALID-EXIT.

           PERFORM OPEN-LTI1-FILE.
           MOVE LTI-PATH-OWNBR TO LTI-BRNO.
           MOVE LP-ACCTNO      TO LTI-ACCTNO.
           MOVE LP-LT-SEQNO    TO LTI-SEQNO.
           PERFORM READ-LTI1-FILE.
           IF IO-FG NOT = 0
              MOVE "INVALID INSURANCE TRAILER" TO MESS
              PERFORM SEND-MESS
              GO TO INVALID-EXIT.
           PERFORM CLOSE-LTI1-FILE.

      *-------------------------------------------
           MOVE LP-INSTYPES TO LN-INSTYPES.
           PERFORM UPDATE-BW1-FILE.

           PERFORM UPDATE-DS1-FILE.

           PERFORM UPDATE-TY1-FILE.

      *    PERFORM UPDATE-DT1-FILE.

           PERFORM UPDATE-TR1-FILE.

           PERFORM UPDATE-LN1-FILE.

           PERFORM DELETE-LT-INS.

           PERFORM UPDATE-LT-SCHLD.

           MOVE LP-REFNO TO LPWSLP-LP-REFNO.
           IF LPWSLP-LP-ADDON-TEXT = "CNL"
              PERFORM UPDATE-LT-CANCELS.

           PERFORM REVERSE-LP1-FILE.
           MOVE WK-LP-REC TO LP-REC.

           PERFORM UPDATE-LP1-FILE.

           PERFORM DELETE-LX1-TRAILER.

           IF LN-PLCD = " " AND ORIG-LN-IS-NOT-REPO
              PERFORM GET-CON-REC-AFTER-LNUPD
              PERFORM UPDATE-DS1-AFTER
              PERFORM UPDATE-TY1-AFTER.

           PERFORM UPDATE-NOTICE-FILE.

       INVALID-EXIT.
           MOVE MENU-PROG-FORM(4) TO NEW-PROG-FORM.
           MOVE "P" TO ERRCD.

       LOAN-UPDATE-EXIT.
           PERFORM SAVE-SCREEN.
           GO TO END-ROUTINE.


      **********************************************
       UPDATE-BW1-FILE SECTION.
      **********************************************
           MOVE "UPDATING BORROWER FILE.............." TO MESS.
           PERFORM SEND-LEGEND.

           MOVE 1 TO SUB.
           PERFORM OPEN-BW1-FILE.
       UPDATE-NEXT-BW.
           MOVE LN-SSNO(SUB) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.
           IF LN-SERVACCT = "Y"
              SUBTRACT LTI-INS-TOT-ADDON FROM BW-LIAOTH
           ELSE
              IF LN-SMALLOAN = "Y"
                 SUBTRACT LTI-INS-TOT-ADDON FROM BW-LIASMLN
              ELSE
                 SUBTRACT LTI-INS-TOT-ADDON FROM BW-LIAOTH.

           PERFORM REWRITE-BW1-FILE.
           IF SUB < 4
              ADD 1 TO SUB
              IF LN-SSNO(SUB) NOT = 0
                 GO TO UPDATE-NEXT-BW.

       EXIT-UPDATE-BW1.
           PERFORM CLOSE-BW1-FILE.

      **********************************************
       UPDATE-DS1-FILE SECTION.
      **********************************************
           MOVE "UPDATING DEALER STATISTICS FILE........." TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM GET-DS1-FILE.

           IF ORIG-LN-IS-REPO-AR
              SUBTRACT LTI-INS-TOT-ADDON
                       FROM DS-AMT-REPO(DSCUR DSNET REPO-AR-SUB)
           ELSE
              SUBTRACT LTI-INS-TOT-ADDON FROM DS-OUTSAMT(1).

      *   IF REVERSE OF FULL CANCEL, UPDATE NEW VOLUME:

           MOVE LP-REFNO TO LPWSLP-LP-REFNO.

           IF ( ( LPWSLP-LP-ADDON-TEXT = "CNL"     ) AND
                ( INS-SUB NOT = 0                  ) AND
                ( LTI-INSPREM = LTI-INS-TOT-ADDON - LTI-ANTICERN(1) ) )
               OR ( ( LPWSLP-LP-ADDON-TEXT NOT = "CNL" ) )
      *       IF NOT GP-I-AM-REGACC
                IF LN-IS-REPO-AR
                   SUBTRACT LTI-INS-TOT-ADDON
                              FROM DS-AMT-REPO(DSMTD DSVOL REPO-AR-SUB)
                                   DS-AMT-REPO(DSYTD DSVOL REPO-AR-SUB)
                ELSE
                   IF LN-SRCD = "PB" OR "RN" OR "SC" OR "RB" OR "RO"
                      SUBTRACT LTI-INS-TOT-ADDON FROM DS-RNEWAMT(1)
                                                     DS-RNEWAMT(2)
                   ELSE
                   IF LN-SALEFIN = "Y" AND GP-THE-ONLY-SF-SRCD NOT = " "
                      IF LN-SRCD = GP-THE-ONLY-SF-SRCD
                         SUBTRACT LTI-INS-TOT-ADDON FROM DS-NPURAMT(1)
                                                        DS-NPURAMT(2)
                      END-IF
                   ELSE
                   IF LN-SRCD = "NB"
                      SUBTRACT LTI-INS-TOT-ADDON FROM DS-NPURAMT(1)
                                                     DS-NPURAMT(2)
                   ELSE
                      SUBTRACT LTI-INS-TOT-ADDON FROM DS-OPURAMT(1)
                                                     DS-OPURAMT(2).

           IF LN-SERVACCT NOT = "Y"
              IF LN-DLRECOURSE = "F"
                 SUBTRACT LTI-INS-TOT-ADDON FROM DS-RCOURSE-AMT(1)
               ELSE
                 IF LN-DLRECOURSE = "P"
                    SUBTRACT LTI-INS-TOT-ADDON FROM DS-RCOURSE-AMT(2)
                  ELSE
                    IF LN-DLRECOURSE = "N"
                       SUBTRACT LTI-INS-TOT-ADDON FROM DS-RCOURSE-AMT(3).
      * UPDATE DELINQUENCY:
           PERFORM UPDATE-DS1-PRIOR.

           PERFORM REWRITE-DS1-FILE.
           PERFORM CLOSE-DS1-FILE.

      ********************************************************
       UPDATE-TY1-FILE SECTION.
      ********************************************************
           MOVE "UPDATING TODAY'S TOTAL FILE............." TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM GET-TY1-FILE.

           IF SRCD-NOT-PL
              IF ORIG-LN-IS-REPO-AR
                 SUBTRACT LTI-INS-TOT-ADDON
                                       FROM TY-AMT-REPO(1 REPO-AR-SUB)
                 SUBTRACT LTI-ANTICERN(1)
                                       FROM TY-INT-REPO(1 REPO-AR-SUB)
              ELSE
                 SUBTRACT LTI-INS-TOT-ADDON FROM TY-AMT(1 SRCD)
                 SUBTRACT LTI-ANTICERN(1)   FROM TY-INT(1 SRCD).

      * IF REVERSE OF FULL CANCEL, UPDATE NEW VOLUME:

           MOVE LP-REFNO TO LPWSLP-LP-REFNO.

           IF ((LPWSLP-LP-ADDON-TEXT = "CNL"     ) AND
               (INS-SUB NOT = 0                  ) AND
               (LTI-INSPREM = LTI-INS-TOT-ADDON - LTI-ANTICERN(1) ))
               OR (( LPWSLP-LP-ADDON-TEXT NOT = "CNL" ))
      *       IF NOT GP-I-AM-REGACC
                 IF LN-IS-REPO-AR
                    SUBTRACT LTI-INS-TOT-ADDON
                                   FROM TY-AMT-REPO(2 REPO-AR-SUB)
                    SUBTRACT LTI-ANTICERN(1) FROM
                                        TY-INT-REPO(2 REPO-AR-SUB)
                 ELSE
                    SUBTRACT LTI-INS-TOT-ADDON FROM TY-AMT(2 SRCD)
                    SUBTRACT LTI-ANTICERN(1) FROM TY-INT(2 SRCD)
                 END-IF
      * UPDATE INSURANCES WROTE:
                 MOVE LTI-INS-TOT-ADDON TO WORKER2
                 IF LN-LOANTYPE = "P"
                    SUBTRACT LTI-ANTICERN(1) FROM WORKER2
                 END-IF
                 IF WORKER2 < 0
                    MOVE -1 TO ONE
                 ELSE
                    MOVE +1 TO ONE
                 END-IF
                 IF INS-SUB = 1 OR 2 OR 3 OR 4 OR 5 OR 6
                    SUBTRACT ONE FROM TY-INS-NO-WROTE(INS-SUB)
                    SUBTRACT WORKER2 FROM TY-INS-AMT-WROTE(INS-SUB).

      * UPDATE DELINQUENCY:
           PERFORM UPDATE-TY1-PRIOR.

       UPDATE-TY-CONTINUE.
           PERFORM REWRITE-TY1-FILE.
           PERFORM CLOSE-TY1-FILE.

      ********************************************************
       UPDATE-TR1-FILE SECTION.
      ********************************************************
           MOVE "UPDATING DAILY TRANSACTION FILE........." TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-TRP1-FILE.
           MOVE 0 TO HOLD-TP-LINE.

           MOVE BRANCH     TO TP-BRNO.
           MOVE TRANS-DATE TO TP-DATE.
      *    MOVE "P"        TO TP-CODE.
      *    MOVE 0          TO TP-POOLID.
           MOVE LN-CLASS   TO TP-CLASS.
           MOVE LN-ACCTNO  TO TP-ACCTNO.

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

           MOVE BRANCH     TO TP-POSTING-BR.
           MOVE LN-LAWCODE TO TP-LAWCODE.
           MOVE LP-SSNO    TO TP-SSNO.
           MOVE "M"        TO TP-MAKER.
           MOVE LP-IBPC    TO TP-IBPC.
           MOVE LTI-TRCD   TO TP-TRCD.
           IF LP-TRCD = "RV"
              MOVE "Y"     TO TP-REV.

           MOVE LP-REFNO     TO TP-REFNO LPWSLP-LP-ADDON.
           MOVE LPWSLP-LP-ADDON-TEXT TO TP-ADDON-TXT.
           MOVE LPWSLP-LP-ADDON-TYPE TO TP-ADDON-TYP.
           MOVE LP-PAYDATE   TO TP-PAYDATE.
           MOVE LP-PDTH-DATE TO TP-DUEDATE.

           ADD LP-TRAMT TO TP-TRAMT.

           ADD LP-APINT TO TP-APINT.
           ADD LP-APCUR TO TP-APCUR.

           ADD LP-OTHBAL TO TP-OTHBAL.
           ADD LP-OT2BAL TO TP-OT2BAL.
           ADD LP-INTBAL TO TP-INTBAL.
           ADD LP-LCBAL  TO TP-LCBAL.
           ADD LP-CURBAL TO TP-CURBAL.
           COMPUTE TP-INSCOMM = LTI-INSCOMM * -1.
           MOVE LTI-INSCOMP TO TP-INSCOMP.
           MOVE LTI-INSOURS TO TP-INSOURS.

      * TP-WORKAMT(2) = ADDON INTEREST
           MOVE LTI-ANTICERN(1) TO TP-WORKAMT(2).

      * TP-EARNINGS(2) = ADDON INTEREST
           COMPUTE TP-EARNINGS(2) = LTI-ACCERN(1) * -1.

           PERFORM UPDATE-TP-GLNO.

           MOVE LP-PDTH-DATE TO TP-PDTH-DATE.
           MOVE LP-TILLNO TO TP-TILLNO.
           MOVE LP-LCPDTH-DATE TO TP-LCPDTH-DATE.
           MOVE LN-DLNO TO TP-DLNO.
           MOVE LP-DLPROC TO TP-DLPROC.
           MOVE LP-INTPAID TO TP-INTPAID.

           MOVE LP-NEWSTAT-RATE TO TP-NEWSTAT-RATE.

           MOVE LN-SERVACCT TO TP-SERVACCT.
           MOVE LN-SALEFIN TO TP-SALEFIN.
           MOVE LN-LOANTYPE TO TP-LOANTYPE.
           MOVE LN-INSURED TO TP-INSURED.
      * ORIGINAL, BEFORE THE CANCEL, SO GL KNOWS WHAT ACCOUNT TO POST
           MOVE LN-INSTYPES TO TP-INSTYPES.

           PERFORM SET-POSTDATE.
           MOVE POST-USERID TO TP-USERID.
           MOVE POST-TIME TO TP-POSTTIME.
           MOVE POST-DATE TO TP-POSTDATE.
           MOVE LN-COLLECTOR TO TP-COLLECTOR.

           MOVE HOLD-LP-REVTRCD  TO TP-REVTRCD.
           MOVE HOLD-LP-REVSEQNO TO TP-REVSEQNO.

           MOVE ORIG-LN-REPOCD TO TP-REPOCD.

           MOVE LN-UNITPER-CD   TO TP-UNITPER-CD.
           MOVE LN-UNITPER-FREQ TO TP-UNITPER-FREQ.
           MOVE LN-ACTIONCD2    TO TP-ACTIONCD2.

           PERFORM WRITE-TRP1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           PERFORM CLOSE-TRP1-FILE.

      ********************************************
       UPDATE-LN1-FILE SECTION.
      ********************************************
           MOVE "UPDATING LOAN MASTER FILE..............." TO MESS.
           PERFORM SEND-LEGEND.

           IF LTI-SEQNO = 1
              MOVE " " TO LN-ADDON-FG.

           IF LP-IBPC = "I"
              ADD LP-INTDUE TO LN-INTBAL
              MOVE LP-INTPDTH-DATE TO LN-INTPDTH-DATE.

      * UPDATE LOAN BALANCES:
           SUBTRACT LTI-INS-TOT-ADDON FROM LN-CURBAL.
           SUBTRACT LTI-INS-TOT-ADDON FROM LN-PURBAL.

      * UPDATE ADDON INSURANCE:
           IF INS-SUB NOT = 0
              MOVE WLXA-LN-INSCOMP     TO LN-INSCOMP(INS-SUB)
              MOVE WLXA-LN-INSOURS     TO LN-INSOURS(INS-SUB)
              MOVE WLXA-LN-INSEFF-DATE TO LN-INSEFF-DATE(INS-SUB)
              MOVE WLXA-LN-INSEXP-DATE TO LN-INSEXP-DATE(INS-SUB)
              MOVE WLXA-LN-INSCOVR     TO LN-INSCOVR(INS-SUB)
              MOVE WLXA-LN-INSPREM     TO LN-INSPREM(INS-SUB)
              MOVE WLXA-LN-INSCOMM     TO LN-INSCOMM(INS-SUB).

           IF INS-SUB = 1
              MOVE WLXA-LN-INS-REBATE TO LN-CL-REBATE
           ELSE
           IF INS-SUB = 2
              MOVE WLXA-LN-INS-REBATE TO LN-AH-REBATE
           ELSE
           IF INS-SUB = 3
              MOVE WLXA-LN-INS-REBATE TO LN-PP-REBATE
           ELSE
           IF INS-SUB = 4
              MOVE WLXA-LN-INS-REBATE TO LN-O1-REBATE
           ELSE
           IF INS-SUB = 5
              MOVE WLXA-LN-INS-REBATE TO LN-O2-REBATE
           ELSE
           IF INS-SUB = 6
              MOVE WLXA-LN-INS-REBATE TO LN-O3-REBATE.

      * UPDATE INS. CONSOLIDATED INFO:
           IF LTI-TRCD = "RT"
              SUBTRACT LTI-INS-PAYMNT-INCR FROM LN-SUM-REGPYAMT.

           SUBTRACT LTI-INS-TOT-ADDON FROM LN-SUM-LNAMT.
           SUBTRACT LTI-INS-TOT-ADDON FROM LN-SUM-ORIGPURAMT.

      * FINCHG IS STORED IN LP-APINT, WILL BE - ON REVERSAL
           ADD LP-APINT TO LN-SUM-FINCHG.
           ADD LP-APINT TO LN-SUM-INTCHG.

           COMPUTE LN-ANTICERN(2) = LN-ANTICERN(2) - LTI-ANTICERN(1).
           COMPUTE LN-ANTICADJ(2) = LN-ANTICADJ(2) - LTI-ANTICADJ(1).
           COMPUTE LN-UFERN(2)    = LN-UFERN(2)    - LTI-UFERN(1).
           COMPUTE LN-ACCERN(2)   = LN-ACCERN(2)   - LTI-ACCERN(1).

      * UPDATE NON ACCRUAL EARNINGS:
           SUBTRACT LTI-ACCERN(1) FROM LN-NONACRERN.

           MOVE LP-REFNO TO LPWSLP-LP-REFNO.
           IF LPWSLP-LP-ADDON-TEXT = "ADD"
              IF INS-SUB = 1 OR 2 OR 3 OR 4
                 MOVE WLXA-LN-INSCODES TO LN-INSCODES.

           PERFORM REWRITE-LN1-FILE.

      *****************************************
       DELETE-LT-INS SECTION.
      *****************************************
           MOVE "DELETING INSURANCE TRAILER FILE........" TO MESS.
           PERFORM SEND-LEGEND.

      * WE HAVE HAD THE RECORD ALL THE TIME
           PERFORM OPEN-LTI1-FILE.
           PERFORM DELETE-LTI1-FILE.
           PERFORM CLOSE-LTI1-FILE.

      *****************************************
       UPDATE-LT-SCHLD SECTION.
      *****************************************
           MOVE "UPDATING PAYMENT SCHEDULE........." TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-LTP1-FILE.
           MOVE LN-OWNBR  TO LTP-BRNO.
           MOVE LN-ACCTNO TO LTP-ACCTNO.
           MOVE 1         TO LTP-SEQNO.
           PERFORM READ-LTP1-FILE.
           IF IO-FG NOT = 0
              MOVE "ERROR UPDATING PAYMENT SCHEDULE" TO MESS
              PERFORM SEND-MESS
              GO TO UPDATE-LT-SCHLD-EXIT.

           MOVE LOW-VALUES TO SAVE-PMT-LXA-TABLE.
           PERFORM UNPACK-PAYMENT-SCHEDULE.

           PERFORM SCHDUP-UPDATE.

           PERFORM PACK-PAYMENT-SCHEDULE.

           PERFORM UPDATE-LN-SUM-ORGTERM.

      *-----------------------------------------------------------------
      * TEST PAYMENT SCHEDULE AGAINST LOAN:
      *
      *    SCENARIO 1:  LN-FIRSTPYAMT NOT = LN-REGPYAMT, AND
      *                 LN-REGPYAMT NOT = LN-LASTPYAMT, AND
      *                 LN-1STPYAMT NOT = LN-LASTPYAMT
      *
      *    IF PAYMENT SCHEDULE IN TRAILER RECORD IS SAME AS THAT DEPICTED
      *       IN THE LOAN RECORD THEN DELETE TRAILER RECORD, IT IS
      *       NOT NEEDED.
      *    EXAMPLE:
      *           LN-1STPYAMT  = 100.00      SCHEDULE:  (1)   1 @ 100.00
      *           LN-REGPYAMT  = 200.00                 (2)  22 @ 200.00
      *           LN-NOREGPY   = 22                     (3)   1 @ 300.00
      *           LN-LASTPYAMT = 300.00                 (4)   0 @   0.00
      *    -------------------------------------------------------------
           IF (LN-1STPYAMT NOT = LN-REGPYAMT) AND
                 (LN-1STPYAMT NOT = LN-LASTPYAMT) AND
                    (LN-REGPYAMT NOT = LN-LASTPYAMT)
              IF (LTP-SCHLD-TERM(1) NOT = 1 OR
                   LN-1STPYAMT NOT = LTP-SCHLD-PAYMNT(1))
                 GO TO UPDATE-LT-SCHLD-CONT
              ELSE
                 IF (LN-NOREGPY NOT = LTP-SCHLD-TERM(2) OR
                      LN-REGPYAMT NOT = LTP-SCHLD-PAYMNT(2))
                    GO TO UPDATE-LT-SCHLD-CONT
                 ELSE
                    IF (LTP-SCHLD-TERM(3) NOT = 1 OR
                         LN-LASTPYAMT NOT = LTP-SCHLD-PAYMNT(3))
                       GO TO UPDATE-LT-SCHLD-CONT
                    ELSE
                       IF (LTP-SCHLD-TERM(4) NOT = 0 OR
                            LTP-SCHLD-PAYMNT(4) NOT = 0)
                          GO TO UPDATE-LT-SCHLD-CONT
                       ELSE
                           PERFORM DELETE-LTP1-FILE
                           PERFORM REMOVE-LN-FLAG
                           GO TO UPDATE-LT-SCHLD-EXIT.

      *    -------------------------------------------------------------
      *    SCENARIO 2:  LN-FIRSTPYAMT = LN-REGPYAMT, AND
      *                 LN-REGPYAMT NOT = LN-LASTPYAMT, AND
      *                 LN-1STPYAMT NOT = LN-LASTPYAMT
      *
      *    IF PAYMENT SCHEDULE IN TRAILER RECORD IS SAME AS THAT DEPICTED
      *       IN THE LOAN RECORD THEN DELETE TRAILER RECORD, IT IS
      *       NOT NEEDED.
      *    EXAMPLE:
      *           LN-1STPYAMT  = 100.00      SCHEDULE:  (1)  23 @ 100.00
      *           LN-REGPYAMT  = 100.00                 (2)   1 @ 300.00
      *           LN-NOREGPY   = 22                     (3)   0 @   0.00
      *           LN-LASTPYAMT = 300.00
      *    -------------------------------------------------------------
           IF (LN-1STPYAMT = LN-REGPYAMT) AND
                 (LN-1STPYAMT NOT = LN-LASTPYAMT) AND
                    (LN-REGPYAMT NOT = LN-LASTPYAMT)
              IF (LTP-SCHLD-TERM(1) NOT = (LN-NOREGPY + 1) OR
                   LN-REGPYAMT NOT = LTP-SCHLD-PAYMNT(1))
                 GO TO UPDATE-LT-SCHLD-CONT
              ELSE
                 IF (LTP-SCHLD-TERM(2) NOT = 1 OR
                      LN-LASTPYAMT NOT = LTP-SCHLD-PAYMNT(2))
                    GO TO UPDATE-LT-SCHLD-CONT
                 ELSE
                    IF (LTP-SCHLD-TERM(3) NOT = 0 OR
                         LTP-SCHLD-PAYMNT(3) NOT = 0)
                       GO TO UPDATE-LT-SCHLD-CONT
                    ELSE
                        PERFORM DELETE-LTP1-FILE
                        PERFORM REMOVE-LN-FLAG
                        GO TO UPDATE-LT-SCHLD-EXIT.

      *    -------------------------------------------------------------
      *    SCENARIO 3:  LN-FIRSTPYAMT NOT = LN-REGPYAMT, AND
      *                 LN-REGPYAMT = LN-LASTPYAMT, AND
      *                 LN-1STPYAMT NOT = LN-LASTPYAMT.
      *
      *    IF PAYMENT SCHEDULE IN TRAILER RECORD IS SAME AS THAT DEPICTED
      *       IN THE LOAN RECORD THEN DELETE TRAILER RECORD, IT IS
      *       NOT NEEDED.
      *    EXAMPLE:
      *           LN-1STPYAMT  = 100.00      SCHEDULE:  (1)   1 @ 100.00
      *           LN-REGPYAMT  = 200.00                 (2)  23 @ 200.00
      *           LN-NOREGPY   = 22                     (3)   0 @   0.00
      *           LN-LASTPYAMT = 200.00
      *    -------------------------------------------------------------
           IF (LN-1STPYAMT NOT = LN-REGPYAMT) AND
                 (LN-1STPYAMT NOT = LN-LASTPYAMT) AND
                    (LN-REGPYAMT = LN-LASTPYAMT)
              IF (LTP-SCHLD-TERM(1) NOT = 1 OR
                   LN-1STPYAMT NOT = LTP-SCHLD-PAYMNT(1))
                 GO TO UPDATE-LT-SCHLD-CONT
              ELSE
                 IF (LTP-SCHLD-TERM(2) NOT = (LN-NOREGPY + 1) OR
                      LN-LASTPYAMT NOT = LTP-SCHLD-PAYMNT(2))
                    GO TO UPDATE-LT-SCHLD-CONT
                 ELSE
                    IF (LTP-SCHLD-TERM(3) NOT = 0 OR
                         LTP-SCHLD-PAYMNT(3) NOT = 0)
                       GO TO UPDATE-LT-SCHLD-CONT
                    ELSE
                        PERFORM DELETE-LTP1-FILE
                        PERFORM REMOVE-LN-FLAG
                        GO TO UPDATE-LT-SCHLD-EXIT.

      *    -------------------------------------------------------------
      *    SCENARIO 4:  LN-FIRSTPYAMT = LN-REGPYAMT, AND
      *                 LN-REGPYAMT = LN-LASTPYAMT, AND
      *                 LN-1STPYAMT = LN-LASTPYAMT.
      *
      *    IF PAYMENT SCHEDULE IN TRAILER RECORD IS SAME AS THAT DEPICTED
      *       IN THE LOAN RECORD THEN DELETE TRAILER RECORD, IT IS
      *       NOT NEEDED.
      *    EXAMPLE:
      *           LN-1STPYAMT  = 100.00      SCHEDULE:  (1)  24 @ 100.00
      *           LN-REGPYAMT  = 100.00                 (2)   0 @   0.00
      *           LN-NOREGPY   = 22
      *           LN-LASTPYAMT = 100.00
      *    -------------------------------------------------------------
      *    -------------------------------------------------------------
      *    IF LN-1STPYAMT = LN-REGPYAMT = LN-LASTPYAMT
      *    -------------------------------------------------------------
           IF (LN-1STPYAMT  = LN-REGPYAMT) AND
                 (LN-1STPYAMT = LN-LASTPYAMT) AND
                    (LN-REGPYAMT = LN-LASTPYAMT)
              IF (LTP-SCHLD-TERM(1) NOT = (LN-NOREGPY + 2) OR
                   LN-1STPYAMT NOT = LTP-SCHLD-PAYMNT(1))
                 GO TO UPDATE-LT-SCHLD-CONT
              ELSE
                 IF (LTP-SCHLD-TERM(2) NOT = 0 OR
                      LTP-SCHLD-PAYMNT(2) NOT = 0)
                    GO TO UPDATE-LT-SCHLD-CONT
                 ELSE
                    PERFORM DELETE-LTP1-FILE
                    PERFORM REMOVE-LN-FLAG
                    GO TO UPDATE-LT-SCHLD-EXIT.


       UPDATE-LT-SCHLD-CONT.
           PERFORM REWRITE-LTP1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

       UPDATE-LT-SCHLD-EXIT.
           PERFORM CLOSE-LTP1-FILE.

      **************************************************
       SCHDUP-UPDATE SECTION.
      **************************************************
           MOVE 1 TO SUB.
       SCHDUP-NEXT.
           IF WLXA-NET-CHANGE(SUB) NOT = 0
              MOVE WLXA-START-PMT(SUB) TO SCHD-SUB
              PERFORM SCHDUP-SET-AMTS
                          UNTIL SCHD-SUB > WLXA-END-PMT(SUB).
           IF SUB < 9
              ADD 1 TO SUB
              GO TO SCHDUP-NEXT.

           GO TO SCHDUP-UPDATE-EXIT.

       SCHDUP-SET-AMTS.
           SUBTRACT WLXA-NET-CHANGE(SUB) FROM SCHD-AMT(SCHD-SUB).
           ADD 1 TO SCHD-SUB.

       SCHDUP-UPDATE-EXIT.
           EXIT.

      *********************************************************
       UPDATE-LT-CANCELS SECTION.
      *********************************************************

      * REWRITE ALL "SAME TYPE" LT'S WITH CANCEL FLAG
           PERFORM OPEN-LTI1-FILE.

           MOVE LTI-PATH-OWNBR  TO LTI-BRNO
                                   QLTI1-WBEG-BRNO
                                   QLTI1-WEND-BRNO.
           MOVE LN-ACCTNO       TO LTI-ACCTNO
                                   QLTI1-WBEG-ACCTNO
                                   QLTI1-WEND-ACCTNO.
           MOVE 0               TO LTI-SEQNO
                                   QLTI1-WBEG-SEQNO.
           MOVE ALL "9"         TO QLTI1-WEND-SEQNO.

           PERFORM START-LTI1-FILE.
       UPDATE-LT1-FILE-NEXT.
           PERFORM READ-LTI1-FILE-NEXT.
           IF IO-FG NOT = 0 OR (LTI-BRNO NOT = LTI-PATH-OWNBR) OR
                                (LTI-ACCTNO NOT = LN-ACCTNO)
              GO TO END-UPDATE-LT1-FILE.

           IF ( INS-SUB = LTI-INS-TYPE          ) AND
              ( LTI-INSEFF-DATE = LN-INSEFF-DATE(INS-SUB) ) AND
              ( LTI-INSEXP-DATE = LN-INSEXP-DATE(INS-SUB) )
              MOVE " " TO LTI-INS-REBATE
              GO TO UPDATE-LT1-FILE-REWRITE.

      *    UNLOCK LTI-FILE.
           GO TO UPDATE-LT1-FILE-NEXT.

       UPDATE-LT1-FILE-REWRITE.
           PERFORM REWRITE-LTI1-FILE.
           GO TO UPDATE-LT1-FILE-NEXT.

       END-UPDATE-LT1-FILE.
           PERFORM CLOSE-LTI1-FILE.

      *****************************************
       REVERSE-LP1-FILE SECTION.
      *****************************************
           MOVE "REVERSING LOAN PAYMENT FILE.............." TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-LP1-FILE.
           PERFORM READ-LP1-FILE.
           IF IO-FG NOT = 0 OR LP-REV = "Y"
              MOVE "PAYMENT REVERSAL ERROR" TO MESS
              PERFORM SEND-MESS
              GO TO IO-ERROR.

           MOVE "Y" TO LP-REV.

           PERFORM REWRITE-LP1-FILE.
           PERFORM CLOSE-LP1-FILE.


      *****************************************
       UPDATE-LP1-FILE SECTION.
      *****************************************
           MOVE "UPDATING LOAN PAYMENT FILE.............." TO MESS.
           PERFORM SEND-LEGEND.

       SETUP-LP-SEQ.
           MOVE LP-REC TO WK-LP-REC.

           PERFORM OPEN-LP1-FILE.

           MOVE LP-PATH-OWNBR TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE ALL "9"       TO LP-SEQNO
                                 QLP1-WEND-SEQNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.

           PERFORM START-LP1-FILE-NOT-GREATER.
           PERFORM READ-LP1-FILE-PREVIOUS.
           PERFORM CLOSE-LP1-FILE.
           IF IO-FG NOT = 0 OR (LP-PATH-OWNBR NOT = LP-BRNO) OR
                                (LN-ACCTNO NOT = LP-ACCTNO)
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

           MOVE WK-LP-REC TO LP-REC.
           MOVE SAVE-LP1-KEY TO LP1-KEY.

           PERFORM SET-POSTDATE.
           MOVE POST-USERID TO LP-USERID.
           MOVE POST-TIME TO LP-POSTTIME.
           MOVE POST-DATE TO LP-POSTDATE.
           MOVE LN-COLLECTOR TO LP-COLLECTOR.

           MOVE HOLD-LP-REVTRCD TO LP-REVTRCD.
           MOVE HOLD-LP-REVSEQNO TO LP-REVSEQNO.

           PERFORM WRITE-LP1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.
           PERFORM CLOSE-LP1-FILE.

      *****************************************
       DELETE-LX1-TRAILER SECTION.
      *****************************************
           MOVE "DELETING PAYMENT TRAILER RECORD..." TO MESS.
           PERFORM SEND-LEGEND.

      * DELETE LXA RECORD, ASSUMES YOU HAVE THE CORRECT SEQUENCE IN THE
      * BUFFER, SO I AM LEAVING THIS PART ALONE! CANT DELETE THE RV THAT 
      * YOU ARE CREATING, ITS TRYING TO CREATE THE ORIGINAL.
      * IN THE NEW COMBINED FILE, WE JUST NEED TO CLEAR THE ADDON FIELDS

      * REALLY SHOULD CHECK IF ANY OTHER TRAILER CODES ARE SET, AND
      * IF THEY ARENT, COULD DELETE THIS TRAILER, BUT DOESNT REALLY MATTER.

           MOVE SPACES TO LXA-CODE LXA-SRCD LXA-LN-INSOURS.
           MOVE 0      TO LXA-LN-INSCOMP.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 9
              MOVE 0 TO LXA-START-PMT(SUB) LXA-END-PMT(SUB)
                        LXA-NET-CHANGE(SUB).
           PERFORM REWRITE-LX1-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           PERFORM CLOSE-LX1-FILE.

      *****************************************
       UPDATE-TP-GLNO SECTION.
      *****************************************

           MOVE LP-REFNO TO LPWSLP-LP-REFNO.

           MOVE 1 TO SUB.
       NEXT-TP-GLNO.
           IF LTI-INSFEECD(SUB) NOT = SPACES
              MOVE BRANCH            TO CDB-BRANCH  GLNO-BR
              MOVE LTI-INSFEECD(SUB) TO CDB-CODE
              PERFORM GET-CODE
              IF IO-FG = 0
                 IF LC-USE-ALT-GL-FEE = "Y"
                     MOVE CDB-GC-ALT-GLFEENO TO GLNO-NUMBER
                 ELSE
                    MOVE CDB-GC-GLFEENO TO GLNO-NUMBER
                 END-IF
                 MOVE GLNO TO TP-GLNO(SUB)
      * LTI-INSFEE IS NOT SIGNED, ALWAYS +
                 IF (LPWSLP-LP-ADDON-TEXT = "CNL")
                    MOVE LTI-INSFEE(SUB) TO TP-GLAMT(SUB)
                 ELSE
                    IF TP-TRAMT >= 0
                       MOVE LTI-INSFEE(SUB) TO TP-GLAMT(SUB)
                    ELSE
                       COMPUTE TP-GLAMT(SUB) = 
                                    LTI-INSFEE(SUB) * -1.

            ADD 1 TO SUB.
            IF SUB < 3
               GO TO NEXT-TP-GLNO.

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

      *************************************
       SAVE-SCREEN SECTION.
      *************************************
           MOVE 3 TO WK-KEY.
           PERFORM OPEN-WK-FILE.
           PERFORM READ-WK-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           MOVE ELE TO WK-ELE.
           MOVE LN-REC TO WK-SAVE-RECORD.
           MOVE LP-REC TO WK-LP-REC.
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
           MOVE " " TO WK-SAVE-RECORD.
           MOVE 0 TO WK-ELE.
           PERFORM WRITE-WK-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.
           GO TO EXIT-RESTORE-SCREEN.
       RESTORE-CLEAR.
           MOVE WK-SAVE-RECORD TO LN-REC.
           MOVE WK-LP-REC      TO LP-REC.
           MOVE 0              TO WK-ELE.
           PERFORM REWRITE-WK-FILE.
       EXIT-RESTORE-SCREEN.
           PERFORM CLOSE-WK-FILE.

      ******************************************************
       GET-CON-REC-PRIOR-LNUPD SECTION.
      ******************************************************

      * SETUP OUTBAL WORKER FOR UPDATE OF STATS

           IF GP-DELINQ-WITH-O2 = "Y"
              MOVE LN-CURBAL TO PRIOR-SAVE-OUTBAL
           ELSE
              SUBTRACT LN-OT2BAL FROM LN-CURBAL
                         GIVING PRIOR-SAVE-OUTBAL.


           MOVE TRANS-DATE TO AGEING-DATE.
           PERFORM COMPUTE-CONTRACTUAL.
           PERFORM COMPUTE-RECENCY.

           MOVE CSUB TO PRIOR-CSUB.
           MOVE RDSUB TO PRIOR-RDSUB.

      *******************************************
       UPDATE-TY1-PRIOR SECTION.
      *******************************************
           IF LN-PLCD = " " AND ORIG-LN-IS-NOT-REPO
              SUBTRACT 1 FROM TY-UNITS(1 PRIOR-RDSUB)
                              TY-UNITS(2 PRIOR-CSUB)
              SUBTRACT PRIOR-SAVE-OUTBAL
                         FROM TY-OUTBAL(1 PRIOR-RDSUB)
                              TY-OUTBAL(2 PRIOR-CSUB)
              IF LN-BNKRPTDATE NOT = 0
                 SUBTRACT 1 FROM TY-UNITS-BNKRPT(1 PRIOR-RDSUB)
                                 TY-UNITS-BNKRPT(2 PRIOR-CSUB)
                 SUBTRACT PRIOR-SAVE-OUTBAL
                            FROM TY-OUTBAL-BNKRPT(1 PRIOR-RDSUB)
                                 TY-OUTBAL-BNKRPT(2 PRIOR-CSUB).

       UPDATE-TY1-PRIOR-EXIT.
           EXIT.

      *********************************************************
       UPDATE-DS1-PRIOR SECTION.
      *********************************************************
           IF LN-PLCD = " " AND ORIG-LN-IS-NOT-REPO
              SUBTRACT 1 FROM DS-UNITS(DSNOW DSREC PRIOR-RDSUB)
                              DS-UNITS(DSNOW DSCON PRIOR-CSUB)
              SUBTRACT PRIOR-SAVE-OUTBAL
                         FROM DS-OUTBAL(DSNOW DSREC PRIOR-RDSUB)
                              DS-OUTBAL(DSNOW DSCON PRIOR-CSUB)
              IF LN-BNKRPTDATE NOT = 0
                 SUBTRACT 1
                         FROM DS-UNITS-BNKRPT(DSNOW DSREC PRIOR-RDSUB)
                              DS-UNITS-BNKRPT(DSNOW DSCON PRIOR-CSUB)
                 SUBTRACT PRIOR-SAVE-OUTBAL
                         FROM DS-OUTBAL-BNKRPT(DSNOW DSREC PRIOR-RDSUB)
                              DS-OUTBAL-BNKRPT(DSNOW DSCON PRIOR-CSUB).

       UPDATE-DS1-PRIOR-EXIT.
           EXIT.

      ******************************************************
       GET-CON-REC-AFTER-LNUPD SECTION.
      ******************************************************

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
           MOVE "UPDATING TODAY'S DELINQ TOTAL FILE......." TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM GET-TY1-FILE.

           PERFORM POST-TY1-AFTER-UPDATE.

      * UPDATE UNEARNED STATISTICS NET AND VOLUME
           IF GP-UNEARNED-STATS = "Y"
              SUBTRACT LP-APINT FROM TY-UNEARNED-AMT(1 2)
                                     TY-UNEARNED-AMT(2 2).

           PERFORM REWRITE-TY1-FILE.
           PERFORM CLOSE-TY1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

       UPDATE-TY1-AFTER-EXIT.
           EXIT.

      *************************************************************
       POST-TY1-AFTER-UPDATE SECTION.
      *************************************************************

      * P&L AND REPO EXCLUDED BEFORE THIS PERFORM
           ADD 1 TO TY-UNITS(1 AFTER-RDSUB)
                    TY-UNITS(2 AFTER-CSUB).
           ADD AFTER-SAVE-OUTBAL TO TY-OUTBAL(1 AFTER-RDSUB)
                                    TY-OUTBAL(2 AFTER-CSUB).
           IF LN-BNKRPTDATE NOT = 0
              ADD 1 TO TY-UNITS-BNKRPT(1 AFTER-RDSUB)
                       TY-UNITS-BNKRPT(2 AFTER-CSUB)
              ADD AFTER-SAVE-OUTBAL
                    TO TY-OUTBAL-BNKRPT(1 AFTER-RDSUB)
                       TY-OUTBAL-BNKRPT(2 AFTER-CSUB).

      *******************************************
       UPDATE-DS1-AFTER SECTION.
      *******************************************
           MOVE "UPDATING DSFILE DELINQ TOTALS........." TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM GET-DS1-FILE.

           PERFORM POST-DS1-AFTER-UPDATE.

      * UPDATE UNEARNED STATISTICS NET AND VOLUME
           IF GP-UNEARNED-STATS = "Y"
              SUBTRACT LP-APINT FROM DS-UNEARNED-AMT(1 1 2)
                                     DS-UNEARNED-AMT(1 2 2)
                                     DS-UNEARNED-AMT(2 2 2).

           PERFORM REWRITE-DS1-FILE.
           PERFORM CLOSE-DS1-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

       UPDATE-DS1-AFTER-EXIT.
           EXIT.

      ************************************************************
       POST-DS1-AFTER-UPDATE SECTION.
      ************************************************************

      * P&L AND REPO EXCLUDED BEFORE THIS PERFORM

           ADD 1 TO DS-UNITS(DSNOW DSREC AFTER-RDSUB)
                    DS-UNITS(DSNOW DSCON AFTER-CSUB).
           ADD AFTER-SAVE-OUTBAL
                 TO DS-OUTBAL(DSNOW DSREC AFTER-RDSUB)
                    DS-OUTBAL(DSNOW DSCON AFTER-CSUB).
           IF LN-BNKRPTDATE NOT = 0
              ADD 1 TO DS-UNITS-BNKRPT(DSNOW DSREC AFTER-RDSUB)
                       DS-UNITS-BNKRPT(DSNOW DSCON AFTER-CSUB)
              ADD AFTER-SAVE-OUTBAL
                    TO DS-OUTBAL-BNKRPT(DSNOW DSREC AFTER-RDSUB)
                       DS-OUTBAL-BNKRPT(DSNOW DSCON AFTER-CSUB).

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
      * 2/28/96 BAH ADDED THE TEST ON E-CODE "22"
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
      * 2/28/96 BAH ADDED THE TEST ON E-CODE "22"
           IF IO-FG = 9 AND E-CODE NOT = "22"
              GO TO IO-ERROR.
           GO TO GET-DS1-FILE-CHECK-EXISTS.

       GET-DS1-FILE-EXIT.
           EXIT.


       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBLP/LPSCHD.CPY".
       COPY "LIBLP/LPTYCL.CPY".
       COPY "LIBLP/LPDSCL.CPY".
       COPY "LIBLP/POSTDATE.CPY".
       COPY "LIBLP/LPSRCD.CPY".
       COPY "LIBLP/LOADLXA.CPY".


      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           IF VDU-FORM-NAME = "LPSMNU"
              MOVE LPSMNU-HELP--FILE TO HELP-LINK-FILE
              MOVE LPSMNU-HELP--DIR  TO HELP-LINK-DIR
           ELSE
              MOVE LONPF-HELP--FILE TO HELP-LINK-FILE
              MOVE LONPF-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LONPF_EVA.CPY".
              COPY "LIBLP/LPSMNU_EVA.CPY".
              COPY "LIBLP/LPLNMU_EVA.CPY".
              COPY "LIBLP/LPCAMU_EVA.CPY".
              COPY "LIBLP/LPVDMU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *************************************
       MISCELLANEOUS-ROUTINES SECTION.
      *************************************
       COPY "LIBGB/GETENV.CPY".
       FORM-RESET.
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       UPDATE-LN-SUM-ORGTERM.
           PERFORM READ-LN1-FILE.
           MOVE SCHD-TERM TO LN-SUM-ORGTERM.
           PERFORM REWRITE-LN1-FILE.
       REMOVE-LN-FLAG.
           PERFORM READ-LN1-FILE.
           MOVE " " TO LN-PAY-SCHLD-FG.
           PERFORM REWRITE-LN1-FILE.
       SET-TY-KEY.
           MOVE LN-OWNBR   TO TY-BRNO.
           MOVE TRANS-DATE TO TY-DATE.
           MOVE LN-CLASS   TO TY-CLASS.
       SET-DS-KEY.
           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM TO DS-DATE-CCYYMM.
           MOVE LN-OWNBR    TO DS-BRANCH.
           MOVE LN-DLNO     TO DS-DLNO.
           MOVE LN-CLASS    TO DS-CLASS.
       GET-CODE.
           MOVE "GC"   TO CDB-TYPE.
      *--> SET BEFORE     CDB-BRANCH.
      *--> SET BEFORE     CDB-CODE.
           PERFORM OPEN-CDB1-FILE.
           PERFORM READ-CDB1-FILE.
           PERFORM CLOSE-CDB1-FILE.

       COPY "LIBLP/LPTRPCL.CPY".
       COPY "LIBLP/LPLXCL.CPY".

      ********************************************
       IO-ROUTINES SECTION.
      ********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTYGS_SQL.CPY".
       COPY "LIBLP/LPTRPGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLXGS_SQL.CPY".
       COPY "LIBLP/LPNOGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLTIGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
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

