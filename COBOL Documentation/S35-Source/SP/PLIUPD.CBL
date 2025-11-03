       IDENTIFICATION DIVISION.
       PROGRAM-ID.      PLIUPD.
      ******************************************************************
      *
      *
      * DESC: UPDATE LOANS TO INACTIVE P&L STATUS FROM ASCII INPUT FILE
      *
      *       READ IN FIXED LENGTH ASCII FILE
      *          GLOBAL SP/PLIUPD.MMDDYY
      *
      * REV:
      *  2025.0130 BAH REMOVE TYFILE AND CREATE LPFILE RECORD SO THEY
      *                CAN REVERSE IF NEEDED #1700 S35Q-161
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       SELECT INPUT-FILE ASSIGN INPUT-PATH
              ORGANIZATION LINE SEQUENTIAL
              ACCESS MODE SEQUENTIAL
              LOCK MODE AUTOMATIC WITH LOCK ON RECORD
              FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      ******************************************************************
      * ASCII INPUT FILE
      ******************************************************************
       FD  INPUT-FILE
           LABEL RECORDS ARE STANDARD.
       01  INPUT-REC.
           03  INPUT-BRNO-ALP.
                05  INPUT-BRNO       PIC 9(4).
           03  INPUT-ACCT-NUMBER-ALP.
               05  INPUT-ACCT-NUMBER PIC 9(6).
           03  INPUT-CURBAL          PIC 9(6).99.
           03  INPUT-CR              PIC X.
           03  INPUT-LF              PIC X.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)SP/PLIUPD S35 02/05/25-13:13:22 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LP01NO_SQL.CPY".
       COPY "LIBLP/LPWSNO.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       01  INPUT-IFN               PIC X(2)   VALUE "WK".

       01  INPUT-PATH.
           03  INPUT-PATH-BASE          PIC X(09).
           03  FILLER                   PIC X(01)   VALUE "/".
           03  INPUT-PATH-MACHINE       PIC X(02).
           03  FILLER                   PIC X(11)   VALUE "/SP/PLIUPD.".
           03  INPUT-PATH-DATE-MMDDYY   PIC 9(06).

       01  LN-WORKERS.
           03  LN-FEED         PIC 99     VALUE 0.
           03  LN-COUNT        PIC 999    VALUE 99.
           03  LN-WK           PIC 999    VALUE 0.
       01  PAGE-NUMBER         PIC 9(4)   VALUE 0.
       01  FIRST-TIME          PIC X      VALUE "Y".
       01  RECORDS-FOUND       PIC X      VALUE " ".
           88  NO-RECORDS-FOUND           VALUE "N".

       01  HOLD-BRNO            PIC 9(4)  VALUE 0.
       01  REC-EXISTS           PIC X     VALUE " ".

       01  TEST-CURBAL          PIC 9(6).99.

       01  NEXT-SEQNO           PIC 9(5)     VALUE 0.

       01  TOT-RECORDS      PIC 9(9)  COMP-3 VALUE 0.
       01  TOT-UPDATED      PIC 9(9)  COMP-3 VALUE 0.
       01  TOT-INCORRECT    PIC 9(9)  COMP-3 VALUE 0.

       01  VERSION-CONTROL      PIC X VALUE "A".

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  TRANS-DATE       PIC 9(8).
           03  REPORT-ONLY-FG   PIC X.
           03  BRANCH-PAGE-FG   PIC X.

       01  MESS-LIST            PIC X(5) VALUE "MESS.".
       01  DISPLAY-BUF.
           03  FILLER           PIC X(22) VALUE "PROCESSING..".
           03  DISPLAY-COUNT    PIC Z(8).
           03  FILLER           PIC X(20) VALUE SPACES.

       01  ERROR-EXPLANATIONS.
           03  ERROR-MESGX.
               05  FILLER       PIC X(33) VALUE
               "INVALID BRANCH, NO UPDATE                 ".
               05  FILLER       PIC X(33) VALUE
               "MISSING LOAN FILE, NO UPDATE".
               05  FILLER       PIC X(33) VALUE
               "INVALID ACCOUNT, NO UPDATE".
               05  FILLER       PIC X(33) VALUE
               "ACCOUNT IS NOT P&L, NO UPDATE".
               05  FILLER       PIC X(33) VALUE
               "ACCOUNT P&L INACTIVE, NO UPDATE".
               05  FILLER       PIC X(33) VALUE
               "ACCOUNT P&L WRITE-OFF, NO UPDATE".
               05  FILLER       PIC X(33) VALUE
               "LOAN BALANCE CHANGED, NO UPDATE".
               05  FILLER       PIC X(33) VALUE
               "ERROR CREATING PAYMENT, NO UPDATE".
           03  ERROR-MESG REDEFINES ERROR-MESGX PIC X(33)
                                OCCURS 8 TIMES.
       01  ERROR-SUB            PIC 9(2)     VALUE 0.

      ********************************************************
      *            P R I N T    B U F F E R S                *
      ********************************************************
       01  HEAD1.
           03  H-DATE             PIC X(8)    VALUE " ".
           03  FILLER             PIC XX      VALUE " ".
           03  H-HR               PIC 99      VALUE 0.
           03  FILLER             PIC X       VALUE ":".
           03  H-MIN              PIC 99      VALUE 0.
           03  FILLER             PIC X(5)   VALUE " ".
           03  H-NAME             PIC X(40)   VALUE " ".
           03  FILLER             PIC X(11)   VALUE " ".
           03  FILLER             PIC X(5)    VALUE "PAGE ".
           03  H-PAGE-NO          PIC ZZZ9.

       01  HEAD2.
           03  HEAD2-BRANCH-INFO.
                05  FILLER        PIC X(9)    VALUE "BRANCH #".
                05  H-BRNO        PIC Z999.
           03  FILLER             PIC X(16)    VALUE " ".
           03  FILLER             PIC X(31)    VALUE
               "UPDATE ACCOUNTS TO INACTIVE P&L".
           03  FILLER             PIC X(6)     VALUE " ".
           03  FILLER             PIC X(10)    VALUE "USER ID:".
           03  H-USERID           PIC X(08).
       01  HEAD3.
           03  FILLER             PIC X(45)    VALUE
               "BRNO  ACCT#     BALANCE  STATUS".
       01  DETAIL-LINE          PIC X(132) VALUE SPACES.
       01  DETAIL-LINE1 REDEFINES DETAIL-LINE.
           03  D-BRNO           PIC Z(4).
           03  FILLER           PIC XX.
           03  D-ACCT-NUMBER    PIC Z(6).
           03  FILLER           PIC XX.
           03  D-CURBAL         PIC Z(6).99.
           03  FILLER           PIC X(2).
           03  D-STATUS         PIC X(6).
           03  FILLER           PIC X(12).
           03  D-STAR1          PIC X(2).
           03  D-ERROR-MSG      PIC X(33).
           03  D-STAR2          PIC X(2).

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(22).
           03  RANGE-SELECTION  PIC X(30).
           03  FILLER           PIC X(5).
           03  RANGE1           PIC X(10).
           03  RANGE1N REDEFINES RANGE1 PIC 9(10).
           03  FILLER           PIC X(2).
           03  RANGE-DATE       PIC 9999/99/99.

       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPPOFFW.CPY".
       COPY "LIBCL/CRNOR2W.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/PLIUPD_DEF.CPY".
       COPY "LIBSP/PLIUPD_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/PLIUPD_SCN.CPY".

      *----------------------------------------------------------------
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               INPUT-FILE PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-NO1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE
               INPUT-FILE PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *******************************************
       MAIN-MODULE SECTION.
      *******************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "PLIUPD" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

       NEXT-WORK.
           MOVE SPACES TO INPUT-REC.
           PERFORM READ-INPUT-FILE.
           IF IO-FG NOT = 0
              GO TO END-ROUTINE.

           INSPECT INPUT-BRNO-ALP REPLACING LEADING " " BY "0".
           INSPECT INPUT-ACCT-NUMBER-ALP REPLACING LEADING " " BY "0".

           MOVE 0 TO ERROR-SUB.
           IF FIRST-TIME = "Y"
              MOVE "N" TO FIRST-TIME
              GO TO GET-BRANCH-PATH-INFO.

           IF INPUT-BRNO = HOLD-BRNO
              GO TO GET-LOAN.

           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LP1-FILE.

       GET-BRANCH-PATH-INFO.
           MOVE INPUT-BRNO TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG = 9
              MOVE 1 TO ERROR-SUB
              GO TO PRINT-DETAIL.

           MOVE INPUT-BRNO TO HOLD-BRNO.
       MOVE-BRANCH-PATH-INFO.
           IF BRANCH-PAGE-FG = "Y"
              MOVE 99 TO LN-COUNT
              MOVE BR-NO TO H-BRNO
           ELSE
              MOVE SPACES TO HEAD2-BRANCH-INFO.

           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.
           MOVE "B"        TO LN-PATH-OVERRIDE
                              LP-PATH-OVERRIDE
                              LTP-PATH-OVERRIDE
                              NO-PATH-OVERRIDE.


           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-LP1-FILE.
           PERFORM OPEN-NO1-FILE.

       GET-LOAN.
           MOVE INPUT-BRNO        TO LN-OWNBR.
           MOVE INPUT-ACCT-NUMBER TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG = 9
              MOVE 3 TO ERROR-SUB
              GO TO PRINT-DETAIL.

           IF LN-PLDATE = 0
      *       UNLOCK LN-FILE
              MOVE 4 TO ERROR-SUB
              GO TO PRINT-DETAIL.

           IF LN-PLCD = "I"
      *       UNLOCK LN-FILE
              MOVE 5 TO ERROR-SUB
              GO TO PRINT-DETAIL.

           IF LN-PLCD = "W"
      *       UNLOCK LN-FILE
              MOVE 6 TO ERROR-SUB
              GO TO PRINT-DETAIL.

           MOVE LN-CURBAL TO TEST-CURBAL.
           IF TEST-CURBAL NOT = INPUT-CURBAL
      *       UNLOCK LN-FILE
              MOVE 7 TO ERROR-SUB
              GO TO PRINT-DETAIL.

           IF REPORT-ONLY-FG = "Y"
      *       UNLOCK LN-FILE
              GO TO PRINT-DETAIL.

           IF EXT-ROUTE-BUF = "00"
              IF PRU-LP NOT = "VDU" AND NOT = "PRU"
                 MOVE TOT-RECORDS TO DISPLAY-COUNT
                 MOVE DISPLAY-BUF TO WR-BUF
                 MOVE MESS-LIST TO WR-BUF
                 PERFORM CALL-WRFORM.

           MOVE LN-ORGST     TO SP-ORGST.
           MOVE LN-SPRCLASS  TO SP-SPRCLASS.
           MOVE LN-SUBCLASS  TO SP-SUBCLASS.
           MOVE LN-LAWCODE   TO SP-LAWCODE.

           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR.

           MOVE "I" TO LN-PLCD.
           PERFORM UPDATE-LN-STATUS.

       UPDATE-LOAN.
           PERFORM REWRITE-LN1-FILE.

           PERFORM UPDATE-NOTICE-FILE.
           PERFORM CREATE-PAYMENT-RECORD.

       PRINT-DETAIL.
           MOVE INPUT-BRNO        TO D-BRNO.
           MOVE INPUT-ACCT-NUMBER TO D-ACCT-NUMBER.
           MOVE INPUT-CURBAL      TO D-CURBAL.
           IF ERROR-SUB = 0
              MOVE "OKAY " TO D-STATUS
              IF REPORT-ONLY-FG = "N"
                 MOVE "ACCOUNT SUCCESSFULLY UPDATED" TO D-ERROR-MSG
                 ADD 1 TO TOT-UPDATED
              ELSE
                 MOVE "ACCOUNT OK TO BE UPDATED" TO D-ERROR-MSG
              END-IF
           ELSE
              MOVE "ERROR" TO D-STATUS
              MOVE ERROR-MESG(ERROR-SUB) TO D-ERROR-MSG
              MOVE "****" TO D-STAR1 D-STAR2
              ADD 1 TO TOT-INCORRECT.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           ADD 1 TO TOT-RECORDS.

           GO TO NEXT-WORK.

       END-ROUTINE.

           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM.

           PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.


      ***********************************************************
       UPDATE-LN-STATUS SECTION.
      ***********************************************************
           SET STFG TO 1.
           SEARCH LN-STATUSFLAGS
              AT END PERFORM PREPARE-TO-SHIFT
              WHEN (
                    (LN-STATUSFLAGS(STFG) = " ")
                     OR (LN-STATUSFLAGS(STFG) = "P" OR "2" OR "3"
                                                    OR "4" OR "5")
                     OR (LN-STATUSFLAGS(STFG) = "I")
                     OR (LN-STATUSFLAGS(STFG) = "W")
                   )
                     MOVE "I" TO LN-STATUSFLAGS(STFG)
                     GO TO OUT-OF-SEARCH.
       SHIFT-STATUS.
           IF STFG < 10
              IF LN-STATUSFLAGS(STFG + 1) = " "
                 GO OUT-OF-SEARCH
              ELSE
                 MOVE LN-STATUSFLAGS(STFG + 1) TO LN-STATUSFLAGS(STFG)
                 MOVE " " TO LN-STATUSFLAGS(STFG + 1)
                 SET STFG UP BY 1
                 GO TO SHIFT-STATUS.
       OUT-OF-SEARCH.
           EXIT.

      ***********************************************************
       PREPARE-TO-SHIFT SECTION.
      ***********************************************************
           SET STFG TO 1.
       SHIFT-LOOP.
           MOVE LN-STATUSFLAGS(STFG + 1) TO LN-STATUSFLAGS(STFG).
           SET STFG UP BY 1.
           IF STFG < 10
              GO TO SHIFT-LOOP.
           MOVE "I" TO LN-STATUSFLAGS(STFG).
       EXIT-PREPARE-TO-SHIFT.
           EXIT.

      ********************************************
       CREATE-PAYMENT-RECORD SECTION.
      ********************************************
           MOVE LN-OWNBR      TO LP-BRNO
                                 QLP1-WBEG-BRNO
                                 QLP1-WEND-BRNO.
           MOVE LN-ACCTNO     TO LP-ACCTNO
                                 QLP1-WBEG-ACCTNO
                                 QLP1-WEND-ACCTNO.
           MOVE 99999         TO LP-SEQNO
                                 QLP1-WEND-SEQNO.
           MOVE ALL "0"       TO QLP1-WBEG-SEQNO.

           PERFORM START-LP1-FILE-NOT-GREATER.
           PERFORM READ-LP1-FILE-PREVIOUS.
           IF IO-FG NOT = 0 OR (LP-BRNO NOT = LN-OWNBR) OR
                               (LP-ACCTNO NOT = LN-ACCTNO)
              MOVE 8 TO ERROR-SUB
              GO TO EXIT-PAYMENT-RECORD.

           COMPUTE NEXT-SEQNO = LP-SEQNO + 1.

           PERFORM CLEAR-LP-FILE.

           MOVE LN-OWNBR   TO LP-BRNO.
           MOVE LN-ACCTNO  TO LP-ACCTNO.
           MOVE NEXT-SEQNO TO LP-SEQNO.

           MOVE TRANS-DATE TO LP-TRDATE LP-PAYDATE.
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO LP-POSTDATE.
           MOVE "PI"     TO LP-TRCD.
           MOVE "PLIUP"  TO LP-REFNO.

           MOVE NO-PDTH-DATE      TO LP-PDTH-DATE.
           MOVE LN-LCPDTH-DATE    TO LP-LCPDTH-DATE.

           MOVE LN-LCBAL  TO LP-LCBAL.
           MOVE LN-INTBAL TO LP-INTBAL.
           MOVE LN-OTHBAL TO LP-OTHBAL.
           MOVE LN-CURBAL TO LP-CURBAL.
      * LP-INTDUE FOR A "PI" IS LN-CURBAL (LONPF9)
           MOVE LN-CURBAL TO LP-INTDUE.

           MOVE 1          TO LP-TILLNO.

           MOVE LN-SSNO(1) TO LP-SSNO.
           PERFORM WRITE-LP1-FILE.
           IF IO-FG NOT = 0
              MOVE 8 TO ERROR-SUB
              GO TO EXIT-PAYMENT-RECORD.

       EXIT-PAYMENT-RECORD.
           EXIT.


      ***********************************************
       UPDATE-NOTICE-FILE SECTION.
      ***********************************************
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

       EXIT-UPDATE-NOTICE-FILE.
           EXIT.

      ***********************************************************
       INITIALIZATION SECTION.
      ***********************************************************
           MOVE PLIUPD-QUEUE-POS TO Q-POS.
           MOVE PLIUPD-COPIES-POS TO C-POS.
           MOVE PLIUPD-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF REPORT-ONLY-FG = "N"
              MOVE "INACTIVE P&L UPDATE IN PROCESS" TO WR-BUF
           ELSE IF REPORT-ONLY-FG = "Y"
              MOVE "INACTIVE P&L REPORT IN PROCESS" TO WR-BUF.
           IF EXT-ROUTE-BUF = "00"
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-INPUT-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-SP1-FILE.

           MOVE 0 TO TOT-RECORDS TOT-UPDATED TOT-INCORRECT.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           PERFORM OPEN-PR-FILE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ****************************************
       ENTER-PARAMETERS SECTION.
      ****************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO PAGE-ON-BRANCH.

       ENTER-TRANS-DATE.
           MOVE "ERNM060TRDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-TRANS-DATE.
           MOVE VDU-DATE-YYYYMMDD TO TRANS-DATE.

           IF TRANS-DATE = 0
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD
              MOVE "S  8000TRDATE." TO VDU
              MOVE DATE-YYYYMMDD TO VDU-DATE-YYYYMMDD TRANS-DATE
              PERFORM SCREENIO.

           MOVE EXT-FILPATH-BASE     TO INPUT-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE  TO INPUT-PATH-MACHINE.
           MOVE TRANS-DATE           TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY          TO INPUT-PATH-DATE-MMDDYY.


           MOVE "S  A000WKPATH." TO VDU.
           MOVE INPUT-PATH TO VDUBUF.
           PERFORM SCREENIO.

       ENTER-PATH.

           MOVE "E  A350WKPATH." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-TRANS-DATE.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-PATH.

           MOVE VDUBUF TO INPUT-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT-BAD
              MOVE "INPUT FILE NOT FOUND; PLEASE RE-ENTER" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PATH.

       REPORT-ONLY-YN.
           MOVE "E  A010REPORTONLY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-PATH.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO REPORT-ONLY-YN.
           MOVE VDUBUF TO REPORT-ONLY-FG.
           IF REPORT-ONLY-FG NOT = "Y"
              IF REPORT-ONLY-FG NOT = "N"
                 GO TO REPORT-ONLY-YN.

       PAGE-ON-BRANCH.
           MOVE "E  A010PAGE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO REPORT-ONLY-YN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO PAGE-ON-BRANCH.
           MOVE VDUBUF TO BRANCH-PAGE-FG.
           IF BRANCH-PAGE-FG = "Y" GO TO EXIT-PARM.
           IF BRANCH-PAGE-FG NOT = "N" GO TO PAGE-ON-BRANCH.

       EXIT-PARM.
           EXIT.

      ****************************************
       EO-EXEC-AUDIT SECTION.
      ****************************************
           MOVE EXT-FILPATH-BASE    TO INPUT-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE TO INPUT-PATH-MACHINE.
           MOVE EXT-EOBUF-DATE2     TO TRANS-DATE DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO INPUT-PATH-DATE-MMDDYY.

      ****************************************
       DISPLAY-PARAMETERS SECTION.
      ****************************************
           MOVE "S  8080TRDATE." TO VDU.
           MOVE TRANS-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  A0-0WKPATH." TO VDU.
           MOVE INPUT-PATH TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010REPORTONLY." TO VDU.
           MOVE REPORT-ONLY-FG TO VDUBUF.
           PERFORM SCREENIO.

      ****************************************
       WRITE-DETAIL-LINE SECTION.
      ****************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.
       EXIT-DET-LINE.
           EXIT.

      ****************************************
       HEAD-PAGE SECTION.
      ****************************************
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
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 2 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      ****************************************
       PRINT-RANGE-LINES SECTION.
      ****************************************

           MOVE 2 TO LN-FEED.

           MOVE "TRANS DATE" TO RANGE-SELECTION.
           MOVE TRANS-DATE TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "REPORT ONLY " TO RANGE-SELECTION.
           MOVE REPORT-ONLY-FG TO RANGE1.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PAGE ON BRANCH" TO RANGE-SELECTION.
           MOVE BRANCH-PAGE-FG TO RANGE1.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.
           MOVE "TOTAL RECORDS READ" TO RANGE-SELECTION.
           MOVE TOT-RECORDS TO RANGE1N.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "TOTAL RECORDS INCORRECT DATA" TO RANGE-SELECTION.
           MOVE TOT-INCORRECT TO RANGE1N.
           PERFORM WRITE-DETAIL-LINE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPLPCL.CPY".
       COPY "LIBGB/GPENV.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO PLIUPD-CONAME.
           MOVE EXT-CONAME-SYSDATE TO PLIUPD-SYSDATE.
           DISPLAY PLIUPD-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE PLIUPD-FORM-FIELDS TO WR-BUF.
           MOVE PLIUPD-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE PLIUPD-HELP--FILE TO HELP-LINK-FILE.
           MOVE PLIUPD-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/PLIUPD_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ************************************************
       MISC-ROUTINES SECTION.
      ************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".

      ********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPNOGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPNO1IN.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPLP1IN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       OPEN-INPUT-FILE.
           PERFORM OPEN-IT.
           OPEN INPUT INPUT-FILE.
           IF IO-FG = 8
              GO TO OPEN-INPUT-FILE.
           IF IO-FG = 7
              CLOSE INPUT-FILE
              GO TO OPEN-INPUT-FILE.
       READ-INPUT-FILE.
           PERFORM READ-IT.
           READ INPUT-FILE.
           IF IO-FG = 8
              GO TO READ-INPUT-FILE.
       CLOSE-INPUT-FILE.
           PERFORM CLOSE-IT.
           CLOSE INPUT-FILE.
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      *********************************
       END-PROGRAM SECTION.
      *********************************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND" TO MESS
              PERFORM SEND-MESS.
       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE "SP/EXTRMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY PLIUPD-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
