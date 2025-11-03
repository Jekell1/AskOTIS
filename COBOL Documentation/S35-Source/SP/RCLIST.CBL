       IDENTIFICATION DIVISION.
       PROGRAM-ID.   RCLIST.
       DATE-WRITTEN. 020294
      *****************************************************************
      *                      (SP)
      *    DESCRIPTION:  BRANCH DAILY STATUS LISTING
      * REV:
      *    092394 BLK CHANGE TO INCLUDE RC-STATUSES OF C1
      *    091595 BLL CHANGE - HIGH STATUS NOW M1 NOT K2
      *    GLF 090397 REMOVED ALL USE OF GLOBAL
      *    BLV 991005 ADD DEPOSIT DATE RANGE - FOR EXAMPLE,
      *               WORLD RUNS DEPOSIT WHICH EXTRACTS BASED ON
      *               RC STATUSES, BEFORE EXTRACTION THEY RUN REPORT
      *               ONLY FOR STATE TOTALS.  IF THEY FORGET & EXTRACT
      *               BEFORE RUNNING REPORT ONLY, THEY CAN SELECT TODAYS
      *               DATE FOR DEPOSIT & USE BRANCH RANGES TO GET STATE
      *               TOTALS
      *    BLV 030506 ADD OPTION FOR F3 ALL DEPOSIT DATES
      *    BLV 030530 FIX F3 ON DEPOSIT DATE WAS PUTTING ALL DATES IN
      *               STATUS DATE RANGE
      *    BLV 040109 FIX TO WORK IN BATCH
      *
      * KEC 2015.1125 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED RC-TRANS-DATE FROM YYYMMDD TO CCYYMMDD.
      *    BLM 1610118 ADD RC-CHECK-DEPOSIT TO REPORT, WORLD PR# 1188
      *  20171013 BAH ACTIVATE 8 DIGIT DATES, PD0006
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/RCLIST.CBL J35 12/21/23-09:45:55 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01RC.CPY".
       COPY "LIBGB/GB01RC_SQL.CPY".
       COPY "LIBGB/GBWSRC.CPY".
      ****************************
      *     F I L E   I F N      *
      ****************************
       01  PR-IFN               PIC X(2)     VALUE "PR".

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  ER-FEED              PIC 9        VALUE 1.

      **********************
      *   MISC WORKERS     *
      **********************
       01  RECORDS-FOUND        PIC X     VALUE " ".
           88  NO-RECORDS-FOUND           VALUE "N".

       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  MSEL-LIST            PIC X(5)     VALUE "MSEL.".

       01  LEGEND               PIC X(70).
       COPY "LIBLP/EMSFRG_CMD.CPY".
      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH             PIC 9999.
           03  END-BRANCH             PIC 9999.
           03  BEG-DATE               PIC 9(8).
           03  END-DATE               PIC 9(8).
           03  STAT1                  PIC XX.
           03  STAT2                  PIC XX.
           03  BEG-DEP-DATE           PIC 9(8).
           03  END-DEP-DATE           PIC 9(8).

       01  VERSION-CONTROL     PIC X     VALUE "A".

       01  TOT-DEPOSITS        PIC S9(9)V99 VALUE +0.
       01  TOT-CHECK-DEPOSITS  PIC S9(9)V99 VALUE +0.
       01  TOT-CHECKS          PIC S9(9)V99 VALUE +0.
       01  TOT-OVER-SHORT      PIC S9(9)V99 VALUE +0.

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
           03  FILLER           PIC X(30)    VALUE " ".
           03  FILLER           PIC X(27) VALUE
               "BRANCH DAILY STATUS LISTING".

       01  HEAD3.
           03  FILLER           PIC X(30)    VALUE
               "BR #  BRANCH NAME".
           03  FILLER           PIC X(46)    VALUE 
               " DATE   STATUS   RECAP   GL EODB  DEP  EOMUPD ".
           03  FILLER           PIC X(35)    VALUE 
               " EOM2       CHECKS       DEPOSIT   ".
           03  FILLER           PIC X(21)    VALUE 
               "CHK DEPOSIT OV-SHORT ".

       01  DETAIL-LINE          VALUE SPACES.
           03  FILLER           PIC X(132).

       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-BRNO             PIC Z999.
           03  FILLER             PIC X(2).
           03  D-BRNAME           PIC X(22).
           03  FILLER             PIC X.
           03  D-DATE             PIC 99/99/99.
           03  FILLER             PIC X.
           03  D-STATUS           PIC X(9).
           03  FILLER             PIC XX.
           03  D-RECAP-FG         PIC X(4).
           03  FILLER             PIC XXX.
           03  D-GL-FG            PIC X(3).
           03  FILLER             PIC X.
           03  D-EOD-BATCH-FG     PIC X(4).
           03  FILLER             PIC X.
           03  D-NIGHTLY-FUNDS-FG PIC X(3).
           03  FILLER             PIC X(4).
           03  D-EOM1-BATCH-FG    PIC X(3).
           03  FILLER             PIC X(4).
           03  D-EOM2-BATCH-FG    PIC X(2).
           03  D-CHECKS           PIC ZZ,ZZZ,ZZ9.99-.
           03  D-DEPOSIT          PIC ZZ,ZZZ,ZZ9.99-.
           03  D-CHECK-DEPOSIT    PIC ZZ,ZZZ,ZZ9.99-.
           03  D-OVER-SHORT       PIC ZZZZ9.99-     BLANK ZERO.

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/RCLIST_DEF.CPY".
       COPY "LIBSP/RCLIST_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/RCLIST_SCN.CPY".

      *----------------------------------------------------------------
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-RC2-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      **************************************************
       MAIN-MODULE SECTION.
      **************************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "RCLIST" TO FORM-PROG VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

       NEXT-RC.
           PERFORM READ-RC2-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-ROUTINE.

           IF RC-STATUS < STAT1
              GO TO NEXT-RC.
           IF RC-STATUS > STAT2
              GO TO END-ROUTINE.

           IF RC-BRNO < BEG-BRANCH OR RC-BRNO > END-BRANCH
              GO TO NEXT-RC.

           IF ( RC-TRANS-DATE < BEG-DATE ) OR
              ( RC-TRANS-DATE > END-DATE )
              GO TO NEXT-RC.

      * IN A15, A 0 RC-FUNDS-DATE WOULD BE INCLUDED, HERE IN A30 IT
      * COMES OUT SYS-IS-LT, SO DONT TEST RANGE IF ALL WAS SELECTED
           IF NOT (BEG-DEP-DATE = EXT-JULIAN-BEG AND
              END-DEP-DATE = EXT-JULIAN-END)
              IF RC-FUNDS-DATE < BEG-DEP-DATE OR
                  RC-FUNDS-DATE > END-DEP-DATE
                 GO TO NEXT-RC.

           MOVE RC-BRNO TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           MOVE BR-NAME TO D-BRNAME.
           IF FIRST-TIME = "Y"
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE 1 TO LN-FEED
              MOVE "N" TO FIRST-TIME.
           MOVE RC-BRNO TO D-BRNO.

           MOVE RC-TRANS-DATE       TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY         TO D-DATE.

           IF (RC-UPDATED OR RC-EOD-BATCH-DONE)
              MOVE "UPDATED" TO D-STATUS
           ELSE
              IF RC-BANK-DEPOSIT
                 MOVE "DEPOSITED" TO D-STATUS
              ELSE
                 MOVE RC-STATUS TO D-STATUS.
           MOVE RC-CHECKS TO D-CHECKS.
           MOVE RC-DEPOSIT TO D-DEPOSIT.
           ADD RC-DEPOSIT TO TOT-DEPOSITS.
           IF RC-CHECK-DEPOSIT = 2020202.02 OR 0202020.20
              MOVE 0 TO RC-CHECK-DEPOSIT.
           MOVE RC-CHECK-DEPOSIT TO D-CHECK-DEPOSIT.
           ADD RC-CHECK-DEPOSIT TO TOT-CHECK-DEPOSITS.
           ADD RC-CHECKS TO TOT-CHECKS.
           ADD RC-OVER-SHORT TO TOT-OVER-SHORT.
           MOVE RC-RECAP-FG TO D-RECAP-FG.
           MOVE RC-GL-FG TO D-GL-FG.
           MOVE RC-EOD-BATCH-FG TO D-EOD-BATCH-FG.
           MOVE RC-NIGHTLY-FUNDS-FG TO D-NIGHTLY-FUNDS-FG.
           MOVE RC-EOM1-BATCH-FG TO D-EOM1-BATCH-FG.
           MOVE RC-EOM2-BATCH-FG TO D-EOM2-BATCH-FG.
           MOVE RC-OVER-SHORT TO D-OVER-SHORT.
           PERFORM WRITE-DETAIL-LINE.

           GO TO NEXT-RC.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM.
           MOVE "***  TOTAL  *** :" TO D-BRNAME.
           MOVE TOT-DEPOSITS TO D-DEPOSIT.
           MOVE TOT-CHECK-DEPOSITS TO D-CHECK-DEPOSIT.
           MOVE TOT-CHECKS TO D-CHECKS.
           MOVE TOT-OVER-SHORT TO D-OVER-SHORT.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE RCLIST-QUEUE-POS TO Q-POS.
           MOVE RCLIST-COPIES-POS TO C-POS.
           MOVE RCLIST-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF BEG-DATE = 0
              MOVE 19000101 TO BEG-DATE.

           MOVE 0 TO PAGE-NUMBER.
       SET-HEAD.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS............" TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-RC2-FILE.

       SETUP-DATES.

           MOVE STAT1      TO RC-STATUS
                              QRC2-WBEG-STATUS
           MOVE STAT2      TO QRC2-WEND-STATUS.
           MOVE BEG-BRANCH TO RC-BRNO
                              QRC2-WBEG-BRNO
           MOVE END-BRANCH TO QRC2-WEND-BRNO.

           MOVE BEG-DATE            TO RC-TRANS-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QRC2-WBEG-TRANS-DATE.
           MOVE END-DATE            TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QRC2-WEND-TRANS-DATE.

           PERFORM START-RC2-FILE.

           MOVE 0 TO TOT-DEPOSITS TOT-CHECKS TOT-OVER-SHORT
                     TOT-CHECK-DEPOSITS.

       EXIT-INITIALIZE.
           EXIT.

      ***********************************
       EO-EXEC-AUDIT SECTION.
      ***********************************
           MOVE EXT-EOBUF-DATE1 TO BEG-DATE BEG-DEP-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE END-DEP-DATE.

      ***********************************
       ENTER-PARAMETERS SECTION.
      ***********************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              IF EXT-ROUTE-BUF = "00"
                 GO TO ENTER-DEP-DATE1
              ELSE
                 GO TO ENTER-STAT2.

       BR-01.
      *    MOVE LEGEND-BR-01 TO LEGEND.
      *    PERFORM SEND-LEGEND.
           MOVE "ENNN040BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-01.
           MOVE VDUNUM0 TO BEG-BRANCH.
       BR-02.
      *    MOVE LEGEND-BR-02 TO LEGEND.
      *    PERFORM SEND-LEGEND.
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
           GO TO ENTER-DATE1.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
       ENTER-DATE1.
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO ENTER-STAT1.
           MOVE "ERNM060DATE1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-DATE1.
           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.
       ENTER-DATE2.
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO ENTER-STAT1.
           MOVE "ERNM060DATE2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-DATE1.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ENTER-DATE2.
           MOVE VDU-DATE-YYYYMMDD TO END-DATE.
      *----------------------------------------------------------
       ENTER-STAT1.
           MOVE "E  A000STAT1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-DATE2.
           IF VDUSTATUS = "1>"
             GO TO EXIT-PARM.
           IF VDUSTATUS = "16"
             GO TO ALL-STATS.
           IF VDUSTATUS NOT = "00"
             GO TO ENTER-STAT1.
           IF VDUBUF < "A1"
              OR VDUBUF > "M1"
                 MOVE "PLEASE ENTER VALUE FROM A1 TO M1" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-STAT1.
           MOVE VDUBUF TO STAT1.
       ENTER-STAT2.
           MOVE "E  A000STAT2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-STAT1.
           IF VDUSTATUS = "1>"
             GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00"
             GO TO ENTER-STAT2.
           IF VDUBUF < "A1"
              OR VDUBUF > "M1"
                 MOVE "PLEASE ENTER VALUE FROM A1 TO M1" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-STAT2.
           MOVE VDUBUF TO STAT2.
           GO TO ENTER-DEP-DATE1.

       ALL-STATS.
           MOVE "S  A000STAT1." TO VDU.
           MOVE "A1" TO STAT1 VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A000STAT2." TO VDU.
           MOVE "M1" TO STAT2 VDUBUF.
           PERFORM SCREENIO.
       END-PARM.
      *    MOVE LEGEND-ACCEPT TO LEGEND.
      *    PERFORM SEND-LEGEND.
           MOVE SPACES TO MESS.
           PERFORM CALL-TMMSG.
       ENTER-DEP-DATE1.
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO EXIT-PARM.
           MOVE "ERNM060DEPDATE1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-DEP-DATES.
           IF VDUSTATUS = "1U" GO TO ENTER-DATE2.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-DEP-DATE1.
           MOVE VDU-DATE-YYYYMMDD TO BEG-DEP-DATE.
       ENTER-DEP-DATE2.
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO ENTER-STAT1.
           MOVE "ERNM060DEPDATE2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-DEP-DATE1.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-DEP-DATE2.
           MOVE VDU-DATE-YYYYMMDD TO END-DEP-DATE.
           GO TO EXIT-PARM.
      *----------------------------------------------------------
       ALL-DEP-DATES.

           MOVE "S  8080DEPDATE1." TO VDU.
           MOVE EXT-JULIAN-BEG TO BEG-DEP-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080DEPDATE2."   TO VDU.
           MOVE EXT-JULIAN-END TO END-DEP-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

       EXIT-PARM.
           EXIT.

      ***********************************
       DISPLAY-PARAMETERS SECTION.
      ***********************************

           MOVE "SNNN040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE END-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A000STAT1." TO VDU.
           MOVE STAT1 TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A000STAT2." TO VDU.
           MOVE STAT2 TO VDUBUF.
           PERFORM SCREENIO.

      ***********************************
       WRITE-DETAIL-LINE SECTION.
      ***********************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *******************************
       HEAD-PAGE SECTION.
      *******************************
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
           MOVE 5 TO LN-COUNT.
           MOVE 2 TO LN-FEED.
       END-HEAD-PAGE.
           EXIT.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      **************************************************
       FORM-RESET SECTION.
      **************************************************
           MOVE EXT-CONAME-CONAME TO RCLIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO RCLIST-SYSDATE.
           DISPLAY RCLIST-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE RCLIST-FORM-FIELDS TO WR-BUF.
           MOVE RCLIST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      **************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************
           MOVE RCLIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE RCLIST-HELP--DIR TO HELP-LINK-DIR.

      **************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/RCLIST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************************
       MISC-PARA SECTION.
      **************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************
       IO-ROUTINES SECTION.
      ****************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBRCGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBRC2RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ****************************************
       END-PROGRAM SECTION.
      ****************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.
       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE "SP/UTMENU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY RCLIST-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
