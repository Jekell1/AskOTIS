       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BB1535.
       DATE-WRITTEN.    03/19/2025.
      ***********************************************************************
      *                 (UP)
      *  DESCRIPTION:   CONVERT A15 BBFILE (BULLETIN BOARD)
      *
      * 2025.0319 BAH
      ***********************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT OBB-FILE ASSIGN TO OBB-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OBB1-KEY
                  ALTERNATE RECORD KEY OBB2-KEY = OBB-CATAGORY
                                                 OBB-SUBCATAGORY
                                                 OBB-NAME
                                                 WITH DUPLICATES
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBGB/GBFDPR.CPY".
       FD  OBB-FILE
           LABEL RECORDS ARE STANDARD.
      **************************************************************************
      * (LIBSP/SP01BB)
      * BULITEN BOARD FILE
      *
      *       KEY  #1 CATAGORY        KEY #2 CATAGORY
      *               SUBCATAGORY            SUBCATAGORY
      *               USERID                 NAME
      *               SEQNO
      * REV:
      *
      **************************************************************************
       78  OBB-SIZE                    VALUE 1291.
       01  OBB-REC.
           03  OBB1-KEY.
               05  OBB-CATAGORY        PIC X(04).
               05  OBB-SUBCATAGORY     PIC X(04).
               05  OBB-USERID          PIC X(10).
               05  OBB-SEQNO           PIC 9(06).
      *    -------------------------------------------------------------
           03  OBB-NAME                PIC X(20).
           03  OBB-LOGNAME             PIC X(10).
           03  OBB-POSTNAME            PIC X(10).
           03  OBB-OTHERBR             PIC X(10).
           03  OBB-POSTDATE            PIC 9(08)       COMP-3.
           03  OBB-POSTTIME            PIC 9(06)       COMP-3.
           03  OBB-PASSWORD            PIC X(08).
      *    -------------------------------------------------------------
           03  OBB-MESSAGE-AREA        PIC X(1200).
           03  OBB-MANAGER-LOCATION    REDEFINES OBB-MESSAGE-AREA.
               05  OBB-GROUP           PIC X(04).
               05  OBB-BRNO            PIC 9(4)    COMP-3.
               05  OBB-ACTION          PIC X(40).
               05  OBB-MEMO            PIC X(600).
               05  OBB-MEMOX REDEFINES OBB-MEMO.
                   07 OBB-MEMO-LINE    PIC X(60) OCCURS 10.
               05  FILLER             PIC X(553).

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)UP/BB1535.CBL S35 03/19/25-10:22:31 >".

       COPY "LIBSP/SP01BB.CPY".
       COPY "LIBSP/SP01BB_SQL.CPY".
       COPY "LIBSP/SPWSBB.CPY".

       01  OBB-PATH   PIC X(25) VALUE "/USR/TMP/BBFILE".

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 9(3)     VALUE 99.
           03  LN-WK            PIC 9(3)     VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

       01  WS-COBOL-PATH.
           03  FILLER          PIC X         VALUE "/".
           03  WS-RELEASE      PIC X(7)      VALUE SPACES.
           03  FILLER          PIC X(4)      VALUE "/CO/".
           03  WS-PROGRAM      PIC X(7)      VALUE SPACES.

       01  ERRCD               PIC X.

       01  REPORT-ONLY         PIC X.
       01  ACCEPT-BUF          PIC X    VALUE " ".
       01  COUNT-SKIPPED       PIC 9(6) VALUE 0.
       01  COUNT-DUPS          PIC 9(6) VALUE 0.
       01  COUNT-READ          PIC 9(6) VALUE 0.
       01  COUNT-WRITTEN       PIC 9(6) VALUE 0.

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************

       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(8)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(8)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(3)    VALUE " ".
           03  H-TITLE          PIC X(52)    VALUE
           "- BULLETIN BOARD A15 TO S35 UPGRADE -".


       01  DETAIL-LINE          PIC X(80) VALUE SPACES.

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(15).
           03  RANGE-SELECTION.
               05  RANGE-NUM    PIC 9(6).

       COPY "LIBGB/DATERW.CPY".
       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       01  BB1535-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBUP/BB1535_DEF.CPY".
       COPY "LIBUP/BB1535_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      /***************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUP/BB1535_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               OBB-FILE PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-BB1-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE OBB-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      **********************************************
       MAIN-MODULE SECTION.
      **********************************************

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BB1535" TO VDU-FORM-NAME.

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS BB1535-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

      D    STOP MESS.

      * A15 BBFILE WILL BE IN /USR/TMP/BBFILE

           CALL "C$TOLOWER" USING OBB-PATH, VALUE 8.

           PERFORM OPEN-BB1-FILE.
           PERFORM OPEN-OBB1-FILE.

           MOVE SPACES TO OBB-CATAGORY BB-SUBCATAGORY BB-USERID.
           MOVE 0 TO BB-SEQNO.

           PERFORM START-OBB1-FILE.

       NEXT-BB.
           PERFORM READ-OBB1-FILE-NEXT.
           IF IO-BAD
              GO TO END-ROUTINE.

           ADD 1 TO COUNT-READ.

           MOVE SPACES TO BB-REC.
         
           MOVE OBB-CATAGORY       TO BB-CATAGORY      
           MOVE OBB-SUBCATAGORY    TO BB-SUBCATAGORY   
           MOVE OBB-USERID         TO BB-USERID        
           MOVE OBB-SEQNO          TO BB-SEQNO         
           MOVE OBB-NAME               TO BB-NAME              
           MOVE OBB-LOGNAME            TO BB-LOGNAME           
           MOVE OBB-POSTNAME           TO BB-POSTNAME          
           MOVE OBB-OTHERBR            TO BB-OTHERBR           
           MOVE OBB-POSTDATE           TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           IF DATE-YYYYMMDD NOT NUMERIC
              MOVE 0     TO DATE-YYYYMMDD
           ELSE
           IF DATE-YYYYMMDD = 0 OR 20202020
              MOVE 0     TO DATE-YYYYMMDD.
      * CANT FIX BAD YEAR
           IF (DATE-YYYYMMDD-YYYY = 0) AND
               (DATE-YYYYMMDD-MM NOT = 0 OR DATE-YYYYMMDD-DD NOT = 0)
              MOVE 0 TO DATE-YYYYMMDD.
           IF (DATE-YYYYMMDD-YYYY NOT = 0) AND
                (DATE-YYYYMMDD-MM < 1 OR > 12)
              MOVE 12 TO DATE-YYYYMMDD-MM.
           MOVE DATE-YYYYMMDD          TO BB-POSTDATE.
           MOVE OBB-POSTTIME           TO BB-POSTTIME          
           MOVE OBB-PASSWORD           TO BB-PASSWORD          
           MOVE OBB-MESSAGE-AREA       TO BB-MESSAGE-AREA      


      * WHEN WE REMOVED THE DIRECTORY, WE HAD DUP PROGRAM NAMES
           PERFORM WRITE-BB1-FILE.
           IF IO-FG NOT = 0
              IF FILE-STAT = 22
                 PERFORM REWRITE-BB1-FILE
              ELSE
              GO TO IO-ERROR.

           ADD 1 TO COUNT-WRITTEN.

           GO TO NEXT-BB.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              GO TO END-PROGRAM.                                   

           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.


      ***********************************
       INITIALIZATION SECTION.
      ***********************************

           MOVE BB1535-QUEUE-POS TO Q-POS.
           MOVE BB1535-COPIES-POS TO C-POS.
           MOVE BB1535-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT-BAD
              GO TO ENDIT.

           MOVE " " TO ERRCD.

       OK-YN.
           MOVE " " TO ERRCD.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.

           IF F7-KEY
              GO TO ENDIT.
           IF NOT (ENTER-KEY OR DOWN-KEY)
              GO TO OK-YN.

           MOVE VDUBUF TO ACCEPT-BUF.

           IF ACCEPT-BUF = "N"
              GO TO OK-YN.
           IF ACCEPT-BUF NOT = "Y"
              GO TO OK-YN.


           MOVE " " TO ERRCD.
           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           MOVE "LISTING IN PROGRESS...................." TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.

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

       END-HEAD-PAGE.
           MOVE 4 TO LN-COUNT.
           MOVE 2 TO LN-FEED.


      *******************************
       PRINT-RANGE-LINES SECTION.
      *******************************
           MOVE 4 TO LN-FEED.
           MOVE "REPORT ONLY" TO RANGE-LINE.
           MOVE REPORT-ONLY  TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.
           MOVE "READ" TO RANGE-LINE.
           MOVE COUNT-READ  TO RANGE-NUM.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "SKIPPED" TO RANGE-LINE.
           MOVE COUNT-SKIPPED  TO RANGE-NUM.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "WRITTEN" TO RANGE-LINE.
           MOVE COUNT-WRITTEN  TO RANGE-NUM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO BB1535-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BB1535-SYSDATE.
           DISPLAY BB1535-SCREEN UPON BB1535-WINDOW-HANDLE.
           MOVE BB1535-FORM-FIELDS TO WR-BUF.
           MOVE BB1535-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE BB1535-HELP--FILE TO HELP-LINK-FILE.
           MOVE BB1535-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUP/BB1535_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ****************************************************
       MISC SECTION.
      ****************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".

      ****************************************************
       IO-ROUTINES SECTION.
      ****************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBSP/SPBBGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBSP/SPBB1IN.CPY".
       OPEN-OBB1-FILE.
           PERFORM OPEN-IT.
           MOVE OBB-PATH TO E-FILE.
           OPEN INPUT OBB-FILE.
           IF IO-FG = 8
              GO TO OPEN-OBB1-FILE.
           IF IO-FG = 7
              CLOSE OBB-FILE
              GO TO OPEN-OBB1-FILE.
       READ-OBB1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OBB-PATH TO E-FILE.
           MOVE OBB1-KEY TO E-KEYX.
           READ OBB-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OBB1-FILE-NEXT.
       START-OBB1-FILE.
           PERFORM START-IT.
           MOVE OBB-PATH TO E-FILE.
           MOVE OBB1-KEY TO E-KEYX.
           START OBB-FILE KEY NOT < OBB1-KEY.
       CLOSE-OBB1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OBB-FILE.
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ****************************************************
       END-PROGRAM SECTION.
      ****************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.

           IF FIRST-TIME = "Y"
              MOVE "NO RECORDS FOUND" TO MESS
              PERFORM SEND-MESS.
       EXIT-PROG.
           MOVE "UP/UPMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN |MJD001
           DESTROY BB1535-SCREEN.
           DESTROY BB1535-WINDOW-HANDLE.
           EXIT PROGRAM.
