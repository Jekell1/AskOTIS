       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LONIA0.
       DATE-WRITTEN.    011884.
      ******************************************************************
      *                 (LP)
      *  DESCRIPTION:   GET TRANSACTION DATE
      *                 (CONVERSIONS)
      *
      * REV:
      * MJD 102196 ADDED TEST FOR OPEN DAY. IF DAY IS NOT OPEN THEN
      *            USER IS NOT ALLOWED TO CONVERT ON THAT DATE.
      *            REMOVED OPTION TO CHANGE BRANCH NUMBER.
      *  CS 991007 ADDED TEST OF BULK-CONVERSION FLAG TO DISPLAY EITHER
      *            BULK OR CONVERSION
      * BAH 050510 ADDED OPEN-OP-FILE TO RESERVE OPEN FLAG WHILE IN CONVERSION
      *            SO LOAN MANAGER CANT CLOSE THE DAY AND MESS UP STATS
      *            AND GL, REGMGM PL# 488
      *  CS 150421 CHANGED TEST ON OPEN/CLOSED/UPDATED DAYS TO RED THE RC FILE
      *            LEFT THE OPEN-OP  FOR LOCKING. SAVED VERSION IS
      *            DATED LONIA0.150421
      *
      * KEC 2015.1120 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED RC-TRANS-DATE FROM YYYMMDD TO CCYYMMDD.
      *  CS 170506 ACTIVATE 8 DIGIT DATES
      *
      * JKC 2024-0201 ADDED OTIS TRACE LOGGING LOGIC
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBLP/LPFSWK.CPY".
       COPY "LIBLP/LPFSOP.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBLP/LPFDWK.CPY".
       COPY "LIBLP/LPFDOP.CPY".
      *************************************
      *   P R O G R A M   W O R K E R S   *
      *************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/LONIA0.CBL S35 02/01/24-10:11:32 >".
       COPY "LIBGB/GB01RC.CPY".
       COPY "LIBGB/GB01RC_SQL.CPY".
       COPY "LIBGB/GBWSRC.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       01  WK-IFN                      PIC X(4) VALUE "B5WK".
       01  WK-KEY                      PIC 99.

       01  TODAYS-DATE         PIC 9(8).

       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPMNUIW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBLP/LPEDPATH.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/LONIA_DEF.CPY".
       COPY "LIBLP/LPCMNU_DEF.CPY".
       COPY "LIBLP/LONIA_WKS.CPY".
       COPY "LIBLP/LPCMNU_WKS.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBLP/LPLONIW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/LONIA_SCN.CPY".
       COPY "LIBLP/LPCMNU_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME PROG-BUF.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               WK-FILE OP-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-RC1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ********************************
      *  C O N T R O L   M O D U L E *
      ********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LONIA" TO VDU-FORM-NAME.

           PERFORM FORM-RESET.

           MOVE "S  A290BULKCONV." TO VDU.
           IF BULK-CONVERSION-FG = "B"
              MOVE "B U L K   P U R C H A S E" TO VDUBUF
           ELSE
              MOVE "L O A N   C O N V E R S I O N" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A160TITLE." TO VDU.
           IF BULK-CONVERSION-FG = "B"
               MOVE "BULK PURCHASE" TO VDUBUF
           ELSE
               MOVE "LOAN CONVERSION" TO VDUBUF.
           PERFORM SCREENIO.

           IF ROUTE-CD = "D"
              MOVE "TRANSACTION DATE CLOSED !" TO MESS
              PERFORM SEND-MESS
              MOVE 0 TO TRANS-DATE MONTH-DATE.

           IF TRANS-DATE NOT = 0
              MOVE "TRANSACTION DATE ERROR !" TO MESS
              PERFORM SEND-MESS
              MOVE "E" TO ERRCD
              GO TO STOP-RUN.

           IF EXT-CONAME-USER-LOC NOT NUMERIC
              MOVE "INVALID USER ID FOR LOAN PROCESSING !" TO MESS
              PERFORM SEND-MESS
              MOVE "E" TO ERRCD
              GO TO STOP-RUN.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO TODAYS-DATE TRANS-DATE
                                             MONTH-DATE.     
           

           MOVE EXT-CONAME-USER-LOC TO BRANCH.
           GO TO SEND-BRANCH.

       ENTER-BRANCH.
           MOVE "ENNN040BR." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "X" TO ERRCD
              GO TO STOP-RUN.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-BRANCH.

           IF VDUNUM0 = 0
              MOVE BRANCH TO VDUNUM0.

           MOVE VDUNUM0 TO BRANCH.
       SEND-BRANCH.
           MOVE "S  A040BR." TO VDU.
           MOVE BRANCH TO VDUBUF BR-NO.
           PERFORM SCREENIO.

           PERFORM OPEN-BR-FILE.
           PERFORM READ-BR-FILE.
           PERFORM CLOSE-BR-FILE.
           IF IO-FG = 9
              MOVE "INVALID BRANCH !" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BRANCH.

           MOVE "S  A040CONAME." TO VDU.
           MOVE BR-NAME TO VDUBUF LPLONI-CONAME.
           PERFORM SCREENIO.

       ENTER-TRANS-DATE.
           MOVE "ERNM080TRANSDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "X" TO ERRCD
              GO TO STOP-RUN.

           IF VDUSTATUS NOT = "00"
              GO TO ENTER-TRANS-DATE.

           IF VDUNUM0 = 0
              MOVE TODAYS-DATE TO VDU-DATE-YYYYMMDD.

           MOVE VDU-DATE-YYYYMMDD TO TRANS-DATE NUM-DATE.
           PERFORM TSTDAT.
           IF NUM-DATE = 0
              MOVE "INVALID DATE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-TRANS-DATE.

           MOVE "S  8000TRANSDATE." TO VDU.
           MOVE TRANS-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "S  8000SYSDATE." TO VDU.
           MOVE TRANS-DATE TO DATE-YYYYMMDD.    
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO VDU-DATE-EDIT LPLONI-SYSDATE.
           PERFORM SCREENIO.

      *    -------------------------------------------------------------
      *    ONLY ALLOW THEN TO ENTER A DATE THAT HAS BEEN OPENED.
      *    CLOSED, UPDATED, OR UNOPENED DAYS ARE INVALID.
      *    -------------------------------------------------------------

           MOVE BRANCH     TO OPEN-PATH-BRNO.
           MOVE TRANS-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO OPEN-PATH-TRDATE.

           PERFORM LOAD-OPEN-FILE.

           MOVE BRANCH     TO RC-BRNO.
           MOVE TRANS-DATE TO RC-TRANS-DATE.

           PERFORM OPEN-RC1-FILE.
           PERFORM READ-RC1-FILE.
           PERFORM CLOSE-RC1-FILE.
           IF IO-BAD   
              MOVE "DAY IS NOT OPEN FOR LOAN PROCESSING!" TO MESS
              PERFORM SEND-MESS
              MOVE 0 TO TRANS-DATE
              GO TO ENTER-TRANS-DATE   
           ELSE
              IF RC-STATUS = "B1"
                 MOVE "TRANSACTION DATE CLOSED !" TO MESS
                 PERFORM SEND-MESS
                 MOVE 0 TO TRANS-DATE
                 GO TO ENTER-TRANS-DATE 
              ELSE
                 IF RC-STATUS > "B1"
                    MOVE "TRANSACTION DATE UPDATED !" TO MESS
                    PERFORM SEND-MESS
                    MOVE 0 TO TRANS-DATE
                    GO TO ENTER-TRANS-DATE.

      * ONLY HERE IF RC-STATUS = "A1" - DAY OPEN

      * TEST FOR OPEN TAG.

           MOVE OPEN-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "TRANSACTION DATE HAS NOT BEEN OPENED !" TO MESS
              PERFORM SEND-MESS
              MOVE 0 TO TRANS-DATE
              GO TO ENTER-TRANS-DATE.


       ENTER-MONTH-DATE.
           MOVE "ERNM080MONTHDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ENTER-TRANS-DATE.
           IF VDUSTATUS = "1>"
              MOVE "X" TO ERRCD
              GO TO STOP-RUN.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-MONTH-DATE.

           IF VDUNUM0 = 0
              MOVE MONTH-DATE TO DATE-YYYYMMDD NDTE-DATE.

           MOVE VDU-DATE-YYYYMMDD TO MONTH-DATE NDTE-DATE.          

      * MOVE 33 TO GET LAST DAY OF MONTH
           MOVE 33 TO NDTE-DD.
           PERFORM NEW-DATE-CALCULATION.
           IF NDTE-DATE NOT = VDU-DATE-YYYYMMDD
              MOVE "MUST ENTER VALID MONTH END DATE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-MONTH-DATE.
           MOVE "S  8000MONTHDATE." TO VDU.
           MOVE MONTH-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           PERFORM LOAD-OPEN-FILE.
           MOVE OPEN-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT NOT = "00" )
              PERFORM OPEN-OP-FILE-OUTPUT
              PERFORM CLOSE-OP-FILE.

           PERFORM OPEN-OP-FILE.

           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LONIA-SCREEN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.

       LOAN-MENU.
           PERFORM MENU-SELECTION.

           IF MENU-FG = 99
              MOVE "MSEL." TO MESS
              PERFORM RESET-MESS
              GO TO ENTER-TRANS-DATE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.

      * WK-FILE FOR LOAN PROCESSING USES PROCESS ID XL(PROCESS ID)
           MOVE PROCESS-ID-BUF TO USER-PID TEMP-PID.
           MOVE 9 TO IO-FG.
           PERFORM OPEN-WK-FILE.

           GO TO END-ROUTINE.

       COPY "LIBLP/LPMNUI.CPY".

       COPY "LIBCL/TRACE.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *******************************
       FORM-RESET SECTION.
      *******************************
           MOVE LPLONI-CONAME TO LONIA-CONAME.
           MOVE LPLONI-SYSDATE TO LONIA-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY LONIA-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE LONIA-FORM-FIELDS TO WR-BUF.
           MOVE LONIA-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************
       SETUP-LINK-INFO SECTION.
      *******************************
           MOVE LONIA-HELP--FILE TO HELP-LINK-FILE.
           MOVE LONIA-HELP--DIR TO HELP-LINK-DIR.

      *******************************
       VDU-CONVERT-FIELD SECTION.
      *******************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LONIA_EVA.CPY".
              COPY "LIBLP/LPCMNU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *******************************
       MISC SECTION.
      *******************************
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".

      *******************************
       IO-ROUTINE SECTION.
      *******************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBRCGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBRC1RN.CPY".
       COPY "LIBLP/LPEDPATHIO.CPY".
       COPY "LIBLP/LPOPIN.CPY".

      **********************************
      *   WKFILE - OPEN OUTPUT         *
      **********************************
       OPEN-WK-FILE.
           PERFORM OPEN-IT.
           MOVE WK-IFN TO E-FILE.
           OPEN OUTPUT WK-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-FILE.
           IF IO-FG = 7
              PERFORM CLOSE-WK-FILE
              GO TO OPEN-WK-FILE.
       CLOSE-WK-FILE.
           MOVE "CLOSE" TO E-MSG.
           CLOSE WK-FILE.

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      **********************************
       END-ROUTINE SECTION.
      **********************************
           PERFORM CLOSE-FILES.
       STOP-RUN.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-FINISH' SO THAT IT CAN GET THE EXACT
      *    PROGRAM NAME IN THE TRACE LOG
      *    *NOTE* YOU MAY GET DOUBLE "PROGRAM FINISH" LINES IN THE LOG
      *    DUE TO ONE BEING CREATED IN 'PERFORM CLOSE-FILES' AND THIS
      *    ONE WHICH FOR ANY 'GO TO STOP-RUN'
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "LONIA0"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-FINISH-STOP.

       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LONIA-SCREEN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           EXIT PROGRAM.
