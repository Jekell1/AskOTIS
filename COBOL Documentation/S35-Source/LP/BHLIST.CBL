       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BHLIST.
       DATE-WRITTEN.    041901.
      ****************************************************************
      *
      *    DESCRIPTION:  BULLETIN BOARD HISTORY
      *
      *
      * REV:
      * CS  NEW    LISTING TO PRINT OFF BULLETIN BOARD HISTORY WORLD #260
      * CS  010515 ADDED PRINT OF BRANCH OR GROUP LOCATION
      * BLV 010720 CHANGED BH-FILE TO BE IN DESCENDING DATE ORDER,
      *            WORLD PR# 272
      * BAH 170508 ACTIVATE 8 DIGIT DATES, PD0006
      *
      * KEC 2018.0423 PR#00000 PARADATA A30 JAY & BETH
      *         REMOVED USE OF LOCAL-PRINT-FG SINCE NOT USED;
      *         LOCAL-PRINT-FG NOT BEING SET; IN EOBUF WHICH IS EXTERNAL
      *         WHERE IF NOT BEING SET WILL CAUSE ISSUES FROM THAT
      *         LOCAL-PRINT-FG BYTE POSITION COULD BE SET FROM PREVIOUS
      *         PROGRAM WITH EOBUF THAT SET SOMETHING IN THAT BYTE.
      *
      ****************************************************************
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

      /
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)LP/BHLIST S35 06/21/24-14:10:24 >".
      ****************************
      *     F I L E   I F N      *
      ****************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBSP/SP01BB.CPY".
       COPY "LIBSP/SP01BB_SQL.CPY".
       COPY "LIBSP/SPWSBB.CPY".
       COPY "LIBSP/SP01BH.CPY".
       COPY "LIBSP/SP01BH_SQL.CPY".
       COPY "LIBSP/SPWSBH.CPY".

       01  PR-IFN               PIC XX      VALUE "PR".

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 99       VALUE 0.
           03  LN-WK            PIC 99       VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  USERID.
           03  FILLER         PIC X.
           03  USERID-BR      PIC 9(4).
           03  USER-GROUP     PIC X(3).
           03  FILLER         PIC X(2).
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  MSEL-LIST            PIC X(5)     VALUE "MSEL.".
       01  FIRST-TIME-PRINT-USER           PIC X    VALUE "Y".

      * INPUT DATES SUBTRACTED FROM 9'S
       01  BEG-HISTORY-DT       PIC 9(8).
       01  END-HISTORY-DT       PIC 9(8).

       78  AM-CMD                VALUE X"616D".
       78  PM-CMD                VALUE X"706D".

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
      *STORED FOR BATCH

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-HISTORY-DATE     PIC 9(8).
           03  END-HISTORY-DATE     PIC 9(8).
           03  GROUP-FLAG           PIC X.
           03  ENT-GROUP            PIC X(4).
           03  TOTAL-LEV-TABLE.
               05  TOTAL-LEVEL-TABLE PIC X OCCURS 10 TIMES.

       COPY "LIBGB/PASSWDW.CPY".
       01  VERSION-CONTROL     PIC X        VALUE "A".

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(30)     VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
      *    03  FILLER           PIC X(11)    VALUE " ".
           03  FILLER           PIC X(20)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(25)     VALUE " ".
           03  FILLER           PIC X(41)    VALUE
           "SUPERVISOR ACTIVITY REPORT               ".

       01  HEAD3.
           03  FILLER           PIC X(15) VALUE "USERID ".
           03  FILLER           PIC X(25) VALUE "NAME".
           03  FILLER           PIC X(13) VALUE "DATE".
           03  FILLER           PIC X(12) VALUE "TIME".
           03  FILLER           PIC X(8)  VALUE "LOC".
           03  FILLER           PIC X(8)  VALUE "ACTIVITY".
           03  FILLER           PIC X(51) VALUE SPACES.

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-USERID       PIC X(10).
           03  FILLER         PIC X(5).
           03  D-NAME         PIC X(20).
           03  FILLER         PIC X(5).
           03  D-DATE         PIC 99/99/99.
           03  FILLER         PIC X(5).
           03  D-TIME         PIC X(8).
           03  FILLER         PIC X(4).
           03  D-LOC          PIC X(4).
           03  FILLER         PIC X(4).
           03  D-ACTION       PIC X(40).
           03  FILLER         PIC X(19).

       01  LEGEND-GROUP-ID.
           03  FILLER           PIC X(39)  VALUE
               "F1, ENTER GROUP TO REPORT ON :     ".
       01  LEGEND-HISTORY-01.
           03  FILLER           PIC X(39)  VALUE
               "^, V, ENTER BEGINNING HISTORY DATE : ".
       01  LEGEND-HISTORY-02.
           03  FILLER           PIC X(36)  VALUE
               "^, V, ENTER ENDING HISTORY DATE : ".

       01  LEGEND             PIC X(70).

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBWI/GRTOTLW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".


       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/BHLIST_DEF.CPY".
       COPY "LIBLP/BHLIST_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      /***************************************
      *                                      *
      *     P R O G R A M   C O N T R O L    *
      *                                      *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/BHLIST_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-BH1-FILE.
           PERFORM CLOSE-BB1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BHLIST" TO FORM-PROG VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           PERFORM GET-BB-MEMBER.

           GO TO END-PROGRAM.

      ******************************************************************
       GET-BB-MEMBER SECTION.
      ******************************************************************

           PERFORM OPEN-BB1-FILE.

           MOVE SPACES TO BB-CATAGORY BB-SUBCATAGORY BB-USERID.
           MOVE ZEROES TO BB-SEQNO.
           PERFORM START-BB1-FILE.

           PERFORM OPEN-BH1-FILE.
           MOVE SPACES TO BH-USERID.
           MOVE ZEROES TO BH-DATE-REVERSED BH-TIME.
           PERFORM START-BH1-FILE.

      *----------------------------------------------------------------
       GET-BB-MEMBER-NEXT.

           PERFORM READ-BB1-FILE-NEXT.
           IF IO-BAD
              GO TO GET-BB-MEMBER-END.

           MOVE "Y" TO FIRST-TIME-PRINT-USER.

           MOVE BB-USERID          TO USERID.
           CALL "C$TOUPPER" USING USER-GROUP, VALUE 3.
           MOVE USER-GROUP         TO WGR-TEST-GROUP.

           PERFORM VERIFY-GROUP-MEMBER.
           IF WGR-ERR NOT = 0
              GO TO GET-BB-MEMBER-NEXT.

           MOVE USER-GROUP         TO GR-ID.
           PERFORM READ-GR1-FILE.
           IF TOTAL-LEVEL-TABLE(GR-LEVEL) NOT = "Y"
              GO TO GET-BB-MEMBER-NEXT.

       GET-BH-HISTORY.
           MOVE BB-USERID       TO BH-USERID.
      * ALREADY SUBTRACTED FROM 9'S
           MOVE END-HISTORY-DT  TO BH-DATE.
           MOVE ZEROS           TO BH-TIME.
           PERFORM START-BH1-FILE.

       READ-BH-NEXT.
           PERFORM READ-BH1-FILE-NEXT.
           IF IO-BAD
              GO TO GET-BB-MEMBER-NEXT.

           IF BH-DATE > BEG-HISTORY-DT
              GO TO GET-BB-MEMBER-NEXT.

           IF BH-USERID NOT = BB-USERID
              GO TO GET-BB-MEMBER-NEXT.

           PERFORM DETAIL-LINE-PROCESS.

           MOVE "N" TO FIRST-TIME.

           GO TO READ-BH-NEXT.

      *=================================================================
       GET-BB-MEMBER-END.

           PERFORM CLOSE-BB1-FILE.
           PERFORM CLOSE-BH1-FILE.

       GET-BB-MEMBER-EXIT.
           EXIT.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE BHLIST-QUEUE-POS  TO Q-POS.
           MOVE BHLIST-COPIES-POS TO C-POS.
           MOVE BHLIST-PRU-POS    TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS....................." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           MOVE BEG-HISTORY-DATE TO DATE-YYYYMMDD..
           COMPUTE BEG-HISTORY-DT = 99999999 - DATE-YYYYMMDD.

           MOVE END-HISTORY-DATE TO DATE-YYYYMMDD.
           COMPUTE END-HISTORY-DT = 99999999 - DATE-YYYYMMDD.

           PERFORM OPEN-PR-FILE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      *****************************************
       EO-EXEC-AUDIT SECTION.
      *****************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-HISTORY-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-HISTORY-DATE.

      *************************************************
       ENTER-PARAMETERS SECTION.
      *************************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO DT-02.

      *----------------------------------------------------------------
       ENTER-GROUPID.
           MOVE LEGEND-GROUP-ID TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A000GRID." TO VDU.
           PERFORM SCREENIO.

           IF F1-KEY  GO TO EXIT-PARM.
           IF F7-KEY  GO TO EXIT-PARM.

           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO ENTER-GROUPID
              ELSE
                 MOVE "S  A000GRID." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF
                 PERFORM SCREENIO.

           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-GROUPID.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP GR-ID.
         
           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-GROUPID.

      *VERIFY GROUP EXISTS
           PERFORM OPEN-GR1-FILE.
           PERFORM READ-GR1-FILE.
           PERFORM CLOSE-GR1-FILE.
           IF IO-BAD
              MOVE "INVALID GROUP-ID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-GROUPID.

      *---------------------------------------------------------------

       DT-01.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO CHECK-DETAIL-PRINT.
           MOVE LEGEND-HISTORY-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ERNM060DT1." TO VDU.
           PERFORM SCREENIO.

           IF F1-KEY GO TO EXIT-PARM.
           IF F3-KEY GO TO ALL-DATES.
           IF UP-KEY GO TO ENTER-GROUPID.
           IF F7-KEY GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DT-02.

           MOVE VDU-DATE-YYYYMMDD TO BEG-HISTORY-DATE.

       DT-02.
           MOVE LEGEND-HISTORY-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ERNM060DT2." TO VDU.
           PERFORM SCREENIO.

           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO DT-01.
           IF F7-KEY GO TO EXIT-PARM.

           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DT-02.
           MOVE VDU-DATE-YYYYMMDD TO END-HISTORY-DATE.

           IF END-HISTORY-DATE < BEG-HISTORY-DATE
               MOVE "INVALID DATE RANGE" TO MESS
               PERFORM SEND-MESS
               GO TO DT-01.

           GO TO OK-YN.

       ALL-DATES.
           MOVE "S  8080DT1." TO VDU.
           MOVE EXT-JULIAN-BEG TO BEG-HISTORY-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080DT2." TO VDU.
           MOVE EXT-JULIAN-END TO END-HISTORY-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

       OK-YN.
           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.

           ADD 1 EXT-CONAME-RPT-LINES GIVING LN-COUNT.

       CHECK-DETAIL-PRINT.
           PERFORM GRTOTL-WINDOW.

       EXIT-PARM.
           EXIT.

      ***************************************
       DISPLAY-PARAMETERS SECTION.
      ***************************************

           MOVE "S  A000GRID." TO VDU.
           MOVE ENT-GROUP TO VDUBUF GTT-SV-ENT-GROUP.
           PERFORM SCREENIO.


      ***************************************
       DETAIL-LINE-PROCESS SECTION.
      ***************************************
           IF FIRST-TIME-PRINT-USER = "Y"
             MOVE BH-USERID TO D-USERID
             MOVE BB-NAME   TO D-NAME
             MOVE EXT-CONAME-RPT-LINES TO LN-COUNT.

           COMPUTE DATE-YYYYMMDD = 99999999 - BH-DATE.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-DATE.
           PERFORM CONVERT-TIME.
           MOVE TIME-EDIT TO D-TIME.
           MOVE BH-ACTION TO D-ACTION.
           MOVE BH-GROUP  TO D-LOC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "N"       TO FIRST-TIME-PRINT-USER.

       END-DETAIL-LN-PROCESS.
           EXIT.

      ***************************************
       WRITE-DETAIL-LINE SECTION.
      ***************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ***************************************
       HEAD-PAGE SECTION.
      ***************************************
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
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 2 TO LN-FEED.
           MOVE 7 TO LN-COUNT.


      ***************************************
       CONVERT-TIME SECTION.
      ***************************************
           MOVE BH-TIME TO H-AND-M.
           MOVE 0 TO CUR-SS CUR-HD.
           IF H-AND-M NOT GREATER THAN 1159
              MOVE AM-CMD TO T-AMPM
           ELSE
              MOVE PM-CMD TO T-AMPM.
           IF H-AND-M IS NOT LESS THAN 1300
              SUBTRACT 1200 FROM H-AND-M.
           IF CUR-HH = 0
              MOVE 12 TO CUR-HH.
           MOVE CUR-HH TO T-HH.
           MOVE CUR-MM TO T-MM.
           MOVE ":" TO T-COL.
           MOVE " " TO T-SPACE.

      ********************************************
       FORM-RESET SECTION.
      ********************************************
           MOVE EXT-CONAME-CONAME TO BHLIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BHLIST-SYSDATE.
           DISPLAY BHLIST-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE BHLIST-FORM-FIELDS TO WR-BUF.
           MOVE BHLIST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ********************************************
       SETUP-LINK-INFO SECTION.
      ********************************************
           MOVE BHLIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE BHLIST-HELP--DIR  TO HELP-LINK-DIR.

      ********************************************
       VDU-CONVERT-FIELD SECTION.
      ********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/BHLIST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBWI/GRTOTL.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".

      ***************************************
       MISC-PARAGRAPHS SECTION.
      ***************************************
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".

       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      ****************************************
       IO-ROUTINE SECTION.
      ****************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBSP/SPBHGS_SQL.CPY".
       COPY "LIBSP/SPBBGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBSP/SPBB1RN.CPY".
       COPY "LIBSP/SPBH1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ****************************************
       END-PROGRAM SECTION.
      ****************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
       EXIT-PROG.
           IF EXT-ROUTE-BUF  = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY BHLIST-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
