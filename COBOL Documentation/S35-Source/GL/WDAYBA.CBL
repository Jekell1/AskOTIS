       IDENTIFICATION     DIVISION.
       PROGRAM-ID.          WDAYBA.
       AUTHOR.                 BLM.
       DATE-WRITTEN.    12/12/2018.
      **************************************************************************
      *
      * DESCRIPTION:  UPDATE WORKDAY BANK NUMBER TO GL-NUMBER1
      *                WORLD PR# 1353
      *
      * INPUT FILE = ../../FTP/WORKDAY/BACODES   
      *   FULL PATH IS LOWERCASE
      *
      * REV:
      *  181212 BLM NEW FOR WORLD, PR# 1353
      *  199124  CS CHANGED TO UPDATE BRFILE FROM THE IMPORT BACODES
      *             (4 DIGIT BANKID)
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT BACODES-FILE ASSIGN BACODES-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  ACCESS SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
 
       COPY "LIBGB/GBFDPR.CPY".
       FD  BACODES-FILE
           LABEL RECORDS ARE STANDARD.
       01  BACODES-REC.
           03  BACODES-BRNO     PIC 9(4).
           03  FILLER           PIC X(3).
           03  BACODES-NUMBER   PIC 9(4).
           03  BACODES-ALPHA    REDEFINES BACODES-NUMBER
                                PIC X(4).

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)GL/WDAYBA.CBL S34 06/10/21-11:43:04 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       01  BACODES-PATH.
           03  BACODES-PATH-GLOBAL   PIC X(13).
           03  BACODES-FILENAME.
                05  FILLER           PIC X(12)  VALUE "FTP/WORKDAY/".
                05  FILLER           PIC X(7)   VALUE "BACODES".

       01  HOLD-SYSDATE         PIC 9(8).
         
      *-----------------------------------------------------------------
      * LINE WORKERS
      *-----------------------------------------------------------------
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.
 
       01  TOTAL-UPDATED        PIC 9(7)     VALUE 0.
       01  TOTAL-INVALID-BA     PIC 9(7)     VALUE 0.
       01  TOTAL-INVALID-BR     PIC 9(7)     VALUE 0.
       01  EXCEPTION-FG         PIC X        VALUE " ".

      *-----------------------------------------------------------------
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(04).
       01  VERSION-CONTROL     PIC X    VALUE "A".

       01  MESS-LIST            PIC X(05) VALUE "MESS.".

      *-----------------------------------------------------------------
      * PRINT BUFFERS
      *-----------------------------------------------------------------
       01  HEAD1.
           03  H-DATE           PIC X(08)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(08)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(08)    VALUE " ".
           03  FILLER           PIC X(05)     VALUE "PAGE:".
           03  H-PAGE-NO        PIC ZZZ9.
 
       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(12).
           03  FILLER           PIC X(18)    VALUE " ".
           03  FILLER           PIC X(30) VALUE
           "   WORKDAY BACODES IMPORT     ".
       01  HEAD3.
           03  FILLER           PIC X(11)    VALUE "BACODE".
           03  FILLER           PIC X(04)    VALUE SPACES.
           03  FILLER           PIC X(13)    VALUE "EXCEPTION".
 
 
       01  DETAIL-LINE          PIC X(80) VALUE SPACES.
       01  EXCEPTION-LINE REDEFINES DETAIL-LINE.
           03  D-BACODE         PIC X(11).
           03  FILLER           PIC X(4).
           03  D-EXCEPTION      PIC X(40).
       01  DETAIL-LINE1 REDEFINES DETAIL-LINE.
           03  D-COUNT          PIC Z(9).
           03  FILLER           PIC X(2).
           03  D-MSG1           PIC X(40).

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  RANGE-SELECTION  PIC X(30).
           03  FILLER           PIC X(05).
           03  RANGE1N          PIC Z(09)9.
           03  RANGE-DATE       PIC 9999/99/99 BLANK ZERO.
           03  FILLER           PIC X(10).
           03  FILLER           PIC X(02).
 
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGL/WDAYBA_DEF.CPY".
       COPY "LIBGL/WDAYBA_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
 
      ****************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBGL/WDAYBA_SCN.CPY".
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE BACODES-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE
                BACODES-FILE.
       COPY "LIBGB/DECLRP2.CPY".
 
      **********************
       MAIN-MODULE SECTION.
      **********************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "WDAYBA" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE EXT-FILPATH-GLOBAL TO BACODES-PATH-GLOBAL.
           CALL "C$TOLOWER" USING BACODES-PATH, VALUE 32.
           MOVE BACODES-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "BACODES FILE NOT FOUND" TO MESS
              PERFORM SEND-MESS
              GO TO END-PROGRAM.

           MOVE "PROCESSING BACODES" TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-BACODES-FILE.
   
       NEXT-BACODE.
           MOVE SPACES TO BACODES-REC.
           PERFORM READ-BACODES-FILE-NEXT.
           IF IO-FG NOT = 0 
              GO TO END-ROUTINE.

      * TEST G/L BRANCH / GROUP
           IF GROUP-FLAG = "Y"
              MOVE BACODES-BRNO TO WGR-SRESULT
              PERFORM SEARCH-GR-VERIFY
              IF WGR-ERR > 0
                 GO TO NEXT-BACODE.
           IF GROUP-FLAG NOT = "Y"
              IF BACODES-BRNO < BEG-BRANCH OR BACODES-BRNO > END-BRANCH
                    GO TO NEXT-BACODE.

           MOVE BACODES-BRNO TO BR-NO.         
           PERFORM READ-BR-FILE.               
           IF IO-BAD
              ADD 1 TO TOTAL-INVALID-BR
              MOVE "2" TO EXCEPTION-FG
              PERFORM PRINT-EXCEPTION
              GO TO NEXT-BACODE.
           IF BACODES-ALPHA NOT NUMERIC 
              ADD 1 TO TOTAL-INVALID-BA
              MOVE "B" TO EXCEPTION-FG
              PERFORM PRINT-EXCEPTION
              GO TO NEXT-BACODE.

            MOVE BACODES-BRNO TO BR-NO.    
            MOVE BACODES-NUMBER TO BR-WKDAY-BANKID.
            PERFORM REWRITE-BR-FILE.
            ADD 1 TO TOTAL-UPDATED.
            GO TO NEXT-BACODE.

       END-ROUTINE.
           PERFORM CLOSE-BACODES-FILE.

           PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE WDAYBA-QUEUE-POS TO Q-POS.
           MOVE WDAYBA-COPIES-POS TO C-POS.
           MOVE WDAYBA-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".
 
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-PR-FILE.
 
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO HOLD-SYSDATE.
               
           IF GROUP-FLAG = "Y"
              MOVE ENT-GROUP TO GR-ID
              PERFORM SEARCH-GR-INIT.

           GO TO INITIALIZE-EXIT.

       INITIALIZE-ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       INITIALIZE-EXIT.
           EXIT.

      *******************************************************
       EO-EXEC-AUDIT SECTION.
      *******************************************************

      *******************************************************
       ENTER-PARAMETERS SECTION.
      *******************************************************
       ENTER-PARAM.
      * FOR RE-ENTRY ON F4 FROM ACCEPT.
           IF VDUSTATUS = "1U"
              GO TO BR-02.

       BR-01.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF F3-KEY GO TO ALL-BR.
           IF F7-KEY GO TO EXIT-PARM.

           IF F6-KEY
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO BR-01
              ELSE
                 MOVE "S  A000BR1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF
                 PERFORM SCREENIO
                 MOVE GRSCAN-KEY TO VDUBUF
                 MOVE "00" TO VDUSTATUS.

           IF NOT ENTER-KEY GO TO BR-01.
           MOVE VDUBUF TO WGR-WORD ENT-GROUP.
           IF WGR-WORD = SPACES
              GO TO BR-01.
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
              GO TO EXIT-PARM.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.

           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
      *----------------------------------------------------------------
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO BR-01.
           IF UP-KEY GO TO BR-01.
           IF F3-KEY GO TO ALL-BR.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.
           IF BEG-BRANCH > END-BRANCH
              GO TO BR-01.
           GO TO EXIT-PARM.
      *----------------------------------------------------------------
       ALL-BR.
           MOVE "N" TO GROUP-FLAG.
           MOVE "SN N040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
       EXIT-PARM.
           EXIT.

      *************************************************
       DISPLAY-PARAMETERS SECTION.
      *************************************************
           MOVE "SNNN040BR1." TO VDU
           MOVE BEG-BRANCH TO VDUNUM0
           PERFORM SCREENIO
           MOVE "SNNN040BR2." TO VDU
           MOVE END-BRANCH TO VDUNUM0
           PERFORM SCREENIO.

      *******************************
       PRINT-EXCEPTION SECTION.
      *******************************
           MOVE BACODES-REC TO D-BACODE.
           IF EXCEPTION-FG = "2"
              MOVE "BRANCH RECORD MISSING FOR BRANCH"           
                                                TO D-EXCEPTION.
           IF EXCEPTION-FG = "B"
              MOVE "INVALID/BLANK BACODE"       TO D-EXCEPTION.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *******************************
       PRINT-RANGE-LINES SECTION.
      *******************************
           MOVE 2 TO LN-FEED.

           MOVE "BEGINNING BRANCH     "          TO RANGE-SELECTION
           MOVE BEG-BRANCH                       TO RANGE1N
           PERFORM WRITE-DETAIL-LINE
           MOVE "ENDING    BRANCH     "          TO RANGE-SELECTION
           MOVE END-BRANCH                       TO RANGE1N
           PERFORM WRITE-DETAIL-LINE.

           MOVE "RUN DATE" TO RANGE-SELECTION.
           MOVE HOLD-SYSDATE TO RANGE-DATE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE TOTAL-UPDATED TO D-COUNT.
           MOVE "RECORDS UPDATED " TO D-MSG1.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE TOTAL-INVALID-BR TO D-COUNT.
           MOVE "INVALID BRANCH RECORDS " TO D-MSG1.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE TOTAL-INVALID-BA TO D-COUNT.
           MOVE "INVALID BA RECORDS " TO D-MSG1.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
 
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
 
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
 
      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO WDAYBA-CONAME.
           MOVE EXT-CONAME-SYSDATE TO WDAYBA-SYSDATE.
           DISPLAY WDAYBA-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE WDAYBA-FORM-FIELDS TO WR-BUF.
           MOVE WDAYBA-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE WDAYBA-HELP--FILE TO HELP-LINK-FILE.
           MOVE WDAYBA-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGL/WDAYBA_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************************
       MISC-PARA SECTION.
      **************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ************************
       IO-ROUTINES SECTION.
      ************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       OPEN-BACODES-FILE.
           PERFORM OPEN-IT.
           MOVE BACODES-PATH TO E-FILE.
           OPEN INPUT BACODES-FILE.
           IF IO-FG = 8
              GO TO OPEN-BACODES-FILE.
           IF IO-FG = 7
              CLOSE BACODES-FILE
              GO TO OPEN-BACODES-FILE.
       READ-BACODES-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BACODES-PATH TO E-FILE.
           READ BACODES-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-BACODES-FILE-NEXT.
       CLOSE-BACODES-FILE.
           PERFORM CLOSE-IT.
           CLOSE BACODES-FILE.
       COPY "LIBGB/GBBRIN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.
 
      ************************
       END-PROGRAM SECTION.
      ************************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY WDAYBA-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
