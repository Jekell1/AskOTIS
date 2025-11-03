       IDENTIFICATION DIVISION.
       PROGRAM-ID.      FX1306.
       DATE-WRITTEN.   03/08/2018.
      *************************************************************************
      *    DIR :  FX
      *    DESC:  SET BRANCH FIELDS FOR USE OF DEBIT CARDS
      *
      *          SETS BR-USE-DEBIT-CARDS
      *          SETS BR-GL-DEBIT-CARD-PROCEEDS(GL#)
      *           BASED UPON YOUR SCREEN INPUT
      *
      * REV:
      *  KEC 20180308  NETSPEND, WORLD PR# 1306
      *************************************************************************
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
           "@(#)FX/FX1306.CBL A30 03/02/18-09:23:46 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
 
      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 1.
           03  LN-COUNT         PIC 999      VALUE 99.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.
 
      **********************
      *   MISC WORKERS     *
      **********************
       01  DISPLAY-MESS.
           03  FILLER           PIC X(25)    VALUE
           "FIX IN PROGRESS..........".
           03  DISPLAY-MESS-BR  PIC 9999.
           03  FILLER           PIC X        VALUE " ".
           03  DISPLAY-MESS-ST  PIC XX.
           03  FILLER           PIC X(7)     VALUE " ".

       01  DISPLAY-TOT-BUF.
           03  DISPLAY-TOTS     OCCURS 4.
               05  DISPLAY-TYPE  PIC X(20).     
               05  DISPLAY-COUNT PIC ZZZ,ZZZ,ZZZ.
       01  DISPLAY-TOT-LIST     PIC X(24)    VALUE
           "BRTOT RCTOT CATOT TRTOT.".

       01  FIRST-TIME           PIC X        VALUE "Y".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
           88  SKIP-RECORDS-FOUND            VALUE " ".

       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

       01  WORK-GLNO            PIC 9(11).
       01  WORK-GL              REDEFINES WORK-GLNO.
           03  WORK-GLBR        PIC 9(4).
           03  WORK-GLNUM       PIC 9(7).

       01  BRFILE-FIX-COUNT     PIC S9(9)    COMP-3 VALUE 0.

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH      PIC 9999.
           03  END-BRANCH      PIC 9999.
           03  SET-DEBIT-CARD  PIC X.
           03  SET-DEBIT-GL-NO PIC 9(7).
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).

       01  VERSION-CONTROL     PIC X VALUE "A".

       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-TIME           PIC X(8)     VALUE " ".
           03  FILLER           PIC X(28)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(37)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.
 
       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(26)    VALUE " ".
           03  H-TITLE          PIC X(39)    VALUE
           "  SET NETSPEND DEBIT BRANCH FIELDS     ".
           03  FILLER           PIC X(47)    VALUE  " ".
 
      ******************************************************************
      *
      *    D E T A I L - L I N E
      *
      ******************************************************************
       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(35).
           03  RANGE-SELECTION.
               04  R-NUM        PIC ZZZ,ZZZ,ZZZ.

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBFX/FX1306_DEF.CPY".
       COPY "LIBFX/FX1306_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
 
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBFX/FX1306_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
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
           MOVE "FX1306" TO FORM-PROG VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.
       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.
           IF WGR-SRESULT = 0
              GO TO END-ROUTINE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF NOT IO-GOOD
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-BAD OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.
       MOVE-BRANCH-PATH-INFO.
           IF PRU-LP NOT = "VDU" AND NOT = "PRU"
              IF EXT-ROUTE-BUF = "00"
                 MOVE BR-NO TO DISPLAY-MESS-BR
                 MOVE BR-STATE TO DISPLAY-MESS-ST
                 MOVE DISPLAY-MESS TO MESS
                 PERFORM SEND-LEGEND.

            PERFORM FIX-BR-FILE.               

       END-OF-GROUP.
           GO TO NEXT-GROUP-LOC.
 
       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM
           ELSE
              MOVE "Y" TO RECORDS-FOUND.

           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.
 
      ****************************
       FIX-BR-FILE SECTION.
      ****************************
           MOVE SET-DEBIT-CARD TO BR-USE-DEBIT-CARDS.

           MOVE 9990            TO WORK-GLBR.
           MOVE SET-DEBIT-GL-NO TO WORK-GLNUM.
           MOVE WORK-GLNO       TO BR-GL-DEBIT-CARD-PROCEEDS.
 
           PERFORM REWRITE-BR-FILE.

           ADD 1 TO BRFILE-FIX-COUNT.

           MOVE "N" TO FIRST-TIME.

       END-FIX-BR.
           EXIT.
 
      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE FX1306-QUEUE-POS TO Q-POS.
           MOVE FX1306-COPIES-POS TO C-POS.
           MOVE FX1306-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".
 
           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.

           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BRANCH TO SV-BEG-BR
              MOVE END-BRANCH TO SV-END-BR
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP TO SV-ENT-GROUP
              MOVE EXT-EOBUF-BRNO TO BEG-BRANCH END-BRANCH
              MOVE "N" TO GROUP-FLAG 
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE
           ELSE
              IF ( BEG-BRANCH = 0 AND END-BRANCH = 0 )
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH END-BRANCH.

       SET-HEAD.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           MOVE "FIX IN PROGRESS......." TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.
 
      ***********************************
       EO-EXEC-AUDIT SECTION.
      ***********************************

      ***********************************
       ENTER-PARAMETERS SECTION.
      ***********************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO BR-01.

       BR-01.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO BR-01
              ELSE
                 MOVE "S  A000BR1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-01.


           MOVE VDUBUF TO WGR-WORD ENT-GROUP.
           IF WGR-WORD-ZERO
              GO TO BRANCH-IS-NUMERIC.
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
              GO TO ENTER-SET-DEBIT-CARD.
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.
           IF BEG-BRANCH > END-BRANCH
              MOVE " INVALID BRANCH RANGE !" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           GO TO ENTER-SET-DEBIT-CARD.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

      *----------------------------------------------------------------
       ENTER-SET-DEBIT-CARD.
           MOVE "E  A010USEDB." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-SET-DEBIT-CARD.

           MOVE VDUBUF TO SET-DEBIT-CARD.
           IF NOT(SET-DEBIT-CARD = "Y" OR "N")
              GO TO ENTER-SET-DEBIT-CARD.
      *----------------------------------------------------------------
       ENTER-SET-DEBIT-GL-NO.
           MOVE "ENNN070DBGL."  TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-SET-DEBIT-CARD.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-SET-DEBIT-GL-NO.

           MOVE VDUNUM0 TO SET-DEBIT-GL-NO.
   
       EXIT-PARM.
           EXIT.

      ***********************************
       DISPLAY-PARAMETERS SECTION.
      ***********************************

           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BRANCH TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BRANCH TO VDUNUM0
              PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
           MOVE "S  A010USEDB." TO VDU.
           MOVE SET-DEBIT-CARD  TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
           MOVE "SNNN070DBGL." TO VDU.
           MOVE SET-DEBIT-GL-NO TO VDUNUM0
           PERFORM SCREENIO.

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************
           MOVE "Y" TO RECORDS-FOUND.
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE TIME-EDIT TO H-TIME.
 
           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE 2 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      ****************************
       WRITE-DETAIL-LINE SECTION.
      ****************************
           ADD LN-FEED TO LN-COUNT.
           IF LN-COUNT > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.
 
      *******************************
       DISPLAY-TOTALS SECTION.
      *******************************
           MOVE "BRANCHES FIXED    : " TO DISPLAY-TYPE(1).
           MOVE BRFILE-FIX-COUNT       TO DISPLAY-COUNT(1).
           PERFORM WRITE-DETAIL-LINE.
           MOVE DISPLAY-TOT-BUF TO WR-BUF.
           MOVE DISPLAY-TOT-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************
       PRINT-RANGE-LINES SECTION.
      *******************************
           MOVE 3 TO LN-FEED.
           MOVE "TOTAL BRANCHES FIXED     :" TO RANGE-LINE.
           MOVE BRFILE-FIX-COUNT             TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 4 TO LN-FEED.
           IF GROUP-FLAG = "Y"
              MOVE "BRANCH GROUP       :" TO RANGE-LINE
              MOVE ENT-GROUP              TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEGINNING BRANCH   :" TO RANGE-LINE
              MOVE BEG-BRANCH             TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING             :" TO RANGE-LINE
              MOVE END-BRANCH             TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
           MOVE "SET USE DEBIT CARD :" TO RANGE-LINE.
           MOVE SET-DEBIT-CARD         TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
           MOVE "SET DEBIT CARD GLNO:" TO RANGE-LINE.
           MOVE SET-DEBIT-GL-NO        TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

      *********************************************
       FORM-RESET SECTION.
      *********************************************
           MOVE EXT-CONAME-CONAME TO FX1306-CONAME.
           MOVE EXT-CONAME-SYSDATE TO FX1306-SYSDATE.
           DISPLAY FX1306-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE FX1306-FORM-FIELDS TO WR-BUF.
           MOVE FX1306-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           MOVE FX1306-HELP--FILE TO HELP-LINK-FILE.
           MOVE FX1306-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBFX/FX1306_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
 
      *******************************
       MISC-PARAGRAPHS SECTION.
      *******************************
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETPR.CPY".
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
 
      ****************************************
       IO-ROUTINE SECTION.
      ****************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRIN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.
 
      ****************************************
       END-PROGRAM SECTION.
      ****************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF ( SKIP-RECORDS-FOUND )
              GO TO EXIT-PROG.

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS
           ELSE
              PERFORM CLOSE-PR-FILE
              PERFORM DISPLAY-TOTALS
              MOVE "FIX COMPLETE" TO MESS
              PERFORM SEND-MESS.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
       EXIT-PROG.
           IF SV-BEG-BR NOT = 9999
              IF EXT-EOBUF-GROUP-FG = "Y"
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH
                 MOVE SV-GROUP-FLAG TO GROUP-FLAG
                 MOVE SV-ENT-GROUP TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN 
              DESTROY FX1306-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
