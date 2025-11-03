       IDENTIFICATION DIVISION.
       PROGRAM-ID.      DRAWER.
       DATE-WRITTEN.    051206.
      ****************************************************************
      *                      (LS)
      *    DESCRIPTION:  LIST OPEN CASH DRAWERS
      *
      *** WRITTEN FOR REGACC
      *
      * REV:
      * BAH 060512 REGACC PR# 131
      * KEC 080212 {PR#00817} WORLD <A15> [ROSENAUER]
      *            ADDED OPTION TO SELECT CASH DRAWER/TILLNO
      *  CS  2013.1025 FIXED TO RUN IN BATCH
      *  BAH 2016.0304 CHANGED TILLNO TO NUMERIC 99
      *  BAH 2017.0425 ACTIVATE 8 DIGIT DATES, PD0006
      *  BAH 2019.0930 REMOVED ACCESS-CALL CAFILE
      *  BAH 2023.1005 CHANGED SCREEN FIELD USERID TO MUST Y TO ALLOW
      *                LOWERCASE
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

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LS/DRAWER.CBL J35 12/21/23-09:24:47 >".
       COPY "LIBLP/LP01CA.CPY".
       COPY "LIBLP/LP01CA_SQL.CPY".
       COPY "LIBLP/LPWSCA.CPY".
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
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 99       VALUE 60.
           03  LN-WK            PIC 99       VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      **********************
      *   MISC WORKERS     *
      **********************
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  MSEL-LIST            PIC X(5)     VALUE "MSEL.".

      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  DRAWER-DATE     PIC 9(8).
           03  OPEN-USERID     PIC X(8).
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).
           03  CASH-DRAWER     PIC 99.


       01  VERSION-CONTROL     PIC X        VALUE "B".

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(4)     VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(11)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(9)     VALUE " ".
           03  FILLER           PIC X(41)    VALUE
           "OPEN CASH DRAWER REPORT".

       01  HEAD3.
           03  FILLER           PIC X(12) VALUE "DATE".
           03  FILLER           PIC X(8) VALUE "BRNO".
           03  FILLER           PIC X(10) VALUE "DRAWER   ".
           03  FILLER           PIC X(13) VALUE "WHO OPENED".
           03  FILLER           PIC X(32)  VALUE "DESC".
           03  FILLER           PIC X(7)  VALUE "OPENED".
           03  FILLER           PIC X(7)  VALUE "CLOSED".


       01  DETAIL-LINE          PIC X(80) VALUE SPACES.

       01  D-LINE1 REDEFINES DETAIL-LINE.
           07  D-DATE         PIC 99/99/99.
           07  FILLER         PIC X(4).
           07  D-BRNO         PIC Z(4).
           07  FILLER         PIC X(4).
           07  D-TILLNO       PIC Z9.
           07  FILLER         PIC X(8).
           07  D-OPEN-USER    PIC X(10).
           07  FILLER         PIC X(3).
           07  D-DESC         PIC X(30).
           07  FILLER         PIC X(2).
           07  D-OPENED       PIC X(8).
           07  FILLER         PIC X(2).
           07  D-CLOSED       PIC X(8).


       01  LEGEND             PIC X(70).

       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBLP/EMSFRG_CMD.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/DRAWER_DEF.CPY".
       COPY "LIBLS/DRAWER_WKS.CPY".
      ****************************************
      *                                      *
      *     P R O G R A M   C O N T R O L    *
      *                                      *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/DRAWER_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-CA1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *************************************
       MAIN-MODULE SECTION.
      *************************************
           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE "DRAWER" TO FORM-PROG VDU-FORM-NAME.

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

           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

       MOVE-BRANCH-PATH-INFO.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO CA-PATH-OVERRIDE.

           PERFORM OPEN-CA1-FILE.

           MOVE BR-NO          TO CA-BRNO
                                  QCA1-WBEG-BRNO
                                  QCA1-WEND-BRNO.

      * ALL DATES
           IF DRAWER-DATE = 99999999
              MOVE 19000101    TO CA-DATE SQL-DATE-YYYYMMDD
           ELSE
              MOVE DRAWER-DATE TO CA-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QCA1-WBEG-DATE
                                       QCA1-WEND-DATE.
           IF DRAWER-DATE = 99999999
              MOVE EXT-JULIAN-END   TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCA1-WEND-DATE.

           MOVE 0              TO CA-TILLNO
                                  QCA1-WBEG-TILLNO.
           MOVE ALL "9"        TO QCA1-WEND-TILLNO.

           PERFORM START-CA1-FILE.

       NEXT-CA.
           PERFORM READ-CA1-FILE-NEXT.
           IF IO-FG = 9 OR (CA-PATH-OWNBR NOT = CA-BRNO)
              GO TO END-OF-GROUP.

           IF CA-DATE = 0
              GO TO NEXT-CA.

      * SKIP CLOSED DRAWERS
           IF CA-BALBY NOT = SPACES
              GO TO NEXT-CA.

      * VERIFY DATE
      * 99999999  = ALL DATES
           IF DRAWER-DATE NOT = 99999999
              IF CA-DATE NOT = DRAWER-DATE
                 GO TO NEXT-CA.

      * VERIFY CASH DRAWER
           IF CASH-DRAWER NOT = 99
              IF CA-TILLNO NOT = CASH-DRAWER
                 GO TO NEXT-CA.

      * VERIFY USER
           IF OPEN-USERID NOT = "********"
              IF CA-OPEN-USERID NOT = OPEN-USERID
                 GO TO NEXT-CA.


           PERFORM DETAIL-LINE-PROCESS.

           MOVE "N" TO FIRST-TIME.
           GO TO NEXT-CA.

       END-OF-GROUP.
           PERFORM CLOSE-CA1-FILE.
           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************

           MOVE DRAWER-QUEUE-POS TO Q-POS.
           MOVE DRAWER-COPIES-POS TO C-POS.
           MOVE DRAWER-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           IF EXT-EOBUF-ERRCD = "E" GO TO ENDIT.

       SET-HEAD.
           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS....................." TO MESS
              MOVE MESS TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ***********************************
       EO-EXEC-AUDIT SECTION.
      ***********************************
           MOVE EXT-EOBUF-DATE2 TO DRAWER-DATE.

      ***********************************
       ENTER-PARAMETERS SECTION.
      ***********************************
       ENTER-PARM.
           IF VDUSTATUS = "1U"
              GO TO OPEN-USER.

       BR-01.
           MOVE LEGEND-BR-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "R  A000BR1." TO VDU.
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
              MOVE 0 TO WGR-SRESULT
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
              GO TO CHECK-BRANCH-SECURITY.

       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.

       BR-02.
           MOVE LEGEND-BR-02 TO LEGEND.
           PERFORM SEND-LEGEND.
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
           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           GO TO CHECK-BRANCH-SECURITY.

       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A160DISPLAY-USER." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.
           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

       DT-01.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO ENTER-CASH-DRAWER.
           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ERNM060DT1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "16"
             MOVE "S  8000DT1." TO VDU
             MOVE 99999999 TO DRAWER-DATE VDU-DATE-YYYYMMDD
             PERFORM SCREENIO
             GO TO ENTER-CASH-DRAWER.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO DT-01.
           MOVE VDU-DATE-YYYYMMDD TO DRAWER-DATE.
           IF DRAWER-DATE = 0
              GO TO DT-01.

       ENTER-CASH-DRAWER.

           MOVE "EN N020TILLNO." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1U"
             IF EXT-ROUTE-BUF = "00"
                GO TO DT-01
             ELSE
                GO TO BR-02.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16"
               MOVE "SN N020TILLNO." TO VDU
               MOVE 99               TO VDUNUM0
                                        CASH-DRAWER
               PERFORM SCREENIO
               GO TO OPEN-USER.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-CASH-DRAWER.

           MOVE VDUNUM0 TO CASH-DRAWER.

           IF ( CASH-DRAWER = 0 OR 99)
              GO TO ENTER-CASH-DRAWER.

       OPEN-USER.
           MOVE "E  A080OPEN-USER." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-USERS.
           IF VDUSTATUS = "1U" GO TO ENTER-CASH-DRAWER.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO OPEN-USER.
           MOVE VDUBUF TO OPEN-USERID.

           GO TO EXIT-PARM.

       ALL-USERS.
           MOVE "S  A080OPEN-USER." TO VDU.
           MOVE "********" TO OPEN-USERID VDUBUF.
           PERFORM SCREENIO.

       EXIT-PARM.
           EXIT.

      ***************************
       DISPLAY-PARAMETERS SECTION.
      ***************************

           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF GTT-SV-ENT-GROUP
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

           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.


           MOVE "S  8080DT1." TO VDU.
           MOVE DRAWER-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "SN N020TILLNO." TO VDU.
           MOVE CASH-DRAWER TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A080OPEN-USER." TO VDU.
           MOVE OPEN-USERID TO VDUBUF.
           PERFORM SCREENIO.

      ****************************
       DETAIL-LINE-PROCESS SECTION.
      ****************************

           MOVE CA-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-DATE.
           MOVE CA-BRNO     TO D-BRNO.
           MOVE CA-TILLNO   TO D-TILLNO.

           MOVE CA-OPEN-USERID TO D-OPEN-USER.
           MOVE CA-DESC        TO D-DESC.
           MOVE CA-OPENED-HHMM TO H-AND-M.
           PERFORM CONVERT-TIME.
           MOVE TIME-EDIT      TO D-OPENED.
           MOVE CA-CLOSED-HHMM TO H-AND-M.
           PERFORM CONVERT-TIME.
           MOVE TIME-EDIT      TO D-CLOSED.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       END-DETAIL-LN-PROCESS.
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

           MOVE HEAD3 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 2 TO LN-FEED.
           MOVE 7 TO LN-COUNT.

      *********************************************
       FORM-RESET SECTION.
      *********************************************
           MOVE EXT-CONAME-CONAME TO DRAWER-CONAME.
           MOVE EXT-CONAME-SYSDATE TO DRAWER-SYSDATE.
           DISPLAY DRAWER-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE DRAWER-FORM-FIELDS TO WR-BUF.
           MOVE DRAWER-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           MOVE DRAWER-HELP--FILE TO HELP-LINK-FILE.
           MOVE DRAWER-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/DRAWER_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/BRSECURE.CPY".

      *************************************
       MISC-PARAGRAPHS SECTION.
      *************************************
       CONVERT-TIME.
           IF H-AND-M NOT GREATER THAN 1159
              MOVE "AM" TO T-AMPM
           ELSE
              MOVE "PM" TO T-AMPM.
           IF H-AND-M IS NOT LESS THAN 1300
              SUBTRACT 1200 FROM H-AND-M.
           IF CUR-HH = 0
              MOVE 12 TO CUR-HH.
           MOVE CUR-HH TO T-HH.
           MOVE CUR-MM TO T-MM.
           MOVE ":" TO T-COL.
           MOVE SPACE TO T-SPACE.
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      *************************************
       IO-ROUTINE SECTION.
      *************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPCAGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPCA1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      *************************************
       END-PROGRAM SECTION.
      *************************************
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
              DESTROY DRAWER-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
