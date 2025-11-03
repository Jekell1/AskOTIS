       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CJPURG.
       DATE-WRITTEN.    010496.
      ******************************************************************
      *
      *                      (SP)
      *      DESCRIPTION: COLLECTOR JOURNAL PURGE
      *
      * REV :
      * KEC 032196 ADDED CHECK FOR VALID DATE RANGE.
      * GLF 022398 DELETED BR-SECURITY-OK AND ADDED LIBGB/BRSECUREW.
      * KEC 000620 {PR#00000} PARADATA FINANCIAL [BARB]
      *             ADDED 'TIMER' LOGIC.
      * BLF 070330 FIX DATE RANGE CHECK TO SPEED UP PURGE, PL #581
      * BAH 170815 ACTIVATE 8 DIGIT DATES, PD0006
      * KEC 2018.0424 REMOVED USE OF LOCAL-PRINT-FG SINCE NOT USED;
      *         LOCAL-PRINT-FG NOT BEING SET; IN EOBUF WHICH IS EXTERNAL
      *         WHERE IF NOT BEING SET WILL CAUSE ISSUES FROM THAT
      *         LOCAL-PRINT-FG BYTE POSITION COULD BE SET FROM PREVIOUS
      *         PROGRAM WITH EOBUF THAT SET SOMETHING IN THAT BYTE.
      *  CS 190118 GLOBAL CJFILE
      * BAH 20191002 REMOVED ACCESS-CALL CJFILE
      * BAH 20220930 HARD CODED START CJ
      ******************************************************************
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

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/CJPURG.CBL S35 02/21/24-13:18:33 >".

      ******************************************************************
      *
      *    F I L E   I F N
      *
      ******************************************************************
       COPY "LIBSP/SP01CJ.CPY".
       COPY "LIBSP/SP01CJ_SQL.CPY".
       COPY "LIBSP/SPWSCJ.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
      *-----------------------------------------------------------------
       01  PR-IFN               PIC XX     VALUE "PR".
       01  MSEL-LIST            PIC X(5)   VALUE "MSEL.".
       01  LEGEND               PIC X(70)  VALUE SPACES.

       01  LN-FEED          PIC 99       VALUE 0.


      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  BEG-COLLID      PIC X(3).
           03  END-COLLID      PIC X(3).
           03  BEG-DATE        PIC 9(8).
           03  END-DATE        PIC 9(8).
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).

       01  VERSION-CONTROL     PIC X     VALUE "A".
       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBSP/CJPURG_CMD.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBWI/TIMERW.CPY".

       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/CJPURG_DEF.CPY".
       COPY "LIBWI/TIMER_DEF.CPY".
       COPY "LIBSP/CJPURG_WKS.CPY".
       COPY "LIBWI/TIMER_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/CJPURG_SCN.CPY".
       COPY "LIBWI/TIMER_SCN.CPY".

      *----------------------------------------------------------------
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
      *
      *   M A I N - M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CJPURG"      TO FORM-PROG VDU-FORM-NAME.

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
           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.
        
           MOVE "B" TO CJ-PATH-OVERRIDE.

           PERFORM OPEN-CJ1-FILE.

           MOVE CJ-PATH-OWNBR     TO CJ-BRNO
                                     QCJ1-WBEG-BRNO
                                     QCJ1-WEND-BRNO.
           MOVE BEG-COLLID        TO CJ-COLLID
                                     QCJ1-WBEG-COLLID.
           MOVE END-COLLID        TO QCJ1-WEND-COLLID.
           MOVE BEG-DATE          TO CJ-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QCJ1-WBEG-DATE.
           MOVE END-DATE          TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QCJ1-WEND-DATE.
           MOVE 0                 TO CJ-START-TIME CJ-SEQNO
                                     QCJ1-WBEG-START-TIME
                                     QCJ1-WBEG-SEQNO.
           MOVE ALL "9"           TO QCJ1-WEND-START-TIME
                                     QCJ1-WEND-SEQNO.


           PERFORM START-CJ1-FILE.

       NEXT-CJ-REPORT.
           PERFORM READ-CJ1-FILE-NEXT.
           IF IO-FG = 9 OR (CJ-BRNO NOT = CJ-PATH-OWNBR)
              GO TO END-OF-GROUP.

           IF ( CJ-BRNO < BEG-BRANCH ) OR ( CJ-BRNO > END-BRANCH )
              GO TO NEXT-CJ-REPORT.

           IF ( CJ-COLLID < BEG-COLLID )
              GO TO NEXT-CJ-REPORT.

           IF ( CJ-COLLID > END-COLLID )
              GO TO END-OF-GROUP.

           IF ( CJ-DATE < BEG-DATE ) OR (CJ-DATE > END-DATE)
              GO TO NEXT-CJ-REPORT.

           PERFORM DELETE-CJ1-FILE.

           GO TO NEXT-CJ-REPORT.

       END-OF-GROUP.
           PERFORM CLOSE-CJ1-FILE.

           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.

           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE CJPURG-QUEUE-POS TO Q-POS.
           MOVE CJPURG-COPIES-POS TO C-POS.
           MOVE CJPURG-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

           MOVE "Y" TO NOACCEPT-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF EXT-ROUTE-BUF = "00"
              MOVE "COLLECTOR JOURNAL PURGE IN PROGRESS..." TO MESS
              PERFORM PROGRESS-MSG.

           IF BEG-BRANCH = 0
              IF END-BRANCH = 0
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH
                                             END-BRANCH.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.

       BEGIN-BRANCH.
           MOVE LEGEND-BR-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "R  A000BEG-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF ( VDUSTATUS = "1<" )
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO BEGIN-BRANCH
              ELSE
                 MOVE "S  A000BEG-BRANCH." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.

           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEGIN-BRANCH.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF WGR-WORD-ZERO
              MOVE 0 TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.

           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO BEGIN-BRANCH.

           IF WGR-TYPE = "A"
              MOVE "S  A000END-BRANCH." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.

       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BEG-BRANCH." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.

       ENDING-BRANCH.
           MOVE LEGEND-BR-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040END-BRANCH." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BEGIN-BRANCH.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENDING-BRANCH.
           MOVE VDUNUM0 TO END-BRANCH.

           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEGIN-BRANCH.

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
           MOVE "SNNN040BEG-BRANCH." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040END-BRANCH." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A160DISPLAY-USER." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BEGIN-BRANCH.

       BEGIN-COLLECTOR.
           MOVE LEGEND-COLL-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A030BEG-COLL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-COLLECTORS.
           IF VDUSTATUS = "1U" GO TO ENDING-BRANCH.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEGIN-COLLECTOR.
           MOVE VDUNUM0 TO BEG-COLLID.

       ENDING-COLLECTOR.
           MOVE LEGEND-COLL-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A030END-COLL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BEGIN-COLLECTOR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENDING-COLLECTOR.
           MOVE VDUNUM0 TO END-COLLID.

           IF BEG-COLLID > END-COLLID
              MOVE "INVALID COLLECTOR ID RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEGIN-COLLECTOR.

           GO TO BEGIN-DATE.

       ALL-COLLECTORS.
           MOVE "S  A030BEG-COLL." TO VDU.
           MOVE SPACES             TO BEG-COLLID VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A030END-COLL." TO VDU.
           MOVE BABY-ZS            TO END-COLLID VDUBUF.
           PERFORM SCREENIO.

       BEGIN-DATE.
           IF ( EXT-ROUTE-BUF NOT = "00" ) AND ( VDUSTATUS = "1U" )
              GO TO ENDING-COLLECTOR.

           IF ( EXT-ROUTE-BUF = "40" OR "50" )
              GO TO EXIT-PARM.

           MOVE LEGEND-DATE-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNM080BEG-DATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.

           IF VDUSTATUS = "1U" GO TO ENDING-COLLECTOR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BEGIN-DATE.

           IF ( VDUNUM0 = 0 )
              GO TO BEGIN-DATE.

           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.

       ENDING-DATE.
           IF EXT-ROUTE-BUF NOT = "00" AND VDUSTATUS = "1U"
              GO TO ENDING-COLLECTOR.

           MOVE LEGEND-DATE-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNM080END-DATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.

           IF VDUSTATUS = "1U" GO TO BEGIN-DATE.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO ENDING-DATE.

           IF ( VDUNUM0 = ZEROES )
              GO TO ENDING-DATE.

           MOVE VDU-DATE-YYYYMMDD TO END-DATE.

           IF END-DATE < BEG-DATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEGIN-DATE.

       ENTER-ACCEPT-YN.

           MOVE LEGEND-ACCEPT TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENDING-DATE.
           IF (        F1-KEY ) GO TO EXIT-PARM.
           IF (        F7-KEY ) GO TO EXIT-PARM.
           IF NOT ( ENTER-KEY ) GO TO ENTER-ACCEPT-YN.

           IF ( VDUBUF = "T" )
              PERFORM TIMER-ROUTINE
              MOVE "CJPURG" TO VDU-FORM-NAME
              MOVE TIMER-ACCEPT-YN TO VDUBUF.

           IF ( VDUBUF     = "N" ) GO TO ENTER-PARM.
           IF ( VDUBUF NOT = "Y" ) GO TO ENTER-ACCEPT-YN.

       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************
           IF GROUP-FLAG = "Y"
              MOVE "S  A000BEG-BRANCH." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000END-BRANCH." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BEG-BRANCH." TO VDU
              MOVE BEG-BRANCH TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040END-BRANCH." TO VDU
              MOVE END-BRANCH TO VDUNUM0
              PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A030BEG-COLL." TO VDU.
           MOVE BEG-COLLID         TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A030END-COLL." TO VDU.
           MOVE END-COLLID         TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".

       COPY "LIBWI/TIMER.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO CJPURG-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CJPURG-SYSDATE.
           DISPLAY CJPURG-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE CJPURG-FORM-FIELDS TO WR-BUF.
           MOVE CJPURG-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE CJPURG-HELP--FILE TO HELP-LINK-FILE.
           MOVE CJPURG-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/CJPURG_EVA.CPY".
              COPY "LIBWI/TIMER_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       PROGRESS-MSG.
           MOVE MESS TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLRP_SQL.CPY".
           PERFORM CLOSE-CJ1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBSP/SPCJGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBSP/SPCJ1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE "LP/EMPUMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY CJPURG-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
