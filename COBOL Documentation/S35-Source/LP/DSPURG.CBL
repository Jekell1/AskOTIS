       IDENTIFICATION DIVISION.
       PROGRAM-ID.      DSPURG.
       DATE-WRITTEN.    022404.
      ******************************************************************
      *
      *  DESCRIPTION:  DELETE DS RECORDS OLDER THAN SELECTED DATE
      *-----------------------------------------------------------------
      * REV:
      * BLM 160512 FIX FOR NEW DS-DATE FORMAT, PD0003
      * KEC 2017.0105 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED DS-DATE TO DS-DATE-CCYYMM FOR CONSISTENCY
      *          WITH THE DATE FIELDS IN BY & UD FILE.
      * BAH 181015 GLOBAL DSFILE
      * BAH 20190924 REMOVED ACCESS-CALL DSFILE
      * BAH 20200722 CHANGED PURGE-DATE TO 9(8)
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.


      ******************************************
      *      WORKING STORAGE SECTION
      ******************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/DSPURG.CBL S35 02/20/24-13:50:16 >".
       COPY "LIBLP/LP01DS.CPY".
       COPY "LIBLP/LP01DS_SQL.CPY".
       COPY "LIBLP/LPWSDS.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".

       01  ERRCD             PIC X     VALUE " ".

       01  ACCEPT-FG         PIC X(7)  VALUE " ".

       01  MESS-LIST         PIC X(5)  VALUE "MESS.".

       01  PURGE-DS-DATE-CCYYMM PIC 9(6)      VALUE 0.

       01  PURGE-DATE           PIC 9(8)      VALUE 0.

      *-----------------------------------------------------------------
       01  BEG-BRANCH        PIC 9(4)      VALUE 0.
       01  END-BRANCH        PIC 9(4)      VALUE 0.
       01  BR1               PIC 9999      VALUE 0.
       01  BR2               PIC 9999      VALUE 0.
       01  GROUP-FLAG        PIC X.
       01  ENT-GROUP         PIC X(4).

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       01  DSPURG-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/DSPURG_DEF.CPY".
       COPY "LIBLP/DSPURG_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/DSPURG_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M - C O N T R O L
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE "DSPURG" TO VDU-FORM-NAME.

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
      *             WITH NO SCROLL
      *             WITH NO WRAP
                   HANDLE IS DSPURG-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           IF EXT-ROUTE-BUF NOT = "00"
              MOVE "PROGRAM NOT BATCHABLE" TO MESS
              PERFORM SEND-MESS
              MOVE "99" TO EXT-ROUTE-BUF
              GO TO END-PROGRAM.

           PERFORM INITIAL-ROUTINE.
           IF ( ERRCD NOT = " " )
              GO TO END-ROUTINE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           IF ( GROUP-FLAG = "N" )
              MOVE BR1 TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *----------------------------------------------------------------
       NEXT-GROUP-LOC.
           IF ( GROUP-FLAG NOT = "Y" )
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.

           IF ( WGR-SRESULT = 0 )
              GO TO END-ROUTINE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF ( IO-FG NOT = 0 )
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

      *----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF ( IO-FG = 9 ) OR ( BR-NO > BR2 )
              GO TO END-ROUTINE.

      *----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO DS-PATH-OVERRIDE.

           PERFORM GLOBAL-UPDATE.

       END-OF-GROUP.
           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           GO TO END-PROGRAM.

      ******************************************************************
       GLOBAL-UPDATE SECTION.
      ******************************************************************
           MOVE " " TO MESS.
           STRING "PURGE IN PROGRESS..." BR-NO
                   DELIMITED BY SIZE INTO MESS.
           PERFORM SEND-LEGEND.

           PERFORM PURGE-DS.

       GLOBAL-UPDATE-EXIT.
           EXIT.


      ********************************************
       PURGE-DS SECTION.
      ********************************************
           PERFORM OPEN-DS1-FILE.

      * ENTER DATE ON SCREEN: PURGE PRIOR TO : _______
      * ALWAYS START AT BEGINNING UNTIL REACH DATE ENTERED
           MOVE 0     TO DS-DATE-CCYYMM
                         QDS1-WBEG-DATE-CCYYMM.
           MOVE BR-NO TO DS-BRANCH
                         QDS1-WBEG-BRANCH
                         QDS1-WEND-BRANCH.
           MOVE 0     TO DS-NUM DS-CLASS
                         QDS1-WBEG-NUM
                         QDS1-WBEG-CLASS.

           MOVE ALL "9"  TO QDS1-WEND-DATE-CCYYMM
                            QDS1-WEND-NUM
                            QDS1-WEND-CLASS.

           PERFORM START-DS1-FILE.

       PURGE-DS-NEXT.
           PERFORM READ-DS1-FILE-NEXT.
           IF (IO-FG = 9) OR (DS-BRANCH NOT = BR-NO)
              GO TO PURGE-DS-EXIT.

           IF DS-DATE-CCYYMM NOT < PURGE-DS-DATE-CCYYMM
              GO TO PURGE-DS-EXIT.

           PERFORM DELETE-DS1-FILE.

           GO TO PURGE-DS-NEXT.

       PURGE-DS-EXIT.
           PERFORM CLOSE-DS1-FILE.

      ******************************************************************
       INITIAL-ROUTINE SECTION.
      ******************************************************************
           MOVE " " TO ERRCD.

       INIT-ACCEPT-PURGE-DATE.
           MOVE "ERNM060PURGEDATE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"
              GO TO INITIALIZE-ENDIT.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO INIT-ACCEPT-PURGE-DATE.

           MOVE VDU-DATE-YYYYMMDD TO PURGE-DATE.
           IF PURGE-DATE = 0
              GO TO INIT-ACCEPT-PURGE-DATE.

      *-----------------------------------------------------------------
       INIT-ACCEPT-BR1.
           MOVE "   A000BR1." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "1>" )
              GO TO INITIALIZE-ENDIT.

           IF ( VDUSTATUS = "16" )
              GO TO INIT-ACCEPT-BR-ALL.

           IF ( VDUSTATUS = "1U" )
              GO TO INIT-ACCEPT-PURGE-DATE.

           IF ( VDUSTATUS = "1<" )
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO INIT-ACCEPT-BR1
              ELSE
                 MOVE "S  A000BR1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO INIT-ACCEPT-BR1.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF ( WGR-WORD-ZERO )
              MOVE 0 TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.

           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF ( WGR-ERR > 0 )
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO INIT-ACCEPT-BR1.

           IF ( WGR-TYPE = "A" )
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BR1 BEG-BRANCH
              MOVE 9999 TO BR2 END-BRANCH
              GO TO INIT-ACCEPT-ACCEPT.

      *----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BR1 BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BR1 TO VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       INIT-ACCEPT-BR2.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "1>" )
              GO TO INITIALIZE-ENDIT.

           IF ( VDUSTATUS = "1U" )
              GO TO INIT-ACCEPT-BR1.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO INIT-ACCEPT-BR2.

           MOVE VDUNUM0 TO BR2 END-BRANCH.

           IF ( BR1 > BR2 )
              PERFORM INVALID-RANGE
              GO TO INIT-ACCEPT-BR1.

           GO TO INIT-ACCEPT-ACCEPT.

      *-----------------------------------------------------------------
       INIT-ACCEPT-BR-ALL.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BR1 BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO BR2 END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

      *-----------------------------------------------------------------
      *    Y - RUN TO COMPLETION
      *    N - CONVERT OPTION
      *-----------------------------------------------------------------
       INIT-ACCEPT-ACCEPT.
           MOVE "S  A040MESS2." TO VDU.
           PERFORM SCREENIO.
           MOVE "E  A070ACCEPT." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>"
              GO TO INITIALIZE-ENDIT.

           IF VDUSTATUS = "1U"
              GO TO INIT-ACCEPT-BR2.

           IF VDUSTATUS NOT = "00"
              GO TO INIT-ACCEPT-ACCEPT.

           MOVE VDUBUF TO ACCEPT-FG.

           IF ACCEPT-FG = "N"
              GO TO INITIALIZE-ENDIT.

           IF NOT (ACCEPT-FG = "Y" )
              GO TO INIT-ACCEPT-ACCEPT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * SET BEGINNING OF MONTH & SET PURGE-BY-DATE:
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE PURGE-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
           MOVE DATE-YYYYMM TO PURGE-DS-DATE-CCYYMM.

           MOVE "PROGRAM IN PROGRESS..........." TO MESS.
           PERFORM SEND-LEGEND.

           GO TO INITIALIZE-EXIT.

      *-----------------------------------------------------------------
       INITIALIZE-ENDIT.
           MOVE "X" TO ERRCD.

      *-----------------------------------------------------------------
       INITIALIZE-EXIT.
           EXIT.


      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           MOVE EXT-CONAME-CONAME TO DSPURG-CONAME.
           MOVE EXT-CONAME-SYSDATE TO DSPURG-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY DSPURG-SCREEN UPON DSPURG-WINDOW-HANDLE.
           MOVE DSPURG-FORM-FIELDS TO WR-BUF.
           MOVE DSPURG-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE DSPURG-HELP--FILE TO HELP-LINK-FILE.
           MOVE DSPURG-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/DSPURG_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***************************************
       MISC-PARA SECTION.
      ***************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-DS1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
       COPY "LIBLP/LPDSGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBLP/LPDS1IN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "LP/EMPUMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY DSPURG-SCREEN.
           DESTROY DSPURG-WINDOW-HANDLE.
           EXIT PROGRAM.
