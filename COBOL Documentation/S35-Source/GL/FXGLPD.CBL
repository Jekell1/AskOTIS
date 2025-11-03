       IDENTIFICATION DIVISION.
       PROGRAM-ID.      FXGLPD.
      ******************************************************
      *               CLEAR AN ENTERED PERIOD
      * PER BRANCH RANGE
      *
      *   NOTE: THIS PROGRAM GL/FXGLPD MUST EXIST IN THE SPECIAL
      *           PASSWORD FILE (PWFILE)
      *
      * MG  000203 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      * BLF 070116 MAKE LOG ENTRY IN /USR/DATA/R1/LG/GL.YYMMDD TO TRACK
      *            WHEN THIS UPDATE WAS RUN, BY WHOM, & RANGES SELECTED,
      *            SUPPORT PR# 2780
      * BAH 2024.0521 FORM HAD 3 DIGITS FOR BRANCH, COPIED FORM FROM A15 S35Q-23
      ******************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.


       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)GL/FXGLPD.CBL S34 06/10/21-14:31:14 >".
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".

       01  ERRCD              PIC X         VALUE SPACES.
       01  ACCEPT-BUF         PIC X         VALUE SPACES.

       01  BEG-BRANCH         PIC 9(4)      VALUE 0.
       01  END-BRANCH         PIC 9(4)      VALUE 0.
       01  PERIOD             PIC 99        VALUE 0.
       01  IO-TYPE            PIC X(3)      VALUE SPACES.

       01  LOG-UPDATE-MSG.
           03  FILLER         PIC X(9)  VALUE " PERIOD: ".
           03  LOG-PERIOD     PIC 99.
           03  FILLER         PIC X(19) VALUE SPACES.
           03  FILLER         PIC X(12) VALUE "  BRANCHES: ".
           03  LOG-BEG-BR     PIC Z999.
           03  FILLER         PIC X     VALUE "-".
           03  LOG-END-BR     PIC Z999.

       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       01  FXGLPD-WINDOW-HANDLE  PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGL/FXGLPD_DEF.CPY".
       COPY "LIBGL/FXGLPD_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBGL/FXGLPD_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

      ********************************************
       MAIN-PROGRAM SECTION.
      ********************************************
           PERFORM SQL-CONNECT.
           MOVE "FXGLPD" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS FXGLPD-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".
           PERFORM FORM-RESET.

           PERFORM INITIALIZATION.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           PERFORM UPDATE-GL1-FILE.

           GO TO END-ROUTINE.

      ******************************************
       INITIALIZATION SECTION.
      ******************************************
           MOVE SPACES TO IO-TYPE.
       BR-01.
           MOVE "ENNN040BR1." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO BR-01.
           IF F3-KEY GO TO ALL-BR.
           IF F7-KEY GO TO ENDIT.
           IF NOT ENTER-KEY AND NOT DOWN-KEY GO TO BR-01.
           MOVE VDUNUM0 TO BEG-BRANCH.
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO BR-01.
           IF UP-KEY GO TO BR-01.
           IF F7-KEY GO TO ENDIT.
           IF NOT ENTER-KEY AND NOT DOWN-KEY GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.
           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           GO TO REC-PERIOD.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
       REC-PERIOD.
           MOVE "ENNN020PERIOD." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY GO TO ENDIT.
           IF UP-KEY GO TO BR-02.
           IF NOT ENTER-KEY AND NOT DOWN-KEY GO TO REC-PERIOD.
           MOVE VDUNUM0 TO PERIOD.
           IF PERIOD < 1 OR PERIOD > 12
              GO TO REC-PERIOD.
       ACCEPT-SCREEN-DATA.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO BR-01.
           IF UP-KEY GO TO REC-PERIOD.
           IF F7-KEY GO TO ENDIT.
           IF NOT ENTER-KEY AND NOT DOWN-KEY
              GO TO ACCEPT-SCREEN-DATA.
           MOVE VDUBUF TO ACCEPT-BUF.
           IF ACCEPT-BUF = "N"
              GO TO BR-01.
           IF ACCEPT-BUF NOT = "Y"
              GO TO ACCEPT-SCREEN-DATA.

           MOVE PERIOD     TO LOG-PERIOD.
           MOVE BEG-BRANCH TO LOG-BEG-BR.
           MOVE END-BRANCH TO LOG-END-BR.
           PERFORM LOG-CHANGES.

           MOVE "S  A000MESS." TO VDU.
           MOVE "GL/MASTER PERIOD CLEAR IN PROGRESS"
               TO VDUBUF.
           PERFORM SCREENIO.

           PERFORM OPEN-GL1-FILE.

           GO TO EXIT-INITIALIZATION.
       ENDIT.
           MOVE "END" TO IO-TYPE.
       EXIT-INITIALIZATION.
           EXIT.

      *******************************************
       UPDATE-GL1-FILE SECTION.
      *******************************************

           MOVE BEG-BRANCH TO GL-BRANCH
                              QGL1-WBEG-BRANCH.
           MOVE END-BRANCH TO QGL1-WEND-BRANCH.
           MOVE 0          TO GL-ACCOUNT
                              QGL1-WBEG-ACCOUNT.
           MOVE ALL "9"    TO QGL1-WEND-ACCOUNT.

           PERFORM START-GL1-FILE.

       UPDATE-GLMASTER.
           PERFORM READ-GL1-FILE-NEXT.
           IF IO-BAD
              GO TO END-UPDATE-GL.

           IF GL-BRANCH > END-BRANCH
              GO TO END-UPDATE-GL.

           IF GL-BRANCH < BEG-BRANCH
      *       UNLOCK GL-FILE
              GO TO UPDATE-GLMASTER.

           MOVE 0 TO GL-CYBAL(PERIOD).
           PERFORM REWRITE-GL1-FILE.

           GO TO UPDATE-GLMASTER.
       END-UPDATE-GL.
           EXIT.

      ******************************************
       LOG-CHANGES SECTION.
      ******************************************

      * CODE TO WRITE CHANGE FILE.
           MOVE "G"              TO ERRLOGW-ACTION.

           MOVE LOG-UPDATE-MSG   TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME    TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".


      **********************************************************
       FORM-RESET SECTION.
      **********************************************************
           MOVE EXT-CONAME-CONAME TO FXGLPD-CONAME.
           MOVE EXT-CONAME-SYSDATE TO FXGLPD-SYSDATE.
      *    DISPLAY OMITTED ERASE SCREEN.
      * USING THE LINE 1 COL 1 WORKS MUCH MORE CONSISTENTLY!
      * THE OMITTED LINE WOULD NOT CLEAR ON A F1 FROM THE BOTTOM BUT WOULD
      * CLEAR WHEN PART WAY THRU A NEW RECORD. DIDN'T MAKE SENSE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY FXGLPD-SCREEN UPON FXGLPD-WINDOW-HANDLE.
           MOVE FXGLPD-FORM-FIELDS TO WR-BUF.
           MOVE FXGLPD-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************************
       SETUP-LINK-INFO SECTION.
      **********************************************************
           MOVE FXGLPD-HELP--FILE TO HELP-LINK-FILE.
           MOVE FXGLPD-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGL/FXGLPD_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **********************************************************
       MISC-ROUTINES SECTION.
      **********************************************************
       COPY "LIBGB/ACCESS.CPY".

      *****************************************
       IO-ROUTINES SECTION.
      *****************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-GL1-FILE.
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGL/GLGL1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      *****************************************
       END-ROUTINE SECTION.
      *****************************************
       END-RUN.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE "GL/GLMNMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY FXGLPD-SCREEN.
           DESTROY FXGLPD-WINDOW-HANDLE.
           EXIT PROGRAM.
