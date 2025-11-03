       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GTFLAG.
      ******************************************************************
      *
      * DESCRIPTION: CLEAR GT-FLAG TO SPACES, DON'T AFFECT GL BALANCE
      *
      *=================================================================
      * REV:
      * KEC 2016.1129 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED GT-DATE1 GT-DATE2 GT-DATE3 AS ACTIVE CCYYMMDD.
      *
      ******************************************************************
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
           "@(#)GL/GTFLAG.CBL S34 06/10/21-14:58:18 >".
       COPY "LIBGL/GL01GT.CPY".
       COPY "LIBGL/GL01GT_SQL.CPY".
       COPY "LIBGL/GLWSGT.CPY".

       01  ERRCD              PIC X         VALUE SPACES.
       01  ACCEPT-BUF         PIC X         VALUE SPACES.

       01  MESS-LIST          PIC X(5)      VALUE
           "MESS.".

       01  BEG-BRANCH         PIC 9(4) VALUE 0.
       01  END-BRANCH         PIC 9(4) VALUE 0.
       01  BEG-DATE           PIC 9(8) VALUE 0.
       01  END-DATE           PIC 9(8) VALUE 0.
       01  IO-TYPE            PIC X(3)      VALUE SPACES.
       01  SOURCE-CODE        PIC 9.
       01  ALL-SRC-CODES REDEFINES SOURCE-CODE  PIC X.

       01  LOG-UPDATE-MSG.
           03  FILLER         PIC XX   VALUE " ".
           03  LOG-BEG-DATE   PIC 99/99/99.
           03  FILLER         PIC X    VALUE "-".
           03  LOG-END-DATE   PIC 99/99/99.
           03  FILLER         PIC X(12) VALUE "  BRANCHES: ".
           03  LOG-BEG-BR     PIC Z999.
           03  FILLER         PIC X    VALUE "-".
           03  LOG-END-BR     PIC Z999.

       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       01  GTFLAG-WINDOW-HANDLE  PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGL/GTFLAG_DEF.CPY".
       COPY "LIBGL/GTFLAG_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBGL/GTFLAG_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

      ********************************************
       MAIN-PROGRAM SECTION.
      ********************************************
           PERFORM SQL-CONNECT.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "GTFLAG" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
      *             WITH NO SCROLL
      *             WITH NO WRAP
                   HANDLE IS GTFLAG-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".
           PERFORM FORM-RESET.

      D    STOP MESS.
           PERFORM INITIALIZATION.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.

           PERFORM UPDATE-GT1-FILE.

           GO TO END-ROUTINE.

      *********************************************************
       INITIALIZATION SECTION.
      *********************************************************
           MOVE SPACES TO IO-TYPE.

       DT-01.
           MOVE "ERNM060BEGDATE." TO VDU.
           PERFORM SCREENIO.
           IF F3-KEY GO TO ALL-DT.
           IF F7-KEY GO TO ENDIT.
           IF NOT (ENTER-KEY OR DOWN-KEY)
              GO TO DT-01.
           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.
       DT-02.
           MOVE "ERNM060ENDDATE." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO DT-01.
           IF UP-KEY GO TO DT-01.
           IF F7-KEY GO TO ENDIT.
           IF NOT (ENTER-KEY OR DOWN-KEY)
              GO TO DT-02.
           MOVE VDU-DATE-YYYYMMDD TO END-DATE.
           IF BEG-DATE > END-DATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO DT-01.
           GO TO BR-01.
       ALL-DT.
           MOVE "S  A080BEGDATE." TO VDU.
           MOVE 0 TO BEG-DATE VDU-DATE-EDIT
           PERFORM SCREENIO.
           MOVE "S  A080ENDDATE." TO VDU.
           MOVE 99999999 TO END-DATE VDU-DATE-EDIT.
           PERFORM SCREENIO.

       BR-01.
           MOVE "ENNN040BR1." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO DT-01.
           IF F3-KEY GO TO ALL-BR.
           IF F7-KEY GO TO ENDIT.
           IF UP-KEY GO TO DT-01.
           IF NOT (ENTER-KEY OR DOWN-KEY)
              GO TO BR-01.
           MOVE VDUNUM0 TO BEG-BRANCH.
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO DT-01.
           IF UP-KEY GO TO BR-01.
           IF F7-KEY GO TO ENDIT.
           IF NOT (ENTER-KEY OR DOWN-KEY)
              GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.
           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           GO TO ENTER-SOURCE-CODE.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.

       ENTER-SOURCE-CODE.
           MOVE "ENNN010SRC." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO DT-01.
           IF F3-KEY GO TO SEND-ALL-SRC.
           IF UP-KEY GO TO BR-02.
           IF F7-KEY GO TO ENDIT.
           IF NOT ENTER-KEY GO TO ENTER-SOURCE-CODE.
           MOVE VDUNUM0 TO SOURCE-CODE.
           GO TO ACCEPT-SCREEN-DATA.
       SEND-ALL-SRC.
            MOVE "S  A010SRC." TO VDU.
            MOVE "*" TO ALL-SRC-CODES VDUBUF.
            PERFORM SCREENIO.

       ACCEPT-SCREEN-DATA.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO DT-01.
           IF UP-KEY GO TO ENTER-SOURCE-CODE.
           IF F7-KEY GO TO ENDIT.
           IF NOT (ENTER-KEY OR DOWN-KEY)
              GO TO ACCEPT-SCREEN-DATA.
           MOVE VDUBUF TO ACCEPT-BUF.
           IF ACCEPT-BUF = "N"
              GO TO DT-01.
           IF ACCEPT-BUF NOT = "Y"
              GO TO ACCEPT-SCREEN-DATA.

           MOVE BEG-DATE    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO LOG-BEG-DATE.
           MOVE END-DATE    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO LOG-END-DATE.
           MOVE BEG-BRANCH  TO LOG-BEG-BR.
           MOVE END-BRANCH  TO LOG-END-BR.
           PERFORM LOG-CHANGES.

           MOVE "CLEAR GL TRANS UPDATE FLAG IN PROGRESS"
               TO MESS.
           MOVE MESS-LIST TO WR-LIST.
           MOVE MESS TO WR-BUF.
           PERFORM CALL-WRFORM.

           PERFORM OPEN-GT1-FILE.

           GO TO EXIT-INITIALIZATION.
       ENDIT.
           MOVE "END" TO IO-TYPE.
       EXIT-INITIALIZATION.
           EXIT.

      **********************************************************
       UPDATE-GT1-FILE SECTION.
      **********************************************************
       READ-GLTRANS.

      * READ ONLY "P" (UPDATED) RECORDS THAT FIT THE DATE RANGE
      * AND REMOVE FROM UPDATE

           PERFORM READ-GT1-FILE-NEXT.
           IF IO-BAD
              GO TO END-UPDATE-GT.

           IF GT-REF = "EOY-MX"
              IF GT-DATE1 = 20040108   
                 PERFORM DELETE-GT1-FILE
                 GO TO READ-GLTRANS.

           IF GT-FLAG NOT = "P" GO TO READ-GLTRANS.

           IF GT-BRANCH < BEG-BRANCH OR GT-BRANCH > END-BRANCH
              GO TO READ-GLTRANS.

           IF ALL-SRC-CODES NOT = "*"
              IF GT-SOURCE NOT = SOURCE-CODE
                 GO TO READ-GLTRANS.

       CHECK-TRANS-DATE.
      * VERIFY BETWEEN BEGINNING AND ENDING DATE
           IF GT-DATE1 < BEG-DATE OR GT-DATE1 > END-DATE
              GO TO READ-GLTRANS.

       REWRITE-GLTRANS.
           MOVE " " TO GT-FLAG.
           MOVE 0 TO GT-PERIOD.
           PERFORM REWRITE-GT1-FILE.

           GO TO READ-GLTRANS.
       END-UPDATE-GT.
           EXIT.

      **********************************************************
       LOG-CHANGES SECTION.
      **********************************************************
      * CODE TO WRITE CHANGE FILE.
           MOVE "G"              TO ERRLOGW-ACTION.

           MOVE LOG-UPDATE-MSG   TO ERRLOGW-DETAIL.
           MOVE FORM-PATHNAME    TO ERRLOGW-PROGX.

           MOVE "CL/ERRLOG" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH ERRLOGW-AREA.
           CANCEL FORM-PROGX.

      **********************************************************
       FORM-RESET SECTION.
      **********************************************************
           MOVE EXT-CONAME-CONAME TO GTFLAG-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GTFLAG-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY GTFLAG-SCREEN UPON GTFLAG-WINDOW-HANDLE.
           MOVE GTFLAG-FORM-FIELDS TO WR-BUF.
           MOVE GTFLAG-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************************
       SETUP-LINK-INFO SECTION.
      **********************************************************
           MOVE GTFLAG-HELP--FILE TO HELP-LINK-FILE.
           MOVE GTFLAG-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGL/GTFLAG_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

      *****************************************
       MISC SECTION.
      *****************************************
       COPY "LIBGB/ACCESS.CPY".

      *****************************************
       IO-ROUTINES SECTION.
      *****************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-GT1-FILE.
       COPY "LIBGL/GLGTGS_SQL.CPY".
       COPY "LIBGL/GLGT1IN.CPY".
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
           DESTROY GTFLAG-SCREEN.
           DESTROY GTFLAG-WINDOW-HANDLE.
           EXIT PROGRAM.
