       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GLMENU.
      **************************************************************************
      *
      *  DIR : GL
      *  DESC: GENERAL LEDGER MASTER MENU
      *
      * REV:
      *  MJD 031097 ADDED EMPTY PARAGRAPHS USED IN LIBGB/MENU:
      *             BEFORE-SEL: PERFORMED BEFORE ENTERING SELECTION.
      *             AFTER-SEL : PERFORMED AFTER ENTERING SELECTION.
      *  BLV 990126 ADDED GLEXMU - GL EXTRACTION MENU
      *  MG  000204 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *  BLV 020215 IF GLOBAL PARAMETER FOR NEW JOURNAL ENTIRES YES,
      *             CALL NEW PROGRAM FROM SEL 3, GIVE OLD JE'S AS
      *             SEL #10, ADD F6 VIEW REPORTS OPTION
      *  JTG 031216 ADDED 6. CLOSE FISCIAL PERIOD
      *             FOR CHANGE GL UPDATE TO TEST NEW GLOBAL PARAMETER FLAG
      *             GP-CLOSED-FISCAL-FG, TO DETERMINE IF GL FISCAL PERIOD
      *             HAS BEEN CLOSED. IF CLOSED STOP UPDATE.          WORLD #J040
      *  BLV 030105 REMOVE GT PURGE (ENDMTH) FROM MENU, PUT ON EMPUMU
      *  BAH 20211021 REMOVED GL/JOURNL, ALWAYS CALL JEMENU
      * SMR 2024-0201 ADDED OTIS TRACE LOGGING LOGIC
      **************************************************************************
       COPY "LIBGB/MENU1A.CPY".
       COPY "LIBGL/GLMENU_DEF.CPY".
       COPY "LIBGL/GLMENU_WKS.CPY".
       COPY "LIBGB/MENU1B.CPY".
       COPY "LIBGL/GLMENU_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-START' SO THAT IT CAN GET THE EXACT PROGRAM
      *    NAME IN THE TRACE LOG.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "GLMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-START.

       DISPLAY-MENU.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH EXIT-PATHNAME.
           MOVE "GLMENU" TO VDU-FORM-NAME.

           PERFORM FORM-RESET.

           ACCEPT GP-REC FROM ENVIRONMENT "GPENV".

      * SECURITY CHECK
           MOVE FORM-PATH TO CHKSEC-FORM-PATH.
           CALL "CHKSEC" USING CHKSEC-FIELDS.
           CANCEL "CHKSEC".

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

           IF CHKSEC-STAT NOT = "00"
              MOVE "1>" TO STAT VDUSTATUS
              GO TO MENU-EXIT.

       GET-MENU-SELECTION-CLR.
           MOVE 0 TO VDU-PROGRAMMING.

      ************************************************
      *    GET SELECTION
      ************************************************
       GET-MENU-SELECTION.
           MOVE FORM-LIST TO WR-LIST.
           MOVE SPACES TO WR-BUF.
           PERFORM CALL-WRFORM.

           MOVE "E  N020SEL." TO VDU.
           PERFORM SCREENIO.
           MOVE VDUNUM0 TO FORM-BUF.

           IF VDUSTATUS = "10"
              MOVE 1 TO RETURN-CODE
              GO TO EXIT-PROG.

       MENU-EXIT.
           IF VDUSTATUS = "1<"
              MOVE "GB/PERUSE" TO FORM-NAM
              GO TO RUN-PROG.

           IF F7-KEY
              MOVE "SP/OPMENU" TO FORM-NAM
              GO TO EXIT-PROG.

           IF NOT ENTER-KEY
              GO TO GET-MENU-SELECTION.

           IF FORM-BUF = 99
              GO TO M99.

           MOVE "00" TO EXT-ROUTE-BUF.

           GO TO M01 M02 M03 M04 M05 M06 M07 M08 M09
              DEPENDING ON FORM-BUF.

           GO TO GET-MENU-SELECTION.
       M01. MOVE "GL/GLMNMU" TO FORM-NAM.
            GO TO RUN-PROG.
       M02. MOVE "GL/GLRPMU" TO FORM-NAM.
            GO TO RUN-PROG.
       M03. MOVE "GL/JOURNL" TO FORM-NAM.
            GO TO RUN-PROG.
       M04. MOVE "GL/GLINQ"  TO FORM-NAM.
            GO TO RUN-PROG.
       M05. MOVE "GL/GLUPD"  TO FORM-NAM.
            GO TO RUN-PROG.
       M06. GO TO GET-MENU-SELECTION.
       M07. MOVE "GL/ENDYER" TO FORM-NAM.
            GO TO RUN-PROG.
       M08. GO TO GET-MENU-SELECTION.
       M09. MOVE "GL/GLEXMU" TO FORM-NAM.
            GO TO RUN-PROG.
       M99. MOVE "SP/WRLDMU" TO FORM-NAM.
            GO TO RUN-PROG.

       RUN-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GLMENU-SCREEN.

           MOVE FORM-PATH TO FORM-PATHNAME.
           CALL FORM-PROGX USING FORM-PATHNAME EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

           IF RETURN-CODE = 1
              GO TO EXIT-PROG.


      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-RETURN' SO THAT IT CAN GET THE EXACT
      *    PROGRAM NAME IN THE TRACE LOG
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "GLMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-RETURN.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           GO TO DISPLAY-MENU.
       EXIT-PROG.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-FINISH' SO THAT IT CAN GET THE EXACT
      *    PROGRAM NAME IN THE TRACE LOG
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "GLMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-FINISH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GLMENU-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           GOBACK.

       COPY "LIBCL/TRACE.CPY".
       COPY "LIBGB/CONAME_XFER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *******************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************
           MOVE "2A" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGL/GLMENU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *******************************************
       SETUP-LINK-INFO SECTION.
      *******************************************
           MOVE GLMENU-HELP--FILE TO HELP-LINK-FILE.

      ******************************************
       FORM-RESET SECTION.
      ******************************************
           DISPLAY GLMENU-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE GLMENU-FORM-FIELDS TO WR-BUF.
           MOVE GLMENU-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM CONAME-XFER.
       FORM-RESET-EXIT.
           EXIT.

      ******************************************
       MISC-ROUTINES SECTION.
      ******************************************
       COPY "LIBGB/SENDMESS.CPY".

      *=================================================================
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-RETURN' SO THAT IT CAN GET THE EXACT
      *    PROGRAM NAME IN THE TRACE LOG
      *=================================================================
       MISC-TRACE-RETURN.

           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "GLMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-RETURN.

