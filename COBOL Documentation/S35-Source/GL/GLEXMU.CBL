       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GLEXMU.
      ***********************************************************
      *
      *  DIR : GL
      *  DESC: GENERAL LEDGER EXTRACTION MENU
      *
      * REV:
      *  MJD 990930 ADDED CMAXEX (BF MOTOR CREDIT PR#953)
      *  BLV 000111 ADDED WISCGT, WISC PR# 1035
      *  MG  000203 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *  BLV 000504 ADDED SUPEXT, SUPERIOR PR# 1140
      *  GLB 000829 ADDED GTXMER, MERCURY  PR# 233
      *  BLV 020815 ADDED ITIEXT, REGWAN  PR# 1838
      *  BLV 021021 CHANGED SELECTION 2 FROM GTEXTR TO ITEXTR,
      *             REMOVED SELECTION 11, REGWAN  PR# 1931
      *  BLV 021113 ADDED SELECTION 11, SOLMON, SOAUTO PR# 1958
      *  BLM 10081R ADDED SELECTION 12, TRIST PR# 3627
      *   CS 110811 ADDED SELECTION 13, GTXCIT CITIZENS PR#3849
      *  BLM 170407 REMOVED NON-WORLD SELECTIONS, 2 - 13
      *  BLM 181209 ADDED SELECTIONS 2 & 3, WORKDAY GT EXTRACT
      *             AND WORKDAY GT EOD DEPOSIT EXTRACT, WORLD PR# 1353
      *  BAH 2020.0722 REMOVED 1. ADPUPD, NO LONGER USED, KM
      * SMR 2024-0201 ADDED OTIS TRACE LOGGING LOGIC
      ***********************************************************
       COPY "LIBGB/MENU1A.CPY".
       COPY "LIBGL/GLEXMU_DEF.CPY".
       COPY "LIBGL/GLEXMU_WKS.CPY".
       COPY "LIBGB/MENU1B.CPY".
       COPY "LIBGL/GLEXMU_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-START' SO THAT IT CAN GET THE EXACT PROGRAM
      *    NAME IN THE TRACE LOG.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "GLEXMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-START.

       DISPLAY-MENU.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH EXIT-PATHNAME.
           MOVE "GLEXMU" TO VDU-FORM-NAME.

           PERFORM FORM-RESET.

           ACCEPT GP-REC FROM ENVIRONMENT "GPENV".

      * SECURITY CHECK
           MOVE FORM-PATH TO CHKSEC-FORM-PATH.
           CALL "CHKSEC" USING CHKSEC-FIELDS.
           CANCEL "CHKSEC".

           PERFORM MISC-TRACE-RETURN.

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
           IF VDUSTATUS = "1>"
              MOVE "GL/GLMENU" TO FORM-NAM
              GO TO EXIT-PROG.

           IF VDUSTATUS = "1<"
              MOVE "GB/PERUSE" TO FORM-NAM
              GO TO RUN-PROG.

           IF VDUSTATUS NOT = "00"
              GO TO GET-MENU-SELECTION.

           MOVE "00" TO EXT-ROUTE-BUF.

           GO TO M01 M02
              DEPENDING ON FORM-BUF.

           GO TO GET-MENU-SELECTION.

       M01. MOVE "GL/WDAYGL" TO FORM-NAM.
            GO TO RUN-PROG.
       M02. MOVE "GL/WDAYBK" TO FORM-NAM.
            GO TO RUN-PROG.

       RUN-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GLEXMU-SCREEN.

           MOVE FORM-PATH TO FORM-PATHNAME.
           CALL FORM-PROGX USING FORM-PATHNAME EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.

           IF RETURN-CODE = 1
              GO TO EXIT-PROG.


      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-RETURN' SO THAT IT CAN GET THE EXACT
      *    PROGRAM NAME IN THE TRACE LOG
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "GLMNMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
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
           MOVE "GLEXMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-FINISH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GLEXMU-SCREEN.

           MOVE FORM-PATH TO FORM-PATHNAME.
           EXIT PROGRAM.

       COPY "LIBCL/TRACE.CPY".
       COPY "LIBGB/CONAME_XFER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      **************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************
           MOVE "2A" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGL/GLEXMU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************
           MOVE GLEXMU-HELP--FILE TO HELP-LINK-FILE.
           MOVE GLEXMU-HELP--DIR TO HELP-LINK-DIR.

      ******************************************
       FORM-RESET SECTION.
      ******************************************
           DISPLAY GLEXMU-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE GLEXMU-FORM-FIELDS TO WR-BUF.
           MOVE GLEXMU-FORM-LIST TO WR-LIST.
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
           MOVE "GLEXMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-RETURN.

