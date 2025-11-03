       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GLRPMU.
      *****************************************************************
      *
      *  DIR : GL
      *  DESC: GENERAL LEDGER REPORT MENU
      *
      * REV:
      *   MJD 011196 ADDED VIEW REPORTS OPTION.
      *   MJD 031097 ADDED EMPTY PARAGRAPHS USED IN LIBGB/MENU:
      *              BEFORE-SEL: PERFORMED BEFORE ENTERING SELECTION.
      *              AFTER-SEL : PERFORMED AFTER ENTERING SELECTION.
      *   BLV 041797 CHANGE DETAIL TRAIL (SEL 1) TO DTLBAL
      *   BLV 082297 CHANGE SELECTIONS 2 - 4 TO NEW VERSIONS FROM A90:
      *              DTLBA2, SUMTRL, SUMTR2
      *   CLS 122297 ADDED CHART OF ACCTS(ACCOUNT TYPE, END OF YEAR CLEAR)
      *   MG  000204 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *   BLF 060113 ADD SELECTION 11, GL A/R AND UNEARNED BALANCING
      *              REPORT, BALAR, TO MENU, WORLD PR# 450
      *   BLM 090618 ADD SELECTIONS 12 & 13, PAYEE/CHECK REPORT BY RFC#
      *              & PAYEE/CHECK DETAIL REPORT BY RFC#, FOR MEXICO,
      *              WORLD PR# 640
      *   BAH 210805 REMOVED 11,12,13. 11 WONT WORK WITH WORKDAY.
      *              12,13 WERE MEXICO. REMOVED 9, KM DIDNT KNOW OF IT.
      *   AMM 211214 ADDED #9, GLLLST TO MENU
      * SMR 2024-0201 ADDED OTIS TRACE LOGGING LOGIC
      *****************************************************************
       COPY "LIBGB/MENU1A.CPY".
       COPY "LIBGL/GLRPMU_DEF.CPY".
       COPY "LIBGL/GLRPMU_WKS.CPY".
       COPY "LIBGB/MENU1B.CPY".
       COPY "LIBGL/GLRPMU_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-START' SO THAT IT CAN GET THE EXACT PROGRAM
      *    NAME IN THE TRACE LOG.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "GLRPMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-START.

       DISPLAY-MENU.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH EXIT-PATHNAME.
           MOVE "GLRPMU" TO VDU-FORM-NAME.

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

           GO TO M01 M02 M03 M04 M05 M06 M07 M08 M09 M10
              DEPENDING ON FORM-BUF.

           GO TO GET-MENU-SELECTION.

       M01. MOVE "GL/DTLBAL" TO FORM-NAM.
            GO TO RUN-PROG.
       M02. MOVE "GL/DTLBA2" TO FORM-NAM.
            GO TO RUN-PROG.
       M03. MOVE "GL/SUMTRL" TO FORM-NAM.
            GO TO RUN-PROG.
       M04. MOVE "GL/SUMTR2" TO FORM-NAM.
            GO TO RUN-PROG.
       M05. GO TO GET-MENU-SELECTION.
       M06. MOVE "GL/TRANLS" TO FORM-NAM.
            GO TO RUN-PROG.
       M07. MOVE "GL/PERLST" TO FORM-NAM.
            GO TO RUN-PROG.
       M08. MOVE "GL/ALLIST" TO FORM-NAM.
            GO TO RUN-PROG.
       M09. MOVE "GL/GLLLST" TO FORM-NAM.
            GO TO RUN-PROG.
       M10. MOVE "GL/CHART"  TO FORM-NAM.
            GO TO RUN-PROG.

       RUN-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GLRPMU-SCREEN.

           MOVE FORM-PATH TO FORM-PATHNAME.
           CALL FORM-PROGX USING FORM-PATHNAME EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-RETURN' SO THAT IT CAN GET THE EXACT
      *    PROGRAM NAME IN THE TRACE LOG
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "GLRPMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
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
           MOVE "GLRPMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-FINISH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GLRPMU-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           EXIT PROGRAM.

       COPY "LIBCL/TRACE.CPY".
       COPY "LIBGB/CONAME_XFER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ***********************************************
       VDU-CONVERT-FIELD SECTION.
      ***********************************************
           MOVE "2A" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGL/GLRPMU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***********************************************
       SETUP-LINK-INFO SECTION.
      ***********************************************
           MOVE GLRPMU-HELP--FILE TO HELP-LINK-FILE.
           MOVE GLRPMU-HELP--DIR TO HELP-LINK-DIR.

      ******************************************
       FORM-RESET SECTION.
      ******************************************
           DISPLAY GLRPMU-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE GLRPMU-FORM-FIELDS TO WR-BUF.
           MOVE GLRPMU-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM CONAME-XFER.
       FORM-RESET-EXIT.
           EXIT.

      ***********************************************
       MISC-ROUTINES SECTION.
      ***********************************************
       COPY "LIBGB/SENDMESS.CPY".

      *=================================================================
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-RETURN' SO THAT IT CAN GET THE EXACT
      *    PROGRAM NAME IN THE TRACE LOG
      *=================================================================
       MISC-TRACE-RETURN.

           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "GLRPMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-RETURN.

