       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GLMNMU.
      ******************************************************************
      *
      *  DIR : GL
      *  DESC: GENERAL LEDGER MAINTENANCE MENU
      *
      * REV:
      *  MJD 031097 ADDED EMPTY PARAGRAPHS USED IN LIBGB/MENU:
      *             BEFORE-SEL: PERFORMED BEFORE ENTERING SELECTION.
      *             AFTER-SEL : PERFORMED AFTER ENTERING SELECTION.
      *  BLV 080897 ADD SELECTION 11: DELETE GL ACCTS (A90)
      *  BLV 020998 ADD SELECTIONS 12,13: ADP GT UPDATE; GT EXTRACT
      *  BLV 020998 MOVE SELECTIONS 12,13: ADP GT UPDATE; GT EXTRACT
      *             TO GLEXMU
      *  MG  000204 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *  BLV 040727 ADD SEL 12, BUDGET ENTRY
      *  BLF 070111 ADD FORCED PASSWORD FOR SELECTIONS 7 (REVERSE GL
      *             PERIOD, 8 (CLEAR GL PERIOD), AND 10 (TRANSACTION
      *             ACCOUNT # CHANGE), SUPPORT PR# 2780
      *  BLF 070112 CHANGED SELECTION 6 TO BE PRIOR YEAR GL UPDATE
      *             (LYGLUP) INSTEAD OF FIX PERIOD 12, SUPPORT PR# 2780
      *  BLM 140305 ADD SELECTION 13, GL BUDGET UPLOAD, WORLD PR# 978
      *  BLM 161206 ADD SELECTION 14, GI INTERFACE ACCT# CHANGE,
      *             WORLD PR# 1204
      * SMR 2024-0201 ADDED OTIS TRACE LOGGING LOGIC
      ******************************************************************
       COPY "LIBGB/MENU1A.CPY".
       COPY "LIBGL/GLMNMU_DEF.CPY".
       COPY "LIBGL/GLMNMU_WKS.CPY".
       COPY "LIBGB/MENU1B.CPY".
       COPY "LIBGL/GLMNMU_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-START' SO THAT IT CAN GET THE EXACT PROGRAM
      *    NAME IN THE TRACE LOG.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "GLMNMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-START.

       DISPLAY-MENU.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH EXIT-PATHNAME.
           MOVE "GLMNMU" TO VDU-FORM-NAME.
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

           IF VDUSTATUS NOT = "00"
              GO TO GET-MENU-SELECTION.

           MOVE "00" TO EXT-ROUTE-BUF.

           GO TO M01 M02 M03 M04 M05 M06 M07 M08 M09 M10
                 M11 M12 M13 M14
              DEPENDING ON FORM-BUF.

           GO TO GET-MENU-SELECTION.

       M01. MOVE "GL/GLMAIN" TO FORM-NAM.
            GO TO RUN-PROG.
       M02. MOVE "GL/GTMAIN" TO FORM-NAM.
            GO TO RUN-PROG.
       M03. MOVE "GL/ALMAIN" TO FORM-NAM.
            GO TO RUN-PROG.
       M04. 
            GO TO GET-MENU-SELECTION.
       M05. MOVE "GL/BRANCH" TO FORM-NAM.
            GO TO RUN-PROG.
       M06. MOVE "GL/LYGLUP" TO FORM-NAM.
            GO TO RUN-PROG.
       M07. MOVE "FXGLUP"                TO PASSWDW-PGM.
            MOVE "TO RUN REVERSE PERIOD" TO PASSWDW-PROMPT2.
            PERFORM VERIFY-PASSWORD.
            IF PASSWDW-STAT NOT = "OK"
               MOVE "PASSWORD REQUIRED FOR THIS OPTION" TO MESS
               PERFORM SEND-MESS
               GO TO GET-MENU-SELECTION
            ELSE
               MOVE "GL/FXGLUP" TO FORM-NAM
               GO TO RUN-PROG.
       M08. MOVE "FXGLPD"                 TO PASSWDW-PGM.
            MOVE "TO RUN CLEAR GL PERIOD" TO PASSWDW-PROMPT2.
            PERFORM VERIFY-PASSWORD.
            IF PASSWDW-STAT NOT = "OK"
               MOVE "PASSWORD REQUIRED FOR THIS OPTION" TO MESS
               PERFORM SEND-MESS
               GO TO GET-MENU-SELECTION
            ELSE
               MOVE "GL/FXGLPD" TO FORM-NAM
               GO TO RUN-PROG.
       M09. MOVE "GL/GLDUP" TO FORM-NAM.
            GO TO RUN-PROG.
       M10. MOVE "GTFIX"                        TO PASSWDW-PGM.
            MOVE "FOR TRANSACTION ACCT# CHANGE" TO PASSWDW-PROMPT2.
            PERFORM VERIFY-PASSWORD.
            IF PASSWDW-STAT NOT = "OK"
               MOVE "PASSWORD REQUIRED FOR THIS OPTION" TO MESS
               PERFORM SEND-MESS
               GO TO GET-MENU-SELECTION
            ELSE
               MOVE "GL/GTFIX" TO FORM-NAM
               GO TO RUN-PROG.
       M11. MOVE "GL/DELGL1" TO FORM-NAM.
            GO TO RUN-PROG.
       M12. GO TO GET-MENU-SELECTION.
       M13. GO TO GET-MENU-SELECTION.
       M14. MOVE "SP/GIACCT" TO FORM-NAM.
            GO TO RUN-PROG.

       RUN-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GLMNMU-SCREEN.

           MOVE FORM-PATH TO FORM-PATHNAME.
           CALL FORM-PROGX USING FORM-PATHNAME EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG


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
           MOVE "GLMNMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-FINISH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GLMNMU-SCREEN.
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
              COPY "LIBGL/GLMNMU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***********************************************
       SETUP-LINK-INFO SECTION.
      ***********************************************
           MOVE GLMNMU-HELP--FILE TO HELP-LINK-FILE.
           MOVE GLMNMU-HELP--DIR TO HELP-LINK-DIR.


      ***********************************************
       FORM-RESET SECTION.
      ***********************************************
           DISPLAY GLMNMU-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE GLMNMU-FORM-FIELDS TO WR-BUF.
           MOVE GLMNMU-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM CONAME-XFER.
       FORM-RESET-EXIT.
           EXIT.

      ***********************************************
       MISC-ROUTINES SECTION.
      ***********************************************
       COPY "LIBGB/SENDMESS.CPY".
      *--------------------------------------------------------
      * FORCE PASSWORD FOR REVERSE PERIOD, CLEAR PERIOD,
      *                    TRANSACTION ACCOUNT # CHANGE
      *--------------------------------------------------------
       VERIFY-PASSWORD.
           MOVE "EX"       TO PASSWDW-STAT.
           MOVE 0          TO PASSWDW-SEQ.
           MOVE "PLEASE ENTER THE PASSWORD" TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL "PASSWD" USING FORM-PATH PASSWDW-BUF.
           CANCEL "PASSWD".

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

      *=================================================================
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-RETURN' SO THAT IT CAN GET THE EXACT
      *    PROGRAM NAME IN THE TRACE LOG
      *=================================================================
       MISC-TRACE-RETURN.

           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "GLMNMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-RETURN.

