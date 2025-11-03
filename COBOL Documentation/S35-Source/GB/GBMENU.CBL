       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GBMENU.
      ***********************************************************************
      *  DIR : GB
      *  DESC: GLOBAL MENU
      * REV:
      *  JTG 102896 CHANGED TO INCLUDE RELEASE NOTES, OPTION
      *  JTG 110796 CHANGED TO INCLUDE PROGRAMMING RELEASE NOTES, OPTION
      *  MJD 031097 ADDED EMPTY PARAGRAPHS USED IN LIBGB/MENU:
      *             BEFORE-SEL: PERFORMED BEFORE ENTERING SELECTION.
      *             AFTER-SEL : PERFORMED AFTER ENTERING SELECTION.
      *  KEC 110998 ADDED #06. GLOBAL PARAMETER MAINTENANCE (RATE TABLE)
      *  MG  991006 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *  BAH 010426 REMOVED SELECTIONS 9 AND 10 AND MADE 7 THE NEW RELEASE
      *             NOTES
      *  BLF 051222 ADD SELECTION 8, GLOBAL FLAG SEARCH & LOCATE
      * KEC 2019-0128 A32 - GPFILE GP-REC MODIFICATION 
      *               REMOVED THE FOLLOWING OPTIONS SINCE THEIR
      *               GP-KEY MAINTENANCES ARE NOT USED ANYMORE:
      *                  M05. MOVE "GB/GPLAMN" TO FORM-NAM.
      *                  M06. MOVE "GB/GPRTMN" TO FORM-NAM.
      * SMR 2024-0201 ADDED OTIS TRACE LOGGING LOGIC
      * JKC 2024-0206 ADDED #6 SQL ERROR
      * BAH 2024.0506 CHANGED FLINQ0 TO FLINQ, DONT NEED BOTH PROGRAMS
      * RMZ 2024-1016 ADDED #5 SQL STATUS (PASS/FAIL) (#S35Q-94 & #S35Q-104)
      ***********************************************************************
       COPY "LIBGB/MENU1A.CPY".
       COPY "LIBGB/GBMENU_DEF.CPY".
       COPY "LIBGB/GBMENU_WKS.CPY".
       COPY "LIBGB/MENU1B.CPY".
       COPY "LIBGB/GBMENU_SCN.CPY".
       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-START' SO THAT IT CAN GET THE EXACT PROGRAM
      *    NAME IN THE TRACE LOG.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "GBMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-START.

       DISPLAY-MENU.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH EXIT-PATHNAME.
           MOVE "GBMENU" TO VDU-FORM-NAME.

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
              MOVE "LP/LPMENU" TO FORM-NAM
              GO TO EXIT-PROG.

           IF VDUSTATUS NOT = "00"
              GO TO GET-MENU-SELECTION.

           MOVE "00" TO EXT-ROUTE-BUF.

           GO TO M01 M02 M03 M04 M05 M06 M07 M08
              DEPENDING ON FORM-BUF.

           GO TO GET-MENU-SELECTION.

       M01. MOVE "GB/GBMAIN" TO FORM-NAM.
            GO TO RUN-PROG.
       M02. MOVE "GB/BRMAIN" TO FORM-NAM.
            GO TO RUN-PROG.
       M03. MOVE "GB/BRLIST" TO FORM-NAM.
            GO TO RUN-PROG.
       M04. MOVE "GB/GPMAIN" TO FORM-NAM.
            GO TO RUN-PROG.
       M05. MOVE "WI/SQLLOG" TO FORM-NAM.
            GO TO RUN-PROG.
       M06. MOVE "WI/SQLERR" TO FORM-NAM.
            GO TO RUN-PROG.
       M07. MOVE "WI/RELNOT" TO FORM-NAM.
            GO TO RUN-PROG.
       M08. MOVE "SP/FLINQ"  TO FORM-NAM.
            GO TO RUN-PROG.

       RUN-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GBMENU-SCREEN.

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
           MOVE "GBMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
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
           MOVE "GBMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-FINISH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GBMENU-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           EXIT PROGRAM.

       COPY "LIBCL/TRACE.CPY".
       COPY "LIBGB/CONAME_XFER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *******************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************
           MOVE "2A" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGB/GBMENU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *******************************************
       SETUP-LINK-INFO SECTION.
      *******************************************
           MOVE GBMENU-HELP--FILE TO HELP-LINK-FILE.
           MOVE GBMENU-HELP--DIR TO HELP-LINK-DIR.

      ******************************************
       FORM-RESET SECTION.
      ******************************************
           DISPLAY GBMENU-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE GBMENU-FORM-FIELDS TO WR-BUF.
           MOVE GBMENU-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM CONAME-XFER.
       FORM-RESET-EXIT.
           EXIT.

      *******************************************
       MISC-ROUTINES SECTION.
      *******************************************
       COPY "LIBGB/SENDMESS.CPY".

      *=================================================================
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-RETURN' SO THAT IT CAN GET THE EXACT
      *    PROGRAM NAME IN THE TRACE LOG
      *=================================================================
       MISC-TRACE-RETURN.

           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "GBMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-RETURN.

