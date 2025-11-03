       IDENTIFICATION DIVISION.
       PROGRAM-ID.    COMENU.
      **************************************************************************
      *  DIR : GB
      *  DESC: COMPANY OPTION MENU.
      * REVS:
      *  MJD 032196 CHANGED OPTION 5 TO BE GPOPMN.
      *  MG  991005 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *  BLM 101118 CHANGED SELECTIONS FOR NEW SECURITY, USED TO BE:
      *             1-COMAIN,2-COLIST,3-BROPMN,4-COUPDT,5-GPOPMN
      *             CHANGED TO:
      *             1-SNMAIN,2-SEMAIN,3-BROPMN,4-GPOPMN
      *             NOTE, SNMAIN & SEMAIN ARE ON THE SECURITY MENU, GB/SEMENU,
      *             BUT THESE CHECK EXITPATH, & EXITPATH OF GB/COMENU ALLOWS
      *             DELETING SN-REC OR SE-REC, & CHANGING PURCHASABLE OPTION
      *             FLAGS & EXPIRATION DATE, PR# NEWSECURITY
      *  BLM 110405 WHOOPS, THEY STILL USED THE CO STUFF TO CONTROL
      *             PURCHASABLE ITEMS, PUT OPMAIN, OPLIST & COUPDT BACK ON
      *             MENU PER JIM, PR# NEWSECURITY
      *  BAH 110906 CHANGED RRFORM TO SCREENIO
      *  BAH 130125 A30 CONVERSION
      *  BAH 130125 REMOVED OLD SECURITY OPTIONS
      * SMR 2024-0201 ADDED OTIS TRACE LOGGING LOGIC
      **************************************************************************
       COPY "LIBGB/MENU1A.CPY".
       COPY "LIBGB/COMENU_DEF.CPY".
       COPY "LIBGB/COMENU_WKS.CPY".
       COPY "LIBGB/MENU1B.CPY".
       COPY "LIBGB/COMENU_SCN.CPY".
       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-START' SO THAT IT CAN GET THE EXACT PROGRAM
      *    NAME IN THE TRACE LOG.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "COMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-START.

       DISPLAY-MENU.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH EXIT-PATHNAME.
           MOVE "COMENU" TO VDU-FORM-NAME.

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
              MOVE "LP/LPMENU" TO FORM-NAM
              GO TO EXIT-PROG.

       MENU-EXIT.
           IF VDUSTATUS = "12"
              MOVE " " TO FORM-NAM
              GO TO EXIT-PROG.

           IF VDUSTATUS = "1>"
              MOVE "LP/LPMENU" TO FORM-NAM
              GO TO EXIT-PROG.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO GET-MENU-SELECTION.

           MOVE "00" TO EXT-ROUTE-BUF.

           GO TO M01 M02 M03 M04
              DEPENDING ON FORM-BUF.

           GO TO GET-MENU-SELECTION.

       M01. MOVE "MN/SNMAIN" TO FORM-NAM.
            GO TO RUN-PROG.
       M02. MOVE "MN/SEMAIN" TO FORM-NAM.
            GO TO RUN-PROG.
       M03. MOVE "GB/BROPMN" TO FORM-NAM.
            GO TO RUN-PROG.
       M04. MOVE "GB/GPOPMN" TO FORM-NAM.
            GO TO RUN-PROG.

       RUN-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY COMENU-SCREEN.

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
           MOVE "COMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
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
           MOVE "COMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-FINISH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY COMENU-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           EXIT PROGRAM.

       COPY "LIBCL/TRACE.CPY".
       COPY "LIBGB/CONAME_XFER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ***************************************************
       VDU-CONVERT-FIELD SECTION.
      ***************************************************
           MOVE "2A" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGB/COMENU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***************************************************
       SETUP-LINK-INFO SECTION.
      ***************************************************
           MOVE COMENU-HELP--FILE TO HELP-LINK-FILE.
           MOVE COMENU-HELP--DIR  TO HELP-LINK-DIR.

      ***************************************************
       FORM-RESET SECTION.
      ***************************************************
           DISPLAY COMENU-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE COMENU-FORM-FIELDS TO WR-BUF.
           MOVE COMENU-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM CONAME-XFER.
       FORM-RESET-EXIT.
           EXIT.

      ***************************************************
       MISC-ROUTINES SECTION.
      ***************************************************
       COPY "LIBGB/SENDMESS.CPY".

      *=================================================================
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-RETURN' SO THAT IT CAN GET THE EXACT
      *    PROGRAM NAME IN THE TRACE LOG
      *=================================================================
       MISC-TRACE-RETURN.

           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "COMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-RETURN.

