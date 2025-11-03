       IDENTIFICATION DIVISION.
       PROGRAM-ID. SEMENU.
      ******************************************************
      *
      *  DIR : GB
      *  DESC: NEW USER SECURITY MENU
      *
      * REV:
      * 150106 MADE OPTION 8 (CONVERT OLD TO NEW) N/A
      * 150116 ADDED OPTION 11 (LIST FORMS IN SNFILE BUT NOT UTFILE
      * 150120 ADDED OPTION 12 (SECURITY PROFILE STATUS)
      * 170210 REMOVE OPTION 11 (SNLIS2), MOVE #12 TO #11
      * SMR 2024-0201 ADDED OTIS TRACE LOGGING LOGIC
      * 2024.0605 BAH ADDED 12, PASSWORD EXCEPTION AUDIT REPORT #1659 S35Q-60
      * 2024.0729 BAH MENU TYPE-O 4. SET _COPY, SET & COPY S35Q-101
      * 2024.0926 BAH REMOVED PROMPT 8. CONVERT OLD TO NEW S35Q-132
      ******************************************************
       COPY "LIBGB/MENU1A.CPY".
       COPY "LIBGB/SEMENU_DEF.CPY".
       COPY "LIBGB/SEMENU_WKS.CPY".
       COPY "LIBGB/MENU1B.CPY".
       COPY "LIBGB/SEMENU_SCN.CPY".
       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-START' SO THAT IT CAN GET THE EXACT PROGRAM
      *    NAME IN THE TRACE LOG.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "SEMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-START.

       DISPLAY-MENU.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH EXIT-PATHNAME.
           MOVE "SEMENU" TO VDU-FORM-NAME.

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
           IF VDUSTATUS = "1<"
              MOVE "GB/PERUSE" TO FORM-NAM
              GO TO RUN-PROG.

           IF VDUSTATUS = "1>"
              MOVE "SP/OPMENU" TO FORM-NAM
              GO TO EXIT-PROG.

           IF VDUSTATUS NOT = "00"
              GO TO GET-MENU-SELECTION.

           MOVE "00" TO EXT-ROUTE-BUF.

           GO TO M01 M02 M03 M04 M05 M06 M07 M08 M09 M10 M11 M12
              DEPENDING ON FORM-BUF.

           GO TO GET-MENU-SELECTION.

       M01. MOVE "MN/SNMAIN" TO FORM-NAM.
            GO TO RUN-PROG.
       M02. MOVE "LS/SNLIST" TO FORM-NAM.
            GO TO RUN-PROG.
       M03. MOVE "MN/SEMAIN" TO FORM-NAM.
            GO TO RUN-PROG.
       M04. MOVE "GB/SECOPY" TO FORM-NAM.
            GO TO RUN-PROG.
       M05. MOVE "GB/SECENT" TO FORM-NAM.
            GO TO RUN-PROG.
       M06. MOVE "LS/SELIST" TO FORM-NAM.
            GO TO RUN-PROG.
       M07. MOVE "GB/SECREA" TO FORM-NAM.
            GO TO RUN-PROG.
       M08. GO TO GET-MENU-SELECTION.                           
       M09. MOVE "MN/PWMAIN" TO FORM-NAM.
            GO TO RUN-PROG.
       M10. MOVE "MN/PEMAIN" TO FORM-NAM.
            GO TO RUN-PROG.
       M11. MOVE "GB/SESTAT" TO FORM-NAM.
            GO TO RUN-PROG.
       M12. MOVE "SP/PEVERI" TO FORM-NAM.
            GO TO RUN-PROG.

       RUN-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY SEMENU-SCREEN.

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
           MOVE "SEMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
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
           MOVE "SEMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-FINISH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY SEMENU-SCREEN.
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
              COPY "LIBGB/SEMENU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***************************************************
       SETUP-LINK-INFO SECTION.
      ***************************************************
           MOVE SEMENU-HELP--FILE TO HELP-LINK-FILE.
           MOVE SEMENU-HELP--DIR TO HELP-LINK-DIR.

      ******************************************
       FORM-RESET SECTION.
      ******************************************
           DISPLAY SEMENU-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE SEMENU-FORM-FIELDS TO WR-BUF.
           MOVE SEMENU-FORM-LIST TO WR-LIST.
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
           MOVE "SEMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-RETURN.

