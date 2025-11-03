       IDENTIFICATION DIVISION.
       PROGRAM-ID.      OPMENU.
      ******************************************************************
      *
      *  DIR : SP
      *  DESC: OPTIONAL MODULES
      *
      * REV:
      *  MJD 031097 ADDED EMPTY PARAGRAPHS USED IN LIBGB/MENU:
      *             BEFORE-SEL: PERFORMED BEFORE ENTERING SELECTION.
      *             AFTER-SEL : PERFORMED AFTER ENTERING SELECTION.
      *
      *  KEC 082297 ***CONVERSION-A90-TO-A10***
      *                ADDED OPTIONS FROM A90'S:
      *                    "SPECIAL PROCEDURES" MENU;
      *                    "OPTIONAL MODULES" MENU.
      *  MJD 010697 REMOVED IQ AND TELETRAK OPTIONS
      *             REMOVED VOYAGER (ADDED TO SP/INSRMU.C)
      *  KEC 012998 ADDED 23. ACCOUNT INFORMATION (FORMSXPRESS)
      *  BLV 020998 REVISED MENU; MOVED SEVERAL OPTIONS TO MISC
      *             EXTRACTION MENU
      *  BLV 021398 ADDED LAMENU; RETURN TO LPMENU
      *  BLV 030422 ADDED EMMENU, EMPLOYEE TIME CARD MENU,
      *             CASHCO PR# 2045
      *  BLF 060315 ADDED EPMENU, ELECTRONIC PAYMENT PROCESSING,
      *             LENDMARK PR# 2711
      *  BLF 070308 IF I-AM-WORLD, EXECUTE SP/TSMENU (TIME SHEET MENU
      *             INSTEAD OF EMMENU (EMPLOYEE TIME CARD MENU),
      *             WORLD PR# 488
      *  BLM 101012 IF GP-SECURITY-FG = "Y", CALL NEW SECURITY MENU
      *             (SEMENU) INSTEAD OF OLD SECURITY MENU (USMENU),
      *             PR# NEWSECURITY
      *  BAH 140814 STRIPPED OUT OLD SECURITY, A30 IS ONLY NEW SEMENU
      *  BLM 160831 DISABLE SELECTION 15 (SPGNMU), WCBC NO LONGER USED
      *  KEC 180913 DISABLE SELECTION 10 (FGMENU), WAC DOES NOT USE
      *  KEC 181026 DISABLE SELECTION 06 (BCMENU), WONT NEED ANYMORE
      *  BAH 20190702 REMOVED 18 (TSMENU) PT0161
      *  KEC 20190708 REMOVED 17 (LAPPMU) PT0149
      *  BAH 20191105 REMOVED 8 (APMENU) PT0192
      *  BAH 2022.0314 REMOVED EXT-SPANISH-FORMS AND ALL MX FORMS
      *  BAH 2023.0307 REMOVED 17,18,19(EPMENU) PUT ALL EPMENU ITEMS IN QA
      * SMR 2024-0201 ADDED OTIS TRACE LOGGING LOGIC
      ******************************************************************
       COPY "LIBGB/MENU1A.CPY".
       COPY "LIBSP/OPMENU_DEF.CPY".
       COPY "LIBSP/OPMENU_WKS.CPY".
       COPY "LIBGB/MENU1B.CPY".
       COPY "LIBSP/OPMENU_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-START' SO THAT IT CAN GET THE EXACT PROGRAM
      *    NAME IN THE TRACE LOG.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "OPMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-START.

       DISPLAY-MENU.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH EXIT-PATHNAME.
           MOVE "OPMENU" TO VDU-FORM-NAME.

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
           IF VDUSTATUS = "1>"
              MOVE "LP/LPMENU" TO FORM-NAM
              GO TO EXIT-PROG.

           IF VDUSTATUS NOT = "00"
              GO TO GET-MENU-SELECTION.

           GO TO M01 M02 M03 M04 M05 M06 M07 M08 M09 M10
                 M11 M12 M13 M14 M15 M16 
              DEPENDING ON FORM-BUF.

           GO TO GET-MENU-SELECTION.

      *=================================================================
       M01. MOVE "LP/ASMENU" TO FORM-NAM.
            GO TO RUN-PROG.
       M02. MOVE "GL/GLMENU" TO FORM-NAM.
            GO TO RUN-PROG.
       M03. MOVE "LP/FDMENU" TO FORM-NAM.
            GO TO RUN-PROG.
       M04. MOVE "SP/TWMENU" TO FORM-NAM.
            GO TO RUN-PROG.
       M05. MOVE "SP/IRMENU" TO FORM-NAM.
            GO TO RUN-PROG.
       M06.  GO TO GET-MENU-SELECTION.
       M07.  GO TO GET-MENU-SELECTION.
       M08.  GO TO GET-MENU-SELECTION.
       M09. MOVE "GB/SEMENU" TO FORM-NAM.
            PERFORM VERIFY-PASSWORD.
            IF PASSWDW-STAT NOT = "OK"
               MOVE "PASSWORD REQUIRED FOR THIS OPTION" TO MESS
               PERFORM SEND-MESS
               GO TO GET-MENU-SELECTION.
            MOVE "GB/SEMENU" TO FORM-NAM.
            GO TO RUN-PROG.
       M10. GO TO GET-MENU-SELECTION.
       M11. MOVE "SP/INSRMU" TO FORM-NAM.
            GO TO RUN-PROG.
       M12. MOVE "SP/BANKMU" TO FORM-NAM.
            GO TO RUN-PROG.
       M13. MOVE "SP/EXTRMU" TO FORM-NAM.
            GO TO RUN-PROG.
       M14. GO TO GET-MENU-SELECTION.
       M15. GO TO GET-MENU-SELECTION.
       M16. MOVE "MN/BYMISC" TO FORM-NAM.
            GO TO RUN-PROG.
       RUN-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY OPMENU-SCREEN.

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
           MOVE "OPMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
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
           MOVE "OPMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-FINISH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATH TO FORM-PATHNAME.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY OPMENU-SCREEN.
           EXIT PROGRAM.

       COPY "LIBCL/TRACE.CPY".
       COPY "LIBGB/CONAME_XFER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ******************************************
       VDU-CONVERT-FIELD SECTION.
      ******************************************
           MOVE "2A" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/OPMENU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************
       SETUP-LINK-INFO SECTION.
      ******************************************
           MOVE OPMENU-HELP--FILE TO HELP-LINK-FILE.
           MOVE OPMENU-HELP--DIR TO HELP-LINK-DIR.

      ******************************************
       FORM-RESET SECTION.
      ******************************************
           DISPLAY OPMENU-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE OPMENU-FORM-FIELDS TO WR-BUF.
           MOVE OPMENU-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           PERFORM CONAME-XFER.
       FORM-RESET-EXIT.
           EXIT.

      **********************************************
       MISC-ROUTINES SECTION.
      **********************************************
       COPY "LIBGB/SENDMESS.CPY".
      *  ENTER PASSWORD FOR USER SECURITY MENU
       VERIFY-PASSWORD.
           MOVE " "        TO PASSWDW-STAT.
           IF GP-SECURITY-FG = "Y"
              MOVE "SEMENU" TO PASSWDW-PGM
           ELSE
              MOVE "USMENU"   TO PASSWDW-PGM.
           MOVE 0          TO PASSWDW-SEQ.
           MOVE "PLEASE ENTER THE PASSWORD" TO PASSWDW-PROMPT1.
           MOVE "FOR USER SECURITY MENU"    TO PASSWDW-PROMPT2.
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
           MOVE "OPMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-RETURN.

