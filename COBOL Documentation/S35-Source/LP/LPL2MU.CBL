       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LPL2MU.
       AUTHOR.          KEC.
       DATE-WRITTEN.    100698.
      ******************************************************************
      *
      *  DIR : LP
      *  DESC: SECOND FILE LISTING MENU
      *
      *-----------------------------------------------------------------
      *  REV.
      *  KEC 102795 ADDED F6-VIEW REPORTS LOGIC.
      *  MJD 011596 ADDED PWLIST.
      *  MJD 031097 ADDED EMPTY PARAGRAPHS USED IN LIBGB/MENU:
      *             BEFORE-SEL: PERFORMED BEFORE ENTERING SELECTION.
      *             AFTER-SEL : PERFORMED AFTER ENTERING SELECTION.
      *  GLF 121297 ADDED APLIST
      *  KEC 100698 DUE TO SPACE CONSTRAINTS, THE LISTING MENU HAS
      *              BEEN SPLIT INTO TWO SEPERATE MENUS JUST LIKE
      *              MAINTENANCE MENUS.  REGENCY REVOLVING
      *  GLF 121299 ADDED MSLIST
      * KEC 030520 {PR#00026} REGIONAL ACCEPTANCE <A15> [K. MULLEN]
      *            ADDED "20. TRIAD FILE"
      * CS  040622  ADDED REVOLVING OTHER CHARGES (RO)
      * CS  041029 ADDED #22 SMLIST (SPR MEMO LISTING) [PR#362 WORLD]
      * BLF 060607 ADDED #23 GRLST4 GROUP SUMMARY LISTING, WORLD PR# 469
      * BLF 060080 ADDED #24 PALIST, PAYEE LISTING, DEVELOPMENT PR# 2633
      * BAH 080310 ADDED #25 LILIST, LOAN APP IMPORT,SUPPORT PR# 3168
      * BAH 080908 ADDED #26 BSLIST, PAYMENT REFNO,CITIZENS PR# 3240
      * KEC 140624 {PR#00999} WORLD ACCEPTANCE <A15> [JIM ROSENAUER]
      *            ADDED "27. BRANCH APPROVAL LIMIT LISTING"
      * KEC 150120 {PR#00356} REGIONAL ACCEPTANCE <A15> [K. MULLEN]
      *            ADDED "28. TRIAD 9 FILE
      *  CS 161222 ADD 29. COST O6(OTHER 6 UNEARNED)
      * AMM 181203 REMOVED MSLIST 
      * BAH 190204 REMOVED RPLIST REVOLVING M15 
      * BAH 190204 REMOVED ROLIST REVOLVING M15 
      * BAH 2019-0617 REMOVED M08, APLIST, QA0036
      * BAH 2019-0705 REMOVED M28, LR9LST, PT0151
      * KEC 2021-1028 {PR#1522} WORLD ACCEPTANCE <A15> [KEVIN MULLEN]
      *            ADDED #30 GRAPPR, GROUP APPROVAL LIMIT
      * BAH 20230908 REMOVED M24, PALIST
      * SMR 2024-0201 ADDED OTIS TRACE LOGGING LOGIC
      ******************************************************************
       COPY "LIBGB/MENU1A.CPY".
       COPY "LIBLP/LPL2MU_DEF.CPY".
       COPY "LIBLP/LPL2MU_WKS.CPY".
       COPY "LIBGB/MENU1B.CPY".
       COPY "LIBLP/LPL2MU_SCN.CPY".
       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-START' SO THAT IT CAN GET THE EXACT PROGRAM
      *    NAME IN THE TRACE LOG.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "LPL2MU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-START.

       DISPLAY-MENU.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH EXIT-PATHNAME.
           MOVE "LPL2MU" TO VDU-FORM-NAME.

           PERFORM FORM-RESET.

           ACCEPT GP-REC FROM ENVIRONMENT "GPENV".

      * SECURITY TEST
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
           IF ( VDUSTATUS = "1>" )
              MOVE "LP/LPLSMU" TO FORM-NAM
              GO TO EXIT-PROG.

           IF ( VDUSTATUS = "1<" )
              MOVE "GB/PERUSE" TO FORM-NAM
              GO TO RUN-PROG.

           IF ( VDUSTATUS NOT = "00" )
              GO TO GET-MENU-SELECTION.

      * MUST INITIALIZE FOR MENU SELECTIONS, IF PREVIOUSLY RAN IN BATCH
      * THE EXT-ROUTE-BUF WONT BE CORRECT
           MOVE "00" TO EXT-ROUTE-BUF.

           GO TO M01 M02 M03 M04 M05 M06 M07 M08 M09 M10
                 M11 M12 M13 M14 M15 M16 M17 M18 M19 M20
                 M21 M22 M23 M24 M25 M26 M27 M28 M29 M30
              DEPENDING ON FORM-BUF.

           GO TO GET-MENU-SELECTION.

       M01. MOVE "LS/AOLIST" TO FORM-NAM.
            GO TO RUN-PROG.

       M02. MOVE "LS/BWLIST" TO FORM-NAM.
            GO TO RUN-PROG.

       M03. MOVE "LS/DSLIST" TO FORM-NAM.
            GO TO RUN-PROG.

       M04. MOVE "LS/GRLIST" TO FORM-NAM.
            GO TO RUN-PROG.

       M05. MOVE "LS/GRLST2" TO FORM-NAM.
            GO TO RUN-PROG.

       M06. MOVE "LS/GRLST3" TO FORM-NAM.
            GO TO RUN-PROG.

       M07. MOVE "LS/IRLIST" TO FORM-NAM.
            GO TO RUN-PROG.

       M08. GO TO GET-MENU-SELECTION.
       M09. GO TO GET-MENU-SELECTION.

       M10. MOVE "LS/LCLIST" TO FORM-NAM.
            GO TO RUN-PROG.

       M11. MOVE "LS/LNLIST" TO FORM-NAM.
            GO TO RUN-PROG.

       M12. MOVE "LS/LPLIST" TO FORM-NAM.
            GO TO RUN-PROG.

       M13. MOVE "LS/PWLIST" TO FORM-NAM.
            GO TO RUN-PROG.

       M14. MOVE "LS/PSLIST" TO FORM-NAM.
            GO TO RUN-PROG.

       M15. GO TO GET-MENU-SELECTION.

       M16. MOVE "LS/PDLIST" TO FORM-NAM.
            GO TO RUN-PROG.

       M17. MOVE "LS/STLIST" TO FORM-NAM.
            GO TO RUN-PROG.

       M18. MOVE "LS/SPLIST" TO FORM-NAM.
            GO TO RUN-PROG.

       M19. GO TO GET-MENU-SELECTION.

       M20. GO TO GET-MENU-SELECTION.

       M21.  GO TO GET-MENU-SELECTION.
      *M21. MOVE "LS/CDLIST" TO FORM-NAM.
      *     CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME "RO"
      *            "   OTHER CHARGES (REVOLVING) LISTING    ".
      * MUST BE 40 CHARACTERS  ^
      *     CANCEL FORM-PROGX.
      *     GO TO GET-MENU-SELECTION.

       M22. MOVE "LS/SMLIST" TO FORM-NAM.
            GO TO RUN-PROG.

       M23. MOVE "LS/GRLST4" TO FORM-NAM.
            GO TO RUN-PROG.

       M24. GO TO GET-MENU-SELECTION.
       M25. GO TO GET-MENU-SELECTION.
       M26. MOVE "LS/CDLIST" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME "BS"
                   "       PAYMENT REFNO FILE LISTING       ".
      * MUST BE 40 CHARACTERS  ^
            CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.
       M27. MOVE "LS/GIAPPR" TO FORM-NAM.
            GO TO RUN-PROG.
       M28. GO TO GET-MENU-SELECTION.
       M29. GO TO GET-MENU-SELECTION.
       M30. MOVE "SP/GRAPPR" TO FORM-NAM.
            GO TO RUN-PROG.

       RUN-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LPL2MU-SCREEN.

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
           MOVE "LPL2MU"      TO EXT-TCLP-TRACE-FORM-PROGX.
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
           MOVE "LPL2MU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-FINISH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LPL2MU-SCREEN.
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
              COPY "LIBLP/LPL2MU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***************************************************
       SETUP-LINK-INFO SECTION.
      ***************************************************
           MOVE LPL2MU-HELP--FILE TO HELP-LINK-FILE.
           MOVE LPL2MU-HELP--DIR TO HELP-LINK-DIR.


      ******************************************
       FORM-RESET SECTION.
      ******************************************
           DISPLAY LPL2MU-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE LPL2MU-FORM-FIELDS TO WR-BUF.
           MOVE LPL2MU-FORM-LIST TO WR-LIST.
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
           MOVE "LPL2MU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-RETURN.

