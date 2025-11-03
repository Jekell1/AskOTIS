       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SPMENU.
      **************************************************************************
      *  DIR : SP
      *  DESC: ADDITIONAL PROCEDURES MENU
      *
      * REV:
      *  JTG 101096 ADDED CLASS TRANSFERS OPTION
      *  MJD 031097 ADDED EMPTY PARAGRAPHS USED IN LIBGB/MENU:
      *             BEFORE-SEL: PERFORMED BEFORE ENTERING SELECTION.
      *             AFTER-SEL : PERFORMED AFTER ENTERING SELECTION.
      *  MJD 110697 CHANGED TO CALL LPCVMU INSTEAD OF LONI.
      *  BLV 021398 REMOVED OPTION 1 OPMENU (MOVED TO LPMENU)
      *  CS  991001 ADDED OPTION TO SELECT BULK PURCHASES MENU
      *  GLB 010718 ADDED OPTION 9. MERCURY DEALER RELEASE EXTRACT
      *  BLV 010810 CHANGE OPTION 9. TO BE SPSFMU, SALES FINANCE BATCH
      *             PROCESSING MENU, MERC PR# 253
      *  BLV 030611 ADDED OPTION 10, BULK TRANSFER FOR GROUP, REGACC PR# 0022
      *  BAH 040218 ADDED 11, LAST 4 SSNO SCAN FOR WORLD, PR# 339
      *  BAH 071024 ADDED 12, PHONE NUMBER SEARCH, REGENCY REGWAN PR# 3071
      *  BLM 130507 ADDED 13, BLKONE, TRANSFERS 1 ACCT TO ANOTHER BRANCH, #865
      *  MJD 160314 ADDED SP/SPDLMU - DIRECT LOAN BATCH PROCEDURES MENU  #1133
      *  BAH 2022.0601 ADDED 15, SECURITIZATION MENU SCMENU #1555
      * SMR 2024-0201 ADDED OTIS TRACE LOGGING LOGIC
      **************************************************************************
       COPY "LIBGB/MENU1A.CPY".
       COPY "LIBSP/SPMENU_DEF.CPY".
       COPY "LIBSP/SPMENU_WKS.CPY".
       COPY "LIBGB/MENU1B.CPY".
       COPY "LIBSP/SPMENU_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-START' SO THAT IT CAN GET THE EXACT PROGRAM
      *    NAME IN THE TRACE LOG.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "SPMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-START.

       DISPLAY-MENU.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH EXIT-PATHNAME.
           MOVE "SPMENU" TO VDU-FORM-NAME.
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

           GO TO M01 M02 M03 M04 M05
                 M06 M07 M08 M09 M10
                 M11 M12 M13 M14 M15
              DEPENDING ON FORM-BUF.

           GO TO GET-MENU-SELECTION.

       M01. MOVE "SP/UTMENU" TO FORM-NAM.
            GO TO RUN-PROG.
       M02. MOVE "LP/LPLSMU" TO FORM-NAM.
            GO TO RUN-PROG.
       M03. MOVE "LP/LPMNMU" TO FORM-NAM.
            GO TO RUN-PROG.
       M04. MOVE "SP/BPMENU" TO FORM-NAM.
            GO TO RUN-PROG.
       M05. MOVE "LP/LPCVMU" TO FORM-NAM.
            GO TO RUN-PROG.
       M06. MOVE "LP/BULKTR" TO FORM-NAM.
            GO TO RUN-PROG.
       M07. MOVE "LP/CLASTR" TO FORM-NAM.
            GO TO RUN-PROG.
       M08. GO TO GET-MENU-SELECTION.
       M09. GO TO GET-MENU-SELECTION.
       M10. MOVE "LP/BULKGR" TO FORM-NAM.
            GO TO RUN-PROG.
       M11. MOVE "SP/LAST4" TO FORM-NAM.
            GO TO RUN-PROG.
       M12. MOVE "SP/PHONE" TO FORM-NAM.
            GO TO RUN-PROG.
       M13. MOVE "LP/BLKONE" TO FORM-NAM.
            GO TO RUN-PROG.
       M14. MOVE "SP/SPDLMU" TO FORM-NAM.
            GO TO RUN-PROG.
       M15. MOVE "SP/SCMENU" TO FORM-NAM.
            GO TO RUN-PROG.

       RUN-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY SPMENU-SCREEN.

           MOVE FORM-PATH TO FORM-PATHNAME.
           CALL FORM-PROGX USING FORM-PATHNAME EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

           IF RETURN-CODE = 1
              GO TO EXIT-PROG.


      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-RETURN' SO THAT IT CAN GET THE EXACT
      *    PROGRAM NAME IN THE TRACE LOG
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "SPMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
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
           MOVE "SPMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-FINISH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY SPMENU-SCREEN.
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
              COPY "LIBSP/SPMENU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************
           MOVE SPMENU-HELP--FILE TO HELP-LINK-FILE.
           MOVE SPMENU-HELP--DIR TO HELP-LINK-DIR.

      ******************************************
       FORM-RESET SECTION.
      ******************************************
           DISPLAY SPMENU-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE SPMENU-FORM-FIELDS TO WR-BUF.
           MOVE SPMENU-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM CONAME-XFER.
       FORM-RESET-EXIT.
           EXIT.

      **************************************************
       MISC-ROUTINES SECTION.
      **************************************************
       COPY "LIBGB/SENDMESS.CPY".

      *=================================================================
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-RETURN' SO THAT IT CAN GET THE EXACT
      *    PROGRAM NAME IN THE TRACE LOG
      *=================================================================
       MISC-TRACE-RETURN.

           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "SPMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-RETURN.

