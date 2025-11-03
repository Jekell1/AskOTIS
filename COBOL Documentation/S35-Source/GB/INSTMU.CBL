       IDENTIFICATION DIVISION.
       PROGRAM-ID.      INSTMU.
      ******************************************************************
      *
      *  DIR : GB
      *  DESC: INSTALL MENU
      *
      * REV:
      * MJD ADDED CAN LOAN COPY/DELETE
      * BLV 981212 ADDED NOTES GL INTERFACE COPY (NICOPY)
      *  MG 991007 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      * BLV 991227 REMOVE SELECTION 17 - GLCOPY, MOVE SELECTION 20
      *            (NICOPY) TO SELECTION 17
      * GLB 000511 MODIFY TO FIT INTO CL/PGMENU F12 SPECIAL WINDOW
      * BLV 010606 ADD SELECTION 21 - SPMERG, COPY STATE PARAMETERS
      *            FROM 1 FILE TO ANOTHER
      * BLV 020509 ADD SELECTION 22 - EOMERG, COPY BATCH PROCEDURES
      *            FROM 1 FILE TO ANOTHER
      * BLV 021121 ADD SELECTION 23 - CPCOPY, COLLECTOR POOL COPY
      * BAH 030424 ADD SELECTION 24 - LCCOPY, LOAN CLASS COPY
      * BAH 040115 ADD SELECTION 25 - ICCOPY, INSURANCE COMPANY COPY
      * BAH 040615 ADD SELECTION 26 - DLCOPY, DEALER MASTER FILE COPY
      * BLF 060414 ADD SELECTION 27 - BRBPCP, COPY BRANCH BATCH OPTIONS
      * BAH 151215 ADDED 28. HOLIDA, COPY GLOBAL HOLIDAY, REGENCY PR#4908
      * BAH 190607 ADDED 29. IRCOPY, INSURANCE RATE TABLE COPY #QA0016
      * BAH 20190802 REMOVED SEVERAL ITEMS, REPLACED CRDATA WITH CRMISS
      *  CS 20190923 REPLACE 6-GBMNAL WITH GPMAIN.  GPMAIN NO LONGER
      *              EXISTS, MOVED OLD GBMNAL TO GBMAIN
      * BAH 20191118 REMOVED 19. GTMERG, CANT MERGE 2 DATA BASE FILES
      * BAH 20191118 REMOVED 22. EOMERG, CANT MERGE 2 DATA BASE FILES
      * BAH 20191118 REMOVED 18. FSCOPY, CANT MERGE 2 DATA BASE FILES
      * BAH 20200129 REMOVED 2. CRWIPE, FOR UP1534 ONLY
      * BAH 20220307 REMOVED 1. CRDATA, CANNOT USE WITH SQL ANYMORE
      * SMR 2024-0201 ADDED OTIS TRACE LOGGING LOGIC
      ******************************************************************
       COPY "LIBGB/MENU1A.CPY".
       COPY "LIBGB/INSTMU_DEF.CPY".
       COPY "LIBGB/INSTMU_WKS.CPY".
       COPY "LIBGB/MENU1B.CPY".
       COPY "LIBGB/INSTMU_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-START' SO THAT IT CAN GET THE EXACT PROGRAM
      *    NAME IN THE TRACE LOG.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "INSTMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-START.

       DISPLAY-MENU.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH EXIT-PATHNAME.
           MOVE "INSTMU" TO VDU-FORM-NAME.

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

           IF VDUSTATUS = "1>"
              MOVE "LP/LPMENU" TO FORM-NAM
              GO TO EXIT-PROG.

           IF VDUSTATUS = "12"
              MOVE " " TO FORM-NAM
              GO TO EXIT-PROG.

           IF VDUSTATUS = "18"
              MOVE "FX/FXMENU" TO FORM-NAM
              GO TO RUN-PROG.

           IF VDUSTATUS NOT = "00"
              GO TO GET-MENU-SELECTION.

           MOVE "00" TO EXT-ROUTE-BUF.

           GO TO M01 M02 M03 M04 M05 M06 M07 M08 M09 M10
                 M11 M12 M13 M14 M15 M16 M17 M18 M19 M20
                 M21 M22 M23 M24 M25 M26 M27 M28 M29
              DEPENDING ON FORM-BUF.

           GO TO GET-MENU-SELECTION.

       M01. MOVE "SP/CRDATA" TO FORM-NAM.
            GO TO RUN-PROG.
       M02. GO TO GET-MENU-SELECTION.
       M03. GO TO GET-MENU-SELECTION.
       M04. GO TO GET-MENU-SELECTION.
       M05. MOVE "SP/GBCOPY" TO FORM-NAM.
            GO TO RUN-PROG.
       M06. MOVE "GB/GBMAIN" TO FORM-NAM.
            GO TO RUN-PROG.
       M07. MOVE "GL/DELGL1" TO FORM-NAM.
            GO TO RUN-PROG.
       M08. MOVE "GL/BRANCH" TO FORM-NAM.
            GO TO RUN-PROG.
       M09. MOVE "SP/CLCOPY" TO FORM-NAM.
            GO TO RUN-PROG.
       M10. MOVE "SP/CLDELE" TO FORM-NAM.
            GO TO RUN-PROG.
       M11. GO TO GET-MENU-SELECTION.
       M12. MOVE "SP/BRCOPY" TO FORM-NAM.
            GO TO RUN-PROG.
       M13. MOVE "SP/EACOPY" TO FORM-NAM.
            GO TO RUN-PROG.
       M14. MOVE "SP/GICOPY" TO FORM-NAM.
            GO TO RUN-PROG.
       M15. MOVE "SP/SPCOPY" TO FORM-NAM.
            GO TO RUN-PROG.
       M16. MOVE "SP/CDCOPY" TO FORM-NAM.
            GO TO RUN-PROG.
       M17. GO TO GET-MENU-SELECTION.
       M18. GO TO GET-MENU-SELECTION.
       M19. GO TO GET-MENU-SELECTION.
       M20. MOVE "SP/CNCOPY" TO FORM-NAM.
            GO TO RUN-PROG.
       M21. GO TO GET-MENU-SELECTION.
       M22. GO TO GET-MENU-SELECTION.
       M23. MOVE "SP/CPCOPY" TO FORM-NAM.
            GO TO RUN-PROG.
       M24. MOVE "SP/LCCOPY" TO FORM-NAM.
            GO TO RUN-PROG.
       M25. MOVE "SP/ICCOPY" TO FORM-NAM.
            GO TO RUN-PROG.
       M26. MOVE "SP/DLCOPY" TO FORM-NAM.
            GO TO RUN-PROG.
       M27. MOVE "SP/BRBPCP" TO FORM-NAM.
            GO TO RUN-PROG.
       M28. MOVE "SP/HOLIDA" TO FORM-NAM.
            GO TO RUN-PROG.
       M29. MOVE "SP/IRCOPY" TO FORM-NAM.
            GO TO RUN-PROG.

       RUN-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY INSTMU-SCREEN.

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
           MOVE "INSTMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
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
           MOVE "INSTMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-FINISH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY INSTMU-SCREEN.
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
              COPY "LIBGB/INSTMU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***********************************************
       SETUP-LINK-INFO SECTION.
      ***********************************************
           MOVE INSTMU-HELP--FILE TO HELP-LINK-FILE.
           MOVE INSTMU-HELP--DIR  TO HELP-LINK-DIR.

      ******************************************
       FORM-RESET SECTION.
      ******************************************
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY INSTMU-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE INSTMU-FORM-FIELDS TO WR-BUF.
           MOVE INSTMU-FORM-LIST TO WR-LIST.
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
           MOVE "INSTMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-RETURN.

