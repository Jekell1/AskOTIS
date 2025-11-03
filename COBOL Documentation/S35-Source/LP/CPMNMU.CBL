       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CPMNMU.
      ******************************************************************
      *
      *  DIR : LP
      *  DESC: COLLECTION MAINTENANCE MENU
      *
      * REV:
      *  MJD 120195 ADDED MN/CJMAIN : COLLECTOR JOURNAL MAINT.
      *  KEC 020296 ADDED F6 - VIEW REPORT LOGIC.
      *  KEC 020996 CHANGED THE CALLING OF MAINTENANCES FOR THE CD-FILE.
      *  MJD 031097 ADDED EMPTY PARAGRAPHS USED IN LIBGB/MENU:
      *             BEFORE-SEL: PERFORMED BEFORE ENTERING SELECTION.
      *             AFTER-SEL : PERFORMED AFTER ENTERING SELECTION.
      *  BLV 041597 ADD # 8 EQLIST
      *  MJD 071097 INSERTED "4. EXTRACTION/ACTION FILE COPY"
      *  MJD 001205 ADDED OPTION FOR GLOBAL VERSION OF CPMAIN
      *             (TFC PR#60)
      *  BLV 001214 ADDED OPTIONS 16 & 17 FOR GLOBAL COLLECTOR QUEUE
      *             MAINTENANCE & GLOBAL COLLECTOR QUEUE HEADER
      *             MAINTENANCE, TFC PR# 60
      *  MJD 131209 CHANGED CALL TO EQMAIN TO PASS EXIT-PATHNAME AND
      *             CHANGE-BRNO-FLAG (PULLED VALUE FROM FORM IN A15)
      *  BAH 190129 CHANGED "CN" TO CALL CDBMAN AND NOT CDMAIN
      *   CS 190301 COULDN'T GET TO ACTION CODE, WAS PASSING "}",
      *             CHANGED TO 'AC'
      *   CS 190308 CHANGED M13 TO CQSMAN(SUMMARY CQ)
      *   CS 190315 CHANGED M17 TO GQSMAN(SUMMARY GQ)
      *  BAH 2022.0420 ADDED 18. ACTION CODE 2, SECURITIZATION #1551
      *  SMR 2024.0201 ADDED OTIS TRACE LOGGING LOGIC
      *  BAH 2024.0925 "A2" ACTION CODE 2 WAS GOING TO RUN-PROG INSTEAD
      *                OF GET-MENU-SELECTION, RUNNING THE PROGRAM A SECOND
      *                TIME, HAD TO PRESS F7 TWICE TO EXIT. S35Q-130
      ******************************************************************
       COPY "LIBGB/MENU1A.CPY".
       COPY "LIBLP/CPMNMU_DEF.CPY".
       COPY "LIBLP/CPMNMU_WKS.CPY".
       COPY "LIBGB/MENU1B.CPY".
       COPY "LIBLP/CPMNMU_SCN.CPY".
       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-START' SO THAT IT CAN GET THE EXACT PROGRAM
      *    NAME IN THE TRACE LOG.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "CPMNMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-START.

       DISPLAY-MENU.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH EXIT-PATHNAME.
           MOVE "CPMNMU" TO VDU-FORM-NAME.

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

           IF F6-KEY
              MOVE "GB/PERUSE" TO FORM-NAM
              GO TO RUN-PROG.

           IF F7-KEY
              MOVE "LP/LPMNMU" TO FORM-NAM
              GO TO EXIT-PROG.

           IF NOT ENTER-KEY
              GO TO GET-MENU-SELECTION.

           MOVE "00" TO EXT-ROUTE-BUF.

           GO TO M01 M02 M03 M04 M05 M06 M07 M08 M09 M10
                 M11 M12 M13 M14 M15 M16 M17 M18
              DEPENDING ON FORM-BUF.

           GO TO GET-MENU-SELECTION.

      *-----EXTRACTION/ACTION FILE MAINTENANCE--------------------------
       M01. MOVE "MN/EAMAIN" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME "Y".
            CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.

      *-----EXTRACTION/ACTION FILE LISTING------------------------------
       M02. MOVE "LS/EALIST" TO FORM-NAM.
            GO TO RUN-PROG.

      *-----EXTRACTION/ACTION GROUP ID CODE MAINTENANCE-----------------
       M03. MOVE "MN/CDMAIN" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME "EA" " ".
            CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.

      *-----EXTRACTION/ACTION FILE COPY --------------------------------
       M04. MOVE "SP/EACOPY" TO FORM-NAM.
            GO TO RUN-PROG.

      *-----ACTION CODE FILE MAINTENANCE--------------------------------
       M05. MOVE "MN/CDMAIN" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME "AC" " ".
            CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.

      *-----ACTIVITY CODE FILE MAINTENANCE------------------------------
       M06. MOVE "MN/CDMAIN" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME "AT" " ".
            CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.

      *-----RESULT CODE FILE MAINTENANCE--------------------------------
       M07. MOVE "MN/CDMAIN" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME "RE" " ".
            CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.

      *-----COLLECTOR QUEUE SET-UP MAINTENANCE--------------------------
       M08. MOVE "MN/EQMAIN" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME "Y".
            CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.

      *-----COLLECTOR QUEUE SET-UP LISTING --------------------------
       M09. MOVE "LS/EQLIST" TO FORM-NAM.
            GO TO RUN-PROG.

      *-----COLLECTOR ID CODE FILE MAINTENANCE-----------CNMAIN
       M10. MOVE "MN/CDBMAN" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME "CN" " ".
            CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.

      *-----COLLECTOR JOURNAL FILE MAINTENANCE--------------------------
       M11. MOVE "MN/CJMAIN" TO FORM-NAM.
            GO TO RUN-PROG.

      *-----COLLECTOR QUEUE FILE MAINTENANCE----------------------------
       M12. MOVE "MN/CQMAIN" TO FORM-NAM.
            GO TO RUN-PROG.

      *-----COLLECTOR QUEUE (HEADER) FILE MAINTENANCE-------------------
       M13. MOVE "MN/CQSMAN" TO FORM-NAM.
            GO TO RUN-PROG.

      *-----COLLECTOR POOL FILE MAINTENANCE-------------------
       M14. MOVE "MN/CPMAIN" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME " ".
            CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.

      *-----COLLECTOR POOL FILE MAINTENANCE-------------------
       M15. MOVE "MN/CPMAIN" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME "G".
            CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.

      *-----GLOBAL COLLECTOR QUEUE FILE MAINTENANCE-----------
       M16. MOVE "MN/GQMAIN" TO FORM-NAM.
            GO TO RUN-PROG.

      *-----GLOBAL COLLECTOR QUEUE HEADER FILE MAINTENANCE----
       M17. MOVE "MN/GQSMAN" TO FORM-NAM.
            GO TO RUN-PROG.

      *-----ACTION CODE 2 FILE MAINTENANCE--------------------------------
       M18. MOVE "MN/CDMAIN" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME "A2" " ".

            PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.


       RUN-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CPMNMU-SCREEN.

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
           MOVE "CPMNMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
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
           MOVE "CPMNMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-FINISH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CPMNMU-SCREEN.
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
              COPY "LIBLP/CPMNMU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***************************************************
       SETUP-LINK-INFO SECTION.
      ***************************************************
           MOVE CPMNMU-HELP--FILE TO HELP-LINK-FILE.
           MOVE CPMNMU-HELP--DIR TO HELP-LINK-DIR.

      ***************************************************
       FORM-RESET SECTION.
      ***************************************************
           DISPLAY CPMNMU-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE CPMNMU-FORM-FIELDS TO WR-BUF.
           MOVE CPMNMU-FORM-LIST TO WR-LIST.
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
           MOVE "CPMNMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-RETURN.

