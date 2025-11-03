       IDENTIFICATION DIVISION.
       PROGRAM-ID.      WRLDMU.
      **************************************************************************
      *
      *  DIR : SP
      *  DESC: CUSTOM WORLD MONTH-END PROCESSING MENU
      *
      * REV:
      * GLB 031107 ADD COMPARATIVE OPERATIONS
      * BAH 031113 ADDED INSURANCE STATISTICS
      * BLV 040128 ADDED BRANCH INSURANCE PRODUCER LISTING
      * BAH 040210 ADDED INSURANCE CLAIMS UPDATE TO TRFILE AND INSURANCE
      *            CLAIMS REGISTER
      * BLV 040224 ADDED BONUS REPORT/UPDATE, PROFICIENCY POINTS MAINT.
      * BLV 040305 ADDED SEL # 1, WR END-OF-MONTH, SEL 15, OFF-LINE BRANCH ENTRY
      * BAH 040317 ADDED 26. NONFILE INSURANCE, SP/NONFLE
      * CS  040324 ADDED 27. SUPERVISOR RATING SHEET BONUS (BONUS2)
      * BLV 040406 COMPLETELY REVISE MENU
      * JTG 040423 ADDED 21. PERFORMANCE RUNNING TOTALS REPORT
      * BLV 040430 ADDED 22. PROFICIENCY RATING REPORT
      * BLV 040607 ADD CLAIMS UPDATE PROGRAMS FOR LIFE OF THE SOUTH & MOTOR
      *            CLUB, BUMP SUBSEQUENT SELECTIONS DOWN 4
      * BLF 041216 ADD P2 NONFILE RECOVERIES RATING SHEET UPDATE, SELECTION
      *            11, BUMP SUBSEQUENT SELECTIONS DOWN 1
      * BLF 041221 SWITCH ORDER OF SELECTIONS 9, 10, 11, GL RATING SHEET
      *            UPDATE SHOULD BE LAST
      *  CS 050217 ADD 28. YTD PROFICENCY POINTS ALPHA (PP2YTD) WORLD PR#373
      * BLF 050404 MAKE SELECTION 9 (WRP2UP) INACTIVE, WORLD PR# 398
      * BLF 060503 CHANGE SELECTION 15 TO BE SUPERVISOR PROFICIENCY
      *            PERFORMANCE, MOVE COPY RISC2 FILES TO SELECTION 9 WHICH
      *            WAS INACTIVE, WORLD PR# 470
      * BLF 060505 ADD 29. SUPERVISOR PROFICIENCY RATING, WORLD PR#470
      * BLF 100824 ADD 31. NON-FILE YTD STATS REPORT, WORLD PR#704
      * BLM 160706 ADD 32. SUPERVISOR QUARTERLY BONUS EXTRACT/REPORT,
      *            WORLD PR#1166
      * MJD 170908 CHECKED FOR 8 DIGIT DATES AND DID DID NOT SEE ANYTHING, SKIP.
      *  CS 190401 REMOVE SELECTION 13, BONUS1
      * BAH 20190701 REMOVED 17. PPMAIN PROF POINTS PT0162
      * BAH 20190701 REMOVED 30. SUPBON PT0162
      * BAH 20190701 REMOVED 13,14,15,25,26,27,28,29,30 AND 32 PT0162
      * BAH 20200709 ADDED 33. GL RECONCILE REPORT BY STATE #1447 (GL/RECONC)
      * BAH 20211022 REMOVED 02. CLAIMS. WORLD SWITCHED TO CLAIM2 LOS
      *              REMOVED 03. CLUPTR. WORLD SWITCHED TO CLUPT2 LOS
      * BAH 20230427 REMOVED 23. CLRPTR, WORLD DOESNT USE
      * SMR 2024-0201 ADDED OTIS TRACE LOGGING LOGIC
      * BAH 2024.1008 33. RECONC WAS STILL BEING CALLED BUT THE PROMPT WAS 
      *               MISSING FROM THE FORM S35Q-138
      **************************************************************************
       COPY "LIBGB/MENU1A.CPY".
       COPY "LIBSP/WRLDMU_DEF.CPY".
       COPY "LIBSP/WRLDMU_WKS.CPY".
       COPY "LIBGB/MENU1B.CPY".
       COPY "LIBSP/WRLDMU_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-START' SO THAT IT CAN GET THE EXACT PROGRAM
      *    NAME IN THE TRACE LOG.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "WRLDMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-START.

       DISPLAY-MENU.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH EXIT-PATHNAME.
           MOVE "WRLDMU" TO VDU-FORM-NAME.

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
              MOVE "GL/GLMENU" TO FORM-NAM
              GO TO EXIT-PROG.    
           IF VDUSTATUS = "1<"
              MOVE "GB/PERUSE" TO FORM-NAM
              GO TO RUN-PROG.   
           IF VDUSTATUS NOT = "00"
              GO TO GET-MENU-SELECTION.

           MOVE "00" TO EXT-ROUTE-BUF.

           GO TO M01 M02 M03 M04 M05 M06 M07
                 M08 M09 M10 M11 M12 M13 M14
                 M15 M16 M17 M18 M19 M20 M21
                 M22 M23 M24 M25 M26 M27 M28
                 M29 M30 M31 M32 M33
                 DEPENDING ON FORM-BUF.
           GO TO GET-MENU-SELECTION.

       M01. MOVE "SP/WREOM1" TO FORM-NAM.
            GO TO RUN-PROG.
       M02. GO TO GET-MENU-SELECTION.
       M03. GO TO GET-MENU-SELECTION.
       M04. MOVE "SP/CLAIM2" TO FORM-NAM.
            GO TO RUN-PROG.
       M05. MOVE "SP/CLUPT2" TO FORM-NAM.
            GO TO RUN-PROG.
       M06. MOVE "SP/CLMCUP" TO FORM-NAM.
            GO TO RUN-PROG.
       M07. MOVE "SP/CLMCTR" TO FORM-NAM.
            GO TO RUN-PROG.
       M08. MOVE "SP/BYRSUP" TO FORM-NAM.
            GO TO RUN-PROG.
       M09. GO TO GET-MENU-SELECTION.
       M10. GO TO GET-MENU-SELECTION.
       M11. GO TO GET-MENU-SELECTION.
       M12. GO TO GET-MENU-SELECTION.
       M13. GO TO GET-MENU-SELECTION.
       M14. GO TO GET-MENU-SELECTION.
       M15. GO TO GET-MENU-SELECTION.
       M16. MOVE "MN/WRMAIN" TO FORM-NAM.
            GO TO RUN-PROG.
       M17. GO TO GET-MENU-SELECTION.
       M18. MOVE "SP/BRIPLS" TO FORM-NAM.
            GO TO RUN-PROG.
       M19. MOVE "SP/RATING" TO FORM-NAM.
            GO TO RUN-PROG.
       M20. GO TO GET-MENU-SELECTION.
       M21. GO TO GET-MENU-SELECTION.
       M22. MOVE "SP/ISTATS"  TO FORM-NAM.
            GO TO RUN-PROG.
       M23. GO TO GET-MENU-SELECTION.
       M24. MOVE "SP/NONFLE"  TO FORM-NAM.
            GO TO RUN-PROG.
       M25. GO TO GET-MENU-SELECTION.
       M26. GO TO GET-MENU-SELECTION.
       M27. GO TO GET-MENU-SELECTION.
       M28. GO TO GET-MENU-SELECTION.
       M29. GO TO GET-MENU-SELECTION.
       M30. GO TO GET-MENU-SELECTION.
       M31. MOVE "SP/YTDNF"  TO FORM-NAM.
            GO TO RUN-PROG.
       M32. GO TO GET-MENU-SELECTION.
       M33. MOVE "SP/RECONC"  TO FORM-NAM.
            GO TO RUN-PROG.
      *=================================================================

       RUN-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY WRLDMU-SCREEN.

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
           MOVE "WRLDMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
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
           MOVE "WRLDMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-FINISH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY WRLDMU-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           EXIT PROGRAM.

       COPY "LIBCL/TRACE.CPY".
       COPY "LIBGB/CONAME_XFER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "2A" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/WRLDMU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE WRLDMU-HELP--FILE TO HELP-LINK-FILE.
           MOVE WRLDMU-HELP--DIR TO HELP-LINK-DIR.

      ******************************************
       FORM-RESET SECTION.
      ******************************************
           DISPLAY WRLDMU-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE WRLDMU-FORM-FIELDS TO WR-BUF.
           MOVE WRLDMU-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM. 

           PERFORM CONAME-XFER.
       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       MISC-ROUTINES SECTION.
      *************************************************
       COPY "LIBGB/SENDMESS.CPY".

      *=================================================================
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-RETURN' SO THAT IT CAN GET THE EXACT
      *    PROGRAM NAME IN THE TRACE LOG
      *=================================================================
       MISC-TRACE-RETURN.

           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "WRLDMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-RETURN.

