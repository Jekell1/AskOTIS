       IDENTIFICATION DIVISION.
       PROGRAM-ID.      EXTRMU.
      ******************************************************************
      *
      *  DIR : SP
      *  DESC: MISC EXTRACTION / IMPORT MENU
      *
      *  REV:
      *  BAH 2024.0125 ADDED 27 UN1065 AND 28 BUYBAC #1640
      *  SMR 2024.0201 ADDED OTIS TRACE LOGGING LOGIC
      *  BAH 2024.0717 FORM HAD "IRS TIM" INSTEAD OF "IRS TIN" S35Q-91
      *  BAH 2025.0220 ADDED 29. AD&D MONITOR EXTRACTOIN #1701 S35Q-165
      ************************************************************************
       COPY "LIBGB/MENU1A.CPY".
       COPY "LIBSP/EXTRMU_DEF.CPY".
       COPY "LIBSP/EXTRMU_WKS.CPY".
       COPY "LIBGB/MENU1B.CPY".
       COPY "LIBSP/EXTRMU_SCN.CPY".
       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-START' SO THAT IT CAN GET THE EXACT PROGRAM
      *    NAME IN THE TRACE LOG.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "EXTRMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-START.

       DISPLAY-MENU.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH EXIT-PATHNAME.
           MOVE "EXTRMU" TO VDU-FORM-NAME.

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
              MOVE "SP/OPMENU" TO FORM-NAM
              GO TO EXIT-PROG.

           IF VDUSTATUS = "1<"
              MOVE "GB/PERUSE" TO FORM-NAM
              GO TO RUN-PROG.

           IF VDUSTATUS NOT = "00"
              GO TO GET-MENU-SELECTION.

      * MUST INITIALIZE FOR MENU SELECTIONS, IF PREVIOUSLY RAN IN BATCH
      * THE EXT-ROUTE-BUF WONT BE CORRECT
           MOVE "00" TO EXT-ROUTE-BUF.

           GO TO M01 M02 M03 M04 M05 M06 M07 M08 M09 M10
                 M11 M12 M13 M14 M15 M16 M17 M18 M19 M20
                 M21 M22 M23 M24 M25 M26 M27 M28 M29
              DEPENDING ON FORM-BUF.

           GO TO GET-MENU-SELECTION.

      *=================================================================
       M01. MOVE "SP/PITBO2" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M02. MOVE "SP/WFARGO" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M03. MOVE "SP/DWEXMU" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M04. MOVE "SP/PLIUPD" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M05. MOVE "SP/EMAIL" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M06. MOVE "SP/TINEXT" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M07. MOVE "SP/UNEXTR" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M08. MOVE "SP/TEXTIN" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M09. MOVE "SP/LEXBNK" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M10. MOVE "SP/DEBEXT" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M11. MOVE "SP/PLTREX" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M12. MOVE "SP/WACMEM" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M13. MOVE "SP/HIEREX" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M14. MOVE "SP/WFARG5" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M15. MOVE "SP/ACTION" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M16. MOVE "SP/SETOPT" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M17. MOVE "SP/REPAYE" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M18. MOVE "SP/PHOIMP" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M19. MOVE "SP/MISPAY" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M20. MOVE "SP/ILNEXT" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M21. MOVE "SP/ILUEXT" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M22. MOVE "SP/ILMEXT" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M23. MOVE "SP/ILMISC" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M24. MOVE "SP/ILMUPD" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M25. MOVE "SP/RTCEXT" TO FORM-NAM.
            GO TO RUN-PROG.
       M26.
           GO TO GET-MENU-SELECTION.
       M27. MOVE "SP/UN1065" TO FORM-NAM.
            GO TO RUN-PROG.
       M28. MOVE "SP/BUYBAC" TO FORM-NAM.
            GO TO RUN-PROG.
       M29. MOVE "SP/ADDEXT" TO FORM-NAM.
            GO TO RUN-PROG.

       RUN-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY EXTRMU-SCREEN.

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
           MOVE "EXTRMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
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
           MOVE "EXTRMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-FINISH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY EXTRMU-SCREEN.
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
              COPY "LIBSP/EXTRMU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************
           MOVE EXTRMU-HELP--FILE TO HELP-LINK-FILE.
           MOVE EXTRMU-HELP--DIR TO HELP-LINK-DIR.


      **************************************************
       FORM-RESET SECTION.
      **************************************************
           DISPLAY EXTRMU-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE EXTRMU-FORM-FIELDS TO WR-BUF.
           MOVE EXTRMU-FORM-LIST TO WR-LIST.
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
           MOVE "EXTRMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-RETURN.

