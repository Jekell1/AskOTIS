       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LPCPMU.
      ******************************************************************
      *
      *  DIR : LP
      *  DESC: LOAN COLLECTIONS MENU #4 FROM MASTER MENU 
      *
      *  REV.
      *  KEC 102795 ADDED F6-VIEW REPORTS LOGIC.
      *  BLL 041896 ADDED SELECTIONS 5 - 10 FOR COLLECTOR QUEUE
      *  MJD 053196 ADDED REPO-DEFICIENCY BALANCE REPORT.
      *  MJD 031097 ADDED EMPTY PARAGRAPHS USED IN LIBGB/MENU:
      *             BEFORE-SEL: PERFORMED BEFORE ENTERING SELECTION.
      *             AFTER-SEL : PERFORMED AFTER ENTERING SELECTION.
      *   CS 061797 ADDED COLLECTOR RESCHEDULE LISTING
      *  MJD 000803 ADDED MN/TZMAIN- TIME ZONE FILE MAINT.
      *             (TFC PR#25 [TFC#11])
      *  BLV 000908 ADDED LP/BWTZRP- BORROWER TIME ZONE AUDIT REPORT
      *             (TFC PR#25 [TFC#11])
      *  MJD 001205 ADDED CPMAIN FOR GLOBAL POOL (TFC PR#60)
      *  BLV 001215 ADDED SELECTION FOR GLOBAL QUEUE LISTING, # 19, HAD
      *             TO BUMP "HIDDEN" SELECTIONS TO 20-22, TFC PR# 60
      *  BLV 010123 ADDED SELECTION FOR GLOBAL COLLECTOR JOURNAL, # 20,
      *             HAD TO BUMP "HIDDEN" SELECTIONS TO 21-23, TFC PR# 60
      *  BAH 090203 ADDED SEL # 21. REPO RE-NOTICE REPORTING FOR REGACC PR# 236
      *             HAD TO BUMP "HIDDEN" SELECTIONS TO 22-24
      *  BAH 130531 ADDED 25. FIRST 3 PAYMENT DEFAULT, HONOR PR# 4279
      *  BAH 131029 ADDED 26. DISPOSITION REPORT, REGACC PR# 344
      *  MJD 131209 ADDED EXIT-PATHNAME TO EQMAIN CALL.
      *  BLM 140415 FIX ARGUMENTS ON CALL OF EAMAIN, WAS GIVING MEMORY
      *             ACCESS ERROR, ADDED EXIT-PATHNAME & "N" TO CALL
      *  BAH 190129 CHANGED "CN" TO CALL CDBMAN AND NOT CDMAIN
      *   CS 190228 KEN HAD ME REMOVE ITEMS 24-28
      *  BAH 2022.0314 REMOVED EXT-SPANISH-FORMS AND ALL MX FORMS
      *  BAH 2023.0919 REMOVED CORPF3 M22, KM
      * SMR 2024-0201 ADDED OTIS TRACE LOGGING LOGIC
      ******************************************************************
       COPY "LIBGB/MENU1A.CPY".
       COPY "LIBLP/LPCPMU_DEF.CPY".
       COPY "LIBLP/LPCPMU_WKS.CPY".
       COPY "LIBGB/MENU1B.CPY".
       COPY "LIBLP/LPCPMU_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-START' SO THAT IT CAN GET THE EXACT PROGRAM
      *    NAME IN THE TRACE LOG.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "LPCPMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-START.

       DISPLAY-MENU.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH EXIT-PATHNAME.
           MOVE "LPCPMU" TO VDU-FORM-NAME.
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
              MOVE "LP/LPMENU" TO FORM-NAM
              GO TO EXIT-PROG.

           IF NOT ENTER-KEY
              GO TO GET-MENU-SELECTION.

      * MUST INITIALIZE FOR MENU SELECTIONS, IF PREVIOUSLY RAN IN BATCH
      * THE EXT-ROUTE-BUF WONT BE CORRECT
           MOVE "00" TO EXT-ROUTE-BUF.

           GO TO M01 M02 M03 M04 M05 M06 M07 M08
                 M09 M10 M11 M12 M13 M14 M15 M16
                 M17 M18 M19 M20 M21 
              DEPENDING ON FORM-BUF.

           GO TO GET-MENU-SELECTION.

       M01. MOVE "LP/CPRUN" TO FORM-NAM.
            GO TO RUN-PROG.
       M02. MOVE "LP/CPCORP" TO FORM-NAM.
            GO TO RUN-PROG.
       M03. MOVE "LP/CQRUN" TO FORM-NAM.
            GO TO RUN-PROG.
       M04. MOVE "SP/SPINQ" TO FORM-NAM.
            GO TO RUN-PROG.
       M05. MOVE "LS/CQLIST" TO FORM-NAM.
            GO TO RUN-PROG.
       M06. GO TO GET-MENU-SELECTION.
       M07. GO TO GET-MENU-SELECTION.
       M08. MOVE "MN/CDBMAN" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME "CN" "N".
            CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.
       M09. MOVE "MN/CPMAIN" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME " ".
            CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.
       M10. MOVE "MN/CDMAIN" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME "EA" "N".
            CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.
       M11. MOVE "MN/EAMAIN" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME "N".
            CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.
       M12. MOVE "MN/EQMAIN" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME "N".
            CANCEL FORM-PROGX. 

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.
       M13. MOVE "LS/CQRELS" TO FORM-NAM.
            GO TO RUN-PROG.
       M14. MOVE "SP/CJINQ" TO FORM-NAM.
            GO TO RUN-PROG.
      *  TFC'S COLLECTION REPORT:
       M15. GO TO GET-MENU-SELECTION.
       M16. MOVE "MN/TZMAIN" TO FORM-NAM.
            GO TO RUN-PROG.
       M17. GO TO GET-MENU-SELECTION.
       M18. MOVE "MN/CPMAIN" TO FORM-NAM.
            CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME "G".
            CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.
       M19. MOVE "LS/GQLIST" TO FORM-NAM.
            GO TO RUN-PROG.
       M20. MOVE "LP/COLRGB" TO FORM-NAM.
            GO TO RUN-PROG.
       M21. GO TO GET-MENU-SELECTION.

       RUN-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LPCPMU-SCREEN.

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
           MOVE "LPCPMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
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
           MOVE "LPCPMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-FINISH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LPCPMU-SCREEN.
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
              COPY "LIBLP/LPCPMU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***********************************************
       SETUP-LINK-INFO SECTION.
      ***********************************************
           MOVE LPCPMU-HELP--FILE TO HELP-LINK-FILE.
           MOVE LPCPMU-HELP--DIR TO HELP-LINK-DIR.


      ***********************************************
       FORM-RESET SECTION.
      ***********************************************
           DISPLAY LPCPMU-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE LPCPMU-FORM-FIELDS TO WR-BUF.
           MOVE LPCPMU-FORM-LIST   TO WR-LIST.
           PERFORM CALL-WRFORM.
           
           PERFORM CONAME-XFER.
       FORM-RESET-EXIT.
           EXIT.

      ***********************************************
       MISC-ROUTINES SECTION.
      ***********************************************
       COPY "LIBGB/SENDMESS.CPY".

      *=================================================================
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-RETURN' SO THAT IT CAN GET THE EXACT
      *    PROGRAM NAME IN THE TRACE LOG
      *=================================================================
       MISC-TRACE-RETURN.

           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "LPCPMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-RETURN.

