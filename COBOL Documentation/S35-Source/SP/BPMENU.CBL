       IDENTIFICATION  DIVISION.
       PROGRAM-ID.       BPMENU.
       AUTHOR.              MJD.
       DATE-WRITTEN. 2000/04/20.
      **************************************************************************
      *  DIR : SP
      *  DESC: BATCH ALLOTMENT FILE PREPARATION MENU
      *
      *REV:
      * JTG 010530 ADDED GP-TRUE-NEXT-BATCH                 MERC PR#243/251
      * JTG 011109 ADDED MONEYGRAM FILE OPTION              MERC PR#263
      * JTG 021024 ADDED NEW CODE FILE TYPE 'BR' - BATCH PAYMENT REFERENCE CODES
      *            BATCH PAYMENT REVISIONS                          REGACC #0006
      * JTG 021203 REMOVED GP-BATCH-PAY-REVISIONS
      *            BATCH PAYMENT REVISIONS                          REGACC #0006
      * JTG 040407 ADDED GP-OTREF-NEXT-BATCH  900-909              CENTRIX #2254
      * BLF 070213 ADDED SOLUTRAN IMPORT FOR WORLD NSF OTHER CHARGES AS
      *            SELECTION 9, MOVED PROCEDURE REPORT PRINT TO SELECTION 10,
      *            WORLD PR# 526
      * BLM 080408 ADDED SELECTION 10, LIFE OF THE SOUTH WEEKLY INSURANCE
      *            CLAIM EFT PROCESSING, MOVED PROCEDURE REPORT PRINT TO
      *            SELECTION 11, WORLD PR# 573
      * BLM 111011 ADDED SELECTION 12, MEXCIO UNION BATCH PAYMENT EXTRACTION,
      *            MOVED PROCEDURE REPORT PRINT TO SELECTION 12, WORLD PR# 770
      * BLM 161021 REMOVE 5 UNUSED SELECTIONS, 2-6, (BPBKB2-6) AND RENUMBER
      *            REMAINING SELECTIONS
      * BLM 161021 BPMENU ONLY HAD 1 SELECTION, BPMEN2, SO MOVE BPMEN2 TO
      *            BPMENU
      * SMR 2024.0201 ADDED OTIS TRACE LOGGING LOGIC
      * BAH 2024.0808 REMOVED 1. SP/BPBKBR FORT KNOX AS PER KEVIN S35Q-111
      **************************************************************************
       COPY "LIBGB/MENU1A.CPY".
       COPY "LIBSP/BPMENU_DEF.CPY".
       COPY "LIBSP/BPMENU_WKS.CPY".
       COPY "LIBGB/MENU1B.CPY".
       COPY "LIBSP/BPMENU_SCN.CPY".
       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-START' SO THAT IT CAN GET THE EXACT PROGRAM
      *    NAME IN THE TRACE LOG.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "BPMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-START.

       DISPLAY-MENU.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH EXIT-PATHNAME.
           MOVE "BPMENU" TO VDU-FORM-NAME.

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
              MOVE "SP/BPMENU" TO FORM-NAM
              GO TO EXIT-PROG.
           IF VDUSTATUS = "1<"
              MOVE "GB/PERUSE" TO FORM-NAM
              GO TO RUN-PROG.
           IF VDUSTATUS NOT = "00"
              GO TO GET-MENU-SELECTION.

           MOVE "00" TO EXT-ROUTE-BUF.

           GO TO M01 M02 M03 M04 M05 M06 M07
              DEPENDING ON FORM-BUF.
           GO TO GET-MENU-SELECTION.

       M01. GO TO GET-MENU-SELECTION.
       M02. MOVE "SP/BPREF"  TO FORM-NAM.
            GO TO RUN-PROG.
       M03. MOVE "SP/BPOTH"  TO FORM-NAM.
            GO TO RUN-PROG.
       M04. GO TO GET-MENU-SELECTION.
       M05. MOVE "SP/LOTSBP" TO FORM-NAM.
            GO TO RUN-PROG.
       M06. GO TO GET-MENU-SELECTION.
       M07. MOVE "SP/EOQRPT" TO FORM-NAM.
            GO TO RUN-PROG.

       RUN-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BPMENU-SCREEN.

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
           MOVE "BPMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
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
           MOVE "BPMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-FINISH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BPMENU-SCREEN.
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
              COPY "LIBSP/BPMENU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***********************************************
       SETUP-LINK-INFO SECTION.
      ***********************************************
           MOVE BPMENU-HELP--FILE TO HELP-LINK-FILE.
           MOVE BPMENU-HELP--DIR TO HELP-LINK-DIR.

      ******************************************
       FORM-RESET SECTION.
      ******************************************
           DISPLAY BPMENU-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE BPMENU-FORM-FIELDS TO WR-BUF.
           MOVE BPMENU-FORM-LIST TO WR-LIST.
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
           MOVE "BPMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-RETURN.

