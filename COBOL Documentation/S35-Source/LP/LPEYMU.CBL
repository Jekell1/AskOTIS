       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LPEYMU.
      ******************************************************************
      *
      *  DIR : LP
      *  DESC: LOANS END-OF-YEAR MENU
      *
      *  REV.
      *  KEC 102795 ADDED F6-VIEW REPORTS LOGIC.
      *  MJD 031097 ADDED EMPTY PARAGRAPHS USED IN LIBGB/MENU:
      *             BEFORE-SEL: PERFORMED BEFORE ENTERING SELECTION.
      *             AFTER-SEL : PERFORMED AFTER ENTERING SELECTION.
      *  CS  020219 ADDED DEBT CANCELLATION [STALLINGS PR#1741]
      *  BLV 051005 ADDED SELECTION 10, CASH FLOW MATURITY REPORT
      *             TO MENU, WORLD PR# 425/426
      *  BAH 2023.1024 REMOVED 04. AUDLET, PROGRAM WAS QA'D
      * SMR 2024-0201 ADDED OTIS TRACE LOGGING LOGIC
      ******************************************************************
       COPY "LIBGB/MENU1A.CPY".
       COPY "LIBLP/LPEYMU_DEF.CPY".
       COPY "LIBLP/LPEYMU_WKS.CPY".
       COPY "LIBGB/MENU1B.CPY".
       COPY "LIBLP/LPEYMU_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-START' SO THAT IT CAN GET THE EXACT PROGRAM
      *    NAME IN THE TRACE LOG.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "LPEYMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-START.

       DISPLAY-MENU.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH EXIT-PATHNAME.
           MOVE "LPEYMU" TO VDU-FORM-NAME.

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
           IF ( VDUSTATUS = "1<" )
              MOVE "GB/PERUSE" TO FORM-NAM
              GO TO RUN-PROG.
           IF VDUSTATUS = "1>"
              MOVE "LP/LPMENU" TO FORM-NAM
              GO TO EXIT-PROG.
           IF VDUSTATUS NOT = "00"
              GO TO GET-MENU-SELECTION.

      * MUST INITIALIZE FOR MENU SELECTIONS, IF PREVIOUSLY RAN IN BATCH
      * THE EXT-ROUTE-BUF WONT BE CORRECT
           MOVE "00" TO EXT-ROUTE-BUF.

           GO TO M01 M02 M03 M04 M05 M06 
              DEPENDING ON FORM-BUF.

           GO TO GET-MENU-SELECTION.

       M01. MOVE "LP/UPDEOY" TO FORM-NAM.
            GO TO RUN-PROG.
       M02. MOVE "LP/BNKRP1" TO FORM-NAM.
            GO TO RUN-PROG.
       M03. GO TO GET-MENU-SELECTION.
       M04. GO TO GET-MENU-SELECTION.
       M05. MOVE "LP/DC1099" TO FORM-NAM.
            GO TO RUN-PROG.
       M06. MOVE "LP/SUMPMT" TO FORM-NAM.
            GO TO RUN-PROG.

       RUN-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LPEYMU-SCREEN.

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
           MOVE "LPEYMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
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
           MOVE "LPEYMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-FINISH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LPEYMU-SCREEN.
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
              COPY "LIBLP/LPEYMU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***********************************************
       SETUP-LINK-INFO SECTION.
      ***********************************************
           MOVE LPEYMU-HELP--FILE TO HELP-LINK-FILE.
           MOVE LPEYMU-HELP--DIR TO HELP-LINK-DIR.


      ******************************************
       FORM-RESET SECTION.
      ******************************************
           DISPLAY LPEYMU-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE LPEYMU-FORM-FIELDS TO WR-BUF.
           MOVE LPEYMU-FORM-LIST TO WR-LIST.
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
           MOVE "LPEYMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-RETURN.

