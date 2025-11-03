       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LPCVMU.
       AUTHOR.             MJD.
       DATE-WRITTEN.  11/06/97.
      *******************************************************
      *  DIR : LP
      *  DESC: CONVERSION OPTIONS MENU
      *
      * REV:
      *  MJD 031097 ADDED EMPTY PARAGRAPHS USED IN LIBGB/MENU:
      *             BEFORE-SEL: PERFORMED BEFORE ENTERING SELECTION.
      *             AFTER-SEL : PERFORMED AFTER ENTERING SELECTION.
      *  BAH 060116 ADDED ALPHA ORDER CONVERSION REPORT, WORLD PR# 451
      *  BAH 060912 ADDED 4. BANKRUPT BULK CONVERSION (SP/CONVON) WORLD #490
      *  BAH 081022 ADDED 5. CONVERSION LABEL EXTRACT (SP/CLABEL) WORLD PR#606
      * SMR 2024-0201 ADDED OTIS TRACE LOGGING LOGIC
      *******************************************************
       COPY "LIBGB/MENU1A.CPY".
       COPY "LIBLP/LPCVMU_DEF.CPY".
       COPY "LIBLP/LPCVMU_WKS.CPY".
       COPY "LIBGB/MENU1B.CPY".
       COPY "LIBLP/LPCVMU_SCN.CPY".
       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-START' SO THAT IT CAN GET THE EXACT PROGRAM
      *    NAME IN THE TRACE LOG.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "LPCVMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-START.

       DISPLAY-MENU.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH EXIT-PATHNAME.
           MOVE "LPCVMU" TO VDU-FORM-NAME.
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
              MOVE "SP/SPMENU" TO FORM-NAM
              GO TO EXIT-PROG.

           IF VDUSTATUS = "1<"
              MOVE "GB/PERUSE" TO FORM-NAM
              GO TO RUN-PROG.

           IF VDUSTATUS NOT = "00"
              GO TO GET-MENU-SELECTION.

      * MUST INITIALIZE FOR MENU SELECTIONS, IF PREVIOUSLY RAN IN BATCH
      * THE EXT-ROUTE-BUF WONT BE CORRECT
           MOVE "00" TO EXT-ROUTE-BUF.

           GO TO M01 M02 M03
              DEPENDING ON FORM-BUF.

           GO TO GET-MENU-SELECTION.

       M01.
            DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
            DESTROY LPCVMU-SCREEN.
            DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
            MOVE "LP/LONI" TO FORM-NAM.
      * NEEDS TO PASS BKCVFG-BUF TO IDENTIFY BULK PURCHASE OR CONVERSION
            CALL "LONI" USING FORM-PATH EXIT-PATHNAME "C".
            CANCEL "LONI".

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

      * 1/3/2014 BAH MENU WAS NOT DISPLAYING IF YOU F7 FROM CONVERSION
            PERFORM FORM-RESET.
            GO TO GET-MENU-SELECTION.
       M02. MOVE "LP/CONVRP" TO FORM-NAM.
            GO TO RUN-PROG.
       M03. MOVE "LP/CONVAL" TO FORM-NAM.
            GO TO RUN-PROG.

       RUN-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LPCVMU-SCREEN.

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
           MOVE "LPCVMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
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
           MOVE "LPCVMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-FINISH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LPCVMU-SCREEN.
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
              COPY "LIBLP/LPCVMU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***********************************************
       SETUP-LINK-INFO SECTION.
      ***********************************************
           MOVE LPCVMU-HELP--FILE TO HELP-LINK-FILE.
           MOVE LPCVMU-HELP--DIR TO HELP-LINK-DIR.


      ***********************************************
       FORM-RESET SECTION.
      ***********************************************
           DISPLAY LPCVMU-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE LPCVMU-FORM-FIELDS TO WR-BUF.
           MOVE LPCVMU-FORM-LIST TO WR-LIST.
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
           MOVE "LPCVMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-RETURN.

