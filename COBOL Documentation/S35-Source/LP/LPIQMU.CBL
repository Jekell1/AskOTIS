       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LPIQMU.
      *********************************************************************
      *
      *  DIR : LP
      *  DESC: LOAN INQUIRY MENU
      *
      * REV:
      *  MJD ADDED BYINQ FOR SNAPSHOT STATISTIC INQ.
      *  MJD 031097 ADDED EMPTY PARAGRAPHS USED IN LIBGB/MENU:
      *             BEFORE-SEL: PERFORMED BEFORE ENTERING SELECTION.
      *             AFTER-SEL : PERFORMED AFTER ENTERING SELECTION.
      *  MJD 101697 ADDED DSINQ FOR SNAPSHOT STATISTIC INQ.
      *   CS 090398 ADDED LTINQ FOR GLOBAL VIN SCAN.
      *   CS 030403 ADDED NFSCN0 FOR SCAN OF CHECK/MO ENTERED AT PAYMENT
      *             POST [PR# 2034  CASHCO]
      *  BLM 110105 ADDED SELECTION 10, CREDIT SCORE STATISTICS INQUIRY,
      *             WORLD PR# 726
      *  MJD 121029 ADDED EXT-MENU-OPTION-FG
      *  BAH 20191030 REMOVED M09. MOVE "SP/NFSCN0" TO FORM-NAM. PER KM
      *  BAH 20211210 ADDED 11, CHECK INQUIRY (CKINQY) #1530
      *  BAH 2022.0314 REMOVED EXT-SPANISH-FORMS AND ALL MX FORMS
      * SMR 2024-0201 ADDED OTIS TRACE LOGGING LOGIC
      *********************************************************************
       COPY "LIBGB/MENU1A.CPY".
       COPY "LIBLP/LPIQMU_DEF.CPY".
       COPY "LIBLP/LPIQMU_WKS.CPY".
       COPY "LIBGB/MENU1B.CPY".
       COPY "LIBLP/LPIQMU_SCN.CPY".
       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-START' SO THAT IT CAN GET THE EXACT PROGRAM
      *    NAME IN THE TRACE LOG.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "LPIQMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-START.

       DISPLAY-MENU.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH EXIT-PATHNAME.
           MOVE "LPIQMU" TO VDU-FORM-NAME.
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
              MOVE 1 TO RETURN-CODE
              GO TO EXIT-PROG.

           IF VDUSTATUS NOT = "00"
              GO TO GET-MENU-SELECTION.

      * MUST INITIALIZE FOR MENU SELECTIONS, IF PREVIOUSLY RAN IN BATCH
      * THE EXT-ROUTE-BUF WONT BE CORRECT
           MOVE "00" TO EXT-ROUTE-BUF.

           GO TO M01 M02 M03 M04 M05 M06 M07 M08 M09 M10 M11
              DEPENDING ON FORM-BUF.

           GO TO GET-MENU-SELECTION.

       M01. MOVE "LP/BWINQ" TO FORM-NAM.
            GO TO RUN-PROG.
      * DSINQ SNAPSHOT STATS
       M02. MOVE "LP/DSINQ" TO FORM-NAM.
            MOVE "1"        TO EXT-MENU-OPTION-FG.
            GO TO RUN-PROG.
       M03. MOVE "LP/DSINQ" TO FORM-NAM.
            MOVE "2"        TO EXT-MENU-OPTION-FG.
            GO TO RUN-PROG.
      * BYINQ SNAPSHOT STATS
       M04. MOVE "LP/BYINQ" TO FORM-NAM.
            MOVE "1"        TO EXT-MENU-OPTION-FG.
            GO TO RUN-PROG.
      * BYINQ REAL TIME STATS
       M05. MOVE "LP/BYINQ" TO FORM-NAM.
            MOVE "2"        TO EXT-MENU-OPTION-FG.
            GO TO RUN-PROG.
       M06. MOVE "SP/SPAQMU" TO FORM-NAM.
            GO TO RUN-PROG.
       M07. MOVE "LP/FDFLEX" TO FORM-NAM.
            GO TO RUN-PROG.
      *GLOBAL VIN SCAN
       M08. MOVE "LP/LTINQ" TO FORM-NAM.
            GO TO RUN-PROG.
       M09.  GO TO GET-MENU-SELECTION.
       M10. MOVE "LP/SSINQ" TO FORM-NAM.
            GO TO RUN-PROG.
       M11. MOVE "LP/CKINQY" TO FORM-NAM.
            GO TO RUN-PROG.

       RUN-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LPIQMU-SCREEN.

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
           MOVE "LPIQMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
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
           MOVE "LPIQMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-FINISH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LPIQMU-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           GOBACK.

       COPY "LIBCL/TRACE.CPY".
       COPY "LIBGB/CONAME_XFER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *******************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************
           MOVE "2A" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LPIQMU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *******************************************
       SETUP-LINK-INFO SECTION.
      *******************************************
           MOVE LPIQMU-HELP--FILE TO HELP-LINK-FILE.
           MOVE LPIQMU-HELP--DIR TO HELP-LINK-DIR.


      ******************************************
       FORM-RESET SECTION.
      ******************************************
           DISPLAY LPIQMU-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE LPIQMU-FORM-FIELDS TO WR-BUF.
           MOVE LPIQMU-FORM-LIST   TO WR-LIST.
           PERFORM CALL-WRFORM.
           
           PERFORM CONAME-XFER.
       FORM-RESET-EXIT.
           EXIT.

      *******************************************
       MISC-ROUTINES SECTION.
      *******************************************
       COPY "LIBGB/SENDMESS.CPY".

      *=================================================================
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-RETURN' SO THAT IT CAN GET THE EXACT
      *    PROGRAM NAME IN THE TRACE LOG
      *=================================================================
       MISC-TRACE-RETURN.

           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "LPIQMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-RETURN.

