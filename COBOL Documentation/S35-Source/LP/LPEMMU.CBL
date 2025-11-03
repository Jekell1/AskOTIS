       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LPEMMU.
      ************************************************************************
      *  DIR : LP
      *  DESC: LOANS END-OF-MONTH MENU
      *
      * REV:
      *  BLL 082395 DISABLE EOM SELECTION (PUT EOMALL ON UTMENU)
      *  KEC 102795 ADDED F6-VIEW REPORTS LOGIC.
      *  KEC 103095 DISABLE BRANCH ANALYSIS SELECTION.
      *  BLV 021997 ADD EOMALL, SELECTION 7
      *  MJD 031097 ADDED EMPTY PARAGRAPHS USED IN LIBGB/MENU:
      *             BEFORE-SEL: PERFORMED BEFORE ENTERING SELECTION.
      *             AFTER-SEL : PERFORMED AFTER ENTERING SELECTION.
      *  BLV 043097 SELECTION 2 (WAS DELEOM) NOT USED
      *  MJD 012098 ADDED OPTION TO PRINT EARNINGS REPORTS
      *  GLB 010123 ADDED OPTION TO PRINT REVOLVING INS EARNINGS
      *  JTG 041110
      *             PROGRAMMED UNEARNED FEE CODE REPORT          LENDMARK 2387
      *   CS 060713 CHANGED #6 TO DEALER STATMENT MENU(DLSTMU) PR#2503 SEC AUTO
      *  BAH 190204 REMOVED 13. REVOLVING
      *  SMR 2024-0201 ADDED OTIS TRACE LOGGING LOGIC
      *  RMZ 2024-1008 CHANGED CALLING FROM 'LP/EOMALL' TO 
      *                'LP/EOMMNU' - S35Q-104
      *
      ************************************************************************

       COPY "LIBGB/MENU1A.CPY".
       COPY "LIBLP/LPEMMU_DEF.CPY".
       COPY "LIBLP/LPEMMU_WKS.CPY".
       COPY "LIBGB/MENU1B.CPY".
       COPY "LIBLP/LPEMMU_SCN.CPY".
       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-START' SO THAT IT CAN GET THE EXACT PROGRAM
      *    NAME IN THE TRACE LOG.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "LPEMMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-START.

       DISPLAY-MENU.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH EXIT-PATHNAME.
           MOVE "LPEMMU" TO VDU-FORM-NAME.

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

           GO TO M01 M02 M03 M04 M05 M06 M07 M08 M09 M10
                 M11 M12 M13 M14
                 DEPENDING ON FORM-BUF.

           GO TO GET-MENU-SELECTION.

       M01. MOVE "LP/ACRERN" TO FORM-NAM.
            GO TO RUN-PROG.
       M02. MOVE "SP/PRTERN" TO FORM-NAM.
            GO TO RUN-PROG.
       M03. GO TO GET-MENU-SELECTION.
       M04. MOVE "LP/BYANRP" TO FORM-NAM.
            GO TO RUN-PROG.
       M05. MOVE "LP/LNAGED" TO FORM-NAM.
            GO TO RUN-PROG.
       M06. GO TO GET-MENU-SELECTION.
       M07. MOVE "LP/EOMMNU" TO FORM-NAM.
            GO TO RUN-PROG.

      *--------------------OPTIONAL SELECTIONS-----------------
       M08. MOVE "LP/EMPUMU" TO FORM-NAM.
            GO TO RUN-PROG.
       M09. MOVE "LP/EMICMU" TO FORM-NAM.
            GO TO RUN-PROG.
       M10. MOVE "LP/ICUNRP" TO FORM-NAM.
            GO TO RUN-PROG.
       M11. MOVE "LP/LPEMRG" TO FORM-NAM.
            GO TO RUN-PROG.
       M12. MOVE "LP/LPCKMU" TO FORM-NAM.
            GO TO RUN-PROG.
      *M13. MOVE "LP/INEARN" TO FORM-NAM.
       M13. GO TO GET-MENU-SELECTION.
       M14. MOVE "LP/FEUNRP" TO FORM-NAM.
            GO TO RUN-PROG.

       RUN-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LPEMMU-SCREEN.

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
           MOVE "LPEMMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
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
           MOVE "LPEMMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-FINISH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LPEMMU-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           EXIT PROGRAM.

       COPY "LIBCL/TRACE.CPY".
       COPY "LIBGB/CONAME_XFER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ********************************************
       VDU-CONVERT-FIELD SECTION.
      ********************************************
           MOVE "2A" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LPEMMU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ********************************************
       SETUP-LINK-INFO SECTION.
      ********************************************
           MOVE LPEMMU-HELP--FILE TO HELP-LINK-FILE.
           MOVE LPEMMU-HELP--DIR TO HELP-LINK-DIR.


      ******************************************
       FORM-RESET SECTION.
      ******************************************
           DISPLAY LPEMMU-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE LPEMMU-FORM-FIELDS TO WR-BUF.
           MOVE LPEMMU-FORM-LIST TO WR-LIST.
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
           MOVE "LPEMMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-RETURN.

