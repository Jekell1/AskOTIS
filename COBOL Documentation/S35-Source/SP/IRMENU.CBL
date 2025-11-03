       IDENTIFICATION DIVISION.
       PROGRAM-ID.      IRMENU.
      **********************************************************************
      *
      *  DIR : SP
      *  DESC: IRS MENU
      *
      * REV:
      *  MJD 031097 ADDED EMPTY PARAGRAPHS USED IN LIBGB/MENU:
      *             BEFORE-SEL: PERFORMED BEFORE ENTERING SELECTION.
      *             AFTER-SEL : PERFORMED AFTER ENTERING SELECTION.
      *  BAH 010598 REMOVED THE CLEAR OR CREATE OPTION # 2 SPCLSP
      *  MJD 001103 ADDED 1099-MISC EXTRACTION FOR AP (WORLD PR#245)
      *  BLF 050728 ADDED LOAN 1099 EXTRACTION FOR 1099-C AS SELECTION
      *             5, BUMPED IRPREP TO SELECTION 6, WORLD PR# 411
      *  BLF 050922 ADDED F6 PERUSE OPTION, WORLD PR# 411
      *  BLF 060628 IF I-AM-MODERN, EXECUTE MO1099 INSTEAD OF LN1099
      *             FOR SELECTION 5, MODERN PR# 2713
      *  BLF 061107 ADD SELECTION 7, CUSTOM NOTES 1099 PRINT FOR
      *             REGENCY, REGWAN PR# 2859
      *  BAH 140502 RENAMED 1099MI TO MI1099, VERYANT CANT HAVE START NUMBERS
      *  BAH 20191031 REMOVED 4. MI1099 - USES AP FILES
      * SMR 2024-0201 ADDED OTIS TRACE LOGGING LOGIC
      **********************************************************************
   
       COPY "LIBGB/MENU1A.CPY".
       COPY "LIBSP/IRMENU_DEF.CPY".
       COPY "LIBSP/IRMENU_WKS.CPY".
       COPY "LIBGB/MENU1B.CPY".
       COPY "LIBSP/IRMENU_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-START' SO THAT IT CAN GET THE EXACT PROGRAM
      *    NAME IN THE TRACE LOG.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "IRMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-START.

       DISPLAY-MENU.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH EXIT-PATHNAME.
           MOVE "IRMENU" TO VDU-FORM-NAME.

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

           MOVE "00" TO EXT-ROUTE-BUF.

           GO TO M01 M02 M03 M04 M05 M06
              DEPENDING ON FORM-BUF.
           GO TO GET-MENU-SELECTION.
       M01. MOVE "SP/IIMAIN" TO FORM-NAM.
            GO TO RUN-PROG.
       M02. MOVE "SP/IR1098" TO FORM-NAM.
            GO TO RUN-PROG.
       M03. GO TO GET-MENU-SELECTION.
       M04. GO TO GET-MENU-SELECTION.
       M05. MOVE "SP/LN1099" TO FORM-NAM.
            GO TO RUN-PROG.
       M06. MOVE "SP/IRPREP" TO FORM-NAM.
            GO TO RUN-PROG.

       RUN-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY IRMENU-SCREEN.

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
           MOVE "IRMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
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
           MOVE "IRMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-FINISH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY IRMENU-SCREEN.
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
              COPY "LIBSP/IRMENU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ********************************************
       SETUP-LINK-INFO SECTION.
      ********************************************
           MOVE IRMENU-HELP--FILE TO HELP-LINK-FILE.
           MOVE IRMENU-HELP--DIR TO HELP-LINK-DIR.

      ******************************************
       FORM-RESET SECTION.
      ******************************************
           DISPLAY IRMENU-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE IRMENU-FORM-FIELDS TO WR-BUF.
           MOVE IRMENU-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM CONAME-XFER.
       FORM-RESET-EXIT.
           EXIT.

      ********************************************
       MISC-ROUTINES SECTION.
      ********************************************
       COPY "LIBGB/SENDMESS.CPY".

      *=================================================================
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-RETURN' SO THAT IT CAN GET THE EXACT
      *    PROGRAM NAME IN THE TRACE LOG
      *=================================================================
       MISC-TRACE-RETURN.

           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "IRMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-RETURN.

