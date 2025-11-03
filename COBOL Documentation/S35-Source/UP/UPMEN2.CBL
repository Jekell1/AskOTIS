       IDENTIFICATION DIVISION.
       PROGRAM-ID.      UPMEN2.
       AUTHOR.          KEC.
       DATE-WRITTEN.    02-27-2019.
      ******************************************************************
      *
      * IDENTIFICATION: UP/UPMEN2.CBL
      *
      * DESCRIPTION   : UPGRADE CONVERSION MENU - PAGE 2
      *
      *=================================================================
      * REV:
      * KEC 2024-0409 ***NEW***
      * KEC 2025-0411 ADDED THE FOLLOWING:
      *         + 8. CREATE NOTICE FILE
      *         + 9. UPGRADE: COPY EO FILES (EO1534)
      ******************************************************************
       COPY "LIBGB/MENU1A.CPY".
       COPY "LIBUP/UPMEN2_DEF.CPY".
       COPY "LIBUP/UPMEN2_WKS.CPY".
       COPY "LIBGB/MENU1B.CPY".
       COPY "LIBUP/UPMEN2_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-START' SO THAT IT CAN GET THE EXACT PROGRAM
      *    NAME IN THE TRACE LOG.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "UPMEN2"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-START.

       DISPLAY-MENU.
      D    STOP FORM-PATHNAME.

           MOVE FORM-PATHNAME              TO FORM-PATH   EXIT-PATHNAME.
           MOVE "UPMEN2"                   TO VDU-FORM-NAME.
           PERFORM FORM-RESET.

           ACCEPT GP-REC FROM ENVIRONMENT "GPENV".

           MOVE FORM-PATH TO CHKSEC-FORM-PATH.
           CALL "CHKSEC"  USING CHKSEC-FIELDS.
           CANCEL "CHKSEC".

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

           IF ( CHKSEC-STAT NOT = "00" )
              MOVE "LP/LPMENU"             TO FORM-NAM
              GO TO EXIT-PROG.

       GET-MENU-SELECTION-CLR.

           MOVE ZEROES                     TO VDU-PROGRAMMING.

       GET-MENU-SELECTION.

           MOVE FORM-LIST                  TO WR-LIST.
           MOVE SPACES                     TO WR-BUF.
           PERFORM CALL-WRFORM.

           MOVE "E  N020SEL."              TO VDU.
           PERFORM SCREENIO.
           MOVE VDUNUM0                    TO FORM-BUF.

           IF ( F1-KEY )
              MOVE 1                       TO RETURN-CODE
              GO TO EXIT-PROG.

           IF ( F6-KEY )
              MOVE "GB/PERUSE"             TO FORM-NAM
              GO TO RUN-PROG.

           IF ( F7-KEY )
              MOVE "UP/UPMENU"             TO FORM-NAM
              GO TO EXIT-PROG.

           IF ( F9-KEY )                   *> SQL INFO WINDOW
              MOVE "GB/SQLINF" TO FORM-NAM
              GO TO RUN-PROG.

           IF ( NOT ENTER-KEY )
              GO TO GET-MENU-SELECTION.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MUST INITIALIZE FOR MENU SELECTIONS,
      *    IF PREVIOUSLY RAN IN BATCH THE EXT-ROUTE-BUF WONT BE CORRECT
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "00"                       TO EXT-ROUTE-BUF.

           GO TO M01 M02 M03 M04 M05 M06 M07 M08 M09 M10
              DEPENDING ON FORM-BUF.
           GO TO GET-MENU-SELECTION.

      *=================================================================
      *
      *    M E N U - S E L E C T I O N
      *
      *=================================================================
       M01. MOVE "UP/IF1534" TO FORM-NAM; GO TO RUN-PROG.
       M02. MOVE "UP/GR15DA" TO FORM-NAM; GO TO RUN-PROG.
       M03. MOVE "UP/UP15VY" TO FORM-NAM; GO TO RUN-PROG.
       M04. MOVE "UP/TR15VY" TO FORM-NAM; GO TO RUN-PROG.
       M05. MOVE "UP/UP3234" TO FORM-NAM; GO TO RUN-PROG.
       M06. MOVE "FX/BRMACH" TO FORM-NAM; GO TO RUN-PROG.
       M07. MOVE "FX/BIBUID" TO FORM-NAM; GO TO RUN-PROG.
       M08. MOVE "SP/CRNOFL" TO FORM-NAM; GO TO RUN-PROG.
       M09. MOVE "UP/EO1534" TO FORM-NAM; GO TO RUN-PROG.
       M10. GO TO GET-MENU-SELECTION.

      *=================================================================
      *
      *    R U N - P R O G
      *
      *=================================================================
       RUN-PROG.

           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY UPMEN2-SCREEN.

           MOVE FORM-PATH              TO FORM-PATHNAME.
           CALL FORM-PROGX          USING FORM-PATHNAME   EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

           IF ( RETURN-CODE = 1 )
              GO TO EXIT-PROG.


      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-RETURN' SO THAT IT CAN GET THE EXACT
      *    PROGRAM NAME IN THE TRACE LOG
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "UPMEN2"      TO EXT-TCLP-TRACE-FORM-PROGX.
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
           MOVE "UPMEN2"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-FINISH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE FORM-PATH                  TO FORM-PATHNAME.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY UPMEN2-SCREEN.
           EXIT PROGRAM.

      ******************************************************************
       FORM-RESET SECTION.
      ******************************************************************

           DISPLAY UPMEN2-SCREEN         UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE UPMEN2-FORM-FIELDS         TO WR-BUF.
           MOVE UPMEN2-FORM-LIST           TO WR-LIST.
           PERFORM CALL-WRFORM.
           PERFORM CONAME-XFER.

       FORM-RESET-EXIT.
           EXIT.

      ******************************************************************
       SETUP-LINK-INFO SECTION.
      ******************************************************************

           MOVE UPMEN2-HELP--FILE          TO HELP-LINK-FILE.
           MOVE UPMEN2-HELP--DIR           TO HELP-LINK-DIR.

       SETUP-LINK-INFO-EXIT.
           EXIT.

      ******************************************************************
       VDU-CONVERT-FIELD SECTION.
      ******************************************************************

           MOVE "2A"                       TO STAT.
           MOVE VDU-FIELD-TO-CONVERT       TO VDU-ALP-FIELD.

           EVALUATE TRUE
              COPY "LIBUP/UPMEN2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       VDU-CONVERT-FIELD-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************

       COPY "LIBCL/TRACE.CPY".
       COPY "LIBGB/CONAME_XFER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ******************************************************************
       MISC-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/SENDMESS.CPY".

      *=================================================================
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-RETURN' SO THAT IT CAN GET THE EXACT
      *    PROGRAM NAME IN THE TRACE LOG
      *=================================================================
       MISC-TRACE-RETURN.

           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "UPMEN2"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-RETURN.

