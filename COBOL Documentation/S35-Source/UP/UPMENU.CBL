       IDENTIFICATION DIVISION.
       PROGRAM-ID.      UPMENU.
       AUTHOR.          KEC.
       DATE-WRITTEN.    02-27-2019.
      ******************************************************************
      *
      * IDENTIFICATION: UP/UPMENU.CBL
      *
      * DESCRIPTION   : UPGRADE CONVERSION MENU
      *
      *=================================================================
      * REV:
      * KEC 2015-1007 ***NEW***
      * KEC 2019-0812 REVISED FROM UP3032 TO UP1534 UPGRADE
      *  CS 2019-0821 ADD FXGBDT #13, SETS EOM/EOY DATES IN GLOBAL FILE
      * KEC 2019-0926 REMOVED SP/FDCOPY (#12); MOVED FX/FXGBDT FROM 
      *               #13 TO #12.
      * KEC 2019-1008 PT#0188 CHANGED #1 FROM GB/CRMISS TO GB/CRWIPE
      * KEC 2020-0210 PT#0210 MOVED #4 TR1534 TO BE #5;
      *                       ADDED #4 UP3034.
      * KEC 2020-0305 PT#0173 MOVED #7 BRMACH TO BE #10;
      *                       ADDED #6 EO1534.
      * KEC 2020-0821 ADDED #19 GR15DA
      * KEC 2020-0831 CHANGED #4 UP3034 TO UP3234
      * KEC 2023-0425 REMOVED THE MENU OPTION OF RUNNING UP/CRWIPE;
      *          SINCE WE ARE NO LONGER USING ACU4GL WE CANNOT USE
      *          CRWIPE TO CREATE THE TABLES WITHIN THE DATABASE;
      *          WILL NEED A NEW PROCESS FOR A DBA TO USE FOR CREATING
      *          NEW TABLE IN THE FUTURE.
      * KEC 2024-0129 ADDED #13 SP/CRNOFL
      * SMR 2024-0201 ADDED OTIS TRACE LOGGING LOGIC
      * KEC 2024-0215 ADDED #3 UP/DATE35; SHUFFLED #4 THRU 8 DOWN
      * KEC 2024-0219 ADDED F9-KEY FOR SQL INFO WINDOW
      * KEC 2024-0409 REVISED FOR CHANGES IN PROCEDURE
      * KEC 2025-0411 REVISED FOR CHANGES IN PROCEDURE
      ******************************************************************
       COPY "LIBGB/MENU1A.CPY".
       COPY "LIBUP/UPMENU_DEF.CPY".
       COPY "LIBUP/UPMENU_WKS.CPY".
       COPY "LIBGB/MENU1B.CPY".
       COPY "LIBUP/UPMENU_SCN.CPY".

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
           MOVE "UPMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-START.

       DISPLAY-MENU.
      D    STOP FORM-PATHNAME.

           MOVE FORM-PATHNAME              TO FORM-PATH   EXIT-PATHNAME.
           MOVE "UPMENU"                   TO VDU-FORM-NAME.
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

           IF ( F4-KEY )
              MOVE "UP/UPMEN2"             TO FORM-NAM
              GO TO RUN-PROG.

           IF ( F6-KEY )
              MOVE "GB/PERUSE"             TO FORM-NAM
              GO TO RUN-PROG.

           IF ( F7-KEY )
              MOVE "LP/LPMENU"             TO FORM-NAM
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
       M01. MOVE "UP/UP1534" TO FORM-NAM; GO TO RUN-PROG.
       M02. MOVE "UP/DATE35" TO FORM-NAM; GO TO RUN-PROG.
       M03. MOVE "UP/TR1534" TO FORM-NAM; GO TO RUN-PROG.
       M04. MOVE "UP/BB1535" TO FORM-NAM; GO TO RUN-PROG.
       M05. MOVE "UP/SE1534" TO FORM-NAM; GO TO RUN-PROG.
       M06. MOVE "FX/FXGBDT" TO FORM-NAM; GO TO RUN-PROG.
       M07. GO TO GET-MENU-SELECTION.
       M08. GO TO GET-MENU-SELECTION.
       M09. GO TO GET-MENU-SELECTION.
       M10. GO TO GET-MENU-SELECTION.

      *=================================================================
      *
      *    R U N - P R O G
      *
      *=================================================================
       RUN-PROG.

           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY UPMENU-SCREEN.

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
           MOVE "UPMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
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
           MOVE "UPMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-FINISH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE FORM-PATH                  TO FORM-PATHNAME.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY UPMENU-SCREEN.
           EXIT PROGRAM.

      ******************************************************************
       FORM-RESET SECTION.
      ******************************************************************

           DISPLAY UPMENU-SCREEN         UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE UPMENU-FORM-FIELDS         TO WR-BUF.
           MOVE UPMENU-FORM-LIST           TO WR-LIST.
           PERFORM CALL-WRFORM.
           PERFORM CONAME-XFER.

       FORM-RESET-EXIT.
           EXIT.

      ******************************************************************
       SETUP-LINK-INFO SECTION.
      ******************************************************************

           MOVE UPMENU-HELP--FILE          TO HELP-LINK-FILE.
           MOVE UPMENU-HELP--DIR           TO HELP-LINK-DIR.

       SETUP-LINK-INFO-EXIT.
           EXIT.

      ******************************************************************
       VDU-CONVERT-FIELD SECTION.
      ******************************************************************

           MOVE "2A"                       TO STAT.
           MOVE VDU-FIELD-TO-CONVERT       TO VDU-ALP-FIELD.

           EVALUATE TRUE
              COPY "LIBUP/UPMENU_EVA.CPY".
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
           MOVE "UPMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-RETURN.

