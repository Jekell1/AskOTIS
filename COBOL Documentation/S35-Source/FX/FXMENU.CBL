       IDENTIFICATION DIVISION.
       PROGRAM-ID.      FXMENU.
      ***********************************************************************
      *                     FIXIT MENU
      *
      * REV:
      *  150529 BAH ADDED 22. SET CONFIDENT FOR MODERN PR# 4737
      *  2020.0714 ADDED 24. SET LPO FROM FILE FX/SETLPO #1450
      *  2023.0405 ADDED 26. SET LOAN CREDIT LIMIT FROM FILE CLIMIT #1601
      *  2023.0728 ADDED 27. SET BRANCH CLOSED FLAG #1618
      * SMR 2024-0201 ADDED OTIS TRACE LOGGING LOGIC
      ***********************************************************************
       COPY "LIBGB/MENU1A.CPY".
       COPY "LIBFX/FXMENU_DEF.CPY".
       COPY "LIBFX/FXMENU_WKS.CPY".
       COPY "LIBGB/MENU1B.CPY".
       COPY "LIBFX/FXMENU_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-START' SO THAT IT CAN GET THE EXACT PROGRAM
      *    NAME IN THE TRACE LOG.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "FXMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-START.

       DISPLAY-MENU.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH EXIT-PATHNAME.
           MOVE "FXMENU" TO VDU-FORM-NAME.

           PERFORM FORM-RESET.

           ACCEPT GP-REC FROM ENVIRONMENT "GPENV".

      * SECURITY CHECK
           MOVE FORM-PATH TO CHKSEC-FORM-PATH.
           CALL "CHKSEC" USING CHKSEC-FIELDS.
           CANCEL "CHKSEC".

           PERFORM MISC-TRACE-RETURN.     

           IF CHKSEC-STAT NOT = "00"
              MOVE "1>" TO STAT VDUSTATUS
              GO TO MENU-EXIT.

       GET-MENU-SELECTION-CLR.
           MOVE 0 TO VDU-PROGRAMMING.

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
              MOVE "LP/LPMENU" TO FORM-NAM
              GO TO EXIT-PROG.
           IF VDUSTATUS = "18"
              MOVE "FX/FXMEN2" TO FORM-NAM
              GO TO EXIT-PROG.

           IF VDUSTATUS NOT = "00"
              GO TO GET-MENU-SELECTION.

           MOVE "00" TO EXT-ROUTE-BUF.

           GO TO M01 M02 M03 M04 M05 M06 M07 M08 M09 M10
                 M11 M12 M13 M14 M15 M16 M17 M18 M19 M20 
                 M21 M22 M23 M24 M25 M26 M27
              DEPENDING ON FORM-BUF.

           GO TO GET-MENU-SELECTION.

       M01. MOVE "FX/FXBYDS" TO FORM-NAM.
            GO TO RUN-PROG.
       M02. MOVE "FX/FXLIAB" TO FORM-NAM.
            GO TO RUN-PROG.
       M03. MOVE "FX/MAKEIX" TO FORM-NAM.
            GO TO RUN-PROG.
       M04. MOVE "FX/MAKERX" TO FORM-NAM.
            GO TO RUN-PROG.
       M05. MOVE "FX/FXBYTY" TO FORM-NAM.
            GO TO RUN-PROG.
       M06. MOVE "FX/FXDLBI" TO FORM-NAM
            GO TO RUN-PROG.
       M07. MOVE "FX/BIBUID" TO FORM-NAM.
            GO TO RUN-PROG.
       M08. MOVE "FX/FXLIMT" TO FORM-NAM.
            GO TO RUN-PROG.
       M09. MOVE "FX/BALCHK" TO FORM-NAM.
            GO TO RUN-PROG.
       M10. MOVE "FX/CKTRLP" TO FORM-NAM.
            GO TO RUN-PROG.
       M11. MOVE "SP/PURGME" TO FORM-NAM.
            GO TO RUN-PROG.
       M12. MOVE "FX/FXMIDA" TO FORM-NAM.
            GO TO RUN-PROG.
       M13. MOVE "FX/INSCOM" TO FORM-NAM.
            GO TO RUN-PROG.
       M14. MOVE "FX/ORIGIN" TO FORM-NAM.
            GO TO RUN-PROG.
       M15. GO TO GET-MENU-SELECTION.
       M16. GO TO GET-MENU-SELECTION.
       M17. MOVE "FX/MPOOL1" TO FORM-NAM.
            GO TO RUN-PROG.
       M18. GO TO GET-MENU-SELECTION.
       M19. MOVE "FX/TRWCON" TO FORM-NAM.
            GO TO RUN-PROG.
       M20. MOVE "FX/TRWCHG" TO FORM-NAM.
            GO TO RUN-PROG.
       M21. MOVE "FX/K2NAME" TO FORM-NAM.
            GO TO RUN-PROG.
       M22. MOVE "FX/SPRTRW" TO FORM-NAM.
            GO TO RUN-PROG.
       M23. MOVE "FX/WR1408" TO FORM-NAM.
            GO TO RUN-PROG.
       M24. MOVE "FX/SETLPO" TO FORM-NAM.
            GO TO RUN-PROG.
       M25. MOVE "SP/SETIMP" TO FORM-NAM.
            GO TO RUN-PROG.
       M26. MOVE "FX/CLIMIT" TO FORM-NAM.
            GO TO RUN-PROG.
       M27. MOVE "FX/BR1618" TO FORM-NAM.
            GO TO RUN-PROG.

       RUN-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY FXMENU-SCREEN.

           MOVE FORM-PATH TO FORM-PATHNAME.
           CALL FORM-PROGX USING FORM-PATHNAME EXIT-PATHNAME.
           CANCEL FORM-PROGX.

           PERFORM MISC-TRACE-RETURN.   

           IF RETURN-CODE = 1
              GO TO EXIT-PROG.


      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-RETURN' SO THAT IT CAN GET THE EXACT
      *    PROGRAM NAME IN THE TRACE LOG
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "FXMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
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
           MOVE "FXMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-FINISH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY FXMENU-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           EXIT PROGRAM.

       COPY "LIBCL/TRACE.CPY".
       COPY "LIBGB/CONAME_XFER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ******************************************
       VDU-CONVERT-FIELD SECTION.
      ******************************************
           MOVE "2A" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBFX/FXMENU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************
       SETUP-LINK-INFO SECTION.
      ******************************************
           MOVE FXMENU-HELP--FILE TO HELP-LINK-FILE.

      ******************************************
       FORM-RESET SECTION.
      ******************************************
           DISPLAY FXMENU-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE FXMENU-FORM-FIELDS TO WR-BUF.
           MOVE FXMENU-FORM-LIST TO WR-LIST.
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
           MOVE "FXMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-RETURN.

