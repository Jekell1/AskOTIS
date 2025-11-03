       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LPLSMU.
      ******************************************************************
      *
      *  DIR : LP
      *  DESC: LOANS LISTING MENU
      *
      *-----------------------------------------------------------------
      *  REV.
      *  KEC 102795 ADDED F6-VIEW REPORTS LOGIC.
      *  MJD 011596 ADDED PWLIST.
      *  MJD 031097 ADDED EMPTY PARAGRAPHS USED IN LIBGB/MENU:
      *             BEFORE-SEL: PERFORMED BEFORE ENTERING SELECTION.
      *             AFTER-SEL : PERFORMED AFTER ENTERING SELECTION.
      *  GLF 121207 ADDED APLIST
      *  KEC 100698 DUE TO SPACE CONSTRAINTS, THE LISTING MENU HAS
      *              BEEN SPLIT INTO TWO SEPERATE MENUS JUST LIKE
      *              MAINTENANCE MENUS.  REGENCY REVOLVING
      *  GLF 080399 ADDED CLLIST
      *  GLB 000502 ADDED CXLIST
      *  GLB 000505 ADDED DTLIST
      *  MJD 000803 ADDED TZLIST (TFC PR#25 [TFC#11])
      *  MJD 001205 ADDED GQLIST (TFC PR#60)
      *  CS  011017 ADDED FOLLOWING LISTINGS (PR#1607)[SUPPORT]
      *             TRW SPECIAL CODES [TSLIST], ACTIVITY [ATLIST]
      *             RESULT [RELIST], ACTION [ACLIST], COLLECTOR ID [CNLIST]
      *             COLLECTOR POOL [CPLIST]
      *  CS  022702 ADDED CTLIST (COUNTY TAX CODE LISTING) [WORLD #288]
      *  BAH 020626 ADDED REPO AND CHARGE OFF REASON CODES REGACC PR# 1775
      *   CS 150610 FIXED CALL TO CL/PASSWD
      *  BAH 190131 REMOVED M01 RFLIST
      *  BAH 190204 REMOVED M14 PRLIST - REVOLVING
      * SMR 2024-0201 ADDED OTIS TRACE LOGGING LOGIC
      ******************************************************************
       COPY "LIBGB/MENU1A.CPY".
       COPY "LIBLP/LPLSMU_DEF.CPY".
       COPY "LIBLP/LPLSMU_WKS.CPY".
       COPY "LIBGB/MENU1B.CPY".
       COPY "LIBLP/LPLSMU_SCN.CPY".
       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-START' SO THAT IT CAN GET THE EXACT PROGRAM
      *    NAME IN THE TRACE LOG.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "LPLSMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-START.

       DISPLAY-MENU.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH EXIT-PATHNAME.
           MOVE "LPLSMU" TO VDU-FORM-NAME.

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
           IF ( VDUSTATUS = "18" )
              PERFORM VERIFY-PASSWORD
              IF ( PASSWDW-STAT NOT = "OK" )
                 MOVE "PASSWORD REQUIRED FOR THIS OPTION!" TO MESS
                 PERFORM SEND-MESS
                 GO TO GET-MENU-SELECTION
              ELSE
                 MOVE "LP/LPL2MU" TO FORM-NAM
                 GO TO RUN-PROG.

           IF ( VDUSTATUS = "1>" )
              MOVE "SP/SPMENU" TO FORM-NAM
              GO TO EXIT-PROG.

           IF ( VDUSTATUS = "1<" )
              MOVE "GB/PERUSE" TO FORM-NAM
              GO TO RUN-PROG.

           IF ( VDUSTATUS NOT = "00" )
              GO TO GET-MENU-SELECTION.

           GO TO M01 M02 M03 M04 M05 M06 M07 M08 M09 M10
                 M11 M12 M13 M14 M15 M16 M17 M18 M19 M20
                 M21 M22 M23 M24 M25 M26 M27 M28 M29 M30
                 M31 M32
              DEPENDING ON FORM-BUF.

           GO TO GET-MENU-SELECTION.

      *M01. MOVE "LS/RFLIST" TO FORM-NAM.
       M01. GO TO GET-MENU-SELECTION.

      *-----------------------------------------------------------------
       M02. MOVE "LS/BMLIST" TO FORM-NAM.
            GO TO RUN-PROG.

      *-----------------------------------------------------------------
       M03. MOVE "LS/CDLIST" TO FORM-NAM.
            CALL "CDLIST" USING FORM-PATH EXIT-PATHNAME "CO"
                   "     COLLATERAL CODE FILE LISTING       ".
      * MUST BE 40 CHARACTERS  ^
            CANCEL "CDLIST".
            
           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.

      *-----------------------------------------------------------------
       M04. MOVE "LS/CQLIST" TO FORM-NAM.
            GO TO RUN-PROG.

      *-----------------------------------------------------------------
       M05. MOVE "LS/DLLIST" TO FORM-NAM.
            GO TO RUN-PROG.

      *-----------------------------------------------------------------
       M06. MOVE "LS/ELLIST" TO FORM-NAM.
            GO TO RUN-PROG.

      *-----------------------------------------------------------------
       M07. MOVE "LS/GILIST" TO FORM-NAM.
            GO TO RUN-PROG.

      *-----------------------------------------------------------------
       M08. MOVE "LS/GLDSLS" TO FORM-NAM.
            GO TO RUN-PROG.

      *-----------------------------------------------------------------
       M09. MOVE "LS/GCLIST" TO FORM-NAM.
            GO TO RUN-PROG.

      *-----------------------------------------------------------------
       M10. MOVE "LS/ICLIST" TO FORM-NAM.
            GO TO RUN-PROG.

      *-----------------------------------------------------------------
       M11. MOVE "LS/LMLIST" TO FORM-NAM.
            GO TO RUN-PROG.

      *-----------------------------------------------------------------
       M12. MOVE "LS/CDLIST" TO FORM-NAM.
            CALL "CDLIST" USING FORM-PATH EXIT-PATHNAME "PO"
                   "       PAY OFF CODE FILE LISTING        ".
      * MUST BE 40 CHARACTERS  ^
            CANCEL "CDLIST".

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.

      *-----------------------------------------------------------------
       M13. MOVE "LS/CDLIST" TO FORM-NAM.
            CALL "CDLIST" USING FORM-PATH EXIT-PATHNAME "PL"
                   "         POOL ID FILE LISTING           ".
      * MUST BE 40 CHARACTERS  ^
            CANCEL "CDLIST".

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.

      *-----------------------------------------------------------------
      *M14. MOVE "LS/CDLIST" TO FORM-NAM.
       M14. GO TO GET-MENU-SELECTION.

      *-----------------------------------------------------------------
       M15. MOVE "LS/CDLIST" TO FORM-NAM.
            CALL "CDLIST" USING FORM-PATH EXIT-PATHNAME "PU"
                   "       PURPOSE CODE FILE LISTING        ".
      * MUST BE 40 CHARACTERS  ^
            CANCEL "CDLIST".

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.

      *-----------------------------------------------------------------
       M16. MOVE "LS/CDLIST" TO FORM-NAM.
            CALL "CDLIST" USING FORM-PATH EXIT-PATHNAME "SR"
                   "        SOURCE CODE FILE LISTING        ".
      * MUST BE 40 CHARACTERS  ^
            CANCEL "CDLIST".

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.

      *-----------------------------------------------------------------
       M17. MOVE "LS/CLLIST" TO FORM-NAM.
            GO TO RUN-PROG.

      *-----------------------------------------------------------------
       M18. MOVE "LS/TZLIST" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M19. MOVE "LS/GQLIST" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M20. MOVE "LS/CDLIST" TO FORM-NAM.
            CALL "CDLIST" USING FORM-PATH EXIT-PATHNAME "TS"
                   "       TRW SPECIAL CODE LISTING         ".
      * MUST BE 40 CHARACTERS  ^
            CANCEL "CDLIST".

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.
      *-----------------------------------------------------------------
       M21. MOVE "LS/ATLIST" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M22. MOVE "LS/RELIST" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M23. MOVE "LS/CDLIST" TO FORM-NAM.
            CALL "CDLIST" USING FORM-PATH EXIT-PATHNAME "AC"
                   "       ACTION CODE FILE LISTING         ".
      * MUST BE 40 CHARACTERS  ^
            CANCEL "CDLIST".

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.
      *-----------------------------------------------------------------
       M24. MOVE "LS/CNLIST" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M25. MOVE "LS/CPLIST" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M26. MOVE "GL/ALLIST" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M27. MOVE "LS/CYLIST" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M28. MOVE "LS/CTLIST" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M29. MOVE "LS/CDLIST" TO FORM-NAM.
            CALL "CDLIST" USING FORM-PATH EXIT-PATHNAME "LS"
                   "       REPO REASON CODE LISTING         ".
      * MUST BE 40 CHARACTERS  ^
            CANCEL "CDLIST".

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.
      *-----------------------------------------------------------------
       M30. MOVE "LS/CDLIST" TO FORM-NAM.
            CALL "CDLIST" USING FORM-PATH EXIT-PATHNAME "CH"
                   "  CHARGE OFF REASON CODE FILE LISTING   ".
      * MUST BE 40 CHARACTERS  ^
            CANCEL "CDLIST".

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

            GO TO GET-MENU-SELECTION.
      *-----------------------------------------------------------------
       M31. MOVE "LS/CXLIST" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M32. MOVE "LS/DTLIST" TO FORM-NAM.
            GO TO RUN-PROG.

       RUN-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LPLSMU-SCREEN.

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
           MOVE "LPLSMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
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
           MOVE "LPLSMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-FINISH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LPLSMU-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           EXIT PROGRAM.

       COPY "LIBCL/TRACE.CPY".
       COPY "LIBGB/CONAME_XFER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "2A" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LPLSMU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE LPLSMU-HELP--FILE TO HELP-LINK-FILE.
           MOVE LPLSMU-HELP--DIR TO HELP-LINK-DIR.


      ******************************************
       FORM-RESET SECTION.
      ******************************************
           DISPLAY LPLSMU-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE LPLSMU-FORM-FIELDS TO WR-BUF.
           MOVE LPLSMU-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM CONAME-XFER.
       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       MISC-ROUTINES SECTION.
      *************************************************
       COPY "LIBGB/SENDMESS.CPY".

      *=================================================================
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-RETURN' SO THAT IT CAN GET THE EXACT
      *    PROGRAM NAME IN THE TRACE LOG
      *=================================================================
       MISC-TRACE-RETURN.

           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "LPLSMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-RETURN.

      *=================================================================
      *   ENTER PASSWORD FOR LPM2MU (F4) OPTION
      *=================================================================
       VERIFY-PASSWORD.
           MOVE SPACES                       TO PASSWDW-STAT.
           MOVE "LPL2MU"                     TO PASSWDW-PGM.
           MOVE ZEROES                       TO PASSWDW-SEQ.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "FOR THE 2ND LISTING MENU:"  TO PASSWDW-PROMPT2.
           MOVE "CL/PASSWD"                  TO FORM-NAM.
           CALL "PASSWD" USING FORM-PATH PASSWDW-BUF.
           CANCEL "PASSWD".

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

