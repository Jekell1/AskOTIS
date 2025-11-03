       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LPEDMU.
      **************************************************************************
      *
      *  DIR : LP
      *  DESC: LOANS END-OF-DAY MENU
      *
      * REV:
      *  MJD 031097 ADDED EMPTY PARAGRAPHS USED IN LIBGB/MENU:
      *             BEFORE-SEL: PERFORMED BEFORE ENTERING SELECTION.
      *             AFTER-SEL : PERFORMED AFTER ENTERING SELECTION.
      *  BLV 072997 CHANGED CPPURG TO NOPURG
      *   CS 10998  PER JR ADDED PASSWORD (SEQ 0) FOR
      *             #15 CUSTOMER STATEMENTS (MERCURY)
      *  JTG 011199 ADDED OPTIONAL DISPLAY OF 18. REVOL STMT RUN
      *  JTG 011199 ADDED OPTIONAL DISPLAY OF 19. REVOL STMT PRINT
      *             AND CHANGED EXITPROG TO LP/LPEDMU.C
      *  KEC 022599 ADDED 'PASSWORD' LOGIC FOR # 18 & #19 (REVOLVING)
      *  JTG 000120 ADDED ENVIORMENT TEST 'RVNWUP' TO CALL NEW VERSION REV.PLANS
      *  BLV 000627 ADDED OPTION 17, GLRCAP (GL DETAIL BY ACCT), CHANGED MENU
      *             SO SELECTIONS 18 & 19 ARE DISPLAYED OPTIONS REGARDLESS OF
      *             GP REVOLVING FLAG
      *  BLV 000825 CHANGED SELECTION 13 TO UNUSED, WAS GIVING "MISSING
      *             PROGRAM" ON DELUPD, TFC COMPLAINED
      *  BLV 020213 ADDED OPTION 20, GLDIST (GL DETAIL BY GLACCT)
      *  GLB 030424 REMOVE ENVIRON TEST OF "RVNWUP"; FORCE RVSTUP
      *   CS 090309 MOVED THE REVOLVING ITEM TO THEIR OWN MENU (RVSTMU)AND
      *             ADDED A 3RD REVOLVING ITEM, CAPITALIZATION OF SAME AS
      *             CASH INTEREST TO THE BALANCE  REGENCY PR#3215
      *  KEC 180220 PR#1305 - WORLD - A15 - KEVIN MULLEN
      *             ADDED "20. DEBIT BALANCING REPORT" EMTRR3
      *  BAH 180228 ADDED 21. DEBIT CARD NETSPEND REPORT - DBREPT, WORLD 1306
      *  BAH 20190625 REMOVED 18. REVOLVING STATEMENT MENU, PT0153
      *  BAH 20191030 REMOVED 15. CUSTOMER STATEMENT "LP/CUSTMT", KM
      *  BAH 2022.0314 REMOVED EXT-SPANISH-FORMS AND ALL MX FORMS
      *  SMR 2024-0201 ADDED OTIS TRACE LOGGING LOGIC
      *  BAH 2024.0205 REMOVED 12. NOTICE FILE PURGE NOPURG
      **************************************************************************
       COPY "LIBGB/MENU1A.CPY".
       COPY "LIBLP/LPEDMU_DEF.CPY".
       COPY "LIBLP/LPEDMU_WKS.CPY".
       COPY "LIBGB/MENU1B.CPY".
       COPY "LIBLP/LPEDMU_SCN.CPY".
       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-START' SO THAT IT CAN GET THE EXACT PROGRAM
      *    NAME IN THE TRACE LOG.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "LPEDMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-START.

       DISPLAY-MENU.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH EXIT-PATHNAME.
           MOVE "LPEDMU" TO VDU-FORM-NAME.

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
              MOVE "LP/LPMENU" TO FORM-NAM
              GO TO EXIT-PROG.

           IF VDUSTATUS = "1<"
              MOVE "GB/PERUSE" TO FORM-NAM
              GO TO RUN-PROG.

           IF VDUSTATUS NOT = "00"
              GO TO GET-MENU-SELECTION.

      * MUST INITIALIZE FOR MENU SELECTIONS, IF PREVIOUSLY RAN IN BATCH
      * THE EXT-ROUTE-BUF WONT BE CORRECT
           MOVE "00" TO EXT-ROUTE-BUF.

           GO TO M01 M02 M03 M04 M05 M06 M07 M08 M09 M10
                 M11 M12 M13 M14 M15 M16 M17 M18 M19 M20
                 M21
              DEPENDING ON FORM-BUF.

           GO TO GET-MENU-SELECTION.

       M01. MOVE "LP/LPPBAT" TO FORM-NAM.
            GO TO RUN-PROG.
       M02. MOVE "SP/EOQRPT" TO FORM-NAM.
            GO TO RUN-PROG.
       M03. MOVE "LP/EMTRRG" TO FORM-NAM.
            GO TO RUN-PROG.
       M04. MOVE "LP/EMLNRG" TO FORM-NAM.
            GO TO RUN-PROG.
       M05. MOVE "LP/EMSFRG" TO FORM-NAM.
            GO TO RUN-PROG.
       M06. MOVE "LP/EMCARG" TO FORM-NAM.
            GO TO RUN-PROG.
       M07. MOVE "LP/EMCKRG" TO FORM-NAM.
            GO TO RUN-PROG.
       M08. MOVE "LP/RECAPJ" TO FORM-NAM.
            GO TO RUN-PROG.
       M09. MOVE "LP/EMPORG" TO FORM-NAM.
            GO TO RUN-PROG.
       M10. MOVE "LP/EMALRG" TO FORM-NAM.
            GO TO RUN-PROG.
       M11. MOVE "LP/FDPURG" TO FORM-NAM.
            GO TO RUN-PROG.
       M12. GO TO GET-MENU-SELECTION.
       M13. GO TO GET-MENU-SELECTION.
       M14. GO TO GET-MENU-SELECTION.
       M15. GO TO GET-MENU-SELECTION.
       M16. GO TO GET-MENU-SELECTION.
       M17. MOVE "LP/GLRCAP" TO FORM-NAM.
            GO TO RUN-PROG.
       M18.  GO TO GET-MENU-SELECTION.

       M19. MOVE "LP/GLDIST" TO FORM-NAM.
            GO TO RUN-PROG.

       M20. MOVE "LP/EMTRR3" TO FORM-NAM.
            GO TO RUN-PROG.

       M21. MOVE "LP/DBREPT" TO FORM-NAM.
            GO TO RUN-PROG.

       RUN-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LPEDMU-SCREEN.

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
           MOVE "LPEDMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
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
           MOVE "LPEDMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-FINISH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LPEDMU-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           EXIT PROGRAM.

       COPY "LIBCL/TRACE.CPY".
       COPY "LIBGB/CONAME_XFER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      **************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************
           MOVE "2A" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LPEDMU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************
           MOVE LPEDMU-HELP--FILE TO HELP-LINK-FILE.
           MOVE LPEDMU-HELP--DIR TO HELP-LINK-DIR.


      ******************************************
       FORM-RESET SECTION.
      ******************************************
           DISPLAY LPEDMU-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE
           MOVE "END OF DAY PROCESSING" TO LPEDMU-TITLE.
           MOVE LPEDMU-FORM-FIELDS TO WR-BUF.
           MOVE LPEDMU-FORM-LIST   TO WR-LIST.
           PERFORM CALL-WRFORM.
           
           PERFORM CONAME-XFER.
       FORM-RESET-EXIT.
           EXIT.

      **************************************************
       MISC-ROUTINES SECTION.
      **************************************************
       COPY "LIBGB/SENDMESS.CPY".
      *=================================================================
      *    ENTER PASSWORD FOR #15. CUSTOMER STATEMENT EXTRACTION
      *=================================================================
       VERIFY-PASSWORD-M15.
           MOVE SPACES                       TO PASSWDW-STAT.
           MOVE "CUSTMT"                     TO PASSWDW-PGM.
           MOVE 0                            TO PASSWDW-SEQ.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "FOR CUSTOMER STATEMENTS:"   TO PASSWDW-PROMPT2.
           MOVE "CL/PASSWD"                  TO FORM-NAM.
           CALL "PASSWD" USING FORM-PATH PASSWDW-BUF.
           CANCEL "PASSWD".

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

      *=================================================================
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-RETURN' SO THAT IT CAN GET THE EXACT
      *    PROGRAM NAME IN THE TRACE LOG
      *=================================================================
       MISC-TRACE-RETURN.

           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "LPEDMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-RETURN.

