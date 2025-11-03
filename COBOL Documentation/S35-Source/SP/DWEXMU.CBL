       IDENTIFICATION DIVISION.
       PROGRAM-ID.      DWEXMU.
      ******************************************************************
      *
      *  DIR : SP
      *  DESC: DATA WAREHOUSE EXTRACTION MENU
      *        REGACC PROG REQUEST 1675
      *
      *=================================================================
      * REV:
      * 020320     ADDED REVOLVING TRANSACTION (RT) EXTRACTION, SUPERIOR #1715
      * 041204 BAH ADDED 10. LOAN MEMO FILE EXTRACTION (LM) FOR CENTRIX PR# 2416
      * 050610 BAH ADDED 11. LOAN APP FILE EXTRACTION (LA) FOR LENDMARK PR#2544
      *                  12. MASTER APP FILE EXTRACTION (LA)
      * 060726  CS ADDED 13. SPR (LIMITED FIELDS) EXTRACT FOR REGACC PR#139
      * 070108  CS ADDED 14. CHECK (LIMITED FIELDS) EXTRACT FOR WORLD PR#517
      * 090212 BAH ADDED 15. MINI VERSION PAYMENT EXTRACT FOR WORLD PR# 626
      * 091014 BLM ADDED 16. ADDED #16, COLLECTOR QUEUE EXTRACT, REGACC PR#257
      * 111020  CS ADDED 17. ACCRUAL EARNINGS EXTRACT REGACC PR#299
      * 111117 BAH ADDED 18. CODES FILE EXTRACT, REGACC PR# 301
      *
      * KEC 2017.0421 {PR#05276} REGIONAL MANAGEMENT <A15> [D SINCLAIR]
      *         19. BORROWER MEMO FILE EXTRACTION (BM)
      *
      * 170828 BAH ADDED 20. CANNED LOAN EXTRACTION, WORLD PR#1272
      * 180727 BAH ADDED 21. ACCOUNT PAYABLE MENU (DWAPMU) WORLD #1327
      * 210805 BAH ADDED 22. CASH DRAWER FILE #1509
      * 2022.1027 BAH ADDED 23. DAILY STATUS FILE RCFILE #1575

      * SMR 2024-0201 ADDED OTIS TRACE LOGGING LOGIC
      ******************************************************************
       COPY "LIBGB/MENU1A.CPY".
       COPY "LIBSP/DWEXMU_DEF.CPY".
       COPY "LIBSP/DWEXMU_WKS.CPY".
       COPY "LIBGB/MENU1B.CPY".
       COPY "LIBSP/DWEXMU_SCN.CPY".
       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-START' SO THAT IT CAN GET THE EXACT PROGRAM
      *    NAME IN THE TRACE LOG.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "DWEXMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-START.

       DISPLAY-MENU.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH EXIT-PATHNAME.
           MOVE "DWEXMU" TO VDU-FORM-NAME.

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
              MOVE "SP/EXTRMU" TO FORM-NAM
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
                 M21 M22 M23
              DEPENDING ON FORM-BUF.

           GO TO GET-MENU-SELECTION.

      *=================================================================
       M01. MOVE "SP/LNEXTR" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M02. MOVE "SP/LTEXTR" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M03. MOVE "SP/BWEXTR" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M04. MOVE "SP/TREXTR" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M05. MOVE "SP/LPEXTR" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M06. MOVE "SP/DLEXTR" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M07. MOVE "SP/CJEXTR" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M08.
           GO TO GET-MENU-SELECTION.
      *-----------------------------------------------------------------
       M09. 
           GO TO GET-MENU-SELECTION.
      *-----------------------------------------------------------------
       M10. MOVE "SP/LMEXTR" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M11. 
           GO TO GET-MENU-SELECTION.
      *-----------------------------------------------------------------
       M12.
           GO TO GET-MENU-SELECTION.
      *-----------------------------------------------------------------
       M13. MOVE "SP/SPEXTR" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M14. MOVE "SP/CKEXTR" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M15. 
            GO TO GET-MENU-SELECTION.
      *-----------------------------------------------------------------
       M16. MOVE "SP/CQEXTR" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M17. 
            GO TO GET-MENU-SELECTION.
      *-----------------------------------------------------------------
       M18. MOVE "SP/CDEXTR" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M19. MOVE "SP/BMEXTR" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M20. MOVE "SP/CLEXTR" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M21. 
           GO TO GET-MENU-SELECTION.
      *-----------------------------------------------------------------
       M22. MOVE "SP/CAEXTR" TO FORM-NAM.
            GO TO RUN-PROG.
      *-----------------------------------------------------------------
       M23. MOVE "SP/RCEXTR" TO FORM-NAM.
            GO TO RUN-PROG.

       RUN-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY DWEXMU-SCREEN.

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
           MOVE "DWEXMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
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
           MOVE "DWEXMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-FINISH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY DWEXMU-SCREEN.
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
              COPY "LIBSP/DWEXMU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***********************************************
       SETUP-LINK-INFO SECTION.
      ***********************************************
           MOVE DWEXMU-HELP--FILE TO HELP-LINK-FILE.
           MOVE DWEXMU-HELP--DIR TO HELP-LINK-DIR.

      ***********************************************
       FORM-RESET SECTION.
      ***********************************************
           DISPLAY DWEXMU-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE DWEXMU-FORM-FIELDS TO WR-BUF.
           MOVE DWEXMU-FORM-LIST TO WR-LIST.
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
           MOVE "DWEXMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-RETURN.

