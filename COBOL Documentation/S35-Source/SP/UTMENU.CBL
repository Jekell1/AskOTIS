       IDENTIFICATION DIVISION.
       PROGRAM-ID.      UTMENU.
      ******************************************************************
      *
      *  DIR : SP
      *  DESC: UTILITIES MENU
      *
      * REV:
      *  MJD 031097 ADDED EMPTY PARAGRAPHS USED IN LIBGB/MENU:
      *             BEFORE-SEL: PERFORMED BEFORE ENTERING SELECTION.
      *             AFTER-SEL : PERFORMED AFTER ENTERING SELECTION.
      *  MJD 082197 ADDED SP/BRXCPY TO MENU
      *  MJD 022698 ADDED KILUSR TO MENU
      *  MJD 022698 ADDED FEECODE DELETE UTILITY TO MENU
      *  MJD 040598 ADDED BYFILE COPY UTILITY TO MENU
      *             ADDED TERMEX DELETE UTILITY TO MENU
      *  MJD 071598 ADDED SP/ULINQ0.C (WI/ULINQ.C)
      *  KEC 113099 ADDED SP/LIBAT (LOAN APPLICATION INTERFACE)
      *  BLV 000626 CHANGE SELECTION 6 FROM GROUP BATCH DRIVER TO
      *             BATCH RESEQUENCER (GROUP DRIVER NOW ON EOMENU)
      *  KEC 000719 {PR#00000} MERCURY FINANCE [CINDY]
      *             ADDED #15.  LOAN APPLICATION STATUS SETTER
      *  MJD 001024 ADDED OPTION #16 ACCOUNTS PAYABLE FILE COPY: R1 > R2
      *  BLV 010810 REMOVE SELECTION 14, LITBAT, AND MOVEED SELECTIONS
      *             15 & 16 BACK TO 14 & 15, MERC PR# 253
      *  CLS 021008 ADD OPTION #16 CUSTOM GARNISHMENT MEMO [CUSTOM FOR
      *             MODERN FINANCE PR# 1916]
      *  CS  021029 ADDED OPTION #17 CUSTOM ALPHABETICAL BY GIVEN COLLECTOR ID
      *             [MODERN FINANCE PR#1943]
      *  BAH 040401 ADDED SP/SETCOL BACK FROM A10 FOR AUTOMOTIVE CREDIT
      *  BAH 040727 ADDED 19. SP/REDEEM REPO REDEEM DAYS UTILITY REGACC #69
      *  CS  040802 ADDED 20. SP/MODME2 MODERN GARN NENI [MODERN PR#2340]
      *  BLV 040810 ADDED 21. LP/REGPAY, WORLD REG PAY CHANGE FOR
      *             SOLDIERS & SAILORS RELIEF ACT
      *  BLM 090522 CHANGED #11 FROM TERM COMMUNICATIONS TO FEE CODE
      *             CLEANUP UTILITY, SP/DELFEE, WORLD PR# 636
      *  BLM 110214 ADDED 22. GB/SSCOPY, COPY SSFILE BETWEEN MACHINES,
      *             WORLD PR# 726
      *  BLM 161130 ADDED 23. SP/GIACCT, CHANGE GI ACCT# BY LINE#, 
      *             WORLD PR# 1204
      *  BLM 161206 REMOVED 23. SP/GIACCT, CHANGE GI ACCT# BY LINE#, 
      *             WORLD PR# 1204
      * MJD 170929 CHECKED FOR 8 DIGIT DATES AND DID DID NOT SEE ANYTHING, SKIP.
      * BAH 20211022 REMOVED M02 ACTIVY, M08 BRXCPY
      * BAH 20220802 REMOVED M13 ULINQ, REMOVING ULFILE, AFRAID OF CONTENTION
      * BAH 20220802 REMOVED M01 VIDEO, REMOVING ULFILE, AFRAID OF CONTENTION
      * SMR 2024-0201 ADDED OTIS TRACE LOGGING LOGIC
      ******************************************************************
       COPY "LIBGB/MENU1A.CPY".
       COPY "LIBSP/UTMENU_DEF.CPY".
       COPY "LIBSP/UTMENU_WKS.CPY".
       COPY "LIBGB/MENU1B.CPY".
       COPY "LIBSP/UTMENU_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-START' SO THAT IT CAN GET THE EXACT PROGRAM
      *    NAME IN THE TRACE LOG.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "UTMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-START.

       DISPLAY-MENU.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH EXIT-PATHNAME.
           MOVE "UTMENU" TO VDU-FORM-NAME.

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

           IF ( VDUSTATUS = "1>" )
              MOVE "SP/SPMENU" TO FORM-NAM
              GO TO EXIT-PROG.

           IF ( VDUSTATUS NOT = "00" )
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
       M01.
      *     MOVE "SP/VIDEO" TO FORM-NAM.
      *     CALL "VIDEO".
      *     CANCEL "VIDEO".
            GO TO GET-MENU-SELECTION.
       M02. GO TO GET-MENU-SELECTION.
       M03. MOVE "SP/RCINQ0" TO FORM-NAM.
            GO TO RUN-PROG.
       M04. MOVE "MN/RCMAIN" TO FORM-NAM.
            GO TO RUN-PROG.
       M05. MOVE "SP/RCLIST" TO FORM-NAM.
            GO TO RUN-PROG.
       M06. MOVE "SP/EOSEQ" TO FORM-NAM.
            GO TO RUN-PROG.
       M07. MOVE "SP/EODEL" TO FORM-NAM.
            GO TO RUN-PROG.
       M08. GO TO GET-MENU-SELECTION.
       M09. MOVE "GB/KILUSR" TO FORM-NAM.
            GO TO RUN-PROG.
       M10. MOVE "SP/FEEDEL" TO FORM-NAM.
            GO TO RUN-PROG.
       M11. MOVE "DELFEE"   TO PASSWDW-PGM.
            MOVE "TO RUN FEE CODE UTILITY" TO PASSWDW-PROMPT2.
            PERFORM VERIFY-PASSWORD.
            IF PASSWDW-STAT NOT = "OK"
               MOVE "PASSWORD REQUIRED FOR THIS OPTION" TO MESS
               PERFORM SEND-MESS
               GO TO GET-MENU-SELECTION
            ELSE
               MOVE "SP/DELFEE" TO FORM-NAM
               GO TO RUN-PROG.
      * M12 WAS GB/BYXCPY 10/26/2018 BAH
       M12.  GO TO GET-MENU-SELECTION.
      * M13 MOVE "SP/ULINQ0" 
       M13.  GO TO GET-MENU-SELECTION.
       M14.  GO TO GET-MENU-SELECTION.
       M15.  GO TO GET-MENU-SELECTION.
       M16.  GO TO GET-MENU-SELECTION.
       M17.  GO TO GET-MENU-SELECTION.
       M18. MOVE "SP/SETCOL" TO FORM-NAM.
            GO TO RUN-PROG.
       M19.  GO TO GET-MENU-SELECTION.
       M20.  GO TO GET-MENU-SELECTION.
       M21. MOVE "LP/REGPAY" TO FORM-NAM.
            GO TO RUN-PROG.
       M22.  GO TO GET-MENU-SELECTION.
       M23. MOVE "SP/CLOSER" TO FORM-NAM.
            GO TO RUN-PROG.

      *--------------------------------------------------------
      * FORCE PASSWORD FOR FEE CODE DELETE UTILITY
      *--------------------------------------------------------
       VERIFY-PASSWORD.
           MOVE "EX"       TO PASSWDW-STAT.
           MOVE 0          TO PASSWDW-SEQ.
           MOVE "PLEASE ENTER THE PASSWORD" TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL "PASSWD" USING FORM-PATH PASSWDW-BUF.
           CANCEL "PASSWD".

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

       RUN-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY UTMENU-SCREEN.

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
           MOVE "UTMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
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
           MOVE "UTMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-FINISH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY UTMENU-SCREEN.
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
              COPY "LIBSP/UTMENU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************
       SETUP-LINK-INFO SECTION.
      ******************************************
           MOVE UTMENU-HELP--FILE TO HELP-LINK-FILE.
           MOVE UTMENU-HELP--DIR TO HELP-LINK-DIR.


      ******************************************
       FORM-RESET SECTION.
      ******************************************
           DISPLAY UTMENU-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE UTMENU-FORM-FIELDS TO WR-BUF.
           MOVE UTMENU-FORM-LIST TO WR-LIST.
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
           MOVE "UTMENU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-RETURN.

