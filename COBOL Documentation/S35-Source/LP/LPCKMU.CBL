       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LPCKMU.
      ******************************************************************
      *  DIR : LP
      *  DESC: LOANS CHECK RECONCILIATION MENU
      *
      *  REV.
      *  KEC 102795 ADDED F6-VIEW REPORTS LOGIC.
      *  MJD 031097 ADDED EMPTY PARAGRAPHS USED IN LIBGB/MENU:
      *             BEFORE-SEL: PERFORMED BEFORE ENTERING SELECTION.
      *             AFTER-SEL : PERFORMED AFTER ENTERING SELECTION.
      *  BAH 020415 ADDED M99 FX/SETCLR, REGMGM PR# 1726
      *  BAH 020415 MOVED CHECK DELETE (CKDEL) TO 3. AND ADDED
      *             2. CHECK CLEAR, REGMGM PR# 1726
      *  BAH 031029 CHANGED 10. SP/CARREC REGMGM, TO 10. SP/CFIRST
      *             FOR WORLD, CAROLINA FIRST CLEARED CHECKS
      *  GLB 031113 ADD OPTION 4. A/P CHECK CLEAR
      *  BLF 070227 ADD OPTION 11. IMPORT & UPDATE EXCEL SPREAD SHEET
      *             FOR WORLD MEXICAN CHECK REC, WORLD PR# 527
      *  BLM 150316 ADDED OPTION 12, WELLS FARGO REVERSE POSITIVE PAY,
      *             WORLD PR# 1055
      *  BAH 180302 ADDED 5. DEBIT CARD PROCEEDS, DEBCLR, WORLD #1306
      *                  13. DEBIT CARD NETSPEND RECONCILE, DEBIMP #1306
      *  BAH 2022.0314 REMOVED #13, DEBIT CARD NETSPEND RECONCILE, THEY
      *                STOPPED RUNNING IN 2021.
      *  BAH 2023.0427 REMOVED 10. CFIRST, WORLD DOESNT USE
      *  SMR 2024.0201 ADDED OTIS TRACE LOGGING LOGIC
      *  BAH 2024.0318 ADDED 4. VOIDED CLEARED CHECKS #1650
      ******************************************************************
       COPY "LIBGB/MENU1A.CPY".
       COPY "LIBLP/LPCKMU_DEF.CPY".
       COPY "LIBLP/LPCKMU_WKS.CPY".
       COPY "LIBGB/MENU1B.CPY".
       COPY "LIBLP/LPCKMU_SCN.CPY".
       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-START' SO THAT IT CAN GET THE EXACT PROGRAM
      *    NAME IN THE TRACE LOG.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "LPCKMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-START.

       DISPLAY-MENU.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH EXIT-PATHNAME.
           MOVE "LPCKMU" TO VDU-FORM-NAME.

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

            IF GP-CHECK-RECONCILIATION NOT = "Y"
               MOVE "OPTION NOT PURCHASED" TO MESS
               PERFORM SEND-MESS
               MOVE "LP/LPEMMU" TO FORM-NAM
               GO TO EXIT-PROG.

           MOVE "E  N020SEL." TO VDU.
           PERFORM SCREENIO.
           MOVE VDUNUM0 TO FORM-BUF.

           IF VDUSTATUS = "10"
              MOVE 1 TO RETURN-CODE
              GO TO EXIT-PROG.

       MENU-EXIT.
           IF VDUSTATUS = "1<"
              MOVE "GB/PERUSE" TO FORM-NAM
              GO TO RUN-PROG.

           IF VDUSTATUS = "1>"
              MOVE "LP/LPEMMU" TO FORM-NAM
              GO TO EXIT-PROG.

           IF VDUSTATUS NOT = "00"
              GO TO GET-MENU-SELECTION.


      * MUST INITIALIZE FOR MENU SELECTIONS, IF PREVIOUSLY RAN IN BATCH
      * THE EXT-ROUTE-BUF WONT BE CORRECT
           MOVE "00" TO EXT-ROUTE-BUF.

           GO TO M01 M02 M03 M04 M05 M06 M07 M08 M09
                 M10 M11 M12
              DEPENDING ON FORM-BUF.

           GO TO GET-MENU-SELECTION.

       M01. MOVE "LP/OUTCHK" TO FORM-NAM.
            GO TO RUN-PROG.
       M02. MOVE "LP/CLRCHK" TO FORM-NAM.
            GO TO RUN-PROG.
       M03. MOVE "LP/DELCHK" TO FORM-NAM.
            GO TO RUN-PROG.
       M04. MOVE "LP/CKVOID" TO FORM-NAM.
            GO TO RUN-PROG.
       M05. GO TO GET-MENU-SELECTION.
       M06. GO TO GET-MENU-SELECTION.
       M07. GO TO GET-MENU-SELECTION.
       M08. GO TO GET-MENU-SELECTION.
       M09. GO TO GET-MENU-SELECTION.

      * BANK IMPORTS
       M10. GO TO GET-MENU-SELECTION.
       M11. GO TO GET-MENU-SELECTION.

       M12. MOVE "SP/WFCKUP" TO FORM-NAM.
            GO TO RUN-PROG.

       RUN-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LPCKMU-SCREEN.

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
           MOVE "LPCKMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
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
           MOVE "LPCKMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-FINISH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LPCKMU-SCREEN.
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
              COPY "LIBLP/LPCKMU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE LPCKMU-HELP--FILE TO HELP-LINK-FILE.
           MOVE LPCKMU-HELP--DIR TO HELP-LINK-DIR.


      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY LPCKMU-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE LPCKMU-FORM-FIELDS TO WR-BUF.
           MOVE LPCKMU-FORM-LIST TO WR-LIST.
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
           MOVE "LPCKMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-RETURN.

