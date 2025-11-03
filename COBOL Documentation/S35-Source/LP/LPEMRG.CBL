       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LPEMRG.
      ******************************************************************
      *  DIR : LP
      *  DESC: MONTH END REGISTERS MENU
      *
      *  REV.
      * KEC 050996 ADDED "LIBGB/MENUFILES";
      *            ADDED 'DEALER SERVICE/AFP REPORT' OPTION.
      * MJD 031097 ADDED EMPTY PARAGRAPHS USED IN LIBGB/MENU:
      *            BEFORE-SEL: PERFORMED BEFORE ENTERING SELECTION.
      *            AFTER-SEL : PERFORMED AFTER ENTERING SELECTION.
      * BAH 052797 CLEANED UP DESCRIPTIONS, ADDED 21. CLASS TRANSFER (EMCLRG)
      * MJD 072397 ADDED LP/EMTKRG - CONVERSION REGISTER
      * JTG 991221 *     * COPIED EMCARG.C TO EMCDRG.C
      *            ADDED DAY RANGE AND GL ACCOUNT RANGE WORLD, #220
      * BAH 020225 REMOVED "LIBGB/MENUFILES"
      * BLV 040730 ADD SEL 26, EMPOR2, DEALER PAYOFF REGISTER,
      *            RIVERSIDE PR# 2321
      * BAH 050121 ADD 27, EMD2RP, POOL ACTIVITY BY DEALER FOR MODERN PR# 2458
      *  CS 070109 ADD 28, EMCKR2, 1 LINE TOTAL CHECKS WRITTEN BY BRANCH PR#516
      * BLM 080709 ADD SEL 29, MILAGE, CASH DISBURSEMENT REGISTER WITH
      *            GL RANGE, FOR MILEAGE REIMBURSEMENT REPORTING, WORLD PR# 591
      * BLM 081294 ADD SEL 30, MISC. CHECK REGISTER BY PAYEE #,
      *            DEVELOPMENT PR# 3273
      * BAH 090619 ADDED 31. IB INTEREST COLLECT, EMINTC, SUPPORT PR# 3363
      * BAH 101020 MOVED 31. TO SECOND PAGE, LPEMR2
      * BAH 101020 ADDED F4-PAGE 2 LPEMR2
      * BAH 2023.0523 COMBINED PAGE 2 WITH 1
      * BAH 2023.0911 REMOVED 6. EMDFRG AND 8. EMDCRG - QA'D PER KEVIN/CODY
      * SMR 2024-0201 ADDED OTIS TRACE LOGGING LOGIC
      * BAH 2024.0620 ADDED 28. TXMOR2 BACK FROM QA S35Q-70
      ******************************************************************
       COPY "LIBGB/MENU1A.CPY".
       COPY "LIBLP/LPEMRG_DEF.CPY".
       COPY "LIBLP/LPEMRG_WKS.CPY".
       COPY "LIBGB/MENU1B.CPY".
       COPY "LIBLP/LPEMRG_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-START' SO THAT IT CAN GET THE EXACT PROGRAM
      *    NAME IN THE TRACE LOG.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "LPEMRG"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-START.

       DISPLAY-MENU.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH EXIT-PATHNAME.
           MOVE "LPEMRG" TO VDU-FORM-NAME.

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
              MOVE "LP/LPEMMU" TO FORM-NAM
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
                 M21 M22 M23 M24 M25 M26 M27 M28
              DEPENDING ON FORM-BUF.

           GO TO GET-MENU-SELECTION.

       M01. MOVE "LP/EMSFRG" TO FORM-NAM.
            GO TO RUN-PROG.
       M02. MOVE "LP/EMLNRG" TO FORM-NAM.
            GO TO RUN-PROG.
       M03. MOVE "LP/EMPORG" TO FORM-NAM.
            GO TO RUN-PROG.
       M04. MOVE "LP/EMCKRG" TO FORM-NAM.
            GO TO RUN-PROG.
       M05. MOVE "LP/EMCARG" TO FORM-NAM.
            GO TO RUN-PROG.
       M06. GO TO GET-MENU-SELECTION.
       M07. MOVE "LP/EMOTRG" TO FORM-NAM.
            GO TO RUN-PROG.
       M08. GO TO GET-MENU-SELECTION.
       M09. MOVE "LS/CHLIST" TO FORM-NAM.
            GO TO RUN-PROG.
       M10. MOVE "LP/EMPLRG" TO FORM-NAM.
            GO TO RUN-PROG.
       M11. MOVE "LP/EMWORG" TO FORM-NAM.
            GO TO RUN-PROG.
       M12. MOVE "LP/EMTRRG" TO FORM-NAM.
            GO TO RUN-PROG.
       M13. MOVE "LP/EMALRG" TO FORM-NAM.
            GO TO RUN-PROG.
       M14. MOVE "LP/EMTBRG" TO FORM-NAM.
            GO TO RUN-PROG.
       M15. MOVE "LP/EMZ2RG" TO FORM-NAM.
            GO TO RUN-PROG.
       M16. MOVE "LP/EMCLRG" TO FORM-NAM.
            GO TO RUN-PROG.
       M17. MOVE "LP/EMTKRG" TO FORM-NAM.
            GO TO RUN-PROG.
       M18. MOVE "LP/EMTRR2" TO FORM-NAM.
            GO TO RUN-PROG.
       M19. MOVE "LP/EMCDRG" TO FORM-NAM.
            GO TO RUN-PROG.
       M20. MOVE "LP/MILAGE" TO FORM-NAM.
            GO TO RUN-PROG.
       M21. MOVE "LP/DISBOV" TO FORM-NAM.
            GO TO RUN-PROG.
       M22. GO TO GET-MENU-SELECTION.
       M23. GO TO GET-MENU-SELECTION.
       M24. GO TO GET-MENU-SELECTION.
       M25. GO TO GET-MENU-SELECTION.
       M26. GO TO GET-MENU-SELECTION.
       M27. GO TO GET-MENU-SELECTION.
       M28. MOVE "LP/EMCKR2" TO FORM-NAM.
            GO TO RUN-PROG.

       RUN-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LPEMRG-SCREEN.

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
           MOVE "LPEMRG"      TO EXT-TCLP-TRACE-FORM-PROGX.
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
           MOVE "LPEMRG"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-FINISH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LPEMRG-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           EXIT PROGRAM.

       COPY "LIBCL/TRACE.CPY".
       COPY "LIBGB/CONAME_XFER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ***************************************
       VDU-CONVERT-FIELD SECTION.
      ***************************************
           MOVE "2A" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LPEMRG_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************
       SETUP-LINK-INFO SECTION.
      ******************************************
           MOVE LPEMRG-HELP--FILE TO HELP-LINK-FILE.
           MOVE LPEMRG-HELP--DIR TO HELP-LINK-DIR.

      ******************************************
       FORM-RESET SECTION.
      ******************************************
           DISPLAY LPEMRG-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE LPEMRG-FORM-FIELDS TO WR-BUF. 
           MOVE LPEMRG-FORM-LIST TO WR-LIST.
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
           MOVE "LPEMRG"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-RETURN.

