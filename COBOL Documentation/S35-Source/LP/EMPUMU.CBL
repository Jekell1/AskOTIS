       IDENTIFICATION DIVISION.
       PROGRAM-ID.      EMPUMU.
      *******************************************************
      *  DIR : LP
      *  DESC: END-OF-MONTH REPORTS AND PURGES MENU
      *
      * REV:
      *  KEC 102795 ADDED F6-VIEW REPORTS LOGIC.
      *  MJD 031097 ADDED EMPTY PARAGRAPHS USED IN LIBGB/MENU:
      *             BEFORE-SEL: PERFORMED BEFORE ENTERING SELECTION.
      *             AFTER-SEL : PERFORMED AFTER ENTERING SELECTION.
      *  KEC 041697 ADDED 6. MASTER APPLICATION FILE.
      *  BAH 080897 MOVED 6. ^ TO 7. ADDED 6. LOAN MEMO ONLY PR# 449
      *  MJD 991018 ADDED #8 MEMO/PAYMENT PURGE FOR TRANSFERRED
      *             ACCOUNTS (MERC PR#219)
      *  BLV 010612 ADDED #9 BHPURG, BULLETIN BOARD HISTORY PURGE
      *  CLS 030501 ADDED #10 NFPURG [CASHCO PR#2034]
      *  CS  030509 ADDED #11 RCPURG [SUPPORT PR#2069]
      *  BLV 040105 ADDED #12 ENDMTH, GL TRANSACTION FILE PURGE
      *  BAH 040225 ADDED #13 DSPURG, DEALER STATS FILE PURGE, PR# 2255
      *  BAH 040603 ADDED #14 POPURG, LOAN FILE > 19 MONTHS, REGMGM PR# 2308
      *  BAH 040621 ADDED #15 PLPURG, P & L PURGE, WORLD PR# 1954
      *  BAH 101008 ADDED #16 UAPURG, MIDSTREAM PURGE, UNITED PR# 3665
      *  BLM 120221 ADDED #17 LNARCH, FORCED PASSWORD (LP/LNARCH,0),
      *             REGACC PR# 304
      *  KEC 190708 REMOVED #6 MAPURG, PT#0149
      *  BAH 20191030 REMOVED #10 NFPURG 
      *  BAH 20230922 ADDED #18 PDPURG, PURGE SAVED LOANS & QUOTES #1624
      * SMR 2024-0201 ADDED OTIS TRACE LOGGING LOGIC
      *******************************************************
       COPY "LIBGB/MENU1A.CPY".
       COPY "LIBLP/EMPUMU_DEF.CPY".
       COPY "LIBLP/EMPUMU_WKS.CPY".
       COPY "LIBGB/MENU1B.CPY".
       COPY "LIBLP/EMPUMU_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-START' SO THAT IT CAN GET THE EXACT PROGRAM
      *    NAME IN THE TRACE LOG.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "EMPUMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-START.

      ************************************************
      *    GET PROGRAMS FORM
      ************************************************
       DISPLAY-MENU.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH EXIT-PATHNAME.
           MOVE "EMPUMU" TO VDU-FORM-NAME.
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

           IF VDUSTATUS = "1>"
              MOVE "LP/LPEMMU" TO FORM-NAM
              GO TO EXIT-PROG.

           IF VDUSTATUS NOT = "00"
              GO TO GET-MENU-SELECTION.

      * MUST INITIALIZE FOR MENU SELECTIONS, IF PREVIOUSLY RAN IN BATCH
      * THE EXT-ROUTE-BUF WONT BE CORRECT
           MOVE "00" TO EXT-ROUTE-BUF.

           GO TO M01 M02 M03 M04 M05 M06 M07 M08
                 M09 M10 M11 M12 M13 M14 M15 M16
                 M17 M18
                 DEPENDING ON FORM-BUF.

           GO TO GET-MENU-SELECTION.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       M01. MOVE "LP/BWPURG" TO FORM-NAM.
            GO TO RUN-PROG.
       M02. MOVE "SP/CJPURG" TO FORM-NAM.
            GO TO RUN-PROG.
       M03. MOVE "LP/DTPURG" TO FORM-NAM.
            GO TO RUN-PROG.
      * WAS LNPURG - WORLD MUST USE LNARCH FOR RESTRICTIONS
       M04. GO TO GET-MENU-SELECTION.
       M05. MOVE "LP/LHPURG" TO FORM-NAM.
            GO TO RUN-PROG.
       M06. GO TO GET-MENU-SELECTION.
       M07. MOVE "LP/LMPURG" TO FORM-NAM.
            GO TO RUN-PROG.
       M08. MOVE "LP/BTPURG" TO FORM-NAM.
            GO TO RUN-PROG.
       M09. MOVE "SP/BHPURG" TO FORM-NAM.
            GO TO RUN-PROG.
       M10. GO TO GET-MENU-SELECTION.
       M11. MOVE "SP/RCPURG" TO FORM-NAM.
            GO TO RUN-PROG.
       M12. MOVE "GL/ENDMTH" TO FORM-NAM.
            GO TO RUN-PROG.
       M13. MOVE "LP/DSPURG" TO FORM-NAM.
            GO TO RUN-PROG.
       M14. GO TO GET-MENU-SELECTION.
       M15. MOVE "LP/PLPURG" TO FORM-NAM.
            GO TO RUN-PROG.
      * WAS UAPURG
       M16. GO TO GET-MENU-SELECTION.
       M17. MOVE "LNARCH"   TO PASSWDW-PGM.
            MOVE "TO RUN THIS PURGE" TO PASSWDW-PROMPT2.
            PERFORM VERIFY-PASSWORD.
            IF PASSWDW-STAT NOT = "OK"
               MOVE "PASSWORD REQUIRED FOR THIS OPTION" TO MESS
               PERFORM SEND-MESS
               GO TO GET-MENU-SELECTION
            ELSE
               MOVE "LP/LNARCH" TO FORM-NAM
               GO TO RUN-PROG.
       M18. MOVE "LP/PDPURG" TO FORM-NAM.
            GO TO RUN-PROG.

       RUN-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY EMPUMU-SCREEN.

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
           MOVE "EMPUMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
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
           MOVE "EMPUMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-FINISH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY EMPUMU-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           EXIT PROGRAM.

       COPY "LIBCL/TRACE.CPY".
       COPY "LIBGB/CONAME_XFER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ***************************************************
       VDU-CONVERT-FIELD SECTION.
      ***************************************************
           MOVE "2A" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/EMPUMU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***************************************************
       SETUP-LINK-INFO SECTION.
      ***************************************************
           MOVE EMPUMU-HELP--FILE TO HELP-LINK-FILE.
           MOVE EMPUMU-HELP--DIR TO HELP-LINK-DIR.

      ***************************************************
       FORM-RESET SECTION.
      ***************************************************
           DISPLAY EMPUMU-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE EMPUMU-FORM-FIELDS TO WR-BUF.
           MOVE EMPUMU-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM CONAME-XFER.
       FORM-RESET-EXIT.
           EXIT.

      ***************************************************
       MISC-ROUTINES SECTION.
      ***************************************************
       COPY "LIBGB/SENDMESS.CPY".

      *=================================================================
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-RETURN' SO THAT IT CAN GET THE EXACT
      *    PROGRAM NAME IN THE TRACE LOG
      *=================================================================
       MISC-TRACE-RETURN.

           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "EMPUMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-RETURN.

      *--------------------------------------------------------
      * FORCE PASSWORD FOR PURGE TO ARCHIVE
      *--------------------------------------------------------
       VERIFY-PASSWORD.
           MOVE "EX"       TO PASSWDW-STAT.
           MOVE 0          TO PASSWDW-SEQ.
           MOVE "PLEASE ENTER THE PASSWORD" TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL "PASSWD" USING FORM-PATH PASSWDW-BUF.
           CANCEL "PASSWD".

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

