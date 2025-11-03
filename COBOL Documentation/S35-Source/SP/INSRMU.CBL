       IDENTIFICATION DIVISION.
       PROGRAM-ID.      INSRMU.
      ******************************************************************
      *
      *  DIR : SP
      *  DESC: INSURANCE REPORTING - AM. BANKERS AND PROGRESSIVE
      *
      * REV:
      *  MJD 031097 ADDED EMPTY PARAGRAPHS USED IN LIBGB/MENU:
      *             BEFORE-SEL: PERFORMED BEFORE ENTERING SELECTION.
      *             AFTER-SEL : PERFORMED AFTER ENTERING SELECTION.
      *  MJD 010697 REMOVED SP/SICFUN.C FROM MENU
      *             ADDED VOYAGER A90 FILES: VOYAGR.C, VOYLIST.C, AND VOYDEL.C
      *             ADDED A48 LIFE OF THE SOUTH FILES: LSOUTH.C
      *             ADDED LSODEL.C AND LSOLST.C
      *  BAH 021599 ADDED SP/AMPLE FOR REGENCY #G-3 AMPLE 2000
      *  BAH 000526 ADDED SP/AMBANK (9) AND SP/AMPROP (10) FOR CFSC PR# 1168
      *  BAH 000726 ADDED SP/CHEROK (11) FOR NEW SOUTH, PR# 1248
      *  BAH 010816 ADDED SP/SEAWEL (12) FOR SECURITY FROM A60
      *  BAH 020108 ADDED SP/AMHERI (13) FOR FINMGM PR#1633
      *  BAH 020913 ADDED SP/MIPINS (14) FOR REGWAN MONTHLY INS PR# 1864
      *  BAH 030710 ADDED SP/AGFINS (15) FOR WISC  PR# 2108
      *  BAH 041103 ADDED SP/UMCEXT (16) FOR WORLD, UNITED MOTOR CLUB
      *
      *  KEC 050629 {PR#02510} REGENCY ADDED SP/AMPRRP (17) REGWAN
      *  BAH 060125 ADDED SP/COLINS (18) FOR WESTERN, COLLATERAL
      *             EXTRACTION, PR# 2691
      *  BAH 060814 ADDED SP/SICREG (19) FOR REGACC, PR# 147
      *  BAH 070416 ADDED SP/GAPEXT (20) FOR HCRED, PR# 2956
      *  BAH 070514 ADDED SP/TRKIMP (21) FOR REGACC, PR# 173
      *  BAH 080930 ADDED SP/AIMSEX (22) FOR ACE, PR# 3254
      *  BAH 090424 ADDED SP/SICRGM (23) FOR REGMGM, PR# 3343
      *  BAH 090818 ADDED SP/CCCEXT (24) FOR FINMGM, PR# 3433
      *  BAH 091006 ADDED SP/ASSURT (25) FOR REGMGM, PR# 3464
      *  BAH 110506 ADDED SP/WESTO3 (26) FOR MULLEN, PR# 3781
      *  BAH 111220 ADDED F4 PAGE 2
      * SMR 2024-0201 ADDED OTIS TRACE LOGGING LOGIC
      ******************************************************************
       COPY "LIBGB/MENU1A.CPY".
       COPY "LIBSP/INSRMU_DEF.CPY".
       COPY "LIBSP/INSRMU_WKS.CPY".
       COPY "LIBGB/MENU1B.CPY".
       COPY "LIBSP/INSRMU_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-START' SO THAT IT CAN GET THE EXACT PROGRAM
      *    NAME IN THE TRACE LOG.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "INSRMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-START.

       DISPLAY-MENU.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH EXIT-PATHNAME.
           MOVE "INSRMU" TO VDU-FORM-NAME.

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
              MOVE "SP/OPMENU" TO FORM-NAM
              GO TO EXIT-PROG.
           IF VDUSTATUS = "1<"
              MOVE "GB/PERUSE" TO FORM-NAM
              GO TO RUN-PROG.    
           IF VDUSTATUS NOT = "00"
              GO TO GET-MENU-SELECTION.

           MOVE "00" TO EXT-ROUTE-BUF.

           GO TO M01 M02 M03 M04
              DEPENDING ON FORM-BUF.
           GO TO GET-MENU-SELECTION.

      *GENERIC
       M01. MOVE "SP/LSOUTH" TO FORM-NAM.
            GO TO RUN-PROG.
       M02. GO TO GET-MENU-SELECTION.
       M03. GO TO GET-MENU-SELECTION.
       M04. MOVE "SP/UMCEXT" TO FORM-NAM.
            GO TO RUN-PROG.

       RUN-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY INSRMU-SCREEN.

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
           MOVE "INSRMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
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
           MOVE "INSRMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-FINISH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY INSRMU-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           EXIT PROGRAM.

       COPY "LIBCL/TRACE.CPY".
       COPY "LIBGB/CONAME_XFER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ******************************************************
       VDU-CONVERT-FIELD SECTION.
      ******************************************************
           MOVE "2A" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/INSRMU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************
       SETUP-LINK-INFO SECTION.
      ******************************************************
           MOVE INSRMU-HELP--FILE TO HELP-LINK-FILE.
           MOVE INSRMU-HELP--DIR TO HELP-LINK-DIR.

      ******************************************
       FORM-RESET SECTION.
      ******************************************
           DISPLAY INSRMU-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE INSRMU-FORM-FIELDS TO WR-BUF.
           MOVE INSRMU-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           PERFORM CONAME-XFER.
       FORM-RESET-EXIT.
           EXIT.

      ******************************************************
       MISC-ROUTINES SECTION.
      ******************************************************
       COPY "LIBGB/SENDMESS.CPY".

      *=================================================================
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-RETURN' SO THAT IT CAN GET THE EXACT
      *    PROGRAM NAME IN THE TRACE LOG
      *=================================================================
       MISC-TRACE-RETURN.

           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "INSRMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-RETURN.

