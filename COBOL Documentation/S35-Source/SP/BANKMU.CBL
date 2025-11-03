       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BANKMU.
      **********************************************************************
      *
      *  DIR : SP
      *  DESC: BANK PROCESSING MENU
      *
      *  NOTE:  1/25/01 - I HAVE ADDED QUITE A BIT OF DOCUMENTATION, AT
      *         LEAST FOR THE CHECK EXTRACTIONS, SINCE WE KEEP GETTING
      *         REQUESTS FOR NEW ONES.  ONE CLARIFICATION - THERE ARE
      *         SOME CHECK EXTRACTIONS WHICH ARE FOR RECONCILIATION,
      *         THESE ARE USUALLY RUN ONCE A MONTH.  THE ONES DESIGNATED
      *         AS POSITIVE PAY USUALLY DRIVE OFF THE RC-STATUS & ARE
      *         RUN DAILY.  POSITIVE PAY IS A BANK SERVICE WHICH TAKES
      *         THE DAILY FILE OF CHECKS ISSUED & VOIDED & HITS IT
      *         AGAINST THE CHECKS WHICH HAVE CLEARED.  FLAGS ARE SENT
      *         UP IN A DAILY REPORT IF ANY CHECKS HAVE CLEARED WHICH
      *         HAVE NOT BEEN ISSUED, SO CUSTOMER CAN BE QUICKLY ALERTED
      *         IF SOMEONE IS KITING CHECKS!
      *
      * REV:
      *  MJD 031097 ADDED EMPTY PARAGRAPHS USED IN LIBGB/MENU:
      *             BEFORE-SEL: PERFORMED BEFORE ENTERING SELECTION.
      *             AFTER-SEL : PERFORMED AFTER ENTERING SELECTION.
      *  MJD 010898 ADDED FIRST UNION PROGRAMS TO MENU.
      *             ELIMINATED BANK ONE OPTIONS 2 AND 4 (MERGES)
      *  BLV 030698 ADDED NATIONS BANK POS PAY CHKRNB
      *  BLV 092198 MOVE PRE-NOTE FROM SELECTION 3 TO 2, CHANGED
      *             SELECTION 3 TO STAR BANK CHECK REC
      *  BAH 030499 ADDED 8 WACHOVIA FOR TFC
      *  GLF 072099 ADDED 9 CAROLINA FIRST FOR REGMGM
      *  GLB 091499 ADDED 10 WACHOVIA FOR B.F. MOTOR CREDIT
      *  BLV 000224 ADDED 11 TRAVELERS CHK RECON, NEW SOUTH, PR# 1082
      *  BLV 000306 ADDED 12 ASSOCIATED CHK RECON, WISC, PR# 1130
      *  GLB 000413 ADDED 14 IPS ISSUE FOR OCMO, REGWAN, PR# 1141
      *  BLV 000418 ADDED 13 CASH CONCENTRATION FOR NAT'L CITY,
      *             REGWAN, PR# 1141
      *  BAH 010215 ADDED 15 DAILY DEPOSIT LISTING FOR FINMGM FROM A48, PR# 1425
      *  BLV 020111 ADDED 16 DEALER ACH PRENOTE EXTRACT, REGACC PR# 1691
      *  BAH 040212 ADDED 17 TRAVELORS TEST FOR LENDMARK PR# 2240
      *  BLF 080214 ADDED 18 CHECK ISSUE FOR REGENCY, PR# 3148
      *  BLM 080507 CHANGE SEL 18 FROM CHKRGC TO CHKRG2, REGENCY PR# 3187
      *  BLM 080507 PUT SEL 18 BACK TO CHKRGC, ADD SEL 19 AS CHKRG2,
      *             REGENCY PR# 3187
      *  BLM 080716 ADDED #20 WELLS FARGO CHECK ISSUE FOR COSMO, PR# 3217
      *  BLM 091109 ADDED #21 WACHOVIA POSTIVE PAY, PR# 3480
      *  BLM 120323 ADDED #22 TD BANK POSTIVE PAY, WORLD PR# 793
      *  BLM 131008 ADDED #23 HEARTLAND FINANCIAL POSTIVE PAY, CITIZENS
      *             IOWA, PR# 4633
      *  BLM 140212 ADDED #24 WELLS FARGO CHECK ISSUANCE, FIXED LENGTH
      *             FORMAT, KSTREET PR# 4881
      *  BLM 151007 ADDED #25 WELLS FARGO POSITIVE PAY, FIXED LENGTH
      *             165 BYTE ARP FORMAT, REGMGM PR# 4849
      *  BLM 160321 ADDED #26 MANUFACTURERS BANK POSITIVE PAY, VAR LENGTH
      *             COMMA DELIMITED, KSTREET PR# 5023
      *  BLM 170411 REMOVE NON-WORLD SELECTIONS
      * SMR 2024-0201 ADDED OTIS TRACE LOGGING LOGIC
      ********************************************************************
       COPY "LIBGB/MENU1A.CPY".
       COPY "LIBSP/BANKMU_DEF.CPY".
       COPY "LIBSP/BANKMU_WKS.CPY".
       COPY "LIBGB/MENU1B.CPY".
       COPY "LIBSP/BANKMU_SCN.CPY".
       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-START' SO THAT IT CAN GET THE EXACT PROGRAM
      *    NAME IN THE TRACE LOG.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "BANKMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-START.

       DISPLAY-MENU.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH EXIT-PATHNAME.
           MOVE "BANKMU" TO VDU-FORM-NAME.

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

      * MUST INITIALIZE FOR MENU SELECTIONS, IF PREVIOUSLY RAN IN BATCH
      * THE EXT-ROUTE-BUF WONT BE CORRECT
           MOVE "00" TO EXT-ROUTE-BUF.

           GO TO M01 M02 M03 M04 M05
              DEPENDING ON FORM-BUF.

           GO TO GET-MENU-SELECTION.

      * CASH CONCENTRATION
       M01. MOVE "SP/ACHCRE" TO FORM-NAM.
            GO TO RUN-PROG.
       M02. MOVE "SP/PRENOT" TO FORM-NAM.
            GO TO RUN-PROG.
       M03. MOVE "SP/CHKRTD" TO FORM-NAM.
            GO TO RUN-PROG.
       M04. MOVE "SP/CHKWF4" TO FORM-NAM.
            GO TO RUN-PROG.
       M05. MOVE "SP/CHKWF5" TO FORM-NAM.
            GO TO RUN-PROG.

       RUN-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BANKMU-SCREEN.

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
           MOVE "BANKMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
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
           MOVE "BANKMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-FINISH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BANKMU-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           EXIT PROGRAM.

       COPY "LIBCL/TRACE.CPY".
       COPY "LIBGB/CONAME_XFER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      **********************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************
           MOVE "2A" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/BANKMU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **********************************************
       SETUP-LINK-INFO SECTION.
      **********************************************
           MOVE BANKMU-HELP--FILE TO HELP-LINK-FILE.
           MOVE BANKMU-HELP--DIR TO HELP-LINK-DIR.

      **********************************************
       FORM-RESET SECTION.
      **********************************************
           DISPLAY BANKMU-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE BANKMU-FORM-FIELDS TO WR-BUF.
           MOVE BANKMU-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           PERFORM CONAME-XFER.
       FORM-RESET-EXIT.
           EXIT.

      **********************************************
       MISC-ROUTINES SECTION.
      **********************************************
       COPY "LIBGB/SENDMESS.CPY".

      *=================================================================
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-RETURN' SO THAT IT CAN GET THE EXACT
      *    PROGRAM NAME IN THE TRACE LOG
      *=================================================================
       MISC-TRACE-RETURN.

           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "BANKMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-RETURN.

