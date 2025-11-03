       IDENTIFICATION DIVISION.
       PROGRAM-ID.    LPMNMU.
      ******************************************************************
      *
      *  IDENTIFICATION: LP/LPMNMU
      *
      *  DESCRIPTION   : LOAN MAINTENANCE MENU
      *
      *-----------------------------------------------------------------
      *  REVS:
      *  KEC 020996 CHANGED THE CALLING OF MAINTENANCES FOR THE CD-FILE.
      *  JFF 092796 ADDED M16 - BORROWER HOUSING CODES
      *  JTG 021797 ADDED M17 - FORM CHARGES CODE
      *  MJD 031097 ADDED EMPTY PARAGRAPHS USED IN LIBGB/MENU:
      *             BEFORE-SEL: PERFORMED BEFORE ENTERING SELECTION.
      *             AFTER-SEL : PERFORMED AFTER ENTERING SELECTION.
      *  JTG 032797 CHANGED 'REFUSAL CODES' TO 'APP STATUS CODES'
      *  BLV 111797 ADDED PASSWORD ON LPM2MU
      *  MJD 012898 ADDED CDMAIN FOR TYPE "ML"
      *  MJD 021798 ADDED CDMAIN FOR TYPE "PH"
      *  KEC 092198 ADDED CDMAIN FOR TYPE "PR"
      *  JTG 120798 ADDED CDMAIN FOR TYPE "RO"
      *  JTG 120798 ADDED CDMAIN FOR TYPE "RO"
      *  GLF 021099 ADDED MESSAGE FILE MAINTENANCES
      *  KEC 022499 REVISED THE LAYOUT OF THE SCREEN & OPTIONS.
      *
      *  KEC 060816 {PR#00000} PARADATA <A15> [JAY CISZEWSKI]
      *             ADDED F9-KEY TO GO TO "MN/MNMENU".
      *  AMM 181312 REMOVED MSMAIN
      *
      * SMR 2024-0201 ADDED OTIS TRACE LOGGING LOGIC
      ******************************************************************
       COPY "LIBGB/MENU1A.CPY".
       COPY "LIBLP/LPMNMU_DEF.CPY".
       COPY "LIBLP/LPMNMU_WKS.CPY".
       COPY "LIBGB/MENU1B.CPY".
       COPY "LIBLP/LPMNMU_SCN.CPY".
       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-START' SO THAT IT CAN GET THE EXACT PROGRAM
      *    NAME IN THE TRACE LOG.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "LPMNMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-START.

       DISPLAY-MENU.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH EXIT-PATHNAME.
           MOVE "LPMNMU" TO VDU-FORM-NAME.

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
              MOVE "SP/SPMENU" TO FORM-NAM
              GO TO EXIT-PROG.

           IF VDUSTATUS = "18"
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT = "OK"
                 MOVE "PASSWORD REQUIRED FOR THIS OPTION!" TO MESS
                 PERFORM SEND-MESS
                 GO TO GET-MENU-SELECTION
              ELSE
                 MOVE "LP/LPM2MU" TO FORM-NAM
                 GO TO RUN-PROG.
           IF VDUSTATUS NOT = "00"
              GO TO GET-MENU-SELECTION.

           GO TO M01 M02 M03 M04 M05 M06 M07
              DEPENDING ON FORM-BUF.

           GO TO GET-MENU-SELECTION.

      *--------------------------------------------{ BATCH BRANCH FILE }
       M01. MOVE "SP/BPBRMN" TO FORM-NAM.
            GO TO RUN-PROG.

      *-----------------------------------{ CODE FILE MAINTENANCE MENU }
       M02. MOVE "LP/CDMNMU" TO FORM-NAM.
            GO TO RUN-PROG.

      *----------------------------------{ COLLECTION MAINTENANCE MENU }
       M03. MOVE "LP/CPMNMU" TO FORM-NAM.
            GO TO RUN-PROG.

      *-------------------------------------------{ DEALER MASTER FILE }
       M04. MOVE "MN/DLMAIN" TO FORM-NAM.
            GO TO RUN-PROG.

      *---------------------------------------------{ G/L CONTROL FILE }
       M05. MOVE "MN/GIMAIN" TO FORM-NAM.
            GO TO RUN-PROG.

      *-----------------------------------------{ G/L DESCRIPTION FILE }
       M06. MOVE "MN/GLDSMN" TO FORM-NAM.
            GO TO RUN-PROG.

      *---------------------------------------{ INSURANCE COMPANY FILE }
       M07. MOVE "MN/ICMAIN" TO FORM-NAM.
            GO TO RUN-PROG.

       RUN-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LPMNMU-SCREEN.

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
           MOVE "LPMNMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
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
           MOVE "LPMNMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-FINISH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LPMNMU-SCREEN.
           MOVE FORM-PATH TO FORM-PATHNAME.
           EXIT PROGRAM.

       COPY "LIBCL/TRACE.CPY".
       COPY "LIBGB/CONAME_XFER.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "2A" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/LPMNMU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE LPMNMU-HELP--FILE TO HELP-LINK-FILE.
           MOVE LPMNMU-HELP--DIR TO HELP-LINK-FILE.


      ******************************************
       FORM-RESET SECTION.
      ******************************************
           DISPLAY LPMNMU-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE LPMNMU-FORM-FIELDS TO WR-BUF.
           MOVE LPMNMU-FORM-LIST TO WR-LIST.
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
           MOVE "LPMNMU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-RETURN.

      *=================================================================
      *    ENTER PASSWORD FOR LPM2MU (F4) OPTION
      *=================================================================
       VERIFY-PASSWORD.
           MOVE SPACES                          TO PASSWDW-STAT.
           MOVE "LPM2MU"                        TO PASSWDW-PGM.
           MOVE ZEROES                          TO PASSWDW-SEQ.
           MOVE "PLEASE ENTER THE PASSWORD,"    TO PASSWDW-PROMPT1.
           MOVE "FOR THE 2ND MAINTENANCE MENU:" TO PASSWDW-PROMPT2.
           MOVE "CL/PASSWD"                     TO FORM-NAM.
           CALL "PASSWD" USING FORM-PATH PASSWDW-BUF.
           CANCEL "PASSWD".

           PERFORM MISC-TRACE-RETURN.               *> UPDATE TRACE LOG

