       IDENTIFICATION DIVISION.
       PROGRAM-ID.    LPM3MU.
       AUTHOR.        KEC.
       DATE-WRITTEN.  07-20-2006.
      ******************************************************************
      *
      * IDENTIFICATION:  LP/LPM3MU.C
      *
      * DESCRIPTION   :  THIRD LOAN MAINTENANCE MENU
      *                  --- CALLED FROM F4-KEY ON LP/LPM2MU.C
      *
      *=================================================================
      * REV.
      * KEC 2006.0720 {PR#00127} REGIONAL ACCEPT. <A15> [KEVIN MULLEN]
      *               **     ** CREATED BECAUSE RAN OUT OF ROOM ON
      *               SECOND LOAN MAINTENANCE MENU (LP/LPM2MU)
      * BLM 100302 ADD SELECTION 4, ESCROW FILE MAINTENANCE,
      *            REGWAN PR# 3506
      * BLM 110126 ADD SELECTION 5, CREDIT SCORE STATISTICS FILE
      *            MAINTENANCE, WORLD PR# 726
      * BLM 110301 ADD SELECTION 6, CUSTOMER INFORMATION FILE
      *            MAINTENANCE, WORLD PR# 739
      *  CS 111014 ADD #7 ACCRUAL EARNINGS FILE REGMGM PR#299
      * KEC 130131 {PR#04196} REGENCY <A15> [LORI]
      *            ADDED #8. LOAN APPLICATION TRAILER FILE
      * BAH 180221 ADDED 10. DEBIT CARD TRAILER FILE WORLD #1306
      * KEC 130131 {PR#00000} PARADATA <A15> [LORI]
      *            REMOVED #8. LOAN APPLICATION TRAILER FILE
      * BAH 190122 REMOVED 7. AEMAIN
      * BAH 20190705 REMOVED 3. LRRMAN, 2. LRBMAN PT0151
      * BAH 20190710 REMOVED 1. LRMAIN PT0151
      * BAH 20211022 REMOVED F9 CALLING LIMAIN, DOESNT EXIST
      *
      * SMR 2024-0201 ADDED OTIS TRACE LOGGING LOGIC
      ******************************************************************
       COPY "LIBGB/MENU1A.CPY".
       COPY "LIBLP/LPM3MU_DEF.CPY".
       COPY "LIBLP/LPM3MU_WKS.CPY".
       COPY "LIBGB/MENU1B.CPY".
       COPY "LIBLP/LPM3MU_SCN.CPY".
       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    NEED TO SET EXT-TCLP-TRACE-FORM-PATHNAME BEFORE GOING INTO
      *    'TRACE-PROGRAM-START' SO THAT IT CAN GET THE EXACT PROGRAM
      *    NAME IN THE TRACE LOG.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE FORM-PATHNAME TO EXT-TCLP-TRACE-FORM-PATH.
           MOVE "LPM3MU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-START.

       DISPLAY-MENU.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH EXIT-PATHNAME.
           MOVE "LPM3MU" TO VDU-FORM-NAME.

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

           IF ( F6-KEY )
              IF ( FORM-BUF = 99 )
                 MOVE "SP/GICOPY" TO FORM-NAM
                 GO TO RUN-PROG.

           IF ( F7-KEY )
              MOVE "LP/LPM2MU" TO FORM-NAM
              GO TO EXIT-PROG.

           IF ( NOT ENTER-KEY )
              GO TO GET-MENU-SELECTION.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           GO TO M01 M02 M03 M04 M05 
              DEPENDING ON FORM-BUF.

           GO TO GET-MENU-SELECTION.

      *=================================================================
       M01.  MOVE "MN/SSMAIN" TO FORM-NAM.
             GO TO RUN-PROG.
       M02.  MOVE "MN/CIMAIN" TO FORM-NAM.
             GO TO RUN-PROG.
       M03.  MOVE "MN/DBMAIN" TO FORM-NAM.
             GO TO RUN-PROG.
       M04.  MOVE "MN/AQMAIN" TO FORM-NAM.
             GO TO RUN-PROG.
       M05.  MOVE "MN/AQSMAN" TO FORM-NAM.
             GO TO RUN-PROG.

       RUN-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LPM3MU-SCREEN.

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
           MOVE "LPM3MU"      TO EXT-TCLP-TRACE-FORM-PROGX.
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
           MOVE "LPM3MU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-FINISH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LPM3MU-SCREEN.
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
              COPY "LIBLP/LPM3MU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***********************************************
       SETUP-LINK-INFO SECTION.
      ***********************************************
           MOVE LPM3MU-HELP--FILE TO HELP-LINK-FILE.
           MOVE LPM3MU-HELP--DIR TO HELP-LINK-DIR.

      ******************************************
       FORM-RESET SECTION.
      ******************************************
           DISPLAY LPM3MU-SCREEN UPON TCLP-MAIN-WINDOW-HANDLE.
           MOVE LPM3MU-FORM-FIELDS TO WR-BUF.
           MOVE LPM3MU-FORM-LIST TO WR-LIST.
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
           MOVE "LPM3MU"      TO EXT-TCLP-TRACE-FORM-PROGX.
           PERFORM TRACE-PROGRAM-RETURN.

