       IDENTIFICATION DIVISION.
       PROGRAM-ID.      PGMENU.
       DATE-WRITTEN.    082989.
      ******************************************************************
      *
      *  DESC:  PROGRAMMING INTERRUPT MENU
      *
      *         F12, UP ARROW, UP ARROR, F12
      *
      *         CALLED FROM SCREEN.CPY
      *-----------------------------------------------------------------
      * REV:
      *  JTG 110797 STOPPED THIS PROGRAM FROM EXECUTION IF NOT
      *              AN 'A' LOGIN
      *  JTG 122298 ADDED SELECT OF NOTE MAINTENANCE MENU
      *  KEC 022499 MADE CHANGE TO GO TO SECONDARY MENUS OFF OF THE TWO
      *              MAINTENANCE MENU.
      *  KEC 030299 MADE CHANGE TO GO TO SECONDARY MENU
      *              (REVISED; LP/LPM2M2)
      *  MG  990929 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *  GLB 000511 ADDED SELECTION OF INSTALL MENU GB/INSTMU
      *  CLS 100323 ADDED SELECTION OF 3RD MAINT. MENU, MOVED THE OTHER
      *             ITEMS DOWN SO THE MAINT MENU'S APPEAR TOGETHER REGENCY
      *  MJD 131028 ADDED TEMP-WINDOW-OPEN/TEMP-WINDOW-CLOSE SECTIONS
      *             TO FIX PROBLEM WITH CALLING MENU'S (SEE MORE DETAILED
      *             DESCRIPTION WITH SECTION HEADER)
      *  MJD 140603 ADDED EXT-F12-OPEN TO LIMIT F12 IF ALREADY ACTIVE.
      *  BAH 140814 ADDED SET AND CANCEL OF CHKSEC-FORM-PATH FOR EVERY CALL
      *             FORM-PATH, PROGRAMS WERE NOT BEING CANCELLED.
      *  MJD 140922 MOVED LEADING SPACE IN FLDSEL-BUF AND DISPLAY-BUF.
      *             IT WAS PREVENTING SEARCH FUNCTION FROM WORKING. I.E.
      *             HITTING "E" TO JUMP TO ELAPSED.
      *  MJD 140926 ADDED CLMENU_HLD.CPY.  WORKER USED TO HOLD EXT-WI-CL-WORKER
      *             SO VALUES CAN BE RESTORED AFTER CALL
      *  MJD 150317 REMOVED IS????? COPYBOOKS.
      *  MJD 150421 CALL TO ELAPS WAS NOT PERFORMING SETUP WINDOW UPON RETURN
      * KEC 2016.0502 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         REMOVED USE OF LIBCL/CLMENUW_HLD.CPY AND NOW USING
      *         EXT-WI-CL-SIZE (NEW 78; 200 BYTES).
      * BAH 20211022 REMOVED NPMNMU, NOTE MAINTENANCE
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)CL/PGMENU.CBL S34 09/30/21-13:31:29 >".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************

       01  ROUTE-BUF               PIC XX  VALUE " ".
       01  EXT-EOBUF-MESS              REDEFINES ROUTE-BUF
                                   PIC XX.
       01 PGMENU-WINDOW-HANDLE             PIC X(10).
       01 HOLD-TCLP-MAIN-WINDOW-HANDLE     PIC X(10).
       01 TEMP-WINDOW-HANDLE               PIC X(10).
       01 SUB                      PIC 9(6)     COMP.

       01  FLDSEL-FIELD            PIC X(4)   VALUE "010.".
       01  FLDSEL-STAT             PIC XX.
       01  FLDSEL-LIST.
           03  FILLER              PIC X(56)  VALUE
           "010 020 030 040 050 060 070 080 090 100 110 120 130 140.".
       01  FLDSEL-BUF.
           05  FLDSEL-KEY          PIC XX.
           05  FLDSEL-NAME         PIC X(26).
           05  FILLER              PIC X.

       01  FIRST-FIELD             PIC X(4)  VALUE "010.".
       01  LAST-FIELD              PIC X(4)  VALUE "140.".

       01  DISPLAY-LIST.
           03  FILLER              PIC X(56)  VALUE
           "010 020 030 040 050 060 070 080 090 100 110 120 130 140".
           03  FILLER              PIC X(16)  VALUE
           "PGNO ENDMORE:SF.".
       01  DISPLAY-BUF.
           03  D-LINE              OCCURS 14 TIMES.
               05  D-KEY           PIC XX.
               05  D-NAME          PIC X(26).
               05  FILLER          PIC X.
           03  D-PAGENO            PIC ZZ9.
           03  D-END-MORE          PIC X(7).

       77  SCRN-LINES PIC 9(4).
       77  SCRN-SIZE PIC 9(4).

       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/PGMENU_DEF.CPY".
       COPY "LIBWI/PGMENU_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
      *-----------------------------------------------------------------
       COPY "LIBCL/CLMENUW_EXT.CPY".
       01  HLD-WI-CL-WORKER        PIC X(EXT-WI-CL-SIZE)   VALUE SPACES.
      *-----------------------------------------------------------------

       COPY "LIBGB/GETFMW.CPY".
       01  PGMENU-SELECTION    PIC XX.
       SCREEN SECTION.
       COPY "LIBWI/PGMENU_SCN.CPY".


       PROCEDURE DIVISION
            USING FORM-PATHNAME PGMENU-SELECTION EXIT-PATHNAME.

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.
      D    STOP MESS.
           IF EXT-F12-OPEN = "Y"
              GO TO EXIT-PROG. |GOTO EXIT-PROG SO WE DO NOT UNSET THE FLAG.
           MOVE "Y" TO EXT-F12-OPEN.

           MOVE FORM-PATHNAME TO FORM-PATH.
           IF NOT (EXT-CONAME-USER-PROFILE-CD = X"61")
              GO TO END-ROUTINE.


       COPY "LIBGB/CHKSEC.CPY".
           PERFORM SETUP-WINDOW.

      *************************************
      *    SELECT FIELD FROM PAGES
      *************************************
       SELECT-FIELD-FROM-DISPLAY.
           MOVE " F7-EXIT, SELECT OPTION(RTN):" TO MESS.
           PERFORM SEND-LEGEND.
           PERFORM CALL-FLDSEL.
      * TEST UP ARROW:
           IF FLDSEL-STAT = "1U"
              GO TO SELECT-FIELD-FROM-DISPLAY.

      * TEST PAGE UP:
           IF FLDSEL-STAT = "1P"
              GO TO SELECT-FIELD-FROM-DISPLAY.

      * TEST PAGE DOWN OR DOWN ARROW:
           IF FLDSEL-STAT = "1N" OR FLDSEL-STAT = "1D"
              GO TO SELECT-FIELD-FROM-DISPLAY.

           IF FLDSEL-STAT = "1>"
              GO TO END-ROUTINE.
           IF NOT (FLDSEL-STAT = "00")
              GO TO SELECT-FIELD-FROM-DISPLAY.

           IF FLDSEL-KEY = "AD"
              PERFORM ADDON-MAINT
           ELSE
           IF FLDSEL-KEY = "DE"
              PERFORM DEALER-MAINT
           ELSE
           IF FLDSEL-KEY = "LO"
              PERFORM LOAN-MAINT
           ELSE
           IF FLDSEL-KEY = "PA"
              IF FLDSEL-BUF = "PAYMENTS"
                 PERFORM PAYMENT-MAINT
              ELSE
                 PERFORM SCHEDULE-MAINT
           ELSE
           IF FLDSEL-KEY = "ST"
              PERFORM SPR-MAINT
           ELSE
           IF FLDSEL-KEY = "1S"
              PERFORM FIRST-MENU
           ELSE
           IF FLDSEL-KEY = "2N"
              PERFORM SECOND-MENU
           ELSE
           IF FLDSEL-KEY = "GL"
              PERFORM GLOBAL-MENU
           ELSE
           IF FLDSEL-KEY = "EL"
              PERFORM ELAPS-MENU
           ELSE
           IF FLDSEL-KEY = "IN"
              PERFORM INSTALL-MENU
           ELSE
           IF FLDSEL-KEY = "EX"
              PERFORM CALL-EXTVAR
           ELSE
           IF FLDSEL-KEY = "3R"
              PERFORM THIRD-MENU.

           GO TO SELECT-FIELD-FROM-DISPLAY.

      **************************************************
       ADDON-MAINT SECTION.
      **************************************************
           CLOSE WINDOW PGMENU-WINDOW-HANDLE.
           MOVE "MN/LTMAIN" TO FORM-NAM.
           CALL "LTMAIN" USING FORM-PATH EXIT-PATHNAME.
           CANCEL "LTMAIN".
           PERFORM SETUP-WINDOW.

      **************************************************
       LOAN-MAINT SECTION.
      **************************************************
           CLOSE WINDOW PGMENU-WINDOW-HANDLE.
           MOVE "MN/LNMAIN" TO FORM-NAM.
           CALL "LNMAIN" USING FORM-PATH EXIT-PATHNAME.
           CANCEL "LNMAIN".
           PERFORM SETUP-WINDOW.

      **************************************************
       DEALER-MAINT SECTION.
      **************************************************
           CLOSE WINDOW PGMENU-WINDOW-HANDLE.
           MOVE "MN/DLMAIN" TO FORM-NAM.
           CALL "DLMAIN" USING FORM-PATH EXIT-PATHNAME.
           CANCEL "DLMAIN".
           PERFORM SETUP-WINDOW.

      **************************************************
       PAYMENT-MAINT SECTION.
      **************************************************
           CLOSE WINDOW PGMENU-WINDOW-HANDLE.
           MOVE "MN/LPMAIN" TO FORM-NAM.
           CALL "LPMAIN" USING FORM-PATH EXIT-PATHNAME.
           CANCEL "LPMAIN".
           PERFORM SETUP-WINDOW.

      **************************************************
       SCHEDULE-MAINT SECTION.
      **************************************************
           CLOSE WINDOW PGMENU-WINDOW-HANDLE.
           MOVE "MN/PSMAIN" TO FORM-NAM.
           CALL "PSMAIN" USING FORM-PATH EXIT-PATHNAME.
           CANCEL "PSMAIN".
           PERFORM SETUP-WINDOW.

      **************************************************
       SPR-MAINT SECTION.
      **************************************************
           CLOSE WINDOW PGMENU-WINDOW-HANDLE.
           MOVE "MN/SPMAIN" TO FORM-NAM.
           CALL "SPMAIN" USING FORM-PATH EXIT-PATHNAME.
           CANCEL "SPMAIN".
           PERFORM SETUP-WINDOW.

      ******************************************************************
      *
      *    F I R S T - M E N U
      *
      ******************************************************************
       FIRST-MENU SECTION.
           CLOSE WINDOW PGMENU-WINDOW-HANDLE.
           PERFORM TEMP-WINDOW-OPEN.

       FIRST-MENU-LOOP.
           MOVE "LP/LPMNMU" TO FORM-NAM.
           CALL "LPMNMU" USING FORM-PATH EXIT-PATHNAME.
           CANCEL "LPMNMU".

      * CHECK: CODE FILE  MENU
      * COLLECTION MENU
           IF ( FORM-NAM = "LP/CDMNMU" ) OR
              ( FORM-NAM = "LP/CPMNMU" )
              PERFORM FIRST-MENU-OTHER-MENU.

      * CHECK: FIRST MENU 
           IF ( FORM-NAM = "LP/LPMNMU" )
              GO TO FIRST-MENU-LOOP.

      * CHECK: EXIT
           IF ( FORM-NAM = "LP/LPMENU" ) OR
              ( FORM-NAM = "LP/LPM2MU" ) OR
              ( FORM-NAM = "SP/SPMENU" )
              GO TO FIRST-MENU-END.

      * EXECUTE SELECTION
           MOVE FORM-PATH TO CHKSEC-FORM-PATH.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL CHKSEC-FORM-PATH.
           GO TO FIRST-MENU-LOOP.

       FIRST-MENU-OTHER-MENU.

           MOVE FORM-PATH TO CHKSEC-FORM-PATH.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL CHKSEC-FORM-PATH.

           IF ( FORM-NAM NOT = "LP/LPMENU" ) AND
              ( FORM-NAM NOT = "LP/LPMNMU" ) AND
              ( FORM-NAM NOT = "LP/LPM2MU" ) AND
              ( FORM-NAM NOT = "SP/SPMENU" )
              GO TO FIRST-MENU-OTHER-MENU.

       FIRST-MENU-END.

           PERFORM TEMP-WINDOW-CLOSE.
           PERFORM SETUP-WINDOW.

      ******************************************************************
       SECOND-MENU SECTION.
      ******************************************************************
           CLOSE WINDOW PGMENU-WINDOW-HANDLE.
           PERFORM TEMP-WINDOW-OPEN.

       SECOND-MENU-LOOP.
           MOVE "LP/LPM2MU" TO FORM-NAM.
           MOVE FORM-PATH TO CHKSEC-FORM-PATH.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL CHKSEC-FORM-PATH.

      * CHECK: SECOND MENU (REVISED)
           IF ( FORM-NAM = "LP/LPM2M2" )
              PERFORM SECOND-MENU-OTHER-MENU.

      * CHECK: SECOND MENU
           IF ( FORM-NAM = "LP/LPM2MU" )
              GO TO SECOND-MENU-LOOP.

      * CHECK: EXIT 
           IF ( FORM-NAM = "LP/LPMENU" ) OR
              ( FORM-NAM = "LP/LPMNMU" ) OR
              ( FORM-NAM = "SP/SPMENU" ) OR
              ( FORM-NAM = "LP/LPM3MU" )
              GO TO SECOND-MENU-END.

      * EXECUTE SELECTION
           MOVE FORM-PATH TO CHKSEC-FORM-PATH.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL CHKSEC-FORM-PATH.
           GO TO SECOND-MENU-LOOP.

       SECOND-MENU-OTHER-MENU.

           MOVE FORM-PATH TO CHKSEC-FORM-PATH.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL CHKSEC-FORM-PATH.

           IF ( FORM-NAM NOT = "LP/LPMENU" ) AND
              ( FORM-NAM NOT = "LP/LPMNNU" ) AND
              ( FORM-NAM NOT = "LP/LPM2MU" ) AND
              ( FORM-NAM NOT = "SP/SPMENU" )
              GO TO SECOND-MENU-OTHER-MENU.

       SECOND-MENU-END.

           PERFORM TEMP-WINDOW-CLOSE.
           PERFORM SETUP-WINDOW.

      ******************************************************************
       THIRD-MENU SECTION.
      ******************************************************************
           CLOSE WINDOW PGMENU-WINDOW-HANDLE.
           PERFORM TEMP-WINDOW-OPEN.

       THIRD-MENU-LOOP.
           CLOSE WINDOW PGMENU-WINDOW-HANDLE.
           MOVE "LP/LPM3MU" TO FORM-NAM.
           MOVE FORM-PATH TO CHKSEC-FORM-PATH.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL CHKSEC-FORM-PATH.

      * CHECK: SECOND MENU
           IF ( FORM-NAM = "LP/LPM3MU" )
              GO TO THIRD-MENU-LOOP.

      * CHECK: EXIT
           IF ( FORM-NAM = "LP/LPMENU" ) OR
              ( FORM-NAM = "LP/LPMNMU" ) OR
              ( FORM-NAM = "LP/LPM2MU")  OR
              ( FORM-NAM = "SP/SPMENU" )
              GO TO THIRD-MENU-END.

      * EXECUTE SELECTION 
           MOVE FORM-PATH TO CHKSEC-FORM-PATH.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL CHKSEC-FORM-PATH.
           GO TO THIRD-MENU-LOOP.

       THIRD-MENU-OTHER-MENU.

           MOVE FORM-PATH TO CHKSEC-FORM-PATH.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL CHKSEC-FORM-PATH.

           IF ( FORM-NAM NOT = "LP/LPMENU" ) AND
              ( FORM-NAM NOT = "LP/LPMNNU" ) AND
              ( FORM-NAM NOT = "LP/LPM3MU" ) AND
              ( FORM-NAM NOT = "SP/SPMENU" )
              GO TO THIRD-MENU-OTHER-MENU.

       THIRD-MENU-END.
           PERFORM TEMP-WINDOW-CLOSE.
           PERFORM SETUP-WINDOW.

      ***************************************************
       INSTALL-MENU SECTION.
      ***************************************************
           CLOSE WINDOW PGMENU-WINDOW-HANDLE.
           PERFORM TEMP-WINDOW-OPEN.
       INSTALL-MENU-BEGIN.

           CLOSE WINDOW PGMENU-WINDOW-HANDLE.
           MOVE "GB/INSTMU" TO FORM-NAM.
           CALL "INSTMU" USING FORM-PATH EXIT-PATHNAME.
           CANCEL "INSTMU".

           IF ( FORM-NAM = "GB/INSTMU" )
              GO TO INSTALL-MENU-BEGIN.

           IF ( FORM-NAM = " " ) OR
              ( FORM-NAM = "LP/LPMENU" ) OR
              ( FORM-NAM = "LP/LPMNMU" ) OR
              ( FORM-NAM = "CL/PGMENU" )
              GO TO INSTALL-MENU-END.

      * EXECUTE SELECTION 
           MOVE FORM-PATH TO CHKSEC-FORM-PATH.
           CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME.
           CANCEL CHKSEC-FORM-PATH.

           GO TO INSTALL-MENU-BEGIN.

       INSTALL-MENU-END.
           PERFORM TEMP-WINDOW-CLOSE.
           PERFORM SETUP-WINDOW.

      ******************************************************************
       GLOBAL-MENU SECTION.
      ******************************************************************
           CLOSE WINDOW PGMENU-WINDOW-HANDLE.
           PERFORM TEMP-WINDOW-OPEN.
       GLOBAL-MENU-LOOP.
           MOVE "GB/GBMENU" TO FORM-NAM.
           CALL "GBMENU" USING FORM-PATH EXIT-PATHNAME.
           CANCEL "GBMENU".

           IF NOT (FORM-NAM = "LP/LPMENU")
              MOVE FORM-PATH TO CHKSEC-FORM-PATH
              CALL FORM-PROGX USING FORM-PATH EXIT-PATHNAME
              CANCEL CHKSEC-FORM-PATH.

           IF FORM-NAM = "GB/GBMENU"
              GO TO GLOBAL-MENU-LOOP.

           PERFORM TEMP-WINDOW-CLOSE.
           PERFORM SETUP-WINDOW.

      ***************************************
       ELAPS-MENU SECTION.
      ***************************************
           CLOSE WINDOW PGMENU-WINDOW-HANDLE.
           MOVE "WI/ELAPS" TO FORM-NAM.
           CALL "ELAPS" USING FORM-PATH EXIT-PATHNAME.
           CANCEL "ELAPS".
           PERFORM SETUP-WINDOW.

      ***************************************
       CALL-EXTVAR SECTION.
      ***************************************
           CLOSE WINDOW PGMENU-WINDOW-HANDLE.
           PERFORM TEMP-WINDOW-OPEN.
           MOVE "WI/EXTVAR" TO FORM-NAM.
           CALL "EXTVAR" USING FORM-PATH EXIT-PATHNAME.
           CANCEL "EXTVAR".
           PERFORM TEMP-WINDOW-CLOSE.
           PERFORM SETUP-WINDOW.

      ***************************************
       SETUP-WINDOW SECTION.
      ***************************************

           MOVE "PGMENU" TO VDU-FORM-NAME.
           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT LINE 04 * VDU-WIN-L-OFFSET
                   AT COL 26 * VDU-WIN-C-OFFSET
                   SIZE 33 + VDU-WIN-W-MOD
                   LINES 18 + VDU-WIN-H-MOD
                   CELL SIZE IS LABEL FONT
                   COLOR IS VDU-WCOLOR-FWIN
                   CONTROL VALUE IS VDU-WCTRL-FWIN
                   BLANK SCREEN
                   HANDLE  IS PGMENU-WINDOW-HANDLE.

           PERFORM FORM-RESET.

       BUILD-PAGENO.
           MOVE SPACES TO DISPLAY-BUF.
           MOVE SPACES TO FLDSEL-LIST.
           STRING "010 020 030 040 050 060 070 "
                  "080 090 100 110 120 130 140."
                  DELIMITED BY SIZE INTO FLDSEL-LIST.

           MOVE "1S" TO D-KEY(1).
           MOVE "T MAINTENANCE MENU  " TO D-NAME(1).
           MOVE "2N" TO D-KEY(2).
           MOVE "D MAINTENANCE MENU  " TO D-NAME(2).
           MOVE "3R" TO D-KEY(3).
           MOVE "D MAINTENANCE MENU  " TO D-NAME(3).

           MOVE "AD" TO D-KEY(4).
           MOVE "DON INSURANCE" TO D-NAME(4).
           MOVE "DE" TO D-KEY(5).
           MOVE "ALER MASTER" TO D-NAME(5).
           MOVE "LO" TO D-KEY(6).
           MOVE "AN MASTER" TO D-NAME(6).
           MOVE "PA" TO D-KEY(7).
           MOVE "YMENTS" TO D-NAME(7).
           MOVE "PA" TO D-KEY(8).
           MOVE "YMENT SCHEDULE" TO D-NAME(8).
           MOVE "ST" TO D-KEY(9).
           MOVE "ATE PARAMETERS" TO D-NAME(9).
           MOVE "GL" TO D-KEY(10).
           MOVE "OBAL MENU     " TO D-NAME(10).
           MOVE "EL" TO D-KEY(11).
           MOVE "APSED TIME    " TO D-NAME(11).
           MOVE "NO" TO D-KEY(12).
           MOVE "T AVAILABLE" TO D-NAME(12).
           MOVE "IN" TO D-KEY(13).
           MOVE "STALL MENU" TO D-NAME(13).
           MOVE "EX" TO D-KEY(14).
           MOVE "TERNAL VARIABLES" TO D-NAME(14).

       DISPLAY-PAGE.
           MOVE "* END *" TO D-END-MORE.
           MOVE 1 TO D-PAGENO.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           DISPLAY PGMENU-SCREEN UPON PGMENU-WINDOW-HANDLE.
           MOVE PGMENU-FORM-FIELDS TO WR-BUF.
           MOVE PGMENU-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.
      
      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE PGMENU-HELP--FILE TO HELP-LINK-FILE.
           MOVE PGMENU-HELP--DIR  TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/PGMENU_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *****************************************************************
      * OPEN UP A TEMPORARY WINDOW OVER TOP OF THE MAIN TCLP WINDOW AND
      * PASS THE HANDLE FORWARD IN THE TCLP-MAIN-WINDOW HANDLE
      * AFTER BACKING UP THE TCLP-MAIN-WINDOW HANDLE.)
      *
      * IF WE CALL A MENU FROM THE F10 AND THE MENU DISPLAY ON THE
      * TCLP-MAIN-WINDOW-HANDLE THEN WE ARE DISPLAYING OVER THE EXISTING
      * SCREEN AND IT LOOKS JUMBLED.   AT THE TIME OF THE CALL I DO
      * NOT KNOW WHAT SCREEN IS DISPLAYED AND PERFORMING A DESTROY
      * ALL CONTROLS MAY GET RID OF WORK THE USER HAS NOT COMPLETED.
      *
      * TO GET AROUND THIS I AM SAVING THE TCLP-MAIN-WINDOW HANDLE IN
      * A WORKER, OPENING A NEW WINDOW OVER TOP OF THE TCLP-MAIN-WINDOW
      * HANDLE AND THEM MOVING THE NEW WINDOWS HANDLE TO TCLP-MAIN-WINDOW
      * HANDLE.   SUBSEQUENT PROGRAMS THAT DISPLAY TO TCLP-MAIN WINDOW
      * HANDLE WILL NOT AUTOMATICALLY DISPLAY TO THIS NEW TEMP WINDOW.
      * UPON RETURNING FROM THE CALLED PROGRAM, THE WINDOW MUST BE
      * CLOSED AND THE TCLP-MAIN-WINDOW HANDLE RESTORED SO THAT
      * EXISTING PROGRAMA CAN CONTINUE TO FUNCTION PROPERLY.
      *****************************************************************
       TEMP-WINDOW-OPEN SECTION.
           MOVE TCLP-MAIN-WINDOW-HANDLE TO
                HOLD-TCLP-MAIN-WINDOW-HANDLE.

      * BACK UP WORKER AS VALUES MAY CHANGE IN CALLED PROGRAM AND WE DO NOT
      * WANT IT TO AFFECT PROGRAM YOU JUST INTERRUPED OUT OF.
           MOVE EXT-WI-CL-WORKER TO HLD-WI-CL-WORKER.

      * FORCE TO 80X24.  IF NOT VEERYANT WILL FILL ENTORE SCREEN WITH WINDOW.
      *  IF CHANGING HERE CHANGE GB/TCLP, CL/SIMENU, CL/WIMENU AS WELL.
            MOVE 80  TO SCRN-SIZE.
            MOVE 24 TO SCRN-LINES.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SIZE SCRN-SIZE + VDU-WIN-W-MOD
                   LINES SCRN-LINES + VDU-WIN-H-MOD
                   CELL SIZE IS LABEL FONT
                   COLOR IS VDU-WCOLOR-TCLP
                   CONTROL VALUE IS VDU-WCTRL-TCLP
                   BLANK SCREEN
                   HANDLE IS TEMP-WINDOW-HANDLE.
           MOVE TEMP-WINDOW-HANDLE TO
                TCLP-MAIN-WINDOW-HANDLE.
  
       TEMP-WINDOW-CLOSE SECTION.
           DESTROY TEMP-WINDOW-HANDLE
           MOVE HOLD-TCLP-MAIN-WINDOW-HANDLE TO
                TCLP-MAIN-WINDOW-HANDLE.

      * RESTORE  UP WORKER AS VALUES MAY CHANGE IN CALLED PROGRAM AND WE DO NOT
      * WANT IT TO AFFECT PROGRAM YOU JUST INTERRUPED OUT OF.
           MOVE HLD-WI-CL-WORKER TO EXT-WI-CL-WORKER.

      ********************************************
       MISC SECTION.
      ********************************************
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/SENDMESS.CPY".
       SEND-LEGEND.
           MOVE "WSEL:R" TO WR-LIST.
           MOVE MESS TO WR-BUF.
           PERFORM CALL-WRFORM.

      ********************************************
       IO-ERROR SECTION.
      ********************************************
           MOVE "WINDOW ERROR " TO MESS.
           PERFORM SEND-MESS.
       END-ROUTINE.
           MOVE "N" TO EXT-F12-OPEN.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY PGMENU-SCREEN.
           DESTROY PGMENU-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
