      * A30 CONVERSION COMPLETED
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      AUDIQ.
       DATE-WRITTEN.    080192.
      *******************************************************
      *  DESC:  OLD AUDITOR INQUIRY DRIVER.
      * CALLED FROM:
      *              LP/LPIQMU INQUIRY MENU
      *  REV:
      * MJD 080895 COPIED SP/CPINQ.C TO LP/AUDIQ.C
      *               CHANGED EXIT TO RETURN TO $ RATHER
      *               THEN TO MENU.
      * BAH 170320 ACTIVATE 8 DIGIT DATES, PD0006
      *******************************************************
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
       77  SCCS-IDENTIFIER PIC X(36) VALUE
           "@(#)LP/AUDIQ A30 10/30/14-15:27:43 >".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************
       01  PROG-WK.
           03  FILLER          PIC X(8)  VALUE "LP/AUDIQ".
           03  PROG-NO         PIC X     VALUE "0".
       01  INQ-PROG-WK.
           03  FILLER          PIC X(9)  VALUE "LP/AUDIQ0".
       01  CHG-PROG-WK.
           03  FILLER          PIC X(9)  VALUE "LP/LONPS4".

      *01  ROUTE-BUF           PIC XX    VALUE "00".

       COPY "LIBLP/LPEDPATH.CPY".
       COPY "LIBLP/LPLOANW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       01  EXT-EOBUF-MESS         REDEFINES MESS
                              PIC X(60).
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/LOADCOW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       01  FORM-LINK-AREA.
           03  AUDIQ-PROG-BUF      PIC X(400).
           03  SOC-BUF             PIC X(11)    VALUE SPACES.
           03  SOC-BUF-ED          REDEFINES SOC-BUF.
               05  SOC-ED1         PIC 999.
               05  FILLER          PIC X.
               05  SOC-ED2         PIC 99.
               05  FILLER          PIC X.
               05  SOC-ED3         PIC 9999.
           03  INTERRUPT-FG        PIC X.
           03  AVAILABLE-BUF       PIC X(12).
           03  ARCHIVE-FG          PIC X.
           03  GET-NEXT-FG         PIC X(02).
           03  FLEX-INQ-BUF        PIC 9(3).
           03  AUDIQ-WINDOW-HANDLE PIC X(10).

      /*************************************************
      *   C O N T R O L   M O D U L E                  *
      **************************************************
       LINKAGE SECTION.
       01  FORM-PATHNAME PIC X(22).
       01  EXIT-PATHNAME PIC X(22).

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      /*************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
      *    MOVE "AUDIQ" TO VDU-FORM-NAME.

           MOVE TRANS-DATE   TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY  TO OPEN-PATH-TRDATE.

       COPY "LIBGB/CHKSEC.CPY".
              DISPLAY FLOATING GRAPHICAL WINDOW
                      AT SCREEN LINE 01 * VDU-WIN-L-OFFSET
                      AT SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                      SIZE 80 + VDU-WIN-W-MOD
                      LINES 24 + VDU-WIN-H-MOD
                      BLANK SCREEN
      *             WITH NO SCROLL
      *             WITH NO WRAP
                      HANDLE IS AUDIQ-WINDOW-HANDLE.

           MOVE EXT-CONAME-USER-LOC TO BRANCH.
           MOVE 0 TO LOAN-KEY BORROWER-KEY.

           MOVE 0 TO PROG-NO.
       LOAD-PRE.
           MOVE PROG-WK TO FORM-NAM.
           MOVE " " TO ERRCD.

           MOVE PROG-BUF TO AUDIQ-PROG-BUF.

      * THIS USES ; CALL FORM-PROGX USING FORM-PATH FORM-LINK-AREA
      *                                  EXIT-PATHNAME
       COPY "LIBGB/LOADC2.CPY".
       LOAD-POST.

           MOVE AUDIQ-PROG-BUF TO PROG-BUF.
           IF ERRCD = "E" OR ERRCD = "X"
              GO TO EXIT-PROG.

           IF ERRCD = "C"
              MOVE CHG-PROG-WK TO PROG-WK
           ELSE
              MOVE INQ-PROG-WK TO PROG-WK.

           MOVE NEW-PROG-SUB TO PROG-NO.

           GO TO LOAD-PRE.

       COPY "LIBGB/DATER.CPY".

      ********************************************
       MISC SECTION.
      ********************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/SENDMESS.CPY".
       COPY "LIBGB/ERRMSG.CPY".
       FORM-RESET.
       SETUP-LINK-INFO.
       VDU-CONVERT-FIELD.

      ********************************************
       IO-ERROR SECTION.
      ********************************************
           MOVE "WINDOW ERROR " TO MESS.
           PERFORM SEND-MESS.
       END-ROUTINE.
       LOAD-RESTART.
       EXIT-PROG.
            MOVE "SP/SPAQMU" TO FORM-NAM.
            MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
            DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
            DESTROY AUDIQ-WINDOW-HANDLE.
            EXIT PROGRAM.
