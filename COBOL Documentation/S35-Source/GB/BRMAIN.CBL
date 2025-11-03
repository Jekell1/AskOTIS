       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BRMAIN.
      ******************************************************************
      *   DIR :  GB
      *   DESC:  BRANCH FILE MAINT - DRIVER
      *
      *   REV.
      *   KEC 071296 ADDED 'EXITPROG' LOGIC.
      *   MG  991004 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *   BAH 120514 REMOVED SPECIAL-FG FROM CONAME-BUF
      *   MJD 131101 CHANGED FROM STANDARD TO FLOATING WINDOW.
      *              CHANGE TO SET UP FORM-LINK-AREA AFTER OPENING
      *              THE WINDOW ELSE THE HANDLE WILL NOT GET PASSED
      *              IN THE LINKAGE.
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)GB/BRMAIN S34 10/29/21-13:19:36 >".
       01  PROG-WK.
           03  FILLER        PIC X(8)  VALUE "GB/BRMAN".
           03  PROG-NO       PIC X     VALUE "1".
       01  EXT-EOBUF-MESS    PIC X(60).


       01  BRMAIN-LINK       VALUE SPACES.
           03  FORM-NAME     PIC X(20).
           03  SAVEKEY       PIC X(4).
           03  NEW-REC       PIC X.
           03  ERRCD         PIC X.
           03  FORMNUM       PIC 9.
           03  BRMAIN-WINDOW-HANDLE PIC X(10).

       COPY "LIBGB/CONAME_EXT.CPY".

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/LOADCOW.CPY".
       01  FORM-LINK-AREA      PIC X(2000).

       LINKAGE SECTION.
       01  FORM-PATHNAME PIC X(22).
       01  EXIT-PATHNAME.
           03  FILLER         PIC X(12).
           03  EXIT-PATH      PIC X(9).
           03  FILLER         PIC X.

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *****************************
       MAIN-PROGRAM SECTION.
      *****************************
       MAIN-MODULE.
      D    STOP MESS.

           MOVE "OPT" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF STAT-GOOD
              MOVE GETENV-BUF TO LOADCO-OPT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "GB/BRMAN1" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.


           DISPLAY FLOATING GRAPHICAL WINDOW
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   ERASE SCREEN
                   HANDLE IS BRMAIN-WINDOW-HANDLE.

           MOVE BRMAIN-LINK TO FORM-LINK-AREA.

           PERFORM CHECK-SECURITY.

       LOAD-PRE.
           MOVE PROG-WK TO FORM-NAM.
       COPY "LIBGB/LOADC2.CPY".
       LOAD-POST.
           MOVE FORM-LINK-AREA TO BRMAIN-LINK.

           IF ERRCD = "X"
              GO TO END-ROUTINE.

           IF ERRCD = "E"
              MOVE "ERROR IN PROGRAM EXECUTION" TO MESS
              PERFORM SEND-MESS
              GO TO END-ROUTINE.

           MOVE FORMNUM TO PROG-NO.

           GO TO LOAD-PRE.

      ****************************
       MISC SECTION.
      ****************************
       COPY "LIBGB/CHKSEC.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/SENDMESS.CPY".
       COPY "LIBGB/ERRMSG.CPY".

      ****************************
       END-ROUTINE SECTION.
      ****************************
       LOAD-RESTART.
       EXIT-PROG.
           MOVE EXIT-PATH TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DESTROY BRMAIN-WINDOW-HANDLE.
           EXIT PROGRAM.
