       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GLMAIN.
      *****************************************************
      *   DIR :  GL
      *   DESC:  GENERAL LEDGER MASTER FILE
      *                MAINTENANCE - PROG 0
      *  MG  000204 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *  BAH 120514 REMOVED SPECIAL-FG
      *  MJD 131101 CHANGED FROM STANDARD TO FLOATING WINDOW.
      *             CHANGE TO SET UP FORM-LINK-AREA AFTER OPENING
      *             THE WINDOW ELSE THE HANDLE WILL NOT GET PASSED
      *             IN THE LINKAGE.
      *****************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(55) VALUE
           "@(#)GL/GLMAIN A30 09/08/14-16:07:34 09/18/14-13:48:03 >".
       01  EXT-EOBUF-MESS        PIC X.
       01  PROG-WK.
           03  FILLER        PIC X(8)  VALUE "GL/GLMAN".
           03  PROG-NO       PIC X     VALUE "1".

       01  GLMAIN-LINK                 VALUE SPACES.
           03  FORM-NAME               PIC X(20).
           03  SAVEKEY                 PIC X(11).
           03  NEW-REC                 PIC X(01).
           03  ERRCD                   PIC X(01).
           03  FORMSEL.
               05  FILLER              PIC 9(01).
               05  FORMNUM             PIC 9(01).
           03  GLMAIN-WINDOW-HANDLE    PIC X(10).

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/LOADCOW.CPY".
       01 FORM-LINK-AREA               PIC X(2000).

       LINKAGE SECTION.
       01  FORM-PATHNAME PIC X(22).
       01  EXIT-PATHNAME PIC X(22).

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      *************************************
       MAIN-PROGRAM SECTION.
      *************************************
       MAIN-MODULE.
           MOVE "OPT" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF STAT-GOOD
              MOVE GETENV-BUF TO LOADCO-OPT.
           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE "GL/GLMAN1" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.


           DISPLAY FLOATING GRAPHICAL WINDOW
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   ERASE SCREEN
      *             WITH NO SCROLL
      *             WITH NO WRAP
                   HANDLE IS GLMAIN-WINDOW-HANDLE.
           MOVE GLMAIN-LINK TO FORM-LINK-AREA.
      *-----------------------------------------------------------------
       COPY "LIBGB/CHKSEC.CPY".
      *-----------------------------------------------------------------

       LOAD-PRE.
           MOVE PROG-WK TO FORM-NAM.
       COPY "LIBGB/LOADC2.CPY".
       LOAD-POST.
           MOVE FORM-LINK-AREA TO GLMAIN-LINK.
           IF ERRCD = "X"
              GO TO END-ROUTINE.
           IF ERRCD = "E"
              MOVE "ERROR IN PROGRAM EXECUTION" TO MESS
              PERFORM SEND-MESS
              GO TO END-ROUTINE.
           MOVE FORMNUM TO PROG-NO.
           GO TO LOAD-PRE.

      *************************************
       MISC SECTION.
      *************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/SENDMESS.CPY".
       COPY "LIBGB/ERRMSG.CPY".

      *************************************
       END-ROUTINE SECTION.
      *************************************
       LOAD-RESTART.
       EXIT-PROG.
           MOVE EXIT-PATHNAME TO FORM-PATHNAME.
       STOP-RUN.
           DESTROY GLMAIN-WINDOW-HANDLE.
           EXIT PROGRAM.
