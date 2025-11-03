       IDENTIFICATION DIVISION.
       PROGRAM-ID.      DLMAIN.
      *****************************************************
      *   DIR :  MN
      *   DESC:  DEALER MASTER FILE MAINTENANCE - PROG 0
      * REV:
      * MJD 131101 CHANGED FROM STANDARD TO FLOATING WINDOW.
      *            CHANGE TO SET UP FORM-LINK-AREA AFTER OPENING
      *            THE WINDOW ELSE THE HANDLE WILL NOT GET PASSED
      *            IN THE LINKAGE.
      *****************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(55) VALUE
           "@(#)MN/DLMAIN A30 11/01/13-09:11:33 09/18/14-13:53:04 >".
       01  EXT-EOBUF-MESS        PIC X(60).
       01  PROG-WK.
           03  FILLER        PIC X(8)  VALUE "MN/DLMAN".
           03  PROG-NO       PIC X     VALUE "1".


       01  DLMAIN-LINK        VALUE SPACES.
           03  FORM-NAME     PIC X(20).
           03  SAVEKEY       PIC X(6).
           03  NEW-REC       PIC X.
           03  ERRCD         PIC X.
           03  FORMSEL.
               05  FILLER    PIC 9.
               05  FORMNUM   PIC 9.
           03  DLMAIN-WINDOW-HANDLE PIC X(10).

       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/LOADCOW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".

       01  FORM-LINK-AREA        PIC X(2000).

       LINKAGE SECTION.
       01  FORM-PATHNAME PIC X(22).
       01  EXIT-PATHNAME PIC X(22).

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      *********************************************
       MAIN-PROGRAM SECTION.
      *********************************************
       MAIN-MODULE.
           MOVE "OPT" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF STAT = "00"
              MOVE GETENV-BUF TO LOADCO-OPT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "MN/DLMAN1" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       COPY "LIBGB/CHKSEC.CPY".

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   ERASE SCREEN
                   HANDLE IS DLMAIN-WINDOW-HANDLE.

           MOVE DLMAIN-LINK TO FORM-LINK-AREA.

       LOAD-PRE.
           MOVE PROG-WK TO FORM-NAM.

       COPY "LIBGB/LOADC2.CPY".

       LOAD-POST.
      D    STOP MESS.
           MOVE FORM-LINK-AREA TO DLMAIN-LINK.
           IF ERRCD = "X"
              GO TO END-ROUTINE.
           IF ERRCD = "E"
              MOVE "ERROR IN PROGRAM EXECUTION" TO MESS
              PERFORM SEND-MESS
              GO TO END-ROUTINE.
           MOVE FORMNUM TO PROG-NO.
           GO TO LOAD-PRE.

      *********************************************
       MISC SECTION.
      *********************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/SENDMESS.CPY".
       COPY "LIBGB/ERRMSG.CPY".

      *********************************************
       END-ROUTINE SECTION.
      *********************************************
       LOAD-RESTART.
       EXIT-PROG.
           MOVE "LP/LPMNMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DESTROY DLMAIN-WINDOW-HANDLE.
           EXIT PROGRAM.
