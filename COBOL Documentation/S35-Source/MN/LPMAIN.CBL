       IDENTIFICATION  DIVISION.
       PROGRAM-ID.       LPMAIN.
       DATE-WRITTEN. 2000/11/15.
      *****************************************************
      *   DIR :  MN
      *   DESC:  LOAN PAYMENT FILE MAINT - PROG 0
      *
      *  REV:
      *  MJD 001115 NEW PROGRAM - NEEDED TO BREAK LPMAIN INTO
      *             MULTIPLE PAGES.
      *             (OLD LPMAIN SAVED AS LP/SV/LPMAIN.001115)
      *   CS 121023 REMOVED DISPLAY STANDARD GRAPHICAL WINDOW SETUP
      *   CS 121219 ADDED BACK DISPLAY OF STANDARD GRAPHICAL WINDOW &
      *             INCLUDED HANDLE
      *   CS 130314 ADDED LIBGB/ERRMSG
      * MJD 131104 CHANGED FROM STANDARD TO FLOATING WINDOW.
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
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./MN/LPMAIN.CBL A30 04/26/17-11:55:07 >".
       01  EXT-EOBUF-MESS        PIC X(60).
       01  PROG-WK.
           03  FILLER        PIC X(8)  VALUE "MN/LPMAN".
           03  PROG-NO       PIC X     VALUE "1".


       01  LPMAIN-LINK        VALUE SPACES.
           03  FORM-NAME     PIC X(20).
           03  SAVEKEY       PIC X(17).
           03  NEW-REC       PIC X.
           03  ERRCD         PIC X.
           03  FORMSEL.
               05  FILLER    PIC 9.
               05  FORMNUM   PIC 9.
           03  LPMAIN-WINDOW-HANDLE PIC X(10).

       COPY "LIBGB/CONAME_EXT.CPY".

       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/LOADCOW.CPY".

       01  FORM-LINK-AREA    PIC X(2000).

       LINKAGE SECTION.
       01  FORM-PATHNAME PIC X(22).
       01  EXIT-PATHNAME PIC X(22).

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      *****************************************
       MAIN-PROGRAM SECTION.
      *****************************************

      D    STOP MESS.

           MOVE "OPT" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF STAT = "00"
              MOVE GETENV-BUF TO LOADCO-OPT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "MN/LPMAN1" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
               SIZE 80 + VDU-WIN-W-MOD
               LINES 24 + VDU-WIN-H-MOD
               COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
               CELL SIZE IS LABEL FONT
               ERASE SCREEN
               HANDLE IS LPMAIN-WINDOW-HANDLE.
           MOVE LPMAIN-LINK TO FORM-LINK-AREA.


       LOAD-PRE.
           MOVE PROG-WK TO FORM-NAM.
       COPY "LIBGB/LOADC2.CPY".
       LOAD-POST.
           MOVE FORM-LINK-AREA TO LPMAIN-LINK.

           IF ERRCD = "X"
              GO TO END-ROUTINE.
           IF ERRCD = "E"
              MOVE "ERROR IN PROGRAM EXECUTION" TO MESS
              PERFORM SEND-MESS
              GO TO END-ROUTINE.
           IF FORMSEL = "10"
              MOVE "A" TO PROG-NO
           ELSE
           IF FORMSEL = "11"
              MOVE "B" TO PROG-NO
           ELSE
           IF FORMSEL = "12"
              MOVE "C" TO PROG-NO
           ELSE
              MOVE FORMNUM TO PROG-NO.
           GO TO LOAD-PRE.


      ****************************************
       MISC SECTION.
      ****************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/SENDMESS.CPY".
       COPY "LIBGB/ERRMSG.CPY".

      ****************************************
       END-ROUTINE SECTION.
      ****************************************
       LOAD-RESTART.
       EXIT-PROG.
           MOVE "LP/LPM2MU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DESTROY LPMAIN-WINDOW-HANDLE.
           EXIT PROGRAM.
