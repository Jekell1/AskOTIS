      * A30 CONVERSION COMPLETED
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BWINQ.
      *****************************************************
      *    BORROWER FILE INQUIRY DRIVER
      *
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
       77  SCCS-IDENTIFIER PIC X(54) VALUE
           "@(#)LP/BWINQ A30 11/04/13-15:03:56 09/18/14-13:48:54 >".
       01  PROG-WK.
           03  FILLER        PIC X(8)  VALUE "LP/BWINQ".
           03  PROG-NO       PIC X     VALUE "1".

       01  EXT-EOBUF-MESS        PIC X  VALUE SPACES.

       01  BWINQ-LINK        VALUE SPACES.
           03  FORM-NAME     PIC X(20).
           03  SAVEKEY       PIC X(9).
           03  ERRCD         PIC X.
           03  FORMSEL.
               05  FILLER    PIC 9.
               05  FORMNUM   PIC 9.
           03  BWINQ-WINDOW-HANDLE   PIC X(10).

       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/LOADCOW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       01  FORM-LINK-AREA      PIC X(2000).

       LINKAGE SECTION.
       01  FORM-PATHNAME PIC X(22).
       01  EXIT-PATHNAME PIC X(22).
       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      ************************************************
       MAIN-PROGRAM SECTION.
      ************************************************

           MOVE "OPT" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF STAT = "00"
              MOVE GETENV-BUF TO LOADCO-OPT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LP/BWINQ1" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

      D    STOP MESS.
       COPY "LIBGB/CHKSEC.CPY".

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   ERASE SCREEN
      *             WITH NO SCROLL
      *             WITH NO WRAP
                   HANDLE IS BWINQ-WINDOW-HANDLE.
           MOVE BWINQ-LINK TO FORM-LINK-AREA.

       LOAD-PRE.
           MOVE PROG-WK TO FORM-NAM.
       COPY "LIBGB/LOADC2.CPY".
       LOAD-POST.
           MOVE FORM-LINK-AREA TO BWINQ-LINK.
           IF ERRCD = "X"
              GO TO END-ROUTINE.
           IF ERRCD = "E"
              MOVE "ERROR IN PROGRAM EXECUTION" TO MESS
              PERFORM SEND-MESS
              GO TO END-ROUTINE.
           MOVE FORMNUM TO PROG-NO.
           GO TO LOAD-PRE.

      ********************************************
       MISC SECTION.
      ********************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/SENDMESS.CPY".
       COPY "LIBGB/ERRMSG.CPY".

      ********************************************
       END-ROUTINE SECTION.
      ********************************************
       LOAD-RESTART.
       EXIT-PROG.
           MOVE "LP/LPIQMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DESTROY BWINQ-WINDOW-HANDLE.
           EXIT PROGRAM.
