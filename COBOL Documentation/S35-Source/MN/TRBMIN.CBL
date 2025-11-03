       IDENTIFICATION DIVISION.
       PROGRAM-ID.      TRBMIN.
      ******************************************************************
      *
      *    TRANSACTION FILE BRANCH TRANSFER MAINTENANCE DRIVER
      *
      * MJD 131204 A30 CONVERSION COMPLETED
      *  CS 161003 CHANGED SAVEKEY FROM X(14) TO X(27)
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       DATA DIVISION.

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)MN/TRBMIN A30 03/29/18-11:33:19 >".

      ******************************************************************
      *    W O R K E R S
      ******************************************************************
       01  PROG-WK.
           03  FILLER        PIC X(8)  VALUE "MN/TRBMN".
           03  PROG-NO       PIC X     VALUE "1".

       01  EXT-EOBUF-MESS    PIC X  VALUE SPACES.

       01  TRBMIN-LINK VALUE SPACES.
           03  FORM-NAME     PIC X(20).
           03  SAVEKEY       PIC X(28).
           03  NEW-REC       PIC X.
           03  ERRCD         PIC X.
           03  FORMSEL       PIC 99.
           03 FILLER REDEFINES FORMSEL.
               05  FILLER    PIC 9.
               05  FORMNUM   PIC 9.
           03  SAVEBRNO      PIC 9(4).
           03  TRBMIN-WINDOW-HANDLE PIC X(10).

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/LOADCOW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       01 FORM-LINK-AREA     PIC X(2000).

       LINKAGE SECTION.
       01  FORM-PATHNAME      PIC X(22).
       01  EXIT-PATHNAME.
           03  FILLER         PIC X(12).
           03  EXIT-PATH      PIC X(9).
           03  FILLER         PIC X.
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N    P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           MOVE "OPT" TO GETENV-BUF.
           PERFORM GETENV-CALL.

           IF STAT = "00"
              MOVE GETENV-BUF TO LOADCO-OPT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "MN/TRBMN1" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       COPY "LIBGB/CHKSEC.CPY".
           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
      *             WITH NO SCROLL
      *             WITH NO WRAP
                   HANDLE IS TRBMIN-WINDOW-HANDLE.
           MOVE TRBMIN-LINK TO FORM-LINK-AREA.


      *----------------------------------------------------------------
       LOAD-PRE.
           MOVE PROG-WK TO FORM-NAM.

       COPY "LIBGB/LOADC2.CPY".

      *----------------------------------------------------------------
       LOAD-POST.
           MOVE FORM-LINK-AREA TO TRBMIN-LINK.

           IF ERRCD = "X"
              GO TO END-ROUTINE.

           IF ERRCD = "E"
              MOVE "ERROR IN PROGRAM EXECUTION" TO MESS
              PERFORM SEND-MESS
              GO TO END-ROUTINE.

           MOVE FORMNUM TO PROG-NO.
           GO TO LOAD-PRE.

      ******************************************************************
       MISC SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/SENDMESS.CPY".
       COPY "LIBGB/ERRMSG.CPY".


      ******************************************************************
       END-ROUTINE SECTION.
      ******************************************************************
       LOAD-RESTART.
       EXIT-PROG.
           MOVE "LP/LPM2MU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           DESTROY TRBMIN-WINDOW-HANDLE.
           EXIT PROGRAM.
