       IDENTIFICATION DIVISION.
       PROGRAM-ID.      TRPMIN.
      ******************************************************************
      *
      *    PAYMENT MASTER FILE MAINTENANCE DRIVER
      *
      * MJD 131203 A30 CONVERSION COMPLETED
      *  CS 160930 CHANGED SAVEKEY TO X(27)
      * BLM 211011 CHANGED SAVEKEY TO X(26)
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       DATA DIVISION.

      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./MN/TRPMIN.CBL S34 08/11/21-13:55:26 >".

      ******************************************************************
      *    W O R K E R S
      ******************************************************************
       01  PROG-WK.
           03  FILLER        PIC X(8)  VALUE "MN/TRPMN".
           03  PROG-NO       PIC X     VALUE "1".

       01  EXT-EOBUF-MESS        PIC X  VALUE SPACES.

       01  TRPMIN-LINK VALUE SPACES.
           03  FORM-NAME     PIC X(20).
           03  SAVEKEY       PIC X(26).
           03  NEW-REC       PIC X.
           03  ERRCD         PIC X.
           03  FORMSEL       PIC 99.
           03  FILLER REDEFINES FORMSEL.
               05  FILLER    PIC 9.
               05  FORMNUM   PIC 9.
           03  SAVEBRNO      PIC 9(4).
           03  TRPMIN-WINDOW-HANDLE PIC X(10).

      ******************************************************************
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/LOADCOW.CPY".

       01 FORM-LINK-AREA     PIC X(2000).
       COPY "LIBGB/CONAME_EXT.CPY".

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
      *    M A I N    M O D U L E
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
      D    STOP MESS.
           MOVE "OPT" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF STAT = "00"
              MOVE GETENV-BUF TO LOADCO-OPT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "MN/TRPMN1" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       COPY "LIBGB/CHKSEC.CPY".
           DISPLAY FLOATING GRAPHICAL WINDOW
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   ERASE SCREEN
                   HANDLE IS TRPMIN-WINDOW-HANDLE.

           MOVE TRPMIN-LINK TO FORM-LINK-AREA.

       LOAD-PRE.
           MOVE PROG-WK TO FORM-NAM.

       COPY "LIBGB/LOADC2.CPY".

       LOAD-POST.
           MOVE FORM-LINK-AREA TO TRPMIN-LINK.

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
           DESTROY TRPMIN-WINDOW-HANDLE
           EXIT PROGRAM.
