       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GIMAIN.
      *****************************************************
      *   DIR :  MN
      *   DESC:  GENERAL LEDGER CONTROL
      *****************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./MN/GIMAIN.CBL A30 02/05/16-13:31:27 >".
       01  EXT-EOBUF-MESS        PIC X(60).
       01  PROG-WK.
           03  FILLER        PIC X(8)  VALUE "MN/GIMAN".
           03  PROG-NO       PIC 9     VALUE  1 .

       01  GIMAIN-LINK       VALUE SPACES.
           03  FORM-NAME     PIC X(20).
           03  SAVEKEY       PIC 9(7).
           03  NEW-REC       PIC X.
           03  ERRCD         PIC X.
           03  FORMSEL       PIC 99.
           03  GIMAIN-WINDOW-HANDLE  PIC X(10).

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/LOADCOW.CPY".
       01 FORM-LINK-AREA     PIC X(2000).

       LINKAGE SECTION.
       01  FORM-PATHNAME PIC X(22).
       01  EXIT-PATHNAME.
           03  FILLER         PIC X(12).
           03  EXIT-PATH      PIC X(9).
           03  FILLER         PIC X.
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      **********************************************************
       MAIN-PROGRAM SECTION.
      **********************************************************

           MOVE "OPT" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF STAT = "00"
              MOVE GETENV-BUF TO LOADCO-OPT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "MN/GIMAN1" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       COPY "LIBGB/CHKSEC.CPY".

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   ERASE SCREEN
                   HANDLE IS GIMAIN-WINDOW-HANDLE.

           MOVE ALL "0" TO SAVEKEY.

           MOVE GIMAIN-LINK TO FORM-LINK-AREA.

       LOAD-PRE.
           MOVE PROG-WK TO FORM-NAM.
       COPY "LIBGB/LOADC2.CPY".
       LOAD-POST.
           MOVE FORM-LINK-AREA TO GIMAIN-LINK.

           IF ERRCD = "X"
              GO TO END-ROUTINE.
           IF ERRCD = "E"
              MOVE "ERROR IN PROGRAM EXECUTION" TO MESS
              PERFORM SEND-MESS
              GO TO END-ROUTINE.

           MOVE FORMSEL TO PROG-NO.
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
           DESTROY GIMAIN-WINDOW-HANDLE.
           EXIT PROGRAM.
