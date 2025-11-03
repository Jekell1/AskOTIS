       IDENTIFICATION  DIVISION.
       PROGRAM-ID.       WRMAIN.
       DATE-WRITTEN. 7/10/03
      *****************************************************
      *   DIR :  MN
      *   DESC:  WORLD RATING SHEET FILE MAINT - PROG 0
      *
      *  REV:
      *****************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./MN/WRMAIN.CBL A30 12/20/16-14:38:30 >".
       01  EXT-EOBUF-MESS        PIC X(60).
       01  PROG-WK.
           03  FILLER        PIC X(8)  VALUE "MN/WRMAN".
           03  PROG-NO       PIC X     VALUE "1".

       01  WRMAIN-LINK        VALUE SPACES.
           03  SAVEKEY       PIC X(10).
           03  NEW-REC       PIC X.
           03  ERRCD         PIC X.
           03  FORMSEL.
               05  FILLER    PIC 9.
               05  FORMNUM   PIC 9.
           02  WRMAIN-WINDOW-HANDLE PIC X(10).

       01 FORM-LINK-AREA     PIC X(2000).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/LOADCOW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".

       LINKAGE SECTION.
       01  FORM-PATHNAME      PIC X(22).
       01  EXIT-PATHNAME.
           03  FILLER         PIC X(12).
           03  EXIT-PATH      PIC X(9).
           03  FILLER         PIC X.

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      ********************************************
       MAIN-PROGRAM SECTION.
      ********************************************
       MAIN-MODULE.
      D    STOP MESS.

           MOVE "OPT" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF STAT = "00"
              MOVE GETENV-BUF TO LOADCO-OPT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "MN/WRMAN1" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       COPY "LIBGB/CHKSEC.CPY".

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   ERASE SCREEN
                   HANDLE IS WRMAIN-WINDOW-HANDLE.

           MOVE WRMAIN-LINK TO FORM-LINK-AREA.

       LOAD-PRE.
           MOVE PROG-WK TO FORM-NAM.
       COPY "LIBGB/LOADC2.CPY".
       LOAD-POST.
           MOVE FORM-LINK-AREA TO WRMAIN-LINK.

           IF ERRCD = "X"
              GO TO END-ROUTINE.
           IF ERRCD = "E"
              MOVE "ERROR IN PROGRAM EXECUTION" TO MESS
              PERFORM SEND-MESS
              GO TO END-ROUTINE.
           MOVE FORMNUM TO PROG-NO.
           IF FORMSEL = "10"
              MOVE "A" TO PROG-NO.
           IF FORMSEL = "11"
              MOVE "B" TO PROG-NO.
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
           MOVE "SP/WRLDMU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DESTROY WRMAIN-WINDOW-HANDLE.
           EXIT PROGRAM.
