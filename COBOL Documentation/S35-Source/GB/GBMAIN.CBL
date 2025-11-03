       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GBMAIN.
      *****************************************************
      *   DIR :  GB
      *   DESC:  GLOBAL FILE MAINTENANCE - DRIVER
      *   REV :
      *   CS 190726 CHANGED GBMNAL TO BE THE DRIVER ADDING A PAGE2
      *             FOR EOM/EOY LOAN PROCESSING TAGS
      *****************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./GB/GBMAIN.CBL J35 04/04/24-11:14:59 >".
       01  PROG-WK.
           03  FILLER        PIC X(8)  VALUE "GB/GBMAN".
           03  PROG-NO       PIC X     VALUE "1".
       01  EXT-EOBUF-MESS        PIC X(60).

       01  GBMAIN-LINK.
           03  SAVEKEY.
               05  SAVE-BRNO        PIC 9(4).
               05  SAVE-NO          PIC 9.
           03  NEW-REC              PIC X.
           03  ERRCD                PIC X.
           03  FORMNUM              PIC 9.
           03  GBMAIN-WINDOW-HANDLE PIC X(10).

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SCREENW.CPY".
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

      *******************************************************
       MAIN-PROGRAM SECTION.
      *******************************************************

      D    STOP MESS.
           MOVE "OPT" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF STAT-GOOD
              MOVE GETENV-BUF TO LOADCO-OPT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "GB/GBMAN1" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
           MOVE GBMAIN-LINK TO FORM-LINK-AREA.

           DISPLAY STANDARD GRAPHICAL WINDOW
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   ERASE SCREEN
                   HANDLE IS GBMAIN-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".

       LOAD-PRE.
           MOVE PROG-WK TO FORM-NAM.
       COPY "LIBGB/LOADC2.CPY".
       LOAD-POST.
           MOVE FORM-LINK-AREA TO GBMAIN-LINK.
           IF ERRCD = "X"
              GO TO END-ROUTINE.
           IF ERRCD = "E"
              MOVE "ERROR IN PROGRAM EXECUTION" TO MESS
              PERFORM SEND-MESS
              GO TO END-ROUTINE.

           MOVE FORMNUM TO PROG-NO.
           GO TO LOAD-PRE.

      ************************************
       MISC SECTION.
      ************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/SENDMESS.CPY".
       COPY "LIBGB/ERRMSG.CPY".

      ************************************
       END-ROUTINE SECTION.
      ************************************
       LOAD-RESTART.
       EXIT-PROG.
           MOVE EXIT-PATH TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DESTROY GBMAIN-WINDOW-HANDLE.
           EXIT PROGRAM.
