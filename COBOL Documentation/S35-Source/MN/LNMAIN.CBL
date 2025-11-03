       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LNMAIN.
      *****************************************************
      *   DIR :  MN
      *   DESC:  LOAN MASTER FILE MAINT - PROG 0
      *
      *  REV:
      *  MJD 082297 ADDED LOGIC FOR PAGE 10.
      *  KEC 091598 ADDED LOGIC FOR PAGE 11.
      *  JTG 120198 ADDED LOGIC FOR PAGE 12.
      *  MJD 121018 A30 CONVERSION COMPLETE
      *  MJD 121024 REMOVED POP UP WINDOW LOGIC
      *  MJD 121217 ADDED BACK WINDOW LOGIC AND INCLUDED HANDLE.
      *  BLM 130219 ADDED SECURITY
      *  BLM 130228 FIX CALL-ERRMSG
      *  MJD 131031 CHANGED WINDOW FROM STANDARD TO FLOATING.
      *  BLM 211216 REMOVE BLOWING UP TRYING TO GO TO PAGE 12
      *****************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)MN/LNMAIN.CBL S35 04/06/22-14:30:31 >".
       01  EXT-EOBUF-MESS        PIC X(60).
       01  PROG-WK.
           03  FILLER        PIC X(8)  VALUE "MN/LNMAN".
           03  PROG-NO       PIC X     VALUE "1".

       01  LNMAIN-LINK       VALUE SPACES.
           03  FORM-NAME     PIC X(20).
           03  SAVEKEY       PIC X(8).
           03  NEW-REC       PIC X.
           03  ERRCD         PIC X.
           03  FORMSEL.
               05  FILLER    PIC 9.
               05  FORMNUM   PIC 9.
           03  LNMAIN-WINDOW-HANDLE  PIC X(10).

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
       01  FORM-PATHNAME      PIC X(22).
       01  EXIT-PATHNAME.
           03  FILLER         PIC X(12).
           03  EXIT-PATH      PIC X(9).
           03  FILLER         PIC X.

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      ************************************
       MAIN-PROGRAM SECTION.
      ************************************
       MAIN-MODULE.
      D    STOP MESS.

           MOVE "OPT" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF STAT = "00"
              MOVE GETENV-BUF TO LOADCO-OPT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "MN/LNMAN1" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       COPY "LIBGB/CHKSEC.CPY".

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   ERASE SCREEN
                   HANDLE IS LNMAIN-WINDOW-HANDLE.

           MOVE LNMAIN-LINK TO FORM-LINK-AREA.

       LOAD-PRE.
           MOVE PROG-WK TO FORM-NAM.
       COPY "LIBGB/LOADC2.CPY".
       LOAD-POST.
           MOVE FORM-LINK-AREA TO LNMAIN-LINK.

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
           DESTROY LNMAIN-WINDOW-HANDLE.
           EXIT PROGRAM.
