       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SPMAIN.
      ******************************************************************
      *
      * IDENTIFICATION:  MN/SPMAIN.C
      *
      * DESCRIPTION   :  STATE PARAMETER MAINTENANCE DRIVER
      *
      *-----------------------------------------------------------------
      * REV:
      * JTG 950623 ADDED PAGE 19
      * BLL 950630 PUT PASSWORD LOGIC BACK IN
      * JFF 960705 ADDED PAGE 20
      * KEC 980923 ADDED PAGE 21
      * KEC 010611 {PR#01478} REGIONAL ACCEPTANCE [KEVIN MULLEN]
      *             ADDED PAGE 23
      *  CS 130213 A30 COMPLETED
      *  CS 130228 ADDED LIBGB/ERRMSG MISC SECTION
      * MJD 131101 CHANGED FROM STANDARD TO FLOATING WINDOW AND
      *            MOVED SETUP OF THE LINKAGE AREA TO AFTER DEFINEING
      *            THE WINDOW HANDLE.
      *  CS 160728 CHANGED SAVEKEY FROM PIC X(8) TO X(9)
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)MN/SPMAIN S34 08/02/21-12:12:37 >".
       01  EXT-EOBUF-MESS        PIC X(60).
       01  PROG-WK.
           03  FILLER        PIC X(8)  VALUE "MN/SPMAN".
           03  PROG-NO       PIC X     VALUE "1".

       01  SPMAIN-LINK VALUE SPACES.
           03  FORM-NAME     PIC X(20).
           03  SAVEKEY       PIC X(9).
           03  NEW-REC       PIC X.
           03  ERRCD         PIC X.
           03  FORMSEL.
               05  FILLER    PIC 9.
               05  FORMNUM   PIC 9.
           03  SPMAIN-WINDOW-HANDLE PIC X(10).

       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/LOADCOW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       01  FORM-LINK-AREA       PIC X(2000).

       LINKAGE SECTION.
       01  FORM-PATHNAME      PIC X(22).
       01  EXIT-PATHNAME.
           03  FILLER         PIC X(12).
           03  EXIT-PATH      PIC X(9).
           03  FILLER         PIC X.

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

      **********************************************
       MAIN-PROGRAM SECTION.
      **********************************************
      D    STOP MESS.

           MOVE "OPT" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           IF STAT = "00"
              MOVE GETENV-BUF TO LOADCO-OPT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "MN/SPMAN1" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       COPY "LIBGB/CHKSEC.CPY".

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                    ERASE SCREEN
                 HANDLE IS SPMAIN-WINDOW-HANDLE.

           MOVE SPMAIN-LINK TO FORM-LINK-AREA.

       LOAD-PRE.
           MOVE PROG-WK TO FORM-NAM.
       COPY "LIBGB/LOADC2.CPY".
       LOAD-POST.
           MOVE FORM-LINK-AREA TO SPMAIN-LINK.

           IF ERRCD = "X"
              GO TO END-ROUTINE.
           IF ERRCD = "E"
              MOVE "ERROR IN PROGRAM EXECUTION" TO MESS
              PERFORM SEND-MESS
              GO TO END-ROUTINE.
           IF FORMSEL = "10"
              MOVE "A" TO PROG-NO
           ELSE IF FORMSEL = "11"
                MOVE "B" TO PROG-NO
           ELSE IF FORMSEL = "12"
                MOVE "C" TO PROG-NO
           ELSE IF FORMSEL = "13"
                MOVE "D" TO PROG-NO
           ELSE IF FORMSEL = "14"
                MOVE "E" TO PROG-NO
           ELSE IF FORMSEL = "15"
                MOVE "F" TO PROG-NO
           ELSE IF FORMSEL = "16"
                MOVE "G" TO PROG-NO
           ELSE IF FORMSEL = "17"
                MOVE "H" TO PROG-NO
           ELSE IF FORMSEL = "18"
                MOVE "I" TO PROG-NO
           ELSE IF FORMSEL = "19"
                MOVE "J" TO PROG-NO
           ELSE IF FORMSEL = "20"
                MOVE "K" TO PROG-NO
           ELSE IF FORMSEL = "21"
                MOVE "L" TO PROG-NO
           ELSE IF FORMSEL = "22"
                MOVE "M" TO PROG-NO
           ELSE IF FORMSEL = "23"
                MOVE "N" TO PROG-NO
           ELSE
              MOVE FORMNUM TO PROG-NO.

      ************  PASSWORD CHECK  **************
           IF FORMSEL > 13 AND < 18
              PERFORM VERIFY-PASSWORD
              IF PASSWDW-STAT NOT= "OK"
                 MOVE 1 TO PROG-NO FORMSEL.

           GO TO LOAD-PRE.

      *******************************************************
       VERIFY-PASSWORD SECTION.
      *******************************************************
           MOVE " " TO PASSWDW-STAT.
           MOVE "SPMAN" TO PASSWDW-PGM.
           MOVE FORMSEL TO PASSWDW-SEQ.
           INSPECT PASSWDW-PGM REPLACING FIRST SPACE BY PROG-NO.
           MOVE "PLEASE ENTER THE PASSWORD," TO PASSWDW-PROMPT1.
           MOVE "CL/PASSWD" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH PASSWDW-BUF.
           CANCEL FORM-PROGX.

      *******************************************************
       MISC SECTION.
      *******************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/SENDMESS.CPY".
       COPY "LIBGB/ERRMSG.CPY".

      *******************************************************
       END-ROUTINE SECTION.
      *******************************************************
       LOAD-RESTART.
       EXIT-PROG.
           MOVE "LP/LPM2MU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DESTROY SPMAIN-WINDOW-HANDLE.
           EXIT PROGRAM.
