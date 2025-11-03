       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SFDLO3.
       DATE-WRITTEN.    061995.
      *******************************************************
      *  DESC:  DISBURSE O3 WARRANTY PREMIUM
      *           CALLED FROM LONPB0 & LONIJ0
      *
      *  MG  991004 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      * BAH  091015 MOVED SFDLO3 WORKERS TO COPY MEMBER SFDLO3W
      *             GP-REGMGM "NC" ONLY - FORCE 495. PREMIUM, 25. PAYABLE
      *             THEY ENTER DEALER COMM 50.00 OR 0, THEN COMPUTE COMM
      *              REGMGM PR# 3475
      * BAH 100217 ADDED "AL" TO ABOVE CHANGE, REGMGM PR# 3543
      * BAH 130131 A30 CONVERSION
      * BAH 131219 FORCE 595. PREMIUM, 25. PAYABLE AND THEY CAN ENTER
      *            150.00 OR 0, THEN COMPUTE COMM, REGMGM PR# 4440
      * BAH 20220127 COMMENTED OUT GP-I-AM
      *******************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(55) VALUE
           "@(#)CL/SFDLO3 A30 07/09/14-08:40:22 09/18/14-13:47:11 >".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************

       01  ROUTE-BUF               PIC XX  VALUE " ".
       01  EXT-EOBUF-MESS              REDEFINES ROUTE-BUF
                                   PIC XX.
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       01 SFDLO3-WINDOW-HANDLE     PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/SFDLO3_DEF.CPY".
       COPY "LIBWI/SFDLO3_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       01  INSPAYABLE              PIC S9(6)V99.
       01  COMPUTE-FG              PIC X.
       01  WK-SFDLO3-WORKERS.
           03  WK-SFDLO3-PREM      PIC S9(6)V99.
           03  WK-SFDLO3-COMM      PIC S9(6)V99.
           03  WK-SFDLO3-DLPROC    PIC S9(6)V99.

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBCL/SFDLO3W.CPY".


       SCREEN SECTION.
       COPY "LIBWI/SFDLO3_SCN.CPY".


       PROCEDURE DIVISION
            USING FORM-PATHNAME SFDLO3-WORKERS EXIT-PATHNAME.

      **************************************************
       MAIN-PROGRAM SECTION.
      **************************************************

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "SFDLO3" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT SCREEN LINE 12 * VDU-WIN-L-OFFSET
                   AT SCREEN COL 09 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   SIZE 29 + VDU-WIN-W-MOD
                   LINES 11 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS SFDLO3-WINDOW-HANDLE.


           PERFORM FORM-RESET.

       SFDLO3-RESET.
           MOVE SFDLO3-WORKERS TO WK-SFDLO3-WORKERS.

      * INS. PREMIUM:
           MOVE "SNNS062010." TO VDU.
           MOVE SFDLO3-PREM TO VDUNUM2.
           PERFORM SCREENIO.

      * INS. PAYABLE:
           COMPUTE INSPAYABLE =
                      SFDLO3-PREM - SFDLO3-DLPROC - SFDLO3-COMM.
           MOVE "SNNS062020." TO VDU.
           MOVE INSPAYABLE TO VDUNUM2.
           PERFORM SCREENIO.

      * DEALER COMM:
           MOVE "SNNS062030." TO VDU.
           MOVE SFDLO3-DLPROC TO VDUNUM2.
           PERFORM SCREENIO.

      * OUR COMM:
           MOVE "SNNS062040." TO VDU.
           MOVE SFDLO3-COMM TO VDUNUM2.
           PERFORM SCREENIO.
           MOVE "C" TO COMPUTE-FG.

       SFDLO3-ENTER-PREM.
           MOVE " F1,F7, ENTER INS. PREMIUM:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNS062010." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY
              GO TO SFDLO3-RESET.
           IF F7-KEY
              GO TO END-SFDLO3.
           IF NOT ENTER-KEY
              GO TO SFDLO3-ENTER-PREM.
           IF VDUNUM2 < 0
              GO TO SFDLO3-ENTER-PREM.
           IF VDUNUM2 NOT = WK-SFDLO3-PREM
              MOVE VDUNUM2 TO WK-SFDLO3-PREM
              COMPUTE WK-SFDLO3-COMM =
                   WK-SFDLO3-PREM - INSPAYABLE - WK-SFDLO3-DLPROC
              MOVE "SNNS062040." TO VDU
              MOVE WK-SFDLO3-COMM TO VDUNUM2
              PERFORM SCREENIO
              IF COMPUTE-FG NOT = "C"
                 MOVE "C" TO COMPUTE-FG.

       SFDLO3-ENTER-INSPAYABLE.
           MOVE " F1,F7, ENTER INS.PAYABLE :" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNS062020." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY
              GO TO END-SFDLO3.
           IF F1-KEY
              GO TO SFDLO3-RESET.
           IF UP-KEY
              GO TO SFDLO3-ENTER-PREM.
           IF NOT ENTER-KEY
              GO TO SFDLO3-ENTER-INSPAYABLE.
           IF VDUNUM2 < 0
              GO TO SFDLO3-ENTER-INSPAYABLE.
           MOVE VDUNUM2 TO INSPAYABLE.
           PERFORM SEND-COMPUTED.

       SFDLO3-ENTER-DLPROC.
           MOVE " F1,F7, ENTER DEALER COMM:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNS062030." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY
              GO TO SFDLO3-RESET.
           IF UP-KEY
              GO TO SFDLO3-ENTER-INSPAYABLE.
           IF F7-KEY
              GO TO END-SFDLO3.
           IF NOT ENTER-KEY
              GO TO SFDLO3-ENTER-DLPROC.
           IF VDUNUM2 < 0
              GO TO SFDLO3-ENTER-DLPROC.
           MOVE VDUNUM2 TO WK-SFDLO3-DLPROC.
           PERFORM SEND-COMPUTED.

       SFDLO3-ENTER-COMM.
           MOVE " F1,F7, ENTER OUR COMM:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNS062040." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY
              GO TO SFDLO3-RESET.
           IF UP-KEY
              GO TO SFDLO3-ENTER-DLPROC.
           IF F7-KEY
              GO TO END-SFDLO3.
           IF NOT ENTER-KEY
              GO TO SFDLO3-ENTER-COMM.
           IF VDUNUM2 < 0
              GO TO SFDLO3-ENTER-COMM.

       END-SFDLO3.
           IF (WK-SFDLO3-PREM < 0)
                  OR (INSPAYABLE < 0)
                   OR (WK-SFDLO3-DLPROC < 0)
                    OR (WK-SFDLO3-COMM < 0)
              MOVE
              "NEGATIVE AMOUNTS NOT ALLOWED!!" TO MESS
              PERFORM SEND-MESS
              GO TO SFDLO3-ENTER-PREM.

           MOVE WK-SFDLO3-WORKERS TO SFDLO3-WORKERS.
           GO TO END-ROUTINE.

       COPY "LIBGB/SCREEN.CPY".

      **************************************************
       FORM-RESET SECTION.
      **************************************************
           DISPLAY SFDLO3-SCREEN UPON SFDLO3-WINDOW-HANDLE.
           MOVE SFDLO3-FORM-FIELDS TO WR-BUF.
           MOVE SFDLO3-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************
           MOVE SFDLO3-HELP--FILE TO HELP-LINK-FILE.
           MOVE SFDLO3-HELP--DIR TO HELP-LINK-DIR.

      **************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/SFDLO3_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************************************
       MISC SECTION.
      **************************************************
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SENDMESS.CPY".
       SEND-LEGEND.
           MOVE "WSEL:D." TO WR-LIST.
           MOVE MESS TO WR-BUF.
           PERFORM CALL-WRFORM.
       SEND-COMPUTED.
           IF COMPUTE-FG = "C"
              COMPUTE WK-SFDLO3-COMM =
                   WK-SFDLO3-PREM - INSPAYABLE - WK-SFDLO3-DLPROC
              MOVE "SNNS062040." TO VDU
              MOVE WK-SFDLO3-COMM TO VDUNUM2
              PERFORM SCREENIO
           ELSE
              COMPUTE WK-SFDLO3-PREM =
                   INSPAYABLE + WK-SFDLO3-DLPROC + WK-SFDLO3-COMM
              MOVE "SNNS062010." TO VDU
              MOVE WK-SFDLO3-PREM TO VDUNUM2
              PERFORM SCREENIO.

      ***********************************
       IO-ERROR SECTION.
      ***********************************
           MOVE "WINDOW ERROR " TO MESS.
           PERFORM SEND-MESS.

      ***********************************
       END-ROUTINE SECTION.
      ***********************************
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY SFDLO3-SCREEN.
           DESTROY SFDLO3-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
