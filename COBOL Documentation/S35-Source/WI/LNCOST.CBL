       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LNCOST.
       DATE-WRITTEN.    030899.
      *******************************************************
      *
      *  USED TO MANUALLY INPUT LOAN COSTS IN DIRECT LOAN (XONPC0)
      *
      * REV:
      *  070404 PR#2942 COLTN
      *
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
           "@(#)WI/LNCOST A30 07/08/14-08:52:21 09/18/14-13:58:55 >".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************

       01  ERRCD                   PIC X VALUE " ".
       01  EXT-EOBUF-MESS          PIC X(15).

       01  FIRST-TIME      PIC X            VALUE "Y".

       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/LNCOST_DEF.CPY".
       COPY "LIBWI/LNCOST_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".


       01  WK-LNCOST-WORKER.
           03  WK-LNCOST-COST      PIC S9(6)V99.


       COPY "LIBGB/GETFMW.CPY".
       01  LNCOST-WORKER.
           03  LNCOST-COST         PIC S9(6)V99.

       SCREEN SECTION.
       COPY "LIBWI/LNCOST_SCN.CPY".


       PROCEDURE DIVISION
            USING FORM-PATHNAME LNCOST-WORKER EXIT-PATHNAME.

      **************************************************
       MAIN-PROGRAM SECTION.
      **************************************************

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE 09 TO WINDOW-LINES.
           MOVE 40 TO WINDOW-COLUMNS.
           MOVE 14 TO WINDOW-BEGIN-Y.
           MOVE 9 TO WINDOW-BEGIN-X.
           MOVE "WI/LNCOST" TO WINDOW-FORM-NAM.
           MOVE "LNCOST" TO VDU-FORM-NAME.

           PERFORM OPEN-WINDOW.
           PERFORM ALARM.
           DISPLAY LNCOST-SCREEN.

           MOVE LNCOST-FORM-FIELDS TO WR-BUF.
           MOVE LNCOST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "N" TO FIRST-TIME.
       WINDOW-RESET.
           MOVE LNCOST-COST   TO WK-LNCOST-COST.
           IF LNCOST-COST   NOT = ZEROS
              MOVE "S NS062010." TO VDU
              MOVE LNCOST-COST TO VDUNUM2
              PERFORM SCREENIO.

       WINDOW-ENTER-LNCOST.
           IF FIRST-TIME = "Y"
              MOVE "N" TO FIRST-TIME.

           MOVE "E NN062010." TO VDU
           PERFORM SCREENIO.

           IF VDUSTATUS = "10"
              GO TO WINDOW-RESET.

           IF NOT (VDUSTATUS = "00")
              GO TO WINDOW-ENTER-LNCOST.

           MOVE VDUNUM2 TO WK-LNCOST-COST.

       END-WINDOW.

           MOVE WK-LNCOST-WORKER TO LNCOST-WORKER.
           GO TO END-ROUTINE.

       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE LNCOST-HELP--FILE TO HELP-LINK-FILE.
           MOVE LNCOST-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/LNCOST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ********************************************
       MISC SECTION.
      ********************************************
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SENDMESS.CPY".
       ALARM.
           DISPLAY OMITTED WITH BEEP.
       SEND-LEGEND.
           MOVE "WSEL:D." TO WR-LIST.
           MOVE MESS TO WR-BUF.
           PERFORM CALL-WRFORM.

      ********************************************
       IO-ERROR SECTION.
      ********************************************

      ********************************************
       END-ROUTINE SECTION.
      ********************************************
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LNCOST-SCREEN.
           PERFORM CLOSE-WINDOW UNTIL WINDOWS-OPEN = 0.
       STOP-RUN.
           EXIT PROGRAM.
