       IDENTIFICATION DIVISION.
       PROGRAM-ID.      TENDER.
       DATE-WRITTEN.    080392.
      *******************************************************
      *  DESC:  CASH TENDER WINDOW
      *           CALLED FROM LONPF2 IF REFNO'S MATCH BRANCH CASH
      *                 TENDER CODES BRMAN6
      *
      *  REV:
      *  MG  991004 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *  BLV 000726 DON'T ALLOW F10 INTERRUPT (F10 FROM TENDER
      *             WINDOW COULD FREAK OUT SCREEN, ESP. IF
      *             USER WENT INTO PAYMENT POSTING FROM F10
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
           "@(#)CL/TENDER A30 07/08/14-14:00:33 09/18/14-13:47:14 >".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************

       01  AMT                     PIC S9(7)V99 COMP-3.
       01  ROUTE-BUF               PIC XX  VALUE " ".
       01  EXT-EOBUF-MESS              REDEFINES ROUTE-BUF
                                   PIC XX.
       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/TENDER_DEF.CPY".
       COPY "LIBWI/TENDER_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01  TENDER-LP-TRAMT         PIC S9(7)V99.

       SCREEN SECTION.
       COPY "LIBWI/TENDER_SCN.CPY".

      **************************************************
      *   C O N T R O L   M O D U L E                  *
      **************************************************

       PROCEDURE DIVISION
            USING FORM-PATHNAME TENDER-LP-TRAMT EXIT-PATHNAME.

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE 9           TO WINDOW-LINES.
           MOVE 24          TO WINDOW-COLUMNS.
           MOVE 9           TO WINDOW-BEGIN-Y.
           MOVE 29          TO WINDOW-BEGIN-X.
           MOVE "WI/TENDER" TO WINDOW-FORM-NAM.

           MOVE "TENDER" TO VDU-FORM-NAME.

           PERFORM OPEN-WINDOW.
           DISPLAY TENDER-SCREEN.

           MOVE TENDER-FORM-FIELDS TO WR-BUF.
           MOVE TENDER-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE " " TO VDU-WIMENU-SELECTION.

           PERFORM ALARM.
       CASH-TENDERED-ENTER-CASH.
           MOVE " F7, ENTER CASH:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN062010." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY
              GO TO END-CASH-TENDERED.
           IF NOT ENTER-KEY
              GO TO CASH-TENDERED-ENTER-CASH.
           IF VDUNUM2 = 0 OR VDUNUM2 < TENDER-LP-TRAMT
              GO TO CASH-TENDERED-ENTER-CASH.

           MOVE VDUNUM2 TO AMT.

           MOVE "SNNN062020." TO VDU.
           MOVE TENDER-LP-TRAMT TO VDUNUM2.
           PERFORM SCREENIO.

           MOVE "S  A08003A." TO VDU.
           MOVE "CHANGE :" TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SNNN062030:F." TO VDU.
           SUBTRACT TENDER-LP-TRAMT FROM AMT GIVING VDUNUM2.
           PERFORM SCREENIO.

           MOVE " F7-EXIT, ^-BACK:" TO MESS.
           PERFORM SEND-LEGEND.
       CASH-TENDERED-ENTER-SEL.
           MOVE "E  A010SEL." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY
              GO TO END-CASH-TENDERED.
           IF UP-KEY
              GO TO CASH-TENDERED-ENTER-CASH.

           IF NOT F7-KEY
              GO TO CASH-TENDERED-ENTER-SEL.

       END-CASH-TENDERED.
           GO TO END-ROUTINE.

       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ********************************************
       VDU-CONVERT-FIELD SECTION.
      ********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/TENDER_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ********************************************
       MISC SECTION.
      ********************************************
       FORM-RESET.
       SETUP-LINK-INFO.
           MOVE TENDER-HELP--FILE TO HELP-LINK-FILE.
           MOVE TENDER-HELP--DIR  TO HELP-LINK-DIR.
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SENDMESS.CPY".
       ALARM.
           DISPLAY OMITTED WITH BEEP.
       SEND-LEGEND.
           MOVE "WSEL:D." TO WR-LIST.
           MOVE MESS TO WR-BUF.
           PERFORM CALL-WRFORM.

      ***************************************
       IO-ERROR SECTION.
      ***************************************
           MOVE "WINDOW ERROR " TO MESS.
           PERFORM SEND-MESS.

       END-ROUTINE.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY TENDER-SCREEN.
           PERFORM CLOSE-WINDOW UNTIL WINDOWS-OPEN = 0.
       STOP-RUN.
           EXIT PROGRAM.
