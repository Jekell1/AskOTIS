       IDENTIFICATION DIVISION.
       PROGRAM-ID.      PENALT.
       DATE-WRITTEN.    000126.
      *******************************************************
      *  DESC:  EARLY PAYOFF PENALTY WINDOW
      *
      *  JTG 000126
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
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)CL/PENALT A32 01/12/21-15:13:19 >".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************

       01  ENTERED-INTEREST        PIC S9(5)V99 COMP.
       01  ENTERED-PENALTY         PIC S9(5)V99 COMP.
       01  ROUTE-BUF               PIC XX  VALUE " ".
       01  EXT-EOBUF-MESS             REDEFINES ROUTE-BUF
                                   PIC XX.
       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/PENALT_DEF.CPY".
       COPY "LIBWI/PENALT_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       01  INDU-INTEREST      PIC S9(5)V99 COMP.
       01  INDU-PENALTY       PIC S9(5)V99 COMP.

       SCREEN SECTION.
       COPY "LIBWI/PENALT_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME INDU-INTEREST INDU-PENALTY
                  EXIT-PATHNAME.

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.
           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE 9           TO WINDOW-LINES.
           MOVE 33          TO WINDOW-COLUMNS.
           MOVE 9           TO WINDOW-BEGIN-Y.
           MOVE 26          TO WINDOW-BEGIN-X.
           MOVE "WI/PENALT" TO WINDOW-FORM-NAM.

           MOVE "PENALT" TO VDU-FORM-NAME.

           PERFORM OPEN-WINDOW.
           DISPLAY PENALT-SCREEN.

           PERFORM ALARM.
           PERFORM SET-INITIAL-FIELDS.

       ENTER-PENALTY.
           MOVE " ENTER PENALTY TO CHARGE:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNS052030." TO VDU.
           PERFORM SCREENIO.
           IF VDUNUM2 < 0
              GO TO ENTER-PENALTY.
           MOVE VDUNUM2 TO ENTERED-PENALTY.

           MOVE " F7-EXIT, F1-RESET, ^-BACK:" TO MESS.
           PERFORM SEND-LEGEND.
       ACCEPT-SEL.
           MOVE "E  A010WSELX." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY
              PERFORM SET-INITIAL-FIELDS
              GO TO ACCEPT-SEL.
           IF F7-KEY
              MOVE ENTERED-PENALTY TO INDU-PENALTY
              GO TO END-ROUTINE.
           IF UP-KEY
              GO TO ENTER-PENALTY.
           GO TO ACCEPT-SEL.

       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ******************************************
       FORM-RESET SECTION.
      ******************************************
           DISPLAY PENALT-SCREEN.
           MOVE PENALT-FORM-FIELDS TO WR-BUF.
           MOVE PENALT-FORM-LIST   TO WR-LIST.
           PERFORM CALL-WRFORM.
           
       FORM-RESET-EXIT.
           EXIT.

      ******************************************
       SETUP-LINK-INFO SECTION.
      ******************************************
           MOVE PENALT-HELP--FILE TO HELP-LINK-FILE.
           MOVE PENALT-HELP--DIR TO HELP-LINK-DIR.

      ******************************************
       VDU-CONVERT-FIELD SECTION.
      ******************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/PENALT_EVA.CPY".
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
       SET-INITIAL-FIELDS.
           MOVE INDU-INTEREST TO ENTERED-INTEREST.
           MOVE INDU-PENALTY  TO ENTERED-PENALTY.

           MOVE "SNNS052010." TO VDU.
           MOVE INDU-INTEREST TO VDUNUM2.
           PERFORM SCREENIO.

           MOVE "SNNS052020." TO VDU.
           MOVE INDU-PENALTY  TO VDUNUM2.
           PERFORM SCREENIO.

           MOVE "SNNS052030." TO VDU.
           MOVE INDU-PENALTY  TO VDUNUM2.
           PERFORM SCREENIO.


      ******************************************
       IO-ERROR SECTION.
      ******************************************
           MOVE "WINDOW ERROR " TO MESS.
           PERFORM SEND-MESS.

       END-ROUTINE.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY PENALT-SCREEN.
           PERFORM CLOSE-WINDOW UNTIL WINDOWS-OPEN = 0.
       STOP-RUN.
           EXIT PROGRAM.
