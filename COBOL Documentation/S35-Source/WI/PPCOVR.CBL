       IDENTIFICATION DIVISION.
       PROGRAM-ID.      PPCOVR.
       DATE-WRITTEN.    031203.
      ***********************************************************************
      *  DESC:  ENTRY OF (EXHIBIT A) PP COVERAGE WINDOW
      *
      * REV:
      *  MG  991004 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *  BLV 000726 DON'T ALLOW F10 INTERRUPT (F10 FROM PPCOVR
      *             WINDOW COULD FREAK OUT SCREEN, ESP. IF
      *             USER WENT INTO PAYMENT POSTING FROM F10
      *  JTG 030312 **NEW** ADDED GP-FORCE-INPUT-PP-COVERAGE       WORLD #319
      *  JTG 030324 REMOVED F7 EXIT AT "ENTER EXHIBIT A COVERAGE:" WORLD #319
      *             AND ALLOWED COVERAGE IN EXCESS OF COMPUTED
      *  JTG 030513 SUPRESSED MESSAGE "EXCEEDS COMPUTED COVERAGE AMOUNT!!"
      *                                                            WORLD #319
      ***********************************************************************
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)WI/PPCOVR.CBL S34 06/03/21-15:03:48 >".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************

       01  ROUTE-BUF               PIC XX  VALUE " ".
       01  EXT-EOBUF-MESS              REDEFINES ROUTE-BUF
                                   PIC XX.

       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       01 PPCOVR-WINDOW-HANDLE     PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/PPCOVR_DEF.CPY".
       COPY "LIBWI/PPCOVR_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/PPCOVRW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/PPCOVR_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME PPCOVR-WORKERS EXIT-PATHNAME.

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE "PPCOVR" TO VDU-FORM-NAME.

       COPY "LIBGB/CHKSEC.CPY".

           DISPLAY FLOATING GRAPHICAL WINDOW
               AT SCREEN LINE 10 * VDU-WIN-L-OFFSET
               AT SCREEN COL 18 * VDU-WIN-C-OFFSET
               CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
               SIZE 40 + VDU-WIN-W-MOD
               LINES 08 + VDU-WIN-H-MOD
               BLANK SCREEN
               HANDLE IS PPCOVR-WINDOW-HANDLE.

           PERFORM FORM-RESET.

      * SEND PERSONAL PROPERTY COMPUTED COVERAGE:
           MOVE "SNNN062COMPU." TO VDU.
           MOVE PPCOVR-COMPUTED TO VDUNUM2.
           PERFORM SCREENIO.

           MOVE " " TO VDU-WIMENU-SELECTION.
           PERFORM ALARM.
       PPCOVR-ENTER-EXHIBIT-A.
           MOVE " ENTER EXHIBIT A COVERAGE:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN060010." TO VDU.
           PERFORM SCREENIO.
           IF NOT ENTER-KEY
              GO TO PPCOVR-ENTER-EXHIBIT-A.
           IF VDUNUM0 = 0
              GO TO PPCOVR-ENTER-EXHIBIT-A.
           MOVE VDUNUM0 TO PPCOVR-EXHIBIT-A.

           MOVE " F7-EXIT, ^-BACK:" TO MESS.
           PERFORM SEND-LEGEND.
       PPCOVR-ENTER-SEL.
           MOVE "E  A010SEL." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY
              GO TO END-PPCOVR.
           IF UP-KEY
              GO TO PPCOVR-ENTER-EXHIBIT-A.
           IF NOT F7-KEY
              GO TO PPCOVR-ENTER-SEL.

       END-PPCOVR.
           GO TO END-ROUTINE.

       COPY "LIBGB/SCREEN.CPY".

      ********************************************
       FORM-RESET SECTION.
      ********************************************
           DISPLAY SPACES UPON PPCOVR-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.
           DISPLAY PPCOVR-SCREEN UPON PPCOVR-WINDOW-HANDLE.
           MOVE PPCOVR-FORM-FIELDS TO WR-BUF.
           MOVE PPCOVR-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ********************************************
       SETUP-LINK-INFO SECTION.
      ********************************************
           MOVE PPCOVR-HELP--FILE TO HELP-LINK-FILE.
           MOVE PPCOVR-HELP--DIR TO HELP-LINK-DIR.

      ********************************************
       VDU-CONVERT-FIELD SECTION.
      ********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/PPCOVR_EVA.CPY".
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
           MOVE "WINDOW ERROR " TO MESS.
           PERFORM SEND-MESS.
       END-ROUTINE.
       EXIT-PROG.
           DESTROY PPCOVR-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
