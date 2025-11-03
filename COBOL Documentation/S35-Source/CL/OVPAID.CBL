       IDENTIFICATION DIVISION.
       PROGRAM-ID.      OVPAID.
       DATE-WRITTEN.    080392.
      ********************************************************************
      *  DESC:  OVERPAY PAYOFF WINDOW
      *          CALLED FROM LONPFA.C
      *
      * REV:
      *  MG  991004 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING
      *  BLV 000726 DON'T ALLOW F10 INTERRUPT (F10 FROM OVPAID
      *             WINDOW COULD FREAK OUT SCREEN, ESP. IF
      *             USER WENT INTO PAYMENT POSTING FROM F10
      *  JTG 001121 ** NEW **
      ********************************************************************
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
           "@(#)CL/OVPAID A30 07/08/14-14:03:09 09/18/14-13:47:04 >".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************

       01  ROUTE-BUF               PIC XX  VALUE " ".
       01  EXT-EOBUF-MESS              REDEFINES ROUTE-BUF
                                   PIC XX.
       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/OVPAID_DEF.CPY".
       COPY "LIBWI/OVPAID_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBCL/OVPAIDW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/OVPAID_SCN.CPY".

      **************************************************
      *   C O N T R O L   M O D U L E                  *
      **************************************************

       PROCEDURE DIVISION
            USING FORM-PATHNAME OVPAID-BUF EXIT-PATHNAME.

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE 09 TO WINDOW-LINES.
           MOVE 31 TO WINDOW-COLUMNS.
           MOVE 14 TO WINDOW-BEGIN-Y.
           MOVE 28 TO WINDOW-BEGIN-X.
           MOVE "WI/OVPAID" TO WINDOW-FORM-NAM.
           MOVE "OVPAID" TO VDU-FORM-NAME.

           PERFORM OPEN-WINDOW.
           DISPLAY OVPAID-SCREEN.

           MOVE OVPAID-FORM-FIELDS TO WR-BUF.
           MOVE OVPAID-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE " " TO VDU-WIMENU-SELECTION.

           PERFORM ALARM.

      * SEND AMOUNT OF PAYOFF REQUIRED:
           MOVE "SNNN062020." TO VDU.
           MOVE OVPAID-TRAMT TO VDUNUM2.
           PERFORM SCREENIO.
      * DETERMINE IF TIS IS FIRST TIME IN WINDOW AND IF NOT DISPLAY INFORMATION
           IF OVPAID-CKAMT NOT = 0
              MOVE "SNNN062010." TO VDU
              MOVE OVPAID-CKAMT TO VDUNUM2
              PERFORM SCREENIO
              PERFORM SEND-OVERPAID.
              PERFORM SCREENIO.

       ENTER-CKAMT.
           MOVE " F7, ENTER CHECK AMOUNT:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN062010." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY
              GO TO END-OVPAID.
           IF NOT ENTER-KEY
              GO TO ENTER-CKAMT.
           MOVE VDUNUM2 TO OVPAID-CKAMT.
           IF OVPAID-CKAMT = 0
              MOVE "SNNN062010." TO VDU
              MOVE OVPAID-TRAMT  TO OVPAID-CKAMT
                                    VDUNUM2
              PERFORM SCREENIO
           ELSE
           IF OVPAID-CKAMT < OVPAID-TRAMT
              MOVE
              "CHECK AMOUNT CAN'T BE LESS THAN THE REQUIRED PAYOFF"
                                                          TO MESS
              PERFORM SEND-MESS
              MOVE 0 TO OVPAID-CKAMT
              GO TO ENTER-CKAMT.
           PERFORM SEND-OVERPAID.

           MOVE " F7-EXIT, ^-BACK:" TO MESS.
           PERFORM SEND-LEGEND.
       ENTER-SEL.
           MOVE "E  A010SEL." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY
              GO TO END-OVPAID.
           IF UP-KEY
              GO TO ENTER-CKAMT.
           IF NOT F7-KEY
              GO TO ENTER-SEL.

       END-OVPAID.
           GO TO END-ROUTINE.

       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/SCREEN.CPY".

      ***************************************
       VDU-CONVERT-FIELD SECTION.
      ***************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/OVPAID_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ********************************************
       MISC SECTION.
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SENDMESS.CPY".
       FORM-RESET.
       SETUP-LINK-INFO.
       ALARM.
           DISPLAY OMITTED WITH BEEP.
       SEND-LEGEND.
           MOVE "WSEL:D." TO WR-LIST.
           MOVE MESS TO WR-BUF.
           PERFORM CALL-WRFORM.
       SEND-OVERPAID.
           MOVE "SNNN062030." TO VDU.
           SUBTRACT OVPAID-TRAMT FROM OVPAID-CKAMT
                                      GIVING OVPAID-CHECK
                                             VDUNUM2.
           PERFORM SCREENIO.
           IF OVPAID-CHECK = 0
              MOVE "THERE WILL BE NO OVERPAY INDICATED" TO MESS
              PERFORM SEND-MESS.

      ****************************************
       IO-ERROR SECTION.
      ****************************************
           MOVE "WINDOW ERROR " TO MESS.
           PERFORM SEND-MESS.
       END-ROUTINE.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY OVPAID-SCREEN.
           PERFORM CLOSE-WINDOW UNTIL WINDOWS-OPEN = 0.
       STOP-RUN.
           EXIT PROGRAM.
