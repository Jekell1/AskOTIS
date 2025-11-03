       IDENTIFICATION DIVISION.
       PROGRAM-ID.      ORIGRB.
       DATE-WRITTEN.    080392.
      *******************************************************
      *  DESC:  REBATE ORIGRB WINDOW
      *         CALLED FROM REBL01
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)CL/ORIGRB.CBL S34 10/06/21-11:37:11 >".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************

       01  ROUTE-BUF               PIC XX  VALUE " ".
       01  EXT-EOBUF-MESS              REDEFINES ROUTE-BUF
                                   PIC XX.

       01  DISPLAY-INTCHG          PIC ZZZZZ9.99.

       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/ORIGRB_DEF.CPY".
       COPY "LIBWI/ORIGRB_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".


       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBCL/ORIGRBW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/ORIGRB_SCN.CPY".

      **************************************************
      *   C O N T R O L   M O D U L E                  *
      **************************************************

       PROCEDURE DIVISION
            USING FORM-PATHNAME ORIGRB-BUF EXIT-PATHNAME.

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE 9           TO WINDOW-LINES.
           MOVE 45          TO WINDOW-COLUMNS.
           MOVE 5           TO WINDOW-BEGIN-Y.
           MOVE 18          TO WINDOW-BEGIN-X.
           MOVE "WI/ORIGRB" TO WINDOW-FORM-NAM.

           MOVE "ORIGRB" TO VDU-FORM-NAME.

           PERFORM OPEN-WINDOW.
           DISPLAY ORIGRB-SCREEN.

           MOVE ORIGRB-FORM-FIELDS TO WR-BUF.
           MOVE ORIGRB-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "SNNN060ACCT." TO VDU.
           MOVE ORIGRB-ACCTNO TO VDUNUM0.
           PERFORM SCREENIO.

           PERFORM ALARM.
       ORIGRB-ENTER-REBATE.
           MOVE " ENTER ORIGINAL REFUND:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN062010." TO VDU.
           PERFORM SCREENIO.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ORIGRB-ENTER-REBATE.
           IF VDUNUM2 < 0
              GO TO ORIGRB-ENTER-REBATE.
           IF VDUNUM2 > ORIGRB-INTCHG
              MOVE ORIGRB-INTCHG TO DISPLAY-INTCHG
              STRING
                "THIS EXCEEDS THE ORIGINAL INTEREST CHARGE OF"
                   DISPLAY-INTCHG
                    "??"
                      DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              GO TO ORIGRB-ENTER-REBATE.
           MOVE VDUNUM2 TO ORIGRB-INT-REBATE.

       ORIGRB-ACCEPT-REBATE.
           MOVE " ARE YOU SURE THIS IS ----:" TO MESS.
           PERFORM SEND-MESS.
           STRING "THE INTEREST REFUNDED ON ACCOUNT " ORIGRB-ACCTNO
                  DELIMITED BY SIZE INTO MESS.
           PERFORM SEND-MESS.

           MOVE " ENTER 'YES' TO CONTINUE:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E RA030SELW." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1U"
              GO TO ORIGRB-ENTER-REBATE.
           IF VDUSTATUS NOT = "00"
              GO TO ORIGRB-ACCEPT-REBATE.
           IF VDUBUF NOT = "YES"
              GO TO ORIGRB-ACCEPT-REBATE.

       END-ORIGRB.
           GO TO END-ROUTINE.

       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/SCREEN.CPY".

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/ORIGRB_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ********************************************
       MISC SECTION.
      ********************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/SENDMESS.CPY".
       FORM-RESET.
       SETUP-LINK-INFO.
       ALARM.
           DISPLAY OMITTED WITH BEEP.
       SEND-LEGEND.
           MOVE "WSEL:D." TO WR-LIST.
           MOVE MESS TO WR-BUF.
           PERFORM CALL-WRFORM.

      **********************************************
       IO-ERROR SECTION.
      **********************************************
           MOVE "WINDOW ERROR " TO MESS.
           PERFORM SEND-MESS.

       END-ROUTINE.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY ORIGRB-SCREEN.
           PERFORM CLOSE-WINDOW UNTIL WINDOWS-OPEN = 0.
       STOP-RUN.
           EXIT PROGRAM.
