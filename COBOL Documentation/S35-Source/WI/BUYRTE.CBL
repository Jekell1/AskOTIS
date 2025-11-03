       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BUYRTE.
       DATE-WRITTEN.    080392.
      *************************************************************************
      *  DESC:  BUY RATE ENTRY WINDOW
      *         CALLED IN LP/LONPB0, WI/SFQUOT
      *
      *REV:
      * JTG 030618 **NEW**
      *            ADDED LOGIC FOR IB SALES FINANCE AMORTIZATED PARTICIPATION
      *            RESERVE PAID EXPENSE, DIFFERENCE BETWEEN A BUY RATE
      *            AND THE CONTRACT RATE                WISCONSIN FINANCE #2049
      * BAH 130131 A30 CONVERSION
      *************************************************************************
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
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./WI/BUYRTE.CBL A30 02/09/15-12:40:27 >".

       01  ROUTE-BUF               PIC XX  VALUE " ".
       01  EXT-EOBUF-MESS       REDEFINES ROUTE-BUF
                                   PIC XX.

       01  DISPLAY-BUF           VALUE SPACES.
           03  D-010             PIC ZZ9.999.

       01  DISPLAY-LIST.
           03  FILLER            PIC X(4) VALUE "010.".

       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       01 BUYRTE-WINDOW-HANDLE     PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/BUYRTE_DEF.CPY".
       COPY "LIBWI/BUYRTE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/BUYRTEW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/BUYRTE_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME BUYRTE-BUF EXIT-PATHNAME.

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.
           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE "BUYRTE" TO VDU-FORM-NAME.

              DISPLAY FLOATING GRAPHICAL WINDOW
                      AT SCREEN LINE 05 * VDU-WIN-L-OFFSET
                      AT SCREEN COL 05 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                      SIZE 29 + VDU-WIN-W-MOD
                      LINES 06 + VDU-WIN-H-MOD
                      BLANK SCREEN
                      HANDLE IS BUYRTE-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE BUYRTE-BUYRATE     TO D-010.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM..

       ENTER-BUYRATE.
           MOVE " F7, ENTER BUY RATE:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNN033010." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY
              GO TO END-ENTER-BUYRATE.
           IF NOT ENTER-KEY
              GO TO ENTER-BUYRATE.
           IF VDUNUM3 = 0
              GO TO ENTER-BUYRATE.
           MOVE VDUNUM3 TO BUYRTE-BUYRATE.


           MOVE " F7-EXIT, ^-BACK:" TO MESS.
           PERFORM SEND-LEGEND.
       ENTER-SEL.
           MOVE "E  A010WSELX." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY
              GO TO END-ENTER-BUYRATE.
           IF UP-KEY
              GO TO ENTER-BUYRATE.
           IF NOT F7-KEY
              GO TO ENTER-SEL.

       END-ENTER-BUYRATE.
           GO TO END-ROUTINE.

       COPY "LIBGB/SCREEN.CPY".

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           DISPLAY SPACES UPON BUYRTE-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.
           DISPLAY BUYRTE-SCREEN UPON BUYRTE-WINDOW-HANDLE.
           MOVE BUYRTE-FORM-FIELDS TO WR-BUF.
           MOVE BUYRTE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE BUYRTE-HELP--FILE TO HELP-LINK-FILE.
           MOVE BUYRTE-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/BUYRTE_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ********************************************
       MISC SECTION.
      ********************************************
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SENDMESS.CPY".
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
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BUYRTE-SCREEN.
           DESTROY BUYRTE-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
