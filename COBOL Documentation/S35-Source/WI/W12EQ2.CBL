       IDENTIFICATION DIVISION.
       PROGRAM-ID.      W12EQ2.
       DATE-WRITTEN.    030430.
      **************************************************************************
      *  DESC:  DISPLAY FIRST PAYMENT EXTENSION CHARGE FORMULA 12,
      *         COMPUTED LIKE FORMULA 2 AND SPREAD EVENLY OVER ALL PAYMENTS
      *
      * REV:
      *  JTG 030430 *NEW* CREATED FORMULA 12 LIKE 2 BUT SPREADS EXTENSION CHARGE
      *             OVER PAYMENTS  88  SP-PYEXFRMLA-SAME2-SPREAD VALUE 12.
      *                                                           CITIZENS #2006
      *  BAH 130131 A30 CONVERSION
      *   CS 150519 FIXED WINDOW LINES/COLUMNS FOR WI/W12EQ2, WASN'T DISPLAYING
      *             BECAUSE LINE WAS 37
      **************************************************************************
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
           "@(#)./WI/W12EQ2.CBL A30 05/19/15-11:41:19 >".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************

       01  ROUTE-BUF               PIC XX           VALUE " ".
       01  EXT-EOBUF-MESS              REDEFINES ROUTE-BUF
                                   PIC XX.
      *************************************
      *   SCREEN DISPLAY BUFFER
      *************************************
       01  DISPLAY-BUF           VALUE SPACES.
           03  D-010             PIC ZZ9.999.
           03  D-011             PIC X.
           03  D-020             PIC ZZ9.999.
           03  D-030             PIC ZZ9.999.
           03  D-040             PIC ZZZZZ9.99   BLANK ZERO.
           03  D-050             PIC ZZZZZ9.99   BLANK ZERO.
           03  D-060             PIC ZZZZZ9.99   BLANK ZERO.
           03  D-070             PIC ZZZZZ9.99   BLANK ZERO.
           03  D-080             PIC ZZZZZ9.99   BLANK ZERO.

       01  DISPLAY-LIST.
           03  FILLER      PIC X(36) VALUE
           "010 011 020 030 040 050 060 070 080.".

       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       01 W12EQ2-WINDOW-HANDLE     PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/W12EQ2_DEF.CPY".
       COPY "LIBWI/W12EQ2_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/W12EQ2W.CPY".

       SCREEN SECTION.
       COPY "LIBWI/W12EQ2_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME W12EQ2-BUF EXIT-PATHNAME.

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "W12EQ2" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                 AT SCREEN LINE 5 * VDU-WIN-L-OFFSET
                 AT SCREEN COL 37 * VDU-WIN-C-OFFSET
                 CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                 SIZE 43 + VDU-WIN-W-MOD
                 LINES 09 + VDU-WIN-H-MOD
                 BLANK SCREEN
                 HANDLE IS W12EQ2-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           MOVE W12EQ2-OVRDRATE    TO D-010.
           MOVE W12EQ2-OVRDRATE-FG TO D-011.
           MOVE W12EQ2-EFFRATE     TO D-020.
           MOVE W12EQ2-APRATE      TO D-030.
           MOVE W12EQ2-EXTCHG      TO D-040.
           MOVE W12EQ2-SPREAD      TO D-050.
           MOVE W12EQ2-1STPYAMT    TO D-060.
           MOVE W12EQ2-REGPYAMT    TO D-070.
           MOVE W12EQ2-LASTPYAMT   TO D-080.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       ENTER-WSELX.
           MOVE "R RA010WSELX." TO VDU.
           PERFORM SCREENIO.

           GO TO END-ROUTINE.

       COPY "LIBGB/SCREEN.CPY".

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY SPACES UPON W12EQ2-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.
           DISPLAY W12EQ2-SCREEN UPON W12EQ2-WINDOW-HANDLE.
           MOVE W12EQ2-FORM-FIELDS TO WR-BUF.
           MOVE W12EQ2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE W12EQ2-HELP--FILE TO HELP-LINK-FILE.
           MOVE W12EQ2-HELP--DIR TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/W12EQ2_EVA.CPY".
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
           DESTROY W12EQ2-SCREEN.
           DESTROY W12EQ2-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
