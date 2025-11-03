       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SCHG33.
       DATE-WRITTEN.    990222.
      **************************************************************************
      *  DESC:  OVERRIDE DISTRIBUTION OF SERVICE CHARGE
      *         FOR SP-SCFRMLA 33 AND 39:
      *           - POINTS
      *           - ORIGINATION FEE
      *
      * INPUT:
      *        SCHG33-SP-SCMIN
      *        SCHG33-SP-SCMAX
      *        SCHG33-CUR-POINTS
      *        SCHG33-CUR-ORIGFEE
      *
      * OUTPUT:
      *        SCHG33-RTNCD
      *        SCHG33-CHGTO-POINTS
      *        SCHG33-CHGTO-ORIGFEE
      *
      * REV:
      *  130131 BAH A30 CONVERSION
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
       77  SCCS-IDENTIFIER PIC X(55) VALUE
           "@(#)WI/SCHG33 A30 09/30/13-12:00:45 09/18/14-13:59:18 >".
      **************************************************
      *     P R O G R A M   W O R K E R S              *
      **************************************************

       01  AMT                     PIC S9(7)V99     COMP.
       01  D-AMT                   PIC ZZZZZ9.99-.

       01  ROUTE-BUF               PIC XX           VALUE " ".
       01  EXT-EOBUF-MESS              REDEFINES ROUTE-BUF
                                   PIC XX.

       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       01 SCHG33-WINDOW-HANDLE     PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/SCHG33_DEF.CPY".
       COPY "LIBWI/SCHG33_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/SCHG33W.CPY".

       SCREEN SECTION.
       COPY "LIBWI/SCHG33_SCN.CPY".

      **************************************************
      *   C O N T R O L   M O D U L E                  *
      **************************************************

       PROCEDURE DIVISION
            USING FORM-PATHNAME SCHG33-WORKERS EXIT-PATHNAME.

      **************************************************
      *      P R O G R A M   C O N T R O L             *
      **************************************************
       MAIN-PROGRAM SECTION.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.

           MOVE "SCHG33" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
               AT SCREEN LINE 14 * VDU-WIN-L-OFFSET
               AT SCREEN COL 01 * VDU-WIN-C-OFFSET
               CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
               SIZE 40 + VDU-WIN-W-MOD
               LINES 09 + VDU-WIN-H-MOD
               BLANK SCREEN
               HANDLE IS SCHG33-WINDOW-HANDLE.

       SCHG33-INITIALIZE.

           PERFORM FORM-RESET.

           MOVE 0 TO SCHG33-CHGTO-POINTS
                     SCHG33-CHGTO-ORIGFEE.
           MOVE " " TO SCHG33-RTNCD.

      * SEND CURRENT POINTS AND ORIG. FEES

           MOVE "SNNS062010C."     TO VDU.
           MOVE SCHG33-CUR-POINTS  TO VDUNUM2.
           PERFORM SCREENIO.
           MOVE "SNNS062010."      TO VDU.
           MOVE SCHG33-CUR-POINTS  TO VDUNUM2
                                      SCHG33-CHGTO-POINTS.
           PERFORM SCREENIO.

           MOVE "SNNS062020C."     TO VDU.
           MOVE SCHG33-CUR-ORIGFEE TO VDUNUM2.
           PERFORM SCREENIO.
           MOVE "SNNS062020."      TO VDU.
           MOVE SCHG33-CUR-ORIGFEE TO VDUNUM2
                                      SCHG33-CHGTO-ORIGFEE.
           PERFORM SCREENIO.

           MOVE "SNNS062030C."      TO VDU.
           ADD SCHG33-CUR-POINTS
               SCHG33-CUR-ORIGFEE   GIVING VDUNUM2.
           PERFORM SCREENIO.
           MOVE "SNNS062030."       TO VDU.
           ADD SCHG33-CHGTO-POINTS
               SCHG33-CHGTO-ORIGFEE GIVING VDUNUM2.
           PERFORM SCREENIO.

       SCHG33-ENTER-POINTS.
           MOVE " F7, F1, ENTER $AMT OF POINTS:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNS062010." TO VDU.
           PERFORM SCREENIO.
           IF VDUNUM2 < 0
              GO TO SCHG33-ENTER-POINTS.
           IF VDUSTATUS = "10"
              GO TO SCHG33-INITIALIZE.
           IF VDUSTATUS = "1>"
              PERFORM SCHG33-VERIFY-CHGTO-TOT
              IF IO-FG = 9
                 GO TO SCHG33-ENTER-POINTS
              ELSE
                 GO TO END-SCHG33.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO SCHG33-ENTER-POINTS.
           IF VDUNUM2 > SCHG33-CUR-POINTS
              MOVE "AMOUNT EXCEEDS CURRENT $POINTS" TO MESS
              PERFORM SEND-MESS
              GO TO SCHG33-ENTER-POINTS.
           MOVE VDUNUM2 TO SCHG33-CHGTO-POINTS.

           PERFORM SCHG33-DISPLAY-CHGTO-TOT.

       SCHG33-ENTER-ORIGFEE.
           MOVE " F7, F1, ^, ENTER ORIG. FEE:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "ENNS062020." TO VDU.
           PERFORM SCREENIO.
           IF VDUNUM2 < 0
              GO TO SCHG33-ENTER-ORIGFEE.
           IF VDUSTATUS = "10"
              GO TO SCHG33-INITIALIZE.
           IF VDUSTATUS = "1>"
              PERFORM SCHG33-VERIFY-CHGTO-TOT
              IF IO-FG = 9
                 GO TO SCHG33-ENTER-ORIGFEE
              ELSE
                 GO TO END-SCHG33.
           IF VDUSTATUS = "1U"
              GO TO SCHG33-ENTER-POINTS.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO SCHG33-ENTER-ORIGFEE.
           IF VDUNUM2 > SCHG33-CUR-ORIGFEE
              MOVE "AMOUNT EXCEEDS CURRENT ORIGFEE" TO MESS
              PERFORM SEND-MESS
              GO TO SCHG33-ENTER-ORIGFEE.
           MOVE VDUNUM2 TO SCHG33-CHGTO-ORIGFEE.

           PERFORM SCHG33-DISPLAY-CHGTO-TOT.
           PERFORM SCHG33-VERIFY-CHGTO-TOT.

       SCHG33-ENTER-SEL.
           MOVE " F7-END CHANGES, F1, ^-BACK:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A010SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO SCHG33-INITIALIZE.
           IF VDUSTATUS = "1U"
              GO TO SCHG33-ENTER-ORIGFEE.

           IF VDUSTATUS NOT = "1>"
              GO TO SCHG33-ENTER-SEL.

           PERFORM SCHG33-VERIFY-CHGTO-TOT.
           IF IO-FG = 9
              GO TO SCHG33-ENTER-SEL.

       END-SCHG33.
           GO TO END-ROUTINE.

       COPY "LIBGB/SCREEN.CPY".

      **************************************************************************
       FORM-RESET SECTION.
      **************************************************************************
           DISPLAY SPACES UPON SCHG33-WINDOW-HANDLE
                          LINE 1 COL 1 ERASE SCREEN.
           DISPLAY SCHG33-SCREEN UPON SCHG33-WINDOW-HANDLE.
           MOVE SCHG33-FORM-FIELDS TO WR-BUF.
           MOVE SCHG33-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **************************************************************************
       SETUP-LINK-INFO SECTION.
      **************************************************************************
           MOVE SCHG33-HELP--FILE TO HELP-LINK-FILE.
           MOVE SCHG33-HELP--DIR TO HELP-LINK-DIR.

      **************************************************************************
       VDU-CONVERT-FIELD SECTION.
      **************************************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/SCHG33_EVA.CPY".
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
       SCHG33-DISPLAY-CHGTO-TOT.
           MOVE "SNNS062030."    TO VDU.
           ADD SCHG33-CHGTO-POINTS
               SCHG33-CHGTO-ORIGFEE GIVING VDUNUM2.
           PERFORM SCREENIO.

       SCHG33-VERIFY-CHGTO-TOT.
           MOVE 0 TO IO-FG.
           ADD SCHG33-CHGTO-POINTS
               SCHG33-CHGTO-ORIGFEE GIVING AMT.

           IF AMT > SCHG33-SP-SCMAX
              MOVE SCHG33-SP-SCMAX TO D-AMT
              STRING "COMBINED SERVICE CHARGE EXCEEDS SPR MAXIMUM OF "
                     D-AMT
                     "!                             "
                 DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              MOVE 9 TO IO-FG
           ELSE
           IF AMT < SCHG33-SP-SCMIN
              MOVE SCHG33-SP-SCMIN TO D-AMT
              STRING "COMBINED SERVICE CHARGE IS BELOW SPR MINIMUM OF "
                     D-AMT
                     "!                             "
                 DELIMITED BY SIZE INTO MESS
              PERFORM SEND-MESS
              MOVE 9 TO IO-FG.

      *****************************
       IO-ERROR SECTION.
      *****************************
           MOVE "WINDOW ERROR " TO MESS.
           PERFORM SEND-MESS.

      *****************************
       END-ROUTINE SECTION.
      *****************************
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY SCHG33-SCREEN.
           DESTROY SCHG33-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.
