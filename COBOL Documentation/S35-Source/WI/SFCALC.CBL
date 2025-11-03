       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SFCALC.
      ****************************************************************
      *  SALES FINANCE CALCULATOR CALCULATE (PROCEEDS) OPTION WINDOW
      ****************************************************************
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
           "@(#)WI/SFCALC S35 05/03/24-14:07:26 >".

       01  SFCALC-LINES                PIC 99  VALUE 6.
       01  SFCALC-COLUMNS              PIC 99  VALUE 33.
       01  SFCALC-BEGIN-Y              PIC 99  VALUE 20.
       01  SFCALC-BEGIN-X              PIC 99  VALUE 8.
       01  SUB                     PIC 9.
       01  FLDSEL-FIELD            PIC X(4)  VALUE "010.".
       01  FLDSEL-STAT             PIC XX.
       01  FLDSEL-LIST.
           03  FILLER              PIC X(8) VALUE
           "010 020.".
       01  FLDSEL-BUF.
           03  FLDSEL-KEY          PIC X.

       01  FIRST-FIELD             PIC X(4)  VALUE "010.".
       01  LAST-FIELD              PIC X(4)  VALUE "020.".

       01  DISPLAY-LIST.
           03  FILLER              PIC X(8) VALUE
           "010 020.".
       01  DISPLAY-BUF.
           03  D-LINE              OCCURS 2 TIMES.
               05  D-DESC          PIC X(16).
               05  FILLER          PIC X(4).
               05  D-AMOUNT        PIC Z(4)9.99-.


       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/GETENVW.CPY".
       01  SFCALC-WINDOW-HANDLE      PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/SFCALC_DEF.CPY".
       COPY "LIBWI/SFCALC_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/SFCALCW.CPY".
        
       SCREEN SECTION.
       COPY "LIBWI/SFCALC_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME SFCALC-BUF EXIT-PATHNAME.

      ****************************
      *    MAIN PROGRAM MODULE
      ****************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

       MAIN-MODULE.
      D    STOP MESS.
           MOVE "00" TO SFCALC-STATUS.
           MOVE "SFCALC" TO VDU-FORM-NAME.
           MOVE " "      TO VDU-WIMENU-SELECTION.
           MOVE " "      TO VDU-CLMENU-SELECTION.
           MOVE FORM-PATHNAME TO FORM-PATH.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT LINE SFCALC-BEGIN-X 
                   AT COL  SFCALC-BEGIN-Y
                   SIZE    SFCALC-COLUMNS + VDU-WIN-W-MOD
                   LINES   SFCALC-LINES + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE IS SFCALC-WINDOW-HANDLE.
           
           PERFORM FORM-RESET.

       BEGIN-BUILD-PAGENO.
           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE SPACES TO DISPLAY-BUF.
      D    STOP MESS.

           MOVE 1 TO SUB.
           IF SFCALC-SELLPR-FG NOT = " "
              MOVE "SELLING PRICE  :" TO D-DESC(SUB)
              MOVE SFCALC-SELLPR TO D-AMOUNT(SUB)
              ADD 1 TO SUB.

           IF SFCALC-DOWNPY-FG NOT = " "
              MOVE "DOWN PAYMENT   :" TO D-DESC(SUB)
              MOVE SFCALC-DOWNPY TO D-AMOUNT(SUB)
              ADD 1 TO SUB.

           IF SFCALC-PERDOWN-FG NOT = " "
              MOVE "%PERCENT DOWN  :" TO D-DESC(SUB)
              MOVE SFCALC-PERDOWN     TO D-AMOUNT(SUB)
              INSPECT D-AMOUNT(SUB) REPLACING TRAILING " " BY "%"
              ADD 1 TO SUB.

           MOVE DISPLAY-BUF  TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


       SELECT-FIELD-FROM-DISPLAY.
           MOVE "F7-EXIT, SELECT OPTION(RTN):" TO MESS. 
           PERFORM SEND-LEGEND.

           PERFORM CALL-FLDSEL.

      * TEST UP ARROW:
           IF FLDSEL-STAT = "1U"
              PERFORM ALARM
              GO TO SELECT-FIELD-FROM-DISPLAY.

      * TEST PAGE UP:
           IF FLDSEL-STAT = "1P"
              PERFORM ALARM
              GO TO SELECT-FIELD-FROM-DISPLAY.

      * TEST PAGE DOWN OR DOWN ARROW:
           IF FLDSEL-STAT = "1N" OR FLDSEL-STAT = "1D"
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY.

           IF FLDSEL-STAT = "1>" 
              MOVE "XX" TO SFCALC-STATUS
              GO TO END-ROUTINE.

           IF FLDSEL-STAT NOT = "00"
              PERFORM ALARM
              GO TO SELECT-FIELD-FROM-DISPLAY.

           MOVE FLDSEL-KEY TO SFCALC-SELECT-KEY.
           MOVE "00"       TO SFCALC-STATUS.

           GO TO END-ROUTINE.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".

      ***********************************************
       FORM-RESET SECTION.
      ***********************************************

           DISPLAY SFCALC-SCREEN UPON SFCALC-WINDOW-HANDLE.
           MOVE SFCALC-FORM-FIELDS TO WR-BUF.
           MOVE SFCALC-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ***********************************************
       SETUP-LINK-INFO SECTION.
      ***********************************************
           MOVE SFCALC-HELP--FILE TO HELP-LINK-FILE.
           MOVE SFCALC-HELP--DIR TO HELP-LINK-DIR.

      ***********************************************
       VDU-CONVERT-FIELD SECTION.
      ***********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/SFCALC_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.
      **************************************
       MISC-PARAGRAPHS SECTION.
      **************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/CONNECT_SQL.CPY".
       SEND-LEGEND.
           MOVE "S  A280WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       ALARM.
      *    CALL ALARMS USING STAT.
       SEND-MESS.
           IF ( EXT-API-SCREEN-DISABLE = "Y" )
              MOVE MESS TO EXT-API-MESS
           ELSE
              PERFORM ALARM
              DISPLAY MESSAGE BOX MESS.

       END-ROUTINE SECTION.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY SFCALC-SCREEN.
           DESTROY SFCALC-WINDOW-HANDLE.
           EXIT PROGRAM.

