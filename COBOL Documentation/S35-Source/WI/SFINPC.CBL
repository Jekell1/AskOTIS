       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SFINPC.
      *************************************************************
      *  SALES FINANCE CALCULATOR CALCULATE/FREEZE OPTION WINDOW
      *   ** NOTE FORM IS DIFF THAN A15
      *
      * REV:
      *  JTG 032498 ADDED SFINPC-ORGTERM TO CAPTURE SINGLE PAY LOAN
      *  BLM 140507 CHANGE TO DISPLAY MESSAGE BOX FOR SEND-MESS
      *             INSTEAD OF CALLING GB/SENDME, VERYANT
      *
      * JKC 2024-0501 ADDED USE OF EXT-API-SCREEN-DISABLE AND
      *         EXT-API-MESS FOR SUPPRESS SCREEN INTERACTIONS FOR
      *         USE IN API PROGRAMS WITHOUT SCREENS.
      *
      *************************************************************
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
           "@(#)WI/SFINPC.CBL S35 05/02/24-08:48:36 >".

       01  SFINPC-LINES                PIC 99  VALUE 8.
       01  SFINPC-COLUMNS              PIC 99  VALUE 33.
       01  SFINPC-BEGIN-Y              PIC 99  VALUE 04.
       01  SFINPC-BEGIN-X              PIC 99  VALUE 26.
       01  SUB                     PIC 9.
       01  FLDSEL-FIELD            PIC X(4)  VALUE "010.".
       01  FLDSEL-STAT             PIC XX.
       01  FLDSEL-LIST.
           03  FILLER              PIC X(20) VALUE
           "010 020 030 040 050.".
       01  FLDSEL-BUF.
           05  FLDSEL-KEY          PIC XX.
           05  FLDSEL-NAME         PIC X(27).

       01  FIRST-FIELD             PIC X(4)  VALUE "010.".
       01  LAST-FIELD              PIC X(4)  VALUE "050.".

       01  DISPLAY-LIST.
           03  FILLER              PIC X(20) VALUE
           "010 020 030 040 050.".
       01  DISPLAY-BUF.
           03  D-LINE              OCCURS 5 TIMES.
               05  D-KEY           PIC XX.
               05  FILLER          PIC X(14).
               05  D-AMOUNT        PIC Z(6)9.99-.
               05  FILLER          PIC X.
               05  D-FG            PIC X.


       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       01 SFINPC-WINDOW-HANDLE     PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/SFINPC_DEF.CPY".
       COPY "LIBWI/SFINPC_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/SFINPCW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/SFINPC_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME SFINPC-BUF EXIT-PATHNAME.

      ****************************
      *    MAIN PROGRAM MODULE
      ****************************
       MAIN-PROGRAM SECTION.

       COPY "LIBGB/CHKSEC.CPY".

      D    STOP MESS.
           MOVE "XX" TO SFINPC-STATUS.

           MOVE "SFINPC" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
               AT SCREEN LINE 10 * VDU-WIN-L-OFFSET
               AT SCREEN COL 09 * VDU-WIN-C-OFFSET
               CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
               SIZE 33 + VDU-WIN-W-MOD
               LINES 09 + VDU-WIN-H-MOD
               BLANK SCREEN
               HANDLE IS SFINPC-WINDOW-HANDLE.

           PERFORM FORM-RESET.

       BEGIN-BUILD-PAGENO.
           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE SPACES TO DISPLAY-BUF.

           MOVE 1 TO SUB.
           IF SFINPC-SELLPR-FG NOT = " "
              MOVE "SELLING PRICE  :" TO D-LINE(SUB)
              MOVE SFINPC-SELLPR TO D-AMOUNT(SUB)
              ADD 1 TO SUB.

           IF SFINPC-DOWNPY-FG NOT = " "
              MOVE "DOWN PAYMENT   :" TO D-LINE(SUB)
              MOVE SFINPC-DOWNPY TO D-AMOUNT(SUB)
              ADD 1 TO SUB.

           IF SFINPC-PERDOWN-FG NOT = " "
              MOVE "%PERCENT DOWN  :" TO D-LINE(SUB)
              MOVE SFINPC-PERDOWN TO D-AMOUNT(SUB)
              INSPECT D-AMOUNT(SUB) REPLACING TRAILING " " BY "%"
              ADD 1 TO SUB.

           IF SFINPC-BALLOON-FG NOT = " "
              IF SFINPC-BALLOON-LAST-FG = "L"
                 MOVE "LAST PAYMENT   :" TO D-LINE(SUB)
                 MOVE SFINPC-LASTPYAMT TO D-AMOUNT(SUB)
                 ADD 1 TO SUB
              ELSE
                 MOVE "BALLOON AMOUNT :" TO D-LINE(SUB)
                 IF SFINPC-ORGTERM = 1
                    MOVE 0 TO D-AMOUNT(SUB)
                 ELSE
                    SUBTRACT SFINPC-REGPYAMT FROM SFINPC-LASTPYAMT
                                          GIVING D-AMOUNT(SUB)
                 END-IF
                 ADD 1 TO SUB.

           IF SFINPC-REGPYAMT-FG NOT = " "
              MOVE "REGULAR PAYMENT:" TO D-LINE(SUB)
              MOVE SFINPC-REGPYAMT TO D-AMOUNT(SUB)
              ADD 1 TO SUB.

           MOVE DISPLAY-BUF TO WR-BUF.
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
              GO TO END-ROUTINE.

           IF FLDSEL-STAT NOT = "00"
              PERFORM ALARM
              GO TO SELECT-FIELD-FROM-DISPLAY.

           MOVE FLDSEL-KEY TO SFINPC-CALC-SELECT-KEY.
           MOVE "00"       TO SFINPC-STATUS.

           GO TO END-ROUTINE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY SFINPC-SCREEN UPON SFINPC-WINDOW-HANDLE.
           MOVE SFINPC-FORM-FIELDS TO WR-BUF.
           MOVE SFINPC-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE SFINPC-HELP--FILE TO HELP-LINK-FILE.
           MOVE SFINPC-HELP--DIR  TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/SFINPC_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ********************************************
       MISC-PARAGRAPHS SECTION.
      ********************************************
       COPY "LIBGB/ACCESS.CPY".
       ALARM.
           DISPLAY OMITTED WITH BEEP.
       SEND-LEGEND.
           MOVE "S  A600WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       SEND-MESS.
           IF ( EXT-API-SCREEN-DISABLE = "Y" )
              MOVE MESS TO EXT-API-MESS
           ELSE
              PERFORM ALARM
              DISPLAY MESSAGE BOX MESS.

      ********************************************
       END-ROUTINE SECTION.
      ********************************************
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY SFINPC-SCREEN.
           DESTROY SFINPC-WINDOW-HANDLE.
           EXIT PROGRAM.

