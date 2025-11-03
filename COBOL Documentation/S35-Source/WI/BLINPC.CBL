       IDENTIFICATION DIVISION.
       PROGRAM-ID.      BLINPC.
      *************************************************************
      *  SALES FINANCE CALCULATOR CALCULATE/FREEZE OPTION WINDOW
      *    CALLED FROM LP/ZONPC0.C, WI/BLQUOT.C, WI/NMQUOT.C,
      * REV:
      * BLM 140507 CHANGE TO DISPLAY MESSAGE BOX FOR SEND-MESS
      *            INSTEAD OF CALLING GB/SENDME, VERYANT
      * JKC 2024-0501 ADDED USE OF EXT-API-SCREEN-DISABLE AND
      *         EXT-API-MESS FOR SUPPRESS SCREEN INTERACTIONS FOR
      *         USE IN API PROGRAMS WITHOUT SCREENS.
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

      *********************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)WI/BLINPC.CBL S35 05/02/24-08:49:05 >".

       01  SUB                     PIC 9.
       01  FLDSEL-FIELD            PIC X(4)  VALUE "010.".
       01  FLDSEL-STAT             PIC XX.
       01  FLDSEL-LIST.
           03  FILLER              PIC X(8)  VALUE
           "010 020.".
       01  FLDSEL-BUF.
           05  FLDSEL-KEY          PIC XX.
           05  FLDSEL-NAME         PIC X(27).

       01  FIRST-FIELD             PIC X(4)  VALUE "010.".
       01  LAST-FIELD              PIC X(4)  VALUE "020.".

       01  DISPLAY-LIST.
           03  FILLER              PIC X(20) VALUE
           "010 011 020 021.".
       01  DISPLAY-BUF.
           03  D-LINE              OCCURS 2 TIMES.
               05  D-KEY           PIC XX.
               05  FILLER          PIC X(14).
               05  D-AMOUNT        PIC Z(6)9.99-.
               05  FILLER          PIC X.
               05  D-FG            PIC X.

       01  BLINPC-LINES                PIC 99  VALUE 7.
       01  BLINPC-COLUMNS              PIC 99  VALUE 33.
       01  BLINPC-BEGIN-Y              PIC 99  VALUE 04.
       01  BLINPC-BEGIN-X              PIC 99  VALUE 26.

       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       01 BLINPC-WINDOW-HANDLE     PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/BLINPC_DEF.CPY".
       COPY "LIBWI/BLINPC_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/BLINPCW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/BLINPC_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME BLINPC-BUF EXIT-PATHNAME.

      ****************************
       MAIN-PROGRAM SECTION.
      ****************************
       COPY "LIBGB/CHKSEC.CPY".
       MAIN-MODULE.
           MOVE "XX" TO BLINPC-STATUS.

           MOVE "BLINPC" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT SCREEN LINE 10 * VDU-WIN-L-OFFSET
                   AT SCREEN COL 09 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   SIZE 33 + VDU-WIN-W-MOD
                   LINES 09 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS BLINPC-WINDOW-HANDLE.

           PERFORM FORM-RESET.

       BEGIN-BUILD-PAGENO.
           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE SPACES TO DISPLAY-BUF.

           MOVE 1 TO SUB.
           IF BLINPC-SELLPR-FG NOT = " "
              MOVE "PROCEEDS OF LOAN:" TO D-LINE(SUB)
              MOVE BLINPC-SELLPR TO D-AMOUNT(SUB)
              ADD 1 TO SUB.

           IF BLINPC-BALLOON-FG NOT = " "
              IF BLINPC-BALLOON-LAST-FG = "L"
                 MOVE "LAST PAYMENT   :" TO D-LINE(SUB)
                 MOVE BLINPC-LASTPYAMT TO D-AMOUNT(SUB)
                 ADD 1 TO SUB
              ELSE
                 MOVE "BALLOON AMOUNT :" TO D-LINE(SUB)
                 SUBTRACT BLINPC-REGPYAMT FROM BLINPC-LASTPYAMT
                                          GIVING D-AMOUNT(SUB)
                 ADD 1 TO SUB.

           IF BLINPC-REGPYAMT-FG NOT = " "
              MOVE "REGULAR PAYMENT:" TO D-LINE(SUB)
              MOVE BLINPC-REGPYAMT TO D-AMOUNT(SUB)
              ADD 1 TO SUB.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *************************************
      *    SELECT FIELD
      *************************************
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

           MOVE FLDSEL-KEY TO BLINPC-CALC-SELECT-KEY.
           MOVE "00"       TO BLINPC-STATUS.

           GO TO END-ROUTINE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY BLINPC-SCREEN UPON BLINPC-WINDOW-HANDLE.
           MOVE BLINPC-FORM-FIELDS TO WR-BUF.
           MOVE BLINPC-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE BLINPC-HELP--FILE TO HELP-LINK-FILE.
           MOVE BLINPC-HELP--DIR  TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/BLINPC_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************
       MISC-PARAGRAPHS SECTION.
      **************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-LEGEND.
           MOVE "S  A600WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       ALARM.
           DISPLAY OMITTED WITH BEEP.
       SEND-MESS.
           IF ( EXT-API-SCREEN-DISABLE = "Y" )
              MOVE MESS TO EXT-API-MESS
           ELSE
              PERFORM ALARM
              DISPLAY MESSAGE BOX MESS.

      **************************
       END-ROUTINE SECTION.
      **************************
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY BLINPC-SCREEN.
           DESTROY BLINPC-WINDOW-HANDLE.
           EXIT PROGRAM.
