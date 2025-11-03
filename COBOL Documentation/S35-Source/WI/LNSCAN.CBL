       IDENTIFICATION DIVISION.
       PROGRAM-ID.      LNSCAN.
      *****************************************************
      *  BORROWER LOAN WINDOW SCAN
      *         CALLED FROM LONPB0, XONPC0
      *         WHEN 2 PERSONS ON LOAN, FROM PR LOAN #
      *****************************************************

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
           "@(#)WI/LNSCAN A30 07/08/14-11:00:45 10/01/14-09:55:48 >".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".

       01  LNSCAN-LINES          PIC 99  VALUE 8.
       01  LNSCAN-COLUMNS        PIC 99  VALUE 39.
       01  LNSCAN-BEGIN-Y        PIC 99  VALUE 3.
       01  LNSCAN-BEGIN-X        PIC 99  VALUE 40.

       01  ERRCD                       PIC X VALUE " ".
       01  SUB                     PIC 99.
       01  HOLD-SSNO               PIC 9(9).

       01  ALP-BW.
           03  ALP-BW123           PIC X(3).
           03  ALP-HYPHEN1         PIC X.
           03  ALP-BW45            PIC X(2).
           03  ALP-HYPHEN2         PIC X.
           03  ALP-BW6789          PIC X(4).

       01  NUM-BW                  PIC 9(9).
       01  FILLER REDEFINES NUM-BW.
           03  NUM-BW123           PIC X(3).
           03  NUM-BW45            PIC X(2).
           03  NUM-BW6789          PIC X(4).

       01  A-NAME                  PIC X(16).


       01  FLDSEL-STAT             PIC XX.
       01  FLDSEL-FIELD            PIC X(4)   VALUE "010.".
       01  FLDSEL-LIST.
           03  FILLER              PIC X(16)  VALUE
           "010 020 030 040.".
       01  FLDSEL-BUF.
           05  FLDSEL-KEY          PIC X(11).
           05  FILLER              PIC XX.
           05  FLDSEL-NAME         PIC X(22).

       01  FIRST-FIELD             PIC X(4)  VALUE "010.".
       01  LAST-FIELD              PIC X(4)  VALUE "040.".

       01  DISPLAY-LIST.
           03  FILLER              PIC X(51)  VALUE
           "010 020 030 040.".
       01  DISPLAY-BUF.
           03  D-LINE              OCCURS 4 TIMES.
               05  D-KEY           PIC X(11).
               05  FILLER          PIC XX.
               05  D-NAME          PIC X(22).

       01  PAGE-KEY-01.
           03  PAGE-KEY                OCCURS 1 TIMES
                                       PIC X(20).

       01  MORE-LIST.
           03  FILLER     PIC X(39)  VALUE
           "TITLE ADR1 ADR2 ADR3 PHONE EMPL SPOUSE ".
           03  FILLER     PIC X(16)  VALUE
           "LIA OPEN CLOSED.".
       01  MORE-BUF.
           03  M-NAME    PIC X(30).
           03  M-ADR1    PIC X(30).
           03  M-ADR2    PIC X(30).
           03  M-ADR3    PIC X(30).
           03  M-PHONE   PIC X(14).
           03  M-EMPL    PIC X(20).
           03  M-SPOUSE  PIC X(28).
           03  M-LIA     PIC Z,ZZZ,ZZ9.99.
           03  M-OPEN    PIC ZZ.
           03  M-CLOSED  PIC ZZ.

       COPY "LIBGB/PHON400.CPY".
       COPY "LIBWI/LNSCN2W.CPY".


       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       01  LNSCAN-WINDOW-HANDLE    PIC X(10).
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/LNSCAN_DEF.CPY".
       COPY "LIBWI/LNSCN3_DEF.CPY".
       COPY "LIBWI/LNSCAN_WKS.CPY".
       COPY "LIBWI/LNSCN3_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/LNSCANW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/LNSCAN_SCN.CPY".
       COPY "LIBWI/LNSCN3_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME LNSCAN-BUF EXIT-PATHNAME.

      **********************************************
       MAIN-PROGRAM SECTION.
      **********************************************

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
      D    STOP MESS.
           MOVE "XX" TO LNSCAN-STATUS.

           MOVE "LNSCAN" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT  SCREEN LINE LNSCAN-BEGIN-Y * VDU-WIN-L-OFFSET
                   AT  SCREEN COL  LNSCAN-BEGIN-X * VDU-WIN-C-OFFSET
                   SIZE    LNSCAN-COLUMNS + VDU-WIN-W-MOD
                   LINES   LNSCAN-LINES + VDU-WIN-H-MOD
            COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS LNSCAN-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM OPEN-BW1-FILE.
           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE SPACES TO DISPLAY-BUF.
           MOVE 1 TO SUB.

       BUILD-PAGENO.
           IF SUB > 4
              GO TO DISPLAY-PAGE.

           IF LNSCAN-SSNO(SUB) = 0
              GO TO DISPLAY-PAGE.

           MOVE LNSCAN-SSNO(SUB) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
              MOVE 919191919 TO BW-SSNO
              MOVE "**NOT FOUND**" TO BW-LNAME.
           MOVE BW-SSNO TO NUM-BW.
           PERFORM NUMBW-TO-ALPBW.
           MOVE ALP-BW TO D-KEY(SUB).
           MOVE BW-LNAME TO MESS.
           MOVE BW-FNAME TO A-NAME.
           PERFORM SET-BW-LNAME.
           MOVE MESS TO D-NAME(SUB).
           ADD 1 TO SUB.
           GO TO BUILD-PAGENO.

       DISPLAY-PAGE.
           IF SUB = 1
              MOVE "NO RECORDS FOUND!" TO MESS
              PERFORM SEND-LEGEND
              GO TO END-ROUTINE.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       SELECT-FIELD-FROM-DISPLAY.
           MOVE "F7, F6-MORE BORR INFO, (RTN)-LOANS:" TO MESS.
           PERFORM SEND-LEGEND.
           PERFORM CALL-FLDSEL.

           IF FLDSEL-STAT = "1>"
              GO TO END-ROUTINE.
           IF NOT (FLDSEL-STAT = "00" OR "1<")
              PERFORM ALARM
              GO TO SELECT-FIELD-FROM-DISPLAY.

           MOVE FLDSEL-KEY TO ALP-BW.
           PERFORM ALPBW-TO-NUMBW.
           MOVE NUM-BW TO HOLD-SSNO.

           IF FLDSEL-STAT = "00"
              MOVE HOLD-SSNO      TO LNSCN2-KEY
              MOVE LNSCAN-SYSDATE TO LNSCN2-SYSDATE
              MOVE "WI/LNSCN2 " TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH LNSCN2-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF LNSCN2-STATUS NOT = "00" OR LNSCN2-ACCTNO = 0
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE LNSCN2-ACCTNO TO LNSCAN-NO
                 MOVE "00"          TO LNSCAN-STATUS
                 GO TO END-ROUTINE.

           IF FLDSEL-STAT = "1<"
              PERFORM MORE-INFO
              GO TO SELECT-FIELD-FROM-DISPLAY.

           GO TO END-ROUTINE.


      *************************************
       MORE-INFO SECTION.
      *************************************
           MOVE HOLD-SSNO TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
              MOVE "UNABLE TO READ BW1-FILE RECORD" TO MESS
              PERFORM SEND-MESS
              GO TO MORE-INFO-EXIT.

           MOVE 13 TO WINDOW-LINES.
           MOVE 44 TO WINDOW-COLUMNS.
           MOVE 3  TO WINDOW-BEGIN-Y.
           MOVE 3  TO WINDOW-BEGIN-X.
           MOVE FLDSEL-FIELD TO WINDOW-FIT-TO.
           MOVE "LNSCN3"     TO VDU-FORM-NAME.
           MOVE "WI/LNSCN3"  TO WINDOW-FORM-NAM.

           PERFORM OPEN-WINDOW.
           DISPLAY LNSCN3-SCREEN.

           MOVE LNSCN3-FORM-FIELDS TO WR-BUF.
           MOVE LNSCN3-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE BW-FNAME TO MESS.
           MOVE BW-LNAME TO A-NAME.
           PERFORM SET-BW-FNAME.
           MOVE MESS TO M-NAME.
           PERFORM SET-CITY-STATE-ZIP.
           IF BW-ADR2 = " "
              MOVE BW-ADR1 TO M-ADR1
              MOVE MESS TO M-ADR2
              MOVE " " TO M-ADR3
           ELSE
              MOVE BW-ADR1 TO M-ADR1
              MOVE BW-ADR2 TO M-ADR2
              MOVE MESS TO M-ADR3.
           MOVE BW-PHONE TO TEL-BUF.
           PERFORM SEND-PHONE.
           MOVE TEL-EDIT TO M-PHONE.
           MOVE BW-EMPLOYER TO M-EMPL.
           MOVE BW-SPFNAME TO MESS.
           MOVE BW-SPLNAME TO A-NAME.
           PERFORM SET-BW-FNAME.
           MOVE MESS TO M-SPOUSE.
           ADD BW-LIASMLN BW-LIAOTH GIVING M-LIA.
           MOVE BW-LOANS TO M-OPEN.
           MOVE BW-PRLOANS TO M-CLOSED.

           MOVE MORE-BUF TO WR-BUF.
           MOVE MORE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
           MOVE "E  A010CONTINUE." TO VDU.
           PERFORM SCREENIO.

       MORE-INFO-CLOSE-WINDOW.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LNSCN3-SCREEN.
           PERFORM CLOSE-WINDOW.
           MOVE "LNSCAN" TO VDU-FORM-NAME.
       MORE-INFO-EXIT.
           EXIT.

       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".
       COPY "LIBGB/DATER.CPY".

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY LNSCAN-SCREEN UPON LNSCAN-WINDOW-HANDLE.
           MOVE LNSCAN-FORM-FIELDS TO WR-BUF.
           MOVE LNSCAN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE LNSCAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE LNSCAN-HELP--DIR  TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/LNSCAN_EVA.CPY".
              COPY "LIBWI/LNSCN3_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ********************************************
       MISC-PARAGRAPHS SECTION.
      ********************************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-PHONE.
           MOVE TEL-1 TO TEL-ED1.
           MOVE TEL-2 TO TEL-ED2.
           MOVE TEL-3 TO TEL-ED3.
       SET-BW-LNAME.
           INSPECT MESS REPLACING FIRST "  " BY ",?".
           INSPECT MESS REPLACING FIRST "                "
               BY A-NAME.
           INSPECT MESS REPLACING ALL "?" BY " ".
       SET-BW-FNAME.
           INSPECT MESS REPLACING FIRST "  " BY "? ".
           INSPECT MESS REPLACING FIRST "                "
               BY A-NAME.
           INSPECT MESS REPLACING ALL "?" BY " ".
       SET-CITY-STATE-ZIP.
           MOVE BW-CITY TO MESS.
           INSPECT MESS REPLACING FIRST "  " BY ",?".
           INSPECT MESS REPLACING FIRST "  "
               BY BW-STATE.
           INSPECT MESS REPLACING FIRST "  " BY "? ".
           INSPECT MESS REPLACING FIRST "          "
               BY BW-ZIP.
           INSPECT MESS REPLACING ALL "?" BY " ".
       SEND-LEGEND.
           MOVE "S  A350WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       NUMBW-TO-ALPBW.
           MOVE NUM-BW123 TO ALP-BW123.
           MOVE NUM-BW45 TO ALP-BW45.
           MOVE NUM-BW6789 TO ALP-BW6789.
           MOVE "-" TO ALP-HYPHEN1 ALP-HYPHEN2.
       ALPBW-TO-NUMBW.
           MOVE ALP-BW123 TO NUM-BW123.
           MOVE ALP-BW45 TO NUM-BW45.
           MOVE ALP-BW6789 TO NUM-BW6789.
           IF NUM-BW NOT NUMERIC
              MOVE 0 TO NUM-BW.

      ************************************
       IO-ROUTINES SECTION.
      ************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-BW1-FILE.
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPBW1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      ************************************
       END-ROUTINE SECTION.
      ************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY LNSCAN-SCREEN.
           DESTROY LNSCAN-WINDOW-HANDLE.
           EXIT PROGRAM.

