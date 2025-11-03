       IDENTIFICATION DIVISION.
       PROGRAM-ID.      DLSCAN.
      *****************************************************
      *  DEALER FILE WINDOW SCAN
      *  REV:
      *  BLV 041122 INCREASE MAX PAGES & KEY TABLE FROM 50 TO 150
      *   CS 141110 CHANGED DISPLAY OF WINDOW TO USE AT SCREEN LINE BEGIN-X
      *             AND BEGIN-Y, DEALER SCAN CALLED FROM LONPF1 AND PLACEMENT 
      *             IS REALATIVE TO WI/DLRVNO FORM. 
      *   CS 160225 CHANGED DL-BRANCH TO DL-BRNO
      *  BAH 181012 GLOBAL DLFILE
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)WI/DLSCAN.CBL S35 02/22/24-08:41:16 >".
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".

       01  DLSCAN-LINES          PIC 99  VALUE 16.
       01  DLSCAN-COLUMNS        PIC 99  VALUE 39.
       01  DLSCAN-BEGIN-Y        PIC 99  VALUE 06.
       01  DLSCAN-BEGIN-X        PIC 99  VALUE 39.
       01  ERRCD                   PIC X      VALUE SPACES.
       01  ONE                     PIC S9     VALUE 1.
       01  SUB                     PIC 99.

       01  ALP-DL.
           03  ALP-DL1234          PIC X(4).
           03  ALP-PERIOD          PIC X.
           03  ALP-DL56            PIC X(2).

       01  NUM-DL                  PIC 9(6).
       01  FILLER REDEFINES NUM-DL.
           03  NUM-DL1234          PIC X(4).
           03  NUM-DL56            PIC X(2).

       01  FLDSEL-STAT             PIC XX.
       01  FLDSEL-FIELD            PIC X(4)   VALUE "010.".
       01  FLDSEL-LIST.
           03  FILLER              PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080 090.".
       01  FLDSEL-BUF.
           05  FLDSEL-KEY          PIC X(7).
           05  FILLER              PIC XX.
           05  FLDSEL-NAME         PIC X(22).

       01  FIRST-FIELD             PIC X(4)  VALUE "010.".
       01  LAST-FIELD              PIC X(4)  VALUE "090.".

       01  DISPLAY-LIST.
           03  FILLER              PIC X(51)  VALUE
           "010 020 030 040 050 060 070 080 090 PGNO ENDMORE:F.".
       01  DISPLAY-BUF.
           03  D-LINE              OCCURS 9 TIMES.
               05  D-KEY           PIC X(7).
               05  FILLER          PIC XX.
               05  D-NAME          PIC X(26).
           03  D-PAGENO            PIC ZZ9.
           03  D-END-MORE          PIC X(7).

       01  CURRENT-PAGE            PIC S999.
       01  PAGE-COUNT              PIC 999.
       01  MAX-PAGES               PIC 999    VALUE 150.
       01  PAGE-KEY-01.
           03  PAGE-KEY                OCCURS 150 TIMES
                                       PIC X(20).

       01  MORE-LIST.
           03  FILLER     PIC X(37)  VALUE
           "TITLE ADR1 ADR2 ADR3 PHONE SCAN ORIG ".
           03  FILLER     PIC X(8)  VALUE
           "TERM BR.".
       01  MORE-BUF.
           03  M-NAME    PIC X(30).
           03  M-ADR1    PIC X(30).
           03  M-ADR2    PIC X(30).
           03  M-ADR3    PIC X(30).
           03  M-PHONE   PIC X(14).
           03  M-SCAN    PIC X(20).
           03  M-ORIG    PIC X(8).
           03  M-TERM    PIC X(8).
           03  M-BR      PIC Z(4).

       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/PHON400.CPY".
       COPY "LIBWI/PFSWINDOWSW.CPY".

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/DLSCAN_DEF.CPY".
       COPY "LIBWI/DLSCN2_DEF.CPY".
       COPY "LIBWI/DLSCAN_WKS.CPY".
       COPY "LIBWI/DLSCN2_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       01  DLSCAN-WINDOW-HANDLE    PIC X(10).

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/DLSCANW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/DLSCAN_SCN.CPY".
       COPY "LIBWI/DLSCN2_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME DLSCAN-BUF EXIT-PATHNAME.

      ********************************
       MAIN-PROGRAM SECTION.
      ********************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

           MOVE "XX" TO DLSCAN-STATUS.
           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE "DLSCAN" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT SCREEN LINE DLSCAN-BEGIN-Y
                   AT SCREEN COL  DLSCAN-BEGIN-X
                   SIZE    DLSCAN-COLUMNS + VDU-WIN-W-MOD
                   LINES   DLSCAN-LINES + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS DLSCAN-WINDOW-HANDLE.

           PERFORM FORM-RESET.

       ENTER-SCANKEY.
           MOVE "ENTER DEALER NAME:" TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A200SORTKEY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10"
              GO TO END-ROUTINE.
           IF VDUSTATUS = "1>"
              GO TO END-ROUTINE.
           IF VDUSTATUS NOT = "00"
              GO TO ENTER-SCANKEY.

       ENTER-SCANKEY-CONT.
           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1 TO PAGE-COUNT ONE.
           MOVE 0 TO CURRENT-PAGE.
           PERFORM OPEN-DL2-FILE.
           MOVE VDUBUF TO PAGE-KEY(PAGE-COUNT).
       START-BUILD-PAGENO.
           ADD ONE TO CURRENT-PAGE.
           IF CURRENT-PAGE < 1
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.
           IF CURRENT-PAGE > PAGE-COUNT
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.

           MOVE SPACES TO DISPLAY-BUF.
           MOVE 1 TO SUB.
           MOVE DL-PATH-OWNBR          TO DL-BRNO.
           MOVE PAGE-KEY(CURRENT-PAGE) TO DL-SHORT. 

           PERFORM START-DL2-FILE.

       BUILD-PAGENO.
           PERFORM READ-DL2-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (SUB > 9) OR
                                 (DL-BRNO NOT = DL-PATH-OWNBR)
              GO TO DISPLAY-PAGE.

           MOVE DL-NUM TO NUM-DL.
           PERFORM NUMDL-TO-ALPDL.
           MOVE ALP-DL TO D-KEY(SUB).
           MOVE DL-NAME TO D-NAME(SUB).
           ADD 1 TO SUB.
           GO TO BUILD-PAGENO.

       DISPLAY-PAGE.
           IF IO-FG NOT = 0
              IF SUB = 1
                 MOVE "NO RECORDS FOUND!" TO MESS
                 PERFORM SEND-LEGEND
                 MOVE DISPLAY-BUF TO WR-BUF
                 MOVE DISPLAY-LIST TO WR-LIST
                 PERFORM CALL-WRFORM
                 GO TO ENTER-SCANKEY.
           MOVE "MORE..." TO D-END-MORE.
           IF CURRENT-PAGE = PAGE-COUNT
              IF IO-FG = 0
                 IF PAGE-COUNT < MAX-PAGES
                    ADD 1 TO PAGE-COUNT
                    MOVE DL-SHORT TO PAGE-KEY(PAGE-COUNT)
                 ELSE
                    MOVE "MAXIMUM PAGES REACHED!" TO MESS
                    PERFORM SEND-LEGEND
                    MOVE "OVRFLOW" TO D-END-MORE
              ELSE
                 MOVE "* END *" TO D-END-MORE.
           MOVE CURRENT-PAGE TO D-PAGENO.
           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       SELECT-FIELD-FROM-DISPLAY.
           MOVE "SELECT DEAL(RTN), F6-MORE INFO:" TO MESS.
           PERFORM SEND-LEGEND.
           PERFORM CALL-FLDSEL.

      * TEST UP ARROW:
           IF FLDSEL-STAT = "1U"
              IF CURRENT-PAGE = 1
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE LAST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST PAGE UP:
           IF FLDSEL-STAT = "1P"
              IF CURRENT-PAGE = 1
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST PAGE DOWN OR DOWN ARROW:
           IF FLDSEL-STAT = "1N" OR FLDSEL-STAT = "1D"
              IF CURRENT-PAGE NOT < PAGE-COUNT
                 PERFORM ALARM
                 GO TO SELECT-FIELD-FROM-DISPLAY
              ELSE
                 MOVE 1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO START-BUILD-PAGENO.

      * TEST HOME:
           IF FLDSEL-STAT = "1H"
              MOVE 0 TO CURRENT-PAGE
              MOVE 1 TO ONE
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              GO TO START-BUILD-PAGENO.

           IF FLDSEL-STAT = "1>"
              GO TO END-ROUTINE.
           IF FLDSEL-STAT = "10"
              GO TO ENTER-SCANKEY.
           IF NOT (FLDSEL-STAT = "00" OR "1<")
              PERFORM ALARM
              GO TO SELECT-FIELD-FROM-DISPLAY.

           MOVE FLDSEL-KEY TO ALP-DL.
           PERFORM ALPDL-TO-NUMDL.
           MOVE NUM-DL      TO DLSCAN-NUM.
           MOVE FLDSEL-NAME TO DLSCAN-NAME.
           MOVE "00"        TO DLSCAN-STATUS.

           IF FLDSEL-STAT = "1<"
              PERFORM MORE-INFO
              GO TO SELECT-FIELD-FROM-DISPLAY.

           GO TO END-ROUTINE.

      *************************************
       MORE-INFO SECTION.
      *************************************
           PERFORM CLOSE-DL2-FILE.

           PERFORM OPEN-DL1-FILE.
           MOVE DLSCAN-NUM TO DL-NUM.
           PERFORM READ-DL1-FILE.
           PERFORM CLOSE-DL1-FILE.
           IF IO-FG NOT = 0
              MOVE "UNABLE TO READ DEALER RECORD" TO MESS
              PERFORM SEND-MESS
              GO TO MORE-INFO-EXIT.

           MOVE 11 TO WINDOW-LINES.
           MOVE 44 TO WINDOW-COLUMNS.
           MOVE 2 TO WINDOW-BEGIN-Y.
           MOVE 2 TO WINDOW-BEGIN-X.
           MOVE FLDSEL-FIELD TO WINDOW-FIT-TO.
           MOVE "WI/DLSCN2" TO WINDOW-FORM-NAM.
           MOVE "DLSCN2" TO VDU-FORM-NAME.

           PERFORM OPEN-WINDOW.
           DISPLAY DLSCN2-SCREEN.

           MOVE DLSCN2-FORM-FIELDS TO WR-BUF.
           MOVE DLSCN2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE DL-NAME TO M-NAME.
           PERFORM SET-CITY-STATE-ZIP.
           IF DL-ADR2 = " "
              MOVE DL-ADR1 TO M-ADR1
              MOVE MESS TO M-ADR2
              MOVE " " TO M-ADR3
           ELSE
              MOVE DL-ADR1 TO M-ADR1
              MOVE DL-ADR2 TO M-ADR2
              MOVE MESS TO M-ADR3.
           MOVE DL-PHONE TO TEL-BUF.
           PERFORM SEND-PHONE.
           MOVE TEL-EDIT TO M-PHONE.
           MOVE DL-SHORT TO M-SCAN.   
           IF DL-ORIGDATE NOT = 0
              MOVE DL-ORIGDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-ALPDATE
              MOVE ALP-DATE TO M-ORIG.
           IF DL-TERMDATE NOT = 0
              MOVE DL-TERMDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-ALPDATE
              MOVE ALP-DATE TO M-TERM.
           MOVE DL-BRNO   TO M-BR.

           MOVE MORE-BUF TO WR-BUF.
           MOVE MORE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "E  A010CONTINUE." TO VDU.
           PERFORM SCREENIO.

       MORE-INFO-CLOSE-WINDOW.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY DLSCN2-SCREEN.
           PERFORM CLOSE-WINDOW.
           MOVE "DLSCAN" TO VDU-FORM-NAME.
       MORE-INFO-EXIT.
           PERFORM OPEN-DL2-FILE.

       COPY "LIBWI/PFSWINDOWS.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".
       COPY "LIBGB/DATER.CPY".

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY DLSCAN-SCREEN UPON DLSCAN-WINDOW-HANDLE.
           MOVE DLSCAN-FORM-FIELDS TO WR-BUF.
           MOVE DLSCAN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE DLSCAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE DLSCAN-HELP--DIR  TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/DLSCAN_EVA.CPY".
              COPY "LIBWI/DLSCN2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************
       MISC-PARAGRAPHS SECTION.
      ******************************
       COPY "LIBGB/ACCESS.CPY".
       SEND-PHONE.
           MOVE TEL-1 TO TEL-ED1.
           MOVE TEL-2 TO TEL-ED2.
           MOVE TEL-3 TO TEL-ED3.
       SET-CITY-STATE-ZIP.
           MOVE DL-CITY TO MESS.
           INSPECT MESS REPLACING FIRST "  " BY ",?".
           INSPECT MESS REPLACING FIRST "  "
               BY DL-STATE.
           INSPECT MESS REPLACING FIRST "  " BY "? ".
           INSPECT MESS REPLACING FIRST "          "
               BY DL-ZIP.
           INSPECT MESS REPLACING ALL "?" BY " ".
       SEND-LEGEND.
           MOVE "S  A600WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       NUMDL-TO-ALPDL.
           MOVE NUM-DL1234 TO ALP-DL1234.
           MOVE NUM-DL56 TO ALP-DL56.
           MOVE "." TO ALP-PERIOD.
       ALPDL-TO-NUMDL.
           MOVE ALP-DL1234 TO NUM-DL1234.
           MOVE ALP-DL56 TO NUM-DL56.
           IF NUM-DL NOT NUMERIC
              MOVE 0 TO NUM-DL.

      ****************************
       IO-ROUTINES SECTION.
      ****************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-DL2-FILE.
           PERFORM CLOSE-DL1-FILE.
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBLP/LPDL2RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ****************************
       END-ROUTINE SECTION.
      ****************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY DLSCAN-SCREEN.
           DESTROY DLSCAN-WINDOW-HANDLE.
           EXIT PROGRAM.
