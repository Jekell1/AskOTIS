       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CPSCAN.
      *****************************************************
      *  COLLECTOR POOL WINDOW SCAN.
      *
      * REV:
      *  MJD 001205 ADDED CPSCAN-TYPE TO DETERMINE WHETHER
      *             TO SCAN THE LOCAL FILE OR THE GLOBAL FILE.
      *             (TFC PR#60)
      *
      * KEC 2018-1026 CONVERTED CPFILE FOR A32 & A34
      *               CP-PATH-OVERRIDE TO "G" IF GLOBAL QUEUEING.
      * KEC 2019-0405 FIXED HOW TO CHECK AFTER READ-CP1-FILE-NEXT
      *               FOR GLOBAL AND LOCAL USAGE.
      * BAH 2021-0119 MISSED SET OF CP-BRNO. SCAN WAS NOT FINDING ANY
      *               RECORDS. 
      *
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
           "@(#)WI/CPSCAN.CBL S35 02/22/24-12:49:29 >".
       COPY "LIBSP/SP01CP.CPY".
       COPY "LIBSP/SP01CP_SQL.CPY".
       COPY "LIBSP/SPWSCP.CPY".

       01  CPSCAN-LINES              PIC 99  VALUE 15.
       01  CPSCAN-COLUMNS            PIC 99  VALUE 29.
       01  CPSCAN-BEGIN-Y            PIC 99  VALUE 03.
       01  CPSCAN-BEGIN-X            PIC 99  VALUE 40.
       01  ERRCD                   PIC X      VALUE SPACES.
       01  ONE                     PIC S9     VALUE 1.
       01  SUB                     PIC 99.

       01  FLDSEL-FIELD            PIC X(4)   VALUE "010.".
       01  FLDSEL-STAT             PIC XX.
       01  FLDSEL-LIST.
           03  FILLER              PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080 090.".
       01  FLDSEL-BUF.
           05  FLDSEL-KEY          PIC X(3).
           05  FILLER              PIC XX.
           05  FLDSEL-NAME         PIC X(20).

       01  FIRST-FIELD             PIC X(4)  VALUE "010.".
       01  LAST-FIELD              PIC X(4)  VALUE "090.".

       01  DISPLAY-LIST.
           03  FILLER              PIC X(51)  VALUE
           "010 020 030 040 050 060 070 080 090 PGNO ENDMORE:F.".
       01  DISPLAY-BUF.
           03  D-LINE              OCCURS 9 TIMES.
               05  D-KEY           PIC X(3).
               05  FILLER          PIC XX.
               05  D-NAME          PIC X(20).
           03  D-PAGENO            PIC ZZ9.
           03  D-END-MORE          PIC X(7).

       01  CURRENT-PAGE            PIC S999.
       01  PAGE-COUNT              PIC 999.
       01  MAX-PAGES               PIC 999    VALUE 10.
       01  PAGE-KEY-01.
           03  PAGE-KEY                OCCURS 10 TIMES
                                       PIC X(3).


       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".

       01  CPSCAN-WINDOW-HANDLE    PIC X(10).
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/CPSCAN_DEF.CPY".
       COPY "LIBWI/CPSCAN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/CPSCANW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/CPSCAN_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME CPSCAN-BUF EXIT-PATHNAME.

      ***************************
       MAIN-PROGRAM SECTION.
      ***************************

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
      D    STOP MESS.

           MOVE "XX" TO CPSCAN-STATUS.
           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE "CPSCAN" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT LINE CPSCAN-BEGIN-Y
                   AT COL  CPSCAN-BEGIN-X
                   SIZE    CPSCAN-COLUMNS + VDU-WIN-W-MOD
                   LINES   CPSCAN-LINES + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS CPSCAN-WINDOW-HANDLE.

           PERFORM FORM-RESET.
     
           IF CPSCAN-TYPE = "G"
              MOVE EXT-FILPATH-BASE TO CP-GLOBAL-BASE
              MOVE EXT-FILPATH-MACHINE TO CP-GLOBAL-MACHINE
              MOVE "G"                 TO CP-PATH-OVERRIDE         
              MOVE CP-GLOBAL-PATH TO CP-PATH
              MOVE " GLOBAL COLLECTOR POOL SCAN" TO WR-BUF
              MOVE "TITLE." TO WR-LIST
              PERFORM CALL-WRFORM.

       BEGIN-BUILD-PAGENO.
           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1 TO PAGE-COUNT ONE.
           MOVE 0 TO CURRENT-PAGE.

           PERFORM OPEN-CP1-FILE.

           MOVE SPACES TO PAGE-KEY(PAGE-COUNT).
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

           MOVE CPSCAN-BRNO            TO CP-BRNO.
           MOVE PAGE-KEY(CURRENT-PAGE) TO CP-ID.

           PERFORM START-CP1-FILE.

       BUILD-PAGENO.
           PERFORM READ-CP1-FILE-NEXT.
           IF IO-FG NOT = 0 OR SUB > 9
              GO TO DISPLAY-PAGE.

           IF ( CP-PATH-OVERRIDE-GLOBAL ) AND                  
              ( CP-BRNO NOT = ZEROES    )                      
              MOVE 9 TO IO-FG                                  
              GO TO DISPLAY-PAGE                               
           ELSE                                                
           IF ( NOT CP-PATH-OVERRIDE-GLOBAL ) AND              
              ( CP-BRNO NOT = CP-PATH-OWNBR )                  
              MOVE 9 TO IO-FG                                  
              GO TO DISPLAY-PAGE.                              

           MOVE CP-ID TO D-KEY(SUB).
           MOVE CP-DESC TO D-NAME(SUB).
           ADD 1 TO SUB.
           GO TO BUILD-PAGENO.

       DISPLAY-PAGE.
           IF IO-FG NOT = 0
              IF SUB = 1
                 MOVE DISPLAY-LIST TO WR-LIST
                 MOVE DISPLAY-BUF TO WR-BUF
                 PERFORM CALL-WRFORM
                 MOVE "NO RECORDS FOUND!" TO MESS
                 PERFORM SEND-MESS
                 GO TO END-ROUTINE.

           MOVE "MORE..." TO D-END-MORE.
           IF CURRENT-PAGE = PAGE-COUNT
              IF IO-FG = 0
                 IF PAGE-COUNT < MAX-PAGES
                    ADD 1 TO PAGE-COUNT
                    MOVE CP-ID TO PAGE-KEY(PAGE-COUNT)
                 ELSE
                    MOVE "MAXIMUM PAGES REACHED!" TO MESS
                    PERFORM SEND-LEGEND
                    MOVE "OVRFLOW" TO D-END-MORE
              ELSE
                 MOVE "* END *" TO D-END-MORE.
           MOVE CURRENT-PAGE TO D-PAGENO.
           MOVE DISPLAY-LIST TO WR-LIST.
           MOVE DISPLAY-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.

       SELECT-FIELD-FROM-DISPLAY.
           MOVE "F7 EXIT, SELECT CODE(RTN):" TO MESS.
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
           IF NOT (FLDSEL-STAT = "00" OR "1<")
              PERFORM ALARM
              GO TO SELECT-FIELD-FROM-DISPLAY.

           MOVE FLDSEL-KEY TO CPSCAN-KEY.
           MOVE "00"       TO CPSCAN-STATUS.

           GO TO END-ROUTINE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".
       COPY "LIBGB/DATER.CPY".

      *****************************************************
       FORM-RESET SECTION.
      *****************************************************
           DISPLAY CPSCAN-SCREEN UPON CPSCAN-WINDOW-HANDLE.
           MOVE CPSCAN-FORM-FIELDS TO WR-BUF.
           MOVE CPSCAN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *****************************************************
       SETUP-LINK-INFO SECTION.
      *****************************************************
           MOVE CPSCAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE CPSCAN-HELP--DIR  TO HELP-LINK-DIR.

      *****************************************************
       VDU-CONVERT-FIELD SECTION.
      *****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/CPSCAN_EVA.CPY".
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

      **************************
       IO-ROUTINES SECTION.
      **************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-CP1-FILE.
       COPY "LIBSP/SPCPGS_SQL.CPY".
       COPY "LIBSP/SPCP1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      **************************
       END-ROUTINE SECTION.
      **************************
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY CPSCAN-SCREEN.
           DESTROY CPSCAN-WINDOW-HANDLE.
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           EXIT PROGRAM.
