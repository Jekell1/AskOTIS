       IDENTIFICATION DIVISION.
       PROGRAM-ID.      DTCKR3.
      *****************************************************
      *  DEALER CHECK RELEASE / DEALER CHECK DISPLAY
      *
      * REV:
      *  000807 BAH MOVE LOW-VALUES TO WK2-KEY WAS CAUSING A
      *             NON-NUMERIC ERROR IN ACU_TCLP AT WISCONSIN,
      *             WHY LOW-VALUES TO 9(5) ? CHANGED TO 0
      * GLB 011107 MAKE WK-REC MATCH LONPT0, DTCKRL
      * GLB 011107 ADD CLASS TO SCREEN
      * GLB 011107 BUG; REMOVE WK2-FILE
      * GLB 020108 CHANGE WK-FILE
      * BAH 181012 GLOBAL DLFILE
      *****************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WK-FILE ASSIGN WK-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK-KEY
                  FILE STATUS FILE-STAT.
           SELECT WK2-FILE ASSIGN WK2-PATH
                  ORGANIZATION RELATIVE
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RELATIVE KEY WK2-KEY
                  FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.


      *************************************************
      *
      *     WKFILE - NOT THE SAME AS RELATIVE WKFILE
      *     USED IS LOAN PROCESSING
      *
      *     WKFILE - USED TO CAPTURE THE AMOUNT OF EACH
      *              CHECK THAT WILL BE CREATED FOR
      *              EACH DEALER/DEALER BRANCH
      *
      *************************************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-DLNO       PIC 9(4)V99.
               05  WK-NUM-RED   REDEFINES WK-DLNO.
                   07  WK-DL     PIC 9(4).
                   07  WK-DLSUB  PIC 99.
               05  WK-CLASS      PIC 9(3).
           03  WK-DLBR           PIC 99.
           03  WK-NO-TRANS       PIC S9(5)    COMP.
           03  WK-UNPAID         PIC S9(9)V99 COMP.
           03  WK-LAST-ACCTNO    PIC 9(8).
           03  WK-CX-BRNO        PIC 9(4).
           03  WK-CX-TYPE        PIC X(2).
           03  WK-CX-SEQNO       PIC 9(4).

      ********************************************************
      *
      *     WK2FILE - REPORT DISPLAY OF THE CHECK TO BE
      *               CREATED FOR EACH DEALER/DEALER BRANCH
      *
      ********************************************************
       FD  WK2-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK2-REC.
           03  FILLER          PIC X.
           03  WK2-DLNO        PIC ZZZ9.99.
           03  FILLER          PIC X.
           03  WK2-NAME        PIC X(24).
           03  FILLER          PIC X.
           03  WK2-CLASS       PIC 99.
           03  FILLER          PIC X.
           03  WK2-NO-TRANS    PIC ZZZZ9.
           03  FILLER          PIC X.
           03  WK2-UNPAID      PIC Z,ZZZ,ZZ9.99-.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)WI/DTCKR3.CBL S35 02/22/24-09:58:46 >".
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".
       01  WK-IFN                PIC XX      VALUE "WK".
       01  WK2-IFN               PIC XXX     VALUE "WK2".
       01  WK2-KEY               PIC 9(5).
       01  WK2-PATH              PIC X(30)   VALUE " ".
       01  WK-PATH               PIC X(30)   VALUE " ".

       01  DTCKR3-LINES          PIC 99  VALUE 22.
       01  DTCKR3-COLUMNS        PIC 99  VALUE 61.
       01  DTCKR3-BEGIN-Y        PIC 99  VALUE 2.
       01  DTCKR3-BEGIN-X        PIC 99  VALUE 10.

       01  ERRCD                 PIC X       VALUE SPACES.
       01  GRAND-TOTALS.
           03  UNPAID-TOT          PIC S9(9)V99.
           03  NO-TRANS-TOT        PIC S9(9)V99.
       01  ONE                     PIC S9     VALUE 1.
       01  SUB                     PIC 99.

       01  FLDSEL-FIELD            PIC X(4)   VALUE "010.".
       01  FLDSEL-STAT             PIC XX.
       01  FLDSEL-LIST.
           03  FILLER              PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080 090.".
       01  FLDSEL-BUF.
           05  FLDSEL-KEY          PIC X(66).

       01  FIRST-FIELD             PIC X(4)  VALUE "010.".
       01  LAST-FIELD              PIC X(4)  VALUE "090.".

       01  DISPLAY-LIST.
           03  FILLER              PIC X(36)  VALUE
           "010 020 030 040 050 060 070 080 090 ".
           03  FILLER              PIC X(39)  VALUE
           "100 110 120 130 140 150 PGNO ENDMORE:F.".
       01  DISPLAY-BUF.
           03  D-LINE              OCCURS 15 TIMES
                                   PIC X(56).
           03  D-PAGENO            PIC ZZ9.
           03  D-END-MORE          PIC X(7).

       01  HOLD-DL1-KEY            PIC X(10)   VALUE " ". 
       01  CURRENT-PAGE            PIC S999.
       01  PAGE-COUNT              PIC 999.
       01  MAX-PAGES               PIC 999    VALUE 50.
       01  PAGE-KEY-01.
           03  PAGE-KEY-WORK       OCCURS 50 TIMES.
               05  PAGE-KEY        PIC 9(5).

      *01  MORE-LIST.
      *    03  FILLER     PIC X(39)  VALUE
      *    "TITLE ADR1 ADR2 ADR3 PHONE EMPL SPOUSE ".
      *    03  FILLER     PIC X(16)  VALUE
      *    "LIA OPEN CLOSED.".
      *01  MORE-BUF.
      *    03  M-NAME    PIC X(30).
      *    03  M-ADR1    PIC X(30).
      *    03  M-ADR2    PIC X(30).
      *    03  M-ADR3    PIC X(30).
      *    03  M-PHONE   PIC X(14).
      *    03  M-EMPL    PIC X(20).
      *    03  M-SPOUSE  PIC X(28).
      *    03  M-LIA     PIC Z,ZZZ,ZZ9.99.
      *    03  M-OPEN    PIC ZZ.
      *    03  M-CLOSED  PIC ZZ.


       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/DATERW.CPY".

       01  DTCKR3-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/DTCKR3_DEF.CPY".
       COPY "LIBWI/DTCKR2_DEF.CPY".
       COPY "LIBWI/DTCKR3_WKS.CPY".
       COPY "LIBWI/DTCKR2_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBWI/DTCKR3W.CPY".

       SCREEN SECTION.
       COPY "LIBWI/DTCKR3_SCN.CPY".
       COPY "LIBWI/DTCKR2_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME DTCKR3-BUF EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               WK-FILE WK2-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-DL1-FILE.
           CLOSE
               WK-FILE WK2-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ***********************************
      *      MAIN PROGRAM MODULE
      ***********************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".

      D    STOP MESS.
           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE "DTCKR3" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT LINE DTCKR3-BEGIN-Y
                   AT COL  DTCKR3-BEGIN-X
                   SIZE    DTCKR3-COLUMNS + VDU-WIN-W-MOD
                   LINES   DTCKR3-LINES + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS DTCKR3-WINDOW-HANDLE.

           PERFORM FORM-RESET.

      * CREATE TEMPORARY WORK FILE TO HOLD THE AMOUNT OF DEALER CHECKS:
           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WK2-PATH.
           PERFORM OPEN-WK2-FILE.

      * SET WK-PATH:
           MOVE DTCKR3-WK-PATH TO WK-PATH.

           MOVE 0 TO UNPAID-TOT
                     NO-TRANS-TOT.
           PERFORM GENERATE-REPORT.

       BEGIN-BUILD-PAGENO.
           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1 TO PAGE-COUNT ONE.
           MOVE 0 TO CURRENT-PAGE.
           PERFORM OPEN-WK2-FILE.
           MOVE 0 TO PAGE-KEY(PAGE-COUNT).
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
           MOVE PAGE-KEY(CURRENT-PAGE) TO WK2-KEY.
           PERFORM START-WK2-FILE.

       BUILD-PAGENO.
           PERFORM READ-WK2-FILE-NEXT.
           IF IO-FG NOT = 0 OR SUB > 15
              GO TO DISPLAY-PAGE.
           MOVE WK2-REC TO D-LINE(SUB).
           ADD 1 TO SUB.
           GO TO BUILD-PAGENO.

       DISPLAY-PAGE.
           IF IO-FG NOT = 0
              IF SUB = 1
                 MOVE DISPLAY-BUF TO WR-BUF
                 MOVE DISPLAY-LIST TO WR-LIST
                 PERFORM CALL-WRFORM
                 MOVE "NO RECORDS FOUND!" TO MESS
                 PERFORM SEND-MESS
                 GO TO END-ROUTINE.

           MOVE "MORE..." TO D-END-MORE.
           IF CURRENT-PAGE = PAGE-COUNT
              IF IO-FG = 0
                 IF PAGE-COUNT < MAX-PAGES
                    ADD 1 TO PAGE-COUNT
                    MOVE WK2-KEY TO PAGE-KEY(PAGE-COUNT)
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

           GO TO END-ROUTINE.


      *************************************
       GENERATE-REPORT SECTION.
      *************************************
           PERFORM OPEN-WK-FILE.
           MOVE 0 TO WK2-KEY.

           MOVE 0 TO WK-DLNO WK-CLASS.
           PERFORM START-WK-FILE.
       GENERATE-REPORT-NEXT.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG = 9
              GO TO GENERATE-REPORT-EXIT.

           PERFORM GENERATE-REPORT-DETAIL.
           ADD 1 TO WK2-KEY.
           PERFORM WRITE-WK2-FILE.
           GO TO GENERATE-REPORT-NEXT.

       GENERATE-REPORT-EXIT.
           PERFORM GENERATE-TOTAL-LINE.
           ADD 1 TO WK2-KEY.
           PERFORM WRITE-WK2-FILE.

           PERFORM CLOSE-WK-FILE.
           PERFORM CLOSE-WK2-FILE.

      *************************************
       GENERATE-REPORT-DETAIL SECTION.
      *************************************

           MOVE " " TO WK2-REC.
           MOVE WK-DLNO TO WK2-DLNO DL-DLNO.
           PERFORM GET-DL.
           MOVE DL-NAME     TO WK2-NAME.
           MOVE WK-NO-TRANS TO WK2-NO-TRANS.
           MOVE WK-CLASS    TO WK2-CLASS.
           MOVE WK-UNPAID   TO WK2-UNPAID.

           ADD WK-NO-TRANS TO NO-TRANS-TOT.
           ADD WK-UNPAID TO UNPAID-TOT.

       GENERATE-REPORT-DETAIL-EXIT.
           EXIT.

      *************************************
       GENERATE-TOTAL-LINE SECTION.
      *************************************

           MOVE " " TO WK2-REC.
           MOVE "TOTAL OF CHECKS: " TO WK2-NAME.
           MOVE NO-TRANS-TOT TO WK2-NO-TRANS.
           MOVE UNPAID-TOT TO WK2-UNPAID.

       GENERATE-TOTAL-LINE-EXIT.
           EXIT.


      *************************************************
       FORM-RESET SECTION.
      *************************************************
           DISPLAY DTCKR3-SCREEN UPON DTCKR3-WINDOW-HANDLE.
           MOVE DTCKR3-FORM-FIELDS TO WR-BUF.
           MOVE DTCKR3-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE DTCKR3-HELP--FILE TO HELP-LINK-FILE.
           MOVE DTCKR3-HELP--DIR  TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/DTCKR3_EVA.CPY".
              COPY "LIBWI/DTCKR2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *************************************
       MISC-PARAGRAPHS SECTION.
      *************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       SEND-LEGEND.
           MOVE "S  A600WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.
       GET-DL.
           IF HOLD-DL1-KEY NOT = DL1-KEY
              PERFORM OPEN-DL1-FILE
              PERFORM READ-DL1-FILE
              PERFORM CLOSE-DL1-FILE
              IF IO-FG = 9
                 MOVE "** NOT ON FILE **" TO DL-NAME
              ELSE
                 MOVE DL1-KEY TO HOLD-DL1-KEY.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".
       COPY "LIBGB/DATER.CPY".

      ********************************************
       IO-ROUTINES SECTION.
      ********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBLP/LPDL1RN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBGB/GBWK2IN.CPY".

       COPY "LIBGB/FERRORS.CPY".

      *************************************
       END-ROUTINE SECTION.
      *************************************
           PERFORM CLOSE-FILES.
           MOVE WK2-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
       EXIT-PROG.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY DTCKR3-SCREEN.
           DESTROY DTCKR3-WINDOW-HANDLE.
       STOP-RUN.
           EXIT PROGRAM.

