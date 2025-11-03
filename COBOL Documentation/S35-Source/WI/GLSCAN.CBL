       IDENTIFICATION DIVISION.
       PROGRAM-ID.      GLSCAN.
      *****************************************************
      *          GENERAL LEDGER ACCOUNT # SCAN
      *
      *  BLV 082297 EXPAND GL BRANCH # ENTRY FROM 3 TO 4
      *  GLB 990301 ADD WK2-FILE TO SPEED UP SEARCH FOR
      *             ONE BRANCH ACCOUNT NO.S
      *  GLB 990614 REMOVE WK2-FILE
      *  MG  000204 FIXED VDUSTATUS/IO-FG/STAT PER DEVELOPMENT MEETING.
      *  GLB BUG WHEN EXITING TO END-ROUTINE;FLDSEL-STAT NOT SET
      *  BLM 210728 IN BUILD-WK2-GL-FILE, MOVE 0 TO GL-ACCOUNT TO
      *             FIX CASTING ERROR IN START-GL
      *****************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSWK2I.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

      ******************************************************************
       FD  WK2-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK2-REC.
           03  WK2-KEY.
               05  WK2-DESCRIPTION         PIC X(36).
           03  WK2-BRANCH                  PIC 9(4).
           03  WK2-ACCOUNT                 PIC 9(7).


       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./GL/GLSCAN.CBL S34 07/28/21-12:31:04 >".
      ***************************
      *  FILE PATH DEFINITIONS  *
      ***************************
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".

       01  WK2-IFN         PIC X(3) VALUE "WK2".
       01  WK2PATH         PIC X(35).

       01  GLSCAN-LINES            PIC 99  VALUE 16.
       01  GLSCAN-COLUMNS          PIC 99  VALUE 54.
       01  GLSCAN-BEGIN-Y          PIC 99  VALUE 7.
       01  GLSCAN-BEGIN-X          PIC 99  VALUE 10.

       01  ERRCD           PIC X VALUE " ".
       01  SUB             PIC 99.
       01  DUPCNT          PIC 999.
       01  DUPSUB          PIC 999.

       01  FLDSEL-STAT     PIC XX.
       01  FLDSEL-LIST.
           03  FILLER      PIC X(36)  VALUE
               "010 020 030 040 050 060 070 080 090.".
       01  FLDSEL-BUF.
           07  FLDSEL-KEY         PIC X(12).
           07  FLDSEL-NO          REDEFINES FLDSEL-KEY.
               09  FLDSEL-BRANCH  PIC 9(4).
               09  FLDSEL-BR-ALP  REDEFINES FLDSEL-BRANCH
                                  PIC X(4).
               09  FLDSEL-DASH    PIC X.
               09  FLDSEL-ACCOUNT PIC 9(7).
           07  FILLER             PIC XX.
           07  FLDSEL-DESC        PIC X(36).
           07  FLDSEL-TYPE        PIC 99 VALUE 2.

       01  FIRST-FIELD          PIC X(4)  VALUE "010.".
       01  LAST-FIELD           PIC X(4)  VALUE "090.".

       01  PROMPT-LIST              PIC X(6)  VALUE "000:S.".

       01  FLDSEL-FIELD  PIC X(4)  VALUE "010.".

       01  DISPLAY-LIST.
           03  FILLER      PIC X(51)  VALUE
               "010 020 030 040 050 060 070 080 090 PGNO ENDMORE:F.".
       01  DISPLAY-BUF.
           03  D-LINE      OCCURS 9 TIMES.
               05  D-BRANCH  PIC 9999.
               05  D-DASH    PIC X.
               05  D-ACCOUNT PIC 9(7).
               05  FILLER    PIC XX.
               05  D-DESC    PIC X(36).
           03  D-PAGE        PIC ZZ9.
           03  D-END-MORE    PIC X(7).

       01  SCAN-TYPE       PIC 9      VALUE 2.
       01  SCAN-GL1-KEY.
           03  SCAN-LOC        PIC 9999.
           03  SCAN-ACCOUNT    PIC X(7).
       01  SCAN-ALPHA      PIC X(40).
       01  PAGE-KEY-TABLE.
           03  PAGE-KEY    PIC X(36)  OCCURS 200.
       01  ONE             PIC S9     VALUE 1.
       01  PAGE-NUMBER     PIC S999.
       01  MAX-PAGES       PIC 999.
       01  TABLE-SIZE      PIC 999    VALUE 200.

       COPY "LIBGL/GLSCAN_CMD.CPY".

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/PHONE.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       01  GLSCAN-WINDOW-HANDLE  PIC X(10).
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBGL/GLSCAN_DEF.CPY".
       COPY "LIBGL/GLSCAN_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       COPY "LIBGL/GLSCANW.CPY".

       SCREEN SECTION.
       COPY "LIBGL/GLSCAN_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME GLSCAN-BUF EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               WK2-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-GL2-FILE.
           PERFORM CLOSE-GL1-FILE.
           CLOSE
               WK2-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      *************************************
      *    MAIN PROGRAM MODULE
      *************************************
       MAIN-PROGRAM SECTION.
      D    STOP MESS.
           PERFORM SQL-CONNECT.

           MOVE " " TO VDU-WIMENU-SELECTION.
           MOVE "XX" TO GLSCAN-STATUS.

           MOVE "GLSCAN" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT  SCREEN LINE GLSCAN-BEGIN-Y * VDU-WIN-L-OFFSET
                   AT  SCREEN COL  GLSCAN-BEGIN-X * VDU-WIN-C-OFFSET
                   SIZE    GLSCAN-COLUMNS + VDU-WIN-W-MOD
                   LINES   GLSCAN-LINES + VDU-WIN-H-MOD
           COLOR IS VDU-WCOLOR-SCAN,CONTROL VALUE IS VDU-WCTRL-SCAN
                   CELL SIZE IS LABEL FONT
                   BLANK SCREEN
                   HANDLE  IS GLSCAN-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".
           PERFORM FORM-RESET.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WK2PATH.
           PERFORM OPEN-WK2-FILE-OUTPUT.
           PERFORM CLOSE-WK2-FILE.
           PERFORM OPEN-WK2-FILE.
           PERFORM OPEN-GL2-FILE.

           MOVE 2 TO SCAN-TYPE.
           MOVE "S  A010TYPE." TO VDU.
           MOVE SCAN-TYPE TO VDUNUM0.
           MOVE "SN N040SCANLOC." TO VDU.

           MOVE GLSCAN-BRANCH TO SCAN-LOC VDUNUM0.
           PERFORM SCREENIO.
           GO TO SEND-SCAN-TYPE-PROMPT.

      *----------------------------------------------------------------
      *    ENTER G/L SCAN TYPE
      *----------------------------------------------------------------
       ENTER-TYPE.
           MOVE LEGEND-01 TO MESS.
           PERFORM SEND-LEGEND.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010TYPE." TO VDU.
           MOVE "_" TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "EN N010TYPE." TO VDU.
           PERFORM SCREENIO.
           IF F7-KEY
      * GLB BUG IF EXIT FROM HERE 12/07/00
              MOVE VDUSTATUS TO FLDSEL-STAT
              GO TO END-ROUTINE.
           IF NOT ENTER-KEY
              GO TO ENTER-TYPE.
           IF NOT (VDUNUM0 = 1 OR 2)
              GO TO ENTER-TYPE.
           MOVE VDUNUM0 TO SCAN-TYPE.

      *----------------------------------------------------------------
       SEND-SCAN-TYPE-PROMPT.
           IF SCAN-TYPE = 1
              MOVE WGLACCT-PROMPT TO MESS
           ELSE
              MOVE WGLDESC-PROMPT TO MESS.
           MOVE "S  A000PROMPT." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
      *    ENTER SCAN KEY
      *----------------------------------------------------------------
       ENTER-SCAN-LOC-KEY.
           MOVE LEGEND-02 TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040SCANLOC." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY OR F7-KEY
      * GLB BUG IF EXIT FROM HERE 12/07/00
              MOVE VDUSTATUS TO FLDSEL-STAT
              GO TO END-ROUTINE.
           IF UP-KEY
              GO TO ENTER-TYPE.
           IF NOT ENTER-KEY
              GO TO ENTER-SCAN-LOC-KEY.
           IF VDUNUM0 = 0
              MOVE "SN N040SCANLOC." TO VDU
              MOVE SCAN-LOC TO VDUNUM0
              PERFORM SCREENIO.
           MOVE VDUNUM0 TO SCAN-LOC.


       ENTER-DESC-KEY.
           IF SCAN-TYPE = 1
              MOVE LEGEND-03A TO MESS
           ELSE
              MOVE LEGEND-03B TO MESS.
           PERFORM SEND-LEGEND.

           MOVE "E  A000ALPHA." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY OR F7-KEY
      * GLB BUG IF EXIT FROM HERE 12/07/00
              MOVE VDUSTATUS TO FLDSEL-STAT
              GO TO END-ROUTINE.
           IF UP-KEY
              GO TO ENTER-SCAN-LOC-KEY.
           IF NOT ENTER-KEY
              GO TO ENTER-DESC-KEY.
           IF SCAN-TYPE = 1
              MOVE VDUBUF TO SCAN-ACCOUNT
              INSPECT SCAN-ACCOUNT REPLACING ALL " " BY "0"
              MOVE SCAN-GL1-KEY TO VDUBUF.
           MOVE VDUBUF TO SCAN-ALPHA.

      *----------------------------------------------------------------
       START-SCAN-LOGIC.

           IF SCAN-LOC NOT = 0
              IF SCAN-TYPE = 2
                 PERFORM BUILD-WK2-GL-FILE.

           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1 TO MAX-PAGES ONE.
           MOVE 0 TO PAGE-NUMBER.
           IF SCAN-TYPE = 1
              MOVE SCAN-GL1-KEY TO VDUBUF
           ELSE
              MOVE SCAN-ALPHA TO VDUBUF.
           MOVE VDUBUF TO PAGE-KEY(MAX-PAGES).
       DISPLAY-PAGE.
           ADD ONE TO PAGE-NUMBER.
           IF PAGE-NUMBER < 1
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO PAGE-NUMBER.
           IF PAGE-NUMBER > MAX-PAGES
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO PAGE-NUMBER.
           MOVE SPACES TO DISPLAY-BUF.
           MOVE 1 TO SUB.
           MOVE 1 TO DUPCNT.

           IF SCAN-LOC = 0
              PERFORM START-ALL-BRANCHES
           ELSE
              PERFORM START-ONE-BRANCH.

       CHECK-DUPS.
           IF PAGE-NUMBER > 1
              COMPUTE DUPSUB = PAGE-NUMBER - DUPCNT
              IF PAGE-KEY(PAGE-NUMBER) = PAGE-KEY(DUPSUB)
                 PERFORM NEXT-VALID-GL 9 TIMES
                 ADD 1 TO DUPCNT
                 IF DUPCNT < PAGE-NUMBER
                    GO TO CHECK-DUPS.
       MOVE-DISPLAY.
           PERFORM NEXT-VALID-GL.
           IF IO-BAD OR SUB > 9
              GO TO SEND-DISPLAY.
           IF SUB = 1
              IF PAGE-NUMBER = 1
                 IF SCAN-TYPE = 1
                    MOVE GL1-KEY TO PAGE-KEY(1)
                  ELSE
                    MOVE GL2-KEY TO PAGE-KEY(1).
           MOVE GL-BRANCH TO D-BRANCH(SUB).
           MOVE "-" TO D-DASH(SUB).
           MOVE GL-ACCOUNT TO D-ACCOUNT(SUB).
           MOVE GL-DESCRIPTION TO D-DESC(SUB).
           ADD 1 TO SUB.
           GO TO MOVE-DISPLAY.

       SEND-DISPLAY.
           IF IO-BAD
              IF SUB = 1
                 MOVE "   NO GL ACCOUNTS RECORDS FOUND" TO D-LINE(4)
                 MOVE DISPLAY-LIST TO WR-LIST
                 MOVE DISPLAY-BUF TO WR-BUF
                 PERFORM CALL-WRFORM
                 GO TO ENTER-SCAN-LOC-KEY.
           MOVE "MORE..." TO D-END-MORE.
           IF PAGE-NUMBER = MAX-PAGES
              IF IO-GOOD
                 IF MAX-PAGES < TABLE-SIZE
                    ADD 1 TO MAX-PAGES
                    IF SCAN-TYPE = 1
                       MOVE GL1-KEY TO PAGE-KEY(MAX-PAGES)
                    ELSE
                       MOVE GL2-KEY TO PAGE-KEY(MAX-PAGES)
                    END-IF
                 ELSE
                    MOVE "OVRFLOW" TO D-END-MORE
              ELSE
                 MOVE "* END *" TO D-END-MORE.
           MOVE PAGE-NUMBER TO D-PAGE.
           MOVE DISPLAY-LIST TO WR-LIST.
           MOVE DISPLAY-BUF TO WR-BUF.
           PERFORM CALL-WRFORM.

           MOVE SELECT-PROMPT TO MESS.
           PERFORM SEND-LEGEND.

       SELECT-DISPLAY.
           PERFORM CALL-FLDSEL.

           IF FLDSEL-STAT = "1U"
              IF PAGE-NUMBER = 1
                 PERFORM ALARM
                 GO TO SELECT-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE LAST-FIELD TO FLDSEL-FIELD
                 GO TO DISPLAY-PAGE.
           IF FLDSEL-STAT = "1P"
              IF PAGE-NUMBER = 1
                 PERFORM ALARM
                 GO TO SELECT-DISPLAY
              ELSE
                 MOVE -1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO DISPLAY-PAGE.
           IF FLDSEL-STAT = "1N" OR FLDSEL-STAT = "1D"
              IF PAGE-NUMBER NOT < MAX-PAGES
                 PERFORM ALARM
                 GO TO SELECT-DISPLAY
              ELSE
                 MOVE 1 TO ONE
                 MOVE FIRST-FIELD TO FLDSEL-FIELD
                 GO TO DISPLAY-PAGE.
           IF FLDSEL-STAT = "1H"
              MOVE 0 TO PAGE-NUMBER
              MOVE 1 TO ONE
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              GO TO DISPLAY-PAGE.
           IF FLDSEL-STAT = "1>"
              GO TO END-ROUTINE.
           IF FLDSEL-STAT = "10"
              GO TO ENTER-SCAN-LOC-KEY.
           IF FLDSEL-STAT NOT = "00"
              PERFORM ALARM
              GO TO SELECT-DISPLAY.

           IF FLDSEL-BRANCH NOT NUMERIC GO TO SELECT-DISPLAY.
           IF FLDSEL-ACCOUNT NOT NUMERIC GO TO SELECT-DISPLAY.

           MOVE FLDSEL-KEY TO GLSCAN-KEY.
           MOVE "00"       TO GLSCAN-STATUS.

           GO TO END-ROUTINE.

      *************************************
       NEXT-VALID-GL SECTION.
      *************************************
           IF SCAN-LOC = 0
              IF SCAN-TYPE = 1
                 PERFORM READ-GL1-FILE-NEXT
              ELSE
                 PERFORM READ-GL2-FILE-NEXT.

           IF SCAN-LOC NOT = 0
              IF SCAN-TYPE = 1
                 PERFORM READ-GL1-FILE-NEXT
              ELSE
                 PERFORM READ-WK2-FILE-NEXT
                 MOVE WK2-BRANCH TO GL-BRANCH
                 MOVE WK2-ACCOUNT TO GL-ACCOUNT
                 MOVE WK2-DESCRIPTION TO GL-DESCRIPTION.

       NEXT-VALID-EXIT.
           EXIT.

      *************************************
       START-ALL-BRANCHES SECTION.
      *************************************
           IF SCAN-TYPE = 1
              MOVE PAGE-KEY(PAGE-NUMBER) TO GL1-KEY
              PERFORM START-GL1-FILE
           ELSE
              MOVE PAGE-KEY(PAGE-NUMBER) TO GL2-KEY
              PERFORM START-GL2-FILE.

      *************************************
       START-ONE-BRANCH SECTION.
      *************************************
           IF SCAN-TYPE = 1
              MOVE PAGE-KEY(PAGE-NUMBER) TO GL1-KEY
              PERFORM START-GL1-FILE
           ELSE
              MOVE PAGE-KEY(PAGE-NUMBER) TO WK2-KEY
              PERFORM START-WK2-FILE.

      *************************************
       BUILD-WK2-GL-FILE SECTION.
      *************************************
           PERFORM OPEN-WK2-FILE-OUTPUT.
           PERFORM CLOSE-WK2-FILE.
           PERFORM OPEN-WK2-FILE.
           MOVE SPACES TO GL-REC.
           MOVE SCAN-LOC TO GL-BRANCH.
           MOVE 0 TO GL-ACCOUNT.
           PERFORM START-GL1-FILE.
       NEXT-GL-BUILD.
           PERFORM READ-GL1-FILE-NEXT.
           IF IO-BAD GO TO EXIT-BUILD-WK2-FILE.
           IF GL-BRANCH NOT = SCAN-LOC
                 GO TO EXIT-BUILD-WK2-FILE.
           MOVE GL-DESCRIPTION TO WK2-DESCRIPTION.
           MOVE GL-BRANCH TO WK2-BRANCH.
           MOVE GL-ACCOUNT TO WK2-ACCOUNT.
           PERFORM WRITE-WK2-FILE.
           GO TO NEXT-GL-BUILD.
       EXIT-BUILD-WK2-FILE.
           EXIT.

      **********************************************************
       FORM-RESET SECTION.
      **********************************************************
           DISPLAY GLSCAN-SCREEN UPON GLSCAN-WINDOW-HANDLE.
           MOVE GLSCAN-FORM-FIELDS TO WR-BUF.
           MOVE GLSCAN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************************
       SETUP-LINK-INFO SECTION.
      **********************************************************
           MOVE GLSCAN-HELP--FILE TO HELP-LINK-FILE.
           MOVE GLSCAN-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBGL/GLSCAN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".
       COPY "LIBGB/DATER.CPY".

      ******************************************
       MISC-PARAGRAPHS SECTION.
      ******************************************
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/PHONEIO.CPY".
       SEND-SORT-PROMPT.
           IF SCAN-TYPE = 1
              MOVE WGLACCT-PROMPT TO MESS
           ELSE
              MOVE WGLDESC-PROMPT TO MESS.
           MOVE PROMPT-LIST TO WR-LIST.
           MOVE MESS TO WR-BUF.
           PERFORM CALL-WRFORM.

       SEND-LEGEND.
           MOVE "S  A600WSEL:D." TO VDU.
           MOVE MESS TO VDUBUF.
           PERFORM SCREENIO.

      **************************************
       IO-ROUTINES SECTION.
      **************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGL/GLGL1RN.CPY".
       COPY "LIBGL/GLGL2RN.CPY".
       COPY "LIBGB/GBWK2IN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      **************************************
       END-ROUTINE SECTION.
      **************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
           MOVE WK2PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY GLSCAN-SCREEN.
           DESTROY GLSCAN-WINDOW-HANDLE.

       STOP-RUN.
           EXIT PROGRAM.
