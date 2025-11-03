       IDENTIFICATION DIVISION.
       PROGRAM-ID.      FLINQ.
      ******************************************************************
      *                 (WI)
      *
      *  DESCRIPTION:   FLAG FIELD INQUIRY
      *                  CALLED FROM GBMENU GLOBAL MENU
      * REV:
      * BLF 060106 CHANGED KEYWORDS FROM 15 TO 20, NOW MAY ONLY SELECT
      *            UP TO 6 INSTEAD OF UP TO 8 KEYWORDS
      *
      * KEC 2018.0514 PR#00000 PARADATA A30 JAY & BETH
      *         REMOVED USE OF ENT-GROUP AND/OR GROUP-FLAG.
      *         THIS WAS DONE IN RELATION TO THE EOBUF EXTERNAL ISSUE
      *         WHICH IF NOT BEING SET WILL CAUSE ISSUES FROM THOSE
      *         BYTE POSITIONS COULD BE SET FROM PREVIOUS
      *         PROGRAM WITH EOBUF THAT SET SOMETHING IN THOSE BYTES.
      *  CS 20190726 FIXED FL-PATH
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSFL.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDFL.CPY".
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(42) VALUE
           "@(#)./WI/FLINQ.CBL A32 07/26/19-13:13:17 >".
      *********************************************************
      *        LIBGB/GBWSFL: FLFILE PATH WORKERS
      *********************************************************
       01  FL-PATH.                             
            03  FILLER                PIC X VALUE "/".
            03  FL-PATH-REL           PIC X(7).
            03  FILLER                PIC X(25) VALUE
                                  "/FILE.DOC/FLAGS          ".

      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       01  FLINQ-LINES           PIC 99   VALUE 22.
       01  FLINQ-COLUMNS         PIC 99   VALUE 80.
       01  FLINQ-BEGIN-Y         PIC 99   VALUE 1.
       01  FLINQ-BEGIN-X         PIC 99   VALUE 1.
       01  ERRCD                PIC X VALUE " ".
       01  MSEL-LIST         PIC X(8)       VALUE "MSEL:P.".
       01  ONE               PIC S9.
       01  KEYSUB            PIC 99.
       01  KEYSUBX REDEFINES KEYSUB PIC XX.
       01  KEYSUB2           PIC 99.
       01  SUB               PIC 999 COMP   VALUE 0.
       01  IO-TYPE           PIC X(3).

       01  INQUIRY-RANGES.
           03  KEYWORD          PIC X(20) OCCURS 6.

       01  FLDSEL-STAT     PIC XX.
       01  FLDSEL-LIST.
           03  FILLER      PIC X(40)  VALUE
               "010 020 030 040 050 060 070 080 090 100 ".
           03  FILLER      PIC X(20)  VALUE
               "110 120 130 140 150.".
       01  FLDSEL-BUF      PIC X(80).

       01  FIRST-FIELD          PIC X(4)  VALUE "010.".
       01  LAST-FIELD           PIC X(4)  VALUE "150.".

       01  FLDSEL-FIELD  PIC X(6)  VALUE "010.  ".
      *       THE ABOVE FIELD IS 2 EXTRA ON PURPOSE

       01  DISPLAY-LIST.
           03  FILLER      PIC X(40)  VALUE
               "010 020 030 040 050 060 070 080 090 100 ".
           03  FILLER      PIC X(35)  VALUE
               "110 120 130 140 150 PGNO ENDMORE:F.".
       01  DISPLAY-BUF.
           03  D-LINE                 OCCURS 15 TIMES.
               05  D-DESC             PIC X(35).
               05  FILLER             PIC X.
               05  D-PROG             PIC X(23).
               05  FILLER             PIC X.
               05  D-PAGE-PROMPT      PIC X(4).
               05  D-PAGE-NO          PIC ZZ9.
               05  FILLER             PIC X.
               05  D-FIELD-PROMPT     PIC X(5).
               05  D-FIELD-NO         PIC ZZ9.
           03  D-PAGENO               PIC ZZ9.
           03  D-END-MORE             PIC X(7).

       01  PAGE-KEY-TABLE.
           03  PAGE-KEY               PIC X(30) OCCURS 600.
       01  CURRENT-PAGE               PIC S999.
       01  PAGE-COUNT                 PIC 999.
       01  MAX-PAGES                  PIC 9(4)  VALUE 600.
       01  MATCH-FOUND                PIC X     VALUE "N".
      *--------------------------------------------
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/PHON400.CPY".

       COPY "LIBGB/FLDSELW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/GETENVW.CPY".

       01  FLINQ-WINDOW-HANDLE      PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBWI/FLINQ_DEF.CPY".
       COPY "LIBWI/FLINQ_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBWI/FLINQ_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE
              FL-FILE.
       COPY "LIBGB/DECLARE.CPY".
           CLOSE
              FL-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ****************************
      *      MAIN PROGRAM MODULE
      ***************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
       COPY "LIBGB/CHKSEC.CPY".
           MOVE " " TO VDU-WIMENU-SELECTION.

           MOVE "FLINQ" TO VDU-FORM-NAME.
           MOVE "REL" TO GETENV-BUF.
           PERFORM GETENV-CALL.
           MOVE GETENV-BUF TO FL-PATH-REL.


           DISPLAY FLOATING GRAPHICAL WINDOW
                   AT  SCREEN LINE FLINQ-BEGIN-X * VDU-WIN-L-OFFSET
                   AT  SCREEN COL  FLINQ-BEGIN-Y * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   SIZE    FLINQ-COLUMNS + VDU-WIN-W-MOD
                   LINES   FLINQ-LINES + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS FLINQ-WINDOW-HANDLE.

           PERFORM FORM-RESET.

      * /USR
           CALL "C$TOLOWER" USING FL-PATH, VALUE 4.

       MAIN-CONT.

           PERFORM REC-INQ-RANGES.
           IF IO-TYPE = "END"
              GO TO END-ROUTINE.
           IF IO-TYPE = "CLR"
              GO TO MAIN-CONT.

           PERFORM INQUIRY-DISPLAY.

           GO TO MAIN-CONT.

      *****************************************
       REC-INQ-RANGES SECTION.
      *****************************************
           MOVE SPACES TO DISPLAY-BUF.

           MOVE DISPLAY-BUF TO WR-BUF.
           MOVE DISPLAY-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE SPACES TO IO-TYPE.

           MOVE 0 TO KEYSUB.

       ENTER-KEYWORD.
           ADD 1 TO KEYSUB.
           IF KEYSUB > 8
              GO TO END-REC-RANGES.
           IF KEYSUB = 1
              MOVE "F7, ENTER SEARCH KEYWORD." TO MESS
           ELSE
              MOVE "F7, RETURN = START SEARCH, ENTER SEARCH KEYWORD."
              TO MESS.
           PERFORM SEND-LEGEND.
           MOVE "E  A000KEYWORDXX." TO VDU.
           INSPECT VDU REPLACING FIRST "XX" BY KEYSUBX.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>"
              MOVE "END" TO IO-TYPE
              GO TO END-REC-RANGES.

           MOVE VDUBUF TO KEYWORD(KEYSUB).
           IF KEYWORD(KEYSUB) = SPACES
              IF KEYSUB > 1
                 GO TO END-REC-RANGES.
           GO TO ENTER-KEYWORD.

       END-REC-RANGES.
           EXIT.

      *****************************************
       INQUIRY-DISPLAY SECTION.
      *****************************************
           MOVE SPACES TO FL1-KEY.

           MOVE FIRST-FIELD TO FLDSEL-FIELD.
           MOVE 1 TO PAGE-COUNT ONE.
           MOVE 0 TO CURRENT-PAGE.
           PERFORM OPEN-FL1-FILE.
           MOVE FL1-KEY TO PAGE-KEY(PAGE-COUNT).

       START-BUILD-PAGENO.
           ADD ONE TO CURRENT-PAGE.
           IF CURRENT-PAGE < 1
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.
           IF CURRENT-PAGE > PAGE-COUNT
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              MOVE 1 TO CURRENT-PAGE.

       CONT-BUILD-PAGENO.
           MOVE SPACES TO DISPLAY-BUF.
           MOVE 1 TO SUB.
           MOVE PAGE-KEY(CURRENT-PAGE) TO FL1-KEY.
           PERFORM START-FL1-FILE.

       BUILD-PAGENO.
           PERFORM READ-FL1-FILE-NEXT.
           IF IO-FG NOT = 0
              GO TO DISPLAY-PAGE.

           IF SUB > 15
              GO TO DISPLAY-PAGE.

           MOVE 1 TO KEYSUB.
           MOVE "N" TO MATCH-FOUND.
       KEYSUB-LOOP.
           IF KEYWORD(KEYSUB) = SPACES
              GO TO BUILD-PAGENO.
           PERFORM VARYING KEYSUB2 FROM 1 BY 1 UNTIL
              KEYSUB2 > 10
              IF FL-KEYWORD(KEYSUB2) = KEYWORD(KEYSUB)
                 MOVE "Y" TO MATCH-FOUND
              END-IF
           END-PERFORM.

           IF MATCH-FOUND = "Y"
              GO TO MOVE-FIELDS.

           ADD 1 TO KEYSUB.
           IF KEYSUB < 7
              GO TO KEYSUB-LOOP.

           GO TO BUILD-PAGENO.

       MOVE-FIELDS.
           MOVE FL-DESC TO D-DESC(SUB).
           IF FL-PROG-1ST5 = "BRMAN"
              MOVE "BRANCH MAINTENANCE" TO D-PROG(SUB)
           ELSE
           IF FL-PROG-1ST5 = "GPMAN"
              MOVE "GLOBAL PARAMETER MAINT" TO D-PROG(SUB)
           ELSE
           IF FL-PROG-1ST5 = "BROPM"
              MOVE "BRANCH OPTION MAINT" TO D-PROG(SUB)
           ELSE
           IF FL-PROG-1ST5 = "GPOPM"
              MOVE "GLOBAL PARAMETER OPTNS" TO D-PROG(SUB)
           ELSE
           IF FL-PROG-1ST5 = "GBMAI"
              MOVE "(LOCAL) GLOBAL MAINT" TO D-PROG(SUB)
           ELSE
           IF FL-PROG-1ST5 = "SPMAN"
              MOVE "STATE PARAMETER MAINT" TO D-PROG(SUB)
           ELSE
              MOVE "???" TO D-PROG(SUB).
           MOVE "PAGE:" TO D-PAGE-PROMPT(SUB).
           MOVE FL-PAGE TO D-PAGE-NO(SUB).
           MOVE "FIELD:" TO D-FIELD-PROMPT(SUB).
           MOVE FL-FIELD-NO TO D-FIELD-NO(SUB).

           ADD 1 TO SUB.
           GO TO BUILD-PAGENO.

       DISPLAY-PAGE.
           IF IO-FG NOT = 0
              IF SUB = 1
                 MOVE "NO RECORDS FOUND!" TO MESS
                 PERFORM SEND-MESS
                 MOVE DISPLAY-BUF TO WR-BUF
                 MOVE DISPLAY-LIST TO WR-LIST
                 PERFORM CALL-WRFORM
                 GO TO END-INQUIRY-MODULE.

           MOVE "MORE..." TO D-END-MORE.
           IF CURRENT-PAGE = PAGE-COUNT
              IF IO-FG = 0
                 IF PAGE-COUNT < MAX-PAGES
                    ADD 1 TO PAGE-COUNT
                    MOVE FL1-KEY TO PAGE-KEY(PAGE-COUNT)
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
           MOVE "^, V, F1,F7-EXIT" TO MESS.
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
              GO TO END-INQUIRY-MODULE.
           IF FLDSEL-STAT = "16"
              MOVE 0 TO CURRENT-PAGE
              MOVE 1 TO ONE
              MOVE FIRST-FIELD TO FLDSEL-FIELD
              GO TO START-BUILD-PAGENO.
           IF NOT (FLDSEL-STAT = "00")
              PERFORM ALARM
              GO TO SELECT-FIELD-FROM-DISPLAY.

           GO TO SELECT-FIELD-FROM-DISPLAY.

       END-INQUIRY-MODULE.
           EXIT.

       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/FLDSEL.CPY".

      ***********************************************
       FORM-RESET SECTION.
      ***********************************************
           MOVE EXT-CONAME-CONAME TO FLINQ-CONAME.
           MOVE EXT-CONAME-SYSDATE TO FLINQ-SYSDATE.
           DISPLAY FLINQ-SCREEN UPON FLINQ-WINDOW-HANDLE.
           MOVE FLINQ-FORM-FIELDS TO WR-BUF.
           MOVE FLINQ-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ***********************************************
       SETUP-LINK-INFO SECTION.
      ***********************************************
           MOVE FLINQ-HELP--FILE TO HELP-LINK-FILE.
           MOVE FLINQ-HELP--DIR TO HELP-LINK-DIR.

      ***********************************************
       VDU-CONVERT-FIELD SECTION.
      ***********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBWI/FLINQ_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ***********************************************
       MISC-PARAGRAPHS SECTION.
      ***********************************************
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ***********************************************
       IO-ROUTINES SECTION.
      ***********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBFL1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      ***********************************************
       END-ROUTINE SECTION.
      ***********************************************
           PERFORM CLOSE-FILES.
       EXIT-PROG.
       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY FLINQ-SCREEN.
           DESTROY FLINQ-WINDOW-HANDLE.
           EXIT PROGRAM.
