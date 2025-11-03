       IDENTIFICATION DIVISION.
       PROGRAM-ID.    FXBRBL.
       DATE-WRITTEN.  20160923.
      ******************************************************************
      * IDENTIFICATION:  FX/FXBRBL.C
      *
      * DESCRIPTION   :  FIX TO ALTER BRANCH FILE:
      *                       SET BR-BALLOON-LOANS-FG = "Y"
      *
      * REV:
      * MJD 160923 NEW PROGRAM #1191
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./FX/FXBRBL.CBL A30 09/23/16-09:43:57 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

       01  ERRCD                       PIC X(01)     VALUE SPACES.
       01  LEGEND                      PIC X(80)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(07)     VALUE "LEGEND.".
       01  BEG-BRANCH              PIC 9(04)     VALUE 0.
       01  END-BRANCH              PIC 9(04)     VALUE 0.
       01  FXBRBL-WINDOW-HANDLE PIC X(10).

       01  COMMENT-BUF.
           03  COMMENT-01              PIC X(38)     VALUE SPACES.
           03  COMMENT-02              PIC X(38)     VALUE SPACES.
           03  COMMENT-03              PIC X(38)     VALUE SPACES.
           03  COMMENT-04              PIC X(38)     VALUE SPACES.
           03  COMMENT-05              PIC X(38)     VALUE SPACES.
           03  COMMENT-06              PIC X(38)     VALUE SPACES.
           03  COMMENT-07              PIC X(38)     VALUE SPACES.
           03  COMMENT-08              PIC X(38)     VALUE SPACES.
           03  COMMENT-09              PIC X(38)     VALUE SPACES.
           03  COMMENT-10              PIC X(38)     VALUE SPACES.
           03  COMMENT-11              PIC X(38)     VALUE SPACES.
           03  COMMENT-12              PIC X(38)     VALUE SPACES.
           03  COMMENT-13              PIC X(38)     VALUE SPACES.
           03  COMMENT-14              PIC X(38)     VALUE SPACES.
           03  COMMENT-15              PIC X(38)     VALUE SPACES.
           03  COMMENT-16              PIC X(38)     VALUE SPACES.
           03  COMMENT-17              PIC X(38)     VALUE SPACES.
           03  COMMENT-18              PIC X(38)     VALUE SPACES.
           03  COMMENT-19              PIC X(38)     VALUE SPACES.
           03  COMMENT-20              PIC X(38)     VALUE SPACES.
           03  COMMENT-21              PIC X(38)     VALUE SPACES.
           03  COMMENT-22              PIC X(38)     VALUE SPACES.
           03  COMMENT-23              PIC X(38)     VALUE SPACES.
           03  COMMENT-24              PIC X(38)     VALUE SPACES.
           03  COMMENT-25              PIC X(38)     VALUE SPACES.
           03  COMMENT-26              PIC X(38)     VALUE SPACES.
           03  COMMENT-27              PIC X(38)     VALUE SPACES.
           03  COMMENT-28              PIC X(38)     VALUE SPACES.
           03  COMMENT-29              PIC X(38)     VALUE SPACES.
           03  COMMENT-30              PIC X(38)     VALUE SPACES.

       01  COMMENT-LIST.
           03  FILLER    PIC X(22)     VALUE "COMMENT-01 COMMENT-02 ".
           03  FILLER    PIC X(22)     VALUE "COMMENT-03 COMMENT-04 ".
           03  FILLER    PIC X(22)     VALUE "COMMENT-05 COMMENT-06 ".
           03  FILLER    PIC X(22)     VALUE "COMMENT-07 COMMENT-08 ".
           03  FILLER    PIC X(22)     VALUE "COMMENT-09 COMMENT-10 ".
           03  FILLER    PIC X(22)     VALUE "COMMENT-11 COMMENT-12 ".
           03  FILLER    PIC X(22)     VALUE "COMMENT-13 COMMENT-14 ".
           03  FILLER    PIC X(22)     VALUE "COMMENT-15 COMMENT-16 ".
           03  FILLER    PIC X(22)     VALUE "COMMENT-17 COMMENT-18 ".
           03  FILLER    PIC X(22)     VALUE "COMMENT-19 COMMENT-20 ".
           03  FILLER    PIC X(22)     VALUE "COMMENT-21 COMMENT-22 ".
           03  FILLER    PIC X(22)     VALUE "COMMENT-23 COMMENT-24 ".
           03  FILLER    PIC X(22)     VALUE "COMMENT-25 COMMENT-26 ".
           03  FILLER    PIC X(22)     VALUE "COMMENT-27 COMMENT-28 ".
           03  FILLER    PIC X(22)     VALUE "COMMENT-29 COMMENT-30.".

       COPY "LIBGB/PROGIDW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBFX/FXBRBL_DEF.CPY".
       COPY "LIBFX/FXBRBL_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBFX/FXBRBL_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      *******************************************************************
       MAIN-PROGRAM SECTION.
      *******************************************************************

           PERFORM SQL-CONNECT.

           PERFORM INITIALIZATION.
           IF ERRCD = SPACES
              PERFORM FIX-BR-FILE.

           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE "FXBRBL" TO VDU-FORM-NAME.
           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS FXBRBL-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".
           PERFORM FORM-RESET.

           PERFORM DISPLAY-COMMENTS.

           PERFORM ENTER-PARAMETERS.
           IF ERRCD NOT = SPACES
              GO TO INITIALIZATION-EXIT.

           MOVE "PROGRAM IN PROGRESS........................" TO LEGEND.
           PERFORM SEND-LEGEND.

           PERFORM INITIAL-PROGRESS-INDICATOR.

       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
      * ENTER-PARAMETERS
      ******************************************************************
       ENTER-PARAMETERS SECTION.
           MOVE SPACES TO ERRCD.

       ENTER-BEG-BRANCH.
           MOVE "E  N040BEG-BRANCH." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO ENTER-BEG-BRANCH.
           IF F3-KEY
              MOVE "SNNN040BEG-BRANCH." TO VDU
              MOVE 0 TO VDUNUM0 BEG-BRANCH
              PERFORM SCREENIO
              MOVE "SNNN040END-BRANCH." TO VDU
              MOVE 9999 TO VDUNUM0 END-BRANCH
              PERFORM SCREENIO
              GO TO ENTER-ACCEPT-YN.
           IF F7-KEY GO TO ENTER-PARAMETERS-END.
           IF  NOT ENTER-KEY  GO TO ENTER-BEG-BRANCH.
           MOVE VDUNUM0 TO BEG-BRANCH.

       ENTER-END-BRANCH.
           MOVE "E  N040END-BRANCH." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO ENTER-BEG-BRANCH.
           IF F7-KEY GO TO ENTER-PARAMETERS-END.
           IF UP-KEY GO TO ENTER-BEG-BRANCH
           IF NOT ENTER-KEY GO TO ENTER-END-BRANCH.
           MOVE VDUNUM0 TO END-BRANCH.

           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.

           GO TO ENTER-ACCEPT-YN.

       ENTER-ACCEPT-YN.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF  F1-KEY GO TO ENTER-BEG-BRANCH.
           IF  F7-KEY GO TO ENTER-PARAMETERS-END.
           IF  NOT ENTER-KEY GO TO ENTER-ACCEPT-YN.
           IF VDUBUF = "Y" GO TO ENTER-PARAMETERS-END.
           IF VDUBUF = "N" GO TO ENTER-BEG-BRANCH.
           GO TO ENTER-ACCEPT-YN.

       ENTER-PARAMETERS-END.
           IF ( F7-KEY ) MOVE "X" TO ERRCD.

      ******************************************************************
      * DISPLAY-COMMENTS
      ******************************************************************
       DISPLAY-COMMENTS SECTION.
           MOVE "> MOVE Y TO BR-BALLOON-LOANS-FG      <" TO COMMENT-01.
           MOVE ">                                    <" TO COMMENT-02.
           MOVE ">                                    <" TO COMMENT-03.
           MOVE ">                                    <" TO COMMENT-04.
           MOVE ">                                    <" TO COMMENT-05.
           MOVE ">                                    <" TO COMMENT-06.
           MOVE ">                                    <" TO COMMENT-07.
           MOVE ">                                    <" TO COMMENT-08.
           MOVE ">                                    <" TO COMMENT-09.
           MOVE ">                                    <" TO COMMENT-10.
           MOVE ">                                    <" TO COMMENT-11.
           MOVE ">                                    <" TO COMMENT-12.
           MOVE ">                                    <" TO COMMENT-13.
           MOVE ">                                    <" TO COMMENT-14.
           MOVE ">                                    <" TO COMMENT-15.
           MOVE ">                                    <" TO COMMENT-16.
           MOVE ">                                    <" TO COMMENT-17.
           MOVE ">                                    <" TO COMMENT-18.
           MOVE ">                                    <" TO COMMENT-19.
           MOVE ">                                    <" TO COMMENT-20.
           MOVE ">                                    <" TO COMMENT-21.
           MOVE ">                                    <" TO COMMENT-22.
           MOVE ">                                    <" TO COMMENT-23.
           MOVE ">                                    <" TO COMMENT-24.
           MOVE ">                                    <" TO COMMENT-25.
           MOVE ">                                    <" TO COMMENT-26.
           MOVE ">                                    <" TO COMMENT-27.
           MOVE ">                                    <" TO COMMENT-28.
           MOVE ">                                    <" TO COMMENT-29.
           MOVE ">                                    <" TO COMMENT-30.
           PERFORM SEND-COMMENT.

      ******************************************************************
      * FIX BR FILE
      ******************************************************************
       FIX-BR-FILE SECTION.
           PERFORM OPEN-BR-FILE.
           MOVE BEG-BRANCH TO BR-NO.
           PERFORM START-BR-FILE.

       FIX-BR-FILE-NEXT.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-BAD OR ( BR-NO > END-BRANCH )
              GO TO FIX-BR-FILE-EXIT.

           PERFORM SEND-PROGRESS-INDICATOR.

           IF BR-BALLOON-LOANS-FG NOT = "Y"
              MOVE "Y" TO BR-BALLOON-LOANS-FG
              PERFORM REWRITE-BR-FILE
              IF IO-BAD GO TO IO-ERROR.

           GO TO FIX-BR-FILE-NEXT.

       FIX-BR-FILE-EXIT.
           EXIT.

      ******************************************************************
      * COPY-LIBRARY-SECTIONS
      ******************************************************************
       COPY "LIBGB/PROGID.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".

       FORM-RESET SECTION.
           MOVE EXT-CONAME-CONAME TO FXBRBL-CONAME.
           MOVE EXT-CONAME-SYSDATE TO FXBRBL-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY FXBRBL-SCREEN UPON FXBRBL-WINDOW-HANDLE.
           MOVE FXBRBL-FORM-FIELDS TO WR-BUF.
           MOVE FXBRBL-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       SETUP-LINK-INFO SECTION.
           MOVE FXBRBL-HELP--FILE TO HELP-LINK-FILE.
           MOVE FXBRBL-HELP--DIR  TO HELP-LINK-DIR.

       VDU-CONVERT-FIELD SECTION.
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBFX/FXBRBL_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       MISCELLANEOUS-PARAGRAPHS SECTION.
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       SEND-COMMENT.
           MOVE COMMENT-BUF TO WR-BUF.
           MOVE COMMENT-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       INITIAL-PROGRESS-INDICATOR.
           MOVE ZEROES                 TO PROGID-BRANCH.
           MOVE "LEGEND:B."            TO PROGID-LINE-LIST.
           MOVE SPACES                 TO PROGID-PRU-LP.
           MOVE SPACES                 TO PROGID-ROUTE-BUF.
           MOVE ZEROES                 TO PROGID-SLEEP-COUNTER.
           MOVE 001                    TO PROGID-SLEEP-COUNTER-MAX.
           MOVE SPACES                 TO PROGID-TIME.
           MOVE "S"                    TO PROGID-TYPE.
           MOVE PROGID-TEXT-PROCESSING TO PROGID-TEXT.

       SEND-PROGRESS-INDICATOR.
           MOVE ZEROES TO PROGID-BRANCH.
           MOVE SPACES TO PROGID-TIME.
           PERFORM PROGID-DISPLAY.

       IO-ROUTINES SECTION.
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-BR-FILE.
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRIN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************************************
      * END-PROGRAM
      ******************************************************************
       END-PROGRAM SECTION.
       END-RUN.
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           MOVE "FX/FXMENU"  TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY FXBRBL-SCREEN.
           DESTROY FXBRBL-WINDOW-HANDLE.
           EXIT PROGRAM.
      * A30 CONVERSION COMPLETED
