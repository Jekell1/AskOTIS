       IDENTIFICATION DIVISION.
       PROGRAM-ID.    FIXSPR.
       DATE-WRITTEN.  072695.
      ******************************************************************
      *
      * IDENTIFICATION:  FX/FIXSPR.C
      *
      * DESCRIPTION   :  FIX TO ALTER SPRS VIA. CUSTOM CHANGES AS NEEDED
      *
      *-----------------------------------------------------------------
      * REV:
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.
      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)FX/FIXSPR.CBL S34 05/24/21-11:54:23 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  ERRCD                       PIC X(01)     VALUE SPACES.
       01  LEGEND                      PIC X(80)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(07)     VALUE "LEGEND.".

      *-----------------------------------------------------------------
       01  SCREEN-RANGE-WORKERS.
           03  BEG-STATE               PIC X(02)     VALUE SPACES.
           03  END-STATE               PIC X(02)     VALUE SPACES.
      *-------------------------------------------------------------


      *=================================================================
      *
      *    C O M M E N T - W O R K E R S
      *
      *=================================================================
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

      *-----------------------------------------------------------------
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
      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/PROGIDW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       01  FIXSPR-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBFX/FIXSPR_DEF.CPY".
       COPY "LIBFX/FIXSPR_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBFX/FIXSPR_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

           PERFORM INITIALIZATION.

           IF ( ERRCD NOT = SPACES )
              GO TO MAIN-PROGRAM-EXIT.

           PERFORM FIX-SP-FILE.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE "FIXSPR" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS FIXSPR-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".
           PERFORM FORM-RESET.

           PERFORM DISPLAY-COMMENTS.

           PERFORM ENTER-PARAMETERS.
           IF ( ERRCD NOT = SPACES )
              GO TO INITIALIZATION-EXIT.

           MOVE "PROGRAM IN PROGRESS........................" TO LEGEND.
           PERFORM SEND-LEGEND.
           PERFORM INITIAL-PROGRESS-INDICATOR.

       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARAMETERS-BEGIN.

           MOVE SPACES TO ERRCD.

      *=================================================================
       ENTER-BEG-STATE.

           MOVE "E  A020BEG-STATE." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F3-KEY ) GO TO ENTER-ALL-STATE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-STATE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-STATE.

      *-----------------------------------------------------------------
       ENTER-END-STATE.

           MOVE "E  A020END-STATE." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-STATE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-STATE.

           IF ( BEG-STATE > END-STATE )
              MOVE "INVALID RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-STATE.

           GO TO ENTER-ACCEPT-YN.

      *-----------------------------------------------------------------
       ENTER-ALL-STATE.

           MOVE "S  A020BEG-STATE." TO VDU.
           MOVE SPACES              TO VDUBUF BEG-STATE.
           PERFORM SCREENIO.

           MOVE "S  A020END-STATE." TO VDU.
           MOVE "ZZ"                TO VDUBUF END-STATE.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-ACCEPT-YN.

           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-ACCEPT-YN.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUBUF = "Y" ) GO TO ENTER-PARAMETERS-END.
           IF ( VDUBUF = "N" ) GO TO ENTER-PARAMETERS-BEGIN.

           GO TO ENTER-ACCEPT-YN.

      *=================================================================
       ENTER-PARAMETERS-END.

           IF ( F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF ( F7-KEY ) MOVE "X" TO ERRCD.

       ENTER-PARAMETERS-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-COMMENTS SECTION.
      ******************************************************************

           MOVE "> SP-LC-INT-ON-REPO-NON-PL           <" TO COMMENT-01.
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

       DISPLAY-COMMENTS-EXIT.
           EXIT.

      ******************************************************************
       FIX-SP-FILE SECTION.
      ******************************************************************

           PERFORM OPEN-SP1-FILE.

           MOVE BEG-STATE TO SP-ORGST.
           MOVE ZEROS     TO SP-SPRCLASS.
           MOVE ZEROES    TO SP-SUBCLASS.
           MOVE ZEROES    TO SP-LAWCODE.
           PERFORM START-SP1-FILE.

       FIX-SP-FILE-NEXT.
           PERFORM READ-SP1-FILE-NEXT.
           IF ( IO-BAD               ) OR
              ( SP-ORGST > END-STATE )
              GO TO FIX-SP-FILE-EXIT.

           PERFORM SEND-PROGRESS-INDICATOR.

      * CHANGES TO 'SP'  

           MOVE 2 TO SP-DEFPOLMX(1).
           MOVE 4 TO SP-DEFPOLMX(2).

           PERFORM REWRITE-SP1-FILE.

           GO TO FIX-SP-FILE-NEXT.

       FIX-SP-FILE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/PROGID.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPTBNO.CPY".

      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           MOVE EXT-CONAME-CONAME TO FIXSPR-CONAME.
           MOVE EXT-CONAME-SYSDATE TO FIXSPR-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY FIXSPR-SCREEN UPON FIXSPR-WINDOW-HANDLE.
           MOVE FIXSPR-FORM-FIELDS TO WR-BUF.
           MOVE FIXSPR-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE FIXSPR-HELP--FILE TO HELP-LINK-FILE.
           MOVE FIXSPR-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBFX/FIXSPR_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
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

      *-----------------------------------------------------------------
       SEND-PROGRESS-INDICATOR.

           MOVE ZEROES TO PROGID-BRANCH.
           MOVE SPACES TO PROGID-TIME.
           PERFORM PROGID-DISPLAY.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-SP1-FILE.
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPSP1IN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           MOVE "FX/FXMENU"  TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY FIXSPR-SCREEN.
           DESTROY FIXSPR-WINDOW-HANDLE.
           EXIT PROGRAM.
