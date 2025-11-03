       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SPRTRW.
       DATE-WRITTEN.    10/23/2019.
      ******************************************************************
      *
      *  DESCRIPTION: SET SP-SKIP-TRW FLAG
      *
      * REV:
      *   20191023 BAH NEW FIX FOR WORLD TO SET SP-SKIP-TRW #1395A
      *   20221025 CHANGED ~~ TO ZZ FOR END-STATE
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
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)FX/SPRTRW A32 10/28/19-08:20:34 >".

       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01SM.CPY".
       COPY "LIBLP/LP01SM_SQL.CPY".
       COPY "LIBLP/LPWSSM.CPY".

      * YYYYMMDD
       01  HOLD-SYSDATE      PIC 9(8).

       01  HOLD-SM1-KEY.
           03  HOLD-SM-SP-KEY.
               05  HOLD-SM-SP-ORGST       PIC X(2).
               05  HOLD-SM-SP-SPRCLASS    PIC 9(3).
               05  HOLD-SM-SP-SUBCLASS    PIC 9(2).
               05  HOLD-SM-SP-LAWCODE     PIC 9(2).
           03  HOLD-SM-DATE               PIC 9(8).
           03  HOLD-SM-SEQ                PIC 9(3).

       01  MEMO-AREA.
           03  FILLER                PIC X(14) VALUE
               "SKIPCB: 23#06 ".
           03  MEMO-OR-CHAR          PIC X.
           03  FILLER                PIC X(4)  VALUE " TO ".
           03  MEMO-CH-CHAR          PIC X.
           03  FILLER                PIC X(12) VALUE ", SPRTRW DCR".
           03  MEMO-DCR              PIC X(4).
           03  FILLER                PIC X(8)     VALUE " ".
           03  MEMO-USERID           PIC X(8).
           03  FILLER                PIC X     VALUE " ".
           03  MEMO-TIME             PIC X(7).


       01  MESS-LIST         PIC X(5)  VALUE "MESS.".
       01  MESS-BUF          PIC X(40) VALUE SPACES.

       01  ERRCD                       PIC X(01)     VALUE SPACES.

      ******************************************************************
      *
      *     S C R E E N   F I E L D S   
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-STATE       PIC X(2).
           03  END-STATE       PIC X(2).
           03  BEG-CL          PIC 9(3).
           03  END-CL          PIC 9(3).
           03  SKIP-TRW        PIC X.
           03  DCR-NUMBER      PIC X(4).


      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETPRW.CPY".

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       01  SPRTRW-WINDOW-HANDLE PIC X(10).

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBFX/SPRTRW_DEF.CPY".
       COPY "LIBFX/SPRTRW_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBFX/SPRTRW_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "SPRTRW" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS SPRTRW-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".
           PERFORM FORM-RESET.

           PERFORM INITIALIZATION.
           IF ( ERRCD NOT = SPACES )
              GO TO MAIN-PROGRAM-EXIT.

           PERFORM PROCESS-SP-FILE.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
      *
      *    I N I T I A L I Z A T I O N
      *
      ******************************************************************
       INITIALIZATION SECTION.

           PERFORM ENTER-PARAMETERS.
           IF ( ERRCD NOT = SPACES )
              GO TO EXIT-INITIALIZE.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO HOLD-SYSDATE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO ERRCD.
          
       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
      *
      *    E N T E R - P A R A M E T E R S
      *
      ******************************************************************
       ENTER-PARAMETERS SECTION.

           IF VDUSTATUS = "1U"
              GO TO ENTER-DCR.

       STATE-01.
           MOVE "E  A020ST1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-STATES.

           IF NOT (VDUSTATUS = "00" OR "1D") GO TO STATE-01.
           MOVE VDUBUF TO BEG-STATE.

      *-----------------------------------------------------------------
       STATE-02.
           MOVE "E  A020ST2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO STATE-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO STATE-02.
           MOVE VDUBUF TO END-STATE.
           IF BEG-STATE NOT > END-STATE GO TO CL-01.
           PERFORM INVALID-RANGE.
           GO TO STATE-01.
      *-----------------------------------------------------------------
       ALL-STATES.
           MOVE "S  A020ST1." TO VDU.
           MOVE " " TO BEG-STATE VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A020ST2." TO VDU.
           MOVE "ZZ" TO END-STATE.
           CALL "C$TOLOWER" USING END-STATE, VALUE 2.
           MOVE END-STATE TO VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       CL-01.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO STATE-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-01.
           MOVE VDUNUM0 TO BEG-CL.

      *-----------------------------------------------------------------
       CL-02.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-02.
           MOVE VDUNUM0 TO END-CL.
           IF BEG-CL NOT > END-CL GO TO SKIP-TRW-FLAG.
           PERFORM INVALID-RANGE.
           GO TO CL-01.

      *-----------------------------------------------------------------
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CL VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CL VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       SKIP-TRW-FLAG.
           MOVE "E  A010SKIPTRW." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT-PARM.
           IF VDUSTATUS = "10" GO TO STATE-01.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO SKIP-TRW-FLAG.
           MOVE VDUBUF TO SKIP-TRW.
           IF NOT (SKIP-TRW = "Y" OR "N")
              GO TO SKIP-TRW-FLAG.

      *-----------------------------------------------------------------
       ENTER-DCR.
           MOVE "E  A040DCR." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT-PARM.
           IF VDUSTATUS = "10" GO TO STATE-01.
           IF VDUSTATUS = "1U" GO TO SKIP-TRW-FLAG.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-DCR.
           MOVE VDUBUF TO DCR-NUMBER.

      *-----------------------------------------------------------------
       INIT-ACCEPT-ACCEPT.
           MOVE "E  A010ACCEPT." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO ENDIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-DCR.
           IF VDUSTATUS NOT = "00" GO TO INIT-ACCEPT-ACCEPT.
           IF VDUBUF = "N" GO TO STATE-01.
           IF VDUBUF NOT = "Y" GO TO INIT-ACCEPT-ACCEPT.

           GO TO EXIT-PARM.

       ENDIT-PARM.
           MOVE "X" TO ERRCD.

       EXIT-PARM.
           EXIT.


      *****************************
       PROCESS-SP-FILE SECTION.
      *****************************

           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-SM1-FILE.

           MOVE BEG-STATE TO SP-ORGST.
           MOVE BEG-CL    TO SP-SPRCLASS.
           MOVE 0         TO SP-SUBCLASS SP-LAWCODE.
           PERFORM START-SP1-FILE.

       NEXT-SP.
           PERFORM READ-SP1-FILE-NEXT.
           IF ( IO-FG NOT = 0 )
              GO TO EXIT-PROCESS-SP-FILE.

           IF SP-ORGST > END-STATE
              GO TO EXIT-PROCESS-SP-FILE.

           IF SP-SPRCLASS < BEG-CL OR SP-SPRCLASS > END-CL
              GO TO NEXT-SP.

           IF SKIP-TRW = SP-SKIP-TRW
              GO TO NEXT-SP.

           STRING "PROGRAM IN PROGRESS......SPR: " SP1-KEY
                       DELIMITED BY SIZE INTO MESS-BUF.
           PERFORM SEND-LEGEND.

           MOVE SP-SKIP-TRW     TO MEMO-OR-CHAR.
           MOVE SKIP-TRW        TO SP-SKIP-TRW MEMO-CH-CHAR.
           PERFORM REWRITE-SP1-FILE.

           MOVE SP-ORGST    TO HOLD-SM-SP-ORGST.
           MOVE SP-SPRCLASS TO HOLD-SM-SP-SPRCLASS.
           MOVE SP-SUBCLASS TO HOLD-SM-SP-SUBCLASS.
           MOVE SP-LAWCODE  TO HOLD-SM-SP-LAWCODE.
           COMPUTE HOLD-SM-DATE = 99999999 - HOLD-SYSDATE.

           MOVE DCR-NUMBER TO MEMO-DCR.
           PERFORM GET-TIME.
           MOVE TIME-EDIT TO MEMO-TIME.

           MOVE EXT-CONAME-USER-ID TO MEMO-USERID.
           MOVE 1 TO HOLD-SM-SEQ.

       UPDATE-SM-FILE-AGAIN.
           MOVE SPACES       TO SM-REC.

           MOVE HOLD-SM1-KEY TO SM1-KEY.
           MOVE "A"          TO SM-MEMO-TYPE.
           MOVE MEMO-AREA    TO SM-MEMO.
           PERFORM WRITE-SM1-FILE.
           IF IO-FG NOT = 0
              IF E-CODE NOT = "22"
                  GO TO IO-ERROR
              ELSE
                 IF HOLD-SM-SEQ < 999
                    ADD 1 TO HOLD-SM-SEQ
                    GO TO UPDATE-SM-FILE-AGAIN.

           GO TO NEXT-SP.

       EXIT-PROCESS-SP-FILE.
           EXIT.


       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPTBNO.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           MOVE EXT-CONAME-CONAME TO SPRTRW-CONAME.
           MOVE EXT-CONAME-SYSDATE TO SPRTRW-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY SPRTRW-SCREEN UPON SPRTRW-WINDOW-HANDLE.
           MOVE SPRTRW-FORM-FIELDS TO WR-BUF. 
           MOVE SPRTRW-FORM-LIST   TO WR-LIST.
           PERFORM CALL-WRFORM. 


      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE SPRTRW-HELP--FILE TO HELP-LINK-FILE.
           MOVE SPRTRW-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBFX/SPRTRW_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ********************
       MISC-PARA SECTION.
      ********************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".
      *-----------------------------------------------------------------
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
       SEND-LEGEND.
           MOVE MESS-BUF  TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      **********************
       IO-ROUTINES SECTION.
      **********************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/DECLARE_SQL.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-SM1-FILE.
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPSMGS_SQL.CPY".
       COPY "LIBLP/LPSP1IN.CPY".
       COPY "LIBLP/LPSM1IN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************************************
      *
      *    E N D - P R O G R A M
      *
      ******************************************************************
       END-PROGRAM SECTION.
       END-RUN.
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE "FX/FXMENU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
           DESTROY SPRTRW-SCREEN
           DESTROY SPRTRW-WINDOW-HANDLE.
           EXIT PROGRAM.
