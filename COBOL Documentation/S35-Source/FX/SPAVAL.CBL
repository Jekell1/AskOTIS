       IDENTIFICATION DIVISION.
       DATE-WRITTEN.    11/26/2013.
      ******************************************************************
      *
      *  DESCRIPTION: SET SP-CASH-AVAIL-PERCENT
      *               
      *=================================================================
      * REV:
      * BAH 131126 WORLD PR# 944
      * BAH 161010 ADDED SET % REGPAY AVAILABLE (SP-PERCENT-REGPAY-AVAIL)
      *            AND ENTER A DCR NUMBER FOR A MEMO, WORLD PR# 1194
      *
      * KEC 2017.0313 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED IF CONDITION TO CHECK HOLD-SM-SEQ FOR 999
      * 20171106 BAH ACTIVATE 8 DIGIT DATES, PD0006
      *
      * KEC 2018.0425 PR#00000 PARADATA A30 JAY & BETH
      *         REMOVED USE OF ENT-GROUP AND/OR GROUP-FLAG.
      *         THIS WAS DONE IN RELATION TO THE EOBUF EXTERNAL ISSUE
      *         WHICH IF NOT BEING SET WILL CAUSE ISSUES FROM THOSE
      *         BYTE POSITIONS COULD BE SET FROM PREVIOUS
      *         PROGRAM WITH EOBUF THAT SET SOMETHING IN THOSE BYTES.
      *
      *  20221025 CHANGED ~~ TO ZZ FOR END-STATE
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
           "@(#)FX/SPAVAL.CBL A30 04/25/18-09:42:33 >".

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
       COPY "LIBLP/LP01SM.CPY".
       COPY "LIBLP/LP01SM_SQL.CPY".
       COPY "LIBLP/LPWSSM.CPY".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  ERRCD                       PIC X(01)     VALUE SPACES.
       01  LEGEND                      PIC X(80)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(07)     VALUE "LEGEND.".

      *-------------------------------------------------------------

       01  TODAYS-DATE       PIC 9(8).
 
       01  MESS-BUF          PIC X(40) VALUE SPACES.

       01  ORIG-CASH         PIC 99.
       01  ORIG-REGPAY       PIC 99.

       01  HOLD-SM1-KEY.
           03  HOLD-SM-SP-KEY.
               05  HOLD-SM-SP-ORGST       PIC X(2).
               05  HOLD-SM-SP-SPRCLASS    PIC 9(3).
               05  HOLD-SM-SP-SUBCLASS    PIC 9(2).
               05  HOLD-SM-SP-LAWCODE     PIC 9(2).
           03  HOLD-SM-DATE               PIC 9(8).
           03  HOLD-SM-SEQ                PIC 9(3).

       01  MEMO-AREA.
           03  MEMO-FIELD            PIC X(12).
           03  FILLER                PIC X VALUE ":".
           03  MEMO-PG-FLD           PIC X(5) VALUE SPACES.
           03  FILLER                PIC X VALUE SPACES.
           03  MEMO-OR-CHAR          PIC X(10).
           03  FILLER REDEFINES MEMO-OR-CHAR.
               05  FILLER            PIC X(2).
               05  MEMO-OR-NUM       PIC ZZZZZZZ9.
           03  FILLER                PIC X     VALUE " ".
           03  FILLER                PIC X(3)  VALUE "TO:".

           03  MEMO-CH-CHAR          PIC X(10).
           03  FILLER REDEFINES MEMO-CH-CHAR.
               05  FILLER            PIC X(2).
               05  MEMO-CH-DATE      PIC 99/99/99.
           03  FILLER REDEFINES MEMO-CH-CHAR.
               05  FILLER            PIC X(2).
               05  MEMO-CH-NUM       PIC ZZZZZZZ9.
           03  FILLER                PIC X.
           03  MEMO-USERID           PIC X(8).
           03  FILLER                PIC X.
           03  MEMO-DCR-TEXT         PIC X(3).
           03  MEMO-DCR              PIC 9(4).

      ******************************************************************
      *
      *     S C R E E N   F I E L D S   
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-STATE       PIC X(2).
           03  END-STATE       PIC X(2).
           03  BEG-CL          PIC 9(2).
           03  END-CL          PIC 9(2).
           03  BEG-SUB         PIC 9(2).
           03  END-SUB         PIC 9(2).
           03  BEG-LAW         PIC 9(2).
           03  END-LAW         PIC 9(2).
           03  AVAIL-PERCENT   PIC 99.
           03  REGPAY-PERCENT  PIC 99.
           03  DCR-NUMBER      PIC 9(4).


      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/PROGIDW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       01  SPAVAL-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBFX/SPAVAL_DEF.CPY".
       COPY "LIBFX/SPAVAL_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBFX/SPAVAL_SCN.CPY".

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

           PERFORM PROCESS-SP-FILE.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE "SPAVAL" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
           COLOR IS VDU-WCOLOR-TCLP,CONTROL VALUE IS VDU-WCTRL-TCLP
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS SPAVAL-WINDOW-HANDLE.

       COPY "LIBGB/CHKSEC.CPY".
           PERFORM FORM-RESET.

           PERFORM ENTER-PARAMETERS.
           IF ( ERRCD NOT = SPACES )
              GO TO INITIALIZATION-EXIT.

           MOVE "PROGRAM IN PROGRESS........................" TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO TODAYS-DATE.
          
       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************

           MOVE SPACES TO ERRCD.

       STATE-01.
           MOVE "E  A000ST1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-STATES.

           IF NOT (VDUSTATUS = "00" OR "1D") GO TO STATE-01.
           MOVE VDUBUF TO BEG-STATE.

      *-----------------------------------------------------------------
       STATE-02.
           MOVE "E  A000ST2." TO VDU.
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
           MOVE "S  A040ST1." TO VDU.
           MOVE " " TO BEG-STATE VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A040ST2." TO VDU.
           MOVE ALL "Z" TO END-STATE.
           CALL "C$TOLOWER" USING END-STATE, VALUE 4.
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
           IF BEG-CL NOT > END-CL GO TO SUB-01.
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
       SUB-01.
           MOVE "ENNN020SUB1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-SUB.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO SUB-01.
           MOVE VDUNUM0 TO BEG-SUB.

      *-----------------------------------------------------------------
       SUB-02.
           MOVE "ENNN020SUB2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO SUB-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO SUB-02.
           MOVE VDUNUM0 TO END-SUB.
           IF BEG-SUB NOT > END-SUB GO TO LAW-01.
           PERFORM INVALID-RANGE.
           GO TO SUB-01.

      *-----------------------------------------------------------------
       ALL-SUB.
           MOVE "SNNN020SUB1." TO VDU.
           MOVE 0 TO BEG-SUB VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020SUB2." TO VDU.
           MOVE 99 TO END-SUB VDUNUM0.
           PERFORM SCREENIO.

       LAW-01.
           MOVE "ENNN020LAW1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-LAW.
           IF VDUSTATUS = "1U" GO TO SUB-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO LAW-01.
           MOVE VDUNUM0 TO BEG-LAW.

      *-----------------------------------------------------------------
       LAW-02.
           MOVE "ENNN020LAW2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO LAW-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO LAW-02.
           MOVE VDUNUM0 TO END-LAW.
           IF BEG-LAW NOT > END-LAW GO TO ENTER-PERCENT.
           PERFORM INVALID-RANGE.
           GO TO LAW-01.

      *-----------------------------------------------------------------
       ALL-LAW.
           MOVE "SNNN020LAW1." TO VDU.
           MOVE 0 TO BEG-LAW VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020LAW2." TO VDU.
           MOVE 99 TO END-LAW VDUNUM0.
           PERFORM SCREENIO.
      *-----------------------------------------------------------------
       ENTER-PERCENT.
           MOVE "EN N020PERCENT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO LAW-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-PERCENT.
           MOVE VDUNUM0 TO AVAIL-PERCENT.

      *-----------------------------------------------------------------
       ENTER-REGPAY-PERCENT.
           MOVE "EN N020REGPAY." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-PERCENT.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-REGPAY-PERCENT.
           MOVE VDUNUM0 TO REGPAY-PERCENT.

       ENTER-DCR.
           MOVE "EN N040DCR." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO ENDIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO ENTER-REGPAY-PERCENT.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-DCR.
           MOVE VDUNUM0 TO DCR-NUMBER.

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

           MOVE BEG-STATE  TO SP-ORGST.
           MOVE BEG-CL TO SP-SPRCLASS.
           MOVE BEG-SUB TO SP-SUBCLASS.
           MOVE BEG-LAW TO SP-LAWCODE.
           PERFORM START-SP1-FILE.

       NEXT-SP.
           PERFORM READ-SP1-FILE-NEXT.
           IF ( IO-FG NOT = 0 )
              GO TO EXIT-PROCESS-SP-FILE.

           IF SP-ORGST > END-STATE
              GO TO EXIT-PROCESS-SP-FILE.

           IF SP-SPRCLASS < BEG-CL OR SP-SPRCLASS > END-CL
              GO TO NEXT-SP.
           IF SP-SUBCLASS < BEG-SUB OR SP-SUBCLASS > END-SUB
              GO TO NEXT-SP.
           IF SP-LAWCODE < BEG-LAW OR SP-LAWCODE > END-LAW
              GO TO NEXT-SP.

      * ONLY FIX NON-EXPIRED SPR
           IF SP-EXPDATE NOT = 0
              IF SP-EXPDATE NOT > TODAYS-DATE
                 GO TO NEXT-SP.

           STRING "PROGRAM IN PROGRESS......SPR: " SP1-KEY
                       DELIMITED BY SIZE INTO MESS-BUF.
           PERFORM SEND-LEGEND.

           MOVE SP-CASH-AVAIL-PERCENT   TO ORIG-CASH.
           MOVE SP-PERCENT-REGPAY-AVAIL TO ORIG-REGPAY.
           MOVE AVAIL-PERCENT TO SP-CASH-AVAIL-PERCENT.
           MOVE REGPAY-PERCENT TO SP-PERCENT-REGPAY-AVAIL.

           PERFORM REWRITE-SP1-FILE.

           MOVE SP-ORGST    TO HOLD-SM-SP-ORGST.
           MOVE SP-SPRCLASS TO HOLD-SM-SP-SPRCLASS.
           MOVE SP-SUBCLASS TO HOLD-SM-SP-SUBCLASS.
           MOVE SP-LAWCODE  TO HOLD-SM-SP-LAWCODE.
           COMPUTE HOLD-SM-DATE = 99999999 - TODAYS-DATE.

           MOVE "CASH AVAIL %"        TO MEMO-FIELD.
           MOVE "18#26"               TO MEMO-PG-FLD.
           MOVE ORIG-CASH             TO MEMO-OR-NUM.
           MOVE SP-CASH-AVAIL-PERCENT TO MEMO-CH-NUM.
           PERFORM WRITE-MEMO.

           MOVE "REGPY AVAIL%"          TO MEMO-FIELD.
           MOVE "23#04"                 TO MEMO-PG-FLD.
           MOVE ORIG-REGPAY             TO MEMO-OR-NUM.
           MOVE SP-PERCENT-REGPAY-AVAIL TO MEMO-CH-NUM.
           PERFORM WRITE-MEMO.

           GO TO NEXT-SP.

       EXIT-PROCESS-SP-FILE.
           EXIT.

      *************************************************
       WRITE-MEMO SECTION.
      *************************************************
           MOVE EXT-CONAME-USER-ID    TO MEMO-USERID.
           MOVE "DCR"                 TO MEMO-DCR-TEXT.
           MOVE DCR-NUMBER            TO MEMO-DCR.

           MOVE 1 TO HOLD-SM-SEQ.
       UPDATE-MEMO-AGAIN.
           MOVE SPACES TO SM-REC.
           MOVE HOLD-SM1-KEY TO SM1-KEY.
           MOVE "A" TO SM-MEMO-TYPE.
           MOVE MEMO-AREA TO SM-MEMO.

           PERFORM WRITE-SM1-FILE.
           IF IO-FG NOT = 0
              IF E-CODE NOT = "22"
                  GO TO IO-ERROR
              ELSE
                 IF HOLD-SM-SEQ < 999
                    ADD 1 TO HOLD-SM-SEQ
                    GO TO UPDATE-MEMO-AGAIN.

       EXIT-WRITE-MEMO.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPTBNO.CPY".
       COPY "LIBGB/DATER.CPY".

      *******************************************************
       FORM-RESET SECTION.
      *******************************************************
           MOVE EXT-CONAME-CONAME TO SPAVAL-CONAME.
           MOVE EXT-CONAME-SYSDATE TO SPAVAL-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY SPAVAL-SCREEN UPON SPAVAL-WINDOW-HANDLE.
           MOVE SPAVAL-FORM-FIELDS TO WR-BUF.
           MOVE SPAVAL-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *******************************************************
       SETUP-LINK-INFO SECTION.
      *******************************************************
           MOVE SPAVAL-HELP--FILE TO HELP-LINK-FILE.
           MOVE SPAVAL-HELP--DIR  TO HELP-LINK-DIR.

      *******************************************************
       VDU-CONVERT-FIELD SECTION.
      *******************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBFX/SPAVAL_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ********************
       MISC-PARA SECTION.
      ********************
       COPY "LIBGB/ACCESS.CPY".
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
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
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

       EXIT-PROG.
           MOVE "FX/FXMENU"  TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DESTROY SPAVAL-SCREEN.
           DESTROY SPAVAL-WINDOW-HANDLE.
           EXIT PROGRAM.
