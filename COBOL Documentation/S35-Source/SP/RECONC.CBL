       IDENTIFICATION DIVISION.
       PROGRAM-ID. RECONC.
      ********************************************************************
      *                   (SP)
      *       GL IS NO LONGER UPDATED TO STATS/RATING SHEET, AND
      *       ADJUSTING JE'S ARE NO LONGER MADE IN OTIS, ACCOUNTING NEEDS
      *       A SUMMARY TO RECONCILE GL 3508 - NONFILE RECOVERY.
      *
      *       THIS IS A SUMMARY LINE BY STATE FOR EACH GT-EXPLANATION,
      *       GROUP, BATCHABLE, CAN ENTER WHAT GL# TO RECONCILE
      *
      * DATE RATE MM/DD/YY TO MM/DD/YY
      *  EXPLANATION            AMOUNT
      *
      *    EOD-TRANS JRNL        -4633.75
      *    EOD-CHECKS            -3144.93
      *    TOTALS FOR "AL"       XXXXXXXX
      *
      * REV:
      *  20200708 BAH NEW #1447
      *  20230515 SMR CHANGED GBGR1IN TO GBGR1RN
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBGB/GBFSWKI.CPY".

       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      ***************************************************
      *    INDEXED WORK FILE
      ***************************************************
       FD  WK-FILE
           LABEL RECORD IS STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-STATE           PIC XX.
               05  WK-EXPL            PIC X(32).
           03  WK-AMT                 PIC S9(10)V99 COMP-3.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./SP/RECONC.CBL S35 05/15/23-08:57:24 >".

       COPY "LIBGL/GL01GT.CPY".
       COPY "LIBGL/GL01GT_SQL.CPY".
       COPY "LIBGL/GLWSGT.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".

       01  PR-IFN                      PIC X(3)      VALUE "PR".
       01  WK-IFN                      PIC X(3)      VALUE "WK".

       01  LN-WORKERS.
           03  LN-FEED        PIC 999   VALUE 0.
           03  LN-COUNT       PIC 999   VALUE 99.
           03  LN-WK          PIC 999   VALUE 0.
       01  PAGE-NUMBER        PIC 999   VALUE 0.

       01  HOLD-STATE         PIC XX    VALUE SPACES.
       01  STATE-TOTAL        PIC S9(10)V99 COMP-3 VALUE +0.
       01  GRAND-TOTAL        PIC S9(13)V99 COMP-3 VALUE +0.

       01  FIRST-TIME         PIC X VALUE "Y".
       01  RECORDS-FOUND      PIC X VALUE " ".
           88  NO-RECORDS-FOUND     VALUE "N".

       01  MESS-LIST          PIC X(5) VALUE "MESS.".

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-DATE               PIC 9(8).
           03  END-DATE               PIC 9(8).
           03  BEG-BR                 PIC 9(4).
           03  END-BR                 PIC 9(4).
           03  BEG-GLNO               PIC 9(7).
           03  GROUP-FLAG             PIC X.
           03  ENT-GROUP              PIC X(4).

       01  VERSION-CONTROL    PIC X VALUE "A".

       01  SV-BEG-BR          PIC 9(4)   VALUE 9999.
       01  SV-END-BR          PIC 9(4)   VALUE 9999.
       01  SV-GROUP-FLAG      PIC X      VALUE SPACES.
       01  SV-ENT-GROUP       PIC X(4)   VALUE SPACES.

       01  HEAD-1.
           03  HD-DATE        PIC X(8).
           03  FILLER         PIC X(3)    VALUE SPACES.
           03  HD-TIME        PIC X(8)    VALUE SPACES.
           03  FILLER         PIC X(30)   VALUE SPACES.
           03  HD-CONAME      PIC X(40).
           03  FILLER         PIC X(34)   VALUE SPACES.
           03  FILLER         PIC X(5)    VALUE "PAGE ".
           03  HD-PAGE        PIC ZZ9.

       01  HEAD-2.
           03  FILLER         PIC X(9)    VALUE "USERID:".
           03  HD-USERID      PIC X(10)   VALUE SPACES.
           03  FILLER         PIC X(30)   VALUE SPACES.
           03  HD-TITLE       PIC X(30)   VALUE
               "GL RECONCILIATION SUMMARY FOR ".
           03  HD-GLNUM       PIC Z(9).

       01  HEAD-3.
           03  FILLER         PIC X(44) VALUE SPACES.
           03  FILLER         PIC X(17) VALUE
               "BEGINNING DATE:  ".
           03  HD-BEG-DATE    PIC 99/99/99.
           03  FILLER         PIC X(2)  VALUE SPACES.
           03  FILLER         PIC X(13) VALUE "ENDING DATE  ".
           03  HD-END-DATE    PIC 99/99/99.

       01  HEAD-4.
           03  FILLER         PIC X(40)   VALUE "EXPLANATION ".
           03  FILLER         PIC X(40)   VALUE "SUM OF AMOUNT".

       01  DETAIL-LINE        PIC X(80)   VALUE SPACES.

       01  D-LINE1     REDEFINES DETAIL-LINE.
           03  D-EXPL         PIC X(32).
           03  FILLER         PIC X(5).
           03  D-AMT          PIC ZZZ,ZZZ,ZZ9.99-.

       01  TOTAL-LINE  REDEFINES DETAIL-LINE.
           03  FILLER         PIC X(11).
           03  TOT-STATE      PIC XX.
           03  FILLER         PIC X(24).
           03  TOT-AMT        PIC ZZZ,ZZZ,ZZ9.99-.

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  RANGE-DESC         PIC X(33).
           03  RANGE-SEL1         PIC X(20).
           03  FILLER REDEFINES RANGE-SEL1.
               05  RANGE-DATE1    PIC 9999/99/99.
               05  FILLER         PIC X(10).
           03  FILLER             PIC X(5).
           03  RANGE-SEL2         PIC X(40).
           03  FILLER REDEFINES RANGE-SEL2.
               05  RANGE-DATE2    PIC 9999/99/99.
               05  FILLER         PIC X(30).
           03  FILLER             PIC X(34).


       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/RECONC_DEF.CPY".
       COPY "LIBSP/RECONC_WKS.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/RECONC_SCN.CPY".


       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
              WK-FILE PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-GT2-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK-FILE.

       COPY "LIBGB/DECLRP2.CPY".

      ***********************************************************
       MAIN-PROGRAM SECTION.
      ***********************************************************
           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "RECONC" TO VDU-FORM-NAME.

      D    STOP MESS.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "Y"
              MOVE ENT-GROUP TO GR-ID
              PERFORM SEARCH-GR-INIT.

           PERFORM CREATE-GT-WORK.

           PERFORM CLOSE-GT2-FILE.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           MOVE SPACES TO HOLD-STATE.
           MOVE 2 TO LN-FEED.

           MOVE LOW-VALUES TO WK-KEY.
           PERFORM START-WK-FILE.

       NEXT-WK-RECORD.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-BAD
              IF HOLD-STATE NOT = SPACES
                 MOVE "TOTALS FOR" TO DETAIL-LINE
                 MOVE HOLD-STATE   TO TOT-STATE
                 MOVE STATE-TOTAL  TO TOT-AMT
                 PERFORM WRITE-DETAIL-LINE
                 GO TO END-ROUTINE
              ELSE
                 GO TO END-ROUTINE.

           IF HOLD-STATE NOT = WK-STATE
              IF HOLD-STATE = SPACES
                 MOVE WK-STATE TO HOLD-STATE
              ELSE
                 MOVE "TOTALS FOR" TO DETAIL-LINE
                 MOVE HOLD-STATE   TO TOT-STATE
                 MOVE STATE-TOTAL  TO TOT-AMT
                 PERFORM WRITE-DETAIL-LINE
                 MOVE 0        TO STATE-TOTAL
                 MOVE WK-STATE TO HOLD-STATE
                 MOVE 2 TO LN-FEED.


           MOVE WK-EXPL TO D-EXPL.
           MOVE WK-AMT  TO D-AMT.
           PERFORM WRITE-DETAIL-LINE.

           ADD WK-AMT TO STATE-TOTAL GRAND-TOTAL.

           GO TO NEXT-WK-RECORD.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM.

           PERFORM PRINT-GRAND-TOTALS.

           IF EXT-EOBUF-GROUP-FG NOT = "Y"
              PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.


      **************************************************
       CREATE-GT-WORK SECTION.
      **************************************************
       NEXT-GT-RECORD.
           PERFORM READ-GT2-FILE-NEXT.
           IF IO-BAD
              GO TO EXIT-CREATE-GT-WORK.

           IF GT-DATE1 > END-DATE
              GO TO EXIT-CREATE-GT-WORK.

      * TEST G/L ACCOUNT NUMBER
           IF GT-ACCTNO NOT = BEG-GLNO 
              GO TO NEXT-GT-RECORD.

      * TEST BRANCH RANGE USING REF CODE (LP-YYMMDD-BBBB)
           IF GROUP-FLAG = "Y"
              MOVE GT-BRANCH TO WGR-SRESULT
              PERFORM SEARCH-GR-VERIFY
              IF WGR-ERR > 0
                 GO TO NEXT-GT-RECORD.
           IF GROUP-FLAG NOT = "Y"
              IF GT-BRANCH < BEG-BR OR GT-BRANCH > END-BR
                 GO TO NEXT-GT-RECORD.

      * NEED BRANCH FOR THE STATE
           MOVE GT-BRANCH TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO NEXT-GT-RECORD.

           PERFORM CREATE-WK-RECORD.

           GO TO NEXT-GT-RECORD.

       EXIT-CREATE-GT-WORK.
           PERFORM CLOSE-GT2-FILE.


      *******************************************
       HEAD-PAGE SECTION.
      *******************************************
           MOVE "N" TO FIRST-TIME.

           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO HD-PAGE.
           PERFORM GET-TIME.
           MOVE TIME-EDIT TO HD-TIME.

           MOVE HEAD-1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD-2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD-3 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD-4 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE SPACES TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE 5 TO LN-COUNT.
           MOVE 2 TO LN-FEED.


      *******************************************
       WRITE-DETAIL-LINE SECTION.
      *******************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.


      *******************************************
       PRINT-GRAND-TOTALS SECTION.
      *******************************************

           MOVE "GRAND TOTAL" TO DETAIL-LINE.
           MOVE GRAND-TOTAL   TO  TOT-AMT.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.



      *******************************************
       INITIALIZATION SECTION.
      *******************************************
           MOVE RECONC-QUEUE-POS TO Q-POS.
           MOVE RECONC-COPIES-POS TO C-POS.
           MOVE RECONC-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           MOVE "G/L RECONCILIATION SUMMARY IN PROGRESS..." TO WR-BUF.
           MOVE MESS-LIST                                   TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE BEG-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO HD-BEG-DATE.
           MOVE END-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO HD-END-DATE.

           MOVE EXT-CONAME-CONAME  TO HD-CONAME.
           MOVE EXT-CONAME-SYSDATE TO HD-DATE.
           MOVE EXT-CONAME-USER-ID TO HD-USERID.
           MOVE BEG-GLNO           TO HD-GLNUM.

           IF EXT-ROUTE-BUF = "70"
              MOVE EXT-EOBUF-MESS TO HD-TITLE.

           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BR     TO SV-BEG-BR
              MOVE END-BR     TO SV-END-BR
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP  TO SV-ENT-GROUP
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE
              IF EXT-EOBUF-BRNO NUMERIC
                 MOVE EXT-EOBUF-BRNO TO BEG-BR END-BR
                 MOVE "N" TO GROUP-FLAG
              ELSE
                 MOVE EXT-EOBUF-GROUP TO ENT-GROUP
                 MOVE 0 TO BEG-BR
                 MOVE 9999 TO END-BR
                 MOVE "Y" TO GROUP-FLAG
              END-IF
           ELSE
              IF ( BEG-BR = 0 AND END-BR = 0 )
                 MOVE BEG-BR TO SV-BEG-BR
                 MOVE END-BR TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BR END-BR.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-GT2-FILE.

           IF BEG-DATE = 0
              MOVE 19000101 TO BEG-DATE.
           IF END-DATE = 0
              MOVE 19000101 TO END-DATE.

           MOVE BEG-DATE TO GT-DATE1 SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE
           MOVE SQL-DATE-YYYY-MM-DD TO QGT2-WBEG-DATE1.

           MOVE END-DATE TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE
           MOVE SQL-DATE-YYYY-MM-DD TO QGT2-WEND-DATE1.

           PERFORM START-GT2-FILE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      *******************************************
       EO-EXEC-AUDIT SECTION.
      *******************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE.

      *******************************************
       ENTER-PARAMETERS SECTION.
      *******************************************

           IF UP-KEY
              GO TO GET-BEG-GLNO.

       GET-BDT.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO GET-BEG-BR.

           MOVE "ERNM060BEGDATE." TO VDU
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY
              GO TO GET-BDT.
           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.
       GET-EDT.
           MOVE "ERNM060ENDDATE." TO VDU
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO GET-BDT.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY
              GO TO GET-EDT.
           MOVE VDU-DATE-YYYYMMDD TO END-DATE.

       GET-BEG-BR.
           MOVE "E RA000BEGBR." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF F3-KEY GO TO ALL-BRS.
           IF UP-KEY GO TO GET-EDT.
           IF F7-KEY GO TO EXIT-PARM.

           IF F6-KEY
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO GET-BEG-BR
              ELSE
                 MOVE "S  A000BEGBR." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF
                 PERFORM SCREENIO
                 MOVE GRSCAN-KEY TO VDUBUF
                 MOVE "00" TO VDUSTATUS.
           IF NOT ENTER-KEY
              GO TO GET-BEG-BR.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF WGR-WORD-ZERO
              MOVE 0 TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.

           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.
           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO GET-BEG-BR.
           IF WGR-TYPE = "A"
              MOVE "S  A000ENDBR." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BR
              MOVE 9999 TO END-BR
              GO TO GET-BEG-GLNO.

       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BR.

           MOVE "SN N040BEGBR." TO VDU.
           MOVE BEG-BR TO VDUNUM0.
           PERFORM SCREENIO.

        GET-END-BR.
           MOVE "ENNN040ENDBR." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF F3-KEY GO TO ALL-BRS.
           IF UP-KEY GO TO GET-BEG-BR.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO GET-END-BR.
           MOVE VDUNUM0 TO END-BR.
           IF BEG-BR > END-BR
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO GET-BEG-BR.

           GO TO GET-BEG-GLNO.
       ALL-BRS.
           MOVE "SNNN040BEGBR." TO VDU.
           MOVE 1 TO BEG-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040ENDBR." TO VDU.
           MOVE 9999 TO END-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

       GET-BEG-GLNO.
           MOVE "ENNN070BEGACT." TO VDU.
           PERFORM SCREENIO.
           IF F1-KEY GO TO EXIT-PARM.
           IF UP-KEY GO TO GET-END-BR.
           IF F7-KEY GO TO EXIT-PARM.
           IF NOT ENTER-KEY GO TO GET-BEG-GLNO.
           MOVE VDUNUM0 TO BEG-GLNO.

       EXIT-PARM.
           EXIT.

      *******************************************
       DISPLAY-PARAMETERS SECTION.
      *******************************************

           IF GROUP-FLAG = "Y"
              MOVE "S  A000BEGBR." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000ENDBR." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BEGBR." TO VDU
              MOVE BEG-BR TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040ENDBR." TO VDU
              MOVE END-BR TO VDUNUM0
              PERFORM SCREENIO.

           MOVE "SNNN070BEGACT." TO VDU.
           MOVE BEG-GLNO TO VDUNUM0.
           PERFORM SCREENIO.

      *******************************************
       CREATE-WK-RECORD SECTION.
      *******************************************

           MOVE BR-STATE TO WK-STATE.
           MOVE GT-EXPL  TO WK-EXPL.
           PERFORM READ-WK-FILE.
           IF IO-FG = 0
              ADD GT-AMT TO WK-AMT
              PERFORM REWRITE-WK-FILE
              GO TO EXIT-CREATE-WK-REC.

           MOVE BR-STATE TO WK-STATE.
           MOVE GT-EXPL  TO WK-EXPL.
           MOVE GT-AMT   TO WK-AMT.
           PERFORM WRITE-WK-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

       EXIT-CREATE-WK-REC.
           EXIT.

      ********************************************
       PRINT-RANGE-LINES SECTION.
      ********************************************
           MOVE "BEG/ENDING DATE RANGE          :" TO RANGE-DESC.
           MOVE BEG-DATE                           TO RANGE-DATE1.
           MOVE END-DATE                           TO RANGE-DATE2.
           MOVE 5 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF GROUP-FLAG = "Y"
              MOVE "GROUP SELECTED                 :" TO RANGE-DESC
              MOVE ENT-GROUP                          TO RANGE-SEL1
           ELSE
              MOVE "BEG/ENDING BRANCH RANGE        :" TO RANGE-DESC
              MOVE BEG-BR                             TO RANGE-SEL1
              MOVE END-BR                             TO RANGE-SEL2.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ACCOUNT NUMBER                    :" TO RANGE-DESC.
           MOVE BEG-GLNO                            TO RANGE-SEL1.
           PERFORM WRITE-DETAIL-LINE.

       END-RANGE.
           EXIT.

      **********************************************************
       FORM-RESET SECTION.
      **********************************************************
           MOVE EXT-CONAME-CONAME TO RECONC-CONAME.
           MOVE EXT-CONAME-SYSDATE TO RECONC-SYSDATE.
           DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN.
           DISPLAY RECONC-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE RECONC-FORM-FIELDS TO WR-BUF.
           MOVE RECONC-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      **********************************************************
       SETUP-LINK-INFO SECTION.
      **********************************************************
           MOVE RECONC-HELP--FILE TO HELP-LINK-FILE.
           MOVE RECONC-HELP--DIR  TO HELP-LINK-DIR.

      **********************************************************
       VDU-CONVERT-FIELD SECTION.
      **********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/RECONC_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      **********************************************************
       MISC SECTION.
      **********************************************************
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/MKTEMP.CPY".

      *********************************
       IO-ROUTINES SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGL/GLGTGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGL/GLGT2RN.CPY".
       COPY "LIBGB/FERRORS.CPY".

      *********************************
       END-PROGRAM SECTION.
      *********************************
       END-RUN.
           PERFORM CLOSE-FILES.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.
       EXIT-PROG.

           IF SV-BEG-BR NOT = 9999
              IF EXT-EOBUF-GROUP-FG = "Y"
                 MOVE SV-BEG-BR TO BEG-BR
                 MOVE SV-END-BR TO END-BR
                 MOVE SV-GROUP-FLAG TO GROUP-FLAG
                 MOVE SV-ENT-GROUP TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BR TO BEG-BR
                 MOVE SV-END-BR TO END-BR.

           IF EXT-ROUTE-BUF = "00"
              MOVE "SP/WRLDMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN |MJD001
              DESTROY RECONC-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
