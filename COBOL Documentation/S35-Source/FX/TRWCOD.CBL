       IDENTIFICATION DIVISION.
       PROGRAM-ID.      TRWCOD.
       DATE-WRITTEN.    11/09/2021.
      ******************************************************************
      *
      *  DESCRIPTION:
      *     HARDSHIP DEFERMENTS DUE TO DISASTER/COVID19. ACCOUNTS THAT HAVE
      *     BEEN AFFECTED BY THIS NEED TO REFLECT THAT USING THE "AW"
      *     SPECIAL COMMENT CODE, BUT ONLY FOR THE PERIOD BEING DEFERRED
      *     (1 MONTH). NEED A WAY TO ENSURE THAT THE "AW" CODE IS REMOVED
      *     FROM THE ACCOUNT AFTER REPORTING ONE MONTH.
      *
      *     RUN IN BATCH/EOCRON. DETAIL REPORT TO LIST ACCOUNTS CLEARED.
      *     CREATE MEMO.
      *
      * REV:
      *  20211109 BAH  NEW #1523
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       DATA DIVISION.
       FILE SECTION.
 
       COPY "LIBGB/GBFDPR.CPY".
 
      ******************************************
      *      WORKING STORAGE SECTION
      ******************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)FX/TRWCOD.CBL A32 10/11/19-09:17:14 >".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
      ******************************************
       01  PR-IFN             PIC X(2) VALUE "PR".
 
       01  TOTAL-RECORDS-READ    PIC 9(9).
       01  TOTAL-RECORDS-CHANGED PIC 9(9).

       01  TODAYS-DATE-YYYYMMDD PIC 9(8).

       01  TRWCOD-MEMO.
           03  FILLER                 PIC X(46) VALUE
               "FX/TRWCOD: CLEARED SPECIAL COMMENT CODE, WAS: ".
           03  MEMO-SPECIAL-CODE     PIC X(2).
           03  FILLER                PIC X(3).
           03  MEMO-USERID           PIC X(8).

       01  HOLD-MEMO                  PIC X(60).

       01  HOLD-LM-SEQ                PIC 9(3).

       01  D-MESS.
           03  FILLER        PIC X(9)  VALUE "BRANCH:".
           03  D-NUMBER      PIC Z(6).
 
       01  MESS-LIST         PIC X(5) VALUE "MESS.".
 
      ***********************************
      *     S C R E E N   F I E L D S   *
      ***********************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).
           03  CHANGE-CODE     PIC XX.

       01  VERSION-CONTROL     PIC X VALUE "A".

       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBFX/TRWCOD_DEF.CPY".
       COPY "LIBFX/TRWCOD_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       01  MSEL-LIST            PIC X(5)  VALUE "MSEL.".
       01  LEGEND               PIC X(70).

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(8)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(8)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE:".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(12).
           03  FILLER           PIC X(9)    VALUE " ".
           03  FILLER           PIC X(40) VALUE
           " CLEAR LOAN SPECIAL COMMENT CODE".


       01  DETAIL-LINE          PIC X(80) VALUE SPACES.

       01  DETAIL-LINE1 REDEFINES DETAIL-LINE.
           03  D-BRNO           PIC Z(4).
           03  FILLER           PIC X(2).
           03  D-ACCTNO         PIC Z(8).
           03  FILLER           PIC X(2).
           03  D-CODE           PIC X(2).
           03  FILLER           PIC X(2).
           03  D-TEXT           PIC X(30).

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  RANGE-SELECTION  PIC X(30).
           03  FILLER           PIC X(5).
           03  RANGE1           PIC X(10).
           03  RANGE1N REDEFINES RANGE1 PIC 9(10).
           03  FILLER           PIC X(2).
           03  RANGE2N          PIC 9(10).

       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBFX/TRWCOD_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".
 
      **********************************
       MAIN-PROGRAM SECTION.
      **********************************

           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "TRWCOD" TO VDU-FORM-NAME.

           PERFORM INITIAL-ROUTINE.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-ROUTINE.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.
           IF WGR-SRESULT = 0
              GO TO END-ROUTINE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

       MOVE-BRANCH-PATH-INFO.
           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           MOVE "B" TO LN-PATH-OVERRIDE
                       LM-PATH-OVERRIDE.

      * DISPLAY BRANCH
           MOVE BR-NO    TO D-NUMBER.
           MOVE D-MESS   TO MESS.
           PERFORM SEND-LEGEND.

           PERFORM OPEN-LN1-FILE.
           PERFORM OPEN-LM1-FILE.

           MOVE LN-PATH-OWNBR TO LN-OWNBR.
           MOVE 0             TO LN-ACCTNO.

           PERFORM START-LN1-FILE.

       NEXT-LN.
           PERFORM READ-LN1-FILE-NEXT.
           IF IO-FG = 9 OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-OF-GROUP.

           ADD 1 TO TOTAL-RECORDS-READ.
      * SKIP DELETED LOANS
           IF ( LN-POCD-NONLOAN )
              GO TO NEXT-LN.

      * ONLY INCLUDE CODES THAT MATCH
           IF CHANGE-CODE NOT = LN-TRW-SPECIAL-CODE
              GO TO NEXT-LN.

           MOVE "  " TO LN-TRW-SPECIAL-CODE.
           PERFORM REWRITE-LN1-FILE.

           MOVE CHANGE-CODE        TO MEMO-SPECIAL-CODE.
           MOVE EXT-CONAME-USER-ID TO MEMO-USERID.
           MOVE TRWCOD-MEMO        TO HOLD-MEMO.
           PERFORM CREATE-MEMO.

           MOVE BR-NO       TO D-BRNO.
           MOVE LN-ACCTNO   TO D-ACCTNO.
           MOVE CHANGE-CODE TO D-CODE.
           PERFORM WRITE-DETAIL-LINE.

           ADD 1 TO TOTAL-RECORDS-CHANGED.

           GO TO NEXT-LN.
 
       END-OF-GROUP.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LM1-FILE.

           GO TO NEXT-GROUP-LOC.
       END-ROUTINE.
           PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.

      ***********************************
       INITIAL-ROUTINE SECTION.
      ***********************************
           MOVE TRWCOD-QUEUE-POS TO Q-POS.
           MOVE TRWCOD-COPIES-POS TO C-POS.
           MOVE TRWCOD-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           MOVE " " TO EXT-EOBUF-ERRCD.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           PERFORM OPEN-PR-FILE.

           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.

           ACCEPT TODAYS-DATE-YYYYMMDD FROM CENTURY-DATE.

           MOVE 0 TO TOTAL-RECORDS-READ TOTAL-RECORDS-CHANGED.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ****************************************
       EO-EXEC-AUDIT SECTION.
      ****************************************
           MOVE EXT-EOBUF-DATE2 TO TODAYS-DATE-YYYYMMDD.

      ****************************************
       ENTER-PARAMETERS SECTION.
      ****************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO ENTER-CODE.

       BR-01.
           MOVE "F7-EXIT, F1, F3-ALL, ^, ENTER BEGINNING BRANCH"
                                                     TO LEGEND.
           PERFORM SEND-MSEL.
           MOVE "R  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO BR-01
              ELSE
                 MOVE "S  A000BR1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-01.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.
           IF WGR-WORD-ZERO
              MOVE 0 TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.

           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.
           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           IF WGR-TYPE = "A"
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
       BR-02.
           MOVE "F7-EXIT, F1, ^-BACK, ENTER ENDING BRANCH" TO LEGEND.
           PERFORM SEND-MSEL.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.
           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.

           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           GO TO CHECK-BRANCH-SECURITY.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A160DISPLAY-USER." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.
           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.


       ENTER-CODE.
           MOVE "F7-EXIT, F1, ^-BACK, ENTER CODE TO CLEAR" TO LEGEND.
           PERFORM SEND-MSEL.
           MOVE "E  A020CODE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENTER-CODE.
           MOVE VDUBUF TO CHANGE-CODE.

       ACCEPT-IT.
           MOVE "F7-EXIT, F1, ^-BACK, ACCEPT (Y/N):" TO LEGEND.
           PERFORM SEND-MSEL.

       EXIT-PARM.
           EXIT.

      **********************************************
       DISPLAY-PARAMETERS SECTION.
      **********************************************

           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BRANCH TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BRANCH TO VDUNUM0
              PERFORM SCREENIO.

           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           MOVE "S  A020CODE." TO VDU.
           MOVE CHANGE-CODE TO VDUBUF.
           PERFORM SCREENIO.

      *************************************************
       CREATE-MEMO SECTION.
      *************************************************
           MOVE LN-ACCTNO TO LM-ACCTNO.
           SUBTRACT TODAYS-DATE-YYYYMMDD FROM 99999999 GIVING LM-DATE.
           MOVE "S"       TO LM-MEMO-TYPE.
           MOVE 1         TO HOLD-LM-SEQ.
       WRITE-MEMO.
           MOVE HOLD-MEMO   TO LM-MEMO.
           MOVE HOLD-LM-SEQ TO LM-SEQ.
           PERFORM WRITE-LM1-FILE.
           IF IO-FG NOT = 0
              IF LM-SEQ NOT = 999
                 ADD 1 TO HOLD-LM-SEQ
                 GO TO WRITE-MEMO
              ELSE
                 MOVE BR-NO     TO D-BRNO
                 MOVE LN-ACCTNO TO D-NUMBER
                 MOVE "MAX LINES FOR MEMO REACHED" TO D-TEXT
                 PERFORM WRITE-DETAIL-LINE.

      *******************************
       PRINT-RANGE-LINES SECTION.
      *******************************

           MOVE 2 TO LN-FEED.
           IF GROUP-FLAG = "Y"
              MOVE "BRANCH GROUP" TO RANGE-SELECTION
              MOVE ENT-GROUP TO RANGE1
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEG BR - END BR" TO RANGE-SELECTION
              MOVE BEG-BRANCH TO RANGE1N
              MOVE END-BRANCH TO RANGE2N
              PERFORM WRITE-DETAIL-LINE.

           MOVE "CODE TO CLEAR" TO RANGE-SELECTION.
           MOVE CHANGE-CODE TO RANGE1.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL RECORDS READ" TO RANGE-SELECTION.
           MOVE TOTAL-RECORDS-READ   TO RANGE1N.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL RECORDS CHANGED" TO RANGE-SELECTION.
           MOVE TOTAL-RECORDS-CHANGED   TO RANGE1N.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM WRITE-DETAIL-LINE.

      ****************************
       WRITE-DETAIL-LINE SECTION.
      ****************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *******************************
       HEAD-PAGE SECTION.
      *******************************
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.

           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 4 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      *********************************************
       FORM-RESET SECTION.
      *********************************************
           MOVE EXT-CONAME-CONAME TO TRWCOD-CONAME.
           MOVE EXT-CONAME-SYSDATE TO TRWCOD-SYSDATE.
           DISPLAY TRWCOD-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE TRWCOD-FORM-FIELDS TO WR-BUF.
           MOVE TRWCOD-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           MOVE TRWCOD-HELP--FILE TO HELP-LINK-FILE.
           MOVE TRWCOD-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBFX/TRWCOD_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
    
      ******************************
       MISC-ROUTINES SECTION.
      ******************************
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETPR.CPY".
       SEND-LEGEND.
           MOVE MESS      TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       SEND-MSEL.
           MOVE LEGEND    TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
 
 
      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPLM1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.
 
      *********************************
       END-PROGRAM SECTION.
      *********************************
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE "SP/TWMENU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY TRWCOD-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
