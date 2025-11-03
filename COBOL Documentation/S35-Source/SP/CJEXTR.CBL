       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CJEXTR.
       DATE-WRITTEN.    121201.
      **************************************************************************
      *                      (SP)
      *    DESCRIPTION:  COLLECTOR JOURNAL DATA WAREHOUSE EXTRACTION
      *
      * CREATES AN OUTPUT EXTRACT FILE  /USR/EXTRACT/COLLECTOR_JRL
      * RSZ 218 INCLUDING CR,LF
      *
      * REV:
      *            REGIONAL ACCEPTANCE, PR 1675
      * BLV 030207 UPDATE XCJ-QUEUE, REGACC PR# 0023
      *  CS 040907 ADD UPDATE OF XCJ-REGION-NUMBER FROM BR-REGION-NUMBER
      * BLF 050713 EXPAND XCJ-KEY TO 9(8), CENTRIX HAS MORE THAN 9999999
      *            RECORDS, GIVES WRITE ERROR OF 24
      *                                                 [PR#76 REGACC]
      * BAH 150615 EXPANDED XCJ-KEY TO 9(9), WORLD PR# 1088
      * BAH 170815 ACTIVATE 8 DIGIT DATES, PD0006
      *  CS 180530 FIXED XCJ-ACCTNO TO ADD THE 01 SEQ TO MATCH A15
      * BAH 180810 FIXED XCJ-BRACCTNO, WAS TRUNCATING NUMBER
      * BAH 20200827 REMOVED RESET ON FIRST FIELD
      * RMZ 20231003 PERFORMANCE TUNING - ADDED STORED PROCEDURE
      * RMZ 20240116 CHANGED JULIAN-BEG(END,CC) TO EXT-JULIAN-BEG(END,CC)
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBEX/EXFSXCJ.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       COPY "LIBEX/EXFDXCJ.CPY".

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/CJEXTR.CBL S35 12/22/23-11:49:16 >".

      ******************************************************************
      *
      *     F I L E   S P E C S
      *
      ******************************************************************
      ******************************************************************
      * LIBEX/EXWSXCJ: COLLECTION JOURNAL EXTRACTION FILE PATH WORKERS
      ******************************************************************
       01  XCJ-PATH.
           03  XCJ-DIRECTORY      PIC X(13) VALUE "/USR/EXTRACT/".
           03  FILLER             PIC X(13) VALUE "COLLECTOR_JNL".

       COPY "LIBSP/SP01CJ.CPY".
       COPY "LIBSP/SP01CJ_SQL.CPY".
       COPY "LIBSP/SPWSCJ.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
      *----------------------------------------------------------------
       01  XCJ-KEY              PIC 9(9)     VALUE 0.

       01  WS-BRACCTNO.
           03  WS-OWNBR         PIC 9(4).
           03  WS-NUMBER        PIC 9(6).

       01  WORK-ACCOUNT.
           03  WORK-NUMBER      PIC 9(6).
           03  WORK-SEQNO       PIC 99.

       01  CJ-CONN-OPEN-SW      PIC X        VALUE "N".

      ******************************************************************
      *
      *   LINE WORKERS
      *
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99       VALUE 0.
           03  LN-COUNT         PIC 99       VALUE 61.
           03  LN-WK            PIC 99       VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  EXITPROG             PIC X(9)     VALUE "SP/DWEXMU".

       01  STARTING-TIME        PIC X(8)     VALUE SPACES.

      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-BRANCH             PIC 9(4).
           03  END-BRANCH             PIC 9(4).
           03  GROUP-FLAG             PIC X.
           03  ENT-GROUP              PIC X(4).
           03  BEG-DATE               PIC 9(8).
           03  END-DATE               PIC 9(8).

       01  VERSION-CONTROL     PIC X    VALUE "A".

       01  SV-BEG-BR           PIC 9999 VALUE 9999.
       01  SV-END-BR           PIC 9999 VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.


      ******************************************************************
      *
      *     P R I N T   B U F F E R S
      *
      ******************************************************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(31)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(37)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(19)    VALUE " ".
           03  H-TITLE          PIC X(33)    VALUE
           "COLLECTOR JOURNAL FILE EXTRACTION".


      **********************
      *   MISC WORKERS     *
      **********************
       01  FIRST-TIME           PIC X        VALUE "Y".
       01  NEW-BRANCH           PIC X        VALUE "N".
       01  RECORDS-FOUND        PIC X        VALUE " ".
           88  NO-RECORDS-FOUND              VALUE "N".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".
       01  WK-DISPLAY-MESS.
           03  FILLER           PIC X(29)    VALUE
           "BUILDING EXTRACTION FILE....".
           03  DISPLAY-MESS-BR  PIC 9(4).
           03  FILLER           PIC X(10).

       01  RECORD-COUNT           PIC 9(9)      VALUE 0.

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  RANGE-LN REDEFINES DETAIL-LINE.
           03  RANGE-LINE        PIC X(21).
           03  RANGE-SELECTION   PIC X(111).

       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/CJEXTR_DEF.CPY".
       COPY "LIBSP/CJEXTR_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/CJEXTR_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE XCJ-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-CJ1-FILE.
           PERFORM CLOSE-CJ1-CONNECTION.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE
               XCJ-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N   M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.
           PERFORM SQL-CONNECT.
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CJEXTR" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *-----------------------------------------------------------------
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

      *-----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.

      *-----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.

           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           IF EXT-ROUTE-BUF = "00"
              MOVE SPACES TO WR-BUF
              STRING "EXTRACTION IN PROGRESS.........", BR-NO
                     DELIMITED "  " INTO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           MOVE "B" TO CJ-PATH-OVERRIDE.

           PERFORM OPEN-CJ1-FILE.

           IF EXT-ROUTE-BUF = "00"
              MOVE BR-NO TO DISPLAY-MESS-BR
              MOVE WK-DISPLAY-MESS TO MESS
              PERFORM SEND-LEGEND.

           MOVE BR-NO    TO CJ-BRNO
                            QCJ1-WBEG-BRNO
                            QCJ1-WEND-BRNO.
           MOVE SPACES   TO CJ-COLLID
                            QCJ1-WBEG-COLLID.
           MOVE ALL "Z"  TO QCJ1-WEND-COLLID.
           MOVE BEG-DATE TO CJ-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QCJ1-WBEG-DATE.
           MOVE END-DATE TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QCJ1-WEND-DATE.
           
           MOVE 0        TO CJ-START-TIME CJ-SEQNO
                            QCJ1-WBEG-START-TIME
                            QCJ1-WBEG-SEQNO.
           MOVE ALL "9"  TO QCJ1-WEND-START-TIME
                            QCJ1-WEND-SEQNO.

           PERFORM START-CJ1-FILE. *> SSP - CJFILE

       NEXT-CJ.
           PERFORM READ-CJ1-FILE-NEXT. *> EMBEDDED - CJFILE
           IF IO-FG = 9 OR (CJ-BRNO NOT = CJ-PATH-OWNBR)
              GO TO END-OF-GROUP.

           IF CJ-DATE < BEG-DATE OR CJ-DATE > END-DATE
              GO TO NEXT-CJ.

           MOVE SPACES            TO XCJ-REC.

           MOVE BR-REGION-NUMBER  TO XCJ-REGION-NUMBER.
           MOVE CJ-BRNO           TO XCJ-BRNO WS-OWNBR.
           MOVE CJ-ACCTNO         TO WS-NUMBER.
           MOVE WS-BRACCTNO       TO XCJ-BRACCTNO.
      *NEED 01 SEQ TO MATCH A15
           MOVE CJ-ACCTNO         TO WORK-NUMBER.
           MOVE 01                TO WORK-SEQNO.
           MOVE WORK-ACCOUNT      TO XCJ-ACCTNO.

           MOVE CJ-COLLID         TO XCJ-COLLID.
           MOVE CJ-DATE           TO XCJ-DATE.
           MOVE CJ-START-TIME     TO XCJ-START-TIME.
           MOVE CJ-SEQNO          TO XCJ-SEQNO.
           MOVE CJ-END-TIME       TO XCJ-END-TIME.
           MOVE CJ-ACTIVITY-CD    TO XCJ-ACTIVITY-CD.
           MOVE CJ-RESULT-CD      TO XCJ-RESULT-CD.
           MOVE CJ-ACTION-CD      TO XCJ-ACTION-CD.
           MOVE CJ-NEW-COLLID     TO XCJ-NEW-COLLID.
           MOVE CJ-CON-STAT       TO XCJ-CON-STAT.
           MOVE CJ-REC-STAT       TO XCJ-REC-STAT.
           MOVE CJ-FORM-NO        TO XCJ-FORM-NO.
           MOVE CJ-PROMISE-DATE1  TO XCJ-PROMISE-DATE1.
           MOVE CJ-PROMISE-AMT    TO XCJ-PROMISE-AMT.
           MOVE CJ-BEG-LM-SEQ     TO XCJ-BEG-LM-SEQ.
           MOVE CJ-END-LM-SEQ     TO XCJ-END-LM-SEQ.
           MOVE CJ-PROMISE-DATE2  TO XCJ-PROMISE-DATE2.
           MOVE CJ-CUR-BAL        TO XCJ-CUR-BAL.
           MOVE CJ-PAST-DUE       TO XCJ-PAST-DUE.
           MOVE CJ-POSTING-BR     TO XCJ-POSTING-BR.
           MOVE CJ-QUEUE          TO XCJ-QUEUE.

           MOVE X"0D" TO XCJ-CR.
           MOVE X"0A" TO XCJ-LF.

           ADD 1 TO XCJ-KEY.
           PERFORM WRITE-XCJ-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           ADD 1 TO RECORD-COUNT.

           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO NEW-BRANCH.
           MOVE "Y" TO RECORDS-FOUND.

           GO TO NEXT-CJ.

      *----------------------------------------------------------------
       END-OF-GROUP.
           PERFORM CLOSE-CJ1-FILE.

           MOVE "Y" TO FIRST-TIME.

           GO TO NEXT-GROUP-LOC.

      *----------------------------------------------------------------
       END-ROUTINE.

           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.


           PERFORM HEAD-PAGE.
           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE CJEXTR-QUEUE-POS TO Q-POS.
           MOVE CJEXTR-COPIES-POS TO C-POS.
           MOVE CJEXTR-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           CALL "C$TOLOWER" USING XCJ-DIRECTORY, VALUE 13.
           MOVE XCJ-DIRECTORY TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "EXTRACTION DIRECTORY DOES NOT EXIST!" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           PERFORM GET-TIME.
           MOVE TIME-EDIT TO STARTING-TIME.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF EXT-ROUTE-BUF = "70"
              MOVE EXT-EOBUF-MESS TO H-TITLE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BRANCH TO SV-BEG-BR
              MOVE END-BRANCH TO SV-END-BR
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP TO SV-ENT-GROUP
              MOVE EXT-EOBUF-BRNO TO BEG-BRANCH END-BRANCH
              MOVE "N" TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE
           ELSE
              IF ( BEG-BRANCH = 0 AND END-BRANCH = 0 )
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH END-BRANCH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE " " TO EXT-EOBUF-ERRCD.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           IF EXT-ROUTE-BUF = "00"
              MOVE "EXTRACTION IN PROGRESS........." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.

           PERFORM OPEN-XCJ-FILE-OUTPUT.
           PERFORM CLOSE-XCJ-FILE.
           PERFORM OPEN-XCJ-FILE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE.

       EXIT-EO-EXEC-AUDIT.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * DONT GO FURTHER IF NOT A VALID LOGIN
      * NEED USERS BRANCH FOR BR-FILE
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF EXT-CONAME-USER-LOC NOT NUMERIC
              MOVE "INVALID USER" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

      *----------------------------------------------------------------
       BR-01.
           MOVE "E  A000BR1." TO VDU.
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
                 MOVE "S NA000BR1." TO VDU
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
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.

      *----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       BR-02.
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

      *----------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S NA160DISPLAY-USER." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "N" TO GROUP-FLAG.

       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

      *----------------------------------------------------------------
       BEGDT.

           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO EXIT-PARM.

           MOVE "ERNM060BEGDATE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEGDT.

           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.

           IF BEG-DATE = ZERO
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD
              MOVE "S  8000BEGDATE." TO VDU
              MOVE DATE-YYYYMMDD TO BEG-DATE VDU-DATE-YYYYMMDD
              PERFORM SCREENIO.

      *----------------------------------------------------------------
       ENDDT.

           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO BR-01.

           MOVE "ERNM060ENDDATE." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEGDT.

           MOVE VDU-DATE-YYYYMMDD TO END-DATE.

           IF END-DATE = ZERO
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD
              MOVE "S  8000ENDDATE." TO VDU
              MOVE DATE-YYYYMMDD TO END-DATE VDU-DATE-YYYYMMDD
              PERFORM SCREENIO.

       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************

           IF GROUP-FLAG = "Y"
              MOVE "S  A040BR1." TO VDU
              MOVE ENT-GROUP TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNA040BR2." TO VDU
              MOVE SPACES TO VDUNUM0
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BRANCH TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BRANCH TO VDUNUM0
              PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( (BEG-BRANCH = 0) AND (END-BRANCH = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ******************************************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************************************
           MOVE 4 TO LN-FEED.
       
           IF GROUP-FLAG = "Y"
              MOVE "BEGINNING BRANCH   :" TO RANGE-LINE
              MOVE ENT-GROUP              TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH      :" TO RANGE-LINE
              MOVE SPACES                 TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEGINNING BRANCH   :" TO RANGE-LINE
              MOVE BEG-BRANCH             TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH      :" TO RANGE-LINE
              MOVE END-BRANCH             TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE.
       
           MOVE "STARTING TIME      :" TO RANGE-LINE.
           MOVE STARTING-TIME         TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "ENDING   TIME      :" TO RANGE-LINE.
           PERFORM GET-TIME.
           MOVE TIME-EDIT             TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
       
           MOVE "# RECORDS EXTRACTED:" TO RANGE-LINE.
           MOVE RECORD-COUNT TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
       

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************

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

      *----------------------------------------------------------------
       END-HEAD-PAGE.
           MOVE 3 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      ******************************************************************
      *
      *     C O P Y    L I B R A R Y    S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO CJEXTR-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CJEXTR-SYSDATE.
           DISPLAY CJEXTR-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE CJEXTR-FORM-FIELDS TO WR-BUF.
           MOVE CJEXTR-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE CJEXTR-HELP--FILE TO HELP-LINK-FILE.
           MOVE CJEXTR-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/CJEXTR_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARA SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
      *----------------------------------------------------------------
       SEND-LEGEND.
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
       CONDITIONAL-PRINT.
           IF DETAIL-LINE NOT = " "
              MOVE 1 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBSP/SPCJGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBEX/EXXCJI.CPY".

      * COPY "LIBSP/SPCJ1RN.CPY".
      * COPYMEMBER: LIBSP/SPCJ1RN
       LOAD-CJ1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CJ-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CJ-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO CJ-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CJ-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO CJ-ALL-DATA
              MOVE CJ-ALL-PATH             TO CJ-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CJ-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO CJ-ALL-BASE
              MOVE FILPATH-MACHINE         TO CJ-ALL-MACHINE
              MOVE FILPATH-DATA            TO CJ-ALL-DATA
              MOVE CJ-ALL-PATH             TO CJ-PATH.

      *-----------------------------------------------------------------
       OPEN-CJ1-FILE.
           PERFORM LOAD-CJ1-FILE.
           PERFORM OPEN-IT.
           MOVE CJ-PATH-SQL TO E-FILE.

            IF CJ-CONN-OPEN-SW = "N"
              MOVE "Y" TO CJ-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS CJ_CONNECT
                    USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-CJ1-FILE. *> SSP - CJFILE
           PERFORM START-IT.
           MOVE CJ-PATH-OWNBR  TO CJ-BRNO.
           MOVE CJ-PATH-SQL    TO E-FILE.
           MOVE CJ1-KEY        TO E-KEYX.

           IF ( CJ1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-CJ1-FILE.

           IF ( QCJ1-WEND-SEQNO = ZEROES    ) OR
              ( QCJ1-WEND-SEQNO NOT NUMERIC )
              MOVE CJ-BRNO              TO QCJ1-WBEG-BRNO
              MOVE CJ-BRNO              TO QCJ1-WEND-BRNO
              MOVE CJ-COLLID            TO QCJ1-WBEG-COLLID
              MOVE ALL "Z"              TO QCJ1-WEND-COLLID
              MOVE CJ-DATE              TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QCJ1-WBEG-DATE
              MOVE EXT-JULIAN-END       TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QCJ1-WEND-DATE
              MOVE CJ-START-TIME        TO QCJ1-WBEG-START-TIME
              MOVE ALL "9"              TO QCJ1-WEND-START-TIME
              MOVE CJ-SEQNO             TO QCJ1-WBEG-SEQNO
              MOVE ALL "9"              TO QCJ1-WEND-SEQNO
      *- - - -{ ADD ONE TO WHERE SCAN CONTINUE FIELDS
              MOVE 03                   TO SQL-TEXT-SIZE
              MOVE CJ-COLLID            TO SQL-TEXT
              PERFORM SQL-TEXT-INCREMENT
              MOVE SQL-TEXT             TO QCJ1-WSBEG-COLLID

              MOVE CJ-DATE              TO NDTE-DATE
              MOVE 1                    TO NDTE-HOLD
              PERFORM INCREMENT-DAYS
              MOVE NDTE-DATE            TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QCJ1-WSBEG-DATE

              COMPUTE QCJ1-WSBEG-START-TIME = CJ-START-TIME + 1
              IF ( CJ-SEQNO = 99 )
                 MOVE CJ-SEQNO          TO QCJ1-WSBEG-SEQNO
              ELSE
                 COMPUTE QCJ1-WSBEG-SEQNO = CJ-SEQNO + 1.

      *  STORED PROCEDURE:
      *
      *   SELECT CJFILE.CJ_BRNO,
      *          CJFILE.CJ_COLLID, 
      *          CAST(CJFILE.CJ_DATE          AS VARCHAR(10)), 
      *          CJFILE.CJ_START_TIME,
      *          CJFILE.CJ_SEQNO,
      *          CJFILE.CJ_ACCTNO,
      *          CJFILE.CJ_END_TIME,
      *          CJFILE.CJ_ACTIVITY_CD, 
      *          CJFILE.CJ_RESULT_CD, 
      *          CJFILE.CJ_ACTION_CD, 
      *          CJFILE.CJ_NEW_COLLID,
      *          CJFILE.CJ_CON_STAT, 
      *          CJFILE.CJ_REC_STAT, 
      *          CJFILE.CJ_FORM_NO,
      *          CAST(CJFILE.CJ_PROMISE_DATE1 AS VARCHAR(10)), 
      *          CJFILE.CJ_PROMISE_AMT,
      *          CJFILE.CJ_BEG_LM_SEQ,
      *          CJFILE.CJ_END_LM_SEQ,
      *          CAST(CJFILE.CJ_PROMISE_DATE2 AS VARCHAR(10)), 
      *          CJFILE.CJ_CUR_BAL,
      *          CJFILE.CJ_PAST_DUE,
      *          CJFILE.CJ_POSTING_BR,
      *          CJFILE.CJ_QUEUE 
      *  
      *   FROM DBO.CJFILE
      *  
      *   WHERE CJFILE.CJ_BRNO       = @QCJ1_WBEG_BRNO
      *     AND CJFILE.CJ_DATE BETWEEN @QCJ1_WBEG_DATE
      *                            AND @QCJ1_WEND_DATE
      *  
      *   ORDER BY CJFILE.CJ_BRNO,
      *            CJFILE.CJ_COLLID,
      *            CJFILE.CJ_DATE,
      *            CJFILE.CJ_START_TIME,
      *            CJFILE.CJ_SEQNO
 
           EXEC SQL SET CONNECTION CJ_CONNECT END-EXEC.

           EXEC SQL
            DECLARE CJ1_CURSOR CURSOR FOR
                :CJ1-RETURN-CODE = EXEC SSP_CJEXTR_CJFILE_SELECT
              ( :QCJ1-WBEG-BRNO,
                :QCJ1-WBEG-DATE, 
                :QCJ1-WEND-DATE )                    
           END-EXEC.

           EXEC SQL
               OPEN CJ1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QCJ1-WBEG-BRNO 
                          QCJ1-WEND-BRNO
                          QCJ1-WBEG-START-TIME
                          QCJ1-WEND-START-TIME
                          QCJ1-WBEG-SEQNO
                          QCJ1-WEND-SEQNO
                          QCJ1-WSBEG-START-TIME
                          QCJ1-WSBEG-SEQNO.
 
           MOVE SPACES TO QCJ1-WBEG-COLLID
                          QCJ1-WEND-COLLID
                          QCJ1-WBEG-DATE
                          QCJ1-WEND-DATE
                          QCJ1-WSBEG-COLLID
                          QCJ1-WSBEG-DATE.

           MOVE STAT---GOOD TO CJ1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-CJ1-FILE-NEXT. *> EMBEDDED - CJFILE
           PERFORM READ-IT.
           MOVE CJ-PATH-OWNBR  TO CJ-BRNO.
           MOVE CJ-PATH-SQL    TO E-FILE.
           MOVE CJ1-KEY        TO E-KEYX.
           INITIALIZE QCJ-REC.

           EXEC SQL SET CONNECTION CJ_CONNECT END-EXEC.

           IF ( CJ1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH CJ1_CURSOR 
                    INTO 
                     :QCJ-BRNO,
                     :QCJ-COLLID, 
                     :QCJ-DATE, 
                     :QCJ-START-TIME,
                     :QCJ-SEQNO,
                     :QCJ-ACCTNO,
                     :QCJ-END-TIME,
                     :QCJ-ACTIVITY-CD, 
                     :QCJ-RESULT-CD, 
                     :QCJ-ACTION-CD, 
                     :QCJ-NEW-COLLID,
                     :QCJ-CON-STAT, 
                     :QCJ-REC-STAT, 
                     :QCJ-FORM-NO,
                     :QCJ-PROMISE-DATE1, 
                     :QCJ-PROMISE-AMT,
                     :QCJ-BEG-LM-SEQ,
                     :QCJ-END-LM-SEQ,
                     :QCJ-PROMISE-DATE2, 
                     :QCJ-CUR-BAL,
                     :QCJ-PAST-DUE,
                     :QCJ-POSTING-BR,
                     :QCJ-QUEUE 
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CJ1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-CJ-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-CJ1-FILE.

           PERFORM CLOSE-IT.
           IF ( CJ1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION CJ_CONNECT END-EXEC
              EXEC SQL
                   CLOSE CJ1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO CJ1-CURSOR-STAT
              EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-CJ1-CONNECTION.

           IF CJ-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION CJ_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT CJ_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO CJ-CONN-OPEN-SW.


       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

       EXIT-PROG.

           IF SV-BEG-BR NOT = 9999
              IF EXT-EOBUF-GROUP-FG = "Y"
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH
                 MOVE SV-GROUP-FLAG TO GROUP-FLAG
                 MOVE SV-ENT-GROUP TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE EXITPROG TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY CJEXTR-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
