       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CQEXTR.
       DATE-WRITTEN.    101209.
      **************************************************************************
      *                      (SP)
      *    DESCRIPTION:  COLLECTOR QUEUE DATA WAREHOUSE EXTRACTION
      *
      * CREATES AN OUTPUT EXTRACT FILE  /USR/EXTRACT/COL_QUEUE 166 BYTES
      * AND                             /USR/EXTRACT/GLB_QUEUE 170 BYTES
      *
      * REV:
      *             REGIONAL ACCEPTANCE, PR 257
      *  BAH 150615 EXPANDED XCQ-KEY AND XGQ-KEY TO 9(9) FROM 9(8),
      *                                                     WORLD PR# 1088
      *   CS 180531 ADD 01 SEQNO TO XCQ-SEQNO TO MATCH A15
      *  BAH 20200827 REMOVED RESET ON FIRST FIELD
      *  RMZ 20231005 PERFORMANCE TUNING - ADDED STORED PROCEDURE
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBEX/EXFSXCQ.CPY".
       COPY "LIBEX/EXFSXGQ.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       COPY "LIBEX/EXFDXCQ.CPY".
       COPY "LIBEX/EXFDXGQ.CPY".

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/CQEXTR.CBL S35 11/22/23-12:57:26 >".

      ******************************************************************
      *
      *     F I L E   S P E C S
      *
      ******************************************************************
       01  XCQ-PATH.
           03  XCQ-DIRECTORY      PIC X(13) VALUE "/USR/EXTRACT/".
           03  FILLER             PIC X(9) VALUE "COL_QUEUE".
       01  XGQ-PATH.
           03  XGQ-DIRECTORY      PIC X(13) VALUE "/USR/EXTRACT/".
           03  FILLER             PIC X(9) VALUE "GLB_QUEUE".
       COPY "LIBSP/SP01CQ.CPY".
       COPY "LIBSP/SP01CQ_SQL.CPY".
       COPY "LIBSP/SPWSCQ.CPY".
       COPY "LIBSP/SP01GQ.CPY".
       COPY "LIBSP/SP01GQ_SQL.CPY".
       COPY "LIBSP/SPWSGQ.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       
       01  XCQ-KEY              PIC 9(9)     VALUE 0.
       01  XGQ-KEY              PIC 9(9)     VALUE 0.

       01  WS-BRACCTNO.
           03  WS-OWNBR         PIC 9(4).
           03  WS-NUMBER        PIC 9(6).

       01  WORK-ACCOUNT.
           03  WORK-NUMBER      PIC 9(6).
           03  WORK-SEQNO       PIC 99.
       01  WORK-ACCTNO REDEFINES WORK-ACCOUNT PIC 9(8).

       01  CQ-CONN-OPEN-SW      PIC X        VALUE "N".
       01  GQ-CONN-OPEN-SW      PIC X        VALUE "N".

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

       01  STARTING-TIME        PIC X(8)     VALUE SPACES.

      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
       
           03  BEG-BRANCH             PIC 9(4).
           03  END-BRANCH             PIC 9(4).
           03  EXTRACT-CQ-FG          PIC X.
           03  EXTRACT-GQ-FG          PIC X.
           03  GROUP-FLAG             PIC X.
           03  ENT-GROUP              PIC X(4).

       01  VERSION-CONTROL     PIC X    VALUE "A".

       01  SV-BEG-BR           PIC 9999 VALUE 9999.
       01  SV-END-BR           PIC 9999 VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.

       
       COPY "LIBGB/PASSWDW.CPY".
       

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

       01  XCQ-COUNT            PIC 9(9)      VALUE 0.
       01  XGQ-COUNT            PIC 9(9)      VALUE 0.

       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  RANGE-LN REDEFINES DETAIL-LINE.
           03  RANGE-LINE        PIC X(21).
           03  RANGE-SELECTION   PIC X(111).

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
       COPY "LIBSP/CQEXTR_DEF.CPY".
       COPY "LIBSP/CQEXTR_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/CQEXTR_SCN.CPY".

       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE XCQ-FILE XGQ-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-GQ1-FILE.
           PERFORM CLOSE-CQ1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE
               XCQ-FILE XGQ-FILE.
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
           MOVE "CQEXTR" TO FORM-PROG VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           IF EXTRACT-CQ-FG = "N"
              GO TO EXTRACT-GQ-FILE.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

       
       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.

           IF WGR-SRESULT = 0
              GO TO EXTRACT-GQ-FILE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.

           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

       
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO EXTRACT-GQ-FILE.

       
       MOVE-BRANCH-PATH-INFO.

           IF EXT-ROUTE-BUF = "00"
              MOVE SPACES TO WR-BUF
              STRING "EXTRACTION IN PROGRESS...........", BR-NO
                     DELIMITED "  " INTO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.
   

           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           MOVE "B"  TO CQ-PATH-OVERRIDE.

           PERFORM OPEN-CQ1-FILE.

           IF PRU-LP NOT = "VDU" AND NOT = "PRU"
              IF EXT-ROUTE-BUF = "00"
                 MOVE BR-NO TO DISPLAY-MESS-BR
                 MOVE WK-DISPLAY-MESS TO MESS
                 PERFORM SEND-LEGEND.

           MOVE BR-NO   TO CQ-BRNO
                           QCQ1-WBEG-BRNO
                           QCQ1-WEND-BRNO.
           MOVE 0       TO CQ-ACCTNO
                           QCQ1-WBEG-ACCTNO.
           MOVE ALL "9" TO QCQ1-WEND-ACCTNO.
      D    STOP MESS.

           PERFORM START-CQ1-FILE. *> SSP - CQFILE
       
       NEXT-CQ.
           PERFORM READ-CQ1-FILE-NEXT. *> EMBEDDED - CQFILE
           IF IO-FG = 9 OR (CQ-BRNO NOT = CQ-PATH-OWNBR)
              GO TO END-OF-GROUP.

      * CQFILE NOW ONLY HOLDS THE "D" RECORDS (DETAIL)
      * CQSFILE HOLDS THE SUMMARY/HEADER INFO
      *    IF CQ-TYPE NOT = "D"
      *       GO TO NEXT-CQ.

           MOVE SPACES               TO XCQ-REC.
           MOVE BR-NO                TO WS-OWNBR.
      *    MOVE CQ-ACCTNO            TO WORK-ACCTNO.
      *    MOVE WORK-NUMBER          TO WS-NUMBER XCQ-NUMBER.
           MOVE CQ-ACCTNO            TO WS-NUMBER XCQ-NUMBER.
           MOVE 01                   TO XCQ-SEQNO.
           MOVE WS-BRACCTNO          TO XCQ-BRACCTNO.
           MOVE CQ-COLLID            TO XCQ-COLLID.
           MOVE CQ-REV-BEHAVIOR-RISK TO XCQ-REV-BEHAVIOR-RISK.
           MOVE CQ-SEQ-FG            TO XCQ-SEQ-FG.
           MOVE CQ-DATE              TO XCQ-DATE.
           MOVE CQ-START-TIME        TO XCQ-START-TIME.
           MOVE CQ-REV-DEL           TO XCQ-REV-DEL.
           MOVE CQ-REV-PDUE-DAYS     TO XCQ-REV-PDUE-DAYS.
           MOVE CQ-REV-BAL           TO XCQ-REV-BAL.
           MOVE CQ-WORK-COLLID       TO XCQ-WORK-COLLID.
           MOVE CQ-WORKED-FG         TO XCQ-WORKED-FG.
           MOVE CQ-IN-PROCESS        TO XCQ-IN-PROCESS.
           MOVE CQ-DATE-QUEUED       TO XCQ-DATE-QUEUED.
           MOVE CQ-TIME-ZONE-ERROR   TO XCQ-TIME-ZONE-ERROR.
           MOVE X"0D"                TO XCQ-CR.
           MOVE X"0A"                TO XCQ-LF.

           ADD 1 TO XCQ-KEY.
           PERFORM WRITE-XCQ-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           ADD 1 TO XCQ-COUNT.

           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO NEW-BRANCH.
           MOVE "Y" TO RECORDS-FOUND.

           GO TO NEXT-CQ.

       
       END-OF-GROUP.
           PERFORM CLOSE-CQ1-FILE.

           MOVE "Y" TO FIRST-TIME.

           GO TO NEXT-GROUP-LOC.

       
       EXTRACT-GQ-FILE.
           PERFORM OPEN-GQ1-FILE.

           IF PRU-LP NOT = "VDU" AND NOT = "PRU"
              MOVE "EXTRACTING GLOBAL QUEUE" TO MESS
              PERFORM SEND-LEGEND.

           MOVE BR-NO   TO GQ-BRNO
                           QGQ1-WBEG-BRNO
                           QGQ1-WEND-BRNO.
           MOVE 0       TO GQ-ACCTNO
                           QGQ1-WBEG-ACCTNO.
           MOVE ALL "9" TO QGQ1-WEND-ACCTNO.
           PERFORM START-GQ1-FILE. *> SSP - GQFILE
       
       NEXT-GQ.
           PERFORM READ-GQ1-FILE-NEXT. *> EMBEDDED - GQFILE
           IF IO-FG = 9
              GO TO END-ROUTINE.

      * GQFILE ONLY CONTAINS THE DETAIL RECORDS
      *    IF GQ-TYPE NOT = "D"
      *       GO TO NEXT-GQ.

           IF GROUP-FLAG = "N"
              IF (GQ-BRNO < BEG-BRANCH OR GQ-BRNO > END-BRANCH)
                 GO TO NEXT-GQ
              END-IF
           ELSE
              MOVE GQ-BRNO TO WGR-SRESULT
              PERFORM SEARCH-GR-VERIFY
              IF WGR-ERR NOT = 0
                 GO TO NEXT-GQ.

       FILL-GQ-WORK-FLDS.
           MOVE SPACES               TO XGQ-REC.
           MOVE GQ-BRNO              TO XGQ-BRNO WS-OWNBR.
      *    MOVE GQ-ACCTNO            TO WORK-ACCTNO.
      *    MOVE WORK-NUMBER          TO WS-NUMBER XGQ-NUMBER.
           MOVE GQ-ACCTNO            TO WS-NUMBER XGQ-NUMBER.
      *    MOVE WORK-SEQNO           TO XGQ-SEQNO.
           MOVE 01                   TO XGQ-SEQNO.
           MOVE WS-BRACCTNO          TO XGQ-BRACCTNO.
           MOVE GQ-COLLID            TO XGQ-COLLID.
           MOVE GQ-SEQ-FG            TO XGQ-SEQ-FG.
           MOVE GQ-DATE              TO XGQ-DATE.
           MOVE GQ-START-TIME        TO XGQ-START-TIME.
           MOVE GQ-REV-DEL           TO XGQ-REV-DEL.
           MOVE GQ-REV-PDUE-DAYS     TO XGQ-REV-PDUE-DAYS.
           MOVE GQ-REV-BAL           TO XGQ-REV-BAL.
           MOVE GQ-WORK-COLLID       TO XGQ-WORK-COLLID.
           MOVE GQ-WORKED-FG         TO XGQ-WORKED-FG.
           MOVE GQ-IN-PROCESS        TO XGQ-IN-PROCESS.
           MOVE GQ-DATE-QUEUED       TO XGQ-DATE-QUEUED.
           MOVE GQ-TIME-ZONE-ERROR   TO XGQ-TIME-ZONE-ERROR.
           MOVE X"0D"                TO XGQ-CR.
           MOVE X"0A"                TO XGQ-LF.

           ADD 1 TO XGQ-KEY.
           PERFORM WRITE-XGQ-FILE.
           IF IO-FG = 9
              GO TO IO-ERROR.

           ADD 1 TO XGQ-COUNT.

           MOVE "N" TO FIRST-TIME.
           MOVE "Y" TO RECORDS-FOUND.

           GO TO NEXT-GQ.

       END-ROUTINE.

           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM.

           PERFORM HEAD-PAGE.
           PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE CQEXTR-QUEUE-POS TO Q-POS.
           MOVE CQEXTR-COPIES-POS TO C-POS.
           MOVE CQEXTR-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           CALL "C$TOLOWER" USING XCQ-DIRECTORY, VALUE 13.
           MOVE XCQ-DIRECTORY TO DIR-ACCESS-BUF.
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

           MOVE " " TO EXT-EOBUF-ERRCD.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           IF EXT-ROUTE-BUF = "00"
              MOVE "EXTRACTION IN PROGRESS..........." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.

           IF EXTRACT-CQ-FG = "Y"
              PERFORM OPEN-XCQ-FILE-OUTPUT
              PERFORM CLOSE-XCQ-FILE
              PERFORM OPEN-XCQ-FILE.

           CALL "C$TOLOWER" USING XGQ-DIRECTORY, VALUE 13.
           IF EXTRACT-GQ-FG = "Y"
              PERFORM OPEN-XGQ-FILE-OUTPUT
              PERFORM CLOSE-XGQ-FILE
              PERFORM OPEN-XGQ-FILE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           EXIT.

       EXIT-EO-EXEC-AUDIT.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.

      * DONT GO FURTHER IF NOT A VALID LOGIN
      * NEED USERS BRANCH FOR BR-FILE
           IF EXT-CONAME-USER-LOC NOT NUMERIC
              MOVE "INVALID USER" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

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

       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.

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

       EXTRACT-CQ-YN.
           MOVE "E  A010CQ." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO EXTRACT-CQ-YN.
           MOVE VDUBUF TO EXTRACT-CQ-FG.
           IF EXTRACT-CQ-FG NOT = "Y"
              IF EXTRACT-CQ-FG NOT = "N"
                 GO TO EXTRACT-CQ-YN.

       EXTRACT-GQ-YN.
           MOVE "E  A010GQ." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO EXTRACT-CQ-YN.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO EXTRACT-GQ-YN.
           MOVE VDUBUF TO EXTRACT-GQ-FG.
           IF EXTRACT-GQ-FG NOT = "Y"
              IF EXTRACT-GQ-FG NOT = "N"
                 GO TO EXTRACT-GQ-YN.

           IF EXTRACT-CQ-FG = "N"
              IF EXTRACT-GQ-FG = "N"
                 MOVE "NO FILES SELECTED" TO MESS
                 PERFORM SEND-MESS
                 GO TO EXTRACT-CQ-YN.

       
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

           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
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
           MOVE "# COL QUE EXTRACTED:" TO RANGE-LINE.
           MOVE XCQ-COUNT TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "# GLB QUE EXTRACTED:" TO RANGE-LINE.
           MOVE XGQ-COUNT TO RANGE-SELECTION.
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
           MOVE EXT-CONAME-CONAME TO CQEXTR-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CQEXTR-SYSDATE.
           DISPLAY CQEXTR-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE CQEXTR-FORM-FIELDS TO WR-BUF.
           MOVE CQEXTR-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE CQEXTR-HELP--FILE TO HELP-LINK-FILE.
           MOVE CQEXTR-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/CQEXTR_EVA.CPY".
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
       COPY "LIBSP/SPGQGS_SQL.CPY".
       COPY "LIBSP/SPCQGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBEX/EXXCQI.CPY".
       COPY "LIBEX/EXXGQI.CPY".

      * COPY "LIBSP/SPCQ1RN.CPY".
      * COPYMEMBER: LIBSP/SPCQ1RN
       LOAD-CQ1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CQ-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CQ-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO CQ-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CQ-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO CQ-ALL-DATA
              MOVE CQ-ALL-PATH             TO CQ-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CQ-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO CQ-ALL-BASE
              MOVE FILPATH-MACHINE         TO CQ-ALL-MACHINE
              MOVE FILPATH-DATA            TO CQ-ALL-DATA
              MOVE CQ-ALL-PATH             TO CQ-PATH.

      *-----------------------------------------------------------------
       OPEN-CQ1-FILE.
           PERFORM LOAD-CQ1-FILE.
           PERFORM OPEN-IT.
           MOVE CQ-PATH-SQL TO E-FILE.

            IF CQ-CONN-OPEN-SW = "N"
              MOVE "Y" TO CQ-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS CQ_CONNECT
                    USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-CQ1-FILE. *> SSP - CQFILE
           PERFORM START-IT.
           MOVE CQ-PATH-OWNBR  TO CQ-BRNO.
           MOVE CQ-PATH-SQL    TO E-FILE.
           MOVE CQ1-KEY        TO E-KEYX.

           IF ( CQ1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-CQ1-FILE.

           IF ( QCQ1-WEND-ACCTNO = ZEROES    ) OR
              ( QCQ1-WEND-ACCTNO NOT NUMERIC )
              MOVE CQ-PATH-OWNBR        TO QCQ1-WBEG-BRNO
              MOVE CQ-PATH-OWNBR        TO QCQ1-WEND-BRNO
              MOVE CQ-ACCTNO            TO QCQ1-WBEG-ACCTNO
              MOVE ALL "9"              TO QCQ1-WEND-ACCTNO.

      *  STORED PROCEDURE:
      *
      *   SELECT CQFILE.CQ_BRNO,
      *          CQFILE.CQ_ACCTNO,
      *          CQFILE.CQ_COLLID, 
      *          CQFILE.CQ_REV_BEHAVIOR_RISK,
      *          CQFILE.CQ_SEQ_FG,  
      *          CAST(CQFILE.CQ_DATE         AS VARCHAR(10)), 
      *          CQFILE.CQ_START_TIME,
      *          CQFILE.CQ_REV_DEL,
      *          CQFILE.CQ_REV_PDUE_DAYS,
      *          CQFILE.CQ_REV_BAL,
      *          CQFILE.CQ_WORK_COLLID, 
      *          CQFILE.CQ_WORKED_FG, 
      *          CQFILE.CQ_IN_PROCESS, 
      *          CAST(CQFILE.CQ_DATE_QUEUED  AS VARCHAR(10)), 
      *          CQFILE.CQ_TIME_ZONE_ERROR 
      *     FROM DBO.CQFILE  CQFILE
      *    WHERE ( CQFILE.CQ_BRNO   = @QCQ1_WBEG_BRNO )  
      *    ORDER BY CQFILE.CQ_BRNO,
      *             CQFILE.CQ_ACCTNO

           EXEC SQL SET CONNECTION CQ_CONNECT END-EXEC.

           EXEC SQL
            DECLARE CQ1_CURSOR CURSOR FOR
                :CQ1-RETURN-CODE = EXEC SSP_CQEXTR_CQFILE_SELECT
              ( :QCQ1-WBEG-BRNO )                    
           END-EXEC.

           EXEC SQL
               OPEN CQ1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QCQ1-WBEG-BRNO 
                          QCQ1-WEND-BRNO
                          QCQ1-WBEG-ACCTNO
                          QCQ1-WEND-ACCTNO.
 
           MOVE STAT---GOOD TO CQ1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-CQ1-FILE-NEXT. *> EMBEDDED - CQFILE
           PERFORM READ-IT.
           MOVE CQ-PATH-OWNBR  TO CQ-BRNO.
           MOVE CQ-PATH-SQL    TO E-FILE.
           MOVE CQ1-KEY        TO E-KEYX.
           INITIALIZE QCQ-REC.

           EXEC SQL SET CONNECTION CQ_CONNECT END-EXEC.

           IF ( CQ1-CURSOR-STAT-GOOD )
              EXEC SQL
                FETCH CQ1_CURSOR 
                  INTO  
                   :QCQ-BRNO,
                   :QCQ-ACCTNO,
                   :QCQ-COLLID, 
                   :QCQ-REV-BEHAVIOR-RISK,
                   :QCQ-SEQ-FG,  
                   :QCQ-DATE, 
                   :QCQ-START-TIME,
                   :QCQ-REV-DEL,
                   :QCQ-REV-PDUE-DAYS,
                   :QCQ-REV-BAL,
                   :QCQ-WORK-COLLID, 
                   :QCQ-WORKED-FG, 
                   :QCQ-IN-PROCESS, 
                   :QCQ-DATE-QUEUED, 
                   :QCQ-TIME-ZONE-ERROR 
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CQ1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-CQ-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-CQ1-FILE.

           PERFORM CLOSE-IT.
           IF ( CQ1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION CQ_CONNECT END-EXEC
              EXEC SQL
                   CLOSE CQ1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO CQ1-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-CQ1-CONNECTION.

           IF CQ-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION CQ_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT CQ_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO CQ-CONN-OPEN-SW.


      * COPY "LIBSP/SPGQ1RN.CPY".
      * COPYMEMBER: LIBSP/SPGQ1RN
       LOAD-GQ1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( GQ-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( GQ-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO GQ-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO GQ-ALL-MACHINE
              MOVE GQ-ALL-PATH             TO GQ-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( GQ-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO GQ-ALL-BASE
              MOVE FILPATH-MACHINE         TO GQ-ALL-MACHINE
              MOVE GQ-ALL-PATH             TO GQ-PATH.

      *-----------------------------------------------------------------
       OPEN-GQ1-FILE.
           PERFORM LOAD-GQ1-FILE.
           PERFORM OPEN-IT.
           MOVE GQ-PATH-SQL TO E-FILE.

            IF GQ-CONN-OPEN-SW = "N"
              MOVE "Y" TO GQ-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS GQ_CONNECT
                    USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-GQ1-FILE. *> SSP - GQFILE
           PERFORM START-IT.
           MOVE GQ-PATH-SQL TO E-FILE.
           MOVE GQ1-KEY TO E-KEYX.

           IF ( GQ1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-GQ1-FILE.

           IF ( QGQ1-WEND-ACCTNO = ZEROES    ) OR
              ( QGQ1-WEND-ACCTNO NOT NUMERIC )
              MOVE GQ-BRNO              TO QGQ1-WBEG-BRNO
              MOVE ALL "9"              TO QGQ1-WEND-BRNO
              MOVE GQ-ACCTNO            TO QGQ1-WBEG-ACCTNO
              MOVE ALL "9"              TO QGQ1-WEND-ACCTNO.


      *  STORED PROCEDURE:
      *
      *    SELECT GQFILE.GQ_BRNO,
      *           GQFILE.GQ_ACCTNO,
      *           GQFILE.GQ_COLLID, 
      *           GQFILE.GQ_SEQ_FG, 
      *           CAST(GQFILE.GQ_DATE        AS VARCHAR(10)), 
      *           GQFILE.GQ_START_TIME,
      *           GQFILE.GQ_REV_DEL,
      *           GQFILE.GQ_REV_PDUE_DAYS,
      *           GQFILE.GQ_REV_BAL,
      *           GQFILE.GQ_WORK_COLLID, 
      *           GQFILE.GQ_WORKED_FG, 
      *           GQFILE.GQ_IN_PROCESS, 
      *           CAST(GQFILE.GQ_DATE_QUEUED AS VARCHAR(10)), 
      *           GQFILE.GQ_TIME_ZONE_ERROR  
      *      FROM DBO.GQFILE  GQFILE
      *     WHERE  ( GQFILE.GQ_BRNO   = @QGQ1_WBEG_BRNO )  
      *     ORDER BY GQFILE.GQ_BRNO,
      *              GQFILE.GQ_ACCTNO

           EXEC SQL SET CONNECTION GQ_CONNECT END-EXEC.

           EXEC SQL
            DECLARE GQ1_CURSOR CURSOR FOR
                :GQ1-RETURN-CODE = EXEC SSP_CQEXTR_GQFILE_SELECT
              ( :QGQ1-WBEG-BRNO )                    
           END-EXEC.

           EXEC SQL
               OPEN GQ1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QGQ1-WBEG-BRNO
                          QGQ1-WEND-BRNO
                          QGQ1-WBEG-ACCTNO
                          QGQ1-WEND-ACCTNO.

           MOVE STAT---GOOD TO GQ1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-GQ1-FILE-NEXT. *> EMBEDDED - GQFILE
           PERFORM READ-IT.
           MOVE GQ-PATH-SQL TO E-FILE.
           MOVE GQ1-KEY TO E-KEYX.
           INITIALIZE QGQ-REC.

           EXEC SQL SET CONNECTION GQ_CONNECT END-EXEC.

           IF ( GQ1-CURSOR-STAT-GOOD )
              EXEC SQL
                FETCH GQ1_CURSOR 
                  INTO 
                   :QGQ-BRNO,
                   :QGQ-ACCTNO,
                   :QGQ-COLLID, 
                   :QGQ-SEQ-FG, 
                   :QGQ-DATE, 
                   :QGQ-START-TIME,
                   :QGQ-REV-DEL,
                   :QGQ-REV-PDUE-DAYS,
                   :QGQ-REV-BAL,
                   :QGQ-WORK-COLLID, 
                   :QGQ-WORKED-FG, 
                   :QGQ-IN-PROCESS, 
                   :QGQ-DATE-QUEUED, 
                   :QGQ-TIME-ZONE-ERROR 
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GQ1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-GQ-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-GQ1-FILE.
           PERFORM CLOSE-IT.
           IF ( GQ1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION GQ_CONNECT END-EXEC
              EXEC SQL
                   CLOSE GQ1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO GQ1-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-GQ1-CONNECTION.

           IF GQ-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION GQ_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT GQ_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO GQ-CONN-OPEN-SW.

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
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY CQEXTR-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
