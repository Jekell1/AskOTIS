       IDENTIFICATION DIVISION.
       PROGRAM-ID.      SETAC2.
       DATE-WRITTEN.    06/01/2022.
      *****************************************************************
      *                      (SP)
      *
      *    DESCRIPTION:  SET TRANSACTIONS TRP, TRL, TRS AND TRB WITH
      *                  LN-ACTIONCD2 (SECURITIZATION POOL)
      *
      * REV:
      *  20220601 BAH NEW #1555
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBGB/GBFSWKI.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       COPY "LIBGB/GBFDPR.CPY".

      ******************************************************************
      *   BRANCH TOTALS WORK RECORD
      ******************************************************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-BRNO      PIC 9(4).
               05  WK-ACTIONCD2 PIC X(4).

           03  WK-TOTAL-SET  PIC 9(9).

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/SETAC2.CBL S35 06/01/22-16:02:11 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01TRB.CPY".
       COPY "LIBLP/LP01TRB_SQL.CPY".
       COPY "LIBLP/LPWSTRB.CPY".
       COPY "LIBLP/LP01TRL.CPY".
       COPY "LIBLP/LP01TRL_SQL.CPY".
       COPY "LIBLP/LPWSTRL.CPY".
       COPY "LIBLP/LP01TRP.CPY".
       COPY "LIBLP/LP01TRP_SQL.CPY".
       COPY "LIBLP/LPWSTRP.CPY".
       COPY "LIBLP/LP01TRS.CPY".
       COPY "LIBLP/LP01TRS_SQL.CPY".
       COPY "LIBLP/LPWSTRS.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
      *-----------------------------------------------------------------
       01  WK-IFN               PIC X(2)     VALUE "WK".

      ******************************************************************
      *   LINE WORKERS
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      ******************************************************************
      *   MISC WORKERS
      ******************************************************************
       01  MESS-LIST            PIC X(5)     VALUE "MSEL.".

      ******************************************************************
      *     S C R E E N   F I E L D S
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  BEG-DATE        PIC 9(8).
           03  END-DATE        PIC 9(8).
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).

       01  VERSION-CONTROL     PIC X      VALUE "A".

       01  SV-BEG-BR           PIC 9999   VALUE 9999.
       01  SV-END-BR           PIC 9999   VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.


      ******************************************************************
      *     P R I N T   B U F F E R S
      ******************************************************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(31)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(29)    VALUE " ".
           03  FILLER           PIC X(8)     VALUE "COMPANY ".
           03  FILLER           PIC X(5)     VALUE "PAGE:".
           03  H-PAGE-NO        PIC ZZZ9.

      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(26)    VALUE " ".
           03  FILLER           PIC X(40) VALUE
           "SET TRANSACTION RECS WITH LN-ACTIONCD2".

      *-----------------------------------------------------------------
       01  HEAD3.
           03  FILLER           PIC X(50)    VALUE
           "BRNO   NUMBER   CLASS  TRANS  ACTIONCD2".

      ******************************************************************
      *    D E T A I L - L I N E
      ******************************************************************
       01  DETAIL-LINE          PIC X(80) VALUE SPACES.

       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  D-BRNO           PIC 9(4).
           03  FILLER           PIC X(3).
           03  D-NUMBER         PIC Z(6).
           03  FILLER           PIC X(5).
           03  D-CLASS          PIC 99.
           03  FILLER           PIC X(5).
           03  D-TRANSCD        PIC X.
           03  FILLER           PIC X(7).
           03  D-ACTIONCD2      PIC X(4).

      *-----------------------------------------------------------------
       01  DETAIL2 REDEFINES DETAIL-LINE.
           03  T-BRNO           PIC 9(4).
           03  FILLER           PIC XX.
           03  T-ACTIONCD2      PIC X(4).
           03  FILLER           PIC XX.
           03  T-TOTAL          PIC Z(8)9.

      *-----------------------------------------------------------------
       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(20).
           03  RANGE-SEL        PIC X(60).

      ******************************************************************
      *
      *      C O P Y    L I B R A R Y    W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/ACCESSW.CPY".

       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/SETAC2_DEF.CPY".
       COPY "LIBSP/SETAC2_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/SETAC2_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TRS1-FILE.
           PERFORM CLOSE-TRP1-FILE.
           PERFORM CLOSE-TRL1-FILE.
           PERFORM CLOSE-TRB1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *     M A I N    M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "SETAC2" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.

           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

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
              GO TO PROCESS-TOTALS.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO PROCESS-TOTALS.

       MOVE-BRANCH-PATH-INFO.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO LN-PATH-OVERRIDE
                                              TRB-PATH-OVERRIDE
                                              TRP-PATH-OVERRIDE
                                              TRL-PATH-OVERRIDE
                                              TRS-PATH-OVERRIDE.

           PERFORM OPEN-LN1-FILE.

           PERFORM SET-TRP-FILE.
           PERFORM SET-TRB-FILE.
           PERFORM SET-TRL-FILE.
           PERFORM SET-TRS-FILE.

           PERFORM PRINT-BRANCH-TOTALS.
           MOVE 66 TO LN-FEED.

           PERFORM CLOSE-LN1-FILE.

           GO TO NEXT-GROUP-LOC.

       PROCESS-TOTALS.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           PERFORM PRINT-GRAND-TOTALS.

       END-ROUTINE.
           PERFORM RANGE-LINES.

           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE SETAC2-QUEUE-POS TO Q-POS.
           MOVE SETAC2-COPIES-POS TO C-POS.
           MOVE SETAC2-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

      * CREATE TOTALS WORK FILE

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT NOT = "00"
              MOVE "CANNOT MAKE WORK FILE" TO MESS
              PERFORM SEND-MESS
              GO TO INITIALIZE-ENDIT.

           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           IF EXT-ROUTE-BUF = "00"
              MOVE "SET SECURITIZATION IN PROGRESS......." TO MESS
              PERFORM SEND-LEGEND.

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

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-PR-FILE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           GO TO INITIALIZE-EXIT.

       INITIALIZE-ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       INITIALIZE-EXIT.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARAM.

           IF VDUSTATUS = "1U" GO TO BR-01.

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
                 MOVE "S  A000BR1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.

           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-01.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF WGR-WORD-ZERO
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
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.
           IF BEG-BRANCH > END-BRANCH
              PERFORM INVALID-RANGE
              GO TO BR-01.

           GO TO CHECK-BRANCH-SECURITY.

       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

       DT-01.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO EXIT-PARM.

           MOVE "ERNM060DATE1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DT-01.
           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.

       DT-02.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO EXIT-PARM.

           MOVE "ERNM060DATE2." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DT-01.
           IF NOT (VDUSTATUS = "00" OR "LD") GO TO DT-02.

           MOVE VDU-DATE-YYYYMMDD TO END-DATE.
  
           IF END-DATE < BEG-DATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO DT-02.


       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************
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

      ******************************************************************
       PRINT-BRANCH-TOTALS SECTION.
      ******************************************************************

           MOVE "BRANCH TOTALS:" TO DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 2 TO LN-FEED.

           MOVE BR-NO  TO WK-BRNO.
           MOVE SPACES TO WK-ACTIONCD2.
           PERFORM START-WK-FILE.

       PRINT-BRANCH-NEXT.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG NOT = 0
              GO TO PRINT-BRANCH-TOTALS-EXIT.

           IF WK-BRNO NOT = BR-NO
              GO TO PRINT-BRANCH-TOTALS-EXIT.

           MOVE WK-BRNO      TO T-BRNO.
           MOVE WK-ACTIONCD2 TO T-ACTIONCD2.
           MOVE WK-TOTAL-SET TO T-TOTAL.
           PERFORM WRITE-DETAIL-LINE.

           GO TO PRINT-BRANCH-NEXT.

       PRINT-BRANCH-TOTALS-EXIT.
           EXIT.

      ******************************************************************
       PRINT-GRAND-TOTALS SECTION.
      ******************************************************************

           MOVE "COMPANY TOTALS:" TO DETAIL-LINE.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 2 TO LN-FEED.

           MOVE 0      TO WK-BRNO.
           MOVE SPACES TO WK-ACTIONCD2.
           PERFORM START-WK-FILE.
       PRINT-GRAND-NEXT.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG NOT = 0
              GO TO PRINT-GRAND-TOTALS-EXIT.

           IF WK-BRNO NOT = 0
              GO TO PRINT-GRAND-TOTALS-EXIT.

           MOVE WK-ACTIONCD2 TO T-ACTIONCD2.
           MOVE WK-TOTAL-SET TO T-TOTAL.
           PERFORM WRITE-DETAIL-LINE.

           GO TO PRINT-GRAND-NEXT.

       PRINT-GRAND-TOTALS-EXIT.
           EXIT.


      ******************************************************************
       SET-TRP-FILE SECTION.
      ******************************************************************

           PERFORM OPEN-TRP1-FILE.

           MOVE BR-NO          TO TP-BRNO.
           MOVE BEG-DATE       TO TP-DATE.
           MOVE 0              TO TP-CLASS TP-ACCTNO TP-LINE.

           PERFORM START-TRP1-FILE.
       SET-TRP-NEXT.
           PERFORM READ-TRP1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRP-PATH-OWNBR NOT = TP-BRNO)
              GO TO SET-TRP-EXIT.

           IF TP-DATE > END-DATE
              GO TO SET-TRP-EXIT.

      * ONLY FIX OWNING BRANCH
           IF TP-OTHBR-FG = "C"
              GO TO SET-TRP-NEXT.

           MOVE TP-BRNO   TO LN-OWNBR.
           MOVE TP-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              GO TO SET-TRP-NEXT.

           MOVE LN-ACTIONCD2 TO TP-ACTIONCD2.

           PERFORM REWRITE-TRP1-FILE.

      * BRANCH TOTAL
           MOVE BR-NO        TO WK-BRNO.
           PERFORM UPDATE-WK-FILE.

      * GRAND TOTAL
           MOVE 0            TO WK-BRNO.
           PERFORM UPDATE-WK-FILE.

           MOVE "P"          TO D-TRANSCD.
           PERFORM PRINT-DETAIL-LINE.

           GO TO SET-TRP-NEXT.

       SET-TRP-EXIT.
           PERFORM CLOSE-TRP1-FILE.

      ******************************************************************
       SET-TRL-FILE SECTION.
      ******************************************************************

           PERFORM OPEN-TRL1-FILE.

           MOVE BR-NO          TO TL-BRNO.
           MOVE BEG-DATE       TO TL-DATE.
           MOVE 0              TO TL-CLASS TL-ACCTNO 
                                  TL-LAWCODE TL-LINE.

           PERFORM START-TRL1-FILE.
       SET-TRL-NEXT.
           PERFORM READ-TRL1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRL-PATH-OWNBR NOT = TL-BRNO)
              GO TO SET-TRL-EXIT.

           IF TL-DATE > END-DATE
              GO TO SET-TRL-EXIT.

           MOVE TL-BRNO   TO LN-OWNBR.
           MOVE TL-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              GO TO SET-TRL-NEXT.

           MOVE LN-ACTIONCD2 TO TL-ACTIONCD2.

           PERFORM REWRITE-TRL1-FILE.

      * BRANCH TOTAL
           MOVE BR-NO        TO WK-BRNO.
           PERFORM UPDATE-WK-FILE.

      * GRAND TOTAL
           MOVE 0            TO WK-BRNO.
           PERFORM UPDATE-WK-FILE.

           MOVE "L"          TO D-TRANSCD.
           PERFORM PRINT-DETAIL-LINE.

           GO TO SET-TRL-NEXT.

       SET-TRL-EXIT.
           PERFORM CLOSE-TRL1-FILE.

      ******************************************************************
       SET-TRB-FILE SECTION.
      ******************************************************************

           PERFORM OPEN-TRB1-FILE.

           MOVE BR-NO          TO TB-BRNO.
           MOVE BEG-DATE       TO TB-DATE.
           MOVE 0              TO TB-CLASS TB-ACCTNO 
                                  TB-LAWCODE TB-LINE.

           PERFORM START-TRB1-FILE.
       SET-TRB-NEXT.
           PERFORM READ-TRB1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRB-PATH-OWNBR NOT = TB-BRNO)
              GO TO SET-TRB-EXIT.

           IF TB-DATE > END-DATE
              GO TO SET-TRB-EXIT.

           MOVE TB-BRNO   TO LN-OWNBR.
           MOVE TB-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              GO TO SET-TRB-NEXT.

           MOVE LN-ACTIONCD2 TO TB-ACTIONCD2.

           PERFORM REWRITE-TRB1-FILE.

      * BRANCH TOTAL
           MOVE BR-NO        TO WK-BRNO.
           PERFORM UPDATE-WK-FILE.

      * GRAND TOTAL
           MOVE 0            TO WK-BRNO.
           PERFORM UPDATE-WK-FILE.

           MOVE "B"          TO D-TRANSCD.
           PERFORM PRINT-DETAIL-LINE.

           GO TO SET-TRB-NEXT.

       SET-TRB-EXIT.
           PERFORM CLOSE-TRB1-FILE.

      ******************************************************************
       SET-TRS-FILE SECTION.
      ******************************************************************

           PERFORM OPEN-TRS1-FILE.

           MOVE BR-NO          TO TS-BRNO.
           MOVE BEG-DATE       TO TS-DATE.
           MOVE 0              TO TS-CLASS TS-ACCTNO 
                                  TS-LAWCODE TS-LINE.

           PERFORM START-TRS1-FILE.
       SET-TRS-NEXT.
           PERFORM READ-TRS1-FILE-NEXT.
           IF (IO-FG NOT = 0) OR (TRS-PATH-OWNBR NOT = TS-BRNO)
              GO TO SET-TRS-EXIT.

           IF TS-DATE > END-DATE
              GO TO SET-TRS-EXIT.

           MOVE TS-BRNO   TO LN-OWNBR.
           MOVE TS-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              GO TO SET-TRS-NEXT.

           MOVE LN-ACTIONCD2 TO TS-ACTIONCD2.

           PERFORM REWRITE-TRS1-FILE.

      * BRANCH TOTAL
           MOVE BR-NO        TO WK-BRNO.
           PERFORM UPDATE-WK-FILE.

      * GRAND TOTAL
           MOVE 0            TO WK-BRNO.
           PERFORM UPDATE-WK-FILE.

           MOVE "S"          TO D-TRANSCD.
           PERFORM PRINT-DETAIL-LINE.

           GO TO SET-TRS-NEXT.

       SET-TRS-EXIT.
           PERFORM CLOSE-TRS1-FILE.

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
      
           MOVE HEAD3 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      
       END-HEAD-PAGE.
           MOVE 5 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      ******************************************************************
       RANGE-LINES SECTION.
      ******************************************************************

           MOVE 3 TO LN-FEED.
           IF GROUP-FLAG = "Y"
              MOVE "BRANCH GROUP" TO RANGE-LINE
              MOVE ENT-GROUP TO RANGE-SEL
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEG BRANCH" TO RANGE-LINE
              MOVE BEG-BRANCH TO RANGE-SEL
              PERFORM WRITE-DETAIL-LINE
              MOVE "END BRANCH" TO RANGE-LINE
              MOVE END-BRANCH TO RANGE-SEL
              PERFORM WRITE-DETAIL-LINE.
      
           MOVE "BEG DATE" TO RANGE-LINE.
           MOVE BEG-DATE   TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "END DATE" TO RANGE-LINE.
           MOVE END-DATE   TO RANGE-SEL.
           PERFORM WRITE-DETAIL-LINE.

       EXIT-RANGE-PRINT.
           EXIT.

      ******************************************************************
      *
      *     C O P Y    L I B R A R Y     S E C T I O N
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO SETAC2-CONAME.
           MOVE EXT-CONAME-SYSDATE TO SETAC2-SYSDATE.
           DISPLAY SETAC2-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE SETAC2-FORM-FIELDS TO WR-BUF.
           MOVE SETAC2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE SETAC2-HELP--FILE TO HELP-LINK-FILE.
           MOVE SETAC2-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/SETAC2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARA SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/RMFILE.CPY".
       UPDATE-WK-FILE.
           MOVE LN-ACTIONCD2 TO WK-ACTIONCD2.
           PERFORM READ-WK-FILE.
           IF IO-FG = 0
              ADD 1 TO WK-TOTAL-SET
              PERFORM REWRITE-WK-FILE
           ELSE
              MOVE 1 TO WK-TOTAL-SET
              PERFORM WRITE-WK-FILE.
       PRINT-DETAIL-LINE.
           MOVE BR-NO        TO D-BRNO.
           MOVE LN-ACCTNO    TO D-NUMBER.
           MOVE LN-CLASS     TO D-CLASS.
           MOVE LN-ACTIONCD2 TO D-ACTIONCD2.
           PERFORM WRITE-DETAIL-LINE.
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPTRSGS_SQL.CPY".
       COPY "LIBLP/LPTRPGS_SQL.CPY".
       COPY "LIBLP/LPTRLGS_SQL.CPY".
       COPY "LIBLP/LPTRBGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPTRP1IN.CPY".
       COPY "LIBLP/LPTRL1IN.CPY".
       COPY "LIBLP/LPTRB1IN.CPY".
       COPY "LIBLP/LPTRS1IN.CPY".
       COPY "LIBGB/GBWKIN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

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
              MOVE "LP/EMICMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY SETAC2-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
