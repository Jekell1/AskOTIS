       IDENTIFICATION  DIVISION.
       PROGRAM-ID.       GQLIST.
       AUTHOR.              MJD.
       DATE-WRITTEN. 2000/12/01.
      ******************************************************************
      *
      *                      (LS)
      *    DESCRIPTION:  GLOBAL COLLECTOR QUEUE FILE LISTING
      *
      *=================================================================
      * REV:
      * MJD 001201 COPIED OVER FROM LS/CQLIST AND CONVERTED FOR GLOBAL
      *            (TFC PR#60)
      * BLV 010108 FIX ALL "NOT ON FILE" LINES - PATHS INCORRECT BECAUSE
      *            OF BUG WITH HOLD-BRNO; FIX TITLE; DELETE USER BRANCH
      *            INFO IN PAGE HEADINGS - TOO CONFUSING!
      * BAH 081010 ADDED FIELDS TO THE FM FOR BATCH, PL# 640 REGACC
      * BAH 160128 SPLIT OUT LTFILE, PD0003
      *
      * KEC 2016.1006 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         REVOMED USING LIBSP/SPSETGQ.
      *         ADDED LOGIC OF CONVERTING & SETTING GQ-DATE (CCYYMMDD).
      *         FIXED HOLD-GQ2-KEY FOR NEW LAYOUT.
      *
      * BAH 170427 ACTIVATE 8 DIGIT DATES, PD0006
      * KEC 2018-1023 CONVERTED GQFILE FOR A32 & A34
      *  CS 190326 DONT NEED GQ-TYPE BUT LEFT IN FOR DOCUMENTATION
      *            THE SUMMARY RECORDS ARE NOW THE GQSFILE, DETAIL RECORDS
      *            ARE IN THE GQFILE
      *            REPLACE GQ2-KEY HOLD-GQ2 KEY WITH LOAD-GQ2-KEY AND
      *            SAVE GQ2-KEY (IN IO-ROUTINES)
      *  CS 190327 REMOVED GQS-GQ-BRNO FROM GQS-KEY-FIELDS,(THIS IS A COPY
      *            OF DETAIL GQ2-KEY. REMOVED BRANCH FROM GQ2-KEY
      *            
      * BAH 20190930 REMOVED ACCESS-CALL GQFILE
      *  CS 20210506 REMOVE HOLD-CQ-FUTURE-KEY1,KEY2
      *
      * SMR 2024-0214 ADDED VGQFILE LOGIC TO RESOLVE DIFFICULTY WITH
      *          THE GQ2-KEY WORKING WITH A SCAN WINDOW AND/OR USING A
      *          START-GREATER IO PARAGRAPH.  USING A TEMPORARY VISION
      *          GQFILE HELPS KEEP THE SAME EXPECT RESULTS FROM A15.
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBSP/SPFSVGQ.CPY".

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       COPY "LIBSP/SPFDVGQ.CPY".       

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LS/GQLIST.CBL S35 03/01/24-08:16:57 >".
      ******************************************************************
      *
      *   F I L E   P A T H S
      *
      ******************************************************************
       COPY "LIBSP/SP01GQ.CPY".
       COPY "LIBSP/SP01GQ_SQL.CPY".
       COPY "LIBSP/SPWSGQ.CPY".
       COPY "LIBSP/SPWSVGQ.CPY".
       COPY "LIBSP/SP01GQS.CPY".
       COPY "LIBSP/SP01GQS_SQL.CPY".
       COPY "LIBSP/SPWSGQS.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       COPY "LIBLP/LP01TZ.CPY".
       COPY "LIBLP/LP01TZ_SQL.CPY".
       COPY "LIBLP/LPWSTZ.CPY".
      *-----------------------------------------------------------------


      ******************************************************************
      *
      *   LINE WORKERS
      *
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  ER-FEED              PIC 9        VALUE 1.

      ******************************************************************
      *
      *   MISC WORKERS
      *
      ******************************************************************
       01  MSEL-LIST            PIC X(5)      VALUE "MSEL.".

       01  RECORDS-FOUND        PIC X     VALUE " ".
           88  NO-RECORDS-FOUND           VALUE "N".

       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".

       01  WK-START-TIME        PIC 9(4).
       01  WK-START-TIMEX       REDEFINES WK-START-TIME.
           03  WK-START-HH      PIC 9(2).
           03  WK-START-MM      PIC 9(2).

       01  A-NAME               PIC X(16).
       01  BAL-WK               PIC 9(6).

      *--------------------------------------------------- PROMPTS -----
       01  PROMPT-COLL-TOT-01      PIC X(15)   VALUE "TOTAL ACCOUNTS:".
       01  PROMPT-COLL-TOT-02      PIC X(13)   VALUE "TO BE WORKED:".
       01  PROMPT-COLL-TOT-03      PIC X(12)   VALUE "RESCHEDULED:".
       01  PROMPT-COLL-TOT-04      PIC X(7)    VALUE "WORKED:".

      *-----------------------------------------------------------------

       01  HOLD-COLLECTOR          PIC X(3)    VALUE SPACES.

       01  HOLD-BRNO               PIC 9(4).

       01  HOLD-SYSDATE            PIC 9(8).
      *-----------------------------------------------------------------
       01  WS-FIELDS.
           03  WS-HOME-PHONE               PIC X(14) VALUE SPACES.
           03  WS-WORK-PHONE               PIC X(14) VALUE SPACES.

           03  WS-PHONE-NUMBER-1           PIC 9(10) VALUE ZEROES.
           03  WS-PHONE-NUMBER-1-RDF       REDEFINES WS-PHONE-NUMBER-1.
               05  WS-PHONE-1-AREA         PIC X(3).
               05  WS-PHONE-1-3X           PIC X(3).
               05  WS-PHONE-1-4X           PIC X(4).

           03  WS-PHONE-NUMBER-2X.
               05  FILLER                  PIC X(1)  VALUE "(".
               05  WS-PHONE-2-AREA         PIC X(3).
               05  FILLER                  PIC X(2)  VALUE ")-".
               05  WS-PHONE-2-3X           PIC X(3).
               05  FILLER                  PIC X(1)  VALUE "-".
               05  WS-PHONE-2-4X           PIC X(4).
           03  WS-PHONE-NUMBER-2           REDEFINES WS-PHONE-NUMBER-2X
                                           PIC X(14).
       01  HOLD-PHONE        PIC 9(10).
       01  HOLD-PHONEX REDEFINES HOLD-PHONE.
           03  HOLD-AC       PIC 9(03).
           03  HOLD-PF       PIC 9(03).
           03  HOLD-4567     PIC 9(04).

      *-----------------------------------------------------------------
       COPY "LIBSP/COLRPT_CMD.CPY".
      *-----------------------------------------------------------------
       01  LEGEND               PIC X(70).

      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-COLLID      PIC X(3).
           03  END-COLLID      PIC X(3).
           03  BAD-ZONES-ONLY  PIC X.

       01  VERSION-CONTROL     PIC X VALUE "A".

      *****************************************************************
      *
      *  P R I N T  B U F F E R S
      *
      *****************************************************************
       01  HEAD1.
           03  H1-DATE       PIC X(8)    VALUE " ".
           03  FILLER        PIC XX      VALUE " ".
           03  H1-TIME       PIC X(8)    VALUE " ".
           03  FILLER        PIC X(28)   VALUE " ".
           03  H1-NAME       PIC X(40)   VALUE " ".
           03  FILLER        PIC X(37)   VALUE " ".
           03  FILLER        PIC X(5)    VALUE "PAGE".
           03  H1-PAGE-NO    PIC ZZZ9.

      *---------------------------------------------------------------
       01  HEAD2.
           03  FILLER        PIC X(10)   VALUE "USER ID:".
           03  H2-USERID     PIC X(10).
           03  FILLER        PIC X(28)   VALUE " ".
           03  FILLER        PIC X(35)   VALUE
               "GLOBAL COLLECTOR QUEUE FILE LISTING".
           03  FILLER        PIC X(49)   VALUE " ".

      *---------------------------------------------------------------
       01  HEAD4.
           03  FILLER        PIC X(10)   VALUE "COLLECTOR:".
           03  FILLER        PIC X(2)    VALUE SPACES.
           03  H4-COLLID     PIC X(3)    VALUE SPACES.
           03  FILLER        PIC X(2)    VALUE SPACES.
           03  H4-COLL-DESC  PIC X(25)   VALUE SPACES.
           03  FILLER        PIC X(59)   VALUE SPACES.
           03  FILLER        PIC X(31)
               VALUE "* DENOTES PRIORITY PROMISE DATE".

      *---------------------------------------------------------------
       01  HEAD5.
           03  FILLER    PIC X(41)   VALUE SPACES.
           03  FILLER    PIC X(3)    VALUE "CON".
           03  FILLER    PIC X(2)    VALUE SPACES.
           03  FILLER    PIC X(5)    VALUE "PAST ".
           03  FILLER    PIC X(7)    VALUE "CURRENT".
           03  FILLER    PIC X(3)    VALUE SPACES.
           03  FILLER    PIC X(17)   VALUE "---RESCHEDULED---".
           03  FILLER    PIC X(2)    VALUE SPACES.
           03  FILLER    PIC X(6)    VALUE "WORKED".
           03  FILLER    PIC X(39)   VALUE SPACES.
           03  FILLER    PIC X(5)    VALUE "QUEUE".
           03  FILLER    PIC X(2)    VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEAD6.
           03  FILLER    PIC X(16)   VALUE "BRNO ACCOUNT NO.".
           03  FILLER    PIC X(3)    VALUE SPACES.
           03  FILLER    PIC X(20)   VALUE "---BORROWER NAME----".
           03  FILLER    PIC X(2)    VALUE SPACES.
           03  FILLER    PIC X(4)    VALUE "DEL ".
           03  FILLER    PIC X(6)    VALUE " DUE  ".
           03  FILLER    PIC X(7)    VALUE "BALANCE".
           03  FILLER    PIC X(4)    VALUE SPACES.
           03  FILLER    PIC X(4)    VALUE "DATE".
           03  FILLER    PIC X(6)    VALUE SPACES.
           03  FILLER    PIC X(4)    VALUE "TIME".
           03  FILLER    PIC X(4)    VALUE SPACES.
           03  FILLER    PIC X(2)    VALUE "BY".
           03  FILLER    PIC X(4)    VALUE SPACES.
           03  FILLER    PIC X(10)   VALUE "HOME-PHONE".
           03  FILLER    PIC X(10)   VALUE "  ZONE    ".
           03  FILLER    PIC X(10)   VALUE "WORK-PHONE".
           03  FILLER    PIC X(7)    VALUE "  ZONE ".
           03  FILLER    PIC X(3)    VALUE SPACES.
           03  FILLER    PIC X(4)    VALUE "DATE".
           03  FILLER    PIC X(2)    VALUE SPACES.
      *****************************************************************
      *
      *    D E T A I L  L I N E
      *
      *****************************************************************
       01  DETAIL-LINE      PIC X(132)   VALUE SPACES.

      *----------------------------------------------------------------
       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  D1-BRNO             PIC ZZZ9.
           03  FILLER              PIC X(01).
           03  D1-LN-ACCTNO        PIC X(13).
           03  FILLER              PIC X.
           03  D1-BW-NAME          PIC X(20).
           03  D1-CON-DEL          PIC ZZZ99.
           03  FILLER              REDEFINES D1-CON-DEL.
               05  FILLER          PIC X(2).
               05  D1-CON-DELX     PIC X(3).
           03  D1-PDUE-DAYS        PIC Z(5).
           03  FILLER              PIC X.
           03  D1-CURRENT-BALANCE  PIC ZZZZ,ZZ9.
           03  FILLER              PIC X(2).
           03  D1-RESCHEDULE-DATE  PIC 99/99/99 BLANK ZERO.
           03  FILLER              PIC X(2).
           03  D1-HOUR             PIC Z9.
           03  D1-COLON            PIC X(1).
           03  D1-MINUTES          PIC 99.
           03  FILLER              PIC X(1).
           03  D1-AM-PM            PIC X(2).
           03  FILLER              PIC XX.
           03  D1-WORK-COLLID      PIC X(3).
           03  FILLER              PIC X.
           03  D1-HOME-PHONE       PIC X(14).
           03  FILLER              PIC X.
           03  D1-HOME-ZONE.
               05  D1-HOME-ZONE1   PIC X.
               05  D1-HOME-ZONE2   PIC X.
               05  D1-HOME-ZONE3   PIC X.
           03  FILLER              PIC XX.
           03  D1-WORK-PHONE       PIC X(14).
           03  FILLER              PIC X.
           03  D1-WORK-ZONE.
               05  D1-WORK-ZONE1   PIC X.
               05  D1-WORK-ZONE2   PIC X.
               05  D1-WORK-ZONE3   PIC X.
           03  FILLER              PIC X.
           03  D1-DATE-TYPE        PIC X.
           03  D1-DATE             PIC 99/99/99 BLANK ZERO.
      *----------------------------------------------------------------
       01  DETAIL2 REDEFINES DETAIL-LINE.
           03  D2-PROMPT-1         PIC X(15).
           03  FILLER              PIC X(1).
           03  D2-TOT-ACCTS        PIC ZZZZ9.
           03  FILLER              PIC X(5).
           03  D2-PROMPT-2         PIC X(13).
           03  FILLER              PIC X(1).
           03  D2-TO-BE-WORKED     PIC ZZZZ9.
           03  FILLER              PIC X(5).
           03  D2-PROMPT-3         PIC X(12).
           03  FILLER              PIC X(1).
           03  D2-RESCHEDULED      PIC ZZZZ9.
           03  FILLER              PIC X(5).
           03  D2-PROMPT-4         PIC X(7).
           03  FILLER              PIC X(1).
           03  D2-WORKED           PIC ZZZZ9.
           03  FILLER              PIC X(46).

      *-----------------------------------------------------------------
       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(35).
           03  RANGE-SELECTION.
               05  R-CHAR       PIC X(6).

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      ******************************************************************
       COPY "LIBGB/PASSWDW.CPY".

       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBLP/LPUPWKW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/GQLIST_DEF.CPY".
       COPY "LIBLS/GQLIST_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/GQLIST_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE VGQ-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-GQS1-FILE.
           PERFORM CLOSE-GQ2-FILE.
           PERFORM CLOSE-TZ1-FILE.
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           PERFORM CLOSE-VGQ1-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.
           PERFORM SQL-CONNECT.
      D    STOP MESS.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "GQLIST" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE "Y"        TO FIRST-TIME.

      *    NEED TO CREATE & LOAD TEMPORARY VISION GQFILE FOR USE
           PERFORM CREATE-VGQ-FILE.

           MOVE BEG-COLLID TO GQ-COLLID
                              QGQ2-WBEG-COLLID.
           MOVE END-COLLID TO QGQ2-WEND-COLLID.
           MOVE SPACES     TO GQ-SEQ-FG
                              QGQ2-WBEG-SEQ-FG.
           MOVE ALL "Z"    TO QGQ2-WEND-SEQ-FG.
           MOVE 19000101   TO GQ-DATE SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QGQ2-WBEG-DATE.
           MOVE EXT-JULIAN-END TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QGQ2-WEND-DATE.

           MOVE 0          TO GQ-START-TIME GQ-SEQ
                              GQ-REV-DEL GQ-REV-PDUE-DAYS GQ-REV-BAL
                              QGQ2-WBEG-START-TIME
                              QGQ2-WBEG-SEQ
                              QGQ2-WBEG-REV-DEL
                              QGQ2-WBEG-REV-PDUE-DAYS
                              QGQ2-WBEG-REV-BAL.
           MOVE ALL "9"    TO QGQ2-WEND-START-TIME
                              QGQ2-WEND-SEQ
                              QGQ2-WEND-REV-DEL
                              QGQ2-WEND-REV-PDUE-DAYS
                              QGQ2-WEND-REV-BAL.

           MOVE SPACES     TO GQ-MISC-KEY-FLD
                              QGQ2-WBEG-MISC-KEY-FLD.
           MOVE ALL "Z"    TO QGQ2-WEND-MISC-KEY-FLD.

           PERFORM OPEN-VGQ1-FILE.
           PERFORM START-VGQ1-FILE.

       NEXT-GQ-REPORT.

           PERFORM READ-VGQ1-FILE-NEXT.

           IF ( IO-FG = 9 )
              GO TO END-ROUTINE.

           MOVE VGQ-GQ-REC TO GQ-REC.

           IF ( GQ-COLLID = SPACES )
              GO TO NEXT-GQ-REPORT.
     
           IF GQ-COLLID > END-COLLID
              GO TO END-ROUTINE.

           IF BAD-ZONES-ONLY = "Y"
              IF GQ-TIME-ZONE-ERROR NOT = "Y"
                 GO TO NEXT-GQ-REPORT.

           IF FIRST-TIME = "Y"
              MOVE GQ-COLLID TO HOLD-COLLECTOR
              MOVE GQ-BRNO TO BR-NO
              PERFORM GET-BR
              PERFORM SET-FILE-PATHS
              PERFORM SET-COLLECTOR-HEADINGS
              MOVE GQ-BRNO TO HOLD-BRNO.

           IF GQ-COLLID NOT = HOLD-COLLECTOR
              PERFORM COLLECTOR-TOTALS
              PERFORM SET-COLLECTOR-HEADINGS.

           IF GQ-BRNO NOT = HOLD-BRNO
              MOVE GQ-BRNO TO BR-NO
              PERFORM GET-BR
              IF IO-BAD
                 MOVE
                  "ERROR: BRANCH RECORD NOT FOUND FOR &&&& - SKIPPED"
                           TO MESS
                 INSPECT MESS REPLACING ALL "&&&&" BY BR-NO
                 PERFORM SEND-MESS
                 GO TO NEXT-GQ-REPORT
              END-IF
              PERFORM SET-FILE-PATHS
              MOVE GQ-BRNO TO HOLD-BRNO.

           PERFORM PRINT-DETAIL-LINE.
           GO TO NEXT-GQ-REPORT.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND
              GO TO END-PROGRAM
           ELSE
              PERFORM COLLECTOR-TOTALS
              PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE GQLIST-QUEUE-POS TO Q-POS.
           MOVE GQLIST-COPIES-POS TO C-POS.
           MOVE GQLIST-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           MOVE 0 TO PAGE-NUMBER.

           MOVE EXT-CONAME-CONAME TO H1-NAME.
           MOVE EXT-CONAME-SYSDATE TO H1-DATE.
           MOVE EXT-CONAME-USER-ID TO H2-USERID.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO HOLD-SYSDATE.

           IF EXT-ROUTE-BUF = "00"
              MOVE "COLLECTOR QUEUE LISTING IN PROGRESS....." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-BR-FILE.
           IF GP-TIME-ZONE-Q = "Y"
              PERFORM OPEN-TZ1-FILE.

           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-CDB1-FILE.
           PERFORM OPEN-GQS1-FILE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE2 TO HOLD-SYSDATE.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.

           IF UP-KEY
              GO TO ENDING-COLLECTOR.

       BEGIN-COLLECTOR.
           MOVE LEGEND-COLL-01 TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A030BEG-COLLID." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "16" ) GO TO ALL-COLLECTORS.
           IF ( VDUSTATUS = "1>" ) GO TO EXIT-PARM.

           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEGIN-COLLECTOR.
           MOVE VDUBUF TO BEG-COLLID.

      *-----------------------------------------------------------------
       ENDING-COLLECTOR.
           MOVE LEGEND-COLL-02 TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A030END-COLLID." TO VDU.
           PERFORM SCREENIO.
           IF ( VDUSTATUS = "10" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1U" ) GO TO BEGIN-COLLECTOR.
           IF ( VDUSTATUS = "1>" ) GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENDING-COLLECTOR.
           MOVE VDUBUF TO END-COLLID.

           IF ( BEG-COLLID > END-COLLID )
              MOVE "INVALID COLLECTOR ID RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEGIN-COLLECTOR.

           GO TO BAD-ZONES-YN.

      *-----------------------------------------------------------------
       ALL-COLLECTORS.
           MOVE "S  A030BEG-COLLID." TO VDU.
           MOVE SPACES             TO BEG-COLLID VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A030END-COLLID." TO VDU.
           MOVE "ZZZ"              TO END-COLLID VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       BAD-ZONES-YN.

           IF GP-TIME-ZONE-Q NOT = "Y"
              MOVE "N" TO BAD-ZONES-ONLY
              GO TO END-PARM.

           MOVE "PRINT ONLY ACCOUNTS WITH BAD TIME ZONES" TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "S  A000ZONEPROMPT." TO VDU.
           MOVE "BAD TIME ZONES ONLY   :" TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "E  A010BADZONES." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1U" ) GO TO ENDING-COLLECTOR.
           IF ( VDUSTATUS = "1>" ) GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BAD-ZONES-YN.
           MOVE VDUBUF TO BAD-ZONES-ONLY.
           IF BAD-ZONES-ONLY NOT = "Y"
              IF BAD-ZONES-ONLY NOT = "N"
                 MOVE "PLEASE ENTER 'Y' OR 'N'" TO MESS
                 PERFORM SEND-MESS
                 GO TO BAD-ZONES-YN.

       END-PARM.
           MOVE LEGEND-ACCEPT TO LEGEND.
           PERFORM SEND-LEGEND.

       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************
           MOVE "S  A030BEG-COLLID." TO VDU.
           MOVE BEG-COLLID           TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A030END-COLLID." TO VDU.
           MOVE END-COLLID           TO VDUBUF.
           PERFORM SCREENIO.
           IF GP-TIME-ZONE-Q = "Y"
              MOVE "S  A010BADZONES." TO VDU
              MOVE BAD-ZONES-ONLY     TO VDUBUF
              PERFORM SCREENIO.

      ******************************************************************
       PRINT-DETAIL-LINE SECTION.
      ******************************************************************

           IF ( FIRST-TIME = "Y" )
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE 1 TO LN-FEED
              MOVE "Y" TO RECORDS-FOUND
              MOVE "N" TO FIRST-TIME.

      * LINE 1

           MOVE GQ-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF ( IO-FG = 9 )
              MOVE "<NOT ON FILE>" TO D1-BW-NAME
           ELSE
              MOVE LN-SSNO(1)       TO BW-SSNO
              PERFORM READ-BW1-FILE
              IF ( IO-FG = 9 )
                 MOVE "<NOT ON FILE>" TO D1-BW-NAME
              ELSE
                 PERFORM GET-PHONE-NUMBERS
                 MOVE WS-HOME-PHONE TO D1-HOME-PHONE
                 MOVE WS-WORK-PHONE TO D1-WORK-PHONE
                 MOVE BW-LNAME TO MESS
                 MOVE BW-FNAME TO A-NAME
                 PERFORM SET-GQ-LNAME
                 MOVE MESS TO D1-BW-NAME
                 IF GP-TIME-ZONE-Q = "Y"
                    PERFORM GET-TIME-ZONE
                 END-IF
              END-IF
              PERFORM LPSTAT-DISPLAY
              MOVE LPS-DISPLAY-LNNO TO D1-LN-ACCTNO.

           PERFORM GET-SP1-FILE.

           MOVE HOLD-SYSDATE TO AGEING-DATE.
           PERFORM COMPUTE-CONTRACTUAL.
           IF CPOT30 = "Y"
              MOVE "POT" TO D1-CON-DELX
           ELSE
           IF CPOT30 = "M"
              MOVE "MIS" TO D1-CON-DELX
           ELSE
           IF CPOTCHGOFF = "Y"
              MOVE "PC" TO D1-CON-DELX
           ELSE
           IF CPOTCHGOFF = "M"
              MOVE "PL" TO D1-CON-DELX
           ELSE
              MOVE CONTRACTUAL TO D1-CON-DEL.

           MOVE GQ-BRNO         TO D1-BRNO.
           MOVE HOLD-SYSDATE TO PDUE-TODAY-DATE.
           MOVE LN-1STPYDATE TO PDUE-1STPYDATE.
           PERFORM PAST-DUE-DAYS-CALCULATION.
           MOVE PDUE-DAYS       TO D1-PDUE-DAYS.
           MOVE GQ-WORK-COLLID  TO D1-WORK-COLLID.

           MOVE GQ-DATE             TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.

           IF GQ-SEQ-FG = "B"
             MOVE "*" TO D1-DATE-TYPE.

           IF GQ-SEQ-FG = "R"
             MOVE DATE-MMDDYY         TO D1-RESCHEDULE-DATE
             MOVE GQ-DATE-QUEUED      TO DATE-YYYYMMDD
             PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.

           MOVE DATE-MMDDYY TO D1-DATE.

           COMPUTE BAL-WK ROUNDED = LN-CURBAL - LN-OT2BAL.
           MOVE BAL-WK TO D1-CURRENT-BALANCE.

           IF GQ-START-TIME = 0 OR GQ-SEQ-FG NOT = "R"
              GO TO SKIP-PRINT-TIME.

           MOVE GQ-START-TIME TO WK-START-TIME.

           IF ( WK-START-HH > 11 )
              MOVE "PM" TO D1-AM-PM
           ELSE
              MOVE "AM" TO D1-AM-PM.

           IF ( WK-START-HH > 12 )
              SUBTRACT 12 FROM WK-START-HH.

           MOVE WK-START-HH TO D1-HOUR.
           MOVE ":"         TO D1-COLON.
           MOVE WK-START-MM TO D1-MINUTES.

       SKIP-PRINT-TIME.
           MOVE 1                TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       EXIT-PRINT-DETAIL-LINE.
           EXIT.

      ******************************************************************
       GET-TIME-ZONE SECTION.
      ******************************************************************

      * GET HOME ZONE

           MOVE BW-PHONE TO HOLD-PHONE.
           MOVE HOLD-AC TO TZ-CODE.
           MOVE HOLD-PF TO TZ-PREFIX.
           PERFORM READ-TZ1-FILE.
           IF IO-FG NOT = 0
              MOVE 0 TO TZ-PREFIX
              PERFORM READ-TZ1-FILE.
           IF IO-FG NOT = 0
              MOVE "BAD" TO D1-HOME-ZONE
           ELSE
              MOVE TZ-ZONE TO D1-HOME-ZONE1
              MOVE "S" TO D1-HOME-ZONE2
              MOVE "T" TO D1-HOME-ZONE3
              IF TZ-DLS = "Y"
                 MOVE "D" TO D1-HOME-ZONE2.

      * GET WORK ZONE

           MOVE BW-EMPPHONE TO HOLD-PHONE.
           MOVE HOLD-AC TO TZ-CODE.
           MOVE HOLD-PF TO TZ-PREFIX.
           PERFORM READ-TZ1-FILE.
           IF IO-FG NOT = 0
              MOVE 0 TO TZ-PREFIX
              PERFORM READ-TZ1-FILE.
           IF IO-FG NOT = 0
              MOVE "BAD" TO D1-WORK-ZONE
           ELSE
              MOVE TZ-ZONE TO D1-WORK-ZONE1
              MOVE "S" TO D1-WORK-ZONE2
              MOVE "T" TO D1-WORK-ZONE3
              IF TZ-DLS = "Y"
                 MOVE "D" TO D1-WORK-ZONE2.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF ( LN-WK GREATER THAN EXT-CONAME-RPT-LINES )
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1      TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ******************************************************************
       SET-COLLECTOR-HEADINGS SECTION.
      ******************************************************************
           MOVE GQ-COLLID  TO H4-COLLID.
           PERFORM GET-COLLID-DESC.
           MOVE CDB-DESC TO H4-COLL-DESC.

      ******************************************************************
       COLLECTOR-TOTALS SECTION.
      ******************************************************************

           MOVE HOLD-COLLECTOR TO GQS-COLLID.
           PERFORM READ-GQS1-FILE.
           IF ( IO-FG = 9 )
              MOVE "NO QUEUE AVAILABLE FOR COLLECTOR &&& " TO MESS
              INSPECT MESS REPLACING FIRST "&&&" BY HOLD-COLLECTOR
              MOVE MESS TO DETAIL-LINE
              MOVE 2    TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              GO TO EXIT-COLLECTOR-TOTALS.

      * PRINT COLLECTOR TOTAL

           MOVE PROMPT-COLL-TOT-01 TO D2-PROMPT-1.
           MOVE GQS-TOT-REC         TO D2-TOT-ACCTS.

           MOVE PROMPT-COLL-TOT-02 TO     D2-PROMPT-2.
           SUBTRACT GQS-WORKED-REC  FROM   GQS-TOT-REC
                                   GIVING D2-TO-BE-WORKED.

           MOVE PROMPT-COLL-TOT-03 TO D2-PROMPT-3.
           MOVE GQS-TIMED-REC       TO D2-RESCHEDULED.

           MOVE PROMPT-COLL-TOT-04 TO D2-PROMPT-4.
           MOVE GQS-WORKED-REC      TO D2-WORKED.

           MOVE 2                  TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       EXIT-COLLECTOR-TOTALS.
           MOVE GQ-COLLID TO HOLD-COLLECTOR.
           PERFORM SET-COLLECTOR-HEADINGS.
           MOVE 99 TO LN-COUNT.

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H1-PAGE-NO.
           PERFORM GET-TIME.
           MOVE TIME-EDIT   TO H1-TIME.
           MOVE HEAD1       TO PR-REC.
           MOVE 99          TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD2 TO PR-REC.
           MOVE 1     TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD4 TO PR-REC.
           MOVE 2     TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD5 TO PR-REC.
           MOVE 2     TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD6 TO PR-REC.
           MOVE 1     TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE ALL "-" TO PR-REC.
           MOVE 1       TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE 9 TO LN-COUNT.
           MOVE 1  TO LN-FEED.

       END-HEAD-PAGE.
           EXIT.

      ******************************************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************************************
           MOVE SPACES TO H4-COLLID.
           MOVE SPACES TO H4-COLL-DESC.
           MOVE 99     TO LN-COUNT.
           MOVE 4      TO LN-FEED.

           MOVE "BEG COLLECTOR" TO RANGE-LINE.
           MOVE BEG-COLLID      TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "END COLLECTOR" TO RANGE-LINE.
           MOVE END-COLLID      TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.
           IF GP-TIME-ZONE-Q = "Y"
              MOVE "BAD TIME ZONES ONLY" TO RANGE-LINE
              MOVE BAD-ZONES-ONLY        TO R-CHAR
              PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
       SET-FILE-PATHS SECTION.
      ******************************************************************
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LN1-FILE.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              LTP-PATH-OVERRIDE.

           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LTP1-FILE.
           PERFORM OPEN-LN1-FILE.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBLP/LPUPWK.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBSP/SPFRVGQ.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO GQLIST-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GQLIST-SYSDATE.
           DISPLAY GQLIST-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE GQLIST-FORM-FIELDS TO WR-BUF.
           MOVE GQLIST-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE GQLIST-HELP--FILE TO HELP-LINK-FILE.
           MOVE GQLIST-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/GQLIST_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARA SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
      *-----------------------------------------------------------------
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       GET-COLLID-DESC.
           MOVE "CN"      TO CDB-TYPE.
           MOVE ZEROS     TO CDB-BRANCH.
           MOVE GQ-COLLID TO CDB-CODE.
           PERFORM READ-CDB1-FILE.
           IF ( IO-FG NOT = 0 )
              MOVE "<NOT ON FILE>" TO CDB-DESC.

      *-----------------------------------------------------------------
       SET-GQ-LNAME.
           INSPECT MESS REPLACING FIRST "  " BY ",?".
           INSPECT MESS REPLACING FIRST "                "
               BY A-NAME.
           INSPECT MESS REPLACING ALL "?" BY " ".


      *-----------------------------------------------------------------
       GET-BR.
           PERFORM READ-BR-FILE.
           IF IO-BAD
              MOVE "** INVALID BRANCH **" TO BR-NAME.

      *-----------------------------------------------------------------
       GET-SP1-FILE.
           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           IF SP1-KEY NOT = LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE
              IF IO-FG NOT = 0
                 PERFORM FILE-ERRORS
                 PERFORM SEND-MESS
                 MOVE SPACES TO LPWSSP-SP1-KEY
              ELSE
                 MOVE SP1-KEY TO LPWSSP-SP1-KEY.
      *-----------------------------------------------------------------
       GET-PHONE-NUMBERS.

           IF ( BW-PHONE NOT = ZEROES )
              MOVE BW-PHONE          TO WS-PHONE-NUMBER-1
              MOVE WS-PHONE-1-AREA   TO WS-PHONE-2-AREA
              MOVE WS-PHONE-1-3X     TO WS-PHONE-2-3X
              MOVE WS-PHONE-1-4X     TO WS-PHONE-2-4X
              MOVE WS-PHONE-NUMBER-2 TO WS-HOME-PHONE
           ELSE
              MOVE SPACES            TO WS-HOME-PHONE.

           IF ( BW-EMPPHONE NOT = ZEROES )
              MOVE BW-EMPPHONE       TO WS-PHONE-NUMBER-1
              MOVE WS-PHONE-1-AREA   TO WS-PHONE-2-AREA
              MOVE WS-PHONE-1-3X     TO WS-PHONE-2-3X
              MOVE WS-PHONE-1-4X     TO WS-PHONE-2-4X
              MOVE WS-PHONE-NUMBER-2 TO WS-WORK-PHONE
           ELSE
              MOVE SPACES            TO WS-WORK-PHONE.

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBSP/SPGQSGS_SQL.CPY".
       COPY "LIBSP/SPGQGS_SQL.CPY".
       COPY "LIBLP/LPTZGS_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBSP/SPGQ2RN.CPY".
       COPY "LIBSP/SPGQ1RN.CPY".
       COPY "LIBSP/SPVGQ1IN.CPY".
       COPY "LIBSP/SPGQS1RN.CPY".
       COPY "LIBLP/LPTZ1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
             MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF ( EXT-ROUTE-BUF = "00" )
              PERFORM PRU-OUT.

           IF ( NO-RECORDS-FOUND )
              IF ( EXT-ROUTE-BUF = "00" OR "70" )
                 MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
                 PERFORM SEND-MESS.

       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY GQLIST-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
