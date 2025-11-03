       IDENTIFICATION DIVISION.
       PROGRAM-ID.   CQRELS.
       DATE-WRITTEN. 060697.
      ******************************************************************
      *
      *                      (LS)
      *    DESCRIPTION:  COLLECTOR QUEUE RESCHEDULE REPORT
      *
      * REV:
      * KEC 042696 FIXED THE CQ-DATE CONVERSION (LIBSP/SPSETCQ).
      * MJD 121096 CHANGED TO PERFORM LPSTAT-DISPLAY AFTER READING BWFILE.
      * BLV 041497 CHECK BR-COL-QUEUE-LOW-TO-HI FOR QUEUE DELINQUENCY ORDER
      *  CS 061297 PRINT RESCHEDULES GREATER THAN XXX DAYS
      * KEC 072997 CHANGED 'LIBGB/SFTDATE' TO 'LIBGB/EDITOR' AND
      *             COMMENTED OUT.
      * GLF 022398 DELETED BR-SECURITY-OK AND ADDED LIBGB/BRSECUREW.
      * BLV 040623 FIX START ON CQ2-KEY, REGACC PR# 59
      *
      * KEC 2016.1004 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED HOLD-CQ2-KEY FOR NEW LAYOUT & SIZE.
      *
      * KEC 2016.1006 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         REVOMED USING LIBSP/SPSETCQ & LIBSP/SPSETGQ.
      *         ADDED LOGIC OF CONVERTING & SETTING CQ-DATE (CCYYMMDD).
      *
      * BAH 170425 ACTIVATE 8 DIGIT DATES, PD0006
      *  CS 190129 GLOBAL CQFILE
      *            SEE NOTES IN LIBSP/SPCQ2IN,SPCQ2RN FOR CQ2-KEY
      *  CS 190308 CQFILE IS DETAIL QUEUE, CQSFILE IS SUMMARY, NO REASON FOR
      *            CQ-TYPE, REMOVED FROM LAYOUT. IF READING CQ, ITS A DETAIL
      *            RECORD
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      *     S T O P
      * DO NOT REMOVE CQ-TYPE COMMENT UNTIL ALL TESTING DONE, THE TYPE
      * IS A CLUE AS TO WHAT TYPE OF RECORD YOU NEED, ESPECIALLY IN PROGRAMS
      * THAT OPEN CQ AND CQS
      *%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      * BAH 20190930 REMOVED ACCESS-CALL CQFILE
      * SMR 2023-0925 ADDED VCQFILE LOGIC TO RESOLVE DIFFICULTY WITH
      *          THE CQ2-KEY WORKING WITH A SCAN WINDOW AND/OR USING A
      *          START-GREATER IO PARAGRAPH.  USING A TEMPORARY VISION
      *          CQFILE HELPS KEEP THE SAME EXPECT RESULTS FROM A15.
      * SMR 2025-0318 CHANGED REFERENCES OF VCQ1 TO VCQ2 S35Q-191
      * JKC 2025-0401 S35Q-191 CHANGED USE OF LIBSP/SPVCQ2IN TO BE
      *         LIBSP/SPVC2RN;DID THIS SO WE COULD REMOVE THE
      *         NEED FOR LIBSP/SPVCQ2IN
      *
      * JKC 2025-0424 S35Q-200 ADDED NEW LOGIC FOR UPDATE THE VCQFILE;
      *          THE ORIGINAL VCQFILE WAS CREATED ONE PER USER PUT INTO
      *          /USR/TMP; THE NEW VCQFILE IS CREATED FOR ONE PER BRANCH
      *          PUT INTO /USR/DATA/R#/WK/VCQ#### WHICH WAS DONE FOR
      *          POTENTIALLY AVOID ISSUES WITH A VCQFILE ONE PER USER.
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBSP/SPFSVCQ.CPY".

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       COPY "LIBSP/SPFDVCQ.CPY".

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E    S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)LS/CQRELS S35 04/28/25-08:59:59 >".
      ******************************************************************
      *
      *   F I L E   P A T H S
      *
      ******************************************************************
       COPY "LIBSP/SP01CQ.CPY".
       COPY "LIBSP/SP01CQ_SQL.CPY".
       COPY "LIBSP/SPWSCQ.CPY".
       COPY "LIBSP/SPWSVCQ.CPY".
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
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       
       01  PR-IFN               PIC X(2)     VALUE "PR".

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
       01  HOLD-RESCHD-DATE     PIC 9(8).
       01  COLL-RESCHD-TOTAL    PIC 9(3) VALUE 0.

       01  A-NAME               PIC X(16).
       01  BAL-WK               PIC 9(6).
       01  DEL-WK               PIC 9(5).
       01  SP-EXPD-FRMLA        PIC X(1).


       
       01  PROMPT-COLL-TOT-03      PIC X(12)   VALUE "RESCHEDULED:".

       

       01  HOLD-COLLECTOR          PIC X(3)    VALUE SPACES.

       01  WS-COLL-TOTAL-FG            PIC X(1)  VALUE SPACES.
       01  WS-HOME-PHONE               PIC X(14) VALUE SPACES.
       01  WS-WORK-PHONE               PIC X(14) VALUE SPACES.
       01  WS-PHONE-NUMBER-1           PIC 9(10) VALUE ZEROES.
       01  WS-PHONE-NUMBER-1-RDF       REDEFINES WS-PHONE-NUMBER-1.
           03  WS-PHONE-1-AREA         PIC X(3).
           03  WS-PHONE-1-3X           PIC X(3).
           03  WS-PHONE-1-4X           PIC X(4).
       01  WS-PHONE-NUMBER-2X.
           03  FILLER                  PIC X(1)  VALUE "(".
           03  WS-PHONE-2-AREA         PIC X(3).
           03  FILLER                  PIC X(2)  VALUE ")-".
           03  WS-PHONE-2-3X           PIC X(3).
           03  FILLER                  PIC X(1)  VALUE "-".
           03  WS-PHONE-2-4X           PIC X(4).
       01  WS-PHONE-NUMBER-2           REDEFINES WS-PHONE-NUMBER-2X
                                       PIC X(14).

       01  LEGEND               PIC X(70).

      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
       
           03  BEG-BRANCH      PIC 9(4).
           03  END-BRANCH      PIC 9(4).
           03  BEG-COLLID      PIC X(3).
           03  END-COLLID      PIC X(3).
           03  RESCHD-XDAYS         PIC 9(2).
           03  GROUP-FLAG      PIC X.
           03  ENT-GROUP       PIC X(4).

       01  VERSION-CONTROL     PIC X VALUE "A".

       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.
       
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

       
       01  HEAD2.
           03  FILLER        PIC X(10)   VALUE "USER ID:".
           03  H2-USERID     PIC X(10).
           03  FILLER        PIC X(30)   VALUE " ".
           03  FILLER        PIC X(28)   VALUE
               "COLLECTOR RESCHEDULE LISTING".
           03  FILLER        PIC X(54)   VALUE " ".

       
       01  HEAD3.
           03  FILLER        PIC X(10)   VALUE "BRANCH :".
           03  H3-BRNO       PIC 9(4).
           03  FILLER        PIC X(2)     VALUE " ".
           03  H3-BRDESC     PIC X(40)    VALUE " ".
           03  FILLER        PIC X(76)    VALUE " ".

       
       01  HEAD4.
           03  FILLER        PIC X(10)   VALUE "COLLECTOR:".
           03  FILLER        PIC X(2)    VALUE SPACES.
           03  H4-COLLID     PIC X(3)    VALUE SPACES.
           03  FILLER        PIC X(2)    VALUE SPACES.
           03  H4-COLL-DESC  PIC X(25)   VALUE SPACES.
           03  FILLER        PIC X(90)   VALUE SPACES.

       
       01  HEAD5.
           03  FILLER    PIC X(44)   VALUE SPACES.
           03  FILLER    PIC X(3)    VALUE "CON".
           03  FILLER    PIC X(2)    VALUE SPACES.
           03  FILLER    PIC X(6)    VALUE "PAST  ".
           03  FILLER    PIC X(7)    VALUE "CURRENT".
           03  FILLER    PIC X(3)    VALUE SPACES.
           03  FILLER    PIC X(17)   VALUE "---RESCHEDULED---".
           03  FILLER    PIC X(3)    VALUE SPACES.
           03  FILLER    PIC X(6)    VALUE "WORKED".
           03  FILLER    PIC X(40)   VALUE SPACES.

       
       01  HEAD6.
           03  FILLER    PIC X(11)   VALUE "ACCOUNT NO.".
           03  FILLER    PIC X(4)    VALUE SPACES.
           03  FILLER    PIC X(25)   VALUE "------BORROWER NAME------".
           03  FILLER    PIC X(4)    VALUE SPACES.
           03  FILLER    PIC X(3)    VALUE "DEL".
           03  FILLER    PIC X(2)    VALUE SPACES.
           03  FILLER    PIC X(6)    VALUE " DUE  ".
           03  FILLER    PIC X(7)    VALUE "BALANCE".
           03  FILLER    PIC X(4)    VALUE SPACES.
           03  FILLER    PIC X(4)    VALUE "DATE".
           03  FILLER    PIC X(6)    VALUE SPACES.
           03  FILLER    PIC X(4)    VALUE "TIME".
           03  FILLER    PIC X(5)    VALUE SPACES.
           03  FILLER    PIC X(2)    VALUE "BY".
           03  FILLER    PIC X(7)    VALUE SPACES.
           03  FILLER    PIC X(10)   VALUE "HOME-PHONE".
           03  FILLER    PIC X(6)    VALUE SPACES.
           03  FILLER    PIC X(10)   VALUE "WORK-PHONE".
           03  FILLER    PIC X(12)   VALUE SPACES.

      *****************************************************************
      *
      *    D E T A I L  L I N E
      *
      *****************************************************************
       01  DETAIL-LINE      PIC X(132)   VALUE SPACES.

       
       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  D1-LN-ACCTNO        PIC X(13).
           03  FILLER              PIC X(2).
           03  D1-BW-NAME          PIC X(25).
           03  FILLER              PIC X(2).
           03  D1-CON-DEL          PIC ZZZ99.
           03  FILLER              REDEFINES D1-CON-DEL.
               05  FILLER          PIC X(2).
               05  D1-CON-DELX     PIC X(3).
           03  D1-PDUE-DAYS        PIC Z(6).
           03  FILLER              PIC X(2).
           03  D1-CURRENT-BALANCE  PIC ZZZ,ZZ9.
           03  FILLER              PIC X(2).
           03  D1-RESCHEDULE-DATE  PIC 99/99/99 BLANK ZERO.
           03  FILLER              PIC X(2).
           03  D1-HOUR             PIC Z9.
           03  D1-COLON            PIC X(1).
           03  D1-MINUTES          PIC 99.
           03  FILLER              PIC X(1).
           03  D1-AM-PM            PIC X(2).
           03  FILLER              PIC X(3).
           03  D1-WORK-COLLID      PIC X(3).
           03  FILLER              PIC X(4).
           03  D1-HOME-PHONE       PIC X(14).
           03  FILLER              PIC X(2).
           03  D1-WORK-PHONE       PIC X(14).
           03  FILLER              PIC X(10).

       
       01  DETAIL2 REDEFINES DETAIL-LINE.
      *    03  D2-PROMPT-1         PIC X(15).
      *    03  FILLER              PIC X(1).
      *    03  D2-TOT-ACCTS        PIC ZZZZ9.
      *    03  FILLER              PIC X(5).
      *    03  D2-PROMPT-2         PIC X(13).
      *    03  FILLER              PIC X(1).
      *    03  D2-TO-BE-WORKED     PIC ZZZZ9.
      *    03  FILLER              PIC X(5).
           03  D2-PROMPT-3         PIC X(12).

      *    03  FILLER              PIC X(1).
           03  D2-RESCHEDULED      PIC ZZZZ9.

      *    03  FILLER              PIC X(5).
      *    03  D2-PROMPT-4         PIC X(7).
      *    03  FILLER              PIC X(1).
      *    03  D2-WORKED           PIC ZZZZ9.
      *    03  FILLER              PIC X(46).

       
       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(35).
           03  RANGE-SELECTION.
               04  R-NUM        PIC Z(5)9.
               04  R-CHAR       REDEFINES R-NUM
                                PIC X(6).
      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBSP/COLRPT_CMD.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/CQRELS_DEF.CPY".
       COPY "LIBLS/CQRELS_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLS/CQRELS_SCN.CPY".

       
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE VCQ-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-CQ1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE VCQ-FILE.
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
           MOVE "CQRELS" TO FORM-PROG VDU-FORM-NAME.

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

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              CQ-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE.
           
           MOVE FILPATH-BASE               TO VCQ-ALL-BASE.
           MOVE FILPATH-MACHINE            TO VCQ-ALL-MACHINE.
           MOVE FILPATH-BRANCH             TO VCQ-ALL-BRNO.
           MOVE VCQ-ALL-PATH               TO VCQ-PATH.
           MOVE VCQ-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              GO TO NEXT-GROUP-LOC.

           PERFORM OPEN-VCQ2-FILE.

           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN1-FILE.

           PERFORM SET-BRANCH-HEADINGS.
           MOVE "Y"        TO FIRST-TIME.
           MOVE "N"        TO WS-COLL-TOTAL-FG.

           MOVE SPACES                     TO CQ-REC.
           MOVE CQ-PATH-OWNBR              TO CQ-BRNO.
           MOVE BEG-COLLID                 TO CQ-COLLID.
           MOVE SPACES                     TO CQ-SEQ-FG
                                              CQ-MISC-KEY-FLD.
           MOVE ZEROES                     TO CQ-REV-BEHAVIOR-RISK 
                                              CQ-DATE
                                              CQ-START-TIME
                                              CQ-REV-DEL
                                              CQ-REV-PDUE-DAYS
                                              CQ-REV-BAL
                                              CQ-ACCTNO
                                              CQ-SEQ.
           PERFORM LOAD-VCQ2-KEY.
           PERFORM START-VCQ2-FILE.

       
       NEXT-CQ-REPORT.
           PERFORM READ-VCQ2-FILE-NEXT.
           IF ( IO-FG = 9 )
              GO TO END-OF-GROUP.

           MOVE VCQ-CQ-REC           TO CQ-REC.

      * CHECKS FOR IF RECORDS MATCHES PARAMETERS ENTERED.
       
           IF ( CQ-COLLID = SPACES )
              GO TO NEXT-CQ-REPORT.

           IF CQ-WORKED-FG NOT = "R"
              GO TO NEXT-CQ-REPORT.

           IF HOLD-RESCHD-DATE > CQ-DATE
              GO TO NEXT-CQ-REPORT.

           IF CQ-COLLID > END-COLLID
              GO TO END-OF-GROUP.

           IF ( FIRST-TIME = "Y" )
              MOVE "Y"       TO WS-COLL-TOTAL-FG
              MOVE CQ-COLLID TO HOLD-COLLECTOR
              PERFORM SET-COLLECTOR-HEADINGS
           ELSE
           IF ( CQ-COLLID NOT = HOLD-COLLECTOR )
              PERFORM COLLECTOR-TOTALS.

           PERFORM PRINT-DETAIL-LINE.

           ADD 1 TO COLL-RESCHD-TOTAL.

           GO TO NEXT-CQ-REPORT.
       
       END-OF-GROUP.

           IF ( WS-COLL-TOTAL-FG = "Y" )
              PERFORM COLLECTOR-TOTALS.

           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-VCQ2-FILE.

           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           IF NO-RECORDS-FOUND
              GO TO END-PROGRAM
           ELSE
              PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE CQRELS-QUEUE-POS TO Q-POS.
           MOVE CQRELS-COPIES-POS TO C-POS.
           MOVE CQRELS-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".
 
           MOVE "N" TO RECORDS-FOUND.
           MOVE 0 TO PAGE-NUMBER.

           MOVE EXT-CONAME-CONAME  TO H1-NAME.
           MOVE EXT-CONAME-SYSDATE TO H1-DATE.
           MOVE EXT-CONAME-USER-ID TO H2-USERID.

           IF BEG-BRANCH = 0
              IF END-BRANCH = 0
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH
                                             END-BRANCH.

           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BRANCH TO SV-BEG-BR
              MOVE END-BRANCH TO SV-END-BR
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP TO SV-ENT-GROUP
              MOVE EXT-EOBUF-GROUP TO ENT-GROUP
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              MOVE "Y" TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE.


           MOVE "D" TO DATER-UNITPER-CD.
           MOVE 1 TO DATER-UNITPER-FREQ.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO NDTE-DATE.
           MOVE RESCHD-XDAYS  TO NDTE-HOLD.
           PERFORM INCREMENT-UNITPER.
           MOVE NDTE-DATE     TO HOLD-RESCHD-DATE.

           IF EXT-ROUTE-BUF = "00"
              MOVE "RESCHEDULE LISTING IN PROGRESS..." TO MESS
              MOVE MESS-LIST TO WR-LIST
              MOVE MESS TO WR-BUF
              PERFORM CALL-WRFORM.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.

           IF ( VDUSTATUS = "1U" )
              GO TO RESCHEDULE-XDAYS.

       
       BEGIN-BRANCH.
           MOVE LEGEND-BR-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "R  A000BEG-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.

           IF ( VDUSTATUS = "1<" )
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO BEGIN-BRANCH
              ELSE
                 MOVE "S  A000BEG-BRANCH." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.

           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEGIN-BRANCH.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF WGR-WORD-ZERO
              MOVE 0 TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.

           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO BEGIN-BRANCH.

           IF WGR-TYPE = "A"
              MOVE "S  A000END-BRANCH." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.

       
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BEG-BRANCH." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.

       
       ENDING-BRANCH.
           MOVE LEGEND-BR-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040END-BRANCH." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BEGIN-BRANCH.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENDING-BRANCH.
           MOVE VDUNUM0 TO END-BRANCH.

           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEGIN-BRANCH.

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
           MOVE "SNNN040BEG-BRANCH." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040END-BRANCH." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "S  A160DISPLAY-USER." TO VDU.
           MOVE SPACES TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

       
       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BEGIN-BRANCH.

       
       BEGIN-COLLECTOR.

           MOVE LEGEND-COLL-01 TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A030BEG-COLLID." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "16" ) GO TO ALL-COLLECTORS.
           IF ( VDUSTATUS = "1U" ) GO TO ENDING-BRANCH.
           IF ( VDUSTATUS = "1>" ) GO TO EXIT-PARM.

           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEGIN-COLLECTOR.
           MOVE VDUBUF TO BEG-COLLID.

       
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

           GO TO RESCHEDULE-XDAYS.


       
       ALL-COLLECTORS.
           MOVE "S  A030BEG-COLLID." TO VDU.
           MOVE SPACES             TO BEG-COLLID VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A030END-COLLID." TO VDU.
           MOVE "ZZZ"              TO END-COLLID VDUBUF.
           PERFORM SCREENIO.


       
       RESCHEDULE-XDAYS.

           MOVE "EN N020RESDAYS." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1U" ) GO TO ENDING-COLLECTOR.
           IF ( VDUSTATUS = "1>" ) GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO RESCHEDULE-XDAYS.

           MOVE VDUNUM0 TO RESCHD-XDAYS.

       END-PARM.
           MOVE LEGEND-ACCEPT TO LEGEND.
           PERFORM SEND-LEGEND.

       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************
       
           IF GROUP-FLAG = "Y"
              MOVE "S  A000BEG-BRANCH." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000END-BRANCH." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BEG-BRANCH." TO VDU
              MOVE BEG-BRANCH TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040END-BRANCH." TO VDU
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
       
           MOVE "S  A030BEG-COLLID." TO VDU.
           MOVE BEG-COLLID           TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A030END-COLLID." TO VDU.
           MOVE END-COLLID           TO VDUBUF.
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

           MOVE CQ-ACCTNO TO LN-ACCTNO.
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
                 PERFORM SET-CQ-LNAME
                 MOVE MESS TO D1-BW-NAME
              END-IF
              PERFORM LPSTAT-DISPLAY
              MOVE LPS-DISPLAY-LNNO TO D1-LN-ACCTNO.

       
           IF BR-COL-QUEUE-LOW-TO-HI = "Y"
              IF ( CQ-REV-DEL = 15 )
                 MOVE "POT" TO D1-CON-DELX
              ELSE
                 MOVE CQ-REV-DEL TO D1-CON-DEL
           ELSE
              IF ( CQ-REV-DEL = 98985 )
                 MOVE "POT" TO D1-CON-DELX
              ELSE
                 COMPUTE DEL-WK =  99000 - CQ-REV-DEL
                 MOVE DEL-WK TO D1-CON-DEL.

           IF CQ-REV-PDUE-DAYS NOT = 0
              IF BR-COL-QUEUE-LOW-TO-HI = "Y"
                 MOVE CQ-REV-PDUE-DAYS TO D1-PDUE-DAYS
              ELSE
                 COMPUTE DEL-WK =  99999 - CQ-REV-PDUE-DAYS
                 MOVE DEL-WK TO D1-PDUE-DAYS.
       
           MOVE CQ-WORK-COLLID  TO D1-WORK-COLLID.

           MOVE CQ-DATE     TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D1-RESCHEDULE-DATE.

       
           COMPUTE BAL-WK ROUNDED = LN-CURBAL - LN-OT2BAL.
           MOVE BAL-WK TO D1-CURRENT-BALANCE.

       
           IF ( CQ-START-TIME = 0 )
              MOVE 1 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              GO TO EXIT-PRINT-DETAIL-LINE.

           MOVE CQ-START-TIME TO WK-START-TIME.

           IF ( WK-START-HH > 11 )
              MOVE "PM" TO D1-AM-PM
           ELSE
              MOVE "AM" TO D1-AM-PM.

           IF ( WK-START-HH > 12 )
              SUBTRACT 12 FROM WK-START-HH.

           MOVE WK-START-HH TO D1-HOUR.
           MOVE ":"         TO D1-COLON.
           MOVE WK-START-MM TO D1-MINUTES.

       
           MOVE 1                TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       EXIT-PRINT-DETAIL-LINE.
           EXIT.

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
       SET-BRANCH-HEADINGS SECTION.
      ******************************************************************
           MOVE BR-NO     TO H3-BRNO.
           MOVE BR-NAME   TO H3-BRDESC.

      ******************************************************************
       SET-COLLECTOR-HEADINGS SECTION.
      ******************************************************************
           MOVE CQ-COLLID  TO H4-COLLID.
           PERFORM GET-COLLID-DESC.
           MOVE CDB-DESC TO H4-COLL-DESC.

      ******************************************************************
       COLLECTOR-TOTALS SECTION.
      ******************************************************************
       
           MOVE PROMPT-COLL-TOT-03 TO D2-PROMPT-3.
           MOVE COLL-RESCHD-TOTAL TO D2-RESCHEDULED.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 0 TO COLL-RESCHD-TOTAL.

           MOVE CQ-COLLID TO HOLD-COLLECTOR.
           PERFORM SET-COLLECTOR-HEADINGS.
           MOVE 99 TO LN-COUNT.

       EXIT-COLLECTOR-TOTALS.
           EXIT.

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
       
           MOVE HEAD2 TO PR-REC.
           MOVE 1     TO LN-FEED.
           PERFORM WRITE-PR-REC.
       
           MOVE HEAD3 TO PR-REC.
           PERFORM WRITE-PR-REC.
       
           MOVE HEAD4 TO PR-REC.
           MOVE 2     TO LN-FEED.
           PERFORM WRITE-PR-REC.
       
           MOVE HEAD5 TO PR-REC.
           MOVE 2     TO LN-FEED.
           PERFORM WRITE-PR-REC.
       
           MOVE HEAD6 TO PR-REC.
           MOVE 1     TO LN-FEED.
           PERFORM WRITE-PR-REC.
       
           MOVE ALL "-" TO PR-REC.
           MOVE 1       TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE 10 TO LN-COUNT.
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
       
           IF ( GROUP-FLAG = "Y" )
              MOVE "BRANCH GROUP" TO RANGE-LINE
              MOVE ENT-GROUP      TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEG BRANCH" TO RANGE-LINE
              MOVE BEG-BRANCH   TO R-NUM
              PERFORM WRITE-DETAIL-LINE
              MOVE "END BRANCH" TO RANGE-LINE
              MOVE END-BRANCH   TO R-NUM
              PERFORM WRITE-DETAIL-LINE.
       
           MOVE "BEG COLLECTOR" TO RANGE-LINE.
           MOVE BEG-COLLID      TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "END COLLECTOR" TO RANGE-LINE.
           MOVE END-COLLID      TO R-CHAR.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "MORE THAN"     TO RANGE-LINE.
           MOVE RESCHD-XDAYS    TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "   DAYS"       TO RANGE-LINE.
           PERFORM WRITE-DETAIL-LINE.

       
      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPMDTE.CPY".

      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO CQRELS-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CQRELS-SYSDATE.
           DISPLAY CQRELS-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE CQRELS-FORM-FIELDS TO WR-BUF.
           MOVE CQRELS-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE CQRELS-HELP--FILE TO HELP-LINK-FILE.
           MOVE CQRELS-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/CQRELS_EVA.CPY".
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

       SEND-LEGEND.
           MOVE MSEL-LIST TO WR-LIST.
           MOVE LEGEND TO WR-BUF.
           PERFORM CALL-WRFORM.

       GET-COLLID-DESC.
           MOVE "CN"      TO CDB-TYPE.
           MOVE BR-NO    TO CDB-BRANCH.
           MOVE CQ-COLLID TO CDB-CODE.
           PERFORM OPEN-CDB1-FILE.
           PERFORM READ-CDB1-FILE.
           PERFORM CLOSE-CDB1-FILE.

           IF ( IO-FG NOT = 0 )
              MOVE "<NOT ON FILE>" TO CDB-DESC.
       
       SET-CQ-LNAME.
           INSPECT MESS REPLACING FIRST "  " BY ",?".
           INSPECT MESS REPLACING FIRST "                "
               BY A-NAME.
           INSPECT MESS REPLACING ALL "?" BY " ".

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
       COPY "LIBSP/SPCQGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBSP/SPCQ1RN.CPY".
       COPY "LIBSP/SPVCQ2RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
             MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       
       END-RUN.
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF ( NO-RECORDS-FOUND )
              IF ( EXT-ROUTE-BUF = "00" OR "70" )
                 MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
                 PERFORM SEND-MESS.
       
       EXIT-PROG.
           IF ( SV-BEG-BR NOT = 9999 )
              MOVE SV-BEG-BR TO BEG-BRANCH
              MOVE SV-END-BR TO END-BRANCH.

           IF ( EXT-EOBUF-GROUP-FG = "Y" )
              MOVE SV-BEG-BR TO BEG-BRANCH
              MOVE SV-END-BR TO END-BRANCH
              MOVE SV-GROUP-FLAG TO GROUP-FLAG
              MOVE SV-ENT-GROUP TO ENT-GROUP.

           IF EXT-ROUTE-BUF = "00"
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY CQRELS-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
