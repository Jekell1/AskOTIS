       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CQFILE.
       DATE-WRITTEN.  11-15-2023.
      ******************************************************************
      *
      * DESCRIPTION   : COMPARE S35 CQFILE WITH A15 CQFILE
      *            
      *                 TO BE USED AFTER AN UPDATE
      *                 WILL COMPARE EACH FIELD AND REPORT ANY 
      *                 DIFFERENCES, AS LONG AS FIELD EXIST IN BOTH FILES
      *
      *  LOOPS THRU THE A15 FILE AND VERIFIES THE S35 FILE EXIST AND MATCHES
      * 
      * MB  20240620 CORRECTED REPORT FIELD NAME NO-DATE-DATE TO CQ-DATE
      *              CORRECTED OCQ-DATE AND OCQ-DATE-QUEUED BEING MOVEED
      *                 TO WRONG DATE FORMAT.
      *              COMMENTED OUT CQ-MISC-KEY-FLD CHECK - A15 CONTAINS GARBAGE
      * MB  20250313 CHANGED TO USE OCQ2 INSTEAD OF OCQ1               E
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
      * COPYMEMBER: LIBSP/SPFSCQ
           SELECT OCQ-FILE ASSIGN TO OCQ-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OCQ1-KEY
                  ALTERNATE RECORD KEY OCQ2-KEY WITH DUPLICATES
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  OCQ-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A15SRC/SP01CQ.CPY" REPLACING LEADING "CQ" BY "OCQ".

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)UT/CQFILE.CBL S35 11/03/23-10:01:20 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBSP/SP01CQ.CPY".
       COPY "LIBSP/SP01CQ_SQL.CPY".
       COPY "LIBSP/SPWSCQ.CPY".
       COPY "LIBSP/SP01CQS.CPY".
       COPY "LIBSP/SP01CQS_SQL.CPY".
       COPY "LIBSP/SPWSCQS.CPY".
       01  OCQ-PATH                    PIC X(35) VALUE SPACES.
       01  OCQ-ALL-PATH.
           03  OCQ-ALL-BASE     PIC X(9).
           03  FILLER           PIC X     VALUE "/".
           03  OCQ-ALL-MACHINE  PIC XX.
           03  FILLER           PIC X     VALUE "/".
           03  OCQ-ALL-DATA     PIC X(6).
           03  FILLER           PIC X(10) VALUE
               "/SP/CQFILE".

      *********************************************************
      *        SQL DECLARE SECTION - WORKERS
      *********************************************************

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  PASS-SQL                    PIC X(200).
       01  RET-DB-NAME                 PIC X(50).
       01  RET-SERVER-NAME             PIC X(50).
       01  RET-VERSION                 PIC X(100).
       01  MISC-RETURN-CODE            PIC S9(4)   COMP-5.
       EXEC SQL END DECLARE SECTION END-EXEC.

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************

       01  MACHINE-R1.
           03  FILLER                  PIC X VALUE X"72".
           03  FILLER                  PIC 9 VALUE 1.
       01  MACHINE-R2.
           03  FILLER                  PIC X VALUE X"72".
           03  FILLER                  PIC 9 VALUE 2.

       01  OLD-ACCTNO                  PIC 9(8).
       01  FILLER REDEFINES OLD-ACCTNO.
           03  NEW-ACCTNO              PIC 9(6).
           03  FILLER                  PIC 99.

       01  MISC-CURSOR-STAT            PIC X(2) VALUE SPACES.
           88  MISC-CURSOR-STAT-GOOD            VALUE "00".
           88  MISC-CURSOR-STAT-BAD             VALUE "  " THRU "//"
                                                      "01" THRU "~~".

       01  PROCESS-STAT                PIC X(08)    VALUE SPACES.
           88  PROCESS-STAT-GOOD                    VALUE "00".
           88  PROCESS-STAT-BAD                     VALUE "  " THRU "//"
                                                         "01" THRU "~~".

       01  TOTAL-ERRORS-CTR            PIC 9(9)     VALUE 0.
       01  S35-CQS-RECORDS-PROCESSED   PIC 9(9)     VALUE 0.
       01  S35-CQ-RECORDS-PROCESSED    PIC 9(9)     VALUE 0.
       01  A15-RECORDS-PROCESSED       PIC 9(9)     VALUE 0.
           88  NO-RECORDS-FOUND                     VALUE ZEROES.

       01  STARTING-TIME               PIC X(08)     VALUE SPACES.
       01  SUB                         PIC 9(04)     VALUE ZEROES.

       01  SYSDATE-YYYYMMDD            PIC 9(8)     VALUE ZEROES.

       01  LEGEND                      PIC X(79)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(07)     VALUE "LEGEND.".


      *-----------------------------------------------------------------
       01  LEGEND-ACCEPT-YN.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT             ".
           03  FILLER PIC X(30) VALUE "                              ".
           03  FILLER PIC X(19) VALUE "     ACCEPT (Y/N): ".

       01  SOURCE-PATH-LIST            PIC X(07)     VALUE "S-PATH.".

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  SOURCE-PATH             PIC X(9).
           03  BEG-BRANCH              PIC 9(04).
           03  END-BRANCH              PIC 9(04).
           03  GROUP-FLAG              PIC X(01).
           03  ENT-GROUP               PIC X(04).

       01  VERSION-CONTROL             PIC X(01)     VALUE "A".

      *-----------------------------------------------------------------
       01  SV-BEG-BRANCH               PIC 9(04)     VALUE 9999.
       01  SV-END-BRANCH               PIC 9(04)     VALUE 9999.
       01  SV-GROUP-FLAG               PIC X(01)     VALUE SPACES.
       01  SV-ENT-GROUP                PIC X(04)     VALUE SPACES.

       01  LN-COUNT            PIC 9(03)     VALUE 99.
       01  LN-FEED             PIC 9(02).
       01  PAGE-NUMBER         PIC 9(04)     VALUE ZEROES.

      ******************************************************************
      *
      *    H E A D E R - L I N E - W O R K E R S
      *
      ******************************************************************
       01  HEADER-LINE-01.
           03  H01-DATE                PIC X(08) VALUE SPACES.
           03  FILLER                  PIC X(02) VALUE SPACES.
           03  H01-TIME                PIC X(08) VALUE SPACES.
           03  FILLER                  PIC X(6) VALUE SPACES.
           03  H01-NAME                PIC X(40) VALUE SPACES.
           03  FILLER                  PIC X(6) VALUE SPACES.
           03  FILLER                  PIC X(06) VALUE "PAGE: ".
           03  H01-PAGE                PIC Z(04).

      *-----------------------------------------------------------------
       01  HEADER-LINE-02.
           03  FILLER                  PIC X(10) VALUE "USER ID:".
           03  H02-USERID              PIC X(10) VALUE SPACES.
           03  FILLER                  PIC X(10) VALUE SPACES.
           03  H02-TITLE               PIC X(50) VALUE 
           "COLLECTOR QUEUE FILE (CQFILE) COMPARE A15 TO S35".

       01  HEADER-LINE-03.
           03  FILLER                PIC X(4) VALUE "BRNO".
           03  FILLER                PIC X(5) VALUE SPACES.
           03  FILLER                PIC X(6) VALUE "ACCTNO".
           03  FILLER                PIC X(5) VALUE SPACES.
           03  FILLER                PIC X(11) VALUE "DESCRIPTION".
           03  FILLER                PIC X(17) VALUE SPACES.
           03  FILLER                PIC X(3) VALUE "A15".
           03  FILLER                PIC X(19) VALUE SPACES.
           03  FILLER                PIC X(3) VALUE "S35".

      ******************************************************************
      *
      *    D E T A I L - L I N E - W O R K E R S
      *
      ******************************************************************
       01  DETAIL-LINE                        PIC X(80)   VALUE SPACES.

       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-BRNO                       PIC Z(3)9.
           03  FILLER                         PIC X(5).
           03  D01-ACCTNO                     PIC X(8).
           03  FILLER                         PIC X(2).
           03  D01-TEXT                       PIC X(25).
           03  FILLER                         PIC X(3).
           03  D01-A15                        PIC X(20).
           03  FILLER REDEFINES D01-A15.
               05  D01-A15-NUM                PIC Z(7)9.9(3).
               05  FILLER                     PIC X(8).
           03  FILLER                         PIC X(2).
           03  D01-S35                        PIC X(20).
           03  FILLER REDEFINES D01-S35.
               05  D01-S35-NUM                PIC Z(7)9.9(3).
               05  FILLER                     PIC X(8).

       01  RANGE-LINE-01                      REDEFINES DETAIL-LINE.
           03  R01-PROMPT                     PIC X(32).
           03  R01-FIELD                      PIC X(48).
           03  R01-AMT                        REDEFINES R01-FIELD
                                              PIC ZZZ,ZZZ,ZZ9.99-.
           03  R01-NUMERIC                    REDEFINES R01-FIELD
                                              PIC Z(10)9-.
           03  R01-TEXT                       REDEFINES R01-FIELD
                                              PIC X(48).

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/PROGIDW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBUT/CQFILE_DEF.CPY".
       COPY "LIBUT/CQFILE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/CQFILE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE OCQ-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-CQ1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE OCQ-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           PERFORM INITIALIZATION.

           IF ( PROCESS-STAT-BAD )
              GO TO MAIN-PROGRAM-EXIT.

           PERFORM EXECUTION-MODULE.

           PERFORM RANGE-LINES.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

           MOVE STAT---GOOD                TO PROCESS-STAT.

           MOVE FORM-PATHNAME              TO FORM-PATH.
           MOVE "CQFILE" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE CQFILE-QUEUE-POS TO Q-POS.
           MOVE CQFILE-COPIES-POS TO C-POS.
           MOVE CQFILE-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           PERFORM GET-TIME.
           MOVE TIME-EDIT                  TO STARTING-TIME.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS......" TO LEGEND
              PERFORM SEND-LEGEND.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO SYSDATE-YYYYMMDD.

           PERFORM START-MISC-CSR.
           PERFORM READ-MISC-CSR-NEXT.
           PERFORM CLOSE-MISC-CSR.

           IF ( EXT-EOBUF-GROUP-FG = "Y" )
              MOVE BEG-BRANCH              TO SV-BEG-BRANCH
              MOVE END-BRANCH              TO SV-END-BRANCH
              MOVE GROUP-FLAG              TO SV-GROUP-FLAG
              MOVE ENT-GROUP               TO SV-ENT-GROUP
              MOVE EXT-EOBUF-BRNO              TO BEG-BRANCH END-BRANCH
              MOVE "N"                     TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH      TO PRU-FILE
           ELSE
           IF ( BEG-BRANCH = ZEROES ) AND
              ( END-BRANCH = ZEROES )
              MOVE BEG-BRANCH              TO SV-BEG-BRANCH
              MOVE END-BRANCH              TO SV-END-BRANCH
              MOVE EXT-CONAME-USER-LOC     TO BEG-BRANCH
                                              END-BRANCH.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-PR-FILE.

           GO TO INITIALIZATION-EXIT.

       INITIALIZATION-END.

           MOVE "X"                        TO EXT-EOBUF-ERRCD.
           MOVE STAT---BAD                 TO PROCESS-STAT.

       INITIALIZATION-EXIT.
           EXIT.

      ***************************************************
       ENTER-PARAMETERS SECTION.
      ***************************************************

           MOVE STAT---GOOD                TO PROCESS-STAT.

       ENTER-SOURCE-PATH.

           IF SOURCE-PATH = SPACES
              MOVE "A15_PATH"       TO GETENV-BUF
              PERFORM GETENV-CALL
              IF ( STAT-GOOD )
                 MOVE GETENV-BUF    TO SOURCE-PATH
              ELSE
              MOVE "/USR/DATA"      TO SOURCE-PATH
              END-IF
              CALL "C$TOLOWER" USING SOURCE-PATH, VALUE 9
              MOVE SOURCE-PATH-LIST TO WR-LIST
              MOVE SOURCE-PATH      TO WR-BUF
              PERFORM CALL-WRFORM.

           MOVE "E  A090S-PATH." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF NOT ( ENTER-KEY ) GO TO ENTER-SOURCE-PATH.

           IF ( VDUBUF = SPACES )
              GO TO ENTER-SOURCE-PATH.

           MOVE VDUBUF TO DIR-ACCESS-BUF SOURCE-PATH.
           PERFORM DIR-ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE "INVALID SOURCE DATA PATH" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-SOURCE-PATH.

           MOVE SPACES                     TO DIR-ACCESS-BUF.
           STRING VDUBUF, "/", EXT-FILPATH-MACHINE
                  DELIMITED BY " "       INTO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE "INVALID SOURCE DATA PATH" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-SOURCE-PATH.

           MOVE VDUBUF                TO SOURCE-PATH.

       ENTER-BEG-BRANCH.

           MOVE "E RA000BEG-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F3-KEY ) GO TO ENTER-ALL-BRANCH.
           IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-BRANCH.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF ( WGR-WORD-ZERO )
              MOVE ZEROES TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.

           MOVE ZEROES TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF ( WGR-ERR > ZEROES )
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.

           IF ( WGR-TYPE = "A" )
              MOVE "S  A000END-BRANCH." TO VDU
              MOVE SPACES               TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y"                  TO GROUP-FLAG
              MOVE ZEROES               TO BEG-BRANCH
              MOVE 9999                 TO END-BRANCH
              GO TO CHECK-BR-SECURITY.

      *-----------------------------------------------------------------
       BRANCH-IS-NUMERIC.

           MOVE "N"           TO GROUP-FLAG.
           MOVE WGR-SRESULT   TO BEG-BRANCH.

           MOVE "SN N040BEG-BRANCH." TO VDU.
           MOVE BEG-BRANCH           TO VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       ENTER-END-BRANCH.

           MOVE "ENNN040END-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-BRANCH.

           MOVE VDUNUM0 TO END-BRANCH.

           IF ( BEG-BRANCH > END-BRANCH )
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.

           GO TO CHECK-BR-SECURITY.

      *-----------------------------------------------------------------
       ENTER-ALL-BRANCH.

           MOVE "SNNN040BEG-BRANCH." TO VDU.
           MOVE ZEROES               TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040END-BRANCH." TO VDU.
           MOVE 9999                 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "N"                    TO GROUP-FLAG.

      *-----------------------------------------------------------------
       CHECK-BR-SECURITY.

           PERFORM BR-RANGE-SECURITY.

           IF ( BR-SECURITY-OKAY = "N" ) GO TO ENTER-BEG-BRANCH.

       ENTER-PARAMETERS-END.

           MOVE LEGEND-ACCEPT-YN TO LEGEND.
           PERFORM SEND-LEGEND.

           IF ( F1-KEY ) MOVE STAT---BAD   TO PROCESS-STAT.
           IF ( F7-KEY ) MOVE STAT---BAD   TO PROCESS-STAT.

       ENTER-PARAMETERS-EXIT.
           EXIT.

      ****************************************
       EO-EXEC-AUDIT SECTION.
      ****************************************

      ****************************************
       DISPLAY-PARAMETERS SECTION.
      ****************************************

           IF ( GROUP-FLAG = "Y" )
              MOVE "S  A000BEG-BRANCH."    TO VDU
              MOVE ENT-GROUP               TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000END-BRANCH."    TO VDU
              MOVE SPACES                  TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BEG-BRANCH."    TO VDU
              MOVE BEG-BRANCH              TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040END-BRANCH."    TO VDU
              MOVE END-BRANCH              TO VDUNUM0
              PERFORM SCREENIO.

           MOVE "S  A090S-PATH."        TO VDU.     
           MOVE SOURCE-PATH             TO VDUBUF.
           PERFORM SCREENIO.

       DISPLAY-PARAMETERS-EXIT.
           EXIT.

      *********************************************
       SCAN-GR1-FILE SECTION.
      *********************************************

           MOVE "WI/GRSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF ( GRSCAN-STATUS NOT = "00"   ) OR
              ( GRSCAN-KEY      = SPACES )
              MOVE "XX" TO VDUSTATUS
              GO TO SCAN-GR1-FILE-EXIT.

           MOVE "S  A000BEG-BRANCH." TO VDU.
           MOVE GRSCAN-KEY           TO VDUBUF.
           PERFORM SCREENIO.

           MOVE GRSCAN-KEY TO VDUBUF.
           MOVE "00"       TO VDUSTATUS.

       SCAN-GR1-FILE-EXIT.
           EXIT.

      *******************************************
       EXECUTION-MODULE SECTION.
      *******************************************

           IF ( GROUP-FLAG = "N" )
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO EXECUTION-NEXT-BRANCH.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

       EXECUTION-NEXT-GROUP.
           IF ( GROUP-FLAG NOT = "Y" )
              GO TO EXECUTION-NEXT-BRANCH.

           PERFORM SEARCH-GR-NEXT.
           IF ( WGR-SRESULT = 0 )
              GO TO EXECUTION-MODULE-EXIT.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO EXECUTION-PROCESS-BRANCH.

       EXECUTION-NEXT-BRANCH.
           PERFORM READ-BR-FILE-NEXT.
           IF ( IO-BAD             ) OR
              ( BR-NO > END-BRANCH )
              GO TO EXECUTION-MODULE-EXIT.

       EXECUTION-PROCESS-BRANCH.

           PERFORM PROCESS-BRANCH-DRIVER.

           GO TO EXECUTION-NEXT-GROUP.

       EXECUTION-MODULE-EXIT.
           EXIT.

     *********************************************************
       PROCESS-BRANCH-DRIVER SECTION.
     *********************************************************

           MOVE 9999                       TO PROGID-SLEEP-COUNTER.
           PERFORM SEND-PROGRESS-INDICATOR.

      D    STOP MESS.
           MOVE SOURCE-PATH  TO OCQ-ALL-BASE.
           MOVE MACHINE-R1   TO OCQ-ALL-MACHINE.
           MOVE BR-DATA-PATH TO OCQ-ALL-DATA.
           MOVE OCQ-ALL-PATH TO OCQ-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE MACHINE-R2   TO OCQ-ALL-MACHINE
              MOVE OCQ-ALL-PATH TO OCQ-PATH ACCESS-BUF
              PERFORM ACCESS-CALL
              IF ( STAT-BAD )
                 MOVE "MISSING A15 FILE" TO D01-TEXT
                 MOVE BR-NO              TO D01-BRNO
                 MOVE OCQ-ALL-PATH       TO D01-A15
                 PERFORM WRITE-DETAIL-LINE-ERROR
                 GO TO PROCESS-BRANCH-DRIVER-END.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO CQ-PATH-OVERRIDE
                                             CQS-PATH-OVERRIDE.

           PERFORM OPEN-CQS1-FILE.
           PERFORM OPEN-CQ1-FILE.
           PERFORM OPEN-OCQ2-FILE.

      *     MOVE SPACES            TO OCQ-TYPE
      *                               OCQ-HEAD-COLLID.

           MOVE SPACES TO OCQ2-KEY.

           PERFORM START-OCQ2-FILE.

       PROCESS-BRANCH-NEXT-CQ.
           PERFORM READ-OCQ2-FILE-NEXT.
           IF IO-BAD
              GO TO PROCESS-BRANCH-DRIVER-END.

           PERFORM SEND-PROGRESS-INDICATOR.

           ADD  01 TO A15-RECORDS-PROCESSED.

           IF OCQ-TYPE = "D"
              PERFORM COMPARE-DETAIL-FIELDS
           ELSE
              PERFORM COMPARE-SUMMARY-FIELDS.

           GO TO PROCESS-BRANCH-NEXT-CQ.

       PROCESS-BRANCH-DRIVER-END.

           PERFORM CLOSE-CQ1-FILE.
           PERFORM CLOSE-CQS1-FILE.

       PROCESS-BRANCH-DRIVER-EXIT.
           EXIT.

      ******************************************************************
       COMPARE-DETAIL-FIELDS SECTION.
      ******************************************************************

           MOVE BR-NO             TO CQ-BRNO.
           MOVE OCQ-ACCTNO        TO OLD-ACCTNO.
           MOVE NEW-ACCTNO        TO CQ-ACCTNO.
           PERFORM READ-CQ1-FILE.
           IF IO-FG NOT = 0
              MOVE "MISSING S35 DETAIL RECORD" TO D01-TEXT
              MOVE BR-NO        TO D01-BRNO
              MOVE CQ-ACCTNO    TO D01-ACCTNO
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-DETAIL-FIELDS
           ELSE
              ADD 1 TO S35-CQ-RECORDS-PROCESSED.

           IF OCQ-COLLID NOT = CQ-COLLID
              MOVE "CQ-COLLID" TO D01-TEXT
              MOVE CQ-BRNO     TO D01-BRNO
              MOVE CQ-ACCTNO   TO D01-ACCTNO
              MOVE OCQ-COLLID  TO D01-A15
              MOVE CQ-COLLID   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OCQ-REV-BEHAVIOR-RISK NOT NUMERIC
              MOVE 0 TO OCQ-REV-BEHAVIOR-RISK.
           IF OCQ-REV-BEHAVIOR-RISK NOT = CQ-REV-BEHAVIOR-RISK
              MOVE "CQ-REV-BEHAVIOR-RISK" TO D01-TEXT
              MOVE BR-NO                  TO D01-BRNO
              MOVE CQ-ACCTNO              TO D01-ACCTNO
              MOVE OCQ-REV-BEHAVIOR-RISK  TO D01-A15-NUM
              MOVE CQ-REV-BEHAVIOR-RISK   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OCQ-SEQ-FG NOT = CQ-SEQ-FG
              MOVE "CQ-SEQ-FG" TO D01-TEXT
              MOVE CQ-BRNO     TO D01-BRNO
              MOVE CQ-ACCTNO   TO D01-ACCTNO
              MOVE OCQ-SEQ-FG  TO D01-A15
              MOVE CQ-SEQ-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OCQ-DATE TO DATE-YYMMDD.
           PERFORM CONVERT-YYMMDD-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = CQ-DATE
              MOVE "CQ-DATE"      TO D01-TEXT
              MOVE BR-NO          TO D01-BRNO
              MOVE CQ-ACCTNO      TO D01-ACCTNO
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE CQ-DATE        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OCQ-START-TIME NOT NUMERIC
              MOVE 0 TO OCQ-START-TIME.
           IF OCQ-START-TIME NOT = CQ-START-TIME
              MOVE "CQ-START-TIME" TO D01-TEXT
              MOVE BR-NO           TO D01-BRNO
              MOVE CQ-ACCTNO       TO D01-ACCTNO
              MOVE OCQ-START-TIME  TO D01-A15
              MOVE CQ-START-TIME   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OCQ-REV-DEL NOT NUMERIC
              MOVE 0 TO OCQ-REV-DEL.
           IF OCQ-REV-DEL NOT = CQ-REV-DEL
              MOVE "CQ-REV-DEL" TO D01-TEXT
              MOVE BR-NO        TO D01-BRNO
              MOVE CQ-ACCTNO    TO D01-ACCTNO
              MOVE OCQ-REV-DEL  TO D01-A15
              MOVE CQ-REV-DEL   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

      * A15 CQ-MISC-KEY-FLD CONTAINS GARBAGE
      *     IF OCQ-MISC-KEY-FLD NOT = CQ-MISC-KEY-FLD
      *        MOVE "CQ-MISC-KEY-FLD" TO D01-TEXT
      *        MOVE CQ-BRNO           TO D01-BRNO
      *        MOVE CQ-ACCTNO         TO D01-ACCTNO
      *        MOVE OCQ-MISC-KEY-FLD  TO D01-A15
      *        MOVE CQ-MISC-KEY-FLD   TO D01-S35
      *        PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OCQ-REV-PDUE-DAYS NOT NUMERIC
              MOVE 0 TO OCQ-REV-PDUE-DAYS.
           IF OCQ-REV-PDUE-DAYS NOT = CQ-REV-PDUE-DAYS
              MOVE "CQ-REV-PDUE-DAYS" TO D01-TEXT
              MOVE BR-NO              TO D01-BRNO
              MOVE CQ-ACCTNO          TO D01-ACCTNO
              MOVE OCQ-REV-PDUE-DAYS  TO D01-A15
              MOVE CQ-REV-PDUE-DAYS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OCQ-REV-BAL NOT NUMERIC
              MOVE 0 TO OCQ-REV-BAL.
           IF OCQ-REV-BAL NOT = CQ-REV-BAL
              MOVE "CQ-REV-BAL" TO D01-TEXT
              MOVE BR-NO        TO D01-BRNO
              MOVE CQ-ACCTNO    TO D01-ACCTNO
              MOVE OCQ-REV-BAL  TO D01-A15
              MOVE CQ-REV-BAL   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
      
           IF OCQ-WORK-COLLID NOT = CQ-WORK-COLLID
              MOVE "CQ-WORK-COLLID" TO D01-TEXT
              MOVE CQ-BRNO          TO D01-BRNO
              MOVE CQ-ACCTNO        TO D01-ACCTNO
              MOVE OCQ-WORK-COLLID  TO D01-A15
              MOVE CQ-WORK-COLLID   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OCQ-WORKED-FG NOT = CQ-WORKED-FG
              MOVE "CQ-WORKED-FG" TO D01-TEXT
              MOVE CQ-BRNO        TO D01-BRNO
              MOVE CQ-ACCTNO      TO D01-ACCTNO
              MOVE OCQ-WORKED-FG  TO D01-A15
              MOVE CQ-WORKED-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OCQ-IN-PROCESS NOT = CQ-IN-PROCESS
              MOVE "CQ-IN-PROCESS" TO D01-TEXT
              MOVE CQ-BRNO         TO D01-BRNO
              MOVE CQ-ACCTNO       TO D01-ACCTNO
              MOVE OCQ-IN-PROCESS  TO D01-A15
              MOVE CQ-IN-PROCESS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OCQ-DATE-QUEUED TO DATE-YYMMDD.
           PERFORM CONVERT-YYMMDD-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = CQ-DATE-QUEUED
              MOVE "CQ-DATE-QUEUED" TO D01-TEXT
              MOVE BR-NO            TO D01-BRNO
              MOVE CQ-ACCTNO        TO D01-ACCTNO
              MOVE DATE-YYYYMMDD    TO D01-A15
              MOVE CQ-DATE-QUEUED   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OCQ-TIME-ZONE-ERROR NOT = CQ-TIME-ZONE-ERROR
              MOVE "CQ-TIME-ZONE-ERROR" TO D01-TEXT
              MOVE CQ-BRNO              TO D01-BRNO
              MOVE CQ-ACCTNO            TO D01-ACCTNO
              MOVE OCQ-TIME-ZONE-ERROR  TO D01-A15
              MOVE CQ-TIME-ZONE-ERROR   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

       EXIT-COMPARE-DETAIL-FIELDS.
           EXIT.

      ******************************************************************
       COMPARE-SUMMARY-FIELDS SECTION.
      ******************************************************************

           MOVE BR-NO             TO CQS-BRNO.
           MOVE OCQ-COLLID        TO CQS-COLLID.
           PERFORM READ-CQS1-FILE.
           IF IO-FG NOT = 0
              MOVE "MISSING S35 SUMMARY RECORD" TO D01-TEXT
              MOVE BR-NO                        TO D01-BRNO
              MOVE CQS-COLLID                   TO D01-ACCTNO
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-SUMMARY-FIELDS
           ELSE
              ADD 1 TO S35-CQS-RECORDS-PROCESSED.

           IF OCQ-TOT-REC NOT NUMERIC
              MOVE 0 TO OCQ-TOT-REC.
           IF OCQ-TOT-REC NOT = CQS-TOT-REC
              MOVE "CQS-TOT-REC" TO D01-TEXT
              MOVE BR-NO         TO D01-BRNO
              MOVE CQS-COLLID    TO D01-ACCTNO
              MOVE OCQ-TOT-REC   TO D01-A15
              MOVE CQS-TOT-REC   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OCQ-WORKED-REC NOT NUMERIC
              MOVE 0 TO OCQ-WORKED-REC.
           IF OCQ-WORKED-REC NOT = CQS-WORKED-REC
              MOVE "CQS-WORKED-REC" TO D01-TEXT
              MOVE BR-NO            TO D01-BRNO
              MOVE CQS-COLLID       TO D01-ACCTNO
              MOVE OCQ-WORKED-REC   TO D01-A15
              MOVE CQS-WORKED-REC   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OCQ-TIMED-REC NOT NUMERIC
              MOVE 0 TO OCQ-TIMED-REC.
           IF OCQ-TIMED-REC NOT = CQS-TIMED-REC
              MOVE "CQS-TIMED-REC" TO D01-TEXT
              MOVE BR-NO           TO D01-BRNO
              MOVE CQS-COLLID      TO D01-ACCTNO
              MOVE OCQ-TIMED-REC   TO D01-A15
              MOVE CQS-TIMED-REC   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OCQ-COLLID NOT = CQS-CQ-COLLID
              MOVE "CQS-CQ-COLLID" TO D01-TEXT
              MOVE CQS-BRNO        TO D01-BRNO
              MOVE CQS-COLLID      TO D01-ACCTNO
              MOVE OCQ-COLLID      TO D01-A15
              MOVE CQS-CQ-COLLID   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OCQ-REV-BEHAVIOR-RISK NOT NUMERIC
              MOVE 0 TO OCQ-REV-BEHAVIOR-RISK.
           IF OCQ-REV-BEHAVIOR-RISK NOT = CQS-CQ-REV-BEHAVIOR-RISK
              MOVE "CQS-CQ-REV-BEHAVIOR-RISK" TO D01-TEXT
              MOVE BR-NO                      TO D01-BRNO
              MOVE CQS-COLLID                 TO D01-ACCTNO
              MOVE OCQ-REV-BEHAVIOR-RISK      TO D01-A15-NUM
              MOVE CQS-CQ-REV-BEHAVIOR-RISK   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OCQ-SEQ-FG NOT = CQS-CQ-SEQ-FG
              MOVE "CQ-SEQ-FG"   TO D01-TEXT
              MOVE CQ-BRNO       TO D01-BRNO
              MOVE CQS-COLLID    TO D01-ACCTNO
              MOVE OCQ-SEQ-FG    TO D01-A15
              MOVE CQS-CQ-SEQ-FG TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

       EXIT-COMPARE-SUMMARY-FIELDS.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE-ERROR SECTION.
      ******************************************************************
           ADD 1 TO TOTAL-ERRORS-CTR.
           PERFORM WRITE-DETAIL-LINE.

       WRITE-DETAIL-LINE-ERROR-EXIT.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************

           ADD  LN-FEED                   TO LN-COUNT.

           IF ( LN-COUNT > EXT-CONAME-RPT-LINES )
              PERFORM WRITE-HEADER-LINES.

           MOVE DETAIL-LINE                TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 01                         TO LN-FEED.
           MOVE SPACES                     TO DETAIL-LINE.

       WRITE-DETAIL-LINE-EXIT.
           EXIT.

      ******************************************************************
       WRITE-HEADER-LINES SECTION.
      ******************************************************************

           MOVE 01                         TO LN-COUNT.
           ADD  01                         TO PAGE-NUMBER.
           PERFORM GET-TIME.

           MOVE EXT-CONAME-SYSDATE         TO H01-DATE.
           MOVE TIME-EDIT                  TO H01-TIME.
           MOVE EXT-CONAME-CONAME          TO H01-NAME.
           CALL "C$JUSTIFY"             USING H01-NAME "C".
           MOVE PAGE-NUMBER            TO H01-PAGE.

           MOVE HEADER-LINE-01             TO PR-REC.
           MOVE 99                         TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE EXT-CONAME-USER-ID         TO H02-USERID.

           MOVE HEADER-LINE-02             TO PR-REC.
           ADD  01                         TO LN-COUNT.
           MOVE 01                         TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEADER-LINE-03             TO PR-REC.
           ADD  02                         TO LN-COUNT.
           MOVE 02                         TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE 2                         TO LN-FEED.

       WRITE-HEADER-LINES-EXIT.
           EXIT.

      ******************************************************************
       RANGE-LINES SECTION.
      ******************************************************************

           MOVE 88                                    TO LN-COUNT.

           IF ( GROUP-FLAG = "Y" )
              MOVE "BEGINNING BRANCH              : " TO R01-PROMPT
              MOVE ENT-GROUP                          TO R01-TEXT
              MOVE 01                                 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH                 : " TO R01-PROMPT
              MOVE SPACES                             TO R01-TEXT
              MOVE 01                                 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEGINNING BRANCH              : " TO R01-PROMPT
              MOVE BEG-BRANCH                         TO R01-NUMERIC
              MOVE 01                                 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH                 : " TO R01-PROMPT
              MOVE END-BRANCH                         TO R01-NUMERIC
              MOVE 01                                 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.


           MOVE "A15 RECORDS PROCESSED         :"     TO R01-PROMPT.
           MOVE A15-RECORDS-PROCESSED                     TO R01-NUMERIC.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "S35 CQ RECORDS PROCESSED      :"     TO R01-PROMPT.
           MOVE S35-CQ-RECORDS-PROCESSED              TO R01-NUMERIC.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "S35 CQS RECORDS PROCESSED     :"     TO R01-PROMPT.
           MOVE S35-CQS-RECORDS-PROCESSED             TO R01-NUMERIC.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "EERRORS REPORTED              :"     TO R01-PROMPT.
           MOVE TOTAL-ERRORS-CTR                      TO R01-NUMERIC.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "STARTING TIME                 : "    TO R01-PROMPT.
           MOVE STARTING-TIME                         TO R01-TEXT.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING   TIME                 : "    TO R01-PROMPT.
           PERFORM GET-TIME.
           MOVE TIME-EDIT                             TO R01-TEXT.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DATABASE NAME : "                    TO R01-PROMPT.
           MOVE RET-DB-NAME                           TO R01-TEXT.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       RANGE-LINES-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/PROGID.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO CQFILE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CQFILE-SYSDATE.
           DISPLAY CQFILE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE CQFILE-FORM-FIELDS TO WR-BUF.
           MOVE CQFILE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE CQFILE-HELP--FILE TO HELP-LINK-FILE.
           MOVE CQFILE-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/CQFILE_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ****************************************
       TEST-INVALID-DATE SECTION.
      ****************************************
           IF DATE-YYYYMMDD NOT NUMERIC
              MOVE 0     TO DATE-YYYYMMDD
              GO TO EXIT-TEST-INVALID-DATE
           ELSE
           IF DATE-YYYYMMDD = 0 OR 20202020
              MOVE 0     TO DATE-YYYYMMDD
              GO TO EXIT-TEST-INVALID-DATE.

      * CANT FIX BAD YEAR
           IF (DATE-YYYYMMDD-YYYY = 0) AND
               (DATE-YYYYMMDD-MM NOT = 0 OR DATE-YYYYMMDD-DD NOT = 0)
              MOVE 0 TO DATE-YYYYMMDD
              GO TO EXIT-TEST-INVALID-DATE.
      * 20000003  00/03/00 A15

      * 19638133
      * 19750000
      * 19970229

      * CANT FIX A BAD MONTH, SO FORCE IT TO 12 IF ONLY THING IS WRONG
      * DONT CARE IF DAY IS INVALID, IT WILL GET FIXED IN THE NEXT 'IF'
           IF (DATE-YYYYMMDD-YYYY NOT = 0) AND
                (DATE-YYYYMMDD-MM < 1 OR > 12)
              MOVE 12 TO DATE-YYYYMMDD-MM.

           IF (DATE-YYYYMMDD-DD < 1 OR > 31) OR
              ((DATE-YYYYMMDD-MM = 02) AND (DATE-YYYYMMDD-DD > 28)) OR
              ((DATE-YYYYMMDD-DD > 30) AND
                            (DATE-YYYYMMDD-MM = 4 OR 6 OR 9 OR 11))
                 MOVE 33            TO DATE-YYYYMMDD-DD
                 MOVE DATE-YYYYMMDD TO NDTE-DATE
                 PERFORM NEW-DATE-CALCULATION
                 MOVE NDTE-DATE     TO DATE-YYYYMMDD.

       EXIT-TEST-INVALID-DATE.
           EXIT.

      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".

       INITIAL-PROGRESS-INDICATOR.

           MOVE ZEROES                 TO PROGID-BRANCH.
           MOVE "LEGEND:B."            TO PROGID-LINE-LIST.
           MOVE PRU-LP                 TO PROGID-PRU-LP.
           MOVE EXT-ROUTE-BUF              TO PROGID-ROUTE-BUF.
           MOVE 9999                   TO PROGID-SLEEP-COUNTER.
           MOVE 250                    TO PROGID-SLEEP-COUNTER-MAX.

           PERFORM GET-TIME.
           MOVE TIME-EDIT              TO PROGID-TIME.

           MOVE "B"                    TO PROGID-TYPE.
           MOVE PROGID-TEXT-PROCESSING TO PROGID-TEXT.

      *-----------------------------------------------------------------
       SEND-PROGRESS-INDICATOR.

           MOVE BR-NO                      TO PROGID-BRANCH.
           STRING "PROCESSING BRANCH: ", BR-NO INTO PROGID-TEXT.

           PERFORM GET-TIME.
           MOVE TIME-EDIT                  TO PROGID-TIME.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PROGID-DISPLAY.

       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBSP/SPCQGS_SQL.CPY".
       COPY "LIBSP/SPCQSGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBSP/SPCQ1RN.CPY".
       COPY "LIBSP/SPCQS1RN.CPY".
       OPEN-OCQ1-FILE.
           PERFORM OPEN-IT.
           MOVE OCQ-PATH TO E-FILE.
           OPEN INPUT OCQ-FILE.
           IF IO-FG = 8
              GO TO OPEN-OCQ1-FILE.
           IF IO-FG = 7
              CLOSE OCQ-FILE
              GO TO OPEN-OCQ1-FILE.
       READ-OCQ1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OCQ-PATH TO E-FILE.
           MOVE OCQ1-KEY TO E-KEYX.
           READ OCQ-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OCQ1-FILE-NEXT.
       START-OCQ1-FILE.
           PERFORM START-IT.
           MOVE OCQ-PATH TO E-FILE.
           MOVE OCQ1-KEY TO E-KEYX.
           START OCQ-FILE KEY NOT < OCQ1-KEY.
       CLOSE-OCQ1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OCQ-FILE.

       OPEN-OCQ2-FILE.
           PERFORM OPEN-IT.
           MOVE OCQ-PATH TO E-FILE.
           OPEN INPUT OCQ-FILE.
           IF IO-FG = 8
              GO TO OPEN-OCQ2-FILE.
           IF IO-FG = 7
              CLOSE OCQ-FILE
              GO TO OPEN-OCQ2-FILE.
       READ-OCQ2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OCQ-PATH TO E-FILE.
           MOVE OCQ2-KEY TO E-KEYX.
           READ OCQ-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OCQ2-FILE-NEXT.
       START-OCQ2-FILE.
           PERFORM START-IT.
           MOVE OCQ-PATH TO E-FILE.
           MOVE OCQ2-KEY TO E-KEYX.
           START OCQ-FILE KEY NOT < OCQ2-KEY.
       CLOSE-OCQ2-FILE.
           PERFORM CLOSE-IT.
           CLOSE OCQ-FILE.


      *=================================================================
       START-MISC-CSR.
           MOVE "SSP_TCLP_SELECT" TO E-FILE.
           MOVE SPACES           TO E-KEYX.
           MOVE "START ERROR"    TO E-MSG.
           MOVE "GB/SQLINF"      TO E-PROG.

           IF ( MISC-CURSOR-STAT-GOOD )
              PERFORM CLOSE-MISC-CSR.

           MOVE "SELECT DB_NAME(),@@SERVERNAME,@@VERSION" 
              TO PASS-SQL.

           EXEC SQL
            DECLARE MISC_CURSOR CURSOR FOR
                :MISC-RETURN-CODE = 
                 EXEC SSP_TCLP_SELECT
                  ( :PASS-SQL)
                END-EXEC.

           EXEC SQL
               OPEN MISC_CURSOR
           END-EXEC.

           IF SQLCODE NOT = 0
             PERFORM SQL-ERROR.

           MOVE STAT---GOOD TO MISC-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-MISC-CSR-NEXT.
           MOVE "SSP_TCLP_SELECT" TO E-FILE.
           MOVE SPACES           TO E-KEYX.
           MOVE "READ ERROR"     TO E-MSG.
           MOVE "GB/SQLINF"      TO E-PROG.

           IF ( MISC-CURSOR-STAT-GOOD )
              EXEC SQL
                 FETCH MISC_CURSOR INTO  
                   :RET-DB-NAME,
                   :RET-SERVER-NAME, 
                   :RET-VERSION
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           IF SQLCODE NOT = 0
             PERFORM SQL-ERROR.

      *-----------------------------------------------------------------
       CLOSE-MISC-CSR.
           MOVE "SSP_TCLP_SELECT" TO E-FILE.
           MOVE SPACES           TO E-KEYX.
           MOVE "CLOSE ERROR"    TO E-MSG.
           MOVE "GB/SQLINF"      TO E-PROG.

           IF ( MISC-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE MISC_CURSOR
              END-EXEC
              MOVE STAT---BAD TO MISC-CURSOR-STAT.

      *-----------------------------------------------------------------

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************

           PERFORM CLOSE-FILES.

           IF ( EXT-ROUTE-BUF = "00" )
              PERFORM PRU-OUT.

      *     IF ( NO-RECORDS-FOUND )
      *        MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
      *        PERFORM SEND-MESS.

       EXIT-PROG.

           IF ( SV-BEG-BRANCH NOT = 9999 )
              IF ( EXT-EOBUF-GROUP-FG = "Y" )
                 MOVE SV-BEG-BRANCH        TO BEG-BRANCH
                 MOVE SV-END-BRANCH        TO END-BRANCH
                 MOVE SV-GROUP-FLAG        TO GROUP-FLAG
                 MOVE SV-ENT-GROUP         TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BRANCH        TO BEG-BRANCH
                 MOVE SV-END-BRANCH        TO END-BRANCH.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE EXIT-PATH               TO FORM-NAM
              MOVE FORM-PATH               TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY CQFILE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
