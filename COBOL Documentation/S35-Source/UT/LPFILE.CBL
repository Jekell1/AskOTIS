       IDENTIFICATION DIVISION.
       PROGRAM-ID.    LPFILE.
       DATE-WRITTEN.  10-24-2023.
      ******************************************************************
      *
      * DESCRIPTION   : COMPARE S35 LPFILE WITH A15 LPFILE
      *            
      *                 TO BE USED AFTER AN UPDATE
      *                 WILL COMPARE EACH FIELD AND REPORT ANY 
      *                 DIFFERENCES, AS LONG AS FIELD EXIST IN BOTH FILES
      *
      *  2 FILE MATCH PROCESSING COMPARING A15 AND S35 BY BRANCH
      *
      * * * NEED TO TAKE OUT THE C$JUSTIFY STATEMENTS AFTER BRIAN REMOVES
      *     THE "TRIM" STATEMENT FROM THE ASCII LOAD
      * * *
      *
      * 11-05-2024 RMZ - ADDED "LP-REPAY-TRANS-ID" VALIDATION TO PROGRAM.
      * 01-07-2025 RMZ - ADDED 'BTRACK' - PROCESS TRACKING TO PROGRAM.
      * 04-29-2025 MB  - SKIP POSTTIME/USERID CHECK IF COMPARE FOR USER TESTING
      *                    ENT-GROUP = 'S351'
      * 05-01-2025 MB  - ADDED COMPARE FOR LP_ESCROW_PREPAID 
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

      * COPYMEMBER: LIBLP/LPFSLP
           SELECT OLP-FILE ASSIGN TO OLP-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OLP1-KEY
                  FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  OLP-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A15SRC/LP01LP.CPY" REPLACING LEADING "LP" BY "OLP".

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/LPFILE S35 11/08/23-09:29:03 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LP.CPY".
       COPY "LIBLP/LP01LP_SQL.CPY".
       COPY "LIBLP/LPWSLP.CPY".
       01  OLP-PATH                    PIC X(35) VALUE SPACES.
       01  OLP-ALL-PATH.
           03  OLP-ALL-BASE     PIC X(9).
           03  FILLER           PIC X     VALUE "/".
           03  OLP-ALL-MACHINE  PIC XX.
           03  FILLER           PIC X     VALUE "/".
           03  OLP-ALL-DATA     PIC X(6).
           03  FILLER           PIC X(10) VALUE
               "/GB/LPFILE".

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

       01  LP-EOF-SW                   PIC X      VALUE "N".
       01  OLP-EOF-SW                  PIC X      VALUE "N".
       01  LP1-CONN-OPEN-SW            PIC X      VALUE "N".

       01  OLP-USE-KEY.
           03 OLP-USE-KEY-BRNO         PIC 9(4).
           03 OLP-USE-KEY-ACCT         PIC 9(8).
           03 OLP-USE-KEY-SEQ          PIC 9(5).

       01  TEST-TRCD                   PIC X(02).
           88  DEFER-TRCD              VALUE "DF" "D2" "D3" "D4" "D5"
                                             "D6" "D7" "D8" "D9".
           88  PMT-TRCD                VALUE "PY" "PA".

       01  HOLD-LN-TILLNO       PIC 9(2).
       01  HOLD-BYTE            PIC X.
           88  VALID-SPECIAL-CHARS  VALUE "(" ")"
                              "/" "\" "-" "?" "+" "%" "#" "[" "]"
                              "!" "<" ">" "$" "*" "&" "." """"  "="
                              "'" "_" ":" "@" "," ";" "~" "|" "{" "}".

       01  WS-ACCTNO                PIC 9(8).
       01  WS-ACCTNOX REDEFINES WS-ACCTNO.
           03  WS-NUMBER            PIC 9(6).
           03  WS-SEQNO             PIC 99.
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

       01  SQL-RECORDS-PROCESSED           PIC 9(9) VALUE 0.
       01  A15-RECORDS-PROCESSED           PIC 9(9) VALUE 0.
           88  NO-RECORDS-FOUND                     VALUE ZEROES.
       01  TOTAL-ERRORS-CTR            PIC 9(9)   VALUE 0.

       01  STARTING-TIME               PIC X(08)     VALUE SPACES.
       01  SUB                         PIC 9(04)     VALUE ZEROES.

       01  SYSDATE-YYYYMMDD            PIC 9(8)     VALUE ZEROES.

       01  LEGEND                      PIC X(79)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(07)     VALUE "LEGEND.".

       01  LEGEND-BEG-BRANCH.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F6-SCAN, F7-".
           03  FILLER PIC X(30) VALUE "EXIT, ENTER BEGINNING BRANCH/G".
           03  FILLER PIC X(19) VALUE "ROUP               ".

       01  LEGEND-END-BRANCH.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER ENDIN".
           03  FILLER PIC X(30) VALUE "G BRANCH NUMBER               ".
           03  FILLER PIC X(19) VALUE "                   ".

      *-----------------------------------------------------------------
       01  LEGEND-BEG-ACCTNO.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F7-EXIT, ENT".
           03  FILLER PIC X(30) VALUE "ER BEGINNING ACCOUNT NUMBER   ".
           03  FILLER PIC X(19) VALUE "                   ".

       01  LEGEND-END-ACCTNO.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER ENDIN".
           03  FILLER PIC X(30) VALUE "G ACCOUNT NUMBER              ".
           03  FILLER PIC X(19) VALUE "                   ".



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
           03  BEG-ACCTNO              PIC 9(06).
           03  END-ACCTNO              PIC 9(06).
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
           03  H02-TITLE               PIC X(40) VALUE 
           "PAYMENT FILE (LPFILE) COMPARE A15 TO S35".

       01  HEADER-LINE-03.
           03  FILLER                PIC X(4)  VALUE "BRNO".
           03  FILLER                PIC X(3)  VALUE SPACES.
           03  FILLER                PIC X(8)  VALUE " ACCTNO ".
           03  FILLER                PIC X(3)  VALUE SPACES.
           03  FILLER                PIC X(3)  VALUE "SEQ".
           03  FILLER                PIC X(3)  VALUE SPACES.
           03  FILLER                PIC X(28) VALUE "DESCRIPTION".
           03  FILLER                PIC X(22) VALUE "A15".
           03  FILLER                PIC X(3)  VALUE "S35".

      ******************************************************************
      *
      *    D E T A I L - L I N E - W O R K E R S
      *
      ******************************************************************
       01  DETAIL-LINE                        PIC X(100)   VALUE SPACES.

       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-BRNO                       PIC Z(3)9.
           03  FILLER                         PIC X(3).
           03  D01-ACCTNO                     PIC Z(7)9.
           03  FILLER                         PIC X(3).
           03  D01-SEQ                        PIC ZZ9.
           03  FILLER                         PIC X(3).
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
       COPY "LIBCL/BTRACKW.CPY".
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
       COPY "LIBUT/LPFILE_DEF.CPY".
       COPY "LIBUT/LPFILE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/LPFILE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE OLP-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LP1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           PERFORM CLOSE-LP1-CONNECTION.
           CLOSE OLP-FILE.
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

           PERFORM BTRACK-CREATE.

           PERFORM EXECUTION-MODULE.

           PERFORM RANGE-LINES.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

           MOVE STAT---GOOD                TO PROCESS-STAT.

           MOVE FORM-PATHNAME              TO FORM-PATH.
           MOVE "LPFILE" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE LPFILE-QUEUE-POS TO Q-POS.
           MOVE LPFILE-COPIES-POS TO C-POS.
           MOVE LPFILE-PRU-POS   TO P-POS.
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
              MOVE EXT-EOBUF-BRNO          TO BEG-BRANCH END-BRANCH
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

           IF ( UP-KEY )
              GO TO ENTER-END-ACCTNO.

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

           MOVE LEGEND-BEG-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.

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

           MOVE LEGEND-END-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.

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


       ENTER-BEG-ACCTNO.

           MOVE LEGEND-BEG-ACCTNO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN060BEG-ACCTNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F3-KEY ) GO TO ENTER-ALL-ACCTNO.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-ACCTNO.

           MOVE VDUNUM0 TO BEG-ACCTNO.

      *-----------------------------------------------------------------
       ENTER-END-ACCTNO.

           MOVE LEGEND-END-ACCTNO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN060END-ACCTNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-ACCTNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-ACCTNO.

           MOVE VDUNUM0 TO END-ACCTNO.

           IF ( BEG-ACCTNO > END-ACCTNO )
              MOVE "INVALID ACCOUNT NUMBER RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-ACCTNO.

           GO TO ENTER-PARAMETERS-END.

      *-----------------------------------------------------------------
       ENTER-ALL-ACCTNO.

           MOVE "SNNN060BEG-ACCTNO."  TO VDU.
           MOVE ZEROES                TO BEG-ACCTNO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060END-ACCTNO."  TO VDU.
           MOVE 999999                TO END-ACCTNO VDUNUM0.
           PERFORM SCREENIO.

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

           MOVE "SNNN060BEG-ACCTNO."       TO VDU.
           MOVE BEG-ACCTNO                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060END-ACCTNO."       TO VDU.
           MOVE END-ACCTNO                 TO VDUNUM0.
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

           MOVE BR-NO TO BTRACK-BRNO.
           PERFORM BTRACK-LOCAL.

           PERFORM PROCESS-BRANCH-DRIVER.

           GO TO EXECUTION-NEXT-GROUP.

       EXECUTION-MODULE-EXIT.
           EXIT.

     *********************************************************
       PROCESS-BRANCH-DRIVER SECTION.
     *********************************************************

           MOVE 9999                       TO PROGID-SLEEP-COUNTER.
           PERFORM SEND-PROGRESS-INDICATOR.

           MOVE "N"  TO LP-EOF-SW OLP-EOF-SW.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO LP-PATH-OVERRIDE.

           MOVE BR-NO      TO QLP1-WBEG-BRNO QLP1-WEND-BRNO.
           MOVE BEG-ACCTNO TO QLP1-WBEG-ACCTNO.
           MOVE END-ACCTNO TO QLP1-WEND-ACCTNO.

           PERFORM OPEN-LP1-FILE.
           PERFORM START-LP1-FILE.    *> SSP LPFILE
           PERFORM M-READ-LP1.

      D    STOP MESS.
           MOVE SOURCE-PATH  TO OLP-ALL-BASE.
           MOVE MACHINE-R1   TO OLP-ALL-MACHINE.
           MOVE BR-DATA-PATH TO OLP-ALL-DATA.
           MOVE OLP-ALL-PATH TO OLP-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE MACHINE-R2 TO OLP-ALL-MACHINE
              MOVE OLP-ALL-PATH TO OLP-PATH ACCESS-BUF
              PERFORM ACCESS-CALL
              IF ( STAT-BAD )
                 MOVE "MISSING A15 FILE" TO D01-TEXT 
                 MOVE BR-NO              TO D01-BRNO
                 MOVE OLP-ALL-PATH       TO D01-A15
                 MOVE SPACES             TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE
                 GO TO PROCESS-BRANCH-DRIVER-END.

           MOVE BEG-ACCTNO           TO WS-NUMBER.
           MOVE 1                    TO WS-SEQNO.
           MOVE WS-ACCTNO            TO OLP-ACCTNO.
           MOVE 1                    TO OLP-SEQNO.
           PERFORM OPEN-OLP1-FILE.
           PERFORM START-OLP1-FILE.
           PERFORM M-READ-OLP.

           PERFORM PROCESS-BRANCH-NEXT-LP
              UNTIL OLP-EOF-SW = "Y" AND LP-EOF-SW = "Y".

           PERFORM CLOSE-OLP1-FILE.
           PERFORM CLOSE-LP1-FILE.

           GO TO PROCESS-BRANCH-DRIVER-END.

       PROCESS-BRANCH-NEXT-LP.

           PERFORM SEND-PROGRESS-INDICATOR.

           PERFORM COMPARE-FIELDS.

           IF OLP-USE-KEY < LP1-KEY
              PERFORM M-READ-OLP 
           ELSE
              IF OLP-USE-KEY > LP1-KEY
                 PERFORM M-READ-LP1
              ELSE
                 PERFORM M-READ-OLP
                 PERFORM M-READ-LP1.

       PROCESS-BRANCH-DRIVER-END.
           EXIT.

       M-READ-LP1 SECTION.
           PERFORM READ-LP1-FILE-NEXT.   *> SSP LPFILE
           IF IO-BAD
              MOVE ALL '9' TO LP1-KEY
              MOVE "Y"     TO LP-EOF-SW
           ELSE
              ADD 1 TO SQL-RECORDS-PROCESSED.

       EXIT-M-READ-LP1.
           EXIT.

       M-READ-OLP SECTION.
           PERFORM READ-OLP1-FILE-NEXT.
           IF IO-BAD
              MOVE ALL '9'  TO OLP-USE-KEY
              MOVE "Y"      TO OLP-EOF-SW
           ELSE
              MOVE OLP-ACCTNO TO WS-ACCTNO
              IF (WS-NUMBER < BEG-ACCTNO) OR (WS-NUMBER > END-ACCTNO)
                 GO TO M-READ-OLP
              ELSE
                 MOVE BR-NO     TO OLP-USE-KEY-BRNO
                 MOVE WS-NUMBER TO OLP-USE-KEY-ACCT
                 MOVE OLP-SEQNO TO OLP-USE-KEY-SEQ
                 ADD 1 TO A15-RECORDS-PROCESSED.

      ******************************************************************
       COMPARE-FIELDS SECTION.
      ******************************************************************

            IF OLP-USE-KEY < LP1-KEY
              MOVE "MISSING S35 RECORD" TO D01-TEXT
              MOVE OLP-USE-KEY-BRNO     TO D01-BRNO
              MOVE OLP-USE-KEY-ACCT     TO D01-ACCTNO
              MOVE OLP-USE-KEY-SEQ      TO D01-SEQ
              ADD 1                     TO TOTAL-ERRORS-CTR
              PERFORM WRITE-DETAIL-LINE 
              GO TO EXIT-COMPARE-FIELDS.

           IF OLP-USE-KEY > LP1-KEY
              MOVE "MISSING A15 RECORD" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE
              GO TO EXIT-COMPARE-FIELDS.

           IF ENT-GROUP NOT = 'S351'
              IF OLP-USERID NOT = LP-USERID
                 MOVE "LP-USERID" TO D01-TEXT
                 MOVE OLP-USERID  TO D01-A15
                 MOVE LP-USERID   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE.

           MOVE OLP-POSTDATE       TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LP-POSTDATE
              MOVE "LP-POSTDATE"  TO D01-TEXT
              MOVE DATE-YYYYMMDD  TO D01-A15 
              MOVE LP-POSTDATE    TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.

           IF ENT-GROUP NOT = 'S351'
              IF OLP-POSTTIME NOT NUMERIC
                 MOVE 0               TO OLP-POSTTIME
              END-IF
              IF OLP-POSTTIME NOT = LP-POSTTIME
                 MOVE "LP-POSTTIME" TO D01-TEXT
                 MOVE OLP-POSTTIME  TO D01-A15 
                 MOVE LP-POSTTIME   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE.

           MOVE OLP-TRDATE       TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LP-TRDATE
              MOVE "LP-TRDATE"    TO D01-TEXT
              MOVE DATE-YYYYMMDD  TO D01-A15 
              MOVE LP-TRDATE     TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.

           MOVE OLP-PAYDATE       TO DATE-MMDDYY.
      * AL 1542 42076 SEQ 1 20080033   TRY TRDATE
      * LA 0735 2636  SEQ 1 00/11/27   TRY TRDATE
           IF DATE-MMDDYY-MM < 1 OR > 12
              MOVE OLP-TRDATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LP-PAYDATE
              MOVE "LP-PAYDATE" TO D01-TEXT
              MOVE DATE-YYYYMMDD  TO D01-A15 
              MOVE LP-PAYDATE   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.

           IF OLP-TRCD NOT = LP-TRCD
              MOVE "LP-TRCD" TO D01-TEXT
              MOVE OLP-TRCD  TO D01-A15
              MOVE LP-TRCD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.

           IF OLP-REFNO NOT = LP-REFNO
              MOVE "LP-REFNO" TO D01-TEXT
              MOVE OLP-REFNO  TO D01-A15
              MOVE LP-REFNO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.

           IF OLP-TRAMT NOT NUMERIC
              MOVE 0 TO OLP-TRAMT.
           IF OLP-TRAMT NOT = LP-TRAMT
              MOVE "LP-TRAMT" TO D01-TEXT
              MOVE OLP-TRAMT  TO D01-A15-NUM
              MOVE LP-TRAMT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.

           IF OLP-APOTH NOT NUMERIC
              MOVE 0 TO OLP-APOTH.
           IF OLP-APOTH NOT = LP-APOTH
              MOVE "LP-APOTH" TO D01-TEXT
              MOVE OLP-APOTH  TO D01-A15-NUM
              MOVE LP-APOTH   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.

           IF OLP-APOT2 NOT NUMERIC
              MOVE 0 TO OLP-APOT2.
           IF OLP-APOT2 NOT = LP-APOT2
              MOVE "LP-APOT2" TO D01-TEXT
              MOVE OLP-APOT2  TO D01-A15-NUM
              MOVE LP-APOT2   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.

           IF OLP-APINT NOT NUMERIC
              MOVE 0 TO OLP-APINT.
           IF OLP-APINT NOT = LP-APINT
              MOVE "LP-APINT" TO D01-TEXT
              MOVE OLP-APINT  TO D01-A15-NUM
              MOVE LP-APINT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.

           IF OLP-APLC NOT NUMERIC
              MOVE 0 TO OLP-APLC.
           IF OLP-APLC NOT = LP-APLC
              MOVE "LP-APLC" TO D01-TEXT
              MOVE OLP-APLC  TO D01-A15-NUM
              MOVE LP-APLC   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.

           IF OLP-APCUR NOT NUMERIC
              MOVE 0 TO OLP-APCUR.
           IF OLP-APCUR NOT = LP-APCUR
              MOVE "LP-APCUR" TO D01-TEXT
              MOVE OLP-APCUR  TO D01-A15-NUM
              MOVE LP-APCUR   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.

           IF OLP-OTHBAL NOT NUMERIC
              MOVE 0 TO OLP-OTHBAL.
           IF OLP-OTHBAL NOT = LP-OTHBAL
              MOVE "LP-OTHBAL" TO D01-TEXT
              MOVE OLP-OTHBAL  TO D01-A15-NUM
              MOVE LP-OTHBAL   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.

           IF OLP-OT2BAL NOT NUMERIC
              MOVE 0 TO OLP-OT2BAL.
           IF OLP-OT2BAL NOT = LP-OT2BAL
              MOVE "LP-OT2BAL" TO D01-TEXT
              MOVE OLP-OT2BAL  TO D01-A15-NUM
              MOVE LP-OT2BAL   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.

           IF OLP-INTBAL NOT NUMERIC
              MOVE 0 TO OLP-INTBAL.
           IF OLP-INTBAL NOT = LP-INTBAL
              MOVE "LP-INTBAL" TO D01-TEXT
              MOVE OLP-INTBAL  TO D01-A15-NUM
              MOVE LP-INTBAL   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.

           IF OLP-LCBAL NOT NUMERIC
              MOVE 0 TO OLP-LCBAL.
           IF OLP-LCBAL NOT = LP-LCBAL
              MOVE "LP-LCBAL" TO D01-TEXT
              MOVE OLP-LCBAL  TO D01-A15-NUM
              MOVE LP-LCBAL   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.

           IF OLP-CURBAL NOT NUMERIC
              MOVE 0 TO OLP-CURBAL.
           IF OLP-CURBAL NOT = LP-CURBAL
              MOVE "LP-CURBAL" TO D01-TEXT
              MOVE OLP-CURBAL  TO D01-A15-NUM
              MOVE LP-CURBAL   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.

           IF OLP-INTDUE NOT NUMERIC
              MOVE 0 TO OLP-INTDUE.
           IF OLP-INTDUE NOT = LP-INTDUE
              MOVE "LP-INTDUE" TO D01-TEXT
              MOVE OLP-INTDUE  TO D01-A15-NUM
              MOVE LP-INTDUE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.

           IF OLP-DLPROC NOT NUMERIC
              MOVE 0 TO OLP-DLPROC.
           IF OLP-DLPROC NOT = LP-DLPROC
              MOVE "LP-DLPROC" TO D01-TEXT
              MOVE OLP-DLPROC  TO D01-A15-NUM
              MOVE LP-DLPROC   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.

           IF OLP-EARNED(1) NOT NUMERIC
              MOVE 0 TO OLP-EARNED(1).
           IF OLP-EARNED(1) NOT = LP-EARNED(1)
              MOVE "LP-EARNED(1)" TO D01-TEXT
              MOVE OLP-EARNED(1)  TO D01-A15-NUM
              MOVE LP-EARNED(1)   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.

           IF OLP-EARNED(2) NOT NUMERIC
              MOVE 0 TO OLP-EARNED(2).
           IF OLP-EARNED(2) NOT = LP-EARNED(2)
              MOVE "LP-EARNED(2)" TO D01-TEXT
              MOVE OLP-EARNED(2)  TO D01-A15-NUM
              MOVE LP-EARNED(2)   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.
 
           IF OLP-EARNED(3) NOT NUMERIC
              MOVE 0 TO OLP-EARNED(3).
           IF OLP-EARNED(3) NOT = LP-EARNED(3)
              MOVE "LP-EARNED(3)" TO D01-TEXT
              MOVE OLP-EARNED(3)  TO D01-A15-NUM
              MOVE LP-EARNED(3)   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.

           IF OLP-INTPAID NOT NUMERIC
              MOVE 0 TO OLP-INTPAID.
           IF OLP-INTPAID NOT = LP-INTPAID
              MOVE "LP-INTPAID" TO D01-TEXT
              MOVE OLP-INTPAID  TO D01-A15-NUM
              MOVE LP-INTPAID   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.
           IF OLP-SSNO NOT NUMERIC
              MOVE 0 TO OLP-SSNO.
           IF OLP-SSNO NOT = LP-SSNO
              MOVE "LP-SSNO" TO D01-TEXT
              MOVE OLP-SSNO  TO D01-A15-NUM
              MOVE LP-SSNO   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.

           IF OLP-PDTH NOT NUMERIC OR (OLP-PDTH = 202020)
              MOVE 0               TO OLP-PDTH.
           MOVE OLP-PDTH       TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LP-PDTH-DATE
              MOVE "LP-PDTH" TO D01-TEXT
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE LP-PDTH-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.
           IF OLP-LCPDTH NOT NUMERIC OR (OLP-LCPDTH = 202020)
              MOVE 0               TO OLP-LCPDTH.
           MOVE OLP-LCPDTH       TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LP-LCPDTH-DATE
              MOVE "LP-LCPDTH" TO D01-TEXT
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE LP-LCPDTH-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.

           PERFORM SET-HOLD-TILLNO.
           IF LP-TILLNO NOT = HOLD-LN-TILLNO
              MOVE "LP-TILLNO" TO D01-TEXT
              MOVE HOLD-LN-TILLNO TO D01-A15
              MOVE LP-TILLNO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.

           IF OLP-TRCD = "SP"
              IF OLP-PRIOR-POOLID NOT = LP-PRIOR-POOLID
                 MOVE "LP-PRIOR-POOLID" TO D01-TEXT
                 MOVE OLP-PRIOR-POOLID  TO D01-A15
                 MOVE LP-PRIOR-POOLID   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE.

           IF OLP-TRCD NOT = "SP"
              IF OLP-APINTOWE NOT = LP-APINTOWE
                 MOVE "LP-APINTOWE" TO D01-TEXT
                 MOVE OLP-APINTOWE TO D01-A15-NUM
                 MOVE LP-APINTOWE   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE.

           IF OLP-DLPARVHELD-ADJ NOT NUMERIC
              MOVE 0 TO OLP-DLPARVHELD-ADJ.
           IF OLP-DLPARVHELD-ADJ NOT = LP-DLPARVHELD-ADJ
              MOVE "LP-DLPARVHELD-ADJ" TO D01-TEXT
              MOVE OLP-DLPARVHELD-ADJ  TO D01-A15-NUM
              MOVE LP-DLPARVHELD-ADJ   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.

           IF OLP-IBPC NOT = LP-IBPC
              MOVE "LP-IBPC" TO D01-TEXT
              MOVE OLP-IBPC  TO D01-A15
              MOVE LP-IBPC   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.

           IF OLP-TRCD = "DC"
              MOVE OLP-NEW1ST-PYDATE       TO DATE-MMDDYY
              PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
              PERFORM TEST-INVALID-DATE
              IF DATE-YYYYMMDD NOT = LP-NEW1ST-PYDATE
                 MOVE "LP-NEW1ST-PYDATE" TO D01-TEXT
                 MOVE DATE-YYYYMMDD  TO D01-A15
                 MOVE LP-NEW1ST-PYDATE   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE.
        

           MOVE OLP-TRCD TO TEST-TRCD.
           IF DEFER-TRCD
              IF OLP-DEF-TERM NOT = LP-DEF-TERM
                 MOVE "LP-DEF-TERM" TO D01-TEXT
                 MOVE DATE-YYYYMMDD  TO D01-A15
                 MOVE LP-DEF-TERM   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE
              END-IF
              MOVE OLP-DEF-DUEDATE       TO DATE-MMDDYY
              PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
              PERFORM TEST-INVALID-DATE
              IF DATE-YYYYMMDD NOT = LP-DEF-DUEDATE
                 MOVE "LP-DEF-DUEDATE" TO D01-TEXT
                 MOVE DATE-YYYYMMDD  TO D01-A15
                 MOVE LP-DEF-DUEDATE   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE.

           IF NOT DEFER-TRCD
              IF OLP-TRCD NOT = "DC"
                 IF OLP-LCWORK1 NOT NUMERIC
                    MOVE 0 TO OLP-LCWORK1
                 END-IF
                 IF OLP-LCWORK1 NOT = LP-LCPARTIALS
                    MOVE "LP-LCPARTIALS" TO D01-TEXT
                    MOVE OLP-LCWORK1  TO D01-A15-NUM
                    MOVE LP-LCPARTIALS   TO D01-S35-NUM
                    PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                    PERFORM WRITE-DETAIL-LINE.

           IF OLP-TRCD = "DC"
              MOVE OLP-OLD1ST-PYDATE       TO DATE-MMDDYY
              PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
              PERFORM TEST-INVALID-DATE
              IF DATE-YYYYMMDD NOT = LP-OLD1ST-PYDATE
                 MOVE "LP-OLD1ST-PYDATE" TO D01-TEXT
                 MOVE DATE-YYYYMMDD  TO D01-A15
                 MOVE LP-OLD1ST-PYDATE   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY 
                 PERFORM WRITE-DETAIL-LINE.

           IF NOT DEFER-TRCD
              IF OLP-TRCD NOT = "DC"
                 IF OLP-LCWORK2 NOT NUMERIC
                    MOVE 0 TO OLP-LCWORK2
                 END-IF
                 IF OLP-LCWORK2 NOT = LP-LCPAID
                    MOVE "LP-LCPAID" TO D01-TEXT
                    MOVE OLP-LCWORK2  TO D01-A15-NUM
                    MOVE LP-LCPAID   TO D01-S35-NUM
                    PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                    PERFORM WRITE-DETAIL-LINE.

           IF OLP-MANUAL-RB NOT = LP-MANUAL-RB
              MOVE "LP-MANUAL-RB" TO D01-TEXT
              MOVE OLP-MANUAL-RB  TO D01-A15
              MOVE LP-MANUAL-RB   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.

           IF OLP-REV NOT = LP-REV
              MOVE "LP-REV" TO D01-TEXT
              MOVE OLP-REV  TO D01-A15
              MOVE LP-REV   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.

           IF OLP-REVTRCD NOT = LP-REVTRCD
              MOVE "LP-REVTRCD" TO D01-TEXT
              MOVE OLP-REVTRCD  TO D01-A15
              MOVE LP-REVTRCD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.

           IF OLP-REVSEQNO NOT NUMERIC
              MOVE 0 TO OLP-REVSEQNO.
           IF OLP-REVSEQNO NOT = LP-REVSEQNO
              MOVE "LP-REVSEQNO" TO D01-TEXT
              MOVE OLP-REVSEQNO  TO D01-A15-NUM
              MOVE LP-REVSEQNO   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.
              
           IF OLP-STATUSFG NOT = LP-STATUSFG
              MOVE "LP-STATUSFG" TO D01-TEXT
              MOVE OLP-STATUSFG  TO D01-A15
              MOVE LP-STATUSFG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.
              
           IF OLP-NEWSTAT-RATE NOT NUMERIC
              MOVE 0 TO OLP-NEWSTAT-RATE.
           IF OLP-NEWSTAT-RATE NOT = LP-NEWSTAT-RATE
              MOVE "LP-NEWSTAT-RATE" TO D01-TEXT
              MOVE OLP-NEWSTAT-RATE  TO D01-A15-NUM
              MOVE LP-NEWSTAT-RATE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.
              
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
              IF OLP-DLRVCHGS-ADJ(SUB) NOT NUMERIC
                 MOVE 0 TO OLP-DLRVCHGS-ADJ(SUB)
              END-IF
              IF OLP-DLRVCHGS-ADJ(SUB) NOT = LP-DLRVCHGS-ADJ(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "LP-DLRVCHGS-ADJ",SUB INTO D01-TEXT
                 MOVE OLP-DLRVCHGS-ADJ(SUB)  TO D01-A15-NUM
                 MOVE LP-DLRVCHGS-ADJ(SUB)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE
              END-IF
           END-PERFORM.

           IF OLP-DLPARVPAID-ADJ NOT NUMERIC
              MOVE 0 TO OLP-DLPARVPAID-ADJ.
           IF OLP-DLPARVPAID-ADJ NOT = LP-DLPARVPAID-ADJ
              MOVE "LP-DLPARVPAID-ADJ" TO D01-TEXT
              MOVE OLP-DLPARVPAID-ADJ  TO D01-A15-NUM
              MOVE LP-DLPARVPAID-ADJ   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.

           IF OLP-DLNO NOT NUMERIC
              MOVE 0 TO OLP-DLNO.
           IF OLP-DLNO NOT = LP-DLNO
              MOVE "LP-DLNO" TO D01-TEXT
              MOVE OLP-DLNO  TO D01-A15-NUM
              MOVE LP-DLNO   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.

           IF OLP-WORKAMT1 NOT NUMERIC
              MOVE 0 TO OLP-WORKAMT1.
           IF OLP-WORKAMT1 NOT = LP-PREPAY-PENALTY
              MOVE "LP-PREPAY-PENALTY" TO D01-TEXT
              MOVE OLP-WORKAMT1  TO D01-A15-NUM
              MOVE LP-PREPAY-PENALTY   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.

           IF OLP-WORKAMT2 NOT NUMERIC
              MOVE 0 TO OLP-WORKAMT2.
           IF OLP-WORKAMT2 NOT = LP-PRORATED-INS-REB
              MOVE "LP-PRORATED-INS-REB" TO D01-TEXT
              MOVE OLP-WORKAMT2  TO D01-A15-NUM
              MOVE LP-PRORATED-INS-REB   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.

           IF OLP-POSTING-BR NOT NUMERIC
              MOVE 0 TO OLP-POSTING-BR.
           IF OLP-POSTING-BR NOT = LP-POSTING-BR
              MOVE "LP-POSTING-BR" TO D01-TEXT
              MOVE OLP-POSTING-BR  TO D01-A15-NUM
              MOVE LP-POSTING-BR   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.
           IF OLP-NOLCHG NOT NUMERIC
              MOVE 0 TO OLP-NOLCHG.
           IF OLP-NOLCHG NOT = LP-NOLCHG
              MOVE "LP-NOLCHG" TO D01-TEXT
              MOVE OLP-NOLCHG  TO D01-A15-NUM
              MOVE LP-NOLCHG   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.

           IF OLP-COLLECTOR NOT = LP-COLLECTOR
              MOVE "LP-COLLECTOR" TO D01-TEXT
              MOVE OLP-COLLECTOR  TO D01-A15 
              MOVE LP-COLLECTOR   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.

           IF OLP-EXTRA-PRIN-LFP NOT NUMERIC OR
                 (OLP-EXTRA-PRIN-LFP = 2020202.02 OR 0202020.20)
              MOVE 0 TO OLP-EXTRA-PRIN-LFP.
           IF OLP-EXTRA-PRIN-LFP NOT = LP-EXTRA-PRIN-LFP
              MOVE "LP-EXTRA-PRIN-LFP" TO D01-TEXT
               MOVE OLP-EXTRA-PRIN-LFP  TO D01-A15-NUM
              MOVE LP-EXTRA-PRIN-LFP   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.


           IF OLP-REPAY-TRANS-ID NOT NUMERIC OR
                 (OLP-REPAY-TRANS-ID = 2020202020 OR 0202020202)
              MOVE 0 TO OLP-REPAY-TRANS-ID.
           IF OLP-REPAY-TRANS-ID NOT = LP-REPAY-TRANS-ID
              MOVE "LP-REPAY-TRANS-ID" TO D01-TEXT
               MOVE OLP-REPAY-TRANS-ID  TO D01-A15-NUM
              MOVE LP-REPAY-TRANS-ID   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.

           IF OLP-REMAIN-INT-SINCE-LFP NOT NUMERIC OR
               (OLP-REMAIN-INT-SINCE-LFP = 2020202.02 OR 0202020.20)
              MOVE 0 TO OLP-REMAIN-INT-SINCE-LFP.
           IF OLP-REMAIN-INT-SINCE-LFP NOT = LP-REMAIN-INT-SINCE-LFP
              MOVE "LP-REMAIN-INT-SINCE-LFP" TO D01-TEXT
              MOVE OLP-REMAIN-INT-SINCE-LFP  TO D01-A15-NUM
              MOVE LP-REMAIN-INT-SINCE-LFP   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.
           IF OLP-REMAIN-PRIN-SINCE-LFP NOT NUMERIC OR
               (OLP-REMAIN-PRIN-SINCE-LFP = 2020202.02 OR 0202020.20)
              MOVE 0 TO OLP-REMAIN-PRIN-SINCE-LFP.
           IF OLP-REMAIN-PRIN-SINCE-LFP NOT = LP-REMAIN-PRIN-SINCE-LFP
              MOVE "LP-REMAIN-PRIN-SINCE-LFP" TO D01-TEXT
              MOVE OLP-REMAIN-PRIN-SINCE-LFP  TO D01-A15-NUM
              MOVE LP-REMAIN-PRIN-SINCE-LFP   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.

           IF OLP-APPLIED-TO-ESCROW NOT NUMERIC OR
                 (OLP-APPLIED-TO-ESCROW = 20202.02 OR 02020.20)
              MOVE 0 TO OLP-APPLIED-TO-ESCROW.

           IF PMT-TRCD
              IF OLP-APPLIED-TO-ESCROW NOT NUMERIC
                 MOVE 0 TO OLP-APPLIED-TO-ESCROW
              END-IF
              IF OLP-APPLIED-TO-ESCROW NOT = LP-APPLIED-TO-ESCROW
                 MOVE "LP-APPLIED-TO-ESCROW" TO D01-TEXT
                 MOVE OLP-APPLIED-TO-ESCROW  TO D01-A15-NUM
                 MOVE LP-APPLIED-TO-ESCROW   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE.

           IF OLP-ESCROW-PREPAID NOT NUMERIC
              MOVE 0 TO OLP-ESCROW-PREPAID.
           IF OLP-ESCROW-PREPAID NOT = LP-ESCROW-PREPAID
              MOVE "LP-ESCROW-PREPAID" TO D01-TEXT
              MOVE OLP-ESCROW-PREPAID  TO D01-A15-NUM
              MOVE LP-ESCROW-PREPAID   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE.

       EXIT-COMPARE-FIELDS.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE-ERROR-KEY SECTION.
      ******************************************************************

           MOVE BR-NO      TO D01-BRNO.
           MOVE LP-ACCTNO  TO D01-ACCTNO.
           MOVE LP-SEQNO   TO D01-SEQ.
           ADD 1           TO TOTAL-ERRORS-CTR.    

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

           MOVE "BEGINNING ACCOUNT NUMBER      : "    TO R01-PROMPT.
           MOVE BEG-ACCTNO                            TO R01-NUMERIC.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    ACCOUNT NUMBER      : "    TO R01-PROMPT.
           MOVE END-ACCTNO                            TO R01-NUMERIC.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "A15 RECORDS PROCESSED         :"     TO R01-PROMPT.
           MOVE A15-RECORDS-PROCESSED                 TO R01-NUMERIC.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "S35 RECORDS PROCESSED         :"     TO R01-PROMPT.
           MOVE SQL-RECORDS-PROCESSED                 TO R01-NUMERIC.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL ERRORS REPORTED         :"     TO R01-PROMPT.
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
       COPY "LIBCL/BTRACK.CPY".
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
           MOVE EXT-CONAME-CONAME TO LPFILE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LPFILE-SYSDATE.
           DISPLAY LPFILE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE LPFILE-FORM-FIELDS TO WR-BUF.
           MOVE LPFILE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE LPFILE-HELP--FILE TO HELP-LINK-FILE.
           MOVE LPFILE-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/LPFILE_EVA.CPY".
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
       SET-HOLD-TILLNO.
           IF OLP-TILLNO = "1"
              MOVE 1  TO HOLD-LN-TILLNO 
           ELSE
           IF OLP-TILLNO = "2"
              MOVE 2  TO HOLD-LN-TILLNO 
           ELSE
           IF OLP-TILLNO = "3"
              MOVE 3  TO HOLD-LN-TILLNO 
           ELSE
           IF OLP-TILLNO = "4"
              MOVE 4  TO HOLD-LN-TILLNO 
           ELSE
           IF OLP-TILLNO = "5"
              MOVE 5  TO HOLD-LN-TILLNO 
           ELSE
           IF OLP-TILLNO = "6"
              MOVE 6  TO HOLD-LN-TILLNO 
           ELSE
           IF OLP-TILLNO = "7"
              MOVE 7  TO HOLD-LN-TILLNO 
           ELSE
           IF OLP-TILLNO = "8"
              MOVE 8  TO HOLD-LN-TILLNO 
           ELSE
           IF OLP-TILLNO = "9"
              MOVE 9  TO HOLD-LN-TILLNO 
           ELSE
           IF OLP-TILLNO = "A"
              MOVE 10 TO HOLD-LN-TILLNO 
           ELSE
           IF OLP-TILLNO = "B"
              MOVE 11 TO HOLD-LN-TILLNO 
           ELSE
           IF OLP-TILLNO = "C"
              MOVE 12 TO HOLD-LN-TILLNO 
           ELSE
           IF OLP-TILLNO = "D"
              MOVE 13 TO HOLD-LN-TILLNO 
           ELSE
           IF OLP-TILLNO = "E"
              MOVE 14 TO HOLD-LN-TILLNO 
           ELSE
           IF OLP-TILLNO = "F"
              MOVE 15 TO HOLD-LN-TILLNO 
           ELSE
           IF OLP-TILLNO = "G"
              MOVE 16 TO HOLD-LN-TILLNO 
           ELSE
           IF OLP-TILLNO = "H"
              MOVE 17 TO HOLD-LN-TILLNO 
           ELSE
           IF OLP-TILLNO = "I"
              MOVE 18 TO HOLD-LN-TILLNO 
           ELSE
           IF OLP-TILLNO = "J"
              MOVE 19 TO HOLD-LN-TILLNO 
           ELSE
           IF OLP-TILLNO = "K"
              MOVE 20 TO HOLD-LN-TILLNO 
           ELSE
           IF OLP-TILLNO = "L"
              MOVE 21 TO HOLD-LN-TILLNO 
           ELSE
           IF OLP-TILLNO = "M"
              MOVE 22 TO HOLD-LN-TILLNO 
           ELSE
           IF OLP-TILLNO = "N"
              MOVE 23 TO HOLD-LN-TILLNO 
           ELSE
           IF OLP-TILLNO = "O"
              MOVE 24 TO HOLD-LN-TILLNO 
           ELSE
           IF OLP-TILLNO = "P"
              MOVE 25 TO HOLD-LN-TILLNO 
           ELSE
           IF OLP-TILLNO = "Q"
              MOVE 26 TO HOLD-LN-TILLNO 
           ELSE
           IF OLP-TILLNO = "R"
              MOVE 27 TO HOLD-LN-TILLNO 
           ELSE
           IF OLP-TILLNO = "S"
              MOVE 28 TO HOLD-LN-TILLNO 
           ELSE
           IF OLP-TILLNO = "T"
              MOVE 29 TO HOLD-LN-TILLNO 
           ELSE
           IF OLP-TILLNO = "U"
              MOVE 30 TO HOLD-LN-TILLNO 
           ELSE
           IF OLP-TILLNO = "V"
              MOVE 31 TO HOLD-LN-TILLNO 
           ELSE
           IF OLP-TILLNO = "W"
              MOVE 32 TO HOLD-LN-TILLNO 
           ELSE
           IF OLP-TILLNO = "X"
              MOVE 33 TO HOLD-LN-TILLNO 
           ELSE
           IF OLP-TILLNO = "Y"
              MOVE 34 TO HOLD-LN-TILLNO 
           ELSE
           IF OLP-TILLNO = "Z"
              MOVE 35 TO HOLD-LN-TILLNO
           ELSE
              MOVE 0 TO HOLD-LN-TILLNO.
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
       COPY "LIBLP/LPLPGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

      * COPY "LIBLP/LPLP1RN.CPY".
      * COPYMEMBER: LIBLP/LPLP1RN
      * BAH 20240212 ADDED LP-REPAY-TRANS-ID #1641
       LOAD-LP1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LP-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LP-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LP-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LP-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LP-ALL-DATA
              MOVE LP-ALL-PATH             TO LP-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LP-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LP-ALL-BASE
              MOVE FILPATH-MACHINE         TO LP-ALL-MACHINE
              MOVE FILPATH-DATA            TO LP-ALL-DATA
              MOVE LP-ALL-PATH             TO LP-PATH.

      *-----------------------------------------------------------------
       OPEN-LP1-FILE.
           PERFORM LOAD-LP1-FILE.
           PERFORM OPEN-IT.
           MOVE LP-PATH-SQL TO E-FILE.

           IF LP1-CONN-OPEN-SW = "N" 
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS LP1_CONNECT
                        USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE 'Y' TO LP1-CONN-OPEN-SW.

       START-LP1-FILE.
           PERFORM START-IT.
           MOVE LP-PATH-OWNBR  TO LP-BRNO.
           MOVE LP-PATH-SQL    TO E-FILE.
           MOVE LP-PATH-OWNBR  TO LP-BRNO.

           IF ( LP1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LP1-FILE.

           EXEC SQL SET CONNECTION LP1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE LP1_CURSOR CURSOR FOR
                :LP1-RETURN-CODE =  
            EXEC SSP_LPFILE_LPFILE_SELECT
              ( :QLP1-WBEG-BRNO,
                :QLP1-WBEG-ACCTNO, :QLP1-WEND-ACCTNO)
           END-EXEC.

      *     EXEC SQL
      *      DECLARE LP1_CURSOR CURSOR FOR
      *     SELECT *
      *     FROM DBO.LPFILE LPFILE
      *     WHERE LPFILE.LP_BRNO = @QLP1_WBEG_BRNO
      *     AND LPFILE.LP_ACCTNO  BETWEEN @QLP1_WBEG_ACCTNO  AND @QLP1_WEND_ACCTNO
      *     ORDER BY LPFILE.LP_ACCTNO,LPFILE.LP_SEQNO
      *     END-EXEC.

           EXEC SQL
               OPEN LP1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE STAT---GOOD TO LP1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-LP1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LP-PATH-OWNBR  TO LP-BRNO.
           MOVE LP-PATH-SQL    TO E-FILE.
           MOVE LP1-KEY        TO E-KEYX.
           INITIALIZE QLP-REC.

           EXEC SQL SET CONNECTION LP1_CONNECT END-EXEC.

           IF ( LP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LP1_CURSOR INTO :QLP-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LP1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LP-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-LP1-FILE.

           PERFORM CLOSE-IT.
           IF ( LP1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION LP1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE LP1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LP1-CURSOR-STAT
              EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-LP1-CONNECTION.

           IF LP1-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION LP1_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT LP1_CONNECT END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE "N" TO LP1-CONN-OPEN-SW.
      *-----------------------------------------------------------------

       OPEN-OLP1-FILE.
           PERFORM OPEN-IT.
           MOVE OLP-PATH TO E-FILE.
           OPEN INPUT OLP-FILE.
           IF IO-FG = 8
              GO TO OPEN-OLP1-FILE.
           IF IO-FG = 7
              CLOSE OLP-FILE
              GO TO OPEN-OLP1-FILE.
      *READ-OLP1-FILE.
      *    PERFORM READ-IT.
      *    MOVE OLP-PATH TO E-FILE.
      *    MOVE OLP1-KEY TO E-KEYX.
      *    READ OLP-FILE.
      *    IF IO-FG = 8
      *       GO TO READ-OLP1-FILE.
       START-OLP1-FILE.
           PERFORM START-IT.
           MOVE OLP-PATH TO E-FILE.
           MOVE OLP1-KEY TO E-KEYX.
           START OLP-FILE KEY NOT < OLP1-KEY.
       READ-OLP1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OLP-PATH TO E-FILE.
           MOVE OLP1-KEY TO E-KEYX.
           READ OLP-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OLP1-FILE-NEXT.
       CLOSE-OLP1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OLP-FILE.

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

           PERFORM BTRACK-DONE. 

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
              DESTROY LPFILE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
