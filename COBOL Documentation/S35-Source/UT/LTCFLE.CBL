       IDENTIFICATION DIVISION.
       PROGRAM-ID.    LTCFLE.
       DATE-WRITTEN.  11-09-2023.
      ******************************************************************
      *
      * DESCRIPTION   : COMPARE S35 LTCFLE WITH A15 LTCFLE
      *                  LOAN COLLATERAL TRAILER
      *            
      *                 TO BE USED AFTER AN UPDATE
      *                 WILL COMPARE EACH FIELD AND REPORT ANY 
      *                 DIFFERENCES, AS LONG AS FIELD EXIST IN BOTH FILES
      *
      *  LOOPS THRU THE A15 FILE AND VERIFIES THE S35 FILE EXIST AND MATCHES
      *
      * * * NEED TO TAKE OUT THE C$JUSTIFY STATEMENTS AFTER BRIAN REMOVES
      *     THE "TRIM" STATEMENT FROM THE ASCII LOAD
      * * *
      * 01-08-2025 RMZ - ADDED 'BTRACK' - PROCESS TRACKING TO PROGRAM.
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
      * COPYMEMBER: LIBLP/LPFSLT
           SELECT OLT-FILE ASSIGN TO OLT-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OLT1-KEY
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  OLT-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A15SRC/LP01LT.CPY" REPLACING LEADING "LT" BY "OLT".

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/LTCFLE S35 11/03/23-08:06:15 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LTC.CPY".
       COPY "LIBLP/LP01LTC_SQL.CPY".
       COPY "LIBLP/LPWSLTC.CPY".

       01  OLT-PATH                    PIC X(35) VALUE SPACES.
       01  OLT-ALL-PATH.
           03  OLT-ALL-BASE            PIC X(9).
           03  FILLER                  PIC X     VALUE "/".
           03  OLT-ALL-MACHINE         PIC XX.
           03  FILLER                  PIC X     VALUE "/".
           03  OLT-ALL-DATA            PIC X(6).
           03  FILLER                  PIC X(10) VALUE
               "/LP/LTFILE".

       01  LTC1-CONN-OPEN-SW           PIC X     VALUE "N".

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
       01  HOLD-BYTE            PIC X.
           88  VALID-SPECIAL-CHARS  VALUE "(" ")"
                              "/" "\" "-" "?" "+" "%" "#" "[" "]"
                              "!" "<" ">" "$" "*" "&" "." """"  "="
                              "'" "_" ":" "@" "," ";" "~" "|" "{" "}".

       01  MACHINE-R1.
           03  FILLER                  PIC X VALUE X"72".
           03  FILLER                  PIC 9 VALUE 1.
       01  MACHINE-R2.
           03  FILLER                  PIC X VALUE X"72".
           03  FILLER                  PIC 9 VALUE 2.
       01  OLD-ACCTNO                  PIC 9(8).
       01  FILLER REDEFINES OLD-ACCTNO.
           03  NEW-ACCTNO              PIC 9(6).
           03  NEW-SEQNO               PIC 99.

       01  MISC-CURSOR-STAT            PIC X(2) VALUE SPACES.
           88  MISC-CURSOR-STAT-GOOD            VALUE "00".
           88  MISC-CURSOR-STAT-BAD             VALUE "  " THRU "//"
                                                      "01" THRU "~~".

       01  PROCESS-STAT                PIC X(08)    VALUE SPACES.
           88  PROCESS-STAT-GOOD                    VALUE "00".
           88  PROCESS-STAT-BAD                     VALUE "  " THRU "//"
                                                         "01" THRU "~~".

       01  TOTAL-ERRORS-CTR            PIC 9(9)     VALUE 0.
       01  A15-RECORD-CTR              PIC 9(9)     VALUE 0.
           88  NO-RECORDS-FOUND                     VALUE ZEROES.
       01  S35-RECORD-CTR              PIC 9(9)     VALUE 0.

       01  STARTING-TIME               PIC X(08)    VALUE SPACES.
       01  SUB                         PIC 9(04)    VALUE ZEROES.

       01  A15-FILE-FOUND-SW           PIC X        VALUE "N".
       01  LTC-EOF-SW                  PIC X        VALUE "N".
       01  OLTC-EOF-SW                 PIC X        VALUE "N".

       01  WS-LTC1-KEY.
           05  WS-LTC-BRNO             PIC 9(4).
           05  WS-LTC-ACCTNO           PIC 9(8).
 
       01  WS-OLTC1-KEY.
           05  WS-OLTC-BRNO            PIC 9(4).
           05  WS-OLTC-ACCTNO          PIC 9(8).
           05  FILLER REDEFINES WS-OLTC-ACCTNO.
               07 WS-OLTC-ACCTNO1-2    PIC 9(2).
               07 WS-OLTC-ACCTNO3-8    PIC 9(6).

       01  WS-PREV-OLTC1-KEY.
           05  WS-PREV-OLTC-BRNO       PIC 9(4).
           05  WS-PREV-OLTC-ACCTNO     PIC 9(8).

       01  WS-ACCTNO                   PIC 9(8).
       01  FILLER REDEFINES WS-ACCTNO.
           03 WS-ACCTNO1-6             PIC 9(6).
           03 FILLER                   PIC 9(2).

       01  SYSDATE-YYYYMMDD            PIC 9(8)     VALUE ZEROES.

       01  OLD-ACCTNO1                 PIC 9(8).
       01  FILLER REDEFINES OLD-ACCTNO1.
           03  NEW-ACCTNO1             PIC 9(6).
           03  FILLER                  PIC 99.

       01  LEGEND                      PIC X(79)    VALUE SPACES.
       01  LEGEND-LIST                 PIC X(07)    VALUE "LEGEND.".

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
           03  FILLER                  PIC X(6)  VALUE SPACES.
           03  H01-NAME                PIC X(40) VALUE SPACES.
           03  FILLER                  PIC X(6)  VALUE SPACES.
           03  FILLER                  PIC X(06) VALUE "PAGE: ".
           03  H01-PAGE                PIC Z(04).

      *-----------------------------------------------------------------
       01  HEADER-LINE-02.
           03  FILLER                  PIC X(10) VALUE "USER ID:".
           03  H02-USERID              PIC X(10) VALUE SPACES.
           03  FILLER                  PIC X(10) VALUE SPACES.
           03  H02-TITLE               PIC X(60) VALUE 
           "COLLATERAL TRAILER FILE (LTCFILE) COMPARE A15 TO S35".

       01  HEADER-LINE-03.
           03  FILLER                PIC X(4)  VALUE "BRNO".
           03  FILLER                PIC X(5)  VALUE SPACES.
           03  FILLER                PIC X(6)  VALUE "ACCTNO".
           03  FILLER                PIC X(5)  VALUE SPACES.
           03  FILLER                PIC X(11) VALUE "DESCRIPTION".
           03  FILLER                PIC X(17) VALUE SPACES.
           03  FILLER                PIC X(3)  VALUE "A15".
           03  FILLER                PIC X(29) VALUE SPACES.
           03  FILLER                PIC X(3)  VALUE "S35".

      ******************************************************************
      *
      *    D E T A I L - L I N E - W O R K E R S
      *
      ******************************************************************
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-BRNO                       PIC Z(3)9.
           03  FILLER                         PIC X(5).
           03  D01-ACCTNO                     PIC Z(7)9.
           03  FILLER                         PIC X(3).
           03  D01-TEXT                       PIC X(25).
           03  FILLER                         PIC X(3).
           03  D01-A15                        PIC X(30).
           03  FILLER REDEFINES D01-A15.
               05  D01-A15-NUM                PIC Z(7)9.9(3).
               05  FILLER                     PIC X(8).
           03  FILLER                         PIC X(2).
           03  D01-S35                        PIC X(30).
           03  FILLER REDEFINES D01-S35.
               05  D01-S35-NUM                PIC Z(7)9.9(3).
               05  FILLER                     PIC X(18).

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
       COPY "LIBUT/LTCFLE_DEF.CPY".
       COPY "LIBUT/LTCFLE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/LTCFLE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE OLT-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LTC1-FILE.
           PERFORM CLOSE-LTC1-CONNECTION.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE OLT-FILE.
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
           MOVE "LTCFLE" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE LTCFLE-QUEUE-POS TO Q-POS.
           MOVE LTCFLE-COPIES-POS TO C-POS.
           MOVE LTCFLE-PRU-POS   TO P-POS.
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
           PERFORM OPEN-LTC1-FILE.

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

      D    STOP MESS.

           MOVE 'N' TO LTC-EOF-SW OLTC-EOF-SW.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO LTC-PATH-OVERRIDE.

           MOVE BR-NO                TO QLTC1-WBEG-BRNO
                                        QLTC1-WEND-BRNO.
           MOVE BEG-ACCTNO           TO QLTC1-WBEG-ACCTNO.
           MOVE END-ACCTNO           TO QLTC1-WEND-ACCTNO.

           PERFORM START-LTC1-FILE.
           PERFORM PROCESS-READ-LTC1. 

           MOVE "Y"          TO A15-FILE-FOUND-SW.
           MOVE SOURCE-PATH  TO OLT-ALL-BASE.
           MOVE MACHINE-R1   TO OLT-ALL-MACHINE.
           MOVE BR-DATA-PATH TO OLT-ALL-DATA.
           MOVE OLT-ALL-PATH TO OLT-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE MACHINE-R2   TO OLT-ALL-MACHINE
              MOVE OLT-ALL-PATH TO OLT-PATH ACCESS-BUF
              PERFORM ACCESS-CALL
              IF ( STAT-BAD )
                 MOVE "MISSING A15 FILE" TO D01-TEXT
                 MOVE BR-NO              TO D01-BRNO
                 MOVE 0                  TO D01-ACCTNO 
                 MOVE OLT-ALL-PATH       TO D01-A15
                 PERFORM WRITE-DETAIL-LINE-ERROR
                 MOVE "N"                TO A15-FILE-FOUND-SW.

           IF A15-FILE-FOUND-SW = "N"
              MOVE "Y"     TO OLTC-EOF-SW
              MOVE ALL '9' TO WS-OLTC1-KEY
           ELSE
              MOVE BEG-ACCTNO TO NEW-ACCTNO
              MOVE 01         TO NEW-SEQNO
              MOVE OLD-ACCTNO TO OLT-ACCTNO
              MOVE "C"        TO OLT-CODE
              MOVE 0          TO OLT-SEQNO
              PERFORM OPEN-OLT1-FILE
              PERFORM START-OLT1-FILE
              PERFORM PROCESS-READ-OLTC1.

           PERFORM PROCESS-BRANCH-NEXT-LTC
              UNTIL LTC-EOF-SW = "Y" AND OLTC-EOF-SW = "Y".

           PERFORM CLOSE-LTC1-FILE.
           PERFORM CLOSE-OLT1-FILE.

           GO TO PROCESS-BRANCH-DRIVER-EXIT.

      *********************************************************
       PROCESS-BRANCH-NEXT-LTC.
      *********************************************************
           PERFORM SEND-PROGRESS-INDICATOR.

           PERFORM COMPARE-FIELDS.

      * CANT READ LTC WHEN MATCH - OLTC WILL CONTAIN MULT ROWS FOR 1 LTC ROW
           IF WS-OLTC1-KEY < WS-LTC1-KEY 
              PERFORM PROCESS-READ-OLTC1
           ELSE
              IF WS-OLTC1-KEY > WS-LTC1-KEY
                 PERFORM PROCESS-READ-LTC1 
              ELSE
                 PERFORM PROCESS-READ-OLTC1.

       PROCESS-BRANCH-DRIVER-EXIT.
           EXIT.

      ******************************************************************
       PROCESS-READ-OLTC1 SECTION.
      ******************************************************************

           MOVE WS-OLTC1-KEY TO WS-PREV-OLTC1-KEY.

           PERFORM READ-OLT1-FILE-NEXT.
           IF IO-BAD 
              MOVE ALL '9' TO WS-OLTC1-KEY
              MOVE "Y"     TO OLTC-EOF-SW
              GO TO EXIT-PROCESS-READ-OLTC1.
           
           IF OLT-CODE NOT = "C"
              GO TO PROCESS-READ-OLTC1.
                                      
           MOVE OLT-ACCTNO TO OLD-ACCTNO1.
           IF NEW-ACCTNO1 = 0
              GO TO PROCESS-READ-OLTC1. 

           MOVE OLT-ACCTNO     TO WS-ACCTNO. 
           IF ( WS-ACCTNO1-6  < BEG-ACCTNO ) OR
              ( WS-ACCTNO1-6  > END-ACCTNO )
              GO TO PROCESS-READ-OLTC1.
   
           MOVE BR-NO          TO WS-OLTC-BRNO.
           MOVE ZEROES         TO WS-OLTC-ACCTNO1-2. 
           MOVE WS-ACCTNO1-6   TO WS-OLTC-ACCTNO3-8.
           ADD  1              TO A15-RECORD-CTR.

       EXIT-PROCESS-READ-OLTC1.
           EXIT.

      ******************************************************************
       PROCESS-READ-LTC1 SECTION.
      ******************************************************************

           PERFORM READ-LTC1-FILE-NEXT.
           IF IO-BAD 
              MOVE ALL '9' TO WS-LTC1-KEY
              MOVE "Y"     TO LTC-EOF-SW
           ELSE 
              MOVE BR-NO       TO WS-LTC-BRNO
              MOVE LTC-ACCTNO  TO WS-LTC-ACCTNO
              ADD 1            TO S35-RECORD-CTR.

       EXIT-PROCESS-READ-LTC1.
           EXIT.

      ******************************************************************
       COMPARE-FIELDS SECTION.
      ******************************************************************

      *    STOP MESS.
      *    MOVE LTC-PATH-OWNBR TO LTC-BRNO.
      *    MOVE OLT-ACCTNO     TO OLD-ACCTNO.
      *    MOVE NEW-ACCTNO     TO LTC-ACCTNO.
      * S35 ONLY HAS THE 1 SEQUENCE, READ IT FOR EACH A15 SEQUENCE
      *    MOVE 1              TO LTC-SEQNO.
      *    PERFORM READ-LTC1-FILE.
      *    IF IO-FG NOT = 0
      *       MOVE "MISSING S35 RECORD" TO D01-TEXT
      *       MOVE BR-NO        TO D01-BRNO
      *       MOVE LTC-ACCTNO   TO D01-ACCTNO
      *       PERFORM WRITE-DETAIL-LINE-ERROR
      *       GO TO EXIT-COMPARE-FIELDS.

      * A15 HAS MULT RECORDS TO S35
           IF WS-OLTC1-KEY > WS-LTC1-KEY 
              IF WS-PREV-OLTC1-KEY = WS-LTC1-KEY 
                 PERFORM PROCESS-READ-LTC1
                 IF LTC-EOF-SW = "Y" AND OLTC-EOF-SW = "Y"
                    GO TO EXIT-COMPARE-FIELDS.

           IF WS-OLTC1-KEY < WS-LTC1-KEY 
              MOVE "MISSING S35 RECORD" TO D01-TEXT
              MOVE WS-OLTC-BRNO         TO D01-BRNO  
              MOVE WS-OLTC-ACCTNO       TO D01-ACCTNO 
              MOVE WS-OLTC1-KEY         TO D01-A15
              MOVE WS-LTC1-KEY          TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR 
              GO TO EXIT-COMPARE-FIELDS.

           IF WS-OLTC1-KEY > WS-LTC1-KEY
              MOVE "MISSING A15 RECORD" TO D01-TEXT
              MOVE WS-LTC-BRNO          TO D01-BRNO
              MOVE WS-LTC-ACCTNO        TO D01-ACCTNO
              MOVE WS-OLTC1-KEY         TO D01-A15
              MOVE WS-LTC1-KEY          TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.

           IF OLT-SEQNO = 1
              MOVE 1 TO SUB
              PERFORM COMPARE-AUTO
           ELSE
           IF OLT-SEQNO = 2
              MOVE 2 TO SUB
              PERFORM COMPARE-AUTO
           ELSE
           IF OLT-SEQNO = 3
              MOVE 1 TO SUB
              PERFORM COMPARE-HHGS
           ELSE
           IF OLT-SEQNO = 4
              MOVE 2 TO SUB
              PERFORM COMPARE-HHGS
           ELSE
           IF OLT-SEQNO = 5
              MOVE 3 TO SUB
              PERFORM COMPARE-HHGS.

       EXIT-COMPARE-FIELDS.
           EXIT.

      ****************************************
       COMPARE-HHGS SECTION.
      ****************************************

           IF OLT-COLL-INFO1 NOT = LTC-COLL-INFO1(SUB)
              MOVE SPACES TO D01-TEXT
              STRING "LT-COLL-INFO1",SUB INTO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LTC-ACCTNO          TO D01-ACCTNO
              MOVE OLT-COLL-INFO1      TO D01-A15
              MOVE LTC-COLL-INFO1(SUB) TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLT-COLL-INFO2 NOT = LTC-COLL-INFO2(SUB)
              MOVE SPACES TO D01-TEXT
              STRING "LT-COLL-INFO2",SUB INTO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LTC-ACCTNO          TO D01-ACCTNO
              MOVE OLT-COLL-INFO2      TO D01-A15
              MOVE LTC-COLL-INFO2(SUB) TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLT-COLL-INFO3 NOT = LTC-COLL-INFO3(SUB)
              MOVE SPACES TO D01-TEXT
              STRING "LT-COLL-INFO3",SUB INTO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LTC-ACCTNO          TO D01-ACCTNO
              MOVE OLT-COLL-INFO3      TO D01-A15
              MOVE LTC-COLL-INFO3(SUB) TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLT-COLL-INFO4 NOT = LTC-COLL-INFO4(SUB)
              MOVE SPACES TO D01-TEXT
              STRING "LT-COLL-INFO4",SUB INTO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LTC-ACCTNO          TO D01-ACCTNO
              MOVE OLT-COLL-INFO4      TO D01-A15
              MOVE LTC-COLL-INFO4(SUB) TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

      ****************************************
       COMPARE-AUTO SECTION.
      ****************************************
           IF OLT-DESCRIPTION NOT = LTC-DESC(SUB)
              MOVE SPACES TO D01-TEXT
              STRING "LT-DESC",SUB INTO D01-TEXT
              MOVE BR-NO            TO D01-BRNO
              MOVE LTC-ACCTNO       TO D01-ACCTNO
              MOVE OLT-DESCRIPTION  TO D01-A15
              MOVE LTC-DESC(SUB)    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLT-MAKE NOT = LTC-MAKE(SUB)
              MOVE SPACES TO D01-TEXT
              STRING "LT-MAKE",SUB INTO D01-TEXT
              MOVE BR-NO           TO D01-BRNO
              MOVE LTC-ACCTNO      TO D01-ACCTNO
              MOVE OLT-MAKE        TO D01-A15
              MOVE LTC-MAKE(SUB)   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLT-MODEL NOT = LTC-MODEL(SUB)
              MOVE SPACES TO D01-TEXT
              STRING "LT-MODEL",SUB INTO D01-TEXT
              MOVE BR-NO           TO D01-BRNO
              MOVE LTC-ACCTNO      TO D01-ACCTNO
              MOVE OLT-MODEL       TO D01-A15
              MOVE LTC-MODEL(SUB)  TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLT-YEAR NOT = LTC-YEAR(SUB)
              MOVE SPACES TO D01-TEXT
              STRING "LT-YEAR",SUB INTO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LTC-ACCTNO          TO D01-ACCTNO
              MOVE OLT-YEAR            TO D01-A15
              MOVE LTC-YEAR(SUB)       TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLT-STYLE NOT = LTC-STYLE(SUB)
              MOVE SPACES TO D01-TEXT
              STRING "LT-STYLE",SUB INTO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LTC-ACCTNO          TO D01-ACCTNO
              MOVE OLT-STYLE           TO D01-A15
              MOVE LTC-STYLE(SUB)      TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLT-CLASS NOT = LTC-CLASS(SUB)
              MOVE SPACES TO D01-TEXT
              STRING "LT-CLASS",SUB INTO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LTC-ACCTNO          TO D01-ACCTNO
              MOVE OLT-CLASS           TO D01-A15
              MOVE LTC-CLASS(SUB)      TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLT-VIN NOT = LTC-VIN(SUB)
              MOVE SPACES TO D01-TEXT
              STRING "LT-VIN",SUB INTO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LTC-ACCTNO          TO D01-ACCTNO
              MOVE OLT-VIN             TO D01-A15
              MOVE LTC-VIN(SUB)        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLT-TITLE-NO NOT = LTC-TITLE-NO(SUB)
              MOVE SPACES TO D01-TEXT
              STRING "LT-TITLE-NO",SUB INTO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LTC-ACCTNO          TO D01-ACCTNO
              MOVE OLT-TITLE-NO        TO D01-A15
              MOVE LTC-TITLE-NO(SUB)   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLT-MILEAGE NOT NUMERIC OR
               (OLT-MILEAGE = 2020202 OR 0202020)
              MOVE 0 TO OLT-MILEAGE.
           IF OLT-MILEAGE NOT = LTC-MILEAGE(SUB)
              MOVE SPACES TO D01-TEXT
              STRING "LT-MILEAGE",SUB INTO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LTC-ACCTNO          TO D01-ACCTNO
              MOVE OLT-MILEAGE         TO D01-A15
              MOVE LTC-MILEAGE(SUB)    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLT-VALUE NOT NUMERIC OR
               (OLT-VALUE = 20202.02)
              MOVE 0 TO OLT-VALUE.
           IF OLT-VALUE NOT = LTC-VALUE(SUB)
              MOVE SPACES TO D01-TEXT
              STRING "LT-VALUE",SUB INTO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LTC-ACCTNO          TO D01-ACCTNO
              MOVE OLT-VALUE           TO D01-A15-NUM
              MOVE LTC-VALUE(SUB)      TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLT-SELLPR NOT NUMERIC OR
               (OLT-SELLPR = 20202.02)
              MOVE 0 TO OLT-SELLPR.
           IF OLT-SELLPR NOT = LTC-SELLPR(SUB)
              MOVE SPACES TO D01-TEXT
              STRING "LT-SELLPR",SUB INTO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LTC-ACCTNO          TO D01-ACCTNO
              MOVE OLT-SELLPR          TO D01-A15-NUM
              MOVE LTC-SELLPR(SUB)     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLT-MISC-AMT NOT NUMERIC OR
               (OLT-MISC-AMT = 20202.02)
              MOVE 0 TO OLT-MISC-AMT.
           IF OLT-MISC-AMT NOT = LTC-MISC-AMT(SUB)
              MOVE SPACES TO D01-TEXT
              STRING "LT-MISC-AMT",SUB INTO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LTC-ACCTNO          TO D01-ACCTNO
              MOVE OLT-MISC-AMT        TO D01-A15-NUM
              MOVE LTC-MISC-AMT(SUB)   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLT-SENT-FLAG NOT = LTC-SENT-FLAG(SUB)
              MOVE SPACES TO D01-TEXT
              STRING "LT-SENT-FLAG",SUB INTO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LTC-ACCTNO          TO D01-ACCTNO
              MOVE OLT-SENT-FLAG       TO D01-A15
              MOVE LTC-SENT-FLAG(SUB)  TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLT-VIN-CHG-DATE NOT NUMERIC
              MOVE 0 TO OLT-VIN-CHG-DATE.
           MOVE OLT-VIN-CHG-DATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LTC-VIN-CHG-DATE(SUB)
              MOVE SPACES TO D01-TEXT
              STRING "LT-VIN-CHG-DATE",SUB INTO D01-TEXT
              MOVE BR-NO                 TO D01-BRNO
              MOVE LTC-ACCTNO            TO D01-ACCTNO
              MOVE DATE-YYYYMMDD         TO D01-A15
              MOVE LTC-VIN-CHG-DATE(SUB) TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.


      ******************************************************************
       WRITE-DETAIL-LINE-ERROR SECTION.
      ******************************************************************
           ADD 1    TO TOTAL-ERRORS-CTR.
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

           MOVE "BEGINNING ACCOUNT NUMBER      : "    TO R01-PROMPT.
           MOVE BEG-ACCTNO                            TO R01-NUMERIC.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    ACCOUNT NUMBER      : "    TO R01-PROMPT.
           MOVE END-ACCTNO                            TO R01-NUMERIC.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "A15 RECORDS PROCESSED         :"     TO R01-PROMPT.
           MOVE A15-RECORD-CTR                        TO R01-NUMERIC.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "S35 RECORDS PROCESSED         :"     TO R01-PROMPT.
           MOVE S35-RECORD-CTR                        TO R01-NUMERIC.
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
           MOVE EXT-CONAME-CONAME TO LTCFLE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LTCFLE-SYSDATE.
           DISPLAY LTCFLE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE LTCFLE-FORM-FIELDS TO WR-BUF.
           MOVE LTCFLE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE LTCFLE-HELP--FILE TO HELP-LINK-FILE.
           MOVE LTCFLE-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/LTCFLE_EVA.CPY".
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
       COPY "LIBLP/LPLTCGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

      * COPY "LIBLP/LPLTC1RN.CPY".
      * COPYMEMBER: LIBLP/LPLTC1RN
       LOAD-LTC1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LTC-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LTC-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LTC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LTC-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LTC-ALL-DATA
              MOVE LTC-ALL-PATH             TO LTC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LTC-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LTC-ALL-BASE
              MOVE FILPATH-MACHINE         TO LTC-ALL-MACHINE
              MOVE FILPATH-DATA            TO LTC-ALL-DATA
              MOVE LTC-ALL-PATH             TO LTC-PATH.

      *-----------------------------------------------------------------
       OPEN-LTC1-FILE.
           PERFORM LOAD-LTC1-FILE.
           PERFORM OPEN-IT.
           MOVE LTC-PATH-SQL TO E-FILE.

           IF LTC1-CONN-OPEN-SW = "N" 
              MOVE "Y"         TO LTC1-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS LTC1_CONNECT 
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-LTC1-FILE.
           PERFORM START-IT.
           MOVE LTC-PATH-OWNBR  TO LTC-BRNO.
           MOVE LTC-PATH-SQL    TO E-FILE.
           MOVE LTC1-KEY        TO E-KEYX.

           IF ( LTC1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LTC1-FILE.

           EXEC SQL SET CONNECTION LTC1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE LTC1_CURSOR CURSOR FOR
                :LTC1-RETURN-CODE = EXEC SSP_LTCFLE_LTCFILE_SELECT 
              ( :QLTC1-WBEG-BRNO,
                :QLTC1-WBEG-ACCTNO,
                :QLTC1-WEND-ACCTNO) 
           END-EXEC.

           EXEC SQL
               OPEN LTC1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE STAT---GOOD TO LTC1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-LTC1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LTC-PATH-OWNBR  TO LTC-BRNO.
           MOVE LTC-PATH-SQL    TO E-FILE.
           MOVE LTC1-KEY        TO E-KEYX.
           INITIALIZE QLTC-REC.

           EXEC SQL SET CONNECTION LTC1_CONNECT END-EXEC.

           IF ( LTC1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LTC1_CURSOR INTO :QLTC-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTC1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LTC-FIELDS.
  
           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-LTC1-FILE.

           PERFORM CLOSE-IT.

           IF ( LTC1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION LTC1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE LTC1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO LTC1-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-LTC1-CONNECTION.

           IF LTC1-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION LTC1_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT LTC1_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO LTC1-CONN-OPEN-SW.

      *----------------------------------------------------------------- 

       OPEN-OLT1-FILE.
           PERFORM OPEN-IT.
           MOVE OLT-PATH TO E-FILE.
           OPEN INPUT OLT-FILE.
           IF IO-FG = 8
              GO TO OPEN-OLT1-FILE.
           IF IO-FG = 7
              CLOSE OLT-FILE
              GO TO OPEN-OLT1-FILE.
      *READ-OLT1-FILE.
      *    PERFORM READ-IT.
      *    MOVE OLT-PATH TO E-FILE.
      *    MOVE OLT1-KEY TO E-KEYX.
      *    READ OLT-FILE.
      *    IF IO-FG = 8
      *       GO TO READ-OLT1-FILE.
       READ-OLT1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OLT-PATH TO E-FILE.
           MOVE OLT1-KEY TO E-KEYX.
           READ OLT-FILE NEXT.
           IF IO-FG = 8
             GO TO READ-OLT1-FILE-NEXT.
       START-OLT1-FILE.
           PERFORM START-IT.
           MOVE OLT-PATH TO E-FILE.
           MOVE OLT1-KEY TO E-KEYX.
           START OLT-FILE KEY NOT < OLT1-KEY.
       CLOSE-OLT1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OLT-FILE.

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
              DESTROY LTCFLE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
