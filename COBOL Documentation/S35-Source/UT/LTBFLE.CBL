       IDENTIFICATION DIVISION.
       PROGRAM-ID.    LTBFLE.
       DATE-WRITTEN.  10-24-2023.
      ******************************************************************
      *
      * DESCRIPTION   : COMPARE S35 LTBFLE WITH A15 LTBFLE
      *                  LOAN BANKRUPTCY TRAILER
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
           "@(#)UT/LTBFLE S35 03/20/25-10:40:09 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LTB.CPY".
       COPY "LIBLP/LP01LTB_SQL.CPY".
       COPY "LIBLP/LPWSLTB.CPY".

       01  OLT-PATH                    PIC X(35) VALUE SPACES.
       01  OLT-ALL-PATH.
           03  OLT-ALL-BASE     PIC X(9).
           03  FILLER           PIC X     VALUE "/".
           03  OLT-ALL-MACHINE  PIC XX.
           03  FILLER           PIC X     VALUE "/".
           03  OLT-ALL-DATA     PIC X(6).
           03  FILLER           PIC X(10) VALUE
               "/LP/LTFILE".

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
       01  WSS-AM-APP-NUMBER           PIC 9(09)     VALUE ZEROES.
       01  FILLER            REDEFINES WSS-AM-APP-NUMBER.
           03  WSS-AM-APPNO            PIC 9(08).
           03  WSS-AM-SEQNO            PIC 9(01).
       01  WSS-MA-APP-NUMBER REDEFINES WSS-AM-APP-NUMBER
                                       PIC 9(09).
       01  FILLER            REDEFINES WSS-AM-APP-NUMBER.
           03  WSS-MA-APPNO            PIC 9(08).
           03  WSS-MA-SEQNO            PIC 9(01).

       01  HOLD-LN-TILLNO       PIC 9(2).
       01  HOLD-LN-MESSAGE      PIC X(30).
       01  HOLD-LN-MESSAGEX REDEFINES HOLD-LN-MESSAGE.
           03  HOLD-LN-MESS-BYTE PIC X OCCURS 30.
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

       01  USE-OLT-KEY.
           03  USE-OLT-BRNO            PIC 9(4).
           03  USE-OLT-ACCTNO          PIC 9(8).
           03  USE-OLT-SEQNO           PIC 999         VALUE 1.

       01  LTB1-CONN-OPEN-SW           PIC X           VALUE "N".
       01  LTB-EOF-SW                  PIC X           VALUE "N".
       01  OLTB-EOF-SW                 PIC X           VALUE "N".
       01  A15-FILE-FOUND-SW           PIC X           VALUE "N".
       01  TOTAL-ERRORS-CTR            PIC 9(9)        VALUE 0.
       01  PREV-OLT-KEY.
           03  PREV-OLT-BRNO           PIC 9(4)        VALUE 0.
           03  PREV-OLT-ACCTNO         PIC 9(8)        VALUE 0.
           03  PREV-OLT-SEQNO          PIC 999         VALUE 1.


       01  SQL-RECORDS-PROCESSED       PIC 9(9)        VALUE 0.
       01  A15-RECORDS-PROCESSED       PIC 9(9)        VALUE 0.
           88  NO-RECORDS-FOUND                        VALUE ZEROES.

       01  STARTING-TIME               PIC X(08)     VALUE SPACES.
       01  SUB                         PIC 9(04)     VALUE ZEROES.

       01  SYSDATE-YYYYMMDD            PIC 9(8)      VALUE ZEROES.

       01  OLD-ACCTNO1                 PIC 9(8).
       01  FILLER REDEFINES OLD-ACCTNO1.
           03  NEW-ACCTNO1             PIC 9(6).
           03  FILLER                  PIC 99.

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
           03  H02-TITLE               PIC X(60) VALUE 
           "BANKRUPT TRAILER FILE (LTBFILE) COMPARE A15 TO S35".

       01  HEADER-LINE-03.
           03  FILLER                PIC X(4)  VALUE "BRNO".
           03  FILLER                PIC X(3)  VALUE SPACES.
           03  FILLER                PIC X(8)  VALUE " ACCTNO ".
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
       COPY "LIBUT/LTBFLE_DEF.CPY".
       COPY "LIBUT/LTBFLE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/LTBFLE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE OLT-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LTB1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           PERFORM CLOSE-LTB1-CONNECTION.
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
           MOVE "LTBFLE" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE LTBFLE-QUEUE-POS TO Q-POS.
           MOVE LTBFLE-COPIES-POS TO C-POS.
           MOVE LTBFLE-PRU-POS   TO P-POS.
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

           MOVE "S  A090S-PATH."           TO VDU.     
           MOVE SOURCE-PATH                TO VDUBUF.
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

           MOVE "N" TO LTB-EOF-SW OLTB-EOF-SW.
           MOVE "Y" TO A15-FILE-FOUND-SW.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO LTB-PATH-OVERRIDE.

           MOVE BR-NO      TO QLTB1-WBEG-BRNO QLTB1-WEND-BRNO.
           MOVE BEG-ACCTNO TO QLTB1-WBEG-ACCTNO.
           MOVE ZEROS      TO QLTB1-WBEG-SEQNO.
           MOVE END-ACCTNO TO QLTB1-WEND-ACCTNO.
           MOVE 999        TO QLTB1-WEND-SEQNO.

           PERFORM OPEN-LTB1-FILE.
           PERFORM START-LTB1-FILE.  *> SSP  LTBFILE
           PERFORM M-READ-LTB.

      D    STOP MESS.

           MOVE "Y" TO A15-FILE-FOUND-SW.
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
                  MOVE OLT-ALL-PATH       TO D01-A15
                  MOVE SPACES             TO D01-S35
                  PERFORM WRITE-DETAIL-LINE-ERROR
                  MOVE "N" TO A15-FILE-FOUND-SW.

           IF A15-FILE-FOUND-SW = "N"
              MOVE ALL '9' TO USE-OLT-KEY
              MOVE "Y"     TO OLTB-EOF-SW
           ELSE
              MOVE BEG-ACCTNO TO NEW-ACCTNO
              MOVE 01         TO NEW-SEQNO
              MOVE OLD-ACCTNO TO OLT-ACCTNO
              MOVE "B"        TO OLT-CODE 
              MOVE 0          TO OLT-SEQNO
              PERFORM OPEN-OLT1-FILE
              PERFORM START-OLT1-FILE
              PERFORM M-READ-OLTB.

           PERFORM PROCESS-BRANCH-NEXT-LTB
              UNTIL LTB-EOF-SW = "Y" AND OLTB-EOF-SW = "Y".

           PERFORM CLOSE-OLT1-FILE.
           PERFORM CLOSE-LTB1-FILE.

           GO TO PROCESS-BRANCH-DRIVER-END.

       PROCESS-BRANCH-NEXT-LTB.
           PERFORM SEND-PROGRESS-INDICATOR.

           PERFORM COMPARE-FIELDS.
           IF LTB-EOF-SW NOT = "Y" OR OLTB-EOF-SW NOT = "Y"
              IF USE-OLT-KEY < LTB1-KEY
                 PERFORM M-READ-OLTB 
              ELSE
                 IF USE-OLT-KEY > LTB1-KEY
                    PERFORM M-READ-LTB
                 ELSE
      * A15 HAS MULT RECORDS TO S35 - CAN'T READ S35 IF KEY ARE EQUAL
                    PERFORM M-READ-OLTB.
      *           PERFORM M-READ-LTB.

       PROCESS-BRANCH-DRIVER-END.
           EXIT.

       M-READ-LTB SECTION.
           PERFORM READ-LTB1-FILE-NEXT.    *> SSP  LTBFILE
           IF IO-BAD
              MOVE ALL '9' TO LTB1-KEY
              MOVE "Y"     TO LTB-EOF-SW
           ELSE
              ADD 1 TO SQL-RECORDS-PROCESSED.

       EXIT-M-READ-LTB1.
           EXIT.

       M-READ-OLTB SECTION.

           MOVE USE-OLT-BRNO   TO PREV-OLT-BRNO.
           MOVE USE-OLT-ACCTNO TO PREV-OLT-ACCTNO.

           PERFORM READ-OLT1-FILE-NEXT.
           IF IO-BAD
              MOVE ALL '9'  TO USE-OLT-KEY
              MOVE "Y"      TO OLTB-EOF-SW
              GO TO EXIT-M-READ-OLTB.

           MOVE OLT-ACCTNO TO OLD-ACCTNO1.
           IF NEW-ACCTNO1 = 0
              GO TO M-READ-OLTB.

           IF OLT-CODE NOT = "B"
              GO TO M-READ-OLTB.
                               
           MOVE OLT-ACCTNO TO OLD-ACCTNO.
           IF ( NEW-ACCTNO < BEG-ACCTNO ) OR
              ( NEW-ACCTNO > END-ACCTNO )
              GO TO M-READ-OLTB
           ELSE
              ADD 1 TO A15-RECORDS-PROCESSED
              MOVE BR-NO      TO USE-OLT-BRNO
              MOVE NEW-ACCTNO TO USE-OLT-ACCTNO
              MOVE 1          TO USE-OLT-SEQNO.

       EXIT-M-READ-OLTB.
           EXIT.

      ******************************************************************
       COMPARE-FIELDS SECTION.
      ******************************************************************

      * BANKRUPT INFO: A15
      *  CODE   SEQNO   TYPE
      *    B      1     N/A     GENERAL INFO
      *           2     N/A     DEBTOR'S ATTORNEY
      *           3     N/A     CLERK OF BANKRUPTCY COURT
      *           4     N/A     TRUSTEE
      *           5     N/A     REAFFIRMATION AND DISCHARGE


      * A15 HAS MULT RECORDS TO S35 - NEED TO READ LTB IF LTB KEY LESS AND MATCHES PREV 
           IF USE-OLT-KEY > LTB1-KEY AND LTB1-KEY = PREV-OLT-KEY 
              PERFORM M-READ-LTB
              IF LTB-EOF-SW = "Y" AND OLTB-EOF-SW = "Y"
                 GO TO EXIT-COMPARE-FIELDS.

      *    NO NEED TO REPORT ALL SEQUENCES FROM A15
           IF USE-OLT-KEY < LTB1-KEY
              IF USE-OLT-KEY NOT = PREV-OLT-KEY 
                 MOVE "MISSING S35 RECORD" TO D01-TEXT
                 MOVE BR-NO                TO D01-BRNO
                 MOVE OLT-ACCTNO           TO D01-ACCTNO
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              GO TO EXIT-COMPARE-FIELDS.

           IF USE-OLT-KEY > LTB1-KEY
              MOVE "MISSING A15 RECORD" TO D01-TEXT
              MOVE BR-NO                TO D01-BRNO
              MOVE LTB-ACCTNO           TO D01-ACCTNO
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.

           IF OLT-SEQNO = 1
              PERFORM COMPARE-SEQ-1
              GO TO EXIT-COMPARE-FIELDS
           ELSE
           IF OLT-SEQNO = 2 OR 3 OR 4
              PERFORM COMPARE-SEQ-2-3-4
              GO TO EXIT-COMPARE-FIELDS.

      * OLT-SEQNO = 5

           MOVE OLT-B-REAFF-DATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LTB-REAFF-DATE
              MOVE "LTB-REAFF-DATE" TO D01-TEXT
              MOVE BR-NO            TO D01-BRNO
              MOVE LTB-ACCTNO       TO D01-ACCTNO
              MOVE DATE-YYYYMMDD    TO D01-A15
              MOVE LTB-REAFF-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLT-B-REAFF-SYSDATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LTB-REAFF-SYSDATE
              MOVE "LTB-REAFF-SYSDATE" TO D01-TEXT
              MOVE BR-NO             TO D01-BRNO
              MOVE LTB-ACCTNO        TO D01-ACCTNO
              MOVE DATE-YYYYMMDD     TO D01-A15
              MOVE LTB-REAFF-SYSDATE TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLT-B-REAFF-BALANCE NOT NUMERIC
              MOVE 0 TO OLT-B-REAFF-BALANCE.
           IF LTB-REAFF-BALANCE NOT = OLT-B-REAFF-BALANCE
              MOVE "LTB-REAFF-BALANCE" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LTB-ACCTNO          TO D01-ACCTNO
              MOVE OLT-B-REAFF-BALANCE TO D01-A15-NUM
              MOVE LTB-REAFF-BALANCE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OLT-B-REAFF-PAYAMT NOT NUMERIC
              MOVE 0 TO OLT-B-REAFF-PAYAMT.
           IF LTB-REAFF-PAYAMT NOT = OLT-B-REAFF-PAYAMT
              MOVE "LTB-REAFF-PAYAMT" TO D01-TEXT
              MOVE BR-NO              TO D01-BRNO
              MOVE LTB-ACCTNO         TO D01-ACCTNO
              MOVE OLT-B-REAFF-PAYAMT TO D01-A15-NUM
              MOVE LTB-REAFF-PAYAMT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLT-B-REAFF-NO-PAYS NOT NUMERIC
              MOVE 0 TO OLT-B-REAFF-NO-PAYS.
           IF LTB-REAFF-NO-PAYS NOT = OLT-B-REAFF-NO-PAYS
              MOVE "LTB-REAFF-NO-PAYS" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LTB-ACCTNO          TO D01-ACCTNO
              MOVE OLT-B-REAFF-NO-PAYS TO D01-A15
              MOVE LTB-REAFF-NO-PAYS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLT-B-INTRATE NOT NUMERIC
              MOVE 0 TO OLT-B-INTRATE.
           IF LTB-INTRATE NOT = OLT-B-INTRATE
              MOVE "LTB-INTRATE" TO D01-TEXT
              MOVE BR-NO         TO D01-BRNO
              MOVE LTB-ACCTNO    TO D01-ACCTNO
              MOVE OLT-B-INTRATE TO D01-A15-NUM
              MOVE LTB-INTRATE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLT-B-RESCIND-DATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LTB-RESCIND-DATE
              MOVE "LTB-RESCIND-DATE" TO D01-TEXT
              MOVE BR-NO            TO D01-BRNO
              MOVE LTB-ACCTNO       TO D01-ACCTNO
              MOVE DATE-YYYYMMDD    TO D01-A15
              MOVE LTB-RESCIND-DATE TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LTB-PARTIAL-FULL NOT = OLT-B-PARTIAL-FULL
              MOVE "LTB-PARTIAL-FULL" TO D01-TEXT
              MOVE BR-NO              TO D01-BRNO
              MOVE LTB-ACCTNO         TO D01-ACCTNO
              MOVE OLT-B-PARTIAL-FULL TO D01-A15-NUM
              MOVE LTB-PARTIAL-FULL   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLT-B-DISCHARGE-DATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LTB-DISCHARGE-DATE
              MOVE "LTB-DISCHARGE-DATE" TO D01-TEXT
              MOVE BR-NO                TO D01-BRNO
              MOVE LTB-ACCTNO           TO D01-ACCTNO
              MOVE DATE-YYYYMMDD        TO D01-A15
              MOVE LTB-DISCHARGE-DATE    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLT-B-DISCHARGE-SYSDATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LTB-DISCHARGE-SYSDATE
              MOVE "LTB-DISCHARGE-SYSDATE" TO D01-TEXT
              MOVE BR-NO                   TO D01-BRNO
              MOVE LTB-ACCTNO              TO D01-ACCTNO
              MOVE DATE-YYYYMMDD           TO D01-A15
              MOVE LTB-DISCHARGE-SYSDATE   TO D01-S35.

           IF OLT-B-GROSS-BAL NOT NUMERIC
              MOVE 0 TO OLT-B-GROSS-BAL.
           IF LTB-GROSS-BAL NOT = OLT-B-GROSS-BAL
      D        STOP MESS
              MOVE "LTB-GROSS-BAL"  TO D01-TEXT
              MOVE BR-NO            TO D01-BRNO
              MOVE LTB-ACCTNO       TO D01-ACCTNO
              MOVE OLT-B-GROSS-BAL  TO D01-A15-NUM
              MOVE LTB-GROSS-BAL    TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

       EXIT-COMPARE-FIELDS.
           EXIT.
       
      *********************************
       COMPARE-SEQ-1 SECTION.
      *********************************
           MOVE OLT-B-FILEDATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LTB-FILEDATE
              MOVE "LTB-FILEDATE" TO D01-TEXT
              MOVE BR-NO          TO D01-BRNO
              MOVE LTB-ACCTNO     TO D01-ACCTNO
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE LTB-FILEDATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LTB-CASE-NO NOT = OLT-B-CASE-NO
              MOVE "LTB-CASE-NO" TO D01-TEXT
              MOVE BR-NO         TO D01-BRNO
              MOVE LTB-ACCTNO    TO D01-ACCTNO
              MOVE OLT-B-CASE-NO TO D01-A15
              MOVE LTB-CASE-NO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLT-B-CHAPTER-X = SPACES
              MOVE 0 TO OLT-B-CHAPTER.
           IF LTB-CHAPTER NOT = OLT-B-CHAPTER
              MOVE "LTB-CHAPTER" TO D01-TEXT
              MOVE BR-NO         TO D01-BRNO
              MOVE LTB-ACCTNO    TO D01-ACCTNO
              MOVE OLT-B-CHAPTER TO D01-A15
              MOVE LTB-CHAPTER   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LTB-MISC1 NOT = OLT-B-MISC1
              MOVE "LTB-MISC1" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE LTB-ACCTNO  TO D01-ACCTNO
              MOVE OLT-B-MISC1 TO D01-A15
              MOVE LTB-MISC1   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LTB-MISC2 NOT = OLT-B-MISC2
              MOVE "LTB-MISC2" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE LTB-ACCTNO  TO D01-ACCTNO
              MOVE OLT-B-MISC2 TO D01-A15
              MOVE LTB-MISC2   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LTB-MISC3 NOT = OLT-B-MISC3
              MOVE "LTB-MISC3" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE LTB-ACCTNO  TO D01-ACCTNO
              MOVE OLT-B-MISC3 TO D01-A15
              MOVE LTB-MISC3   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLT-B-BDDATE NOT NUMERIC
              MOVE 0 TO OLT-B-BDDATE.
           MOVE OLT-B-BDDATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LTB-BDDATE
              MOVE "LTB-BDDATE"  TO D01-TEXT
              MOVE BR-NO         TO D01-BRNO
              MOVE LTB-ACCTNO    TO D01-ACCTNO
              MOVE DATE-YYYYMMDD TO D01-A15
              MOVE LTB-BDDATE    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LTB-WHO-FILED NOT = OLT-B-WHO-FILED
              MOVE "LTB-WHO-FILED" TO D01-TEXT
              MOVE BR-NO           TO D01-BRNO
              MOVE LTB-ACCTNO      TO D01-ACCTNO
              MOVE OLT-B-WHO-FILED TO D01-A15
              MOVE LTB-WHO-FILED   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LTB-ENDORSER-FILED NOT = OLT-B-ENDORSER-FILED
              MOVE "LTB-ENDORSER-FILED" TO D01-TEXT
              MOVE BR-NO                TO D01-BRNO
              MOVE LTB-ACCTNO           TO D01-ACCTNO
              MOVE OLT-B-ENDORSER-FILED TO D01-A15
              MOVE LTB-ENDORSER-FILED   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLT-B-CLAIM-AMT NOT NUMERIC OR
               (OLT-B-CLAIM-AMT = 2020202.02 OR 0202020.20)
              MOVE 0  TO OLT-B-CLAIM-AMT.
           IF LTB-CLAIM-AMT NOT = OLT-B-CLAIM-AMT
              MOVE "LTB-CLAIM-AMT"  TO D01-TEXT
              MOVE BR-NO            TO D01-BRNO
              MOVE LTB-ACCTNO       TO D01-ACCTNO
              MOVE OLT-B-CLAIM-AMT  TO D01-A15-NUM
              MOVE LTB-CLAIM-AMT    TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LTB-ACTIVE-INACTIVE NOT = OLT-B-ACTIVE-INACTIVE
              MOVE "LTB-ACTIVE-INACTIVE" TO D01-TEXT
              MOVE BR-NO                 TO D01-BRNO
              MOVE LTB-ACCTNO            TO D01-ACCTNO
              MOVE OLT-B-ACTIVE-INACTIVE TO D01-A15
              MOVE LTB-ACTIVE-INACTIVE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLT-B-INT-LC-DUE NOT NUMERIC OR
               (OLT-B-INT-LC-DUE = 2020202.02 OR 0202020.20)
              MOVE 0               TO OLT-B-INT-LC-DUE.
           IF LTB-INT-LC-DUE NOT = OLT-B-INT-LC-DUE
              MOVE "LTB-INT-LC-DUE"  TO D01-TEXT
              MOVE BR-NO             TO D01-BRNO
              MOVE LTB-ACCTNO        TO D01-ACCTNO
              MOVE OLT-B-INT-LC-DUE  TO D01-A15-NUM
              MOVE LTB-INT-LC-DUE    TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLT-B-OTH-DUE NOT NUMERIC OR
               (OLT-B-OTH-DUE = 2020202.02 OR 0202020.20)
              MOVE 0               TO OLT-B-OTH-DUE.
           IF LTB-OTH-DUE NOT = OLT-B-OTH-DUE
              MOVE "LTB-OTH-DUE" TO D01-TEXT
              MOVE BR-NO         TO D01-BRNO
              MOVE LTB-ACCTNO    TO D01-ACCTNO
              MOVE OLT-B-OTH-DUE TO D01-A15-NUM
              MOVE LTB-OTH-DUE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLT-B-TDUEAMT NOT NUMERIC OR
               (OLT-B-TDUEAMT = 2020202.02 OR 0202020.20)
              MOVE 0               TO OLT-B-TDUEAMT.
           IF LTB-TDUEAMT NOT = OLT-B-TDUEAMT
              MOVE "LTB-TDUEAMT" TO D01-TEXT
              MOVE BR-NO         TO D01-BRNO
              MOVE LTB-ACCTNO    TO D01-ACCTNO
              MOVE OLT-B-TDUEAMT TO D01-A15-NUM
              MOVE LTB-TDUEAMT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

      ***********************************
       COMPARE-SEQ-2-3-4 SECTION.
      ***********************************

           COMPUTE SUB = OLT-SEQNO - 1.

           IF LTB-NAME(SUB) NOT = OLT-B-NAME
      *       CALL "C$JUSTIFY" USING OLT-B-NAME, "L"
              IF LTB-NAME(SUB) NOT = OLT-B-NAME
              MOVE SPACES TO D01-TEXT
              STRING "LTB-NAME", SUB INTO D01-TEXT
              MOVE BR-NO         TO D01-BRNO
              MOVE LTB-ACCTNO    TO D01-ACCTNO
              MOVE OLT-B-NAME    TO D01-A15
              MOVE LTB-NAME(SUB) TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LTB-ADR1(SUB) NOT = OLT-B-ADR1
      *       CALL "C$JUSTIFY" USING OLT-B-ADR1, "L"
              IF LTB-ADR1(SUB) NOT = OLT-B-ADR1
              MOVE SPACES TO D01-TEXT
              STRING "LTB-ADR1", SUB INTO D01-TEXT
              MOVE BR-NO         TO D01-BRNO
              MOVE LTB-ACCTNO    TO D01-ACCTNO
              MOVE OLT-B-ADR1    TO D01-A15
              MOVE LTB-ADR1(SUB) TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LTB-ADR2(SUB) NOT = OLT-B-ADR2
      *       CALL "C$JUSTIFY" USING OLT-B-ADR2, "L"
              IF LTB-ADR2(SUB) NOT = OLT-B-ADR2
                MOVE SPACES TO D01-TEXT
                STRING "LTB-ADR2", SUB INTO D01-TEXT
                MOVE BR-NO         TO D01-BRNO
                MOVE LTB-ACCTNO    TO D01-ACCTNO
                MOVE OLT-B-ADR2    TO D01-A15
                MOVE LTB-ADR2(SUB) TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LTB-CITY(SUB) NOT = OLT-B-CITY
      *       CALL "C$JUSTIFY" USING OLT-B-CITY, "L"
              IF LTB-CITY(SUB) NOT = OLT-B-CITY
              MOVE SPACES TO D01-TEXT
              STRING "LTB-CITY", SUB INTO D01-TEXT
              MOVE BR-NO         TO D01-BRNO
              MOVE LTB-ACCTNO    TO D01-ACCTNO
              MOVE OLT-B-CITY    TO D01-A15
              MOVE LTB-CITY(SUB) TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LTB-STATE(SUB) NOT = OLT-B-STATE
              MOVE SPACES TO D01-TEXT
              STRING "LTB-STATE", SUB INTO D01-TEXT
              MOVE BR-NO          TO D01-BRNO
              MOVE LTB-ACCTNO     TO D01-ACCTNO
              MOVE OLT-B-STATE    TO D01-A15
              MOVE LTB-STATE(SUB) TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LTB-ZIP(SUB) NOT = OLT-B-ZIP
      *       CALL "C$JUSTIFY" USING OLT-B-ZIP, "L"
              IF LTB-ZIP(SUB) NOT = OLT-B-ZIP
              MOVE SPACES TO D01-TEXT
              STRING "LTB-ZIP", SUB INTO D01-TEXT
              MOVE BR-NO         TO D01-BRNO
              MOVE LTB-ACCTNO    TO D01-ACCTNO
              MOVE OLT-B-ZIP     TO D01-A15
              MOVE LTB-ZIP(SUB)  TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLT-B-PHONE NOT NUMERIC
              MOVE 0 TO OLT-B-PHONE. 
           IF LTB-PHONE(SUB) NOT = OLT-B-PHONE
              MOVE SPACES TO D01-TEXT
              STRING "LTB-PHONE", SUB INTO D01-TEXT
              MOVE BR-NO          TO D01-BRNO
              MOVE LTB-ACCTNO     TO D01-ACCTNO
              MOVE OLT-B-PHONE    TO D01-A15-NUM
              MOVE LTB-PHONE(SUB) TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

      ******************************************************************
       WRITE-DETAIL-LINE-ERROR SECTION.
      ******************************************************************

           ADD 1    TO TOTAL-ERRORS-CTR.
           PERFORM WRITE-DETAIL-LINE.

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
           MOVE A15-RECORDS-PROCESSED                     TO R01-NUMERIC.
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
           MOVE EXT-CONAME-CONAME TO LTBFLE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LTBFLE-SYSDATE.
           DISPLAY LTBFLE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE LTBFLE-FORM-FIELDS TO WR-BUF.
           MOVE LTBFLE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE LTBFLE-HELP--FILE TO HELP-LINK-FILE.
           MOVE LTBFLE-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/LTBFLE_EVA.CPY".
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
       COPY "LIBLP/LPLTBGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

      * COPY "LIBLP/LPLTB1RN.CPY".
      * COPYMEMBER: LIBLP/LPLTB1RN
       LOAD-LTB1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LTB-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LTB-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LTB-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LTB-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LTB-ALL-DATA
              MOVE LTB-ALL-PATH             TO LTB-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LTB-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LTB-ALL-BASE
              MOVE FILPATH-MACHINE         TO LTB-ALL-MACHINE
              MOVE FILPATH-DATA            TO LTB-ALL-DATA
              MOVE LTB-ALL-PATH             TO LTB-PATH.

      *-----------------------------------------------------------------
       OPEN-LTB1-FILE.
           PERFORM LOAD-LTB1-FILE.
           PERFORM OPEN-IT.
           MOVE LTB-PATH-SQL TO E-FILE.

           IF LTB1-CONN-OPEN-SW = "N" 
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS LTB1_CONNECT
                        USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE 'Y' TO LTB1-CONN-OPEN-SW.

      *-----------------------------------------------------------------
       START-LTB1-FILE.   *> SSP  LTBFILE
           PERFORM START-IT.
           MOVE LTB-PATH-OWNBR  TO LTB-BRNO.
           MOVE LTB-PATH-SQL    TO E-FILE.
           MOVE LTB1-KEY        TO E-KEYX.

           IF ( LTB1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LTB1-FILE.

      *      SELECT *
      *      FROM DBO.LTBFILE
      *      WHERE LTBFILE.LTB_BRNO  = :QLTB1-WBEG-BRNO
      *        AND LTBFILE.LTB_ACCTNO BETWEEN @QLTB_WBEG_ACCTNO AND @QLTB_WEND_ACCTNO
      *      ORDER BY LTBFILE.LTB_BRNO,
      *               LTBFILE.LTB_ACCTNO,
      *               LTBFILE.LTB_SEQNO

           EXEC SQL SET CONNECTION LTB1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE LTB1_CURSOR CURSOR FOR
                :LTB1-RETURN-CODE =  
            EXEC SSP_LTBFLE_LTBFILE_SELECT
              ( :QLTB1-WBEG-BRNO,:QLTB1-WBEG-ACCTNO,:QLTB1-WEND-ACCTNO)
           END-EXEC.

           EXEC SQL
               OPEN LTB1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           MOVE STAT---GOOD TO LTB1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LTB1-FILE-NEXT.  *> SSP  LTBFILE
           PERFORM READ-IT.
           MOVE LTB-PATH-OWNBR  TO LTB-BRNO.
           MOVE LTB-PATH-SQL    TO E-FILE.
           MOVE LTB1-KEY        TO E-KEYX.
           INITIALIZE QLTB-REC.

           EXEC SQL SET CONNECTION LTB1_CONNECT END-EXEC.

           IF ( LTB1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LTB1_CURSOR INTO :QLTB-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTB1-FILE-NEXT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           IF ( IO-FG = 0 )
              PERFORM GET-LTB-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LTB1-FILE.

           PERFORM CLOSE-IT.

           IF ( LTB1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION LTB1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE LTB1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO LTB1-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-LTB1-CONNECTION.

           IF LTB1-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION LTB1_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT LTB1_CONNECT END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE "N" TO LTB1-CONN-OPEN-SW.
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
              DESTROY LTBFLE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
