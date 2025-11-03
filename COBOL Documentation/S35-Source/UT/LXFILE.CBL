       IDENTIFICATION DIVISION.
       PROGRAM-ID.    LXFILE.
       DATE-WRITTEN.  10-24-2023.
      ******************************************************************
      *
      * DESCRIPTION   : COMPARE S35 LXFILE WITH A15 LXFILE
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
      * 02-28-2025 MB  - PROGRAM OVER LAYING ACCOUNT NUMBER ON REPORT.
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

      * COPYMEMBER: LIBLP/LPFSLX
           SELECT OLX-FILE ASSIGN TO OLX-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OLX1-KEY
                  FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  OLX-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A15SRC/LP01LX.CPY" REPLACING LEADING "LX" BY "OLX".

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/LXFILE S35 02/28/25-13:25:43 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LX.CPY".
       COPY "LIBLP/LP01LX_SQL.CPY".
       COPY "LIBLP/LPWSLX.CPY".
       COPY "LIBLP/LP01LXG.CPY".
       COPY "LIBLP/LP01LXG_SQL.CPY".
       COPY "LIBLP/LPWSLXG.CPY".
       COPY "LIBLP/LP01LXE.CPY".
       COPY "LIBLP/LP01LXE_SQL.CPY".
       COPY "LIBLP/LPWSLXE.CPY".

       01  OLX-PATH                    PIC X(35) VALUE SPACES.
       01  OLX-ALL-PATH.
           03  OLX-ALL-BASE     PIC X(9).
           03  FILLER           PIC X     VALUE "/".
           03  OLX-ALL-MACHINE  PIC XX.
           03  FILLER           PIC X     VALUE "/".
           03  OLX-ALL-DATA     PIC X(6).
           03  FILLER           PIC X(10) VALUE
               "/GB/LXFILE".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  WS-TMP-BRNO                 PIC 9(4).
       01  WS-TMP-ACCTNO               PIC 9(8).
       01  WS-TMP-SEQNO                PIC 9(5).
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

       01  USE-LX-KEY.
           03  USE-LX-BRNO             PIC 9(4).
           03  USE-LX-ACCTNO           PIC 9(8).
           03  USE-LX-SEQNO            PIC 9(5).

       01  USE-OLX-KEY.
           03  USE-OLX-BRNO            PIC 9(4).
           03  USE-OLX-ACCTNO          PIC 9(8).
           03  USE-OLX-SEQNO           PIC 9(5).

       01  PREV-OLX-KEY.
           03  PREV-OLX-BRNO           PIC 9(4)   VALUE 0.
           03  PREV-OLX-ACCTNO         PIC 9(8)   VALUE 0.
           03  PREV-OLX-SEQNO          PIC 9(5)   VALUE 0.

       01  LX1-CONN-OPEN-SW            PIC X      VALUE "N".
       01  LX-EOF-SW                   PIC X      VALUE "N".
       01  OLX-EOF-SW                  PIC X      VALUE "N".
       01  A15-FILE-FOUND-SW           PIC X      VALUE "N".
       01  TOTAL-ERRORS-CTR            PIC 9(9)   VALUE 0.
       01  SQL-RECORDS-PROCESSED       PIC 9(9)   VALUE 0.
       01  A15-RECORDS-PROCESSED       PIC 9(9)   VALUE 0.
           88  NO-RECORDS-FOUND                   VALUE ZEROES.

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

       01  LN-COUNT                    PIC 9(03)     VALUE 99.
       01  LN-FEED                     PIC 9(02).
       01  PAGE-NUMBER                 PIC 9(04)     VALUE ZEROES.

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
           "PAYMENT TRAILER (LXFILE) COMPARE A15 TO S35".

       01  HEADER-LINE-03.
           03  FILLER                PIC X(4)  VALUE "BRNO".
           03  FILLER                PIC X(3)  VALUE SPACES.
           03  FILLER                PIC X(8)  VALUE " ACCTNO ".
           03  FILLER                PIC X(3)  VALUE SPACES.
           03  FILLER                PIC X(3)  VALUE "SEQ".
           03  FILLER                PIC X(3)  VALUE SPACES.
           03  FILLER                PIC X(25) VALUE "DESCRIPTION".
           03  FILLER                PIC X(22) VALUE "A15".
           03  FILLER                PIC X(3)  VALUE "S35".

      ******************************************************************
      *
      *    D E T A I L - L I N E - W O R K E R S
      *
      ******************************************************************
       01  DETAIL-LINE                        PIC X(100)   VALUE SPACES.

       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-BRNO                       PIC ZZZ9.
           03  FILLER                         PIC X(3).
           03  D01-ACCTNO                     PIC Z(7)9.
           03  FILLER                         PIC X(3).
           03  D01-SEQNO                      PIC ZZ9.
           03  FILLER                         PIC X(3).
           03  D01-TEXT                       PIC X(23).
           03  FILLER                         PIC X(2).
           03  D01-A15                        PIC X(20).
           03  FILLER REDEFINES D01-A15.
               05  D01-A15-NUM                PIC Z(7)9.9(3)-.
               05  FILLER                     PIC X(7).
           03  FILLER                         PIC X(2).
           03  D01-S35                        PIC X(20).
           03  FILLER REDEFINES D01-S35.
               05  D01-S35-NUM                PIC Z(7)9.9(3)-.
               05  FILLER                     PIC X(7).

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
       COPY "LIBUT/LXFILE_DEF.CPY".
       COPY "LIBUT/LXFILE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/LXFILE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE OLX-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LX1-FILE.
      *     PERFORM CLOSE-LXG1-FILE.
      *     PERFORM CLOSE-LXE1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           PERFORM CLOSE-LX1-CONNECTION.
           CLOSE OLX-FILE.
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
           MOVE "LXFILE" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE LXFILE-QUEUE-POS TO Q-POS.
           MOVE LXFILE-COPIES-POS TO C-POS.
           MOVE LXFILE-PRU-POS   TO P-POS.
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

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO LX-PATH-OVERRIDE.

           MOVE "N"   TO LX-EOF-SW OLX-EOF-SW.
           MOVE "Y"   TO A15-FILE-FOUND-SW.
           MOVE ZEROS TO PREV-OLX-KEY.

      D    STOP MESS.

           MOVE BR-NO      TO QLX1-WBEG-BRNO.
           MOVE BEG-ACCTNO TO QLX1-WBEG-ACCTNO.
           MOVE END-ACCTNO TO QLX1-WEND-ACCTNO.

           PERFORM OPEN-LX1-FILE.
           PERFORM START-LX1-FILE.  *> SSP  LXFILE LXEFILE LXGFILE
           PERFORM M-READ-LX.

           MOVE SOURCE-PATH  TO OLX-ALL-BASE.
           MOVE MACHINE-R1   TO OLX-ALL-MACHINE.
           MOVE BR-DATA-PATH TO OLX-ALL-DATA.
           MOVE OLX-ALL-PATH TO OLX-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE MACHINE-R2 TO OLX-ALL-MACHINE
              MOVE OLX-ALL-PATH TO OLX-PATH ACCESS-BUF
              PERFORM ACCESS-CALL
              IF ( STAT-BAD )
                MOVE "MISSING A15 FILE" TO D01-TEXT 
                MOVE OLX-ALL-PATH       TO D01-A15
                MOVE SPACES             TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR
                MOVE "N" TO A15-FILE-FOUND-SW.

           IF A15-FILE-FOUND-SW = "N"
              MOVE ALL '9' TO USE-OLX-KEY
              MOVE "Y"     TO OLX-EOF-SW
           ELSE
              MOVE BEG-ACCTNO  TO WS-NUMBER
              MOVE 1           TO WS-SEQNO
              MOVE WS-ACCTNO   TO OLX-ACCTNO
              MOVE 1           TO OLX-SEQNO
              MOVE SPACES      TO OLX-CODE
              PERFORM OPEN-OLX1-FILE
              PERFORM START-OLX1-FILE
              PERFORM M-READ-OLX.

           PERFORM PROCESS-BRANCH-NEXT-LX
              UNTIL LX-EOF-SW = "Y" AND OLX-EOF-SW = "Y".

           PERFORM CLOSE-OLX1-FILE.
           PERFORM CLOSE-LX1-FILE.

           GO TO PROCESS-BRANCH-DRIVER-END.

       PROCESS-BRANCH-NEXT-LX.
           PERFORM SEND-PROGRESS-INDICATOR.

           PERFORM COMPARE-FIELDS.
           IF LX-EOF-SW NOT = "Y" OR OLX-EOF-SW NOT = "Y"
              IF USE-OLX-KEY < USE-LX-KEY
                 PERFORM M-READ-OLX 
              ELSE
                 IF USE-OLX-KEY > USE-LX-KEY
                    PERFORM M-READ-LX
                 ELSE
      * A15 HAS MULT RECORDS TO S35 - CAN'T READ S35 IF KEY ARE EQUAL
                    PERFORM M-READ-OLX.
      *           PERFORM M-READ-LX.

       PROCESS-BRANCH-DRIVER-END.
           EXIT.

       M-READ-LX SECTION.
           PERFORM READ-LX1-FILE-NEXT.  *> SSP  LXFILE LXEFILE LXGFILE
           IF IO-BAD
              MOVE ALL '9' TO USE-LX-KEY
              MOVE "Y"     TO LX-EOF-SW
           ELSE
              ADD 1              TO SQL-RECORDS-PROCESSED
              MOVE BR-NO         TO USE-LX-BRNO
              MOVE WS-TMP-ACCTNO TO USE-LX-ACCTNO
              MOVE WS-TMP-SEQNO  TO USE-LX-SEQNO.

       EXIT-M-READ-LX1.
           EXIT.

       M-READ-OLX SECTION.

           MOVE USE-OLX-BRNO   TO PREV-OLX-BRNO.
           MOVE USE-OLX-ACCTNO TO PREV-OLX-ACCTNO.
           MOVE USE-OLX-SEQNO  TO PREV-OLX-SEQNO.

           PERFORM READ-OLX1-FILE-NEXT.
           IF IO-BAD
              MOVE ALL '9'  TO USE-OLX-KEY
              MOVE "Y"      TO OLX-EOF-SW
           ELSE
              MOVE OLX-ACCTNO TO WS-ACCTNO
              IF ( WS-NUMBER < BEG-ACCTNO ) OR
                 ( WS-NUMBER > END-ACCTNO )
                 GO TO M-READ-OLX
              ELSE
                 ADD 1           TO A15-RECORDS-PROCESSED
                 MOVE BR-NO      TO USE-OLX-BRNO
                 MOVE WS-NUMBER  TO USE-OLX-ACCTNO
                 MOVE OLX-SEQNO  TO USE-OLX-SEQNO.

       EXIT-M-READ-OLX.
           EXIT.

      ******************************************************************
       COMPARE-FIELDS SECTION.
      ******************************************************************

      * A15 HAS MULT RECORDS TO S35 - NEED TO READ LX IF LX KEY LESS AND MATCHES PREV 
           IF USE-OLX-KEY > USE-LX-KEY AND USE-LX-KEY = PREV-OLX-KEY 
              PERFORM M-READ-LX
              IF LX-EOF-SW = "Y" AND OLX-EOF-SW = "Y"
                 GO TO EXIT-COMPARE-FIELDS.

      *    NO NEED TO REPORT ALL SEQUENCES FROM A15
           IF USE-OLX-KEY < USE-LX-KEY
              IF USE-OLX-KEY NOT = PREV-OLX-KEY 
                 MOVE "MISSING S35 RECORD" TO D01-TEXT
                 MOVE BR-NO                TO D01-BRNO
                 MOVE USE-OLX-ACCTNO       TO D01-ACCTNO
                 MOVE USE-OLX-SEQNO        TO D01-SEQNO
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              GO TO EXIT-COMPARE-FIELDS.

           IF USE-OLX-KEY > USE-LX-KEY
              MOVE "MISSING A15 RECORD" TO D01-TEXT
              MOVE BR-NO                TO D01-BRNO
              MOVE USE-LX-ACCTNO        TO D01-ACCTNO
              MOVE USE-LX-SEQNO         TO D01-SEQNO
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.

           IF OLX-CODE = "G"
              PERFORM COMPARE-FIELDS-LXG
           ELSE
           IF OLX-CODE = "E"
              PERFORM COMPARE-FIELDS-LXE
           ELSE
      * ADDON
           IF OLX-CODE = "A" OR "2"
              PERFORM COMPARE-LXA
           ELSE
           IF OLX-CODE = "M"
              PERFORM COMPARE-LXM
           ELSE
           IF OLX-CODE = "Z"
              PERFORM COMPARE-LXZ
           ELSE
           IF OLX-CODE = "P"
              PERFORM COMPARE-LXP
           ELSE
           IF OLX-CODE = "T"
              PERFORM COMPARE-LXT
           ELSE
           IF OLX-CODE = "R"
              PERFORM COMPARE-LXR
           ELSE
           IF OLX-CODE = "X"
              PERFORM COMPARE-LXX.

           GO TO EXIT-COMPARE-FIELDS.

       EXIT-COMPARE-FIELDS.
           EXIT.
       
      ******************************************************************
       COMPARE-FIELDS-LXG SECTION.
      ******************************************************************

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
              IF (OLXG-GLNO(SUB) NOT NUMERIC)
                 MOVE 0  TO OLXG-GLNO(SUB)
              END-IF
              IF OLXG-GLNO(SUB) NOT = LXG-GLNO(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "LXG-GLNO"SUB INTO D01-TEXT
                 PERFORM WRITE-DETAIL-LINE-KEYS
                 MOVE OLXG-GLNO(SUB) TO D01-A15-NUM
                 MOVE LXG-GLNO(SUB)  TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF (OLXG-GLAMT(SUB) NOT NUMERIC)
                 MOVE 0  TO OLXG-GLAMT(SUB)
              END-IF
              IF OLXG-GLAMT(SUB) NOT = LXG-GLAMT(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "LXG-GLAMT"SUB INTO D01-TEXT
                 PERFORM WRITE-DETAIL-LINE-KEYS
                 MOVE OLXG-GLAMT(SUB) TO D01-A15-NUM
                 MOVE LXG-GLAMT(SUB)  TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.

       EXIT-COMPARE-LXG.
           EXIT.

      ******************************************************************
       COMPARE-FIELDS-LXE SECTION.
      ******************************************************************

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
              IF (OLXE-EARN(SUB) NOT NUMERIC)
                 MOVE 0  TO OLXE-EARN(SUB)
              END-IF
              IF OLXE-EARN(SUB) NOT = LXE-EARN(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "LXE-EARN"SUB INTO D01-TEXT
                 PERFORM WRITE-DETAIL-LINE-KEYS
                 MOVE OLXE-EARN(SUB) TO D01-A15-NUM
                 MOVE LXE-EARN(SUB)  TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.

      *     IF (OLXE-WORKER NOT NUMERIC)
      *        MOVE 0  TO OLXE-WORKER.
           IF OLXE-WORKER NOT = LXE-WORKER
              MOVE SPACES TO D01-TEXT
              STRING "LXE-WORKER"SUB INTO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-KEYS
              MOVE OLXE-WORKER  TO D01-A15-NUM
              MOVE LXE-WORKER   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

      *     IF (OLXE-WORKER2 NOT NUMERIC)
      *        MOVE 0  TO OLXE-WORKER2.
           IF OLXE-WORKER2 NOT = LXE-WORKER2
              MOVE SPACES TO D01-TEXT
              STRING "LXE-WORKER2"SUB INTO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-KEYS
              MOVE OLXE-WORKER2  TO D01-A15-NUM
              MOVE LXE-WORKER2   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

       EXIT-COMPARE-LXE.
           EXIT.

      ******************************************************************
       COMPARE-LXA SECTION.
      ******************************************************************
            
           IF OLX-CODE = "2"
              PERFORM COMPARE-LX2
              GO TO EXIT-COMPARE-LXA.

           IF OLXA-SRCD NOT = LXA-SRCD
              MOVE "LXA-SRCD" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-KEYS
              MOVE OLXA-SRCD  TO D01-A15
              MOVE LXA-SRCD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

      * A15 OCCURS 3 TIMES IN THE "A" RECORD
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
             IF (OLXA-START-PMT(SUB) NOT NUMERIC)
                MOVE 0  TO OLXA-START-PMT(SUB)
             END-IF
             IF OLXA-START-PMT(SUB) NOT = LXA-START-PMT(SUB)
                MOVE SPACES TO D01-TEXT
                STRING "LXA-START-PMT"SUB INTO D01-TEXT
                PERFORM WRITE-DETAIL-LINE-KEYS
                MOVE OLXA-START-PMT(SUB)  TO D01-A15-NUM
                MOVE LXA-START-PMT(SUB)   TO D01-S35-NUM
                PERFORM WRITE-DETAIL-LINE-ERROR
             END-IF
             IF (OLXA-END-PMT(SUB) NOT NUMERIC)
                MOVE 0  TO OLXA-END-PMT(SUB)
             END-IF
             IF OLXA-END-PMT(SUB) NOT = LXA-END-PMT(SUB)
                MOVE SPACES TO D01-TEXT
                STRING "LXA-END-PMT"SUB INTO D01-TEXT
                PERFORM WRITE-DETAIL-LINE-KEYS
                MOVE OLXA-END-PMT(SUB)  TO D01-A15-NUM
                MOVE LXA-END-PMT(SUB)   TO D01-S35-NUM
                PERFORM WRITE-DETAIL-LINE-ERROR
             END-IF
      *       IF (OLXA-NET-CHANGE(SUB) NOT NUMERIC)
      *          MOVE 0  TO OLXA-NET-CHANGE(SUB)
      *       END-IF
             IF OLXA-NET-CHANGE(SUB) NOT = LXA-NET-CHANGE(SUB)
                MOVE SPACES TO D01-TEXT
                STRING "LXA-NET-CHANGE"SUB INTO D01-TEXT
                PERFORM WRITE-DETAIL-LINE-KEYS
                MOVE OLXA-NET-CHANGE(SUB) TO D01-A15-NUM
                MOVE LXA-NET-CHANGE(SUB)  TO D01-S35-NUM
                PERFORM WRITE-DETAIL-LINE-ERROR
             END-IF
           END-PERFORM.

      *     IF (OLXA-LN-INSCOMP NOT NUMERIC)
      *        MOVE 0  TO OLXA-LN-INSCOMP.
           IF OLXA-LN-INSCOMP NOT = LXA-LN-INSCOMP
              MOVE "LXA-LN-INSCOMP" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-KEYS
              MOVE OLXA-LN-INSCOMP  TO D01-A15-NUM
              MOVE LXA-LN-INSCOMP   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLXA-LN-INSOURS NOT = LXA-LN-INSOURS
              MOVE "LXA-LN-INSOURS" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-KEYS
              MOVE OLXA-LN-INSOURS  TO D01-A15
              MOVE LXA-LN-INSOURS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLXA-LN-INSEFF TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LXA-LN-INSEFF-DATE
              MOVE "LXA-LN-INSEFF-DATE" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-KEYS
              MOVE DATE-YYYYMMDD        TO D01-A15
              MOVE LXA-LN-INSEFF-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLXA-LN-INSEXP TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LXA-LN-INSEXP-DATE
              MOVE "LXA-LN-INSEXP-DATE" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-KEYS
              MOVE DATE-YYYYMMDD        TO D01-A15
              MOVE LXA-LN-INSEXP-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

      *     IF (OLXA-LN-INSPREM NOT NUMERIC)
      *        MOVE 0  TO OLXA-LN-INSPREM.
           IF OLXA-LN-INSPREM NOT = LXA-LN-INSPREM
              MOVE "LXA-LN-INSPREM" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-KEYS
              MOVE OLXA-LN-INSPREM  TO D01-A15-NUM
              MOVE LXA-LN-INSPREM   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

      *     IF (OLXA-LN-INSCOVR NOT NUMERIC)
      *        MOVE 0  TO OLXA-LN-INSCOVR.
           IF OLXA-LN-INSCOVR NOT = LXA-LN-INSCOVR
              MOVE "LXA-LN-INSCOVR" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-KEYS
              MOVE OLXA-LN-INSCOVR  TO D01-A15-NUM
              MOVE LXA-LN-INSCOVR   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

      *     IF (OLXA-LN-INSCOMM NOT NUMERIC)
      *        MOVE 0  TO OLXA-LN-INSCOMM.
           IF OLXA-LN-INSCOMM NOT = LXA-LN-INSCOMM
              MOVE "LXA-LN-INSCOMM" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-KEYS
              MOVE OLXA-LN-INSCOMM  TO D01-A15-NUM
              MOVE LXA-LN-INSCOMM   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLXA-LN-INS-REBATE NOT = LXA-LN-INS-REBATE
              MOVE "LXA-LN-INS-REBATE" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-KEYS
              MOVE OLXA-LN-INS-REBATE  TO D01-A15
              MOVE LXA-LN-INS-REBATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLXA-LN-INSCODES NOT = LXA-LN-INSCODES
              MOVE "LXA-LN-INSCODES"  TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-KEYS
              MOVE OLXA-LN-INSCODES   TO D01-A15
              MOVE LXA-LN-INSCODES    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

       EXIT-COMPARE-LXA.
           EXIT.

      ******************************************************************
       COMPARE-LX2 SECTION.
      ******************************************************************

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
             IF (OLX2-START-PMT(SUB) NOT NUMERIC)
                MOVE 0  TO OLX2-START-PMT(SUB)
             END-IF
             IF OLX2-START-PMT(SUB) NOT = LXA-START-PMT(SUB + 3)
                MOVE SPACES TO D01-TEXT
                STRING "LX2-START-PMT"SUB INTO D01-TEXT
                PERFORM WRITE-DETAIL-LINE-KEYS
                MOVE OLX2-START-PMT(SUB)    TO D01-A15-NUM
                MOVE LXA-START-PMT(SUB + 3) TO D01-S35-NUM
                PERFORM WRITE-DETAIL-LINE-ERROR
             END-IF
             IF (OLX2-END-PMT(SUB) NOT NUMERIC)
                MOVE 0  TO OLX2-END-PMT(SUB)
             END-IF
             IF OLX2-END-PMT(SUB) NOT = LXA-END-PMT(SUB + 3)
                MOVE SPACES TO D01-TEXT
                STRING "LX2-END-PMT"SUB INTO D01-TEXT
                PERFORM WRITE-DETAIL-LINE-KEYS
                MOVE OLX2-END-PMT(SUB)    TO D01-A15-NUM
                MOVE LXA-END-PMT(SUB + 3) TO D01-S35-NUM
                PERFORM WRITE-DETAIL-LINE-ERROR
             END-IF
      *       IF (OLX2-NET-CHANGE(SUB) NOT NUMERIC)
      *          MOVE 0  TO OLX2-NET-CHANGE(SUB)
      *       END-IF
             IF OLX2-NET-CHANGE(SUB) NOT = LXA-NET-CHANGE(SUB + 3)
                MOVE SPACES TO D01-TEXT
                STRING "LX2-NET-CHANGE"SUB INTO D01-TEXT
                PERFORM WRITE-DETAIL-LINE-KEYS
                MOVE OLX2-NET-CHANGE(SUB)    TO D01-A15-NUM
                MOVE LXA-NET-CHANGE(SUB + 3) TO D01-S35-NUM
                PERFORM WRITE-DETAIL-LINE-ERROR
             END-IF
           END-PERFORM.

      ******************************************************************
       COMPARE-LXM SECTION.
      ******************************************************************
           IF (OLXM-MIP-CLPAID NOT NUMERIC)
              MOVE 0  TO OLXM-MIP-CLPAID.
           IF OLXM-MIP-CLPAID NOT = LXM-MIP-CLPAID
              MOVE "LXM-MIP-CLPAID" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-KEYS
              MOVE OLXM-MIP-CLPAID  TO D01-A15-NUM
              MOVE LXM-MIP-CLPAID   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLXM-MIP-CLPDTH TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LXM-MIP-CLPDTH-DATE
              MOVE "LXM-MIP-CLPDTH-DATE" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-KEYS
              MOVE DATE-YYYYMMDD       TO D01-A15
              MOVE LXM-MIP-CLPDTH-DATE TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF (OLXM-MIP-AHPAID NOT NUMERIC)
              MOVE 0  TO OLXM-MIP-AHPAID.
           IF OLXM-MIP-AHPAID NOT = LXM-MIP-AHPAID
              MOVE "LXM-MIP-AHPAID"  TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-KEYS
              MOVE OLXM-MIP-AHPAID   TO D01-A15-NUM
              MOVE LXM-MIP-AHPAID    TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLXM-MIP-AHPDTH TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LXM-MIP-AHPDTH-DATE
              MOVE "LXM-MIP-AHPDTH-DATE-DATE" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-KEYS
              MOVE DATE-YYYYMMDD         TO D01-A15
              MOVE LXM-MIP-AHPDTH-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

      ******************************************************************
       COMPARE-LXZ SECTION.
      ******************************************************************
      *     IF (OLXZ-POST-REGPY NOT NUMERIC)
      *        MOVE 0  TO OLXZ-POST-REGPY.
           IF OLXZ-POST-REGPY NOT = LXZ-POST-REGPY
              MOVE "LXZ-POST-REGPY"  TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-KEYS
              MOVE OLXZ-POST-REGPY   TO D01-A15-NUM
              MOVE LXZ-POST-REGPY    TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

      *     IF (OLXZ-POST-EXTRA-PRIN NOT NUMERIC)
      *        MOVE 0  TO OLXZ-POST-EXTRA-PRIN.
           IF OLXZ-POST-EXTRA-PRIN NOT = LXZ-POST-EXTRA-PRIN
              MOVE "LXZ-POST-EXTRA-PRIN" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-KEYS
              MOVE OLXZ-POST-EXTRA-PRIN  TO D01-A15-NUM
              MOVE LXZ-POST-EXTRA-PRIN   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

      *     IF (OLXZ-POST-OTH NOT NUMERIC)
      *        MOVE 0  TO OLXZ-POST-OTH.
           IF OLXZ-POST-OTH NOT = LXZ-POST-OTH
              MOVE "LXZ-POST-OTH" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-KEYS
              MOVE OLXZ-POST-OTH  TO D01-A15-NUM
              MOVE LXZ-POST-OTH   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

      *     IF (OLXZ-POST-LCHG NOT NUMERIC)
      *        MOVE 0  TO OLXZ-POST-LCHG.
           IF OLXZ-POST-LCHG NOT = LXZ-POST-LCHG
              MOVE "LXZ-POST-LCHG" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-KEYS
              MOVE OLXZ-POST-LCHG  TO D01-A15-NUM
              MOVE LXZ-POST-LCHG   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

      *     IF (OLXZ-PDUE-APINT NOT NUMERIC)
      *        MOVE 0  TO OLXZ-PDUE-APINT.
           IF OLXZ-PDUE-APINT NOT = LXZ-PDUE-APINT
              MOVE "LXZ-PDUE-APINT" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-KEYS
              MOVE OLXZ-PDUE-APINT  TO D01-A15-NUM
              MOVE LXZ-PDUE-APINT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

      *     IF (OLXZ-PDUE-APPRIN NOT NUMERIC)
      *        MOVE 0  TO OLXZ-PDUE-APPRIN.
           IF OLXZ-PDUE-APPRIN NOT = LXZ-PDUE-APPRIN
              MOVE "LXZ-PDUE-APPRIN" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-KEYS
              MOVE OLXZ-PDUE-APPRIN  TO D01-A15-NUM
              MOVE LXZ-PDUE-APPRIN   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

      *     IF (OLXZ-CUR-APINT NOT NUMERIC)
      *        MOVE 0  TO OLXZ-CUR-APINT.
           IF OLXZ-CUR-APINT NOT = LXZ-CUR-APINT
              MOVE "LXZ-CUR-APINT" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-KEYS
              MOVE OLXZ-CUR-APINT  TO D01-A15-NUM
              MOVE LXZ-CUR-APINT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

      *     IF (OLXZ-CUR-APPRIN NOT NUMERIC)
      *        MOVE 0  TO OLXZ-CUR-APPRIN.
           IF OLXZ-CUR-APPRIN NOT = LXZ-CUR-APPRIN
              MOVE "LXZ-CUR-APPRIN" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-KEYS
              MOVE OLXZ-CUR-APPRIN  TO D01-A15-NUM
              MOVE LXZ-CUR-APPRIN   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

      ******************************************************************
       COMPARE-LXP SECTION.
      ******************************************************************

      *LXP-EARN CHANGED FROM OCCURS 12 TO OCCURS 20 BUT KM DIDNT WANT
      *CHANGED IN A30, WORLD DOESNT USE SO CORRECTING IN A32. 7-10 AND
      *17 -20 SHOULD BE 0, 7 SHOULD MOVE TO 11, 8 TO 12, ETC

      *  T   1 - 10  ANTIC          P   1 - 10 ACCRERN
      *  T  11 - 20 ANTIC-ADJ       P  11 - 20 UFERN(UPFRONT)

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 20
              IF SUB < 7
                 IF OLXT-EARN(SUB) NOT = LXP-EARN(SUB)
                    MOVE SPACES TO D01-TEXT
                    STRING "LXP-EARN"SUB INTO D01-TEXT
                    PERFORM WRITE-DETAIL-LINE-KEYS
                    MOVE OLXT-EARN(SUB) TO D01-A15-NUM
                    MOVE LXP-EARN(SUB)  TO D01-S35-NUM
                    PERFORM WRITE-DETAIL-LINE-ERROR
                 END-IF
              ELSE
                 IF SUB < 13
                    IF OLXT-EARN(SUB) NOT = LXP-EARN(SUB + 4)
                       MOVE SPACES TO D01-TEXT
                       STRING "OLXT-EARN"SUB INTO D01-TEXT
                       PERFORM WRITE-DETAIL-LINE-KEYS
                       MOVE OLXT-EARN(SUB) TO D01-A15-NUM
                       MOVE LXP-EARN(SUB + 4)   TO D01-S35-NUM
                       PERFORM WRITE-DETAIL-LINE-ERROR
                    END-IF
                 END-IF
              END-IF 
           END-PERFORM.

      ******************************************************************
       COMPARE-LXT SECTION.
      ******************************************************************

      * A15 WE HAD LXT-EARN ONLY FOR BOTH "P" AND "T". OCCURS 12
      *
      * A30 WE HAD LXT-EARN ONLY FOR BOTH "P" AND "T". OCCURS 20

      *  T   1 - 10  ANTIC          P   1 - 10 ACCRERN
      *  T  11 - 20 ANTIC-ADJ       P  11 - 20 UFERN(UPFRONT)

      *LXT-EARN CHANGED FROM OCCURS 12 TO OCCURS 20 BUT KM DIDNT WANT
      *CHANGED IN A30, WORLD DOESNT USE SO CORRECTING IN A32. 7-10 AND
      *17 -20 SHOULD BE 0, 7 SHOULD MOVE TO 11, 8 TO 12, ETC
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 20
              IF SUB < 7
                 IF OLXT-EARN(SUB) NOT = LXT-EARN(SUB)
                    MOVE SPACES TO D01-TEXT
                    STRING "LXT-EARN"SUB INTO D01-TEXT
                    PERFORM WRITE-DETAIL-LINE-KEYS
                    MOVE OLXT-EARN(SUB) TO D01-A15-NUM
                    MOVE LXT-EARN(SUB)  TO D01-S35-NUM
                    PERFORM WRITE-DETAIL-LINE-ERROR
                 END-IF
              ELSE
                 IF SUB < 13
                    IF OLXT-EARN(SUB) NOT = LXT-EARN(SUB + 4)
                       MOVE SPACES TO D01-TEXT
                       STRING "OLXT-EARN"SUB INTO D01-TEXT
                       PERFORM WRITE-DETAIL-LINE-KEYS
                       MOVE OLXT-EARN(SUB)    TO D01-A15-NUM
                       MOVE LXP-EARN(SUB + 4) TO D01-S35-NUM
                       PERFORM WRITE-DETAIL-LINE-ERROR
                    END-IF
                 END-IF
              END-IF
           END-PERFORM.

      ******************************************************************
       COMPARE-LXR SECTION.
      ******************************************************************

           IF (OLXR-REB-AMT NOT NUMERIC)
              MOVE 0  TO OLXR-REB-AMT.
           IF OLXR-REB-AMT NOT = LXR-REB-AMT
              MOVE "LXR-REB-AMT" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-KEYS
              MOVE OLXR-REB-AMT  TO D01-A15-NUM
              MOVE LXR-REB-AMT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

      ******************************************************************
       COMPARE-LXX SECTION.
      ******************************************************************

           IF OLXX-INS-TAX-COUNTY-CD NOT = LXX-INS-TAX-COUNTY-CD
              MOVE "LXX-INS-TAX-COUNTY-CD"  TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-KEYS
              MOVE OLXX-INS-TAX-COUNTY-CD   TO D01-A15
              MOVE LXX-INS-TAX-COUNTY-CD    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLXX-INS-TAX-CITY-CD NOT = LXX-INS-TAX-CITY-CD
              MOVE "LXX-INS-TAX-CITY-CD" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-KEYS
              MOVE OLXX-INS-TAX-CITY-CD  TO D01-A15
              MOVE LXX-INS-TAX-CITY-CD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.


      ******************************************************************
       WRITE-DETAIL-LINE-KEYS SECTION.
      ******************************************************************
           MOVE BR-NO             TO D01-BRNO.
           MOVE USE-LX-ACCTNO     TO D01-ACCTNO.
           MOVE USE-LX-SEQNO      TO D01-SEQNO.

       WRITE-DETAIL-LINE-KEYS-EXIT.
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
           MOVE EXT-CONAME-CONAME TO LXFILE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LXFILE-SYSDATE.
           DISPLAY LXFILE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE LXFILE-FORM-FIELDS TO WR-BUF.
           MOVE LXFILE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE LXFILE-HELP--FILE TO HELP-LINK-FILE.
           MOVE LXFILE-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/LXFILE_EVA.CPY".
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
       COPY "LIBLP/LPLXEGS_SQL.CPY".
       COPY "LIBLP/LPLXGGS_SQL.CPY".
       COPY "LIBLP/LPLXGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

      * COPY "LIBLP/LPLXE1RN.CPY".
      * COPY "LIBLP/LPLXG1RN.CPY".

      * COPY "LIBLP/LPLX1RN.CPY".
      * COPYMEMBER: LIBLP/LPLX1RN
       LOAD-LX1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LX-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LX-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LX-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LX-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LX-ALL-DATA
              MOVE LX-ALL-PATH             TO LX-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LX-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LX-ALL-BASE
              MOVE FILPATH-MACHINE         TO LX-ALL-MACHINE
              MOVE FILPATH-DATA            TO LX-ALL-DATA
              MOVE LX-ALL-PATH             TO LX-PATH.

      *-----------------------------------------------------------------
       OPEN-LX1-FILE.
           PERFORM LOAD-LX1-FILE.
           PERFORM OPEN-IT.
           MOVE LX-PATH-SQL TO E-FILE.

           IF LX1-CONN-OPEN-SW = "N" 
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS LX1_CONNECT
                        USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE 'Y' TO LX1-CONN-OPEN-SW.

      *-----------------------------------------------------------------
       START-LX1-FILE.   *> SSP  LXFILE LXEFILE LXGFILE
           PERFORM START-IT.
           MOVE LX-PATH-OWNBR  TO LX-BRNO.
           MOVE LX-PATH-SQL    TO E-FILE.
           MOVE LX1-KEY        TO E-KEYX.

           IF ( LX1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LX1-FILE.

      *     SELECT TMP.BRNO,
      *            TMP.ACCTNO,
      *            TMP.SEQNO,
      *            ISNULL(LXFILE.LX_BRNO,0),
      *            LXFILE.LX_ACCTNO,
      *            LXFILE.LX_SEQNO,
      *            CAST(LXFILE.LX_LTOUCH_DATE AS VARCHAR(10)),
      *            LXFILE.LXT_CODE,
      *            LXFILE.LXT_EARN_1,
      *            LXFILE.LXT_EARN_2,
      *            LXFILE.LXT_EARN_3,
      *            LXFILE.LXT_EARN_4,
      *            LXFILE.LXT_EARN_5,
      *            LXFILE.LXT_EARN_6,
      *            LXFILE.LXT_EARN_7,
      *            LXFILE.LXT_EARN_8,
      *            LXFILE.LXT_EARN_9,
      *            LXFILE.LXT_EARN_10,
      *            LXFILE.LXT_EARN_11,
      *            LXFILE.LXT_EARN_12,
      *            LXFILE.LXT_EARN_13,
      *            LXFILE.LXT_EARN_14,
      *            LXFILE.LXT_EARN_15,
      *            LXFILE.LXT_EARN_16,
      *            LXFILE.LXT_EARN_17,
      *            LXFILE.LXT_EARN_18,
      *            LXFILE.LXT_EARN_19,
      *            LXFILE.LXT_EARN_20,
      *            LXFILE.LXP_CODE,
      *            LXFILE.LXP_EARN_1,
      *            LXFILE.LXP_EARN_2,
      *            LXFILE.LXP_EARN_3,
      *            LXFILE.LXP_EARN_4,
      *            LXFILE.LXP_EARN_5,
      *            LXFILE.LXP_EARN_6,
      *            LXFILE.LXP_EARN_7,
      *            LXFILE.LXP_EARN_8,
      *            LXFILE.LXP_EARN_9,
      *            LXFILE.LXP_EARN_10,
      *            LXFILE.LXP_EARN_11,
      *            LXFILE.LXP_EARN_12,
      *            LXFILE.LXP_EARN_13,
      *            LXFILE.LXP_EARN_14,
      *            LXFILE.LXP_EARN_15,
      *            LXFILE.LXP_EARN_16,
      *            LXFILE.LXP_EARN_17,
      *            LXFILE.LXP_EARN_18,
      *            LXFILE.LXP_EARN_19,
      *            LXFILE.LXP_EARN_20,
      *            LXFILE.LXA_CODE,
      *            LXFILE.LXA_SRCD,
      *            LXFILE.LXA_START_PMT_1,
      *            LXFILE.LXA_END_PMT_1,
      *            LXFILE.LXA_NET_CHANGE_1,
      *            LXFILE.LXA_START_PMT_2,
      *            LXFILE.LXA_END_PMT_2,
      *            LXFILE.LXA_NET_CHANGE_2,
      *            LXFILE.LXA_START_PMT_3,
      *            LXFILE.LXA_END_PMT_3,
      *            LXFILE.LXA_NET_CHANGE_3,
      *            LXFILE.LXA_START_PMT_4,
      *            LXFILE.LXA_END_PMT_4,
      *            LXFILE.LXA_NET_CHANGE_4,
      *            LXFILE.LXA_START_PMT_5,
      *            LXFILE.LXA_END_PMT_5,
      *            LXFILE.LXA_NET_CHANGE_5,
      *            LXFILE.LXA_START_PMT_6,
      *            LXFILE.LXA_END_PMT_6,
      *            LXFILE.LXA_NET_CHANGE_6,
      *            LXFILE.LXA_START_PMT_7,
      *            LXFILE.LXA_END_PMT_7,
      *            LXFILE.LXA_NET_CHANGE_7,
      *            LXFILE.LXA_START_PMT_8,
      *            LXFILE.LXA_END_PMT_8,
      *            LXFILE.LXA_NET_CHANGE_8,
      *            LXFILE.LXA_START_PMT_9,
      *            LXFILE.LXA_END_PMT_9,
      *            LXFILE.LXA_NET_CHANGE_9,
      *            LXFILE.LXA_LN_INSCOMP,
      *            LXFILE.LXA_LN_INSOURS,
      *            CAST(LXFILE.LXA_LN_INSEFF_DATE AS VARCHAR(10)),
      *            CAST(LXFILE.LXA_LN_INSEXP_DATE AS VARCHAR(10)),
      *            LXFILE.LXA_LN_INSPREM,
      *            LXFILE.LXA_LN_INSCOVR,
      *            LXFILE.LXA_LN_INSCOMM,
      *            LXFILE.LXA_LN_INS_REBATE,
      *            LXFILE.LXA_LN_INSCODES,
      *            LXFILE.LXZ_CODE,
      *            LXFILE.LXZ_POST_REGPY,
      *            LXFILE.LXZ_POST_EXTRA_PRIN,
      *            LXFILE.LXZ_POST_OTH,
      *            LXFILE.LXZ_POST_LCHG,
      *            LXFILE.LXZ_PDUE_APINT,
      *            LXFILE.LXZ_PDUE_APPRIN,
      *            LXFILE.LXZ_CUR_APINT,
      *            LXFILE.LXZ_CUR_APPRIN,
      *            LXFILE.LXR_CODE,
      *            LXFILE.LXR_REB_AMT,
      *            LXFILE.LXX_CODE,
      *            LXFILE.LXX_INS_TAX_COUNTY_CD,
      *            LXFILE.LXX_INS_TAX_CITY_CD,
      *            LXFILE.LXM_CODE,
      *            LXFILE.LXM_MIP_CLPAID,
      *            CAST(LXFILE.LXM_MIP_CLPDTH_DATE AS VARCHAR(10)),
      *            LXFILE.LXM_MIP_AHPAID,
      *            CAST(LXFILE.LXM_MIP_AHPDTH_DATE AS VARCHAR(10)),
      *            ISNULL(LXEFILE.LXE_BRNO,0),
      *            LXEFILE.LXE_ACCTNO,
      *            LXEFILE.LXE_SEQNO,
      *            CAST(LXEFILE.LXE_LTOUCH_DATE AS VARCHAR(10)),
      *            LXEFILE.LXE_EARN_1,
      *            LXEFILE.LXE_EARN_2,
      *            LXEFILE.LXE_EARN_3,
      *            LXEFILE.LXE_EARN_4,
      *            LXEFILE.LXE_EARN_5,
      *            LXEFILE.LXE_EARN_6,
      *            LXEFILE.LXE_EARN_7,
      *            LXEFILE.LXE_WORKER,
      *            LXEFILE.LXE_WORKER2, 
      *            ISNULL(LXGFILE.LXG_BRNO,0),
      *            LXGFILE.LXG_ACCTNO,
      *            LXGFILE.LXG_SEQNO,
      *            CAST(LXGFILE.LXG_LTOUCH_DATE AS VARCHAR(10)),
      *            LXGFILE.LXG_GLNO_1,
      *            LXGFILE.LXG_GLAMT_1,
      *            LXGFILE.LXG_GLNO_2,
      *            LXGFILE.LXG_GLAMT_2,
      *            LXGFILE.LXG_GLNO_3,
      *            LXGFILE.LXG_GLAMT_3
      *         FROM
      *          (SELECT LXFILE.LX_BRNO   AS BRNO
      *                 ,LXFILE.LX_ACCTNO AS ACCTNO
      *                 ,LXFILE.LX_SEQNO  AS SEQNO
      *           FROM DBO.LXFILE
      *           WHERE LXFILE.LX_BRNO = @QLX_WBEG_BRNO
      *             AND LXFILE.LX_ACCTNO BETWEEN @QLX_WBEG_ACCTNO AND @QLX_WEND_ACCTNO
      *       UNION 
      *        SELECT LXEFILE.LXE_BRNO   AS BRNO
      *              ,LXEFILE.LXE_ACCTNO AS ACCTNO
      *              ,LXEFILE.LXE_SEQNO  AS SEQNO
      *        FROM DBO.LXEFILE
      *        WHERE LXEFILE.LXE_BRNO = @QLX_WBEG_BRNO
      *          AND LXEFILE.LXE_ACCTNO BETWEEN @QLX_WBEG_ACCTNO AND @QLX_WEND_ACCTNO
      *       UNION
      *        SELECT LXGFILE.LXG_BRNO   AS BRNO
      *              ,LXGFILE.LXG_ACCTNO AS ACCTNO
      *	             ,LXGFILE.LXG_SEQNO  AS SEQNO
      *        FROM DBO.LXGFILE
      *        WHERE LXGFILE.LXG_BRNO  = @QLX_WBEG_BRNO
      *          AND LXGFILE.LXG_ACCTNO BETWEEN @QLX_WBEG_ACCTNO AND @QLX_WEND_ACCTNO) TMP
      *    LEFT OUTER JOIN DBO.LXFILE
      *     ON LXFILE.LX_BRNO = TMP.BRNO
      *    AND LXFILE.LX_ACCTNO = TMP.ACCTNO
      *    AND LXFILE.LX_SEQNO  = TMP.SEQNO
      *    LEFT OUTER JOIN DBO.LXEFILE
      *     ON LXEFILE.LXE_BRNO = TMP.BRNO
      *    AND LXEFILE.LXE_ACCTNO = TMP.ACCTNO
      *    AND LXEFILE.LXE_SEQNO  = TMP.SEQNO
      *   LEFT OUTER JOIN DBO.LXGFILE
      *    ON LXGFILE.LXG_BRNO = TMP.BRNO
      *   AND LXGFILE.LXG_ACCTNO = TMP.ACCTNO
      *   AND LXGFILE.LXG_SEQNO  = TMP.SEQNO
      * ORDER BY TMP.ACCTNO, TMP.SEQNO

           EXEC SQL SET CONNECTION LX1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE LX1_CURSOR CURSOR FOR
                :LX1-RETURN-CODE =  
            EXEC SSP_LXFILE_LXFILE_SELECT
              ( :QLX1-WBEG-BRNO,:QLX1-WBEG-ACCTNO,:QLX1-WEND-ACCTNO)
           END-EXEC.

           EXEC SQL
               OPEN LX1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           MOVE STAT---GOOD TO LX1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LX1-FILE-NEXT.  *> SSP  LXFILE LXEFILE LXGFILE
           PERFORM READ-IT.
           MOVE LX-PATH-OWNBR  TO LX-BRNO.
           MOVE LX-PATH-SQL    TO E-FILE.
           MOVE LX1-KEY        TO E-KEYX.
           INITIALIZE QLX-REC QLXE-REC QLXG-REC.

           EXEC SQL SET CONNECTION LX1_CONNECT END-EXEC.

           IF ( LX1-CURSOR-STAT-GOOD )
              EXEC SQL
               FETCH LX1_CURSOR INTO 
                   :WS-TMP-BRNO,
                   :WS-TMP-ACCTNO,
                   :WS-TMP-SEQNO,
                   :QLX-BRNO,
                   :QLX-ACCTNO,
                   :QLX-SEQNO,
                   :QLX-LTOUCH-DATE,
                   :QLXT-CODE,
                   :QLXT-EARN-1,
                   :QLXT-EARN-2,
                   :QLXT-EARN-3,
                   :QLXT-EARN-4,
                   :QLXT-EARN-5,
                   :QLXT-EARN-6,
                   :QLXT-EARN-7,
                   :QLXT-EARN-8,
                   :QLXT-EARN-9,
                   :QLXT-EARN-10,
                   :QLXT-EARN-11,
                   :QLXT-EARN-12,
                   :QLXT-EARN-13,
                   :QLXT-EARN-14,
                   :QLXT-EARN-15,
                   :QLXT-EARN-16,
                   :QLXT-EARN-17,
                   :QLXT-EARN-18,
                   :QLXT-EARN-19,
                   :QLXT-EARN-20,
                   :QLXP-CODE,
                   :QLXP-EARN-1,
                   :QLXP-EARN-2,
                   :QLXP-EARN-3,
                   :QLXP-EARN-4,
                   :QLXP-EARN-5,
                   :QLXP-EARN-6,
                   :QLXP-EARN-7,
                   :QLXP-EARN-8,
                   :QLXP-EARN-9,
                   :QLXP-EARN-10,
                   :QLXP-EARN-11,
                   :QLXP-EARN-12,
                   :QLXP-EARN-13,
                   :QLXP-EARN-14,
                   :QLXP-EARN-15,
                   :QLXP-EARN-16,
                   :QLXP-EARN-17,
                   :QLXP-EARN-18,
                   :QLXP-EARN-19,
                   :QLXP-EARN-20,
                   :QLXA-CODE,
                   :QLXA-SRCD,
                   :QLXA-START-PMT-1,
                   :QLXA-END-PMT-1,
                   :QLXA-NET-CHANGE-1,
                   :QLXA-START-PMT-2,
                   :QLXA-END-PMT-2,
                   :QLXA-NET-CHANGE-2,
                   :QLXA-START-PMT-3,
                   :QLXA-END-PMT-3,
                   :QLXA-NET-CHANGE-3,
                   :QLXA-START-PMT-4,
                   :QLXA-END-PMT-4,
                   :QLXA-NET-CHANGE-4,
                   :QLXA-START-PMT-5,
                   :QLXA-END-PMT-5,
                   :QLXA-NET-CHANGE-5,
                   :QLXA-START-PMT-6,
                   :QLXA-END-PMT-6,
                   :QLXA-NET-CHANGE-6,
                   :QLXA-START-PMT-7,
                   :QLXA-END-PMT-7,
                   :QLXA-NET-CHANGE-7,
                   :QLXA-START-PMT-8,
                   :QLXA-END-PMT-8,
                   :QLXA-NET-CHANGE-8,
                   :QLXA-START-PMT-9,
                   :QLXA-END-PMT-9,
                   :QLXA-NET-CHANGE-9,
                   :QLXA-LN-INSCOMP,
                   :QLXA-LN-INSOURS,
                   :QLXA-LN-INSEFF-DATE,
                   :QLXA-LN-INSEXP-DATE,
                   :QLXA-LN-INSPREM,
                   :QLXA-LN-INSCOVR,
                   :QLXA-LN-INSCOMM,
                   :QLXA-LN-INS-REBATE,
                   :QLXA-LN-INSCODES,
                   :QLXZ-CODE,
                   :QLXZ-POST-REGPY,
                   :QLXZ-POST-EXTRA-PRIN,
                   :QLXZ-POST-OTH,
                   :QLXZ-POST-LCHG,
                   :QLXZ-PDUE-APINT,
                   :QLXZ-PDUE-APPRIN,
                   :QLXZ-CUR-APINT,
                   :QLXZ-CUR-APPRIN,
                   :QLXR-CODE,
                   :QLXR-REB-AMT,
                   :QLXX-CODE,
                   :QLXX-INS-TAX-COUNTY-CD,
                   :QLXX-INS-TAX-CITY-CD,
                   :QLXM-CODE,
                   :QLXM-MIP-CLPAID,
                   :QLXM-MIP-CLPDTH-DATE,
                   :QLXM-MIP-AHPAID,
                   :QLXM-MIP-AHPDTH-DATE,
                   :QLXE-BRNO,
                   :QLXE-ACCTNO,
                   :QLXE-SEQNO,
                   :QLXE-LTOUCH-DATE,
                   :QLXE-EARN-1,
                   :QLXE-EARN-2,
                   :QLXE-EARN-3,
                   :QLXE-EARN-4,
                   :QLXE-EARN-5,
                   :QLXE-EARN-6,
                   :QLXE-EARN-7,
                   :QLXE-WORKER,
                   :QLXE-WORKER2, 
                   :QLXG-BRNO,
                   :QLXG-ACCTNO,
                   :QLXG-SEQNO,
                   :QLXG-LTOUCH-DATE,
                   :QLXG-GLNO-1,
                   :QLXG-GLAMT-1,
                   :QLXG-GLNO-2,
                   :QLXG-GLAMT-2,
                   :QLXG-GLNO-3,
                   :QLXG-GLAMT-3
 
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LX1-FILE-NEXT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           IF ( IO-FG = 0 )
              IF QLX-BRNO > 0
                 PERFORM GET-LX-FIELDS
              END-IF
              IF QLXE-BRNO > 0
                 PERFORM GET-LXE-FIELDS
              END-IF
              IF QLXG-BRNO > 0
                 PERFORM GET-LXG-FIELDS
              END-IF.

      *-----------------------------------------------------------------
       CLOSE-LX1-FILE.

           PERFORM CLOSE-IT.

           IF ( LX1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION LX1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE LX1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO LX1-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-LX1-CONNECTION.

           IF LX1-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION LX1_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT LX1_CONNECT END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE "N" TO LX1-CONN-OPEN-SW.
      *-----------------------------------------------------------------


       OPEN-OLX1-FILE.
           PERFORM OPEN-IT.
           MOVE OLX-PATH TO E-FILE.
           OPEN INPUT OLX-FILE.
           IF IO-FG = 8
              GO TO OPEN-OLX1-FILE.
           IF IO-FG = 7
              CLOSE OLX-FILE
              GO TO OPEN-OLX1-FILE.
      *READ-OLX1-FILE.
      *    PERFORM READ-IT.
      *    MOVE OLX-PATH TO E-FILE.
      *    MOVE OLX1-KEY TO E-KEYX.
      *    READ OLX-FILE.
      *    IF IO-FG = 8
      *       GO TO READ-OLX1-FILE.
       START-OLX1-FILE.
           PERFORM START-IT.
           MOVE OLX-PATH TO E-FILE.
           MOVE OLX1-KEY TO E-KEYX.
           START OLX-FILE KEY NOT < OLX1-KEY.
       READ-OLX1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OLX-PATH TO E-FILE.
           MOVE OLX1-KEY TO E-KEYX.
           READ OLX-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OLX1-FILE-NEXT.
       CLOSE-OLX1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OLX-FILE.

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
              DESTROY LXFILE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
