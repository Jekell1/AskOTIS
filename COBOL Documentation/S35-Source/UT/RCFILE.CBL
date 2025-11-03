       IDENTIFICATION DIVISION.
       PROGRAM-ID.    RCFILE.
       DATE-WRITTEN.  01-24-2024.
      **********************************************************************
      *
      * DESCRIPTION   : COMPARE S35 RCFILE WITH A15 RCFILE
      *            
      *                 TO BE USED AFTER AN UPDATE
      *                 WILL COMPARE EACH FIELD AND REPORT ANY 
      *                 DIFFERENCES, AS LONG AS FIELD EXIST IN BOTH FILES
      *
      *  LOOPS THRU THE A15 FILE AND VERIFIES THE S35 FILE EXIST AND MATCHES
      *
      * 01-08-2025 RMZ - ADDED 'BTRACK' - PROCESS TRACKING TO PROGRAM.
      * 03-07-2025 MB  - CHANGED TO ONLY COMPARE TIME FIELDS IF ALL BRANCHS
      *                    REQUEST.  THIS SHOULD THEN JUST CHECK FOR RELOAD
      *                    AND NOT THE DAILY QA TESTS.
      ***********************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
      * COPYMEMBER: LIBGB/GBFSRC
           SELECT ORC-FILE ASSIGN TO ORC-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY ORC1-KEY
                  ALTERNATE RECORD KEY ORC2-KEY
                  FILE STATUS FILE-STAT.

           SELECT WK-FILE ASSIGN TO WK-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK-KEY
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  ORC-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A15SRC/GB01RC.CPY" REPLACING LEADING "RC" BY "ORC".

       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
              05  WK-KEY-BRNO         PIC 9(4).
              05  WK-KEY-DATE         PIC 9(8).
           03  WK-DATA                PIC X(160).

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/RCFILE S35 01/26/24-07:55:57 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGB/GB01RC.CPY".
       COPY "LIBGB/GB01RC_SQL.CPY".
       COPY "LIBGB/GBWSRC.CPY".

       01  ORC-PATH                    PIC X(35) VALUE SPACES.
       01  ORC-ALL-PATH.
           03  ORC-ALL-BASE            PIC X(9).
           03  FILLER                  PIC X     VALUE "/".
           03  ORC-ALL-MACHINE         PIC XX.
           03  FILLER                  PIC X(10) VALUE
               "/GB/RCFILE".

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

       01  LITTLE-Z.
           03  FILLER           PIC X VALUE X"7A".

      *    OUTPUT:
           03  SETRC-DATE          PIC 9(7)    VALUE 0.
           03  FILLER              REDEFINES SETRC-DATE.
               05  SETRC-YYY       PIC 999.
               05  SETRC-MM        PIC 99.
               05  SETRC-DD        PIC 99.

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

       01  FIRST-SW                    PIC X         VALUE "Y".
       01  WK-EOF-SW                   PIC X         VALUE "N".
       01  RC1-CONN-OPEN-SW            PIC X         VALUE "N".
       01  RC-EOF-SW                   PIC X         VALUE "N".
       01  ORC-EOF-SW                  PIC X         VALUE "N".
       01  TOTAL-ERRORS-CTR            PIC 9(9)      VALUE 0.
       01  SQL-RECORDS-PROCESSED       PIC 9(9)      VALUE 0.
       01  A15-RECORDS-PROCESSED       PIC 9(9)      VALUE 0.
           88  NO-RECORDS-FOUND                      VALUE ZEROES.

       01  STARTING-TIME               PIC X(08)     VALUE SPACES.
       01  SUB                         PIC 9(04)     VALUE ZEROES.
       01  S1                          PIC 9(04)     VALUE ZEROES.
       01  S2                          PIC 9(04)     VALUE ZEROES.
       01  S3                          PIC 9(04)     VALUE ZEROES.

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
           03  BEG-DATE               PIC 9(08).
           03  BEG-DATEX REDEFINES BEG-DATE.
               05  BEG-DATE-CC         PIC 99.
               05  BEG-DATE-YY         PIC 99.
               05  BEG-DATE-MM         PIC 99.
               05  BEG-DATE-DD         PIC 99.
           03  END-DATE               PIC 9(08).
           03  END-DATEX REDEFINES END-DATE.
               05  END-DATE-CC         PIC 99.
               05  END-DATE-YY         PIC 99.
               05  END-DATE-MM         PIC 99.
               05  END-DATE-DD         PIC 99.
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
           "DAILY STATUS (RCFILE) COMPARE A15 TO S35".

       01  HEADER-LINE-03.
           03  FILLER                PIC X(4)  VALUE "BRNO".
           03  FILLER                PIC X(3)  VALUE SPACES.
           03  FILLER                PIC X(10) VALUE "  DATE  ".
           03  FILLER                PIC X(3) VALUE SPACES.
           03  FILLER                PIC X(28) VALUE "DESCRIPTION".
            03  FILLER               PIC X(22) VALUE "A15".
           03  FILLER                PIC X(3) VALUE "S35".

      ******************************************************************
      *
      *    D E T A I L - L I N E - W O R K E R S
      *
      ******************************************************************
       01  DETAIL-LINE                        PIC X(100)   VALUE SPACES.

       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-BRNO                       PIC Z(3)9.
           03  FILLER                         PIC X(3).
           03  D01-DATE                       PIC 9999/99/99.
           03  FILLER                         PIC XXX.
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
       COPY "LIBUT/RCFILE_DEF.CPY".
       COPY "LIBUT/RCFILE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/RCFILE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE ORC-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-RC1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           PERFORM CLOSE-WK1-FILE.
           PERFORM CLOSE-ORC1-FILE.
           PERFORM CLOSE-RC1-CONNECTION.
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
           MOVE "RCFILE" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE RCFILE-QUEUE-POS TO Q-POS.
           MOVE RCFILE-COPIES-POS TO C-POS.
           MOVE RCFILE-PRU-POS   TO P-POS.
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

           PERFORM OPEN-RC1-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-PR-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WK-PATH.

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
              GO TO ENTER-END-DATE.

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

      *-----------------------------------------------------------------
       ENTER-BEG-DATE.

           MOVE "ERNM060BEG-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F3-KEY ) GO TO ENTER-ALL-DATES.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-DATE.

           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.

      *-----------------------------------------------------------------
       ENTER-END-DATE.

           MOVE "ERNM060END-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-DATE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-DATE.

           MOVE VDU-DATE-YYYYMMDD TO END-DATE.

           IF END-DATE < BEG-DATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DATE.

           GO TO ENTER-PARAMETERS-END.

      *-----------------------------------------------------------------
       ENTER-ALL-DATES.

           MOVE "S  8080BEG-DATE."  TO VDU.
           MOVE EXT-JULIAN-BEG TO BEG-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "S  8080END-DATE."  TO VDU.
           MOVE EXT-JULIAN-END TO END-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
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

           MOVE 9999    TO PROGID-SLEEP-COUNTER.
           PERFORM SEND-PROGRESS-INDICATOR.

      D    STOP MESS.

           MOVE EXT-FILPATH-BASE   TO FILPATH-BASE.
           MOVE BR-MACHINE         TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH       TO FILPATH-DATA.

           MOVE "N" TO RC-EOF-SW ORC-EOF-SW.

           MOVE BEG-DATE TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QRC1-WBEG-TRANS-DATE.

           MOVE END-DATE TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QRC1-WEND-TRANS-DATE.

           MOVE BR-NO TO QRC1-WBEG-BRNO QRC1-WEND-BRNO.

           MOVE "PROCESSING S35 DATA" TO BTRACK-MESS. 
           PERFORM BTRACK-GLOBAL.

           PERFORM START-RC1-FILE.  *> SSP  RCFILE
           PERFORM M-READ-RC.

           MOVE "PROCESSING A15 DATA" TO BTRACK-MESS.
           PERFORM BTRACK-GLOBAL.

           IF FIRST-SW = "Y"
      * MUST LOAD ALL ORC FILES INTO WK FILE FOR REQUESTED BRANCH  
              PERFORM OPEN-WK1-FILE-OUTPUT
              MOVE "N" TO FIRST-SW
              MOVE SOURCE-PATH  TO ORC-ALL-BASE
              MOVE MACHINE-R1   TO ORC-ALL-MACHINE
              MOVE ORC-ALL-PATH TO ORC-PATH ACCESS-BUF
              PERFORM ACCESS-CALL
              IF ( STAT-BAD )
                 MOVE "MISSING R1 A15 FILE" TO D01-TEXT 
                 MOVE ORC-ALL-PATH          TO D01-A15
                 PERFORM WRITE-DETAIL-LINE-ERROR
                 GO TO PROCESS-BRANCH-DRIVER-END
              END-IF
              MOVE "N" TO ORC-EOF-SW
              PERFORM OPEN-ORC1-FILE
              PERFORM LOAD-WORK-FILE
              PERFORM CLOSE-ORC1-FILE

              MOVE SOURCE-PATH  TO ORC-ALL-BASE
              MOVE MACHINE-R2   TO ORC-ALL-MACHINE
              MOVE ORC-ALL-PATH TO ORC-PATH ACCESS-BUF
              PERFORM ACCESS-CALL
              IF ( STAT-BAD )
                 MOVE "MISSING R2 A15 FILE" TO D01-TEXT 
                 MOVE ORC-ALL-PATH          TO D01-A15
                 MOVE SPACES                TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR
                 GO TO PROCESS-BRANCH-DRIVER-END
              END-IF 
              MOVE "N" TO ORC-EOF-SW
              PERFORM OPEN-ORC1-FILE
              PERFORM LOAD-WORK-FILE
              PERFORM CLOSE-ORC1-FILE
              PERFORM CLOSE-WK1-FILE
              PERFORM OPEN-WK1-FILE
              PERFORM SEND-PROGRESS-INDICATOR.

           MOVE "N"      TO WK-EOF-SW.
           MOVE BR-NO    TO WK-KEY-BRNO.
           MOVE BEG-DATE TO WK-KEY-DATE.
           PERFORM START-WK1-FILE.
           PERFORM M-READ-WK1.

           PERFORM PROCESS-BRANCH-NEXT-RC
              UNTIL RC-EOF-SW = "Y" AND WK-EOF-SW = "Y".

           PERFORM CLOSE-RC1-FILE.

           GO TO PROCESS-BRANCH-DRIVER-EXIT.

       PROCESS-BRANCH-NEXT-RC.
           PERFORM SEND-PROGRESS-INDICATOR.

           PERFORM COMPARE-FIELDS.

           IF WK-KEY < RC1-KEY
              PERFORM M-READ-WK1
           ELSE
              IF WK-KEY > RC1-KEY
                 PERFORM M-READ-RC
              ELSE
                 PERFORM M-READ-WK1
                 PERFORM M-READ-RC.

       PROCESS-BRANCH-DRIVER-END.

           PERFORM CLOSE-RC1-FILE.

       PROCESS-BRANCH-DRIVER-EXIT.
           EXIT.

       LOAD-WORK-FILE SECTION.

           PERFORM M-READ-ORC.
           IF ORC-EOF-SW = "Y"
              GO TO LOAD-WORK-FILE-END.

           MOVE ORC-TRANS-DATE TO SETRC-DATE
           MOVE SETRC-YYY      TO DATE-MMDDYY-YY.
           MOVE SETRC-MM       TO DATE-MMDDYY-MM.
           MOVE SETRC-DD       TO DATE-MMDDYY-DD.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.

           IF DATE-YYYYMMDD < BEG-DATE OR DATE-YYYYMMDD > END-DATE
              GO TO LOAD-WORK-FILE.

           MOVE DATE-YYYYMMDD  TO WK-KEY-DATE.
           MOVE ORC-BRNO       TO WK-KEY-BRNO.

           MOVE ORC-REC TO WK-DATA.
           PERFORM WRITE-WK1-FILE.

           GO TO LOAD-WORK-FILE.

       LOAD-WORK-FILE-END.
           EXIT. 

       M-READ-WK1 SECTION.
           PERFORM READ-WK1-FILE-NEXT.
           IF IO-BAD OR WK-KEY-BRNO NOT = BR-NO 
             OR WK-KEY-DATE > QRC1-WEND-TRANS-DATE
              MOVE ALL '9'  TO WK-KEY
              MOVE "Y"      TO WK-EOF-SW
           ELSE
              MOVE WK-DATA TO ORC-REC
              ADD 1  TO A15-RECORDS-PROCESSED.

       EXIT-M-READ-WK1.
           EXIT.

       M-READ-RC SECTION.
           PERFORM READ-RC1-FILE-NEXT.   *> SSP  RCFILE
           IF IO-BAD
              MOVE ALL '9' TO RC1-KEY
              MOVE "Y"     TO RC-EOF-SW
           ELSE
              ADD 1 TO SQL-RECORDS-PROCESSED.

       EXIT-M-READ-CH.
           EXIT.

       M-READ-ORC SECTION.
           PERFORM READ-ORC1-FILE-NEXT.
           IF IO-BAD 
              MOVE "Y"      TO ORC-EOF-SW
           ELSE
              IF ORC-BRNO NOT NUMERIC OR ORC-TRANS-DATE NOT NUMERIC
                 GO TO M-READ-ORC
              END-IF 
              IF ORC-ALL-MACHINE = MACHINE-R2 AND ORC-BRNO = 9990
                 GO TO M-READ-ORC
              END-IF
              MOVE ORC-TRANS-DATE TO SETRC-DATE
              MOVE SETRC-YYY      TO DATE-MMDDYY-YY
              MOVE SETRC-MM       TO DATE-MMDDYY-MM
              MOVE SETRC-DD       TO DATE-MMDDYY-DD
              PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
              IF DATE-YYYYMMDD < BEG-DATE OR > END-DATE
                 GO TO M-READ-ORC
              END-IF.

       EXIT-M-READ-ORC1.
           EXIT.

      ******************************************************************
       COMPARE-FIELDS SECTION.
      ******************************************************************

           IF WK-KEY < RC1-KEY
              MOVE "MISSING S35 RECORD" TO D01-TEXT
              MOVE BR-NO                TO D01-BRNO
              MOVE WK-KEY-DATE          TO D01-DATE
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.

           IF WK-KEY > RC1-KEY
              MOVE "MISSING A15 RECORD" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.

           IF ORC-STATUS NOT = RC-STATUS
              MOVE "RC-STATUS"    TO D01-TEXT
              MOVE ORC-STATUS     TO D01-A15
              MOVE RC-STATUS      TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF ORC-RECAP-FG NOT = RC-RECAP-FG
              MOVE "RC-RECAP-FG" TO D01-TEXT
              MOVE ORC-RECAP-FG  TO D01-A15
              MOVE RC-RECAP-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF ORC-GL-FG NOT = RC-GL-FG
              MOVE "RC-GL-FG"    TO D01-TEXT
              MOVE BR-NO         TO D01-BRNO
              MOVE RC-TRANS-DATE TO D01-DATE
              MOVE ORC-GL-FG     TO D01-A15
              MOVE RC-GL-FG      TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF ORC-EOD-BATCH-FG NOT = RC-EOD-BATCH-FG
              MOVE "RC-EOD-BATCH-FG" TO D01-TEXT
              MOVE ORC-EOD-BATCH-FG  TO D01-A15
              MOVE RC-EOD-BATCH-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF ORC-NIGHTLY-FUNDS-FG NOT = RC-NIGHTLY-FUNDS-FG
              MOVE "RC-NIGHTLY-FUNDS-FG" TO D01-TEXT
              MOVE ORC-NIGHTLY-FUNDS-FG  TO D01-A15
              MOVE RC-NIGHTLY-FUNDS-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF ORC-EOM1-BATCH-FG NOT = RC-EOM1-BATCH-FG
              MOVE "RC-EOM1-BATCH-FG" TO D01-TEXT
              MOVE ORC-EOM1-BATCH-FG  TO D01-A15
              MOVE RC-EOM1-BATCH-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF ORC-EOM-UPDATE-FG NOT = RC-EOM-UPDATE-FG
              MOVE "RC-EOM-UPDATE-FG" TO D01-TEXT
              MOVE ORC-STATUS         TO D01-A15
              MOVE RC-STATUS          TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF ORC-EOM2-BATCH-FG NOT = RC-EOM2-BATCH-FG
              MOVE "RC-EOM2-BATCH-FG"    TO D01-TEXT
              MOVE ORC-EOM2-BATCH-FG     TO D01-A15
              MOVE RC-EOM2-BATCH-FG      TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF ORC-BS-P2P3-INBAL-FG NOT = RC-BS-P2P3-INBAL-FG
              MOVE "RC-BS-P2P3-INBAL-FG" TO D01-TEXT
              MOVE ORC-BS-P2P3-INBAL-FG  TO D01-A15
              MOVE RC-BS-P2P3-INBAL-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

      *     IF (ORC-DEPOSIT NOT NUMERIC)
      *        MOVE 0  TO ORC-DEPOSIT.
           IF ORC-DEPOSIT NOT = RC-DEPOSIT
              MOVE "RC-DEPOSIT" TO D01-TEXT
              MOVE ORC-DEPOSIT  TO D01-A15-NUM
              MOVE RC-DEPOSIT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

      *     IF (ORC-OVER-SHORT NOT NUMERIC)
      *        MOVE 0  TO ORC-OVER-SHORT.
           IF ORC-OVER-SHORT NOT = RC-OVER-SHORT
              MOVE "RC-OVER-SHORT" TO D01-TEXT
              MOVE ORC-OVER-SHORT  TO D01-A15-NUM
              MOVE RC-OVER-SHORT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

      *     IF (ORC-CHECKS NOT NUMERIC)
      *        MOVE 0  TO ORC-CHECKS.
           IF ORC-CHECKS NOT = RC-CHECKS
              MOVE "RC-CHECKS" TO D01-TEXT
              MOVE ORC-CHECKS  TO D01-A15-NUM
              MOVE RC-CHECKS   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE ORC-OPENED-DATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = RC-OPENED-DATE
              MOVE "RC-OPENED-DATE-DATE" TO D01-TEXT
              MOVE DATE-YYYYMMDD         TO D01-A15
              MOVE RC-OPENED-DATE        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

      *     IF (ORC-OPENED-TIME NOT NUMERIC)
      *        MOVE 0  TO ORC-OPENED-TIME.
           IF ENT-GROUP = "N" AND BEG-BRANCH = 0
            AND END-BRANCH = 9999
              IF ORC-OPENED-TIME NOT = RC-OPENED-TIME
                 MOVE "RC-OPENED-TIME" TO D01-TEXT
                 MOVE ORC-OPENED-TIME  TO D01-A15-NUM
                 MOVE RC-OPENED-TIME   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF ORC-OP-USERID NOT = RC-OP-USERID
              MOVE "RC-OP-USERID" TO D01-TEXT
              MOVE ORC-OP-USERID  TO D01-A15
              MOVE RC-OP-USERID   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE ORC-CLOSED-DATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = RC-CLOSED-DATE
              MOVE "RC-CLOSED-DATE-DATE" TO D01-TEXT
              MOVE DATE-YYYYMMDD         TO D01-A15
              MOVE RC-CLOSED-DATE        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

      *     IF (ORC-CLOSED-TIME NOT NUMERIC)
      *        MOVE 0  TO ORC-CLOSED-TIME.
           IF ENT-GROUP = "N" AND BEG-BRANCH = 0
            AND END-BRANCH = 9999
              IF ORC-CLOSED-TIME NOT = RC-CLOSED-TIME
                 MOVE "RC-CLOSED-TIME" TO D01-TEXT
                 MOVE ORC-CLOSED-TIME  TO D01-A15-NUM
                 MOVE RC-CLOSED-TIME   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF ORC-CL-USERID NOT = RC-CL-USERID
              MOVE "RC-CL-USERID" TO D01-TEXT
              MOVE ORC-CL-USERID  TO D01-A15
              MOVE RC-CL-USERID   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE ORC-UPDATED-DATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = RC-UPDATED-DATE
              MOVE "RC-UPDATED-DATE-DATE" TO D01-TEXT
              MOVE DATE-YYYYMMDD          TO D01-A15
              MOVE RC-UPDATED-DATE        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

      *     IF (ORC-UPDATED-TIME NOT NUMERIC)
      *        MOVE 0  TO ORC-UPDATED-TIME.
           IF ENT-GROUP = "N" AND BEG-BRANCH = 0
            AND END-BRANCH = 9999
              IF ORC-UPDATED-TIME NOT = RC-UPDATED-TIME
                 MOVE "RC-UPDATED-TIME" TO D01-TEXT
                 MOVE ORC-UPDATED-TIME  TO D01-A15-NUM
                 MOVE RC-UPDATED-TIME   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE ORC-EOD-BATCH-DATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = RC-EOD-BATCH-DATE
              MOVE "RC-EOD-BATCH-DATE-DATE" TO D01-TEXT
              MOVE DATE-YYYYMMDD            TO D01-A15
              MOVE RC-EOD-BATCH-DATE        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

      *     IF (ORC-EOD-BATCH-TIME NOT NUMERIC)
      *        MOVE 0  TO ORC-EOD-BATCH-TIME.
           IF ENT-GROUP = "N" AND BEG-BRANCH = 0
            AND END-BRANCH = 9999
              IF ORC-EOD-BATCH-TIME NOT = RC-EOD-BATCH-TIME
                 MOVE "RC-EOD-BATCH-TIME" TO D01-TEXT
                 MOVE ORC-EOD-BATCH-TIME  TO D01-A15-NUM
                 MOVE RC-EOD-BATCH-TIME   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE ORC-FUNDS-DATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = RC-FUNDS-DATE
              MOVE "RC-FUNDS-DATE-DATE" TO D01-TEXT
              MOVE DATE-YYYYMMDD        TO D01-A15
              MOVE RC-FUNDS-DATE        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

      *     IF (ORC-FUNDS-TIME NOT NUMERIC)
      *        MOVE 0  TO ORC-FUNDS-TIME.
           IF ENT-GROUP = "N" AND BEG-BRANCH = 0
            AND END-BRANCH = 9999
              IF ORC-FUNDS-TIME NOT = RC-FUNDS-TIME
                 MOVE "RC-FUNDS-TIME" TO D01-TEXT
                 MOVE ORC-FUNDS-TIME  TO D01-A15-NUM
                 MOVE RC-FUNDS-TIME   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE ORC-EARNINGS-DATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = RC-EARNINGS-DATE
              MOVE "RC-EARNINGS-DATE-DATE" TO D01-TEXT
              MOVE DATE-YYYYMMDD           TO D01-A15
              MOVE RC-EARNINGS-DATE        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

      *     IF (ORC-EARNINGS-TIME NOT NUMERIC)
      *        MOVE 0  TO ORC-EARNINGS-TIME.
           IF ENT-GROUP = "N" AND BEG-BRANCH = 0
            AND END-BRANCH = 9999
              IF ORC-EARNINGS-TIME NOT = RC-EARNINGS-TIME
                 MOVE "RC-EARNINGS-TIME" TO D01-TEXT
                 MOVE ORC-EARNINGS-TIME  TO D01-A15-NUM
                 MOVE RC-EARNINGS-TIME   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE ORC-EOM-UPDATED-DATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = RC-EOM-UPDATED-DATE
              MOVE "RC-EOM-UPDATED-DATE-DATE" TO D01-TEXT
              MOVE DATE-YYYYMMDD              TO D01-A15
              MOVE RC-EOM-UPDATED-DATE        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

      *     IF (ORC-EOM-UPDATED-TIME NOT NUMERIC)
      *        MOVE 0  TO ORC-EOM-UPDATED-TIME.
           IF ENT-GROUP = "N" AND BEG-BRANCH = 0
            AND END-BRANCH = 9999
              IF ORC-EOM-UPDATED-TIME NOT = RC-EOM-UPDATED-TIME
                 MOVE "RC-EOM-UPDATED-TIME" TO D01-TEXT
                 MOVE ORC-EOM-UPDATED-TIME  TO D01-A15-NUM
                 MOVE RC-EOM-UPDATED-TIME   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE ORC-EOM2-BATCH-DATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = RC-EOM2-BATCH-DATE
              MOVE "RC-EOM2-BATCH-DATE-DATE" TO D01-TEXT
              MOVE DATE-YYYYMMDD             TO D01-A15
              MOVE RC-EOM2-BATCH-DATE        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

      *     IF (ORC-EOM2-BATCH-TIME NOT NUMERIC)
      *        MOVE 0  TO ORC-EOM2-BATCH-TIME.
           IF ENT-GROUP = "N" AND BEG-BRANCH = 0
            AND END-BRANCH = 9999
              IF ORC-EOM2-BATCH-TIME NOT = RC-EOM2-BATCH-TIME
                 MOVE "RC-EOM2-BATCH-TIME" TO D01-TEXT
                 MOVE ORC-EOM2-BATCH-TIME  TO D01-A15-NUM
                 MOVE RC-EOM2-BATCH-TIME   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE ORC-CHECK-DATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = RC-CHECK-DATE
              MOVE "RC-CHECK-DATE-DATE" TO D01-TEXT
              MOVE DATE-YYYYMMDD        TO D01-A15
              MOVE RC-CHECK-DATE        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

      *     IF (ORC-CHECK-TIME NOT NUMERIC)
      *        MOVE 0  TO ORC-CHECK-TIME.
           IF ENT-GROUP = "N" AND BEG-BRANCH = 0
            AND END-BRANCH = 9999
              IF ORC-CHECK-TIME NOT = RC-CHECK-TIME
                 MOVE "RC-CHECK-TIME" TO D01-TEXT
                 MOVE ORC-CHECK-TIME  TO D01-A15-NUM
                 MOVE RC-CHECK-TIME   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF ORC-LAST-ACCRUE NOT NUMERIC
              MOVE 0  TO ORC-LAST-ACCRUE.
           IF ORC-LAST-ACCRUE NOT = RC-LAST-ACCRUE
              MOVE "RC-LAST-ACCRUE" TO D01-TEXT
              MOVE ORC-LAST-ACCRUE  TO D01-A15-NUM
              MOVE RC-LAST-ACCRUE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF ORC-ERROR-MSG NOT = RC-ERROR-MSG
              MOVE "RC-ERROR-MSG" TO D01-TEXT
              MOVE ORC-ERROR-MSG  TO D01-A15
              MOVE RC-ERROR-MSG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF (ORC-BANK-BAL NOT NUMERIC)
              MOVE 0  TO ORC-BANK-BAL.
           IF ORC-BANK-BAL NOT = RC-BANK-BAL
              MOVE "RC-BANK-BAL" TO D01-TEXT
              MOVE ORC-BANK-BAL  TO D01-A15-NUM
              MOVE RC-BANK-BAL   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF (ORC-CHECK-DEPOSIT NOT NUMERIC)
              MOVE 0  TO ORC-CHECK-DEPOSIT.
           IF ORC-CHECK-DEPOSIT NOT = RC-CHECK-DEPOSIT
              MOVE "RC-CHECK-DEPOSIT" TO D01-TEXT
              MOVE ORC-CHECK-DEPOSIT  TO D01-A15-NUM
              MOVE RC-CHECK-DEPOSIT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

       EXIT-COMPARE-FIELDS.
           EXIT.
       
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
       WRITE-DETAIL-LINE-ERROR SECTION.
      ******************************************************************

           ADD 1 TO TOTAL-ERRORS-CTR.
           PERFORM WRITE-DETAIL-LINE.

       WRITE-DETAIL-LINE-ERROR-EXIT.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE-ERROR-KEY SECTION.
      ******************************************************************

           MOVE BR-NO         TO D01-BRNO.
           MOVE RC-TRANS-DATE TO D01-DATE.

       WRITE-DETAIL-LINE-ERROR-KEY-EXIT.
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

           MOVE "BEGINNING DATE               : "    TO R01-PROMPT.
           MOVE BEG-DATE                            TO R01-NUMERIC.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    DATE               : "    TO R01-PROMPT.
           MOVE END-DATE                            TO R01-NUMERIC.
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
           MOVE EXT-CONAME-CONAME TO RCFILE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO RCFILE-SYSDATE.
           DISPLAY RCFILE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE RCFILE-FORM-FIELDS TO WR-BUF.
           MOVE RCFILE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE RCFILE-HELP--FILE TO HELP-LINK-FILE.
           MOVE RCFILE-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/RCFILE_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

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

           IF FIRST-SW = "Y"
              MOVE "LOADING WORK FILE" TO PROGID-TEXT
           ELSE
              MOVE SPACES TO PROGID-TEXT
              STRING "PROCESSING BRANCH: ", BR-NO INTO PROGID-TEXT.

           PERFORM GET-TIME.
           MOVE TIME-EDIT                  TO PROGID-TIME.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PROGID-DISPLAY.

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
       COPY "LIBGB/GBRCGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

      * COPY "LIBGB/GBRC1RN.CPY".
      * COPYMEMBER: LIBGB/GBRC1RN
       LOAD-RC1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( RC-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( RC-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO RC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO RC-ALL-MACHINE
              MOVE RC-ALL-PATH             TO RC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( RC-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO RC-ALL-BASE
              MOVE FILPATH-MACHINE         TO RC-ALL-MACHINE
              MOVE RC-ALL-PATH             TO RC-PATH.

      *-----------------------------------------------------------------
       OPEN-RC1-FILE.
           PERFORM LOAD-RC1-FILE.
           PERFORM OPEN-IT.
           MOVE RC-PATH-SQL TO E-FILE.

           IF RC1-CONN-OPEN-SW = "N" 
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS RC1_CONNECT
                        USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE 'Y' TO RC1-CONN-OPEN-SW.

      *-----------------------------------------------------------------
       START-RC1-FILE.    *> SSP  RCFILE
           PERFORM START-IT.
           MOVE RC-PATH-SQL TO E-FILE.
           MOVE RC1-KEY TO E-KEYX.

           IF ( RC1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-RC1-FILE.

      *      SELECT *
      *      FROM DBO.CHFILE
      *      WHERE RCFILE.RC_BRNO = @QRC_WBEG_BRNO
      *        AND RCFILE.RC_TRANS_DATE BETWEEN @QRC_WBEG_DATE AND @QRC_WEND_DATE
      *      ORDER BY RCFILE.RC_BRNO,
      *               RCFILE.RC_TRANS_DATE

           EXEC SQL SET CONNECTION RC1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE RC1_CURSOR CURSOR FOR
                :RC1-RETURN-CODE =  
            EXEC SSP_RCFILE_RCFILE_SELECT
              ( :QRC1-WBEG-BRNO, :QRC1-WBEG-TRANS-DATE
                 ,:QRC1-WEND-TRANS-DATE)
           END-EXEC.

           EXEC SQL
               OPEN RC1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           MOVE STAT---GOOD TO RC1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-RC1-FILE-NEXT.      *> SSP  RCFILE
           PERFORM READ-IT.
           MOVE RC-PATH-SQL TO E-FILE.
           INITIALIZE QRC-REC.

           EXEC SQL SET CONNECTION RC1_CONNECT END-EXEC.

           IF ( RC1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH RC1_CURSOR INTO :QRC-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-RC1-FILE-NEXT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           IF ( IO-FG = 0 )
              PERFORM GET-RC-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-RC1-FILE.
           PERFORM CLOSE-IT.

            IF ( RC1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION RC1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE RC1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO RC1-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-RC1-CONNECTION.

           IF RC1-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION RC1_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT RC1_CONNECT END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE "N" TO RC1-CONN-OPEN-SW.
      *-----------------------------------------------------------------

       OPEN-ORC1-FILE.
           PERFORM OPEN-IT.
           MOVE ORC-PATH TO E-FILE.
           OPEN INPUT ORC-FILE.
           IF IO-FG = 8
              GO TO OPEN-ORC1-FILE.
           IF IO-FG = 7
              CLOSE ORC-FILE
              GO TO OPEN-ORC1-FILE.
      *READ-ORC1-FILE.
      *    PERFORM READ-IT.
      *    MOVE ORC-PATH TO E-FILE.
      *    MOVE ORC1-KEY TO E-KEYX.
      *    READ ORC-FILE.
      *    IF IO-FG = 8
      *       GO TO READ-ORC1-FILE.
       START-ORC1-FILE.
           PERFORM START-IT.
           MOVE ORC-PATH TO E-FILE.
           MOVE ORC1-KEY TO E-KEYX.
           START ORC-FILE KEY NOT < ORC1-KEY.
       READ-ORC1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE ORC-PATH TO E-FILE.
           MOVE ORC1-KEY TO E-KEYX.
           READ ORC-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-ORC1-FILE-NEXT.
       CLOSE-ORC1-FILE.
           PERFORM CLOSE-IT.
           CLOSE ORC-FILE.

       OPEN-WK1-FILE.
           PERFORM OPEN-IT.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           OPEN I-O WK-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK1-FILE.
           IF IO-FG = 7
              CLOSE WK-FILE
              GO TO OPEN-WK1-FILE.
       OPEN-WK1-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           OPEN OUTPUT WK-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK1-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK-FILE
              GO TO OPEN-WK1-FILE-OUTPUT.
       START-WK1-FILE.
           PERFORM START-IT.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           MOVE WK-KEY  TO E-KEYX.
           START WK-FILE KEY NOT < WK-KEY.
           UNLOCK WK-FILE.
       READ-WK1-FILE.
           PERFORM READ-IT.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           MOVE WK-KEY  TO E-KEYX.
           READ WK-FILE
              INVALID KEY
                MOVE 1 TO IO-FG
           END-READ.
           IF IO-FG = 8
              GO TO READ-WK1-FILE.
       READ-WK1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           MOVE WK-KEY  TO E-KEYX.
           READ WK-FILE NEXT
              AT END  MOVE 9 TO IO-STATUS IO-FG
           END-READ.
           IF IO-FG = 8
              GO TO READ-WK1-FILE-NEXT.
       WRITE-WK1-FILE.
           PERFORM WRITE-IT.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           MOVE WK-KEY  TO E-KEYX.
           WRITE WK-REC 
             INVALID KEY 
                MOVE 22 TO IO-STATUS IO-FG
           END-WRITE.
           IF IO-FG = 8
             GO TO WRITE-WK1-FILE.
           UNLOCK WK-FILE.
       CLOSE-WK1-FILE.
           PERFORM CLOSE-IT.
           MOVE 0 TO IO-FG IO-STATUS.
           CLOSE WK-FILE.

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

           MOVE WK-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY RCFILE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
