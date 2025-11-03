       IDENTIFICATION DIVISION.
       PROGRAM-ID.    EAFILE.
       DATE-WRITTEN.  12-08-2023.
      ******************************************************************
      *
      * DESCRIPTION   : COMPARE S35 EAFILE WITH A15 EAFILE
      *            
      *                 TO BE USED AFTER AN UPDATE
      *                 WILL COMPARE EACH FIELD AND REPORT ANY 
      *                 DIFFERENCES, AS LONG AS FIELD EXIST IN BOTH FILES
      *
      *  LOOPS THRU THE A15 FILE AND VERIFIES THE S35 FILE EXIST AND MATCHES
      *
      *  01-07-25  RMZ  ADDED 'BTRACK' - PROCESS TRACKING TO PROGRAM.
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT OEA-FILE ASSIGN TO OEA-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OEA1-KEY
                  FILE STATUS FILE-STAT.

           SELECT OBR-FILE ASSIGN TO OBR-PATH
                  ORGANIZATION RELATIVE
                  ACCESS MODE DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RELATIVE KEY OBR-KEY
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
       FD  OEA-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A15SRC/LP01EA.CPY" REPLACING LEADING "EA" BY "OEA".

      * COPYMEMBER: LIBGB/GBFDBR
      *********************************************************
      *    BRANCH FILE RECORD    GBFDBR    798
      *
      **********************************************************
       FD  OBR-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A15SRC/GB01BR.CPY" REPLACING LEADING "BR" BY "OBR".

       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
              05  WK-KEY-BRANCH      PIC 9(4).
              05  WK-KEY-CLASS       PIC 9(3).
              05  WK-KEY-GROUP-ID    PIC X(6).
              05  WK-KEY-SEQNO       PIC 99.
           03  WK-DATA               PIC X(OEA-SIZE).

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/EAFILE S35 11/06/23-13:11:52 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01EA.CPY".
       COPY "LIBLP/LP01EA_SQL.CPY".
       COPY "LIBLP/LPWSEA.CPY".
       01  OEA-PATH                    PIC X(35) VALUE SPACES.
       01  OEA-ALL-PATH.
           03  OEA-ALL-BASE     PIC X(9).
           03  FILLER           PIC X     VALUE "/".
           03  OEA-ALL-MACHINE  PIC XX.
           03  FILLER           PIC X(10) VALUE
               "/LP/EAFILE".

       01  OBR-KEY       PIC 9(4)   VALUE 0.
       01  OBR-PATH. 
           03  OBR-ALL-BASE      PIC X(9).
           03  FILLER            PIC X     VALUE "/".
           03  OBR-ALL-MACHINE   PIC XX.
           03  FILLER            PIC X(10) VALUE
               "/GB/BRFILE".

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

       01  LITTLE-Z.
           03  FILLER           PIC X VALUE X"7A".
       01  HOLD-A15-ALPNAME            PIC X(3).
       01  FILLER REDEFINES HOLD-A15-ALPNAME.
           03  HOLD-A15-BYTE           PIC X(1) OCCURS 3 TIMES.

       01  HOLD-A30-ALPNAME            PIC X(4).
       01  FILLER REDEFINES HOLD-A30-ALPNAME.
           03  HOLD-A30-BYTE           PIC X(1) OCCURS 4 TIMES.

       01  MACHINE-R1.
           03  FILLER                  PIC X VALUE X"72".
           03  FILLER                  PIC 9 VALUE 1.
       01  MACHINE-R2.
           03  FILLER                  PIC X VALUE X"72".
           03  FILLER                  PIC 9 VALUE 2.

       01  R12-ARRAY                    OCCURS 9999 TIMES  
                                       PIC XX.

       01  EA-EOF-SW                   PIC X        VALUE "N".
       01  OEA-EOF-SW                  PIC X        VALUE "N".
       01  TOTAL-ERRORS-CTR            PIC 9(9)     VALUE 0.
       01  EA1-CONN-OPEN-SW            PIC X        VALUE "N".
       01  WK-EOF-SW                   PIC X        VALUE "N".
       01  FIRST-SW                    PIC X        VALUE "Y".

       01  MISC-CURSOR-STAT            PIC X(2) VALUE SPACES.
           88  MISC-CURSOR-STAT-GOOD            VALUE "00".
           88  MISC-CURSOR-STAT-BAD             VALUE "  " THRU "//"
                                                      "01" THRU "~~".

       01  PROCESS-STAT                PIC X(08)    VALUE SPACES.
           88  PROCESS-STAT-GOOD                    VALUE "00".
           88  PROCESS-STAT-BAD                     VALUE "  " THRU "//"
                                                         "01" THRU "~~".

       01  A15-DUP-RECORDS-PROCESSED   PIC 9(9) VALUE 0.
       01  SQL-RECORDS-PROCESSED       PIC 9(9) VALUE 0.
       01  A15-RECORDS-PROCESSED       PIC 9(9) VALUE 0.
           88  NO-RECORDS-FOUND                     VALUE ZEROES.

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
           03  BEG-CLASS               PIC 9(02).
           03  END-CLASS               PIC 9(02).
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
           03  H02-TITLE               PIC X(50) VALUE 
           "EXTRACTION ACTION (EAFILE) COMPARE A15 TO S35".

       01  HEADER-LINE-03.
           03  FILLER                PIC X(4) VALUE "BRNO".
           03  FILLER                PIC X(2) VALUE SPACES.
           03  FILLER                PIC X(5) VALUE "CLASS".
           03  FILLER                PIC X(2) VALUE SPACES.
           03  FILLER                PIC X(6) VALUE "GROUP".
           03  FILLER                PIC X(2) VALUE SPACES.
           03  FILLER                PIC X(3) VALUE "SEQ".
           03  FILLER                PIC X(2) VALUE SPACES.
           03  FILLER                PIC X(28) VALUE "DESCRIPTION".
           03  FILLER                PIC X(3) VALUE "A15".
           03  FILLER                PIC X(19) VALUE SPACES.
           03  FILLER                PIC X(3) VALUE "S35".

      ******************************************************************
      *
      *    D E T A I L - L I N E - W O R K E R S
      *
      ******************************************************************
       01  DETAIL-LINE                        PIC X(100)   VALUE SPACES.

       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-BRNO                       PIC Z(3)9.
           03  FILLER                         PIC XX.
           03  D01-CLASS                      PIC Z(4)9.
           03  FILLER                         PIC XX.
           03  D01-GROUP-ID                   PIC X(6).
           03  FILLER                         PIC XX.
           03  D01-SEQNO                      PIC ZZ9.
           03  FILLER                         PIC XX.
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
       COPY "LIBUT/EAFILE_DEF.CPY".
       COPY "LIBUT/EAFILE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/EAFILE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE OEA-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-EA1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-WK1-FILE.
           PERFORM CLOSE-PR-FILE.
           PERFORM CLOSE-EA1-CONNECTION.
           CLOSE OEA-FILE WK-FILE.
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
           MOVE "EAFILE" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE EAFILE-QUEUE-POS TO Q-POS.
           MOVE EAFILE-COPIES-POS TO C-POS.
           MOVE EAFILE-PRU-POS   TO P-POS.
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

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WK-PATH.

           MOVE SOURCE-PATH   TO OBR-ALL-BASE.
           MOVE MACHINE-R1    TO OBR-ALL-MACHINE.
           PERFORM OPEN-OBR-FILE.
           PERFORM READ-OBR-FILE-NEXT.
           PERFORM UNTIL IO-BAD
              MOVE OBR-MACHINE TO R12-ARRAY(OBR-NO)
              PERFORM READ-OBR-FILE-NEXT
           END-PERFORM.
           PERFORM CLOSE-OBR-FILE.

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
              GO TO ENTER-END-CLASS.

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


       ENTER-BEG-CLASS.

           MOVE "ENNN020BEG-CLASS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F3-KEY ) GO TO ENTER-ALL-CLASS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-CLASS.

           MOVE VDUNUM0 TO BEG-CLASS.

      *-----------------------------------------------------------------
       ENTER-END-CLASS.

           MOVE "ENNN020END-CLASS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-CLASS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-CLASS.

           MOVE VDUNUM0 TO END-CLASS.

           IF ( BEG-CLASS > END-CLASS )
              MOVE "INVALID CLASS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-CLASS.

           GO TO ENTER-PARAMETERS-END.

      *-----------------------------------------------------------------
       ENTER-ALL-CLASS.

           MOVE "SNNN020BEG-CLASS."  TO VDU.
           MOVE ZEROES                TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020END-CLASS."  TO VDU.
           MOVE 99                    TO END-CLASS VDUNUM0.
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

           MOVE "SNNN020BEG-CLASS."       TO VDU.
           MOVE BEG-CLASS                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020END-CLASS."       TO VDU.
           MOVE END-CLASS                 TO VDUNUM0.
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

      * IF FIRST-SW = "Y" A15 FILE ERROR OCCURRED
           IF FIRST-SW = "Y"
              GO TO EXECUTION-MODULE-EXIT.

           GO TO EXECUTION-NEXT-GROUP.

       EXECUTION-MODULE-EXIT.
           EXIT.

     *********************************************************
       PROCESS-BRANCH-DRIVER SECTION.
     *********************************************************

           MOVE 9999                       TO PROGID-SLEEP-COUNTER.
           PERFORM SEND-PROGRESS-INDICATOR.

      D    STOP MESS.

           MOVE "N"  TO EA-EOF-SW OEA-EOF-SW WK-EOF-SW.
           MOVE "B"  TO EA-PATH-OVERRIDE.

           MOVE BR-NO       TO QEA1-WBEG-BRANCH.
           MOVE BEG-CLASS   TO QEA1-WBEG-CLASS.
           MOVE END-CLASS   TO QEA1-WEND-CLASS.

           MOVE "PROCESSING S35 DATA" TO BTRACK-MESS.
           PERFORM BTRACK-GLOBAL.

           PERFORM OPEN-EA1-FILE.
           PERFORM START-EA1-FILE.
           PERFORM M-READ-EA.

      * LOAD OEA FILES INTO WORKFILE  
           IF FIRST-SW = "Y"
              PERFORM OPEN-WK1-FILE-OUTPUT
              PERFORM CLOSE-WK1-FILE
              PERFORM OPEN-WK1-FILE
              MOVE SOURCE-PATH  TO OEA-ALL-BASE
              MOVE MACHINE-R1   TO OEA-ALL-MACHINE
              MOVE OEA-ALL-PATH TO OEA-PATH ACCESS-BUF
              PERFORM ACCESS-CALL
              IF ( STAT-BAD )
                 MOVE "MISSING A15 R1 FILE" TO D01-TEXT 
                 MOVE OEA-ALL-PATH          TO D01-A15
                 MOVE SPACES                TO D01-S35
                 PERFORM WRITE-DETAIL-LINE
                 GO TO PROCESS-BRANCH-DRIVER-END
              END-IF
              MOVE "N" TO OEA-EOF-SW
              PERFORM OPEN-OEA1-FILE
              PERFORM LOAD-WORK-FILE
              PERFORM CLOSE-OEA1-FILE 
              MOVE SOURCE-PATH  TO OEA-ALL-BASE
              MOVE MACHINE-R2   TO OEA-ALL-MACHINE
              MOVE OEA-ALL-PATH TO OEA-PATH ACCESS-BUF
              PERFORM ACCESS-CALL
              IF ( STAT-BAD )
                 MOVE "MISSING A15 R2 FILE" TO D01-TEXT 
                 MOVE OEA-ALL-PATH          TO D01-A15
                 MOVE SPACES                TO D01-S35
                 PERFORM WRITE-DETAIL-LINE
                 GO TO PROCESS-BRANCH-DRIVER-END
              END-IF
              MOVE "N" TO OEA-EOF-SW
              PERFORM OPEN-OEA1-FILE
              PERFORM LOAD-WORK-FILE
              PERFORM CLOSE-OEA1-FILE
              PERFORM CLOSE-WK1-FILE
              PERFORM OPEN-WK1-FILE
              MOVE "N" TO FIRST-SW
           END-IF.

           MOVE "PROCESSING A15 DATA" TO BTRACK-MESS.  
           PERFORM BTRACK-GLOBAL.

           MOVE BR-NO     TO WK-KEY-BRANCH.
           MOVE BEG-CLASS TO WK-KEY-CLASS.
           MOVE SPACES    TO WK-KEY-GROUP-ID.
           MOVE 0         TO WK-KEY-SEQNO.

           PERFORM START-WK1-FILE.
           PERFORM M-READ-WK.

           PERFORM PROCESS-BRANCH-NEXT-EA
              UNTIL EA-EOF-SW = "Y" AND WK-EOF-SW = "Y".

           PERFORM CLOSE-EA1-FILE.

           GO TO PROCESS-BRANCH-DRIVER-END.

       PROCESS-BRANCH-NEXT-EA.
           PERFORM SEND-PROGRESS-INDICATOR.

           PERFORM COMPARE-FIELDS.

           IF WK-KEY < EA1-KEY
              PERFORM M-READ-WK
           ELSE
              IF WK-KEY > EA1-KEY
                 PERFORM M-READ-EA
              ELSE
                 PERFORM M-READ-WK
                 PERFORM M-READ-EA.

       PROCESS-BRANCH-DRIVER-END.
           EXIT.

       LOAD-WORK-FILE SECTION.
           PERFORM M-READ-OEA.

           IF OEA-EOF-SW = "Y"
              GO TO LOAD-WORK-FILE-END.

           INITIALIZE WK-REC.

           MOVE OEA-BRANCH   TO WK-KEY-BRANCH.
           MOVE OEA-CLASS    TO WK-KEY-CLASS.
           MOVE OEA-GROUP-ID TO WK-KEY-GROUP-ID.
           MOVE OEA-SEQNO    TO WK-KEY-SEQNO.
           MOVE OEA-REC      TO WK-DATA.
           PERFORM WRITE-WK1-FILE.

           IF IO-STATUS = 9
              IF R12-ARRAY(WK-KEY-BRANCH) = MACHINE-R2
                 ADD 1 TO A15-DUP-RECORDS-PROCESSED
                 PERFORM REWRITE-WK1-FILE.

           GO TO LOAD-WORK-FILE.

       LOAD-WORK-FILE-END.
           EXIT. 

       M-READ-WK SECTION.
           PERFORM READ-WK1-FILE-NEXT.

           IF IO-BAD OR WK-KEY-BRANCH > BR-NO
              MOVE ALL '9' TO WK-KEY
              MOVE "Y"     TO WK-EOF-SW
           ELSE
              MOVE WK-DATA     TO OEA-REC.

       EXIT-M-READ-WK.
           EXIT.


       M-READ-EA SECTION.
           PERFORM READ-EA1-FILE-NEXT.    *> SSP EA
           IF IO-BAD
              MOVE ALL '9' TO EA1-KEY
              MOVE "Y"     TO EA-EOF-SW
           ELSE
              ADD 1 TO SQL-RECORDS-PROCESSED.

       EXIT-M-READ-EA.
           EXIT.

       M-READ-OEA SECTION.
           PERFORM READ-OEA1-FILE-NEXT.
           IF IO-BAD
              MOVE "Y"      TO OEA-EOF-SW
           ELSE
           IF OEA-BRANCH NOT NUMERIC OR OEA-CLASS NOT NUMERIC
               OR OEA-SEQNO NOT NUMERIC
              GO TO M-READ-OEA
           END-IF
           IF (OEA-CLASS < BEG-CLASS)   OR (OEA-CLASS > END-CLASS )
              GO TO M-READ-OEA
           ELSE 
              ADD 1 TO A15-RECORDS-PROCESSED.

       EXIT-M-READ-OEA.
           EXIT.

      ******************************************************************
       COMPARE-FIELDS SECTION.
      ******************************************************************

            IF WK-KEY < EA1-KEY
              MOVE "MISSING S35 RECORD" TO D01-TEXT
              MOVE BR-NO                TO D01-BRNO
              MOVE OEA-CLASS            TO D01-CLASS
              MOVE OEA-GROUP-ID         TO D01-GROUP-ID
              MOVE OEA-SEQNO            TO D01-SEQNO
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.

           IF WK-KEY > EA1-KEY
              MOVE "MISSING A15 RECORD" TO D01-TEXT
              MOVE BR-NO                TO D01-BRNO
              MOVE EA-CLASS             TO D01-CLASS
              MOVE EA-GROUP-ID          TO D01-GROUP-ID
              MOVE EA-SEQNO             TO D01-SEQNO
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.

           IF EA-DESC NOT = OEA-DESC
      *       CALL "C$JUSTIFY" USING OEA-DESC, "L"
              IF EA-DESC NOT = OEA-DESC
                 MOVE "EA-DESC"    TO D01-TEXT
                 MOVE BR-NO        TO D01-BRNO
                 MOVE EA-CLASS     TO D01-CLASS
                 MOVE EA-GROUP-ID  TO D01-GROUP-ID
                 MOVE EA-SEQNO     TO D01-SEQNO
                 MOVE OEA-DESC     TO D01-A15
                 MOVE EA-DESC      TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OEA-PASTDUE-DAY1 NOT NUMERIC
              MOVE 0 TO OEA-PASTDUE-DAY1.
           IF OEA-PASTDUE-DAY1 NOT = EA-PASTDUE-DAY1
              MOVE "EA-PASTDUE-DAY1" TO D01-TEXT
              MOVE BR-NO             TO D01-BRNO
              MOVE EA-CLASS          TO D01-CLASS
              MOVE EA-GROUP-ID       TO D01-GROUP-ID
              MOVE EA-SEQNO          TO D01-SEQNO
              MOVE OEA-PASTDUE-DAY1  TO D01-A15-NUM
              MOVE EA-PASTDUE-DAY1   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OEA-PASTDUE-DAY2 NOT NUMERIC
              MOVE 0 TO OEA-PASTDUE-DAY2.
           IF OEA-PASTDUE-DAY2 NOT = EA-PASTDUE-DAY2
              MOVE "EA-PASTDUE-DAY2" TO D01-TEXT
              MOVE BR-NO             TO D01-BRNO
              MOVE EA-CLASS          TO D01-CLASS
              MOVE EA-GROUP-ID       TO D01-GROUP-ID
              MOVE EA-SEQNO          TO D01-SEQNO
              MOVE OEA-PASTDUE-DAY2  TO D01-A15-NUM
              MOVE EA-PASTDUE-DAY2   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OEA-PASTDUE-AMT1 NOT NUMERIC
              MOVE 0 TO OEA-PASTDUE-AMT1.
           IF OEA-PASTDUE-AMT1 NOT = EA-PASTDUE-AMT1
              MOVE "EA-PASTDUE-AMT1" TO D01-TEXT
              MOVE BR-NO             TO D01-BRNO
              MOVE EA-CLASS          TO D01-CLASS
              MOVE EA-GROUP-ID       TO D01-GROUP-ID
              MOVE EA-SEQNO          TO D01-SEQNO
              MOVE OEA-PASTDUE-AMT1  TO D01-A15-NUM
              MOVE EA-PASTDUE-AMT1   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OEA-PASTDUE-AMT2 NOT NUMERIC
              MOVE 0 TO OEA-PASTDUE-AMT2.
           IF OEA-PASTDUE-AMT2 NOT = EA-PASTDUE-AMT2
              MOVE "EA-PASTDUE-AMT2" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE EA-CLASS       TO D01-CLASS
              MOVE EA-GROUP-ID    TO D01-GROUP-ID
              MOVE EA-SEQNO       TO D01-SEQNO
              MOVE OEA-PASTDUE-AMT2    TO D01-A15-NUM
              MOVE EA-PASTDUE-AMT2     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OEA-ACCT-BAL1 NOT NUMERIC
              MOVE 0 TO OEA-ACCT-BAL1.
           IF OEA-ACCT-BAL1 NOT = EA-ACCT-BAL1
              MOVE "EA-ACCT-BAL1" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE EA-CLASS       TO D01-CLASS
              MOVE EA-GROUP-ID    TO D01-GROUP-ID
              MOVE EA-SEQNO       TO D01-SEQNO
              MOVE OEA-ACCT-BAL1    TO D01-A15-NUM
              MOVE EA-ACCT-BAL1     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OEA-ACCT-BAL2 NOT NUMERIC
              MOVE 0 TO OEA-ACCT-BAL2.
           IF OEA-ACCT-BAL2 NOT = EA-ACCT-BAL2
              MOVE "EA-ACCT-BAL2" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE EA-CLASS       TO D01-CLASS
              MOVE EA-GROUP-ID    TO D01-GROUP-ID
              MOVE EA-SEQNO       TO D01-SEQNO
              MOVE OEA-ACCT-BAL2    TO D01-A15-NUM
              MOVE EA-ACCT-BAL2     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF EA-MATURED NOT = OEA-MATURED
                 MOVE "EA-MATURED" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE EA-CLASS       TO D01-CLASS
                 MOVE EA-GROUP-ID    TO D01-GROUP-ID
                 MOVE EA-SEQNO       TO D01-SEQNO
                 MOVE OEA-MATURED  TO D01-A15
                 MOVE EA-MATURED   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           IF EA-PROMISED NOT = OEA-PROMISED
                 MOVE "EA-PROMISED" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE EA-CLASS       TO D01-CLASS
                 MOVE EA-GROUP-ID    TO D01-GROUP-ID
                 MOVE EA-SEQNO       TO D01-SEQNO
                 MOVE OEA-PROMISED  TO D01-A15
                 MOVE EA-PROMISED   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           IF EA-ACTION NOT = OEA-ACTION
      *       CALL "C$JUSTIFY" USING OEA-ACTION, "L"
              IF EA-ACTION NOT = OEA-ACTION
                 MOVE "EA-ACTION" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE EA-CLASS       TO D01-CLASS
                 MOVE EA-GROUP-ID    TO D01-GROUP-ID
                 MOVE EA-SEQNO       TO D01-SEQNO
                 MOVE OEA-ACTION  TO D01-A15
                 MOVE EA-ACTION   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           MOVE OEA-LSTFM-DATE1 TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = EA-LSTFM-DATE1
              MOVE "EA-LSTFM-DATE1" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE EA-CLASS       TO D01-CLASS
              MOVE EA-GROUP-ID    TO D01-GROUP-ID
              MOVE EA-SEQNO       TO D01-SEQNO
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE EA-LSTFM-DATE1    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           MOVE OEA-LSTFM-DATE2 TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = EA-LSTFM-DATE2
              MOVE "EA-LSTFM-DATE2" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE EA-CLASS       TO D01-CLASS
              MOVE EA-GROUP-ID    TO D01-GROUP-ID
              MOVE EA-SEQNO       TO D01-SEQNO
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE EA-LSTFM-DATE2    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OEA-LSTFM-NO1 NOT NUMERIC
              MOVE 0 TO OEA-LSTFM-NO1.
           IF OEA-LSTFM-NO1 NOT = EA-LSTFM-NO1
              MOVE "EA-LSTFM-NO1" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE EA-CLASS       TO D01-CLASS
              MOVE EA-GROUP-ID    TO D01-GROUP-ID
              MOVE EA-SEQNO       TO D01-SEQNO
              MOVE OEA-LSTFM-NO1    TO D01-A15-NUM
              MOVE EA-LSTFM-NO1     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OEA-LSTFM-NO2 NOT NUMERIC
              MOVE 0 TO OEA-LSTFM-NO2.
           IF OEA-LSTFM-NO2 NOT = EA-LSTFM-NO2
              MOVE "EA-LSTFM-NO2" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE EA-CLASS       TO D01-CLASS
              MOVE EA-GROUP-ID    TO D01-GROUP-ID
              MOVE EA-SEQNO       TO D01-SEQNO
              MOVE OEA-LSTFM-NO2    TO D01-A15-NUM
              MOVE EA-LSTFM-NO2     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OEA-PASTPROM-DAY1 NOT NUMERIC
              MOVE 0 TO OEA-PASTPROM-DAY1.
           IF OEA-PASTPROM-DAY1 NOT = EA-PASTPROM-DAY1
              MOVE "EA-PASTPROM-DAY1" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE EA-CLASS       TO D01-CLASS
              MOVE EA-GROUP-ID    TO D01-GROUP-ID
              MOVE EA-SEQNO       TO D01-SEQNO
              MOVE OEA-PASTPROM-DAY1    TO D01-A15-NUM
              MOVE EA-PASTPROM-DAY1     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OEA-PASTPROM-DAY2 NOT NUMERIC
              MOVE 0 TO OEA-PASTPROM-DAY2.
           IF OEA-PASTPROM-DAY2 NOT = EA-PASTPROM-DAY2
              MOVE "EA-PASTPROM-DAY2" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE EA-CLASS       TO D01-CLASS
              MOVE EA-GROUP-ID    TO D01-GROUP-ID
              MOVE EA-SEQNO       TO D01-SEQNO
              MOVE OEA-PASTPROM-DAY2    TO D01-A15-NUM
              MOVE EA-PASTPROM-DAY2     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OEA-RECENCY-DAY1 NOT NUMERIC
              MOVE 0 TO OEA-RECENCY-DAY1.
           IF OEA-RECENCY-DAY1 NOT = EA-RECENCY-DAY1
              MOVE "EA-RECENCY-DAY1" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE EA-CLASS       TO D01-CLASS
              MOVE EA-GROUP-ID    TO D01-GROUP-ID
              MOVE EA-SEQNO       TO D01-SEQNO
              MOVE OEA-RECENCY-DAY1    TO D01-A15-NUM
              MOVE EA-RECENCY-DAY1     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OEA-RECENCY-DAY2 NOT NUMERIC
              MOVE 0 TO OEA-RECENCY-DAY2.
           IF OEA-RECENCY-DAY2 NOT = EA-RECENCY-DAY2
              MOVE "EA-RECENCY-DAY2" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE EA-CLASS       TO D01-CLASS
              MOVE EA-GROUP-ID    TO D01-GROUP-ID
              MOVE EA-SEQNO       TO D01-SEQNO
              MOVE OEA-RECENCY-DAY2    TO D01-A15-NUM
              MOVE EA-RECENCY-DAY2     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

      * ALPNAME1 AND 2 WENT FROM X(3) TO X(4).
           IF OEA-ALPNAME1 NOT = SPACES
              MOVE OEA-ALPNAME1       TO HOLD-A15-ALPNAME
              PERFORM CONVERT-A15-A30-ALPNAME
              IF HOLD-A30-ALPNAME NOT = EA-ALPNAME1
                 MOVE "EA-ALPNAME1" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE EA-CLASS       TO D01-CLASS
                 MOVE EA-GROUP-ID    TO D01-GROUP-ID
                 MOVE EA-SEQNO       TO D01-SEQNO
                 MOVE OEA-ALPNAME1  TO D01-A15
                 MOVE EA-ALPNAME1   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OEA-ALPNAME2 NOT = SPACES
              MOVE OEA-ALPNAME2 TO HOLD-A15-ALPNAME
              PERFORM CONVERT-A15-A30-ALPNAME
              IF HOLD-A30-ALPNAME NOT = EA-ALPNAME2
                 MOVE "EA-ALPNAME2" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE EA-CLASS       TO D01-CLASS
                 MOVE EA-GROUP-ID    TO D01-GROUP-ID
                 MOVE EA-SEQNO       TO D01-SEQNO
                 MOVE OEA-ALPNAME2  TO D01-A15
                 MOVE EA-ALPNAME2   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           IF EA-RECENCYDATE NOT = OEA-RECENCYDATE
                 MOVE "EA-RECENCYDATE" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE EA-CLASS       TO D01-CLASS
                 MOVE EA-GROUP-ID    TO D01-GROUP-ID
                 MOVE EA-SEQNO       TO D01-SEQNO
                 MOVE OEA-RECENCYDATE  TO D01-A15
                 MOVE EA-RECENCYDATE   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OEA-DLNO1 NOT NUMERIC
              MOVE 0 TO OEA-DLNO1.
           IF OEA-DLNO1 NOT = EA-DLNO1
              MOVE "EA-DLNO1" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE EA-CLASS       TO D01-CLASS
              MOVE EA-GROUP-ID    TO D01-GROUP-ID
              MOVE EA-SEQNO       TO D01-SEQNO
              MOVE OEA-DLNO1    TO D01-A15-NUM
              MOVE EA-DLNO1     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OEA-DLNO2 NOT NUMERIC
              MOVE 0 TO OEA-DLNO2.
           IF OEA-DLNO2 NOT = EA-DLNO2
              MOVE "EA-DLNO2" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE EA-CLASS       TO D01-CLASS
              MOVE EA-GROUP-ID    TO D01-GROUP-ID
              MOVE EA-SEQNO       TO D01-SEQNO
              MOVE OEA-DLNO2    TO D01-A15-NUM
              MOVE EA-DLNO2     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OEA-CONTRACTUAL-DAY1 NOT NUMERIC
              MOVE 0 TO OEA-CONTRACTUAL-DAY1.
           IF OEA-CONTRACTUAL-DAY1 NOT = EA-CONTRACTUAL-DAY1
              MOVE "EA-CONTRACTUAL-DAY1" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE EA-CLASS       TO D01-CLASS
              MOVE EA-GROUP-ID    TO D01-GROUP-ID
              MOVE EA-SEQNO       TO D01-SEQNO
              MOVE OEA-CONTRACTUAL-DAY1    TO D01-A15-NUM
              MOVE EA-CONTRACTUAL-DAY1     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OEA-CONTRACTUAL-DAY2 NOT NUMERIC
              MOVE 0 TO OEA-CONTRACTUAL-DAY2.
           IF OEA-CONTRACTUAL-DAY2 NOT = EA-CONTRACTUAL-DAY2
              MOVE "EA-CONTRACTUAL-DAY2" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE EA-CLASS       TO D01-CLASS
              MOVE EA-GROUP-ID    TO D01-GROUP-ID
              MOVE EA-SEQNO       TO D01-SEQNO
              MOVE OEA-CONTRACTUAL-DAY2    TO D01-A15-NUM
              MOVE EA-CONTRACTUAL-DAY2     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OEA-SETFM-NO NOT NUMERIC
              MOVE 0 TO OEA-SETFM-NO.
           IF OEA-SETFM-NO NOT = EA-SETFM-NO
              MOVE "EA-SETFM-NO" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE EA-CLASS       TO D01-CLASS
              MOVE EA-GROUP-ID    TO D01-GROUP-ID
              MOVE EA-SEQNO       TO D01-SEQNO
              MOVE OEA-SETFM-NO    TO D01-A15-NUM
              MOVE EA-SETFM-NO     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF EA-SETACT-CD NOT = OEA-SETACT-CD
      *       CALL "C$JUSTIFY" USING OEA-SETACT-CD, "L"
              IF EA-SETACT-CD NOT = OEA-SETACT-CD
                 MOVE "EA-SETACT-CD" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE EA-CLASS       TO D01-CLASS
                 MOVE EA-GROUP-ID    TO D01-GROUP-ID
                 MOVE EA-SEQNO       TO D01-SEQNO
                 MOVE OEA-SETACT-CD  TO D01-A15
                 MOVE EA-SETACT-CD   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           IF EA-INC-CNT NOT = OEA-INC-CNT
                 MOVE "EA-INC-CNT" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE EA-CLASS       TO D01-CLASS
                 MOVE EA-GROUP-ID    TO D01-GROUP-ID
                 MOVE EA-SEQNO       TO D01-SEQNO
                 MOVE OEA-INC-CNT  TO D01-A15
                 MOVE EA-INC-CNT   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OEA-INC-PROMISED NOT NUMERIC
              MOVE 0 TO OEA-INC-PROMISED.
           IF OEA-INC-PROMISED NOT = EA-INC-PROMISED
              MOVE "EA-INC-PROMISED" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE EA-CLASS       TO D01-CLASS
              MOVE EA-GROUP-ID    TO D01-GROUP-ID
              MOVE EA-SEQNO       TO D01-SEQNO
              MOVE OEA-INC-PROMISED    TO D01-A15-NUM
              MOVE EA-INC-PROMISED     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
              IF OEA-SETFM(SUB) NOT NUMERIC
                 MOVE 0 TO OEA-SETFM(SUB)
              END-IF
              IF OEA-SETFM(SUB) NOT = EA-SETFM(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "EA-SETFM", SUB INTO D01-TEXT
                 MOVE BR-NO       TO D01-BRNO
                 MOVE EA-CLASS       TO D01-CLASS
                 MOVE EA-GROUP-ID    TO D01-GROUP-ID
                 MOVE EA-SEQNO       TO D01-SEQNO
                 MOVE OEA-SETFM(SUB)    TO D01-A15-NUM
                 MOVE EA-SETFM(SUB)     TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.
           IF EA-SET-LN-COL NOT = OEA-SET-LN-COL
                 MOVE "EA-SET-LN-COL" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE EA-CLASS       TO D01-CLASS
                 MOVE EA-GROUP-ID    TO D01-GROUP-ID
                 MOVE EA-SEQNO       TO D01-SEQNO
                 MOVE OEA-SET-LN-COL  TO D01-A15
                 MOVE EA-SET-LN-COL   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           IF EA-LN-COL-ID NOT = OEA-LN-COL-ID
                 MOVE "EA-LN-COL-ID" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE EA-CLASS       TO D01-CLASS
                 MOVE EA-GROUP-ID    TO D01-GROUP-ID
                 MOVE EA-SEQNO       TO D01-SEQNO
                 MOVE OEA-LN-COL-ID  TO D01-A15
                 MOVE EA-LN-COL-ID   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           IF EA-CQ-FG NOT = OEA-CQ-FG
                 MOVE "EA-CQ-FG" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE EA-CLASS       TO D01-CLASS
                 MOVE EA-GROUP-ID    TO D01-GROUP-ID
                 MOVE EA-SEQNO       TO D01-SEQNO
                 MOVE OEA-CQ-FG  TO D01-A15
                 MOVE EA-CQ-FG   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           IF EA-COL-POOL NOT = OEA-COL-POOL
      *       CALL "C$JUSTIFY" USING OEA-COL-POOL, "L"
              IF EA-COL-POOL NOT = OEA-COL-POOL
                 MOVE "EA-COL-POOL" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE EA-CLASS       TO D01-CLASS
                 MOVE EA-GROUP-ID    TO D01-GROUP-ID
                 MOVE EA-SEQNO       TO D01-SEQNO
                 MOVE OEA-COL-POOL  TO D01-A15
                 MOVE EA-COL-POOL   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF EA-CPOTS NOT = OEA-CPOTS
                 MOVE "EA-CPOTS" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE EA-CLASS       TO D01-CLASS
                 MOVE EA-GROUP-ID    TO D01-GROUP-ID
                 MOVE EA-SEQNO       TO D01-SEQNO
                 MOVE OEA-CPOTS  TO D01-A15
                 MOVE EA-CPOTS   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           IF EA-REPO NOT = OEA-REPO
                 MOVE "EA-REPO" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE EA-CLASS       TO D01-CLASS
                 MOVE EA-GROUP-ID    TO D01-GROUP-ID
                 MOVE EA-SEQNO       TO D01-SEQNO
                 MOVE OEA-REPO  TO D01-A15
                 MOVE EA-REPO   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           IF EA-PL NOT = OEA-PL
                 MOVE "EA-PL" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE EA-CLASS       TO D01-CLASS
                 MOVE EA-GROUP-ID    TO D01-GROUP-ID
                 MOVE EA-SEQNO       TO D01-SEQNO
                 MOVE OEA-PL  TO D01-A15
                 MOVE EA-PL   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           IF EA-BANKRUPT NOT = OEA-BANKRUPT
                 MOVE "EA-BANKRUPT" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE EA-CLASS       TO D01-CLASS
                 MOVE EA-GROUP-ID    TO D01-GROUP-ID
                 MOVE EA-SEQNO       TO D01-SEQNO
                 MOVE OEA-BANKRUPT  TO D01-A15
                 MOVE EA-BANKRUPT   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           IF EA-PL-IN-ACTIVE-BOTH NOT = OEA-PL-IN-ACTIVE-BOTH
                 MOVE "EA-PL-IN-ACTIVE-BOTH" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE EA-CLASS       TO D01-CLASS
                 MOVE EA-GROUP-ID    TO D01-GROUP-ID
                 MOVE EA-SEQNO       TO D01-SEQNO
                 MOVE OEA-PL-IN-ACTIVE-BOTH  TO D01-A15
                 MOVE EA-PL-IN-ACTIVE-BOTH   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           IF EA-CONFIDENTIAL-FG NOT = OEA-CONFIDENTIAL-FG
                 MOVE "EA-CONFIDENTIAL-FG" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE EA-CLASS       TO D01-CLASS
                 MOVE EA-GROUP-ID    TO D01-GROUP-ID
                 MOVE EA-SEQNO       TO D01-SEQNO
                 MOVE OEA-CONFIDENTIAL-FG  TO D01-A15
                 MOVE EA-CONFIDENTIAL-FG   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           IF EA-1ST-PMT-MISSES NOT = OEA-1ST-PMT-MISSES
                 MOVE "EA-1ST-PMT-MISSES" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE EA-CLASS       TO D01-CLASS
                 MOVE EA-GROUP-ID    TO D01-GROUP-ID
                 MOVE EA-SEQNO       TO D01-SEQNO
                 MOVE OEA-1ST-PMT-MISSES  TO D01-A15
                 MOVE EA-1ST-PMT-MISSES   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           IF EA-DEBTAID NOT = OEA-DEBTAID
                 MOVE "EA-DEBTAID" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE EA-CLASS       TO D01-CLASS
                 MOVE EA-GROUP-ID    TO D01-GROUP-ID
                 MOVE EA-SEQNO       TO D01-SEQNO
                 MOVE OEA-DEBTAID  TO D01-A15
                 MOVE EA-DEBTAID   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           IF EA-JUDGEMENT NOT = OEA-JUDGEMENT
                 MOVE "EA-JUDGEMENT" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE EA-CLASS       TO D01-CLASS
                 MOVE EA-GROUP-ID    TO D01-GROUP-ID
                 MOVE EA-SEQNO       TO D01-SEQNO
                 MOVE OEA-JUDGEMENT  TO D01-A15
                 MOVE EA-JUDGEMENT   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           IF EA-BROKEN-PROM-FG NOT = OEA-BROKEN-PROM-FG
                 MOVE "EA-BROKEN-PROM-FG" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE EA-CLASS       TO D01-CLASS
                 MOVE EA-GROUP-ID    TO D01-GROUP-ID
                 MOVE EA-SEQNO       TO D01-SEQNO
                 MOVE OEA-BROKEN-PROM-FG  TO D01-A15
                 MOVE EA-BROKEN-PROM-FG   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OEA-PROM-BREAK1 NOT NUMERIC
              MOVE 0 TO OEA-PROM-BREAK1.
           IF OEA-PROM-BREAK1 NOT = EA-PROM-BREAK1
              MOVE "EA-PROM-BREAK1" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE EA-CLASS       TO D01-CLASS
              MOVE EA-GROUP-ID    TO D01-GROUP-ID
              MOVE EA-SEQNO       TO D01-SEQNO
              MOVE OEA-PROM-BREAK1    TO D01-A15-NUM
              MOVE EA-PROM-BREAK1     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OEA-PROM-BREAK2 NOT NUMERIC
              MOVE 0 TO OEA-PROM-BREAK2.
           IF OEA-PROM-BREAK2 NOT = EA-PROM-BREAK2
              MOVE "EA-PROM-BREAK2" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE EA-CLASS       TO D01-CLASS
              MOVE EA-GROUP-ID    TO D01-GROUP-ID
              MOVE EA-SEQNO       TO D01-SEQNO
              MOVE OEA-PROM-BREAK2    TO D01-A15-NUM
              MOVE EA-PROM-BREAK2     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF EA-ORIGIN NOT = OEA-ORIGIN
      *       CALL "C$JUSTIFY" USING OEA-ORIGIN, "L"
              IF EA-ORIGIN NOT = OEA-ORIGIN
                 MOVE "EA-ORIGIN" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE EA-CLASS       TO D01-CLASS
                 MOVE EA-GROUP-ID    TO D01-GROUP-ID
                 MOVE EA-SEQNO       TO D01-SEQNO
                 MOVE OEA-ORIGIN  TO D01-A15
                 MOVE EA-ORIGIN   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

      * CLEANSE
      *     EA-PL-BEG-DATE         PIC 9(06)      COMP-3.
      *     EA-PL-END-DATE         PIC 9(06)      COMP-3.
           IF OEA-PL-BEG-DATE = 202020 OR 020202 OR
                   (OEA-PL-BEG-DATE NOT NUMERIC)
              MOVE 0                TO OEA-PL-BEG-DATE.
           MOVE OEA-PL-BEG-DATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = EA-PL-BEG-DATE
              MOVE "EA-PL-BEG-DATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE EA-CLASS       TO D01-CLASS
              MOVE EA-GROUP-ID    TO D01-GROUP-ID
              MOVE EA-SEQNO       TO D01-SEQNO
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE EA-PL-BEG-DATE    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OEA-PL-END-DATE = 202020 OR 020202 OR
                   (OEA-PL-END-DATE NOT NUMERIC)
              MOVE 0                TO OEA-PL-END-DATE.
           MOVE OEA-PL-END-DATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = EA-PL-END-DATE
              MOVE "EA-PL-END-DATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE EA-CLASS       TO D01-CLASS
              MOVE EA-GROUP-ID    TO D01-GROUP-ID
              MOVE EA-SEQNO       TO D01-SEQNO
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE EA-PL-END-DATE    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF EA-RPOTS NOT = OEA-RPOTS
                 MOVE "EA-RPOTS" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE EA-CLASS       TO D01-CLASS
                 MOVE EA-GROUP-ID    TO D01-GROUP-ID
                 MOVE EA-SEQNO       TO D01-SEQNO
                 MOVE OEA-RPOTS  TO D01-A15
                 MOVE EA-RPOTS   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF EA-MISPAYS NOT = OEA-MISPAYS
                 MOVE "EA-MISPAYS" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE EA-CLASS       TO D01-CLASS
                 MOVE EA-GROUP-ID    TO D01-GROUP-ID
                 MOVE EA-SEQNO       TO D01-SEQNO
                 MOVE OEA-MISPAYS  TO D01-A15
                 MOVE EA-MISPAYS   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF EA-ALLOTMENTS NOT = OEA-ALLOTMENTS
                 MOVE "EA-ALLOTMENTS" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE EA-CLASS       TO D01-CLASS
                 MOVE EA-GROUP-ID    TO D01-GROUP-ID
                 MOVE EA-SEQNO       TO D01-SEQNO
                 MOVE OEA-ALLOTMENTS  TO D01-A15
                 MOVE EA-ALLOTMENTS   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
      * CLEANSE
      *    EA-LNDATE-DAYS1        PIC 9(4)       COMP-6.
      *    EA-LNDATE-DAYS2        PIC 9(4)       COMP-6.

           IF OEA-LNDATE-DAYS1 = 2020 OR 0202 OR 
               OEA-LNDATE-DAYS1 NOT NUMERIC
              MOVE 0 TO OEA-LNDATE-DAYS1.
           IF OEA-LNDATE-DAYS1 NOT = EA-LNDATE-DAYS1
              MOVE "EA-LNDATE-DAYS1" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE EA-CLASS       TO D01-CLASS
              MOVE EA-GROUP-ID    TO D01-GROUP-ID
              MOVE EA-SEQNO       TO D01-SEQNO
              MOVE OEA-LNDATE-DAYS1    TO D01-A15-NUM
              MOVE EA-LNDATE-DAYS1     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OEA-LNDATE-DAYS2 = 2020 OR 0202 OR 
                OEA-LNDATE-DAYS2 NOT NUMERIC
              MOVE 9999 TO OEA-LNDATE-DAYS2.
           IF OEA-LNDATE-DAYS2 NOT = EA-LNDATE-DAYS2
              MOVE "EA-LNDATE-DAYS2" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE EA-CLASS       TO D01-CLASS
              MOVE EA-GROUP-ID    TO D01-GROUP-ID
              MOVE EA-SEQNO       TO D01-SEQNO
              MOVE OEA-LNDATE-DAYS2    TO D01-A15-NUM
              MOVE EA-LNDATE-DAYS2     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

      * CLEANSE
      *    EA-MATURE-DAYS1        PIC 9(4)       COMP-6.
      *    EA-MATURE-DAYS2        PIC 9(4)       COMP-6.
           IF OEA-MATURE-DAYS1 = 2020 OR 0202 OR
                OEA-MATURE-DAYS1 NOT NUMERIC
              MOVE 0 TO OEA-MATURE-DAYS1.
           IF OEA-MATURE-DAYS1 NOT = EA-MATURE-DAYS1
              MOVE "EA-MATURE-DAYS1" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE EA-CLASS       TO D01-CLASS
              MOVE EA-GROUP-ID    TO D01-GROUP-ID
              MOVE EA-SEQNO       TO D01-SEQNO
              MOVE OEA-MATURE-DAYS1    TO D01-A15-NUM
              MOVE EA-MATURE-DAYS1     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OEA-MATURE-DAYS2 = 2020 OR 0202 OR
                OEA-MATURE-DAYS2 NOT NUMERIC
              MOVE 9999 TO OEA-MATURE-DAYS2.
           IF OEA-MATURE-DAYS2 NOT = EA-MATURE-DAYS2
              MOVE "EA-MATURE-DAYS2" TO D01-TEXT
              MOVE BR-NO             TO D01-BRNO
              MOVE EA-CLASS          TO D01-CLASS
              MOVE EA-GROUP-ID       TO D01-GROUP-ID
              MOVE EA-SEQNO          TO D01-SEQNO
              MOVE OEA-MATURE-DAYS2  TO D01-A15-NUM
              MOVE EA-MATURE-DAYS2   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.


      * CLEANSE OEA-DAY-OF-MO-DUE1
      *         OEA-DAY-OF-MO-DUE2
           IF OEA-DAY-OF-MO-DUE1 = 20 OR 02 OR
                OEA-DAY-OF-MO-DUE1 NOT NUMERIC
              MOVE 1 TO OEA-DAY-OF-MO-DUE1.
           IF OEA-DAY-OF-MO-DUE1 NOT = EA-DAY-OF-MO-DUE1
              MOVE "EA-DAY-OF-MO-DUE1" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE EA-CLASS       TO D01-CLASS
              MOVE EA-GROUP-ID    TO D01-GROUP-ID
              MOVE EA-SEQNO       TO D01-SEQNO
              MOVE OEA-DAY-OF-MO-DUE1    TO D01-A15-NUM
              MOVE EA-DAY-OF-MO-DUE1     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OEA-DAY-OF-MO-DUE2 = 20 OR 02 OR
                OEA-DAY-OF-MO-DUE2 NOT NUMERIC
              MOVE 31 TO OEA-DAY-OF-MO-DUE2.
           IF OEA-DAY-OF-MO-DUE2 NOT = EA-DAY-OF-MO-DUE2
              MOVE "EA-DAY-OF-MO-DUE2" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE EA-CLASS       TO D01-CLASS
              MOVE EA-GROUP-ID    TO D01-GROUP-ID
              MOVE EA-SEQNO       TO D01-SEQNO
              MOVE OEA-DAY-OF-MO-DUE2    TO D01-A15-NUM
              MOVE EA-DAY-OF-MO-DUE2     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF EA-INCL-COLLECTOR NOT = OEA-INCL-COLLECTOR
      *       CALL "C$JUSTIFY" USING OEA-INCL-COLLECTOR, "L"
              IF EA-INCL-COLLECTOR NOT = OEA-INCL-COLLECTOR
                 MOVE "EA-INCL-COLLECTOR" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE EA-CLASS       TO D01-CLASS
                 MOVE EA-GROUP-ID    TO D01-GROUP-ID
                 MOVE EA-SEQNO       TO D01-SEQNO
                 MOVE OEA-INCL-COLLECTOR  TO D01-A15
                 MOVE EA-INCL-COLLECTOR   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF EA-BORR-STATE NOT = OEA-BORR-STATE
      *       CALL "C$JUSTIFY" USING OEA-BORR-STATE, "L"
              IF EA-BORR-STATE NOT = OEA-BORR-STATE
                 MOVE "EA-BORR-STATE" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE EA-CLASS       TO D01-CLASS
                 MOVE EA-GROUP-ID    TO D01-GROUP-ID
                 MOVE EA-SEQNO       TO D01-SEQNO
                 MOVE OEA-BORR-STATE  TO D01-A15
                 MOVE EA-BORR-STATE   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

       EXIT-COMPARE-FIELDS.
           EXIT.
       
      ******************************************************************
      * CONVERT "  A" TO "   A"
      *         "BAC" TO " BAC"
      * CANT LEAVE SPACES AT THE END IN A30
      ******************************************************************
       CONVERT-A15-A30-ALPNAME SECTION.

           MOVE HOLD-A15-ALPNAME TO HOLD-A30-ALPNAME.
          
      * "AAA" TO "AAAA"
           IF HOLD-A15-ALPNAME = "AAA"
              MOVE "AAAA" TO HOLD-A30-ALPNAME
              GO TO EXIT-CONVERT-A15-A30-ALPNAME.
      * "~~~" TO "~~~~"
           IF HOLD-A15-ALPNAME = "~~~"
              MOVE "~~~~" TO HOLD-A30-ALPNAME
              GO TO EXIT-CONVERT-A15-A30-ALPNAME.
      * "ZZZ" TO "ZZZZ"
           IF HOLD-A15-BYTE(1) = LITTLE-Z AND
               HOLD-A15-BYTE(2) = LITTLE-Z AND
                HOLD-A15-BYTE(3) = LITTLE-Z 
              MOVE LITTLE-Z TO HOLD-A30-BYTE(1)
                               HOLD-A30-BYTE(2)
                               HOLD-A30-BYTE(3)
                               HOLD-A30-BYTE(4)
              GO TO EXIT-CONVERT-A15-A30-ALPNAME.
      * "  A" TO "   A"
      * "  J" TO "   J"
      * " AH" TO "  AH"
           IF HOLD-A15-BYTE(1) = SPACES
              MOVE HOLD-A15-ALPNAME TO HOLD-A30-ALPNAME
              CALL "C$JUSTIFY" USING HOLD-A30-ALPNAME, "R"
              GO TO EXIT-CONVERT-A15-A30-ALPNAME.
 
      * IF ALL 3 BYTES ARE FILLED WITH OTHER TYPE LETTER COMBOS,
      * TRY TO FIGURE IT OUT, THEN JUST GIVE UP AND MOVE AND JUSTIFY
      * "A~~" TO "A~~~"
      * "BZZ" TO "BZZZ"
           IF HOLD-A15-BYTE(3) = LITTLE-Z
              MOVE LITTLE-Z  TO HOLD-A30-BYTE(4) 
           ELSE
           IF HOLD-A15-BYTE(3) = "~"
              MOVE "~" TO HOLD-A30-BYTE(4) 
           ELSE
              MOVE HOLD-A15-ALPNAME TO HOLD-A30-ALPNAME
              CALL "C$JUSTIFY" USING HOLD-A30-ALPNAME, "R".

       EXIT-CONVERT-A15-A30-ALPNAME.
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

           MOVE "BEGINNING CLASS               : "    TO R01-PROMPT.
           MOVE BEG-CLASS                            TO R01-NUMERIC.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    CLASS               : "    TO R01-PROMPT.
           MOVE END-CLASS                             TO R01-NUMERIC.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "A15 RECORDS PROCESSED         :"     TO R01-PROMPT.
           MOVE A15-RECORDS-PROCESSED                 TO R01-NUMERIC.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "A15 DUP RECORDS PROCESSED     :"     TO R01-PROMPT.
           MOVE A15-DUP-RECORDS-PROCESSED             TO R01-NUMERIC.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SQL RECORDS PROCESSED         :"     TO R01-PROMPT.
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
           MOVE EXT-CONAME-CONAME TO EAFILE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO EAFILE-SYSDATE.
           DISPLAY EAFILE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE EAFILE-FORM-FIELDS TO WR-BUF.
           MOVE EAFILE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE EAFILE-HELP--FILE TO HELP-LINK-FILE.
           MOVE EAFILE-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/EAFILE_EVA.CPY".
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
       COPY "LIBLP/LPEAGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

      * COPY "LIBLP/LPEA1RN.CPY".
      * COPYMEMBER: LIBLP/LPEA1RN
       LOAD-EA1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( EA-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( EA-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO EA-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO EA-ALL-MACHINE
              MOVE EA-ALL-PATH             TO EA-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( EA-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO EA-ALL-BASE
              MOVE FILPATH-MACHINE         TO EA-ALL-MACHINE
              MOVE EA-ALL-PATH             TO EA-PATH.

      *-----------------------------------------------------------------
       OPEN-EA1-FILE.
           PERFORM LOAD-EA1-FILE.
           PERFORM OPEN-IT.
           MOVE EA-PATH-SQL TO E-FILE.

           IF EA1-CONN-OPEN-SW = "N" 
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS EA1_CONNECT
                        USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE 'Y' TO EA1-CONN-OPEN-SW.

      *-----------------------------------------------------------------
       START-EA1-FILE.   *> SSP EA
           PERFORM START-IT.
           MOVE EA-PATH-SQL TO E-FILE.
           MOVE EA1-KEY TO E-KEYX.

           IF ( EA1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-EA1-FILE.

           EXEC SQL SET CONNECTION EA1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE EA1_CURSOR CURSOR FOR
                :EA1-RETURN-CODE =  
            EXEC SSP_EAFILE_EAFILE_SELECT
              ( :QEA1-WBEG-BRANCH,
                :QEA1-WBEG-CLASS, :QEA1-WEND-CLASS)
           END-EXEC.

      *     EXEC SQL
      *      DECLARE EA1_CURSOR CURSOR FOR
      *     SELECT *
      *     FROM DBO.EAFILE EAFILE
      *     WHERE EAFILE.EA_BRANCH = @QEA1_WBEG_BRNO
      *      AND EAFILE.EA_CLASS BETWEEN @QEA_WBEG_CLASS AND @QEA_WEND_CLASS
      *     ORDER BY ORDER BY EAFILE.EA_BRANCH, EAFILE.EA_CLASS, EAFILE.EA_GROUP_ID, EAFILE.EA_SEQNO
      *     END-EXEC.

           EXEC SQL
               OPEN EA1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE STAT---GOOD TO EA1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-EA1-FILE-NEXT.  *> SSP EA
           PERFORM READ-IT.
           MOVE EA-PATH-SQL TO E-FILE.
           MOVE EA1-KEY TO E-KEYX.
           INITIALIZE QEA-REC.

           EXEC SQL SET CONNECTION EA1_CONNECT END-EXEC.

           IF ( EA1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH EA1_CURSOR INTO 
                   :QEA-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-EA1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-EA-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-EA1-FILE.
           PERFORM CLOSE-IT.

            IF ( EA1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION EA1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE EA1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO EA1-CURSOR-STAT
              EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-EA1-CONNECTION.

           IF EA1-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION EA1_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT EA1_CONNECT END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE "N" TO EA1-CONN-OPEN-SW.
      *-----------------------------------------------------------------
       OPEN-OEA1-FILE.
           PERFORM OPEN-IT.
           MOVE OEA-PATH TO E-FILE.
           OPEN INPUT OEA-FILE.
           IF IO-FG = 8
              GO TO OPEN-OEA1-FILE.
           IF IO-FG = 7
              CLOSE OEA-FILE
              GO TO OPEN-OEA1-FILE.
      *READ-OEA1-FILE.
      *    PERFORM READ-IT.
      *    MOVE OEA-PATH TO E-FILE.
      *    MOVE OEA1-KEY TO E-KEYX.
      *    READ OEA-FILE.
      *    IF IO-FG = 8
      *       GO TO READ-OEA1-FILE.
       START-OEA1-FILE.
           PERFORM START-IT.
           MOVE OEA-PATH TO E-FILE.
           MOVE OEA1-KEY TO E-KEYX.
           START OEA-FILE KEY NOT < OEA1-KEY.
       READ-OEA1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OEA-PATH TO E-FILE.
           MOVE OEA1-KEY TO E-KEYX.
           READ OEA-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OEA1-FILE-NEXT.
       CLOSE-OEA1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OEA-FILE.

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
           MOVE WK-PATH TO E-FILE.
           MOVE WK-KEY  TO E-KEYX.
           START WK-FILE KEY NOT < WK-KEY.
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
                MOVE 9 TO IO-STATUS IO-FG
           END-WRITE.
           IF IO-FG = 8
             GO TO WRITE-WK1-FILE.
           UNLOCK WK-FILE.
       REWRITE-WK1-FILE.
           PERFORM REWRITE-IT.
           MOVE WK-PATH TO E-FILE.
           MOVE WK-KEY  TO E-KEYX.
           REWRITE WK-REC.
           IF IO-FG = 8
             GO TO REWRITE-WK1-FILE.
           UNLOCK WK-FILE.
       CLOSE-WK1-FILE.
           PERFORM CLOSE-IT.
           MOVE 0 TO IO-FG IO-STATUS.
           CLOSE WK-FILE.

      * COPYMEMBER: LIBGB/GBBRIN
       OPEN-OBR-FILE.
           PERFORM OPEN-IT.
           MOVE OBR-PATH TO E-FILE.
           OPEN I-O OBR-FILE.
           IF IO-FG = 8
              GO TO OPEN-OBR-FILE.
           IF IO-FG = 7
              CLOSE OBR-FILE
              GO TO OPEN-OBR-FILE.
           UNLOCK OBR-FILE.
       READ-OBR-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OBR-PATH TO E-FILE.
           MOVE OBR-KEY TO E-KEYX.
           READ OBR-FILE NEXT
              AT END  MOVE 9 TO IO-STATUS IO-FG
           END-READ.
           IF IO-FG = 8
              GO TO READ-OBR-FILE-NEXT.
       CLOSE-OBR-FILE.
           PERFORM CLOSE-IT.
           CLOSE OBR-FILE.

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

           MOVE WK-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY EAFILE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
