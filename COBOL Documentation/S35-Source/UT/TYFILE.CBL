       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TYFILE.
       DATE-WRITTEN.  10-31-2023.
      *********************************************************************
      *
      * DESCRIPTION   : COMPARE S35 TYFILE WITH A15 TYFILE
      *            
      *                 TO BE USED AFTER AN UPDATE
      *                 WILL COMPARE EACH FIELD AND REPORT ANY 
      *                 DIFFERENCES, AS LONG AS FIELD EXIST IN BOTH FILES
      *
      *  LOOPS THRU THE A15 FILE AND VERIFIES THE S35 FILE EXIST AND MATCHES
      *
      * 2024-1125 MB  CORRECT USE OF TY-PATH-OWNBR USE IN ERROR REPORTING.
      * 2025-0108 RZ  ADDED 'BTRACK' - PROCESS TRACKING TO PROGRAM.
      **********************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
      * COPYMEMBER: LIBLP/LPFSTY
           SELECT OTY-FILE ASSIGN TO OTY-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OTY1-KEY
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  OTY-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A15SRC/LP01TY.CPY" REPLACING LEADING "TY" BY "OTY".

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/TYFILE S35 12/02/24-09:54:27 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01TY.CPY".
       COPY "LIBLP/LP01TY_SQL.CPY".
       COPY "LIBLP/LPWSTY.CPY".
       01  OTY-PATH                    PIC X(35) VALUE SPACES.
       01  OTY-ALL-PATH.
           03  OTY-ALL-BASE     PIC X(9).
           03  FILLER           PIC X     VALUE "/".
           03  OTY-ALL-MACHINE  PIC XX.
           03  FILLER           PIC X     VALUE "/".
           03  OTY-ALL-DATA     PIC X(6).
           03  FILLER           PIC X(10) VALUE
               "/LP/TYFILE".

       01  TY1-CONN-OPEN-SW     PIC X     VALUE "N".

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

       01  MISC-CURSOR-STAT            PIC X(2) VALUE SPACES.
           88  MISC-CURSOR-STAT-GOOD            VALUE "00".
           88  MISC-CURSOR-STAT-BAD             VALUE "  " THRU "//"
                                                      "01" THRU "~~".

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

       01  SETTY-DATE          PIC 9(7)    VALUE 0.
       01  FILLER              REDEFINES SETTY-DATE.
           03  SETTY-YYY       PIC 999.
           03  SETTY-MM        PIC 99.
           03  SETTY-DD        PIC 99.

       01  PROCESS-STAT                PIC X(08)  VALUE SPACES.
           88  PROCESS-STAT-GOOD                  VALUE "00".
           88  PROCESS-STAT-BAD                   VALUE "  " THRU "//"
                                                        "01" THRU "~~".

       01  A15-FILE-FOUND-SW           PIC X      VALUE "N".
       01  S35-RECORD-CTR              PIC 9(9)   VALUE 0.
       01  TOTAL-ERRORS-CTR            PIC 9(9)   VALUE 0.
       01  A15-RECORDS-PROCESSED       PIC 9(9)   VALUE 0.
           88  NO-RECORDS-FOUND                   VALUE ZEROES.

       01  TY-EOF-SW                   PIC X      VALUE "N".
       01  OTY-EOF-SW                  PIC X      VALUE "N".
       01  WS-FIELD-NUM-A15            PIC S9(8)V99  VALUE 0.
       01  WS-FIELD-NUM-S35            PIC S9(8)V99  VALUE 0.

       01  WS-TY1-KEY.
           05  WS-TY-BRNO              PIC 9(4).
           05  WS-TY-DATE              PIC 9(8).
           05  WS-TY-CLASS             PIC 9(3).
 
       01  WS-OTY1-KEY.
           05  WS-OTY-BRNO             PIC 9(4).
           05  WS-OTY-DATE             PIC 9(8).
           05  WS-OTY-CLASS            PIC 9(3).

       01  STARTING-TIME               PIC X(08)     VALUE SPACES.
       01  SUB                         PIC 9(04)     VALUE ZEROES.
       01  S1                          PIC 9(04)     VALUE ZEROES.
       01  S2                          PIC 9(04)     VALUE ZEROES.
       01  S3                          PIC 9(04)     VALUE ZEROES.

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
           03  BEG-CLASS               PIC 9(02).
           03  END-CLASS               PIC 9(02).
           03  BEG-DATE                PIC 9(08).
           03  END-DATE                PIC 9(08).
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
           "TODAYS TOTAL (TYFILE) COMPARE A15 TO S35".

       01  HEADER-LINE-03.
           03  FILLER                PIC X(4)  VALUE "BRNO".
           03  FILLER                PIC X(3)  VALUE SPACES.
           03  FILLER                PIC X(6)  VALUE " DATE ".
           03  FILLER                PIC X(3)  VALUE SPACES.
           03  FILLER                PIC X(2)  VALUE "CL".
           03  FILLER                PIC XXX   VALUE SPACES.
           03  FILLER                PIC X(28) VALUE "DESCRIPTION".
           03  FILLER                PIC X(22) VALUE "A15".
           03  FILLER                PIC X(3)  VALUE "S35".

       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-BRNO                       PIC Z(3)9.
           03  FILLER                         PIC XXX.
           03  D01-DATE                       PIC 9(6).
           03  FILLER                         PIC X(3).
           03  D01-CLASS                      PIC 9(2).
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
       COPY "LIBUT/TYFILE_DEF.CPY".
       COPY "LIBUT/TYFILE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/TYFILE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE OTY-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TY1-FILE.
           PERFORM CLOSE-TY1-CONNECTION.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE OTY-FILE.
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
           MOVE "TYFILE" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE TYFILE-QUEUE-POS TO Q-POS.
           MOVE TYFILE-COPIES-POS TO C-POS.
           MOVE TYFILE-PRU-POS   TO P-POS.
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

           PERFORM OPEN-TY1-FILE.
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

       ENTER-BEG-CLASS.

           MOVE "ENNN060BEG-CLASS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F3-KEY ) GO TO ENTER-ALL-CLASS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-CLASS.

           MOVE VDUNUM0 TO BEG-CLASS.

      *-----------------------------------------------------------------
       ENTER-END-CLASS.

           MOVE "ENNN060END-CLASS." TO VDU.
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

           GO TO ENTER-BEG-DATE.

      *-----------------------------------------------------------------
       ENTER-ALL-CLASS.

           MOVE "SNNN020BEG-CLASS."  TO VDU.
           MOVE ZEROES                TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020END-CLASS."  TO VDU.
           MOVE 99                    TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

       ENTER-BEG-DATE.

           MOVE "ERNM060BEG-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-CLASS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F3-KEY ) GO TO ENTER-ALL-DATES.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-DATE.

           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.

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
           MOVE 20230101 TO BEG-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "S  8080END-DATE."  TO VDU.
           MOVE EXT-JULIAN-END TO END-DATE VDU-DATE-YYYYMMDD.
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

           MOVE "SNNN060BEG-CLASS."       TO VDU.
           MOVE BEG-CLASS                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060END-CLASS."       TO VDU.
           MOVE END-CLASS                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  8080BEG-DATE."  TO VDU.
           MOVE BEG-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "S  8080END-DATE."  TO VDU.
           MOVE END-DATE TO VDU-DATE-YYYYMMDD.
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

           MOVE EXT-FILPATH-BASE     TO FILPATH-BASE.
           MOVE BR-MACHINE           TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH         TO FILPATH-DATA.

           MOVE "B"                  TO TY-PATH-OVERRIDE.

           MOVE "N"                  TO TY-EOF-SW OTY-EOF-SW.  

           MOVE BR-NO                TO QTY1-WBEG-BRNO.     
           MOVE BEG-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QTY1-WBEG-DATE.
           MOVE END-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QTY1-WEND-DATE.
           MOVE BEG-CLASS            TO QTY1-WBEG-CLASS.
           MOVE END-CLASS            TO QTY1-WEND-CLASS.
           PERFORM START-TY1-FILE.
           PERFORM PROCESS-READ-TY1.

           MOVE SOURCE-PATH  TO OTY-ALL-BASE.
           MOVE MACHINE-R1   TO OTY-ALL-MACHINE.
           MOVE BR-DATA-PATH TO OTY-ALL-DATA.
           MOVE OTY-ALL-PATH TO OTY-PATH ACCESS-BUF.
           MOVE "Y" TO A15-FILE-FOUND-SW
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE MACHINE-R2   TO OTY-ALL-MACHINE
              MOVE OTY-ALL-PATH TO OTY-PATH ACCESS-BUF
              PERFORM ACCESS-CALL
              IF ( STAT-BAD )
                 MOVE "N" TO A15-FILE-FOUND-SW.

           IF A15-FILE-FOUND-SW = "Y"
              PERFORM OPEN-OTY1-FILE
      *        MOVE BR-NO                TO OTY-BRNO
              MOVE ZEROS                TO OTY-BRNO
              MOVE BEG-CLASS            TO OTY-CLASS
              MOVE BEG-DATE             TO DATE-YYYYMMDD
              MOVE DATE-YYYYMMDD-MM     TO SETTY-MM
              MOVE DATE-YYYYMMDD-DD     TO SETTY-DD
              MOVE DATE-YYYYMMDD-YY     TO SETTY-YYY
              IF SETTY-YYY < EXT-JULIAN-CC
                 ADD 100 TO SETTY-YYY
              END-IF
              MOVE SETTY-DATE           TO OTY-DATE
              PERFORM START-OTY1-FILE
              PERFORM PROCESS-READ-OTY1
           ELSE
              MOVE 9999              TO WS-OTY-BRNO
              MOVE 99999999          TO WS-OTY-DATE
              MOVE 999               TO WS-OTY-CLASS
              MOVE "Y"               TO OTY-EOF-SW.

           PERFORM  PROCESS-BRANCH-NEXT-TY
              UNTIL TY-EOF-SW = "Y" AND OTY-EOF-SW = "Y".

           PERFORM CLOSE-TY1-FILE.
           PERFORM CLOSE-OTY1-FILE.

           GO TO PROCESS-BRANCH-DRIVER-EXIT.

       PROCESS-BRANCH-NEXT-TY.

           PERFORM SEND-PROGRESS-INDICATOR.

           PERFORM COMPARE-FIELDS.

           IF WS-OTY1-KEY < WS-TY1-KEY
              PERFORM PROCESS-READ-OTY1
           ELSE
              IF WS-OTY1-KEY > WS-TY1-KEY
                 PERFORM PROCESS-READ-TY1
              ELSE
                 PERFORM PROCESS-READ-OTY1
                 PERFORM PROCESS-READ-TY1.

       PROCESS-BRANCH-DRIVER-EXIT.
           EXIT.

      ******************************************************************
       PROCESS-READ-OTY1 SECTION.
      ******************************************************************

           PERFORM READ-OTY1-FILE-NEXT.
           IF IO-BAD 
              MOVE 9999              TO WS-OTY-BRNO
              MOVE 99999999          TO WS-OTY-DATE
              MOVE 999               TO WS-OTY-CLASS
              MOVE "Y"               TO OTY-EOF-SW
              GO TO EXIT-PROCESS-READ-OTY1
           ELSE 
              MOVE OTY-BRNO         TO WS-OTY-BRNO
              MOVE OTY-DATE         TO DATE-YYYMMDD
              MOVE DATE-YYYMMDD-MM  TO DATE-MMDDYY-MM
              MOVE DATE-YYYMMDD-DD  TO DATE-MMDDYY-DD
              MOVE DATE-YYYMMDD-YYY TO DATE-MMDDYY-YY
              PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
              MOVE DATE-YYYYMMDD    TO WS-OTY-DATE
              MOVE OTY-CLASS        TO WS-OTY-CLASS. 

      *     IF ( OTY-BRNO  < BEG-BRANCH ) OR
      *        ( OTY-BRNO  > END-BRANCH )
      *        GO TO PROCESS-READ-OTY1.
  
           IF DATE-YYYYMMDD < BEG-DATE OR
              DATE-YYYYMMDD > END-DATE
              GO TO PROCESS-READ-OTY1. 

           IF ( OTY-CLASS < BEG-CLASS )  OR
              ( OTY-CLASS > END-CLASS )
              GO TO PROCESS-READ-OTY1.

           ADD  01 TO A15-RECORDS-PROCESSED.

       EXIT-PROCESS-READ-OTY1.
           EXIT.

      ******************************************************************
       PROCESS-READ-TY1 SECTION.
      ******************************************************************

           PERFORM READ-TY1-FILE-NEXT.
           IF IO-BAD 
              MOVE 9999              TO WS-TY-BRNO
              MOVE 99999999          TO WS-TY-DATE
              MOVE 999               TO WS-TY-CLASS 
              MOVE "Y"               TO TY-EOF-SW
           ELSE 
              MOVE TY-BRNO           TO WS-TY-BRNO
              MOVE TY-DATE           TO WS-TY-DATE
              MOVE TY-CLASS          TO WS-TY-CLASS
              ADD  01                TO S35-RECORD-CTR.

       EXIT-PROCESS-READ-TY1.
           EXIT.

      ******************************************************************
       COMPARE-FIELDS SECTION.
      ******************************************************************
      *     MOVE OTY-BRNO                   TO TY-BRNO.
      *     MOVE OTY-CLASS                  TO TY-CLASS.
      *     MOVE OTY-DATE                   TO DATE-YYYMMDD.
      *     MOVE DATE-YYYMMDD-MM            TO DATE-MMDDYY-MM.
      *     MOVE DATE-YYYMMDD-DD            TO DATE-MMDDYY-DD.
      *     MOVE DATE-YYYMMDD-YYY           TO DATE-MMDDYY-YY.
      *     PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
      *     PERFORM TEST-INVALID-DATE.
      *     MOVE DATE-YYYYMMDD              TO TY-DATE.
      *
      *     PERFORM READ-TY1-FILE.
      *     IF IO-FG NOT = 0
      *        MOVE "MISSING S35 RECORD"    TO D01-TEXT
      *        MOVE BR-NO                   TO D01-BRNO
      *        MOVE TY-DATE                 TO D01-DATE
      *        MOVE TY-CLASS                TO D01-CLASS
      *        PERFORM WRITE-DETAIL-LINE-ERROR
      *        GO TO EXIT-COMPARE-FIELDS.

           IF WS-OTY1-KEY < WS-TY1-KEY 
              MOVE "MISSING S35 RECORD" TO D01-TEXT
              MOVE BR-NO                TO D01-BRNO   *> USING BR-NO SINCE OTY MAY HAVE DATA FOR DIFF BRANCH
              MOVE OTY-DATE             TO D01-DATE
              MOVE OTY-CLASS            TO D01-CLASS
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.
 
           IF WS-TY1-KEY < WS-OTY1-KEY
              MOVE "MISSING A15 RECORD" TO D01-TEXT
              MOVE BR-NO                TO D01-BRNO
              MOVE TY-DATE              TO D01-DATE
              MOVE TY-CLASS             TO D01-CLASS 
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.

           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 2
                   AFTER   S2 FROM 1 BY 1 UNTIL S2 > 11
               IF OTY-NO (S1 S2) NOT NUMERIC
                  MOVE 0                   TO OTY-NO (S1 S2)
               END-IF
               IF OTY-NO (S1 S2) NOT = TY-NO (S1 S2)
                  MOVE SPACES              TO D01-TEXT
                  STRING "TY-NO",S1,S2 INTO D01-TEXT
                  MOVE TY-BRNO             TO D01-BRNO
                  MOVE TY-DATE             TO D01-DATE
                  MOVE TY-CLASS            TO D01-CLASS
                  MOVE OTY-NO(S1 S2)       TO D01-A15
                  MOVE TY-NO(S1 S2)        TO D01-S35
                  PERFORM WRITE-DETAIL-LINE-ERROR
               END-IF
               IF OTY-AMT (S1 S2) NOT NUMERIC
                  MOVE 0                   TO OTY-AMT (S1 S2)
               END-IF
               IF OTY-AMT (S1 S2) NOT = TY-AMT (S1 S2)
                  MOVE SPACES              TO D01-TEXT
                  STRING "TY-AMT",S1,S2 INTO D01-TEXT
                  MOVE TY-BRNO             TO D01-BRNO
                  MOVE TY-DATE             TO D01-DATE
                  MOVE TY-CLASS            TO D01-CLASS
                  MOVE OTY-AMT(S1 S2)      TO D01-A15-NUM
                  MOVE TY-AMT(S1 S2)       TO D01-S35-NUM
                  PERFORM WRITE-DETAIL-LINE-ERROR
               END-IF
               IF OTY-O2 (S1 S2) NOT NUMERIC
                  MOVE 0                   TO OTY-O2 (S1 S2)
               END-IF
               IF OTY-O2 (S1 S2) NOT = TY-O2 (S1 S2)
                  MOVE SPACES              TO D01-TEXT
                  STRING "TY-O2",S1,S2 INTO D01-TEXT
                  MOVE TY-BRNO             TO D01-BRNO
                  MOVE TY-DATE             TO D01-DATE
                  MOVE TY-CLASS            TO D01-CLASS
                  MOVE OTY-O2(S1 S2)       TO D01-A15-NUM
                  MOVE TY-O2(S1 S2)        TO D01-S35-NUM
                  PERFORM WRITE-DETAIL-LINE-ERROR
               END-IF
               IF OTY-INT (S1 S2) IS NOT NUMERIC
                  MOVE 0                   TO OTY-INT (S1 S2)
               END-IF
               IF OTY-INT (S1 S2) IS NOT = TY-INT (S1 S2)
                  MOVE SPACES              TO D01-TEXT
                  STRING "TY-INT",S1,S2  INTO D01-TEXT
                  MOVE TY-BRNO             TO D01-BRNO
                  MOVE TY-DATE             TO D01-DATE
                  MOVE TY-CLASS            TO D01-CLASS
                  MOVE OTY-INT(S1 S2)      TO D01-A15-NUM
                  MOVE TY-INT(S1 S2)       TO D01-S35-NUM
                  PERFORM WRITE-DETAIL-LINE-ERROR
               END-IF
           END-PERFORM.

           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 2
                  AFTER S2 FROM 1 BY 1 UNTIL S2 > 5
               IF OTY-NO-REPO (S1 S2) NOT NUMERIC
                  MOVE 0                   TO OTY-NO-REPO (S1 S2)
               END-IF
               IF OTY-NO-REPO (S1 S2) NOT = TY-NO-REPO (S1 S2)
                  MOVE SPACES TO D01-TEXT
                  STRING "TY-NO-REPO",S1,S2 INTO D01-TEXT
                  MOVE TY-BRNO             TO D01-BRNO
                  MOVE TY-DATE             TO D01-DATE
                  MOVE TY-CLASS            TO D01-CLASS
                  MOVE OTY-NO-REPO(S1 S2)  TO D01-A15-NUM
                  MOVE TY-NO-REPO(S1 S2)   TO D01-S35-NUM
                  PERFORM WRITE-DETAIL-LINE-ERROR
               END-IF
               IF OTY-AMT-REPO (S1 S2) NOT NUMERIC
                  MOVE 0                   TO OTY-AMT-REPO (S1 S2)
               END-IF
               IF OTY-AMT-REPO (S1 S2) NOT = TY-AMT-REPO (S1 S2)
                  MOVE SPACES TO D01-TEXT
                  STRING "TY-AMT-REPO",S1,S2 INTO D01-TEXT
                  MOVE TY-BRNO             TO D01-BRNO
                  MOVE TY-DATE             TO D01-DATE
                  MOVE TY-CLASS            TO D01-CLASS
                  MOVE OTY-AMT-REPO(S1 S2) TO D01-A15-NUM
                  MOVE TY-AMT-REPO(S1 S2)  TO D01-S35-NUM
                  PERFORM WRITE-DETAIL-LINE-ERROR
               END-IF
               IF OTY-O2-REPO (S1 S2) NOT NUMERIC
                  MOVE 0                   TO OTY-O2-REPO (S1 S2)
               END-IF
               IF OTY-O2-REPO (S1 S2) NOT = TY-O2-REPO (S1 S2)
                  MOVE SPACES TO D01-TEXT
                  STRING "TY-O2-REPO",S1,S2 INTO D01-TEXT
                  MOVE TY-BRNO             TO D01-BRNO 
                  MOVE TY-DATE             TO D01-DATE
                  MOVE TY-CLASS            TO D01-CLASS
                  MOVE OTY-O2-REPO(S1 S2)  TO D01-A15-NUM
                  MOVE TY-O2-REPO(S1 S2)   TO D01-S35-NUM
                  PERFORM WRITE-DETAIL-LINE-ERROR
               END-IF
               IF OTY-INT-REPO (S1 S2) NOT NUMERIC
                  MOVE 0                   TO OTY-INT-REPO (S1 S2)
               END-IF
               IF OTY-INT-REPO (S1 S2) NOT = TY-INT-REPO (S1 S2)
                  MOVE SPACES TO D01-TEXT
                  STRING "TY-INT-REPO",S1,S2 INTO D01-TEXT
                  MOVE TY-BRNO             TO D01-BRNO
                  MOVE TY-DATE             TO D01-DATE
                  MOVE TY-CLASS            TO D01-CLASS
                  MOVE OTY-INT-REPO(S1 S2) TO D01-A15-NUM
                  MOVE TY-INT-REPO(S1 S2)  TO D01-S35-NUM
                  PERFORM WRITE-DETAIL-LINE-ERROR
               END-IF
           END-PERFORM.

           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 2
                   AFTER S2 FROM 1 BY 1 UNTIL S2 > 10
               IF OTY-NO-PL (S1 S2) NOT NUMERIC
                  MOVE 0                   TO OTY-NO-PL (S1 S2)
               END-IF
               IF OTY-NO-PL (S1 S2) NOT = TY-NO-PL (S1 S2)
                  MOVE SPACES TO D01-TEXT
                  STRING "TY-NO-PL",S1,S2 INTO D01-TEXT
                  MOVE TY-BRNO             TO D01-BRNO
                  MOVE TY-DATE             TO D01-DATE
                  MOVE TY-CLASS            TO D01-CLASS
                  MOVE OTY-NO-PL(S1 S2)    TO D01-A15 
                  MOVE TY-NO-PL(S1 S2)     TO D01-S35 
                  PERFORM WRITE-DETAIL-LINE-ERROR
               END-IF
               IF OTY-AMT-PL (S1 S2) NOT NUMERIC
                  MOVE 0                   TO OTY-AMT-PL (S1 S2)
               END-IF
               IF OTY-AMT-PL (S1 S2) NOT = TY-AMT-PL (S1 S2)
                  MOVE SPACES TO D01-TEXT
                  STRING "TY-AMT-PL",S1,S2 INTO D01-TEXT
                  MOVE TY-BRNO             TO D01-BRNO
                  MOVE TY-DATE             TO D01-DATE
                  MOVE TY-CLASS            TO D01-CLASS
                  MOVE OTY-AMT-PL(S1 S2)   TO D01-A15-NUM
                  MOVE TY-AMT-PL(S1 S2)    TO D01-S35-NUM
                  PERFORM WRITE-DETAIL-LINE-ERROR
               END-IF
               IF OTY-O2-PL (S1 S2) NOT NUMERIC
                  MOVE 0              TO OTY-O2-PL (S1 S2)
               END-IF
               IF OTY-O2-PL (S1 S2) NOT = TY-O2-PL (S1 S2)
                  MOVE SPACES TO D01-TEXT
                  STRING "TY-O2-PL",S1,S2 INTO D01-TEXT
                  MOVE TY-BRNO             TO D01-BRNO
                  MOVE TY-DATE             TO D01-DATE
                  MOVE TY-CLASS            TO D01-CLASS
                  MOVE OTY-O2-PL(S1 S2)    TO D01-A15-NUM
                  MOVE TY-O2-PL(S1 S2)     TO D01-S35-NUM
                  PERFORM WRITE-DETAIL-LINE-ERROR
               END-IF
               IF OTY-INT-PL (S1 S2) NOT NUMERIC
                  MOVE 0                   TO OTY-INT-PL (S1 S2)
               END-IF
               IF OTY-INT-PL (S1 S2) NOT = TY-INT-PL (S1 S2)
                  MOVE SPACES              TO D01-TEXT
                  STRING "TY-INT-PL",S1,S2 INTO D01-TEXT
                  MOVE TY-BRNO             TO D01-BRNO
                  MOVE TY-DATE             TO D01-DATE
                  MOVE TY-CLASS            TO D01-CLASS
                  MOVE OTY-INT-PL(S1 S2)   TO D01-A15-NUM
                  MOVE TY-INT-PL(S1 S2)    TO D01-S35-NUM
                  PERFORM WRITE-DETAIL-LINE-ERROR
               END-IF
           END-PERFORM.

           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 2
                   AFTER S2 FROM 1 BY 1 UNTIL S2 > 3
               IF OTY-DLRVPRIN (S1 S2) NOT NUMERIC
                  MOVE 0                   TO OTY-DLRVPRIN (S1 S2)
               END-IF
               IF OTY-DLRVPRIN (S1 S2) NOT = TY-DLRVPRIN (S1 S2)
                  MOVE SPACES              TO D01-TEXT
                  STRING "TY-DLRVPRIN",S1,S2 INTO D01-TEXT
                  MOVE TY-BRNO             TO D01-BRNO
                  MOVE TY-DATE             TO D01-DATE
                  MOVE TY-CLASS            TO D01-CLASS
                  MOVE OTY-DLRVPRIN(S1 S2) TO D01-A15-NUM
                  MOVE TY-DLRVPRIN(S1 S2)  TO D01-S35-NUM
                  PERFORM WRITE-DETAIL-LINE-ERROR
               END-IF
               IF OTY-DLRVCHGS (S1 S2) NOT NUMERIC
                  MOVE 0                   TO OTY-DLRVCHGS (S1 S2)
               END-IF
               IF OTY-DLRVCHGS (S1 S2) NOT = TY-DLRVCHGS (S1 S2)
                  MOVE SPACES              TO D01-TEXT
                  STRING "TY-DLRVCHGS",S1,S2 INTO D01-TEXT
                  MOVE TY-BRNO             TO D01-BRNO
                  MOVE TY-DATE             TO D01-DATE
                  MOVE TY-CLASS            TO D01-CLASS
                  MOVE OTY-DLRVCHGS(S1 S2) TO D01-A15-NUM
                  MOVE TY-DLRVCHGS(S1 S2)  TO D01-S35-NUM
                  PERFORM WRITE-DETAIL-LINE-ERROR
               END-IF
           END-PERFORM.

           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 2
               IF OTY-DLPARVHELD (S1) NOT NUMERIC
                  MOVE 0                   TO OTY-DLPARVHELD (S1)
               END-IF
               IF OTY-DLPARVHELD (S1) NOT = TY-DLPARVHELD (S1)
                  MOVE SPACES TO D01-TEXT
                  STRING "TY-DLPARVHELD",S1 INTO D01-TEXT
                  MOVE TY-BRNO             TO D01-BRNO
                  MOVE TY-DATE             TO D01-DATE
                  MOVE TY-CLASS            TO D01-CLASS
                  MOVE OTY-DLPARVHELD(S1)  TO D01-A15-NUM
                  MOVE TY-DLPARVHELD(S1)   TO D01-S35-NUM
                  PERFORM WRITE-DETAIL-LINE-ERROR
               END-IF
           END-PERFORM.

           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 5
               IF OTY-PL-PRINCOL (S1) NOT NUMERIC
                  MOVE 0                   TO OTY-PL-PRINCOL (S1)
               END-IF
               IF OTY-PL-PRINCOL (S1) NOT = TY-PL-PRINCOL (S1)
                  MOVE SPACES              TO D01-TEXT
                  STRING "TY-PL-PRINCOL",S1 INTO D01-TEXT
                  MOVE TY-BRNO             TO D01-BRNO
                  MOVE TY-DATE             TO D01-DATE
                  MOVE TY-CLASS            TO D01-CLASS
                  MOVE OTY-PL-PRINCOL(S1)  TO D01-A15-NUM
                  MOVE TY-PL-PRINCOL(S1)   TO D01-S35-NUM
                  PERFORM WRITE-DETAIL-LINE-ERROR
               END-IF
               IF OTY-PL-INTCOL (S1) NOT NUMERIC
                  MOVE 0                   TO OTY-PL-INTCOL (S1)
               END-IF
               IF OTY-PL-INTCOL (S1) NOT = TY-PL-INTCOL (S1)
                  MOVE SPACES              TO D01-TEXT
                  STRING "TY-PL-INTCOL",S1 INTO D01-TEXT
                  MOVE TY-BRNO             TO D01-BRNO
                  MOVE TY-DATE             TO D01-DATE
                  MOVE TY-CLASS            TO D01-CLASS
                  MOVE OTY-PL-INTCOL(S1)   TO D01-A15-NUM
                  MOVE TY-PL-INTCOL(S1)    TO D01-S35-NUM
                  PERFORM WRITE-DETAIL-LINE-ERROR
               END-IF
               IF OTY-PL-LATECOL (S1) NOT NUMERIC
                  MOVE 0                   TO OTY-PL-LATECOL (S1)
               END-IF
               IF OTY-PL-LATECOL (S1) NOT = TY-PL-LATECOL (S1)
                  MOVE SPACES              TO D01-TEXT
                  STRING "TY-PL-LATECOL",S1 INTO D01-TEXT
                  MOVE TY-BRNO             TO D01-BRNO
                  MOVE TY-DATE             TO D01-DATE
                  MOVE TY-CLASS            TO D01-CLASS
                  MOVE OTY-PL-LATECOL(S1)  TO D01-A15-NUM
                  MOVE TY-PL-LATECOL(S1)   TO D01-S35-NUM
                  PERFORM WRITE-DETAIL-LINE-ERROR
               END-IF
               IF OTY-PL-O2COL (S1) NOT NUMERIC
                  MOVE 0                   TO OTY-PL-O2COL (S1)
               END-IF
               IF OTY-PL-O2COL (S1) NOT = TY-PL-O2COL (S1)
                  MOVE SPACES              TO D01-TEXT
                  STRING "TY-PL-O2COL",S1 INTO D01-TEXT
                  MOVE TY-BRNO             TO D01-BRNO
                  MOVE TY-DATE             TO D01-DATE
                  MOVE TY-CLASS            TO D01-CLASS
                  MOVE OTY-PL-O2COL(S1)    TO D01-A15-NUM
                  MOVE TY-PL-O2COL(S1)     TO D01-S35-NUM
                  PERFORM WRITE-DETAIL-LINE-ERROR
               END-IF
           END-PERFORM.

           IF OTY-AMT-SOLD  NOT NUMERIC
              MOVE 0                       TO OTY-AMT-SOLD.
           IF OTY-AMT-SOLD  NOT = TY-AMT-SOLD
              MOVE "TY-AMT-SOLD"           TO D01-TEXT
              MOVE TY-BRNO                 TO D01-BRNO
              MOVE TY-DATE                 TO D01-DATE
              MOVE TY-CLASS                TO D01-CLASS
              MOVE OTY-AMT-SOLD            TO D01-A15-NUM
              MOVE TY-AMT-SOLD             TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OTY-AMT-TRANS  NOT NUMERIC
              MOVE 0                       TO OTY-AMT-TRANS.
           IF OTY-AMT-TRANS  NOT = TY-AMT-TRANS
              MOVE "TY-AMT-TRANS"          TO D01-TEXT
              MOVE TY-BRNO                 TO D01-BRNO
              MOVE TY-DATE                 TO D01-DATE
              MOVE TY-CLASS                TO D01-CLASS
              MOVE OTY-AMT-TRANS           TO D01-A15-NUM
              MOVE TY-AMT-TRANS            TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OTY-AMT-PURCH  NOT NUMERIC
              MOVE 0                       TO OTY-AMT-PURCH.
           IF OTY-AMT-PURCH  NOT = TY-AMT-PURCH
              MOVE "TY-AMT-PURCH"          TO D01-TEXT
              MOVE TY-BRNO                 TO D01-BRNO
              MOVE TY-DATE                 TO D01-DATE
              MOVE TY-CLASS                TO D01-CLASS
              MOVE OTY-AMT-PURCH           TO D01-A15-NUM
              MOVE TY-AMT-PURCH            TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OTY-NO-SOLD  NOT NUMERIC
              MOVE 0                       TO OTY-NO-SOLD.
           IF OTY-NO-SOLD  NOT = TY-NO-SOLD
              MOVE "TY-NO-SOLD"            TO D01-TEXT
              MOVE TY-BRNO                 TO D01-BRNO
              MOVE TY-DATE                 TO D01-DATE
              MOVE TY-CLASS                TO D01-CLASS
              MOVE OTY-NO-SOLD             TO D01-A15 
              MOVE TY-NO-SOLD              TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OTY-NO-TRANS  NOT NUMERIC
              MOVE 0                       TO OTY-NO-TRANS.
           IF OTY-NO-TRANS  NOT = TY-NO-TRANS
              MOVE "TY-NO-TRANS"           TO D01-TEXT
              MOVE TY-BRNO                 TO D01-BRNO
              MOVE TY-DATE                 TO D01-DATE
              MOVE TY-CLASS                TO D01-CLASS
              MOVE OTY-NO-TRANS            TO D01-A15 
              MOVE TY-NO-TRANS             TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OTY-NO-PURCH  NOT NUMERIC
              MOVE 0                       TO OTY-NO-PURCH.
           IF OTY-NO-PURCH  NOT = TY-NO-PURCH
              MOVE "TY-NO-PURCH"           TO D01-TEXT
              MOVE TY-BRNO                 TO D01-BRNO
              MOVE TY-DATE                 TO D01-DATE
              MOVE TY-CLASS                TO D01-CLASS
              MOVE OTY-NO-PURCH            TO D01-A15 
              MOVE TY-NO-PURCH             TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OTY-OTHCOL  NOT NUMERIC
              MOVE 0                       TO OTY-OTHCOL.
           IF OTY-OTHCOL  NOT = TY-OTHCOL
              MOVE "TY-OTHCOL"             TO D01-TEXT
              MOVE TY-BRNO                 TO D01-BRNO
              MOVE TY-DATE                 TO D01-DATE
              MOVE TY-CLASS                TO D01-CLASS
              MOVE OTY-OTHCOL              TO D01-A15-NUM
              MOVE TY-OTHCOL               TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OTY-OT2COL  NOT NUMERIC
              MOVE 0                       TO OTY-OT2COL.
           IF OTY-OT2COL  NOT = TY-OT2COL
              MOVE "TY-OT2COL"             TO D01-TEXT
              MOVE TY-BRNO                 TO D01-BRNO
              MOVE TY-DATE                 TO D01-DATE
              MOVE TY-CLASS                TO D01-CLASS
              MOVE OTY-OT2COL              TO D01-A15-NUM
              MOVE TY-OT2COL               TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OTY-INTCOL  NOT NUMERIC
              MOVE 0                TO OTY-INTCOL.
           IF OTY-INTCOL  NOT = TY-INTCOL
              MOVE "TY-INTCOL"      TO D01-TEXT
              MOVE TY-BRNO          TO D01-BRNO
              MOVE TY-DATE          TO D01-DATE
              MOVE TY-CLASS         TO D01-CLASS
              MOVE OTY-INTCOL       TO D01-A15-NUM
              MOVE TY-INTCOL        TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OTY-LATECOL  NOT NUMERIC
              MOVE 0                TO OTY-LATECOL.
           IF OTY-LATECOL  NOT = TY-LATECOL
              MOVE "TY-LATECOL"     TO D01-TEXT
              MOVE TY-BRNO          TO D01-BRNO
              MOVE TY-DATE          TO D01-DATE
              MOVE TY-CLASS         TO D01-CLASS
              MOVE OTY-LATECOL      TO D01-A15-NUM
              MOVE TY-LATECOL       TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OTY-PRINCOL  NOT NUMERIC
              MOVE 0                TO OTY-PRINCOL.
           IF OTY-PRINCOL  NOT = TY-PRINCOL
              MOVE "TY-PRINCOL"     TO D01-TEXT
              MOVE TY-BRNO          TO D01-BRNO
              MOVE TY-DATE          TO D01-DATE
              MOVE TY-CLASS         TO D01-CLASS
              MOVE OTY-PRINCOL      TO D01-A15-NUM
              MOVE TY-PRINCOL       TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OTY-DEFCOL  NOT NUMERIC
              MOVE 0                TO OTY-DEFCOL.
           IF OTY-DEFCOL  NOT = TY-DEFCOL
              MOVE "TY-DEFCOL"      TO D01-TEXT
              MOVE TY-BRNO          TO D01-BRNO
              MOVE TY-DATE          TO D01-DATE
              MOVE TY-CLASS         TO D01-CLASS
              MOVE OTY-DEFCOL       TO D01-A15-NUM
              MOVE TY-DEFCOL        TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OTY-RNPRIN  NOT NUMERIC
              MOVE 0                TO OTY-RNPRIN.
           IF OTY-RNPRIN  NOT = TY-RNPRIN
              MOVE "TY-RNPRIN"      TO D01-TEXT
              MOVE TY-BRNO          TO D01-BRNO 
              MOVE TY-DATE          TO D01-DATE
              MOVE TY-CLASS         TO D01-CLASS
              MOVE OTY-RNPRIN       TO D01-A15-NUM
              MOVE TY-RNPRIN        TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OTY-INTREB  NOT NUMERIC
              MOVE 0                TO OTY-INTREB.
           IF OTY-INTREB  NOT = TY-INTREB
              MOVE "TY-INTREB"      TO D01-TEXT
              MOVE TY-BRNO          TO D01-BRNO
              MOVE TY-DATE          TO D01-DATE
              MOVE TY-CLASS         TO D01-CLASS
              MOVE OTY-INTREB       TO D01-A15-NUM
              MOVE TY-INTREB        TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OTY-CLREB1  NOT NUMERIC
              MOVE 0                TO OTY-CLREB1.
           IF OTY-CLREB1  NOT = TY-CLREB1
              MOVE "TY-CLREB1"      TO D01-TEXT
              MOVE TY-BRNO          TO D01-BRNO
              MOVE TY-DATE          TO D01-DATE
              MOVE TY-CLASS         TO D01-CLASS
              MOVE OTY-CLREB1       TO D01-A15-NUM
              MOVE TY-CLREB1        TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OTY-CLREB2  NOT NUMERIC
              MOVE 0                TO OTY-CLREB2.
           IF OTY-CLREB2  NOT = TY-CLREB2
              MOVE "TY-CLREB2"      TO D01-TEXT
              MOVE TY-BRNO          TO D01-BRNO
              MOVE TY-DATE          TO D01-DATE
              MOVE TY-CLASS         TO D01-CLASS
              MOVE OTY-CLREB2       TO D01-A15-NUM
              MOVE TY-CLREB2        TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
	
           IF OTY-CLREB3  NOT NUMERIC
              MOVE 0                TO OTY-CLREB3.
           IF OTY-CLREB3  NOT = TY-CLREB3
              MOVE "TY-CLREB3"      TO D01-TEXT
              MOVE TY-BRNO          TO D01-BRNO
              MOVE TY-DATE          TO D01-DATE
              MOVE TY-CLASS         TO D01-CLASS
              MOVE OTY-CLREB3       TO D01-A15-NUM
              MOVE TY-CLREB3        TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OTY-CLREB4  NOT NUMERIC
              MOVE 0                TO OTY-CLREB4.
           IF OTY-CLREB4  NOT = TY-CLREB4
              MOVE "TY-CLREB4"      TO D01-TEXT
              MOVE TY-BRNO          TO D01-BRNO
              MOVE TY-DATE          TO D01-DATE
              MOVE TY-CLASS         TO D01-CLASS
              MOVE OTY-CLREB4       TO D01-A15-NUM
              MOVE TY-CLREB4        TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OTY-AHREB  NOT NUMERIC
              MOVE 0                TO OTY-AHREB.
           IF OTY-AHREB  NOT = TY-AHREB
              MOVE "TY-AHREB"       TO D01-TEXT
              MOVE TY-BRNO          TO D01-BRNO
              MOVE TY-DATE          TO D01-DATE
              MOVE TY-CLASS         TO D01-CLASS
              MOVE OTY-AHREB        TO D01-A15-NUM
              MOVE TY-AHREB         TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
			  
           IF OTY-PPREB  NOT NUMERIC
              MOVE 0                TO OTY-PPREB.
           IF OTY-PPREB  NOT = TY-PPREB
              MOVE "TY-PPREB"       TO D01-TEXT
              MOVE TY-BRNO          TO D01-BRNO
              MOVE TY-DATE          TO D01-DATE
              MOVE TY-CLASS         TO D01-CLASS
              MOVE OTY-PPREB        TO D01-A15-NUM
              MOVE TY-PPREB         TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OTY-O1REB  NOT NUMERIC
              MOVE 0                TO OTY-O1REB.
           IF OTY-O1REB  NOT = TY-O1REB
              MOVE "TY-O1REB"       TO D01-TEXT
              MOVE TY-BRNO          TO D01-BRNO
              MOVE TY-DATE          TO D01-DATE
              MOVE TY-CLASS         TO D01-CLASS
              MOVE OTY-O1REB        TO D01-A15-NUM
              MOVE TY-O1REB         TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OTY-O2REB  NOT NUMERIC
              MOVE 0                TO OTY-O2REB.
           IF OTY-O2REB  NOT = TY-O2REB
              MOVE "TY-O2REB"       TO D01-TEXT
              MOVE TY-BRNO          TO D01-BRNO
              MOVE TY-DATE          TO D01-DATE
              MOVE TY-CLASS         TO D01-CLASS
              MOVE OTY-O2REB        TO D01-A15-NUM
              MOVE TY-O2REB         TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OTY-O3REB  NOT NUMERIC
              MOVE 0                TO OTY-O3REB.
           IF OTY-O3REB  NOT = TY-O3REB
              MOVE "TY-O3REB"       TO D01-TEXT
              MOVE TY-BRNO          TO D01-BRNO
              MOVE TY-DATE          TO D01-DATE
              MOVE TY-CLASS         TO D01-CLASS
              MOVE OTY-O3REB        TO D01-A15-NUM
              MOVE TY-O3REB         TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OTY-O4REB  NOT NUMERIC
              MOVE 0                TO OTY-O4REB.
           IF OTY-O4REB  NOT = TY-O4REB
              MOVE "TY-O4REB"       TO D01-TEXT
              MOVE TY-BRNO          TO D01-BRNO 
              MOVE TY-DATE          TO D01-DATE
              MOVE TY-CLASS         TO D01-CLASS
              MOVE OTY-O4REB        TO D01-A15-NUM
              MOVE TY-O4REB         TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OTY-O5REB  NOT NUMERIC
              MOVE 0                TO OTY-O5REB.
           IF OTY-O5REB  NOT = TY-O5REB
              MOVE "TY-O5REB"       TO D01-TEXT
              MOVE TY-BRNO          TO D01-BRNO
              MOVE TY-DATE          TO D01-DATE
              MOVE TY-CLASS         TO D01-CLASS
              MOVE OTY-O5REB        TO D01-A15-NUM
              MOVE TY-O5REB         TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OTY-O6REB  NOT NUMERIC
              MOVE 0                TO OTY-O6REB.
           IF OTY-O6REB  NOT = TY-O6REB
              MOVE "TY-O6REB"       TO D01-TEXT
              MOVE TY-BRNO          TO D01-BRNO
              MOVE TY-DATE          TO D01-DATE
              MOVE TY-CLASS         TO D01-CLASS
              MOVE OTY-O6REB        TO D01-A15-NUM
              MOVE TY-O6REB         TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OTY-O7REB  NOT NUMERIC
              MOVE 0                TO OTY-O7REB.
           IF OTY-O7REB  NOT = TY-O7REB
              MOVE "TY-O7REB"       TO D01-TEXT
              MOVE TY-BRNO          TO D01-BRNO
              MOVE TY-DATE          TO D01-DATE
              MOVE TY-CLASS         TO D01-CLASS
              MOVE OTY-O7REB        TO D01-A15-NUM
              MOVE TY-O7REB         TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OTY-SCREB  NOT NUMERIC
              MOVE 0                TO OTY-SCREB.
           IF OTY-SCREB  NOT = TY-SCREB
              MOVE "TY-SCREB"       TO D01-TEXT
              MOVE TY-BRNO          TO D01-BRNO
              MOVE TY-DATE          TO D01-DATE
              MOVE TY-CLASS         TO D01-CLASS
              MOVE OTY-SCREB        TO D01-A15-NUM
              MOVE TY-SCREB         TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OTY-MFREB  NOT NUMERIC
              MOVE 0                TO OTY-MFREB.
           IF OTY-MFREB  NOT = TY-MFREB
              MOVE "TY-MFREB"       TO D01-TEXT
              MOVE TY-BRNO          TO D01-BRNO
              MOVE TY-DATE          TO D01-DATE
              MOVE TY-CLASS         TO D01-CLASS
              MOVE OTY-MFREB        TO D01-A15-NUM
              MOVE TY-MFREB         TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OTY-DFREB  NOT NUMERIC
              MOVE 0                TO OTY-DFREB.
           IF OTY-DFREB  NOT = TY-DFREB
              MOVE "TY-DFREB"       TO D01-TEXT
              MOVE TY-BRNO          TO D01-BRNO
              MOVE TY-DATE          TO D01-DATE
              MOVE TY-CLASS         TO D01-CLASS
              MOVE OTY-DFREB        TO D01-A15-NUM
              MOVE TY-DFREB         TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OTY-PART-CHGOFF NOT NUMERIC
              MOVE 0                   TO OTY-PART-CHGOFF.
           IF OTY-PART-CHGOFF  NOT NUMERIC
              MOVE 0                   TO OTY-PART-CHGOFF.
           IF OTY-PART-CHGOFF  NOT = TY-PART-CHGOFF
              MOVE "TY-PART-CHGOFF"    TO D01-TEXT
              MOVE TY-BRNO             TO D01-BRNO
              MOVE TY-DATE             TO D01-DATE
              MOVE TY-CLASS            TO D01-CLASS
              MOVE OTY-PART-CHGOFF     TO D01-A15-NUM
              MOVE TY-PART-CHGOFF      TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 7
               IF OTY-INS-NO-ELIG (S1) NOT NUMERIC
                  MOVE 0 TO OTY-INS-NO-ELIG (S1)
               END-IF
               IF OTY-INS-NO-ELIG (S1) NOT = TY-INS-NO-ELIG (S1)
                  MOVE SPACES              TO D01-TEXT
                  STRING "TY-INS-NO-ELIG",S1 INTO D01-TEXT
                  MOVE TY-BRNO             TO D01-BRNO  
                  MOVE TY-DATE             TO D01-DATE
                  MOVE TY-CLASS            TO D01-CLASS
                  MOVE OTY-INS-NO-ELIG(S1) TO D01-A15 
                  MOVE TY-INS-NO-ELIG(S1)  TO D01-S35 
                  PERFORM WRITE-DETAIL-LINE-ERROR
               END-IF
               IF OTY-INS-NO-WROTE (S1) NOT NUMERIC
                  MOVE 0 TO OTY-INS-NO-WROTE (S1)
               END-IF
               IF OTY-INS-NO-WROTE (S1) NOT = TY-INS-NO-WROTE (S1)
                  MOVE SPACES               TO D01-TEXT
                  STRING "TY-INS-NO-WROTE",S1 INTO D01-TEXT
                  MOVE TY-BRNO              TO D01-BRNO
                  MOVE TY-DATE              TO D01-DATE
                  MOVE TY-CLASS             TO D01-CLASS
                  MOVE OTY-INS-NO-WROTE(S1) TO D01-A15 
                  MOVE TY-INS-NO-WROTE(S1)  TO D01-S35 
                  PERFORM WRITE-DETAIL-LINE-ERROR
               END-IF
               IF OTY-INS-NO-NOTWROTE (S1) NOT NUMERIC
                  MOVE 0 TO OTY-INS-NO-NOTWROTE (S1)
               END-IF
               IF OTY-INS-NO-NOTWROTE (S1) NOT = TY-INS-NO-NOTWROTE (S1)
                  MOVE SPACES                  TO D01-TEXT
                  STRING "TY-INS-NO-NOTWROTE",S1 INTO D01-TEXT
                  MOVE TY-BRNO                 TO D01-BRNO
                  MOVE TY-DATE                 TO D01-DATE
                  MOVE TY-CLASS                TO D01-CLASS
                  MOVE OTY-INS-NO-NOTWROTE(S1) TO D01-A15 
                  MOVE TY-INS-NO-NOTWROTE(S1)  TO D01-S35 
                  PERFORM WRITE-DETAIL-LINE-ERROR
               END-IF
               IF OTY-INS-AMT-WROTE (S1) NOT NUMERIC
                  MOVE 0 TO OTY-INS-AMT-WROTE (S1)
               END-IF
               IF OTY-INS-AMT-WROTE (S1) NOT = TY-INS-AMT-WROTE (S1)
                  MOVE SPACES                TO D01-TEXT
                  STRING "TY-INS-AMT-WROTE",S1 INTO D01-TEXT
                  MOVE TY-BRNO               TO D01-BRNO
                  MOVE TY-DATE               TO D01-DATE
                  MOVE TY-CLASS              TO D01-CLASS
                  MOVE OTY-INS-AMT-WROTE(S1) TO D01-A15-NUM
                  MOVE TY-INS-AMT-WROTE(S1)  TO D01-S35-NUM
                  PERFORM WRITE-DETAIL-LINE-ERROR
               END-IF
           END-PERFORM.

      * NONFILE, AD&D, ROADGARD WERE 8,9,10 IN INS TABLE, NOW THEY
      * ARE 1, 2, 3 IN FEE INS TABLE:

           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 3
               IF OTY-INS-NO-ELIG (S1 + 7) NOT NUMERIC
                  MOVE 0 TO OTY-INS-NO-ELIG (S1 + 7)
               END-IF
               IF OTY-INS-NO-ELIG (S1 + 7) NOT = TY-FEE-NO-ELIG (S1)
                  MOVE SPACES              TO D01-TEXT
                  STRING "TY-FEE-NO-ELIG",S1 INTO D01-TEXT
                  MOVE TY-BRNO             TO D01-BRNO
                  MOVE TY-DATE             TO D01-DATE
                  MOVE TY-CLASS            TO D01-CLASS
                  MOVE OTY-INS-NO-ELIG(S1) TO D01-A15 
                  MOVE TY-FEE-NO-ELIG(S1)  TO D01-S35 
                  PERFORM WRITE-DETAIL-LINE-ERROR
               END-IF
               IF OTY-INS-NO-WROTE (S1 + 7) NOT NUMERIC
                  MOVE 0 TO OTY-INS-NO-WROTE (S1 + 7)
               END-IF
               IF OTY-INS-NO-WROTE (S1 + 7) NOT = TY-FEE-NO-WROTE (S1)
                  MOVE SPACES               TO D01-TEXT
                  STRING "TY-FEE-NO-WROTE",S1 INTO D01-TEXT
                  MOVE TY-BRNO              TO D01-BRNO
                  MOVE TY-DATE              TO D01-DATE
                  MOVE TY-CLASS             TO D01-CLASS
                  MOVE OTY-INS-NO-WROTE(S1) TO D01-A15 
                  MOVE TY-FEE-NO-WROTE(S1)  TO D01-S35 
                  PERFORM WRITE-DETAIL-LINE-ERROR
               END-IF
               IF OTY-INS-NO-NOTWROTE (S1 + 7) NOT NUMERIC
                  MOVE 0 TO OTY-INS-NO-NOTWROTE (S1 + 7)
               END-IF
               IF OTY-INS-NO-NOTWROTE (S1 + 7) NOT =
                             TY-FEE-NO-NOTWROTE (S1)
                  MOVE SPACES                  TO D01-TEXT
                  STRING "TY-FEE-NO-NOTWROTE",S1 INTO D01-TEXT
                  MOVE TY-BRNO                 TO D01-BRNO
                  MOVE TY-DATE                 TO D01-DATE
                  MOVE TY-CLASS                TO D01-CLASS
                  MOVE OTY-INS-NO-NOTWROTE(S1) TO D01-A15 
                  MOVE TY-FEE-NO-NOTWROTE(S1)  TO D01-S35 
                  PERFORM WRITE-DETAIL-LINE-ERROR
               END-IF
               IF OTY-INS-AMT-WROTE (S1 + 7) NOT NUMERIC
                  MOVE 0 TO OTY-INS-AMT-WROTE (S1 + 7)
               END-IF
               IF OTY-INS-AMT-WROTE (S1 + 7) NOT =
                  TY-FEE-AMT-WROTE (S1)
                  MOVE SPACES                TO D01-TEXT
                  STRING "TY-FEE-AMT-WROTE",S1 INTO D01-TEXT
                  MOVE TY-BRNO               TO D01-BRNO
                  MOVE TY-DATE               TO D01-DATE
                  MOVE TY-CLASS              TO D01-CLASS
                  MOVE OTY-INS-AMT-WROTE(S1) TO D01-A15-NUM
                  MOVE TY-FEE-AMT-WROTE(S1)  TO D01-S35-NUM
                  PERFORM WRITE-DETAIL-LINE-ERROR
               END-IF
           END-PERFORM.

           IF OTY-OTH-NO-COL  NOT NUMERIC
              MOVE 0                TO OTY-OTH-NO-COL.
           IF OTY-OTH-NO-COL  NOT = TY-OTH-NO-COL
              MOVE "TY-OTH-NO-COL"  TO D01-TEXT
              MOVE TY-BRNO          TO D01-BRNO 
              MOVE TY-DATE          TO D01-DATE
              MOVE TY-CLASS         TO D01-CLASS
              MOVE OTY-OTH-NO-COL   TO D01-A15 
              MOVE TY-OTH-NO-COL    TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OTY-OT2-NO-COL  NOT NUMERIC
              MOVE 0                 TO OTY-OT2-NO-COL.
           IF OTY-OT2-NO-COL  NOT = TY-OT2-NO-COL
              MOVE "TY-OT2-NO-COL"   TO D01-TEXT
              MOVE TY-BRNO           TO D01-BRNO 
              MOVE TY-DATE           TO D01-DATE
              MOVE TY-CLASS          TO D01-CLASS
              MOVE OTY-OT2-NO-COL    TO D01-A15 
              MOVE TY-OT2-NO-COL     TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OTY-INT-NO-COL  NOT NUMERIC
              MOVE 0                  TO OTY-INT-NO-COL.
           IF OTY-INT-NO-COL  NOT = TY-INT-NO-COL
              MOVE "TY-INT-NO-COL"    TO D01-TEXT
              MOVE TY-BRNO            TO D01-BRNO 
              MOVE TY-DATE            TO D01-DATE
              MOVE TY-CLASS           TO D01-CLASS
              MOVE OTY-INT-NO-COL     TO D01-A15   
              MOVE TY-INT-NO-COL      TO D01-S35   
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OTY-LATE-NO-COL  NOT NUMERIC
              MOVE 0                 TO OTY-LATE-NO-COL.
           IF OTY-LATE-NO-COL  NOT = TY-LATE-NO-COL
              MOVE "TY-LATE-NO-COL"  TO D01-TEXT
              MOVE TY-BRNO           TO D01-BRNO 
              MOVE TY-DATE           TO D01-DATE
              MOVE TY-CLASS          TO D01-CLASS
              MOVE OTY-LATE-NO-COL   TO D01-A15 
              MOVE TY-LATE-NO-COL    TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OTY-OTH-NO-WAIVED  NOT NUMERIC
              MOVE 0                   TO OTY-OTH-NO-WAIVED.
           IF OTY-OTH-NO-WAIVED  NOT = TY-OTH-NO-WAIVED
              MOVE "TY-OTH-NO-WAIVED"  TO D01-TEXT
              MOVE TY-BRNO             TO D01-BRNO 
              MOVE TY-DATE             TO D01-DATE
              MOVE TY-CLASS            TO D01-CLASS
              MOVE OTY-OTH-NO-WAIVED   TO D01-A15 
              MOVE TY-OTH-NO-WAIVED    TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OTY-OT2-NO-WAIVED  NOT NUMERIC
              MOVE 0                   TO OTY-OT2-NO-WAIVED.
           IF OTY-OT2-NO-WAIVED  NOT = TY-OT2-NO-WAIVED
              MOVE "TY-OT2-NO-WAIVED"  TO D01-TEXT
              MOVE TY-BRNO             TO D01-BRNO 
              MOVE TY-DATE             TO D01-DATE
              MOVE TY-CLASS            TO D01-CLASS
              MOVE OTY-OT2-NO-WAIVED   TO D01-A15 
              MOVE TY-OT2-NO-WAIVED    TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OTY-INT-NO-WAIVED  NOT NUMERIC
              MOVE 0                   TO OTY-INT-NO-WAIVED.
           IF OTY-INT-NO-WAIVED  NOT = TY-INT-NO-WAIVED
              MOVE "TY-INT-NO-WAIVED"  TO D01-TEXT
              MOVE TY-BRNO             TO D01-BRNO 
              MOVE TY-DATE             TO D01-DATE
              MOVE TY-CLASS            TO D01-CLASS
              MOVE OTY-INT-NO-WAIVED   TO D01-A15 
              MOVE TY-INT-NO-WAIVED    TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OTY-LATE-NO-WAIVED  NOT NUMERIC
              MOVE 0                   TO OTY-LATE-NO-WAIVED.
           IF OTY-LATE-NO-WAIVED  NOT = TY-LATE-NO-WAIVED
              MOVE "TY-LATE-NO-WAIVED" TO D01-TEXT
              MOVE TY-BRNO             TO D01-BRNO 
              MOVE TY-DATE             TO D01-DATE
              MOVE TY-CLASS            TO D01-CLASS
              MOVE OTY-LATE-NO-WAIVED  TO D01-A15 
              MOVE TY-LATE-NO-WAIVED   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OTY-OTH-AMT-WAIVED  NOT NUMERIC
              MOVE 0                   TO OTY-OTH-AMT-WAIVED.
           IF OTY-OTH-AMT-WAIVED  NOT = TY-OTH-AMT-WAIVED
              MOVE "TY-OTH-AMT-WAIVED" TO D01-TEXT
              MOVE TY-BRNO             TO D01-BRNO 
              MOVE TY-DATE             TO D01-DATE
              MOVE TY-CLASS            TO D01-CLASS
              MOVE OTY-OTH-AMT-WAIVED  TO D01-A15-NUM
              MOVE TY-OTH-AMT-WAIVED   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OTY-OT2-AMT-WAIVED  NOT NUMERIC
              MOVE 0                   TO OTY-OT2-AMT-WAIVED.
           IF OTY-OT2-AMT-WAIVED  NOT = TY-OT2-AMT-WAIVED
              MOVE "TY-OT2-AMT-WAIVED" TO D01-TEXT
              MOVE TY-BRNO             TO D01-BRNO 
              MOVE TY-DATE             TO D01-DATE
              MOVE TY-CLASS            TO D01-CLASS
              MOVE OTY-OT2-AMT-WAIVED  TO D01-A15-NUM
              MOVE TY-OT2-AMT-WAIVED   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OTY-INT-AMT-WAIVED  NOT NUMERIC
              MOVE 0                     TO OTY-INT-AMT-WAIVED.
           IF OTY-INT-AMT-WAIVED  NOT = TY-INT-AMT-WAIVED
              MOVE "TY-INT-AMT-WAIVED"   TO D01-TEXT
              MOVE TY-BRNO               TO D01-BRNO 
              MOVE TY-DATE               TO D01-DATE
              MOVE TY-CLASS              TO D01-CLASS
              MOVE OTY-INT-AMT-WAIVED    TO D01-A15-NUM
              MOVE TY-INT-AMT-WAIVED     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OTY-LATE-AMT-WAIVED  NOT NUMERIC
              MOVE 0                     TO OTY-LATE-AMT-WAIVED.
           IF OTY-LATE-AMT-WAIVED  NOT = TY-LATE-AMT-WAIVED
              MOVE "TY-LATE-AMT-WAIVED"  TO D01-TEXT
              MOVE TY-BRNO               TO D01-BRNO 
              MOVE TY-DATE               TO D01-DATE
              MOVE TY-CLASS              TO D01-CLASS
              MOVE OTY-LATE-AMT-WAIVED   TO D01-A15-NUM
              MOVE TY-LATE-AMT-WAIVED    TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.


           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 8
               IF OTY-CKWRITTEN (S1) NOT NUMERIC
                  MOVE 0                 TO OTY-CKWRITTEN (S1)
               END-IF
               IF OTY-CKWRITTEN (S1) NOT = TY-CKWRITTEN (S1)
                  MOVE SPACES            TO D01-TEXT
                  STRING "TY-CKWRITTEN",S1 INTO D01-TEXT
                  MOVE TY-BRNO           TO D01-BRNO    
                  MOVE TY-DATE           TO D01-DATE
                  MOVE TY-CLASS          TO D01-CLASS
                  MOVE OTY-CKWRITTEN(S1) TO D01-A15-NUM
                  MOVE TY-CKWRITTEN(S1)  TO D01-S35-NUM
                  PERFORM WRITE-DETAIL-LINE-ERROR
               END-IF
           END-PERFORM.

           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 2
                   AFTER S2 FROM 1 BY 1 UNTIL S2 > 10
               IF OTY-UNITS (S1 S2) NOT NUMERIC
                  MOVE 0                TO OTY-UNITS (S1 S2)
               END-IF  
               IF OTY-UNITS (S1 S2) NOT = TY-UNITS (S1 S2)
                  MOVE SPACES           TO D01-TEXT
                  STRING "TY-UNITS",S1,S2 INTO D01-TEXT
                  MOVE TY-BRNO          TO D01-BRNO  
                  MOVE TY-DATE          TO D01-DATE
                  MOVE TY-CLASS         TO D01-CLASS
                  MOVE OTY-UNITS(S1 S2) TO D01-A15 
                  MOVE TY-UNITS(S1 S2)  TO D01-S35 
                  PERFORM WRITE-DETAIL-LINE-ERROR
               END-IF
               IF OTY-OUTBAL (S1 S2) NOT NUMERIC
                  MOVE 0                 TO OTY-OUTBAL (S1 S2)
               END-IF
               IF OTY-OUTBAL (S1 S2) NOT = TY-OUTBAL (S1 S2)
                  MOVE SPACES            TO D01-TEXT
                  STRING "TY-OUTBAL",S1,S2 INTO D01-TEXT
                  MOVE TY-BRNO           TO D01-BRNO 
                  MOVE TY-DATE           TO D01-DATE
                  MOVE TY-CLASS          TO D01-CLASS
                  MOVE OTY-OUTBAL(S1 S2) TO D01-A15-NUM
                  MOVE TY-OUTBAL(S1 S2)  TO D01-S35-NUM
                  PERFORM WRITE-DETAIL-LINE-ERROR
               END-IF
           END-PERFORM.

           IF OTY-SUMWGTTM  NOT NUMERIC
              MOVE 0                TO OTY-SUMWGTTM.
           IF OTY-SUMWGTTM  NOT = TY-SUMWGTTM
              MOVE "TY-SUMWGTTM"    TO D01-TEXT
              MOVE TY-BRNO          TO D01-BRNO 
              MOVE TY-DATE          TO D01-DATE
              MOVE TY-CLASS         TO D01-CLASS
              MOVE OTY-SUMWGTTM TO D01-A15 
              MOVE TY-SUMWGTTM  TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OTY-SUMWGTAV  NOT NUMERIC
              MOVE 0                TO OTY-SUMWGTAV.
           IF OTY-SUMWGTAV  NOT = TY-SUMWGTAV
              MOVE "TY-SUMWGTAV"    TO D01-TEXT
              MOVE TY-BRNO          TO D01-BRNO 
              MOVE TY-DATE          TO D01-DATE
              MOVE TY-CLASS         TO D01-CLASS
              MOVE OTY-SUMWGTAV     TO D01-A15-NUM
              MOVE TY-SUMWGTAV      TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
			  
           IF OTY-SUMWGTTA  NOT NUMERIC
              MOVE 0                           TO OTY-SUMWGTTA.
           IF OTY-SUMWGTTA  NOT = TY-SUMWGTTA
              MOVE "TY-SUMWGTTA"               TO D01-TEXT
              MOVE TY-BRNO                     TO D01-BRNO 
              MOVE TY-DATE                     TO D01-DATE
              MOVE TY-CLASS                    TO D01-CLASS
              MOVE OTY-SUMWGTTA                TO D01-A15-NUM
              MOVE TY-SUMWGTTA                 TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OTY-ALLOTMENTS  NOT NUMERIC
              MOVE 0                           TO OTY-ALLOTMENTS.
           IF OTY-ALLOTMENTS  NOT = TY-ALLOTMENTS
              MOVE "TY-ALLOTMENTS"             TO D01-TEXT
              MOVE TY-BRNO                     TO D01-BRNO 
              MOVE TY-DATE                     TO D01-DATE
              MOVE TY-CLASS                    TO D01-CLASS
              MOVE OTY-ALLOTMENTS              TO D01-A15-NUM
              MOVE TY-ALLOTMENTS               TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OTY-BRFG  NOT = TY-BRFG
              MOVE "TY-BRFG"                   TO D01-TEXT
              MOVE TY-BRNO                     TO D01-BRNO 
              MOVE TY-DATE                     TO D01-DATE
              MOVE TY-CLASS                    TO D01-CLASS
              MOVE OTY-BRFG                    TO D01-A15 
              MOVE TY-BRFG                     TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OTY-DLDISC  NOT NUMERIC
              MOVE 0                           TO OTY-DLDISC.
           IF OTY-DLDISC  NOT = TY-DLDISC
              MOVE "TY-DLDISC"                 TO D01-TEXT
              MOVE TY-BRNO                     TO D01-BRNO 
              MOVE TY-DATE                     TO D01-DATE
              MOVE TY-CLASS                    TO D01-CLASS
              MOVE OTY-DLDISC                  TO D01-A15-NUM
              MOVE TY-DLDISC                   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 02
                     AFTER S2 FROM 1 BY 1 UNTIL S2 > 06
               IF OTY-UNEARNED-AMT (S1 S2) NOT NUMERIC
                  MOVE 0 TO OTY-UNEARNED-AMT (S1 S2)
               END-IF
               IF OTY-UNEARNED-AMT (S1 S2) NOT =
                   TY-UNEARNED-AMT (S1 S2)
                  MOVE SPACES                  TO D01-TEXT
                  STRING "TY-UNEARNED-AMT",S1,S2 INTO D01-TEXT
                  MOVE TY-BRNO                 TO D01-BRNO     
                  MOVE TY-DATE                 TO D01-DATE
                  MOVE TY-CLASS                TO D01-CLASS
                  MOVE OTY-UNEARNED-AMT(S1 S2) TO D01-A15-NUM
                  MOVE TY-UNEARNED-AMT(S1 S2)  TO D01-S35-NUM
                  PERFORM WRITE-DETAIL-LINE-ERROR
               END-IF
           END-PERFORM.

           IF OTY-PROCEEDS  NOT NUMERIC
              MOVE 0                TO OTY-PROCEEDS.
            IF OTY-PROCEEDS  NOT = TY-PROCEEDS
               MOVE "TY-PROCEEDS"    TO D01-TEXT
               MOVE TY-BRNO          TO D01-BRNO 
               MOVE TY-DATE          TO D01-DATE
               MOVE TY-CLASS         TO D01-CLASS
               MOVE OTY-PROCEEDS     TO D01-S35-NUM
               PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OTY-LOCKBOX  NOT NUMERIC
              MOVE 0                TO OTY-LOCKBOX.
           IF OTY-LOCKBOX  NOT = TY-LOCKBOX
              MOVE "TY-LOCKBOX"     TO D01-TEXT
              MOVE TY-BRNO          TO D01-BRNO 
              MOVE TY-DATE          TO D01-DATE
              MOVE TY-CLASS         TO D01-CLASS
              MOVE OTY-LOCKBOX      TO D01-A15-NUM
              MOVE TY-LOCKBOX       TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

       EXIT-COMPARE-FIELDS.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE-ERROR SECTION.
      ******************************************************************
      D    STOP MESS.
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
           MOVE SPACES                                TO DETAIL-LINE.

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
           MOVE BEG-CLASS                             TO R01-NUMERIC.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    CLASS               : "    TO R01-PROMPT.
           MOVE END-CLASS                             TO R01-NUMERIC.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING DATE               : "     TO R01-PROMPT.
           MOVE BEG-DATE                              TO R01-NUMERIC.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
 
           MOVE "ENDING    DATE               : "     TO R01-PROMPT.
           MOVE END-DATE                              TO R01-NUMERIC.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "A15 RECORDS PROCESSED         :"     TO R01-PROMPT.
           MOVE A15-RECORDS-PROCESSED                 TO R01-NUMERIC.
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
           MOVE EXT-CONAME-CONAME TO TYFILE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO TYFILE-SYSDATE.
           DISPLAY TYFILE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE TYFILE-FORM-FIELDS TO WR-BUF.
           MOVE TYFILE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE TYFILE-HELP--FILE TO HELP-LINK-FILE.
           MOVE TYFILE-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/TYFILE_EVA.CPY".
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
       COPY "LIBLP/LPTYGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
      * COPY "LIBLP/LPTY1RN.CPY".
      * COPYMEMBER: LIBLP/LPTY1RN
      ***************************************************
       LOAD-TY1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TY-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TY-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TY-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TY-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TY-ALL-DATA
              MOVE TY-ALL-PATH             TO TY-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TY-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TY-ALL-BASE
              MOVE FILPATH-MACHINE         TO TY-ALL-MACHINE
              MOVE FILPATH-DATA            TO TY-ALL-DATA
              MOVE TY-ALL-PATH             TO TY-PATH.

      *-----------------------------------------------------------------
       OPEN-TY1-FILE.
           PERFORM LOAD-TY1-FILE.
           PERFORM OPEN-IT.
           MOVE TY-PATH-SQL TO E-FILE.

           IF TY1-CONN-OPEN-SW = "N" 
              MOVE "Y"         TO TY1-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS TY1_CONNECT 
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-TY1-FILE.
           PERFORM START-IT.
           MOVE TY-PATH-SQL    TO E-FILE.
           MOVE TY1-KEY        TO E-KEYX.

           IF ( TY1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TY1-FILE.

           EXEC SQL SET CONNECTION TY1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE TY1_CURSOR CURSOR FOR
                :TY1-RETURN-CODE = EXEC SSP_TYFILE_TYFILE_SELECT 
              ( :QTY1-WBEG-BRNO, 
                :QTY1-WBEG-DATE, 
                :QTY1-WEND-DATE, 
                :QTY1-WBEG-CLASS,
                :QTY1-WEND-CLASS )      
           END-EXEC.

           EXEC SQL
               OPEN TY1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTY1-WBEG-BRNO
                          QTY1-WEND-BRNO
                          QTY1-WBEG-CLASS
                          QTY1-WEND-CLASS.

           MOVE SPACES TO QTY1-WBEG-DATE
                          QTY1-WEND-DATE.

           MOVE STAT---GOOD TO TY1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-TY1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE TY-PATH-SQL    TO E-FILE.
           MOVE TY1-KEY        TO E-KEYX.
           INITIALIZE QTY-REC.

           EXEC SQL SET CONNECTION TY1_CONNECT END-EXEC.

           IF ( TY1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TY1_CURSOR INTO :QTY-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TY1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TY-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TY1-FILE.

           PERFORM CLOSE-IT.
           IF ( TY1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION TY1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE TY1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO TY1-CURSOR-STAT.

      *----------------------------------------------------------------- 
       CLOSE-TY1-CONNECTION.

           IF TY1-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION TY1_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT TY1_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO TY1-CONN-OPEN-SW.

      *----------------------------------------------------------------- 

       OPEN-OTY1-FILE.
           PERFORM OPEN-IT.
           MOVE OTY-PATH TO E-FILE.
           OPEN INPUT OTY-FILE.
           IF IO-FG = 8
              GO TO OPEN-OTY1-FILE.
           IF IO-FG = 7
              CLOSE OTY-FILE
              GO TO OPEN-OTY1-FILE.
      *READ-OTY1-FILE.
      *    PERFORM READ-IT.
      *    MOVE OTY-PATH TO E-FILE.
      *    MOVE OTY1-KEY TO E-KEYX.
      *    READ OTY-FILE.
      *    IF IO-FG = 8
      *       GO TO READ-OTY1-FILE.
       READ-OTY1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OTY-PATH TO E-FILE.
           MOVE OTY1-KEY TO E-KEYX.
           READ OTY-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OTY1-FILE-NEXT.
       START-OTY1-FILE.
           PERFORM START-IT.
           MOVE OTY-PATH TO E-FILE.
           MOVE OTY1-KEY TO E-KEYX.
           START OTY-FILE KEY NOT < OTY1-KEY.
       CLOSE-OTY1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OTY-FILE.

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
              DESTROY TYFILE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
