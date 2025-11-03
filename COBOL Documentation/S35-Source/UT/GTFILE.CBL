       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GTFILE.
       DATE-WRITTEN.  10-31-2023.
      **********************************************************************
      *
      * DESCRIPTION   : COMPARE S35 GTFILE WITH A15 GTFILE
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
      *  01-07-25  RMZ  ADDED 'BTRACK' - PROCESS TRACKING TO PROGRAM.
      *  04-21-25  MB   CHANGED TO USE R1 AND R2 GTFILE BASED ON BR-MACHINE
      *  04-30-25  MB   BYPASS CHECK GT-SMITH-SENT
      *  05-01-25  MB   BYPASS USERID CHECK IF ENT-GROUP = 'S351' USER TESTING
      ************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
      * COPYMEMBER: LIBGL/GLFSGT
           SELECT OGT-FILE ASSIGN TO OGT-PATH
                  ORGANIZATION INDEXED
                  ACCESS MODE DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OGT1-KEY
                  ALTERNATE RECORD KEY OGT2-KEY WITH DUPLICATES
                  ALTERNATE RECORD KEY OGT3-KEY WITH DUPLICATES
                  ALTERNATE RECORD KEY OGT4-KEY WITH DUPLICATES
                  FILE STATUS FILE-STAT.

           SELECT OGTR2-FILE ASSIGN TO OGTR2-PATH
                  ORGANIZATION INDEXED
                  ACCESS MODE DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OGTR21-KEY
                  ALTERNATE RECORD KEY OGTR22-KEY WITH DUPLICATES
                  ALTERNATE RECORD KEY OGTR23-KEY WITH DUPLICATES
                  ALTERNATE RECORD KEY OGTR24-KEY WITH DUPLICATES
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  OGT-FILE
           BLOCK CONTAINS 1024 TO 1024 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A15SRC/GL01GT.CPY" REPLACING LEADING "GT" BY "OGT".

       FD  OGTR2-FILE
           BLOCK CONTAINS 1024 TO 1024 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A15SRC/GL01GT.CPY" REPLACING LEADING "GT" BY "OGTR2".

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/GTFILE S35 04/27/25-11:07:33 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGL/GL01GT.CPY".
       COPY "LIBGL/GL01GT_SQL.CPY".
       COPY "LIBGL/GLWSGT.CPY".

       01  OGT-PATH                    PIC X(35) VALUE SPACES.
       01  OGT-ALL-PATH.
           03  OGT-ALL-BASE            PIC X(9).
           03  FILLER                  PIC X     VALUE "/".
           03  OGT-ALL-MACHINE         PIC XX.
           03  FILLER                  PIC X(10) VALUE
               "/GL/GTFILE".

       01  OGTR2-PATH                  PIC X(35) VALUE SPACES.
       01  OGTR2-ALL-PATH.
           03  OGTR2-ALL-BASE          PIC X(9).
           03  FILLER                  PIC X     VALUE "/".
           03  OGTR2-ALL-MACHINE       PIC XX.
           03  FILLER                  PIC X(10) VALUE
               "/GL/GTFILE".

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

       01  FILE-ERROR-SW               PIC X      VALUE "N".
       01  GT-EOF-SW                   PIC X      VALUE "N".
       01  OGT-EOF-SW                  PIC X      VALUE "N".
       01  GT1-CONN-OPEN-SW            PIC X      VALUE "N".
       01  TOTAL-ERRORS-CTR            PIC 9(9)   VALUE 0.
       01  A15-FND-SW                   PIC X      VALUE "N".
       01  USING-R2-REC-SW             PIC X      VALUE "N".
       01  HOLD-GT-REC                 PIC X(OGT-SIZE) VALUE SPACES.
       01  FIRST-SW                    PIC X      VALUE "Y".
       01  PREV-BR9991                 PIC X      VALUE "N".
       01  PROC-OGT-BRANCH             PIC 9(4)   VALUE 0.
       01  USE-A15-BEG-DATE            PIC 9(08).

       01  USE-GT-KEY.
           03  USE-GT-KEY-BRANCH       PIC 9(4).
           03  USE-GT-KEY-ACCTNO       PIC 9(8).
           03  USE-GT-KEY-DATE3        PIC 9(8).
           03  USE-GT-KEY-DRCRFG       PIC X.
           03  USE-GT-KEY-LINE         PIC 9(6).

       01  USE-OGT-KEY.
           03  USE-OGT-KEY-BRANCH       PIC 9(4).
           03  USE-OGT-KEY-ACCTNO       PIC 9(8).
           03  USE-OGT-KEY-DATE3        PIC 9(8).
           03  USE-OGT-KEY-DRCRFG       PIC X.
           03  USE-OGT-KEY-LINE         PIC 9(6).

       01  MACHINE-R1.
           03  FILLER                  PIC X VALUE X"72".
           03  FILLER                  PIC 9 VALUE 1.
       01  MACHINE-R2.
           03  FILLER                  PIC X VALUE X"72".
           03  FILLER                  PIC 9 VALUE 2.

       01  PROCESS-STAT                PIC X(08)    VALUE SPACES.
           88  PROCESS-STAT-GOOD                    VALUE "00".
           88  PROCESS-STAT-BAD                     VALUE "  " THRU "//"
                                                         "01" THRU "~~".
       01  SQL-RECORDS-PROCESSED       PIC 9(9)  VALUE 0.
       01  A15-RECORDS-PROCESSED       PIC 9(9)  VALUE 0.
           88  NO-RECORDS-FOUND                     VALUE ZEROES.

       01  STARTING-TIME               PIC X(08)     VALUE SPACES.
       01  SUB                         PIC 9(04)     VALUE ZEROES.

       01  SYSDATE-YYYYMMDD            PIC 9(8)     VALUE ZEROES.

       01  LEGEND                      PIC X(79)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(07)     VALUE "LEGEND.".

       01  WS-OGT-REF-MDY.  
           03 WS-OGTF-MDY-13           PIC X(03).
           03 WS-OGTF-MDY-MM           PIC X(02).
           03 WS-OGTF-MDY-DD           PIC X(02).
           03 WS-OGTF-MDY-YY           PIC X(02).
           03 WS-OGTF-MDY-1014         PIC X(05).

       01  WS-OGT-REF-YMD.  
           03 WS-OGTF-YMD-13           PIC X(03).
           03 WS-OGTF-YMD-YY           PIC X(02).
           03 WS-OGTF-YMD-MM           PIC X(02).
           03 WS-OGTF-YMD-DD           PIC X(02).
           03 WS-OGTF-YMD-1014         PIC X(05).

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
           03  FILLER                  PIC X(6)  VALUE SPACES.
           03  H01-NAME                PIC X(40) VALUE SPACES.
           03  FILLER                  PIC X(26) VALUE SPACES.
           03  FILLER                  PIC X(06) VALUE "PAGE: ".
           03  H01-PAGE                PIC Z(04).

      *-----------------------------------------------------------------
       01  HEADER-LINE-02.
           03  FILLER                  PIC X(10) VALUE "USER ID:".
           03  H02-USERID              PIC X(10) VALUE SPACES.
           03  FILLER                  PIC X(10) VALUE SPACES.
           03  H02-TITLE               PIC X(50) VALUE 
           "GL TRANS FILE (GTFILE) COMPARE A15 TO S35".

       01  HEADER-LINE-03.
           03  FILLER                PIC X(4) VALUE "BRNO".
           03  FILLER                PIC X(2) VALUE SPACES.
           03  FILLER                PIC X(8) VALUE "  GLNO  ".
           03  FILLER                PIC X(2) VALUE SPACES.
           03  FILLER                PIC X(8) VALUE "  DATE  ".
           03  FILLER                PIC X(2) VALUE SPACES.
           03  FILLER                PIC X(6) VALUE "DRCRFG".
           03  FILLER                PIC X(2) VALUE SPACES.
           03  FILLER                PIC X(6) VALUE " LINE ".
           03  FILLER                PIC X(2) VALUE SPACES.
           03  FILLER                PIC X(27) VALUE "DESCRIPTION".
           03  FILLER                PIC X(22) VALUE "A15".
           03  FILLER                PIC X(3) VALUE "S35".

      ******************************************************************
      *
      *    D E T A I L - L I N E - W O R K E R S
      *
      ******************************************************************
       01  DETAIL-LINE                        PIC X(100)   VALUE SPACES.

       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-BRNO                       PIC Z(3)9.
           03  FILLER                         PIC X(2).
           03  D01-GLNO                       PIC ZZZZZZZ9.
           03  FILLER                         PIC X(2).
           03  D01-DATE                       PIC 9(8).
           03  FILLER                         PIC X(4).
           03  D01-DRCRFG                     PIC X.
           03  FILLER                         PIC X(4).
           03  D01-LINE                       PIC ZZZZZ9.
           03  FILLER                         PIC X(3).
           03  D01-TEXT                       PIC X(20).
           03  FILLER                         PIC X(2).
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
       COPY "LIBUT/GTFILE_DEF.CPY".
       COPY "LIBUT/GTFILE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/GTFILE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE OGT-FILE OGTR2-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-GT1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           PERFORM CLOSE-GT1-CONNECTION.
           CLOSE OGT-FILE OGTR2-FILE.
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
           MOVE "GTFILE" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE GTFILE-QUEUE-POS TO Q-POS.
           MOVE GTFILE-COPIES-POS TO C-POS.
           MOVE GTFILE-PRU-POS   TO P-POS.
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

           PERFORM START-MISC-CSR.
           PERFORM READ-MISC-CSR-NEXT.
           PERFORM CLOSE-MISC-CSR.

           MOVE SOURCE-PATH TO OGT-ALL-BASE OGTR2-ALL-BASE.
           MOVE MACHINE-R1  TO OGT-ALL-MACHINE.
           MOVE MACHINE-R2  TO OGTR2-ALL-MACHINE.

           GO TO INITIALIZATION-EXIT.

       INITIALIZATION-END.

           MOVE "X"                        TO EXT-EOBUF-ERRCD.
           MOVE STAT---BAD                 TO PROCESS-STAT.

       INITIALIZATION-EXIT.
           EXIT.

      ***************************************************
       ENTER-PARAMETERS SECTION.
      ***************************************************

           MOVE STAT---GOOD  TO PROCESS-STAT.

           IF ( UP-KEY )
              GO TO ENTER-END-DATE.

       ENTER-SOURCE-PATH.

           IF SOURCE-PATH = SPACES
              MOVE "A15_PATH" TO GETENV-BUF
              PERFORM GETENV-CALL
              IF ( STAT-GOOD )
                 MOVE GETENV-BUF TO SOURCE-PATH
              ELSE
                 MOVE "/US2/DA15" TO SOURCE-PATH
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


       ENTER-BEG-DATE.

           MOVE "ERNM060BEG-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-BRANCH.
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

           MOVE "S  A090S-PATH."           TO VDU.
           MOVE SOURCE-PATH                TO VDUBUF.
           PERFORM SCREENIO.

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

           MOVE "S  8080BEG-DATE."  TO VDU.
           MOVE BEG-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

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

           PERFORM PROCESS-BRANCH-DRIVER.

           IF FILE-ERROR-SW = "N"
              GO TO EXECUTION-NEXT-GROUP.

       EXECUTION-MODULE-EXIT.
           EXIT.

      *********************************************************
       PROCESS-BRANCH-DRIVER SECTION.
      *********************************************************
      D    STOP MESS.

           MOVE 9999 TO PROGID-SLEEP-COUNTER.
           PERFORM SEND-PROGRESS-INDICATOR.

           MOVE "N" TO GT-EOF-SW OGT-EOF-SW USING-R2-REC-SW.
 
           MOVE EXT-FILPATH-BASE  TO FILPATH-BASE.
           MOVE BR-MACHINE        TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH      TO FILPATH-DATA.

           MOVE BR-NO     TO QGT1-WBEG-BRANCH.
           MOVE BEG-DATE  TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD   TO QGT1-WBEG-DATE3.
           MOVE END-DATE              TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD   TO QGT1-WEND-DATE3.

           MOVE "PROCESSING S35 DATA" TO BTRACK-MESS.
           PERFORM BTRACK-GLOBAL.

           PERFORM OPEN-GT1-FILE.
           PERFORM START-GT1-FILE.    *> SSP GTFILE
           PERFORM M-READ-GT1.

           MOVE "PROCESSING A15 DATA" TO BTRACK-MESS.  
           PERFORM BTRACK-GLOBAL.

      * NEED TO OPEN/REOPEN OGT IF FIRST TIME, BRANCH 9991 OR NEXT BRANCH AFTER 9991.
           IF FIRST-SW = "Y" OR BR-NO = 9991 OR PREV-BR9991 = "Y"
              PERFORM OPEN-OGT-FILES
              IF FILE-ERROR-SW = "Y"
                 GO TO PROCESS-BRANCH-DRIVER-END
              END-IF
              IF FIRST-SW = "Y"
                 MOVE "N" TO FIRST-SW
              END-IF.

      * MUST USE ZEROS FOR OGT-DATE3 SINCE IT IS IN YYMMDD FORMAT (NOT YYYYMMDD)
      *  SO CAN NOT USE BEG-DATE
      *     MOVE BEG-DATE    TO DATE-YYYYMMDD.
      *     PERFORM CONVERT-YYYYMMDD-TO-YYMMDD.
      *     MOVE DATE-YYMMDD TO OGT-DATE3.
           IF BR-NO = 9991
              MOVE 9990             TO OGT-BRANCH PROC-OGT-BRANCH
              MOVE USE-GT-KEY-DATE3 TO USE-A15-BEG-DATE
           ELSE 
              MOVE BR-NO    TO OGT-BRANCH PROC-OGT-BRANCH
              MOVE BEG-DATE TO USE-A15-BEG-DATE.

           MOVE ZEROS       TO OGT-ACCTNO OGT-LINE OGT-DATE3.
           MOVE SPACE       TO OGT-DRCRFG.
           PERFORM START-OGT4-FILE.
           PERFORM M-READ-OGT.

           PERFORM PROCESS-BRANCH-NEXT-GT
              UNTIL OGT-EOF-SW = "Y" AND GT-EOF-SW = "Y".

           PERFORM CLOSE-GT1-FILE.

           IF BR-NO = 9991
              MOVE "Y" TO PREV-BR9991
           ELSE
              MOVE "N" TO PREV-BR9991.

           GO TO PROCESS-BRANCH-DRIVER-END.

       PROCESS-BRANCH-NEXT-GT.

           PERFORM SEND-PROGRESS-INDICATOR.

           PERFORM COMPARE-FIELDS.

           IF USE-OGT-KEY < USE-GT-KEY
              PERFORM M-READ-OGT 
           ELSE
              IF USE-OGT-KEY > USE-GT-KEY
                 PERFORM M-READ-GT1
              ELSE
                 PERFORM M-READ-OGT
                 PERFORM M-READ-GT1.

       PROCESS-BRANCH-DRIVER-END.
           EXIT.

       OPEN-OGT-FILES SECTION.
      * FOR BRANCH 9991 DRIVE OFF R2 GTFILE - SET R1 PATH TO R2 PATH. DO NOT OPEN R2
      * NEED TO RESET R1 PATH WHEN NEXT BRANCH AFTER 9991 PROCESSED.

           PERFORM CLOSE-OGT4-FILE.
           PERFORM CLOSE-OGTR2-FILE.

           IF BR-NO = 9991
              MOVE OGTR2-ALL-PATH TO OGT-ALL-PATH
           ELSE
              MOVE SOURCE-PATH  TO OGT-ALL-BASE
              MOVE MACHINE-R1   TO OGT-ALL-MACHINE.

           MOVE OGT-ALL-PATH TO OGT-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE "MISSING R1 GTFILE " TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-ERROR
              MOVE "Y" TO FILE-ERROR-SW
              GO TO OPEN-OGT-FILES-EXIT.

           PERFORM OPEN-OGT4-FILE.

           IF BR-NO = 9991
              GO TO OPEN-OGT-FILES-EXIT.

           MOVE OGTR2-ALL-PATH TO OGTR2-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE "MISSING R2 GTFILE " TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-ERROR
              MOVE "Y" TO FILE-ERROR-SW
              GO TO OPEN-OGT-FILES-EXIT.

           PERFORM OPEN-OGTR2-FILE.

       OPEN-OGT-FILES-EXIT.
           EXIT.      

       M-READ-GT1 SECTION.
           PERFORM READ-GT1-FILE-NEXT.   *> SSP GTFILE
           IF IO-BAD
              MOVE ALL '9' TO USE-GT-KEY
              MOVE "Y"     TO GT-EOF-SW
           ELSE
              MOVE GT-BRANCH  TO USE-GT-KEY-BRANCH
              MOVE GT-ACCTNO  TO USE-GT-KEY-ACCTNO 
              MOVE GT-DATE3   TO USE-GT-KEY-DATE3
              MOVE GT-DRCRFG  TO USE-GT-KEY-DRCRFG
              MOVE GT-LINE    TO USE-GT-KEY-LINE
              ADD 1           TO SQL-RECORDS-PROCESSED.

       M-READ-OGT SECTION.
           PERFORM READ-OGT4-FILE-NEXT.

           IF IO-BAD OR OGT-BRANCH > PROC-OGT-BRANCH
              MOVE ALL '9'  TO USE-OGT-KEY
              MOVE "Y"      TO OGT-EOF-SW
           ELSE 
              MOVE OGT-DATE3  TO DATE-YYMMDD
              PERFORM CONVERT-YYMMDD-TO-YYYYMMDD
              IF ( DATE-YYYYMMDD < USE-A15-BEG-DATE ) OR
                 ( DATE-YYYYMMDD > END-DATE )
                 GO TO M-READ-OGT
              END-IF
              MOVE BR-NO            TO USE-OGT-KEY-BRANCH
              MOVE OGT-ACCTNO       TO USE-OGT-KEY-ACCTNO
              MOVE DATE-YYYYMMDD    TO USE-OGT-KEY-DATE3
              MOVE OGT-DRCRFG       TO USE-OGT-KEY-DRCRFG
              MOVE OGT-LINE         TO USE-OGT-KEY-LINE
              ADD 1                 TO A15-RECORDS-PROCESSED
              MOVE OGT-REC          TO HOLD-GT-REC.

      ******************************************************************
       COMPARE-FIELDS SECTION.
      ******************************************************************

            IF USE-OGT-KEY < USE-GT-KEY
              MOVE "MISSING S35 RECORD" TO D01-TEXT
              MOVE USE-OGT-KEY-BRANCH   TO D01-BRNO
              MOVE USE-OGT-KEY-ACCTNO   TO D01-GLNO
              MOVE USE-OGT-KEY-DATE3    TO D01-DATE
              MOVE USE-OGT-KEY-DRCRFG   TO D01-DRCRFG
              MOVE USE-OGT-KEY-LINE     TO D01-LINE
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.

           MOVE "Y" TO A15-FND-SW.
           IF USE-OGT-KEY > USE-GT-KEY
              MOVE "N" TO A15-FND-SW
              IF BR-MACHINE = MACHINE-R2  
                 PERFORM CHECK-A15-R2
              END-IF
              IF A15-FND-SW = "N"
                 MOVE "MISSING A15 RECORD" TO D01-TEXT
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
                 GO TO EXIT-COMPARE-FIELDS
              ELSE
                 MOVE OGTR2-REC TO OGT-REC
                 MOVE "Y"       TO USING-R2-REC-SW.

           IF GT-FLAG NOT = OGT-FLAG
              MOVE "GT-FLAG"  TO D01-TEXT
              MOVE OGT-FLAG   TO D01-A15
              MOVE GT-FLAG    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OGT-REF        TO WS-OGT-REF-MDY.
           MOVE WS-OGT-REF-MDY TO WS-OGT-REF-YMD.
           MOVE WS-OGTF-MDY-YY TO WS-OGTF-YMD-YY.
           MOVE WS-OGTF-MDY-MM TO WS-OGTF-YMD-MM.
           MOVE WS-OGTF-MDY-DD TO WS-OGTF-YMD-DD.

           IF GT-REF = OGT-REF 
              NEXT SENTENCE
           ELSE 
              IF GT-REF = WS-OGT-REF-YMD
                 NEXT SENTENCE
              ELSE
                 MOVE "GT-REF"   TO D01-TEXT
                 MOVE OGT-REF    TO D01-A15
                 MOVE GT-REF     TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OGT-DATE1 TO DATE-YYMMDD.
           PERFORM CONVERT-YYMMDD-TO-YYYYMMDD
           PERFORM TEST-INVALID-DATE.
           IF GT-DATE1 NOT = DATE-YYYYMMDD
              MOVE "GT-DATE1"     TO D01-TEXT
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE GT-DATE1       TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OGT-SOURCE NOT NUMERIC
              MOVE 0 TO OGT-SOURCE.
           IF GT-SOURCE NOT = OGT-SOURCE
              MOVE "GT-SOURCE" TO D01-TEXT
              MOVE OGT-SOURCE  TO D01-A15
              MOVE GT-SOURCE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OGT-DATE2 TO DATE-YYMMDD.
           PERFORM CONVERT-YYMMDD-TO-YYYYMMDD
           PERFORM TEST-INVALID-DATE.
           IF GT-DATE2 NOT = DATE-YYYYMMDD
              MOVE "GT-DATE2"     TO D01-TEXT
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE GT-DATE2       TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OGT-ACCTNO NOT NUMERIC
              MOVE 0 TO OGT-ACCTNO.
           IF GT-ACCTNO NOT = OGT-ACCTNO
              MOVE "GT-ACCTNO" TO D01-TEXT
              MOVE OGT-ACCTNO  TO D01-A15-NUM
              MOVE GT-ACCTNO   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OGT-DATE3 TO DATE-YYMMDD.
           PERFORM CONVERT-YYMMDD-TO-YYYYMMDD
           PERFORM TEST-INVALID-DATE.
           IF GT-DATE3 NOT = DATE-YYYYMMDD
              MOVE "GT-DATE3"    TO D01-TEXT
              MOVE DATE-YYYYMMDD TO D01-A15
              MOVE GT-DATE3      TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GT-DRCRFG NOT = OGT-DRCRFG
              MOVE "GT-DRCRFG" TO D01-TEXT
              MOVE OGT-DRCRFG  TO D01-A15
              MOVE GT-DRCRFG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OGT-LINE NOT NUMERIC
              MOVE 0 TO OGT-LINE.
           IF GT-LINE NOT = OGT-LINE
              MOVE "GT-LINE" TO D01-TEXT
              MOVE OGT-LINE  TO D01-A15-NUM
              MOVE GT-LINE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OGT-AMT NOT NUMERIC
              MOVE 0 TO OGT-AMT.
           IF GT-AMT NOT = OGT-AMT
              MOVE "GT-AMT" TO D01-TEXT
              MOVE OGT-AMT  TO D01-A15-NUM
              MOVE GT-AMT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GT-EXPL NOT = OGT-EXPL
              MOVE "GT-EXPL" TO D01-TEXT
              MOVE OGT-EXPL  TO D01-A15
              MOVE GT-EXPL   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OGT-PERIOD NOT NUMERIC
              MOVE 0 TO OGT-PERIOD.
           IF GT-PERIOD NOT = OGT-PERIOD
              MOVE "GT-PERIOD" TO D01-TEXT
              MOVE OGT-PERIOD  TO D01-A15-NUM
              MOVE GT-PERIOD   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

      *    IF GT-SMITH-SENT NOT = OGT-SMITH-SENT
      *       MOVE "GT-SMITH-SENT" TO D01-TEXT
      *       MOVE OGT-SMITH-SENT  TO D01-A15
      *       MOVE GT-SMITH-SENT   TO D01-S35
      *       PERFORM WRITE-DETAIL-LINE-ERROR-KEY
      *       PERFORM WRITE-DETAIL-LINE-ERROR.

           IF ENT-GROUP NOT = 'S351'
              IF GT-POST-USERID NOT = OGT-POST-USERID
                 MOVE "GT-POST-USERID" TO D01-TEXT
                 MOVE OGT-POST-USERID  TO D01-A15
                 MOVE GT-POST-USERID   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OGT-POST-DATE NOT NUMERIC OR OGT-POST-DATE = 202020
              MOVE 0 TO OGT-POST-DATE.
           MOVE OGT-POST-DATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
           PERFORM TEST-INVALID-DATE.
           IF GT-POST-DATE NOT = DATE-YYYYMMDD
              MOVE "GT-POST-DATE" TO D01-TEXT
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE GT-POST-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

      *     IF OGT-POST-TIME NOT NUMERIC
      *        MOVE 0 TO OGT-POST-TIME.
      *     IF GT-POST-TIME NOT = OGT-POST-TIME
      *        MOVE "GT-POST-TIME" TO D01-TEXT
      *        MOVE OGT-POST-TIME  TO D01-A15-NUM
      *        MOVE GT-POST-TIME   TO D01-S35-NUM.
XXX   *        PERFORM WRITE-DETAIL-LINE-ERROR-KEY
XXX   *        PERFORM WRITE-DETAIL-LINE-ERROR.

            IF USING-R2-REC-SW = "Y"
               MOVE HOLD-GT-REC TO OGT-REC
               MOVE "N"         TO USING-R2-REC-SW.

       EXIT-COMPARE-FIELDS.
           EXIT.

       CHECK-A15-R2 SECTION.
           MOVE GT-SOURCE   TO OGTR2-SOURCE.
           MOVE GT-DATE2    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYMMDD.
           MOVE DATE-YYMMDD TO OGTR2-DATE2.

           MOVE GT-BRANCH   TO OGTR2-BRANCH.
           MOVE GT-ACCTNO   TO OGTR2-ACCTNO.

           MOVE GT-DATE3    TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-YYMMDD.
           MOVE DATE-YYMMDD TO OGTR2-DATE3.

           MOVE GT-DRCRFG   TO OGTR2-DRCRFG.
           MOVE GT-LINE     TO OGTR2-LINE.

           PERFORM READ-OGTR2-FILE.
           IF IO-BAD
             MOVE "N" TO A15-FND-SW
           ELSE 
             ADD 1          TO A15-RECORDS-PROCESSED
             MOVE "Y"       TO A15-FND-SW.

      ******************************************************************
       WRITE-DETAIL-LINE-ERROR-KEY SECTION.
      ******************************************************************

           MOVE BR-NO      TO D01-BRNO.
           MOVE GT-ACCTNO  TO D01-GLNO.
           MOVE GT-DATE3   TO D01-DATE.
           MOVE GT-DRCRFG  TO D01-DRCRFG.
           MOVE GT-LINE    TO D01-LINE.

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

           MOVE SPACES                                TO DETAIL-LINE
                                                         PR-REC.

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

           MOVE "BEGINNING DATE                : "    TO R01-PROMPT.
           MOVE BEG-DATE                              TO R01-FIELD.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    GLNO                : "    TO R01-PROMPT.
           MOVE END-DATE                              TO R01-FIELD.
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

           MOVE "SOURCE PATH   : "                    TO R01-PROMPT.
           MOVE SOURCE-PATH                           TO R01-TEXT.
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
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/PROGID.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO GTFILE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GTFILE-SYSDATE.
           DISPLAY GTFILE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE GTFILE-FORM-FIELDS TO WR-BUF.
           MOVE GTFILE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE GTFILE-HELP--FILE TO HELP-LINK-FILE.
           MOVE GTFILE-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/GTFILE_EVA.CPY".
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
       COPY "LIBGL/GLGTGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

      * COPY "LIBGL/GLGT1RN.CPY".
      * COPYMEMBER: LIBGL/GLGT1R
       LOAD-GT1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( GT-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( GT-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO GT-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO GT-ALL-MACHINE
              MOVE GT-ALL-PATH             TO GT-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( GT-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO GT-ALL-BASE
              MOVE FILPATH-MACHINE         TO GT-ALL-MACHINE
              MOVE GT-ALL-PATH             TO GT-PATH.

      *-----------------------------------------------------------------
       OPEN-GT1-FILE.
           PERFORM LOAD-GT1-FILE.
           PERFORM OPEN-IT.
           MOVE GT-PATH-SQL TO E-FILE.

           IF GT1-CONN-OPEN-SW = "N" 
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS GT1_CONNECT
                        USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE 'Y' TO GT1-CONN-OPEN-SW.

      *-----------------------------------------------------------------
       START-GT1-FILE.
           PERFORM START-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT1-KEY TO E-KEYX.
           IF ( GT1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-GT1-FILE.

           EXEC SQL SET CONNECTION GT1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE GT1_CURSOR CURSOR FOR
                :GT1-RETURN-CODE =  
            EXEC SSP_GTFILE_GTFILE_SELECT
              ( :QGT1-WBEG-BRANCH,
                :QGT1-WBEG-DATE3, :QGT1-WEND-DATE3)
           END-EXEC.

      *     EXEC SQL
      *      DECLARE GT1_CURSOR CURSOR FOR
      *     SELECT *
      *     FROM DBO.GTFILE GTFILE
      *     WHERE GTFILE.GT_BRNO = @QGT1_WBEG_BRANCH
      *      AND GTFILE.GT_DATE3 BETWEEN @QGT1_WBEG_DATE3 AND @QGT1_WEND_DATE3
      *      ORDER BY GTFILE.GT_BRANCH, GTFILE.GT_ACCTNO, GTFILE.GT_DATE3, GTFILE.GT_DRCRFG, 
      *               GTFILE.GT_LINE
      *     END-EXEC.

           EXEC SQL
               OPEN GT1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE STAT---GOOD TO GT1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-GT1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE GT-PATH-SQL TO E-FILE.
           MOVE GT1-KEY TO E-KEYX.
           INITIALIZE QGT-REC.

           EXEC SQL SET CONNECTION GT1_CONNECT END-EXEC.

           IF ( GT1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH GT1_CURSOR INTO :QGT-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GT1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-GT-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-GT1-FILE.
           PERFORM CLOSE-IT.
           IF ( GT1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION GT1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE GT1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO GT1-CURSOR-STAT
              EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-GT1-CONNECTION.

           IF GT1-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION GT1_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT GT1_CONNECT END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE "N" TO GT1-CONN-OPEN-SW.

      *-----------------------------------------------------------------
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
       OPEN-OGT4-FILE.
           PERFORM OPEN-IT.
           MOVE OGT-PATH TO E-FILE.
           OPEN INPUT OGT-FILE.
           IF IO-FG = 8
              GO TO OPEN-OGT4-FILE.
           IF IO-FG = 7
              CLOSE OGT-FILE
              GO TO OPEN-OGT4-FILE.
      *READ-OGT1-FILE.
      *    PERFORM READ-IT.
      *    MOVE OGT-PATH TO E-FILE.
      *    MOVE OGT1-KEY TO E-KEYX.
      *    READ OGT-FILE.
      *    IF IO-FG = 8
      *       GO TO READ-OGT1-FILE.
       READ-OGT4-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OGT-PATH TO E-FILE.
           MOVE OGT4-KEY TO E-KEYX.
           READ OGT-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OGT4-FILE-NEXT.
       START-OGT4-FILE.
           PERFORM START-IT.
           MOVE OGT-PATH TO E-FILE.
           MOVE OGT4-KEY TO E-KEYX.
           START OGT-FILE KEY NOT < OGT4-KEY.
       CLOSE-OGT4-FILE.
           PERFORM CLOSE-IT.
           CLOSE OGT-FILE.
      * COPYMEMBER: LIBGL/GLGT1R

       OPEN-OGTR2-FILE.
           PERFORM OPEN-IT.
           MOVE OGTR2-PATH TO E-FILE.
           OPEN INPUT OGTR2-FILE.
           IF IO-FG = 8
              GO TO OPEN-OGTR2-FILE.
           IF IO-FG = 7
              CLOSE OGTR2-FILE
              GO TO OPEN-OGTR2-FILE.
       READ-OGTR2-FILE.
           PERFORM READ-IT.
           MOVE OGTR2-PATH TO E-FILE.
           MOVE OGTR21-KEY TO E-KEYX.
           READ OGTR2-FILE.
           IF IO-FG = 8
             GO TO READ-OGTR2-FILE.
       CLOSE-OGTR2-FILE.
           PERFORM CLOSE-IT.
           CLOSE OGTR2-FILE.

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
              DESTROY GTFILE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
