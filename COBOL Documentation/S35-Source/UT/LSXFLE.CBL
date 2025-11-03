       IDENTIFICATION DIVISION.
       PROGRAM-ID.    LSXFLE.
       DATE-WRITTEN.  10-24-2023.
      ******************************************************************
      *
      * DESCRIPTION   : COMPARE S35 LSXFILE WITH A15 LSFILE "X"
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
      * 04-15-2025 MB  - USE 0 FOR A15 FIELDS LSX-CYLINDERS, LSX-AGENT-PHONE
      *                   LSX-AGENT-FAX IF VALUE = 20, 02, 2020202020
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
      * COPYMEMBER: LIBLP/LPFSLS
           SELECT OLS-FILE ASSIGN TO OLS-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OLS1-KEY
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  OLS-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A15SRC/LP01LS.CPY" REPLACING LEADING "LS" BY "OLS".

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/LSXFLE S35 11/03/23-08:06:15 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LSX.CPY".
       COPY "LIBLP/LP01LSX_SQL.CPY".
       COPY "LIBLP/LPWSLSX.CPY".
       01  OLS-PATH                    PIC X(35) VALUE SPACES.
       01  OLS-ALL-PATH.
           03  OLS-ALL-BASE     PIC X(9).
           03  FILLER           PIC X     VALUE "/".
           03  OLS-ALL-MACHINE  PIC XX.
           03  FILLER           PIC X     VALUE "/".
           03  OLS-ALL-DATA     PIC X(6).
           03  FILLER           PIC X(10) VALUE
               "/LP/LSFILE".

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
           03  NEW-SEQNO               PIC 99.


       01  MISC-CURSOR-STAT            PIC X(2) VALUE SPACES.
           88  MISC-CURSOR-STAT-GOOD            VALUE "00".
           88  MISC-CURSOR-STAT-BAD             VALUE "  " THRU "//"
                                                      "01" THRU "~~".

       01  PROCESS-STAT                PIC X(08)    VALUE SPACES.
           88  PROCESS-STAT-GOOD                    VALUE "00".
           88  PROCESS-STAT-BAD                     VALUE "  " THRU "//"
                                                         "01" THRU "~~".

       01  TOTAL-ERRORS-CTR            PIC 9(9)      VALUE 0.
       01  S35-RECORDS-PROCESSED       PIC 9(9)      VALUE 0.
       01  A15-RECORDS-PROCESSED       PIC 9(9)      VALUE 0.
           88  NO-RECORDS-FOUND                      VALUE ZEROES.

       01  STARTING-TIME               PIC X(08)     VALUE SPACES.
       01  SUB                         PIC 9(04)     VALUE ZEROES.

       01  SYSDATE-YYYYMMDD            PIC 9(8)      VALUE ZEROES.

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
           "REPO TRAILER FILE (LSXFILE) COMPARE A15 TO S35".

       01  HEADER-LINE-03.
           03  FILLER                PIC X(4) VALUE "BRNO".
           03  FILLER                PIC X(3) VALUE SPACES.
           03  FILLER                PIC X(8) VALUE " ACCTNO ".
           03  FILLER                PIC X(3) VALUE SPACES.
          03  FILLER                 PIC X(4) VALUE "OSEQ".
           03  FILLER                PIC X(3) VALUE SPACES.
           03  FILLER                PIC X(28) VALUE "DESCRIPTION".
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
           03  FILLER                         PIC X(3).
           03  D01-ACCTNO                     PIC Z(7)9.
           03  FILLER                         PIC X(3).
           03  D01-OSEQ                       PIC ZZZ9.
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
       COPY "LIBUT/LSXFLE_DEF.CPY".
       COPY "LIBUT/LSXFLE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/LSXFLE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE OLS-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LSX1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE OLS-FILE.
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
           MOVE "LSXFLE" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE LSXFLE-QUEUE-POS TO Q-POS.
           MOVE LSXFLE-COPIES-POS TO C-POS.
           MOVE LSXFLE-PRU-POS   TO P-POS.
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
           MOVE SOURCE-PATH  TO OLS-ALL-BASE.
           MOVE MACHINE-R1   TO OLS-ALL-MACHINE.
           MOVE BR-DATA-PATH TO OLS-ALL-DATA.
           MOVE OLS-ALL-PATH TO OLS-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE MACHINE-R2 TO OLS-ALL-MACHINE
              MOVE OLS-ALL-PATH TO OLS-PATH ACCESS-BUF
              PERFORM ACCESS-CALL
              IF ( STAT-BAD )
                 MOVE "MISSING A15 FILE" TO D01-TEXT 
                 MOVE BR-NO              TO D01-BRNO
                 MOVE OLS-ALL-PATH       TO D01-A15
                 PERFORM WRITE-DETAIL-LINE
                 GO TO PROCESS-BRANCH-DRIVER-END.


           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO LSX-PATH-OVERRIDE.

           PERFORM OPEN-LSX1-FILE.
           PERFORM OPEN-OLS1-FILE.

           MOVE BEG-ACCTNO TO NEW-ACCTNO.
           MOVE 01         TO NEW-SEQNO.
           MOVE OLD-ACCTNO TO OLS-ACCTNO.
           MOVE "X"        TO OLS-CODE.
           MOVE 0          TO OLS-SEQNO.

           PERFORM START-OLS1-FILE.

       PROCESS-BRANCH-NEXT-LSX.
           PERFORM READ-OLS1-FILE-NEXT.
           IF IO-BAD 
              GO TO PROCESS-BRANCH-DRIVER-END.

           PERFORM SEND-PROGRESS-INDICATOR.

           IF OLS-CODE NOT = "X" AND NOT = "Y"
              GO TO PROCESS-BRANCH-NEXT-LSX.

           MOVE OLS-ACCTNO TO OLD-ACCTNO.
           IF ( NEW-ACCTNO < BEG-ACCTNO ) OR
              ( NEW-ACCTNO > END-ACCTNO )
              GO TO PROCESS-BRANCH-NEXT-LSX.

           ADD 1 TO A15-RECORDS-PROCESSED.

           IF OLS-CODE = "X"
              PERFORM COMPARE-X-FIELDS
           ELSE
              PERFORM COMPARE-Y-FIELDS.

           GO TO PROCESS-BRANCH-NEXT-LSX.

       PROCESS-BRANCH-DRIVER-END.

           PERFORM CLOSE-LSX1-FILE.

       PROCESS-BRANCH-DRIVER-EXIT.
           EXIT.

      ******************************************************************
       COMPARE-X-FIELDS SECTION.
      ******************************************************************

           MOVE LSX-PATH-OWNBR TO LSX-BRNO.
           MOVE OLS-ACCTNO     TO OLD-ACCTNO.
           MOVE NEW-ACCTNO     TO LSX-ACCTNO.
           MOVE 1              TO LSX-SEQNO.
           PERFORM READ-LSX1-FILE.
           IF IO-FG NOT = 0
              MOVE "MISSING S35 RECORD" TO D01-TEXT
              MOVE BR-NO                TO D01-BRNO
              MOVE LSX-ACCTNO           TO D01-ACCTNO
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-X-FIELDS.

           ADD 1 TO S35-RECORDS-PROCESSED.

           MOVE OLS-X-REPODATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LSX-REPODATE
              MOVE "LSX-REPODATE" TO D01-TEXT
              MOVE BR-NO          TO D01-BRNO
              MOVE LSX-ACCTNO     TO D01-ACCTNO
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE LSX-REPODATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-TITLE-MILEAGE NOT NUMERIC
              MOVE 0 TO OLS-X-TITLE-MILEAGE.
           IF OLS-X-TITLE-MILEAGE NOT = LSX-TITLE-MILEAGE
              MOVE "LSX-TITLE-MILEAGE" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-TITLE-MILEAGE TO D01-A15-NUM
              MOVE LSX-TITLE-MILEAGE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-VOLUNTARY-FG NOT = LSX-VOLUNTARY-FG
              MOVE "LSX-VOLUNTARY-FG" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-VOLUNTARY-FG TO D01-A15
              MOVE LSX-VOLUNTARY-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OLS-X-PERFECT NOT = LSX-PERFECT
              MOVE "LSX-PERFECT" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-PERFECT TO D01-A15
              MOVE LSX-PERFECT   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLS-X-AUTHORIZED-DATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LSX-AUTHORIZED-DATE
              MOVE "LSX-AUTHORIZED-DATE" TO D01-TEXT
              MOVE BR-NO          TO D01-BRNO
              MOVE LSX-ACCTNO     TO D01-ACCTNO
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE LSX-AUTHORIZED-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-AUTHORIZED-BY NOT = LSX-AUTHORIZED-BY
              MOVE "LSX-AUTHORIZED-BY" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-AUTHORIZED-BY TO D01-A15
              MOVE LSX-AUTHORIZED-BY   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-REASON NOT = LSX-REASON
              MOVE "LSX-REASON" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-REASON TO D01-A15
              MOVE LSX-REASON   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-ASSIGNED-TO NOT = LSX-X-ASSIGNED-TO
              MOVE "LSX-ASSIGNED-TO" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-ASSIGNED-TO TO D01-A15
              MOVE LSX-X-ASSIGNED-TO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLS-X-ASSIGNED-DATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LSX-ASSIGNED-DATE
              MOVE "LSX-ASSIGNED-DATE" TO D01-TEXT
              MOVE BR-NO          TO D01-BRNO
              MOVE LSX-ACCTNO     TO D01-ACCTNO
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE LSX-ASSIGNED-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-REPO-CKNO NOT NUMERIC
              MOVE 0 TO OLS-X-REPO-CKNO.
           IF OLS-X-REPO-CKNO NOT = LSX-REPO-CKNO
              MOVE "LSX-REPO-CKNO" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-REPO-CKNO TO D01-A15-NUM
              MOVE LSX-REPO-CKNO   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLS-X-REPO-CKDATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LSX-REPO-CKDATE
              MOVE "LSX-REPO-CKDATE" TO D01-TEXT
              MOVE BR-NO          TO D01-BRNO
              MOVE LSX-ACCTNO     TO D01-ACCTNO
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE LSX-REPO-CKDATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-REPO-CKAMT NOT NUMERIC
              MOVE 0 TO OLS-X-REPO-CKAMT.
           IF OLS-X-REPO-CKAMT NOT = LSX-REPO-CKAMT
              MOVE "LSX-REPO-CKAMT" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-REPO-CKAMT TO D01-A15-NUM
              MOVE LSX-REPO-CKAMT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-STORED-ADR1 NOT = LSX-STORED-ADR1
              MOVE "LSX-STORED-ADR1" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-STORED-ADR1 TO D01-A15
              MOVE LSX-STORED-ADR1   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           MOVE OLS-X-STORED-CKDATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LSX-STORED-CKDATE
              MOVE "LSX-STORED-CKDATE" TO D01-TEXT
              MOVE BR-NO          TO D01-BRNO
              MOVE LSX-ACCTNO     TO D01-ACCTNO
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE LSX-STORED-CKDATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OLS-X-STORED-CKNO NOT NUMERIC
              MOVE 0 TO OLS-X-STORED-CKNO.
           IF OLS-X-STORED-CKNO NOT = LSX-STORED-CKNO
              MOVE "LSX-STORED-CKNO" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-STORED-CKNO TO D01-A15-NUM
              MOVE LSX-STORED-CKNO   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OLS-X-STORED-CKAMT NOT NUMERIC
              MOVE 0 TO OLS-X-STORED-CKAMT.
           IF OLS-X-STORED-CKAMT NOT = LSX-STORED-CKAMT
              MOVE "LSX-STORED-CKAMT" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-STORED-CKAMT TO D01-A15-NUM
              MOVE LSX-STORED-CKAMT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           MOVE OLS-X-REDEEM-DATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LSX-REDEEM-DATE
              MOVE "LSX-REDEEM-DATE" TO D01-TEXT
              MOVE BR-NO          TO D01-BRNO
              MOVE LSX-ACCTNO     TO D01-ACCTNO
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE LSX-REDEEM-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OLS-X-BUREAU-NOTIFIED NOT = LSX-BUREAU-NOTIFIED
              MOVE "LSX-BUREAU-NOTIFIED" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-BUREAU-NOTIFIED TO D01-A15
              MOVE LSX-BUREAU-NOTIFIED   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           MOVE OLS-X-BUREAU-DATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LSX-BUREAU-DATE
              MOVE "LSX-BUREAU-DATE" TO D01-TEXT
              MOVE BR-NO          TO D01-BRNO
              MOVE LSX-ACCTNO     TO D01-ACCTNO
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE LSX-BUREAU-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OLS-X-BUREAU-BY NOT = LSX-BUREAU-BY
              MOVE "LSX-BUREAU-BY" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-BUREAU-BY TO D01-A15
              MOVE LSX-BUREAU-BY   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           MOVE OLS-X-10DAY-DATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LSX-10DAY-DATE
              MOVE "LSX-10DAY-DATE" TO D01-TEXT
              MOVE BR-NO          TO D01-BRNO
              MOVE LSX-ACCTNO     TO D01-ACCTNO
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE LSX-10DAY-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OLS-X-10DAY-BY NOT = LSX-10DAY-BY
              MOVE "LSX-10DAY-BY" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-10DAY-BY TO D01-A15
              MOVE LSX-10DAY-BY   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OLS-X-10DAY-COST NOT NUMERIC
              MOVE 0 TO OLS-X-10DAY-COST.
           IF OLS-X-10DAY-COST NOT = LSX-10DAY-COST
              MOVE "LSX-10DAY-COST" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-10DAY-COST TO D01-A15-NUM
              MOVE LSX-10DAY-COST   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLS-X-TITLE-APPL-DATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LSX-TITLE-APPL-DATE
              MOVE "LSX-TITLE-APPL-DATE" TO D01-TEXT
              MOVE BR-NO          TO D01-BRNO
              MOVE LSX-ACCTNO     TO D01-ACCTNO
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE LSX-TITLE-APPL-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OLS-X-TITLE-APPL-BY NOT = LSX-TITLE-APPL-BY
              MOVE "LSX-TITLE-APPL-BY" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-TITLE-APPL-BY TO D01-A15
              MOVE LSX-TITLE-APPL-BY   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OLS-X-TITLE-APPL-COST NOT NUMERIC
              MOVE 0 TO OLS-X-TITLE-APPL-COST.
           IF OLS-X-TITLE-APPL-COST NOT = LSX-TITLE-APPL-COST
              MOVE "LSX-TITLE-APPL-COST" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-TITLE-APPL-COST TO D01-A15-NUM
              MOVE LSX-TITLE-APPL-COST   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OLS-X-COND-MILEAGE NOT NUMERIC
              MOVE 0 TO OLS-X-COND-MILEAGE.
           IF OLS-X-COND-MILEAGE NOT = LSX-COND-MILEAGE
              MOVE "LSX-COND-MILEAGE" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-COND-MILEAGE TO D01-A15-NUM
              MOVE LSX-COND-MILEAGE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OLS-X-COND-REPORT-Y-N NOT = LSX-COND-REPORT-Y-N
              MOVE "LSX-COND-REPORT-Y-N" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-COND-REPORT-Y-N TO D01-A15
              MOVE LSX-COND-REPORT-Y-N   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OLS-X-REPAIRS-DESC(1) NOT = LSX-REPAIRS-DESC(1)
              MOVE "LSX-REPAIRS-DESC(1)" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-REPAIRS-DESC(1) TO D01-A15
              MOVE LSX-REPAIRS-DESC(1)   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OLS-X-REPAIRS-DESC(2) NOT = LSX-REPAIRS-DESC(2)
              MOVE "LSX-REPAIRS-DESC(2)" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-REPAIRS-DESC(2) TO D01-A15
              MOVE LSX-REPAIRS-DESC(2)   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OLS-X-REPAIRS-COST NOT NUMERIC
              MOVE 0 TO OLS-X-REPAIRS-COST.
           IF OLS-X-REPAIRS-COST NOT = LSX-REPAIRS-COST
              MOVE "LSX-REPAIRS-COST" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-REPAIRS-COST TO D01-A15-NUM
              MOVE LSX-REPAIRS-COST   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
             IF OLS-X-ASSIGN-TO(SUB) NOT = LSX-ASSIGN-TO(SUB)
                MOVE SPACES TO D01-TEXT
                STRING "LSX-ASSIGN-TO",SUB INTO D01-TEXT
                MOVE BR-NO               TO D01-BRNO
                MOVE LSX-ACCTNO          TO D01-ACCTNO
                MOVE OLS-X-ASSIGN-TO(SUB) TO D01-A15
                MOVE LSX-ASSIGN-TO(SUB)   TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR
             END-IF
             MOVE OLS-X-ASSIGN-COMPLETE(SUB) TO DATE-MMDDYY
             PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
             PERFORM TEST-INVALID-DATE
             IF DATE-YYYYMMDD NOT = LSX-ASSIGN-COMPLETE-DATE(SUB)
                MOVE SPACES TO D01-TEXT
                STRING "LSX-ASSIGN-COMPLETE-DATE",SUB INTO D01-TEXT
                MOVE BR-NO          TO D01-BRNO
                MOVE LSX-ACCTNO     TO D01-ACCTNO
                MOVE DATE-YYYYMMDD  TO D01-A15
                MOVE LSX-ASSIGN-COMPLETE-DATE(SUB)   TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR
             END-IF
             MOVE OLS-X-ASSIGN-CKDATE(SUB) TO DATE-MMDDYY
             PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
             PERFORM TEST-INVALID-DATE
             IF DATE-YYYYMMDD NOT = LSX-ASSIGN-CKDATE(SUB)
                MOVE SPACES TO D01-TEXT
                STRING "LSX-ASSIGN-CKDATE",SUB INTO D01-TEXT
                MOVE BR-NO          TO D01-BRNO
                MOVE LSX-ACCTNO     TO D01-ACCTNO
                MOVE DATE-YYYYMMDD  TO D01-A15
                MOVE LSX-ASSIGN-CKDATE(SUB)   TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR
             END-IF
             IF OLS-X-ASSIGN-CKNO(SUB) NOT NUMERIC
                MOVE 0 TO OLS-X-ASSIGN-CKNO(SUB)
             END-IF
             IF OLS-X-ASSIGN-CKNO(SUB) NOT = LSX-ASSIGN-CKNO(SUB)
                MOVE SPACES TO D01-TEXT
                STRING "LSX-ASSIGN-CKNO",SUB INTO D01-TEXT
                MOVE BR-NO               TO D01-BRNO
                MOVE LSX-ACCTNO          TO D01-ACCTNO
                MOVE OLS-X-ASSIGN-CKNO(SUB) TO D01-A15-NUM
                MOVE LSX-ASSIGN-CKNO(SUB)   TO D01-S35-NUM
                PERFORM WRITE-DETAIL-LINE-ERROR
             END-IF
             IF OLS-X-ASSIGN-CKAMT(SUB) NOT NUMERIC
                MOVE 0 TO OLS-X-ASSIGN-CKAMT(SUB)
             END-IF
             IF OLS-X-ASSIGN-CKAMT(SUB) NOT = LSX-ASSIGN-CKAMT(SUB)
                MOVE SPACES TO D01-TEXT
                STRING "LSX-ASSIGN-CKAMT",SUB INTO D01-TEXT
                MOVE BR-NO               TO D01-BRNO
                MOVE LSX-ACCTNO          TO D01-ACCTNO
                MOVE OLS-X-ASSIGN-CKAMT(SUB) TO D01-A15-NUM
                MOVE LSX-ASSIGN-CKAMT(SUB)   TO D01-S35-NUM
                PERFORM WRITE-DETAIL-LINE-ERROR
             END-IF
           END-PERFORM.


           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
             IF OLS-X-OTH-EXP(SUB) NOT = LSX-OTH-EXP(SUB)
                MOVE SPACES              TO D01-TEXT
                STRING "LSX-OTH-EXP",SUB INTO D01-TEXT
                MOVE BR-NO               TO D01-BRNO
                MOVE LSX-ACCTNO          TO D01-ACCTNO
                MOVE OLS-SEQNO           TO D01-OSEQ
                MOVE OLS-X-OTH-EXP(SUB)  TO D01-A15
                MOVE LSX-OTH-EXP(SUB)    TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR
             END-IF

             MOVE OLS-X-OTH-EXP-CKDATE(SUB) TO DATE-MMDDYY
             PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
             PERFORM TEST-INVALID-DATE
             IF DATE-YYYYMMDD NOT = LSX-OTH-EXP-CKDATE(SUB)
                MOVE SPACES TO D01-TEXT
                STRING "LSX-OTH-EXP-CKDATE",SUB INTO D01-TEXT
                MOVE BR-NO          TO D01-BRNO
                MOVE LSX-ACCTNO     TO D01-ACCTNO
                MOVE OLS-SEQNO      TO D01-OSEQ
                MOVE DATE-YYYYMMDD  TO D01-A15
                MOVE LSX-OTH-EXP-CKDATE(SUB)   TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR
             END-IF

             IF OLS-X-OTH-EXP-CKNO(SUB) NOT NUMERIC
                MOVE 0 TO OLS-X-OTH-EXP-CKNO(SUB)
             END-IF
             IF OLS-X-OTH-EXP-CKNO(SUB) NOT = LSX-OTH-EXP-CKNO(SUB)
                MOVE SPACES TO D01-TEXT
                STRING "LSX-OTH-EXP-CKNO",SUB INTO D01-TEXT
                MOVE BR-NO               TO D01-BRNO
                MOVE LSX-ACCTNO          TO D01-ACCTNO
                MOVE OLS-SEQNO           TO D01-OSEQ
                MOVE OLS-X-OTH-EXP-CKNO(SUB) TO D01-A15-NUM
                MOVE LSX-OTH-EXP-CKNO(SUB)   TO D01-S35-NUM
                PERFORM WRITE-DETAIL-LINE-ERROR
             END-IF

             IF OLS-X-OTH-EXP-CKAMT(SUB) NOT NUMERIC
                MOVE 0 TO OLS-X-OTH-EXP-CKAMT(SUB)
             END-IF
             IF OLS-X-OTH-EXP-CKAMT(SUB) NOT = LSX-OTH-EXP-CKAMT(SUB)
                MOVE SPACES TO D01-TEXT
                STRING "LSX-OTH-EXP-CKAMT",SUB INTO D01-TEXT
                MOVE BR-NO               TO D01-BRNO
                MOVE LSX-ACCTNO          TO D01-ACCTNO
                MOVE OLS-SEQNO           TO D01-OSEQ
                MOVE OLS-X-OTH-EXP-CKAMT(SUB) TO D01-A15-NUM
                MOVE LSX-OTH-EXP-CKAMT(SUB)   TO D01-S35-NUM
                PERFORM WRITE-DETAIL-LINE-ERROR
             END-IF
           END-PERFORM.

           MOVE OLS-X-INS-RB-REQ-DATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LSX-INS-RB-REQ-DATE
              MOVE "LSX-INS-RB-REQ-DATE" TO D01-TEXT
              MOVE BR-NO          TO D01-BRNO
              MOVE LSX-ACCTNO     TO D01-ACCTNO
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE LSX-INS-RB-REQ-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLS-X-INS-RB-REC-DATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LSX-INS-RB-REC-DATE
              MOVE "LSX-INS-RB-REC-DATE" TO D01-TEXT
              MOVE BR-NO          TO D01-BRNO
              MOVE LSX-ACCTNO     TO D01-ACCTNO
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE LSX-INS-RB-REC-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-INS-RB-AMT NOT NUMERIC
              MOVE 0 TO OLS-X-INS-RB-AMT.
           IF OLS-X-INS-RB-AMT NOT = LSX-INS-RB-AMT
              MOVE "LSX-INS-RB-AMT" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-INS-RB-AMT TO D01-A15-NUM
              MOVE LSX-INS-RB-AMT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLS-X-RB-REQ-DATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LSX-RB-REQ-DATE
              MOVE "LSX-RB-REQ-DATE" TO D01-TEXT
              MOVE BR-NO          TO D01-BRNO
              MOVE LSX-ACCTNO     TO D01-ACCTNO
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE LSX-RB-REQ-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLS-X-RB-REC-DATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LSX-RB-REC-DATE
              MOVE "LSX-RB-REC-DATE" TO D01-TEXT
              MOVE BR-NO          TO D01-BRNO
              MOVE LSX-ACCTNO     TO D01-ACCTNO
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE LSX-RB-REC-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-RB-AMT NOT NUMERIC
              MOVE 0 TO OLS-X-RB-AMT.
           IF OLS-X-RB-AMT NOT = LSX-RB-AMT
              MOVE "LSX-RB-AMT" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-RB-AMT TO D01-A15-NUM
              MOVE LSX-RB-AMT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.


           MOVE OLS-X-SIC-INS-FILE-DATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LSX-SIC-INS-FILE-DATE
              MOVE "LSX-SIC-INS-FILE-DATE" TO D01-TEXT
              MOVE BR-NO          TO D01-BRNO
              MOVE LSX-ACCTNO     TO D01-ACCTNO
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE LSX-SIC-INS-FILE-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLS-X-SIC-INS-PAID-DATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LSX-SIC-INS-PAID-DATE
              MOVE "LSX-SIC-INS-PAID-DATE" TO D01-TEXT
              MOVE BR-NO          TO D01-BRNO
              MOVE LSX-ACCTNO     TO D01-ACCTNO
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE LSX-SIC-INS-PAID-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-SIC-INS-PAID-AMT NOT NUMERIC
              MOVE 0 TO OLS-X-SIC-INS-PAID-AMT.
           IF OLS-X-SIC-INS-PAID-AMT NOT = LSX-SIC-INS-PAID-AMT
              MOVE "LSX-SIC-INS-PAID-AMT" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-SIC-INS-PAID-AMT TO D01-A15-NUM
              MOVE LSX-SIC-INS-PAID-AMT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-BOOK-VALUE NOT NUMERIC
              MOVE 0 TO OLS-X-BOOK-VALUE.
           IF OLS-X-BOOK-VALUE NOT = LSX-BOOK-VALUE
              MOVE "LSX-BOOK-VALUE" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-BOOK-VALUE TO D01-A15-NUM
              MOVE LSX-BOOK-VALUE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
              IF OLS-X-SOLD-TO(SUB) NOT = LSX-SOLD-TO(SUB)
                 MOVE "LSX-SOLD-TO(SUB)" TO D01-TEXT
                 MOVE BR-NO               TO D01-BRNO
                 MOVE LSX-ACCTNO          TO D01-ACCTNO
                 MOVE OLS-X-SOLD-TO(SUB) TO D01-A15
                 MOVE LSX-SOLD-TO(SUB)   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.

           MOVE OLS-X-SOLD-TO-DATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LSX-SOLD-TO-DATE
              MOVE "LSX-SOLD-TO-DATE" TO D01-TEXT
              MOVE BR-NO          TO D01-BRNO
              MOVE LSX-ACCTNO     TO D01-ACCTNO
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE LSX-SOLD-TO-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-SOLD-TO-AMT NOT NUMERIC
              MOVE 0 TO OLS-X-SOLD-TO-AMT.
           IF OLS-X-SOLD-TO-AMT NOT = LSX-SOLD-TO-AMT
              MOVE "LSX-SOLD-TO-AMT" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-SOLD-TO-AMT TO D01-A15-NUM
              MOVE LSX-SOLD-TO-AMT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-DLNO NOT NUMERIC
              MOVE 0 TO OLS-X-DLNO.
           IF OLS-X-DLNO NOT = LSX-DLNO
              MOVE "LSX-DLNO" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-DLNO TO D01-A15-NUM
              MOVE LSX-DLNO   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-DLR-RSV-CKNO NOT NUMERIC
              MOVE 0 TO OLS-X-DLR-RSV-CKNO.
           IF OLS-X-DLR-RSV-CKNO NOT = LSX-DLR-RSV-CKNO
              MOVE "LSX-DLR-RSV-CKNO" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-DLR-RSV-CKNO TO D01-A15-NUM
              MOVE LSX-DLR-RSV-CKNO   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-DLR-RSV-CKAMT NOT NUMERIC
              MOVE 0 TO OLS-X-DLR-RSV-CKAMT.
           IF OLS-X-DLR-RSV-CKAMT NOT = LSX-DLR-RSV-CKAMT
              MOVE "LSX-DLR-RSV-CKAMT" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-DLR-RSV-CKAMT TO D01-A15-NUM
              MOVE LSX-DLR-RSV-CKAMT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-PL-EXPENSE NOT NUMERIC
              MOVE 0 TO OLS-X-PL-EXPENSE.
           IF OLS-X-PL-EXPENSE NOT = LSX-PL-EXPENSE
              MOVE "LSX-PL-EXPENSE" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-PL-EXPENSE TO D01-A15-NUM
              MOVE LSX-PL-EXPENSE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-DEFICIENCY-Y-N NOT = LSX-DEFICIENCY-Y-N
              MOVE "LSX-DEFICIENCY-Y-N" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-DEFICIENCY-Y-N TO D01-A15
              MOVE LSX-DEFICIENCY-Y-N   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-NET-BAL-DUE NOT NUMERIC
              MOVE 0 TO OLS-X-NET-BAL-DUE.
           IF OLS-X-NET-BAL-DUE NOT = LSX-NET-BAL-DUE
              MOVE "LSX-NET-BAL-DUE" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-NET-BAL-DUE TO D01-A15-NUM
              MOVE LSX-NET-BAL-DUE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-DEFICIENCY-BAL NOT NUMERIC
              MOVE 0 TO OLS-X-DEFICIENCY-BAL.
           IF OLS-X-DEFICIENCY-BAL NOT = LSX-DEFICIENCY-BAL
              MOVE "LSX-DEFICIENCY-BAL" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-DEFICIENCY-BAL TO D01-A15-NUM
              MOVE LSX-DEFICIENCY-BAL   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-AUCTION-EXPENSE NOT NUMERIC
              MOVE 0 TO OLS-X-AUCTION-EXPENSE.
           IF OLS-X-AUCTION-EXPENSE NOT = LSX-AUCTION-EXPENSE
              MOVE "LSX-AUCTION-EXPENSE" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-AUCTION-EXPENSE TO D01-A15-NUM
              MOVE LSX-AUCTION-EXPENSE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-REASON-CODE NOT = LSX-REASON-CODE
              MOVE "LSX-REASON-CODE" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-REASON-CODE TO D01-A15 
              MOVE LSX-REASON-CODE   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-TRIM NOT = LSX-TRIM
              MOVE "LSX-TRIM" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-TRIM TO D01-A15
              MOVE LSX-TRIM   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-DOORS NOT = LSX-DOORS
              MOVE "LSX-DOORS" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-DOORS TO D01-A15
              MOVE LSX-DOORS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-COLOR NOT = LSX-COLOR
              MOVE "LSX-COLOR" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-COLOR TO D01-A15
              MOVE LSX-COLOR   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-CYLINDERS NOT NUMERIC OR OLS-X-CYLINDERS = 20 OR
              OLS-X-CYLINDERS = 02
              MOVE 0 TO OLS-X-CYLINDERS.
           IF OLS-X-CYLINDERS NOT = LSX-CYLINDERS
              MOVE "LSX-CYLINDERS" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-CYLINDERS TO D01-A15
              MOVE LSX-CYLINDERS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-AUTO-TRANS NOT = LSX-AUTO-TRANS
              MOVE "LSX-AUTO-TRANS" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-AUTO-TRANS TO D01-A15
              MOVE LSX-AUTO-TRANS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-AC NOT = LSX-AC
              MOVE "LSX-AC" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-AC TO D01-A15
              MOVE LSX-AC   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-SROOF NOT = LSX-SROOF
              MOVE "LSX-SROOF" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-SROOF TO D01-A15
              MOVE LSX-SROOF   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-LEATHER NOT = LSX-LEATHER
              MOVE "LSX-LEATHER" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-LEATHER TO D01-A15
              MOVE LSX-LEATHER   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-4WD NOT = LSX-4WD
              MOVE "LSX-4WD" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-4WD TO D01-A15
              MOVE LSX-4WD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-DIESEL NOT = LSX-DIESEL
              MOVE "LSX-DIESEL" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-DIESEL TO D01-A15
              MOVE LSX-DIESEL   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-DUAL-AIR NOT = LSX-DUAL-AIR
              MOVE "LSX-DUAL-AIR" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-DUAL-AIR TO D01-A15
              MOVE LSX-DUAL-AIR   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-EXT-CAB NOT = LSX-EXT-CAB
              MOVE "LSX-EXT-CAB" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-EXT-CAB TO D01-A15
              MOVE LSX-EXT-CAB   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-AGENT-PHONE NOT NUMERIC OR
              OLS-X-AGENT-PHONE = 2020202020
              MOVE 0 TO OLS-X-AGENT-PHONE.
           IF OLS-X-AGENT-PHONE NOT = LSX-AGENT-PHONE
              MOVE "LSX-AGENT-PHONE" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-AGENT-PHONE TO D01-A15
              MOVE LSX-AGENT-PHONE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-AGENT-FAX NOT NUMERIC OR
              OLS-X-AGENT-FAX = 2020202020
              MOVE 0 TO OLS-X-AGENT-FAX.
           IF OLS-X-AGENT-FAX NOT = LSX-AGENT-FAX
              MOVE "LSX-AGENT-FAX" TO D01-TEXT
              MOVE BR-NO           TO D01-BRNO
              MOVE LSX-ACCTNO      TO D01-ACCTNO
              MOVE OLS-X-AGENT-FAX TO D01-A15
              MOVE LSX-AGENT-FAX   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-STORED-ADR2 NOT = LSX-STORED-ADR2
              MOVE "LSX-STORED-ADR2" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-STORED-ADR2 TO D01-A15
              MOVE LSX-STORED-ADR2   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-STORED-CITY NOT = LSX-X-STORED-CITY
              MOVE "LSX-STORED-CITY" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-STORED-CITY TO D01-A15
              MOVE LSX-X-STORED-CITY   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-STORED-STATE NOT = LSX-X-STORED-STATE
              MOVE "LSX-STORED-STATE" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-STORED-STATE TO D01-A15
              MOVE LSX-X-STORED-STATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-STORED-ZIP NOT = LSX-X-STORED-ZIP
              MOVE "LSX-STORED-ZIP" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-STORED-ZIP TO D01-A15
              MOVE LSX-X-STORED-ZIP   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-INS-CLAIM-PENDING NOT = LSX-INS-CLAIM-PENDING
              MOVE "LSX-INS-CLAIM-PENDING" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-INS-CLAIM-PENDING TO D01-A15
              MOVE LSX-INS-CLAIM-PENDING   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-WARRANTY-AVAILABLE NOT = LSX-WARRANTY-AVAILABLE
              MOVE "LSX-WARRANTY-AVAILABLE" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-WARRANTY-AVAILABLE TO D01-A15
              MOVE LSX-WARRANTY-AVAILABLE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-INS-REBATE-REQUESTED NOT = LSX-INS-REBATE-REQUESTED
              MOVE "LSX-INS-REBATE-REQUESTED" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-INS-REBATE-REQUESTED TO D01-A15
              MOVE LSX-INS-REBATE-REQUESTED   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-EXTRACTED NOT = LSX-EXTRACTED
              MOVE "LSX-EXTRACTED" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-EXTRACTED TO D01-A15
              MOVE LSX-EXTRACTED   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-TITLE-SENT-DATE NOT NUMERIC
              MOVE 0 TO OLS-X-TITLE-SENT-DATE.
           MOVE OLS-X-TITLE-SENT-DATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LSX-TITLE-SENT-DATE
              MOVE "LSX-TITLE-SENT-DATE" TO D01-TEXT
              MOVE BR-NO          TO D01-BRNO
              MOVE LSX-ACCTNO     TO D01-ACCTNO
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE LSX-TITLE-SENT-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-X-KEYS NOT = LSX-KEYS
              MOVE "LSX-KEYS" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-X-KEYS TO D01-A15
              MOVE LSX-KEYS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

       EXIT-COMPARE-X-FIELDS.
           EXIT.
       

      ******************************************************************
       COMPARE-Y-FIELDS SECTION.
      ******************************************************************

      D    STOP MESS.
           MOVE LSX-PATH-OWNBR TO LSX-BRNO.
           MOVE OLS-ACCTNO     TO OLD-ACCTNO.
           MOVE NEW-ACCTNO     TO LSX-ACCTNO.
           MOVE 1              TO LSX-SEQNO.
           PERFORM READ-LSX1-FILE.
           IF IO-FG NOT = 0
              MOVE "MISSING S35 RECORD" TO D01-TEXT
              MOVE BR-NO        TO D01-BRNO
              MOVE LSX-ACCTNO   TO D01-ACCTNO
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-Y-FIELDS.

           IF OLS-Y-DISPOSAL-HOLD NOT = LSX-DISPOSAL-HOLD
              MOVE "LSX-DISPOSAL-HOLD" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-Y-DISPOSAL-HOLD TO D01-A15
              MOVE LSX-DISPOSAL-HOLD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLS-Y-NEXT-HLD-RVW-DATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LSX-NEXT-HLD-RVW-DATE
              MOVE "LSX-NEXT-HLD-RVW-DATE" TO D01-TEXT
              MOVE BR-NO          TO D01-BRNO
              MOVE LSX-ACCTNO     TO D01-ACCTNO
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE LSX-NEXT-HLD-RVW-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-Y-CASE-ID NOT NUMERIC
              MOVE 0 TO OLS-Y-CASE-ID.
           IF OLS-Y-CASE-ID NOT = LSX-CASE-ID
              MOVE "LSX-CASE-ID" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-Y-CASE-ID TO D01-A15-NUM
              MOVE LSX-CASE-ID   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-Y-COLLATERAL-LOCATION NOT = LSX-COLLATERAL-LOCATION
              MOVE "LSX-COLLATERAL-LOCATION" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-Y-COLLATERAL-LOCATION TO D01-A15
              MOVE LSX-COLLATERAL-LOCATION   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-Y-VIN-MATCH NOT = LSX-VIN-MATCH
              MOVE "LSX-VIN-MATCH" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-Y-VIN-MATCH TO D01-A15
              MOVE LSX-VIN-MATCH   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-Y-PERSONALS NOT = LSX-PERSONALS
              MOVE "LSX-PERSONALS" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-Y-PERSONALS TO D01-A15
              MOVE LSX-PERSONALS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-Y-KEYS NOT = LSX-KEYS
              MOVE "LSX-KEYS" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-Y-KEYS TO D01-A15
              MOVE LSX-KEYS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-Y-DRIVES NOT = LSX-DRIVES
              MOVE "LSX-DRIVES" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-Y-DRIVES TO D01-A15
              MOVE LSX-DRIVES   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-Y-DMG NOT = LSX-DMG
              MOVE "LSX-DMG" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-Y-DMG TO D01-A15
              MOVE LSX-DMG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-Y-ASSIGNED-TO NOT = LSX-Y-ASSIGNED-TO
              MOVE "LSX-ASSIGNED-TO" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-Y-ASSIGNED-TO TO D01-A15
              MOVE LSX-Y-ASSIGNED-TO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-Y-SUPPLIER-ID NOT NUMERIC
              MOVE 0 TO OLS-Y-SUPPLIER-ID.
           IF OLS-Y-SUPPLIER-ID NOT = LSX-SUPPLIER-ID
              MOVE "LSX-SUPPLIER-ID" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-Y-SUPPLIER-ID TO D01-A15-NUM
              MOVE LSX-SUPPLIER-ID   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-Y-TRANSPORT-EMAIL NOT = LSX-TRANSPORT-EMAIL
              MOVE "LSX-TRANSPORT-EMAIL" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-Y-TRANSPORT-EMAIL TO D01-A15
              MOVE LSX-TRANSPORT-EMAIL   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-Y-STORED-LOCATION NOT = LSX-STORED-LOCATION
              MOVE "LSX-STORED-LOCATION" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-Y-STORED-LOCATION TO D01-A15
              MOVE LSX-STORED-LOCATION   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-Y-STORED-ADDRESS NOT = LSX-STORED-ADDRESS
              MOVE "LSX-STORED-ADDRESS" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-Y-STORED-ADDRESS TO D01-A15
              MOVE LSX-STORED-ADDRESS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-Y-STORED-CITY NOT = LSX-Y-STORED-CITY
              MOVE "LSX-STORED-CITY" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-Y-STORED-CITY TO D01-A15
              MOVE LSX-Y-STORED-CITY   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-Y-STORED-STATE NOT = LSX-Y-STORED-STATE
              MOVE "LSX-STORED-STATE" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-Y-STORED-STATE TO D01-A15
              MOVE LSX-Y-STORED-STATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-Y-STORED-ZIP NOT = LSX-Y-STORED-ZIP
              MOVE "LSX-STORED-ZIP" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-Y-STORED-ZIP TO D01-A15
              MOVE LSX-Y-STORED-ZIP   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-Y-AGENCY-NAME NOT = LSX-AGENCY-NAME
              MOVE "LSX-AGENCY-NAME" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-Y-AGENCY-NAME TO D01-A15
              MOVE LSX-AGENCY-NAME   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-Y-AGENCY-ADDRESS NOT = LSX-AGENCY-ADDRESS
              MOVE "LSX-AGENCY-ADDRESS" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-Y-AGENCY-ADDRESS TO D01-A15
              MOVE LSX-AGENCY-ADDRESS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-Y-AGENCY-CITY NOT = LSX-AGENCY-CITY
              MOVE "LSX-AGENCY-CITY" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-Y-AGENCY-CITY TO D01-A15
              MOVE LSX-AGENCY-CITY   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-Y-AGENCY-STATE NOT = LSX-AGENCY-STATE
              MOVE "LSX-AGENCY-STATE" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-Y-AGENCY-STATE TO D01-A15
              MOVE LSX-AGENCY-STATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-Y-AGENCY-ZIP NOT = LSX-AGENCY-ZIP
              MOVE "LSX-AGENCY-ZIP" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-Y-AGENCY-ZIP TO D01-A15
              MOVE LSX-AGENCY-ZIP   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-Y-AGENCY-PHONE NOT NUMERIC
              MOVE 0 TO OLS-Y-AGENCY-PHONE.
           IF OLS-Y-AGENCY-PHONE NOT = LSX-AGENCY-PHONE
              MOVE "LSX-AGENCY-PHONE" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSX-ACCTNO          TO D01-ACCTNO
              MOVE OLS-Y-AGENCY-PHONE TO D01-A15
              MOVE LSX-AGENCY-PHONE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

       EXIT-COMPARE-Y-FIELDS.
           EXIT.

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
           MOVE A15-RECORDS-PROCESSED                 TO R01-NUMERIC.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "S35 RECORDS PROCESSED         :"     TO R01-PROMPT.
           MOVE S35-RECORDS-PROCESSED                 TO R01-NUMERIC.
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
           MOVE EXT-CONAME-CONAME TO LSXFLE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LSXFLE-SYSDATE.
           DISPLAY LSXFLE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE LSXFLE-FORM-FIELDS TO WR-BUF.
           MOVE LSXFLE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE LSXFLE-HELP--FILE TO HELP-LINK-FILE.
           MOVE LSXFLE-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/LSXFLE_EVA.CPY".
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
       COPY "LIBLP/LPLSXGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPLSX1RN.CPY".
       OPEN-OLS1-FILE.
           PERFORM OPEN-IT.
           MOVE OLS-PATH TO E-FILE.
           OPEN INPUT OLS-FILE.
           IF IO-FG = 8
              GO TO OPEN-OLS1-FILE.
           IF IO-FG = 7
              CLOSE OLS-FILE
              GO TO OPEN-OLS1-FILE.
      *READ-OLS1-FILE.
      *    PERFORM READ-IT.
      *    MOVE OLS-PATH TO E-FILE.
      *    MOVE OLS1-KEY TO E-KEYX.
      *    READ OLS-FILE.
      *    IF IO-FG = 8
      *       GO TO READ-OLS1-FILE.
       READ-OLS1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OLS-PATH TO E-FILE.
           MOVE OLS1-KEY TO E-KEYX.
           READ OLS-FILE NEXT.
           IF IO-FG = 8
             GO TO READ-OLS1-FILE-NEXT.
       START-OLS1-FILE.
           PERFORM START-IT.
           MOVE OLS-PATH TO E-FILE.
           MOVE OLS1-KEY TO E-KEYX.
           START OLS-FILE KEY NOT < OLS1-KEY.
       CLOSE-OLS1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OLS-FILE.

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
              DESTROY LSXFLE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
