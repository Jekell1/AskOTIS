       IDENTIFICATION DIVISION.
       PROGRAM-ID.    LSDFLE.
       DATE-WRITTEN.  11-09-2023.
      ******************************************************************
      *
      * DESCRIPTION   : COMPARE S35 LSDFILE WITH A15 LSFILE "D"
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
      * 03-09-2025 MBZ - ADDED S35 AND ERRORS COUNTERS
      * 04-15-2025 MBZ - USE 0 FOR A15 FIELS LSD-AUCTN-TIME, LSD-AUCTN-PHONE
      *                   IF VALUE = 2020 OR 2020202020
      ***********************************************************************
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
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)UT/LSDFLE.CBL S35 11/14/23-16:13:50 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LSD.CPY".
       COPY "LIBLP/LP01LSD_SQL.CPY".
       COPY "LIBLP/LPWSLSD.CPY".
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

       01  TOTAL-ERRORS-CTR            PIC 9(9) VALUE 0.
       01  SQL-RECORDS-PROCESSED       PIC 9(9) VALUE 0.
       01  A15-RECORDS-PROCESSED       PIC 9(9) VALUE 0.
           88  NO-RECORDS-FOUND                     VALUE ZEROES.

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
           03  H02-TITLE               PIC X(60) VALUE 
           "DEFICIENCY TRAILER FILE (LSDFILE) COMPARE A15 TO S35".

       01  HEADER-LINE-03.
           03  FILLER                PIC X(4) VALUE "BRNO".
           03  FILLER                PIC X(5) VALUE SPACES.
           03  FILLER                PIC X(6) VALUE "ACCTNO".
           03  FILLER                PIC X(5) VALUE SPACES.
           03  FILLER                PIC X(11) VALUE "DESCRIPTION".
           03  FILLER                PIC X(17) VALUE SPACES.
           03  FILLER                PIC X(3) VALUE "A15".
           03  FILLER                PIC X(22) VALUE SPACES.
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
           03  D01-ACCTNO                     PIC Z(7)9.
           03  FILLER                         PIC X(3).
           03  D01-TEXT                       PIC X(25).
           03  FILLER                         PIC X(3).
           03  D01-A15                        PIC X(22).
           03  FILLER REDEFINES D01-A15.
               05  D01-A15-NUM                PIC Z(9)9.9(3).
               05  FILLER                     PIC X(8).
           03  FILLER                         PIC X(5).
           03  D01-S35                        PIC X(22).
           03  FILLER REDEFINES D01-S35.
               05  D01-S35-NUM                PIC Z(9)9.9(3).
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
       COPY "LIBUT/LSDFLE_DEF.CPY".
       COPY "LIBUT/LSDFLE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/LSDFLE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE OLS-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LSD1-FILE.
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
           MOVE "LSDFLE" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE LSDFLE-QUEUE-POS TO Q-POS.
           MOVE LSDFLE-COPIES-POS TO C-POS.
           MOVE LSDFLE-PRU-POS   TO P-POS.
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

           MOVE ZEROES TO A15-RECORDS-PROCESSED.

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
              IF OLS-ALL-MACHINE = MACHINE-R1
                 MOVE MACHINE-R2 TO OLS-ALL-MACHINE
                 MOVE OLS-ALL-PATH TO OLS-PATH ACCESS-BUF
                 PERFORM ACCESS-CALL
                 IF ( STAT-BAD )
                    MOVE "MISSING A15 FILE" TO D01-TEXT 
                    MOVE OLS-ALL-PATH       TO D01-A15
                    MOVE SPACES             TO D01-S35
                    PERFORM WRITE-DETAIL-LINE
                    GO TO PROCESS-BRANCH-DRIVER-END.


           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO LSD-PATH-OVERRIDE.

           PERFORM OPEN-LSD1-FILE.
           PERFORM OPEN-OLS1-FILE.

           MOVE BEG-ACCTNO TO NEW-ACCTNO.
           MOVE 01         TO NEW-SEQNO.
           MOVE OLD-ACCTNO TO OLS-ACCTNO.
           MOVE "D"        TO OLS-CODE.
           MOVE 0          TO OLS-SEQNO.

           PERFORM START-OLS1-FILE.

       PROCESS-BRANCH-NEXT-LSD.
           PERFORM READ-OLS1-FILE-NEXT.
           IF IO-BAD 
              GO TO PROCESS-BRANCH-DRIVER-END.

           PERFORM SEND-PROGRESS-INDICATOR.

           IF OLS-CODE NOT = "D" 
              GO TO PROCESS-BRANCH-NEXT-LSD.

      D    STOP MESS.
           MOVE OLS-ACCTNO TO OLD-ACCTNO.
           IF ( NEW-ACCTNO < BEG-ACCTNO ) OR
              ( NEW-ACCTNO > END-ACCTNO )
              GO TO PROCESS-BRANCH-NEXT-LSD.

           ADD  01 TO A15-RECORDS-PROCESSED.

           PERFORM COMPARE-FIELDS.

           GO TO PROCESS-BRANCH-NEXT-LSD.

       PROCESS-BRANCH-DRIVER-END.

           PERFORM CLOSE-LSD1-FILE.

       PROCESS-BRANCH-DRIVER-EXIT.
           EXIT.

      ******************************************************************
       COMPARE-FIELDS SECTION.
      ******************************************************************

           MOVE LSD-PATH-OWNBR TO LSD-BRNO.
           MOVE OLS-ACCTNO     TO OLD-ACCTNO.
           MOVE NEW-ACCTNO     TO LSD-ACCTNO.
           MOVE 1              TO LSD-SEQNO.
           PERFORM READ-LSD1-FILE.
           IF IO-FG NOT = 0
              MOVE "MISSING S35 RECORD" TO D01-TEXT
              MOVE BR-NO        TO D01-BRNO
              MOVE LSD-ACCTNO   TO D01-ACCTNO
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS
           ELSE
              ADD 1 TO SQL-RECORDS-PROCESSED.

           MOVE OLS-D-STOLEN-DATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LSD-STOLEN-DATE
              MOVE "LSD-STOLEN-DATE" TO D01-TEXT
              MOVE BR-NO          TO D01-BRNO
              MOVE LSD-ACCTNO     TO D01-ACCTNO
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE LSD-STOLEN-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF (OLS-D-GAP-INS-PAID NOT NUMERIC)
              MOVE 0  TO OLS-D-GAP-INS-PAID.
           IF OLS-D-GAP-INS-PAID NOT = LSD-GAP-INS-PAID
              MOVE "LSD-GAP-INS-PAID" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LSD-ACCTNO TO D01-ACCTNO
              MOVE OLS-D-GAP-INS-PAID TO D01-A15-NUM
              MOVE LSD-GAP-INS-PAID   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF (OLS-D-OTHER-EXPENSE NOT NUMERIC)
              MOVE 0  TO OLS-D-OTHER-EXPENSE.
           IF OLS-D-OTHER-EXPENSE NOT = LSD-OTHER-EXPENSE
              MOVE "LSD-OTHER-EXPENSE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LSD-ACCTNO TO D01-ACCTNO
              MOVE OLS-D-OTHER-EXPENSE TO D01-A15-NUM
              MOVE LSD-OTHER-EXPENSE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF (OLS-D-MISC-AMT-EXPENSE NOT NUMERIC)
              MOVE 0  TO OLS-D-MISC-AMT-EXPENSE.
           IF OLS-D-MISC-AMT-EXPENSE NOT = LSD-GAP-INS-PAID
              MOVE "LSD-MISC-AMT-EXPENSE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LSD-ACCTNO TO D01-ACCTNO
              MOVE OLS-D-MISC-AMT-EXPENSE TO D01-A15-NUM
              MOVE LSD-MISC-AMT-EXPENSE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OLS-D-MISC-DESC-EXPENSE NOT = LSD-MISC-DESC-EXPENSE
              MOVE "LSD-MISC-DESC-EXPENSE" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSD-ACCTNO          TO D01-ACCTNO
              MOVE OLS-D-MISC-DESC-EXPENSE TO D01-A15
              MOVE LSD-MISC-DESC-EXPENSE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF (OLS-D-TOT-EXPENSE NOT NUMERIC)
              MOVE 0  TO OLS-D-TOT-EXPENSE.
           IF OLS-D-TOT-EXPENSE NOT = LSD-TOT-EXPENSE
              MOVE "LSD-TOT-EXPENSE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LSD-ACCTNO TO D01-ACCTNO
              MOVE OLS-D-TOT-EXPENSE TO D01-A15-NUM
              MOVE LSD-TOT-EXPENSE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF (OLS-D-INT-REB NOT NUMERIC)
              MOVE 0  TO OLS-D-INT-REB.
           IF OLS-D-INT-REB NOT = LSD-INT-REB
              MOVE "LSD-INT-REB" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LSD-ACCTNO TO D01-ACCTNO
              MOVE OLS-D-INT-REB TO D01-A15-NUM
              MOVE LSD-INT-REB   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF (OLS-D-CPP-REB NOT NUMERIC)
              MOVE 0  TO OLS-D-CPP-REB.
           IF OLS-D-CPP-REB NOT = LSD-CPP-REB
              MOVE "LSD-CPP-REB" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LSD-ACCTNO TO D01-ACCTNO
              MOVE OLS-D-CPP-REB TO D01-A15-NUM
              MOVE LSD-CPP-REB   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF (OLS-D-GAP-REB NOT NUMERIC)
              MOVE 0  TO OLS-D-GAP-REB.
           IF OLS-D-GAP-REB NOT = LSD-GAP-REB
              MOVE "LSD-GAP-REB" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LSD-ACCTNO TO D01-ACCTNO
              MOVE OLS-D-GAP-REB TO D01-A15-NUM
              MOVE LSD-GAP-REB   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF (OLS-D-WARRANTY-REB NOT NUMERIC)
              MOVE 0  TO OLS-D-WARRANTY-REB.
           IF OLS-D-WARRANTY-REB NOT = LSD-WARRANTY-REB
              MOVE "LSD-WARRANTY-REB" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LSD-ACCTNO TO D01-ACCTNO
              MOVE OLS-D-WARRANTY-REB TO D01-A15-NUM
              MOVE LSD-WARRANTY-REB   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF (OLS-D-MISC-AMT-CREDIT NOT NUMERIC)
              MOVE 0  TO OLS-D-MISC-AMT-CREDIT.
           IF OLS-D-MISC-AMT-CREDIT NOT = LSD-MISC-AMT-CREDIT
              MOVE "LSD-MISC-AMT-CREDIT" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LSD-ACCTNO TO D01-ACCTNO
              MOVE OLS-D-MISC-AMT-CREDIT TO D01-A15-NUM
              MOVE LSD-MISC-AMT-CREDIT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OLS-D-MISC-DESC-CREDIT NOT = LSD-MISC-DESC-CREDIT
              MOVE "LSD-MISC-DESC-CREDIT" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSD-ACCTNO          TO D01-ACCTNO
              MOVE OLS-D-MISC-DESC-CREDIT TO D01-A15
              MOVE LSD-MISC-DESC-CREDIT   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF (OLS-D-TOT-CREDITS NOT NUMERIC)
              MOVE 0  TO OLS-D-TOT-CREDITS.
           IF OLS-D-TOT-CREDITS NOT = LSD-TOT-CREDITS
              MOVE "LSD-TOT-CREDITS" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LSD-ACCTNO TO D01-ACCTNO
              MOVE OLS-D-TOT-CREDITS TO D01-A15-NUM
              MOVE LSD-TOT-CREDITS   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF (OLS-D-PAID-TO-OTHER NOT NUMERIC)
              MOVE 0  TO OLS-D-PAID-TO-OTHER.
           IF OLS-D-PAID-TO-OTHER NOT = LSD-PAID-TO-OTHER
              MOVE "LSD-PAID-TO-OTHER" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LSD-ACCTNO TO D01-ACCTNO
              MOVE OLS-D-PAID-TO-OTHER TO D01-A15-NUM
              MOVE LSD-PAID-TO-OTHER   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF (OLS-D-AMT-FROM-OWING NOT NUMERIC)
              MOVE 0  TO OLS-D-AMT-FROM-OWING.
           IF OLS-D-AMT-FROM-OWING NOT = LSD-AMT-FROM-OWING
              MOVE "LSD-AMT-FROM-OWING" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LSD-ACCTNO TO D01-ACCTNO
              MOVE OLS-D-AMT-FROM-OWING TO D01-A15-NUM
              MOVE LSD-AMT-FROM-OWING   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF (OLS-D-AMT-OF-SURPLUS NOT NUMERIC)
              MOVE 0  TO OLS-D-AMT-OF-SURPLUS.
           IF OLS-D-AMT-OF-SURPLUS NOT = LSD-AMT-OF-SURPLUS
              MOVE "LSD-AMT-OF-SURPLUS" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LSD-ACCTNO TO D01-ACCTNO
              MOVE OLS-D-AMT-OF-SURPLUS TO D01-A15-NUM
              MOVE LSD-AMT-OF-SURPLUS   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF (OLS-D-AMT-OF-DEFICIENCY NOT NUMERIC)
              MOVE 0  TO OLS-D-AMT-OF-DEFICIENCY.
           IF OLS-D-AMT-OF-DEFICIENCY NOT = LSD-AMT-OF-DEFICIENCY
              MOVE "LSD-AMT-OF-DEFICIENCY" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LSD-ACCTNO TO D01-ACCTNO
              MOVE OLS-D-AMT-OF-DEFICIENCY TO D01-A15-NUM
              MOVE LSD-AMT-OF-DEFICIENCY   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF (OLS-D-SEND-FMNO NOT NUMERIC)
              MOVE 0  TO OLS-D-SEND-FMNO.
           IF OLS-D-SEND-FMNO NOT = LSD-SEND-FMNO
              MOVE "LSD-SEND-FMNO" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LSD-ACCTNO TO D01-ACCTNO
              MOVE OLS-D-SEND-FMNO TO D01-A15-NUM
              MOVE LSD-SEND-FMNO   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF (OLS-D-INSURANCE-PROCEEDS NOT NUMERIC)
              MOVE 0  TO OLS-D-INSURANCE-PROCEEDS.
           IF OLS-D-INSURANCE-PROCEEDS NOT = LSD-INSURANCE-PROCEEDS
              MOVE "LSD-INSURANCE-PROCEEDS" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LSD-ACCTNO TO D01-ACCTNO
              MOVE OLS-D-INSURANCE-PROCEEDS TO D01-A15-NUM
              MOVE LSD-INSURANCE-PROCEEDS   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           MOVE OLS-D-DATE-CHECK-RECV TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LSD-DATE-CHECK-RECV
              MOVE "LSD-DATE-CHECK-RECV" TO D01-TEXT
              MOVE BR-NO          TO D01-BRNO
              MOVE LSD-ACCTNO     TO D01-ACCTNO
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE LSD-DATE-CHECK-RECV   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF (OLS-D-INSURANCE-CHECK NOT NUMERIC)
              MOVE 0  TO OLS-D-INSURANCE-CHECK.
           IF OLS-D-INSURANCE-CHECK NOT = LSD-INSURANCE-CHECK
              MOVE "LSD-INSURANCE-CHECK" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LSD-ACCTNO TO D01-ACCTNO
              MOVE OLS-D-INSURANCE-CHECK TO D01-A15-NUM
              MOVE LSD-INSURANCE-CHECK   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OLS-D-WHO-ADJUSTED NOT = LSD-WHO-ADJUSTED
              MOVE "LSD-WHO-ADJUSTED" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSD-ACCTNO          TO D01-ACCTNO
              MOVE OLS-D-WHO-ADJUSTED TO D01-A15
              MOVE LSD-WHO-ADJUSTED   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OLS-D-INS-COMPANY NOT = LSD-INS-COMPANY
              MOVE "LSD-INS-COMPANY" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSD-ACCTNO          TO D01-ACCTNO
              MOVE OLS-D-INS-COMPANY TO D01-A15
              MOVE LSD-INS-COMPANY   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           MOVE OLS-D-DEF-LETTER-SENT TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LSD-DEF-LETTER-SENT-DATE
              MOVE "LSD-DEF-LETTER-SENT-DATE" TO D01-TEXT
              MOVE BR-NO          TO D01-BRNO
              MOVE LSD-ACCTNO     TO D01-ACCTNO
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE LSD-DEF-LETTER-SENT-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           MOVE OLS-D-SALES-DATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LSD-SALES-DATE
              MOVE "LSD-SALES-DATE" TO D01-TEXT
              MOVE BR-NO          TO D01-BRNO
              MOVE LSD-ACCTNO     TO D01-ACCTNO
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE LSD-SALES-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OLS-D-AUCTION NOT = LSD-AUCTION
              MOVE "LSD-AUCTION" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSD-ACCTNO          TO D01-ACCTNO
              MOVE OLS-D-AUCTION TO D01-A15
              MOVE LSD-AUCTION   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OLS-D-AUCTN-TIME NOT NUMERIC OR OLS-D-AUCTN-TIME = 2020
              MOVE 0  TO OLS-D-AUCTN-TIME.
           IF OLS-D-AUCTN-TIME NOT = LSD-AUCTN-TIME
              MOVE "LSD-AUCTN-TIME" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LSD-ACCTNO TO D01-ACCTNO
              MOVE OLS-D-AUCTN-TIME TO D01-A15-NUM
              MOVE LSD-AUCTN-TIME   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OLS-D-AUCTN-ADR NOT = LSD-AUCTN-ADR
              MOVE "LSD-AUCTN-ADR" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSD-ACCTNO          TO D01-ACCTNO
              MOVE OLS-D-AUCTN-ADR TO D01-A15
              MOVE LSD-AUCTN-ADR   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OLS-D-AUCTN-CITY NOT = LSD-AUCTN-CITY
              MOVE "LSD-AUCTN-CITY" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSD-ACCTNO          TO D01-ACCTNO
              MOVE OLS-D-AUCTN-CITY TO D01-A15
              MOVE LSD-AUCTN-CITY   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OLS-D-AUCTN-STATE NOT = LSD-AUCTN-STATE
              MOVE "LSD-AUCTN-STATE" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSD-ACCTNO          TO D01-ACCTNO
              MOVE OLS-D-AUCTN-STATE TO D01-A15
              MOVE LSD-AUCTN-STATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OLS-D-AUCTN-ZIP NOT = LSD-AUCTN-ZIP
              MOVE "LSD-AUCTN-ZIP" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSD-ACCTNO          TO D01-ACCTNO
              MOVE OLS-D-AUCTN-ZIP TO D01-A15
              MOVE LSD-AUCTN-ZIP   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OLS-D-AUCTN-PHONE NOT NUMERIC OR
              OLS-D-AUCTN-PHONE = 2020202020
              MOVE 0  TO OLS-D-AUCTN-PHONE.
           IF OLS-D-AUCTN-PHONE NOT = LSD-AUCTN-PHONE
              MOVE "LSD-AUCTN-PHONE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LSD-ACCTNO TO D01-ACCTNO
              MOVE OLS-D-AUCTN-PHONE TO D01-A15-NUM
              MOVE LSD-AUCTN-PHONE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF (OLS-D-REPO-CHARGE NOT NUMERIC)
              MOVE 0  TO OLS-D-REPO-CHARGE.
           IF OLS-D-REPO-CHARGE NOT = LSD-REPO-CHARGE
              MOVE "LSD-REPO-CHARGE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LSD-ACCTNO TO D01-ACCTNO
              MOVE OLS-D-REPO-CHARGE TO D01-A15-NUM
              MOVE LSD-REPO-CHARGE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF (OLS-D-STORAGE-CHARGE NOT NUMERIC)
              MOVE 0  TO OLS-D-STORAGE-CHARGE.
           IF OLS-D-STORAGE-CHARGE NOT = LSD-STORAGE-CHARGE
              MOVE "LSD-STORAGE-CHARGE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LSD-ACCTNO TO D01-ACCTNO
              MOVE OLS-D-STORAGE-CHARGE TO D01-A15-NUM
              MOVE LSD-STORAGE-CHARGE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF (OLS-D-REPAIR-CHARGE NOT NUMERIC)
              MOVE 0  TO OLS-D-REPAIR-CHARGE.
           IF OLS-D-REPAIR-CHARGE NOT = LSD-REPAIR-CHARGE
              MOVE "LSD-REPAIR-CHARGE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LSD-ACCTNO TO D01-ACCTNO
              MOVE OLS-D-REPAIR-CHARGE TO D01-A15-NUM
              MOVE LSD-REPAIR-CHARGE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF (OLS-D-OTHER-CHARGE NOT NUMERIC)
              MOVE 0  TO OLS-D-OTHER-CHARGE.
           IF OLS-D-OTHER-CHARGE NOT = LSD-OTHER-CHARGE
              MOVE "LSD-OTHER-CHARGE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LSD-ACCTNO TO D01-ACCTNO
              MOVE OLS-D-OTHER-CHARGE TO D01-A15-NUM
              MOVE LSD-OTHER-CHARGE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
    
           IF (OLS-D-STORAGE-RATE NOT NUMERIC)
              MOVE 0  TO OLS-D-STORAGE-RATE.
           IF OLS-D-STORAGE-RATE NOT = LSD-STORAGE-RATE
              MOVE "LSD-STORAGE-RATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LSD-ACCTNO TO D01-ACCTNO
              MOVE OLS-D-STORAGE-RATE TO D01-A15-NUM
              MOVE LSD-STORAGE-RATE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF (OLS-D-EST-REBATE NOT NUMERIC)
              MOVE 0  TO OLS-D-EST-REBATE.
           IF OLS-D-EST-REBATE NOT = LSD-EST-REBATE
              MOVE "LSD-EST-REBATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LSD-ACCTNO TO D01-ACCTNO
              MOVE OLS-D-EST-REBATE TO D01-A15-NUM
              MOVE LSD-EST-REBATE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF (OLS-D-MINIMUM-BID NOT NUMERIC)
              MOVE 0  TO OLS-D-MINIMUM-BID.
           IF OLS-D-MINIMUM-BID NOT = LSD-MINIMUM-BID
              MOVE "LSD-MINIMUM-BID" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LSD-ACCTNO TO D01-ACCTNO
              MOVE OLS-D-MINIMUM-BID TO D01-A15-NUM
              MOVE LSD-MINIMUM-BID   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OLS-D-MAY-REINSTATE NOT = LSD-MAY-REINSTATE
              MOVE "LSD-MAY-REINSTATE" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSD-ACCTNO          TO D01-ACCTNO
              MOVE OLS-D-MAY-REINSTATE TO D01-A15
              MOVE LSD-MAY-REINSTATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OLS-D-MAY-NOT-REINSTATE NOT = LSD-MAY-NOT-REINSTATE
              MOVE "LSD-MAY-NOT-REINSTATE" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSD-ACCTNO          TO D01-ACCTNO
              MOVE OLS-D-MAY-NOT-REINSTATE TO D01-A15
              MOVE LSD-MAY-NOT-REINSTATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OLS-D-IN-PAST-12MONTHS NOT = LSD-IN-PAST-12MONTHS
              MOVE "LSD-IN-PAST-12MONTHS" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSD-ACCTNO          TO D01-ACCTNO
              MOVE OLS-D-IN-PAST-12MONTHS TO D01-A15
              MOVE LSD-IN-PAST-12MONTHS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OLS-D-2X-DURING-CONTRACT NOT = LSD-2X-DURING-CONTRACT
              MOVE "LSD-2X-DURING-CONTRACT" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSD-ACCTNO          TO D01-ACCTNO
              MOVE OLS-D-2X-DURING-CONTRACT TO D01-A15
              MOVE LSD-2X-DURING-CONTRACT   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OLS-D-OTHER NOT = LSD-OTHER
              MOVE "LSD-OTHER" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSD-ACCTNO          TO D01-ACCTNO
              MOVE OLS-D-OTHER TO D01-A15
              MOVE LSD-OTHER   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLS-D-OTHER-COMMENTS1 NOT = LSD-OTHER-COMMENTS1
              MOVE "LSD-OTHER-COMMENTS1" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSD-ACCTNO          TO D01-ACCTNO
              MOVE OLS-D-OTHER-COMMENTS1 TO D01-A15
              MOVE LSD-OTHER-COMMENTS1   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OLS-D-OTHER-COMMENTS2 NOT = LSD-OTHER-COMMENTS2
              MOVE "LSD-OTHER-COMMENTS2" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSD-ACCTNO          TO D01-ACCTNO
              MOVE OLS-D-OTHER-COMMENTS2 TO D01-A15
              MOVE LSD-OTHER-COMMENTS2   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OLS-D-CA-PYMTS-AS-AGREED NOT = LSD-CA-PYMTS-AS-AGREED
              MOVE "LSD-CA-PYMTS-AS-AGREED" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSD-ACCTNO          TO D01-ACCTNO
              MOVE OLS-D-CA-PYMTS-AS-AGREED TO D01-A15
              MOVE LSD-CA-PYMTS-AS-AGREED   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OLS-D-CA-KEEP-INS NOT = LSD-CA-KEEP-INS
              MOVE "LSD-CA-KEEP-INS" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSD-ACCTNO          TO D01-ACCTNO
              MOVE OLS-D-CA-KEEP-INS TO D01-A15
              MOVE LSD-CA-KEEP-INS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OLS-D-CA-KEEP-LIEN-FREE NOT = LSD-CA-KEEP-LIEN-FREE
              MOVE "LSD-CA-KEEP-LIEN-FREE" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSD-ACCTNO          TO D01-ACCTNO
              MOVE OLS-D-CA-KEEP-LIEN-FREE TO D01-A15
              MOVE LSD-CA-KEEP-LIEN-FREE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OLS-D-CA-OTHER NOT = LSD-CA-OTHER
              MOVE "LSD-CA-OTHER" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSD-ACCTNO          TO D01-ACCTNO
              MOVE OLS-D-CA-OTHER TO D01-A15
              MOVE LSD-CA-OTHER   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OLS-D-CA-OTHER-DESC NOT = LSD-CA-OTHER-DESC
              MOVE "LSD-CA-OTHER-DESC" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSD-ACCTNO          TO D01-ACCTNO
              MOVE OLS-D-CA-OTHER-DESC TO D01-A15
              MOVE LSD-CA-OTHER-DESC   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OLS-D-CURE NOT = LSD-CURE
              MOVE "LSD-CURE" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE LSD-ACCTNO          TO D01-ACCTNO
              MOVE OLS-D-CURE TO D01-A15
              MOVE LSD-CURE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

       EXIT-COMPARE-FIELDS.
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
           MOVE EXT-CONAME-CONAME TO LSDFLE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LSDFLE-SYSDATE.
           DISPLAY LSDFLE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE LSDFLE-FORM-FIELDS TO WR-BUF.
           MOVE LSDFLE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE LSDFLE-HELP--FILE TO HELP-LINK-FILE.
           MOVE LSDFLE-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/LSDFLE_EVA.CPY".
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
           MOVE PROGID-TEXT-PROCESSING     TO PROGID-TEXT.

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
       COPY "LIBLP/LPLSDGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPLSD1RN.CPY".
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
              DESTROY LSDFLE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
