       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CAFILE.
       DATE-WRITTEN.  12-05-2023.
      ******************************************************************
      *
      * DESCRIPTION   : COMPARE S35 CAFILE WITH A15 CAFILE
      *            
      *                 TO BE USED AFTER AN UPDATE
      *                 WILL COMPARE EACH FIELD AND REPORT ANY 
      *                 DIFFERENCES, AS LONG AS FIELD EXIST IN BOTH FILES
      *
      *  LOOPS THRU THE A15 FILE AND VERIFIES THE S35 FILE EXIST AND MATCHES
      *
      *
      * * * NEED TO TAKE OUT THE C$JUSTIFY STATEMENTS AFTER BRIAN REMOVES
      *     THE "TRIM" STATEMENT FROM THE ASCII LOAD
      * * *
      *
      *  01-07-25  RMZ  ADDED 'BTRACK' - PROCESS TRACKING TO PROGRAM.
      *  02-14-25  MB   CHANGE TO LOOK BACK 4 YEARS MAX.
      *  04-29-25  MB   CHANGE TO BYPASS CHECK FOR OPEN-HHMM, CLOSED-HHMM 
      *                  AND COP-USERID IF ENT-GROUP = 'S351' (USER TESTING)
      *  05-01-25  MB    MISC REPORT CHANGES FOR MISSING RECS
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
      * COPYMEMBER: LIBLP/LPFSCA
           SELECT OCA-FILE ASSIGN TO OCA-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OCA1-KEY
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
       FD  OCA-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A15SRC/LP01CA.CPY" REPLACING LEADING "CA" BY "OCA".

       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.

       01  WK-REC.
           03  WK-KEY.
               05  WK-BRNO             PIC 9(4).
               05  WK-DATE             PIC 9(8).
               05  WK-TILLNO           PIC 99.
      *             07  WK-TILLNO-ZERO  PIC 9.
      *             07  WK-TILLNO-ONE   PIC X. 
           03  WK-DESC                 PIC X(30).
           03  WK-BEGBAL               PIC S9(7)V99 COMP-3.
           03  WK-OPENED               PIC 9(4)     COMP-3.
           03  WK-CLOSED               PIC 9(4)     COMP-3.
           03  WK-ENDBAL               PIC S9(7)V99 COMP-3.
           03  WK-BALBY                PIC X(10).
           03  WK-RUNNING              PIC S9(7)V99 COMP-3.
           03  WK-DEPOSIT              PIC S9(7)V99 COMP-3.
           03  WK-OPENID               PIC X(8).
           03  WK-NET-CASH-NOTES       PIC S9(7)V99 COMP-3.
           03  WK-PASSWORD             PIC X(4).
           03  WK-CHECK-DEPOSIT        PIC S9(7)V99 COMP-3.
           03  FILLER                  PIC X(8).

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/CAFILE S35 03/22/24-05:18:03 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01CA.CPY".
       COPY "LIBLP/LP01CA_SQL.CPY".
       COPY "LIBLP/LPWSCA.CPY".
       01  OCA-PATH                    PIC X(35) VALUE SPACES.
       01  OCA-ALL-PATH.
           03  OCA-ALL-BASE     PIC X(9).
           03  FILLER           PIC X     VALUE "/".
           03  OCA-ALL-MACHINE  PIC XX.
           03  FILLER           PIC X     VALUE "/".
           03  OCA-ALL-DATA     PIC X(6).
           03  FILLER           PIC X(10) VALUE
               "/LP/CAFILE".

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

       01  CA1-CONN-OPEN-SW     PIC X     VALUE "N".

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

       01  FOUR-YEARS-BACK-DATE        PIC 9(8)  VALUE 0.
       01  OLD-ACCTNO                  PIC 9(8).
       01  FILLER REDEFINES OLD-ACCTNO.
           03  NEW-ACCTNO              PIC 9(6).
           03  FILLER                  PIC 99.

       01  MISC-CURSOR-STAT            PIC X(2) VALUE SPACES.
           88  MISC-CURSOR-STAT-GOOD            VALUE "00".
           88  MISC-CURSOR-STAT-BAD             VALUE "  " THRU "//"
                                                      "01" THRU "~~".

       01  PROCESS-STAT                PIC X(08)  VALUE SPACES.
           88  PROCESS-STAT-GOOD                  VALUE "00".
           88  PROCESS-STAT-BAD                   VALUE "  " THRU "//"
                                                        "01" THRU "~~".

       01  TOTAL-ERRORS-CTR            PIC 9(9)   VALUE 0.
       01  S35-RECORD-CTR              PIC 9(9)   VALUE 0.
       01  WK-RECORD-CTR               PIC 9(9)   VALUE 0.

       01  A15-RECORDS-PROCESSED           PIC 9(9)   VALUE 0.
           88  NO-RECORDS-FOUND                   VALUE ZEROES.

       01  STARTING-TIME               PIC X(08)  VALUE SPACES.
       01  SUB                         PIC 9(04)  VALUE ZEROES.

       01  SYSDATE-YYYYMMDD            PIC 9(8)   VALUE ZEROES.

       01  WK-EOF-SW                   PIC X      VALUE "N".
       01  CA-EOF-SW                   PIC X      VALUE "N".
       01  OCA-EOF-SW                  PIC X      VALUE "N".

       01  WS-CA1-KEY.
           05  WS-CA-BRNO              PIC 9(4).
           05  WS-CA-DATE              PIC 9(8).
           05  WS-CA-TILLNO            PIC 9(2).
 
       01  WS-OCA1-KEY.
           05  WS-OCA-BRNO             PIC 9(4).
           05  WS-OCA-DATE             PIC 9(8).
           05  WS-OCA-TILLNO           PIC 9(2).

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
           03  FILLER                  PIC X(6)  VALUE SPACES.
           03  FILLER                  PIC X(06) VALUE "PAGE: ".
           03  H01-PAGE                PIC Z(04).

      *-----------------------------------------------------------------
       01  HEADER-LINE-02.
           03  FILLER                  PIC X(10) VALUE "USER ID:".
           03  H02-USERID              PIC X(10) VALUE SPACES.
           03  FILLER                  PIC X(10) VALUE SPACES.
           03  H02-TITLE               PIC X(50) VALUE 
           "CASH DRAWER FILE (CAFILE) COMPARE A15 TO S35".

       01  HEADER-LINE-03.
           03  FILLER                PIC X(4)  VALUE "BRNO".
           03  FILLER                PIC X(3)  VALUE SPACES.
           03  FILLER                PIC X(6)  VALUE "DATE".
           03  FILLER                PIC X(4)  VALUE SPACES.
           03  FILLER                PIC X(4)  VALUE "TILL".
           03  FILLER                PIC X(1)  VALUE SPACES.
           03  FILLER                PIC X(11) VALUE "DESCRIPTION".
           03  FILLER                PIC X(17) VALUE SPACES.
           03  FILLER                PIC X(3)  VALUE "A15".
           03  FILLER                PIC X(19) VALUE SPACES.
           03  FILLER                PIC X(3)  VALUE "S35".

       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-BRNO                       PIC Z(3)9.
           03  FILLER                         PIC X(2).
           03  D01-DATE                       PIC 9(8).
           03  FILLER                         PIC X(2).
           03  D01-TILLNO                     PIC 99.
           03  FILLER                         PIC X(2).
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
       COPY "LIBUT/CAFILE_DEF.CPY".
       COPY "LIBUT/CAFILE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/CAFILE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE OCA-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-CA1-FILE.
           PERFORM CLOSE-CA1-CONNECTION.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE OCA-FILE WK-FILE.
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
           MOVE "CAFILE" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD      TO SYSDATE-YYYYMMDD.

           MOVE SYSDATE-YYYYMMDD   TO NDTE-DATE.
           MOVE -48                TO NDTE-HOLD.
           PERFORM INCREMENT-MONTHS.
           MOVE NDTE-DATE TO FOUR-YEARS-BACK-DATE.

           MOVE CAFILE-QUEUE-POS  TO Q-POS.
           MOVE CAFILE-COPIES-POS TO C-POS.
           MOVE CAFILE-PRU-POS    TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           PERFORM GET-TIME.
           MOVE TIME-EDIT                  TO STARTING-TIME.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS......" TO LEGEND
              PERFORM SEND-LEGEND.

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


       ENTER-BEG-DATE.

           MOVE "ERNM060BEG-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F3-KEY ) GO TO ENTER-ALL-DATE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-DATE.

           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.

           IF BEG-DATE < FOUR-YEARS-BACK-DATE 
              MOVE "BEGIN DATE < FOUR YEARS BACK" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DATE.

      *-----------------------------------------------------------------
       ENTER-END-DATE.

           MOVE "ERNM060END-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-DATE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-DATE.

           MOVE VDU-DATE-YYYYMMDD TO END-DATE.

           IF ( BEG-DATE > END-DATE )
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DATE.

           GO TO ENTER-PARAMETERS-END.

      *-----------------------------------------------------------------
       ENTER-ALL-DATE.

           MOVE "S  8080BEG-DATE."  TO VDU.
           MOVE FOUR-YEARS-BACK-DATE TO BEG-DATE VDU-DATE-YYYYMMDD.
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
           MOVE FOUR-YEARS-BACK-DATE TO BEG-DATE.
           MOVE 20491231             TO END-DATE.

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

      D    STOP MESS.
           MOVE SOURCE-PATH  TO OCA-ALL-BASE.
           MOVE MACHINE-R1   TO OCA-ALL-MACHINE.
           MOVE BR-DATA-PATH TO OCA-ALL-DATA.
           MOVE OCA-ALL-PATH TO OCA-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE MACHINE-R2 TO OCA-ALL-MACHINE
              MOVE OCA-ALL-PATH TO OCA-PATH ACCESS-BUF
              PERFORM ACCESS-CALL
              IF ( STAT-BAD )
                 MOVE "MISSING A15 FILE" TO D01-TEXT
                 MOVE BR-NO              TO D01-BRNO
                 MOVE ZEROES             TO D01-DATE 
                 MOVE ZEROES             TO D01-TILLNO
                 MOVE OCA-ALL-PATH       TO D01-A15
                 MOVE SPACES             TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR
                 GO TO PROCESS-BRANCH-DRIVER-END.

           MOVE 'N' TO WK-EOF-SW CA-EOF-SW OCA-EOF-SW. 

      * DELETE/EMPTY TEMP FILE IF EXIST
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           PERFORM OPEN-OCA1-FILE.
           MOVE EXT-FILPATH-BASE     TO FILPATH-BASE.
           MOVE BR-MACHINE           TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH         TO FILPATH-DATA.
           MOVE "B"                  TO CA-PATH-OVERRIDE.
           PERFORM OPEN-CA1-FILE.

           MOVE CA-PATH-OWNBR        TO OCA-BRNO.
           MOVE ZEROES               TO OCA-DATE.
           MOVE 0                    TO OCA-TILLNO-ZERO.
           MOVE SPACES               TO OCA-TILLNO.
           PERFORM START-OCA1-FILE.

           PERFORM LOAD-WK-FILE.
           PERFORM CLOSE-OCA1-FILE. 

           MOVE CA-PATH-OWNBR       TO QCA1-WBEG-BRNO
                                       QCA1-WEND-BRNO.
           MOVE BEG-DATE            TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QCA1-WBEG-DATE.
           MOVE END-DATE            TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QCA1-WEND-DATE.
           MOVE 00                  TO QCA1-WBEG-TILLNO.
           MOVE 99                  TO QCA1-WEND-TILLNO.
           PERFORM START-CA1-FILE.

           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           PERFORM PROCESS-READ-WK.
           PERFORM PROCESS-READ-CA1.  

           PERFORM PROCESS-BRANCH-NEXT-CA
              UNTIL WK-EOF-SW  = "Y" AND CA-EOF-SW = "Y".

           PERFORM CLOSE-CA1-FILE.
           PERFORM CLOSE-WK-FILE.

           GO TO PROCESS-BRANCH-DRIVER-END.

       PROCESS-BRANCH-NEXT-CA.
   
           PERFORM COMPARE-FIELDS.

           IF WS-OCA1-KEY < WS-CA1-KEY
              PERFORM PROCESS-READ-WK
           ELSE
              IF WS-OCA1-KEY > WS-CA1-KEY
                 PERFORM PROCESS-READ-CA1
              ELSE 
                 PERFORM PROCESS-READ-WK
                 PERFORM PROCESS-READ-CA1.

       PROCESS-BRANCH-DRIVER-END.
           EXIT.

      ******************************************************************
       LOAD-WK-FILE SECTION.
      ******************************************************************

       LOAD-WK-FILE-CONTINUE.

           PERFORM READ-OCA1-FILE-NEXT.
           IF IO-BAD 
              MOVE "Y"            TO OCA-EOF-SW
              GO TO EXIT-LOAD-WK-FILE. 
  
      * CLEANSE BR 701 HAD DATE 19623897 DRAWER 1 AND 3
      * CLEANSE BR 102 HAD DATE 19623897 DRAWER 1 AND 3
           MOVE OCA-DATE              TO DATE-MMDDYY.
           IF (DATE-MMDDYY-MM < 1 OR > 12) OR (DATE-MMDDYY-DD > 30)
              PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
              MOVE DATE-YYYYMMDD TO NUM-DATE
              PERFORM TSTDAT
              IF NUM-DATE = 0
                 GO TO LOAD-WK-FILE-CONTINUE.
 
           INITIALIZE WK-REC.
 
           MOVE OCA-BRNO              TO WK-BRNO.
           MOVE OCA-DATE              TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           IF DATE-YYYYMMDD  < BEG-DATE OR
               DATE-YYYYMMDD > END-DATE
              GO TO LOAD-WK-FILE-CONTINUE
           ELSE
               MOVE DATE-YYYYMMDD     TO WK-DATE.

           PERFORM LOAD-WORK-FILE-CONV-TILLNO.
      *     MOVE OCA-TILLNO-ZERO       TO WK-TILLNO-ZERO.   
      *     MOVE OCA-TILLNO            TO WK-TILLNO-ONE.        
           MOVE OCA-DESC              TO WK-DESC.          
           MOVE OCA-BEGBAL            TO WK-BEGBAL.

           IF (OCA-OPENED NOT NUMERIC) OR (OCA-OPENED = 2020 OR 0202)   
              MOVE ZEROES             TO WK-OPENED
           ELSE
              MOVE OCA-OPENED         TO WK-OPENED.        

           IF (OCA-CLOSED NOT NUMERIC) OR (OCA-CLOSED = 2020 OR 0202)
              MOVE ZEROES             TO WK-CLOSED
           ELSE
              MOVE OCA-CLOSED         TO WK-CLOSED.        

           MOVE OCA-ENDBAL            TO WK-ENDBAL.        
           MOVE OCA-BALBY             TO WK-BALBY.         
           MOVE OCA-RUNNING           TO WK-RUNNING.       
           MOVE OCA-DEPOSIT           TO WK-DEPOSIT.       
           MOVE OCA-OPENID            TO WK-OPENID.        
           MOVE OCA-NET-CASH-NOTES    TO WK-NET-CASH-NOTES.
           MOVE OCA-PASSWORD          TO WK-PASSWORD.      
           MOVE OCA-CHECK-DEPOSIT     TO WK-CHECK-DEPOSIT. 
 
           PERFORM WRITE-WK-FILE. 
  
           PERFORM SEND-PROGRESS-INDICATOR.

           ADD  1 TO A15-RECORDS-PROCESSED.
  
           GO TO LOAD-WK-FILE-CONTINUE.

       EXIT-LOAD-WK-FILE.
           EXIT.

       LOAD-WORK-FILE-CONV-TILLNO SECTION.
           IF OCA-TILLNO = "1"
              MOVE 1  TO WK-TILLNO 
           ELSE
           IF OCA-TILLNO = "2"
              MOVE 2  TO WK-TILLNO 
           ELSE
           IF OCA-TILLNO = "3"
              MOVE 3  TO WK-TILLNO 
           ELSE
           IF OCA-TILLNO = "4"
              MOVE 4  TO WK-TILLNO 
           ELSE
           IF OCA-TILLNO = "5"
              MOVE 5  TO WK-TILLNO 
           ELSE
           IF OCA-TILLNO = "6"
              MOVE 6  TO WK-TILLNO 
           ELSE
           IF OCA-TILLNO = "7"
              MOVE 7  TO WK-TILLNO 
           ELSE
           IF OCA-TILLNO = "8"
              MOVE 8  TO WK-TILLNO 
           ELSE
           IF OCA-TILLNO = "9"
              MOVE 9  TO WK-TILLNO 
           ELSE
           IF OCA-TILLNO = "A"
              MOVE 10 TO WK-TILLNO 
           ELSE
           IF OCA-TILLNO = "B"
              MOVE 11 TO WK-TILLNO 
           ELSE
           IF OCA-TILLNO = "C"
              MOVE 12 TO WK-TILLNO 
           ELSE
           IF OCA-TILLNO = "D"
              MOVE 13 TO WK-TILLNO 
           ELSE
           IF OCA-TILLNO = "E"
              MOVE 14 TO WK-TILLNO 
           ELSE
           IF OCA-TILLNO = "F"
              MOVE 15 TO WK-TILLNO 
           ELSE
           IF OCA-TILLNO = "G"
              MOVE 16 TO WK-TILLNO 
           ELSE
           IF OCA-TILLNO = "H"
              MOVE 17 TO WK-TILLNO 
           ELSE
           IF OCA-TILLNO = "I"
              MOVE 18 TO WK-TILLNO 
           ELSE
           IF OCA-TILLNO = "J"
              MOVE 19 TO WK-TILLNO 
           ELSE
           IF OCA-TILLNO = "K"
              MOVE 20 TO WK-TILLNO 
           ELSE
           IF OCA-TILLNO = "L"
              MOVE 21 TO WK-TILLNO 
           ELSE
           IF OCA-TILLNO = "M"
              MOVE 22 TO WK-TILLNO 
           ELSE
           IF OCA-TILLNO = "N"
              MOVE 23 TO WK-TILLNO 
           ELSE
           IF OCA-TILLNO = "O"
              MOVE 24 TO WK-TILLNO 
           ELSE
           IF OCA-TILLNO = "P"
              MOVE 25 TO WK-TILLNO 
           ELSE
           IF OCA-TILLNO = "Q"
              MOVE 26 TO WK-TILLNO 
           ELSE
           IF OCA-TILLNO = "R"
              MOVE 27 TO WK-TILLNO 
           ELSE
           IF OCA-TILLNO = "S"
              MOVE 28 TO WK-TILLNO 
           ELSE
           IF OCA-TILLNO = "T"
              MOVE 29 TO WK-TILLNO 
           ELSE
           IF OCA-TILLNO = "U"
              MOVE 30 TO WK-TILLNO 
           ELSE
           IF OCA-TILLNO = "V"
              MOVE 31 TO WK-TILLNO 
           ELSE
           IF OCA-TILLNO = "W"
              MOVE 32 TO WK-TILLNO 
           ELSE
           IF OCA-TILLNO = "X"
              MOVE 33 TO WK-TILLNO 
           ELSE
           IF OCA-TILLNO = "Y"
              MOVE 34 TO WK-TILLNO 
           ELSE
           IF OCA-TILLNO = "Z"
              MOVE 35 TO WK-TILLNO.

       LOAD-WORK-FILE-CONV-TILLNO-EXIT.
           EXIT.

      ******************************************************************
       PROCESS-READ-WK SECTION.
      ******************************************************************

           PERFORM READ-WK-FILE-NEXT.
           IF IO-BAD 
              MOVE 9999           TO WS-OCA-BRNO
              MOVE 99999999       TO WS-OCA-DATE
              MOVE "99"           TO WS-OCA-TILLNO
              MOVE "Y"            TO WK-EOF-SW
           ELSE 
              MOVE WK-BRNO        TO WS-OCA-BRNO
              MOVE WK-DATE        TO WS-OCA-DATE 
              MOVE WK-TILLNO      TO WS-OCA-TILLNO
              ADD 1               TO WK-RECORD-CTR.

           PERFORM SEND-PROGRESS-INDICATOR.

       EXIT-PROCESS-READ-WK.
           EXIT.

      ******************************************************************
       PROCESS-READ-CA1 SECTION.
      ******************************************************************

           PERFORM READ-CA1-FILE-NEXT.
           IF IO-BAD 
              MOVE 9999           TO WS-CA-BRNO
              MOVE 99999999       TO WS-CA-DATE
              MOVE 99             TO WS-CA-TILLNO
              MOVE "Y"            TO CA-EOF-SW
           ELSE 
              MOVE CA-PATH-OWNBR  TO WS-CA-BRNO
              MOVE CA-DATE        TO WS-CA-DATE
              MOVE CA-TILLNO      TO WS-CA-TILLNO
              ADD 1               TO S35-RECORD-CTR.

       EXIT-PROCESS-READ-CA1.
           EXIT.

      ******************************************************************
       COMPARE-FIELDS SECTION.
      ******************************************************************

           IF WS-OCA1-KEY < WS-CA1-KEY 
              MOVE "MISSING S35 RECORD" TO D01-TEXT
              MOVE WK-BRNO              TO D01-BRNO  
              MOVE WS-OCA-DATE          TO D01-DATE
              MOVE WK-TILLNO            TO D01-TILLNO 
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.
 
           IF WS-CA1-KEY < WS-OCA1-KEY
              MOVE "MISSING A15 RECORD" TO D01-TEXT
              MOVE CA-BRNO              TO D01-BRNO  
              MOVE CA-DATE              TO D01-DATE 
              MOVE CA-TILLNO            TO D01-TILLNO 
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.

      *    MOVE OCA-BRNO TO CA-BRNO.
      *    MOVE OCA-DATE TO DATE-MMDDYY.
      *    PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
      *    MOVE DATE-YYYYMMDD TO CA-DATE.
      *    IF OCA-TILLNO = "1"
      *       MOVE 1  TO CA-TILLNO 
      *    ELSE
      *    IF OCA-TILLNO = "2"
      *       MOVE 2  TO CA-TILLNO 
      *    ELSE
      *    IF OCA-TILLNO = "3"
      *       MOVE 3  TO CA-TILLNO 
      *    ELSE
      *    IF OCA-TILLNO = "4"
      *       MOVE 4  TO CA-TILLNO 
      *    ELSE
      *    IF OCA-TILLNO = "5"
      *       MOVE 5  TO CA-TILLNO 
      *    ELSE
      *    IF OCA-TILLNO = "6"
      *       MOVE 6  TO CA-TILLNO 
      *    ELSE
      *    IF OCA-TILLNO = "7"
      *       MOVE 7  TO CA-TILLNO 
      *    ELSE
      *    IF OCA-TILLNO = "8"
      *       MOVE 8  TO CA-TILLNO 
      *    ELSE
      *    IF OCA-TILLNO = "9"
      *       MOVE 9  TO CA-TILLNO 
      *    ELSE
      *    IF OCA-TILLNO = "A"
      *       MOVE 10 TO CA-TILLNO 
      *    ELSE
      *    IF OCA-TILLNO = "B"
      *       MOVE 11 TO CA-TILLNO 
      *    ELSE
      *    IF OCA-TILLNO = "C"
      *       MOVE 12 TO CA-TILLNO 
      *    ELSE
      *    IF OCA-TILLNO = "D"
      *       MOVE 13 TO CA-TILLNO 
      *    ELSE
      *    IF OCA-TILLNO = "E"
      *       MOVE 14 TO CA-TILLNO 
      *    ELSE
      *    IF OCA-TILLNO = "F"
      *       MOVE 15 TO CA-TILLNO 
      *    ELSE
      *    IF OCA-TILLNO = "G"
      *       MOVE 16 TO CA-TILLNO 
      *    ELSE
      *    IF OCA-TILLNO = "H"
      *       MOVE 17 TO CA-TILLNO 
      *    ELSE
      *    IF OCA-TILLNO = "I"
      *       MOVE 18 TO CA-TILLNO 
      *    ELSE
      *    IF OCA-TILLNO = "J"
      *       MOVE 19 TO CA-TILLNO 
      *    ELSE
      *    IF OCA-TILLNO = "K"
      *       MOVE 20 TO CA-TILLNO 
      *    ELSE
      *    IF OCA-TILLNO = "L"
      *       MOVE 21 TO CA-TILLNO 
      *    ELSE
      *    IF OCA-TILLNO = "M"
      *       MOVE 22 TO CA-TILLNO 
      *    ELSE
      *    IF OCA-TILLNO = "N"
      *       MOVE 23 TO CA-TILLNO 
      *    ELSE
      *    IF OCA-TILLNO = "O"
      *       MOVE 24 TO CA-TILLNO 
      *    ELSE
      *    IF OCA-TILLNO = "P"
      *       MOVE 25 TO CA-TILLNO 
      *    ELSE
      *    IF OCA-TILLNO = "Q"
      *       MOVE 26 TO CA-TILLNO 
      *    ELSE
      *    IF OCA-TILLNO = "R"
      *       MOVE 27 TO CA-TILLNO 
      *    ELSE
      *    IF OCA-TILLNO = "S"
      *       MOVE 28 TO CA-TILLNO 
      *    ELSE
      *    IF OCA-TILLNO = "T"
      *       MOVE 29 TO CA-TILLNO 
      *    ELSE
      *    IF OCA-TILLNO = "U"
      *       MOVE 30 TO CA-TILLNO 
      *    ELSE
      *    IF OCA-TILLNO = "V"
      *       MOVE 31 TO CA-TILLNO 
      *    ELSE
      *    IF OCA-TILLNO = "W"
      *       MOVE 32 TO CA-TILLNO 
      *    ELSE
      *    IF OCA-TILLNO = "X"
      *       MOVE 33 TO CA-TILLNO 
      *    ELSE
      *    IF OCA-TILLNO = "Y"
      *       MOVE 34 TO CA-TILLNO 
      *    ELSE
      *    IF OCA-TILLNO = "Z"
      *       MOVE 35 TO CA-TILLNO.
      *
      *     PERFORM READ-CA1-FILE.
      *     IF IO-FG NOT = 0
      *        MOVE "MISSING S35 RECORD" TO D01-TEXT
      *        MOVE BR-NO      TO D01-BRNO
      *        MOVE OCA-DATE   TO D01-DATE
      *        MOVE CA-TILLNO  TO D01-TILLNO
      *        PERFORM WRITE-DETAIL-LINE
      *        GO TO EXIT-COMPARE-FIELDS.

           IF CA-DESC NOT = WK-DESC
      *       CALL "C$JUSTIFY" USING WK-DESC, "L"
              IF CA-DESC NOT = WK-DESC
                 MOVE "CA-DESC"  TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE WK-DATE    TO D01-DATE
                 MOVE CA-TILLNO  TO D01-TILLNO
                 MOVE WK-DESC    TO D01-A15
                 MOVE CA-DESC    TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF WK-BEGBAL NOT NUMERIC
              MOVE 0 TO WK-BEGBAL.
           IF WK-BEGBAL NOT = CA-BEGBAL
              MOVE "CA-BEGBAL"   TO D01-TEXT
              MOVE BR-NO         TO D01-BRNO
              MOVE WK-DATE       TO D01-DATE
              MOVE CA-TILLNO     TO D01-TILLNO
              MOVE WK-BEGBAL     TO D01-A15-NUM
              MOVE CA-BEGBAL     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF ENT-GROUP NOT = 'S351'
              IF WK-OPENED NOT NUMERIC
                 MOVE 0 TO WK-OPENED
              END-IF
              IF WK-OPENED NOT = CA-OPENED-HHMM
                 MOVE "CA-OPENED-HHMM" TO D01-TEXT
                 MOVE BR-NO            TO D01-BRNO
                 MOVE WK-DATE          TO D01-DATE
                 MOVE CA-TILLNO        TO D01-TILLNO
                 MOVE WK-OPENED        TO D01-A15 
                 MOVE CA-OPENED-HHMM   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF ENT-GROUP NOT = 'S351'
              IF WK-CLOSED NOT NUMERIC
                 MOVE 0 TO WK-CLOSED
              END-IF
              IF WK-CLOSED NOT = CA-CLOSED-HHMM
                 MOVE "CA-CLOSED-HHMM" TO D01-TEXT
                 MOVE BR-NO            TO D01-BRNO
                 MOVE WK-DATE          TO D01-DATE
                 MOVE CA-TILLNO        TO D01-TILLNO
                 MOVE WK-CLOSED        TO D01-A15 
                 MOVE CA-CLOSED-HHMM   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF WK-ENDBAL NOT NUMERIC
              MOVE 0 TO WK-ENDBAL.
           IF WK-ENDBAL NOT = CA-ENDBAL
              MOVE "CA-ENDBAL"  TO D01-TEXT
              MOVE BR-NO        TO D01-BRNO
              MOVE WK-DATE      TO D01-DATE
              MOVE CA-TILLNO    TO D01-TILLNO
              MOVE WK-ENDBAL    TO D01-A15-NUM
              MOVE CA-ENDBAL    TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CA-BALBY NOT = WK-BALBY
      *       CALL "C$JUSTIFY" USING WK-BALBY, "L"
              IF CA-BALBY NOT = WK-BALBY
                 MOVE "CA-BALBY" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE WK-DATE    TO D01-DATE
                 MOVE CA-TILLNO  TO D01-TILLNO
                 MOVE WK-BALBY   TO D01-A15
                 MOVE CA-BALBY   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF WK-RUNNING NOT NUMERIC
              MOVE 0 TO WK-RUNNING.
           IF WK-RUNNING NOT = CA-RUNNING
              MOVE "CA-RUNNING" TO D01-TEXT
              MOVE BR-NO        TO D01-BRNO
              MOVE WK-DATE      TO D01-DATE
              MOVE CA-TILLNO    TO D01-TILLNO
              MOVE WK-RUNNING   TO D01-A15-NUM
              MOVE CA-RUNNING   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF WK-DEPOSIT NOT NUMERIC
              MOVE 0 TO WK-DEPOSIT.
           IF WK-DEPOSIT NOT = CA-DEPOSIT
              MOVE "CA-DEPOSIT" TO D01-TEXT
              MOVE BR-NO        TO D01-BRNO
              MOVE WK-DATE      TO D01-DATE
              MOVE CA-TILLNO    TO D01-TILLNO
              MOVE WK-DEPOSIT   TO D01-A15-NUM
              MOVE CA-DEPOSIT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF ENT-GROUP NOT = 'S351'
              IF CA-OPEN-USERID NOT = WK-OPENID
      *       CALL "C$JUSTIFY" USING WK-OPENID, "L"
                 IF CA-OPEN-USERID NOT = WK-OPENID
                    MOVE "CA-OPEN-USERID" TO D01-TEXT
                    MOVE BR-NO            TO D01-BRNO
                    MOVE WK-DATE          TO D01-DATE
                    MOVE CA-TILLNO        TO D01-TILLNO
                    MOVE WK-OPENID        TO D01-A15
                    MOVE CA-OPEN-USERID   TO D01-S35
                    PERFORM WRITE-DETAIL-LINE-ERROR.

           IF WK-NET-CASH-NOTES NOT NUMERIC
              MOVE 0 TO WK-NET-CASH-NOTES.
           IF WK-NET-CASH-NOTES NOT = CA-NET-CASH-NOTES
              MOVE "CA-NET-CASH-NOTES" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE WK-DATE             TO D01-DATE
              MOVE CA-TILLNO           TO D01-TILLNO
              MOVE WK-NET-CASH-NOTES   TO D01-A15-NUM
              MOVE CA-NET-CASH-NOTES   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CA-DESC NOT = WK-DESC
      *       CALL "C$JUSTIFY" USING WK-DESC, "L"
              IF CA-DESC NOT = WK-DESC
                 MOVE "CA-DESC"  TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE WK-DATE    TO D01-DATE
                 MOVE CA-TILLNO  TO D01-TILLNO
                 MOVE WK-DESC    TO D01-A15
                 MOVE CA-DESC    TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CA-PASSWORD NOT = WK-PASSWORD
      *       CALL "C$JUSTIFY" USING WK-PASSWORD, "L"
              IF CA-PASSWORD NOT = WK-PASSWORD
                 MOVE "CA-PASSWORD" TO D01-TEXT
                 MOVE BR-NO         TO D01-BRNO
                 MOVE WK-DATE       TO D01-DATE
                 MOVE CA-TILLNO     TO D01-TILLNO
                 MOVE WK-PASSWORD   TO D01-A15
                 MOVE CA-PASSWORD   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

       EXIT-COMPARE-FIELDS.
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
       WRITE-DETAIL-LINE-ERROR SECTION.
      ******************************************************************
           ADD 1 TO TOTAL-ERRORS-CTR.
           PERFORM WRITE-DETAIL-LINE.

       WRITE-DETAIL-LINE-ERROR-EXIT.
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

           MOVE "BEGINNING DATE                : "    TO R01-PROMPT.
           MOVE BEG-DATE                              TO R01-NUMERIC.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    DATE                : "    TO R01-PROMPT.
           MOVE END-DATE                              TO R01-NUMERIC.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "A15 RECORDS PROCESSED         :"     TO R01-PROMPT.
           MOVE A15-RECORDS-PROCESSED                 TO R01-NUMERIC.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "WK RECORDS PROCESSED          :"     TO R01-PROMPT.
           MOVE WK-RECORD-CTR                         TO R01-NUMERIC.
           MOVE 01                                    TO LN-FEED.
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
           MOVE EXT-CONAME-CONAME TO CAFILE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CAFILE-SYSDATE.
           DISPLAY CAFILE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE CAFILE-FORM-FIELDS TO WR-BUF.
           MOVE CAFILE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE CAFILE-HELP--FILE TO HELP-LINK-FILE.
           MOVE CAFILE-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/CAFILE_EVA.CPY".
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
       COPY "LIBLP/LPCAGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
      * COPY "LIBLP/LPCA1RN.CPY".
      * COPYMEMBER: LIBLP/LPCA1RN
       LOAD-CA1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CA-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CA-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO CA-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CA-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO CA-ALL-DATA
              MOVE CA-ALL-PATH             TO CA-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CA-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO CA-ALL-BASE
              MOVE FILPATH-MACHINE         TO CA-ALL-MACHINE
              MOVE FILPATH-DATA            TO CA-ALL-DATA
              MOVE CA-ALL-PATH             TO CA-PATH.

      *-----------------------------------------------------------------
       OPEN-CA1-FILE.
           PERFORM LOAD-CA1-FILE.
           PERFORM OPEN-IT.
           MOVE CA-PATH-SQL    TO E-FILE.

           IF CA1-CONN-OPEN-SW = "N" 
              MOVE "Y"         TO CA1-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS CA1_CONNECT 
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-CA1-FILE.
           PERFORM START-IT.
           MOVE CA-PATH-OWNBR  TO CA-BRNO.
           MOVE CA-PATH-SQL    TO E-FILE.
           MOVE CA1-KEY        TO E-KEYX.

           IF ( CA1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-CA1-FILE.

           IF ( QCA1-WEND-BRNO = ZEROES    ) OR
              ( QCA1-WEND-BRNO NOT NUMERIC )
              MOVE CA-PATH-OWNBR        TO QCA1-WBEG-BRNO
              MOVE CA-PATH-OWNBR        TO QCA1-WEND-BRNO
              MOVE '1900-01-01'         TO QCA1-WBEG-DATE
              MOVE '2099-12-31'         TO QCA1-WEND-DATE
              MOVE CA-TILLNO            TO QCA1-WBEG-TILLNO
              MOVE ALL "9"              TO QCA1-WEND-TILLNO.


           EXEC SQL SET CONNECTION CA1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE CA1_CURSOR CURSOR FOR
                :CA1-RETURN-CODE = EXEC SSP_CAFILE_CAFILE_SELECT 
              ( :QCA1-WBEG-BRNO,     
                :QCA1-WBEG-DATE,   
                :QCA1-WEND-DATE )      
           END-EXEC.

           EXEC SQL
               OPEN CA1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QCA1-WBEG-BRNO 
                          QCA1-WEND-BRNO
                          QCA1-WBEG-TILLNO
                          QCA1-WEND-TILLNO.
 
           MOVE SPACES TO QCA1-WBEG-DATE
                          QCA1-WEND-DATE.

           MOVE STAT---GOOD TO CA1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-CA1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE CA-PATH-OWNBR  TO CA-BRNO.
           MOVE CA-PATH-SQL    TO E-FILE.
           MOVE CA1-KEY        TO E-KEYX.
           INITIALIZE QCA-REC.

           EXEC SQL SET CONNECTION CA1_CONNECT END-EXEC.

           IF ( CA1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH CA1_CURSOR INTO :QCA-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CA1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-CA-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-CA1-FILE.

           PERFORM CLOSE-IT.
           IF ( CA1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION CA1_CONNECT END-EXEC 
              EXEC SQL
                   CLOSE CA1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC 
              MOVE STAT---BAD TO CA1-CURSOR-STAT.

      *----------------------------------------------------------------- 
       CLOSE-CA1-CONNECTION.

           IF CA1-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION CA1_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT CA1_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO CA1-CONN-OPEN-SW.

      *----------------------------------------------------------------- 

       OPEN-OCA1-FILE.
           PERFORM OPEN-IT.
           MOVE OCA-PATH TO E-FILE.
           OPEN INPUT OCA-FILE.
           IF IO-FG = 8
              GO TO OPEN-OCA1-FILE.
           IF IO-FG = 7
              CLOSE OCA-FILE
              GO TO OPEN-OCA1-FILE.
      *READ-OCA1-FILE.
      *    PERFORM READ-IT.
      *    MOVE OCA-PATH TO E-FILE.
      *    MOVE OCA1-KEY TO E-KEYX.
      *    READ OCA-FILE.
      *    IF IO-FG = 8
      *       GO TO READ-OCA1-FILE.
       READ-OCA1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OCA-PATH TO E-FILE.
           MOVE OCA1-KEY TO E-KEYX.
           READ OCA-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OCA1-FILE-NEXT.
       START-OCA1-FILE.
           PERFORM START-IT.
           MOVE OCA-PATH TO E-FILE.
           MOVE OCA1-KEY TO E-KEYX.
           START OCA-FILE KEY NOT < OCA1-KEY.
       CLOSE-OCA1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OCA-FILE.

      **************       

       OPEN-WK-FILE.
           PERFORM OPEN-IT.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           OPEN I-O WK-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-FILE.
           IF IO-FG = 7
              CLOSE WK-FILE
              GO TO OPEN-WK-FILE.
       OPEN-WK-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           OPEN OUTPUT WK-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK-FILE
              GO TO OPEN-WK-FILE-OUTPUT.
       READ-WK-FILE-NEXT.
           PERFORM READ-IT.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           MOVE WK-KEY  TO E-KEYX.
           READ WK-FILE NEXT
              AT END  MOVE 9 TO IO-STATUS IO-FG
           END-READ.
           IF IO-FG = 8
              GO TO READ-WK-FILE-NEXT.
       WRITE-WK-FILE.
           PERFORM WRITE-IT.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           MOVE WK-KEY  TO E-KEYX.
           WRITE WK-REC 
             INVALID KEY 
                MOVE 2 TO IO-STATUS IO-FG
           END-WRITE.
           IF IO-FG = 8
             GO TO WRITE-WK-FILE.
           UNLOCK WK-FILE.
       CLOSE-WK-FILE.
           PERFORM CLOSE-IT.
           MOVE 0 TO IO-FG IO-STATUS.
           CLOSE WK-FILE.

      *=================================================================
       START-MISC-CSR.
           MOVE "SSP_TCLP_SELECT" TO E-FILE.
           MOVE SPACES            TO E-KEYX.
           MOVE "START ERROR"     TO E-MSG.
           MOVE "GB/SQLINF"       TO E-PROG.

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
           MOVE SPACES            TO E-KEYX.
           MOVE "READ ERROR"      TO E-MSG.
           MOVE "GB/SQLINF"       TO E-PROG.

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
           MOVE SPACES            TO E-KEYX.
           MOVE "CLOSE ERROR"     TO E-MSG.
           MOVE "GB/SQLINF"       TO E-PROG.

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
              DESTROY CAFILE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
