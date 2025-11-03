       IDENTIFICATION DIVISION.
       PROGRAM-ID.    LMFILE.
       DATE-WRITTEN.  11-01-2023.
      ******************************************************************
      *
      * DESCRIPTION   : COMPARE S35 LMFILE WITH A15 LMFILE
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
      *
      *  01-07-25  RMZ  ADDED 'BTRACK' - PROCESS TRACKING TO PROGRAM.
      *  03-26-25  MB   ADDED BEG-DATE, END-DATE IN FORM/PROCESSING.
      *  05-01-25  MB   MISC REPORT CHANGES FOR MISSING RECS
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
      * COPYMEMBER: LIBLP/LPFSLM
           SELECT OLM-FILE ASSIGN TO OLM-PATH
                  WITH COMPRESSION
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OLM1-KEY
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  OLM-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A15SRC/LP01LM.CPY" REPLACING LEADING "LM" BY "OLM".

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/LMFILE S35 05/03/24-05:15:39 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".
       01  OLM-PATH                    PIC X(35) VALUE SPACES.
       01  OLM-ALL-PATH.
           03  OLM-ALL-BASE     PIC X(9).
           03  FILLER           PIC X     VALUE "/".
           03  OLM-ALL-MACHINE  PIC XX.
           03  FILLER           PIC X     VALUE "/".
           03  OLM-ALL-DATA     PIC X(6).
           03  FILLER           PIC X(10) VALUE
               "/LP/LMFILE".

       01  LM1-CONN-OPEN-SW           PIC X        VALUE "N".

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

       01  LM-EOF-SW                   PIC X      VALUE "N".
       01  OLM-EOF-SW                  PIC X      VALUE "N".

       01  WS-OLM1-KEY.
           03 WS-OLM-BRNO              PIC 9(04)  VALUE ZEROES.
           03 WS-OLM-ACCTNO            PIC 9(08)  VALUE ZEROES.
           03 WS-OLM-DATE-REV          PIC 9(08)  VALUE ZEROES.
           03 WS-OLM-SEQ               PIC 9(03)  VALUE ZEROES.

       01  WS-LM1-KEY.
           03 WS-LM-BRNO               PIC 9(04)  VALUE ZEROES.
           03 WS-LM-ACCTNO             PIC 9(08)  VALUE ZEROES.
           03 FILLER REDEFINES WS-LM-ACCTNO.
              05 WS-LM-ACCTNO16        PIC 9(06).
              05 WS-LM-ACCTNO78        PIC 9(02).
           03 WS-LM-DATE-REV           PIC 9(08)  VALUE ZEROES.
           03 WS-LM-SEQ                PIC 9(03)  VALUE ZEROES.

       01  WORK-ACCTNO. 
           03 WORK-ACCTNO12            PIC 9(02).
           03 WORK-ACCTNO38            PIC 9(06).

      * TFR TO BR XXXX FR BR XXXX, FOR
      * THE END OF THIS MEMO, WE CHANGED THE ACCTNO IN S35 TO BE 8 DIGITS
       01  HOLD-MEMO-A15              PIC X(30).
       01  HOLD-MEMO-A15X REDEFINES HOLD-MEMO-A15.
           03  HOLD-MEMO-TFR          PIC X(9).
           03  HOLD-MEMOX    REDEFINES HOLD-MEMO-TFR.
                05  HOLD-MEMO-FB      PIC X(6).
       01  HOLD-MEMO-S35              PIC X(30).

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

       01  PROCESS-STAT                PIC X(08)  VALUE SPACES.
           88  PROCESS-STAT-GOOD                  VALUE "00".
           88  PROCESS-STAT-BAD                   VALUE "  " THRU "//"
                                                        "01" THRU "~~".

       01  A15-RECORDS-PROCESSED           PIC 9(9) VALUE 0.
           88  NO-RECORDS-FOUND                     VALUE ZEROES.

       01  BEG-OLM-DATE-REV            PIC 9(8)      VALUE 0.
       01  END-OLM-DATE-REV            PIC 9(8)      VALUE 0.
       01  TOTAL-ERRORS-CTR            PIC 9(9)      VALUE 0.
       01  S35-RECORD-CTR              PIC 9(10)     VALUE ZEROES.  
       01  A15-RECORD-CTR              PIC 9(10)     VALUE ZEROES. 

       01  STARTING-TIME               PIC X(08)     VALUE SPACES.
       01  SUB                         PIC 9(04)     VALUE ZEROES.

       01  MESS-LIST                   PIC X(5)      VALUE "MESS.".

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
           03  BEG-DATE                PIC 9(8).
           03  END-DATE                PIC 9(8).

       01  VERSION-CONTROL             PIC X(01)     VALUE "B".

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
           "LOAN MEMO FILE (LMFILE) COMPARE A15 TO S35".

       01  HEADER-LINE-03.
           03  FILLER                PIC X(23) VALUE "LM-KEY ".
           03  FILLER                PIC X(3)  VALUE SPACES.
           03  FILLER                PIC X(11) VALUE "DESCRIPTION".
           03  FILLER                PIC X(17) VALUE SPACES.
           03  FILLER                PIC X(3)  VALUE "A15".
           03  FILLER                PIC X(24) VALUE SPACES.
           03  FILLER                PIC X(3)  VALUE "S35".

      ******************************************************************
      *
      *    D E T A I L - L I N E - W O R K E R S
      *
      ******************************************************************
       01  DETAIL-LINE                        PIC X(133) VALUE SPACES.

       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-KEY                        PIC X(23).
           03  FILLER                         PIC X(3).
           03  D01-TEXT                       PIC X(25).
           03  FILLER                         PIC X(3).
           03  D01-A15                        PIC X(25).
           03  FILLER REDEFINES D01-A15.
               05  D01-A15-NUM                PIC Z(7)9.9(3).
               05  FILLER                     PIC X(13).
           03  FILLER                         PIC X(2).
           03  D01-S35                        PIC X(25).
           03  FILLER REDEFINES D01-S35.
               05  D01-S35-NUM                PIC Z(7)9.9(3).
               05  FILLER                     PIC X(13).

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
       COPY "LIBUT/LMFILE_DEF.CPY".
       COPY "LIBUT/LMFILE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/LMFILE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE OLM-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-LM1-CONNECTION.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE OLM-FILE.
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
           MOVE "LMFILE" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE LMFILE-QUEUE-POS TO Q-POS.
           MOVE LMFILE-COPIES-POS TO C-POS.
           MOVE LMFILE-PRU-POS   TO P-POS.
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

           MOVE ZEROES                     TO A15-RECORDS-PROCESSED
                                              A15-RECORD-CTR
                                              S35-RECORD-CTR.

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

      *-----------------------------------------------------------------
       ENTER-BEGDATE.

           MOVE "ERNM060BEG-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-ACCTNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F3-KEY ) GO TO ENTER-ALL-DATE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEGDATE.

           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.
      *-----------------------------------------------------------
       ENTER-ENDDATE.

           MOVE "ERNM060END-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEGDATE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-ENDDATE.

           MOVE VDU-DATE-YYYYMMDD  TO END-DATE.

      * MAKE SURE THE DATE RANGE IS VALID.

           IF BEG-DATE NOT = 0
              IF BEG-DATE > END-DATE
                 MOVE "INVALID DATE RANGE" TO MESS
                 PERFORM SEND-MESS
                 GO TO ENTER-BEGDATE.

           GO TO ENTER-PARAMETERS-END.

      *----------------------------------------------------------
       ENTER-ALL-DATE.

           MOVE "S  8080BEG-DATE." TO VDU.
           MOVE EXT-JULIAN-BEG TO BEG-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "S  8080END-DATE."   TO VDU.
           MOVE EXT-JULIAN-END TO END-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

      *----------------------------------------------------------
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

           MOVE "SNNN060BEG-DATE."        TO VDU.
           MOVE BEG-DATE                  TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN080END-DATE."       TO VDU.
           MOVE END-DATE                 TO VDUNUM0.
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

      *     MOVE 9999                       TO PROGID-SLEEP-COUNTER.
      *     PERFORM SEND-PROGRESS-INDICATOR.

      D    STOP MESS.

           MOVE SOURCE-PATH  TO OLM-ALL-BASE.
           MOVE MACHINE-R1   TO OLM-ALL-MACHINE.
           MOVE BR-DATA-PATH TO OLM-ALL-DATA.
           MOVE OLM-ALL-PATH TO OLM-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE MACHINE-R2 TO OLM-ALL-MACHINE
              MOVE OLM-ALL-PATH TO OLM-PATH ACCESS-BUF
              PERFORM ACCESS-CALL
              IF ( STAT-BAD )
                 MOVE "MISSING A15 FILE" TO D01-TEXT 
                 MOVE OLM-ALL-PATH       TO D01-A15
                 MOVE SPACES             TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR
                 GO TO PROCESS-BRANCH-DRIVER-END.

            IF EXT-ROUTE-BUF = "00"
              STRING "REPORT IN PROGRESS......", BR-NO INTO LEGEND
              PERFORM SEND-LEGEND.

           MOVE EXT-FILPATH-BASE      TO FILPATH-BASE.
           MOVE BR-MACHINE            TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH          TO FILPATH-DATA.

           MOVE "B"                   TO LM-PATH-OVERRIDE.
           MOVE "N"                   TO OLM-EOF-SW
                                         LM-EOF-SW. 

           PERFORM OPEN-LM1-FILE.
           PERFORM OPEN-OLM1-FILE.

           IF END-DATE = 20491231
              MOVE 99999999 TO QLM1-WEND-DATE-REV END-OLM-DATE-REV
           ELSE 
              SUBTRACT BEG-DATE FROM 99999999 GIVING QLM1-WEND-DATE-REV
                                                     END-OLM-DATE-REV.

           IF BEG-DATE = 19500101
              MOVE 0 TO QLM1-WBEG-DATE-REV BEG-OLM-DATE-REV
           ELSE
              SUBTRACT END-DATE FROM 99999999 GIVING QLM1-WBEG-DATE-REV
                                                     BEG-OLM-DATE-REV.

           MOVE BEG-ACCTNO            TO OLM-ACCTNO.
           MOVE 0                     TO OLM-DATE.
           MOVE 0                     TO OLM-SEQ.

           PERFORM START-OLM1-FILE.

           MOVE LM-PATH-OWNBR         TO QLM1-WBEG-BRNO.
           MOVE BEG-ACCTNO            TO QLM1-WBEG-ACCTNO.
           MOVE END-ACCTNO            TO QLM1-WEND-ACCTNO.
      *     MOVE 00000000              TO QLM1-WBEG-DATE-REV.
      *     MOVE 99999999              TO QLM1-WEND-DATE-REV. 
           MOVE 000                   TO QLM1-WBEG-SEQ.
           MOVE 999                   TO QLM1-WEND-SEQ.

           PERFORM START-LM1-FILE.

           PERFORM PROCESS-READ-OLM1.
           PERFORM PROCESS-READ-LM1.

       PROCESS-BRANCH-NEXT-LM.

           IF LM-EOF-SW  = "Y" AND
              OLM-EOF-SW = "Y"
              GO TO PROCESS-BRANCH-DRIVER-EXIT.

           IF WS-OLM1-KEY < WS-LM1-KEY 
              MOVE "MISSING S35 RECORD" TO D01-TEXT
              MOVE WS-OLM1-KEY          TO D01-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR
              PERFORM PROCESS-READ-OLM1 
              GO TO PROCESS-BRANCH-NEXT-LM.
 
           IF WS-LM1-KEY < WS-OLM1-KEY
              MOVE "MISSING A15 RECORD" TO D01-TEXT
              MOVE WS-LM1-KEY           TO D01-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR
              PERFORM PROCESS-READ-LM1 
              GO TO PROCESS-BRANCH-NEXT-LM.

           PERFORM COMPARE-FIELDS.

           PERFORM PROCESS-READ-OLM1.  
           PERFORM PROCESS-READ-LM1. 

           GO TO PROCESS-BRANCH-NEXT-LM.

       PROCESS-BRANCH-DRIVER-END.

           PERFORM CLOSE-LM1-FILE.

       PROCESS-BRANCH-DRIVER-EXIT.
           EXIT.

      ******************************************************************
       PROCESS-READ-OLM1 SECTION.
      ******************************************************************

           PERFORM READ-OLM1-FILE-NEXT.
           IF IO-BAD 
              MOVE 9999          TO WS-OLM-BRNO
              MOVE 99999999      TO WS-OLM-ACCTNO  
              MOVE 99999999      TO WS-OLM-DATE-REV
              MOVE 999           TO WS-OLM-SEQ
              MOVE "Y"           TO OLM-EOF-SW
              GO TO EXIT-PROCESS-READ-OLM1.

           MOVE OLM-ACCTNO TO OLD-ACCTNO1.
           IF NEW-ACCTNO1 = 0
              GO TO PROCESS-READ-OLM1.

      * THESE 2 DATES CAUSING SORT ERRORS IN THIS PROCESSING - MB 2024-07-17
           IF OLM-DATE = 80749999 or 99999999
              GO TO PROCESS-READ-OLM1.

           MOVE LM-PATH-OWNBR TO WS-OLM-BRNO.
           MOVE OLM-ACCTNO    TO WS-OLM-ACCTNO.
JKC        IF ( OLM-DATE = 99999999 )
JKC           MOVE 80299898   TO WS-OLM-DATE-REV *> 1970-01-01
           ELSE
              MOVE OLM-DATE   TO WS-OLM-DATE-REV.

           IF WS-OLM-DATE-REV < BEG-OLM-DATE-REV OR
              WS-OLM-DATE-REV > END-OLM-DATE-REV
              GO TO PROCESS-READ-OLM1.

           MOVE OLM-SEQ TO WS-OLM-SEQ.  
           ADD  01 TO A15-RECORDS-PROCESSED
                      A15-RECORD-CTR.

       EXIT-PROCESS-READ-OLM1.
           EXIT.

      ******************************************************************
       PROCESS-READ-LM1 SECTION.
      ******************************************************************

           PERFORM READ-LM1-FILE-NEXT.
           IF IO-BAD 
              MOVE 9999          TO WS-LM-BRNO
              MOVE 99999999      TO WS-LM-ACCTNO  
              MOVE 99999999      TO WS-LM-DATE-REV
              MOVE 999           TO WS-LM-SEQ              
              MOVE "Y"           TO LM-EOF-SW
              GO TO EXIT-PROCESS-READ-LM1.

      * THESE 2 DATES CAUSING SORT ERRORS IN THIS PROCESSING - MB 2024-07-17
           IF LM-DATE = 80749999 or 80299898    *> (80299898 = 99999999 IN A15)
              GO TO PROCESS-READ-LM1.


           MOVE LM-PATH-OWNBR    TO WS-LM-BRNO.

           IF LM-ACCTNO           = ZEROES
              MOVE LM-ACCTNO     TO WS-LM-ACCTNO
           ELSE
              MOVE LM-ACCTNO     TO WORK-ACCTNO    
              MOVE WORK-ACCTNO38 TO WS-LM-ACCTNO16
              MOVE 01            TO WS-LM-ACCTNO78.
                                                      
           MOVE LM-DATE          TO WS-LM-DATE-REV.
           MOVE LM-SEQ           TO WS-LM-SEQ.
           ADD 1                 TO S35-RECORD-CTR.

       EXIT-PROCESS-READ-LM1.
           EXIT.

      ******************************************************************
       COMPARE-FIELDS SECTION.
      ******************************************************************

      *    MOVE LM-PATH-OWNBR TO LM-BRNO
      *    MOVE OLM-NUMBER    TO LM-ACCTNO.
      *    MOVE OLM-DATE      TO LM-DATE.
      *    MOVE OLM-SEQ       TO LM-SEQ.
      *
      *    PERFORM READ-LM1-FILE.
      *    IF IO-FG NOT = 0
      *       MOVE "MISSING S35 RECORD" TO D01-TEXT
      *       MOVE BR-NO        TO D01-BRNO
      *       MOVE LM-ACCTNO    TO D01-ACCTNO
      *       MOVE LM1-KEY      TO D01-A15
      *       PERFORM WRITE-DETAIL-LINE
      *       GO TO EXIT-COMPARE-FIELDS.

           INSPECT OLM-MEMO REPLACING ALL X"09" BY " ".

      * THIS GOT FIXED IN THE UPGRADE BUT THE FILES HAVENT BEEN UPDATED YET
      * THIS X96 GOT CONVERTED AS A ? SOMEHOW IN S35
      *    INSPECT OLM-MEMO REPLACING ALL X"96" BY "-".
      * UNCOMMENT LINE ABOVE WHEN FILES ARE RERUN AND REMOVED THE BELOW
      * 3 LINES. 11/1/2023
           MOVE OLM-MEMO TO HOLD-MEMO-A15.
           IF HOLD-MEMO-FB = "FB ITA"
              INSPECT OLM-MEMO REPLACING ALL X"96" BY "?".

      * ONLY TEST FIRST 30 CHARACTERS OF TRANSFER MEMO, THEN ACCTNO GOT
      * CHANGED TO 8 DIGITS
           IF HOLD-MEMO-TFR = "TFR FR BR" OR "TFR TO BR"
              MOVE LM-MEMO  TO HOLD-MEMO-S35
              IF HOLD-MEMO-A15 NOT = HOLD-MEMO-S35
                 MOVE "LM-MEMO"   TO D01-TEXT
                 MOVE WS-LM1-KEY  TO D01-KEY  
                 MOVE OLM-MEMO    TO D01-A15
                 MOVE LM-MEMO     TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              GO TO COMPARE-FIELDS-TYPE.
              
           IF LM-MEMO NOT = OLM-MEMO
      *       CALL "C$JUSTIFY" USING OLM-MEMO, "L"
              IF ( LM-MEMO  NOT ALPHABETIC AND 
                   LM-MEMO  NOT NUMERIC )  OR 
                 (OLM-MEMO  NOT ALPHABETIC AND 
                  OLM-MEMO  NOT NUMERIC )   
                 NEXT SENTENCE
              ELSE
                 MOVE "LM-MEMO"  TO D01-TEXT
                 MOVE WS-LM1-KEY TO D01-KEY
                 MOVE OLM-MEMO   TO D01-A15
                 MOVE LM-MEMO    TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

       COMPARE-FIELDS-TYPE.
       
           IF LM-MEMO-TYPE NOT = OLM-MEMO-TYPE
                 MOVE "LM-MEMO-TYPE" TO D01-TEXT
                 MOVE WS-LM1-KEY     TO D01-KEY 
                 MOVE OLM-MEMO-TYPE  TO D01-A15
                 MOVE LM-MEMO-TYPE   TO D01-S35
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

           MOVE "BEGINNING ACCOUNT NUMBER      : "    TO R01-PROMPT.
           MOVE BEG-ACCTNO                            TO R01-NUMERIC.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    ACCOUNT NUMBER      : "    TO R01-PROMPT.
           MOVE END-ACCTNO                            TO R01-NUMERIC.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING DATE                : "    TO R01-PROMPT.
           MOVE BEG-DATE                              TO R01-NUMERIC.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING DATE                   : "    TO R01-PROMPT.
           MOVE END-DATE                              TO R01-NUMERIC.
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
           MOVE EXT-CONAME-CONAME TO LMFILE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LMFILE-SYSDATE.
           DISPLAY LMFILE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE LMFILE-FORM-FIELDS TO WR-BUF.
           MOVE LMFILE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE LMFILE-HELP--FILE TO HELP-LINK-FILE.
           MOVE LMFILE-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/LMFILE_EVA.CPY".
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
           MOVE PROGID-TEXT-PROCESSING     TO PROGID-TEXT.

           PERFORM GET-TIME.
           MOVE TIME-EDIT                  TO PROGID-TIME.

      *     IF EXT-ROUTE-BUF = "00"
      *        PERFORM PROGID-DISPLAY.

       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
      * COPY "LIBLP/LPLM1RN.CPY".
      * COPYMEMBER: LIBLP/LPLM1RN
       LOAD-LM1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LM-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LM-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LM-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LM-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LM-ALL-DATA
              MOVE LM-ALL-PATH             TO LM-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LM-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LM-ALL-BASE
              MOVE FILPATH-MACHINE         TO LM-ALL-MACHINE
              MOVE FILPATH-DATA            TO LM-ALL-DATA
              MOVE LM-ALL-PATH             TO LM-PATH.

      *-----------------------------------------------------------------
       OPEN-LM1-FILE.
           PERFORM LOAD-LM1-FILE.
           PERFORM OPEN-IT.
           MOVE LM-PATH-SQL TO E-FILE.

           IF LM1-CONN-OPEN-SW = "N" 
              MOVE "Y"         TO LM1-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS LM1_CONNECT 
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-LM1-FILE.
           PERFORM START-IT.
           MOVE LM-PATH-OWNBR  TO LM-BRNO.
           MOVE LM-PATH-SQL    TO E-FILE.
           MOVE LM1-KEY        TO E-KEYX.

           IF ( LM1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LM1-FILE.

      *    IF ( QLM1-WEND-BRNO = ZEROES    ) OR
      *       ( QLM1-WEND-BRNO NOT NUMERIC )
      *       MOVE LM-PATH-OWNBR        TO QLM1-WBEG-BRNO
      *       MOVE LM-PATH-OWNBR        TO QLM1-WEND-BRNO
      *       MOVE LM-ACCTNO            TO QLM1-WBEG-ACCTNO
      *       MOVE ALL "9"              TO QLM1-WEND-ACCTNO
      *       MOVE LM-DATE-REVERSED     TO QLM1-WBEG-DATE-REV
      *       MOVE ALL "9"              TO QLM1-WEND-DATE-REV
      *       MOVE LM-SEQ               TO QLM1-WBEG-SEQ
      *       MOVE ALL "9"              TO QLM1-WEND-SEQ.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           EXEC SQL SET CONNECTION LM1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE LM1_CURSOR CURSOR FOR
                :LM1-RETURN-CODE = EXEC SSP_LMFILE_LMFILE_SELECT 
              ( :QLM1-WBEG-BRNO,
                :QLM1-WBEG-ACCTNO,
                :QLM1-WEND-ACCTNO,
                :QLM1-WBEG-DATE-REV,
                :QLM1-WEND-DATE-REV,
                :QLM1-WBEG-SEQ,
                :QLM1-WEND-SEQ )
           END-EXEC.

           EXEC SQL
               OPEN LM1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLM1-WBEG-BRNO 
                          QLM1-WEND-BRNO
                          QLM1-WBEG-ACCTNO
                          QLM1-WEND-ACCTNO
                          QLM1-WBEG-DATE-REV
                          QLM1-WEND-DATE-REV
                          QLM1-WBEG-SEQ
                          QLM1-WEND-SEQ.

           MOVE STAT---GOOD TO LM1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-LM1-FILE.
           PERFORM READ-IT.
           MOVE LM-PATH-OWNBR  TO LM-BRNO. 
           MOVE LM-PATH-SQL    TO E-FILE.
           MOVE LM1-KEY        TO E-KEYX.
           INITIALIZE QLM-REC.

           MOVE LM-BRNO          TO QLM-BRNO.
           MOVE LM-ACCTNO        TO QLM-ACCTNO.
           MOVE LM-DATE-REVERSED TO QLM-DATE-REVERSED.
           MOVE LM-SEQ           TO QLM-SEQ.

           EXEC SQL
            SELECT LMFILE.LM_BRNO,
                   LMFILE.LM_ACCTNO,
                   LMFILE.LM_DATE_REVERSED,
                   LMFILE.LM_SEQ,
                   CAST(LMFILE.LM_LTOUCH_DATE AS VARCHAR(10)),
                   LMFILE.LM_LTOUCH_TIME_HHMM,
                   LMFILE.LM_LTOUCH_USERID,
                   LMFILE.LM_MEMO,
                   LMFILE.LM_MEMO_TYPE
            INTO :QLM-REC
            FROM DBO.LMFILE
            WHERE LMFILE.LM_BRNO = :QLM-BRNO
              AND LMFILE.LM_ACCTNO = :QLM-ACCTNO
              AND LMFILE.LM_DATE_REVERSED = :QLM-DATE-REVERSED
              AND LMFILE.LM_SEQ = :QLM-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LM1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LM-FIELDS.

      *-----------------------------------------------------------------
       READ-LM1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LM-PATH-OWNBR  TO LM-BRNO.
           MOVE LM-PATH-SQL    TO E-FILE.
           MOVE LM1-KEY        TO E-KEYX.
           INITIALIZE QLM-REC.

           EXEC SQL SET CONNECTION LM1_CONNECT END-EXEC.

           IF ( LM1-CURSOR-STAT-GOOD )
              EXEC SQL
               FETCH LM1_CURSOR 
                INTO :QLM-BRNO,
                     :QLM-ACCTNO,
                     :QLM-DATE-REVERSED,
                     :QLM-SEQ,
                     :QLM-MEMO,
                     :QLM-MEMO-TYPE
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LM1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LM-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-LM1-FILE.

           PERFORM CLOSE-IT.
           IF ( LM1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION LM1_CONNECT END-EXEC 
              EXEC SQL
                   CLOSE LM1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO LM1-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-LM1-CONNECTION.

           IF LM1-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION LM1_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT LM1_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO LM1-CONN-OPEN-SW.

      *----------------------------------------------------------------- 
   
       OPEN-OLM1-FILE.
           PERFORM OPEN-IT.
           MOVE OLM-PATH TO E-FILE.
           OPEN INPUT OLM-FILE.
           IF IO-FG = 8
              GO TO OPEN-OLM1-FILE.
           IF IO-FG = 7
              CLOSE OLM-FILE
              GO TO OPEN-OLM1-FILE.
      *READ-OLM1-FILE.
      *    PERFORM READ-IT.
      *    MOVE OLM-PATH TO E-FILE.
      *    MOVE OLM1-KEY TO E-KEYX.
      *    READ OLM-FILE.
      *    IF IO-FG = 8
      *       GO TO READ-OLM1-FILE.
       READ-OLM1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OLM-PATH TO E-FILE.
           MOVE OLM1-KEY TO E-KEYX.
           READ OLM-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OLM1-FILE-NEXT.
       START-OLM1-FILE.
           PERFORM START-IT.
           MOVE OLM-PATH TO E-FILE.
           MOVE OLM1-KEY TO E-KEYX.
           START OLM-FILE KEY NOT < OLM1-KEY.
       CLOSE-OLM1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OLM-FILE.

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
              DESTROY LMFILE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
