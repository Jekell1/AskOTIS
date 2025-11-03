       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SSFILE.
       DATE-WRITTEN.  11-06-2023.
      ******************************************************************
      *
      * DESCRIPTION   : COMPARE S35 SSFILE WITH A15 SSFILE
      *            
      *                 TO BE USED AFTER AN UPDATE
      *                 WILL COMPARE EACH FIELD AND REPORT ANY 
      *                 DIFFERENCES, AS LONG AS FIELD EXIST IN BOTH FILES
      *
      *  LOOPS THRU THE A15 FILE AND VERIFIES THE S35 FILE EXIST AND MATCHES
      *
      *
      *  FROM UP1534:  SKIP SSFILE RECORDS OLDER THAN 6 YEARS FROM SYSTEM 
      *
      * 01-08-2025 RMZ - ADDED 'BTRACK' - PROCESS TRACKING TO PROGRAM.
      * 02-20-2025 MB  - CHANGED TO NOT ALLOW BEG-DATE < 201901 IN BATCH RUN
      *                   AND USE 201901 BEG DATE FOR F3 ALL DATE REQUEST.
      * 
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
      * COPYMEMBER: LIBLP/LPFSSS
           SELECT OSS-FILE ASSIGN TO OSS-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OSS1-KEY
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
       FD  OSS-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A15SRC/LP01SS.CPY" REPLACING LEADING "SS" BY "OSS".

       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
              05  WK-KEY-DATE         PIC 9(6).
              05  WK-KEY-BRNO         PIC 9(4).
              05  WK-KEY-CLASS        PIC 9(3).
           03  WK-DATA                PIC X(OSS-SIZE).

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)UT/SSFILE.CBL S35 01/03/25-09:24:01 >".

       COPY "LIBLP/LP01SS.CPY".
       COPY "LIBLP/LP01SS_SQL.CPY".
       COPY "LIBLP/LPWSSS.CPY".

       01  OSS-PATH                    PIC X(35) VALUE SPACES.
       01  OSS-ALL-PATH.
           03  OSS-ALL-BASE     PIC X(9).
           03  FILLER           PIC X     VALUE "/".
           03  OSS-ALL-MACHINE  PIC XX.
           03  FILLER           PIC X(10) VALUE
               "/LP/SSFILE".

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
 
       01  WK-EOF-SW                   PIC X      VALUE "N".
       01  SS-EOF-SW                   PIC X      VALUE "N".
       01  OSS-EOF-SW                  PIC X      VALUE "N".
       01  SS1-CONN-OPEN-SW            PIC X      VALUE "N".
       01  TOTAL-ERRORS-CTR            PIC 9(9)   VALUE 0.

       01  PROCESS-STAT                PIC X(08)    VALUE SPACES.
           88  PROCESS-STAT-GOOD                    VALUE "00".
           88  PROCESS-STAT-BAD                     VALUE "  " THRU "//"
                                                         "01" THRU "~~".

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

      *-----------------------------------------------------------------
       01  LEGEND-BEG-DATE.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F7-EXIT, ENT".
           03  FILLER PIC X(30) VALUE "ER BEGINNING DATE (YYYYMM)    ".
           03  FILLER PIC X(19) VALUE "                   ".

       01  LEGEND-END-DATE.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER ENDIN".
           03  FILLER PIC X(30) VALUE "G DATE (YYYYMM)               ".
           03  FILLER PIC X(19) VALUE "                   ".

      *-----------------------------------------------------------------
       01  LEGEND-ACCEPT-YN.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT             ".
           03  FILLER PIC X(30) VALUE "                              ".
           03  FILLER PIC X(19) VALUE "     ACCEPT (Y/N): ".

       01  SOURCE-PATH-LIST            PIC X(07)     VALUE "S-PATH.".

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  SOURCE-PATH             PIC X(9).
           03  BEG-DATE                PIC 9(06).
           03  END-DATE                PIC 9(06).

       01  VERSION-CONTROL             PIC X(01)     VALUE "A".

      *-----------------------------------------------------------------

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
           03  H02-TITLE               PIC X(40) VALUE 
           "BRANCH STAT (SSFILE) COMPARE A15 TO S35".

       01  HEADER-LINE-03.
           03  FILLER                PIC X(4) VALUE "BRNO".
           03  FILLER                PIC X(2) VALUE SPACES.
           03  FILLER                PIC X(6) VALUE " DATE ".
           03  FILLER                PIC X(3) VALUE SPACES.
           03  FILLER                PIC X(5) VALUE "CLASS".
           03  FILLER                PIC X(2) VALUE SPACES.
           03  FILLER                PIC X(28) VALUE "DESCRIPTION".
           03  FILLER                PIC X(3) VALUE "A15".
           03  FILLER                PIC X(19) VALUE SPACES.
           03  FILLER                PIC X(3) VALUE "S35".

       01  DETAIL-LINE                        PIC X(100)   VALUE SPACES.
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-BRNO                       PIC Z(3)9.
           03  FILLER                         PIC XX.
           03  D01-DATE                       PIC 9(6).
           03  FILLER                         PIC X(4).
           03  D01-CLASS                      PIC 9(3).
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
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBUT/SSFILE_DEF.CPY".
       COPY "LIBUT/SSFILE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/SSFILE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE OSS-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SS1-FILE.
           PERFORM CLOSE-PR-FILE.
           PERFORM CLOSE-SS1-CONNECTION.
           CLOSE OSS-FILE WK-FILE.
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
           MOVE "SSFILE" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE SSFILE-QUEUE-POS TO Q-POS.
           MOVE SSFILE-COPIES-POS TO C-POS.
           MOVE SSFILE-PRU-POS   TO P-POS.
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

           PERFORM OPEN-PR-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WK-PATH.

           PERFORM START-MISC-CSR.
           PERFORM READ-MISC-CSR-NEXT.
           PERFORM CLOSE-MISC-CSR.

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

      *-----------------------------------------------------------------
       ENTER-BEG-DATE.

           MOVE LEGEND-BEG-DATE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN060BEG-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-SOURCE-PATH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F3-KEY ) GO TO ENTER-ALL-DATE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-DATE.

           MOVE VDUNUM0 TO BEG-DATE.

      *-----------------------------------------------------------------
       ENTER-END-DATE.

           MOVE LEGEND-BEG-DATE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN060END-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-DATE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-DATE.

           MOVE VDUNUM0 TO END-DATE.

           IF ( BEG-DATE > END-DATE )
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DATE.

           GO TO ENTER-PARAMETERS-END.

      *-----------------------------------------------------------------
       ENTER-ALL-DATE.

           MOVE "SN N060BEG-DATE."  TO VDU.
           MOVE 201901              TO BEG-DATE VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N020END-DATE."  TO VDU.
           MOVE 204912                TO END-DATE VDUNUM0.
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
           IF BEG-DATE < 201901
              MOVE 201901 TO BEG-DATE.

      ****************************************
       DISPLAY-PARAMETERS SECTION.
      ****************************************

           MOVE "S  A090S-PATH."         TO VDU.
           MOVE SOURCE-PATH              TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SN N060BEG-DATE."       TO VDU.
           MOVE BEG-DATE                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N060END-DATE."       TO VDU.
           MOVE END-DATE                 TO VDUNUM0.
           PERFORM SCREENIO.

       DISPLAY-PARAMETERS-EXIT.
           EXIT.

      *******************************************
       EXECUTION-MODULE SECTION.
      *******************************************

           MOVE 9999                       TO PROGID-SLEEP-COUNTER.
           PERFORM SEND-PROGRESS-INDICATOR.

      D    STOP MESS.

      * BEG-DATE IS YYYYMM
           MOVE BEG-DATE TO QSS1-WBEG-DATE OSS-DATE.
           MOVE END-DATE TO QSS1-WEND-DATE.

           MOVE "PROCESSING S35 DATA" TO BTRACK-MESS. 
           PERFORM BTRACK-GLOBAL.

           PERFORM OPEN-SS1-FILE.
           PERFORM START-SS1-FILE.  *> SSP  SSFILE
           PERFORM M-READ-SS.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
      *     MOVE BR-DATA-PATH               TO FILPATH-DATA.

      * DELETE/EMPTY TEMP FILE IF EXIST
           PERFORM OPEN-WK1-FILE-OUTPUT.

           MOVE "PROCESSING A15 DATA" TO BTRACK-MESS.
           PERFORM BTRACK-GLOBAL.

      *** MUST LOAD R1 AND R2 SSFILE ***
      *** BYPASS DUPS FROM R2        ***
           MOVE SOURCE-PATH  TO OSS-ALL-BASE.
           MOVE MACHINE-R1   TO OSS-ALL-MACHINE FILPATH-MACHINE.
           MOVE OSS-ALL-PATH TO OSS-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE "MISSING R1 FILE" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO PROCESS-DRIVER-END.

           MOVE "N" TO OSS-EOF-SW.
           PERFORM OPEN-OSS1-FILE.
           PERFORM LOAD-WORK-FILE.
           PERFORM CLOSE-OSS1-FILE. 

           MOVE SOURCE-PATH  TO OSS-ALL-BASE.
           MOVE MACHINE-R2   TO OSS-ALL-MACHINE FILPATH-MACHINE.
           MOVE OSS-ALL-PATH TO OSS-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE "MISSING R2 FILE" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO PROCESS-DRIVER-END.

           MOVE "N" TO OSS-EOF-SW.
           PERFORM OPEN-OSS1-FILE.
           PERFORM LOAD-WORK-FILE.
           PERFORM CLOSE-OSS1-FILE. 
           PERFORM CLOSE-WK1-FILE.

           PERFORM OPEN-WK1-FILE.
           PERFORM M-READ-WK1.

           PERFORM PROCESS-NEXT-SS
              UNTIL SS-EOF-SW = "Y" AND WK-EOF-SW = "Y".

           PERFORM CLOSE-SS1-FILE.
           PERFORM CLOSE-WK1-FILE.

           GO TO PROCESS-DRIVER-END.

       PROCESS-NEXT-SS.
           PERFORM SEND-PROGRESS-INDICATOR.

           PERFORM COMPARE-FIELDS.

           IF SS1-KEY < WK-KEY
              PERFORM M-READ-SS
           ELSE
              IF SS1-KEY > WK-KEY
                 PERFORM M-READ-WK1
              ELSE
                 PERFORM M-READ-SS
                 PERFORM M-READ-WK1.

       PROCESS-DRIVER-END.
           EXIT.

       LOAD-WORK-FILE SECTION.
           PERFORM M-READ-OSS.

           IF OSS-EOF-SW = "N"
              INITIALIZE WK-REC
              MOVE OSS-DATE   TO WK-KEY-DATE 
              MOVE OSS-BRNO   TO WK-KEY-BRNO
              MOVE OSS-CLASS  TO WK-KEY-CLASS
              MOVE OSS-REC    TO WK-DATA
              PERFORM WRITE-WK1-FILE
              IF IO-GOOD 
                  ADD 1 TO A15-RECORDS-PROCESSED
              END-IF
              GO TO LOAD-WORK-FILE.

       LOAD-WORK-FILE-END.
           EXIT. 

       M-READ-WK1 SECTION.
           PERFORM READ-WK1-FILE-NEXT.

           IF IO-BAD 
              MOVE "Y"     TO WK-EOF-SW
              MOVE ALL '9' TO WK-KEY
           ELSE
              MOVE WK-DATA TO OSS-REC.

       EXIT-M-READ-WK1.
           EXIT.

       M-READ-SS SECTION.
           PERFORM READ-SS1-FILE-NEXT.   *> SSP  SSFILE
           IF IO-BAD
              MOVE ALL '9' TO SS1-KEY
              MOVE "Y"     TO SS-EOF-SW
           ELSE
              ADD 1 TO SQL-RECORDS-PROCESSED.

       EXIT-M-READ-SS.
           EXIT.

       M-READ-OSS SECTION.
           PERFORM READ-OSS1-FILE-NEXT.
           IF IO-BAD OR OSS-DATE > END-DATE
              MOVE "Y"      TO OSS-EOF-SW
           ELSE
      * DIDNT CONVERT PRIOR TO 6 YEARS AGO
              MOVE OSS-DATE TO DATE-YYYYMM
              IF DATE-YYYYMM-YYYY < 2017
                 GO TO M-READ-OSS
              END-IF
              IF OSS-DATE < BEG-DATE OR OSS-DATE > END-DATE
                  GO TO M-READ-OSS.

       EXIT-M-READ-OSS.
           EXIT.

      ******************************************************************
       COMPARE-FIELDS SECTION.
      ******************************************************************

           IF WK-KEY < SS1-KEY
              MOVE "MISSING S35 RECORD" TO D01-TEXT
              MOVE OSS-BRNO             TO D01-BRNO
              MOVE OSS-CLASS            TO D01-CLASS
              MOVE OSS-DATE             TO D01-DATE
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.

           IF WK-KEY > SS1-KEY
              MOVE "MISSING A15 RECORD" TO D01-TEXT
              MOVE SS-BRNO              TO D01-BRNO
              MOVE SS-CLASS             TO D01-CLASS
              MOVE SS-DATE-YYYYMM       TO D01-DATE
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.

           PERFORM VARYING S3 FROM 1 BY 1 UNTIL S3 > 5
             AFTER S2 FROM 1 BY 1 UNTIL S2 > 2
              AFTER S1 FROM 1 BY 1 UNTIL S1 > 8
      *          IF OSS-UNITS (S1 S2 S3) NOT NUMERIC
      *             MOVE 0 TO OSS-UNITS(S1 S2 S3)
      *          END-IF
                IF OSS-UNITS (S1 S2 S3) NOT = SS-UNITS (S1 S2 S3)
                   MOVE SPACES              TO D01-TEXT
                   STRING "SS-UNITS",S1,S2,S3 INTO D01-TEXT
                   MOVE OSS-BRNO            TO D01-BRNO
                   MOVE SS-DATE-YYYYMM      TO D01-DATE
                   MOVE SS-CLASS            TO D01-CLASS
                   MOVE OSS-UNITS(S1 S2 S3) TO D01-A15 
                   MOVE SS-UNITS(S1 S2 S3)  TO D01-S35 
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
      *          IF OSS-OUTBAL (S1 S2 S3) NOT NUMERIC
      *             MOVE 0 TO OSS-OUTBAL(S1 S2 S3)
      *          END-IF
                IF OSS-OUTBAL (S1 S2 S3) NOT = SS-OUTBAL (S1 S2 S3)
                   MOVE SPACES               TO D01-TEXT
                   STRING "SS-OUTBAL",S1,S2,S3 INTO D01-TEXT
                   MOVE OSS-BRNO             TO D01-BRNO
                   MOVE SS-DATE-YYYYMM       TO D01-DATE
                   MOVE SS-CLASS             TO D01-CLASS
                   MOVE OSS-OUTBAL(S1 S2 S3) TO D01-A15-NUM
                   MOVE SS-OUTBAL(S1 S2 S3)  TO D01-S35-NUM
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
                IF S3 < 3
      *          IF OSS-NO-PL (S1 S2 S3) NOT NUMERIC
      *             MOVE 0 TO OSS-NO-PL(S1 S2 S3)
      *          END-IF
                IF OSS-NO-PL (S1 S2 S3) NOT = SS-NO-PL (S1 S2 S3)
                   MOVE SPACES              TO D01-TEXT
                   STRING "SS-NO-PL",S1,S2,S3 INTO D01-TEXT
                   MOVE OSS-BRNO            TO D01-BRNO
                   MOVE SS-DATE-YYYYMM      TO D01-DATE
                   MOVE SS-CLASS            TO D01-CLASS
                   MOVE OSS-NO-PL(S1 S2 S3) TO D01-A15 
                   MOVE SS-NO-PL(S1 S2 S3)  TO D01-S35 
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
      *          IF OSS-AMT-PL (S1 S2 S3) NOT NUMERIC
      *             MOVE 0 TO OSS-AMT-PL(S1 S2 S3)
      *          END-IF
                IF OSS-AMT-PL (S1 S2 S3) NOT = SS-AMT-PL (S1 S2 S3)
                   MOVE SPACES               TO D01-TEXT
                   STRING "SS-AMT-PL",S1,S2,S3 INTO D01-TEXT
                   MOVE OSS-BRNO             TO D01-BRNO
                   MOVE SS-DATE-YYYYMM       TO D01-DATE
                   MOVE SS-CLASS             TO D01-CLASS
                   MOVE OSS-AMT-PL(S1 S2 S3) TO D01-A15-NUM
                   MOVE SS-AMT-PL(S1 S2 S3)  TO D01-S35-NUM
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
               END-IF
           END-PERFORM.

       EXIT-COMPARE-FIELDS.
           EXIT.

       WRITE-DETAIL-LINE-ERROR SECTION.

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
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/PROGID.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO SSFILE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO SSFILE-SYSDATE.
           DISPLAY SSFILE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE SSFILE-FORM-FIELDS TO WR-BUF.
           MOVE SSFILE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE SSFILE-HELP--FILE TO HELP-LINK-FILE.
           MOVE SSFILE-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/SSFILE_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

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
           MOVE EXT-ROUTE-BUF          TO PROGID-ROUTE-BUF.
           MOVE 9999                   TO PROGID-SLEEP-COUNTER.
           MOVE 250                    TO PROGID-SLEEP-COUNTER-MAX.

           PERFORM GET-TIME.
           MOVE TIME-EDIT              TO PROGID-TIME.

           MOVE "B"                    TO PROGID-TYPE.
           MOVE PROGID-TEXT-PROCESSING TO PROGID-TEXT.

      *-----------------------------------------------------------------
       SEND-PROGRESS-INDICATOR.

      *     MOVE BR-NO                      TO PROGID-BRANCH.
           STRING "PROCESSING DATE: ", OSS-DATE INTO PROGID-TEXT.

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
       COPY "LIBLP/LPSSGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".

      * COPY "LIBLP/LPSS1RN.CPY".
      * COPYMEMBER: LIBLP/LPSS1RN
       LOAD-SS1-FILE.

      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( SS-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( SS-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO SS-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO SS-ALL-MACHINE
              MOVE SS-ALL-PATH             TO SS-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( SS-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO SS-ALL-BASE
              MOVE FILPATH-MACHINE         TO SS-ALL-MACHINE
              MOVE SS-ALL-PATH             TO SS-PATH.

      *-----------------------------------------------------------------
       OPEN-SS1-FILE.
           PERFORM LOAD-SS1-FILE.
           PERFORM OPEN-IT.
           MOVE SS-PATH-SQL TO E-FILE.

           IF SS1-CONN-OPEN-SW = "N" 
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS SS1_CONNECT
                        USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE 'Y' TO SS1-CONN-OPEN-SW.

      *-----------------------------------------------------------------
       START-SS1-FILE.
           PERFORM START-IT.
           MOVE SS-PATH-SQL TO E-FILE.
           MOVE SS1-KEY TO E-KEYX.

           IF ( SS1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-SS1-FILE.

      *      SELECT *
      *      FROM DBO.SSFILE
      *      WHERE SSFILE.SS_DATE_YYYYMM BETWEEN @QSS_WBEG_DATE AND @QSS_WEND_DATE
      *      ORDER BY SSFILE.SS_DATE_YYYYMM,
      *               SSFILE.SS_BRNO,
      *               SSFILE.SS_CLASS

           EXEC SQL SET CONNECTION SS1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE SS1_CURSOR CURSOR FOR
                :SS1-RETURN-CODE =  
            EXEC SSP_SSFILE_SSFILE_SELECT
              ( :QSS1-WBEG-DATE, :QSS1-WEND-DATE)
           END-EXEC.

           EXEC SQL
               OPEN SS1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           MOVE STAT---GOOD TO SS1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-SS1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE SS-PATH-SQL TO E-FILE.
           MOVE SS1-KEY TO E-KEYX.
           INITIALIZE QSS-REC.

           EXEC SQL SET CONNECTION SS1_CONNECT END-EXEC.

           IF ( SS1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH SS1_CURSOR INTO :QSS-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SS1-FILE-NEXT.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SS1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-SS-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-SS1-FILE.
           PERFORM CLOSE-IT.

           IF ( SS1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION SS1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE SS1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO SS1-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-SS1-CONNECTION.

           IF SS1-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION SS1_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT SS1_CONNECT END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE "N" TO SS1-CONN-OPEN-SW.

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
       OPEN-OSS1-FILE.
           PERFORM OPEN-IT.
           MOVE OSS-PATH TO E-FILE.
           OPEN INPUT OSS-FILE.
           IF IO-FG = 8
              GO TO OPEN-OSS1-FILE.
           IF IO-FG = 7
              CLOSE OSS-FILE
              GO TO OPEN-OSS1-FILE.
      *READ-OSS1-FILE.
      *    PERFORM READ-IT.
      *    MOVE OSS-PATH TO E-FILE.
      *    MOVE OSS1-KEY TO E-KEYX.
      *    READ OSS-FILE.
      *    IF IO-FG = 8
      *       GO TO READ-OSS1-FILE.
       START-OSS1-FILE.
           PERFORM START-IT.
           MOVE OSS-PATH TO E-FILE.
           MOVE OSS1-KEY TO E-KEYX.
           START OSS-FILE KEY NOT < OSS1-KEY.
       READ-OSS1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OSS-PATH TO E-FILE.
           MOVE OSS1-KEY TO E-KEYX.
           READ OSS-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OSS1-FILE-NEXT.
       CLOSE-OSS1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OSS-FILE.

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

      *     IF A15-RECORDS-PROCESSED = 0 AND SQL-RECORDS-PROCESSED = 0
      *        MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
      *        PERFORM SEND-MESS.

       EXIT-PROG.

           PERFORM BTRACK-DONE.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE EXIT-PATH       TO FORM-NAM
              MOVE FORM-PATH       TO FORM-PATHNAME.

           MOVE WK-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY SSFILE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
