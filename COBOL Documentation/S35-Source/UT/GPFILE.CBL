       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GPFILE.
       DATE-WRITTEN.  11-27-2023.
      ******************************************************************
      *
      * DESCRIPTION   : COMPARE S35 GPFILE WITH A15 GPFILE
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
           SELECT OGP-FILE ASSIGN OGP-PATH
                  ORGANIZATION RELATIVE
                  ACCESS MODE DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RELATIVE KEY OGP-KEY
                  FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  OGP-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A15SRC/GB01GP.CPY" REPLACING LEADING "GP" BY "OGP".

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)UT/GPFILE.CBL S35 12/17/24-18:05:33 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGB/GB01GP.CPY".
       COPY "LIBGB/GB01GP_SQL.CPY".
       COPY "LIBGB/GBWSGP.CPY".

       01  OGP-KEY              PIC 9.
       01  OGP-PATH                    PIC X(35) VALUE SPACES.
       01  OGP-ALL-PATH.
           03  OGP-ALL-BASE     PIC X(9).
           03  FILLER           PIC X     VALUE "/".
           03  OGP-ALL-MACHINE  PIC XX.
           03  FILLER           PIC X(10) VALUE
               "/GB/GPFILE".

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

       01  MACHINE-R1.
           03  FILLER                  PIC X VALUE X"72".
           03  FILLER                  PIC 9 VALUE 1.
       01  MACHINE-R2.
           03  FILLER                  PIC X VALUE X"72".
           03  FILLER                  PIC 9 VALUE 2.

       01  MISC-CURSOR-STAT            PIC X(2) VALUE SPACES.
           88  MISC-CURSOR-STAT-GOOD            VALUE "00".
           88  MISC-CURSOR-STAT-BAD             VALUE "  " THRU "//"
                                                      "01" THRU "~~".

       01  PROCESS-STAT                PIC X(08)    VALUE SPACES.
           88  PROCESS-STAT-GOOD                    VALUE "00".
           88  PROCESS-STAT-BAD                     VALUE "  " THRU "//"
                                                         "01" THRU "~~".
       01  RECORDS-PROCESSED           PIC 9(9) VALUE 0.
           88  NO-RECORDS-FOUND                     VALUE ZEROES.
       01  S35-RECORDS-PROCESSED       PIC 9(9)   VALUE 0.
       01  TOTAL-ERRORS-CTR            PIC 9(9)   VALUE 0.


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
           03  GROUP-FLAG              PIC X(01).
           03  ENT-GROUP               PIC X(04).
           03  BEG-BRANCH              PIC 9(4).
           03  END-BRANCH              PIC 9(4).

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
           "GLOBAL PARAMETER (GPFILE) COMPARE A15 TO S35".

       01  HEADER-LINE-03.
           03  FILLER                PIC X(4) VALUE "BRNO".
           03  FILLER                PIC X(15) VALUE SPACES.
           03  FILLER                PIC X(11) VALUE "DESCRIPTION".
           03  FILLER                PIC X(17) VALUE SPACES.
           03  FILLER                PIC X(3) VALUE "A15".
           03  FILLER                PIC X(19) VALUE SPACES.
           03  FILLER                PIC X(3) VALUE "S35".

      ******************************************************************
      *
      *    D E T A I L - L I N E - W O R K E R S
      *
      ******************************************************************
       01  DETAIL-LINE                        PIC X(80)   VALUE SPACES.

       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-BRNO                       PIC Z(3)9.
           03  FILLER                         PIC X(15).
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
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/PROGIDW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBUT/GPFILE_DEF.CPY".
       COPY "LIBUT/GPFILE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/GPFILE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE OGP-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-GP-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE OGP-FILE.
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
           MOVE "GPFILE" TO VDU-FORM-NAME.

           MOVE GPFILE-QUEUE-POS TO Q-POS.
           MOVE GPFILE-COPIES-POS TO C-POS.
           MOVE GPFILE-PRU-POS   TO P-POS.
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

           MOVE ZEROES                TO BEG-BRANCH.
           MOVE 9999                  TO END-BRANCH.
           MOVE "N"                   TO GROUP-FLAG.


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

      *    IF ( GROUP-FLAG = "Y" )
      *       MOVE "S  A000BEG-BRANCH."    TO VDU
      *       MOVE ENT-GROUP               TO VDUBUF
      *       PERFORM SCREENIO
      *       MOVE "S  A000END-BRANCH."    TO VDU
      *       MOVE SPACES                  TO VDUBUF
      *       PERFORM SCREENIO
      *    ELSE
      *       MOVE "SNNN040BEG-BRANCH."    TO VDU
      *       MOVE BEG-BRANCH              TO VDUNUM0
      *       PERFORM SCREENIO
      *       MOVE "SNNN040END-BRANCH."    TO VDU
      *       MOVE END-BRANCH              TO VDUNUM0
      *       PERFORM SCREENIO.

           MOVE "S  A090S-PATH."        TO VDU.     
           MOVE SOURCE-PATH             TO VDUBUF.
           PERFORM SCREENIO.

       DISPLAY-PARAMETERS-EXIT.
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

      * GLOBAL FILE
      *    GO TO EXECUTION-NEXT-GROUP.

       EXECUTION-MODULE-EXIT.
           EXIT.

      *********************************************************
       PROCESS-BRANCH-DRIVER SECTION.
      *********************************************************

           MOVE 9999                       TO PROGID-SLEEP-COUNTER.
           PERFORM SEND-PROGRESS-INDICATOR.

      D    STOP MESS.

      *  USE R1 GPFILE ONLY
           MOVE SOURCE-PATH  TO OGP-ALL-BASE.
           MOVE MACHINE-R1   TO OGP-ALL-MACHINE.
           MOVE OGP-ALL-PATH TO OGP-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE "MISSING A15 FILE" TO D01-TEXT 
              MOVE OGP-ALL-PATH       TO D01-A15
              MOVE SPACES             TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO PROCESS-BRANCH-DRIVER-END.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "PROCESSING A15 DATA" TO BTRACK-MESS.  
           PERFORM BTRACK-GLOBAL.

           MOVE "PROCESSING S35 DATA" TO BTRACK-MESS.
           PERFORM BTRACK-GLOBAL.

           PERFORM OPEN-GP-FILE.
           PERFORM OPEN-OGP-FILE.

           MOVE 1 TO OGP-KEY.
           PERFORM START-OGP-FILE.

       PROCESS-BRANCH-NEXT-GP.
           PERFORM READ-OGP-FILE-NEXT.
           IF IO-BAD 
              GO TO PROCESS-BRANCH-DRIVER-END.

           PERFORM SEND-PROGRESS-INDICATOR.

      * WE ONLY CONVERTED KEY 1
           IF OGP-KEY > 1
              GO TO PROCESS-BRANCH-NEXT-GP.

           ADD  01 TO RECORDS-PROCESSED.

           PERFORM COMPARE-FIELDS.

           GO TO PROCESS-BRANCH-NEXT-GP.

       PROCESS-BRANCH-DRIVER-END.

           PERFORM CLOSE-GP-FILE.

       PROCESS-BRANCH-DRIVER-EXIT.
           EXIT.

      ******************************************************************
       COMPARE-FIELDS SECTION.
      ******************************************************************

           MOVE OGP-KEY    TO GP-KEY.
           PERFORM READ-GP-FILE.
           IF IO-FG NOT = 0
              MOVE "MISSING S35 RECORD" TO D01-TEXT
              MOVE GP-KEY               TO D01-BRNO
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS
           ELSE
              ADD 1 TO S35-RECORDS-PROCESSED.

           MOVE OGP-BEG-FISCAL TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = GP-BEG-FISCAL-DATE
              MOVE "GP-BEG-FISCAL-DATE" TO D01-TEXT
              MOVE GP-KEY          TO D01-BRNO
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE GP-BEG-FISCAL-DATE TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
              MOVE OGP-END-FISCAL(SUB) TO DATE-MMDDYY
              PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
              PERFORM TEST-INVALID-DATE
              IF DATE-YYYYMMDD NOT = GP-END-FISCAL-DATE(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "GP-END-FISCAL-DATE",SUB INTO D01-TEXT
                 MOVE GP-KEY          TO D01-BRNO
                 MOVE DATE-YYYYMMDD  TO D01-A15
                 MOVE GP-END-FISCAL-DATE(SUB) TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.

           IF OGP-BANK-GL NOT NUMERIC
              MOVE 0 TO OGP-BANK-GL.
           IF OGP-BANK-GL NOT = GP-BANK-GL
              MOVE "GP-BANK-GL" TO D01-TEXT
              MOVE GP-KEY       TO D01-BRNO
              MOVE OGP-BANK-GL  TO D01-A15
              MOVE GP-BANK-GL   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OGP-BANK-CLEARING-GL NOT NUMERIC
              MOVE 0 TO OGP-BANK-CLEARING-GL.
           IF OGP-BANK-CLEARING-GL NOT = GP-BANK-CLEARING-GL
              MOVE "GP-BANK-CLEARING-GL" TO D01-TEXT
              MOVE GP-KEY       TO D01-BRNO
              MOVE OGP-BANK-CLEARING-GL  TO D01-A15
              MOVE GP-BANK-CLEARING-GL   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OGP-BANK-ROUTING-NO NOT NUMERIC
              MOVE 0 TO OGP-BANK-ROUTING-NO.
           IF OGP-BANK-ROUTING-NO NOT = GP-BANK-ROUTING-NO
              MOVE "GP-BANK-ROUTING-NO" TO D01-TEXT
              MOVE GP-KEY       TO D01-BRNO
              MOVE OGP-BANK-ROUTING-NO  TO D01-A15
              MOVE GP-BANK-ROUTING-NO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OGP-BANK-ACCTNO NOT NUMERIC
              MOVE 0 TO OGP-BANK-ACCTNO.
           IF OGP-BANK-ACCTNO NOT = GP-BANK-ACCTNO
              MOVE "GP-BANK-ACCTNO" TO D01-TEXT
              MOVE GP-KEY       TO D01-BRNO
              MOVE OGP-BANK-ACCTNO  TO D01-A15
              MOVE GP-BANK-ACCTNO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OGP-TR-RETAIN-MONTHS NOT NUMERIC
              MOVE 0 TO OGP-TR-RETAIN-MONTHS.
           IF OGP-TR-RETAIN-MONTHS NOT = GP-TR-RETAIN-MONTHS
              MOVE "GP-TR-RETAIN-MONTHS" TO D01-TEXT
              MOVE GP-KEY       TO D01-BRNO
              MOVE OGP-TR-RETAIN-MONTHS  TO D01-A15
              MOVE GP-TR-RETAIN-MONTHS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OGP-FED-TAX-ID NOT NUMERIC
              MOVE 0 TO OGP-FED-TAX-ID.
           IF OGP-FED-TAX-ID NOT = GP-FED-TAX-ID
              MOVE "GP-FED-TAX-ID" TO D01-TEXT
              MOVE GP-KEY       TO D01-BRNO
              MOVE OGP-FED-TAX-ID  TO D01-A15
              MOVE GP-FED-TAX-ID   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-CORP-NAME NOT = OGP-CORP-NAME
      *       CALL "C$JUSTIFY" USING OGP-CORP-NAME, "L"
              IF GP-CORP-NAME NOT = OGP-CORP-NAME
                 MOVE "GP-CORP-NAME" TO D01-TEXT
                 MOVE GP-KEY      TO D01-BRNO
                 MOVE OGP-CORP-NAME  TO D01-A15
                 MOVE GP-CORP-NAME   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-SHORT-NAME NOT = OGP-SHORT-NAME
      *       CALL "C$JUSTIFY" USING OGP-SHORT-NAME, "L"
              IF GP-SHORT-NAME NOT = OGP-SHORT-NAME
                 MOVE "GP-SHORT-NAME" TO D01-TEXT
                 MOVE GP-KEY      TO D01-BRNO
                 MOVE OGP-SHORT-NAME  TO D01-A15
                 MOVE GP-SHORT-NAME   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GP-BANK-NAME NOT = OGP-BANK-NAME
      *       CALL "C$JUSTIFY" USING OGP-BANK-NAME, "L"
              IF GP-BANK-NAME NOT = OGP-BANK-NAME
                 MOVE "GP-BANK-NAME" TO D01-TEXT
                 MOVE GP-KEY      TO D01-BRNO
                 MOVE OGP-BANK-NAME  TO D01-A15
                 MOVE GP-BANK-NAME   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OGP-BANK-DEPOSIT-GL NOT NUMERIC
              MOVE 0 TO OGP-BANK-DEPOSIT-GL.
           IF OGP-BANK-DEPOSIT-GL NOT = GP-BANK-DEPOSIT-GL
              MOVE "GP-BANK-DEPOSIT-GL" TO D01-TEXT
              MOVE GP-KEY       TO D01-BRNO
              MOVE OGP-BANK-DEPOSIT-GL  TO D01-A15
              MOVE GP-BANK-DEPOSIT-GL   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-AGE-BUCKET-CD NOT = OGP-AGE-BUCKET-CD
              MOVE "GP-AGE-BUCKET-CD" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-AGE-BUCKET-CD  TO D01-A15
              MOVE GP-AGE-BUCKET-CD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-THE-ONLY-SF-SRCD NOT = OGP-THE-ONLY-SF-SRCD
              MOVE "GP-THE-ONLY-SF-SRCD" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-THE-ONLY-SF-SRCD  TO D01-A15
              MOVE GP-THE-ONLY-SF-SRCD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-ALLOW-BATCH-CHECKS NOT = OGP-ALLOW-BATCH-CHECKS
              MOVE "GP-ALLOW-BATCH-CHECKS" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-ALLOW-BATCH-CHECKS  TO D01-A15
              MOVE GP-ALLOW-BATCH-CHECKS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-DLRVPRIN-3-FG NOT = OGP-DLRVPRIN-3-FG
              MOVE "GP-DLRVPRIN-3-FG" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-DLRVPRIN-3-FG  TO D01-A15
              MOVE GP-DLRVPRIN-3-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-DLRVCHGS-3-FG NOT = OGP-DLRVCHGS-3-FG
              MOVE "GP-DLRVCHGS-3-FG" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-DLRVCHGS-3-FG  TO D01-A15
              MOVE GP-DLRVCHGS-3-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-CASH-METHOD-ONLY NOT = OGP-CASH-METHOD-ONLY
              MOVE "GP-CASH-METHOD-ONLY" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-CASH-METHOD-ONLY  TO D01-A15
              MOVE GP-CASH-METHOD-ONLY   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-IB-ACCRUALS-DONE NOT = OGP-IB-ACCRUALS-DONE
              MOVE "GP-IB-ACCRUALS-DONE" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-IB-ACCRUALS-DONE  TO D01-A15
              MOVE GP-IB-ACCRUALS-DONE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OGP-EOD-RETAIN-MONTHS NOT NUMERIC
              MOVE 0 TO OGP-EOD-RETAIN-MONTHS.
           IF OGP-EOD-RETAIN-MONTHS NOT = GP-EOD-RETAIN-MONTHS
              MOVE "GP-EOD-RETAIN-MONTHS" TO D01-TEXT
              MOVE GP-KEY       TO D01-BRNO
              MOVE OGP-EOD-RETAIN-MONTHS  TO D01-A15
              MOVE GP-EOD-RETAIN-MONTHS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-NO-LC-IN-ARREARAGE-FG NOT =
                           OGP-NO-LC-IN-ARREARAGE-FG
              MOVE "GP-NO-LC-IN-ARREARAGE-FG" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-NO-LC-IN-ARREARAGE-FG  TO D01-A15
              MOVE GP-NO-LC-IN-ARREARAGE-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-DEFAULT-O3-INS-COMPANYX NOT =
                          OGP-DEFAULT-O3-INS-COMPANYX
              MOVE "GP-DEFAULT-O3-INS-COMPANYX" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-DEFAULT-O3-INS-COMPANYX  TO D01-A15
              MOVE GP-DEFAULT-O3-INS-COMPANYX   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-STOP-ON-PRIOR-FG NOT = OGP-STOP-ON-PRIOR-FG
              MOVE "GP-STOP-ON-PRIOR-FG" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-STOP-ON-PRIOR-FG  TO D01-A15
              MOVE GP-STOP-ON-PRIOR-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-APPLICATION-FG NOT = OGP-APPLICATION-FG
              MOVE "GP-APPLICATION-FG" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-APPLICATION-FG  TO D01-A15
              MOVE GP-APPLICATION-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OGP-DEFAULT-ACCRATE NOT NUMERIC
              MOVE 0 TO OGP-DEFAULT-ACCRATE.
           IF OGP-DEFAULT-ACCRATE NOT = GP-DEFAULT-ACCRATE
              MOVE "GP-DEFAULT-ACCRATE" TO D01-TEXT
              MOVE GP-KEY       TO D01-BRNO
              MOVE OGP-DEFAULT-ACCRATE  TO D01-A15-NUM
              MOVE GP-DEFAULT-ACCRATE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-DLR-DISP-TYPE NOT = OGP-DLR-DISP-TYPE
              MOVE "GP-DLR-DISP-TYPE" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-DLR-DISP-TYPE  TO D01-A15
              MOVE GP-DLR-DISP-TYPE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-ALLOW-RLOGINS NOT = OGP-ALLOW-RLOGINS
              MOVE "GP-ALLOW-RLOGINS" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-ALLOW-RLOGINS  TO D01-A15
              MOVE GP-ALLOW-RLOGINS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-AUTO-REPO-WINDOW NOT = OGP-AUTO-REPO-WINDOW
              MOVE "GP-AUTO-REPO-WINDOW" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-AUTO-REPO-WINDOW  TO D01-A15
              MOVE GP-AUTO-REPO-WINDOW   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-ALLOW-PI-WITH-SE-BAL NOT =
                       OGP-ALLOW-PI-WITH-SE-BAL
              MOVE "GP-ALLOW-PI-WITH-SE-BAL" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-ALLOW-PI-WITH-SE-BAL  TO D01-A15
              MOVE GP-ALLOW-PI-WITH-SE-BAL   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-ALLOW-MORE-COLLECT NOT = OGP-ALLOW-MORE-COLLECT
              MOVE "GP-ALLOW-MORE-COLLECT" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-ALLOW-MORE-COLLECT  TO D01-A15
              MOVE GP-ALLOW-MORE-COLLECT   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-BATCH-MORE-THAN-DAY NOT = OGP-BATCH-MORE-THAN-DAY
              MOVE "GP-BATCH-MORE-THAN-DAY" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-BATCH-MORE-THAN-DAY  TO D01-A15
              MOVE GP-BATCH-MORE-THAN-DAY   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-PRINT-GL-RECAP NOT = OGP-PRINT-GL-RECAP
              MOVE "GP-PRINT-GL-RECAP" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-PRINT-GL-RECAP  TO D01-A15
              MOVE GP-PRINT-GL-RECAP   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-LOAN-CONVERSION-UPD-FG NOT =
                            OGP-LOAN-CONVERSION-UPD-FG
              MOVE "GP-LOAN-CONVERSION-UPD-FG" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-LOAN-CONVERSION-UPD-FG  TO D01-A15
              MOVE GP-LOAN-CONVERSION-UPD-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-FORM-CHG-CD NOT = OGP-FORM-CHG-CD
              MOVE "GP-FORM-CHG-CD" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-FORM-CHG-CD  TO D01-A15
              MOVE GP-FORM-CHG-CD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-PROGRP-PRINT-REDEEM NOT = OGP-PROGRP-PRINT-REDEEM
              MOVE "GP-PROGRP-PRINT-REDEEM" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-PROGRP-PRINT-REDEEM  TO D01-A15
              MOVE GP-PROGRP-PRINT-REDEEM   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-BW-HOUSING-CODE NOT = OGP-BW-HOUSING-CODE
              MOVE "GP-BW-HOUSING-CODE" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-BW-HOUSING-CODE  TO D01-A15
              MOVE GP-BW-HOUSING-CODE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-BW-CREDIT-SCORE NOT = OGP-BW-CREDIT-SCORE
              MOVE "GP-BW-CREDIT-SCORE" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-BW-CREDIT-SCORE  TO D01-A15
              MOVE GP-BW-CREDIT-SCORE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-DISPLAY-BANK-BAL NOT = OGP-DISPLAY-BANK-BAL
              MOVE "GP-DISPLAY-BANK-BAL" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-DISPLAY-BANK-BAL  TO D01-A15
              MOVE GP-DISPLAY-BANK-BAL   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-PROGRP-DL-CONT NOT = OGP-PROGRP-DL-CONT
              MOVE "GP-PROGRP-DL-CONT" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-PROGRP-DL-CONT  TO D01-A15
              MOVE GP-PROGRP-DL-CONT   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-DLRVPRIN-2-FG NOT = OGP-DLRVPRIN-2-FG
              MOVE "GP-DLRVPRIN-2-FG" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-DLRVPRIN-2-FG  TO D01-A15
              MOVE GP-DLRVPRIN-2-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-DLRVPRIN-1-FG NOT = OGP-DLRVPRIN-1-FG
              MOVE "GP-DLRVPRIN-1-FG" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-DLRVPRIN-1-FG  TO D01-A15
              MOVE GP-DLRVPRIN-1-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-ADV-TO-1ST-NO-CORR NOT = OGP-ADV-TO-1ST-NO-CORR
              MOVE "GP-ADV-TO-1ST-NO-CORR" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-ADV-TO-1ST-NO-CORR  TO D01-A15
              MOVE GP-ADV-TO-1ST-NO-CORR   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-COMPANY-FLAG NOT = OGP-COMPANY-FLAG
      *       CALL "C$JUSTIFY" USING OGP-COMPANY-FLAG, "L"
              IF GP-COMPANY-FLAG NOT = OGP-COMPANY-FLAG
                 MOVE "GP-COMPANY-FLAG" TO D01-TEXT
                 MOVE GP-KEY      TO D01-BRNO
                 MOVE OGP-COMPANY-FLAG  TO D01-A15
                 MOVE GP-COMPANY-FLAG   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-INTERBR-AR-AP-FULLGL NOT = OGP-INTERBR-AR-AP-FULLGL
              MOVE "GP-INTERBR-AR-AP-FULLGL" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-INTERBR-AR-AP-FULLGL  TO D01-A15
              MOVE GP-INTERBR-AR-AP-FULLGL   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-SKIP-BI-UPDATE NOT = OGP-SKIP-BI-UPDATE
              MOVE "GP-SKIP-BI-UPDATE" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-SKIP-BI-UPDATE  TO D01-A15
              MOVE GP-SKIP-BI-UPDATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-ASK-HARD-COPY-MEMOS NOT = OGP-ASK-HARD-COPY-MEMOS
              MOVE "GP-ASK-HARD-COPY-MEMOS" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-ASK-HARD-COPY-MEMOS  TO D01-A15
              MOVE GP-ASK-HARD-COPY-MEMOS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-RLOGIN-LOCAL-PROFILE NOT = OGP-RLOGIN-LOCAL-PROFILE
              MOVE "GP-RLOGIN-LOCAL-PROFILE" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-RLOGIN-LOCAL-PROFILE  TO D01-A15
              MOVE GP-RLOGIN-LOCAL-PROFILE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-RLOGIN-REMOTE-PROFILE NOT = OGP-RLOGIN-REMOTE-PROFILE
              MOVE "GP-RLOGIN-REMOTE-PROFILE" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-RLOGIN-REMOTE-PROFILE  TO D01-A15
              MOVE GP-RLOGIN-REMOTE-PROFILE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-REPODATE-PAYDATE NOT = OGP-REPODATE-PAYDATE
              MOVE "GP-REPODATE-PAYDATE" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-REPODATE-PAYDATE  TO D01-A15
              MOVE GP-REPODATE-PAYDATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-POST-INTO-FUTURE NOT = OGP-POST-INTO-FUTURE
              MOVE "GP-POST-INTO-FUTURE" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-POST-INTO-FUTURE  TO D01-A15
              MOVE GP-POST-INTO-FUTURE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-RENEWAL-COLLATERAL NOT = OGP-RENEWAL-COLLATERAL
              MOVE "GP-RENEWAL-COLLATERAL" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-RENEWAL-COLLATERAL  TO D01-A15
              MOVE GP-RENEWAL-COLLATERAL   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
              IF OGP-PROGRP-LN-CLASS(SUB) NOT NUMERIC
                 MOVE 0 TO OGP-PROGRP-LN-CLASS(SUB)
              END-IF
              IF OGP-PROGRP-LN-CLASS(SUB) NOT =
                                        GP-PROGRP-LN-CLASS(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "GP-PROGRP-LN-CLASS",SUB INTO D01-TEXT
                 MOVE GP-KEY       TO D01-BRNO
                 MOVE OGP-PROGRP-LN-CLASS(SUB)  TO D01-A15
                 MOVE GP-PROGRP-LN-CLASS(SUB)   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
              IF OGP-PROGRP-SF-CLASS(SUB) NOT NUMERIC
                 MOVE 0 TO OGP-PROGRP-SF-CLASS(SUB)
              END-IF
              IF OGP-PROGRP-SF-CLASS(SUB) NOT =
                                        GP-PROGRP-SF-CLASS(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "GP-PROGRP-SF-CLASS",SUB INTO D01-TEXT
                 MOVE GP-KEY       TO D01-BRNO
                 MOVE OGP-PROGRP-SF-CLASS(SUB)  TO D01-A15
                 MOVE GP-PROGRP-SF-CLASS(SUB)   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.
           IF GP-RENEW-DISC-UNERN NOT = OGP-RENEW-DISC-UNERN
              MOVE "GP-RENEW-DISC-UNERN" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-RENEW-DISC-UNERN  TO D01-A15
              MOVE GP-RENEW-DISC-UNERN   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-NOTES-EOD-BATCH-ID NOT = OGP-NOTES-EOD-BATCH-ID
      *       CALL "C$JUSTIFY" USING OGP-NOTES-EOD-BATCH-ID, "L"
              IF GP-NOTES-EOD-BATCH-ID NOT = OGP-NOTES-EOD-BATCH-ID
                 MOVE "GP-NOTES-EOD-BATCH-ID" TO D01-TEXT
                 MOVE GP-KEY      TO D01-BRNO
                 MOVE OGP-NOTES-EOD-BATCH-ID  TO D01-A15
                 MOVE GP-NOTES-EOD-BATCH-ID   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OGP-NEXT-NOTE NOT NUMERIC
              MOVE 1 TO OGP-NEXT-NOTE.
           IF OGP-NEXT-NOTE NOT = GP-NEXT-NOTE
              MOVE "GP-NEXT-NOTE" TO D01-TEXT
              MOVE GP-KEY         TO D01-BRNO
              MOVE OGP-NEXT-NOTE  TO D01-A15
              MOVE GP-NEXT-NOTE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OGP-NEXT-BATCH-ID NOT NUMERIC
              MOVE 0 TO OGP-NEXT-BATCH-ID.
           IF OGP-NEXT-BATCH-ID NOT = GP-NEXT-BATCH-ID
              MOVE "GP-NEXT-BATCH-ID" TO D01-TEXT
              MOVE GP-KEY             TO D01-BRNO
              MOVE OGP-NEXT-BATCH-ID  TO D01-A15
              MOVE GP-NEXT-BATCH-ID   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GP-REFNO-VERIFY NOT = OGP-REFNO-VERIFY
              MOVE "GP-REFNO-VERIFY" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-REFNO-VERIFY  TO D01-A15
              MOVE GP-REFNO-VERIFY   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-ACTIONCD-VERIFY NOT = OGP-ACTIONCD-VERIFY
              MOVE "GP-ACTIONCD-VERIFY" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-ACTIONCD-VERIFY  TO D01-A15
              MOVE GP-ACTIONCD-VERIFY   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OGP-BP-NEXT-BATCH NOT NUMERIC
              MOVE 0 TO OGP-BP-NEXT-BATCH.
           IF OGP-BP-NEXT-BATCH NOT = GP-BP-NEXT-BATCH
              MOVE "GP-BP-NEXT-BATCH" TO D01-TEXT
              MOVE GP-KEY       TO D01-BRNO
              MOVE OGP-BP-NEXT-BATCH  TO D01-A15
              MOVE GP-BP-NEXT-BATCH   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GP-IB-ACCRUE-EARN-ON-REPO NOT =
                            OGP-IB-ACCRUE-EARN-ON-REPO
              MOVE "GP-IB-ACCRUE-EARN-ON-REPO" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-IB-ACCRUE-EARN-ON-REPO  TO D01-A15
              MOVE GP-IB-ACCRUE-EARN-ON-REPO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GP-DELINQ-WITH-O2 NOT = OGP-DELINQ-WITH-O2
              MOVE "GP-DELINQ-WITH-O2" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-DELINQ-WITH-O2  TO D01-A15
              MOVE GP-DELINQ-WITH-O2   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GP-ALLOW-REPO-ON-PL NOT = OGP-ALLOW-REPO-ON-PL
              MOVE "GP-ALLOW-REPO-ON-PL" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-ALLOW-REPO-ON-PL  TO D01-A15
              MOVE GP-ALLOW-REPO-ON-PL   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GP-BK-PRIOR-TO-POSTING NOT = OGP-BK-PRIOR-TO-POSTING
              MOVE "GP-BK-PRIOR-TO-POSTING" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-BK-PRIOR-TO-POSTING  TO D01-A15
              MOVE GP-BK-PRIOR-TO-POSTING   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OGP-BANK-GL NOT NUMERIC
              MOVE 0 TO OGP-BANK-GL.
           IF OGP-BANK-GL NOT = GP-BANK-GL
              MOVE "GP-BANK-GL" TO D01-TEXT
              MOVE GP-KEY       TO D01-BRNO
              MOVE OGP-BANK-GL  TO D01-A15
              MOVE GP-BANK-GL   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OGP-AMT-CASH-PROCEEDS NOT NUMERIC
              MOVE 0 TO OGP-AMT-CASH-PROCEEDS.
           IF OGP-AMT-CASH-PROCEEDS NOT = GP-AMT-CASH-PROCEEDS
              MOVE "GP-AMT-CASH-PROCEEDS" TO D01-TEXT
              MOVE GP-KEY       TO D01-BRNO
              MOVE OGP-AMT-CASH-PROCEEDS  TO D01-A15-NUM
              MOVE GP-AMT-CASH-PROCEEDS   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GP-INCL-OTH-IN-EARNED NOT = OGP-INCL-OTH-IN-EARNED
              MOVE "GP-INCL-OTH-IN-EARNED" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-INCL-OTH-IN-EARNED  TO D01-A15
              MOVE GP-INCL-OTH-IN-EARNED   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GP-VALID-BDATE NOT = OGP-VALID-BDATE
              MOVE "GP-VALID-BDATE" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-VALID-BDATE  TO D01-A15
              MOVE GP-VALID-BDATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GP-ARCHIVE-DIRECTORY NOT = OGP-ARCHIVE-DIRECTORY
      *       CALL "C$JUSTIFY" USING OGP-ARCHIVE-DIRECTORY, "L"
              IF GP-ARCHIVE-DIRECTORY NOT = OGP-ARCHIVE-DIRECTORY
                 MOVE "GP-ARCHIVE-DIRECTORY" TO D01-TEXT
                 MOVE GP-KEY      TO D01-BRNO
                 MOVE OGP-ARCHIVE-DIRECTORY  TO D01-A15
                 MOVE GP-ARCHIVE-DIRECTORY   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GP-DL-MEMO-TOGGLE NOT = OGP-DL-MEMO-TOGGLE
              MOVE "GP-DL-MEMO-TOGGLE" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-DL-MEMO-TOGGLE  TO D01-A15
              MOVE GP-DL-MEMO-TOGGLE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GP-RN-FOR10-LN NOT = OGP-RN-FOR10-LN
              MOVE "GP-RN-FOR10-LN" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-RN-FOR10-LN  TO D01-A15
              MOVE GP-RN-FOR10-LN   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GP-LI-APPLICATION-FG NOT = OGP-LI-APPLICATION-FG
              MOVE "GP-LI-APPLICATION-FG" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-LI-APPLICATION-FG  TO D01-A15
              MOVE GP-LI-APPLICATION-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-OT-INS-TERM NOT = OGP-OT-INS-TERM
              MOVE "GP-OT-INS-TERM" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-OT-INS-TERM  TO D01-A15
              MOVE GP-OT-INS-TERM   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-UNEARNED-STATS NOT = OGP-UNEARNED-STATS
              MOVE "GP-UNEARNED-STATS" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-UNEARNED-STATS  TO D01-A15
              MOVE GP-UNEARNED-STATS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GP-PDTH-GT-SCHED-PAY NOT = OGP-PDTH-GT-SCHED-PAY
              MOVE "GP-PDTH-GT-SCHED-PAY" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-PDTH-GT-SCHED-PAY  TO D01-A15
              MOVE GP-PDTH-GT-SCHED-PAY   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OGP-LB-NEXT-BATCH NOT NUMERIC
              MOVE 200 TO OGP-LB-NEXT-BATCH.
           IF OGP-LB-NEXT-BATCH NOT = GP-LB-NEXT-BATCH
              MOVE "GP-LB-NEXT-BATCH" TO D01-TEXT
              MOVE GP-KEY       TO D01-BRNO
              MOVE OGP-LB-NEXT-BATCH  TO D01-A15
              MOVE GP-LB-NEXT-BATCH   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GP-RECDEL-REVERSAL-FG NOT = OGP-RECDEL-REVERSAL-FG
              MOVE "GP-RECDEL-REVERSAL-FG" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-RECDEL-REVERSAL-FG  TO D01-A15
              MOVE GP-RECDEL-REVERSAL-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GP-REF-PHONE NOT = OGP-REF-PHONE
              MOVE "GP-REF-PHONE" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-REF-PHONE  TO D01-A15
              MOVE GP-REF-PHONE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-ALLOW-PL-ON-SX NOT = OGP-ALLOW-PL-ON-SX
              MOVE "GP-ALLOW-PL-ON-SX" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-ALLOW-PL-ON-SX  TO D01-A15
              MOVE GP-ALLOW-PL-ON-SX   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-USE-CONRATE NOT = OGP-USE-CONRATE
              MOVE "GP-USE-CONRATE" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-USE-CONRATE  TO D01-A15
              MOVE GP-USE-CONRATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-CPCORP-CURBAL NOT = OGP-CPCORP-CURBAL
              MOVE "GP-CPCORP-CURBAL" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-CPCORP-CURBAL  TO D01-A15
              MOVE GP-CPCORP-CURBAL   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-TIME-ZONE-Q NOT = OGP-TIME-ZONE-Q
              MOVE "GP-TIME-ZONE-Q" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-TIME-ZONE-Q  TO D01-A15
              MOVE GP-TIME-ZONE-Q   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OGP-TIME-ZONE-FACTOR NOT NUMERIC
              MOVE 0 TO OGP-TIME-ZONE-FACTOR.
           IF OGP-TIME-ZONE-FACTOR NOT = GP-TIME-ZONE-FACTOR
              MOVE "GP-TIME-ZONE-FACTOR" TO D01-TEXT
              MOVE GP-KEY       TO D01-BRNO
              MOVE OGP-TIME-ZONE-FACTOR  TO D01-A15-NUM
              MOVE GP-TIME-ZONE-FACTOR   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GP-TIME-ZONE-DLS NOT = OGP-TIME-ZONE-DLS
              MOVE "GP-TIME-ZONE-DLS" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-TIME-ZONE-DLS  TO D01-A15
              MOVE GP-TIME-ZONE-DLS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OGP-CALL-START-TIME NOT NUMERIC
              MOVE 0 TO OGP-CALL-START-TIME.
           IF OGP-CALL-START-TIME NOT = GP-CALL-START-TIME
              MOVE "GP-CALL-START-TIME" TO D01-TEXT
              MOVE GP-KEY       TO D01-BRNO
              MOVE OGP-CALL-START-TIME  TO D01-A15
              MOVE GP-CALL-START-TIME   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OGP-CALL-END-TIME NOT NUMERIC
              MOVE 0 TO OGP-CALL-END-TIME.
           IF OGP-CALL-END-TIME NOT = GP-CALL-END-TIME
              MOVE "GP-CALL-END-TIME" TO D01-TEXT
              MOVE GP-KEY       TO D01-BRNO
              MOVE OGP-CALL-END-TIME  TO D01-A15
              MOVE GP-CALL-END-TIME   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GP-RV-MEMO NOT = OGP-RV-MEMO
              MOVE "GP-RV-MEMO" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-RV-MEMO  TO D01-A15
              MOVE GP-RV-MEMO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-PENALTY-INT NOT = OGP-PENALTY-INT
              MOVE "GP-PENALTY-INT" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-PENALTY-INT  TO D01-A15
              MOVE GP-PENALTY-INT   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-ALLOW-SET-FOR-NONCASH NOT =
                           OGP-ALLOW-SET-FOR-NONCASH
              MOVE "GP-ALLOW-SET-FOR-NONCASH" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-ALLOW-SET-FOR-NONCASH  TO D01-A15
              MOVE GP-ALLOW-SET-FOR-NONCASH   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-IGNORE-LCHG-NXDUE NOT = OGP-IGNORE-LCHG-NXDUE
              MOVE "GP-IGNORE-LCHG-NXDUE" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-IGNORE-LCHG-NXDUE  TO D01-A15
              MOVE GP-IGNORE-LCHG-NXDUE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-ALLOW-ENTRY-TRADEIN NOT = OGP-ALLOW-ENTRY-TRADEIN
              MOVE "GP-ALLOW-ENTRY-TRADEIN" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-ALLOW-ENTRY-TRADEIN  TO D01-A15
              MOVE GP-ALLOW-ENTRY-TRADEIN   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OGP-TRUE-NEXT-BATCH NOT NUMERIC
              MOVE 500 TO OGP-TRUE-NEXT-BATCH.
           IF OGP-TRUE-NEXT-BATCH NOT = GP-TRUE-NEXT-BATCH
              MOVE "GP-TRUE-NEXT-BATCH" TO D01-TEXT
              MOVE GP-KEY       TO D01-BRNO
              MOVE OGP-TRUE-NEXT-BATCH  TO D01-A15
              MOVE GP-TRUE-NEXT-BATCH   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GP-REPO-CODE NOT = OGP-REPO-CODE
              MOVE "GP-REPO-CODE" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-REPO-CODE  TO D01-A15
              MOVE GP-REPO-CODE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-ALLOW-MORTGAGE-INT-Z NOT = OGP-ALLOW-MORTGAGE-INT-Z
              MOVE "GP-ALLOW-MORTGAGE-INT-Z" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-ALLOW-MORTGAGE-INT-Z  TO D01-A15
              MOVE GP-ALLOW-MORTGAGE-INT-Z   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OGP-PROM-AMT-MIN-PCT NOT NUMERIC
              MOVE 0 TO OGP-PROM-AMT-MIN-PCT.
           IF OGP-PROM-AMT-MIN-PCT NOT = GP-PROM-AMT-MIN-PCT
              MOVE "GP-PROM-AMT-MIN-PCT" TO D01-TEXT
              MOVE GP-KEY       TO D01-BRNO
              MOVE OGP-PROM-AMT-MIN-PCT  TO D01-A15
              MOVE GP-PROM-AMT-MIN-PCT   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-BW-PRIVACY-OPT-OUT NOT = OGP-BW-PRIVACY-OPT-OUT
              MOVE "GP-BW-PRIVACY-OPT-OUT" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-BW-PRIVACY-OPT-OUT  TO D01-A15
              MOVE GP-BW-PRIVACY-OPT-OUT   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-NO-GL-ON-O2-WHEN-PL NOT = OGP-NO-GL-ON-O2-WHEN-PL
              MOVE "GP-NO-GL-ON-O2-WHEN-PL" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-NO-GL-ON-O2-WHEN-PL  TO D01-A15
              MOVE GP-NO-GL-ON-O2-WHEN-PL   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-INTERIM-COUPONS NOT = OGP-INTERIM-COUPONS
              MOVE "GP-INTERIM-COUPONS" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-INTERIM-COUPONS  TO D01-A15
              MOVE GP-INTERIM-COUPONS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OGP-MGRAM-NEXT-BATCH NOT NUMERIC
              MOVE 600 TO OGP-MGRAM-NEXT-BATCH.
           IF OGP-MGRAM-NEXT-BATCH NOT = GP-MGRAM-NEXT-BATCH
              MOVE "GP-MGRAM-NEXT-BATCH" TO D01-TEXT
              MOVE GP-KEY       TO D01-BRNO
              MOVE OGP-MGRAM-NEXT-BATCH  TO D01-A15
              MOVE GP-MGRAM-NEXT-BATCH   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GP-RESCHEDULE-MEMO NOT = OGP-RESCHEDULE-MEMO
              MOVE "GP-RESCHEDULE-MEMO" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-RESCHEDULE-MEMO  TO D01-A15
              MOVE GP-RESCHEDULE-MEMO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-DONT-COLL-INT-ON-SS NOT = OGP-DONT-COLL-INT-ON-SS
              MOVE "GP-DONT-COLL-INT-ON-SS" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-DONT-COLL-INT-ON-SS  TO D01-A15
              MOVE GP-DONT-COLL-INT-ON-SS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-DL-ACH-FG NOT = OGP-DL-ACH-FG
              MOVE "GP-DL-ACH-FG" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-DL-ACH-FG  TO D01-A15
              MOVE GP-DL-ACH-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-GL-NEW-JE NOT = OGP-GL-NEW-JE
              MOVE "GP-GL-NEW-JE" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-GL-NEW-JE  TO D01-A15
              MOVE GP-GL-NEW-JE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-CLEAR-PROM-PDTH NOT = OGP-CLEAR-PROM-PDTH
              MOVE "GP-CLEAR-PROM-PDTH" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-CLEAR-PROM-PDTH  TO D01-A15
              MOVE GP-CLEAR-PROM-PDTH   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-CREATE-TP-FORMS NOT = OGP-CREATE-TP-FORMS
              MOVE "GP-CREATE-TP-FORMS" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-CREATE-TP-FORMS  TO D01-A15
              MOVE GP-CREATE-TP-FORMS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-CHECK-RECONCILIATION NOT =
                                 OGP-CHECK-RECONCILIATION
              MOVE "GP-CHECK-RECONCILIATION" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-CHECK-RECONCILIATION  TO D01-A15
              MOVE GP-CHECK-RECONCILIATION   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OGP-CORP-BRNO NOT NUMERIC
              MOVE 0 TO OGP-CORP-BRNO.
           IF OGP-CORP-BRNO NOT = GP-CORP-BRNO
              MOVE "GP-CORP-BRNO" TO D01-TEXT
              MOVE GP-KEY       TO D01-BRNO
              MOVE OGP-CORP-BRNO  TO D01-A15
              MOVE GP-CORP-BRNO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-PL-REPO-REASON NOT = OGP-PL-REPO-REASON
              MOVE "GP-PL-REPO-REASON" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-PL-REPO-REASON  TO D01-A15
              MOVE GP-PL-REPO-REASON   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-ALLOW-MIP-INSURANCE NOT = OGP-ALLOW-MIP-INSURANCE
              MOVE "GP-ALLOW-MIP-INSURANCE" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-ALLOW-MIP-INSURANCE  TO D01-A15
              MOVE GP-ALLOW-MIP-INSURANCE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-ENTER-LPO-FG NOT = OGP-ENTER-LPO-FG
              MOVE "GP-ENTER-LPO-FG" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-ENTER-LPO-FG  TO D01-A15
              MOVE GP-ENTER-LPO-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-DLCOMM-CMSI-FG NOT = OGP-DLCOMM-CMSI-FG
              MOVE "GP-DLCOMM-CMSI-FG" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-DLCOMM-CMSI-FG  TO D01-A15
              MOVE GP-DLCOMM-CMSI-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OGP-REFCD-NEXT-BATCH NOT NUMERIC
              MOVE 0 TO OGP-REFCD-NEXT-BATCH.
           IF OGP-REFCD-NEXT-BATCH NOT = GP-REFCD-NEXT-BATCH
              MOVE "GP-REFCD-NEXT-BATCH" TO D01-TEXT
              MOVE GP-KEY       TO D01-BRNO
              MOVE OGP-REFCD-NEXT-BATCH  TO D01-A15
              MOVE GP-REFCD-NEXT-BATCH   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OGP-EARLIEST-CLOSE-TIME NOT NUMERIC
              MOVE 0 TO OGP-EARLIEST-CLOSE-TIME.
           IF OGP-EARLIEST-CLOSE-TIME NOT = GP-EARLIEST-CLOSE-TIME
              MOVE "GP-EARLIEST-CLOSE-TIME" TO D01-TEXT
              MOVE GP-KEY       TO D01-BRNO
              MOVE OGP-EARLIEST-CLOSE-TIME  TO D01-A15
              MOVE GP-EARLIEST-CLOSE-TIME   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-GROUP-APPROVAL-FG NOT = OGP-GROUP-APPROVAL-FG
              MOVE "GP-GROUP-APPROVAL-FG" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-GROUP-APPROVAL-FG  TO D01-A15
              MOVE GP-GROUP-APPROVAL-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-APPROVAL-MEMO NOT = OGP-APPROVAL-MEMO
              MOVE "GP-APPROVAL-MEMO" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-APPROVAL-MEMO  TO D01-A15
              MOVE GP-APPROVAL-MEMO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-FORCE-INPUT-PP-COVERAGE NOT =
                                OGP-FORCE-INPUT-PP-COVERAGE
              MOVE "GP-FORCE-INPUT-PP-COVERAGE" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-FORCE-INPUT-PP-COVERAGE  TO D01-A15
              MOVE GP-FORCE-INPUT-PP-COVERAGE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-NO-SC-USE-DM NOT = OGP-NO-SC-USE-DM
              MOVE "GP-NO-SC-USE-DM" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-NO-SC-USE-DM  TO D01-A15
              MOVE GP-NO-SC-USE-DM   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-DEFAULT-O3-INSOURS NOT = OGP-DEFAULT-O3-INSOURS
              MOVE "GP-DEFAULT-O3-INSOURS" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-DEFAULT-O3-INSOURS  TO D01-A15
              MOVE GP-DEFAULT-O3-INSOURS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-USE-BW-BAD-FLAGS NOT = OGP-USE-BW-BAD-FLAGS
              MOVE "GP-USE-BW-BAD-FLAGS" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-USE-BW-BAD-FLAGS  TO D01-A15
              MOVE GP-USE-BW-BAD-FLAGS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-NON-FILE-FEECODE NOT = OGP-NON-FILE-FEECODE
              MOVE "GP-NON-FILE-FEECODE" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-NON-FILE-FEECODE  TO D01-A15
              MOVE GP-NON-FILE-FEECODE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OGP-CHBORR-PAST-DUE-DAYS NOT NUMERIC
              MOVE 0 TO OGP-CHBORR-PAST-DUE-DAYS.
           IF OGP-CHBORR-PAST-DUE-DAYS NOT = GP-CHBORR-PAST-DUE-DAYS
              MOVE "GP-CHBORR-PAST-DUE-DAYS" TO D01-TEXT
              MOVE GP-KEY       TO D01-BRNO
              MOVE OGP-CHBORR-PAST-DUE-DAYS  TO D01-A15
              MOVE GP-CHBORR-PAST-DUE-DAYS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GP-CLOSED-FISCAL-TABLE NOT = OGP-CLOSED-FISCAL-TABLE
              MOVE "GP-CLOSED-FISCAL-TABLE" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-CLOSED-FISCAL-TABLE  TO D01-A15
              MOVE GP-CLOSED-FISCAL-TABLE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-BACKDATE-MEMO NOT = OGP-BACKDATE-MEMO
              MOVE "GP-BACKDATE-MEMO" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-BACKDATE-MEMO  TO D01-A15
              MOVE GP-BACKDATE-MEMO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OGP-OTREF-NEXT-BATCH NOT NUMERIC
              MOVE 0 TO OGP-OTREF-NEXT-BATCH.
           IF OGP-OTREF-NEXT-BATCH NOT = GP-OTREF-NEXT-BATCH
              MOVE "GP-OTREF-NEXT-BATCH" TO D01-TEXT
              MOVE GP-KEY       TO D01-BRNO
              MOVE OGP-OTREF-NEXT-BATCH  TO D01-A15
              MOVE GP-OTREF-NEXT-BATCH   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-NEW-REPO-UNEARNED-ACCTS NOT =
                                       OGP-NEW-REPO-UNEARNED-ACCTS
              MOVE "GP-NEW-REPO-UNEARNED-ACCTS" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-NEW-REPO-UNEARNED-ACCTS  TO D01-A15
              MOVE GP-NEW-REPO-UNEARNED-ACCTS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-BK-FLAG-CONFIDENTIAL NOT = OGP-BK-FLAG-CONFIDENTIAL
              MOVE "GP-BK-FLAG-CONFIDENTIAL" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-BK-FLAG-CONFIDENTIAL  TO D01-A15
              MOVE GP-BK-FLAG-CONFIDENTIAL   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-AMORTIZE-ACQUIST-FEE NOT = OGP-AMORTIZE-ACQUIST-FEE
              MOVE "GP-AMORTIZE-ACQUIST-FEE" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-AMORTIZE-ACQUIST-FEE  TO D01-A15
              MOVE GP-AMORTIZE-ACQUIST-FEE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-DENY-FORM-NO-ADDRESS NOT = OGP-DENY-FORM-NO-ADDRESS
              MOVE "GP-DENY-FORM-NO-ADDRESS" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-DENY-FORM-NO-ADDRESS  TO D01-A15
              MOVE GP-DENY-FORM-NO-ADDRESS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-HIDE-FIRST-5-SSNO NOT = OGP-HIDE-FIRST-5-SSNO
              MOVE "GP-HIDE-FIRST-5-SSNO" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-HIDE-FIRST-5-SSNO  TO D01-A15
              MOVE GP-HIDE-FIRST-5-SSNO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-DEFERMENT-APPROVAL NOT = OGP-DEFERMENT-APPROVAL
              MOVE "GP-DEFERMENT-APPROVAL" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-DEFERMENT-APPROVAL  TO D01-A15
              MOVE GP-DEFERMENT-APPROVAL   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-INCLUDE-O2-IN-BAL NOT = OGP-INCLUDE-O2-IN-BAL
              MOVE "GP-INCLUDE-O2-IN-BAL" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-INCLUDE-O2-IN-BAL  TO D01-A15
              MOVE GP-INCLUDE-O2-IN-BAL   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-LOAN-ACH-PROCESSING NOT = OGP-LOAN-ACH-PROCESSING
              MOVE "GP-LOAN-ACH-PROCESSING" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-LOAN-ACH-PROCESSING  TO D01-A15
              MOVE GP-LOAN-ACH-PROCESSING   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-PAYEE-PROCESSING-FG NOT = OGP-PAYEE-PROCESSING-FG
              MOVE "GP-PAYEE-PROCESSING-FG" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-PAYEE-PROCESSING-FG  TO D01-A15
              MOVE GP-PAYEE-PROCESSING-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-AUTO-BANKRUPT-WINDOW NOT = OGP-AUTO-BANKRUPT-WINDOW
              MOVE "GP-AUTO-BANKRUPT-WINDOW" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-AUTO-BANKRUPT-WINDOW  TO D01-A15
              MOVE GP-AUTO-BANKRUPT-WINDOW   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OGP-BW-SCORES-QUARTER-SUB NOT NUMERIC
              MOVE 0 TO OGP-BW-SCORES-QUARTER-SUB.
           IF OGP-BW-SCORES-QUARTER-SUB NOT =
                                  GP-BW-SCORES-QUARTER-SUB
              MOVE "GP-BW-SCORES-QUARTER-SUB" TO D01-TEXT
              MOVE GP-KEY       TO D01-BRNO
              MOVE OGP-BW-SCORES-QUARTER-SUB  TO D01-A15
              MOVE GP-BW-SCORES-QUARTER-SUB   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GP-POST-XX-BF-PAYMENTS NOT = OGP-POST-XX-BF-PAYMENTS
              MOVE "GP-POST-XX-BF-PAYMENTS" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-POST-XX-BF-PAYMENTS  TO D01-A15
              MOVE GP-POST-XX-BF-PAYMENTS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-VALIDATE-ENDORSER-AGE NOT = OGP-VALIDATE-ENDORSER-AGE
              MOVE "GP-VALIDATE-ENDORSER-AGE" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-VALIDATE-ENDORSER-AGE  TO D01-A15
              MOVE GP-VALIDATE-ENDORSER-AGE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-ALLOW-MANUAL-CHECKS NOT = OGP-ALLOW-MANUAL-CHECKS
              MOVE "GP-ALLOW-MANUAL-CHECKS" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-ALLOW-MANUAL-CHECKS  TO D01-A15
              MOVE GP-ALLOW-MANUAL-CHECKS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-EXPANDED-FOLLOW-UP NOT = OGP-EXPANDED-FOLLOW-UP
              MOVE "GP-EXPANDED-FOLLOW-UP" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-EXPANDED-FOLLOW-UP  TO D01-A15
              MOVE GP-EXPANDED-FOLLOW-UP   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OGP-NEXT-EMPLOYEE-NO NOT NUMERIC
              MOVE 0 TO OGP-NEXT-EMPLOYEE-NO.
           IF OGP-NEXT-EMPLOYEE-NO NOT = GP-NEXT-EMPLOYEE-NO
              MOVE "GP-NEXT-EMPLOYEE-NO" TO D01-TEXT
              MOVE GP-KEY       TO D01-BRNO
              MOVE OGP-NEXT-EMPLOYEE-NO  TO D01-A15
              MOVE GP-NEXT-EMPLOYEE-NO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-USE-RESULT-WORKED-FG NOT = OGP-USE-RESULT-WORKED-FG
              MOVE "GP-USE-RESULT-WORKED-FG" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-USE-RESULT-WORKED-FG  TO D01-A15
              MOVE GP-USE-RESULT-WORKED-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OGP-MAX-PROMISE-DAYS NOT NUMERIC
              MOVE 0 TO OGP-MAX-PROMISE-DAYS.
           IF OGP-MAX-PROMISE-DAYS NOT = GP-MAX-PROMISE-DAYS
              MOVE "GP-MAX-PROMISE-DAYS" TO D01-TEXT
              MOVE GP-KEY       TO D01-BRNO
              MOVE OGP-MAX-PROMISE-DAYS  TO D01-A15
              MOVE GP-MAX-PROMISE-DAYS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-PURGE-TO-ARCHIVE-FG NOT = OGP-PURGE-TO-ARCHIVE-FG
              MOVE "GP-PURGE-TO-ARCHIVE-FG" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-PURGE-TO-ARCHIVE-FG  TO D01-A15
              MOVE GP-PURGE-TO-ARCHIVE-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-EDIT-BORROWER-NAME-FG NOT = OGP-EDIT-BORROWER-NAME-FG
              MOVE "GP-EDIT-BORROWER-NAME-FG" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-EDIT-BORROWER-NAME-FG  TO D01-A15
              MOVE GP-EDIT-BORROWER-NAME-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-RECAP-CLASS-TOTALS NOT = OGP-RECAP-CLASS-TOTALS
              MOVE "GP-RECAP-CLASS-TOTALS" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-RECAP-CLASS-TOTALS  TO D01-A15
              MOVE GP-RECAP-CLASS-TOTALS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-BRING-AUTO-INS-ON-RENEWAL NOT =
                                 OGP-BRING-AUTO-INS-ON-RENEWAL
              MOVE "GP-BRING-AUTO-INS-ON-RENEWAL" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-BRING-AUTO-INS-ON-RENEWAL  TO D01-A15
              MOVE GP-BRING-AUTO-INS-ON-RENEWAL   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-ALLOW-MF-ON-IB NOT = OGP-ALLOW-MF-ON-IB
              MOVE "GP-ALLOW-MF-ON-IB" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-ALLOW-MF-ON-IB  TO D01-A15
              MOVE GP-ALLOW-MF-ON-IB   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-USE-MIN-FORM-SENT-DAYS NOT = OGP-USE-MIN-FORM-SENT-DAYS
              MOVE "GP-USE-MIN-FORM-SENT-DAYS" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-USE-MIN-FORM-SENT-DAYS  TO D01-A15
              MOVE GP-USE-MIN-FORM-SENT-DAYS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-DISPLAY-BAD-ADR-FLAG NOT = OGP-DISPLAY-BAD-ADR-FLAG
              MOVE "GP-DISPLAY-BAD-ADR-FLAG" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-DISPLAY-BAD-ADR-FLAG  TO D01-A15
              MOVE GP-DISPLAY-BAD-ADR-FLAG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OGP-DAYS-IDENT-NEW-PAPER NOT NUMERIC
              MOVE 0 TO OGP-DAYS-IDENT-NEW-PAPER.
           IF OGP-DAYS-IDENT-NEW-PAPER NOT = GP-DAYS-IDENT-NEW-PAPER
              MOVE "GP-DAYS-IDENT-NEW-PAPER" TO D01-TEXT
              MOVE GP-KEY       TO D01-BRNO
              MOVE OGP-DAYS-IDENT-NEW-PAPER  TO D01-A15
              MOVE GP-DAYS-IDENT-NEW-PAPER   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-ESCROW-FG NOT = OGP-ESCROW-FG
              MOVE "GP-ESCROW-FG" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-ESCROW-FG  TO D01-A15
              MOVE GP-ESCROW-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-SECURITY-FG NOT = OGP-SECURITY-FG
              MOVE "GP-SECURITY-FG" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-SECURITY-FG  TO D01-A15
              MOVE GP-SECURITY-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-GPS-INFORMATION NOT = OGP-GPS-INFORMATION
              MOVE "GP-GPS-INFORMATION" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-GPS-INFORMATION  TO D01-A15
              MOVE GP-GPS-INFORMATION   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-CUST-INFO-FG NOT = OGP-CUST-INFO-FG
              MOVE "GP-CUST-INFO-FG" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-CUST-INFO-FG  TO D01-A15
              MOVE GP-CUST-INFO-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-USE-ADDRESS-BW-SCAN NOT = OGP-USE-ADDRESS-BW-SCAN
              MOVE "GP-USE-ADDRESS-BW-SCAN" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-USE-ADDRESS-BW-SCAN  TO D01-A15
              MOVE GP-USE-ADDRESS-BW-SCAN   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-EXPANDED-LEGAL-WINDOW NOT = OGP-EXPANDED-LEGAL-WINDOW
              MOVE "GP-EXPANDED-LEGAL-WINDOW" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-EXPANDED-LEGAL-WINDOW  TO D01-A15
              MOVE GP-EXPANDED-LEGAL-WINDOW   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-USE-SCRA NOT = OGP-USE-SCRA
              MOVE "GP-USE-SCRA" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-USE-SCRA  TO D01-A15
              MOVE GP-USE-SCRA   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-USE-ACCRUAL-EARN-FILE NOT = OGP-USE-ACCRUAL-EARN-FILE
              MOVE "GP-USE-ACCRUAL-EARN-FILE" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-USE-ACCRUAL-EARN-FILE  TO D01-A15
              MOVE GP-USE-ACCRUAL-EARN-FILE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-MESSAGE-ATTR NOT = OGP-MESSAGE-ATTR
              MOVE "GP-MESSAGE-ATTR" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-MESSAGE-ATTR  TO D01-A15
              MOVE GP-MESSAGE-ATTR   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-JUDGMENT-WINDOW NOT = OGP-JUDGMENT-WINDOW
              MOVE "GP-JUDGMENT-WINDOW" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-JUDGMENT-WINDOW  TO D01-A15
              MOVE GP-JUDGMENT-WINDOW   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-DUE-29-30-31 NOT = OGP-DUE-29-30-31
              MOVE "GP-DUE-29-30-31" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-DUE-29-30-31  TO D01-A15
              MOVE GP-DUE-29-30-31   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-DEFAULT-SKIP-TRW NOT = OGP-DEFAULT-SKIP-TRW
              MOVE "GP-DEFAULT-SKIP-TRW" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-DEFAULT-SKIP-TRW  TO D01-A15
              MOVE GP-DEFAULT-SKIP-TRW   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OGP-EARLIEST-OPEN-TIME NOT NUMERIC
              MOVE 0 TO OGP-EARLIEST-OPEN-TIME.
           IF OGP-EARLIEST-OPEN-TIME NOT = GP-EARLIEST-OPEN-TIME
              MOVE "GP-EARLIEST-OPEN-TIME" TO D01-TEXT
              MOVE GP-KEY       TO D01-BRNO
              MOVE OGP-EARLIEST-OPEN-TIME  TO D01-A15
              MOVE GP-EARLIEST-OPEN-TIME   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-TRANSFER-CONPROFILE NOT = OGP-TRANSFER-CONPROFILE
              MOVE "GP-TRANSFER-CONPROFILE" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-TRANSFER-CONPROFILE  TO D01-A15
              MOVE GP-TRANSFER-CONPROFILE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OGP-BATCH-REFCD-DUP-DAYS NOT NUMERIC
              MOVE 0 TO OGP-BATCH-REFCD-DUP-DAYS.
           IF OGP-BATCH-REFCD-DUP-DAYS NOT = GP-BATCH-REFCD-DUP-DAYS
              MOVE "GP-BATCH-REFCD-DUP-DAYS" TO D01-TEXT
              MOVE GP-KEY       TO D01-BRNO
              MOVE OGP-BATCH-REFCD-DUP-DAYS  TO D01-A15
              MOVE GP-BATCH-REFCD-DUP-DAYS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF GP-CREATE-PAY-XTRP-XNO NOT = OGP-CREATE-PAY-XTRP-XNO
              MOVE "GP-CREATE-PAY-XTRP-XNO" TO D01-TEXT
              MOVE GP-KEY      TO D01-BRNO
              MOVE OGP-CREATE-PAY-XTRP-XNO  TO D01-A15
              MOVE GP-CREATE-PAY-XTRP-XNO   TO D01-S35
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

           MOVE "A15 RECORDS PROCESSED         :"     TO R01-PROMPT.
           MOVE RECORDS-PROCESSED                     TO R01-NUMERIC.
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
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/PROGID.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO GPFILE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GPFILE-SYSDATE.
           DISPLAY GPFILE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE GPFILE-FORM-FIELDS TO WR-BUF.
           MOVE GPFILE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE GPFILE-HELP--FILE TO HELP-LINK-FILE.
           MOVE GPFILE-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/GPFILE_EVA.CPY".
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
       COPY "LIBGB/GBGPGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBGPRN.CPY".

       OPEN-OGP-FILE.
           PERFORM OPEN-IT.
           MOVE OGP-PATH TO E-FILE.
           OPEN INPUT OGP-FILE.
           IF IO-FG = 8
              GO TO OPEN-OGP-FILE.
           IF IO-FG = 7
              CLOSE OGP-FILE
              GO TO OPEN-OGP-FILE.
      *READ-OGP-FILE.
      *    PERFORM READ-IT.
      *    MOVE OGP-PATH TO E-FILE.
      *    MOVE OGP-KEY TO E-KEYX.
      *    READ OGP-FILE.
      *    IF IO-FG = 8
      *       GO TO READ-OGP-FILE.
       START-OGP-FILE.
           PERFORM START-IT.
           MOVE OGP-PATH TO E-FILE.
           MOVE OGP-KEY TO E-KEYX.
           START OGP-FILE KEY NOT < OGP-KEY.
       READ-OGP-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OGP-PATH TO E-FILE.
           MOVE OGP-KEY TO E-KEYX.
           READ OGP-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OGP-FILE-NEXT.
       CLOSE-OGP-FILE.
           PERFORM CLOSE-IT.
           CLOSE OGP-FILE.

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
              DESTROY GPFILE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
