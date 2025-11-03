       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IRFILE.
       DATE-WRITTEN.  12-08-2023.
      ******************************************************************
      *
      * DESCRIPTION   : COMPARE S35 IRFILE WITH A15 IRFILE
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

      * COPYMEMBER: LIBLP/LPFSIR
           SELECT OIR-FILE ASSIGN TO OIR-PATH
                  ORGANIZATION RELATIVE
                  ACCESS MODE DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RELATIVE KEY OIR-KEY
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
       FD  OIR-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A15SRC/LP01IR.CPY" REPLACING LEADING "IR" BY "OIR".

       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY                  PIC 9(3).
           03  WK-DATA                 PIC X(761).


      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/IRFILE S35 04/12/24-13:40:17 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01IR.CPY".
       COPY "LIBLP/LP01IR_SQL.CPY".
       COPY "LIBLP/LPWSIR.CPY".

       01  OIR-PATH                    PIC X(35) VALUE SPACES.
       01  OIR-ALL-PATH.
           03  OIR-ALL-BASE            PIC X(9).
           03  FILLER                  PIC X     VALUE "/".
           03  OIR-ALL-MACHINE         PIC XX.
           03  FILLER                  PIC X(10) VALUE
               "/LP/IRFILE".

       01  OIR-KEY     PIC 9(3).

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

       01  WK-EOF-SW                   PIC X      VALUE "N".
       01  OIR-EOF-SW                  PIC X      VALUE "N".

       01  PROCESS-STAT                PIC X(08)  VALUE SPACES.
           88  PROCESS-STAT-GOOD                  VALUE "00".
           88  PROCESS-STAT-BAD                   VALUE "  " THRU "//"
                                                        "01" THRU "~~".
       01  A15-RECORDS-PROCESSED       PIC 9(9)     VALUE 0.
           88  NO-RECORDS-FOUND                     VALUE ZEROES.
       01  S35-RECORDS-PROCESSED       PIC 9(9)   VALUE 0.
       01  TOTAL-ERRORS-CTR            PIC 9(9)   VALUE 0.


       01  STARTING-TIME               PIC X(08)    VALUE SPACES.
       01  SUB                         PIC 9(04)    VALUE ZEROES.
       01  S1                          PIC 9(04)    VALUE ZEROES.
       01  S2                          PIC 9(04)    VALUE ZEROES.
       01  S3                          PIC 9(04)    VALUE ZEROES.

       01  SYSDATE-YYYYMMDD            PIC 9(8)     VALUE ZEROES.

       01  LEGEND                      PIC X(79)    VALUE SPACES.
       01  LEGEND-LIST                 PIC X(07)    VALUE "LEGEND.".

      *-----------------------------------------------------------------
       01  LEGEND-ACCEPT-YN.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT             ".
           03  FILLER PIC X(30) VALUE "                              ".
           03  FILLER PIC X(19) VALUE "     ACCEPT (Y/N): ".

       01  SOURCE-PATH-LIST            PIC X(07)     VALUE "S-PATH.".

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  SOURCE-PATH             PIC X(9).

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
           03  H02-TITLE               PIC X(50) VALUE 
           "INSURANCE RATE (IRFILE) COMPARE A15 TO S35".

       01  HEADER-LINE-03.
           03  FILLER                PIC X(7)  VALUE SPACES.
           03  FILLER                PIC X(3)  VALUE "KEY".
           03  FILLER                PIC X(3)  VALUE SPACES.
           03  FILLER                PIC X(28) VALUE "DESCRIPTION".
           03  FILLER                PIC X(23) VALUE "A15".
           03  FILLER                PIC X(3)  VALUE "S35".

      ******************************************************************
      *
      *    D E T A I L - L I N E - W O R K E R S
      *
      ******************************************************************
       01  DETAIL-LINE                        PIC X(100)   VALUE SPACES.

       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(7).
           03  D01-KEY                        PIC 9(3).
           03  FILLER                         PIC X(3).
           03  D01-TEXT                       PIC X(25).
           03  FILLER                         PIC X(3).
           03  D01-A15                        PIC X(20).
           03  FILLER REDEFINES D01-A15.
               05  D01-A15-NUM                PIC Z(7)9.9(3).
               05  FILLER                     PIC X(8).
           03  FILLER                         PIC X(3).
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
       COPY "LIBUT/IRFILE_DEF.CPY".
       COPY "LIBUT/IRFILE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/IRFILE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE OIR-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-IR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE OIR-FILE WK-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.

           PERFORM SQL-CONNECT.

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
           MOVE "IRFILE" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE IRFILE-QUEUE-POS TO Q-POS.
           MOVE IRFILE-COPIES-POS TO C-POS.
           MOVE IRFILE-PRU-POS   TO P-POS.
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

           MOVE "S  A090S-PATH."        TO VDU.     
           MOVE SOURCE-PATH             TO VDUBUF.
           PERFORM SCREENIO.

       DISPLAY-PARAMETERS-EXIT.
           EXIT.

      *******************************************
       EXECUTION-MODULE SECTION.
      *******************************************

           MOVE 9999    TO PROGID-SLEEP-COUNTER.
      *     PERFORM SEND-PROGRESS-INDICATOR.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE MACHINE-R1                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

      D    STOP MESS.

           MOVE "PROCESSING A15 DATA" TO BTRACK-MESS.
           PERFORM BTRACK-GLOBAL.

           MOVE "PROCESSING S35 DATA" TO BTRACK-MESS.
           PERFORM BTRACK-GLOBAL.

      *** MUST LOAD R1 AND R2 IRFILE INTO WKFILE ***
      *** DO NOT REWRITE ***
      * FIRST DELETE/EMPTY TEMP FILE IF EXIST
           PERFORM OPEN-WK1-FILE-OUTPUT.
           PERFORM CLOSE-WK1-FILE.
           PERFORM OPEN-WK1-FILE.

           MOVE SOURCE-PATH  TO OIR-ALL-BASE.
           MOVE MACHINE-R1   TO OIR-ALL-MACHINE.
           MOVE OIR-ALL-PATH TO OIR-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              PERFORM CLOSE-WK1-FILE
              MOVE "MISSING R1 IRFILE" TO D01-TEXT
              MOVE OIR-ALL-PATH        TO D01-A15
              PERFORM WRITE-DETAIL-LINE-ERROR 
              GO TO EXECUTION-MODULE-END.

           PERFORM OPEN-OIR1-FILE.
           PERFORM LOAD-WORK-FILE.
           PERFORM CLOSE-OIR1-FILE. 

      *** LOAD R2 DO NOT UPDATE IF KEY ALREADY IN WKFILE ***
           MOVE SOURCE-PATH  TO OIR-ALL-BASE.
           MOVE MACHINE-R2   TO OIR-ALL-MACHINE.
           MOVE OIR-ALL-PATH TO OIR-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              PERFORM CLOSE-WK1-FILE
              MOVE "MISSING R2 IRFILE" TO D01-TEXT
              MOVE OIR-ALL-PATH        TO D01-A15
              PERFORM WRITE-DETAIL-LINE-ERROR 
              GO TO EXECUTION-MODULE-END.

           PERFORM OPEN-OIR1-FILE.
           PERFORM LOAD-WORK-FILE.
           PERFORM CLOSE-OIR1-FILE.

           PERFORM CLOSE-WK1-FILE.
           PERFORM OPEN-WK1-FILE.

           PERFORM OPEN-IR-FILE.

       PROCESS-BRANCH-NEXT-IR.
           PERFORM READ-WK1-FILE-NEXT.
           IF IO-BAD 
              GO TO EXECUTION-MODULE-END.

           MOVE WK-DATA TO OIR-REC.

      *     PERFORM SEND-PROGRESS-INDICATOR.

           PERFORM COMPARE-FIELDS.

           GO TO PROCESS-BRANCH-NEXT-IR.


       EXECUTION-MODULE-END.

           PERFORM CLOSE-WK1-FILE.
           PERFORM CLOSE-IR-FILE.

       EXECUTION-MODULE-EXIT.
           EXIT.

      *----------------------------------------------------------------------------
       LOAD-WORK-FILE SECTION.
           PERFORM READ-OIR1-FILE-NEXT.
           IF IO-BAD 
              GO TO LOAD-WORK-FILE-EXIT.

           PERFORM SEND-PROGRESS-INDICATOR.

           IF OIR-ALL-MACHINE = MACHINE-R1
              IF OIR-KEY NOT = 20
                IF OIR-KEY < 4 OR OIR-KEY > 16
                   GO TO LOAD-WORK-FILE.

           IF OIR-ALL-MACHINE = MACHINE-R2
              IF OIR-KEY = 1 OR 2 OR 3 OR 14
                 NEXT SENTENCE
              ELSE
                   GO TO LOAD-WORK-FILE.

           INITIALIZE WK-REC.

           MOVE OIR-KEY    TO WK-KEY.
           MOVE OIR-REC    TO WK-DATA.

           PERFORM WRITE-WK1-FILE.

           IF IO-GOOD 
              ADD 1 TO A15-RECORDS-PROCESSED.

           GO TO LOAD-WORK-FILE.

       LOAD-WORK-FILE-EXIT.
           EXIT. 

      ******************************************************************
       COMPARE-FIELDS SECTION.
      ******************************************************************

           MOVE WK-KEY TO IR-KEY.
           PERFORM READ-IR-FILE.
           IF IO-FG NOT = 0
              MOVE "MISSING S35 RECORD" TO D01-TEXT
              MOVE IR-KEY               TO D01-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS
           ELSE
              ADD 1 TO S35-RECORDS-PROCESSED.

           IF IR-PER-FG NOT = OIR-PER-FG
              MOVE "IR-PER-FG" TO D01-TEXT
              MOVE IR-KEY      TO D01-KEY
              MOVE OIR-PER-FG  TO D01-A15
              MOVE IR-PER-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 120
              IF OIR-INSRATE(SUB) NOT NUMERIC
                 MOVE 0 TO OIR-INSRATE(SUB)
              END-IF
              IF OIR-INSRATE(SUB) NOT = IR-INSRATE(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "IR-INSRATE",SUB INTO D01-TEXT
                 MOVE IR-KEY             TO D01-KEY
                 MOVE OIR-INSRATE(SUB)   TO D01-A15-NUM
                 MOVE IR-INSRATE(SUB)    TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.

           IF IR-DESC NOT = OIR-DESC
              MOVE "IR-DESC" TO D01-TEXT
              MOVE IR-KEY    TO D01-KEY
              MOVE OIR-DESC  TO D01-A15
              MOVE IR-DESC   TO D01-S35
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
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/PROGID.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO IRFILE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IRFILE-SYSDATE.
           DISPLAY IRFILE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IRFILE-FORM-FIELDS TO WR-BUF.
           MOVE IRFILE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE IRFILE-HELP--FILE TO HELP-LINK-FILE.
           MOVE IRFILE-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/IRFILE_EVA.CPY".
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
       COPY "LIBLP/LPIRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
        COPY "LIBLP/LPIRRN.CPY".
       OPEN-OIR1-FILE.
           PERFORM OPEN-IT.
           MOVE OIR-PATH TO E-FILE.
           OPEN INPUT OIR-FILE.
           IF IO-FG = 8
              GO TO OPEN-OIR1-FILE.
           IF IO-FG = 7
              CLOSE OIR-FILE
              GO TO OPEN-OIR1-FILE.
       START-OIR1-FILE.
           PERFORM START-IT.
           MOVE OIR-PATH TO E-FILE.
           MOVE OIR-KEY TO E-KEYX.
           START OIR-FILE KEY NOT < OIR-KEY.
       READ-OIR1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OIR-PATH TO E-FILE.
           MOVE OIR-KEY TO E-KEYX.
           READ OIR-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OIR1-FILE-NEXT.
       CLOSE-OIR1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OIR-FILE.

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
                MOVE 2 TO IO-STATUS IO-FG
           END-WRITE.
           IF IO-FG = 8
             GO TO WRITE-WK1-FILE.
           UNLOCK WK-FILE.

       CLOSE-WK1-FILE.
           PERFORM CLOSE-IT.
           MOVE 0 TO IO-FG IO-STATUS.
           CLOSE WK-FILE.

      *----------------------------------------------------------------------
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

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE EXIT-PATH               TO FORM-NAM
              MOVE FORM-PATH               TO FORM-PATHNAME.

           MOVE WK-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.


       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY IRFILE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
