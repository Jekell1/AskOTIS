       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CHFILE.
       DATE-WRITTEN.  01-26-2024.
       AUTHOR. MARK BOOMGAARD.
      ******************************************************************
      *
      * DESCRIPTION   : COMPARE S35 CHFILE WITH A15 CHFILE
      *            
      *                 TO BE USED AFTER AN UPDATE
      *                 WILL COMPARE EACH FIELD AND REPORT ANY 
      *                 DIFFERENCES, AS LONG AS FIELD EXIST IN BOTH FILES
      *
      *  2 FILE MATCH PROCESSING.
      *
      *  01-07-25  RMZ  ADDED 'BTRACK' - PROCESS TRACKING TO PROGRAM.
      *
      ******************************************************************
      * 2024-1112 MB  ADDED DEFAULT BEGIN DATE = SYSDATE - 5 YEARS
      * 2025-0103 RZ  CHANGED DATE CODE FOR BATCH PROCESSING. 
      * 2025-0106 MB  ADD CODE TO EO-EXEC-AUDIT TO FORMAT SQL DATES.
      * 2025-0225 RZ  ADDED DEFAULT BEGIN DATE = SYSDATE - 59 MONTHS
      ********************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT OCH-FILE ASSIGN TO OCH-PATH
                  ORGANIZATION SEQUENTIAL
                  ACCESS SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
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
        FD  OCH-FILE
           LABEL RECORDS ARE STANDARD.
      *-----------------------------------------------------------------
       COPY "LIBUP/A15SRC/LP01CH.CPY" REPLACING LEADING "CH" BY "OCH".
      *-----------------------------------------------------------------

       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
              05  WK-KEY-BRNO          PIC 9(4).
              05  WK-KEY-DATE.
                07  WK-KEY-DATE-CN     PIC 9(2).
                07  WK-KEY-DATE-YY     PIC 9(2).
                07  WK-KEY-DATE-MM     PIC 9(2).
                07  WK-KEY-DATE-DD     PIC 9(2).
              05  WK-KEY-CODE          PIC X(3).
              05  WK-KEY-USERID        PIC X(10).
              05  WK-KEY-TIME          PIC 9(6).
              05  WK-KEY-SEQ           PIC 9(3).
           03  WK-DATA                 PIC X(OCH-SIZE).

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/CHFILE S35 01/06/25-08:33:44 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01CH.CPY".
       COPY "LIBLP/LP01CH_SQL.CPY".
       COPY "LIBLP/LPWSCH.CPY".

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

       01  OCH-PATH                  PIC X(35) VALUE SPACES.
       01  OCH-ALL-PATH.
           03  OCH-ALL-BASE          PIC X(9).
           03  FILLER                PIC X     VALUE "/".
           03  OCH-ALL-MACHINE       PIC XX.
           03  FILLER                PIC X    VALUE "/".
           03  OCH-ALL-DATA          PIC X(6).
           03  FILLER                PIC X(6) VALUE "/ED/CH".
           03  OCH-DATE-PATH         PIC 9(4).
           03  OCH-DATE-P REDEFINES OCH-DATE-PATH.
               05  OCH-ALL-MONTH     PIC 99.
               05  OCH-ALL-YEAR      PIC 99.

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01 BEG-BY-DATE-CCYYMMDD.
           03  BEG-BY-DATE-CCYYMM.
             05 BEG-BY-DATE-CC       PIC 99.
             05 BEG-BY-DATE-YYMM.
                07 BEG-BY-DATE-YY    PIC 99.
                07 BEG-BY-DATE-MM    PIC 99.
           03  BEG-BY-DATE-DD        PIC 99        VALUE 01.

       01 END-BY-DATE-CCYYMMDD.
          03  END-BY-DATE-CCYYMM.
              05 END-BY-DATE-CC      PIC 99.
              05 END-BY-DATE-YYMM.
                07 END-BY-DATE-YY    PIC 99.
                07 END-BY-DATE-MM    PIC 99.
           03  END-BY-DATE-DD        PIC 99.

       01  PROC-YYMM                   PIC 9(4).
       01  PROC-YYMM-RDF  REDEFINES PROC-YYMM.
           03 PROC-YYMM-YY             PIC 99.
           03 PROC-YYMM-MM             PIC 99.

       01  CH-EOF-SW                   PIC X      VALUE "N".
       01  OCH-EOF-SW                  PIC X      VALUE "N".
       01  WK-EOF-SW                   PIC X      VALUE "N".
       01  PREV-BRANCH-ERR-SW          PIC X      VALUE "N".
       01  TOTAL-ERRORS-CTR            PIC 9(9)   VALUE 0.
       01  SUB1                        PIC 99     VALUE 0.
       01  A15-FILE-FOUND-SW               PIC X      VALUE "N".
       01  HOLD-NUM-CHECK              PIC 9(10)  VALUE 0.
       01  WK-PATH                     PIC X(40)  VALUE SPACE.
       01  WK-RECS-CTR                 PIC 9(9)   VALUE 0.
       01  HEX-CMD-CRLF-CR             PIC X(01)  VALUE X"0A".
       01  HEX-CMD-CRLF-LF             PIC X(01)  VALUE X"0D".
       01  PRINTING-RANGES-SW          PIC X      VALUE "N".
       01  CH1-CONN-OPEN-SW            PIC X      VALUE "N".

       01  WORK-USERID.
           03  FILLER                  PIC X.
           03  WORK-USERID-BR          PIC X(4).
           03  WORK-USERID-BR-NUM      REDEFINES WORK-USERID-BR
                                       PIC 9(4).
           03 FILLER                   PIC X(5).

       01 TEST-ORG-DATE.
           03 TEST-ORG-DATE-CHAR1      PIC X.
           03 TEST-ORG-DATE-REST       PIC X(7).

       01  WORK-TIME.
           03  WORK-HR                 PIC 9(02).
           03  WORK-MIN                PIC 9(02).
           03  WORK-SEC                PIC 9(02).

       01  CONV-TYPE-CHAR              PIC X.
       01  CONV-TYPE-NUM    REDEFINES CONV-TYPE-CHAR
                                       PIC 9.

       01  TEST-ACCTNO                 PIC 9(8).
       01  FILLER REDEFINES TEST-ACCTNO.
             03  TEST-NUMBER           PIC 9(6).
             03  TEST-SEQ              PIC 9(2).    

       01  CH-USE-KEY.
           03 CH-USE-KEY-BRNO          PIC 9(4).
           03 CH-USE-KEY-DATE          PIC 9(8). 
           03 CH-USE-KEY-CODE          PIC X(3).
           03 CH-USE-KEY-USERID        PIC X(10).
           03 CH-USE-KEY-TIME          PIC 9(6).
           03 CH-USE-KEY-SEQ           PIC 9(3).

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
       01  A15-RECORDS-PROCESSED       PIC 9(9)     VALUE 0.
       01  SQL-RECORDS-PROCESSED       PIC 9(9)     VALUE 0.

       01  STARTING-TIME               PIC X(08)     VALUE SPACES.
       01  SUB                         PIC 9(04)     VALUE ZEROES.

       01  SYSDATE-YYYYMMDD.
           03  SYSDATE-CN              PIC 99. 
           03  SYSDATE-YY              PIC 99. 
           03  SYSDATE-MM              PIC 99. 
           03  SYSDATE-DD              PIC 99. 

       01  WS-59MONTH-YYMM.
           03  WS-59MONTH-YY           PIC 99.
           03  WS-59MONTH-MM           PIC 99.

       01  WS-BEGIN-YYMM.
           03  WS-BEGIN-YY             PIC 99.
           03  WS-BEGIN-MM             PIC 99.

       01  MISC-DATE-MMYY.
           03  MISC-DATE-MM            PIC 99. 
           03  MISC-DATE-YY            PIC 99. 
       01  MISC-DATE-MMYY-NUM          REDEFINES MISC-DATE-MMYY
                                       PIC 9(4).

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
       01  LEGEND-BEG-MMYY.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F7-EXIT, ENT".
           03  FILLER PIC X(30) VALUE "ER BEGINNING DATE (MMYY)      ".
           03  FILLER PIC X(19) VALUE "                   ".

       01  LEGEND-END-MMYY.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER ENDIN".
           03  FILLER PIC X(30) VALUE "G DATE (MMYY)                  ".
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
           03  BEG-MMYY                PIC 9(4).
           03  END-MMYY                PIC 9(4).
           03  GROUP-FLAG              PIC X(01).
           03  ENT-GROUP               PIC X(04).

       01  VERSION-CONTROL             PIC X(01)     VALUE "A".

      *-----------------------------------------------------------------
       01  SV-BEG-BRANCH               PIC 9(04)     VALUE 9999.
       01  SV-END-BRANCH               PIC 9(04)     VALUE 9999.
       01  SV-GROUP-FLAG               PIC X(01)     VALUE SPACES.
       01  SV-ENT-GROUP                PIC X(04)     VALUE SPACES.
       01  SV-BEG-MMYY                 PIC 9(04)     VALUE 9999.
       01  SV-END-MMYY                 PIC 9(04)     VALUE 9999.

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
           03  FILLER                  PIC X(6)  VALUE "PAGE: ".
           03  H01-PAGE                PIC Z(04).

      *-----------------------------------------------------------------
       01  HEADER-LINE-02.
           03  FILLER                  PIC X(10) VALUE "USER ID:".
           03  H02-USERID              PIC X(10) VALUE SPACES.
           03  FILLER                  PIC X(5)  VALUE SPACES.
           03  H02-TITLE               PIC X(65) VALUE 
           "DAILY CHANGES (CHFILE) COMPARE A15 TO S35".

       01  HEADER-LINE-03.
           03  FILLER                PIC X(4)  VALUE "BRNO".
           03  FILLER                PIC X(2)  VALUE SPACES.
           03  FILLER                PIC X(8)  VALUE "  DATE  ".
           03  FILLER                PIC X(3)  VALUE SPACES.
           03  FILLER                PIC X(2)  VALUE "CD".
           03  FILLER                PIC X(3)  VALUE SPACES.
           03  FILLER                PIC X(10) VALUE "  USERID  ".
           03  FILLER                PIC X(4)  VALUE SPACES.
           03  FILLER                PIC X(6)  VALUE " TIME ".
           03  FILLER                PIC X(3)  VALUE SPACES.
           03  FILLER                PIC X(3)  VALUE "SEQ".
           03  FILLER                PIC X(2)  VALUE SPACES.
           03  FILLER                PIC X(11) VALUE "DESCRIPTION".
           03  FILLER                PIC X(11) VALUE SPACES.
           03  FILLER                PIC X(27)  VALUE "A15".
           03  FILLER                PIC X(3)  VALUE "S35".

      ******************************************************************
      *
      *    D E T A I L - L I N E - W O R K E R S
      *
      ******************************************************************
       01  DETAIL-LINE                        PIC X(120)   VALUE SPACES.

       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-BRNO                       PIC Z(3)9.
           03  FILLER                         PIC X(2).
           03  D01-DATE                       PIC 9(8).
           03  FILLER                         PIC X(3).
           03  D01-CODE                       PIC X(2).
           03  FILLER                         PIC X(3).
           03  D01-USERID                     PIC X(10).
           03  FILLER                         PIC X(4).
           03  D01-TIME                       PIC 9(6).
           03  FILLER                         PIC X(3).
           03  D01-SEQ                        PIC 999.
           03  FILLER                         PIC X(2).
           03  D01-TEXT                       PIC X(20).
           03  FILLER                         PIC X(2).
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
       COPY "LIBUT/CHFILE_DEF.CPY".
       COPY "LIBUT/CHFILE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/CHFILE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE OCH-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-CH-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           PERFORM CLOSE-CH1-CONNECTION.
           CLOSE OCH-FILE WK-FILE.
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
           MOVE "CHFILE" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO SYSDATE-YYYYMMDD.


      * OBTAIN EARLIEST BEGIN DATE OF 4 YEARS / 11 MONTHS BEFORE TODAY *
           IF SYSDATE-MM            = 12
              MOVE 1               TO WS-59MONTH-MM
              COMPUTE WS-59MONTH-YY = SYSDATE-YY - 4
           ELSE
              MOVE SYSDATE-MM      TO WS-59MONTH-MM
              ADD 1                TO WS-59MONTH-MM
              COMPUTE WS-59MONTH-YY = SYSDATE-YY - 5. 

           MOVE CHFILE-QUEUE-POS   TO Q-POS.
           MOVE CHFILE-COPIES-POS  TO C-POS.
           MOVE CHFILE-PRU-POS     TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           PERFORM GET-TIME.
           MOVE TIME-EDIT                  TO STARTING-TIME.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS......" TO LEGEND
              PERFORM SEND-LEGEND.

           IF ( EXT-EOBUF-GROUP-FG = "Y" )
              MOVE BEG-BRANCH              TO SV-BEG-BRANCH
              MOVE END-BRANCH              TO SV-END-BRANCH
              MOVE GROUP-FLAG              TO SV-GROUP-FLAG
              MOVE BEG-MMYY                TO SV-BEG-MMYY
              MOVE END-MMYY                TO SV-END-MMYY
              MOVE ENT-GROUP               TO SV-ENT-GROUP
              MOVE EXT-EOBUF-BRNO          TO BEG-BRANCH END-BRANCH
              MOVE "N"                     TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH  TO PRU-FILE
           ELSE
           IF ( BEG-BRANCH = ZEROES ) AND
              ( END-BRANCH = ZEROES )
              MOVE BEG-BRANCH              TO SV-BEG-BRANCH
              MOVE END-BRANCH              TO SV-END-BRANCH
              MOVE BEG-MMYY                TO SV-BEG-MMYY
              MOVE END-MMYY                TO SV-END-MMYY
              MOVE EXT-CONAME-USER-LOC     TO BEG-BRANCH
                                              END-BRANCH.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-PR-FILE.

           PERFORM START-MISC-CSR.
           PERFORM READ-MISC-CSR-NEXT.
           PERFORM CLOSE-MISC-CSR.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WK-PATH.

      *     PERFORM ENABLE-INTERRUPT.

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
              GO TO ENTER-END-BRANCH.

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

      *-----------------------------------------------------------------
       ENTER-BEG-MMYY.

           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO ENTER-PARAMETERS-END.

           MOVE LEGEND-BEG-MMYY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040BEG-MMYY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F3-KEY ) GO TO ENTER-ALL-DATES.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-MMYY.

           MOVE VDUNUM0 TO DATE-MMYY.

           IF ( DATE-MMYY-MM < 01 ) OR
              ( DATE-MMYY-MM > 12 )
              MOVE "ERROR: ENTRY NOT VALID (MMYY)" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-MMYY.

           MOVE DATE-MMYY-MM       TO WS-BEGIN-MM.
           MOVE DATE-MMYY-YY       TO WS-BEGIN-YY.   

           IF WS-BEGIN-YYMM         < WS-59MONTH-YYMM
              MOVE WS-59MONTH-MM   TO DATE-MMYY-MM
              MOVE WS-59MONTH-YY   TO DATE-MMYY-YY.  

           MOVE DATE-MMYY          TO BEG-MMYY VDUNUM0.
           MOVE "SNNN040BEG-MMYY." TO VDU.
           MOVE BEG-MMYY           TO VDUNUM0.
           PERFORM SCREENIO.

           PERFORM CONVERT-MMYY-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.
           MOVE DATE-YYYYMM      TO BEG-BY-DATE-CCYYMM.

      *-----------------------------------------------------------------
       ENTER-END-MMYY.

           MOVE LEGEND-END-MMYY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040END-MMYY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-MMYY.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-MMYY.

           MOVE VDUNUM0 TO DATE-MMYY.

           IF ( DATE-MMYY-MM < 01 ) OR
              ( DATE-MMYY-MM > 12 )
              MOVE "ERROR: ENTRY NOT VALID (MMYY)" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-END-MMYY.

           PERFORM CONVERT-MMYY-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.
           MOVE DATE-YYYYMM TO END-BY-DATE-CCYYMM.

           IF BEG-BY-DATE-CCYYMM > END-BY-DATE-CCYYMM
               MOVE "INVALID DATE RANGE !" TO MESS
               PERFORM SEND-MESS
               GO TO ENTER-BEG-MMYY.

           MOVE VDUNUM0 TO END-MMYY.

           GO TO ENTER-PARAMETERS-END.

      *-----------------------------------------------------------------
       ENTER-ALL-DATES.

           MOVE WS-59MONTH-MM      TO MISC-DATE-MM
           MOVE WS-59MONTH-YY      TO MISC-DATE-YY. 

           MOVE "SNNN040BEG-MMYY." TO VDU.
           MOVE MISC-DATE-MMYY-NUM TO BEG-MMYY VDUNUM0 DATE-MMYY.
           PERFORM SCREENIO.

           PERFORM CONVERT-MMYY-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.
           MOVE DATE-YYYYMM TO BEG-BY-DATE-CCYYMM.

           MOVE "SNNN040END-MMYY." TO VDU.
           MOVE 1249               TO END-MMYY VDUNUM0 DATE-MMYY.
           PERFORM SCREENIO.

           PERFORM CONVERT-MMYY-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.
           MOVE DATE-YYYYMM TO END-BY-DATE-CCYYMM.

      *-----------------------------------------------------------------
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

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.

           COMPUTE MISC-DATE-YY = DATE-MMDDYY-YY - 5.
           MOVE DATE-MMDDYY-MM TO MISC-DATE-MM.

           MOVE MISC-DATE-MMYY-NUM TO BEG-MMYY DATE-MMYY.
           PERFORM CONVERT-MMYY-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.
           MOVE DATE-YYYYMM TO BEG-BY-DATE-CCYYMM.

           MOVE 1249 TO END-MMYY DATE-MMYY.
           PERFORM CONVERT-MMYY-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.
           MOVE DATE-YYYYMM TO END-BY-DATE-CCYYMM.


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

           MOVE "SNNN040BEG-MMYY."        TO VDU.
           MOVE BEG-MMYY                  TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040END-MMYY."       TO VDU.
           MOVE END-MMYY                 TO VDUNUM0.
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

      * PROCESS WILL ALWAYS COPY A15 BRANCH 9000+ RECORDS FOR BANKRUPT
      *  (CODE = "B") RECORDS THAT ARE IN A15 AS BRANCH 9000+ BUT IN
      *  IN S35 UNDER REGULAR BRANCH NUMBER.

           MOVE 9999 TO PROGID-SLEEP-COUNTER.
           PERFORM SEND-PROGRESS-INDICATOR.

      D    STOP MESS.

           IF PREV-BRANCH-ERR-SW = "Y"
              PERFORM WRITE-DETAIL-LINE.

           MOVE "N" TO PREV-BRANCH-ERR-SW.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO CH-PATH-OVERRIDE.

           MOVE "N" TO CH-EOF-SW OCH-EOF-SW 
                       WK-EOF-SW.
           MOVE 0   TO WK-RECS-CTR.

           MOVE BR-NO   TO QCH-WBEG-BRNO.

           MOVE BEG-BY-DATE-CCYYMMDD TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QCH-WBEG-DATE.

           MOVE 31 TO END-BY-DATE-DD.
           IF END-BY-DATE-MM = 4 OR 6 OR 9 OR 11
              MOVE 30 TO END-BY-DATE-DD
           ELSE
              IF END-BY-DATE-MM = 2
                 MOVE 29 TO END-BY-DATE-DD
                 MOVE END-BY-DATE-CCYYMMDD TO NDTE-DATE
      * JUST IN CASE IF LEAP YEAR NEW-DATE-CALCULATION WILL CHANGE 20230229 TO 20230228 !!!
                 PERFORM NEW-DATE-CALCULATION     
                 MOVE NDTE-DATE TO END-BY-DATE-CCYYMMDD.

           MOVE END-BY-DATE-CCYYMMDD TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QCH-WEND-DATE.

           PERFORM OPEN-CH-FILE.
           PERFORM START-CH-FILE.  *> SSP  CHFILE
           PERFORM M-READ-CH.

           MOVE SOURCE-PATH   TO OCH-ALL-BASE.
           MOVE MACHINE-R1    TO OCH-ALL-MACHINE.
           MOVE BR-DATA-PATH  TO OCH-ALL-DATA.

           MOVE BEG-BY-DATE-YYMM TO PROC-YYMM.

      * MUST LOAD ALL OCH FILES INTO WK FILE FOR REQUESTED BRANCH  
           PERFORM OPEN-WK1-FILE-OUTPUT.

           PERFORM VARYING PROC-YYMM FROM PROC-YYMM BY 1
             UNTIL PROC-YYMM > END-BY-DATE-YYMM
              MOVE MACHINE-R1    TO OCH-ALL-MACHINE
              MOVE PROC-YYMM-YY  TO OCH-ALL-YEAR
              MOVE PROC-YYMM-MM  TO OCH-ALL-MONTH
              MOVE OCH-ALL-PATH  TO OCH-PATH ACCESS-BUF

              MOVE "Y" TO A15-FILE-FOUND-SW
              PERFORM ACCESS-CALL
              IF ( STAT-BAD )
                 MOVE MACHINE-R2   TO OCH-ALL-MACHINE
                 MOVE OCH-ALL-PATH TO OCH-PATH ACCESS-BUF
                 PERFORM ACCESS-CALL
                 IF ( STAT-BAD )
                    MOVE "N" TO A15-FILE-FOUND-SW
                 END-IF
              END-IF
 
              IF A15-FILE-FOUND-SW = "Y"
                 PERFORM OPEN-OCH1-FILE
                 MOVE "N" TO OCH-EOF-SW
                 PERFORM LOAD-WORK-FILE
                 PERFORM CLOSE-OCH1-FILE
              END-IF
 
              IF PROC-YYMM-MM = 12
                 ADD  1 TO PROC-YYMM-YY
                 MOVE 0 TO PROC-YYMM-MM
              END-IF
           END-PERFORM.
           PERFORM CLOSE-WK1-FILE.

           PERFORM OPEN-WK1-FILE.
           PERFORM M-READ-WK1.

           PERFORM PROCESS-BRANCH-NEXT-CH
              UNTIL CH-EOF-SW = "Y" AND WK-EOF-SW = "Y".

           PERFORM CLOSE-CH-FILE.
           PERFORM CLOSE-WK1-FILE.

           GO TO PROCESS-BRANCH-DRIVER-END.

       PROCESS-BRANCH-NEXT-CH.
           PERFORM SEND-PROGRESS-INDICATOR.
      *     PERFORM TRAP-INTERRUPT.

           PERFORM COMPARE-FIELDS.

           IF WK-KEY < CH-USE-KEY
              PERFORM M-READ-WK1
           ELSE
              IF WK-KEY > CH-USE-KEY
                 PERFORM M-READ-CH
              ELSE
                 PERFORM M-READ-WK1
                 PERFORM M-READ-CH.

       PROCESS-BRANCH-DRIVER-END.
           EXIT. 

       LOAD-WORK-FILE SECTION.

           PERFORM M-READ-OCH1.
           IF OCH-EOF-SW = "Y"
              GO TO LOAD-WORK-FILE-END.

           MOVE BR-NO        TO WK-KEY-BRNO.
           IF PROC-YYMM-YY > 80
              MOVE 19        TO WK-KEY-DATE-CN
           ELSE
              MOVE 20        TO WK-KEY-DATE-CN.
           MOVE PROC-YYMM-YY TO WK-KEY-DATE-YY.
           MOVE PROC-YYMM-MM TO WK-KEY-DATE-MM.
           MOVE OCH-DAY      TO WK-KEY-DATE-DD.
           STRING OCH-CODE,"  " INTO WK-KEY-CODE.
           MOVE OCH-USERID   TO WK-KEY-USERID.
           MOVE OCH-HR       TO WORK-HR.
           MOVE OCH-MIN      TO WORK-MIN.
           MOVE OCH-SEC      TO WORK-SEC.
           MOVE WORK-TIME    TO WK-KEY-TIME.
           MOVE 1            TO WK-KEY-SEQ.
           MOVE OCH-REC      TO WK-DATA.

           PERFORM WRITE-WK1-FILE.
           PERFORM UNTIL NOT IO-BAD   *> MAY BE DUPLICATE SEQ
              ADD 1 TO WK-KEY-SEQ
              PERFORM WRITE-WK1-FILE
           END-PERFORM. 

           ADD 1 TO WK-RECS-CTR.
           GO TO LOAD-WORK-FILE.

       LOAD-WORK-FILE-END.
           EXIT. 

       M-READ-CH SECTION.
           PERFORM READ-CH-FILE-NEXT.   *> SSP  CHFILE
           IF IO-BAD
              MOVE ALL '9' TO CH-USE-KEY
              MOVE "Y"     TO CH-EOF-SW
           ELSE
              MOVE CH-BRNO   TO CH-USE-KEY-BRNO 
              MOVE CH-DATE   TO CH-USE-KEY-DATE
              MOVE CH-CODE   TO CH-USE-KEY-CODE 
              MOVE CH-USERID TO CH-USE-KEY-USERID
              MOVE CH-TIME   TO CH-USE-KEY-TIME
              MOVE CH-SEQ    TO CH-USE-KEY-SEQ
              ADD 1 TO SQL-RECORDS-PROCESSED.

       EXIT-M-READ-CH.
           EXIT.

       M-READ-OCH1 SECTION.
           PERFORM READ-OCH1-FILE-NEXT.
           IF IO-BAD
              MOVE "Y" TO OCH-EOF-SW
           ELSE
              ADD 1    TO A15-RECORDS-PROCESSED.
 
       EXIT-M-READ-OCH1.
           EXIT.

       M-READ-WK1 SECTION.
           PERFORM READ-WK1-FILE-NEXT.
           IF IO-BAD 
              MOVE ALL '9'  TO WK-KEY
              MOVE "Y"      TO WK-EOF-SW
           ELSE
              MOVE WK-DATA  TO OCH-REC.

       EXIT-M-READ-WK1.
           EXIT.

      ******************************************************************
       COMPARE-FIELDS SECTION.
      ******************************************************************

           IF WK-KEY < CH-USE-KEY
              MOVE "MISSING S35 RECORD" TO D01-TEXT
              MOVE WK-KEY-BRNO   TO D01-BRNO
              MOVE WK-KEY-DATE   TO D01-DATE
              MOVE WK-KEY-CODE   TO D01-CODE
              MOVE WK-KEY-USERID TO D01-USERID
              MOVE WK-KEY-TIME   TO D01-TIME
              MOVE WK-KEY-SEQ    TO D01-SEQ
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.

           IF WK-KEY > CH-USE-KEY
              MOVE "MISSING A15 RECORD" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.

           IF CH-CODE = "B" OR "T" OR "Z" OR "M" OR "C"
              IF CH-DESCRIPT NOT = OCH-DESCRIPT
                 MOVE "CH-DESCRIPT" TO D01-TEXT
                 MOVE OCH-DESCRIPT  TO D01-A15
                 MOVE CH-DESCRIPT   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CH-CODE = "A" OR "S" 
              IF CH-ORG-SSNO NOT = OCH-OLD-SSNO
                 MOVE "CH-ORG-SSNO" TO D01-TEXT
                 MOVE OCH-OLD-SSNO  TO D01-A15
                 MOVE CH-ORG-SSNO   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF 
              IF CH-NEW-SSNO NOT = OCH-NEW-SSNO
                 MOVE "CH-NEW-SSNO" TO D01-TEXT
                 MOVE OCH-NEW-SSNO  TO D01-A15
                 MOVE CH-NEW-SSNO   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF 
           END-IF.

           IF CH-CODE = "B"
              MOVE OCH-CREDIT-ACCTNO TO TEST-ACCTNO
              IF CH-ACCTNO NOT = TEST-NUMBER
                 MOVE "CH-ACCTNO"       TO D01-TEXT
                 MOVE OCH-CREDIT-ACCTNO TO D01-A15
                 MOVE CH-ACCTNO         TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF 

              IF ( CH-DESCRIPT = "BANKRUPT DATE" )
               OR ( CH-DESCRIPT = "REAFF EFFDATE" )
               OR ( CH-DESCRIPT = "REAFF SYSDATE" )
               OR ( CH-DESCRIPT = "REAFF RESCIND DATE" )
               OR ( CH-DESCRIPT = "DISCHARGE EFFDATE" )
               OR ( CH-DESCRIPT = "DISCHARGE SYSDATE" )
                 MOVE OCH-ORG-DATE TO TEST-ORG-DATE
                 IF TEST-ORG-DATE-CHAR1 = SPACE
                    MOVE "0" TO TEST-ORG-DATE-CHAR1
                 END-IF
                 IF CH-ORG-DATE NOT = TEST-ORG-DATE
                    MOVE "CH-ORG-DATE" TO D01-TEXT
                    MOVE OCH-ORG-DATE  TO D01-A15
                    MOVE CH-ORG-DATE   TO D01-S35
                    PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                    PERFORM WRITE-DETAIL-LINE-ERROR
                 END-IF 
                 IF CH-NEW-DATE NOT = OCH-NEW-DATE
                    MOVE "CH-NEW-DATE" TO D01-TEXT
                    MOVE OCH-NEW-DATE  TO D01-A15
                    MOVE CH-NEW-DATE   TO D01-S35
                    PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                    PERFORM WRITE-DETAIL-LINE-ERROR
                 END-IF 
              ELSE 
                 IF CH-DESCRIPT = "REAFF BALANCE" OR "REAFF PAYAMT"
                    IF CH-ORG-6DEC2 NOT = OCH-ORG-6DEC2
                       MOVE "CH-ORG-6DEC2" TO D01-TEXT
                       MOVE OCH-ORG-6DEC2  TO D01-A15-NUM
                       MOVE CH-ORG-6DEC2   TO D01-S35-NUM
                       PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                       PERFORM WRITE-DETAIL-LINE-ERROR
                    END-IF 
                    IF CH-NEW-6DEC2 NOT = OCH-NEW-6DEC2
                       MOVE "CH-NEW-6DEC2" TO D01-TEXT
                       MOVE OCH-NEW-6DEC2  TO D01-A15-NUM
                       MOVE CH-NEW-6DEC2   TO D01-S35-NUM
                       PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                       PERFORM WRITE-DETAIL-LINE-ERROR
                    END-IF 
                 ELSE
                    IF OCH-DESCRIPT = "REAFF NUMBER PAYS" 
                       IF CH-ORG-NODEC NOT = OCH-ORG-NODEC
                          MOVE "CH-ORG-NODEC" TO D01-TEXT
                          MOVE OCH-ORG-NODEC  TO D01-A15
                          MOVE CH-ORG-NODEC   TO D01-S35
                          PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                          PERFORM WRITE-DETAIL-LINE-ERROR
                       END-IF 
                       IF CH-NEW-NODEC NOT = OCH-NEW-NODEC
                          MOVE "CH-NEW-NODEC" TO D01-TEXT
                          MOVE OCH-NEW-NODEC  TO D01-A15
                          MOVE CH-NEW-NODEC   TO D01-S35
                          PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                          PERFORM WRITE-DETAIL-LINE-ERROR
                       END-IF 
                    ELSE
                       IF OCH-DESCRIPT = "REAFF INTRATE"  
                          IF CH-ORG-RATE NOT = OCH-ORG-RATE
                             MOVE "CH-ORG-RATE" TO D01-TEXT
                             MOVE OCH-ORG-RATE  TO D01-A15-NUM
                             MOVE CH-ORG-RATE   TO D01-S35-NUM
                             PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                             PERFORM WRITE-DETAIL-LINE-ERROR
                          END-IF 
                          IF CH-NEW-RATE NOT = OCH-NEW-RATE
                             MOVE "CH-NEW-RATE" TO D01-TEXT
                             MOVE OCH-NEW-RATE  TO D01-A15-NUM
                             MOVE CH-NEW-RATE   TO D01-S35-NUM
                             PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                             PERFORM WRITE-DETAIL-LINE-ERROR
                          END-IF 
                       ELSE 
                          IF CH-ORG-CHAR NOT = OCH-ORG-CHAR
                             MOVE "CH-ORG-CHAR" TO D01-TEXT
                             MOVE OCH-ORG-CHAR  TO D01-A15
                             MOVE CH-ORG-CHAR   TO D01-S35
                             PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                             PERFORM WRITE-DETAIL-LINE-ERROR
                         END-IF 
                         IF CH-NEW-CHAR NOT = OCH-NEW-CHAR
                            MOVE "CH-NEW-CHAR" TO D01-TEXT
                            MOVE OCH-NEW-CHAR  TO D01-A15
                            MOVE CH-NEW-CHAR   TO D01-S35
                            PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                            PERFORM WRITE-DETAIL-LINE-ERROR
                         END-IF 
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF.

           IF CH-CODE = "C"
              MOVE OCH-CONFID-ACCTNO TO TEST-ACCTNO
              IF CH-ACCTNO NOT = TEST-NUMBER
                 MOVE "CH-ACCTNO"       TO D01-TEXT
                 MOVE OCH-CONFID-ACCTNO TO D01-A15
                 MOVE CH-ACCTNO         TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF 
              IF CH-ORG-CHAR NOT = OCH-OLD-CONFIDENT
                 MOVE "CH-ORG-CHAR" TO D01-TEXT
                 MOVE OCH-OLD-CONFIDENT  TO D01-A15
                 MOVE CH-ORG-CHAR   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF 
              IF CH-NEW-CHAR NOT = OCH-NEW-CONFIDENT
                 MOVE "CH-NEW-CHAR" TO D01-TEXT
                 MOVE OCH-NEW-CONFIDENT  TO D01-A15
                 MOVE CH-NEW-CHAR   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF 
           END-IF.

           IF CH-CODE = "D" OR "F" OR "H" OR "L" OR "X" OR "1"  
              MOVE OCH-CREDIT-ACCTNO  TO TEST-ACCTNO
              IF CH-ACCTNO NOT = TEST-NUMBER
                 MOVE "CH-ACCTNO"       TO D01-TEXT
                 MOVE OCH-CREDIT-ACCTNO TO D01-A15
                 MOVE CH-ACCTNO         TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF 

              MOVE CH-ORG-NODEC TO HOLD-NUM-CHECK
              IF HOLD-NUM-CHECK NOT = OCH-OLD-CREDIT
                 MOVE "CH-ORG-NODEC"  TO D01-TEXT
                 MOVE OCH-OLD-CREDIT  TO D01-A15
                 MOVE CH-ORG-NODEC    TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF 
              MOVE CH-NEW-NODEC TO HOLD-NUM-CHECK
              IF HOLD-NUM-CHECK NOT = OCH-NEW-CREDIT
                 MOVE "CH-NEW-NODEC" TO D01-TEXT
                 MOVE OCH-NEW-CREDIT  TO D01-A15
                 MOVE CH-NEW-NODEC    TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF 
           END-IF.

           IF CH-CODE = "E" 
              MOVE OCH-FROZEN-ACCTNO  TO TEST-ACCTNO
              IF CH-ACCTNO NOT = TEST-NUMBER
                 MOVE "CH-ACCTNO"       TO D01-TEXT
                 MOVE OCH-FROZEN-ACCTNO TO D01-A15
                 MOVE CH-ACCTNO         TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF 
              IF CH-ORG-CHAR NOT = OCH-OLD-FROZEN
                 MOVE "CH-ORG-CHAR"  TO D01-TEXT
                 MOVE OCH-OLD-FROZEN TO D01-A15
                 MOVE CH-ORG-CHAR    TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF 
              IF CH-NEW-CHAR NOT = OCH-NEW-FROZEN
                 MOVE "CH-NEW-CHAR"  TO D01-TEXT
                 MOVE OCH-NEW-FROZEN TO D01-A15
                 MOVE CH-NEW-CHAR    TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-IF.

           IF CH-CODE = "K" OR "P"
              MOVE OCH-TRW-ACCTNO     TO TEST-ACCTNO
              IF CH-ACCTNO NOT = TEST-NUMBER
                 MOVE "CH-ACCTNO"      TO D01-TEXT
                 MOVE OCH-TRW-ACCTNO   TO D01-A15
                 MOVE CH-ACCTNO        TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF 
              IF CH-ORG-CHAR NOT = OCH-OLD-TRW
                 MOVE "CH-ORG-CHAR" TO D01-TEXT
                 MOVE OCH-OLD-TRW   TO D01-A15
                 MOVE CH-ORG-CHAR   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF 
              IF CH-NEW-CHAR NOT = OCH-NEW-TRW
                 MOVE "CH-NEW-CHAR" TO D01-TEXT
                 MOVE OCH-NEW-TRW   TO D01-A15
                 MOVE CH-NEW-CHAR   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF 
              IF ( OCH-CODE = "P" )  *> IF "P" ETL MOVED ZEROS TO CH-TYPE
                 IF CH-TYPE NOT = ZEROS
                    MOVE "CH-TYPE" TO D01-TEXT
                    MOVE "0"       TO D01-A15
                    MOVE CH-TYPE   TO D01-S35
                    PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                    PERFORM WRITE-DETAIL-LINE-ERROR
                 END-IF
              END-IF 
              IF OCH-CODE = "K" 
                 IF OCH-TRW-TYPE NUMERIC
                    MOVE OCH-TRW-TYPE TO CONV-TYPE-CHAR
                 ELSE
                    MOVE '9' TO CONV-TYPE-CHAR
                 END-IF
                 IF CH-TYPE NOT = CONV-TYPE-NUM
                    MOVE "CH-TYPE"     TO D01-TEXT
                    MOVE OCH-TRW-TYPE  TO D01-A15
                    MOVE CH-TYPE       TO D01-S35
                    PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                    PERFORM WRITE-DETAIL-LINE-ERROR
                 END-IF
              END-IF 
           END-IF.

           IF ( OCH-CODE = "M" OR "Q" )
              IF CH-SSNO NOT = OCH-M-SSN
                 MOVE "CH-SSNO" TO D01-TEXT
                 MOVE OCH-M-SSN TO D01-A15
                 MOVE CH-SSNO   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
             IF CH-ORG-CHAR NOT = OCH-ORG-CHAR
                 MOVE FUNCTION TRIM(OCH-ORG-CHAR) TO OCH-ORG-CHAR
                 IF CH-ORG-CHAR NOT = OCH-ORG-CHAR
                    MOVE "CH-ORG-CHAR" TO D01-TEXT
                    MOVE OCH-ORG-CHAR  TO D01-A15
                    MOVE CH-ORG-CHAR   TO D01-S35
                    PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                   PERFORM WRITE-DETAIL-LINE-ERROR
                 END-IF
              END-IF
             IF CH-NEW-CHAR NOT = OCH-NEW-CHAR
                 MOVE FUNCTION TRIM(OCH-NEW-CHAR) TO OCH-NEW-CHAR
                 IF CH-NEW-CHAR NOT = OCH-NEW-CHAR
                    MOVE "CH-NEW-CHAR" TO D01-TEXT
                    MOVE OCH-NEW-CHAR  TO D01-A15
                    MOVE CH-NEW-CHAR   TO D01-S35
                    PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                    PERFORM WRITE-DETAIL-LINE-ERROR
                 END-IF 
              END-IF
           END-IF.

           IF CH-CODE = "N" 
              MOVE OCH-LOAN-ORIGIN-ACCTNO TO TEST-ACCTNO
              IF CH-ACCTNO NOT = TEST-NUMBER
                 MOVE "CH-ACCTNO"            TO D01-TEXT
                 MOVE OCH-LOAN-ORIGIN-ACCTNO TO D01-A15
                 MOVE CH-ACCTNO              TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF 
              IF CH-ORG-CHAR NOT = OCH-OLD-LOAN-ORIGIN
                 MOVE "CH-ORG-CHAR"       TO D01-TEXT
                 MOVE OCH-OLD-LOAN-ORIGIN TO D01-A15
                 MOVE CH-ORG-CHAR         TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF 
              IF CH-NEW-CHAR NOT = OCH-NEW-LOAN-ORIGIN
                 MOVE "CH-NEW-CHAR"       TO D01-TEXT
                 MOVE OCH-NEW-LOAN-ORIGIN TO D01-A15
                 MOVE CH-NEW-CHAR         TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-IF.

           IF OCH-CODE = "O" 
              MOVE OCH-ESCROW-ACCTNO  TO TEST-ACCTNO
              IF CH-ACCTNO NOT = TEST-NUMBER
                 MOVE "CH-ACCTNO"       TO D01-TEXT
                 MOVE OCH-ESCROW-ACCTNO TO D01-A15
                 MOVE CH-ACCTNO         TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF 
              IF CH-ORG-CHAR NOT = OCH-OLD-ESCROW-FG
                 MOVE "CH-ORG-CHAR"     TO D01-TEXT
                 MOVE OCH-OLD-ESCROW-FG TO D01-A15
                 MOVE CH-ORG-CHAR       TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF 
              IF CH-NEW-CHAR NOT = OCH-NEW-ESCROW-FG
                 MOVE "CH-NEW-CHAR"     TO D01-TEXT
                 MOVE OCH-NEW-ESCROW-FG TO D01-A15
                 MOVE CH-NEW-CHAR       TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-IF.

           IF OCH-CODE = "R" OR "T" 
              IF CH-ORG-CHAR NOT = OCH-ORG-CHAR
                 MOVE FUNCTION TRIM(OCH-ORG-CHAR) TO OCH-ORG-CHAR
                 IF CH-ORG-CHAR NOT = OCH-ORG-CHAR
                    MOVE "CH-ORG-CHAR" TO D01-TEXT
                    MOVE OCH-ORG-CHAR  TO D01-A15
                    MOVE CH-ORG-CHAR   TO D01-S35
                    PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                    PERFORM WRITE-DETAIL-LINE-ERROR
                 END-IF
              END-IF 
             IF CH-NEW-CHAR NOT = OCH-NEW-CHAR
                 MOVE FUNCTION TRIM(OCH-NEW-CHAR) TO OCH-NEW-CHAR
                 IF CH-NEW-CHAR NOT = OCH-NEW-CHAR
                    MOVE "CH-NEW-CHAR"  TO D01-TEXT
                    MOVE OCH-NEW-CHAR   TO D01-A15
                    MOVE CH-NEW-CHAR    TO D01-S35
                    PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                    PERFORM WRITE-DETAIL-LINE-ERROR
                 END-IF
              END-IF
           END-IF.

           IF OCH-CODE = "Y"
              MOVE OCH-CREDIT-ACCTNO TO TEST-ACCTNO
              IF CH-ACCTNO NOT = TEST-NUMBER
                 MOVE "CH-ACCTNO"        TO D01-TEXT
                 MOVE OCH-CREDIT-ACCTNO  TO D01-A15
                 MOVE CH-ACCTNO          TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF 
              IF CH-ORG-ALLOTCD NOT = OCH-OLD-ALLOTCD
                 MOVE "CH-ORG-ALLOTCD" TO D01-TEXT
                 MOVE OCH-OLD-ALLOTCD  TO D01-A15
                 MOVE CH-ORG-ALLOTCD   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF 
              IF CH-NEW-ALLOTCD NOT = OCH-NEW-ALLOTCD
                 MOVE "CH-NEW-ALLOTCD" TO D01-TEXT
                 MOVE OCH-NEW-ALLOTCD  TO D01-A15
                 MOVE CH-NEW-ALLOTCD   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CH-ORG-LBOX NOT = OCH-OLD-LBOX
                 MOVE "CH-ORG-LBOX" TO D01-TEXT
                 MOVE OCH-OLD-LBOX  TO D01-A15
                 MOVE CH-ORG-LBOX   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF 
              IF CH-NEW-LBOX NOT = OCH-NEW-LBOX
                 MOVE "CH-NEW-LBOX" TO D01-TEXT
                 MOVE OCH-NEW-LBOX  TO D01-A15
                 MOVE CH-NEW-LBOX   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-IF.

           IF OCH-CODE = "Z"
              MOVE OCH-CREDIT-ACCTNO  TO TEST-ACCTNO
              IF CH-ACCTNO NOT = TEST-NUMBER
                 MOVE "CH-ACCTNO"        TO D01-TEXT
                 MOVE OCH-CREDIT-ACCTNO  TO D01-A15
                 MOVE CH-ACCTNO          TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF 
              IF CH-ORG-CHAR NOT = OCH-ORG-CHAR
                 MOVE "CH-ORG-CHAR" TO D01-TEXT
                 MOVE OCH-ORG-CHAR  TO D01-A15
                 MOVE CH-ORG-CHAR   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF 
              IF CH-NEW-CHAR NOT = OCH-NEW-CHAR
                 MOVE "CH-NEW-CHAR" TO D01-TEXT
                 MOVE OCH-NEW-CHAR  TO D01-A15
                 MOVE CH-NEW-CHAR   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-IF.

           IF OCH-CODE = "2"
              MOVE OCH-TDRS-ACCTNO TO TEST-ACCTNO
              IF CH-ACCTNO NOT = TEST-NUMBER
                 MOVE "CH-ACCTNO"        TO D01-TEXT
                 MOVE OCH-TDRS-ACCTNO  TO D01-A15
                 MOVE CH-ACCTNO          TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF 
              IF CH-TDR-1-2 NOT = OCH-TDR-1-2
                 MOVE "CH-TDR-1-2" TO D01-TEXT
                 MOVE OCH-TDR-1-2  TO D01-A15
                 MOVE CH-TDR-1-2   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF 
              IF CH-ORG-TDR NOT = OCH-OLD-TDR
                 MOVE "CH-ORG-TDR" TO D01-TEXT
                 MOVE OCH-OLD-TDR  TO D01-A15
                 MOVE CH-ORG-TDR   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CH-ORG-TDR-DATE NOT = OCH-OLD-TDR-DATE
                 MOVE "CH-ORG-TDR-DATE" TO D01-TEXT
                 MOVE OCH-OLD-TDR-DATE  TO D01-A15
                 MOVE CH-ORG-TDR-DATE   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF 
              IF CH-NEW-TDR NOT = OCH-NEW-TDR 
                 MOVE "CH-NEW-TDR" TO D01-TEXT
                 MOVE OCH-NEW-TDR  TO D01-A15
                 MOVE CH-NEW-TDR   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CH-NEW-TDR-DATE NOT = OCH-NEW-TDR-DATE
                 MOVE "CH-NEW-TDR-DATE" TO D01-TEXT
                 MOVE OCH-NEW-TDR-DATE  TO D01-A15
                 MOVE CH-NEW-TDR-DATE   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF 
              IF CH-TDRS-SOURCE NOT = OCH-TDRS-SOURCE 
                 MOVE "CH-TDRS-SOURCE" TO D01-TEXT
                 MOVE OCH-TDRS-SOURCE  TO D01-A15
                 MOVE CH-TDRS-SOURCE   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-IF.

       EXIT-COMPARE-FIELDS.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE-ERROR-KEY SECTION.
      ******************************************************************
           MOVE CH-BRNO   TO D01-BRNO.
           MOVE CH-DATE   TO D01-DATE.
           MOVE CH-CODE   TO D01-CODE.
           MOVE CH-USERID TO D01-USERID.
           MOVE CH-TIME   TO D01-TIME.
           MOVE CH-SEQ    TO D01-SEQ.

       WRITE-DETAIL-LINE-ERROR-KEY-EXIT.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE-ERROR SECTION.
      ******************************************************************
           ADD 1    TO TOTAL-ERRORS-CTR.
           MOVE "Y" TO PREV-BRANCH-ERR-SW.
           PERFORM WRITE-DETAIL-LINE.

       WRITE-DETAIL-LINE-ERROR-EXIT.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************
           ADD  LN-FEED TO LN-COUNT.

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
           MOVE PAGE-NUMBER                TO H01-PAGE.

           MOVE HEADER-LINE-01             TO PR-REC.
           MOVE 99                         TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE EXT-CONAME-USER-ID         TO H02-USERID.

           MOVE HEADER-LINE-02             TO PR-REC.
           ADD  01                         TO LN-COUNT.
           MOVE 01                         TO LN-FEED.
           PERFORM WRITE-PR-REC.

           IF PRINTING-RANGES-SW = "N"
              MOVE HEADER-LINE-03          TO PR-REC
              ADD  02                      TO LN-COUNT
              MOVE 02                      TO LN-FEED
              PERFORM WRITE-PR-REC.

           MOVE 2                          TO LN-FEED.

       WRITE-HEADER-LINES-EXIT.
           EXIT.

      ******************************************************************
       RANGE-LINES SECTION.
      ******************************************************************

           MOVE 88  TO LN-COUNT.
           MOVE "Y" TO PRINTING-RANGES-SW.

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
           MOVE BEG-MMYY                              TO R01-NUMERIC.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING DATE                   : "    TO R01-PROMPT.
           MOVE END-MMYY                              TO R01-NUMERIC.
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
           MOVE EXT-CONAME-CONAME TO CHFILE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CHFILE-SYSDATE.
           DISPLAY CHFILE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE CHFILE-FORM-FIELDS TO WR-BUF.
           MOVE CHFILE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE CHFILE-HELP--FILE TO HELP-LINK-FILE.
           MOVE CHFILE-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/CHFILE_EVA.CPY".
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
           MOVE EXT-ROUTE-BUF          TO PROGID-ROUTE-BUF.
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
       COPY "LIBLP/LPCHGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

      * COPY "LIBLP/LPCHRN.CPY".
      * COPYMEMBER: LIBLP/LPCHR
       LOAD-CH-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CH-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CH-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO CH-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CH-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO CH-ALL-DATA
              MOVE CH-ALL-PATH             TO CH-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CH-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO CH-ALL-BASE
              MOVE FILPATH-MACHINE         TO CH-ALL-MACHINE
              MOVE FILPATH-DATA            TO CH-ALL-DATA
              MOVE CH-ALL-PATH             TO CH-PATH.

      *-----------------------------------------------------------------
       OPEN-CH-FILE.
           PERFORM LOAD-CH-FILE.
           PERFORM OPEN-IT.
           MOVE CH-PATH-SQL TO E-FILE.

           IF CH1-CONN-OPEN-SW = "N" 
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS CH1_CONNECT
                        USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE 'Y' TO CH1-CONN-OPEN-SW.

      *-----------------------------------------------------------------
       START-CH-FILE.
           PERFORM START-IT.
           MOVE CH-PATH-OWNBR  TO CH-BRNO.
           MOVE CH-PATH-SQL    TO E-FILE.
           MOVE CH-KEY         TO E-KEYX.

           IF ( CH-CURSOR-STAT-GOOD )
              PERFORM CLOSE-CH-FILE.

      *      SELECT *
      *      FROM DBO.CHFILE
      *      WHERE CHFILE.CH_BRNO  = :QCH1-WBEG-BRNO
      *      ORDER BY CHFILE.CH_BRNO,
      *               CHFILE.CH_DATE,
      *               CHFILE.CH_CODE,
      *               CHFILE.CH_USERID,
      *               CHFILE.CH_TIME,
      *               CHFILE.CH_SEQ

           EXEC SQL SET CONNECTION CH1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE CH1_CURSOR CURSOR FOR
                :CH1-RETURN-CODE =  
            EXEC SSP_CHFILE_CHFILE_SELECT
              ( :QCH-WBEG-BRNO, :QCH-WBEG-DATE, :QCH-WEND-DATE)
           END-EXEC.

           EXEC SQL
               OPEN CH1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           MOVE ZEROES TO QCH-WBEG-BRNO.

           MOVE STAT---GOOD TO CH-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-CH-FILE-NEXT.
           PERFORM READ-IT.
           MOVE CH-PATH-OWNBR  TO CH-BRNO.
           MOVE CH-PATH-SQL    TO E-FILE.
           MOVE CH-KEY         TO E-KEYX.
           INITIALIZE QCH-REC.

           EXEC SQL SET CONNECTION CH1_CONNECT END-EXEC.

           IF ( CH-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH CH1_CURSOR INTO :QCH-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CH-FILE-NEXT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           IF ( IO-FG = 0 )
              PERFORM GET-CH-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-CH-FILE.

           PERFORM CLOSE-IT.
           IF ( CH-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION CH1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE CH1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO CH-CURSOR-STAT.
      *-----------------------------------------------------------------
       CLOSE-CH1-CONNECTION.

           IF CH1-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION CH1_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT CH1_CONNECT END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE "N" TO CH1-CONN-OPEN-SW.

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
       OPEN-OCH1-FILE.
           PERFORM OPEN-IT.
           MOVE OCH-PATH TO E-FILE.
           OPEN INPUT OCH-FILE.
           IF IO-FG = 8
              GO TO OPEN-OCH1-FILE.
           IF IO-FG = 7
              CLOSE OCH-FILE
              GO TO OPEN-OCH1-FILE.
       READ-OCH1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OCH-PATH TO E-FILE.
      *     MOVE OCH-KEY  TO E-KEYX.
           READ OCH-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OCH1-FILE-NEXT.
       CLOSE-OCH1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OCH-FILE.

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
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           MOVE WK-KEY  TO E-KEYX.
           START WK-FILE.
           UNLOCK WK-FILE.
       READ-WK1-FILE.
           PERFORM READ-IT.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           MOVE WK-KEY  TO E-KEYX.
           READ WK-FILE
              INVALID KEY
                MOVE 1 TO IO-FG
           END-READ.
           IF IO-FG = 8
              GO TO READ-WK1-FILE.
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
                MOVE 22 TO IO-STATUS IO-FG
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

           IF ( SV-BEG-BRANCH NOT = 9999 )
              IF ( EXT-EOBUF-GROUP-FG = "Y" )
                 MOVE SV-BEG-BRANCH        TO BEG-BRANCH
                 MOVE SV-END-BRANCH        TO END-BRANCH
                 MOVE SV-BEG-MMYY          TO BEG-MMYY
                 MOVE SV-END-MMYY          TO END-MMYY
                 MOVE SV-GROUP-FLAG        TO GROUP-FLAG
                 MOVE SV-ENT-GROUP         TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BRANCH        TO BEG-BRANCH
                 MOVE SV-END-BRANCH        TO END-BRANCH
                 MOVE SV-BEG-MMYY          TO BEG-MMYY
                 MOVE SV-END-MMYY          TO END-MMYY.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE EXIT-PATH               TO FORM-NAM
              MOVE FORM-PATH               TO FORM-PATHNAME.

           MOVE WK-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY CHFILE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
