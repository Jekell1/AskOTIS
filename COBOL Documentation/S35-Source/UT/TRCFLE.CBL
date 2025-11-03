       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TRCFLE.
       DATE-WRITTEN.  01-24-2024.
       AUTHOR. MARK BOOMGAARD.
      ******************************************************************
      *
      * DESCRIPTION   : COMPARE S35 TRCFILE WITH A15 TRCFILE
      *            
      *                 TO BE USED AFTER AN UPDATE
      *                 WILL COMPARE EACH FIELD AND REPORT ANY 
      *                 DIFFERENCES, AS LONG AS FIELD EXIST IN BOTH FILES
      *
      *  2 FILE MATCH PROCESSING.
      *
      ******************************************************************
      * 2024-1112 MB  ADDED DEFAULT BEGIN DATE = SYSDATE - 5 YEARS 
      * 2025-0106 RZ  CHANGED DATE PARAMETERS CODE FOR BATCH PROCESSING.
      * 2025-0106 MB  ADD CODE TO EO-EXEC-AUDIT TO FORMAT SQL DATES.
      * 2025-0108 RZ  ADDED 'BTRACK' - PROCESS TRACKING TO PROGRAM.
      * 2025-0225 RZ  ADDED DEFAULT BEGIN DATE = SYSDATE - 59 MONTHS
      * 2025-0430 MB  SKIP POSTTIME/USERID IF EN-GROUP = S351 (USER TESTING)
      ********************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT OTR-FILE ASSIGN TO TRANS-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OTR1-KEY
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  OTR-FILE
           LABEL RECORDS ARE STANDARD.
      *-----------------------------------------------------------------
       78  OTR-SIZE                     VALUE 550.
       01  OTR-REC.
           03  OTR1-KEY.
               05  OTR1-DAY              PIC 9(2)      COMP.
               05  OTR1-BRNO             PIC 9(4)      COMP.
               05  OTR1-CODE             PIC X.
                   88  OTR1-VALID-CD-B   VALUE "B".
                   88  OTR1-VALID-CD-C   VALUE "C".
                   88  OTR1-VALID-CD-D   VALUE "D".
                   88  OTR1-VALID-CD-G   VALUE "G".
                   88  OTR1-VALID-CD-I   VALUE "I".
                   88  OTR1-VALID-CD-J   VALUE "J".
                   88  OTR1-VALID-CD-K   VALUE "K".
                   88  OTR1-VALID-CD-L   VALUE "L".
                   88  OTR1-VALID-CD-P   VALUE "P".
                   88  OTR1-VALID-CD-S   VALUE "S".
                   88  OTR1-VALID-CD-X   VALUE "X".
                   88  OTR1-VALID-CD-ALL VALUE "B" "C" "D" "G" "J"
                                               "K" "L" "P" "S" "X" "I".
               05  OTR1-CLASS            PIC 99        COMP.
               05  OTR1-SUBKEY           PIC X(9).
           03  FILLER                    PIC X(536).

       COPY "LIBUP/A15SRC/LPFDTRC.CPY" REPLACING LEADING "TC" BY "OTC".

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/TRCFLE S35 01/26/24-11:00:34 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01TRC.CPY".
       COPY "LIBLP/LP01TRC_SQL.CPY".
       COPY "LIBLP/LPWSTRC.CPY".
       01  OTR-PATH                  PIC X(35) VALUE SPACES.
       01  OTR-ALL-PATH.
           03  OTR-ALL-BASE          PIC X(9).
           03  FILLER                PIC X     VALUE "/".
           03  OTR-ALL-MACHINE       PIC XX.
           03  FILLER                PIC X    VALUE "/".
           03  OTR-ALL-DATA          PIC X(6).
           03  FILLER                PIC X(6) VALUE "/ED/TR".
           03  OTR-DATE-PATH         PIC 9(4).
           03  OTR-DATE-P REDEFINES OTR-DATE-PATH.
               05  OTR-ALL-MONTH     PIC 99.
               05  OTR-ALL-YEAR      PIC 99.

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

       01  TRC-EOF-SW                  PIC X      VALUE "N".
       01  OTRC-EOF-SW                 PIC X      VALUE "N".
       01  PREV-BRANCH-ERR-SW          PIC X      VALUE "N".
       01  TOTAL-ERRORS-CTR            PIC 9(9)   VALUE 0.
       01  A15-FILE-FOUND-SW           PIC X      VALUE "N".
       01  SUB1                        PIC 9      VALUE 0.
       01  HOLD-LN-TILLNO              PIC 9(2).
       01  TRC1-CONN-OPEN-SW           PIC X      VALUE "N".

       01  TEST-ACCTNO                 PIC 9(8).
       01  FILLER REDEFINES TEST-ACCTNO.
             03  TEST-NUMBER           PIC 9(6).
             03  TEST-SEQ              PIC 9(2).    

       01  OTRC-USE-KEY.
           03 OTRC-USE-KEY-BRNO        PIC 9(4).
           03 OTRC-USE-KEY-DATE.
             05 OTRC-USE-KEY-DATE-CN    PIC 9(2).
             05 OTRC-USE-KEY-DATE-YY    PIC 9(2).
             05 OTRC-USE-KEY-DATE-MM    PIC 9(2).
             05 OTRC-USE-KEY-DATE-DD    PIC 9(2).
           03 OTRC-USE-KEY-CKNO         PIC 9(6).
           03 OTRC-USE-KEY-CLASS        PIC 9(3).
           03 OTRC-USE-KEY-SEQ          PIC 9(5).

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

       01  MISC-DATE-MMYY.
           03  MISC-DATE-MM            PIC 99. 
           03  MISC-DATE-YY            PIC 99. 
       01  MISC-DATE-MMYY-NUM          REDEFINES MISC-DATE-MMYY
                                       PIC 9(4).

       01  WS-59MONTH-YYMM.
           03  WS-59MONTH-YY           PIC 99.
           03  WS-59MONTH-MM           PIC 99.

       01  WS-BEGIN-YYMM.
           03  WS-BEGIN-YY             PIC 99.
           03  WS-BEGIN-MM             PIC 99.


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

       01  LN-COUNT                    PIC 9(03)     VALUE 99.
       01  LN-FEED                     PIC 9(02).
       01  PAGE-NUMBER                 PIC 9(04)     VALUE ZEROES.

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
           03  FILLER                  PIC X(16) VALUE SPACES.
           03  FILLER                  PIC X(06) VALUE "PAGE: ".
           03  H01-PAGE                PIC Z(04).

      *-----------------------------------------------------------------
       01  HEADER-LINE-02.
           03  FILLER                  PIC X(10) VALUE "USER ID:".
           03  H02-USERID              PIC X(10) VALUE SPACES.
           03  FILLER                  PIC X(10) VALUE SPACES.
           03  H02-TITLE               PIC X(45) VALUE 
           "TR CHECKS FILE (TRCFILE) COMPARE A15 TO S35".

       01  HEADER-LINE-03.
           03  FILLER                PIC X(4)  VALUE "BRNO".
           03  FILLER                PIC X(2)  VALUE SPACES.
           03  FILLER                PIC X(8)  VALUE "  DATE  ".
           03  FILLER                PIC X(3)  VALUE SPACES.
           03  FILLER                PIC X(4)  VALUE "CKNO".
           03  FILLER                PIC X(3)  VALUE SPACES.
           03  FILLER                PIC X(3)  VALUE "CLS".
           03  FILLER                PIC X(4)  VALUE SPACES.
           03  FILLER                PIC X(4)  VALUE "SEQ".
           03  FILLER                PIC X(2)  VALUE SPACES.
           03  FILLER                PIC X(11) VALUE "DESCRIPTION".
           03  FILLER                PIC X(11) VALUE SPACES.
           03  FILLER                PIC X(3)  VALUE "A15".
           03  FILLER                PIC X(19) VALUE SPACES.
           03  FILLER                PIC X(3)  VALUE "S35".

      ******************************************************************
      *
      *    D E T A I L - L I N E - W O R K E R S
      *
      ******************************************************************
       01  DETAIL-LINE                        PIC X(100)   VALUE SPACES.

       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-BRNO                       PIC Z(3)9.
           03  FILLER                         PIC X(2).
           03  D01-DATE                       PIC 9(8).
           03  FILLER                         PIC X(2).
           03  D01-CKNO                       PIC 9(6).
           03  FILLER                         PIC X(2).
           03  D01-CLASS                      PIC 9(3).
           03  FILLER                         PIC X(3).
           03  D01-SEQ                        PIC 9(5).
           03  FILLER                         PIC X(2).
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
       COPY "LIBUT/TRCFLE_DEF.CPY".
       COPY "LIBUT/TRCFLE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/TRCFLE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE OTR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TRC1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           PERFORM CLOSE-TRC1-CONNECTION.
           CLOSE OTR-FILE.
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
           MOVE "TRCFLE" TO VDU-FORM-NAME.

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

           MOVE TRCFLE-QUEUE-POS  TO Q-POS.
           MOVE TRCFLE-COPIES-POS TO C-POS.
           MOVE TRCFLE-PRU-POS    TO P-POS.
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
           MOVE DATE-YYYYMM TO BEG-BY-DATE-CCYYMM.

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

           MOVE 9999 TO PROGID-SLEEP-COUNTER.
           PERFORM SEND-PROGRESS-INDICATOR.

      D    STOP MESS.

           IF PREV-BRANCH-ERR-SW = "Y"
              PERFORM WRITE-DETAIL-LINE.

           MOVE "N" TO PREV-BRANCH-ERR-SW.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO TRC-PATH-OVERRIDE.

           MOVE BR-NO TO QTRC1-WBEG-BRNO.

           MOVE BEG-BY-DATE-CCYYMMDD TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QTRC1-WBEG-KEY-DATE.

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
           MOVE SQL-DATE-YYYY-MM-DD  TO QTRC1-WEND-KEY-DATE.

           MOVE "N"  TO TRC-EOF-SW.
           PERFORM OPEN-TRC1-FILE.
           PERFORM START-TRC1-FILE.   *> SSP TRCFILE
           PERFORM M-READ-TRC1.

           MOVE SOURCE-PATH   TO OTR-ALL-BASE.
           MOVE BR-DATA-PATH  TO OTR-ALL-DATA.

           MOVE BEG-BY-DATE-YYMM TO PROC-YYMM.

           PERFORM VARYING PROC-YYMM FROM PROC-YYMM BY 1
             UNTIL PROC-YYMM > END-BY-DATE-YYMM

              MOVE PROC-YYMM-YY  TO OTR-ALL-YEAR
              MOVE PROC-YYMM-MM  TO OTR-ALL-MONTH
              MOVE MACHINE-R1    TO OTR-ALL-MACHINE
              MOVE OTR-ALL-PATH  TO OTR-PATH TRANS-PATH ACCESS-BUF
              MOVE "Y" TO A15-FILE-FOUND-SW
              PERFORM ACCESS-CALL
              IF ( STAT-BAD )
                 MOVE MACHINE-R2   TO OTR-ALL-MACHINE
                 MOVE OTR-ALL-PATH TO OTR-PATH TRANS-PATH ACCESS-BUF
                 PERFORM ACCESS-CALL
                 IF ( STAT-BAD )
                    MOVE "N" TO A15-FILE-FOUND-SW
                 END-IF
              END-IF

              IF A15-FILE-FOUND-SW = "Y"
                 PERFORM OPEN-OTRC1-FILE
                 MOVE LOW-VALUES   TO OTR1-KEY
                 MOVE 01           TO OTR1-DAY
                 MOVE BR-NO        TO OTR1-BRNO
                 MOVE "C"          TO OTR1-CODE
                 MOVE "N"          TO OTRC-EOF-SW

                 MOVE BR-NO        TO OTRC-USE-KEY-BRNO
                 IF PROC-YYMM-YY > 80
                    MOVE 19 TO OTRC-USE-KEY-DATE-CN
                 ELSE 
                    MOVE 20 TO OTRC-USE-KEY-DATE-CN
                 END-IF
                 MOVE PROC-YYMM-YY TO OTRC-USE-KEY-DATE-YY
                 MOVE PROC-YYMM-MM TO OTRC-USE-KEY-DATE-MM
                 MOVE 0            TO OTRC-USE-KEY-DATE-DD

                 PERFORM START-OTRC1-FILE
                 PERFORM M-READ-OTRC
                 PERFORM PROCESS-BRANCH-NEXT-TRC
                    UNTIL OTRC-EOF-SW = "Y"
                 PERFORM CLOSE-OTRC1-FILE
              END-IF
              IF PROC-YYMM-MM = 12
                  ADD  1 TO PROC-YYMM-YY
                  MOVE 0 TO PROC-YYMM-MM
              END-IF

           END-PERFORM.

      * CHECK FOR MORE S35 RECORDS
           PERFORM PROCESS-BRANCH-NEXT-TRC
              UNTIL TRC-EOF-SW = "Y"

           PERFORM CLOSE-TRC1-FILE.

           GO TO PROCESS-BRANCH-DRIVER-END.

       PROCESS-BRANCH-NEXT-TRC.

           PERFORM SEND-PROGRESS-INDICATOR.

           PERFORM COMPARE-FIELDS.

           IF OTRC-USE-KEY < TRC1-KEY
              PERFORM M-READ-OTRC 
           ELSE
           IF OTRC-USE-KEY > TRC1-KEY
              PERFORM M-READ-TRC1
           ELSE
              PERFORM M-READ-OTRC
              PERFORM M-READ-TRC1.

       PROCESS-BRANCH-DRIVER-END.
           EXIT. 

       M-READ-TRC1 SECTION.
           PERFORM READ-TRC1-FILE-NEXT.  *> SSP TRCFILE
           IF IO-BAD
              MOVE ALL '9'  TO TRC1-KEY
              MOVE "Y"      TO TRC-EOF-SW
           ELSE
              ADD 1 TO SQL-RECORDS-PROCESSED.

       EXIT-M-READ-TRC1.
           EXIT.

       M-READ-OTRC SECTION.
           PERFORM READ-OTRC1-FILE-NEXT.
           IF IO-BAD
              MOVE ALL '9'  TO OTRC-USE-KEY
              MOVE "Y"      TO OTRC-EOF-SW
           ELSE
              IF (OTC-BRNO NOT = BR-NO) OR (OTC-CODE NOT = "C") 
                 GO TO M-READ-OTRC
              ELSE
                 ADD 1 TO A15-RECORDS-PROCESSED
                 MOVE OTC-DAY   TO OTRC-USE-KEY-DATE-DD
                 MOVE OTC-CKNO  TO OTRC-USE-KEY-CKNO
                 MOVE OTC-CLASS TO OTRC-USE-KEY-CLASS
                 MOVE OTC-SEQ   TO OTRC-USE-KEY-SEQ.

       EXIT-M-READ-OTRC.
           EXIT.

      ******************************************************************
       COMPARE-FIELDS SECTION.
      ******************************************************************

           IF OTRC-USE-KEY < TRC1-KEY
              MOVE "MISSING S35 RECORD" TO D01-TEXT
              MOVE OTRC-USE-KEY-BRNO    TO D01-BRNO
              MOVE OTRC-USE-KEY-DATE    TO D01-DATE
              MOVE OTRC-USE-KEY-CKNO    TO D01-CKNO
              MOVE OTRC-USE-KEY-CLASS   TO D01-CLASS
              MOVE OTRC-USE-KEY-SEQ     TO D01-SEQ
              PERFORM WRITE-DETAIL-LINE-ERROR
              MOVE SPACES TO DETAIL-LINE
              GO TO EXIT-COMPARE-FIELDS.

           IF OTRC-USE-KEY > TRC1-KEY
              MOVE "MISSING A15 RECORD" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR
              MOVE SPACES TO DETAIL-LINE
              GO TO EXIT-COMPARE-FIELDS.

           IF TC-TYPE NOT = OTC-TYPE
              MOVE "TC-TYPE" TO D01-TEXT
              MOVE OTC-TYPE  TO D01-A15-NUM
              MOVE TC-TYPE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OTC-DATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = TC-DATE
              MOVE "TC-DATE" TO D01-TEXT
              MOVE OTC-DATE  TO D01-A15
              MOVE TC-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TC-AMT NOT = OTC-AMT
              MOVE "TC-AMT" TO D01-TEXT
              MOVE OTC-AMT  TO D01-A15-NUM
              MOVE TC-AMT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TC-PAYEE-NO  NOT = OTC-PAYEE-NO
      *       CALL "C$JUSTIFY" USING OTC-PAYEE-NO, "L"
              IF TC-PAYEE-NO  NOT = OTC-PAYEE-NO
                 MOVE "TC-PAYEE-NO" TO D01-TEXT
                 MOVE OTC-PAYEE-NO  TO D01-A15
                 MOVE TC-PAYEE-NO   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TC-REF NOT = OTC-REF
              MOVE "TC-REF" TO D01-TEXT
              MOVE OTC-REF  TO D01-A15
              MOVE TC-REF   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TC-REASON NOT = OTC-REASON
      *       CALL "C$JUSTIFY" USING OTC-REASON, "L"
              IF TC-REASON NOT = OTC-REASON
                 MOVE "TC-REASON" TO D01-TEXT
                 MOVE OTC-REASON  TO D01-A15
                 MOVE TC-REASON   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 5
              IF TC-GLNO(SUB1) NOT = OTC-GLNO(SUB1)
                 STRING "TC-GLNO(",SUB1,")" INTO D01-TEXT
                 MOVE OTC-GLNO(SUB1)  TO D01-A15-NUM
                 MOVE TC-GLNO(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.

           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 5
              IF TC-GLAMT(SUB1) NOT = OTC-GLAMT(SUB1)
                 STRING "TC-GLAMT(",SUB1,")" INTO D01-TEXT
                 MOVE OTC-GLAMT(SUB1)  TO D01-A15-NUM
                 MOVE TC-GLAMT(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.

           IF TC-VOID NOT = OTC-VOID
              MOVE "TC-VOID" TO D01-TEXT
              MOVE OTC-VOID  TO D01-A15
              MOVE TC-VOID   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OTC-ACCTNO TO TEST-ACCTNO.
           IF TC-ACCTNO  NOT = TEST-NUMBER
              MOVE "TC-ACCTNO" TO D01-TEXT
              MOVE OTC-ACCTNO  TO D01-A15
              MOVE TC-ACCTNO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TC-PAID  NOT = OTC-PAID
              MOVE "TC-PAID" TO D01-TEXT
              MOVE OTC-PAID  TO D01-A15
              MOVE TC-PAID   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TC-PGM  NOT = OTC-PGM
              MOVE "TC-PGM" TO D01-TEXT
              MOVE OTC-PGM  TO D01-A15
              MOVE TC-PGM   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OTC-DLRVNO NOT NUMERIC
              MOVE 0 TO OTC-DLRVNO. 
           IF TC-DLRVNO  NOT = OTC-DLRVNO
              MOVE "TC-DLRVNO" TO D01-TEXT
              MOVE OTC-DLRVNO  TO D01-A15
              MOVE TC-DLRVNO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TC-SPRCLASS  NOT = OTC-SPRCLASS
              MOVE "TC-SPRCLASS" TO D01-TEXT
              MOVE OTC-SPRCLASS  TO D01-A15
              MOVE TC-SPRCLASS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TC-DLNO NOT = OTC-DLNO
              MOVE "TC-DLNO" TO D01-TEXT
              MOVE OTC-DLNO  TO D01-A15-NUM
              MOVE TC-DLNO   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF ENT-GROUP NOT = 'S351'
              IF TC-USERID  NOT = OTC-USERID
                 MOVE "TC-USERID" TO D01-TEXT
                 MOVE OTC-USERID  TO D01-A15
                 MOVE TC-USERID   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OTC-POSTDATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = TC-POSTDATE
              MOVE "TC-POSTDATE" TO D01-TEXT
              MOVE OTC-POSTDATE  TO D01-A15
              MOVE TC-POSTDATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF ENT-GROUP NOT = 'S351'
              IF TC-POSTTIME NOT = OTC-POSTTIME
                 MOVE "TC-POSTTIME" TO D01-TEXT
                 MOVE OTC-POSTTIME  TO D01-A15
                 MOVE TC-POSTTIME   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM SET-HOLD-TILLNO.
           IF TC-TILLNO  NOT = HOLD-LN-TILLNO
              MOVE "TC-TILLNO" TO D01-TEXT
              MOVE OTC-TILLNO  TO D01-A15
              MOVE TC-TILLNO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TC-ADR1  NOT = OTC-ADR1
              MOVE "TC-ADR1" TO D01-TEXT
              MOVE OTC-ADR1  TO D01-A15
              MOVE TC-ADR1   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.
 
           IF TC-ACH-FG  NOT = OTC-ACH-FG
              MOVE "TC-ACH-FG" TO D01-TEXT
              MOVE OTC-ACH-FG  TO D01-A15
              MOVE TC-ACH-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TC-EXTRACT-FG  NOT = OTC-EXTRACTED-FG
              MOVE "TC-EXTRACT-FG"  TO D01-TEXT
              MOVE OTC-EXTRACTED-FG TO D01-A15
              MOVE TC-EXTRACT-FG    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TC-DEBIT-REFCD  NOT = OTC-DEBIT-REFCD
              MOVE "TC-DEBIT-REFCD" TO D01-TEXT
              MOVE OTC-DEBIT-REFCD  TO D01-A15
              MOVE TC-DEBIT-REFCD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

       EXIT-COMPARE-FIELDS.
           EXIT.

       WRITE-DETAIL-LINE-ERROR-KEY SECTION.

           MOVE BR-NO         TO D01-BRNO.
           MOVE TC-KEY-DATE   TO D01-DATE.
           MOVE TC-CKNO       TO D01-CKNO.
           MOVE TC-CLASS      TO D01-CLASS.
           MOVE TC-SEQ        TO D01-SEQ.

       WRITE-DETAIL-LINE-ERROR SECTION.

           ADD 1    TO TOTAL-ERRORS-CTR.
           MOVE "Y" TO PREV-BRANCH-ERR-SW.
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
           MOVE PAGE-NUMBER                TO H01-PAGE.

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

           MOVE 2                          TO LN-FEED.

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
           MOVE EXT-CONAME-CONAME TO TRCFLE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO TRCFLE-SYSDATE.
           DISPLAY TRCFLE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE TRCFLE-FORM-FIELDS TO WR-BUF.
           MOVE TRCFLE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE TRCFLE-HELP--FILE TO HELP-LINK-FILE.
           MOVE TRCFLE-HELP--DIR  TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/TRCFLE_EVA.CPY".
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

       SET-HOLD-TILLNO.
           IF OTC-TILLNO = "1"
              MOVE 1  TO HOLD-LN-TILLNO 
           ELSE
           IF OTC-TILLNO = "2"
              MOVE 2  TO HOLD-LN-TILLNO 
           ELSE
           IF OTC-TILLNO = "3"
              MOVE 3  TO HOLD-LN-TILLNO 
           ELSE
           IF OTC-TILLNO = "4"
              MOVE 4  TO HOLD-LN-TILLNO 
           ELSE
           IF OTC-TILLNO = "5"
              MOVE 5  TO HOLD-LN-TILLNO 
           ELSE
           IF OTC-TILLNO = "6"
              MOVE 6  TO HOLD-LN-TILLNO 
           ELSE
           IF OTC-TILLNO = "7"
              MOVE 7  TO HOLD-LN-TILLNO 
           ELSE
           IF OTC-TILLNO = "8"
              MOVE 8  TO HOLD-LN-TILLNO 
           ELSE
           IF OTC-TILLNO = "9"
              MOVE 9  TO HOLD-LN-TILLNO 
           ELSE
           IF OTC-TILLNO = "A"
              MOVE 10 TO HOLD-LN-TILLNO 
           ELSE
           IF OTC-TILLNO = "B"
              MOVE 11 TO HOLD-LN-TILLNO 
           ELSE
           IF OTC-TILLNO = "C"
              MOVE 12 TO HOLD-LN-TILLNO 
           ELSE
           IF OTC-TILLNO = "D"
              MOVE 13 TO HOLD-LN-TILLNO 
           ELSE
           IF OTC-TILLNO = "E"
              MOVE 14 TO HOLD-LN-TILLNO 
           ELSE
           IF OTC-TILLNO = "F"
              MOVE 15 TO HOLD-LN-TILLNO 
           ELSE
           IF OTC-TILLNO = "G"
              MOVE 16 TO HOLD-LN-TILLNO 
           ELSE
           IF OTC-TILLNO = "H"
              MOVE 17 TO HOLD-LN-TILLNO 
           ELSE
           IF OTC-TILLNO = "I"
              MOVE 18 TO HOLD-LN-TILLNO 
           ELSE
           IF OTC-TILLNO = "J"
              MOVE 19 TO HOLD-LN-TILLNO 
           ELSE
           IF OTC-TILLNO = "K"
              MOVE 20 TO HOLD-LN-TILLNO 
           ELSE
           IF OTC-TILLNO = "L"
              MOVE 21 TO HOLD-LN-TILLNO 
           ELSE
           IF OTC-TILLNO = "M"
              MOVE 22 TO HOLD-LN-TILLNO 
           ELSE
           IF OTC-TILLNO = "N"
              MOVE 23 TO HOLD-LN-TILLNO 
           ELSE
           IF OTC-TILLNO = "O"
              MOVE 24 TO HOLD-LN-TILLNO 
           ELSE
           IF OTC-TILLNO = "P"
              MOVE 25 TO HOLD-LN-TILLNO 
           ELSE
           IF OTC-TILLNO = "Q"
              MOVE 26 TO HOLD-LN-TILLNO 
           ELSE
           IF OTC-TILLNO = "R"
              MOVE 27 TO HOLD-LN-TILLNO 
           ELSE
           IF OTC-TILLNO = "S"
              MOVE 28 TO HOLD-LN-TILLNO 
           ELSE
           IF OTC-TILLNO = "T"
              MOVE 29 TO HOLD-LN-TILLNO 
           ELSE
           IF OTC-TILLNO = "U"
              MOVE 30 TO HOLD-LN-TILLNO 
           ELSE
           IF OTC-TILLNO = "V"
              MOVE 31 TO HOLD-LN-TILLNO 
           ELSE
           IF OTC-TILLNO = "W"
              MOVE 32 TO HOLD-LN-TILLNO 
           ELSE
           IF OTC-TILLNO = "X"
              MOVE 33 TO HOLD-LN-TILLNO 
           ELSE
           IF OTC-TILLNO = "Y"
              MOVE 34 TO HOLD-LN-TILLNO 
           ELSE
           IF OTC-TILLNO = "Z"
              MOVE 35 TO HOLD-LN-TILLNO
           ELSE
              MOVE 0 TO HOLD-LN-TILLNO.

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
       COPY "LIBLP/LPTRCGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

      * COPY "LIBLP/LPTRC1RN.CPY".
      * COPYMEMBER: LIBLP/LPTR1RN
       LOAD-TRC1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRC-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRC-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRC-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRC-ALL-DATA
              MOVE TRC-ALL-PATH             TO TRC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRC-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRC-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRC-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRC-ALL-DATA
              MOVE TRC-ALL-PATH             TO TRC-PATH.

      *-----------------------------------------------------------------
       OPEN-TRC1-FILE.
           PERFORM LOAD-TRC1-FILE.
           PERFORM OPEN-IT.
           MOVE TRC-PATH-SQL TO E-FILE.

           IF TRC1-CONN-OPEN-SW = "N" 
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS TRC1_CONNECT
                        USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE 'Y' TO TRC1-CONN-OPEN-SW.

      *-----------------------------------------------------------------
       START-TRC1-FILE.  *> SSP TRCFILE
           PERFORM START-IT.
           MOVE TRC-PATH-OWNBR  TO TC-BRNO.
           MOVE TRC-PATH-SQL    TO E-FILE.
           MOVE TRC1-KEY        TO E-KEYX.

           IF ( TRC1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRC1-FILE.

           EXEC SQL SET CONNECTION TRC1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE TRC1_CURSOR CURSOR FOR
                :TRC1-RETURN-CODE =  
            EXEC SSP_TRCFLE_TRCFILE_SELECT
              ( :QTRC1-WBEG-BRNO,
                :QTRC1-WBEG-KEY-DATE, :QTRC1-WEND-KEY-DATE)
           END-EXEC.

      *     EXEC SQL
      *      DECLARE TRC1_CURSOR CURSOR FOR
      *     SELECT *
      *     FROM DBO.TRCFILE TRCFILE
      *     WHERE TRCFILE.TC_BRNO = @QTRC1_WBEG_BRNO
      *     AND TRCFILE.TC_DATE  BETWEEN @QTRC1_WBEG_DATE  AND @QTRC1_WEND_DATE
      *     ORDER BY TRCFILE.TC_KEY_DATE,TRCFILE.TC_CKNO,, TRCFILE.TC_CLASS,
      *             TRCFILE.TC_SEQ
      *     END-EXEC.

           EXEC SQL
               OPEN TRC1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE STAT---GOOD TO TRC1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-TRC1-FILE-NEXT.   *> SSP TRCFILE
           PERFORM READ-IT.
           MOVE TRC-PATH-OWNBR  TO TC-BRNO.
           MOVE TRC-PATH-SQL    TO E-FILE.
           MOVE TRC1-KEY        TO E-KEYX.
           INITIALIZE QTRC-REC.

           EXEC SQL SET CONNECTION TRC1_CONNECT END-EXEC.

           IF ( TRC1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TRC1_CURSOR INTO :QTRC-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRC1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRC-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TRC1-FILE.

           PERFORM CLOSE-IT.
           IF ( TRC1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION TRC1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE TRC1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRC1-CURSOR-STAT
              EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TRC1-CONNECTION.

           IF TRC1-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION TRC1_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT TRC1_CONNECT END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE "N" TO TRC1-CONN-OPEN-SW.
      *-----------------------------------------------------------------


       OPEN-OTRC1-FILE.
           PERFORM OPEN-IT.
           MOVE OTR-PATH TO E-FILE.
           OPEN INPUT OTR-FILE.
           IF IO-FG = 8
              GO TO OPEN-OTRC1-FILE.
           IF IO-FG = 7
              CLOSE OTR-FILE
              GO TO OPEN-OTRC1-FILE.

       START-OTRC1-FILE.
           PERFORM START-IT.
           MOVE OTR-PATH TO E-FILE.
           MOVE OTR1-KEY TO E-KEYX.
           START OTR-FILE KEY NOT < OTR1-KEY.

       READ-OTRC1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OTR-PATH TO E-FILE.
           MOVE OTR1-KEY TO E-KEYX.
           READ OTR-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OTRC1-FILE-NEXT.

       CLOSE-OTRC1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OTR-FILE.

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

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY TRCFLE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
