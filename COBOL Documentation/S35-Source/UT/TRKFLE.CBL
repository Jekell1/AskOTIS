       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TRKFLE.
       DATE-WRITTEN.  01-26-2024.
       AUTHOR. MARK BOOMGAARD.
      ******************************************************************
      *
      * DESCRIPTION   : COMPARE S35 TRKFILE WITH A15 TRKFILE
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

       COPY "LIBUP/A15SRC/LPFDTRK.CPY" REPLACING LEADING "TK" BY "OTK".

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/TRKFLE S35 01/24/24-14:02:09 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01TRK.CPY".
       COPY "LIBLP/LP01TRK_SQL.CPY".
       COPY "LIBLP/LPWSTRK.CPY".
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

       01  TRK-EOF-SW                  PIC X      VALUE "N".
       01  OTRK-EOF-SW                 PIC X      VALUE "N".
       01  PREV-BRANCH-ERR-SW          PIC X      VALUE "N".
       01  TOTAL-ERRORS-CTR            PIC 9(9)   VALUE 0.
       01  SUB1                        PIC 9      VALUE 0.
       01  A15-FILE-FOUND-SW           PIC X      VALUE "N".
       01  PROCESS-CODE                PIC X      VALUE "K".

       01  TEST-ACCTNO                 PIC 9(8).
       01  FILLER REDEFINES TEST-ACCTNO.
             03  TEST-NUMBER           PIC 9(6).
             03  TEST-SEQ              PIC 9(2).    

       01  OTRK-USE-KEY.
           03 OTRK-USE-KEY-BRNO        PIC 9(4).
           03 OTRK-USE-KEY-DATE.
             05 OTRK-USE-KEY-DATE-CN    PIC 9(2).
             05 OTRK-USE-KEY-DATE-YY    PIC 9(2).
             05 OTRK-USE-KEY-DATE-MM    PIC 9(2).
             05 OTRK-USE-KEY-DATE-DD    PIC 9(2).
           03 OTRK-USE-KEY-CLASS        PIC 9(3).
           03 OTRK-USE-KEY-ACCT         PIC 9(8).
           03 OTRK-USE-KEY-LAWCODE      PIC 9(2).
           03 OTRK-USE-KEY-LINE         PIC 9(3).

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
           "TR ACCOUNT CONVERSIONS (TRKFILE) COMPARE A15 TO S35".

       01  HEADER-LINE-03.
           03  FILLER                PIC X(4)  VALUE "BRNO".
           03  FILLER                PIC X(2)  VALUE SPACES.
           03  FILLER                PIC X(8)  VALUE "  DATE  ".
           03  FILLER                PIC X(2)  VALUE SPACES.
           03  FILLER                PIC X(4)  VALUE "CLS ".
           03  FILLER                PIC X(2)  VALUE SPACES.
           03  FILLER                PIC X(7)  VALUE " ACCT  ".
           03  FILLER                PIC X(2)  VALUE SPACES.
           03  FILLER                PIC X(4)  VALUE "LWCD".
           03  FILLER                PIC X(3)  VALUE SPACES.
           03  FILLER                PIC X(4)  VALUE "LINE".
           03  FILLER                PIC X(2)  VALUE SPACES.
           03  FILLER                PIC X(11) VALUE "DESCRIPTION".
           03  FILLER                PIC X(12) VALUE SPACES.
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
           03  D01-CLASS                      PIC 9(3).
           03  FILLER                         PIC X(2).
           03  D01-ACCT                       PIC 9(8).
           03  FILLER                         PIC X(3).
           03  D01-LAWCODE                    PIC 9(2).
           03  FILLER                         PIC X(5).
           03  D01-LINE                       PIC ZZ9.
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
       COPY "LIBUT/TRKFLE_DEF.CPY".
       COPY "LIBUT/TRKFLE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/TRKFLE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE OTR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TRK1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
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
           MOVE "TRKFLE" TO VDU-FORM-NAME.

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

           MOVE TRKFLE-QUEUE-POS TO Q-POS.
           MOVE TRKFLE-COPIES-POS TO C-POS.
           MOVE TRKFLE-PRU-POS   TO P-POS.
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

      *-----------------------------------------------------------------
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

      *-----------------------------------------------------------------
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

           MOVE "B"                        TO TRK-PATH-OVERRIDE.

           MOVE BR-NO TO QTRK1-WBEG-BRNO QTRK1-WEND-BRNO.
           MOVE ZEROS TO QTRK1-WBEG-CLASS 
                         QTRK1-WBEG-ACCTNO
                         QTRK1-WBEG-LAWCODE
                         QTRK1-WBEG-LINE.
           MOVE ALL '9' TO QTRK1-WEND-CLASS
                           QTRK1-WEND-ACCTNO
                           QTRK1-WEND-LAWCODE
                           QTRK1-WEND-LINE.

           MOVE BEG-BY-DATE-CCYYMMDD TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QTRK1-WBEG-DATE.

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
           MOVE SQL-DATE-YYYY-MM-DD  TO QTRK1-WEND-DATE.

           MOVE "N"  TO TRK-EOF-SW.
           PERFORM OPEN-TRK1-FILE.
           PERFORM START-TRK1-FILE.
           PERFORM M-READ-TRK1.

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
                 PERFORM OPEN-OTRK1-FILE
                 MOVE LOW-VALUES   TO OTR1-KEY
                 MOVE 01           TO OTR1-DAY
                 MOVE BR-NO        TO OTR1-BRNO
                 MOVE PROCESS-CODE TO OTR1-CODE
                 MOVE "N"          TO OTRK-EOF-SW

                 MOVE BR-NO        TO OTRK-USE-KEY-BRNO
                 IF PROC-YYMM-YY > 80
                    MOVE 19 TO OTRK-USE-KEY-DATE-CN
                 ELSE 
                    MOVE 20 TO OTRK-USE-KEY-DATE-CN
                 END-IF
                 MOVE PROC-YYMM-YY TO OTRK-USE-KEY-DATE-YY
                 MOVE PROC-YYMM-MM TO OTRK-USE-KEY-DATE-MM
                 MOVE 0            TO OTRK-USE-KEY-DATE-DD

                 PERFORM START-OTRK1-FILE
                 PERFORM M-READ-OTRK

                 PERFORM PROCESS-BRANCH-NEXT-TRK
                    UNTIL OTRK-EOF-SW = "Y"
                 PERFORM CLOSE-OTRK1-FILE
              END-IF

              IF PROC-YYMM-MM = 12
                 ADD  1 TO PROC-YYMM-YY
                 MOVE 0 TO PROC-YYMM-MM
              END-IF

           END-PERFORM.

      * CHECK FOR MORE SQL RECORDS
           PERFORM PROCESS-BRANCH-NEXT-TRK
              UNTIL TRK-EOF-SW = "Y"

           PERFORM CLOSE-TRK1-FILE.

           GO TO PROCESS-BRANCH-DRIVER-END.

       PROCESS-BRANCH-NEXT-TRK.

           PERFORM SEND-PROGRESS-INDICATOR.

           PERFORM COMPARE-FIELDS.

           IF OTRK-USE-KEY < TRK1-KEY
              PERFORM M-READ-OTRK 
           ELSE
              IF OTRK-USE-KEY > TRK1-KEY
                 PERFORM M-READ-TRK1
              ELSE
                 PERFORM M-READ-OTRK
                 PERFORM M-READ-TRK1.

       PROCESS-BRANCH-DRIVER-END.
           EXIT. 

       M-READ-TRK1 SECTION.
           PERFORM READ-TRK1-FILE-NEXT.
           IF IO-BAD
              MOVE ALL '9' TO TRK1-KEY
              MOVE "Y"     TO TRK-EOF-SW
           ELSE
              ADD 1 TO SQL-RECORDS-PROCESSED.

       EXIT-M-READ-TRK1.
           EXIT.

       M-READ-OTRK SECTION.
           PERFORM READ-OTRK1-FILE-NEXT.
           IF IO-BAD
              MOVE ALL '9'  TO OTRK-USE-KEY
              MOVE "Y"      TO OTRK-EOF-SW
           ELSE
              IF OTK-CODE NOT = PROCESS-CODE 
                 GO TO M-READ-OTRK
              ELSE
                 ADD 1            TO A15-RECORDS-PROCESSED
                 MOVE OTK-DAY     TO OTRK-USE-KEY-DATE-DD
                 MOVE OTK-CLASS   TO OTRK-USE-KEY-CLASS
                 MOVE OTK-NUMBER  TO OTRK-USE-KEY-ACCT
                 MOVE OTK-LAWCODE TO OTRK-USE-KEY-LAWCODE
                 MOVE OTK-LINE    TO OTRK-USE-KEY-LINE.

       EXIT-M-READ-OTRK.
           EXIT.

      ******************************************************************
       COMPARE-FIELDS SECTION.
      ******************************************************************

           IF OTRK-USE-KEY < TRK1-KEY
              MOVE "MISSING S35 RECORD" TO D01-TEXT
              MOVE BR-NO                TO D01-BRNO
              MOVE OTRK-USE-KEY-DATE    TO D01-DATE
              MOVE OTRK-USE-KEY-CLASS   TO D01-CLASS
              MOVE OTRK-USE-KEY-ACCT    TO D01-ACCT
              MOVE OTRK-USE-KEY-LAWCODE TO D01-LAWCODE
              MOVE OTRK-USE-KEY-LINE    TO D01-LINE
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.

           IF OTRK-USE-KEY > TRK1-KEY
              MOVE "MISSING A15 RECORD" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.

           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 4
              IF TK-SSNO(SUB1) NOT = OTK-SSNO(SUB1)
                 STRING "TK-SSNO(",SUB1,")" INTO D01-TEXT
                 MOVE OTK-SSNO(SUB1) TO D01-A15 
                 MOVE TK-SSNO(SUB1)  TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.

           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 4
              IF TK-MAKERCD(SUB1) NOT = OTK-MAKERCD(SUB1)
                 STRING "TK-MAKERCD(",SUB1,")" INTO D01-TEXT
                 MOVE OTK-MAKERCD(SUB1) TO D01-A15
                 MOVE TK-MAKERCD(SUB1)  TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.

           IF TK-REFNO NOT = OTK-REFNO
              MOVE "TK-REFNO" TO D01-TEXT
              MOVE OTK-REFNO  TO D01-A15
              MOVE TK-REFNO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TK-PURCD NOT = OTK-PURCD
              MOVE "TK-PURCD" TO D01-TEXT
              MOVE OTK-PURCD  TO D01-A15
              MOVE TK-PURCD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TK-SRCD NOT = OTK-SRCD
              MOVE "TK-SRCD" TO D01-TEXT
              MOVE OTK-SRCD  TO D01-A15
              MOVE TK-SRCD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TK-COLLCD NOT = OTK-COLLCD
              MOVE "TK-COLLCD" TO D01-TEXT
              MOVE OTK-COLLCD  TO D01-A15-NUM
              MOVE TK-COLLCD   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TK-SERVACCT NOT = OTK-SERVACCT
              MOVE "TK-SERVACCT" TO D01-TEXT
              MOVE OTK-SERVACCT  TO D01-A15
              MOVE TK-SERVACCT   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TK-LOANTYPE NOT = OTK-LOANTYPE
              MOVE "TK-LOANTYPE" TO D01-TEXT
              MOVE OTK-LOANTYPE  TO D01-A15
              MOVE TK-LOANTYPE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TK-REPOCD NOT = OTK-REPOCD
              MOVE "TK-REPOCD" TO D01-TEXT
              MOVE OTK-REPOCD  TO D01-A15
              MOVE TK-REPOCD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TK-PLCD NOT = OTK-PLCD
              MOVE "TK-PLCD" TO D01-TEXT
              MOVE OTK-PLCD  TO D01-A15
              MOVE TK-PLCD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TK-PLTYPE NOT = OTK-PLTYPE
              MOVE "TK-PLTYPE" TO D01-TEXT
              MOVE OTK-PLTYPE  TO D01-A15
              MOVE TK-PLTYPE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OTK-LOANDATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = TK-LOANDATE
              MOVE "TK-LOANDATE" TO D01-TEXT
              MOVE OTK-LOANDATE  TO D01-A15
              MOVE TK-LOANDATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TK-PROCEEDS NOT = OTK-PROCEEDS
              MOVE "TK-PROCEEDS" TO D01-TEXT
              MOVE OTK-PROCEEDS  TO D01-A15-NUM
              MOVE TK-PROCEEDS   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TK-LNAMT NOT = OTK-LNAMT
              MOVE "TK-LNAMT" TO D01-TEXT
              MOVE OTK-LNAMT  TO D01-A15-NUM
              MOVE TK-LNAMT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TK-FINCHG NOT = OTK-FINCHG
              MOVE "TK-FINCHG" TO D01-TEXT
              MOVE OTK-FINCHG  TO D01-A15-NUM
              MOVE TK-FINCHG   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TK-INTCHG NOT = OTK-INTCHG
              MOVE "TK-INTCHG" TO D01-TEXT
              MOVE OTK-INTCHG  TO D01-A15-NUM
              MOVE TK-INTCHG   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TK-SERCHG NOT = OTK-SERCHG
              MOVE "TK-SERCHG" TO D01-TEXT
              MOVE OTK-SERCHG  TO D01-A15-NUM
              MOVE TK-SERCHG   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TK-EXTCHG NOT = OTK-EXTCHG
              MOVE "TK-EXTCHG" TO D01-TEXT
              MOVE OTK-EXTCHG  TO D01-A15-NUM
              MOVE TK-EXTCHG   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TK-MAINTFEE NOT = OTK-MAINTFEE
              MOVE "TK-MAINTFEE" TO D01-TEXT
              MOVE OTK-MAINTFEE  TO D01-A15-NUM
              MOVE TK-MAINTFEE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TK-ORGTERM NOT = OTK-ORGTERM
              MOVE "TK-ORGTERM" TO D01-TEXT
              MOVE OTK-ORGTERM  TO D01-A15
              MOVE TK-ORGTERM   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TK-INSURED NOT = OTK-INSURED
              MOVE "TK-INSURED" TO D01-TEXT
              MOVE OTK-INSURED  TO D01-A15
              MOVE TK-INSURED   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TK-DLNO NOT = OTK-DLNO
              MOVE "TK-DLNO" TO D01-TEXT
              MOVE OTK-DLNO  TO D01-A15-NUM
              MOVE TK-DLNO   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TK-DLRV NOT = OTK-DLRV
              MOVE "TK-DLRV" TO D01-TEXT
              MOVE OTK-DLRV  TO D01-A15-NUM
              MOVE TK-DLRV   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TK-DLSPRV NOT = OTK-DLSPRV
              MOVE "TK-DLSPRV" TO D01-TEXT
              MOVE OTK-DLSPRV  TO D01-A15-NUM
              MOVE TK-DLSPRV   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TK-DLDISC NOT = OTK-DLDISC
              MOVE "TK-DLDISC" TO D01-TEXT
              MOVE OTK-DLDISC  TO D01-A15-NUM
              MOVE TK-DLDISC   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TK-DLPARV NOT = OTK-DLPARV
              MOVE "TK-DLPARV" TO D01-TEXT
              MOVE OTK-DLPARV  TO D01-A15-NUM
              MOVE TK-DLPARV   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TK-DLRECOURSE NOT = OTK-DLRECOURSE
              MOVE "TK-DLRECOURSE" TO D01-TEXT
              MOVE OTK-DLRECOURSE  TO D01-A15
              MOVE TK-DLRECOURSE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TK-DLPARTRCAMT NOT = OTK-DLPARTRCAMT
              MOVE "TK-DLPARTRCAMT" TO D01-TEXT
              MOVE OTK-DLPARTRCAMT  TO D01-A15-NUM
              MOVE TK-DLPARTRCAMT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TK-PURBAL NOT = OTK-PURBAL
              MOVE "TK-PURBAL" TO D01-TEXT
              MOVE OTK-PURBAL  TO D01-A15-NUM
              MOVE TK-PURBAL   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 6
              IF TK-UNEARN(SUB1) NOT = OTK-UNEARN(SUB1)
                 STRING "TK-UNEARN(",SUB1,")" INTO D01-TEXT
                 MOVE OTK-UNEARN(SUB1)  TO D01-A15-NUM
                 MOVE TK-UNEARN(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.

           IF TK-PRLNNO NOT = OTK-PRLNNO
              MOVE "TK-PRLNNO" TO D01-TEXT
              MOVE OTK-PRLNNO  TO D01-A15
              MOVE TK-PRLNNO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 3
              IF TK-DLRVPRIN(SUB1) NOT = OTK-DLRVPRIN(SUB1)
                 STRING "TK-DLRVPRIN(",SUB1,")" INTO D01-TEXT
                 MOVE OTK-DLRVPRIN(SUB1)  TO D01-A15-NUM
                 MOVE TK-DLRVPRIN(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.

           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 3
              IF TK-DLRVCHGS(SUB1) NOT = OTK-DLRVCHGS(SUB1)
                 STRING "TK-DLRVCHGS(",SUB1,")" INTO D01-TEXT
                 MOVE OTK-DLRVCHGS(SUB1)  TO D01-A15-NUM
                 MOVE TK-DLRVCHGS(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.
 
           IF TK-DLPARVPAID NOT = OTK-DLPARVPAID
              MOVE "TK-DLPARVPAID" TO D01-TEXT
              MOVE OTK-DLPARVPAID  TO D01-A15-NUM
              MOVE TK-DLPARVPAID   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TK-DLPARVHELD NOT = OTK-DLPARVHELD
              MOVE "TK-DLPARVHELD" TO D01-TEXT
              MOVE OTK-DLPARVHELD  TO D01-A15-NUM
              MOVE TK-DLPARVHELD  TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TK-USERID NOT = OTK-USERID
              MOVE "TK-USERID" TO D01-TEXT
              MOVE OTK-USERID  TO D01-A15
              MOVE TK-USERID   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OTK-POSTDATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = TK-POSTDATE
              MOVE "TK-POSTDATE" TO D01-TEXT
              MOVE OTK-POSTDATE  TO D01-A15
              MOVE TK-POSTDATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TK-POSTTIME NOT = OTK-POSTTIME
              MOVE "TK-POSTTIME" TO D01-TEXT
              MOVE OTK-POSTTIME  TO D01-A15
              MOVE TK-POSTTIME   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 6
              IF TK-EARNED(SUB1) NOT = OTK-EARNED(SUB1)
                 STRING "TK-EARNED(",SUB1,")" INTO D01-TEXT
                 MOVE OTK-EARNED(SUB1)  TO D01-A15-NUM
                 MOVE TK-EARNED(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.

       EXIT-COMPARE-FIELDS.
           EXIT.

       WRITE-DETAIL-LINE-ERROR-KEY SECTION.

           MOVE BR-NO         TO D01-BRNO.
           MOVE TK-DATE       TO D01-DATE.
           MOVE TK-CLASS      TO D01-CLASS.
           MOVE TK-ACCTNO     TO D01-ACCT.
           MOVE TK-LAWCODE    TO D01-LAWCODE.
           MOVE TK-LINE       TO D01-LINE.

       WRITE-DETAIL-LINE-ERROR SECTION.

           ADD 1    TO TOTAL-ERRORS-CTR.
           MOVE "Y" TO PREV-BRANCH-ERR-SW.
           PERFORM WRITE-DETAIL-LINE.

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
           MOVE EXT-CONAME-CONAME TO TRKFLE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO TRKFLE-SYSDATE.
           DISPLAY TRKFLE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE TRKFLE-FORM-FIELDS TO WR-BUF.
           MOVE TRKFLE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE TRKFLE-HELP--FILE TO HELP-LINK-FILE.
           MOVE TRKFLE-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/TRKFLE_EVA.CPY".
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
       COPY "LIBLP/LPTRKGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPTRK1RN.CPY".

       OPEN-OTRK1-FILE.
           PERFORM OPEN-IT.
           MOVE OTR-PATH TO E-FILE.
           OPEN INPUT OTR-FILE.
           IF IO-FG = 8
              GO TO OPEN-OTRK1-FILE.
           IF IO-FG = 7
              CLOSE OTR-FILE
              GO TO OPEN-OTRK1-FILE.

       START-OTRK1-FILE.
           PERFORM START-IT.
           MOVE OTR-PATH TO E-FILE.
           MOVE OTR1-KEY TO E-KEYX.
           START OTR-FILE KEY NOT < OTR1-KEY.

       READ-OTRK1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OTR-PATH TO E-FILE.
           MOVE OTR1-KEY TO E-KEYX.
           READ OTR-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OTRK1-FILE-NEXT.

       CLOSE-OTRK1-FILE.
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
              DESTROY TRKFLE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
