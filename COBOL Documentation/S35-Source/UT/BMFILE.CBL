       IDENTIFICATION DIVISION.
       PROGRAM-ID.    BMFILE.
       DATE-WRITTEN.  11-01-2023.
      ******************************************************************
      *
      * DESCRIPTION   : COMPARE S35 BMFILE WITH A15 BMFILE
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
      *  01-07-25  RMZ  ADDED 'BTRACK' - PROCESS TRACKING TO PROGRAM.
      *  05-01-25  MB   MISC REPORT CHANGES FOR MISSING RECS
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
      * COPYMEMBER: LIBLP/LPFSBM
           SELECT OBM-FILE ASSIGN TO OBM-PATH
                  WITH COMPRESSION
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OBM1-KEY
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  OBM-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A15SRC/LP01BM.CPY" REPLACING LEADING "BM" BY "OBM".

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/BMFILE S35 11/01/23-14:41:26 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01BM.CPY".
       COPY "LIBLP/LP01BM_SQL.CPY".
       COPY "LIBLP/LPWSBM.CPY".
       01  OBM-PATH                    PIC X(35) VALUE SPACES.
       01  OBM-ALL-PATH.
           03  OBM-ALL-BASE     PIC X(9).
           03  FILLER           PIC X     VALUE "/".
           03  OBM-ALL-MACHINE  PIC XX.
           03  FILLER           PIC X     VALUE "/".
           03  OBM-ALL-DATA     PIC X(6).
           03  FILLER           PIC X(10) VALUE
               "/LP/BMFILE".

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

       01  BM1-CONN-OPEN-SW     PIC X        VALUE "N".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************

       01  BM-EOF-SW                   PIC X      VALUE "N".
       01  OBM-EOF-SW                  PIC X      VALUE "N".
 
       01  WS-OBM1-KEY.
           03  WS-OBM-OWNBR            PIC 9(4).
           03  WS-OBM-SSNO             PIC 9(9).
           03  WS-OBM-DATE-REVERSED    PIC 9(8).
           03  WS-OBM-SEQ              PIC 9(3).

       01  WS-BM1-KEY.
           03  WS-BM-OWNBR             PIC 9(4).
           03  WS-BM-SSNO              PIC 9(9).
           03  WS-BM-DATE-REVERSED     PIC 9(8).
           03  WS-BM-SEQ               PIC 9(3).

       01  S35-RECORD-CTR              PIC 9(10)     VALUE ZEROES.
       01  TOTAL-ERRORS-CTR            PIC 9(10)     VALUE ZEROES. 

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

       01  MISC-CURSOR-STAT            PIC X(2) VALUE SPACES.
           88  MISC-CURSOR-STAT-GOOD            VALUE "00".
           88  MISC-CURSOR-STAT-BAD             VALUE "  " THRU "//"
                                                      "01" THRU "~~".

       01  PROCESS-STAT                PIC X(08)    VALUE SPACES.
           88  PROCESS-STAT-GOOD                    VALUE "00".
           88  PROCESS-STAT-BAD                     VALUE "  " THRU "//"
                                                         "01" THRU "~~".
       01  A15-RECORDS-PROCESSED       PIC 9(9)  VALUE 0.
           88  NO-RECORDS-FOUND                  VALUE ZEROES.

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
       01  LEGEND-BEG-SSNO.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F7-EXIT, ENT".
           03  FILLER PIC X(30) VALUE "ER BEGINNING SSNO             ".
           03  FILLER PIC X(19) VALUE "                   ".

       01  LEGEND-END-SSNO.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER ENDIN".
           03  FILLER PIC X(30) VALUE "G SSNO                        ".
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
           03  BEG-SSNO                PIC 9(09).
           03  END-SSNO                PIC 9(09).
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
           03  H02-TITLE               PIC X(50) VALUE 
           "BORROWER MEMO FILE (BMFILE) COMPARE A15 TO S35".

       01  HEADER-LINE-03.
           03  FILLER                PIC X(4) VALUE "KEY".
           03  FILLER                PIC X(24) VALUE SPACES.
           03  FILLER                PIC X(11) VALUE "DESCRIPTION".
           03  FILLER                PIC X(17) VALUE SPACES.
           03  FILLER                PIC X(3) VALUE "A15".
           03  FILLER                PIC X(19) VALUE SPACES.
           03  FILLER                PIC X(3) VALUE "S35".

       01  DETAIL-LINE                        PIC X(133)   VALUE SPACES.
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-KEY                        PIC X(24).
           03  FILLER                         PIC X(4).
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
       COPY "LIBUT/BMFILE_DEF.CPY".
       COPY "LIBUT/BMFILE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/BMFILE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE OBM-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-BM1-FILE.
           PERFORM CLOSE-BM1-CONNECTION.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE OBM-FILE.
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
           MOVE "BMFILE" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE BMFILE-QUEUE-POS TO Q-POS.
           MOVE BMFILE-COPIES-POS TO C-POS.
           MOVE BMFILE-PRU-POS   TO P-POS.
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
              GO TO ENTER-END-SSNO.

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


       ENTER-BEG-SSNO.

           MOVE LEGEND-BEG-SSNO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN090BEG-SSNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F3-KEY ) GO TO ENTER-ALL-SSNO.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-SSNO.

           MOVE VDUNUM0 TO BEG-SSNO.

      *-----------------------------------------------------------------
       ENTER-END-SSNO.

           MOVE LEGEND-END-SSNO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN090END-SSNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-SSNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-SSNO.

           MOVE VDUNUM0 TO END-SSNO.

           IF ( BEG-SSNO > END-SSNO )
              MOVE "INVALID SSNO RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-SSNO.

           GO TO ENTER-PARAMETERS-END.

      *-----------------------------------------------------------------
       ENTER-ALL-SSNO.

           MOVE "SNNN090BEG-SSNO."  TO VDU.
           MOVE ZEROES                TO BEG-SSNO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN090END-SSNO."  TO VDU.
           MOVE ALL "9"             TO END-SSNO VDUNUM0.
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

           MOVE "SNNN060BEG-SSNO."       TO VDU.
           MOVE BEG-SSNO                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060END-SSNO."       TO VDU.
           MOVE END-SSNO                 TO VDUNUM0.
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
           MOVE SOURCE-PATH  TO OBM-ALL-BASE.
           MOVE MACHINE-R1   TO OBM-ALL-MACHINE.
           MOVE BR-DATA-PATH TO OBM-ALL-DATA.
           MOVE OBM-ALL-PATH TO OBM-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              IF OBM-ALL-MACHINE = MACHINE-R1
                 MOVE MACHINE-R2 TO OBM-ALL-MACHINE
                 MOVE OBM-ALL-PATH TO OBM-PATH ACCESS-BUF
                 PERFORM ACCESS-CALL
                 IF ( STAT-BAD )
                    MOVE "MISSING A15 FILE" TO D01-TEXT
                    MOVE SPACES             TO D01-KEY
                    MOVE OBM-ALL-PATH       TO D01-A15
                    MOVE SPACES             TO D01-S35
                    PERFORM WRITE-DETAIL-LINE-ERROR
                    GO TO PROCESS-BRANCH-DRIVER-END.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BM-PATH-OVERRIDE.
           MOVE "N"                        TO OBM-EOF-SW
                                              BM-EOF-SW.

           PERFORM OPEN-BM1-FILE.
           PERFORM OPEN-OBM1-FILE.

           MOVE BEG-SSNO      TO OBM-SSNO.
           MOVE 0             TO OBM-DATE.
           MOVE 0             TO OBM-SEQ.

           PERFORM START-OBM1-FILE.

           MOVE BM-PATH-OWNBR        TO QBM1-WBEG-BRNO.     
           MOVE BEG-SSNO             TO QBM1-WBEG-SSNO.   
           MOVE END-SSNO             TO QBM1-WEND-SSNO.
           MOVE ALL "0"              TO QBM1-WBEG-DATE-REV.
           MOVE ALL "9"              TO QBM1-WEND-DATE-REV.
           MOVE ALL "0"              TO QBM1-WBEG-SEQ.
           MOVE ALL "9"              TO QBM1-WEND-SEQ.

           PERFORM START-BM1-FILE.

           PERFORM PROCESS-READ-OBM1.
           PERFORM PROCESS-READ-BM1.

       PROCESS-BRANCH-NEXT-BM.

           IF BM-EOF-SW  = "Y" AND
              OBM-EOF-SW = "Y"
              GO TO PROCESS-BRANCH-DRIVER-END.

           IF WS-OBM1-KEY < WS-BM1-KEY 
              IF OBM-DATE NOT = 99999999
                 MOVE "MISSING S35 RECORD" TO D01-TEXT
                 MOVE WS-OBM1-KEY          TO D01-KEY 
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              PERFORM PROCESS-READ-OBM1 
              GO TO PROCESS-BRANCH-NEXT-BM.
 
           IF WS-BM1-KEY < WS-OBM1-KEY
              MOVE "MISSING A15 RECORD" TO D01-TEXT
              MOVE WS-BM1-KEY           TO D01-KEY 
              PERFORM WRITE-DETAIL-LINE-ERROR
              PERFORM PROCESS-READ-BM1 
              GO TO PROCESS-BRANCH-NEXT-BM.

           PERFORM COMPARE-FIELDS.

           PERFORM PROCESS-READ-OBM1.  
           PERFORM PROCESS-READ-BM1.

           GO TO PROCESS-BRANCH-NEXT-BM.

       PROCESS-BRANCH-DRIVER-END.

           PERFORM CLOSE-BM1-FILE.

       PROCESS-BRANCH-DRIVER-EXIT.
           EXIT.


      ******************************************************************
       PROCESS-READ-OBM1 SECTION.
      ******************************************************************

           PERFORM READ-OBM1-FILE-NEXT.
           IF IO-BAD 
              MOVE 9999              TO WS-OBM-OWNBR
              MOVE 999999999         TO WS-OBM-SSNO              
              MOVE 99999999          TO WS-OBM-DATE-REVERSED 
              MOVE 999               TO WS-OBM-SEQ           
              MOVE "Y"               TO OBM-EOF-SW
              GO TO EXIT-PROCESS-READ-OBM1
           ELSE 
              MOVE BM-PATH-OWNBR     TO WS-OBM-OWNBR
              MOVE OBM-SSNO          TO WS-OBM-SSNO
              MOVE OBM-DATE          TO WS-OBM-DATE-REVERSED 
      *        MOVE OBM-DATE-REVERSED TO WS-OBM-DATE-REVERSED 
              MOVE OBM-SEQ           TO WS-OBM-SEQ.  

           PERFORM SEND-PROGRESS-INDICATOR.

           IF ( OBM-SSNO < BEG-SSNO ) OR
              ( OBM-SSNO > END-SSNO )
              GO TO PROCESS-READ-OBM1.

      * 99999999 1-10 ARE BORROWER CONTACTS, IN S35 THEY GO INTO BWFILE
           IF OBM-DATE = 99999999 AND OBM-SEQ < 11
              GO TO PROCESS-READ-OBM1.

           ADD  01 TO A15-RECORDS-PROCESSED.

       EXIT-PROCESS-READ-OBM1.
           EXIT.

      ******************************************************************
       PROCESS-READ-BM1 SECTION.
      ******************************************************************

           PERFORM READ-BM1-FILE-NEXT.
           IF IO-BAD 
              MOVE 9999              TO WS-BM-OWNBR
              MOVE 999999999         TO WS-BM-SSNO
              MOVE 99999999          TO WS-BM-DATE-REVERSED 
              MOVE 999               TO WS-BM-SEQ
              MOVE "Y"               TO BM-EOF-SW
           ELSE 
              MOVE BM-PATH-OWNBR     TO WS-BM-OWNBR
              MOVE BM-SSNO           TO WS-BM-SSNO
              MOVE BM-DATE-REVERSED  TO WS-BM-DATE-REVERSED 
              MOVE BM-SEQ            TO WS-BM-SEQ
              ADD  1                 TO S35-RECORD-CTR.  

       EXIT-PROCESS-READ-BM1.
           EXIT.

      ******************************************************************
       COMPARE-FIELDS SECTION.
      ******************************************************************

      *     MOVE BM-PATH-OWNBR TO BM-BRNO
      *     MOVE OBM-SSNO      TO BM-SSNO.
      *     MOVE OBM-DATE      TO BM-DATE.
      *     MOVE OBM-SEQ       TO BM-SEQ.
      *
      *     PERFORM READ-BM1-FILE.
      *     IF IO-FG NOT = 0
      *        MOVE "MISSING S35 RECORD" TO D01-TEXT
      *        MOVE BR-NO        TO D01-BRNO
      *        MOVE BM-SSNO    TO D01-SSNO
      *        MOVE BM1-KEY      TO D01-A15
      *        PERFORM WRITE-DETAIL-LINE-ERROR
      *        GO TO EXIT-COMPARE-FIELDS.

           INSPECT OBM-MEMO REPLACING ALL X"09" BY " ".

           MOVE OBM-MEMO TO HOLD-MEMO-A15.

      * ONLY TEST FIRST 30 CHARACTERS OF TRANSFER MEMO
           IF HOLD-MEMO-TFR = "TFR FR BR" OR "TFR TO BR"
              MOVE BM-MEMO  TO HOLD-MEMO-S35
              IF HOLD-MEMO-A15 NOT = HOLD-MEMO-S35
                 MOVE "BM-MEMO" TO D01-TEXT
                 MOVE BM1-KEY   TO D01-KEY
                 MOVE OBM-MEMO  TO D01-A15
                 MOVE BM-MEMO   TO D01-S35
              END-IF
              GO TO COMPARE-FIELDS-TYPE.
              
           IF BM-MEMO NOT = OBM-MEMO
      *       CALL "C$JUSTIFY" USING OBM-MEMO, "L"
              IF ( BM-MEMO  NOT ALPHABETIC AND 
                   BM-MEMO  NOT NUMERIC )  OR 
                 (OBM-MEMO  NOT ALPHABETIC AND 
                  OBM-MEMO  NOT NUMERIC )   
                 NEXT SENTENCE
              ELSE
                 MOVE "BM-MEMO" TO D01-TEXT
                 MOVE BM1-KEY   TO D01-KEY
                 MOVE OBM-MEMO  TO D01-A15
                 MOVE BM-MEMO   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

       COMPARE-FIELDS-TYPE.
       
           IF BM-MEMO-TYPE NOT = OBM-MEMO-TYPE
                 MOVE "BM-MEMO-TYPE" TO D01-TEXT
                 MOVE BM1-KEY        TO D01-KEY
                 MOVE OBM-MEMO-TYPE  TO D01-A15
                 MOVE BM-MEMO-TYPE   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

       EXIT-COMPARE-FIELDS.
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

           MOVE "BEGINNING SSNO      : "    TO R01-PROMPT.
           MOVE BEG-SSNO                            TO R01-NUMERIC.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    SSNO      : "    TO R01-PROMPT.
           MOVE END-SSNO                            TO R01-NUMERIC.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "A15 RECORDS PROCESSED         :"     TO R01-PROMPT.
           MOVE A15-RECORDS-PROCESSED                 TO R01-NUMERIC.
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
           MOVE EXT-CONAME-CONAME TO BMFILE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BMFILE-SYSDATE.
           DISPLAY BMFILE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE BMFILE-FORM-FIELDS TO WR-BUF.
           MOVE BMFILE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE BMFILE-HELP--FILE TO HELP-LINK-FILE.
           MOVE BMFILE-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/BMFILE_EVA.CPY".
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
       COPY "LIBLP/LPBMGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

      * COPY "LIBLP/LPBM1RN.CPY".
      * COPYMEMBER: LIBLP/LPBM1RN
       LOAD-BM1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BM-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BM-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BM-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BM-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO BM-ALL-DATA
              MOVE BM-ALL-PATH             TO BM-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BM-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BM-ALL-BASE
              MOVE FILPATH-MACHINE         TO BM-ALL-MACHINE
              MOVE FILPATH-DATA            TO BM-ALL-DATA
              MOVE BM-ALL-PATH             TO BM-PATH.

      *-----------------------------------------------------------------
       OPEN-BM1-FILE.
           PERFORM LOAD-BM1-FILE.
           PERFORM OPEN-IT.
           MOVE BM-PATH-SQL TO E-FILE.

           IF BM1-CONN-OPEN-SW = "N" 
              MOVE "Y"         TO BM1-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS BM1_CONNECT 
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-BM1-FILE.
           PERFORM START-IT.
           MOVE BM-PATH-OWNBR  TO BM-BRNO.
           MOVE BM-PATH-SQL    TO E-FILE.
           MOVE BM1-KEY        TO E-KEYX.

           IF ( BM1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BM1-FILE.

           IF ( QBM1-WEND-SEQ = ZEROES    ) OR
              ( QBM1-WEND-SEQ NOT NUMERIC )
              MOVE BM-PATH-OWNBR        TO QBM1-WBEG-BRNO
              MOVE BM-PATH-OWNBR        TO QBM1-WEND-BRNO
              MOVE BM-SSNO              TO QBM1-WBEG-SSNO
              MOVE ALL "9"              TO QBM1-WEND-SSNO
              MOVE BM-DATE-REVERSED     TO QBM1-WBEG-DATE-REV
              MOVE ALL "9"              TO QBM1-WEND-DATE-REV
              MOVE BM-SEQ               TO QBM1-WBEG-SEQ
              MOVE ALL "9"              TO QBM1-WEND-SEQ.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           EXEC SQL SET CONNECTION BM1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE BM1_CURSOR CURSOR FOR
                :BM1-RETURN-CODE = EXEC SSP_BMFILE_BMFILE_SELECT 
              ( :QBM1-WBEG-BRNO,    
                :QBM1-WBEG-SSNO,    
                :QBM1-WEND-SSNO,    
                :QBM1-WBEG-DATE-REV,
                :QBM1-WEND-DATE-REV,
                :QBM1-WBEG-SEQ,     
                :QBM1-WEND-SEQ )      
           END-EXEC.

           EXEC SQL
               OPEN BM1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QBM1-WBEG-BRNO
                          QBM1-WEND-BRNO
                          QBM1-WBEG-SSNO
                          QBM1-WEND-SSNO
                          QBM1-WBEG-DATE-REV
                          QBM1-WEND-DATE-REV
                          QBM1-WBEG-SEQ
                          QBM1-WEND-SEQ.

           MOVE STAT---GOOD TO BM1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-BM1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BM-PATH-OWNBR  TO BM-BRNO.
           MOVE BM-PATH-SQL    TO E-FILE.
           MOVE BM1-KEY        TO E-KEYX.
           INITIALIZE QBM-REC.

           EXEC SQL SET CONNECTION BM1_CONNECT END-EXEC.

           IF ( BM1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BM1_CURSOR INTO :QBM-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BM1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BM-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-BM1-FILE.
           PERFORM CLOSE-IT.
           IF ( BM1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION BM1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE BM1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO BM1-CURSOR-STAT.

      *-----------------------------------------------------------------

       CLOSE-BM1-CONNECTION.

           IF BM1-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION BM1_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT BM1_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO BM1-CONN-OPEN-SW.

      *-----------------------------------------------------------------   

       OPEN-OBM1-FILE.
           PERFORM OPEN-IT.
           MOVE OBM-PATH TO E-FILE.
           OPEN INPUT OBM-FILE.
           IF IO-FG = 8
              GO TO OPEN-OBM1-FILE.
           IF IO-FG = 7
              CLOSE OBM-FILE
              GO TO OPEN-OBM1-FILE.
      *READ-OBM1-FILE.
      *    PERFORM READ-IT.
      *    MOVE OBM-PATH TO E-FILE.
      *    MOVE OBM1-KEY TO E-KEYX.
      *    READ OBM-FILE.
      *    IF IO-FG = 8
      *       GO TO READ-OBM1-FILE.
       READ-OBM1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OBM-PATH TO E-FILE.
           MOVE OBM1-KEY TO E-KEYX.
           READ OBM-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OBM1-FILE-NEXT.
       START-OBM1-FILE.
           PERFORM START-IT.
           MOVE OBM-PATH TO E-FILE.
           MOVE OBM1-KEY TO E-KEYX.
           START OBM-FILE KEY NOT < OBM1-KEY.
       CLOSE-OBM1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OBM-FILE.

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
              DESTROY BMFILE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
