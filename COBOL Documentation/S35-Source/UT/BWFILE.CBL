       IDENTIFICATION DIVISION.
       PROGRAM-ID.    BWFILE.
       DATE-WRITTEN.  10-24-2023.
      ******************************************************************
      *
      * DESCRIPTION   : COMPARE S35 BWFILE WITH A15 BWFILE
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
      *  01-06-25  KC  ADDED 'BTRACK' - PROCESS TRACKING TO PROGRAM.
      *  01-14-25  RMZ ADDED 'CEASE-DESIST/UMC' FIELDS TO PROGRAM.
      *       
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
      * COPYMEMBER: LIBLP/LPFSBW
           SELECT OBW-FILE ASSIGN TO OBW-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OBW1-KEY
                  ALTERNATE RECORD KEY OBW2-KEY WITH DUPLICATES
                  ALTERNATE RECORD KEY OBW3-KEY WITH DUPLICATES
                  ALTERNATE RECORD KEY OBW4-KEY WITH DUPLICATES
                  FILE STATUS FILE-STAT.
      * COPYMEMBER: LIBLP/LPFSBX
           SELECT OBX-FILE ASSIGN TO OBX-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OBX1-KEY
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  OBW-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A15SRC/LP01BW.CPY" REPLACING LEADING "BW" BY "OBW".
       FD  OBX-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A15SRC/LP01BX.CPY" REPLACING LEADING "BX" BY "OBX".

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/BWFILE S35 01/28/25-06:59:13 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".

       01  OBW-PATH                    PIC X(35) VALUE SPACES.
       01  OBW-ALL-PATH.
           03  OBW-ALL-BASE     PIC X(9).
           03  FILLER           PIC X     VALUE "/".
           03  OBW-ALL-MACHINE  PIC XX.
           03  FILLER           PIC X     VALUE "/".
           03  OBW-ALL-DATA     PIC X(6).
           03  FILLER           PIC X(10) VALUE
               "/GB/BWFILE".
       01  OBX-PATH                    PIC X(35) VALUE SPACES.
       01  OBX-ALL-PATH.
           03  OBX-ALL-BASE     PIC X(9).
           03  FILLER           PIC X     VALUE "/".
           03  OBX-ALL-MACHINE  PIC XX.
           03  FILLER           PIC X     VALUE "/".
           03  OBX-ALL-DATA     PIC X(6).
           03  FILLER           PIC X(10) VALUE
               "/GB/BXFILE".

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

       01  BW1-CONN-OPEN-SW            PIC X      VALUE "N".
       01  BW-EOF-SW                   PIC X      VALUE "N".
       01  OBW-EOF-SW                  PIC X      VALUE "N".
 
       01  WS-OBW1-KEY.
           03  WS-OBW-OWNBR            PIC 9(4).
           03  WS-OBW-SSNO             PIC 9(9).

       01  WS-BW1-KEY.
           03  WS-BW-OWNBR             PIC 9(4).
           03  WS-BW-SSNO              PIC 9(9).

       01  MACHINE-R1.
           03  FILLER                  PIC X VALUE X"72".
           03  FILLER                  PIC 9 VALUE 1.
       01  MACHINE-R2.
           03  FILLER                  PIC X VALUE X"72".
           03  FILLER                  PIC 9 VALUE 2.

       01  MESS-LIST                   PIC X(5)     VALUE "MESS.".

       01  PROCESS-STAT                PIC X(08)    VALUE SPACES.
           88  PROCESS-STAT-GOOD                    VALUE "00".
           88  PROCESS-STAT-BAD                     VALUE "  " THRU "//"
                                                         "01" THRU "~~".

       01  TOTAL-ERRORS-CTR            PIC 9(10)    VALUE ZEROES.  
       01  S35-RECORD-CTR              PIC 9(10)    VALUE ZEROES.  
       01  A15-RECORDS-PROCESSED       PIC 9(9)     VALUE ZEROES.
           88  NO-RECORDS-FOUND                     VALUE ZEROES.

       01  STARTING-TIME               PIC X(08)    VALUE SPACES.
       01  SUB                         PIC 9(04)    VALUE ZEROES.

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
           "BORROWER FILE (BWFILE) COMPARE A15 TO S35".

       01  HEADER-LINE-03.
           03  FILLER                PIC X(4) VALUE "BRNO".
           03  FILLER                PIC X(5) VALUE SPACES.
           03  FILLER                PIC X(6) VALUE "SSNO".
           03  FILLER                PIC X(5) VALUE SPACES.
           03  FILLER                PIC X(11) VALUE "DESCRIPTION".
           03  FILLER                PIC X(17) VALUE SPACES.
           03  FILLER                PIC X(3) VALUE "A15".
           03  FILLER                PIC X(39) VALUE SPACES.
           03  FILLER                PIC X(3) VALUE "S35".

      ******************************************************************
      *
      *    D E T A I L - L I N E - W O R K E R S
      *
      ******************************************************************
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-BRNO                       PIC Z(3)9.
           03  FILLER                         PIC X(5).
           03  D01-SSNO                       PIC 9(9).
           03  FILLER                         PIC X(2).
           03  D01-TEXT                       PIC X(25).
           03  FILLER                         PIC X(3).
           03  D01-A15                        PIC X(40).
           03  FILLER REDEFINES D01-A15.
               05  D01-A15-NUM                PIC Z(7)9.9(3).
               05  FILLER                     PIC X(28).
           03  FILLER                         PIC X(2).
           03  D01-S35                        PIC X(40).
           03  FILLER REDEFINES D01-S35.
               05  D01-S35-NUM                PIC Z(7)9.9(3).
               05  FILLER                     PIC X(28).

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
       COPY "LIBUT/BWFILE_DEF.CPY".
       COPY "LIBUT/BWFILE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/BWFILE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE OBW-FILE OBX-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BW1-CONNECTION.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE OBW-FILE OBX-FILE.
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
           MOVE "BWFILE" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE BWFILE-QUEUE-POS TO Q-POS.
           MOVE BWFILE-COPIES-POS TO C-POS.
           MOVE BWFILE-PRU-POS   TO P-POS.
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
              GO TO ENTER-END-SSNO.

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


       ENTER-BEG-SSNO.

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
           MOVE 999999999                TO END-SSNO VDUNUM0.
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

           MOVE "S  A090S-PATH."           TO VDU.
           MOVE SOURCE-PATH                TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SNNN090BEG-SSNO."       TO VDU.
           MOVE BEG-SSNO                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN090END-SSNO."       TO VDU.
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

           MOVE SOURCE-PATH  TO OBW-ALL-BASE
                                OBX-ALL-BASE.
           MOVE MACHINE-R1   TO OBW-ALL-MACHINE
                                OBX-ALL-MACHINE.
           MOVE BR-DATA-PATH TO OBW-ALL-DATA
                                OBX-ALL-DATA.
           MOVE OBX-ALL-PATH TO OBX-PATH.
           MOVE OBW-ALL-PATH TO OBW-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE MACHINE-R2 TO OBW-ALL-MACHINE
                                 OBX-ALL-MACHINE
              MOVE OBX-ALL-PATH TO OBX-PATH 
              MOVE OBW-ALL-PATH TO OBW-PATH ACCESS-BUF
              PERFORM ACCESS-CALL
              IF ( STAT-BAD )
                 MOVE "MISSING A15 FILE" TO D01-TEXT
                 MOVE BR-NO              TO D01-BRNO
                 MOVE 0                  TO D01-SSNO
                 MOVE OBW-ALL-PATH       TO D01-A15
                 MOVE SPACES             TO D01-S35
                 PERFORM WRITE-DETAIL-LINE
                 GO TO PROCESS-BRANCH-DRIVER-END.

           PERFORM OPEN-OBW1-FILE.
           PERFORM OPEN-OBX1-FILE.

           MOVE EXT-FILPATH-BASE     TO FILPATH-BASE.
           MOVE BR-MACHINE           TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH         TO FILPATH-DATA.

           MOVE "B"                  TO BW-PATH-OVERRIDE.
           MOVE "N"                  TO OBW-EOF-SW
                                        BW-EOF-SW.

           PERFORM OPEN-BW1-FILE.

           MOVE BEG-SSNO             TO OBW-SSNO.

           PERFORM START-OBW1-FILE.

           MOVE BW-PATH-OWNBR        TO QBW1-WBEG-OWNBR.     
           MOVE BEG-SSNO             TO QBW1-WBEG-SSNO.   
           MOVE END-SSNO             TO QBW1-WEND-SSNO. 

           PERFORM START-BW1-FILE.

           PERFORM PROCESS-READ-OBW1.
           PERFORM PROCESS-READ-BW1.

       PROCESS-BRANCH-NEXT-BW.
 
           IF BW-EOF-SW  = "Y" AND OBW-EOF-SW = "Y"
              GO TO PROCESS-BRANCH-DRIVER-END.              

           IF WS-OBW1-KEY < WS-BW1-KEY 
              MOVE "MISSING S35 RECORD" TO D01-TEXT
              MOVE WS-OBW-OWNBR         TO D01-BRNO
              MOVE WS-OBW-SSNO          TO D01-SSNO 
              MOVE WS-OBW1-KEY          TO D01-A15
              MOVE WS-BW1-KEY           TO D01-S35
              PERFORM WRITE-DETAIL-LINE 
              PERFORM PROCESS-READ-OBW1 
              GO TO PROCESS-BRANCH-NEXT-BW.
 
           IF WS-BW1-KEY < WS-OBW1-KEY
              MOVE "MISSING A15 RECORD" TO D01-TEXT
              MOVE WS-BW-OWNBR          TO D01-BRNO
              MOVE WS-BW-SSNO           TO D01-SSNO
              MOVE WS-OBW1-KEY          TO D01-A15
              MOVE WS-BW1-KEY           TO D01-S35
              PERFORM WRITE-DETAIL-LINE       
              PERFORM PROCESS-READ-BW1 
              GO TO PROCESS-BRANCH-NEXT-BW.

           PERFORM COMPARE-FIELDS.

           PERFORM PROCESS-READ-OBW1.  
           PERFORM PROCESS-READ-BW1. 

           GO TO PROCESS-BRANCH-NEXT-BW.

       PROCESS-BRANCH-DRIVER-END.

           PERFORM CLOSE-BW1-FILE.

       PROCESS-BRANCH-DRIVER-EXIT.
           EXIT.

      ******************************************************************
       PROCESS-READ-OBW1 SECTION.
      ******************************************************************

           PERFORM READ-OBW1-FILE-NEXT.
           IF IO-BAD 
              MOVE 9999          TO WS-OBW-OWNBR
              MOVE 999999999     TO WS-OBW-SSNO
              MOVE "Y"           TO OBW-EOF-SW
              GO TO EXIT-PROCESS-READ-OBW1
           ELSE 
              MOVE BW-PATH-OWNBR TO WS-OBW-OWNBR
              MOVE OBW-SSNO      TO WS-OBW-SSNO.  

           PERFORM SEND-PROGRESS-INDICATOR.

           IF ( OBW-SSNO < BEG-SSNO ) OR
              ( OBW-SSNO > END-SSNO )
              GO TO PROCESS-READ-OBW1.

           ADD 1 TO A15-RECORDS-PROCESSED.

       EXIT-PROCESS-READ-OBW1.
           EXIT.

      ******************************************************************
       PROCESS-READ-BW1 SECTION.
      ******************************************************************

           PERFORM READ-BW1-FILE-NEXT.
           IF IO-BAD 
              MOVE 9999          TO WS-BW-OWNBR
              MOVE 999999999     TO WS-BW-SSNO  
              MOVE "Y"           TO BW-EOF-SW
           ELSE 
              MOVE BW-PATH-OWNBR TO WS-BW-OWNBR
              MOVE BW-SSNO       TO WS-BW-SSNO 
              ADD 1              TO S35-RECORD-CTR.

       EXIT-PROCESS-READ-BW1.
           EXIT.

      ******************************************************************
       COMPARE-FIELDS SECTION.
      ******************************************************************

      *     MOVE BW-PATH-OWNBR TO BW-OWNBR.
      *     MOVE OBW-SSNO     TO BW-SSNO.
      *     PERFORM READ-BW1-FILE.
      *     IF IO-FG NOT = 0
      *        MOVE "MISSING S35 RECORD" TO D01-TEXT
      *        MOVE BR-NO      TO D01-BRNO
      *        MOVE BW-SSNO    TO D01-SSNO
      *        PERFORM WRITE-DETAIL-LINE
      *        GO TO EXIT-COMPARE-FIELDS.

      * THE TRIM IN BRIANS PROGRAM DOESNT REMOVE JUST 1 LEADING SPACE
           IF BW-LNAME NOT = OBW-LNAME
      *        CALL "C$JUSTIFY" USING OBW-LNAME, "L"
              IF ( BW-LNAME  NOT ALPHABETIC AND 
                   BW-LNAME  NOT NUMERIC )  OR 
                 (OBW-LNAME  NOT ALPHABETIC AND 
                  OBW-LNAME  NOT NUMERIC )   
                 NEXT SENTENCE
              ELSE
                 MOVE "BW-LNAME" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE BW-SSNO    TO D01-SSNO
                 MOVE OBW-LNAME  TO D01-A15
                 MOVE BW-LNAME   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BW-FNAME NOT = OBW-FNAME
      *        CALL "C$JUSTIFY" USING OBW-FNAME, "L"
              IF ( BW-FNAME  NOT ALPHABETIC AND 
                   BW-FNAME  NOT NUMERIC )  OR 
                 (OBW-FNAME  NOT ALPHABETIC AND 
                  OBW-FNAME  NOT NUMERIC )   
                 NEXT SENTENCE
              ELSE
                 MOVE "BW-FNAME" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE BW-SSNO    TO D01-SSNO
                 MOVE OBW-FNAME  TO D01-A15
                 MOVE BW-FNAME   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BW-ADR1 NOT = OBW-ADR1
      *        CALL "C$JUSTIFY" USING OBW-ADR1, "L"
              IF ( BW-ADR1  NOT ALPHABETIC AND 
                   BW-ADR1  NOT NUMERIC )  OR 
                 (OBW-ADR1  NOT ALPHABETIC AND 
                  OBW-ADR1  NOT NUMERIC )   
                 NEXT SENTENCE
              ELSE
                 MOVE "BW-ADR1" TO D01-TEXT
                 MOVE BR-NO     TO D01-BRNO
                 MOVE BW-SSNO   TO D01-SSNO
                 MOVE OBW-ADR1  TO D01-A15
                 MOVE BW-ADR1   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BW-ADR2 NOT = OBW-ADR2
      *        CALL "C$JUSTIFY" USING OBW-ADR2, "L"
              IF ( BW-ADR2  NOT ALPHABETIC AND 
                   BW-ADR2  NOT NUMERIC )  OR 
                 (OBW-ADR2  NOT ALPHABETIC AND 
                  OBW-ADR2  NOT NUMERIC )   
                 NEXT SENTENCE
              ELSE
                 MOVE "BW-ADR2" TO D01-TEXT
                 MOVE BR-NO     TO D01-BRNO
                 MOVE BW-SSNO   TO D01-SSNO
                 MOVE OBW-ADR2  TO D01-A15
                 MOVE BW-ADR2   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BW-CITY NOT = OBW-CITY
      *        CALL "C$JUSTIFY" USING OBW-CITY, "L"
              IF ( BW-CITY  NOT ALPHABETIC AND 
                   BW-CITY  NOT NUMERIC )  OR 
                 (OBW-CITY  NOT ALPHABETIC AND 
                  OBW-CITY  NOT NUMERIC )   
                 NEXT SENTENCE
              ELSE
                 MOVE "BW-CITY" TO D01-TEXT
                 MOVE BR-NO     TO D01-BRNO
                 MOVE BW-SSNO   TO D01-SSNO
                 MOVE OBW-CITY  TO D01-A15
                 MOVE BW-CITY   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BW-STATE NOT = OBW-STATE
      *        CALL "C$JUSTIFY" USING OBW-STATE, "L"
              IF ( BW-STATE  NOT ALPHABETIC AND 
                   BW-STATE  NOT NUMERIC )  OR 
                 (OBW-STATE  NOT ALPHABETIC AND 
                  OBW-STATE  NOT NUMERIC )   
                 NEXT SENTENCE
              ELSE
                 MOVE "BW-STATE" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE BW-SSNO    TO D01-SSNO
                 MOVE OBW-STATE  TO D01-A15
                 MOVE BW-STATE   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BW-ZIP NOT = OBW-ZIP
      *        CALL "C$JUSTIFY" USING OBW-ZIP, "L"
              IF ( BW-ZIP  NOT ALPHABETIC AND 
                   BW-ZIP  NOT NUMERIC )  OR 
                 (OBW-ZIP  NOT ALPHABETIC AND 
                  OBW-ZIP  NOT NUMERIC )   
                 NEXT SENTENCE
              ELSE
                 MOVE "BW-ZIP" TO D01-TEXT
                 MOVE BR-NO    TO D01-BRNO
                 MOVE BW-SSNO  TO D01-SSNO
                 MOVE OBW-ZIP  TO D01-A15
                 MOVE BW-ZIP   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF (OBW-REFDATE NOT NUMERIC) OR
                (OBW-REFDATE = 020202) OR
                 (OBW-REFDATE = 0)
              MOVE 0  TO OBW-REFDATE.
           MOVE OBW-REFDATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
           PERFORM TEST-INVALID-DATE.
           IF BW-REFDATE NOT = DATE-YYYYMMDD
              MOVE "BW-REFDATE"   TO D01-TEXT
              MOVE BR-NO          TO D01-BRNO
              MOVE BW-SSNO        TO D01-SSNO
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE BW-REFDATE     TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

      * 2/7/2019 BW-AGE NEEDS TO CONVERT CONSISTENTLY WITH HOW WE CONVERT
      *          IT TO 8 DIGIT DATE IN THE A15 BWEXTR
      * EXAMPLE: BW-AGE 01/04/00 - CONVERT TO 01/04/2000
      *                 01/04/05 - CONVERT TO 01/04/2005
      *                 01/04/48 - CONVERT TO 01/04/1948

           IF (OBW-AGE = 0) OR (OBW-AGE NOT NUMERIC)
              MOVE 0 TO DATE-YYYYMMDD
           ELSE
              MOVE OBW-AGE        TO DATE-MMDDYY
              MOVE DATE-MMDDYY-MM TO DATE-YYYYMMDD-MM
              MOVE DATE-MMDDYY-DD TO DATE-YYYYMMDD-DD
              MOVE DATE-MMDDYY-YY TO DATE-YYYYMMDD-YY
              MOVE 19             TO DATE-YYYYMMDD-CC
              IF DATE-MMDDYY-YY < EXT-JULIAN-CC
                 MOVE EXT-CONAME-SYSDATE TO ALP-DATE
                 IF DATE-MMDDYY-YY NOT > ALP-YR
                    MOVE 20 TO DATE-YYYYMMDD-CC.

      * AGES! CANT FIX. BR 110 254354386 AGE 23
      *                 BR 143 999000818 AGE 88
           IF DATE-YYYYMMDD-MM > 12 OR
               DATE-YYYYMMDD-DD > 31 OR
               (DATE-YYYYMMDD-YYYY NOT = 0 AND DATE-YYYYMMDD-MM = 0) OR
                (DATE-YYYYMMDD-YYYY NOT = 0 AND DATE-YYYYMMDD-DD = 0)
              MOVE 0 TO DATE-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD-YYYY > 2008
              MOVE 19 TO DATE-YYYYMMDD-CC.

           IF BW-BDATE NOT = DATE-YYYYMMDD
              MOVE "BW-BDATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE BW-SSNO    TO D01-SSNO
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE BW-BDATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OBW-PHONE NOT NUMERIC
              MOVE 0 TO OBW-PHONE.
           IF BW-PHONE NOT = OBW-PHONE
              MOVE "BW-PHONE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE BW-SSNO    TO D01-SSNO
              MOVE OBW-PHONE  TO D01-A15
              MOVE BW-PHONE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OBW-CELL-PHONE NOT NUMERIC OR
               (OBW-CELL-PHONE = 2020202020 OR 0202020202)
              MOVE 0               TO OBW-CELL-PHONE.
           IF BW-CELL-PHONE NOT = OBW-CELL-PHONE
              MOVE "BW-CELL-PHONE" TO D01-TEXT
              MOVE BR-NO           TO D01-BRNO
              MOVE BW-SSNO         TO D01-SSNO
              MOVE OBW-CELL-PHONE  TO D01-A15
              MOVE BW-CELL-PHONE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF (OBW-DEPEND NOT NUMERIC) OR (OBW-DEPEND = 20)
              MOVE 0           TO OBW-DEPEND.
           IF BW-DEPEND NOT = OBW-DEPEND
              MOVE "BW-DEPEND" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE BW-SSNO     TO D01-SSNO
              MOVE OBW-DEPEND  TO D01-A15
              MOVE BW-DEPEND   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BW-RENTCD NOT = OBW-RENTCD
      *        CALL "C$JUSTIFY" USING OBW-RENTCD, "L"
              IF ( BW-RENTCD  NOT ALPHABETIC AND 
                   BW-RENTCD  NOT NUMERIC )  OR 
                 (OBW-RENTCD  NOT ALPHABETIC AND 
                  OBW-RENTCD  NOT NUMERIC )   
                 NEXT SENTENCE
              ELSE  
                 MOVE "BW-RENTCD" TO D01-TEXT
                 MOVE BR-NO       TO D01-BRNO
                 MOVE BW-SSNO     TO D01-SSNO
                 MOVE OBW-RENTCD  TO D01-A15
                 MOVE BW-RENTCD   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BW-TIMEADDR NOT = OBW-TIMEADDR
      *        CALL "C$JUSTIFY" USING OBW-TIMEADDR, "L"
              IF ( BW-TIMEADDR  NOT ALPHABETIC AND 
                   BW-TIMEADDR  NOT NUMERIC )  OR 
                 (OBW-TIMEADDR  NOT ALPHABETIC AND 
                  OBW-TIMEADDR  NOT NUMERIC )   
                 NEXT SENTENCE
              ELSE
                 MOVE "BW-TIMEADDR" TO D01-TEXT
                 MOVE BR-NO         TO D01-BRNO
                 MOVE BW-SSNO       TO D01-SSNO
                 MOVE OBW-TIMEADDR  TO D01-A15
                 MOVE BW-TIMEADDR   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BW-CREDITCD NOT = OBW-CREDITCD
      *        CALL "C$JUSTIFY" USING OBW-CREDITCD, "L"
              IF ( BW-CREDITCD  NOT ALPHABETIC AND 
                   BW-CREDITCD  NOT NUMERIC )  OR 
                 (OBW-CREDITCD  NOT ALPHABETIC AND 
                  OBW-CREDITCD  NOT NUMERIC )   
                 NEXT SENTENCE
              ELSE
                 MOVE "BW-CREDITCD" TO D01-TEXT
                 MOVE BR-NO         TO D01-BRNO
                 MOVE BW-SSNO       TO D01-SSNO
                 MOVE OBW-CREDITCD  TO D01-A15
                 MOVE BW-CREDITCD   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BW-STRTGCD NOT = OBW-STRTGCD
      *        CALL "C$JUSTIFY" USING OBW-STRTGCD, "L"
              IF ( BW-STRTGCD  NOT ALPHABETIC AND 
                   BW-STRTGCD  NOT NUMERIC )  OR 
                 (OBW-STRTGCD  NOT ALPHABETIC AND 
                  OBW-STRTGCD  NOT NUMERIC )   
                 NEXT SENTENCE
              ELSE
                 MOVE "BW-STRTGCD" TO D01-TEXT
                 MOVE BR-NO        TO D01-BRNO
                 MOVE BW-SSNO      TO D01-SSNO
                 MOVE OBW-STRTGCD  TO D01-A15
                 MOVE BW-STRTGCD   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BW-DWELLCD NOT = OBW-DWELLCD
      *        CALL "C$JUSTIFY" USING OBW-DWELLCD, "L"
              IF ( BW-DWELLCD  NOT ALPHABETIC AND 
                   BW-DWELLCD  NOT NUMERIC )  OR 
                 (OBW-DWELLCD  NOT ALPHABETIC AND 
                  OBW-DWELLCD  NOT NUMERIC )   
                 NEXT SENTENCE
              ELSE
                 MOVE "BW-DWELLCD" TO D01-TEXT
                 MOVE BR-NO        TO D01-BRNO
                 MOVE BW-SSNO      TO D01-SSNO
                 MOVE OBW-DWELLCD  TO D01-A15
                 MOVE BW-DWELLCD   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BW-COUNTY NOT = OBW-COUNTY
      *        CALL "C$JUSTIFY" USING OBW-COUNTY, "L"
              IF ( BW-COUNTY  NOT ALPHABETIC AND 
                   BW-COUNTY  NOT NUMERIC )  OR 
                 (OBW-COUNTY  NOT ALPHABETIC AND 
                  OBW-COUNTY  NOT NUMERIC )   
                 NEXT SENTENCE
              ELSE
                 MOVE "BW-COUNTY" TO D01-TEXT
                 MOVE BR-NO       TO D01-BRNO
                 MOVE BW-SSNO     TO D01-SSNO
                 MOVE OBW-COUNTY  TO D01-A15
                 MOVE BW-COUNTY   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BW-ORIGIN NOT = OBW-ORIGIN
      *        CALL "C$JUSTIFY" USING OBW-ORIGIN, "L"
              IF ( BW-ORIGIN  NOT ALPHABETIC AND 
                   BW-ORIGIN  NOT NUMERIC )  OR 
                 (OBW-ORIGIN  NOT ALPHABETIC AND 
                  OBW-ORIGIN  NOT NUMERIC )   
                 NEXT SENTENCE
              ELSE
                 MOVE "BW-ORIGIN" TO D01-TEXT
                 MOVE BR-NO       TO D01-BRNO
                 MOVE BW-SSNO     TO D01-SSNO
                 MOVE OBW-ORIGIN  TO D01-A15
                 MOVE BW-ORIGIN   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BW-CENSUSCD NOT = OBW-CENSUSCD
      *        CALL "C$JUSTIFY" USING OBW-CENSUSCD, "L"
              IF ( BW-CENSUSCD  NOT ALPHABETIC AND 
                   BW-CENSUSCD  NOT NUMERIC )  OR 
                 (OBW-CENSUSCD  NOT ALPHABETIC AND 
                  OBW-CENSUSCD  NOT NUMERIC )   
                 NEXT SENTENCE
              ELSE
                 MOVE "BW-CENSUSCD" TO D01-TEXT
                 MOVE BR-NO         TO D01-BRNO
                 MOVE BW-SSNO       TO D01-SSNO
                 MOVE OBW-CENSUSCD  TO D01-A15
                 MOVE BW-CENSUSCD   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OBW-ORGSLSMN NOT NUMERIC
              MOVE 0 TO OBW-ORGSLSMN.
           IF BW-ORGSLSMN NOT = OBW-ORGSLSMN
              MOVE "BW-ORGSLSMN" TO D01-TEXT
              MOVE BR-NO         TO D01-BRNO
              MOVE BW-SSNO       TO D01-SSNO
              MOVE OBW-ORGSLSMN  TO D01-A15
              MOVE BW-ORGSLSMN   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BW-REPCODE NOT = OBW-REPCODE
      *        CALL "C$JUSTIFY" USING OBW-REPCODE, "L"
              IF ( BW-REPCODE  NOT ALPHABETIC AND 
                   BW-REPCODE  NOT NUMERIC )  OR 
                 (OBW-REPCODE  NOT ALPHABETIC AND 
                  OBW-REPCODE  NOT NUMERIC )   
                 NEXT SENTENCE
              ELSE
                 MOVE "BW-REPCODE" TO D01-TEXT
                 MOVE BR-NO        TO D01-BRNO
                 MOVE BW-SSNO      TO D01-SSNO
                 MOVE OBW-REPCODE  TO D01-A15
                 MOVE BW-REPCODE   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OBW-SPSSNO NOT NUMERIC
              MOVE 0 TO OBW-SPSSNO.
           IF BW-SPSSNO NOT = OBW-SPSSNO
              MOVE "BW-SPSSNO" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE BW-SSNO     TO D01-SSNO
              MOVE OBW-SPSSNO  TO D01-A15
              MOVE BW-SPSSNO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BW-SPLNAME NOT = OBW-SPLNAME
      *        CALL "C$JUSTIFY" USING OBW-SPLNAME, "L"
              IF ( BW-SPLNAME  NOT ALPHABETIC AND 
                   BW-SPLNAME  NOT NUMERIC )  OR 
                 (OBW-SPLNAME  NOT ALPHABETIC AND 
                  OBW-SPLNAME  NOT NUMERIC )   
                 NEXT SENTENCE
              ELSE
                 MOVE "BW-SPLNAME" TO D01-TEXT
                 MOVE BR-NO        TO D01-BRNO
                 MOVE BW-SSNO      TO D01-SSNO
                 MOVE OBW-SPLNAME  TO D01-A15
                 MOVE BW-SPLNAME   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BW-SPFNAME NOT = OBW-SPFNAME
      *        CALL "C$JUSTIFY" USING OBW-SPFNAME, "L"
              IF ( BW-SPFNAME  NOT ALPHABETIC AND 
                   BW-SPFNAME  NOT NUMERIC )  OR 
                 (OBW-SPFNAME  NOT ALPHABETIC AND 
                  OBW-SPFNAME  NOT NUMERIC )   
                 NEXT SENTENCE
              ELSE
                 MOVE "BW-SPFNAME" TO D01-TEXT
                 MOVE BR-NO        TO D01-BRNO
                 MOVE BW-SSNO      TO D01-SSNO
                 MOVE OBW-SPFNAME  TO D01-A15
                 MOVE BW-SPFNAME   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

      * 2/7/2019 BW-AGE NEEDS TO CONVERT CONSISTENTLY WITH HOW WE CONVERT
      *          IT TO 8 DIGIT DATE IN THE A15 BWEXTR
      * EXAMPLE: BW-AGE 01/04/00 - CONVERT TO 01/04/2000
      *                 01/04/05 - CONVERT TO 01/04/2005
      *                 01/04/48 - CONVERT TO 01/04/1948

           IF (OBW-SPAGE = 0) OR (OBW-SPAGE NOT NUMERIC) OR
               (OBW-SPAGE = 20202)
              MOVE 0 TO DATE-YYYYMMDD
           ELSE
              MOVE OBW-SPAGE      TO DATE-MMDDYY
              MOVE DATE-MMDDYY-MM TO DATE-YYYYMMDD-MM
              MOVE DATE-MMDDYY-DD TO DATE-YYYYMMDD-DD
              MOVE DATE-MMDDYY-YY TO DATE-YYYYMMDD-YY
              MOVE 19             TO DATE-YYYYMMDD-CC
              IF DATE-MMDDYY-YY < EXT-JULIAN-CC
                 MOVE EXT-CONAME-SYSDATE TO ALP-DATE
                 IF DATE-MMDDYY-YY NOT > ALP-YR
                    MOVE 20 TO DATE-YYYYMMDD-CC.

           IF DATE-YYYYMMDD-MM > 12 OR
               DATE-YYYYMMDD-DD > 31 OR
               (DATE-YYYYMMDD-YYYY NOT = 0 AND DATE-YYYYMMDD-MM = 0) OR
                (DATE-YYYYMMDD-YYYY NOT = 0 AND DATE-YYYYMMDD-DD = 0)
              MOVE 0 TO DATE-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
      * BR 1502 BW-AGE 08/30/24 - YEAR 1924! WE HIT 2024 AND IT SET IT
      * TO 08/30/2024 IN S35. FORCE ANYONE 16 YEARS OLD OR YOUNGER TO BE
      * CENTURY 1900'S! A15 DIDNT HAVE CENTURY, TOUGH CONVERSION
           IF DATE-YYYYMMDD-YYYY > 2008
              MOVE 19 TO DATE-YYYYMMDD-CC.
           IF BW-SP-BDATE NOT = DATE-YYYYMMDD
              MOVE "BW-SP-BDATE"  TO D01-TEXT
              MOVE BR-NO          TO D01-BRNO
              MOVE BW-SSNO        TO D01-SSNO
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE BW-SP-BDATE    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BW-EMPLOYER NOT = OBW-EMPLOYER
      *        CALL "C$JUSTIFY" USING OBW-EMPLOYER, "L"
              IF ( BW-EMPLOYER  NOT ALPHABETIC AND 
                   BW-EMPLOYER  NOT NUMERIC )  OR 
                 (OBW-EMPLOYER  NOT ALPHABETIC AND 
                  OBW-EMPLOYER  NOT NUMERIC )   
                 NEXT SENTENCE
              ELSE
                 MOVE "BW-EMPLOYER" TO D01-TEXT
                 MOVE BR-NO         TO D01-BRNO
                 MOVE BW-SSNO       TO D01-SSNO
                 MOVE OBW-EMPLOYER  TO D01-A15
                 MOVE BW-EMPLOYER   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BW-EMPLPOS NOT = OBW-EMPLPOS
      *        CALL "C$JUSTIFY" USING OBW-EMPLPOS, "L"
              IF ( BW-EMPLPOS  NOT ALPHABETIC AND 
                   BW-EMPLPOS  NOT NUMERIC )  OR 
                 (OBW-EMPLPOS  NOT ALPHABETIC AND 
                  OBW-EMPLPOS  NOT NUMERIC )   
                 NEXT SENTENCE
              ELSE
                 MOVE "BW-EMPLPOS" TO D01-TEXT
                 MOVE BR-NO        TO D01-BRNO
                 MOVE BW-SSNO      TO D01-SSNO
                 MOVE OBW-EMPLPOS  TO D01-A15
                 MOVE BW-EMPLPOS   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OBW-EMPPHONE NOT NUMERIC
              MOVE 0 TO OBW-EMPPHONE.
           IF BW-EMPPHONE NOT = OBW-EMPPHONE
              MOVE "BW-EMPPHONE" TO D01-TEXT
              MOVE BR-NO         TO D01-BRNO
              MOVE BW-SSNO       TO D01-SSNO
              MOVE OBW-EMPPHONE  TO D01-A15-NUM
              MOVE BW-EMPPHONE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OBW-EMPEXT NOT NUMERIC
              MOVE 0 TO OBW-EMPEXT.
           IF BW-EMPEXT NOT = OBW-EMPEXT
              MOVE "BW-EMPEXT" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE BW-SSNO     TO D01-SSNO
              MOVE OBW-EMPEXT  TO D01-A15-NUM
              MOVE BW-EMPEXT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OBW-EMPYEARS NOT NUMERIC
              MOVE 0 TO OBW-EMPYEARS.
           IF BW-EMPYEARS NOT = OBW-EMPYEARS
              MOVE "BW-EMPYEARS" TO D01-TEXT
              MOVE BR-NO         TO D01-BRNO
              MOVE BW-SSNO       TO D01-SSNO
              MOVE OBW-EMPYEARS  TO D01-A15-NUM
              MOVE BW-EMPYEARS   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OBW-EMPPAY NOT NUMERIC
              MOVE 0 TO OBW-EMPPAY.
           IF BW-EMPPAY NOT = OBW-EMPPAY
              MOVE "BW-EMPPAY" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE BW-SSNO     TO D01-SSNO
              MOVE OBW-EMPPAY  TO D01-A15-NUM
              MOVE BW-EMPPAY   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OBW-EMPPAYOT NOT NUMERIC
              MOVE 0 TO OBW-EMPPAYOT.
           IF BW-EMPPAYOT NOT = OBW-EMPPAYOT
              MOVE "BW-EMPPAYOT" TO D01-TEXT
              MOVE BR-NO         TO D01-BRNO
              MOVE BW-SSNO       TO D01-SSNO
              MOVE OBW-EMPPAYOT  TO D01-A15-NUM
              MOVE BW-EMPPAYOT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BW-EMPPAYSR NOT = OBW-EMPPAYSR
      *       CALL "C$JUSTIFY" USING OBW-EMPPAYSR, "L"
              IF ( BW-EMPPAYSR  NOT ALPHABETIC AND 
                   BW-EMPPAYSR  NOT NUMERIC )  OR 
                 (OBW-EMPPAYSR  NOT ALPHABETIC AND 
                  OBW-EMPPAYSR  NOT NUMERIC )   
                 NEXT SENTENCE
              ELSE
                 MOVE "BW-EMPPAYSR" TO D01-TEXT
                 MOVE BR-NO         TO D01-BRNO
                 MOVE BW-SSNO       TO D01-SSNO
                 MOVE OBW-EMPPAYSR  TO D01-A15
                 MOVE BW-EMPPAYSR   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BW-EMPADDR1 NOT = OBW-EMPADDR1
      *        CALL "C$JUSTIFY" USING OBW-EMPADDR1, "L"
              IF ( BW-EMPADDR1  NOT ALPHABETIC AND 
                   BW-EMPADDR1  NOT NUMERIC )  OR 
                 (OBW-EMPADDR1  NOT ALPHABETIC AND 
                  OBW-EMPADDR1  NOT NUMERIC )   
                 NEXT SENTENCE
              ELSE
                 MOVE "BW-EMPADDR1" TO D01-TEXT
                 MOVE BR-NO         TO D01-BRNO
                 MOVE BW-SSNO       TO D01-SSNO
                 MOVE OBW-EMPADDR1  TO D01-A15
                 MOVE BW-EMPADDR1   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BW-EMPADDR2 NOT = OBW-EMPADDR2
      *        CALL "C$JUSTIFY" USING OBW-EMPADDR2, "L"
              IF ( BW-EMPADDR2  NOT ALPHABETIC AND 
                   BW-EMPADDR2  NOT NUMERIC )  OR 
                 (OBW-EMPADDR2  NOT ALPHABETIC AND 
                  OBW-EMPADDR2  NOT NUMERIC )   
                 NEXT SENTENCE
              ELSE
                 MOVE "BW-EMPADDR2" TO D01-TEXT
                 MOVE BR-NO         TO D01-BRNO
                 MOVE BW-SSNO       TO D01-SSNO
                 MOVE OBW-EMPADDR2  TO D01-A15
                 MOVE BW-EMPADDR2   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OBW-LOANS NOT NUMERIC
              MOVE 0 TO OBW-LOANS.
           IF BW-LOANS NOT = OBW-LOANS
              MOVE "BW-LOANS" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE BW-SSNO    TO D01-SSNO
              MOVE OBW-LOANS  TO D01-A15-NUM
              MOVE BW-LOANS   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BW-SSNO = "640247040" OR
              BW-SSNO = "636487215" OR
              BW-SSNO = "436175464" OR
              BW-SSNO = "434983950"
      D          STOP MESS
                 MOVE SPACES TO MESS.

           IF OBW-INTYTD NOT NUMERIC
              MOVE 0 TO OBW-INTYTD.
           IF BW-INTYTD NOT = OBW-INTYTD
              MOVE "BW-INTYTD" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE BW-SSNO     TO D01-SSNO
              MOVE OBW-INTYTD  TO D01-A15-NUM
              MOVE BW-INTYTD   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OBW-INTLYR NOT NUMERIC
              MOVE 0 TO OBW-INTLYR.
           IF BW-INTLYR NOT = OBW-INTLYR
              MOVE "BW-INTLYR" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE BW-SSNO     TO D01-SSNO
              MOVE OBW-INTLYR  TO D01-A15-NUM
              MOVE BW-INTLYR   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OBW-LIASMLN NOT NUMERIC
              MOVE 0 TO OBW-LIASMLN.
           IF BW-LIASMLN NOT = OBW-LIASMLN
              MOVE "BW-LIASMLN" TO D01-TEXT
              MOVE BR-NO        TO D01-BRNO
              MOVE BW-SSNO      TO D01-SSNO
              MOVE OBW-LIASMLN  TO D01-A15-NUM
              MOVE BW-LIASMLN   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OBW-LIAOTH NOT NUMERIC
              MOVE 0 TO OBW-LIAOTH.
           IF BW-LIAOTH NOT = OBW-LIAOTH
              MOVE "BW-LIAOTH" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE BW-SSNO     TO D01-SSNO
              MOVE OBW-LIAOTH  TO D01-A15-NUM
              MOVE BW-LIAOTH   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF (OBW-LSTPODATE NOT NUMERIC) OR
                 (OBW-LSTPODATE = 020202) OR
                   (OBW-LSTPODATE = 0)
              MOVE 0               TO OBW-LSTPODATE.
           MOVE OBW-LSTPODATE      TO DATE-MMDDYY
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
           PERFORM TEST-INVALID-DATE
           IF BW-LASTPODATE NOT = DATE-YYYYMMDD
              MOVE "BW-LSTPODATE"  TO D01-TEXT
              MOVE BR-NO           TO D01-BRNO
              MOVE BW-SSNO         TO D01-SSNO
              MOVE DATE-YYYYMMDD   TO D01-A15
              MOVE BW-LASTPODATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BW-PRLOANS NOT = OBW-PRLOANS
              MOVE "BW-PRLOANS" TO D01-TEXT
              MOVE BR-NO        TO D01-BRNO
              MOVE BW-SSNO      TO D01-SSNO
              MOVE OBW-PRLOANS  TO D01-A15
              MOVE BW-PRLOANS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BW-RE-INTYTD NOT = OBW-RE-INTYTD
              MOVE "BW-RE-INTYTD" TO D01-TEXT
              MOVE BR-NO          TO D01-BRNO
              MOVE BW-SSNO        TO D01-SSNO
              MOVE OBW-RE-INTYTD  TO D01-A15-NUM
              MOVE BW-RE-INTYTD   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BW-RE-INTLYR NOT = OBW-RE-INTLYR
              MOVE "BW-RE-INTLYR" TO D01-TEXT
              MOVE BR-NO          TO D01-BRNO
              MOVE BW-SSNO        TO D01-SSNO
              MOVE OBW-RE-INTLYR  TO D01-A15-NUM
              MOVE BW-RE-INTLYR   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OBW-CREDIT-LIMIT NOT NUMERIC OR
               (OBW-CREDIT-LIMIT = 202020.20 OR 020202.02)
               MOVE 0                TO OBW-CREDIT-LIMIT.
           IF BW-CREDIT-LIMIT NOT = OBW-CREDIT-LIMIT
              MOVE "BW-CREDIT-LIMIT" TO D01-TEXT
              MOVE BR-NO             TO D01-BRNO
              MOVE BW-SSNO           TO D01-SSNO
              MOVE OBW-CREDIT-LIMIT  TO D01-A15-NUM
              MOVE BW-CREDIT-LIMIT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
          
           IF BW-CREDIT-SCORE NOT = OBW-CREDIT-SCORE
              MOVE "BW-CREDIT-SCORE" TO D01-TEXT
              MOVE BR-NO             TO D01-BRNO
              MOVE BW-SSNO           TO D01-SSNO
              MOVE OBW-CREDIT-SCORE  TO D01-A15
              MOVE BW-CREDIT-SCORE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF (OBW-LAST-FORM-NO NOT NUMERIC) OR (OBW-LAST-FORM-NO = 202)
              MOVE 0                 TO OBW-LAST-FORM-NO.
           IF BW-LAST-FORM-NO NOT = OBW-LAST-FORM-NO
              MOVE "BW-LAST-FORM-NO" TO D01-TEXT
              MOVE BR-NO             TO D01-BRNO
              MOVE BW-SSNO           TO D01-SSNO
              MOVE OBW-LAST-FORM-NO  TO D01-A15-NUM
              MOVE BW-LAST-FORM-NO   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF (OBW-LAST-FORM-DATE NOT NUMERIC) OR
                  (OBW-LAST-FORM-DATE  = 020202) OR
                   (OBW-LAST-FORM-DATE = 0)
              MOVE 0                   TO OBW-LAST-FORM-DATE.
           MOVE OBW-LAST-FORM-DATE     TO DATE-MMDDYY
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
           PERFORM TEST-INVALID-DATE.
           IF BW-LAST-FORM-DATE NOT = DATE-YYYYMMDD
              MOVE "BW-LAST-FORM-DATE" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE BW-SSNO             TO D01-SSNO
              MOVE DATE-YYYYMMDD       TO D01-A15
              MOVE BW-LAST-FORM-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF (OBW-PRIOR-RN-FOR10 NOT NUMERIC) OR
               (OBW-PRIOR-RN-FOR10 = 0)
              MOVE 0               TO OBW-PRIOR-RN-FOR10.
           MOVE OBW-PRIOR-RN-FOR10 TO DATE-MMDDYY
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
           PERFORM TEST-INVALID-DATE
           IF BW-PRIOR-RN-FOR10-DATE NOT = DATE-YYYYMMDD
              MOVE "BW-PRIOR-RN-FOR10-DATE" TO D01-TEXT
              MOVE BR-NO           TO D01-BRNO
              MOVE BW-SSNO         TO D01-SSNO
              MOVE DATE-YYYYMMDD   TO D01-A15
              MOVE BW-PRIOR-RN-FOR10-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF (OBW-LAST-RN-FOR10 NOT NUMERIC) OR
               (OBW-LAST-RN-FOR10 = 020202) OR
                (OBW-LAST-RN-FOR10 = 0)
              MOVE 0               TO OBW-LAST-RN-FOR10.
           MOVE OBW-LAST-RN-FOR10  TO DATE-MMDDYY
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
           PERFORM TEST-INVALID-DATE
           IF BW-LAST-RN-FOR10-DATE NOT = DATE-YYYYMMDD
              MOVE "BW-LAST-RN-FOR10-DATE" TO D01-TEXT
              MOVE BR-NO           TO D01-BRNO
              MOVE BW-SSNO         TO D01-SSNO
              MOVE DATE-YYYYMMDD   TO D01-A15
              MOVE BW-LAST-RN-FOR10-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OBW-ADR-PHONE-CHG-DATE NOT NUMERIC OR
               (OBW-ADR-PHONE-CHG-DATE = 202020) OR
                (OBW-ADR-PHONE-CHG-DATE = 0)
              MOVE 0              TO OBW-ADR-PHONE-CHG-DATE.
           MOVE OBW-ADR-PHONE-CHG-DATE TO DATE-MMDDYY
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
           PERFORM TEST-INVALID-DATE
           IF BW-ADR-PHONE-CHG-DATE NOT = DATE-YYYYMMDD
              MOVE "BW-ADR-PHONE-CHG-DATE" TO D01-TEXT
              MOVE BR-NO          TO D01-BRNO
              MOVE BW-SSNO        TO D01-SSNO
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE BW-ADR-PHONE-CHG-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BW-BAD-ADR-FG NOT = OBW-BAD-ADR-FG
              MOVE "BW-BAD-ADR-FG" TO D01-TEXT
              MOVE BR-NO           TO D01-BRNO
              MOVE BW-SSNO         TO D01-SSNO
              MOVE OBW-BAD-ADR-FG  TO D01-A15
              MOVE BW-BAD-ADR-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BW-BAD-PHONE-FG NOT = OBW-BAD-PHONE-FG
              MOVE "BW-BAD-PHONE-FG" TO D01-TEXT
              MOVE BR-NO             TO D01-BRNO
              MOVE BW-SSNO           TO D01-SSNO
              MOVE OBW-BAD-PHONE-FG  TO D01-A15
              MOVE BW-BAD-PHONE-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BW-BAD-EMPPHONE-FG NOT = OBW-BAD-EMPPHONE-FG
              MOVE "BW-BAD-EMPPHONE-FG" TO D01-TEXT
              MOVE BR-NO                TO D01-BRNO
              MOVE BW-SSNO              TO D01-SSNO
              MOVE OBW-BAD-EMPPHONE-FG  TO D01-A15
              MOVE BW-BAD-EMPPHONE-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BW-BAD-MISC-PHONE1-FG NOT = OBW-BAD-MISC-PHONE1-FG
              MOVE "BW-BAD-MISC-PHONE1-FG" TO D01-TEXT
              MOVE BR-NO                   TO D01-BRNO
              MOVE BW-SSNO                 TO D01-SSNO
              MOVE OBW-BAD-MISC-PHONE1-FG  TO D01-A15
              MOVE BW-BAD-MISC-PHONE1-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BW-BAD-MISC-PHONE2-FG NOT = OBW-BAD-MISC-PHONE2-FG
              MOVE "BW-BAD-MISC-PHONE2-FG" TO D01-TEXT
              MOVE BR-NO                   TO D01-BRNO
              MOVE BW-SSNO                 TO D01-SSNO
              MOVE OBW-BAD-MISC-PHONE2-FG  TO D01-A15
              MOVE BW-BAD-MISC-PHONE2-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BW-BAD-CELL-FG NOT = OBW-BAD-CELL-FG
              MOVE "BW-BAD-CELL-FG" TO D01-TEXT
              MOVE BR-NO            TO D01-BRNO
              MOVE BW-SSNO          TO D01-SSNO
              MOVE OBW-BAD-CELL-FG  TO D01-A15
              MOVE BW-BAD-CELL-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
               IF OBW-MISC-PHONE(SUB) = 0202020202 OR
                   OBW-MISC-PHONE(SUB) NOT NUMERIC
                  MOVE 0 TO OBW-MISC-PHONE(SUB)
               END-IF
               IF BW-MISC-PHONE(SUB) NOT = OBW-MISC-PHONE(SUB)
                  MOVE SPACES TO D01-TEXT
                  STRING "BW-MISC-PHONE", SUB INTO D01-TEXT
                  MOVE BR-NO      TO D01-BRNO
                  MOVE BW-SSNO    TO D01-SSNO
                  MOVE OBW-MISC-PHONE(SUB)  TO D01-A15-NUM
                  MOVE BW-MISC-PHONE(SUB)   TO D01-S35-NUM
                  PERFORM WRITE-DETAIL-LINE-ERROR
               END-IF
               IF OBW-MISC-EXT(SUB) = 0202020202 OR
                   OBW-MISC-EXT(SUB) NOT NUMERIC
                  MOVE 0 TO OBW-MISC-EXT(SUB)
               END-IF
               IF BW-MISC-EXT(SUB) NOT = OBW-MISC-EXT(SUB)
                  MOVE SPACES TO D01-TEXT
                  STRING "BW-MISC-EXT", SUB INTO D01-TEXT
                  MOVE BR-NO      TO D01-BRNO
                  MOVE BW-SSNO    TO D01-SSNO
                  MOVE OBW-MISC-EXT(SUB)  TO D01-A15-NUM
                  MOVE BW-MISC-EXT(SUB)   TO D01-S35-NUM
                  PERFORM WRITE-DETAIL-LINE-ERROR
               END-IF
               IF BW-MISC-PHONE(SUB) = 0 AND
                   BW-MISC-EXT(SUB) = 0
                  MOVE SPACES TO OBW-MISC-PHONE-CD(SUB)
               END-IF
               IF BW-MISC-PHONE-CD(SUB) NOT = OBW-MISC-PHONE-CD(SUB)
                  MOVE SPACES TO D01-TEXT
                  STRING "BW-MISC-PHONE-CD", SUB INTO D01-TEXT
                  MOVE BR-NO      TO D01-BRNO
                  MOVE BW-SSNO    TO D01-SSNO
                  MOVE OBW-MISC-PHONE-CD(SUB)  TO D01-A15-NUM
                  MOVE BW-MISC-PHONE-CD(SUB)   TO D01-S35-NUM
                  PERFORM WRITE-DETAIL-LINE-ERROR
               END-IF
           END-PERFORM.

           IF BW-SRCD NOT = OBW-SRCD
              MOVE "BW-SRCD"  TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE BW-SSNO    TO D01-SSNO
              MOVE OBW-SRCD   TO D01-A15
              MOVE BW-SRCD    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BW-CALL-ON-SUNDAY NOT = OBW-CALL-ON-SUNDAY
              MOVE "BW-CALL-ON-SUNDAY" TO D01-TEXT
              MOVE BR-NO               TO D01-BRNO
              MOVE BW-SSNO             TO D01-SSNO
              MOVE OBW-CALL-ON-SUNDAY  TO D01-A15
              MOVE BW-CALL-ON-SUNDAY   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BW-SOLICIT NOT = OBW-SOLICIT
              MOVE "BW-SOLICIT" TO D01-TEXT
              MOVE BR-NO        TO D01-BRNO
              MOVE BW-SSNO      TO D01-SSNO
              MOVE OBW-SOLICIT  TO D01-A15
              MOVE BW-SOLICIT   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BW-CONFIDENTIAL NOT = OBW-CONFIDENTIAL
              MOVE "BW-CONFIDENTIAL" TO D01-TEXT
              MOVE BR-NO             TO D01-BRNO
              MOVE BW-SSNO           TO D01-SSNO
              MOVE OBW-CONFIDENTIAL  TO D01-A15
              MOVE BW-CONFIDENTIAL   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BW-PRIVACY-OPT-OUT NOT = OBW-PRIVACY-OPT-OUT
              MOVE "BW-PRIVACY-OPT-OUT" TO D01-TEXT
              MOVE BR-NO                TO D01-BRNO
              MOVE BW-SSNO              TO D01-SSNO
              MOVE OBW-PRIVACY-OPT-OUT  TO D01-A15
              MOVE BW-PRIVACY-OPT-OUT   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BW-CITY-TAX-CD NOT = OBW-CITY-TAX-CD
      *        CALL "C$JUSTIFY" USING OBW-CITY-TAX-CD, "L"
              IF ( BW-CITY-TAX-CD  NOT ALPHABETIC AND 
                   BW-CITY-TAX-CD  NOT NUMERIC )  OR 
                 (OBW-CITY-TAX-CD  NOT ALPHABETIC AND 
                  OBW-CITY-TAX-CD  NOT NUMERIC )   
                 NEXT SENTENCE
              ELSE
                 MOVE "BW-CITY-TAX-CD" TO D01-TEXT
                 MOVE BR-NO            TO D01-BRNO
                 MOVE BW-SSNO          TO D01-SSNO
                 MOVE OBW-CITY-TAX-CD  TO D01-A15
                 MOVE BW-CITY-TAX-CD   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OBW-DEALTRACK-CHG-DATE NOT NUMERIC OR
               (OBW-DEALTRACK-CHG-DATE = 202020) OR
                (OBW-DEALTRACK-CHG-DATE = 0)
              MOVE 0               TO OBW-DEALTRACK-CHG-DATE.

           MOVE OBW-DEALTRACK-CHG-DATE  TO DATE-MMDDYY
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
           PERFORM TEST-INVALID-DATE.
           IF BW-DEALTRACK-CHG-DATE NOT = DATE-YYYYMMDD
              MOVE "BW-DEALTRACK-CHG-DATE" TO D01-TEXT
              MOVE BR-NO          TO D01-BRNO
              MOVE BW-SSNO        TO D01-SSNO
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE BW-DEALTRACK-CHG-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
   
           IF OBW-CEASE-DESIST-DATE NOT NUMERIC OR
               (OBW-CEASE-DESIST-DATE = 202020) OR
                (OBW-CEASE-DESIST-DATE = 0)
              MOVE 0               TO OBW-CEASE-DESIST-DATE.

           IF BW-CEASE-DESIST-TYPE NOT = OBW-CEASE-DESIST-TYPE
              MOVE "BW-CEASE-DESIST-TYPE" TO D01-TEXT
              MOVE BR-NO                  TO D01-BRNO
              MOVE BW-SSNO                TO D01-SSNO
              MOVE OBW-CEASE-DESIST-TYPE  TO D01-A15
              MOVE BW-CEASE-DESIST-TYPE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OBW-CEASE-DESIST-DATE  TO DATE-MMDDYY
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
           PERFORM TEST-INVALID-DATE.
           IF BW-CEASE-DESIST-DATE NOT = DATE-YYYYMMDD
              MOVE "BW-CEASE-DESIST-DATE" TO D01-TEXT
              MOVE BR-NO                  TO D01-BRNO
              MOVE BW-SSNO                TO D01-SSNO
              MOVE DATE-YYYYMMDD          TO D01-A15
              MOVE BW-CEASE-DESIST-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
   
           IF OBW-UMC-EXPIRE-DATE NOT NUMERIC OR
               (OBW-UMC-EXPIRE-DATE  = 202020) OR
                (OBW-UMC-EXPIRE-DATE = 0)
              MOVE 0               TO OBW-UMC-EXPIRE-DATE.

           MOVE OBW-UMC-EXPIRE-DATE  TO DATE-MMDDYY
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
           PERFORM TEST-INVALID-DATE.
           IF BW-UMC-EXPIRE-DATE NOT = DATE-YYYYMMDD
              MOVE "BW-UMC-EXPIRE-DATE"   TO D01-TEXT
              MOVE BR-NO                  TO D01-BRNO
              MOVE BW-SSNO                TO D01-SSNO
              MOVE DATE-YYYYMMDD          TO D01-A15
              MOVE BW-UMC-EXPIRE-DATE     TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OBW-UMC-PR-EXPIRE-DATE NOT NUMERIC OR
               (OBW-UMC-PR-EXPIRE-DATE  = 202020) OR
                (OBW-UMC-PR-EXPIRE-DATE = 0)
              MOVE 0               TO OBW-UMC-PR-EXPIRE-DATE.

           MOVE OBW-UMC-PR-EXPIRE-DATE  TO DATE-MMDDYY
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
           PERFORM TEST-INVALID-DATE.
           IF BW-UMC-PR-EXPIRE-DATE NOT = DATE-YYYYMMDD
              MOVE "BW-UMC-PR-EXPIRE-DATE" TO D01-TEXT
              MOVE BR-NO                   TO D01-BRNO
              MOVE BW-SSNO                 TO D01-SSNO
              MOVE DATE-YYYYMMDD           TO D01-A15
              MOVE BW-UMC-PR-EXPIRE-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BW-WORKHRS NOT = OBW-WORKHRS
      *        CALL "C$JUSTIFY" USING OBW-WORKHRS, "L"
              IF ( BW-WORKHRS  NOT ALPHABETIC AND 
                   BW-WORKHRS  NOT NUMERIC )  OR 
                 (OBW-WORKHRS  NOT ALPHABETIC AND 
                  OBW-WORKHRS  NOT NUMERIC )   
                 NEXT SENTENCE
              ELSE
                 MOVE "BW-WORKHRS" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE BW-SSNO    TO D01-SSNO
                 MOVE OBW-WORKHRS  TO D01-A15
                 MOVE BW-WORKHRS   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
       
      * GET BORROWER FIELDS OUT OF OLD BX BORROWER TRAILER
           MOVE OBW-SSNO TO OBX-SSNO.
           PERFORM READ-OBX1-FILE.
           IF IO-FG NOT = 0
              GO TO EXIT-COMPARE-FIELDS.

           IF BW-EMAIL NOT = OBX-EMAIL
      *        CALL "C$JUSTIFY" USING OBX-EMAIL, "L"
              IF ( BW-EMAIL  NOT ALPHABETIC AND 
                   BW-EMAIL  NOT NUMERIC )  OR 
                 (OBX-EMAIL  NOT ALPHABETIC AND 
                  OBX-EMAIL  NOT NUMERIC )   
                 NEXT SENTENCE
              ELSE
                 MOVE "BW-EMAIL" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE BW-SSNO    TO D01-SSNO
                 MOVE OBX-EMAIL  TO D01-A15
                 MOVE BW-EMAIL   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
          
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
               IF OBX-CREDIT-SCORE(SUB) NOT NUMERIC
                  MOVE 0 TO OBX-CREDIT-SCORE(SUB)
               END-IF
               IF BW-CRED-SCORE(SUB) NOT = OBX-CREDIT-SCORE(SUB)
                  MOVE SPACES TO D01-TEXT
                  STRING "BW-CRED-SCORE", SUB INTO D01-TEXT
                  MOVE "BW-CRED-SCORE(SUB)" TO D01-TEXT
                  MOVE BR-NO      TO D01-BRNO
                  MOVE BW-SSNO    TO D01-SSNO
                  MOVE OBX-CREDIT-SCORE(SUB)  TO D01-A15-NUM
                  MOVE BW-CRED-SCORE(SUB)   TO D01-S35-NUM
                  PERFORM WRITE-DETAIL-LINE-ERROR
               END-IF
               IF OBX-BNKRPT-SCORE(SUB) NOT NUMERIC
                  MOVE 0 TO OBX-BNKRPT-SCORE(SUB)
               END-IF
               IF BW-BNKRPT-SCORE(SUB) NOT = OBX-BNKRPT-SCORE(SUB)
                  MOVE SPACES TO D01-TEXT
                  STRING "BW-BNKRPT-SCORE", SUB INTO D01-TEXT
                  MOVE "BW-BNKRPT-SCORE(SUB)" TO D01-TEXT
                  MOVE BR-NO      TO D01-BRNO
                  MOVE BW-SSNO    TO D01-SSNO
                  MOVE OBX-BNKRPT-SCORE(SUB)  TO D01-A15-NUM
                  MOVE BW-BNKRPT-SCORE(SUB)   TO D01-S35-NUM
                  PERFORM WRITE-DETAIL-LINE-ERROR
               END-IF
           END-PERFORM.
          
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

           MOVE "BEGINNING SSNO                : "    TO R01-PROMPT.
           MOVE BEG-SSNO                              TO R01-NUMERIC.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    SSNO                : "    TO R01-PROMPT.
           MOVE END-SSNO                              TO R01-NUMERIC.
           MOVE 01                                    TO LN-FEED.
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

           MOVE "A15 RECORDS PROCESSED         : "    TO R01-PROMPT.
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
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/PROGID.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO BWFILE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BWFILE-SYSDATE.
           DISPLAY BWFILE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE BWFILE-FORM-FIELDS TO WR-BUF.
           MOVE BWFILE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE BWFILE-HELP--FILE TO HELP-LINK-FILE.
           MOVE BWFILE-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/BWFILE_EVA.CPY".
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
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
      * COPY "LIBLP/LPBW1RN.CPY".
      * COPYMEMBER: LIBLP/LPBW1RN
       LOAD-BW1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BW-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BW-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BW-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BW-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO BW-ALL-DATA
              MOVE BW-ALL-PATH             TO BW-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BW-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BW-ALL-BASE
              MOVE FILPATH-MACHINE         TO BW-ALL-MACHINE
              MOVE FILPATH-DATA            TO BW-ALL-DATA
              MOVE BW-ALL-PATH             TO BW-PATH.

      *-----------------------------------------------------------------
       OPEN-BW1-FILE.
           PERFORM LOAD-BW1-FILE.
           PERFORM OPEN-IT.
           MOVE BW-PATH-SQL TO E-FILE.

           IF BW1-CONN-OPEN-SW = "N" 
              MOVE "Y"         TO BW1-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS BW1_CONNECT 
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-BW1-FILE.
           PERFORM START-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR.
           MOVE BW-PATH-SQL    TO E-FILE.
           MOVE BW1-KEY        TO E-KEYX.

           IF ( BW1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BW1-FILE.

           IF ( QBW1-WEND-SSNO = ZEROES    ) OR
              ( QBW1-WEND-SSNO NOT NUMERIC )
              MOVE BW-OWNBR             TO QBW1-WBEG-OWNBR
                                           QBW1-WEND-OWNBR
              MOVE BW-SSNO              TO QBW1-WBEG-SSNO
              MOVE ALL "9"              TO QBW1-WEND-SSNO.

           EXEC SQL SET CONNECTION BW1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE BW1_CURSOR CURSOR FOR
                :BW1-RETURN-CODE = EXEC SSP_BWFILE_BWFILE_SELECT 
              ( :QBW1-WBEG-OWNBR,     
                :QBW1-WBEG-SSNO,   
                :QBW1-WEND-SSNO )      
           END-EXEC.

           EXEC SQL
               OPEN BW1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QBW1-WBEG-OWNBR 
                          QBW1-WEND-OWNBR
                          QBW1-WBEG-SSNO
                          QBW1-WEND-SSNO.

           MOVE STAT---GOOD TO BW1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-BW1-FILE.
           PERFORM READ-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR. 
           MOVE BW-PATH-SQL    TO E-FILE.
           MOVE BW1-KEY        TO E-KEYX.
           INITIALIZE QBW-REC.

           MOVE BW-OWNBR   TO QBW-OWNBR.
           MOVE BW-SSNO    TO QBW-SSNO.

           EXEC SQL
            SELECT BWFILE.BW_OWNBR,
                   BWFILE.BW_SSNO,
                   BWFILE.BW_LNAME,
                   BWFILE.BW_FNAME,
                   BWFILE.BW_MNAME,
                   BWFILE.BW_NICKNAME,
                   BWFILE.BW_GENERATION,
                   BWFILE.BW_ADR1,
                   BWFILE.BW_ADR2,
                   BWFILE.BW_CITY,
                   BWFILE.BW_STATE,
                   BWFILE.BW_ZIP,
                   CAST(BWFILE.BW_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   CAST(BWFILE.BW_REFDATE AS VARCHAR(10)),
                   CAST(BWFILE.BW_BDATE AS VARCHAR(10)),
                   BWFILE.BW_PHONE,
                   BWFILE.BW_CELL_PHONE,
                   BWFILE.BW_NATIONALITY,
                   BWFILE.BW_GENDER,
                   BWFILE.BW_DEPEND,
                   BWFILE.BW_RENTCD,
                   BWFILE.BW_TIMEADDR,
                   BWFILE.BW_CREDITCD,
                   BWFILE.BW_STRTGCD,
                   BWFILE.BW_DWELLCD,
                   BWFILE.BW_COUNTY,
                   BWFILE.BW_ORIGIN,
                   BWFILE.BW_CENSUSCD,
                   BWFILE.BW_ORGSLSMN,
                   BWFILE.BW_REPCODE,
                   BWFILE.BW_SPSSNO,
                   BWFILE.BW_SPLNAME,
                   BWFILE.BW_SPFNAME,
                   CAST(BWFILE.BW_SP_BDATE AS VARCHAR(10)),
                   BWFILE.BW_EMPLOYER,
                   BWFILE.BW_EMPLPOS,
                   BWFILE.BW_EMPPHONE,
                   BWFILE.BW_EMPEXT,
                   BWFILE.BW_EMPYEARS,
                   BWFILE.BW_EMPPAY,
                   BWFILE.BW_EMPPAYOT,
                   BWFILE.BW_EMPPAYSR,
                   BWFILE.BW_EMPADDR1,
                   BWFILE.BW_EMPADDR2,
                   BWFILE.BW_EMPCITY,
                   BWFILE.BW_EMPSTATE,
                   BWFILE.BW_EMPZIP,
                   BWFILE.BW_WORKHRS,
                   BWFILE.BW_LOANS,
                   BWFILE.BW_INTYTD,
                   BWFILE.BW_INTLYR,
                   BWFILE.BW_LIASMLN,
                   BWFILE.BW_LIAOTH,
                   CAST(BWFILE.BW_LASTPODATE
                                                  AS VARCHAR(10)),
                   BWFILE.BW_PRLOANS,
                   BWFILE.BW_RE_INTYTD,
                   BWFILE.BW_RE_INTLYR,
                   BWFILE.BW_CREDIT_LIMIT,
                   BWFILE.BW_CREDIT_SCORE,
                   BWFILE.BW_CRED_SCORE_1,
                   BWFILE.BW_CRED_SCORE_2,
                   BWFILE.BW_CRED_SCORE_3,
                   BWFILE.BW_CRED_SCORE_4,
                   BWFILE.BW_BNKRPT_SCORE_1,
                   BWFILE.BW_BNKRPT_SCORE_2,
                   BWFILE.BW_BNKRPT_SCORE_3,
                   BWFILE.BW_BNKRPT_SCORE_4,
                   BWFILE.BW_LAST_FORM_NO,
                   CAST(BWFILE.BW_LAST_FORM_DATE
                                                  AS VARCHAR(10)),
                   CAST(BWFILE.BW_PRIOR_RN_FOR10_DATE
                                                  AS VARCHAR(10)),
                   CAST(BWFILE.BW_LAST_RN_FOR10_DATE
                                                  AS VARCHAR(10)),
                   CAST(BWFILE.BW_ADR_PHONE_CHG_DATE
                                                  AS VARCHAR(10)),
                   BWFILE.BW_BAD_ADR_FG,
                   BWFILE.BW_BAD_PHONE_FG,
                   BWFILE.BW_BAD_EMPPHONE_FG,
                   BWFILE.BW_BAD_MISC_PHONE1_FG,
                   BWFILE.BW_BAD_MISC_PHONE2_FG,
                   BWFILE.BW_BAD_CELL_FG,
                   BWFILE.BW_MISC_PHONE_1,
                   BWFILE.BW_MISC_EXT_1,
                   BWFILE.BW_MISC_PHONE_CD_1,
                   BWFILE.BW_MISC_PHONE_2,
                   BWFILE.BW_MISC_EXT_2,
                   BWFILE.BW_MISC_PHONE_CD_2,
                   BWFILE.BW_MISC_PHONE_3,
                   BWFILE.BW_MISC_EXT_3,
                   BWFILE.BW_MISC_PHONE_CD_3,
                   BWFILE.BW_MISC_PHONE_4,
                   BWFILE.BW_MISC_EXT_4,
                   BWFILE.BW_MISC_PHONE_CD_4,
                   BWFILE.BW_MISC_PHONE_5,
                   BWFILE.BW_MISC_EXT_5,
                   BWFILE.BW_MISC_PHONE_CD_5,
                   BWFILE.BW_MISC_PHONE_6,
                   BWFILE.BW_MISC_EXT_6,
                   BWFILE.BW_MISC_PHONE_CD_6,
                   BWFILE.BW_CONTACT_DESC_1,
                   BWFILE.BW_CONTACT_PHONE_1,
                   BWFILE.BW_CONTACT_EXT_1,
                   BWFILE.BW_CONTACT_RELATION_1,
                   BWFILE.BW_CONTACT_DESC_2,
                   BWFILE.BW_CONTACT_PHONE_2,
                   BWFILE.BW_CONTACT_EXT_2,
                   BWFILE.BW_CONTACT_RELATION_2,
                   BWFILE.BW_CONTACT_DESC_3,
                   BWFILE.BW_CONTACT_PHONE_3,
                   BWFILE.BW_CONTACT_EXT_3,
                   BWFILE.BW_CONTACT_RELATION_3,
                   BWFILE.BW_CONTACT_DESC_4,
                   BWFILE.BW_CONTACT_PHONE_4,
                   BWFILE.BW_CONTACT_EXT_4,
                   BWFILE.BW_CONTACT_RELATION_4,
                   BWFILE.BW_CONTACT_DESC_5,
                   BWFILE.BW_CONTACT_PHONE_5,
                   BWFILE.BW_CONTACT_EXT_5,
                   BWFILE.BW_CONTACT_RELATION_5,
                   BWFILE.BW_CONTACT_DESC_6,
                   BWFILE.BW_CONTACT_PHONE_6,
                   BWFILE.BW_CONTACT_EXT_6,
                   BWFILE.BW_CONTACT_RELATION_6,
                   BWFILE.BW_CONTACT_DESC_7,
                   BWFILE.BW_CONTACT_PHONE_7,
                   BWFILE.BW_CONTACT_EXT_7,
                   BWFILE.BW_CONTACT_RELATION_7,
                   BWFILE.BW_CONTACT_DESC_8,
                   BWFILE.BW_CONTACT_PHONE_8,
                   BWFILE.BW_CONTACT_EXT_8,
                   BWFILE.BW_CONTACT_RELATION_8,
                   BWFILE.BW_CONTACT_DESC_9,
                   BWFILE.BW_CONTACT_PHONE_9,
                   BWFILE.BW_CONTACT_EXT_9,
                   BWFILE.BW_CONTACT_RELATION_9,
                   BWFILE.BW_CONTACT_DESC_10,
                   BWFILE.BW_CONTACT_PHONE_10,
                   BWFILE.BW_CONTACT_EXT_10,
                   BWFILE.BW_CONTACT_RELATION_10,
                   BWFILE.BW_SRCD,
                   BWFILE.BW_CALL_ON_SUNDAY,
                   BWFILE.BW_SOLICIT,
                   BWFILE.BW_CONFIDENTIAL,
                   BWFILE.BW_PRIVACY_OPT_OUT,
                   BWFILE.BW_OPT_IN_EMAIL,
                   BWFILE.BW_OPT_IN_TEXT,
                   BWFILE.BW_CITY_TAX_CD,
                   CAST(BWFILE.BW_DEALTRACK_CHG_DATE
                                                  AS VARCHAR(10)),
                   BWFILE.BW_DECEASED,
                   CAST(BWFILE.BW_DECEASED_DATE
                                                  AS VARCHAR(10)),
                   BWFILE.BW_EMAIL,
                   BWFILE.BW_IDENT_NO,
                   BWFILE.BW_IDENT_TYPE,
                   BWFILE.BW_IDENT_STATE,
                   BWFILE.BW_MILITARY_FLAG,
                   BWFILE.BW_ORGBR,
                   CAST(BWFILE.BW_CEASE_DESIST_DATE
                                                  AS VARCHAR(10)),
                   BWFILE.BW_CEASE_DESIST_TYPE,
                   CAST(BWFILE.BW_UMC_EXPIRE_DATE
                                                  AS VARCHAR(10)),
                   CAST(BWFILE.BW_UMC_PR_EXPIRE_DATE
                                                  AS VARCHAR(10))

            INTO :QBW-REC
            FROM DBO.BWFILE
            WHERE BWFILE.BW_OWNBR = :QBW-OWNBR
              AND BWFILE.BW_SSNO = :QBW-SSNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BW1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-BW-FIELDS.

      *-----------------------------------------------------------------
       READ-BW1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR.
           MOVE BW-PATH-SQL    TO E-FILE.
           MOVE BW1-KEY        TO E-KEYX.
           INITIALIZE QBW-REC.

           EXEC SQL SET CONNECTION BW1_CONNECT END-EXEC.

           IF ( BW1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BW1_CURSOR INTO :QBW-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BW1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BW-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-BW1-FILE.

           PERFORM CLOSE-IT.
           IF ( BW1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION BW1_CONNECT END-EXEC
              EXEC SQL 
                   CLOSE BW1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC 
              MOVE STAT---BAD TO BW1-CURSOR-STAT.

      *-----------------------------------------------------------------

       CLOSE-BW1-CONNECTION.

           IF BW1-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION BW1_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT BW1_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO BW1-CONN-OPEN-SW.

      *-----------------------------------------------------------------
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
       OPEN-OBW1-FILE.
           PERFORM OPEN-IT.
           MOVE OBW-PATH TO E-FILE.
           OPEN INPUT OBW-FILE.
           IF IO-FG = 8
              GO TO OPEN-OBW1-FILE.
           IF IO-FG = 7
              CLOSE OBW-FILE
              GO TO OPEN-OBW1-FILE.
      *READ-OBW1-FILE.
      *    PERFORM READ-IT.
      *    MOVE OBW-PATH TO E-FILE.
      *    MOVE OBW1-KEY TO E-KEYX.
      *    READ OBW-FILE.
      *    IF IO-FG = 8
      *       GO TO READ-OBW1-FILE.
       READ-OBW1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OBW-PATH TO E-FILE.
           MOVE OBW1-KEY TO E-KEYX.
           READ OBW-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OBW1-FILE-NEXT.
       START-OBW1-FILE.
           PERFORM START-IT.
           MOVE OBW-PATH TO E-FILE.
           MOVE OBW1-KEY TO E-KEYX.
           START OBW-FILE KEY NOT < OBW1-KEY.
       CLOSE-OBW1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OBW-FILE.
       OPEN-OBX1-FILE.
           PERFORM OPEN-IT.
           MOVE OBX-PATH TO E-FILE.
           OPEN INPUT OBX-FILE.
           IF IO-FG = 8
              GO TO OPEN-OBX1-FILE.
           IF IO-FG = 7
              CLOSE OBX-FILE
              GO TO OPEN-OBX1-FILE.
       READ-OBX1-FILE.
           PERFORM READ-IT.
           MOVE OBX-PATH TO E-FILE.
           MOVE OBX1-KEY TO E-KEYX.
           READ OBX-FILE.
           IF IO-FG = 8
              GO TO READ-OBX1-FILE.
       CLOSE-OBX1-FILE.
           PERFORM CLOSE-IT.

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
              DESTROY BWFILE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
