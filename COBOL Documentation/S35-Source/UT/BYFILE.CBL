       IDENTIFICATION DIVISION.
       PROGRAM-ID.    BYFILE.
       DATE-WRITTEN.  11-06-2023.
      ******************************************************************
      * DESCRIPTION   : COMPARE S35 BYFILE WITH A15 BYFILE
      *            
      *                 TO BE USED AFTER AN UPDATE
      *                 WILL COMPARE EACH FIELD AND REPORT ANY 
      *                 DIFFERENCES, AS LONG AS FIELD EXIST IN BOTH FILES
      *
      *  LOOPS THRU THE A15 FILE AND VERIFIES THE S35 FILE EXIST AND MATCHES
      *
      *  01-07-25  RMZ  ADDED 'BTRACK' - PROCESS TRACKING TO PROGRAM.
      *  01-13-25  MB   CHANGED TO READ A15 DURING PROCESSING - NOT LOAD 
      *                  INTO WORK VISION FILE.
      *  02-19-25  MB   CHANGED TO DEFAUL BATCH REQUEST DATE RANGE TO
      *                  201401 TO 29912  
      *  02-20-25  MB   ADD BRANCH(GROUP) INPUT PARMS
      *  03-05-25  MB   WILL PASS BRANCH LIST TO SSP IF < 1501 BRANCHES REQUESTED
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
      * COPYMEMBER: LIBLP/LPFSBY
           SELECT OBYR1-FILE ASSIGN TO OBYR1-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OBYR11-KEY
                  FILE STATUS FILE-STAT.

           SELECT OBYR2-FILE ASSIGN TO OBYR2-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OBYR21-KEY
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  OBYR1-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A15SRC/LP01BY.CPY" REPLACING LEADING "BY" BY "OBYR1".

       FD  OBYR2-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A15SRC/LP01BY.CPY" REPLACING LEADING "BY" BY "OBYR2".

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/BYFILE S35 04/15/25-04:40:43 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01BY.CPY".
       COPY "LIBLP/LP01BYA.CPY".
       COPY "LIBLP/LP01BYB.CPY".
       COPY "LIBLP/LP01BY_SQL.CPY".
       COPY "LIBLP/LPWSBY.CPY".

       COPY "LIBGB/GRSSPW.CPY".
       COPY "LIBGB/GRBRLSW.CPY".

       01  OBYR1-PATH.
           03  OBYR1-ALL-BASE          PIC X(9).
           03  FILLER                  PIC X     VALUE "/".
           03  OBYR1-ALL-MACHINE       PIC XX    VALUE "R1".
           03  FILLER                  PIC X(23) VALUE "/LP/BYFILE".

       01  OBYR2-PATH.
           03  OBYR2-ALL-BASE          PIC X(9).
           03  FILLER                  PIC X     VALUE "/".
           03  OBYR2-ALL-MACHINE       PIC XX    VALUE "R2".
           03  FILLER                  PIC X(23) VALUE "/LP/BYFILE".

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

       01  SETBY-DATE-WORKERS.
           03  SETBY-DATE          PIC 9(5)    VALUE 0.
           03  FILLER              REDEFINES SETBY-DATE.
               05  SETBY-YYY       PIC 999.
               05  SETBY-MM        PIC 99.

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

       01  PROCESS-STAT                PIC X(08)    VALUE SPACES.
           88  PROCESS-STAT-GOOD                    VALUE "00".
           88  PROCESS-STAT-BAD                     VALUE "  " THRU "//"
                                                         "01" THRU "~~".

       01  USE-OBY-REC.
           03  USE-OBY-KEY.
               05  USE-OBY-KEY-DATE         PIC 9(6).
               05  USE-OBY-KEY-BRNO         PIC 9(4).
               05  USE-OBY-KEY-CLASS        PIC 9(3).

       COPY "LIBUP/A15SRC/LP01BY.CPY" REPLACING LEADING "BY" BY "OBY".

       01  BY-EOF-SW                   PIC X    VALUE "N".
       01  BYR1-EOF-SW                 PIC X    VALUE "N".
       01  BYR2-EOF-SW                 PIC X    VALUE "N".
       01  BY1-CONN-OPEN-SW            PIC X    VALUE "N".
       01  TOTAL-ERRORS-CTR            PIC 9(9) VALUE 0.
       01  SQL-RECORDS-PROCESSED       PIC 9(9) VALUE 0.
       01  A15-RECORDS-PROCESSED       PIC 9(9) VALUE 0.

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

       01  PROC-BRANCH-ARRAY           OCCURS 9999 TIMES
                                       PIC X.

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
           03  BEG-DATE                PIC 9(06).
           03  END-DATE                PIC 9(06).
           03  GROUP-FLAG              PIC X(01).
           03  ENT-GROUP               PIC X(04).

       01  VERSION-CONTROL             PIC X(01)     VALUE "B".

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
           "BRANCH STAT (BYFILE) COMPARE A15 TO S35".

       01  HEADER-LINE-03.
           03  FILLER                PIC X(4)  VALUE "BRNO".
           03  FILLER                PIC X(2)  VALUE SPACES.
           03  FILLER                PIC X(6)  VALUE " DATE ".
           03  FILLER                PIC X(2)  VALUE SPACES.
           03  FILLER                PIC X(3)  VALUE "CLS".
           03  FILLER                PIC X(2)  VALUE SPACES.
           03  FILLER                PIC X(28) VALUE "DESCRIPTION".
           03  FILLER                PIC X(3)  VALUE "A15".
           03  FILLER                PIC X(19) VALUE SPACES.
           03  FILLER                PIC X(3)  VALUE "S35".

       01  DETAIL-LINE                        PIC X(100)   VALUE SPACES.

       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-BRNO                       PIC ZZZZ.
           03  FILLER                         PIC XX.
           03  D01-DATE                       PIC 9(6).
           03  FILLER                         PIC X(2).
           03  D01-CLASS                      PIC 999.
           03  FILLER                         PIC XX.
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
       COPY "LIBUT/BYFILE_DEF.CPY".
       COPY "LIBUT/BYFILE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBWI/GRSCANW.CPY".


      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/BYFILE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE OBYR1-FILE OBYR2-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-BYA1-FILE.
           PERFORM CLOSE-BYB1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE OBYR1-FILE OBYR2-FILE.
           PERFORM CLOSE-BY1-CONNECTION.
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
           MOVE "BYFILE" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE BYFILE-QUEUE-POS TO Q-POS.
           MOVE BYFILE-COPIES-POS TO C-POS.
           MOVE BYFILE-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           PERFORM GET-TIME.
           MOVE TIME-EDIT  TO STARTING-TIME.

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

      * LOAD BRANCHES TO PROCESS USING STORED PROCEDURE SSP_TCLP_BRANCH_GROUP_LOGIC_SELECT
      *   INTO BGLS-BRANCH-TBL
           IF GROUP-FLAG = "Y"
               MOVE ENT-GROUP TO BGLS-PASS-BEG-BRGRP
               MOVE SPACES    TO BGLS-PASS-END-BRGRP
           ELSE
               MOVE BEG-BRANCH                TO BRANCHLIST-WORK-BRNO
               MOVE BRANCHLIST-WORK-BRNO-TEXT TO BGLS-PASS-BEG-BRGRP
               MOVE END-BRANCH                TO BRANCHLIST-WORK-BRNO
               MOVE BRANCHLIST-WORK-BRNO-TEXT TO BGLS-PASS-END-BRGRP.

           PERFORM LOAD-BGLS-BRANCH-LIST.

           IF BGLS-BRANCHES-RETR-CTR < 1501
              PERFORM VARYING SUB FROM 1 BY 1 
               UNTIL SUB > BGLS-BRANCHES-RETR-CTR
                 MOVE BGLS-BRANCH-BRNO(SUB) TO S1 BRANCHLIST-BRNO(SUB)
                 IF SUB > 1
                    MOVE ","   TO BRANCHLIST-COMMA(SUB)
                 END-IF 
                 MOVE "Y" TO PROC-BRANCH-ARRAY(S1) 
              END-PERFORM
              MOVE BRANCHLIST-TBL TO PASS-BRANCHLIST
           ELSE
              MOVE SPACES TO PASS-BRANCHLIST
              PERFORM VARYING SUB FROM 1 BY 1 
               UNTIL SUB > BGLS-BRANCHES-RETR-CTR
                 MOVE BGLS-BRANCH-BRNO(SUB) TO S1
                 MOVE "Y" TO PROC-BRANCH-ARRAY(S1)
              END-PERFORM.

           GO TO INITIALIZATION-EXIT.

       INITIALIZATION-END.
           MOVE "X"        TO EXT-EOBUF-ERRCD.
           MOVE STAT---BAD TO PROCESS-STAT.

       INITIALIZATION-EXIT.
           EXIT.

      ***************************************************
       ENTER-PARAMETERS SECTION.
      ***************************************************
           MOVE STAT---GOOD TO PROCESS-STAT.

           IF ( UP-KEY )
              GO TO ENTER-END-DATE.

       ENTER-SOURCE-PATH.

           IF SOURCE-PATH = SPACES
              MOVE "A15_PATH"       TO GETENV-BUF
              PERFORM GETENV-CALL
              IF ( STAT-GOOD )
                 MOVE GETENV-BUF    TO SOURCE-PATH
              ELSE
                 MOVE "/US2/DA15"   TO SOURCE-PATH
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

           IF (        UP-KEY ) GO TO ENTER-SOURCE-PATH.
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
       ENTER-BEG-DATE.

           IF (        UP-KEY ) GO TO ENTER-SOURCE-PATH. 
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END. 

           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO ENTER-PARAMETERS-END.

           MOVE "ENNN060BEG-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-SOURCE-PATH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F3-KEY ) GO TO ENTER-ALL-DATE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-DATE.

           IF ( VDUNUM0 = ZEROES )
              GO TO ENTER-BEG-DATE.

           MOVE VDUNUM0 TO DATE-YYYYMM.

           IF ( DATE-YYYYMM-CC NOT = 19 ) AND
              ( DATE-YYYYMM-CC NOT = 20 )
              MOVE "ERROR: INVALID DATE - CC" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DATE.

           IF ( DATE-YYYYMM-MM < 01 ) OR
              ( DATE-YYYYMM-MM > 12 )
              MOVE "ERROR: INVALID DATE - MM 01 THRU 12 " TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DATE.

              IF DATE-YYYYMM < 201401
              MOVE "ERROR: BEGIN DATE < 201401 - INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DATE.

           MOVE VDUNUM0 TO BEG-DATE.

      *-----------------------------------------------------------------
       ENTER-END-DATE.

           MOVE "ENNN060END-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-DATE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-DATE.

           MOVE VDUNUM0 TO DATE-YYYYMM.

           IF ( DATE-YYYYMM-CC NOT = 19 ) AND
              ( DATE-YYYYMM-CC NOT = 20 )
              MOVE "ERROR: INVALID DATE - CC" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-END-DATE.

           IF ( DATE-YYYYMM-MM < 01 ) OR
              ( DATE-YYYYMM-MM > 12 )
              MOVE "ERROR: INVALID DATE - MM 01 THRU 12 " TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-END-DATE.

              IF DATE-YYYYMM < 201401
              MOVE "ERROR: END DATE < 201401 - INVALID " TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-END-DATE.

           MOVE VDUNUM0 TO END-DATE.

           IF ( BEG-DATE > END-DATE )
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DATE.

           GO TO ENTER-PARAMETERS-END.

      *-----------------------------------------------------------------
       ENTER-ALL-DATE.

           MOVE "SNNN060BEG-DATE."  TO VDU.
           MOVE 201401              TO BEG-DATE VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020END-DATE."  TO VDU.
           MOVE 209912              TO END-DATE VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       ENTER-PARAMETERS-END.

           MOVE LEGEND-ACCEPT-YN TO LEGEND.
           PERFORM SEND-LEGEND.

           IF ( F1-KEY ) MOVE STAT---BAD   TO PROCESS-STAT.
           IF ( F7-KEY ) MOVE STAT---BAD   TO PROCESS-STAT.

       ENTER-PARAMETERS-EXIT.
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

      ****************************************
       EO-EXEC-AUDIT SECTION.
      ****************************************

      *     MOVE EXT-EOBUF-DATE1 TO DATE-YYYYMMDD.
      *     PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
      *     IF DATE-YYYYMM < 201401
      *        MOVE 201401 TO DATE-YYYYMM.
      *     MOVE DATE-YYYYMM TO BEG-DATE.
      *     MOVE EXT-EOBUF-DATE2 TO DATE-YYYYMMDD.
      *     PERFORM CONVERT-YYYYMMDD-TO-YYYYMM.
      *     MOVE DATE-YYYYMM TO END-DATE.

           MOVE 201401 TO BEG-DATE.
           MOVE 209912 TO END-DATE.

      ****************************************
       DISPLAY-PARAMETERS SECTION.
      ****************************************
           MOVE "S  A090S-PATH."        TO VDU.     
           MOVE SOURCE-PATH             TO VDUBUF.
           PERFORM SCREENIO.

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

       DISPLAY-PARAMETERS-EXIT.
           EXIT.

      *******************************************
       EXECUTION-MODULE SECTION.
      *******************************************
 
           MOVE 9999       TO PROGID-SLEEP-COUNTER.
           PERFORM SEND-PROGRESS-INDICATOR.

      D    STOP MESS.

           MOVE EXT-FILPATH-BASE      TO FILPATH-BASE.
           MOVE MACHINE-R1            TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH          TO FILPATH-DATA.

           CALL "C$TOLOWER" USING OBYR1-ALL-MACHINE VALUE 2.
           CALL "C$TOLOWER" USING OBYR2-ALL-MACHINE VALUE 2.
           MOVE SOURCE-PATH    TO OBYR1-ALL-BASE OBYR2-ALL-BASE.

           MOVE OBYR1-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE ZEROS                   TO OBY-BRNO
              MOVE "MISSING R1 A15 BYFILE" TO D01-TEXT
              MOVE OBYR1-PATH              TO D01-A15
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXECUTION-MODULE-EXIT.

           MOVE OBYR2-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE ZEROS                   TO OBY-BRNO
              MOVE "MISSING R2 A15 BYFILE" TO D01-TEXT
              MOVE OBYR2-PATH              TO D01-A15
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXECUTION-MODULE-EXIT.

           MOVE SPACES TO PROGID-TEXT.

           MOVE BEG-DATE TO QBY1-WBEG-DATE-CCYYMM.
           MOVE END-DATE TO QBY1-WEND-DATE-CCYYMM.
           MOVE ZEROS    TO QBY1-WBEG-BRNO QBY1-WBEG-CLASS.
           MOVE ALL '9'  TO QBY1-WEND-BRNO QBY1-WEND-CLASS.

           PERFORM OPEN-BYA1-FILE.

           MOVE "PROCESSING S35 DATA" TO BTRACK-MESS.
           PERFORM BTRACK-GLOBAL.

           PERFORM START-BYB1-FILE.  *> SSP  BYBFILE
           PERFORM START-BYA1-FILE.  *> SSP  BYAFILE
           PERFORM M-READ-BYAB.

           MOVE BEG-DATE TO DATE-YYYYMM.
           PERFORM CONVERT-YYYYMM-TO-MMDDYY.
      
           MOVE DATE-MMDDYY-MM TO SETBY-MM.
           MOVE DATE-MMDDYY-YY TO SETBY-YYY.
      * JULIAN-CC = 50
           IF SETBY-YYY < EXT-JULIAN-CC
              ADD 100 TO SETBY-YYY.

           MOVE SETBY-DATE  TO OBYR1-DATE OBYR2-DATE.
           MOVE ZEROS       TO OBYR1-BRNO OBYR1-CLASS
                               OBYR2-BRNO OBYR2-CLASS.

           MOVE "PROCESSING A15 DATA" TO BTRACK-MESS.
           PERFORM BTRACK-GLOBAL.

           PERFORM OPEN-OBY1R1-FILE.
           PERFORM START-OBY1R1-FILE.
           PERFORM M-READ-OBY-R1.

           PERFORM OPEN-OBY1R2-FILE.
           PERFORM START-OBY1R2-FILE.
           PERFORM M-READ-OBY-R2.

           PERFORM DETERMINE-OBY-REC-TO-USE.

           PERFORM PROCESS-BRANCH-NEXT-BY
              UNTIL BYR1-EOF-SW = "Y" AND BYR2-EOF-SW = "Y"
               AND BY-EOF-SW = "Y".

            GO TO EXECUTION-MODULE-EXIT.

       PROCESS-BRANCH-NEXT-BY.
           PERFORM SEND-PROGRESS-INDICATOR.

           IF PROC-BRANCH-ARRAY(USE-OBY-KEY-BRNO) = "Y" AND
              PROC-BRANCH-ARRAY(BY-BRNO) = "Y"
              PERFORM COMPARE-FIELDS.

           IF USE-OBY-KEY < BY1-KEY
              PERFORM M-READ-OBY
           ELSE
              IF USE-OBY-KEY > BY1-KEY
                 PERFORM M-READ-BYAB
              ELSE
                 PERFORM M-READ-OBY
                 PERFORM M-READ-BYAB.

           IF BYA1-KEY NOT = BYB1-KEY
              MOVE ZEROS                       TO OBY-BRNO
              MOVE "SQL BYA KEY NOT = BYB KEY" TO D01-TEXT
              MOVE BYA1-KEY                    TO D01-A15
              MOVE BYB1-KEY                    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR
              MOVE "Y" TO BYR1-EOF-SW BY-EOF-SW BYR2-EOF-SW.

       EXECUTION-MODULE-EXIT.
           EXIT.

       M-READ-BYAB SECTION.
           PERFORM READ-BYA1-FILE-NEXT.   *> SSP  BYAFILE
           IF IO-BAD
              MOVE ALL '9'  TO BY1-KEY
              MOVE "Y"      TO BY-EOF-SW
           ELSE
              ADD 1 TO SQL-RECORDS-PROCESSED
              PERFORM READ-BYB1-FILE-NEXT    *> SSP  BYBFILE
              MOVE BYA1-KEY   TO BY1-KEY
              MOVE BYA-AREA-A TO BY-AREA-A
              MOVE BYB-AREA-B TO BY-AREA-B.

       EXIT-M-READ-BY.
           EXIT.

       M-READ-OBY SECTION.
      * ONLY USE R2 IF NOT IN R1
           IF OBYR11-KEY = OBYR21-KEY
              PERFORM M-READ-OBY-R1
              PERFORM M-READ-OBY-R2
           ELSE
              IF OBYR11-KEY > OBYR21-KEY
                 PERFORM M-READ-OBY-R2
              ELSE
                 PERFORM M-READ-OBY-R1.

           PERFORM DETERMINE-OBY-REC-TO-USE.

       EXIT-M-READ-OBY.
           EXIT.

       DETERMINE-OBY-REC-TO-USE SECTION.
      * ONLY USE R2 IF NOT IN R1
           IF BYR1-EOF-SW = "Y" AND BYR2-EOF-SW = "Y"
              MOVE ALL '9' TO USE-OBY-KEY
           ELSE
              ADD 1 TO A15-RECORDS-PROCESSED
              IF OBYR11-KEY > OBYR21-KEY
                 MOVE OBYR2-REC TO OBY-REC
              ELSE
                 MOVE OBYR1-REC TO OBY-REC
              END-IF
              MOVE OBY-BRNO       TO USE-OBY-KEY-BRNO
              MOVE OBY-CLASS      TO USE-OBY-KEY-CLASS
              MOVE OBY-DATE       TO DATE-YYYMM
              MOVE DATE-YYYMM-MM  TO DATE-MMDDYY-MM
              MOVE 01             TO DATE-MMDDYY-DD
              MOVE DATE-YYYMM-YYY TO DATE-MMDDYY-YY
              PERFORM CONVERT-MMDDYY-TO-YYYYMM
              MOVE DATE-YYYYMM    TO USE-OBY-KEY-DATE.

       DETERMINE-OBY-REC-TO-USE-EXIT.
           EXIT.

       M-READ-OBY-R1 SECTION.
           PERFORM READ-OBY1R1-FILE-NEXT.
           IF IO-BAD
              MOVE ALL '9' TO OBYR11-KEY
              MOVE "Y"     TO BYR1-EOF-SW
           ELSE
              IF PROC-BRANCH-ARRAY(OBYR1-BRNO) NOT = "Y"
                 GO TO M-READ-OBY-R1.

       EXIT-M-READ-OBY-R1.
           EXIT.

       M-READ-OBY-R2 SECTION.
           PERFORM READ-OBY1R2-FILE-NEXT.
           IF IO-BAD
              MOVE ALL '9' TO OBYR21-KEY
              MOVE "Y"     TO BYR2-EOF-SW
           ELSE
              IF PROC-BRANCH-ARRAY(OBYR2-BRNO) NOT = "Y"
                 GO TO M-READ-OBY-R2.

       EXIT-M-READ-OBY-R2.
           EXIT.

      ******************************************************************
       COMPARE-FIELDS SECTION.
      ******************************************************************

            IF USE-OBY-KEY < BY1-KEY
              MOVE "MISSING S35 RECORD" TO D01-TEXT
              MOVE USE-OBY-KEY-BRNO     TO D01-BRNO
              MOVE USE-OBY-KEY-DATE     TO D01-DATE
              MOVE USE-OBY-KEY-CLASS    TO D01-CLASS
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.

            IF USE-OBY-KEY > BY1-KEY
              MOVE "MISSING A15 RECORD" TO D01-TEXT
              PERFORM MOVE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.

           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 3
                   AFTER   S2 FROM 1 BY 1 UNTIL S2 > 2
                   AFTER   S3 FROM 1 BY 1 UNTIL S3 > 11
                IF OBY-NO (S1 S2 S3) NOT = BY-NO (S1 S2 S3)
                   MOVE SPACES TO D01-TEXT
                   STRING "BY-NO",S1,S2,S3 INTO D01-TEXT
                   MOVE OBY-NO(S1 S2 S3)  TO D01-A15-NUM
                   MOVE BY-NO(S1 S2 S3)   TO D01-S35-NUM
                   PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
                IF OBY-AMT (S1 S2 S3) NOT = BY-AMT (S1 S2 S3)
                   MOVE SPACES TO D01-TEXT
                   STRING "BY-AMT",S1,S2,S3 INTO D01-TEXT
                   MOVE OBY-AMT(S1 S2 S3) TO D01-A15-NUM
                   MOVE BY-AMT(S1 S2 S3)  TO D01-S35-NUM
                   PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
                IF OBY-O2 (S1 S2 S3) NOT = BY-O2 (S1 S2 S3)
                   MOVE SPACES TO D01-TEXT
                   STRING "BY-O2",S1,S2,S3 INTO D01-TEXT
                   MOVE OBY-O2(S1 S2 S3) TO D01-A15-NUM
                   MOVE BY-O2(S1 S2 S3)  TO D01-S35-NUM
                   PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
                IF OBY-INT (S1 S2 S3) NOT = BY-INT (S1 S2 S3)
                   MOVE SPACES TO D01-TEXT
                   STRING "BY-INT",S1,S2,S3 INTO D01-TEXT
                   MOVE OBY-INT(S1 S2 S3) TO D01-A15-NUM
                   MOVE BY-INT(S1 S2 S3)  TO D01-S35-NUM
                   PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
           END-PERFORM.

           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 3
                   AFTER   S2 FROM 1 BY 1 UNTIL S2 > 2
                   AFTER   S3 FROM 1 BY 1 UNTIL S3 > 5
                IF OBY-NO-REPO (S1 S2 S3) NOT = BY-NO-REPO (S1 S2 S3)
                   MOVE SPACES TO D01-TEXT
                   STRING "BY-NO-REPO",S1,S2,S3 INTO D01-TEXT
                    MOVE OBY-NO-REPO(S1 S2 S3) TO D01-A15-NUM
                   MOVE BY-NO-REPO(S1 S2 S3)  TO D01-S35-NUM
                   PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
                IF OBY-AMT-REPO (S1 S2 S3) NOT = BY-AMT-REPO (S1 S2 S3)
                   MOVE SPACES TO D01-TEXT
                   STRING "BY-AMT-REPO",S1,S2,S3 INTO D01-TEXT
                   MOVE OBY-AMT-REPO(S1 S2 S3) TO D01-A15-NUM
                   MOVE BY-AMT-REPO(S1 S2 S3)  TO D01-S35-NUM
                   PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
                IF OBY-O2-REPO (S1 S2 S3) NOT = BY-O2-REPO (S1 S2 S3)
                   MOVE SPACES TO D01-TEXT
                   STRING "BY-O2-REPO",S1,S2,S3 INTO D01-TEXT
                   MOVE OBY-O2-REPO(S1 S2 S3) TO D01-A15-NUM
                   MOVE BY-O2-REPO(S1 S2 S3)  TO D01-S35-NUM
                   PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
                IF OBY-INT-REPO (S1 S2 S3) NOT = BY-INT-REPO (S1 S2 S3)
                   MOVE SPACES TO D01-TEXT
                   STRING "BY-INT-REPO",S1,S2,S3 INTO D01-TEXT
                   MOVE OBY-INT-REPO(S1 S2 S3) TO D01-A15-NUM
                   MOVE BY-INT-REPO(S1 S2 S3)  TO D01-S35-NUM
                   PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
           END-PERFORM.

           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 3
                   AFTER   S2 FROM 1 BY 1 UNTIL S2 > 2
                   AFTER   S3 FROM 1 BY 1 UNTIL S3 > 10
                IF OBY-NO-PL (S1 S2 S3) NOT = BY-NO-PL (S1 S2 S3)
                   MOVE SPACES TO D01-TEXT
                   STRING "BY-NO-PL",S1,S2,S3 INTO D01-TEXT
                   MOVE OBY-NO-PL(S1 S2 S3) TO D01-A15-NUM
                   MOVE BY-NO-PL(S1 S2 S3)  TO D01-S35-NUM
                   PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
                IF OBY-AMT-PL (S1 S2 S3) NOT = BY-AMT-PL (S1 S2 S3)
                   MOVE SPACES TO D01-TEXT
                   STRING "BY-AMT-PL",S1,S2,S3 INTO D01-TEXT
                   MOVE OBY-AMT-PL(S1 S2 S3) TO D01-A15-NUM
                   MOVE BY-AMT-PL(S1 S2 S3)  TO D01-S35-NUM
                   PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
                IF OBY-O2-PL (S1 S2 S3) NOT = BY-O2-PL (S1 S2 S3)
                   MOVE SPACES TO D01-TEXT
                   STRING "BY-O2-PL",S1,S2,S3 INTO D01-TEXT
                   MOVE OBY-O2-PL(S1 S2 S3) TO D01-A15-NUM
                   MOVE BY-O2-PL(S1 S2 S3)  TO D01-S35-NUM
                   PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
                IF OBY-INT-PL (S1 S2 S3) NOT = BY-INT-PL (S1 S2 S3)
                   MOVE SPACES TO D01-TEXT
                   STRING "BY-INT-PL",S1,S2,S3 INTO D01-TEXT
                   MOVE OBY-INT-PL(S1 S2 S3) TO D01-A15-NUM
                   MOVE BY-INT-PL(S1 S2 S3)  TO D01-S35-NUM
                   PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
           END-PERFORM.

           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 3
                   AFTER   S2 FROM 1 BY 1 UNTIL S2 > 2
                   AFTER   S3 FROM 1 BY 1 UNTIL S3 > 3
                IF OBY-DLRVPRIN (S1 S2 S3) NOT = BY-DLRVPRIN (S1 S2 S3)
                   MOVE SPACES TO D01-TEXT
                   STRING "BY-DLRVPRIN",S1,S2,S3 INTO D01-TEXT
                   MOVE OBY-DLRVPRIN(S1 S2 S3) TO D01-A15-NUM
                   MOVE BY-DLRVPRIN(S1 S2 S3)  TO D01-S35-NUM
                   PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                  PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
                IF OBY-DLRVCHGS (S1 S2 S3) NOT = BY-DLRVCHGS (S1 S2 S3)
                   MOVE SPACES TO D01-TEXT
                   STRING "BY-DLRVCHGS",S1,S2,S3 INTO D01-TEXT
                   MOVE OBY-DLRVCHGS(S1 S2 S3) TO D01-A15-NUM
                   MOVE BY-DLRVCHGS(S1 S2 S3)  TO D01-S35-NUM
                   PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
           END-PERFORM.

           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 3
                   AFTER   S2 FROM 1 BY 1 UNTIL S2 > 2
             IF OBY-DLPARVHELD (S1 S2) NOT = BY-DLPARVHELD (S1 S2)
                MOVE SPACES TO D01-TEXT
                STRING "BY-DLPARVHELD",S1,S2 INTO D01-TEXT
                MOVE OBY-DLPARVHELD(S1 S2) TO D01-A15-NUM
                MOVE BY-DLPARVHELD(S1 S2)  TO D01-S35-NUM
               PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                PERFORM WRITE-DETAIL-LINE-ERROR
             END-IF
           END-PERFORM.

           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 02
              PERFORM VARYING S2 FROM 1 BY 1 UNTIL S2 > 05
                IF OBY-PL-PRINCOL (S1 S2) NOT =
                     BY-PL-PRINCOL (S1 S2)
                   MOVE SPACES TO D01-TEXT
                   STRING "BY-PL-PRINCOL",S1,S2 INTO D01-TEXT
                   MOVE OBY-PL-PRINCOL(S1 S2) TO D01-A15-NUM
                   MOVE BY-PL-PRINCOL(S1 S2)  TO D01-S35-NUM
                   PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
                IF OBY-PL-INTCOL (S1 S2) NOT =
                     BY-PL-INTCOL (S1 S2)
                   MOVE SPACES TO D01-TEXT
                   STRING "BY-PL-INTCOL",S1,S2 INTO D01-TEXT
                   MOVE OBY-PL-INTCOL(S1 S2) TO D01-A15-NUM
                   MOVE BY-PL-INTCOL(S1 S2)  TO D01-S35-NUM
                   PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
                IF OBY-PL-LATECOL (S1 S2) NOT =
                     BY-PL-LATECOL (S1 S2)
                   MOVE SPACES TO D01-TEXT
                   STRING "BY-PL-LATECOL",S1,S2 INTO D01-TEXT
                   MOVE OBY-PL-LATECOL(S1 S2) TO D01-A15-NUM
                   MOVE BY-PL-LATECOL(S1 S2)  TO D01-S35-NUM
                   PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
                IF OBY-PL-O2COL(S1 S2) NOT = BY-PL-O2COL(S1 S2)
                   MOVE SPACES TO D01-TEXT
                   STRING "BY-PL-O2COL",S1,S2 INTO D01-TEXT
                   MOVE OBY-PL-O2COL(S1 S2) TO D01-A15-NUM
                   MOVE BY-PL-O2COL(S1 S2)  TO D01-S35-NUM
                   PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
              END-PERFORM
              IF OBY-AMT-SOLD(S1) NOT = BY-AMT-SOLD(S1)
                 MOVE SPACES TO D01-TEXT
                 STRING "BY-AMT-SOLD",S1 INTO D01-TEXT
                 MOVE OBY-AMT-SOLD(S1) TO D01-A15-NUM
                 MOVE BY-AMT-SOLD(S1)  TO D01-S35-NUM
                 PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OBY-AMT-TRANS(S1) NOT = BY-AMT-TRANS(S1)
                 MOVE SPACES TO D01-TEXT
                 STRING "BY-AMT-TRANS",S1 INTO D01-TEXT
                 MOVE OBY-AMT-TRANS(S1) TO D01-A15-NUM
                 MOVE BY-AMT-TRANS(S1)  TO D01-S35-NUM
                 PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OBY-AMT-PURCH(S1) NOT = BY-AMT-PURCH(S1)
                 MOVE SPACES TO D01-TEXT
                 STRING "BY-AMT-PURCH",S1 INTO D01-TEXT
                 MOVE OBY-AMT-PURCH(S1) TO D01-A15-NUM
                 MOVE BY-AMT-PURCH(S1)  TO D01-S35-NUM
                 PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OBY-NO-SOLD(S1) NOT = BY-NO-SOLD(S1)
                 MOVE SPACES TO D01-TEXT
                 STRING "BY-NO-SOLD",S1 INTO D01-TEXT
                 MOVE OBY-NO-SOLD(S1) TO D01-A15-NUM
                 MOVE BY-NO-SOLD(S1)  TO D01-S35-NUM
                 PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OBY-NO-TRANS(S1) NOT = BY-NO-TRANS(S1)
                 MOVE SPACES TO D01-TEXT
                 STRING "BY-NO-TRANS",S1 INTO D01-TEXT
                 MOVE OBY-NO-TRANS(S1) TO D01-A15-NUM
                 MOVE BY-NO-TRANS(S1)  TO D01-S35-NUM
                 PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OBY-NO-PURCH(S1) NOT = BY-NO-PURCH(S1)
                 MOVE SPACES TO D01-TEXT
                 STRING "BY-NO-PURCH",S1 INTO D01-TEXT
                 MOVE OBY-NO-PURCH(S1) TO D01-A15-NUM
                 MOVE BY-NO-PURCH(S1)  TO D01-S35-NUM
                 PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OBY-OTHCOL(S1) NOT = BY-OTHCOL(S1)
                 MOVE SPACES TO D01-TEXT
                 STRING "BY-OTHCOL",S1 INTO D01-TEXT
                 MOVE OBY-OTHCOL(S1)  TO D01-A15-NUM
                 MOVE BY-OTHCOL(S1)   TO D01-S35-NUM
                 PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OBY-OT2COL(S1) NOT = BY-OT2COL(S1)
                 MOVE SPACES TO D01-TEXT
                 STRING "BY-OT2COL",S1 INTO D01-TEXT
                 MOVE OBY-OT2COL(S1)  TO D01-A15-NUM
                 MOVE BY-OT2COL(S1)   TO D01-S35-NUM
                 PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OBY-INTCOL(S1) NOT = BY-INTCOL(S1)
                 MOVE SPACES TO D01-TEXT
                 STRING "BY-INTCOL",S1 INTO D01-TEXT
                 MOVE OBY-INTCOL(S1) TO D01-A15-NUM
                 MOVE BY-INTCOL(S1)  TO D01-S35-NUM
                 PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OBY-LATECOL(S1) NOT = BY-LATECOL(S1)
                 MOVE SPACES TO D01-TEXT
                 STRING "BY-LATECOL",S1 INTO D01-TEXT
                 MOVE OBY-LATECOL(S1) TO D01-A15-NUM
                 MOVE BY-LATECOL(S1)  TO D01-S35-NUM
                 PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OBY-PRINCOL(S1) NOT = BY-PRINCOL(S1)
                 MOVE SPACES TO D01-TEXT
                 STRING "BY-PRINCOL",S1 INTO D01-TEXT
                 MOVE OBY-PRINCOL(S1) TO D01-A15-NUM
                 MOVE BY-PRINCOL(S1)  TO D01-S35-NUM
                 PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OBY-DEFCOL(S1) NOT = BY-DEFCOL(S1)
                 MOVE SPACES TO D01-TEXT
                 STRING "BY-DEFCOL",S1 INTO D01-TEXT
                 MOVE OBY-DEFCOL(S1) TO D01-A15-NUM
                 MOVE BY-DEFCOL(S1)  TO D01-S35-NUM
                 PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OBY-RNPRIN(S1) NOT = BY-RNPRIN(S1)
                 MOVE SPACES TO D01-TEXT
                 STRING "BY-RNPRIN",S1 INTO D01-TEXT
                 MOVE OBY-RNPRIN(S1) TO D01-A15-NUM
                 MOVE BY-RNPRIN(S1)  TO D01-S35-NUM
                 PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OBY-INTREB(S1) NOT = BY-INTREB(S1)
                 MOVE SPACES TO D01-TEXT
                 STRING "BY-INTREB",S1 INTO D01-TEXT
                 MOVE OBY-INTREB(S1)  TO D01-A15-NUM
                 MOVE BY-INTREB(S1)   TO D01-S35-NUM
                 PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OBY-CLREB1(S1) NOT = BY-CLREB1(S1)
                 MOVE SPACES TO D01-TEXT
                 STRING "BY-CLREB1",S1 INTO D01-TEXT
                 MOVE OBY-CLREB1(S1) TO D01-A15-NUM
                 MOVE BY-CLREB1(S1)  TO D01-S35-NUM
                 PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OBY-CLREB2(S1) NOT = BY-CLREB2(S1)
                 MOVE SPACES TO D01-TEXT
                 STRING "BY-CLREB2",S1 INTO D01-TEXT
                 MOVE OBY-CLREB2(S1) TO D01-A15-NUM
                 MOVE BY-CLREB2(S1)  TO D01-S35-NUM
                 PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OBY-CLREB3(S1) NOT = BY-CLREB3(S1)
                 MOVE SPACES TO D01-TEXT
                 STRING "BY-CLREB3",S1 INTO D01-TEXT
                 MOVE OBY-CLREB3(S1) TO D01-A15-NUM
                 MOVE BY-CLREB3(S1)  TO D01-S35-NUM
                 PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OBY-CLREB4(S1) NOT = BY-CLREB4(S1)
                 MOVE SPACES TO D01-TEXT
                 STRING "BY-CLREB4",S1 INTO D01-TEXT
                 MOVE OBY-CLREB4(S1) TO D01-A15-NUM
                 MOVE BY-CLREB4(S1)  TO D01-S35-NUM
                 PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OBY-AHREB(S1) NOT = BY-AHREB(S1)
                 MOVE SPACES TO D01-TEXT
                 STRING "BY-AHREB",S1 INTO D01-TEXT
                 MOVE OBY-AHREB(S1)  TO D01-A15-NUM
                 MOVE BY-AHREB(S1)   TO D01-S35-NUM
                 PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OBY-PPREB(S1) NOT = BY-PPREB(S1)
                 MOVE SPACES TO D01-TEXT
                 STRING "BY-PPREB",S1 INTO D01-TEXT
                 MOVE OBY-PPREB(S1)  TO D01-A15-NUM
                 MOVE BY-PPREB(S1)   TO D01-S35-NUM
                 PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OBY-O1REB(S1) NOT = BY-O1REB(S1)
                 MOVE SPACES TO D01-TEXT
                 STRING "BY-O1REB",S1 INTO D01-TEXT
                 MOVE OBY-O1REB(S1)  TO D01-A15-NUM
                 MOVE BY-O1REB(S1)   TO D01-S35-NUM
                 PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OBY-O2REB(S1) NOT = BY-O2REB(S1)
                 MOVE SPACES TO D01-TEXT
                 STRING "BY-O2REB",S1 INTO D01-TEXT
                 MOVE OBY-O2REB(S1)  TO D01-A15-NUM
                 MOVE BY-O2REB(S1)   TO D01-S35-NUM
                 PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OBY-O3REB(S1) NOT = BY-O3REB(S1)
                 MOVE SPACES TO D01-TEXT
                 STRING "BY-O3REB",S1 INTO D01-TEXT
                 MOVE OBY-O3REB(S1)  TO D01-A15-NUM
                 MOVE BY-O3REB(S1)   TO D01-S35-NUM
                 PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OBY-O4REB(S1) NOT = BY-O4REB(S1)
                 MOVE SPACES TO D01-TEXT
                 STRING "BY-O4REB",S1 INTO D01-TEXT
                 MOVE OBY-O4REB(S1)  TO D01-A15-NUM
                 MOVE BY-O4REB(S1)   TO D01-S35-NUM
                 PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OBY-O5REB(S1) NOT = BY-O5REB(S1)
                 MOVE SPACES TO D01-TEXT
                 STRING "BY-O5REB",S1 INTO D01-TEXT
                 MOVE OBY-O5REB(S1)  TO D01-A15-NUM
                 MOVE BY-O5REB(S1)   TO D01-S35-NUM
                 PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OBY-O6REB(S1) NOT = BY-O6REB(S1)
                 MOVE SPACES TO D01-TEXT
                 STRING "BY-O6REB",S1 INTO D01-TEXT
                 MOVE OBY-O6REB(S1)  TO D01-A15-NUM
                 MOVE BY-O6REB(S1)   TO D01-S35-NUM
                 PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OBY-O7REB(S1) NOT = BY-O7REB(S1)
                 MOVE SPACES TO D01-TEXT
                 STRING "BY-O7REB",S1 INTO D01-TEXT
                 MOVE OBY-O7REB(S1)  TO D01-A15-NUM
                 MOVE BY-O7REB(S1)   TO D01-S35-NUM
                 PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OBY-SCREB(S1) NOT = BY-SCREB(S1)
                 MOVE SPACES TO D01-TEXT
                 STRING "BY-SCREB",S1 INTO D01-TEXT
                 MOVE OBY-SCREB(S1)  TO D01-A15-NUM
                 MOVE BY-SCREB(S1)   TO D01-S35-NUM
                 PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OBY-MFREB(S1) NOT = BY-MFREB(S1)
                 MOVE SPACES TO D01-TEXT
                 STRING "BY-MFREB",S1 INTO D01-TEXT
                 MOVE OBY-MFREB(S1)  TO D01-A15-NUM
                 MOVE BY-MFREB(S1)   TO D01-S35-NUM
                 PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OBY-DFREB(S1) NOT = BY-DFREB(S1)
                 MOVE SPACES TO D01-TEXT
                 STRING "BY-DFREB",S1 INTO D01-TEXT
                 MOVE OBY-DFREB(S1)  TO D01-A15-NUM
                 MOVE BY-DFREB(S1)   TO D01-S35-NUM
                 PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OBY-PART-CHGOFF(S1) NOT = BY-PART-CHGOFF(S1)
                 MOVE SPACES TO D01-TEXT
                 STRING "BY-PART-CHGOFF",S1 INTO D01-TEXT
                 MOVE OBY-PART-CHGOFF(S1) TO D01-A15-NUM
                 MOVE BY-PART-CHGOFF(S1)  TO D01-S35-NUM
                 PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OBY-ALLOTMENTS(S1) NOT = BY-ALLOTMENTS(S1)
                 MOVE SPACES TO D01-TEXT
                 STRING "BY-ALLOTMENTS",S1 INTO D01-TEXT
                 MOVE OBY-ALLOTMENTS(S1) TO D01-A15-NUM
                 MOVE BY-ALLOTMENTS(S1)  TO D01-S35-NUM
                 PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              PERFORM VARYING S2 FROM 1 BY 1 UNTIL S2 > 7
                IF OBY-INS-NO-ELIG(S1 S2) NOT = BY-INS-NO-ELIG(S1 S2)
                   MOVE SPACES TO D01-TEXT
                   STRING "BY-INS-NO-ELIG",S1,S2 INTO D01-TEXT
                   MOVE OBY-INS-NO-ELIG(S1 S2) TO D01-A15-NUM
                   MOVE BY-INS-NO-ELIG(S1 S2)  TO D01-S35-NUM
                   PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
                IF OBY-INS-NO-WROTE(S1 S2) NOT = BY-INS-NO-WROTE(S1 S2)
                   MOVE SPACES TO D01-TEXT
                   STRING "BY-INS-NO-WROTE",S1,S2 INTO D01-TEXT
                   MOVE OBY-INS-NO-WROTE(S1 S2) TO D01-A15-NUM
                   MOVE BY-INS-NO-WROTE(S1 S2)  TO D01-S35-NUM
                   PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
                IF OBY-INS-NO-NOTWROTE(S1 S2) NOT =
                     BY-INS-NO-NOTWROTE(S1 S2)
                   MOVE SPACES TO D01-TEXT
                   STRING "BY-INS-NO-NOTWROTE",S1,S2 INTO D01-TEXT
                   MOVE OBY-INS-NO-NOTWROTE(S1 S2) TO D01-A15-NUM
                   MOVE BY-INS-NO-NOTWROTE(S1 S2)  TO D01-S35-NUM
                   PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
                IF OBY-INS-AMT-WROTE(S1 S2) NOT =
                     BY-INS-AMT-WROTE(S1 S2)
                   MOVE SPACES TO D01-TEXT
                   STRING "BY-INS-AMT-WROTE",S1,S2 INTO D01-TEXT
                   MOVE OBY-INS-AMT-WROTE(S1 S2) TO D01-A15-NUM
                   MOVE BY-INS-AMT-WROTE(S1 S2)  TO D01-S35-NUM
                   PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
               END-PERFORM
      * NONFILE, AD&D, ROADGARD WERE 8,9,10 IN INS TABLE, NOW THEY
      * ARE 1, 2, 3 IN FEE INS TABLE:
               PERFORM VARYING S2 FROM 1 BY 1 UNTIL S2 > 3
                IF OBY-INS-NO-ELIG(S1, S2 + 7) NOT =
                         BY-FEE-NO-ELIG(S1 S2)
                   MOVE SPACES TO D01-TEXT
                   STRING "BY-FEE-NO-ELIG",S1,S2 INTO D01-TEXT
                   MOVE OBY-INS-NO-ELIG(S1, S2 + 7) TO D01-A15-NUM
                   MOVE BY-FEE-NO-ELIG(S1 S2)       TO D01-S35-NUM
                   PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
                IF OBY-INS-NO-WROTE(S1, S2 + 7) NOT =
                         BY-FEE-NO-WROTE(S1 S2)
                   MOVE SPACES TO D01-TEXT
                   STRING "BY-FEE-NO-WROTE",S1,S2 INTO D01-TEXT
                   MOVE OBY-INS-NO-WROTE(S1, S2 + 7) TO D01-A15-NUM
                   MOVE BY-FEE-NO-WROTE(S1 S2)       TO D01-S35-NUM
                   PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
                IF OBY-INS-NO-NOTWROTE(S1, S2 + 7) NOT =
                         BY-FEE-NO-NOTWROTE(S1 S2)
                   MOVE SPACES TO D01-TEXT
                   STRING "BY-FEE-NO-NOTWROTE",S1,S2 INTO D01-TEXT
                   MOVE OBY-INS-NO-NOTWROTE(S1, S2 + 7) TO D01-A15-NUM
                   MOVE BY-FEE-NO-NOTWROTE(S1 S2)       TO D01-S35-NUM
                   PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
                IF OBY-INS-AMT-WROTE(S1, S2 + 7) NOT =
                         BY-FEE-AMT-WROTE(S1 S2)
                   MOVE SPACES TO D01-TEXT
                   STRING "BY-FEE-AMT-WROTE",S1,S2 INTO D01-TEXT
                   MOVE OBY-INS-AMT-WROTE(S1, S2 + 7) TO D01-A15-NUM
                   MOVE BY-FEE-AMT-WROTE(S1 S2)       TO D01-S35-NUM
                   PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
                END-PERFORM
                IF OBY-OTH-NO-COL(S1) NOT = BY-OTH-NO-COL(S1)
                   MOVE SPACES TO D01-TEXT
                   STRING "BY-OTH-NO-COL",S1 INTO D01-TEXT
                   MOVE OBY-OTH-NO-COL(S1)      TO D01-A15-NUM
                   MOVE BY-OTH-NO-COL(S1)       TO D01-S35-NUM
                   PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
                IF OBY-OT2-NO-COL(S1) NOT = BY-OT2-NO-COL(S1)
                   MOVE SPACES TO D01-TEXT
                   STRING "BY-OT2-NO-COL",S1 INTO D01-TEXT
                   MOVE OBY-OT2-NO-COL(S1) TO D01-A15-NUM
                   MOVE BY-OT2-NO-COL(S1)  TO D01-S35-NUM
                   PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
                IF OBY-INT-NO-COL(S1) NOT = BY-INT-NO-COL(S1)
                   MOVE SPACES TO D01-TEXT
                   STRING "BY-INT-NO-COL",S1 INTO D01-TEXT
                   MOVE OBY-INT-NO-COL(S1) TO D01-A15-NUM
                   MOVE BY-INT-NO-COL(S1)  TO D01-S35-NUM
                   PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
                IF OBY-LATE-NO-COL(S1) NOT = BY-LATE-NO-COL(S1)
                   MOVE SPACES TO D01-TEXT
                   STRING "BY-LATE-NO-COL",S1 INTO D01-TEXT
                   MOVE OBY-LATE-NO-COL(S1) TO D01-A15-NUM
                   MOVE BY-LATE-NO-COL(S1)  TO D01-S35-NUM
                   PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
                IF OBY-OTH-NO-WAIVED(S1) NOT = BY-OTH-NO-WAIVED(S1)
                   MOVE SPACES TO D01-TEXT
                   STRING "BY-OTH-NO-WAIVED",S1 INTO D01-TEXT
                   MOVE OBY-OTH-NO-WAIVED(S1) TO D01-A15-NUM
                   MOVE BY-OTH-NO-WAIVED(S1)  TO D01-S35-NUM
                   PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
                IF OBY-OT2-NO-WAIVED(S1) NOT = BY-OT2-NO-WAIVED(S1)
                   MOVE SPACES TO D01-TEXT
                   STRING "BY-OT2-NO-WAIVED",S1 INTO D01-TEXT
                   MOVE OBY-OT2-NO-WAIVED(S1) TO D01-A15-NUM
                   MOVE BY-OT2-NO-WAIVED(S1)  TO D01-S35-NUM
                   PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
                IF OBY-INT-NO-WAIVED(S1) NOT = BY-INT-NO-WAIVED(S1)
                   MOVE SPACES TO D01-TEXT
                   STRING "BY-INT-NO-WAIVED",S1 INTO D01-TEXT
                   MOVE OBY-INT-NO-WAIVED(S1) TO D01-A15-NUM
                   MOVE BY-INT-NO-WAIVED(S1)  TO D01-S35-NUM
                   PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
                IF OBY-LATE-NO-WAIVED(S1) NOT = BY-LATE-NO-WAIVED(S1)
                   MOVE SPACES TO D01-TEXT
                   STRING "BY-LATE-NO-WAIVED",S1 INTO D01-TEXT
                   MOVE OBY-LATE-NO-WAIVED(S1) TO D01-A15-NUM
                   MOVE BY-LATE-NO-WAIVED(S1)  TO D01-S35-NUM
                   PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
                IF OBY-OTH-AMT-WAIVED(S1) NOT = BY-OTH-AMT-WAIVED(S1)
                   MOVE SPACES TO D01-TEXT
                   STRING "BY-OTH-AMT-WAIVED",S1 INTO D01-TEXT
                   MOVE OBY-OTH-AMT-WAIVED(S1) TO D01-A15-NUM
                   MOVE BY-OTH-AMT-WAIVED(S1)  TO D01-S35-NUM
                   PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
                IF OBY-OT2-AMT-WAIVED(S1) NOT = BY-OT2-AMT-WAIVED(S1)
                   MOVE SPACES TO D01-TEXT
                   STRING "BY-OT2-AMT-WAIVED",S1 INTO D01-TEXT
                   MOVE OBY-OT2-AMT-WAIVED(S1) TO D01-A15-NUM
                   MOVE BY-OT2-AMT-WAIVED(S1)  TO D01-S35-NUM
                   PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
                IF OBY-INT-AMT-WAIVED(S1) NOT = BY-INT-AMT-WAIVED(S1)
                   MOVE SPACES TO D01-TEXT
                   STRING "BY-INT-AMT-WAIVED",S1 INTO D01-TEXT
                   MOVE OBY-INT-AMT-WAIVED(S1) TO D01-A15-NUM
                   MOVE BY-INT-AMT-WAIVED(S1)  TO D01-S35-NUM
                   PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
                IF OBY-LATE-AMT-WAIVED(S1) NOT = BY-LATE-AMT-WAIVED(S1)
                   MOVE SPACES TO D01-TEXT
                   STRING "BY-LATE-AMT-WAIVED",S1 INTO D01-TEXT
                   MOVE OBY-LATE-AMT-WAIVED(S1) TO D01-A15-NUM
                   MOVE BY-LATE-AMT-WAIVED(S1)  TO D01-S35-NUM
                   PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
           END-PERFORM.

           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 3
                     AFTER S2 FROM 1 BY 1 UNTIL S2 > 2
                     AFTER S3 FROM 1 BY 1 UNTIL S3 > 10
                IF OBY-UNITS(S1 S2 S3) NOT = BY-UNITS(S1 S2 S3)
                   MOVE SPACES TO D01-TEXT
                   STRING "BY-UNITS",S1,S2,S3 INTO D01-TEXT
                   MOVE OBY-UNITS(S1 S2 S3) TO D01-A15-NUM
                   MOVE BY-UNITS(S1 S2 S3)  TO D01-S35-NUM
                   PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
                IF OBY-OUTBAL(S1 S2 S3) NOT = BY-OUTBAL(S1 S2 S3)
                   MOVE SPACES TO D01-TEXT
                   STRING "BY-OUTBAL",S1,S2,S3 INTO D01-TEXT
                   MOVE OBY-OUTBAL(S1 S2 S3) TO D01-A15-NUM
                   MOVE BY-OUTBAL(S1 S2 S3)  TO D01-S35-NUM
                   PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
           END-PERFORM.

           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 3
                     AFTER S2 FROM 1 BY 1 UNTIL S2 > 2
                     AFTER S3 FROM 1 BY 1 UNTIL S3 > 10
                IF OBY-UNITS-BNKRPT(S1 S2 S3) NOT =
                       BY-UNITS-BNKRPT(S1 S2 S3)
                   MOVE SPACES TO D01-TEXT
                   STRING "BY-UNITS-BNKRPT",S1,S2,S3 INTO D01-TEXT
                   MOVE OBY-UNITS-BNKRPT(S1 S2 S3) TO D01-A15-NUM
                   MOVE BY-UNITS-BNKRPT(S1 S2 S3)  TO D01-S35-NUM
                   PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
                IF OBY-OUTBAL-BNKRPT(S1 S2 S3) NOT =
                       BY-OUTBAL-BNKRPT(S1 S2 S3)
                   MOVE SPACES TO D01-TEXT
                   STRING "BY-OUTBAL-BNKRPT",S1,S2,S3 INTO D01-TEXT
                   MOVE OBY-OUTBAL-BNKRPT(S1 S2 S3) TO D01-A15-NUM
                   MOVE BY-OUTBAL-BNKRPT(S1 S2 S3)  TO D01-S35-NUM
                   PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
           END-PERFORM.

           IF BY-SUMWGTTM NOT = OBY-SUMWGTTM
              MOVE "BY-SUMWGTTM" TO D01-TEXT
              MOVE OBY-SUMWGTTM     TO D01-A15-NUM
              MOVE BY-SUMWGTTM      TO D01-S35-NUM
              PERFORM MOVE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BY-SUMWGTAV NOT = OBY-SUMWGTAV
              MOVE "BY-SUMWGTAV" TO D01-TEXT
              MOVE OBY-SUMWGTAV     TO D01-A15-NUM
              MOVE BY-SUMWGTAV      TO D01-S35-NUM
              PERFORM MOVE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BY-SUMWGTTA NOT = OBY-SUMWGTTA
              MOVE "BY-SUMWGTTA" TO D01-TEXT
              MOVE OBY-SUMWGTTA     TO D01-A15-NUM
              MOVE BY-SUMWGTTA      TO D01-S35-NUM
              PERFORM MOVE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BY-IB-ACCRUED NOT = OBY-IB-ACCRUED
              MOVE "BY-IB-ACCRUED" TO D01-TEXT
              MOVE OBY-IB-ACCRUED  TO D01-A15-NUM
              MOVE BY-IB-ACCRUED   TO D01-S35-NUM
              PERFORM MOVE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 03
                IF OBY-DLDISC(S1) NOT = BY-DLDISC(S1)
                   MOVE SPACES TO D01-TEXT
                   STRING "BY-DLDISC",S1 INTO D01-TEXT
                   MOVE OBY-DLDISC(S1) TO D01-A15-NUM
                   MOVE BY-DLDISC(S1)  TO D01-S35-NUM
                   PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
           END-PERFORM.

           IF BY-STD-POS NOT = OBY-STD-POS
              MOVE "BY-STD-POS" TO D01-TEXT
              MOVE OBY-STD-POS     TO D01-A15-NUM
              MOVE BY-STD-POS      TO D01-S35-NUM
              PERFORM MOVE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BY-30-DAY-MOVED NOT = OBY-30-DAY-MOVED
              MOVE "BY-30-DAY-MOVED" TO D01-TEXT
              MOVE OBY-30-DAY-MOVED  TO D01-A15-NUM
              MOVE BY-30-DAY-MOVED   TO D01-S35-NUM
              PERFORM MOVE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BY-PROJ-INCREASE NOT = OBY-PROJ-INCREASE
              MOVE "BY-PROJ-INCREASE" TO D01-TEXT
              MOVE OBY-PROJ-INCREASE   TO D01-A15-NUM
              MOVE BY-PROJ-INCREASE    TO D01-S35-NUM
              PERFORM MOVE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BY-PROJ-PROFIT NOT = OBY-PROJ-PROFIT
              MOVE "BY-PROJ-PROFIT" TO D01-TEXT
              MOVE OBY-PROJ-PROFIT  TO D01-A15-NUM
              MOVE BY-PROJ-PROFIT   TO D01-S35-NUM
              PERFORM MOVE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 3
                     AFTER S2 FROM 1 BY 1 UNTIL S2 > 2
                     AFTER S3 FROM 1 BY 1 UNTIL S3 > 6
                IF OBY-UNEARNED-AMT (S1 S2 S3) NOT = 
                      BY-UNEARNED-AMT (S1 S2 S3)
                   MOVE SPACES TO D01-TEXT
                   STRING "BY-UNEARNED-AMT",S1,S2,S3 INTO D01-TEXT
                   MOVE OBY-UNEARNED-AMT(S1 S2 S3) TO D01-A15-NUM
                   MOVE BY-UNEARNED-AMT(S1 S2 S3)  TO D01-S35-NUM
                   PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
           END-PERFORM.

           PERFORM VARYING S1 FROM 1 BY 1 UNTIL S1 > 2
              IF OBY-PROCEEDS(S1) NOT = BY-PROCEEDS(S1)
                 MOVE SPACES TO D01-TEXT
                 STRING "BY-PROCEEDS",S1 INTO D01-TEXT
                 MOVE OBY-PROCEEDS(S1) TO D01-A15-NUM
                 MOVE BY-PROCEEDS(S1)  TO D01-S35-NUM
                 PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OBY-LOCKBOX(S1) NOT = BY-LOCKBOX(S1)
                 MOVE SPACES TO D01-TEXT
                 STRING "BY-LOCKBOX",S1 INTO D01-TEXT
                 MOVE OBY-LOCKBOX(S1) TO D01-A15-NUM
                 MOVE BY-LOCKBOX(S1)  TO D01-S35-NUM
                 PERFORM MOVE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.

       EXIT-COMPARE-FIELDS.
           EXIT.

      ******************************************************************
       MOVE-DETAIL-LINE-ERROR-KEY SECTION.
      ******************************************************************
           MOVE BY-BRNO         TO D01-BRNO.
           MOVE BY-DATE-CCYYMM  TO D01-DATE.
           MOVE BY-CLASS        TO D01-CLASS.

      ******************************************************************
       WRITE-DETAIL-LINE-ERROR SECTION.
      ******************************************************************
           ADD 1 TO TOTAL-ERRORS-CTR.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************
           ADD  LN-FEED TO LN-COUNT.

           IF ( LN-COUNT > EXT-CONAME-RPT-LINES )
              PERFORM WRITE-HEADER-LINES.

           MOVE DETAIL-LINE TO PR-REC.
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
              MOVE "BEGINNING BRANCH             : "  TO R01-PROMPT
              MOVE ENT-GROUP                          TO R01-TEXT
              MOVE 01                                 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH                : "  TO R01-PROMPT
              MOVE SPACES                             TO R01-TEXT
              MOVE 01                                 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEGINNING BRANCH             : "  TO R01-PROMPT
              MOVE BEG-BRANCH                         TO R01-NUMERIC
              MOVE 01                                 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH                : "  TO R01-PROMPT
              MOVE END-BRANCH                         TO R01-NUMERIC
              MOVE 01                                 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING DATE               : "    TO R01-PROMPT.
           MOVE BEG-DATE                             TO R01-NUMERIC.
           MOVE 02                                   TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    DATE               : "    TO R01-PROMPT.
           MOVE END-DATE                             TO R01-NUMERIC.
           MOVE 01                                   TO LN-FEED.
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
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/PROGID.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/BRSECURE.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO BYFILE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BYFILE-SYSDATE.
           DISPLAY BYFILE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE BYFILE-FORM-FIELDS TO WR-BUF.
           MOVE BYFILE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE BYFILE-HELP--FILE TO HELP-LINK-FILE.
           MOVE BYFILE-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/BYFILE_EVA.CPY".
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

           IF EXT-ROUTE-BUF = "00"
              MOVE USE-OBY-KEY-BRNO TO PROGID-BRANCH
              STRING "PROCESSING DATE: ", USE-OBY-KEY-DATE 
                INTO PROGID-TEXT
              PERFORM GET-TIME
              MOVE TIME-EDIT TO PROGID-TIME
              PERFORM PROGID-DISPLAY.

       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPBYGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

       COPY "LIBGB/GRBRLS_SQL.CPY".

      * COPY "LIBLP/LPBY1RN.CPY".
      * COPYMEMBER: LIBLP/LPBY1RN
      ******************************************************************
      *
      * DESCRIPTION: BRANCH STATISTICS FILE - IO ROUTINES - A THRU B
      *
      * FILE KEY   : #1 = DATE-CCYYMM, BRNO, CLASS
      *
      * FILE PATH  : /USR/DATA/R#/LP/BYAFILE
      *              /USR/DATA/R#/LP/BYBFILE
      *
      * ATTENTION  : BY-PATH-OVERRIDE CONTROLS ALL FILES - A THRU B
      *              ^^^^^^^^^^^^^^^^
      *              THERE IS NO BYA-PATH-OVERRIDE OR BYB-PATH-OVERRIDE
      *
      *=================================================================
      * REV:
      * KEC 2019-0205 ***NEW*** TO A32; MODIFIED FROM A30
      * KEC 2021-0524 ADDED ACUSQL VERSION
      ******************************************************************
      *-----------------------------------------------------------------
             
      *-----------------------------------------------------------------
       LOAD-BYA1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BY-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BY-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BYA-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BYA-ALL-MACHINE
              MOVE BYA-ALL-PATH            TO BYA-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BY-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BYA-ALL-BASE
              MOVE FILPATH-MACHINE         TO BYA-ALL-MACHINE
              MOVE BYA-ALL-PATH            TO BYA-PATH.

      *-----------------------------------------------------------------
       OPEN-BYA1-FILE.
           PERFORM LOAD-BYA1-FILE.
           PERFORM OPEN-IT.
           MOVE BYA-PATH-SQL TO E-FILE.

           IF BY1-CONN-OPEN-SW = "N" 
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS BY1_CONNECT
                        USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC

               EXEC SQL SET CONNECTION C1 END-EXEC
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS BY2_CONNECT
                        USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC

               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE 'Y' TO BY1-CONN-OPEN-SW.

      *-----------------------------------------------------------------
       START-BYA1-FILE.   *> SSP  BYAFILE
           PERFORM START-IT.
           MOVE BYA-PATH-SQL TO E-FILE.
           MOVE BYA1-KEY TO E-KEYX.

           IF ( BYA1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BYA1-FILE.

      *      SELECT BYAFILE.*
      *      FROM DBO.BYAFILE
      *      WHERE BYAFILE.BYA_DATE_CCYYMM BETWEEN @QBY_WBEG_DATE AND @QBY_WEND_DATE
      *      ORDER BY BYAFILE.BYA_DATE_CCYYMM, BYAFILE.BYA_BRNO, BYAFILE.BYA_CLASS
 
           EXEC SQL SET CONNECTION BY1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE BYA_CURSOR CURSOR FOR
                :BY1-RETURN-CODE =  
            EXEC SSP_BYFILE_BYAFILE_SELECT
              ( :PASS-BRANCHLIST, :QBY1-WBEG-DATE-CCYYMM, 
                :QBY1-WEND-DATE-CCYYMM)
           END-EXEC.

           EXEC SQL
               OPEN BYA_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           MOVE STAT---GOOD TO BYA1-CURSOR-STAT.

      *-----------------------------------------------------------------
       START-BYB1-FILE.   *> SSP  BYBFILE
           PERFORM START-IT.
           MOVE BYB-PATH-SQL TO E-FILE.
           MOVE BYB1-KEY TO E-KEYX.

           IF ( BYB1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BYA1-FILE.

      *      SELECT BYBFILE.*
      *      FROM DBO.BYFILE
      *      WHERE BYBFILE.BYB_DATE_CCYYMM BETWEEN @QBY_WBEG_DATE AND @QBY_WEND_DATE
      *      ORDER BY BYBFILE.BYB_DATE_CCYYMM, BYBFILE.BYB_BRNO, BYBFILE.BYB_CLASS
 
           EXEC SQL SET CONNECTION BY2_CONNECT END-EXEC.

           EXEC SQL
            DECLARE BYB_CURSOR CURSOR FOR
                :BY1-RETURN-CODE =  
            EXEC SSP_BYFILE_BYBFILE_SELECT
              ( :PASS-BRANCHLIST, :QBY1-WBEG-DATE-CCYYMM, 
                :QBY1-WEND-DATE-CCYYMM)
           END-EXEC.

           EXEC SQL
               OPEN BYB_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           MOVE STAT---GOOD TO BYB1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-BYA1-FILE-NEXT.   *> SSP  BYAFILE
           PERFORM READ-IT.
           MOVE BYA-PATH-SQL TO E-FILE.
           MOVE BYA1-KEY TO E-KEYX.
           INITIALIZE QBYA-REC.

           EXEC SQL SET CONNECTION BY1_CONNECT END-EXEC.

           IF ( BYA1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BYA_CURSOR INTO :QBYA-REC
               END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BYA1-FILE-NEXT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           IF ( IO-FG = 0 )
              PERFORM GET-BYA-FIELDS.

      *-----------------------------------------------------------------
       READ-BYB1-FILE-NEXT.   *> SSP  BYBFILE
           PERFORM READ-IT.
           MOVE BYB-PATH-SQL TO E-FILE.
           INITIALIZE QBYB-REC.

           EXEC SQL SET CONNECTION BY2_CONNECT END-EXEC.

           IF ( BYB1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BYB_CURSOR INTO :QBYB-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BYB1-FILE-NEXT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           IF ( IO-FG = 0 )
              PERFORM GET-BYB-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-BYA1-FILE.
           PERFORM CLOSE-IT.

           IF ( BYA1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION BY1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE BYA_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO BYA1-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-BYB1-FILE.
           PERFORM CLOSE-IT.

           IF ( BYB1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION BY2_CONNECT END-EXEC
              EXEC SQL
                   CLOSE BYB_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO BYB1-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-BY1-CONNECTION.

           IF BY1-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION BY1_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT BY1_CONNECT END-EXEC

               EXEC SQL SET CONNECTION BY2_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT BY2_CONNECT END-EXEC

               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE "N" TO BY1-CONN-OPEN-SW.

      *-----------------------------------------------------------------
       OPEN-OBY1R1-FILE.
           PERFORM OPEN-IT.
           MOVE OBYR1-PATH TO E-FILE.
           OPEN INPUT OBYR1-FILE.
           IF IO-FG = 8
              GO TO OPEN-OBY1R1-FILE.
           IF IO-FG = 7
              CLOSE OBYR1-FILE
              GO TO OPEN-OBY1R1-FILE.
       START-OBY1R1-FILE.
           PERFORM START-IT.
           MOVE OBYR1-PATH TO E-FILE.
           MOVE OBYR11-KEY TO E-KEYX.
           START OBYR1-FILE KEY NOT < OBYR11-KEY.
       READ-OBY1R1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OBYR1-PATH TO E-FILE.
           MOVE OBYR11-KEY TO E-KEYX.
           READ OBYR1-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OBY1R1-FILE-NEXT.
           IF IO-GOOD 
              MOVE OBYR1-DATE     TO DATE-YYYMM
              MOVE DATE-YYYMM-MM  TO DATE-MMDDYY-MM
              MOVE 01             TO DATE-MMDDYY-DD
              MOVE DATE-YYYMM-YYY TO DATE-MMDDYY-YY
              PERFORM CONVERT-MMDDYY-TO-YYYYMM
              IF DATE-YYYYMM-YYYY < 2012 OR
                 DATE-YYYYMM < BEG-DATE  OR
                 DATE-YYYYMM > END-DATE
                 GO TO READ-OBY1R1-FILE-NEXT.
       CLOSE-OBY1R1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OBYR1-FILE.

      *-----------------------------------------------------------------
       OPEN-OBY1R2-FILE.
           PERFORM OPEN-IT.
           MOVE OBYR2-PATH TO E-FILE.
           OPEN INPUT OBYR2-FILE.
           IF IO-FG = 8
              GO TO OPEN-OBY1R2-FILE.
           IF IO-FG = 7
              CLOSE OBYR2-FILE
              GO TO OPEN-OBY1R2-FILE.
       START-OBY1R2-FILE.
           PERFORM START-IT.
           MOVE OBYR2-PATH TO E-FILE.
           MOVE OBYR21-KEY TO E-KEYX.
           START OBYR2-FILE KEY NOT < OBYR21-KEY.
       READ-OBY1R2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OBYR2-PATH TO E-FILE.
           MOVE OBYR21-KEY TO E-KEYX.
           READ OBYR2-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OBY1R2-FILE-NEXT.
           IF IO-GOOD 
              MOVE OBYR2-DATE     TO DATE-YYYMM
              MOVE DATE-YYYMM-MM  TO DATE-MMDDYY-MM
              MOVE 01             TO DATE-MMDDYY-DD
              MOVE DATE-YYYMM-YYY TO DATE-MMDDYY-YY
              PERFORM CONVERT-MMDDYY-TO-YYYYMM
              IF DATE-YYYYMM-YYYY < 2012 OR
                DATE-YYYYMM < BEG-DATE   OR
                DATE-YYYYMM > END-DATE
                 GO TO READ-OBY1R2-FILE-NEXT.
       CLOSE-OBY1R2-FILE.
           PERFORM CLOSE-IT.
           CLOSE OBYR2-FILE.


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

       EXIT-PROG.

           PERFORM BTRACK-DONE. 

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE EXIT-PATH               TO FORM-NAM
              MOVE FORM-PATH               TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY BYFILE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
