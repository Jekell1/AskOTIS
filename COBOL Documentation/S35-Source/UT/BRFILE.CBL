       IDENTIFICATION DIVISION.
       PROGRAM-ID.    BRFILE.
       DATE-WRITTEN.  11-28-2023.
      ******************************************************************
      *
      * DESCRIPTION   : COMPARE S35 BRFILE WITH A15 BRFILE
      *            
      *                 TO BE USED AFTER AN UPDATE
      *                 WILL COMPARE EACH FIELD AND REPORT ANY 
      *                 DIFFERENCES, AS LONG AS FIELD EXIST IN BOTH FILES
      *
      *  LOOPS THRU THE A15 FILE AND VERIFIES THE S35 FILE EXIST AND MATCHES
      *
      *  01-07-25  RMZ  ADDED 'BTRACK' - PROCESS TRACKING TO PROGRAM.
      *  03-05-25  MB   ADDED INPUT BEG/ENDING BRANCH.
      *  04-16-25  MB   USE 0 FOR OB2-GL-ESCROW-BALANCE IF = 2020202020
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

           SELECT OBR-FILE ASSIGN TO OBR-PATH
                  ORGANIZATION RELATIVE
                  ACCESS MODE DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RELATIVE KEY OBR-KEY
                  FILE STATUS FILE-STAT.

           SELECT OB2-FILE ASSIGN TO OB2-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OB21-KEY
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

       FD  OBR-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A15SRC/GB01BR.CPY" REPLACING LEADING "BR" BY "OBR".

       FD  OB2-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A15SRC/GB01B2.CPY" REPLACING LEADING "B2" BY "OB2".

       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
              05  WK-KEY-BRNO         PIC 9(4).
           03  WK-DATA-BR             PIC X(OBR-SIZE).
           03  WK-DATA-B2             PIC X(OB2-SIZE).

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/BRFILE S35 12/01/23-13:58:36 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".

       COPY "LIBGB/GRSSPW.CPY".
       COPY "LIBGB/GRBRLSW.CPY".

       01  OBR-KEY                     PIC 9(4).
       01  OBR-PATH                    PIC X(35) VALUE SPACES.
       01  OBR-ALL-PATH.
           03  OBR-ALL-BASE            PIC X(9).
           03  FILLER                  PIC X     VALUE "/".
           03  OBR-ALL-MACHINE         PIC XX.
           03  FILLER                  PIC X(10) VALUE
               "/GB/BRFILE".

       01  OB2-PATH                    PIC X(35) VALUE SPACES.
       01  OB2-ALL-PATH.
           03  OB2-ALL-BASE            PIC X(9).
           03  FILLER                  PIC X     VALUE "/".
           03  OB2-ALL-MACHINE         PIC XX.
           03  FILLER                  PIC X(10) VALUE
               "/GB/B2FILE".

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
       01  USE-MACHINE.
           03  FILLER                  PIC X.
           03  FILLER                  PIC 9.

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

       01  PROCESS-STAT                PIC X(08)  VALUE SPACES.
           88  PROCESS-STAT-GOOD                  VALUE "00".
           88  PROCESS-STAT-BAD                   VALUE "  " THRU "//"
                                                      "01" THRU "~~".

       01  ALL-DONE-SW                 PIC X      VALUE "N".
       01  BR-EOF-SW                   PIC X      VALUE "N".
       01  WK-EOF-SW                   PIC X      VALUE "N".
       01  WK-REC-CTR                  PIC 9(9)   VALUE 0.
       01  TOTAL-ERRORS-CTR            PIC 9(9)   VALUE 0.
       01  SQL-RECORDS-PROCESSED       PIC 9(9)   VALUE 0.
       01  A15-RECORDS-PROCESSED       PIC 9(9)   VALUE 0.

       01  STARTING-TIME               PIC X(08)     VALUE SPACES.
       01  SUB                         PIC 9(04)     VALUE ZEROES.
       01  S1                          PIC 9(04)     VALUE ZEROES.
       01  S2                          PIC 9(04)     VALUE ZEROES.
       01  S3                          PIC 9(04)     VALUE ZEROES.
       01  WS-NUM                      PIC 9(13)     VALUE ZEROS.

       01  SYSDATE-YYYYMMDD            PIC 9(8)      VALUE ZEROES.

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
       01  LEGEND-ACCEPT-YN.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT             ".
           03  FILLER PIC X(30) VALUE "                              ".
           03  FILLER PIC X(19) VALUE "     ACCEPT (Y/N): ".

       01  SOURCE-PATH-LIST            PIC X(07)     VALUE "S-PATH.".

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  SOURCE-PATH             PIC X(9).
           03  BEG-BRANCH              PIC 9(04).
           03  END-BRANCH              PIC 9(04).
           03  GROUP-FLAG              PIC X(01).
           03  ENT-GROUP               PIC X(04).

       01  VERSION-CONTROL             PIC X(01)     VALUE "B".

       01  PROC-BRANCH-ARRAY           OCCURS 9999 TIMES
                                       PIC X.

      *-----------------------------------------------------------------
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
           03  FILLER                  PIC X(6) VALUE SPACES.
           03  FILLER                  PIC X(06) VALUE "PAGE: ".
           03  H01-PAGE                PIC Z(04).

      *-----------------------------------------------------------------
       01  HEADER-LINE-02.
           03  FILLER                  PIC X(10) VALUE "USER ID:".
           03  H02-USERID              PIC X(10) VALUE SPACES.
           03  FILLER                  PIC X(10) VALUE SPACES.
           03  H02-TITLE               PIC X(50) VALUE 
           "BRANCH MASTER (BRFILE) COMPARE A15 TO S35".

       01  HEADER-LINE-03.
           03  FILLER                  PIC X(4)  VALUE "BRNO".
           03  FILLER                  PIC X(3)  VALUE SPACES.
           03  FILLER                  PIC X(28) VALUE "DESCRIPTION".
           03  FILLER                  PIC X(22) VALUE "A15".
           03  FILLER                  PIC X(3) VALUE "S35".

       01  DETAIL-LINE                        PIC X(100)   VALUE SPACES.

       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-BRNO                       PIC Z(3)9.
           03  FILLER                         PIC X(3).
           03  D01-TEXT                       PIC X(25).
           03  FILLER                         PIC X(3).
           03  D01-A15                        PIC X(21).
           03  FILLER REDEFINES D01-A15.
               05  D01-A15-NUM                PIC Z(7)9.9(3)-.
               05  FILLER                     PIC X(8).
           03  FILLER                         PIC X(2).
           03  D01-S35                        PIC X(21).
           03  FILLER REDEFINES D01-S35.
               05  D01-S35-NUM                PIC Z(7)9.9(3)-.
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
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBUT/BRFILE_DEF.CPY".
       COPY "LIBUT/BRFILE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/BRFILE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE OBR-FILE OB2-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE OBR-FILE OB2-FILE WK-FILE.
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
           MOVE "BRFILE" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE BRFILE-QUEUE-POS TO Q-POS.
           MOVE BRFILE-COPIES-POS TO C-POS.
           MOVE BRFILE-PRU-POS   TO P-POS.
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
              MOVE EXT-EOBUF-BRNO         TO BEG-BRANCH END-BRANCH
              MOVE "N"                    TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE
           ELSE
              IF ( BEG-BRANCH = ZEROES ) AND ( END-BRANCH = ZEROES )
                 MOVE EXT-CONAME-USER-LOC     TO BEG-BRANCH
                                                 END-BRANCH.

           PERFORM OPEN-BR-FILE.
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

           PERFORM VARYING SUB FROM 1 BY 1 
            UNTIL SUB > BGLS-BRANCHES-RETR-CTR
              MOVE BGLS-BRANCH-BRNO(SUB) TO S1  
              MOVE "Y" TO PROC-BRANCH-ARRAY(S1) 
           END-PERFORM.

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

           MOVE 9999  TO PROGID-SLEEP-COUNTER.
           PERFORM SEND-PROGRESS-INDICATOR.

           MOVE EXT-FILPATH-BASE    TO FILPATH-BASE.
           MOVE BR-MACHINE          TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH        TO FILPATH-DATA.

      D    STOP MESS.

           MOVE SOURCE-PATH  TO OBR-ALL-BASE OB2-ALL-BASE.

           PERFORM LOAD-WORK-FILE.
           IF WK-REC-CTR = 0
              GO TO PROCESS-BRANCH-DRIVER-EXIT.

           MOVE "PROCESSING S35 DATA" TO BTRACK-MESS.
           PERFORM BTRACK-GLOBAL.

           MOVE 0    TO QBR-WBEG-NO.
           MOVE 9999 TO QBR-WEND-NO.
           PERFORM START-BR-FILE.
           PERFORM M-READ-BR.

           PERFORM OPEN-WK1-FILE.
           PERFORM M-READ-WK1.

           PERFORM UNTIL WK-EOF-SW = "Y" AND BR-EOF-SW = "Y"
              PERFORM SEND-PROGRESS-INDICATOR
              PERFORM COMPARE-FIELDS
              IF WK-KEY < BR-KEY
                 PERFORM M-READ-WK1
              ELSE
                 IF WK-KEY > BR-KEY
                    PERFORM M-READ-BR
                 ELSE
                    PERFORM M-READ-WK1
                    PERFORM M-READ-BR
                 END-IF
              END-IF
           END-PERFORM.

           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-WK1-FILE.

       PROCESS-BRANCH-DRIVER-EXIT.
           EXIT.

       M-READ-WK1 SECTION.
           PERFORM READ-WK1-FILE-NEXT.
           IF IO-BAD 
              MOVE 9999 TO WK-KEY
              MOVE "Y"  TO WK-EOF-SW
           ELSE
              MOVE WK-DATA-BR TO OBR-REC
              MOVE WK-DATA-B2 TO OB2-REC.

       EXIT-M-READ-WK1.
           EXIT.

       M-READ-BR SECTION.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-BAD
              MOVE 9999 TO BR-KEY
              MOVE "Y"  TO BR-EOF-SW
           ELSE
              IF PROC-BRANCH-ARRAY(BR-NO) = "Y"
                 ADD 1 TO SQL-RECORDS-PROCESSED
              ELSE
                 GO TO M-READ-BR.


       EXIT-M-READ-BR.
           EXIT.

       LOAD-WORK-FILE SECTION.

      * DELETE/EMPTY TEMP FILE IF EXIST
           PERFORM OPEN-WK1-FILE-OUTPUT.
           PERFORM CLOSE-WK1-FILE.

      * OPENS FOR I/O - REWRITE NEEDS FILE OPENED FOR I/O
           PERFORM OPEN-WK1-FILE.  

      * LOAD R1 BRFILE AND B2FILE TO WK-FILE - REWRITE IF R2 RECORD ALREADY EXIST

           MOVE MACHINE-R1   TO OBR-ALL-MACHINE.
           MOVE OBR-ALL-PATH TO OBR-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE "MISSING R1 BRFILE" TO D01-TEXT
              MOVE SPACES              TO D01-BRNO
              MOVE OBR-ALL-PATH        TO D01-A15
              MOVE SPACES              TO D01-S35
              PERFORM WRITE-DETAIL-LINE
              GO TO LOAD-WORK-FILE-EXIT.

           MOVE MACHINE-R1   TO OB2-ALL-MACHINE.
           MOVE OB2-ALL-PATH TO OB2-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE "MISSING R1 B2FILE" TO D01-TEXT
              MOVE SPACES              TO D01-BRNO
              MOVE OB2-ALL-PATH        TO D01-A15
              MOVE SPACES              TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO LOAD-WORK-FILE-EXIT.

           MOVE "PROCESSING A15 DATA" TO BTRACK-MESS.  
           PERFORM BTRACK-GLOBAL.

           PERFORM OPEN-OBR-FILE.
           PERFORM OPEN-OB2-FILE.
           MOVE "N" TO ALL-DONE-SW
           PERFORM UNTIL ALL-DONE-SW = "Y"
              PERFORM READ-OBR-FILE-NEXT
              IF IO-BAD
                 MOVE "Y" TO ALL-DONE-SW
              ELSE
                 IF PROC-BRANCH-ARRAY(OBR-NO) = "Y"
                    MOVE OBR-NO TO OB2-BRNO
                    PERFORM READ-OB2-FILE
                    IF IO-BAD
                       MOVE 0                          TO WK-REC-CTR
                       MOVE "MISSING R1 B2FILE RECORD" TO D01-TEXT
                       MOVE OBR-NO                     TO D01-BRNO
                       PERFORM WRITE-DETAIL-LINE-ERROR
                       PERFORM CLOSE-OBR-FILE
                       PERFORM CLOSE-OB2-FILE
                       GO TO LOAD-WORK-FILE-EXIT
                    END-IF
                    MOVE OBR-NO   TO WK-KEY-BRNO
                    MOVE OBR-REC  TO WK-DATA-BR
                    MOVE OB2-REC  TO WK-DATA-B2
                    PERFORM WRITE-WK1-FILE
                    ADD 1 TO WK-REC-CTR A15-RECORDS-PROCESSED
                 END-IF
              END-IF
           END-PERFORM.
           PERFORM CLOSE-OBR-FILE.
           PERFORM CLOSE-OB2-FILE.

           MOVE MACHINE-R2   TO OBR-ALL-MACHINE.
           MOVE OBR-ALL-PATH TO OBR-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE "MISSING R2 BRFILE" TO D01-TEXT
              MOVE OBR-ALL-PATH        TO D01-A15
              PERFORM WRITE-DETAIL-LINE
              MOVE 0                   TO WK-REC-CTR
              GO TO LOAD-WORK-FILE-EXIT.

           MOVE MACHINE-R2   TO OB2-ALL-MACHINE.
           MOVE OB2-ALL-PATH TO OB2-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE 0                   TO WK-REC-CTR
              MOVE "MISSING R2 B2FILE" TO D01-TEXT
              MOVE OB2-ALL-PATH        TO D01-A15
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO LOAD-WORK-FILE-EXIT.

           PERFORM OPEN-OB2-FILE.
           PERFORM OPEN-OBR-FILE.
           MOVE "N" TO ALL-DONE-SW
           PERFORM UNTIL ALL-DONE-SW = "Y"
              PERFORM READ-OBR-FILE-NEXT
              IF IO-BAD
                 MOVE "Y" TO ALL-DONE-SW
              ELSE
                 IF PROC-BRANCH-ARRAY(OBR-NO) = "Y"
      * DO NOT OVERWRITE THE BRANCH 9990 FROM RISC1!!!!!
                    IF OBR-MACHINE = MACHINE-R2 AND OBR-NO NOT = 9990
                       MOVE OBR-NO TO OB2-BRNO
                       PERFORM READ-OB2-FILE
                       IF IO-BAD
                           MOVE 0                          TO WK-REC-CTR
                           MOVE "MISSING R2 B2FILE RECORD" TO D01-TEXT   
                           MOVE OBR-NO                     TO D01-BRNO
                           PERFORM WRITE-DETAIL-LINE-ERROR
                           PERFORM CLOSE-OBR-FILE
                           PERFORM CLOSE-OB2-FILE
                           GO TO LOAD-WORK-FILE-EXIT
                       END-IF
                       MOVE OBR-NO   TO WK-KEY-BRNO
                       MOVE OBR-REC  TO WK-DATA-BR
                       MOVE OB2-REC  TO WK-DATA-B2
                       PERFORM WRITE-WK1-FILE
                       IF IO-STATUS = 9 
                          IF OBR-MACHINE = MACHINE-R2   *> RERWITE IF R2 BRANCH
                             PERFORM REWRITE-WK1-FILE
                          END-IF
                       ELSE
                          ADD 1 TO WK-REC-CTR A15-RECORDS-PROCESSED
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-PERFORM.
           PERFORM CLOSE-OBR-FILE.
           PERFORM CLOSE-OB2-FILE.

       LOAD-WORK-FILE-EXIT.
           EXIT. 

      ******************************************************************
       COMPARE-FIELDS SECTION.
      ******************************************************************

           MOVE OBR-NO TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              MOVE "MISSING S35 RECORD" TO D01-TEXT
              MOVE BR-NO          TO D01-BRNO
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.

           IF BR-NAME NOT = OBR-NAME
      *       CALL "C$JUSTIFY" USING OBR-NAME, "L"
              IF BR-NAME NOT = OBR-NAME
                 MOVE "BR-NAME" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE OBR-NAME  TO D01-A15
                 MOVE BR-NAME   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BR-ADR1 NOT = OBR-ADR1
      *       CALL "C$JUSTIFY" USING OBR-ADR1, "L"
              IF BR-ADR1 NOT = OBR-ADR1
                 MOVE "BR-ADR1" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE OBR-ADR1  TO D01-A15
                 MOVE BR-ADR1   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BR-ADR2 NOT = OBR-ADR2
      *       CALL "C$JUSTIFY" USING OBR-ADR2, "L"
              IF BR-ADR2 NOT = OBR-ADR2
                 MOVE "BR-ADR2" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE OBR-ADR2  TO D01-A15
                 MOVE BR-ADR2   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BR-CITY NOT = OBR-CITY
      *       CALL "C$JUSTIFY" USING OBR-CITY, "L"
              IF BR-CITY NOT = OBR-CITY
                 MOVE "BR-CITY" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE OBR-CITY  TO D01-A15
                 MOVE BR-CITY   TO D01-S35

                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BR-STATE NOT = OBR-STATE
                 MOVE "BR-STATE" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE OBR-STATE  TO D01-A15
                 MOVE BR-STATE   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BR-ZIP NOT = OBR-ZIP
      *       CALL "C$JUSTIFY" USING OBR-ZIP, "L"
              IF BR-ZIP NOT = OBR-ZIP
                 MOVE "BR-ZIP" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE OBR-ZIP  TO D01-A15
                 MOVE BR-ZIP   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OBR-PHONE NOT NUMERIC
              MOVE 0 TO OBR-PHONE.
           IF OBR-PHONE NOT = BR-PHONE
              MOVE "BR-PHONE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-PHONE  TO D01-A15
              MOVE BR-PHONE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OBR-RETAIN NOT NUMERIC
              MOVE 0 TO OBR-RETAIN.
           IF OBR-RETAIN NOT = BR-RETAIN
              MOVE "BR-RETAIN" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-RETAIN  TO D01-A15
              MOVE BR-RETAIN   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OBR-NEXT-CHECK NOT NUMERIC
              MOVE 0 TO OBR-NEXT-CHECK.
           IF OBR-NEXT-CHECK NOT = BR-NEXT-CHECK
              MOVE "BR-NEXT-CHECK" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-NEXT-CHECK  TO D01-A15
              MOVE BR-NEXT-CHECK   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
              IF OBR-PRINTER(SUB) NOT NUMERIC
                 MOVE 0 TO OBR-PRINTER(SUB)
              END-IF
              IF OBR-PRINTER(SUB) NOT = BR-PRINTER(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "BR-PRINTER",SUB INTO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE OBR-PRINTER(SUB)  TO D01-A15
                 MOVE BR-PRINTER(SUB)   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF BR-PRINTERFG(SUB) NOT = OBR-PRINTERFG(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "BR-PRINTERFG",SUB INTO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE OBR-PRINTERFG(SUB)  TO D01-A15
                 MOVE BR-PRINTERFG(SUB)   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.

           IF OBR-FEE-GL-BRNO NOT NUMERIC
              MOVE 0 TO OBR-FEE-GL-BRNO.
           IF OBR-FEE-GL-BRNO NOT = BR-FEE-GL-BRNO
              MOVE "BR-FEE-GL-BRNO" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-FEE-GL-BRNO  TO D01-A15
              MOVE BR-FEE-GL-BRNO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BR-REMIT1 NOT = OBR-REMIT1
      *       CALL "C$JUSTIFY" USING OBR-REMIT1, "L"
              IF BR-REMIT1 NOT = OBR-REMIT1
                 MOVE "BR-REMIT1" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE OBR-REMIT1  TO D01-A15
                 MOVE BR-REMIT1   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BR-REMIT2 NOT = OBR-REMIT2
      *       CALL "C$JUSTIFY" USING OBR-REMIT2, "L"
              IF BR-REMIT2 NOT = OBR-REMIT2
                 MOVE "BR-REMIT2" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE OBR-REMIT2  TO D01-A15
                 MOVE BR-REMIT2   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BR-REMIT3 NOT = OBR-REMIT3
      *       CALL "C$JUSTIFY" USING OBR-REMIT3, "L"
              IF BR-REMIT3 NOT = OBR-REMIT3
                 MOVE "BR-REMIT3" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE OBR-REMIT3  TO D01-A15
                 MOVE BR-REMIT3   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BR-STMNT1 NOT = OBR-STMNT1
      *       CALL "C$JUSTIFY" USING OBR-STMNT1, "L"
              IF BR-STMNT1 NOT = OBR-STMNT1
                 MOVE "BR-STMNT1" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE OBR-STMNT1  TO D01-A15
                 MOVE BR-STMNT1   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BR-STMNT2 NOT = OBR-STMNT2
      *       CALL "C$JUSTIFY" USING OBR-STMNT2, "L"
              IF BR-STMNT2 NOT = OBR-STMNT2
                 MOVE "BR-STMNT2" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE OBR-STMNT2  TO D01-A15
                 MOVE BR-STMNT2   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OBR-INS-PRODUCER-NUM NOT NUMERIC
              MOVE 0 TO OBR-INS-PRODUCER-NUM.
           IF OBR-INS-PRODUCER-NUM NOT = BR-INS-PRODUCER-NUM
              MOVE "BR-INS-PRODUCER-NUM" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-INS-PRODUCER-NUM  TO D01-A15
              MOVE BR-INS-PRODUCER-NUM   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BR-DLRCHK NOT = OBR-DLRCHK
              MOVE "BR-DLRCHK" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-DLRCHK  TO D01-A15
              MOVE BR-DLRCHK   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BR-FRONT-END-FG NOT = OBR-FRONT-END-FG
              MOVE "BR-FRONT-END-FG" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-FRONT-END-FG  TO D01-A15
              MOVE BR-FRONT-END-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BR-EMLNRG-PHONE NOT = OBR-EMLNRG-PHONE
              MOVE "BR-EMLNRG-PHONE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-EMLNRG-PHONE  TO D01-A15
              MOVE BR-EMLNRG-PHONE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BR-POST-GL NOT = OBR-POST-GL
              MOVE "BR-POST-GL" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-POST-GL  TO D01-A15
              MOVE BR-POST-GL   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BR-ALLOTMNTS NOT = OBR-ALLOTMNTS
              MOVE "BR-ALLOTMNTS" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-ALLOTMNTS  TO D01-A15
              MOVE BR-ALLOTMNTS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BR-EXTRACT-AUTO-UPD NOT = OBR-EXTRACT-AUTO-UPD
              MOVE "BR-EXTRACT-AUTO-UPD" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-EXTRACT-AUTO-UPD  TO D01-A15
              MOVE BR-EXTRACT-AUTO-UPD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BR-PCNI-LEDGER NOT = OBR-PCNI-LEDGER
              MOVE "BR-PCNI-LEDGER" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-PCNI-LEDGER  TO D01-A15
              MOVE BR-PCNI-LEDGER   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BR-NEGDLRSV NOT = OBR-NEGDLRSV
              MOVE "BR-NEGDLRSV" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-NEGDLRSV  TO D01-A15
              MOVE BR-NEGDLRSV   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BR-CPCORP-ORDER NOT = OBR-CPCORP-ORDER
              MOVE "BR-CPCORP-ORDER" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-CPCORP-ORDER  TO D01-A15
              MOVE BR-CPCORP-ORDER   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BR-PRIO-MEMO-FLAG NOT = OBR-PRIO-MEMO-FLAG
              MOVE "BR-PRIO-MEMO-FLAG" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-PRIO-MEMO-FLAG  TO D01-A15
              MOVE BR-PRIO-MEMO-FLAG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BR-PREBILL-PL-JD-OPT NOT = OBR-PREBILL-PL-JD-OPT
              MOVE "BR-PREBILL-PL-JD-OPT" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-PREBILL-PL-JD-OPT  TO D01-A15
              MOVE BR-PREBILL-PL-JD-OPT   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BR-LOAN-NUM-FLAG NOT = OBR-LOAN-NUM-FLAG
              MOVE "BR-LOAN-NUM-FLAG" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-LOAN-NUM-FLAG  TO D01-A15
              MOVE BR-LOAN-NUM-FLAG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BR-DOWNPY-FLAG NOT = OBR-DOWNPY-FLAG
              MOVE "BR-DOWNPY-FLAG" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-DOWNPY-FLAG  TO D01-A15
              MOVE BR-DOWNPY-FLAG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BR-AUTO-INS-EARN NOT = OBR-AUTO-INS-EARN
              MOVE "BR-AUTO-INS-EARN" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-AUTO-INS-EARN  TO D01-A15
              MOVE BR-AUTO-INS-EARN   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-OTH-INS-LEVELS NOT = OBR-OTH-INS-LEVELS
              MOVE "BR-OTH-INS-LEVELS" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-OTH-INS-LEVELS  TO D01-A15
              MOVE BR-OTH-INS-LEVELS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-ALLOW-INTERBR NOT = OBR-ALLOW-INTERBR
              MOVE "BR-ALLOW-INTERBR" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-ALLOW-INTERBR  TO D01-A15
              MOVE BR-ALLOW-INTERBR   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-UNER-DLANRP NOT = OBR-UNER-DLANRP
              MOVE "BR-UNER-DLANRP" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-UNER-DLANRP  TO D01-A15
              MOVE BR-UNER-DLANRP   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-SECOND-PROMISE NOT = OBR-SECOND-PROMISE
              MOVE "BR-SECOND-PROMISE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-SECOND-PROMISE  TO D01-A15
              MOVE BR-SECOND-PROMISE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-AVAIL-CREDIT NOT = OBR-AVAIL-CREDIT
              MOVE "BR-AVAIL-CREDIT" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-AVAIL-CREDIT  TO D01-A15
              MOVE BR-AVAIL-CREDIT   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-RECP1 NOT = OBR-RECP1
      *       CALL "C$JUSTIFY" USING OBR-RECP1, "L"
              IF BR-RECP1 NOT = OBR-RECP1
                 MOVE "BR-RECP1" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE OBR-RECP1  TO D01-A15
                 MOVE BR-RECP1   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-RECP2 NOT = OBR-RECP2
      *       CALL "C$JUSTIFY" USING OBR-RECP2, "L"
              IF BR-RECP2 NOT = OBR-RECP2
                 MOVE "BR-RECP2" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE OBR-RECP2  TO D01-A15
                 MOVE BR-RECP2   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-INC-WAIVERS NOT = OBR-INC-WAIVERS
              MOVE "BR-INC-WAIVERS" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-INC-WAIVERS  TO D01-A15
              MOVE BR-INC-WAIVERS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-INC-INS NOT = OBR-INC-INS
              MOVE "BR-INC-INS" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-INC-INS  TO D01-A15
              MOVE BR-INC-INS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OBR-BP-NEXT-BATCH NOT NUMERIC
              MOVE 0 TO OBR-BP-NEXT-BATCH.
           IF OBR-BP-NEXT-BATCH NOT = BR-BP-NEXT-BATCH
              MOVE "BR-BP-NEXT-BATCH" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-BP-NEXT-BATCH  TO D01-A15
              MOVE BR-BP-NEXT-BATCH   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-BP-NOPAYOFF NOT = OBR-BP-NOPAYOFF
              MOVE "BR-BP-NOPAYOFF" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-BP-NOPAYOFF  TO D01-A15
              MOVE BR-BP-NOPAYOFF   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-BP-ODDPAY NOT = OBR-BP-ODDPAY
              MOVE "BR-BP-ODDPAY" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-BP-ODDPAY  TO D01-A15
              MOVE BR-BP-ODDPAY   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BR-BP-LC NOT = OBR-BP-LC
              MOVE "BR-BP-LC" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-BP-LC  TO D01-A15
              MOVE BR-BP-LC   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-BP-NOPRIN NOT = OBR-BP-NOPRIN
              MOVE "BR-BP-NOPRIN" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-BP-NOPRIN  TO D01-A15
              MOVE BR-BP-NOPRIN   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-BP-FLASH NOT = OBR-BP-FLASH
              MOVE "BR-BP-FLASH" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-BP-FLASH  TO D01-A15
              MOVE BR-BP-FLASH   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-BP-ACTIONCD NOT = OBR-BP-ACTIONCD
              MOVE "BR-BP-ACTIONCD" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-BP-ACTIONCD  TO D01-A15
              MOVE BR-BP-ACTIONCD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OBR-BP-MAXCON NOT NUMERIC
              MOVE 0 TO OBR-BP-MAXCON.
           IF OBR-BP-MAXCON NOT = BR-BP-MAXCON
              MOVE "BR-BP-MAXCON" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-BP-MAXCON  TO D01-A15
              MOVE BR-BP-MAXCON   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OBR-BP-MAXREC NOT NUMERIC
              MOVE 0 TO OBR-BP-MAXREC.
           IF OBR-BP-MAXREC NOT = BR-BP-MAXREC
              MOVE "BR-BP-MAXREC" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-BP-MAXREC  TO D01-A15
              MOVE BR-BP-MAXREC   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-BP-PL NOT = OBR-BP-PL
              MOVE "BR-BP-PL" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-BP-PL  TO D01-A15
              MOVE BR-BP-PL   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-BP-JD NOT = OBR-BP-JD
              MOVE "BR-BP-JD" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-BP-JD  TO D01-A15
              MOVE BR-BP-JD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OBR-BP-BRANCH NOT NUMERIC
              MOVE 0 TO OBR-BP-BRANCH.
           IF OBR-BP-BRANCH NOT = BR-BP-BRANCH
              MOVE "BR-BP-BRANCH" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-BP-BRANCH  TO D01-A15
              MOVE BR-BP-BRANCH   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-AGE-ERN NOT = OBR-AGE-ERN
              MOVE "BR-AGE-ERN" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-AGE-ERN  TO D01-A15
              MOVE BR-AGE-ERN   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-OTHER-BRANCH-GL NOT = OBR-OTHER-BRANCH-GL
              MOVE "BR-OTHER-BRANCH-GL" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-OTHER-BRANCH-GL  TO D01-A15
              MOVE BR-OTHER-BRANCH-GL   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-SALES-SMALL NOT = OBR-SALES-SMALL
              MOVE "BR-SALES-SMALL" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-SALES-SMALL  TO D01-A15
              MOVE BR-SALES-SMALL   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-NEW-COLL-INQUIRY NOT = OBR-NEW-COLL-INQUIRY
              MOVE "BR-NEW-COLL-INQUIRY" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-NEW-COLL-INQUIRY  TO D01-A15
              MOVE BR-NEW-COLL-INQUIRY   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-CHANGE-MEMO NOT = OBR-CHANGE-MEMO
              MOVE "BR-CHANGE-MEMO" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-CHANGE-MEMO  TO D01-A15
              MOVE BR-CHANGE-MEMO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-INS-ADDON NOT = OBR-INS-ADDON
              MOVE "BR-INS-ADDON" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-INS-ADDON  TO D01-A15
              MOVE BR-INS-ADDON   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-TRAILER-REC NOT = OBR-TRAILER-REC
              MOVE "BR-TRAILER-REC" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-TRAILER-REC  TO D01-A15
              MOVE BR-TRAILER-REC   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-ALDEL-FLAG NOT = OBR-ALDEL-FLAG
              MOVE "BR-ALDEL-FLAG" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-ALDEL-FLAG  TO D01-A15
              MOVE BR-ALDEL-FLAG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-IB-LCHG-MESS NOT = OBR-IB-LCHG-MESS
              MOVE "BR-IB-LCHG-MESS" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-IB-LCHG-MESS  TO D01-A15
              MOVE BR-IB-LCHG-MESS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-COL-QUEUE-PAST-DUE NOT = OBR-COL-QUEUE-PAST-DUE
              MOVE "BR-COL-QUEUE-PAST-DUE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-COL-QUEUE-PAST-DUE  TO D01-A15
              MOVE BR-COL-QUEUE-PAST-DUE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OBR-LI-DLCOMM-CMSI-PP-MAX NOT NUMERIC
              MOVE 0 TO OBR-LI-DLCOMM-CMSI-PP-MAX.
           IF OBR-LI-DLCOMM-CMSI-PP-MAX NOT = BR-LI-DLCOMM-CMSI-PP-MAX
              MOVE "BR-LI-DLCOMM-CMSI-PP-MAX" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-LI-DLCOMM-CMSI-PP-MAX  TO D01-A15-NUM
              MOVE BR-LI-DLCOMM-CMSI-PP-MAX   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-ADDON-FEECD-O2(1) NOT = OBR-ADDON-FEECD-O2(1)
              MOVE "BR-ADDON-FEECD-O2(1)" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-ADDON-FEECD-O2(1)  TO D01-A15
              MOVE BR-ADDON-FEECD-O2(1)   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-ADDON-FEECD-O2(2) NOT = OBR-ADDON-FEECD-O2(2)
              MOVE "BR-ADDON-FEECD-O2(2)" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-ADDON-FEECD-O2(2)  TO D01-A15
              MOVE BR-ADDON-FEECD-O2(2)   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-AUTO-CHECK NOT = OBR-AUTO-CHECK
              MOVE "BR-AUTO-CHECK" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-AUTO-CHECK  TO D01-A15
              MOVE BR-AUTO-CHECK   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-RECEIPT-PROMDATE NOT = OBR-RECEIPT-PROMDATE
              MOVE "BR-RECEIPT-PROMDATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-RECEIPT-PROMDATE  TO D01-A15
              MOVE BR-RECEIPT-PROMDATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-RECEIPT-INTPD NOT = OBR-RECEIPT-INTPD
              MOVE "BR-RECEIPT-INTPD" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-RECEIPT-INTPD  TO D01-A15
              MOVE BR-RECEIPT-INTPD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-COLL-ID-SCAN NOT = OBR-COLL-ID-SCAN
              MOVE "BR-COLL-ID-SCAN" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-COLL-ID-SCAN  TO D01-A15
              MOVE BR-COLL-ID-SCAN   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-STOP-ON-COLL NOT = OBR-STOP-ON-COLL
              MOVE "BR-STOP-ON-COLL" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-STOP-ON-COLL  TO D01-A15
              MOVE BR-STOP-ON-COLL   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BR-DUEDATE-CHANGE-CD NOT = OBR-DUEDATE-CHANGE-CD
              MOVE "BR-DUEDATE-CHANGE-CD" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-DUEDATE-CHANGE-CD  TO D01-A15
              MOVE BR-DUEDATE-CHANGE-CD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-CHECK-EOM-FG NOT = OBR-CHECK-EOM-FG
              MOVE "BR-CHECK-EOM-FG" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-CHECK-EOM-FG  TO D01-A15
              MOVE BR-CHECK-EOM-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-VOLUME-DLANRP NOT = OBR-VOLUME-DLANRP
              MOVE "BR-VOLUME-DLANRP" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-VOLUME-DLANRP  TO D01-A15
              MOVE BR-VOLUME-DLANRP   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-PDTH-NXDUE NOT = OBR-PDTH-NXDUE
              MOVE "BR-PDTH-NXDUE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-PDTH-NXDUE  TO D01-A15
              MOVE BR-PDTH-NXDUE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-LC-CODE NOT = OBR-LC-CODE
              MOVE "BR-LC-CODE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-LC-CODE  TO D01-A15
              MOVE BR-LC-CODE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-VALIDATE-PRUS NOT = OBR-VALIDATE-PRUS
              MOVE "BR-VALIDATE-PRUS" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-VALIDATE-PRUS  TO D01-A15
              MOVE BR-VALIDATE-PRUS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-AUTO-OPEN-BAL NOT = OBR-AUTO-OPEN-BAL
              MOVE "BR-AUTO-OPEN-BAL" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-AUTO-OPEN-BAL  TO D01-A15
              MOVE BR-AUTO-OPEN-BAL   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 9
              IF OBR-OPEN-BAL(SUB) NOT NUMERIC
                 MOVE 0 TO OBR-OPEN-BAL(SUB)
              END-IF
              IF OBR-OPEN-BAL(SUB) NOT = BR-OPEN-BAL(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "BR-OPEN-BAL",SUB INTO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE OBR-OPEN-BAL(SUB)  TO D01-A15-NUM
                 MOVE BR-OPEN-BAL(SUB)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
              IF BR-TENDER-CD(SUB) NOT = OBR-TENDER-CD(SUB)
      *          CALL "C$JUSTIFY" USING OBR-TENDER-CD(SUB), "L"
                 IF BR-TENDER-CD(SUB) NOT = OBR-TENDER-CD(SUB)
                    MOVE SPACES TO D01-TEXT
                    STRING "BR-TENDER-CD",SUB INTO D01-TEXT
                    MOVE BR-NO      TO D01-BRNO
                    MOVE OBR-TENDER-CD(SUB)  TO D01-A15
                    MOVE BR-TENDER-CD(SUB)   TO D01-S35
                    PERFORM WRITE-DETAIL-LINE-ERROR
                 END-IF
              END-IF
           END-PERFORM.

           IF BR-DLPARV-ADJ-FG NOT = OBR-DLPARV-ADJ-FG
              MOVE "BR-DLPARV-ADJ-FG" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-DLPARV-ADJ-FG  TO D01-A15
              MOVE BR-DLPARV-ADJ-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BR-OVERRIDE-BR-STATE NOT = OBR-OVERRIDE-BR-STATE
              MOVE "BR-OVERRIDE-BR-STATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-OVERRIDE-BR-STATE  TO D01-A15
              MOVE BR-OVERRIDE-BR-STATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OBR-DEFAULT-SC-CLASS NOT NUMERIC
              MOVE 0 TO OBR-DEFAULT-SC-CLASS.
           IF OBR-DEFAULT-SC-CLASS NOT = BR-DEFAULT-SC-CLASS
              MOVE "BR-DEFAULT-SC-CLASS" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-DEFAULT-SC-CLASS  TO D01-A15
              MOVE BR-DEFAULT-SC-CLASS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BR-RECDEL-NOT-REC NOT = OBR-RECDEL-NOT-REC
              MOVE "BR-RECDEL-NOT-REC" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-RECDEL-NOT-REC  TO D01-A15
              MOVE BR-RECDEL-NOT-REC   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BR-POST-AHEAD-FG NOT = OBR-POST-AHEAD-FG
              MOVE "BR-POST-AHEAD-FG" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-POST-AHEAD-FG  TO D01-A15
              MOVE BR-POST-AHEAD-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BR-CAN-LOANS-FG NOT = OBR-CAN-LOANS-FG
              MOVE "BR-CAN-LOANS-FG" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-CAN-LOANS-FG  TO D01-A15
              MOVE BR-CAN-LOANS-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BR-NON-MONTHLY-CAN-LOANS NOT = OBR-NON-MONTHLY-CAN-LOANS
              MOVE "BR-NON-MONTHLY-CAN-LOANS" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-NON-MONTHLY-CAN-LOANS  TO D01-A15
              MOVE BR-NON-MONTHLY-CAN-LOANS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BR-BP-ALLOW-PMT-ACCT-OTHBAL NOT =
                                OBR-BP-ALLOW-PMT-ACCT-OTHBAL
              MOVE "BR-BP-ALLOW-PMT-ACCT-OTHBAL" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-BP-ALLOW-PMT-ACCT-OTHBAL  TO D01-A15
              MOVE BR-BP-ALLOW-PMT-ACCT-OTHBAL   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BR-LBOX-ALLOW-PMT-ACCT-OTHBAL NOT =
                                OBR-LBOX-ALLOW-PMT-ACCT-OTHBAL
              MOVE "BR-LBOX-ALLOW-PMT-ACCT-OTHBAL" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-LBOX-ALLOW-PMT-ACCT-OTHBAL  TO D01-A15
              MOVE BR-LBOX-ALLOW-PMT-ACCT-OTHBAL   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OBR-REGION-NUMBER NOT NUMERIC
              MOVE 0 TO OBR-REGION-NUMBER.
           IF OBR-REGION-NUMBER NOT = BR-REGION-NUMBER
              MOVE "BR-REGION-NUMBER" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-REGION-NUMBER  TO D01-A15
              MOVE BR-REGION-NUMBER   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BR-OUR-FEE NOT = OBR-OUR-FEE
              MOVE "BR-OUR-FEE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-OUR-FEE  TO D01-A15
              MOVE BR-OUR-FEE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
              IF OBR-REB-METHOD(SUB) NOT NUMERIC
                 MOVE 0 TO OBR-REB-METHOD(SUB)
              END-IF
              IF OBR-REB-METHOD(SUB) NOT = BR-REB-METHOD(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "BR-REB-METHOD",SUB INTO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE OBR-REB-METHOD(SUB)  TO D01-A15
                 MOVE BR-REB-METHOD(SUB)   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OBR-REBDAYS-MONTH(SUB) NOT NUMERIC
                 MOVE 0 TO OBR-REBDAYS-MONTH(SUB)
              END-IF
              IF OBR-REBDAYS-MONTH(SUB) NOT = BR-REBDAYS-MONTH(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "BR-REBDAYS-MONTH",SUB INTO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE OBR-REBDAYS-MONTH(SUB)  TO D01-A15
                 MOVE BR-REBDAYS-MONTH(SUB)   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OBR-ACCRUAL-MONTHS(SUB) NOT NUMERIC
                 MOVE 0 TO OBR-ACCRUAL-MONTHS(SUB)
              END-IF
              IF OBR-ACCRUAL-MONTHS(SUB) NOT = BR-ACCRUAL-MONTHS(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "BR-ACCRUAL-MONTHS",SUB INTO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE OBR-ACCRUAL-MONTHS(SUB)  TO D01-A15
                 MOVE BR-ACCRUAL-MONTHS(SUB)   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.

           IF OBR-GLOVERSHORT NOT NUMERIC
              MOVE 0 TO OBR-GLOVERSHORT.
           IF OBR-GLOVERSHORT NOT = BR-GLOVERSHORT
              MOVE "BR-GLOVERSHORT" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-GLOVERSHORT  TO D01-A15
              MOVE BR-GLOVERSHORT   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OBR-GLBANK NOT NUMERIC
              MOVE 0 TO OBR-GLBANK.
           IF OBR-GLBANK NOT = BR-GLBANK
              MOVE "BR-GLBANK" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-GLBANK  TO D01-A15
              MOVE BR-GLBANK   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OBR-GLONHAND NOT NUMERIC
              MOVE 0 TO OBR-GLONHAND.
           IF OBR-GLONHAND NOT = BR-GLONHAND
              MOVE "BR-GLONHAND" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-GLONHAND  TO D01-A15
              MOVE BR-GLONHAND   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OBR-BANKBAL NOT NUMERIC
              MOVE 0 TO OBR-BANKBAL.
           IF OBR-BANKBAL NOT = BR-BANKBAL
              MOVE "BR-BANKBAL" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-BANKBAL  TO D01-A15-NUM
              MOVE BR-BANKBAL   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OBR-RETAIN-AMT NOT NUMERIC
              MOVE 0 TO OBR-RETAIN-AMT.
           IF OBR-RETAIN-AMT NOT = BR-RETAIN-AMT
              MOVE "BR-RETAIN-AMT" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-RETAIN-AMT  TO D01-A15-NUM
              MOVE BR-RETAIN-AMT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-LCARDFG NOT = OBR-LCARDFG
              MOVE "BR-LCARDFG" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-LCARDFG  TO D01-A15
              MOVE BR-LCARDFG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-DATA-PATH NOT = OBR-DATA-PATH
      *       CALL "C$JUSTIFY" USING OBR-DATA-PATH, "L"
              IF BR-DATA-PATH NOT = OBR-DATA-PATH
                 MOVE "BR-DATA-PATH" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE OBR-DATA-PATH  TO D01-A15
                 MOVE BR-DATA-PATH   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
      *     IF BR-MACHINE NOT = OBR-MACHINE
      *        MOVE "BR-MACHINE" TO D01-TEXT
      *        MOVE BR-NO      TO D01-BRNO
      *        MOVE OBR-MACHINE  TO D01-A15
      *        MOVE BR-MACHINE   TO D01-S35
      *        PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BR-MANAGER NOT = OBR-MANAGER
      *       CALL "C$JUSTIFY" USING OBR-MANAGER, "L"
              IF BR-MANAGER NOT = OBR-MANAGER
                 MOVE "BR-MANAGER" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE OBR-MANAGER  TO D01-A15
                 MOVE BR-MANAGER   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-EOD-PROCEDURE NOT = OBR-EOD-PROCEDURE
      *       CALL "C$JUSTIFY" USING OBR-EOD-PROCEDURE, "L"
              IF BR-EOD-PROCEDURE NOT = OBR-EOD-PROCEDURE
                 MOVE "BR-EOD-PROCEDURE" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE OBR-EOD-PROCEDURE  TO D01-A15
                 MOVE BR-EOD-PROCEDURE   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-BP-PAYOFF-NONCASH NOT = OBR-BP-PAYOFF-NONCASH
              MOVE "BR-BP-PAYOFF-NONCASH" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-BP-PAYOFF-NONCASH  TO D01-A15
              MOVE BR-BP-PAYOFF-NONCASH   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-BP-PAYOFF-OV NOT = OBR-BP-PAYOFF-OV
              MOVE "BR-BP-PAYOFF-OV" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-BP-PAYOFF-OV  TO D01-A15
              MOVE BR-BP-PAYOFF-OV   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OBR-LI-DLCOMM-CMSI-O3-MAX NOT NUMERIC
              MOVE 0 TO OBR-LI-DLCOMM-CMSI-O3-MAX.
           IF OBR-LI-DLCOMM-CMSI-O3-MAX NOT = BR-LI-DLCOMM-CMSI-O3-MAX
              MOVE "BR-LI-DLCOMM-CMSI-O3-MAX" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-LI-DLCOMM-CMSI-O3-MAX  TO D01-A15-NUM
              MOVE BR-LI-DLCOMM-CMSI-O3-MAX   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-LR-GROUP-COLLID NOT = OBR-LR-GROUP-COLLID
              MOVE "BR-LR-GROUP-COLLID" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-LR-GROUP-COLLID  TO D01-A15
              MOVE BR-LR-GROUP-COLLID   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-UNITPER-FG NOT = OBR-UNITPER-FG
              MOVE "BR-UNITPER-FG" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-UNITPER-FG  TO D01-A15
              MOVE BR-UNITPER-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-UNITPER-Y-D-FG NOT = OBR-UNITPER-Y-D-FG
              MOVE "BR-UNITPER-Y-D-FG" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-UNITPER-Y-D-FG  TO D01-A15
              MOVE BR-UNITPER-Y-D-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-NO-PROMPT-FOR-OTHBR NOT = OBR-NO-PROMPT-FOR-OTHBR
              MOVE "BR-NO-PROMPT-FOR-OTHBR" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-NO-PROMPT-FOR-OTHBR  TO D01-A15
              MOVE BR-NO-PROMPT-FOR-OTHBR   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-WORLD-BRANCH-TYPE NOT = OBR-WORLD-BRANCH-TYPE
              MOVE "BR-WORLD-BRANCH-TYPE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-WORLD-BRANCH-TYPE  TO D01-A15
              MOVE BR-WORLD-BRANCH-TYPE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-COL-QUEUE-BEHAVIOR-FG NOT = OBR-COL-QUEUE-BEHAVIOR-FG
              MOVE "BR-COL-QUEUE-BEHAVIOR-FG" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-COL-QUEUE-BEHAVIOR-FG  TO D01-A15
              MOVE BR-COL-QUEUE-BEHAVIOR-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-CONVERT-AUTO-ACCTNO NOT = OBR-CONVERT-AUTO-ACCTNO
              MOVE "BR-CONVERT-AUTO-ACCTNO" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-CONVERT-AUTO-ACCTNO  TO D01-A15
              MOVE BR-CONVERT-AUTO-ACCTNO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-TS-REPO-INS-FG NOT = OBR-TS-REPO-INS-FG
              MOVE "BR-TS-REPO-INS-FG" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-TS-REPO-INS-FG  TO D01-A15
              MOVE BR-TS-REPO-INS-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-BOOK-SF-BR-ONLY NOT = OBR-BOOK-SF-BR-ONLY
              MOVE "BR-BOOK-SF-BR-ONLY" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-BOOK-SF-BR-ONLY  TO D01-A15
              MOVE BR-BOOK-SF-BR-ONLY   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-FORCE-VALID-CITY-COUNTY NOT =
                                  OBR-FORCE-VALID-CITY-COUNTY
              MOVE "BR-FORCE-VALID-CITY-COUNTY" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-FORCE-VALID-CITY-COUNTY  TO D01-A15
              MOVE BR-FORCE-VALID-CITY-COUNTY   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-SET-COLLCD-DL-MFGCD NOT = OBR-SET-COLLCD-DL-MFGCD
              MOVE "BR-SET-COLLCD-DL-MFGCD" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-SET-COLLCD-DL-MFGCD  TO D01-A15
              MOVE BR-SET-COLLCD-DL-MFGCD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BR-DEFAULT-SF-UNITPER-CD NOT = OBR-DEFAULT-SF-UNITPER-CD
              MOVE "BR-DEFAULT-SF-UNITPER-CD" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-DEFAULT-SF-UNITPER-CD  TO D01-A15
              MOVE BR-DEFAULT-SF-UNITPER-CD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-DEFAULT-SF-PURCD NOT = OBR-DEFAULT-SF-PURCD
      *       CALL "C$JUSTIFY" USING OBR-DEFAULT-SF-PURCD, "L"
              IF BR-DEFAULT-SF-PURCD NOT = OBR-DEFAULT-SF-PURCD
                 MOVE "BR-DEFAULT-SF-PURCD" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE OBR-DEFAULT-SF-PURCD  TO D01-A15
                 MOVE BR-DEFAULT-SF-PURCD   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-DEFAULT-LN-UNITPER-CD NOT = OBR-DEFAULT-LN-UNITPER-CD
              MOVE "BR-DEFAULT-LN-UNITPER-CD" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-DEFAULT-LN-UNITPER-CD  TO D01-A15
              MOVE BR-DEFAULT-LN-UNITPER-CD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-DEFAULT-LN-PURCD NOT = OBR-DEFAULT-LN-PURCD
      *       CALL "C$JUSTIFY" USING OBR-DEFAULT-LN-PURCD, "L"
              IF BR-DEFAULT-LN-PURCD NOT = OBR-DEFAULT-LN-PURCD
                 MOVE "BR-DEFAULT-LN-PURCD" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE OBR-DEFAULT-LN-PURCD  TO D01-A15
                 MOVE BR-DEFAULT-LN-PURCD   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-TAP-OTHER-DL NOT = OBR-TAP-OTHER-DL
              MOVE "BR-TAP-OTHER-DL" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-TAP-OTHER-DL  TO D01-A15
              MOVE BR-TAP-OTHER-DL   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-AH-AND-CL NOT = OBR-AH-AND-CL
              MOVE "BR-AH-AND-CL" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-AH-AND-CL  TO D01-A15
              MOVE BR-AH-AND-CL   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-NO-CASH-PROCEEDS NOT = OBR-NO-CASH-PROCEEDS
              MOVE "BR-NO-CASH-PROCEEDS" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-NO-CASH-PROCEEDS  TO D01-A15
              MOVE BR-NO-CASH-PROCEEDS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-RI-BEFORE-REPO NOT = OBR-RI-BEFORE-REPO
              MOVE "BR-RI-BEFORE-REPO" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-RI-BEFORE-REPO  TO D01-A15
              MOVE BR-RI-BEFORE-REPO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-RI-BEFORE-P4P5 NOT = OBR-RI-BEFORE-P4P5
              MOVE "BR-RI-BEFORE-P4P5" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-RI-BEFORE-P4P5  TO D01-A15
              MOVE BR-RI-BEFORE-P4P5   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-NOT-OURS-REFUND NOT = OBR-NOT-OURS-REFUND
              MOVE "BR-NOT-OURS-REFUND" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-NOT-OURS-REFUND  TO D01-A15
              MOVE BR-NOT-OURS-REFUND   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-SFDLO3-WINDOW-FG NOT = OBR-SFDLO3-WINDOW-FG
              MOVE "BR-SFDLO3-WINDOW-FG" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-SFDLO3-WINDOW-FG  TO D01-A15
              MOVE BR-SFDLO3-WINDOW-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-CALC-INS-ADJ NOT = OBR-CALC-INS-ADJ
              MOVE "BR-CALC-INS-ADJ" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-CALC-INS-ADJ  TO D01-A15
              MOVE BR-CALC-INS-ADJ   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-AC-BEFORE-REPO NOT = OBR-AC-BEFORE-REPO
              MOVE "BR-AC-BEFORE-REPO" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-AC-BEFORE-REPO  TO D01-A15
              MOVE BR-AC-BEFORE-REPO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-AC-BEFORE-P4P5 NOT = OBR-AC-BEFORE-P4P5
              MOVE "BR-AC-BEFORE-P4P5" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-AC-BEFORE-P4P5  TO D01-A15
              MOVE BR-AC-BEFORE-P4P5   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OBR-BANK-ROUTING-NO NOT NUMERIC
              MOVE 0 TO OBR-BANK-ROUTING-NO.
           IF OBR-BANK-ROUTING-NO NOT = BR-BANK-ROUTING-NO
              MOVE "BR-BANK-ROUTING-NO" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-BANK-ROUTING-NO  TO D01-A15-NUM
              MOVE BR-BANK-ROUTING-NO   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OBR-BANK-CLEARING-GL NOT NUMERIC
              MOVE 0 TO OBR-BANK-CLEARING-GL.
           IF OBR-BANK-CLEARING-GL NOT = BR-BANK-CLEARING-GL
              MOVE "BR-BANK-CLEARING-GL" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-BANK-CLEARING-GL  TO D01-A15-NUM
              MOVE BR-BANK-CLEARING-GL   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OBR-GLBANK-DEPOSIT NOT NUMERIC
              MOVE 0 TO OBR-GLBANK-DEPOSIT.
           IF OBR-GLBANK-DEPOSIT NOT = BR-GLBANK-DEPOSIT
              MOVE "BR-GLBANK-DEPOSIT" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-GLBANK-DEPOSIT  TO D01-A15-NUM
              MOVE BR-GLBANK-DEPOSIT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-BANK-ACCTNO NOT = OBR-BANK-ACCTNO
      *       CALL "C$JUSTIFY" USING OBR-BANK-ACCTNO, "L"
              IF BR-BANK-ACCTNO NOT = OBR-BANK-ACCTNO
                 MOVE "BR-BANK-ACCTNO" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE OBR-BANK-ACCTNO  TO D01-A15
                 MOVE BR-BANK-ACCTNO   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OBR-DEFAULT-SC-SUBCLASS NOT NUMERIC
              MOVE 0 TO OBR-DEFAULT-SC-SUBCLASS.
           IF OBR-DEFAULT-SC-SUBCLASS NOT = BR-DEFAULT-SC-SUBCLASS
              MOVE "BR-DEFAULT-SC-SUBCLASS" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-DEFAULT-SC-SUBCLASS  TO D01-A15-NUM
              MOVE BR-DEFAULT-SC-SUBCLASS   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-NO-OUT-DLR-RSV NOT = OBR-NO-OUT-DLR-RSV
              MOVE "BR-NO-OUT-DLR-RSV" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-NO-OUT-DLR-RSV  TO D01-A15
              MOVE BR-NO-OUT-DLR-RSV   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-COLLECTOR-QUEUE NOT = OBR-COLLECTOR-QUEUE
              MOVE "BR-COLLECTOR-QUEUE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-COLLECTOR-QUEUE  TO D01-A15
              MOVE BR-COLLECTOR-QUEUE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-COLL-QUEUE-CON-REC NOT = OBR-COLL-QUEUE-CON-REC
              MOVE "BR-COLL-QUEUE-CON-REC" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-COLL-QUEUE-CON-REC  TO D01-A15
              MOVE BR-COLL-QUEUE-CON-REC   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-JE-ALLOCATION-SKIP NOT = OBR-JE-ALLOCATION-SKIP
              MOVE "BR-JE-ALLOCATION-SKIP" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-JE-ALLOCATION-SKIP  TO D01-A15
              MOVE BR-JE-ALLOCATION-SKIP   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-EXPAND-APP-INPUT NOT = OBR-EXPAND-APP-INPUT
              MOVE "BR-EXPAND-APP-INPUT" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-EXPAND-APP-INPUT  TO D01-A15
              MOVE BR-EXPAND-APP-INPUT   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-A50-CPCORP NOT = OBR-A50-CPCORP
              MOVE "BR-A50-CPCORP" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-A50-CPCORP  TO D01-A15
              MOVE BR-A50-CPCORP   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-INBR-BILNSC-FG NOT = OBR-INBR-BILNSC-FG
              MOVE "BR-INBR-BILNSC-FG" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-INBR-BILNSC-FG  TO D01-A15
              MOVE BR-INBR-BILNSC-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-INBR-BRIEF-FG NOT = OBR-INBR-BRIEF-FG
              MOVE "BR-INBR-BRIEF-FG" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-INBR-BRIEF-FG  TO D01-A15
              MOVE BR-INBR-BRIEF-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-LEDGER-JUST-INTCHG NOT = OBR-LEDGER-JUST-INTCHG
              MOVE "BR-LEDGER-JUST-INTCHG" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-LEDGER-JUST-INTCHG  TO D01-A15
              MOVE BR-LEDGER-JUST-INTCHG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OBR-BYINQ-START-PAGE NOT NUMERIC
              MOVE 0 TO OBR-BYINQ-START-PAGE.
           IF OBR-BYINQ-START-PAGE NOT = BR-BYINQ-START-PAGE
              MOVE "BR-BYINQ-START-PAGE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-BYINQ-START-PAGE  TO D01-A15-NUM
              MOVE BR-BYINQ-START-PAGE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OBR-DSINQ-START-PAGE NOT NUMERIC
              MOVE 0 TO OBR-DSINQ-START-PAGE.
           IF OBR-DSINQ-START-PAGE NOT = BR-DSINQ-START-PAGE
              MOVE "BR-DSINQ-START-PAGE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-DSINQ-START-PAGE  TO D01-A15-NUM
              MOVE BR-DSINQ-START-PAGE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-APP-REQUIRED-FOR-INPUT NOT = OBR-APP-REQUIRED-FOR-INPUT
              MOVE "BR-APP-REQUIRED-FOR-INPUT" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-APP-REQUIRED-FOR-INPUT  TO D01-A15
              MOVE BR-APP-REQUIRED-FOR-INPUT   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-CREDIT-SCORING-FG NOT = OBR-CREDIT-SCORING-FG
              MOVE "BR-CREDIT-SCORING-FG" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-CREDIT-SCORING-FG  TO D01-A15
              MOVE BR-CREDIT-SCORING-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-COL-QUEUE-LOW-TO-HI NOT = OBR-COL-QUEUE-LOW-TO-HI
              MOVE "BR-COL-QUEUE-LOW-TO-HI" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-COL-QUEUE-LOW-TO-HI  TO D01-A15
              MOVE BR-COL-QUEUE-LOW-TO-HI   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-CAN-GROUP-ID NOT = OBR-CAN-GROUP-ID
      *       CALL "C$JUSTIFY" USING OBR-CAN-GROUP-ID, "L"
              IF BR-CAN-GROUP-ID NOT = OBR-CAN-GROUP-ID
                 MOVE "BR-CAN-GROUP-ID" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE OBR-CAN-GROUP-ID  TO D01-A15
                 MOVE BR-NAME   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BR-AUTO-LNSTAT-WINDOW NOT = OBR-AUTO-LNSTAT-WINDOW
              MOVE "BR-AUTO-LNSTAT-WINDOW" TO D01-TEXT
              MOVE BR-NO                   TO D01-BRNO
              MOVE OBR-AUTO-LNSTAT-WINDOW  TO D01-A15
              MOVE BR-AUTO-LNSTAT-WINDOW   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OBR-MACHINE = MACHINE-R2
              IF OBR-TIME-ZONE-FACTOR = 0
                 MOVE -1 TO OBR-TIME-ZONE-FACTOR
              ELSE
              IF OBR-TIME-ZONE-FACTOR = -1
                 MOVE -2 TO OBR-TIME-ZONE-FACTOR
              ELSE
              IF OBR-TIME-ZONE-FACTOR = -2
                 MOVE -3 TO OBR-TIME-ZONE-FACTOR.
           IF OBR-TIME-ZONE-FACTOR NOT = BR-TIME-ZONE-FACTOR
              MOVE "BR-TIME-ZONE-FACTOR" TO D01-TEXT
              MOVE BR-NO                 TO D01-BRNO
              MOVE OBR-TIME-ZONE-FACTOR  TO D01-A15-NUM
              MOVE BR-TIME-ZONE-FACTOR   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BR-LONPF-OPT-DISPLAY NOT = OBR-LONPF-OPT-DISPLAY
              MOVE "BR-LONPF-OPT-DISPLAY" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-LONPF-OPT-DISPLAY  TO D01-A15
              MOVE BR-LONPF-OPT-DISPLAY   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-GIFTNET NOT = OBR-GIFTNET
              MOVE "BR-GIFTNET" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-GIFTNET  TO D01-A15
              MOVE BR-GIFTNET   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-GIFTNET-LEDGER-LINE NOT = OBR-GIFTNET-LEDGER-LINE
              MOVE "BR-GIFTNET-LEDGER-LINE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-GIFTNET-LEDGER-LINE  TO D01-A15
              MOVE BR-GIFTNET-LEDGER-LINE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OBR-NO-FT-EMP NOT NUMERIC
              MOVE 0 TO OBR-NO-FT-EMP.
           IF OBR-NO-FT-EMP NOT = BR-NO-FT-EMP
              MOVE "BR-NO-FT-EMP" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-NO-FT-EMP  TO D01-A15-NUM
              MOVE BR-NO-FT-EMP   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OBR-NO-PT-EMP NOT NUMERIC
              MOVE 0 TO OBR-NO-PT-EMP.
           IF OBR-NO-PT-EMP NOT = BR-NO-PT-EMP
              MOVE "BR-NO-PT-EMP" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-NO-PT-EMP  TO D01-A15-NUM
              MOVE BR-NO-PT-EMP   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-BALLOON-LOANS-FG NOT = OBR-BALLOON-LOANS-FG
              MOVE "BR-BALLOON-LOANS-FG" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-BALLOON-LOANS-FG  TO D01-A15
              MOVE BR-BALLOON-LOANS-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-GL-TYPE-ALLOWED NOT = OBR-GL-TYPE-ALLOWED
              MOVE "BR-GL-TYPE-ALLOWED" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-GL-TYPE-ALLOWED  TO D01-A15
              MOVE BR-GL-TYPE-ALLOWED   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-GL-ENTRY-DEFAULT NOT = OBR-GL-ENTRY-DEFAULT
              MOVE "BR-GL-ENTRY-DEFAULT" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-GL-ENTRY-DEFAULT  TO D01-A15
              MOVE BR-GL-ENTRY-DEFAULT   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-MIDSTREAM-1STPAY-FG NOT = OBR-MIDSTREAM-1STPAY-FG
              MOVE "BR-MIDSTREAM-1STPAY-FG" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-MIDSTREAM-1STPAY-FG  TO D01-A15
              MOVE BR-MIDSTREAM-1STPAY-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OBR-NODEF-1ST-X-MONTHS NOT NUMERIC
              MOVE 0 TO OBR-NODEF-1ST-X-MONTHS.
           IF OBR-NODEF-1ST-X-MONTHS NOT = BR-NODEF-1ST-X-MONTHS
              MOVE "BR-NODEF-1ST-X-MONTHS" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-NODEF-1ST-X-MONTHS  TO D01-A15-NUM
              MOVE BR-NODEF-1ST-X-MONTHS   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-ADD-INS-FEECODE NOT = OBR-ADD-INS-FEECODE
      *       CALL "C$JUSTIFY" USING OBR-ADD-INS-FEECODE, "L"
              IF BR-ADD-INS-FEECODE NOT = OBR-ADD-INS-FEECODE
                 MOVE "BR-ADD-INS-FEECODE" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE OBR-ADD-INS-FEECODE  TO D01-A15
                 MOVE BR-ADD-INS-FEECODE   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-ROAD-G-SGL-FEECODE NOT = OBR-ROAD-G-SGL-FEECODE
      *       CALL "C$JUSTIFY" USING OBR-ROAD-G-SGL-FEECODE, "L"
              IF BR-ROAD-G-SGL-FEECODE NOT = OBR-ROAD-G-SGL-FEECODE
                 MOVE "BR-ROAD-G-SGL-FEECODE" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE OBR-ROAD-G-SGL-FEECODE  TO D01-A15
                 MOVE BR-ROAD-G-SGL-FEECODE   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-ROAD-G-FAM-FEECODE NOT = OBR-ROAD-G-FAM-FEECODE
      *       CALL "C$JUSTIFY" USING OBR-ROAD-G-FAM-FEECODE, "L"
              IF BR-ROAD-G-FAM-FEECODE NOT = OBR-ROAD-G-FAM-FEECODE
                 MOVE "BR-ROAD-G-FAM-FEECODE" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE OBR-ROAD-G-FAM-FEECODE  TO D01-A15
                 MOVE BR-ROAD-G-FAM-FEECODE   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-NODEF-IF-ZERO-AMT NOT = OBR-NODEF-IF-ZERO-AMT
              MOVE "BR-NODEF-IF-ZERO-AMT" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-NODEF-IF-ZERO-AMT  TO D01-A15
              MOVE BR-NODEF-IF-ZERO-AMT   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-ASSESS-LATE-CHGS NOT = OBR-ASSESS-LATE-CHGS
              MOVE "BR-ASSESS-LATE-CHGS" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-ASSESS-LATE-CHGS  TO D01-A15
              MOVE BR-ASSESS-LATE-CHGS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OBR-NEXT-SOLICIT-NO NOT NUMERIC
              MOVE 0 TO OBR-NEXT-SOLICIT-NO.
           IF OBR-NEXT-SOLICIT-NO NOT = BR-NEXT-SOLICIT-NO
              MOVE "BR-NEXT-SOLICIT-NO" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-NEXT-SOLICIT-NO  TO D01-A15-NUM
              MOVE BR-NEXT-SOLICIT-NO   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-ADDON-FEECD-O3(1) NOT = OBR-ADDON-FEECD-O3(1)
      *       CALL "C$JUSTIFY" USING OBR-ADDON-FEECD-O3(1), "L"
              IF BR-ADDON-FEECD-O3(1) NOT = OBR-ADDON-FEECD-O3(1)
                 MOVE "BR-ADDON-FEECD-O3(1)" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE OBR-ADDON-FEECD-O3(1)  TO D01-A15
                 MOVE BR-ADDON-FEECD-O3(1)   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-ADDON-FEECD-O3(2) NOT = OBR-ADDON-FEECD-O3(2)
      *       CALL "C$JUSTIFY" USING OBR-ADDON-FEECD-O3(2), "L"
              IF BR-ADDON-FEECD-O3(2) NOT = OBR-ADDON-FEECD-O3(2)
                 MOVE "BR-ADDON-FEECD-O3(2)" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE OBR-ADDON-FEECD-O3(2)  TO D01-A15
                 MOVE BR-ADDON-FEECD-O3(2)   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-IMMEDIATE-RECEIPT NOT = OBR-IMMEDIATE-RECEIPT
              MOVE "BR-IMMEDIATE-RECEIPT" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-IMMEDIATE-RECEIPT  TO D01-A15
              MOVE BR-IMMEDIATE-RECEIPT   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-IMMEDIATE-DOC-PRINT NOT = OBR-IMMEDIATE-DOC-PRINT
              MOVE "BR-IMMEDIATE-DOC-PRINT" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-IMMEDIATE-DOC-PRINT  TO D01-A15
              MOVE BR-IMMEDIATE-DOC-PRINT   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-AUTO-PL-REF-AC NOT = OBR-AUTO-PL-REF-AC
              MOVE "BR-AUTO-PL-REF-AC" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-AUTO-PL-REF-AC  TO D01-A15
              MOVE BR-AUTO-PL-REF-AC   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OBR-FAX-NUMBER NOT NUMERIC
              MOVE 0 TO OBR-FAX-NUMBER.
           IF OBR-FAX-NUMBER NOT = BR-FAX-NUMBER
              MOVE "BR-FAX-NUMBER" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-FAX-NUMBER  TO D01-A15-NUM
              MOVE BR-FAX-NUMBER   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-ALPHA-DRAWERS NOT = OBR-ALPHA-DRAWERS
              MOVE "BR-ALPHA-DRAWERS" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-ALPHA-DRAWERS  TO D01-A15
              MOVE BR-ALPHA-DRAWERS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-LATECHG-ON-ALLOTMENTS NOT = OBR-LATECHG-ON-ALLOTMENTS
              MOVE "BR-LATECHG-ON-ALLOTMENTS" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-LATECHG-ON-ALLOTMENTS  TO D01-A15
              MOVE BR-LATECHG-ON-ALLOTMENTS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-ALLOW-SYS-AS-EFFDATE NOT = OBR-ALLOW-SYS-AS-EFFDATE
              MOVE "BR-ALLOW-SYS-AS-EFFDATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-ALLOW-SYS-AS-EFFDATE  TO D01-A15
              MOVE BR-ALLOW-SYS-AS-EFFDATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OBR-TAX-PREBOOK-CUST NOT NUMERIC
              MOVE 0 TO OBR-TAX-PREBOOK-CUST.
           IF OBR-TAX-PREBOOK-CUST NOT = BR-TAX-PREBOOK-CUST
              MOVE "BR-TAX-PREBOOK-CUST" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-TAX-PREBOOK-CUST  TO D01-A15-NUM
              MOVE BR-TAX-PREBOOK-CUST   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OBR-TAX-PREBOOK-NONCUST NOT NUMERIC
              MOVE 0 TO OBR-TAX-PREBOOK-NONCUST.
           IF OBR-TAX-PREBOOK-NONCUST NOT = BR-TAX-PREBOOK-NONCUST
              MOVE "BR-TAX-PREBOOK-NONCUST" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-TAX-PREBOOK-NONCUST  TO D01-A15-NUM
              MOVE BR-TAX-PREBOOK-NONCUST   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-PASSWORD-DRAWER NOT = OBR-PASSWORD-DRAWER
              MOVE "BR-PASSWORD-DRAWER" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-PASSWORD-DRAWER  TO D01-A15
              MOVE BR-PASSWORD-DRAWER   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-ONESTEP-RN-FG NOT = OBR-ONESTEP-RN-FG
              MOVE "BR-ONESTEP-RN-FG" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-ONESTEP-RN-FG  TO D01-A15
              MOVE BR-ONESTEP-RN-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-LI-LISCAN-FG NOT = OBR-LI-LISCAN-FG
              MOVE "BR-LI-LISCAN-FG" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-LI-LISCAN-FG  TO D01-A15
              MOVE BR-LI-LISCAN-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-HIDE-END-BAL NOT = OBR-HIDE-END-BAL
              MOVE "BR-HIDE-END-BAL" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-HIDE-END-BAL  TO D01-A15
              MOVE BR-HIDE-END-BAL   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-LBOX-NOPAYOFF NOT = OBR-LBOX-NOPAYOFF
              MOVE "BR-LBOX-NOPAYOFF" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-LBOX-NOPAYOFF  TO D01-A15
              MOVE BR-LBOX-NOPAYOFF   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-LBOX-ODDPAY NOT = OBR-LBOX-ODDPAY
              MOVE "BR-LBOX-ODDPAY" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-LBOX-ODDPAY  TO D01-A15
              MOVE BR-LBOX-ODDPAY   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-LBOX-LC NOT = OBR-LBOX-LC
              MOVE "BR-LBOX-LC" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-LBOX-LC  TO D01-A15
              MOVE BR-LBOX-LC   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-LBOX-NOPRIN NOT = OBR-LBOX-NOPRIN
              MOVE "BR-LBOX-NOPRIN" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-LBOX-NOPRIN  TO D01-A15
              MOVE BR-LBOX-NOPRIN   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-LBOX-FLASH NOT = OBR-LBOX-FLASH
              MOVE "BR-LBOX-FLASH" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-LBOX-FLASH  TO D01-A15
              MOVE BR-LBOX-FLASH   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-LBOX-ACTIONCD NOT = OBR-LBOX-ACTIONCD
              MOVE "BR-LBOX-ACTIONCD" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-LBOX-ACTIONCD  TO D01-A15
              MOVE BR-LBOX-ACTIONCD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OBR-LBOX-MAXCON NOT NUMERIC
              MOVE 0 TO OBR-LBOX-MAXCON.
           IF OBR-LBOX-MAXCON NOT = BR-LBOX-MAXCON
              MOVE "BR-LBOX-MAXCON" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-LBOX-MAXCON  TO D01-A15-NUM
              MOVE BR-LBOX-MAXCON   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OBR-LBOX-MAXREC NOT NUMERIC
              MOVE 0 TO OBR-LBOX-MAXREC.
           IF OBR-LBOX-MAXREC NOT = BR-LBOX-MAXREC
              MOVE "BR-LBOX-MAXREC" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-LBOX-MAXREC  TO D01-A15-NUM
              MOVE BR-LBOX-MAXREC   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-LBOX-PL NOT = OBR-LBOX-PL
              MOVE "BR-LBOX-PL" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-LBOX-PL  TO D01-A15
              MOVE BR-LBOX-PL   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-LBOX-JD NOT = OBR-LBOX-JD
              MOVE "BR-LBOX-JD" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-LBOX-JD  TO D01-A15
              MOVE BR-LBOX-JD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-LBOX NOT = OBR-LBOX
              MOVE "BR-LBOX" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-LBOX  TO D01-A15
              MOVE BR-LBOX   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-LATECHG-ON-LBOX NOT = OBR-LATECHG-ON-LBOX
              MOVE "BR-LATECHG-ON-LBOX" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-LATECHG-ON-LBOX  TO D01-A15
              MOVE BR-LATECHG-ON-LBOX   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-ALLOW-OV-TRCD NOT = OBR-ALLOW-OV-TRCD
              MOVE "BR-ALLOW-OV-TRCD" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-ALLOW-OV-TRCD  TO D01-A15
              MOVE BR-ALLOW-OV-TRCD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-GLOBAL-QUEUE NOT = OBR-GLOBAL-QUEUE
              MOVE "BR-GLOBAL-QUEUE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-GLOBAL-QUEUE  TO D01-A15
              MOVE BR-GLOBAL-QUEUE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-LBOX-PAYOFF-NONCASH NOT = OBR-LBOX-PAYOFF-NONCASH
              MOVE "BR-LBOX-PAYOFF-NONCASH" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-LBOX-PAYOFF-NONCASH  TO D01-A15
              MOVE BR-LBOX-PAYOFF-NONCASH   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-LBOX-PAYOFF-OV NOT = OBR-LBOX-PAYOFF-OV
              MOVE "BR-LBOX-PAYOFF-OV" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OBR-LBOX-PAYOFF-OV  TO D01-A15
              MOVE BR-LBOX-PAYOFF-OV   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BR-COLLECT-MANAGER NOT = OB2-COLLECT-MANAGER
      *       CALL "C$JUSTIFY" USING OB2-COLLECT-MANAGER, "L"
              IF BR-COLLECT-MANAGER NOT = OB2-COLLECT-MANAGER
                 MOVE "BR-COLLECT-MANAGER" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE OB2-COLLECT-MANAGER  TO D01-A15
                 MOVE BR-COLLECT-MANAGER   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OB2-COLLECT-PHONE NOT NUMERIC
              MOVE 0 TO OB2-COLLECT-PHONE.
           IF OB2-COLLECT-PHONE NOT = BR-COLLECT-PHONE
              MOVE "BR-COLLECT-PHONE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OB2-COLLECT-PHONE  TO D01-A15-NUM
              MOVE BR-COLLECT-PHONE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OB2-COLLECT-EXT NOT NUMERIC
              MOVE 0 TO OB2-COLLECT-EXT.
           IF OB2-COLLECT-EXT NOT = BR-COLLECT-EXT
              MOVE "BR-COLLECT-EXT" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OB2-COLLECT-EXT  TO D01-A15-NUM
              MOVE BR-COLLECT-EXT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BR-ALT-INTO-THIS-BRANCH NOT = OB2-ALT-INTO-THIS-BRANCH
              MOVE "BR-ALT-INTO-THIS-BRANCH" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OB2-ALT-INTO-THIS-BRANCH  TO D01-A15
              MOVE BR-ALT-INTO-THIS-BRANCH   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-POFF-TITLE-WINDOW NOT = OB2-POFF-TITLE-WINDOW
              MOVE "BR-POFF-TITLE-WINDOW" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OB2-POFF-TITLE-WINDOW  TO D01-A15
              MOVE BR-POFF-TITLE-WINDOW   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-COUPON-STATEMENT NOT = OB2-COUPON-STATEMENT
              MOVE "BR-COUPON-STATEMENT" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OB2-COUPON-STATEMENT  TO D01-A15
              MOVE BR-COUPON-STATEMENT   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-SUPERVISOR NOT = OB2-SUPERVISOR
      *       CALL "C$JUSTIFY" USING OB2-SUPERVISOR, "L"
              IF BR-SUPERVISOR NOT = OB2-SUPERVISOR
                 MOVE "BR-SUPERVISOR" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE OB2-SUPERVISOR  TO D01-A15
                 MOVE BR-SUPERVISOR   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-AREA-MANAGER NOT = OB2-AREA-MANAGER
      *       CALL "C$JUSTIFY" USING OB2-AREA-MANAGER, "L"
              IF BR-AREA-MANAGER NOT = OB2-AREA-MANAGER
                 MOVE "BR-AREA-MANAGER" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE OB2-AREA-MANAGER  TO D01-A15
                 MOVE BR-AREA-MANAGER   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OB2-MIN-MONTHS-DEF NOT NUMERIC
              MOVE 0 TO OB2-MIN-MONTHS-DEF.
           IF OB2-MIN-MONTHS-DEF NOT = BR-MIN-MONTHS-DEF
              MOVE "BR-MIN-MONTHS-DEF" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OB2-MIN-MONTHS-DEF  TO D01-A15-NUM
              MOVE BR-MIN-MONTHS-DEF   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-CC-FLAG NOT = OB2-CC-FLAG
              MOVE "BR-CC-FLAG" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OB2-CC-FLAG  TO D01-A15
              MOVE BR-CC-FLAG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-CC-ADR1 NOT = OB2-CC-ADR1
      *       CALL "C$JUSTIFY" USING OB2-CC-ADR1, "L"
              IF BR-CC-ADR1 NOT = OB2-CC-ADR1
                 MOVE "BR-CC-ADR1" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE OB2-CC-ADR1  TO D01-A15
                 MOVE BR-CC-ADR1   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-CC-CITY NOT = OB2-CC-CITY
      *       CALL "C$JUSTIFY" USING OB2-CC-CITY, "L"
              IF BR-CC-CITY NOT = OB2-CC-CITY
                 MOVE "BR-CC-CITY" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE OB2-CC-CITY  TO D01-A15
                 MOVE BR-CC-CITY   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-CC-STATE NOT = OB2-CC-STATE
              MOVE "BR-CC-STATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OB2-CC-STATE  TO D01-A15
              MOVE BR-CC-STATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-CC-ZIP NOT = OB2-CC-ZIP
      *       CALL "C$JUSTIFY" USING OB2-CC-ZIP, "L"
              IF BR-CC-ZIP NOT = OB2-CC-ZIP
                 MOVE "BR-CC-ZIP" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE OB2-CC-ZIP  TO D01-A15
                 MOVE BR-CC-ZIP   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OB2-MIN-MONTHS-D2 NOT NUMERIC
              MOVE 0 TO OB2-MIN-MONTHS-D2.
           IF OB2-MIN-MONTHS-D2 NOT = BR-MIN-MONTHS-D2
              MOVE "BR-MIN-MONTHS-D2" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OB2-MIN-MONTHS-D2  TO D01-A15-NUM
              MOVE BR-MIN-MONTHS-D2   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-SKIP-MIDSTREAM-TRW NOT = OB2-SKIP-MIDSTREAM-TRW
              MOVE "BR-SKIP-MIDSTREAM-TRW" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OB2-SKIP-MIDSTREAM-TRW  TO D01-A15
              MOVE BR-SKIP-MIDSTREAM-TRW   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

      * NEED TO USE WS-NUM SINCE OB2-GL-ESCROW-BALANCE IS COMP-6.
           MOVE OB2-GL-ESCROW-BALANCE TO WS-NUM.
           IF WS-NUM NOT NUMERIC   OR
              WS-NUM = 20202020202 OR
              WS-NUM =  2020202020 OR
              WS-NUM =    20202020 
             MOVE 0 TO OB2-GL-ESCROW-BALANCE.
           IF OB2-GL-ESCROW-BALANCE NOT = BR-GL-ESCROW-BALANCE
              MOVE "BR-GL-ESCROW-BALANCE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OB2-GL-ESCROW-BALANCE  TO D01-A15-NUM
              MOVE BR-GL-ESCROW-BALANCE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

      * NEED TO USE WS-NUM SINCE OB2-GL-ESCROW-PAYABLE IS COMP-6.
           MOVE OB2-GL-ESCROW-PAYABLE TO WS-NUM.
           IF WS-NUM NOT NUMERIC   OR
              WS-NUM = 20202020202 OR
              WS-NUM =  2020202020 OR
              WS-NUM =    20202020 
             MOVE 0  TO OB2-GL-ESCROW-PAYABLE.
           IF OB2-GL-ESCROW-PAYABLE NOT = BR-GL-ESCROW-PAYABLE
              MOVE "BR-GL-ESCROW-PAYABLE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OB2-GL-ESCROW-PAYABLE  TO D01-A15-NUM
              MOVE BR-GL-ESCROW-PAYABLE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BR-ALLOW-FORCE-OPEN-FG NOT = OB2-ALLOW-FORCE-OPEN-FG
              MOVE "BR-ALLOW-FORCE-OPEN-FG" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OB2-ALLOW-FORCE-OPEN-FG  TO D01-A15
              MOVE BR-ALLOW-FORCE-OPEN-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OB2-ACH-BANK-NO NOT NUMERIC
              MOVE 0 TO OB2-ACH-BANK-NO.
           IF OB2-ACH-BANK-NO NOT = BR-ACH-BANK-NO
              MOVE "BR-ACH-BANK-NO" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OB2-ACH-BANK-NO  TO D01-A15-NUM
              MOVE BR-ACH-BANK-NO   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-WIMENU-CHECK-WRITING NOT = OB2-WIMENU-CHECK-WRITING
              MOVE "BR-WIMENU-CHECK-WRITING" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OB2-WIMENU-CHECK-WRITING  TO D01-A15
              MOVE BR-WIMENU-CHECK-WRITING   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-ADP-PAY-GROUP NOT = OB2-ADP-PAY-GROUP
      *       CALL "C$JUSTIFY" USING OB2-ADP-PAY-GROUP, "L"
              IF BR-ADP-PAY-GROUP NOT = OB2-ADP-PAY-GROUP
                 MOVE "BR-ADP-PAY-GROUP" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE OB2-ADP-PAY-GROUP  TO D01-A15
                 MOVE BR-ADP-PAY-GROUP   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-ALLOW-SCANNED-CKS NOT = OB2-ALLOW-SCANNED-CKS
              MOVE "BR-ALLOW-SCANNED-CKS" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OB2-ALLOW-SCANNED-CKS  TO D01-A15
              MOVE BR-ALLOW-SCANNED-CKS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OB2-CHECK-DEPOSIT-GL NOT NUMERIC
              MOVE 0 TO OB2-CHECK-DEPOSIT-GL.
           IF OB2-CHECK-DEPOSIT-GL NOT = BR-CHECK-DEPOSIT-GL
              MOVE "BR-CHECK-DEPOSIT-GL" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OB2-CHECK-DEPOSIT-GL  TO D01-A15-NUM
              MOVE BR-CHECK-DEPOSIT-GL   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-OPTION-PST-LASER NOT = OB2-OPTION-PST-LASER
              MOVE "BR-OPTION-PST-LASER" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OB2-OPTION-PST-LASER  TO D01-A15
              MOVE BR-OPTION-PST-LASER   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-USE-DEBIT-CARDS NOT = OB2-USE-DEBIT-CARDS
              MOVE "BR-USE-DEBIT-CARDS" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OB2-USE-DEBIT-CARDS  TO D01-A15
              MOVE BR-USE-DEBIT-CARDS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OB2-GL-DEBIT-CARD-PROCEEDS NOT NUMERIC
              MOVE 0 TO OB2-GL-DEBIT-CARD-PROCEEDS.
           IF OB2-GL-DEBIT-CARD-PROCEEDS NOT = BR-GL-DEBIT-CARD-PROCEEDS
              MOVE "BR-GL-DEBIT-CARD-PROCEEDS" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OB2-GL-DEBIT-CARD-PROCEEDS  TO D01-A15-NUM
              MOVE BR-GL-DEBIT-CARD-PROCEEDS   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

            IF OB2-WKDAY-BANKID NOT NUMERIC OR
             (OB2-WKDAY-BANKID = 2020 OR 0202)
               MOVE 0  TO OB2-WKDAY-BANKID.
           IF OB2-WKDAY-BANKID NOT = BR-WKDAY-BANKID
              MOVE "BR-WKDAY-BANKID" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OB2-WKDAY-BANKID  TO D01-A15-NUM
              MOVE BR-WKDAY-BANKID   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OB2-VERITEC-LICENSE NOT NUMERIC
              MOVE 0 TO OB2-VERITEC-LICENSE.
           IF OB2-VERITEC-LICENSE NOT = BR-VERITEC-LICENSE
              MOVE "BR-VERITEC-LICENSE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OB2-VERITEC-LICENSE  TO D01-A15-NUM
              MOVE BR-VERITEC-LICENSE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-TEST-PP-AUTO-INSCOVR NOT = OB2-TEST-PP-AUTO-INSCOVR
              MOVE "BR-TEST-PP-AUTO-INSCOVR" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OB2-TEST-PP-AUTO-INSCOVR  TO D01-A15
              MOVE BR-TEST-PP-AUTO-INSCOVR   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-USE-NEW-WFARGO-ACCT NOT = OB2-USE-NEW-WFARGO-ACCT
              MOVE "BR-USE-NEW-WFARGO-ACCT" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OB2-USE-NEW-WFARGO-ACCT  TO D01-A15
              MOVE BR-USE-NEW-WFARGO-ACCT   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-BRANCH-CLOSED NOT = OB2-BRANCH-CLOSED
              MOVE "BR-BRANCH-CLOSED" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OB2-BRANCH-CLOSED  TO D01-A15
              MOVE BR-BRANCH-CLOSED   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF BR-ALLOW-F3-ALL-DOCS NOT = OB2-ALLOW-F3-ALL-DOCS
              MOVE "BR-ALLOW-F3-ALL-DOCS" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OB2-ALLOW-F3-ALL-DOCS  TO D01-A15
              MOVE BR-ALLOW-F3-ALL-DOCS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

       EXIT-COMPARE-FIELDS.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE-ERROR SECTION.
      ******************************************************************

           ADD 1    TO TOTAL-ERRORS-CTR.
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

           IF ( GROUP-FLAG = "Y" )
              MOVE "BEGINNING BRANCH              : "  TO R01-PROMPT
              MOVE ENT-GROUP                          TO R01-TEXT
              MOVE 01                                 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH                 : "  TO R01-PROMPT
              MOVE SPACES                             TO R01-TEXT
              MOVE 01                                 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEGINNING BRANCH              : "  TO R01-PROMPT
              MOVE BEG-BRANCH                         TO R01-NUMERIC
              MOVE 01                                 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH                 : "  TO R01-PROMPT
              MOVE END-BRANCH                         TO R01-NUMERIC
              MOVE 01                                 TO LN-FEED
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
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/PROGID.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO BRFILE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BRFILE-SYSDATE.
           DISPLAY BRFILE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE BRFILE-FORM-FIELDS TO WR-BUF.
           MOVE BRFILE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE BRFILE-HELP--FILE TO HELP-LINK-FILE.
           MOVE BRFILE-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/BRFILE_EVA.CPY".
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
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

       COPY "LIBGB/GRBRLS_SQL.CPY".

       OPEN-OBR-FILE.
           PERFORM OPEN-IT.
           MOVE OBR-PATH TO E-FILE.
           OPEN INPUT OBR-FILE.
           IF IO-FG = 8
              GO TO OPEN-OBR-FILE.
           IF IO-FG = 7
              CLOSE OBR-FILE
              GO TO OPEN-OBR-FILE.
       READ-OBR-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OBR-PATH TO E-FILE.
           MOVE OBR-KEY TO E-KEYX.
           READ OBR-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OBR-FILE-NEXT.
       CLOSE-OBR-FILE.
           PERFORM CLOSE-IT.
           CLOSE OBR-FILE.

       OPEN-OB2-FILE.
           PERFORM OPEN-IT.
           MOVE OB2-PATH TO E-FILE.
           OPEN INPUT OB2-FILE.
           IF IO-FG = 8
             GO TO OPEN-OB2-FILE.
           IF IO-FG = 7
             CLOSE OB2-FILE
             GO TO OPEN-OB2-FILE.
       READ-OB2-FILE.
           PERFORM READ-IT.
           MOVE OB2-PATH TO E-FILE.
           READ OB2-FILE.
           IF IO-FG = 8
              GO TO READ-OB2-FILE.
       CLOSE-OB2-FILE.
           PERFORM CLOSE-IT.
           CLOSE OB2-FILE.

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
       REWRITE-WK1-FILE.
           PERFORM REWRITE-IT.
           MOVE WK-PATH TO E-FILE.
           MOVE WK-KEY  TO E-KEYX.
           REWRITE WK-REC.
           IF IO-FG = 8
             GO TO REWRITE-WK1-FILE.
           UNLOCK WK-FILE.
       CLOSE-WK1-FILE.
           PERFORM CLOSE-IT.
           MOVE 0 TO IO-FG IO-STATUS.
           CLOSE WK-FILE.

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
              DESTROY BRFILE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
