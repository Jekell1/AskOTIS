       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CPGFLE.
       DATE-WRITTEN.  11-15-2023.
      ******************************************************************
      *
      * DESCRIPTION   : COMPARE S35 CPFILE WITH A15 GLOBAL CPFILE
      *            
      * THIS IS THE GLOBAL COLLECTOR POOL
      * MB 2024-03-29 - CHANGED TO USE LOCAL FILES PER JAY
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
      * COPYMEMBER: LIBSP/SPFSCP
           SELECT OCP-FILE ASSIGN TO OCP-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OCP1-KEY
                  FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  OCP-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A15SRC/SP01CP.CPY" REPLACING LEADING "CP" BY "OCP".

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/CPGFLE S35 11/15/23-13:25:40 >".

       COPY "LIBSP/SP01CP.CPY".
       COPY "LIBSP/SP01CP_SQL.CPY".
       COPY "LIBSP/SPWSCP.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGB/GRSSPW.CPY".
       COPY "LIBGB/GRBRLSW.CPY".

       01  OCP-PATH                    PIC X(35) VALUE SPACES.
       01  OCP-ALL-PATH.
           03  OCP-ALL-BASE     PIC X(9).
           03  FILLER           PIC X     VALUE "/".
           03  OCP-ALL-MACHINE  PIC XX.
           03  FILLER           PIC X     VALUE "/".
           03  OCP-ALL-DATA     PIC X(6).
           03  FILLER           PIC X(10) VALUE
               "/SP/CPFILE".

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
       01  MACHINE-R1.
           03  FILLER                  PIC X VALUE X"72".
           03  FILLER                  PIC 9 VALUE 1.
       01  MACHINE-R2.
           03  FILLER                  PIC X VALUE X"72".
           03  FILLER                  PIC 9 VALUE 2.

       01  CP-EOF-SW                   PIC X      VALUE "N".
       01  OCP-EOF-SW                  PIC X      VALUE "N".
       01  A15-FILE-FOUND-SW           PIC X      VALUE "N".
       01  PROC-SUB                    PIC 9999   VALUE 0.
       01  TOTAL-ERRORS-CTR            PIC 9(9)   VALUE 0.

       01  USE-OCP-KEY.
           03  USE-OCP-KEY-BRNO        PIC 9(4).
           03  USE-OCP-KEY-ID          PIC X(3).

       01  PROCESS-STAT                PIC X(08)    VALUE SPACES.
           88  PROCESS-STAT-GOOD                    VALUE "00".
           88  PROCESS-STAT-BAD                     VALUE "  " THRU "//"
                                                         "01" THRU "~~".

       01  SQL-RECORDS-PROCESSED       PIC 9(9)     VALUE 0.
       01  A15-RECORDS-PROCESSED       PIC 9(9)     VALUE 0.
           88  NO-RECORDS-FOUND                     VALUE ZEROES.

       01  STARTING-TIME               PIC X(08)    VALUE SPACES.
       01  SUB                         PIC 9(04)    VALUE ZEROES.
       01  S1                          PIC 9(04)    VALUE ZEROES.
       01  S2                          PIC 9(04)    VALUE ZEROES.
       01  S3                          PIC 9(04)    VALUE ZEROES.

       01  SYSDATE-YYYYMMDD            PIC 9(8)     VALUE ZEROES.

       01  LEGEND                      PIC X(79)    VALUE SPACES.
       01  LEGEND-LIST                 PIC X(07)    VALUE "LEGEND.".

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
           03  BEG-CLASS               PIC 9(02).
           03  END-CLASS               PIC 9(02).
           03  BEG-DATE                PIC 9(06).
           03  END-DATE                PIC 9(06).
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
           03  H02-TITLE               PIC X(40) VALUE 
           "GLOBAL CPFILE COMPARE A15 TO S35".

       01  HEADER-LINE-03.
           03  FILLER                PIC X(4) VALUE "BRNO".
           03  FILLER                PIC X(2) VALUE SPACES.
           03  FILLER                PIC X(2) VALUE "ID".
           03  FILLER                PIC X(2) VALUE SPACES.
           03  FILLER                PIC X(11) VALUE "DESCRIPTION".
           03  FILLER                PIC X(17) VALUE SPACES.
           03  FILLER                PIC X(3) VALUE "A15".
           03  FILLER                PIC X(19) VALUE SPACES.
           03  FILLER                PIC X(3) VALUE "S35".

       01  DETAIL-LINE                        PIC X(80)   VALUE SPACES.
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-BRNO                       PIC Z(3)9.
           03  FILLER                         PIC X.
           03  D01-ID                         PIC X(3).
           03  FILLER                         PIC X.
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
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
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
       COPY "LIBUT/CPGFLE_DEF.CPY".
       COPY "LIBUT/CPGFLE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/CPGFLE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE OCP-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-CP1-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE OCP-FILE.
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
           MOVE "CPGFLE" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE CPGFLE-QUEUE-POS TO Q-POS.
           MOVE CPGFLE-COPIES-POS TO C-POS.
           MOVE CPGFLE-PRU-POS   TO P-POS.
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
              MOVE EXT-EOBUF-BRNO          TO BEG-BRANCH END-BRANCH
              MOVE "N"                     TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH      TO PRU-FILE
           ELSE
           IF ( BEG-BRANCH = ZEROES ) AND
              ( END-BRANCH = ZEROES )
              MOVE BEG-BRANCH              TO SV-BEG-BRANCH
              MOVE END-BRANCH              TO SV-END-BRANCH
              MOVE EXT-CONAME-USER-LOC     TO BEG-BRANCH
                                              END-BRANCH.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-CP1-FILE.

           PERFORM START-MISC-CSR.
           PERFORM READ-MISC-CSR-NEXT.
           PERFORM CLOSE-MISC-CSR.

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

           MOVE BR-NO TO BTRACK-BRNO.
           PERFORM BTRACK-LOCAL.

           MOVE 1 TO PROC-SUB.
           PERFORM PROCESS-BRANCH-DRIVER
              UNTIL PROC-SUB > BGLS-BRANCHES-RETR-CTR.

       EXECUTION-MODULE-EXIT.
           EXIT.

      *********************************************************
       PROCESS-BRANCH-DRIVER SECTION.
      *********************************************************
      D    STOP MESS.
 
           MOVE 9999                       TO PROGID-SLEEP-COUNTER.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE EXT-FILPATH-MACHINE        TO FILPATH-MACHINE.
           MOVE EXT-FILPATH-DATA           TO FILPATH-DATA.

           MOVE "B"                        TO CP-PATH-OVERRIDE.

           MOVE BGLS-BRANCH-BRNO(PROC-SUB) TO BR-NO.
           PERFORM READ-BR-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           PERFORM SEND-PROGRESS-INDICATOR.

      * THIS IS THE GLOBAL COLLECTOR POOL
      * MB 2024-03-29 - CHANGED TO USE LOCAL FILES PER JAY
      *    MOVE SOURCE-PATH           TO OCP-ALL-BASE.
      *    MOVE EXT-FILPATH-MACHINE   TO OCP-ALL-MACHINE.
      *    MOVE OCP-ALL-PATH          TO OCP-PATH ACCESS-BUF.
      *    PERFORM ACCESS-CALL.
      *    IF ( STAT-BAD )
      *       IF OCP-ALL-MACHINE = MACHINE-R1
      *          MOVE MACHINE-R2   TO OCP-ALL-MACHINE
      *          MOVE OCP-ALL-PATH TO OCP-PATH ACCESS-BUF
      *          PERFORM ACCESS-CALL
      *          IF ( STAT-BAD )
      *             MOVE "MISSING A15 FILE" TO D01-TEXT
      *             PERFORM WRITE-DETAIL-LINE 
      *             GO TO PROCESS-BRANCH-DRIVER-END.

           MOVE "N" TO CP-EOF-SW OCP-EOF-SW.

           MOVE BR-NO   TO QCP1-WBEG-BRNO.
           MOVE SPACES  TO QCP1-WBEG-ID.
           MOVE ALL 'Z' TO QCP1-WEND-ID.

           PERFORM OPEN-CP1-FILE.
           PERFORM START-CP1-FILE.  *> EMBEDDED CPFILE
           PERFORM M-READ-CP.

           MOVE "Y"            TO A15-FILE-FOUND-SW.
           MOVE SOURCE-PATH    TO OCP-ALL-BASE.
           MOVE MACHINE-R1     TO OCP-ALL-MACHINE.
           MOVE BR-DATA-PATH   TO OCP-ALL-DATA.
           MOVE OCP-ALL-PATH   TO OCP-PATH ACCESS-BUF.

           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE MACHINE-R2    TO OCP-ALL-MACHINE
              MOVE OCP-ALL-PATH  TO OCP-PATH ACCESS-BUF
              PERFORM ACCESS-CALL
              IF ( STAT-BAD )
                 MOVE "N" TO A15-FILE-FOUND-SW.

           IF A15-FILE-FOUND-SW = "N"
              MOVE ALL '9'  TO USE-OCP-KEY
              MOVE "Y"      TO OCP-EOF-SW
           ELSE
              PERFORM OPEN-OCP1-FILE
              PERFORM M-READ-OCP.

           PERFORM PROCESS-BRANCH-NEXT-CP
              UNTIL CP-EOF-SW = "Y" AND OCP-EOF-SW = "Y".

           PERFORM CLOSE-CP1-FILE.
           PERFORM CLOSE-OCP1-FILE.

           GO TO PROCESS-BRANCH-DRIVER-END.

       PROCESS-BRANCH-NEXT-CP.
           PERFORM SEND-PROGRESS-INDICATOR.

           PERFORM COMPARE-FIELDS.

            IF USE-OCP-KEY < CP1-KEY
              PERFORM M-READ-OCP
           ELSE
              IF USE-OCP-KEY > CP1-KEY
                 PERFORM M-READ-CP
              ELSE
                 PERFORM M-READ-OCP
                 PERFORM M-READ-CP.

       PROCESS-BRANCH-DRIVER-END.
           ADD 1 TO PROC-SUB.

       PROCESS-BRANCH-DRIVER-EXIT.
           EXIT.

       M-READ-OCP SECTION.
           PERFORM READ-OCP1-FILE-NEXT.
           IF IO-BAD
              MOVE ALL '9'  TO USE-OCP-KEY
              MOVE "Y"      TO OCP-EOF-SW
           ELSE
              MOVE BR-NO  TO USE-OCP-KEY-BRNO
              MOVE OCP-ID TO USE-OCP-KEY-ID
              ADD 1       TO A15-RECORDS-PROCESSED.

       EXIT-M-READ-OCP.
           EXIT.

       M-READ-CP SECTION.
           PERFORM READ-CP1-FILE-NEXT.   *> EMBEDDED  CPFILE
           IF IO-BAD
              MOVE ALL 'Z' TO CP1-KEY
              MOVE "Y"     TO CP-EOF-SW
           ELSE
              ADD 1 TO SQL-RECORDS-PROCESSED.

       EXIT-M-READ-CP.
           EXIT.

      ******************************************************************
       COMPARE-FIELDS SECTION.
      ******************************************************************

           IF USE-OCP-KEY < CP1-KEY
              MOVE "MISSING S35 RECORD" TO D01-TEXT
              MOVE BR-NO                TO D01-BRNO
              MOVE OCP-ID               TO D01-ID
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.

           IF USE-OCP-KEY > CP1-KEY
              MOVE "MISSING A15 RECORD" TO D01-TEXT
              MOVE BR-NO                TO D01-BRNO
              MOVE CP-ID                TO D01-ID
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.

           IF OCP-DESC NOT = CP-DESC
              MOVE "CP-DESC" TO D01-TEXT
              MOVE CP-BRNO   TO D01-BRNO
              MOVE CP-ID     TO D01-ID
              MOVE OCP-DESC  TO D01-A15
              MOVE CP-DESC   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 80
             IF OCP-COL-ID(SUB) NOT = CP-COL-ID(SUB)
                MOVE "CP-COL-ID(SUB)" TO D01-TEXT
                MOVE CP-BRNO          TO D01-BRNO
                MOVE CP-ID            TO D01-ID
                MOVE OCP-COL-ID(SUB)  TO D01-A15
                MOVE CP-COL-ID(SUB)   TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR
             END-IF
           END-PERFORM.

           IF OCP-ALPHA-FLAG NOT = CP-ALPHA-FLAG
              MOVE "CP-ALPHA-FLAG" TO D01-TEXT
              MOVE CP-BRNO         TO D01-BRNO
              MOVE CP-ID           TO D01-ID
              MOVE OCP-ALPHA-FLAG  TO D01-A15
              MOVE CP-ALPHA-FLAG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OCP-TOTAL-ACCOUNTS NOT NUMERIC
              MOVE 0 TO OCP-TOTAL-ACCOUNTS.
           IF OCP-TOTAL-ACCOUNTS NOT = CP-TOTAL-ACCOUNTS
              MOVE "CP-TOTAL-ACCOUNTS" TO D01-TEXT
              MOVE CP-BRNO             TO D01-BRNO
              MOVE CP-ID               TO D01-ID
              MOVE OCP-TOTAL-ACCOUNTS  TO D01-A15
              MOVE CP-TOTAL-ACCOUNTS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OCP-COLLECTOR-COUNT NOT NUMERIC
              MOVE 0 TO OCP-COLLECTOR-COUNT.
           IF OCP-COLLECTOR-COUNT NOT = CP-COLLECTOR-COUNT
              MOVE "CP-COLLECTOR-COUNT" TO D01-TEXT
              MOVE CP-BRNO              TO D01-BRNO
              MOVE CP-ID                TO D01-ID
              MOVE OCP-COLLECTOR-COUNT  TO D01-A15
              MOVE CP-COLLECTOR-COUNT   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OCP-LAST-SUBSCRIPT NOT NUMERIC
              MOVE 0 TO OCP-LAST-SUBSCRIPT.
           IF OCP-LAST-SUBSCRIPT NOT = CP-LAST-SUBSCRIPT
              MOVE "CP-LAST-SUBSCRIPT" TO D01-TEXT
              MOVE CP-BRNO             TO D01-BRNO
              MOVE CP-ID               TO D01-ID
              MOVE OCP-LAST-SUBSCRIPT  TO D01-A15
              MOVE CP-LAST-SUBSCRIPT   TO D01-S35
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
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO CPGFLE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CPGFLE-SYSDATE.
           DISPLAY CPGFLE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE CPGFLE-FORM-FIELDS TO WR-BUF.
           MOVE CPGFLE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE CPGFLE-HELP--FILE TO HELP-LINK-FILE.
           MOVE CPGFLE-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/CPGFLE_EVA.CPY".
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

           MOVE PROGID-TEXT-PROCESSING     TO PROGID-TEXT.
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
       COPY "LIBSP/SPCPGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBSP/SPCP1RN.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GRBRLS_SQL.CPY".

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
       OPEN-OCP1-FILE.
           PERFORM OPEN-IT.
           MOVE OCP-PATH TO E-FILE.
           OPEN INPUT OCP-FILE.
           IF IO-FG = 8
              GO TO OPEN-OCP1-FILE.
           IF IO-FG = 7
              CLOSE OCP-FILE
              GO TO OPEN-OCP1-FILE.
       START-OCP1-FILE.
           PERFORM START-IT.
           MOVE OCP-PATH TO E-FILE.
           MOVE OCP1-KEY TO E-KEYX.
           START OCP-FILE KEY NOT < OCP1-KEY.
       READ-OCP1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OCP-PATH TO E-FILE.
           MOVE OCP1-KEY TO E-KEYX.
           READ OCP-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OCP1-FILE-NEXT.
       CLOSE-OCP1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OCP-FILE.

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

           IF ( SV-BEG-BRANCH NOT = 9999 )
              IF ( EXT-EOBUF-GROUP-FG = "Y" )
                 MOVE SV-BEG-BRANCH        TO BEG-BRANCH
                 MOVE SV-END-BRANCH        TO END-BRANCH
                 MOVE SV-GROUP-FLAG        TO GROUP-FLAG
                 MOVE SV-ENT-GROUP         TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BRANCH        TO BEG-BRANCH
                 MOVE SV-END-BRANCH        TO END-BRANCH.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY CPGFLE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
