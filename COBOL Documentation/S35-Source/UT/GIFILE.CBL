       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GIFILE.
       DATE-WRITTEN.  02-07-2024.
       AUTHOR. MARK BOOMGAARD.
      ******************************************************************
      *
      * DESCRIPTION   : COMPARE S35 GIFILE WITH A15 GIFILE
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
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

      * COPYMEMBER: LIBGL/GLFSGI
           SELECT OGI-FILE ASSIGN TO OGI-PATH
                  ORGANIZATION INDEXED
                  ACCESS MODE DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OGI1-KEY
                  FILE STATUS FILE-STAT.

           SELECT OBR-FILE ASSIGN TO OBR-PATH
                  ORGANIZATION RELATIVE
                  ACCESS MODE DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RELATIVE KEY OBR-KEY
                  FILE STATUS FILE-STAT.

           SELECT WK-FILE ASSIGN TO WK-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK1-KEY
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  OGI-FILE
           LABEL RECORDS ARE STANDARD.
      *-----------------------------------------------------------------
       COPY "LIBUP/A15SRC/GL01GI.CPY" REPLACING LEADING "GI" BY "OGI".
      *-----------------------------------------------------------------

      * COPYMEMBER: LIBGB/GBFDBR
      *********************************************************
      *    BRANCH FILE RECORD    GBFDBR    798
      *
      **********************************************************
       FD  OBR-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A15SRC/GB01BR.CPY" REPLACING LEADING "BR" BY "OBR".


       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A15SRC/GL01GI.CPY" REPLACING LEADING "GI" BY "WK".

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/GIFILE S35 02/08/24-08:33:27 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGL/GL01GI.CPY".
       COPY "LIBGL/GL01GI_SQL.CPY".
       COPY "LIBGL/GLWSGI.CPY".

       01  OGI-PATH                    PIC X(35) VALUE SPACES.
       01  OGI-ALL-PATH.
           03  OGI-ALL-BASE            PIC X(9).
           03  FILLER                  PIC X     VALUE "/".
           03  OGI-ALL-MACHINE         PIC XX.
           03  FILLER                  PIC X(10) VALUE
               "/GL/GIFILE".

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
       01  GI-EOF-SW                   PIC X      VALUE "N".
       01  OGI-EOF-SW                  PIC X      VALUE "N".
       01  PREV-BRANCH-ERR-SW          PIC X      VALUE "N".
       01  TOTAL-ERRORS-CTR            PIC 9(9)   VALUE 0.
       01  PRINTING-RANGES-SW          PIC X      VALUE "N".
       01  SUB1                        PIC 9      VALUE 0.

       01  OBR-KEY       PIC 9(4)   VALUE 0.
       01  OBR-PATH. 
           03  OBR-ALL-BASE      PIC X(9).
           03  FILLER            PIC X     VALUE "/".
           03  OBR-ALL-MACHINE   PIC XX.
           03  FILLER            PIC X(10) VALUE
               "/GB/BRFILE".

       01  R12-ARRAY                   OCCURS 9999 TIMES  
                                       PIC XX.

       01  WK1-USE-KEY.
           03 WK1-USE-KEY-BRNO         PIC 9(4).
           03 WK1-USE-KEY-CLASS        PIC 9(3).

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
           "G/L GENERAL INTERFACE (GIFILE) COMPARE A15 TO S35".

       01  HEADER-LINE-03.
           03  FILLER                PIC X(4)  VALUE "BRNO".
           03  FILLER                PIC X(2)  VALUE SPACES.
           03  FILLER                PIC X(5)  VALUE "CLASS".
           03  FILLER                PIC X(3)  VALUE SPACES.
           03  FILLER                PIC X(22) VALUE "DESCRIPTION".
           03  FILLER                PIC X(27) VALUE "A15".
           03  FILLER                PIC X(3)  VALUE "S35".

      ******************************************************************
      *
      *    D E T A I L - L I N E - W O R K E R S
      *
      ******************************************************************
       01  DETAIL-LINE                        PIC X(120)   VALUE SPACES.

       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-BRNO                       PIC Z(3)9.
           03  FILLER                         PIC X(3).
           03  D01-CLASS                      PIC 9(3).
           03  FILLER                         PIC X(4).
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
       COPY "LIBUT/GIFILE_DEF.CPY".
       COPY "LIBUT/GIFILE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/GIFILE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE OGI-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-GI1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           PERFORM CLOSE-WK1-FILE.
           CLOSE OGI-FILE WK-FILE.
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
           MOVE "GIFILE" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE GIFILE-QUEUE-POS  TO Q-POS.
           MOVE GIFILE-COPIES-POS TO C-POS.
           MOVE GIFILE-PRU-POS    TO P-POS.
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
              MOVE EXT-EOBUF-GROUP-PRPATH  TO PRU-FILE
           ELSE
           IF ( BEG-BRANCH = ZEROES ) AND
              ( END-BRANCH = ZEROES )
              MOVE BEG-BRANCH              TO SV-BEG-BRANCH
              MOVE END-BRANCH              TO SV-END-BRANCH
              MOVE EXT-CONAME-USER-LOC     TO BEG-BRANCH
                                              END-BRANCH.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WK-PATH.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-PR-FILE.

           MOVE SOURCE-PATH   TO OBR-ALL-BASE.
           MOVE MACHINE-R1    TO OBR-ALL-MACHINE.
           PERFORM OPEN-OBR-FILE.
           PERFORM READ-OBR-FILE-NEXT.
           PERFORM UNTIL IO-BAD
              MOVE OBR-MACHINE TO R12-ARRAY(OBR-NO)
              PERFORM READ-OBR-FILE-NEXT
           END-PERFORM.
           PERFORM CLOSE-OBR-FILE.

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

           PERFORM LOAD-WORK-FILE.
           IF A15-RECORDS-PROCESSED = 0
              MOVE "MISSING A15 FILE" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXECUTION-MODULE-EXIT.

           PERFORM OPEN-WK1-FILE.

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

           MOVE "B"                        TO GI-PATH-OVERRIDE.

           MOVE BR-NO  TO QGI1-WBEG-BRANCH 
                          QGI1-WEND-BRANCH
                          WK-BRANCH.

           MOVE ZEROS   TO QGI1-WBEG-CLASS
                           WK-CLASS. 
           MOVE ALL '9' TO QGI1-WEND-CLASS.

           MOVE "PROCESSING S35 DATA" TO BTRACK-MESS.
           PERFORM BTRACK-GLOBAL.

           MOVE "N" TO GI-EOF-SW.
           PERFORM OPEN-GI1-FILE.
           PERFORM START-GI1-FILE.
           PERFORM M-READ-GI.

           PERFORM START-WK1-FILE.
           PERFORM M-READ-WK1.

           PERFORM PROCESS-BRANCH-NEXT-GI
              UNTIL GI-EOF-SW = "Y" AND WK1-USE-KEY-BRNO > BR-NO.

           PERFORM CLOSE-GI1-FILE.

           GO TO PROCESS-BRANCH-DRIVER-END.

       PROCESS-BRANCH-NEXT-GI.

           PERFORM SEND-PROGRESS-INDICATOR.

           PERFORM COMPARE-FIELDS.

           IF WK1-USE-KEY < GI1-KEY
              PERFORM M-READ-WK1
           ELSE
              IF WK1-USE-KEY > GI1-KEY
                 PERFORM M-READ-GI
              ELSE
                 PERFORM M-READ-WK1
                 PERFORM M-READ-GI.

       PROCESS-BRANCH-DRIVER-END.
           EXIT. 

       LOAD-WORK-FILE SECTION.
      *** MUST LOAD R1 AND R2 GIFILE INTO WK1 - REWRITE IF R2 REC ALREADY LOADED IF R2 BRANCH***

           PERFORM OPEN-WK1-FILE-OUTPUT.
           PERFORM CLOSE-WK1-FILE.
      * OPENS FOR I/O - REWRITE NEEDS FILE OPENED FOR I/O

           MOVE "PROCESSING A15 DATA" TO BTRACK-MESS.  
           PERFORM BTRACK-GLOBAL.

           PERFORM OPEN-WK1-FILE.

           MOVE SOURCE-PATH   TO OGI-ALL-BASE.
           MOVE MACHINE-R1    TO OGI-ALL-MACHINE.
           MOVE OGI-ALL-PATH  TO OGI-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT-GOOD
              MOVE "N" TO OGI-EOF-SW
              PERFORM OPEN-OGI1-FILE
              PERFORM LOAD-WORK-FILE-CONT
              PERFORM CLOSE-OGI1-FILE.

           MOVE MACHINE-R2   TO OGI-ALL-MACHINE.
           MOVE OGI-ALL-PATH TO OGI-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT-GOOD
              MOVE "N" TO OGI-EOF-SW
              PERFORM OPEN-OGI1-FILE
              PERFORM LOAD-WORK-FILE-CONT
              PERFORM CLOSE-OGI1-FILE.

           PERFORM CLOSE-WK1-FILE.

       LOAD-WORK-FILE-END.
           EXIT.

       LOAD-WORK-FILE-CONT SECTION.
           PERFORM M-READ-OGI1.

           IF OGI-EOF-SW = "Y"
              GO TO LOAD-WORK-FILE-CONT-END.

           MOVE OGI-REC TO WK-REC.
           PERFORM WRITE-WK1-FILE.
           IF IO-STATUS = 9 
              IF R12-ARRAY(WK-BRANCH) = MACHINE-R2
                 PERFORM REWRITE-WK1-FILE
              END-IF
           ELSE
              ADD 1 TO A15-RECORDS-PROCESSED
           END-IF.

           GO TO LOAD-WORK-FILE-CONT.

       LOAD-WORK-FILE-CONT-END.
           EXIT. 

       M-READ-GI SECTION.
           PERFORM READ-GI1-FILE-NEXT.
           IF IO-BAD
              MOVE ALL '9' TO GI1-KEY
              MOVE "Y"     TO GI-EOF-SW
           ELSE
              ADD 1 TO SQL-RECORDS-PROCESSED.

       EXIT-M-READ-GI.
           EXIT.

       M-READ-OGI1 SECTION.
           PERFORM READ-OGI1-FILE-NEXT.
           IF IO-BAD
              MOVE "Y" TO OGI-EOF-SW
           ELSE
              ADD 1 TO A15-RECORDS-PROCESSED.
 
       EXIT-M-READ-OGI1.
           EXIT.

       M-READ-WK1 SECTION.
           PERFORM READ-WK1-FILE-NEXT.
           IF IO-BAD
              MOVE ALL '9'    TO WK1-USE-KEY
           ELSE
              MOVE WK-REC     TO OGI-REC
              MOVE OGI-BRANCH TO WK1-USE-KEY-BRNO
              MOVE OGI-CLASS  TO WK1-USE-KEY-CLASS.

       EXIT-M-READ-WK1.
           EXIT.

      ******************************************************************
       COMPARE-FIELDS SECTION.
      ******************************************************************

           IF WK1-USE-KEY < GI1-KEY
              MOVE "MISSING S35 RECORD" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE OGI-CLASS  TO D01-CLASS
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.

           IF WK1-USE-KEY > GI1-KEY
              MOVE "MISSING A15 RECORD" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.

           IF GI-CASH NOT = OGI-CASH
              MOVE "GI-CASH" TO D01-TEXT
              MOVE OGI-CASH  TO D01-A15
              MOVE GI-CASH   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-BANK NOT = OGI-BANK
              MOVE "GI-BANK"  TO D01-TEXT
              MOVE OGI-BANK   TO D01-A15
              MOVE GI-BANK    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-NSF NOT = OGI-NSF
              MOVE "GI-NSF" TO D01-TEXT
              MOVE OGI-NSF  TO D01-A15
              MOVE GI-NSF   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-AR NOT = OGI-AR
              MOVE "GI-AR" TO D01-TEXT
              MOVE OGI-AR  TO D01-A15
              MOVE GI-AR   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 5
              IF GI-PL(SUB1) NOT = OGI-PL(SUB1)
                 STRING "GI-PL(",SUB1,")" INTO D01-TEXT
                 MOVE OGI-PL(SUB1)  TO D01-A15 
                 MOVE GI-PL(SUB1)   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.
  
           IF GI-DLINSREB NOT = OGI-DLINSREB
              MOVE "GI-DLINSREB" TO D01-TEXT
              MOVE OGI-DLINSREB  TO D01-A15-NUM
              MOVE GI-DLINSREB   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-ACR-INT-AR-AR NOT = OGI-ACR-INT-AR-AR
              MOVE "GI-ACR-INT-AR-AR" TO D01-TEXT
              MOVE OGI-ACR-INT-AR-AR  TO D01-A15
              MOVE GI-ACR-INT-AR-AR   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-OTHBR-REC NOT = OGI-OTHBR-REC
              MOVE "GI-OTHBR-REC" TO D01-TEXT
              MOVE OGI-OTHBR-REC  TO D01-A15
              MOVE GI-OTHBR-REC   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-UNINTCHG NOT = OGI-UNINTCHG
              MOVE "GI-UNINTCHG" TO D01-TEXT
              MOVE OGI-UNINTCHG  TO D01-A15
              MOVE GI-UNINTCHG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-UNADDON NOT = OGI-UNADDON
              MOVE "GI-UNADDON" TO D01-TEXT
              MOVE OGI-UNADDON  TO D01-A15-NUM
              MOVE GI-UNADDON   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-UNSERCHG NOT = OGI-UNSERCHG
              MOVE "GI-UNSERCHG" TO D01-TEXT
              MOVE OGI-UNSERCHG  TO D01-A15
              MOVE GI-UNSERCHG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-UNMAINTFEE NOT = OGI-UNMAINTFEE
              MOVE "GI-UNMAINTFEE" TO D01-TEXT
              MOVE OGI-UNMAINTFEE  TO D01-A15
              MOVE GI-UNMAINTFEE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-UNDLDISC NOT = OGI-UNDLDISC
              MOVE "GI-UNDLDISC" TO D01-TEXT
              MOVE OGI-UNDLDISC  TO D01-A15
              MOVE GI-UNDLDISC   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-UNCL1 NOT = OGI-UNCL1
              MOVE "GI-UNCL1" TO D01-TEXT
              MOVE OGI-UNCL1  TO D01-A15
              MOVE GI-UNCL1   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-UNAH NOT = OGI-UNAH
              MOVE "GI-UNAH" TO D01-TEXT
              MOVE OGI-UNAH  TO D01-A15
              MOVE GI-UNAH   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-UNPP NOT = OGI-UNPP
              MOVE "GI-UNPP" TO D01-TEXT
              MOVE OGI-UNPP  TO D01-A15
              MOVE GI-UNPP   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-UNO1 NOT = OGI-UNO1
              MOVE "GI-UNO1" TO D01-TEXT
              MOVE OGI-UNO1  TO D01-A15
              MOVE GI-UNO1   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-UNO2 NOT = OGI-UNO2
              MOVE "GI-UNO2" TO D01-TEXT
              MOVE OGI-UNO2  TO D01-A15
              MOVE GI-UNO2   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-UNO3 NOT = OGI-UNO3
              MOVE "GI-UNO3" TO D01-TEXT
              MOVE OGI-UNO3  TO D01-A15
              MOVE GI-UNO3   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-UNO4 NOT = OGI-UNO4
              MOVE "GI-UNO4" TO D01-TEXT
              MOVE OGI-UNO4  TO D01-A15
              MOVE GI-UNO4   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-UNO5 NOT = OGI-UNO5
              MOVE "GI-UNO5" TO D01-TEXT
              MOVE OGI-UNO5  TO D01-A15
              MOVE GI-UNO5   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-UNINTREPO NOT = OGI-UNINTREPO
              MOVE "GI-UNINTREPO" TO D01-TEXT
              MOVE OGI-UNINTREPO  TO D01-A15
              MOVE GI-UNINTREPO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-UNADDONINTREPO NOT = OGI-UNADDONINTREPO
              MOVE "GI-UNADDONINTREPO" TO D01-TEXT
              MOVE OGI-UNADDONINTREPO  TO D01-A15
              MOVE GI-UNADDONINTREPO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 5
              IF GI-PLEXP(SUB1) NOT = OGI-PLEXP(SUB1)
                 STRING "GI-PLEXP(",SUB1,")" INTO D01-TEXT
                 MOVE OGI-PLEXP(SUB1)  TO D01-A15 
                 MOVE GI-PLEXP(SUB1)   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.

           IF GI-DLPARVEX NOT = OGI-DLPARVEX
              MOVE "GI-DLPARVEX" TO D01-TEXT
              MOVE OGI-DLPARVEX  TO D01-A15
              MOVE GI-DLPARVEX   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 5
              IF GI-PLOFFSET(SUB1) NOT = OGI-PLOFFSET(SUB1)
                 STRING "GI-PLOFFSET(",SUB1,")" INTO D01-TEXT
                 MOVE OGI-PLOFFSET(SUB1)  TO D01-A15 
                 MOVE GI-PLOFFSET(SUB1)   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.

           IF GI-SERVOFF NOT = OGI-SERVOFF
              MOVE "GI-SERVOFF" TO D01-TEXT
              MOVE OGI-SERVOFF  TO D01-A15
              MOVE GI-SERVOFF   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-PAYCL1 NOT = OGI-PAYCL1
              MOVE "GI-PAYCL1" TO D01-TEXT
              MOVE OGI-PAYCL1  TO D01-A15
              MOVE GI-PAYCL1   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-PAYCL2 NOT = OGI-PAYCL2
              MOVE "GI-PAYCL2" TO D01-TEXT
              MOVE OGI-PAYCL2  TO D01-A15
              MOVE GI-PAYCL2   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-PAYAH NOT = OGI-PAYAH
              MOVE "GI-PAYAH" TO D01-TEXT
              MOVE OGI-PAYAH  TO D01-A15
              MOVE GI-PAYAH   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-PAYPP NOT = OGI-PAYPP
              MOVE "GI-PAYPP" TO D01-TEXT
              MOVE OGI-PAYPP  TO D01-A15
              MOVE GI-PAYPP   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-PAYO1 NOT = OGI-PAYO1
              MOVE "GI-PAYO1" TO D01-TEXT
              MOVE OGI-PAYO1  TO D01-A15
              MOVE GI-PAYO1   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-PAYO2 NOT = OGI-PAYO2
              MOVE "GI-PAYO2" TO D01-TEXT
              MOVE OGI-PAYO2  TO D01-A15
              MOVE GI-PAYO2   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-PAYO3 NOT = OGI-PAYO3
              MOVE "GI-PAYO3" TO D01-TEXT
              MOVE OGI-PAYO3  TO D01-A15
              MOVE GI-PAYO3   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-PAYO4 NOT = OGI-PAYO4
              MOVE "GI-PAYO4" TO D01-TEXT
              MOVE OGI-PAYO4  TO D01-A15
              MOVE GI-PAYO4   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-PAYO5 NOT = OGI-PAYO5
              MOVE "GI-PAYO5" TO D01-TEXT
              MOVE OGI-PAYO5  TO D01-A15
              MOVE GI-PAYO5   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-PAYO6 NOT = OGI-PAYO6
              MOVE "GI-PAYO6" TO D01-TEXT
              MOVE OGI-PAYO6  TO D01-A15
              MOVE GI-PAYO6   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-PAYO7 NOT = OGI-PAYO7
              MOVE "GI-PAYO7" TO D01-TEXT
              MOVE OGI-PAYO7  TO D01-A15
              MOVE GI-PAYO7   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-DLPAYABLE NOT = OGI-DLPAYABLE
              MOVE "GI-DLPAYABLE" TO D01-TEXT
              MOVE OGI-DLPAYABLE  TO D01-A15
              MOVE GI-DLPAYABLE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-DLRVPRIN-1 NOT = OGI-DLRVPRIN-1
              MOVE "GI-DLRVPRIN-1" TO D01-TEXT
              MOVE OGI-DLRVPRIN-1  TO D01-A15
              MOVE GI-DLRVPRIN-1   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-DLRVPRIN-2 NOT = OGI-DLRVPRIN-2
              MOVE "GI-DLRVPRIN-2" TO D01-TEXT
              MOVE OGI-DLRVPRIN-2  TO D01-A15
              MOVE GI-DLRVPRIN-2   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-DLRVPRIN-3 NOT = OGI-DLRVPRIN-3
              MOVE "GI-DLRVPRIN-3" TO D01-TEXT
              MOVE OGI-DLRVPRIN-3  TO D01-A15
              MOVE GI-DLRVPRIN-3   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-DLRVCHGS-1 NOT = OGI-DLRVCHGS-1
              MOVE "GI-DLRVCHGS-1" TO D01-TEXT
              MOVE OGI-DLRVCHGS-1  TO D01-A15
              MOVE GI-DLRVCHGS-1   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-DLRVCHGS-2 NOT = OGI-DLRVCHGS-2
              MOVE "GI-DLRVCHGS-2" TO D01-TEXT
              MOVE OGI-DLRVCHGS-2  TO D01-A15
              MOVE GI-DLRVCHGS-2   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-DLRVCHGS-3 NOT = OGI-DLRVCHGS-3
              MOVE "GI-DLRVCHGS-3" TO D01-TEXT
              MOVE OGI-DLRVCHGS-3  TO D01-A15
              MOVE GI-DLRVCHGS-3   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-DLPARV NOT = OGI-DLPARV
              MOVE "GI-DLPARV" TO D01-TEXT
              MOVE OGI-DLPARV  TO D01-A15
              MOVE GI-DLPARV   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-PAYALLOT NOT = OGI-PAYALLOT
              MOVE "GI-PAYALLOT" TO D01-TEXT
              MOVE OGI-PAYALLOT  TO D01-A15
              MOVE GI-PAYALLOT   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-DIRECT-PAY NOT = OGI-DIRECT-PAY
              MOVE "GI-DIRECT-PAY" TO D01-TEXT
              MOVE OGI-DIRECT-PAY  TO D01-A15
              MOVE GI-DIRECT-PAY   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-ERINTCHG NOT = OGI-ERINTCHG
              MOVE "GI-ERINTCHG" TO D01-TEXT
              MOVE OGI-ERINTCHG  TO D01-A15
              MOVE GI-ERINTCHG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-ERADDON NOT = OGI-ERADDON
              MOVE "GI-ERADDON" TO D01-TEXT
              MOVE OGI-ERADDON  TO D01-A15
              MOVE GI-ERADDON   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-ERSERCHG NOT = OGI-ERSERCHG
              MOVE "GI-ERSERCHG" TO D01-TEXT
              MOVE OGI-ERSERCHG  TO D01-A15
              MOVE GI-ERSERCHG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-ERMAINTFEE NOT = OGI-ERMAINTFEE
              MOVE "GI-ERMAINTFEE" TO D01-TEXT
              MOVE OGI-ERMAINTFEE  TO D01-A15
              MOVE GI-ERMAINTFEE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-ERDLDISC NOT = OGI-ERDLDISC
              MOVE "GI-ERDLDISC" TO D01-TEXT
              MOVE OGI-ERDLDISC  TO D01-A15
              MOVE GI-ERDLDISC   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-EROTHER NOT = OGI-EROTHER
              MOVE "GI-EROTHER" TO D01-TEXT
              MOVE OGI-EROTHER  TO D01-A15
              MOVE GI-EROTHER   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-LATECHG NOT = OGI-LATECHG
              MOVE "GI-LATECHG" TO D01-TEXT
              MOVE OGI-LATECHG  TO D01-A15
              MOVE GI-LATECHG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-INTEREST NOT = OGI-INTEREST
              MOVE "GI-INTEREST" TO D01-TEXT
              MOVE OGI-INTEREST  TO D01-A15
              MOVE GI-INTEREST   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-DEFERMENTS NOT = OGI-DEFERMENTS
              MOVE "GI-DEFERMENTS" TO D01-TEXT
              MOVE OGI-DEFERMENTS  TO D01-A15
              MOVE GI-DEFERMENTS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-SAPRINCIPAL NOT = OGI-SAPRINCIPAL
              MOVE "GI-SAPRINCIPAL" TO D01-TEXT
              MOVE OGI-SAPRINCIPAL  TO D01-A15
              MOVE GI-SAPRINCIPAL   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 5
              IF GI-PLINT(SUB1) NOT = OGI-PLINT(SUB1)
                 STRING "GI-PLINT(",SUB1,")" INTO D01-TEXT
                 MOVE OGI-PLINT(SUB1)  TO D01-A15 
                 MOVE GI-PLINT(SUB1)   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF GI-PLREC(SUB1) NOT = OGI-PLREC(SUB1)
                 STRING "GI-PLREC(",SUB1,")" INTO D01-TEXT
                 MOVE OGI-PLREC(SUB1)  TO D01-A15 
                 MOVE GI-PLREC(SUB1)   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.

           IF GI-CL1 NOT = OGI-CL1
              MOVE "GI-CL1" TO D01-TEXT
              MOVE OGI-CL1  TO D01-A15
              MOVE GI-CL1   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-CL2 NOT = OGI-CL2
              MOVE "GI-CL2" TO D01-TEXT
              MOVE OGI-CL2  TO D01-A15
              MOVE GI-CL2   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-AH NOT = OGI-AH
              MOVE "GI-AH" TO D01-TEXT
              MOVE OGI-AH  TO D01-A15
              MOVE GI-AH   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-PP NOT = OGI-PP
              MOVE "GI-PP" TO D01-TEXT
              MOVE OGI-PP  TO D01-A15
              MOVE GI-PP   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-O1 NOT = OGI-O1
              MOVE "GI-O1" TO D01-TEXT
              MOVE OGI-O1  TO D01-A15
              MOVE GI-O1   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-O2 NOT = OGI-O2
              MOVE "GI-O2" TO D01-TEXT
              MOVE OGI-O2  TO D01-A15
              MOVE GI-O2   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-O3 NOT = OGI-O3
              MOVE "GI-O3" TO D01-TEXT
              MOVE OGI-O3  TO D01-A15
              MOVE GI-O3   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-O4 NOT = OGI-O4
              MOVE "GI-O4" TO D01-TEXT
              MOVE OGI-O4  TO D01-A15
              MOVE GI-O4   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-O5 NOT = OGI-O5
              MOVE "GI-O5" TO D01-TEXT
              MOVE OGI-O5  TO D01-A15
              MOVE GI-O5   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-O6 NOT = OGI-O6
              MOVE "GI-O6" TO D01-TEXT
              MOVE OGI-O6  TO D01-A15
              MOVE GI-O6   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-O7 NOT = OGI-O7
              MOVE "GI-O7" TO D01-TEXT
              MOVE OGI-O7  TO D01-A15
              MOVE GI-O7   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 5
              IF GI-PLNEW-AR(SUB1) NOT = OGI-PLNEW-AR(SUB1)
                 STRING "GI-PLNEW-AR(",SUB1,")" INTO D01-TEXT
                 MOVE OGI-PLNEW-AR(SUB1)  TO D01-A15 
                 MOVE GI-PLNEW-AR(SUB1)   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.

           IF GI-REPO-AR NOT = OGI-REPO-AR
              MOVE "GI-REPO-AR" TO D01-TEXT
              MOVE OGI-REPO-AR  TO D01-A15
              MOVE GI-REPO-AR   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-REPO-INT NOT = OGI-REPO-INT
              MOVE "GI-REPO-INT" TO D01-TEXT
              MOVE OGI-REPO-INT  TO D01-A15
              MOVE GI-REPO-INT   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-ACR-INT-AR NOT = OGI-ACR-INT-AR
              MOVE "GI-ACR-INT-AR" TO D01-TEXT
              MOVE OGI-ACR-INT-AR  TO D01-A15
              MOVE GI-ACR-INT-AR   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-CONV-OFFSET NOT = OGI-CONV-OFFSET
              MOVE "GI-CONV-OFFSET" TO D01-TEXT
              MOVE OGI-CONV-OFFSET  TO D01-A15
              MOVE GI-CONV-OFFSET   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-PARTIAL-PLEXP NOT = OGI-PARTIAL-PLEXP
              MOVE "GI-PARTIAL-PLEXP" TO D01-TEXT
              MOVE OGI-PARTIAL-PLEXP  TO D01-A15
              MOVE GI-PARTIAL-PLEXP   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-PAYLBOX NOT = OGI-PAYLBOX
              MOVE "GI-PAYLBOX" TO D01-TEXT
              MOVE OGI-PAYLBOX  TO D01-A15
              MOVE GI-PAYLBOX   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-PENALTY-INT NOT = OGI-PENALTY-INT
              MOVE "GI-PENALTY-INT" TO D01-TEXT
              MOVE OGI-PENALTY-INT  TO D01-A15
              MOVE GI-PENALTY-INT   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-PAYTPAY NOT = OGI-PAYTPAY
              MOVE "GI-PAYTPAY" TO D01-TEXT
              MOVE OGI-PAYTPAY  TO D01-A15
              MOVE GI-PAYTPAY   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-PAYTCOLL NOT = OGI-PAYTCOLL
              MOVE "GI-PAYTCOLL" TO D01-TEXT
              MOVE OGI-PAYTCOLL  TO D01-A15
              MOVE GI-PAYTCOLL  TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-PAYMGRAM NOT = OGI-PAYMGRAM
              MOVE "GI-PAYMGRAM" TO D01-TEXT
              MOVE OGI-PAYMGRAM  TO D01-A15
              MOVE GI-PAYMGRAM   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-REPO-ADDON-EXP NOT = OGI-REPO-ADDON-EXP
              MOVE "GI-REPO-ADDON-EXP" TO D01-TEXT
              MOVE OGI-REPO-ADDON-EXP  TO D01-A15
              MOVE GI-REPO-ADDON-EXP   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-ACH-BANK NOT = OGI-ACH-BANK
              MOVE "GI-ACH-BANK" TO D01-TEXT
              MOVE OGI-ACH-BANK  TO D01-A15
              MOVE GI-ACH-BANK   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GI-LN-APPROVAL-LIMIT NOT = OGI-LN-APPROVAL-LIMIT
              MOVE "GI-LN-APPROVAL-LIMIT" TO D01-TEXT
              MOVE OGI-LN-APPROVAL-LIMIT  TO D01-A15
              MOVE GI-LN-APPROVAL-LIMIT   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

       EXIT-COMPARE-FIELDS.
           EXIT.

       WRITE-DETAIL-LINE-ERROR-KEY SECTION.

           MOVE BR-NO     TO D01-BRNO.
           MOVE GI-CLASS  TO D01-CLASS.

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
           MOVE EXT-CONAME-CONAME TO GIFILE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GIFILE-SYSDATE.
           DISPLAY GIFILE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE GIFILE-FORM-FIELDS TO WR-BUF.
           MOVE GIFILE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE GIFILE-HELP--FILE TO HELP-LINK-FILE.
           MOVE GIFILE-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/GIFILE_EVA.CPY".
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
       COPY "LIBGL/GLGIGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGL/GLGI1RN.CPY".

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
       OPEN-OGI1-FILE.
           PERFORM OPEN-IT.
           MOVE OGI-PATH TO E-FILE.
           OPEN INPUT OGI-FILE.
           IF IO-FG = 8
              GO TO OPEN-OGI1-FILE.
           IF IO-FG = 7
              CLOSE OGI-FILE
              GO TO OPEN-OGI1-FILE.
       START-OGI1-FILE.
           PERFORM START-IT.
           MOVE OGI-PATH TO E-FILE.
           MOVE OGI1-KEY TO E-KEYX.
           START OGI-FILE KEY NOT < OGI1-KEY.
        READ-OGI1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OGI-PATH TO E-FILE.
           READ OGI-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OGI1-FILE-NEXT.
       CLOSE-OGI1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OGI-FILE.

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
           MOVE WK-PATH TO E-FILE.
           START WK-FILE KEY NOT < WK1-KEY.
           UNLOCK WK-FILE.
       READ-WK1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           MOVE WK1-KEY TO E-KEYX.
           READ WK-FILE NEXT
              AT END  MOVE 9 TO IO-STATUS IO-FG
           END-READ.
           IF IO-FG = 8
              GO TO READ-WK1-FILE-NEXT.
       WRITE-WK1-FILE.
           PERFORM WRITE-IT.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           MOVE WK1-KEY TO E-KEYX.
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
           MOVE WK1-KEY  TO E-KEYX.
           REWRITE WK-REC.
           IF IO-FG = 8
             GO TO REWRITE-WK1-FILE.
           UNLOCK WK-FILE.
       CLOSE-WK1-FILE.
           PERFORM CLOSE-IT.
           MOVE 0 TO IO-FG IO-STATUS.
           CLOSE WK-FILE.

      * COPYMEMBER: LIBGB/GBBRIN
       OPEN-OBR-FILE.
           PERFORM OPEN-IT.
           MOVE OBR-PATH TO E-FILE.
           OPEN I-O OBR-FILE.
           IF IO-FG = 8
              GO TO OPEN-OBR-FILE.
           IF IO-FG = 7
              CLOSE OBR-FILE
              GO TO OPEN-OBR-FILE.
           UNLOCK OBR-FILE.
       READ-OBR-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OBR-PATH TO E-FILE.
           MOVE OBR-KEY TO E-KEYX.
           READ OBR-FILE NEXT
              AT END  MOVE 9 TO IO-STATUS IO-FG
           END-READ.
           IF IO-FG = 8
              GO TO READ-OBR-FILE-NEXT.
       CLOSE-OBR-FILE.
           PERFORM CLOSE-IT.
           CLOSE OBR-FILE.

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
                 MOVE SV-GROUP-FLAG        TO GROUP-FLAG
                 MOVE SV-ENT-GROUP         TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BRANCH        TO BEG-BRANCH
                 MOVE SV-END-BRANCH        TO END-BRANCH.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE EXIT-PATH               TO FORM-NAM
              MOVE FORM-PATH               TO FORM-PATHNAME.

           MOVE WK-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY GIFILE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
