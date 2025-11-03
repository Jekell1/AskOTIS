       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GLFILE.
       DATE-WRITTEN.  12-13-2023.
      ******************************************************************
      *
      * DESCRIPTION   : COMPARE S35 GLFILE WITH A15 GLFILE
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
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
      * COPYMEMBER: LIBGL/GLFSGL
           SELECT OGL-FILE ASSIGN TO OGL-PATH
                  ORGANIZATION INDEXED
                  ACCESS MODE DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OGL1-KEY
                  ALTERNATE RECORD KEY OGL2-KEY WITH DUPLICATES
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  OGL-FILE
           LABEL RECORD STANDARD.
       COPY "LIBUP/A15SRC/GL01GL.CPY" REPLACING LEADING "GL" BY "OGL".

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)UT/GLFILE.CBL S35 12/13/23-12:29:42 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGL/GL01GL.CPY".
       COPY "LIBGL/GL01GL_SQL.CPY".
       COPY "LIBGL/GLWSGL.CPY".

       01  OGL-PATH                    PIC X(35) VALUE SPACES.
       01  OGL-ALL-PATH.
           03  OGL-ALL-BASE     PIC X(9).
           03  FILLER           PIC X     VALUE "/".
           03  OGL-ALL-MACHINE  PIC XX.
           03  FILLER           PIC X(10) VALUE
               "/GL/GLFILE".

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

       01  PROCESS-STAT                PIC X(08)    VALUE SPACES.
           88  PROCESS-STAT-GOOD                    VALUE "00".
           88  PROCESS-STAT-BAD                     VALUE "  " THRU "//"
                                                         "01" THRU "~~".

       01  FIRST-SW                    PIC X        VALUE "Y".
       01  GL-EOF-SW                   PIC X        VALUE "N".
       01  OGL-EOF-SW                  PIC X        VALUE "N".
       01  TOTAL-ERRORS-CTR            PIC 9(9)     VALUE 0.
       01  SUB2                        PIC 99.
       01  SQL-RECORDS-PROCESSED       PIC 9(9)     VALUE 0.
       01  A15-RECORDS-PROCESSED       PIC 9(9)     VALUE 0.
           88  NO-RECORDS-FOUND                     VALUE ZEROES.

       01  STARTING-TIME               PIC X(08)     VALUE SPACES.
       01  SUB                         PIC 9(04)     VALUE ZEROES.

       01  SYSDATE-YYYYMMDD            PIC 9(8)     VALUE ZEROES.

       01  LEGEND                      PIC X(79)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(07)     VALUE "LEGEND.".


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
           "GL MASTER FILE (GLFILE) COMPARE A15 TO S35".

       01  HEADER-LINE-03.
           03  FILLER                PIC X(4)   VALUE "BRNO".
           03  FILLER                PIC X(3)   VALUE SPACES.
           03  FILLER                PIC X(7)   VALUE "  GLNO ".
           03  FILLER                PIC X(3)   VALUE SPACES.
           03  FILLER                PIC X(28)  VALUE "DESCRIPTION".
           03  FILLER                PIC X(22)  VALUE "A15".
           03  FILLER                PIC X(3)   VALUE "S35".

      ******************************************************************
      *
      *    D E T A I L - L I N E - W O R K E R S
      *
      ******************************************************************
       01  DETAIL-LINE                        PIC X(80)   VALUE SPACES.

       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-BRNO                       PIC Z(3)9.
           03  FILLER                         PIC X(3).
           03  D01-NO                         PIC Z(6)9.
           03  FILLER                         PIC X(3).
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
       COPY "LIBUT/GLFILE_DEF.CPY".
       COPY "LIBUT/GLFILE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/GLFILE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE OGL-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-GL1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE OGL-FILE.
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
           MOVE "GLFILE" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE GLFILE-QUEUE-POS TO Q-POS.
           MOVE GLFILE-COPIES-POS TO C-POS.
           MOVE GLFILE-PRU-POS   TO P-POS.
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
           PERFORM OPEN-GL1-FILE.

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

      * IF FIRST-SW = "N' ERROR - A15 FILE WAS NOT FOUND
           IF FIRST-SW = "N"
              GO TO EXECUTION-NEXT-GROUP
           ELSE
              GO TO EXECUTION-MODULE-EXIT.

       EXECUTION-MODULE-EXIT.
           EXIT.

     *********************************************************
       PROCESS-BRANCH-DRIVER SECTION.
     *********************************************************

           MOVE 9999                       TO PROGID-SLEEP-COUNTER.
           PERFORM SEND-PROGRESS-INDICATOR.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

      D    STOP MESS.

           MOVE "N"        TO OGL-EOF-SW GL-EOF-SW.

           MOVE BR-NO      TO QGL1-WBEG-BRANCH QGL1-WEND-BRANCH.
           MOVE ZEROS      TO QGL1-WBEG-ACCOUNT.
           MOVE ALL '9'    TO QGL1-WEND-ACCOUNT.

           MOVE "PROCESSING S35 DATA" TO BTRACK-MESS.
           PERFORM BTRACK-GLOBAL.

           PERFORM START-GL1-FILE.
           PERFORM M-READ-GL.

           IF FIRST-SW = "Y"
              MOVE SOURCE-PATH  TO OGL-ALL-BASE
              MOVE MACHINE-R1   TO OGL-ALL-MACHINE
              MOVE OGL-ALL-PATH TO OGL-PATH ACCESS-BUF
              PERFORM ACCESS-CALL
              IF ( STAT-BAD )
                 MOVE "MISSING R1 A15 FILE" TO D01-TEXT
                 PERFORM WRITE-DETAIL-LINE-ERROR
                 GO TO PROCESS-BRANCH-DRIVER-END
              ELSE 
                 MOVE "N" TO FIRST-SW
                 PERFORM OPEN-OGL1-FILE.

           MOVE "PROCESSING A15 DATA" TO BTRACK-MESS.  
           PERFORM BTRACK-GLOBAL.

           MOVE BR-NO TO OGL-BRANCH.
           MOVE 0     TO OGL-ACCOUNT.
 
           PERFORM START-OGL1-FILE.
           PERFORM M-READ-OGL.

           PERFORM PROCESS-BRANCH-NEXT-GL
              UNTIL GL-EOF-SW = "Y" AND OGL-EOF-SW = "Y".

           GO TO PROCESS-BRANCH-DRIVER-END.

       PROCESS-BRANCH-NEXT-GL.
           PERFORM SEND-PROGRESS-INDICATOR.

           PERFORM COMPARE-FIELDS.
           IF OGL1-KEY < GL1-KEY
              PERFORM M-READ-OGL
           ELSE
              IF OGL1-KEY > GL1-KEY
                 PERFORM M-READ-GL
              ELSE
                 PERFORM M-READ-OGL
                 PERFORM M-READ-GL.

       PROCESS-BRANCH-DRIVER-END.

           PERFORM CLOSE-GL1-FILE.

       PROCESS-BRANCH-DRIVER-EXIT.
           EXIT.

       M-READ-OGL SECTION.
           PERFORM READ-OGL1-FILE-NEXT.
           IF IO-BAD OR OGL-BRANCH NOT = BR-NO
              MOVE ALL '9'  TO OGL1-KEY
              MOVE "Y"      TO OGL-EOF-SW
           ELSE
              ADD 1 TO A15-RECORDS-PROCESSED.

       EXIT-M-READ-OGL.
           EXIT.

       M-READ-GL SECTION.
           PERFORM READ-GL1-FILE-NEXT.
           IF IO-BAD
              MOVE ALL '9' TO GL1-KEY
              MOVE "Y"     TO GL-EOF-SW
           ELSE
              ADD 1 TO SQL-RECORDS-PROCESSED.

       EXIT-M-READ-GL.
           EXIT.

      ******************************************************************
       COMPARE-FIELDS SECTION.
      ******************************************************************

           IF OGL1-KEY < GL1-KEY
              MOVE "MISSING S35 RECORD" TO D01-TEXT
              MOVE BR-NO                TO D01-BRNO
              MOVE OGL-ACCOUNT          TO D01-NO
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.

           IF OGL1-KEY > GL1-KEY
              MOVE "MISSING A15 RECORD" TO D01-TEXT
              MOVE BR-NO                TO D01-BRNO
              MOVE GL-ACCOUNT           TO D01-NO
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.

           IF GL-FLAG NOT = OGL-FLAG
      *        CALL "C$JUSTIFY" USING OGL-FLAG, "L"
              IF GL-FLAG NOT = OGL-FLAG
                 MOVE "GL-FLAG"  TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE GL-ACCOUNT TO D01-NO
                 MOVE OGL-FLAG   TO D01-A15
                 MOVE GL-FLAG    TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GL-DESCRIPTION NOT = OGL-DESCRIPTION
      *        CALL "C$JUSTIFY" USING OGL-DESCRIPTION, "L"
              IF GL-DESCRIPTION NOT = OGL-DESCRIPTION
                 MOVE "GL-DESCRIPTION" TO D01-TEXT
                 MOVE BR-NO            TO D01-BRNO
                 MOVE GL-ACCOUNT       TO D01-NO
                 MOVE OGL-DESCRIPTION  TO D01-A15
                 MOVE GL-DESCRIPTION   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OGL-CONSOL NOT NUMERIC
              MOVE 0 TO OGL-CONSOL.
           IF OGL-CONSOL NOT = GL-CONSOL
              MOVE "GL-CONSOL" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE GL-ACCOUNT  TO D01-NO
              MOVE OGL-CONSOL  TO D01-A15-NUM
              MOVE GL-CONSOL   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OGL-OPENBAL NOT NUMERIC
              MOVE 0 TO OGL-OPENBAL.
           IF OGL-OPENBAL NOT = GL-OPENBAL
              MOVE "GL-OPENBAL" TO D01-TEXT
              MOVE BR-NO        TO D01-BRNO
              MOVE GL-ACCOUNT   TO D01-NO
              MOVE OGL-OPENBAL  TO D01-A15-NUM
              MOVE GL-OPENBAL   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB2 FROM 1 BY 1 UNTIL SUB2 > 12
              IF OGL-CYBAL(SUB2) NOT NUMERIC
                 MOVE 0 TO OGL-CYBAL(SUB2)
              END-IF
              IF OGL-CYBAL(SUB2) NOT = GL-CYBAL(SUB2)
                 MOVE SPACES TO D01-TEXT
                 STRING "GL-CYBAL(", SUB2, ")" INTO D01-TEXT
                 MOVE BR-NO           TO D01-BRNO
                 MOVE GL-ACCOUNT      TO D01-NO
                 MOVE OGL-CYBAL(SUB2) TO D01-A15-NUM
                 MOVE GL-CYBAL(SUB2)  TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR
               END-IF
              IF OGL-LYBAL(SUB2) NOT NUMERIC
                 MOVE 0 TO OGL-LYBAL(SUB2)
              END-IF
              IF OGL-LYBAL(SUB2) NOT = GL-LYBAL(SUB2)
                 MOVE SPACES TO D01-TEXT
                 STRING "GL-LYBAL(", SUB2, ")" INTO D01-TEXT
                 MOVE BR-NO           TO D01-BRNO
                 MOVE GL-ACCOUNT      TO D01-NO
                 MOVE OGL-LYBAL(SUB2) TO D01-A15-NUM
                 MOVE GL-LYBAL(SUB2)  TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR
               END-IF
              IF OGL-CYBUD(SUB2) NOT NUMERIC
                 MOVE 0 TO OGL-CYBUD(SUB2)
              END-IF
              IF OGL-CYBUD(SUB2) NOT = GL-CYBUD(SUB2)
                 MOVE SPACES TO D01-TEXT
                 STRING "GL-CYBUD(", SUB2, ")" INTO D01-TEXT
                 MOVE BR-NO           TO D01-BRNO
                 MOVE GL-ACCOUNT      TO D01-NO
                 MOVE OGL-CYBUD(SUB2) TO D01-A15-NUM
                 MOVE GL-CYBUD(SUB2)  TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR
               END-IF
              IF OGL-LYBUD(SUB2) NOT NUMERIC
                 MOVE 0 TO OGL-LYBUD(SUB2)
              END-IF
              IF OGL-LYBUD(SUB2) NOT = GL-LYBUD(SUB2)
                 MOVE SPACES TO D01-TEXT
                 STRING "GL-LYBUD(", SUB2, ")" INTO D01-TEXT
                 MOVE BR-NO           TO D01-BRNO
                 MOVE GL-ACCOUNT      TO D01-NO
                 MOVE OGL-LYBUD(SUB2) TO D01-A15-NUM
                 MOVE GL-LYBUD(SUB2)  TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR
               END-IF
              IF OGL-NYBUD(SUB2) NOT NUMERIC
                 MOVE 0 TO OGL-NYBUD(SUB2)
              END-IF
              IF OGL-NYBUD(SUB2) NOT = GL-NYBUD(SUB2)
                 MOVE SPACES TO D01-TEXT
                 STRING "GL-NYBUD(", SUB2, ")" INTO D01-TEXT
                 MOVE BR-NO            TO D01-BRNO
                 MOVE GL-ACCOUNT       TO D01-NO
                 MOVE OGL-NYBUD(SUB2)  TO D01-A15-NUM
                 MOVE GL-NYBUD(SUB2)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR
               END-IF
           END-PERFORM.

           IF GL-TYPE NOT = OGL-TYPE
              MOVE "GL-TYPE"  TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE GL-ACCOUNT TO D01-NO
              MOVE OGL-TYPE   TO D01-A15
              MOVE GL-TYPE    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GL-CLEAR NOT = OGL-CLEAR
              MOVE "GL-CLEAR" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE GL-ACCOUNT TO D01-NO
              MOVE OGL-CLEAR  TO D01-A15
              MOVE GL-CLEAR   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GL-NUMBER1 NOT = OGL-NUMBER1
              MOVE "GL-NUMBER1" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE GL-ACCOUNT TO D01-NO
              MOVE OGL-NUMBER1  TO D01-A15
              MOVE GL-NUMBER1   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GL-NUMBER2 NOT = OGL-NUMBER2
              MOVE "GL-NUMBER2" TO D01-TEXT
              MOVE BR-NO        TO D01-BRNO
              MOVE GL-ACCOUNT   TO D01-NO
              MOVE OGL-NUMBER2  TO D01-A15
              MOVE GL-NUMBER2   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GL-FS-FLAG NOT = OGL-FS-FLAG
              MOVE "GL-FS-FLAG" TO D01-TEXT
              MOVE BR-NO        TO D01-BRNO
              MOVE GL-ACCOUNT   TO D01-NO
              MOVE OGL-FS-FLAG  TO D01-A15
              MOVE GL-FS-FLAG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB2 FROM 1 BY 1 UNTIL SUB2 > 5
             IF OGL-REPORTS(SUB2) NOT = GL-REPORTS(SUB2)
                 MOVE SPACES            TO D01-TEXT
                 STRING "GL-REPORTS(", SUB2, ")" INTO D01-TEXT
                 MOVE BR-NO             TO D01-BRNO
                 MOVE GL-ACCOUNT        TO D01-NO
                 MOVE OGL-REPORTS(SUB2) TO D01-A15
                 MOVE GL-REPORTS(SUB2)  TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR
               END-IF
           END-PERFORM. 

           IF OGL-LYOPENBAL NOT = GL-LYOPENBAL
              MOVE SPACES TO D01-TEXT
              MOVE "GL-LYOPENBAL" TO D01-TEXT
              MOVE BR-NO          TO D01-BRNO
              MOVE GL-ACCOUNT     TO D01-NO
              MOVE OGL-LYOPENBAL  TO D01-A15-NUM
              MOVE GL-LYOPENBAL   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GL-DLALLOC-GL NOT = OGL-DLALLOC-GL
              MOVE "GL-DLALLOC-GL" TO D01-TEXT
              MOVE BR-NO           TO D01-BRNO
              MOVE GL-ACCOUNT      TO D01-NO
              MOVE OGL-DLALLOC-GL  TO D01-A15
              MOVE GL-DLALLOC-GL   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF GL-SPANISH-DESC NOT = OGL-SPANISH-DESC
              MOVE "GL-SPANISH-DESC" TO D01-TEXT
              MOVE BR-NO             TO D01-BRNO
              MOVE GL-ACCOUNT        TO D01-NO
              MOVE OGL-SPANISH-DESC  TO D01-A15
              MOVE GL-SPANISH-DESC   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

       EXIT-COMPARE-FIELDS.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE-ERROR SECTION.
      ******************************************************************
           ADD 1 TO TOTAL-ERRORS-CTR.
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
           MOVE EXT-CONAME-CONAME TO GLFILE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GLFILE-SYSDATE.
           DISPLAY GLFILE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE GLFILE-FORM-FIELDS TO WR-BUF.
           MOVE GLFILE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE GLFILE-HELP--FILE TO HELP-LINK-FILE.
           MOVE GLFILE-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/GLFILE_EVA.CPY".
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
       COPY "LIBGL/GLGLGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGL/GLGL1RN.CPY".

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

      *------------------------------------------------------
       OPEN-OGL1-FILE.
           PERFORM OPEN-IT.
           MOVE OGL-PATH TO E-FILE.
           OPEN INPUT OGL-FILE.
           IF IO-FG = 8
              GO TO OPEN-OGL1-FILE.
           IF IO-FG = 7
              CLOSE OGL-FILE
              GO TO OPEN-OGL1-FILE.
      *READ-OGL1-FILE.
      *    PERFORM READ-IT.
      *    MOVE OGL-PATH TO E-FILE.
      *    MOVE OGL1-KEY TO E-KEYX.
      *    READ OGL-FILE.
      *    IF IO-FG = 8
      *       GO TO READ-OGL1-FILE.
       READ-OGL1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OGL-PATH TO E-FILE.
           MOVE OGL1-KEY TO E-KEYX.
           READ OGL-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OGL1-FILE-NEXT.
       START-OGL1-FILE.
           PERFORM START-IT.
           MOVE OGL-PATH TO E-FILE.
           MOVE OGL1-KEY TO E-KEYX.
           START OGL-FILE KEY NOT < OGL1-KEY.
       CLOSE-OGL1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OGL-FILE.

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
              DESTROY GLFILE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
