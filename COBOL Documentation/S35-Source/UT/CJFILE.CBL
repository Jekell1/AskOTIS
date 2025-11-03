       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CJFILE.
       DATE-WRITTEN.  12-06-2023.
      ******************************************************************
      *
      * DESCRIPTION   : COMPARE S35 CJFILE WITH A15 CJFILE
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
      *  01-07-25  RMZ  ADDED 'BTRACK' - PROCESS TRACKING TO PROGRAM.
      *  03-04-25  MB   CREATED COMMON PARAGRAPH FOR REPORT KEYS 
      *  04-30-25  MB   MISC REPORT CHANGES.
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
      * COPYMEMBER: LIBSP/SPFSCJ
           SELECT OCJ-FILE ASSIGN TO OCJ-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OCJ1-KEY
                  ALTERNATE RECORD KEY OCJ2-KEY 
                             = OCJ-BRNO
                               OCJ-ACCTNO
                               OCJ-DATE
                               OCJ-START-TIME
                             WITH DUPLICATES
                  FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  OCJ-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A15SRC/SP01CJ.CPY" REPLACING LEADING "CJ" BY "OCJ".

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)UT/CJFILE.CBL S35 01/03/25-14:11:23 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBSP/SP01CJ.CPY".
       COPY "LIBSP/SP01CJ_SQL.CPY".
       COPY "LIBSP/SPWSCJ.CPY".
       01  OCJ-PATH                    PIC X(35) VALUE SPACES.
       01  OCJ-ALL-PATH.
           03  OCJ-ALL-BASE     PIC X(9).
           03  FILLER           PIC X     VALUE "/".
           03  OCJ-ALL-MACHINE  PIC XX.
           03  FILLER           PIC X     VALUE "/".
           03  OCJ-ALL-DATA     PIC X(6).
           03  FILLER           PIC X(10) VALUE
               "/SP/CJFILE".

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

       01  CJ1-CONN-OPEN-SW     PIC X     VALUE "N".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************

       01  CJ-EOF-SW                   PIC X      VALUE "N".
       01  OCJ-EOF-SW                  PIC X      VALUE "N".

       01  WS-CJ1-KEY.
           05  WS-CJ-BRNO              PIC 9(4).
           05  WS-CJ-COLLID            PIC XXX.
           05  WS-CJ-DATE              PIC 9(8).
           05  WS-CJ-START-TIME        PIC 9(4).
           05  WS-CJ-SEQNO             PIC 99.

       01  WS-OCJ1-KEY.
           05  WS-OCJ-BRNO             PIC 9(4).
           05  WS-OCJ-COLLID           PIC XXX.
           05  WS-OCJ-DATE             PIC 9(8).
           05  WS-OCJ-START-TIME       PIC 9(4).
           05  WS-OCJ-SEQNO            PIC 99.

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
       01  RECORDS-PROCESSED           PIC 9(9)     VALUE 0.
           88  NO-RECORDS-FOUND                     VALUE ZEROES.

       01  A15-RECORD-CTR              PIC 9(9)      VALUE 0.
       01  S35-RECORD-CTR              PIC 9(9)      VALUE 0.
       01  TOTAL-ERRORS-CTR            PIC 9(9)      VALUE 0.

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
       01  LEGEND-BEG-ACCTNO.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F7-EXIT, ENT".
           03  FILLER PIC X(30) VALUE "ER BEGINNING ACCOUNT NUMBER   ".
           03  FILLER PIC X(19) VALUE "                   ".

       01  LEGEND-END-ACCTNO.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER ENDIN".
           03  FILLER PIC X(30) VALUE "G ACCOUNT NUMBER              ".
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
           03  BEG-COLLID              PIC X(3).
           03  END-COLLID              PIC X(3).
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
           "COLLECTOR JOURNAL (CJFILE) COMPARE A15 TO S35".

       01  HEADER-LINE-03.
           03  FILLER                PIC X(4) VALUE "BRNO".
           03  FILLER                PIC X(2) VALUE SPACES.
           03  FILLER                PIC X(2) VALUE "CD".
           03  FILLER                PIC X(5) VALUE SPACES.
           03  FILLER                PIC X(4) VALUE "DATE".
           03  FILLER                PIC X(4) VALUE SPACES.
           03  FILLER                PIC X(11) VALUE "DESCRIPTION".
           03  FILLER                PIC X(17) VALUE SPACES.
           03  FILLER                PIC X(3) VALUE "A15".
           03  FILLER                PIC X(20) VALUE SPACES.
           03  FILLER                PIC X(3) VALUE "S35".

       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-BRNO                       PIC Z(3)9.
           03  FILLER                         PIC X(2).
           03  D01-COLLID                     PIC X(3).
           03  FILLER                         PIC X(2).
           03  D01-DATE                       PIC 9(8).
           03  FILLER                         PIC X(2).
           03  D01-TEXT                       PIC X(25).
           03  FILLER                         PIC X(3).
           03  D01-A15                        PIC X(21).
           03  FILLER REDEFINES D01-A15.
               05  D01-A15-NUM                PIC Z(7)9.9(3).
               05  FILLER                     PIC X(8).
           03  FILLER                         PIC X(2).
           03  D01-S35                        PIC X(21).
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
       COPY "LIBUT/CJFILE_DEF.CPY".
       COPY "LIBUT/CJFILE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/CJFILE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE OCJ-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-CJ1-FILE.
           PERFORM CLOSE-CJ1-CONNECTION.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE OCJ-FILE.
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
           MOVE "CJFILE" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE CJFILE-QUEUE-POS TO Q-POS.
           MOVE CJFILE-COPIES-POS TO C-POS.
           MOVE CJFILE-PRU-POS   TO P-POS.
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
              GO TO ENTER-END-COLLID.

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


       ENTER-BEG-COLLID.
           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A030BEG-COLLID." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F3-KEY ) GO TO ENTER-ALL-COLLID.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-COLLID.

           MOVE VDUBUF TO BEG-COLLID.

      *-----------------------------------------------------------------
       ENTER-END-COLLID.


           MOVE "E  A030END-COLLID." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-COLLID.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-COLLID.

           MOVE VDUBUF TO END-COLLID.

           IF ( BEG-COLLID > END-COLLID )
              MOVE "INVALID RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-COLLID.

           GO TO ENTER-PARAMETERS-END.

      *-----------------------------------------------------------------
       ENTER-ALL-COLLID.

           MOVE "S  A030BEG-COLLID."  TO VDU.
           MOVE SPACES                TO BEG-COLLID VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A030END-COLLID."  TO VDU.
           MOVE ALL "Z"                TO END-COLLID VDUBUF.
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

           MOVE "S  A030BEG-COLLID."       TO VDU.
           MOVE BEG-COLLID                 TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A030END-COLLID."       TO VDU.
           MOVE END-COLLID                 TO VDUBUF.
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

           MOVE ZEROES                     TO RECORDS-PROCESSED.

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

      *    MOVE "PROCESS NEW BRANCH" TO D01-TEXT.
      *    MOVE BR-NO                TO D01-BRNO.
      *    MOVE SPACES               TO D01-COLLID.
      *    MOVE SPACES               TO D01-A15.
      *    MOVE SPACES               TO D01-S35.
      *    PERFORM WRITE-DETAIL-LINE.

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
           MOVE SOURCE-PATH  TO OCJ-ALL-BASE.
           MOVE MACHINE-R1   TO OCJ-ALL-MACHINE.
           MOVE BR-DATA-PATH TO OCJ-ALL-DATA.
           MOVE OCJ-ALL-PATH TO OCJ-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              IF OCJ-ALL-MACHINE = MACHINE-R1
                 MOVE MACHINE-R2 TO OCJ-ALL-MACHINE
                 MOVE OCJ-ALL-PATH TO OCJ-PATH ACCESS-BUF
                 PERFORM ACCESS-CALL
                 IF ( STAT-BAD )
                    MOVE "MISSING A15 FILE" TO D01-TEXT
                    MOVE BR-NO              TO D01-BRNO
                    MOVE SPACES             TO D01-COLLID
                    MOVE OCJ-ALL-PATH       TO D01-A15
                    MOVE SPACES             TO D01-S35
                    PERFORM WRITE-DETAIL-LINE
                    GO TO PROCESS-BRANCH-DRIVER-END.

           IF EXT-ROUTE-BUF = "00"
              STRING "REPORT IN PROGRESS......", BR-NO INTO LEGEND
              PERFORM SEND-LEGEND.

           MOVE 'N' TO CJ-EOF-SW  
                       OCJ-EOF-SW. 

           PERFORM OPEN-OCJ1-FILE.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO CJ-PATH-OVERRIDE.

           PERFORM OPEN-CJ1-FILE.

           MOVE BR-NO                TO OCJ-BRNO.
           MOVE BEG-COLLID           TO OCJ-COLLID.
           MOVE 0                    TO OCJ-DATE OCJ-START-TIME.
           MOVE 0                    TO OCJ-SEQNO.

           PERFORM START-OCJ1-FILE.

           MOVE BR-NO                TO QCJ1-WBEG-BRNO.     
           MOVE BEG-COLLID           TO QCJ1-WBEG-COLLID.   
           MOVE END-COLLID           TO QCJ1-WEND-COLLID.

           PERFORM START-CJ1-FILE.

           PERFORM PROCESS-READ-OCJ1.
           PERFORM PROCESS-READ-CJ1.

       PROCESS-BRANCH-NEXT-CJ.

           IF CJ-EOF-SW  = "Y" AND
              OCJ-EOF-SW = "Y"
              GO TO PROCESS-BRANCH-DRIVER-EXIT.      

           IF WS-OCJ1-KEY < WS-CJ1-KEY 
              MOVE "MISSING S35 RECORD" TO D01-TEXT
              MOVE OCJ-BRNO             TO D01-BRNO  
              MOVE OCJ-COLLID           TO D01-COLLID
              MOVE WS-OCJ-DATE          TO D01-DATE
              PERFORM WRITE-DETAIL-LINE-ERROR 
              PERFORM PROCESS-READ-OCJ1 
              GO TO PROCESS-BRANCH-NEXT-CJ.
 
           IF WS-CJ1-KEY < WS-OCJ1-KEY
              MOVE "MISSING A15 RECORD" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR       
              PERFORM PROCESS-READ-CJ1 
              GO TO PROCESS-BRANCH-NEXT-CJ.

           PERFORM COMPARE-FIELDS.

           PERFORM PROCESS-READ-OCJ1.  
           PERFORM PROCESS-READ-CJ1. 

           GO TO PROCESS-BRANCH-NEXT-CJ.

       PROCESS-BRANCH-DRIVER-END.

           PERFORM CLOSE-CJ1-FILE.

       PROCESS-BRANCH-DRIVER-EXIT.
           EXIT.

      ******************************************************************
       PROCESS-READ-OCJ1 SECTION.
      ******************************************************************

           PERFORM READ-OCJ1-FILE-NEXT.
           IF IO-BAD 
              MOVE 9999           TO WS-OCJ-BRNO      
              MOVE 'ZZZ'          TO WS-OCJ-COLLID    
              MOVE 99999999       TO WS-OCJ-DATE      
              MOVE 9999           TO WS-OCJ-START-TIME
              MOVE 99             TO WS-OCJ-SEQNO     
              MOVE "Y"            TO OCJ-EOF-SW
              GO TO EXIT-PROCESS-READ-OCJ1
           ELSE 
              MOVE CJ-PATH-OWNBR  TO WS-OCJ-BRNO
              MOVE OCJ-COLLID     TO WS-OCJ-COLLID   
              MOVE OCJ-DATE       TO JULIAN-DATE
              PERFORM CJUL
              MOVE NUM-DATE       TO WS-OCJ-DATE
              MOVE OCJ-START-TIME TO WS-OCJ-START-TIME
              MOVE OCJ-SEQNO      TO WS-OCJ-SEQNO.     
 
      *     PERFORM SEND-PROGRESS-INDICATOR.

           IF ( OCJ-COLLID < BEG-COLLID ) OR
              ( OCJ-COLLID > END-COLLID )
              GO TO PROCESS-READ-OCJ1.

           ADD  01 TO RECORDS-PROCESSED
                      A15-RECORD-CTR.

       EXIT-PROCESS-READ-OCJ1.
           EXIT.

      ******************************************************************
       PROCESS-READ-CJ1 SECTION.
      ******************************************************************

           PERFORM READ-CJ1-FILE-NEXT.
           IF IO-BAD 
              MOVE 9999           TO WS-CJ-BRNO      
              MOVE 'ZZZ'          TO WS-CJ-COLLID    
              MOVE 99999999       TO WS-CJ-DATE      
              MOVE 9999           TO WS-CJ-START-TIME
              MOVE 99             TO WS-CJ-SEQNO     
              MOVE "Y"            TO CJ-EOF-SW
			  
              GO TO EXIT-PROCESS-READ-CJ1
           ELSE 
              MOVE CJ-PATH-OWNBR  TO WS-CJ-BRNO
              MOVE CJ-COLLID      TO WS-CJ-COLLID   
              MOVE CJ-DATE        TO WS-CJ-DATE
              MOVE CJ-START-TIME  TO WS-CJ-START-TIME
              MOVE CJ-SEQNO       TO WS-CJ-SEQNO.  

           IF ( CJ-COLLID < BEG-COLLID ) OR
              ( CJ-COLLID > END-COLLID )
              GO TO PROCESS-READ-CJ1.

           ADD 1                  TO S35-RECORD-CTR.

       EXIT-PROCESS-READ-CJ1.
           EXIT.

      ******************************************************************
       COMPARE-FIELDS SECTION.
      ******************************************************************

      *     MOVE OCJ-BRNO      TO CJ-BRNO.
      *     MOVE OCJ-COLLID    TO CJ-COLLID.
      *     MOVE OCJ-DATE      TO JULIAN-DATE.
      *     PERFORM CJUL.
      *     MOVE NUM-DATE       TO CJ-DATE.
      *     MOVE OCJ-START-TIME TO CJ-START-TIME.
      *     MOVE OCJ-SEQNO      TO CJ-SEQNO.
      *     PERFORM READ-CJ1-FILE.
      *     IF IO-FG NOT = 0
      *        MOVE "MISSING S35 RECORD" TO D01-TEXT
      *        MOVE BR-NO        TO D01-BRNO
      *        MOVE CJ-COLLID    TO D01-COLLID
      *        MOVE CJ-DATE      TO D01-DATE
      *        PERFORM WRITE-DETAIL-LINE
      *        GO TO EXIT-COMPARE-FIELDS.

           IF OCJ-ACCTNO NOT NUMERIC
              MOVE 0          TO OLD-ACCTNO
           ELSE
              MOVE OCJ-ACCTNO TO OLD-ACCTNO.
           IF NEW-ACCTNO NOT = CJ-ACCTNO
              MOVE "CJ-ACCTNO" TO D01-TEXT
              MOVE OCJ-ACCTNO    TO D01-A15-NUM
              MOVE CJ-ACCTNO     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OCJ-END-TIME NOT NUMERIC
              MOVE 0 TO OCJ-END-TIME.
           IF OCJ-END-TIME NOT = CJ-END-TIME
              MOVE "CJ-END-TIME" TO D01-TEXT
              MOVE OCJ-END-TIME    TO D01-A15-NUM
              MOVE CJ-END-TIME     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF CJ-ACTIVITY-CD NOT = OCJ-ACTIVITY-CD
      *       CALL "C$JUSTIFY" USING OCJ-ACTIVITY-CD, "L"
              IF CJ-ACTIVITY-CD NOT = OCJ-ACTIVITY-CD
                 MOVE "CJ-ACTIVITY-CD" TO D01-TEXT
                 MOVE OCJ-ACTIVITY-CD  TO D01-A15
                 MOVE CJ-ACTIVITY-CD   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           IF CJ-RESULT-CD NOT = OCJ-RESULT-CD
      *       CALL "C$JUSTIFY" USING OCJ-RESULT-CD, "L"
              IF CJ-RESULT-CD NOT = OCJ-RESULT-CD
                 MOVE "CJ-RESULT-CD" TO D01-TEXT
                 MOVE OCJ-RESULT-CD  TO D01-A15
                 MOVE CJ-RESULT-CD   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           IF CJ-ACTION-CD NOT = OCJ-ACTION-CD
      *       CALL "C$JUSTIFY" USING OCJ-ACTION-CD, "L"
              IF CJ-ACTION-CD NOT = OCJ-ACTION-CD
                 MOVE "CJ-ACTION-CD" TO D01-TEXT
                 MOVE OCJ-ACTION-CD  TO D01-A15
                 MOVE CJ-ACTION-CD   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           IF CJ-NEW-COLLID NOT = OCJ-NEW-COLLID
      *       CALL "C$JUSTIFY" USING OCJ-NEW-COLLID, "L"
              IF CJ-NEW-COLLID NOT = OCJ-NEW-COLLID
                 MOVE "CJ-NEW-COLLID" TO D01-TEXT
                 MOVE OCJ-NEW-COLLID  TO D01-A15
                 MOVE CJ-NEW-COLLID   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR.
           IF CJ-CON-STAT NOT = OCJ-CON-STAT
      *       CALL "C$JUSTIFY" USING OCJ-CON-STAT, "L"
              IF CJ-CON-STAT NOT = OCJ-CON-STAT
                 MOVE "CJ-CON-STAT" TO D01-TEXT
                 MOVE OCJ-CON-STAT  TO D01-A15
                 MOVE CJ-CON-STAT   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CJ-REC-STAT NOT = OCJ-REC-STAT
      *       CALL "C$JUSTIFY" USING OCJ-REC-STAT, "L"
              IF CJ-REC-STAT NOT = OCJ-REC-STAT
                 MOVE "CJ-REC-STAT" TO D01-TEXT
                 MOVE OCJ-REC-STAT  TO D01-A15
                 MOVE CJ-REC-STAT   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OCJ-FORM-NO NOT NUMERIC
              MOVE 0 TO OCJ-FORM-NO.
           IF OCJ-FORM-NO NOT = CJ-FORM-NO
              MOVE "CJ-FORM-NO" TO D01-TEXT
              MOVE OCJ-FORM-NO    TO D01-A15-NUM
              MOVE CJ-FORM-NO     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OCJ-PROMISE-DATE1 TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = CJ-PROMISE-DATE1
              MOVE "CJ-PROMISE-DATE1" TO D01-TEXT
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE CJ-PROMISE-DATE1    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OCJ-PROMISE-AMT NOT NUMERIC
              MOVE 0 TO OCJ-PROMISE-AMT.
           IF OCJ-PROMISE-AMT NOT = CJ-PROMISE-AMT
              MOVE "CJ-PROMISE-AMT" TO D01-TEXT
              MOVE OCJ-PROMISE-AMT  TO D01-A15-NUM
              MOVE CJ-PROMISE-AMT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OCJ-BEG-LM-SEQ NOT NUMERIC
              MOVE 0 TO OCJ-BEG-LM-SEQ.
           IF OCJ-BEG-LM-SEQ NOT = CJ-BEG-LM-SEQ
              MOVE "CJ-BEG-LM-SEQ" TO D01-TEXT
               MOVE OCJ-BEG-LM-SEQ    TO D01-A15-NUM
              MOVE CJ-BEG-LM-SEQ     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OCJ-END-LM-SEQ NOT NUMERIC
              MOVE 0 TO OCJ-END-LM-SEQ.
           IF OCJ-END-LM-SEQ NOT = CJ-END-LM-SEQ
              MOVE "CJ-END-LM-SEQ"   TO D01-TEXT
              MOVE OCJ-END-LM-SEQ    TO D01-A15-NUM
              MOVE CJ-END-LM-SEQ     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OCJ-PROMISE-DATE2 TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = CJ-PROMISE-DATE2
              MOVE "CJ-PROMISE-DATE2" TO D01-TEXT
              MOVE DATE-YYYYMMDD      TO D01-A15
              MOVE CJ-PROMISE-DATE2   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OCJ-CUR-BAL NOT NUMERIC
              MOVE 0 TO OCJ-CUR-BAL.
           IF OCJ-CUR-BAL NOT = CJ-CUR-BAL
              MOVE "CJ-CUR-BAL"   TO D01-TEXT
              MOVE OCJ-CUR-BAL    TO D01-A15-NUM
              MOVE CJ-CUR-BAL     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OCJ-PAST-DUE NOT NUMERIC
              MOVE 0 TO OCJ-PAST-DUE.
           IF OCJ-PAST-DUE NOT = CJ-PAST-DUE
              MOVE "CJ-PAST-DUE" TO D01-TEXT
              MOVE OCJ-PAST-DUE    TO D01-A15-NUM
              MOVE CJ-PAST-DUE     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OCJ-POSTING-BR NOT NUMERIC
              MOVE 0 TO OCJ-POSTING-BR.
           IF OCJ-POSTING-BR NOT = CJ-POSTING-BR
              MOVE "CJ-POSTING-BR"   TO D01-TEXT
              MOVE OCJ-POSTING-BR    TO D01-A15-NUM
              MOVE CJ-POSTING-BR     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CJ-QUEUE NOT = OCJ-QUEUE
      *       CALL "C$JUSTIFY" USING OCJ-QUEUE, "L"
              IF CJ-QUEUE NOT = OCJ-QUEUE
                 MOVE "CJ-QUEUE" TO D01-TEXT
                 MOVE OCJ-QUEUE  TO D01-A15
                 MOVE CJ-QUEUE   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR.

       EXIT-COMPARE-FIELDS.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE-ERROR-KEY SECTION.
      ******************************************************************
           MOVE CJ-BRNO   TO D01-BRNO.
           MOVE CJ-COLLID TO D01-COLLID.
           MOVE CJ-DATE   TO D01-DATE.

       WRITE-DETAIL-LINE-ERROR-KEY-EXIT.
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

           MOVE "BEGINNING COLLECTOR ID           : "    TO R01-PROMPT.
           MOVE BEG-COLLID                            TO R01-TEXT.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    COLLECTOR ID           : "    TO R01-PROMPT.
           MOVE END-COLLID                            TO R01-TEXT.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "A15 RECORDS PROCESSED         :"     TO R01-PROMPT.
           MOVE A15-RECORD-CTR                        TO R01-NUMERIC.
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
           MOVE EXT-CONAME-CONAME TO CJFILE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CJFILE-SYSDATE.
           DISPLAY CJFILE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE CJFILE-FORM-FIELDS TO WR-BUF.
           MOVE CJFILE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE CJFILE-HELP--FILE TO HELP-LINK-FILE.
           MOVE CJFILE-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/CJFILE_EVA.CPY".
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
           MOVE PROGID-TEXT-PROCESSING     TO PROGID-TEXT.

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
       COPY "LIBSP/SPCJGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
      * COPY "LIBSP/SPCJ1RN.CPY".
      * COPYMEMBER: LIBSP/SPCJ1RN
       LOAD-CJ1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CJ-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CJ-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO CJ-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CJ-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO CJ-ALL-DATA
              MOVE CJ-ALL-PATH             TO CJ-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CJ-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO CJ-ALL-BASE
              MOVE FILPATH-MACHINE         TO CJ-ALL-MACHINE
              MOVE FILPATH-DATA            TO CJ-ALL-DATA
              MOVE CJ-ALL-PATH             TO CJ-PATH.

      *-----------------------------------------------------------------
       OPEN-CJ1-FILE.
           PERFORM LOAD-CJ1-FILE.
           PERFORM OPEN-IT.
           MOVE CJ-PATH-SQL TO E-FILE.

           IF CJ1-CONN-OPEN-SW = "N" 
              MOVE "Y"         TO CJ1-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS CJ1_CONNECT 
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-CJ1-FILE.
           PERFORM START-IT.
           MOVE CJ-PATH-OWNBR  TO CJ-BRNO.
           MOVE CJ-PATH-SQL    TO E-FILE.
           MOVE CJ1-KEY        TO E-KEYX.

           IF ( CJ1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-CJ1-FILE.

           EXEC SQL SET CONNECTION CJ1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE CJ1_CURSOR CURSOR FOR
                :CJ1-RETURN-CODE = EXEC SSP_CJFILE_CJFILE_SELECT 
              ( :QCJ1-WBEG-BRNO,     
                :QCJ1-WBEG-COLLID,   
                :QCJ1-WEND-COLLID )      
           END-EXEC.

           EXEC SQL
               OPEN CJ1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

      *     MOVE ZEROES TO QCJ1-WBEG-BRNO 
      *                    QCJ1-WEND-BRNO
      *                    QCJ1-WBEG-START-TIME
      *                    QCJ1-WEND-START-TIME
      *                    QCJ1-WBEG-SEQNO
      *                    QCJ1-WEND-SEQNO
      *                    QCJ1-WSBEG-START-TIME
      *                    QCJ1-WSBEG-SEQNO.
      *
      *     MOVE SPACES TO QCJ1-WBEG-COLLID
      *                    QCJ1-WEND-COLLID
      *                    QCJ1-WBEG-DATE
      *                    QCJ1-WEND-DATE
      *                    QCJ1-WSBEG-COLLID
      *                    QCJ1-WSBEG-DATE.

           MOVE STAT---GOOD TO CJ1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-CJ1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE CJ-PATH-OWNBR  TO CJ-BRNO.
           MOVE CJ-PATH-SQL    TO E-FILE.
           MOVE CJ1-KEY        TO E-KEYX.
           INITIALIZE QCJ-REC.

           EXEC SQL SET CONNECTION CJ1_CONNECT END-EXEC.

           IF ( CJ1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH CJ1_CURSOR INTO :QCJ-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CJ1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-CJ-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-CJ1-FILE.

           PERFORM CLOSE-IT.
           IF ( CJ1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION CJ1_CONNECT END-EXEC 
              EXEC SQL
                   CLOSE CJ1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO CJ1-CURSOR-STAT.

      *----------------------------------------------------------------- 
       CLOSE-CJ1-CONNECTION.

           IF CJ1-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION CJ1_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT CJ1_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO CJ1-CONN-OPEN-SW.

      *----------------------------------------------------------------- 
   

       OPEN-OCJ1-FILE.
           PERFORM OPEN-IT.
           MOVE OCJ-PATH TO E-FILE.
           OPEN INPUT OCJ-FILE.
           IF IO-FG = 8
              GO TO OPEN-OCJ1-FILE.
           IF IO-FG = 7
              CLOSE OCJ-FILE
              GO TO OPEN-OCJ1-FILE.
       START-OCJ1-FILE.
           PERFORM START-IT.
           MOVE OCJ-PATH TO E-FILE.
           MOVE OCJ1-KEY TO E-KEYX.
           START OCJ-FILE KEY NOT < OCJ1-KEY.
       READ-OCJ1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OCJ-PATH TO E-FILE.
           MOVE OCJ1-KEY TO E-KEYX.
           READ OCJ-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OCJ1-FILE-NEXT.
       CLOSE-OCJ1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OCJ-FILE.

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
              DESTROY CJFILE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
