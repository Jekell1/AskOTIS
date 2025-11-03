       IDENTIFICATION DIVISION.
       PROGRAM-ID.    LTPFLE.
       DATE-WRITTEN.  10-24-2023.
      ******************************************************************
      *
      * DESCRIPTION   : COMPARE S35 LTPFLE WITH A15 LTPFILE
      *                  PAYMENT SCHEDULE TRAILER
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
      * 01-08-2025 RMZ - ADDED 'BTRACK' - PROCESS TRACKING TO PROGRAM.
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
      * COPYMEMBER: LIBLP/LPFSLT
           SELECT OLT-FILE ASSIGN TO OLT-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OLT1-KEY
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  OLT-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A15SRC/LP01LT.CPY" REPLACING LEADING "LT" BY "OLT".

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/LTPFLE S35 01/23/24-07:40:06 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".
       01  OLT-PATH                    PIC X(35) VALUE SPACES.
       01  OLT-ALL-PATH.
           03  OLT-ALL-BASE     PIC X(9).
           03  FILLER           PIC X     VALUE "/".
           03  OLT-ALL-MACHINE  PIC XX.
           03  FILLER           PIC X     VALUE "/".
           03  OLT-ALL-DATA     PIC X(6).
           03  FILLER           PIC X(10) VALUE
               "/LP/LTFILE".

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
       01  WSS-AM-APP-NUMBER           PIC 9(09)     VALUE ZEROES.
       01  FILLER            REDEFINES WSS-AM-APP-NUMBER.
           03  WSS-AM-APPNO            PIC 9(08).
           03  WSS-AM-SEQNO            PIC 9(01).
       01  WSS-MA-APP-NUMBER REDEFINES WSS-AM-APP-NUMBER
                                       PIC 9(09).
       01  FILLER            REDEFINES WSS-AM-APP-NUMBER.
           03  WSS-MA-APPNO            PIC 9(08).
           03  WSS-MA-SEQNO            PIC 9(01).

       01  HOLD-LN-TILLNO       PIC 9(2).
       01  HOLD-LN-MESSAGE      PIC X(30).
       01  HOLD-LN-MESSAGEX REDEFINES HOLD-LN-MESSAGE.
           03  HOLD-LN-MESS-BYTE PIC X OCCURS 30.
       01  HOLD-BYTE            PIC X.
           88  VALID-SPECIAL-CHARS  VALUE "(" ")"
                              "/" "\" "-" "?" "+" "%" "#" "[" "]"
                              "!" "<" ">" "$" "*" "&" "." """"  "="
                              "'" "_" ":" "@" "," ";" "~" "|" "{" "}".

       01  MACHINE-R1.
           03  FILLER                  PIC X VALUE X"72".
           03  FILLER                  PIC 9 VALUE 1.
       01  MACHINE-R2.
           03  FILLER                  PIC X VALUE X"72".
           03  FILLER                  PIC 9 VALUE 2.
       01  OLD-ACCTNO                  PIC 9(8).
       01  FILLER REDEFINES OLD-ACCTNO.
           03  NEW-ACCTNO              PIC 9(6).
           03  NEW-SEQNO               PIC 99.

       01  MISC-CURSOR-STAT            PIC X(2) VALUE SPACES.
           88  MISC-CURSOR-STAT-GOOD            VALUE "00".
           88  MISC-CURSOR-STAT-BAD             VALUE "  " THRU "//"
                                                      "01" THRU "~~".

       01  PROCESS-STAT                PIC X(08)    VALUE SPACES.
           88  PROCESS-STAT-GOOD                    VALUE "00".
           88  PROCESS-STAT-BAD                     VALUE "  " THRU "//"
                                                         "01" THRU "~~".

       01  TOTAL-ERRORS-CTR            PIC 9(9)  VALUE 0.
       01  S35-RECORD-CTR              PIC 9(9)  VALUE 0.
       01  A15-RECORDS-PROCESSED       PIC 9(9)  VALUE 0.
           88  NO-RECORDS-FOUND                  VALUE ZEROES.

       01  STARTING-TIME               PIC X(08)     VALUE SPACES.
       01  SUB                         PIC 9(04)     VALUE ZEROES.

       01  SYSDATE-YYYYMMDD            PIC 9(8)     VALUE ZEROES.

       01  OLD-ACCTNO1                 PIC 9(8).
       01  FILLER REDEFINES OLD-ACCTNO1.
           03  NEW-ACCTNO1             PIC 9(6).
           03  FILLER                  PIC 99.

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
           03  BEG-ACCTNO              PIC 9(06).
           03  END-ACCTNO              PIC 9(06).
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
           03  H02-TITLE               PIC X(60) VALUE 
           "PAYMENT SCHEDULE TRAILER (LTPFILE) COMPARE A15 TO S35".

       01  HEADER-LINE-03.
           03  FILLER                PIC X(4) VALUE "BRNO".
           03  FILLER                PIC X(5) VALUE SPACES.
           03  FILLER                PIC X(6) VALUE "ACCTNO".
           03  FILLER                PIC X(5) VALUE SPACES.
           03  FILLER                PIC X(11) VALUE "DESCRIPTION".
           03  FILLER                PIC X(17) VALUE SPACES.
           03  FILLER                PIC X(3) VALUE "A15".
           03  FILLER                PIC X(19) VALUE SPACES.
           03  FILLER                PIC X(3) VALUE "S35".

      ******************************************************************
      *
      *    D E T A I L - L I N E - W O R K E R S
      *
      ******************************************************************
       01  DETAIL-LINE                        PIC X(80)   VALUE SPACES.

       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-BRNO                       PIC Z(3)9.
           03  FILLER                         PIC X(5).
           03  D01-ACCTNO                     PIC Z(7)9.
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
       COPY "LIBUT/LTPFLE_DEF.CPY".
       COPY "LIBUT/LTPFLE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/LTPFLE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE OLT-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE OLT-FILE.
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
           MOVE "LTPFLE" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE LTPFLE-QUEUE-POS TO Q-POS.
           MOVE LTPFLE-COPIES-POS TO C-POS.
           MOVE LTPFLE-PRU-POS   TO P-POS.
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
              GO TO ENTER-END-ACCTNO.

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


       ENTER-BEG-ACCTNO.

           MOVE LEGEND-BEG-ACCTNO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN060BEG-ACCTNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F3-KEY ) GO TO ENTER-ALL-ACCTNO.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-ACCTNO.

           MOVE VDUNUM0 TO BEG-ACCTNO.

      *-----------------------------------------------------------------
       ENTER-END-ACCTNO.

           MOVE LEGEND-END-ACCTNO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN060END-ACCTNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-ACCTNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-ACCTNO.

           MOVE VDUNUM0 TO END-ACCTNO.

           IF ( BEG-ACCTNO > END-ACCTNO )
              MOVE "INVALID ACCOUNT NUMBER RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-ACCTNO.

           GO TO ENTER-PARAMETERS-END.

      *-----------------------------------------------------------------
       ENTER-ALL-ACCTNO.

           MOVE "SNNN060BEG-ACCTNO."  TO VDU.
           MOVE ZEROES                TO BEG-ACCTNO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060END-ACCTNO."  TO VDU.
           MOVE 999999                TO END-ACCTNO VDUNUM0.
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

           MOVE "SNNN060BEG-ACCTNO."       TO VDU.
           MOVE BEG-ACCTNO                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060END-ACCTNO."       TO VDU.
           MOVE END-ACCTNO                 TO VDUNUM0.
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

           MOVE SOURCE-PATH  TO OLT-ALL-BASE.
           MOVE MACHINE-R1   TO OLT-ALL-MACHINE.
           MOVE BR-DATA-PATH TO OLT-ALL-DATA.
           MOVE OLT-ALL-PATH TO OLT-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              IF OLT-ALL-MACHINE = MACHINE-R1
                 MOVE MACHINE-R2 TO OLT-ALL-MACHINE
                 MOVE OLT-ALL-PATH TO OLT-PATH ACCESS-BUF
                 PERFORM ACCESS-CALL
                 IF ( STAT-BAD )
                    MOVE "MISSING A15 FILE" TO D01-TEXT
                    MOVE BR-NO        TO D01-BRNO
                    PERFORM WRITE-DETAIL-LINE-ERROR
                    GO TO PROCESS-BRANCH-DRIVER-END.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO LTP-PATH-OVERRIDE.

           PERFORM OPEN-LTP1-FILE.
           PERFORM OPEN-OLT1-FILE.

           MOVE BEG-ACCTNO TO NEW-ACCTNO.
           MOVE 01         TO NEW-SEQNO.
           MOVE OLD-ACCTNO TO OLT-ACCTNO.
           MOVE "P"        TO OLT-CODE.
           MOVE 0          TO OLT-SEQNO.

           PERFORM START-OLT1-FILE.

       PROCESS-BRANCH-NEXT-LTP.
           PERFORM READ-OLT1-FILE-NEXT.
           IF IO-BAD 
              GO TO PROCESS-BRANCH-DRIVER-END.

           PERFORM SEND-PROGRESS-INDICATOR.

           MOVE OLT-ACCTNO TO OLD-ACCTNO1.
           IF NEW-ACCTNO1 = 0
              GO TO PROCESS-BRANCH-NEXT-LTP. 

           IF OLT-CODE NOT = "P"
              GO TO PROCESS-BRANCH-NEXT-LTP.

           MOVE OLT-ACCTNO TO OLD-ACCTNO.
           IF ( NEW-ACCTNO < BEG-ACCTNO ) OR
              ( NEW-ACCTNO > END-ACCTNO )
              GO TO PROCESS-BRANCH-NEXT-LTP.

           ADD 1 TO A15-RECORDS-PROCESSED.

           PERFORM COMPARE-FIELDS.

           GO TO PROCESS-BRANCH-NEXT-LTP.

       PROCESS-BRANCH-DRIVER-END.

           PERFORM CLOSE-LTP1-FILE.

       PROCESS-BRANCH-DRIVER-EXIT.
           EXIT.

      ******************************************************************
       COMPARE-FIELDS SECTION.
      ******************************************************************

           MOVE LTP-PATH-OWNBR TO LTP-BRNO.
           MOVE OLT-ACCTNO     TO OLD-ACCTNO.
           MOVE NEW-ACCTNO     TO LTP-ACCTNO.
      * S35 ONLY HAS THE 1 SEQUENCE, READ IT FOR EACH A15 SEQUENCE
           MOVE 1              TO LTP-SEQNO.
           PERFORM READ-LTP1-FILE.
           IF IO-FG NOT = 0
              MOVE "MISSING S35 RECORD" TO D01-TEXT
              MOVE BR-NO        TO D01-BRNO
              MOVE LTP-ACCTNO   TO D01-ACCTNO
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.

           ADD 1 TO S35-RECORD-CTR.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 17
      *        IF (OLT-SCHLD-TERM(SUB) NOT NUMERIC)
      *           MOVE 0  TO OLT-SCHLD-TERM(SUB)
      *        END-IF
              IF OLT-SCHLD-TERM(SUB) NOT = LTP-SCHLD-TERM(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "LTP-SCHLD-TERM(SUB)"SUB INTO D01-TEXT
                 MOVE BR-NO         TO D01-BRNO
                 MOVE LTP-ACCTNO    TO D01-ACCTNO
                 MOVE OLT-SCHLD-TERM(SUB)  TO D01-A15-NUM
                 MOVE LTP-SCHLD-TERM(SUB)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
      *        IF (OLT-SCHLD-PAYMNT(SUB) NOT NUMERIC)
      *           MOVE 0  TO OLT-SCHLD-PAYMNT(SUB)
      *        END-IF
              IF OLT-SCHLD-PAYMNT(SUB) NOT = LTP-SCHLD-PAYMNT(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "LTP-SCHLD-PAYMNT(SUB)"SUB INTO D01-TEXT
                 MOVE BR-NO         TO D01-BRNO
                 MOVE LTP-ACCTNO    TO D01-ACCTNO
                 MOVE OLT-SCHLD-PAYMNT(SUB)  TO D01-A15-NUM
                 MOVE LTP-SCHLD-PAYMNT(SUB)   TO D01-S35-NUM
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

           MOVE "BEGINNING ACCOUNT NUMBER      : "    TO R01-PROMPT.
           MOVE BEG-ACCTNO                            TO R01-NUMERIC.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    ACCOUNT NUMBER      : "    TO R01-PROMPT.
           MOVE END-ACCTNO                            TO R01-NUMERIC.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "A15 RECORDS PROCESSED         :"     TO R01-PROMPT.
           MOVE A15-RECORDS-PROCESSED                     TO R01-NUMERIC.
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
           MOVE EXT-CONAME-CONAME TO LTPFLE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LTPFLE-SYSDATE.
           DISPLAY LTPFLE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE LTPFLE-FORM-FIELDS TO WR-BUF.
           MOVE LTPFLE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE LTPFLE-HELP--FILE TO HELP-LINK-FILE.
           MOVE LTPFLE-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/LTPFLE_EVA.CPY".
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
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       LOAD-LTP1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LTP-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LTP-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LTP-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LTP-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LTP-ALL-DATA
              MOVE LTP-ALL-PATH             TO LTP-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LTP-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LTP-ALL-BASE
              MOVE FILPATH-MACHINE         TO LTP-ALL-MACHINE
              MOVE FILPATH-DATA            TO LTP-ALL-DATA
              MOVE LTP-ALL-PATH             TO LTP-PATH.

      *-----------------------------------------------------------------
       OPEN-LTP1-FILE.
           PERFORM LOAD-LTP1-FILE.
           PERFORM OPEN-IT.
           MOVE LTP-PATH-SQL TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       READ-LTP1-FILE.
           PERFORM READ-IT.
           MOVE LTP-PATH-OWNBR  TO LTP-BRNO. 
           MOVE LTP-PATH-SQL    TO E-FILE.
           MOVE LTP1-KEY        TO E-KEYX.
           INITIALIZE QLTP-REC.

           MOVE LTP-BRNO   TO QLTP-BRNO.
           MOVE LTP-ACCTNO TO QLTP-ACCTNO.
           MOVE LTP-SEQNO  TO QLTP-SEQNO.

           EXEC SQL
            SELECT LTPFILE.LTP_BRNO,
                   LTPFILE.LTP_ACCTNO,
                   LTPFILE.LTP_SEQNO,
                   CAST(LTPFILE.LTP_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   LTPFILE.LTP_SCHLD_TERM_1,
                   LTPFILE.LTP_SCHLD_PAYMNT_1,
                   LTPFILE.LTP_SCHLD_TERM_2,
                   LTPFILE.LTP_SCHLD_PAYMNT_2,
                   LTPFILE.LTP_SCHLD_TERM_3,
                   LTPFILE.LTP_SCHLD_PAYMNT_3,
                   LTPFILE.LTP_SCHLD_TERM_4,
                   LTPFILE.LTP_SCHLD_PAYMNT_4,
                   LTPFILE.LTP_SCHLD_TERM_5,
                   LTPFILE.LTP_SCHLD_PAYMNT_5,
                   LTPFILE.LTP_SCHLD_TERM_6,
                   LTPFILE.LTP_SCHLD_PAYMNT_6,
                   LTPFILE.LTP_SCHLD_TERM_7,
                   LTPFILE.LTP_SCHLD_PAYMNT_7,
                   LTPFILE.LTP_SCHLD_TERM_8,
                   LTPFILE.LTP_SCHLD_PAYMNT_8,
                   LTPFILE.LTP_SCHLD_TERM_9,
                   LTPFILE.LTP_SCHLD_PAYMNT_9,
                   LTPFILE.LTP_SCHLD_TERM_10,
                   LTPFILE.LTP_SCHLD_PAYMNT_10,
                   LTPFILE.LTP_SCHLD_TERM_11,
                   LTPFILE.LTP_SCHLD_PAYMNT_11,
                   LTPFILE.LTP_SCHLD_TERM_12,
                   LTPFILE.LTP_SCHLD_PAYMNT_12,
                   LTPFILE.LTP_SCHLD_TERM_13,
                   LTPFILE.LTP_SCHLD_PAYMNT_13,
                   LTPFILE.LTP_SCHLD_TERM_14,
                   LTPFILE.LTP_SCHLD_PAYMNT_14,
                   LTPFILE.LTP_SCHLD_TERM_15,
                   LTPFILE.LTP_SCHLD_PAYMNT_15,
                   LTPFILE.LTP_SCHLD_TERM_16,
                   LTPFILE.LTP_SCHLD_PAYMNT_16,
                   LTPFILE.LTP_SCHLD_TERM_17,
                   LTPFILE.LTP_SCHLD_PAYMNT_17,
                   LTPFILE.LTP_SCHLD_TERM_18,
                   LTPFILE.LTP_SCHLD_PAYMNT_18,
                   LTPFILE.LTP_SCHLD_TERM_19,
                   LTPFILE.LTP_SCHLD_PAYMNT_19,
                   LTPFILE.LTP_SCHLD_TERM_20,
                   LTPFILE.LTP_SCHLD_PAYMNT_20,
                   LTPFILE.LTP_SCHLD_TERM_21,
                   LTPFILE.LTP_SCHLD_PAYMNT_21,
                   LTPFILE.LTP_SCHLD_TERM_22,
                   LTPFILE.LTP_SCHLD_PAYMNT_22,
                   LTPFILE.LTP_SCHLD_TERM_23,
                   LTPFILE.LTP_SCHLD_PAYMNT_23,
                   LTPFILE.LTP_SCHLD_TERM_24,
                   LTPFILE.LTP_SCHLD_PAYMNT_24,
                   LTPFILE.LTP_SCHLD_TERM_25,
                   LTPFILE.LTP_SCHLD_PAYMNT_25,
                   LTPFILE.LTP_SCHLD_TERM_26,
                   LTPFILE.LTP_SCHLD_PAYMNT_26,
                   LTPFILE.LTP_SCHLD_TERM_27,
                   LTPFILE.LTP_SCHLD_PAYMNT_27,
                   LTPFILE.LTP_SCHLD_TERM_28,
                   LTPFILE.LTP_SCHLD_PAYMNT_28,
                   LTPFILE.LTP_SCHLD_TERM_29,
                   LTPFILE.LTP_SCHLD_PAYMNT_29,
                   LTPFILE.LTP_SCHLD_TERM_30,
                   LTPFILE.LTP_SCHLD_PAYMNT_30,
                   LTPFILE.LTP_SCHLD_TERM_31,
                   LTPFILE.LTP_SCHLD_PAYMNT_31,
                   LTPFILE.LTP_SCHLD_TERM_32,
                   LTPFILE.LTP_SCHLD_PAYMNT_32,
                   LTPFILE.LTP_SCHLD_TERM_33,
                   LTPFILE.LTP_SCHLD_PAYMNT_33,
                   LTPFILE.LTP_SCHLD_TERM_34,
                   LTPFILE.LTP_SCHLD_PAYMNT_34,
                   LTPFILE.LTP_SCHLD_TERM_35,
                   LTPFILE.LTP_SCHLD_PAYMNT_35,
                   LTPFILE.LTP_SCHLD_TERM_36,
                   LTPFILE.LTP_SCHLD_PAYMNT_36,
                   LTPFILE.LTP_SCHLD_TERM_37,
                   LTPFILE.LTP_SCHLD_PAYMNT_37,
                   LTPFILE.LTP_SCHLD_TERM_38,
                   LTPFILE.LTP_SCHLD_PAYMNT_38,
                   LTPFILE.LTP_SCHLD_TERM_39,
                   LTPFILE.LTP_SCHLD_PAYMNT_39,
                   LTPFILE.LTP_SCHLD_TERM_40,
                   LTPFILE.LTP_SCHLD_PAYMNT_40,
                   LTPFILE.LTP_SCHLD_TERM_41,
                   LTPFILE.LTP_SCHLD_PAYMNT_41
            INTO :QLTP-REC
            FROM DBO.LTPFILE
            WHERE LTPFILE.LTP_BRNO = :QLTP-BRNO
              AND LTPFILE.LTP_ACCTNO = :QLTP-ACCTNO
              AND LTPFILE.LTP_SEQNO = :QLTP-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTP1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LTP-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LTP1-FILE.

           PERFORM CLOSE-IT.
      *     IF ( LTP1-CURSOR-STAT-GOOD )
      *        EXEC SQL
      *             CLOSE LTP1_CURSOR
      *        END-EXEC
      *        MOVE STAT---BAD TO LTP1-CURSOR-STAT.

       OPEN-OLT1-FILE.
           PERFORM OPEN-IT.
           MOVE OLT-PATH TO E-FILE.
           OPEN INPUT OLT-FILE.
           IF IO-FG = 8
              GO TO OPEN-OLT1-FILE.
           IF IO-FG = 7
              CLOSE OLT-FILE
              GO TO OPEN-OLT1-FILE.
      *READ-OLT1-FILE.
      *    PERFORM READ-IT.
      *    MOVE OLT-PATH TO E-FILE.
      *    MOVE OLT1-KEY TO E-KEYX.
      *    READ OLT-FILE.
      *    IF IO-FG = 8
      *       GO TO READ-OLT1-FILE.
       READ-OLT1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OLT-PATH TO E-FILE.
           MOVE OLT1-KEY TO E-KEYX.
           READ OLT-FILE NEXT.
           IF IO-FG = 8
             GO TO READ-OLT1-FILE-NEXT.
       START-OLT1-FILE.
           PERFORM START-IT.
           MOVE OLT-PATH TO E-FILE.
           MOVE OLT1-KEY TO E-KEYX.
           START OLT-FILE KEY NOT < OLT1-KEY.
       CLOSE-OLT1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OLT-FILE.

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
              DESTROY LTPFLE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
