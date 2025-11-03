       IDENTIFICATION DIVISION.
       PROGRAM-ID.    LEFILE.
       DATE-WRITTEN.  10-24-2023.
      ******************************************************************
      *
      * DESCRIPTION   : COMPARE S35 LEFILE WITH A15 LEFILE
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
      *  01-08-25  RMZ  ADDED REPORT LINE TO LET US KNOW THAT THERE 
      *                 WILL POSSIBLY BE NO RECORDS PROCESSED FOR 
      *                 BRANCHES 200-9999.  BRANCHES 10-199 ARE 
      *                 GEORGIA BRANCHES.
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

      * COPYMEMBER: LIBLP/LPFSLE
           SELECT OLE-FILE ASSIGN TO OLE-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OLE1-KEY
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  OLE-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A15SRC/LP01LE.CPY" REPLACING LEADING "LE" BY "OLE".

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/LEFILE S35 01/07/25-11:38:25 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LE.CPY".
       COPY "LIBLP/LP01LE_SQL.CPY".
       COPY "LIBLP/LPWSLE.CPY".

       01  OLE-PATH                    PIC X(35) VALUE SPACES.
       01  OLE-ALL-PATH.
           03  OLE-ALL-BASE     PIC X(9).
           03  FILLER           PIC X     VALUE "/".
           03  OLE-ALL-MACHINE  PIC XX.
           03  FILLER           PIC X     VALUE "/".
           03  OLE-ALL-DATA     PIC X(6).
           03  FILLER           PIC X(10) VALUE
               "/LP/LEFILE".

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

       01  WS-ACCTNO                PIC 9(8).
       01  WS-ACCTNOX REDEFINES WS-ACCTNO.
           03  WS-NUMBER            PIC 9(6).
           03  WS-SEQNO             PIC 99.
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

       01  LE-EOF-SW                   PIC X      VALUE "N".
       01  OLE-EOF-SW                  PIC X      VALUE "N".
       01  TOTAL-ERRORS-CTR            PIC 9(9)   VALUE 0.
       01  LE1-CONN-OPEN-SW            PIC X      VALUE "N".
       01  A15-FILE-FOUND-SW           PIC X      VALUE "N".

       01  USE-OLE-KEY.
           05  USE-OLE-KEY-BRNO        PIC 9(4).
           05  USE-OLE-KEY-ACCTNO      PIC 9(8).
           05  USE-OLE-KEY-TYPE        PIC X.
           05  USE-OLE-KEY-FLAG        PIC X.
           05  USE-OLE-KEY-SSNO        PIC 9(9).
           05  USE-OLE-KEY-OSACCTNO    PIC 9(8).

       01  MISC-CURSOR-STAT            PIC X(2) VALUE SPACES.
           88  MISC-CURSOR-STAT-GOOD            VALUE "00".
           88  MISC-CURSOR-STAT-BAD             VALUE "  " THRU "//"
                                                      "01" THRU "~~".

       01  PROCESS-STAT                PIC X(08)    VALUE SPACES.
           88  PROCESS-STAT-GOOD                    VALUE "00".
           88  PROCESS-STAT-BAD                     VALUE "  " THRU "//"
                                                         "01" THRU "~~".
       01  SQL-RECORDS-PROCESSED       PIC 9(9) VALUE 0.
       01  A15-RECORDS-PROCESSED       PIC 9(9) VALUE 0.
           88  NO-RECORDS-FOUND                     VALUE ZEROES.

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
           03  H02-TITLE               PIC X(50) VALUE 
           "LOAN ENDORSER FILE (LEFILE) COMPARE A15 TO S35".

       01  HEADER-LINE-03.
           03  FILLER                PIC X(4) VALUE "BRNO".
           03  FILLER                PIC X    VALUE SPACES.
           03  FILLER                PIC X(6) VALUE "ACCTNO".
           03  FILLER                PIC X(3) VALUE SPACES.
           03  FILLER                PIC X(6) VALUE "SSNO".
           03  FILLER                PIC X(4) VALUE SPACES.
           03  FILLER                PIC X(7) VALUE "OACCTNO".
           03  FILLER                PIC X(2) VALUE SPACES.
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
           03  FILLER                         PIC X.
           03  D01-ACCTNO                     PIC Z(6).
           03  FILLER                         PIC X.
           03  D01-SSNO                       PIC 9(9).
           03  FILLER                         PIC X.
           03  D01-OACCTNO                    PIC Z(6).
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
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBUT/LEFILE_DEF.CPY".
       COPY "LIBUT/LEFILE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/LEFILE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE OLE-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LE1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           PERFORM CLOSE-LE1-CONNECTION.
           CLOSE OLE-FILE.
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
           MOVE "LEFILE" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE LEFILE-QUEUE-POS TO Q-POS.
           MOVE LEFILE-COPIES-POS TO C-POS.
           MOVE LEFILE-PRU-POS   TO P-POS.
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
      D    STOP MESS.

           MOVE 9999   TO PROGID-SLEEP-COUNTER.
           PERFORM SEND-PROGRESS-INDICATOR.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.
           MOVE "B"                        TO LE-PATH-OVERRIDE.

           PERFORM OPEN-LE1-FILE.

           MOVE "N" TO LE-EOF-SW OLE-EOF-SW.

           MOVE BR-NO      TO QLE1-WBEG-BRNO.
           MOVE BEG-ACCTNO TO QLE1-WBEG-ACCTNO.
           MOVE END-ACCTNO TO QLE1-WEND-ACCTNO.

           PERFORM OPEN-LE1-FILE.
           PERFORM START-LE1-FILE.  *> SSP  LEFILE
           PERFORM M-READ-LE.

           MOVE "Y"          TO A15-FILE-FOUND-SW.
           MOVE SOURCE-PATH  TO OLE-ALL-BASE.
           MOVE MACHINE-R1   TO OLE-ALL-MACHINE.
           MOVE BR-DATA-PATH TO OLE-ALL-DATA.
           MOVE OLE-ALL-PATH TO OLE-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE MACHINE-R2   TO OLE-ALL-MACHINE
              MOVE OLE-ALL-PATH TO OLE-PATH ACCESS-BUF
              PERFORM ACCESS-CALL
              IF ( STAT-BAD )
                 MOVE "N" TO A15-FILE-FOUND-SW.

           IF A15-FILE-FOUND-SW = "N"
              MOVE ALL '9'  TO USE-OLE-KEY
              MOVE "Y"      TO OLE-EOF-SW
           ELSE
              PERFORM OPEN-OLE1-FILE
              MOVE SPACES TO OLE1-KEY
              MOVE BEG-ACCTNO  TO WS-NUMBER
              MOVE 1           TO WS-SEQNO
              MOVE WS-ACCTNO   TO OLE-ACCTNO
              MOVE 0           TO OLE-SSNO OLE-OSACCTNO
              PERFORM START-OLE1-FILE
              PERFORM M-READ-OLE.

           PERFORM PROCESS-BRANCH-NEXT-LE
              UNTIL LE-EOF-SW = "Y" AND OLE-EOF-SW = "Y".

           PERFORM CLOSE-LE1-FILE.
           PERFORM CLOSE-OLE1-FILE.

           GO TO PROCESS-BRANCH-DRIVER-END.

       PROCESS-BRANCH-NEXT-LE.

           PERFORM SEND-PROGRESS-INDICATOR.

           PERFORM COMPARE-FIELDS.

           IF USE-OLE-KEY < LE1-KEY
              PERFORM M-READ-OLE
           ELSE
              IF USE-OLE-KEY > LE1-KEY
                 PERFORM M-READ-LE
              ELSE
                 PERFORM M-READ-OLE
                 PERFORM M-READ-LE.

        PROCESS-BRANCH-DRIVER-END.
           EXIT.

       M-READ-LE SECTION.
           PERFORM READ-LE1-FILE-NEXT.   *> SSP  LEFILE
           IF IO-BAD
              MOVE ALL '9' TO LE1-KEY
              MOVE "Y"     TO LE-EOF-SW
           ELSE
              ADD 1 TO SQL-RECORDS-PROCESSED.

       EXIT-M-READ-LE.
           EXIT.

       M-READ-OLE SECTION.
           PERFORM READ-OLE1-FILE-NEXT.
           IF IO-BAD 
              MOVE ALL '9'  TO USE-OLE-KEY
              MOVE "Y"      TO OLE-EOF-SW
           ELSE
              MOVE OLE-ACCTNO TO WS-ACCTNO
              IF ( WS-NUMBER < BEG-ACCTNO ) OR
                 ( WS-NUMBER > END-ACCTNO )
                 GO TO M-READ-OLE
              END-IF
              IF OLE-TYPE NOT = "R"
                 GO TO M-READ-OLE
              END-IF 
              ADD 1             TO A15-RECORDS-PROCESSED
              MOVE BR-NO        TO USE-OLE-KEY-BRNO
              MOVE WS-NUMBER    TO USE-OLE-KEY-ACCTNO
              MOVE OLE-TYPE     TO USE-OLE-KEY-TYPE
              MOVE OLE-OSACCTNO TO WS-ACCTNO
              MOVE WS-NUMBER    TO USE-OLE-KEY-OSACCTNO
              IF OLE-TYPE = "E"
                 MOVE OLE-SSNO      TO USE-OLE-KEY-SSNO
                 MOVE SPACES        TO USE-OLE-KEY-FLAG
              ELSE
                 MOVE 0             TO USE-OLE-KEY-SSNO
                 MOVE OLE-FLAG      TO USE-OLE-KEY-FLAG.

       EXIT-M-READ-OLE1.
           EXIT.

      ******************************************************************
       COMPARE-FIELDS SECTION.
      ******************************************************************

           IF USE-OLE-KEY < LE1-KEY
              MOVE "MISSING S35 RECORD" TO D01-TEXT
              MOVE BR-NO        TO D01-BRNO
              MOVE OLE-ACCTNO   TO D01-ACCTNO
              MOVE OLE-SSNO     TO D01-SSNO
              MOVE OLE-OSACCTNO TO D01-OACCTNO
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.

           IF USE-OLE-KEY > LE1-KEY
              MOVE "MISSING A15 RECORD" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE LE-ACCTNO   TO D01-ACCTNO
              MOVE LE-SSNO     TO D01-SSNO
              MOVE LE-OSACCTNO TO D01-OACCTNO
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.

           IF OLE-TYPE NOT = "E"
              GO TO TEST-LE-R.

           IF OLE-OSRATE NOT NUMERIC
              MOVE 0               TO OLE-OSRATE.
           IF OLE-OSRATE NOT = LE-E-OSRATE
              MOVE "LE-OSRATE" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE LE-ACCTNO   TO D01-ACCTNO
              MOVE LE-SSNO     TO D01-SSNO
              MOVE LE-OSACCTNO TO D01-OACCTNO
              MOVE OLE-OSRATE  TO D01-A15-NUM
              MOVE LE-E-OSRATE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLE-OSAMT NOT NUMERIC
              MOVE 0               TO OLE-OSAMT.
           IF OLE-OSAMT NOT = LE-E-OSAMT
              MOVE "LE-OSAMT" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE LE-ACCTNO   TO D01-ACCTNO
              MOVE LE-SSNO     TO D01-SSNO
              MOVE LE-OSACCTNO TO D01-OACCTNO
              MOVE OLE-OSAMT  TO D01-A15-NUM
              MOVE LE-E-OSAMT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLE-OSDATE       TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LE-E-OSDATE
              MOVE "LE-OSDATE" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE LE-ACCTNO   TO D01-ACCTNO
              MOVE LE-SSNO     TO D01-SSNO
              MOVE LE-OSACCTNO TO D01-OACCTNO
              MOVE DATE-YYYYMMDD  TO D01-A15-NUM
              MOVE LE-E-OSDATE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLE-AMT NOT NUMERIC
              MOVE 0               TO OLE-AMT.
           IF OLE-AMT NOT = LE-E-AMT
              MOVE "LE-AMT" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE LE-ACCTNO   TO D01-ACCTNO
              MOVE LE-SSNO     TO D01-SSNO
              MOVE LE-OSACCTNO TO D01-OACCTNO
              MOVE OLE-AMT  TO D01-A15-NUM
              MOVE LE-E-AMT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLE-DATE       TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LE-E-DATE
              MOVE "LE-DATE" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE LE-ACCTNO   TO D01-ACCTNO
              MOVE LE-SSNO     TO D01-SSNO
              MOVE LE-OSACCTNO TO D01-OACCTNO
              MOVE DATE-YYYYMMDD  TO D01-A15-NUM
              MOVE LE-E-DATE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLE-OSMAKERCD NOT = LE-E-OSMAKERCD
              MOVE "LE-OSMAKERCD" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE LE-ACCTNO   TO D01-ACCTNO
              MOVE LE-SSNO     TO D01-SSNO
              MOVE LE-OSACCTNO TO D01-OACCTNO
              MOVE OLE-OSMAKERCD  TO D01-A15
              MOVE LE-E-OSMAKERCD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           GO TO EXIT-COMPARE-FIELDS.

       TEST-LE-R.
           MOVE OLE-LOANDATE       TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LE-R-LOANDATE
              MOVE "LE-LOANDATE" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE LE-ACCTNO   TO D01-ACCTNO
              MOVE LE-SSNO     TO D01-SSNO
              MOVE LE-OSACCTNO TO D01-OACCTNO
              MOVE DATE-YYYYMMDD  TO D01-A15-NUM
              MOVE LE-R-LOANDATE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLE-LNAMT NOT NUMERIC
              MOVE 0 TO OLE-LNAMT.
           IF OLE-LNAMT NOT = LE-R-LNAMT
              MOVE "LE-LNAMT" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE LE-ACCTNO   TO D01-ACCTNO
              MOVE LE-SSNO     TO D01-SSNO
              MOVE LE-OSACCTNO TO D01-OACCTNO
              MOVE OLE-LNAMT  TO D01-A15-NUM
              MOVE LE-R-LNAMT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLE-POFFDATE       TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LE-R-POFFDATE
              MOVE "LE-POFFDATE" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE LE-ACCTNO   TO D01-ACCTNO
              MOVE LE-SSNO     TO D01-SSNO
              MOVE LE-OSACCTNO TO D01-OACCTNO
              MOVE DATE-YYYYMMDD  TO D01-A15-NUM
              MOVE LE-R-POFFDATE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLE-RESTRICTED-MONEY NOT NUMERIC
              MOVE 0 TO OLE-RESTRICTED-MONEY.
           IF OLE-RESTRICTED-MONEY NOT = LE-R-RESTRICTED-MONEY
              MOVE "LE-RESTRICTED-MONEY" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE LE-ACCTNO   TO D01-ACCTNO
              MOVE LE-SSNO     TO D01-SSNO
              MOVE LE-OSACCTNO TO D01-OACCTNO
              MOVE OLE-RESTRICTED-MONEY  TO D01-A15-NUM
              MOVE LE-R-RESTRICTED-MONEY   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLE-BALANCE NOT NUMERIC
              MOVE 0 TO OLE-BALANCE.
           IF OLE-BALANCE NOT = LE-R-BALANCE
              MOVE "LE-BALANCE" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE LE-ACCTNO   TO D01-ACCTNO
              MOVE LE-SSNO     TO D01-SSNO
              MOVE LE-OSACCTNO TO D01-OACCTNO
              MOVE OLE-BALANCE  TO D01-A15-NUM
              MOVE LE-R-BALANCE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLE-SALEFIN NOT = LE-R-SALEFIN
              MOVE "LE-SALEFIN" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE LE-ACCTNO   TO D01-ACCTNO
              MOVE LE-SSNO     TO D01-SSNO
              MOVE LE-OSACCTNO TO D01-OACCTNO
              MOVE OLE-SALEFIN  TO D01-A15
              MOVE LE-R-SALEFIN   TO D01-S35
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

           MOVE "** FOR BRANCHES 200-9999,  **"       TO R01-PROMPT.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "** IT IS POSSIBLE TO HAVE  **"       TO R01-PROMPT.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "** NO RECORDS TO PROCESS.  **"       TO R01-PROMPT.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "                             "       TO R01-PROMPT.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( GROUP-FLAG = "Y" )
              MOVE "BEGINNING BRANCH              : " TO R01-PROMPT
              MOVE ENT-GROUP                          TO R01-TEXT
              MOVE 02                                 TO LN-FEED
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
           MOVE EXT-CONAME-CONAME TO LEFILE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LEFILE-SYSDATE.
           DISPLAY LEFILE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE LEFILE-FORM-FIELDS TO WR-BUF.
           MOVE LEFILE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE LEFILE-HELP--FILE TO HELP-LINK-FILE.
           MOVE LEFILE-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/LEFILE_EVA.CPY".
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
       COPY "LIBLP/LPLEGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

      * COPY "LIBLP/LPLE1RN.CPY".
      * COPYMEMBER: LIBLP/LPLE1RN
       LOAD-LE1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LE-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LE-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LE-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LE-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LE-ALL-DATA
              MOVE LE-ALL-PATH             TO LE-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LE-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LE-ALL-BASE
              MOVE FILPATH-MACHINE         TO LE-ALL-MACHINE
              MOVE FILPATH-DATA            TO LE-ALL-DATA
              MOVE LE-ALL-PATH             TO LE-PATH.

      *-----------------------------------------------------------------
       OPEN-LE1-FILE.
           PERFORM LOAD-LE1-FILE.
           PERFORM OPEN-IT.
           MOVE LE-PATH-SQL TO E-FILE.

           IF LE1-CONN-OPEN-SW = "N" 
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS LE1_CONNECT
                        USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE 'Y' TO LE1-CONN-OPEN-SW.

      *-----------------------------------------------------------------
       START-LE1-FILE.
           PERFORM START-IT.
           MOVE LE-PATH-OWNBR  TO LE-BRNO.
           MOVE LE-PATH-SQL    TO E-FILE.
           MOVE LE1-KEY        TO E-KEYX.

           IF ( LE1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LE1-FILE.

      *      SELECT *
      *      FROM DBO.CHFILE
      *      WHERE LEFILE.LE_BRNO  = :QLE1-WBEG-BRNO
      *        AND LEFILE.LE_ACCTNO BETWEEN @QLE_WBEG_ACCTNO AND @QLE_WEND_ACCTNO
      *      ORDER BY LEFILE.LE_BRNO,
      *               LEFILE.LE_ACCTNO,
      *               LEFILE.LE_TYPE,
      *               LEFILE.LE_FLAG,
      *               LEFILE.LE_SSNO,
      *               LEFILE.LE_OSACCTNO

           EXEC SQL SET CONNECTION LE1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE LE1_CURSOR CURSOR FOR
                :LE1-RETURN-CODE =  
            EXEC SSP_LEFILE_LEFILE_SELECT
              ( :QLE1-WBEG-BRNO, :QLE1-WBEG-ACCTNO, :QLE1-WEND-ACCTNO)
           END-EXEC.

           EXEC SQL
               OPEN LE1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           MOVE STAT---GOOD TO LE1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LE1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LE-PATH-OWNBR  TO LE-BRNO.
           MOVE LE-PATH-SQL    TO E-FILE.
           MOVE LE1-KEY        TO E-KEYX.
           INITIALIZE QLE-REC.

           EXEC SQL SET CONNECTION LE1_CONNECT END-EXEC.

           IF ( LE1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LE1_CURSOR INTO :QLE-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LE1-FILE-NEXT.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LE1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LE-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LE1-FILE.

           PERFORM CLOSE-IT.

           IF ( LE1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION LE1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE LE1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO LE1-CURSOR-STAT.
      *-----------------------------------------------------------------
       CLOSE-LE1-CONNECTION.

           IF LE1-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION LE1_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT LE1_CONNECT END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE "N" TO LE1-CONN-OPEN-SW.
      *-----------------------------------------------------------------


       OPEN-OLE1-FILE.
           PERFORM OPEN-IT.
           MOVE OLE-PATH TO E-FILE.
           OPEN INPUT OLE-FILE.
           IF IO-FG = 8
              GO TO OPEN-OLE1-FILE.
           IF IO-FG = 7
              CLOSE OLE-FILE
              GO TO OPEN-OLE1-FILE.
      *READ-OLE1-FILE.
      *    PERFORM READ-IT.
      *    MOVE OLE-PATH TO E-FILE.
      *    MOVE OLE1-KEY TO E-KEYX.
      *    READ OLE-FILE.
      *    IF IO-FG = 8
      *       GO TO READ-OLE1-FILE.
       START-OLE1-FILE.
           PERFORM START-IT.
           MOVE OLE-PATH TO E-FILE.
           MOVE OLE1-KEY TO E-KEYX.
           START OLE-FILE KEY NOT < OLE1-KEY.
       READ-OLE1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OLE-PATH TO E-FILE.
           MOVE OLE1-KEY TO E-KEYX.
           READ OLE-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OLE1-FILE-NEXT.
       CLOSE-OLE1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OLE-FILE.

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
              DESTROY LEFILE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
