       IDENTIFICATION DIVISION.
       PROGRAM-ID.    LNFILE.
       DATE-WRITTEN.  10-24-2023.
      ******************************************************************
      *
      * DESCRIPTION   : COMPARE S35 LNFILE WITH A15 LNFILE
      *            
      *                 TO BE USED AFTER AN UPDATE
      *                 WILL COMPARE EACH FIELD AND REPORT ANY 
      *                 DIFFERENCES, AS LONG AS FIELD EXIST IN BOTH FILES
      *
      *  2 FILE MATCH PROCESSING COMPARING A15 AND S35 BY BRANCH
      *
      * * * NEED TO TAKE OUT THE C$JUSTIFY STATEMENTS AFTER BRIAN REMOVES
      *     THE "TRIM" STATEMENT FROM THE ASCII LOAD
      * * *
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT OLN-FILE ASSIGN TO OLN-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OLN1-KEY
                  ALTERNATE RECORD KEY OLN2-KEY WITH DUPLICATES
                  ALTERNATE RECORD KEY OLN3-KEY WITH DUPLICATES
                  ALTERNATE RECORD KEY OLN4-KEY WITH DUPLICATES
                  ALTERNATE RECORD KEY OLN5-KEY WITH DUPLICATES
                  ALTERNATE RECORD KEY OLN6-KEY WITH DUPLICATES
                  ALTERNATE RECORD KEY OLN7-KEY WITH DUPLICATES
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  OLN-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A15SRC/LP01LN.CPY" REPLACING LEADING "LN" BY "OLN".

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/LNFILU S35 05/17/24-05:41:40 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       01  OLN-PATH                    PIC X(35) VALUE SPACES.
       01  OLN-ALL-PATH.
           03  OLN-ALL-BASE     PIC X(9).
           03  FILLER           PIC X     VALUE "/".
           03  OLN-ALL-MACHINE  PIC XX.
           03  FILLER           PIC X     VALUE "/".
           03  OLN-ALL-DATA     PIC X(6).
           03  FILLER           PIC X(10) VALUE
               "/LP/LNFILE".

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

       01  LN-EOF-SW                   PIC X      VALUE "N".
       01  OLN-EOF-SW                  PIC X      VALUE "N".
       01  LN1-CONN-OPEN-SW            PIC X      VALUE "N".

       01  OLN-USE-KEY.
           03 OLN-USE-KEY-BRNO         PIC 9(4).
           03 OLN-USE-KEY-ACCT         PIC 9(8).

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

       01  SQL-RECORDS-PROCESSED           PIC 9(9) VALUE 0.
       01  A15-RECORDS-PROCESSED           PIC 9(9) VALUE 0.
       01  TOTAL-ERRORS-CTR                PIC 9(9) VALUE 0.
       01  REWRITE-CTR                     PIC 9(9) VALUE 0.

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
           03  FILLER PIC X(30) VALUE "ER BEGINNING ACCOUNT          ".
           03  FILLER PIC X(19) VALUE "                   ".

       01  LEGEND-END-ACCTNO.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER ENDIN".
           03  FILLER PIC X(30) VALUE "G ACCOUNT                     ".
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
           03  H02-TITLE               PIC X(40) VALUE 
           "LOAN FILE (LNFILE) COMPARE A15 TO S35".

       01  HEADER-LINE-03.
           03  FILLER                PIC X(4) VALUE "BRNO".
           03  FILLER                PIC X(3) VALUE SPACES.
           03  FILLER                PIC X(8) VALUE " ACCTNO ".
           03  FILLER                PIC X(3) VALUE SPACES.
           03  FILLER                PIC X(28) VALUE "DESCRIPTION".
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
           03  FILLER                         PIC X(2).
           03  D01-ACCTNO                     PIC Z(7)9.
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
       COPY "LIBUT/LNFILE_DEF.CPY".
       COPY "LIBUT/LNFILE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/LNFILE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE OLN-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LN1-CONNECTION.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           PERFORM SQL-CLOSE-UPDATE-CONNECTION.
           CLOSE OLN-FILE.
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

           PERFORM EXECUTION-MODULE.

           PERFORM RANGE-LINES.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

           MOVE STAT---GOOD                TO PROCESS-STAT.

           MOVE FORM-PATHNAME              TO FORM-PATH.
           MOVE "LNFILE" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE LNFILE-QUEUE-POS TO Q-POS.
           MOVE LNFILE-COPIES-POS TO C-POS.
           MOVE LNFILE-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           PERFORM GET-TIME.
           MOVE TIME-EDIT                  TO STARTING-TIME.

           MOVE 0 TO REWRITE-CTR. 
           PERFORM SQL-OPEN-UPDATE-CONNECTION.

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

           PERFORM PROCESS-BRANCH-DRIVER.

           GO TO EXECUTION-NEXT-GROUP.

       EXECUTION-MODULE-EXIT.
           EXIT.

     *********************************************************
       PROCESS-BRANCH-DRIVER SECTION.
     *********************************************************

           MOVE 9999                       TO PROGID-SLEEP-COUNTER.
           PERFORM SEND-PROGRESS-INDICATOR.

           MOVE "N"  TO LN-EOF-SW OLN-EOF-SW.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO LN-PATH-OVERRIDE.

           MOVE BR-NO      TO QLN1-WBEG-OWNBR QLN1-WEND-OWNBR.
           MOVE BEG-ACCTNO TO QLN1-WBEG-ACCTNO.
           MOVE END-ACCTNO TO QLN1-WEND-ACCTNO.

           PERFORM OPEN-LN1-FILE.
           PERFORM START-LN1-FILE.    *> SSP LNFILE
           PERFORM M-READ-LN1.

      D    STOP MESS.
           MOVE SOURCE-PATH  TO OLN-ALL-BASE.
           MOVE MACHINE-R1   TO OLN-ALL-MACHINE.
           MOVE BR-DATA-PATH TO OLN-ALL-DATA.
           MOVE OLN-ALL-PATH TO OLN-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE MACHINE-R2 TO OLN-ALL-MACHINE
              MOVE OLN-ALL-PATH TO OLN-PATH ACCESS-BUF
              PERFORM ACCESS-CALL
              IF ( STAT-BAD )
                 MOVE "MISSING A15 FILE" TO D01-TEXT 
                 MOVE OLN-ALL-PATH       TO D01-A15
                 MOVE SPACES             TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR
                 GO TO PROCESS-BRANCH-DRIVER-END.

           MOVE BEG-ACCTNO TO OLN-NUMBER.
           MOVE 1          TO OLN-SEQNO.
           PERFORM OPEN-OLN1-FILE.
           PERFORM START-OLN1-FILE. 
           PERFORM M-READ-OLN.

           PERFORM PROCESS-BRANCH-NEXT-LN
              UNTIL OLN-EOF-SW = "Y" AND LN-EOF-SW = "Y".

           PERFORM CLOSE-OLN1-FILE.
           PERFORM CLOSE-LN1-FILE.

           GO TO PROCESS-BRANCH-DRIVER-END.

       PROCESS-BRANCH-NEXT-LN.

           PERFORM SEND-PROGRESS-INDICATOR.

           PERFORM COMPARE-FIELDS.

           IF OLN-USE-KEY < LN1-KEY
              PERFORM M-READ-OLN 
           ELSE
              IF OLN-USE-KEY > LN1-KEY
                 PERFORM M-READ-LN1
              ELSE
                 PERFORM M-READ-OLN
                 PERFORM M-READ-LN1.

       PROCESS-BRANCH-DRIVER-END.
           EXIT.

       M-READ-LN1 SECTION.
           PERFORM READ-LN1-FILE-NEXT.   *> SSP LNFILE
           IF IO-BAD
              MOVE ALL '9' TO LN1-KEY
              MOVE "Y"     TO LN-EOF-SW
           ELSE
              ADD 1 TO SQL-RECORDS-PROCESSED.

       EXIT-M-READ-LN1.
           EXIT.

       M-READ-OLN SECTION.
           PERFORM READ-OLN1-FILE-NEXT.
           IF IO-BAD
              MOVE ALL '9'  TO OLN-USE-KEY
              MOVE "Y"      TO OLN-EOF-SW
              GO TO EXIT-M-READ-OLN1.
 
           MOVE OLN-NUMBER  TO OLD-ACCTNO1.
           IF NEW-ACCTNO1 = 0
              GO TO M-READ-OLN.       
           
           IF (OLN-NUMBER < BEG-ACCTNO) OR (OLN-NUMBER > END-ACCTNO)
              GO TO M-READ-OLN
           ELSE
              MOVE OLN-OWNBR  TO OLN-USE-KEY-BRNO
              MOVE OLN-NUMBER TO OLN-USE-KEY-ACCT
              ADD 1 TO A15-RECORDS-PROCESSED.

       EXIT-M-READ-OLN1.
           EXIT.
 
      ******************************************************************
       COMPARE-FIELDS SECTION.
      ******************************************************************

            IF OLN-USE-KEY < LN1-KEY
              MOVE "MISSING S35 RECORD" TO D01-TEXT
              MOVE OLN-USE-KEY-BRNO     TO D01-BRNO
              MOVE OLN-USE-KEY-ACCT     TO D01-ACCTNO
              PERFORM WRITE-DETAIL-LINE-ERROR 
              GO TO EXIT-COMPARE-FIELDS.

           IF OLN-USE-KEY > LN1-KEY
              MOVE "MISSING A15 RECORD" TO D01-TEXT
              MOVE LN-OWNBR   TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.

           IF OLN-CLASS NOT NUMERIC
              MOVE 0 TO OLN-CLASS.
           IF LN-CLASS NOT = OLN-CLASS
              MOVE "LN-CLASS" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-CLASS  TO D01-A15
              MOVE LN-CLASS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-SUBCLASS NOT NUMERIC
              MOVE 0 TO OLN-SUBCLASS.
           IF LN-SUBCLASS NOT = OLN-SUBCLASS
              MOVE "LN-SUBCLASS" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-SUBCLASS  TO D01-A15
              MOVE LN-SUBCLASS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-LAWCODE NOT NUMERIC
              MOVE 0 TO OLN-LAWCODE.
           IF LN-LAWCODE NOT = OLN-LAWCODE
              MOVE "LN-LAWCODE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-LAWCODE  TO D01-A15
              MOVE LN-LAWCODE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 4
              IF OLN-SSNO(SUB) NOT NUMERIC
                 MOVE 0 TO OLN-SSNO(SUB)
              END-IF
              IF LN-SSNO(SUB) NOT = OLN-SSNO(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "LN-SSNO",SUB INTO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE LN-ACCTNO  TO D01-ACCTNO
                 MOVE OLN-SSNO(SUB)  TO D01-A15
                 MOVE LN-SSNO(SUB)   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF LN-MAKERCD(SUB) NOT = OLN-MAKERCD(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "LN-MAKERCD",SUB INTO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE LN-ACCTNO  TO D01-ACCTNO
                 MOVE OLN-MAKERCD(SUB)  TO D01-A15
                 MOVE LN-MAKERCD(SUB)   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.

           IF LN-ORGST NOT = OLN-ORGST
              MOVE "LN-ORGST" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-ORGST  TO D01-A15
              MOVE LN-ORGST   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-ORGBR NOT NUMERIC
              MOVE 0 TO OLN-ORGBR.
           IF LN-ORGBR NOT = OLN-ORGBR
              MOVE "LN-ORGBR" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-ORGBR  TO D01-A15
              MOVE LN-ORGBR   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-OWNST NOT = OLN-OWNST
              MOVE "LN-OWNST" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-OWNST  TO D01-A15
              MOVE LN-OWNST   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-SPRCLASS NOT NUMERIC
              MOVE 0 TO OLN-SPRCLASS.
           IF LN-SPRCLASS NOT = OLN-SPRCLASS
              MOVE "LN-SPRCLASS" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-SPRCLASS  TO D01-A15
              MOVE LN-SPRCLASS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-CONVERCD NOT = OLN-CONVERCD
              MOVE "LN-CONVERCD" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-CONVERCD  TO D01-A15
              MOVE LN-CONVERCD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-MIDSTREAM NOT = OLN-MIDSTREAM
              MOVE "LN-MIDSTREAM" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-MIDSTREAM  TO D01-A15
              MOVE LN-MIDSTREAM   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-PURCD NOT = OLN-PURCD
              MOVE "LN-PURCD" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-PURCD  TO D01-A15
              MOVE LN-PURCD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-SRCD NOT = OLN-SRCD
              MOVE "LN-SRCD" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-SRCD  TO D01-A15
              MOVE LN-SRCD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-COLLCD NOT = OLN-COLLCD
              MOVE "LN-COLLCD" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-COLLCD  TO D01-A15
              MOVE LN-COLLCD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-COLLID NOT = OLN-COLLID
      *       CALL "C$JUSTIFY" USING OLN-COLLID, "L"
              IF LN-COLLID NOT = OLN-COLLID
              MOVE "LN-COLLID" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-COLLID  TO D01-A15
              MOVE LN-COLLID   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-POCD NOT = OLN-POCD
              MOVE "LN-POCD" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-POCD  TO D01-A15
              MOVE LN-POCD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-PLCD NOT = OLN-PLCD
              MOVE "LN-PLCD" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-PLCD  TO D01-A15
              MOVE LN-PLCD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-PLTYPE NOT = OLN-PLTYPE
              MOVE "LN-PLTYPE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-PLTYPE  TO D01-A15
              MOVE LN-PLTYPE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-CONFIDENTCD NOT = OLN-CONFIDENTCD
              MOVE "LN-CONFIDENTCD" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-CONFIDENTCD  TO D01-A15
              MOVE LN-CONFIDENTCD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-ALLOTCD NOT = OLN-ALLOTCD
              MOVE "LN-ALLOTCD" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-ALLOTCD  TO D01-A15
              MOVE LN-ALLOTCD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-RECASTCD NOT = OLN-RECASTCD
              MOVE "LN-RECASTCD" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-RECASTCD  TO D01-A15
              MOVE LN-RECASTCD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-SKIP-TRW NOT = OLN-SKIP-TRW
              MOVE "LN-SKIP-TRW" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-SKIP-TRW  TO D01-A15
              MOVE LN-SKIP-TRW   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-STATUSFG NOT = OLN-STATUSFG
              MOVE "LN-STATUSFG" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-STATUSFG  TO D01-A15
              MOVE LN-STATUSFG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-SMALLOAN NOT = OLN-SMALLOAN
              MOVE "LN-SMALLOAN" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-SMALLOAN  TO D01-A15
              MOVE LN-SMALLOAN   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-SALEFIN NOT = OLN-SALEFIN
              MOVE "LN-SALEFIN" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-SALEFIN  TO D01-A15
              MOVE LN-SALEFIN   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-SERVACCT NOT = OLN-SERVACCT
              MOVE "LN-SERVACCT" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-SERVACCT  TO D01-A15
              MOVE LN-SERVACCT   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-LOANTYPE NOT = OLN-LOANTYPE
              MOVE "LN-LOANTYPE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-LOANTYPE  TO D01-A15
              MOVE LN-LOANTYPE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-ADDON-FG NOT = OLN-ADDON-FG
              MOVE "LN-ADDON-FG" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-ADDON-FG  TO D01-A15
              MOVE LN-ADDON-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-PAY-SCHLD-FG NOT = OLN-PAY-SCHLD-FG
              MOVE "LN-PAY-SCHLD-FG" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-PAY-SCHLD-FG  TO D01-A15
              MOVE LN-PAY-SCHLD-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-LC-CODE NOT NUMERIC
              MOVE 0 TO OLN-LC-CODE.
           IF LN-LC-CODE NOT = OLN-LC-CODE
              MOVE "LN-LC-CODE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-LC-CODE  TO D01-A15
              MOVE LN-LC-CODE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-CAN-LOAN-NO NOT = OLN-CAN-LOAN-NO
              MOVE "LN-CAN-LOAN-NO" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-CAN-LOAN-NO  TO D01-A15
              MOVE LN-CAN-LOAN-NO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-STATFLAGS NOT = OLN-STATFLAGS
      *       CALL "C$JUSTIFY" USING OLN-STATFLAGS, "L"
              IF LN-STATFLAGS NOT = OLN-STATFLAGS
              MOVE "LN-STATFLAGS" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-STATFLAGS  TO D01-A15
              MOVE LN-STATFLAGS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLN-ENTDATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LN-ENTDATE
              MOVE "LN-ENTDATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE LN-ENTDATE     TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLN-LOANDATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LN-LOANDATE
              MOVE "LN-LOANDATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE LN-LOANDATE     TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLN-PURDATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LN-PURDATE
              MOVE "LN-PURDATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE LN-PURDATE     TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLN-INTDATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LN-INTDATE
              MOVE "LN-INTDATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE LN-INTDATE     TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLN-CASHDATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LN-CASHDATE
              MOVE "LN-CASHDATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE LN-CASHDATE     TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLN-FILEDATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF OLN-RESCISSION = "Y"
               IF DATE-YYYYMMDD NOT = LN-RESCISSION-DATE
                  MOVE "LN-RESISSION-DATE" TO D01-TEXT
                  MOVE BR-NO      TO D01-BRNO
                  MOVE LN-ACCTNO  TO D01-ACCTNO
                  MOVE DATE-YYYYMMDD  TO D01-A15
                  MOVE LN-RESCISSION-DATE TO D01-S35
                  PERFORM WRITE-DETAIL-LINE-ERROR
           ELSE
              IF DATE-YYYYMMDD NOT = LN-FILEDATE
                 MOVE "LN-FILEDATE" TO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE LN-ACCTNO  TO D01-ACCTNO
                 MOVE DATE-YYYYMMDD  TO D01-A15
                 MOVE LN-FILEDATE     TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-ORIG-1STPAY NOT NUMERIC OR
              OLN-ORIG-1STPAY = 0
              MOVE OLN-LOANDATE TO OLN-ORIG-1STPAY.
           MOVE OLN-ORIG-1STPAY TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LN-ORIG-1STPYDATE
              MOVE "LN-ORIG-1STPYDATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-ORIG-1STPAY  TO D01-A15
              MOVE LN-ORIG-1STPYDATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-1STPYDATE NOT NUMERIC OR
              OLN-1STPYDATE = 0
              MOVE OLN-LOANDATE TO OLN-1STPYDATE.
           MOVE OLN-1STPYDATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LN-1STPYDATE
              MOVE "LN-1STPYDATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-1STPYDATE  TO D01-A15
              MOVE LN-1STPYDATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLN-JDDATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LN-JDDATE
              MOVE "LN-JDDATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-JDDATE  TO D01-A15
              MOVE LN-JDDATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLN-PLDATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LN-PLDATE
              MOVE "LN-PLDATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-PLDATE  TO D01-A15
              MOVE LN-PLDATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLN-ACCELDATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LN-ACCELDATE
              MOVE "LN-ACCELDATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-ACCELDATE  TO D01-A15
              MOVE LN-ACCELDATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLN-REDUDATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LN-REDUDATE
              MOVE "LN-REDUDATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-REDUDATE  TO D01-A15
              MOVE LN-REDUDATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLN-POFFDATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LN-POFFDATE
              MOVE "LN-POFFDATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-POFFDATE  TO D01-A15
              MOVE LN-POFFDATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLN-LEGALDATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LN-LEGALDATE
              MOVE "LN-LEGALDATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-LEGALDATE  TO D01-A15
              MOVE LN-LEGALDATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLN-BNKRPTDATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LN-BNKRPTDATE
              MOVE "LN-BNKRPTDATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-BNKRPTDATE  TO D01-A15
              MOVE LN-BNKRPTDATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

      * A15 OLN-BNKRPT-DISMISSAL IS A REDEFINE OF OLN-CYCLE-DATE
            IF OLN-BNKRPT-DISMISSAL NUMERIC
               IF OLN-BNKRPT-DISMISSAL NOT = 202020
                                   AND NOT = SPACES
                  MOVE OLN-BNKRPT-DISMISSAL TO DATE-MMDDYY
                  PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                  PERFORM TEST-INVALID-DATE
                  IF DATE-YYYYMMDD NOT = LN-BNKRPT-DISMISS-DATE
                     MOVE "LN-BNKRPT-DISMISS-DATE" TO D01-TEXT
                     MOVE BR-NO      TO D01-BRNO
                     MOVE LN-ACCTNO  TO D01-ACCTNO
                     MOVE OLN-BNKRPT-DISMISSAL  TO D01-A15
                     MOVE LN-BNKRPT-DISMISS-DATE   TO D01-S35
                     PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLN-DEBTAIDATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LN-DEBTAIDATE
              MOVE "LN-DEBTAIDATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-DEBTAIDATE  TO D01-A15
              MOVE LN-DEBTAIDATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLN-REPODATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LN-REPODATE
              MOVE "LN-REPODATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-REPODATE  TO D01-A15
              MOVE LN-REPODATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLN-REDEEMDATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LN-REDEEMDATE
              MOVE "LN-REDEEMDATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-REDEEMDATE  TO D01-A15
              MOVE LN-REDEEMDATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLN-SOLDDATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LN-SOLDDATE
              MOVE "LN-SOLDDATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-SOLDDATE  TO D01-A15
              MOVE LN-SOLDDATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLN-INS-CONFIRMATION TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LN-INS-CONFIRM-DATE
              MOVE "LN-INS-CONFIRM-DATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-INS-CONFIRMATION  TO D01-A15
              MOVE LN-INS-CONFIRM-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-APRATE NOT NUMERIC
              IF OLN-EFFRATE NUMERIC
                 MOVE OLN-EFFRATE TO OLN-APRATE
              ELSE
                 MOVE 0 TO OLN-APRATE.
           IF LN-APRATE NOT = OLN-APRATE
              MOVE "LN-APRATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-APRATE  TO D01-A15-NUM
              MOVE LN-APRATE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-EFFRATE NOT NUMERIC
              MOVE 0 TO OLN-EFFRATE.
           IF LN-EFFRATE NOT = OLN-EFFRATE
              MOVE "LN-EFFRATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-EFFRATE  TO D01-A15-NUM
              MOVE LN-EFFRATE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-SMPRATE NOT NUMERIC
              MOVE 0 TO OLN-SMPRATE.
           IF LN-SMPRATE NOT = OLN-SMPRATE
              MOVE "LN-SMPRATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-SMPRATE  TO D01-A15-NUM
              MOVE LN-SMPRATE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-YLDRATE NOT NUMERIC
              MOVE 0 TO OLN-YLDRATE.
           IF LN-YLDRATE NOT = OLN-YLDRATE
              MOVE "LN-YLDRATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-YLDRATE  TO D01-A15-NUM
              MOVE LN-YLDRATE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-OVRDRATE NOT NUMERIC
              MOVE 0 TO OLN-OVRDRATE.
           IF LN-OVRDRATE NOT = OLN-OVRDRATE
              MOVE "LN-OVRDRATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-OVRDRATE  TO D01-A15-NUM
              MOVE LN-OVRDRATE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-ENDORATE NOT NUMERIC
              MOVE 0 TO OLN-ENDORATE.
           IF LN-ENDORATE NOT = OLN-ENDORATE
              MOVE "LN-ENDORATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-ENDORATE  TO D01-A15-NUM
              MOVE LN-ENDORATE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-JDRATE NOT NUMERIC
              MOVE 0 TO OLN-JDRATE.
           IF LN-JDRATE NOT = OLN-JDRATE
              MOVE "LN-JDRATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-JDRATE  TO D01-A15-NUM
              MOVE LN-JDRATE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-ACCRATE NOT NUMERIC
              MOVE 0 TO OLN-ACCRATE.
           IF LN-ACCRATE NOT = OLN-ACCRATE
              MOVE "LN-ACCRATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-ACCRATE  TO D01-A15-NUM
              MOVE LN-ACCRATE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-REDURATE NOT NUMERIC
              MOVE 0 TO OLN-REDURATE.
           IF LN-REDURATE NOT = OLN-REDURATE
              MOVE "LN-REDURATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-REDURATE  TO D01-A15-NUM
              MOVE LN-REDURATE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-SERVPER NOT NUMERIC
              MOVE 0 TO OLN-SERVPER.
           IF LN-SERVPER NOT = OLN-SERVPER
              MOVE "LN-SERVPER" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-SERVPER  TO D01-A15-NUM
              MOVE LN-SERVPER   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-SELLPR NOT NUMERIC
              MOVE 0 TO OLN-SELLPR.
           IF LN-SELLPR NOT = OLN-SELLPR
              MOVE "LN-SELLPR" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-SELLPR  TO D01-A15-NUM
              MOVE LN-SELLPR   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-DOWNPY NOT NUMERIC
              MOVE 0 TO OLN-DOWNPY.
           IF LN-DOWNPY NOT = OLN-DOWNPY
              MOVE "LN-DOWNPY" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-DOWNPY  TO D01-A15-NUM
              MOVE LN-DOWNPY   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-TRADEIN NOT NUMERIC
              MOVE 0 TO OLN-TRADEIN.
           IF LN-TRADEIN NOT = OLN-TRADEIN
              MOVE "LN-TRADEIN" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-TRADEIN  TO D01-A15-NUM
              MOVE LN-TRADEIN   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-LNAMT NOT NUMERIC
              MOVE 0 TO OLN-LNAMT.
           IF LN-LNAMT NOT = OLN-LNAMT
              MOVE "LN-LNAMT" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-LNAMT  TO D01-A15-NUM
              MOVE LN-LNAMT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-FINCHG NOT NUMERIC
              MOVE 0 TO OLN-FINCHG.
           IF LN-FINCHG NOT = OLN-FINCHG
              MOVE "LN-FINCHG" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-FINCHG  TO D01-A15-NUM
              MOVE LN-FINCHG   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-INTCHG NOT NUMERIC
              MOVE 0 TO OLN-INTCHG.
           IF LN-INTCHG NOT = OLN-INTCHG
              MOVE "LN-INTCHG" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-INTCHG  TO D01-A15-NUM
              MOVE LN-INTCHG   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-SERCHG NOT NUMERIC
              MOVE 0 TO OLN-SERCHG.
           IF LN-SERCHG NOT = OLN-SERCHG
              MOVE "LN-SERCHG" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-SERCHG  TO D01-A15-NUM
              MOVE LN-SERCHG   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-EXTCHG NOT NUMERIC
              MOVE 0 TO OLN-EXTCHG.
           IF LN-EXTCHG NOT = OLN-EXTCHG
              MOVE "LN-EXTCHG" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-EXTCHG  TO D01-A15-NUM
              MOVE LN-EXTCHG   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-MAINTFEE NOT NUMERIC
              MOVE 0 TO OLN-MAINTFEE.
           IF LN-MAINTFEE NOT = OLN-MAINTFEE
              MOVE "LN-MAINTFEE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-MAINTFEE  TO D01-A15-NUM
              MOVE LN-MAINTFEE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-POINTS NOT NUMERIC
              MOVE 0 TO OLN-POINTS.
           IF LN-POINTS NOT = OLN-POINTS
              MOVE "LN-POINTS" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-POINTS  TO D01-A15-NUM
              MOVE LN-POINTS   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-PROCEEDS NOT NUMERIC
              MOVE 0 TO OLN-PROCEEDS.
           IF LN-PROCEEDS NOT = OLN-PROCEEDS
              MOVE "LN-PROCEEDS" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-PROCEEDS  TO D01-A15-NUM
              MOVE LN-PROCEEDS   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-ORGTERM NOT NUMERIC
              MOVE 0 TO OLN-ORGTERM.
           IF LN-ORGTERM NOT = OLN-ORGTERM
              MOVE "LN-ORGTERM" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-ORGTERM  TO D01-A15
              MOVE LN-ORGTERM   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-1STPYAMT NOT NUMERIC
              MOVE 0 TO OLN-1STPYAMT.
           IF LN-1STPYAMT NOT = OLN-1STPYAMT
              MOVE "LN-1STPYAMT" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-1STPYAMT  TO D01-A15-NUM
              MOVE LN-1STPYAMT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-NOREGPY NOT NUMERIC
              MOVE 0 TO OLN-NOREGPY.
           IF LN-NOREGPY NOT = OLN-NOREGPY
              MOVE "LN-NOREGPY" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-NOREGPY  TO D01-A15
              MOVE LN-NOREGPY   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-REGPYAMT NOT NUMERIC
              MOVE 0 TO OLN-REGPYAMT.
           IF LN-REGPYAMT NOT = OLN-REGPYAMT
              MOVE "LN-REGPYAMT" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-REGPYAMT  TO D01-A15-NUM
              MOVE LN-REGPYAMT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-LASTPYAMT NOT NUMERIC
              MOVE 0 TO OLN-LASTPYAMT.
           IF LN-LASTPYAMT NOT = OLN-LASTPYAMT
              MOVE "LN-LASTPYAMT" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-LASTPYAMT  TO D01-A15-NUM
              MOVE LN-LASTPYAMT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-PROCASH NOT NUMERIC
              MOVE 0 TO OLN-PROCASH.
           IF LN-PROCASH NOT = OLN-PROCASH
              MOVE "LN-PROCASH" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-PROCASH  TO D01-A15-NUM
              MOVE LN-PROCASH   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM SET-HOLD-TILLNO.
           IF LN-TILLNO NOT = HOLD-LN-TILLNO
              MOVE "LN-TILLNO" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE HOLD-LN-TILLNO TO D01-A15
              MOVE LN-TILLNO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-STATE-FLAG NOT = OLN-STATE-FLAG
              MOVE "LN-STATE-FLAG" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-STATE-FLAG  TO D01-A15
              MOVE LN-STATE-FLAG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-STATE-WORKER NOT NUMERIC
              MOVE 0 TO OLN-STATE-WORKER.
           IF LN-STATE-WORKER NOT = OLN-STATE-WORKER
              MOVE "LN-STATE-WORKER" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-STATE-WORKER  TO D01-A15-NUM
              MOVE LN-STATE-WORKER   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-STATE-WORKER2 NOT NUMERIC
              MOVE 0 TO OLN-STATE-WORKER2.
           IF LN-STATE-WORKER2 NOT = OLN-STATE-WORKER2
              MOVE "LN-STATE-WORKER2" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-STATE-WORKER2  TO D01-A15-NUM
              MOVE LN-STATE-WORKER2   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
              IF LN-FEECODE(SUB) NOT = OLN-FEECODE(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "LN-FEECODE",SUB INTO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE LN-ACCTNO  TO D01-ACCTNO
                 MOVE OLN-FEECODE(SUB)  TO D01-A15
                 MOVE LN-FEECODE(SUB)   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
      * CLEANSE
              IF OLN-FEEAMT(SUB) NOT NUMERIC
                 MOVE 1.00 TO OLN-FEEAMT(SUB)
              END-IF
              IF LN-FEEAMT(SUB) NOT = OLN-FEEAMT(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "LN-FEEAMT",SUB INTO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE LN-ACCTNO  TO D01-ACCTNO
                 MOVE OLN-FEEAMT(SUB)  TO D01-A15-NUM
                 MOVE LN-FEEAMT(SUB)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF LN-FEEOURS(SUB) NOT = OLN-FEEOURS(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "LN-FEEOURS",SUB INTO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE LN-ACCTNO  TO D01-ACCTNO
                 MOVE OLN-FEEOURS(SUB)  TO D01-A15
                 MOVE LN-FEEOURS(SUB)   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF LN-FEEFINCHG(SUB) NOT = OLN-FEEFINCHG(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "LN-FEEFINCHG",SUB INTO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE LN-ACCTNO  TO D01-ACCTNO
                 MOVE OLN-FEEFINCHG(SUB)  TO D01-A15
                 MOVE LN-FEEFINCHG(SUB)   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.

           IF LN-INSURED NOT = OLN-INSURED
              MOVE "LN-INSURED" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-INSURED  TO D01-A15
              MOVE LN-INSURED   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-INSTYPES NOT = OLN-INSTYPES
              MOVE "LN-INSTYPES" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-INSTYPES  TO D01-A15
              MOVE LN-INSTYPES   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-INSELIGIBLE NOT = OLN-INSELIGIBLE
              MOVE "LN-INSELIGIBLE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-INSELIGIBLE  TO D01-A15
              MOVE LN-INSELIGIBLE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

      * CLEANSE
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
              IF OLN-INSPREM(SUB) NOT = 0
                 IF OLN-INSEFF(SUB) = 0 AND OLN-INSEXP(SUB) NOT = 0
                    MOVE OLN-LOANDATE TO OLN-INSEFF(SUB)
                                         OLN-INSEXP(SUB)
                 END-IF
              END-IF
              IF OLN-INSCOMP(SUB) NOT NUMERIC
                 MOVE 0 TO OLN-INSCOMP(SUB)
              END-IF
              IF OLN-INSEFF(SUB) NOT NUMERIC
                 MOVE 0 TO OLN-INSEFF(SUB)
              END-IF
              IF OLN-INSEXP(SUB) NOT NUMERIC
                 MOVE 0 TO OLN-INSEXP(SUB)
              END-IF
              IF OLN-INSCOMM(SUB) NOT NUMERIC
                 MOVE 0 TO OLN-INSCOMM(SUB)
              END-IF
              IF OLN-INSANTICERN(SUB) NOT NUMERIC
                 MOVE 0 TO OLN-INSANTICERN(SUB)
              END-IF
              IF OLN-INSANTICADJ(SUB) NOT NUMERIC
                 MOVE 0 TO OLN-INSANTICADJ(SUB)
              END-IF
              IF OLN-INSUFERN(SUB) NOT NUMERIC
                 MOVE 0 TO OLN-INSUFERN(SUB)
              END-IF
              IF OLN-INSACCERN(SUB) NOT NUMERIC
                 MOVE 0 TO OLN-INSACCERN(SUB)
              END-IF
           END-PERFORM.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 8
              IF LN-INSCOMP(SUB) NOT = OLN-INSCOMP(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "LN-INSCOMP",SUB INTO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE LN-ACCTNO  TO D01-ACCTNO
                 MOVE OLN-INSCOMP(SUB)  TO D01-A15
                 MOVE LN-INSCOMP(SUB)   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF LN-INSOURS(SUB) NOT = OLN-INSOURS(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "LN-INSOURS",SUB INTO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE LN-ACCTNO  TO D01-ACCTNO
                 MOVE OLN-INSOURS(SUB)  TO D01-A15
                 MOVE LN-INSOURS(SUB)   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
      * IF A DATE
              IF OLN-INSEFF(SUB) > 900
                 MOVE OLN-INSEFF(SUB) TO DATE-MMDDYY
                 PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
                 PERFORM TEST-INVALID-DATE
                 IF DATE-YYYYMMDD NOT = LN-INSEFF-DATE(SUB)
                    MOVE SPACES TO D01-TEXT
                    STRING "LN-INSEFF-DATE",SUB INTO D01-TEXT
                    MOVE BR-NO      TO D01-BRNO
                    MOVE LN-ACCTNO  TO D01-ACCTNO
                    MOVE OLN-INSEFF(SUB)  TO D01-A15
                    MOVE LN-INSEFF-DATE(SUB)   TO D01-S35
                    PERFORM WRITE-DETAIL-LINE-ERROR
                 END-IF
              ELSE
      * IF IT IS A TERM
                 IF LN-INSEFF-DATE(SUB) NOT = OLN-INSEFF(SUB)
                    MOVE SPACES TO D01-TEXT
                    STRING "LN-INSEFF-DATE",SUB INTO D01-TEXT
                    MOVE BR-NO      TO D01-BRNO
                    MOVE LN-ACCTNO  TO D01-ACCTNO
                    MOVE OLN-INSEFF(SUB)  TO D01-A15
                    MOVE LN-INSEFF-DATE(SUB)   TO D01-S35
                    PERFORM WRITE-DETAIL-LINE-ERROR
                 END-IF
              END-IF
              MOVE OLN-INSEXP(SUB) TO DATE-MMDDYY
              PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
              PERFORM TEST-INVALID-DATE
              IF DATE-YYYYMMDD NOT = LN-INSEXP-DATE(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "LN-INSEXP-DATE",SUB INTO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE LN-ACCTNO  TO D01-ACCTNO
                 MOVE OLN-INSEXP(SUB)  TO D01-A15
                 MOVE LN-INSEXP-DATE(SUB)   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
      * CLEANSE
              IF OLN-INSPREM(SUB) NOT NUMERIC
                 MOVE 1.00 TO OLN-INSPREM(SUB)
              END-IF
              IF LN-INSPREM(SUB) NOT = OLN-INSPREM(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "LN-INSPREM",SUB INTO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE LN-ACCTNO  TO D01-ACCTNO
                 MOVE OLN-INSPREM(SUB)  TO D01-A15-NUM
                 MOVE LN-INSPREM(SUB)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
      * CLEANSE
              IF OLN-INSCOVR(SUB) NOT NUMERIC
                 MOVE OLN-LNAMT TO OLN-INSCOVR(SUB)
              END-IF
              IF LN-INSCOVR(SUB) NOT = OLN-INSCOVR(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "LN-INSCOVR",SUB INTO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE LN-ACCTNO  TO D01-ACCTNO
                 MOVE OLN-INSCOVR(SUB)  TO D01-A15-NUM
                 MOVE LN-INSCOVR(SUB)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF LN-INSCOMM(SUB) NOT = OLN-INSCOMM(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "LN-INSCOMM",SUB INTO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE LN-ACCTNO  TO D01-ACCTNO
                 MOVE OLN-INSCOMM(SUB)  TO D01-A15-NUM
                 MOVE LN-INSCOMM(SUB)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF LN-INSANTICERN(SUB) NOT = OLN-INSANTICERN(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "LN-INSANTICERN",SUB INTO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE LN-ACCTNO  TO D01-ACCTNO
                 MOVE OLN-INSANTICERN(SUB)  TO D01-A15-NUM
                 MOVE LN-INSANTICERN(SUB)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF LN-INSANTICADJ(SUB) NOT = OLN-INSANTICADJ(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "LN-INSANTICADJ",SUB INTO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE LN-ACCTNO  TO D01-ACCTNO
                 MOVE OLN-INSANTICADJ(SUB)  TO D01-A15-NUM
                 MOVE LN-INSANTICADJ(SUB)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF LN-INSUFERN(SUB) NOT = OLN-INSUFERN(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "LN-INSUFERN",SUB INTO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE LN-ACCTNO  TO D01-ACCTNO
                 MOVE OLN-INSUFERN(SUB)  TO D01-A15-NUM
                 MOVE LN-INSUFERN(SUB)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF LN-INSACCERN(SUB) NOT = OLN-INSACCERN(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "LN-INSACCERN",SUB INTO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE LN-ACCTNO  TO D01-ACCTNO
                 MOVE OLN-INSACCERN(SUB)  TO D01-A15-NUM
                 MOVE LN-INSACCERN(SUB)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF LN-INSEARN-ORDER(SUB) NOT = OLN-INSEARN-ORDER(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "LN-INSEARN-ORDER",SUB INTO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE LN-ACCTNO  TO D01-ACCTNO
                 MOVE OLN-INSEARN-ORDER(SUB)  TO D01-A15
                 MOVE LN-INSEARN-ORDER(SUB)   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           
      * S35 HAS A "0" IN HERE, SHOULD BE Y OR N, NOTHING EVER CHANGES IT SO
              IF LN-INSFINCHG(SUB) NOT = OLN-INSFINCHG(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "LN-INSFINCHG",SUB INTO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE LN-ACCTNO  TO D01-ACCTNO
                 MOVE OLN-INSFINCHG(SUB)  TO D01-A15
                 MOVE LN-INSFINCHG(SUB)   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

           END-PERFORM.

           MOVE OLN-PYMMYY-RECDEL TO DATE-MMYY.
           PERFORM CONVERT-MMYY-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.
           IF LN-PY-YYYYMM-RECDEL NOT = DATE-YYYYMM
              MOVE "LN-PY-YYYYMM-RECDEL" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-PYMMYY-RECDEL  TO D01-A15
              MOVE LN-PY-YYYYMM-RECDEL   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-MTD-PAYS-RECDEL NOT = OLN-MTD-PAYS-RECDEL
              MOVE "LN-MTD-PAYS-RECDEL" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-MTD-PAYS-RECDEL  TO D01-A15-NUM
              MOVE LN-MTD-PAYS-RECDEL   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-GARNISH-EXTR NOT = OLN-GARNISH-EXTR
              MOVE "LN-GARNISH-EXTR" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-GARNISH-EXTR  TO D01-A15
              MOVE LN-GARNISH-EXTR   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-SHIP-SAME-FG NOT = OLN-SHIP-SAME-FG
              MOVE "LN-SHIP-SAME-FG" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-SHIP-SAME-FG  TO D01-A15
              MOVE LN-SHIP-SAME-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-AD-D-INS-COVR NOT NUMERIC
              MOVE 0 TO OLN-AD-D-INS-COVR.
           IF LN-AD-D-INS-COVR NOT = OLN-AD-D-INS-COVR
              MOVE "LN-AD-D-INS-COVR" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-AD-D-INS-COVR  TO D01-A15
              MOVE LN-AD-D-INS-COVR   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-TOT-PARTIAL-CHGOFF NOT NUMERIC
              MOVE 0 TO OLN-TOT-PARTIAL-CHGOFF.
           IF LN-TOT-PARTIAL-CHGOFF NOT = OLN-TOT-PARTIAL-CHGOFF
              MOVE "LN-TOT-PARTIAL-CHGOFF" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-TOT-PARTIAL-CHGOFF  TO D01-A15-NUM
              MOVE LN-TOT-PARTIAL-CHGOFF   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-LPO NOT = OLN-LPO
              MOVE "LN-LPO" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-LPO  TO D01-A15
              MOVE LN-LPO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

      * CLEANSE - FOUND TONS OF RECORDS WITH 00020202
           IF OLN-EMP-ID NUMERIC AND OLN-EMP-ID = 00020202
              MOVE 0                  TO OLN-EMP-ID 
           ELSE
              IF OLN-EMP-ID NUMERIC
                 IF LN-EMP-ID NOT = OLN-EMP-ID
                    MOVE "LN-EMP-ID" TO D01-TEXT
                    MOVE BR-NO      TO D01-BRNO
                    MOVE LN-ACCTNO  TO D01-ACCTNO
                    MOVE OLN-EMP-ID  TO D01-A15
                    MOVE LN-EMP-ID   TO D01-S35
                    PERFORM WRITE-DETAIL-LINE-ERROR.
           IF OLN-EMP-ID NUMERIC AND OLN-EMP-ID = 00020202
              MOVE 0                  TO OLN-EMP-ID 
           ELSE
              IF OLN-EMP-ID NUMERIC
                 MOVE OLN-EMP-ID      TO OLN-EMP-ID
              ELSE
                 IF LN-REP-CD NOT = OLN-REP-CD
                    MOVE "LN-REP-CD" TO D01-TEXT
                    MOVE BR-NO      TO D01-BRNO
                    MOVE LN-ACCTNO  TO D01-ACCTNO
                    MOVE OLN-REP-CD  TO D01-A15
                    MOVE LN-REP-CD   TO D01-S35
                    PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-PROCESS-CD NOT = OLN-PROCESS-CD
              MOVE "LN-PROCESS-CD" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-PROCESS-CD  TO D01-A15
              MOVE LN-PROCESS-CD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-PRIOR-RN-FOR10 NOT NUMERIC
              MOVE 0 TO OLN-PRIOR-RN-FOR10.
           MOVE OLN-PRIOR-RN-FOR10 TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LN-PRIOR-RN-FOR10-DATE
              MOVE "LN-PRIOR-RN-FOR10" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-PRIOR-RN-FOR10  TO D01-A15
              MOVE LN-PRIOR-RN-FOR10-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-LAST-RN-FOR10 NOT NUMERIC
              MOVE 0 TO OLN-LAST-RN-FOR10.
           MOVE OLN-LAST-RN-FOR10 TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LN-LAST-RN-FOR10-DATE
              MOVE "LN-LAST-RN-FOR10" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-LAST-RN-FOR10  TO D01-A15
              MOVE LN-LAST-RN-FOR10-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-WCBC-CREDITLIM NOT NUMERIC
              MOVE 0 TO OLN-WCBC-CREDITLIM.
           IF LN-WCBC-CREDITLIM NOT = OLN-WCBC-CREDITLIM
              MOVE "LN-WCBC-CREDITLIM" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-CREDITLIM  TO D01-A15
              MOVE LN-CREDITLIM   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-FOLLOWUP-CD NOT = OLN-FOLLOWUP-CD
              MOVE "LN-FOLLOWUP-CD" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-FOLLOWUP-CD  TO D01-A15
              MOVE LN-FOLLOWUP-CD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-MIP-FG NOT = OLN-MIP-FG
              MOVE "LN-MIP-FG" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-MIP-FG  TO D01-A15
              MOVE LN-MIP-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-MIP-MTD-CLAHPAID NOT NUMERIC
              MOVE 0 TO OLN-MIP-MTD-CLAHPAID.
           IF LN-MIP-MTD-CLAHPAID NOT = OLN-MIP-MTD-CLAHPAID
              MOVE "LN-MIP-MTD-CLAHPAID" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-MIP-MTD-CLAHPAID  TO D01-A15-NUM
              MOVE LN-MIP-MTD-CLAHPAID   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLN-MIP-CLPDTH TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LN-MIP-CLPDTH-DATE
              MOVE "LN-MIP-CLPDTH" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-MIP-CLPDTH  TO D01-A15
              MOVE LN-MIP-CLPDTH-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-MIP-TOT-CLPAID NOT NUMERIC
              MOVE 0 TO OLN-MIP-TOT-CLPAID.
           IF LN-MIP-TOT-CLPAID NOT = OLN-MIP-TOT-CLPAID
              MOVE "LN-MIP-TOT-CLPAID" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-MIP-TOT-CLPAID  TO D01-A15-NUM
              MOVE LN-MIP-TOT-CLPAID   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLN-MIP-AHPDTH TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LN-MIP-AHPDTH-DATE
              MOVE "LN-MIP-AHPDTH" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-MIP-AHPDTH  TO D01-A15
              MOVE LN-MIP-AHPDTH-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-MIP-TOT-AHPAID NOT NUMERIC
              MOVE 0 TO OLN-MIP-TOT-AHPAID.
           IF LN-MIP-TOT-AHPAID NOT = OLN-MIP-TOT-AHPAID
              MOVE "LN-MIP-TOT-AHPAID" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-MIP-TOT-AHPAID  TO D01-A15-NUM
              MOVE LN-MIP-TOT-AHPAID   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-RESCISSION NOT = OLN-RESCISSION
              MOVE "LN-RESCISSION" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-RESCISSION  TO D01-A15
              MOVE LN-RESCISSION   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-INTERIM-TRW NOT = OLN-INTERIM-TRW
              MOVE "LN-INTERIM-TRW" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-INTERIM-TRW  TO D01-A15
              MOVE LN-INTERIM-TRW   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-DLNO NOT NUMERIC
              MOVE 0 TO OLN-DLNO.
           IF LN-DLNO NOT = OLN-DLNO
              MOVE "LN-DLNO" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-DLNO  TO D01-A15-NUM
              MOVE LN-DLNO   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-DLPOOL NOT NUMERIC
              MOVE 0 TO OLN-DLPOOL.
           IF LN-DLPOOL NOT = OLN-DLPOOL
              MOVE "LN-DLPOOL" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-DLPOOL  TO D01-A15
              MOVE LN-DLPOOL   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-DLRECOURSE NOT = OLN-DLRECOURSE
              MOVE "LN-DLRECOURSE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-DLRECOURSE  TO D01-A15
              MOVE LN-DLRECOURSE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-DLPARTRCAMT NOT NUMERIC
              MOVE 0 TO OLN-DLPARTRCAMT.
           IF LN-DLPARTRCAMT NOT = OLN-DLPARTRCAMT
              MOVE "LN-DLPARTRCAMT" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-DLPARTRCAMT  TO D01-A15-NUM
              MOVE LN-DLPARTRCAMT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-ORIGPURAMT NOT NUMERIC
              MOVE 0 TO OLN-ORIGPURAMT.
           IF LN-ORIGPURAMT NOT = OLN-ORIGPURAMT
              MOVE "LN-ORIGPURAMT" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-ORIGPURAMT  TO D01-A15-NUM
              MOVE LN-ORIGPURAMT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-PURBAL NOT NUMERIC
              MOVE 0 TO OLN-PURBAL.
           IF LN-PURBAL NOT = OLN-PURBAL
              MOVE "LN-PURBAL" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-PURBAL  TO D01-A15-NUM
              MOVE LN-PURBAL   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
              IF OLN-DLRVPRIN(SUB) NOT NUMERIC
                 MOVE 0 TO OLN-DLRVPRIN(SUB)
              END-IF
              IF LN-DLRVPRIN(SUB) NOT = OLN-DLRVPRIN(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "LN-DLRVPRIN",SUB INTO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE LN-ACCTNO  TO D01-ACCTNO
                 MOVE OLN-DLRVPRIN(SUB)  TO D01-A15-NUM
                 MOVE LN-DLRVPRIN(SUB)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OLN-DLRVCHGS(SUB) NOT NUMERIC
                 MOVE 0 TO OLN-DLRVCHGS(SUB)
              END-IF
              IF LN-DLRVCHGS(SUB) NOT = OLN-DLRVCHGS(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "LN-DLRVCHGS",SUB INTO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE LN-ACCTNO  TO D01-ACCTNO
                 MOVE OLN-DLRVCHGS(SUB)  TO D01-A15-NUM
                 MOVE LN-DLRVCHGS(SUB)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.

           IF OLN-DLPARVHELD NOT NUMERIC
              MOVE 0 TO OLN-DLPARVHELD.
           IF LN-DLPARVHELD NOT = OLN-DLPARVHELD
              MOVE "LN-DLPARVHELD" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-DLPARVHELD  TO D01-A15-NUM
              MOVE LN-DLPARVHELD   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-DLPARVPAID NOT NUMERIC
              MOVE 0 TO OLN-DLPARVPAID.
           IF LN-DLPARVPAID NOT = OLN-DLPARVPAID
              MOVE "LN-DLPARVPAID" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-DLPARVPAID  TO D01-A15-NUM
              MOVE LN-DLPARVPAID   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-DLDISC NOT NUMERIC
              MOVE 0 TO OLN-DLDISC.
           IF LN-DLDISC NOT = OLN-DLDISC
              MOVE "LN-DLDISC" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-DLDISC  TO D01-A15-NUM
              MOVE LN-DLDISC   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLN-LCPDTH TO DATE-MMDDYY.
           IF (DATE-MMDDYY-MM = 0) AND (DATE-MMDDYY-YY = 0) AND
               (OLN-POFFDATE NOT = 0)
              MOVE OLN-POFFDATE TO OLN-LCPDTH DATE-MMDDYY.
           IF (DATE-MMDDYY-MM = 0) AND (DATE-MMDDYY-YY = 0)
              MOVE OLN-LOANDATE TO OLN-LCPDTH DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LN-LCPDTH-DATE
              MOVE "LN-LCPDTH" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-LCPDTH  TO D01-A15
              MOVE LN-LCPDTH-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-INTPDTH NOT NUMERIC
              MOVE 0 TO OLN-INTPDTH.
           MOVE OLN-INTPDTH TO DATE-MMDDYY.
      * CLEANSE   BR 1007 ACCT 1500 INTPDTH = 2007/33/00; 33/00/07
           IF (DATE-MMDDYY-MM = 0) AND (DATE-MMDDYY-YY = 0) AND
               (OLN-POFFDATE NOT = 0)
              MOVE OLN-POFFDATE TO OLN-INTPDTH DATE-MMDDYY.
           IF (DATE-MMDDYY-MM = 0) AND (DATE-MMDDYY-YY = 0)
              MOVE OLN-LOANDATE TO OLN-INTPDTH DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LN-INTPDTH-DATE
              MOVE "LN-INTPDTH" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-INTPDTH  TO D01-A15
              MOVE LN-INTPDTH-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-CURBAL NOT NUMERIC
              MOVE 0 TO OLN-CURBAL.
           IF LN-CURBAL NOT = OLN-CURBAL
              MOVE "LN-CURBAL" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-CURBAL  TO D01-A15-NUM
              MOVE LN-CURBAL   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-INTBAL NOT NUMERIC
              MOVE 0 TO OLN-INTBAL.
           IF LN-INTBAL NOT = OLN-INTBAL
              MOVE "LN-INTBAL" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-INTBAL  TO D01-A15-NUM
              MOVE LN-INTBAL   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-LCBAL NOT NUMERIC
              MOVE 0 TO OLN-LCBAL.
           IF LN-LCBAL NOT = OLN-LCBAL
              MOVE "LN-LCBAL" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-LCBAL  TO D01-A15-NUM
              MOVE LN-LCBAL   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-OTHBAL NOT NUMERIC
              MOVE 0 TO OLN-OTHBAL.
           IF LN-OTHBAL NOT = OLN-OTHBAL
              MOVE "LN-OTHBAL" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-OTHBAL  TO D01-A15-NUM
              MOVE LN-OTHBAL   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-OT2BAL NOT NUMERIC
              MOVE 0 TO OLN-OT2BAL.
           IF LN-OT2BAL NOT = OLN-OT2BAL
              MOVE "LN-OT2BAL" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-OT2BAL  TO D01-A15-NUM
              MOVE LN-OT2BAL   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-LCPAID NOT NUMERIC
              MOVE 0 TO OLN-LCPAID.
           IF LN-LCPAID NOT = OLN-LCPAID
              MOVE "LN-LCPAID" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-LCPAID  TO D01-A15-NUM
              MOVE LN-LCPAID   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-LCPARTIALS NOT NUMERIC
              MOVE 0 TO OLN-LCPARTIALS.
           IF LN-LCPARTIALS NOT = OLN-LCPARTIALS
              MOVE "LN-LCPARTIALS" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-LCPARTIALS  TO D01-A15-NUM
              MOVE LN-LCPARTIALS   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-STMT-COUNTER NOT NUMERIC
              MOVE 0 TO OLN-STMT-COUNTER.
           IF LN-STMT-COUNTER NOT = OLN-STMT-COUNTER
              MOVE "LN-STMT-COUNTER" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-STMT-COUNTER  TO D01-A15
              MOVE LN-STMT-COUNTER   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-FROZEN-REASON1 = X"00"
               MOVE SPACES TO OLN-FROZEN-REASON.
           IF LN-FROZEN-REASON NOT = OLN-FROZEN-REASON
              MOVE "LN-FROZEN-REASON" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-FROZEN-REASON  TO D01-A15
              MOVE LN-FROZEN-REASON   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

      * CLEANSE, SOME ACCOUNTS HAD ^@ DISPLAYING, CONVERSION ACCOUNTS
           IF OLN-COUPON-STATEMENT NOT = " " AND
                                    NOT = "C" AND
                                     NOT = "S" AND
                                      NOT = "E"
              MOVE " "                    TO OLN-COUPON-STATEMENT.
           IF LN-COUPON-STATEMENT NOT = OLN-COUPON-STATEMENT
              MOVE "LN-COUPON-STATEMENT" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-COUPON-STATEMENT  TO D01-A15
              MOVE LN-COUPON-STATEMENT   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-MONTHEND-BAL NOT NUMERIC
              MOVE 0 TO OLN-MONTHEND-BAL.
           IF LN-MONTHEND-BAL NOT = OLN-MONTHEND-BAL
              MOVE "LN-MONTHEND-BAL" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-MONTHEND-BAL  TO D01-A15-NUM
              MOVE LN-MONTHEND-BAL   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-ANTICTERM NOT NUMERIC
              MOVE 0 TO OLN-ANTICTERM.
           IF LN-ANTICTERM NOT = OLN-ANTICTERM
              MOVE "LN-ANTICTERM" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-ANTICTERM  TO D01-A15
              MOVE LN-ANTICTERM   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
              IF OLN-ANTICERN(SUB) NOT NUMERIC
                  MOVE 0 TO OLN-ANTICERN(SUB)
              END-IF
              IF LN-ANTICERN(SUB) NOT = OLN-ANTICERN(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "LN-ANTICERN",SUB INTO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE LN-ACCTNO  TO D01-ACCTNO
                 MOVE OLN-ANTICERN(SUB)  TO D01-A15-NUM
                 MOVE LN-ANTICERN(SUB)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OLN-ANTICADJ(SUB) NOT NUMERIC
                  MOVE 0 TO OLN-ANTICADJ(SUB)
              END-IF
              IF LN-ANTICADJ(SUB) NOT = OLN-ANTICADJ(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "LN-ANTICADJ",SUB INTO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE LN-ACCTNO  TO D01-ACCTNO
                 MOVE OLN-ANTICADJ(SUB)  TO D01-A15-NUM
                 MOVE LN-ANTICADJ(SUB)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OLN-UFERN(SUB) NOT NUMERIC
                  MOVE 0 TO OLN-UFERN(SUB)
              END-IF
              IF LN-UFERN(SUB) NOT = OLN-UFERN(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "LN-UFERN",SUB INTO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE LN-ACCTNO  TO D01-ACCTNO
                 MOVE OLN-UFERN(SUB)  TO D01-A15-NUM
                 MOVE LN-UFERN(SUB)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OLN-ACCERN(SUB) NOT NUMERIC
                  MOVE 0 TO OLN-ACCERN(SUB)
              END-IF
              IF LN-ACCERN(SUB) NOT = OLN-ACCERN(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "LN-ACCERN",SUB INTO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE LN-ACCTNO  TO D01-ACCTNO
                 MOVE OLN-ACCERN(SUB)  TO D01-A15-NUM
                 MOVE LN-ACCERN(SUB)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF LN-EARN-ORDER(SUB) NOT = OLN-EARN-ORDER(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "LN-EARN-ORDER",SUB INTO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE LN-ACCTNO  TO D01-ACCTNO
                 MOVE OLN-EARN-ORDER(SUB)  TO D01-A15
                 MOVE LN-EARN-ORDER(SUB)   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.

           IF OLN-LMO-PRIOR-INTEREST NOT NUMERIC
              MOVE 0 TO OLN-LMO-PRIOR-INTEREST.
           IF LN-LMO-PRIOR-INTEREST NOT = OLN-LMO-PRIOR-INTEREST
              MOVE "LN-LMO-PRIOR-INTEREST" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-LMO-PRIOR-INTEREST  TO D01-A15-NUM
              MOVE LN-LMO-PRIOR-INTEREST   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-PRIOR-UNEARNED NOT NUMERIC
              MOVE 0 TO OLN-PRIOR-UNEARNED.
           IF LN-PRIOR-UNEARNED NOT = OLN-PRIOR-UNEARNED
              MOVE "LN-PRIOR-UNEARNED" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-PRIOR-UNEARNED  TO D01-A15-NUM
              MOVE LN-PRIOR-UNEARNED   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-ERNDELQ-STOP NOT = OLN-ERNDELQ-STOP
              MOVE "LN-ERNDELQ-STOP" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-ERNDELQ-STOP  TO D01-A15
              MOVE LN-ERNDELQ-STOP   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-NONACRERN NOT NUMERIC
              MOVE 0 TO OLN-NONACRERN.
           IF LN-NONACRERN NOT = OLN-NONACRERN
              MOVE "LN-NONACRERN" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-NONACRERN  TO D01-A15-NUM
              MOVE LN-NONACRERN   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-PRIOR-NONACRERN NOT NUMERIC
              MOVE 0 TO OLN-PRIOR-NONACRERN.
           IF LN-PRIOR-NONACRERN NOT = OLN-PRIOR-NONACRERN
              MOVE "LN-PRIOR-NONACRERN" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-PRIOR-NONACRERN  TO D01-A15-NUM
              MOVE LN-PRIOR-NONACRERN   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLN-ERN-IB-PDTH TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LN-ERN-IB-PDTH-DATE
              MOVE "LN-ERN-IB-PDTH" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-ERN-IB-PDTH  TO D01-A15
              MOVE LN-ERN-IB-PDTH-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-ERN-IB-PRINBAL NOT NUMERIC
              MOVE 0 TO OLN-ERN-IB-PRINBAL.
           IF LN-ERN-IB-PRINBAL NOT = OLN-ERN-IB-PRINBAL
              MOVE "LN-ERN-IB-PRINBAL" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-ERN-IB-PRINBAL  TO D01-A15-NUM
              MOVE LN-ERN-IB-PRINBAL   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-ERN-IB-INTOWE NOT NUMERIC
              MOVE 0 TO OLN-ERN-IB-INTOWE.
           IF LN-ERN-IB-INTOWE NOT = OLN-ERN-IB-INTOWE
              MOVE "LN-ERN-IB-INTOWE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-ERN-IB-INTOWE  TO D01-A15-NUM
              MOVE LN-ERN-IB-INTOWE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-TOTERN NOT NUMERIC
              MOVE 0 TO OLN-TOTERN.
           IF LN-TOTERN NOT = OLN-TOTERN
              MOVE "LN-TOTERN" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-TOTERN  TO D01-A15-NUM
              MOVE LN-TOTERN   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-TOTOTH NOT NUMERIC
              MOVE 0 TO OLN-TOTOTH.
           IF LN-TOTOTH NOT = OLN-TOTOTH
              MOVE "LN-TOTOTH" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-TOTOTH  TO D01-A15-NUM
              MOVE LN-TOTOTH   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-TOTINT NOT NUMERIC
              MOVE 0 TO OLN-TOTINT.
           IF LN-TOTINT NOT = OLN-TOTINT
              MOVE "LN-TOTINT" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-TOTINT  TO D01-A15-NUM
              MOVE LN-TOTINT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-TOTLCHG NOT NUMERIC
              MOVE 0 TO OLN-TOTLCHG.
           IF LN-TOTLCHG NOT = OLN-TOTLCHG
              MOVE "LN-TOTLCHG" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-TOTLCHG  TO D01-A15-NUM
              MOVE LN-TOTLCHG   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-TOTDEF NOT NUMERIC
              MOVE 0 TO OLN-TOTDEF.
           IF LN-TOTDEF NOT = OLN-TOTDEF
              MOVE "LN-TOTDEF" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-TOTDEF  TO D01-A15-NUM
              MOVE LN-TOTDEF   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-TOTSERACCT NOT NUMERIC
              MOVE 0 TO OLN-TOTSERACCT.
           IF LN-TOTSERACCT NOT = OLN-TOTSERACCT
              MOVE "LN-TOTSERACCT" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-TOTSERACCT  TO D01-A15-NUM
              MOVE LN-TOTSERACCT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-LCWAIVERS NOT NUMERIC
              MOVE 0 TO OLN-LCWAIVERS.
           IF LN-LCWAIVERS NOT = OLN-LCWAIVERS
              MOVE "LN-LCWAIVERS" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-LCWAIVERS  TO D01-A15-NUM
              MOVE LN-LCWAIVERS   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-INTWAIVERS NOT NUMERIC
              MOVE 0 TO OLN-INTWAIVERS.
           IF LN-INTWAIVERS NOT = OLN-INTWAIVERS
              MOVE "LN-INTWAIVERS" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-INTWAIVERS  TO D01-A15-NUM
              MOVE LN-INTWAIVERS   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-TOTPAYMNTD NOT NUMERIC
              MOVE 0 TO OLN-TOTPAYMNTD.
           IF LN-TOTPAYMNTD NOT = OLN-TOTPAYMNTD
              MOVE "LN-TOTPAYMNTD" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-TOTPAYMNTD  TO D01-A15-NUM
              MOVE LN-TOTPAYMNTD   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-TOTEXCPAYMNTD NOT NUMERIC
              MOVE 0 TO OLN-TOTEXCPAYMNTD.
           IF LN-TOTEXCPAYMNTD NOT = OLN-TOTEXCPAYMNTD
              MOVE "LN-TOTEXCPAYMNTD" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-TOTEXCPAYMNTD  TO D01-A15-NUM
              MOVE LN-TOTEXCPAYMNTD   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-TOTNODEF NOT NUMERIC
              MOVE 0 TO OLN-TOTNODEF.
           IF LN-TOTNODEF NOT = OLN-TOTNODEF
              MOVE "LN-TOTNODEF" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-TOTNODEF  TO D01-A15
              MOVE LN-TOTNODEF   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-YTDNODEF NOT NUMERIC
              MOVE 0 TO OLN-YTDNODEF.
           IF LN-YTDNODEF NOT = OLN-YTDNODEF
              MOVE "LN-YTDNODEF" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-YTDNODEF  TO D01-A15
              MOVE LN-YTDNODEF   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-TOTNOALDEL NOT NUMERIC
              MOVE 0 TO OLN-TOTNOALDEL.
           IF LN-TOTNOALDEL NOT = OLN-TOTNOALDEL
              MOVE "LN-TOTNOALDEL" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-TOTNOALDEL  TO D01-A15
              MOVE LN-TOTNOALDEL   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-YTDINTPD NOT = OLN-YTDINTPD
              MOVE "LN-YTDINTPD" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-YTDINTPD  TO D01-A15-NUM
              MOVE LN-YTDINTPD   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-LYRINTPD NOT = OLN-LYRINTPD
              MOVE "LN-LYRINTPD" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-LYRINTPD  TO D01-A15-NUM
              MOVE LN-LYRINTPD   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-TOTINTPD NOT NUMERIC
              MOVE 0 TO OLN-TOTINTPD.
           IF LN-TOTINTPD NOT = OLN-TOTINTPD
              MOVE "LN-TOTINTPD" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-TOTINTPD  TO D01-A15-NUM
              MOVE LN-TOTINTPD   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-RECASTINT NOT NUMERIC
              MOVE 0 TO OLN-RECASTINT.
           IF LN-RECASTINT NOT = OLN-RECASTINT
              MOVE "LN-RECASTINT" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-RECASTINT  TO D01-A15-NUM
              MOVE LN-RECASTINT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-RECASTPRIN NOT NUMERIC
              MOVE 0 TO OLN-RECASTPRIN.
           IF LN-RECASTPRIN NOT = OLN-RECASTPRIN
              MOVE "LN-RECASTPRIN" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-RECASTPRIN  TO D01-A15-NUM
              MOVE LN-RECASTPRIN   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-TRW-DOFD NOT NUMERIC OR
                 (OLN-TRW-DOFD = 202020 OR 220202)
              MOVE 0 TO OLN-TRW-DOFD.
           MOVE OLN-TRW-DOFD TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LN-TRW-DOFD-DATE
              MOVE "LN-TRW-DOFD-DATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-TRW-DOFD  TO D01-A15
              MOVE LN-TRW-DOFD-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 13
              IF LN-REBATE(SUB) NOT = OLN-REBATE(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "LN-REBATE",SUB INTO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE LN-ACCTNO  TO D01-ACCTNO
                 MOVE OLN-REBATE(SUB)  TO D01-A15
                 MOVE LN-REBATE(SUB)   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.

           IF LN-PLREASON NOT = OLN-PLREASON
              MOVE "LN-PLREASON" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-PLREASON  TO D01-A15
              MOVE LN-PLREASON   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-COLLECTOR NOT = OLN-COLLECTOR
              MOVE "LN-COLLECTOR" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-COLLECTOR  TO D01-A15
              MOVE LN-COLLECTOR   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-ACTIONCD NOT = OLN-ACTIONCD
      *       CALL "C$JUSTIFY" USING OLN-ACTIONCD, "L"
              IF LN-ACTIONCD NOT = OLN-ACTIONCD
              MOVE "LN-ACTIONCD" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-ACTIONCD  TO D01-A15
              MOVE LN-ACTIONCD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-ACTIONCD2 NOT = OLN-ACTIONCD2
              MOVE "LN-ACTIONCD2" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-ACTIONCD2  TO D01-A15
              MOVE LN-ACTIONCD2   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-ATCODE NOT = OLN-ATCODE
              MOVE "LN-ATCODE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-ATCODE  TO D01-A15
              MOVE LN-ATCODE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-RECODE NOT = OLN-RECODE
              MOVE "LN-RECODE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-RECODE  TO D01-A15
              MOVE LN-RECODE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-LAST-PYPA-DATE NOT NUMERIC
              MOVE 0 TO OLN-LAST-PYPA-DATE.
           MOVE OLN-LAST-PYPA-DATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LN-LAST-PYPA-DATE
              MOVE "LN-LAST-PYPA-DATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-LAST-PYPA-DATE  TO D01-A15
              MOVE LN-LAST-PYPA-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-ESCROW-FG NOT = OLN-ESCROW-FG
              MOVE "LN-ESCROW-FG" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-ESCROW-FG  TO D01-A15
              MOVE LN-ESCROW-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF (OLN-PROMDAYS-PASTDUE NOT NUMERIC) OR
                (OLN-PROMDAYS-PASTDUE = 202)
              MOVE 0                  TO OLN-PROMDAYS-PASTDUE.
           IF LN-PROMDAYS-PASTDUE NOT = OLN-PROMDAYS-PASTDUE
              MOVE "LN-PROMDAYS-PASTDUE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-PROMDAYS-PASTDUE  TO D01-A15
              MOVE LN-PROMDAYS-PASTDUE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-PROMID NOT = OLN-PROMID
              MOVE "LN-PROMID" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-PROMID  TO D01-A15
              MOVE LN-PROMID   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLN-PROMDATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LN-PROMDATE
              MOVE "LN-PROMDATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-PROMDATE  TO D01-A15
              MOVE LN-PROMDATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-PROMAMT NOT NUMERIC
              MOVE 0 TO OLN-PROMAMT.
           IF LN-PROMAMT NOT = OLN-PROMAMT
              MOVE "LN-PROMAMT" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-PROMAMT  TO D01-A15-NUM
              MOVE LN-PROMAMT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLN-DATE-PAID-LAST TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LN-DATE-PAID-LAST
              MOVE "LN-DATE-PAID-LAST" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-DATE-PAID-LAST  TO D01-A15
              MOVE LN-DATE-PAID-LAST   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
      * START METTO2 FIELD WITH DATE-PAID-LAST
           IF DATE-YYYYMMDD NOT = LN-METRO2-LAST-PMT-DATE
              MOVE "LN-METRO2-LAST-PMT-DATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE LN-METRO2-LAST-PMT-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-AMT-PAID-LAST NOT NUMERIC
              MOVE 0 TO OLN-AMT-PAID-LAST.
           IF LN-AMT-PAID-LAST NOT = OLN-AMT-PAID-LAST
              MOVE "LN-AMT-PAID-LAST" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-AMT-PAID-LAST  TO D01-A15-NUM
              MOVE LN-AMT-PAID-LAST   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-LASTCFORMNO NOT NUMERIC
              MOVE 0 TO OLN-LASTCFORMNO.
           IF LN-LASTCFORMNO NOT = OLN-LASTCFORMNO
              MOVE "LN-LASTCFORMNO" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-LASTCFORMNO  TO D01-A15
              MOVE LN-LASTCFORMNO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-LASTCFORMDATE NOT NUMERIC
              MOVE 0 TO OLN-LASTCFORMDATE.
           MOVE OLN-LASTCFORMDATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LN-LASTCFORMDATE
              MOVE "LN-LASTCFORMDATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-LASTCFORMDATE  TO D01-A15
              MOVE LN-LASTCFORMDATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-DELCNT NOT NUMERIC
              MOVE 0 TO OLN-DELCNT.
           IF LN-DELCNT NOT = OLN-DELCNT
              MOVE "LN-DELCNT" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-DELCNT  TO D01-A15
              MOVE LN-DELCNT   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-DELCNT2 NOT NUMERIC
              MOVE 0 TO OLN-DELCNT2.
           IF LN-DELCNT2 NOT = OLN-DELCNT2
              MOVE "LN-DELCNT2" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-DELCNT2  TO D01-A15
              MOVE LN-DELCNT2   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-CREDITLIM NOT NUMERIC
              MOVE 0 TO OLN-CREDITLIM.
           IF LN-CREDITLIM NOT = OLN-CREDITLIM
              MOVE "LN-CREDITLIM" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-CREDITLIM  TO D01-A15
              MOVE LN-CREDITLIM   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-CRSCORE NOT NUMERIC
              MOVE 0 TO OLN-CRSCORE.
           IF LN-CRSCORE NOT = OLN-CRSCORE
              MOVE "LN-CRSCORE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-CRSCORE  TO D01-A15
              MOVE LN-CRSCORE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-LASTSFORMNO NOT NUMERIC
              MOVE 0 TO OLN-LASTSFORMNO.
           IF LN-LASTSFORMNO NOT = OLN-LASTSFORMNO
              MOVE "LN-LASTSFORMNO" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-LASTSFORMNO  TO D01-A15
              MOVE LN-LASTSFORMNO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF OLN-LASTSFORMDATE NOT NUMERIC
              MOVE 0 TO OLN-LASTSFORMDATE.
           MOVE OLN-LASTSFORMDATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LN-LASTSFORMDATE
              MOVE "LN-LASTSFORMDATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-LASTSFORMDATE  TO D01-A15
              MOVE LN-LASTSFORMDATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-SOLCNT NOT NUMERIC
              MOVE 0 TO OLN-SOLCNT.
           IF LN-SOLCNT NOT = OLN-SOLCNT
              MOVE "LN-SOLCNT" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-SOLCNT  TO D01-A15
              MOVE LN-SOLCNT   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLN-MESSAGE TO HOLD-LN-MESSAGE.
           PERFORM TEST-LN-MESSAGE VARYING SUB FROM 1 BY 1 UNTIL
                    SUB > 30.
           IF LN-MESSAGE NOT = HOLD-LN-MESSAGE
      *       CALL "C$JUSTIFY" USING HOLD-LN-MESSAGE, "L"
              IF LN-MESSAGE NOT = HOLD-LN-MESSAGE
              MOVE "LN-MESSAGE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE HOLD-LN-MESSAGE  TO D01-A15
              MOVE LN-MESSAGE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLN-PROMDATE2 TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LN-PROMDATE2
              MOVE "LN-PROMDATE2" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-PROMDATE2  TO D01-A15
              MOVE LN-PROMDATE2   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLN-OLDACCTNO TO OLD-ACCTNO.
           IF LN-OLDACCTNO NOT = NEW-ACCTNO
              MOVE "LN-OLDACCTNO" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-OLDACCTNO  TO D01-A15
              MOVE LN-OLDACCTNO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLN-PRLNNO TO OLD-ACCTNO.
           IF LN-PRLNNO NOT = NEW-ACCTNO
              MOVE "LN-PRLNNO" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-PRLNNO  TO D01-A15
              MOVE LN-PRLNNO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-PRBAL NOT NUMERIC
              MOVE 0 TO OLN-PRBAL.
           IF LN-PRBAL NOT = OLN-PRBAL
              MOVE "LN-PRBAL" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-PRBAL  TO D01-A15-NUM
              MOVE LN-PRBAL   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-PRINTDUE NOT NUMERIC
              MOVE 0 TO OLN-PRINTDUE.
           IF LN-PRINTDUE NOT = OLN-PRINTDUE
              MOVE "LN-PRINTDUE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-PRINTDUE  TO D01-A15-NUM
              MOVE LN-PRINTDUE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-PRFINCHGRB NOT NUMERIC
              MOVE 0 TO OLN-PRFINCHGRB.
           IF LN-PRFINCHGRB NOT = OLN-PRFINCHGRB
              MOVE "LN-PRFINCHGRB" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-PRFINCHGRB  TO D01-A15-NUM
              MOVE LN-PRFINCHGRB   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-PRINSRB NOT NUMERIC
              MOVE 0 TO OLN-PRINSRB.
           IF LN-PRINSRB NOT = OLN-PRINSRB
              MOVE "LN-PRINSRB" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-PRINSRB  TO D01-A15-NUM
              MOVE LN-PRINSRB   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-PRPOCD NOT = OLN-PRPOCD
              MOVE "LN-PRPOCD" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-PRPOCD  TO D01-A15
              MOVE LN-PRPOCD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OLN-PRPODATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LN-PRPODATE
              MOVE "LN-PRPODATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-PRPODATE  TO D01-A15
              MOVE LN-PRPODATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-PRCONPROFILE NOT NUMERIC
              MOVE 0 TO OLN-PRCONPROFILE.
           IF LN-PRCONPROFILE NOT = OLN-PRCONPROFILE
              MOVE "LN-PRCONPROFILE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-PRCONPROFILE  TO D01-A15
              MOVE LN-PRCONPROFILE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-PRDELINQAMT NOT NUMERIC
              MOVE 0 TO OLN-PRDELINQAMT.
           IF LN-PRDELINQAMT NOT = OLN-PRDELINQAMT
              MOVE "LN-PRDELINQAMT" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-PRDELINQAMT  TO D01-A15-NUM
              MOVE LN-PRDELINQAMT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-SUM-ORIGPURAMT NOT NUMERIC
              MOVE 0 TO OLN-SUM-ORIGPURAMT.
           IF LN-SUM-ORIGPURAMT NOT = OLN-SUM-ORIGPURAMT
              MOVE "LN-SUM-ORIGPURAMT" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-SUM-ORIGPURAMT  TO D01-A15-NUM
              MOVE LN-SUM-ORIGPURAMT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-SUM-LNAMT NOT NUMERIC
              MOVE 0 TO OLN-SUM-LNAMT.
           IF LN-SUM-LNAMT NOT = OLN-SUM-LNAMT
              MOVE "LN-SUM-LNAMT" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-SUM-LNAMT  TO D01-A15-NUM
              MOVE LN-SUM-LNAMT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-SUM-FINCHG NOT NUMERIC
              MOVE 0 TO OLN-SUM-FINCHG.
           IF LN-SUM-FINCHG NOT = OLN-SUM-FINCHG
              MOVE "LN-SUM-FINCHG" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-SUM-FINCHG  TO D01-A15-NUM
              MOVE LN-SUM-FINCHG   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-SUM-INTCHG NOT NUMERIC
              MOVE 0 TO OLN-SUM-INTCHG.
           IF LN-SUM-INTCHG NOT = OLN-SUM-INTCHG
              MOVE "LN-SUM-INTCHG" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-SUM-INTCHG  TO D01-A15-NUM
              MOVE LN-SUM-INTCHG   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-SUM-ORGTERM NOT NUMERIC
              MOVE 0 TO OLN-SUM-ORGTERM.
           IF LN-SUM-ORGTERM NOT = OLN-SUM-ORGTERM
              MOVE "LN-SUM-ORGTERM" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-SUM-ORGTERM  TO D01-A15
              MOVE LN-SUM-ORGTERM   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-SUM-REGPYAMT NOT NUMERIC
              MOVE 0 TO OLN-SUM-REGPYAMT.
           IF LN-SUM-REGPYAMT NOT = OLN-SUM-REGPYAMT
              MOVE "LN-SUM-REGPYAMT" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-SUM-REGPYAMT  TO D01-A15-NUM
              MOVE LN-SUM-REGPYAMT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-SUM-EXTCHG NOT NUMERIC
              MOVE 0 TO OLN-SUM-EXTCHG.
           IF LN-SUM-EXTCHG NOT = OLN-SUM-EXTCHG
              MOVE "LN-SUM-EXTCHG" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-SUM-EXTCHG  TO D01-A15-NUM
              MOVE LN-SUM-EXTCHG   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-SUM-SERCHG NOT NUMERIC
              MOVE 0 TO OLN-SUM-SERCHG.
           IF LN-SUM-SERCHG NOT = OLN-SUM-SERCHG
              MOVE "LN-SUM-SERCHG" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-SUM-SERCHG  TO D01-A15-NUM
              MOVE LN-SUM-SERCHG   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-SUM-MAINTFEE NOT NUMERIC
              MOVE 0 TO OLN-SUM-MAINTFEE.
           IF LN-SUM-MAINTFEE NOT = OLN-SUM-MAINTFEE
              MOVE "LN-SUM-MAINTFEE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-SUM-MAINTFEE  TO D01-A15-NUM
              MOVE LN-SUM-MAINTFEE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-UNITPER-CD NOT = OLN-UNITPER-CD
              MOVE "LN-UNITPER-CD" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-UNITPER-CD  TO D01-A15
              MOVE LN-UNITPER-CD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-UNITPER-FREQ NOT NUMERIC
              MOVE 0 TO OLN-UNITPER-FREQ.
           IF LN-UNITPER-FREQ NOT = OLN-UNITPER-FREQ
              MOVE "LN-UNITPER-FREQ" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-UNITPER-FREQ  TO D01-A15
              MOVE LN-UNITPER-FREQ   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-DAYS-IN-UNITPER NOT NUMERIC
              MOVE 0 TO OLN-DAYS-IN-UNITPER.
           IF LN-DAYS-IN-UNITPER NOT = OLN-DAYS-IN-UNITPER
              MOVE "LN-DAYS-IN-UNITPER" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-DAYS-IN-UNITPER  TO D01-A15
              MOVE LN-DAYS-IN-UNITPER   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-UNITPER-PER-YEAR NOT NUMERIC
              MOVE 0 TO OLN-UNITPER-PER-YEAR.
           IF LN-UNITPER-PER-YEAR NOT = OLN-UNITPER-PER-YEAR
              MOVE "LN-UNITPER-PER-YEAR" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-UNITPER-PER-YEAR  TO D01-A15-NUM
              MOVE LN-UNITPER-PER-YEAR   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-REPOCD NOT = OLN-REPOCD
              MOVE "LN-REPOCD" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-REPOCD  TO D01-A15
              MOVE LN-REPOCD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-BR-TFR-FG NOT = OLN-BR-TFR-FG
              MOVE "LN-BR-TFR-FG" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-BR-TFR-FG  TO D01-A15
              MOVE LN-BR-TFR-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-O3INS-DLPROC NOT NUMERIC
              MOVE 0 TO OLN-O3INS-DLPROC.
           IF LN-O3INS-DLPROC NOT = OLN-O3INS-DLPROC
              MOVE "LN-O3INS-DLPROC" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-O3INS-DLPROC  TO D01-A15-NUM
              MOVE LN-O3INS-DLPROC   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-INSPREM-DLADJ NOT NUMERIC
              MOVE 0 TO OLN-INSPREM-DLADJ.
           IF LN-INSPREM-DLADJ NOT = OLN-INSPREM-DLADJ
              MOVE "LN-INSPREM-DLADJ" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-INSPREM-DLADJ  TO D01-A15-NUM
              MOVE LN-INSPREM-DLADJ   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-PRLNAMT NOT NUMERIC
              MOVE 0 TO OLN-PRLNAMT.
           IF LN-PRLNAMT NOT = OLN-PRLNAMT
              MOVE "LN-PRLNAMT" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-PRLNAMT  TO D01-A15-NUM
              MOVE LN-PRLNAMT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-PRPDUE-DAYS NOT NUMERIC
              MOVE 0 TO OLN-PRPDUE-DAYS.
           IF LN-PRPDUE-DAYS NOT = OLN-PRPDUE-DAYS
              MOVE "LN-PRPDUE-DAYS" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-PRPDUE-DAYS  TO D01-A15
              MOVE LN-PRPDUE-DAYS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-PLAMT NOT NUMERIC
              MOVE 0 TO OLN-PLAMT.
           IF LN-PLAMT NOT = OLN-PLAMT
              MOVE "LN-PLAMT" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-PLAMT  TO D01-A15-NUM
              MOVE LN-PLAMT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-MTD-PRIN NOT NUMERIC
              MOVE 0 TO OLN-MTD-PRIN.
           IF LN-MTD-PRIN NOT = OLN-MTD-PRIN
              MOVE "LN-MTD-PRIN" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-MTD-PRIN  TO D01-A15-NUM
              MOVE LN-MTD-PRIN   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-MTD-LC NOT NUMERIC
              MOVE 0 TO OLN-MTD-LC.
           IF LN-MTD-LC NOT = OLN-MTD-LC
              MOVE "LN-MTD-LC" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-MTD-LC  TO D01-A15-NUM
              MOVE LN-MTD-LC   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-MTD-INT NOT NUMERIC
              MOVE 0 TO OLN-MTD-INT.
           IF LN-MTD-INT NOT = OLN-MTD-INT
              MOVE "LN-MTD-INT" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-MTD-INT  TO D01-A15-NUM
              MOVE LN-MTD-INT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-MTD-OT NOT NUMERIC
              MOVE 0 TO OLN-MTD-OT.
           IF LN-MTD-OT NOT = OLN-MTD-OT
              MOVE "LN-MTD-OT" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-MTD-OT  TO D01-A15-NUM
              MOVE LN-MTD-OT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-MTD-EX NOT NUMERIC
              MOVE 0 TO OLN-MTD-EX.
           IF LN-MTD-EX NOT = OLN-MTD-EX
              MOVE "LN-MTD-EX" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-MTD-EX  TO D01-A15-NUM
              MOVE LN-MTD-EX   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-BNKRPT-CHAPTER NOT NUMERIC
              MOVE 0 TO OLN-BNKRPT-CHAPTER.
           IF LN-BNKRPT-CHAPTER NOT = OLN-BNKRPT-CHAPTER
              MOVE "LN-BNKRPT-CHAPTER" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-BNKRPT-CHAPTER  TO D01-A15
              MOVE LN-BNKRPT-CHAPTER   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-BMO-CON-STATUS NOT = OLN-BMO-CON-STATUS
              MOVE "LN-BMO-CON-STATUS" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-BMO-CON-STATUS  TO D01-A15
              MOVE LN-BMO-CON-STATUS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
           IF LN-BMO-REC-STATUS NOT = OLN-BMO-REC-STATUS
              MOVE "LN-BMO-REC-STATUS" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-BMO-REC-STATUS  TO D01-A15
              MOVE LN-BMO-REC-STATUS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-APPNO NOT NUMERIC OR (OLN-APPNO = 0)
              MOVE 0 TO OLN-APPNO WSS-MA-APPNO WSS-MA-SEQNO
           ELSE
              MOVE OLN-APPNO         TO WSS-MA-APPNO
              MOVE 01                TO WSS-MA-SEQNO.
      *    MOVE WSS-MA-APP-NUMBER TO LN-APP-NUMBER.
      *    IF LN-APP-NUMBER NOT = OLN-APPNO
           IF LN-APP-NUMBER NOT = WSS-MA-APP-NUMBER
              MOVE "LN-APPNO"         TO D01-TEXT
              MOVE BR-NO              TO D01-BRNO
              MOVE LN-ACCTNO          TO D01-ACCTNO
              MOVE WSS-MA-APP-NUMBER  TO D01-A15
              MOVE LN-APP-NUMBER      TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-GIFTNET NOT = OLN-GIFTNET
              MOVE "LN-GIFTNET" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-GIFTNET  TO D01-A15
              MOVE LN-GIFTNET   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-GIFTNET-OUR-PO NOT NUMERIC OR
               OLN-GIFTNET-OUR-PO = 202020
                 MOVE 0 TO OLN-GIFTNET-OUR-PO.
           IF LN-GIFTNET-OUR-PO NOT = OLN-GIFTNET-OUR-PO
              MOVE "LN-GIFTNET-OUR-PO" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-GIFTNET-OUR-PO  TO D01-A15
              MOVE LN-GIFTNET-OUR-PO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-PROC-DISBURSED NOT = OLN-PROC-DISBURSED
              MOVE "LN-PROC-DISBURSED" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-PROC-DISBURSED  TO D01-A15
              MOVE LN-PROC-DISBURSED   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
              IF OLN-CONPROFILE(SUB) NOT NUMERIC
                 MOVE 0 TO OLN-CONPROFILE(SUB)
              END-IF
              IF LN-CONPROFILE(SUB) NOT = OLN-CONPROFILE(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "LN-CONPROFILE",SUB INTO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE LN-ACCTNO  TO D01-ACCTNO
                 MOVE OLN-CONPROFILE(SUB)  TO D01-A15
                 MOVE LN-CONPROFILE(SUB)   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OLN-RECPROFILE(SUB) NOT NUMERIC
                 MOVE 0 TO OLN-RECPROFILE(SUB)
              END-IF
              IF LN-RECPROFILE(SUB) NOT = OLN-RECPROFILE(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "LN-RECPROFILE",SUB INTO D01-TEXT
                 MOVE BR-NO      TO D01-BRNO
                 MOVE LN-ACCTNO  TO D01-ACCTNO
                 MOVE OLN-RECPROFILE(SUB)  TO D01-A15
                 MOVE LN-RECPROFILE(SUB)   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.

           IF LN-TRW-STATUS NOT = OLN-TRW-STATUS
              MOVE "LN-TRW-STATUS" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-TRW-STATUS  TO D01-A15
              MOVE LN-TRW-STATUS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-ORIGIN NOT = OLN-ORIGIN
              MOVE "LN-ORIGIN" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-ORIGIN  TO D01-A15
              MOVE LN-ORIGIN   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-FOLLOWUP-DATE NOT NUMERIC
              MOVE 0 TO OLN-FOLLOWUP-DATE.
           MOVE OLN-FOLLOWUP-DATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LN-FOLLOWUP-DATE
              MOVE "LN-FOLLOWUP-DATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-FOLLOWUP-DATE  TO D01-A15
              MOVE LN-FOLLOWUP-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-PO-TRCD NOT = OLN-PO-TRCD
              MOVE "LN-PO-TRCD" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-PO-TRCD  TO D01-A15
              MOVE LN-PO-TRCD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-PRDLDISC-UNEARN NOT NUMERIC
              MOVE 0 TO OLN-PRDLDISC-UNEARN.
           IF LN-PRDLDISC-UNEARN NOT = OLN-PRDLDISC-UNEARN
              MOVE "LN-PRDLDISC-UNEARN" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-PRDLDISC-UNEARN  TO D01-A15-NUM
              MOVE LN-PRDLDISC-UNEARN   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-YTDNOALDEL NOT NUMERIC
              MOVE 0 TO OLN-YTDNOALDEL.
           IF LN-YTDNOALDEL NOT = OLN-YTDNOALDEL
              MOVE "LN-YTDNOALDEL" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-NEXIS-CRSCORE NOT NUMERIC
              MOVE 0                 TO OLN-NEXIS-CRSCORE.
           IF LN-NEXIS-CRSCORE NOT = OLN-NEXIS-CRSCORE
              MOVE "LN-NEXIS-CRSCORE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-NEXIS-CRSCORE  TO D01-A15
              MOVE LN-NEXIS-CRSCORE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

      * S35 HAS 000F IN IT SO I DONT THINK THE UPGRADE HAD THIS IN IT WHEN
      * WE RAN IT
           IF OLN-TRW-B-CONSUMER-IND = X"000F"
              MOVE SPACES TO OLN-TRW-B-CONSUMER-IND.
           IF LN-TRW-B-CONSUMER-IND NOT = OLN-TRW-B-CONSUMER-IND
              MOVE "LN-TRW-B-CONSUMER-IND" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-TRW-B-CONSUMER-IND  TO D01-A15
              MOVE LN-TRW-B-CONSUMER-IND   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-PARVHELD-PAID-FG NOT = "A" AND 
                                   NOT = "R" AND
                                   NOT = "V"
              MOVE SPACES TO OLN-PARVHELD-PAID-FG.
           IF LN-PARVHELD-PAID-FG NOT = OLN-PARVHELD-PAID-FG
              MOVE "LN-PARVHELD-PAID-FG" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-PARVHELD-PAID-FG  TO D01-A15
              MOVE LN-PARVHELD-PAID-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-ACCT-FROZEN NOT = OLN-ACCT-FROZEN
              MOVE "LN-ACCT-FROZEN" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-ACCT-FROZEN  TO D01-A15
              MOVE LN-ACCT-FROZEN   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-LAST-ACCRUAL-DATE = 105376
              MOVE 0 TO OLN-LAST-ACCRUAL-DATE.
           MOVE OLN-LAST-ACCRUAL-DATE TO DATE-MMDDYY.
           IF DATE-MMDDYY-DD > 31
              MOVE 0 TO OLN-LAST-ACCRUAL-DATE DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = LN-LAST-ACCRUAL-DATE
              MOVE "LN-LAST-ACCRUAL-DATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-LAST-ACCRUAL-DATE  TO D01-A15
              MOVE LN-LAST-ACCRUAL-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-TRW-SPECIAL-CODE NOT = OLN-TRW-SPECIAL-CODE
              MOVE "LN-TRW-SPECIAL-CODE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-TRW-SPECIAL-CODE  TO D01-A15
              MOVE LN-TRW-SPECIAL-CODE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-CREATION-STATUS NOT = OLN-CREATION-STATUS
              MOVE "LN-CREATION-STATUS" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-CREATION-STATUS  TO D01-A15
              MOVE LN-CREATION-STATUS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-TOTNODEFACT NOT NUMERIC
              MOVE 0 TO OLN-TOTNODEFACT.
           IF LN-TOTNODEFACT NOT = OLN-TOTNODEFACT
              MOVE "LN-TOTNODEFACT" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-TOTNODEFACT  TO D01-A15
              MOVE LN-TOTNODEFACT   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-YTDNODEFACT NOT NUMERIC
              MOVE 0 TO OLN-YTDNODEFACT.
           IF LN-YTDNODEFACT NOT = OLN-YTDNODEFACT
              MOVE "LN-YTDNODEFACT" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-YTDNODEFACT  TO D01-A15
              MOVE LN-YTDNODEFACT   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-TOTNOLCHG NOT NUMERIC
              MOVE 0 TO OLN-TOTNOLCHG.
           IF LN-TOTNOLCHG NOT = OLN-TOTNOLCHG
              MOVE "LN-TOTNOLCHG" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-TOTNOLCHG  TO D01-A15
              MOVE LN-TOTNOLCHG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-PRIOR-STMT-COUNTER NOT NUMERIC
              MOVE 0 TO OLN-PRIOR-STMT-COUNTER.
           IF LN-PRIOR-STMT-COUNTER NOT = OLN-PRIOR-STMT-COUNTER
              MOVE "LN-PRIOR-STMT-COUNTER" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-PRIOR-STMT-COUNTER  TO D01-A15
              MOVE LN-PRIOR-STMT-COUNTER   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-CONRATE NOT NUMERIC
              MOVE 0 TO OLN-CONRATE.
           IF LN-CONRATE NOT = OLN-CONRATE
              MOVE "LN-CONRATE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-CONRATE  TO D01-A15-NUM
              MOVE LN-CONRATE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-LBOX NOT = OLN-LBOX
              MOVE "LN-LBOX" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-EXTRA-PRIN-LFP NOT NUMERIC
              MOVE 0 TO OLN-EXTRA-PRIN-LFP.
           IF LN-EXTRA-PRIN-LFP NOT = OLN-EXTRA-PRIN-LFP
              MOVE "LN-EXTRA-PRIN-LFP" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-EXTRA-PRIN-LFP  TO D01-A15-NUM
              MOVE LN-EXTRA-PRIN-LFP   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-REMAIN-INT-SINCE-LFP NOT NUMERIC
              MOVE 0 TO OLN-REMAIN-INT-SINCE-LFP.
           IF LN-REMAIN-INT-SINCE-LFP NOT = OLN-REMAIN-INT-SINCE-LFP
              MOVE "LN-REMAIN-INT-SINCE-LFP" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-REMAIN-INT-SINCE-LFP  TO D01-A15-NUM
              MOVE LN-REMAIN-INT-SINCE-LFP   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-REMAIN-PRIN-SINCE-LFP NOT NUMERIC
              MOVE 0 TO OLN-REMAIN-PRIN-SINCE-LFP.
           IF LN-REMAIN-PRIN-SINCE-LFP NOT = OLN-REMAIN-PRIN-SINCE-LFP
              MOVE "LN-REMAIN-PRIN-SINCE-LFP" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-REMAIN-PRIN-SINCE-LFP  TO D01-A15-NUM
              MOVE LN-REMAIN-PRIN-SINCE-LFP   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-INTERIM-COUPON NOT = OLN-INTERIM-COUPON
              MOVE "LN-INTERIM-COUPON" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-INTERIM-COUPON  TO D01-A15
              MOVE LN-INTERIM-COUPON   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-DLCOMM-O3-CMSI NOT NUMERIC
              MOVE 0 TO OLN-DLCOMM-O3-CMSI.
           IF LN-DLCOMM-O3-CMSI NOT = OLN-DLCOMM-O3-CMSI
              MOVE "LN-DLCOMM-O3-CMSI" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-DLCOMM-O3-CMSI  TO D01-A15-NUM
              MOVE LN-DLCOMM-O3-CMSI   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OLN-DLCOMM-PP-CMSI NOT NUMERIC
              MOVE 0 TO OLN-DLCOMM-PP-CMSI.
           IF LN-DLCOMM-PP-CMSI NOT = OLN-DLCOMM-PP-CMSI
              MOVE "LN-DLCOMM-PP-CMSI" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-DLCOMM-PP-CMSI  TO D01-A15-NUM
              MOVE LN-DLCOMM-PP-CMSI   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-TRW-J-CONSUMER-IND NOT = OLN-TRW-J-CONSUMER-IND
              MOVE "LN-TRW-J-CONSUMER-IND" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-TRW-J-CONSUMER-IND  TO D01-A15
              MOVE LN-TRW-J-CONSUMER-IND   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-ACH NOT = OLN-ACH
              MOVE "LN-ACH" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-ACH  TO D01-A15
              MOVE LN-ACH   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-1099-C-FG NOT = OLN-1099-C-FG
              MOVE "LN-1099-C-FG" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-1099-C-FG  TO D01-A15
              MOVE LN-1099-C-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-TRW-COMPLIANCE-CODE NOT = OLN-TRW-COMPLIANCE-CODE
              MOVE "LN-TRW-COMPLIANCE-CODE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-TRW-COMPLIANCE-CODE  TO D01-A15
              MOVE LN-TRW-COMPLIANCE-CODE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LN-UNWIND-REASON-CODE NOT = OLN-UNWIND-REASON-CODE
              MOVE "LN-UNWIND-REASON-CODE" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE LN-ACCTNO  TO D01-ACCTNO
              MOVE OLN-UNWIND-REASON-CODE  TO D01-A15
              MOVE LN-UNWIND-REASON-CODE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

XXX   D    STOP MESS.

           PERFORM REWRITE-LN1-FILE.
           ADD 1 TO REWRITE-CTR.

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

           MOVE "TOTAL LNFILE ROWS UPDATED     :"     TO R01-PROMPT.
           MOVE REWRITE-CTR                           TO R01-NUMERIC.
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
           MOVE EXT-CONAME-CONAME TO LNFILE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LNFILE-SYSDATE.
           DISPLAY LNFILE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE LNFILE-FORM-FIELDS TO WR-BUF.
           MOVE LNFILE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE LNFILE-HELP--FILE TO HELP-LINK-FILE.
           MOVE LNFILE-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/LNFILE_EVA.CPY".
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
       TEST-LN-MESSAGE SECTION.
      ******************************************************************
           IF HOLD-LN-MESS-BYTE(SUB) NOT = SPACES
              IF HOLD-LN-MESS-BYTE(SUB) NOT ALPHABETIC
                 IF HOLD-LN-MESS-BYTE(SUB) NOT NUMERIC
                    MOVE HOLD-LN-MESS-BYTE(SUB) TO HOLD-BYTE
                    IF NOT VALID-SPECIAL-CHARS
                       MOVE SPACES TO HOLD-LN-MESS-BYTE(SUB).

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
       SET-HOLD-TILLNO.
           IF OLN-TILLNO = "1"
              MOVE 1  TO HOLD-LN-TILLNO 
           ELSE
           IF OLN-TILLNO = "2"
              MOVE 2  TO HOLD-LN-TILLNO 
           ELSE
           IF OLN-TILLNO = "3"
              MOVE 3  TO HOLD-LN-TILLNO 
           ELSE
           IF OLN-TILLNO = "4"
              MOVE 4  TO HOLD-LN-TILLNO 
           ELSE
           IF OLN-TILLNO = "5"
              MOVE 5  TO HOLD-LN-TILLNO 
           ELSE
           IF OLN-TILLNO = "6"
              MOVE 6  TO HOLD-LN-TILLNO 
           ELSE
           IF OLN-TILLNO = "7"
              MOVE 7  TO HOLD-LN-TILLNO 
           ELSE
           IF OLN-TILLNO = "8"
              MOVE 8  TO HOLD-LN-TILLNO 
           ELSE
           IF OLN-TILLNO = "9"
              MOVE 9  TO HOLD-LN-TILLNO 
           ELSE
           IF OLN-TILLNO = "A"
              MOVE 10 TO HOLD-LN-TILLNO 
           ELSE
           IF OLN-TILLNO = "B"
              MOVE 11 TO HOLD-LN-TILLNO 
           ELSE
           IF OLN-TILLNO = "C"
              MOVE 12 TO HOLD-LN-TILLNO 
           ELSE
           IF OLN-TILLNO = "D"
              MOVE 13 TO HOLD-LN-TILLNO 
           ELSE
           IF OLN-TILLNO = "E"
              MOVE 14 TO HOLD-LN-TILLNO 
           ELSE
           IF OLN-TILLNO = "F"
              MOVE 15 TO HOLD-LN-TILLNO 
           ELSE
           IF OLN-TILLNO = "G"
              MOVE 16 TO HOLD-LN-TILLNO 
           ELSE
           IF OLN-TILLNO = "H"
              MOVE 17 TO HOLD-LN-TILLNO 
           ELSE
           IF OLN-TILLNO = "I"
              MOVE 18 TO HOLD-LN-TILLNO 
           ELSE
           IF OLN-TILLNO = "J"
              MOVE 19 TO HOLD-LN-TILLNO 
           ELSE
           IF OLN-TILLNO = "K"
              MOVE 20 TO HOLD-LN-TILLNO 
           ELSE
           IF OLN-TILLNO = "L"
              MOVE 21 TO HOLD-LN-TILLNO 
           ELSE
           IF OLN-TILLNO = "M"
              MOVE 22 TO HOLD-LN-TILLNO 
           ELSE
           IF OLN-TILLNO = "N"
              MOVE 23 TO HOLD-LN-TILLNO 
           ELSE
           IF OLN-TILLNO = "O"
              MOVE 24 TO HOLD-LN-TILLNO 
           ELSE
           IF OLN-TILLNO = "P"
              MOVE 25 TO HOLD-LN-TILLNO 
           ELSE
           IF OLN-TILLNO = "Q"
              MOVE 26 TO HOLD-LN-TILLNO 
           ELSE
           IF OLN-TILLNO = "R"
              MOVE 27 TO HOLD-LN-TILLNO 
           ELSE
           IF OLN-TILLNO = "S"
              MOVE 28 TO HOLD-LN-TILLNO 
           ELSE
           IF OLN-TILLNO = "T"
              MOVE 29 TO HOLD-LN-TILLNO 
           ELSE
           IF OLN-TILLNO = "U"
              MOVE 30 TO HOLD-LN-TILLNO 
           ELSE
           IF OLN-TILLNO = "V"
              MOVE 31 TO HOLD-LN-TILLNO 
           ELSE
           IF OLN-TILLNO = "W"
              MOVE 32 TO HOLD-LN-TILLNO 
           ELSE
           IF OLN-TILLNO = "X"
              MOVE 33 TO HOLD-LN-TILLNO 
           ELSE
           IF OLN-TILLNO = "Y"
              MOVE 34 TO HOLD-LN-TILLNO 
           ELSE
           IF OLN-TILLNO = "Z"
              MOVE 35 TO HOLD-LN-TILLNO
           ELSE
              MOVE 0 TO HOLD-LN-TILLNO.
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
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

      * COPY "LIBLP/LPLN1RN.CPY".
      * COPYMEMBER: LIBLP/LPLN1RN
       LOAD-LN1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LN-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LN-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LN-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LN-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LN-ALL-DATA
              MOVE LN-ALL-PATH             TO LN-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LN-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LN-ALL-BASE
              MOVE FILPATH-MACHINE         TO LN-ALL-MACHINE
              MOVE FILPATH-DATA            TO LN-ALL-DATA
              MOVE LN-ALL-PATH             TO LN-PATH.

      *-----------------------------------------------------------------
       OPEN-LN1-FILE.
           PERFORM LOAD-LN1-FILE.
           PERFORM OPEN-IT.
           MOVE LN-PATH-SQL TO E-FILE.

           IF LN1-CONN-OPEN-SW = "N" 
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS LN1_CONNECT
                        USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE 'Y' TO LN1-CONN-OPEN-SW.

      *-----------------------------------------------------------------
       START-LN1-FILE.  *> SSP LNFILE
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE LN1-KEY        TO E-KEYX.

           IF ( LN1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LN1-FILE.

           EXEC SQL SET CONNECTION LN1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE LN1_CURSOR CURSOR FOR
                :LN1-RETURN-CODE =  
            EXEC SSP_LNFILE_LNFILE_SELECT
              ( :QLN1-WBEG-OWNBR,
                :QLN1-WBEG-ACCTNO, :QLN1-WEND-ACCTNO)
           END-EXEC.

      *     EXEC SQL
      *      DECLARE LN1_CURSOR CURSOR FOR
      *     SELECT *
      *     FROM DBO.LNFILE LNFILE
      *     WHERE LNFILE.LN_BRNO = @QLN1_WBEG_BRNO
      *     AND LNFILE.LN_ACCTNO  BETWEEN @QLN1_WBEG_ACCTNO  AND @QLN1_WEND_ACCTNO
      *     ORDER BY LNFILE.LN_ACCTNO
      *     END-EXEC.

           EXEC SQL
               OPEN LN1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE STAT---GOOD TO LN1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.
      *-----------------------------------------------------------------
       READ-LN1-FILE-NEXT.     *> SSP LNFILE
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE LN1-KEY        TO E-KEYX.
           INITIALIZE QLN-REC.

           EXEC SQL SET CONNECTION LN1_CONNECT END-EXEC.

           IF ( LN1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LN1_CURSOR INTO :QLN-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.
   
      *-----------------------------------------------------------------
       REWRITE-LN1-FILE. *> EMBEDDED LN

      *     MOVE LN-PATH-OWNBR  TO LN-OWNBR.
      *     ACCEPT LN-LTOUCH-DATE FROM CENTURY-DATE.
      *     PERFORM REWRITE-IT.
      *     MOVE LN-PATH-SQL    TO E-FILE.
      *     MOVE LN1-KEY        TO E-KEYX.
      *     PERFORM SET-LN-FIELDS.

           MOVE '2' TO SQL-UPDATE-CONN-USE-SW.    *> CHANGE CONNECTION TO UPDATE_CONNECT
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           EXEC SQL
            UPDATE DBO.LNFILE
               SET LNFILE.LN_APP_NUMBER = :QLN-APP-NUMBER 
             WHERE LNFILE.LN_OWNBR      = :QLN-OWNBR
               AND LNFILE.LN_ACCTNO     = :QLN-ACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE '1' TO SQL-UPDATE-CONN-USE-SW.    *> CHANGE CONNECTION BACK TO C1
           PERFORM SQL-SWAP-UPDATE-CONNECTION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-LN1-FILE.

      *     IF ( IO-FG = 0 )
      *       EXEC SQL COMMIT END-EXEC
      *       PERFORM SQL-IO-VALIDATION
      *        IF LN-BI-FG = "Y"
      *           MOVE "R" TO STAT
      *           PERFORM UPDATE-BI-FILE.
 
      *-----------------------------------------------------------------
       CLOSE-LN1-FILE.

           PERFORM CLOSE-IT.
           IF ( LN1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION LN1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE LN1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LN1-CURSOR-STAT
              EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-LN1-CONNECTION.

           IF LN1-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION LN1_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT LN1_CONNECT END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE "N" TO LN1-CONN-OPEN-SW.
      *-----------------------------------------------------------------


       OPEN-OLN1-FILE.
           PERFORM OPEN-IT.
           MOVE OLN-PATH TO E-FILE.
           OPEN INPUT OLN-FILE.
           IF IO-FG = 8
              GO TO OPEN-OLN1-FILE.
           IF IO-FG = 7
              CLOSE OLN-FILE
              GO TO OPEN-OLN1-FILE.
      *READ-OLN1-FILE.
      *    PERFORM READ-IT.
      *    MOVE OLN-PATH TO E-FILE.
      *    MOVE OLN1-KEY TO E-KEYX.
      *    READ OLN-FILE.
      *    IF IO-FG = 8
      *       GO TO READ-OLN1-FILE.
       START-OLN1-FILE.
           PERFORM START-IT.
           MOVE OLN-PATH TO E-FILE.
           MOVE OLN1-KEY TO E-KEYX.
           START OLN-FILE KEY NOT < OLN1-KEY.
       READ-OLN1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OLN-PATH TO E-FILE.
           MOVE OLN1-KEY TO E-KEYX.
           READ OLN-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OLN1-FILE-NEXT.
       CLOSE-OLN1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OLN-FILE.

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

      *     IF A15-RECORDS-PROCESSED = 0 AND SQL-RECORDS-PROCESSED = 0
      *        MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
      *        PERFORM SEND-MESS.

       EXIT-PROG.

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
              DESTROY LNFILE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
