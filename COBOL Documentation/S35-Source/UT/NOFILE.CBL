       IDENTIFICATION DIVISION.
       PROGRAM-ID.    NOFILE.
       DATE-WRITTEN.  10-31-2023.
      ******************************************************************
      *
      * DESCRIPTION   : COMPARE S35 NOFILE WITH A15 NOFILE
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
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
      * COPYMEMBER: LIBLP/LPFSNO
           SELECT ONO-FILE ASSIGN TO ONO-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY ONO1-KEY
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  ONO-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A15SRC/LP01NO.CPY" REPLACING LEADING "NO" BY "ONO".

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/NOFILE S35 01/24/25-12:51:08 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01NO.CPY".
       COPY "LIBLP/LP01NO_SQL.CPY".
       COPY "LIBLP/LPWSNO.CPY".
       01  ONO-PATH                    PIC X(35) VALUE SPACES.
       01  ONO-ALL-PATH.
           03  ONO-ALL-BASE     PIC X(9).
           03  FILLER           PIC X     VALUE "/".
           03  ONO-ALL-MACHINE  PIC XX.
           03  FILLER           PIC X     VALUE "/".
           03  ONO-ALL-DATA     PIC X(6).
           03  FILLER           PIC X(10) VALUE
               "/LP/NOFILE".

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

       01  MACHINE-R1.
           03  FILLER                  PIC X VALUE X"72".
           03  FILLER                  PIC 9 VALUE 1.
       01  MACHINE-R2.
           03  FILLER                  PIC X VALUE X"72".
           03  FILLER                  PIC 9 VALUE 2.

       01  HOLD-TILLNO                 PIC 9(2).
       01  HOLD-PAYEE                  PIC X(60).
       01  OLD-ACCTNO                  PIC 9(8).
       01  FILLER REDEFINES OLD-ACCTNO.
           03  NEW-ACCTNO              PIC 9(6).
           03  FILLER                  PIC 99.

       01  MISC-CURSOR-STAT            PIC X(2) VALUE SPACES.
           88  MISC-CURSOR-STAT-GOOD            VALUE "00".
           88  MISC-CURSOR-STAT-BAD             VALUE "  " THRU "//"
                                                      "01" THRU "~~". 

       01  PROCESS-STAT                PIC X(08) VALUE SPACES.
           88  PROCESS-STAT-GOOD                 VALUE "00".
           88  PROCESS-STAT-BAD                  VALUE "  " THRU "//"
                                                       "01" THRU "~~".
       01  RECORDS-PROCESSED           PIC 9(9)      VALUE 0.
           88  NO-RECORDS-FOUND                      VALUE ZEROES.

       01  STARTING-TIME               PIC X(08)     VALUE SPACES.
       01  SUB                         PIC 9(04)     VALUE ZEROES.

       01  SYSDATE-YYYYMMDD            PIC 9(8)      VALUE ZEROES.

       01  NO1-CONN-OPEN-SW            PIC X         VALUE "N".
       01  NO-EOF-SW                   PIC X         VALUE "N".
       01  ONO-EOF-SW                  PIC X         VALUE "N".

       01  WS-ONO1-KEY.
           03  WS-ONO-OWNBR            PIC 9(4).
           03  WS-ONO-ACCTNO           PIC 9(9).
  
       01  WS-ONO-ACCTNO1-9            PIC 9(9).
       01  FILLER                      REDEFINES WS-ONO-ACCTNO1-9.
           03 WS-ONO-ACCTNO1-7         PIC 9(7).
           02 WS-ONO-ACCTNO8-9         PIC 9(2).

       01  WS-NO1-KEY.
           03  WS-NO-OWNBR             PIC 9(4).
           03  WS-NO-ACCTNO            PIC 9(9).

       01  TOTAL-ERRORS-CTR            PIC 9(10)    VALUE ZEROES.  
       01  A15-ENTER-CTR               PIC 9(10)    VALUE ZEROES.  
       01  S35-RECORD-CTR              PIC 9(10)    VALUE ZEROES.  
       01  A15-RECORDS-PROCESSED       PIC 9(9)     VALUE ZEROES.
           88  NO-RECORDS-FOUND                     VALUE ZEROES.

       01  LEGEND                      PIC X(79)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(07)     VALUE "LEGEND.".

       01  WS-A15-LSTLPKEY             PIC X(17).    
       01  FILLER                      REDEFINES WS-A15-LSTLPKEY.
           03 WS-A15-LSTLPKEY1-12      PIC X(12).   
           03 WS-A15-LSTLPKEY13-17     PIC X(5).

       01  WS-S35-LSTLPKEY             PIC X(17).    
       01  FILLER                      REDEFINES WS-S35-LSTLPKEY.
           03 WS-S35-LSTLPKEY1-6       PIC X(6).
           03 WS-S35-LSTLPKEY7-12      PIC X(6).
           03 WS-S35-LSTLPKEY13        PIC X(1).   
           03 WS-S35-LSTLPKEY14-17     PIC X(4).

       01  WS-LSTLPKEY                 PIC X(17).
       01  FILLER                      REDEFINES WS-LSTLPKEY.
           03  WS-LSTLPKEY1-6          PIC X(6).
           03  WS-LSTLPKEY7-8          PIC X(2).
           03  WS-LSTLPKEY9-12         PIC X(4).
           03  WS-LSTLPKEY13-17        PIC X(5).

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
           03  BEG-ACCTNO              PIC 9(09).
           03  END-ACCTNO              PIC 9(09).
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
           "NOTICE FILE (NOFILE) COMPARE A15 TO S35".

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
           03  D01-ACCTNO                     PIC Z(9).
           03  FILLER                         PIC X(2).
           03  D01-TEXT                       PIC X(20).
           03  FILLER                         PIC X(3).
           03  D01-A15                        PIC X(20).
           03  FILLER REDEFINES D01-A15.
               05  D01-A15-NUM                PIC Z(7)9.9(3).
               05  FILLER                     PIC X(8).
           03  FILLER                         PIC X(2).
           03  D01-S35                        PIC X(25).
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
       COPY "LIBUT/NOFILE_DEF.CPY".
       COPY "LIBUT/NOFILE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/NOFILE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE ONO-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-NO1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE ONO-FILE.
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
           MOVE "NOFILE" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE NOFILE-QUEUE-POS TO Q-POS.
           MOVE NOFILE-COPIES-POS TO C-POS.
           MOVE NOFILE-PRU-POS   TO P-POS.
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


       ENTER-BEG-ACCTNO.

           MOVE "ENNN060BEG-ACCTNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F3-KEY ) GO TO ENTER-ALL-CKNO.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-ACCTNO.

           MOVE VDUNUM0 TO BEG-ACCTNO.

      *-----------------------------------------------------------------
       ENTER-END-ACCTNO.

           MOVE "ENNN060END-ACCTNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-ACCTNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-ACCTNO.

           MOVE VDUNUM0 TO END-ACCTNO.

           IF ( BEG-ACCTNO > END-ACCTNO )
              MOVE "INVALID CKNO RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-ACCTNO.

           GO TO ENTER-PARAMETERS-END.

      *-----------------------------------------------------------------
       ENTER-ALL-CKNO.

           MOVE "SNNN060BEG-ACCTNO."  TO VDU.
           MOVE ZEROES                TO BEG-ACCTNO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060END-ACCTNO."  TO VDU.
           MOVE 999999999             TO END-ACCTNO VDUNUM0.
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

           PERFORM PROCESS-BRANCH-DRIVER.

           GO TO EXECUTION-NEXT-GROUP.

       EXECUTION-MODULE-EXIT.
           EXIT.

     *********************************************************
       PROCESS-BRANCH-DRIVER SECTION.
     *********************************************************

           MOVE 9999                       TO PROGID-SLEEP-COUNTER.
           PERFORM SEND-PROGRESS-INDICATOR.

           MOVE SOURCE-PATH  TO ONO-ALL-BASE.
           MOVE MACHINE-R1   TO ONO-ALL-MACHINE.
           MOVE BR-DATA-PATH TO ONO-ALL-DATA.
           MOVE ONO-ALL-PATH TO ONO-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE MACHINE-R2   TO ONO-ALL-MACHINE
              MOVE ONO-ALL-PATH TO ONO-PATH ACCESS-BUF
              PERFORM ACCESS-CALL
              IF ( STAT-BAD )
                 MOVE "MISSING A15 FILE" TO D01-TEXT 
                 MOVE ONO-ALL-PATH       TO D01-A15
                 MOVE SPACES             TO D01-S35
                 PERFORM WRITE-DETAIL-LINE
                 GO TO PROCESS-BRANCH-DRIVER-END.


           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO NO-PATH-OVERRIDE.

           PERFORM OPEN-NO1-FILE.
           PERFORM OPEN-ONO1-FILE.

           MOVE BEG-ACCTNO             TO ONO-ACCTNO.
           MOVE 0                      TO ONO-FMNO.

           PERFORM START-ONO1-FILE.

           MOVE NO-PATH-OWNBR        TO QNO1-WBEG-BRNO.     
           MOVE BEG-ACCTNO           TO QNO1-WBEG-ACCTNO.   
           MOVE END-ACCTNO           TO QNO1-WEND-ACCTNO. 

           PERFORM START-NO1-FILE.

           PERFORM PROCESS-READ-ONO1.
           PERFORM PROCESS-READ-NO1.

       PROCESS-BRANCH-NEXT-NO.

           IF NO-EOF-SW  = "Y" AND ONO-EOF-SW = "Y"
              GO TO PROCESS-BRANCH-DRIVER-END.              

           IF WS-ONO1-KEY < WS-NO1-KEY 
              MOVE "MISSING S35 RECORD" TO D01-TEXT
              MOVE WS-ONO-OWNBR         TO D01-BRNO
              MOVE WS-ONO-ACCTNO        TO D01-ACCTNO 
              MOVE WS-ONO1-KEY          TO D01-A15
              MOVE WS-NO1-KEY           TO D01-S35
              PERFORM WRITE-DETAIL-LINE 
              PERFORM PROCESS-READ-ONO1 
              GO TO PROCESS-BRANCH-NEXT-NO.
 
           IF WS-NO1-KEY < WS-ONO1-KEY
              MOVE "MISSING A15 RECORD" TO D01-TEXT
              MOVE WS-NO-OWNBR          TO D01-BRNO
              MOVE WS-NO-ACCTNO         TO D01-ACCTNO
              MOVE WS-ONO1-KEY          TO D01-A15
              MOVE WS-NO1-KEY           TO D01-S35
              PERFORM WRITE-DETAIL-LINE       
              PERFORM PROCESS-READ-NO1 
              GO TO PROCESS-BRANCH-NEXT-NO.

      *      IF IO-BAD
      *         GO TO PROCESS-BRANCH-DRIVER-END.
      *
      *      PERFORM SEND-PROGRESS-INDICATOR.
      *
      *      IF ( ONO-ACCTNO < BEG-ACCTNO ) OR
      *         ( ONO-ACCTNO > END-ACCTNO )
      *         GO TO PROCESS-BRANCH-NEXT-NO.
      *
      *      ADD  01 TO RECORDS-PROCESSED.

           PERFORM COMPARE-FIELDS.

           PERFORM PROCESS-READ-ONO1.  
           PERFORM PROCESS-READ-NO1. 

           GO TO PROCESS-BRANCH-NEXT-NO.

       PROCESS-BRANCH-DRIVER-END.

           PERFORM CLOSE-NO1-FILE.

       PROCESS-BRANCH-DRIVER-EXIT.
           EXIT.

      ******************************************************************
       PROCESS-READ-ONO1 SECTION.
      ******************************************************************

           PERFORM READ-ONO1-FILE-NEXT.
           IF IO-BAD 
              MOVE 9999              TO WS-ONO-OWNBR
              MOVE 999999999         TO WS-ONO-ACCTNO
              MOVE "Y"               TO ONO-EOF-SW
              GO TO EXIT-PROCESS-READ-ONO1
           ELSE 
              MOVE NO-PATH-OWNBR     TO WS-ONO-OWNBR
              MOVE ONO-ACCTNO        TO WS-ONO-ACCTNO1-9 
              MOVE WS-ONO-ACCTNO1-7  TO WS-ONO-ACCTNO.  

           PERFORM SEND-PROGRESS-INDICATOR.

           IF ( WS-ONO-ACCTNO1-9 < BEG-ACCTNO ) OR
              ( WS-ONO-ACCTNO1-9 > END-ACCTNO )
              GO TO PROCESS-READ-ONO1.

           ADD 1 TO A15-RECORDS-PROCESSED.

       EXIT-PROCESS-READ-ONO1.
           EXIT.

      ******************************************************************
       PROCESS-READ-NO1 SECTION.
      ******************************************************************

           PERFORM READ-NO1-FILE-NEXT.
           IF IO-BAD 
              MOVE 9999          TO WS-NO-OWNBR
              MOVE 999999999     TO WS-NO-ACCTNO  
              MOVE "Y"           TO NO-EOF-SW
           ELSE 
              MOVE NO-PATH-OWNBR TO WS-NO-OWNBR
              MOVE NO-ACCTNO     TO WS-NO-ACCTNO 
              ADD 1              TO S35-RECORD-CTR.

       EXIT-PROCESS-READ-NO1.
           EXIT.


      ******************************************************************
       COMPARE-FIELDS SECTION.
      ******************************************************************

      *     MOVE NO-PATH-OWNBR     TO NO-BRNO.
      *     MOVE ONO-ACCTNO        TO NEW-ACCTNO.
      *     MOVE NEW-ACCTNO        TO NO-ACCTNO.
      *     PERFORM READ-NO1-FILE.
      *     IF IO-FG NOT = 0
      *        MOVE "MISSING S35 RECORD" TO D01-TEXT
      *        MOVE BR-NO      TO D01-BRNO
      *        MOVE NO-ACCTNO    TO D01-ACCTNO
      *        MOVE ONO-FMNO    TO D01-A15
      *        MOVE ONO-TDUE    TO D01-S35
      *        PERFORM WRITE-DETAIL-LINE
      *        GO TO EXIT-COMPARE-FIELDS.

           IF NO-BRNO NOT = ONO-BRNO
              MOVE "NO-BRNO"          TO D01-TEXT
              MOVE BR-NO              TO D01-BRNO
              MOVE NO-ACCTNO          TO D01-ACCTNO
              MOVE ONO-BRNO           TO D01-A15-NUM
              MOVE NO-BRNO            TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE.

           MOVE ONO-TDUE              TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = NO-TDUE-DATE
              MOVE "NO-TDUE-DATE"     TO D01-TEXT
              MOVE BR-NO              TO D01-BRNO
              MOVE NO-ACCTNO          TO D01-ACCTNO
              MOVE DATE-YYYYMMDD      TO D01-A15
              MOVE NO-TDUE-DATE       TO D01-S35 
              PERFORM WRITE-DETAIL-LINE.

           IF NO-TDUELC NOT = ONO-TDUELC
              MOVE "NO-TDUELC"        TO D01-TEXT
              MOVE BR-NO              TO D01-BRNO
              MOVE NO-ACCTNO          TO D01-ACCTNO
              MOVE ONO-TDUELC         TO D01-A15-NUM
              MOVE NO-TDUELC          TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE.

           IF NO-TDUEAMT NOT = ONO-TDUEAMT
              MOVE "NO-TDUEAMT"       TO D01-TEXT
              MOVE BR-NO              TO D01-BRNO
              MOVE NO-ACCTNO          TO D01-ACCTNO
              MOVE ONO-TDUEAMT        TO D01-A15-NUM
              MOVE NO-TDUEAMT         TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE.

           IF NO-TDUEPRIN NOT = ONO-TDUEPRIN
              MOVE "NO-TDUEPRIN"      TO D01-TEXT
              MOVE BR-NO              TO D01-BRNO
              MOVE NO-ACCTNO          TO D01-ACCTNO
              MOVE ONO-TDUEPRIN       TO D01-A15-NUM
              MOVE NO-TDUEPRIN        TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE.

           IF NO-TDUETBAL NOT = ONO-TDUETBAL
              MOVE "NO-TDUETBAL"      TO D01-TEXT
              MOVE BR-NO              TO D01-BRNO
              MOVE NO-ACCTNO          TO D01-ACCTNO
              MOVE ONO-TDUETBAL       TO D01-A15-NUM
              MOVE NO-TDUETBAL        TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE.

           IF NO-TDUEPOFF NOT = ONO-TDUEPOFF
              MOVE "NO-TDUEPOFF"      TO D01-TEXT
              MOVE BR-NO              TO D01-BRNO
              MOVE NO-ACCTNO          TO D01-ACCTNO
              MOVE ONO-TDUEPOFF       TO D01-A15-NUM
              MOVE NO-TDUEPOFF        TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE.

           MOVE ONO-LDUE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = NO-LDUE-DATE
              MOVE "NO-LDUE-DATE"     TO D01-TEXT
              MOVE BR-NO              TO D01-BRNO
              MOVE NO-ACCTNO          TO D01-ACCTNO
              MOVE DATE-YYYYMMDD      TO D01-A15
              MOVE NO-LDUE-DATE       TO D01-S35
              PERFORM WRITE-DETAIL-LINE.

           IF NO-LDUELC NOT = ONO-LDUELC
              MOVE "NO-LDUELC"        TO D01-TEXT
              MOVE BR-NO              TO D01-BRNO
              MOVE NO-ACCTNO          TO D01-ACCTNO
              MOVE ONO-LDUELC         TO D01-A15-NUM
              MOVE NO-LDUELC          TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE.

           IF NO-LDUEAMT NOT = ONO-LDUEAMT
              MOVE "NO-LDUEAMT"       TO D01-TEXT
              MOVE BR-NO              TO D01-BRNO
              MOVE NO-ACCTNO          TO D01-ACCTNO
              MOVE ONO-LDUEAMT        TO D01-A15-NUM
              MOVE NO-LDUEAMT         TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE.

           MOVE ONO-DATE-PAID-LAST    TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = NO-DATE-PAID-LAST
              MOVE "NO-DATE-PAID-LAST-DATE" 
                                      TO D01-TEXT
              MOVE BR-NO              TO D01-BRNO
              MOVE NO-ACCTNO          TO D01-ACCTNO
              MOVE DATE-YYYYMMDD      TO D01-A15
              MOVE NO-DATE-PAID-LAST  TO D01-S35
              PERFORM WRITE-DETAIL-LINE.

           IF NO-AMT-PAID-LAST NOT = ONO-AMT-PAID-LAST
              MOVE "NO-AMT-PAID-LAST" TO D01-TEXT
              MOVE BR-NO              TO D01-BRNO
              MOVE NO-ACCTNO          TO D01-ACCTNO
              MOVE ONO-AMT-PAID-LAST  TO D01-A15-NUM
              MOVE NO-AMT-PAID-LAST   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE.
 
           MOVE ONO-LSTLPKEY          TO WS-A15-LSTLPKEY.
           MOVE NO-LSTLPKEY           TO WS-S35-LSTLPKEY.
           MOVE WS-S35-LSTLPKEY7-12   TO WS-LSTLPKEY1-6.
           MOVE "01"                  TO WS-LSTLPKEY7-8.
           MOVE WS-S35-LSTLPKEY14-17  TO WS-LSTLPKEY9-12.
           MOVE SPACES                TO WS-LSTLPKEY13-17.

           IF WS-S35-LSTLPKEY = ZEROES 
              MOVE "0               " TO WS-LSTLPKEY.

           IF WS-LSTLPKEY NOT = WS-A15-LSTLPKEY
              MOVE "NO-LSTLPKEY"      TO D01-TEXT
              MOVE BR-NO              TO D01-BRNO
              MOVE NO-ACCTNO          TO D01-ACCTNO
              MOVE WS-A15-LSTLPKEY    TO D01-A15     
              MOVE WS-LSTLPKEY        TO D01-S35      
              PERFORM WRITE-DETAIL-LINE.

           MOVE ONO-NXDUE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = NO-NXDUE-DATE
              MOVE "NO-NXDUE-DATE"    TO D01-TEXT
              MOVE BR-NO              TO D01-BRNO
              MOVE NO-ACCTNO          TO D01-ACCTNO
              MOVE DATE-YYYYMMDD      TO D01-A15
              MOVE NO-NXDUE-DATE      TO D01-S35
              PERFORM WRITE-DETAIL-LINE.

           IF NO-NXDUEAMT NOT = ONO-NXDUEAMT
              MOVE "NO-NXDUEAMT"      TO D01-TEXT
              MOVE BR-NO              TO D01-BRNO
              MOVE NO-ACCTNO          TO D01-ACCTNO
              MOVE ONO-NXDUEAMT       TO D01-A15-NUM
              MOVE NO-NXDUEAMT        TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE.

           IF NO-NXDUELC NOT = ONO-NXDUELC
              MOVE "NO-NXDUELC"       TO D01-TEXT
              MOVE BR-NO              TO D01-BRNO
              MOVE NO-ACCTNO          TO D01-ACCTNO
              MOVE ONO-NXDUELC        TO D01-A15-NUM
              MOVE NO-NXDUELC         TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE.

           IF NO-TRUCONTR NOT = ONO-TRUCONTR
              MOVE "NO-TRUCONTR"      TO D01-TEXT
              MOVE BR-NO              TO D01-BRNO
              MOVE NO-ACCTNO          TO D01-ACCTNO
              MOVE ONO-TRUCONTR       TO D01-A15-NUM
              MOVE NO-TRUCONTR        TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE.

           IF NO-RECENCY NOT = ONO-RECENCY
              MOVE "NO-RECENCY"       TO D01-TEXT
              MOVE BR-NO              TO D01-BRNO
              MOVE NO-ACCTNO          TO D01-ACCTNO
              MOVE ONO-RECENCY        TO D01-A15-NUM
              MOVE NO-RECENCY         TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE.

           MOVE ONO-PDTH TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = NO-PDTH-DATE
              MOVE "NO-PDTH-DATE"     TO D01-TEXT
              MOVE BR-NO              TO D01-BRNO
              MOVE NO-ACCTNO          TO D01-ACCTNO
              MOVE DATE-YYYYMMDD      TO D01-A15
              MOVE NO-PDTH-DATE       TO D01-S35
              PERFORM WRITE-DETAIL-LINE.

           MOVE ONO-LCPDTH TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = NO-LCPDTH-DATE
              MOVE "NO-LCPDTH-DATE"   TO D01-TEXT
              MOVE BR-NO              TO D01-BRNO
              MOVE NO-ACCTNO          TO D01-ACCTNO
              MOVE DATE-YYYYMMDD      TO D01-A15
              MOVE NO-LCPDTH-DATE     TO D01-S35
              PERFORM WRITE-DETAIL-LINE.

           IF NO-LCHG-INTDUE NOT = ONO-LCHG-INTDUE
              MOVE "NO-LCHG-INTDUE"   TO D01-TEXT
              MOVE BR-NO              TO D01-BRNO
              MOVE NO-ACCTNO          TO D01-ACCTNO
              MOVE ONO-LCHG-INTDUE    TO D01-A15-NUM
              MOVE NO-LCHG-INTDUE     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE.

           IF NO-AVAILCREDIT NOT = ONO-AVAILCREDIT
              MOVE "NO-AVAILCREDIT"   TO D01-TEXT
              MOVE BR-NO              TO D01-BRNO
              MOVE NO-ACCTNO          TO D01-ACCTNO
              MOVE ONO-AVAILCREDIT    TO D01-A15-NUM
              MOVE NO-AVAILCREDIT     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE.

           IF NO-TDUEDELINQ NOT = ONO-TDUEDELINQ
              MOVE "NO-TDUEDELINQ"    TO D01-TEXT
              MOVE BR-NO              TO D01-BRNO
              MOVE NO-ACCTNO          TO D01-ACCTNO
              MOVE ONO-TDUEDELINQ     TO D01-A15-NUM
              MOVE NO-TDUEDELINQ      TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE.

           IF NO-DEFERMENT NOT = ONO-DEFERMENT
              MOVE "NO-DEFERMENT"     TO D01-TEXT
              MOVE BR-NO              TO D01-BRNO
              MOVE NO-ACCTNO          TO D01-ACCTNO
              MOVE ONO-DEFERMENT      TO D01-A15-NUM
              MOVE NO-DEFERMENT       TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE.

           MOVE ONO-TRUE-DUE-DATE     TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = NO-TRUE-DUE-DATE
              MOVE "NO-TRUE-DUE-DATE-DATE" 
                                      TO D01-TEXT
              MOVE BR-NO              TO D01-BRNO
              MOVE NO-ACCTNO          TO D01-ACCTNO
              MOVE DATE-YYYYMMDD      TO D01-A15
              MOVE NO-TRUE-DUE-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE.

           IF NO-SCHED-PAYMENT NOT = ONO-SCHED-PAYMENT
              MOVE "NO-SCHED-PAYMENT" TO D01-TEXT
              MOVE BR-NO              TO D01-BRNO
              MOVE NO-ACCTNO          TO D01-ACCTNO
              MOVE ONO-SCHED-PAYMENT  TO D01-A15-NUM
              MOVE NO-SCHED-PAYMENT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE.

           IF NO-TDUECONTR NOT = ONO-TDUECONTR
              MOVE "NO-TDUECONTR"     TO D01-TEXT
              MOVE BR-NO              TO D01-BRNO
              MOVE NO-ACCTNO          TO D01-ACCTNO
              MOVE ONO-TDUECONTR      TO D01-A15-NUM
              MOVE NO-TDUECONTR       TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE.

           IF NO-LDUECONTR NOT = ONO-LDUECONTR
              MOVE "NO-LDUECONTR"     TO D01-TEXT
              MOVE BR-NO              TO D01-BRNO
              MOVE NO-ACCTNO          TO D01-ACCTNO
              MOVE ONO-LDUECONTR      TO D01-A15-NUM
              MOVE NO-LDUECONTR       TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE.

           IF NO-NXDUECONTR NOT = ONO-NXDUECONTR
              MOVE "NO-NXDUECONTR"    TO D01-TEXT
              MOVE BR-NO              TO D01-BRNO
              MOVE NO-ACCTNO          TO D01-ACCTNO
              MOVE ONO-NXDUECONTR     TO D01-A15-NUM
              MOVE NO-NXDUECONTR      TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE.

           IF NO-CURRENT-AVAIL NOT = ONO-CURRENT-AVAIL
              MOVE "NO-CURRENT-AVAIL" TO D01-TEXT
              MOVE BR-NO              TO D01-BRNO
              MOVE NO-ACCTNO          TO D01-ACCTNO
              MOVE ONO-CURRENT-AVAIL  TO D01-A15-NUM
              MOVE NO-CURRENT-AVAIL   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE.

           IF NO-LATE-CHG NOT = ONO-LATE-CHG
              MOVE "NO-LATE-CHG"      TO D01-TEXT
              MOVE BR-NO              TO D01-BRNO
              MOVE NO-ACCTNO          TO D01-ACCTNO
              MOVE ONO-LATE-CHG       TO D01-A15-NUM
              MOVE NO-LATE-CHG        TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE.

           IF NO-PAST-DUE-DAYS NOT = ONO-PAST-DUE-DAYS
              MOVE "NO-PAST-DUE-DAYS" TO D01-TEXT
              MOVE BR-NO              TO D01-BRNO
              MOVE NO-ACCTNO          TO D01-ACCTNO
              MOVE ONO-PAST-DUE-DAYS  TO D01-A15-NUM
              MOVE NO-PAST-DUE-DAYS   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE.


       EXIT-COMPARE-FIELDS.
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

           MOVE "BEGINNING ACCTNO              : "    TO R01-PROMPT.
           MOVE BEG-ACCTNO                            TO R01-NUMERIC.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    ACCTNO              : "    TO R01-PROMPT.
           MOVE END-ACCTNO                            TO R01-NUMERIC.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "A15 RECORDS PROCESSED         :"     TO R01-PROMPT.
           MOVE A15-RECORDS-PROCESSED                 TO R01-NUMERIC.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "S35 RECORDS PROCESSED         :"     TO R01-PROMPT.
           MOVE S35-RECORD-CTR                        TO R01-NUMERIC.
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
           MOVE EXT-CONAME-CONAME TO NOFILE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO NOFILE-SYSDATE.
           DISPLAY NOFILE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE NOFILE-FORM-FIELDS TO WR-BUF.
           MOVE NOFILE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE NOFILE-HELP--FILE TO HELP-LINK-FILE.
           MOVE NOFILE-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/NOFILE_EVA.CPY".
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
       COPY "LIBLP/LPNOGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
      * COPY "LIBLP/LPNO1RN.CPY".
      * COPYMEMBER: LIBLP/LPNO1RN
      * BAH 2024.0201 REMOVED NO-FMNO, NO-PRINTED, NO-WAGE-ASSIGN-DELINQ
      *               ADDED FIELDS AT THE END OF RECORD
       LOAD-NO1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( NO-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( NO-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO NO-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO NO-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO NO-ALL-DATA
              MOVE NO-ALL-PATH             TO NO-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( NO-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO NO-ALL-BASE
              MOVE FILPATH-MACHINE         TO NO-ALL-MACHINE
              MOVE FILPATH-DATA            TO NO-ALL-DATA
              MOVE NO-ALL-PATH             TO NO-PATH.

      *-----------------------------------------------------------------
       OPEN-NO1-FILE.
           PERFORM LOAD-NO1-FILE.
           PERFORM OPEN-IT.
           MOVE NO-PATH-SQL TO E-FILE.

           IF NO1-CONN-OPEN-SW = "N" 
              MOVE "Y"         TO NO1-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS NO1_CONNECT 
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-NO1-FILE.

           PERFORM START-IT.
           MOVE NO-PATH-OWNBR  TO NO-BRNO.
           MOVE NO-PATH-SQL    TO E-FILE.
           MOVE NO1-KEY        TO E-KEYX.

           IF ( NO1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-NO1-FILE.

           IF ( QNO1-WEND-ACCTNO = ZEROES    ) OR
              ( QNO1-WEND-ACCTNO NOT NUMERIC )
              MOVE NO-PATH-OWNBR        TO QNO1-WBEG-BRNO
              MOVE NO-PATH-OWNBR        TO QNO1-WEND-BRNO
              MOVE NO-ACCTNO            TO QNO1-WBEG-ACCTNO
              MOVE ALL "9"              TO QNO1-WEND-ACCTNO.

           EXEC SQL SET CONNECTION NO1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE NO1_CURSOR CURSOR FOR
                :NO1-RETURN-CODE = EXEC SSP_NOFILE_NOFILE_SELECT 
              ( :QNO1-WBEG-BRNO,     
                :QNO1-WBEG-ACCTNO,   
                :QNO1-WEND-ACCTNO )      
           END-EXEC.

           EXEC SQL
               OPEN NO1_CURSOR
           END-EXEC.

      *     EXEC SQL
      *      DECLARE NO1_CURSOR CURSOR FOR
      *      SELECT NOFILE.NO_BRNO,
      *             NOFILE.NO_ACCTNO,
      *             CAST(NOFILE.NO_LTOUCH_DATE
      *                                            AS VARCHAR(10)),
      *             CAST(NOFILE.NO_TDUE_DATE
      *                                            AS VARCHAR(10)),
      *             NOFILE.NO_TDUELC,
      *             NOFILE.NO_TDUEAMT,
      *             NOFILE.NO_TDUEPRIN,
      *             NOFILE.NO_TDUETBAL,
      *             NOFILE.NO_TDUEPOFF,
      *             CAST(NOFILE.NO_LDUE_DATE
      *                                            AS VARCHAR(10)),
      *             NOFILE.NO_LDUELC,
      *             NOFILE.NO_LDUEAMT,
      *             CAST(NOFILE.NO_DATE_PAID_LAST
      *                                            AS VARCHAR(10)),
      *             NOFILE.NO_AMT_PAID_LAST,
      *             NOFILE.NO_LSTLPKEY,
      *             CAST(NOFILE.NO_NXDUE_DATE
      *                                            AS VARCHAR(10)),
      *             NOFILE.NO_NXDUEAMT,
      *             NOFILE.NO_NXDUELC,
      *             NOFILE.NO_TRUCONTR,
      *             NOFILE.NO_RECENCY,
      *             CAST(NOFILE.NO_PDTH_DATE
      *                                            AS VARCHAR(10)),
      *             CAST(NOFILE.NO_LCPDTH_DATE
      *                                            AS VARCHAR(10)),
      *             NOFILE.NO_LCHG_INTDUE,
      *             NOFILE.NO_AVAILCREDIT,
      *             NOFILE.NO_TDUEDELINQ,
      *             NOFILE.NO_DEFERMENT,
      *             CAST(NOFILE.NO_TRUE_DUE_DATE
      *                                            AS VARCHAR(10)),
      *             NOFILE.NO_SCHED_PAYMENT,
      *             NOFILE.NO_TDUECONTR,
      *             NOFILE.NO_LDUECONTR,
      *             NOFILE.NO_NXDUECONTR,
      *             NOFILE.NO_CURRENT_AVAIL,
      *             NOFILE.NO_LATE_CHG,
      *             NOFILE.NO_PAST_DUE_DAYS,
      *             NOFILE.NO_RECDEL,
      *             NOFILE.NO_AVCR_RESTRICT_FG,
      *             NOFILE.NO_INDU_PENALTY,
      *             NOFILE.NO_CPOT30,
      *             NOFILE.NO_RDPOT30,
      *             NOFILE.NO_CPOTCHGOFF,
      *             NOFILE.NO_RDPOTCHGOFF,
      *             NOFILE.NO_CSUB,
      *             NOFILE.NO_RSUB
      *      FROM DBO.NOFILE
      *      WHERE NOFILE.NO_BRNO  = :QNO1-WBEG-BRNO
      *        AND NOFILE.NO_ACCTNO >= :QNO1-WBEG-ACCTNO
      *        AND NOFILE.NO_ACCTNO <= :QNO1-WEND-ACCTNO
      *      ORDER BY NOFILE.NO_BRNO,
      *               NOFILE.NO_ACCTNO
      *     END-EXEC.
      *
      *     EXEC SQL
      *         OPEN NO1_CURSOR
      *     END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QNO1-WBEG-BRNO 
                          QNO1-WEND-BRNO
                          QNO1-WBEG-ACCTNO
                          QNO1-WEND-ACCTNO.

           MOVE STAT---GOOD TO NO1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-NO1-FILE.
           PERFORM READ-IT.
           MOVE NO-PATH-OWNBR  TO NO-BRNO. 
           MOVE NO-PATH-SQL    TO E-FILE.
           MOVE NO1-KEY        TO E-KEYX.
           INITIALIZE QNO-REC.

           MOVE NO-BRNO   TO QNO-BRNO.
           MOVE NO-ACCTNO TO QNO-ACCTNO.

           EXEC SQL
            SELECT NOFILE.NO_BRNO,
                   NOFILE.NO_ACCTNO,
                   CAST(NOFILE.NO_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   CAST(NOFILE.NO_TDUE_DATE
                                                  AS VARCHAR(10)),
                   NOFILE.NO_TDUELC,
                   NOFILE.NO_TDUEAMT,
                   NOFILE.NO_TDUEPRIN,
                   NOFILE.NO_TDUETBAL,
                   NOFILE.NO_TDUEPOFF,
                   CAST(NOFILE.NO_LDUE_DATE
                                                  AS VARCHAR(10)),
                   NOFILE.NO_LDUELC,
                   NOFILE.NO_LDUEAMT,
                   CAST(NOFILE.NO_DATE_PAID_LAST
                                                  AS VARCHAR(10)),
                   NOFILE.NO_AMT_PAID_LAST,
                   NOFILE.NO_LSTLPKEY,
                   CAST(NOFILE.NO_NXDUE_DATE
                                                  AS VARCHAR(10)),
                   NOFILE.NO_NXDUEAMT,
                   NOFILE.NO_NXDUELC,
                   NOFILE.NO_TRUCONTR,
                   NOFILE.NO_RECENCY,
                   CAST(NOFILE.NO_PDTH_DATE
                                                  AS VARCHAR(10)),
                   CAST(NOFILE.NO_LCPDTH_DATE
                                                  AS VARCHAR(10)),
                   NOFILE.NO_LCHG_INTDUE,
                   NOFILE.NO_AVAILCREDIT,
                   NOFILE.NO_TDUEDELINQ,
                   NOFILE.NO_DEFERMENT,
                   CAST(NOFILE.NO_TRUE_DUE_DATE
                                                  AS VARCHAR(10)),
                   NOFILE.NO_SCHED_PAYMENT,
                   NOFILE.NO_TDUECONTR,
                   NOFILE.NO_LDUECONTR,
                   NOFILE.NO_NXDUECONTR,
                   NOFILE.NO_CURRENT_AVAIL,
                   NOFILE.NO_LATE_CHG,
                   NOFILE.NO_PAST_DUE_DAYS,
                   NOFILE.NO_RECDEL,
                   NOFILE.NO_AVCR_RESTRICT_FG,
                   NOFILE.NO_INDU_PENALTY,
                   NOFILE.NO_CPOT30,
                   NOFILE.NO_RDPOT30,
                   NOFILE.NO_CPOTCHGOFF,
                   NOFILE.NO_RDPOTCHGOFF,
                   NOFILE.NO_CSUB,
                   NOFILE.NO_RSUB
            INTO :QNO-REC
            FROM DBO.NOFILE
            WHERE NOFILE.NO_BRNO = :QNO-BRNO
              AND NOFILE.NO_ACCTNO = :QNO-ACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-NO1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-NO-FIELDS.

      *-----------------------------------------------------------------
       READ-NO1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE NO-PATH-OWNBR  TO NO-BRNO.
           MOVE NO-PATH-SQL    TO E-FILE.
           MOVE NO1-KEY        TO E-KEYX.
           INITIALIZE QNO-REC.

           EXEC SQL SET CONNECTION NO1_CONNECT END-EXEC.

           IF ( NO1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH NO1_CURSOR INTO :QNO-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-NO1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-NO-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-NO1-FILE.

           PERFORM CLOSE-IT.
           IF ( NO1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION NO1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE NO1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO NO1-CURSOR-STAT.

      *----------------------------------------------------------------- 
  
       OPEN-ONO1-FILE.
           PERFORM OPEN-IT.
           MOVE ONO-PATH TO E-FILE.
           OPEN INPUT ONO-FILE.
           IF IO-FG = 8
              GO TO OPEN-ONO1-FILE.
           IF IO-FG = 7
              CLOSE ONO-FILE
              GO TO OPEN-ONO1-FILE.
      *READ-ONO1-FILE.
      *    PERFORM READ-IT.
      *    MOVE ONO-PATH TO E-FILE.
      *    MOVE ONO1-KEY TO E-KEYX.
      *    READ ONO-FILE.
      *    IF IO-FG = 8
      *       GO TO READ-ONO1-FILE.
       READ-ONO1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE ONO-PATH TO E-FILE.
           MOVE ONO1-KEY TO E-KEYX.
           READ ONO-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-ONO1-FILE-NEXT.
       START-ONO1-FILE.
           PERFORM START-IT.
           MOVE ONO-PATH TO E-FILE.
           MOVE ONO1-KEY TO E-KEYX.
           START ONO-FILE KEY NOT < ONO1-KEY.
       CLOSE-ONO1-FILE.
           PERFORM CLOSE-IT.
           CLOSE ONO-FILE.

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
              DESTROY NOFILE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
