       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CKFILE.
       DATE-WRITTEN.  10-31-2023.
      ******************************************************************
      *
      * DESCRIPTION   : COMPARE S35 CKFILE WITH A15 CKFILE
      *            
      *                 TO BE USED AFTER AN UPDATE
      *                 WILL COMPARE EACH FIELD AND REPORT ANY 
      *                 DIFFERENCES, AS LONG AS FIELD EXIST IN BOTH FILES
      *
      *  LOOPS THRU THE A15 FILE AND VERIFIES THE S35 FILE EXIST AND MATCHES
      *
      * UPGRADE HAD A BUG WITH CK-PAYEE, THEY WONT MATCH AND THERE IS NO
      * WAY I CAN DUPLICATE IT. IT DIDNT CLEAR A FIELD BEFORE A STRING
      * THIS PROGRAM WILL NOT COMPARE PAYEE, IT IS MESSED UP!!!!
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
      * COPYMEMBER: LIBLP/LPFSCK
           SELECT OCK-FILE ASSIGN TO OCK-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OCK1-KEY
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  OCK-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A15SRC/LP01CK.CPY" REPLACING LEADING "CK" BY "OCK".

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/CKFILE S35 10/31/23-12:58:54 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01CK.CPY".
       COPY "LIBLP/LP01CK_SQL.CPY".
       COPY "LIBLP/LPWSCK.CPY".
       01  OCK-PATH                    PIC X(35) VALUE SPACES.
       01  OCK-ALL-PATH.
           03  OCK-ALL-BASE     PIC X(9).
           03  FILLER           PIC X     VALUE "/".
           03  OCK-ALL-MACHINE  PIC XX.
           03  FILLER           PIC X     VALUE "/".
           03  OCK-ALL-DATA     PIC X(6).
           03  FILLER           PIC X(10) VALUE
               "/LP/CKFILE".

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


       01  CK1-CONN-OPEN-SW     PIC X     VALUE "N".

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
   
       01  PROCESS-STAT                PIC X(08)   VALUE SPACES.
           88  PROCESS-STAT-GOOD                   VALUE "00".
           88  PROCESS-STAT-BAD                    VALUE "  " THRU "//"
                                                         "01" THRU "~~".

       01  TOTAL-ERRORS-CTR            PIC 9(9)    VALUE 0.
       01  SQL-RECORD-CTR              PIC 9(9)    VALUE 0.
       01  A15-RECORD-CTR              PIC 9(9)    VALUE 0.
           88  NO-RECORDS-FOUND                    VALUE ZEROES.
  
       01  STARTING-TIME               PIC X(08)   VALUE SPACES.
       01  SUB                         PIC 9(04)   VALUE ZEROES.

       01  SYSDATE-YYYYMMDD            PIC 9(8)    VALUE ZEROES.

       01  CK-EOF-SW                   PIC X       VALUE "N".
       01  OCK-EOF-SW                  PIC X       VALUE "N".

       01  WS-CK1-KEY.
           05  WS-CK-BRNO              PIC 9(4).
           05  WS-CK-CKNO              PIC 9(6).
 
       01  WS-OCK1-KEY.
           05  WS-OCK-BRNO             PIC 9(4).
           05  WS-OCK-CKNO             PIC 9(6).

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
           03  BEG-CKNO                PIC 9(06).
           03  END-CKNO                PIC 9(06).
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
           03  FILLER                  PIC X(6)  VALUE SPACES.
           03  FILLER                  PIC X(06) VALUE "PAGE: ".
           03  H01-PAGE                PIC Z(04).

      *-----------------------------------------------------------------
       01  HEADER-LINE-02.
           03  FILLER                  PIC X(10) VALUE "USER ID:".
           03  H02-USERID              PIC X(10) VALUE SPACES.
           03  FILLER                  PIC X(10) VALUE SPACES.
           03  H02-TITLE               PIC X(50) VALUE 
           "CHECK FILE (CKFILE) COMPARE A15 TO S35".

       01  HEADER-LINE-03.
           03  FILLER                PIC X(4)  VALUE "BRNO".
           03  FILLER                PIC X(5)  VALUE SPACES.
           03  FILLER                PIC X(6)  VALUE "CKNO".
           03  FILLER                PIC X(5)  VALUE SPACES.
           03  FILLER                PIC X(11) VALUE "DESCRIPTION".
           03  FILLER                PIC X(17) VALUE SPACES.
           03  FILLER                PIC X(3)  VALUE "A15".
           03  FILLER                PIC X(19) VALUE SPACES.
           03  FILLER                PIC X(3)  VALUE "S35".

      ******************************************************************
      *
      *    D E T A I L - L I N E - W O R K E R S
      *
      ******************************************************************
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-BRNO                       PIC Z(3)9.
           03  FILLER                         PIC X(5).
           03  D01-CKNO                       PIC Z(9).
           03  FILLER                         PIC X(2).
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
       COPY "LIBUT/CKFILE_DEF.CPY".
       COPY "LIBUT/CKFILE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/CKFILE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE OCK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-CK1-FILE.
           PERFORM CLOSE-CK1-CONNECTION.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE OCK-FILE.
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
           MOVE "CKFILE" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE CKFILE-QUEUE-POS TO Q-POS.
           MOVE CKFILE-COPIES-POS TO C-POS.
           MOVE CKFILE-PRU-POS   TO P-POS.
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
              GO TO ENTER-END-CKNO.

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


       ENTER-BEG-CKNO.

           MOVE "ENNN060BEG-CKNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F3-KEY ) GO TO ENTER-ALL-CKNO.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-CKNO.

           MOVE VDUNUM0 TO BEG-CKNO.

      *-----------------------------------------------------------------
       ENTER-END-CKNO.

           MOVE "ENNN060END-CKNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-CKNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-CKNO.

           MOVE VDUNUM0 TO END-CKNO.

           IF ( BEG-CKNO > END-CKNO )
              MOVE "INVALID CKNO RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-CKNO.

           GO TO ENTER-PARAMETERS-END.

      *-----------------------------------------------------------------
       ENTER-ALL-CKNO.

           MOVE "SNNN060BEG-CKNO."  TO VDU.
           MOVE ZEROES                TO BEG-CKNO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060END-CKNO."  TO VDU.
           MOVE 999999                TO END-CKNO VDUNUM0.
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

           MOVE "SNNN060BEG-CKNO."       TO VDU.
           MOVE BEG-CKNO                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060END-CKNO."       TO VDU.
           MOVE END-CKNO                 TO VDUNUM0.
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

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO CK-PATH-OVERRIDE.

      D    STOP MESS.
           MOVE SOURCE-PATH  TO OCK-ALL-BASE.
           MOVE MACHINE-R1   TO OCK-ALL-MACHINE.
           MOVE BR-DATA-PATH TO OCK-ALL-DATA.
           MOVE OCK-ALL-PATH TO OCK-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE MACHINE-R2   TO OCK-ALL-MACHINE
              MOVE OCK-ALL-PATH TO OCK-PATH ACCESS-BUF
              PERFORM ACCESS-CALL
              IF ( STAT-BAD )
                 MOVE "MISSING A15 FILE" TO D01-TEXT
                 MOVE BR-NO              TO D01-BRNO
                 MOVE OCK-ALL-PATH       TO D01-A15
                 PERFORM WRITE-DETAIL-LINE-ERROR
                 GO TO PROCESS-BRANCH-DRIVER-END.

           IF EXT-ROUTE-BUF = "00"
              STRING "REPORT IN PROGRESS......", BR-NO INTO LEGEND
              PERFORM SEND-LEGEND.

           MOVE 'N' TO CK-EOF-SW OCK-EOF-SW. 

           PERFORM OPEN-OCK1-FILE.
           PERFORM OPEN-CK1-FILE.

           MOVE BR-NO      TO OCK-BRNO.
           MOVE BEG-CKNO   TO OCK-NO.
           PERFORM START-OCK1-FILE.

           MOVE BR-NO        TO QCK1-WBEG-BRNO.
           MOVE BEG-CKNO     TO QCK1-WBEG-CKNO.
           MOVE END-CKNO     TO QCK1-WEND-CKNO.
           PERFORM START-CK1-FILE.

           PERFORM PROCESS-READ-OCK1.
           PERFORM PROCESS-READ-CK1.

       PROCESS-BRANCH-NEXT-CK.

           IF CK-EOF-SW  = "Y" AND OCK-EOF-SW = "Y"
              GO TO PROCESS-BRANCH-DRIVER-EXIT.

           IF WS-OCK1-KEY < WS-CK1-KEY 
              MOVE "MISSING S35 RECORD" TO D01-TEXT
              MOVE OCK-BRNO             TO D01-BRNO  
              MOVE OCK-NO               TO D01-CKNO 
              MOVE WS-OCK1-KEY          TO D01-A15
              MOVE WS-CK1-KEY           TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR 
              PERFORM PROCESS-READ-OCK1 
              GO TO PROCESS-BRANCH-NEXT-CK.
 
           IF WS-CK1-KEY < WS-OCK1-KEY
              MOVE "MISSING A15 RECORD" TO D01-TEXT
              MOVE CK-BRNO              TO D01-BRNO  
              MOVE CK-NO                TO D01-CKNO 
              MOVE WS-OCK1-KEY          TO D01-A15
              MOVE WS-CK1-KEY           TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR       
              PERFORM PROCESS-READ-CK1 
              GO TO PROCESS-BRANCH-NEXT-CK.

           PERFORM COMPARE-FIELDS.

           PERFORM PROCESS-READ-OCK1.
           PERFORM PROCESS-READ-CK1.

           GO TO PROCESS-BRANCH-NEXT-CK.

       PROCESS-BRANCH-DRIVER-END.

           PERFORM CLOSE-CK1-FILE.
           PERFORM CLOSE-OCK1-FILE.

       PROCESS-BRANCH-DRIVER-EXIT.
           EXIT.

      ******************************************************************
       PROCESS-READ-OCK1 SECTION.
      ******************************************************************

           PERFORM READ-OCK1-FILE-NEXT.
           IF IO-BAD 
              MOVE 9999           TO WS-OCK-BRNO
              MOVE 999999         TO WS-OCK-CKNO
              MOVE "Y"            TO OCK-EOF-SW
              GO TO EXIT-PROCESS-READ-OCK1
           ELSE 
              IF ( OCK-NO < BEG-CKNO ) OR
                 ( OCK-NO > END-CKNO )
                 GO TO PROCESS-READ-OCK1
              END-IF
              MOVE CK-PATH-OWNBR  TO WS-OCK-BRNO
              MOVE OCK-NO         TO WS-OCK-CKNO
              ADD 1 TO A15-RECORD-CTR.

       EXIT-PROCESS-READ-OCK1.
           EXIT.

      ******************************************************************
       PROCESS-READ-CK1 SECTION.
      ******************************************************************

           PERFORM READ-CK1-FILE-NEXT.
           IF IO-BAD 
              MOVE 9999           TO WS-CK-BRNO
              MOVE 999999         TO WS-CK-CKNO
              MOVE "Y"            TO CK-EOF-SW
           ELSE 
              MOVE CK-PATH-OWNBR  TO WS-CK-BRNO
              MOVE CK-NO          TO WS-CK-CKNO
              ADD 1 TO SQL-RECORD-CTR.

       EXIT-PROCESS-READ-CK1.
           EXIT.

      ******************************************************************
       COMPARE-FIELDS SECTION.
      ******************************************************************

      *    MOVE OCK-BRNO TO CK-BRNO.
      *    MOVE OCK-NO   TO CK-NO.
      *    PERFORM READ-CK1-FILE.
      *    IF IO-FG NOT = 0
      *       MOVE "MISSING S35 RECORD" TO D01-TEXT
      *       MOVE BR-NO      TO D01-BRNO
      *       MOVE CK-NO    TO D01-CKNO
      *       PERFORM WRITE-DETAIL-LINE-ERROR
      *       GO TO EXIT-COMPARE-FIELDS.
      *
      *    STOP MESS.

           IF CK-TYPE NOT = OCK-TYPE
              MOVE "CK-TYPE" TO D01-TEXT
              MOVE BR-NO     TO D01-BRNO
              MOVE CK-NO     TO D01-CKNO
              MOVE OCK-TYPE  TO D01-A15
              MOVE CK-TYPE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF (OCK-DATE NOT NUMERIC) 
              MOVE 0  TO OCK-DATE.
           MOVE OCK-DATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
           PERFORM TEST-INVALID-DATE.
           IF CK-DATE NOT = DATE-YYYYMMDD
              MOVE "CK-DATE"      TO D01-TEXT
              MOVE BR-NO          TO D01-BRNO
              MOVE CK-NO          TO D01-CKNO
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE CK-DATE        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OCK-AMT NOT NUMERIC
              MOVE 0 TO OCK-AMT.
           IF CK-AMT NOT = OCK-AMT
              MOVE "CK-AMT" TO D01-TEXT
              MOVE BR-NO    TO D01-BRNO
              MOVE CK-NO    TO D01-CKNO
              MOVE OCK-AMT  TO D01-A15-NUM
              MOVE CK-AMT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

      ******************************************************************
      * S35 CK-PAYEE IS MESSED UP! I FIXED THE UP1534 PROGRAM. 
      * WAS DOING A STRING TO CK-PAYEE WITHOUT CLEARING IT FIRST.
      * THERE IS NO WAY WE CAN DUPLICATE THAT FIELD.
      *
      *   MOVE SPACES TO HOLD-PAYEE.
      *   IF OCK-ADR1-PARTIAL NOT = SPACES
      *      STRING OCK-PAYEE," ",OCK-ADR1-PARTIAL
      *             DELIMITED BY "  "   INTO HOLD-PAYEE
      *   ELSE
      *      MOVE OCK-PAYEE               TO HOLD-PAYEE.
      *   IF CK-PAYEE NOT = HOLD-PAYEE
      *      CALL "C$JUSTIFY" USING HOLD-PAYEE, "L"
      *      IF CK-PAYEE NOT = HOLD-PAYEE
      *         MOVE "CK-PAYEE" TO D01-TEXT
      *         MOVE BR-NO      TO D01-BRNO
      *         MOVE CK-NO    TO D01-CKNO
      *         MOVE HOLD-PAYEE  TO D01-A15
      *         MOVE CK-PAYEE   TO D01-S35
      *         PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OCK-REF NOT NUMERIC OR OCK-REF = 202020
              MOVE 0               TO OCK-REF.
           IF CK-REF NOT = OCK-REF
              MOVE "CK-REF" TO D01-TEXT
              MOVE BR-NO    TO D01-BRNO
              MOVE CK-NO    TO D01-CKNO
              MOVE OCK-REF  TO D01-A15
              MOVE CK-REF   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.


           IF CK-REASON NOT = OCK-REASON
      *       CALL "C$JUSTIFY" USING OCK-REASON, "L"
              IF CK-REASON NOT = OCK-REASON
              MOVE "CK-REASON" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE CK-NO       TO D01-CKNO
              MOVE OCK-REASON  TO D01-A15
              MOVE CK-REASON   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
              IF OCK-GLNO(SUB) NOT NUMERIC
                 MOVE 0             TO OCK-GLNO(SUB) 
              END-IF
              IF CK-GLNO(SUB) NOT = OCK-GLNO(SUB)
                 MOVE SPACES TO D01-TEXT
                 STRING "CK-GLNO", SUB INTO D01-TEXT
                 MOVE BR-NO          TO D01-BRNO
                 MOVE CK-NO          TO D01-CKNO
                 MOVE OCK-GLNO(SUB)  TO D01-A15-NUM
                 MOVE CK-GLNO(SUB)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OCK-GLAMT(SUB) NOT NUMERIC
                 MOVE 0             TO OCK-GLAMT(SUB) 
              END-IF
              IF CK-GLAMT(SUB) NOT = OCK-GLAMT(SUB)
                 STRING "CK-GLAMT", SUB INTO D01-TEXT
                 MOVE BR-NO           TO D01-BRNO
                 MOVE CK-NO           TO D01-CKNO
                 MOVE OCK-GLAMT(SUB)  TO D01-A15-NUM
                 MOVE CK-GLAMT(SUB)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.

           IF CK-VOID NOT = OCK-VOID
              MOVE "CK-VOID" TO D01-TEXT
              MOVE BR-NO     TO D01-BRNO
              MOVE CK-NO     TO D01-CKNO
              MOVE OCK-VOID  TO D01-A15
              MOVE CK-VOID   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CK-PAID NOT = OCK-PAID
              MOVE "CK-PAID" TO D01-TEXT
              MOVE BR-NO     TO D01-BRNO
              MOVE CK-NO     TO D01-CKNO
              MOVE OCK-PAID  TO D01-A15
              MOVE CK-PAID   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OCK-CLASS NOT NUMERIC
              MOVE 0 TO OCK-CLASS.
           IF CK-CLASS NOT = OCK-CLASS
              MOVE "CK-CLASS" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE CK-NO      TO D01-CKNO
              MOVE OCK-CLASS  TO D01-A15-NUM
              MOVE CK-CLASS   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OCK-DLNO NOT NUMERIC
              MOVE 0 TO OCK-DLNO.
           IF CK-DLNO NOT = OCK-DLNO
              MOVE "CK-DLNO" TO D01-TEXT
              MOVE BR-NO     TO D01-BRNO
              MOVE CK-NO     TO D01-CKNO
              MOVE OCK-DLNO  TO D01-A15-NUM
              MOVE CK-DLNO   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CK-PGM NOT = OCK-PGM
              MOVE "CK-PGM" TO D01-TEXT
              MOVE BR-NO    TO D01-BRNO
              MOVE CK-NO    TO D01-CKNO
              MOVE OCK-PGM  TO D01-A15
              MOVE CK-PGM   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF (OCK-VOID-DATE NOT NUMERIC) 
              MOVE 0  TO OCK-VOID-DATE.
           MOVE OCK-VOID-DATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF CK-VOID-DATE NOT = DATE-YYYYMMDD
              MOVE "CK-VOID-DATE" TO D01-TEXT
              MOVE BR-NO          TO D01-BRNO
              MOVE CK-NO          TO D01-CKNO
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE CK-VOID-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OCK-ACCTNO NOT NUMERIC
              MOVE 0 TO OLD-ACCTNO
           ELSE
              MOVE OCK-ACCTNO TO OLD-ACCTNO.
           IF CK-ACCTNO NOT = NEW-ACCTNO
              MOVE "CK-ACCTNO" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE CK-NO       TO D01-CKNO
              MOVE NEW-ACCTNO  TO D01-A15-NUM
              MOVE CK-ACCTNO   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OCK-DLRVNO NOT NUMERIC
              MOVE 0 TO OCK-DLRVNO.
           IF CK-DLRVNO NOT = OCK-DLRVNO
              MOVE "CK-DLRVNO" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE CK-NO       TO D01-CKNO
              MOVE OCK-DLRVNO  TO D01-A15
              MOVE CK-DLRVNO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CK-CLEARED-FG NOT = OCK-CLEARED-FG
              MOVE "CK-CLEARED-FG" TO D01-TEXT
              MOVE BR-NO           TO D01-BRNO
              MOVE CK-NO           TO D01-CKNO
              MOVE OCK-CLEARED-FG  TO D01-A15
              MOVE CK-CLEARED-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OCK-SSNO NOT NUMERIC
              MOVE 0 TO OCK-SSNO.
           IF CK-SSNO NOT = OCK-SSNO
              MOVE "CK-SSNO"  TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE CK-NO      TO D01-CKNO
              MOVE OCK-SSNO   TO D01-A15-NUM
              MOVE CK-SSNO    TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OCK-BATCHID NOT NUMERIC
              MOVE 0 TO OCK-BATCHID.
           IF CK-BATCHID NOT = OCK-BATCHID
              MOVE "CK-BATCHID" TO D01-TEXT
              MOVE BR-NO        TO D01-BRNO
              MOVE CK-NO        TO D01-CKNO
              MOVE OCK-BATCHID  TO D01-A15-NUM
              MOVE CK-BATCHID   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CK-UNPAID NOT = OCK-UNPAID
              MOVE "CK-UNPAID" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE CK-NO       TO D01-CKNO
              MOVE OCK-UNPAID  TO D01-A15
              MOVE CK-UNPAID   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF OCK-CLEARED-FG = "Y"
              IF OCK-CLEARED-DATE NOT NUMERIC
                 MOVE 0 TO OCK-CLEARED-DATE.
           IF OCK-CLEARED-FG = "Y"
              MOVE OCK-CLEARED-DATE TO DATE-MMDDYY
              PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
              PERFORM TEST-INVALID-DATE
              MOVE DATE-YYYYMMDD TO NUM-DATE
              PERFORM TSTDAT
              IF NUM-DATE = 0
      *            MOVE 0 TO DATE-YYYYMMDD
                  MOVE OCK-DATE TO DATE-MMDDYY
                  PERFORM CONVERT-MMDDYY-TO-YYYYMMDD
              END-IF
              IF CK-CLEARED-DATE NOT = DATE-YYYYMMDD
                 MOVE "CK-CLEARED-DATE" TO D01-TEXT
                 MOVE BR-NO             TO D01-BRNO
                 MOVE CK-NO             TO D01-CKNO
                 MOVE DATE-YYYYMMDD     TO D01-A15
                 MOVE CK-CLEARED-DATE   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM SET-HOLD-TILLNO.
           IF CK-TILLNO NOT = HOLD-TILLNO
              MOVE "CK-TILLNO" TO D01-TEXT
              MOVE BR-NO       TO D01-BRNO
              MOVE CK-NO       TO D01-CKNO
              MOVE HOLD-TILLNO TO D01-A15
              MOVE CK-TILLNO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CK-EVENT-CD NOT = OCK-EVENT-CD
              MOVE "CK-EVENT-CD" TO D01-TEXT
              MOVE BR-NO         TO D01-BRNO
              MOVE CK-NO         TO D01-CKNO
              MOVE OCK-EVENT-CD  TO D01-A15
              MOVE CK-EVENT-CD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OCK-PAYEE-NO NOT NUMERIC OR
                (OCK-PAYEE-NO = 202020 OR 020202)
              MOVE 0             TO OCK-PAYEE-NO.
           IF CK-PAYEE-NO NOT = OCK-PAYEE-NO
              MOVE "CK-PAYEE-NO" TO D01-TEXT
              MOVE BR-NO         TO D01-BRNO
              MOVE CK-NO         TO D01-CKNO
              MOVE OCK-PAYEE-NO  TO D01-A15-NUM
              MOVE CK-PAYEE-NO   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

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

           MOVE "BEGINNING CKNO                : "    TO R01-PROMPT.
           MOVE BEG-CKNO                            TO R01-NUMERIC.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    CKNO                : "    TO R01-PROMPT.
           MOVE END-CKNO                            TO R01-NUMERIC.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "A15 RECORDS PROCESSED         :"     TO R01-PROMPT.
           MOVE A15-RECORD-CTR                        TO R01-NUMERIC.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
 
           MOVE "S35 RECORDS PROCESSED         :"     TO R01-PROMPT.
           MOVE SQL-RECORD-CTR                        TO R01-NUMERIC.
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
           MOVE EXT-CONAME-CONAME TO CKFILE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CKFILE-SYSDATE.
           DISPLAY CKFILE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE CKFILE-FORM-FIELDS TO WR-BUF.
           MOVE CKFILE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE CKFILE-HELP--FILE TO HELP-LINK-FILE.
           MOVE CKFILE-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/CKFILE_EVA.CPY".
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

       SET-HOLD-TILLNO.
           MOVE 0 TO HOLD-TILLNO.
           IF OCK-TILLNO = "1"
              MOVE 1  TO HOLD-TILLNO 
           ELSE
           IF OCK-TILLNO = "2"
              MOVE 2  TO HOLD-TILLNO 
           ELSE
           IF OCK-TILLNO = "3"
              MOVE 3  TO HOLD-TILLNO 
           ELSE
           IF OCK-TILLNO = "4"
              MOVE 4  TO HOLD-TILLNO 
           ELSE
           IF OCK-TILLNO = "5"
              MOVE 5  TO HOLD-TILLNO 
           ELSE
           IF OCK-TILLNO = "6"
              MOVE 6  TO HOLD-TILLNO 
           ELSE
           IF OCK-TILLNO = "7"
              MOVE 7  TO HOLD-TILLNO 
           ELSE
           IF OCK-TILLNO = "8"
              MOVE 8  TO HOLD-TILLNO 
           ELSE
           IF OCK-TILLNO = "9"
              MOVE 9  TO HOLD-TILLNO 
           ELSE
           IF OCK-TILLNO = "A"
              MOVE 10 TO HOLD-TILLNO 
           ELSE
           IF OCK-TILLNO = "B"
              MOVE 11 TO HOLD-TILLNO 
           ELSE
           IF OCK-TILLNO = "C"
              MOVE 12 TO HOLD-TILLNO 
           ELSE
           IF OCK-TILLNO = "D"
              MOVE 13 TO HOLD-TILLNO 
           ELSE
           IF OCK-TILLNO = "E"
              MOVE 14 TO HOLD-TILLNO 
           ELSE
           IF OCK-TILLNO = "F"
              MOVE 15 TO HOLD-TILLNO 
           ELSE
           IF OCK-TILLNO = "G"
              MOVE 16 TO HOLD-TILLNO 
           ELSE
           IF OCK-TILLNO = "H"
              MOVE 17 TO HOLD-TILLNO 
           ELSE
           IF OCK-TILLNO = "I"
              MOVE 18 TO HOLD-TILLNO 
           ELSE
           IF OCK-TILLNO = "J"
              MOVE 19 TO HOLD-TILLNO 
           ELSE
           IF OCK-TILLNO = "K"
              MOVE 20 TO HOLD-TILLNO 
           ELSE
           IF OCK-TILLNO = "L"
              MOVE 21 TO HOLD-TILLNO 
           ELSE
           IF OCK-TILLNO = "M"
              MOVE 22 TO HOLD-TILLNO 
           ELSE
           IF OCK-TILLNO = "N"
              MOVE 23 TO HOLD-TILLNO 
           ELSE
           IF OCK-TILLNO = "O"
              MOVE 24 TO HOLD-TILLNO 
           ELSE
           IF OCK-TILLNO = "P"
              MOVE 25 TO HOLD-TILLNO 
           ELSE
           IF OCK-TILLNO = "Q"
              MOVE 26 TO HOLD-TILLNO 
           ELSE
           IF OCK-TILLNO = "R"
              MOVE 27 TO HOLD-TILLNO 
           ELSE
           IF OCK-TILLNO = "S"
              MOVE 28 TO HOLD-TILLNO 
           ELSE
           IF OCK-TILLNO = "T"
              MOVE 29 TO HOLD-TILLNO 
           ELSE
           IF OCK-TILLNO = "U"
              MOVE 30 TO HOLD-TILLNO 
           ELSE
           IF OCK-TILLNO = "V"
              MOVE 31 TO HOLD-TILLNO 
           ELSE
           IF OCK-TILLNO = "W"
              MOVE 32 TO HOLD-TILLNO 
           ELSE
           IF OCK-TILLNO = "X"
              MOVE 33 TO HOLD-TILLNO 
           ELSE
           IF OCK-TILLNO = "Y"
              MOVE 34 TO HOLD-TILLNO 
           ELSE
           IF OCK-TILLNO = "Z"
              MOVE 35 TO HOLD-TILLNO
           ELSE
              MOVE 0 TO HOLD-TILLNO.

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
       COPY "LIBLP/LPCKGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
      * COPY "LIBLP/LPCK1RN.CPY".
      * COPYMEMBER: LIBLP/LPCK1RN
       LOAD-CK1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CK-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CK-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO CK-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CK-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO CK-ALL-DATA
              MOVE CK-ALL-PATH             TO CK-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CK-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO CK-ALL-BASE
              MOVE FILPATH-MACHINE         TO CK-ALL-MACHINE
              MOVE FILPATH-DATA            TO CK-ALL-DATA
              MOVE CK-ALL-PATH             TO CK-PATH.

      *-----------------------------------------------------------------
       OPEN-CK1-FILE.
           PERFORM LOAD-CK1-FILE.
           PERFORM OPEN-IT.
           MOVE CK-PATH-SQL TO E-FILE.

           IF CK1-CONN-OPEN-SW = "N" 
              MOVE "Y"         TO CK1-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS CK1_CONNECT 
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-CK1-FILE.
           PERFORM START-IT.
           MOVE CK-PATH-OWNBR  TO CK-BRNO.
           MOVE CK-PATH-SQL    TO E-FILE.
           MOVE CK1-KEY        TO E-KEYX.

           IF ( CK1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-CK1-FILE.

      *    IF ( QCK1-WEND-BRNO = ZEROES    ) OR
      *       ( QCK1-WEND-BRNO NOT NUMERIC )
      *       MOVE CK-PATH-OWNBR        TO QCK1-WBEG-BRNO
      *       MOVE CK-PATH-OWNBR        TO QCK1-WEND-BRNO
      *       MOVE CK-NO                TO QCK1-WBEG-CKNO
      *       MOVE ALL "9"              TO QCK1-WEND-CKNO.

            EXEC SQL SET CONNECTION CK1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE CK1_CURSOR CURSOR FOR
                :CK1-RETURN-CODE = EXEC SSP_CKFILE_CKFILE_SELECT 
              ( :QCK1-WBEG-BRNO,:QCK1-WBEG-CKNO, :QCK1-WEND-CKNO ) 
           END-EXEC.

           EXEC SQL
               OPEN CK1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE STAT---GOOD TO CK1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-CK1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE CK-PATH-OWNBR  TO CK-BRNO.
           MOVE CK-PATH-SQL    TO E-FILE.
           MOVE CK1-KEY        TO E-KEYX.
           INITIALIZE QCK-REC.

           EXEC SQL SET CONNECTION CK1_CONNECT END-EXEC. 

           IF ( CK1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH CK1_CURSOR INTO :QCK-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CK1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-CK-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-CK1-FILE.

           PERFORM CLOSE-IT.
           IF ( CK1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION CK1_CONNECT END-EXEC 
              EXEC SQL
                   CLOSE CK1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC 
              MOVE STAT---BAD TO CK1-CURSOR-STAT.

      *----------------------------------------------------------------- 
       CLOSE-CK1-CONNECTION.

           IF CK1-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION CK1_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT CK1_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO CK1-CONN-OPEN-SW.

      *----------------------------------------------------------------- 
       OPEN-OCK1-FILE.
           PERFORM OPEN-IT.
           MOVE OCK-PATH TO E-FILE.
           OPEN INPUT OCK-FILE.
           IF IO-FG = 8
              GO TO OPEN-OCK1-FILE.
           IF IO-FG = 7
              CLOSE OCK-FILE
              GO TO OPEN-OCK1-FILE.
      *READ-OCK1-FILE.
      *    PERFORM READ-IT.
      *    MOVE OCK-PATH TO E-FILE.
      *    MOVE OCK1-KEY TO E-KEYX.
      *    READ OCK-FILE.
      *    IF IO-FG = 8
      *       GO TO READ-OCK1-FILE.
       READ-OCK1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OCK-PATH TO E-FILE.
           MOVE OCK1-KEY TO E-KEYX.
           READ OCK-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OCK1-FILE-NEXT.
       START-OCK1-FILE.
           PERFORM START-IT.
           MOVE OCK-PATH TO E-FILE.
           MOVE OCK1-KEY TO E-KEYX.
           START OCK-FILE KEY NOT < OCK1-KEY.
       CLOSE-OCK1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OCK-FILE.

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
              DESTROY CKFILE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
