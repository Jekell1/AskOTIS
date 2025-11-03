       IDENTIFICATION DIVISION.
       PROGRAM-ID.    BIFILE.
       DATE-WRITTEN.  12-01-2023.
      ******************************************************************
      *
      * DESCRIPTION   : COMPARE S35 BIFILE WITH A15 BIFILE
      *            
      *                 TO BE USED AFTER AN UPDATE
      *                 WILL COMPARE EACH FIELD AND REPORT ANY 
      *                 DIFFERENCES, AS LONG AS FIELD EXIST IN BOTH FILES
      *
      *  LOOPS THRU THE A15 FILE AND VERIFIES THE S35 FILE EXIST AND MATCHES
      *
      *  11-13-24  RMZ  ADDED CODE IN LOAD-WK-FILE TO BYASS 4 RECORDS
      *                 IN A15 THAT HAVE THE ACCOUNT NUMBER EQUALING 0'S.
      *  01-07-25  RMZ  ADDED 'BTRACK' - PROCESS TRACKING TO PROGRAM.
      *  05-01-25  MB   MISC REPORT CHANGES FOR MISSING RECS
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
      * COPYMEMBER: LIBLP/LPFSBI
           SELECT OBI-FILE ASSIGN TO OBI-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OBI1-KEY
                  ALTERNATE RECORD KEY OBI2-KEY WITH DUPLICATES
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
       FD  OBI-FILE
           LABEL RECORDS ARE STANDARD.
       COPY "LIBUP/A15SRC/LP01BI.CPY" REPLACING LEADING "BI" BY "OBI".

       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.

       01  WK-REC.
           03  WK-KEY.
               05  WK-OWNBR        PIC 9(4) COMP.
               05  WK-ACCTNO       PIC 9(6) COMP.
               05  WK-SSNO         PIC 9(9) COMP.
           03  WK-LNAME            PIC X(16).
           03  WK-FNAME            PIC X(16).
           03  WK-MAKERCD          PIC X.
           03  WK-ORGST            PIC XX.
           03  WK-GIFTNET          PIC X.
           03  WK-POFFDATE         PIC 9(6) COMP.
           03  WK-ENTDATE          PIC 9(6) COMP.
           03  WK-ENTDATEX        REDEFINES WK-ENTDATE
                                   PIC X(3).
           03  WK-SALEFIN          PIC X.
           03  WK-CONVERCD         PIC X.
           03  WK-PLCD             PIC X.
           03  WK-SMALLOAN         PIC X.
           03  WK-PLDATE           PIC 9(6) COMP.

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/BIFILE S35 11/12/24-09:42:08 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01BI.CPY".
       COPY "LIBLP/LP01BI_SQL.CPY".
       COPY "LIBLP/LPWSBI.CPY".

       COPY "LIBGB/GRSSPW.CPY".
       COPY "LIBGB/GRBRLSW.CPY".

       01  OBI-PATH                    PIC X(35) VALUE SPACES.
       01  OBI-ALL-PATH.
           03  OBI-ALL-BASE            PIC X(9).
           03  FILLER                  PIC X(4)  VALUE "/SH/".
           03  FILLER                  PIC X(6)  VALUE "BIFILE".

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
       01  OLD-ACCTNO                  PIC 9(8).
       01  FILLER REDEFINES OLD-ACCTNO.
           03  NEW-ACCTNO              PIC 9(6).
           03  FILLER                  PIC 99.

       01  PROCESS-STAT                PIC X(08) VALUE SPACES.
           88  PROCESS-STAT-GOOD                 VALUE "00".
           88  PROCESS-STAT-BAD                  VALUE "  " THRU "//"
                                                       "01" THRU "~~".

       01  MISC-CURSOR-STAT            PIC X(2) VALUE SPACES.
           88  MISC-CURSOR-STAT-GOOD            VALUE "00".
           88  MISC-CURSOR-STAT-BAD             VALUE "  " THRU "//"
                                                      "01" THRU "~~".

       01  BI1-CONN-OPEN-SW            PIC X     VALUE "N".
       01  A15-RECORD-CTR              PIC 9(09) VALUE 0.
           88  NO-RECORDS-FOUND                  VALUE ZEROES.
       01  S35-RECORD-CTR              PIC 9(09) VALUE 0.
       01  WK-RECORD-CTR               PIC 9(9)  VALUE 0.

       01  STARTING-TIME               PIC X(08) VALUE SPACES.
       01  SUB                         PIC 9(04) VALUE ZEROES.
       01  S1                          PIC 9(04) VALUE ZEROES.
       01  S2                          PIC 9(04) VALUE ZEROES.
       01  S3                          PIC 9(04) VALUE ZEROES.

       01  SYSDATE-YYYYMMDD            PIC 9(8)  VALUE ZEROES.

       01  WK-LOADED-SW                PIC X      VALUE "N".
       01  BI-EOF-SW                   PIC X      VALUE "N".
       01  WK-EOF-SW                   PIC X      VALUE "N".
       01  OBI-EOF-SW                  PIC X      VALUE "N".
       01  TOTAL-ERRORS-CTR            PIC 9(9)   VALUE 0.
       01  PROC-BRANCHES-ARRAY         OCCURS 9999 TIMES
                                       PIC X.

       01  WS-BI1-KEY.
           05  WS-BI-OWNBR             PIC 9(4).
           05  WS-BI-ACCTNO            PIC 9(6).
           05  WS-BI-SSNO              PIC 9(9).

       01  WS-OBI1-KEY.
           05  WS-OBI-OWNBR            PIC 9(4).
           05  WS-OBI-ACCTNO           PIC 9(6).
           05  WS-OBI-SSNO             PIC 9(9).

       01  LEGEND                      PIC X(79) VALUE SPACES.
       01  LEGEND-LIST                 PIC X(07) VALUE "LEGEND.".


      *-----------------------------------------------------------------
       01  LEGEND-ACCEPT-YN.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT             ".
           03  FILLER PIC X(30) VALUE "                              ".
           03  FILLER PIC X(19) VALUE "     ACCEPT (Y/N): ".

       01  SOURCE-PATH-LIST            PIC X(07)     VALUE "S-PATH.".

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  SOURCE-PATH             PIC X(9).
           03  BEG-BRANCH              PIC 9(04).
           03  FILLER REDEFINES BEG-BRANCH.
               05 BEG-BRANCH-A         PIC X(04).
           03  END-BRANCH              PIC 9(04).
           03  FILLER REDEFINES END-BRANCH.
               05 END-BRANCH-A         PIC X(04).
           03  BEG-SSNO                PIC 9(9).
           03  END-SSNO                PIC 9(9).
           03  GROUP-FLAG              PIC X(01).
           03  ENT-GROUP               PIC X(04).

       01  VERSION-CONTROL             PIC X(01)     VALUE "A".

      *-----------------------------------------------------------------
       01  SV-GROUP-FLAG               PIC X(01)     VALUE SPACES.
       01  SV-ENT-GROUP                PIC X(04)     VALUE SPACES.

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
           "BORROWER INDEX (BIFILE) COMPARE A15 TO S35".

       01  HEADER-LINE-03.
           03  FILLER                  PIC X(4)  VALUE "BRNO".
           03  FILLER                  PIC X(1)  VALUE SPACES.
           03  FILLER                  PIC X(6)  VALUE "ACCTNO".
           03  FILLER                  PIC X(1)  VALUE SPACES.
           03  FILLER                  PIC X(9)  VALUE "  SSNO   ".
           03  FILLER                  PIC X(1)  VALUE SPACES.
           03  FILLER                  PIC X(28) VALUE "DESCRIPTION".
           03  FILLER                  PIC X(22) VALUE "A15".
           03  FILLER                  PIC X(3)  VALUE "S35".

       01  DETAIL-LINE                 PIC X(132) VALUE SPACES.

       01  DETAIL-LINE-01              REDEFINES DETAIL-LINE.
           03  D01-BRNO                PIC Z(3)9.
           03  FILLER                  PIC X.
           03  D01-ACCTNO              PIC 9(6).
           03  FILLER                  PIC X(1).
           03  D01-SSNO                PIC 9(9).
           03  FILLER                  PIC X.
           03  D01-TEXT                PIC X(25).
           03  FILLER                  PIC X(3).
           03  D01-A15                 PIC X(20).
           03  FILLER REDEFINES D01-A15.
               05  D01-A15-NUM         PIC Z(7)9.9(3).
               05  FILLER              PIC X(8).
           03  FILLER                  PIC X(2).
           03  D01-S35                 PIC X(20).
           03  FILLER REDEFINES D01-S35.
               05  D01-S35-NUM         PIC Z(7)9.9(3).
               05  FILLER              PIC X(8).

       01  RANGE-LINE-01               REDEFINES DETAIL-LINE.
           03  R01-PROMPT              PIC X(32).
           03  R01-FIELD               PIC X(48).
           03  R01-AMT                 REDEFINES R01-FIELD
                                       PIC ZZZ,ZZZ,ZZ9.99-.
           03  R01-NUMERIC             REDEFINES R01-FIELD
                                       PIC Z(10)9-.
           03  R01-TEXT                REDEFINES R01-FIELD
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
       COPY "LIBUT/BIFILE_DEF.CPY".
       COPY "LIBUT/BIFILE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/BIFILE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE OBI-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-BI1-FILE.
           PERFORM CLOSE-BI1-CONNECTION.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE OBI-FILE WK-FILE. 
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
           MOVE "BIFILE" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE BIFILE-QUEUE-POS TO Q-POS.
           MOVE BIFILE-COPIES-POS TO C-POS.
           MOVE BIFILE-PRU-POS   TO P-POS.
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

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-PR-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WK-PATH.

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
              MOVE "Y" TO PROC-BRANCHES-ARRAY(S1) 
           END-PERFORM.

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
              GO TO ENTER-END-SSNO.

       ENTER-SOURCE-PATH.

           IF SOURCE-PATH = SPACES
              MOVE "A15_PATH" TO GETENV-BUF
              PERFORM GETENV-CALL
              IF ( STAT-GOOD )
                 MOVE GETENV-BUF TO SOURCE-PATH
              ELSE
                 MOVE "/US2/DA15" TO SOURCE-PATH
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

      *     MOVE LEGEND-BEG-BRANCH TO LEGEND.
      *     PERFORM SEND-LEGEND.

           MOVE "E RA000BEG-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F3-KEY ) GO TO ENTER-ALL-BRANCH.
           IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-BRANCH.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF ( WGR-WORD-ZERO )
              MOVE ZEROES     TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.

           MOVE WGR-ZERO-TEST TO BEG-BRANCH-A.
           MOVE SPACES        TO END-BRANCH-A.
           MOVE ZEROES        TO WGR-MIN-LEVEL.
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

      *     MOVE LEGEND-END-BRANCH TO LEGEND.
      *     PERFORM SEND-LEGEND.

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
       ENTER-BEG-SSNO.

           MOVE "ENNN090BEG-SSNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-SOURCE-PATH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F3-KEY ) GO TO ENTER-ALL-SSNO.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-SSNO.

           MOVE VDUNUM0 TO BEG-SSNO.

      *-----------------------------------------------------------------
       ENTER-END-SSNO.

           MOVE "ENNN090END-SSNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-SSNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-SSNO.

           MOVE VDUNUM0 TO END-SSNO.

           IF ( BEG-SSNO > END-SSNO )
              MOVE "INVALID SSNO RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-SSNO.

           GO TO ENTER-PARAMETERS-END.

      *-----------------------------------------------------------------
       ENTER-ALL-SSNO.

           MOVE "SNNN090BEG-SSNO."  TO VDU.
           MOVE ZEROES              TO BEG-SSNO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN090END-SSNO."  TO VDU.
           MOVE ALL "9"             TO END-SSNO VDUNUM0.
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

           MOVE "S  A090S-PATH."           TO VDU.
           MOVE SOURCE-PATH                TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SNNN090BEG-SSNO."       TO VDU.
           MOVE BEG-SSNO                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN090END-SSNO."       TO VDU.
           MOVE END-SSNO                 TO VDUNUM0.
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

      * ONLY CONTINUE IF PROCESS-BRANCH-DRIVE FOUND GLOBAL FILE *
           IF WK-LOADED-SW = "Y"
              GO TO EXECUTION-NEXT-GROUP.

       EXECUTION-MODULE-EXIT.
           EXIT.

      ********************************************************
       PROCESS-BRANCH-DRIVER SECTION.
      ********************************************************

           MOVE 9999 TO PROGID-SLEEP-COUNTER.
           PERFORM SEND-PROGRESS-INDICATOR.

           MOVE 'N' TO WK-EOF-SW BI-EOF-SW. 

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           PERFORM OPEN-BI1-FILE.

           IF WK-LOADED-SW = "N"
              MOVE SOURCE-PATH  TO OBI-ALL-BASE
              MOVE OBI-ALL-PATH TO OBI-PATH ACCESS-BUF
              PERFORM ACCESS-CALL
              IF ( STAT-BAD )
                 MOVE "MISSING A15 FILE" TO D01-TEXT
                 MOVE BI-OWNBR           TO D01-BRNO
                 PERFORM WRITE-DETAIL-LINE-ERROR
                 GO TO PROCESS-BRANCH-DRIVER-END
              END-IF
              PERFORM OPEN-WK-FILE-OUTPUT
              PERFORM CLOSE-WK-FILE
              PERFORM OPEN-WK-FILE
              PERFORM OPEN-OBI1-FILE
              PERFORM SEND-PROGRESS-INDICATOR 
              PERFORM LOAD-WK-FILE
              PERFORM CLOSE-OBI1-FILE
              PERFORM CLOSE-WK-FILE
              PERFORM OPEN-WK-FILE
              MOVE "Y" TO WK-LOADED-SW.

           MOVE "PROCESSING S35 DATA" TO BTRACK-MESS.
           PERFORM BTRACK-GLOBAL.

           MOVE BR-NO      TO QBI1-WBEG-OWNBR
                              QBI1-WEND-OWNBR.
           MOVE BEG-SSNO   TO QBI1-WBEG-SSNO.
           MOVE END-SSNO   TO QBI1-WEND-SSNO.
           PERFORM START-BI1-FILE.

           MOVE "PROCESSING A15 DATA" TO BTRACK-MESS.  
           PERFORM BTRACK-GLOBAL.

           MOVE BR-NO     TO WK-OWNBR.
           MOVE BEG-SSNO  TO WK-SSNO.
           MOVE ZEROS     TO WK-ACCTNO.
           PERFORM START-WK-FILE.

           PERFORM PROCESS-READ-WK.
           PERFORM PROCESS-READ-BI1. 

       PROCESS-BRANCH-NEXT-BI.

           PERFORM SEND-PROGRESS-INDICATOR.

           IF WK-EOF-SW  = "Y" AND OBI-EOF-SW = "Y"
              GO TO PROCESS-BRANCH-DRIVER-EXIT.

           IF WS-OBI1-KEY < WS-BI1-KEY 
              MOVE "MISSING S35 RECORD" TO D01-TEXT
              MOVE WK-OWNBR             TO D01-BRNO  
              MOVE WK-ACCTNO            TO D01-ACCTNO
              MOVE WK-SSNO              TO D01-SSNO 
              PERFORM WRITE-DETAIL-LINE-ERROR 
              PERFORM PROCESS-READ-WK 
              GO TO PROCESS-BRANCH-NEXT-BI.
 
           IF WS-BI1-KEY < WS-OBI1-KEY
              MOVE "MISSING A15 RECORD" TO D01-TEXT
              MOVE BI-OWNBR             TO D01-BRNO  
              MOVE BI-ACCTNO            TO D01-ACCTNO 
              MOVE BI-SSNO              TO D01-SSNO 
              PERFORM WRITE-DETAIL-LINE-ERROR
              PERFORM PROCESS-READ-BI1
              GO TO PROCESS-BRANCH-NEXT-BI.

           PERFORM SEND-PROGRESS-INDICATOR.

           PERFORM COMPARE-FIELDS.

           PERFORM PROCESS-READ-WK.
           PERFORM PROCESS-READ-BI1.

           GO TO PROCESS-BRANCH-NEXT-BI.

       PROCESS-BRANCH-DRIVER-END.

           PERFORM CLOSE-BI1-FILE.

       PROCESS-BRANCH-DRIVER-EXIT.
           EXIT.

      ******************************************************************
       LOAD-WK-FILE SECTION.
      ******************************************************************

       LOAD-WK-FILE-CONTINUE.

           PERFORM READ-OBI1-FILE-NEXT.
           IF IO-BAD OR OBI-SSNO > END-SSNO
              MOVE "Y" TO OBI-EOF-SW
              GO TO EXIT-LOAD-WK-FILE. 

           ADD 1 TO A15-RECORD-CTR.

           PERFORM SEND-PROGRESS-INDICATOR.
 
           IF OBI-SSNO < BEG-SSNO 
              GO TO LOAD-WK-FILE-CONTINUE.

           IF PROC-BRANCHES-ARRAY(OBI-OWNBR) NOT = "Y"
              GO TO LOAD-WK-FILE-CONTINUE.

           IF (OBI-OWNBR = 23  AND OBI-ACCTNO = 00000001) OR
              (OBI-OWNBR = 423 AND OBI-ACCTNO = 00000001) OR
              (OBI-OWNBR = 493 AND OBI-ACCTNO = 00000001) OR
              (OBI-OWNBR = 495 AND OBI-ACCTNO = 00000001)   
              GO TO LOAD-WK-FILE-CONTINUE.
 
           MOVE OBI-OWNBR       TO WK-OWNBR.
           MOVE OBI-ACCTNO      TO OLD-ACCTNO.
           MOVE NEW-ACCTNO      TO WK-ACCTNO.
           MOVE OBI-SSNO        TO WK-SSNO.
           MOVE OBI-LNAME       TO WK-LNAME.
           MOVE OBI-FNAME       TO WK-FNAME.
           MOVE OBI-MAKERCD     TO WK-MAKERCD.
           MOVE OBI-ORGST       TO WK-ORGST.
           MOVE OBI-GIFTNET     TO WK-GIFTNET.
           MOVE OBI-POFFDATE    TO WK-POFFDATE.
           MOVE OBI-ENTDATE     TO WK-ENTDATE.
           MOVE OBI-SALEFIN     TO WK-SALEFIN.
           MOVE OBI-CONVERCD    TO WK-CONVERCD.
           MOVE OBI-PLCD        TO WK-PLCD.
           MOVE OBI-SMALLOAN    TO WK-SMALLOAN.
           MOVE OBI-PLDATE      TO WK-PLDATE.
 
           PERFORM WRITE-WK-FILE. 
  
           GO TO LOAD-WK-FILE-CONTINUE.

       EXIT-LOAD-WK-FILE.
           EXIT.

      ******************************************************************
       PROCESS-READ-WK SECTION.
      ******************************************************************

           PERFORM READ-WK-FILE-NEXT.
           IF IO-BAD OR WK-OWNBR NOT = BR-NO 
              MOVE ALL '9'        TO WS-OBI1-KEY
              MOVE "Y"            TO WK-EOF-SW
           ELSE 
              MOVE WK-OWNBR       TO WS-OBI-OWNBR
              MOVE WK-ACCTNO      TO WS-OBI-ACCTNO
              MOVE WK-SSNO        TO WS-OBI-SSNO 
              ADD  01             TO WK-RECORD-CTR.

       EXIT-PROCESS-READ-WK.
           EXIT.

      ******************************************************************
       PROCESS-READ-BI1 SECTION.
      ******************************************************************

           PERFORM READ-BI1-FILE-NEXT.
           IF IO-BAD 
              MOVE 9999           TO WS-BI-OWNBR
              MOVE 999999         TO WS-BI-ACCTNO
              MOVE 999999999      TO WS-BI-SSNO  
              MOVE "Y"            TO BI-EOF-SW
           ELSE 
              MOVE BR-NO          TO WS-BI-OWNBR
              MOVE BI-ACCTNO      TO WS-BI-ACCTNO
              MOVE BI-SSNO        TO WS-BI-SSNO
              ADD 1               TO S35-RECORD-CTR.

       EXIT-PROCESS-READ-BI1.
           EXIT.

      ******************************************************************
       COMPARE-FIELDS SECTION.
      ******************************************************************

           IF BI-LNAME NOT = WK-LNAME
               CALL "C$JUSTIFY" USING WK-LNAME, "L"
              IF BI-LNAME NOT = WK-LNAME
                 MOVE "BI-LNAME"  TO D01-TEXT
                 MOVE BI-OWNBR    TO D01-BRNO
                 MOVE BI-SSNO     TO D01-SSNO
                 MOVE BI-ACCTNO   TO D01-ACCTNO
                 MOVE WK-LNAME    TO D01-A15
                 MOVE BI-LNAME    TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.
       
           IF BI-FNAME NOT = WK-FNAME
               CALL "C$JUSTIFY" USING WK-FNAME, "L"
              IF BI-FNAME NOT = WK-FNAME
                 MOVE "BI-FNAME"  TO D01-TEXT
                 MOVE BI-OWNBR    TO D01-BRNO
                 MOVE BI-SSNO     TO D01-SSNO
                 MOVE BI-ACCTNO   TO D01-ACCTNO
                 MOVE WK-FNAME    TO D01-A15
                 MOVE BI-FNAME    TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BI-MAKERCD NOT = WK-MAKERCD
              MOVE "BI-MAKERCD"   TO D01-TEXT
              MOVE BI-OWNBR       TO D01-BRNO
              MOVE BI-SSNO        TO D01-SSNO
              MOVE BI-ACCTNO      TO D01-ACCTNO
              MOVE WK-MAKERCD     TO D01-A15
              MOVE BI-MAKERCD     TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BI-ORGST NOT = WK-ORGST
              MOVE "BI-ORGST"     TO D01-TEXT
              MOVE BI-OWNBR       TO D01-BRNO
              MOVE BI-SSNO        TO D01-SSNO
              MOVE BI-ACCTNO      TO D01-ACCTNO
              MOVE WK-ORGST       TO D01-A15
              MOVE BI-ORGST       TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BI-GIFTNET NOT = WK-GIFTNET
              MOVE "BI-GIFTNET"   TO D01-TEXT
              MOVE BI-OWNBR       TO D01-BRNO
              MOVE BI-SSNO        TO D01-SSNO
              MOVE BI-ACCTNO      TO D01-ACCTNO
              MOVE WK-GIFTNET     TO D01-A15
              MOVE BI-GIFTNET     TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE WK-POFFDATE       TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.

           IF DATE-YYYYMMDD NOT = BI-POFFDATE
              MOVE "BI-POFFDATE"  TO D01-TEXT
              MOVE BI-OWNBR       TO D01-BRNO
              MOVE BI-SSNO        TO D01-SSNO
              MOVE BI-ACCTNO      TO D01-ACCTNO
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE BI-POFFDATE    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE WK-ENTDATE        TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.

           IF DATE-YYYYMMDD NOT = BI-ENTDATE
              MOVE "BI-ENTDATE"   TO D01-TEXT
              MOVE BI-OWNBR       TO D01-BRNO
              MOVE BI-SSNO        TO D01-SSNO
              MOVE BI-ACCTNO      TO D01-ACCTNO
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE BI-ENTDATE     TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BI-SALEFIN NOT = WK-SALEFIN
              MOVE "BI-SALEFIN"   TO D01-TEXT
              MOVE BI-OWNBR       TO D01-BRNO
              MOVE BI-SSNO        TO D01-SSNO
              MOVE BI-ACCTNO      TO D01-ACCTNO
              MOVE WK-SALEFIN     TO D01-A15
              MOVE BI-SALEFIN     TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BI-CONVERCD NOT = WK-CONVERCD
              MOVE "BI-CONVERCD"  TO D01-TEXT
              MOVE BI-OWNBR       TO D01-BRNO
              MOVE BI-SSNO        TO D01-SSNO
              MOVE BI-ACCTNO      TO D01-ACCTNO
              MOVE WK-CONVERCD    TO D01-A15
              MOVE BI-CONVERCD    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF BI-PLCD NOT = WK-PLCD
              MOVE "BI-PLCD"      TO D01-TEXT
              MOVE BI-OWNBR       TO D01-BRNO
              MOVE BI-SSNO        TO D01-SSNO
              MOVE BI-ACCTNO      TO D01-ACCTNO
              MOVE WK-PLCD        TO D01-A15
              MOVE BI-PLCD        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
       
           IF BI-SMALLOAN NOT = WK-SMALLOAN
              MOVE "BI-SMALLOAN"  TO D01-TEXT
              MOVE BI-OWNBR       TO D01-BRNO
              MOVE BI-SSNO        TO D01-SSNO
              MOVE BI-ACCTNO      TO D01-ACCTNO
              MOVE WK-SMALLOAN    TO D01-A15
              MOVE BI-SMALLOAN    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE WK-PLDATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.

           IF DATE-YYYYMMDD NOT = BI-PLDATE
              MOVE "BI-PLDATE"    TO D01-TEXT
              MOVE BI-OWNBR       TO D01-BRNO
              MOVE BI-SSNO        TO D01-SSNO
              MOVE BI-ACCTNO      TO D01-ACCTNO
              MOVE DATE-YYYYMMDD  TO D01-A15
              MOVE BI-PLDATE      TO D01-S35
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

           MOVE "BEGINNING SSNO               : "     TO R01-PROMPT.
           MOVE BEG-SSNO                              TO R01-NUMERIC.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    SSNO               : "     TO R01-PROMPT.
           MOVE END-SSNO                              TO R01-NUMERIC.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "A15 RECORDS PROCESSED         :"     TO R01-PROMPT.
           MOVE A15-RECORD-CTR                        TO R01-NUMERIC.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "WK RECORDS PROCESSED          :"     TO R01-PROMPT.
           MOVE WK-RECORD-CTR                         TO R01-NUMERIC.
           MOVE 01                                    TO LN-FEED.
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
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/PROGID.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

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

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO BIFILE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BIFILE-SYSDATE.
           DISPLAY BIFILE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE BIFILE-FORM-FIELDS TO WR-BUF.
           MOVE BIFILE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE BIFILE-HELP--FILE TO HELP-LINK-FILE.
           MOVE BIFILE-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/BIFILE_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
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

           MOVE BR-NO TO PROGID-BRANCH.

           MOVE SPACES TO PROGID-TEXT.
           IF WK-LOADED-SW = "N"
              STRING "LOADING WORK FILE: ", A15-RECORD-CTR 
                INTO PROGID-TEXT
           ELSE
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
       COPY "LIBLP/LPBIGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

       COPY "LIBGB/GRBRLS_SQL.CPY".

      * COPY "LIBLP/LPBI1RN.CPY".
      * COPYMEMBER: LIBLP/LPBI1RN
       LOAD-BI1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BI-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BI-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BI-ALL-BASE
              MOVE BI-ALL-PATH             TO BI-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BI-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BI-ALL-BASE
              MOVE BI-ALL-PATH             TO BI-PATH.

      *-----------------------------------------------------------------
       OPEN-BI1-FILE.
           PERFORM LOAD-BI1-FILE.
           PERFORM OPEN-IT.
           MOVE BI-PATH-SQL TO E-FILE.

           IF BI1-CONN-OPEN-SW = "N" 
              MOVE "Y"         TO BI1-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS BI1_CONNECT 
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-BI1-FILE.
           PERFORM START-IT.
           MOVE BI-PATH-SQL TO E-FILE.
           MOVE BI1-KEY TO E-KEYX.

           IF ( BI1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BI1-FILE.

           IF ( QBI1-WEND-OWNBR = ZEROES    ) OR
              ( QBI1-WEND-OWNBR NOT NUMERIC )
              MOVE BI-SSNO        TO QBI1-WBEG-SSNO
              MOVE ALL "9"        TO QBI1-WEND-SSNO
              MOVE BI-ACCTNO      TO QBI1-WBEG-ACCTNO
              MOVE ALL "9"        TO QBI1-WEND-ACCTNO
              MOVE BI-OWNBR       TO QBI1-WBEG-OWNBR
              MOVE ALL "9"        TO QBI1-WEND-OWNBR.

           EXEC SQL SET CONNECTION BI1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE BI1_CURSOR CURSOR FOR
                :BI1-RETURN-CODE = EXEC SSP_BIFILE_BIFILE_SELECT 
              ( :QBI1-WBEG-OWNBR,     
                :QBI1-WBEG-SSNO,   
                :QBI1-WEND-SSNO )      
           END-EXEC.

           EXEC SQL
               OPEN BI1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QBI1-WBEG-SSNO
                          QBI1-WEND-SSNO
                          QBI1-WBEG-ACCTNO
                          QBI1-WEND-ACCTNO
                          QBI1-WBEG-OWNBR
                          QBI1-WEND-OWNBR.

           MOVE STAT---GOOD TO BI1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-BI1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BI-PATH-SQL TO E-FILE.
           MOVE BI1-KEY TO E-KEYX.
           INITIALIZE QBI-REC.

           EXEC SQL SET CONNECTION BI1_CONNECT END-EXEC.

           IF ( BI1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BI1_CURSOR INTO :QBI-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BI1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BI-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-BI1-FILE.
           PERFORM CLOSE-IT.
           IF ( BI1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION BI1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE BI1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO BI1-CURSOR-STAT.

      *----------------------------------------------------------------- 
       CLOSE-BI1-CONNECTION.

           IF BI1-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION BI1_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT BI1_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO BI1-CONN-OPEN-SW.

      *----------------------------------------------------------------- 
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


       OPEN-OBI1-FILE.
           PERFORM OPEN-IT.
           MOVE OBI-PATH TO E-FILE.
           OPEN INPUT OBI-FILE.
           IF IO-FG = 8
              GO TO OPEN-OBI1-FILE.
           IF IO-FG = 7
              CLOSE OBI-FILE
              GO TO OPEN-OBI1-FILE.
      *READ-OBI1-FILE.
      *    PERFORM READ-IT.
      *    MOVE OBI-PATH TO E-FILE.
      *    MOVE OBI1-KEY TO E-KEYX.
      *    READ OBI-FILE.
      *    IF IO-FG = 8
      *       GO TO READ-OBI1-FILE.
       START-OBI1-FILE.
           PERFORM START-IT.
           MOVE OBI-PATH TO E-FILE.
           MOVE OBI1-KEY TO E-KEYX.
           START OBI-FILE KEY NOT < OBI1-KEY.
       READ-OBI1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OBI-PATH TO E-FILE.
           MOVE OBI1-KEY TO E-KEYX.
           READ OBI-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OBI1-FILE-NEXT.
       CLOSE-OBI1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OBI-FILE.

      **************       

       OPEN-WK-FILE.
           PERFORM OPEN-IT.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           OPEN I-O WK-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-FILE.
           IF IO-FG = 7
              CLOSE WK-FILE
              GO TO OPEN-WK-FILE.
       OPEN-WK-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           OPEN OUTPUT WK-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK-FILE
              GO TO OPEN-WK-FILE-OUTPUT.
       START-WK-FILE.
           PERFORM START-IT.
           MOVE WK-PATH TO E-FILE.
           MOVE WK-KEY  TO E-KEYX.
           START WK-FILE KEY NOT < WK-KEY.
       READ-WK-FILE-NEXT.
           PERFORM READ-IT.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           MOVE WK-KEY  TO E-KEYX.
           READ WK-FILE NEXT
              AT END  MOVE 9 TO IO-STATUS IO-FG
           END-READ.
           IF IO-FG = 8
              GO TO READ-WK-FILE-NEXT.
       WRITE-WK-FILE.
           PERFORM WRITE-IT.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           MOVE WK-KEY  TO E-KEYX.
           WRITE WK-REC 
             INVALID KEY 
                MOVE 2 TO IO-STATUS IO-FG
           END-WRITE.
           IF IO-FG = 8
             GO TO WRITE-WK-FILE.
           UNLOCK WK-FILE.
       CLOSE-WK-FILE.
           PERFORM CLOSE-IT.
           MOVE 0 TO IO-FG IO-STATUS.
           CLOSE WK-FILE.

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

           MOVE WK-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY BIFILE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
