       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CDBFLE.
       DATE-WRITTEN.  02-13-2024.
       AUTHOR. MARK BOOMGAARD.
      ******************************************************************
      *
      * DESCRIPTION   : COMPARE S35 CDBFILE WITH A15 CDFILE (FOR CDB RECS)
      *            
      *                 TO BE USED AFTER AN UPDATE
      *                 WILL COMPARE EACH FIELD AND REPORT ANY 
      *                 DIFFERENCES, AS LONG AS FIELD EXIST IN BOTH FILES
      *
      *  2 FILE MATCH PROCESSING.
      *
      *  01-07-25  RMZ  ADDED 'BTRACK' - PROCESS TRACKING TO PROGRAM.
      *  03-05-25  MB   ADDED INPUT BEG/ENDING BRANCH.
      *  04-16-25  MB   USE 0 FOR OCDB-CN-PHONE IF = 0202020000
      *                 USE 0 FOR OCD-COMM-PERCENT IF = 20.200
      *                 USE 0 FOR OCD-COMM-GLFEENO IF = 02020202020
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

      * COPYMEMBER: LIBLP/LPFSCD
           SELECT OCD-FILE ASSIGN TO OCD-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OCD1-KEY
                  FILE STATUS FILE-STAT.

      * COPYMEMBER: LIBGB/GBFSBR
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
                  RECORD KEY WK-KEY
                  FILE STATUS FILE-STAT.

           SELECT WK2-FILE ASSIGN TO WK2-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK2-KEY
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  OCD-FILE
           LABEL RECORDS ARE STANDARD.
      *-----------------------------------------------------------------
       COPY "LIBUP/A15SRC/LP01CD.CPY" REPLACING LEADING "CD" BY "OCD".
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
       01  WK-REC.
           03  WK-KEY.
              05  WK-KEY-TYPE         PIC XX.
              05  WK-KEY-BRNO         PIC 9(4).
              05  WK-KEY-CODE         PIC X(8).
           03  WK-DATA                PIC X(OCD-SIZE).

       FD  WK2-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK2-REC.
           03  WK2-KEY.
              05  WK2-KEY-TYPE         PIC XX.
              05  WK2-KEY-BRNO         PIC 9(4).
              05  WK2-KEY-CODE         PIC X(8).
           03  WK2-DATA                PIC X(200).

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)UT/CDBFLE.CBL S35 01/02/25-12:13:31 >".

       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LP01CDB_SQL.CPY".
       COPY "LIBLP/LPWSCDB.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".

       COPY "LIBGB/GRSSPW.CPY".
       COPY "LIBGB/GRBRLSW.CPY".
 
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
 
      ***************************
      *  FILE PATH DEFINITIONS  *
      ***************************
       01  OBR-IFN       PIC XX VALUE "BR".
       01  OBR-KEY       PIC 9(4)   VALUE 0.
       01  OBR-PATH. 
           03  OBR-ALL-BASE      PIC X(9).
           03  FILLER           PIC X     VALUE "/".
           03  OBR-ALL-MACHINE   PIC XX.
           03  FILLER           PIC X(10) VALUE
               "/GB/BRFILE".
 
       01  OCD-PATH             PIC X(35) VALUE SPACES.
       01  OCD-ALL-PATH.
           03  OCD-ALL-BASE     PIC X(9).
           03  FILLER           PIC X     VALUE "/".
           03  OCD-ALL-MACHINE  PIC XX.
           03  FILLER           PIC X(10) VALUE
               "/LP/CDFILE".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  CDB-EOF-SW                  PIC X      VALUE "N".
       01  OCD-EOF-SW                  PIC X      VALUE "N".
       01  A15-FILE-FOUND-SW           PIC X      VALUE "N".
       01  TOTAL-ERRORS-CTR            PIC 9(9)   VALUE 0.
       01  PRINTING-RANGES-SW          PIC X      VALUE "N".
       01  SUB1                        PIC 9      VALUE 0.
       01  SUB2                        PIC 99     VALUE 0.
       01  S1                          PIC 9999   VALUE 0.
       01  WK-EOF-SW                   PIC X      VALUE "N".
       01  WK-RECS-CTR                 PIC 9(9)   VALUE 0.
       01  WK2-EOF-SW                  PIC X      VALUE "N".
       01  WS-OCDB-CODE                PIC X(8).
       01  TEST-LOWER-Z1               PIC X(8)   VALUE "Z1      ".
       01  TEST-LOWER-X5               PIC X(8)   VALUE "X5      ".

       01  R12-ARRAY                   OCCURS 9999 TIMES  
                                       PIC XX.

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

       01  LEGEND-BEG-TYPE.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F7-EXIT, EN".
           03  FILLER PIC X(30) VALUE "TER BEGINNING TYPE           ".
           03  FILLER PIC X(19) VALUE "                   ".

       01  LEGEND-END-TYPE.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER ENDIN".
           03  FILLER PIC X(30) VALUE "G TYPE                        ".
           03  FILLER PIC X(19) VALUE "                   ".

      *-----------------------------------------------------------------
       01  LEGEND-ACCEPT-YN.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT             ".
           03  FILLER PIC X(30) VALUE "                              ".
           03  FILLER PIC X(19) VALUE "     ACCEPT (Y/N): ".

       01  SOURCE-PATH-LIST            PIC X(07)     VALUE "S-PATH.".

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  SOURCE-PATH             PIC X(9).
           03  BEG-TYPE                PIC XX.
           03  END-TYPE                PIC XX.
           03  BEG-BRANCH              PIC 9(04).
           03  END-BRANCH              PIC 9(04).
           03  GROUP-FLAG              PIC X(01).
           03  ENT-GROUP               PIC X(04).


       01  VERSION-CONTROL             PIC X(01)     VALUE "B".

       01  PROC-BRANCH-ARRAY           OCCURS 9999 TIMES
                                       PIC X.

      *-----------------------------------------------------------------
       01  LN-COUNT                 PIC 9(03)     VALUE 99.
       01  LN-FEED                  PIC 9(02).
       01  PAGE-NUMBER              PIC 9(04)     VALUE ZEROES.

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
           "CODE DESCRIPTION BRANCH FILE (CDBFILE) COMPARE A15 TO S35".

       01  HEADER-LINE-03.
           03  FILLER                PIC X(4)  VALUE "TYPE".
           03  FILLER                PIC X(2)  VALUE SPACES.
           03  FILLER                PIC X(4)  VALUE "BRNO".
           03  FILLER                PIC X(2)  VALUE SPACES.
           03  FILLER                PIC X(8)  VALUE "  CODE  ".
           03  FILLER                PIC X(3)  VALUE SPACES.
           03  FILLER                PIC X(23) VALUE "DESCRIPTION".
           03  FILLER                PIC X(27) VALUE "A15".
           03  FILLER                PIC X(3)  VALUE "S35".

      ******************************************************************
      *
      *    D E T A I L - L I N E - W O R K E R S
      *
      ******************************************************************
       01  DETAIL-LINE                        PIC X(120)   VALUE SPACES.

       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(1).
           03  D01-TYPE                       PIC X(2).
           03  FILLER                         PIC X(3).
           03  D01-BRNO                       PIC ZZZ9.
           03  FILLER                         PIC X(2).
           03  D01-CODE                       PIC X(8).
           03  FILLER                         PIC X(3).
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
       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBUT/CDBFLE_DEF.CPY".
       COPY "LIBUT/CDBFLE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/CDBFLE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE OCD-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-CDB1-FILE.
           PERFORM CLOSE-OCD1-FILE.
           PERFORM CLOSE-PR-FILE.
           PERFORM CLOSE-WK1-FILE.
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
           MOVE "CDBFLE" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE CDBFLE-QUEUE-POS  TO Q-POS.
           MOVE CDBFLE-COPIES-POS TO C-POS.
           MOVE CDBFLE-PRU-POS    TO P-POS.
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

           MOVE "PROCESSING S35 DATA" TO BTRACK-MESS.
           PERFORM BTRACK-GLOBAL.

           PERFORM OPEN-CDB1-FILE.
           PERFORM OPEN-PR-FILE.

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
              MOVE "Y" TO PROC-BRANCH-ARRAY(S1) 
           END-PERFORM.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WK-PATH.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WK2-PATH.

      * LOAD R12-ARRAY WITH A15 MACHINE (FROM A15 BRFILE) BY BRANCH
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

           CALL "C$TOLOWER" USING TEST-LOWER-Z1, VALUE 2.
           CALL "C$TOLOWER" USING TEST-LOWER-X5, VALUE 2.  

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
              GO TO ENTER-END-TYPE.

       ENTER-SOURCE-PATH.

           IF SOURCE-PATH = SPACES
              MOVE "A15_PATH" TO GETENV-BUF
              PERFORM GETENV-CALL
              IF ( STAT-GOOD )
                 MOVE GETENV-BUF TO SOURCE-PATH
              ELSE
                 MOVE "/USR/DATA" TO SOURCE-PATH
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

       ENTER-BEG-TYPE.

           MOVE LEGEND-BEG-TYPE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A020BEG-TYPE." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F3-KEY ) GO TO ENTER-ALL-TYPE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-TYPE.

           MOVE VDUBUF TO BEG-TYPE.

      *-----------------------------------------------------------------
       ENTER-END-TYPE.

           MOVE LEGEND-END-TYPE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A020END-TYPE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-TYPE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-TYPE.

           MOVE VDUBUF TO END-TYPE.

           IF ( BEG-TYPE > END-TYPE )
              MOVE "INVALID TYPE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-TYPE.

           GO TO ENTER-BEG-BRANCH.

      *-----------------------------------------------------------------
       ENTER-ALL-TYPE.

           MOVE "S RA000BEG-TYPE." TO VDU.
           MOVE "AA"                TO BEG-TYPE VDUBUF.
           PERFORM SCREENIO.

           MOVE "S RA000END-TYPE." TO VDU.
           MOVE "ZZ"               TO END-TYPE VDUBUF.
           PERFORM SCREENIO.

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

      ****************************************
       EO-EXEC-AUDIT SECTION.
      ****************************************

      ****************************************
       DISPLAY-PARAMETERS SECTION.
      ****************************************

           MOVE "S  A090S-PATH."           TO VDU.
           MOVE SOURCE-PATH                TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A020BEG-TYPE." TO VDU.
           MOVE BEG-TYPE              TO VDUBUF.
           PERFORM SCREENIO

           MOVE "S  A020END-TYPE." TO VDU.
           MOVE END-TYPE              TO VDUBUF.
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

       DISPLAY-PARAMETERS-EXIT.
           EXIT.

      *******************************************
       EXECUTION-MODULE SECTION.
      *******************************************

           MOVE 9999 TO PROGID-SLEEP-COUNTER.
           MOVE "LOADING WK2 FILE" TO PROGID-TEXT.
           PERFORM SEND-PROGRESS-INDICATOR.

      D    STOP MESS.

      *     MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE MACHINE-R1                 TO FILPATH-MACHINE.
      *     MOVE GR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO CDB-PATH-OVERRIDE.

           MOVE BEG-TYPE  TO QCDB1-WBEG-TYPE.
           MOVE END-TYPE  TO QCDB1-WEND-TYPE.
           MOVE ZEROS     TO QCDB1-WBEG-BRANCH.
           MOVE 9999      TO QCDB1-WEND-BRANCH.
           MOVE SPACES    TO QCDB1-WBEG-CODE.
           MOVE ALL 'Z'   TO QCDB1-WEND-CODE.

      * NEED TO LOAD S35 INTO W2-FILE
           PERFORM OPEN-WK2-FILE-OUTPUT.
           PERFORM OPEN-CDB1-FILE.
           PERFORM START-CDB1-FILE.
           PERFORM M-READ-CDB.
           PERFORM UNTIL CDB-EOF-SW = "Y"
              MOVE CDB-BRANCH TO WK2-KEY-BRNO
              MOVE CDB-TYPE   TO WK2-KEY-TYPE
              MOVE CDB-CODE   TO WK2-KEY-CODE
              MOVE CDB-REC    TO WK2-DATA
              PERFORM WRITE-WK2-FILE
              PERFORM M-READ-CDB
           END-PERFORM.
           PERFORM CLOSE-WK2-FILE.
           PERFORM CLOSE-CDB1-FILE.


           MOVE "LOADING WK FILE" TO PROGID-TEXT.
           PERFORM SEND-PROGRESS-INDICATOR.

      * DELETE/EMPTY TEMP FILE IF EXIST
           PERFORM OPEN-WK1-FILE-OUTPUT.
           PERFORM CLOSE-WK1-FILE.

      * OPENS FOR I/O - REWRITE NEEDS FILE OPENED FOR I/O
           PERFORM OPEN-WK1-FILE.  

      *** MUST LOAD R1 AND R2 CDFILE IN WORK FILE***
           MOVE SOURCE-PATH   TO OCD-ALL-BASE.
           MOVE MACHINE-R1    TO OCD-ALL-MACHINE.
           MOVE OCD-ALL-PATH  TO OCD-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE "MISSING R1 FILE" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO PROCESS-DRIVER-END.

           MOVE "PROCESSING A15 DATA" TO BTRACK-MESS.  
           PERFORM BTRACK-GLOBAL.

           MOVE "N" TO OCD-EOF-SW.
           PERFORM OPEN-OCD1-FILE.
           PERFORM LOAD-WORK-FILE.
           PERFORM CLOSE-OCD1-FILE. 

           MOVE MACHINE-R2   TO OCD-ALL-MACHINE.
           MOVE OCD-ALL-PATH TO OCD-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE "MISSING R2 FILE" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO PROCESS-DRIVER-END.

           MOVE "N" TO OCD-EOF-SW.
           PERFORM OPEN-OCD1-FILE.
           PERFORM LOAD-WORK-FILE.
           PERFORM CLOSE-OCD1-FILE. 
           PERFORM CLOSE-WK1-FILE.
 
           PERFORM OPEN-WK1-FILE.
           PERFORM M-READ-WK1.

           PERFORM OPEN-WK2-FILE.
           PERFORM M-READ-WK2.

           PERFORM PROCESS-NEXT-CDB
              UNTIL WK2-EOF-SW = "Y" AND WK-EOF-SW = "Y".
 
           PERFORM CLOSE-WK2-FILE.
           PERFORM CLOSE-WK1-FILE.

           GO TO PROCESS-DRIVER-END.

       PROCESS-NEXT-CDB.

           STRING "PROCESSING TYPE: ", WK-KEY-TYPE INTO PROGID-TEXT.
           PERFORM SEND-PROGRESS-INDICATOR.

           PERFORM COMPARE-FIELDS.

           IF WK-KEY < WK2-KEY
              PERFORM M-READ-WK1
           ELSE
              IF WK-KEY > WK2-KEY
                 PERFORM M-READ-WK2
              ELSE
                 PERFORM M-READ-WK1
                 PERFORM M-READ-WK2.

       PROCESS-DRIVER-END.
           EXIT. 

       LOAD-WORK-FILE SECTION.
           PERFORM M-READ-OCD1.
           IF OCD-EOF-SW = "Y"
              GO TO LOAD-WORK-FILE-END.

           IF OCD-TYPE < BEG-TYPE OR OCD-TYPE > END-TYPE
              GO TO LOAD-WORK-FILE.

           INITIALIZE WK-REC.

           IF OCD-TYPE = "CN" OR "GC"
              IF OCD-TYPE = "CN"
                MOVE OCD-REC         TO OCD-CN-REC
                MOVE OCD-CN-TYPE     TO WK-KEY-TYPE
                MOVE OCD-CN-BRANCH   TO WK-KEY-BRNO
                MOVE OCD-CN-CODE     TO WK-KEY-CODE
              ELSE
                MOVE OCD-REC         TO OCD-FEEREC
                MOVE OCD-FEETYPE     TO WK-KEY-TYPE
                MOVE OCD-FEEBR       TO WK-KEY-BRNO
                MOVE OCD-FEECD       TO WK-KEY-CODE
              END-IF 
              IF PROC-BRANCH-ARRAY(WK-KEY-BRNO) NOT = "Y"
                 GO TO LOAD-WORK-FILE
              END-IF
              MOVE OCD-REC TO WK-DATA
              IF WK-KEY-BRNO NUMERIC
                 PERFORM WRITE-WK1-FILE
                 IF IO-STATUS = 9 
                    IF R12-ARRAY(WK-KEY-BRNO) = OCD-ALL-MACHINE
                       PERFORM REWRITE-WK1-FILE
                    END-IF
                 ELSE
                    ADD 1 TO A15-RECORDS-PROCESSED
                 END-IF
              END-IF
           END-IF.

           GO TO LOAD-WORK-FILE.

       LOAD-WORK-FILE-END.
           EXIT. 

       M-READ-WK2 SECTION.
           PERFORM READ-WK2-FILE-NEXT.
           IF IO-BAD 
              MOVE HIGH-VALUES TO WK2-KEY
              MOVE "Y"         TO WK2-EOF-SW
           ELSE
              MOVE WK2-DATA     TO CDB-REC.

       EXIT-M-READ-WK1.
           EXIT.

       M-READ-WK1 SECTION.
           PERFORM READ-WK1-FILE-NEXT.
           IF IO-BAD 
              MOVE HIGH-VALUES TO WK-KEY
              MOVE "Y"         TO WK-EOF-SW
           ELSE
              MOVE WK-DATA     TO OCD-REC.

       EXIT-M-READ-WK1.
           EXIT.

       M-READ-CDB SECTION.
           PERFORM READ-CDB1-FILE-NEXT.
           IF IO-BAD
              MOVE "Y" TO CDB-EOF-SW
           ELSE
              IF PROC-BRANCH-ARRAY(CDB-BRANCH) = "Y"
                 ADD 1 TO SQL-RECORDS-PROCESSED
              ELSE 
                 GO TO M-READ-CDB.

       EXIT-M-READ-CDB.
           EXIT.

       M-READ-OCD1 SECTION.
           PERFORM READ-OCD1-FILE-NEXT.
           IF IO-BAD  
              MOVE "Y" TO OCD-EOF-SW
              GO TO EXIT-M-READ-OCD1.

           IF OCD-TYPE = "CN"
              MOVE OCD-CN-CODE     TO WS-OCDB-CODE
           ELSE
              MOVE OCD-FEECD       TO WS-OCDB-CODE.

           IF WS-OCDB-CODE = TEST-LOWER-X5 OR
              WS-OCDB-CODE = TEST-LOWER-Z1 
              GO TO M-READ-OCD1.

       EXIT-M-READ-OCD1.
           EXIT.

      ******************************************************************
       COMPARE-FIELDS SECTION.
      ******************************************************************

           IF WK-KEY < CDB1-KEY
              MOVE "MISSING S35 RECORD" TO D01-TEXT
              MOVE WK-KEY-TYPE TO D01-TYPE
              MOVE WK-KEY-BRNO TO D01-BRNO
              MOVE WK-KEY-CODE TO D01-CODE
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.

           IF WK-KEY > CDB1-KEY
              MOVE "MISSING A15 RECORD" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.

           IF OCD-TYPE = "CN"
             MOVE OCD-REC TO OCD-CN-REC
           ELSE
             MOVE OCD-REC TO OCD-FEEREC.
 
           IF CDB-TYPE = "CN"
              IF CDB-DESC NOT = OCD-CN-DESC
                 MOVE "CDB-DESC"  TO D01-TEXT
                 MOVE OCD-CN-DESC TO D01-A15
                 MOVE CDB-DESC    TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CDB-CN-GROUP-ID NOT = OCD-CN-GROUP-ID
                 MOVE "CDB-CN-GROUP-ID" TO D01-TEXT
                 MOVE OCD-CN-GROUP-ID   TO D01-A15
                 MOVE CDB-CN-GROUP-ID   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OCD-CN-PHONE NOT NUMERIC OR OCD-CN-PHONE = 0202020000
                  MOVE 0 TO OCD-CN-PHONE
              END-IF
              IF CDB-CN-PHONE NOT = OCD-CN-PHONE
                 MOVE "CDB-CN-PHONE"  TO D01-TEXT
                 MOVE OCD-CN-PHONE    TO D01-A15
                 MOVE CDB-CN-PHONE    TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OCD-CN-PHONE-EXT NOT NUMERIC
                  MOVE 0 TO OCD-CN-PHONE-EXT
              END-IF
              IF CDB-CN-PHONE-EXT NOT = OCD-CN-PHONE-EXT
                 MOVE "CD-CN-PHONE-EXT" TO D01-TEXT
                 MOVE OCD-CN-PHONE-EXT  TO D01-A15
                 MOVE CDB-CN-PHONE-EXT  TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-IF.

           IF CDB-TYPE = "GC"
              IF CDB-GC-GLFEENO NOT = OCD-GLFEENO
                 MOVE "CDB-GC-GLFEENO" TO D01-TEXT
                 MOVE OCD-GLFEENO      TO D01-A15 
                 MOVE CDB-GC-GLFEENO   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CDB-GC-GLFEEFINCHG NOT = OCD-GLFEEFINCHG
                 MOVE "CDB-GC-GLFEEFINCHG" TO D01-TEXT
                 MOVE OCD-GLFEEFINCHG      TO D01-A15 
                 MOVE CDB-GC-GLFEEFINCHG   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CDB-GC-GLFEE-INT-CHARGABLE NOT = 
                 OCD-GLFEE-INT-CHARGABLE
                 MOVE "CDB-GC-GLFEE-INT-CHARGABLE" TO D01-TEXT
                 MOVE OCD-GLFEE-INT-CHARGABLE      TO D01-A15
                 MOVE CDB-GC-GLFEE-INT-CHARGABLE   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CDB-GC-FEE-GIFTNET NOT = OCD-FEE-GIFTNET
                 MOVE "CDB-GC-FEE-GIFTNET" TO D01-TEXT
                 MOVE OCD-FEE-GIFTNET      TO D01-A15 
                 MOVE CDB-GC-FEE-GIFTNET   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OCD-FEE-PRODNO NOT NUMERIC
                  MOVE 0 TO OCD-FEE-PRODNO
              END-IF
              IF CDB-GC-FEE-PRODNO NOT = OCD-FEE-PRODNO
                 MOVE "CDB-GC-FEE-PRODNO" TO D01-TEXT
                 MOVE OCD-FEE-PRODNO      TO D01-A15 
                 MOVE CDB-GC-FEE-PRODNO   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OCD-FEE-PRICE NOT NUMERIC
                  MOVE 0 TO OCD-FEE-PRICE
              END-IF
              IF CDB-GC-FEE-PRICE NOT = OCD-FEE-PRICE
                 MOVE "CDB-GC-FEE-PRICE" TO D01-TEXT
                 MOVE OCD-FEE-PRICE      TO D01-A15-NUM
                 MOVE CDB-GC-FEE-PRICE   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OCD-NEW-PRODNO NOT NUMERIC
                  MOVE 0 TO OCD-NEW-PRODNO
              END-IF
              IF CDB-GC-NEW-PRODNO NOT = OCD-NEW-PRODNO
                 MOVE "CDB-GC-NEW-PRODNO" TO D01-TEXT
                 MOVE OCD-NEW-PRODNO      TO D01-A15 
                 MOVE CDB-GC-NEW-PRODNO   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CDB-GC-FEE-MAXAMT NOT = OCD-FEE-MAXAMT
                 MOVE "CDB-GC-FEE-MAXAMT" TO D01-TEXT
                 MOVE OCD-FEE-MAXAMT      TO D01-A15-NUM
                 MOVE CDB-GC-FEE-MAXAMT   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CDB-GC-ALT-GLFEENO NOT = OCD-ALT-GLFEENO
                 MOVE "CDB-GC-ALT-GLFEENO" TO D01-TEXT
                 MOVE OCD-ALT-GLFEENO      TO D01-A15 
                 MOVE CDB-GC-ALT-GLFEENO   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OCD-COMM-GLFEENO NOT NUMERIC OR 
                 OCD-COMM-GLFEENO = 02020202020
                 MOVE 0 TO OCD-COMM-GLFEENO
              END-IF
              IF CDB-GC-COMM-GLFEENO NOT = OCD-COMM-GLFEENO
                 MOVE "CDB-GC-COMM-GLFEENO" TO D01-TEXT
                 MOVE OCD-COMM-GLFEENO      TO D01-A15 
                 MOVE CDB-GC-COMM-GLFEENO   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OCD-COMM-PERCENT NOT NUMERIC OR 
                 OCD-COMM-PERCENT = 20.2
                  MOVE 0 TO OCD-COMM-PERCENT
              END-IF
              IF CDB-GC-COMM-PERCENT NOT = OCD-COMM-PERCENT
                 MOVE "CDB-GC-COMM-PERCENT" TO D01-TEXT
                 MOVE OCD-COMM-PERCENT      TO D01-A15-NUM
                 MOVE CDB-GC-COMM-PERCENT   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CDB-GC-FEE-EARNINGS-FG NOT = OCD-FEE-EARNINGS-FG
                 MOVE "CDB-GC-FEE-EARNINGS-FG" TO D01-TEXT
                 MOVE OCD-FEE-EARNINGS-FG      TO D01-A15 
                 MOVE CDB-GC-FEE-EARNINGS-FG   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OCD-BRANCH-COMM-AMOUNT NOT NUMERIC
                  MOVE 0 TO OCD-BRANCH-COMM-AMOUNT
              END-IF
              IF CDB-GC-BRANCH-COMM-AMOUNT NOT = OCD-BRANCH-COMM-AMOUNT
                 MOVE "CDB-GC-BRANCH-COMM-AMOUNT" TO D01-TEXT
                 MOVE OCD-BRANCH-COMM-AMOUNT      TO D01-A15 
                 MOVE CDB-GC-BRANCH-COMM-AMOUNT   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-IF.

        EXIT-COMPARE-FIELDS.
           EXIT.

       WRITE-DETAIL-LINE-ERROR-KEY SECTION.

           MOVE CDB-TYPE   TO D01-TYPE.
           MOVE CDB-BRANCH TO D01-BRNO.
           MOVE CDB-CODE   TO D01-CODE.

       WRITE-DETAIL-LINE-ERROR SECTION.

           ADD 1    TO TOTAL-ERRORS-CTR.
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

           MOVE "BEGINNING TYPE                : " TO R01-PROMPT.
           MOVE BEG-TYPE                           TO R01-NUMERIC.
           MOVE 01                                 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING TYPE                   : " TO R01-PROMPT.
           MOVE END-TYPE                           TO R01-NUMERIC.
           MOVE 01                                 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( GROUP-FLAG = "Y" )
              MOVE "BEGINNING BRANCH              : "  TO R01-PROMPT
              MOVE ENT-GROUP                          TO R01-TEXT
              MOVE 02                                 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH                 : "  TO R01-PROMPT
              MOVE SPACES                             TO R01-TEXT
              MOVE 01                                 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEGINNING BRANCH              : "  TO R01-PROMPT
              MOVE BEG-BRANCH                         TO R01-NUMERIC
              MOVE 02                                 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING BRANCH                 : "  TO R01-PROMPT
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
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/PROGID.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO CDBFLE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CDBFLE-SYSDATE.
           DISPLAY CDBFLE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE CDBFLE-FORM-FIELDS TO WR-BUF.
           MOVE CDBFLE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE CDBFLE-HELP--FILE TO HELP-LINK-FILE.
           MOVE CDBFLE-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/CDBFLE_EVA.CPY".
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
           MOVE EXT-ROUTE-BUF          TO PROGID-ROUTE-BUF.
           MOVE 9999                   TO PROGID-SLEEP-COUNTER.
           MOVE 250                    TO PROGID-SLEEP-COUNTER-MAX.

           PERFORM GET-TIME.
           MOVE TIME-EDIT              TO PROGID-TIME.

           MOVE "B"                    TO PROGID-TYPE.
           MOVE PROGID-TEXT-PROCESSING TO PROGID-TEXT.

      *-----------------------------------------------------------------
       SEND-PROGRESS-INDICATOR.

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
       COPY "LIBLP/LPCDBGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPCDB1RN.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".

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
       OPEN-OCD1-FILE.
           PERFORM OPEN-IT.
           MOVE OCD-PATH TO E-FILE.
           OPEN INPUT OCD-FILE.
           IF IO-FG = 8
              GO TO OPEN-OCD1-FILE.
           IF IO-FG = 7
              CLOSE OCD-FILE
              GO TO OPEN-OCD1-FILE.
        READ-OCD1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OCD-PATH TO E-FILE.
           READ OCD-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OCD1-FILE-NEXT.
       CLOSE-OCD1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OCD-FILE.

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
       READ-WK1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           MOVE WK-KEY  TO E-KEYX.
           READ WK-FILE NEXT
              AT END  MOVE 9 TO IO-STATUS IO-FG
           END-READ.
           IF IO-FG = 8
              GO TO READ-WK1-FILE-NEXT.
       WRITE-WK1-FILE.
           PERFORM WRITE-IT.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           MOVE WK-KEY  TO E-KEYX.
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
           MOVE WK-KEY  TO E-KEYX.
           REWRITE WK-REC.
           IF IO-FG = 8
             GO TO REWRITE-WK1-FILE.
           UNLOCK WK-FILE.
       CLOSE-WK1-FILE.
           PERFORM CLOSE-IT.
           MOVE 0 TO IO-FG IO-STATUS.
           CLOSE WK-FILE.

       OPEN-WK2-FILE.
           PERFORM OPEN-IT.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK2-PATH TO E-FILE.
           OPEN I-O WK2-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK2-FILE.
           IF IO-FG = 7
              CLOSE WK2-FILE
              GO TO OPEN-WK2-FILE.

       OPEN-WK2-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK2-PATH TO E-FILE.
           OPEN OUTPUT WK2-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK2-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK2-FILE
              GO TO OPEN-WK2-FILE-OUTPUT.

       READ-WK2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK2-PATH TO E-FILE.
           MOVE WK2-KEY  TO E-KEYX.
           READ WK2-FILE NEXT
              AT END  MOVE 9 TO IO-STATUS IO-FG
           END-READ.
           IF IO-FG = 8
              GO TO READ-WK2-FILE-NEXT.

       WRITE-WK2-FILE.
           PERFORM WRITE-IT.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK2-PATH TO E-FILE.
           MOVE WK2-KEY  TO E-KEYX.
           WRITE WK2-REC 
             INVALID KEY 
                MOVE 9 TO IO-STATUS IO-FG
           END-WRITE.
           IF IO-FG = 8
             GO TO WRITE-WK2-FILE.
           UNLOCK WK2-FILE.

       CLOSE-WK2-FILE.
           PERFORM CLOSE-IT.
           MOVE 0 TO IO-FG IO-STATUS.
           CLOSE WK2-FILE.


      * COPYMEMBER: LIBGB/GBBRIN
       OPEN-OBR-FILE.
           PERFORM OPEN-IT.
           MOVE OBR-PATH TO E-FILE.
           OPEN INPUT OBR-FILE.
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

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE EXIT-PATH               TO FORM-NAM
              MOVE FORM-PATH               TO FORM-PATHNAME.

           MOVE WK-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

           MOVE WK2-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY CDBFLE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
