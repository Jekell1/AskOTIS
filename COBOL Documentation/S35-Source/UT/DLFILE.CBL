       IDENTIFICATION DIVISION.
       PROGRAM-ID.    DLFILE.
       DATE-WRITTEN.  02-05-2024.
       AUTHOR. MARK BOOMGAARD.
      ******************************************************************
      *
      * DESCRIPTION   : COMPARE S35 DLFILE WITH A15 DLFILE
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

      * COPYMEMBER: LIBLP/LPFSDL
           SELECT ODL-FILE ASSIGN TO ODL-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY ODL1-KEY
                  ALTERNATE RECORD KEY ODL2-KEY WITH DUPLICATES
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
        FD  ODL-FILE
           LABEL RECORDS ARE STANDARD.
      *-----------------------------------------------------------------
       COPY "LIBUP/A15SRC/LP01DL.CPY" REPLACING LEADING "DL" BY "ODL".
      *-----------------------------------------------------------------

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/DLFILE S35 01/30/24-10:18:54 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01DL.CPY".
       COPY "LIBLP/LP01DL_SQL.CPY".
       COPY "LIBLP/LPWSDL.CPY".

       01  ODL-PATH                  PIC X(35) VALUE SPACES.
       01  ODL-ALL-PATH.
           03  ODL-ALL-BASE          PIC X(9).
           03  FILLER                PIC X     VALUE "/".
           03  ODL-ALL-MACHINE       PIC XX.
           03  FILLER                PIC X    VALUE "/".
           03  ODL-ALL-DATA          PIC X(6).
           03  FILLER                PIC X(11) VALUE "/LP/DLFILE".

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
       01  DL-EOF-SW                   PIC X      VALUE "N".
       01  ODL-EOF-SW                  PIC X      VALUE "N".
       01  PREV-BRANCH-ERR-SW          PIC X      VALUE "N".
       01  A15-FILE-FOUND-SW           PIC X      VALUE "N".
       01  TOTAL-ERRORS-CTR            PIC 9(9)   VALUE 0.
       01  SUB1                        PIC 9      VALUE 0.
       01  SUB2                        PIC 9      VALUE 0.
       01  HOLD-DETAIL-LINE            PIC X(120) VALUE SPACES.
       01  PRINTING-RANGES-SW          PIC X      VALUE "N".

       01  ODL1-USE-KEY.
           03 ODL1-USE-KEY-BRNO        PIC 9(4).
           03 ODL1-USE-KEY-DLNUM       PIC 9(6).

       01  MACHINE-R1.
           03  FILLER                  PIC X VALUE X"72".
           03  FILLER                  PIC 9 VALUE 1.
       01  MACHINE-R2.
           03  FILLER                  PIC X VALUE X"72".
           03  FILLER                  PIC 9 VALUE 2.

       01  MISC-CURSOR-STAT            PIC X(2) VALUE SPACES.
           88  MISC-CURSOR-STAT-GOOD            VALUE "00".
           88  MISC-CURSOR-STAT-BAD             VALUE "  " THRU "//"
                                                      "01" THRU "~~".

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
           03  FILLER                  PIC X(26) VALUE SPACES.
           03  FILLER                  PIC X(6)  VALUE "PAGE: ".
           03  H01-PAGE                PIC Z(04).

      *-----------------------------------------------------------------
       01  HEADER-LINE-02.
           03  FILLER                  PIC X(10) VALUE "USER ID:".
           03  H02-USERID              PIC X(10) VALUE SPACES.
           03  FILLER                  PIC X(5)  VALUE SPACES.
           03  H02-TITLE               PIC X(65) VALUE 
           "DEALER (DLFILE) COMPARE A15 TO S35".

       01  HEADER-LINE-03.
           03  FILLER                PIC X(4)  VALUE "BRNO".
           03  FILLER                PIC X(2)  VALUE SPACES.
           03  FILLER                PIC X(6)  VALUE "DL NUM".
           03  FILLER                PIC X(3)  VALUE SPACES.
           03  FILLER                PIC X(11) VALUE "DESCRIPTION".
           03  FILLER                PIC X(11) VALUE SPACES.
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
           03  FILLER                         PIC X(2).
           03  D01-NUM                        PIC 9(6).
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

       01  WARN-MSG-9000                      PIC X(100) VALUE
           "* WARNING-CODE B RECORDS FOUND UNDER 9000+ BRANCH IN A15".

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
       COPY "LIBUT/DLFILE_DEF.CPY".
       COPY "LIBUT/DLFILE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/DLFILE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE ODL-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-DL1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE ODL-FILE.
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
           MOVE "DLFILE" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE DLFILE-QUEUE-POS  TO Q-POS.
           MOVE DLFILE-COPIES-POS TO C-POS.
           MOVE DLFILE-PRU-POS    TO P-POS.
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
              MOVE EXT-EOBUF-GROUP-PRPATH  TO PRU-FILE
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

           MOVE 9999 TO PROGID-SLEEP-COUNTER.
           PERFORM SEND-PROGRESS-INDICATOR.

      D    STOP MESS.

           IF PREV-BRANCH-ERR-SW = "Y"
              PERFORM WRITE-DETAIL-LINE.

           MOVE "N" TO PREV-BRANCH-ERR-SW.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO DL-PATH-OVERRIDE.

           MOVE BR-NO  TO QDL1-WBEG-BRNO 
                          QDL1-WEND-BRNO.

           MOVE ZEROS   TO QDL1-WBEG-NUM.
           MOVE ALL '9' TO QDL1-WEND-NUM.

           MOVE "N"      TO ODL-EOF-SW 
                            DL-EOF-SW.

           PERFORM OPEN-DL1-FILE.
           PERFORM START-DL1-FILE.
           PERFORM M-READ-DL.

           MOVE SOURCE-PATH   TO ODL-ALL-BASE.
           MOVE MACHINE-R1    TO ODL-ALL-MACHINE.
           MOVE BR-DATA-PATH  TO ODL-ALL-DATA.

           MOVE ODL-ALL-PATH  TO ODL-PATH ACCESS-BUF.
           MOVE "Y"           TO A15-FILE-FOUND-SW.

           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE MACHINE-R2   TO ODL-ALL-MACHINE
              MOVE ODL-ALL-PATH TO ODL-PATH ACCESS-BUF
              PERFORM ACCESS-CALL
              IF ( STAT-BAD )
                 IF DL-EOF-SW = "N" 
                    MOVE "MISSING A15 FILE" TO D01-TEXT
                    MOVE BR-NO              TO D01-BRNO
                    PERFORM WRITE-DETAIL-LINE-ERROR
                 END-IF
                 MOVE "N" TO A15-FILE-FOUND-SW.

           IF A15-FILE-FOUND-SW = "N"
              MOVE ALL '9'  TO ODL1-USE-KEY
              MOVE "Y"      TO ODL-EOF-SW
           ELSE
              PERFORM OPEN-ODL1-FILE
              PERFORM M-READ-ODL1.

           PERFORM PROCESS-BRANCH-NEXT-DL
              UNTIL DL-EOF-SW = "Y" AND ODL-EOF-SW = "Y".

           PERFORM CLOSE-ODL1-FILE.
           PERFORM CLOSE-DL1-FILE.

           GO TO PROCESS-BRANCH-DRIVER-END.

       PROCESS-BRANCH-NEXT-DL.

           PERFORM SEND-PROGRESS-INDICATOR.

           PERFORM COMPARE-FIELDS.

           IF ODL1-USE-KEY < DL1-KEY
              PERFORM M-READ-ODL1
           ELSE
              IF ODL1-USE-KEY > DL1-KEY
                 PERFORM M-READ-DL
              ELSE
                 PERFORM M-READ-ODL1
                 PERFORM M-READ-DL.

       PROCESS-BRANCH-DRIVER-END.
           EXIT. 

       M-READ-DL SECTION.
           PERFORM READ-DL1-FILE-NEXT.
           IF IO-BAD
              MOVE ALL '9' TO DL1-KEY
              MOVE "Y"     TO DL-EOF-SW
           ELSE
              ADD 1 TO SQL-RECORDS-PROCESSED.

       EXIT-M-READ-DL.
           EXIT.

       M-READ-ODL1 SECTION.
           PERFORM READ-ODL1-FILE-NEXT.
           IF IO-BAD
              MOVE ALL '9'  TO ODL1-USE-KEY
              MOVE "Y"      TO ODL-EOF-SW
           ELSE
              IF ODL-NUM NOT NUMERIC
                 GO TO M-READ-ODL1
              ELSE
                 ADD 1        TO A15-RECORDS-PROCESSED
                 MOVE BR-NO   TO ODL1-USE-KEY-BRNO
                 MOVE ODL-NUM TO ODL1-USE-KEY-DLNUM.

       EXIT-M-READ-ODL1.
           EXIT.

      ******************************************************************
       COMPARE-FIELDS SECTION.
      ******************************************************************

           IF ODL1-USE-KEY < DL1-KEY
              MOVE "MISSING S35 RECORD" TO D01-TEXT
              MOVE BR-NO      TO D01-BRNO
              MOVE ODL-NUM    TO D01-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.

           IF ODL1-USE-KEY > DL1-KEY
              MOVE "MISSING A15 RECORD" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.

           IF DL-NAME NOT = ODL-NAME
              MOVE "DL-NAME" TO D01-TEXT
              MOVE ODL-NAME  TO D01-A15
              MOVE DL-NAME   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF DL-ADR1 NOT = ODL-ADR1
              MOVE "DL-ADR1" TO D01-TEXT
              MOVE ODL-ADR1  TO D01-A15
              MOVE DL-ADR1   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF DL-ADR2 NOT = ODL-ADR2
              MOVE "DL-ADR2"  TO D01-TEXT
              MOVE ODL-ADR2   TO D01-A15
              MOVE DL-ADR2    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF DL-CITY NOT = ODL-CITY
              MOVE "DL-CITY" TO D01-TEXT
              MOVE ODL-CITY  TO D01-A15
              MOVE DL-CITY   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF DL-STATE NOT = ODL-STATE
              MOVE "DL-STATE" TO D01-TEXT
              MOVE ODL-STATE  TO D01-A15
              MOVE DL-STATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.
 
           IF DL-ZIP NOT = ODL-ZIP
              MOVE "DL-ZIP" TO D01-TEXT
              MOVE ODL-ZIP  TO D01-A15
              MOVE DL-ZIP   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF DL-SHORT NOT = ODL-SHORT
              MOVE "DL-SHORT" TO D01-TEXT
              MOVE ODL-SHORT  TO D01-A15
              MOVE DL-SHORT   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF ODL-PHONE NOT NUMERIC
              MOVE 0   TO ODL-PHONE.
           IF DL-PHONE NOT = ODL-PHONE
              MOVE "DL-PHONE" TO D01-TEXT
              MOVE ODL-PHONE  TO D01-A15
              MOVE DL-PHONE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF DL-PURPOSE NOT = ODL-PURPOSE
              MOVE "DL-PURPOSE" TO D01-TEXT
              MOVE ODL-PURPOSE  TO D01-A15
              MOVE DL-PURPOSE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF DL-MFGCODE NOT = ODL-MFGCODE
              MOVE "DL-MFGCODE"  TO D01-TEXT
              MOVE ODL-MFGCODE   TO D01-A15
              MOVE DL-MFGCODE    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE ODL-ORIGDATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = DL-ORIGDATE
              MOVE "DL-ORIGDATE" TO D01-TEXT
              MOVE ODL-ORIGDATE  TO D01-A15
              MOVE DL-ORIGDATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE ODL-TERMDATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           PERFORM TEST-INVALID-DATE.
           IF DATE-YYYYMMDD NOT = DL-TERMDATE
              MOVE "DL-TERMDATE" TO D01-TEXT
              MOVE ODL-TERMDATE  TO D01-A15
              MOVE DL-TERMDATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF ODL-LC-CODE NOT NUMERIC
              MOVE 0  TO ODL-LC-CODE.
           IF DL-LC-CODE NOT = ODL-LC-CODE
              MOVE "DL-LC-CODE" TO D01-TEXT
              MOVE ODL-LC-CODE  TO D01-A15
              MOVE DL-LC-CODE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

            IF ODL-FULLCEIL NOT NUMERIC
              MOVE 0  TO ODL-FULLCEIL.
              IF DL-FULLCEIL NOT = ODL-FULLCEIL
                 MOVE "DL-FULLCEIL" TO D01-TEXT
                 MOVE ODL-FULLCEIL  TO D01-A15-NUM
                 MOVE DL-FULLCEIL   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF ODL-PARTCEIL NOT NUMERIC
              MOVE 0 TO ODL-PARTCEIL.
              IF DL-PARTCEIL NOT = ODL-PARTCEIL
                 MOVE "DL-PARTCEIL"  TO D01-TEXT
                 MOVE ODL-PARTCEIL   TO D01-A15-NUM
                 MOVE DL-PARTCEIL    TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR.

           IF ODL-NONCEIL NOT NUMERIC
              MOVE 0 TO ODL-NONCEIL.
           IF ODL-NONCEIL NOT = ODL-NONCEIL
              MOVE "DL-NONCEILR" TO D01-TEXT
              MOVE ODL-NONCEIL   TO D01-A15-NUM
              MOVE DL-NONCEIL    TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF DL-SIGN1 NOT = ODL-SIGN1
              MOVE "DL-SIGN1"  TO D01-TEXT
              MOVE ODL-SIGN1   TO D01-A15
              MOVE DL-SIGN1    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF DL-SIGN2 NOT = ODL-SIGN2
              MOVE "DL-SIGN2"  TO D01-TEXT
              MOVE ODL-SIGN2   TO D01-A15
              MOVE DL-SIGN2    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF DL-SIGN3 NOT = ODL-SIGN3
              MOVE "DL-SIGN3"  TO D01-TEXT
              MOVE ODL-SIGN3   TO D01-A15
              MOVE DL-SIGN3    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF ODL-MAXPURTRM NOT NUMERIC
              MOVE 0  TO ODL-MAXPURTRM.
           IF DL-MAXPURTRM NOT = ODL-MAXPURTRM
              MOVE "DL-MAXPURTRM" TO D01-TEXT
              MOVE ODL-MAXPURTRM  TO D01-A15
              MOVE DL-MAXPURTRM   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF ODL-MAXPURPER NOT NUMERIC
              MOVE 0  TO ODL-MAXPURPER.
           IF DL-MAXPURPER NOT = ODL-MAXPURPER
              MOVE "DL-MAXPURPER" TO D01-TEXT
              MOVE ODL-MAXPURPER  TO D01-A15
              MOVE DL-MAXPURPER   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF ODL-EARNCD NOT NUMERIC
              MOVE 0 TO ODL-EARNCD.
           IF DL-EARNCD NOT = ODL-EARNCD
              MOVE "DL-EARNCD" TO D01-TEXT
              MOVE ODL-EARNCD  TO D01-A15
              MOVE DL-EARNCD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF ODL-EARNFAC NOT NUMERIC
              MOVE 0 TO ODL-EARNFAC.
           IF DL-EARNFAC NOT = ODL-EARNFAC
              MOVE "DL-EARNFAC" TO D01-TEXT
              MOVE ODL-EARNFAC  TO D01-A15
              MOVE DL-EARNFAC   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF ODL-DISC-FRMLA NOT NUMERIC
              MOVE 0  TO ODL-DISC-FRMLA.
           IF DL-DISC-FRMLA NOT = ODL-DISC-FRMLA
              MOVE "DL-DISC-FRMLA" TO D01-TEXT
              MOVE ODL-DISC-FRMLA  TO D01-A15
              MOVE DL-DISC-FRMLA   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF DL-DISC-CODE NOT = ODL-DISC-CODE
              MOVE "DL-DISC-CODE" TO D01-TEXT
              MOVE DL-DISC-CODE   TO D01-A15
              MOVE DL-DISC-CODE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 6
              IF ODL-DISC-TERM(SUB1) NOT NUMERIC
                MOVE 0 TO ODL-DISC-TERM(SUB1)
              END-IF
              IF DL-DISC-TERM(SUB1) NOT = ODL-DISC-TERM(SUB1)
                 STRING "DL-DISC-TERM(",SUB1,")" INTO D01-TEXT
                 MOVE ODL-DISC-TERM(SUB1)  TO D01-A15 
                 MOVE DL-DISC-TERM(SUB1)   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF ODL-DISC-RATE(SUB1) NOT NUMERIC
                  MOVE 0 TO ODL-DISC-RATE(SUB1)
              END-IF
              IF DL-DISC-RATE(SUB1) NOT = ODL-DISC-RATE(SUB1)
                 STRING "DL-DISC-RATE(",SUB1,")" INTO D01-TEXT
                 MOVE ODL-DISC-RATE(SUB1)  TO D01-A15-NUM
                 MOVE DL-DISC-RATE(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.

           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 3
              IF DL-RVPRIN-FRMLA(SUB1) NOT = ODL-RVPRIN-FRMLA(SUB1)
                 STRING "DL-RVPRIN-FRMLA(",SUB1,")" INTO D01-TEXT
                 MOVE ODL-RVPRIN-FRMLA(SUB1)  TO D01-A15 
                 MOVE DL-RVPRIN-FRMLA(SUB1)   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF DL-RVPRIN-CODE(SUB1) NOT = ODL-RVPRIN-CODE(SUB1)
                 STRING "DL-RVPRIN-CODE(",SUB1,")" INTO D01-TEXT
                 MOVE ODL-RVPRIN-CODE(SUB1)  TO D01-A15 
                 MOVE DL-RVPRIN-CODE(SUB1)   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
               IF DL-RVPRIN-ADDON(SUB1) NOT = ODL-RVPRIN-ADDON(SUB1)
                 STRING "DL-RVPRIN-ADDON(",SUB1,")" INTO D01-TEXT
                 MOVE ODL-RVPRIN-ADDON(SUB1)  TO D01-A15-NUM
                 MOVE DL-RVPRIN-ADDON(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
 
              PERFORM VARYING SUB2 FROM 1 BY 1 UNTIL SUB2 > 6
                 IF DL-RVPRIN-TERM(SUB1,SUB2) NOT = 
                     ODL-RVPRIN-TERM(SUB1,SUB2)
                    STRING "DL-RVPRIN-TERM(",SUB1,")-",SUB2 
                       INTO D01-TEXT
                    MOVE ODL-RVPRIN-TERM(SUB1,SUB2)  TO D01-A15 
                    MOVE DL-RVPRIN-TERM(SUB1,SUB2)   TO D01-S35 
                    PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                    PERFORM WRITE-DETAIL-LINE-ERROR
                 END-IF
                 IF DL-RVPRIN-RATE(SUB1,SUB2) NOT = 
                     ODL-RVPRIN-RATE(SUB1,SUB2)
                    STRING "DL-RVPRIN-RATE(",SUB1,")-",SUB2 
                      INTO D01-TEXT
                    MOVE ODL-RVPRIN-RATE(SUB1,SUB2)  TO D01-A15-NUM
                    MOVE DL-RVPRIN-RATE(SUB1,SUB2)   TO D01-S35-NUM
                    PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                    PERFORM WRITE-DETAIL-LINE-ERROR
                 END-IF
              END-PERFORM
           END-PERFORM.


           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 3
              IF DL-RVCHGS-FRMLA(SUB1) NOT = ODL-RVCHGS-FRMLA(SUB1)
                 STRING "DL-RVCHGS-FRMLA(",SUB1,")" INTO D01-TEXT
                 MOVE ODL-RVCHGS-FRMLA(SUB1)  TO D01-A15 
                 MOVE DL-RVCHGS-FRMLA(SUB1)   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF DL-RVCHGS-CODE(SUB1) NOT = ODL-RVCHGS-CODE(SUB1)
                 STRING "DL-RVCHGS-CODE(",SUB1,")" INTO D01-TEXT
                 MOVE ODL-RVCHGS-CODE(SUB1)  TO D01-A15 
                 MOVE DL-RVCHGS-CODE(SUB1)   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
               IF DL-RVCHGS-ADDON(SUB1) NOT = ODL-RVCHGS-ADDON(SUB1)
                 STRING "DL-RVCHGS-ADDON(",SUB1,")" INTO D01-TEXT
                 MOVE ODL-RVCHGS-ADDON(SUB1)  TO D01-A15-NUM
                 MOVE DL-RVCHGS-ADDON(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
 
              PERFORM VARYING SUB2 FROM 1 BY 1 UNTIL SUB2 > 6
                 IF DL-RVCHGS-TERM(SUB1,SUB2) NOT = 
                     ODL-RVCHGS-TERM(SUB1,SUB2)
                    STRING "DL-RVCHGS-TERM(",SUB1,")-",SUB2 
                       INTO D01-TEXT
                    MOVE ODL-RVCHGS-TERM(SUB1,SUB2)  TO D01-A15 
                    MOVE DL-RVCHGS-TERM(SUB1,SUB2)   TO D01-S35 
                    PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                    PERFORM WRITE-DETAIL-LINE-ERROR
                 END-IF
                 IF DL-RVCHGS-RATE(SUB1,SUB2) NOT = 
                     ODL-RVCHGS-RATE(SUB1,SUB2)
                    STRING "DL-RVCHGS-RATE(",SUB1,")-",SUB2  
                       INTO D01-TEXT
                    MOVE ODL-RVCHGS-RATE(SUB1,SUB2)  TO D01-A15-NUM
                    MOVE DL-RVCHGS-RATE(SUB1,SUB2)   TO D01-S35-NUM
                    PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                    PERFORM WRITE-DETAIL-LINE-ERROR
                 END-IF
              END-PERFORM
           END-PERFORM.

           IF DL-PARVHELD-FRMLA NOT = ODL-PARVHELD-FRMLA
              MOVE "DL-PARVHELD-FRMLA" TO D01-TEXT
              MOVE ODL-PARVHELD-FRMLA  TO D01-A15
              MOVE DL-PARVHELD-FRMLA   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.
 
           IF DL-PARVHELD-CODE NOT = ODL-PARVHELD-CODE
              MOVE "DL-PARVHELD-CODE" TO D01-TEXT
              MOVE ODL-PARVHELD-CODE  TO D01-A15
              MOVE DL-PARVHELD-CODE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF DL-PARVHELD-ADDON NOT = ODL-PARVHELD-ADDON
              MOVE "DL-PARVHELD-ADDON" TO D01-TEXT
              MOVE ODL-PARVHELD-ADDON  TO D01-A15-NUM
              MOVE DL-PARVHELD-ADDON   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 6
              IF DL-PARVHELD-TERM(SUB1) NOT = ODL-PARVHELD-TERM(SUB1)
                 STRING "DL-PARVHELD-TERM(",SUB1,")" INTO D01-TEXT
                 MOVE ODL-PARVHELD-TERM(SUB1) TO D01-A15 
                 MOVE DL-PARVHELD-TERM(SUB1) TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF DL-PARVHELD-RATE(SUB1) NOT = ODL-PARVHELD-RATE(SUB1)
                 STRING "DL-PARVHELD-RATE(",SUB1,")" INTO D01-TEXT
                 MOVE ODL-PARVHELD-RATE(SUB1) TO D01-A15-NUM
                 MOVE DL-PARVHELD-RATE(SUB1) TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.


           IF DL-PARVPAID-FRMLA NOT = ODL-PARVPAID-FRMLA
              MOVE "DL-PARVPAID-FRMLA" TO D01-TEXT
              MOVE ODL-PARVPAID-FRMLA  TO D01-A15
              MOVE DL-PARVPAID-FRMLA   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF DL-PARVPAID-CODE NOT = ODL-PARVPAID-CODE
              MOVE "DL-PARVPAID-CODE" TO D01-TEXT
              MOVE ODL-PARVPAID-CODE  TO D01-A15 
              MOVE DL-PARVPAID-CODE   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF DL-PARVPAID-ADDON NOT = ODL-PARVPAID-ADDON
              MOVE "DL-PARVPAID-ADDONN" TO D01-TEXT
              MOVE ODL-PARVPAID-ADDON  TO D01-A15-NUM
              MOVE DL-PARVPAID-ADDON   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 6
              IF DL-PARVPAID-TERM(SUB1) NOT = ODL-PARVPAID-TERM(SUB1)
                 STRING "DL-PARVPAID-TERM(",SUB1,")" INTO D01-TEXT
                 MOVE ODL-PARVPAID-TERM(SUB1) TO D01-A15 
                 MOVE DL-PARVPAID-TERM(SUB1) TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF DL-PARVPAID-RATE(SUB1) NOT = ODL-PARVPAID-RATE(SUB1)
                 STRING "DL-PARVPAID-RATE(",SUB1,")" INTO D01-TEXT
                 MOVE ODL-PARVPAID-RATE(SUB1) TO D01-A15-NUM
                 MOVE DL-PARVPAID-RATE(SUB1) TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.

           IF DL-SVACCT-FRMLA NOT = ODL-SVACCT-FRMLA
              MOVE "DL-SVACCT-FRMLAR" TO D01-TEXT
              MOVE ODL-SVACCT-FRMLA   TO D01-A15
              MOVE DL-SVACCT-FRMLA    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.
 
           IF DL-SVCODE NOT = ODL-SVCODE
              MOVE "DL-SVCODER" TO D01-TEXT
              MOVE ODL-SVCODE   TO D01-A15
              MOVE DL-SVCODE    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF DL-SVACCT-AMT NOT = ODL-SVACCT-AMT
              MOVE "DL-SVACCT-AMT" TO D01-TEXT
              MOVE ODL-SVACCT-AMT  TO D01-A15-NUM
              MOVE DL-SVACCT-AMT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.
 
           IF DL-SVACCT-RATE NOT = ODL-SVACCT-RATE
              MOVE "DL-SVACCT-RATE" TO D01-TEXT
              MOVE ODL-SVACCT-RATE  TO D01-A15-NUM
              MOVE DL-SVACCT-RATE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF DL-SVACCT-LCHGPER NOT = ODL-SVACCT-LCHGPER
              MOVE "DL-SVACCT-LCHGPER" TO D01-TEXT
              MOVE ODL-SVACCT-LCHGPER  TO D01-A15
              MOVE DL-SVACCT-LCHGPER   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF DL-SVACCT-INTPER NOT = ODL-SVACCT-INTPER
              MOVE "DL-SVACCT-INTPERR" TO D01-TEXT
              MOVE ODL-SVACCT-INTPER   TO D01-A15
              MOVE DL-SVACCT-INTPER    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF DL-SVACCT-DEFPER NOT = ODL-SVACCT-DEFPER
              MOVE "DL-SVACCT-DEFPER"  TO D01-TEXT
              MOVE ODL-SVACCT-DEFPER   TO D01-A15
              MOVE DL-SVACCT-DEFPER    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.
 
           IF DL-FULLCEIL-BAL NOT = ODL-FULLCEIL-BAL
              MOVE "DL-FULLCEIL-BAL" TO D01-TEXT
              MOVE ODL-FULLCEIL-BAL  TO D01-A15-NUM
              MOVE DL-FULLCEIL-BAL   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF DL-PARTCEIL-BAL NOT = ODL-PARTCEIL-BAL
              MOVE "DL-PARTCEIL-BAL" TO D01-TEXT
              MOVE ODL-PARTCEIL-BAL  TO D01-A15-NUM
              MOVE DL-PARTCEIL-BAL   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF DL-NONCEIL-BAL NOT = ODL-NONCEIL-BAL
              MOVE "DL-NONCEIL-BALD" TO D01-TEXT
              MOVE ODL-NONCEIL-BAL  TO D01-A15-NUM
              MOVE DL-NONCEIL-BAL   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF DL-AFP-DEALER NOT = ODL-AFP-DEALER
              MOVE "DL-AFP-DEALER" TO D01-TEXT
              MOVE ODL-AFP-DEALER  TO D01-A15
              MOVE DL-AFP-DEALER   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF DL-RECOURSE-FRMLA NOT = ODL-RECOURSE-FRMLA
              MOVE "DL-RECOURSE-FRMLA" TO D01-TEXT
              MOVE ODL-RECOURSE-FRMLA  TO D01-A15
              MOVE DL-RECOURSE-FRMLA   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF ODL-FAX-NUMBER NOT NUMERIC
              MOVE 0 TO ODL-FAX-NUMBER.
           IF DL-FAX-NUMBER NOT = ODL-FAX-NUMBER
              MOVE "DL-FAX-NUMBERO" TO D01-TEXT
              MOVE ODL-FAX-NUMBER   TO D01-A15
              MOVE DL-FAX-NUMBER    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF DL-PARVPAID-FRMLA-IB NOT = ODL-PARVPAID-FRMLA-IB
              MOVE "DL-PARVPAID-FRMLA-IB" TO D01-TEXT
              MOVE ODL-PARVPAID-FRMLA-IB  TO D01-A15
              MOVE DL-PARVPAID-FRMLA-IB   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF DL-PARVPAID-CODE-IB NOT = ODL-PARVPAID-CODE-IB
              MOVE "DL-PARVPAID-CODE-IB" TO D01-TEXT
              MOVE ODL-PARVPAID-CODE-IB  TO D01-A15
              MOVE DL-PARVPAID-CODE-IB   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF DL-LPO NOT = ODL-LPO
              MOVE "DL-LPO" TO D01-TEXT
              MOVE ODL-LPO  TO D01-A15
              MOVE DL-LPO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF DL-ACH-FG NOT = ODL-ACH-FG
              MOVE "DL-ACH-FG" TO D01-TEXT
              MOVE ODL-ACH-FG  TO D01-A15
              MOVE DL-ACH-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF ODL-BANK-ROUTING-NO NOT NUMERIC
              MOVE 0 TO ODL-BANK-ROUTING-NO.
           IF DL-BANK-ROUTING-NO NOT = ODL-BANK-ROUTING-NO
              MOVE "DL-BANK-ROUTING-NO" TO D01-TEXT
              MOVE ODL-BANK-ROUTING-NO  TO D01-A15
              MOVE DL-BANK-ROUTING-NO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF DL-BANK-ACCTNO NOT = ODL-BANK-ACCTNO
              MOVE "DL-BANK-ACCTNO" TO D01-TEXT
              MOVE ODL-BANK-ACCTNO  TO D01-A15
              MOVE DL-BANK-ACCTNO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF DL-ACH-PRENOTE-FG NOT = ODL-ACH-PRENOTE-FG 
              MOVE "DL-ACH-PRENOTE-FG" TO D01-TEXT
              MOVE ODL-ACH-PRENOTE-FG  TO D01-A15
              MOVE DL-ACH-PRENOTE-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF ODL-SAL-MISC-PCT = 20.20 OR 02.02 OR
                 (ODL-SAL-MISC-PCT NOT NUMERIC)
              MOVE 0 TO ODL-SAL-MISC-PCT.
           IF DL-SAL-MISC-PCT NOT = ODL-SAL-MISC-PCT
              MOVE "DL-SAL-MISC-PCT" TO D01-TEXT
              MOVE ODL-SAL-MISC-PCT  TO D01-A15-NUM
              MOVE DL-SAL-MISC-PCT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF ODL-SAL-MISC-PCT2 = 20.20 OR 02.02 OR
                 (ODL-SAL-MISC-PCT2 NOT NUMERIC)
              MOVE 0 TO ODL-SAL-MISC-PCT2.
           IF DL-SAL-MISC-PCT2 NOT = ODL-SAL-MISC-PCT2 
              MOVE "DL-SAL-MISC-PCT2" TO D01-TEXT
              MOVE ODL-SAL-MISC-PCT2  TO D01-A15-NUM
              MOVE DL-SAL-MISC-PCT2   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.
 
 
            IF DL-WAIVE-ACH-FEE NOT = ODL-WAIVE-ACH-FEE 
              MOVE "DL-WAIVE-ACH-FEE" TO D01-TEXT
              MOVE ODL-WAIVE-ACH-FEE  TO D01-A15
              MOVE DL-WAIVE-ACH-FEE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF DL-TRACK-CHANGE-FLAG NOT = ODL-TRACK-CHANGE-FLAG 
              MOVE "DL-TRACK-CHANGE-FLAG" TO D01-TEXT
              MOVE ODL-TRACK-CHANGE-FLAG  TO D01-A15
              MOVE DL-TRACK-CHANGE-FLAG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF DL-DBA-NAME NOT = ODL-DBA-NAME 
              MOVE "DL-DBA-NAME" TO D01-TEXT
              MOVE ODL-DBA-NAME  TO D01-A15
              MOVE DL-DBA-NAME   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

       EXIT-COMPARE-FIELDS.
           EXIT.

       WRITE-DETAIL-LINE-ERROR-KEY SECTION.

           MOVE BR-NO   TO D01-BRNO.
           MOVE DL-NUM  TO D01-NUM.

       WRITE-DETAIL-LINE-ERROR-KEY-EXIT.
           EXIT.

       WRITE-DETAIL-LINE-ERROR SECTION.

           ADD 1    TO TOTAL-ERRORS-CTR.
           MOVE "Y" TO PREV-BRANCH-ERR-SW.
           PERFORM WRITE-DETAIL-LINE.

       WRITE-DETAIL-LINE-ERROR-EXIT.
           EXIT.

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
           MOVE EXT-CONAME-CONAME TO DLFILE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO DLFILE-SYSDATE.
           DISPLAY DLFILE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE DLFILE-FORM-FIELDS TO WR-BUF.
           MOVE DLFILE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE DLFILE-HELP--FILE TO HELP-LINK-FILE.
           MOVE DLFILE-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/DLFILE_EVA.CPY".
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
       COPY "LIBLP/LPDLGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBLP/LPDL1RN.CPY".

       OPEN-ODL1-FILE.
           PERFORM OPEN-IT.
           MOVE ODL-PATH TO E-FILE.
           OPEN INPUT ODL-FILE.
           IF IO-FG = 8
              GO TO OPEN-ODL1-FILE.
           IF IO-FG = 7
              CLOSE ODL-FILE
              GO TO OPEN-ODL1-FILE.
        READ-ODL1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE ODL-PATH TO E-FILE.
      *     MOVE ODL-KEY  TO E-KEYX.
           READ ODL-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-ODL1-FILE-NEXT.
       CLOSE-ODL1-FILE.
           PERFORM CLOSE-IT.
           CLOSE ODL-FILE.

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
              DESTROY DLFILE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
