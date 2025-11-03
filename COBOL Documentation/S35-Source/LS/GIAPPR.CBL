       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GIAPPR.
       AUTHOR.        KEC.
       DATE-WRITTEN.  06/23/2014.
      ******************************************************************
      *
      *  IDENTIFICATION: LS/GIAPPR.CBL
      *
      *  DESCRIPTION   : BRANCH APPROVAL LIMIT LISTING
      *                  - APPROVAL LIMIT BASED FROM THE GIFILE
      *
      * IF EXTRACT IS "Y";
      *     /USR/EXTRACT/OPS/GIAPPR.TXT) 
      *         BRNO(TAB)CLASS(TAB)LIMIT(CR)
      *
      *=================================================================
      * REV :
      * KEC 2014.0623 {PR#999} WORLD <A15> [JIM ROSENAUER]
      *         ***NEW***
      * KEC 2017.0609 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          CHANGED VERIFY-GI-CLASS FROM 2 TO 3 BYTES.
      * BAH 2018.0906 WORLD #1332 ADD EXTRACT OPTION, ALL CLASSES OPTION
      *               FOR BOTH REPORT AND EXTRACT.
      * RMZ 2023.0926 PERFORMANCE TUNING - ADDED STORED PROCEDURE
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT GIAPPR-FILE ASSIGN OUTPUT GIAPPR-PATH
                           ORGANIZATION LINE SEQUENTIAL
                           LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                           FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
      **************************************************
      *  /USR/EXTRACT/OPS/GIAPPR.TXT)  RSZ 
      **************************************************
      *XFD  FILE=GIAPPR  COMMENTED OUT JKC 2019-0916
       FD  GIAPPR-FILE
           LABEL RECORDS ARE STANDARD.
       01  GIAPPR-REC.
           03  GIA-BRANCH               PIC 9(4).
           03  GIA-TAB1                 PIC X.
           03  GIA-CLASS                PIC 9(6).
           03  GIA-TAB2                 PIC X.
           03  GIA-LIMIT                PIC 9(7).
           03  GIA-CR                   PIC X.

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LS/GIAPPR.CBL S35 11/22/23-10:15:34 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGL/GL01GI.CPY".
       COPY "LIBGL/GL01GI_SQL.CPY".
       COPY "LIBGL/GLWSGI.CPY".

       01  GIAPPR-PATH.
           03  FILLER           PIC X(17) VALUE "/USR/EXTRACT/OPS/".
           03  FILLER           PIC X(10) VALUE "GIAPPR.TXT".

      *-----------------------------------------------------------------
       01  CURRENT-GI-BRANCH           PIC 9(04)     VALUE ZEROES.
       01  VERIFY-GI-CLASS             PIC 9(03)     VALUE ZEROES.
           88  VERIFY-GI-CLASS-GOOD                  VALUE 01 05 40.

       01  GI1-CONN-OPEN-SW     PIC X        VALUE "N".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  FIRST-TIME                  PIC X(01)     VALUE "Y".
       01  TOTAL-EXTRACTS              PIC 9(7)      VALUE 0.

       01  RECORDS-FOUND               PIC X(01)     VALUE SPACES.
           88  NO-RECORDS-FOUND                      VALUE "N".

      ******************************************************************
      *
      *    L E G E N D - W O R K E R S
      *
      ******************************************************************
       01  LEGEND                      PIC X(80)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(07)     VALUE "LEGEND.".

      *=================================================================
       01  LEGEND-BEG-BRANCH.
           03  FILLER PIC X(30) VALUE "F1-RESET, F6-SCAN, F7-EXIT, EN".
           03  FILLER PIC X(30) VALUE "TER BRANCH/GROUP              ".
           03  FILLER PIC X(30) VALUE "                    ".

       01  LEGEND-REST.
           03  FILLER PIC X(30) VALUE "F1-RESET, F6-SCAN, F7-EXIT ".

      ******************************************************************
      *
      *    R E P O R T - P R I N T - W O R K E R S               (R P W)
      *
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED                 PIC 9(02)     VALUE ZEROES.
           03  LN-COUNT                PIC 9(02)     VALUE 99.
           03  LN-WK                   PIC 9(03)     VALUE ZEROES.

       01  PAGE-NUMBER                 PIC 9(04)     VALUE ZEROES.
 
      *=================================================================
      *
      *    R E P O R T - H E A D E R - W O R K E R S
      *
      *=================================================================
       01  RPW-HEADER-01.
           03  RPW-H01-DATE            PIC X(08)     VALUE SPACES.
           03  FILLER                  PIC X(02)     VALUE SPACES.
           03  RPW-H01-TIME            PIC X(08)     VALUE SPACES.
           03  FILLER                  PIC X(02)     VALUE SPACES.
           03  RPW-H01-NAME            PIC X(40)     VALUE SPACES.
           03  FILLER                  PIC X(11)     VALUE SPACES.
           03  FILLER                  PIC X(05)     VALUE "PAGE ".
           03  RPW-H01-PAGE-NO         PIC ZZZ9.
 
      *-----------------------------------------------------------------
       01  RPW-HEADER-02.
           03  FILLER                  PIC X(10)    VALUE "USER ID:".
           03  RPW-H02-USERID          PIC X(10).
           03  RPW-H02-TITLE           PIC X(40)    VALUE
                                       " BRANCH APPROVAL LIMIT LISTING".
           03  FILLER                  PIC X(20).
 
      *-----------------------------------------------------------------
       01  RPW-HEADER-03.
           03  FILLER                  PIC X(80)    VALUE ALL "=".

      *=================================================================
      *
      *    R E P O R T - D E T A I L - W O R K E R S
      *
      *=================================================================
       01  DETAIL-LINE                               PIC X(80). 
 
      *-----------------------------------------------------------------
       01  RPW-DETAIL-01                   REDEFINES DETAIL-LINE.
           03  RPW-D01-BRANCH                        PIC X(07).
           03  RPW-D01-BRNO                          PIC ZZZ9.
           03  FILLER                                PIC X(10).
           03  RPW-D01-CLASS                         PIC X(06).
           03  RPW-D01-CLNO                          PIC Z9.
           03  RPW-D01-HYPHEN                        PIC X(03).
           03  RPW-D01-APPROVAL-LIMIT                PIC $$$$$$$.
           03  FILLER                                PIC X(41).

      *-----------------------------------------------------------------
       01  RPW-RANGE-01                    REDEFINES DETAIL-LINE.
           03  RPW-R01-PROMPT                        PIC X(25).
           03  RPW-R01-TEXT                          PIC X(55).
           03  RPW-R01-NUMBER              REDEFINES RPW-R01-TEXT
                                                     PIC ZZZ,ZZ9.

      ******************************************************************
      *
      *    S C R E E N - F I E L D S - W O R K E R S             (S F W)
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH          PIC 9(04).
           03  END-BRANCH          PIC 9(04).
           03  ENT-GROUP           PIC X(04).
           03  GROUP-FLAG          PIC X(01).
           03  REPORT-ALL-CLASSES  PIC X.
           03  EXTRACT-FLAG        PIC X.
           03  EXTRACT-ALL-CLASSES PIC X.

       01  SV-BEG-BR                   PIC 9(04)     VALUE 9999.
       01  SV-END-BR                   PIC 9(04)     VALUE 9999.
       01  SV-GROUP-FLAG               PIC X(01)     VALUE SPACES.
       01  SV-ENT-GROUP                PIC X(04)     VALUE SPACES.

       01  VERSION-CONTROL             PIC X(01)     VALUE "A".
 
      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLS/GIAPPR_DEF.CPY".
       COPY "LIBLS/GIAPPR_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
 
      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLS/GIAPPR_SCN.CPY".


       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-GI1-FILE.
           PERFORM CLOSE-GI1-CONNECTION.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME              TO FORM-PATH.
           MOVE "GIAPPR"                   TO FORM-PROG VDU-FORM-NAME.

      D    STOP MESS.
           PERFORM INITIALIZATION.
           IF ( EXT-EOBUF-ERRCD NOT = SPACES )
              GO TO END-PROGRAM.

           IF ( GROUP-FLAG = "N" )
              MOVE BEG-BRANCH              TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP                  TO GR-ID.
           PERFORM SEARCH-GR-INIT.

       NEXT-GROUP-LOC.

           IF ( GROUP-FLAG NOT = "Y" )
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.
           IF ( WGR-SRESULT = ZEROES )
              GO TO MAIN-PROGRAM-EXIT.

           MOVE WGR-SRESULT                TO BR-NO.
           PERFORM READ-BR-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

       NEXT-BRANCH-NO.

           PERFORM READ-BR-FILE-NEXT.
           IF ( IO-BAD ) OR ( BR-NO > END-BRANCH )
              GO TO MAIN-PROGRAM-EXIT.

       MOVE-BRANCH-PATH-INFO.

           MOVE BR-NO                      TO GI-BRANCH.
           MOVE ZEROES                     TO GI-CLASS.

           PERFORM START-GI1-FILE. *> SSP - GIFILEE

       PROCESS-NEXT-RECORD.

           PERFORM READ-GI1-FILE-NEXT. *> EMBEDDED - GIFILE
           IF ( IO-BAD )
              GO TO END-OF-GROUP.

           IF ( GI-BRANCH NOT = BR-NO )
              GO TO END-OF-GROUP.

      * CHECKS IF ALL CLASSES INCLUDED IN THIS SECTION
           IF EXTRACT-FLAG = "Y"
              PERFORM BUILD-EXTRACTION-RECORD.

      * EITHER ALL CLASSES OR VERIFY HARD CODED CLASSES
           IF REPORT-ALL-CLASSES NOT = "Y"
              MOVE GI-CLASS               TO VERIFY-GI-CLASS
              IF NOT ( VERIFY-GI-CLASS-GOOD )
                 GO TO PROCESS-NEXT-RECORD.

           IF ( FIRST-TIME = "Y" )
              MOVE "N" TO FIRST-TIME
              MOVE "Y" TO RECORDS-FOUND.

           IF ( GI-BRANCH NOT = CURRENT-GI-BRANCH )
              MOVE GI-BRANCH               TO CURRENT-GI-BRANCH
              MOVE SPACES                  TO DETAIL-LINE
              MOVE 01                      TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE "BRANCH"                TO RPW-D01-BRANCH
              MOVE GI-BRANCH               TO RPW-D01-BRNO.

           MOVE "CLASS"                    TO RPW-D01-CLASS.
           MOVE GI-CLASS                   TO RPW-D01-CLNO.
           MOVE " - "                      TO RPW-D01-HYPHEN.
           MOVE GI-LN-APPROVAL-LIMIT       TO RPW-D01-APPROVAL-LIMIT.
           MOVE 01                         TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           GO TO PROCESS-NEXT-RECORD.

       END-OF-GROUP.

           GO TO NEXT-GROUP-LOC.

       MAIN-PROGRAM-EXIT.

           IF ( NO-RECORDS-FOUND )
              GO TO END-PROGRAM.

           IF EXTRACT-FLAG = "Y"
              PERFORM CLOSE-GIAPPR-FILE.

           PERFORM RANGE-LINES.

           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
       COPY "LIBGB/CHKSEC.CPY".

           MOVE GIAPPR-QUEUE-POS  TO Q-POS.
           MOVE GIAPPR-COPIES-POS TO C-POS.
           MOVE GIAPPR-PRU-POS    TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF EXT-ROUTE-BUF = "00"
              MOVE "LISTING IN PROGRESS....." TO LEGEND
              PERFORM SEND-LEGEND.

           IF ( EXT-EOBUF-GROUP-FG = "Y" )
              MOVE BEG-BRANCH              TO SV-BEG-BR
              MOVE END-BRANCH              TO SV-END-BR
              MOVE GROUP-FLAG              TO SV-GROUP-FLAG
              MOVE ENT-GROUP               TO SV-ENT-GROUP
              MOVE EXT-EOBUF-GROUP         TO ENT-GROUP
              MOVE ZEROES                  TO BEG-BRANCH
              MOVE 9999                    TO END-BRANCH
              MOVE "Y"                     TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH  TO PRU-FILE.

      *- - - - - - - - - - - - - - - - - - - - - -{ OPEN GLOBAL FILES  }
           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-GI1-FILE.

           IF EXTRACT-FLAG = "Y"
              CALL "C$TOLOWER" USING GIAPPR-PATH, VALUE 13
              PERFORM OPEN-GIAPPR-FILE-OUTPUT.

           MOVE 0 TO TOTAL-EXTRACTS.

           MOVE ZEROES                     TO PAGE-NUMBER.
           MOVE "N"                        TO RECORDS-FOUND.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARAMETERS-BEGIN.

           MOVE SPACES                     TO EXT-EOBUF-ERRCD.
           MOVE ZEROES                     TO END-BRANCH.

           IF VDUSTATUS = "1U"
              GO TO EPS-EXT-CLASSES.

      *=================================================================
       EPS-BEG-BRANCH.

           MOVE LEGEND-BEG-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "R  A000BEG-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF     (    F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF     (    F6-KEY ) PERFORM SCAN-GR-FILE.
           IF     (    F7-KEY ) GO TO ENTER-PARAMETERS-QUIT.
           IF NOT ( ENTER-KEY ) GO TO EPS-BEG-BRANCH.

           MOVE VDUBUF                     TO WGR-WORD ENT-GROUP.

           IF ( WGR-WORD-ZERO )
              MOVE ZEROES                  TO WGR-SRESULT
              GO TO EPS-BRANCH-IS-NUMERIC.

           MOVE ZEROES                     TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF ( WGR-ERR > ZEROES )
              MOVE WGR-MSG(WGR-ERR)        TO MESS
              PERFORM SEND-MESS
              GO TO EPS-BEG-BRANCH.

           IF ( WGR-TYPE = "A" )
              MOVE "Y"                     TO GROUP-FLAG
              MOVE ZEROES                  TO BEG-BRANCH
              MOVE 9999                    TO END-BRANCH
              GO TO EPS-CHECK-BRANCH-SECURITY.

      *-----------------------------------------------------------------
       EPS-BRANCH-IS-NUMERIC.

           MOVE "N"                        TO GROUP-FLAG.
           MOVE WGR-SRESULT                TO BEG-BRANCH.
           MOVE "SN N040BEG-BRANCH."       TO VDU.
           MOVE BEG-BRANCH                 TO VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       EPS-END-BRANCH.

           MOVE BEG-BRANCH                 TO END-BRANCH.

      *-----------------------------------------------------------------
       EPS-CHECK-BRANCH-SECURITY.

           PERFORM BR-RANGE-SECURITY.
           IF ( BR-SECURITY-OKAY = "N" )
              GO TO EPS-BEG-BRANCH.

      *-------------------------- INCLUDE ALL CLASSES ON REPORT ? 
       EPS-REP-CLASSES.
           MOVE LEGEND-REST TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010REP-CLASS." TO VDU.
           PERFORM SCREENIO.
           IF     (    F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF     (    F7-KEY ) GO TO ENTER-PARAMETERS-QUIT.
           IF VDUSTATUS = "1U"  GO TO EPS-BEG-BRANCH.
           IF NOT ( ENTER-KEY ) GO TO EPS-REP-CLASSES.

           MOVE VDUBUF TO REPORT-ALL-CLASSES.

           IF REPORT-ALL-CLASSES NOT = "Y" AND NOT = "N"
              GO TO EPS-REP-CLASSES.

      *------------------------------------------ EXTRACT ?
       EPS-EXTRACT.
           MOVE "E  A010EXTRACT." TO VDU.
           PERFORM SCREENIO.
           IF     (    F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF     (    F7-KEY ) GO TO ENTER-PARAMETERS-QUIT.
           IF VDUSTATUS = "1U"  GO TO EPS-REP-CLASSES.

           IF NOT ( ENTER-KEY ) GO TO EPS-EXTRACT.

           MOVE VDUBUF TO EXTRACT-FLAG.

           IF EXTRACT-FLAG NOT = "Y" AND NOT = "N"
              GO TO EPS-EXTRACT.

           IF EXTRACT-FLAG = "N"
              MOVE "S  A010EXT-CLASS." TO VDU
              MOVE "N" TO EXTRACT-ALL-CLASSES VDUBUF
              PERFORM SCREENIO
              GO TO ENTER-PARAMETERS-END.

      *-------------------------- INCLUDE ALL CLASSES IN EXTRACT?
       EPS-EXT-CLASSES.
           MOVE "E  A010EXT-CLASS." TO VDU.
           PERFORM SCREENIO.
           IF     (    F1-KEY ) GO TO ENTER-PARAMETERS-BEGIN.
           IF     (    F7-KEY ) GO TO ENTER-PARAMETERS-QUIT.
           IF VDUSTATUS = "1U"  GO TO EPS-EXTRACT.
           IF NOT ( ENTER-KEY ) GO TO EPS-EXT-CLASSES.

           MOVE VDUBUF TO EXTRACT-ALL-CLASSES.

           IF EXTRACT-ALL-CLASSES NOT = "Y" AND NOT = "N"
              GO TO EPS-EXT-CLASSES.

           GO TO ENTER-PARAMETERS-END.

      *=================================================================
       ENTER-PARAMETERS-QUIT.

           MOVE "X"                        TO EXT-EOBUF-ERRCD.

       ENTER-PARAMETERS-END.

           MOVE SPACES                     TO LEGEND.
           PERFORM SEND-LEGEND.

       ENTER-PARAMETERS-EXIT.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************

       EO-EXEC-AUDIT-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************

           IF ( GROUP-FLAG = "Y" )
              MOVE "S  A000BEG-BRANCH."    TO VDU
              MOVE ENT-GROUP               TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BEG-BRANCH."    TO VDU
              MOVE BEG-BRANCH              TO VDUNUM0
              PERFORM SCREENIO.

           MOVE "S  A010REP-CLASS."    TO VDU.
           MOVE REPORT-ALL-CLASSES     TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010EXTRACT."    TO VDU.
           MOVE EXTRACT-FLAG         TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010EXT-CLASS."    TO VDU.
           MOVE EXTRACT-ALL-CLASSES    TO VDUBUF.
           PERFORM SCREENIO.

       DISPLAY-PARAMETERS-EXIT.
           EXIT.

      ******************************************************************
       SCAN-GR-FILE SECTION.
      ******************************************************************

           MOVE "WI/GRSCAN" TO FORM-NAM.
           CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME.
           CANCEL FORM-PROGX.
           IF GRSCAN-STATUS NOT = "00"
              MOVE STAT---BAD              TO VDUSTATUS
           ELSE
              MOVE "S  A000BEG-BRANCH."    TO VDU
              MOVE GRSCAN-KEY              TO VDUBUF GR1-KEY
              PERFORM SCREENIO
              MOVE VDU---ENTER-KEY         TO VDUSTATUS.

       SCAN-GR-FILE-EXIT.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************

           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF ( LN-WK > EXT-CONAME-RPT-LINES )
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE                TO PR-REC.
           PERFORM WRITE-PR-REC.

           ADD  LN-FEED                    TO LN-COUNT.
           MOVE 01                         TO LN-FEED.
           MOVE SPACES                     TO DETAIL-LINE.
 
       WRITE-DETAIL-LINE-EXIT.
           EXIT.

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************

           MOVE EXT-CONAME-SYSDATE   TO RPW-H01-DATE.
           MOVE EXT-CONAME-CONAME    TO RPW-H01-NAME.
           CALL "C$JUSTIFY" USING RPW-H01-NAME "C".

           ADD  01                         TO PAGE-NUMBER.
           MOVE PAGE-NUMBER                TO RPW-H01-PAGE-NO.

           PERFORM GET-TIME.
           MOVE TIME-EDIT                  TO RPW-H01-TIME.

           MOVE RPW-HEADER-01              TO PR-REC.
           MOVE 99                         TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE EXT-CONAME-USER-ID         TO RPW-H02-USERID.
           CALL "C$JUSTIFY" USING RPW-H02-TITLE "C".
           MOVE RPW-HEADER-02              TO PR-REC.
           MOVE 01                         TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE RPW-HEADER-03              TO PR-REC.
           MOVE 01                         TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE 03                         TO LN-COUNT.

       HEAD-PAGE-EXIT.
           EXIT.

      ***********************************************
       BUILD-EXTRACTION-RECORD SECTION.
      ***********************************************

      * EITHER ALL CLASSES OR VERIFY HARD CODED CLASSES
           IF EXTRACT-ALL-CLASSES NOT = "Y"
              MOVE GI-CLASS               TO VERIFY-GI-CLASS
              IF NOT ( VERIFY-GI-CLASS-GOOD )
                 GO TO EXIT-BUILD-EXTRACTION-RECORD.

           MOVE SPACES TO GIAPPR-REC.

           MOVE GI-BRANCH            TO GIA-BRANCH.
           MOVE GI-CLASS             TO GIA-CLASS.
           MOVE GI-LN-APPROVAL-LIMIT TO GIA-LIMIT.

           MOVE X"09" TO GIA-TAB1 GIA-TAB2.

           MOVE X"0D" TO GIA-CR.

           PERFORM WRITE-GIAPPR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           ADD 1 TO TOTAL-EXTRACTS.
           MOVE "Y" TO RECORDS-FOUND.

       EXIT-BUILD-EXTRACTION-RECORD.
           EXIT.

      ******************************************************************
       RANGE-LINES SECTION.
      ******************************************************************

           MOVE ALL "="                    TO DETAIL-LINE.           
           MOVE 01                         TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( GROUP-FLAG = "Y" )
              MOVE "GROUP NAME         : " TO RPW-R01-PROMPT
              MOVE ENT-GROUP               TO RPW-R01-TEXT
              MOVE 01                      TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEGINNING BRANCH   : " TO RPW-R01-PROMPT
              MOVE BEG-BRANCH              TO RPW-R01-NUMBER
              MOVE 01                      TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

           MOVE "ALL CLASSES ON REPORT" TO RPW-R01-PROMPT.
           MOVE REPORT-ALL-CLASSES      TO RPW-R01-TEXT.
           MOVE 01                      TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "EXTRACT FLAG"          TO RPW-R01-PROMPT.
           MOVE EXTRACT-FLAG            TO RPW-R01-TEXT.
           MOVE 01                      TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ALL CLASSES IN EXTRACT" TO RPW-R01-PROMPT.
           MOVE EXTRACT-ALL-CLASSES      TO RPW-R01-TEXT.
           MOVE 01                       TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL RECORDS EXTRACTED" TO RPW-R01-PROMPT.
           MOVE TOTAL-EXTRACTS            TO RPW-R01-NUMBER.
           MOVE 03                        TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 02                        TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       RANGE-LINES-EXIT.
           EXIT.

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      *************************************************
       FORM-RESET SECTION.
      *************************************************
           MOVE EXT-CONAME-CONAME TO GIAPPR-CONAME.
           MOVE EXT-CONAME-SYSDATE TO GIAPPR-SYSDATE.
           DISPLAY GIAPPR-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE GIAPPR-FORM-FIELDS TO WR-BUF.
           MOVE GIAPPR-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *************************************************
       SETUP-LINK-INFO SECTION.
      *************************************************
           MOVE GIAPPR-HELP--FILE TO HELP-LINK-FILE.
           MOVE GIAPPR-HELP--DIR TO HELP-LINK-DIR.

      *************************************************
       VDU-CONVERT-FIELD SECTION.
      *************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLS/GIAPPR_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGL/GLGIGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

      * COPY "LIBGL/GLGI1RN.CPY".
      * COPYMEMBER: LIBGL/GLGI1RN
       LOAD-GI1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( GI-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( GI-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO GI-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO GI-ALL-MACHINE
              MOVE GI-ALL-PATH             TO GI-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( GI-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO GI-ALL-BASE
              MOVE FILPATH-MACHINE         TO GI-ALL-MACHINE
              MOVE GI-ALL-PATH             TO GI-PATH.

      *-----------------------------------------------------------------
       OPEN-GI1-FILE.
           PERFORM LOAD-GI1-FILE.
           PERFORM OPEN-IT.
           MOVE GI-PATH-SQL TO E-FILE.

           IF GI1-CONN-OPEN-SW = "N" 
              MOVE "Y"         TO GI1-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS GI1_CONNECT 
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-GI1-FILE. *> SSP - GIFILE
           PERFORM START-IT.
           MOVE GI-PATH-SQL TO E-FILE.
           MOVE GI1-KEY TO E-KEYX.

           IF ( GI1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-GI1-FILE.

           IF ( QGI1-WEND-CLASS = ZEROES    ) OR
              ( QGI1-WEND-CLASS NOT NUMERIC )
              MOVE GI-BRANCH            TO QGI1-WBEG-BRANCH
              MOVE ALL "9"              TO QGI1-WEND-BRANCH
              MOVE GI-CLASS             TO QGI1-WBEG-CLASS
              MOVE ALL "9"              TO QGI1-WEND-CLASS.

      *  STORED PROCEDURE:
      *
      *   SELECT GIFILE.GI_BRANCH,
      *          GIFILE.GI_CLASS,
      *          GIFILE.GI_LN_APPROVAL_LIMIT
      *     FROM DBO.GIFILE
      *    WHERE GIFILE.GI_BRANCH       = @QGI1_WBEG_BRANCH
      *      AND GIFILE.GI_BRANCH BETWEEN @QGI1_WBEG_CLASS
      *                               AND @QGI1_WEND_CLASS
      *    ORDER BY GIFILE.GI_BRANCH,
      *             GIFILE.GI_CLASS

           EXEC SQL SET CONNECTION GI1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE GI1_CURSOR CURSOR FOR
                :GI1-RETURN-CODE = EXEC SSP_GIAPPR_GIFILE_SELECT 
              ( :QGI1-WBEG-BRANCH, 
                :QGI1-WBEG-CLASS,  
                :QGI1-WEND-CLASS )
           END-EXEC.

           EXEC SQL
               OPEN GI1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QGI1-WBEG-BRANCH
                          QGI1-WBEG-CLASS
                          QGI1-WEND-BRANCH
                          QGI1-WEND-CLASS.

           MOVE STAT---GOOD TO GI1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC. 

      *-----------------------------------------------------------------
       READ-GI1-FILE-NEXT. *> EMBEDDED - GIFILE
           PERFORM READ-IT.
           MOVE GI-PATH-SQL TO E-FILE.
           MOVE GI1-KEY TO E-KEYX.
           INITIALIZE QGI-REC.

           EXEC SQL SET CONNECTION GI1_CONNECT END-EXEC.

           IF ( GI1-CURSOR-STAT-GOOD )
              EXEC SQL
                FETCH GI1_CURSOR 
                 INTO 
                     :QGI-BRANCH,
                     :QGI-CLASS,
                     :QGI-LN-APPROVAL-LIMIT
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GI1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-GI-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC. 

      *-----------------------------------------------------------------
       CLOSE-GI1-FILE.
           PERFORM CLOSE-IT.
           IF ( GI1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION GI1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE GI1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO GI1-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-GI1-CONNECTION.

           IF GI1-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION GI1_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT GI1_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO GI1-CONN-OPEN-SW.

      *-----------------------------------------------------------------

       OPEN-GIAPPR-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE "GIAPPR" TO E-FILE.
           OPEN OUTPUT GIAPPR-FILE.
           IF IO-FG = 8
              GO TO OPEN-GIAPPR-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE GIAPPR-FILE
              GO TO OPEN-GIAPPR-FILE-OUTPUT.
       WRITE-GIAPPR-FILE.
           PERFORM WRITE-IT.
           MOVE "GIAPPR" TO E-FILE.
           MOVE "GIAPPR" TO E-KEYX.
           WRITE GIAPPR-REC.
           IF IO-FG = 8
              GO TO WRITE-GIAPPR-FILE.
       CLOSE-GIAPPR-FILE.
           PERFORM CLOSE-IT.
           CLOSE GIAPPR-FILE.

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.

           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF ( NO-RECORDS-FOUND )
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE!" TO MESS
              PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE "LP/LPL2MU" TO FORM-NAM
              MOVE FORM-PATH   TO FORM-PATHNAME.

       STOP-RUN.
            IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
                DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
                DESTROY GIAPPR-SCREEN
                DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
