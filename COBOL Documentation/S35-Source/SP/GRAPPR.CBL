       IDENTIFICATION DIVISION.
       PROGRAM-ID.    GRAPPR.
       AUTHOR.        JKC.
       DATE-WRITTEN.  10-27-2021.
      ******************************************************************
      *
      * IDENTIFICATION:  SP/GRAPPR.C
      *
      * DESCRIPTION   :  GROUP APPROVAL LIMIT REPORT/EXTRACTION
      *
      * OUTPUT FILE   :  /USR/DATA/R1/FTP/GRAPPR.TXT
      *                   ... .... .  ...        ...  = LOWERCASE
      *
      * REV:
      * KEC 2021.1028 PR#1522 ***NEW***
      * KEC 2021-1102 PR#1522 FIXED TITLE FOR "LIMITE" TO BE "LIMIT"
      * KEC 2021-1108 PR#1522 FIXED LEVEL-NUMBER IN DISPLAY-PARAMETERS SECTION
      *          SO THAT IT WORKS IN BATCH.
      *          FIXED LEVEL-NUMBER USING F3-ALL IN DISPLAY-PARAMETERS
      *          END ENTER-PARAMETERS SECTIONS.
      *          FIXED TO NOT DISPLAY EXTRACTION FILE PATH TO SCREEN
      *          IF IT IS A BATCH EXECUTION.
      * BAH 2024.0621 CHANGE INPUT MESSAGE LEGEND-LIMIT-GR-ZERO-YN TO NOT BE
      *               INCLUDE,SKIP,ONLY, IT IS YES OR NO. #1661 S35Q-72
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

           SELECT GRAPPR-FILE ASSIGN OUTPUT GRAPPR-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.

      ******************************************************************
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

       FD  GRAPPR-FILE
           RECORD IS VARYING IN SIZE FROM 10 TO 1250 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  GRAPPR-REC.
           03  GRAPPR-BYTE             PIC X(01)    OCCURS 1250 TIMES.

      ******************************************************************
      ******************************************************************
      ******************************************************************
      *    W O R K I N G - S T O R A G E - S E C T I O N
      ******************************************************************
      ******************************************************************
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/GRAPPR.CBL S34 11/09/21-13:48:52 >".

      ******************************************************************
      *
      *    F I L E - R E C O R D - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".

      * /USR/DATA/R1/FTP/GRAPPR.TXT
      *  ... .... .  ...        ...  = LOWERCASE
       01  GRAPPR-PATH                 PIC X(40)     VALUE SPACES.
       01  FILLER                      REDEFINES GRAPPR-PATH.
           03  FILLER                  PIC X(01).   
           03  GRAPPR-PATH-FIL         PIC X(39). 
       01  FILLER                      REDEFINES GRAPPR-PATH.
           03  GRAPPR-PATH-SLASH-1     PIC X(01).    
           03  GRAPPR-PATH-DIR-1       PIC X(03).    
           03  GRAPPR-PATH-SLASH-2     PIC X(01).    
           03  GRAPPR-PATH-DIR-2       PIC X(04).    
           03  GRAPPR-PATH-SLASH-3     PIC X(01).    
           03  GRAPPR-PATH-DIR-3       PIC X(02).    
           03  GRAPPR-PATH-SLASH-4     PIC X(01).    
           03  GRAPPR-PATH-DIR-4       PIC X(03).    
           03  GRAPPR-PATH-SLASH-5     PIC X(01).    
           03  GRAPPR-PATH-FILENAME    PIC X(22).    

       01  GRAPPR-DIR-FTP              PIC X(03)     VALUE "FTP".
       01  GRAPPR-EXT-TXT              PIC X(04)     VALUE ".TXT".

       01  EDF-WORKERS.
           03  EDF-GR-LN-APPROVAL-LIMIT    PIC 9(10).

       01  HEX-CARRIAGE-RETURN         PIC X(01)     VALUE X"0D".
       01  HEX-TAB                     PIC X(01)     VALUE X"09".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  EXITPROG                    PIC X(09)     VALUE "LP/LPL2MU".

       01  LN-FEED                 PIC 9(02)     VALUE 1.
       01  LN-COUNT                PIC 9(03)     VALUE 61.

       01  PAGE-NUMBER                 PIC 9(06)     VALUE ZEROES.

      ******************************************************************
      *
      *    L E G E N D - W O R K E R S
      *
      ******************************************************************
       01  LEGEND                      PIC X(80)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(09)     VALUE "LEGEND:B.".

       01  LEGEND-LEVEL-NUMBER.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F7-EXIT, ENT".
           03  FILLER PIC X(30) VALUE "ER GROUP LEVEL                ".
           03  FILLER PIC X(20) VALUE "                    ".

      *-----------------------------------------------------------------
       01  LEGEND-YN.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER (Y)ES".
           03  FILLER PIC X(30) VALUE " OR (N)O FOR REPORT           ".
           03  FILLER PIC X(20) VALUE "                    ".

      *-----------------------------------------------------------------
       01  LEGEND-ACCEPT-YN.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER (Y)ES".
           03  FILLER PIC X(30) VALUE " OR (N)O TO ACCEPT SETTINGS   ".
           03  FILLER PIC X(20) VALUE "                    ".

      ******************************************************************
      *
      *    R A N G E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  LEVEL-NUMBER            PIC 9(02).
               88  LEVEL-NUMBER-ALL        VALUE 99.
           03  LIMIT-GR-ZERO-YN        PIC X(01).
           03  REPORT-ONLY-YN          PIC X(01).

       01  VERSION-CONTROL             PIC X(01)     VALUE "A".

       01  WSS-LEVEL-NUMBER            PIC 9(01)     VALUE ZEROES.
       01  WSS-LEVEL-NUMBERX           REDEFINES WSS-LEVEL-NUMBER
                                       PIC X(01).
      ******************************************************************
      *
      *    R E P O R T - W O R K E R S
      *
      ******************************************************************
       01  HEADER-LINE-01.
           03  H01-DATE                PIC X(08)     VALUE SPACES.
           03  FILLER                  PIC X(02)     VALUE SPACES.
           03  H01-HR                  PIC 9(02)     VALUE ZEROES.
           03  FILLER                  PIC X(01)     VALUE ":".
           03  H01-MIN                 PIC 9(02)     VALUE ZEROES.
           03  FILLER                  PIC X(31)     VALUE SPACES.
           03  H01-NAME                PIC X(40)     VALUE SPACES.
           03  FILLER                  PIC X(37)     VALUE SPACES.
           03  FILLER                  PIC X(05)     VALUE "PAGE ".
           03  H01-PAGE-NO             PIC ZZZ9.
 
       01  HEADER-LINE-02.
           03  FILLER                  PIC X(10)     VALUE "USER ID:".
           03  H02-USERID              PIC X(10)     VALUE SPACES.
           03  H02-TITLE               PIC X(92)     VALUE
                              "GROUP APPROVAL LIMIT EXTRACTION REPORT".
           03  FILLER                  PIC X(20)     VALUE SPACES.
 
       01  HEADER-LINE-03.
           03  FILLER PIC X(30) VALUE "GROUP NAME                 LEV".
           03  FILLER PIC X(30) VALUE "EL CONTACT NAME         APP LI".
           03  FILLER PIC X(30) VALUE "MIT                           ".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(12) VALUE SPACES.

       01  DETAIL-LINE                 PIC X(132)    VALUE SPACES.
 
       01  DETAIL-LINE-01              REDEFINES DETAIL-LINE.
           03  D01-GR-ID                PIC X(04).
           03  FILLER                   PIC X(02).
           03  D01-GR-NAME              PIC X(20).
           03  FILLER                   PIC X(03).
           03  D01-GR-LEVEL             PIC 9(01).
           03  FILLER                   PIC X(03).
           03  D01-GR-CONTACT           PIC X(20).
           03  FILLER                   PIC X(01).
           03  D01-GR-LN-APPROVAL-LIMIT PIC Z,ZZZ,ZZ9.

       01  RANGE-LINE-01               REDEFINES DETAIL-LINE.
           03  R01-DESC                PIC X(22).
           03  R01-TEXT                PIC X(110).
           03  R01-NUMBER-01           REDEFINES R01-TEXT
                                       PIC 9(01).

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/GRAPPR_DEF.CPY".
       COPY "LIBSP/GRAPPR_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/GRAPPR_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
           USING FORM-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE
                 GRAPPR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-GR1-FILE.
           CLOSE PR-FILE
                 GRAPPR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
      D    STOP FORM-PATHNAME.
           PERFORM SQL-CONNECT.

           PERFORM INITIALIZATION.

           IF ( EXT-EOBUF-ERRCD NOT = SPACES )
              GO TO END-PROGRAM.

           MOVE SPACES TO GR-ID.
           PERFORM START-GR1-FILE.

       MAIN-PROGRAM-NEXT-GR.

           PERFORM READ-GR1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO MAIN-PROGRAM-END.

           IF NOT ( LEVEL-NUMBER-ALL ) 
              IF ( GR-LEVEL NOT = LEVEL-NUMBER )
                 GO TO MAIN-PROGRAM-NEXT-GR.

           IF ( LIMIT-GR-ZERO-YN = "Y" )
              IF ( GR-LN-APPROVAL-LIMIT = ZEROES )
                 GO TO MAIN-PROGRAM-NEXT-GR.

      * VALID RECORD 

           PERFORM EXTRACT-DETAIL-ROUTINE.
           PERFORM REPORT-DETAIL-ROUTINE.

           GO TO MAIN-PROGRAM-NEXT-GR.

       MAIN-PROGRAM-END.

           PERFORM REPORT-RANGES.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
       EXTRACT-DETAIL-ROUTINE SECTION.
      ******************************************************************

           IF ( REPORT-ONLY-YN = "Y" )
              GO TO EXTRACT-DETAIL-R-EXIT.

           MOVE GR-LN-APPROVAL-LIMIT TO EDF-GR-LN-APPROVAL-LIMIT.

           MOVE SPACES                     TO GRAPPR-REC.

           STRING GR-ID,                      HEX-TAB,
                  GR-NAME,                    HEX-TAB,
                  GR-LEVEL,                   HEX-TAB,
                  GR-CONTACT,                 HEX-TAB,
                  EDF-GR-LN-APPROVAL-LIMIT,   HEX-CARRIAGE-RETURN
                  DELIMITED BY SIZE      INTO GRAPPR-REC.

           PERFORM WRITE-GRAPPR-FILE.

           IF ( IO-BAD )
              GO TO IO-ERROR.

       EXTRACT-DETAIL-R-EXIT.
           EXIT.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

           MOVE FORM-PATHNAME              TO FORM-PATH.
           MOVE "GRAPPR"                   TO FORM-PROG VDU-FORM-NAME.
           MOVE GRAPPR-QUEUE-POS           TO Q-POS.
           MOVE GRAPPR-COPIES-POS          TO C-POS.
           MOVE GRAPPR-PRU-POS             TO P-POS.
           MOVE SPACES                     TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".
       INITIALIZATION-POST-EOINIT.

           MOVE EXT-CONAME-SYSDATE         TO H01-DATE.
           MOVE EXT-CONAME-CONAME          TO H01-NAME.
           MOVE EXT-CONAME-USER-ID         TO H02-USERID.

      *A15 CALL WORKING.

           IF ( EXT-ROUTE-BUF = "70" )
              MOVE EXT-EOBUF-MESS          TO H02-TITLE.

           CALL "C$JUSTIFY"             USING H02-TITLE "C".

      * SET GRAPPR-PATH BY GETTING CURRENT USERS MACHINE VIA FIL
      
           CALL "C$TOLOWER"             USING GRAPPR-DIR-FTP, VALUE 3.
           CALL "C$TOLOWER"             USING GRAPPR-EXT-TXT, VALUE 4.

           MOVE "FIL"                      TO GETENV-BUF.
           PERFORM GETENV-CALL.

           IF ( STAT-BAD )
              GO TO IO-ERROR.

           MOVE "/"                        TO GRAPPR-PATH-SLASH-1.
           MOVE GETENV-BUF                 TO GRAPPR-PATH-FIL.
           MOVE GRAPPR-DIR-FTP             TO GRAPPR-PATH-DIR-4.
           MOVE SPACES                     TO GRAPPR-PATH-SLASH-5.
           MOVE SPACES                     TO GRAPPR-PATH-FILENAME.
           MOVE GRAPPR-PATH                TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.

           IF ( STAT-BAD )
              MOVE SPACES                  TO MESS
              STRING GRAPPR-PATH, " DOES NOT EXIST"
                     DELIMITED BY "  "   INTO MESS
              PERFORM SEND-MESS
              GO TO INITIALIZATION-END.

           MOVE "/"                        TO GRAPPR-PATH-SLASH-5.
           MOVE SPACES                     TO GRAPPR-PATH-FILENAME.

           STRING "GRAPPR", GRAPPR-EXT-TXT
                  DELIMITED BY "  "      INTO GRAPPR-PATH-FILENAME.

           IF ( REPORT-ONLY-YN = "N" )
              PERFORM OPEN-GRAPPR-FILE-OUTPUT
              IF ( EXT-ROUTE-BUF = "00" )
                 MOVE SPACES TO MESS
                 STRING "EXTRACTION FILE: ", GRAPPR-PATH
                        DELIMITED BY "  " INTO MESS
                 CALL "C$JUSTIFY"             USING MESS "C"
                 MOVE "S  A800EXTRACT-LOC." TO VDU
                 MOVE MESS                  TO VDUBUF
                 PERFORM SCREENIO.

           MOVE "REPORT/EXTRACTION IN PROGRESS....." TO LEGEND.
           IF ( EXT-EOBUF-ERRCD NOT = "X" )
              PERFORM SEND-LEGEND.

           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-PR-FILE.

           GO TO INITIALIZATION-EXIT.

       INITIALIZATION-END.

           MOVE "X" TO EXT-EOBUF-ERRCD.

       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************

           IF ( UP-KEY )
              GO TO ENTER-REPORT-ONLY-YN.

       ENTER-LEVEL-NUMBER.

           MOVE LEGEND-LEVEL-NUMBER        TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010LEV-NUM."          TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-LEVEL-NUMBER.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-LEVEL-NUMBER.

           MOVE VDUBUF TO WSS-LEVEL-NUMBERX.

           IF ( WSS-LEVEL-NUMBERX = "*" )
              GO TO ENTER-ALL-LEVEL-NUMBER.

           IF ( WSS-LEVEL-NUMBER = ZEROES ) OR
              ( WSS-LEVEL-NUMBER > 8      )
              GO TO ENTER-LEVEL-NUMBER.

           MOVE WSS-LEVEL-NUMBER TO LEVEL-NUMBER.

           GO TO ENTER-LIMIT-GR-ZERO-YN.

      *-----------------------------------------------------------------
       ENTER-ALL-LEVEL-NUMBER.

           MOVE "S  A010LEV-NUM."          TO VDU.
           MOVE "*"                        TO VDUBUF.
           PERFORM SCREENIO.

           MOVE 99 TO LEVEL-NUMBER.

       ENTER-LIMIT-GR-ZERO-YN.

           MOVE LEGEND-YN    TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010APP-LIM-YN."       TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-LEVEL-NUMBER.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-LIMIT-GR-ZERO-YN.

           IF ( VDUBUF = "Y" OR "N" )
              MOVE VDUBUF TO LIMIT-GR-ZERO-YN
           ELSE
              MOVE "ERROR: (Y)ES OR (N)O" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-LIMIT-GR-ZERO-YN.

       ENTER-REPORT-ONLY-YN.

           MOVE LEGEND-YN    TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010REPORT-YN."        TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-LIMIT-GR-ZERO-YN.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-REPORT-ONLY-YN.

           IF ( VDUBUF = "Y" OR "N" )
              MOVE VDUBUF TO REPORT-ONLY-YN
           ELSE
              MOVE "ERROR: (Y)ES OR (N)O" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-REPORT-ONLY-YN.

       ENTER-PARAMETERS-END.

           MOVE LEGEND-ACCEPT-YN TO LEGEND.
           PERFORM SEND-LEGEND.

       ENTER-PARAMETERS-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************

           IF ( LEVEL-NUMBER-ALL )
              MOVE "S  A010LEV-NUM."       TO VDU
              MOVE "*"                     TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN010LEV-NUM."       TO VDU
              MOVE LEVEL-NUMBER            TO VDUNUM0
              PERFORM SCREENIO.

           MOVE "S  A010APP-LIM-YN."       TO VDU.
           MOVE LIMIT-GR-ZERO-YN           TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010REPORT-YN."        TO VDU.
           MOVE REPORT-ONLY-YN             TO VDUBUF.
           PERFORM SCREENIO.

       DISPLAY-PARAMETERS-EXIT.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************

       EO-EXEC-AUDIT-EXIT.
           EXIT.

      ******************************************************************
       REPORT-DETAIL-ROUTINE SECTION.
      ******************************************************************

           MOVE GR-ID                      TO D01-GR-ID.
           MOVE GR-NAME                    TO D01-GR-NAME.
           MOVE GR-LEVEL                   TO D01-GR-LEVEL.
           MOVE GR-CONTACT                 TO D01-GR-CONTACT.
           MOVE GR-LN-APPROVAL-LIMIT       TO D01-GR-LN-APPROVAL-LIMIT.

           PERFORM WRITE-DETAIL-LINE.

       REPORT-DETAIL-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
       REPORT-HEADER SECTION.
      ******************************************************************

           ADD 01                          TO PAGE-NUMBER.
           MOVE PAGE-NUMBER                TO H01-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH                       TO H01-HR.
           MOVE T-MM                       TO H01-MIN.
           MOVE HEADER-LINE-01             TO PR-REC.
           MOVE 99                         TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEADER-LINE-02             TO PR-REC.
           MOVE 01                         TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEADER-LINE-03             TO PR-REC.
           MOVE 02                         TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE ALL "="                    TO PR-REC.
           MOVE 01                         TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE 6 TO LN-COUNT.
           MOVE 1 TO LN-FEED.

       REPORT-HEADER-EXIT.
           EXIT.

      ******************************************************************
       REPORT-RANGES SECTION.
      ******************************************************************

           MOVE 99 TO LN-COUNT.

           MOVE "GROUP LEVEL (*=ALL) : "   TO R01-DESC.
           IF ( LEVEL-NUMBER-ALL )
              MOVE "*"                     TO R01-TEXT
           ELSE
              MOVE LEVEL-NUMBER            TO R01-NUMBER-01.
           MOVE 01                         TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "APP LIMIT > ZERO    : "   TO R01-DESC.
           MOVE LIMIT-GR-ZERO-YN           TO R01-TEXT.
           MOVE 02                         TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "REPORT ONLY (Y/N)   : "   TO R01-DESC.
           MOVE REPORT-ONLY-YN             TO R01-TEXT.
           MOVE 02                         TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       REPORT-RANGES-EXIT.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************

           ADD LN-FEED                     TO LN-COUNT.

           IF ( LN-COUNT > EXT-CONAME-RPT-LINES )
              PERFORM REPORT-HEADER.

           MOVE DETAIL-LINE                TO PR-REC.
           PERFORM WRITE-PR-REC.

           MOVE 01                         TO LN-FEED.
           MOVE SPACES                     TO DETAIL-LINE.

       WRITE-DETAIL-LINE-EXIT.
           EXIT.

      ******************************************************************
       FORM-RESET SECTION.
      ******************************************************************

           MOVE EXT-CONAME-CONAME          TO GRAPPR-CONAME.
           MOVE EXT-CONAME-SYSDATE         TO GRAPPR-SYSDATE.
           DISPLAY GRAPPR-SCREEN         UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE GRAPPR-FORM-FIELDS         TO WR-BUF.
           MOVE GRAPPR-FORM-LIST           TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ******************************************************************
       SETUP-LINK-INFO SECTION.
      ******************************************************************

           MOVE GRAPPR-HELP--FILE          TO HELP-LINK-FILE.
           MOVE GRAPPR-HELP--DIR           TO HELP-LINK-DIR.

       SETUP-LINK-INFO-EXIT.
           EXIT.

      ******************************************************************
       VDU-CONVERT-FIELD SECTION.
      ******************************************************************

           MOVE "00"                       TO STAT.
           MOVE VDU-FIELD-TO-CONVERT       TO VDU-ALP-FIELD.

           EVALUATE TRUE
              COPY "LIBSP/GRAPPR_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       VDU-CONVERT-FIELD-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       SEND-LEGEND.
           MOVE LEGEND-LIST                TO WR-LIST.
           MOVE LEGEND                     TO WR-BUF.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".

       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

       OPEN-GRAPPR-FILE-OUTPUT.

           PERFORM OPEN-IT.
           MOVE GRAPPR-PATH TO E-FILE.
           OPEN OUTPUT GRAPPR-FILE.

           IF ( IO-FG = 8 )
              GO TO OPEN-GRAPPR-FILE-OUTPUT.

           IF ( IO-FG = 7 )
              CLOSE GRAPPR-FILE
              GO TO OPEN-GRAPPR-FILE-OUTPUT.

      *-----------------------------------------------------------------
       WRITE-GRAPPR-FILE.

           PERFORM WRITE-IT.
           MOVE GRAPPR-PATH TO E-FILE.
           WRITE GRAPPR-REC.

           IF ( IO-FG = 8 )
              GO TO WRITE-GRAPPR-FILE.

      *-----------------------------------------------------------------
       CLOSE-GRAPPR-FILE.

           PERFORM CLOSE-IT.
           CLOSE GRAPPR-FILE.

       COPY "LIBGB/FERRORS.CPY".

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.

           PERFORM CLOSE-FILES.

           IF ( EXT-ROUTE-BUF = "00" )
              PERFORM PRU-OUT.

       EXIT-PROG.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE EXITPROG                TO FORM-NAM
              MOVE FORM-PATH               TO FORM-PATHNAME.

       STOP-RUN.
           IF ( EXT-ROUTE-BUF = "00" OR "40" OR "50" )
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY GRAPPR-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
