       IDENTIFICATION DIVISION.
       PROGRAM-ID.    BMEXTR.
       DATE-WRITTEN.  04-17-2017.
      ******************************************************************
      *
      * IDENTIFICATION:  SP/BMEXTR
      * DESCRIPTION   :  BORROWER MEMO EXTRACTION DATA WAREHOUSE
      * OUTPUT FILE   :  /USR/EXTRACT/BORROWERMEMO
      * RECORD SIZE   :  88 BYTES (INCLUDING CR, LF)
      *
      *=================================================================
      * REV:
      *
      * KEC 2017.0417 {PR#05276} REGIONAL MANAGEMENT <A15> [D SINCLAIR]
      *         ***NEW***
      * BAH 170804    ACTIVATE 8 DIGIT DATES, PD0006
      * RMZ 2025.0319 PERFORMANCE TUNING - ADDED STORED PROCEDURE 
      *               (S35Q-182) 
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBEX/EXFSXBM.CPY".
       COPY "LIBGB/GBFSPR.CPY".

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBEX/EXFDXBM.CPY".
       COPY "LIBGB/GBFDPR.CPY".

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)SP/BMEXTR S35 03/24/25-09:06:19 >".

      ******************************************************************
      *
      *    F I L E - R E C O R D - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01BM.CPY".
       COPY "LIBLP/LP01BM_SQL.CPY".
       COPY "LIBLP/LPWSBM.CPY".

      *=================================================================
      *    S Q L - W O R K E R S
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.            
       01  QWS-PRIORITY-ISO            PIC X      VALUE SPACE. 
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  BM1-CONN-OPEN-SW            PIC X      VALUE "N".

      *=================================================================
       01  XBM-PATH.
           03  XBM-DIRECTORY           PIC X(13)  VALUE "/USR/EXTRACT/".
           03  XBM-FILENAME            PIC X(12)  VALUE "BORROWERMEMO".

       01  XBM-KEY                     PIC 9(09)  VALUE ZEROES.

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  COUNT-RECORDS-WORKED        PIC 9(09)     VALUE ZEROES.
       01  EXITPROG                    PIC X(09)     VALUE "SP/DWEXMU".
       01  STARTING-TIME               PIC X(08)     VALUE SPACES.
       01  TRUE-BM-DATE-YYYYMMDD       PIC 9(08)     VALUE ZEROES.

      ******************************************************************
      *
      *    L E G E N D - W O R K E R S
      *
      ******************************************************************
       01  LEGEND-BUF                  PIC X(80)     VALUE SPACES.
       01  FILLER                      REDEFINES LEGEND-BUF.
           03  LEGEND-BRNO-PROMPT      PIC X(07).
           03  LEGEND-BRNO             PIC 9(04).
           03  LEGEND-BRNO-PROCESSING  PIC X(69).

       01  LEGEND-LIST                 PIC X(09)     VALUE "LEGEND:B.".

      *=================================================================
       01  LEGEND-BEG-BRANCH.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F6-SCAN, F7-".
           03  FILLER PIC X(30) VALUE "EXIT, ENTER BEGINNING BRANCH  ".
           03  FILLER PIC X(20) VALUE "                    ".

       01  LEGEND-END-BRANCH.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER ENDIN".
           03  FILLER PIC X(30) VALUE "G BRANCH                      ".
           03  FILLER PIC X(20) VALUE "                    ".

      *-----------------------------------------------------------------
       01  LEGEND-BEG-DATE.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER BEGIN".
           03  FILLER PIC X(30) VALUE "NING WEEK DATE (FRIDAY)       ".
           03  FILLER PIC X(20) VALUE "                    ".

       01  LEGEND-END-DATE.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER ENDIN".
           03  FILLER PIC X(30) VALUE "G WEEK DATE (FRIDAY)          ".
           03  FILLER PIC X(20) VALUE "                    ".

      *-----------------------------------------------------------------
       01  LEGEND-BEG-SSNO.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F7-EXIT, ENT".
           03  FILLER PIC X(30) VALUE "ER SOCIAL SECURITY NUMBER     ".
           03  FILLER PIC X(20) VALUE "                    ".

       01  LEGEND-END-SSNO.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER SOCIA".
           03  FILLER PIC X(30) VALUE "L SECURITY NUMBER             ".
           03  FILLER PIC X(20) VALUE "                    ".

      *-----------------------------------------------------------------
       01  LEGEND-PRIORITY-ISO.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER (I)NC".
           03  FILLER PIC X(30) VALUE "LUDE, (S)KIP, OR (O)NLY FOR PR".
           03  FILLER PIC X(20) VALUE "IORITY MEMOS        ".

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
      *=================================================================
           03  BEG-BRANCH              PIC 9(04).
           03  END-BRANCH              PIC 9(04).
           03  BEG-SSNO                PIC 9(09).
           03  END-SSNO                PIC 9(09).
           03  BEG-DATE                PIC 9(08).
           03  END-DATE                PIC 9(08).
           03  PRIORITY-ISO            PIC X(01).
               88  PRIORITY-ISO-INCLUDE    VALUE "I".
               88  PRIORITY-ISO-SKIP       VALUE "S".
               88  PRIORITY-ISO-ONLY       VALUE "O".
           03  GROUP-FLAG              PIC X(01).
           03  ENT-GROUP               PIC X(04).

      *=================================================================
       01  VERSION-CONTROL             PIC X(01)     VALUE "A".

       01  SV-BEG-BRANCH               PIC 9(04)     VALUE 9999.
       01  SV-END-BRANCH               PIC 9(04)     VALUE 9999.
       01  SV-GROUP-FLAG               PIC X(01)     VALUE SPACES.
       01  SV-ENT-GROUP                PIC X(04)     VALUE SPACES.

      ******************************************************************
      *
      *     R E P O R T - W O R K E R S
      *
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED                 PIC 9(03)     VALUE ZEROES.
           03  LN-COUNT                PIC 9(03)     VALUE 99.
           03  LN-WK                   PIC 9(03)     VALUE ZEROES.

       01  PAGE-NUMBER                 PIC 9(04)     VALUE ZEROES.
 
      *=================================================================
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
           03  FILLER                  PIC X(10)     VALUE "USER ID:  ".
           03  H02-USERID              PIC X(10).
           03  FILLER                  PIC X(23)     VALUE SPACES.
           03  H02-TITLE               PIC X(28)     VALUE
                                       "LOAN MEMO FILE EXTRACTION".
           03  FILLER                  PIC X(10)     VALUE "     FOR: ".
           03  H02-AGEDATE             PIC X(10).
 
      *=================================================================
       01  DETAIL-LINE                 PIC X(132)    VALUE SPACES.

       01  RANGE-LINE-01               REDEFINES DETAIL-LINE.
           03  R01-PROMPT              PIC X(24).
           03  R01-TEXT                PIC X(108).
           03  R01-AMT                 REDEFINES R01-TEXT
                                       PIC ZZZZZ9.99.
           03  R01-DATE                REDEFINES R01-TEXT
                                       PIC 9999/99/99.
           03  R01-NUMBER              REDEFINES R01-TEXT
                                       PIC Z(8)9.
           03  R01-SSNO                REDEFINES R01-TEXT
                                       PIC 999/99/9999.

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
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBLP/LPWKPATH.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/BMEXTR_DEF.CPY".
       COPY "LIBSP/BMEXTR_WKS.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBSP/BMEXTR_SCN.CPY".

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
                 XBM-FILE PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-BM1-FILE.
           PERFORM CLOSE-BM1-CONNECTION.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE XBM-FILE.
       COPY "LIBGB/DECLRP2.CPY".
 
      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.
      D    STOP FORM-PATHNAME.

           PERFORM INITIALIZATION.

           IF ( EXT-EOBUF-ERRCD NOT = SPACE )
              GO TO MAIN-PROGRAM-EXIT.

           IF ( GROUP-FLAG = "N" )
              MOVE BEG-BRANCH              TO BR-NO
              PERFORM START-BR-FILE
              GO TO MAIN-PROGRAM-NEXT-BRNO.

           MOVE ENT-GROUP                  TO GR-ID.
           PERFORM SEARCH-GR-INIT.

       MAIN-PROGRAM-NEXT-GROUP.

           IF ( GROUP-FLAG NOT = "Y" )
              GO TO MAIN-PROGRAM-NEXT-BRNO.

           PERFORM SEARCH-GR-NEXT.
           IF ( WGR-SRESULT = ZEROES )
              GO TO MAIN-PROGRAM-END.

           MOVE WGR-SRESULT                TO BR-NO.
           PERFORM READ-BR-FILE.
           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO MAIN-PROGRAM-LOAD-PATHS.

       MAIN-PROGRAM-NEXT-BRNO.

           PERFORM READ-BR-FILE-NEXT.
           IF ( IO-BAD ) OR ( BR-NO > END-BRANCH )
              GO TO MAIN-PROGRAM-END.

       MAIN-PROGRAM-LOAD-PATHS.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BM-PATH-OVERRIDE.

           IF ( PRU-LP    NOT = "VDU" ) AND
              ( PRU-LP    NOT = "PRU" ) AND
              ( EXT-ROUTE-BUF = "00" )
              MOVE "BRANCH: "              TO LEGEND-BRNO-PROMPT
              MOVE BR-NO                   TO LEGEND-BRNO
              MOVE "BUILDING EXTRACTION FILE..........................."
                                           TO LEGEND-BRNO-PROCESSING
              PERFORM SEND-LEGEND.

           PERFORM OPEN-BM1-FILE.
 
           MOVE BM-PATH-OWNBR TO BM-BRNO
                                 QBM1-WBEG-BRNO
                                 QBM1-WEND-BRNO.
           MOVE BEG-SSNO      TO BM-SSNO
                                 QBM1-WBEG-SSNO.
           MOVE END-SSNO      TO QBM1-WEND-SSNO.
           MOVE 0             TO BM-DATE BM-SEQ
                                 QBM1-WBEG-DATE-REV
                                 QBM1-WBEG-SEQ.
           MOVE ALL "9"       TO QBM1-WEND-DATE-REV
                                 QBM1-WEND-SEQ.
           MOVE PRIORITY-ISO  TO QWS-PRIORITY-ISO.

           PERFORM START-BM1-FILE. *> SSP - BMFILE

       MAIN-PROGRAM-NEXT-BM.

           PERFORM READ-BM1-FILE-NEXT. *> EMBEDDED - BMFILE
           IF (IO-BAD) OR (BM-PATH-OWNBR NOT = BM-BRNO) OR
                           (BM-SSNO > END-SSNO)
              GO TO MAIN-PROGRAM-END-GROUP.

           SUBTRACT BM-DATE FROM 99999999 GIVING TRUE-BM-DATE-YYYYMMDD.

      *- - - - - - - - - - - - - - - - - - - - - - - -{ PHONE CONTACTS }
      *    PHONE CONTACTS - RECORDS NOT TRUE MEMO RECORDS.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( TRUE-BM-DATE-YYYYMMDD = ZEROES )
              GO TO MAIN-PROGRAM-NEXT-BM.

      *- - - - - - - - - - - - - - - - - - - - - - - -{ PRIORITY MEMOS }
           IF ( PRIORITY-ISO-SKIP                ) AND
              ( TRUE-BM-DATE-YYYYMMDD = 99999999 )
              GO TO MAIN-PROGRAM-NEXT-BM.

           IF ( PRIORITY-ISO-ONLY )
              IF ( TRUE-BM-DATE-YYYYMMDD NOT = 99999999 )
                 GO TO MAIN-PROGRAM-NEXT-BM
              ELSE
                 GO TO MAIN-PROGRAM-EXTRACT-REC.

           IF ( PRIORITY-ISO-INCLUDE             ) AND
              ( TRUE-BM-DATE-YYYYMMDD = 99999999 )
              GO TO MAIN-PROGRAM-EXTRACT-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - -{ STANDARD MEMOS }
           IF ( TRUE-BM-DATE-YYYYMMDD < BEG-DATE ) OR
                ( TRUE-BM-DATE-YYYYMMDD > END-DATE )
              GO TO MAIN-PROGRAM-NEXT-BM.

      *-----------------------------------------------------------------
      *    RECORD VALID FOR PROCESSING; CREATE EXTRACTION RECORD.
      *-----------------------------------------------------------------
       MAIN-PROGRAM-EXTRACT-REC.

           MOVE SPACES                     TO XBM-REC.
           MOVE BR-NO                      TO XBM-BRNO.
           MOVE BM-SSNO                    TO XBM-SSNO.
           MOVE TRUE-BM-DATE-YYYYMMDD      TO XBM-DATE.
           MOVE BM-SEQ                     TO XBM-SEQ.
           MOVE BM-MEMO                    TO XBM-MEMO.
           MOVE BM-MEMO-TYPE               TO XBM-MEMO-TYPE.

      *** TO MAKE REGION NUMBER MATCH A15 FILE FOR SPACES, *** 
      *** THE FOLLOWING "IF" STATEMENT IS PERFORMED        ***
           IF BR-REGION-NUMBER = 00
              MOVE SPACES           TO XBM-REGION-NUMBER
           ELSE
              MOVE BR-REGION-NUMBER TO XBM-REGION-NUMBER.
   

           MOVE X"0D"                      TO XBM-CR.
           MOVE X"0A"                      TO XBM-LF.
           ADD  01                         TO XBM-KEY.

           PERFORM WRITE-XBM-FILE.

           IF ( IO-BAD )
              GO TO IO-ERROR.

           ADD 01                          TO COUNT-RECORDS-WORKED.

           GO TO MAIN-PROGRAM-NEXT-BM.
 
      *=================================================================
       MAIN-PROGRAM-END-GROUP.

           PERFORM CLOSE-BM1-FILE.

           GO TO MAIN-PROGRAM-NEXT-GROUP.

       MAIN-PROGRAM-END.

           IF ( COUNT-RECORDS-WORKED NOT = ZEROES )
              PERFORM WRITE-RANGE-LINE.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

           MOVE FORM-PATHNAME              TO FORM-PATH.
           MOVE "BMEXTR"                   TO FORM-PROG   VDU-FORM-NAME.

           MOVE BMEXTR-QUEUE-POS           TO Q-POS.
           MOVE BMEXTR-COPIES-POS          TO C-POS.
           MOVE BMEXTR-PRU-POS             TO P-POS.
           MOVE SPACES                     TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           MOVE SPACES                     TO EXT-EOBUF-ERRCD.

           CALL "C$TOLOWER" USING XBM-DIRECTORY, VALUE 13.
           MOVE XBM-DIRECTORY              TO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.

           IF ( STAT-BAD )
              MOVE "EXTRACTION DIRECTORY DOES NOT EXIST!" TO MESS
              PERFORM SEND-MESS
              MOVE "X"                     TO EXT-EOBUF-ERRCD
              GO TO INITIALIZATION-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-ROUTE-BUF = "70" )
              MOVE EXT-EOBUF-MESS          TO H02-TITLE.

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
              MOVE EXT-CONAME-USER-LOC     TO BEG-BRANCH END-BRANCH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM GET-TIME.
           MOVE TIME-EDIT                  TO STARTING-TIME.

           MOVE EXT-CONAME-SYSDATE         TO H01-DATE.
           MOVE EXT-CONAME-CONAME          TO H01-NAME.
           MOVE EXT-CONAME-USER-ID         TO H02-USERID.
           MOVE EXT-CONAME-SYSDATE         TO H02-AGEDATE.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.

           PERFORM OPEN-PR-FILE.

           PERFORM OPEN-XBM-FILE-OUTPUT.
           PERFORM CLOSE-XBM-FILE.
           PERFORM OPEN-XBM-FILE.

           MOVE "EXTRACTION IN PROGRESS................." TO LEGEND-BUF.
           PERFORM SEND-LEGEND.

       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************

      * DONT GO FURTHER IF NOT A VALID LOGIN, NEED USERS BRANCH FOR BR-FILE
           IF ( EXT-CONAME-USER-LOC NOT NUMERIC )
              MOVE "INVALID USER"          TO MESS
              PERFORM SEND-MESS
              MOVE "X"                     TO EXT-EOBUF-ERRCD
              GO TO END-PROGRAM.

           IF ( UP-KEY )
              GO TO ENTER-END-SSNO.

       ENTER-BEG-BRANCH.

           MOVE LEGEND-BEG-BRANCH TO LEGEND-BUF.
           PERFORM SEND-LEGEND.

           MOVE "E  A000BEG-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-BRANCH.
           IF (        F6-KEY ) PERFORM SCAN-GR-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-BRANCH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO WGR-WORD ENT-GROUP.

           IF ( WGR-WORD-ZERO )
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
              GO TO CHECK-BRANCH-SECURITY.

      *----------------------------------------------------------------
       BRANCH-IS-NUMERIC.

           MOVE "N"                  TO GROUP-FLAG.
           MOVE WGR-SRESULT          TO BEG-BRANCH.
           MOVE "SN N040BEG-BRANCH." TO VDU.
           MOVE BEG-BRANCH           TO VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
       ENTER-END-BRANCH.

           MOVE LEGEND-END-BRANCH TO LEGEND-BUF.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040END-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-BRANCH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-BRANCH.

           IF ( END-BRANCH = ZEROES )
              GO TO ENTER-END-BRANCH.

           IF ( BEG-BRANCH > END-BRANCH )
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.

           GO TO CHECK-BRANCH-SECURITY.

      *----------------------------------------------------------------
       ENTER-ALL-BRANCH.

           MOVE "SNNN040BEG-BRANCH." TO VDU.
           MOVE ZEROES               TO VDUNUM0 BEG-BRANCH.
           PERFORM SCREENIO.

           MOVE "SNNN040END-BRANCH." TO VDU.
           MOVE 9999                 TO VDUNUM0 END-BRANCH.
           PERFORM SCREENIO.

           MOVE "N"                  TO GROUP-FLAG.

      *-----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.
       
           PERFORM BR-RANGE-SECURITY.
       
           IF ( BR-SECURITY-OKAY = "N" )
              GO TO ENTER-BEG-BRANCH.

       ENTER-BEG-SSNO.

           MOVE LEGEND-BEG-SSNO            TO LEGEND-BUF.
           PERFORM SEND-LEGEND.

           MOVE "ERNX090BEG-SSNO."         TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-SSNO.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-SSNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0                    TO BEG-SSNO.

      *-----------------------------------------------------------------
       ENTER-END-SSNO.

           MOVE LEGEND-END-SSNO            TO LEGEND-BUF.
           PERFORM SEND-LEGEND.

           MOVE "ERNX090END-SSNO."         TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-SSNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-SSNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUNUM0 = ZEROES )
              GO TO ENTER-END-SSNO.

           MOVE VDUNUM0                    TO END-SSNO.

           IF ( BEG-SSNO > END-SSNO )
              MOVE "ERROR: SSNO RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS 
              GO TO ENTER-BEG-SSNO.

           GO TO ENTER-BEG-DATE.

      *-----------------------------------------------------------------
       ENTER-ALL-SSNO.

           MOVE "S  X110BEG-SSNO."         TO VDU.
           MOVE ZEROES                     TO VDU-SSNO-EDIT  BEG-SSNO.
           PERFORM SCREENIO.

           MOVE "S  X110END-SSNO."         TO VDU.
           MOVE 999999999                  TO VDU-SSNO-EDIT  END-SSNO.
           PERFORM SCREENIO.

       ENTER-BEG-DATE.

           IF ( EXT-ROUTE-BUF = "40" OR "50" )
              GO TO ENTER-PRIORITY-ISO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "ERNM060BEG-DATE."         TO VDU. 
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-SSNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-DATE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-DATE.

           MOVE VDU-DATE-YYYYMMDD          TO BEG-DATE. 

           IF ( BEG-DATE = ZEROES )
              MOVE "S  8000BEG-DATE." TO VDU
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD
              MOVE DATE-YYYYMMDD TO VDU-DATE-YYYYMMDD  BEG-DATE
              PERFORM SCREENIO.

      *-----------------------------------------------------------------
       ENTER-END-DATE.
         
           IF ( EXT-ROUTE-BUF = "40" OR "50" )
              IF ( UP-KEY )
                 GO TO ENTER-END-SSNO
              ELSE
                 GO TO ENTER-PRIORITY-ISO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE LEGEND-PRIORITY-ISO        TO LEGEND-BUF.
           PERFORM SEND-LEGEND.

           MOVE "ERNM060END-DATE."         TO VDU. 
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-DATE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-DATE.
           
           MOVE VDU-DATE-YYYYMMDD          TO END-DATE.

           IF ( END-DATE = ZEROES )
              MOVE "S  8000END-DATE."      TO VDU
              MOVE EXT-CONAME-SYSDATE      TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-YYYYMMDD
              MOVE DATE-YYYYMMDD TO VDU-DATE-YYYYMMDD  END-DATE
              PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF END-DATE < BEG-DATE
              MOVE "ERROR: DATE RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS 
              GO TO ENTER-BEG-DATE.

           GO TO ENTER-PRIORITY-ISO.
     
      *-----------------------------------------------------------------
       ENTER-ALL-DATE.

           MOVE "S  8080BEG-DATE."  TO VDU.
           MOVE EXT-JULIAN-BEG      TO BEG-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "S  8080END-DATE."  TO VDU.
           MOVE EXT-JULIAN-END      TO END-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

       ENTER-PRIORITY-ISO.

           MOVE LEGEND-PRIORITY-ISO        TO LEGEND-BUF.
           PERFORM SEND-LEGEND.

           MOVE "E  A010PRIORITY."         TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-DATE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PRIORITY-ISO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUBUF = "I" OR "S" OR "O" )
              MOVE VDUBUF TO PRIORITY-ISO
           ELSE
              MOVE "ERROR: (I)NCLUDE, (S)KIP OR (O)NLY" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PRIORITY-ISO.

       ENTER-PARAMETERS-END.

           MOVE LEGEND-ACCEPT-YN           TO LEGEND-BUF.
           PERFORM SEND-LEGEND.

       ENTER-PARAMETERS-EXIT.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************

           MOVE EXT-EOBUF-DATE1            TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2            TO END-DATE.

       EO-EXEC-AUDIT-EXIT.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************

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

           MOVE "S  X110BEG-SSNO."         TO VDU.
           MOVE BEG-SSNO                   TO VDU-SSNO-EDIT.
           PERFORM SCREENIO.

           MOVE "S  X110END-SSNO."         TO VDU.
           MOVE END-SSNO                   TO VDU-SSNO-EDIT.
           PERFORM SCREENIO.

           MOVE "S  A010PRIORITY."         TO VDU
           MOVE PRIORITY-ISO               TO VDUBUF.
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
              MOVE "XX"                    TO VDUSTATUS
              GO TO SCAN-GR-FILE-EXIT.

           MOVE "S  A000BEG-BRANCH."       TO VDU.
           MOVE GRSCAN-KEY                 TO VDUBUF GR1-KEY.
           PERFORM SCREENIO.

           MOVE GRSCAN-KEY                 TO VDUBUF.
           MOVE "00"                       TO VDUSTATUS.

       SCAN-GR-FILE-EXIT.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************

           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF ( LN-WK > EXT-CONAME-RPT-LINES )
              PERFORM WRITE-HEADER-LINE.

           MOVE DETAIL-LINE                TO PR-REC.
           PERFORM WRITE-PR-REC.

           ADD LN-FEED                     TO LN-COUNT.
           MOVE 01                         TO LN-FEED.
           MOVE SPACES                     TO DETAIL-LINE.

       WRITE-DETAIL-LINE-EXIT.
           EXIT.
 
      ******************************************************************
       WRITE-HEADER-LINE SECTION.
      ******************************************************************

           ADD  01                         TO PAGE-NUMBER.
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
 
           MOVE 03                         TO LN-COUNT.
           MOVE 02                         TO LN-FEED.

       WRITE-HEADER-LINE-EXIT.
           EXIT.
 
      ******************************************************************
       WRITE-RANGE-LINE SECTION.
      ******************************************************************

           MOVE 99 TO LN-COUNT.

           IF ( GROUP-FLAG = "Y" )
              MOVE "BEGINNING BRANCH      :" TO R01-PROMPT
              MOVE ENT-GROUP                 TO R01-TEXT
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING    BRANCH      :" TO R01-PROMPT
              MOVE ENT-GROUP                 TO R01-TEXT
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEGINNING BRANCH      :" TO R01-PROMPT
              MOVE BEG-BRANCH                TO R01-NUMBER
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING    BRANCH      :" TO R01-PROMPT
              MOVE END-BRANCH                TO R01-NUMBER
              PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING SSNO        : " TO R01-PROMPT.
           MOVE BEG-SSNO                   TO R01-SSNO.
           INSPECT R01-SSNO REPLACING ALL "/" BY "-".
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    SSNO        : " TO R01-PROMPT.
           MOVE END-SSNO                   TO R01-SSNO.
           INSPECT R01-SSNO REPLACING ALL "/" BY "-".
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING MEMODATE    : " TO R01-PROMPT.
           MOVE BEG-DATE                   TO R01-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    MEMODATE    : " TO R01-PROMPT.
           MOVE END-DATE                   TO R01-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "PRIORITY (I,S,O)      : " TO R01-PROMPT.
           MOVE PRIORITY-ISO               TO R01-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "STARTING TIME         : " TO R01-PROMPT.
           MOVE STARTING-TIME              TO R01-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING   TIME         : " TO R01-PROMPT.
           PERFORM GET-TIME.
           MOVE TIME-EDIT                  TO R01-TEXT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "# RECORDS EXTRACTED   : " TO R01-PROMPT.
           MOVE COUNT-RECORDS-WORKED       TO R01-NUMBER.
           PERFORM WRITE-DETAIL-LINE.

       WRITE-RANGE-LINE-EXIT.
           EXIT.

      ******************************************************************
       FORM-RESET SECTION.
      ******************************************************************

           MOVE EXT-CONAME-CONAME          TO BMEXTR-CONAME.
           MOVE EXT-CONAME-SYSDATE         TO BMEXTR-SYSDATE.

           DISPLAY BMEXTR-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.

           MOVE BMEXTR-FORM-FIELDS         TO WR-BUF.
           MOVE BMEXTR-FORM-LIST           TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ******************************************************************
       SETUP-LINK-INFO SECTION.
      ******************************************************************

           MOVE BMEXTR-HELP--FILE          TO HELP-LINK-FILE.
           MOVE BMEXTR-HELP--DIR           TO HELP-LINK-DIR.

       SETUP-LINK-INFO-EXIT.
           EXIT.

      ******************************************************************
      *
      *    V D U - C O N V E R T - F I E L D
      *
      ******************************************************************
       VDU-CONVERT-FIELD SECTION.

           MOVE "00"                       TO STAT.
           MOVE VDU-FIELD-TO-CONVERT       TO VDU-ALP-FIELD.

           EVALUATE TRUE
              COPY "LIBSP/BMEXTR_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       VDU-CONVERT-FIELD-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
 
      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       INVALID-RANGE.

           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       SEND-LEGEND.

           MOVE LEGEND-BUF                 TO WR-BUF.
           MOVE LEGEND-LIST                TO WR-LIST.
           PERFORM CALL-WRFORM.
 
      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPBMGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".

       COPY "LIBEX/EXXBMI.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".

      *COPY "LIBLP/LPBM1RN.CPY".
      * COPYMEMBER: LIBLP/LPBM1RN
       LOAD-BM1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BM-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BM-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BM-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BM-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO BM-ALL-DATA
              MOVE BM-ALL-PATH             TO BM-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BM-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BM-ALL-BASE
              MOVE FILPATH-MACHINE         TO BM-ALL-MACHINE
              MOVE FILPATH-DATA            TO BM-ALL-DATA
              MOVE BM-ALL-PATH             TO BM-PATH.

      *-----------------------------------------------------------------
       OPEN-BM1-FILE.
           PERFORM LOAD-BM1-FILE.
           PERFORM OPEN-IT.
           MOVE BM-PATH-SQL TO E-FILE.

           IF BM1-CONN-OPEN-SW = "N" 
              MOVE "Y"         TO BM1-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS BM1_CONNECT 
                       USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
              END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-BM1-FILE.
           PERFORM START-IT.
           MOVE BM-PATH-OWNBR  TO BM-BRNO.
           MOVE BM-PATH-SQL    TO E-FILE.
           MOVE BM1-KEY        TO E-KEYX.

           IF ( BM1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BM1-FILE.

           IF ( QBM1-WEND-SEQ = ZEROES    ) OR
              ( QBM1-WEND-SEQ NOT NUMERIC )
              MOVE BM-PATH-OWNBR        TO QBM1-WBEG-BRNO
              MOVE BM-PATH-OWNBR        TO QBM1-WEND-BRNO
              MOVE BM-SSNO              TO QBM1-WBEG-SSNO
              MOVE ALL "9"              TO QBM1-WEND-SSNO
              MOVE BM-DATE-REVERSED     TO QBM1-WBEG-DATE-REV
              MOVE ALL "9"              TO QBM1-WEND-DATE-REV
              MOVE BM-SEQ               TO QBM1-WBEG-SEQ
              MOVE ALL "9"              TO QBM1-WEND-SEQ.

      *  STORED PROCEDURE:
      *
      *   SELECT TEMP.BM_BRNO,
      *          TEMP.BM_SSNO,
      *          TEMP.BM_DATE_REVERSED,
      *          TEMP.BM_SEQ,
      *          TEMP.BM_MEMO,
      *          TEMP.BM_MEMO_TYPE
      *
      *     FROM
      *
      *  (SELECT BMFILE.BM_BRNO,
      *          BMFILE.BM_SSNO,
      *          CAST(BMFILE.BM_DATE_REVERSED AS VARCHAR(10))
      *                                       AS BM_DATE_REVERSED,
      *          BMFILE.BM_SEQ,
      *          BMFILE.BM_MEMO,
      *          BMFILE.BM_MEMO_TYPE
      *
      *     FROM DBO.BMFILE
      * 
      *    WHERE  BMFILE.BM_BRNO               = @QBM1_WBEG_BRNO
      *      AND  BMFILE.BM_SSNO          BETWEEN @QBM1_WBEG_SSNO
      *                                       AND @QBM1_WEND_SSNO
      *      AND (BMFILE.BM_DATE_REVERSED BETWEEN @QBM1_WBEG_DATE_REV
      *                                       AND @QBM1_WEND_DATE_REV
      *       OR  BMFILE.BM_DATE_REVERSED       = 0)
      *      AND  BMFILE.BM_SEQ           BETWEEN @QBM1_WBEG_SEQ       
      *                                       AND @QBM1_WEND_SEQ) TEMP
      *
      *    WHERE ((TEMP.BM_DATE_REVERSED       != 99999999)
      *      AND  (@QWS_PRIORITY_ISO            = 'S'         
      *      AND   TEMP.BM_DATE_REVERSED       != 0)
      *       OR  (@QWS_PRIORITY_ISO            = 'O'         
      *      AND   TEMP.BM_DATE_REVERSED        = 0) 
      *       OR  (@QWS_PRIORITY_ISO            = 'I')) 
      *    ORDER BY TEMP.BM_BRNO,
      *             TEMP.BM_SSNO,
      *             TEMP.BM_DATE_REVERSED,
      *             TEMP.BM_SEQ;

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           EXEC SQL SET CONNECTION BM1_CONNECT END-EXEC.

           EXEC SQL
            DECLARE BM1_CURSOR CURSOR FOR
                :BM1-RETURN-CODE = EXEC SSP_BMEXTR_BMFILE_SELECT 
              ( :QBM1-WBEG-BRNO,    
                :QBM1-WBEG-SSNO,     
                :QBM1-WEND-SSNO,     
                :QBM1-WBEG-DATE-REV, 
                :QBM1-WEND-DATE-REV, 
                :QBM1-WBEG-SEQ,      
                :QBM1-WEND-SEQ,    
                :QWS-PRIORITY-ISO )
           END-EXEC.

           EXEC SQL
               OPEN BM1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QBM1-WBEG-BRNO
                          QBM1-WEND-BRNO
                          QBM1-WBEG-SSNO
                          QBM1-WEND-SSNO
                          QBM1-WBEG-DATE-REV
                          QBM1-WEND-DATE-REV
                          QBM1-WBEG-SEQ
                          QBM1-WEND-SEQ.

           MOVE STAT---GOOD TO BM1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC. 

      *-----------------------------------------------------------------
       READ-BM1-FILE.
           PERFORM READ-IT.
           MOVE BM-PATH-OWNBR  TO BM-BRNO. 
           MOVE BM-PATH-SQL    TO E-FILE.
           MOVE BM1-KEY        TO E-KEYX.
           INITIALIZE QBM-REC.

           MOVE BM-BRNO          TO QBM-BRNO.
           MOVE BM-SSNO          TO QBM-SSNO.
           MOVE BM-DATE-REVERSED TO QBM-DATE-REVERSED.
           MOVE BM-SEQ           TO QBM-SEQ.

           EXEC SQL
            SELECT BMFILE.BM_BRNO,
                   BMFILE.BM_SSNO,
                   BMFILE.BM_DATE_REVERSED,
                   BMFILE.BM_SEQ,
                   CAST(BMFILE.BM_LTOUCH_DATE
                                                  AS VARCHAR(10)),
                   BMFILE.BM_LTOUCH_TIME_HHMM,
                   BMFILE.BM_LTOUCH_USERID,
                   BMFILE.BM_MEMO,
                   BMFILE.BM_MEMO_TYPE
            INTO :QBM-REC
            FROM DBO.BMFILE
            WHERE BMFILE.BM_BRNO = :QBM-BRNO
              AND BMFILE.BM_SSNO = :QBM-SSNO
              AND BMFILE.BM_DATE_REVERSED = :QBM-DATE-REVERSED
              AND BMFILE.BM_SEQ = :QBM-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BM1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-BM-FIELDS.

      *-----------------------------------------------------------------
       READ-BM1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BM-PATH-OWNBR  TO BM-BRNO.
           MOVE BM-PATH-SQL    TO E-FILE.
           MOVE BM1-KEY        TO E-KEYX.
           INITIALIZE QBM-REC.

           EXEC SQL SET CONNECTION BM1_CONNECT END-EXEC.

           IF ( BM1-CURSOR-STAT-GOOD )
              EXEC SQL
                FETCH BM1_CURSOR 
                  INTO 
                    :QBM-BRNO,
                    :QBM-SSNO,
                    :QBM-DATE-REVERSED,
                    :QBM-SEQ,
                    :QBM-MEMO,
                    :QBM-MEMO-TYPE
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BM1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BM-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC. 

      *-----------------------------------------------------------------
       CLOSE-BM1-FILE.
           PERFORM CLOSE-IT.
           IF ( BM1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION BM1_CONNECT END-EXEC
              EXEC SQL
                   CLOSE BM1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO BM1-CURSOR-STAT
              EXEC SQL SET CONNECTION C1 END-EXEC.
      *-----------------------------------------------------------------
       CLOSE-BM1-CONNECTION.

           IF BM1-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION BM1_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT BM1_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO BM1-CONN-OPEN-SW.

      *-----------------------------------------------------------------
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.
 
      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************

           PERFORM CLOSE-FILES.

           IF ( EXT-ROUTE-BUF = "00" )
              PERFORM PRU-OUT.

           IF ( COUNT-RECORDS-WORKED = ZEROES )
              MOVE "NO REPORT FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

      *-----------------------------------------------------------------
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
              MOVE EXITPROG                TO FORM-NAM
              MOVE FORM-PATH               TO FORM-PATHNAME.

      *-----------------------------------------------------------------
       STOP-RUN.

           IF ( EXT-ROUTE-BUF = "00" OR "40" OR "50" )
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY BMEXTR-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
