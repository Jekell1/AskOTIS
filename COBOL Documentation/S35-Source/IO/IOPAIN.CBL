       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOPAIN.
       AUTHOR.        BAH
       DATE-WRITTEN.  04-15-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOPAIN.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : PAFILE 
      * TABLE DESC    : PAYEE  FILE
      * OUTPUT FILE   : XPC/IOPAIN.PRE
      * USED FOR      : LIBLP/LPWSPA.CPY  --- QPA-REC
      *                 LIBLP/LPPA1R.CPY  --- READ
      *                 LIBLP/LPPA1IN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * BAH 2021-0415 ***NEW***
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)IO/IOPAIN.CBL S34 05/10/21-14:29:15 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".

      *=================================================================
       COPY "LIBLP/LP01PA.CPY".
       COPY "LIBLP/LPWSPA.CPY".

      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S LIBXX/XX01YY_SQL.CPY
      *#################################################################

      *=================================================================
      *
      *    Q - F I L E
      *
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QPA1-WHERE-BEG-KEY.
           03  QPA1-WBEG-BRNO             PIC 9(04).
           03  QPA1-WBEG-NUMBER           PIC 9(06).

       01  QPA1-WHERE-END-KEY.
           03  QPA1-WEND-BRNO             PIC 9(04).
           03  QPA1-WEND-NUMBER           PIC 9(06).

       01  QPA-REC.
           03  QPA-BRNO            PIC 9(4).
           03  QPA-NUMBER          PIC 9(6).
           03  QPA-NAME            PIC X(30).
           03  QPA-ADR1            PIC X(30).
           03  QPA-ADR2            PIC X(30).
           03  QPA-CITY            PIC X(20).
           03  QPA-STATE           PIC X(2).
           03  QPA-ZIP             PIC X(10).
           03  QPA-LCHECK          PIC 9(6).
           03  QPA-LCDATE          PIC X(10).
           03  QPA-LAMT            PIC S9(7)V99.
           03  QPA-YTD-PAID        PIC S9(7)V99.
           03  QPA-LYTD-PAID       PIC S9(7)V99.
           03  QPA-SORTKEY         PIC X(10).
           03  QPA-CUSNO           PIC X(12).
           03  QPA-CONTACT         PIC X(30).
           03  QPA-PHONE           PIC 9(10).
           03  QPA-FAX             PIC 9(10).
           03  QPA-1099-FLAG       PIC X.
           03  QPA-1099-ID         PIC X(12).
           03  QPA-LEGAL-NAME1     PIC X(50).
           03  QPA-LEGAL-NAME2     PIC X(50).
           03  QPA-VAT-PERC        PIC 99V99.
           03  QPA-RFC             PIC X(13).
           03  QPA-CURP            PIC X(18).
           03  QPA-LTOUCH-DATE     PIC X(10).

       EXEC SQL END DECLARE SECTION END-EXEC.
      *#################################################################
      *    ENDING OF UPDATE TO THE FILE'S LIBXX/XX01YY_SQL.CPY
      *#################################################################

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  RECORDS-FOUND               PIC X(01)     VALUE "X".
           88  NO-RECORDS-FOUND                      VALUE "N".
           88  EXIT-PRIOR-TO-EXECUTION               VALUE "X".

       01  RECORDS-READ                PIC 9(11)     VALUE ZEROES.

       01  SUBSCRIPT-WORKERS.
           03  SUB                     PIC 9(09)     VALUE ZEROES.
           03  S1                      PIC 9(09)     VALUE ZEROES.
           03  S2                      PIC 9(09)     VALUE ZEROES.
           03  S3                      PIC 9(09)     VALUE ZEROES.

      ******************************************************************
      *
      *    L E G E N D - W O R K E R S
      *
      ******************************************************************
       01  LEGEND                      PIC X(77)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(09)     VALUE "LEGEND:B.".

      *=================================================================
       01  LEGEND-ACCEPT-YN.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, UP-ARROW, (".
           03  FILLER PIC X(30) VALUE "Y)ES OR (N)O TO ACCEPT        ".
           03  FILLER PIC X(17) VALUE "   ACCEPT (Y/N): ".

       01  LEGEND-PRINTER-OPTIONS.
           03  FILLER PIC X(30) VALUE "F7-EXIT, ENTER PRINTER OPTIONS".

       01  LEGEND-BEG-BRANCH.
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING BRANCH NO. (F3-ALL) ".

       01  LEGEND-END-BRANCH.
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING BRANCH NO. ".

       01  LEGEND-PROCESS-KEY.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, V, ENTER NU".
           03  FILLER PIC X(30) VALUE "MBER OF FILE KEY TO PROCESS   ".

       01  LEGEND-BEG-END-RANGE.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, V, ENTER RA".
           03  FILLER PIC X(30) VALUE "ANGE VALUE                    ".

      ******************************************************************
      *
      *    P A C E - W O R K E R S
      *
      ******************************************************************
       78  PACE-COUNTER-MAX                          VALUE 1000.
       77  PACE-COUNTER                PIC 9(6)      VALUE ZEROES.
       77  PACE-DISPLAY-MAX            PIC ZZZ,ZZ9   VALUE ZEROES.
       78  PACE-MAX                                  VALUE 15.
       77  PACE-LEV                    PIC 9(2)      VALUE ZEROES.

       01  PACE-HEAD-LINE.
           03  FILLER PIC X(28) VALUE "TIME        KEY             ".
           03  PACE-HEAD-MAX    PIC ZZZ,ZZ9 VALUE ZEROES.
           03  FILLER PIC X(05) VALUE " RECS".

       01  PACE-TIME.
           03  PACE-TIME-HH            PIC 99.
           03  FILLER                  PIC X(1)      VALUE ":".
           03  PACE-TIME-MM            PIC 99.
           03  FILLER                  PIC X(1)      VALUE ":".
           03  PACE-TIME-SS            PIC 99.
           03  FILLER                  PIC X(1)      VALUE ".".
           03  PACE-TIME-HD            PIC 99.

       01  PACE-DETAIL.
           03  PACE-DETAIL-TIME        PIC 99/99/99/99.
           03  FILLER                  PIC X(1).
           03  PACE-DETAIL-KEY         PIC X(28).

       01  PACE-BUF.
           03  PACE-ELE                PIC X(40)     OCCURS PACE-MAX.

       01  PACE-LIST.
           03  FILLER PIC X(24) VALUE "PACE-01 PACE-02 PACE-03 ".
           03  FILLER PIC X(24) VALUE "PACE-04 PACE-05 PACE-06 ".
           03  FILLER PIC X(24) VALUE "PACE-07 PACE-08 PACE-09 ".
           03  FILLER PIC X(24) VALUE "PACE-10 PACE-11 PACE-12 ".
           03  FILLER PIC X(24) VALUE "PACE-13 PACE-14 PACE-15.".

      ******************************************************************
      *
      *    P A R A M E T E R - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-BRANCH              PIC 9(04).
           03  END-BRANCH              PIC 9(04).
           03  GROUP-FLAG              PIC X(01).
           03  ENT-GROUP               PIC X(04).

           03  PROCESS-KEY             PIC 9(01).
           03  BEG-NUMBER              PIC 9(06).
           03  END-NUMBER              PIC 9(06).
           03  BEG-SORTKEY             PIC X(10).
           03  END-SORTKEY             PIC X(10).

      *-----------------------------------------------------------------
       01  VERSION-CONTROL             PIC X(01)     VALUE "A".

       01  SAVE-SCREEN-RANGES.
           03  SV-BEG-BRANCH           PIC 9(04)     VALUE 9999.
           03  SV-END-BRANCH           PIC 9(04)     VALUE 9999.

           03  SV-GROUP-OPTION.
               05  SV-GROUP-FLAG       PIC X(01).
               05  SV-ENT-GROUP        PIC X(04).

      ******************************************************************
      *
      *    R E P O R T - W O R K E R S
      *
      ******************************************************************
       01  PRINT-LINE-WORKERS.
           03  LN-COUNT                PIC 9(03)     VALUE 99.
           03  LN-FEED                 PIC 9(02)     VALUE ZEROES.
           03  LN-WK                   PIC 9(03)     VALUE ZEROES.
           03  PAGE-NUMBER             PIC 9(04)     VALUE ZEROES.

       01  STARTING-TIME               PIC X(08)     VALUE SPACES.

      *=================================================================
       01  HEADER-LINE-01.
           03  H01-DATE           PIC X(08) VALUE SPACES.
           03  FILLER             PIC X(02) VALUE SPACES.
           03  H01-HOUR           PIC 9(02) VALUE ZEROES.
           03  FILLER             PIC X(01) VALUE ":".
           03  H01-MINUTE         PIC 9(02) VALUE ZEROES.
           03  FILLER             PIC X(30) VALUE SPACES.
           03  H01-NAME           PIC X(40) VALUE SPACES.
           03  FILLER             PIC X(37) VALUE SPACES.
           03  FILLER             PIC X(06) VALUE "PAGE: ".
           03  H01-PAGE           PIC 9(04).

      *-----------------------------------------------------------------
       01  HEADER-LINE-02.
           03  FILLER             PIC X(10) VALUE "USER ID:".
           03  H02-USERID         PIC X(10) VALUE SPACES.
           03  H02-TITLE          PIC X(92) VALUE SPACES.
           03  FILLER             PIC X(20) VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEADER-LINE-03.
           03  FILLER PIC X(38) VALUE "BRNO   NUMBER   NAME".
           03  FILLER PIC X(30) VALUE "STATE   PHONE".

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-PA-BRNO                    PIC 9(04).
           03  FILLER                         PIC X(03).
           03  D01-PA-NUMBER                  PIC 9(06).
           03  FILLER                         PIC X(03).
           03  D01-PA-NAME                    PIC X(20).
           03  FILLER                         PIC X(03).
           03  D01-PA-STATE                   PIC X(2).
           03  FILLER                         PIC X(03).
           03  D01-PA-PHONE                   PIC 999/999/9999.

      *-------------------------------------------------------{ RANGES }
       01  RANGE-LINE-01                      REDEFINES DETAIL-LINE.
           03  R01-PROMPT                     PIC X(32).
           03  R01-FIELD                      PIC X(100).
           03  R01-DATE                       REDEFINES R01-FIELD
                                              PIC 99/99/99.
           03  R01-NUMERIC                    REDEFINES R01-FIELD
                                              PIC Z(10)9.
           03  R01-TEXT                       REDEFINES R01-FIELD
                                              PIC X(100).

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
      *COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/MKTEMPW.CPY".
       COPY "LIBGB/PASSWDW.CPY".
      *-----------------------------------------------------------------
       COPY "LIBIO/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
      *-----------------------------------------------------------------
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/TIMEWK.CPY".

       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBWI/TSTINTW.CPY".
       COPY "LIBWI/GRSCANW.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBIO/IOPAIN_DEF.CPY".
       COPY "LIBIO/IOPAIN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOPAIN_SCN.CPY".

      ******************************************************************
      *
      *    P R O C E D U R E - D I V I S I O N
      *
      ******************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE.

       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

           PERFORM INITIALIZATION.

           IF ( EXT-EOBUF-ERRCD NOT = SPACES )
              GO TO MAIN-PROGRAM-EXIT.

           IF ( GROUP-FLAG = "N" )
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO MAIN-PROGRAM-NEXT-BRANCH.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

      *=================================================================
       MAIN-PROGRAM-NEXT-GROUP.

           IF ( GROUP-FLAG = "N" )
              GO TO MAIN-PROGRAM-NEXT-BRANCH.

           PERFORM SEARCH-GR-NEXT.

           IF ( WGR-SRESULT = ZEROES )
              GO TO MAIN-PROGRAM-END.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.

           IF ( IO-BAD )
              GO TO IO-ERROR.

           GO TO MAIN-PROGRAM-PROCESS-BRNO.

      *-----------------------------------------------------------------
       MAIN-PROGRAM-NEXT-BRANCH.

           PERFORM READ-BR-FILE-NEXT.

           IF ( IO-BAD             ) OR
              ( BR-NO > END-BRANCH )
              GO TO MAIN-PROGRAM-END.

      *-----------------------------------------------------------------
       MAIN-PROGRAM-PROCESS-BRNO.

           PERFORM PROCESS-BRANCH-MODULE.

           GO TO MAIN-PROGRAM-NEXT-GROUP.

      *=================================================================
       MAIN-PROGRAM-END.

           PERFORM WRITE-SUMMARY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    DISPLAY SUMMARY TO SCREEN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           PERFORM GET-TIME.
           MOVE SPACES TO SENDMESS-HEAD.

           STRING "PROGRAM COMPLETED AT ", TIME-EDIT
                  DELIMITED BY SIZE INTO SENDMESS-HEAD.

           MOVE SPACES TO SENDMESS-MSG1.
           MOVE SPACES TO SENDMESS-MSG2.
           STRING "RECORDS READ: ", RECORDS-READ
                  DELIMITED BY SIZE INTO SENDMESS-MSG2.
           DISPLAY MESSAGE BOX SENDMESS-HEAD, X"0A"
                               SENDMESS-MSG1, X"0A"
                               SENDMESS-MSG2.

      *-----------------------------------------------------------------
       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
      *
      *    I N I T I A L I Z A T I O N
      *
      ******************************************************************
       INITIALIZATION SECTION.
      D    STOP FORM-PATHNAME.

           MOVE "X"               TO RECORDS-FOUND.
           MOVE FORM-PATHNAME     TO FORM-PATH.
           MOVE "IOPAIN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOPAIN-QUEUE-POS  TO Q-POS.
           MOVE IOPAIN-COPIES-POS TO C-POS.
           MOVE IOPAIN-PRU-POS    TO P-POS.
           MOVE SPACES            TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF EXT-ROUTE-BUF = "00"
              PERFORM GET-TIME
              MOVE SPACES TO LEGEND
              STRING "STARTING TIME: ", TIME-EDIT
                     DELIMITED BY SIZE INTO LEGEND
              PERFORM SEND-LEGEND.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE "S  A010ACCEPT." TO VDU
              MOVE SPACES           TO VDUBUF
              PERFORM SCREENIO.

           IF ( BEG-BRANCH = 0 AND END-BRANCH = 0 )
              MOVE BEG-BRANCH TO SV-BEG-BRANCH
              MOVE END-BRANCH TO SV-END-BRANCH
              MOVE EXT-CONAME-USER-LOC  TO BEG-BRANCH
                                           END-BRANCH.

           IF ( EXT-EOBUF-GROUP-FG = "Y" )
              MOVE BEG-BRANCH         TO SV-BEG-BRANCH
              MOVE END-BRANCH         TO SV-END-BRANCH
              MOVE GROUP-FLAG         TO SV-GROUP-FLAG
              MOVE ENT-GROUP          TO SV-ENT-GROUP
              MOVE EXT-EOBUF-GROUP        TO ENT-GROUP
              MOVE ZEROES             TO BEG-BRANCH
              MOVE 9999               TO END-BRANCH
              MOVE "Y"                TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE.

           PERFORM GET-GPENV.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-PR-FILE.

           PERFORM ENABLE-INTERRUPT.
           PERFORM SEND-PACE-HEAD.

           MOVE ZEROES TO PACE-LEV.
           MOVE ZEROES TO RECORDS-READ.

           PERFORM GET-TIME.
           MOVE "N"       TO RECORDS-FOUND.
           MOVE TIME-EDIT TO STARTING-TIME.

           MOVE "R" TO TIMER-TYPE.
           PERFORM TIMER-START.
           MOVE TIMER-LINK-BUF TO P-TIMER-LINK-BUF.

      *=================================================================
       INITIALIZATION-EXIT.
           EXIT.

      ******************************************************************
      *
      *    E N T E R - P A R A M E T E R S
      *
      ******************************************************************
       ENTER-PARAMETERS SECTION.

           IF ( UP-KEY )
              GO TO ENTER-END-SORTKEY.

      *=================================================================
       ENTER-BEG-BRANCH.

           MOVE LEGEND-BEG-BRANCH TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A000BEG-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-BRANCH.
           IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-BRANCH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-BRANCH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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

      *=================================================================
       ENTER-PROCESS-KEY.

           MOVE LEGEND-PROCESS-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN010PROCESS-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUNUM0 = ZEROES ) OR
              ( VDUNUM0 > 1      ) 
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PROCESS-KEY.

           MOVE VDUNUM0 TO PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE LEGEND-BEG-END-RANGE TO LEGEND.
           PERFORM SEND-LEGEND.

      *=================================================================
       ENTER-BEG-NUMBER.

           MOVE "ENNN060BEG-NUMBER." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PROCESS-KEY.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-NUMBER.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-NUMBER.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-NUMBER.

      *-----------------------------------------------------------------
       ENTER-END-NUMBER.

           MOVE "ENNN060END-NUMBER." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-NUMBER.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-NUMBER.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-NUMBER.

           IF ( BEG-NUMBER > END-NUMBER )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-NUMBER.

           GO TO ENTER-BEG-SORTKEY.

      *-----------------------------------------------------------------
       ENTER-ALL-NUMBER.

           MOVE "SNNN060BEG-NUMBER." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-NUMBER.
           PERFORM SCREENIO.

           MOVE "SNNN060END-NUMBER." TO VDU.
           MOVE 999                 TO VDUNUM0  END-NUMBER.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-SORTKEY.

           MOVE "E  A100BEG-SORTKEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-NUMBER.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-SORTKEY.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-SORTKEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-SORTKEY.

      *-----------------------------------------------------------------
       ENTER-END-SORTKEY.

           MOVE "E  A100END-SORTKEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-SORTKEY.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-SORTKEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-SORTKEY.

           IF ( BEG-SORTKEY > END-SORTKEY )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-SORTKEY.

           GO TO ENTER-ACCEPT-YN.

      *-----------------------------------------------------------------
       ENTER-ALL-SORTKEY.

           MOVE "S  A080BEG-SORTKEY." TO VDU.
           MOVE SPACES              TO VDUBUF  BEG-SORTKEY.
           PERFORM SCREENIO.

           MOVE "S  A080END-SORTKEY." TO VDU.
           MOVE ALL "Z"            TO VDUBUF  END-SORTKEY.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-ACCEPT-YN.

           MOVE LEGEND-ACCEPT-YN TO LEGEND.
           PERFORM SEND-LEGEND.

      *=================================================================
       ENTER-PARAMETERS-EXIT.
           EXIT.

      *****************************************************************
      *
      *    D I S P L A Y - P A R A M E T E R S
      *
      *****************************************************************
       DISPLAY-PARAMETERS SECTION.

           IF ( GROUP-FLAG = "Y" )
              MOVE "S  A000BEG-BRANCH."   TO VDU
              MOVE ENT-GROUP              TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000END-BRANCH."   TO VDU
              MOVE SPACES                 TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BEG-BRANCH."   TO VDU
              MOVE BEG-BRANCH             TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040END-BRANCH."   TO VDU
              MOVE END-BRANCH             TO VDUNUM0
              PERFORM SCREENIO.

           MOVE "SNNN060BEG-NUMBER."       TO VDU.
           MOVE BEG-NUMBER                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060END-NUMBER."       TO VDU.
           MOVE END-NUMBER                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A000BEG-SORTKEY."      TO VDU.
           MOVE BEG-SORTKEY                TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A000END-SORTKEY."      TO VDU.
           MOVE END-SORTKEY                TO VDUBUF.
           PERFORM SCREENIO.


      *=================================================================
       DISPLAY-PARAMETERS-EXIT.
           EXIT.

      *****************************************************************
      *
      *    E O - E X E C - A U D I T
      *
      *****************************************************************
       EO-EXEC-AUDIT SECTION.

      *    MOVE EXT-EOBUF-DATE1 TO ???.
      *    MOVE EXT-EOBUF-DATE2 TO ???.

       EO-EXEC-AUDIT-EXIT.
           EXIT.

      ******************************************************************
      *
      *    S C A N - G R 1 - F I L E
      *
      ******************************************************************
       SCAN-GR1-FILE SECTION.

           MOVE GRSCAN-BUF TO SCAN-BUF.
           PERFORM CALL-WINDOW-PROGRAM.
           MOVE SCAN-BUF   TO GRSCAN-BUF.

           IF ( SCAN-STATUS NOT = "00"   ) OR
              ( GRSCAN-KEY      = SPACES )
              MOVE "XX" TO VDUSTATUS
              GO TO SCAN-GR1-FILE-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - -{ VALID SELECTION  }
           MOVE "S  A000BEG-BRANCH." TO VDU.
           MOVE GRSCAN-KEY           TO VDUBUF.
           PERFORM SCREENIO.

           MOVE GRSCAN-KEY TO VDUBUF.
           MOVE "00"       TO VDUSTATUS.

      *=================================================================
       SCAN-GR1-FILE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P R O C E S S - B R A N C H - M O D U L E
      *
      ******************************************************************
       PROCESS-BRANCH-MODULE SECTION.

           MOVE PACE-COUNTER-MAX TO PACE-COUNTER.

      *- - - - - - - - - - - - - - - - - -{ VERIFY 'LOCAL' FILE PATHS  }
           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO PA-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - -{ SET WHERE BEG & END FIELDS }
           MOVE BR-NO     TO QPA1-WBEG-BRNO
                              QPA1-WEND-BRNO.

           MOVE BEG-NUMBER  TO QPA1-WBEG-NUMBER.
           MOVE END-NUMBER  TO QPA1-WEND-NUMBER.

      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           PERFORM OPEN-PA1-FILE
           PERFORM START-PA1-FILE.

      *=================================================================
       PROCESS-BRANCH-NEXT.

           PERFORM READ-PA1-FILE-NEXT.
           IF ( IO-BAD ) OR ( PA-BRNO NOT = PA-PATH-OWNBR )
              GO TO PROCESS-BRANCH-MODULE-END.

           ADD 01 TO RECORDS-READ.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

              IF ( PA-NUMBER < BEG-NUMBER )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( PA-NUMBER > END-NUMBER )
                 GO TO PROCESS-BRANCH-MODULE-END.

      *-----------------------------------------------------------------
       PROCESS-BRANCH-PRINT.

           MOVE PA-BRNO                    TO D01-PA-BRNO.
           MOVE PA-NUMBER                  TO D01-PA-NUMBER.
           MOVE PA-NAME                   TO D01-PA-NAME.
           MOVE PA-STATE                    TO D01-PA-STATE.
           MOVE PA-PHONE                    TO D01-PA-PHONE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.
           GO TO PROCESS-BRANCH-NEXT.

      *=================================================================
       PROCESS-BRANCH-MODULE-END.

           PERFORM CLOSE-PA1-FILE.

      *-----------------------------------------------------------------
       PROCESS-BRANCH-MODULE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    P A C E - R O U T I N E
      *
      *=================================================================
      * DESC: DISPLAY THE PACE OF THE EXECUTION OF THE PROGRAM.
      ******************************************************************
       PACE-ROUTINE SECTION.

           IF ( EXT-ROUTE-BUF NOT = "00" )
              GO TO PACE-ROUTINE-EXIT.

           ADD 01 TO PACE-COUNTER.

           IF ( PACE-COUNTER <= PACE-COUNTER-MAX )
              GO TO PACE-ROUTINE-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           ADD 01 TO PACE-LEV.

           IF ( PACE-LEV <= PACE-MAX )
              GO TO PACE-ROUTINE-UPDATE.

           PERFORM VARYING SUB FROM 2 BY 1 UNTIL SUB > PACE-MAX
              MOVE PACE-ELE(SUB) TO PACE-ELE(SUB - 1)
           END-PERFORM.

           MOVE PACE-MAX TO PACE-LEV.
           MOVE SPACES   TO PACE-ELE(PACE-LEV).

      *-----------------------------------------------------------------
       PACE-ROUTINE-UPDATE.

           MOVE SPACES TO PACE-DETAIL.

           PERFORM GET-TIME.
           MOVE CUR-HH    TO PACE-TIME-HH.
           MOVE CUR-MM    TO PACE-TIME-MM.
           MOVE CUR-SS    TO PACE-TIME-SS.
           MOVE CUR-HD    TO PACE-TIME-HD.
           MOVE PACE-TIME TO PACE-DETAIL-TIME.

           MOVE PA1-KEY TO PACE-DETAIL-KEY.

           MOVE PACE-DETAIL TO PACE-ELE(PACE-LEV).
           MOVE PACE-BUF    TO WR-BUF.
           MOVE PACE-LIST   TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE ZEROES TO PACE-COUNTER.

      *=================================================================
       PACE-ROUTINE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - S U M M A R Y
      *
      ******************************************************************
       WRITE-SUMMARY SECTION.

           IF ( NO-RECORDS-FOUND )
              GO TO WRITE-SUMMARY-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 99     TO LN-COUNT.
           MOVE 1      TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( GROUP-FLAG = "Y" )
              MOVE "GROUP ID NAME                 : " TO R01-PROMPT
              MOVE ENT-GROUP                          TO R01-TEXT
              MOVE 2                                  TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEGINNING BRANCH NUMBER       : " TO R01-PROMPT
              MOVE BEG-BRANCH                         TO R01-NUMERIC
              MOVE 2                                  TO LN-FEED
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING    BRANCH NUMBER       : " TO R01-PROMPT
              MOVE END-BRANCH                         TO R01-NUMERIC
              MOVE 1                                  TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PROCESS KEY                   : "    TO R01-PROMPT.
           MOVE PROCESS-KEY                           TO R01-NUMERIC.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING NUMBER               : "    TO R01-PROMPT.
           MOVE BEG-NUMBER                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    NUMBER               : "    TO R01-PROMPT.
           MOVE END-NUMBER                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING SORTKEY              : "    TO R01-PROMPT.
           MOVE BEG-SORTKEY                            TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    SORTKEY              : "    TO R01-PROMPT.
           MOVE END-SORTKEY                            TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "STARTING TIME                 : "    TO R01-PROMPT.
           MOVE STARTING-TIME                         TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING   TIME                 : "    TO R01-PROMPT.
           PERFORM GET-TIME.
           MOVE TIME-EDIT                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *=================================================================
       WRITE-SUMMARY-EXIT.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - D E T A I L - L I N E
      *
      ******************************************************************
       WRITE-DETAIL-LINE SECTION.

           ADD LN-FEED TO LN-COUNT.

           IF ( LN-COUNT > EXT-CONAME-RPT-LINES )
              PERFORM WRITE-PAGE-HEADER.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.

           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *=================================================================
       WRITE-DETAIL-LINE-EXIT.
           EXIT.

      ******************************************************************
      *
      *    W R I T E - P A G E - H E A D E R
      *
      ******************************************************************
       WRITE-PAGE-HEADER SECTION.

           MOVE 01 TO LN-COUNT.
           ADD  01 TO PAGE-NUMBER.
           PERFORM GET-TIME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{01}
           MOVE EXT-CONAME-SYSDATE               TO H01-DATE.
           MOVE T-HH                             TO H01-HOUR.
           MOVE T-MM                             TO H01-MINUTE.
           MOVE EXT-CONAME-CONAME                TO H01-NAME.
           CALL "C$JUSTIFY"                   USING H01-NAME "C".
           MOVE PAGE-NUMBER                      TO H01-PAGE.

           MOVE HEADER-LINE-01                   TO PR-REC.
           MOVE 99                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{02}
           MOVE EXT-CONAME-USER-ID               TO H02-USERID.
           MOVE "SQL TABLE IO UTILITY - PAFILE"  TO H02-TITLE.
           CALL "C$JUSTIFY"                   USING H02-TITLE "C".

           MOVE HEADER-LINE-02                   TO PR-REC.
           ADD  01                               TO LN-COUNT.
           MOVE 01                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{03}
           MOVE HEADER-LINE-03                   TO PR-REC.
           ADD  01                               TO LN-COUNT.
           MOVE 01                               TO LN-FEED.
           PERFORM WRITE-PR-REC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE 01 TO LN-FEED.

      *=================================================================
       WRITE-PAGE-HEADER-EXIT.
           EXIT.

      ******************************************************************
      *
      *    F O R M - R E S E T
      *
      ******************************************************************
       FORM-RESET SECTION.

           MOVE EXT-CONAME-CONAME TO IOPAIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOPAIN-SYSDATE.
           DISPLAY IOPAIN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOPAIN-FORM-FIELDS TO WR-BUF.
           MOVE IOPAIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *=================================================================
       FORM-RESET-EXIT.
           EXIT.

      ******************************************************************
      *
      *    S E T U P - L I N K - I N F O
      *
      ******************************************************************
       SETUP-LINK-INFO SECTION.

           MOVE IOPAIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOPAIN-HELP--DIR  TO HELP-LINK-DIR.

      *=================================================================
       SETUP-LINK-INFO-EXIT.
           EXIT.

      ******************************************************************
      *
      *    S E T U P - L I N K - I N F O
      *
      ******************************************************************
       VDU-CONVERT-FIELD SECTION.

           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBIO/IOPAIN_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      *=================================================================
       VDU-CONVERT-FIELD-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/EOSPOOL.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

       COPY "LIBWI/PFSWINDOWS.CPY".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - P A R A G R A P H S
      *
      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      *COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
      *COPY "LIBGB/RMFILE.CPY".
      *=================================================================
       SEND-LEGEND.

           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *-----------------------------------------------------------------
       SEND-PACE-HEAD.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE PACE-COUNTER-MAX    TO PACE-HEAD-MAX
              MOVE "S  A400PACE-HEAD." TO VDU
              MOVE PACE-HEAD-LINE      TO VDUBUF
              PERFORM SCREENIO.

      ******************************************************************
      *
      *    I O - R O U T I N E S
      *
      ******************************************************************
       IO-ROUTINE SECTION.
       COPY "LIBIO/CONNECT_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBPRINT.CPY".

      *-----------------------------------------------------------------
      *    LOAD PA-REC FROM QPA-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-PA-FIELDS.
           MOVE QPA-BRNO TO PA-BRNO.
           MOVE QPA-NUMBER TO PA-NUMBER.
           MOVE QPA-NAME TO PA-NAME.
           MOVE QPA-ADR1 TO PA-ADR1.
           MOVE QPA-ADR2 TO PA-ADR2.
           MOVE QPA-CITY TO PA-CITY.
           MOVE QPA-STATE TO PA-STATE.
           MOVE QPA-ZIP TO PA-ZIP.
           MOVE QPA-LCHECK TO PA-LCHECK.

           IF ( QPA-LCDATE = "1900-01-01" )
              MOVE ZEROES TO PA-LCDATE
           ELSE
              MOVE QPA-LCDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PA-LCDATE.

           MOVE QPA-LAMT TO PA-LAMT.
           MOVE QPA-YTD-PAID TO PA-YTD-PAID.
           MOVE QPA-LYTD-PAID TO PA-LYTD-PAID.
           MOVE QPA-SORTKEY TO PA-SORTKEY.
           MOVE QPA-CUSNO TO PA-CUSNO.
           MOVE QPA-CONTACT TO PA-CONTACT.
           MOVE QPA-PHONE TO PA-PHONE.
           MOVE QPA-FAX TO PA-FAX.
           MOVE QPA-1099-FLAG TO PA-1099-FLAG.
           MOVE QPA-1099-ID TO PA-1099-ID.
           MOVE QPA-LEGAL-NAME1 TO PA-LEGAL-NAME1.
           MOVE QPA-LEGAL-NAME2 TO PA-LEGAL-NAME2.
           MOVE QPA-VAT-PERC TO PA-VAT-PERC.
           MOVE QPA-RFC TO PA-RFC.
           MOVE QPA-CURP TO PA-CURP.

           IF ( QPA-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO PA-LTOUCH-DATE
           ELSE
              MOVE QPA-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PA-LTOUCH-DATE.

 
      *-----------------------------------------------------------------
      *    LOAD QPA-REC FROM PA-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-PA-FIELDS.
           MOVE PA-BRNO TO QPA-BRNO.
           MOVE PA-NUMBER TO QPA-NUMBER.
           MOVE PA-NAME TO QPA-NAME.
           MOVE PA-ADR1 TO QPA-ADR1.
           MOVE PA-ADR2 TO QPA-ADR2.
           MOVE PA-CITY TO QPA-CITY.
           MOVE PA-STATE TO QPA-STATE.
           MOVE PA-ZIP TO QPA-ZIP.
           MOVE PA-LCHECK TO QPA-LCHECK.

           IF ( PA-LCDATE = ZEROES )
              MOVE "1900-01-01" TO QPA-LCDATE
           ELSE
              MOVE PA-LCDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPA-LCDATE.

           MOVE PA-LAMT TO QPA-LAMT.
           MOVE PA-YTD-PAID TO QPA-YTD-PAID.
           MOVE PA-LYTD-PAID TO QPA-LYTD-PAID.
           MOVE PA-SORTKEY TO QPA-SORTKEY.
           MOVE PA-CUSNO TO QPA-CUSNO.
           MOVE PA-CONTACT TO QPA-CONTACT.
           MOVE PA-PHONE TO QPA-PHONE.
           MOVE PA-FAX TO QPA-FAX.
           MOVE PA-1099-FLAG TO QPA-1099-FLAG.
           MOVE PA-1099-ID TO QPA-1099-ID.
           MOVE PA-LEGAL-NAME1 TO QPA-LEGAL-NAME1.
           MOVE PA-LEGAL-NAME2 TO QPA-LEGAL-NAME2.
           MOVE PA-VAT-PERC TO QPA-VAT-PERC.
           MOVE PA-RFC TO QPA-RFC.
           MOVE PA-CURP TO QPA-CURP.

           IF ( PA-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QPA-LTOUCH-DATE
           ELSE
              MOVE PA-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPA-LTOUCH-DATE.

 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-PA1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( PA-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( PA-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO PA-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO PA-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO PA-ALL-DATA
              MOVE PA-ALL-PATH             TO PA-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( PA-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO PA-ALL-BASE
              MOVE FILPATH-MACHINE         TO PA-ALL-MACHINE
              MOVE FILPATH-DATA            TO PA-ALL-DATA
              MOVE PA-ALL-PATH             TO PA-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-PAFILE NOT = "S" )
              MOVE PA-PATH-BEG             TO PA-SELECT-PATH-BEG
              MOVE PA-PATH-FILE            TO PA-SELECT-PATH-FILE
           ELSE
              MOVE PA-PATH-SQL             TO PA-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-PA1-FILE.
           PERFORM LOAD-PA1-FILE.
           PERFORM OPEN-IT.
           MOVE PA-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-PA1-FILE.
           PERFORM START-IT.
           MOVE PA-PATH-OWNBR  TO PA-BRNO.
           MOVE PA-SELECT-PATH TO E-FILE.
           MOVE PA1-KEY        TO E-KEYX.

           IF ( PA1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-PA1-FILE.

           IF ( QPA1-WEND-NUMBER = ZEROES    ) OR
              ( QPA1-WEND-NUMBER NOT NUMERIC )
              MOVE PA-PATH-OWNBR        TO QPA1-WBEG-BRNO
              MOVE PA-PATH-OWNBR        TO QPA1-WEND-BRNO
              MOVE PA-NUMBER            TO QPA1-WBEG-NUMBER
              MOVE ALL "9"              TO QPA1-WEND-NUMBER.


           EXEC SQL
            DECLARE PA1_CURSOR CURSOR FOR
            SELECT PAFILE.PA_BRNO,
                   PAFILE.PA_NUMBER,
                   ISNULL(PAFILE.PA_NAME,''),
                   ISNULL(PAFILE.PA_ADR1,''),
                   ISNULL(PAFILE.PA_ADR2,''),
                   ISNULL(PAFILE.PA_CITY,''),
                   ISNULL(PAFILE.PA_STATE,''),
                   ISNULL(PAFILE.PA_ZIP,''),
                   PAFILE.PA_LCHECK,
                   ISNULL(CAST(PAFILE.PA_LCDATE AS VARCHAR(10)),'0'),
                   PAFILE.PA_LAMT,
                   PAFILE.PA_YTD_PAID,
                   PAFILE.PA_LYTD_PAID,
                   ISNULL(PAFILE.PA_SORTKEY,''),
                   ISNULL(PAFILE.PA_CUSNO,''),
                   ISNULL(PAFILE.PA_CONTACT,''),
                   PAFILE.PA_PHONE,
                   PAFILE.PA_FAX,
                   ISNULL(PAFILE.PA_1099_FLAG,''),
                   ISNULL(PAFILE.PA_1099_ID,''),
                   ISNULL(PAFILE.PA_LEGAL_NAME1,''),
                   ISNULL(PAFILE.PA_LEGAL_NAME2,''),
                   PAFILE.PA_VAT_PERC,
                   ISNULL(PAFILE.PA_RFC,''),
                   ISNULL(PAFILE.PA_CURP,''),
                   ISNULL(CAST(PAFILE.PA_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0')
            FROM DBO.PAFILE
            WHERE PAFILE.PA_BRNO >= :QPA1-WBEG-BRNO
              AND PAFILE.PA_BRNO <= :QPA1-WEND-BRNO
              AND PAFILE.PA_NUMBER >= :QPA1-WBEG-NUMBER
              AND PAFILE.PA_NUMBER <= :QPA1-WEND-NUMBER
            ORDER BY PAFILE.PA_BRNO,
                     PAFILE.PA_NUMBER
           END-EXEC.

           EXEC SQL
               OPEN PA1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QPA1-WBEG-BRNO 
                          QPA1-WEND-BRNO
                          QPA1-WBEG-NUMBER
                          QPA1-WEND-NUMBER.
 
           MOVE STAT---GOOD TO PA1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-PA1-FILE.
           PERFORM READ-IT.
           MOVE PA-PATH-OWNBR  TO PA-BRNO. 
           MOVE PA-SELECT-PATH TO E-FILE.
           MOVE PA1-KEY        TO E-KEYX.

           MOVE PA-BRNO   TO QPA-BRNO.
           MOVE PA-NUMBER TO QPA-NUMBER.

           EXEC SQL
            SELECT PAFILE.PA_BRNO,
                   PAFILE.PA_NUMBER,
                   ISNULL(PAFILE.PA_NAME,''),
                   ISNULL(PAFILE.PA_ADR1,''),
                   ISNULL(PAFILE.PA_ADR2,''),
                   ISNULL(PAFILE.PA_CITY,''),
                   ISNULL(PAFILE.PA_STATE,''),
                   ISNULL(PAFILE.PA_ZIP,''),
                   PAFILE.PA_LCHECK,
                   ISNULL(CAST(PAFILE.PA_LCDATE AS VARCHAR(10)),'0'),
                   PAFILE.PA_LAMT,
                   PAFILE.PA_YTD_PAID,
                   PAFILE.PA_LYTD_PAID,
                   ISNULL(PAFILE.PA_SORTKEY,''),
                   ISNULL(PAFILE.PA_CUSNO,''),
                   ISNULL(PAFILE.PA_CONTACT,''),
                   PAFILE.PA_PHONE,
                   PAFILE.PA_FAX,
                   ISNULL(PAFILE.PA_1099_FLAG,''),
                   ISNULL(PAFILE.PA_1099_ID,''),
                   ISNULL(PAFILE.PA_LEGAL_NAME1,''),
                   ISNULL(PAFILE.PA_LEGAL_NAME2,''),
                   PAFILE.PA_VAT_PERC,
                   ISNULL(PAFILE.PA_RFC,''),
                   ISNULL(PAFILE.PA_CURP,''),
                   ISNULL(CAST(PAFILE.PA_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0')
            INTO :QPA-REC
            FROM DBO.PAFILE
            WHERE PAFILE.PA_BRNO = :QPA-BRNO
              AND PAFILE.PA_NUMBER = :QPA-NUMBER
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-PA1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-PA-FIELDS.

      *-----------------------------------------------------------------
       READ-PA1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE PA-PATH-OWNBR  TO PA-BRNO.
           MOVE PA-SELECT-PATH TO E-FILE.
           MOVE PA1-KEY        TO E-KEYX.
           INITIALIZE QPA-REC.

           IF ( PA1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH PA1_CURSOR INTO :QPA-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-PA1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-PA-FIELDS.

      *-----------------------------------------------------------------
       WRITE-PA1-FILE.

           MOVE PA-PATH-OWNBR  TO PA-BRNO.
           ACCEPT PA-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE PA-SELECT-PATH TO E-FILE.
           MOVE PA1-KEY        TO E-KEYX.
           PERFORM SET-PA-FIELDS.

           EXEC SQL
            INSERT INTO DBO.PAFILE
             (PAFILE.PA_BRNO,
              PAFILE.PA_NUMBER,
              PAFILE.PA_NAME,
              PAFILE.PA_ADR1,
              PAFILE.PA_ADR2,
              PAFILE.PA_CITY,
              PAFILE.PA_STATE,
              PAFILE.PA_ZIP,
              PAFILE.PA_LCHECK,
              PAFILE.PA_LCDATE,
              PAFILE.PA_LAMT,
              PAFILE.PA_YTD_PAID,
              PAFILE.PA_LYTD_PAID,
              PAFILE.PA_SORTKEY,
              PAFILE.PA_CUSNO,
              PAFILE.PA_CONTACT,
              PAFILE.PA_PHONE,
              PAFILE.PA_FAX,
              PAFILE.PA_1099_FLAG,
              PAFILE.PA_1099_ID,
              PAFILE.PA_LEGAL_NAME1,
              PAFILE.PA_LEGAL_NAME2,
              PAFILE.PA_VAT_PERC,
              PAFILE.PA_RFC,
              PAFILE.PA_CURP,
              PAFILE.PA_LTOUCH_DATE)
            VALUES
             (:QPA-BRNO,
              :QPA-NUMBER,
              :QPA-NAME,
              :QPA-ADR1,
              :QPA-ADR2,
              :QPA-CITY,
              :QPA-STATE,
              :QPA-ZIP,
              :QPA-LCHECK,
              NULLIF(:QPA-LCDATE,'1900-01-01'),
              :QPA-LAMT,
              :QPA-YTD-PAID,
              :QPA-LYTD-PAID,
              :QPA-SORTKEY,
              :QPA-CUSNO,
              :QPA-CONTACT,
              :QPA-PHONE,
              :QPA-FAX,
              :QPA-1099-FLAG,
              :QPA-1099-ID,
              :QPA-LEGAL-NAME1,
              :QPA-LEGAL-NAME2,
              :QPA-VAT-PERC,
              :QPA-RFC,
              :QPA-CURP,
              NULLIF(:QPA-LTOUCH-DATE,'1900-01-01'))
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-PA1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-PA1-FILE.

           MOVE PA-PATH-OWNBR  TO PA-BRNO.
           ACCEPT PA-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE PA-SELECT-PATH TO E-FILE.
           MOVE PA1-KEY        TO E-KEYX.
           PERFORM SET-PA-FIELDS.

           EXEC SQL
            UPDATE DBO.PAFILE
             SET
              PAFILE.PA_BRNO = :QPA-BRNO,
              PAFILE.PA_NUMBER = :QPA-NUMBER,
              PAFILE.PA_NAME = :QPA-NAME,
              PAFILE.PA_ADR1 = :QPA-ADR1,
              PAFILE.PA_ADR2 = :QPA-ADR2,
              PAFILE.PA_CITY = :QPA-CITY,
              PAFILE.PA_STATE = :QPA-STATE,
              PAFILE.PA_ZIP = :QPA-ZIP,
              PAFILE.PA_LCHECK = :QPA-LCHECK,
              PAFILE.PA_LCDATE =
                 NULLIF(:QPA-LCDATE,'1900-01-01'),
              PAFILE.PA_LAMT = :QPA-LAMT,
              PAFILE.PA_YTD_PAID = :QPA-YTD-PAID,
              PAFILE.PA_LYTD_PAID = :QPA-LYTD-PAID,
              PAFILE.PA_SORTKEY = :QPA-SORTKEY,
              PAFILE.PA_CUSNO = :QPA-CUSNO,
              PAFILE.PA_CONTACT = :QPA-CONTACT,
              PAFILE.PA_PHONE = :QPA-PHONE,
              PAFILE.PA_FAX = :QPA-FAX,
              PAFILE.PA_1099_FLAG = :QPA-1099-FLAG,
              PAFILE.PA_1099_ID = :QPA-1099-ID,
              PAFILE.PA_LEGAL_NAME1 = :QPA-LEGAL-NAME1,
              PAFILE.PA_LEGAL_NAME2 = :QPA-LEGAL-NAME2,
              PAFILE.PA_VAT_PERC = :QPA-VAT-PERC,
              PAFILE.PA_RFC = :QPA-RFC,
              PAFILE.PA_CURP = :QPA-CURP,
              PAFILE.PA_LTOUCH_DATE =
                 NULLIF(:QPA-LTOUCH-DATE,'1900-01-01')
            WHERE PAFILE.PA_BRNO = :QPA-BRNO
              AND PAFILE.PA_NUMBER = :QPA-NUMBER
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-PA1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-PA1-FILE.

           MOVE PA-PATH-OWNBR  TO PA-BRNO.
           PERFORM DELETE-IT.
           MOVE PA-SELECT-PATH TO E-FILE.
           MOVE PA1-KEY        TO E-KEYX.
           PERFORM SET-PA-FIELDS.

           EXEC SQL
            DELETE FROM DBO.PAFILE
            WHERE PAFILE.PA_BRNO = :QPA-BRNO
              AND PAFILE.PA_NUMBER = :QPA-NUMBER
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-PA1-FILE.

      *-----------------------------------------------------------------
       CLOSE-PA1-FILE.

           IF ( PA1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE PA1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO PA1-CURSOR-STAT.

      *#################################################################
      *    ENDING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
 
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
      *
      *    E N D - P R O G R A M
      *
      ******************************************************************
       END-PROGRAM SECTION.
      *-----------------------------------------------------------------
       END-RUN.

           PERFORM CLOSE-FILES.

      *    MOVE WK1-PATH TO ACCESS-BUF.
      *    PERFORM REMOVE-WORKFILE.

           IF ( EXIT-PRIOR-TO-EXECUTION )
              GO TO EXIT-PROG.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-ROUTE-BUF = "00" )
              PERFORM PRU-OUT.

           IF ( NO-RECORDS-FOUND         ) AND
              ( EXT-ROUTE-BUF = "00" OR "70" )
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       EXIT-PROG.

           IF ( SV-BEG-BRANCH NOT = 9999 )
              IF ( EXT-EOBUF-GROUP-FG = "Y"  )
                 MOVE SV-BEG-BRANCH     TO BEG-BRANCH
                 MOVE SV-END-BRANCH     TO END-BRANCH
                 MOVE SV-GROUP-FLAG     TO GROUP-FLAG
                 MOVE SV-ENT-GROUP      TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BRANCH TO BEG-BRANCH
                 MOVE SV-END-BRANCH TO END-BRANCH.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE EXIT-PATH  TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

      *-----------------------------------------------------------------
       STOP-RUN.

           IF ( EXT-ROUTE-BUF = "00" OR "40" OR "50" )
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY IOPAIN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
