       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOICIN.
       AUTHOR.        AMM.
       DATE-WRITTEN.  05-12-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOICIN.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : ICFILE 
      * TABLE DESC    : INSURANCE COMPNAY FILE
      * OUTPUT FILE   : XPC/IOICIN.PRE
      * USED FOR      : LIBLP/LPWSIC.CPY  --- QIC-REC
      *                 LIBLP/LPIC1R.CPY  --- READ
      *                 LIBLP/LPIC1RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * AMM 2021-0512 ***NEW***
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
           "@(#)IO/IOICIN.CBL S34 09/29/21-11:22:27 >".

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
       COPY "LIBLP/LP01IC.CPY".
       COPY "LIBLP/LPWSIC.CPY".


      *=================================================================
      *
      *    Q - F I L E
      *
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QIC1-WHERE-BEG-KEY.
           03  QIC1-WBEG-BRNO             PIC 9(04).
           03  QIC1-WBEG-CODE             PIC 99.
 
       01  QIC1-WHERE-END-KEY.
           03  QIC1-WEND-BRNO             PIC 9(04).
           03  QIC1-WEND-CODE             PIC 99.
 
       01  QIC-REC.
           03  QIC-BRNO                 PIC 9(4).
           03  QIC-CODE                 PIC 99.
           03  QIC-LTOUCH-DATE          PIC X(10).
           03  QIC-NAME                 PIC X(20).
           03  QIC-ADDR1                PIC X(25).
           03  QIC-ADDR2                PIC X(25).
           03  QIC-CITY                 PIC X(15).
           03  QIC-STATE                PIC XX.
           03  QIC-ZIP                  PIC X(10).
           03  QIC-PHONE                PIC 9(10).
           03  QIC-COMMRATE-1           PIC S9(3)V99.
           03  QIC-COMMRATE-2           PIC S9(3)V99.
           03  QIC-COMMRATE-3           PIC S9(3)V99.
           03  QIC-COMMRATE-4           PIC S9(3)V99.
           03  QIC-COMMRATE-5           PIC S9(3)V99.
           03  QIC-COMMRATE-6           PIC S9(3)V99.
           03  QIC-COMMRATE-7           PIC S9(3)V99.
           03  QIC-COMMRATE-8           PIC S9(3)V99.
           03  QIC-COMMRATE-9           PIC S9(3)V99.
           03  QIC-COMMRATE-10          PIC S9(3)V99.
           03  QIC-COMMRATE-11          PIC S9(3)V99.
           03  QIC-COMMRATE-12          PIC S9(3)V99.
           03  QIC-COMMRATE-13          PIC S9(3)V99.
           03  QIC-COMMRATE-14          PIC S9(3)V99.
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
           03  BEG-CODE                PIC 9(02).
           03  END-CODE                PIC 9(02).

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
           03  FILLER PIC X(29) VALUE "BRNO   CO  NAME              ".
           03  FILLER PIC X(30) VALUE "ST    RATE".

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.


      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-IC-BRNO                    PIC 9(04).
           03  FILLER                         PIC X(03).
           03  D01-IC-CODE                    PIC 9(02).
           03  FILLER                         PIC X(03).
           03  D01-IC-NAME                    PIC X(15).
           03  FILLER                         PIC X(03).
           03  D01-IC-STATE                   PIC XX.
           03  FILLER                         PIC X(03).
           03  D01-IC-COMMRATE-1              PIC 999.99.

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
       COPY "LIBIO/IOICIN_DEF.CPY".
       COPY "LIBIO/IOICIN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOICIN_SCN.CPY".

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
           MOVE "IOICIN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOICIN-QUEUE-POS  TO Q-POS.
           MOVE IOICIN-COPIES-POS TO C-POS.
           MOVE IOICIN-PRU-POS    TO P-POS.
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
              GO TO ENTER-END-CODE.

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
       ENTER-BEG-CODE.

           MOVE "ENNN020BEG-CODE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PROCESS-KEY.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-CODE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-CODE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-CODE.

      *-----------------------------------------------------------------
       ENTER-END-CODE.

           MOVE "ENNN020END-CODE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-CODE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-CODE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-CODE.

           IF ( BEG-CODE > END-CODE )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-CODE.

           GO TO ENTER-ACCEPT-YN.

      *-----------------------------------------------------------------
       ENTER-ALL-CODE.

           MOVE "SNNN020BEG-CODE." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-CODE.
           PERFORM SCREENIO.

           MOVE "SNNN020END-CODE." TO VDU.
           MOVE 99                 TO VDUNUM0  END-CODE.
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

           MOVE "SNNN020BEG-CODE."       TO VDU.
           MOVE BEG-CODE                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020END-CODE."       TO VDU.
           MOVE END-CODE                 TO VDUNUM0.
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

           MOVE "B"                        TO IC-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - -{ SET WHERE BEG & END FIELDS }
           MOVE BR-NO     TO QIC1-WBEG-BRNO.
           MOVE BR-NO     TO QIC1-WEND-BRNO.

           MOVE BEG-CODE  TO QIC1-WBEG-CODE.
           MOVE END-CODE  TO QIC1-WEND-CODE.

      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           PERFORM OPEN-IC1-FILE.
           PERFORM START-IC1-FILE.

      *=================================================================
       PROCESS-BRANCH-NEXT.

           PERFORM READ-IC1-FILE-NEXT.
           IF ( IO-BAD ) OR ( IC-BRNO NOT = IC-PATH-OWNBR )
              GO TO PROCESS-BRANCH-MODULE-END.

           ADD 01 TO RECORDS-READ.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

           IF ( IC-CODE < BEG-CODE )
              GO TO PROCESS-BRANCH-NEXT
           ELSE
           IF ( IC-CODE > END-CODE )
              GO TO PROCESS-BRANCH-MODULE-END.

      *-----------------------------------------------------------------
       PROCESS-BRANCH-PRINT.

           MOVE IC-BRNO            TO D01-IC-BRNO.
           MOVE IC-CODE            TO D01-IC-CODE.
           MOVE IC-NAME             TO D01-IC-NAME.
           MOVE IC-STATE            TO D01-IC-STATE.
           MOVE IC-COMMRATE(1)      TO D01-IC-COMMRATE-1.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.
           GO TO PROCESS-BRANCH-NEXT.

      *=================================================================
       PROCESS-BRANCH-MODULE-END.

           PERFORM CLOSE-IC1-FILE.

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

           MOVE IC1-KEY TO PACE-DETAIL-KEY.

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
           MOVE "BEGINNING CODE               : "    TO R01-PROMPT.
           MOVE BEG-CODE                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    CODE               : "    TO R01-PROMPT.
           MOVE END-CODE                             TO R01-NUMERIC.
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
           MOVE "SQL TABLE IO UTILITY - ICFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOICIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOICIN-SYSDATE.
           DISPLAY IOICIN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOICIN-FORM-FIELDS TO WR-BUF.
           MOVE IOICIN-FORM-LIST TO WR-LIST.
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

           MOVE IOICIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOICIN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOICIN_EVA.CPY".
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
      *    LOAD IC-REC FROM QIC-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-IC-FIELDS.
           MOVE QIC-BRNO TO IC-BRNO.
           MOVE QIC-CODE TO IC-CODE.

           IF ( QIC-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO IC-LTOUCH-DATE
           ELSE
              MOVE QIC-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO IC-LTOUCH-DATE.

           MOVE QIC-NAME TO IC-NAME.
           MOVE QIC-ADDR1 TO IC-ADDR1.
           MOVE QIC-ADDR2 TO IC-ADDR2.
           MOVE QIC-CITY TO IC-CITY.
           MOVE QIC-STATE TO IC-STATE.
           MOVE QIC-ZIP TO IC-ZIP.
           MOVE QIC-PHONE TO IC-PHONE.
           MOVE QIC-COMMRATE-1 TO IC-COMMRATE(1).
           MOVE QIC-COMMRATE-2 TO IC-COMMRATE(2).
           MOVE QIC-COMMRATE-3 TO IC-COMMRATE(3).
           MOVE QIC-COMMRATE-4 TO IC-COMMRATE(4).
           MOVE QIC-COMMRATE-5 TO IC-COMMRATE(5).
           MOVE QIC-COMMRATE-6 TO IC-COMMRATE(6).
           MOVE QIC-COMMRATE-7 TO IC-COMMRATE(7).
           MOVE QIC-COMMRATE-8 TO IC-COMMRATE(8).
           MOVE QIC-COMMRATE-9 TO IC-COMMRATE(9).
           MOVE QIC-COMMRATE-10 TO IC-COMMRATE(10).
           MOVE QIC-COMMRATE-11 TO IC-COMMRATE(11).
           MOVE QIC-COMMRATE-12 TO IC-COMMRATE(12).
           MOVE QIC-COMMRATE-13 TO IC-COMMRATE(13).
           MOVE QIC-COMMRATE-14 TO IC-COMMRATE(14).
 
      *-----------------------------------------------------------------
      *    LOAD QIC-REC FROM IC-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-IC-FIELDS.
           MOVE IC-BRNO TO QIC-BRNO.
           MOVE IC-CODE TO QIC-CODE.

           IF ( IC-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QIC-LTOUCH-DATE
           ELSE
              MOVE IC-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QIC-LTOUCH-DATE.

           MOVE IC-NAME TO QIC-NAME.
           MOVE IC-ADDR1 TO QIC-ADDR1.
           MOVE IC-ADDR2 TO QIC-ADDR2.
           MOVE IC-CITY TO QIC-CITY.
           MOVE IC-STATE TO QIC-STATE.
           MOVE IC-ZIP TO QIC-ZIP.
           MOVE IC-PHONE TO QIC-PHONE.
           MOVE IC-COMMRATE(1) TO QIC-COMMRATE-1.
           MOVE IC-COMMRATE(2) TO QIC-COMMRATE-2.
           MOVE IC-COMMRATE(3) TO QIC-COMMRATE-3.
           MOVE IC-COMMRATE(4) TO QIC-COMMRATE-4.
           MOVE IC-COMMRATE(5) TO QIC-COMMRATE-5.
           MOVE IC-COMMRATE(6) TO QIC-COMMRATE-6.
           MOVE IC-COMMRATE(7) TO QIC-COMMRATE-7.
           MOVE IC-COMMRATE(8) TO QIC-COMMRATE-8.
           MOVE IC-COMMRATE(9) TO QIC-COMMRATE-9.
           MOVE IC-COMMRATE(10) TO QIC-COMMRATE-10.
           MOVE IC-COMMRATE(11) TO QIC-COMMRATE-11.
           MOVE IC-COMMRATE(12) TO QIC-COMMRATE-12.
           MOVE IC-COMMRATE(13) TO QIC-COMMRATE-13.
           MOVE IC-COMMRATE(14) TO QIC-COMMRATE-14.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-IC1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( IC-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( IC-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO IC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO IC-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO IC-ALL-DATA
              MOVE IC-ALL-PATH             TO IC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( IC-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO IC-ALL-BASE
              MOVE FILPATH-MACHINE         TO IC-ALL-MACHINE
              MOVE FILPATH-DATA            TO IC-ALL-DATA
              MOVE IC-ALL-PATH             TO IC-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-ICFILE NOT = "S" )
              MOVE IC-PATH-BEG             TO IC-SELECT-PATH-BEG
              MOVE IC-PATH-FILE            TO IC-SELECT-PATH-FILE
           ELSE
              MOVE IC-PATH-SQL             TO IC-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-IC1-FILE.
           PERFORM LOAD-IC1-FILE.
           PERFORM OPEN-IT.
           MOVE IC-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-IC1-FILE.
           PERFORM START-IT.
           MOVE IC-PATH-OWNBR  TO IC-BRNO.
           MOVE IC-SELECT-PATH TO E-FILE.
           MOVE IC1-KEY        TO E-KEYX.

           IF ( IC1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-IC1-FILE.

           IF ( QIC1-WEND-CODE = ZEROES    ) OR
              ( QIC1-WEND-CODE NOT NUMERIC )
              MOVE IC-PATH-OWNBR        TO QIC1-WBEG-BRNO
              MOVE IC-PATH-OWNBR        TO QIC1-WEND-BRNO
              MOVE IC-CODE              TO QIC1-WBEG-CODE
              MOVE ALL "9"              TO QIC1-WEND-CODE.

           EXEC SQL
            DECLARE IC1_CURSOR CURSOR FOR
            SELECT ICFILE.IC_BRNO,
                   ICFILE.IC_CODE,
                   ISNULL(CAST(ICFILE.IC_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(ICFILE.IC_NAME,''),
                   ISNULL(ICFILE.IC_ADDR1,''),
                   ISNULL(ICFILE.IC_ADDR2,''),
                   ISNULL(ICFILE.IC_CITY,''),
                   ISNULL(ICFILE.IC_STATE,''),
                   ISNULL(ICFILE.IC_ZIP,''),
                   ICFILE.IC_PHONE,
                   ICFILE.IC_COMMRATE_1,
                   ICFILE.IC_COMMRATE_2,
                   ICFILE.IC_COMMRATE_3,
                   ICFILE.IC_COMMRATE_4,
                   ICFILE.IC_COMMRATE_5,
                   ICFILE.IC_COMMRATE_6,
                   ICFILE.IC_COMMRATE_7,
                   ICFILE.IC_COMMRATE_8,
                   ICFILE.IC_COMMRATE_9,
                   ICFILE.IC_COMMRATE_10,
                   ICFILE.IC_COMMRATE_11,
                   ICFILE.IC_COMMRATE_12,
                   ICFILE.IC_COMMRATE_13,
                   ICFILE.IC_COMMRATE_14
            FROM DBO.ICFILE
            WHERE ICFILE.IC_BRNO >= :QIC1-WBEG-BRNO
              AND ICFILE.IC_BRNO <= :QIC1-WEND-BRNO
              AND ICFILE.IC_CODE >= :QIC1-WBEG-CODE
              AND ICFILE.IC_CODE <= :QIC1-WEND-CODE
            ORDER BY ICFILE.IC_BRNO,
                     ICFILE.IC_CODE
           END-EXEC.

           EXEC SQL
               OPEN IC1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QIC1-WBEG-BRNO 
                          QIC1-WEND-BRNO
                          QIC1-WBEG-CODE
                          QIC1-WEND-CODE.

           MOVE STAT---GOOD TO IC1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-IC1-FILE.
           PERFORM READ-IT.
           MOVE IC-PATH-OWNBR  TO IC-BRNO. 
           MOVE IC-SELECT-PATH TO E-FILE.
           MOVE IC1-KEY        TO E-KEYX.

           MOVE IC-BRNO   TO QIC-BRNO.
           MOVE IC-CODE   TO QIC-CODE.

           EXEC SQL
            SELECT ICFILE.IC_BRNO,
                   ICFILE.IC_CODE,
                   ISNULL(CAST(ICFILE.IC_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(ICFILE.IC_NAME,''),
                   ISNULL(ICFILE.IC_ADDR1,''),
                   ISNULL(ICFILE.IC_ADDR2,''),
                   ISNULL(ICFILE.IC_CITY,''),
                   ISNULL(ICFILE.IC_STATE,''),
                   ISNULL(ICFILE.IC_ZIP,''),
                   ICFILE.IC_PHONE,
                   ICFILE.IC_COMMRATE_1,
                   ICFILE.IC_COMMRATE_2,
                   ICFILE.IC_COMMRATE_3,
                   ICFILE.IC_COMMRATE_4,
                   ICFILE.IC_COMMRATE_5,
                   ICFILE.IC_COMMRATE_6,
                   ICFILE.IC_COMMRATE_7,
                   ICFILE.IC_COMMRATE_8,
                   ICFILE.IC_COMMRATE_9,
                   ICFILE.IC_COMMRATE_10,
                   ICFILE.IC_COMMRATE_11,
                   ICFILE.IC_COMMRATE_12,
                   ICFILE.IC_COMMRATE_13,
                   ICFILE.IC_COMMRATE_14
            INTO :QIC-REC
            FROM DBO.ICFILE
            WHERE ICFILE.IC_BRNO = :QIC-BRNO
              AND ICFILE.IC_CODE = :QIC-CODE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-IC1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-IC-FIELDS.

      *-----------------------------------------------------------------
       READ-IC1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE IC-PATH-OWNBR  TO IC-BRNO.
           MOVE IC-SELECT-PATH TO E-FILE.
           MOVE IC1-KEY        TO E-KEYX.
           INITIALIZE QIC-REC.

           IF ( IC1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH IC1_CURSOR INTO :QIC-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-IC1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-IC-FIELDS.

      *-----------------------------------------------------------------
       WRITE-IC1-FILE.

           MOVE IC-PATH-OWNBR  TO IC-BRNO.
           ACCEPT IC-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE IC-SELECT-PATH TO E-FILE.
           MOVE IC1-KEY        TO E-KEYX.
           PERFORM SET-IC-FIELDS.

           EXEC SQL
            INSERT INTO DBO.ICFILE
             (ICFILE.IC_BRNO,
              ICFILE.IC_CODE,
              ICFILE.IC_LTOUCH_DATE,
              ICFILE.IC_NAME,
              ICFILE.IC_ADDR1,
              ICFILE.IC_ADDR2,
              ICFILE.IC_CITY,
              ICFILE.IC_STATE,
              ICFILE.IC_ZIP,
              ICFILE.IC_PHONE,
              ICFILE.IC_COMMRATE_1,
              ICFILE.IC_COMMRATE_2,
              ICFILE.IC_COMMRATE_3,
              ICFILE.IC_COMMRATE_4,
              ICFILE.IC_COMMRATE_5,
              ICFILE.IC_COMMRATE_6,
              ICFILE.IC_COMMRATE_7,
              ICFILE.IC_COMMRATE_8,
              ICFILE.IC_COMMRATE_9,
              ICFILE.IC_COMMRATE_10,
              ICFILE.IC_COMMRATE_11,
              ICFILE.IC_COMMRATE_12,
              ICFILE.IC_COMMRATE_13,
              ICFILE.IC_COMMRATE_14)
            VALUES
             (:QIC-BRNO,
              :QIC-CODE,
              NULLIF(:QIC-LTOUCH-DATE,'1900-01-01'),
              :QIC-NAME,
              :QIC-ADDR1,
              :QIC-ADDR2,
              :QIC-CITY,
              :QIC-STATE,
              :QIC-ZIP,
              :QIC-PHONE,
              :QIC-COMMRATE-1,
              :QIC-COMMRATE-2,
              :QIC-COMMRATE-3,
              :QIC-COMMRATE-4,
              :QIC-COMMRATE-5,
              :QIC-COMMRATE-6,
              :QIC-COMMRATE-7,
              :QIC-COMMRATE-8,
              :QIC-COMMRATE-9,
              :QIC-COMMRATE-10,
              :QIC-COMMRATE-11,
              :QIC-COMMRATE-12,
              :QIC-COMMRATE-13,
              :QIC-COMMRATE-14)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-IC1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-IC1-FILE.

           MOVE IC-PATH-OWNBR  TO IC-BRNO.
           ACCEPT IC-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE IC-SELECT-PATH TO E-FILE.
           MOVE IC1-KEY        TO E-KEYX.
           PERFORM SET-IC-FIELDS.

           EXEC SQL
            UPDATE DBO.ICFILE
             SET
              ICFILE.IC_BRNO = :QIC-BRNO,
              ICFILE.IC_CODE = :QIC-CODE,
              ICFILE.IC_LTOUCH_DATE =
                 NULLIF(:QIC-LTOUCH-DATE,'1900-01-01'),
              ICFILE.IC_NAME = :QIC-NAME,
              ICFILE.IC_ADDR1 = :QIC-ADDR1,
              ICFILE.IC_ADDR2 = :QIC-ADDR2,
              ICFILE.IC_CITY = :QIC-CITY,
              ICFILE.IC_STATE = :QIC-STATE,
              ICFILE.IC_ZIP = :QIC-ZIP,
              ICFILE.IC_PHONE = :QIC-PHONE,
              ICFILE.IC_COMMRATE_1 = :QIC-COMMRATE-1,
              ICFILE.IC_COMMRATE_2 = :QIC-COMMRATE-2,
              ICFILE.IC_COMMRATE_3 = :QIC-COMMRATE-3,
              ICFILE.IC_COMMRATE_4 = :QIC-COMMRATE-4,
              ICFILE.IC_COMMRATE_5 = :QIC-COMMRATE-5,
              ICFILE.IC_COMMRATE_6 = :QIC-COMMRATE-6,
              ICFILE.IC_COMMRATE_7 = :QIC-COMMRATE-7,
              ICFILE.IC_COMMRATE_8 = :QIC-COMMRATE-8,
              ICFILE.IC_COMMRATE_9 = :QIC-COMMRATE-9,
              ICFILE.IC_COMMRATE_10 = :QIC-COMMRATE-10,
              ICFILE.IC_COMMRATE_11 = :QIC-COMMRATE-11,
              ICFILE.IC_COMMRATE_12 = :QIC-COMMRATE-12,
              ICFILE.IC_COMMRATE_13 = :QIC-COMMRATE-13,
              ICFILE.IC_COMMRATE_14 = :QIC-COMMRATE-14
             WHERE ICFILE.IC_BRNO = :QIC-BRNO
               AND ICFILE.IC_CODE = :QIC-CODE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-IC1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-IC1-FILE.

           MOVE IC-PATH-OWNBR  TO IC-BRNO.
           PERFORM DELETE-IT.
           MOVE IC-SELECT-PATH TO E-FILE.
           MOVE IC1-KEY        TO E-KEYX.
           PERFORM SET-IC-FIELDS.

           EXEC SQL
            DELETE FROM DBO.ICFILE
             WHERE ICFILE.IC_BRNO = :QIC-BRNO
               AND ICFILE.IC_CODE = :QIC-CODE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-IC1-FILE.

      *-----------------------------------------------------------------
       CLOSE-IC1-FILE.

           IF ( IC1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE IC1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO IC1-CURSOR-STAT.

      *#################################################################
      *    ENDING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
 
      *=================================================================
      *=================================================================
      *=================================================================
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
              DESTROY IOICIN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
