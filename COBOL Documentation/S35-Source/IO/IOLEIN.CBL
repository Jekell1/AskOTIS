       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOLEIN.
       AUTHOR.        AMM.
       DATE-WRITTEN.  05-12-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOLEIN.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : LEFILE 
      * TABLE DESC    : LOAN ENDORSER FILE
      * OUTPUT FILE   : XPC/IOLEIN.PRE
      * USED FOR      : LIBLP/LPWSLE.CPY  --- QLE-REC
      *                 LIBLP/LPLE1R.CPY  --- READ
      *                 LIBLP/LPLE1RN.CPY --- READ/READ-NEXT/START
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
           "@(#)IO/IOLEIN.CBL S34 05/20/21-09:17:22 >".

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
       COPY "LIBLP/LP01LE.CPY".
       COPY "LIBLP/LPWSLE.CPY".

      *=================================================================
      *
      *    Q - F I L E
      *
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QLE1-WHERE-BEG-KEY.
           03  QLE1-WBEG-BRNO            PIC 9(04).
           03  QLE1-WBEG-ACCTNO          PIC 9(08).
           03  QLE1-WBEG-TYPE            PIC X.
           03  QLE1-WBEG-FLAG            PIC X.
           03  QLE1-WBEG-SSNO            PIC 9(09).
           03  QLE1-WBEG-OSACCTNO        PIC 9(08).
       01  QLE1-WHERE-END-KEY.
           03  QLE1-WEND-BRNO            PIC 9(04).
           03  QLE1-WEND-ACCTNO          PIC 9(08).
           03  QLE1-WEND-TYPE            PIC X.
           03  QLE1-WEND-FLAG            PIC X.
           03  QLE1-WEND-SSNO            PIC 9(09).
           03  QLE1-WEND-OSACCTNO        PIC 9(08).
 
 
       01  QLE-REC.
           03  QLE-BRNO             PIC 9(4).
           03  QLE-ACCTNO           PIC 9(8).
           03  QLE-TYPE             PIC X.
           03  QLE-FLAG             PIC X.
           03  QLE-SSNO             PIC 9(9).
           03  QLE-OSACCTNO         PIC 9(8).
           03  QLE-LTOUCH-DATE      PIC X(10).
           03  QLE-E-OSRATE             PIC 999V999.
           03  QLE-E-OSAMT              PIC 9(7)V99.
           03  QLE-E-OSDATE             PIC X(10).
           03  QLE-E-AMT                PIC 9(7)V99.
           03  QLE-E-DATE               PIC X(10).
           03  QLE-E-OSMAKERCD          PIC X.
           03  QLE-R-LOANDATE           PIC X(10).
           03  QLE-R-LNAMT              PIC 9(7)V99.
           03  QLE-R-POFFDATE           PIC X(10).
           03  QLE-R-RESTRICTED-MONEY   PIC 9(7)V99.
           03  QLE-R-BALANCE            PIC 9(7)V99.
           03  QLE-R-SALEFIN            PIC X.

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
           03  BEG-ACCTNO              PIC 9(08).
           03  END-ACCTNO              PIC 9(08).
           03  BEG-TYPE                PIC X.
           03  END-TYPE                PIC X.
           03  BEG-FLAG                PIC X.
           03  END-FLAG                PIC X.
           03  BEG-SSNO                PIC 9(09).
           03  END-SSNO                PIC 9(09).
           03  BEG-OSACCTNO             PIC 9(08).
           03  END-OSACCTNO             PIC 9(08).

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
           03  FILLER PIC X(28) VALUE "BRNO     ACCTNO TYPE FLAG   ".
           03  FILLER PIC X(31) VALUE "SSNO        OSACCTNO     E-DATE".
           03  FILLER PIC X(30) VALUE "       E-AMT    R-LOANDATE".

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-LE-BRNO                    PIC 9(04).
           03  FILLER                         PIC X(03).
           03  D01-LE-ACCTNO                  PIC 9(08).
           03  FILLER                         PIC X(03).
           03  D01-LE-TYPE                    PIC X.
           03  FILLER                         PIC X(03).
           03  D01-LE-FLAG                    PIC X.
           03  FILLER                         PIC X(03).
           03  D01-LE-SSNO                    PIC 999/99/9999.
           03  FILLER                         PIC X(03).
           03  D01-LE-OSACCTNO                PIC 9(08).
           03  FILLER                         PIC X(03).
           03  D01-LE-E-DATE                  PIC 9999/99/99.
           03  FILLER                         PIC X(03).
           03  D01-LE-E-AMT                   PIC Z(6)9.99.
           03  FILLER                         PIC X(03).
           03  D01-LE-R-LOANDATE              PIC 9999/99/99.

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
       COPY "LIBIO/IOLEIN_DEF.CPY".
       COPY "LIBIO/IOLEIN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOLEIN_SCN.CPY".

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
           MOVE "IOLEIN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOLEIN-QUEUE-POS  TO Q-POS.
           MOVE IOLEIN-COPIES-POS TO C-POS.
           MOVE IOLEIN-PRU-POS    TO P-POS.
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
              GO TO ENTER-END-OSACCTNO.

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
       ENTER-BEG-ACCTNO.

           MOVE "ENNN080BEG-ACCTNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PROCESS-KEY.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-ACCTNO.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-ACCTNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-ACCTNO.

      *-----------------------------------------------------------------
       ENTER-END-ACCTNO.

           MOVE "ENNN080END-ACCTNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-ACCTNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-ACCTNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-ACCTNO.

           IF ( BEG-ACCTNO > END-ACCTNO )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-ACCTNO.

           GO TO ENTER-BEG-TYPE.

      *-----------------------------------------------------------------
       ENTER-ALL-ACCTNO.

           MOVE "SNNN080BEG-ACCTNO." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-ACCTNO.
           PERFORM SCREENIO.

           MOVE "SNNN080END-ACCTNO." TO VDU.
           MOVE 99999999            TO VDUNUM0  END-ACCTNO.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       ENTER-BEG-TYPE.

           MOVE "E  A010BEG-TYPE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-ACCTNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-TYPE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-TYPE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-TYPE.

      *-----------------------------------------------------------------
       ENTER-END-TYPE.

           MOVE "E  A010END-TYPE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-TYPE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-TYPE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-TYPE.

           IF ( BEG-TYPE > END-TYPE )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-TYPE.

           GO TO ENTER-BEG-FLAG.

      *-----------------------------------------------------------------
       ENTER-ALL-TYPE.

           MOVE "S  A010BEG-TYPE." TO VDU.
           MOVE SPACES              TO VDUBUF  BEG-TYPE.
           PERFORM SCREENIO.

           MOVE "S  A010END-TYPE." TO VDU.
           MOVE "Z"            TO VDUBUF  END-TYPE.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-FLAG.

           MOVE "E  A010BEG-FLAG." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-TYPE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-FLAG.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-FLAG.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-FLAG.

      *-----------------------------------------------------------------
       ENTER-END-FLAG.

           MOVE "E  A010END-FLAG." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-FLAG.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-FLAG.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-FLAG.

           IF ( BEG-FLAG > END-FLAG )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-FLAG.

           GO TO ENTER-BEG-SSNO.

      *-----------------------------------------------------------------
       ENTER-ALL-FLAG.

           MOVE "S  A010BEG-FLAG." TO VDU.
           MOVE SPACES              TO VDUBUF  BEG-FLAG.
           PERFORM SCREENIO.

           MOVE "S  A010END-FLAG." TO VDU.
           MOVE "Z"            TO VDUBUF  END-FLAG.
           PERFORM SCREENIO.
      *-----------------------------------------------------------------
       ENTER-BEG-SSNO.

           MOVE "ERNX090BEG-SSNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-FLAG.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-SSNO.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-SSNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-SSNO.

      *-----------------------------------------------------------------
       ENTER-END-SSNO.

           MOVE "ERNX090END-SSNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-SSNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-SSNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-SSNO.

           IF ( BEG-SSNO > END-SSNO )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-SSNO.

           GO TO ENTER-BEG-OSACCTNO.

      *-----------------------------------------------------------------
       ENTER-ALL-SSNO.

           MOVE "SRNX090BEG-SSNO." TO VDU.
           MOVE ZEROES             TO VDU-SSNO-EDIT BEG-SSNO.
           PERFORM SCREENIO.

           MOVE "SRNX090END-SSNO." TO VDU.
           MOVE 999999999          TO VDU-SSNO-EDIT END-SSNO.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-OSACCTNO.

           MOVE "ENNN080BEG-OSACCTNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PROCESS-KEY.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-OSACCTNO.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-OSACCTNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-OSACCTNO.

      *-----------------------------------------------------------------
       ENTER-END-OSACCTNO.

           MOVE "ENNN080END-OSACCTNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-OSACCTNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-OSACCTNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-OSACCTNO.

           IF ( BEG-OSACCTNO > END-OSACCTNO )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-OSACCTNO.

           GO TO ENTER-ACCEPT-YN.

      *-----------------------------------------------------------------
       ENTER-ALL-OSACCTNO.

           MOVE "SNNN080BEG-OSACCTNO." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-OSACCTNO.
           PERFORM SCREENIO.

           MOVE "SNNN080END-OSACCTNO." TO VDU.
           MOVE 99999999            TO VDUNUM0  END-OSACCTNO.
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


           MOVE "SNNN080BEG-ACCTNO."      TO VDU.
           MOVE BEG-ACCTNO                TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN080END-ACCTNO."      TO VDU.
           MOVE END-ACCTNO                TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A010BEG-TYPE."       TO VDU.
           MOVE BEG-TYPE                 TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010END-TYPE."       TO VDU.
           MOVE END-TYPE                 TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010BEG-FLAG."       TO VDU.
           MOVE BEG-FLAG                 TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A010END-FLAG."       TO VDU.
           MOVE END-FLAG                 TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SRNX090BEG-SSNO."        TO VDU.
           MOVE BEG-SSNO                  TO VDU-SSNO-EDIT.
           PERFORM SCREENIO.
           MOVE "SRNX090END-SSNO."        TO VDU.
           MOVE END-SSNO                  TO VDU-SSNO-EDIT.
           PERFORM SCREENIO.

           MOVE "SNNN080BEG-OSACCTNO."      TO VDU.
           MOVE BEG-OSACCTNO                TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN080END-OSACCTNO."      TO VDU.
           MOVE END-OSACCTNO                TO VDUNUM0.
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

           MOVE "B"                        TO LE-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - -{ SET WHERE BEG & END FIELDS }
           MOVE BR-NO     TO QLE1-WBEG-BRNO.
           MOVE BR-NO     TO QLE1-WEND-BRNO.

           MOVE BEG-ACCTNO TO QLE1-WBEG-ACCTNO.
           MOVE END-ACCTNO TO QLE1-WEND-ACCTNO.

           MOVE BEG-TYPE  TO QLE1-WBEG-TYPE.
           MOVE END-TYPE  TO QLE1-WEND-TYPE.
           MOVE BEG-FLAG  TO QLE1-WBEG-FLAG.
           MOVE END-FLAG  TO QLE1-WEND-FLAG.
           MOVE BEG-SSNO  TO QLE1-WBEG-SSNO.
           MOVE END-SSNO  TO QLE1-WEND-SSNO.
           MOVE BEG-OSACCTNO  TO QLE1-WBEG-OSACCTNO.
           MOVE END-OSACCTNO  TO QLE1-WEND-OSACCTNO.

      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           PERFORM OPEN-LE1-FILE.
           PERFORM START-LE1-FILE.

      *=================================================================
       PROCESS-BRANCH-NEXT.

           PERFORM READ-LE1-FILE-NEXT.
           IF ( IO-BAD ) OR ( LE-BRNO NOT = LE-PATH-OWNBR )
              GO TO PROCESS-BRANCH-MODULE-END.

           ADD 01 TO RECORDS-READ.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

           IF ( LE-ACCTNO < BEG-ACCTNO )
              GO TO PROCESS-BRANCH-NEXT
           ELSE
           IF ( LE-ACCTNO > END-ACCTNO )
              GO TO PROCESS-BRANCH-MODULE-END.

           IF LE-TYPE < BEG-TYPE OR > END-TYPE
              GO TO PROCESS-BRANCH-NEXT.
           IF LE-FLAG < BEG-FLAG OR > END-FLAG
              GO TO PROCESS-BRANCH-NEXT.
           IF LE-SSNO < BEG-SSNO OR > END-SSNO
              GO TO PROCESS-BRANCH-NEXT.
           IF LE-OSACCTNO < BEG-OSACCTNO OR > END-OSACCTNO
              GO TO PROCESS-BRANCH-NEXT.

      *-----------------------------------------------------------------
       PROCESS-BRANCH-PRINT.

           MOVE LE-BRNO                    TO D01-LE-BRNO.
           MOVE LE-ACCTNO                  TO D01-LE-ACCTNO.
           MOVE LE-TYPE                   TO D01-LE-TYPE.
           MOVE LE-FLAG                   TO D01-LE-FLAG.
           MOVE LE-SSNO                    TO D01-LE-SSNO.
           MOVE LE-OSACCTNO               TO D01-LE-OSACCTNO.
           IF LE-TYPE = "E"
              MOVE LE-E-DATE              TO D01-LE-E-DATE
              MOVE LE-E-AMT               TO D01-LE-E-AMT.
           IF LE-TYPE = "R"
              MOVE LE-R-LOANDATE    TO D01-LE-R-LOANDATE.

           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.
           GO TO PROCESS-BRANCH-NEXT.

      *=================================================================
       PROCESS-BRANCH-MODULE-END.

           PERFORM CLOSE-LE1-FILE.

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

           MOVE LE1-KEY TO PACE-DETAIL-KEY.

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
           MOVE "BEGINNING ACCTNO              : "    TO R01-PROMPT.
           MOVE BEG-ACCTNO                            TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    ACCTNO              : "    TO R01-PROMPT.
           MOVE END-ACCTNO                            TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING TYPE               : "    TO R01-PROMPT.
           MOVE BEG-TYPE                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    TYPE               : "    TO R01-PROMPT.
           MOVE END-TYPE                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING FLAG               : "    TO R01-PROMPT.
           MOVE BEG-FLAG                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    FLAG               : "    TO R01-PROMPT.
           MOVE END-FLAG                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING SSNO                : "    TO R01-PROMPT.
           MOVE BEG-SSNO                              TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    SSNO                : "    TO R01-PROMPT.
           MOVE END-SSNO                              TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING OSACCTNO            : "    TO R01-PROMPT.
           MOVE BEG-OSACCTNO                          TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    OSACCTNO            : "    TO R01-PROMPT.
           MOVE END-OSACCTNO                          TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

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
           MOVE "SQL TABLE IO UTILITY - LEFILEFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOLEIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOLEIN-SYSDATE.
           DISPLAY IOLEIN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOLEIN-FORM-FIELDS TO WR-BUF.
           MOVE IOLEIN-FORM-LIST TO WR-LIST.
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

           MOVE IOLEIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOLEIN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOLEIN_EVA.CPY".
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
      *    LOAD LE-REC FROM QLE-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-LE-FIELDS.
           MOVE QLE-BRNO TO LE-BRNO.
           MOVE QLE-ACCTNO TO LE-ACCTNO.
           MOVE QLE-TYPE TO LE-TYPE.
           MOVE QLE-FLAG TO LE-FLAG.
           MOVE QLE-SSNO TO LE-SSNO.
           MOVE QLE-OSACCTNO TO LE-OSACCTNO.

           IF ( QLE-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO LE-LTOUCH-DATE
           ELSE
              MOVE QLE-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LE-LTOUCH-DATE.

           MOVE QLE-E-OSRATE TO LE-E-OSRATE.
           MOVE QLE-E-OSAMT TO LE-E-OSAMT.

           IF ( QLE-E-OSDATE = "1900-01-01" )
              MOVE ZEROES TO LE-E-OSDATE
           ELSE
              MOVE QLE-E-OSDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LE-E-OSDATE.

           MOVE QLE-E-AMT TO LE-E-AMT.

           IF ( QLE-E-DATE = "1900-01-01" )
              MOVE ZEROES TO LE-E-DATE
           ELSE
              MOVE QLE-E-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LE-E-DATE.

           MOVE QLE-E-OSMAKERCD TO LE-E-OSMAKERCD.

           IF ( QLE-R-LOANDATE = "1900-01-01" )
              MOVE ZEROES TO LE-R-LOANDATE
           ELSE
              MOVE QLE-R-LOANDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LE-R-LOANDATE.

           MOVE QLE-R-LNAMT TO LE-R-LNAMT.

           IF ( QLE-R-POFFDATE = "1900-01-01" )
              MOVE ZEROES TO LE-R-POFFDATE
           ELSE
              MOVE QLE-R-POFFDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LE-R-POFFDATE.

           MOVE QLE-R-RESTRICTED-MONEY TO LE-R-RESTRICTED-MONEY.
           MOVE QLE-R-BALANCE TO LE-R-BALANCE.
           MOVE QLE-R-SALEFIN TO LE-R-SALEFIN.
 
      *-----------------------------------------------------------------
      *    LOAD QLE-REC FROM LE-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-LE-FIELDS.
           MOVE LE-BRNO TO QLE-BRNO.
           MOVE LE-ACCTNO TO QLE-ACCTNO.
           MOVE LE-TYPE TO QLE-TYPE.
           MOVE LE-FLAG TO QLE-FLAG.
           MOVE LE-SSNO TO QLE-SSNO.
           MOVE LE-OSACCTNO TO QLE-OSACCTNO.

           IF ( LE-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QLE-LTOUCH-DATE
           ELSE
              MOVE LE-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLE-LTOUCH-DATE.

           MOVE LE-E-OSRATE TO QLE-E-OSRATE.
           MOVE LE-E-OSAMT TO QLE-E-OSAMT.

           IF ( LE-E-OSDATE = ZEROES )
              MOVE "1900-01-01" TO QLE-E-OSDATE
           ELSE
              MOVE LE-E-OSDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLE-E-OSDATE.

           MOVE LE-E-AMT TO QLE-E-AMT.

           IF ( LE-E-DATE = ZEROES )
              MOVE "1900-01-01" TO QLE-E-DATE
           ELSE
              MOVE LE-E-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLE-E-DATE.

           MOVE LE-E-OSMAKERCD TO QLE-E-OSMAKERCD.

           IF ( LE-R-LOANDATE = ZEROES )
              MOVE "1900-01-01" TO QLE-R-LOANDATE
           ELSE
              MOVE LE-R-LOANDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLE-R-LOANDATE.

           MOVE LE-R-LNAMT TO QLE-R-LNAMT.

           IF ( LE-R-POFFDATE = ZEROES )
              MOVE "1900-01-01" TO QLE-R-POFFDATE
           ELSE
              MOVE LE-R-POFFDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLE-R-POFFDATE.

           MOVE LE-R-RESTRICTED-MONEY TO QLE-R-RESTRICTED-MONEY.
           MOVE LE-R-BALANCE TO QLE-R-BALANCE.
           MOVE LE-R-SALEFIN TO QLE-R-SALEFIN.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
       LOAD-LE1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LE-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LE-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LE-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LE-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LE-ALL-DATA
              MOVE LE-ALL-PATH             TO LE-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LE-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LE-ALL-BASE
              MOVE FILPATH-MACHINE         TO LE-ALL-MACHINE
              MOVE FILPATH-DATA            TO LE-ALL-DATA
              MOVE LE-ALL-PATH             TO LE-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-LEFILE NOT = "S" )
              MOVE LE-PATH-BEG             TO LE-SELECT-PATH-BEG
              MOVE LE-PATH-FILE            TO LE-SELECT-PATH-FILE
           ELSE
              MOVE LE-PATH-SQL             TO LE-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-LE1-FILE.
           PERFORM LOAD-LE1-FILE.
           PERFORM OPEN-IT.
           MOVE LE-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LE1-FILE.
           PERFORM START-IT.
           MOVE LE-PATH-OWNBR  TO LE-BRNO.
           MOVE LE-SELECT-PATH TO E-FILE.
           MOVE LE1-KEY        TO E-KEYX.

           IF ( LE1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LE1-FILE.

           IF ( QLE1-WEND-OSACCTNO = ZEROES    ) OR
              ( QLE1-WEND-OSACCTNO NOT NUMERIC )
              MOVE LE-PATH-OWNBR        TO QLE1-WBEG-BRNO
              MOVE LE-PATH-OWNBR        TO QLE1-WEND-BRNO
              MOVE LE-ACCTNO            TO QLE1-WBEG-ACCTNO
              MOVE ALL "9"              TO QLE1-WEND-ACCTNO
              MOVE LE-TYPE              TO QLE1-WBEG-TYPE
              MOVE "Z"                  TO QLE1-WEND-TYPE
              MOVE LE-FLAG              TO QLE1-WBEG-FLAG
              MOVE "Z"                  TO QLE1-WEND-FLAG
              MOVE LE-SSNO              TO QLE1-WBEG-SSNO
              MOVE ALL "9"              TO QLE1-WEND-SSNO
              MOVE LE-OSACCTNO          TO QLE1-WBEG-OSACCTNO
              MOVE ALL "9"              TO QLE1-WEND-OSACCTNO.

           EXEC SQL
            DECLARE LE1_CURSOR CURSOR FOR
            SELECT LEFILE.LE_BRNO,
                   LEFILE.LE_ACCTNO,
                   ISNULL(LEFILE.LE_TYPE,''),
                   ISNULL(LEFILE.LE_FLAG,''),
                   LEFILE.LE_SSNO,
                   LEFILE.LE_OSACCTNO,
                   ISNULL(CAST(LEFILE.LE_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LEFILE.LE_E_OSRATE,
                   LEFILE.LE_E_OSAMT,
                   ISNULL(CAST(LEFILE.LE_E_OSDATE AS VARCHAR(10)),'0'),
                   LEFILE.LE_E_AMT,
                   ISNULL(CAST(LEFILE.LE_E_DATE AS VARCHAR(10)),'0'),
                   ISNULL(LEFILE.LE_E_OSMAKERCD,''),
                   ISNULL(CAST(LEFILE.LE_R_LOANDATE
                                                  AS VARCHAR(10)),'0'),
                   LEFILE.LE_R_LNAMT,
                   ISNULL(CAST(LEFILE.LE_R_POFFDATE
                                                  AS VARCHAR(10)),'0'),
                   LEFILE.LE_R_RESTRICTED_MONEY,
                   LEFILE.LE_R_BALANCE,
                   ISNULL(LEFILE.LE_R_SALEFIN,'')
            FROM DBO.LEFILE
            WHERE LEFILE.LE_BRNO>= :QLE1-WBEG-BRNO
              AND LEFILE.LE_BRNO <= :QLE1-WEND-BRNO
              AND LEFILE.LE_ACCTNO >= :QLE1-WBEG-ACCTNO
              AND LEFILE.LE_ACCTNO <= :QLE1-WEND-ACCTNO
              AND LEFILE.LE_TYPE >= :QLE1-WBEG-TYPE
              AND LEFILE.LE_TYPE <= :QLE1-WEND-TYPE
              AND LEFILE.LE_FLAG >= :QLE1-WBEG-FLAG
              AND LEFILE.LE_FLAG <= :QLE1-WEND-FLAG
              AND LEFILE.LE_SSNO >= :QLE1-WBEG-SSNO
              AND LEFILE.LE_SSNO <= :QLE1-WEND-SSNO
              AND LEFILE.LE_OSACCTNO >= :QLE1-WBEG-OSACCTNO
              AND LEFILE.LE_OSACCTNO <= :QLE1-WEND-OSACCTNO
            ORDER BY LEFILE.LE_BRNO,
                     LEFILE.LE_ACCTNO,
                     LEFILE.LE_TYPE,
                     LEFILE.LE_FLAG,
                     LEFILE.LE_SSNO,
                     LEFILE.LE_OSACCTNO
           END-EXEC.

           EXEC SQL
               OPEN LE1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLE1-WBEG-BRNO 
                          QLE1-WEND-BRNO
                          QLE1-WBEG-ACCTNO
                          QLE1-WEND-ACCTNO
                          QLE1-WBEG-SSNO
                          QLE1-WEND-SSNO
                          QLE1-WBEG-OSACCTNO
                          QLE1-WEND-OSACCTNO.
           MOVE SPACES TO QLE1-WBEG-TYPE
                          QLE1-WEND-TYPE
                          QLE1-WBEG-FLAG
                          QLE1-WEND-FLAG

           MOVE STAT---GOOD TO LE1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LE1-FILE.
           PERFORM READ-IT.
           MOVE LE-PATH-OWNBR  TO LE-BRNO. 
           MOVE LE-SELECT-PATH TO E-FILE.
           MOVE LE1-KEY        TO E-KEYX.

           MOVE LE-BRNO     TO QLE-BRNO.
           MOVE LE-ACCTNO   TO QLE-ACCTNO.
           MOVE LE-TYPE     TO QLE-TYPE.
           MOVE LE-FLAG     TO QLE-FLAG.
           MOVE LE-SSNO     TO QLE-SSNO.
           MOVE LE-OSACCTNO TO QLE-OSACCTNO.

           EXEC SQL
            SELECT LEFILE.LE_BRNO,
                   LEFILE.LE_ACCTNO,
                   ISNULL(LEFILE.LE_TYPE,''),
                   ISNULL(LEFILE.LE_FLAG,''),
                   LEFILE.LE_SSNO,
                   LEFILE.LE_OSACCTNO,
                   ISNULL(CAST(LEFILE.LE_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LEFILE.LE_E_OSRATE,
                   LEFILE.LE_E_OSAMT,
                   ISNULL(CAST(LEFILE.LE_E_OSDATE AS VARCHAR(10)),'0'),
                   LEFILE.LE_E_AMT,
                   ISNULL(CAST(LEFILE.LE_E_DATE AS VARCHAR(10)),'0'),
                   ISNULL(LEFILE.LE_E_OSMAKERCD,''),
                   ISNULL(CAST(LEFILE.LE_R_LOANDATE
                                                  AS VARCHAR(10)),'0'),
                   LEFILE.LE_R_LNAMT,
                   ISNULL(CAST(LEFILE.LE_R_POFFDATE
                                                  AS VARCHAR(10)),'0'),
                   LEFILE.LE_R_RESTRICTED_MONEY,
                   LEFILE.LE_R_BALANCE,
                   ISNULL(LEFILE.LE_R_SALEFIN,'')
            INTO :QLE-REC
            FROM DBO.LEFILE
            WHERE LEFILE.LE_BRNO = :QLE-BRNO
              AND LEFILE.LE_ACCTNO = :QLE-ACCTNO
              AND LEFILE.LE_TYPE = :QLE-TYPE
              AND LEFILE.LE_FLAG = :QLE-FLAG
              AND LEFILE.LE_SSNO = :QLE-SSNO
              AND LEFILE.LE_OSACCTNO = :QLE-OSACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LE1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LE-FIELDS.

      *-----------------------------------------------------------------
       READ-LE1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LE-PATH-OWNBR  TO LE-BRNO.
           MOVE LE-SELECT-PATH TO E-FILE.
           MOVE LE1-KEY        TO E-KEYX.
           INITIALIZE QLE-REC.

           IF ( LE1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LE1_CURSOR INTO :QLE-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LE1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LE-FIELDS.

      *-----------------------------------------------------------------
       WRITE-LE1-FILE.

           MOVE LE-PATH-OWNBR  TO LE-BRNO.
           ACCEPT LE-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE LE-SELECT-PATH TO E-FILE.
           MOVE LE1-KEY        TO E-KEYX.
           PERFORM SET-LE-FIELDS.

           EXEC SQL
            INSERT INTO DBO.LEFILE
             (LEFILE.LE_BRNO,
              LEFILE.LE_ACCTNO,
              LEFILE.LE_TYPE,
              LEFILE.LE_FLAG,
              LEFILE.LE_SSNO,
              LEFILE.LE_OSACCTNO,
              LEFILE.LE_LTOUCH_DATE,
              LEFILE.LE_E_OSRATE,
              LEFILE.LE_E_OSAMT,
              LEFILE.LE_E_OSDATE,
              LEFILE.LE_E_AMT,
              LEFILE.LE_E_DATE,
              LEFILE.LE_E_OSMAKERCD,
              LEFILE.LE_R_LOANDATE,
              LEFILE.LE_R_LNAMT,
              LEFILE.LE_R_POFFDATE,
              LEFILE.LE_R_RESTRICTED_MONEY,
              LEFILE.LE_R_BALANCE,
              LEFILE.LE_R_SALEFIN)
            VALUES
             (:QLE-BRNO,
              :QLE-ACCTNO,
              :QLE-TYPE,
              :QLE-FLAG,
              :QLE-SSNO,
              :QLE-OSACCTNO,
              NULLIF(:QLE-LTOUCH-DATE,'1900-01-01'),
              :QLE-E-OSRATE,
              :QLE-E-OSAMT,
              NULLIF(:QLE-E-OSDATE,'1900-01-01'),
              :QLE-E-AMT,
              NULLIF(:QLE-E-DATE,'1900-01-01'),
              :QLE-E-OSMAKERCD,
              NULLIF(:QLE-R-LOANDATE,'1900-01-01'),
              :QLE-R-LNAMT,
              NULLIF(:QLE-R-POFFDATE,'1900-01-01'),
              :QLE-R-RESTRICTED-MONEY,
              :QLE-R-BALANCE,
              :QLE-R-SALEFIN)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-LE1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-LE1-FILE.

           MOVE LE-PATH-OWNBR  TO LE-BRNO.
           ACCEPT LE-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE LE-SELECT-PATH TO E-FILE.
           MOVE LE1-KEY        TO E-KEYX.
           PERFORM SET-LE-FIELDS.

           EXEC SQL
            UPDATE DBO.LEFILE
             SET
              LEFILE.LE_BRNO = :QLE-BRNO,
              LEFILE.LE_ACCTNO = :QLE-ACCTNO,
              LEFILE.LE_TYPE = :QLE-TYPE,
              LEFILE.LE_FLAG = :QLE-FLAG,
              LEFILE.LE_SSNO = :QLE-SSNO,
              LEFILE.LE_OSACCTNO = :QLE-OSACCTNO,
              LEFILE.LE_LTOUCH_DATE =
                 NULLIF(:QLE-LTOUCH-DATE,'1900-01-01'),
              LEFILE.LE_E_OSRATE = :QLE-E-OSRATE,
              LEFILE.LE_E_OSAMT = :QLE-E-OSAMT,
              LEFILE.LE_E_OSDATE =
                 NULLIF(:QLE-E-OSDATE,'1900-01-01'),
              LEFILE.LE_E_AMT = :QLE-E-AMT,
              LEFILE.LE_E_DATE =
                 NULLIF(:QLE-E-DATE,'1900-01-01'),
              LEFILE.LE_E_OSMAKERCD = :QLE-E-OSMAKERCD,
              LEFILE.LE_R_LOANDATE =
                 NULLIF(:QLE-R-LOANDATE,'1900-01-01'),
              LEFILE.LE_R_LNAMT = :QLE-R-LNAMT,
              LEFILE.LE_R_POFFDATE =
                 NULLIF(:QLE-R-POFFDATE,'1900-01-01'),
              LEFILE.LE_R_RESTRICTED_MONEY =
                   :QLE-R-RESTRICTED-MONEY,
              LEFILE.LE_R_BALANCE = :QLE-R-BALANCE,
              LEFILE.LE_R_SALEFIN = :QLE-R-SALEFIN
             WHERE LEFILE.LE_BRNO = :QLE-BRNO
               AND LEFILE.LE_ACCTNO = :QLE-ACCTNO
               AND LEFILE.LE_TYPE = :QLE-TYPE
               AND LEFILE.LE_FLAG = :QLE-FLAG
               AND LEFILE.LE_SSNO = :QLE-SSNO
               AND LEFILE.LE_OSACCTNO = :QLE-OSACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-LE1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-LE1-FILE.

           MOVE LE-PATH-OWNBR  TO LE-BRNO.
           PERFORM DELETE-IT.
           MOVE LE-SELECT-PATH TO E-FILE.
           MOVE LE1-KEY        TO E-KEYX.
           PERFORM SET-LE-FIELDS.

           EXEC SQL
            DELETE FROM DBO.LEFILE
             WHERE LEFILE.LE_BRNO = :QLE-BRNO
               AND LEFILE.LE_ACCTNO = :QLE-ACCTNO
               AND LEFILE.LE_TYPE = :QLE-TYPE
               AND LEFILE.LE_FLAG = :QLE-FLAG
               AND LEFILE.LE_SSNO = :QLE-SSNO
               AND LEFILE.LE_OSACCTNO = :QLE-OSACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-LE1-FILE.

      *-----------------------------------------------------------------
       CLOSE-LE1-FILE.

           IF ( LE1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LE1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LE1-CURSOR-STAT.

      *#################################################################
      *    ENDING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
 
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
              DESTROY IOLEIN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
