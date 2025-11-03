       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOLTCR.
       AUTHOR.        BAH
       DATE-WRITTEN.  04-05-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOLTCR.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : LTCFILE 
      * TABLE DESC    : LOAN TRAILER COLLATERAL FILE
      * OUTPUT FILE   : XPC/IOLTCR.PRE
      * USED FOR      : LIBLP/LPWSLTC.CPY  --- QLTC-REC
      *                 LIBLP/LPLTC1R.CPY  --- READ
      *                 LIBLP/LPLTC1RN.CPY --- READ/READ-NEXT/START
      *                 LIBLP/LPLTC2RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * BAH 2021-04-05 ***NEW***
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
           "@(#)IO/IOLTCR.CBL S34 05/10/21-12:23:49 >".

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
       COPY "LIBLP/LP01LTC.CPY".
       COPY "LIBLP/LPWSLTC.CPY".

      *=================================================================
      *
      *    Q - F I L E
      *
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QLTC1-WHERE-BEG-KEY.
           03  QLTC1-WBEG-BRNO             PIC 9(04).
           03  QLTC1-WBEG-ACCTNO           PIC 9(08).
           03  QLTC1-WBEG-SEQNO            PIC 9(03).
 
       01  QLTC1-WHERE-END-KEY.
           03  QLTC1-WEND-BRNO             PIC 9(04).
           03  QLTC1-WEND-ACCTNO           PIC 9(08).
           03  QLTC1-WEND-SEQNO            PIC 9(03).

       01  QLTC2-WHERE-BEG-KEY.
           03  QLTC2-WBEG-BRNO             PIC 9(04).
           03  QLTC2-WBEG-VIN-1            PIC X(20).
       01  QLTC2-WHERE-END-KEY.
           03  QLTC2-WEND-BRNO             PIC 9(04).
           03  QLTC2-WEND-VIN-1            PIC X(20).
 
       01  QLTC3-WHERE-BEG-KEY.
           03  QLTC3-WBEG-BRNO             PIC 9(04).
           03  QLTC3-WBEG-VIN-2            PIC X(20).
       01  QLTC3-WHERE-END-KEY.
           03  QLTC3-WEND-BRNO             PIC 9(04).
           03  QLTC3-WEND-VIN-2            PIC X(20).

       01  QLTC4-WHERE-BEG-KEY.
           03  QLTC4-WBEG-BRNO             PIC 9(04).
           03  QLTC4-WBEG-VIN-3            PIC X(20).
       01  QLTC4-WHERE-END-KEY.
           03  QLTC4-WEND-BRNO             PIC 9(04).
           03  QLTC4-WEND-VIN-3            PIC X(20).
 
 
       01  QLTC-REC.
           03  QLTC-BRNO                              PIC 9(4).
           03  QLTC-ACCTNO                            PIC 9(8).
           03  QLTC-SEQNO                             PIC 999.
           03  QLTC-LTOUCH-DATE                       PIC X(10).
           03  QLTC-DESC-1                            PIC X(15).
           03  QLTC-MAKE-1                            PIC X(15).
           03  QLTC-MODEL-1                           PIC X(15).
           03  QLTC-YEAR-1                            PIC X(4).
           03  QLTC-STYLE-1                           PIC X(8).
           03  QLTC-CLASS-1                           PIC X(8).
           03  QLTC-VIN-1                             PIC X(20).
           03  QLTC-TITLE-NO-1                        PIC X(20).
           03  QLTC-MILEAGE-1                         PIC S9(7).
           03  QLTC-VALUE-1                           PIC S9(7)V99.
           03  QLTC-SELLPR-1                          PIC S9(7)V99.
           03  QLTC-MISC-AMT-1                        PIC S9(7)V99.
           03  QLTC-SENT-FLAG-1                       PIC X.
           03  QLTC-VIN-CHG-DATE-1                    PIC X(10).
           03  QLTC-DESC-2                            PIC X(15).
           03  QLTC-MAKE-2                            PIC X(15).
           03  QLTC-MODEL-2                           PIC X(15).
           03  QLTC-YEAR-2                            PIC X(4).
           03  QLTC-STYLE-2                           PIC X(8).
           03  QLTC-CLASS-2                           PIC X(8).
           03  QLTC-VIN-2                             PIC X(20).
           03  QLTC-TITLE-NO-2                        PIC X(20).
           03  QLTC-MILEAGE-2                         PIC S9(7).
           03  QLTC-VALUE-2                           PIC S9(7)V99.
           03  QLTC-SELLPR-2                          PIC S9(7)V99.
           03  QLTC-MISC-AMT-2                        PIC S9(7)V99.
           03  QLTC-SENT-FLAG-2                       PIC X.
           03  QLTC-VIN-CHG-DATE-2                    PIC X(10).
           03  QLTC-DESC-3                            PIC X(15).
           03  QLTC-MAKE-3                            PIC X(15).
           03  QLTC-MODEL-3                           PIC X(15).
           03  QLTC-YEAR-3                            PIC X(4).
           03  QLTC-STYLE-3                           PIC X(8).
           03  QLTC-CLASS-3                           PIC X(8).
           03  QLTC-VIN-3                             PIC X(20).
           03  QLTC-TITLE-NO-3                        PIC X(20).
           03  QLTC-MILEAGE-3                         PIC S9(7).
           03  QLTC-VALUE-3                           PIC S9(7)V99.
           03  QLTC-SELLPR-3                          PIC S9(7)V99.
           03  QLTC-MISC-AMT-3                        PIC S9(7)V99.
           03  QLTC-SENT-FLAG-3                       PIC X.
           03  QLTC-VIN-CHG-DATE-3                    PIC X(10).
           03  QLTC-HHGS-1                            PIC X(80).
           03  QLTC-HHGS-2                            PIC X(80).
           03  QLTC-HHGS-3                            PIC X(80).
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
           03  BEG-SEQNO               PIC 9(03).
           03  END-SEQNO               PIC 9(03).
           03  BEG-VIN1               PIC X(20).
           03  END-VIN1               PIC X(20).
           03  BEG-VIN2               PIC X(20).
           03  END-VIN2               PIC X(20).
           03  BEG-VIN3               PIC X(20).
           03  END-VIN3               PIC X(20).


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
           03  FILLER PIC X(37) VALUE "BRNO    ACCTNO    SEQ   MAKE1 ".
           03  FILLER PIC X(31) VALUE "MODEL1       VIN1".
           03  FILLER PIC X(30) VALUE "MAKE2".

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-LTC-BRNO                   PIC 9(04).
           03  FILLER                         PIC X(03).
           03  D01-LTC-ACCTNO                 PIC 9(08).
           03  FILLER                         PIC X(03).
           03  D01-LTC-SEQNO                  PIC 9(03).
           03  FILLER                         PIC X(03).
           03  D01-LTC-MAKE1                  PIC X(10).
           03  FILLER                         PIC X(03).
           03  D01-LTC-MODEL1                 PIC X(10).
           03  FILLER                         PIC X(03).
           03  D01-LTC-VIN1                   PIC X(17).
           03  FILLER                         PIC X(03).
           03  D01-LTC-MAKE2                  PIC X(15).
           03  FILLER                         PIC X(03).
           03  D01-LTC-MODEL2                 PIC X(15).
           03  FILLER                         PIC X(03).
           03  D01-LTC-VIN2                   PIC X(20).

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
       COPY "LIBIO/IOLTCR_DEF.CPY".
       COPY "LIBIO/IOLTCR_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOLTCR_SCN.CPY".

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
           MOVE "IOLTCR"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOLTCR-QUEUE-POS  TO Q-POS.
           MOVE IOLTCR-COPIES-POS TO C-POS.
           MOVE IOLTCR-PRU-POS    TO P-POS.
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
              GO TO ENTER-END-VIN3.

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
              ( VDUNUM0 > 4      ) 
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

      *=================================================================
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

           GO TO ENTER-BEG-SEQNO.

      *-----------------------------------------------------------------
       ENTER-ALL-ACCTNO.

           MOVE "SNNN080BEG-ACCTNO." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-ACCTNO.
           PERFORM SCREENIO.

           MOVE "SNNN080END-ACCTNO." TO VDU.
           MOVE 99999999            TO VDUNUM0  END-ACCTNO.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-SEQNO.

           MOVE "ENNN030BEG-SEQNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-ACCTNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-SEQNO.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-SEQNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-SEQNO.

      *-----------------------------------------------------------------
       ENTER-END-SEQNO.

           MOVE "ENNN030END-SEQNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-SEQNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-SEQNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-SEQNO.

           IF ( BEG-SEQNO > END-SEQNO )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-SEQNO.

           GO TO ENTER-BEG-VIN1.

      *-----------------------------------------------------------------
       ENTER-ALL-SEQNO.

           MOVE "SNNN030BEG-SEQNO." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-SEQNO.
           PERFORM SCREENIO.

           MOVE "SNNN030END-SEQNO." TO VDU.
           MOVE 999                 TO VDUNUM0  END-SEQNO.
           PERFORM SCREENIO.
      *=================================================================
       ENTER-BEG-VIN1.

           MOVE "E  A200BEG-VIN1." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-SEQNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-VIN1.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-VIN1.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-VIN1.

      *=========================================================
       ENTER-END-VIN1.

           MOVE "E  A200END-VIN1." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-VIN1.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-VIN1.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-VIN1.

           IF ( BEG-VIN1 > END-VIN1 )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-VIN1.

           GO TO ENTER-BEG-VIN2.

      *-----------------------------------------------------------------
       ENTER-ALL-VIN1.

           MOVE "S  A000BEG-VIN1." TO VDU.
           MOVE SPACES              TO VDUBUF  BEG-VIN1.
           PERFORM SCREENIO.

           MOVE "S  A000END-VIN1." TO VDU.
           MOVE ALL "Z"            TO VDUBUF  END-VIN1.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-VIN2.

           MOVE "E  A200BEG-VIN2." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-VIN1.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-VIN2.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-VIN2.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-VIN2.

      *=========================================================
       ENTER-END-VIN2.

           MOVE "E  A200END-VIN2." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-VIN2.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-VIN2.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-VIN2.

           IF ( BEG-VIN2 > END-VIN2 )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-VIN2.

           GO TO ENTER-BEG-VIN3.

      *-----------------------------------------------------------------
       ENTER-ALL-VIN2.

           MOVE "S  A000BEG-VIN2." TO VDU.
           MOVE SPACES              TO VDUBUF  BEG-VIN2.
           PERFORM SCREENIO.

           MOVE "S  A000END-VIN2." TO VDU.
           MOVE ALL "Z"            TO VDUBUF  END-VIN2.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-VIN3.

           MOVE "E  A200BEG-VIN3." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-VIN2.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-VIN3.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-VIN3.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-VIN3.

      *=========================================================
       ENTER-END-VIN3.

           MOVE "E  A200END-VIN3." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-VIN3.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-VIN3.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-VIN3.

           IF ( BEG-VIN3 > END-VIN3 )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-VIN3.

           GO TO ENTER-ACCEPT-YN.

      *-----------------------------------------------------------------
       ENTER-ALL-VIN3.

           MOVE "S  A000BEG-VIN3." TO VDU.
           MOVE SPACES              TO VDUBUF  BEG-VIN3.
           PERFORM SCREENIO.

           MOVE "S  A000END-VIN3." TO VDU.
           MOVE ALL "Z"            TO VDUBUF  END-VIN3.
           PERFORM SCREENIO.

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

           MOVE "SNNN030BEG-SEQNO."       TO VDU.
           MOVE BEG-SEQNO                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN030END-SEQNO."       TO VDU.
           MOVE END-SEQNO                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A200BEG-VIN1."       TO VDU.
           MOVE BEG-VIN1                 TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A200END-VIN1."       TO VDU.
           MOVE END-VIN1                 TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A200BEG-VIN2."       TO VDU.
           MOVE BEG-VIN2                 TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A200END-VIN2."       TO VDU.
           MOVE END-VIN2                 TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A200BEG-VIN3."       TO VDU.
           MOVE BEG-VIN3                 TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A200END-VIN3."       TO VDU.
           MOVE END-VIN3                 TO VDUBUF.
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

           MOVE "B"                        TO LTC-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - -{ SET WHERE BEG & END FIELDS }
           MOVE BR-NO     TO QLTC1-WBEG-BRNO 
                              QLTC2-WBEG-BRNO
                              QLTC3-WBEG-BRNO
                              QLTC4-WBEG-BRNO.

           MOVE BR-NO     TO QLTC1-WEND-BRNO
                              QLTC2-WEND-BRNO
                              QLTC3-WEND-BRNO
                              QLTC4-WEND-BRNO.

           MOVE BEG-ACCTNO TO QLTC1-WBEG-ACCTNO.
           MOVE END-ACCTNO TO QLTC1-WEND-ACCTNO.
           MOVE BEG-SEQNO  TO QLTC1-WBEG-SEQNO.
           MOVE END-SEQNO  TO QLTC1-WEND-SEQNO.

           MOVE BEG-VIN1  TO QLTC2-WBEG-VIN-1.
           MOVE END-VIN1  TO QLTC2-WEND-VIN-1.

           MOVE BEG-VIN2  TO QLTC3-WBEG-VIN-2.
           MOVE END-VIN2  TO QLTC3-WEND-VIN-2.

           MOVE BEG-VIN3  TO QLTC4-WBEG-VIN-3.
           MOVE END-VIN3  TO QLTC4-WEND-VIN-3.


      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           IF ( PROCESS-KEY = 1 )
              PERFORM OPEN-LTC1-FILE
              PERFORM START-LTC1-FILE
           ELSE
           IF ( PROCESS-KEY = 2 )
              PERFORM OPEN-LTC2-FILE
              PERFORM START-LTC2-FILE
           ELSE
           IF ( PROCESS-KEY = 3 )
              PERFORM OPEN-LTC3-FILE
              PERFORM START-LTC3-FILE
           ELSE
              PERFORM OPEN-LTC4-FILE
              PERFORM START-LTC4-FILE.

      *=================================================================
       PROCESS-BRANCH-NEXT.

           IF ( PROCESS-KEY = 1 )
              PERFORM READ-LTC1-FILE-NEXT
           ELSE
           IF ( PROCESS-KEY = 2 )
              PERFORM READ-LTC2-FILE-NEXT
           ELSE
           IF ( PROCESS-KEY = 3 )
              PERFORM READ-LTC3-FILE-NEXT
           ELSE
              PERFORM READ-LTC4-FILE-NEXT.

           IF ( IO-BAD ) OR ( LTC-BRNO NOT = LTC-PATH-OWNBR )
              GO TO PROCESS-BRANCH-MODULE-END.

           ADD 01 TO RECORDS-READ.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

           IF ( PROCESS-KEY = 1 )
              IF ( LTC-ACCTNO < BEG-ACCTNO )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( LTC-ACCTNO > END-ACCTNO )
                 GO TO PROCESS-BRANCH-MODULE-END.

           IF ( PROCESS-KEY = 1 )
              IF ( LTC-SEQNO < BEG-SEQNO OR > END-SEQNO)
                 GO TO PROCESS-BRANCH-NEXT.

           IF ( PROCESS-KEY = 2 )
              IF ( LTC-VIN-1  < BEG-VIN1  )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( LTC-VIN-1  > END-VIN1  )
                 GO TO PROCESS-BRANCH-MODULE-END.

           IF ( PROCESS-KEY = 3 )
              IF ( LTC-VIN-2  < BEG-VIN2  )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( LTC-VIN-2  > END-VIN2  )
                 GO TO PROCESS-BRANCH-MODULE-END.

           IF ( PROCESS-KEY = 4 )
              IF ( LTC-VIN-3  < BEG-VIN3  )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( LTC-VIN-3  > END-VIN3  )
                 GO TO PROCESS-BRANCH-MODULE-END.

      *-----------------------------------------------------------------
       PROCESS-BRANCH-PRINT.

           MOVE LTC-BRNO                 TO D01-LTC-BRNO.
           MOVE LTC-ACCTNO               TO D01-LTC-ACCTNO.
           MOVE LTC-SEQNO                TO D01-LTC-SEQNO.
           MOVE LTC-MAKE-1               TO D01-LTC-MAKE1.
           MOVE LTC-MODEL-1              TO D01-LTC-MODEL1.
           MOVE LTC-VIN-1                TO D01-LTC-VIN1.
           MOVE LTC-MAKE-2               TO D01-LTC-MAKE2.
           MOVE LTC-MODEL-2              TO D01-LTC-MODEL2.
           MOVE LTC-VIN-2                TO D01-LTC-VIN2.

           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.
           GO TO PROCESS-BRANCH-NEXT.

      *=================================================================
       PROCESS-BRANCH-MODULE-END.

           IF ( PROCESS-KEY = 1 )
              PERFORM CLOSE-LTC1-FILE
           ELSE
           IF ( PROCESS-KEY = 2 )
              PERFORM CLOSE-LTC2-FILE
           ELSE
           IF ( PROCESS-KEY = 3 )
              PERFORM CLOSE-LTC3-FILE
           ELSE
              PERFORM CLOSE-LTC4-FILE.

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

           IF ( PROCESS-KEY = 1 )
              MOVE LTC1-KEY TO PACE-DETAIL-KEY
           ELSE
           IF ( PROCESS-KEY = 2 )
              STRING LTC-BRNO, LTC-VIN-1
                     DELIMITED BY SIZE INTO PACE-DETAIL-KEY
           ELSE
           IF ( PROCESS-KEY = 3 )
              STRING LTC-BRNO, LTC-VIN-2
                     DELIMITED BY SIZE INTO PACE-DETAIL-KEY
           ELSE
              STRING LTC-BRNO, LTC-VIN-3
                     DELIMITED BY SIZE INTO PACE-DETAIL-KEY.

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
           MOVE "BEGINNING SEQNO               : "    TO R01-PROMPT.
           MOVE BEG-SEQNO                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    SEQNO               : "    TO R01-PROMPT.
           MOVE END-SEQNO                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING VIN1               : "    TO R01-PROMPT.
           MOVE BEG-VIN1                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    VIN1               : "    TO R01-PROMPT.
           MOVE END-VIN1                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING VIN2               : "    TO R01-PROMPT.
           MOVE BEG-VIN2                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    VIN2               : "    TO R01-PROMPT.
           MOVE END-VIN2                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING VIN3               : "    TO R01-PROMPT.
           MOVE BEG-VIN3                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    VIN3               : "    TO R01-PROMPT.
           MOVE END-VIN3                             TO R01-TEXT.
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
           MOVE "SQL TABLE IO UTILITY - LTCFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOLTCR-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOLTCR-SYSDATE.
           DISPLAY IOLTCR-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOLTCR-FORM-FIELDS TO WR-BUF.
           MOVE IOLTCR-FORM-LIST TO WR-LIST.
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

           MOVE IOLTCR-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOLTCR-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOLTCR_EVA.CPY".
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
      *    LOAD LTC-REC FROM QLTC-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-LTC-FIELDS.
           MOVE QLTC-BRNO TO LTC-BRNO.
           MOVE QLTC-ACCTNO TO LTC-ACCTNO.
           MOVE QLTC-SEQNO TO LTC-SEQNO.

           IF ( QLTC-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO LTC-LTOUCH-DATE
           ELSE
              MOVE QLTC-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LTC-LTOUCH-DATE.

           MOVE QLTC-DESC-1 TO LTC-DESC(1).
           MOVE QLTC-MAKE-1 TO LTC-MAKE(1).
           MOVE QLTC-MODEL-1 TO LTC-MODEL(1).
           MOVE QLTC-YEAR-1 TO LTC-YEAR(1).
           MOVE QLTC-STYLE-1 TO LTC-STYLE(1).
           MOVE QLTC-CLASS-1 TO LTC-CLASS(1).
           MOVE QLTC-VIN-1 TO LTC-VIN(1).
           MOVE QLTC-TITLE-NO-1 TO LTC-TITLE-NO(1).
           MOVE QLTC-MILEAGE-1 TO LTC-MILEAGE(1).
           MOVE QLTC-VALUE-1 TO LTC-VALUE(1).
           MOVE QLTC-SELLPR-1 TO LTC-SELLPR(1).
           MOVE QLTC-MISC-AMT-1 TO LTC-MISC-AMT(1).
           MOVE QLTC-SENT-FLAG-1 TO LTC-SENT-FLAG(1).

           IF ( QLTC-VIN-CHG-DATE-1 = "1900-01-01" )
              MOVE ZEROES TO LTC-VIN-CHG-DATE(1)
           ELSE
              MOVE QLTC-VIN-CHG-DATE-1 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LTC-VIN-CHG-DATE(1).

           MOVE QLTC-DESC-2 TO LTC-DESC(2).
           MOVE QLTC-MAKE-2 TO LTC-MAKE(2).
           MOVE QLTC-MODEL-2 TO LTC-MODEL(2).
           MOVE QLTC-YEAR-2 TO LTC-YEAR(2).
           MOVE QLTC-STYLE-2 TO LTC-STYLE(2).
           MOVE QLTC-CLASS-2 TO LTC-CLASS(2).
           MOVE QLTC-VIN-2 TO LTC-VIN(2).
           MOVE QLTC-TITLE-NO-2 TO LTC-TITLE-NO(2).
           MOVE QLTC-MILEAGE-2 TO LTC-MILEAGE(2).
           MOVE QLTC-VALUE-2 TO LTC-VALUE(2).
           MOVE QLTC-SELLPR-2 TO LTC-SELLPR(2).
           MOVE QLTC-MISC-AMT-2 TO LTC-MISC-AMT(2).
           MOVE QLTC-SENT-FLAG-2 TO LTC-SENT-FLAG(2).

           IF ( QLTC-VIN-CHG-DATE-2 = "1900-01-01" )
              MOVE ZEROES TO LTC-VIN-CHG-DATE(2)
           ELSE
              MOVE QLTC-VIN-CHG-DATE-2 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LTC-VIN-CHG-DATE(2).

           MOVE QLTC-DESC-3 TO LTC-DESC(3).
           MOVE QLTC-MAKE-3 TO LTC-MAKE(3).
           MOVE QLTC-MODEL-3 TO LTC-MODEL(3).
           MOVE QLTC-YEAR-3 TO LTC-YEAR(3).
           MOVE QLTC-STYLE-3 TO LTC-STYLE(3).
           MOVE QLTC-CLASS-3 TO LTC-CLASS(3).
           MOVE QLTC-VIN-3 TO LTC-VIN(3).
           MOVE QLTC-TITLE-NO-3 TO LTC-TITLE-NO(3).
           MOVE QLTC-MILEAGE-3 TO LTC-MILEAGE(3).
           MOVE QLTC-VALUE-3 TO LTC-VALUE(3).
           MOVE QLTC-SELLPR-3 TO LTC-SELLPR(3).
           MOVE QLTC-MISC-AMT-3 TO LTC-MISC-AMT(3).
           MOVE QLTC-SENT-FLAG-3 TO LTC-SENT-FLAG(3).

           IF ( QLTC-VIN-CHG-DATE-3 = "1900-01-01" )
              MOVE ZEROES TO LTC-VIN-CHG-DATE(3)
           ELSE
              MOVE QLTC-VIN-CHG-DATE-3 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LTC-VIN-CHG-DATE(3).

           MOVE QLTC-HHGS-1 TO LTC-HHGS(1).
           MOVE QLTC-HHGS-2 TO LTC-HHGS(2).
           MOVE QLTC-HHGS-3 TO LTC-HHGS(3).
 
      *-----------------------------------------------------------------
      *    LOAD QLTC-REC FROM LTC-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-LTC-FIELDS.
           MOVE LTC-BRNO TO QLTC-BRNO.
           MOVE LTC-ACCTNO TO QLTC-ACCTNO.
           MOVE LTC-SEQNO TO QLTC-SEQNO.

           IF ( LTC-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QLTC-LTOUCH-DATE
           ELSE
              MOVE LTC-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLTC-LTOUCH-DATE.

           MOVE LTC-DESC(1) TO QLTC-DESC-1.
           MOVE LTC-MAKE(1) TO QLTC-MAKE-1.
           MOVE LTC-MODEL(1) TO QLTC-MODEL-1.
           MOVE LTC-YEAR(1) TO QLTC-YEAR-1.
           MOVE LTC-STYLE(1) TO QLTC-STYLE-1.
           MOVE LTC-CLASS(1) TO QLTC-CLASS-1.
           MOVE LTC-VIN(1) TO QLTC-VIN-1.
           MOVE LTC-TITLE-NO(1) TO QLTC-TITLE-NO-1.
           MOVE LTC-MILEAGE(1) TO QLTC-MILEAGE-1.
           MOVE LTC-VALUE(1) TO QLTC-VALUE-1.
           MOVE LTC-SELLPR(1) TO QLTC-SELLPR-1.
           MOVE LTC-MISC-AMT(1) TO QLTC-MISC-AMT-1.
           MOVE LTC-SENT-FLAG(1) TO QLTC-SENT-FLAG-1.

           IF ( LTC-VIN-CHG-DATE(1) = ZEROES )
              MOVE "1900-01-01" TO QLTC-VIN-CHG-DATE-1
           ELSE
              MOVE LTC-VIN-CHG-DATE(1) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLTC-VIN-CHG-DATE-1.

           MOVE LTC-DESC(2) TO QLTC-DESC-2.
           MOVE LTC-MAKE(2) TO QLTC-MAKE-2.
           MOVE LTC-MODEL(2) TO QLTC-MODEL-2.
           MOVE LTC-YEAR(2) TO QLTC-YEAR-2.
           MOVE LTC-STYLE(2) TO QLTC-STYLE-2.
           MOVE LTC-CLASS(2) TO QLTC-CLASS-2.
           MOVE LTC-VIN(2) TO QLTC-VIN-2.
           MOVE LTC-TITLE-NO(2) TO QLTC-TITLE-NO-2.
           MOVE LTC-MILEAGE(2) TO QLTC-MILEAGE-2.
           MOVE LTC-VALUE(2) TO QLTC-VALUE-2.
           MOVE LTC-SELLPR(2) TO QLTC-SELLPR-2.
           MOVE LTC-MISC-AMT(2) TO QLTC-MISC-AMT-2.
           MOVE LTC-SENT-FLAG(2) TO QLTC-SENT-FLAG-2.

           IF ( LTC-VIN-CHG-DATE(2) = ZEROES )
              MOVE "1900-01-01" TO QLTC-VIN-CHG-DATE-2
           ELSE
              MOVE LTC-VIN-CHG-DATE(2) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLTC-VIN-CHG-DATE-2.

           MOVE LTC-DESC(3) TO QLTC-DESC-3.
           MOVE LTC-MAKE(3) TO QLTC-MAKE-3.
           MOVE LTC-MODEL(3) TO QLTC-MODEL-3.
           MOVE LTC-YEAR(3) TO QLTC-YEAR-3.
           MOVE LTC-STYLE(3) TO QLTC-STYLE-3.
           MOVE LTC-CLASS(3) TO QLTC-CLASS-3.
           MOVE LTC-VIN(3) TO QLTC-VIN-3.
           MOVE LTC-TITLE-NO(3) TO QLTC-TITLE-NO-3.
           MOVE LTC-MILEAGE(3) TO QLTC-MILEAGE-3.
           MOVE LTC-VALUE(3) TO QLTC-VALUE-3.
           MOVE LTC-SELLPR(3) TO QLTC-SELLPR-3.
           MOVE LTC-MISC-AMT(3) TO QLTC-MISC-AMT-3.
           MOVE LTC-SENT-FLAG(3) TO QLTC-SENT-FLAG-3.

           IF ( LTC-VIN-CHG-DATE(3) = ZEROES )
              MOVE "1900-01-01" TO QLTC-VIN-CHG-DATE-3
           ELSE
              MOVE LTC-VIN-CHG-DATE(3) TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLTC-VIN-CHG-DATE-3.

           MOVE LTC-HHGS(1) TO QLTC-HHGS-1.
           MOVE LTC-HHGS(2) TO QLTC-HHGS-2.
           MOVE LTC-HHGS(3) TO QLTC-HHGS-3.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-LTC1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LTC-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LTC-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LTC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LTC-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LTC-ALL-DATA
              MOVE LTC-ALL-PATH             TO LTC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LTC-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LTC-ALL-BASE
              MOVE FILPATH-MACHINE         TO LTC-ALL-MACHINE
              MOVE FILPATH-DATA            TO LTC-ALL-DATA
              MOVE LTC-ALL-PATH             TO LTC-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-LTCFILE NOT = "S" )
              MOVE LTC-PATH-BEG             TO LTC-SELECT-PATH-BEG
              MOVE LTC-PATH-FILE            TO LTC-SELECT-PATH-FILE
           ELSE
              MOVE LTC-PATH-SQL             TO LTC-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-LTC1-FILE.
           PERFORM LOAD-LTC1-FILE.
           PERFORM OPEN-IT.
           MOVE LTC-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LTC1-FILE.
           PERFORM START-IT.
           MOVE LTC-PATH-OWNBR  TO LTC-BRNO.
           MOVE LTC-SELECT-PATH TO E-FILE.
           MOVE LTC1-KEY        TO E-KEYX.

           IF ( LTC1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LTC1-FILE.

           IF ( QLTC1-WEND-BRNO = ZEROES    ) OR
              ( QLTC1-WEND-BRNO NOT NUMERIC )
              MOVE LTC-PATH-OWNBR        TO QLTC1-WBEG-BRNO
              MOVE LTC-PATH-OWNBR        TO QLTC1-WEND-BRNO
              MOVE LTC-ACCTNO            TO QLTC1-WBEG-ACCTNO
              MOVE ALL "9"               TO QLTC1-WEND-ACCTNO
              MOVE LTC-SEQNO             TO QLTC1-WBEG-SEQNO
              MOVE ALL "9"               TO QLTC1-WEND-SEQNO.

           EXEC SQL
            DECLARE LTC1_CURSOR CURSOR FOR
            SELECT LTCFILE.LTC_BRNO,
                   LTCFILE.LTC_ACCTNO,
                   LTCFILE.LTC_SEQNO,
                   ISNULL(CAST(LTCFILE.LTC_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LTCFILE.LTC_DESC_1,''),
                   ISNULL(LTCFILE.LTC_MAKE_1,''),
                   ISNULL(LTCFILE.LTC_MODEL_1,''),
                   ISNULL(LTCFILE.LTC_YEAR_1,''),
                   ISNULL(LTCFILE.LTC_STYLE_1,''),
                   ISNULL(LTCFILE.LTC_CLASS_1,''),
                   ISNULL(LTCFILE.LTC_VIN_1,''),
                   ISNULL(LTCFILE.LTC_TITLE_NO_1,''),
                   LTCFILE.LTC_MILEAGE_1,
                   LTCFILE.LTC_VALUE_1,
                   LTCFILE.LTC_SELLPR_1,
                   LTCFILE.LTC_MISC_AMT_1,
                   ISNULL(LTCFILE.LTC_SENT_FLAG_1,''),
                   ISNULL(CAST(LTCFILE.LTC_VIN_CHG_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LTCFILE.LTC_DESC_2,''),
                   ISNULL(LTCFILE.LTC_MAKE_2,''),
                   ISNULL(LTCFILE.LTC_MODEL_2,''),
                   ISNULL(LTCFILE.LTC_YEAR_2,''),
                   ISNULL(LTCFILE.LTC_STYLE_2,''),
                   ISNULL(LTCFILE.LTC_CLASS_2,''),
                   ISNULL(LTCFILE.LTC_VIN_2,''),
                   ISNULL(LTCFILE.LTC_TITLE_NO_2,''),
                   LTCFILE.LTC_MILEAGE_2,
                   LTCFILE.LTC_VALUE_2,
                   LTCFILE.LTC_SELLPR_2,
                   LTCFILE.LTC_MISC_AMT_2,
                   ISNULL(LTCFILE.LTC_SENT_FLAG_2,''),
                   ISNULL(CAST(LTCFILE.LTC_VIN_CHG_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LTCFILE.LTC_DESC_3,''),
                   ISNULL(LTCFILE.LTC_MAKE_3,''),
                   ISNULL(LTCFILE.LTC_MODEL_3,''),
                   ISNULL(LTCFILE.LTC_YEAR_3,''),
                   ISNULL(LTCFILE.LTC_STYLE_3,''),
                   ISNULL(LTCFILE.LTC_CLASS_3,''),
                   ISNULL(LTCFILE.LTC_VIN_3,''),
                   ISNULL(LTCFILE.LTC_TITLE_NO_3,''),
                   LTCFILE.LTC_MILEAGE_3,
                   LTCFILE.LTC_VALUE_3,
                   LTCFILE.LTC_SELLPR_3,
                   LTCFILE.LTC_MISC_AMT_3,
                   ISNULL(LTCFILE.LTC_SENT_FLAG_3,''),
                   ISNULL(CAST(LTCFILE.LTC_VIN_CHG_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LTCFILE.LTC_HHGS_1,''),
                   ISNULL(LTCFILE.LTC_HHGS_2,''),
                   ISNULL(LTCFILE.LTC_HHGS_3,'')
            FROM DBO.LTCFILE
            WHERE LTCFILE.LTC_BRNO >= :QLTC1-WBEG-BRNO
              AND LTCFILE.LTC_BRNO <= :QLTC1-WEND-BRNO
              AND LTCFILE.LTC_ACCTNO >= :QLTC1-WBEG-ACCTNO
              AND LTCFILE.LTC_ACCTNO <= :QLTC1-WEND-ACCTNO
              AND LTCFILE.LTC_SEQNO >= :QLTC1-WBEG-SEQNO
              AND LTCFILE.LTC_SEQNO <= :QLTC1-WEND-SEQNO
            ORDER BY LTCFILE.LTC_BRNO,
                     LTCFILE.LTC_ACCTNO,
                     LTCFILE.LTC_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN LTC1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLTC1-WBEG-BRNO
                          QLTC1-WEND-BRNO
                          QLTC1-WBEG-ACCTNO
                          QLTC1-WEND-ACCTNO
                          QLTC1-WBEG-SEQNO
                          QLTC1-WEND-SEQNO.

           MOVE STAT---GOOD TO LTC1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LTC1-FILE.
           PERFORM READ-IT.
           MOVE LTC-PATH-OWNBR  TO LTC-BRNO. 
           MOVE LTC-SELECT-PATH TO E-FILE.
           MOVE LTC1-KEY        TO E-KEYX.

           MOVE LTC-BRNO   TO QLTC-BRNO.
           MOVE LTC-ACCTNO TO QLTC-ACCTNO.
           MOVE LTC-SEQNO  TO QLTC-SEQNO.

           EXEC SQL
            SELECT LTCFILE.LTC_BRNO,
                   LTCFILE.LTC_ACCTNO,
                   LTCFILE.LTC_SEQNO,
                   ISNULL(CAST(LTCFILE.LTC_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LTCFILE.LTC_DESC_1,''),
                   ISNULL(LTCFILE.LTC_MAKE_1,''),
                   ISNULL(LTCFILE.LTC_MODEL_1,''),
                   ISNULL(LTCFILE.LTC_YEAR_1,''),
                   ISNULL(LTCFILE.LTC_STYLE_1,''),
                   ISNULL(LTCFILE.LTC_CLASS_1,''),
                   ISNULL(LTCFILE.LTC_VIN_1,''),
                   ISNULL(LTCFILE.LTC_TITLE_NO_1,''),
                   LTCFILE.LTC_MILEAGE_1,
                   LTCFILE.LTC_VALUE_1,
                   LTCFILE.LTC_SELLPR_1,
                   LTCFILE.LTC_MISC_AMT_1,
                   ISNULL(LTCFILE.LTC_SENT_FLAG_1,''),
                   ISNULL(CAST(LTCFILE.LTC_VIN_CHG_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LTCFILE.LTC_DESC_2,''),
                   ISNULL(LTCFILE.LTC_MAKE_2,''),
                   ISNULL(LTCFILE.LTC_MODEL_2,''),
                   ISNULL(LTCFILE.LTC_YEAR_2,''),
                   ISNULL(LTCFILE.LTC_STYLE_2,''),
                   ISNULL(LTCFILE.LTC_CLASS_2,''),
                   ISNULL(LTCFILE.LTC_VIN_2,''),
                   ISNULL(LTCFILE.LTC_TITLE_NO_2,''),
                   LTCFILE.LTC_MILEAGE_2,
                   LTCFILE.LTC_VALUE_2,
                   LTCFILE.LTC_SELLPR_2,
                   LTCFILE.LTC_MISC_AMT_2,
                   ISNULL(LTCFILE.LTC_SENT_FLAG_2,''),
                   ISNULL(CAST(LTCFILE.LTC_VIN_CHG_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LTCFILE.LTC_DESC_3,''),
                   ISNULL(LTCFILE.LTC_MAKE_3,''),
                   ISNULL(LTCFILE.LTC_MODEL_3,''),
                   ISNULL(LTCFILE.LTC_YEAR_3,''),
                   ISNULL(LTCFILE.LTC_STYLE_3,''),
                   ISNULL(LTCFILE.LTC_CLASS_3,''),
                   ISNULL(LTCFILE.LTC_VIN_3,''),
                   ISNULL(LTCFILE.LTC_TITLE_NO_3,''),
                   LTCFILE.LTC_MILEAGE_3,
                   LTCFILE.LTC_VALUE_3,
                   LTCFILE.LTC_SELLPR_3,
                   LTCFILE.LTC_MISC_AMT_3,
                   ISNULL(LTCFILE.LTC_SENT_FLAG_3,''),
                   ISNULL(CAST(LTCFILE.LTC_VIN_CHG_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LTCFILE.LTC_HHGS_1,''),
                   ISNULL(LTCFILE.LTC_HHGS_2,''),
                   ISNULL(LTCFILE.LTC_HHGS_3,'')
            INTO :QLTC-REC
            FROM DBO.LTCFILE
            WHERE LTCFILE.LTC_BRNO = :QLTC-BRNO
              AND LTCFILE.LTC_ACCTNO = :QLTC-ACCTNO
              AND LTCFILE.LTC_SEQNO = :QLTC-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTC1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LTC-FIELDS.

      *-----------------------------------------------------------------
       READ-LTC1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LTC-PATH-OWNBR  TO LTC-BRNO.
           MOVE LTC-SELECT-PATH TO E-FILE.
           MOVE LTC1-KEY        TO E-KEYX.
           INITIALIZE QLTC-REC.

           IF ( LTC1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LTC1_CURSOR INTO :QLTC-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTC1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LTC-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LTC1-FILE.

           IF ( LTC1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LTC1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LTC1-CURSOR-STAT.

      *#################################################################
       LOAD-LTC2-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LTC-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LTC-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LTC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LTC-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LTC-ALL-DATA
              MOVE LTC-ALL-PATH             TO LTC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LTC-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LTC-ALL-BASE
              MOVE FILPATH-MACHINE         TO LTC-ALL-MACHINE
              MOVE FILPATH-DATA            TO LTC-ALL-DATA
              MOVE LTC-ALL-PATH             TO LTC-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-LTCFILE NOT = "S" )
              MOVE LTC-PATH-BEG             TO LTC-SELECT-PATH-BEG
              MOVE LTC-PATH-FILE            TO LTC-SELECT-PATH-FILE
           ELSE
              MOVE LTC-PATH-SQL             TO LTC-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-LTC2-FILE.
           PERFORM LOAD-LTC2-FILE.
           PERFORM OPEN-IT.
           MOVE LTC-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LTC2-FILE.
           PERFORM START-IT.
           MOVE LTC-PATH-OWNBR  TO LTC-BRNO.
           MOVE LTC-SELECT-PATH TO E-FILE.
           MOVE SPACES          TO E-KEYX.
           STRING LTC-BRNO, LTC-VIN-1 DELIMITED BY SIZE INTO E-KEYX.

           IF ( LTC2-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LTC2-FILE.

           IF ( QLTC2-WEND-BRNO = ZEROES    ) OR
              ( QLTC2-WEND-BRNO NOT NUMERIC )
              MOVE LTC-PATH-OWNBR        TO QLTC2-WBEG-BRNO
              MOVE LTC-PATH-OWNBR        TO QLTC2-WEND-BRNO
              MOVE LTC-VIN-1             TO QLTC2-WBEG-VIN-1
              MOVE ALL "Z"               TO QLTC2-WEND-VIN-1.

           EXEC SQL
            DECLARE LTC2_CURSOR CURSOR FOR
            SELECT LTCFILE.LTC_BRNO,
                   LTCFILE.LTC_ACCTNO,
                   LTCFILE.LTC_SEQNO,
                   ISNULL(CAST(LTCFILE.LTC_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LTCFILE.LTC_DESC_1,''),
                   ISNULL(LTCFILE.LTC_MAKE_1,''),
                   ISNULL(LTCFILE.LTC_MODEL_1,''),
                   ISNULL(LTCFILE.LTC_YEAR_1,''),
                   ISNULL(LTCFILE.LTC_STYLE_1,''),
                   ISNULL(LTCFILE.LTC_CLASS_1,''),
                   ISNULL(LTCFILE.LTC_VIN_1,''),
                   ISNULL(LTCFILE.LTC_TITLE_NO_1,''),
                   LTCFILE.LTC_MILEAGE_1,
                   LTCFILE.LTC_VALUE_1,
                   LTCFILE.LTC_SELLPR_1,
                   LTCFILE.LTC_MISC_AMT_1,
                   ISNULL(LTCFILE.LTC_SENT_FLAG_1,''),
                   ISNULL(CAST(LTCFILE.LTC_VIN_CHG_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LTCFILE.LTC_DESC_2,''),
                   ISNULL(LTCFILE.LTC_MAKE_2,''),
                   ISNULL(LTCFILE.LTC_MODEL_2,''),
                   ISNULL(LTCFILE.LTC_YEAR_2,''),
                   ISNULL(LTCFILE.LTC_STYLE_2,''),
                   ISNULL(LTCFILE.LTC_CLASS_2,''),
                   ISNULL(LTCFILE.LTC_VIN_2,''),
                   ISNULL(LTCFILE.LTC_TITLE_NO_2,''),
                   LTCFILE.LTC_MILEAGE_2,
                   LTCFILE.LTC_VALUE_2,
                   LTCFILE.LTC_SELLPR_2,
                   LTCFILE.LTC_MISC_AMT_2,
                   ISNULL(LTCFILE.LTC_SENT_FLAG_2,''),
                   ISNULL(CAST(LTCFILE.LTC_VIN_CHG_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LTCFILE.LTC_DESC_3,''),
                   ISNULL(LTCFILE.LTC_MAKE_3,''),
                   ISNULL(LTCFILE.LTC_MODEL_3,''),
                   ISNULL(LTCFILE.LTC_YEAR_3,''),
                   ISNULL(LTCFILE.LTC_STYLE_3,''),
                   ISNULL(LTCFILE.LTC_CLASS_3,''),
                   ISNULL(LTCFILE.LTC_VIN_3,''),
                   ISNULL(LTCFILE.LTC_TITLE_NO_3,''),
                   LTCFILE.LTC_MILEAGE_3,
                   LTCFILE.LTC_VALUE_3,
                   LTCFILE.LTC_SELLPR_3,
                   LTCFILE.LTC_MISC_AMT_3,
                   ISNULL(LTCFILE.LTC_SENT_FLAG_3,''),
                   ISNULL(CAST(LTCFILE.LTC_VIN_CHG_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LTCFILE.LTC_HHGS_1,''),
                   ISNULL(LTCFILE.LTC_HHGS_2,''),
                   ISNULL(LTCFILE.LTC_HHGS_3,'')
            FROM DBO.LTCFILE
            WHERE LTCFILE.LTC_BRNO >= :QLTC2-WBEG-BRNO
              AND LTCFILE.LTC_BRNO <= :QLTC2-WEND-BRNO
              AND LTCFILE.LTC_VIN_1 >= :QLTC2-WBEG-VIN-1
              AND LTCFILE.LTC_VIN_1 <= :QLTC2-WEND-VIN-1
            ORDER BY LTCFILE.LTC_BRNO,
                     LTCFILE.LTC_VIN_1,
                     LTCFILE.LTC_ACCTNO,
                     LTCFILE.LTC_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN LTC2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLTC2-WBEG-BRNO
                          QLTC2-WEND-BRNO
                          QLTC2-WBEG-VIN-1
                          QLTC2-WEND-VIN-1.

           MOVE STAT---GOOD TO LTC2-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LTC2-FILE.
           PERFORM READ-IT.
           MOVE LTC-PATH-OWNBR  TO LTC-BRNO. 
           MOVE LTC-SELECT-PATH TO E-FILE.
           MOVE SPACES          TO E-KEYX.
           STRING LTC-BRNO, LTC-VIN-1 DELIMITED BY SIZE INTO E-KEYX.

           MOVE LTC-BRNO   TO QLTC-BRNO.
           MOVE LTC-VIN-1  TO QLTC-VIN-1.

           EXEC SQL
            SELECT LTCFILE.LTC_BRNO,
                   LTCFILE.LTC_ACCTNO,
                   LTCFILE.LTC_SEQNO,
                   ISNULL(CAST(LTCFILE.LTC_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LTCFILE.LTC_DESC_1,''),
                   ISNULL(LTCFILE.LTC_MAKE_1,''),
                   ISNULL(LTCFILE.LTC_MODEL_1,''),
                   ISNULL(LTCFILE.LTC_YEAR_1,''),
                   ISNULL(LTCFILE.LTC_STYLE_1,''),
                   ISNULL(LTCFILE.LTC_CLASS_1,''),
                   ISNULL(LTCFILE.LTC_VIN_1,''),
                   ISNULL(LTCFILE.LTC_TITLE_NO_1,''),
                   LTCFILE.LTC_MILEAGE_1,
                   LTCFILE.LTC_VALUE_1,
                   LTCFILE.LTC_SELLPR_1,
                   LTCFILE.LTC_MISC_AMT_1,
                   ISNULL(LTCFILE.LTC_SENT_FLAG_1,''),
                   ISNULL(CAST(LTCFILE.LTC_VIN_CHG_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LTCFILE.LTC_DESC_2,''),
                   ISNULL(LTCFILE.LTC_MAKE_2,''),
                   ISNULL(LTCFILE.LTC_MODEL_2,''),
                   ISNULL(LTCFILE.LTC_YEAR_2,''),
                   ISNULL(LTCFILE.LTC_STYLE_2,''),
                   ISNULL(LTCFILE.LTC_CLASS_2,''),
                   ISNULL(LTCFILE.LTC_VIN_2,''),
                   ISNULL(LTCFILE.LTC_TITLE_NO_2,''),
                   LTCFILE.LTC_MILEAGE_2,
                   LTCFILE.LTC_VALUE_2,
                   LTCFILE.LTC_SELLPR_2,
                   LTCFILE.LTC_MISC_AMT_2,
                   ISNULL(LTCFILE.LTC_SENT_FLAG_2,''),
                   ISNULL(CAST(LTCFILE.LTC_VIN_CHG_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LTCFILE.LTC_DESC_3,''),
                   ISNULL(LTCFILE.LTC_MAKE_3,''),
                   ISNULL(LTCFILE.LTC_MODEL_3,''),
                   ISNULL(LTCFILE.LTC_YEAR_3,''),
                   ISNULL(LTCFILE.LTC_STYLE_3,''),
                   ISNULL(LTCFILE.LTC_CLASS_3,''),
                   ISNULL(LTCFILE.LTC_VIN_3,''),
                   ISNULL(LTCFILE.LTC_TITLE_NO_3,''),
                   LTCFILE.LTC_MILEAGE_3,
                   LTCFILE.LTC_VALUE_3,
                   LTCFILE.LTC_SELLPR_3,
                   LTCFILE.LTC_MISC_AMT_3,
                   ISNULL(LTCFILE.LTC_SENT_FLAG_3,''),
                   ISNULL(CAST(LTCFILE.LTC_VIN_CHG_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LTCFILE.LTC_HHGS_1,''),
                   ISNULL(LTCFILE.LTC_HHGS_2,''),
                   ISNULL(LTCFILE.LTC_HHGS_3,'')
            INTO :QLTC-REC
            FROM DBO.LTCFILE
            WHERE LTCFILE.LTC_BRNO = :QLTC2-WBEG-BRNO
              AND LTCFILE.LTC_VIN_1 = :QLTC2-WBEG-VIN-1
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTC2-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LTC-FIELDS.

      *-----------------------------------------------------------------
       READ-LTC2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LTC-PATH-OWNBR  TO LTC-BRNO.
           MOVE LTC-SELECT-PATH TO E-FILE.
           MOVE SPACES          TO E-KEYX.
           STRING LTC-BRNO, LTC-VIN-1 DELIMITED BY SIZE INTO E-KEYX.

           INITIALIZE QLTC-REC.

           IF ( LTC2-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LTC2_CURSOR INTO :QLTC-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTC2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LTC-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LTC2-FILE.

           IF ( LTC2-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LTC2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LTC2-CURSOR-STAT.

      *-----------------------------------------------------------------
       LOAD-LTC3-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LTC-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LTC-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LTC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LTC-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LTC-ALL-DATA
              MOVE LTC-ALL-PATH             TO LTC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LTC-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LTC-ALL-BASE
              MOVE FILPATH-MACHINE         TO LTC-ALL-MACHINE
              MOVE FILPATH-DATA            TO LTC-ALL-DATA
              MOVE LTC-ALL-PATH             TO LTC-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-LTCFILE NOT = "S" )
              MOVE LTC-PATH-BEG             TO LTC-SELECT-PATH-BEG
              MOVE LTC-PATH-FILE            TO LTC-SELECT-PATH-FILE
           ELSE
              MOVE LTC-PATH-SQL             TO LTC-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-LTC3-FILE.
           PERFORM LOAD-LTC3-FILE.
           PERFORM OPEN-IT.
           MOVE LTC-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LTC3-FILE.
           PERFORM START-IT.
           MOVE LTC-PATH-OWNBR  TO LTC-BRNO.
           MOVE LTC-SELECT-PATH TO E-FILE.
           MOVE SPACES          TO E-KEYX.
           STRING LTC-BRNO, LTC-VIN-2 DELIMITED BY SIZE INTO E-KEYX.

           IF ( LTC3-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LTC3-FILE.

           IF ( QLTC3-WEND-BRNO = ZEROES    ) OR
              ( QLTC3-WEND-BRNO NOT NUMERIC )
              MOVE LTC-PATH-OWNBR        TO QLTC3-WBEG-BRNO
              MOVE LTC-PATH-OWNBR        TO QLTC3-WEND-BRNO
              MOVE LTC-VIN-2             TO QLTC3-WBEG-VIN-2
              MOVE ALL "Z"               TO QLTC3-WEND-VIN-2.

           EXEC SQL
            DECLARE LTC3_CURSOR CURSOR FOR
            SELECT LTCFILE.LTC_BRNO,
                   LTCFILE.LTC_ACCTNO,
                   LTCFILE.LTC_SEQNO,
                   ISNULL(CAST(LTCFILE.LTC_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LTCFILE.LTC_DESC_1,''),
                   ISNULL(LTCFILE.LTC_MAKE_1,''),
                   ISNULL(LTCFILE.LTC_MODEL_1,''),
                   ISNULL(LTCFILE.LTC_YEAR_1,''),
                   ISNULL(LTCFILE.LTC_STYLE_1,''),
                   ISNULL(LTCFILE.LTC_CLASS_1,''),
                   ISNULL(LTCFILE.LTC_VIN_1,''),
                   ISNULL(LTCFILE.LTC_TITLE_NO_1,''),
                   LTCFILE.LTC_MILEAGE_1,
                   LTCFILE.LTC_VALUE_1,
                   LTCFILE.LTC_SELLPR_1,
                   LTCFILE.LTC_MISC_AMT_1,
                   ISNULL(LTCFILE.LTC_SENT_FLAG_1,''),
                   ISNULL(CAST(LTCFILE.LTC_VIN_CHG_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LTCFILE.LTC_DESC_2,''),
                   ISNULL(LTCFILE.LTC_MAKE_2,''),
                   ISNULL(LTCFILE.LTC_MODEL_2,''),
                   ISNULL(LTCFILE.LTC_YEAR_2,''),
                   ISNULL(LTCFILE.LTC_STYLE_2,''),
                   ISNULL(LTCFILE.LTC_CLASS_2,''),
                   ISNULL(LTCFILE.LTC_VIN_2,''),
                   ISNULL(LTCFILE.LTC_TITLE_NO_2,''),
                   LTCFILE.LTC_MILEAGE_2,
                   LTCFILE.LTC_VALUE_2,
                   LTCFILE.LTC_SELLPR_2,
                   LTCFILE.LTC_MISC_AMT_2,
                   ISNULL(LTCFILE.LTC_SENT_FLAG_2,''),
                   ISNULL(CAST(LTCFILE.LTC_VIN_CHG_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LTCFILE.LTC_DESC_3,''),
                   ISNULL(LTCFILE.LTC_MAKE_3,''),
                   ISNULL(LTCFILE.LTC_MODEL_3,''),
                   ISNULL(LTCFILE.LTC_YEAR_3,''),
                   ISNULL(LTCFILE.LTC_STYLE_3,''),
                   ISNULL(LTCFILE.LTC_CLASS_3,''),
                   ISNULL(LTCFILE.LTC_VIN_3,''),
                   ISNULL(LTCFILE.LTC_TITLE_NO_3,''),
                   LTCFILE.LTC_MILEAGE_3,
                   LTCFILE.LTC_VALUE_3,
                   LTCFILE.LTC_SELLPR_3,
                   LTCFILE.LTC_MISC_AMT_3,
                   ISNULL(LTCFILE.LTC_SENT_FLAG_3,''),
                   ISNULL(CAST(LTCFILE.LTC_VIN_CHG_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LTCFILE.LTC_HHGS_1,''),
                   ISNULL(LTCFILE.LTC_HHGS_2,''),
                   ISNULL(LTCFILE.LTC_HHGS_3,'')
            FROM DBO.LTCFILE
            WHERE LTCFILE.LTC_BRNO >= :QLTC3-WBEG-BRNO
              AND LTCFILE.LTC_BRNO <= :QLTC3-WEND-BRNO
              AND LTCFILE.LTC_VIN_2 >= :QLTC3-WBEG-VIN-2
              AND LTCFILE.LTC_VIN_2 <= :QLTC3-WEND-VIN-2
            ORDER BY LTCFILE.LTC_BRNO,
                     LTCFILE.LTC_VIN_2,
                     LTCFILE.LTC_ACCTNO,
                     LTCFILE.LTC_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN LTC3_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLTC3-WBEG-BRNO
                          QLTC3-WEND-BRNO
                          QLTC3-WBEG-VIN-2
                          QLTC3-WEND-VIN-2.

           MOVE STAT---GOOD TO LTC3-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LTC3-FILE.
           PERFORM READ-IT.
           MOVE LTC-PATH-OWNBR  TO LTC-BRNO. 
           MOVE LTC-SELECT-PATH TO E-FILE.
           MOVE SPACES          TO E-KEYX.
           STRING LTC-BRNO, LTC-VIN-2 DELIMITED BY SIZE INTO E-KEYX.

           MOVE LTC-BRNO   TO QLTC-BRNO.
           MOVE LTC-VIN-2  TO QLTC-VIN-2.

           EXEC SQL
            SELECT LTCFILE.LTC_BRNO,
                   LTCFILE.LTC_ACCTNO,
                   LTCFILE.LTC_SEQNO,
                   ISNULL(CAST(LTCFILE.LTC_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LTCFILE.LTC_DESC_1,''),
                   ISNULL(LTCFILE.LTC_MAKE_1,''),
                   ISNULL(LTCFILE.LTC_MODEL_1,''),
                   ISNULL(LTCFILE.LTC_YEAR_1,''),
                   ISNULL(LTCFILE.LTC_STYLE_1,''),
                   ISNULL(LTCFILE.LTC_CLASS_1,''),
                   ISNULL(LTCFILE.LTC_VIN_1,''),
                   ISNULL(LTCFILE.LTC_TITLE_NO_1,''),
                   LTCFILE.LTC_MILEAGE_1,
                   LTCFILE.LTC_VALUE_1,
                   LTCFILE.LTC_SELLPR_1,
                   LTCFILE.LTC_MISC_AMT_1,
                   ISNULL(LTCFILE.LTC_SENT_FLAG_1,''),
                   ISNULL(CAST(LTCFILE.LTC_VIN_CHG_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LTCFILE.LTC_DESC_2,''),
                   ISNULL(LTCFILE.LTC_MAKE_2,''),
                   ISNULL(LTCFILE.LTC_MODEL_2,''),
                   ISNULL(LTCFILE.LTC_YEAR_2,''),
                   ISNULL(LTCFILE.LTC_STYLE_2,''),
                   ISNULL(LTCFILE.LTC_CLASS_2,''),
                   ISNULL(LTCFILE.LTC_VIN_2,''),
                   ISNULL(LTCFILE.LTC_TITLE_NO_2,''),
                   LTCFILE.LTC_MILEAGE_2,
                   LTCFILE.LTC_VALUE_2,
                   LTCFILE.LTC_SELLPR_2,
                   LTCFILE.LTC_MISC_AMT_2,
                   ISNULL(LTCFILE.LTC_SENT_FLAG_2,''),
                   ISNULL(CAST(LTCFILE.LTC_VIN_CHG_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LTCFILE.LTC_DESC_3,''),
                   ISNULL(LTCFILE.LTC_MAKE_3,''),
                   ISNULL(LTCFILE.LTC_MODEL_3,''),
                   ISNULL(LTCFILE.LTC_YEAR_3,''),
                   ISNULL(LTCFILE.LTC_STYLE_3,''),
                   ISNULL(LTCFILE.LTC_CLASS_3,''),
                   ISNULL(LTCFILE.LTC_VIN_3,''),
                   ISNULL(LTCFILE.LTC_TITLE_NO_3,''),
                   LTCFILE.LTC_MILEAGE_3,
                   LTCFILE.LTC_VALUE_3,
                   LTCFILE.LTC_SELLPR_3,
                   LTCFILE.LTC_MISC_AMT_3,
                   ISNULL(LTCFILE.LTC_SENT_FLAG_3,''),
                   ISNULL(CAST(LTCFILE.LTC_VIN_CHG_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LTCFILE.LTC_HHGS_1,''),
                   ISNULL(LTCFILE.LTC_HHGS_2,''),
                   ISNULL(LTCFILE.LTC_HHGS_3,'')
            INTO :QLTC-REC
            FROM DBO.LTCFILE
            WHERE LTCFILE.LTC_BRNO = :QLTC3-WBEG-BRNO
              AND LTCFILE.LTC_VIN_2 = :QLTC3-WBEG-VIN-2
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTC3-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LTC-FIELDS.

      *-----------------------------------------------------------------
       READ-LTC3-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LTC-PATH-OWNBR  TO LTC-BRNO.
           MOVE LTC-SELECT-PATH TO E-FILE.
           MOVE SPACES          TO E-KEYX.
           STRING LTC-BRNO, LTC-VIN-2 DELIMITED BY SIZE INTO E-KEYX.

           INITIALIZE QLTC-REC.

           IF ( LTC3-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LTC3_CURSOR INTO :QLTC-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTC3-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LTC-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LTC3-FILE.

           IF ( LTC3-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LTC3_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LTC3-CURSOR-STAT.

      *-----------------------------------------------------------------
       LOAD-LTC4-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LTC-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LTC-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LTC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LTC-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LTC-ALL-DATA
              MOVE LTC-ALL-PATH             TO LTC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LTC-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LTC-ALL-BASE
              MOVE FILPATH-MACHINE         TO LTC-ALL-MACHINE
              MOVE FILPATH-DATA            TO LTC-ALL-DATA
              MOVE LTC-ALL-PATH             TO LTC-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-LTCFILE NOT = "S" )
              MOVE LTC-PATH-BEG             TO LTC-SELECT-PATH-BEG
              MOVE LTC-PATH-FILE            TO LTC-SELECT-PATH-FILE
           ELSE
              MOVE LTC-PATH-SQL             TO LTC-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-LTC4-FILE.
           PERFORM LOAD-LTC4-FILE.
           PERFORM OPEN-IT.
           MOVE LTC-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LTC4-FILE.
           PERFORM START-IT.
           MOVE LTC-PATH-OWNBR  TO LTC-BRNO.
           MOVE LTC-SELECT-PATH TO E-FILE.
           MOVE SPACES          TO E-KEYX.
           STRING LTC-BRNO, LTC-VIN-3 DELIMITED BY SIZE INTO E-KEYX.

           IF ( LTC4-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LTC4-FILE.

           IF ( QLTC4-WEND-BRNO = ZEROES    ) OR
              ( QLTC4-WEND-BRNO NOT NUMERIC )
              MOVE LTC-PATH-OWNBR        TO QLTC4-WBEG-BRNO
              MOVE LTC-PATH-OWNBR        TO QLTC4-WEND-BRNO
              MOVE LTC-VIN-3             TO QLTC4-WBEG-VIN-3
              MOVE ALL "Z"               TO QLTC4-WEND-VIN-3.

           EXEC SQL
            DECLARE LTC4_CURSOR CURSOR FOR
            SELECT LTCFILE.LTC_BRNO,
                   LTCFILE.LTC_ACCTNO,
                   LTCFILE.LTC_SEQNO,
                   ISNULL(CAST(LTCFILE.LTC_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LTCFILE.LTC_DESC_1,''),
                   ISNULL(LTCFILE.LTC_MAKE_1,''),
                   ISNULL(LTCFILE.LTC_MODEL_1,''),
                   ISNULL(LTCFILE.LTC_YEAR_1,''),
                   ISNULL(LTCFILE.LTC_STYLE_1,''),
                   ISNULL(LTCFILE.LTC_CLASS_1,''),
                   ISNULL(LTCFILE.LTC_VIN_1,''),
                   ISNULL(LTCFILE.LTC_TITLE_NO_1,''),
                   LTCFILE.LTC_MILEAGE_1,
                   LTCFILE.LTC_VALUE_1,
                   LTCFILE.LTC_SELLPR_1,
                   LTCFILE.LTC_MISC_AMT_1,
                   ISNULL(LTCFILE.LTC_SENT_FLAG_1,''),
                   ISNULL(CAST(LTCFILE.LTC_VIN_CHG_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LTCFILE.LTC_DESC_2,''),
                   ISNULL(LTCFILE.LTC_MAKE_2,''),
                   ISNULL(LTCFILE.LTC_MODEL_2,''),
                   ISNULL(LTCFILE.LTC_YEAR_2,''),
                   ISNULL(LTCFILE.LTC_STYLE_2,''),
                   ISNULL(LTCFILE.LTC_CLASS_2,''),
                   ISNULL(LTCFILE.LTC_VIN_2,''),
                   ISNULL(LTCFILE.LTC_TITLE_NO_2,''),
                   LTCFILE.LTC_MILEAGE_2,
                   LTCFILE.LTC_VALUE_2,
                   LTCFILE.LTC_SELLPR_2,
                   LTCFILE.LTC_MISC_AMT_2,
                   ISNULL(LTCFILE.LTC_SENT_FLAG_2,''),
                   ISNULL(CAST(LTCFILE.LTC_VIN_CHG_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LTCFILE.LTC_DESC_3,''),
                   ISNULL(LTCFILE.LTC_MAKE_3,''),
                   ISNULL(LTCFILE.LTC_MODEL_3,''),
                   ISNULL(LTCFILE.LTC_YEAR_3,''),
                   ISNULL(LTCFILE.LTC_STYLE_3,''),
                   ISNULL(LTCFILE.LTC_CLASS_3,''),
                   ISNULL(LTCFILE.LTC_VIN_3,''),
                   ISNULL(LTCFILE.LTC_TITLE_NO_3,''),
                   LTCFILE.LTC_MILEAGE_3,
                   LTCFILE.LTC_VALUE_3,
                   LTCFILE.LTC_SELLPR_3,
                   LTCFILE.LTC_MISC_AMT_3,
                   ISNULL(LTCFILE.LTC_SENT_FLAG_3,''),
                   ISNULL(CAST(LTCFILE.LTC_VIN_CHG_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LTCFILE.LTC_HHGS_1,''),
                   ISNULL(LTCFILE.LTC_HHGS_2,''),
                   ISNULL(LTCFILE.LTC_HHGS_3,'')
            FROM DBO.LTCFILE
            WHERE LTCFILE.LTC_BRNO >= :QLTC4-WBEG-BRNO
              AND LTCFILE.LTC_BRNO <= :QLTC4-WEND-BRNO
              AND LTCFILE.LTC_VIN_3 >= :QLTC4-WBEG-VIN-3
              AND LTCFILE.LTC_VIN_3 <= :QLTC4-WEND-VIN-3
            ORDER BY LTCFILE.LTC_BRNO,
                     LTCFILE.LTC_VIN_3,
                     LTCFILE.LTC_ACCTNO,
                     LTCFILE.LTC_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN LTC4_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLTC4-WBEG-BRNO
                          QLTC4-WEND-BRNO
                          QLTC4-WBEG-VIN-3
                          QLTC4-WEND-VIN-3.

           MOVE STAT---GOOD TO LTC4-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LTC4-FILE.
           PERFORM READ-IT.
           MOVE LTC-PATH-OWNBR  TO LTC-BRNO. 
           MOVE LTC-SELECT-PATH TO E-FILE.
           MOVE SPACES          TO E-KEYX.
           STRING LTC-BRNO, LTC-VIN-3 DELIMITED BY SIZE INTO E-KEYX.

           MOVE LTC-BRNO   TO QLTC-BRNO.
           MOVE LTC-VIN-3  TO QLTC-VIN-3.

           EXEC SQL
            SELECT LTCFILE.LTC_BRNO,
                   LTCFILE.LTC_ACCTNO,
                   LTCFILE.LTC_SEQNO,
                   ISNULL(CAST(LTCFILE.LTC_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LTCFILE.LTC_DESC_1,''),
                   ISNULL(LTCFILE.LTC_MAKE_1,''),
                   ISNULL(LTCFILE.LTC_MODEL_1,''),
                   ISNULL(LTCFILE.LTC_YEAR_1,''),
                   ISNULL(LTCFILE.LTC_STYLE_1,''),
                   ISNULL(LTCFILE.LTC_CLASS_1,''),
                   ISNULL(LTCFILE.LTC_VIN_1,''),
                   ISNULL(LTCFILE.LTC_TITLE_NO_1,''),
                   LTCFILE.LTC_MILEAGE_1,
                   LTCFILE.LTC_VALUE_1,
                   LTCFILE.LTC_SELLPR_1,
                   LTCFILE.LTC_MISC_AMT_1,
                   ISNULL(LTCFILE.LTC_SENT_FLAG_1,''),
                   ISNULL(CAST(LTCFILE.LTC_VIN_CHG_DATE_1
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LTCFILE.LTC_DESC_2,''),
                   ISNULL(LTCFILE.LTC_MAKE_2,''),
                   ISNULL(LTCFILE.LTC_MODEL_2,''),
                   ISNULL(LTCFILE.LTC_YEAR_2,''),
                   ISNULL(LTCFILE.LTC_STYLE_2,''),
                   ISNULL(LTCFILE.LTC_CLASS_2,''),
                   ISNULL(LTCFILE.LTC_VIN_2,''),
                   ISNULL(LTCFILE.LTC_TITLE_NO_2,''),
                   LTCFILE.LTC_MILEAGE_2,
                   LTCFILE.LTC_VALUE_2,
                   LTCFILE.LTC_SELLPR_2,
                   LTCFILE.LTC_MISC_AMT_2,
                   ISNULL(LTCFILE.LTC_SENT_FLAG_2,''),
                   ISNULL(CAST(LTCFILE.LTC_VIN_CHG_DATE_2
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LTCFILE.LTC_DESC_3,''),
                   ISNULL(LTCFILE.LTC_MAKE_3,''),
                   ISNULL(LTCFILE.LTC_MODEL_3,''),
                   ISNULL(LTCFILE.LTC_YEAR_3,''),
                   ISNULL(LTCFILE.LTC_STYLE_3,''),
                   ISNULL(LTCFILE.LTC_CLASS_3,''),
                   ISNULL(LTCFILE.LTC_VIN_3,''),
                   ISNULL(LTCFILE.LTC_TITLE_NO_3,''),
                   LTCFILE.LTC_MILEAGE_3,
                   LTCFILE.LTC_VALUE_3,
                   LTCFILE.LTC_SELLPR_3,
                   LTCFILE.LTC_MISC_AMT_3,
                   ISNULL(LTCFILE.LTC_SENT_FLAG_3,''),
                   ISNULL(CAST(LTCFILE.LTC_VIN_CHG_DATE_3
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LTCFILE.LTC_HHGS_1,''),
                   ISNULL(LTCFILE.LTC_HHGS_2,''),
                   ISNULL(LTCFILE.LTC_HHGS_3,'')
            INTO :QLTC-REC
            FROM DBO.LTCFILE
            WHERE LTCFILE.LTC_BRNO = :QLTC4-WBEG-BRNO
              AND LTCFILE.LTC_VIN_3 = :QLTC4-WBEG-VIN-3
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTC4-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LTC-FIELDS.

      *-----------------------------------------------------------------
       READ-LTC4-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LTC-PATH-OWNBR  TO LTC-BRNO.
           MOVE LTC-SELECT-PATH TO E-FILE.
           MOVE SPACES          TO E-KEYX.
           STRING LTC-BRNO, LTC-VIN-3 DELIMITED BY SIZE INTO E-KEYX.

           INITIALIZE QLTC-REC.

           IF ( LTC4-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LTC4_CURSOR INTO :QLTC-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTC4-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LTC-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LTC4-FILE.

           IF ( LTC4-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LTC4_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LTC4-CURSOR-STAT.

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
              DESTROY IOLTCR-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
