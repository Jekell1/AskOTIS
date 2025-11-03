       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IORXRN.
       AUTHOR.        BAH
       DATE-WRITTEN.  04-20-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IORXRN.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : RXFILE 
      * TABLE DESC    : RESCISSION INDEX FILE
      * OUTPUT FILE   : XPC/IORXRN.PRE
      * USED FOR      : LIBLP/LPWSRX.CPY  --- QRX-REC
      *                 LIBLP/LPRX1R.CPY  --- READ
      *                 LIBLP/LPRX1RN.CPY --- READ/READ-NEXT/START
      *                 LIBLP/LPRX2RN.CPY --- READ/READ-NEXT/START
      *                 LIBLP/LPRX3RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * BAH 2021-0420 ***NEW***
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
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./IO/IORXRN.CBL S34 05/11/21-09:52:03 >".

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
       COPY "LIBLP/LP01RX.CPY".
       COPY "LIBLP/LPWSRX.CPY".

      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S LIBXX/XX01YY_SQL.CPY
      *#################################################################

      *=================================================================
      *
      *    Q - F I L E
      *
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QRX1-WHERE-BEG-KEY.
           03  QRX1-WBEG-BRNO             PIC 9(04).
           03  QRX1-WBEG-ACCTNO           PIC 9(08).
 
       01  QRX1-WHERE-END-KEY.
           03  QRX1-WEND-BRNO             PIC 9(04).
           03  QRX1-WEND-ACCTNO           PIC 9(08).

       01  QRX2-WHERE-BEG-KEY.
           03  QRX2-WBEG-BRNO             PIC 9(04).
           03  QRX2-WBEG-LNAME            PIC X(35).
           03  QRX2-WBEG-FNAME            PIC X(35).

       01  QRX2-WHERE-END-KEY.
           03  QRX2-WEND-BRNO             PIC 9(04).
           03  QRX2-WEND-LNAME            PIC X(35).
           03  QRX2-WEND-FNAME            PIC X(35).


       01  QRX3-WHERE-BEG-KEY.
           03  QRX3-WBEG-BRNO             PIC 9(04).
           03  QRX3-WBEG-DLNO             PIC 9(04)V99.
           03  QRX3-WBEG-INTDATE          PIC X(10).
           03  QRX3-WBEG-LNAME            PIC X(35).
           03  QRX3-WBEG-FNAME            PIC X(35).

       01  QRX3-WHERE-END-KEY.
           03  QRX3-WEND-BRNO             PIC 9(04).
           03  QRX3-WEND-DLNO             PIC 9(04)V99.
           03  QRX3-WEND-INTDATE          PIC X(10).
           03  QRX3-WEND-LNAME            PIC X(35).
           03  QRX3-WEND-FNAME            PIC X(35).
 
       01  QRX-REC.
           03  QRX-BRNO                    PIC 9(4).
           03  QRX-ACCTNO                  PIC 9(8).
           03  QRX-DLNO                    PIC 9(4)V99.
           03  QRX-INTDATE                 PIC X(10).
           03  QRX-LNAME                   PIC X(35).
           03  QRX-FNAME                   PIC X(35).
           03  QRX-LTOUCH-DATE             PIC X(10).
           03  QRX-LOANDATE                    PIC X(10).
           03  QRX-LOANAMT                     PIC 9(7)V99.
           03  QRX-PROCEEDS                    PIC 9(7)V99.
           03  QRX-STATUS                      PIC X(1).
           
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
           03  BEG-LNAME               PIC 9(03).
           03  END-LNAME               PIC 9(03).
           03  BEG-FNAME               PIC 9(03).
           03  END-FNAME               PIC 9(03).
           03  BEG-DLNO                PIC 9(4)V99.
           03  END-DLNO                PIC 9(4)V99.
           03  BEG-INTDATE             PIC 9(08).
           03  END-INTDATE             PIC 9(08).

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
           03  FILLER PIC X(30) VALUE "BRNO    ACCTNO      INTDATE   ".
           03  FILLER PIC X(30) VALUE "  LNAME             FNAME     ".
           03  FILLER PIC X(30) VALUE "         DLNO".

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-RX-BRNO                    PIC 9(04).
           03  FILLER                         PIC X(03).
           03  D01-RX-ACCTNO                  PIC 9(08).
           03  FILLER                         PIC X(03).
           03  D01-RX-INTDATE                 PIC 9999/99/99.
           03  FILLER                         PIC X(03).
           03  D01-RX-LNAME                   PIC X(15).
           03  FILLER                         PIC X(03).
           03  D01-RX-FNAME                   PIC X(15).
           03  FILLER                         PIC X(03).
           03  D01-RX-DLNO                    PIC 9(4).99.

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
       COPY "LIBIO/IORXRN_DEF.CPY".
       COPY "LIBIO/IORXRN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IORXRN_SCN.CPY".

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
           MOVE "IORXRN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IORXRN-QUEUE-POS  TO Q-POS.
           MOVE IORXRN-COPIES-POS TO C-POS.
           MOVE IORXRN-PRU-POS    TO P-POS.
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
              GO TO ENTER-END-INTDATE.

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
              ( VDUNUM0 > 3      ) 
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

           GO TO ENTER-BEG-LNAME.

      *-----------------------------------------------------------------
       ENTER-ALL-ACCTNO.

           MOVE "SNNN080BEG-ACCTNO." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-ACCTNO.
           PERFORM SCREENIO.

           MOVE "SNNN080END-ACCTNO." TO VDU.
           MOVE 99999999            TO VDUNUM0  END-ACCTNO.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-LNAME.

           MOVE "E  A350BEG-LNAME." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-ACCTNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-LNAME.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-LNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-LNAME.

      *-----------------------------------------------------------------
       ENTER-END-LNAME.

           MOVE "E  A350END-LNAME." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-LNAME.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-LNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-LNAME.

           IF ( BEG-LNAME > END-LNAME )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-LNAME.

           GO TO ENTER-BEG-FNAME.

      *-----------------------------------------------------------------
       ENTER-ALL-LNAME.

           MOVE "S  A000BEG-LNAME." TO VDU.
           MOVE SPACES              TO VDUBUF  BEG-LNAME.
           PERFORM SCREENIO.

           MOVE "S  A000END-LNAME." TO VDU.
           MOVE ALL "Z"             TO VDUBUF  END-LNAME.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-FNAME.

           MOVE "E  A150BEG-FNAME." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-LNAME.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-FNAME.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-FNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-FNAME.

      *-----------------------------------------------------------------
       ENTER-END-FNAME.

           MOVE "E  A150END-FNAME." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-FNAME.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-FNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-FNAME.

           IF ( BEG-FNAME > END-FNAME )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-FNAME.

           GO TO ENTER-BEG-DLNO.

      *-----------------------------------------------------------------
       ENTER-ALL-FNAME.

           MOVE "S  A000BEG-FNAME." TO VDU.
           MOVE SPACES              TO VDUBUF  BEG-FNAME.
           PERFORM SCREENIO.

           MOVE "S  A000END-FNAME." TO VDU.
           MOVE ALL "Z"             TO VDUBUF  END-FNAME.
           PERFORM SCREENIO.

      *=======================================================
       ENTER-BEG-DLNO.

           MOVE "ENNN042BEG-DLNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-FNAME.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-NUM.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-DLNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM2 TO BEG-DLNO.

      *-----------------------------------------------------------------
       ENTER-END-DLNO.

           MOVE "ENNN042END-DLNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-DLNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-DLNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM2 TO END-DLNO.

           IF ( BEG-DLNO > END-DLNO )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DLNO.

           GO TO ENTER-BEG-INTDATE.

      *-----------------------------------------------------------------
       ENTER-ALL-NUM.

           MOVE "SNNN042BEG-DLNO." TO VDU.
           MOVE ZEROES              TO VDUNUM2  BEG-DLNO.
           PERFORM SCREENIO.

           MOVE "SNNN042END-DLNO." TO VDU.
           MOVE 9999.99             TO VDUNUM2  END-DLNO.
           PERFORM SCREENIO.

      *=================================================================

       ENTER-BEG-INTDATE.

           MOVE SPACES  TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ERNM060BEG-INTDATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-DLNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-INTDATE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-INTDATE.

           MOVE VDU-DATE-YYYYMMDD TO BEG-INTDATE.

      *-----------------------------------------------------------------
       ENTER-END-INTDATE.

           MOVE "ERNM060END-INTDATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-INTDATE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-INTDATE.

           MOVE VDU-DATE-YYYYMMDD TO END-INTDATE.

           IF ( BEG-INTDATE > END-INTDATE )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-INTDATE.

           GO TO ENTER-ACCEPT-YN.

       ENTER-ALL-INTDATE.

           MOVE "S  8080BEG-INTDATE." TO VDU.
           MOVE JULIAN-BEG  TO BEG-INTDATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "S  8080END-INTDATE." TO VDU.
           MOVE JULIAN-END  TO END-INTDATE VDU-DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-ACCEPT-YN.

           MOVE LEGEND-ACCEPT-YN TO LEGEND.
           PERFORM SEND-LEGEND.
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

           MOVE "S  A350BEG-LNAME."      TO VDU.
           MOVE BEG-LNAME                TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A350END-LNAME."      TO VDU.
           MOVE END-LNAME                TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A150BEG-FNAME."      TO VDU.
           MOVE BEG-FNAME                TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A150END-FNAME."      TO VDU.
           MOVE END-FNAME                TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SNNN042BEG-DLNO."       TO VDU.
           MOVE BEG-DLNO                 TO VDUNUM2.
           PERFORM SCREENIO.

           MOVE "SNNN042END-DLNO."       TO VDU.
           MOVE END-DLNO                 TO VDUNUM2.
           PERFORM SCREENIO.

           MOVE "S  A000BEG-INTDATE."    TO VDU.
           MOVE BEG-INTDATE              TO VDU-DATE-EDIT.
           PERFORM SCREENIO.

           MOVE "S  A000END-INTDATE."    TO VDU.
           MOVE END-INTDATE              TO VDU-DATE-EDIT.
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

           MOVE "B"                        TO RX-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - -{ SET WHERE BEG & END FIELDS }
           MOVE BR-NO     TO QRX1-WBEG-BRNO 
                              QRX2-WBEG-BRNO
                              QRX3-WBEG-BRNO.

           MOVE BR-NO     TO QRX1-WEND-BRNO
                              QRX2-WEND-BRNO
                              QRX3-WEND-BRNO.

           MOVE BEG-ACCTNO  TO QRX1-WBEG-ACCTNO.
           MOVE END-ACCTNO  TO QRX1-WEND-ACCTNO.

           MOVE BEG-LNAME  TO QRX2-WBEG-LNAME
                              QRX3-WBEG-LNAME.
           MOVE END-LNAME  TO QRX2-WEND-LNAME
                              QRX3-WEND-LNAME.
           MOVE BEG-FNAME  TO QRX2-WBEG-FNAME
                              QRX3-WBEG-FNAME.
           MOVE END-FNAME  TO QRX2-WEND-FNAME
                              QRX3-WEND-FNAME.

           MOVE BEG-DLNO  TO QRX3-WBEG-DLNO.
           MOVE END-DLNO  TO QRX3-WEND-DLNO.

           MOVE BEG-INTDATE         TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QRX3-WBEG-INTDATE.
           MOVE END-INTDATE         TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QRX3-WEND-INTDATE.

      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           IF ( PROCESS-KEY = 1 )
              PERFORM OPEN-RX1-FILE
              PERFORM START-RX1-FILE
           ELSE
           IF ( PROCESS-KEY = 2 )
              PERFORM OPEN-RX2-FILE
              PERFORM START-RX2-FILE
           ELSE
              PERFORM OPEN-RX3-FILE
              PERFORM START-RX3-FILE.

      *=================================================================
       PROCESS-BRANCH-NEXT.

           IF ( PROCESS-KEY = 1 )
              PERFORM READ-RX1-FILE-NEXT
           ELSE
           IF ( PROCESS-KEY = 2 )
              PERFORM READ-RX2-FILE-NEXT
           ELSE
              PERFORM READ-RX3-FILE-NEXT.
           IF ( IO-BAD ) OR ( RX-BRNO NOT = RX-PATH-OWNBR )
              GO TO PROCESS-BRANCH-MODULE-END.

           ADD 01 TO RECORDS-READ.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

           IF ( PROCESS-KEY = 1 )
              IF ( RX-ACCTNO < BEG-ACCTNO )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( RX-ACCTNO > END-ACCTNO )
                 GO TO PROCESS-BRANCH-MODULE-END.

           IF ( PROCESS-KEY = 2 )
              IF ( RX-LNAME  < BEG-LNAME  )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( RX-LNAME  > END-LNAME  )
                 GO TO PROCESS-BRANCH-MODULE-END
              ELSE
              IF ( RX-FNAME < BEG-FNAME ) OR
                 ( RX-FNAME > END-FNAME )
                 GO TO PROCESS-BRANCH-NEXT.

           IF ( PROCESS-KEY = 3 )
              IF ( RX-DLNO < BEG-DLNO )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( RX-DLNO > END-DLNO )
                 GO TO PROCESS-BRANCH-MODULE-END
              ELSE
              IF ( RX-INTDATE < BEG-INTDATE ) OR
                 ( RX-INTDATE > END-INTDATE )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( RX-LNAME < BEG-LNAME ) OR
                 ( RX-LNAME > END-LNAME )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( RX-FNAME < BEG-FNAME ) OR
                 ( RX-FNAME > END-FNAME )
                 GO TO PROCESS-BRANCH-NEXT.


      *-----------------------------------------------------------------
       PROCESS-BRANCH-PRINT.

      *-------------------------------------------------------{ RANGES }
           MOVE RX-BRNO                 TO D01-RX-BRNO.
           MOVE RX-ACCTNO               TO D01-RX-ACCTNO.
           MOVE RX-INTDATE              TO D01-RX-INTDATE.
           MOVE RX-LNAME                TO D01-RX-LNAME.
           MOVE RX-FNAME                TO D01-RX-FNAME.
           MOVE RX-DLNO                 TO D01-RX-DLNO.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.
           GO TO PROCESS-BRANCH-NEXT.

      *=================================================================
       PROCESS-BRANCH-MODULE-END.

           IF ( PROCESS-KEY = 1 )
              PERFORM CLOSE-RX1-FILE
           ELSE
           IF ( PROCESS-KEY = 2 )
              PERFORM CLOSE-RX2-FILE
           ELSE
              PERFORM CLOSE-RX3-FILE.

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
              MOVE RX1-KEY TO PACE-DETAIL-KEY
           ELSE
           IF ( PROCESS-KEY = 2 )
              STRING RX-BRNO, RX-LNAME, RX-FNAME
                     DELIMITED BY SIZE INTO PACE-DETAIL-KEY
           ELSE
              STRING RX-BRNO, RX-DLNO, RX-INTDATE, RX-LNAME,
                     RX-FNAME
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
           MOVE "BEGINNING LNAME              : "    TO R01-PROMPT.
           MOVE BEG-LNAME                            TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    LNAME              : "    TO R01-PROMPT.
           MOVE END-LNAME                            TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING FNAME              : "    TO R01-PROMPT.
           MOVE BEG-FNAME                            TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    FNAME              : "    TO R01-PROMPT.
           MOVE END-FNAME                            TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.


      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING DLNO              : "    TO R01-PROMPT.
           MOVE BEG-DLNO                            TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    DLNO              : "    TO R01-PROMPT.
           MOVE END-DLNO                            TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING INTDATE              : "    TO R01-PROMPT.
           MOVE BEG-INTDATE                            TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    INTDATE              : "    TO R01-PROMPT.
           MOVE END-INTDATE                            TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "STARTING TIME                 : "    TO R01-PROMPT.
           MOVE STARTING-TIME                         TO R01-NUMERIC.
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
           MOVE "SQL TABLE IO UTILITY - RXFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IORXRN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IORXRN-SYSDATE.
           DISPLAY IORXRN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IORXRN-FORM-FIELDS TO WR-BUF.
           MOVE IORXRN-FORM-LIST TO WR-LIST.
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

           MOVE IORXRN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IORXRN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IORXRN_EVA.CPY".
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
      *    LOAD RX-REC FROM QRX-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-RX-FIELDS.
           MOVE QRX-BRNO TO RX-BRNO.
           MOVE QRX-ACCTNO TO RX-ACCTNO.
           MOVE QRX-DLNO TO RX-DLNO.

           IF ( QRX-INTDATE = "1900-01-01" )
              MOVE ZEROES TO RX-INTDATE
           ELSE
              MOVE QRX-INTDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO RX-INTDATE.

           MOVE QRX-LNAME TO RX-LNAME.
           MOVE QRX-FNAME TO RX-FNAME.

           IF ( QRX-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO RX-LTOUCH-DATE
           ELSE
              MOVE QRX-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO RX-LTOUCH-DATE.


           IF ( QRX-LOANDATE = "1900-01-01" )
              MOVE ZEROES TO RX-LOANDATE
           ELSE
              MOVE QRX-LOANDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO RX-LOANDATE.

           MOVE QRX-LOANAMT TO RX-LOANAMT.
           MOVE QRX-PROCEEDS TO RX-PROCEEDS.
           MOVE QRX-STATUS TO RX-STATUS.
 
      *-----------------------------------------------------------------
      *    LOAD QRX-REC FROM RX-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-RX-FIELDS.
           MOVE RX-BRNO TO QRX-BRNO.
           MOVE RX-ACCTNO TO QRX-ACCTNO.
           MOVE RX-DLNO TO QRX-DLNO.

           IF ( RX-INTDATE = ZEROES )
              MOVE "1900-01-01" TO QRX-INTDATE
           ELSE
              MOVE RX-INTDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QRX-INTDATE.

           MOVE RX-LNAME TO QRX-LNAME.
           MOVE RX-FNAME TO QRX-FNAME.

           IF ( RX-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QRX-LTOUCH-DATE
           ELSE
              MOVE RX-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QRX-LTOUCH-DATE.


           IF ( RX-LOANDATE = ZEROES )
              MOVE "1900-01-01" TO QRX-LOANDATE
           ELSE
              MOVE RX-LOANDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QRX-LOANDATE.

           MOVE RX-LOANAMT TO QRX-LOANAMT.
           MOVE RX-PROCEEDS TO QRX-PROCEEDS.
           MOVE RX-STATUS TO QRX-STATUS.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-RX1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( RX-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( RX-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO RX-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO RX-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO RX-ALL-DATA
              MOVE RX-ALL-PATH             TO RX-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( RX-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO RX-ALL-BASE
              MOVE FILPATH-MACHINE         TO RX-ALL-MACHINE
              MOVE FILPATH-DATA            TO RX-ALL-DATA
              MOVE RX-ALL-PATH             TO RX-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-RXFILE NOT = "S" )
              MOVE RX-PATH-BEG             TO RX-SELECT-PATH-BEG
              MOVE RX-PATH-FILE            TO RX-SELECT-PATH-FILE
           ELSE
              MOVE RX-PATH-SQL             TO RX-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-RX1-FILE.
           PERFORM LOAD-RX1-FILE.
           PERFORM OPEN-IT.
           MOVE RX-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-RX1-FILE.
           PERFORM START-IT.
           MOVE RX-PATH-OWNBR  TO RX-BRNO.
           MOVE RX-SELECT-PATH TO E-FILE.
           MOVE RX1-KEY        TO E-KEYX.

           IF ( RX1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-RX1-FILE.

           IF ( QRX1-WEND-ACCTNO = ZEROES    ) OR
              ( QRX1-WEND-ACCTNO NOT NUMERIC )
              MOVE RX-PATH-OWNBR        TO QRX1-WBEG-BRNO
              MOVE RX-PATH-OWNBR        TO QRX1-WEND-BRNO
              MOVE RX-ACCTNO            TO QRX1-WBEG-ACCTNO
              MOVE ALL "9"              TO QRX1-WEND-ACCTNO.

           EXEC SQL
            DECLARE RX1_CURSOR CURSOR FOR
            SELECT RXFILE.RX_BRNO,
                   RXFILE.RX_ACCTNO,
                   RXFILE.RX_DLNO,
                   ISNULL(CAST(RXFILE.RX_INTDATE AS VARCHAR(10)),'0'),
                   ISNULL(RXFILE.RX_LNAME,''),
                   ISNULL(RXFILE.RX_FNAME,''),
                   ISNULL(CAST(RXFILE.RX_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(RXFILE.RX_LOANDATE AS VARCHAR(10)),'0'),
                   RXFILE.RX_LOANAMT,
                   RXFILE.RX_PROCEEDS,
                   ISNULL(RXFILE.RX_STATUS,'')
            FROM DBO.RXFILE
            WHERE RXFILE.RX_BRNO >= :QRX1-WBEG-BRNO
              AND RXFILE.RX_BRNO <= :QRX1-WEND-BRNO
              AND RXFILE.RX_ACCTNO >= :QRX1-WBEG-ACCTNO
              AND RXFILE.RX_ACCTNO <= :QRX1-WEND-ACCTNO
            ORDER BY RXFILE.RX_BRNO,
                     RXFILE.RX_ACCTNO
           END-EXEC.

           EXEC SQL
               OPEN RX1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QRX1-WBEG-BRNO 
                          QRX1-WEND-BRNO
                          QRX1-WBEG-ACCTNO
                          QRX1-WEND-ACCTNO.
 
           MOVE STAT---GOOD TO RX1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-RX1-FILE.
           PERFORM READ-IT.
           MOVE RX-PATH-OWNBR  TO RX-BRNO. 
           MOVE RX-SELECT-PATH TO E-FILE.
           MOVE RX1-KEY        TO E-KEYX.

           MOVE RX-BRNO   TO QRX-BRNO.
           MOVE RX-ACCTNO TO QRX-ACCTNO.

           EXEC SQL
            SELECT RXFILE.RX_BRNO,
                   RXFILE.RX_ACCTNO,
                   RXFILE.RX_DLNO,
                   ISNULL(CAST(RXFILE.RX_INTDATE AS VARCHAR(10)),'0'),
                   ISNULL(RXFILE.RX_LNAME,''),
                   ISNULL(RXFILE.RX_FNAME,''),
                   ISNULL(CAST(RXFILE.RX_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(RXFILE.RX_LOANDATE AS VARCHAR(10)),'0'),
                   RXFILE.RX_LOANAMT,
                   RXFILE.RX_PROCEEDS,
                   ISNULL(RXFILE.RX_STATUS,'')
            INTO :QRX-REC
            FROM DBO.RXFILE
            WHERE RXFILE.RX_BRNO = :QRX-BRNO
              AND RXFILE.RX_ACCTNO = :QRX-ACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-RX1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-RX-FIELDS.

      *-----------------------------------------------------------------
       READ-RX1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE RX-PATH-OWNBR  TO RX-BRNO.
           MOVE RX-SELECT-PATH TO E-FILE.
           MOVE RX1-KEY        TO E-KEYX.
           INITIALIZE QRX-REC.

           IF ( RX1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH RX1_CURSOR INTO :QRX-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-RX1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-RX-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-RX1-FILE.

           IF ( RX1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE RX1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO RX1-CURSOR-STAT.


      *#################################################################
       LOAD-RX2-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( RX-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( RX-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO RX-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO RX-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO RX-ALL-DATA
              MOVE RX-ALL-PATH             TO RX-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( RX-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO RX-ALL-BASE
              MOVE FILPATH-MACHINE         TO RX-ALL-MACHINE
              MOVE FILPATH-DATA            TO RX-ALL-DATA
              MOVE RX-ALL-PATH             TO RX-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-RXFILE NOT = "S" )
              MOVE RX-PATH-BEG             TO RX-SELECT-PATH-BEG
              MOVE RX-PATH-FILE            TO RX-SELECT-PATH-FILE
           ELSE
              MOVE RX-PATH-SQL             TO RX-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-RX2-FILE.
           PERFORM LOAD-RX2-FILE.
           PERFORM OPEN-IT.
           MOVE RX-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-RX2-FILE.
           PERFORM START-IT.
           MOVE RX-PATH-OWNBR  TO RX-BRNO.
           MOVE RX-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING RX-BRNO, RX-LNAME, RX-FNAME
                  DELIMITED BY SIZE INTO E-KEYX.

           IF ( RX2-CURSOR-STAT-GOOD )
              PERFORM CLOSE-RX2-FILE.

           IF ( QRX2-WEND-BRNO = ZEROES    ) OR
              ( QRX2-WEND-BRNO NOT NUMERIC )
            MOVE RX-PATH-OWNBR        TO QRX2-WBEG-BRNO
            MOVE RX-PATH-OWNBR        TO QRX2-WEND-BRNO
            MOVE RX-LNAME             TO QRX2-WBEG-LNAME
            MOVE ALL "Z"              TO QRX2-WEND-LNAME
            MOVE RX-FNAME             TO QRX2-WBEG-FNAME
            MOVE ALL "Z"              TO QRX2-WEND-FNAME.

           EXEC SQL
            DECLARE RX2_CURSOR CURSOR FOR
            SELECT RXFILE.RX_BRNO,
                   RXFILE.RX_ACCTNO,
                   RXFILE.RX_DLNO,
                   ISNULL(CAST(RXFILE.RX_INTDATE AS VARCHAR(10)),'0'),
                   ISNULL(RXFILE.RX_LNAME,''),
                   ISNULL(RXFILE.RX_FNAME,''),
                   ISNULL(CAST(RXFILE.RX_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(RXFILE.RX_LOANDATE AS VARCHAR(10)),'0'),
                   RXFILE.RX_LOANAMT,
                   RXFILE.RX_PROCEEDS,
                   ISNULL(RXFILE.RX_STATUS,'')
            FROM DBO.RXFILE
            WHERE RXFILE.RX_BRNO >= :QRX2-WBEG-BRNO
              AND RXFILE.RX_BRNO <= :QRX2-WEND-BRNO
              AND RXFILE.RX_LNAME >= :QRX2-WBEG-LNAME
              AND RXFILE.RX_LNAME <= :QRX2-WEND-LNAME
              AND RXFILE.RX_FNAME >= :QRX2-WBEG-FNAME
              AND RXFILE.RX_FNAME <= :QRX2-WEND-FNAME
            ORDER BY RXFILE.RX_BRNO,
                     RXFILE.RX_LNAME,
                     RXFILE.RX_FNAME,
                     RXFILE.RX_ACCTNO
           END-EXEC.

           EXEC SQL
               OPEN RX2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QRX2-WBEG-BRNO 
                          QRX2-WEND-BRNO.

           MOVE SPACES TO QRX2-WBEG-LNAME
                          QRX2-WEND-LNAME
                          QRX2-WBEG-FNAME
                          QRX2-WEND-FNAME.
 
           MOVE STAT---GOOD TO RX2-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-RX2-FILE.
           PERFORM READ-IT.
           MOVE RX-PATH-OWNBR  TO RX-BRNO. 
           MOVE RX-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING RX-BRNO, RX-LNAME, RX-FNAME
                  DELIMITED BY SIZE INTO E-KEYX.

           MOVE RX-BRNO   TO QRX-BRNO.
           MOVE RX-LNAME TO QRX-LNAME.
           MOVE RX-FNAME TO QRX-FNAME.

           EXEC SQL
            SELECT RXFILE.RX_BRNO,
                   RXFILE.RX_ACCTNO,
                   RXFILE.RX_DLNO,
                   ISNULL(CAST(RXFILE.RX_INTDATE AS VARCHAR(10)),'0'),
                   ISNULL(RXFILE.RX_LNAME,''),
                   ISNULL(RXFILE.RX_FNAME,''),
                   ISNULL(CAST(RXFILE.RX_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(RXFILE.RX_LOANDATE AS VARCHAR(10)),'0'),
                   RXFILE.RX_LOANAMT,
                   RXFILE.RX_PROCEEDS,
                   ISNULL(RXFILE.RX_STATUS,'')
            INTO :QRX-REC
            FROM DBO.RXFILE
            WHERE RXFILE.RX_BRNO = :QRX-BRNO
              AND RXFILE.RX_LNAME = :QRX-LNAME
              AND RXFILE.RX_FNAME = :QRX-FNAME
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-RX2-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-RX-FIELDS.

      *-----------------------------------------------------------------
       READ-RX2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE RX-PATH-OWNBR  TO RX-BRNO.
           MOVE RX-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING RX-BRNO, RX-LNAME, RX-FNAME
                  DELIMITED BY SIZE INTO E-KEYX.

           INITIALIZE QRX-REC.

           IF ( RX2-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH RX2_CURSOR INTO :QRX-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-RX2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-RX-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-RX2-FILE.

           IF ( RX2-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE RX2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO RX2-CURSOR-STAT.

      *=============================================================
       LOAD-RX3-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( RX-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( RX-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO RX-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO RX-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO RX-ALL-DATA
              MOVE RX-ALL-PATH             TO RX-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( RX-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO RX-ALL-BASE
              MOVE FILPATH-MACHINE         TO RX-ALL-MACHINE
              MOVE FILPATH-DATA            TO RX-ALL-DATA
              MOVE RX-ALL-PATH             TO RX-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-RXFILE NOT = "S" )
              MOVE RX-PATH-BEG             TO RX-SELECT-PATH-BEG
              MOVE RX-PATH-FILE            TO RX-SELECT-PATH-FILE
           ELSE
              MOVE RX-PATH-SQL             TO RX-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-RX3-FILE.
           PERFORM LOAD-RX3-FILE.
           PERFORM OPEN-IT.
           MOVE RX-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-RX3-FILE.
           PERFORM START-IT.
           MOVE RX-PATH-OWNBR  TO RX-BRNO.
           MOVE RX-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING RX-BRNO, RX-LNAME, RX-FNAME
                           DELIMITED BY SIZE INTO E-KEYX.

           IF ( RX3-CURSOR-STAT-GOOD )
              PERFORM CLOSE-RX3-FILE.

           IF ( QRX3-WEND-BRNO = ZEROES    ) OR
              ( QRX3-WEND-BRNO NOT NUMERIC )
              MOVE RX-PATH-OWNBR        TO QRX3-WBEG-BRNO
              MOVE RX-PATH-OWNBR        TO QRX3-WEND-BRNO
              MOVE RX-DLNO              TO QRX3-WBEG-DLNO
              MOVE ALL "9"              TO QRX3-WEND-DLNO
              MOVE RX-INTDATE           TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QRX3-WBEG-INTDATE
              MOVE JULIAN-END           TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QRX3-WEND-INTDATE
              MOVE RX-LNAME            TO QRX3-WBEG-LNAME
              MOVE ALL "Z"              TO QRX3-WEND-LNAME
              MOVE RX-FNAME            TO QRX3-WBEG-FNAME
              MOVE ALL "Z"              TO QRX3-WEND-FNAME.

           EXEC SQL
            DECLARE RX3_CURSOR CURSOR FOR
            SELECT RXFILE.RX_BRNO,
                   RXFILE.RX_ACCTNO,
                   RXFILE.RX_DLNO,
                   ISNULL(CAST(RXFILE.RX_INTDATE AS VARCHAR(10)),'0'),
                   ISNULL(RXFILE.RX_LNAME,''),
                   ISNULL(RXFILE.RX_FNAME,''),
                   ISNULL(CAST(RXFILE.RX_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(RXFILE.RX_LOANDATE AS VARCHAR(10)),'0'),
                   RXFILE.RX_LOANAMT,
                   RXFILE.RX_PROCEEDS,
                   ISNULL(RXFILE.RX_STATUS,'')
            FROM DBO.RXFILE
            WHERE RXFILE.RX_BRNO >= :QRX3-WBEG-BRNO
              AND RXFILE.RX_BRNO <= :QRX3-WEND-BRNO
              AND RXFILE.RX_DLNO >= :QRX3-WBEG-DLNO
              AND RXFILE.RX_DLNO <= :QRX3-WEND-DLNO
              AND CAST(RXFILE.RX_INTDATE AS VARCHAR(10))
                          >= :QRX3-WBEG-INTDATE
              AND CAST(RXFILE.RX_INTDATE AS VARCHAR(10))
                          <= :QRX3-WEND-INTDATE
              AND RXFILE.RX_LNAME >= :QRX3-WBEG-LNAME
              AND RXFILE.RX_LNAME <= :QRX3-WEND-LNAME
              AND RXFILE.RX_FNAME >= :QRX3-WBEG-FNAME
              AND RXFILE.RX_FNAME <= :QRX3-WEND-FNAME
            ORDER BY RXFILE.RX_BRNO,
                     RXFILE.RX_DLNO,
                     RXFILE.RX_INTDATE,
                     RXFILE.RX_LNAME,
                     RXFILE.RX_FNAME,
                     RXFILE.RX_ACCTNO
           END-EXEC.

           EXEC SQL
               OPEN RX3_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QRX3-WBEG-BRNO 
                          QRX3-WEND-BRNO
                          QRX3-WBEG-DLNO
                          QRX3-WEND-DLNO.

           MOVE SPACES TO QRX3-WBEG-LNAME
                          QRX3-WEND-LNAME
                          QRX3-WBEG-FNAME
                          QRX3-WEND-FNAME
                          QRX3-WBEG-INTDATE
                          QRX3-WEND-INTDATE.
 
           MOVE STAT---GOOD TO RX3-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-RX3-FILE.
           PERFORM READ-IT.
           MOVE RX-PATH-OWNBR  TO RX-BRNO. 
           MOVE RX-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING RX-BRNO, RX-LNAME, RX-FNAME
                           DELIMITED BY SIZE INTO E-KEYX.

           MOVE RX-BRNO             TO QRX-BRNO.
           MOVE RX-DLNO             TO QRX-DLNO.
           MOVE RX-INTDATE          TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QRX-INTDATE.
           MOVE RX-LNAME            TO QRX-LNAME.
           MOVE RX-FNAME            TO QRX-FNAME.

           EXEC SQL
            SELECT RXFILE.RX_BRNO,
                   RXFILE.RX_ACCTNO,
                   RXFILE.RX_DLNO,
                   ISNULL(CAST(RXFILE.RX_INTDATE AS VARCHAR(10)),'0'),
                   ISNULL(RXFILE.RX_LNAME,''),
                   ISNULL(RXFILE.RX_FNAME,''),
                   ISNULL(CAST(RXFILE.RX_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(RXFILE.RX_LOANDATE AS VARCHAR(10)),'0'),
                   RXFILE.RX_LOANAMT,
                   RXFILE.RX_PROCEEDS,
                   ISNULL(RXFILE.RX_STATUS,'')
            INTO :QRX-REC
            FROM DBO.RXFILE
            WHERE RXFILE.RX_BRNO = :QRX-BRNO
              AND RXFILE.RX_DLNO = :QRX-DLNO
              AND RXFILE.RX_INTDATE = :QRX-INTDATE
              AND RXFILE.RX_LNAME = :QRX-LNAME
              AND RXFILE.RX_FNAME = :QRX-FNAME
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-RX3-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-RX-FIELDS.

      *-----------------------------------------------------------------
       READ-RX3-FILE-NEXT.
           PERFORM READ-IT.
           MOVE RX-PATH-OWNBR  TO RX-BRNO.
           MOVE RX-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING RX-BRNO, RX-LNAME, RX-FNAME
                           DELIMITED BY SIZE INTO E-KEYX.

           INITIALIZE QRX-REC.

           IF ( RX3-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH RX3_CURSOR INTO :QRX-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-RX3-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-RX-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-RX3-FILE.

           IF ( RX3-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE RX3_CURSOR
              END-EXEC
              MOVE STAT---BAD TO RX3-CURSOR-STAT.

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
              DESTROY IORXRN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
