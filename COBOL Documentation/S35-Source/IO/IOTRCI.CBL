       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOTRCI.
       AUTHOR.        BAH.
       DATE-WRITTEN.  04-12-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOTRCI.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : TRCFILE 
      * TABLE DESC    : TRANSCATION FILE CHECK/CASH  FILE
      * OUTPUT FILE   : XPC/IOTRCI.PRE
      * USED FOR      : LIBLP/LPWSTRC.CPY  --- QTRC-REC
      *                 LIBLP/LPTRC1I.CPY  --- READ/WRITE/DELETE
      *                 LIBLP/LPTRC1IN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * BAH 2021-0412 ***NEW***
      * BAH 2021-1027 MISSING TC-EXTRACT-FG AND TC-DEBIT-REFCD
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
           "@(#)IO/IOTRCI.CBL S34 10/04/21-12:02:22 >".

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
       COPY "LIBLP/LP01TRC.CPY".
       COPY "LIBLP/LPWSTRC.CPY".

      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S LIBXX/XX01YY_SQL.CPY
      *#################################################################

      *=================================================================
      *
      *    Q - F I L E
      *
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QTRC1-WHERE-BEG-KEY.
           03  QTRC1-WBEG-BRNO             PIC 9(04).
           03  QTRC1-WBEG-KEY-DATE         PIC X(10).
           03  QTRC1-WBEG-CKNO             PIC 9(06).
           03  QTRC1-WBEG-CLASS            PIC 9(03).
           03  QTRC1-WBEG-SEQ              PIC 9(05).

       01  QTRC1-WHERE-END-KEY.
           03  QTRC1-WEND-BRNO             PIC 9(04).
           03  QTRC1-WEND-KEY-DATE         PIC X(10).
           03  QTRC1-WEND-CKNO             PIC 9(06).
           03  QTRC1-WEND-CLASS            PIC 9(03).
           03  QTRC1-WEND-SEQ              PIC 9(05).

       01  QTRC-REC.
           03  QTC-BRNO             PIC 9(4).
           03  QTC-KEY-DATE         PIC X(10).
           03  QTC-CKNO             PIC 9(6).
           03  QTC-CLASS            PIC 9(3).
           03  QTC-SEQ              PIC 9(5).
           03  QTC-LTOUCH-DATE      PIC X(10).
           03  QTC-TYPE             PIC X(2).
           03  QTC-DATE             PIC X(10).
           03  QTC-AMT              PIC S9(7)V99.
           03  QTC-PAYEE            PIC X(60).
           03  QTC-REF              PIC 9(6).
           03  QTC-REASON           PIC X(30).
           03  QTC-GLNO-1           PIC 9(11).
           03  QTC-GLAMT-1          PIC S9(7)V99.
           03  QTC-GLNO-2           PIC 9(11).
           03  QTC-GLAMT-2          PIC S9(7)V99.
           03  QTC-GLNO-3           PIC 9(11).
           03  QTC-GLAMT-3          PIC S9(7)V99.
           03  QTC-GLNO-4           PIC 9(11).
           03  QTC-GLAMT-4          PIC S9(7)V99.
           03  QTC-GLNO-5           PIC 9(11).
           03  QTC-GLAMT-5          PIC S9(7)V99.
           03  QTC-VOID             PIC X.
           03  QTC-ACCTNO           PIC 9(8).
           03  QTC-PAID             PIC X.
           03  QTC-PGM              PIC XX.
           03  QTC-DLRVNO           PIC 9.
           03  QTC-SPRCLASS         PIC 999.
           03  QTC-DLNO             PIC 9(4)V99.
           03  QTC-USERID           PIC X(10).
           03  QTC-POSTDATE         PIC X(10).
           03  QTC-POSTTIME         PIC 9(6).
           03  QTC-TILLNO           PIC 9(2).
           03  QTC-ADR1             PIC X(30).
           03  QTC-ADR2             PIC X(30).
           03  QTC-CITY             PIC X(30).
           03  QTC-STATE            PIC X(2).
           03  QTC-ZIP              PIC X(10).
           03  QTC-PAYEE-NO         PIC 9(6).
           03  QTC-ACH-FG           PIC X.
           03  QTC-EXTRACT-FG       PIC X.
           03  QTC-DEBIT-REFCD      PIC X(5).

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
           03  BEG-KEY-DATE            PIC 9(08).
           03  END-KEY-DATE            PIC 9(08).
           03  BEG-CKNO                PIC 9(06).
           03  END-CKNO                PIC 9(06).
           03  BEG-CLASS               PIC 9(03).
           03  END-CLASS               PIC 9(03).
           03  BEG-SEQ                 PIC 9(05).
           03  END-SEQ                 PIC 9(05).

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
           03  FILLER PIC X(30) VALUE "BRNO    KEY DATE     CKNO   CL".
           03  FILLER PIC X(30) VALUE "ASS   SEQ   TYPE      AMT     ".
           03  FILLER PIC X(30) VALUE "   PAYEE".

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-TC-BRNO                    PIC 9(04).
           03  FILLER                         PIC X(03).
           03  D01-TC-KEY-DATE                PIC 9999/99/99.
           03  FILLER                         PIC X(03).
           03  D01-TC-CKNO                    PIC 9(06).
           03  FILLER                         PIC X(03).
           03  D01-TC-CLASS                   PIC 9(03).
           03  FILLER                         PIC X(03).
           03  D01-TC-SEQ                     PIC 9(05).
           03  FILLER                         PIC X(03).
           03  D01-TC-TYPE                    PIC XX.
           03  FILLER                         PIC X(03).
           03  D01-TC-AMT                     PIC Z(6)9.99-.
           03  FILLER                         PIC X(03).
           03  D01-TC-PAYEE                   PIC X(15).

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
       COPY "LIBIO/IOTRCI_DEF.CPY".
       COPY "LIBIO/IOTRCI_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOTRCI_SCN.CPY".

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
           MOVE "IOTRCI"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOTRCI-QUEUE-POS  TO Q-POS.
           MOVE IOTRCI-COPIES-POS TO C-POS.
           MOVE IOTRCI-PRU-POS    TO P-POS.
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
              GO TO ENTER-END-SEQ.

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
       ENTER-BEG-DATE.

           MOVE SPACES  TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ERNM060BEG-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PROCESS-KEY.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-DATE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-DATE.

           MOVE VDU-DATE-YYYYMMDD TO BEG-KEY-DATE.

      *-----------------------------------------------------------------
       ENTER-END-DATE.

           MOVE "ERNM060END-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-DATE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-DATE.

           MOVE VDU-DATE-YYYYMMDD TO END-KEY-DATE.

           IF ( BEG-KEY-DATE > END-KEY-DATE )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DATE.

           GO TO ENTER-BEG-CKNO.

      *-----------------------------------------------------------------
       ENTER-ALL-DATE.

           MOVE "S  8080BEG-DATE." TO VDU.
           MOVE JULIAN-BEG         TO BEG-KEY-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "S  8080END-DATE." TO VDU.
           MOVE JULIAN-END         TO END-KEY-DATE VDU-DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           PERFORM SCREENIO.
      *=================================================================
       ENTER-BEG-CKNO.

           MOVE "ENNN060BEG-CKNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-DATE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-CKNO.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-CKNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-CKNO.

      *-----------------------------------------------------------------
       ENTER-END-CKNO.

           MOVE "ENNN060END-CKNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-CKNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-CKNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-CKNO.

           IF ( BEG-CKNO > END-CKNO )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-CKNO.

           GO TO ENTER-BEG-CLASS.

      *-----------------------------------------------------------------
       ENTER-ALL-CKNO.

           MOVE "SNNN060BEG-CKNO." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-CKNO.
           PERFORM SCREENIO.

           MOVE "SNNN060END-CKNO." TO VDU.
           MOVE ALL "9"            TO VDUNUM0  END-CKNO.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-CLASS.

           MOVE "ENNN030BEG-CLASS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-CKNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-CLASS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-CLASS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-CLASS.

      *-----------------------------------------------------------------
       ENTER-END-CLASS.

           MOVE "ENNN030END-CLASS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-CLASS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-CLASS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-CLASS.

           IF ( BEG-CLASS > END-CLASS )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-CLASS.

           GO TO ENTER-BEG-SEQ.

      *-----------------------------------------------------------------
       ENTER-ALL-CLASS.

           MOVE "SNNN030BEG-CLASS." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-CLASS.
           PERFORM SCREENIO.

           MOVE "SNNN030END-CLASS." TO VDU.
           MOVE 999                 TO VDUNUM0  END-CLASS.
           PERFORM SCREENIO.
      *-----------------------------------------------------------------
       ENTER-BEG-SEQ.

           MOVE "ENNN050BEG-SEQ." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-CLASS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-SEQ.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-SEQ.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-SEQ.

      *-----------------------------------------------------------------
       ENTER-END-SEQ.

           MOVE "ENNN050END-SEQ." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-SEQ.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-SEQ.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-SEQ.

           IF ( BEG-SEQ > END-SEQ )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-SEQ.

           GO TO ENTER-ACCEPT-YN.

      *-----------------------------------------------------------------
       ENTER-ALL-SEQ.

           MOVE "SNNN050BEG-SEQ." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-SEQ.
           PERFORM SCREENIO.

           MOVE "SNNN050END-SEQ." TO VDU.
           MOVE 999999            TO VDUNUM0  END-SEQ.
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

           MOVE "S  A000BEG-DATE."    TO VDU.
           MOVE BEG-KEY-DATE              TO VDU-DATE-EDIT.
           PERFORM SCREENIO.

           MOVE "S  A000END-DATE."    TO VDU.
           MOVE END-KEY-DATE              TO VDU-DATE-EDIT.
           PERFORM SCREENIO.

           MOVE "SNNN060BEG-CKNO."       TO VDU.
           MOVE BEG-CKNO                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060END-CKNO."       TO VDU.
           MOVE END-CKNO                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN030BEG-CLASS."       TO VDU.
           MOVE BEG-CLASS                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN030END-CLASS."       TO VDU.
           MOVE END-CLASS                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN050BEG-SEQ."      TO VDU.
           MOVE BEG-SEQ                TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN050END-SEQ."      TO VDU.
           MOVE END-SEQ                TO VDUNUM0.
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

           MOVE "B"                        TO TRC-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - -{ SET WHERE BEG & END FIELDS }
           MOVE BR-NO     TO QTRC1-WBEG-BRNO.
           MOVE BR-NO     TO QTRC1-WEND-BRNO.

           MOVE BEG-KEY-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QTRC1-WBEG-KEY-DATE.

           MOVE END-KEY-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QTRC1-WEND-KEY-DATE.

           MOVE BEG-CKNO  TO QTRC1-WBEG-CKNO.
           MOVE END-CKNO  TO QTRC1-WEND-CKNO.

           MOVE BEG-CLASS  TO QTRC1-WBEG-CLASS.
           MOVE END-CLASS  TO QTRC1-WEND-CLASS.

           MOVE BEG-SEQ TO QTRC1-WBEG-SEQ.
           MOVE END-SEQ TO QTRC1-WEND-SEQ.

      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           PERFORM OPEN-TRC1-FILE
           PERFORM START-TRC1-FILE.

      *=================================================================
       PROCESS-BRANCH-NEXT.

           PERFORM READ-TRC1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO PROCESS-BRANCH-MODULE-END.

           ADD 01 TO RECORDS-READ.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

           IF ( PROCESS-KEY = 1 )
              IF ( TC-BRNO < BEG-BRANCH )
                  GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( TC-BRNO > END-BRANCH )
                 GO TO PROCESS-BRANCH-MODULE-END
              ELSE
                 IF TC-KEY-DATE < BEG-KEY-DATE OR > END-KEY-DATE
                    GO TO PROCESS-BRANCH-NEXT.

           IF ( PROCESS-KEY = 2 )
              IF ( TC-KEY-DATE < BEG-KEY-DATE )
                  GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( TC-KEY-DATE > END-KEY-DATE )
                 GO TO PROCESS-BRANCH-MODULE-END
              ELSE
                 IF TC-BRNO < BEG-BRANCH OR > END-BRANCH
                    GO TO PROCESS-BRANCH-NEXT.

           IF TC-CKNO < BEG-CKNO OR > END-CKNO
              GO TO PROCESS-BRANCH-NEXT.
           IF TC-CLASS < BEG-CLASS OR > END-CLASS
              GO TO PROCESS-BRANCH-NEXT.
           IF TC-SEQ < BEG-SEQ OR > END-SEQ
              GO TO PROCESS-BRANCH-NEXT.

      *-----------------------------------------------------------------
       PROCESS-BRANCH-PRINT.

           MOVE TC-BRNO                 TO D01-TC-BRNO.
           MOVE TC-KEY-DATE             TO D01-TC-KEY-DATE.
           MOVE TC-CKNO                 TO D01-TC-CKNO.
           MOVE TC-CLASS                TO D01-TC-CLASS.
           MOVE TC-SEQ                  TO D01-TC-SEQ.
           MOVE TC-TYPE                 TO D01-TC-TYPE.
           MOVE TC-AMT                  TO D01-TC-AMT.
           MOVE TC-PAYEE                TO D01-TC-PAYEE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.
      * KEY SINGLE READ
           IF PROCESS-KEY = 9
              GO TO PROCESS-BRANCH-MODULE-END.

           GO TO PROCESS-BRANCH-NEXT.

      *=================================================================
       PROCESS-BRANCH-MODULE-END.

 
           PERFORM CLOSE-TRC1-FILE.

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
              MOVE TRC1-KEY TO PACE-DETAIL-KEY
           ELSE
              STRING TC-KEY-DATE,TC-BRNO,TC-CKNO,TC-CLASS,TC-SEQ
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
           MOVE "BEGINNING DATE               : "    TO R01-PROMPT.
           MOVE BEG-KEY-DATE                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    DATE               : "    TO R01-PROMPT.
           MOVE END-KEY-DATE                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING CKNO               : "    TO R01-PROMPT.
           MOVE BEG-CKNO                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    CKNO               : "    TO R01-PROMPT.
           MOVE END-CKNO                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING CLASS               : "    TO R01-PROMPT.
           MOVE BEG-CLASS                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    CLASS               : "    TO R01-PROMPT.
           MOVE END-CLASS                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING SEQ              : "    TO R01-PROMPT.
           MOVE BEG-SEQ                            TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    SEQ              : "    TO R01-PROMPT.
           MOVE END-SEQ                            TO R01-NUMERIC.
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
           MOVE "SQL TABLE IO UTILITY - TRCFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOTRCI-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOTRCI-SYSDATE.
           DISPLAY IOTRCI-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOTRCI-FORM-FIELDS TO WR-BUF.
           MOVE IOTRCI-FORM-LIST TO WR-LIST.
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

           MOVE IOTRCI-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOTRCI-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOTRCI_EVA.CPY".
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
      *    LOAD TRC-REC FROM QTRC-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-TRC-FIELDS.
           MOVE QTC-BRNO TO TC-BRNO.

           IF ( QTC-KEY-DATE = "1900-01-01" )
              MOVE ZEROES TO TC-KEY-DATE
           ELSE
              MOVE QTC-KEY-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TC-KEY-DATE.

           MOVE QTC-CKNO TO TC-CKNO.
           MOVE QTC-CLASS TO TC-CLASS.
           MOVE QTC-SEQ TO TC-SEQ.

           IF ( QTC-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO TC-LTOUCH-DATE
           ELSE
              MOVE QTC-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TC-LTOUCH-DATE.

           MOVE QTC-TYPE TO TC-TYPE.

           IF ( QTC-DATE = "1900-01-01" )
              MOVE ZEROES TO TC-DATE
           ELSE
              MOVE QTC-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TC-DATE.

           MOVE QTC-AMT TO TC-AMT.
           MOVE QTC-PAYEE TO TC-PAYEE.
           MOVE QTC-REF TO TC-REF.
           MOVE QTC-REASON TO TC-REASON.
           MOVE QTC-GLNO-1 TO TC-GLNO(1).
           MOVE QTC-GLAMT-1 TO TC-GLAMT(1).
           MOVE QTC-GLNO-2 TO TC-GLNO(2).
           MOVE QTC-GLAMT-2 TO TC-GLAMT(2).
           MOVE QTC-GLNO-3 TO TC-GLNO(3).
           MOVE QTC-GLAMT-3 TO TC-GLAMT(3).
           MOVE QTC-GLNO-4 TO TC-GLNO(4).
           MOVE QTC-GLAMT-4 TO TC-GLAMT(4).
           MOVE QTC-GLNO-5 TO TC-GLNO(5).
           MOVE QTC-GLAMT-5 TO TC-GLAMT(5).
           MOVE QTC-VOID TO TC-VOID.
           MOVE QTC-ACCTNO TO TC-ACCTNO.
           MOVE QTC-PAID TO TC-PAID.
           MOVE QTC-PGM TO TC-PGM.
           MOVE QTC-DLRVNO TO TC-DLRVNO.
           MOVE QTC-SPRCLASS TO TC-SPRCLASS.
           MOVE QTC-DLNO TO TC-DLNO.
           MOVE QTC-USERID TO TC-USERID.

           IF ( QTC-POSTDATE = "1900-01-01" )
              MOVE ZEROES TO TC-POSTDATE
           ELSE
              MOVE QTC-POSTDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TC-POSTDATE.

           MOVE QTC-POSTTIME TO TC-POSTTIME.
           MOVE QTC-TILLNO TO TC-TILLNO.
           MOVE QTC-ADR1 TO TC-ADR1.
           MOVE QTC-ADR2 TO TC-ADR2.
           MOVE QTC-CITY TO TC-CITY.
           MOVE QTC-STATE TO TC-STATE.
           MOVE QTC-ZIP TO TC-ZIP.
           MOVE QTC-PAYEE-NO TO TC-PAYEE-NO.
           MOVE QTC-ACH-FG TO TC-ACH-FG.
           MOVE QTC-EXTRACT-FG TO TC-EXTRACT-FG.
           MOVE QTC-DEBIT-REFCD TO TC-DEBIT-REFCD.
 
      *-----------------------------------------------------------------
      *    LOAD QTRC-REC FROM TRC-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-TRC-FIELDS.
           MOVE TC-BRNO TO QTC-BRNO.

           IF ( TC-KEY-DATE = ZEROES )
              MOVE "1900-01-01" TO QTC-KEY-DATE
           ELSE
              MOVE TC-KEY-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTC-KEY-DATE.

           MOVE TC-CKNO TO QTC-CKNO.
           MOVE TC-CLASS TO QTC-CLASS.
           MOVE TC-SEQ TO QTC-SEQ.

           IF ( TC-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QTC-LTOUCH-DATE
           ELSE
              MOVE TC-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTC-LTOUCH-DATE.

           MOVE TC-TYPE TO QTC-TYPE.

           IF ( TC-DATE = ZEROES )
              MOVE "1900-01-01" TO QTC-DATE
           ELSE
              MOVE TC-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTC-DATE.

           MOVE TC-AMT TO QTC-AMT.
           MOVE TC-PAYEE TO QTC-PAYEE.
           MOVE TC-REF TO QTC-REF.
           MOVE TC-REASON TO QTC-REASON.
           MOVE TC-GLNO(1) TO QTC-GLNO-1.
           MOVE TC-GLAMT(1) TO QTC-GLAMT-1.
           MOVE TC-GLNO(2) TO QTC-GLNO-2.
           MOVE TC-GLAMT(2) TO QTC-GLAMT-2.
           MOVE TC-GLNO(3) TO QTC-GLNO-3.
           MOVE TC-GLAMT(3) TO QTC-GLAMT-3.
           MOVE TC-GLNO(4) TO QTC-GLNO-4.
           MOVE TC-GLAMT(4) TO QTC-GLAMT-4.
           MOVE TC-GLNO(5) TO QTC-GLNO-5.
           MOVE TC-GLAMT(5) TO QTC-GLAMT-5.
           MOVE TC-VOID TO QTC-VOID.
           MOVE TC-ACCTNO TO QTC-ACCTNO.
           MOVE TC-PAID TO QTC-PAID.
           MOVE TC-PGM TO QTC-PGM.
           MOVE TC-DLRVNO TO QTC-DLRVNO.
           MOVE TC-SPRCLASS TO QTC-SPRCLASS.
           MOVE TC-DLNO TO QTC-DLNO.
           MOVE TC-USERID TO QTC-USERID.

           IF ( TC-POSTDATE = ZEROES )
              MOVE "1900-01-01" TO QTC-POSTDATE
           ELSE
              MOVE TC-POSTDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTC-POSTDATE.

           MOVE TC-POSTTIME TO QTC-POSTTIME.
           MOVE TC-TILLNO TO QTC-TILLNO.
           MOVE TC-ADR1 TO QTC-ADR1.
           MOVE TC-ADR2 TO QTC-ADR2.
           MOVE TC-CITY TO QTC-CITY.
           MOVE TC-STATE TO QTC-STATE.
           MOVE TC-ZIP TO QTC-ZIP.
           MOVE TC-PAYEE-NO TO QTC-PAYEE-NO.
           MOVE TC-ACH-FG TO QTC-ACH-FG.
           MOVE TC-EXTRACT-FG TO QTC-EXTRACT-FG.
           MOVE TC-DEBIT-REFCD TO QTC-DEBIT-REFCD.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-TRC1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRC-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRC-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO TRC-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRC-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO TRC-ALL-DATA
              MOVE TRC-ALL-PATH             TO TRC-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRC-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO TRC-ALL-BASE
              MOVE FILPATH-MACHINE         TO TRC-ALL-MACHINE
              MOVE FILPATH-DATA            TO TRC-ALL-DATA
              MOVE TRC-ALL-PATH             TO TRC-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-TRCFILE NOT = "S" )
              MOVE TRC-PATH-BEG             TO TRC-SELECT-PATH-BEG
              MOVE TRC-PATH-FILE            TO TRC-SELECT-PATH-FILE
           ELSE
              MOVE TRC-PATH-SQL             TO TRC-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-TRC1-FILE.
           PERFORM LOAD-TRC1-FILE.
           PERFORM OPEN-IT.
           MOVE TRC-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRC1-FILE.
           PERFORM START-IT.
           MOVE TRC-PATH-OWNBR  TO TC-BRNO.
           MOVE TRC-SELECT-PATH TO E-FILE.
           MOVE TRC1-KEY        TO E-KEYX.

           IF ( TRC1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRC1-FILE.

           IF ( QTRC1-WEND-SEQ = ZEROES    ) OR
              ( QTRC1-WEND-SEQ NOT NUMERIC )
              MOVE TRC-PATH-OWNBR        TO QTRC1-WBEG-BRNO
              MOVE TRC-PATH-OWNBR        TO QTRC1-WEND-BRNO
              MOVE TC-KEY-DATE           TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRC1-WBEG-KEY-DATE
              MOVE JULIAN-END           TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QTRC1-WEND-KEY-DATE
              MOVE TC-CKNO              TO QTRC1-WBEG-CKNO
              MOVE TC-CLASS             TO QTRC1-WBEG-CLASS
              MOVE TC-SEQ               TO QTRC1-WBEG-SEQ
              MOVE ALL "9"              TO QTRC1-WEND-CKNO
                                           QTRC1-WEND-CLASS
                                           QTRC1-WEND-SEQ.

           EXEC SQL
            DECLARE TRC1_CURSOR CURSOR FOR
            SELECT TRCFILE.TC_BRNO,
                   ISNULL(CAST(TRCFILE.TC_KEY_DATE
                                                  AS VARCHAR(10)),'0'),
                   TRCFILE.TC_CKNO,
                   TRCFILE.TC_CLASS,
                   TRCFILE.TC_SEQ,
                   ISNULL(CAST(TRCFILE.TC_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(TRCFILE.TC_TYPE,''),
                   ISNULL(CAST(TRCFILE.TC_DATE AS VARCHAR(10)),'0'),
                   TRCFILE.TC_AMT,
                   ISNULL(TRCFILE.TC_PAYEE,''),
                   TRCFILE.TC_REF,
                   ISNULL(TRCFILE.TC_REASON,''),
                   TRCFILE.TC_GLNO_1,
                   TRCFILE.TC_GLAMT_1,
                   TRCFILE.TC_GLNO_2,
                   TRCFILE.TC_GLAMT_2,
                   TRCFILE.TC_GLNO_3,
                   TRCFILE.TC_GLAMT_3,
                   TRCFILE.TC_GLNO_4,
                   TRCFILE.TC_GLAMT_4,
                   TRCFILE.TC_GLNO_5,
                   TRCFILE.TC_GLAMT_5,
                   ISNULL(TRCFILE.TC_VOID,''),
                   TRCFILE.TC_ACCTNO,
                   ISNULL(TRCFILE.TC_PAID,''),
                   ISNULL(TRCFILE.TC_PGM,''),
                   TRCFILE.TC_DLRVNO,
                   TRCFILE.TC_SPRCLASS,
                   TRCFILE.TC_DLNO,
                   ISNULL(TRCFILE.TC_USERID,''),
                   ISNULL(CAST(TRCFILE.TC_POSTDATE
                                                  AS VARCHAR(10)),'0'),
                   TRCFILE.TC_POSTTIME,
                   TRCFILE.TC_TILLNO,
                   ISNULL(TRCFILE.TC_ADR1,''),
                   ISNULL(TRCFILE.TC_ADR2,''),
                   ISNULL(TRCFILE.TC_CITY,''),
                   ISNULL(TRCFILE.TC_STATE,''),
                   ISNULL(TRCFILE.TC_ZIP,''),
                   TRCFILE.TC_PAYEE_NO,
                   ISNULL(TRCFILE.TC_ACH_FG,''),
                   ISNULL(TRCFILE.TC_EXTRACT_FG,''),
                   ISNULL(TRCFILE.TC_DEBIT_REFCD,'')
            FROM DBO.TRCFILE
            WHERE TRCFILE.TC_BRNO >= :QTRC1-WBEG-BRNO
              AND TRCFILE.TC_BRNO <= :QTRC1-WEND-BRNO
              AND CAST(TRCFILE.TC_KEY_DATE AS VARCHAR(10))
                          >= :QTRC1-WBEG-KEY-DATE
              AND CAST(TRCFILE.TC_KEY_DATE AS VARCHAR(10))
                          <= :QTRC1-WEND-KEY-DATE
              AND TRCFILE.TC_CKNO >= :QTRC1-WBEG-CKNO
              AND TRCFILE.TC_CKNO <= :QTRC1-WEND-CKNO
              AND TRCFILE.TC_CLASS >= :QTRC1-WBEG-CLASS
              AND TRCFILE.TC_CLASS <= :QTRC1-WEND-CLASS
              AND TRCFILE.TC_SEQ >= :QTRC1-WBEG-SEQ
              AND TRCFILE.TC_SEQ <= :QTRC1-WEND-SEQ
            ORDER BY TRCFILE.TC_BRNO,
                     TRCFILE.TC_KEY_DATE,
                     TRCFILE.TC_CKNO,
                     TRCFILE.TC_CLASS,
                     TRCFILE.TC_SEQ
           END-EXEC.

           EXEC SQL
               OPEN TRC1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRC1-WBEG-BRNO
                          QTRC1-WEND-BRNO
                          QTRC1-WBEG-CKNO
                          QTRC1-WEND-CKNO
                          QTRC1-WBEG-CLASS
                          QTRC1-WEND-CLASS
                          QTRC1-WBEG-SEQ
                          QTRC1-WEND-SEQ.
           MOVE SPACES TO QTRC1-WBEG-KEY-DATE
                          QTRC1-WEND-KEY-DATE.

           MOVE STAT---GOOD TO TRC1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-TRC1-FILE.
           PERFORM READ-IT.
           MOVE TRC-PATH-OWNBR  TO TC-BRNO. 
           MOVE TRC-SELECT-PATH TO E-FILE.
           MOVE TRC1-KEY        TO E-KEYX.

           MOVE TC-BRNO       TO QTC-BRNO.

           MOVE TC-KEY-DATE   TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QTC-KEY-DATE.

           MOVE TC-CKNO  TO QTC-CKNO.
           MOVE TC-CLASS TO QTC-CLASS.
           MOVE TC-SEQ   TO QTC-SEQ.

           EXEC SQL
            SELECT TRCFILE.TC_BRNO,
                   ISNULL(CAST(TRCFILE.TC_KEY_DATE
                                                  AS VARCHAR(10)),'0'),
                   TRCFILE.TC_CKNO,
                   TRCFILE.TC_CLASS,
                   TRCFILE.TC_SEQ,
                   ISNULL(CAST(TRCFILE.TC_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(TRCFILE.TC_TYPE,''),
                   ISNULL(CAST(TRCFILE.TC_DATE AS VARCHAR(10)),'0'),
                   TRCFILE.TC_AMT,
                   ISNULL(TRCFILE.TC_PAYEE,''),
                   TRCFILE.TC_REF,
                   ISNULL(TRCFILE.TC_REASON,''),
                   TRCFILE.TC_GLNO_1,
                   TRCFILE.TC_GLAMT_1,
                   TRCFILE.TC_GLNO_2,
                   TRCFILE.TC_GLAMT_2,
                   TRCFILE.TC_GLNO_3,
                   TRCFILE.TC_GLAMT_3,
                   TRCFILE.TC_GLNO_4,
                   TRCFILE.TC_GLAMT_4,
                   TRCFILE.TC_GLNO_5,
                   TRCFILE.TC_GLAMT_5,
                   ISNULL(TRCFILE.TC_VOID,''),
                   TRCFILE.TC_ACCTNO,
                   ISNULL(TRCFILE.TC_PAID,''),
                   ISNULL(TRCFILE.TC_PGM,''),
                   TRCFILE.TC_DLRVNO,
                   TRCFILE.TC_SPRCLASS,
                   TRCFILE.TC_DLNO,
                   ISNULL(TRCFILE.TC_USERID,''),
                   ISNULL(CAST(TRCFILE.TC_POSTDATE
                                                  AS VARCHAR(10)),'0'),
                   TRCFILE.TC_POSTTIME,
                   TRCFILE.TC_TILLNO,
                   ISNULL(TRCFILE.TC_ADR1,''),
                   ISNULL(TRCFILE.TC_ADR2,''),
                   ISNULL(TRCFILE.TC_CITY,''),
                   ISNULL(TRCFILE.TC_STATE,''),
                   ISNULL(TRCFILE.TC_ZIP,''),
                   TRCFILE.TC_PAYEE_NO,
                   ISNULL(TRCFILE.TC_ACH_FG,''),
                   ISNULL(TRCFILE.TC_EXTRACT_FG,''),
                   ISNULL(TRCFILE.TC_DEBIT_REFCD,'')
            INTO :QTRC-REC
            FROM DBO.TRCFILE
            WHERE TRCFILE.TC_BRNO = :QTC-BRNO
              AND TRCFILE.TC_KEY_DATE = :QTC-KEY-DATE
              AND TRCFILE.TC_CKNO = :QTC-CKNO
              AND TRCFILE.TC_CLASS = :QTC-CLASS
              AND TRCFILE.TC_SEQ = :QTC-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRC1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-TRC-FIELDS.

      *-----------------------------------------------------------------
       READ-TRC1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE TRC-PATH-OWNBR  TO TC-BRNO.
           MOVE TRC-SELECT-PATH TO E-FILE.
           MOVE TRC1-KEY        TO E-KEYX.
           INITIALIZE QTRC-REC.

           IF ( TRC1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TRC1_CURSOR INTO :QTRC-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRC1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRC-FIELDS.

      *-----------------------------------------------------------------
       WRITE-TRC1-FILE.

           MOVE TRC-PATH-OWNBR  TO TC-BRNO.
           ACCEPT TC-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE TRC-SELECT-PATH TO E-FILE.
           MOVE TRC1-KEY        TO E-KEYX.
           PERFORM SET-TRC-FIELDS.

           EXEC SQL
            INSERT INTO DBO.TRCFILE
             (TRCFILE.TC_BRNO,
              TRCFILE.TC_KEY_DATE,
              TRCFILE.TC_CKNO,
              TRCFILE.TC_CLASS,
              TRCFILE.TC_SEQ,
              TRCFILE.TC_LTOUCH_DATE,
              TRCFILE.TC_TYPE,
              TRCFILE.TC_DATE,
              TRCFILE.TC_AMT,
              TRCFILE.TC_PAYEE,
              TRCFILE.TC_REF,
              TRCFILE.TC_REASON,
              TRCFILE.TC_GLNO_1,
              TRCFILE.TC_GLAMT_1,
              TRCFILE.TC_GLNO_2,
              TRCFILE.TC_GLAMT_2,
              TRCFILE.TC_GLNO_3,
              TRCFILE.TC_GLAMT_3,
              TRCFILE.TC_GLNO_4,
              TRCFILE.TC_GLAMT_4,
              TRCFILE.TC_GLNO_5,
              TRCFILE.TC_GLAMT_5,
              TRCFILE.TC_VOID,
              TRCFILE.TC_ACCTNO,
              TRCFILE.TC_PAID,
              TRCFILE.TC_PGM,
              TRCFILE.TC_DLRVNO,
              TRCFILE.TC_SPRCLASS,
              TRCFILE.TC_DLNO,
              TRCFILE.TC_USERID,
              TRCFILE.TC_POSTDATE,
              TRCFILE.TC_POSTTIME,
              TRCFILE.TC_TILLNO,
              TRCFILE.TC_ADR1,
              TRCFILE.TC_ADR2,
              TRCFILE.TC_CITY,
              TRCFILE.TC_STATE,
              TRCFILE.TC_ZIP,
              TRCFILE.TC_PAYEE_NO,
              TRCFILE.TC_ACH_FG,
              TRCFILE.TC_EXTRACT_FG,
              TRCFILE.TC_DEBIT_REFCD)
            VALUES
             (:QTC-BRNO,
              NULLIF(:QTC-KEY-DATE,'1900-01-01'),
              :QTC-CKNO,
              :QTC-CLASS,
              :QTC-SEQ,
              NULLIF(:QTC-LTOUCH-DATE,'1900-01-01'),
              :QTC-TYPE,
              NULLIF(:QTC-DATE,'1900-01-01'),
              :QTC-AMT,
              :QTC-PAYEE,
              :QTC-REF,
              :QTC-REASON,
              :QTC-GLNO-1,
              :QTC-GLAMT-1,
              :QTC-GLNO-2,
              :QTC-GLAMT-2,
              :QTC-GLNO-3,
              :QTC-GLAMT-3,
              :QTC-GLNO-4,
              :QTC-GLAMT-4,
              :QTC-GLNO-5,
              :QTC-GLAMT-5,
              :QTC-VOID,
              :QTC-ACCTNO,
              :QTC-PAID,
              :QTC-PGM,
              :QTC-DLRVNO,
              :QTC-SPRCLASS,
              :QTC-DLNO,
              :QTC-USERID,
              NULLIF(:QTC-POSTDATE,'1900-01-01'),
              :QTC-POSTTIME,
              :QTC-TILLNO,
              :QTC-ADR1,
              :QTC-ADR2,
              :QTC-CITY,
              :QTC-STATE,
              :QTC-ZIP,
              :QTC-PAYEE-NO,
              :QTC-ACH-FG,
              :QTC-EXTRACT-FG,
              :QTC-DEBIT-REFCD)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-TRC1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-TRC1-FILE.

           MOVE TRC-PATH-OWNBR  TO TC-BRNO.
           ACCEPT TC-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE TRC-SELECT-PATH TO E-FILE.
           MOVE TRC1-KEY        TO E-KEYX.
           PERFORM SET-TRC-FIELDS.

           EXEC SQL
            UPDATE DBO.TRCFILE
             SET
              TRCFILE.TC_BRNO = :QTC-BRNO,
              TRCFILE.TC_KEY_DATE =
                 NULLIF(:QTC-KEY-DATE,'1900-01-01'),
              TRCFILE.TC_CKNO = :QTC-CKNO,
              TRCFILE.TC_CLASS = :QTC-CLASS,
              TRCFILE.TC_SEQ = :QTC-SEQ,
              TRCFILE.TC_LTOUCH_DATE =
                 NULLIF(:QTC-LTOUCH-DATE,'1900-01-01'),
              TRCFILE.TC_TYPE = :QTC-TYPE,
              TRCFILE.TC_DATE =
                 NULLIF(:QTC-DATE,'1900-01-01'),
              TRCFILE.TC_AMT = :QTC-AMT,
              TRCFILE.TC_PAYEE = :QTC-PAYEE,
              TRCFILE.TC_REF = :QTC-REF,
              TRCFILE.TC_REASON = :QTC-REASON,
              TRCFILE.TC_GLNO_1 = :QTC-GLNO-1,
              TRCFILE.TC_GLAMT_1 = :QTC-GLAMT-1,
              TRCFILE.TC_GLNO_2 = :QTC-GLNO-2,
              TRCFILE.TC_GLAMT_2 = :QTC-GLAMT-2,
              TRCFILE.TC_GLNO_3 = :QTC-GLNO-3,
              TRCFILE.TC_GLAMT_3 = :QTC-GLAMT-3,
              TRCFILE.TC_GLNO_4 = :QTC-GLNO-4,
              TRCFILE.TC_GLAMT_4 = :QTC-GLAMT-4,
              TRCFILE.TC_GLNO_5 = :QTC-GLNO-5,
              TRCFILE.TC_GLAMT_5 = :QTC-GLAMT-5,
              TRCFILE.TC_VOID = :QTC-VOID,
              TRCFILE.TC_ACCTNO = :QTC-ACCTNO,
              TRCFILE.TC_PAID = :QTC-PAID,
              TRCFILE.TC_PGM = :QTC-PGM,
              TRCFILE.TC_DLRVNO = :QTC-DLRVNO,
              TRCFILE.TC_SPRCLASS = :QTC-SPRCLASS,
              TRCFILE.TC_DLNO = :QTC-DLNO,
              TRCFILE.TC_USERID = :QTC-USERID,
              TRCFILE.TC_POSTDATE =
                 NULLIF(:QTC-POSTDATE,'1900-01-01'),
              TRCFILE.TC_POSTTIME = :QTC-POSTTIME,
              TRCFILE.TC_TILLNO = :QTC-TILLNO,
              TRCFILE.TC_ADR1 = :QTC-ADR1,
              TRCFILE.TC_ADR2 = :QTC-ADR2,
              TRCFILE.TC_CITY = :QTC-CITY,
              TRCFILE.TC_STATE = :QTC-STATE,
              TRCFILE.TC_ZIP = :QTC-ZIP,
              TRCFILE.TC_PAYEE_NO = :QTC-PAYEE-NO,
              TRCFILE.TC_ACH_FG = :QTC-ACH-FG,
              TRCFILE.TC_EXTRACT_FG = :QTC-EXTRACT-FG,
              TRCFILE.TC_DEBIT_REFCD = :QTC-DEBIT-REFCD
            WHERE TRCFILE.TC_BRNO = :QTC-BRNO
              AND TRCFILE.TC_KEY_DATE = :QTC-KEY-DATE
              AND TRCFILE.TC_CKNO = :QTC-CKNO
              AND TRCFILE.TC_CLASS = :QTC-CLASS
              AND TRCFILE.TC_SEQ = :QTC-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-TRC1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-TRC1-FILE.

           MOVE TRC-PATH-OWNBR  TO TC-BRNO.
           PERFORM DELETE-IT.
           MOVE TRC-SELECT-PATH TO E-FILE.
           MOVE TRC1-KEY        TO E-KEYX.
           PERFORM SET-TRC-FIELDS.

           EXEC SQL
            DELETE FROM DBO.TRCFILE
            WHERE TRCFILE.TC_BRNO = :QTC-BRNO
              AND TRCFILE.TC_KEY_DATE = :QTC-KEY-DATE
              AND TRCFILE.TC_CKNO = :QTC-CKNO
              AND TRCFILE.TC_CLASS = :QTC-CLASS
              AND TRCFILE.TC_SEQ = :QTC-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-TRC1-FILE.

      *-----------------------------------------------------------------
       CLOSE-TRC1-FILE.

           IF ( TRC1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRC1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRC1-CURSOR-STAT.

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
              DESTROY IOTRCI-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
