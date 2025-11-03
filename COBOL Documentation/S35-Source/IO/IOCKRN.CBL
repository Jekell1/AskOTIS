       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOCKRN.
       AUTHOR.        BAH
       DATE-WRITTEN.  04-01-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOCKRN.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : CKFILE 
      * TABLE DESC    : CHECK MASTER FILE
      * OUTPUT FILE   : XPC/IOCKRN.PRE
      * USED FOR      : LIBLP/LPWSCK.CPY  --- QCK-REC
      *                 LIBLP/LPCK1R.CPY  --- READ
      *                 LIBLP/LPCK1RN.CPY  --- READ NEXT
      *
      *=================================================================
      * REV:
      * BAH 2021-0401 ***NEW***
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
           "@(#)IO/IOCKRN.CBL S34 05/19/21-12:59:30 >".

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
       COPY "LIBLP/LP01CK.CPY".
       COPY "LIBLP/LPWSCK.CPY".

      *=================================================================
      *
      *    Q - F I L E
      *
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QCK1-WHERE-BEG-KEY.
           03  QCK1-WBEG-BRNO             PIC 9(04).
           03  QCK1-WBEG-CKNO             PIC 9(06).
 
       01  QCK1-WHERE-END-KEY.
           03  QCK1-WEND-BRNO             PIC 9(04).
           03  QCK1-WEND-CKNO             PIC 9(06).
 
       01  QCK-REC.
           03  QCK-BRNO                PIC 9(4).
           03  QCK-NO                  PIC 9(6).
           03  QCK-LTOUCH-DATE         PIC X(10).
           03  QCK-TYPE                PIC X(2).
           03  QCK-DATE                PIC X(10).
           03  QCK-AMT                 PIC S9(7)V99.
           03  QCK-PAYEE               PIC X(60).
           03  QCK-ADR1                PIC X(30).
           03  QCK-ADR2                PIC X(30).
           03  QCK-CITY                PIC X(30).
           03  QCK-STATE               PIC X(2).
           03  QCK-ZIP                 PIC X(10).
           03  QCK-REF                 PIC 9(6).
           03  QCK-REASON              PIC X(30).
           03  QCK-GLNO-1              PIC 9(11).
           03  QCK-GLAMT-1             PIC S9(7)V99.
           03  QCK-GLNO-2              PIC 9(11).
           03  QCK-GLAMT-2             PIC S9(7)V99.
           03  QCK-GLNO-3              PIC 9(11).
           03  QCK-GLAMT-3             PIC S9(7)V99.
           03  QCK-GLNO-4              PIC 9(11).
           03  QCK-GLAMT-4             PIC S9(7)V99.
           03  QCK-GLNO-5              PIC 9(11).
           03  QCK-GLAMT-5             PIC S9(7)V99.
           03  QCK-VOID                PIC X.
           03  QCK-PAID                PIC X.
           03  QCK-CLASS               PIC 9(3).
           03  QCK-DLNO                PIC 9(4)V99.
           03  QCK-PGM                 PIC XX.
           03  QCK-NC-SEQNO            PIC 99.
           03  QCK-VOID-DATE           PIC X(10).
           03  QCK-ACCTNO              PIC 9(8).
           03  QCK-DLRVNO              PIC 9.
           03  QCK-CLEARED-FG          PIC X.
           03  QCK-SSNO                PIC 9(9).
           03  QCK-BATCHID             PIC 9(10).
           03  QCK-UNPAID              PIC X.
           03  QCK-CLEARED-DATE        PIC X(10).
           03  QCK-TILLNO              PIC 99.
           03  QCK-EVENT-CD            PIC X(2).
           03  QCK-PAYEE-NO            PIC 9(6).
           03  QCK-FREE1               PIC X(70).
           03  QCK-FREE2               PIC X(70).
           03  QCK-BWPROC              PIC X.

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
           03  BEG-CKNO               PIC 9(06).
           03  END-CKNO               PIC 9(06).

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
           03  FILLER PIC X(36) VALUE "BRNO    CKNO  TYPE    DATE    ".
           03  FILLER PIC X(30) VALUE "AMT       PAYEE".      

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-CK-BRNO                    PIC 9(04).
           03  FILLER                         PIC X(02).
           03  D01-CK-NO                    PIC 9(06).
           03  FILLER                         PIC X(02).
           03  D01-CK-TYPE                    PIC X(02).
           03  FILLER                         PIC X(03).
           03  D01-CK-DATE                    PIC 9999/99/99.
           03  FILLER                         PIC X(03).
           03  D01-CK-AMT                     PIC Z(6)9.99-.
           03  FILLER                         PIC X(03).
           03  D01-CK-PAYEE                   PIC X(30).

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
       COPY "LIBIO/IOCKRN_DEF.CPY".
       COPY "LIBIO/IOCKRN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOCKRN_SCN.CPY".

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
           MOVE "IOCKRN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOCKRN-QUEUE-POS  TO Q-POS.
           MOVE IOCKRN-COPIES-POS TO C-POS.
           MOVE IOCKRN-PRU-POS    TO P-POS.
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
              GO TO ENTER-END-CKNO.

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
       ENTER-BEG-CKNO.

           MOVE "ENNN060BEG-CKNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PROCESS-KEY.
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

           GO TO ENTER-ACCEPT-YN.

      *-----------------------------------------------------------------
       ENTER-ALL-CKNO.

           MOVE "SNNN060BEG-CKNO." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-CKNO.
           PERFORM SCREENIO.

           MOVE "SNNN060END-CKNO." TO VDU.
           MOVE 999999              TO VDUNUM0  END-CKNO.
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

           MOVE "SNNN060BEG-CKNO."       TO VDU.
           MOVE BEG-CKNO                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060END-CKNO."       TO VDU.
           MOVE END-CKNO                 TO VDUNUM0.
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

           MOVE "B"                        TO CK-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - -{ SET WHERE BEG & END FIELDS }
           MOVE BR-NO     TO QCK1-WBEG-BRNO 
                              QCK1-WEND-BRNO.

           MOVE BEG-CKNO  TO QCK1-WBEG-CKNO.
           MOVE END-CKNO  TO QCK1-WEND-CKNO.

      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           PERFORM OPEN-CK1-FILE.
           PERFORM START-CK1-FILE.

      *=================================================================
       PROCESS-BRANCH-NEXT.

           PERFORM READ-CK1-FILE-NEXT.
           IF ( IO-BAD ) OR ( CK-BRNO NOT = CK-PATH-OWNBR )
              GO TO PROCESS-BRANCH-MODULE-END.

           ADD 01 TO RECORDS-READ.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

           IF ( CK-NO < BEG-CKNO )
              GO TO PROCESS-BRANCH-NEXT
           ELSE
           IF ( CK-NO > END-CKNO )
              GO TO PROCESS-BRANCH-MODULE-END.

      *-----------------------------------------------------------------
       PROCESS-BRANCH-PRINT.

           MOVE CK-BRNO                    TO D01-CK-BRNO.
           MOVE CK-NO                  TO D01-CK-NO.
           MOVE CK-TYPE                   TO D01-CK-TYPE.
           MOVE CK-DATE                    TO D01-CK-DATE.
           MOVE CK-AMT                    TO D01-CK-AMT.
           MOVE CK-PAYEE                    TO D01-CK-PAYEE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.
           GO TO PROCESS-BRANCH-NEXT.

      *=================================================================
       PROCESS-BRANCH-MODULE-END.

           PERFORM CLOSE-CK1-FILE.

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

           MOVE CK1-KEY TO PACE-DETAIL-KEY.

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
           MOVE "BEGINNING CKNO               : "    TO R01-PROMPT.
           MOVE BEG-CKNO                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    CKNO               : "    TO R01-PROMPT.
           MOVE END-CKNO                             TO R01-NUMERIC.
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
           MOVE "SQL TABLE IO UTILITY - CKFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOCKRN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOCKRN-SYSDATE.
           DISPLAY IOCKRN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOCKRN-FORM-FIELDS TO WR-BUF.
           MOVE IOCKRN-FORM-LIST TO WR-LIST.
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

           MOVE IOCKRN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOCKRN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOCKRN_EVA.CPY".
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
      *    LOAD CK-REC FROM QCK-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-CK-FIELDS.
           MOVE QCK-BRNO TO CK-BRNO.
           MOVE QCK-NO TO CK-NO.

           IF ( QCK-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO CK-LTOUCH-DATE
           ELSE
              MOVE QCK-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CK-LTOUCH-DATE.

           MOVE QCK-TYPE TO CK-TYPE.

           IF ( QCK-DATE = "1900-01-01" )
              MOVE ZEROES TO CK-DATE
           ELSE
              MOVE QCK-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CK-DATE.

           MOVE QCK-AMT TO CK-AMT.
           MOVE QCK-PAYEE TO CK-PAYEE.
           MOVE QCK-ADR1 TO CK-ADR1.
           MOVE QCK-ADR2 TO CK-ADR2.
           MOVE QCK-CITY TO CK-CITY.
           MOVE QCK-STATE TO CK-STATE.
           MOVE QCK-ZIP TO CK-ZIP.
           MOVE QCK-REF TO CK-REF.
           MOVE QCK-REASON TO CK-REASON.
           MOVE QCK-GLNO-1 TO CK-GLNO(1).
           MOVE QCK-GLAMT-1 TO CK-GLAMT(1).
           MOVE QCK-GLNO-2 TO CK-GLNO(2).
           MOVE QCK-GLAMT-2 TO CK-GLAMT(2).
           MOVE QCK-GLNO-3 TO CK-GLNO(3).
           MOVE QCK-GLAMT-3 TO CK-GLAMT(3).
           MOVE QCK-GLNO-4 TO CK-GLNO(4).
           MOVE QCK-GLAMT-4 TO CK-GLAMT(4).
           MOVE QCK-GLNO-5 TO CK-GLNO(5).
           MOVE QCK-GLAMT-5 TO CK-GLAMT(5).
           MOVE QCK-VOID TO CK-VOID.
           MOVE QCK-PAID TO CK-PAID.
           MOVE QCK-CLASS TO CK-CLASS.
           MOVE QCK-DLNO TO CK-DLNO.
           MOVE QCK-PGM TO CK-PGM.
           MOVE QCK-NC-SEQNO TO CK-NC-SEQNO.

           IF ( QCK-VOID-DATE = "1900-01-01" )
              MOVE ZEROES TO CK-VOID-DATE
           ELSE
              MOVE QCK-VOID-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CK-VOID-DATE.

           MOVE QCK-ACCTNO TO CK-ACCTNO.
           MOVE QCK-DLRVNO TO CK-DLRVNO.
           MOVE QCK-CLEARED-FG TO CK-CLEARED-FG.
           MOVE QCK-SSNO TO CK-SSNO.
           MOVE QCK-BATCHID TO CK-BATCHID.
           MOVE QCK-UNPAID TO CK-UNPAID.

           IF ( QCK-CLEARED-DATE = "1900-01-01" )
              MOVE ZEROES TO CK-CLEARED-DATE
           ELSE
              MOVE QCK-CLEARED-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CK-CLEARED-DATE.

           MOVE QCK-TILLNO TO CK-TILLNO.
           MOVE QCK-EVENT-CD TO CK-EVENT-CD.
           MOVE QCK-PAYEE-NO TO CK-PAYEE-NO.
           MOVE QCK-FREE1 TO CK-FREE1.
           MOVE QCK-FREE2 TO CK-FREE2.
           MOVE QCK-BWPROC TO CK-BWPROC.
 
      *-----------------------------------------------------------------
      *    LOAD QCK-REC FROM CK-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-CK-FIELDS.
           MOVE CK-BRNO TO QCK-BRNO.
           MOVE CK-NO TO QCK-NO.

           IF ( CK-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QCK-LTOUCH-DATE
           ELSE
              MOVE CK-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCK-LTOUCH-DATE.

           MOVE CK-TYPE TO QCK-TYPE.

           IF ( CK-DATE = ZEROES )
              MOVE "1900-01-01" TO QCK-DATE
           ELSE
              MOVE CK-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCK-DATE.

           MOVE CK-AMT TO QCK-AMT.
           MOVE CK-PAYEE TO QCK-PAYEE.
           MOVE CK-ADR1 TO QCK-ADR1.
           MOVE CK-ADR2 TO QCK-ADR2.
           MOVE CK-CITY TO QCK-CITY.
           MOVE CK-STATE TO QCK-STATE.
           MOVE CK-ZIP TO QCK-ZIP.
           MOVE CK-REF TO QCK-REF.
           MOVE CK-REASON TO QCK-REASON.
           MOVE CK-GLNO(1) TO QCK-GLNO-1.
           MOVE CK-GLAMT(1) TO QCK-GLAMT-1.
           MOVE CK-GLNO(2) TO QCK-GLNO-2.
           MOVE CK-GLAMT(2) TO QCK-GLAMT-2.
           MOVE CK-GLNO(3) TO QCK-GLNO-3.
           MOVE CK-GLAMT(3) TO QCK-GLAMT-3.
           MOVE CK-GLNO(4) TO QCK-GLNO-4.
           MOVE CK-GLAMT(4) TO QCK-GLAMT-4.
           MOVE CK-GLNO(5) TO QCK-GLNO-5.
           MOVE CK-GLAMT(5) TO QCK-GLAMT-5.
           MOVE CK-VOID TO QCK-VOID.
           MOVE CK-PAID TO QCK-PAID.
           MOVE CK-CLASS TO QCK-CLASS.
           MOVE CK-DLNO TO QCK-DLNO.
           MOVE CK-PGM TO QCK-PGM.
           MOVE CK-NC-SEQNO TO QCK-NC-SEQNO.

           IF ( CK-VOID-DATE = ZEROES )
              MOVE "1900-01-01" TO QCK-VOID-DATE
           ELSE
              MOVE CK-VOID-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCK-VOID-DATE.

           MOVE CK-ACCTNO TO QCK-ACCTNO.
           MOVE CK-DLRVNO TO QCK-DLRVNO.
           MOVE CK-CLEARED-FG TO QCK-CLEARED-FG.
           MOVE CK-SSNO TO QCK-SSNO.
           MOVE CK-BATCHID TO QCK-BATCHID.
           MOVE CK-UNPAID TO QCK-UNPAID.

           IF ( CK-CLEARED-DATE = ZEROES )
              MOVE "1900-01-01" TO QCK-CLEARED-DATE
           ELSE
              MOVE CK-CLEARED-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCK-CLEARED-DATE.

           MOVE CK-TILLNO TO QCK-TILLNO.
           MOVE CK-EVENT-CD TO QCK-EVENT-CD.
           MOVE CK-PAYEE-NO TO QCK-PAYEE-NO.
           MOVE CK-FREE1 TO QCK-FREE1.
           MOVE CK-FREE2 TO QCK-FREE2.
           MOVE CK-BWPROC TO QCK-BWPROC.
 
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-CK1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CK-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CK-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO CK-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CK-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO CK-ALL-DATA
              MOVE CK-ALL-PATH             TO CK-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CK-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO CK-ALL-BASE
              MOVE FILPATH-MACHINE         TO CK-ALL-MACHINE
              MOVE FILPATH-DATA            TO CK-ALL-DATA
              MOVE CK-ALL-PATH             TO CK-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-CKFILE NOT = "S" )
              MOVE CK-PATH-BEG             TO CK-SELECT-PATH-BEG
              MOVE CK-PATH-FILE            TO CK-SELECT-PATH-FILE
           ELSE
              MOVE CK-PATH-SQL             TO CK-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-CK1-FILE.
           PERFORM LOAD-CK1-FILE.
           PERFORM OPEN-IT.
           MOVE CK-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-CK1-FILE.
           PERFORM START-IT.
           MOVE CK-PATH-OWNBR  TO CK-BRNO.
           MOVE CK-SELECT-PATH TO E-FILE.
           MOVE CK1-KEY        TO E-KEYX.

           IF ( CK1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-CK1-FILE.

           IF ( QCK1-WEND-BRNO = ZEROES    ) OR
              ( QCK1-WEND-BRNO NOT NUMERIC )
              MOVE CK-PATH-OWNBR        TO QCK1-WBEG-BRNO
              MOVE CK-PATH-OWNBR        TO QCK1-WEND-BRNO
              MOVE CK-NO                TO QCK1-WBEG-CKNO
              MOVE ALL "9"              TO QCK1-WEND-CKNO.

           EXEC SQL
            DECLARE CK1_CURSOR CURSOR FOR
            SELECT CKFILE.CK_BRNO,
                   CKFILE.CK_NO,
                   ISNULL(CAST(CKFILE.CK_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CKFILE.CK_TYPE,''),
                   ISNULL(CAST(CKFILE.CK_DATE AS VARCHAR(10)),'0'),
                   CKFILE.CK_AMT,
                   ISNULL(CKFILE.CK_PAYEE,''),
                   ISNULL(CKFILE.CK_ADR1,''),
                   ISNULL(CKFILE.CK_ADR2,''),
                   ISNULL(CKFILE.CK_CITY,''),
                   ISNULL(CKFILE.CK_STATE,''),
                   ISNULL(CKFILE.CK_ZIP,''),
                   CKFILE.CK_REF,
                   ISNULL(CKFILE.CK_REASON,''),
                   CKFILE.CK_GLNO_1,
                   CKFILE.CK_GLAMT_1,
                   CKFILE.CK_GLNO_2,
                   CKFILE.CK_GLAMT_2,
                   CKFILE.CK_GLNO_3,
                   CKFILE.CK_GLAMT_3,
                   CKFILE.CK_GLNO_4,
                   CKFILE.CK_GLAMT_4,
                   CKFILE.CK_GLNO_5,
                   CKFILE.CK_GLAMT_5,
                   ISNULL(CKFILE.CK_VOID,''),
                   ISNULL(CKFILE.CK_PAID,''),
                   CKFILE.CK_CLASS,
                   CKFILE.CK_DLNO,
                   ISNULL(CKFILE.CK_PGM,''),
                   CKFILE.CK_NC_SEQNO,
                   ISNULL(CAST(CKFILE.CK_VOID_DATE
                                                  AS VARCHAR(10)),'0'),
                   CKFILE.CK_ACCTNO,
                   CKFILE.CK_DLRVNO,
                   ISNULL(CKFILE.CK_CLEARED_FG,''),
                   CKFILE.CK_SSNO,
                   CKFILE.CK_BATCHID,
                   ISNULL(CKFILE.CK_UNPAID,''),
                   ISNULL(CAST(CKFILE.CK_CLEARED_DATE
                                                  AS VARCHAR(10)),'0'),
                   CKFILE.CK_TILLNO,
                   ISNULL(CKFILE.CK_EVENT_CD,''),
                   CKFILE.CK_PAYEE_NO,
                   ISNULL(CKFILE.CK_FREE1,''),
                   ISNULL(CKFILE.CK_FREE2,''),
                   ISNULL(CKFILE.CK_BWPROC,'')
            FROM DBO.CKFILE
            WHERE CKFILE.CK_BRNO >= :QCK1-WBEG-BRNO
              AND CKFILE.CK_BRNO <= :QCK1-WEND-BRNO
              AND CKFILE.CK_NO >= :QCK1-WBEG-CKNO
              AND CKFILE.CK_NO <= :QCK1-WEND-CKNO
              ORDER BY CKFILE.CK_BRNO,
                       CKFILE.CK_NO
           END-EXEC.

           EXEC SQL
               OPEN CK1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QCK1-WBEG-BRNO 
                          QCK1-WEND-BRNO
                          QCK1-WBEG-CKNO
                          QCK1-WEND-CKNO.

           MOVE STAT---GOOD TO CK1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-CK1-FILE.
           PERFORM READ-IT.
           MOVE CK-PATH-OWNBR  TO CK-BRNO. 
           MOVE CK-SELECT-PATH TO E-FILE.
           MOVE CK1-KEY        TO E-KEYX.

           MOVE CK-BRNO   TO QCK-BRNO.
           MOVE CK-NO   TO QCK-NO.

           EXEC SQL
            SELECT CKFILE.CK_BRNO,
                   CKFILE.CK_NO,
                   ISNULL(CAST(CKFILE.CK_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CKFILE.CK_TYPE,''),
                   ISNULL(CAST(CKFILE.CK_DATE AS VARCHAR(10)),'0'),
                   CKFILE.CK_AMT,
                   ISNULL(CKFILE.CK_PAYEE,''),
                   ISNULL(CKFILE.CK_ADR1,''),
                   ISNULL(CKFILE.CK_ADR2,''),
                   ISNULL(CKFILE.CK_CITY,''),
                   ISNULL(CKFILE.CK_STATE,''),
                   ISNULL(CKFILE.CK_ZIP,''),
                   CKFILE.CK_REF,
                   ISNULL(CKFILE.CK_REASON,''),
                   CKFILE.CK_GLNO_1,
                   CKFILE.CK_GLAMT_1,
                   CKFILE.CK_GLNO_2,
                   CKFILE.CK_GLAMT_2,
                   CKFILE.CK_GLNO_3,
                   CKFILE.CK_GLAMT_3,
                   CKFILE.CK_GLNO_4,
                   CKFILE.CK_GLAMT_4,
                   CKFILE.CK_GLNO_5,
                   CKFILE.CK_GLAMT_5,
                   ISNULL(CKFILE.CK_VOID,''),
                   ISNULL(CKFILE.CK_PAID,''),
                   CKFILE.CK_CLASS,
                   CKFILE.CK_DLNO,
                   ISNULL(CKFILE.CK_PGM,''),
                   CKFILE.CK_NC_SEQNO,
                   ISNULL(CAST(CKFILE.CK_VOID_DATE
                                                  AS VARCHAR(10)),'0'),
                   CKFILE.CK_ACCTNO,
                   CKFILE.CK_DLRVNO,
                   ISNULL(CKFILE.CK_CLEARED_FG,''),
                   CKFILE.CK_SSNO,
                   CKFILE.CK_BATCHID,
                   ISNULL(CKFILE.CK_UNPAID,''),
                   ISNULL(CAST(CKFILE.CK_CLEARED_DATE
                                                  AS VARCHAR(10)),'0'),
                   CKFILE.CK_TILLNO,
                   ISNULL(CKFILE.CK_EVENT_CD,''),
                   CKFILE.CK_PAYEE_NO,
                   ISNULL(CKFILE.CK_FREE1,''),
                   ISNULL(CKFILE.CK_FREE2,''),
                   ISNULL(CKFILE.CK_BWPROC,'')
            INTO :QCK-REC
            FROM DBO.CKFILE
            WHERE CKFILE.CK_BRNO = :QCK-BRNO
              AND CKFILE.CK_NO = :QCK-NO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CK1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-CK-FIELDS.

      *-----------------------------------------------------------------
       READ-CK1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE CK-PATH-OWNBR  TO CK-BRNO.
           MOVE CK-SELECT-PATH TO E-FILE.
           MOVE CK1-KEY        TO E-KEYX.
           INITIALIZE QCK-REC.

           IF ( CK1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH CK1_CURSOR INTO :QCK-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CK1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-CK-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-CK1-FILE.

           IF ( CK1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE CK1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO CK1-CURSOR-STAT.

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
              DESTROY IOCKRN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
