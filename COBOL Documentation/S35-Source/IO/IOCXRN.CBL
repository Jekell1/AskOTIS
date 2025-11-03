       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOCXRN.
       AUTHOR.        BAH
       DATE-WRITTEN.  04-01-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOCXRN.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : CXFILE 
      * TABLE DESC    : TEMPORARY CHECK FILE
      * OUTPUT FILE   : XPC/IOCXRN.PRE
      * USED FOR      : LIBLP/LPWSCX.CPY  --- QCX-REC
      *                 LIBLP/LPCX1R.CPY  --- READ
      *                 LIBLP/LPCX1RN.CPY --- READ/READ-NEXT/START
      *                 LIBLP/LPCX2RN.CPY --- READ/READ-NEXT/START
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
           "@(#)IO/IOCXRN.CBL S34 05/19/21-13:18:23 >".

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
       COPY "LIBLP/LP01CX.CPY".
       COPY "LIBLP/LPWSCX.CPY".

      *=================================================================
      *
      *    Q - F I L E
      *
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QCX1-WHERE-BEG-KEY.
           03  QCX1-WBEG-BRNO           PIC 9(04).
           03  QCX1-WBEG-TYPE           PIC X(02).
           03  QCX1-WBEG-SEQNO          PIC 9(04).
 
       01  QCX1-WHERE-END-KEY.
           03  QCX1-WEND-BRNO           PIC 9(04).
           03  QCX1-WEND-TYPE           PIC X(02).
           03  QCX1-WEND-SEQNO          PIC 9(04).
 
       01  QCX2-WHERE-BEG-KEY.
           03  QCX2-WBEG-BRNO           PIC 9(04).
           03  QCX2-WBEG-PAYEE          PIC X(60).
 
       01  QCX2-WHERE-END-KEY.
           03  QCX2-WEND-BRNO           PIC 9(04).
           03  QCX2-WEND-PAYEE          PIC X(60).
 
       01  QCX-REC.
           03  QCX-BRNO             PIC 9(4).
           03  QCX-TYPE             PIC X(2).
           03  QCX-SEQNO            PIC 9(4).
           03  QCX-PAYEE            PIC X(60).
           03  QCX-LTOUCH-DATE          PIC X(10).
           03  QCX-ADR1                 PIC X(30).
           03  QCX-ADR2                 PIC X(30).
           03  QCX-CITY                 PIC X(30).
           03  QCX-STATE                PIC X(2).
           03  QCX-ZIP                  PIC X(10).
           03  QCX-AMT                  PIC S9(7)V99.
           03  QCX-REF                  PIC 9(6).
           03  QCX-REASON               PIC X(30).
           03  QCX-GLNO-1             PIC 9(11).
           03  QCX-GLAMT-1            PIC S9(7)V99.
           03  QCX-GLNO-2             PIC 9(11).
           03  QCX-GLAMT-2            PIC S9(7)V99.
           03  QCX-GLNO-3             PIC 9(11).
           03  QCX-GLAMT-3            PIC S9(7)V99.
           03  QCX-GLNO-4             PIC 9(11).
           03  QCX-GLAMT-4            PIC S9(7)V99.
           03  QCX-GLNO-5             PIC 9(11).
           03  QCX-GLAMT-5            PIC S9(7)V99.
           03  QCX-FREE1                PIC X(70).
           03  QCX-FREE2                PIC X(70).
           03  QCX-VOID                 PIC X.
           03  QCX-CLASS                PIC 9(3).
           03  QCX-CKNO                 PIC 9(7).
           03  QCX-DLNO                 PIC 9(4)V99.
           03  QCX-DATE                 PIC X(10).
           03  QCX-PAID                 PIC X.
           03  QCX-PGM                  PIC XX.
           03  QCX-BATCHID              PIC 9(10).
           03  QCX-BWPROC               PIC X.
           03  QCX-CKNO-NEW             PIC 9(7).
           03  QCX-DLRVNO               PIC 9.
           03  QCX-DT-SEQ               PIC 9(4).
           03  QCX-RESCISSION           PIC X.
           03  QCX-TILLNO               PIC 99.
           03  QCX-ACCTNO               PIC 9(8).
           03  QCX-SERVACCT             PIC X.
           03  QCX-DT-DATE              PIC X(10).
           03  QCX-SSNO                 PIC 9(9).
           03  QCX-DT-PAID              PIC X.
           03  QCX-DT-UNPAID            PIC X.
           03  QCX-CKPAID-NEW           PIC X.
           03  QCX-EVENT-CD             PIC X(2).
           03  QCX-PAYEE-NO             PIC 9(6).
           03  QCX-DEPOSIT-WITHDR       PIC X.

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
           03  BEG-TYPE                PIC X(02).
           03  END-TYPE                PIC X(02).
           03  BEG-SEQNO               PIC 9(04).
           03  END-SEQNO               PIC 9(04).
           03  BEG-PAYEE               PIC X(60).
           03  END-PAYEE               PIC X(60).

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
           03  FILLER PIC X(39) VALUE "BRNO  TYPE  SEQ  PAYEE      ".
           03  FILLER PIC X(30) VALUE "AMT      REASON".

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-CX-BRNO                    PIC 9(04).
           03  FILLER                         PIC X(03).
           03  D01-CX-TYPE                    PIC XX.
           03  FILLER                         PIC X(03).
           03  D01-CX-SEQNO                   PIC 9(04).
           03  FILLER                         PIC X(01).
           03  D01-CX-PAYEE                   PIC X(15).
           03  FILLER                         PIC X(03).
           03  D01-CX-AMT                     PIC Z(6)9.99.
           03  FILLER                         PIC X(03).
           03  D01-CX-REASON                  PIC X(10).

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
       COPY "LIBIO/IOCXRN_DEF.CPY".
       COPY "LIBIO/IOCXRN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOCXRN_SCN.CPY".

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
           MOVE "IOCXRN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOCXRN-QUEUE-POS  TO Q-POS.
           MOVE IOCXRN-COPIES-POS TO C-POS.
           MOVE IOCXRN-PRU-POS    TO P-POS.
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
              GO TO ENTER-END-PAYEE.

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
              ( VDUNUM0 > 2      ) 
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PROCESS-KEY.

           MOVE VDUNUM0 TO PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE LEGEND-BEG-END-RANGE TO LEGEND.
           PERFORM SEND-LEGEND.

      *=================================================================
       ENTER-BEG-TYPE.

           MOVE "E  A020BEG-TYPE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PROCESS-KEY.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-TYPE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-TYPE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-TYPE.

      *-----------------------------------------------------------------
       ENTER-END-TYPE.
           MOVE "E  A020END-TYPE." TO VDU.
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

           GO TO ENTER-BEG-SEQNO.

      *-----------------------------------------------------------------
       ENTER-ALL-TYPE.

           MOVE "S  A000BEG-TYPE." TO VDU.
           MOVE SPACES              TO VDUBUF  BEG-TYPE.
           PERFORM SCREENIO.

           MOVE "S  A000END-TYPE." TO VDU.
           MOVE ALL "Z"             TO VDUBUF  END-TYPE.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-SEQNO.

           MOVE "ENNN040BEG-SEQNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-TYPE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-SEQNO.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-SEQNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-SEQNO.

      *-----------------------------------------------------------------
       ENTER-END-SEQNO.

           MOVE "ENNN040END-SEQNO." TO VDU.
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

           GO TO ENTER-BEG-PAYEE.

      *-----------------------------------------------------------------
       ENTER-ALL-SEQNO.

           MOVE "SNNN040BEG-SEQNO." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-SEQNO.
           PERFORM SCREENIO.

           MOVE "SNNN040END-SEQNO." TO VDU.
           MOVE 9999                TO VDUNUM0  END-SEQNO.
           PERFORM SCREENIO.

       ENTER-BEG-PAYEE.

           MOVE "E  A150BEG-PAYEE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-SEQNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-PAYEE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-PAYEE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-PAYEE.

      *-----------------------------------------------------------------
       ENTER-END-PAYEE.
           MOVE "E  A150END-PAYEE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-PAYEE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-PAYEE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-PAYEE.

           IF ( BEG-PAYEE > END-PAYEE )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-PAYEE.

           GO TO ENTER-ACCEPT-YN.

      *-----------------------------------------------------------------
       ENTER-ALL-PAYEE.

           MOVE "S  A000BEG-PAYEE." TO VDU.
           MOVE SPACES              TO VDUBUF  BEG-PAYEE.
           PERFORM SCREENIO.

           MOVE "S  A000END-PAYEE." TO VDU.
           MOVE ALL "Z"             TO VDUBUF  END-PAYEE.
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

           MOVE "S  A020BEG-TYPE."       TO VDU.
           MOVE BEG-TYPE                 TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A020END-TYPE."       TO VDU.
           MOVE END-TYPE                 TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A040BEG-SEQNO."      TO VDU.
           MOVE BEG-SEQNO                TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040END-SEQNO."      TO VDU.
           MOVE END-SEQNO                TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A150BEG-PAYEE."       TO VDU.
           MOVE BEG-PAYEE                 TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A150END-PAYEE."       TO VDU.
           MOVE END-PAYEE                 TO VDUBUF.
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

           MOVE "B"                        TO CX-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - -{ SET WHERE BEG & END FIELDS }
           MOVE BR-NO     TO QCX1-WBEG-BRNO 
                              QCX2-WBEG-BRNO.
           MOVE BR-NO     TO QCX1-WEND-BRNO
                              QCX2-WEND-BRNO.
           MOVE BEG-TYPE   TO QCX1-WBEG-TYPE.
           MOVE END-TYPE   TO QCX1-WEND-TYPE.
           MOVE BEG-SEQNO  TO QCX1-WBEG-SEQNO.
           MOVE END-SEQNO  TO QCX1-WEND-SEQNO.

           MOVE BEG-PAYEE  TO QCX2-WBEG-PAYEE.
           MOVE END-PAYEE  TO QCX2-WEND-PAYEE.

      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           IF ( PROCESS-KEY = 1 )
              PERFORM OPEN-CX1-FILE
              PERFORM START-CX1-FILE
           ELSE
              PERFORM OPEN-CX2-FILE
              PERFORM START-CX2-FILE.

      *=================================================================
       PROCESS-BRANCH-NEXT.

           IF ( PROCESS-KEY = 1 )
              PERFORM READ-CX1-FILE-NEXT
           ELSE
              PERFORM READ-CX2-FILE-NEXT.
           IF ( IO-BAD ) OR ( CX-BRNO NOT = CX-PATH-OWNBR )
              GO TO PROCESS-BRANCH-MODULE-END.

           ADD 01 TO RECORDS-READ.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

           IF ( PROCESS-KEY = 1 )
              IF ( CX-TYPE < BEG-TYPE )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( CX-TYPE > END-TYPE )
                 GO TO PROCESS-BRANCH-MODULE-END.
           IF ( PROCESS-KEY = 1 )
              IF CX-SEQNO < BEG-SEQNO OR > END-SEQNO
                 GO TO PROCESS-BRANCH-NEXT.

           IF ( PROCESS-KEY = 2 )
              IF ( CX-PAYEE  < BEG-PAYEE  )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( CX-PAYEE  > END-PAYEE  )
                 GO TO PROCESS-BRANCH-MODULE-END.


      *-----------------------------------------------------------------
       PROCESS-BRANCH-PRINT.

           MOVE CX-BRNO                    TO D01-CX-BRNO.
           MOVE CX-TYPE                    TO D01-CX-TYPE.
           MOVE CX-SEQNO                   TO D01-CX-SEQNO.
           MOVE CX-PAYEE                   TO D01-CX-PAYEE.
           MOVE CX-AMT                     TO D01-CX-AMT.
           MOVE CX-REASON                  TO D01-CX-REASON.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.
           GO TO PROCESS-BRANCH-NEXT.

      *=================================================================
       PROCESS-BRANCH-MODULE-END.

           IF ( PROCESS-KEY = 1 )
              PERFORM CLOSE-CX1-FILE
           ELSE
           IF ( PROCESS-KEY = 2 )
              PERFORM CLOSE-CX2-FILE.

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
              MOVE CX1-KEY TO PACE-DETAIL-KEY
           ELSE
           IF ( PROCESS-KEY = 2 )
              STRING CX-BRNO, CX-PAYEE, 
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
           MOVE "BEGINNING TYPE               : "    TO R01-PROMPT.
           MOVE BEG-TYPE                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    TYPE               : "    TO R01-PROMPT.
           MOVE END-TYPE                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING PAYEE               : "    TO R01-PROMPT.
           MOVE BEG-PAYEE                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    PAYEE               : "    TO R01-PROMPT.
           MOVE END-PAYEE                             TO R01-NUMERIC.
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
           MOVE "SQL TABLE IO UTILITY - CXFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOCXRN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOCXRN-SYSDATE.
           DISPLAY IOCXRN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOCXRN-FORM-FIELDS TO WR-BUF.
           MOVE IOCXRN-FORM-LIST TO WR-LIST.
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

           MOVE IOCXRN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOCXRN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOCXRN_EVA.CPY".
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
      *    LOAD CX-REC FROM QCX-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-CX-FIELDS.
           MOVE QCX-BRNO TO CX-BRNO.
           MOVE QCX-TYPE TO CX-TYPE.
           MOVE QCX-SEQNO TO CX-SEQNO.
           MOVE QCX-PAYEE TO CX-PAYEE.

           IF ( QCX-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO CX-LTOUCH-DATE
           ELSE
              MOVE QCX-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CX-LTOUCH-DATE.

           MOVE QCX-ADR1 TO CX-ADR1.
           MOVE QCX-ADR2 TO CX-ADR2.
           MOVE QCX-CITY TO CX-CITY.
           MOVE QCX-STATE TO CX-STATE.
           MOVE QCX-ZIP TO CX-ZIP.
           MOVE QCX-AMT TO CX-AMT.
           MOVE QCX-REF TO CX-REF.
           MOVE QCX-REASON TO CX-REASON.
           MOVE QCX-GLNO-1 TO CX-GLNO(1).
           MOVE QCX-GLAMT-1 TO CX-GLAMT(1).
           MOVE QCX-GLNO-2 TO CX-GLNO(2).
           MOVE QCX-GLAMT-2 TO CX-GLAMT(2).
           MOVE QCX-GLNO-3 TO CX-GLNO(3).
           MOVE QCX-GLAMT-3 TO CX-GLAMT(3).
           MOVE QCX-GLNO-4 TO CX-GLNO(4).
           MOVE QCX-GLAMT-4 TO CX-GLAMT(4).
           MOVE QCX-GLNO-5 TO CX-GLNO(5).
           MOVE QCX-GLAMT-5 TO CX-GLAMT(5).
           MOVE QCX-FREE1 TO CX-FREE1.
           MOVE QCX-FREE2 TO CX-FREE2.
           MOVE QCX-VOID TO CX-VOID.
           MOVE QCX-CLASS TO CX-CLASS.
           MOVE QCX-CKNO TO CX-CKNO.
           MOVE QCX-DLNO TO CX-DLNO.

           IF ( QCX-DATE = "1900-01-01" )
              MOVE ZEROES TO CX-DATE
           ELSE
              MOVE QCX-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CX-DATE.

           MOVE QCX-PAID TO CX-PAID.
           MOVE QCX-PGM TO CX-PGM.
           MOVE QCX-BATCHID TO CX-BATCHID.
           MOVE QCX-BWPROC TO CX-BWPROC.
           MOVE QCX-CKNO-NEW TO CX-CKNO-NEW.
           MOVE QCX-DLRVNO TO CX-DLRVNO.
           MOVE QCX-DT-SEQ TO CX-DT-SEQ.
           MOVE QCX-RESCISSION TO CX-RESCISSION.
           MOVE QCX-TILLNO TO CX-TILLNO.
           MOVE QCX-ACCTNO TO CX-ACCTNO.
           MOVE QCX-SERVACCT TO CX-SERVACCT.

           IF ( QCX-DT-DATE = "1900-01-01" )
              MOVE ZEROES TO CX-DT-DATE
           ELSE
              MOVE QCX-DT-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CX-DT-DATE.

           MOVE QCX-SSNO TO CX-SSNO.
           MOVE QCX-DT-PAID TO CX-DT-PAID.
           MOVE QCX-DT-UNPAID TO CX-DT-UNPAID.
           MOVE QCX-CKPAID-NEW TO CX-CKPAID-NEW.
           MOVE QCX-EVENT-CD TO CX-EVENT-CD.
           MOVE QCX-PAYEE-NO TO CX-PAYEE-NO.
           MOVE QCX-DEPOSIT-WITHDR TO CX-DEPOSIT-WITHDR.
 
      *-----------------------------------------------------------------
      *    LOAD QCX-REC FROM CX-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-CX-FIELDS.
           MOVE CX-BRNO TO QCX-BRNO.
           MOVE CX-TYPE TO QCX-TYPE.
           MOVE CX-SEQNO TO QCX-SEQNO.
           MOVE CX-PAYEE TO QCX-PAYEE.

           IF ( CX-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QCX-LTOUCH-DATE
           ELSE
              MOVE CX-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCX-LTOUCH-DATE.

           MOVE CX-ADR1 TO QCX-ADR1.
           MOVE CX-ADR2 TO QCX-ADR2.
           MOVE CX-CITY TO QCX-CITY.
           MOVE CX-STATE TO QCX-STATE.
           MOVE CX-ZIP TO QCX-ZIP.
           MOVE CX-AMT TO QCX-AMT.
           MOVE CX-REF TO QCX-REF.
           MOVE CX-REASON TO QCX-REASON.
           MOVE CX-GLNO(1) TO QCX-GLNO-1.
           MOVE CX-GLAMT(1) TO QCX-GLAMT-1.
           MOVE CX-GLNO(2) TO QCX-GLNO-2.
           MOVE CX-GLAMT(2) TO QCX-GLAMT-2.
           MOVE CX-GLNO(3) TO QCX-GLNO-3.
           MOVE CX-GLAMT(3) TO QCX-GLAMT-3.
           MOVE CX-GLNO(4) TO QCX-GLNO-4.
           MOVE CX-GLAMT(4) TO QCX-GLAMT-4.
           MOVE CX-GLNO(5) TO QCX-GLNO-5.
           MOVE CX-GLAMT(5) TO QCX-GLAMT-5.
           MOVE CX-FREE1 TO QCX-FREE1.
           MOVE CX-FREE2 TO QCX-FREE2.
           MOVE CX-VOID TO QCX-VOID.
           MOVE CX-CLASS TO QCX-CLASS.
           MOVE CX-CKNO TO QCX-CKNO.
           MOVE CX-DLNO TO QCX-DLNO.

           IF ( CX-DATE = ZEROES )
              MOVE "1900-01-01" TO QCX-DATE
           ELSE
              MOVE CX-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCX-DATE.

           MOVE CX-PAID TO QCX-PAID.
           MOVE CX-PGM TO QCX-PGM.
           MOVE CX-BATCHID TO QCX-BATCHID.
           MOVE CX-BWPROC TO QCX-BWPROC.
           MOVE CX-CKNO-NEW TO QCX-CKNO-NEW.
           MOVE CX-DLRVNO TO QCX-DLRVNO.
           MOVE CX-DT-SEQ TO QCX-DT-SEQ.
           MOVE CX-RESCISSION TO QCX-RESCISSION.
           MOVE CX-TILLNO TO QCX-TILLNO.
           MOVE CX-ACCTNO TO QCX-ACCTNO.
           MOVE CX-SERVACCT TO QCX-SERVACCT.

           IF ( CX-DT-DATE = ZEROES )
              MOVE "1900-01-01" TO QCX-DT-DATE
           ELSE
              MOVE CX-DT-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCX-DT-DATE.

           MOVE CX-SSNO TO QCX-SSNO.
           MOVE CX-DT-PAID TO QCX-DT-PAID.
           MOVE CX-DT-UNPAID TO QCX-DT-UNPAID.
           MOVE CX-CKPAID-NEW TO QCX-CKPAID-NEW.
           MOVE CX-EVENT-CD TO QCX-EVENT-CD.
           MOVE CX-PAYEE-NO TO QCX-PAYEE-NO.
           MOVE CX-DEPOSIT-WITHDR TO QCX-DEPOSIT-WITHDR.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-CX1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CX-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CX-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO CX-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CX-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO CX-ALL-DATA
              MOVE CX-ALL-PATH             TO CX-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CX-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO CX-ALL-BASE
              MOVE FILPATH-MACHINE         TO CX-ALL-MACHINE
              MOVE FILPATH-DATA            TO CX-ALL-DATA
              MOVE CX-ALL-PATH             TO CX-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-CXFILE NOT = "S" )
              MOVE CX-PATH-BEG             TO CX-SELECT-PATH-BEG
              MOVE CX-PATH-FILE            TO CX-SELECT-PATH-FILE
           ELSE
              MOVE CX-PATH-SQL             TO CX-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-CX1-FILE.
           PERFORM LOAD-CX1-FILE.
           PERFORM OPEN-IT.
           MOVE CX-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-CX1-FILE.
           PERFORM START-IT.
           MOVE CX-PATH-OWNBR  TO CX-BRNO.
           MOVE CX-SELECT-PATH TO E-FILE.
           MOVE CX1-KEY        TO E-KEYX.

           IF ( CX1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-CX1-FILE.

           IF ( QCX1-WEND-SEQNO = ZEROES    ) OR
              ( QCX1-WEND-SEQNO NOT NUMERIC )
              MOVE CX-PATH-OWNBR        TO QCX1-WBEG-BRNO
              MOVE CX-PATH-OWNBR        TO QCX1-WEND-BRNO
              MOVE CX-TYPE              TO QCX1-WBEG-TYPE
              MOVE ALL "Z"              TO QCX1-WEND-TYPE
              MOVE CX-SEQNO             TO QCX1-WBEG-SEQNO
              MOVE ALL "9"              TO QCX1-WEND-SEQNO.

           EXEC SQL
            DECLARE CX1_CURSOR CURSOR FOR
            SELECT CXFILE.CX_BRNO,
                   ISNULL(CXFILE.CX_TYPE,''),
                   CXFILE.CX_SEQNO,
                   ISNULL(CXFILE.CX_PAYEE,''),
                   ISNULL(CAST(CXFILE.CX_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CXFILE.CX_ADR1,''),
                   ISNULL(CXFILE.CX_ADR2,''),
                   ISNULL(CXFILE.CX_CITY,''),
                   ISNULL(CXFILE.CX_STATE,''),
                   ISNULL(CXFILE.CX_ZIP,''),
                   CXFILE.CX_AMT,
                   CXFILE.CX_REF,
                   ISNULL(CXFILE.CX_REASON,''),
                   CXFILE.CX_GLNO_1,
                   CXFILE.CX_GLAMT_1,
                   CXFILE.CX_GLNO_2,
                   CXFILE.CX_GLAMT_2,
                   CXFILE.CX_GLNO_3,
                   CXFILE.CX_GLAMT_3,
                   CXFILE.CX_GLNO_4,
                   CXFILE.CX_GLAMT_4,
                   CXFILE.CX_GLNO_5,
                   CXFILE.CX_GLAMT_5,
                   ISNULL(CXFILE.CX_FREE1,''),
                   ISNULL(CXFILE.CX_FREE2,''),
                   ISNULL(CXFILE.CX_VOID,''),
                   CXFILE.CX_CLASS,
                   CXFILE.CX_CKNO,
                   CXFILE.CX_DLNO,
                   ISNULL(CAST(CXFILE.CX_DATE AS VARCHAR(10)),'0'),
                   ISNULL(CXFILE.CX_PAID,''),
                   ISNULL(CXFILE.CX_PGM,''),
                   CXFILE.CX_BATCHID,
                   ISNULL(CXFILE.CX_BWPROC,''),
                   CXFILE.CX_CKNO_NEW,
                   CXFILE.CX_DLRVNO,
                   CXFILE.CX_DT_SEQ,
                   ISNULL(CXFILE.CX_RESCISSION,''),
                   CXFILE.CX_TILLNO,
                   CXFILE.CX_ACCTNO,
                   ISNULL(CXFILE.CX_SERVACCT,''),
                   ISNULL(CAST(CXFILE.CX_DT_DATE AS VARCHAR(10)),'0'),
                   CXFILE.CX_SSNO,
                   ISNULL(CXFILE.CX_DT_PAID,''),
                   ISNULL(CXFILE.CX_DT_UNPAID,''),
                   ISNULL(CXFILE.CX_CKPAID_NEW,''),
                   ISNULL(CXFILE.CX_EVENT_CD,''),
                   CXFILE.CX_PAYEE_NO,
                   ISNULL(CXFILE.CX_DEPOSIT_WITHDR,'')
            FROM DBO.CXFILE
            WHERE CXFILE.CX_BRNO >= :QCX1-WBEG-BRNO
              AND CXFILE.CX_BRNO <= :QCX1-WEND-BRNO
              AND CXFILE.CX_TYPE >= :QCX1-WBEG-TYPE
              AND CXFILE.CX_TYPE <= :QCX1-WEND-TYPE
              AND CXFILE.CX_SEQNO >= :QCX1-WBEG-SEQNO
              AND CXFILE.CX_SEQNO <= :QCX1-WEND-SEQNO
            ORDER BY CXFILE.CX_BRNO,
                     CXFILE.CX_TYPE,
                     CXFILE.CX_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN CX1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QCX1-WBEG-BRNO 
                          QCX1-WEND-BRNO
                          QCX1-WBEG-SEQNO
                          QCX1-WEND-SEQNO.
           MOVE SPACES TO QCX1-WBEG-TYPE
                          QCX1-WEND-TYPE.

           MOVE STAT---GOOD TO CX1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-CX1-FILE.
           PERFORM READ-IT.
           MOVE CX-PATH-OWNBR  TO CX-BRNO. 
           MOVE CX-SELECT-PATH TO E-FILE.
           MOVE CX1-KEY        TO E-KEYX.

           MOVE CX-BRNO   TO QCX-BRNO.
           MOVE CX-TYPE   TO QCX-TYPE.
           MOVE CX-SEQNO  TO QCX-SEQNO.

           EXEC SQL
            SELECT CXFILE.CX_BRNO,
                   ISNULL(CXFILE.CX_TYPE,''),
                   CXFILE.CX_SEQNO,
                   ISNULL(CXFILE.CX_PAYEE,''),
                   ISNULL(CAST(CXFILE.CX_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CXFILE.CX_ADR1,''),
                   ISNULL(CXFILE.CX_ADR2,''),
                   ISNULL(CXFILE.CX_CITY,''),
                   ISNULL(CXFILE.CX_STATE,''),
                   ISNULL(CXFILE.CX_ZIP,''),
                   CXFILE.CX_AMT,
                   CXFILE.CX_REF,
                   ISNULL(CXFILE.CX_REASON,''),
                   CXFILE.CX_GLNO_1,
                   CXFILE.CX_GLAMT_1,
                   CXFILE.CX_GLNO_2,
                   CXFILE.CX_GLAMT_2,
                   CXFILE.CX_GLNO_3,
                   CXFILE.CX_GLAMT_3,
                   CXFILE.CX_GLNO_4,
                   CXFILE.CX_GLAMT_4,
                   CXFILE.CX_GLNO_5,
                   CXFILE.CX_GLAMT_5,
                   ISNULL(CXFILE.CX_FREE1,''),
                   ISNULL(CXFILE.CX_FREE2,''),
                   ISNULL(CXFILE.CX_VOID,''),
                   CXFILE.CX_CLASS,
                   CXFILE.CX_CKNO,
                   CXFILE.CX_DLNO,
                   ISNULL(CAST(CXFILE.CX_DATE AS VARCHAR(10)),'0'),
                   ISNULL(CXFILE.CX_PAID,''),
                   ISNULL(CXFILE.CX_PGM,''),
                   CXFILE.CX_BATCHID,
                   ISNULL(CXFILE.CX_BWPROC,''),
                   CXFILE.CX_CKNO_NEW,
                   CXFILE.CX_DLRVNO,
                   CXFILE.CX_DT_SEQ,
                   ISNULL(CXFILE.CX_RESCISSION,''),
                   CXFILE.CX_TILLNO,
                   CXFILE.CX_ACCTNO,
                   ISNULL(CXFILE.CX_SERVACCT,''),
                   ISNULL(CAST(CXFILE.CX_DT_DATE AS VARCHAR(10)),'0'),
                   CXFILE.CX_SSNO,
                   ISNULL(CXFILE.CX_DT_PAID,''),
                   ISNULL(CXFILE.CX_DT_UNPAID,''),
                   ISNULL(CXFILE.CX_CKPAID_NEW,''),
                   ISNULL(CXFILE.CX_EVENT_CD,''),
                   CXFILE.CX_PAYEE_NO,
                   ISNULL(CXFILE.CX_DEPOSIT_WITHDR,'')
            INTO :QCX-REC
            FROM DBO.CXFILE
            WHERE CXFILE.CX_BRNO = :QCX-BRNO
              AND CXFILE.CX_TYPE = :QCX-TYPE
              AND CXFILE.CX_SEQNO = :QCX-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CX1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-CX-FIELDS.

      *-----------------------------------------------------------------
       READ-CX1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE CX-PATH-OWNBR  TO CX-BRNO.
           MOVE CX-SELECT-PATH TO E-FILE.
           MOVE CX1-KEY        TO E-KEYX.
           INITIALIZE QCX-REC.

           IF ( CX1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH CX1_CURSOR INTO :QCX-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CX1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-CX-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-CX1-FILE.

           IF ( CX1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE CX1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO CX1-CURSOR-STAT.

      *-----------------------------------------------------------------
       LOAD-CX2-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CX-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CX-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO CX-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CX-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO CX-ALL-DATA
              MOVE CX-ALL-PATH             TO CX-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CX-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO CX-ALL-BASE
              MOVE FILPATH-MACHINE         TO CX-ALL-MACHINE
              MOVE FILPATH-DATA            TO CX-ALL-DATA
              MOVE CX-ALL-PATH             TO CX-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-CXFILE NOT = "S" )
              MOVE CX-PATH-BEG             TO CX-SELECT-PATH-BEG
              MOVE CX-PATH-FILE            TO CX-SELECT-PATH-FILE
           ELSE
              MOVE CX-PATH-SQL             TO CX-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-CX2-FILE.
           PERFORM LOAD-CX2-FILE.
           PERFORM OPEN-IT.
           MOVE CX-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-CX2-FILE.
           PERFORM START-IT.
           MOVE CX-PATH-OWNBR  TO CX-BRNO.
           MOVE CX-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING CX-BRNO, CX-PAYEE, DELIMITED BY SIZE INTO E-KEYX.

           IF ( CX2-CURSOR-STAT-GOOD )
              PERFORM CLOSE-CX2-FILE.

           IF ( QCX2-WEND-BRNO = ZEROES    ) OR
              ( QCX2-WEND-BRNO NOT NUMERIC )
              MOVE CX-PATH-OWNBR        TO QCX2-WBEG-BRNO
              MOVE CX-PATH-OWNBR        TO QCX2-WEND-BRNO
              MOVE CX-PAYEE             TO QCX2-WBEG-PAYEE
              MOVE ALL "Z"              TO QCX2-WEND-PAYEE.

           EXEC SQL
            DECLARE CX2_CURSOR CURSOR FOR
            SELECT CXFILE.CX_BRNO,
                   ISNULL(CXFILE.CX_TYPE,''),
                   CXFILE.CX_SEQNO,
                   ISNULL(CXFILE.CX_PAYEE,''),
                   ISNULL(CAST(CXFILE.CX_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CXFILE.CX_ADR1,''),
                   ISNULL(CXFILE.CX_ADR2,''),
                   ISNULL(CXFILE.CX_CITY,''),
                   ISNULL(CXFILE.CX_STATE,''),
                   ISNULL(CXFILE.CX_ZIP,''),
                   CXFILE.CX_AMT,
                   CXFILE.CX_REF,
                   ISNULL(CXFILE.CX_REASON,''),
                   CXFILE.CX_GLNO_1,
                   CXFILE.CX_GLAMT_1,
                   CXFILE.CX_GLNO_2,
                   CXFILE.CX_GLAMT_2,
                   CXFILE.CX_GLNO_3,
                   CXFILE.CX_GLAMT_3,
                   CXFILE.CX_GLNO_4,
                   CXFILE.CX_GLAMT_4,
                   CXFILE.CX_GLNO_5,
                   CXFILE.CX_GLAMT_5,
                   ISNULL(CXFILE.CX_FREE1,''),
                   ISNULL(CXFILE.CX_FREE2,''),
                   ISNULL(CXFILE.CX_VOID,''),
                   CXFILE.CX_CLASS,
                   CXFILE.CX_CKNO,
                   CXFILE.CX_DLNO,
                   ISNULL(CAST(CXFILE.CX_DATE AS VARCHAR(10)),'0'),
                   ISNULL(CXFILE.CX_PAID,''),
                   ISNULL(CXFILE.CX_PGM,''),
                   CXFILE.CX_BATCHID,
                   ISNULL(CXFILE.CX_BWPROC,''),
                   CXFILE.CX_CKNO_NEW,
                   CXFILE.CX_DLRVNO,
                   CXFILE.CX_DT_SEQ,
                   ISNULL(CXFILE.CX_RESCISSION,''),
                   CXFILE.CX_TILLNO,
                   CXFILE.CX_ACCTNO,
                   ISNULL(CXFILE.CX_SERVACCT,''),
                   ISNULL(CAST(CXFILE.CX_DT_DATE AS VARCHAR(10)),'0'),
                   CXFILE.CX_SSNO,
                   ISNULL(CXFILE.CX_DT_PAID,''),
                   ISNULL(CXFILE.CX_DT_UNPAID,''),
                   ISNULL(CXFILE.CX_CKPAID_NEW,''),
                   ISNULL(CXFILE.CX_EVENT_CD,''),
                   CXFILE.CX_PAYEE_NO,
                   ISNULL(CXFILE.CX_DEPOSIT_WITHDR,'')
            FROM DBO.CXFILE
            WHERE CXFILE.CX_BRNO >= :QCX2-WBEG-BRNO
              AND CXFILE.CX_BRNO <= :QCX2-WEND-BRNO
              AND CXFILE.CX_PAYEE >= :QCX2-WBEG-PAYEE
              AND CXFILE.CX_PAYEE <= :QCX2-WEND-PAYEE
            ORDER BY CXFILE.CX_BRNO,
                     CXFILE.CX_PAYEE,
                     CXFILE.CX_TYPE,
                     CXFILE.CX_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN CX2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QCX2-WBEG-BRNO 
                          QCX2-WEND-BRNO
           MOVE SPACES TO QCX2-WBEG-PAYEE
                          QCX2-WEND-PAYEE.

           MOVE STAT---GOOD TO CX2-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-CX2-FILE.
           PERFORM READ-IT.
           MOVE CX-PATH-OWNBR  TO CX-BRNO. 
           MOVE CX-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING CX-BRNO, CX-PAYEE, DELIMITED BY SIZE INTO E-KEYX.

           MOVE CX-BRNO   TO QCX-BRNO.
           MOVE CX-PAYEE  TO QCX-PAYEE.

           EXEC SQL
            SELECT CXFILE.CX_BRNO,
                   ISNULL(CXFILE.CX_TYPE,''),
                   CXFILE.CX_SEQNO,
                   ISNULL(CXFILE.CX_PAYEE,''),
                   ISNULL(CAST(CXFILE.CX_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CXFILE.CX_ADR1,''),
                   ISNULL(CXFILE.CX_ADR2,''),
                   ISNULL(CXFILE.CX_CITY,''),
                   ISNULL(CXFILE.CX_STATE,''),
                   ISNULL(CXFILE.CX_ZIP,''),
                   CXFILE.CX_AMT,
                   CXFILE.CX_REF,
                   ISNULL(CXFILE.CX_REASON,''),
                   CXFILE.CX_GLNO_1,
                   CXFILE.CX_GLAMT_1,
                   CXFILE.CX_GLNO_2,
                   CXFILE.CX_GLAMT_2,
                   CXFILE.CX_GLNO_3,
                   CXFILE.CX_GLAMT_3,
                   CXFILE.CX_GLNO_4,
                   CXFILE.CX_GLAMT_4,
                   CXFILE.CX_GLNO_5,
                   CXFILE.CX_GLAMT_5,
                   ISNULL(CXFILE.CX_FREE1,''),
                   ISNULL(CXFILE.CX_FREE2,''),
                   ISNULL(CXFILE.CX_VOID,''),
                   CXFILE.CX_CLASS,
                   CXFILE.CX_CKNO,
                   CXFILE.CX_DLNO,
                   ISNULL(CAST(CXFILE.CX_DATE AS VARCHAR(10)),'0'),
                   ISNULL(CXFILE.CX_PAID,''),
                   ISNULL(CXFILE.CX_PGM,''),
                   CXFILE.CX_BATCHID,
                   ISNULL(CXFILE.CX_BWPROC,''),
                   CXFILE.CX_CKNO_NEW,
                   CXFILE.CX_DLRVNO,
                   CXFILE.CX_DT_SEQ,
                   ISNULL(CXFILE.CX_RESCISSION,''),
                   CXFILE.CX_TILLNO,
                   CXFILE.CX_ACCTNO,
                   ISNULL(CXFILE.CX_SERVACCT,''),
                   ISNULL(CAST(CXFILE.CX_DT_DATE AS VARCHAR(10)),'0'),
                   CXFILE.CX_SSNO,
                   ISNULL(CXFILE.CX_DT_PAID,''),
                   ISNULL(CXFILE.CX_DT_UNPAID,''),
                   ISNULL(CXFILE.CX_CKPAID_NEW,''),
                   ISNULL(CXFILE.CX_EVENT_CD,''),
                   CXFILE.CX_PAYEE_NO,
                   ISNULL(CXFILE.CX_DEPOSIT_WITHDR,'')
            INTO :QCX-REC
            FROM DBO.CXFILE
            WHERE CXFILE.CX_BRNO = :QCX-BRNO
              AND CXFILE.CX_PAYEE = :QCX-PAYEE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CX2-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-CX-FIELDS.

      *-----------------------------------------------------------------
       READ-CX2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE CX-PATH-OWNBR  TO CX-BRNO.
           MOVE CX-SELECT-PATH TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING CX-BRNO, CX-PAYEE, DELIMITED BY SIZE INTO E-KEYX.
           INITIALIZE QCX-REC.

           IF ( CX2-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH CX2_CURSOR INTO :QCX-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CX2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-CX-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-CX2-FILE.

           IF ( CX2-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE CX2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO CX2-CURSOR-STAT.

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
              DESTROY IOCXRN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
