       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOLTII.
       AUTHOR.        AMM.
       DATE-WRITTEN.  05-12-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOLTII.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : LTIFILE 
      * TABLE DESC    : LOAN TRAILER INSURANCE FILE
      * OUTPUT FILE   : XPC/IOLTII.PRE
      * USED FOR      : LIBLP/LPWSLTI.CPY  --- QLTI-REC
      *                 LIBLP/LPLTI1R.CPY  --- READ
      *                 LIBLP/LPLTI1RN.CPY --- READ/READ-NEXT/START
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
           "@(#)IO/IOLTII.CBL S34 05/12/21-12:38:09 >".

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
       COPY "LIBLP/LP01LTI.CPY".
       COPY "LIBLP/LPWSLTI.CPY".

      *=================================================================
      *
      *    Q - F I L E
      *
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QLTI1-WHERE-BEG-KEY.
           03  QLTI1-WBEG-BRNO             PIC 9(04).
           03  QLTI1-WBEG-ACCTNO           PIC 9(08).
           03  QLTI1-WBEG-SEQNO            PIC 9(03).

       01  QLTI1-WHERE-END-KEY.
           03  QLTI1-WEND-BRNO             PIC 9(04).
           03  QLTI1-WEND-ACCTNO           PIC 9(08).
           03  QLTI1-WEND-SEQNO            PIC 9(03).

       01  QLTI-REC.
           03  QLTI-BRNO           PIC 9(4).
           03  QLTI-ACCTNO         PIC 9(8).
           03  QLTI-SEQNO          PIC 999.
           03  QLTI-LTOUCH-DATE    PIC X(10).
           03  QLTI-INS-TYPE           PIC 99.
           03  QLTI-ORIG-1STPYDATE     PIC X(10).
           03  QLTI-PRIOR-NODEF        PIC 9(3).
           03  QLTI-INSFEECD-1         PIC XX.
           03  QLTI-INSFEE-1           PIC S9(7)V99.
           03  QLTI-INSFEECD-2         PIC XX.
           03  QLTI-INSFEE-2           PIC S9(7)V99.
           03  QLTI-TRCD               PIC X(2).
           03  QLTI-INSCOMP            PIC 99.
           03  QLTI-INSOURS            PIC X.
           03  QLTI-INSEFF-DATE        PIC X(10).
           03  QLTI-INSEXP-DATE        PIC X(10).
           03  QLTI-INSPREM            PIC S9(7)V99.
           03  QLTI-INSCOVR            PIC S9(7)V99.
           03  QLTI-INSCOMM            PIC S9(7)V99.
           03  QLTI-ANTICTERM          PIC 9(3).
           03  QLTI-ANTICERN-1       PIC S9(7)V99.
           03  QLTI-ANTICADJ-1       PIC S9(7)V99.
           03  QLTI-UFERN-1          PIC S9(7)V99.
           03  QLTI-ACCERN-1         PIC S9(7)V99.
           03  QLTI-ANTICERN-2       PIC S9(7)V99.
           03  QLTI-ANTICADJ-2       PIC S9(7)V99.
           03  QLTI-UFERN-2          PIC S9(7)V99.
           03  QLTI-ACCERN-2         PIC S9(7)V99.
           03  QLTI-ANTICERN-3       PIC S9(7)V99.
           03  QLTI-ANTICADJ-3       PIC S9(7)V99.
           03  QLTI-UFERN-3          PIC S9(7)V99.
           03  QLTI-ACCERN-3         PIC S9(7)V99.
           03  QLTI-INT-REBATE         PIC X.
           03  QLTI-INS-PAYMNT-INCR    PIC S9(7)V99.
           03  QLTI-INS-TOT-ADDON      PIC S9(7)V99.
           03  QLTI-INT-LAST-EARNED    PIC S9(7)V99.
           03  QLTI-INS-REBATE         PIC X.
           03  QLTI-RB-WORKER          PIC S9(7)V99.
           03  QLTI-ANTIC-START-DATE   PIC X(10).
           03  QLTI-CANCEL-REASON-CD   PIC X(3).

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
           03  FILLER PIC X(29) VALUE "BRNO   ACCTNO     SEQ  TYPE  ".
           03  FILLER PIC X(30) VALUE "TR     INSPREM".

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-LTI-BRNO                   PIC 9(04).
           03  FILLER                         PIC X(03).
           03  D01-LTI-ACCTNO                 PIC 9(08).
           03  FILLER                         PIC X(03).
           03  D01-LTI-SEQNO                  PIC 9(03).
           03  FILLER                         PIC X(03).
           03  D01-LTI-INS-TYPE               PIC 99.
           03  FILLER                         PIC X(03).
           03  D01-LTI-TRCD                   PIC XX.
           03  FILLER                         PIC X(03).
           03  D01-LTI-INSPREM                PIC Z(6)9.99-.
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
       COPY "LIBIO/IOLTII_DEF.CPY".
       COPY "LIBIO/IOLTII_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOLTII_SCN.CPY".

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
           MOVE "IOLTII"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOLTII-QUEUE-POS  TO Q-POS.
           MOVE IOLTII-COPIES-POS TO C-POS.
           MOVE IOLTII-PRU-POS    TO P-POS.
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
              GO TO ENTER-END-SEQNO.

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

           GO TO ENTER-ACCEPT-YN.

      *-----------------------------------------------------------------
       ENTER-ALL-SEQNO.

           MOVE "SNNN030BEG-SEQNO." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-SEQNO.
           PERFORM SCREENIO.

           MOVE "SNNN030END-SEQNO." TO VDU.
           MOVE 999                 TO VDUNUM0  END-SEQNO.
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

           MOVE "SNNN030BEG-SEQNO."       TO VDU.
           MOVE BEG-SEQNO                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN030END-SEQNO."       TO VDU.
           MOVE END-SEQNO                 TO VDUNUM0.
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

           MOVE "B"                        TO LTI-PATH-OVERRIDE.

           MOVE BR-NO     TO QLTI1-WBEG-BRNO.
           MOVE BR-NO     TO QLTI1-WEND-BRNO.

           MOVE BEG-ACCTNO TO QLTI1-WBEG-ACCTNO.
           MOVE END-ACCTNO TO QLTI1-WEND-ACCTNO.
           MOVE BEG-SEQNO  TO QLTI1-WBEG-SEQNO.
           MOVE END-SEQNO  TO QLTI1-WEND-SEQNO.

      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           PERFORM OPEN-LTI1-FILE.
           PERFORM START-LTI1-FILE.

      *=================================================================
       PROCESS-BRANCH-NEXT.

           PERFORM READ-LTI1-FILE-NEXT.
           IF ( IO-BAD ) OR ( LTI-BRNO NOT = LTI-PATH-OWNBR )
              GO TO PROCESS-BRANCH-MODULE-END.

           ADD 01 TO RECORDS-READ.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

           IF ( LTI-ACCTNO < BEG-ACCTNO )
              GO TO PROCESS-BRANCH-NEXT
           ELSE
           IF ( LTI-ACCTNO > END-ACCTNO )
              GO TO PROCESS-BRANCH-MODULE-END.

           IF LTI-SEQNO < BEG-SEQNO OR > END-SEQNO
              GO TO PROCESS-BRANCH-NEXT.

      *-----------------------------------------------------------------
       PROCESS-BRANCH-PRINT.

           MOVE LTI-BRNO                TO D01-LTI-BRNO.
           MOVE LTI-ACCTNO              TO D01-LTI-ACCTNO.
           MOVE LTI-SEQNO               TO D01-LTI-SEQNO.
           MOVE LTI-INS-TYPE            TO D01-LTI-INS-TYPE.
           MOVE LTI-TRCD                TO D01-LTI-TRCD.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.
           GO TO PROCESS-BRANCH-NEXT.

      *=================================================================
       PROCESS-BRANCH-MODULE-END.

           PERFORM CLOSE-LTI1-FILE.

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

           MOVE LTI1-KEY TO PACE-DETAIL-KEY.

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
           MOVE "SQL TABLE IO UTILITY - LTIFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOLTII-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOLTII-SYSDATE.
           DISPLAY IOLTII-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOLTII-FORM-FIELDS TO WR-BUF.
           MOVE IOLTII-FORM-LIST TO WR-LIST.
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

           MOVE IOLTII-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOLTII-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOLTII_EVA.CPY".
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
      *    LOAD LTI-REC FROM QLTI-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-LTI-FIELDS.
           MOVE QLTI-BRNO TO LTI-BRNO.
           MOVE QLTI-ACCTNO TO LTI-ACCTNO.
           MOVE QLTI-SEQNO TO LTI-SEQNO.

           IF ( QLTI-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO LTI-LTOUCH-DATE
           ELSE
              MOVE QLTI-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LTI-LTOUCH-DATE.

           MOVE QLTI-INS-TYPE TO LTI-INS-TYPE.

           IF ( QLTI-ORIG-1STPYDATE = "1900-01-01" )
              MOVE ZEROES TO LTI-ORIG-1STPYDATE
           ELSE
              MOVE QLTI-ORIG-1STPYDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LTI-ORIG-1STPYDATE.

           MOVE QLTI-PRIOR-NODEF TO LTI-PRIOR-NODEF.
           MOVE QLTI-INSFEECD-1 TO LTI-INSFEECD(1).
           MOVE QLTI-INSFEE-1 TO LTI-INSFEE(1).
           MOVE QLTI-INSFEECD-2 TO LTI-INSFEECD(2).
           MOVE QLTI-INSFEE-2 TO LTI-INSFEE(2).
           MOVE QLTI-TRCD TO LTI-TRCD.
           MOVE QLTI-INSCOMP TO LTI-INSCOMP.
           MOVE QLTI-INSOURS TO LTI-INSOURS.

           IF ( QLTI-INSEFF-DATE = "1900-01-01" )
              MOVE ZEROES TO LTI-INSEFF-DATE
           ELSE
              MOVE QLTI-INSEFF-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LTI-INSEFF-DATE.


           IF ( QLTI-INSEXP-DATE = "1900-01-01" )
              MOVE ZEROES TO LTI-INSEXP-DATE
           ELSE
              MOVE QLTI-INSEXP-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LTI-INSEXP-DATE.

           MOVE QLTI-INSPREM TO LTI-INSPREM.
           MOVE QLTI-INSCOVR TO LTI-INSCOVR.
           MOVE QLTI-INSCOMM TO LTI-INSCOMM.
           MOVE QLTI-ANTICTERM TO LTI-ANTICTERM.
           MOVE QLTI-ANTICERN-1 TO LTI-ANTICERN(1).
           MOVE QLTI-ANTICADJ-1 TO LTI-ANTICADJ(1).
           MOVE QLTI-UFERN-1 TO LTI-UFERN(1).
           MOVE QLTI-ACCERN-1 TO LTI-ACCERN(1).
           MOVE QLTI-ANTICERN-2 TO LTI-ANTICERN(2).
           MOVE QLTI-ANTICADJ-2 TO LTI-ANTICADJ(2).
           MOVE QLTI-UFERN-2 TO LTI-UFERN(2).
           MOVE QLTI-ACCERN-2 TO LTI-ACCERN(2).
           MOVE QLTI-ANTICERN-3 TO LTI-ANTICERN(3).
           MOVE QLTI-ANTICADJ-3 TO LTI-ANTICADJ(3).
           MOVE QLTI-UFERN-3 TO LTI-UFERN(3).
           MOVE QLTI-ACCERN-3 TO LTI-ACCERN(3).
           MOVE QLTI-INT-REBATE TO LTI-INT-REBATE.
           MOVE QLTI-INS-PAYMNT-INCR TO LTI-INS-PAYMNT-INCR.
           MOVE QLTI-INS-TOT-ADDON TO LTI-INS-TOT-ADDON.
           MOVE QLTI-INT-LAST-EARNED TO LTI-INT-LAST-EARNED.
           MOVE QLTI-INS-REBATE TO LTI-INS-REBATE.
           MOVE QLTI-RB-WORKER TO LTI-RB-WORKER.

           IF ( QLTI-ANTIC-START-DATE = "1900-01-01" )
              MOVE ZEROES TO LTI-ANTIC-START-DATE
           ELSE
              MOVE QLTI-ANTIC-START-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO LTI-ANTIC-START-DATE.

           MOVE QLTI-CANCEL-REASON-CD TO LTI-CANCEL-REASON-CD.
 
      *-----------------------------------------------------------------
      *    LOAD QLTI-REC FROM LTI-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-LTI-FIELDS.
           MOVE LTI-BRNO TO QLTI-BRNO.
           MOVE LTI-ACCTNO TO QLTI-ACCTNO.
           MOVE LTI-SEQNO TO QLTI-SEQNO.

           IF ( LTI-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QLTI-LTOUCH-DATE
           ELSE
              MOVE LTI-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLTI-LTOUCH-DATE.

           MOVE LTI-INS-TYPE TO QLTI-INS-TYPE.

           IF ( LTI-ORIG-1STPYDATE = ZEROES )
              MOVE "1900-01-01" TO QLTI-ORIG-1STPYDATE
           ELSE
              MOVE LTI-ORIG-1STPYDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLTI-ORIG-1STPYDATE.

           MOVE LTI-PRIOR-NODEF TO QLTI-PRIOR-NODEF.
           MOVE LTI-INSFEECD(1) TO QLTI-INSFEECD-1.
           MOVE LTI-INSFEE(1) TO QLTI-INSFEE-1.
           MOVE LTI-INSFEECD(2) TO QLTI-INSFEECD-2.
           MOVE LTI-INSFEE(2) TO QLTI-INSFEE-2.
           MOVE LTI-TRCD TO QLTI-TRCD.
           MOVE LTI-INSCOMP TO QLTI-INSCOMP.
           MOVE LTI-INSOURS TO QLTI-INSOURS.

           IF ( LTI-INSEFF-DATE = ZEROES )
              MOVE "1900-01-01" TO QLTI-INSEFF-DATE
           ELSE
              MOVE LTI-INSEFF-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLTI-INSEFF-DATE.


           IF ( LTI-INSEXP-DATE = ZEROES )
              MOVE "1900-01-01" TO QLTI-INSEXP-DATE
           ELSE
              MOVE LTI-INSEXP-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLTI-INSEXP-DATE.

           MOVE LTI-INSPREM TO QLTI-INSPREM.
           MOVE LTI-INSCOVR TO QLTI-INSCOVR.
           MOVE LTI-INSCOMM TO QLTI-INSCOMM.
           MOVE LTI-ANTICTERM TO QLTI-ANTICTERM.
           MOVE LTI-ANTICERN(1) TO QLTI-ANTICERN-1.
           MOVE LTI-ANTICADJ(1) TO QLTI-ANTICADJ-1.
           MOVE LTI-UFERN(1) TO QLTI-UFERN-1.
           MOVE LTI-ACCERN(1) TO QLTI-ACCERN-1.
           MOVE LTI-ANTICERN(2) TO QLTI-ANTICERN-2.
           MOVE LTI-ANTICADJ(2) TO QLTI-ANTICADJ-2.
           MOVE LTI-UFERN(2) TO QLTI-UFERN-2.
           MOVE LTI-ACCERN(2) TO QLTI-ACCERN-2.
           MOVE LTI-ANTICERN(3) TO QLTI-ANTICERN-3.
           MOVE LTI-ANTICADJ(3) TO QLTI-ANTICADJ-3.
           MOVE LTI-UFERN(3) TO QLTI-UFERN-3.
           MOVE LTI-ACCERN(3) TO QLTI-ACCERN-3.
           MOVE LTI-INT-REBATE TO QLTI-INT-REBATE.
           MOVE LTI-INS-PAYMNT-INCR TO QLTI-INS-PAYMNT-INCR.
           MOVE LTI-INS-TOT-ADDON TO QLTI-INS-TOT-ADDON.
           MOVE LTI-INT-LAST-EARNED TO QLTI-INT-LAST-EARNED.
           MOVE LTI-INS-REBATE TO QLTI-INS-REBATE.
           MOVE LTI-RB-WORKER TO QLTI-RB-WORKER.

           IF ( LTI-ANTIC-START-DATE = ZEROES )
              MOVE "1900-01-01" TO QLTI-ANTIC-START-DATE
           ELSE
              MOVE LTI-ANTIC-START-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QLTI-ANTIC-START-DATE.

           MOVE LTI-CANCEL-REASON-CD TO QLTI-CANCEL-REASON-CD.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-LTI1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LTI-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LTI-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LTI-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LTI-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LTI-ALL-DATA
              MOVE LTI-ALL-PATH             TO LTI-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LTI-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LTI-ALL-BASE
              MOVE FILPATH-MACHINE         TO LTI-ALL-MACHINE
              MOVE FILPATH-DATA            TO LTI-ALL-DATA
              MOVE LTI-ALL-PATH             TO LTI-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-LTIFILE NOT = "S" )
              MOVE LTI-PATH-BEG             TO LTI-SELECT-PATH-BEG
              MOVE LTI-PATH-FILE            TO LTI-SELECT-PATH-FILE
           ELSE
              MOVE LTI-PATH-SQL             TO LTI-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-LTI1-FILE.
           PERFORM LOAD-LTI1-FILE.
           PERFORM OPEN-IT.
           MOVE LTI-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-LTI1-FILE.
           PERFORM START-IT.
           MOVE LTI-PATH-OWNBR  TO LTI-BRNO.
           MOVE LTI-SELECT-PATH TO E-FILE.
           MOVE LTI1-KEY        TO E-KEYX.

           IF ( LTI1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LTI1-FILE.

           IF ( QLTI1-WEND-BRNO = ZEROES    ) OR
              ( QLTI1-WEND-BRNO NOT NUMERIC )
              MOVE LTI-PATH-OWNBR        TO QLTI1-WBEG-BRNO
              MOVE LTI-PATH-OWNBR        TO QLTI1-WEND-BRNO
              MOVE LTI-ACCTNO            TO QLTI1-WBEG-ACCTNO
              MOVE ALL "9"               TO QLTI1-WEND-ACCTNO
              MOVE LTI-SEQNO             TO QLTI1-WBEG-SEQNO
              MOVE ALL "9"               TO QLTI1-WEND-SEQNO.


           EXEC SQL
            DECLARE LTI1_CURSOR CURSOR FOR
            SELECT LTIFILE.LTI_BRNO,
                   LTIFILE.LTI_ACCTNO,
                   LTIFILE.LTI_SEQNO,
                   ISNULL(CAST(LTIFILE.LTI_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LTIFILE.LTI_INS_TYPE,
                   ISNULL(CAST(LTIFILE.LTI_ORIG_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   LTIFILE.LTI_PRIOR_NODEF,
                   ISNULL(LTIFILE.LTI_INSFEECD_1,''),
                   LTIFILE.LTI_INSFEE_1,
                   ISNULL(LTIFILE.LTI_INSFEECD_2,''),
                   LTIFILE.LTI_INSFEE_2,
                   ISNULL(LTIFILE.LTI_TRCD,''),
                   LTIFILE.LTI_INSCOMP,
                   ISNULL(LTIFILE.LTI_INSOURS,''),
                   ISNULL(CAST(LTIFILE.LTI_INSEFF_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LTIFILE.LTI_INSEXP_DATE
                                                  AS VARCHAR(10)),'0'),
                   LTIFILE.LTI_INSPREM,
                   LTIFILE.LTI_INSCOVR,
                   LTIFILE.LTI_INSCOMM,
                   LTIFILE.LTI_ANTICTERM,
                   LTIFILE.LTI_ANTICERN_1,
                   LTIFILE.LTI_ANTICADJ_1,
                   LTIFILE.LTI_UFERN_1,
                   LTIFILE.LTI_ACCERN_1,
                   LTIFILE.LTI_ANTICERN_2,
                   LTIFILE.LTI_ANTICADJ_2,
                   LTIFILE.LTI_UFERN_2,
                   LTIFILE.LTI_ACCERN_2,
                   LTIFILE.LTI_ANTICERN_3,
                   LTIFILE.LTI_ANTICADJ_3,
                   LTIFILE.LTI_UFERN_3,
                   LTIFILE.LTI_ACCERN_3,
                   ISNULL(LTIFILE.LTI_INT_REBATE,''),
                   LTIFILE.LTI_INS_PAYMNT_INCR,
                   LTIFILE.LTI_INS_TOT_ADDON,
                   LTIFILE.LTI_INT_LAST_EARNED,
                   ISNULL(LTIFILE.LTI_INS_REBATE,''),
                   LTIFILE.LTI_RB_WORKER,
                   ISNULL(CAST(LTIFILE.LTI_ANTIC_START_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LTIFILE.LTI_CANCEL_REASON_CD,'')
            FROM DBO.LTIFILE
            WHERE LTIFILE.LTI_BRNO >= :QLTI1-WBEG-BRNO
              AND LTIFILE.LTI_BRNO <= :QLTI1-WEND-BRNO
              AND LTIFILE.LTI_ACCTNO >= :QLTI1-WBEG-ACCTNO
              AND LTIFILE.LTI_ACCTNO <= :QLTI1-WEND-ACCTNO
              AND LTIFILE.LTI_SEQNO >= :QLTI1-WBEG-SEQNO
              AND LTIFILE.LTI_SEQNO <= :QLTI1-WEND-SEQNO
            ORDER BY LTIFILE.LTI_BRNO,
                     LTIFILE.LTI_ACCTNO,
                     LTIFILE.LTI_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN LTI1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QLTI1-WBEG-BRNO
                          QLTI1-WEND-BRNO
                          QLTI1-WBEG-ACCTNO
                          QLTI1-WEND-ACCTNO
                          QLTI1-WBEG-SEQNO
                          QLTI1-WEND-SEQNO.

           MOVE STAT---GOOD TO LTI1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LTI1-FILE.
           PERFORM READ-IT.
           MOVE LTI-PATH-OWNBR  TO LTI-BRNO. 
           MOVE LTI-SELECT-PATH TO E-FILE.
           MOVE LTI1-KEY        TO E-KEYX.

           MOVE LTI-BRNO   TO QLTI-BRNO.
           MOVE LTI-ACCTNO TO QLTI-ACCTNO.
           MOVE LTI-SEQNO  TO QLTI-SEQNO.

           EXEC SQL
            SELECT LTIFILE.LTI_BRNO,
                   LTIFILE.LTI_ACCTNO,
                   LTIFILE.LTI_SEQNO,
                   ISNULL(CAST(LTIFILE.LTI_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   LTIFILE.LTI_INS_TYPE,
                   ISNULL(CAST(LTIFILE.LTI_ORIG_1STPYDATE
                                                  AS VARCHAR(10)),'0'),
                   LTIFILE.LTI_PRIOR_NODEF,
                   ISNULL(LTIFILE.LTI_INSFEECD_1,''),
                   LTIFILE.LTI_INSFEE_1,
                   ISNULL(LTIFILE.LTI_INSFEECD_2,''),
                   LTIFILE.LTI_INSFEE_2,
                   ISNULL(LTIFILE.LTI_TRCD,''),
                   LTIFILE.LTI_INSCOMP,
                   ISNULL(LTIFILE.LTI_INSOURS,''),
                   ISNULL(CAST(LTIFILE.LTI_INSEFF_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(LTIFILE.LTI_INSEXP_DATE
                                                  AS VARCHAR(10)),'0'),
                   LTIFILE.LTI_INSPREM,
                   LTIFILE.LTI_INSCOVR,
                   LTIFILE.LTI_INSCOMM,
                   LTIFILE.LTI_ANTICTERM,
                   LTIFILE.LTI_ANTICERN_1,
                   LTIFILE.LTI_ANTICADJ_1,
                   LTIFILE.LTI_UFERN_1,
                   LTIFILE.LTI_ACCERN_1,
                   LTIFILE.LTI_ANTICERN_2,
                   LTIFILE.LTI_ANTICADJ_2,
                   LTIFILE.LTI_UFERN_2,
                   LTIFILE.LTI_ACCERN_2,
                   LTIFILE.LTI_ANTICERN_3,
                   LTIFILE.LTI_ANTICADJ_3,
                   LTIFILE.LTI_UFERN_3,
                   LTIFILE.LTI_ACCERN_3,
                   ISNULL(LTIFILE.LTI_INT_REBATE,''),
                   LTIFILE.LTI_INS_PAYMNT_INCR,
                   LTIFILE.LTI_INS_TOT_ADDON,
                   LTIFILE.LTI_INT_LAST_EARNED,
                   ISNULL(LTIFILE.LTI_INS_REBATE,''),
                   LTIFILE.LTI_RB_WORKER,
                   ISNULL(CAST(LTIFILE.LTI_ANTIC_START_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(LTIFILE.LTI_CANCEL_REASON_CD,'')
            INTO :QLTI-REC
            FROM DBO.LTIFILE
            WHERE LTIFILE.LTI_BRNO = :QLTI-BRNO
              AND LTIFILE.LTI_ACCTNO = :QLTI-ACCTNO
              AND LTIFILE.LTI_SEQNO = :QLTI-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTI1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-LTI-FIELDS.

      *-----------------------------------------------------------------
       READ-LTI1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LTI-PATH-OWNBR  TO LTI-BRNO.
           MOVE LTI-SELECT-PATH TO E-FILE.
           MOVE LTI1-KEY        TO E-KEYX.
           INITIALIZE QLTI-REC.

           IF ( LTI1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH LTI1_CURSOR INTO :QLTI-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LTI1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-LTI-FIELDS.

      *-----------------------------------------------------------------
       WRITE-LTI1-FILE.

           MOVE LTI-PATH-OWNBR  TO LTI-BRNO.
           ACCEPT LTI-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE LTI-SELECT-PATH TO E-FILE.
           MOVE LTI1-KEY        TO E-KEYX.
           PERFORM SET-LTI-FIELDS.

           EXEC SQL
            INSERT INTO DBO.LTIFILE
             (LTIFILE.LTI_BRNO,
              LTIFILE.LTI_ACCTNO,
              LTIFILE.LTI_SEQNO,
              LTIFILE.LTI_LTOUCH_DATE,
              LTIFILE.LTI_INS_TYPE,
              LTIFILE.LTI_ORIG_1STPYDATE,
              LTIFILE.LTI_PRIOR_NODEF,
              LTIFILE.LTI_INSFEECD_1,
              LTIFILE.LTI_INSFEE_1,
              LTIFILE.LTI_INSFEECD_2,
              LTIFILE.LTI_INSFEE_2,
              LTIFILE.LTI_TRCD,
              LTIFILE.LTI_INSCOMP,
              LTIFILE.LTI_INSOURS,
              LTIFILE.LTI_INSEFF_DATE,
              LTIFILE.LTI_INSEXP_DATE,
              LTIFILE.LTI_INSPREM,
              LTIFILE.LTI_INSCOVR,
              LTIFILE.LTI_INSCOMM,
              LTIFILE.LTI_ANTICTERM,
              LTIFILE.LTI_ANTICERN_1,
              LTIFILE.LTI_ANTICADJ_1,
              LTIFILE.LTI_UFERN_1,
              LTIFILE.LTI_ACCERN_1,
              LTIFILE.LTI_ANTICERN_2,
              LTIFILE.LTI_ANTICADJ_2,
              LTIFILE.LTI_UFERN_2,
              LTIFILE.LTI_ACCERN_2,
              LTIFILE.LTI_ANTICERN_3,
              LTIFILE.LTI_ANTICADJ_3,
              LTIFILE.LTI_UFERN_3,
              LTIFILE.LTI_ACCERN_3,
              LTIFILE.LTI_INT_REBATE,
              LTIFILE.LTI_INS_PAYMNT_INCR,
              LTIFILE.LTI_INS_TOT_ADDON,
              LTIFILE.LTI_INT_LAST_EARNED,
              LTIFILE.LTI_INS_REBATE,
              LTIFILE.LTI_RB_WORKER,
              LTIFILE.LTI_ANTIC_START_DATE,
              LTIFILE.LTI_CANCEL_REASON_CD)
            VALUES
             (:QLTI-BRNO,
              :QLTI-ACCTNO,
              :QLTI-SEQNO,
              NULLIF(:QLTI-LTOUCH-DATE,'1900-01-01'),
              :QLTI-INS-TYPE,
              NULLIF(:QLTI-ORIG-1STPYDATE,'1900-01-01'),
              :QLTI-PRIOR-NODEF,
              :QLTI-INSFEECD-1,
              :QLTI-INSFEE-1,
              :QLTI-INSFEECD-2,
              :QLTI-INSFEE-2,
              :QLTI-TRCD,
              :QLTI-INSCOMP,
              :QLTI-INSOURS,
              NULLIF(:QLTI-INSEFF-DATE,'1900-01-01'),
              NULLIF(:QLTI-INSEXP-DATE,'1900-01-01'),
              :QLTI-INSPREM,
              :QLTI-INSCOVR,
              :QLTI-INSCOMM,
              :QLTI-ANTICTERM,
              :QLTI-ANTICERN-1,
              :QLTI-ANTICADJ-1,
              :QLTI-UFERN-1,
              :QLTI-ACCERN-1,
              :QLTI-ANTICERN-2,
              :QLTI-ANTICADJ-2,
              :QLTI-UFERN-2,
              :QLTI-ACCERN-2,
              :QLTI-ANTICERN-3,
              :QLTI-ANTICADJ-3,
              :QLTI-UFERN-3,
              :QLTI-ACCERN-3,
              :QLTI-INT-REBATE,
              :QLTI-INS-PAYMNT-INCR,
              :QLTI-INS-TOT-ADDON,
              :QLTI-INT-LAST-EARNED,
              :QLTI-INS-REBATE,
              :QLTI-RB-WORKER,
              NULLIF(:QLTI-ANTIC-START-DATE,'1900-01-01'),
              :QLTI-CANCEL-REASON-CD)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-LTI1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-LTI1-FILE.

           MOVE LTI-PATH-OWNBR  TO LTI-BRNO.
           ACCEPT LTI-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE LTI-SELECT-PATH TO E-FILE.
           MOVE LTI1-KEY        TO E-KEYX.
           PERFORM SET-LTI-FIELDS.

           EXEC SQL
            UPDATE DBO.LTIFILE
             SET
              LTIFILE.LTI_BRNO = :QLTI-BRNO,
              LTIFILE.LTI_ACCTNO = :QLTI-ACCTNO,
              LTIFILE.LTI_SEQNO = :QLTI-SEQNO,
              LTIFILE.LTI_LTOUCH_DATE =
                 NULLIF(:QLTI-LTOUCH-DATE,'1900-01-01'),
              LTIFILE.LTI_INS_TYPE = :QLTI-INS-TYPE,
              LTIFILE.LTI_ORIG_1STPYDATE =
                 NULLIF(:QLTI-ORIG-1STPYDATE,'1900-01-01'),
              LTIFILE.LTI_PRIOR_NODEF = :QLTI-PRIOR-NODEF,
              LTIFILE.LTI_INSFEECD_1 = :QLTI-INSFEECD-1,
              LTIFILE.LTI_INSFEE_1 = :QLTI-INSFEE-1,
              LTIFILE.LTI_INSFEECD_2 = :QLTI-INSFEECD-2,
              LTIFILE.LTI_INSFEE_2 = :QLTI-INSFEE-2,
              LTIFILE.LTI_TRCD = :QLTI-TRCD,
              LTIFILE.LTI_INSCOMP = :QLTI-INSCOMP,
              LTIFILE.LTI_INSOURS = :QLTI-INSOURS,
              LTIFILE.LTI_INSEFF_DATE =
                 NULLIF(:QLTI-INSEFF-DATE,'1900-01-01'),
              LTIFILE.LTI_INSEXP_DATE =
                 NULLIF(:QLTI-INSEXP-DATE,'1900-01-01'),
              LTIFILE.LTI_INSPREM = :QLTI-INSPREM,
              LTIFILE.LTI_INSCOVR = :QLTI-INSCOVR,
              LTIFILE.LTI_INSCOMM = :QLTI-INSCOMM,
              LTIFILE.LTI_ANTICTERM = :QLTI-ANTICTERM,
              LTIFILE.LTI_ANTICERN_1 = :QLTI-ANTICERN-1,
              LTIFILE.LTI_ANTICADJ_1 = :QLTI-ANTICADJ-1,
              LTIFILE.LTI_UFERN_1 = :QLTI-UFERN-1,
              LTIFILE.LTI_ACCERN_1 = :QLTI-ACCERN-1,
              LTIFILE.LTI_ANTICERN_2 = :QLTI-ANTICERN-2,
              LTIFILE.LTI_ANTICADJ_2 = :QLTI-ANTICADJ-2,
              LTIFILE.LTI_UFERN_2 = :QLTI-UFERN-2,
              LTIFILE.LTI_ACCERN_2 = :QLTI-ACCERN-2,
              LTIFILE.LTI_ANTICERN_3 = :QLTI-ANTICERN-3,
              LTIFILE.LTI_ANTICADJ_3 = :QLTI-ANTICADJ-3,
              LTIFILE.LTI_UFERN_3 = :QLTI-UFERN-3,
              LTIFILE.LTI_ACCERN_3 = :QLTI-ACCERN-3,
              LTIFILE.LTI_INT_REBATE = :QLTI-INT-REBATE,
              LTIFILE.LTI_INS_PAYMNT_INCR = :QLTI-INS-PAYMNT-INCR,
              LTIFILE.LTI_INS_TOT_ADDON = :QLTI-INS-TOT-ADDON,
              LTIFILE.LTI_INT_LAST_EARNED = :QLTI-INT-LAST-EARNED,
              LTIFILE.LTI_INS_REBATE = :QLTI-INS-REBATE,
              LTIFILE.LTI_RB_WORKER = :QLTI-RB-WORKER,
              LTIFILE.LTI_ANTIC_START_DATE =
                 NULLIF(:QLTI-ANTIC-START-DATE,'1900-01-01'),
              LTIFILE.LTI_CANCEL_REASON_CD =
                   :QLTI-CANCEL-REASON-CD
             WHERE LTIFILE.LTI_BRNO = :QLTI-BRNO
               AND LTIFILE.LTI_ACCTNO = :QLTI-ACCTNO
               AND LTIFILE.LTI_SEQNO = :QLTI-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-LTI1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-LTI1-FILE.

           MOVE LTI-PATH-OWNBR  TO LTI-BRNO.
           PERFORM DELETE-IT.
           MOVE LTI-SELECT-PATH TO E-FILE.
           MOVE LTI1-KEY        TO E-KEYX.
           PERFORM SET-LTI-FIELDS.

           EXEC SQL
            DELETE FROM DBO.LTIFILE
             WHERE LTIFILE.LTI_BRNO = :QLTI-BRNO
               AND LTIFILE.LTI_ACCTNO = :QLTI-ACCTNO
               AND LTIFILE.LTI_SEQNO = :QLTI-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-LTI1-FILE.

      *-----------------------------------------------------------------
       CLOSE-LTI1-FILE.

           IF ( LTI1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE LTI1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO LTI1-CURSOR-STAT.

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
              DESTROY IOLTII-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
