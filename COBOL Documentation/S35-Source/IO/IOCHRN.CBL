       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOCHRN.
       AUTHOR.        BAH.
       DATE-WRITTEN.  04-16-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOCHRN.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : CHFILE 
      * TABLE DESC    : CHANGES/AUDIT FILE
      * OUTPUT FILE   : XPC/IOCHRN.PRE
      * USED FOR      : LIBLP/LPWSCH.CPY  --- QLP-REC
      *                 LIBLP/LPCHR.CPY  --- READ
      *                 LIBLP/LPCHRN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * BAH 2021-0416 ***NEW***
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
           "@(#)IO/IOCHRN.CBL S34 05/19/21-12:47:50 >".

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
       COPY "LIBLP/LP01CH.CPY".
       COPY "LIBLP/LPWSCH.CPY".

      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S LIBXX/XX01YY_SQL.CPY
      *#################################################################

      *=================================================================
      *
      *    Q - F I L E
      *
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QCH-WHERE-BEG-KEY.
           03  QCH-WBEG-BRNO             PIC 9(04).
           03  QCH-WBEG-DATE             PIC X(10).
           03  QCH-WBEG-CODE             PIC X(03).
           03  QCH-WBEG-USERID           PIC X(10).
           03  QCH-WBEG-TIME             PIC 9(06).
           03  QCH-WBEG-SEQ              PIC 9(03).
 
       01  QCH-WHERE-END-KEY.
           03  QCH-WEND-BRNO             PIC 9(04).
           03  QCH-WEND-DATE             PIC X(10).
           03  QCH-WEND-CODE             PIC X(03).
           03  QCH-WEND-USERID           PIC X(10).
           03  QCH-WEND-TIME             PIC 9(06).
           03  QCH-WEND-SEQ              PIC 9(03).
 
       01  QCH-REC.
           03  QCH-BRNO                            PIC 9(4).
           03  QCH-DATE                            PIC X(10).
           03  QCH-CODE                            PIC X(3).
           03  QCH-USERID                          PIC X(10).
           03  QCH-TIME                            PIC 9(6).
           03  QCH-SEQ                             PIC 9(3).
           03  QCH-LTOUCH-DATE                     PIC X(10).
           03  QCH-DESCRIPT                        PIC X(25).
           03  QCH-ACCTNO                          PIC 9(8).
           03  QCH-SSNO                            PIC 9(9).
           03  QCH-TYPE                            PIC 99.
           03  QCH-ORG-CHAR                        PIC X(50).
           03  QCH-NEW-CHAR                        PIC X(50).

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
           03  BEG-DATE                PIC 9(08).
           03  END-DATE                PIC 9(08).
           03  BEG-CODE                PIC X(03).
           03  END-CODE                PIC X(03).
           03  BEG-USERID              PIC X(10).
           03  END-USERID              PIC X(10).
           03  BEG-TIME                PIC 9(06).
           03  END-TIME                PIC 9(06).
           03  BEG-SEQ                 PIC 9(03).
           03  END-SEQ                 PIC 9(03).

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
           03  FILLER PIC X(30) VALUE "BRNO      DATE     CODE     US".
           03  FILLER PIC X(30) VALUE "ERID      TIME    SEQ     DESC".
           03  FILLER PIC X(16) VALUE SPACES.
           03  FILLER PIC X(06) VALUE "ORIG".
           03  FILLER PIC X(18) VALUE SPACES.
           03  FILLER PIC X(06) VALUE "NEW".

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-CH-BRNO                    PIC 9(04).
           03  FILLER                         PIC X(03).
           03  D01-CH-DATE                    PIC 9999/99/99.
           03  FILLER                         PIC X(03).
           03  D01-CH-CODE                    PIC X(03).
           03  FILLER                         PIC X(03).
           03  D01-CH-USERID                  PIC X(10).
           03  FILLER                         PIC X(03).
           03  D01-CH-TIME                    PIC 9(06).
           03  FILLER                         PIC X(03).
           03  D01-CH-SEQ                     PIC 9(03).
           03  FILLER                         PIC X(03).
           03  D01-CH-DESCRIPT                PIC X(15).
           03  FILLER                         PIC X(03).
           03  D01-CH-ORG                     PIC X(20).
           03  FILLER                         PIC X(03).
           03  D01-CH-NEW                     PIC X(20).

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
       COPY "LIBIO/IOCHRN_DEF.CPY".
       COPY "LIBIO/IOCHRN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOCHRN_SCN.CPY".

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
           MOVE "IOCHRN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOCHRN-QUEUE-POS  TO Q-POS.
           MOVE IOCHRN-COPIES-POS TO C-POS.
           MOVE IOCHRN-PRU-POS    TO P-POS.
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

           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.

      *-----------------------------------------------------------------
       ENTER-END-DATE.

           MOVE "ERNM060END-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-DATE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-DATE.

           MOVE VDU-DATE-YYYYMMDD TO END-DATE.

           IF ( BEG-DATE > END-DATE )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DATE.

           GO TO ENTER-BEG-CODE.

      *-----------------------------------------------------------------
       ENTER-ALL-DATE.

           MOVE "S  8080BEG-DATE." TO VDU.
           MOVE JULIAN-BEG         TO BEG-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "S  8080END-DATE." TO VDU.
           MOVE JULIAN-END         TO END-DATE VDU-DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-CODE.

           MOVE "E  A030BEG-CODE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-DATE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-CODE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-CODE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-CODE.

      *-----------------------------------------------------------------
       ENTER-END-CODE.

           MOVE "E  A030END-CODE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-CODE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-CODE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-CODE.

           IF ( BEG-CODE > END-CODE )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-CODE.

           GO TO ENTER-BEG-USERID.

      *-----------------------------------------------------------------
       ENTER-ALL-CODE.

           MOVE "S  A030BEG-CODE." TO VDU.
           MOVE SPACES              TO VDUBUF  BEG-CODE.
           PERFORM SCREENIO.

           MOVE "S  A030END-CODE." TO VDU.
           MOVE ALL "Z"            TO VDUBUF  END-CODE.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-USERID.

           MOVE "E  A100BEG-USERID." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-CODE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-USERID.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-USERID.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-USERID.

      *-----------------------------------------------------------------
       ENTER-END-USERID.

           MOVE "E  A100END-USERID." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-USERID.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-USERID.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-USERID.

           IF ( BEG-USERID > END-USERID )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-USERID.

           GO TO ENTER-BEG-TIME.

      *-----------------------------------------------------------------
       ENTER-ALL-USERID.

           MOVE "S  A100BEG-USERID." TO VDU.
           MOVE SPACES              TO VDUBUF  BEG-USERID.
           PERFORM SCREENIO.

           MOVE "S  A100END-USERID." TO VDU.
           MOVE ALL "Z"            TO VDUBUF  END-USERID.
      * MUST BE LOWER CASE Z TO GET V0010BAH
           CALL "C$TOLOWER" USING END-USERID, VALUE 10.
           MOVE END-USERID TO VDUBUF.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-TIME.

           MOVE "ENNN060BEG-TIME." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-USERID.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-TIME.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-TIME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-TIME.

      *-----------------------------------------------------------------
       ENTER-END-TIME.

           MOVE "ENNN060END-TIME." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-TIME.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-TIME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-TIME.

           IF ( BEG-TIME > END-TIME )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-TIME.

           GO TO ENTER-BEG-SEQ.

      *-----------------------------------------------------------------
       ENTER-ALL-TIME.

           MOVE "SNNN060BEG-TIME." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-TIME.
           PERFORM SCREENIO.

           MOVE "SNNN060END-TIME." TO VDU.
           MOVE ALL "9"            TO VDUNUM0  END-TIME.
           PERFORM SCREENIO.


      *=================================================================
       ENTER-BEG-SEQ.

           MOVE "ENNN030BEG-SEQ." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-TIME.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-SEQ.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-SEQ.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-SEQ.

      *-----------------------------------------------------------------
       ENTER-END-SEQ.

           MOVE "ENNN030END-SEQ." TO VDU.
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

           GO TO ENTER-BEG-SEQ.

      *-----------------------------------------------------------------
       ENTER-ALL-SEQ.

           MOVE "SNNN030BEG-SEQ." TO VDU.
           MOVE ZEROES              TO VDUNUM0  BEG-SEQ.
           PERFORM SCREENIO.

           MOVE "SNNN030END-SEQ." TO VDU.
           MOVE ALL "9"            TO VDUNUM0  END-SEQ.
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
           MOVE BEG-DATE              TO VDU-DATE-EDIT.
           PERFORM SCREENIO.

           MOVE "S  A000END-DATE."    TO VDU.
           MOVE END-DATE              TO VDU-DATE-EDIT.
           PERFORM SCREENIO.

           MOVE "S  A030BEG-CODE."       TO VDU.
           MOVE BEG-CODE                 TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A030END-CODE."       TO VDU.
           MOVE END-CODE                 TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A100BEG-USERID."       TO VDU.
           MOVE BEG-USERID                 TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A100END-USERID."       TO VDU.
           MOVE END-USERID                 TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SNNN060BEG-TIME."      TO VDU.
           MOVE BEG-TIME                TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060END-TIME."      TO VDU.
           MOVE END-TIME                TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN030BEG-SEQ."      TO VDU.
           MOVE BEG-SEQ                TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN030END-SEQ."      TO VDU.
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

           MOVE "B"                        TO CH-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - -{ SET WHERE BEG & END FIELDS }
           MOVE BR-NO     TO QCH-WBEG-BRNO 
                              QCH-WEND-BRNO.

           MOVE BEG-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QCH-WBEG-DATE.

           MOVE END-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QCH-WEND-DATE.

           MOVE BEG-CODE  TO QCH-WBEG-CODE.
           MOVE END-CODE  TO QCH-WEND-CODE.
           MOVE BEG-USERID  TO QCH-WBEG-USERID.
           MOVE END-USERID  TO QCH-WEND-USERID.
           MOVE BEG-TIME  TO QCH-WBEG-TIME.
           MOVE END-TIME  TO QCH-WEND-TIME.
           MOVE BEG-SEQ  TO QCH-WBEG-SEQ.
           MOVE END-SEQ  TO QCH-WEND-SEQ.


      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           PERFORM OPEN-CH-FILE.
           PERFORM START-CH-FILE.

      *=================================================================
       PROCESS-BRANCH-NEXT.

           PERFORM READ-CH-FILE-NEXT.
           IF ( IO-BAD ) OR ( CH-BRNO NOT = CH-PATH-OWNBR )
              GO TO PROCESS-BRANCH-MODULE-END.

           ADD 01 TO RECORDS-READ.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

           IF ( CH-DATE < BEG-DATE )
               GO TO PROCESS-BRANCH-NEXT
           ELSE
           IF ( CH-DATE > END-DATE )
              GO TO PROCESS-BRANCH-MODULE-END.

           IF CH-CODE < BEG-CODE OR > END-CODE
              GO TO PROCESS-BRANCH-NEXT.
           IF CH-USERID < BEG-USERID OR > END-USERID
              GO TO PROCESS-BRANCH-NEXT.
           IF CH-TIME < BEG-TIME OR > END-TIME
              GO TO PROCESS-BRANCH-NEXT.
           IF CH-SEQ < BEG-SEQ OR > END-SEQ
              GO TO PROCESS-BRANCH-NEXT.


      *-----------------------------------------------------------------
       PROCESS-BRANCH-PRINT.


      *-------------------------------------------------------{ RANGES }
           MOVE CH-BRNO                    TO D01-CH-BRNO.
           MOVE CH-DATE                    TO D01-CH-DATE.
           MOVE CH-CODE                    TO D01-CH-CODE.
           MOVE CH-USERID                  TO D01-CH-USERID.
           MOVE CH-TIME                   TO D01-CH-TIME.
           MOVE CH-SEQ                    TO D01-CH-SEQ.
           MOVE CH-DESCRIPT               TO D01-CH-DESCRIPT.
           MOVE CH-SEQ                    TO D01-CH-SEQ.
           MOVE CH-ORG-CHAR               TO D01-CH-ORG.
           MOVE CH-NEW-CHAR               TO D01-CH-NEW.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.
           GO TO PROCESS-BRANCH-NEXT.

      *=================================================================
       PROCESS-BRANCH-MODULE-END.

           PERFORM CLOSE-CH-FILE.

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

           MOVE CH-KEY TO PACE-DETAIL-KEY.

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
           MOVE BEG-DATE                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    DATE               : "    TO R01-PROMPT.
           MOVE END-DATE                             TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING CODE               : "    TO R01-PROMPT.
           MOVE BEG-CODE                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    CODE               : "    TO R01-PROMPT.
           MOVE END-CODE                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING USERID               : "    TO R01-PROMPT.
           MOVE BEG-USERID                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    USERID               : "    TO R01-PROMPT.
           MOVE END-USERID                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING TIME              : "    TO R01-PROMPT.
           MOVE BEG-TIME                            TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    TIME              : "    TO R01-PROMPT.
           MOVE END-TIME                            TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING SEQ                : "    TO R01-PROMPT.
           MOVE BEG-SEQ                              TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    SEQ                : "    TO R01-PROMPT.
           MOVE END-SEQ                              TO R01-NUMERIC.
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
           MOVE "SQL TABLE IO UTILITY - CHFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOCHRN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOCHRN-SYSDATE.
           DISPLAY IOCHRN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOCHRN-FORM-FIELDS TO WR-BUF.
           MOVE IOCHRN-FORM-LIST TO WR-LIST.
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

           MOVE IOCHRN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOCHRN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOCHRN_EVA.CPY".
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
      *    LOAD CH-REC FROM QCH-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-CH-FIELDS.
           MOVE QCH-BRNO TO CH-BRNO.

           IF ( QCH-DATE = "1900-01-01" )
              MOVE ZEROES TO CH-DATE
           ELSE
              MOVE QCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CH-DATE.

           MOVE QCH-CODE TO CH-CODE.
           MOVE QCH-USERID TO CH-USERID.
           MOVE QCH-TIME TO CH-TIME.
           MOVE QCH-SEQ TO CH-SEQ.

           IF ( QCH-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO CH-LTOUCH-DATE
           ELSE
              MOVE QCH-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CH-LTOUCH-DATE.

           MOVE QCH-DESCRIPT TO CH-DESCRIPT.
           MOVE QCH-ACCTNO TO CH-ACCTNO.
           MOVE QCH-SSNO TO CH-SSNO.
           MOVE QCH-TYPE TO CH-TYPE.
           MOVE QCH-ORG-CHAR TO CH-ORG-CHAR.
           MOVE QCH-NEW-CHAR TO CH-NEW-CHAR.
 
      *-----------------------------------------------------------------
      *    LOAD QCH-REC FROM CH-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-CH-FIELDS.
           MOVE CH-BRNO TO QCH-BRNO.

           IF ( CH-DATE = ZEROES )
              MOVE "1900-01-01" TO QCH-DATE
           ELSE
              MOVE CH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCH-DATE.

           MOVE CH-CODE TO QCH-CODE.
           MOVE CH-USERID TO QCH-USERID.
           MOVE CH-TIME TO QCH-TIME.
           MOVE CH-SEQ TO QCH-SEQ.

           IF ( CH-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QCH-LTOUCH-DATE
           ELSE
              MOVE CH-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCH-LTOUCH-DATE.

           MOVE CH-DESCRIPT TO QCH-DESCRIPT.
           MOVE CH-ACCTNO TO QCH-ACCTNO.
           MOVE CH-SSNO TO QCH-SSNO.
           MOVE CH-TYPE TO QCH-TYPE.
           MOVE CH-ORG-CHAR TO QCH-ORG-CHAR.
           MOVE CH-NEW-CHAR TO QCH-NEW-CHAR.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-CH-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CH-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CH-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO CH-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CH-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO CH-ALL-DATA
              MOVE CH-ALL-PATH             TO CH-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CH-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO CH-ALL-BASE
              MOVE FILPATH-MACHINE         TO CH-ALL-MACHINE
              MOVE FILPATH-DATA            TO CH-ALL-DATA
              MOVE CH-ALL-PATH             TO CH-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-CHFILE NOT = "S" )
              MOVE CH-PATH-BEG             TO CH-SELECT-PATH-BEG
              MOVE CH-PATH-FILE            TO CH-SELECT-PATH-FILE
           ELSE
              MOVE CH-PATH-SQL             TO CH-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-CH-FILE.
           PERFORM LOAD-CH-FILE.
           PERFORM OPEN-IT.
           MOVE CH-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-CH-FILE.
           PERFORM START-IT.
           MOVE CH-PATH-OWNBR  TO CH-BRNO.
           MOVE CH-SELECT-PATH TO E-FILE.
           MOVE CH-KEY         TO E-KEYX.

           IF ( CH-CURSOR-STAT-GOOD )
              PERFORM CLOSE-CH-FILE.

           IF ( QCH-WEND-SEQ = ZEROES    ) OR
              ( QCH-WEND-SEQ NOT NUMERIC )
              MOVE CH-PATH-OWNBR       TO QCH-WBEG-BRNO
              MOVE CH-PATH-OWNBR       TO QCH-WEND-BRNO
              MOVE CH-DATE             TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCH-WBEG-DATE
              MOVE JULIAN-END          TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCH-WEND-DATE
              MOVE CH-CODE             TO QCH-WBEG-CODE
              MOVE ALL "Z"             TO QCH-WEND-CODE
              MOVE CH-USERID           TO QCH-WBEG-USERID
              MOVE ALL "Z"             TO QCH-WEND-USERID
              MOVE CH-TIME             TO QCH-WBEG-TIME
              MOVE ALL "9"             TO QCH-WEND-TIME
              MOVE CH-SEQ              TO QCH-WBEG-SEQ
              MOVE ALL "9"             TO QCH-WEND-SEQ.


           EXEC SQL
            DECLARE CH_CURSOR CURSOR FOR
            SELECT CHFILE.CH_BRNO,
                   ISNULL(CAST(CHFILE.CH_DATE AS VARCHAR(10)),'0'),
                   ISNULL(CHFILE.CH_CODE,''),
                   ISNULL(CHFILE.CH_USERID,''),
                   CHFILE.CH_TIME,
                   CHFILE.CH_SEQ,
                   ISNULL(CAST(CHFILE.CH_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CHFILE.CH_DESCRIPT,''),
                   CHFILE.CH_ACCTNO,
                   CHFILE.CH_SSNO,
                   CHFILE.CH_TYPE,
                   ISNULL(CHFILE.CH_ORG_CHAR,''),
                   ISNULL(CHFILE.CH_NEW_CHAR,'')
            FROM DBO.CHFILE
            WHERE CHFILE.CH_BRNO >= :QCH-WBEG-BRNO
              AND CHFILE.CH_BRNO <= :QCH-WEND-BRNO
              AND CAST(CHFILE.CH_DATE AS VARCHAR(10))
                          >= :QCH-WBEG-DATE
              AND CAST(CHFILE.CH_DATE AS VARCHAR(10))
                          <= :QCH-WEND-DATE
              AND CHFILE.CH_CODE >= :QCH-WBEG-CODE
              AND CHFILE.CH_CODE <= :QCH-WEND-CODE
              AND CHFILE.CH_USERID >= :QCH-WBEG-USERID
              AND CHFILE.CH_USERID <= :QCH-WEND-USERID
              AND CHFILE.CH_TIME >= :QCH-WBEG-TIME
              AND CHFILE.CH_TIME <= :QCH-WEND-TIME
              AND CHFILE.CH_SEQ >= :QCH-WBEG-SEQ
              AND CHFILE.CH_SEQ <= :QCH-WEND-SEQ
              ORDER BY CHFILE.CH_BRNO,
                       CHFILE.CH_DATE,
                       CHFILE.CH_CODE,
                       CHFILE.CH_USERID,
                       CHFILE.CH_TIME,
                       CHFILE.CH_SEQ
           END-EXEC.

           EXEC SQL
               OPEN CH_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QCH-WBEG-BRNO 
                          QCH-WEND-BRNO
                          QCH-WBEG-TIME
                          QCH-WEND-TIME
                          QCH-WBEG-SEQ
                          QCH-WEND-SEQ.
 
           MOVE SPACES TO QCH-WBEG-DATE
                          QCH-WEND-DATE
                          QCH-WBEG-CODE
                          QCH-WEND-CODE
                          QCH-WBEG-USERID
                          QCH-WEND-USERID.

           MOVE STAT---GOOD TO CH-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-CH-FILE.
           PERFORM READ-IT.
           MOVE CH-PATH-OWNBR  TO CH-BRNO. 
           MOVE CH-SELECT-PATH TO E-FILE.
           MOVE CH-KEY         TO E-KEYX.

           MOVE CH-BRNO             TO QCH-BRNO.
           MOVE CH-DATE             TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QCH-DATE.
           MOVE CH-CODE             TO QCH-CODE.
           MOVE CH-USERID           TO QCH-USERID.
           MOVE CH-TIME             TO QCH-TIME.
           MOVE CH-SEQ              TO QCH-SEQ.


           EXEC SQL
            SELECT CHFILE.CH_BRNO,
                   ISNULL(CAST(CHFILE.CH_DATE AS VARCHAR(10)),'0'),
                   ISNULL(CHFILE.CH_CODE,''),
                   ISNULL(CHFILE.CH_USERID,''),
                   CHFILE.CH_TIME,
                   CHFILE.CH_SEQ,
                   ISNULL(CAST(CHFILE.CH_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CHFILE.CH_DESCRIPT,''),
                   CHFILE.CH_ACCTNO,
                   CHFILE.CH_SSNO,
                   CHFILE.CH_TYPE,
                   ISNULL(CHFILE.CH_ORG_CHAR,''),
                   ISNULL(CHFILE.CH_NEW_CHAR,'')
            INTO :QCH-REC
            FROM DBO.CHFILE
            WHERE CHFILE.CH_BRNO = :QCH-BRNO
              AND CHFILE.CH_DATE = :QCH-DATE
              AND CHFILE.CH_CODE = :QCH-CODE
              AND CHFILE.CH_USERID = :QCH-USERID
              AND CHFILE.CH_TIME = :QCH-TIME
              AND CHFILE.CH_SEQ = :QCH-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CH-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-CH-FIELDS.

      *-----------------------------------------------------------------
       READ-CH-FILE-NEXT.
           PERFORM READ-IT.
           MOVE CH-PATH-OWNBR  TO CH-BRNO.
           MOVE CH-SELECT-PATH TO E-FILE.
           MOVE CH-KEY         TO E-KEYX.
           INITIALIZE QCH-REC.

           IF ( CH-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH CH_CURSOR INTO :QCH-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CH-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-CH-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-CH-FILE.

           IF ( CH-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE CH_CURSOR
              END-EXEC
              MOVE STAT---BAD TO CH-CURSOR-STAT.

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
              DESTROY IOCHRN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
