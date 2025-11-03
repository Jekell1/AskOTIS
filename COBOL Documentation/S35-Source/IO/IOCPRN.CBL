       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOCPRN.
       AUTHOR.        AMM.
       DATE-WRITTEN.  04-15-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOCPRN.SQB
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : CPFILE 
      * TABLE DESC    : COLLECTOR POOL FILE
      * OUTPUT FILE   : XPC/IOCPRN.PRE
      * USED FOR      : LIBSP/SPWSCP.CPY  --- QCP-REC
      *                 LIBSP/SPCP1R.CPY  --- READ
      *                 LIBSP/SPCP1RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * AMM 2021-0415 ***NEW***
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
           "@(#)IO/IOCPRN.CBL S34 05/19/21-13:08:39 >".

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
       COPY "LIBSP/SP01CP.CPY".
       COPY "LIBSP/SPWSCP.CPY".

      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S LIBXX/XX01YY_SQL.CPY
      *#################################################################

      *=================================================================
      *
      *    Q - F I L E
      *
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QCP1-WHERE-BEG-KEY.
           03  QCP1-WBEG-BRNO             PIC 9(04).
           03  QCP1-WBEG-ID               PIC X(10).
 
       01  QCP1-WHERE-END-KEY.
           03  QCP1-WEND-BRNO             PIC 9(04).
           03  QCP1-WEND-ID               PIC X(10).
 
       01  QCP-REC.
           03  QCP-BRNO              PIC 9(4).
           03  QCP-ID                PIC X(3).
           03  QCP-LTOUCH-DATE       PIC X(10).
           03  QCP-DESC              PIC X(20).
           03  QCP-COL-ID-1          PIC X(3).
           03  QCP-COL-ID-2          PIC X(3).
           03  QCP-COL-ID-3          PIC X(3).
           03  QCP-COL-ID-4          PIC X(3).
           03  QCP-COL-ID-5          PIC X(3).
           03  QCP-COL-ID-6          PIC X(3).
           03  QCP-COL-ID-7          PIC X(3).
           03  QCP-COL-ID-8          PIC X(3).
           03  QCP-COL-ID-9          PIC X(3).
           03  QCP-COL-ID-10         PIC X(3).
           03  QCP-COL-ID-11         PIC X(3).
           03  QCP-COL-ID-12         PIC X(3).
           03  QCP-COL-ID-13         PIC X(3).
           03  QCP-COL-ID-14         PIC X(3).
           03  QCP-COL-ID-15         PIC X(3).
           03  QCP-COL-ID-16         PIC X(3).
           03  QCP-COL-ID-17         PIC X(3).
           03  QCP-COL-ID-18         PIC X(3).
           03  QCP-COL-ID-19         PIC X(3).
           03  QCP-COL-ID-20         PIC X(3).
           03  QCP-COL-ID-21         PIC X(3).
           03  QCP-COL-ID-22         PIC X(3).
           03  QCP-COL-ID-23         PIC X(3).
           03  QCP-COL-ID-24         PIC X(3).
           03  QCP-COL-ID-25         PIC X(3).
           03  QCP-COL-ID-26         PIC X(3).
           03  QCP-COL-ID-27         PIC X(3).
           03  QCP-COL-ID-28         PIC X(3).
           03  QCP-COL-ID-29         PIC X(3).
           03  QCP-COL-ID-30         PIC X(3).
           03  QCP-COL-ID-31         PIC X(3).
           03  QCP-COL-ID-32         PIC X(3).
           03  QCP-COL-ID-33         PIC X(3).
           03  QCP-COL-ID-34         PIC X(3).
           03  QCP-COL-ID-35         PIC X(3).
           03  QCP-COL-ID-36         PIC X(3).
           03  QCP-COL-ID-37         PIC X(3).
           03  QCP-COL-ID-38         PIC X(3).
           03  QCP-COL-ID-39         PIC X(3).
           03  QCP-COL-ID-40         PIC X(3).
           03  QCP-COL-ID-41         PIC X(3).
           03  QCP-COL-ID-42         PIC X(3).
           03  QCP-COL-ID-43         PIC X(3).
           03  QCP-COL-ID-44         PIC X(3).
           03  QCP-COL-ID-45         PIC X(3).
           03  QCP-COL-ID-46         PIC X(3).
           03  QCP-COL-ID-47         PIC X(3).
           03  QCP-COL-ID-48         PIC X(3).
           03  QCP-COL-ID-49         PIC X(3).
           03  QCP-COL-ID-50         PIC X(3).
           03  QCP-COL-ID-51         PIC X(3).
           03  QCP-COL-ID-52         PIC X(3).
           03  QCP-COL-ID-53         PIC X(3).
           03  QCP-COL-ID-54         PIC X(3).
           03  QCP-COL-ID-55         PIC X(3).
           03  QCP-COL-ID-56         PIC X(3).
           03  QCP-COL-ID-57         PIC X(3).
           03  QCP-COL-ID-58         PIC X(3).
           03  QCP-COL-ID-59         PIC X(3).
           03  QCP-COL-ID-60         PIC X(3).
           03  QCP-COL-ID-61         PIC X(3).
           03  QCP-COL-ID-62         PIC X(3).
           03  QCP-COL-ID-63         PIC X(3).
           03  QCP-COL-ID-64         PIC X(3).
           03  QCP-COL-ID-65         PIC X(3).
           03  QCP-COL-ID-66         PIC X(3).
           03  QCP-COL-ID-67         PIC X(3).
           03  QCP-COL-ID-68         PIC X(3).
           03  QCP-COL-ID-69         PIC X(3).
           03  QCP-COL-ID-70         PIC X(3).
           03  QCP-COL-ID-71         PIC X(3).
           03  QCP-COL-ID-72         PIC X(3).
           03  QCP-COL-ID-73         PIC X(3).
           03  QCP-COL-ID-74         PIC X(3).
           03  QCP-COL-ID-75         PIC X(3).
           03  QCP-COL-ID-76         PIC X(3).
           03  QCP-COL-ID-77         PIC X(3).
           03  QCP-COL-ID-78         PIC X(3).
           03  QCP-COL-ID-79         PIC X(3).
           03  QCP-COL-ID-80         PIC X(3).
           03  QCP-ALPHA-FLAG        PIC X.
           03  QCP-TOTAL-ACCOUNTS    PIC 9(7).
           03  QCP-COLLECTOR-COUNT   PIC 9(3).
           03  QCP-LAST-SUBSCRIPT    PIC 9(3).
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
           03  BEG-ID                  PIC X(03).
           03  END-ID                  PIC X(03).

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
           03  FILLER PIC X(30) VALUE "BRNO  ID                      ".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(06) VALUE SPACES.

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-CP-BRNO                    PIC 9(04).
           03  FILLER                         PIC X(01).
           03  D01-CP-ID                      PIC X(03).

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
       COPY "LIBIO/IOCPRN_DEF.CPY".
       COPY "LIBIO/IOCPRN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOCPRN_SCN.CPY".

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
           MOVE "IOCPRN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOCPRN-QUEUE-POS  TO Q-POS.
           MOVE IOCPRN-COPIES-POS TO C-POS.
           MOVE IOCPRN-PRU-POS    TO P-POS.
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
              GO TO ENTER-END-ID.

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
       ENTER-BEG-ID.

           MOVE "E  A030BEG-ID." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-PROCESS-KEY.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-ID.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-ID.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-ID.

      *-----------------------------------------------------------------
       ENTER-END-ID.

           MOVE "E  A030END-ID." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-ID.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-ID.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-ID.

           IF ( BEG-ID > END-ID )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-ID.

           GO TO ENTER-ACCEPT-YN.

      *-----------------------------------------------------------------
       ENTER-ALL-ID.

           MOVE "S  A030BEG-ID." TO VDU.
           MOVE ZEROES              TO VDUBUF  BEG-ID.
           PERFORM SCREENIO.

           MOVE "S  A030END-ID." TO VDU.
           MOVE ALL "Z"             TO VDUBUF  END-ID.
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

           MOVE "S  A030BEG-ID."          TO VDU.
           MOVE BEG-ID                    TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A030END-ID."          TO VDU.
           MOVE END-ID                    TO VDUBUF.
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

           MOVE "B"                        TO CP-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - -{ SET WHERE BEG & END FIELDS }
           MOVE BR-NO     TO QCP1-WBEG-BRNO.
           MOVE BR-NO     TO QCP1-WEND-BRNO.

           MOVE BEG-ID     TO QCP1-WBEG-ID.
           MOVE END-ID     TO QCP1-WEND-ID.

      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           IF ( PROCESS-KEY = 1 )
              PERFORM OPEN-CP1-FILE
              PERFORM START-CP1-FILE.

      *=================================================================
       PROCESS-BRANCH-NEXT.

           IF ( PROCESS-KEY = 1 )
              PERFORM READ-CP1-FILE-NEXT.

           IF ( IO-BAD ) OR ( CP-BRNO NOT = CP-PATH-OWNBR )
              GO TO PROCESS-BRANCH-MODULE-END.

           ADD 01 TO RECORDS-READ.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

           IF ( PROCESS-KEY = 1 )
              IF ( CP-ID < BEG-ID )
                 GO TO PROCESS-BRANCH-NEXT
              ELSE
              IF ( CP-ID > END-ID )
                 GO TO PROCESS-BRANCH-MODULE-END.

      *-----------------------------------------------------------------
       PROCESS-BRANCH-PRINT.

           MOVE CP-BRNO                    TO D01-CP-BRNO.
           MOVE CP-ID                      TO D01-CP-ID.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.
           GO TO PROCESS-BRANCH-NEXT.

      *=================================================================
       PROCESS-BRANCH-MODULE-END.

           IF ( PROCESS-KEY = 1 )
              PERFORM CLOSE-CP1-FILE.

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
              MOVE CP1-KEY TO PACE-DETAIL-KEY.

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
           MOVE "BEGINNING ID                  : "    TO R01-PROMPT.
           MOVE BEG-ID                                TO R01-NUMERIC.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    ID                  : "    TO R01-PROMPT.
           MOVE END-ID                                TO R01-NUMERIC.
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
           MOVE "SQL TABLE IO UTILITY - CPFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOCPRN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOCPRN-SYSDATE.
           DISPLAY IOCPRN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOCPRN-FORM-FIELDS TO WR-BUF.
           MOVE IOCPRN-FORM-LIST TO WR-LIST.
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

           MOVE IOCPRN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOCPRN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOCPRN_EVA.CPY".
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
      *    LOAD CP-REC FROM QCP-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-CP-FIELDS.
           MOVE QCP-BRNO TO CP-BRNO.
           MOVE QCP-ID TO CP-ID.

           IF ( QCP-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO CP-LTOUCH-DATE
           ELSE
              MOVE QCP-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CP-LTOUCH-DATE.

           MOVE QCP-DESC TO CP-DESC.
           MOVE QCP-COL-ID-1 TO CP-COL-ID(1).
           MOVE QCP-COL-ID-2 TO CP-COL-ID(2).
           MOVE QCP-COL-ID-3 TO CP-COL-ID(3).
           MOVE QCP-COL-ID-4 TO CP-COL-ID(4).
           MOVE QCP-COL-ID-5 TO CP-COL-ID(5).
           MOVE QCP-COL-ID-6 TO CP-COL-ID(6).
           MOVE QCP-COL-ID-7 TO CP-COL-ID(7).
           MOVE QCP-COL-ID-8 TO CP-COL-ID(8).
           MOVE QCP-COL-ID-9 TO CP-COL-ID(9).
           MOVE QCP-COL-ID-10 TO CP-COL-ID(10).
           MOVE QCP-COL-ID-11 TO CP-COL-ID(11).
           MOVE QCP-COL-ID-12 TO CP-COL-ID(12).
           MOVE QCP-COL-ID-13 TO CP-COL-ID(13).
           MOVE QCP-COL-ID-14 TO CP-COL-ID(14).
           MOVE QCP-COL-ID-15 TO CP-COL-ID(15).
           MOVE QCP-COL-ID-16 TO CP-COL-ID(16).
           MOVE QCP-COL-ID-17 TO CP-COL-ID(17).
           MOVE QCP-COL-ID-18 TO CP-COL-ID(18).
           MOVE QCP-COL-ID-19 TO CP-COL-ID(19).
           MOVE QCP-COL-ID-20 TO CP-COL-ID(20).
           MOVE QCP-COL-ID-21 TO CP-COL-ID(21).
           MOVE QCP-COL-ID-22 TO CP-COL-ID(22).
           MOVE QCP-COL-ID-23 TO CP-COL-ID(23).
           MOVE QCP-COL-ID-24 TO CP-COL-ID(24).
           MOVE QCP-COL-ID-25 TO CP-COL-ID(25).
           MOVE QCP-COL-ID-26 TO CP-COL-ID(26).
           MOVE QCP-COL-ID-27 TO CP-COL-ID(27).
           MOVE QCP-COL-ID-28 TO CP-COL-ID(28).
           MOVE QCP-COL-ID-29 TO CP-COL-ID(29).
           MOVE QCP-COL-ID-30 TO CP-COL-ID(30).
           MOVE QCP-COL-ID-31 TO CP-COL-ID(31).
           MOVE QCP-COL-ID-32 TO CP-COL-ID(32).
           MOVE QCP-COL-ID-33 TO CP-COL-ID(33).
           MOVE QCP-COL-ID-34 TO CP-COL-ID(34).
           MOVE QCP-COL-ID-35 TO CP-COL-ID(35).
           MOVE QCP-COL-ID-36 TO CP-COL-ID(36).
           MOVE QCP-COL-ID-37 TO CP-COL-ID(37).
           MOVE QCP-COL-ID-38 TO CP-COL-ID(38).
           MOVE QCP-COL-ID-39 TO CP-COL-ID(39).
           MOVE QCP-COL-ID-40 TO CP-COL-ID(40).
           MOVE QCP-COL-ID-41 TO CP-COL-ID(41).
           MOVE QCP-COL-ID-42 TO CP-COL-ID(42).
           MOVE QCP-COL-ID-43 TO CP-COL-ID(43).
           MOVE QCP-COL-ID-44 TO CP-COL-ID(44).
           MOVE QCP-COL-ID-45 TO CP-COL-ID(45).
           MOVE QCP-COL-ID-46 TO CP-COL-ID(46).
           MOVE QCP-COL-ID-47 TO CP-COL-ID(47).
           MOVE QCP-COL-ID-48 TO CP-COL-ID(48).
           MOVE QCP-COL-ID-49 TO CP-COL-ID(49).
           MOVE QCP-COL-ID-50 TO CP-COL-ID(50).
           MOVE QCP-COL-ID-51 TO CP-COL-ID(51).
           MOVE QCP-COL-ID-52 TO CP-COL-ID(52).
           MOVE QCP-COL-ID-53 TO CP-COL-ID(53).
           MOVE QCP-COL-ID-54 TO CP-COL-ID(54).
           MOVE QCP-COL-ID-55 TO CP-COL-ID(55).
           MOVE QCP-COL-ID-56 TO CP-COL-ID(56).
           MOVE QCP-COL-ID-57 TO CP-COL-ID(57).
           MOVE QCP-COL-ID-58 TO CP-COL-ID(58).
           MOVE QCP-COL-ID-59 TO CP-COL-ID(59).
           MOVE QCP-COL-ID-60 TO CP-COL-ID(60).
           MOVE QCP-COL-ID-61 TO CP-COL-ID(61).
           MOVE QCP-COL-ID-62 TO CP-COL-ID(62).
           MOVE QCP-COL-ID-63 TO CP-COL-ID(63).
           MOVE QCP-COL-ID-64 TO CP-COL-ID(64).
           MOVE QCP-COL-ID-65 TO CP-COL-ID(65).
           MOVE QCP-COL-ID-66 TO CP-COL-ID(66).
           MOVE QCP-COL-ID-67 TO CP-COL-ID(67).
           MOVE QCP-COL-ID-68 TO CP-COL-ID(68).
           MOVE QCP-COL-ID-69 TO CP-COL-ID(69).
           MOVE QCP-COL-ID-70 TO CP-COL-ID(70).
           MOVE QCP-COL-ID-71 TO CP-COL-ID(71).
           MOVE QCP-COL-ID-72 TO CP-COL-ID(72).
           MOVE QCP-COL-ID-73 TO CP-COL-ID(73).
           MOVE QCP-COL-ID-74 TO CP-COL-ID(74).
           MOVE QCP-COL-ID-75 TO CP-COL-ID(75).
           MOVE QCP-COL-ID-76 TO CP-COL-ID(76).
           MOVE QCP-COL-ID-77 TO CP-COL-ID(77).
           MOVE QCP-COL-ID-78 TO CP-COL-ID(78).
           MOVE QCP-COL-ID-79 TO CP-COL-ID(79).
           MOVE QCP-COL-ID-80 TO CP-COL-ID(80).
           MOVE QCP-ALPHA-FLAG TO CP-ALPHA-FLAG.
           MOVE QCP-TOTAL-ACCOUNTS TO CP-TOTAL-ACCOUNTS.
           MOVE QCP-COLLECTOR-COUNT TO CP-COLLECTOR-COUNT.
           MOVE QCP-LAST-SUBSCRIPT TO CP-LAST-SUBSCRIPT.
 
      *-----------------------------------------------------------------
      *    LOAD QCP-REC FROM CP-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-CP-FIELDS.
           MOVE CP-BRNO TO QCP-BRNO.
           MOVE CP-ID TO QCP-ID.

           IF ( CP-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QCP-LTOUCH-DATE
           ELSE
              MOVE CP-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCP-LTOUCH-DATE.

           MOVE CP-DESC TO QCP-DESC.
           MOVE CP-COL-ID(1) TO QCP-COL-ID-1.
           MOVE CP-COL-ID(2) TO QCP-COL-ID-2.
           MOVE CP-COL-ID(3) TO QCP-COL-ID-3.
           MOVE CP-COL-ID(4) TO QCP-COL-ID-4.
           MOVE CP-COL-ID(5) TO QCP-COL-ID-5.
           MOVE CP-COL-ID(6) TO QCP-COL-ID-6.
           MOVE CP-COL-ID(7) TO QCP-COL-ID-7.
           MOVE CP-COL-ID(8) TO QCP-COL-ID-8.
           MOVE CP-COL-ID(9) TO QCP-COL-ID-9.
           MOVE CP-COL-ID(10) TO QCP-COL-ID-10.
           MOVE CP-COL-ID(11) TO QCP-COL-ID-11.
           MOVE CP-COL-ID(12) TO QCP-COL-ID-12.
           MOVE CP-COL-ID(13) TO QCP-COL-ID-13.
           MOVE CP-COL-ID(14) TO QCP-COL-ID-14.
           MOVE CP-COL-ID(15) TO QCP-COL-ID-15.
           MOVE CP-COL-ID(16) TO QCP-COL-ID-16.
           MOVE CP-COL-ID(17) TO QCP-COL-ID-17.
           MOVE CP-COL-ID(18) TO QCP-COL-ID-18.
           MOVE CP-COL-ID(19) TO QCP-COL-ID-19.
           MOVE CP-COL-ID(20) TO QCP-COL-ID-20.
           MOVE CP-COL-ID(21) TO QCP-COL-ID-21.
           MOVE CP-COL-ID(22) TO QCP-COL-ID-22.
           MOVE CP-COL-ID(23) TO QCP-COL-ID-23.
           MOVE CP-COL-ID(24) TO QCP-COL-ID-24.
           MOVE CP-COL-ID(25) TO QCP-COL-ID-25.
           MOVE CP-COL-ID(26) TO QCP-COL-ID-26.
           MOVE CP-COL-ID(27) TO QCP-COL-ID-27.
           MOVE CP-COL-ID(28) TO QCP-COL-ID-28.
           MOVE CP-COL-ID(29) TO QCP-COL-ID-29.
           MOVE CP-COL-ID(30) TO QCP-COL-ID-30.
           MOVE CP-COL-ID(31) TO QCP-COL-ID-31.
           MOVE CP-COL-ID(32) TO QCP-COL-ID-32.
           MOVE CP-COL-ID(33) TO QCP-COL-ID-33.
           MOVE CP-COL-ID(34) TO QCP-COL-ID-34.
           MOVE CP-COL-ID(35) TO QCP-COL-ID-35.
           MOVE CP-COL-ID(36) TO QCP-COL-ID-36.
           MOVE CP-COL-ID(37) TO QCP-COL-ID-37.
           MOVE CP-COL-ID(38) TO QCP-COL-ID-38.
           MOVE CP-COL-ID(39) TO QCP-COL-ID-39.
           MOVE CP-COL-ID(40) TO QCP-COL-ID-40.
           MOVE CP-COL-ID(41) TO QCP-COL-ID-41.
           MOVE CP-COL-ID(42) TO QCP-COL-ID-42.
           MOVE CP-COL-ID(43) TO QCP-COL-ID-43.
           MOVE CP-COL-ID(44) TO QCP-COL-ID-44.
           MOVE CP-COL-ID(45) TO QCP-COL-ID-45.
           MOVE CP-COL-ID(46) TO QCP-COL-ID-46.
           MOVE CP-COL-ID(47) TO QCP-COL-ID-47.
           MOVE CP-COL-ID(48) TO QCP-COL-ID-48.
           MOVE CP-COL-ID(49) TO QCP-COL-ID-49.
           MOVE CP-COL-ID(50) TO QCP-COL-ID-50.
           MOVE CP-COL-ID(51) TO QCP-COL-ID-51.
           MOVE CP-COL-ID(52) TO QCP-COL-ID-52.
           MOVE CP-COL-ID(53) TO QCP-COL-ID-53.
           MOVE CP-COL-ID(54) TO QCP-COL-ID-54.
           MOVE CP-COL-ID(55) TO QCP-COL-ID-55.
           MOVE CP-COL-ID(56) TO QCP-COL-ID-56.
           MOVE CP-COL-ID(57) TO QCP-COL-ID-57.
           MOVE CP-COL-ID(58) TO QCP-COL-ID-58.
           MOVE CP-COL-ID(59) TO QCP-COL-ID-59.
           MOVE CP-COL-ID(60) TO QCP-COL-ID-60.
           MOVE CP-COL-ID(61) TO QCP-COL-ID-61.
           MOVE CP-COL-ID(62) TO QCP-COL-ID-62.
           MOVE CP-COL-ID(63) TO QCP-COL-ID-63.
           MOVE CP-COL-ID(64) TO QCP-COL-ID-64.
           MOVE CP-COL-ID(65) TO QCP-COL-ID-65.
           MOVE CP-COL-ID(66) TO QCP-COL-ID-66.
           MOVE CP-COL-ID(67) TO QCP-COL-ID-67.
           MOVE CP-COL-ID(68) TO QCP-COL-ID-68.
           MOVE CP-COL-ID(69) TO QCP-COL-ID-69.
           MOVE CP-COL-ID(70) TO QCP-COL-ID-70.
           MOVE CP-COL-ID(71) TO QCP-COL-ID-71.
           MOVE CP-COL-ID(72) TO QCP-COL-ID-72.
           MOVE CP-COL-ID(73) TO QCP-COL-ID-73.
           MOVE CP-COL-ID(74) TO QCP-COL-ID-74.
           MOVE CP-COL-ID(75) TO QCP-COL-ID-75.
           MOVE CP-COL-ID(76) TO QCP-COL-ID-76.
           MOVE CP-COL-ID(77) TO QCP-COL-ID-77.
           MOVE CP-COL-ID(78) TO QCP-COL-ID-78.
           MOVE CP-COL-ID(79) TO QCP-COL-ID-79.
           MOVE CP-COL-ID(80) TO QCP-COL-ID-80.
           MOVE CP-ALPHA-FLAG TO QCP-ALPHA-FLAG.
           MOVE CP-TOTAL-ACCOUNTS TO QCP-TOTAL-ACCOUNTS.
           MOVE CP-COLLECTOR-COUNT TO QCP-COLLECTOR-COUNT.
           MOVE CP-LAST-SUBSCRIPT TO QCP-LAST-SUBSCRIPT.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
       LOAD-CP1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CP-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CP-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO CP-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CP-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO CP-ALL-DATA
              MOVE CP-ALL-PATH             TO CP-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CP-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO CP-ALL-BASE
              MOVE FILPATH-MACHINE         TO CP-ALL-MACHINE
              MOVE FILPATH-DATA            TO CP-ALL-DATA
              MOVE CP-ALL-PATH             TO CP-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-CPFILE NOT = "S" )
              MOVE CP-PATH-BEG             TO CP-SELECT-PATH-BEG
              MOVE CP-PATH-FILE            TO CP-SELECT-PATH-FILE
           ELSE
              MOVE CP-PATH-SQL             TO CP-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-CP1-FILE.
           PERFORM LOAD-CP1-FILE.
           PERFORM OPEN-IT.
           MOVE CP-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-CP1-FILE.
           PERFORM START-IT.
           IF ( CP-PATH-OVERRIDE NOT = "G" )
              MOVE CP-PATH-OWNBR  TO CP-BRNO
           ELSE
              MOVE 0              TO CP-BRNO.
           MOVE CP-SELECT-PATH TO E-FILE.
           MOVE CP1-KEY        TO E-KEYX.

           IF ( CP1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-CP1-FILE.

           IF ( QCP1-WEND-ID = SPACES    ) 
              MOVE CP-BRNO              TO QCP1-WBEG-BRNO
              MOVE CP-BRNO              TO QCP1-WEND-BRNO
              MOVE CP-ID                TO QCP1-WBEG-ID
              MOVE ALL "Z"              TO QCP1-WEND-ID.

           EXEC SQL
            DECLARE CP1_CURSOR CURSOR FOR
            SELECT CPFILE.CP_BRNO,
                   ISNULL(CPFILE.CP_ID,''),
                   ISNULL(CAST(CPFILE.CP_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CPFILE.CP_DESC,''),
                   ISNULL(CPFILE.CP_COL_ID_1,''),
                   ISNULL(CPFILE.CP_COL_ID_2,''),
                   ISNULL(CPFILE.CP_COL_ID_3,''),
                   ISNULL(CPFILE.CP_COL_ID_4,''),
                   ISNULL(CPFILE.CP_COL_ID_5,''),
                   ISNULL(CPFILE.CP_COL_ID_6,''),
                   ISNULL(CPFILE.CP_COL_ID_7,''),
                   ISNULL(CPFILE.CP_COL_ID_8,''),
                   ISNULL(CPFILE.CP_COL_ID_9,''),
                   ISNULL(CPFILE.CP_COL_ID_10,''),
                   ISNULL(CPFILE.CP_COL_ID_11,''),
                   ISNULL(CPFILE.CP_COL_ID_12,''),
                   ISNULL(CPFILE.CP_COL_ID_13,''),
                   ISNULL(CPFILE.CP_COL_ID_14,''),
                   ISNULL(CPFILE.CP_COL_ID_15,''),
                   ISNULL(CPFILE.CP_COL_ID_16,''),
                   ISNULL(CPFILE.CP_COL_ID_17,''),
                   ISNULL(CPFILE.CP_COL_ID_18,''),
                   ISNULL(CPFILE.CP_COL_ID_19,''),
                   ISNULL(CPFILE.CP_COL_ID_20,''),
                   ISNULL(CPFILE.CP_COL_ID_21,''),
                   ISNULL(CPFILE.CP_COL_ID_22,''),
                   ISNULL(CPFILE.CP_COL_ID_23,''),
                   ISNULL(CPFILE.CP_COL_ID_24,''),
                   ISNULL(CPFILE.CP_COL_ID_25,''),
                   ISNULL(CPFILE.CP_COL_ID_26,''),
                   ISNULL(CPFILE.CP_COL_ID_27,''),
                   ISNULL(CPFILE.CP_COL_ID_28,''),
                   ISNULL(CPFILE.CP_COL_ID_29,''),
                   ISNULL(CPFILE.CP_COL_ID_30,''),
                   ISNULL(CPFILE.CP_COL_ID_31,''),
                   ISNULL(CPFILE.CP_COL_ID_32,''),
                   ISNULL(CPFILE.CP_COL_ID_33,''),
                   ISNULL(CPFILE.CP_COL_ID_34,''),
                   ISNULL(CPFILE.CP_COL_ID_35,''),
                   ISNULL(CPFILE.CP_COL_ID_36,''),
                   ISNULL(CPFILE.CP_COL_ID_37,''),
                   ISNULL(CPFILE.CP_COL_ID_38,''),
                   ISNULL(CPFILE.CP_COL_ID_39,''),
                   ISNULL(CPFILE.CP_COL_ID_40,''),
                   ISNULL(CPFILE.CP_COL_ID_41,''),
                   ISNULL(CPFILE.CP_COL_ID_42,''),
                   ISNULL(CPFILE.CP_COL_ID_43,''),
                   ISNULL(CPFILE.CP_COL_ID_44,''),
                   ISNULL(CPFILE.CP_COL_ID_45,''),
                   ISNULL(CPFILE.CP_COL_ID_46,''),
                   ISNULL(CPFILE.CP_COL_ID_47,''),
                   ISNULL(CPFILE.CP_COL_ID_48,''),
                   ISNULL(CPFILE.CP_COL_ID_49,''),
                   ISNULL(CPFILE.CP_COL_ID_50,''),
                   ISNULL(CPFILE.CP_COL_ID_51,''),
                   ISNULL(CPFILE.CP_COL_ID_52,''),
                   ISNULL(CPFILE.CP_COL_ID_53,''),
                   ISNULL(CPFILE.CP_COL_ID_54,''),
                   ISNULL(CPFILE.CP_COL_ID_55,''),
                   ISNULL(CPFILE.CP_COL_ID_56,''),
                   ISNULL(CPFILE.CP_COL_ID_57,''),
                   ISNULL(CPFILE.CP_COL_ID_58,''),
                   ISNULL(CPFILE.CP_COL_ID_59,''),
                   ISNULL(CPFILE.CP_COL_ID_60,''),
                   ISNULL(CPFILE.CP_COL_ID_61,''),
                   ISNULL(CPFILE.CP_COL_ID_62,''),
                   ISNULL(CPFILE.CP_COL_ID_63,''),
                   ISNULL(CPFILE.CP_COL_ID_64,''),
                   ISNULL(CPFILE.CP_COL_ID_65,''),
                   ISNULL(CPFILE.CP_COL_ID_66,''),
                   ISNULL(CPFILE.CP_COL_ID_67,''),
                   ISNULL(CPFILE.CP_COL_ID_68,''),
                   ISNULL(CPFILE.CP_COL_ID_69,''),
                   ISNULL(CPFILE.CP_COL_ID_70,''),
                   ISNULL(CPFILE.CP_COL_ID_71,''),
                   ISNULL(CPFILE.CP_COL_ID_72,''),
                   ISNULL(CPFILE.CP_COL_ID_73,''),
                   ISNULL(CPFILE.CP_COL_ID_74,''),
                   ISNULL(CPFILE.CP_COL_ID_75,''),
                   ISNULL(CPFILE.CP_COL_ID_76,''),
                   ISNULL(CPFILE.CP_COL_ID_77,''),
                   ISNULL(CPFILE.CP_COL_ID_78,''),
                   ISNULL(CPFILE.CP_COL_ID_79,''),
                   ISNULL(CPFILE.CP_COL_ID_80,''),
                   ISNULL(CPFILE.CP_ALPHA_FLAG,''),
                   CPFILE.CP_TOTAL_ACCOUNTS,
                   CPFILE.CP_COLLECTOR_COUNT,
                   CPFILE.CP_LAST_SUBSCRIPT
            FROM DBO.CPFILE
            WHERE CPFILE.CP_BRNO >= :QCP1-WBEG-BRNO
              AND CPFILE.CP_BRNO <= :QCP1-WEND-BRNO
              AND CPFILE.CP_ID >= :QCP1-WBEG-ID
              AND CPFILE.CP_ID <= :QCP1-WEND-ID
            ORDER BY CPFILE.CP_BRNO,
                     CPFILE.CP_ID
           END-EXEC.

           EXEC SQL
               OPEN CP1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QCP1-WBEG-BRNO 
                          QCP1-WEND-BRNO.
 
           MOVE SPACES TO QCP1-WBEG-ID
                          QCP1-WEND-ID.

           MOVE STAT---GOOD TO CP1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-CP1-FILE.
           IF ( CP-PATH-OVERRIDE NOT = "G" )
              MOVE CP-PATH-OWNBR  TO CP-BRNO
           ELSE
              MOVE 0              TO CP-BRNO.
           PERFORM READ-IT.
           MOVE CP-SELECT-PATH TO E-FILE.
           MOVE CP1-KEY        TO E-KEYX.

           MOVE CP-BRNO   TO QCP-BRNO.
           MOVE CP-ID     TO QCP-ID.

           EXEC SQL
            SELECT CPFILE.CP_BRNO,
                   ISNULL(CPFILE.CP_ID,''),
                   ISNULL(CAST(CPFILE.CP_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CPFILE.CP_DESC,''),
                   ISNULL(CPFILE.CP_COL_ID_1,''),
                   ISNULL(CPFILE.CP_COL_ID_2,''),
                   ISNULL(CPFILE.CP_COL_ID_3,''),
                   ISNULL(CPFILE.CP_COL_ID_4,''),
                   ISNULL(CPFILE.CP_COL_ID_5,''),
                   ISNULL(CPFILE.CP_COL_ID_6,''),
                   ISNULL(CPFILE.CP_COL_ID_7,''),
                   ISNULL(CPFILE.CP_COL_ID_8,''),
                   ISNULL(CPFILE.CP_COL_ID_9,''),
                   ISNULL(CPFILE.CP_COL_ID_10,''),
                   ISNULL(CPFILE.CP_COL_ID_11,''),
                   ISNULL(CPFILE.CP_COL_ID_12,''),
                   ISNULL(CPFILE.CP_COL_ID_13,''),
                   ISNULL(CPFILE.CP_COL_ID_14,''),
                   ISNULL(CPFILE.CP_COL_ID_15,''),
                   ISNULL(CPFILE.CP_COL_ID_16,''),
                   ISNULL(CPFILE.CP_COL_ID_17,''),
                   ISNULL(CPFILE.CP_COL_ID_18,''),
                   ISNULL(CPFILE.CP_COL_ID_19,''),
                   ISNULL(CPFILE.CP_COL_ID_20,''),
                   ISNULL(CPFILE.CP_COL_ID_21,''),
                   ISNULL(CPFILE.CP_COL_ID_22,''),
                   ISNULL(CPFILE.CP_COL_ID_23,''),
                   ISNULL(CPFILE.CP_COL_ID_24,''),
                   ISNULL(CPFILE.CP_COL_ID_25,''),
                   ISNULL(CPFILE.CP_COL_ID_26,''),
                   ISNULL(CPFILE.CP_COL_ID_27,''),
                   ISNULL(CPFILE.CP_COL_ID_28,''),
                   ISNULL(CPFILE.CP_COL_ID_29,''),
                   ISNULL(CPFILE.CP_COL_ID_30,''),
                   ISNULL(CPFILE.CP_COL_ID_31,''),
                   ISNULL(CPFILE.CP_COL_ID_32,''),
                   ISNULL(CPFILE.CP_COL_ID_33,''),
                   ISNULL(CPFILE.CP_COL_ID_34,''),
                   ISNULL(CPFILE.CP_COL_ID_35,''),
                   ISNULL(CPFILE.CP_COL_ID_36,''),
                   ISNULL(CPFILE.CP_COL_ID_37,''),
                   ISNULL(CPFILE.CP_COL_ID_38,''),
                   ISNULL(CPFILE.CP_COL_ID_39,''),
                   ISNULL(CPFILE.CP_COL_ID_40,''),
                   ISNULL(CPFILE.CP_COL_ID_41,''),
                   ISNULL(CPFILE.CP_COL_ID_42,''),
                   ISNULL(CPFILE.CP_COL_ID_43,''),
                   ISNULL(CPFILE.CP_COL_ID_44,''),
                   ISNULL(CPFILE.CP_COL_ID_45,''),
                   ISNULL(CPFILE.CP_COL_ID_46,''),
                   ISNULL(CPFILE.CP_COL_ID_47,''),
                   ISNULL(CPFILE.CP_COL_ID_48,''),
                   ISNULL(CPFILE.CP_COL_ID_49,''),
                   ISNULL(CPFILE.CP_COL_ID_50,''),
                   ISNULL(CPFILE.CP_COL_ID_51,''),
                   ISNULL(CPFILE.CP_COL_ID_52,''),
                   ISNULL(CPFILE.CP_COL_ID_53,''),
                   ISNULL(CPFILE.CP_COL_ID_54,''),
                   ISNULL(CPFILE.CP_COL_ID_55,''),
                   ISNULL(CPFILE.CP_COL_ID_56,''),
                   ISNULL(CPFILE.CP_COL_ID_57,''),
                   ISNULL(CPFILE.CP_COL_ID_58,''),
                   ISNULL(CPFILE.CP_COL_ID_59,''),
                   ISNULL(CPFILE.CP_COL_ID_60,''),
                   ISNULL(CPFILE.CP_COL_ID_61,''),
                   ISNULL(CPFILE.CP_COL_ID_62,''),
                   ISNULL(CPFILE.CP_COL_ID_63,''),
                   ISNULL(CPFILE.CP_COL_ID_64,''),
                   ISNULL(CPFILE.CP_COL_ID_65,''),
                   ISNULL(CPFILE.CP_COL_ID_66,''),
                   ISNULL(CPFILE.CP_COL_ID_67,''),
                   ISNULL(CPFILE.CP_COL_ID_68,''),
                   ISNULL(CPFILE.CP_COL_ID_69,''),
                   ISNULL(CPFILE.CP_COL_ID_70,''),
                   ISNULL(CPFILE.CP_COL_ID_71,''),
                   ISNULL(CPFILE.CP_COL_ID_72,''),
                   ISNULL(CPFILE.CP_COL_ID_73,''),
                   ISNULL(CPFILE.CP_COL_ID_74,''),
                   ISNULL(CPFILE.CP_COL_ID_75,''),
                   ISNULL(CPFILE.CP_COL_ID_76,''),
                   ISNULL(CPFILE.CP_COL_ID_77,''),
                   ISNULL(CPFILE.CP_COL_ID_78,''),
                   ISNULL(CPFILE.CP_COL_ID_79,''),
                   ISNULL(CPFILE.CP_COL_ID_80,''),
                   ISNULL(CPFILE.CP_ALPHA_FLAG,''),
                   CPFILE.CP_TOTAL_ACCOUNTS,
                   CPFILE.CP_COLLECTOR_COUNT,
                   CPFILE.CP_LAST_SUBSCRIPT
            INTO :QCP-REC
            FROM DBO.CPFILE
            WHERE CPFILE.CP_BRNO = :QCP-BRNO 
              AND CPFILE.CP_ID = :QCP-ID
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CP1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-CP-FIELDS.

      *-----------------------------------------------------------------
       READ-CP1-FILE-NEXT.
           IF ( CP-PATH-OVERRIDE NOT = "G" )
              MOVE CP-PATH-OWNBR  TO CP-BRNO
           ELSE
              MOVE 0              TO CP-BRNO.
           PERFORM READ-IT.
           MOVE CP-SELECT-PATH TO E-FILE.
           MOVE CP1-KEY        TO E-KEYX.
           INITIALIZE QCP-REC.

           IF ( CP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH CP1_CURSOR INTO :QCP-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CP1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-CP-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-CP1-FILE.

           IF ( CP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE CP1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO CP1-CURSOR-STAT.

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
              DESTROY IOCPRN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
