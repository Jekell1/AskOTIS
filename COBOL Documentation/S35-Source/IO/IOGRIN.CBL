       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOGRIN.
       AUTHOR.        AMM.
       DATE-WRITTEN.  05-11-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOGRIN
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : GRFILE 
      * TABLE DESC    : GROUP MASTER FILE
      * OUTPUT FILE   : XPC/IOGRIN.PRE
      * USED FOR      : LIBGB/GBWSGR.CPY  --- QGR-REC
      *                 LIBGB/GBGR1RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * AMM 2021-0511 ***NEW***
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
           "@(#)IO/IOGRIN.CBL S35 11/01/21-12:44:15 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GBWSGR.CPY".

      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S WS COPY MEMBERS
      *#################################################################
      *=================================================================
      *
      *    Q - F I L E
      *
      *-----------------------------------------------------------------
      *    FOR UPDATING THE FILE'S WS COPY MEMBER: LIB??/??WS??.CPY
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QGR1-WHERE-BEG-KEY.
           05  QGR1-WBEG-ID               PIC X(4).

       01  QGR1-WHERE-END-KEY.
           05  QGR1-WEND-ID               PIC X(4).

       01  QGR-REC.
           03  QGR-ID                    PIC X(4).
           03  QGR-NAME                  PIC X(20).
           03  QGR-LEVEL                 PIC 9.
           03  QGR-MEMBER-ID-1           PIC X(4).
           03  QGR-MEMBER-ID-2           PIC X(4).
           03  QGR-MEMBER-ID-3           PIC X(4).
           03  QGR-MEMBER-ID-4           PIC X(4).
           03  QGR-MEMBER-ID-5           PIC X(4).
           03  QGR-MEMBER-ID-6           PIC X(4).
           03  QGR-MEMBER-ID-7           PIC X(4).
           03  QGR-MEMBER-ID-8           PIC X(4).
           03  QGR-MEMBER-ID-9           PIC X(4).
           03  QGR-MEMBER-ID-10          PIC X(4).
           03  QGR-MEMBER-ID-11           PIC X(4).
           03  QGR-MEMBER-ID-12           PIC X(4).
           03  QGR-MEMBER-ID-13           PIC X(4).
           03  QGR-MEMBER-ID-14           PIC X(4).
           03  QGR-MEMBER-ID-15           PIC X(4).
           03  QGR-MEMBER-ID-16           PIC X(4).
           03  QGR-MEMBER-ID-17           PIC X(4).
           03  QGR-MEMBER-ID-18           PIC X(4).
           03  QGR-MEMBER-ID-19           PIC X(4).
           03  QGR-MEMBER-ID-20           PIC X(4).
           03  QGR-MEMBER-ID-21           PIC X(4).
           03  QGR-MEMBER-ID-22           PIC X(4).
           03  QGR-MEMBER-ID-23           PIC X(4).
           03  QGR-MEMBER-ID-24           PIC X(4).
           03  QGR-MEMBER-ID-25           PIC X(4).
           03  QGR-MEMBER-ID-26           PIC X(4).
           03  QGR-MEMBER-ID-27           PIC X(4).
           03  QGR-MEMBER-ID-28           PIC X(4).
           03  QGR-MEMBER-ID-29           PIC X(4).
           03  QGR-MEMBER-ID-30          PIC X(4).
           03  QGR-MEMBER-ID-31           PIC X(4).
           03  QGR-MEMBER-ID-32           PIC X(4).
           03  QGR-MEMBER-ID-33           PIC X(4).
           03  QGR-MEMBER-ID-34           PIC X(4).
           03  QGR-MEMBER-ID-35           PIC X(4).
           03  QGR-MEMBER-ID-36           PIC X(4).
           03  QGR-MEMBER-ID-37           PIC X(4).
           03  QGR-MEMBER-ID-38           PIC X(4).
           03  QGR-MEMBER-ID-39           PIC X(4).
           03  QGR-MEMBER-ID-40          PIC X(4).
           03  QGR-MEMBER-ID-41           PIC X(4).
           03  QGR-MEMBER-ID-42           PIC X(4).
           03  QGR-MEMBER-ID-43           PIC X(4).
           03  QGR-MEMBER-ID-44           PIC X(4).
           03  QGR-MEMBER-ID-45           PIC X(4).
           03  QGR-MEMBER-ID-46           PIC X(4).
           03  QGR-MEMBER-ID-47           PIC X(4).
           03  QGR-MEMBER-ID-48           PIC X(4).
           03  QGR-MEMBER-ID-49           PIC X(4).
           03  QGR-MEMBER-ID-50          PIC X(4).
           03  QGR-MEMBER-ID-51           PIC X(4).
           03  QGR-MEMBER-ID-52           PIC X(4).
           03  QGR-MEMBER-ID-53           PIC X(4).
           03  QGR-MEMBER-ID-54           PIC X(4).
           03  QGR-MEMBER-ID-55           PIC X(4).
           03  QGR-MEMBER-ID-56           PIC X(4).
           03  QGR-MEMBER-ID-57           PIC X(4).
           03  QGR-MEMBER-ID-58           PIC X(4).
           03  QGR-MEMBER-ID-59           PIC X(4).
           03  QGR-MEMBER-ID-60          PIC X(4).
           03  QGR-MEMBER-ID-61           PIC X(4).
           03  QGR-MEMBER-ID-62           PIC X(4).
           03  QGR-MEMBER-ID-63           PIC X(4).
           03  QGR-MEMBER-ID-64           PIC X(4).
           03  QGR-MEMBER-ID-65           PIC X(4).
           03  QGR-MEMBER-ID-66           PIC X(4).
           03  QGR-MEMBER-ID-67           PIC X(4).
           03  QGR-MEMBER-ID-68           PIC X(4).
           03  QGR-MEMBER-ID-69           PIC X(4).
           03  QGR-MEMBER-ID-70          PIC X(4).
           03  QGR-MEMBER-ID-71           PIC X(4).
           03  QGR-MEMBER-ID-72           PIC X(4).
           03  QGR-MEMBER-ID-73           PIC X(4).
           03  QGR-MEMBER-ID-74           PIC X(4).
           03  QGR-MEMBER-ID-75           PIC X(4).
           03  QGR-MEMBER-ID-76           PIC X(4).
           03  QGR-MEMBER-ID-77           PIC X(4).
           03  QGR-MEMBER-ID-78           PIC X(4).
           03  QGR-MEMBER-ID-79           PIC X(4).
           03  QGR-MEMBER-ID-80          PIC X(4).
           03  QGR-MEMBER-ID-81           PIC X(4).
           03  QGR-MEMBER-ID-82           PIC X(4).
           03  QGR-MEMBER-ID-83           PIC X(4).
           03  QGR-MEMBER-ID-84           PIC X(4).
           03  QGR-MEMBER-ID-85           PIC X(4).
           03  QGR-MEMBER-ID-86           PIC X(4).
           03  QGR-MEMBER-ID-87           PIC X(4).
           03  QGR-MEMBER-ID-88           PIC X(4).
           03  QGR-MEMBER-ID-89           PIC X(4).
           03  QGR-MEMBER-ID-90          PIC X(4).
           03  QGR-MEMBER-ID-91           PIC X(4).
           03  QGR-MEMBER-ID-92           PIC X(4).
           03  QGR-MEMBER-ID-93           PIC X(4).
           03  QGR-MEMBER-ID-94           PIC X(4).
           03  QGR-MEMBER-ID-95           PIC X(4).
           03  QGR-MEMBER-ID-96           PIC X(4).
           03  QGR-MEMBER-ID-97           PIC X(4).
           03  QGR-MEMBER-ID-98           PIC X(4).
           03  QGR-MEMBER-ID-99           PIC X(4).
           03  QGR-MEMBER-ID-100          PIC X(4).
           03  QGR-CONTACT               PIC X(20).
           03  QGR-ADDR-1                PIC X(20).
           03  QGR-ADDR-2                PIC X(20).
           03  QGR-CITY                  PIC X(15).
           03  QGR-STATE                 PIC X(4).
           03  QGR-ZIP                   PIC X(10).
           03  QGR-PHONE                 PIC 9(10).
           03  QGR-HOME-BRANCH           PIC 9(4).
           03  QGR-LN-APPROVAL-LIMIT     PIC 9(7).
           03  QGR-LTOUCH-DATE           PIC X(10).

       EXEC SQL END DECLARE SECTION END-EXEC.
      *#################################################################
      *    ENDING OF UPDATE TO THE FILE'S WS COPY MEMBERS
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

       01  LEGEND-BEG-GROUP.
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING GROUP      (F3-ALL) ".

       01  LEGEND-END-GROUP.
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING GROUP      ".

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
           03  BEG-GROUP               PIC X(04).
           03  END-GROUP               PIC X(04).

           03  PROCESS-KEY             PIC 9(01).

      *-----------------------------------------------------------------
       01  VERSION-CONTROL             PIC X(01)     VALUE "A".

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
           03  FILLER PIC X(30) VALUE "GRID NAME                 # ID".
           03  FILLER PIC X(30) VALUE "1  ID2  ID3  ID4  ID5  CONTACT".
           03  FILLER PIC X(30) VALUE "              DESC-1          ".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(12) VALUE SPACES.

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-GR-ID                      PIC X(4).
           03  FILLER                         PIC X(01).
           03  D01-GR-NAME                    PIC X(20).
           03  FILLER                         PIC X(01).
           03  D01-GR-LEVEL                   PIC 9.
           03  FILLER                         PIC X(01).
           03  D01-GR-MEMBER-ID-1             PIC X(4).
           03  FILLER                         PIC X(01).
           03  D01-GR-MEMBER-ID-2             PIC X(4).
           03  FILLER                         PIC X(01).
           03  D01-GR-MEMBER-ID-3             PIC X(4).
           03  FILLER                         PIC X(01).
           03  D01-GR-MEMBER-ID-4             PIC X(4).
           03  FILLER                         PIC X(01).
           03  D01-GR-MEMBER-ID-5             PIC X(4).
           03  FILLER                         PIC X(01).
           03  D01-GR-CONTACT                 PIC X(20).

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
      *COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
      *COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".
      *COPY "LIBGB/GRWORKW.CPY".
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
      *COPY "LIBWI/GRSCANW.CPY".

       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBIO/IOGRIN_DEF.CPY".
       COPY "LIBIO/IOGRIN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOGRIN_SCN.CPY".

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
           PERFORM CLOSE-PR-FILE.
      *%%% CLOSE GR-FILE.
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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      D    STOP FORM-PATHNAME.
           MOVE "B"                        TO GR-PATH-OVERRIDE.
           MOVE BEG-GROUP                  TO GR-ID.
           PERFORM OPEN-GR1-FILE.
           PERFORM START-GR1-FILE.

      *=================================================================
       MAIN-PROGRAM-NEXT.

           ADD 01 TO RECORDS-READ.

           PERFORM READ-GR1-FILE-NEXT.

           IF ( IO-BAD )
              GO TO MAIN-PROGRAM-END.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( GR-ID < BEG-GROUP )
              GO TO MAIN-PROGRAM-NEXT
           ELSE
           IF ( GR-ID > END-GROUP )
              GO TO MAIN-PROGRAM-END.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE GR-ID                      TO D01-GR-ID.
           MOVE GR-NAME                    TO D01-GR-NAME.
           MOVE GR-LEVEL                   TO D01-GR-LEVEL.
           MOVE GR-MEMBER-ID(1)            TO D01-GR-MEMBER-ID-1.
           MOVE GR-MEMBER-ID(2)            TO D01-GR-MEMBER-ID-2.
           MOVE GR-MEMBER-ID(3)            TO D01-GR-MEMBER-ID-3.
           MOVE GR-MEMBER-ID(4)            TO D01-GR-MEMBER-ID-4.
           MOVE GR-MEMBER-ID(5)            TO D01-GR-MEMBER-ID-5.
           MOVE GR-CONTACT                 TO D01-GR-CONTACT.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.

           GO TO MAIN-PROGRAM-NEXT.

      *=================================================================
       MAIN-PROGRAM-END.

           PERFORM WRITE-SUMMARY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    DISPLAY SUMMARY TO SCREEN
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PROGRAM COMPLETED!!!" TO SENDMESS-HEAD.
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
           MOVE "IOGRIN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOGRIN-QUEUE-POS  TO Q-POS.
           MOVE IOGRIN-COPIES-POS TO C-POS.
           MOVE IOGRIN-PRU-POS    TO P-POS.
           MOVE SPACES            TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF EXT-ROUTE-BUF = "00"
              MOVE "PROCESSING..." TO LEGEND
              PERFORM SEND-LEGEND.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE "S  A010ACCEPT." TO VDU
              MOVE SPACES           TO VDUBUF
              PERFORM SCREENIO.

           PERFORM GET-GPENV.
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
              GO TO ENTER-PROCESS-KEY.

      *=================================================================
       ENTER-BEG-GROUP.

           MOVE LEGEND-BEG-GROUP TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A040BEG-GROUP." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-GROUP.
      *KC  IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-GROUP.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-GROUP.

      *-----------------------------------------------------------------
       ENTER-END-GROUP.

           MOVE LEGEND-END-GROUP TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A040END-GROUP." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-GROUP.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-GROUP.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-GROUP.

           IF ( BEG-GROUP > END-GROUP )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-GROUP.

           GO TO ENTER-PROCESS-KEY.

      *-----------------------------------------------------------------
       ENTER-ALL-GROUP.

           MOVE "S  A040BEG-GROUP." TO VDU.
           MOVE SPACES               TO BEG-GROUP VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A040END-GROUP." TO VDU.
           MOVE ALL "Z"              TO END-GROUP VDUBUF.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-PROCESS-KEY.

           MOVE LEGEND-PROCESS-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN010PROCESS-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-GROUP.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUNUM0 < ZEROES ) OR
              ( VDUNUM0 > 1      )     
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PROCESS-KEY.

           MOVE VDUNUM0 TO PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE LEGEND-BEG-END-RANGE TO LEGEND.
           PERFORM SEND-LEGEND.

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

           MOVE "S  A000BEG-GROUP."    TO VDU.
           MOVE BEG-GROUP              TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A000END-GROUP."    TO VDU.
           MOVE END-GROUP              TO VDUBUF.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN010PROCESS-KEY." TO VDU
           MOVE PROCESS-KEY           TO VDUNUM0.
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

           MOVE GR1-KEY     TO PACE-DETAIL-KEY.

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
           MOVE "BEGINNING GROUP               : "    TO R01-PROMPT.
           MOVE BEG-GROUP                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    GROUP               : "    TO R01-PROMPT.
           MOVE END-GROUP                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PROCESS KEY                   : "    TO R01-PROMPT.
           MOVE PROCESS-KEY                           TO R01-NUMERIC.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    MOVE "BEGINNING ACCTNO              : "    TO R01-PROMPT.
      *    MOVE BEG-ACCTNO                            TO R01-NUMERIC.
      *    MOVE 1                                     TO LN-FEED.
      *    PERFORM WRITE-DETAIL-LINE.

      *    MOVE "ENDING    ACCTNO              : "    TO R01-PROMPT.
      *    MOVE END-ACCTNO                            TO R01-NUMERIC.
      *    MOVE 1                                     TO LN-FEED.
      *    PERFORM WRITE-DETAIL-LINE.

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
           MOVE "SQL TABLE IO UTILITY - GRFILE"  TO H02-TITLE.
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

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -{04}
           MOVE ALL "="                          TO PR-REC.
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

           MOVE EXT-CONAME-CONAME TO IOGRIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOGRIN-SYSDATE.
           DISPLAY IOGRIN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOGRIN-FORM-FIELDS TO WR-BUF.
           MOVE IOGRIN-FORM-LIST TO WR-LIST.
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

           MOVE IOGRIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOGRIN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOGRIN_EVA.CPY".
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
      *COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/DATER.CPY".
      *COPY "LIBGB/EOSPOOL.CPY".
       COPY "LIBGB/GPENV.CPY".
      *COPY "LIBGB/GRSERCH.CPY".
      *COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

       COPY "LIBWI/PFSWINDOWS.CPY".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - P A R A G R A P H S
      *
      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/SYSTEM.CPY".
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
       COPY "LIBGB/GBPRINT.CPY".

      *-----------------------------------------------------------------
      *    LOAD GR-REC FROM QGR-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-GR-FIELDS.
           MOVE QGR-ID TO GR-ID.
           MOVE QGR-NAME TO GR-NAME.
           MOVE QGR-LEVEL TO GR-LEVEL.
           MOVE QGR-MEMBER-ID-1 TO GR-MEMBER-ID(1).
           MOVE QGR-MEMBER-ID-2 TO GR-MEMBER-ID(2).
           MOVE QGR-MEMBER-ID-3 TO GR-MEMBER-ID(3).
           MOVE QGR-MEMBER-ID-4 TO GR-MEMBER-ID(4).
           MOVE QGR-MEMBER-ID-5 TO GR-MEMBER-ID(5).
           MOVE QGR-MEMBER-ID-6 TO GR-MEMBER-ID(6).
           MOVE QGR-MEMBER-ID-7 TO GR-MEMBER-ID(7).
           MOVE QGR-MEMBER-ID-8 TO GR-MEMBER-ID(8).
           MOVE QGR-MEMBER-ID-9 TO GR-MEMBER-ID(9).
           MOVE QGR-MEMBER-ID-10 TO GR-MEMBER-ID(10).
           MOVE QGR-MEMBER-ID-11 TO GR-MEMBER-ID(11).
           MOVE QGR-MEMBER-ID-12 TO GR-MEMBER-ID(12).
           MOVE QGR-MEMBER-ID-13 TO GR-MEMBER-ID(13).
           MOVE QGR-MEMBER-ID-14 TO GR-MEMBER-ID(14).
           MOVE QGR-MEMBER-ID-15 TO GR-MEMBER-ID(15).
           MOVE QGR-MEMBER-ID-16 TO GR-MEMBER-ID(16).
           MOVE QGR-MEMBER-ID-17 TO GR-MEMBER-ID(17).
           MOVE QGR-MEMBER-ID-18 TO GR-MEMBER-ID(18).
           MOVE QGR-MEMBER-ID-19 TO GR-MEMBER-ID(19).
           MOVE QGR-MEMBER-ID-20 TO GR-MEMBER-ID(20).
           MOVE QGR-MEMBER-ID-21 TO GR-MEMBER-ID(21).
           MOVE QGR-MEMBER-ID-22 TO GR-MEMBER-ID(22).
           MOVE QGR-MEMBER-ID-23 TO GR-MEMBER-ID(23).
           MOVE QGR-MEMBER-ID-24 TO GR-MEMBER-ID(24).
           MOVE QGR-MEMBER-ID-25 TO GR-MEMBER-ID(25).
           MOVE QGR-MEMBER-ID-26 TO GR-MEMBER-ID(26).
           MOVE QGR-MEMBER-ID-27 TO GR-MEMBER-ID(27).
           MOVE QGR-MEMBER-ID-28 TO GR-MEMBER-ID(28).
           MOVE QGR-MEMBER-ID-29 TO GR-MEMBER-ID(29).
           MOVE QGR-MEMBER-ID-30 TO GR-MEMBER-ID(30).
           MOVE QGR-MEMBER-ID-31 TO GR-MEMBER-ID(31).
           MOVE QGR-MEMBER-ID-32 TO GR-MEMBER-ID(32).
           MOVE QGR-MEMBER-ID-33 TO GR-MEMBER-ID(33).
           MOVE QGR-MEMBER-ID-34 TO GR-MEMBER-ID(34).
           MOVE QGR-MEMBER-ID-35 TO GR-MEMBER-ID(35).
           MOVE QGR-MEMBER-ID-36 TO GR-MEMBER-ID(36).
           MOVE QGR-MEMBER-ID-37 TO GR-MEMBER-ID(37).
           MOVE QGR-MEMBER-ID-38 TO GR-MEMBER-ID(38).
           MOVE QGR-MEMBER-ID-39 TO GR-MEMBER-ID(39).
           MOVE QGR-MEMBER-ID-40 TO GR-MEMBER-ID(40).
           MOVE QGR-MEMBER-ID-41 TO GR-MEMBER-ID(41).
           MOVE QGR-MEMBER-ID-42 TO GR-MEMBER-ID(42).
           MOVE QGR-MEMBER-ID-43 TO GR-MEMBER-ID(43).
           MOVE QGR-MEMBER-ID-44 TO GR-MEMBER-ID(44).
           MOVE QGR-MEMBER-ID-45 TO GR-MEMBER-ID(45).
           MOVE QGR-MEMBER-ID-46 TO GR-MEMBER-ID(46).
           MOVE QGR-MEMBER-ID-47 TO GR-MEMBER-ID(47).
           MOVE QGR-MEMBER-ID-48 TO GR-MEMBER-ID(48).
           MOVE QGR-MEMBER-ID-49 TO GR-MEMBER-ID(49).
           MOVE QGR-MEMBER-ID-50 TO GR-MEMBER-ID(50).
           MOVE QGR-MEMBER-ID-51 TO GR-MEMBER-ID(51).
           MOVE QGR-MEMBER-ID-52 TO GR-MEMBER-ID(52).
           MOVE QGR-MEMBER-ID-53 TO GR-MEMBER-ID(53).
           MOVE QGR-MEMBER-ID-54 TO GR-MEMBER-ID(54).
           MOVE QGR-MEMBER-ID-55 TO GR-MEMBER-ID(55).
           MOVE QGR-MEMBER-ID-56 TO GR-MEMBER-ID(56).
           MOVE QGR-MEMBER-ID-57 TO GR-MEMBER-ID(57).
           MOVE QGR-MEMBER-ID-58 TO GR-MEMBER-ID(58).
           MOVE QGR-MEMBER-ID-59 TO GR-MEMBER-ID(59).
           MOVE QGR-MEMBER-ID-60 TO GR-MEMBER-ID(60).
           MOVE QGR-MEMBER-ID-61 TO GR-MEMBER-ID(61).
           MOVE QGR-MEMBER-ID-62 TO GR-MEMBER-ID(62).
           MOVE QGR-MEMBER-ID-63 TO GR-MEMBER-ID(63).
           MOVE QGR-MEMBER-ID-64 TO GR-MEMBER-ID(64).
           MOVE QGR-MEMBER-ID-65 TO GR-MEMBER-ID(65).
           MOVE QGR-MEMBER-ID-66 TO GR-MEMBER-ID(66).
           MOVE QGR-MEMBER-ID-67 TO GR-MEMBER-ID(67).
           MOVE QGR-MEMBER-ID-68 TO GR-MEMBER-ID(68).
           MOVE QGR-MEMBER-ID-69 TO GR-MEMBER-ID(69).
           MOVE QGR-MEMBER-ID-70 TO GR-MEMBER-ID(70).
           MOVE QGR-MEMBER-ID-71 TO GR-MEMBER-ID(71).
           MOVE QGR-MEMBER-ID-72 TO GR-MEMBER-ID(72).
           MOVE QGR-MEMBER-ID-73 TO GR-MEMBER-ID(73).
           MOVE QGR-MEMBER-ID-74 TO GR-MEMBER-ID(74).
           MOVE QGR-MEMBER-ID-75 TO GR-MEMBER-ID(75).
           MOVE QGR-MEMBER-ID-76 TO GR-MEMBER-ID(76).
           MOVE QGR-MEMBER-ID-77 TO GR-MEMBER-ID(77).
           MOVE QGR-MEMBER-ID-78 TO GR-MEMBER-ID(78).
           MOVE QGR-MEMBER-ID-79 TO GR-MEMBER-ID(79).
           MOVE QGR-MEMBER-ID-80 TO GR-MEMBER-ID(80).
           MOVE QGR-MEMBER-ID-81 TO GR-MEMBER-ID(81).
           MOVE QGR-MEMBER-ID-82 TO GR-MEMBER-ID(82).
           MOVE QGR-MEMBER-ID-83 TO GR-MEMBER-ID(83).
           MOVE QGR-MEMBER-ID-84 TO GR-MEMBER-ID(84).
           MOVE QGR-MEMBER-ID-85 TO GR-MEMBER-ID(85).
           MOVE QGR-MEMBER-ID-86 TO GR-MEMBER-ID(86).
           MOVE QGR-MEMBER-ID-87 TO GR-MEMBER-ID(87).
           MOVE QGR-MEMBER-ID-88 TO GR-MEMBER-ID(88).
           MOVE QGR-MEMBER-ID-89 TO GR-MEMBER-ID(89).
           MOVE QGR-MEMBER-ID-90 TO GR-MEMBER-ID(90).
           MOVE QGR-MEMBER-ID-91 TO GR-MEMBER-ID(91).
           MOVE QGR-MEMBER-ID-92 TO GR-MEMBER-ID(92).
           MOVE QGR-MEMBER-ID-93 TO GR-MEMBER-ID(93).
           MOVE QGR-MEMBER-ID-94 TO GR-MEMBER-ID(94).
           MOVE QGR-MEMBER-ID-95 TO GR-MEMBER-ID(95).
           MOVE QGR-MEMBER-ID-96 TO GR-MEMBER-ID(96).
           MOVE QGR-MEMBER-ID-97 TO GR-MEMBER-ID(97).
           MOVE QGR-MEMBER-ID-98 TO GR-MEMBER-ID(98).
           MOVE QGR-MEMBER-ID-99 TO GR-MEMBER-ID(99).
           MOVE QGR-MEMBER-ID-100 TO GR-MEMBER-ID(100).
           MOVE QGR-CONTACT TO GR-CONTACT.
           MOVE QGR-ADDR-1 TO GR-ADDR-1.
           MOVE QGR-ADDR-2 TO GR-ADDR-2.
           MOVE QGR-CITY TO GR-CITY.
           MOVE QGR-STATE TO GR-STATE.
           MOVE QGR-ZIP TO GR-ZIP.
           MOVE QGR-PHONE TO GR-PHONE.
           MOVE QGR-HOME-BRANCH TO GR-HOME-BRANCH.
           MOVE QGR-LN-APPROVAL-LIMIT TO GR-LN-APPROVAL-LIMIT.

           IF ( QGR-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO GR-LTOUCH-DATE
           ELSE
              MOVE QGR-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GR-LTOUCH-DATE.

 
      *-----------------------------------------------------------------
      *    LOAD QGR-REC FROM GR-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-GR-FIELDS.
           MOVE GR-ID TO QGR-ID.
           MOVE GR-NAME TO QGR-NAME.
           MOVE GR-LEVEL TO QGR-LEVEL.
           MOVE GR-MEMBER-ID(1) TO QGR-MEMBER-ID-1.
           MOVE GR-MEMBER-ID(2) TO QGR-MEMBER-ID-2.
           MOVE GR-MEMBER-ID(3) TO QGR-MEMBER-ID-3.
           MOVE GR-MEMBER-ID(4) TO QGR-MEMBER-ID-4.
           MOVE GR-MEMBER-ID(5) TO QGR-MEMBER-ID-5.
           MOVE GR-MEMBER-ID(6) TO QGR-MEMBER-ID-6.
           MOVE GR-MEMBER-ID(7) TO QGR-MEMBER-ID-7.
           MOVE GR-MEMBER-ID(8) TO QGR-MEMBER-ID-8.
           MOVE GR-MEMBER-ID(9) TO QGR-MEMBER-ID-9.
           MOVE GR-MEMBER-ID(10) TO QGR-MEMBER-ID-10.
           MOVE GR-MEMBER-ID(11) TO QGR-MEMBER-ID-11.
           MOVE GR-MEMBER-ID(12) TO QGR-MEMBER-ID-12.
           MOVE GR-MEMBER-ID(13) TO QGR-MEMBER-ID-13.
           MOVE GR-MEMBER-ID(14) TO QGR-MEMBER-ID-14.
           MOVE GR-MEMBER-ID(15) TO QGR-MEMBER-ID-15.
           MOVE GR-MEMBER-ID(16) TO QGR-MEMBER-ID-16.
           MOVE GR-MEMBER-ID(17) TO QGR-MEMBER-ID-17.
           MOVE GR-MEMBER-ID(18) TO QGR-MEMBER-ID-18.
           MOVE GR-MEMBER-ID(19) TO QGR-MEMBER-ID-19.
           MOVE GR-MEMBER-ID(20) TO QGR-MEMBER-ID-20.
           MOVE GR-MEMBER-ID(21) TO QGR-MEMBER-ID-21.
           MOVE GR-MEMBER-ID(22) TO QGR-MEMBER-ID-22.
           MOVE GR-MEMBER-ID(23) TO QGR-MEMBER-ID-23.
           MOVE GR-MEMBER-ID(24) TO QGR-MEMBER-ID-24.
           MOVE GR-MEMBER-ID(25) TO QGR-MEMBER-ID-25.
           MOVE GR-MEMBER-ID(26) TO QGR-MEMBER-ID-26.
           MOVE GR-MEMBER-ID(27) TO QGR-MEMBER-ID-27.
           MOVE GR-MEMBER-ID(28) TO QGR-MEMBER-ID-28.
           MOVE GR-MEMBER-ID(29) TO QGR-MEMBER-ID-29.
           MOVE GR-MEMBER-ID(30) TO QGR-MEMBER-ID-30.
           MOVE GR-MEMBER-ID(31) TO QGR-MEMBER-ID-31.
           MOVE GR-MEMBER-ID(32) TO QGR-MEMBER-ID-32.
           MOVE GR-MEMBER-ID(33) TO QGR-MEMBER-ID-33.
           MOVE GR-MEMBER-ID(34) TO QGR-MEMBER-ID-34.
           MOVE GR-MEMBER-ID(35) TO QGR-MEMBER-ID-35.
           MOVE GR-MEMBER-ID(36) TO QGR-MEMBER-ID-36.
           MOVE GR-MEMBER-ID(37) TO QGR-MEMBER-ID-37.
           MOVE GR-MEMBER-ID(38) TO QGR-MEMBER-ID-38.
           MOVE GR-MEMBER-ID(39) TO QGR-MEMBER-ID-39.
           MOVE GR-MEMBER-ID(40) TO QGR-MEMBER-ID-40.
           MOVE GR-MEMBER-ID(41) TO QGR-MEMBER-ID-41.
           MOVE GR-MEMBER-ID(42) TO QGR-MEMBER-ID-42.
           MOVE GR-MEMBER-ID(43) TO QGR-MEMBER-ID-43.
           MOVE GR-MEMBER-ID(44) TO QGR-MEMBER-ID-44.
           MOVE GR-MEMBER-ID(45) TO QGR-MEMBER-ID-45.
           MOVE GR-MEMBER-ID(46) TO QGR-MEMBER-ID-46.
           MOVE GR-MEMBER-ID(47) TO QGR-MEMBER-ID-47.
           MOVE GR-MEMBER-ID(48) TO QGR-MEMBER-ID-48.
           MOVE GR-MEMBER-ID(49) TO QGR-MEMBER-ID-49.
           MOVE GR-MEMBER-ID(50) TO QGR-MEMBER-ID-50.
           MOVE GR-MEMBER-ID(51) TO QGR-MEMBER-ID-51.
           MOVE GR-MEMBER-ID(52) TO QGR-MEMBER-ID-52.
           MOVE GR-MEMBER-ID(53) TO QGR-MEMBER-ID-53.
           MOVE GR-MEMBER-ID(54) TO QGR-MEMBER-ID-54.
           MOVE GR-MEMBER-ID(55) TO QGR-MEMBER-ID-55.
           MOVE GR-MEMBER-ID(56) TO QGR-MEMBER-ID-56.
           MOVE GR-MEMBER-ID(57) TO QGR-MEMBER-ID-57.
           MOVE GR-MEMBER-ID(58) TO QGR-MEMBER-ID-58.
           MOVE GR-MEMBER-ID(59) TO QGR-MEMBER-ID-59.
           MOVE GR-MEMBER-ID(60) TO QGR-MEMBER-ID-60.
           MOVE GR-MEMBER-ID(61) TO QGR-MEMBER-ID-61.
           MOVE GR-MEMBER-ID(62) TO QGR-MEMBER-ID-62.
           MOVE GR-MEMBER-ID(63) TO QGR-MEMBER-ID-63.
           MOVE GR-MEMBER-ID(64) TO QGR-MEMBER-ID-64.
           MOVE GR-MEMBER-ID(65) TO QGR-MEMBER-ID-65.
           MOVE GR-MEMBER-ID(66) TO QGR-MEMBER-ID-66.
           MOVE GR-MEMBER-ID(67) TO QGR-MEMBER-ID-67.
           MOVE GR-MEMBER-ID(68) TO QGR-MEMBER-ID-68.
           MOVE GR-MEMBER-ID(69) TO QGR-MEMBER-ID-69.
           MOVE GR-MEMBER-ID(70) TO QGR-MEMBER-ID-70.
           MOVE GR-MEMBER-ID(71) TO QGR-MEMBER-ID-71.
           MOVE GR-MEMBER-ID(72) TO QGR-MEMBER-ID-72.
           MOVE GR-MEMBER-ID(73) TO QGR-MEMBER-ID-73.
           MOVE GR-MEMBER-ID(74) TO QGR-MEMBER-ID-74.
           MOVE GR-MEMBER-ID(75) TO QGR-MEMBER-ID-75.
           MOVE GR-MEMBER-ID(76) TO QGR-MEMBER-ID-76.
           MOVE GR-MEMBER-ID(77) TO QGR-MEMBER-ID-77.
           MOVE GR-MEMBER-ID(78) TO QGR-MEMBER-ID-78.
           MOVE GR-MEMBER-ID(79) TO QGR-MEMBER-ID-79.
           MOVE GR-MEMBER-ID(80) TO QGR-MEMBER-ID-80.
           MOVE GR-MEMBER-ID(81) TO QGR-MEMBER-ID-81.
           MOVE GR-MEMBER-ID(82) TO QGR-MEMBER-ID-82.
           MOVE GR-MEMBER-ID(83) TO QGR-MEMBER-ID-83.
           MOVE GR-MEMBER-ID(84) TO QGR-MEMBER-ID-84.
           MOVE GR-MEMBER-ID(85) TO QGR-MEMBER-ID-85.
           MOVE GR-MEMBER-ID(86) TO QGR-MEMBER-ID-86.
           MOVE GR-MEMBER-ID(87) TO QGR-MEMBER-ID-87.
           MOVE GR-MEMBER-ID(88) TO QGR-MEMBER-ID-88.
           MOVE GR-MEMBER-ID(89) TO QGR-MEMBER-ID-89.
           MOVE GR-MEMBER-ID(90) TO QGR-MEMBER-ID-90.
           MOVE GR-MEMBER-ID(91) TO QGR-MEMBER-ID-91.
           MOVE GR-MEMBER-ID(92) TO QGR-MEMBER-ID-92.
           MOVE GR-MEMBER-ID(93) TO QGR-MEMBER-ID-93.
           MOVE GR-MEMBER-ID(94) TO QGR-MEMBER-ID-94.
           MOVE GR-MEMBER-ID(95) TO QGR-MEMBER-ID-95.
           MOVE GR-MEMBER-ID(96) TO QGR-MEMBER-ID-96.
           MOVE GR-MEMBER-ID(97) TO QGR-MEMBER-ID-97.
           MOVE GR-MEMBER-ID(98) TO QGR-MEMBER-ID-98.
           MOVE GR-MEMBER-ID(99) TO QGR-MEMBER-ID-99.
           MOVE GR-MEMBER-ID(100) TO QGR-MEMBER-ID-100.
           MOVE GR-CONTACT TO QGR-CONTACT.
           MOVE GR-ADDR-1 TO QGR-ADDR-1.
           MOVE GR-ADDR-2 TO QGR-ADDR-2.
           MOVE GR-CITY TO QGR-CITY.
           MOVE GR-STATE TO QGR-STATE.
           MOVE GR-ZIP TO QGR-ZIP.
           MOVE GR-PHONE TO QGR-PHONE.
           MOVE GR-HOME-BRANCH TO QGR-HOME-BRANCH.
           MOVE GR-LN-APPROVAL-LIMIT TO QGR-LN-APPROVAL-LIMIT.

           IF ( GR-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QGR-LTOUCH-DATE
           ELSE
              MOVE GR-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGR-LTOUCH-DATE.

 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      * COPYMEMBER: LIBGB/GBGR1RN
       LOAD-GR1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( GR-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( GR-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO GR-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO GR-ALL-MACHINE
              MOVE GR-ALL-PATH             TO GR-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
      *- - - -{ DO NOT KNOW WHERE THIS WOULD BE USED, }- - - - - - - - -
      *- - - -{ BUT STAYING CONSISTENT  }- - - - - - - - - - - - - - - -
           IF ( GR-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO GR-ALL-BASE
              MOVE FILPATH-MACHINE         TO GR-ALL-MACHINE
              MOVE GR-ALL-PATH             TO GR-PATH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-GRFILE NOT = "S" )
              MOVE GR-PATH-BEG             TO GR-SELECT-PATH-BEG
              MOVE GR-PATH-FILE            TO GR-SELECT-PATH-FILE
           ELSE
              MOVE GR-PATH-SQL             TO GR-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-GR1-FILE.
           PERFORM LOAD-GR1-FILE.
           PERFORM OPEN-IT.
           MOVE GR-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-GR1-FILE.
           PERFORM START-IT.
           MOVE GR-SELECT-PATH TO E-FILE.
           MOVE GR1-KEY TO E-KEYX.

           IF ( GR1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-GR1-FILE.

           IF ( QGR1-WEND-ID = SPACES    )
              MOVE GR-ID                TO QGR1-WBEG-ID
              MOVE ALL "Z"              TO QGR1-WEND-ID
              CALL "C$TOLOWER" USING QGR1-WEND-ID, VALUE 4.

           EXEC SQL
            DECLARE GR1_CURSOR CURSOR FOR
            SELECT ISNULL(GRFILE.GR_ID,''),
                   ISNULL(GRFILE.GR_NAME,''),
                   GRFILE.GR_LEVEL,
                   ISNULL(GRFILE.GR_MEMBER_ID_1,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_2,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_3,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_4,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_5,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_6,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_7,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_8,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_9,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_10,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_11,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_12,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_13,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_14,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_15,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_16,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_17,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_18,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_19,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_20,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_21,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_22,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_23,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_24,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_25,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_26,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_27,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_28,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_29,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_30,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_31,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_32,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_33,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_34,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_35,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_36,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_37,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_38,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_39,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_40,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_41,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_42,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_43,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_44,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_45,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_46,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_47,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_48,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_49,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_50,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_51,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_52,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_53,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_54,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_55,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_56,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_57,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_58,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_59,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_60,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_61,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_62,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_63,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_64,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_65,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_66,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_67,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_68,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_69,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_70,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_71,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_72,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_73,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_74,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_75,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_76,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_77,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_78,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_79,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_80,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_81,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_82,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_83,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_84,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_85,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_86,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_87,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_88,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_89,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_90,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_91,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_92,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_93,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_94,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_95,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_96,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_97,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_98,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_99,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_100,''),
                   ISNULL(GRFILE.GR_CONTACT,''),
                   ISNULL(GRFILE.GR_ADDR_1,''),
                   ISNULL(GRFILE.GR_ADDR_2,''),
                   ISNULL(GRFILE.GR_CITY,''),
                   ISNULL(GRFILE.GR_STATE,''),
                   ISNULL(GRFILE.GR_ZIP,''),
                   GRFILE.GR_PHONE,
                   GRFILE.GR_HOME_BRANCH,
                   GRFILE.GR_LN_APPROVAL_LIMIT,
                   ISNULL(CAST(GRFILE.GR_LTOUCH_DATE
                                                   AS VARCHAR(10)),'0')
            FROM DBO.GRFILE
            WHERE GRFILE.GR_ID >= :QGR1-WBEG-ID
              AND GRFILE.GR_ID <= :QGR1-WEND-ID
            ORDER BY GRFILE.GR_ID
           END-EXEC.

           EXEC SQL
               OPEN GR1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QGR1-WBEG-ID
                          QGR1-WEND-ID.

           MOVE STAT---GOOD TO GR1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-GR1-FILE.
           PERFORM READ-IT.
           MOVE GR-SELECT-PATH TO E-FILE.
           MOVE GR1-KEY TO E-KEYX.

           MOVE GR-ID TO QGR-ID.

           EXEC SQL
            SELECT ISNULL(GRFILE.GR_ID,''),
                   ISNULL(GRFILE.GR_NAME,''),
                   GRFILE.GR_LEVEL,
                   ISNULL(GRFILE.GR_MEMBER_ID_1,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_2,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_3,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_4,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_5,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_6,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_7,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_8,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_9,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_10,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_11,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_12,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_13,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_14,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_15,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_16,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_17,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_18,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_19,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_20,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_21,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_22,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_23,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_24,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_25,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_26,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_27,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_28,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_29,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_30,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_31,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_32,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_33,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_34,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_35,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_36,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_37,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_38,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_39,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_40,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_41,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_42,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_43,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_44,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_45,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_46,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_47,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_48,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_49,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_50,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_51,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_52,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_53,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_54,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_55,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_56,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_57,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_58,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_59,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_60,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_61,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_62,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_63,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_64,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_65,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_66,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_67,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_68,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_69,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_70,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_71,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_72,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_73,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_74,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_75,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_76,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_77,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_78,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_79,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_80,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_81,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_82,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_83,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_84,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_85,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_86,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_87,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_88,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_89,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_90,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_91,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_92,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_93,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_94,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_95,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_96,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_97,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_98,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_99,''),
                   ISNULL(GRFILE.GR_MEMBER_ID_100,''),
                   ISNULL(GRFILE.GR_CONTACT,''),
                   ISNULL(GRFILE.GR_ADDR_1,''),
                   ISNULL(GRFILE.GR_ADDR_2,''),
                   ISNULL(GRFILE.GR_CITY,''),
                   ISNULL(GRFILE.GR_STATE,''),
                   ISNULL(GRFILE.GR_ZIP,''),
                   GRFILE.GR_PHONE,
                   GRFILE.GR_HOME_BRANCH,
                   GRFILE.GR_LN_APPROVAL_LIMIT,
                   ISNULL(CAST(GRFILE.GR_LTOUCH_DATE
                                                   AS VARCHAR(10)),'0')
            INTO :QGR-REC
            FROM DBO.GRFILE
            WHERE GRFILE.GR_ID = :QGR-ID
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO READ-GR1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-GR-FIELDS.

      *-----------------------------------------------------------------
       READ-GR1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE GR-SELECT-PATH TO E-FILE.
           MOVE GR1-KEY TO E-KEYX.
           INITIALIZE QGR-REC.

           IF ( GR1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH GR1_CURSOR INTO :QGR-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO READ-GR1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-GR-FIELDS.

      *-----------------------------------------------------------------
       WRITE-GR1-FILE.

           ACCEPT GR-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE GR-SELECT-PATH TO E-FILE.
           MOVE GR1-KEY TO E-KEYX.
           PERFORM SET-GR-FIELDS.

           EXEC SQL
            INSERT INTO DBO.GRFILE
             (GRFILE.GR_ID,
              GRFILE.GR_NAME,
              GRFILE.GR_LEVEL,
              GRFILE.GR_MEMBER_ID_1,
              GRFILE.GR_MEMBER_ID_2,
              GRFILE.GR_MEMBER_ID_3,
              GRFILE.GR_MEMBER_ID_4,
              GRFILE.GR_MEMBER_ID_5,
              GRFILE.GR_MEMBER_ID_6,
              GRFILE.GR_MEMBER_ID_7,
              GRFILE.GR_MEMBER_ID_8,
              GRFILE.GR_MEMBER_ID_9,
              GRFILE.GR_MEMBER_ID_10,
              GRFILE.GR_MEMBER_ID_11,
              GRFILE.GR_MEMBER_ID_12,
              GRFILE.GR_MEMBER_ID_13,
              GRFILE.GR_MEMBER_ID_14,
              GRFILE.GR_MEMBER_ID_15,
              GRFILE.GR_MEMBER_ID_16,
              GRFILE.GR_MEMBER_ID_17,
              GRFILE.GR_MEMBER_ID_18,
              GRFILE.GR_MEMBER_ID_19,
              GRFILE.GR_MEMBER_ID_20,
              GRFILE.GR_MEMBER_ID_21,
              GRFILE.GR_MEMBER_ID_22,
              GRFILE.GR_MEMBER_ID_23,
              GRFILE.GR_MEMBER_ID_24,
              GRFILE.GR_MEMBER_ID_25,
              GRFILE.GR_MEMBER_ID_26,
              GRFILE.GR_MEMBER_ID_27,
              GRFILE.GR_MEMBER_ID_28,
              GRFILE.GR_MEMBER_ID_29,
              GRFILE.GR_MEMBER_ID_30,
              GRFILE.GR_MEMBER_ID_31,
              GRFILE.GR_MEMBER_ID_32,
              GRFILE.GR_MEMBER_ID_33,
              GRFILE.GR_MEMBER_ID_34,
              GRFILE.GR_MEMBER_ID_35,
              GRFILE.GR_MEMBER_ID_36,
              GRFILE.GR_MEMBER_ID_37,
              GRFILE.GR_MEMBER_ID_38,
              GRFILE.GR_MEMBER_ID_39,
              GRFILE.GR_MEMBER_ID_40,
              GRFILE.GR_MEMBER_ID_41,
              GRFILE.GR_MEMBER_ID_42,
              GRFILE.GR_MEMBER_ID_43,
              GRFILE.GR_MEMBER_ID_44,
              GRFILE.GR_MEMBER_ID_45,
              GRFILE.GR_MEMBER_ID_46,
              GRFILE.GR_MEMBER_ID_47,
              GRFILE.GR_MEMBER_ID_48,
              GRFILE.GR_MEMBER_ID_49,
              GRFILE.GR_MEMBER_ID_50,
              GRFILE.GR_MEMBER_ID_51,
              GRFILE.GR_MEMBER_ID_52,
              GRFILE.GR_MEMBER_ID_53,
              GRFILE.GR_MEMBER_ID_54,
              GRFILE.GR_MEMBER_ID_55,
              GRFILE.GR_MEMBER_ID_56,
              GRFILE.GR_MEMBER_ID_57,
              GRFILE.GR_MEMBER_ID_58,
              GRFILE.GR_MEMBER_ID_59,
              GRFILE.GR_MEMBER_ID_60,
              GRFILE.GR_MEMBER_ID_61,
              GRFILE.GR_MEMBER_ID_62,
              GRFILE.GR_MEMBER_ID_63,
              GRFILE.GR_MEMBER_ID_64,
              GRFILE.GR_MEMBER_ID_65,
              GRFILE.GR_MEMBER_ID_66,
              GRFILE.GR_MEMBER_ID_67,
              GRFILE.GR_MEMBER_ID_68,
              GRFILE.GR_MEMBER_ID_69,
              GRFILE.GR_MEMBER_ID_70,
              GRFILE.GR_MEMBER_ID_71,
              GRFILE.GR_MEMBER_ID_72,
              GRFILE.GR_MEMBER_ID_73,
              GRFILE.GR_MEMBER_ID_74,
              GRFILE.GR_MEMBER_ID_75,
              GRFILE.GR_MEMBER_ID_76,
              GRFILE.GR_MEMBER_ID_77,
              GRFILE.GR_MEMBER_ID_78,
              GRFILE.GR_MEMBER_ID_79,
              GRFILE.GR_MEMBER_ID_80,
              GRFILE.GR_MEMBER_ID_81,
              GRFILE.GR_MEMBER_ID_82,
              GRFILE.GR_MEMBER_ID_83,
              GRFILE.GR_MEMBER_ID_84,
              GRFILE.GR_MEMBER_ID_85,
              GRFILE.GR_MEMBER_ID_86,
              GRFILE.GR_MEMBER_ID_87,
              GRFILE.GR_MEMBER_ID_88,
              GRFILE.GR_MEMBER_ID_89,
              GRFILE.GR_MEMBER_ID_90,
              GRFILE.GR_MEMBER_ID_91,
              GRFILE.GR_MEMBER_ID_92,
              GRFILE.GR_MEMBER_ID_93,
              GRFILE.GR_MEMBER_ID_94,
              GRFILE.GR_MEMBER_ID_95,
              GRFILE.GR_MEMBER_ID_96,
              GRFILE.GR_MEMBER_ID_97,
              GRFILE.GR_MEMBER_ID_98,
              GRFILE.GR_MEMBER_ID_99,
              GRFILE.GR_MEMBER_ID_100,
              GRFILE.GR_CONTACT,
              GRFILE.GR_ADDR_1,
              GRFILE.GR_ADDR_2,
              GRFILE.GR_CITY,
              GRFILE.GR_STATE,
              GRFILE.GR_ZIP,
              GRFILE.GR_PHONE,
              GRFILE.GR_HOME_BRANCH,
              GRFILE.GR_LN_APPROVAL_LIMIT,
              GRFILE.GR_LTOUCH_DATE)
            VALUES
             (:QGR-ID,
              :QGR-NAME,
              :QGR-LEVEL,
              :QGR-MEMBER-ID-1,
              :QGR-MEMBER-ID-2,
              :QGR-MEMBER-ID-3,
              :QGR-MEMBER-ID-4,
              :QGR-MEMBER-ID-5,
              :QGR-MEMBER-ID-6,
              :QGR-MEMBER-ID-7,
              :QGR-MEMBER-ID-8,
              :QGR-MEMBER-ID-9,
              :QGR-MEMBER-ID-10,
              :QGR-MEMBER-ID-11,
              :QGR-MEMBER-ID-12,
              :QGR-MEMBER-ID-13,
              :QGR-MEMBER-ID-14,
              :QGR-MEMBER-ID-15,
              :QGR-MEMBER-ID-16,
              :QGR-MEMBER-ID-17,
              :QGR-MEMBER-ID-18,
              :QGR-MEMBER-ID-19,
              :QGR-MEMBER-ID-20,
              :QGR-MEMBER-ID-21,
              :QGR-MEMBER-ID-22,
              :QGR-MEMBER-ID-23,
              :QGR-MEMBER-ID-24,
              :QGR-MEMBER-ID-25,
              :QGR-MEMBER-ID-26,
              :QGR-MEMBER-ID-27,
              :QGR-MEMBER-ID-28,
              :QGR-MEMBER-ID-29,
              :QGR-MEMBER-ID-30,
              :QGR-MEMBER-ID-31,
              :QGR-MEMBER-ID-32,
              :QGR-MEMBER-ID-33,
              :QGR-MEMBER-ID-34,
              :QGR-MEMBER-ID-35,
              :QGR-MEMBER-ID-36,
              :QGR-MEMBER-ID-37,
              :QGR-MEMBER-ID-38,
              :QGR-MEMBER-ID-39,
              :QGR-MEMBER-ID-40,
              :QGR-MEMBER-ID-41,
              :QGR-MEMBER-ID-42,
              :QGR-MEMBER-ID-43,
              :QGR-MEMBER-ID-44,
              :QGR-MEMBER-ID-45,
              :QGR-MEMBER-ID-46,
              :QGR-MEMBER-ID-47,
              :QGR-MEMBER-ID-48,
              :QGR-MEMBER-ID-49,
              :QGR-MEMBER-ID-50,
              :QGR-MEMBER-ID-51,
              :QGR-MEMBER-ID-52,
              :QGR-MEMBER-ID-53,
              :QGR-MEMBER-ID-54,
              :QGR-MEMBER-ID-55,
              :QGR-MEMBER-ID-56,
              :QGR-MEMBER-ID-57,
              :QGR-MEMBER-ID-58,
              :QGR-MEMBER-ID-59,
              :QGR-MEMBER-ID-60,
              :QGR-MEMBER-ID-61,
              :QGR-MEMBER-ID-62,
              :QGR-MEMBER-ID-63,
              :QGR-MEMBER-ID-64,
              :QGR-MEMBER-ID-65,
              :QGR-MEMBER-ID-66,
              :QGR-MEMBER-ID-67,
              :QGR-MEMBER-ID-68,
              :QGR-MEMBER-ID-69,
              :QGR-MEMBER-ID-70,
              :QGR-MEMBER-ID-71,
              :QGR-MEMBER-ID-72,
              :QGR-MEMBER-ID-73,
              :QGR-MEMBER-ID-74,
              :QGR-MEMBER-ID-75,
              :QGR-MEMBER-ID-76,
              :QGR-MEMBER-ID-77,
              :QGR-MEMBER-ID-78,
              :QGR-MEMBER-ID-79,
              :QGR-MEMBER-ID-80,
              :QGR-MEMBER-ID-81,
              :QGR-MEMBER-ID-82,
              :QGR-MEMBER-ID-83,
              :QGR-MEMBER-ID-84,
              :QGR-MEMBER-ID-85,
              :QGR-MEMBER-ID-86,
              :QGR-MEMBER-ID-87,
              :QGR-MEMBER-ID-88,
              :QGR-MEMBER-ID-89,
              :QGR-MEMBER-ID-90,
              :QGR-MEMBER-ID-91,
              :QGR-MEMBER-ID-92,
              :QGR-MEMBER-ID-93,
              :QGR-MEMBER-ID-94,
              :QGR-MEMBER-ID-95,
              :QGR-MEMBER-ID-96,
              :QGR-MEMBER-ID-97,
              :QGR-MEMBER-ID-98,
              :QGR-MEMBER-ID-99,
              :QGR-MEMBER-ID-100,
              :QGR-CONTACT,
              :QGR-ADDR-1,
              :QGR-ADDR-2,
              :QGR-CITY,
              :QGR-STATE,
              :QGR-ZIP,
              :QGR-PHONE,
              :QGR-HOME-BRANCH,
              :QGR-LN-APPROVAL-LIMIT,
              NULLIF(:QGR-LTOUCH-DATE,'1900-01-01'))
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-GR1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-GR1-FILE.

           ACCEPT GR-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE GR-SELECT-PATH TO E-FILE.
           MOVE GR1-KEY TO E-KEYX.
           PERFORM SET-GR-FIELDS.

           EXEC SQL
            UPDATE DBO.GRFILE
             SET
              GRFILE.GR_ID = :QGR-ID,
              GRFILE.GR_NAME = :QGR-NAME,
              GRFILE.GR_LEVEL = :QGR-LEVEL,
              GRFILE.GR_MEMBER_ID_1 = :QGR-MEMBER-ID-1,
              GRFILE.GR_MEMBER_ID_2 = :QGR-MEMBER-ID-2,
              GRFILE.GR_MEMBER_ID_3 = :QGR-MEMBER-ID-3,
              GRFILE.GR_MEMBER_ID_4 = :QGR-MEMBER-ID-4,
              GRFILE.GR_MEMBER_ID_5 = :QGR-MEMBER-ID-5,
              GRFILE.GR_MEMBER_ID_6 = :QGR-MEMBER-ID-6,
              GRFILE.GR_MEMBER_ID_7 = :QGR-MEMBER-ID-7,
              GRFILE.GR_MEMBER_ID_8 = :QGR-MEMBER-ID-8,
              GRFILE.GR_MEMBER_ID_9 = :QGR-MEMBER-ID-9,
              GRFILE.GR_MEMBER_ID_10 = :QGR-MEMBER-ID-10,
              GRFILE.GR_MEMBER_ID_11 = :QGR-MEMBER-ID-11,
              GRFILE.GR_MEMBER_ID_12 = :QGR-MEMBER-ID-12,
              GRFILE.GR_MEMBER_ID_13 = :QGR-MEMBER-ID-13,
              GRFILE.GR_MEMBER_ID_14 = :QGR-MEMBER-ID-14,
              GRFILE.GR_MEMBER_ID_15 = :QGR-MEMBER-ID-15,
              GRFILE.GR_MEMBER_ID_16 = :QGR-MEMBER-ID-16,
              GRFILE.GR_MEMBER_ID_17 = :QGR-MEMBER-ID-17,
              GRFILE.GR_MEMBER_ID_18 = :QGR-MEMBER-ID-18,
              GRFILE.GR_MEMBER_ID_19 = :QGR-MEMBER-ID-19,
              GRFILE.GR_MEMBER_ID_20 = :QGR-MEMBER-ID-20,
              GRFILE.GR_MEMBER_ID_21 = :QGR-MEMBER-ID-21,
              GRFILE.GR_MEMBER_ID_22 = :QGR-MEMBER-ID-22,
              GRFILE.GR_MEMBER_ID_23 = :QGR-MEMBER-ID-23,
              GRFILE.GR_MEMBER_ID_24 = :QGR-MEMBER-ID-24,
              GRFILE.GR_MEMBER_ID_25 = :QGR-MEMBER-ID-25,
              GRFILE.GR_MEMBER_ID_26 = :QGR-MEMBER-ID-26,
              GRFILE.GR_MEMBER_ID_27 = :QGR-MEMBER-ID-27,
              GRFILE.GR_MEMBER_ID_28 = :QGR-MEMBER-ID-28,
              GRFILE.GR_MEMBER_ID_29 = :QGR-MEMBER-ID-29,
              GRFILE.GR_MEMBER_ID_30 = :QGR-MEMBER-ID-30,
              GRFILE.GR_MEMBER_ID_31 = :QGR-MEMBER-ID-31,
              GRFILE.GR_MEMBER_ID_32 = :QGR-MEMBER-ID-32,
              GRFILE.GR_MEMBER_ID_33 = :QGR-MEMBER-ID-33,
              GRFILE.GR_MEMBER_ID_34 = :QGR-MEMBER-ID-34,
              GRFILE.GR_MEMBER_ID_35 = :QGR-MEMBER-ID-35,
              GRFILE.GR_MEMBER_ID_36 = :QGR-MEMBER-ID-36,
              GRFILE.GR_MEMBER_ID_37 = :QGR-MEMBER-ID-37,
              GRFILE.GR_MEMBER_ID_38 = :QGR-MEMBER-ID-38,
              GRFILE.GR_MEMBER_ID_39 = :QGR-MEMBER-ID-39,
              GRFILE.GR_MEMBER_ID_40 = :QGR-MEMBER-ID-40,
              GRFILE.GR_MEMBER_ID_41 = :QGR-MEMBER-ID-41,
              GRFILE.GR_MEMBER_ID_42 = :QGR-MEMBER-ID-42,
              GRFILE.GR_MEMBER_ID_43 = :QGR-MEMBER-ID-43,
              GRFILE.GR_MEMBER_ID_44 = :QGR-MEMBER-ID-44,
              GRFILE.GR_MEMBER_ID_45 = :QGR-MEMBER-ID-45,
              GRFILE.GR_MEMBER_ID_46 = :QGR-MEMBER-ID-46,
              GRFILE.GR_MEMBER_ID_47 = :QGR-MEMBER-ID-47,
              GRFILE.GR_MEMBER_ID_48 = :QGR-MEMBER-ID-48,
              GRFILE.GR_MEMBER_ID_49 = :QGR-MEMBER-ID-49,
              GRFILE.GR_MEMBER_ID_50 = :QGR-MEMBER-ID-50,
              GRFILE.GR_MEMBER_ID_51 = :QGR-MEMBER-ID-51,
              GRFILE.GR_MEMBER_ID_52 = :QGR-MEMBER-ID-52,
              GRFILE.GR_MEMBER_ID_53 = :QGR-MEMBER-ID-53,
              GRFILE.GR_MEMBER_ID_54 = :QGR-MEMBER-ID-54,
              GRFILE.GR_MEMBER_ID_55 = :QGR-MEMBER-ID-55,
              GRFILE.GR_MEMBER_ID_56 = :QGR-MEMBER-ID-56,
              GRFILE.GR_MEMBER_ID_57 = :QGR-MEMBER-ID-57,
              GRFILE.GR_MEMBER_ID_58 = :QGR-MEMBER-ID-58,
              GRFILE.GR_MEMBER_ID_59 = :QGR-MEMBER-ID-59,
              GRFILE.GR_MEMBER_ID_60 = :QGR-MEMBER-ID-60,
              GRFILE.GR_MEMBER_ID_61 = :QGR-MEMBER-ID-61,
              GRFILE.GR_MEMBER_ID_62 = :QGR-MEMBER-ID-62,
              GRFILE.GR_MEMBER_ID_63 = :QGR-MEMBER-ID-63,
              GRFILE.GR_MEMBER_ID_64 = :QGR-MEMBER-ID-64,
              GRFILE.GR_MEMBER_ID_65 = :QGR-MEMBER-ID-65,
              GRFILE.GR_MEMBER_ID_66 = :QGR-MEMBER-ID-66,
              GRFILE.GR_MEMBER_ID_67 = :QGR-MEMBER-ID-67,
              GRFILE.GR_MEMBER_ID_68 = :QGR-MEMBER-ID-68,
              GRFILE.GR_MEMBER_ID_69 = :QGR-MEMBER-ID-69,
              GRFILE.GR_MEMBER_ID_70 = :QGR-MEMBER-ID-70,
              GRFILE.GR_MEMBER_ID_71 = :QGR-MEMBER-ID-71,
              GRFILE.GR_MEMBER_ID_72 = :QGR-MEMBER-ID-72,
              GRFILE.GR_MEMBER_ID_73 = :QGR-MEMBER-ID-73,
              GRFILE.GR_MEMBER_ID_74 = :QGR-MEMBER-ID-74,
              GRFILE.GR_MEMBER_ID_75 = :QGR-MEMBER-ID-75,
              GRFILE.GR_MEMBER_ID_76 = :QGR-MEMBER-ID-76,
              GRFILE.GR_MEMBER_ID_77 = :QGR-MEMBER-ID-77,
              GRFILE.GR_MEMBER_ID_78 = :QGR-MEMBER-ID-78,
              GRFILE.GR_MEMBER_ID_79 = :QGR-MEMBER-ID-79,
              GRFILE.GR_MEMBER_ID_80 = :QGR-MEMBER-ID-80,
              GRFILE.GR_MEMBER_ID_81 = :QGR-MEMBER-ID-81,
              GRFILE.GR_MEMBER_ID_82 = :QGR-MEMBER-ID-82,
              GRFILE.GR_MEMBER_ID_83 = :QGR-MEMBER-ID-83,
              GRFILE.GR_MEMBER_ID_84 = :QGR-MEMBER-ID-84,
              GRFILE.GR_MEMBER_ID_85 = :QGR-MEMBER-ID-85,
              GRFILE.GR_MEMBER_ID_86 = :QGR-MEMBER-ID-86,
              GRFILE.GR_MEMBER_ID_87 = :QGR-MEMBER-ID-87,
              GRFILE.GR_MEMBER_ID_88 = :QGR-MEMBER-ID-88,
              GRFILE.GR_MEMBER_ID_89 = :QGR-MEMBER-ID-89,
              GRFILE.GR_MEMBER_ID_90 = :QGR-MEMBER-ID-90,
              GRFILE.GR_MEMBER_ID_91 = :QGR-MEMBER-ID-91,
              GRFILE.GR_MEMBER_ID_92 = :QGR-MEMBER-ID-92,
              GRFILE.GR_MEMBER_ID_93 = :QGR-MEMBER-ID-93,
              GRFILE.GR_MEMBER_ID_94 = :QGR-MEMBER-ID-94,
              GRFILE.GR_MEMBER_ID_95 = :QGR-MEMBER-ID-95,
              GRFILE.GR_MEMBER_ID_96 = :QGR-MEMBER-ID-96,
              GRFILE.GR_MEMBER_ID_97 = :QGR-MEMBER-ID-97,
              GRFILE.GR_MEMBER_ID_98 = :QGR-MEMBER-ID-98,
              GRFILE.GR_MEMBER_ID_99 = :QGR-MEMBER-ID-99,
              GRFILE.GR_MEMBER_ID_100 = :QGR-MEMBER-ID-100,
              GRFILE.GR_CONTACT = :QGR-CONTACT,
              GRFILE.GR_ADDR_1 = :QGR-ADDR-1,
              GRFILE.GR_ADDR_2 = :QGR-ADDR-2,
              GRFILE.GR_CITY = :QGR-CITY,
              GRFILE.GR_STATE = :QGR-STATE,
              GRFILE.GR_ZIP = :QGR-ZIP,
              GRFILE.GR_PHONE = :QGR-PHONE,
              GRFILE.GR_HOME_BRANCH = :QGR-HOME-BRANCH,
              GRFILE.GR_LN_APPROVAL_LIMIT =
                   :QGR-LN-APPROVAL-LIMIT,
              GRFILE.GR_LTOUCH_DATE =
                 NULLIF(:QGR-LTOUCH-DATE,'1900-01-01')
             WHERE GRFILE.GR_ID = :QGR-ID
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-GR1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-GR1-FILE.

           PERFORM DELETE-IT.
           MOVE GR-SELECT-PATH TO E-FILE.
           MOVE GR1-KEY TO E-KEYX.
           PERFORM SET-GR-FIELDS.

           EXEC SQL
            DELETE FROM DBO.GRFILE
             WHERE GRFILE.GR_ID = :QGR-ID
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-GR1-FILE.

      *-----------------------------------------------------------------
       CLOSE-GR1-FILE.
           IF ( GR1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE GR1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO GR1-CURSOR-STAT.
 
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

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

      *-----------------------------------------------------------------
       STOP-RUN.

           IF ( EXT-ROUTE-BUF = "00" OR "40" OR "50" )
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY IOGRIN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
