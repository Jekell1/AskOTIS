       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOGTIN.
       AUTHOR.        BAH.
       DATE-WRITTEN.  03-24-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOGTIN
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : GTFILE
      * TABLE DESC    : G/L TRANSACTION FILE
      * OUTPUT FILE   : XPC/IOGTIN.PRE
      * USED FOR      : LIBGL/GLWSGT.CPY  --- QGT-REC
      *                 LIBGL/GLGT1I.CPY  --- READ
      *                 LIBGL/GLGT1IN.CPY --- READ/READ-NEXT/START/WRITE
      *                 LIBGL/GLGT2IN.CPY --- READ/READ-NEXT/START/WRITE
      *
      *=================================================================
      * REV:
      * BAH 2021-0324 ***NEW***
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
           "@(#)IO/IOGTIN.CBL S34 10/25/21-13:43:08 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGL/GL01GT.CPY".
       COPY "LIBGL/GLWSGT.CPY".

      *=================================================================
      *
      *    Q - F I L E
      *
      *-----------------------------------------------------------------
      *    FOR UPDATING THE FILE'S WS COPY MEMBER: LIB??/??WS??.CPY
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QGT1-WHERE-BEG-KEY.
           03  QGT1-WBEG-SOURCE            PIC 9.
           03  QGT1-WBEG-DATE2             PIC X(10).
           03  QGT1-WBEG-BRANCH            PIC 9(4).
           03  QGT1-WBEG-ACCTNO            PIC 9(8).
           03  QGT1-WBEG-DATE3             PIC X(10).
           03  QGT1-WBEG-DRCRFG            PIC X.
           03  QGT1-WBEG-LINE              PIC 9(6).
 
       01  QGT1-WHERE-END-KEY.
           03  QGT1-WEND-SOURCE            PIC 9.
           03  QGT1-WEND-DATE2             PIC X(10).
           03  QGT1-WEND-BRANCH            PIC 9(4).
           03  QGT1-WEND-ACCTNO            PIC 9(8).
           03  QGT1-WEND-DATE3             PIC X(10).
           03  QGT1-WEND-DRCRFG            PIC X.
           03  QGT1-WEND-LINE              PIC 9(6).

       01  QGT2-WHERE-BEG-KEY.
           03  QGT2-WBEG-DATE1             PIC X(10).
 
       01  QGT2-WHERE-END-KEY.
           03  QGT2-WEND-DATE1             PIC X(10).

       01  QGT-REC.
           03  QGT-FLAG                         PIC X.
           03  QGT-REF                          PIC X(14).
           03  QGT-DATE1                        PIC X(10).
           03  QGT-SOURCE                       PIC 9.
           03  QGT-DATE2                        PIC X(10).
           03  QGT-BRANCH                       PIC 9(4).
           03  QGT-ACCTNO                       PIC 9(8).
           03  QGT-DATE3                        PIC X(10).
           03  QGT-DRCRFG                       PIC X.
           03  QGT-LINE                         PIC 9(6).
           03  QGT-LTOUCH-DATE                  PIC X(10).
           03  QGT-AMT                          PIC S9(9)V99.
           03  QGT-EXPL                         PIC X(32).
           03  QGT-PERIOD                       PIC 99.
           03  QGT-SMITH-SENT                   PIC X.
           03  QGT-POST-USERID                  PIC X(10).
           03  QGT-POST-DATE                    PIC X(10).
           03  QGT-POST-TIME                    PIC 9(4).

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

       01  LEGEND-BEG-DATE. 
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING DATE       (F3-ALL) ".

       01  LEGEND-END-DATE. 
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING DATE      ".

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
           03  BEG-DATE1              PIC 9(08).
           03  END-DATE1              PIC 9(08).
           03  BEG-SOURCE             PIC 9.
           03  END-SOURCE             PIC 9.
           03  BEG-DATE2              PIC 9(08).
           03  END-DATE2              PIC 9(08).
           03  BEG-BRANCH             PIC 9(04).
           03  END-BRANCH             PIC 9(04).
           03  BEG-ACCTNO             PIC 9(08).
           03  END-ACCTNO             PIC 9(08).
           03  BEG-DATE3              PIC 9(08).
           03  END-DATE3              PIC 9(08).
           03  BEG-DRCRFG             PIC X.
           03  END-DRCRFG             PIC X.
           03  BEG-LINE               PIC 9(06).
           03  END-LINE               PIC 9(06).


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
           03  FILLER PIC X(30) VALUE "DATE1       SR   DATE2        ".
           03  FILLER PIC X(30) VALUE "BRNO   ACCOUNT    DATE3       ".
           03  FILLER PIC X(30) VALUE "DC   LINE        EXPLANATION".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(12) VALUE SPACES.

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-GT-DATE1                   PIC 9999/99/99.
           03  FILLER                         PIC X(03).
           03  D01-GT-SOURCE                  PIC 9.
           03  FILLER                         PIC X(03).
           03  D01-GT-DATE2                   PIC 9999/99/99.
           03  FILLER                         PIC X(03).
           03  D01-GT-BRANCH                  PIC 9(4).
           03  FILLER                         PIC X(03).
           03  D01-GT-ACCTNO                  PIC 9(8).
           03  FILLER                         PIC X(03).
           03  D01-GT-DATE3                   PIC 9999/99/99.
           03  FILLER                         PIC X(03).
           03  D01-GT-DRCRFG                  PIC X.
           03  FILLER                         PIC X(03).
           03  D01-GT-LINE                    PIC 9(6).
           03  FILLER                         PIC X(03).
           03  D01-GT-AMT                     PIC Z(8)9.99-.
           03  FILLER                         PIC X(03).
           03  D01-GT-EXPL                    PIC X(10).

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
       COPY "LIBIO/IOGTIN_DEF.CPY".
       COPY "LIBIO/IOGTIN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOGTIN_SCN.CPY".

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
           MOVE "B"                  TO GT-PATH-OVERRIDE.

           MOVE BEG-DATE1            TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QGT2-WBEG-DATE1.
           MOVE BEG-SOURCE           TO QGT1-WBEG-SOURCE.
           MOVE BEG-DATE2            TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WBEG-DATE2.
           MOVE BEG-BRANCH           TO QGT1-WBEG-BRANCH.
           MOVE BEG-ACCTNO           TO QGT1-WBEG-ACCTNO.
           MOVE BEG-DATE3            TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WBEG-DATE3.
           MOVE BEG-DRCRFG           TO QGT1-WBEG-DRCRFG.
           MOVE BEG-LINE             TO QGT1-WBEG-LINE.
           

           MOVE END-DATE1            TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QGT2-WEND-DATE1.
           MOVE END-SOURCE           TO QGT1-WEND-SOURCE.
           MOVE END-DATE2            TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WEND-DATE2.
           MOVE END-BRANCH           TO QGT1-WEND-BRANCH.
           MOVE END-ACCTNO           TO QGT1-WEND-ACCTNO.
           MOVE END-DATE3            TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WEND-DATE3.
           MOVE END-DRCRFG           TO QGT1-WEND-DRCRFG.
           MOVE END-LINE             TO QGT1-WEND-LINE.
           

           IF ( PROCESS-KEY = 1 )
              PERFORM OPEN-GT1-FILE
              PERFORM START-GT1-FILE
           ELSE
           IF ( PROCESS-KEY = 2 )
              PERFORM OPEN-GT2-FILE
              PERFORM START-GT2-FILE
           ELSE
      * KEY READ TEST
           IF ( PROCESS-KEY = 9 )
              PERFORM OPEN-GT1-FILE.

      *=================================================================
       MAIN-PROGRAM-NEXT.

           ADD 01 TO RECORDS-READ.

           IF ( PROCESS-KEY = 1 )
              PERFORM READ-GT1-FILE-NEXT
           ELSE
           IF ( PROCESS-KEY = 2 )
              PERFORM READ-GT2-FILE-NEXT
           ELSE
      * KEY READ
           IF (PROCESS-KEY = 9)
               MOVE BEG-SOURCE TO GT-SOURCE
               MOVE BEG-DATE2  TO GT-DATE2
               MOVE BEG-BRANCH TO GT-BRANCH
               MOVE BEG-ACCTNO TO GT-ACCTNO
               MOVE BEG-DATE3  TO GT-DATE3
               MOVE BEG-DRCRFG TO GT-DRCRFG
               MOVE BEG-LINE   TO GT-LINE
               PERFORM READ-GT1-FILE.
           IF ( IO-BAD )
              GO TO MAIN-PROGRAM-END.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( PROCESS-KEY = 1 OR 9)
              IF ( GT-SOURCE < BEG-SOURCE )
                 GO TO MAIN-PROGRAM-NEXT
              ELSE
              IF ( GT-SOURCE > END-SOURCE )
                 GO TO MAIN-PROGRAM-END.
              
           IF ( PROCESS-KEY = 2 )
              IF ( GT-DATE1 < BEG-DATE1 )
                 GO TO MAIN-PROGRAM-NEXT
              ELSE
              IF ( GT-DATE1 > END-DATE1 )
                 GO TO MAIN-PROGRAM-END.

           IF (PROCESS-KEY = 1)
              IF GT-DATE2 < BEG-DATE2 OR > END-DATE2
                GO TO MAIN-PROGRAM-NEXT.

           IF (PROCESS-KEY = 1)
              IF GT-BRANCH < BEG-BRANCH OR > END-BRANCH
                GO TO MAIN-PROGRAM-NEXT
              ELSE
              IF GT-ACCTNO < BEG-ACCTNO OR > END-ACCTNO
                GO TO MAIN-PROGRAM-NEXT
              ELSE
              IF GT-DATE3 < BEG-DATE3 OR > END-DATE3
                GO TO MAIN-PROGRAM-NEXT
              ELSE
              IF GT-DRCRFG < BEG-DRCRFG OR > END-DRCRFG
                GO TO MAIN-PROGRAM-NEXT
              ELSE
              IF GT-LINE < BEG-LINE OR > END-LINE
                GO TO MAIN-PROGRAM-NEXT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *-------------------------------------------------------{ RANGES }
           MOVE GT-DATE1                      TO D01-GT-DATE1.
           MOVE GT-SOURCE                     TO D01-GT-SOURCE.
           MOVE GT-DATE2                      TO D01-GT-DATE2.
           MOVE GT-BRANCH                     TO D01-GT-BRANCH.
           MOVE GT-ACCTNO                     TO D01-GT-ACCTNO.
           MOVE GT-DATE3                      TO D01-GT-DATE3.
           MOVE GT-DRCRFG                     TO D01-GT-DRCRFG.
           MOVE GT-LINE                       TO D01-GT-LINE.
           MOVE GT-AMT                        TO D01-GT-AMT.
           MOVE GT-EXPL                       TO D01-GT-EXPL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "Y" TO RECORDS-FOUND.

      * KEY READ
           IF (PROCESS-KEY = 9)
              GO TO MAIN-PROGRAM-END.


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
           MOVE "IOGTIN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOGTIN-QUEUE-POS  TO Q-POS.
           MOVE IOGTIN-COPIES-POS TO C-POS.
           MOVE IOGTIN-PRU-POS    TO P-POS.
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
       ENTER-BEG-DATE1.

           MOVE LEGEND-BEG-DATE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ERNM060BEG-DATE1." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-DATE1.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-DATE1.

           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE1.

      *-----------------------------------------------------------------
       ENTER-END-DATE1.
           MOVE LEGEND-END-DATE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ERNM060END-DATE1." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-DATE1.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-DATE1.

           MOVE VDU-DATE-YYYYMMDD TO END-DATE1.

           IF ( BEG-DATE1 > END-DATE1 )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DATE1.

           GO TO ENTER-BEG-SOURCE.

      *-----------------------------------------------------------------
       ENTER-ALL-DATE1.

           MOVE "S  8080BEG-DATE1." TO VDU.
           MOVE JULIAN-BEG         TO BEG-DATE1 VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "S  8080END-DATE1." TO VDU.
           MOVE JULIAN-END         TO END-DATE1 VDU-DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-SOURCE.

           MOVE SPACES  TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "EN N010BEG-SOURCE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-DATE1.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-SOURCE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-SOURCE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-SOURCE.

      *-----------------------------------------------------------------
       ENTER-END-SOURCE.


           MOVE "EN N010END-SOURCE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-SOURCE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-SOURCE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-SOURCE.

           IF ( BEG-SOURCE > END-SOURCE )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-SOURCE.

           GO TO ENTER-BEG-DATE2.

      *-----------------------------------------------------------------
       ENTER-ALL-SOURCE.

           MOVE "SN N010BEG-SOURCE." TO VDU.
           MOVE 0               TO BEG-SOURCE VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N010END-SOURCE." TO VDU.
           MOVE 9              TO END-SOURCE VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-DATE2.

           MOVE SPACES  TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ERNM060BEG-DATE2." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-SOURCE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-DATE2.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-DATE2.

           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE2.

      *-----------------------------------------------------------------
       ENTER-END-DATE2.

           MOVE "ERNM060END-DATE2." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-DATE2.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-DATE2.

           MOVE VDU-DATE-YYYYMMDD TO END-DATE2.

           IF ( BEG-DATE2 > END-DATE2 )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DATE2.

           GO TO ENTER-BEG-BRANCH.

      *-----------------------------------------------------------------
       ENTER-ALL-DATE2.

           MOVE "S  8080BEG-DATE2." TO VDU.
           MOVE JULIAN-BEG         TO BEG-DATE2 VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "S  8080END-DATE2." TO VDU.
           MOVE JULIAN-END         TO END-DATE2 VDU-DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-BRANCH.

           MOVE "EN N040BEG-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-DATE2.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-BRANCH.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-BRANCH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-BRANCH.

      *-----------------------------------------------------------------
       ENTER-END-BRANCH.


           MOVE "EN N040END-BRANCH." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-BRANCH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-BRANCH.

           IF ( BEG-BRANCH > END-BRANCH )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRANCH.

           GO TO ENTER-BEG-ACCTNO.

      *-----------------------------------------------------------------
       ENTER-ALL-BRANCH.

           MOVE "SN N040BEG-BRANCH." TO VDU.
           MOVE 0               TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N040END-BRANCH." TO VDU.
           MOVE 9999            TO END-SOURCE VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-ACCTNO.

           MOVE "EN N080BEG-ACCOUNT." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-ACCTNO.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-ACCTNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-ACCTNO.

      *-----------------------------------------------------------------
       ENTER-END-ACCTNO.


           MOVE "EN N080END-ACCOUNT." TO VDU.
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

           GO TO ENTER-BEG-DATE3.

      *-----------------------------------------------------------------
       ENTER-ALL-ACCTNO.

           MOVE "SN N080BEG-ACCOUNT." TO VDU.
           MOVE 0               TO BEG-ACCTNO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N080END-ACCOUNT." TO VDU.
           MOVE 99999999             TO END-ACCTNO VDUNUM0.
           PERFORM SCREENIO.

      *================================================================
       ENTER-BEG-DATE3.

           MOVE SPACES  TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ERNM060BEG-DATE3." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-ACCTNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-DATE3.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-DATE3.

           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE3.

      *-----------------------------------------------------------------
       ENTER-END-DATE3.

           MOVE "ERNM060END-DATE3." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-DATE3.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-DATE3.

           MOVE VDU-DATE-YYYYMMDD TO END-DATE3.

           IF ( BEG-DATE3 > END-DATE3 )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DATE3.

           GO TO ENTER-BEG-DRCRFG.

      *-----------------------------------------------------------------
       ENTER-ALL-DATE3.

           MOVE "S  8080BEG-DATE3." TO VDU.
           MOVE JULIAN-BEG         TO BEG-DATE3 VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "S  8080END-DATE3." TO VDU.
           MOVE JULIAN-END         TO END-DATE3 VDU-DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYYYY.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-DRCRFG.

           MOVE "E  A010BEG-DRCRFG." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-DATE3.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-DRCRFG.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-DRCRFG.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-DRCRFG.

      *-----------------------------------------------------------------
       ENTER-END-DRCRFG.


           MOVE "E  A010END-DRCRFG." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-DRCRFG.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-DRCRFG.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-DRCRFG.

           IF ( BEG-DRCRFG > END-DRCRFG )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DRCRFG.

           GO TO ENTER-BEG-LINE.

      *-----------------------------------------------------------------
       ENTER-ALL-DRCRFG.

           MOVE "S  A010BEG-DRCRFG." TO VDU.
           MOVE SPACES               TO BEG-DRCRFG VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010END-DRCRFG." TO VDU.
           MOVE "Z"                  TO END-DRCRFG VDUBUF.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-LINE.

           MOVE "EN N060BEG-LINE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-DRCRFG.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-LINE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-LINE.

      *-----------------------------------------------------------------
       ENTER-END-LINE.


           MOVE "EN N060END-LINE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-LINE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-LINE.

           IF ( BEG-LINE > END-LINE )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-LINE.

           GO TO ENTER-PROCESS-KEY.

      *-----------------------------------------------------------------
       ENTER-ALL-LINE.

           MOVE "SN N060BEG-LINE." TO VDU.
           MOVE 0               TO BEG-LINE VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N060END-LINE." TO VDU.
           MOVE 999999             TO END-LINE VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-PROCESS-KEY.

           MOVE LEGEND-PROCESS-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN010PROCESS-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-LINE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUNUM0 < ZEROES ) OR
              ( VDUNUM0 > 2      )    
      * KEY READ
                IF VDUNUM0 NOT = 9
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

           MOVE "S  A000BEG-DATE1."    TO VDU.
           MOVE BEG-DATE1              TO VDU-DATE-EDIT.
           PERFORM SCREENIO.
           MOVE "S  A000END-DATE1."    TO VDU.
           MOVE END-DATE1              TO VDU-DATE-EDIT.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN010BEG-SOURCE." TO VDU
           MOVE BEG-SOURCE           TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN010END-SOURCE." TO VDU
           MOVE BEG-SOURCE           TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A000BEG-DATE2."    TO VDU.
           MOVE BEG-DATE2              TO VDU-DATE-EDIT.
           PERFORM SCREENIO.
           MOVE "S  A000END-DATE2."    TO VDU.
           MOVE END-DATE2              TO VDU-DATE-EDIT.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN040BEG-BRANCH." TO VDU
           MOVE BEG-BRANCH           TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040END-BRANCH." TO VDU
           MOVE BEG-BRANCH           TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN080BEG-ACCOUNT." TO VDU
           MOVE BEG-ACCTNO           TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN080END-ACCOUNT." TO VDU
           MOVE BEG-ACCTNO           TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A000BEG-DATE3."    TO VDU.
           MOVE BEG-DATE3              TO VDU-DATE-EDIT.
           PERFORM SCREENIO.
           MOVE "S  A000END-DATE3."    TO VDU.
           MOVE END-DATE3              TO VDU-DATE-EDIT.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A000BEG-DRCRFG." TO VDU
           MOVE BEG-DRCRFG           TO VDUBUF.
           PERFORM SCREENIO.
           MOVE "S  A000END-DRCRFG." TO VDU
           MOVE BEG-DRCRFG           TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN060BEG-LINE." TO VDU
           MOVE BEG-LINE           TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN060END-LINE." TO VDU
           MOVE BEG-LINE           TO VDUNUM0.
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

       
           IF ( PROCESS-KEY = 1 )
              MOVE GT1-KEY TO PACE-DETAIL-KEY
           ELSE
           IF ( PROCESS-KEY = 2 )
              MOVE GT2-KEY TO PACE-DETAIL-KEY.

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
           MOVE "BEGINNING DATE1               : "    TO R01-PROMPT.
           MOVE BEG-DATE1                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING    DATE1               : "    TO R01-PROMPT.
           MOVE END-DATE1                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING SOURCE               : "    TO R01-PROMPT.
           MOVE BEG-SOURCE                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING    SOURCE               : "    TO R01-PROMPT.
           MOVE END-SOURCE                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING DATE2               : "    TO R01-PROMPT.
           MOVE BEG-DATE2                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING    DATE2               : "    TO R01-PROMPT.
           MOVE END-DATE2                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING BRANCH               : "    TO R01-PROMPT.
           MOVE BEG-BRANCH                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING    BRANCH               : "    TO R01-PROMPT.
           MOVE END-BRANCH                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING ACCTNO               : "    TO R01-PROMPT.
           MOVE BEG-ACCTNO                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING    ACCTNO               : "    TO R01-PROMPT.
           MOVE END-ACCTNO                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING DATE3               : "    TO R01-PROMPT.
           MOVE BEG-DATE3                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING    DATE3               : "    TO R01-PROMPT.
           MOVE END-DATE3                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING DRCRFG               : "    TO R01-PROMPT.
           MOVE BEG-DRCRFG                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING    DRCRFG               : "    TO R01-PROMPT.
           MOVE END-DRCRFG                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING LINE               : "    TO R01-PROMPT.
           MOVE BEG-LINE                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING    LINE               : "    TO R01-PROMPT.
           MOVE END-LINE                             TO R01-TEXT.
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
           MOVE "SQL TABLE IO UTILITY - GTFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOGTIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOGTIN-SYSDATE.
           DISPLAY IOGTIN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOGTIN-FORM-FIELDS TO WR-BUF.
           MOVE IOGTIN-FORM-LIST TO WR-LIST.
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

           MOVE IOGTIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOGTIN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOGTIN_EVA.CPY".
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
      *    LOAD GT-REC FROM QGT-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-GT-FIELDS.
           MOVE QGT-FLAG TO GT-FLAG.
           MOVE QGT-REF TO GT-REF.

           IF ( QGT-DATE1 = "1900-01-01" )
              MOVE ZEROES TO GT-DATE1
           ELSE
              MOVE QGT-DATE1 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GT-DATE1.

           MOVE QGT-SOURCE TO GT-SOURCE.

           IF ( QGT-DATE2 = "1900-01-01" )
              MOVE ZEROES TO GT-DATE2
           ELSE
              MOVE QGT-DATE2 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GT-DATE2.

           MOVE QGT-BRANCH TO GT-BRANCH.
           MOVE QGT-ACCTNO TO GT-ACCTNO.

           IF ( QGT-DATE3 = "1900-01-01" )
              MOVE ZEROES TO GT-DATE3
           ELSE
              MOVE QGT-DATE3 TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GT-DATE3.

           MOVE QGT-DRCRFG TO GT-DRCRFG.
           MOVE QGT-LINE TO GT-LINE.

           IF ( QGT-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO GT-LTOUCH-DATE
           ELSE
              MOVE QGT-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GT-LTOUCH-DATE.

           MOVE QGT-AMT TO GT-AMT.
           MOVE QGT-EXPL TO GT-EXPL.
           MOVE QGT-PERIOD TO GT-PERIOD.
           MOVE QGT-SMITH-SENT TO GT-SMITH-SENT.
           MOVE QGT-POST-USERID TO GT-POST-USERID.

           IF ( QGT-POST-DATE = "1900-01-01" )
              MOVE ZEROES TO GT-POST-DATE
           ELSE
              MOVE QGT-POST-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GT-POST-DATE.

           MOVE QGT-POST-TIME TO GT-POST-TIME.
 
      *-----------------------------------------------------------------
      *    LOAD QGT-REC FROM GT-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-GT-FIELDS.
           MOVE GT-FLAG TO QGT-FLAG.
           MOVE GT-REF TO QGT-REF.

           IF ( GT-DATE1 = ZEROES )
              MOVE "1900-01-01" TO QGT-DATE1
           ELSE
              MOVE GT-DATE1 TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGT-DATE1.

           MOVE GT-SOURCE TO QGT-SOURCE.

           IF ( GT-DATE2 = ZEROES )
              MOVE "1900-01-01" TO QGT-DATE2
           ELSE
              MOVE GT-DATE2 TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGT-DATE2.

           MOVE GT-BRANCH TO QGT-BRANCH.
           MOVE GT-ACCTNO TO QGT-ACCTNO.

           IF ( GT-DATE3 = ZEROES )
              MOVE "1900-01-01" TO QGT-DATE3
           ELSE
              MOVE GT-DATE3 TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGT-DATE3.

           MOVE GT-DRCRFG TO QGT-DRCRFG.
           MOVE GT-LINE TO QGT-LINE.

           IF ( GT-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QGT-LTOUCH-DATE
           ELSE
              MOVE GT-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGT-LTOUCH-DATE.

           MOVE GT-AMT TO QGT-AMT.
           MOVE GT-EXPL TO QGT-EXPL.
           MOVE GT-PERIOD TO QGT-PERIOD.
           MOVE GT-SMITH-SENT TO QGT-SMITH-SENT.
           MOVE GT-POST-USERID TO QGT-POST-USERID.

           IF ( GT-POST-DATE = ZEROES )
              MOVE "1900-01-01" TO QGT-POST-DATE
           ELSE
              MOVE GT-POST-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGT-POST-DATE.

           MOVE GT-POST-TIME TO QGT-POST-TIME.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
       LOAD-GT1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( GT-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( GT-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO GT-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO GT-ALL-MACHINE
              MOVE GT-ALL-PATH             TO GT-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( GT-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO GT-ALL-BASE
              MOVE FILPATH-MACHINE         TO GT-ALL-MACHINE
              MOVE GT-ALL-PATH             TO GT-PATH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-GTFILE NOT = "S" )
              MOVE GT-PATH-BEG             TO GT-SELECT-PATH-BEG
              MOVE GT-PATH-FILE            TO GT-SELECT-PATH-FILE
           ELSE
              MOVE GT-PATH-SQL             TO GT-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-GT1-FILE.
           PERFORM LOAD-GT1-FILE.
           PERFORM OPEN-IT.
           MOVE GT-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-GT1-FILE.
           PERFORM START-IT.
           MOVE GT-SELECT-PATH TO E-FILE.
           MOVE GT1-KEY TO E-KEYX.
           IF ( GT1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-GT1-FILE.

           IF ( QGT1-WEND-LINE = ZEROES    ) OR
              ( QGT1-WEND-LINE NOT NUMERIC )
              MOVE GT-SOURCE            TO QGT1-WBEG-SOURCE
              MOVE 9                    TO QGT1-WEND-SOURCE

              MOVE GT-DATE2             TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WBEG-DATE2
              MOVE GT-DATE3             TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WBEG-DATE3

              MOVE JULIAN-END           TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WEND-DATE2
                                           QGT1-WEND-DATE3

              MOVE GT-BRANCH            TO QGT1-WBEG-BRANCH
              MOVE ALL "9"              TO QGT1-WEND-BRANCH
              MOVE GT-ACCTNO            TO QGT1-WBEG-ACCTNO
              MOVE ALL "9"              TO QGT1-WEND-ACCTNO
              MOVE GT-DRCRFG            TO QGT1-WBEG-DRCRFG
              MOVE ALL "Z"              TO QGT1-WEND-DRCRFG
              MOVE GT-LINE              TO QGT1-WBEG-LINE
              MOVE ALL "9"              TO QGT1-WEND-LINE.

           EXEC SQL
            DECLARE GT1_CURSOR CURSOR FOR
            SELECT ISNULL(GTFILE.GT_FLAG,''),
                   ISNULL(GTFILE.GT_REF,''),
                   ISNULL(CAST(GTFILE.GT_DATE1 AS VARCHAR(10)),'0'),
                   GTFILE.GT_SOURCE,
                   ISNULL(CAST(GTFILE.GT_DATE2 AS VARCHAR(10)),'0'),
                   GTFILE.GT_BRANCH,
                   GTFILE.GT_ACCTNO,
                   ISNULL(CAST(GTFILE.GT_DATE3 AS VARCHAR(10)),'0'),
                   ISNULL(GTFILE.GT_DRCRFG,''),
                   GTFILE.GT_LINE,
                   ISNULL(CAST(GTFILE.GT_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   GTFILE.GT_AMT,
                   ISNULL(GTFILE.GT_EXPL,''),
                   GTFILE.GT_PERIOD,
                   ISNULL(GTFILE.GT_SMITH_SENT,''),
                   ISNULL(GTFILE.GT_POST_USERID,''),
                   ISNULL(CAST(GTFILE.GT_POST_DATE
                                                  AS VARCHAR(10)),'0'),
                   GTFILE.GT_POST_TIME
            FROM DBO.GTFILE
            WHERE GTFILE.GT_SOURCE >= :QGT1-WBEG-SOURCE
              AND GTFILE.GT_SOURCE <= :QGT1-WEND-SOURCE
              AND CAST(GTFILE.GT_DATE2 AS VARCHAR(10))
                           >= :QGT1-WBEG-DATE2
              AND CAST(GTFILE.GT_DATE2 AS VARCHAR(10))
                           <= :QGT1-WEND-DATE2
              AND GTFILE.GT_BRANCH >= :QGT1-WBEG-BRANCH
              AND GTFILE.GT_BRANCH <= :QGT1-WEND-BRANCH
              AND GTFILE.GT_ACCTNO >= :QGT1-WBEG-ACCTNO
              AND GTFILE.GT_ACCTNO <= :QGT1-WEND-ACCTNO
              AND CONCAT ( CAST(GTFILE.GT_DATE3 AS VARCHAR(10) ),
                          GTFILE.GT_DRCRFG,
                          GTFILE.GT_LINE)
                  >=
                  CONCAT (:QGT1-WBEG-DATE3,
                          :QGT1-WBEG-DRCRFG,
                          :QGT1-WBEG-LINE)
              AND CONCAT ( CAST(GTFILE.GT_DATE3 AS VARCHAR(10) ),
                          GTFILE.GT_DRCRFG,
                          GTFILE.GT_LINE)
                  <=
                  CONCAT (:QGT1-WEND-DATE3,
                          :QGT1-WEND-DRCRFG,
                          :QGT1-WEND-LINE)
            ORDER BY GTFILE.GT_SOURCE,
                     GTFILE.GT_DATE2,
                     GTFILE.GT_BRANCH,
                     GTFILE.GT_ACCTNO,
                     GTFILE.GT_DATE3,
                     GTFILE.GT_DRCRFG,
                     GTFILE.GT_LINE
           END-EXEC.

           EXEC SQL
               OPEN GT1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QGT1-WBEG-DRCRFG
                          QGT1-WEND-DRCRFG
                          QGT1-WBEG-DATE2
                          QGT1-WEND-DATE2
                          QGT1-WBEG-DATE3
                          QGT1-WEND-DATE3.


           MOVE ZEROES TO QGT1-WBEG-BRANCH
                          QGT1-WEND-BRANCH
                          QGT1-WBEG-ACCTNO
                          QGT1-WEND-ACCTNO
                          QGT1-WBEG-LINE
                          QGT1-WEND-LINE
                          QGT1-WBEG-SOURCE
                          QGT1-WEND-SOURCE.

           MOVE STAT---GOOD TO GT1-CURSOR-STAT.


      *-----------------------------------------------------------------
       START-GT1-FILE-GREATER.
           PERFORM START-IT.
           MOVE GT-SELECT-PATH TO E-FILE.
           MOVE GT1-KEY TO E-KEYX.

           IF ( GT1-GR-CURSOR-STAT-GOOD )
              PERFORM CLOSE-GT1-FILE.

           IF ( QGT1-WEND-LINE = ZEROES    ) OR
              ( QGT1-WEND-LINE NOT NUMERIC )
              MOVE GT-SOURCE            TO QGT1-WBEG-SOURCE
              MOVE 9                    TO QGT1-WEND-SOURCE

              MOVE GT-DATE2             TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WBEG-DATE2
              MOVE GT-DATE3             TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WBEG-DATE3

              MOVE JULIAN-END           TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WEND-DATE2
                                           QGT1-WEND-DATE3

              MOVE GT-BRANCH            TO QGT1-WBEG-BRANCH
              MOVE ALL "9"              TO QGT1-WEND-BRANCH
              MOVE GT-ACCTNO            TO QGT1-WBEG-ACCTNO
              MOVE ALL "9"              TO QGT1-WEND-ACCTNO
              MOVE GT-DRCRFG            TO QGT1-WBEG-DRCRFG
              MOVE ALL "Z"              TO QGT1-WEND-DRCRFG
              MOVE GT-LINE              TO QGT1-WBEG-LINE
              MOVE ALL "9"              TO QGT1-WEND-LINE.

           EXEC SQL
            DECLARE GT1_GR_CURSOR CURSOR FOR
            SELECT ISNULL(GTFILE.GT_FLAG,''),
                   ISNULL(GTFILE.GT_REF,''),
                   ISNULL(CAST(GTFILE.GT_DATE1 AS VARCHAR(10)),'0'),
                   GTFILE.GT_SOURCE,
                   ISNULL(CAST(GTFILE.GT_DATE2 AS VARCHAR(10)),'0'),
                   GTFILE.GT_BRANCH,
                   GTFILE.GT_ACCTNO,
                   ISNULL(CAST(GTFILE.GT_DATE3 AS VARCHAR(10)),'0'),
                   ISNULL(GTFILE.GT_DRCRFG,''),
                   GTFILE.GT_LINE,
                   ISNULL(CAST(GTFILE.GT_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   GTFILE.GT_AMT,
                   ISNULL(GTFILE.GT_EXPL,''),
                   GTFILE.GT_PERIOD,
                   ISNULL(GTFILE.GT_SMITH_SENT,''),
                   ISNULL(GTFILE.GT_POST_USERID,''),
                   ISNULL(CAST(GTFILE.GT_POST_DATE
                                                  AS VARCHAR(10)),'0'),
                   GTFILE.GT_POST_TIME
            FROM DBO.GTFILE
            WHERE GTFILE.GT_SOURCE > :QGT1-WBEG-SOURCE
              AND GTFILE.GT_SOURCE <= :QGT1-WEND-SOURCE
              AND CAST(GTFILE.GT_DATE2 AS VARCHAR(10))
                           > :QGT1-WBEG-DATE2
              AND CAST(GTFILE.GT_DATE2 AS VARCHAR(10))
                           <= :QGT1-WEND-DATE2
              AND GTFILE.GT_BRANCH > :QGT1-WBEG-BRANCH
              AND GTFILE.GT_BRANCH <= :QGT1-WEND-BRANCH
              AND GTFILE.GT_ACCTNO > :QGT1-WBEG-ACCTNO
              AND GTFILE.GT_ACCTNO <= :QGT1-WEND-ACCTNO
              AND CONCAT ( CAST(GTFILE.GT_DATE3 AS VARCHAR(10) ),
                          GTFILE.GT_DRCRFG,
                          GTFILE.GT_LINE)
                  >=
                  CONCAT (:QGT1-WBEG-DATE3,
                          :QGT1-WBEG-DRCRFG,
                          :QGT1-WBEG-LINE)
              AND CONCAT ( CAST(GTFILE.GT_DATE3 AS VARCHAR(10) ),
                          GTFILE.GT_DRCRFG,
                          GTFILE.GT_LINE)
                  <=
                  CONCAT (:QGT1-WEND-DATE3,
                          :QGT1-WEND-DRCRFG,
                          :QGT1-WEND-LINE)
            ORDER BY GTFILE.GT_SOURCE,
                     GTFILE.GT_DATE2,
                     GTFILE.GT_BRANCH,
                     GTFILE.GT_ACCTNO,
                     GTFILE.GT_DATE3,
                     GTFILE.GT_DRCRFG,
                     GTFILE.GT_LINE
           END-EXEC.

           EXEC SQL
               OPEN GT1_GR_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QGT1-WBEG-DRCRFG
                          QGT1-WEND-DRCRFG
                          QGT1-WBEG-DATE2
                          QGT1-WEND-DATE2
                          QGT1-WBEG-DATE3
                          QGT1-WEND-DATE3.


           MOVE ZEROES TO QGT1-WBEG-BRANCH
                          QGT1-WEND-BRANCH
                          QGT1-WBEG-ACCTNO
                          QGT1-WEND-ACCTNO
                          QGT1-WBEG-LINE
                          QGT1-WEND-LINE
                          QGT1-WBEG-SOURCE
                          QGT1-WEND-SOURCE.

           MOVE STAT---GOOD TO GT1-GR-CURSOR-STAT.


      *-----------------------------------------------------------------
       START-GT1-FILE-NOT-GREATER.
           PERFORM START-IT.
           MOVE GT-SELECT-PATH TO E-FILE.
           MOVE GT1-KEY TO E-KEYX.

           IF ( GT1-NG-CURSOR-STAT-GOOD )
              PERFORM CLOSE-GT1-FILE.

           IF ( QGT1-WEND-LINE = ZEROES    ) OR
              ( QGT1-WEND-LINE NOT NUMERIC )
              MOVE 0                    TO QGT1-WBEG-SOURCE
              MOVE GT-SOURCE            TO QGT1-WEND-SOURCE

              MOVE JULIAN-BEG           TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WBEG-DATE2
                                           QGT1-WBEG-DATE3
              MOVE GT-DATE2             TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WEND-DATE2
              MOVE GT-DATE3             TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGT1-WEND-DATE3

              MOVE ALL "0"              TO QGT1-WBEG-BRANCH
              MOVE GT-BRANCH            TO QGT1-WEND-BRANCH
              MOVE ALL "0"              TO QGT1-WBEG-ACCTNO
              MOVE GT-ACCTNO            TO QGT1-WEND-ACCTNO
              MOVE ALL "A"              TO QGT1-WBEG-DRCRFG
              MOVE GT-DRCRFG            TO QGT1-WEND-DRCRFG
              MOVE ALL "0"              TO QGT1-WBEG-LINE
              MOVE GT-LINE              TO QGT1-WEND-LINE.

           EXEC SQL
            DECLARE GT1_NG_CURSOR CURSOR FOR
            SELECT ISNULL(GTFILE.GT_FLAG,''),
                   ISNULL(GTFILE.GT_REF,''),
                   ISNULL(CAST(GTFILE.GT_DATE1 AS VARCHAR(10)),'0'),
                   GTFILE.GT_SOURCE,
                   ISNULL(CAST(GTFILE.GT_DATE2 AS VARCHAR(10)),'0'),
                   GTFILE.GT_BRANCH,
                   GTFILE.GT_ACCTNO,
                   ISNULL(CAST(GTFILE.GT_DATE3 AS VARCHAR(10)),'0'),
                   ISNULL(GTFILE.GT_DRCRFG,''),
                   GTFILE.GT_LINE,
                   ISNULL(CAST(GTFILE.GT_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   GTFILE.GT_AMT,
                   ISNULL(GTFILE.GT_EXPL,''),
                   GTFILE.GT_PERIOD,
                   ISNULL(GTFILE.GT_SMITH_SENT,''),
                   ISNULL(GTFILE.GT_POST_USERID,''),
                   ISNULL(CAST(GTFILE.GT_POST_DATE
                                                  AS VARCHAR(10)),'0'),
                   GTFILE.GT_POST_TIME
            FROM DBO.GTFILE
            WHERE GTFILE.GT_SOURCE >= :QGT1-WBEG-SOURCE
              AND GTFILE.GT_SOURCE <= :QGT1-WEND-SOURCE
              AND CAST(GTFILE.GT_DATE2 AS VARCHAR(10))
                           >= :QGT1-WBEG-DATE2
              AND CAST(GTFILE.GT_DATE2 AS VARCHAR(10))
                           <= :QGT1-WEND-DATE2
              AND GTFILE.GT_BRANCH >= :QGT1-WBEG-BRANCH
              AND GTFILE.GT_BRANCH <= :QGT1-WEND-BRANCH
              AND GTFILE.GT_ACCTNO >= :QGT1-WBEG-ACCTNO
              AND GTFILE.GT_ACCTNO <= :QGT1-WEND-ACCTNO
              AND CONCAT ( CAST(GTFILE.GT_DATE3 AS VARCHAR(10) ),
                          GTFILE.GT_DRCRFG,
                          GTFILE.GT_LINE)
                  >=
                  CONCAT (:QGT1-WBEG-DATE3,
                          :QGT1-WBEG-DRCRFG,
                          :QGT1-WBEG-LINE)
              AND CONCAT ( CAST(GTFILE.GT_DATE3 AS VARCHAR(10) ),
                          GTFILE.GT_DRCRFG,
                          GTFILE.GT_LINE)
                  <=
                  CONCAT (:QGT1-WEND-DATE3,
                          :QGT1-WEND-DRCRFG,
                          :QGT1-WEND-LINE)
           ORDER BY GTFILE.GT_SOURCE DESC,
                    GTFILE.GT_DATE2 DESC,
                    GTFILE.GT_BRANCH DESC,
                    GTFILE.GT_ACCTNO DESC,
                    GTFILE.GT_DATE3 DESC,
                    GTFILE.GT_DRCRFG DESC,
                    GTFILE.GT_LINE DESC
           END-EXEC.

           EXEC SQL
               OPEN GT1_NG_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QGT1-WBEG-DRCRFG
                          QGT1-WEND-DRCRFG
                          QGT1-WBEG-DATE2
                          QGT1-WEND-DATE2
                          QGT1-WBEG-DATE3
                          QGT1-WEND-DATE3.


           MOVE ZEROES TO QGT1-WBEG-BRANCH
                          QGT1-WEND-BRANCH
                          QGT1-WBEG-ACCTNO
                          QGT1-WEND-ACCTNO
                          QGT1-WBEG-LINE
                          QGT1-WEND-LINE
                          QGT1-WBEG-SOURCE
                          QGT1-WEND-SOURCE.

           MOVE STAT---GOOD TO GT1-NG-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-GT1-FILE.
           PERFORM READ-IT.
           MOVE GT-SELECT-PATH TO E-FILE.
           MOVE GT1-KEY TO E-KEYX.

           MOVE GT-SOURCE           TO QGT-SOURCE.
           MOVE GT-DATE2            TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QGT-DATE2.
           MOVE GT-BRANCH           TO QGT-BRANCH.
           MOVE GT-ACCTNO           TO QGT-ACCTNO.
           MOVE GT-DATE3            TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QGT-DATE3.
           MOVE GT-DRCRFG           TO QGT-DRCRFG.
           MOVE GT-LINE             TO QGT-LINE.

           EXEC SQL
            SELECT ISNULL(GTFILE.GT_FLAG,''),
                   ISNULL(GTFILE.GT_REF,''),
                   ISNULL(CAST(GTFILE.GT_DATE1 AS VARCHAR(10)),'0'),
                   GTFILE.GT_SOURCE,
                   ISNULL(CAST(GTFILE.GT_DATE2 AS VARCHAR(10)),'0'),
                   GTFILE.GT_BRANCH,
                   GTFILE.GT_ACCTNO,
                   ISNULL(CAST(GTFILE.GT_DATE3 AS VARCHAR(10)),'0'),
                   ISNULL(GTFILE.GT_DRCRFG,''),
                   GTFILE.GT_LINE,
                   ISNULL(CAST(GTFILE.GT_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   GTFILE.GT_AMT,
                   ISNULL(GTFILE.GT_EXPL,''),
                   GTFILE.GT_PERIOD,
                   ISNULL(GTFILE.GT_SMITH_SENT,''),
                   ISNULL(GTFILE.GT_POST_USERID,''),
                   ISNULL(CAST(GTFILE.GT_POST_DATE
                                                  AS VARCHAR(10)),'0'),
                   GTFILE.GT_POST_TIME
            INTO :QGT-REC
            FROM DBO.GTFILE
            WHERE GTFILE.GT_SOURCE = :QGT-SOURCE
              AND GTFILE.GT_DATE2 = :QGT-DATE2
              AND GTFILE.GT_BRANCH = :QGT-BRANCH
              AND GTFILE.GT_ACCTNO = :QGT-ACCTNO
              AND GTFILE.GT_DATE3 = :QGT-DATE3
              AND GTFILE.GT_DRCRFG = :QGT-DRCRFG
              AND GTFILE.GT_LINE = :QGT-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GT1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-GT-FIELDS.

      *-----------------------------------------------------------------
       READ-GT1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE GT-SELECT-PATH TO E-FILE.
           MOVE GT1-KEY TO E-KEYX.
           INITIALIZE QGT-REC.

           IF ( GT1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH GT1_CURSOR INTO :QGT-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GT1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-GT-FIELDS.


      *-----------------------------------------------------------------
       READ-GT1-FILE-PREVIOUS.
           PERFORM READ-IT.
           MOVE GT-SELECT-PATH TO E-FILE.
           MOVE GT1-KEY TO E-KEYX.
           INITIALIZE QGT-REC.

           IF ( GT1-NG-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH GT1_NG_CURSOR INTO :QGT-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDPVCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GT1-FILE-PREVIOUS.

           IF ( IO-FG = 0 )
              PERFORM GET-GT-FIELDS.

      *-----------------------------------------------------------------
       WRITE-GT1-FILE.

           ACCEPT GT-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE GT-SELECT-PATH TO E-FILE.
           MOVE GT1-KEY TO E-KEYX.
           PERFORM SET-GT-FIELDS.

           EXEC SQL
            INSERT INTO DBO.GTFILE
             (GTFILE.GT_FLAG,
              GTFILE.GT_REF,
              GTFILE.GT_DATE1,
              GTFILE.GT_SOURCE,
              GTFILE.GT_DATE2,
              GTFILE.GT_BRANCH,
              GTFILE.GT_ACCTNO,
              GTFILE.GT_DATE3,
              GTFILE.GT_DRCRFG,
              GTFILE.GT_LINE,
              GTFILE.GT_LTOUCH_DATE,
              GTFILE.GT_AMT,
              GTFILE.GT_EXPL,
              GTFILE.GT_PERIOD,
              GTFILE.GT_SMITH_SENT,
              GTFILE.GT_POST_USERID,
              GTFILE.GT_POST_DATE,
              GTFILE.GT_POST_TIME)
            VALUES
             (:QGT-FLAG,
              :QGT-REF,
              NULLIF(:QGT-DATE1,'1900-01-01'),
              :QGT-SOURCE,
              NULLIF(:QGT-DATE2,'1900-01-01'),
              :QGT-BRANCH,
              :QGT-ACCTNO,
              NULLIF(:QGT-DATE3,'1900-01-01'),
              :QGT-DRCRFG,
              :QGT-LINE,
              NULLIF(:QGT-LTOUCH-DATE,'1900-01-01'),
              :QGT-AMT,
              :QGT-EXPL,
              :QGT-PERIOD,
              :QGT-SMITH-SENT,
              :QGT-POST-USERID,
              NULLIF(:QGT-POST-DATE,'1900-01-01'),
              :QGT-POST-TIME)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-GT1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-GT1-FILE.

           ACCEPT GT-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE GT-SELECT-PATH TO E-FILE.
           MOVE GT1-KEY TO E-KEYX.
           PERFORM SET-GT-FIELDS.

           EXEC SQL
            UPDATE DBO.GTFILE
             SET
              GTFILE.GT_FLAG = :QGT-FLAG,
              GTFILE.GT_REF = :QGT-REF,
              GTFILE.GT_DATE1 =
                 NULLIF(:QGT-DATE1,'1900-01-01'),
              GTFILE.GT_SOURCE = :QGT-SOURCE,
              GTFILE.GT_DATE2 =
                 NULLIF(:QGT-DATE2,'1900-01-01'),
              GTFILE.GT_BRANCH = :QGT-BRANCH,
              GTFILE.GT_ACCTNO = :QGT-ACCTNO,
              GTFILE.GT_DATE3 =
                 NULLIF(:QGT-DATE3,'1900-01-01'),
              GTFILE.GT_DRCRFG = :QGT-DRCRFG,
              GTFILE.GT_LINE = :QGT-LINE,
              GTFILE.GT_LTOUCH_DATE =
                 NULLIF(:QGT-LTOUCH-DATE,'1900-01-01'),
              GTFILE.GT_AMT = :QGT-AMT,
              GTFILE.GT_EXPL = :QGT-EXPL,
              GTFILE.GT_PERIOD = :QGT-PERIOD,
              GTFILE.GT_SMITH_SENT = :QGT-SMITH-SENT,
              GTFILE.GT_POST_USERID = :QGT-POST-USERID,
              GTFILE.GT_POST_DATE =
                 NULLIF(:QGT-POST-DATE,'1900-01-01'),
              GTFILE.GT_POST_TIME = :QGT-POST-TIME
            WHERE GTFILE.GT_SOURCE = :QGT-SOURCE
              AND GTFILE.GT_DATE2 = :QGT-DATE2
              AND GTFILE.GT_BRANCH = :QGT-BRANCH
              AND GTFILE.GT_ACCTNO = :QGT-ACCTNO
              AND GTFILE.GT_DATE3 = :QGT-DATE3
              AND GTFILE.GT_DRCRFG = :QGT-DRCRFG
              AND GTFILE.GT_LINE = :QGT-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-GT1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-GT1-FILE.

           PERFORM DELETE-IT.
           MOVE GT-SELECT-PATH TO E-FILE.
           MOVE GT1-KEY TO E-KEYX.
           PERFORM SET-GT-FIELDS.

           EXEC SQL
            DELETE FROM DBO.GTFILE
            WHERE GTFILE.GT_SOURCE = :QGT-SOURCE
              AND GTFILE.GT_DATE2 = :QGT-DATE2
              AND GTFILE.GT_BRANCH = :QGT-BRANCH
              AND GTFILE.GT_ACCTNO = :QGT-ACCTNO
              AND GTFILE.GT_DATE3 = :QGT-DATE3
              AND GTFILE.GT_DRCRFG = :QGT-DRCRFG
              AND GTFILE.GT_LINE = :QGT-LINE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-GT1-FILE.

      *-----------------------------------------------------------------
       CLOSE-GT1-FILE.
           IF ( GT1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE GT1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO GT1-CURSOR-STAT.

           IF ( GT1-NG-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE GT1_NG_CURSOR
              END-EXEC
              MOVE STAT---BAD TO GT1-NG-CURSOR-STAT.

           IF ( GT1-GR-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE GT1_GR_CURSOR
              END-EXEC
              MOVE STAT---BAD TO GT1-GR-CURSOR-STAT.

      *============================================================
       LOAD-GT2-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( GT-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( GT-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO GT-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO GT-ALL-MACHINE
              MOVE GT-ALL-PATH             TO GT-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( GT-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO GT-ALL-BASE
              MOVE FILPATH-MACHINE         TO GT-ALL-MACHINE
              MOVE GT-ALL-PATH             TO GT-PATH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-GTFILE NOT = "S" )
              MOVE GT-PATH-BEG             TO GT-SELECT-PATH-BEG
              MOVE GT-PATH-FILE            TO GT-SELECT-PATH-FILE
           ELSE
              MOVE GT-PATH-SQL             TO GT-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-GT2-FILE.
           PERFORM LOAD-GT2-FILE.
           PERFORM OPEN-IT.
           MOVE GT-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-GT2-FILE.
           PERFORM START-IT.
           MOVE GT-SELECT-PATH TO E-FILE.
           MOVE GT2-KEY TO E-KEYX.
           IF ( GT2-CURSOR-STAT-GOOD )
              PERFORM CLOSE-GT2-FILE.

           IF ( QGT2-WEND-DATE1 = SPACES )
              MOVE GT-DATE1            TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGT2-WBEG-DATE1

              MOVE JULIAN-END          TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGT2-WEND-DATE1.

           EXEC SQL
            DECLARE GT2_CURSOR CURSOR FOR
            SELECT ISNULL(GTFILE.GT_FLAG,''),
                   ISNULL(GTFILE.GT_REF,''),
                   ISNULL(CAST(GTFILE.GT_DATE1 AS VARCHAR(10)),'0'),
                   GTFILE.GT_SOURCE,
                   ISNULL(CAST(GTFILE.GT_DATE2 AS VARCHAR(10)),'0'),
                   GTFILE.GT_BRANCH,
                   GTFILE.GT_ACCTNO,
                   ISNULL(CAST(GTFILE.GT_DATE3 AS VARCHAR(10)),'0'),
                   ISNULL(GTFILE.GT_DRCRFG,''),
                   GTFILE.GT_LINE,
                   ISNULL(CAST(GTFILE.GT_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   GTFILE.GT_AMT,
                   ISNULL(GTFILE.GT_EXPL,''),
                   GTFILE.GT_PERIOD,
                   ISNULL(GTFILE.GT_SMITH_SENT,''),
                   ISNULL(GTFILE.GT_POST_USERID,''),
                   ISNULL(CAST(GTFILE.GT_POST_DATE
                                                  AS VARCHAR(10)),'0'),
                   GTFILE.GT_POST_TIME
            FROM DBO.GTFILE
            WHERE CAST(GTFILE.GT_DATE1 AS VARCHAR(10))
                           >= :QGT2-WBEG-DATE1
              AND CAST(GTFILE.GT_DATE1 AS VARCHAR(10))
                           <= :QGT2-WEND-DATE1
            ORDER BY GTFILE.GT_DATE1,
                     GTFILE.GT_SOURCE,
                     GTFILE.GT_DATE2,
                     GTFILE.GT_BRANCH,
                     GTFILE.GT_ACCTNO,
                     GTFILE.GT_DATE3,
                     GTFILE.GT_DRCRFG,
                     GTFILE.GT_LINE
           END-EXEC.

           EXEC SQL
               OPEN GT2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QGT2-WBEG-DATE1
                          QGT2-WEND-DATE1.

           MOVE STAT---GOOD TO GT2-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-GT2-FILE.
           PERFORM READ-IT.
           MOVE GT-SELECT-PATH TO E-FILE.
           MOVE GT2-KEY TO E-KEYX.

           MOVE GT-DATE1            TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QGT-DATE1.

           EXEC SQL
            SELECT ISNULL(GTFILE.GT_FLAG,''),
                   ISNULL(GTFILE.GT_REF,''),
                   ISNULL(CAST(GTFILE.GT_DATE1 AS VARCHAR(10)),'0'),
                   GTFILE.GT_SOURCE,
                   ISNULL(CAST(GTFILE.GT_DATE2 AS VARCHAR(10)),'0'),
                   GTFILE.GT_BRANCH,
                   GTFILE.GT_ACCTNO,
                   ISNULL(CAST(GTFILE.GT_DATE3 AS VARCHAR(10)),'0'),
                   ISNULL(GTFILE.GT_DRCRFG,''),
                   GTFILE.GT_LINE,
                   ISNULL(CAST(GTFILE.GT_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   GTFILE.GT_AMT,
                   ISNULL(GTFILE.GT_EXPL,''),
                   GTFILE.GT_PERIOD,
                   ISNULL(GTFILE.GT_SMITH_SENT,''),
                   ISNULL(GTFILE.GT_POST_USERID,''),
                   ISNULL(CAST(GTFILE.GT_POST_DATE
                                                  AS VARCHAR(10)),'0'),
                   GTFILE.GT_POST_TIME
            INTO :QGT-REC
            FROM DBO.GTFILE
            WHERE GTFILE.GT_DATE1 = :QGT-DATE1
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GT2-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-GT-FIELDS.

      *-----------------------------------------------------------------
       READ-GT2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE GT-SELECT-PATH TO E-FILE.
           MOVE GT2-KEY TO E-KEYX.
           INITIALIZE QGT-REC.

           IF ( GT2-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH GT2_CURSOR INTO :QGT-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GT2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-GT-FIELDS.


      *-----------------------------------------------------------------
       WRITE-GT2-FILE.

           ACCEPT GT-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE GT-SELECT-PATH TO E-FILE.
           MOVE GT2-KEY TO E-KEYX.
           PERFORM SET-GT-FIELDS.

           EXEC SQL
            INSERT INTO DBO.GTFILE
             (GTFILE.GT_FLAG,
              GTFILE.GT_REF,
              GTFILE.GT_DATE1,
              GTFILE.GT_SOURCE,
              GTFILE.GT_DATE2,
              GTFILE.GT_BRANCH,
              GTFILE.GT_ACCTNO,
              GTFILE.GT_DATE3,
              GTFILE.GT_DRCRFG,
              GTFILE.GT_LINE,
              GTFILE.GT_LTOUCH_DATE,
              GTFILE.GT_AMT,
              GTFILE.GT_EXPL,
              GTFILE.GT_PERIOD,
              GTFILE.GT_SMITH_SENT,
              GTFILE.GT_POST_USERID,
              GTFILE.GT_POST_DATE,
              GTFILE.GT_POST_TIME)
            VALUES
             (:QGT-FLAG,
              :QGT-REF,
              NULLIF(:QGT-DATE1,'1900-01-01'),
              :QGT-SOURCE,
              NULLIF(:QGT-DATE2,'1900-01-01'),
              :QGT-BRANCH,
              :QGT-ACCTNO,
              NULLIF(:QGT-DATE3,'1900-01-01'),
              :QGT-DRCRFG,
              :QGT-LINE,
              NULLIF(:QGT-LTOUCH-DATE,'1900-01-01'),
              :QGT-AMT,
              :QGT-EXPL,
              :QGT-PERIOD,
              :QGT-SMITH-SENT,
              :QGT-POST-USERID,
              NULLIF(:QGT-POST-DATE,'1900-01-01'),
              :QGT-POST-TIME)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-GT2-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-GT2-FILE.

           ACCEPT GT-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE GT-SELECT-PATH TO E-FILE.
           MOVE GT2-KEY TO E-KEYX.
           PERFORM SET-GT-FIELDS.

           EXEC SQL
            UPDATE DBO.GTFILE
             SET
              GTFILE.GT_FLAG = :QGT-FLAG,
              GTFILE.GT_REF = :QGT-REF,
              GTFILE.GT_DATE1 =
                 NULLIF(:QGT-DATE1,'1900-01-01'),
              GTFILE.GT_SOURCE = :QGT-SOURCE,
              GTFILE.GT_DATE2 =
                 NULLIF(:QGT-DATE2,'1900-01-01'),
              GTFILE.GT_BRANCH = :QGT-BRANCH,
              GTFILE.GT_ACCTNO = :QGT-ACCTNO,
              GTFILE.GT_DATE3 =
                 NULLIF(:QGT-DATE3,'1900-01-01'),
              GTFILE.GT_DRCRFG = :QGT-DRCRFG,
              GTFILE.GT_LINE = :QGT-LINE,
              GTFILE.GT_LTOUCH_DATE =
                 NULLIF(:QGT-LTOUCH-DATE,'1900-01-01'),
              GTFILE.GT_AMT = :QGT-AMT,
              GTFILE.GT_EXPL = :QGT-EXPL,
              GTFILE.GT_PERIOD = :QGT-PERIOD,
              GTFILE.GT_SMITH_SENT = :QGT-SMITH-SENT,
              GTFILE.GT_POST_USERID = :QGT-POST-USERID,
              GTFILE.GT_POST_DATE =
                 NULLIF(:QGT-POST-DATE,'1900-01-01'),
              GTFILE.GT_POST_TIME = :QGT-POST-TIME
            WHERE GTFILE.GT_SOURCE = :QGT-SOURCE
              AND GTFILE.GT_DATE2 = :QGT-DATE2
              AND GTFILE.GT_BRANCH = :QGT-BRANCH
              AND GTFILE.GT_ACCTNO = :QGT-ACCTNO
              AND GTFILE.GT_DATE3 = :QGT-DATE3
              AND GTFILE.GT_DRCRFG = :QGT-DRCRFG
              AND GTFILE.GT_LINE = :QGT-LINE
           END-EXEC.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * NOTE: THE WHERE CONSTRAINT IN THE ABOVE REWRITE NEEDS TO USE
      *       THE PRIMARY KEY WHERE CONSTRAINT; CANNOT DO A REWRITE ON
      *       THE SECONDARY KEY SINCE THE Q??#-KEY IS CHANGED AND WILL
      *       NOT FIND THE ORIGINAL KEY, AS WELL AS Q??#-KEY COULD
      *       MATCH ANOTHER RECORD/ROW THAT SHOULD NOT CHANGE!!!
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-GT2-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-GT2-FILE.

           PERFORM DELETE-IT.
           MOVE GT-SELECT-PATH TO E-FILE.
           MOVE GT2-KEY TO E-KEYX.
           PERFORM SET-GT-FIELDS.

           EXEC SQL
            DELETE FROM DBO.GTFILE
            WHERE GTFILE.GT_DATE1 = :QGT-DATE1
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-GT2-FILE.

      *-----------------------------------------------------------------
       CLOSE-GT2-FILE.
           IF ( GT2-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE GT2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO GT2-CURSOR-STAT.

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
              DESTROY IOGTIN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
