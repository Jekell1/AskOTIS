       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOGQIN.
       AUTHOR.        AMM.
       DATE-WRITTEN.  03-17-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOGQIN
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : GQFILE 
      * TABLE DESC    : DAILY COLLECTOR QUEUE FILE
      * OUTPUT FILE   : XPC/IOGQIN.PRE
      * USED FOR      : LIBSP/SPWSGQ.CPY  --- QGQ-REC
      *                 LIBSP/SPGQ1R.CPY  --- READ
      *                 LIBSP/SPGQ1R.CPY  --- READ
      *                 LIBSP/SPGQ1RN.CPY --- READ/READ-NEXT/START
      *                 LIBSP/SPGQ2IN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * AMM 2021-0317 ***NEW***
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
           "@(#)IO/IOGQIN.CBL S34 10/25/21-14:46:14 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBSP/SP01GQ.CPY".
       COPY "LIBSP/SPWSGQ.CPY".

      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S 01 SQL COPY MEMBERS
      *#################################################################
      *=================================================================
      *
      *    Q - F I L E
      *
      *-----------------------------------------------------------------
      *    FOR UPDATING THE FILE'S WS COPY MEMBER: LIBSP/SPWSGQ.CPY
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QGQ1-WHERE-BEG-KEY.
           03  QGQ1-WBEG-BRNO        PIC 9(04).
           03  QGQ1-WBEG-ACCTNO      PIC 9(08).
 
       01  QGQ1-WHERE-END-KEY.
           03  QGQ1-WEND-BRNO        PIC 9(04).
           03  QGQ1-WEND-ACCTNO      PIC 9(08).
 
       01  QGQ2-WHERE-BEG-KEY.
           03  QGQ2-WBEG-COLLID         PIC X(03).
           03  QGQ2-WBEG-SEQ-FG         PIC X(01).
           03  QGQ2-WBEG-DATE           PIC X(10).
           03  QGQ2-WBEG-START-TIME     PIC S9(4).
           03  QGQ2-WBEG-REV-DEL        PIC 9(5).
           03  QGQ2-WBEG-MISC-KEY-FLD   PIC X(3).
           03  QGQ2-WBEG-REV-PDUE-DAYS  PIC 9(5).
           03  QGQ2-WBEG-REV-BAL        PIC 9(7).
           03  QGQ2-WBEG-SEQ            PIC 9(3).

       01  QGQ2-WHERE-END-KEY.
           03  QGQ2-WEND-COLLID         PIC X(03).
           03  QGQ2-WEND-SEQ-FG         PIC X(01).
           03  QGQ2-WEND-DATE           PIC X(10).
           03  QGQ2-WEND-START-TIME     PIC S9(4).
           03  QGQ2-WEND-REV-DEL        PIC 9(5).
           03  QGQ2-WEND-MISC-KEY-FLD   PIC X(3).
           03  QGQ2-WEND-REV-PDUE-DAYS  PIC 9(5).
           03  QGQ2-WEND-REV-BAL        PIC 9(7).
           03  QGQ2-WEND-SEQ            PIC 9(3).

       01  QGQ-REC.
           03  QGQ-BRNO              PIC 9(4).
           03  QGQ-ACCTNO            PIC 9(8).
           03  QGQ-COLLID            PIC XXX.
           03  QGQ-SEQ-FG            PIC X.
           03  QGQ-DATE              PIC X(10).
           03  QGQ-START-TIME        PIC S9(4).
           03  QGQ-REV-DEL           PIC 9(5).
           03  QGQ-MISC-KEY-FLD      PIC X(3).
           03  QGQ-REV-PDUE-DAYS     PIC 9(5).
           03  QGQ-REV-BAL           PIC 9(7).
           03  QGQ-SEQ               PIC 9(3).
           03  QGQ-LTOUCH-DATE       PIC X(10).
           03  QGQ-WORK-COLLID       PIC XXX.
           03  QGQ-WORKED-FG         PIC X.
           03  QGQ-IN-PROCESS        PIC X.
           03  QGQ-DATE-QUEUED       PIC X(10).
           03  QGQ-TIME-ZONE-ERROR   PIC X.

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

       01  LEGEND-BEG-BRNO.  
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING BRNO       (F3-ALL) ".

       01  LEGEND-END-BRNO.  
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING BRNO       ".

       01  LEGEND-BEG-ACCTNO.
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING ACCTNO     (F3-ALL) ".

       01  LEGEND-END-ACCTNO.
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING ACCTNO     ".

       01  LEGEND-BEG-COLLID.
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING COLLID     (F3-ALL) ".

       01  LEGEND-END-COLLID.
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING COLLID     ".

       01  LEGEND-BEG-SEQ-FG.
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING SEQ-FG     (F3-ALL) ".

       01  LEGEND-END-SEQ-FG.
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING SEQ-FG     ".

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
           03  BEG-BRNO                PIC 9(04).
           03  END-BRNO                PIC 9(04).
           03  BEG-ACCTNO              PIC 9(08).
           03  END-ACCTNO              PIC 9(08).
           03  BEG-COLLID              PIC X(03).
           03  END-COLLID              PIC X(03).
           03  BEG-SEQ-FG              PIC X(01).
           03  END-SEQ-FG              PIC X(01).

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
           03  FILLER PIC X(30) VALUE "BRNO ACCTNO COLLID SEQ-FG     ".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(12) VALUE SPACES.

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-GQ-BRNO                    PIC 9(04).
           03  FILLER                         PIC X(01).
           03  D01-GQ-ACCTNO                  PIC 9(08).
           03  FILLER                         PIC X(01).
           03  D01-GQ-COLLID                  PIC X(03).
           03  FILLER                         PIC X(01).
           03  D01-GQ-SEQ-FG                  PIC X(01).

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
       COPY "LIBIO/IOGQIN_DEF.CPY".
       COPY "LIBIO/IOGQIN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOGQIN_SCN.CPY".

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
           MOVE "B"                  TO GQ-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - -{ SET WHERE BEG & END FIELDS }
           MOVE BEG-BRNO             TO QGQ1-WBEG-BRNO.
           MOVE END-BRNO             TO QGQ1-WEND-BRNO.

           MOVE BEG-ACCTNO           TO QGQ1-WBEG-ACCTNO.
           MOVE END-ACCTNO           TO QGQ1-WEND-ACCTNO.

           MOVE BEG-COLLID           TO QGQ2-WBEG-COLLID.
           MOVE END-COLLID           TO QGQ2-WEND-COLLID.

           MOVE BEG-SEQ-FG           TO QGQ2-WBEG-SEQ-FG.
           MOVE END-SEQ-FG           TO QGQ2-WEND-SEQ-FG.

           MOVE JULIAN-BEG           TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QGQ2-WBEG-DATE.

           MOVE JULIAN-END           TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO QGQ2-WEND-DATE.

           MOVE ZEROES               TO QGQ2-WBEG-START-TIME.
           MOVE ALL "9"              TO QGQ2-WEND-START-TIME.

           MOVE ZEROES               TO QGQ2-WBEG-START-TIME.
           MOVE ALL "9"              TO QGQ2-WEND-START-TIME.

           MOVE ZEROES               TO QGQ2-WBEG-REV-DEL.
           MOVE ALL "9"              TO QGQ2-WEND-REV-DEL.

           MOVE SPACES               TO QGQ2-WBEG-MISC-KEY-FLD.
           MOVE ALL "Z"              TO QGQ2-WEND-MISC-KEY-FLD.

           MOVE ZEROES               TO QGQ2-WBEG-REV-PDUE-DAYS.
           MOVE ALL "9"              TO QGQ2-WEND-REV-PDUE-DAYS.

           MOVE ZEROES               TO QGQ2-WBEG-REV-BAL.
           MOVE ALL "9"              TO QGQ2-WEND-REV-BAL.

           MOVE ZEROES               TO QGQ2-WBEG-SEQ.
           MOVE ALL "9"              TO QGQ2-WEND-SEQ.

      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           IF ( PROCESS-KEY = 1 )
              PERFORM OPEN-GQ1-FILE
              PERFORM START-GQ1-FILE
           ELSE
              PERFORM OPEN-GQ2-FILE
              PERFORM START-GQ2-FILE.

      * AMM > WILL EVENTUALLY NEED TO ADD THE START GREATER IN THE
      *       IF CONDITION ABOVE. NO DATA FOR ME TO TEST RIGHT NOW

      *=================================================================
       MAIN-PROGRAM-NEXT.

           ADD 01 TO RECORDS-READ.

           IF ( PROCESS-KEY = 1 )
              PERFORM READ-GQ1-FILE-NEXT
           ELSE
              PERFORM READ-GQ2-FILE-NEXT.

           IF ( IO-BAD )
              GO TO MAIN-PROGRAM-END.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( PROCESS-KEY = 1 )
              IF ( GQ-BRNO < BEG-BRNO )
                 GO TO MAIN-PROGRAM-NEXT
              ELSE
              IF ( GQ-BRNO > END-BRNO )
                 GO TO MAIN-PROGRAM-END
              ELSE
              IF ( GQ-ACCTNO < BEG-ACCTNO ) OR
                 ( GQ-ACCTNO > END-ACCTNO )
                 GO TO MAIN-PROGRAM-NEXT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( PROCESS-KEY = 2 )
              IF ( GQ-COLLID < BEG-COLLID )
                 GO TO MAIN-PROGRAM-NEXT
              ELSE
              IF ( GQ-COLLID > END-COLLID )
                 GO TO MAIN-PROGRAM-END
              ELSE
              IF ( GQ-SEQ-FG < BEG-SEQ-FG ) OR
                 ( GQ-SEQ-FG > END-SEQ-FG )
                 GO TO MAIN-PROGRAM-NEXT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE GQ-BRNO                    TO D01-GQ-BRNO.
           MOVE GQ-ACCTNO                  TO D01-GQ-ACCTNO.
           MOVE GQ-COLLID                  TO D01-GQ-COLLID.
           MOVE GQ-SEQ-FG                  TO D01-GQ-SEQ-FG.
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
           MOVE "IOGQIN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOGQIN-QUEUE-POS  TO Q-POS.
           MOVE IOGQIN-COPIES-POS TO C-POS.
           MOVE IOGQIN-PRU-POS    TO P-POS.
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
       ENTER-BEG-BRNO.

           MOVE LEGEND-BEG-BRNO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040BEG-BRNO." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-BRNO.
      *KC  IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-BRNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-BRNO.

      *-----------------------------------------------------------------
       ENTER-END-BRNO.

           MOVE LEGEND-END-BRNO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040END-BRNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-BRNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-BRNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-BRNO.

           IF ( BEG-BRNO > END-BRNO )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-BRNO.

           GO TO ENTER-PROCESS-KEY.

      *-----------------------------------------------------------------
       ENTER-ALL-BRNO.

           MOVE "SNNN040BEG-BRNO." TO VDU.
           MOVE ZEROES             TO BEG-BRNO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040END-BRNO." TO VDU.
           MOVE ALL "9"            TO END-BRNO VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-ACCTNO.

           MOVE LEGEND-BEG-ACCTNO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN080BEG-ACCTNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-BRNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-ACCTNO.
      *KC  IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-ACCTNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-ACCTNO.

      *-----------------------------------------------------------------
       ENTER-END-ACCTNO.

           MOVE LEGEND-END-ACCTNO TO LEGEND.
           PERFORM SEND-LEGEND.

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

           GO TO ENTER-PROCESS-KEY.

      *-----------------------------------------------------------------
       ENTER-ALL-ACCTNO.

           MOVE "SNNN080BEG-ACCTNO." TO VDU.
           MOVE ZEROES               TO BEG-ACCTNO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN080END-ACCTNO." TO VDU.
           MOVE ALL "9"              TO END-ACCTNO VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-COLLID.

           MOVE LEGEND-BEG-COLLID TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A030BEG-COLLID." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-ACCTNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-COLLID.
      *KC  IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-COLLID.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-COLLID.

      *-----------------------------------------------------------------
       ENTER-END-COLLID.

           MOVE LEGEND-END-COLLID TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A030END-COLLID." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-COLLID.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-COLLID.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-COLLID.

           IF ( BEG-COLLID > END-BRNO )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-COLLID.

           GO TO ENTER-PROCESS-KEY.

      *-----------------------------------------------------------------
       ENTER-ALL-COLLID.

           MOVE "S   A03BEG-COLLID." TO VDU.
           MOVE SPACES               TO BEG-COLLID VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A030END-COLLID." TO VDU.
           MOVE ALL "Z"              TO END-COLLID VDUBUF.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-SEQ-FG.

           MOVE LEGEND-BEG-SEQ-FG TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010BEG-SEQ-FG." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-COLLID.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-SEQ-FG.
      *KC  IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-SEQ-FG.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-SEQ-FG.

      *-----------------------------------------------------------------
       ENTER-END-SEQ-FG.

           MOVE LEGEND-END-SEQ-FG TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A010END-SEQ-FG." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-SEQ-FG.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-SEQ-FG.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-SEQ-FG.

           IF ( BEG-SEQ-FG > END-BRNO )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-SEQ-FG.

           GO TO ENTER-PROCESS-KEY.

      *-----------------------------------------------------------------
       ENTER-ALL-SEQ-FG.

           MOVE "S   A01BEG-SEQ-FG." TO VDU.
           MOVE SPACES               TO BEG-SEQ-FG VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010END-SEQ-FG." TO VDU.
           MOVE ALL "Z"              TO END-SEQ-FG VDUBUF.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-PROCESS-KEY.

           MOVE LEGEND-PROCESS-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN010PROCESS-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-SEQ-FG.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUNUM0 < ZEROES ) OR
              ( VDUNUM0 > 2      )     
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

           MOVE "SNNN040BEG-BRNO."     TO VDU.
           MOVE BEG-BRNO               TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040END-BRNO."     TO VDU.
           MOVE END-BRNO               TO VDUNUM0.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN080BEG-ACCTNO."   TO VDU.
           MOVE BEG-ACCTNO             TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN080END-ACCTNO."   TO VDU.
           MOVE END-ACCTNO             TO VDUNUM0.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A030BEG-COLLID."   TO VDU.
           MOVE BEG-COLLID             TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A030END-COLLID."   TO VDU.
           MOVE END-COLLID             TO VDUBUF.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010BEG-SEQ-FG."   TO VDU.
           MOVE BEG-SEQ-FG             TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A010END-SEQ-FG."   TO VDU.
           MOVE END-SEQ-FG             TO VDUBUF.
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
           MOVE CUR-HH      TO PACE-TIME-HH.
           MOVE CUR-MM      TO PACE-TIME-MM.
           MOVE CUR-SS      TO PACE-TIME-SS.
           MOVE CUR-HD      TO PACE-TIME-HD.
           MOVE PACE-TIME   TO PACE-DETAIL-TIME.

           IF ( PROCESS-KEY = 1 ) 
              MOVE GQ1-KEY     TO PACE-DETAIL-KEY
           ELSE
              STRING GQ-COLLID, GQ-SEQ-FG
                 DELIMITED BY SIZE INTO PACE-DETAIL-KEY.

           MOVE PACE-DETAIL TO PACE-ELE(PACE-LEV).
           MOVE PACE-BUF    TO WR-BUF.
           MOVE PACE-LIST   TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE ZEROES      TO PACE-COUNTER.

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
           MOVE "BEGINNING BRNO                : "    TO R01-PROMPT.
           MOVE BEG-BRNO                              TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    BRNO                : "    TO R01-PROMPT.
           MOVE END-BRNO                              TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING ACCTNO              : "    TO R01-PROMPT.
           MOVE BEG-ACCTNO                            TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    ACCTNO              : "    TO R01-PROMPT.
           MOVE END-ACCTNO                            TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PROCESS KEY                   : "    TO R01-PROMPT.
           MOVE PROCESS-KEY                           TO R01-NUMERIC.
           MOVE 2                                     TO LN-FEED.
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
           MOVE "SQL TABLE IO UTILITY - GQFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOGQIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOGQIN-SYSDATE.
           DISPLAY IOGQIN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOGQIN-FORM-FIELDS TO WR-BUF.
           MOVE IOGQIN-FORM-LIST TO WR-LIST.
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

           MOVE IOGQIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOGQIN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOGQIN_EVA.CPY".
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
      *    LOAD GQ-REC FROM QGQ-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-GQ-FIELDS.
           MOVE QGQ-BRNO TO GQ-BRNO.
           MOVE QGQ-ACCTNO TO GQ-ACCTNO.
           MOVE QGQ-COLLID TO GQ-COLLID.
           MOVE QGQ-SEQ-FG TO GQ-SEQ-FG.

           IF ( QGQ-DATE = "1900-01-01" )
              MOVE ZEROES TO GQ-DATE
           ELSE
              MOVE QGQ-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GQ-DATE.

           MOVE QGQ-START-TIME TO GQ-START-TIME.
           MOVE QGQ-REV-DEL TO GQ-REV-DEL.
           MOVE QGQ-MISC-KEY-FLD TO GQ-MISC-KEY-FLD.
           MOVE QGQ-REV-PDUE-DAYS TO GQ-REV-PDUE-DAYS.
           MOVE QGQ-REV-BAL TO GQ-REV-BAL.
           MOVE QGQ-SEQ TO GQ-SEQ.

           IF ( QGQ-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO GQ-LTOUCH-DATE
           ELSE
              MOVE QGQ-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GQ-LTOUCH-DATE.

           MOVE QGQ-WORK-COLLID TO GQ-WORK-COLLID.
           MOVE QGQ-WORKED-FG TO GQ-WORKED-FG.
           MOVE QGQ-IN-PROCESS TO GQ-IN-PROCESS.

           IF ( QGQ-DATE-QUEUED = "1900-01-01" )
              MOVE ZEROES TO GQ-DATE-QUEUED
           ELSE
              MOVE QGQ-DATE-QUEUED TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO GQ-DATE-QUEUED.

           MOVE QGQ-TIME-ZONE-ERROR TO GQ-TIME-ZONE-ERROR.
 
      *-----------------------------------------------------------------
      *    LOAD QGQ-REC FROM GQ-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-GQ-FIELDS.
           MOVE GQ-BRNO TO QGQ-BRNO.
           MOVE GQ-ACCTNO TO QGQ-ACCTNO.
           MOVE GQ-COLLID TO QGQ-COLLID.
           MOVE GQ-SEQ-FG TO QGQ-SEQ-FG.

           IF ( GQ-DATE = ZEROES )
              MOVE "1900-01-01" TO QGQ-DATE
           ELSE
              MOVE GQ-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGQ-DATE.

           MOVE GQ-START-TIME TO QGQ-START-TIME.
           MOVE GQ-REV-DEL TO QGQ-REV-DEL.
           MOVE GQ-MISC-KEY-FLD TO QGQ-MISC-KEY-FLD.
           MOVE GQ-REV-PDUE-DAYS TO QGQ-REV-PDUE-DAYS.
           MOVE GQ-REV-BAL TO QGQ-REV-BAL.
           MOVE GQ-SEQ TO QGQ-SEQ.

           IF ( GQ-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QGQ-LTOUCH-DATE
           ELSE
              MOVE GQ-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGQ-LTOUCH-DATE.

           MOVE GQ-WORK-COLLID TO QGQ-WORK-COLLID.
           MOVE GQ-WORKED-FG TO QGQ-WORKED-FG.
           MOVE GQ-IN-PROCESS TO QGQ-IN-PROCESS.

           IF ( GQ-DATE-QUEUED = ZEROES )
              MOVE "1900-01-01" TO QGQ-DATE-QUEUED
           ELSE
              MOVE GQ-DATE-QUEUED TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGQ-DATE-QUEUED.

           MOVE GQ-TIME-ZONE-ERROR TO QGQ-TIME-ZONE-ERROR.
 
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      * COPYMEMBER: LIBSP/SPGQ1RN
       LOAD-GQ1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( GQ-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( GQ-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO GQ-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO GQ-ALL-MACHINE
              MOVE GQ-ALL-PATH             TO GQ-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( GQ-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO GQ-ALL-BASE
              MOVE FILPATH-MACHINE         TO GQ-ALL-MACHINE
              MOVE GQ-ALL-PATH             TO GQ-PATH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-GQFILE NOT = "S" )
              MOVE GQ-PATH-BEG             TO GQ-SELECT-PATH-BEG
              MOVE GQ-PATH-FILE            TO GQ-SELECT-PATH-FILE
           ELSE
              MOVE GQ-PATH-SQL             TO GQ-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-GQ1-FILE.
           PERFORM LOAD-GQ1-FILE.
           PERFORM OPEN-IT.
           MOVE GQ-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-GQ1-FILE.
           PERFORM START-IT.
           MOVE GQ-SELECT-PATH TO E-FILE.
           MOVE GQ1-KEY TO E-KEYX.

           IF ( GQ1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-GQ1-FILE.

           IF ( QGQ1-WEND-ACCTNO = ZEROES    ) OR
              ( QGQ1-WEND-ACCTNO NOT NUMERIC )
              MOVE GQ-BRNO              TO QGQ1-WBEG-BRNO
              MOVE ALL "9"              TO QGQ1-WEND-BRNO
              MOVE GQ-ACCTNO            TO QGQ1-WBEG-ACCTNO
              MOVE ALL "9"              TO QGQ1-WEND-ACCTNO.

           EXEC SQL
            DECLARE GQ1_CURSOR CURSOR FOR
            SELECT GQFILE.GQ_BRNO,
                   GQFILE.GQ_ACCTNO,
                   ISNULL(GQFILE.GQ_COLLID,''),
                   ISNULL(GQFILE.GQ_SEQ_FG,''),
                   ISNULL(CAST(GQFILE.GQ_DATE AS VARCHAR(10)),'0'),
                   GQFILE.GQ_START_TIME,
                   GQFILE.GQ_REV_DEL,
                   ISNULL(GQFILE.GQ_MISC_KEY_FLD,''),
                   GQFILE.GQ_REV_PDUE_DAYS,
                   GQFILE.GQ_REV_BAL,
                   GQFILE.GQ_SEQ,
                   ISNULL(CAST(GQFILE.GQ_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(GQFILE.GQ_WORK_COLLID,''),
                   ISNULL(GQFILE.GQ_WORKED_FG,''),
                   ISNULL(GQFILE.GQ_IN_PROCESS,''),
                   ISNULL(CAST(GQFILE.GQ_DATE_QUEUED
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(GQFILE.GQ_TIME_ZONE_ERROR,'')
            FROM DBO.GQFILE
            WHERE GQFILE.GQ_BRNO >= :QGQ1-WBEG-BRNO
              AND GQFILE.GQ_BRNO <= :QGQ1-WEND-BRNO
              AND GQFILE.GQ_ACCTNO >= :QGQ1-WBEG-ACCTNO
              AND GQFILE.GQ_ACCTNO <= :QGQ1-WEND-ACCTNO
            ORDER BY GQFILE.GQ_BRNO,
                     GQFILE.GQ_ACCTNO
           END-EXEC.

           EXEC SQL
               OPEN GQ1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QGQ1-WBEG-BRNO
                          QGQ1-WEND-BRNO
                          QGQ1-WBEG-ACCTNO
                          QGQ1-WEND-ACCTNO.

           MOVE STAT---GOOD TO GQ1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-GQ1-FILE.
           PERFORM READ-IT.
           MOVE GQ-SELECT-PATH TO E-FILE.
           MOVE GQ1-KEY TO E-KEYX.

           MOVE GQ-BRNO       TO QGQ-BRNO.  
           MOVE GQ-ACCTNO     TO QGQ-ACCTNO. 

           EXEC SQL
            SELECT GQFILE.GQ_BRNO,
                   GQFILE.GQ_ACCTNO,
                   ISNULL(GQFILE.GQ_COLLID,''),
                   ISNULL(GQFILE.GQ_SEQ_FG,''),
                   ISNULL(CAST(GQFILE.GQ_DATE AS VARCHAR(10)),'0'),
                   GQFILE.GQ_START_TIME,
                   GQFILE.GQ_REV_DEL,
                   ISNULL(GQFILE.GQ_MISC_KEY_FLD,''),
                   GQFILE.GQ_REV_PDUE_DAYS,
                   GQFILE.GQ_REV_BAL,
                   GQFILE.GQ_SEQ,
                   ISNULL(CAST(GQFILE.GQ_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(GQFILE.GQ_WORK_COLLID,''),
                   ISNULL(GQFILE.GQ_WORKED_FG,''),
                   ISNULL(GQFILE.GQ_IN_PROCESS,''),
                   ISNULL(CAST(GQFILE.GQ_DATE_QUEUED
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(GQFILE.GQ_TIME_ZONE_ERROR,'')
            INTO :QGQ-REC
            FROM DBO.GQFILE
            WHERE GQFILE.GQ_BRNO = :QGQ-BRNO
              AND GQFILE.GQ_ACCTNO = :QGQ-ACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GQ1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-GQ-FIELDS.

      *-----------------------------------------------------------------
       READ-GQ1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE GQ-SELECT-PATH TO E-FILE.
           MOVE GQ1-KEY TO E-KEYX.
           INITIALIZE QGQ-REC.

           IF ( GQ1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH GQ1_CURSOR INTO :QGQ-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GQ1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-GQ-FIELDS.

      *-----------------------------------------------------------------
       WRITE-GQ1-FILE.

           ACCEPT GQ-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE GQ-SELECT-PATH TO E-FILE.
           MOVE GQ1-KEY TO E-KEYX.
           PERFORM SET-GQ-FIELDS.

           EXEC SQL
            INSERT INTO DBO.GQFILE
             (GQFILE.GQ_BRNO,
              GQFILE.GQ_ACCTNO,
              GQFILE.GQ_COLLID,
              GQFILE.GQ_SEQ_FG,
              GQFILE.GQ_DATE,
              GQFILE.GQ_START_TIME,
              GQFILE.GQ_REV_DEL,
              GQFILE.GQ_MISC_KEY_FLD,
              GQFILE.GQ_REV_PDUE_DAYS,
              GQFILE.GQ_REV_BAL,
              GQFILE.GQ_SEQ,
              GQFILE.GQ_LTOUCH_DATE,
              GQFILE.GQ_WORK_COLLID,
              GQFILE.GQ_WORKED_FG,
              GQFILE.GQ_IN_PROCESS,
              GQFILE.GQ_DATE_QUEUED,
              GQFILE.GQ_TIME_ZONE_ERROR)
            VALUES
             (:QGQ-BRNO,
              :QGQ-ACCTNO,
              :QGQ-COLLID,
              :QGQ-SEQ-FG,
              NULLIF(:QGQ-DATE,'1900-01-01'),
              :QGQ-START-TIME,
              :QGQ-REV-DEL,
              :QGQ-MISC-KEY-FLD,
              :QGQ-REV-PDUE-DAYS,
              :QGQ-REV-BAL,
              :QGQ-SEQ,
              NULLIF(:QGQ-LTOUCH-DATE,'1900-01-01'),
              :QGQ-WORK-COLLID,
              :QGQ-WORKED-FG,
              :QGQ-IN-PROCESS,
              NULLIF(:QGQ-DATE-QUEUED,'1900-01-01'),
              :QGQ-TIME-ZONE-ERROR)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-GQ1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-GQ1-FILE.

           ACCEPT GQ-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE GQ-SELECT-PATH TO E-FILE.
           MOVE GQ1-KEY TO E-KEYX.
           PERFORM SET-GQ-FIELDS.

           EXEC SQL
            UPDATE DBO.GQFILE
             SET
              GQFILE.GQ_BRNO = :QGQ-BRNO,
              GQFILE.GQ_ACCTNO = :QGQ-ACCTNO,
              GQFILE.GQ_COLLID = :QGQ-COLLID,
              GQFILE.GQ_SEQ_FG = :QGQ-SEQ-FG,
              GQFILE.GQ_DATE =
                 NULLIF(:QGQ-DATE,'1900-01-01'),
              GQFILE.GQ_START_TIME = :QGQ-START-TIME,
              GQFILE.GQ_REV_DEL = :QGQ-REV-DEL,
              GQFILE.GQ_MISC_KEY_FLD = :QGQ-MISC-KEY-FLD,
              GQFILE.GQ_REV_PDUE_DAYS = :QGQ-REV-PDUE-DAYS,
              GQFILE.GQ_REV_BAL = :QGQ-REV-BAL,
              GQFILE.GQ_SEQ = :QGQ-SEQ,
              GQFILE.GQ_LTOUCH_DATE =
                 NULLIF(:QGQ-LTOUCH-DATE,'1900-01-01'),
              GQFILE.GQ_WORK_COLLID = :QGQ-WORK-COLLID,
              GQFILE.GQ_WORKED_FG = :QGQ-WORKED-FG,
              GQFILE.GQ_IN_PROCESS = :QGQ-IN-PROCESS,
              GQFILE.GQ_DATE_QUEUED =
                 NULLIF(:QGQ-DATE-QUEUED,'1900-01-01'),
              GQFILE.GQ_TIME_ZONE_ERROR = :QGQ-TIME-ZONE-ERROR
            WHERE GQFILE.GQ_BRNO = :QGQ-BRNO
              AND GQFILE.GQ_ACCTNO = :QGQ-ACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-GQ1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-GQ1-FILE.

           PERFORM DELETE-IT.
           MOVE GQ-SELECT-PATH TO E-FILE.
           MOVE GQ1-KEY TO E-KEYX.
           PERFORM SET-GQ-FIELDS.

           EXEC SQL
            DELETE FROM DBO.GQFILE
            WHERE GQFILE.GQ_BRNO = :QGQ-BRNO
              AND GQFILE.GQ_ACCTNO = :QGQ-ACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-GQ1-FILE.

      *-----------------------------------------------------------------
       CLOSE-GQ1-FILE.
           IF ( GQ1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE GQ1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO GQ1-CURSOR-STAT.

      *=================================================================
       LOAD-GQ2-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( GQ-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( GQ-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO GQ-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO GQ-ALL-MACHINE
              MOVE GQ-ALL-PATH             TO GQ-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( GQ-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO GQ-ALL-BASE
              MOVE FILPATH-MACHINE         TO GQ-ALL-MACHINE
              MOVE GQ-ALL-PATH             TO GQ-PATH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-GQFILE NOT = "S" )
              MOVE GQ-PATH-BEG             TO GQ-SELECT-PATH-BEG
              MOVE GQ-PATH-FILE            TO GQ-SELECT-PATH-FILE
           ELSE
              MOVE GQ-PATH-SQL             TO GQ-SELECT-PATH.

       LOAD-GQ2-NON-COMP-KEY.
      *    MOVE GQ-BRNO                 TO SPWSGQ-GQ2-BRNO-NC.
           MOVE GQ-COLLID               TO SPWSGQ-GQ2-COLLID-NC.
           MOVE GQ-SEQ-FG               TO SPWSGQ-GQ2-SEQ-FG-NC.
           MOVE GQ-DATE                 TO SPWSGQ-GQ2-DATE-NC.
           MOVE GQ-START-TIME           TO SPWSGQ-GQ2-START-TIME-NC.
           MOVE GQ-REV-DEL              TO SPWSGQ-GQ2-REV-DEL-NC.
           MOVE GQ-MISC-KEY-FLD         TO SPWSGQ-GQ2-MISC-KEY-FLD-NC.
           MOVE GQ-REV-PDUE-DAYS        TO SPWSGQ-GQ2-REV-PDUE-DAYS-NC.
           MOVE GQ-REV-BAL              TO SPWSGQ-GQ2-REV-BAL-NC.
           MOVE GQ-SEQ                  TO SPWSGQ-GQ2-SEQ-NC.

      *-----------------------------------------------------------------
       OPEN-GQ2-FILE.
           PERFORM LOAD-GQ2-FILE.
           PERFORM OPEN-IT.
           MOVE GQ-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-GQ2-FILE.
           PERFORM START-IT.
           MOVE GQ-SELECT-PATH TO E-FILE.
           PERFORM LOAD-GQ2-NON-COMP-KEY.
           MOVE SPWSGQ-GQ2-KEY-NON-COMP TO E-KEYX.

           IF ( GQ2-CURSOR-STAT-GOOD )
              PERFORM CLOSE-GQ2-FILE.

           IF ( QGQ2-WEND-SEQ = ZEROES    ) OR
              ( QGQ2-WEND-SEQ NOT NUMERIC )
              MOVE GQ-COLLID            TO QGQ2-WBEG-COLLID
              MOVE ALL "Z"              TO QGQ2-WEND-COLLID
              MOVE GQ-SEQ-FG            TO QGQ2-WBEG-SEQ-FG
              MOVE ALL "Z"              TO QGQ2-WEND-SEQ-FG
              MOVE GQ-DATE              TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGQ2-WBEG-DATE
              MOVE JULIAN-END           TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD  TO QGQ2-WEND-DATE
              MOVE GQ-START-TIME        TO QGQ2-WBEG-START-TIME
              MOVE ALL "9"              TO QGQ2-WEND-START-TIME
              MOVE GQ-REV-DEL           TO QGQ2-WBEG-REV-DEL
              MOVE ALL "9"              TO QGQ2-WEND-REV-DEL
              MOVE GQ-MISC-KEY-FLD      TO QGQ2-WBEG-MISC-KEY-FLD
              MOVE ALL "Z"              TO QGQ2-WEND-MISC-KEY-FLD
              MOVE GQ-REV-PDUE-DAYS     TO QGQ2-WBEG-REV-PDUE-DAYS
              MOVE ALL "9"              TO QGQ2-WEND-REV-PDUE-DAYS
              MOVE GQ-REV-BAL           TO QGQ2-WBEG-REV-BAL
              MOVE ALL "9"              TO QGQ2-WEND-REV-BAL
              MOVE GQ-SEQ               TO QGQ2-WBEG-SEQ
              MOVE ALL "9"              TO QGQ2-WEND-SEQ.

           EXEC SQL
            DECLARE GQ2_CURSOR CURSOR FOR
            SELECT GQFILE.GQ_BRNO,
                   GQFILE.GQ_ACCTNO,
                   ISNULL(GQFILE.GQ_COLLID,''),
                   ISNULL(GQFILE.GQ_SEQ_FG,''),
                   ISNULL(CAST(GQFILE.GQ_DATE AS VARCHAR(10)),'0'),
                   GQFILE.GQ_START_TIME,
                   GQFILE.GQ_REV_DEL,
                   ISNULL(GQFILE.GQ_MISC_KEY_FLD,''),
                   GQFILE.GQ_REV_PDUE_DAYS,
                   GQFILE.GQ_REV_BAL,
                   GQFILE.GQ_SEQ,
                   ISNULL(CAST(GQFILE.GQ_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(GQFILE.GQ_WORK_COLLID,''),
                   ISNULL(GQFILE.GQ_WORKED_FG,''),
                   ISNULL(GQFILE.GQ_IN_PROCESS,''),
                   ISNULL(CAST(GQFILE.GQ_DATE_QUEUED
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(GQFILE.GQ_TIME_ZONE_ERROR,'')
            FROM DBO.GQFILE
            WHERE GQFILE.GQ_COLLID >= :QGQ2-WBEG-COLLID
              AND GQFILE.GQ_COLLID <= :QGQ2-WEND-COLLID
              AND GQFILE.GQ_SEQ_FG >= :QGQ2-WBEG-SEQ-FG
              AND GQFILE.GQ_SEQ_FG <= :QGQ2-WEND-SEQ-FG
              AND CAST(GQFILE.GQ_DATE AS VARCHAR(10)) >=
                           :QGQ2-WBEG-DATE
              AND CAST(GQFILE.GQ_DATE AS VARCHAR(10)) <=
                           :QGQ2-WEND-DATE
              AND GQFILE.GQ_START_TIME >= :QGQ2-WBEG-START-TIME
              AND GQFILE.GQ_START_TIME <= :QGQ2-WEND-START-TIME
              AND GQFILE.GQ_REV_DEL >= :QGQ2-WBEG-REV-DEL
              AND GQFILE.GQ_REV_DEL <= :QGQ2-WEND-REV-DEL
              AND GQFILE.GQ_MISC_KEY_FLD >= :QGQ2-WBEG-MISC-KEY-FLD
              AND GQFILE.GQ_MISC_KEY_FLD <= :QGQ2-WEND-MISC-KEY-FLD
              AND GQFILE.GQ_REV_PDUE_DAYS>= :QGQ2-WBEG-REV-PDUE-DAYS
              AND GQFILE.GQ_REV_PDUE_DAYS<= :QGQ2-WEND-REV-PDUE-DAYS
              AND GQFILE.GQ_REV_BAL >= :QGQ2-WBEG-REV-BAL
              AND GQFILE.GQ_REV_BAL <= :QGQ2-WEND-REV-BAL
              AND GQFILE.GQ_SEQ >= :QGQ2-WBEG-SEQ
              AND GQFILE.GQ_SEQ <= :QGQ2-WEND-SEQ
            ORDER BY GQFILE.GQ_COLLID,
                     GQFILE.GQ_SEQ_FG,
                     GQFILE.GQ_DATE,
                     GQFILE.GQ_START_TIME,
                     GQFILE.GQ_REV_DEL,
                     GQFILE.GQ_MISC_KEY_FLD,
                     GQFILE.GQ_REV_PDUE_DAYS,
                     GQFILE.GQ_REV_BAL,
                     GQFILE.GQ_SEQ,
                     GQFILE.GQ_BRNO,
                     GQFILE.GQ_ACCTNO
           END-EXEC.

           EXEC SQL
               OPEN GQ2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QGQ2-WBEG-COLLID
                          QGQ2-WEND-COLLID
                          QGQ2-WBEG-SEQ-FG
                          QGQ2-WEND-SEQ-FG
                          QGQ2-WBEG-DATE
                          QGQ2-WEND-DATE
                          QGQ2-WBEG-MISC-KEY-FLD
                          QGQ2-WEND-MISC-KEY-FLD.
      
           MOVE ZEROES TO QGQ2-WBEG-START-TIME
                          QGQ2-WEND-START-TIME
                          QGQ2-WBEG-REV-DEL
                          QGQ2-WEND-REV-DEL
                          QGQ2-WBEG-REV-PDUE-DAYS
                          QGQ2-WEND-REV-PDUE-DAYS
                          QGQ2-WBEG-REV-BAL
                          QGQ2-WEND-REV-BAL
                          QGQ2-WBEG-SEQ
                          QGQ2-WEND-SEQ.

           MOVE STAT---GOOD TO GQ2-CURSOR-STAT.

      *-----------------------------------------------------------------
       START-GQ2-FILE-GREATER.
           PERFORM START-IT.
           MOVE GQ-SELECT-PATH TO E-FILE.
           PERFORM LOAD-GQ2-NON-COMP-KEY.
           MOVE SPWSGQ-GQ2-KEY-NON-COMP TO E-KEYX.

           IF ( GQ2-GR-CURSOR-STAT-GOOD )
              PERFORM CLOSE-GQ2-FILE.

           IF ( QGQ2-WEND-SEQ = ZEROES    ) OR
              ( QGQ2-WEND-SEQ NOT NUMERIC )
              MOVE GQ-COLLID            TO QGQ2-WBEG-COLLID
              MOVE ALL "Z"              TO QGQ2-WEND-COLLID
              MOVE GQ-SEQ-FG            TO QGQ2-WBEG-SEQ-FG
              MOVE ALL "Z"              TO QGQ2-WEND-SEQ-FG
              MOVE GQ-DATE             TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGQ2-WBEG-DATE
              MOVE JULIAN-END          TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QGQ2-WEND-DATE
              MOVE GQ-START-TIME        TO QGQ2-WBEG-START-TIME
              MOVE ALL "9"              TO QGQ2-WEND-START-TIME
              MOVE GQ-REV-DEL           TO QGQ2-WBEG-REV-DEL
              MOVE ALL "9"              TO QGQ2-WEND-REV-DEL
              MOVE GQ-MISC-KEY-FLD      TO QGQ2-WBEG-MISC-KEY-FLD
              MOVE ALL "Z"              TO QGQ2-WEND-MISC-KEY-FLD
              MOVE GQ-REV-PDUE-DAYS     TO QGQ2-WBEG-REV-PDUE-DAYS
              MOVE ALL "9"              TO QGQ2-WEND-REV-PDUE-DAYS
              MOVE GQ-REV-BAL           TO QGQ2-WBEG-REV-BAL
              MOVE ALL "9"              TO QGQ2-WEND-REV-BAL
              MOVE GQ-SEQ               TO QGQ2-WBEG-SEQ
              MOVE ALL "9"              TO QGQ2-WEND-SEQ.

           EXEC SQL
            DECLARE GQ2_GR_CURSOR CURSOR FOR
            SELECT GQFILE.GQ_BRNO,
                   GQFILE.GQ_ACCTNO,
                   ISNULL(GQFILE.GQ_COLLID,''),
                   ISNULL(GQFILE.GQ_SEQ_FG,''),
                   ISNULL(CAST(GQFILE.GQ_DATE AS VARCHAR(10)),'0'),
                   GQFILE.GQ_START_TIME,
                   GQFILE.GQ_REV_DEL,
                   ISNULL(GQFILE.GQ_MISC_KEY_FLD,''),
                   GQFILE.GQ_REV_PDUE_DAYS,
                   GQFILE.GQ_REV_BAL,
                   GQFILE.GQ_SEQ,
                   ISNULL(CAST(GQFILE.GQ_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(GQFILE.GQ_WORK_COLLID,''),
                   ISNULL(GQFILE.GQ_WORKED_FG,''),
                   ISNULL(GQFILE.GQ_IN_PROCESS,''),
                   ISNULL(CAST(GQFILE.GQ_DATE_QUEUED
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(GQFILE.GQ_TIME_ZONE_ERROR,'')
            FROM DBO.GQFILE
            WHERE GQFILE.GQ_COLLID > :QGQ2-WBEG-COLLID
              AND GQFILE.GQ_COLLID <= :QGQ2-WEND-COLLID
              AND GQFILE.GQ_SEQ_FG > :QGQ2-WBEG-SEQ-FG
              AND GQFILE.GQ_SEQ_FG <= :QGQ2-WEND-SEQ-FG
              AND CAST(GQFILE.GQ_DATE AS VARCHAR(10)) >
                           :QGQ2-WBEG-DATE
              AND CAST(GQFILE.GQ_DATE AS VARCHAR(10)) <=
                           :QGQ2-WEND-DATE
              AND GQFILE.GQ_START_TIME > :QGQ2-WBEG-START-TIME
              AND GQFILE.GQ_START_TIME <= :QGQ2-WEND-START-TIME
              AND GQFILE.GQ_REV_DEL > :QGQ2-WBEG-REV-DEL
              AND GQFILE.GQ_REV_DEL <= :QGQ2-WEND-REV-DEL
              AND GQFILE.GQ_MISC_KEY_FLD > :QGQ2-WBEG-MISC-KEY-FLD
              AND GQFILE.GQ_MISC_KEY_FLD <= :QGQ2-WEND-MISC-KEY-FLD
              AND GQFILE.GQ_REV_PDUE_DAYS> :QGQ2-WBEG-REV-PDUE-DAYS
              AND GQFILE.GQ_REV_PDUE_DAYS<= :QGQ2-WEND-REV-PDUE-DAYS
              AND GQFILE.GQ_REV_BAL > :QGQ2-WBEG-REV-BAL
              AND GQFILE.GQ_REV_BAL <= :QGQ2-WEND-REV-BAL
              AND GQFILE.GQ_SEQ > :QGQ2-WBEG-SEQ
              AND GQFILE.GQ_SEQ <= :QGQ2-WEND-SEQ
            ORDER BY GQFILE.GQ_COLLID,
                     GQFILE.GQ_SEQ_FG,
                     GQFILE.GQ_DATE,
                     GQFILE.GQ_START_TIME,
                     GQFILE.GQ_REV_DEL,
                     GQFILE.GQ_MISC_KEY_FLD,
                     GQFILE.GQ_REV_PDUE_DAYS,
                     GQFILE.GQ_REV_BAL,
                     GQFILE.GQ_SEQ,
                     GQFILE.GQ_BRNO,
                     GQFILE.GQ_ACCTNO
           END-EXEC.

           EXEC SQL
               OPEN GQ2_GR_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QGQ2-WBEG-COLLID
                          QGQ2-WEND-COLLID
                          QGQ2-WBEG-SEQ-FG
                          QGQ2-WEND-SEQ-FG
                          QGQ2-WBEG-DATE  
                          QGQ2-WEND-DATE  
                          QGQ2-WBEG-MISC-KEY-FLD
                          QGQ2-WEND-MISC-KEY-FLD.
      
           MOVE ZEROES TO QGQ2-WBEG-START-TIME
                          QGQ2-WEND-START-TIME
                          QGQ2-WBEG-REV-DEL
                          QGQ2-WEND-REV-DEL
                          QGQ2-WBEG-REV-PDUE-DAYS
                          QGQ2-WEND-REV-PDUE-DAYS
                          QGQ2-WBEG-REV-BAL
                          QGQ2-WEND-REV-BAL
                          QGQ2-WBEG-SEQ
                          QGQ2-WEND-SEQ.

           MOVE STAT---GOOD TO GQ2-GR-CURSOR-STAT.

      *-------------------------------------------------------
       READ-GQ2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE GQ-SELECT-PATH TO E-FILE.
           PERFORM LOAD-GQ2-NON-COMP-KEY.
           MOVE SPWSGQ-GQ2-KEY-NON-COMP TO E-KEYX.

           INITIALIZE QGQ-REC.

           IF ( GQ2-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH GQ2_CURSOR INTO :QGQ-REC
              END-EXEC
           ELSE
           IF ( GQ2-GR-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH GQ2_GR_CURSOR INTO :QGQ-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-GQ2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-GQ-FIELDS.

      *-----------------------------------------------------------------
       WRITE-GQ2-FILE.

           ACCEPT GQ-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE GQ-SELECT-PATH TO E-FILE.
           PERFORM LOAD-GQ2-NON-COMP-KEY.
           MOVE SPWSGQ-GQ2-KEY-NON-COMP TO E-KEYX.
           PERFORM SET-GQ-FIELDS.

           EXEC SQL
            INSERT INTO DBO.GQFILE
             (GQFILE.GQ_BRNO,
              GQFILE.GQ_ACCTNO,
              GQFILE.GQ_COLLID,
              GQFILE.GQ_SEQ_FG,
              GQFILE.GQ_DATE,
              GQFILE.GQ_START_TIME,
              GQFILE.GQ_REV_DEL,
              GQFILE.GQ_MISC_KEY_FLD,
              GQFILE.GQ_REV_PDUE_DAYS,
              GQFILE.GQ_REV_BAL,
              GQFILE.GQ_SEQ,
              GQFILE.GQ_LTOUCH_DATE,
              GQFILE.GQ_WORK_COLLID,
              GQFILE.GQ_WORKED_FG,
              GQFILE.GQ_IN_PROCESS,
              GQFILE.GQ_DATE_QUEUED,
              GQFILE.GQ_TIME_ZONE_ERROR)
            VALUES
             (:QGQ-BRNO,
              :QGQ-ACCTNO,
              :QGQ-COLLID,
              :QGQ-SEQ-FG,
              NULLIF(:QGQ-DATE,'1900-01-01'),
              :QGQ-START-TIME,
              :QGQ-REV-DEL,
              :QGQ-MISC-KEY-FLD,
              :QGQ-REV-PDUE-DAYS,
              :QGQ-REV-BAL,
              :QGQ-SEQ,
              NULLIF(:QGQ-LTOUCH-DATE,'1900-01-01'),
              :QGQ-WORK-COLLID,
              :QGQ-WORKED-FG,
              :QGQ-IN-PROCESS,
              NULLIF(:QGQ-DATE-QUEUED,'1900-01-01'),
              :QGQ-TIME-ZONE-ERROR)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-GQ2-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-GQ2-FILE.

           ACCEPT GQ-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE GQ-SELECT-PATH TO E-FILE.
           PERFORM LOAD-GQ2-NON-COMP-KEY.
           MOVE SPWSGQ-GQ2-KEY-NON-COMP TO E-KEYX.
           PERFORM SET-GQ-FIELDS.

           EXEC SQL
            UPDATE DBO.GQFILE
             SET
              GQFILE.GQ_BRNO = :QGQ-BRNO,
              GQFILE.GQ_ACCTNO = :QGQ-ACCTNO,
              GQFILE.GQ_COLLID = :QGQ-COLLID,
              GQFILE.GQ_SEQ_FG = :QGQ-SEQ-FG,
              GQFILE.GQ_DATE =
                 NULLIF(:QGQ-DATE,'1900-01-01'),
              GQFILE.GQ_START_TIME = :QGQ-START-TIME,
              GQFILE.GQ_REV_DEL = :QGQ-REV-DEL,
              GQFILE.GQ_MISC_KEY_FLD = :QGQ-MISC-KEY-FLD,
              GQFILE.GQ_REV_PDUE_DAYS = :QGQ-REV-PDUE-DAYS,
              GQFILE.GQ_REV_BAL = :QGQ-REV-BAL,
              GQFILE.GQ_SEQ = :QGQ-SEQ,
              GQFILE.GQ_LTOUCH_DATE =
                 NULLIF(:QGQ-LTOUCH-DATE,'1900-01-01'),
              GQFILE.GQ_WORK_COLLID = :QGQ-WORK-COLLID,
              GQFILE.GQ_WORKED_FG = :QGQ-WORKED-FG,
              GQFILE.GQ_IN_PROCESS = :QGQ-IN-PROCESS,
              GQFILE.GQ_DATE_QUEUED =
                 NULLIF(:QGQ-DATE-QUEUED,'1900-01-01'),
              GQFILE.GQ_TIME_ZONE_ERROR = :QGQ-TIME-ZONE-ERROR
            WHERE GQFILE.GQ_BRNO = :QGQ-BRNO
              AND GQFILE.GQ_ACCTNO = :QGQ-ACCTNO
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
              GO TO REWRITE-GQ2-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-GQ2-FILE.

           PERFORM DELETE-IT.
           MOVE GQ-SELECT-PATH TO E-FILE.
           PERFORM LOAD-GQ2-NON-COMP-KEY.
           MOVE SPWSGQ-GQ2-KEY-NON-COMP TO E-KEYX.
           PERFORM SET-GQ-FIELDS.

           EXEC SQL
            DELETE FROM DBO.GQFILE
            WHERE GQFILE.GQ_COLLID = :QGQ-COLLID
              AND GQFILE.GQ_SEQ_FG = :QGQ-SEQ-FG
              AND GQFILE.GQ_DATE = :QGQ-DATE
              AND GQFILE.GQ_START_TIME = :QGQ-START-TIME
              AND GQFILE.GQ_REV_DEL = :QGQ-REV-DEL
              AND GQFILE.GQ_MISC_KEY_FLD = :QGQ-MISC-KEY-FLD
              AND GQFILE.GQ_REV_PDUE_DAYS = :QGQ-REV-PDUE-DAYS
              AND GQFILE.GQ_REV_BAL = :QGQ-REV-BAL
              AND GQFILE.GQ_SEQ = :QGQ-SEQ
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-GQ2-FILE.

      *-----------------------------------------------------------------
       CLOSE-GQ2-FILE.
           IF ( GQ2-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE GQ2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO GQ2-CURSOR-STAT.

           IF ( GQ2-GR-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE GQ2_GR_CURSOR
              END-EXEC
              MOVE STAT---BAD TO GQ2-GR-CURSOR-STAT.

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
              DESTROY IOGQIN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
