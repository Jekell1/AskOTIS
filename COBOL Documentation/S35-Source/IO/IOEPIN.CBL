       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOEPIN.
       AUTHOR.        AMM.
       DATE-WRITTEN.  05-11-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOEPIN
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : EPFILE 
      * TABLE DESC    : LOAN ELECTRONIC PAYMENT FILE
      * OUTPUT FILE   : XPC/IOEPIN.PRE
      * USED FOR      : LIBSP/SPWSEP.CPY  --- QEP-REC
      *                 LIBSP/SPEP1R.CPY  --- READ
      *                 LIBSP/SPEP1RN.CPY --- READ/READ-NEXT/START
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
           "@(#)IO/IOEPIN.CBL S34 05/17/21-16:47:02 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBSP/SP01EP.CPY".
       COPY "LIBSP/SPWSEP.CPY".

      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S 01 SQL COPY MEMBERS
      *#################################################################
      *=================================================================
      *
      *    Q - F I L E
      *
      *-----------------------------------------------------------------
      *    FOR UPDATING THE FILE'S WS COPY MEMBER: LIBSP/SPWSEP.CPY
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QEP1-WHERE-BEG-KEY.
           03  QEP1-WBEG-BRNO             PIC 9(04).
           03  QEP1-WBEG-ACCTNO           PIC 9(08).
 
       01  QEP1-WHERE-END-KEY.
           03  QEP1-WEND-BRNO             PIC 9(04).
           03  QEP1-WEND-ACCTNO           PIC 9(08).
 
       01  QEP-REC.
           03  QEP-BRNO                   PIC 9(4).
           03  QEP-ACCTNO                 PIC 9(8).
           03  QEP-LTOUCH-DATE            PIC X(10).
           03  QEP-SSNO                   PIC 9(9).
           03  QEP-NAME-ON-ACCOUNT        PIC X(22).
           03  QEP-BANK-ACCTNO            PIC X(17).
           03  QEP-BANK-ROUTING-NO        PIC 9(9).
           03  QEP-ACCT-TYPE              PIC X.
           03  QEP-PAYMENT-AMT            PIC S9(9)V99.
           03  QEP-UNITPER-CD             PIC X.
           03  QEP-PAY-DAY                PIC 9(2).
           03  QEP-BEG-PAY-DATE           PIC X(10).
           03  QEP-END-PAY-DATE           PIC X(10).
           03  QEP-STOP-FG                PIC X.
           03  QEP-PRENOTE-FG             PIC X.
           03  QEP-LAST-COMPUTED-DATE     PIC X(10).
           03  QEP-REFCD                  PIC X(5).
           03  QEP-ALTERNATE-DATE         PIC X(10).
           03  QEP-ALTERNATE-AMT          PIC S9(9)V99.
           03  QEP-LAST-BATCH-DATE        PIC X(10).
           03  QEP-LAST-BATCH-AMT         PIC S9(9)V99.
           03  QEP-LAST-BATCH-USERID      PIC X(10).
           03  QEP-LAST-NACHA-DATE        PIC X(10).
           03  QEP-LAST-NACHA-AMT         PIC S9(9)V99.
           03  QEP-LAST-NACHA-USERID      PIC X(10).

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
           03  FILLER PIC X(30) VALUE "BRNO ACCTNO                   ".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(12) VALUE SPACES.

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-EP-BRNO                    PIC 9(04).
           03  FILLER                         PIC X(01).
           03  D01-EP-ACCTNO                  PIC 9(08).

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
       COPY "LIBIO/IOEPIN_DEF.CPY".
       COPY "LIBIO/IOEPIN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOEPIN_SCN.CPY".

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
           MOVE "B"                  TO EP-PATH-OVERRIDE.

           MOVE BEG-BRNO             TO QEP1-WBEG-BRNO.
           MOVE BEG-ACCTNO           TO QEP1-WBEG-ACCTNO.

           MOVE END-BRNO             TO QEP1-WEND-BRNO.
           MOVE END-ACCTNO           TO QEP1-WEND-ACCTNO.

           PERFORM OPEN-EP1-FILE.
           PERFORM START-EP1-FILE.

      *=================================================================
       MAIN-PROGRAM-NEXT.

           ADD 01 TO RECORDS-READ.

           PERFORM READ-EP1-FILE-NEXT.

           IF ( IO-BAD )
              GO TO MAIN-PROGRAM-END.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EP-BRNO < BEG-BRNO )
              GO TO MAIN-PROGRAM-NEXT
           ELSE
           IF ( EP-BRNO > END-BRNO )
              GO TO MAIN-PROGRAM-END
           ELSE
           IF ( EP-ACCTNO < BEG-ACCTNO ) OR
              ( EP-ACCTNO > END-ACCTNO )
              GO TO MAIN-PROGRAM-NEXT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE EP-BRNO                    TO D01-EP-BRNO.
           MOVE EP-ACCTNO                  TO D01-EP-ACCTNO.
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
           MOVE "IOEPIN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOEPIN-QUEUE-POS  TO Q-POS.
           MOVE IOEPIN-COPIES-POS TO C-POS.
           MOVE IOEPIN-PRU-POS    TO P-POS.
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
       ENTER-PROCESS-KEY.

           MOVE LEGEND-PROCESS-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN010PROCESS-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-ACCTNO.
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

           MOVE EP1-KEY     TO PACE-DETAIL-KEY.

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
           MOVE "SQL TABLE IO UTILITY - EPFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOEPIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOEPIN-SYSDATE.
           DISPLAY IOEPIN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOEPIN-FORM-FIELDS TO WR-BUF.
           MOVE IOEPIN-FORM-LIST TO WR-LIST.
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

           MOVE IOEPIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOEPIN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOEPIN_EVA.CPY".
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
      *    LOAD EP-REC FROM QEP-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-EP-FIELDS.
           MOVE QEP-BRNO TO EP-BRNO.
           MOVE QEP-ACCTNO TO EP-ACCTNO.

           IF ( QEP-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO EP-LTOUCH-DATE
           ELSE
              MOVE QEP-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO EP-LTOUCH-DATE.

           MOVE QEP-SSNO TO EP-SSNO.
           MOVE QEP-NAME-ON-ACCOUNT TO EP-NAME-ON-ACCOUNT.
           MOVE QEP-BANK-ACCTNO TO EP-BANK-ACCTNO.
           MOVE QEP-BANK-ROUTING-NO TO EP-BANK-ROUTING-NO.
           MOVE QEP-ACCT-TYPE TO EP-ACCT-TYPE.
           MOVE QEP-PAYMENT-AMT TO EP-PAYMENT-AMT.
           MOVE QEP-UNITPER-CD TO EP-UNITPER-CD.
           MOVE QEP-PAY-DAY TO EP-PAY-DAY.

           IF ( QEP-BEG-PAY-DATE = "1900-01-01" )
              MOVE ZEROES TO EP-BEG-PAY-DATE
           ELSE
              MOVE QEP-BEG-PAY-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO EP-BEG-PAY-DATE.


           IF ( QEP-END-PAY-DATE = "1900-01-01" )
              MOVE ZEROES TO EP-END-PAY-DATE
           ELSE
              MOVE QEP-END-PAY-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO EP-END-PAY-DATE.

           MOVE QEP-STOP-FG TO EP-STOP-FG.
           MOVE QEP-PRENOTE-FG TO EP-PRENOTE-FG.

           IF ( QEP-LAST-COMPUTED-DATE = "1900-01-01" )
              MOVE ZEROES TO EP-LAST-COMPUTED-DATE
           ELSE
              MOVE QEP-LAST-COMPUTED-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO EP-LAST-COMPUTED-DATE.

           MOVE QEP-REFCD TO EP-REFCD.

           IF ( QEP-ALTERNATE-DATE = "1900-01-01" )
              MOVE ZEROES TO EP-ALTERNATE-DATE
           ELSE
              MOVE QEP-ALTERNATE-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO EP-ALTERNATE-DATE.

           MOVE QEP-ALTERNATE-AMT TO EP-ALTERNATE-AMT.

           IF ( QEP-LAST-BATCH-DATE = "1900-01-01" )
              MOVE ZEROES TO EP-LAST-BATCH-DATE
           ELSE
              MOVE QEP-LAST-BATCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO EP-LAST-BATCH-DATE.

           MOVE QEP-LAST-BATCH-AMT TO EP-LAST-BATCH-AMT.
           MOVE QEP-LAST-BATCH-USERID TO EP-LAST-BATCH-USERID.

           IF ( QEP-LAST-NACHA-DATE = "1900-01-01" )
              MOVE ZEROES TO EP-LAST-NACHA-DATE
           ELSE
              MOVE QEP-LAST-NACHA-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO EP-LAST-NACHA-DATE.

           MOVE QEP-LAST-NACHA-AMT TO EP-LAST-NACHA-AMT.
           MOVE QEP-LAST-NACHA-USERID TO EP-LAST-NACHA-USERID.
 
      *-----------------------------------------------------------------
      *    LOAD QEP-REC FROM EP-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-EP-FIELDS.
           MOVE EP-BRNO TO QEP-BRNO.
           MOVE EP-ACCTNO TO QEP-ACCTNO.

           IF ( EP-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QEP-LTOUCH-DATE
           ELSE
              MOVE EP-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QEP-LTOUCH-DATE.

           MOVE EP-SSNO TO QEP-SSNO.
           MOVE EP-NAME-ON-ACCOUNT TO QEP-NAME-ON-ACCOUNT.
           MOVE EP-BANK-ACCTNO TO QEP-BANK-ACCTNO.
           MOVE EP-BANK-ROUTING-NO TO QEP-BANK-ROUTING-NO.
           MOVE EP-ACCT-TYPE TO QEP-ACCT-TYPE.
           MOVE EP-PAYMENT-AMT TO QEP-PAYMENT-AMT.
           MOVE EP-UNITPER-CD TO QEP-UNITPER-CD.
           MOVE EP-PAY-DAY TO QEP-PAY-DAY.

           IF ( EP-BEG-PAY-DATE = ZEROES )
              MOVE "1900-01-01" TO QEP-BEG-PAY-DATE
           ELSE
              MOVE EP-BEG-PAY-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QEP-BEG-PAY-DATE.


           IF ( EP-END-PAY-DATE = ZEROES )
              MOVE "1900-01-01" TO QEP-END-PAY-DATE
           ELSE
              MOVE EP-END-PAY-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QEP-END-PAY-DATE.

           MOVE EP-STOP-FG TO QEP-STOP-FG.
           MOVE EP-PRENOTE-FG TO QEP-PRENOTE-FG.

           IF ( EP-LAST-COMPUTED-DATE = ZEROES )
              MOVE "1900-01-01" TO QEP-LAST-COMPUTED-DATE
           ELSE
              MOVE EP-LAST-COMPUTED-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QEP-LAST-COMPUTED-DATE.

           MOVE EP-REFCD TO QEP-REFCD.

           IF ( EP-ALTERNATE-DATE = ZEROES )
              MOVE "1900-01-01" TO QEP-ALTERNATE-DATE
           ELSE
              MOVE EP-ALTERNATE-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QEP-ALTERNATE-DATE.

           MOVE EP-ALTERNATE-AMT TO QEP-ALTERNATE-AMT.

           IF ( EP-LAST-BATCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QEP-LAST-BATCH-DATE
           ELSE
              MOVE EP-LAST-BATCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QEP-LAST-BATCH-DATE.

           MOVE EP-LAST-BATCH-AMT TO QEP-LAST-BATCH-AMT.
           MOVE EP-LAST-BATCH-USERID TO QEP-LAST-BATCH-USERID.

           IF ( EP-LAST-NACHA-DATE = ZEROES )
              MOVE "1900-01-01" TO QEP-LAST-NACHA-DATE
           ELSE
              MOVE EP-LAST-NACHA-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QEP-LAST-NACHA-DATE.

           MOVE EP-LAST-NACHA-AMT TO QEP-LAST-NACHA-AMT.
           MOVE EP-LAST-NACHA-USERID TO QEP-LAST-NACHA-USERID.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      * COPYMEMBER: LIBSP/SPEP1RN
       LOAD-EP1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( EP-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( EP-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO EP-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO EP-ALL-MACHINE
              MOVE EP-ALL-PATH             TO EP-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( EP-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO EP-ALL-BASE
              MOVE FILPATH-MACHINE         TO EP-ALL-MACHINE
              MOVE EP-ALL-PATH             TO EP-PATH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-EPFILE NOT = "S" )
              MOVE EP-PATH-BEG             TO EP-SELECT-PATH-BEG
              MOVE EP-PATH-FILE            TO EP-SELECT-PATH-FILE
           ELSE
              MOVE EP-PATH-SQL             TO EP-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-EP1-FILE.
           PERFORM LOAD-EP1-FILE.
           PERFORM OPEN-IT.
           MOVE EP-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-EP1-FILE.
           PERFORM START-IT.
           MOVE EP-SELECT-PATH TO E-FILE.
           MOVE EP1-KEY TO E-KEYX.

           IF ( EP1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-EP1-FILE.

           IF ( QEP1-WEND-ACCTNO = ZEROES    ) OR
              ( QEP1-WEND-ACCTNO NOT NUMERIC )
              MOVE EP-BRNO              TO QEP1-WBEG-BRNO
              MOVE ALL "9"              TO QEP1-WEND-BRNO
              MOVE EP-ACCTNO            TO QEP1-WBEG-ACCTNO
              MOVE ALL "9"              TO QEP1-WEND-ACCTNO.

           EXEC SQL
            DECLARE EP1_CURSOR CURSOR FOR
            SELECT EPFILE.EP_BRNO,
                   EPFILE.EP_ACCTNO,
                   ISNULL(CAST(EPFILE.EP_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   EPFILE.EP_SSNO,
                   ISNULL(EPFILE.EP_NAME_ON_ACCOUNT,''),
                   ISNULL(EPFILE.EP_BANK_ACCTNO,''),
                   EPFILE.EP_BANK_ROUTING_NO,
                   ISNULL(EPFILE.EP_ACCT_TYPE,''),
                   EPFILE.EP_PAYMENT_AMT,
                   ISNULL(EPFILE.EP_UNITPER_CD,''),
                   EPFILE.EP_PAY_DAY,
                   ISNULL(CAST(EPFILE.EP_BEG_PAY_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(EPFILE.EP_END_PAY_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(EPFILE.EP_STOP_FG,''),
                   ISNULL(EPFILE.EP_PRENOTE_FG,''),
                   ISNULL(CAST(EPFILE.EP_LAST_COMPUTED_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(EPFILE.EP_REFCD,''),
                   ISNULL(CAST(EPFILE.EP_ALTERNATE_DATE
                                                  AS VARCHAR(10)),'0'),
                   EPFILE.EP_ALTERNATE_AMT,
                   ISNULL(CAST(EPFILE.EP_LAST_BATCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   EPFILE.EP_LAST_BATCH_AMT,
                   ISNULL(EPFILE.EP_LAST_BATCH_USERID,''),
                   ISNULL(CAST(EPFILE.EP_LAST_NACHA_DATE
                                                  AS VARCHAR(10)),'0'),
                   EPFILE.EP_LAST_NACHA_AMT,
                   ISNULL(EPFILE.EP_LAST_NACHA_USERID,'')
            FROM DBO.EPFILE
            WHERE EPFILE.EP_BRNO >= :QEP1-WBEG-BRNO
              AND EPFILE.EP_BRNO <= :QEP1-WEND-BRNO
              AND EPFILE.EP_ACCTNO >= :QEP1-WBEG-ACCTNO
              AND EPFILE.EP_ACCTNO <= :QEP1-WEND-ACCTNO
            ORDER BY EPFILE.EP_BRNO,
                     EPFILE.EP_ACCTNO
           END-EXEC.

           EXEC SQL
               OPEN EP1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QEP1-WBEG-BRNO
                          QEP1-WEND-BRNO
                          QEP1-WBEG-ACCTNO
                          QEP1-WEND-ACCTNO.

           MOVE STAT---GOOD TO EP1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-EP1-FILE.
           PERFORM READ-IT.
           MOVE EP-SELECT-PATH TO E-FILE.
           MOVE EP1-KEY TO E-KEYX.

           MOVE EP-BRNO       TO QEP-BRNO.  
           MOVE EP-ACCTNO     TO QEP-ACCTNO. 

           EXEC SQL
            SELECT EPFILE.EP_BRNO,
                   EPFILE.EP_ACCTNO,
                   ISNULL(CAST(EPFILE.EP_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   EPFILE.EP_SSNO,
                   ISNULL(EPFILE.EP_NAME_ON_ACCOUNT,''),
                   ISNULL(EPFILE.EP_BANK_ACCTNO,''),
                   EPFILE.EP_BANK_ROUTING_NO,
                   ISNULL(EPFILE.EP_ACCT_TYPE,''),
                   EPFILE.EP_PAYMENT_AMT,
                   ISNULL(EPFILE.EP_UNITPER_CD,''),
                   EPFILE.EP_PAY_DAY,
                   ISNULL(CAST(EPFILE.EP_BEG_PAY_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CAST(EPFILE.EP_END_PAY_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(EPFILE.EP_STOP_FG,''),
                   ISNULL(EPFILE.EP_PRENOTE_FG,''),
                   ISNULL(CAST(EPFILE.EP_LAST_COMPUTED_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(EPFILE.EP_REFCD,''),
                   ISNULL(CAST(EPFILE.EP_ALTERNATE_DATE
                                                  AS VARCHAR(10)),'0'),
                   EPFILE.EP_ALTERNATE_AMT,
                   ISNULL(CAST(EPFILE.EP_LAST_BATCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   EPFILE.EP_LAST_BATCH_AMT,
                   ISNULL(EPFILE.EP_LAST_BATCH_USERID,''),
                   ISNULL(CAST(EPFILE.EP_LAST_NACHA_DATE
                                                  AS VARCHAR(10)),'0'),
                   EPFILE.EP_LAST_NACHA_AMT,
                   ISNULL(EPFILE.EP_LAST_NACHA_USERID,'')
            INTO :QEP-REC
            FROM DBO.EPFILE
            WHERE EPFILE.EP_BRNO = :QEP-BRNO
              AND EPFILE.EP_ACCTNO = :QEP-ACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-EP1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-EP-FIELDS.

      *-----------------------------------------------------------------
       READ-EP1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE EP-SELECT-PATH TO E-FILE.
           MOVE EP1-KEY TO E-KEYX.
           INITIALIZE QEP-REC.

           IF ( EP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH EP1_CURSOR INTO :QEP-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-EP1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-EP-FIELDS.

      *-----------------------------------------------------------------
       WRITE-EP1-FILE.

           ACCEPT EP-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE EP-SELECT-PATH TO E-FILE.
           MOVE EP1-KEY TO E-KEYX.
           PERFORM SET-EP-FIELDS.

           EXEC SQL
            INSERT INTO DBO.EPFILE
             (EPFILE.EP_BRNO,
              EPFILE.EP_ACCTNO,
              EPFILE.EP_LTOUCH_DATE,
              EPFILE.EP_SSNO,
              EPFILE.EP_NAME_ON_ACCOUNT,
              EPFILE.EP_BANK_ACCTNO,
              EPFILE.EP_BANK_ROUTING_NO,
              EPFILE.EP_ACCT_TYPE,
              EPFILE.EP_PAYMENT_AMT,
              EPFILE.EP_UNITPER_CD,
              EPFILE.EP_PAY_DAY,
              EPFILE.EP_BEG_PAY_DATE,
              EPFILE.EP_END_PAY_DATE,
              EPFILE.EP_STOP_FG,
              EPFILE.EP_PRENOTE_FG,
              EPFILE.EP_LAST_COMPUTED_DATE,
              EPFILE.EP_REFCD,
              EPFILE.EP_ALTERNATE_DATE,
              EPFILE.EP_ALTERNATE_AMT,
              EPFILE.EP_LAST_BATCH_DATE,
              EPFILE.EP_LAST_BATCH_AMT,
              EPFILE.EP_LAST_BATCH_USERID,
              EPFILE.EP_LAST_NACHA_DATE,
              EPFILE.EP_LAST_NACHA_AMT,
              EPFILE.EP_LAST_NACHA_USERID)
            VALUES
             (:QEP-BRNO,
              :QEP-ACCTNO,
              NULLIF(:QEP-LTOUCH-DATE,'1900-01-01'),
              :QEP-SSNO,
              :QEP-NAME-ON-ACCOUNT,
              :QEP-BANK-ACCTNO,
              :QEP-BANK-ROUTING-NO,
              :QEP-ACCT-TYPE,
              :QEP-PAYMENT-AMT,
              :QEP-UNITPER-CD,
              :QEP-PAY-DAY,
              NULLIF(:QEP-BEG-PAY-DATE,'1900-01-01'),
              NULLIF(:QEP-END-PAY-DATE,'1900-01-01'),
              :QEP-STOP-FG,
              :QEP-PRENOTE-FG,
              NULLIF(:QEP-LAST-COMPUTED-DATE,'1900-01-01'),
              :QEP-REFCD,
              NULLIF(:QEP-ALTERNATE-DATE,'1900-01-01'),
              :QEP-ALTERNATE-AMT,
              NULLIF(:QEP-LAST-BATCH-DATE,'1900-01-01'),
              :QEP-LAST-BATCH-AMT,
              :QEP-LAST-BATCH-USERID,
              NULLIF(:QEP-LAST-NACHA-DATE,'1900-01-01'),
              :QEP-LAST-NACHA-AMT,
              :QEP-LAST-NACHA-USERID)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-EP1-FILE.

           IF ( IO-FG = 0 )
               EXEC SQL COMMIT END-EXEC
               PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-EP1-FILE.

           ACCEPT EP-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE EP-SELECT-PATH TO E-FILE.
           MOVE EP1-KEY TO E-KEYX.
           PERFORM SET-EP-FIELDS.

           EXEC SQL
            UPDATE DBO.EPFILE
             SET
              EPFILE.EP_BRNO = :QEP-BRNO,
              EPFILE.EP_ACCTNO = :QEP-ACCTNO,
              EPFILE.EP_LTOUCH_DATE =
                 NULLIF(:QEP-LTOUCH-DATE,'1900-01-01'),
              EPFILE.EP_SSNO = :QEP-SSNO,
              EPFILE.EP_NAME_ON_ACCOUNT = :QEP-NAME-ON-ACCOUNT,
              EPFILE.EP_BANK_ACCTNO = :QEP-BANK-ACCTNO,
              EPFILE.EP_BANK_ROUTING_NO = :QEP-BANK-ROUTING-NO,
              EPFILE.EP_ACCT_TYPE = :QEP-ACCT-TYPE,
              EPFILE.EP_PAYMENT_AMT = :QEP-PAYMENT-AMT,
              EPFILE.EP_UNITPER_CD = :QEP-UNITPER-CD,
              EPFILE.EP_PAY_DAY = :QEP-PAY-DAY,
              EPFILE.EP_BEG_PAY_DATE =
                 NULLIF(:QEP-BEG-PAY-DATE,'1900-01-01'),
              EPFILE.EP_END_PAY_DATE =
                 NULLIF(:QEP-END-PAY-DATE,'1900-01-01'),
              EPFILE.EP_STOP_FG = :QEP-STOP-FG,
              EPFILE.EP_PRENOTE_FG = :QEP-PRENOTE-FG,
              EPFILE.EP_LAST_COMPUTED_DATE =
                 NULLIF(:QEP-LAST-COMPUTED-DATE,'1900-01-01'),
              EPFILE.EP_REFCD = :QEP-REFCD,
              EPFILE.EP_ALTERNATE_DATE =
                 NULLIF(:QEP-ALTERNATE-DATE,'1900-01-01'),
              EPFILE.EP_ALTERNATE_AMT = :QEP-ALTERNATE-AMT,
              EPFILE.EP_LAST_BATCH_DATE =
                 NULLIF(:QEP-LAST-BATCH-DATE,'1900-01-01'),
              EPFILE.EP_LAST_BATCH_AMT = :QEP-LAST-BATCH-AMT,
              EPFILE.EP_LAST_BATCH_USERID =
                   :QEP-LAST-BATCH-USERID,
              EPFILE.EP_LAST_NACHA_DATE =
                 NULLIF(:QEP-LAST-NACHA-DATE,'1900-01-01'),
              EPFILE.EP_LAST_NACHA_AMT = :QEP-LAST-NACHA-AMT,
              EPFILE.EP_LAST_NACHA_USERID =
                   :QEP-LAST-NACHA-USERID
             WHERE EPFILE.EP_BRNO = :QEP-BRNO
               AND EPFILE.EP_ACCTNO = :QEP-ACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-EP1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-EP1-FILE.

           PERFORM DELETE-IT.
           MOVE EP-SELECT-PATH TO E-FILE.
           MOVE EP1-KEY TO E-KEYX.
           PERFORM SET-EP-FIELDS.

           EXEC SQL
            DELETE FROM DBO.EPFILE
             WHERE EPFILE.EP_BRNO = :QEP-BRNO
               AND EPFILE.EP_ACCTNO = :QEP-ACCTNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-EP1-FILE.

      *-----------------------------------------------------------------
       CLOSE-EP1-FILE.
           IF ( EP1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE EP1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO EP1-CURSOR-STAT.

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
              DESTROY IOEPIN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
