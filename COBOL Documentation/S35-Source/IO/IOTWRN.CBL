       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOTWRN.
       AUTHOR.        BAH
       DATE-WRITTEN.  03-17-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOTWRN
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : TRWINFO 
      * TABLE DESC    : METRO2 INFORMATION FILE
      * OUTPUT FILE   : XPC/IOTWRN.PRE
      * USED FOR      : LIBSP/SPWSTRWINFOCPY  --- QTRW-INFO-REC
      *                 LIBSP/SPTRWINFOI.CPY  --- READ
      *                 LIBSP/SPTRWINFORN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * BAH 2021-0317 ***NEW***
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
           "@(#)IO/IOTWRN.CBL S34 07/30/21-08:45:09 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       01  TRW-INFO-REC.
           03  TRW-INFO-KEY.
               05  TRW-INFO-ID             PIC 9(4).

           03  TRW-INFO-EQUIFAX-ID         PIC X(10).
           03  TRW-INFO-EXPERIAN-ID        PIC X(5).
           03  TRW-INFO-TUC-ID             PIC X(10).

           03  TRW-INFO-REPORTER-NAME      PIC X(40).
           03  TRW-INFO-REPORTER-ADDRESS.
               05  TRW-INFO-REPORTER-ADR1  PIC X(32).
               05  TRW-INFO-REPORTER-ADR2  PIC X(32).
               05  TRW-INFO-REPORTER-CSZ   PIC X(32).
           03  TRW-INFO-REPORTER-PHONE     PIC 9(10)    COMP-3.

           03  TRW-INFO-GRANTOR-CODE       PIC X(20).

           03  TRW-INFO-CR-LF              PIC X.
           03  TRW-INFO-BRNO-ACCTNO        PIC X.
           03  TRW-INFO-IDENT-BRNO         PIC X.

      * REMOVED THIS FROM MAINTENANCE #1440, BUT DIDNT WANT TO REMOVE
           03  TRW-YEARS-STOP-REPORTING    PIC 99.
           03  TRW-YEARS-STOP-X REDEFINES TRW-YEARS-STOP-REPORTING
                                           PIC XX.

           03  TRW-INVALID-SSNO            PIC X.
               88  TRW-INVALID-SSNO-VALID  VALUE " " "0" "9".

           03  TRW-INFO-SCHED-PAY          PIC X.

           
      $XFD DATE=YYYYMMDD
           03  TRW-INFO-LTOUCH-DATE        PIC 9(8)     COMP-3.

       COPY "LIBSP/SPWSTRWINFO.CPY".

      *=================================================================
      *
      *    Q - F I L E
      *
      *-----------------------------------------------------------------
      *    FOR UPDATING THE FILE'S WS COPY MEMBER: LIB??/??WS??.CPY
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QTRWINFO-WHERE-BEG-KEY.
           03  QTRW-INFO-WBEG-INFO-ID           PIC 9(04).
 
       01  QTRWINFO-WHERE-END-KEY.
           03  QTRW-INFO-WEND-INFO-ID           PIC 9(04).
 
       01  QTRWINFO-REC.
           03  QTRW-INFO-ID             PIC 9(4).
           03  QTRW-INFO-EQUIFAX-ID         PIC X(10).
           03  QTRW-INFO-EXPERIAN-ID        PIC X(5).
           03  QTRW-INFO-TUC-ID             PIC X(10).
           03  QTRW-INFO-REPORTER-NAME      PIC X(40).
           03  QTRW-INFO-REPORTER-ADR1  PIC X(32).
           03  QTRW-INFO-REPORTER-ADR2  PIC X(32).
           03  QTRW-INFO-REPORTER-CSZ   PIC X(32).
           03  QTRW-INFO-REPORTER-PHONE     PIC 9(10).
           03  QTRW-INFO-GRANTOR-CODE       PIC X(20).
           03  QTRW-INFO-CR-LF              PIC X.
           03  QTRW-INFO-BRNO-ACCTNO        PIC X.
           03  QTRW-INFO-IDENT-BRNO         PIC X.
           03  QTRW-YEARS-STOP-REPORTING    PIC 99.
           03  QTRW-INVALID-SSNO            PIC X.
           03  QTRW-INFO-SCHED-PAY          PIC X.
           03  QTRW-INFO-LTOUCH-DATE        PIC X(10).
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

       01  LEGEND-BEG-ID. 
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING ID         (F3-ALL) ".

       01  LEGEND-END-ID. 
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING ID         ".

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
           03  BEG-ID               PIC 9(04).
           03  END-ID               PIC 9(04).

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
           03  FILLER PIC X(41) VALUE "ID     EQUIFAX      EXPER   TUC".
           03  FILLER PIC X(45) VALUE
                                "REPORTER NAME               PHONE".
           03  FILLER PIC X(30) VALUE SPACES.

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-TRW-ID                     PIC 9(4).
           03  FILLER                         PIC X(03).
           03  D01-TRW-EQUIFAX-ID             PIC X(10).
           03  FILLER                         PIC X(03).
           03  D01-TRW-EXPERIAN-ID            PIC X(5).
           03  FILLER                         PIC X(03).
           03  D01-TRW-TUC-ID                 PIC X(10).
           03  FILLER                         PIC X(03).
           03  D01-TRW-REPORTER-NAME          PIC X(25).
           03  FILLER                         PIC X(03).
           03  D01-TRW-REPORTER-PHONE         PIC 9(10).
           03  FILLER                         PIC X(03).
           03  D01-TRW-GRANTOR-CODE           PIC X(20).
           03  FILLER                         PIC X(03).

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
       COPY "LIBIO/IOTWRN_DEF.CPY".
       COPY "LIBIO/IOTWRN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOTWRN_SCN.CPY".

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
           MOVE "B"                  TO TRW-INFO-PATH-OVERRIDE.

           MOVE BEG-ID               TO QTRW-INFO-WBEG-INFO-ID.
           MOVE END-ID               TO QTRW-INFO-WEND-INFO-ID.

           PERFORM OPEN-TRW-INFO-FILE.
           PERFORM START-TRW-INFO-FILE.

      *=================================================================
       MAIN-PROGRAM-NEXT.

           ADD 01 TO RECORDS-READ.

           PERFORM READ-TRW-INFO-FILE-NEXT.
           IF ( IO-BAD )
              GO TO MAIN-PROGRAM-END.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( TRW-INFO-ID < BEG-ID )
              GO TO MAIN-PROGRAM-NEXT
           ELSE
           IF ( TRW-INFO-ID > END-ID )
              GO TO MAIN-PROGRAM-END.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE TRW-INFO-ID                TO D01-TRW-ID.
           MOVE TRW-INFO-EQUIFAX-ID        TO D01-TRW-EQUIFAX-ID.
           MOVE TRW-INFO-EXPERIAN-ID       TO D01-TRW-EXPERIAN-ID.
           MOVE TRW-INFO-TUC-ID            TO D01-TRW-TUC-ID.
           MOVE TRW-INFO-REPORTER-NAME     TO D01-TRW-REPORTER-NAME.
           MOVE TRW-INFO-REPORTER-PHONE    TO D01-TRW-REPORTER-PHONE.
           MOVE TRW-INFO-GRANTOR-CODE      TO D01-TRW-GRANTOR-CODE.

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
           MOVE "IOTWRN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOTWRN-QUEUE-POS  TO Q-POS.
           MOVE IOTWRN-COPIES-POS TO C-POS.
           MOVE IOTWRN-PRU-POS    TO P-POS.
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
       ENTER-BEG-ID.

           MOVE LEGEND-BEG-ID TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  N040BEG-ID." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-ID.
      *KC  IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-ID.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-ID.

      *-----------------------------------------------------------------
       ENTER-END-ID.

           MOVE LEGEND-END-ID TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  N040END-ID." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-ID.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-ID.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-ID.

           IF ( BEG-ID > END-ID )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-ID.

           GO TO ENTER-PROCESS-KEY.

      *-----------------------------------------------------------------
       ENTER-ALL-ID.

           MOVE "SN N040BEG-ID." TO VDU.
           MOVE 0                TO BEG-ID VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N040END-ID." TO VDU.
           MOVE 9999             TO END-ID VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-PROCESS-KEY.

           MOVE LEGEND-PROCESS-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN010PROCESS-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-ID.
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

           MOVE "S  N040BEG-ID."    TO VDU.
           MOVE BEG-ID              TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  N040END-ID."    TO VDU.
           MOVE END-ID              TO VDUNUM0.
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

           MOVE TRW-INFO-KEY TO PACE-DETAIL-KEY.

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
           MOVE "BEGINNING ID                  : "    TO R01-PROMPT.
           MOVE BEG-ID                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    ID                  : "    TO R01-PROMPT.
           MOVE END-ID                             TO R01-TEXT.
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
           MOVE "SQL TABLE IO UTILITY - TRWFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOTWRN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOTWRN-SYSDATE.
           DISPLAY IOTWRN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOTWRN-FORM-FIELDS TO WR-BUF.
           MOVE IOTWRN-FORM-LIST TO WR-LIST.
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

           MOVE IOTWRN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOTWRN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOTWRN_EVA.CPY".
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
      *    LOAD TRWINFO-REC FROM QTRWINFO-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-TRWINFO-FIELDS.
           MOVE QTRW-INFO-ID TO TRW-INFO-ID.
           MOVE QTRW-INFO-EQUIFAX-ID TO TRW-INFO-EQUIFAX-ID.
           MOVE QTRW-INFO-EXPERIAN-ID TO TRW-INFO-EXPERIAN-ID.
           MOVE QTRW-INFO-TUC-ID TO TRW-INFO-TUC-ID.
           MOVE QTRW-INFO-REPORTER-NAME TO TRW-INFO-REPORTER-NAME.
           MOVE QTRW-INFO-REPORTER-ADR1 TO TRW-INFO-REPORTER-ADR1.
           MOVE QTRW-INFO-REPORTER-ADR2 TO TRW-INFO-REPORTER-ADR2.
           MOVE QTRW-INFO-REPORTER-CSZ TO TRW-INFO-REPORTER-CSZ.
           MOVE QTRW-INFO-REPORTER-PHONE TO TRW-INFO-REPORTER-PHONE.
           MOVE QTRW-INFO-GRANTOR-CODE TO TRW-INFO-GRANTOR-CODE.
           MOVE QTRW-INFO-CR-LF TO TRW-INFO-CR-LF.
           MOVE QTRW-INFO-BRNO-ACCTNO TO TRW-INFO-BRNO-ACCTNO.
           MOVE QTRW-INFO-IDENT-BRNO TO TRW-INFO-IDENT-BRNO.
           MOVE QTRW-YEARS-STOP-REPORTING TO TRW-YEARS-STOP-REPORTING.
           MOVE QTRW-INVALID-SSNO TO TRW-INVALID-SSNO.
           MOVE QTRW-INFO-SCHED-PAY TO TRW-INFO-SCHED-PAY.

           IF ( QTRW-INFO-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO TRW-INFO-LTOUCH-DATE
           ELSE
              MOVE QTRW-INFO-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO TRW-INFO-LTOUCH-DATE.

 
      *-----------------------------------------------------------------
      *    LOAD QTRWINFO-REC FROM TRWINFO-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-TRWINFO-FIELDS.
           MOVE TRW-INFO-ID TO QTRW-INFO-ID.
           MOVE TRW-INFO-EQUIFAX-ID TO QTRW-INFO-EQUIFAX-ID.
           MOVE TRW-INFO-EXPERIAN-ID TO QTRW-INFO-EXPERIAN-ID.
           MOVE TRW-INFO-TUC-ID TO QTRW-INFO-TUC-ID.
           MOVE TRW-INFO-REPORTER-NAME TO QTRW-INFO-REPORTER-NAME.
           MOVE TRW-INFO-REPORTER-ADR1 TO QTRW-INFO-REPORTER-ADR1.
           MOVE TRW-INFO-REPORTER-ADR2 TO QTRW-INFO-REPORTER-ADR2.
           MOVE TRW-INFO-REPORTER-CSZ TO QTRW-INFO-REPORTER-CSZ.
           MOVE TRW-INFO-REPORTER-PHONE TO QTRW-INFO-REPORTER-PHONE.
           MOVE TRW-INFO-GRANTOR-CODE TO QTRW-INFO-GRANTOR-CODE.
           MOVE TRW-INFO-CR-LF TO QTRW-INFO-CR-LF.
           MOVE TRW-INFO-BRNO-ACCTNO TO QTRW-INFO-BRNO-ACCTNO.
           MOVE TRW-INFO-IDENT-BRNO TO QTRW-INFO-IDENT-BRNO.
           MOVE TRW-YEARS-STOP-REPORTING TO QTRW-YEARS-STOP-REPORTING.
           MOVE TRW-INVALID-SSNO TO QTRW-INVALID-SSNO.
           MOVE TRW-INFO-SCHED-PAY TO QTRW-INFO-SCHED-PAY.

           IF ( TRW-INFO-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QTRW-INFO-LTOUCH-DATE
           ELSE
              MOVE TRW-INFO-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QTRW-INFO-LTOUCH-DATE.

 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      * COPYMEMBER: LIBGB/GBTRWINFORN
       LOAD-TRW-INFO-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( TRW-INFO-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( TRW-INFO-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO TRW-INFO-PATH-BASE
              MOVE EXT-FILPATH-MACHINE     TO TRW-INFO-PATH-MACHINE
              MOVE TRW-INFO-PATH-FORMAT    TO TRW-INFO-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( TRW-INFO-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO TRW-INFO-PATH-BASE
              MOVE FILPATH-MACHINE         TO TRW-INFO-PATH-MACHINE
              MOVE TRW-INFO-PATH-FORMAT    TO TRW-INFO-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-TRWINFO NOT = "S" )
              MOVE TRW-INFO-PATH-BEG       TO TRW-INFO-SELECT-PATH-BEG
              MOVE TRW-INFO-PATH-FILE      TO TRW-INFO-SELECT-PATH-FILE
           ELSE
              MOVE TRW-INFO-PATH-SQL       TO TRW-INFO-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-TRW-INFO-FILE.
           PERFORM LOAD-TRW-INFO-FILE.
           PERFORM OPEN-IT.
           MOVE TRW-INFO-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-TRW-INFO-FILE.
           PERFORM START-IT.
           MOVE TRW-INFO-SELECT-PATH TO E-FILE.
           MOVE TRW-INFO-KEY TO E-KEYX.

           IF ( TRW-INFO-CURSOR-STAT-GOOD )
              PERFORM CLOSE-TRW-INFO-FILE.

           IF ( QTRW-INFO-WEND-INFO-ID = ZEROES    ) OR
              ( QTRW-INFO-WEND-INFO-ID NOT NUMERIC )
              MOVE TRW-INFO-ID         TO QTRW-INFO-WBEG-INFO-ID
              MOVE 9999                TO QTRW-INFO-WEND-INFO-ID.

           EXEC SQL
            DECLARE TRWINFO_CURSOR CURSOR FOR
            SELECT TRWINFO.TRW_INFO_ID,
                   ISNULL(TRWINFO.TRW_INFO_EQUIFAX_ID,''),
                   ISNULL(TRWINFO.TRW_INFO_EXPERIAN_ID,''),
                   ISNULL(TRWINFO.TRW_INFO_TUC_ID,''),
                   ISNULL(TRWINFO.TRW_INFO_REPORTER_NAME,''),
                   ISNULL(TRWINFO.TRW_INFO_REPORTER_ADR1,''),
                   ISNULL(TRWINFO.TRW_INFO_REPORTER_ADR2,''),
                   ISNULL(TRWINFO.TRW_INFO_REPORTER_CSZ,''),
                   TRWINFO.TRW_INFO_REPORTER_PHONE,
                   ISNULL(TRWINFO.TRW_INFO_GRANTOR_CODE,''),
                   ISNULL(TRWINFO.TRW_INFO_CR_LF,''),
                   ISNULL(TRWINFO.TRW_INFO_BRNO_ACCTNO,''),
                   ISNULL(TRWINFO.TRW_INFO_IDENT_BRNO,''),
                   TRWINFO.TRW_YEARS_STOP_REPORTING,
                   ISNULL(TRWINFO.TRW_INVALID_SSNO,''),
                   ISNULL(TRWINFO.TRW_INFO_SCHED_PAY,''),
                   ISNULL(CAST(TRWINFO.TRW_INFO_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0')
            FROM DBO.TRWINFO
            WHERE TRWINFO.TRW_INFO_ID >= :QTRW-INFO-WBEG-INFO-ID
              AND TRWINFO.TRW_INFO_ID <= :QTRW-INFO-WEND-INFO-ID
            ORDER BY TRWINFO.TRW_INFO_ID
           END-EXEC.

           EXEC SQL
               OPEN TRWINFO_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QTRW-INFO-WBEG-INFO-ID
                          QTRW-INFO-WEND-INFO-ID.

           MOVE STAT---GOOD TO TRW-INFO-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-TRW-INFO-FILE.
           PERFORM READ-IT.
           MOVE TRW-INFO-SELECT-PATH TO E-FILE.
           MOVE TRW-INFO-KEY TO E-KEYX.

           MOVE TRW-INFO-ID  TO QTRW-INFO-ID. 

           EXEC SQL
            SELECT TRWINFO.TRW_INFO_ID,
                   ISNULL(TRWINFO.TRW_INFO_EQUIFAX_ID,''),
                   ISNULL(TRWINFO.TRW_INFO_EXPERIAN_ID,''),
                   ISNULL(TRWINFO.TRW_INFO_TUC_ID,''),
                   ISNULL(TRWINFO.TRW_INFO_REPORTER_NAME,''),
                   ISNULL(TRWINFO.TRW_INFO_REPORTER_ADR1,''),
                   ISNULL(TRWINFO.TRW_INFO_REPORTER_ADR2,''),
                   ISNULL(TRWINFO.TRW_INFO_REPORTER_CSZ,''),
                   TRWINFO.TRW_INFO_REPORTER_PHONE,
                   ISNULL(TRWINFO.TRW_INFO_GRANTOR_CODE,''),
                   ISNULL(TRWINFO.TRW_INFO_CR_LF,''),
                   ISNULL(TRWINFO.TRW_INFO_BRNO_ACCTNO,''),
                   ISNULL(TRWINFO.TRW_INFO_IDENT_BRNO,''),
                   TRWINFO.TRW_YEARS_STOP_REPORTING,
                   ISNULL(TRWINFO.TRW_INVALID_SSNO,''),
                   ISNULL(TRWINFO.TRW_INFO_SCHED_PAY,''),
                   ISNULL(CAST(TRWINFO.TRW_INFO_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0')
            INTO :QTRWINFO-REC
            FROM DBO.TRWINFO
            WHERE TRWINFO.TRW_INFO_ID = :QTRW-INFO-ID
              AND TRWINFO.TRW_INFO_ID = :QTRW-INFO-ID
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRW-INFO-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-TRWINFO-FIELDS.

      *-----------------------------------------------------------------
       READ-TRW-INFO-FILE-NEXT.
           PERFORM READ-IT.
           MOVE TRW-INFO-SELECT-PATH TO E-FILE.
           MOVE TRW-INFO-KEY TO E-KEYX.
           INITIALIZE QTRWINFO-REC.

           IF ( TRW-INFO-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH TRWINFO_CURSOR INTO :QTRWINFO-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TRW-INFO-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-TRWINFO-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-TRW-INFO-FILE.
           IF ( TRW-INFO-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE TRWINFO_CURSOR
              END-EXEC
              MOVE STAT---BAD TO TRW-INFO-CURSOR-STAT.

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
              DESTROY IOTWRN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
