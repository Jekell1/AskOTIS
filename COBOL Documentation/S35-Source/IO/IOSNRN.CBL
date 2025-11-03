       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOSNRN.
       AUTHOR.        BAH
       DATE-WRITTEN.  03-11-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOSNRN
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : SNFILE 
      * TABLE DESC    : USER SECURITY 
      * OUTPUT FILE   : XPC/IOSNRN.PRE
      * USED FOR      : LIBGB/GBWSSN.CPY  --- QSN-REC
      *                 LIBGB/GBSN1R.CPY  --- READ
      *                 LIBGB/GBSN1RN.CPY --- READ/READ-NEXT/START
      *                 LIBGB/GBSN2RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * BAH 2021-0316 ***NEW***
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
           "@(#)IO/IOSNRN.CBL S34 07/22/21-14:08:57 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01SN.CPY".
       COPY "LIBGB/GBWSSN.CPY".

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
       01  QSN1-WHERE-BEG-KEY.
           03  QSN1-WBEG-FORM           PIC X(6).

       01  QSN1-WHERE-END-KEY.
           03  QSN1-WEND-FORM           PIC X(6).

       01  QSN2-WHERE-BEG-KEY.
           03  QSN2-WBEG-SEQ            PIC 9(8).
           03  QSN2-WBEG-FORM           PIC X(6).

       01  QSN2-WHERE-END-KEY.
           03  QSN2-WEND-SEQ            PIC 9(8).
           03  QSN2-WEND-FORM           PIC X(6).


       01  QSN-REC.
           03  QSN-SEQ              PIC 9(8).
           03  QSN-FORM             PIC X(6).
           03  QSN-LTOUCH-DATE          PIC X(10).
           03  QSN-DESC                 PIC X(30).
           03  QSN-PURCHASABLE          PIC X.

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

       01  LEGEND-BEG-PROGRAM. 
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING PROGRAM    (F3-ALL) ".

       01  LEGEND-END-PROGRAM.
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING PROGRAM    ".

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
           03  BEG-PROGRAM               PIC X(06).
           03  END-PROGRAM               PIC X(06).
           03  BEG-SEQ                   PIC 9(08).
           03  END-SEQ                   PIC 9(08).

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
           03  FILLER PIC X(31) VALUE "PROGRM   LTOUCH     DESC".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(12) VALUE SPACES.

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-SN-FORM                    PIC X(6).
           03  FILLER                         PIC X(03).
           03  D01-SN-LTOUCH-DATE             PIC 9(8).
           03  FILLER                         PIC X(03).
           03  D01-SN-DESC                    PIC X(30).
           03  FILLER                         PIC X(03).
           03  D01-SN-PURCHASABLE             PIC X.
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
       COPY "LIBIO/IOSNRN_DEF.CPY".
       COPY "LIBIO/IOSNRN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOSNRN_SCN.CPY".

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
           MOVE "B"                        TO SN-PATH-OVERRIDE.

           MOVE BEG-PROGRAM                TO QSN1-WBEG-FORM
                                              QSN2-WBEG-FORM.
           MOVE END-PROGRAM                TO QSN1-WEND-FORM
                                              QSN2-WEND-FORM.
           MOVE BEG-SEQ                    TO QSN2-WBEG-SEQ.
           MOVE END-SEQ                    TO QSN2-WEND-SEQ.
   
      D    STOP MESS.
           IF ( PROCESS-KEY = 1 )
              PERFORM OPEN-SN1-FILE
              PERFORM START-SN1-FILE
           ELSE
              PERFORM OPEN-SN2-FILE
              PERFORM START-SN2-FILE.

      *=================================================================
       MAIN-PROGRAM-NEXT.

           ADD 01 TO RECORDS-READ.

           IF ( PROCESS-KEY = 1 )
              PERFORM READ-SN1-FILE-NEXT
           ELSE
              PERFORM READ-SN2-FILE-NEXT.
           IF ( IO-BAD )
              GO TO MAIN-PROGRAM-END.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF PROCESS-KEY = 1
              IF ( SN-FORM < BEG-PROGRAM )
                 GO TO MAIN-PROGRAM-NEXT
              ELSE
              IF ( SN-FORM > END-PROGRAM )
                 GO TO MAIN-PROGRAM-END.

           IF PROCESS-KEY = 2
              IF ( SN-SEQ < BEG-SEQ )
                 GO TO MAIN-PROGRAM-NEXT
              ELSE
              IF ( SN-SEQ > END-SEQ )
                 GO TO MAIN-PROGRAM-END
              ELSE
              IF SN-FORM < BEG-PROGRAM OR > END-PROGRAM
                 GO TO MAIN-PROGRAM-NEXT.
         
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE SN-FORM               TO D01-SN-FORM.
           MOVE SN-LTOUCH-DATE        TO D01-SN-LTOUCH-DATE.
           MOVE SN-DESC               TO D01-SN-DESC.
           MOVE SN-PURCHASABLE        TO D01-SN-PURCHASABLE.
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
           MOVE "IOSNRN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOSNRN-QUEUE-POS  TO Q-POS.
           MOVE IOSNRN-COPIES-POS TO C-POS.
           MOVE IOSNRN-PRU-POS    TO P-POS.
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
       ENTER-BEG-PROGRAM.

           MOVE LEGEND-BEG-PROGRAM TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A060BEG-PROGRAM." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-PROGRAM.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-PROGRAM.

           MOVE VDUBUF TO BEG-PROGRAM.

      *-----------------------------------------------------------------
       ENTER-END-PROGRAM.

           MOVE LEGEND-END-PROGRAM TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A060END-PROGRAM." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-PROGRAM.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-PROGRAM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-PROGRAM.

           IF ( BEG-PROGRAM > END-PROGRAM )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-PROGRAM.

           GO TO ENTER-BEG-SEQ.

      *-----------------------------------------------------------------
       ENTER-ALL-PROGRAM.

           MOVE "S  A060BEG-PROGRAM." TO VDU.
           MOVE SPACES               TO BEG-PROGRAM VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A060END-PROGRAM." TO VDU.
           MOVE ALL "Z"              TO END-PROGRAM VDUBUF.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-SEQ.

           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN080BEG-SEQ." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        UP-KEY ) GO TO ENTER-END-PROGRAM.
           IF (        F3-KEY ) GO TO ENTER-ALL-SEQ.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-SEQ.

           MOVE VDUNUM0 TO BEG-SEQ.

      *-----------------------------------------------------------------
       ENTER-END-SEQ.

           MOVE "ENNN080END-SEQ." TO VDU.
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

           GO TO ENTER-PROCESS-KEY.

      *-----------------------------------------------------------------
       ENTER-ALL-SEQ.

           MOVE "SNNN080BEG-SEQ." TO VDU.
           MOVE 0                 TO BEG-SEQ VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN080END-SEQ." TO VDU.
           MOVE ALL "9"              TO END-SEQ VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-PROCESS-KEY.

           MOVE LEGEND-PROCESS-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN010PROCESS-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-SEQ.
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

           MOVE "S  A000BEG-PROGRAM."    TO VDU.
           MOVE BEG-PROGRAM              TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A000END-PROGRAM."    TO VDU.
           MOVE END-PROGRAM              TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SNNN080BEG-SEQ."    TO VDU.
           MOVE BEG-SEQ              TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN080END-SEQ."    TO VDU.
           MOVE END-SEQ              TO VDUNUM0.
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

           MOVE SN1-KEY     TO PACE-DETAIL-KEY.

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
           MOVE "BEGINNING PROGRAM               : "    TO R01-PROMPT.
           MOVE BEG-PROGRAM                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    PROGRAM               : "    TO R01-PROMPT.
           MOVE END-PROGRAM                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING SEQ               : "    TO R01-PROMPT.
           MOVE BEG-SEQ                             TO R01-NUMERIC.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    SEQ               : "    TO R01-PROMPT.
           MOVE END-SEQ                             TO R01-NUMERIC.
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
           MOVE "SQL TABLE IO UTILITY - SNFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOSNRN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOSNRN-SYSDATE.
           DISPLAY IOSNRN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOSNRN-FORM-FIELDS TO WR-BUF.
           MOVE IOSNRN-FORM-LIST TO WR-LIST.
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

           MOVE IOSNRN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOSNRN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOSNRN_EVA.CPY".
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
      *    LOAD SN-REC FROM QSN-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-SN-FIELDS.
           MOVE QSN-SEQ TO SN-SEQ.
           MOVE QSN-FORM TO SN-FORM.

           IF ( QSN-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO SN-LTOUCH-DATE
           ELSE
              MOVE QSN-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO SN-LTOUCH-DATE.

           MOVE QSN-DESC TO SN-DESC.
           MOVE QSN-PURCHASABLE TO SN-PURCHASABLE.
 
      *-----------------------------------------------------------------
      *    LOAD QSN-REC FROM SN-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-SN-FIELDS.
           MOVE SN-SEQ TO QSN-SEQ.
           MOVE SN-FORM TO QSN-FORM.

           IF ( SN-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QSN-LTOUCH-DATE
           ELSE
              MOVE SN-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QSN-LTOUCH-DATE.

           MOVE SN-DESC TO QSN-DESC.
           MOVE SN-PURCHASABLE TO QSN-PURCHASABLE.
 

      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
       LOAD-SN1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( SN-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( SN-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO SN-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO SN-ALL-MACHINE
              MOVE SN-ALL-PATH             TO SN-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( SN-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO SN-ALL-BASE
              MOVE FILPATH-MACHINE         TO SN-ALL-MACHINE
              MOVE SN-ALL-PATH             TO SN-PATH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-SEFILE NOT = "S" )
              MOVE SN-PATH-BEG             TO SN-SELECT-PATH-BEG
              MOVE SN-PATH-FILE            TO SN-SELECT-PATH-FILE
           ELSE
              MOVE SN-PATH-SQL             TO SN-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-SN1-FILE.
           PERFORM LOAD-SN1-FILE.
           PERFORM OPEN-IT.
           MOVE SN-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-SN1-FILE.
           PERFORM START-IT.
           MOVE SN-SELECT-PATH TO E-FILE.
           MOVE SN1-KEY TO E-KEYX.

           IF ( SN1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-SN1-FILE.

           IF ( QSN1-WEND-FORM = SPACES    )
               MOVE SN-FORM   TO QSN1-WBEG-FORM
               MOVE ALL "Z"   TO QSN1-WEND-FORM.

           EXEC SQL
            DECLARE SN1_CURSOR CURSOR FOR
            SELECT SNFILE.SN_SEQ,
                   ISNULL(SNFILE.SN_FORM,''),
                   ISNULL(CAST(SNFILE.SN_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(SNFILE.SN_DESC,''),
                   ISNULL(SNFILE.SN_PURCHASABLE,'')
            FROM DBO.SNFILE
            WHERE SNFILE.SN_FORM >= :QSN1-WBEG-FORM
              AND SNFILE.SN_FORM <= :QSN1-WEND-FORM
            ORDER BY SNFILE.SN_FORM
           END-EXEC.

           EXEC SQL
               OPEN SN1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QSN1-WBEG-FORM
                          QSN1-WEND-FORM.

           MOVE STAT---GOOD TO SN1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-SN1-FILE.
           PERFORM READ-IT.
           MOVE SN-SELECT-PATH TO E-FILE.
           MOVE SN1-KEY TO E-KEYX.

           MOVE SN-FORM    TO QSN-FORM.

           EXEC SQL
            SELECT SNFILE.SN_SEQ,
                   ISNULL(SNFILE.SN_FORM,''),
                   ISNULL(CAST(SNFILE.SN_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(SNFILE.SN_DESC,''),
                   ISNULL(SNFILE.SN_PURCHASABLE,'')
            INTO :QSN-REC
            FROM DBO.SNFILE
            WHERE SNFILE.SN_FORM = :QSN-FORM
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SN1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-SN-FIELDS.

      *-----------------------------------------------------------------
       READ-SN1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE SN-SELECT-PATH TO E-FILE.
           MOVE SN1-KEY TO E-KEYX.
           INITIALIZE QSN-REC.

           IF ( SN1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH SN1_CURSOR INTO :QSN-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SN1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-SN-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-SN1-FILE.
           IF ( SN1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE SN1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO SN1-CURSOR-STAT.

      *#################################################################
      *    ENDING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
       LOAD-SN2-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( SN-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( SN-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO SN-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO SN-ALL-MACHINE
              MOVE SN-ALL-PATH             TO SN-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( SN-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO SN-ALL-BASE
              MOVE FILPATH-MACHINE         TO SN-ALL-MACHINE
              MOVE SN-ALL-PATH             TO SN-PATH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-SEFILE NOT = "S" )
              MOVE SN-PATH-BEG             TO SN-SELECT-PATH-BEG
              MOVE SN-PATH-FILE            TO SN-SELECT-PATH-FILE
           ELSE
              MOVE SN-PATH-SQL             TO SN-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-SN2-FILE.
           PERFORM LOAD-SN2-FILE.
           PERFORM OPEN-IT.
           MOVE SN-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-SN2-FILE.
           PERFORM START-IT.
           MOVE SN-SELECT-PATH TO E-FILE.
           MOVE SN2-KEY TO E-KEYX.

           IF ( SN2-CURSOR-STAT-GOOD )
              PERFORM CLOSE-SN2-FILE.

           IF ( QSN2-WEND-SEQ = 0    ) OR
              ( QSN2-WEND-SEQ NOT NUMERIC )
               MOVE SN-SEQ    TO QSN2-WBEG-SEQ
               MOVE ALL "9"   TO QSN2-WEND-SEQ
               MOVE SN-FORM   TO QSN2-WBEG-FORM
               MOVE ALL "Z"   TO QSN2-WEND-FORM.

           EXEC SQL
            DECLARE SN2_CURSOR CURSOR FOR
            SELECT SNFILE.SN_SEQ,
                   ISNULL(SNFILE.SN_FORM,''),
                   ISNULL(CAST(SNFILE.SN_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(SNFILE.SN_DESC,''),
                   ISNULL(SNFILE.SN_PURCHASABLE,'')
            FROM DBO.SNFILE
            WHERE SNFILE.SN_SEQ >= :QSN2-WBEG-SEQ
              AND SNFILE.SN_SEQ <= :QSN2-WEND-SEQ
              AND SNFILE.SN_FORM >= :QSN2-WBEG-FORM
              AND SNFILE.SN_FORM <= :QSN2-WEND-FORM
            ORDER BY SNFILE.SN_SEQ,
                     SNFILE.SN_FORM
           END-EXEC.

           EXEC SQL
               OPEN SN2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE 0      TO QSN2-WBEG-SEQ
                          QSN2-WEND-SEQ.
           MOVE SPACES TO QSN2-WBEG-FORM
                          QSN2-WEND-FORM.

           MOVE STAT---GOOD TO SN2-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-SN2-FILE.
           PERFORM READ-IT.
           MOVE SN-SELECT-PATH TO E-FILE.
           MOVE SN2-KEY TO E-KEYX.

           MOVE SN-SEQ     TO QSN-SEQ.
           MOVE SN-FORM    TO QSN-FORM.

           EXEC SQL
            SELECT SNFILE.SN_SEQ,
                   ISNULL(SNFILE.SN_FORM,''),
                   ISNULL(CAST(SNFILE.SN_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(SNFILE.SN_DESC,''),
                   ISNULL(SNFILE.SN_PURCHASABLE,'')
            INTO :QSN-REC
            FROM DBO.SNFILE
            WHERE SNFILE.SN_SEQ = :QSN-SEQ
              AND SNFILE.SN_FORM = :QSN-FORM
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SN2-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-SN-FIELDS.

      *-----------------------------------------------------------------
       READ-SN2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE SN-SELECT-PATH TO E-FILE.
           MOVE SN2-KEY TO E-KEYX.
           INITIALIZE QSN-REC.

           IF ( SN2-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH SN2_CURSOR INTO :QSN-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-SN2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-SN-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-SN2-FILE.
           IF ( SN2-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE SN2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO SN2-CURSOR-STAT.

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
              DESTROY IOSNRN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
