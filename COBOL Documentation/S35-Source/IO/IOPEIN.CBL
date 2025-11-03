       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOPEIN.
       AUTHOR.        BAH.
       DATE-WRITTEN.  03-10-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOPEIN
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : PEFILE 
      * TABLE DESC    : PASSWORD EXCEPTION FILE
      * OUTPUT FILE   : XPC/IOPEIN.PRE
      * USED FOR      : LIBGB/GBWSPECPY  --- QPE-REC
      *                 LIBGB/GBPE1R.CPY  --- READ
      *                 LIBGB/GBPE1RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * BAH 2021-0310 ***NEW***
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
           "@(#)IO/IOPEIN.CBL S34 05/18/21-11:03:18 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGB/GB01PE.CPY".
       COPY "LIBGB/GBWSPE.CPY".

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

       01  QPE1-WHERE-BEG-KEY.
           03  QPE1-WBEG-PGM            PIC X(10).
           03  QPE1-WBEG-SEQ            PIC 9(03).
           03  QPE1-WBEG-LOGIN          PIC X(08).

       01  QPE1-WHERE-END-KEY.
           03  QPE1-WEND-PGM            PIC X(10).
           03  QPE1-WEND-SEQ            PIC 9(03).
           03  QPE1-WEND-LOGIN          PIC X(08).

       01  QPE-REC.
           03  QPE-PGM                  PIC X(10).
           03  QPE-SEQ                  PIC 9(03).
           03  QPE-LOGIN                PIC X(8).
           03  QPE-LTOUCH-DATE          PIC X(10).

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
      *01  SMALL-Z                     PIC X(10)     VALUE "ZZZZZZZZZZ".

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
           03  BEG-PROGRAM               PIC X(10).
           03  END-PROGRAM               PIC X(10).
           03  BEG-SEQ                   PIC 9(3).
           03  END-SEQ                   PIC 9(3).
           03  BEG-LOGIN                 PIC X(8).
           03  END-LOGIN                 PIC X(8).

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
           03  FILLER PIC X(30) VALUE "PROGRAM    SEQ LOGIN   LTOUCH ".
           03  FILLER PIC X(30) VALUE "              DESC-1          ".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(12) VALUE SPACES.

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-PE-PROGRAM                 PIC X(10).
           03  FILLER                         PIC X(01).
           03  D01-PE-SEQ                     PIC 9(3).
           03  FILLER                         PIC X(01).
           03  D01-PE-LOGIN                   PIC X(8).
           03  FILLER                         PIC X(01).
           03  D01-PE-LTOUCH                  PIC 9(8).

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
       COPY "LIBIO/IOPEIN_DEF.CPY".
       COPY "LIBIO/IOPEIN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOPEIN_SCN.CPY".

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
           MOVE "B"                        TO PE-PATH-OVERRIDE.

           MOVE BEG-PROGRAM                TO QPE1-WBEG-PGM.
           MOVE BEG-SEQ                    TO QPE1-WBEG-SEQ.
           MOVE BEG-LOGIN                  TO QPE1-WBEG-LOGIN.
           MOVE END-PROGRAM                TO QPE1-WEND-PGM.
           MOVE END-SEQ                    TO QPE1-WEND-SEQ.
           MOVE END-LOGIN                  TO QPE1-WEND-LOGIN.

           PERFORM OPEN-PE1-FILE.
           PERFORM START-PE1-FILE.

       MAIN-PROGRAM-NEXT.

           ADD 01 TO RECORDS-READ.

           PERFORM READ-PE1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO MAIN-PROGRAM-END.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( PE-PGM < BEG-PROGRAM )
              GO TO MAIN-PROGRAM-NEXT
           ELSE
           IF ( PE-PGM > END-PROGRAM )
              GO TO MAIN-PROGRAM-END.

      D    STOP MESS.
           IF ( PE-SEQ < BEG-SEQ ) OR (PE-SEQ > END-SEQ)
              GO TO MAIN-PROGRAM-NEXT.
           IF ( PE-LOGIN < BEG-LOGIN ) OR (PE-LOGIN > END-LOGIN)
              GO TO MAIN-PROGRAM-NEXT.
      D    STOP MESS.

           MOVE PE-PGM                     TO D01-PE-PROGRAM.
           MOVE PE-SEQ                     TO D01-PE-SEQ.
           MOVE PE-LTOUCH-DATE             TO D01-PE-LTOUCH.
           MOVE PE-LOGIN                   TO D01-PE-LOGIN.

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

      *KC  CALL "C$TOLOWER" USING SMALL-Z, VALUE 10.
           MOVE "X"               TO RECORDS-FOUND.
           MOVE FORM-PATHNAME     TO FORM-PATH.
           MOVE "IOPEIN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOPEIN-QUEUE-POS  TO Q-POS.
           MOVE IOPEIN-COPIES-POS TO C-POS.
           MOVE IOPEIN-PRU-POS    TO P-POS.
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

           MOVE "E  A100BEG-PROGRAM." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-PROGRAM.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-PROGRAM.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-PROGRAM.

      *-----------------------------------------------------------------
       ENTER-END-PROGRAM.

           MOVE LEGEND-END-PROGRAM TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A100END-PROGRAM." TO VDU.
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

           MOVE "S  A100BEG-PROGRAM." TO VDU.
           MOVE SPACES               TO BEG-PROGRAM VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A100END-PROGRAM." TO VDU.
      *KC  MOVE SMALL-Z               TO END-PROGRAM VDUBUF.
           MOVE ALL "Z"               TO END-PROGRAM
           CALL "C$TOLOWER" USING END-PROGRAM, VALUE 10.
           MOVE END-PROGRAM           TO VDUBUF.
           PERFORM SCREENIO.

       ENTER-BEG-SEQ.

           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  N030BEG-SEQ." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-SEQ.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-SEQ.

           MOVE VDUNUM0 TO BEG-SEQ.

      *-----------------------------------------------------------------
       ENTER-END-SEQ.

           MOVE "E  N030END-SEQ." TO VDU.
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

           GO TO ENTER-BEG-LOGIN.

      *-----------------------------------------------------------------
       ENTER-ALL-SEQ.

           MOVE "SN N030BEG-SEQ." TO VDU.
           MOVE 0                 TO BEG-SEQ VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  N030END-SEQ." TO VDU.
           MOVE 999               TO END-SEQ VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       ENTER-BEG-LOGIN.

           MOVE "E  A080BEG-LOGIN." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-LOGIN.
           IF (        UP-KEY ) GO TO ENTER-END-SEQ.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-LOGIN.

           MOVE VDUBUF TO BEG-LOGIN.

      *-----------------------------------------------------------------
       ENTER-END-LOGIN.

           MOVE "E  A080END-LOGIN." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-LOGIN.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-PROGRAM.

           MOVE VDUBUF TO END-LOGIN.

           IF ( BEG-LOGIN > END-LOGIN )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-LOGIN.

           GO TO ENTER-PROCESS-KEY.
      *-----------------------------------------------------------------
       ENTER-ALL-LOGIN.

           MOVE "S  A080BEG-LOGIN." TO VDU.
           MOVE SPACES               TO BEG-LOGIN VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A080END-LOGIN." TO VDU.
      *KC  MOVE SMALL-Z             TO END-LOGIN VDUBUF.
           MOVE ALL "Z"             TO END-LOGIN.
           CALL "C$TOLOWER" USING END-LOGIN, VALUE 8.
           MOVE END-LOGIN           TO VDUBUF.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-PROCESS-KEY.

           MOVE LEGEND-PROCESS-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN010PROCESS-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-PROGRAM.
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

       ENTER-ACCEPT-YN.

           MOVE LEGEND-ACCEPT-YN TO LEGEND.
           PERFORM SEND-LEGEND.

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

           MOVE "S  N030BEG-SEQ."    TO VDU.
           MOVE BEG-SEQ              TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  N030END-SEQ."    TO VDU.
           MOVE END-SEQ              TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A000BEG-LOGIN."    TO VDU.
           MOVE BEG-LOGIN              TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A000END-LOGIN."    TO VDU.
           MOVE END-LOGIN              TO VDUBUF.
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

           MOVE PE1-KEY     TO PACE-DETAIL-KEY.

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

            MOVE "BEGINNING SEQ               : "    TO R01-PROMPT.
            MOVE BEG-SEQ                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

            MOVE "ENDING    SEQ               : "    TO R01-PROMPT.
            MOVE END-SEQ                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

            MOVE "BEGINNING LOGIN               : "    TO R01-PROMPT.
            MOVE BEG-LOGIN                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

            MOVE "ENDING    LOGIN               : "    TO R01-PROMPT.
            MOVE END-LOGIN                             TO R01-TEXT.
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
           MOVE "SQL TABLE IO UTILITY - PEFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOPEIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOPEIN-SYSDATE.
           DISPLAY IOPEIN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOPEIN-FORM-FIELDS TO WR-BUF.
           MOVE IOPEIN-FORM-LIST TO WR-LIST.
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

           MOVE IOPEIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOPEIN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOPEIN_EVA.CPY".
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
      *    LOAD PE-REC FROM QPE-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-PE-FIELDS.
           MOVE QPE-PGM TO PE-PGM.
           MOVE QPE-SEQ TO PE-SEQ.
           MOVE QPE-LOGIN TO PE-LOGIN.

           IF ( QPE-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO PE-LTOUCH-DATE
           ELSE
              MOVE QPE-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO PE-LTOUCH-DATE.

 
      *-----------------------------------------------------------------
      *    LOAD QPE-REC FROM PE-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-PE-FIELDS.
           MOVE PE-PGM TO QPE-PGM.
           MOVE PE-SEQ TO QPE-SEQ.
           MOVE PE-LOGIN TO QPE-LOGIN.

           IF ( PE-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QPE-LTOUCH-DATE
           ELSE
              MOVE PE-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QPE-LTOUCH-DATE.

 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
       LOAD-PE1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( PE-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( PE-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO PE-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO PE-ALL-MACHINE
              MOVE PE-ALL-PATH             TO PE-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( PE-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO PE-ALL-BASE
              MOVE FILPATH-MACHINE         TO PE-ALL-MACHINE
              MOVE PE-ALL-PATH             TO PE-PATH.

*G2G  *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*G2G  *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
*G2G  *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-PEFILE NOT = "S" )
*G2G          MOVE PE-PATH-BEG             TO PE-SELECT-PATH-BEG
*G2G          MOVE PE-PATH-FILE            TO PE-SELECT-PATH-FILE
*G2G       ELSE
*G2G          MOVE PE-PATH-SQL             TO PE-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-PE1-FILE.
           PERFORM LOAD-PE1-FILE.
           PERFORM OPEN-IT.
           MOVE PE-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-PE1-FILE.
           PERFORM START-IT.
           MOVE PE-SELECT-PATH TO E-FILE.
           MOVE PE1-KEY TO E-KEYX.

           IF ( PE1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-PE1-FILE.

           IF ( QPE1-WEND-PGM = SPACES    ) OR
              ( QPE1-WEND-SEQ = 0 )
              MOVE PE-PGM              TO QPE1-WBEG-PGM
              MOVE ALL "Z"             TO QPE1-WEND-PGM
              MOVE PE-LOGIN            TO QPE1-WBEG-LOGIN
              MOVE ALL "Z"             TO QPE1-WEND-LOGIN
              CALL "C$TOLOWER" USING QPE1-WEND-LOGIN, VALUE 8
              MOVE PE-SEQ              TO QPE1-WBEG-SEQ
              MOVE 999                 TO QPE1-WEND-SEQ.

           EXEC SQL
            DECLARE PE1_CURSOR CURSOR FOR
            SELECT ISNULL(PEFILE.PE_PGM,''),
                   PEFILE.PE_SEQ,
                   ISNULL(PEFILE.PE_LOGIN,''),
                   ISNULL(CAST(PEFILE.PE_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0')
            FROM DBO.PEFILE
            WHERE PEFILE.PE_PGM >= :QPE1-WBEG-PGM
              AND PEFILE.PE_PGM <= :QPE1-WEND-PGM
              AND PEFILE.PE_SEQ >= :QPE1-WBEG-SEQ
              AND PEFILE.PE_SEQ <= :QPE1-WEND-SEQ
              AND PEFILE.PE_LOGIN >= :QPE1-WBEG-LOGIN
              AND PEFILE.PE_LOGIN <= :QPE1-WEND-LOGIN
            ORDER BY PEFILE.PE_PGM,
                     PEFILE.PE_SEQ,
                     PEFILE.PE_LOGIN
           END-EXEC.

           EXEC SQL
               OPEN PE1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QPE1-WBEG-PGM
                          QPE1-WEND-PGM
                          QPE1-WBEG-LOGIN
                          QPE1-WEND-LOGIN.
           MOVE ZEROES TO QPE1-WBEG-SEQ
                          QPE1-WEND-SEQ.

           MOVE STAT---GOOD TO PE1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-PE1-FILE.
           PERFORM READ-IT.
           MOVE PE-SELECT-PATH TO E-FILE.
           MOVE PE1-KEY TO E-KEYX.

           MOVE PE-PGM    TO QPE-PGM.
           MOVE PE-SEQ    TO QPE-SEQ.
           MOVE PE-LOGIN  TO QPE-LOGIN.

           EXEC SQL
            SELECT ISNULL(PEFILE.PE_PGM,''),
                   PEFILE.PE_SEQ,
                   ISNULL(PEFILE.PE_LOGIN,''),
                   ISNULL(CAST(PEFILE.PE_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0')
            INTO :QPE-REC
            FROM DBO.PEFILE
            WHERE PEFILE.PE_PGM = :QPE-PGM
              AND PEFILE.PE_SEQ = :QPE-SEQ
              AND PEFILE.PE_LOGIN = :QPE-LOGIN
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-PE1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-PE-FIELDS.

      *-----------------------------------------------------------------
       READ-PE1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE PE-SELECT-PATH TO E-FILE.
           MOVE PE1-KEY TO E-KEYX.
           INITIALIZE QPE-REC.

           IF ( PE1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH PE1_CURSOR INTO :QPE-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-PE1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-PE-FIELDS.


      *-----------------------------------------------------------------
       WRITE-PE1-FILE.

           ACCEPT PE-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE PE-SELECT-PATH TO E-FILE.
           MOVE PE1-KEY TO E-KEYX.
           PERFORM SET-PE-FIELDS.

           EXEC SQL
            INSERT INTO DBO.PEFILE
             (PEFILE.PE_PGM,
              PEFILE.PE_SEQ,
              PEFILE.PE_LOGIN,
              PEFILE.PE_LTOUCH_DATE)
            VALUES
             (:QPE-PGM,
              :QPE-SEQ,
              :QPE-LOGIN,
              NULLIF(:QPE-LTOUCH-DATE,'1900-01-01'))
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-PE1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-PE1-FILE.

           ACCEPT PE-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE PE-SELECT-PATH TO E-FILE.
           MOVE PE1-KEY TO E-KEYX.
           PERFORM SET-PE-FIELDS.

           EXEC SQL
            UPDATE DBO.PEFILE
             SET
              PEFILE.PE_PGM = :QPE-PGM,
              PEFILE.PE_SEQ = :QPE-SEQ,
              PEFILE.PE_LOGIN = :QPE-LOGIN,
              PEFILE.PE_LTOUCH_DATE =
                 NULLIF(:QPE-LTOUCH-DATE,'1900-01-01')
            WHERE PEFILE.PE_PGM = :QPE-PGM
              AND PEFILE.PE_SEQ = :QPE-SEQ
              AND PEFILE.PE_LOGIN = :QPE-LOGIN
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-PE1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-PE1-FILE.

           PERFORM DELETE-IT.
           MOVE PE-SELECT-PATH TO E-FILE.
           MOVE PE1-KEY TO E-KEYX.
           PERFORM SET-PE-FIELDS.

           EXEC SQL
            DELETE FROM DBO.PEFILE
            WHERE PEFILE.PE_PGM = :QPE-PGM
              AND PEFILE.PE_SEQ = :QPE-SEQ
              AND PEFILE.PE_LOGIN = :QPE-LOGIN
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-PE1-FILE.

      *-----------------------------------------------------------------
       CLOSE-PE1-FILE.
           IF ( PE1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE PE1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO PE1-CURSOR-STAT.
 
 
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
              DESTROY IOPEIN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
