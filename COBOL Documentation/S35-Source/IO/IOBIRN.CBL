       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOBIRN.
       AUTHOR.        AMM.
       DATE-WRITTEN.  03-11-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOBIRN
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : BIFILE 
      * TABLE DESC    : BI FILE
      * OUTPUT FILE   : XPC/IOBIRN.PRE
      * USED FOR      : LIBLP/LPWSBI.CPY  --- QBI-REC
      *                 LIBLP/LPBI1R.CPY  --- READ
      *                 LIBLP/LPBI1RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * AMM 2021-0311 ***NEW***
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
           "@(#)IO/IOBIRN.CBL S34 05/25/21-10:24:04 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBLP/LP01BI.CPY".
       COPY "LIBLP/LPWSBI.CPY".

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
       01  QBI1-WHERE-BEG-KEY.
           03  QBI1-WBEG-SSNO       PIC 9(09).
           03  QBI1-WBEG-ACCTNO     PIC 9(08).
           03  QBI1-WBEG-OWNBR      PIC 9(04).

       01  QBI1-WHERE-END-KEY.
           03  QBI1-WEND-SSNO       PIC 9(09).
           03  QBI1-WEND-ACCTNO     PIC 9(08).
           03  QBI1-WEND-OWNBR      PIC 9(04).

       01  QBI2-WHERE-BEG-KEY.
           03  QBI2-WBEG-LNAME      PIC X(35).
           03  QBI2-WBEG-FNAME      PIC X(35).

       01  QBI2-WHERE-END-KEY.
           03  QBI2-WEND-LNAME      PIC X(35).
           03  QBI2-WEND-FNAME      PIC X(35).


       01  QBI-REC.
           03  QBI-SSNO             PIC 9(9).
           03  QBI-ACCTNO           PIC 9(8).
           03  QBI-OWNBR            PIC 9(4).
           03  QBI-LNAME            PIC X(35).
           03  QBI-FNAME            PIC X(35).
           03  QBI-LTOUCH-DATE      PIC X(10).
           03  QBI-MAKERCD          PIC X.
           03  QBI-ORGST            PIC XX.
           03  QBI-GIFTNET          PIC X.
           03  QBI-POFFDATE         PIC X(10).
           03  QBI-ENTDATE          PIC X(10).
           03  QBI-SALEFIN          PIC X.
           03  QBI-CONVERCD         PIC X.
           03  QBI-PLCD             PIC X.
           03  QBI-SMALLOAN         PIC X.
           03  QBI-PLDATE           PIC X(10).

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

       01  LEGEND-BEG-SSNO.
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING SSNO       (F3-ALL) ".

       01  LEGEND-END-SSNO.
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING SSNO       ".

       01  LEGEND-BEG-ACCTNO.
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING ACCTNO     (F3-ALL) ".

       01  LEGEND-END-ACCTNO.
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING ACCTNO     ".

       01  LEGEND-BEG-OWNBR.
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING OWNBR      (F3-ALL) ".

       01  LEGEND-END-OWNBR.
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING OWNBR      ".

       01  LEGEND-BEG-LNAME.
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING LNAME      (F3-ALL) ".

       01  LEGEND-END-LNAME.
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING LNAME      ".

       01  LEGEND-BEG-FNAME.
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING FNAME      (F3-ALL) ".

       01  LEGEND-END-FNAME.
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING FNAME      ".

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
           03  BEG-SSNO                PIC 9(09).
           03  END-SSNO                PIC 9(09).
           03  BEG-ACCTNO              PIC 9(08).
           03  END-ACCTNO              PIC 9(08).
           03  BEG-OWNBR               PIC 9(04).
           03  END-OWNBR               PIC 9(04).
           03  BEG-LNAME               PIC X(35).
           03  END-LNAME               PIC X(35).
           03  BEG-FNAME               PIC X(35).
           03  END-FNAME               PIC X(35).

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
           03  FILLER PIC X(30) VALUE "SSNO    ACCTNO   OWNBR   LNAME".
           03  FILLER PIC X(30) VALUE "              FNAME           ".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(12) VALUE SPACES.

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-BI-SSNO                    PIC 9(09).
           03  FILLER                         PIC X(01).
           03  D01-BI-ACCTNO                  PIC 9(08).
           03  FILLER                         PIC X(01).
           03  D01-BI-OWNBR                   PIC 9(04).
           03  FILLER                         PIC X(01).
           03  D01-BI-LNAME                   PIC X(35).
           03  FILLER                         PIC X(01).
           03  D01-BI-FNAME                   PIC X(35).

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
       COPY "LIBIO/IOBIRN_DEF.CPY".
       COPY "LIBIO/IOBIRN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOBIRN_SCN.CPY".

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
           MOVE "B"                        TO BI-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - -{ SET WHERE BEG & END FIELDS }
           MOVE BEG-SSNO   TO QBI1-WBEG-SSNO.
           MOVE END-SSNO   TO QBI1-WEND-SSNO.

           MOVE BEG-ACCTNO TO QBI1-WBEG-ACCTNO.
           MOVE END-ACCTNO TO QBI1-WEND-ACCTNO.

           MOVE BEG-OWNBR  TO QBI1-WBEG-OWNBR.
           MOVE END-OWNBR  TO QBI1-WEND-OWNBR.

           MOVE BEG-LNAME  TO QBI2-WBEG-LNAME.
           MOVE END-LNAME  TO QBI2-WEND-LNAME.

           MOVE BEG-FNAME  TO QBI2-WBEG-FNAME.
           MOVE END-FNAME  TO QBI2-WEND-FNAME.

      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           IF ( PROCESS-KEY = 1 )
              PERFORM OPEN-BI1-FILE
              PERFORM START-BI1-FILE
           ELSE
           IF ( PROCESS-KEY = 2 )
              PERFORM OPEN-BI2-FILE
              PERFORM START-BI2-FILE.

      *=================================================================
       MAIN-PROGRAM-NEXT.

           ADD 01 TO RECORDS-READ.
           IF ( PROCESS-KEY = 1 )
              PERFORM READ-BI1-FILE-NEXT 
           ELSE
           IF ( PROCESS-KEY = 2 )
              PERFORM READ-BI2-FILE-NEXT. 

           IF ( IO-BAD )
              GO TO MAIN-PROGRAM-END.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( PROCESS-KEY = 1 )
              IF ( BI-SSNO < BEG-SSNO )
                 GO TO MAIN-PROGRAM-NEXT
              ELSE
              IF ( BI-SSNO > END-SSNO )
                 GO TO MAIN-PROGRAM-END
              ELSE
              IF ( BI-ACCTNO < BEG-ACCTNO ) OR
                 ( BI-ACCTNO > END-ACCTNO )
                 GO TO MAIN-PROGRAM-NEXT
              ELSE
              IF ( BI-OWNBR < BEG-OWNBR ) OR
                 ( BI-OWNBR > END-OWNBR ) 
                 GO TO MAIN-PROGRAM-NEXT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( PROCESS-KEY = 2 )
              IF ( BI-LNAME < BEG-LNAME )
                 GO TO MAIN-PROGRAM-NEXT
              ELSE
              IF ( BI-LNAME > END-LNAME )
                 GO TO MAIN-PROGRAM-END
              ELSE
              IF ( BI-FNAME < BEG-FNAME ) OR
                 ( BI-FNAME > END-FNAME )
                 GO TO MAIN-PROGRAM-NEXT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( PROCESS-KEY = 3 )
              IF ( BI-OWNBR < BEG-OWNBR )
                 GO TO MAIN-PROGRAM-NEXT
              ELSE
              IF ( BI-OWNBR > END-OWNBR ) 
                 GO TO MAIN-PROGRAM-END
              ELSE
              IF ( BI-SSNO < BEG-SSNO ) OR
                 ( BI-SSNO > END-SSNO )
                 GO TO MAIN-PROGRAM-NEXT
              ELSE
              IF ( BI-ACCTNO < BEG-ACCTNO ) OR
                 ( BI-ACCTNO > END-ACCTNO )
                 GO TO MAIN-PROGRAM-NEXT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE BI-SSNO                    TO D01-BI-SSNO.
           MOVE BI-ACCTNO                  TO D01-BI-ACCTNO.
           MOVE BI-OWNBR                   TO D01-BI-OWNBR.
	   MOVE BI-LNAME                   TO D01-BI-LNAME.
           MOVE BI-FNAME                   TO D01-BI-FNAME.
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
           MOVE SPACES TO SENDMESS-MSG3.
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
           MOVE "IOBIRN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOBIRN-QUEUE-POS  TO Q-POS.
           MOVE IOBIRN-COPIES-POS TO C-POS.
           MOVE IOBIRN-PRU-POS    TO P-POS.
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
       ENTER-BEG-SSNO.

           MOVE LEGEND-BEG-SSNO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN090BEG-SSNO." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-SSNO.
      *KC  IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-SSNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-SSNO.

      *-----------------------------------------------------------------
       ENTER-END-SSNO.

           MOVE LEGEND-END-SSNO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNNN90END-SSNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-SSNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-SSNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-SSNO.

           IF ( BEG-SSNO > END-SSNO )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-SSNO.

           GO TO ENTER-BEG-ACCTNO.

      *-----------------------------------------------------------------
       ENTER-ALL-SSNO.

           MOVE "SNNN090BEG-SSNO." TO VDU.
           MOVE 0                    TO BEG-SSNO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN090END-SSNO." TO VDU.
           MOVE 999999999            TO END-SSNO VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-ACCTNO.

           MOVE LEGEND-BEG-ACCTNO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN080BEG-ACCTNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-SSNO.
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

           GO TO ENTER-BEG-OWNBR.

      *-----------------------------------------------------------------
       ENTER-ALL-ACCTNO.

           MOVE "SNNN080BEG-ACCTNO." TO VDU.
           MOVE 0                    TO BEG-ACCTNO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN080END-ACCTNO." TO VDU.
           MOVE 99999999             TO END-ACCTNO VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-OWNBR.

           MOVE LEGEND-BEG-OWNBR TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040BEG-OWNBR." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-ACCTNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-OWNBR.
      *KC  IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-OWNBR.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-OWNBR.

      *-----------------------------------------------------------------
       ENTER-END-OWNBR.

           MOVE LEGEND-END-OWNBR TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040END-OWNBR." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-OWNBR.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-OWNBR.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-OWNBR.

           IF ( BEG-OWNBR > END-OWNBR )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-OWNBR.

           GO TO ENTER-BEG-LNAME.

      *-----------------------------------------------------------------
       ENTER-ALL-OWNBR.

           MOVE "SNNN040BEG-OWNBR." TO VDU.
           MOVE ZEROES               TO BEG-OWNBR VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040END-OWNBR." TO VDU.
           MOVE 9999                 TO END-OWNBR VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-LNAME.

           MOVE LEGEND-BEG-LNAME TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A200BEG-LNAME." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-OWNBR.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-LNAME.
      *KC  IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-LNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-LNAME.

      *-----------------------------------------------------------------
       ENTER-END-LNAME.

           MOVE LEGEND-END-LNAME TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A200END-LNAME." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-LNAME.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-LNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-LNAME.

           IF ( BEG-LNAME > END-LNAME )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-LNAME.

           GO TO ENTER-BEG-FNAME.

      *-----------------------------------------------------------------
       ENTER-ALL-LNAME.

           MOVE "S  A200BEG-LNAME." TO VDU.
           MOVE SPACES               TO BEG-LNAME VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A200END-LNAME." TO VDU.
           MOVE ALL "Z"              TO END-LNAME VDUBUF.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-FNAME.

           MOVE LEGEND-BEG-FNAME TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A200BEG-FNAME." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-LNAME.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-FNAME.
      *KC  IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-FNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-FNAME.

      *-----------------------------------------------------------------
       ENTER-END-FNAME.

           MOVE LEGEND-END-FNAME TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A200END-FNAME." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-FNAME.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-FNAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-FNAME.

           IF ( BEG-FNAME > END-FNAME )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-FNAME.

           GO TO ENTER-PROCESS-KEY.

      *-----------------------------------------------------------------
       ENTER-ALL-FNAME.

           MOVE "S  A200BEG-FNAME." TO VDU.
           MOVE SPACES               TO BEG-FNAME VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A200END-FNAME." TO VDU.
           MOVE ALL "Z"              TO END-FNAME VDUBUF.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-PROCESS-KEY.

           MOVE LEGEND-PROCESS-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN010PROCESS-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-FNAME.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( VDUNUM0 < ZEROES ) OR
              ( VDUNUM0 > 3      )      
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

           MOVE "S  A000BEG-SSNO."     TO VDU.
           MOVE BEG-SSNO               TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A000END-SSNO."     TO VDU.
           MOVE END-SSNO               TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A000BEG-ACCTNO."   TO VDU.
           MOVE BEG-ACCTNO             TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A000END-ACCTNO."   TO VDU.
           MOVE END-ACCTNO             TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A000BEG-OWNBR."    TO VDU.
           MOVE BEG-OWNBR              TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A000END-OWNBR."    TO VDU.
           MOVE END-OWNBR              TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A000BEG-LNAME."    TO VDU.
           MOVE BEG-LNAME              TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A000END-LNAME."    TO VDU.
           MOVE END-LNAME              TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A000BEG-FNAME."    TO VDU.
           MOVE BEG-FNAME              TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A000END-FNAME."    TO VDU.
           MOVE END-FNAME              TO VDUBUF.
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
              MOVE BI1-KEY TO PACE-DETAIL-KEY
           ELSE
           IF ( PROCESS-KEY = 2 )
              MOVE BI2-KEY TO PACE-DETAIL-KEY
           ELSE
           IF ( PROCESS-KEY = 3 )
              STRING BI-OWNBR, BI-SSNO, BI-ACCTNO
                   DELIMITED BY SIZE INTO PACE-DETAIL-KEY.

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
           MOVE "BEGINNING SSNO                : "    TO R01-PROMPT.
           MOVE BEG-SSNO                              TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    SSNO                : "    TO R01-PROMPT.
           MOVE END-SSNO                              TO R01-TEXT.
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
           MOVE "BEGINNING OWNBR               : "    TO R01-PROMPT.
           MOVE BEG-OWNBR                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    OWNBR               : "    TO R01-PROMPT.
           MOVE END-OWNBR                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING LNAME               : "    TO R01-PROMPT.
           MOVE BEG-LNAME                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    LNAME               : "    TO R01-PROMPT.
           MOVE END-LNAME                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING FNAME               : "    TO R01-PROMPT.
           MOVE BEG-FNAME                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    FNAME               : "    TO R01-PROMPT.
           MOVE END-FNAME                             TO R01-TEXT.
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
           MOVE "SQL TABLE IO UTILITY - BIFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOBIRN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOBIRN-SYSDATE.
           DISPLAY IOBIRN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOBIRN-FORM-FIELDS TO WR-BUF.
           MOVE IOBIRN-FORM-LIST TO WR-LIST.
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

           MOVE IOBIRN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOBIRN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOBIRN_EVA.CPY".
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
      *    LOAD BI-REC FROM QBI-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-BI-FIELDS.
           MOVE QBI-SSNO TO BI-SSNO.
           MOVE QBI-ACCTNO TO BI-ACCTNO.
           MOVE QBI-OWNBR TO BI-OWNBR.
           MOVE QBI-LNAME TO BI-LNAME.
           MOVE QBI-FNAME TO BI-FNAME.

           IF ( QBI-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO BI-LTOUCH-DATE
           ELSE
              MOVE QBI-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO BI-LTOUCH-DATE.

           MOVE QBI-MAKERCD TO BI-MAKERCD.
           MOVE QBI-ORGST TO BI-ORGST.
           MOVE QBI-GIFTNET TO BI-GIFTNET.

           IF ( QBI-POFFDATE = "1900-01-01" )
              MOVE ZEROES TO BI-POFFDATE
           ELSE
              MOVE QBI-POFFDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO BI-POFFDATE.


           IF ( QBI-ENTDATE = "1900-01-01" )
              MOVE ZEROES TO BI-ENTDATE
           ELSE
              MOVE QBI-ENTDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO BI-ENTDATE.

           MOVE QBI-SALEFIN TO BI-SALEFIN.
           MOVE QBI-CONVERCD TO BI-CONVERCD.
           MOVE QBI-PLCD TO BI-PLCD.
           MOVE QBI-SMALLOAN TO BI-SMALLOAN.

           IF ( QBI-PLDATE = "1900-01-01" )
              MOVE ZEROES TO BI-PLDATE
           ELSE
              MOVE QBI-PLDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO BI-PLDATE.

 
      *-----------------------------------------------------------------
      *    LOAD QBI-REC FROM BI-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-BI-FIELDS.
           MOVE BI-SSNO TO QBI-SSNO.
           MOVE BI-ACCTNO TO QBI-ACCTNO.
           MOVE BI-OWNBR TO QBI-OWNBR.
           MOVE BI-LNAME TO QBI-LNAME.
           MOVE BI-FNAME TO QBI-FNAME.

           IF ( BI-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QBI-LTOUCH-DATE
           ELSE
              MOVE BI-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QBI-LTOUCH-DATE.

           MOVE BI-MAKERCD TO QBI-MAKERCD.
           MOVE BI-ORGST TO QBI-ORGST.
           MOVE BI-GIFTNET TO QBI-GIFTNET.

           IF ( BI-POFFDATE = ZEROES )
              MOVE "1900-01-01" TO QBI-POFFDATE
           ELSE
              MOVE BI-POFFDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QBI-POFFDATE.


           IF ( BI-ENTDATE = ZEROES )
              MOVE "1900-01-01" TO QBI-ENTDATE
           ELSE
              MOVE BI-ENTDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QBI-ENTDATE.

           MOVE BI-SALEFIN TO QBI-SALEFIN.
           MOVE BI-CONVERCD TO QBI-CONVERCD.
           MOVE BI-PLCD TO QBI-PLCD.
           MOVE BI-SMALLOAN TO QBI-SMALLOAN.

           IF ( BI-PLDATE = ZEROES )
              MOVE "1900-01-01" TO QBI-PLDATE
           ELSE
              MOVE BI-PLDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QBI-PLDATE.

      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      * COPYMEMBER: LIBLP/LPBI1RN
       LOAD-BI1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BI-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BI-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BI-ALL-BASE
              MOVE BI-ALL-PATH             TO BI-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BI-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BI-ALL-BASE
              MOVE BI-ALL-PATH             TO BI-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-BIFILE NOT = "S" )
              MOVE BI-PATH-BEG             TO BI-SELECT-PATH-BEG
              MOVE BI-PATH-FILE            TO BI-SELECT-PATH-FILE
           ELSE
              MOVE BI-PATH-SQL             TO BI-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-BI1-FILE.
           PERFORM LOAD-BI1-FILE.
           PERFORM OPEN-IT.
           MOVE BI-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-BI1-FILE.
           PERFORM START-IT.
           MOVE BI-SELECT-PATH TO E-FILE.
           MOVE BI1-KEY TO E-KEYX.

           IF ( BI1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BI1-FILE.

           IF ( QBI1-WEND-OWNBR = ZEROES    ) OR
              ( QBI1-WEND-OWNBR NOT NUMERIC )
              MOVE BI-SSNO        TO QBI1-WBEG-SSNO
              MOVE ALL "9"        TO QBI1-WEND-SSNO
              MOVE BI-ACCTNO      TO QBI1-WBEG-ACCTNO
              MOVE ALL "9"        TO QBI1-WEND-ACCTNO
              MOVE BI-OWNBR       TO QBI1-WBEG-OWNBR
              MOVE ALL "9"        TO QBI1-WEND-OWNBR.

           EXEC SQL
            DECLARE BI1_CURSOR CURSOR FOR
            SELECT BIFILE.BI_SSNO,
                   BIFILE.BI_ACCTNO,
                   BIFILE.BI_OWNBR,
                   ISNULL(BIFILE.BI_LNAME,''),
                   ISNULL(BIFILE.BI_FNAME,''),
                   ISNULL(CAST(BIFILE.BI_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(BIFILE.BI_MAKERCD,''),
                   ISNULL(BIFILE.BI_ORGST,''),
                   ISNULL(BIFILE.BI_GIFTNET,''),
                   ISNULL(CAST(BIFILE.BI_POFFDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BIFILE.BI_ENTDATE AS VARCHAR(10)),'0'),
                   ISNULL(BIFILE.BI_SALEFIN,''),
                   ISNULL(BIFILE.BI_CONVERCD,''),
                   ISNULL(BIFILE.BI_PLCD,''),
                   ISNULL(BIFILE.BI_SMALLOAN,''),
                   ISNULL(CAST(BIFILE.BI_PLDATE AS VARCHAR(10)),'0')
            FROM DBO.BIFILE
            WHERE BIFILE.BI_SSNO >= :QBI1-WBEG-SSNO
              AND BIFILE.BI_SSNO <= :QBI1-WEND-SSNO
              AND BIFILE.BI_ACCTNO >= :QBI1-WBEG-ACCTNO
              AND BIFILE.BI_ACCTNO <= :QBI1-WEND-ACCTNO
              AND BIFILE.BI_OWNBR >= :QBI1-WBEG-OWNBR
              AND BIFILE.BI_OWNBR <= :QBI1-WEND-OWNBR
            ORDER BY BIFILE.BI_SSNO, BIFILE.BI_ACCTNO,
                       BIFILE.BI_OWNBR
           END-EXEC.

           EXEC SQL
               OPEN BI1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QBI1-WBEG-SSNO
                          QBI1-WEND-SSNO
                          QBI1-WBEG-ACCTNO
                          QBI1-WEND-ACCTNO
                          QBI1-WBEG-OWNBR
                          QBI1-WEND-OWNBR.

           MOVE STAT---GOOD TO BI1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-BI1-FILE.
           PERFORM READ-IT.
           MOVE BI-SELECT-PATH TO E-FILE.
           MOVE BI1-KEY TO E-KEYX.

           MOVE BI-SSNO     TO QBI-SSNO.
           MOVE BI-ACCTNO   TO QBI-ACCTNO.
           MOVE BI-OWNBR    TO QBI-OWNBR.

           EXEC SQL
            SELECT BIFILE.BI_SSNO,
                   BIFILE.BI_ACCTNO,
                   BIFILE.BI_OWNBR,
                   ISNULL(BIFILE.BI_LNAME,''),
                   ISNULL(BIFILE.BI_FNAME,''),
                   ISNULL(CAST(BIFILE.BI_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(BIFILE.BI_MAKERCD,''),
                   ISNULL(BIFILE.BI_ORGST,''),
                   ISNULL(BIFILE.BI_GIFTNET,''),
                   ISNULL(CAST(BIFILE.BI_POFFDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BIFILE.BI_ENTDATE AS VARCHAR(10)),'0'),
                   ISNULL(BIFILE.BI_SALEFIN,''),
                   ISNULL(BIFILE.BI_CONVERCD,''),
                   ISNULL(BIFILE.BI_PLCD,''),
                   ISNULL(BIFILE.BI_SMALLOAN,''),
                   ISNULL(CAST(BIFILE.BI_PLDATE AS VARCHAR(10)),'0')
            INTO :QBI-REC
            FROM DBO.BIFILE
            WHERE BIFILE.BI_SSNO = :QBI-SSNO
              AND BIFILE.BI_ACCTNO = :QBI-ACCTNO
              AND BIFILE.BI_OWNBR = :QBI-OWNBR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BI1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-BI-FIELDS.

      *-----------------------------------------------------------------
       READ-BI1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BI-SELECT-PATH TO E-FILE.
           MOVE BI1-KEY TO E-KEYX.
           INITIALIZE QBI-REC.

           IF ( BI1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BI1_CURSOR INTO :QBI-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BI1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BI-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-BI1-FILE.
           IF ( BI1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE BI1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO BI1-CURSOR-STAT.

      *=================================================================
       LOAD-BI2-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BI-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BI-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BI-ALL-BASE
              MOVE BI-ALL-PATH             TO BI-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BI-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BI-ALL-BASE
              MOVE BI-ALL-PATH             TO BI-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-BIFILE NOT = "S" )
              MOVE BI-PATH-BEG             TO BI-SELECT-PATH-BEG
              MOVE BI-PATH-FILE            TO BI-SELECT-PATH-FILE
           ELSE
              MOVE BI-PATH-SQL             TO BI-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-BI2-FILE.
           PERFORM LOAD-BI2-FILE.
           PERFORM OPEN-IT.
           MOVE BI-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-BI2-FILE.
           PERFORM START-IT.
           MOVE BI-SELECT-PATH TO E-FILE.
           MOVE BI2-KEY TO E-KEYX.

           IF ( BI2-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BI2-FILE.

           IF ( QBI2-WEND-FNAME = SPACES )
              MOVE BI-LNAME       TO QBI2-WBEG-LNAME
              MOVE ALL "Z"        TO QBI2-WEND-LNAME
              MOVE BI-FNAME       TO QBI2-WBEG-FNAME
              MOVE ALL "Z"        TO QBI2-WEND-FNAME.

           EXEC SQL
            DECLARE BI2_CURSOR CURSOR FOR
            SELECT BIFILE.BI_SSNO,
                   BIFILE.BI_ACCTNO,
                   BIFILE.BI_OWNBR,
                   ISNULL(BIFILE.BI_LNAME,''),
                   ISNULL(BIFILE.BI_FNAME,''),
                   ISNULL(CAST(BIFILE.BI_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(BIFILE.BI_MAKERCD,''),
                   ISNULL(BIFILE.BI_ORGST,''),
                   ISNULL(BIFILE.BI_GIFTNET,''),
                   ISNULL(CAST(BIFILE.BI_POFFDATE AS VARCHAR(10)),'0'),
                   ISNULL(CAST(BIFILE.BI_ENTDATE AS VARCHAR(10)),'0'),
                   ISNULL(BIFILE.BI_SALEFIN,''),
                   ISNULL(BIFILE.BI_CONVERCD,''),
                   ISNULL(BIFILE.BI_PLCD,''),
                   ISNULL(BIFILE.BI_SMALLOAN,''),
                   ISNULL(CAST(BIFILE.BI_PLDATE AS VARCHAR(10)),'0')
            FROM DBO.BIFILE
            WHERE BIFILE.BI_LNAME >= :QBI2-WBEG-LNAME
              AND BIFILE.BI_LNAME <= :QBI2-WEND-LNAME
              AND BIFILE.BI_FNAME >= :QBI2-WBEG-FNAME
              AND BIFILE.BI_FNAME <= :QBI2-WEND-FNAME
            ORDER BY BIFILE.BI_LNAME, BIFILE.BI_FNAME
           END-EXEC.

           EXEC SQL
               OPEN BI2_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QBI2-WBEG-LNAME
                          QBI2-WEND-LNAME
                          QBI2-WBEG-FNAME
                          QBI2-WEND-FNAME.

           MOVE STAT---GOOD TO BI2-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-BI2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BI-SELECT-PATH TO E-FILE.
           MOVE BI2-KEY TO E-KEYX.
           INITIALIZE QBI-REC.

           IF ( BI2-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BI2_CURSOR INTO :QBI-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BI2-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BI-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-BI2-FILE.
           IF ( BI2-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE BI2_CURSOR
              END-EXEC
              MOVE STAT---BAD TO BI2-CURSOR-STAT.

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
              DESTROY IOBIRN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
