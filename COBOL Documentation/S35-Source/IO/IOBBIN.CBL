       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOBBIN.
       AUTHOR.        AMM.
       DATE-WRITTEN.  05-11-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOBBIN
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : BBFILE 
      * TABLE DESC    : BULLETIN BOARD FILE
      * OUTPUT FILE   : XPC/IOBBIN.PRE
      * USED FOR      : LIBSP/SPWSBB.CPY  --- QBB-REC
      *                 LIBSP/SP/BBR.CPY  --- READ
      *                 LIBSP/SP/BBRN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * AMM 2021-0511 **     **
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
           "@(#)IO/IOBBIN.CBL S34 05/17/21-15:25:31 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBSP/SP01BB.CPY".
       COPY "LIBSP/SPWSBB.CPY".

      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S WS COPY MEMBERS
      *#################################################################
      *=================================================================
      *
      *    Q - F I L E
      *
      *-----------------------------------------------------------------
      *    FOR UPDATING THE FILE'S WS COPY MEMBER: LIBSP/SPWSBB.CPY
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QBB1-WHERE-BEG-KEY.
           03  QBB1-WBEG-CATAGORY       PIC X(04).
           03  QBB1-WBEG-SUBCAT         PIC X(04).
           03  QBB1-WBEG-USERID         PIC X(10).
           03  QBB1-WBEG-SEQNO          PIC 9(06).

       01  QBB1-WHERE-END-KEY.
           03  QBB1-WEND-CATAGORY       PIC X(04).
           03  QBB1-WEND-SUBCAT         PIC X(04).
           03  QBB1-WEND-USERID         PIC X(10).
           03  QBB1-WEND-SEQNO          PIC 9(06).

       01  QBB-REC.
           03  QBB-CATAGORY        PIC X(04).
           03  QBB-SUBCATAGORY     PIC X(04).
           03  QBB-USERID          PIC X(10).
           03  QBB-SEQNO           PIC 9(06).
           03  QBB-NAME            PIC X(20).
           03  QBB-LOGNAME         PIC X(10).
           03  QBB-POSTNAME        PIC X(10).
           03  QBB-OTHERBR         PIC X(10).
           03  QBB-POSTDATE        PIC X(10).
           03  QBB-POSTTIME        PIC 9(06).
           03  QBB-PASSWORD        PIC X(08).
           03  QBB-GROUP           PIC X(04).
           03  QBB-BRNO            PIC 9(4).
           03  QBB-ACTION          PIC X(40).
           03  QBB-MEMO-LINE-1     PIC X(60).
           03  QBB-MEMO-LINE-2     PIC X(60).
           03  QBB-MEMO-LINE-3     PIC X(60).
           03  QBB-MEMO-LINE-4     PIC X(60).
           03  QBB-MEMO-LINE-5     PIC X(60).
           03  QBB-MEMO-LINE-6     PIC X(60).
           03  QBB-MEMO-LINE-7     PIC X(60).
           03  QBB-MEMO-LINE-8     PIC X(60).
           03  QBB-MEMO-LINE-9     PIC X(60).
           03  QBB-MEMO-LINE-10    PIC X(60).
           03  QBB-LTOUCH-DATE     PIC X(10).

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

       01  LEGEND-BEG-CATAGORY. 
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING CATAGORY   (F3-ALL) ".

       01  LEGEND-END-CATAGORY. 
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING CATAGORY   ".

       01  LEGEND-BEG-SUBCAT. 
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING SUBCAT     (F3-ALL) ".

       01  LEGEND-END-SUBCAT. 
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING SUBCAT     ".

       01  LEGEND-BEG-USERID. 
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING USERID     (F3-ALL) ".

       01  LEGEND-END-USERID. 
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING USERID     ".

       01  LEGEND-BEG-SEQNO. 
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING SEQNO      (F3-ALL) ".

       01  LEGEND-END-SEQNO. 
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING SEQNO      ".

       01  LEGEND-BEG-NAME.  
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING NAME       (F3-ALL) ".

       01  LEGEND-END-NAME.  
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING NAME       ".

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
           03  BEG-CATAGORY            PIC X(04).
           03  END-CATAGORY            PIC X(04).
           03  BEG-SUBCAT              PIC X(04).
           03  END-SUBCAT              PIC X(04).
           03  BEG-USERID              PIC X(10).
           03  END-USERID              PIC X(10).
           03  BEG-SEQNO               PIC 9(06).
           03  END-SEQNO               PIC 9(06).
           03  BEG-NAME                PIC X(20).
           03  END-NAME                PIC X(20).

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
           03  FILLER PIC X(30) VALUE "CATGRY SUBCAT USERID     SEQNO".
           03  FILLER PIC X(30) VALUE "   NAME                       ".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(12) VALUE SPACES.

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-BB-CATAGORY                PIC X(04).
           03  FILLER                         PIC X(04).
           03  D01-BB-SUBCATAGORY             PIC X(04).
           03  FILLER                         PIC X(03).
           03  D01-BB-USERID                  PIC X(10).
           03  FILLER                         PIC X(01).
           03  D01-BB-SEQNO                   PIC 9(06).
           03  FILLER                         PIC X(01).
           03  D01-BB-NAME                    PIC X(20).

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
       COPY "LIBIO/IOBBIN_DEF.CPY".
       COPY "LIBIO/IOBBIN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOBBIN_SCN.CPY".

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
           MOVE "B"                        TO BB-PATH-OVERRIDE.

      *- - - - - - - - - - - - - - - - - -{ SET WHERE BEG & END FIELDS }
           MOVE BEG-CATAGORY            TO QBB1-WBEG-CATAGORY.
           MOVE END-CATAGORY            TO QBB1-WEND-CATAGORY.

           MOVE BEG-SUBCAT              TO QBB1-WBEG-SUBCAT.
           MOVE END-SUBCAT              TO QBB1-WEND-SUBCAT.

           MOVE BEG-USERID              TO QBB1-WBEG-USERID.
           MOVE END-USERID              TO QBB1-WEND-USERID.

           MOVE BEG-SEQNO               TO QBB1-WBEG-SEQNO.
           MOVE END-SEQNO               TO QBB1-WEND-SEQNO.

      *- - - - - - - - - - - - - - - - - - -{ OPEN & START FILE/TABLE }
           PERFORM OPEN-BB1-FILE.
           PERFORM START-BB1-FILE.

      *=================================================================
       MAIN-PROGRAM-NEXT.

           ADD 01 TO RECORDS-READ.

           PERFORM READ-BB1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO MAIN-PROGRAM-END.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( PROCESS-KEY = 1 )
              IF ( BB-CATAGORY < BEG-CATAGORY )
                 GO TO MAIN-PROGRAM-NEXT
              ELSE
              IF ( BB-CATAGORY > END-CATAGORY )
                 GO TO MAIN-PROGRAM-END
              ELSE
              IF ( BB-SUBCATAGORY < BEG-SUBCAT ) OR
                 ( BB-SUBCATAGORY > END-SUBCAT )
                 GO TO MAIN-PROGRAM-NEXT
              ELSE
              IF ( BB-USERID < BEG-USERID ) OR
                 ( BB-USERID > END-USERID ) 
                 GO TO MAIN-PROGRAM-NEXT
              ELSE
              IF ( BB-SEQNO < BEG-SEQNO ) OR
                 ( BB-SEQNO > END-SEQNO )
                 GO TO MAIN-PROGRAM-NEXT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( PROCESS-KEY = 2 )
              IF ( BB-CATAGORY < BEG-CATAGORY )
                 GO TO MAIN-PROGRAM-NEXT
              ELSE
              IF ( BB-CATAGORY > END-CATAGORY )
                 GO TO MAIN-PROGRAM-END
              ELSE
              IF ( BB-SUBCATAGORY < BEG-SUBCAT ) OR
                 ( BB-SUBCATAGORY > END-SUBCAT )
                 GO TO MAIN-PROGRAM-NEXT
              ELSE
              IF ( BB-NAME < BEG-NAME ) OR
                 ( BB-NAME > END-NAME )
                 GO TO MAIN-PROGRAM-NEXT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE BB-CATAGORY                TO D01-BB-CATAGORY.
           MOVE BB-SUBCATAGORY             TO D01-BB-SUBCATAGORY.
           MOVE BB-USERID                  TO D01-BB-USERID.
           MOVE BB-SEQNO                   TO D01-BB-SEQNO.
           MOVE BB-NAME                    TO D01-BB-NAME.
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
           MOVE "IOBBIN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOBBIN-QUEUE-POS  TO Q-POS.
           MOVE IOBBIN-COPIES-POS TO C-POS.
           MOVE IOBBIN-PRU-POS    TO P-POS.
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
       ENTER-BEG-CATAGORY.

           MOVE LEGEND-BEG-CATAGORY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A040BEG-CATAGORY." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-CATAGORY.
      *KC  IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-CATAGORY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-CATAGORY.

      *-----------------------------------------------------------------
       ENTER-END-CATAGORY.

           MOVE LEGEND-END-CATAGORY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A040END-CATAGORY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-CATAGORY.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-CATAGORY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-CATAGORY.

           IF ( BEG-CATAGORY > END-CATAGORY )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-CATAGORY.

           GO TO ENTER-BEG-SUBCAT.

      *-----------------------------------------------------------------
       ENTER-ALL-CATAGORY.

           MOVE "S  A040BEG-CATAGORY." TO VDU.
           MOVE SPACES               TO BEG-CATAGORY VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A040END-CATAGORY." TO VDU.
           MOVE ALL "Z"              TO END-CATAGORY VDUBUF.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-SUBCAT.

           MOVE LEGEND-BEG-SUBCAT   TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A040BEG-SUBCAT."   TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-CATAGORY.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-SUBCAT.
      *KC  IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-SUBCAT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-SUBCAT.

      *-----------------------------------------------------------------
       ENTER-END-SUBCAT.

           MOVE LEGEND-END-SUBCAT TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A040END-SUBCAT." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-SUBCAT.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-SUBCAT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-SUBCAT.

           IF ( BEG-SUBCAT > END-SUBCAT )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-SUBCAT.

           GO TO ENTER-BEG-USERID.

      *-----------------------------------------------------------------
       ENTER-ALL-SUBCAT.

           MOVE "S  A040BEG-SUBCAT." TO VDU.
           MOVE SPACES               TO BEG-SUBCAT VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A040END-SUBCAT." TO VDU.
           MOVE ALL "Z"              TO END-SUBCAT VDUBUF.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-USERID.

           MOVE LEGEND-BEG-USERID   TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A040BEG-USERID."   TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-SUBCAT.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-USERID.
      *KC  IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-USERID.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-USERID.

      *-----------------------------------------------------------------
       ENTER-END-USERID.

           MOVE LEGEND-END-USERID TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A040END-USERID." TO VDU.
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

           GO TO ENTER-BEG-SEQNO.

      *-----------------------------------------------------------------
       ENTER-ALL-USERID.

           MOVE "S  A040BEG-USERID." TO VDU.
           MOVE SPACES               TO BEG-USERID VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A040END-USERID." TO VDU.
           MOVE ALL "Z"              TO END-USERID VDUBUF.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-SEQNO.

           MOVE LEGEND-BEG-SEQNO   TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN060BEG-SEQNO."   TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-USERID.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-SEQNO.
      *KC  IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-SEQNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-SEQNO.

      *-----------------------------------------------------------------
       ENTER-END-SEQNO.

           MOVE LEGEND-END-SEQNO TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN060END-SEQNO." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-SEQNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-SEQNO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-SEQNO.

           IF ( BEG-SEQNO > END-SEQNO )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-SEQNO.

           GO TO ENTER-BEG-NAME.

      *-----------------------------------------------------------------
       ENTER-ALL-SEQNO.

           MOVE "SNNN060BEG-SEQNO." TO VDU.
           MOVE 0                   TO BEG-SEQNO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060END-SEQNO." TO VDU.
           MOVE 999999               TO END-SEQNO VDUNUM0.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-BEG-NAME.

           MOVE LEGEND-BEG-NAME   TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A200BEG-NAME."   TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-SEQNO.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-NAME.
      *KC  IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-NAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-NAME.

      *-----------------------------------------------------------------
       ENTER-END-NAME.

           MOVE LEGEND-END-NAME TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A200END-NAME." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-NAME.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-NAME.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-NAME.

           IF ( BEG-NAME > END-NAME )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-NAME.

           GO TO ENTER-PROCESS-KEY.

      *-----------------------------------------------------------------
       ENTER-ALL-NAME.

           MOVE "S  A020BEG-NAME." TO VDU.
           MOVE SPACES               TO BEG-NAME VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A020END-NAME." TO VDU.
           MOVE ALL "Z"              TO END-NAME VDUBUF.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-PROCESS-KEY.

           MOVE LEGEND-PROCESS-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN010PROCESS-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-NAME.
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

           MOVE "S  A040BEG-CATAGORY."    TO VDU.
           MOVE BEG-CATAGORY              TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A040END-CATAGORY."    TO VDU.
           MOVE END-CATAGORY              TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A040BEG-SUBCAT."      TO VDU.
           MOVE BEG-SUBCAT                TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A040END-SUBCAT."      TO VDU.
           MOVE END-SUBCAT                TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A100BEG-USERID."      TO VDU.
           MOVE BEG-USERID                TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A100END-USERID."      TO VDU.
           MOVE END-USERID                TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SNNN060BEG-SEQNO."       TO VDU.
           MOVE BEG-SEQNO                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN060END-SEQNO."       TO VDU.
           MOVE END-SEQNO                 TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A020BEG-NAME."        TO VDU.
           MOVE BEG-NAME                  TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A020END-NAME."        TO VDU.
           MOVE END-NAME                  TO VDUBUF.
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
              MOVE BB1-KEY TO PACE-DETAIL-KEY
           ELSE
           IF ( PROCESS-KEY = 2 )
              STRING BB-CATAGORY, BB-SUBCATAGORY, BB-NAME
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
           MOVE "BEGINNING CATEGORY            : "    TO R01-PROMPT.
           MOVE BEG-CATAGORY                          TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    CATEGORY            : "    TO R01-PROMPT.
           MOVE END-CATAGORY                          TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING SUBCATEGORY         : "    TO R01-PROMPT.
           MOVE BEG-SUBCAT                            TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    SUBCATEGORY         : "    TO R01-PROMPT.
           MOVE END-SUBCAT                            TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING USERID              : "    TO R01-PROMPT.
           MOVE BEG-USERID                            TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    USERID              : "    TO R01-PROMPT.
           MOVE END-USERID                            TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING SEQNO               : "    TO R01-PROMPT.
           MOVE BEG-SEQNO                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    SEQNO               : "    TO R01-PROMPT.
           MOVE END-SEQNO                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING NAME                : "    TO R01-PROMPT.
           MOVE BEG-NAME                              TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    NAME                : "    TO R01-PROMPT.
           MOVE END-NAME                              TO R01-TEXT.
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
           MOVE "SQL TABLE IO UTILITY - BBFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOBBIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOBBIN-SYSDATE.
           DISPLAY IOBBIN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOBBIN-FORM-FIELDS TO WR-BUF.
           MOVE IOBBIN-FORM-LIST TO WR-LIST.
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

           MOVE IOBBIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOBBIN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOBBIN_EVA.CPY".
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

      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      * COPYMEMBER: LIBSP/SPBB1RN
       LOAD-BB1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BB-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BB-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BB-PATH-BASE
              MOVE EXT-FILPATH-MACHINE     TO BB-PATH-MACHINE
              MOVE BB-PATH-FORMAT          TO BB-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BB-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BB-PATH-BASE
              MOVE FILPATH-MACHINE         TO BB-PATH-MACHINE
              MOVE BB-PATH-FORMAT          TO BB-PATH.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-BBFILE NOT = "S" )
              MOVE BB-PATH-BEG             TO BB-SELECT-PATH-BEG
              MOVE BB-PATH-FILE            TO BB-SELECT-PATH-FILE
           ELSE
              MOVE BB-PATH-SQL             TO BB-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-BB1-FILE.
           PERFORM LOAD-BB1-FILE.
           PERFORM OPEN-IT.
           MOVE BB-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-BB1-FILE.
           PERFORM START-IT.
           MOVE BB-SELECT-PATH TO E-FILE.
           MOVE BB1-KEY TO E-KEYX.

           IF ( BB1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BB1-FILE.

           IF ( QBB1-WEND-CATAGORY = SPACES )
              MOVE BB-CATAGORY    TO QBB1-WBEG-CATAGORY
              MOVE ALL "Z"        TO QBB1-WEND-CATAGORY
              MOVE BB-SUBCATAGORY TO QBB1-WBEG-SUBCAT
              MOVE ALL "Z"        TO QBB1-WEND-SUBCAT
              MOVE BB-USERID      TO QBB1-WBEG-USERID
              MOVE ALL "Z"        TO QBB1-WEND-USERID
              MOVE BB-SEQNO       TO QBB1-WBEG-SEQNO
              MOVE ALL "9"        TO QBB1-WEND-SEQNO.

           EXEC SQL
            DECLARE BB1_CURSOR CURSOR FOR
            SELECT ISNULL(BBFILE.BB_CATAGORY,''),
                   ISNULL(BBFILE.BB_SUBCATAGORY,''),
                   ISNULL(BBFILE.BB_USERID,''),
                   BBFILE.BB_SEQNO,
                   ISNULL(BBFILE.BB_NAME,''),
                   ISNULL(BBFILE.BB_LOGNAME,''),
                   ISNULL(BBFILE.BB_POSTNAME,''),
                   ISNULL(BBFILE.BB_OTHERBR,''),
                   ISNULL(CAST(BBFILE.BB_POSTDATE AS VARCHAR(10)),'0'),
                   BBFILE.BB_POSTTIME,
                   ISNULL(BBFILE.BB_PASSWORD,''),
                   ISNULL(BBFILE.BB_GROUP,''),
                   BBFILE.BB_BRNO,
                   ISNULL(BBFILE.BB_ACTION,''),
                   ISNULL(BBFILE.BB_MEMO_LINE_1,''),
                   ISNULL(BBFILE.BB_MEMO_LINE_2,''),
                   ISNULL(BBFILE.BB_MEMO_LINE_3,''),
                   ISNULL(BBFILE.BB_MEMO_LINE_4,''),
                   ISNULL(BBFILE.BB_MEMO_LINE_5,''),
                   ISNULL(BBFILE.BB_MEMO_LINE_6,''),
                   ISNULL(BBFILE.BB_MEMO_LINE_7,''),
                   ISNULL(BBFILE.BB_MEMO_LINE_8,''),
                   ISNULL(BBFILE.BB_MEMO_LINE_9,''),
                   ISNULL(BBFILE.BB_MEMO_LINE_10,''),
                   ISNULL(CAST(BBFILE.BB_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0')
            FROM DBO.BBFILE
            WHERE BBFILE.BB_CATAGORY >= :QBB1-WBEG-CATAGORY
              AND BBFILE.BB_CATAGORY <= :QBB1-WEND-CATAGORY
              AND BBFILE.BB_SUBCATAGORY >= :QBB1-WBEG-SUBCAT
              AND BBFILE.BB_SUBCATAGORY <= :QBB1-WEND-SUBCAT
              AND BBFILE.BB_USERID >= :QBB1-WBEG-USERID
              AND BBFILE.BB_USERID <= :QBB1-WEND-USERID
              AND BBFILE.BB_SEQNO >= :QBB1-WBEG-SEQNO
              AND BBFILE.BB_SEQNO <= :QBB1-WEND-SEQNO
            ORDER BY BBFILE.BB_CATAGORY,
                     BBFILE.BB_SUBCATAGORY,
                     BBFILE.BB_USERID,
                     BFILE.BB_SEQNO
           END-EXEC.

           EXEC SQL
               OPEN BB1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QBB1-WBEG-CATAGORY
                          QBB1-WEND-CATAGORY
                          QBB1-WBEG-SUBCAT
                          QBB1-WEND-SUBCAT
                          QBB1-WBEG-USERID
                          QBB1-WEND-USERID.

           MOVE ZEROES TO QBB1-WBEG-SEQNO
                          QBB1-WEND-SEQNO.

           MOVE STAT---GOOD TO BB1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-BB1-FILE.
           PERFORM READ-IT.
           MOVE BB-SELECT-PATH TO E-FILE.
           MOVE BB1-KEY TO E-KEYX.

           MOVE BB-CATAGORY    TO QBB-CATAGORY.
           MOVE BB-SUBCATAGORY TO QBB-SUBCATAGORY.
           MOVE BB-USERID      TO QBB-USERID.
           MOVE BB-SEQNO       TO QBB-SEQNO.

           EXEC SQL
            SELECT ISNULL(BBFILE.BB_CATAGORY,''),
                   ISNULL(BBFILE.BB_SUBCATAGORY,''),
                   ISNULL(BBFILE.BB_USERID,''),
                   BBFILE.BB_SEQNO,
                   ISNULL(BBFILE.BB_NAME,''),
                   ISNULL(BBFILE.BB_LOGNAME,''),
                   ISNULL(BBFILE.BB_POSTNAME,''),
                   ISNULL(BBFILE.BB_OTHERBR,''),
                   ISNULL(CAST(BBFILE.BB_POSTDATE AS VARCHAR(10)),'0'),
                   BBFILE.BB_POSTTIME,
                   ISNULL(BBFILE.BB_PASSWORD,''),
                   ISNULL(BBFILE.BB_GROUP,''),
                   BBFILE.BB_BRNO,
                   ISNULL(BBFILE.BB_ACTION,''),
                   ISNULL(BBFILE.BB_MEMO_LINE_1,''),
                   ISNULL(BBFILE.BB_MEMO_LINE_2,''),
                   ISNULL(BBFILE.BB_MEMO_LINE_3,''),
                   ISNULL(BBFILE.BB_MEMO_LINE_4,''),
                   ISNULL(BBFILE.BB_MEMO_LINE_5,''),
                   ISNULL(BBFILE.BB_MEMO_LINE_6,''),
                   ISNULL(BBFILE.BB_MEMO_LINE_7,''),
                   ISNULL(BBFILE.BB_MEMO_LINE_8,''),
                   ISNULL(BBFILE.BB_MEMO_LINE_9,''),
                   ISNULL(BBFILE.BB_MEMO_LINE_10,''),
                   ISNULL(CAST(BBFILE.BB_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0')
            INTO :QBB-REC
            FROM DBO.BBFILE.BBFILE
            WHERE BBFILE.BB_CATAGORY = :QBB-CATAGORY
              AND BBFILE.BB_SUBCATAGORY = :QBB-SUBCATAGORY
              AND BBFILE.BB_USERID = :QBB-USERID
              AND BBFILE.BB_SEQNO = :QBB-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BB1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-BB-FIELDS.

      *-----------------------------------------------------------------
       READ-BB1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE BB-SELECT-PATH TO E-FILE.
           MOVE BB1-KEY TO E-KEYX.
           INITIALIZE QBB-REC.

           IF ( BB1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BB1_CURSOR INTO :QBB-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BB1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BB-FIELDS.

      *-----------------------------------------------------------------
       WRITE-BB1-FILE.

           ACCEPT BB-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE BB-SELECT-PATH TO E-FILE.
           MOVE BB1-KEY TO E-KEYX.
           PERFORM SET-BB-FIELDS.

           EXEC SQL
            INSERT INTO DBO.BBFILE
             (BBFILE.BB_CATAGORY,
              BBFILE.BB_SUBCATAGORY,
              BBFILE.BB_USERID,
              BBFILE.BB_SEQNO,
              BBFILE.BB_NAME,
              BBFILE.BB_LOGNAME,
              BBFILE.BB_POSTNAME,
              BBFILE.BB_OTHERBR,
              BBFILE.BB_POSTDATE,
              BBFILE.BB_POSTTIME,
              BBFILE.BB_PASSWORD,
              BBFILE.BB_GROUP,
              BBFILE.BB_BRNO,
              BBFILE.BB_ACTION,
              BBFILE.BB_MEMO_LINE_1,
              BBFILE.BB_MEMO_LINE_2,
              BBFILE.BB_MEMO_LINE_3,
              BBFILE.BB_MEMO_LINE_4,
              BBFILE.BB_MEMO_LINE_5,
              BBFILE.BB_MEMO_LINE_6,
              BBFILE.BB_MEMO_LINE_7,
              BBFILE.BB_MEMO_LINE_8,
              BBFILE.BB_MEMO_LINE_9,
              BBFILE.BB_MEMO_LINE_10,
              BBFILE.BB_LTOUCH_DATE)
            VALUES
             (:QBB-CATAGORY,
              :QBB-SUBCATAGORY,
              :QBB-USERID,
              :QBB-SEQNO,
              :QBB-NAME,
              :QBB-LOGNAME,
              :QBB-POSTNAME,
              :QBB-OTHERBR,
              NULLIF(:QBB-POSTDATE,'1900-01-01'),
              :QBB-POSTTIME,
              :QBB-PASSWORD,
              :QBB-GROUP,
              :QBB-BRNO,
              :QBB-ACTION,
              :QBB-MEMO-LINE-1,
              :QBB-MEMO-LINE-2,
              :QBB-MEMO-LINE-3,
              :QBB-MEMO-LINE-4,
              :QBB-MEMO-LINE-5,
              :QBB-MEMO-LINE-6,
              :QBB-MEMO-LINE-7,
              :QBB-MEMO-LINE-8,
              :QBB-MEMO-LINE-9,
              :QBB-MEMO-LINE-10,
              NULLIF(:QBB-LTOUCH-DATE,'1900-01-01'))
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-BB1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-BB1-FILE.

           ACCEPT BB-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE BB-SELECT-PATH TO E-FILE.
           MOVE BB1-KEY TO E-KEYX.
           PERFORM SET-BB-FIELDS.

           EXEC SQL
            UPDATE DBO.BBFILE
             SET
              BBFILE.BB_CATAGORY = :QBB-CATAGORY,
              BBFILE.BB_SUBCATAGORY = :QBB-SUBCATAGORY,
              BBFILE.BB_USERID = :QBB-USERID,
              BBFILE.BB_SEQNO = :QBB-SEQNO,
              BBFILE.BB_NAME = :QBB-NAME,
              BBFILE.BB_LOGNAME = :QBB-LOGNAME,
              BBFILE.BB_POSTNAME = :QBB-POSTNAME,
              BBFILE.BB_OTHERBR = :QBB-OTHERBR,
              BBFILE.BB_POSTDATE =
                 NULLIF(:QBB-POSTDATE,'1900-01-01'),
              BBFILE.BB_POSTTIME = :QBB-POSTTIME,
              BBFILE.BB_PASSWORD = :QBB-PASSWORD,
              BBFILE.BB_GROUP = :QBB-GROUP,
              BBFILE.BB_BRNO = :QBB-BRNO,
              BBFILE.BB_ACTION = :QBB-ACTION,
              BBFILE.BB_MEMO_LINE_1 = :QBB-MEMO-LINE-1,
              BBFILE.BB_MEMO_LINE_2 = :QBB-MEMO-LINE-2,
              BBFILE.BB_MEMO_LINE_3 = :QBB-MEMO-LINE-3,
              BBFILE.BB_MEMO_LINE_4 = :QBB-MEMO-LINE-4,
              BBFILE.BB_MEMO_LINE_5 = :QBB-MEMO-LINE-5,
              BBFILE.BB_MEMO_LINE_6 = :QBB-MEMO-LINE-6,
              BBFILE.BB_MEMO_LINE_7 = :QBB-MEMO-LINE-7,
              BBFILE.BB_MEMO_LINE_8 = :QBB-MEMO-LINE-8,
              BBFILE.BB_MEMO_LINE_9 = :QBB-MEMO-LINE-9,
              BBFILE.BB_MEMO_LINE_10 = :QBB-MEMO-LINE-10,
              BBFILE.BB_LTOUCH_DATE =
                 NULLIF(:QBB-LTOUCH-DATE,'1900-01-01')
             WHERE BBFILE.BB_CATAGORY = :QBB-CATAGORY
               AND BBFILE.BB_SUBCATAGORY = :QBB-SUBCATAGORY
               AND BBFILE.BB_USERID = :QBB-USERID
               AND BBFILE.BB_SEQNO = :QBB-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-BB1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-BB1-FILE.

           PERFORM DELETE-IT.
           MOVE BB-SELECT-PATH TO E-FILE.
           MOVE BB1-KEY TO E-KEYX.
           PERFORM SET-BB-FIELDS.

           EXEC SQL
            DELETE FROM DBO.BBFILE
             WHERE BBFILE.BB_CATAGORY = :QBB-CATAGORY
               AND BBFILE.BB_SUBCATAGORY = :QBB-SUBCATAGORY
               AND BBFILE.BB_USERID = :QBB-USERID
               AND BBFILE.BB_SEQNO = :QBB-SEQNO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-BB1-FILE.

      *-----------------------------------------------------------------
       CLOSE-BB1-FILE.
           IF ( BB1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE BB1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO BB1-CURSOR-STAT.

      *#################################################################
      *    ENDING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################

      *-----------------------------------------------------------------
      *    LOAD BB-REC FROM QBB-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-BB-FIELDS.
           MOVE QBB-CATAGORY TO BB-CATAGORY.
           MOVE QBB-SUBCATAGORY TO BB-SUBCATAGORY.
           MOVE QBB-USERID TO BB-USERID.
           MOVE QBB-SEQNO TO BB-SEQNO.
           MOVE QBB-NAME TO BB-NAME.
           MOVE QBB-LOGNAME TO BB-LOGNAME.
           MOVE QBB-POSTNAME TO BB-POSTNAME.
           MOVE QBB-OTHERBR TO BB-OTHERBR.

           IF ( QBB-POSTDATE = "1900-01-01" )
              MOVE ZEROES TO BB-POSTDATE
           ELSE
              MOVE QBB-POSTDATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO BB-POSTDATE.

           MOVE QBB-POSTTIME TO BB-POSTTIME.
           MOVE QBB-PASSWORD TO BB-PASSWORD.
           MOVE QBB-GROUP TO BB-GROUP.
           MOVE QBB-BRNO TO BB-BRNO.
           MOVE QBB-ACTION TO BB-ACTION.
           MOVE QBB-MEMO-LINE-1 TO BB-MEMO-LINE(1).
           MOVE QBB-MEMO-LINE-2 TO BB-MEMO-LINE(2).
           MOVE QBB-MEMO-LINE-3 TO BB-MEMO-LINE(3).
           MOVE QBB-MEMO-LINE-4 TO BB-MEMO-LINE(4).
           MOVE QBB-MEMO-LINE-5 TO BB-MEMO-LINE(5).
           MOVE QBB-MEMO-LINE-6 TO BB-MEMO-LINE(6).
           MOVE QBB-MEMO-LINE-7 TO BB-MEMO-LINE(7).
           MOVE QBB-MEMO-LINE-8 TO BB-MEMO-LINE(8).
           MOVE QBB-MEMO-LINE-9 TO BB-MEMO-LINE(9).
           MOVE QBB-MEMO-LINE-10 TO BB-MEMO-LINE(10).

           IF ( QBB-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO BB-LTOUCH-DATE
           ELSE
              MOVE QBB-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO BB-LTOUCH-DATE.

 
      *-----------------------------------------------------------------
      *    LOAD QBB-REC FROM BB-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-BB-FIELDS.
           MOVE BB-CATAGORY TO QBB-CATAGORY.
           MOVE BB-SUBCATAGORY TO QBB-SUBCATAGORY.
           MOVE BB-USERID TO QBB-USERID.
           MOVE BB-SEQNO TO QBB-SEQNO.
           MOVE BB-NAME TO QBB-NAME.
           MOVE BB-LOGNAME TO QBB-LOGNAME.
           MOVE BB-POSTNAME TO QBB-POSTNAME.
           MOVE BB-OTHERBR TO QBB-OTHERBR.

           IF ( BB-POSTDATE = ZEROES )
              MOVE "1900-01-01" TO QBB-POSTDATE
           ELSE
              MOVE BB-POSTDATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QBB-POSTDATE.

           MOVE BB-POSTTIME TO QBB-POSTTIME.
           MOVE BB-PASSWORD TO QBB-PASSWORD.
           MOVE BB-GROUP TO QBB-GROUP.
           MOVE BB-BRNO TO QBB-BRNO.
           MOVE BB-ACTION TO QBB-ACTION.
           MOVE BB-MEMO-LINE(1) TO QBB-MEMO-LINE-1.
           MOVE BB-MEMO-LINE(2) TO QBB-MEMO-LINE-2.
           MOVE BB-MEMO-LINE(3) TO QBB-MEMO-LINE-3.
           MOVE BB-MEMO-LINE(4) TO QBB-MEMO-LINE-4.
           MOVE BB-MEMO-LINE(5) TO QBB-MEMO-LINE-5.
           MOVE BB-MEMO-LINE(6) TO QBB-MEMO-LINE-6.
           MOVE BB-MEMO-LINE(7) TO QBB-MEMO-LINE-7.
           MOVE BB-MEMO-LINE(8) TO QBB-MEMO-LINE-8.
           MOVE BB-MEMO-LINE(9) TO QBB-MEMO-LINE-9.
           MOVE BB-MEMO-LINE(10) TO QBB-MEMO-LINE-10.

           IF ( BB-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QBB-LTOUCH-DATE
           ELSE
              MOVE BB-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QBB-LTOUCH-DATE.

 
 
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
              DESTROY IOBBIN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
