       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOCDBI.
       AUTHOR.        AMM.
       DATE-WRITTEN.  05-11-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOCDBI
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : CDBFILE 
      * TABLE DESC    : CODE FILE WITH BRANCH IN KEY
      * OUTPUT FILE   : XPC/IOCDBI.PRE
      * USED FOR      : LIBLP/LPWSCDBCPY  --- QCDB-REC
      *                 LIBLP/LPCD1R.CPY  --- READ
      *                 LIBLP/LPCD1RN.CPY --- READ/READ-NEXT/START
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
           "@(#)IO/IOCDBI.CBL S34 05/17/21-16:35:26 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBLP/LP01CDB.CPY".
       COPY "LIBLP/LPWSCDB.CPY".

      *=================================================================
      *
      *    Q - F I L E
      *
      *-----------------------------------------------------------------
      *    FOR UPDATING THE FILE'S WS COPY MEMBER: LIB??/??WS??.CPY
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QCDB1-WHERE-BEG-KEY.
           03  QCDB1-WBEG-TYPE            PIC X(2).
           03  QCDB1-WBEG-BRANCH          PIC 9(4).
           03  QCDB1-WBEG-CODE            PIC X(8).
 
       01  QCDB1-WHERE-END-KEY.
           03  QCDB1-WEND-TYPE            PIC X(2).
           03  QCDB1-WEND-BRANCH          PIC 9(4).
           03  QCDB1-WEND-CODE            PIC X(8).
 
       01  QCDB-REC.
           03  QCDB-TYPE                              PIC X(02).
           03  QCDB-BRANCH                            PIC 9(04).
           03  QCDB-CODE                              PIC X(08).
           03  QCDB-LTOUCH-DATE                       PIC X(10).
           03  QCDB-DESC                              PIC X(35).
           03  QCDB-CN-GROUP-ID                       PIC X(6).
           03  QCDB-CN-PHONE                          PIC 9(10).
           03  QCDB-CN-PHONE-EXT                      PIC 9(5).
           03  QCDB-GC-GLFEENO                        PIC 9(7).
           03  QCDB-GC-GLFEEFINCHG                    PIC X(1).
           03  QCDB-GC-GLFEE-INT-CHARGABLE            PIC X(1).
           03  QCDB-GC-FEE-GIFTNET                    PIC X(1).
           03  QCDB-GC-FEE-PRODNO                     PIC 9(5).
           03  QCDB-GC-FEE-PRICE                      PIC 9(4)V99.
           03  QCDB-GC-NEW-PRODNO                     PIC 9(5).
           03  QCDB-GC-FEE-MAXAMT                     PIC 9(5)V99.
           03  QCDB-GC-ALT-GLFEENO                    PIC 9(7).
           03  QCDB-GC-COMM-GLFEENO                   PIC 9(11).
           03  QCDB-GC-COMM-PERCENT                   PIC 999V99.
           03  QCDB-GC-FEE-EARNINGS-FG                PIC X.
           03  QCDB-GC-BRANCH-COMM-AMOUNT             PIC 9(6).

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

       01  LEGEND-BEG-TYPE. 
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "BEGINNING TYPE       (F3-ALL) ".

       01  LEGEND-END-TYPE.
           03  FILLER PIC X(30) VALUE "^, V, ENTER ENDING TYPE      ".

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
           03  BEG-TYPE               PIC X(02).
           03  END-TYPE               PIC X(02).
           03  BEG-BRANCH             PIC 9(04).
           03  END-BRANCH             PIC 9(04).
           03  BEG-CODE               PIC X(08).
           03  END-CODE               PIC X(08).

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
           03  FILLER PIC X(29) VALUE "TYPE   BRNO     CODE         ".
           03  FILLER PIC X(40) VALUE "DESC".
           03  FILLER PIC X(30) VALUE "GROUP".
           03  FILLER PIC X(30) VALUE SPACES.
           03  FILLER PIC X(12) VALUE SPACES.

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-CDB-TYPE                   PIC X(2).
           03  FILLER                         PIC X(05).
           03  D01-CDB-BRANCH                 PIC 9(4).
           03  FILLER                         PIC X(05).
           03  D01-CDB-CODE                   PIC X(08).
           03  FILLER                         PIC X(05).
           03  D01-CDB-DESC                   PIC X(35).
           03  FILLER                         PIC X(05).
           03  D01-CDB-CN-GROUPID             PIC X(6).
           03  FILLER                         PIC X(05).
           03  D01-CDB-CN-PHONE               PIC 9(10).
           03  FILLER                         PIC X(05).
           03  D01-CDB-GC-GLFEENO             PIC 9(7).

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
       COPY "LIBIO/IOCDBI_DEF.CPY".
       COPY "LIBIO/IOCDBI_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOCDBI_SCN.CPY".

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
           MOVE "B"                  TO CDB-PATH-OVERRIDE.

           MOVE BEG-TYPE             TO QCDB1-WBEG-TYPE.
           MOVE BEG-BRANCH           TO QCDB1-WBEG-BRANCH.
           MOVE BEG-CODE             TO QCDB1-WBEG-CODE.

           MOVE END-TYPE             TO QCDB1-WEND-TYPE.
           MOVE END-BRANCH           TO QCDB1-WEND-BRANCH.
           MOVE END-CODE             TO QCDB1-WEND-CODE.

           PERFORM OPEN-CDB1-FILE.
           PERFORM START-CDB1-FILE.

       MAIN-PROGRAM-NEXT.

           ADD 01 TO RECORDS-READ.

           PERFORM READ-CDB1-FILE-NEXT.
           IF ( IO-BAD )
              GO TO MAIN-PROGRAM-END.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( CDB-TYPE < BEG-TYPE )
              GO TO MAIN-PROGRAM-NEXT
           ELSE
           IF ( CDB-TYPE > END-TYPE )
              GO TO MAIN-PROGRAM-END.

           IF CDB-BRANCH < BEG-BRANCH OR > END-BRANCH
              GO TO MAIN-PROGRAM-NEXT.
           IF CDB-CODE < BEG-CODE OR > END-CODE
              GO TO MAIN-PROGRAM-NEXT.

      *-------------------------------------------------------{ RANGES }
           MOVE CDB-TYPE                   TO D01-CDB-TYPE.
           MOVE CDB-BRANCH                 TO D01-CDB-BRANCH.
           MOVE CDB-CODE                   TO D01-CDB-CODE.
           MOVE CDB-DESC                   TO D01-CDB-DESC.
           MOVE CDB-CN-GROUP-ID            TO D01-CDB-CN-GROUPID.
           MOVE CDB-CN-PHONE               TO D01-CDB-CN-PHONE.
           MOVE CDB-GC-GLFEENO             TO D01-CDB-GC-GLFEENO.

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
           MOVE "IOCDBI"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOCDBI-QUEUE-POS  TO Q-POS.
           MOVE IOCDBI-COPIES-POS TO C-POS.
           MOVE IOCDBI-PRU-POS    TO P-POS.
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
       ENTER-BEG-TYPE.

           MOVE LEGEND-BEG-TYPE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A020BEG-TYPE." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-TYPE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-TYPE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO BEG-TYPE.

      *-----------------------------------------------------------------
       ENTER-END-TYPE.

           MOVE LEGEND-END-TYPE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A020END-TYPE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-TYPE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-TYPE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUBUF TO END-TYPE.

           IF ( BEG-TYPE > END-TYPE )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-TYPE.

           GO TO ENTER-BEG-BRANCH.

      *-----------------------------------------------------------------
       ENTER-ALL-TYPE.

           MOVE "S  A020BEG-TYPE." TO VDU.
           MOVE SPACES               TO BEG-TYPE VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A020END-TYPE." TO VDU.
           MOVE ALL "Z"              TO END-TYPE VDUBUF.
           PERFORM SCREENIO.

       ENTER-BEG-BRANCH.

           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "EN N040BEG-BRANCH." TO VDU.
           PERFORM SCREENIO.

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

           GO TO ENTER-BEG-CODE.

      *-----------------------------------------------------------------
       ENTER-ALL-BRANCH.

           MOVE "SN N040BEG-BRANCH." TO VDU.
           MOVE 0                    TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N040END-BRANCH." TO VDU.
           MOVE 9999                 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.

       ENTER-BEG-CODE.

           MOVE "E  A080BEG-CODE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-BRANCH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F3-KEY ) GO TO ENTER-ALL-CODE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-CODE.

           MOVE VDUBUF TO BEG-CODE.

      *-----------------------------------------------------------------
       ENTER-END-CODE.

           MOVE "E  A080END-CODE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-CODE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-CODE.

           MOVE VDUBUF TO END-CODE.

           IF ( BEG-CODE > END-CODE )
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-CODE.

           GO TO ENTER-PROCESS-KEY.

      *-----------------------------------------------------------------
       ENTER-ALL-CODE.

           MOVE "S  A080BEG-CODE." TO VDU.
           MOVE SPACES               TO BEG-CODE VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A080END-CODE." TO VDU.
           MOVE ALL "Z"              TO END-CODE VDUBUF.
           PERFORM SCREENIO.

      *=================================================================
       ENTER-PROCESS-KEY.

           MOVE LEGEND-PROCESS-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN010PROCESS-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-CODE.
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

           MOVE "S  A000BEG-TYPE."    TO VDU.
           MOVE BEG-TYPE              TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A000END-TYPE."    TO VDU.
           MOVE END-TYPE              TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SN N000BEG-BRANCH."    TO VDU.
           MOVE BEG-BRANCH              TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SN N000END-BRANCH."    TO VDU.
           MOVE END-BRANCH              TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A000BEG-CODE."    TO VDU.
           MOVE BEG-CODE              TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A000END-CODE."    TO VDU.
           MOVE END-CODE              TO VDUBUF.
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

           MOVE CDB1-KEY     TO PACE-DETAIL-KEY.

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
           MOVE "BEGINNING TYPE               : "    TO R01-PROMPT.
           MOVE BEG-TYPE                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    TYPE               : "    TO R01-PROMPT.
           MOVE END-TYPE                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING BRANCH               : "    TO R01-PROMPT.
           MOVE BEG-BRANCH                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    BRANCH               : "    TO R01-PROMPT.
           MOVE END-BRANCH                             TO R01-TEXT.
           MOVE 1                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING CODE               : "    TO R01-PROMPT.
           MOVE BEG-CODE                             TO R01-TEXT.
           MOVE 2                                     TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING    CODE               : "    TO R01-PROMPT.
           MOVE END-CODE                             TO R01-TEXT.
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
           MOVE "SQL TABLE IO UTILITY - CDBFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOCDBI-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOCDBI-SYSDATE.
           DISPLAY IOCDBI-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOCDBI-FORM-FIELDS TO WR-BUF.
           MOVE IOCDBI-FORM-LIST TO WR-LIST.
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

           MOVE IOCDBI-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOCDBI-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOCDBI_EVA.CPY".
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
      *    LOAD CDB-REC FROM QCDB-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-CDB-FIELDS.
           MOVE QCDB-TYPE TO CDB-TYPE.
           MOVE QCDB-BRANCH TO CDB-BRANCH.
           MOVE QCDB-CODE TO CDB-CODE.

           IF ( QCDB-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO CDB-LTOUCH-DATE
           ELSE
              MOVE QCDB-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO CDB-LTOUCH-DATE.

           MOVE QCDB-DESC TO CDB-DESC.
           MOVE QCDB-CN-GROUP-ID TO CDB-CN-GROUP-ID.
           MOVE QCDB-CN-PHONE TO CDB-CN-PHONE.
           MOVE QCDB-CN-PHONE-EXT TO CDB-CN-PHONE-EXT.
           MOVE QCDB-GC-GLFEENO TO CDB-GC-GLFEENO.
           MOVE QCDB-GC-GLFEEFINCHG TO CDB-GC-GLFEEFINCHG.
           MOVE QCDB-GC-GLFEE-INT-CHARGABLE
              TO CDB-GC-GLFEE-INT-CHARGABLE.
           MOVE QCDB-GC-FEE-GIFTNET TO CDB-GC-FEE-GIFTNET.
           MOVE QCDB-GC-FEE-PRODNO TO CDB-GC-FEE-PRODNO.
           MOVE QCDB-GC-FEE-PRICE TO CDB-GC-FEE-PRICE.
           MOVE QCDB-GC-NEW-PRODNO TO CDB-GC-NEW-PRODNO.
           MOVE QCDB-GC-FEE-MAXAMT TO CDB-GC-FEE-MAXAMT.
           MOVE QCDB-GC-ALT-GLFEENO TO CDB-GC-ALT-GLFEENO.
           MOVE QCDB-GC-COMM-GLFEENO TO CDB-GC-COMM-GLFEENO.
           MOVE QCDB-GC-COMM-PERCENT TO CDB-GC-COMM-PERCENT.
           MOVE QCDB-GC-FEE-EARNINGS-FG TO CDB-GC-FEE-EARNINGS-FG.
           MOVE QCDB-GC-BRANCH-COMM-AMOUNT
              TO CDB-GC-BRANCH-COMM-AMOUNT.
 
      *-----------------------------------------------------------------
      *    LOAD QCDB-REC FROM CDB-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-CDB-FIELDS.
           MOVE CDB-TYPE TO QCDB-TYPE.
           MOVE CDB-BRANCH TO QCDB-BRANCH.
           MOVE CDB-CODE TO QCDB-CODE.

           IF ( CDB-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QCDB-LTOUCH-DATE
           ELSE
              MOVE CDB-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QCDB-LTOUCH-DATE.

           MOVE CDB-DESC TO QCDB-DESC.
           MOVE CDB-CN-GROUP-ID TO QCDB-CN-GROUP-ID.
           MOVE CDB-CN-PHONE TO QCDB-CN-PHONE.
           MOVE CDB-CN-PHONE-EXT TO QCDB-CN-PHONE-EXT.
           MOVE CDB-GC-GLFEENO TO QCDB-GC-GLFEENO.
           MOVE CDB-GC-GLFEEFINCHG TO QCDB-GC-GLFEEFINCHG.
           MOVE CDB-GC-GLFEE-INT-CHARGABLE
            TO QCDB-GC-GLFEE-INT-CHARGABLE.
           MOVE CDB-GC-FEE-GIFTNET TO QCDB-GC-FEE-GIFTNET.
           MOVE CDB-GC-FEE-PRODNO TO QCDB-GC-FEE-PRODNO.
           MOVE CDB-GC-FEE-PRICE TO QCDB-GC-FEE-PRICE.
           MOVE CDB-GC-NEW-PRODNO TO QCDB-GC-NEW-PRODNO.
           MOVE CDB-GC-FEE-MAXAMT TO QCDB-GC-FEE-MAXAMT.
           MOVE CDB-GC-ALT-GLFEENO TO QCDB-GC-ALT-GLFEENO.
           MOVE CDB-GC-COMM-GLFEENO TO QCDB-GC-COMM-GLFEENO.
           MOVE CDB-GC-COMM-PERCENT TO QCDB-GC-COMM-PERCENT.
           MOVE CDB-GC-FEE-EARNINGS-FG TO QCDB-GC-FEE-EARNINGS-FG.
           MOVE CDB-GC-BRANCH-COMM-AMOUNT
            TO QCDB-GC-BRANCH-COMM-AMOUNT.
 

      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
       LOAD-CDB1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( CDB-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( CDB-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO CDB-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO CDB-ALL-MACHINE
              MOVE CDB-ALL-PATH             TO CDB-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( CDB-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO CDB-ALL-BASE
              MOVE FILPATH-MACHINE         TO CDB-ALL-MACHINE
              MOVE CDB-ALL-PATH             TO CDB-PATH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-CDBFILE NOT = "S" )
              MOVE CDB-PATH-BEG             TO CDB-SELECT-PATH-BEG
              MOVE CDB-PATH-FILE            TO CDB-SELECT-PATH-FILE
           ELSE
              MOVE CDB-PATH-SQL             TO CDB-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-CDB1-FILE.
           PERFORM LOAD-CDB1-FILE.
           PERFORM OPEN-IT.
           MOVE CDB-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-CDB1-FILE.
           PERFORM START-IT.
           MOVE CDB-SELECT-PATH TO E-FILE.
           MOVE CDB1-KEY TO E-KEYX.

           IF ( CDB1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-CDB1-FILE.

           IF ( QCDB1-WEND-BRANCH = ZEROES    )  OR
              ( QCDB1-WEND-TYPE = SPACES )
              MOVE CDB-TYPE             TO QCDB1-WBEG-TYPE
              MOVE ALL "Z"              TO QCDB1-WEND-TYPE
              MOVE CDB-BRANCH           TO QCDB1-WBEG-BRANCH
              MOVE ALL "9"              TO QCDB1-WEND-BRANCH
              MOVE CDB-CODE             TO QCDB1-WBEG-CODE
              MOVE ALL "Z"              TO QCDB1-WEND-CODE.

           EXEC SQL
            DECLARE CDB1_CURSOR CURSOR FOR
            SELECT ISNULL(CDBFILE.CDB_TYPE,''),
                   CDBFILE.CDB_BRANCH,
                   ISNULL(CDBFILE.CDB_CODE,''),
                   ISNULL(CAST(CDBFILE.CDB_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CDBFILE.CDB_DESC,''),
                   ISNULL(CDBFILE.CDB_CN_GROUP_ID,''),
                   CDBFILE.CDB_CN_PHONE,
                   CDBFILE.CDB_CN_PHONE_EXT,
                   CDBFILE.CDB_GC_GLFEENO,
                   ISNULL(CDBFILE.CDB_GC_GLFEEFINCHG,''),
                   ISNULL(CDBFILE.CDB_GC_GLFEE_INT_CHARGABLE,''),
                   ISNULL(CDBFILE.CDB_GC_FEE_GIFTNET,''),
                   CDBFILE.CDB_GC_FEE_PRODNO,
                   CDBFILE.CDB_GC_FEE_PRICE,
                   CDBFILE.CDB_GC_NEW_PRODNO,
                   CDBFILE.CDB_GC_FEE_MAXAMT,
                   CDBFILE.CDB_GC_ALT_GLFEENO,
                   CDBFILE.CDB_GC_COMM_GLFEENO,
                   CDBFILE.CDB_GC_COMM_PERCENT,
                   ISNULL(CDBFILE.CDB_GC_FEE_EARNINGS_FG,''),
                   CDBFILE.CDB_GC_BRANCH_COMM_AMOUNT
            FROM DBO.CDBFILE
            WHERE CDBFILE.CDB_TYPE >= :QCDB1-WBEG-TYPE
              AND CDBFILE.CDB_TYPE <= :QCDB1-WEND-TYPE
              AND CDBFILE.CDB_BRANCH >= :QCDB1-WBEG-BRANCH
              AND CDBFILE.CDB_BRANCH <= :QCDB1-WEND-BRANCH
              AND CDBFILE.CDB_CODE >= :QCDB1-WBEG-CODE
              AND CDBFILE.CDB_CODE <= :QCDB1-WEND-CODE
              ORDER BY CDBFILE.CDB_TYPE,
                       CDBFILE.CDB_BRANCH,
                       CDBFILE.CDB_CODE
           END-EXEC.

           EXEC SQL
               OPEN CDB1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QCDB1-WBEG-TYPE
                          QCDB1-WEND-TYPE
                          QCDB1-WBEG-CODE
                          QCDB1-WEND-CODE.

           MOVE ZEROES TO QCDB1-WBEG-BRANCH
                          QCDB1-WEND-BRANCH.

           MOVE STAT---GOOD TO CDB1-CURSOR-STAT.


      *-----------------------------------------------------------------
       START-CDB1-FILE-GREATER.
           PERFORM START-IT.
           MOVE CDB-SELECT-PATH TO E-FILE.
           MOVE CDB1-KEY TO E-KEYX.

           IF ( CDB1-GR-CURSOR-STAT-GOOD )
              PERFORM CLOSE-CDB1-FILE.

           IF ( QCDB1-WEND-BRANCH = ZEROES    )  OR
              ( QCDB1-WEND-TYPE = SPACES )
              MOVE CDB-TYPE             TO QCDB1-WBEG-TYPE
              MOVE ALL "Z"              TO QCDB1-WEND-TYPE
              MOVE CDB-BRANCH           TO QCDB1-WBEG-BRANCH
              MOVE ALL "9"              TO QCDB1-WEND-BRANCH
              MOVE CDB-CODE             TO QCDB1-WBEG-CODE
              MOVE ALL "Z"              TO QCDB1-WEND-CODE.

           EXEC SQL
            DECLARE CDB1_GR_CURSOR CURSOR FOR
            SELECT ISNULL(CDBFILE.CDB_TYPE,''),
                   CDBFILE.CDB_BRANCH,
                   ISNULL(CDBFILE.CDB_CODE,''),
                   ISNULL(CAST(CDBFILE.CDB_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CDBFILE.CDB_DESC,''),
                   ISNULL(CDBFILE.CDB_CN_GROUP_ID,''),
                   CDBFILE.CDB_CN_PHONE,
                   CDBFILE.CDB_CN_PHONE_EXT,
                   CDBFILE.CDB_GC_GLFEENO,
                   ISNULL(CDBFILE.CDB_GC_GLFEEFINCHG,''),
                   ISNULL(CDBFILE.CDB_GC_GLFEE_INT_CHARGABLE,''),
                   ISNULL(CDBFILE.CDB_GC_FEE_GIFTNET,''),
                   CDBFILE.CDB_GC_FEE_PRODNO,
                   CDBFILE.CDB_GC_FEE_PRICE,
                   CDBFILE.CDB_GC_NEW_PRODNO,
                   CDBFILE.CDB_GC_FEE_MAXAMT,
                   CDBFILE.CDB_GC_ALT_GLFEENO,
                   CDBFILE.CDB_GC_COMM_GLFEENO,
                   CDBFILE.CDB_GC_COMM_PERCENT,
                   ISNULL(CDBFILE.CDB_GC_FEE_EARNINGS_FG,''),
                   CDBFILE.CDB_GC_BRANCH_COMM_AMOUNT
            FROM DBO.CDBFILE
            WHERE CDBFILE.CDB_TYPE > :QCDB1-WBEG-TYPE
              AND CDBFILE.CDB_TYPE <= :QCDB1-WEND-TYPE
              AND CDBFILE.CDB_BRANCH > :QCDB1-WBEG-BRANCH
              AND CDBFILE.CDB_BRANCH <= :QCDB1-WEND-BRANCH
              AND CDBFILE.CDB_CODE > :QCDB1-WBEG-CODE
              AND CDBFILE.CDB_CODE <= :QCDB1-WEND-CODE
              ORDER BY CDBFILE.CDB_TYPE,
                       CDBFILE.CDB_BRANCH,
                       CDBFILE.CDB_CODE
           END-EXEC.

           EXEC SQL
               OPEN CDB1_GR_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE SPACES TO QCDB1-WBEG-TYPE
                          QCDB1-WEND-TYPE
                          QCDB1-WBEG-CODE
                          QCDB1-WEND-CODE.

           MOVE ZEROES TO QCDB1-WBEG-BRANCH
                          QCDB1-WEND-BRANCH.

           MOVE STAT---GOOD TO CDB1-GR-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-CDB1-FILE.
           PERFORM READ-IT.
           MOVE CDB-SELECT-PATH TO E-FILE.
           MOVE CDB1-KEY TO E-KEYX.

           MOVE CDB-TYPE    TO QCDB-TYPE. 
           MOVE CDB-BRANCH  TO QCDB-BRANCH.
           MOVE CDB-CODE    TO QCDB-CODE.

           EXEC SQL
            SELECT ISNULL(CDBFILE.CDB_TYPE,''),
                   CDBFILE.CDB_BRANCH,
                   ISNULL(CDBFILE.CDB_CODE,''),
                   ISNULL(CAST(CDBFILE.CDB_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(CDBFILE.CDB_DESC,''),
                   ISNULL(CDBFILE.CDB_CN_GROUP_ID,''),
                   CDBFILE.CDB_CN_PHONE,
                   CDBFILE.CDB_CN_PHONE_EXT,
                   CDBFILE.CDB_GC_GLFEENO,
                   ISNULL(CDBFILE.CDB_GC_GLFEEFINCHG,''),
                   ISNULL(CDBFILE.CDB_GC_GLFEE_INT_CHARGABLE,''),
                   ISNULL(CDBFILE.CDB_GC_FEE_GIFTNET,''),
                   CDBFILE.CDB_GC_FEE_PRODNO,
                   CDBFILE.CDB_GC_FEE_PRICE,
                   CDBFILE.CDB_GC_NEW_PRODNO,
                   CDBFILE.CDB_GC_FEE_MAXAMT,
                   CDBFILE.CDB_GC_ALT_GLFEENO,
                   CDBFILE.CDB_GC_COMM_GLFEENO,
                   CDBFILE.CDB_GC_COMM_PERCENT,
                   ISNULL(CDBFILE.CDB_GC_FEE_EARNINGS_FG,''),
                   CDBFILE.CDB_GC_BRANCH_COMM_AMOUNT
            INTO :QCDB-REC
            FROM DBO.CDBFILE
            WHERE CDBFILE.CDB_TYPE = :QCDB-TYPE
              AND CDBFILE.CDB_BRANCH = :QCDB-BRANCH
              AND CDBFILE.CDB_CODE = :QCDB-CODE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CDB1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-CDB-FIELDS.

      *-----------------------------------------------------------------
       READ-CDB1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE CDB-SELECT-PATH TO E-FILE.
           MOVE CDB1-KEY TO E-KEYX.
           INITIALIZE QCDB-REC.

           IF ( CDB1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH CDB1_CURSOR INTO :QCDB-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-CDB1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-CDB-FIELDS.

      *-----------------------------------------------------------------
       WRITE-CDB1-FILE.

           ACCEPT CDB-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM WRITE-IT.
           MOVE CDB-SELECT-PATH TO E-FILE.
           MOVE CDB1-KEY TO E-KEYX.
           PERFORM SET-CDB-FIELDS.

           EXEC SQL
            INSERT INTO DBO.CDBFILE
             (CDBFILE.CDB_TYPE,
              CDBFILE.CDB_BRANCH,
              CDBFILE.CDB_CODE,
              CDBFILE.CDB_LTOUCH_DATE,
              CDBFILE.CDB_DESC,
              CDBFILE.CDB_CN_GROUP_ID,
              CDBFILE.CDB_CN_PHONE,
              CDBFILE.CDB_CN_PHONE_EXT,
              CDBFILE.CDB_GC_GLFEENO,
              CDBFILE.CDB_GC_GLFEEFINCHG,
              CDBFILE.CDB_GC_GLFEE_INT_CHARGABLE,
              CDBFILE.CDB_GC_FEE_GIFTNET,
              CDBFILE.CDB_GC_FEE_PRODNO,
              CDBFILE.CDB_GC_FEE_PRICE,
              CDBFILE.CDB_GC_NEW_PRODNO,
              CDBFILE.CDB_GC_FEE_MAXAMT,
              CDBFILE.CDB_GC_ALT_GLFEENO,
              CDBFILE.CDB_GC_COMM_GLFEENO,
              CDBFILE.CDB_GC_COMM_PERCENT,
              CDBFILE.CDB_GC_FEE_EARNINGS_FG,
              CDBFILE.CDB_GC_BRANCH_COMM_AMOUNT)
            VALUES
             (:QCDB-TYPE,
              :QCDB-BRANCH,
              :QCDB-CODE,
              NULLIF(:QCDB-LTOUCH-DATE,'1900-01-01'),
              :QCDB-DESC,
              :QCDB-CN-GROUP-ID,
              :QCDB-CN-PHONE,
              :QCDB-CN-PHONE-EXT,
              :QCDB-GC-GLFEENO,
              :QCDB-GC-GLFEEFINCHG,
              :QCDB-GC-GLFEE-INT-CHARGABLE,
              :QCDB-GC-FEE-GIFTNET,
              :QCDB-GC-FEE-PRODNO,
              :QCDB-GC-FEE-PRICE,
              :QCDB-GC-NEW-PRODNO,
              :QCDB-GC-FEE-MAXAMT,
              :QCDB-GC-ALT-GLFEENO,
              :QCDB-GC-COMM-GLFEENO,
              :QCDB-GC-COMM-PERCENT,
              :QCDB-GC-FEE-EARNINGS-FG,
              :QCDB-GC-BRANCH-COMM-AMOUNT)
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO WRITE-CDB1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       REWRITE-CDB1-FILE.

           ACCEPT CDB-LTOUCH-DATE FROM CENTURY-DATE.
           PERFORM REWRITE-IT.
           MOVE CDB-SELECT-PATH TO E-FILE.
           MOVE CDB1-KEY TO E-KEYX.
           PERFORM SET-CDB-FIELDS.

           EXEC SQL
            UPDATE DBO.CDBFILE
             SET
              CDBFILE.CDB_TYPE = :QCDB-TYPE,
              CDBFILE.CDB_BRANCH = :QCDB-BRANCH,
              CDBFILE.CDB_CODE = :QCDB-CODE,
              CDBFILE.CDB_LTOUCH_DATE =
                 NULLIF(:QCDB-LTOUCH-DATE,'1900-01-01'),
              CDBFILE.CDB_DESC = :QCDB-DESC,
              CDBFILE.CDB_CN_GROUP_ID = :QCDB-CN-GROUP-ID,
              CDBFILE.CDB_CN_PHONE = :QCDB-CN-PHONE,
              CDBFILE.CDB_CN_PHONE_EXT = :QCDB-CN-PHONE-EXT,
              CDBFILE.CDB_GC_GLFEENO = :QCDB-GC-GLFEENO,
              CDBFILE.CDB_GC_GLFEEFINCHG = :QCDB-GC-GLFEEFINCHG,
              CDBFILE.CDB_GC_GLFEE_INT_CHARGABLE =
                   :QCDB-GC-GLFEE-INT-CHARGABLE,
              CDBFILE.CDB_GC_FEE_GIFTNET = :QCDB-GC-FEE-GIFTNET,
              CDBFILE.CDB_GC_FEE_PRODNO = :QCDB-GC-FEE-PRODNO,
              CDBFILE.CDB_GC_FEE_PRICE = :QCDB-GC-FEE-PRICE,
              CDBFILE.CDB_GC_NEW_PRODNO = :QCDB-GC-NEW-PRODNO,
              CDBFILE.CDB_GC_FEE_MAXAMT = :QCDB-GC-FEE-MAXAMT,
              CDBFILE.CDB_GC_ALT_GLFEENO = :QCDB-GC-ALT-GLFEENO,
              CDBFILE.CDB_GC_COMM_GLFEENO = :QCDB-GC-COMM-GLFEENO,
              CDBFILE.CDB_GC_COMM_PERCENT = :QCDB-GC-COMM-PERCENT,
              CDBFILE.CDB_GC_FEE_EARNINGS_FG =
                   :QCDB-GC-FEE-EARNINGS-FG,
              CDBFILE.CDB_GC_BRANCH_COMM_AMOUNT =
                   :QCDB-GC-BRANCH-COMM-AMOUNT
             WHERE CDBFILE.CDB_TYPE = :QCDB-TYPE
               AND CDBFILE.CDB_BRANCH = :QCDB-BRANCH
               AND CDBFILE.CDB_CODE = :QCDB-CODE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO REWRITE-CDB1-FILE.

           IF ( IO-FG = 0 )
              EXEC SQL COMMIT END-EXEC
              PERFORM SQL-IO-VALIDATION.

      *-----------------------------------------------------------------
       DELETE-CDB1-FILE.

           PERFORM DELETE-IT.
           MOVE CDB-SELECT-PATH TO E-FILE.
           MOVE CDB1-KEY TO E-KEYX.
           PERFORM SET-CDB-FIELDS.

           EXEC SQL
            DELETE FROM DBO.CDBFILE
             WHERE CDBFILE.CDB_TYPE = :QCDB-TYPE
               AND CDBFILE.CDB_BRANCH = :QCDB-BRANCH
               AND CDBFILE.CDB_CODE = :QCDB-CODE
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *> LOCKED
              GO TO DELETE-CDB1-FILE.

      *-----------------------------------------------------------------
       CLOSE-CDB1-FILE.
           IF ( CDB1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE CDB1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO CDB1-CURSOR-STAT.

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
              DESTROY IOCDBI-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
