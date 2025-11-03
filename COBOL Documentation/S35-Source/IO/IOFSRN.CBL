       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IOFSRN.
       AUTHOR.        CS.   
       DATE-WRITTEN.  03-15-2021.
      ******************************************************************
      *
      * IDENTIFICATION: IO/IOFSRN
      * DESCRIPTION   : IO PROGRAM FOR SQL PRE-COMPILE LOGIC
      * TABLE NAME    : FSFILE 
      * TABLE DESC    : FS FILE
      * OUTPUT FILE   : XPC/IOFSRN.PRE
      * USED FOR      : LIBGL/GLWSFS.CPY  --- QFS-REC
      *                 LIBGL/GLFS1R.CPY  --- READ
      *                 LIBGL/GLFS1RN.CPY --- READ/READ-NEXT/START
      *
      *=================================================================
      * REV:
      * 20210316  **NEW**
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
           "@(#)IO/IOFSRN.CBL S34 07/15/21-13:27:42 >".

      ******************************************************************
      *
      *    F I L E - W O R K E R S
      *
      ******************************************************************
       COPY "LIBGL/GL01FS.CPY".
       COPY "LIBGL/GLWSFS.CPY".

      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S WS COPY MEMBERS
      *#################################################################
????  *ONLY NEED HERE IF DATE



      *=================================================================
      *
      *    Q - F I L E
      *
      *-----------------------------------------------------------------
      *    FOR UPDATING THE FILE'S WS COPY MEMBER: LIBGL/GSWSFS.CPY
      *=================================================================
       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  QFS1-WHERE-BEG-KEY.
           03  QFS1-WBEG-REPORT                 PIC 9(2).   
           03  QFS1-WBEG-LINENO                 PIC 9(4).
 
       01  QFS1-WHERE-END-KEY.
           03  QFS1-WEND-REPORT            PIC 9(2).   
           03  QFS1-WEND-LINENO            PIC 9(4).
 
       01  QFS-REC.
           03  QFS-REPORT                       PIC 99.
           03  QFS-LINENO                       PIC 9(4).
           03  QFS-LTOUCH-DATE                  PIC X(10).
           03  QFS-TYPE                     PIC X.
           03  QFS-LEVEL                    PIC 9.
           03  QFS-CONTROL                  PIC 9.
           03  QFS-ACCOUNT                  PIC 9(11).
           03  QFS-DESCR                    PIC X(36).
           03  QFS-CMONTH                   PIC S9(9)V99.
           03  QFS-LYMONTH                  PIC S9(9)V99.
           03  QFS-CYTD                     PIC S9(9)V99.
           03  QFS-LYTD                     PIC S9(9)V99.
           03  QFS-CMBUD                    PIC S9(9).
           03  QFS-YTDBUD                   PIC S9(9).
           03  QFS-ANBUD                    PIC S9(9).
           03  QFS-REFNO                    PIC 9(4).
           03  QFS-PERIOD                   PIC 99.
           03  QFS-PRNCD                    PIC X.
           03  QFS-SIGN                     PIC X.
           03  QFS-DRCR                     PIC X.
           03  QFS-BR                       PIC 9(4).
           03  QFS-BR2                      PIC 9(4).

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

       01  LEGEND-REPORT.    
           03  FILLER PIC X(30) VALUE "F1-PRINTER, F7-EXIT, V, ENTER ".
           03  FILLER PIC X(30) VALUE "REPORT NO:                    ".


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
           03  BEG-REPORT                  PIC 9(2).
           03  END-REPORT                  PIC 9(2).
 
           03  BEG-LINENO                  PIC 9(4).
           03  END-LINENO                  PIC 9(4).

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
           03  FILLER PIC X(29) VALUE "RPT LINE#  TYPE CTRL         ".            
           03  FILLER PIC X(19) VALUE "GLNUM   DESCRIPTION".      

      *=================================================================
       01  DETAIL-LINE                        PIC X(132)   VALUE SPACES.

      *-----------------------------------------------------------------
       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-FS-REPORT                  PIC 99.
           03  FILLER                         PIC X(3).
           03  D01-FS-LINENO                  PIC 9(4).
           03  FILLER                         PIC X(3).
           03  D01-FS-TYPE                    PIC X.
           03  FILLER                         PIC X(6).
           03  D01-FS-CONTROL                 PIC 9.
           03  FILLER                         PIC X(3).
           03  D01-FS-ACCOUNT                 PIC 9(11).
           03  FILLER                         PIC X(3).
           03  D01-FS-DESCR                   PIC X(36). 
           03  FILLER                         PIC X.
           03  D01-FS-LTOUCH-DATE             PIC 9(8).

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
       COPY "LIBIO/IOFSRN_DEF.CPY".
       COPY "LIBIO/IOFSRN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *=================================================================
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBIO/IOFSRN_SCN.CPY".

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
      D    STOP MESS.
           PERFORM INITIALIZATION.

           IF ( EXT-EOBUF-ERRCD NOT = SPACES )
              GO TO MAIN-PROGRAM-EXIT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      D    STOP FORM-PATHNAME.
           MOVE "B"                        TO FS-PATH-OVERRIDE.
           MOVE BEG-REPORT                 TO QFS1-WBEG-REPORT.
           MOVE BEG-LINENO                 TO QFS1-WBEG-LINENO.  

           MOVE END-REPORT                 TO QFS1-WEND-REPORT.
           MOVE END-LINENO                 TO QFS1-WEND-LINENO.


           PERFORM OPEN-FS1-FILE.
           PERFORM START-FS1-FILE.

      *=================================================================
       MAIN-PROGRAM-NEXT.

           ADD 01 TO RECORDS-READ.

           PERFORM READ-FS1-FILE-NEXT.

           IF ( IO-BAD )
              GO TO MAIN-PROGRAM-END.

           PERFORM PACE-ROUTINE.
           PERFORM TRAP-INTERRUPT.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF FS-REPORT < BEG-REPORT
              GO TO MAIN-PROGRAM-END.

           IF FS-REPORT > END-REPORT
              GO TO MAIN-PROGRAM-END.
           
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE FS-REPORT TO     D01-FS-REPORT.
           MOVE FS-LINENO TO     D01-FS-LINENO.
           MOVE FS-TYPE TO       D01-FS-TYPE.
           MOVE FS-CONTROL TO    D01-FS-CONTROL.
           MOVE FS-ACCOUNT TO    D01-FS-ACCOUNT.
           MOVE FS-DESCR   TO    D01-FS-DESCR.

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
           MOVE "IOFSRN"          TO FORM-PROG VDU-FORM-NAME.
           MOVE IOFSRN-QUEUE-POS  TO Q-POS.
           MOVE IOFSRN-COPIES-POS TO C-POS.
           MOVE IOFSRN-PRU-POS    TO P-POS.
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
       ENTER-REPORT1.      

           MOVE LEGEND-REPORT  TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN020REPORT-1."    TO VDU.
           PERFORM SCREENIO.

      *    IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
      *    IF (        F3-KEY ) GO TO ENTER-ALL-GROUP.
      *KC  IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-REPORT1.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-REPORT.


%      ENTER-REPORT2.      

           MOVE LEGEND-REPORT  TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN020REPORT-2."    TO VDU.
           PERFORM SCREENIO.

      *    IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
      *    IF (        F3-KEY ) GO TO ENTER-ALL-GROUP.
      *KC  IF (        F6-KEY ) PERFORM SCAN-GR1-FILE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (UP-KEY)          GO TO ENTER-REPORT1.

           IF ( NOT ENTER-KEY ) GO TO ENTER-REPORT2.      
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-REPORT.
 
           IF BEG-REPORT > END-REPORT
             MOVE "INVALID RANGE" TO MESS
             PERFORM SEND-MESS
             GO TO ENTER-REPORT2.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       LINE1.
           MOVE "ENNN040LINENO-1."    TO VDU.
           PERFORM SCREENIO.
           
           IF  (F3-KEY)         GO TO ALL-LINES.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (UP-KEY) GO TO ENTER-REPORT2.
           IF ( NOT ENTER-KEY ) GO TO LINE1.        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-LINENO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       LINE2.
           MOVE "ENNN040LINENO-2."    TO VDU.
           PERFORM SCREENIO.

           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (UP-KEY)     GO TO LINE1.
           IF ( NOT ENTER-KEY ) GO TO LINE2.                
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-LINENO.

           IF BEG-LINENO > END-LINENO
              GO TO LINE2.
 
           GO TO ENTER-PROCESS-KEY.
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
       ALL-LINES.
           MOVE "SNNN040LINENO-1." TO VDU.
           MOVE 0                TO BEG-LINENO VDUNUM0.
           PERFORM SCREENIO.
 
           MOVE "SNNN040LINENO-2." TO VDU.
           MOVE 9999             TO END-LINENO VDUNUM0.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       ENTER-PROCESS-KEY.
           MOVE LEGEND-PROCESS-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN010PROCESS-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO LINE2.             
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-EXIT.
           IF ( NOT ENTER-KEY ) GO TO ENTER-PROCESS-KEY.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           IF ( VDUNUM0 < ZEROES ) OR
              ( VDUNUM0 > 1      )      *> %%%%%% SET FOR MAX # OF KEYS
              MOVE "ERROR: RANGE IS INVALID" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-PROCESS-KEY.

           MOVE VDUNUM0 TO PROCESS-KEY.


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

      *%%% MOVE "S  A020REPORT-1."       TO VDU.
           MOVE BEG-REPORT             TO VDUNUM0.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *%%% MOVE "S  A020REPORT-2."       TO VDU.
           MOVE END-REPORT             TO VDUNUM0.
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

           MOVE FS1-KEY     TO PACE-DETAIL-KEY.

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

           MOVE "BEG REPORT NO                : "    TO R01-PROMPT.   
           MOVE BEG-REPORT                           TO R01-NUMERIC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

           MOVE "END REPORT NO                : "    TO R01-PROMPT.   
           MOVE END-REPORT                           TO R01-NUMERIC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEG LINE NO                  :"    TO R01-PROMPT.
           MOVE BEG-LINENO                          TO R01-NUMERIC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "END LINE NO                  :"    TO R01-PROMPT.
           MOVE END-LINENO                          TO R01-NUMERIC.
           MOVE 2 TO LN-FEED.
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
           MOVE "SQL TABLE IO UTILITY - FSFILE"  TO H02-TITLE.
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

           MOVE EXT-CONAME-CONAME TO IOFSRN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IOFSRN-SYSDATE.
           DISPLAY IOFSRN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IOFSRN-FORM-FIELDS TO WR-BUF.
           MOVE IOFSRN-FORM-LIST TO WR-LIST.
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

           MOVE IOFSRN-HELP--FILE TO HELP-LINK-FILE.
           MOVE IOFSRN-HELP--DIR  TO HELP-LINK-DIR.

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
              COPY "LIBIO/IOFSRN_EVA.CPY".
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
      *    LOAD FS-REC FROM QFS-REC; FOR READ & READ-NEXT
      *-----------------------------------------------------------------
       GET-FS-FIELDS.
           MOVE QFS-REPORT TO FS-REPORT.
           MOVE QFS-LINENO TO FS-LINENO.

           IF ( QFS-LTOUCH-DATE = "1900-01-01" )
              MOVE ZEROES TO FS-LTOUCH-DATE
           ELSE
              MOVE QFS-LTOUCH-DATE TO SQL-DATE-YYYY-MM-DD
              PERFORM SQL-GET-DATE
              MOVE SQL-DATE-YYYYMMDD TO FS-LTOUCH-DATE.

           MOVE QFS-TYPE TO FS-TYPE.
           MOVE QFS-LEVEL TO FS-LEVEL.
           MOVE QFS-CONTROL TO FS-CONTROL.
           MOVE QFS-ACCOUNT TO FS-ACCOUNT.
           MOVE QFS-DESCR TO FS-DESCR.
           MOVE QFS-CMONTH TO FS-CMONTH.
           MOVE QFS-LYMONTH TO FS-LYMONTH.
           MOVE QFS-CYTD TO FS-CYTD.
           MOVE QFS-LYTD TO FS-LYTD.
           MOVE QFS-CMBUD TO FS-CMBUD.
           MOVE QFS-YTDBUD TO FS-YTDBUD.
           MOVE QFS-ANBUD TO FS-ANBUD.
           MOVE QFS-REFNO TO FS-REFNO.
           MOVE QFS-PERIOD TO FS-PERIOD.
           MOVE QFS-PRNCD TO FS-PRNCD.
           MOVE QFS-SIGN TO FS-SIGN.
           MOVE QFS-DRCR TO FS-DRCR.
           MOVE QFS-BR TO FS-BR.
           MOVE QFS-BR2 TO FS-BR2.
 
      *-----------------------------------------------------------------
      *    LOAD QFS-REC FROM FS-REC; FOR WRITE & REWRITE
      *-----------------------------------------------------------------
       SET-FS-FIELDS.
           MOVE FS-REPORT TO QFS-REPORT.
           MOVE FS-LINENO TO QFS-LINENO.

           IF ( FS-LTOUCH-DATE = ZEROES )
              MOVE "1900-01-01" TO QFS-LTOUCH-DATE
           ELSE
              MOVE FS-LTOUCH-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO QFS-LTOUCH-DATE.

           MOVE FS-TYPE TO QFS-TYPE.
           MOVE FS-LEVEL TO QFS-LEVEL.
           MOVE FS-CONTROL TO QFS-CONTROL.
           MOVE FS-ACCOUNT TO QFS-ACCOUNT.
           MOVE FS-DESCR TO QFS-DESCR.
           MOVE FS-CMONTH TO QFS-CMONTH.
           MOVE FS-LYMONTH TO QFS-LYMONTH.
           MOVE FS-CYTD TO QFS-CYTD.
           MOVE FS-LYTD TO QFS-LYTD.
           MOVE FS-CMBUD TO QFS-CMBUD.
           MOVE FS-YTDBUD TO QFS-YTDBUD.
           MOVE FS-ANBUD TO QFS-ANBUD.
           MOVE FS-REFNO TO QFS-REFNO.
           MOVE FS-PERIOD TO QFS-PERIOD.
           MOVE FS-PRNCD TO QFS-PRNCD.
           MOVE FS-SIGN TO QFS-SIGN.
           MOVE FS-DRCR TO QFS-DRCR.
           MOVE FS-BR TO QFS-BR.
           MOVE FS-BR2 TO QFS-BR2.
 
      *#################################################################
      *    BEGINNING OF UPDATE TO THE FILE'S IO COPY MEMBERS
      *#################################################################
      * COPYMEMBER: LIBGB/GLFS1RN
       LOAD-FS1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( FS-PATH-OVERRIDE = "Y" )
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( FS-PATH-OVERRIDE = SPACES )
              MOVE EXT-FILPATH-BASE        TO FS-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO FS-ALL-MACHINE
              MOVE FS-ALL-PATH             TO FS-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( FS-PATH-OVERRIDE = "B" )
              MOVE FILPATH-BASE            TO FS-ALL-BASE
              MOVE FILPATH-MACHINE         TO FS-ALL-MACHINE
              MOVE FS-ALL-PATH             TO FS-PATH.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *    CREATE PATH FOR FILE SELECT AND SQL TABLE NAME
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF ( EXT-IO-FSFILE NOT = "S" )
              MOVE FS-PATH-BEG             TO FS-SELECT-PATH-BEG
              MOVE FS-PATH-FILE            TO FS-SELECT-PATH-FILE
           ELSE
              MOVE FS-PATH-SQL             TO FS-SELECT-PATH.

      *-----------------------------------------------------------------
       OPEN-FS1-FILE.
           PERFORM LOAD-FS1-FILE.
           PERFORM OPEN-IT.
           MOVE FS-SELECT-PATH TO E-FILE.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

      *-----------------------------------------------------------------
       START-FS1-FILE.
           PERFORM START-IT.
           MOVE FS-SELECT-PATH TO E-FILE.
           MOVE FS1-KEY TO E-KEYX.

           IF ( FS1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-FS1-FILE.

           IF ( QFS1-WEND-LINENO = ZEROES    ) OR
              ( QFS1-WEND-LINENO NOT NUMERIC )
              MOVE FS-REPORT            TO QFS1-WBEG-REPORT
              MOVE ALL "9"              TO QFS1-WEND-REPORT
              MOVE FS-LINENO            TO QFS1-WBEG-LINENO
              MOVE ALL "9"              TO QFS1-WEND-LINENO.

           EXEC SQL
            DECLARE FS1_CURSOR CURSOR FOR
            SELECT FSFILE.FS_REPORT,
                   FSFILE.FS_LINENO,
                   ISNULL(CAST(FSFILE.FS_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(FSFILE.FS_TYPE,''),
                   FSFILE.FS_LEVEL,
                   FSFILE.FS_CONTROL,
                   FSFILE.FS_ACCOUNT,
                   ISNULL(FSFILE.FS_DESCR,''),
                   FSFILE.FS_CMONTH,
                   FSFILE.FS_LYMONTH,
                   FSFILE.FS_CYTD,
                   FSFILE.FS_LYTD,
                   FSFILE.FS_CMBUD,
                   FSFILE.FS_YTDBUD,
                   FSFILE.FS_ANBUD,
                   FSFILE.FS_REFNO,
                   FSFILE.FS_PERIOD,
                   ISNULL(FSFILE.FS_PRNCD,''),
                   ISNULL(FSFILE.FS_SIGN,''),
                   ISNULL(FSFILE.FS_DRCR,''),
                   FSFILE.FS_BR,
                   FSFILE.FS_BR2
            FROM DBO.FSFILE
            WHERE FSFILE.FS_REPORT >= :QFS1-WBEG-REPORT
              AND FSFILE.FS_REPORT <= :QFS1-WEND-REPORT
              AND FSFILE.FS_LINENO >= :QFS1-WBEG-LINENO 
              AND FSFILE.FS_LINENO <= :QFS1-WEND-LINENO 
            ORDER BY FSFILE.FS_REPORT,
                     FSFILE.FS_LINENO
           END-EXEC.

           EXEC SQL
               OPEN FS1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QFS1-WBEG-REPORT
                          QFS1-WBEG-LINENO
                          QFS1-WEND-REPORT
                          QFS1-WEND-LINENO. 

           MOVE STAT---GOOD TO FS1-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-FS1-FILE.
           PERFORM READ-IT.
           MOVE FS-SELECT-PATH TO E-FILE.
           MOVE FS1-KEY TO E-KEYX.

           MOVE FS-REPORT TO QFS-REPORT.
           MOVE FS-LINENO TO QFS-LINENO.

           EXEC SQL
            SELECT FSFILE.FS_REPORT,
                   FSFILE.FS_LINENO,
                   ISNULL(CAST(FSFILE.FS_LTOUCH_DATE
                                                  AS VARCHAR(10)),'0'),
                   ISNULL(FSFILE.FS_TYPE,''),
                   FSFILE.FS_LEVEL,
                   FSFILE.FS_CONTROL,
                   FSFILE.FS_ACCOUNT,
                   ISNULL(FSFILE.FS_DESCR,''),
                   FSFILE.FS_CMONTH,
                   FSFILE.FS_LYMONTH,
                   FSFILE.FS_CYTD,
                   FSFILE.FS_LYTD,
                   FSFILE.FS_CMBUD,
                   FSFILE.FS_YTDBUD,
                   FSFILE.FS_ANBUD,
                   FSFILE.FS_REFNO,
                   FSFILE.FS_PERIOD,
                   ISNULL(FSFILE.FS_PRNCD,''),
                   ISNULL(FSFILE.FS_SIGN,''),
                   ISNULL(FSFILE.FS_DRCR,''),
                   FSFILE.FS_BR,
                   FSFILE.FS_BR2
            INTO :QFS-REC
            FROM DBO.FSFILE
            WHERE FSFILE.FS_REPORT = :QFS-REPORT
              AND FSFILE.FS_LINENO = :QFS-LINENO
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-FS1-FILE.

           IF ( IO-FG = 0 )
              PERFORM GET-FS-FIELDS.

      *-----------------------------------------------------------------
       READ-FS1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE FS-SELECT-PATH TO E-FILE.
           MOVE FS1-KEY TO E-KEYX.
           INITIALIZE QFS-REC.

           IF ( FS1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH FS1_CURSOR INTO :QFS-REC
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-FS1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-FS-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-FS1-FILE.
           IF ( FS1-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE FS1_CURSOR
              END-EXEC
              MOVE STAT---BAD TO FS1-CURSOR-STAT.

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
              DESTROY IOFSRN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.

           EXIT PROGRAM.
