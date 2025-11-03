       IDENTIFICATION DIVISION.
       PROGRAM-ID.    LCFILE.
       DATE-WRITTEN.  02-08-2024.
       AUTHOR. MARK BOOMGAARD.
      ******************************************************************
      *
      * DESCRIPTION   : COMPARE S35 LCFILE WITH A15 LCFILE
      *            
      *                 TO BE USED AFTER AN UPDATE
      *                 WILL COMPARE EACH FIELD AND REPORT ANY 
      *                 DIFFERENCES, AS LONG AS FIELD EXIST IN BOTH FILES
      *
      *  2 FILE MATCH PROCESSING.
      *
      *  01-07-25  RMZ  ADDED 'BTRACK' - PROCESS TRACKING TO PROGRAM.
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

      * COPYMEMBER: LIBLP/LPFSLC
           SELECT OLC-FILE ASSIGN TO OLC-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OLC1-KEY
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  OLC-FILE
           LABEL RECORDS ARE STANDARD.
      *-----------------------------------------------------------------
       COPY "LIBUP/A15SRC/LP01LC.CPY" REPLACING LEADING "LC" BY "OLC".
      *-----------------------------------------------------------------

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)UT/LCFILE.CBL S35 01/02/25-15:10:04 >".

       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
 
       01  OLC-PATH                    PIC X(35) VALUE SPACES.
       01  OLC-ALL-PATH.
           03  OLC-ALL-BASE            PIC X(9).
           03  FILLER                  PIC X     VALUE "/".
           03  OLC-ALL-MACHINE         PIC XX.
           03  FILLER                  PIC X(10) VALUE
               "/LP/LCFILE".

      *********************************************************
      *        SQL DECLARE SECTION - WORKERS
      *********************************************************

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  PASS-SQL                    PIC X(200).
       01  RET-DB-NAME                 PIC X(50).
       01  RET-SERVER-NAME             PIC X(50).
       01  RET-VERSION                 PIC X(100).
       01  MISC-RETURN-CODE            PIC S9(4)   COMP-5.
       EXEC SQL END DECLARE SECTION END-EXEC.

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  LC-EOF-SW                   PIC X      VALUE "N".
       01  OLC-EOF-SW                  PIC X      VALUE "N".
       01  A15-FILE-FOUND-SW           PIC X      VALUE "N".
       01  TOTAL-ERRORS-CTR            PIC 9(9)   VALUE 0.
       01  PRINTING-RANGES-SW          PIC X      VALUE "N".
       01  SUB1                        PIC 999    VALUE 0.

       01  OLC-USE-KEY.
           03 OLC-USE-KEY-CODE         PIC 9(3).

       01  MACHINE-R1.
           03  FILLER                  PIC X VALUE X"72".
           03  FILLER                  PIC 9 VALUE 1.
       01  MACHINE-R2.
           03  FILLER                  PIC X VALUE X"72".
           03  FILLER                  PIC 9 VALUE 2.

       01  MISC-CURSOR-STAT            PIC X(2) VALUE SPACES.
           88  MISC-CURSOR-STAT-GOOD            VALUE "00".
           88  MISC-CURSOR-STAT-BAD             VALUE "  " THRU "//"
                                                      "01" THRU "~~".

       01  PROCESS-STAT                PIC X(08)    VALUE SPACES.
           88  PROCESS-STAT-GOOD                    VALUE "00".
           88  PROCESS-STAT-BAD                     VALUE "  " THRU "//"
                                                         "01" THRU "~~".
       01  A15-RECORDS-PROCESSED       PIC 9(9)     VALUE 0.
       01  SQL-RECORDS-PROCESSED       PIC 9(9)     VALUE 0.

       01  STARTING-TIME               PIC X(08)     VALUE SPACES.
       01  SUB                         PIC 9(04)     VALUE ZEROES.

       01  SYSDATE-YYYYMMDD            PIC 9(8)     VALUE ZEROES.

       01  LEGEND                      PIC X(79)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(07)     VALUE "LEGEND.".

       01  LEGEND-BEG-CODE.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F7-EXIT, EN".
           03  FILLER PIC X(30) VALUE "TER BEGINNING CODE           ".
           03  FILLER PIC X(19) VALUE "                   ".

       01  LEGEND-END-CODE.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER ENDIN".
           03  FILLER PIC X(30) VALUE "G CODE                        ".
           03  FILLER PIC X(19) VALUE "                   ".

      *-----------------------------------------------------------------
       01  LEGEND-ACCEPT-YN.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT             ".
           03  FILLER PIC X(30) VALUE "                              ".
           03  FILLER PIC X(19) VALUE "     ACCEPT (Y/N): ".

       01  SOURCE-PATH-LIST            PIC X(07)     VALUE "S-PATH.".

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  SOURCE-PATH           PIC X(9).
           03  BEG-CODE              PIC 9(3).
           03  END-CODE              PIC 9(3).

       01  VERSION-CONTROL           PIC X(01)     VALUE "A".

      *-----------------------------------------------------------------
       01  LN-COUNT                  PIC 9(03)     VALUE 99.
       01  LN-FEED                   PIC 9(02).
       01  PAGE-NUMBER               PIC 9(04)     VALUE ZEROES.

      ******************************************************************
      *
      *    H E A D E R - L I N E - W O R K E R S
      *
      ******************************************************************
       01  HEADER-LINE-01.
           03  H01-DATE                PIC X(08) VALUE SPACES.
           03  FILLER                  PIC X(02) VALUE SPACES.
           03  H01-TIME                PIC X(08) VALUE SPACES.
           03  FILLER                  PIC X(6)  VALUE SPACES.
           03  H01-NAME                PIC X(40) VALUE SPACES.
           03  FILLER                  PIC X(26) VALUE SPACES.
           03  FILLER                  PIC X(6)  VALUE "PAGE: ".
           03  H01-PAGE                PIC Z(04).

      *-----------------------------------------------------------------
       01  HEADER-LINE-02.
           03  FILLER                  PIC X(10) VALUE "USER ID:".
           03  H02-USERID              PIC X(10) VALUE SPACES.
           03  FILLER                  PIC X(5)  VALUE SPACES.
           03  H02-TITLE               PIC X(65) VALUE 
           "LOAN CLASS FILE (LCFILE) COMPARE A15 TO S35".

       01  HEADER-LINE-03.
           03  FILLER                PIC X(4)  VALUE "CODE".
           03  FILLER                PIC X(2)  VALUE SPACES.
           03  FILLER                PIC X(23) VALUE "DESCRIPTION".
           03  FILLER                PIC X(27) VALUE "A15".
           03  FILLER                PIC X(3)  VALUE "S35".

      ******************************************************************
      *
      *    D E T A I L - L I N E - W O R K E R S
      *
      ******************************************************************
       01  DETAIL-LINE                        PIC X(120)   VALUE SPACES.

       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-CODE                       PIC ZZZ9.
           03  FILLER                         PIC X(2).
           03  D01-TEXT                       PIC X(20).
           03  FILLER                         PIC X(3).
           03  D01-A15                        PIC X(25).
           03  FILLER REDEFINES D01-A15.
               05  D01-A15-NUM                PIC Z(7)9.9(3).
               05  FILLER                     PIC X(13).
           03  FILLER                         PIC X(2).
           03  D01-S35                        PIC X(25).
           03  FILLER REDEFINES D01-S35.
               05  D01-S35-NUM                PIC Z(7)9.9(3).
               05  FILLER                     PIC X(13).

       01  RANGE-LINE-01                      REDEFINES DETAIL-LINE.
           03  R01-PROMPT                     PIC X(32).
           03  R01-FIELD                      PIC X(48).
           03  R01-AMT                        REDEFINES R01-FIELD
                                              PIC ZZZ,ZZZ,ZZ9.99-.
           03  R01-NUMERIC                    REDEFINES R01-FIELD
                                              PIC Z(10)9-.
           03  R01-TEXT                       REDEFINES R01-FIELD
                                              PIC X(48).

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBCL/BTRACKW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GPENVW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBGB/PROGIDW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBUT/LCFILE_DEF.CPY".
       COPY "LIBUT/LCFILE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/LCFILE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE OLC-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-OLC1-FILE.
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

           IF ( PROCESS-STAT-BAD )
              GO TO MAIN-PROGRAM-EXIT.

           PERFORM BTRACK-CREATE.

           PERFORM EXECUTION-MODULE.

           PERFORM RANGE-LINES.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

           MOVE STAT---GOOD                TO PROCESS-STAT.

           MOVE FORM-PATHNAME              TO FORM-PATH.
           MOVE "LCFILE" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE LCFILE-QUEUE-POS  TO Q-POS.
           MOVE LCFILE-COPIES-POS TO C-POS.
           MOVE LCFILE-PRU-POS    TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           PERFORM GET-TIME.
           MOVE TIME-EDIT                  TO STARTING-TIME.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS......" TO LEGEND
              PERFORM SEND-LEGEND.

           PERFORM START-MISC-CSR.
           PERFORM READ-MISC-CSR-NEXT.
           PERFORM CLOSE-MISC-CSR.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO SYSDATE-YYYYMMDD.

           PERFORM OPEN-LC1-FILE.
           PERFORM OPEN-PR-FILE.

           GO TO INITIALIZATION-EXIT.

       INITIALIZATION-END.

           MOVE "X"                        TO EXT-EOBUF-ERRCD.
           MOVE STAT---BAD                 TO PROCESS-STAT.

       INITIALIZATION-EXIT.
           EXIT.

      ***************************************************
       ENTER-PARAMETERS SECTION.
      ***************************************************

           MOVE STAT---GOOD  TO PROCESS-STAT.

           IF ( UP-KEY )
              GO TO ENTER-END-CODE.

       ENTER-SOURCE-PATH.

           IF SOURCE-PATH = SPACES
              MOVE "A15_PATH"       TO GETENV-BUF
              PERFORM GETENV-CALL
              IF ( STAT-GOOD )
                 MOVE GETENV-BUF    TO SOURCE-PATH
              ELSE
              MOVE "/USR/DATA"      TO SOURCE-PATH
              END-IF
              CALL "C$TOLOWER" USING SOURCE-PATH, VALUE 9
              MOVE SOURCE-PATH-LIST TO WR-LIST
              MOVE SOURCE-PATH      TO WR-BUF
              PERFORM CALL-WRFORM.

           MOVE "E  A090S-PATH." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF NOT ( ENTER-KEY ) GO TO ENTER-SOURCE-PATH.

           IF ( VDUBUF = SPACES )
              GO TO ENTER-SOURCE-PATH.

           MOVE VDUBUF TO DIR-ACCESS-BUF SOURCE-PATH.
           PERFORM DIR-ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE "INVALID SOURCE DATA PATH" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-SOURCE-PATH.

           MOVE SPACES                     TO DIR-ACCESS-BUF.
           STRING VDUBUF, "/", EXT-FILPATH-MACHINE
                  DELIMITED BY " "       INTO DIR-ACCESS-BUF.
           PERFORM DIR-ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE "INVALID SOURCE DATA PATH" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-SOURCE-PATH.

           MOVE VDUBUF                TO SOURCE-PATH.

       ENTER-BEG-CODE.

           MOVE LEGEND-BEG-CODE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040BEG-CODE." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F3-KEY ) GO TO ENTER-ALL-CODE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-CODE.

           MOVE VDUNUM0 TO BEG-CODE.

      *-----------------------------------------------------------------
       ENTER-END-CODE.

           MOVE LEGEND-END-CODE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040END-CODE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-CODE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-CODE.

           MOVE VDUNUM0 TO END-CODE.

           IF ( BEG-CODE > END-CODE )
              MOVE "INVALID CODE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-CODE.

           GO TO ENTER-PARAMETERS-END.

      *-----------------------------------------------------------------
       ENTER-ALL-CODE.

           MOVE "SN N040BEG-CODE." TO VDU.
           MOVE ZEROS              TO BEG-CODE VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N040END-CODE." TO VDU.
           MOVE 9999               TO END-CODE VDUNUM0.
           PERFORM SCREENIO.
 
       ENTER-PARAMETERS-END.

           MOVE LEGEND-ACCEPT-YN TO LEGEND.
           PERFORM SEND-LEGEND.

           IF ( F1-KEY ) MOVE STAT---BAD   TO PROCESS-STAT.
           IF ( F7-KEY ) MOVE STAT---BAD   TO PROCESS-STAT.

       ENTER-PARAMETERS-EXIT.
           EXIT.

      ****************************************
       EO-EXEC-AUDIT SECTION.
      ****************************************

      ****************************************
       DISPLAY-PARAMETERS SECTION.
      ****************************************

           MOVE "SN N040BEG-CODE." TO VDU.
           MOVE BEG-CODE              TO VDUNUM0.
           PERFORM SCREENIO

           MOVE "SN N040END-CODE." TO VDU.
           MOVE END-CODE              TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A090S-PATH."        TO VDU.     
           MOVE SOURCE-PATH             TO VDUBUF.
           PERFORM SCREENIO.

       DISPLAY-PARAMETERS-EXIT.
           EXIT.

      *******************************************
       EXECUTION-MODULE SECTION.
      *******************************************

           MOVE 9999 TO PROGID-SLEEP-COUNTER.
           PERFORM SEND-PROGRESS-INDICATOR.

      D    STOP MESS.

      *     MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE MACHINE-R1                 TO FILPATH-MACHINE.
      *     MOVE GR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO LC-PATH-OVERRIDE.

           MOVE BEG-CODE  TO QLC1-WBEG-CODE.
           MOVE END-CODE  TO QLC1-WEND-CODE.

           MOVE "N"      TO OLC-EOF-SW 
                            LC-EOF-SW.

           MOVE "PROCESSING S35 DATA" TO BTRACK-MESS.
           PERFORM BTRACK-GLOBAL.

           PERFORM OPEN-LC1-FILE.
           PERFORM START-LC1-FILE.
           PERFORM M-READ-LC.

           MOVE SOURCE-PATH   TO OLC-ALL-BASE.
           MOVE MACHINE-R1    TO OLC-ALL-MACHINE.
           MOVE OLC-ALL-PATH  TO OLC-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE "MISSING R1 A15 FILE" TO D01-TEXT
              MOVE OLC-ALL-PATH          TO D01-A15
              MOVE SPACES                TO D01-S35
              PERFORM WRITE-DETAIL-LINE
           GO TO PROCESS-DRIVER-END.

           MOVE "PROCESSING A15 DATA" TO BTRACK-MESS.  
           PERFORM BTRACK-GLOBAL.

           PERFORM OPEN-OLC1-FILE.
           PERFORM M-READ-OLC1.

           PERFORM PROCESS-NEXT-LC
              UNTIL LC-EOF-SW = "Y" AND OLC-EOF-SW = "Y".
 
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-OLC1-FILE.

           GO TO PROCESS-DRIVER-END.

       PROCESS-NEXT-LC.

           PERFORM SEND-PROGRESS-INDICATOR.

           PERFORM COMPARE-FIELDS.

           IF OLC-USE-KEY < LC1-KEY
              PERFORM M-READ-OLC1
           ELSE
              IF OLC-USE-KEY > LC1-KEY
                 PERFORM M-READ-LC
              ELSE
                 PERFORM M-READ-OLC1
                 PERFORM M-READ-LC.

       PROCESS-DRIVER-END.
           EXIT. 

       M-READ-LC SECTION.
           PERFORM READ-LC1-FILE-NEXT.
           IF IO-BAD
              MOVE ALL '9'  TO LC1-KEY
              MOVE "Y"      TO LC-EOF-SW
           ELSE
              ADD 1 TO SQL-RECORDS-PROCESSED.

       EXIT-M-READ-LC.
           EXIT.

       M-READ-OLC1 SECTION.
           PERFORM READ-OLC1-FILE-NEXT.
           IF IO-BAD OR OLC-CODE > END-CODE
              MOVE ALL '9'  TO OLC-USE-KEY
              MOVE "Y"      TO OLC-EOF-SW
           ELSE
              IF OLC-CODE < BEG-CODE
                 GO TO M-READ-OLC1
              ELSE
                 MOVE OLC-CODE TO OLC-USE-KEY-CODE
                 ADD 1 TO A15-RECORDS-PROCESSED.

       EXIT-M-READ-OLC1.
           EXIT.

      ******************************************************************
       COMPARE-FIELDS SECTION.
      ******************************************************************

           IF OLC-USE-KEY < LC1-KEY
              MOVE "MISSING S35 RECORD" TO D01-TEXT
              MOVE OLC-CODE TO D01-CODE
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.

           IF OLC-USE-KEY > LC1-KEY
              MOVE "MISSING A15 RECORD" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.

           IF LC-DESC NOT = OLC-DESC
              MOVE "LC-DESC" TO D01-TEXT
              MOVE OLC-DESC  TO D01-A15
              MOVE LC-DESC   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LC-SMALLOAN NOT = OLC-SMALLOAN
              MOVE "LC-SMALLOAN"  TO D01-TEXT
              MOVE OLC-SMALLOAN   TO D01-A15
              MOVE LC-SMALLOAN    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LC-SALEFIN NOT = OLC-SALEFIN
              MOVE "LC-SALEFIN" TO D01-TEXT
              MOVE OLC-SALEFIN  TO D01-A15
              MOVE LC-SALEFIN   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LC-SERVACCT NOT = OLC-SERVACCT
              MOVE "LC-SERVACCT" TO D01-TEXT
              MOVE OLC-SERVACCT  TO D01-A15
              MOVE LC-SERVACCT   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF LC-LOANTYPE NOT = OLC-LOANTYPE
              MOVE "LC-LOANTYPE" TO D01-TEXT
              MOVE OLC-LOANTYPE  TO D01-A15-NUM
              MOVE LC-LOANTYPE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LC-PROF1 NOT = OLC-PROF1
              MOVE "LC-PROF1" TO D01-TEXT
              MOVE OLC-PROF1  TO D01-A15
              MOVE LC-PROF1   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LC-PROF2 NOT = OLC-PROF2
              MOVE "LC-PROF2" TO D01-TEXT
              MOVE OLC-PROF2  TO D01-A15
              MOVE LC-PROF2   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LC-SKIP-TRW NOT = OLC-SKIP-TRW
              MOVE "LC-SKIP-TRW" TO D01-TEXT
              MOVE OLC-SKIP-TRW  TO D01-A15
              MOVE LC-SKIP-TRW   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LC-DISPLAY-ERN NOT = OLC-DISPLAY-ERN
              MOVE "LC-DISPLAY-ERN" TO D01-TEXT
              MOVE OLC-DISPLAY-ERN  TO D01-A15 
              MOVE LC-DISPLAY-ERN   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LC-LCARDFG NOT = OLC-LCARDFG
              MOVE "LC-LCARDFG" TO D01-TEXT
              MOVE OLC-LCARDFG  TO D01-A15
              MOVE LC-LCARDFG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LC-CALC-HI-CREDIT NOT = OLC-CALC-HI-CREDIT
              MOVE "LC-CALC-HI-CREDIT" TO D01-TEXT
              MOVE OLC-CALC-HI-CREDIT  TO D01-A15
              MOVE LC-CALC-HI-CREDIT   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LC-STOP-ON-DLNO NOT = OLC-STOP-ON-DLNO
              MOVE "LC-STOP-ON-DLNO" TO D01-TEXT
              MOVE OLC-STOP-ON-DLNO  TO D01-A15
              MOVE LC-STOP-ON-DLNO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LC-AFP-CLASS NOT = OLC-AFP-CLASS
              MOVE "LC-AFP-CLASS" TO D01-TEXT
              MOVE OLC-AFP-CLASS  TO D01-A15
              MOVE LC-AFP-CLASS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LC-PASTDUE NOT = OLC-PASTDUE
              MOVE "LC-PASTDUE" TO D01-TEXT
              MOVE OLC-PASTDUE  TO D01-A15
              MOVE LC-PASTDUE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LC-GIFTNET NOT = OLC-GIFTNET
              MOVE "LC-GIFTNET" TO D01-TEXT
              MOVE OLC-GIFTNET  TO D01-A15
              MOVE LC-GIFTNET   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LC-APP-REQ-FOR-LOAN NOT = OLC-APP-REQ-FOR-LOAN
              MOVE "LC-APP-REQ-FOR-LOAN" TO D01-TEXT
              MOVE OLC-APP-REQ-FOR-LOAN  TO D01-A15
              MOVE LC-APP-REQ-FOR-LOAN   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LC-POOLID NOT = OLC-POOLID
              MOVE "LC-POOLID" TO D01-TEXT
              MOVE OLC-POOLID  TO D01-A15
              MOVE LC-POOLID   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LC-USE-ALT-GL-FEE NOT = OLC-USE-ALT-GL-FEE
              MOVE "LC-USE-ALT-GL-FEE" TO D01-TEXT
              MOVE OLC-USE-ALT-GL-FEE  TO D01-A15
              MOVE LC-USE-ALT-GL-FEE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LC-PROGRP-CLASS NOT = OLC-PROGRP-CLASS
              MOVE "LC-PROGRP-CLASS" TO D01-TEXT
              MOVE OLC-PROGRP-CLASS  TO D01-A15
              MOVE LC-PROGRP-CLASS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF LC-ALLOW-CASH-PROCEEDS NOT = OLC-ALLOW-CASH-PROCEEDS
              MOVE "LC-ALLOW-CASH-PROCEEDS" TO D01-TEXT
              MOVE OLC-ALLOW-CASH-PROCEEDS  TO D01-A15
              MOVE LC-ALLOW-CASH-PROCEEDS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

        EXIT-COMPARE-FIELDS.
           EXIT.

       WRITE-DETAIL-LINE-ERROR-KEY SECTION.

           MOVE LC-CODE  TO D01-CODE.

       WRITE-DETAIL-LINE-ERROR-KEY-EXIT.
           EXIT.

       WRITE-DETAIL-LINE-ERROR SECTION.

           ADD 1    TO TOTAL-ERRORS-CTR.
           PERFORM WRITE-DETAIL-LINE.

       WRITE-DETAIL-LINE-ERROR-EXIT.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************

           ADD  LN-FEED TO LN-COUNT.

           IF ( LN-COUNT > EXT-CONAME-RPT-LINES )
              PERFORM WRITE-HEADER-LINES.

           MOVE DETAIL-LINE                TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 01                         TO LN-FEED.
           MOVE SPACES                     TO DETAIL-LINE.

       WRITE-DETAIL-LINE-EXIT.
           EXIT.

      ******************************************************************
       WRITE-HEADER-LINES SECTION.
      ******************************************************************

           MOVE 01                         TO LN-COUNT.
           ADD  01                         TO PAGE-NUMBER.
           PERFORM GET-TIME.

           MOVE EXT-CONAME-SYSDATE         TO H01-DATE.
           MOVE TIME-EDIT                  TO H01-TIME.
           MOVE EXT-CONAME-CONAME          TO H01-NAME.
           CALL "C$JUSTIFY"             USING H01-NAME "C".
           MOVE PAGE-NUMBER                TO H01-PAGE.

           MOVE HEADER-LINE-01             TO PR-REC.
           MOVE 99                         TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE EXT-CONAME-USER-ID         TO H02-USERID.

           MOVE HEADER-LINE-02             TO PR-REC.
           ADD  01                         TO LN-COUNT.
           MOVE 01                         TO LN-FEED.
           PERFORM WRITE-PR-REC.

           IF PRINTING-RANGES-SW = "N"
              MOVE HEADER-LINE-03          TO PR-REC
              ADD  02                      TO LN-COUNT
              MOVE 02                      TO LN-FEED
              PERFORM WRITE-PR-REC.

           MOVE 2                          TO LN-FEED.

       WRITE-HEADER-LINES-EXIT.
           EXIT.

      ******************************************************************
       RANGE-LINES SECTION.
      ******************************************************************

           MOVE 88  TO LN-COUNT.
           MOVE "Y" TO PRINTING-RANGES-SW.

           MOVE "BEGINNING CODE                : " TO R01-PROMPT.
           MOVE BEG-CODE                           TO R01-NUMERIC.
           MOVE 01                                 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING CODE                   : " TO R01-PROMPT.
           MOVE END-CODE                           TO R01-NUMERIC.
           MOVE 01                                 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "A15 RECORDS PROCESSED         :"     TO R01-PROMPT.
           MOVE A15-RECORDS-PROCESSED                 TO R01-NUMERIC.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "S35 RECORDS PROCESSED         :"     TO R01-PROMPT.
           MOVE SQL-RECORDS-PROCESSED                 TO R01-NUMERIC.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL ERRORS REPORTED         :"     TO R01-PROMPT.
           MOVE TOTAL-ERRORS-CTR                      TO R01-NUMERIC.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "STARTING TIME                 : "    TO R01-PROMPT.
           MOVE STARTING-TIME                         TO R01-TEXT.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING   TIME                 : "    TO R01-PROMPT.
           PERFORM GET-TIME.
           MOVE TIME-EDIT                             TO R01-TEXT.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "DATABASE NAME : "                    TO R01-PROMPT.
           MOVE RET-DB-NAME                           TO R01-TEXT.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       RANGE-LINES-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBCL/BTRACK.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/PROGID.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO LCFILE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LCFILE-SYSDATE.
           DISPLAY LCFILE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE LCFILE-FORM-FIELDS TO WR-BUF.
           MOVE LCFILE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE LCFILE-HELP--FILE TO HELP-LINK-FILE.
           MOVE LCFILE-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/LCFILE_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".

       INITIAL-PROGRESS-INDICATOR.

           MOVE ZEROES                 TO PROGID-BRANCH.
           MOVE "LEGEND:B."            TO PROGID-LINE-LIST.
           MOVE PRU-LP                 TO PROGID-PRU-LP.
           MOVE EXT-ROUTE-BUF          TO PROGID-ROUTE-BUF.
           MOVE 9999                   TO PROGID-SLEEP-COUNTER.
           MOVE 250                    TO PROGID-SLEEP-COUNTER-MAX.

           PERFORM GET-TIME.
           MOVE TIME-EDIT              TO PROGID-TIME.

           MOVE "B"                    TO PROGID-TYPE.
           MOVE PROGID-TEXT-PROCESSING TO PROGID-TEXT.

      *-----------------------------------------------------------------
       SEND-PROGRESS-INDICATOR.

           PERFORM GET-TIME.
           MOVE TIME-EDIT                  TO PROGID-TIME.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PROGID-DISPLAY.

       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPLC1RN.CPY".

       OPEN-OLC1-FILE.
           PERFORM OPEN-IT.
           MOVE OLC-PATH TO E-FILE.
           OPEN INPUT OLC-FILE.
           IF IO-FG = 8
              GO TO OPEN-OLC1-FILE.
           IF IO-FG = 7
              CLOSE OLC-FILE
              GO TO OPEN-OLC1-FILE.

        READ-OLC1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OLC-PATH TO E-FILE.
           READ OLC-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OLC1-FILE-NEXT.

       CLOSE-OLC1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OLC-FILE.

      *=================================================================
       START-MISC-CSR.
           MOVE "SSP_TCLP_SELECT" TO E-FILE.
           MOVE SPACES           TO E-KEYX.
           MOVE "START ERROR"    TO E-MSG.
           MOVE "GB/SQLINF"      TO E-PROG.

           IF ( MISC-CURSOR-STAT-GOOD )
              PERFORM CLOSE-MISC-CSR.

           MOVE "SELECT DB_NAME(),@@SERVERNAME,@@VERSION" 
              TO PASS-SQL.

           EXEC SQL
            DECLARE MISC_CURSOR CURSOR FOR
                :MISC-RETURN-CODE = 
                 EXEC SSP_TCLP_SELECT
                  ( :PASS-SQL)
                END-EXEC.

           EXEC SQL
               OPEN MISC_CURSOR
           END-EXEC.

           IF SQLCODE NOT = 0
             PERFORM SQL-ERROR.

           MOVE STAT---GOOD TO MISC-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-MISC-CSR-NEXT.
           MOVE "SSP_TCLP_SELECT" TO E-FILE.
           MOVE SPACES           TO E-KEYX.
           MOVE "READ ERROR"     TO E-MSG.
           MOVE "GB/SQLINF"      TO E-PROG.

           IF ( MISC-CURSOR-STAT-GOOD )
              EXEC SQL
                 FETCH MISC_CURSOR INTO  
                   :RET-DB-NAME,
                   :RET-SERVER-NAME, 
                   :RET-VERSION
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           IF SQLCODE NOT = 0
             PERFORM SQL-ERROR.

      *-----------------------------------------------------------------
       CLOSE-MISC-CSR.
           MOVE "SSP_TCLP_SELECT" TO E-FILE.
           MOVE SPACES           TO E-KEYX.
           MOVE "CLOSE ERROR"    TO E-MSG.
           MOVE "GB/SQLINF"      TO E-PROG.

           IF ( MISC-CURSOR-STAT-GOOD )
              EXEC SQL
                   CLOSE MISC_CURSOR
              END-EXEC
              MOVE STAT---BAD TO MISC-CURSOR-STAT.

      *-----------------------------------------------------------------

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************

           PERFORM CLOSE-FILES.

           IF ( EXT-ROUTE-BUF = "00" )
              PERFORM PRU-OUT.

      *     IF A15-RECORDS-PROCESSED = 0 AND SQL-RECORDS-PROCESSED = 0
      *        MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
      *        PERFORM SEND-MESS.

       EXIT-PROG.

           PERFORM BTRACK-DONE. 

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE EXIT-PATH               TO FORM-NAM
              MOVE FORM-PATH               TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY LCFILE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
