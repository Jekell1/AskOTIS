       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TRWIFL.
       DATE-WRITTEN.  02-12-2024.
       AUTHOR. MARK BOOMGAARD.
      ******************************************************************
      *
      * DESCRIPTION   : COMPARE S35 TRWINFO WITH A15 TRWINFO
      *            
      *                 TO BE USED AFTER AN UPDATE
      *                 WILL COMPARE EACH FIELD AND REPORT ANY 
      *                 DIFFERENCES, AS LONG AS FIELD EXIST IN BOTH FILES
      *
      *  2 FILE MATCH PROCESSING.
      *
      * 01-08-2025 RMZ - ADDED 'BTRACK' - PROCESS TRACKING TO PROGRAM.
      * 
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

      * COPYMEMBER: LIBSP/SPFSTRWINFO
           SELECT OTRW-INFO-FILE ASSIGN TO OTRW-INFO-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OTRW-INFO-KEY
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  OTRW-INFO-FILE
           LABEL RECORDS ARE STANDARD.
      *-----------------------------------------------------------------
      * COPY "COPYMEMBER: LIBSP/SPFDTRWINFO" REPLACING LEADING "XX" BY "OXX".
      *-----------------------------------------------------------------
       78  OTRW-SIZE                       VALUE  471.
       01  OTRW-INFO-REC.
           03  OTRW-INFO-KEY.
               05  OTRW-INFO-ID             PIC 9(4).
           03  OTRW-INFO-EQUIFAX-ID         PIC X(10).
           03  OTRW-INFO-EXPERIAN-ID        PIC X(5).
           03  OTRW-INFO-TUC-ID             PIC X(10).
           03  OTRW-INFO-REPORTER-NAME      PIC X(40).
           03  OTRW-INFO-REPORTER-ADDRESS.
               05  OTRW-INFO-REPORTER-ADR1  PIC X(32).
               05  OTRW-INFO-REPORTER-ADR2  PIC X(32).
               05  OTRW-INFO-REPORTER-CSZ   PIC X(32).
           03  OTRW-INFO-REPORTER-PHONE     PIC 9(10)    COMP-3.
           03  OTRW-INFO-GRANTOR-CODE       PIC X(20).
           03  OTRW-INFO-METRO2-PURCHASED   PIC X.
           03  OTRW-INFO-CR-LF              PIC X.
           03  OTRW-INFO-BRNO-ACCTNO        PIC X.
           03  OTRW-INFO-IDENT-BRNO         PIC X.
           03  OTRW-YEARS-STOP-REPORTING    PIC 99.
           03  OTRW-YEARS-STOP-X REDEFINES OTRW-YEARS-STOP-REPORTING
                                           PIC XX.
           03  OTRW-INVALID-SSNO            PIC X.
               88  OTRW-INVALID-SSNO-VALID  VALUE " " "0" "9".
           03  OTRW-INFO-SCHED-PAY          PIC X.
           03  FILLER                      PIC X(272).

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)UT/TRWIFL.CBL S35 01/03/25-09:33:18 >".

       COPY "LIBSP/SP01TRWINFO.CPY".
       COPY "LIBSP/SP01TRWINFO_SQL.CPY".
       COPY "LIBSP/SPWSTRWINFO.CPY".

       01  OTRW-INFO-PATH              PIC X(35) VALUE SPACES.
       01  OTRW-INFO-ALL-PATH.
           03  OTRW-INFO-ALL-BASE      PIC X(9).
           03  FILLER                  PIC X     VALUE "/".
           03  OTRW-INFO-ALL-MACHINE   PIC XX.
           03  FILLER                  PIC X(11) VALUE
               "/GB/TRWINFO".

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

       01  MISC-CURSOR-STAT            PIC X(2) VALUE SPACES.
           88  MISC-CURSOR-STAT-GOOD            VALUE "00".
           88  MISC-CURSOR-STAT-BAD             VALUE "  " THRU "//"
                                                      "01" THRU "~~".

      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01  TRW-EOF-SW                  PIC X      VALUE "N".
       01  OTRW-EOF-SW                 PIC X      VALUE "N".
       01  A15-FILE-FOUND-SW           PIC X      VALUE "N".
       01  TOTAL-ERRORS-CTR            PIC 9(9)   VALUE 0.
       01  PRINTING-RANGES-SW          PIC X      VALUE "N".
       01  SUB1                        PIC 999    VALUE 0.

       01  MACHINE-R1.
           03  FILLER                  PIC X VALUE X"72".
           03  FILLER                  PIC 9 VALUE 1.
       01  MACHINE-R2.
           03  FILLER                  PIC X VALUE X"72".
           03  FILLER                  PIC 9 VALUE 2.

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

       01  LEGEND-BEG-ID.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F7-EXIT, EN".
           03  FILLER PIC X(30) VALUE "TER BEGINNING ID             ".
           03  FILLER PIC X(19) VALUE "                   ".

       01  LEGEND-END-ID.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER ENDIN".
           03  FILLER PIC X(30) VALUE "G ID                          ".
           03  FILLER PIC X(19) VALUE "                   ".

      *-----------------------------------------------------------------
       01  LEGEND-ACCEPT-YN.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT             ".
           03  FILLER PIC X(30) VALUE "                              ".
           03  FILLER PIC X(19) VALUE "     ACCEPT (Y/N): ".

       01  SOURCE-PATH-LIST            PIC X(07)     VALUE "S-PATH.".

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  SOURCE-PATH           PIC X(9).
           03  BEG-ID              PIC X(04).
           03  END-ID              PIC X(04).

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
           "TRW INFORMATION FILE (TRWINFO) COMPARE A15 TO S35".

       01  HEADER-LINE-03.
           03  FILLER                PIC X(4)  VALUE " ID ".
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
           03  D01-ID                         PIC ZZZ9.
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
      * COPY "LIBGB/BRSECUREW.CPY".
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
       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBUT/TRWIFL_DEF.CPY".
       COPY "LIBUT/TRWIFL_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/TRWIFL_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE OTRW-INFO-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-TRW-INFO-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE OTRW-INFO-FILE.
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
           MOVE "TRWIFL" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE TRWIFL-QUEUE-POS  TO Q-POS.
           MOVE TRWIFL-COPIES-POS TO C-POS.
           MOVE TRWIFL-PRU-POS    TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           PERFORM GET-TIME.
           MOVE TIME-EDIT                  TO STARTING-TIME.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS......" TO LEGEND
              PERFORM SEND-LEGEND.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO SYSDATE-YYYYMMDD.

           PERFORM START-MISC-CSR.
           PERFORM READ-MISC-CSR-NEXT.
           PERFORM CLOSE-MISC-CSR.

           PERFORM OPEN-TRW-INFO-FILE.
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
              GO TO ENTER-END-ID.

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

       ENTER-BEG-ID.

           MOVE LEGEND-BEG-ID TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040BEG-ID." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F3-KEY ) GO TO ENTER-ALL-ID.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-ID.

           MOVE VDUNUM0 TO BEG-ID.

      *-----------------------------------------------------------------
       ENTER-END-ID.

           MOVE LEGEND-END-ID TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040END-ID." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-ID.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-ID.

           MOVE VDUNUM0 TO END-ID.

           IF ( BEG-ID > END-ID )
              MOVE "INVALID ID RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-ID.

           GO TO ENTER-PARAMETERS-END.

      *-----------------------------------------------------------------
       ENTER-ALL-ID.

           MOVE "SNNN040BEG-ID." TO VDU.
           MOVE 0           TO BEG-ID VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040END-ID." TO VDU.
           MOVE 9999               TO END-ID VDUNUM0.
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

           MOVE "S  A040BEG-ID." TO VDU.
           MOVE BEG-ID              TO VDUBUF.
           PERFORM SCREENIO
           MOVE "S  A040END-ID." TO VDU.
           MOVE END-ID              TO VDUBUF.
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

           MOVE "B"                        TO TRW-INFO-PATH-OVERRIDE.

           MOVE BEG-ID  TO QTRW-INFO-WBEG-INFO-ID.
           MOVE END-ID  TO QTRW-INFO-WEND-INFO-ID.

           MOVE "N"      TO OTRW-EOF-SW 
                            TRW-EOF-SW.

           MOVE "PROCESSING S35 DATA" TO BTRACK-MESS. 
           PERFORM BTRACK-GLOBAL.

           PERFORM OPEN-TRW-INFO-FILE.
           PERFORM START-TRW-INFO-FILE.
           PERFORM M-READ-TRW.

           MOVE SOURCE-PATH        TO OTRW-INFO-ALL-BASE.
           MOVE MACHINE-R1         TO OTRW-INFO-ALL-MACHINE.
           MOVE OTRW-INFO-ALL-PATH TO OTRW-INFO-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE MACHINE-R2         TO OTRW-INFO-ALL-MACHINE
              MOVE OTRW-INFO-ALL-PATH TO OTRW-INFO-PATH ACCESS-BUF
              PERFORM ACCESS-CALL
              IF ( STAT-BAD )
                 MOVE "MISSING A15 FILE" TO D01-TEXT 
                 MOVE OTRW-INFO-ALL-PATH TO D01-A15
                 MOVE SPACES             TO D01-S35
                 PERFORM WRITE-DETAIL-LINE
                 GO TO PROCESS-DRIVER-END.

           MOVE "PROCESSING A15 DATA" TO BTRACK-MESS.
           PERFORM BTRACK-GLOBAL.
 
           PERFORM OPEN-OTRW-INFO-FILE.
           PERFORM M-READ-OTRW.

           PERFORM PROCESS-NEXT-TRW
              UNTIL TRW-EOF-SW = "Y" AND OTRW-EOF-SW = "Y".
 
           PERFORM CLOSE-TRW-INFO-FILE.
           PERFORM CLOSE-OTRW-INFO-FILE.

           GO TO PROCESS-DRIVER-END.

       PROCESS-NEXT-TRW.

           PERFORM SEND-PROGRESS-INDICATOR.

           PERFORM COMPARE-FIELDS.

           IF OTRW-INFO-KEY < TRW-INFO-KEY
              PERFORM M-READ-OTRW
           ELSE
              IF OTRW-INFO-KEY > TRW-INFO-KEY
                 PERFORM M-READ-TRW
              ELSE
                 PERFORM M-READ-OTRW
                 PERFORM M-READ-TRW.

       PROCESS-DRIVER-END.
           EXIT. 

       M-READ-TRW SECTION.
           PERFORM READ-TRW-INFO-FILE-NEXT.
           IF IO-BAD
              MOVE ALL '9' TO TRW-INFO-KEY
              MOVE "Y"     TO TRW-EOF-SW
           ELSE
              ADD 1 TO SQL-RECORDS-PROCESSED.

       EXIT-M-READ-TRW.
           EXIT.

       M-READ-OTRW SECTION.
           PERFORM READ-OTRW-INFO-FILE-NEXT.
           IF IO-BAD OR OTRW-INFO-ID > END-ID
              MOVE ALL '9' TO OTRW-INFO-KEY
              MOVE "Y"     TO OTRW-EOF-SW
           ELSE
              IF OTRW-INFO-ID < BEG-ID
                 GO TO M-READ-OTRW
              ELSE
                 ADD 1 TO A15-RECORDS-PROCESSED.

       EXIT-M-READ-OTRW.
           EXIT.

      ******************************************************************
       COMPARE-FIELDS SECTION.
      ******************************************************************

           IF OTRW-INFO-KEY < TRW-INFO-KEY
              MOVE "MISSING S35 RECORD" TO D01-TEXT
              MOVE OTRW-INFO-ID TO D01-ID
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.

           IF OTRW-INFO-KEY > TRW-INFO-KEY
              MOVE "MISSING A15 RECORD" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.

           IF TRW-INFO-EQUIFAX-ID NOT = OTRW-INFO-EQUIFAX-ID
              MOVE "TRW-INFO-EQUIFAX-ID" TO D01-TEXT
              MOVE OTRW-INFO-EQUIFAX-ID  TO D01-A15
              MOVE TRW-INFO-EQUIFAX-ID   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TRW-INFO-EXPERIAN-ID NOT = OTRW-INFO-EXPERIAN-ID
              MOVE "TRW-INFO-EXPERIAN-ID"  TO D01-TEXT
              MOVE OTRW-INFO-EXPERIAN-ID   TO D01-A15
              MOVE TRW-INFO-EXPERIAN-ID    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.
 
           IF TRW-INFO-TUC-ID NOT = OTRW-INFO-TUC-ID
              MOVE "TRW-INFO-TUC-ID" TO D01-TEXT
              MOVE OTRW-INFO-TUC-ID  TO D01-A15
              MOVE TRW-INFO-TUC-ID   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TRW-INFO-REPORTER-NAME NOT = OTRW-INFO-REPORTER-NAME
              MOVE "TRW-INFO-REPORTER-NAME" TO D01-TEXT
              MOVE OTRW-INFO-REPORTER-NAME  TO D01-A15
              MOVE TRW-INFO-REPORTER-NAME   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF TRW-INFO-REPORTER-ADR1 NOT = OTRW-INFO-REPORTER-ADR1
              MOVE "TRW-INFO-REPORTER-ADR1" TO D01-TEXT
              MOVE OTRW-INFO-REPORTER-ADR1  TO D01-A15-NUM
              MOVE TRW-INFO-REPORTER-ADR1   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TRW-INFO-REPORTER-ADR2 NOT = OTRW-INFO-REPORTER-ADR2
              MOVE "TRW-INFO-REPORTER-ADR2" TO D01-TEXT
              MOVE OTRW-INFO-REPORTER-ADR2  TO D01-A15-NUM
              MOVE TRW-INFO-REPORTER-ADR2   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TRW-INFO-REPORTER-CSZ NOT = OTRW-INFO-REPORTER-CSZ
              MOVE "TRW-INFO-REPORTER-CSZ" TO D01-TEXT
              MOVE OTRW-INFO-REPORTER-CSZ  TO D01-A15-NUM
              MOVE TRW-INFO-REPORTER-CSZ   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TRW-INFO-REPORTER-PHONE NOT = OTRW-INFO-REPORTER-PHONE
              MOVE "TRW-INFO-REPORTER-PHONE" TO D01-TEXT
              MOVE OTRW-INFO-REPORTER-PHONE  TO D01-A15-NUM
              MOVE TRW-INFO-REPORTER-PHONE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TRW-INFO-GRANTOR-CODE NOT = OTRW-INFO-GRANTOR-CODE
              MOVE "TRW-INFO-GRANTOR-CODE" TO D01-TEXT
              MOVE OTRW-INFO-GRANTOR-CODE  TO D01-A15-NUM
              MOVE TRW-INFO-GRANTOR-CODE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OTRW-INFO-CR-LF NOT = OTRW-INFO-CR-LF
              MOVE "OTRW-INFO-CR-LF" TO D01-TEXT
              MOVE OTRW-INFO-CR-LF   TO D01-A15-NUM
              MOVE TRW-INFO-CR-LF    TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TRW-INFO-BRNO-ACCTNO NOT = OTRW-INFO-BRNO-ACCTNO
              MOVE "TRW-INFO-BRNO-ACCTNO" TO D01-TEXT
              MOVE OTRW-INFO-BRNO-ACCTNO  TO D01-A15-NUM
              MOVE TRW-INFO-BRNO-ACCTNO   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TRW-INFO-IDENT-BRNO NOT = OTRW-INFO-IDENT-BRNO
              MOVE "TRW-INFO-IDENT-BRNO" TO D01-TEXT
              MOVE OTRW-INFO-IDENT-BRNO  TO D01-A15-NUM
              MOVE TRW-INFO-IDENT-BRNO   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OTRW-YEARS-STOP-REPORTING NOT = OTRW-YEARS-STOP-REPORTING
              MOVE "OTRW-YEARS-STOP-REPORTING" TO D01-TEXT
              MOVE OTRW-YEARS-STOP-REPORTING  TO D01-A15-NUM
              MOVE TRW-YEARS-STOP-REPORTING   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TRW-INVALID-SSNO NOT = OTRW-INVALID-SSNO
              MOVE "TRW-INVALID-SSNO" TO D01-TEXT
              MOVE OTRW-INVALID-SSNO  TO D01-A15-NUM
              MOVE TRW-INVALID-SSNO   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF TRW-INFO-SCHED-PAY NOT = OTRW-INFO-SCHED-PAY
              MOVE "TRW-INFO-SCHED-PAY" TO D01-TEXT
              MOVE OTRW-INFO-SCHED-PAY  TO D01-A15-NUM
              MOVE TRW-INFO-SCHED-PAY   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

       EXIT-COMPARE-FIELDS.
           EXIT.

       WRITE-DETAIL-LINE-ERROR-KEY SECTION.

           MOVE TRW-INFO-ID   TO D01-ID.

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
           MOVE BEG-ID                           TO R01-NUMERIC.
           MOVE 01                                 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE
           MOVE "ENDING CODE                   : " TO R01-PROMPT.
           MOVE END-ID                           TO R01-NUMERIC.
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
      * COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/PROGID.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBWI/PFSWINDOWS.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO TRWIFL-CONAME.
           MOVE EXT-CONAME-SYSDATE TO TRWIFL-SYSDATE.
           DISPLAY TRWIFL-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE TRWIFL-FORM-FIELDS TO WR-BUF.
           MOVE TRWIFL-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE TRWIFL-HELP--FILE TO HELP-LINK-FILE.
           MOVE TRWIFL-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/TRWIFL_EVA.CPY".
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
       COPY "LIBSP/SPTRWINFOGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBSP/SPTRWINFORN.CPY".

       OPEN-OTRW-INFO-FILE.
           PERFORM OPEN-IT.
           MOVE OTRW-INFO-PATH TO E-FILE.
           OPEN INPUT OTRW-INFO-FILE.
           IF IO-FG = 8
              GO TO OPEN-OTRW-INFO-FILE.
           IF IO-FG = 7
              CLOSE OTRW-INFO-FILE
              GO TO OPEN-OTRW-INFO-FILE.

        READ-OTRW-INFO-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OTRW-INFO-PATH TO E-FILE.
           READ OTRW-INFO-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OTRW-INFO-FILE-NEXT.

       CLOSE-OTRW-INFO-FILE.
           PERFORM CLOSE-IT.
           CLOSE OTRW-INFO-FILE.

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
              DESTROY TRWIFL-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
