       IDENTIFICATION DIVISION.
       PROGRAM-ID.    WRFILE.
       DATE-WRITTEN.  02-12-2024.
       AUTHOR. MARK BOOMGAARD.
      ******************************************************************
      *
      * DESCRIPTION   : COMPARE S35 WRFILE WITH A15 WRFILE
      *            
      *                 TO BE USED AFTER AN UPDATE
      *                 WILL COMPARE EACH FIELD AND REPORT ANY 
      *                 DIFFERENCES, AS LONG AS FIELD EXIST IN BOTH FILES
      *
      *  2 FILE MATCH PROCESSING.
      *
      * 20250103 MB  ADDED EO-EXEC-AUDIT MOVES TO FORMAT PARM DATE WHEN RUN
      *               VIA BATCH.
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
           SELECT OWR-FILE ASSIGN TO OWR-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OWR1-KEY
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  OWR-FILE
           LABEL RECORDS ARE STANDARD.
      *-----------------------------------------------------------------
       COPY "LIBUP/A15SRC/SP01WR.CPY" REPLACING LEADING "WR" BY "OWR".

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)UT/WRFILE.CBL S35 01/03/25-12:34:48 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBSP/SP01WR.CPY".
       COPY "LIBSP/SP01WR_SQL.CPY".
       COPY "LIBSP/SPWSWR.CPY".

       01  OWR-PATH                    PIC X(35) VALUE SPACES.
       01  OWR-ALL-PATH.
           03  OWR-ALL-BASE            PIC X(9).
           03  FILLER                  PIC X     VALUE "/".
           03  OWR-ALL-MACHINE         PIC XX.
           03  FILLER                  PIC X(10) VALUE
               "/SP/WRFILE".

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
       01 BEG-BY-DATE-CCYYMMDD.
           03  BEG-BY-DATE-CCYYMM.
             05 BEG-BY-DATE-CC         PIC 99.
             05 BEG-BY-DATE-YYMM.
                07 BEG-BY-DATE-YY      PIC 99.
                07 BEG-BY-DATE-MM      PIC 99.

       01 END-BY-DATE-CCYYMMDD.
          03  END-BY-DATE-CCYYMM.
              05 END-BY-DATE-CC        PIC 99.
              05 END-BY-DATE-YYMM.
                07 END-BY-DATE-YY      PIC 99.
                07 END-BY-DATE-MM      PIC 99.

       01  WR-EOF-SW                   PIC X      VALUE "N".
       01  OWR-EOF-SW                  PIC X      VALUE "N".
       01  TOTAL-ERRORS-CTR            PIC 9(9)   VALUE 0.
       01  SUB1                        PIC 9      VALUE 0.

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

      *-----------------------------------------------------------------
       01  LEGEND-BEG-MMYY.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F7-EXIT, ENT".
           03  FILLER PIC X(30) VALUE "ER BEGINNING DATE (MMYY)      ".
           03  FILLER PIC X(19) VALUE "                   ".

       01  LEGEND-END-MMYY.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER ENDIN".
           03  FILLER PIC X(30) VALUE "G DATE (MMYY)                  ".
           03  FILLER PIC X(19) VALUE "                   ".

      *-----------------------------------------------------------------
       01  LEGEND-ACCEPT-YN.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT             ".
           03  FILLER PIC X(30) VALUE "                              ".
           03  FILLER PIC X(19) VALUE "     ACCEPT (Y/N): ".

       01  SOURCE-PATH-LIST            PIC X(07)     VALUE "S-PATH.".

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  SOURCE-PATH             PIC X(9).
           03  BEG-MMYY                PIC 9(4).
           03  END-MMYY                PIC 9(4).
 
       01  VERSION-CONTROL             PIC X(01)     VALUE "A".

      *-----------------------------------------------------------------
       01  LN-COUNT                    PIC 9(03)     VALUE 99.
       01  LN-FEED                     PIC 9(02).
       01  PAGE-NUMBER                 PIC 9(04)     VALUE ZEROES.

      ******************************************************************
      *
      *    H E A D E R - L I N E - W O R K E R S
      *
      ******************************************************************
       01  HEADER-LINE-01.
           03  H01-DATE                PIC X(08) VALUE SPACES.
           03  FILLER                  PIC X(02) VALUE SPACES.
           03  H01-TIME                PIC X(08) VALUE SPACES.
           03  FILLER                  PIC X(6) VALUE SPACES.
           03  H01-NAME                PIC X(40) VALUE SPACES.
           03  FILLER                  PIC X(16) VALUE SPACES.
           03  FILLER                  PIC X(06) VALUE "PAGE: ".
           03  H01-PAGE                PIC Z(04).

      *-----------------------------------------------------------------
       01  HEADER-LINE-02.
           03  FILLER                  PIC X(10) VALUE "USER ID:".
           03  H02-USERID              PIC X(10) VALUE SPACES.
           03  FILLER                  PIC X(10) VALUE SPACES.
           03  H02-TITLE               PIC X(60) VALUE 
           "WORLD RATING SHEET FILE (WRFILE) COMPARE A15 TO S35".

       01  HEADER-LINE-03.
           03  FILLER                PIC X(6) VALUE " DATE ".
           03  FILLER                PIC X(3) VALUE SPACES.
           03  FILLER                PIC X(4) VALUE "BRNO".
           03  FILLER                PIC X(4) VALUE SPACES.
           03  FILLER                PIC X(11) VALUE "DESCRIPTION".
           03  FILLER                PIC X(18) VALUE SPACES.
           03  FILLER                PIC X(3) VALUE "A15".
           03  FILLER                PIC X(19) VALUE SPACES.
           03  FILLER                PIC X(3) VALUE "S35".

      ******************************************************************
      *
      *    D E T A I L - L I N E - W O R K E R S
      *
      ******************************************************************
       01  DETAIL-LINE                        PIC X(80)   VALUE SPACES.

       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-DATE                       PIC 9(6).
           03  FILLER                         PIC X(3).
           03  D01-BRNO                       PIC Z(3)9.
           03  FILLER                         PIC X(4).
           03  D01-TEXT                       PIC X(25).
           03  FILLER                         PIC X(4).
           03  D01-A15                        PIC X(20).
           03  FILLER REDEFINES D01-A15.
               05  D01-A15-NUM                PIC Z(7)9.9(3).
               05  FILLER                     PIC X(8).
           03  FILLER                         PIC X(2).
           03  D01-S35                        PIC X(20).
           03  FILLER REDEFINES D01-S35.
               05  D01-S35-NUM                PIC Z(7)9.9(3).
               05  FILLER                     PIC X(8).

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
       COPY "LIBUT/WRFILE_DEF.CPY".
       COPY "LIBUT/WRFILE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/WRFILE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE OWR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-WR1-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE OWR-FILE.
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
           MOVE "WRFILE" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE WRFILE-QUEUE-POS TO Q-POS.
           MOVE WRFILE-COPIES-POS TO C-POS.
           MOVE WRFILE-PRU-POS   TO P-POS.
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
              GO TO ENTER-END-MMYY.

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


       ENTER-BEG-MMYY.

           MOVE LEGEND-BEG-MMYY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040BEG-MMYY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-SOURCE-PATH.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F3-KEY ) GO TO ENTER-ALL-DATES.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-MMYY.

           MOVE VDUNUM0 TO DATE-MMYY.

           IF ( DATE-MMYY-MM < 01 ) OR
              ( DATE-MMYY-MM > 12 )
              MOVE "ERROR: ENTRY NOT VALID (MMYY)" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-MMYY.

           MOVE VDUNUM0 TO BEG-MMYY.

           PERFORM CONVERT-MMYY-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.
           MOVE DATE-YYYYMM TO BEG-BY-DATE-CCYYMM.

      *-----------------------------------------------------------------
       ENTER-END-MMYY.

           MOVE LEGEND-END-MMYY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN040END-MMYY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-MMYY.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-MMYY.

           MOVE VDUNUM0 TO DATE-MMYY.

           IF ( DATE-MMYY-MM < 01 ) OR
              ( DATE-MMYY-MM > 12 )
              MOVE "ERROR: ENTRY NOT VALID (MMYY)" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-END-MMYY.

           PERFORM CONVERT-MMYY-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.
           MOVE DATE-YYYYMM TO END-BY-DATE-CCYYMM.

           IF BEG-BY-DATE-CCYYMM > END-BY-DATE-CCYYMM
               MOVE "INVALID DATE RANGE !" TO MESS
               PERFORM SEND-MESS
               GO TO ENTER-BEG-MMYY.

           MOVE VDUNUM0 TO END-MMYY.

           GO TO ENTER-PARAMETERS-END.

      *-----------------------------------------------------------------
       ENTER-ALL-DATES.

           MOVE "SNNN040BEG-MMYY." TO VDU.
           MOVE 0150               TO BEG-MMYY VDUNUM0 DATE-MMYY.
           PERFORM SCREENIO.

           PERFORM CONVERT-MMYY-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.
           MOVE DATE-YYYYMM TO BEG-BY-DATE-CCYYMM.


           MOVE "SNNN040END-MMYY." TO VDU.
           MOVE 1249               TO END-MMYY VDUNUM0 DATE-MMYY.
           PERFORM SCREENIO.

           PERFORM CONVERT-MMYY-TO-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMM.
           MOVE DATE-YYYYMM TO END-BY-DATE-CCYYMM.

      *-----------------------------------------------------------------
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
           MOVE EXT-EOBUF-DATE1    TO DATE-YYYYMMDD.
           MOVE DATE-YYYYMMDD-CC   TO BEG-BY-DATE-CC.
           MOVE DATE-YYYYMMDD-YY   TO BEG-BY-DATE-YY.
           MOVE DATE-YYYYMMDD-MM   TO BEG-BY-DATE-MM.

           MOVE EXT-EOBUF-DATE2    TO DATE-YYYYMMDD.
           MOVE DATE-YYYYMMDD-CC   TO END-BY-DATE-CC.
           MOVE DATE-YYYYMMDD-YY   TO END-BY-DATE-YY.
           MOVE DATE-YYYYMMDD-MM   TO END-BY-DATE-MM.

      *    PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
      *    PERFORM CONVERT-MMYY-TO-MMDDYY.
      *    PERFORM CONVERT-MMDDYY-TO-YYYYMM.
      *    MOVE DATE-YYYYMM TO BEG-BY-DATE-CCYYMM.

      *    MOVE END-MMYY TO DATE-MMYY.
      *    PERFORM CONVERT-MMYY-TO-MMDDYY.
      *    PERFORM CONVERT-MMDDYY-TO-YYYYMM.
      *    MOVE DATE-YYYYMM TO END-BY-DATE-CCYYMM.

      ****************************************
       DISPLAY-PARAMETERS SECTION.
      ****************************************

           MOVE "S  A090S-PATH."        TO VDU.     
           MOVE SOURCE-PATH             TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SNNN040BEG-MMYY."        TO VDU.
           MOVE BEG-MMYY                  TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN040END-MMYY."       TO VDU.
           MOVE END-MMYY                 TO VDUNUM0.
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
      *     MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO WR-PATH-OVERRIDE.

           MOVE ZEROS   TO QWR1-WBEG-BRNO.
           MOVE ALL '9' TO QWR1-WEND-BRNO.

           MOVE BEG-BY-DATE-CCYYMM  TO QWR1-WBEG-YYYYMM.
           MOVE END-BY-DATE-CCYYMM  TO QWR1-WEND-YYYYMM.

           MOVE "PROCESSING S35 DATA" TO BTRACK-MESS. 
           PERFORM BTRACK-GLOBAL.

           MOVE "N"  TO WR-EOF-SW.
           PERFORM OPEN-WR1-FILE.
           PERFORM START-WR1-FILE.
           PERFORM M-READ-WR1.

           MOVE SOURCE-PATH   TO OWR-ALL-BASE.
           MOVE MACHINE-R1    TO OWR-ALL-MACHINE.
           MOVE OWR-ALL-PATH  TO OWR-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE "MISSING A15 FILE" TO D01-TEXT 
              MOVE OWR-ALL-PATH       TO D01-A15
              MOVE SPACES             TO D01-S35
              PERFORM WRITE-DETAIL-LINE
              GO TO PROCESS-DRIVER-END.

           MOVE "PROCESSING A15 DATA" TO BTRACK-MESS.
           PERFORM BTRACK-GLOBAL.

           MOVE BEG-BY-DATE-CCYYMM TO OWR-YYYYMM.
           MOVE ZEROS              TO OWR-BRNO.
           PERFORM OPEN-OWR1-FILE.
           PERFORM START-OWR1-FILE.
           PERFORM M-READ-OWR.

           PERFORM PROCESS-NEXT-WR
              UNTIL WR-EOF-SW = "Y" AND OWR-EOF-SW = "Y".
 
           PERFORM CLOSE-WR1-FILE.
           PERFORM CLOSE-OWR1-FILE.

           GO TO PROCESS-DRIVER-END.

       PROCESS-NEXT-WR.

           PERFORM SEND-PROGRESS-INDICATOR.

           PERFORM COMPARE-FIELDS.

           IF OWR1-KEY < WR1-KEY
              PERFORM M-READ-OWR 
           ELSE
              IF OWR1-KEY > WR1-KEY
                 PERFORM M-READ-WR1
              ELSE
                 PERFORM M-READ-OWR
                 PERFORM M-READ-WR1.

       PROCESS-DRIVER-END.
           EXIT. 

       M-READ-WR1 SECTION.
           PERFORM READ-WR1-FILE-NEXT.
           IF IO-BAD
              MOVE ALL '9'  TO WR1-KEY
              MOVE "Y"      TO WR-EOF-SW
           ELSE
              ADD 1 TO SQL-RECORDS-PROCESSED.

       EXIT-M-READ-WR1.
           EXIT.

       M-READ-OWR SECTION.
           PERFORM READ-OWR1-FILE-NEXT.
           IF IO-BAD OR OWR-YYYYMM > END-BY-DATE-CCYYMM
              MOVE ALL '9'  TO OWR1-KEY
              MOVE "Y"      TO OWR-EOF-SW 
              GO TO EXIT-M-READ-OWR.

           IF OWR-BRNO NOT NUMERIC OR OWR-YYYYMM NOT NUMERIC
              MOVE "OWR1-KEY INVALID  " TO D01-TEXT
              MOVE OWR-BRNO             TO D01-BRNO
              MOVE OWR-YYYYMM           TO D01-DATE
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO M-READ-OWR.
               
              ADD 1 TO A15-RECORDS-PROCESSED.

       EXIT-M-READ-OWR.
           EXIT.

      ******************************************************************
       COMPARE-FIELDS SECTION.
      ******************************************************************

           IF OWR1-KEY < WR1-KEY
              MOVE "MISSING S35 RECORD" TO D01-TEXT
              MOVE OWR-BRNO             TO D01-BRNO
              MOVE OWR-YYYYMM           TO D01-DATE
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.

           IF OWR1-KEY > WR1-KEY
              MOVE "MISSING A15 RECORD" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.

           IF OWR-CUR-NO-CUST NOT NUMERIC
              MOVE 0                TO OWR-CUR-NO-CUST.
           IF WR-CUR-NO-CUST  NOT = OWR-CUR-NO-CUST
              MOVE "WR-CUR-NO-CUST" TO D01-TEXT
              MOVE OWR-CUR-NO-CUST  TO D01-A15-NUM
              MOVE WR-CUR-NO-CUST   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-ELM-NO-CUST NOT NUMERIC
              MOVE 0                TO OWR-ELM-NO-CUST.
           IF WR-ELM-NO-CUST  NOT = OWR-ELM-NO-CUST
              MOVE "WR-ELM-NO-CUST" TO D01-TEXT
              MOVE OWR-ELM-NO-CUST  TO D01-A15-NUM
              MOVE WR-ELM-NO-CUST   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-ELY-NO-CUST NOT NUMERIC
              MOVE 0                TO OWR-ELY-NO-CUST.
           IF WR-ELY-NO-CUST  NOT = OWR-ELY-NO-CUST
              MOVE "WR-ELY-NO-CUST" TO D01-TEXT
              MOVE OWR-ELY-NO-CUST  TO D01-A15
              MOVE WR-ELY-NO-CUST   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-OUTS-BALANCE NOT NUMERIC
              MOVE 0                 TO OWR-OUTS-BALANCE.
           IF WR-OUTS-BALANCE  NOT = WR-OUTS-BALANCE
              MOVE "WR-OUTS-BALANCE" TO D01-TEXT
              MOVE OWR-OUTS-BALANCE  TO D01-A15-NUM
              MOVE WR-OUTS-BALANCE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-ELM-OUTS-BALANCE NOT NUMERIC
              MOVE 0                 TO OWR-ELM-OUTS-BALANCE.
           IF WR-ELM-OUTS-BALANCE NOT = OWR-ELM-OUTS-BALANCE
              MOVE "WR-ELM-OUTS-BALANCE" TO D01-TEXT
              MOVE OWR-ELM-OUTS-BALANCE  TO D01-A15-NUM
              MOVE WR-ELM-OUTS-BALANCE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-ELY-OUTS-BALANCE NOT NUMERIC
              MOVE 0                     TO OWR-ELY-OUTS-BALANCE.
           IF WR-ELY-OUTS-BALANCE  NOT = OWR-ELY-OUTS-BALANCE
              MOVE "WR-ELY-OUTS-BALANCE" TO D01-TEXT
              MOVE OWR-ELY-OUTS-BALANCE  TO D01-A15-NUM
              MOVE WR-ELY-OUTS-BALANCE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 2
              IF OWR-NO-NB(SUB1)      NOT NUMERIC
                 MOVE 0                TO OWR-NO-NB(SUB1) 
              END-IF
              IF WR-NO-NB(SUB1)       NOT = OWR-NO-NB(SUB1)
                 STRING "WR-NO-NB(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-NO-NB(SUB1)  TO D01-A15 
                 MOVE WR-NO-NB(SUB1)   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-NO-FB(SUB1)      NOT NUMERIC
                 MOVE 0                TO OWR-NO-FB(SUB1) 
              END-IF
              IF WR-NO-FB(SUB1) NOT = OWR-NO-FB(SUB1)
                 STRING "WR-NO-FB(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-NO-FB(SUB1)  TO D01-A15 
                 MOVE WR-NO-FB(SUB1)   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-NO-PB(SUB1)      NOT NUMERIC
                 MOVE 0                TO OWR-NO-PB(SUB1) 
              END-IF
              IF WR-NO-PB(SUB1) NOT = OWR-NO-PB(SUB1)
                 STRING "WR-NO-PB(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-NO-PB(SUB1)  TO D01-A15 
                 MOVE WR-NO-PB(SUB1)   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-GROSS-VOLUME(SUB1)      NOT NUMERIC
                 MOVE 0                       TO OWR-GROSS-VOLUME(SUB1) 
              END-IF
              IF WR-GROSS-VOLUME(SUB1) NOT = OWR-GROSS-VOLUME(SUB1)
                 STRING "WR-GROSS-VOLUMEB(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-GROSS-VOLUME(SUB1)  TO D01-A15-NUM
                 MOVE WR-GROSS-VOLUME(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-BSR-BONUS(SUB1) NOT NUMERIC
                 MOVE 0                   TO OWR-BSR-BONUS(SUB1) 
              END-IF
              IF WR-BSR-BONUS(SUB1)  NOT = OWR-BSR-BONUS(SUB1)
                 STRING "WR-BSR-BONUSB(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-BSR-BONUS(SUB1) TO D01-A15-NUM
                 MOVE WR-BSR-BONUS(SUB1)  TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-INTEREST-ALLOCATION(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-INTEREST-ALLOCATION(SUB1) 
              END-IF
              IF WR-INTEREST-ALLOCATION(SUB1) NOT = 
                 OWR-INTEREST-ALLOCATION(SUB1)
                 STRING "WR-INTEREST-ALLOCATION(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-INTEREST-ALLOCATION(SUB1)  TO D01-A15-NUM
                 MOVE WR-INTEREST-ALLOCATION(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

               IF OWR-OLD-SUPERVISION(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-OLD-SUPERVISION(SUB1)
              END-IF
              IF WR-OLD-SUPERVISION(SUB1) NOT = 
                 OWR-OLD-SUPERVISION(SUB1)
                 STRING "WR-OLD-SUPERVISION(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-OLD-SUPERVISION(SUB1)  TO D01-A15-NUM
                 MOVE WR-OLD-SUPERVISION(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-GEN-RESERVE-ADJ(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-GEN-RESERVE-ADJ(SUB1)
              END-IF
              IF WR-GEN-RESERVE-ADJ(SUB1) NOT = 
                 OWR-GEN-RESERVE-ADJ(SUB1)
                 STRING "WR-GEN-RESERVE-ADJ(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-GEN-RESERVE-ADJ(SUB1)  TO D01-A15-NUM
                 MOVE WR-GEN-RESERVE-ADJ(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-INTANGIBLE-AMORT(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-INTANGIBLE-AMORT(SUB1)
              END-IF
              IF WR-INTANGIBLE-AMORT(SUB1) NOT = 
                OWR-INTANGIBLE-AMORT(SUB1)
                 STRING "WR-INTANGIBLE-AMORT(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-INTANGIBLE-AMORT(SUB1)  TO D01-A15-NUM
                 MOVE WR-INTANGIBLE-AMORT(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-ROBBERIES(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-ROBBERIES(SUB1)
              END-IF
              IF WR-ROBBERIES(SUB1) NOT = OWR-ROBBERIES(SUB1)
                 STRING "WR-ROBBERIES(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-ROBBERIES(SUB1)  TO D01-A15-NUM
                 MOVE WR-ROBBERIES(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-RATING-SHEET-INCOME(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-RATING-SHEET-INCOME(SUB1)
              END-IF
              IF WR-RATING-SHEET-INCOME(SUB1) NOT = 
                OWR-RATING-SHEET-INCOME(SUB1)
                 STRING "WR-RATING-SHEET-INCOME(",SUB1,")" 
                    INTO D01-TEXT
                 MOVE OWR-RATING-SHEET-INCOME(SUB1)  TO D01-A15-NUM
                 MOVE WR-RATING-SHEET-INCOME(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-INT-FEE-INCOME(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-INT-FEE-INCOME(SUB1)
              END-IF
              IF WR-INT-FEE-INCOME(SUB1) NOT = OWR-INT-FEE-INCOME(SUB1)
                 STRING "WR-INT-FEE-INCOME(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-INT-FEE-INCOME(SUB1)  TO D01-A15-NUM
                 MOVE WR-INT-FEE-INCOME(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-INSURANCE-INCOME(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-INSURANCE-INCOME(SUB1)
              END-IF
              IF WR-INSURANCE-INCOME(SUB1) NOT = 
                 OWR-INSURANCE-INCOME(SUB1)
                 STRING "WR-INSURANCE-INCOME(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-INSURANCE-INCOME(SUB1)  TO D01-A15-NUM
                 MOVE WR-INSURANCE-INCOME(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-RATING-SHEET-EXPENSES(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-RATING-SHEET-EXPENSES(SUB1)
              END-IF
              IF WR-RATING-SHEET-EXPENSES(SUB1) NOT = 
                 OWR-RATING-SHEET-EXPENSES(SUB1)
                 STRING "WR-RATING-SHEET-EXPENSES(",SUB1,")" 
                   INTO D01-TEXT
                 MOVE OWR-RATING-SHEET-EXPENSES(SUB1)  TO D01-A15-NUM
                 MOVE WR-RATING-SHEET-EXPENSES(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-PERSONNEL-EXPENSE(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-PERSONNEL-EXPENSE(SUB1)
              END-IF
              IF WR-PERSONNEL-EXPENSE(SUB1) NOT = 
                 OWR-PERSONNEL-EXPENSE(SUB1)
                 STRING "WR-PERSONNEL-EXPENSE(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-PERSONNEL-EXPENSE(SUB1)  TO D01-A15-NUM
                 MOVE WR-PERSONNEL-EXPENSE(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-CHARGEOFF-EXPENSE(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-CHARGEOFF-EXPENSE(SUB1)
              END-IF
              IF WR-CHARGEOFF-EXPENSE(SUB1) NOT = 
                 OWR-CHARGEOFF-EXPENSE(SUB1)
                 STRING "WR-CHARGEOFF-EXPENSE(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-CHARGEOFF-EXPENSE(SUB1)  TO D01-A15-NUM
                 MOVE WR-CHARGEOFF-EXPENSE(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-NO-CHARGEOFFS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-NO-CHARGEOFFS(SUB1)
              END-IF
              IF WR-NO-CHARGEOFFS(SUB1) NOT = OWR-NO-CHARGEOFFS(SUB1)
                 STRING "WR-NO-CHARGEOFFS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-NO-CHARGEOFFS(SUB1)  TO D01-A15 
                 MOVE WR-NO-CHARGEOFFS(SUB1)   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-RECOVERY-INCOME(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-RECOVERY-INCOME(SUB1)
              END-IF
              IF WR-RECOVERY-INCOME(SUB1) NOT = 
                 OWR-RECOVERY-INCOME(SUB1)
                 STRING "WR-RECOVERY-INCOME(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-RECOVERY-INCOME(SUB1)  TO D01-A15-NUM
                 MOVE WR-RECOVERY-INCOME(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-BAD-DEBT-INCREASE(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-BAD-DEBT-INCREASE(SUB1)
              END-IF
              IF WR-BAD-DEBT-INCREASE(SUB1) NOT = 
                 OWR-BAD-DEBT-INCREASE(SUB1)
                 STRING "WR-BAD-DEBT-INCREASE(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-BAD-DEBT-INCREASE(SUB1)  TO D01-A15-NUM
                 MOVE WR-BAD-DEBT-INCREASE(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

           END-PERFORM.

           IF OWR-NO-30-DAY-ACCTS NOT NUMERIC
              MOVE 0                    TO OWR-NO-30-DAY-ACCTS.
           IF WR-NO-30-DAY-ACCTS NOT = OWR-NO-30-DAY-ACCTS
              MOVE "WR-NO-30-DAY-ACCTS" TO D01-TEXT
              MOVE OWR-NO-30-DAY-ACCTS  TO D01-A15
              MOVE WR-NO-30-DAY-ACCTS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-NO-30-DAY-ACCTS NOT NUMERIC
              MOVE 0                     TO OWR-NO-30-DAY-ACCTS.
           IF WR-AMT-30-DAY-ACCTS NOT = OWR-AMT-30-DAY-ACCTS
              MOVE "WR-AMT-30-DAY-ACCTS" TO D01-TEXT
              MOVE OWR-AMT-30-DAY-ACCTS  TO D01-A15-NUM
              MOVE WR-AMT-30-DAY-ACCTS   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-NO-60-DAY-ACCTS NOT NUMERIC
              MOVE 0                    TO OWR-NO-60-DAY-ACCTS.
           IF WR-NO-60-DAY-ACCTS NOT = OWR-NO-60-DAY-ACCTS
              MOVE "WR-NO-60-DAY-ACCTS" TO D01-TEXT
              MOVE OWR-NO-60-DAY-ACCTS  TO D01-A15
              MOVE WR-NO-60-DAY-ACCTS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-AMT-60-DAY-ACCTS NOT NUMERIC
              MOVE 0                     TO OWR-AMT-60-DAY-ACCTS.
           IF WR-AMT-60-DAY-ACCTS NOT = OWR-AMT-60-DAY-ACCTS
              MOVE "WR-AMT-60-DAY-ACCTS" TO D01-TEXT
              MOVE OWR-AMT-60-DAY-ACCTS  TO D01-A15-NUM
              MOVE WR-AMT-60-DAY-ACCTS   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-NO-90-DAY-ACCTS NOT NUMERIC
              MOVE 0                    TO OWR-NO-90-DAY-ACCTS.
           IF WR-NO-90-DAY-ACCTS NOT = OWR-NO-90-DAY-ACCTS
              MOVE "WR-NO-90-DAY-ACCTS" TO D01-TEXT
              MOVE OWR-NO-90-DAY-ACCTS  TO D01-A15
              MOVE WR-NO-90-DAY-ACCTS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-AMT-90-DAY-ACCTS NOT NUMERIC
              MOVE 0                     TO OWR-AMT-90-DAY-ACCTS.
           IF WR-AMT-90-DAY-ACCTS NOT = OWR-AMT-90-DAY-ACCTS
              MOVE "WR-AMT-90-DAY-ACCTS" TO D01-TEXT
              MOVE OWR-AMT-90-DAY-ACCTS  TO D01-A15-NUM
              MOVE WR-AMT-90-DAY-ACCTS   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-ELM-AMT-90-DAY-ACCTS NOT NUMERIC
              MOVE 0 TO OWR-ELM-AMT-90-DAY-ACCTS.
           IF WR-ELM-AMT-90-DAY-ACCTS NOT = OWR-ELM-AMT-90-DAY-ACCTS
              MOVE "WR-ELM-AMT-90-DAY-ACCTS" TO D01-TEXT
              MOVE OWR-ELM-AMT-90-DAY-ACCTS  TO D01-A15-NUM
              MOVE WR-ELM-AMT-90-DAY-ACCTS   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-AMT-30-DAY-ACCTS NOT NUMERIC
              MOVE 0 TO OWR-AMT-30-DAY-ACCTS.
           IF WR-AMT-30-DAY-ACCTS NOT = OWR-AMT-30-DAY-ACCTS
              MOVE "WR-AMT-30-DAY-ACCTS" TO D01-TEXT
              MOVE OWR-AMT-30-DAY-ACCTS  TO D01-A15-NUM
              MOVE WR-AMT-30-DAY-ACCTS   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-YTD-PCT-TURNOVER NOT NUMERIC
              MOVE 0 TO OWR-YTD-PCT-TURNOVER.
           IF WR-YTD-PCT-TURNOVER NOT = OWR-YTD-PCT-TURNOVER
              MOVE "WR-YTD-PCT-TURNOVER" TO D01-TEXT
              MOVE OWR-YTD-PCT-TURNOVER  TO D01-A15-NUM
              MOVE WR-YTD-PCT-TURNOVER   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-YTD-PCT-RATE-SHT-PROFIT NOT NUMERIC
              MOVE 0 TO OWR-YTD-PCT-RATE-SHT-PROFIT.
           IF WR-YTD-PCT-RATE-SHT-PROFIT NOT = 
              OWR-YTD-PCT-RATE-SHT-PROFIT
              MOVE "WR-YTD-PCT-RATE-SHT-PROFIT" TO D01-TEXT
              MOVE OWR-YTD-PCT-RATE-SHT-PROFIT  TO D01-A15-NUM
              MOVE WR-YTD-PCT-RATE-SHT-PROFIT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-YTD-PCT-INT-FEE-LEDGER NOT NUMERIC
              MOVE 0 TO OWR-YTD-PCT-INT-FEE-LEDGER.
           IF WR-YTD-PCT-INT-FEE-LEDGER NOT = 
              OWR-YTD-PCT-INT-FEE-LEDGER
              MOVE "WR-YTD-PCT-INT-FEE-LEDGER" TO D01-TEXT
              MOVE OWR-YTD-PCT-INT-FEE-LEDGER  TO D01-A15-NUM
              MOVE WR-YTD-PCT-INT-FEE-LEDGER   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-YTD-PCT-INS-INC-LEDGER NOT NUMERIC
              MOVE 0 TO OWR-YTD-PCT-INS-INC-LEDGER.
           IF WR-YTD-PCT-INS-INC-LEDGER NOT = OWR-YTD-PCT-INS-INC-LEDGER
              MOVE "WR-YTD-PCT-INS-INC-LEDGER" TO D01-TEXT
              MOVE OWR-YTD-PCT-INS-INC-LEDGER  TO D01-A15-NUM
              MOVE WR-YTD-PCT-INS-INC-LEDGER   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-YTD-PCT-EXPENSE-LEDGER NOT NUMERIC
              MOVE 0 TO OWR-YTD-PCT-EXPENSE-LEDGER.
           IF WR-YTD-PCT-EXPENSE-LEDGER NOT = OWR-YTD-PCT-EXPENSE-LEDGER
              MOVE "WR-YTD-PCT-EXPENSE-LEDGER" TO D01-TEXT
              MOVE OWR-YTD-PCT-EXPENSE-LEDGER  TO D01-A15-NUM
              MOVE WR-YTD-PCT-EXPENSE-LEDGER   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-YTD-PCT-BAD-DEBT-LEDGER NOT NUMERIC
              MOVE 0 TO OWR-YTD-PCT-BAD-DEBT-LEDGER.
           IF WR-YTD-PCT-BAD-DEBT-LEDGER NOT = 
              OWR-YTD-PCT-BAD-DEBT-LEDGER
              MOVE "WR-YTD-PCT-BAD-DEBT-LEDGER" TO D01-TEXT
              MOVE OWR-YTD-PCT-BAD-DEBT-LEDGER  TO D01-A15-NUM
              MOVE WR-YTD-PCT-BAD-DEBT-LEDGER   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-YTD-PCT-NF-BNK-LEDGER NOT NUMERIC
              MOVE 0 TO OWR-YTD-PCT-NF-BNK-LEDGER.
           IF WR-YTD-PCT-NF-BNK-LEDGER NOT = OWR-YTD-PCT-NF-BNK-LEDGER
              MOVE "WR-YTD-PCT-NF-BNK-LEDGER" TO D01-TEXT
              MOVE OWR-YTD-PCT-NF-BNK-LEDGER  TO D01-A15-NUM
              MOVE WR-YTD-PCT-NF-BNK-LEDGER   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-YTD-PCT-PRETAX-LEDGER NOT NUMERIC
              MOVE 0 TO OWR-YTD-PCT-PRETAX-LEDGER.
           IF WR-YTD-PCT-PRETAX-LEDGER NOT = OWR-YTD-PCT-PRETAX-LEDGER
              MOVE "WR-YTD-PCT-PRETAX-LEDGER" TO D01-TEXT
              MOVE OWR-YTD-PCT-PRETAX-LEDGER  TO D01-A15-NUM
              MOVE WR-YTD-PCT-PRETAX-LEDGER   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-YTD-PCT-DEBT-NF-LEDGER NOT NUMERIC
              MOVE 0 TO OWR-YTD-PCT-DEBT-NF-LEDGER.
           IF WR-YTD-PCT-DEBT-NF-LEDGER NOT = OWR-YTD-PCT-DEBT-NF-LEDGER
              MOVE "WR-YTD-PCT-DEBT-NF-LEDGER" TO D01-TEXT
              MOVE OWR-YTD-PCT-DEBT-NF-LEDGER  TO D01-A15-NUM
              MOVE WR-YTD-PCT-DEBT-NF-LEDGER   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-YTD-PCT-DEBT-NF-LEDGER NOT NUMERIC
              MOVE 0 TO OWR-YTD-PCT-DEBT-NF-LEDGER.
           IF WR-WCBC-CUR-NO-CUST NOT = OWR-WCBC-CUR-NO-CUST
              MOVE "WR-WCBC-CUR-NO-CUST" TO D01-TEXT
              MOVE OWR-WCBC-CUR-NO-CUST  TO D01-A15 
              MOVE WR-WCBC-CUR-NO-CUST   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-WCBC-ELM-NO-CUST NOT NUMERIC
              MOVE 0 TO OWR-WCBC-ELM-NO-CUST.
           IF WR-WCBC-ELM-NO-CUST NOT = OWR-WCBC-ELM-NO-CUST
              MOVE "WR-WCBC-ELM-NO-CUST" TO D01-TEXT
              MOVE OWR-WCBC-ELM-NO-CUST  TO D01-A15 
              MOVE WR-WCBC-ELM-NO-CUST   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-WCBC-ELY-NO-CUST NOT NUMERIC
              MOVE 0 TO OWR-WCBC-ELY-NO-CUST.
           IF WR-WCBC-ELY-NO-CUST NOT = OWR-WCBC-ELY-NO-CUST
              MOVE "WR-WCBC-ELY-NO-CUST" TO D01-TEXT
              MOVE OWR-WCBC-ELY-NO-CUST  TO D01-A15 
              MOVE WR-WCBC-ELY-NO-CUST   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-WCBC-OUTS-BALANCE NOT NUMERIC
              MOVE 0 TO OWR-WCBC-OUTS-BALANCE.
           IF WR-WCBC-OUTS-BALANCE NOT = OWR-WCBC-OUTS-BALANCE
              MOVE "WR-WCBC-OUTS-BALANCET" TO D01-TEXT
              MOVE OWR-WCBC-OUTS-BALANCE   TO D01-A15-NUM
              MOVE WR-WCBC-OUTS-BALANCE    TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-WCBC-ELM-OUTS-BAL NOT NUMERIC
              MOVE 0 TO OWR-WCBC-ELM-OUTS-BAL.
           IF WR-WCBC-ELM-OUTS-BAL NOT = OWR-WCBC-ELM-OUTS-BAL
              MOVE "WR-WCBC-ELM-OUTS-BALT" TO D01-TEXT
              MOVE OWR-WCBC-ELM-OUTS-BAL   TO D01-A15-NUM
              MOVE WR-WCBC-ELM-OUTS-BAL    TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-WCBC-ELY-OUTS-BAL NOT NUMERIC
              MOVE 0 TO OWR-WCBC-ELY-OUTS-BAL.
           IF WR-WCBC-ELY-OUTS-BAL NOT = OWR-WCBC-ELY-OUTS-BAL
              MOVE "WR-WCBC-ELY-OUTS-BAL" TO D01-TEXT
              MOVE OWR-WCBC-ELY-OUTS-BAL  TO D01-A15-NUM
              MOVE WR-WCBC-ELY-OUTS-BAL   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.


           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 2
              IF OWR-WCBC-NO-NB(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-WCBC-NO-NB(SUB1)
              END-IF
              IF WR-WCBC-NO-NB(SUB1) NOT = OWR-WCBC-NO-NB(SUB1)
                 STRING "WR-WCBC-NO-NB(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-WCBC-NO-NB(SUB1)  TO D01-A15 
                 MOVE WR-WCBC-NO-NB(SUB1)   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-WCBC-NO-FB(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-WCBC-NO-FB(SUB1)
              END-IF
              IF WR-WCBC-NO-FB(SUB1) NOT = OWR-WCBC-NO-FB(SUB1)
                 STRING "WR-WCBC-NO-FB(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-WCBC-NO-FB(SUB1)  TO D01-A15 
                 MOVE WR-WCBC-NO-FB(SUB1)   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-WCBC-NO-PB(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-WCBC-NO-PB(SUB1)
              END-IF
              IF WR-WCBC-NO-PB(SUB1) NOT = OWR-WCBC-NO-PB(SUB1)
                 STRING "WR-WCBC-NO-PB(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-WCBC-NO-PB(SUB1)  TO D01-A15 
                 MOVE WR-WCBC-NO-PB(SUB1)   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-WCBC-GROSS-VOLUME(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-WCBC-GROSS-VOLUME(SUB1)
              END-IF
              IF WR-WCBC-GROSS-VOLUME(SUB1) NOT = 
                 OWR-WCBC-GROSS-VOLUME(SUB1)
                 STRING "WR-WCBC-GROSS-VOLUME(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-WCBC-GROSS-VOLUME(SUB1)  TO D01-A15-NUM
                 MOVE WR-WCBC-GROSS-VOLUME(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-WCBC-GROSS-VOLUME(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-WCBC-GROSS-VOLUME(SUB1)
              END-IF
              IF WR-WCBC-GROSS-VOLUME(SUB1) NOT = 
                OWR-WCBC-GROSS-VOLUME(SUB1)
                 STRING "WR-WCBC-GROSS-VOLUME(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-WCBC-GROSS-VOLUME(SUB1)  TO D01-A15-NUM
                 MOVE WR-WCBC-GROSS-VOLUME(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-WCBC-LATE-CHGS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-WCBC-LATE-CHGS(SUB1)
              END-IF
              IF WR-WCBC-LATE-CHGS(SUB1) NOT = 
                 OWR-WCBC-LATE-CHGS(SUB1)
                 STRING "WR-WCBC-LATE-CHGS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-WCBC-LATE-CHGS(SUB1)  TO D01-A15-NUM
                 MOVE WR-WCBC-LATE-CHGS(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-WCBC-NET-FIN-CHGS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-WCBC-NET-FIN-CHGS(SUB1)
              END-IF
              IF WR-WCBC-NET-FIN-CHGS(SUB1) NOT = 
                 OWR-WCBC-NET-FIN-CHGS(SUB1)
                 STRING "WR-WCBC-NET-FIN-CHGS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-WCBC-NET-FIN-CHGS(SUB1)  TO D01-A15-NUM
                 MOVE WR-WCBC-NET-FIN-CHGS(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-WCBC-SALES(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-WCBC-SALES(SUB1)
              END-IF
              IF WR-WCBC-SALES(SUB1) NOT = OWR-WCBC-SALES(SUB1)
                 STRING "WR-WCBC-SALES(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-WCBC-SALES(SUB1)  TO D01-A15-NUM
                 MOVE WR-WCBC-SALES(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-WCBC-COST-OF-SALES(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-WCBC-COST-OF-SALES(SUB1)
              END-IF
              IF WR-WCBC-COST-OF-SALES(SUB1) NOT = 
                OWR-WCBC-COST-OF-SALES(SUB1)
                 STRING "WR-WCBC-COST-OF-SALES(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-WCBC-COST-OF-SALES(SUB1)  TO D01-A15-NUM
                 MOVE WR-WCBC-COST-OF-SALES(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-WCBC-EXPENSES(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-WCBC-EXPENSES(SUB1)
              END-IF
              IF WR-WCBC-EXPENSES(SUB1) NOT = OWR-WCBC-EXPENSES(SUB1)
                 STRING "WR-WCBC-EXPENSES(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-WCBC-EXPENSES(SUB1)  TO D01-A15-NUM
                 MOVE WR-WCBC-EXPENSES(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-WCBC-RECOVERY-INCOME(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-WCBC-RECOVERY-INCOME(SUB1)
              END-IF
              IF WR-WCBC-RECOVERY-INCOME(SUB1) NOT = 
                 OWR-WCBC-RECOVERY-INCOME(SUB1)
                 STRING "WR-WCBC-RECOVERY-INCOME(",SUB1,")" 
                   INTO D01-TEXT
                 MOVE OWR-WCBC-RECOVERY-INCOME(SUB1)  TO D01-A15-NUM
                 MOVE WR-WCBC-RECOVERY-INCOME(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-WCBC-CHARGEOFF-EXPENSE(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-WCBC-CHARGEOFF-EXPENSE(SUB1)
              END-IF
              IF WR-WCBC-CHARGEOFF-EXPENSE(SUB1) NOT = 
                 OWR-WCBC-CHARGEOFF-EXPENSE(SUB1)
                 STRING "WR-WCBC-CHARGEOFF-EXPENSE(",SUB1,")" 
                    INTO D01-TEXT
                 MOVE OWR-WCBC-CHARGEOFF-EXPENSE(SUB1)  TO D01-A15-NUM
                 MOVE WR-WCBC-CHARGEOFF-EXPENSE(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-WCBC-BAD-DEBT-BNKRPCY(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-WCBC-BAD-DEBT-BNKRPCY(SUB1)
              END-IF
              IF WR-WCBC-BAD-DEBT-BNKRPCY(SUB1) NOT = 
                 OWR-WCBC-BAD-DEBT-BNKRPCY(SUB1)
                 STRING "WR-WCBC-BAD-DEBT-BNKRPCY(",SUB1,")" 
                    INTO D01-TEXT
                 MOVE OWR-WCBC-BAD-DEBT-BNKRPCY(SUB1)  TO D01-A15-NUM
                 MOVE WR-WCBC-BAD-DEBT-BNKRPCY(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.   

           IF OWR-WCBC-NO-30-DAY-ACCTS NOT NUMERIC
              MOVE 0 TO OWR-WCBC-NO-30-DAY-ACCTS.
           IF WR-WCBC-NO-30-DAY-ACCTS NOT = OWR-WCBC-NO-30-DAY-ACCTS
              MOVE "WR-WCBC-NO-30-DAY-ACCTS" TO D01-TEXT
              MOVE OWR-WCBC-NO-30-DAY-ACCTS  TO D01-A15
              MOVE WR-WCBC-NO-30-DAY-ACCTS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-WCBC-AMT-30-DAY-ACCTS NOT NUMERIC
              MOVE 0 TO OWR-WCBC-AMT-30-DAY-ACCTS.
           IF WR-WCBC-AMT-30-DAY-ACCTS NOT = OWR-WCBC-AMT-30-DAY-ACCTS
              MOVE "WR-WCBC-AMT-30-DAY-ACCTS" TO D01-TEXT
              MOVE OWR-WCBC-AMT-30-DAY-ACCTS  TO D01-A15-NUM
              MOVE WR-WCBC-AMT-30-DAY-ACCTS  TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-WCBC-NO-60-DAY-ACCTS NOT NUMERIC
              MOVE 0 TO OWR-WCBC-NO-60-DAY-ACCTS.
           IF WR-WCBC-NO-60-DAY-ACCTS NOT = OWR-WCBC-NO-60-DAY-ACCTS
              MOVE "WR-WCBC-NO-60-DAY-ACCTS" TO D01-TEXT
              MOVE OWR-WCBC-NO-60-DAY-ACCTS  TO D01-A15
              MOVE WR-WCBC-NO-60-DAY-ACCTS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-WCBC-AMT-60-DAY-ACCTS NOT NUMERIC
              MOVE 0 TO OWR-WCBC-AMT-60-DAY-ACCTS.
           IF WR-WCBC-AMT-60-DAY-ACCTS NOT = OWR-WCBC-AMT-60-DAY-ACCTS
              MOVE "WR-WCBC-AMT-60-DAY-ACCTS" TO D01-TEXT
              MOVE OWR-WCBC-AMT-60-DAY-ACCTS  TO D01-A15-NUM
              MOVE WR-WCBC-AMT-60-DAY-ACCTS   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-WCBC-NO-90-DAY-ACCTS NOT NUMERIC
              MOVE 0 TO OWR-WCBC-NO-90-DAY-ACCTS.
           IF WR-WCBC-NO-90-DAY-ACCTS NOT = OWR-WCBC-NO-90-DAY-ACCTS
              MOVE "WR-WCBC-NO-90-DAY-ACCTS" TO D01-TEXT
              MOVE OWR-WCBC-NO-90-DAY-ACCTS  TO D01-A15
              MOVE WR-WCBC-NO-90-DAY-ACCTS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-WCBC-AMT-90-DAY-ACCTS NOT NUMERIC
              MOVE 0 TO OWR-WCBC-AMT-90-DAY-ACCTS.
           IF WR-WCBC-AMT-90-DAY-ACCTS NOT = OWR-WCBC-AMT-90-DAY-ACCTS
              MOVE "WR-WCBC-AMT-90-DAY-ACCTS" TO D01-TEXT
              MOVE OWR-WCBC-AMT-90-DAY-ACCTS  TO D01-A15-NUM
              MOVE WR-WCBC-AMT-90-DAY-ACCTS   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-WCBC-ELM-AMT-90-DAYS NOT NUMERIC
              MOVE 0 TO OWR-WCBC-ELM-AMT-90-DAYS.
           IF WR-WCBC-ELM-AMT-90-DAYS NOT = OWR-WCBC-ELM-AMT-90-DAYS
              MOVE "WR-WCBC-ELM-AMT-90-DAYS" TO D01-TEXT
              MOVE OWR-WCBC-ELM-AMT-90-DAYS  TO D01-A15-NUM
              MOVE WR-WCBC-ELM-AMT-90-DAYS   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-WCBC-ELY-AMT-90-DAYS NOT NUMERIC
              MOVE 0 TO OWR-WCBC-ELY-AMT-90-DAYS.
           IF WR-WCBC-ELY-AMT-90-DAYS NOT = OWR-WCBC-ELY-AMT-90-DAYS
              MOVE "WR-WCBC-ELY-AMT-90-DAYS" TO D01-TEXT
              MOVE OWR-WCBC-ELY-AMT-90-DAYS  TO D01-A15-NUM
              MOVE WR-WCBC-ELY-AMT-90-DAYS   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-WCBC-PCT-VOLUME NOT NUMERIC
              MOVE 0 TO OWR-WCBC-PCT-VOLUME.
           IF WR-WCBC-PCT-VOLUME NOT = OWR-WCBC-PCT-VOLUME
              MOVE "WR-WCBC-PCT-VOLUME" TO D01-TEXT
              MOVE OWR-WCBC-PCT-VOLUME  TO D01-A15-NUM
              MOVE WR-WCBC-PCT-VOLUME   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-WCBC-GROSS-PROF-PCT NOT NUMERIC
              MOVE 0 TO OWR-WCBC-GROSS-PROF-PCT.
           IF WR-WCBC-GROSS-PROF-PCT NOT = OWR-WCBC-GROSS-PROF-PCT
              MOVE "WR-WCBC-GROSS-PROF-PCT" TO D01-TEXT
              MOVE OWR-WCBC-GROSS-PROF-PCT  TO D01-A15-NUM
              MOVE WR-WCBC-GROSS-PROF-PCT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-LARGE-CUR-NO-CUST NOT NUMERIC
              MOVE 0 TO OWR-LARGE-CUR-NO-CUST.
           IF WR-LARGE-CUR-NO-CUST NOT = OWR-LARGE-CUR-NO-CUST
              MOVE "WR-LARGE-CUR-NO-CUST" TO D01-TEXT
              MOVE OWR-LARGE-CUR-NO-CUST  TO D01-A15
              MOVE WR-LARGE-CUR-NO-CUST   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-LARGE-ELM-NO-CUST NOT NUMERIC
              MOVE 0 TO OWR-LARGE-ELM-NO-CUST.
           IF WR-LARGE-ELM-NO-CUST NOT = OWR-LARGE-ELM-NO-CUST
              MOVE "WR-LARGE-ELM-NO-CUST" TO D01-TEXT
              MOVE OWR-LARGE-ELM-NO-CUST  TO D01-A15
              MOVE WR-LARGE-ELM-NO-CUST   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-LARGE-ELY-NO-CUST NOT NUMERIC
              MOVE 0 TO OWR-LARGE-ELY-NO-CUST.
           IF WR-LARGE-ELY-NO-CUST NOT = OWR-LARGE-ELY-NO-CUST
              MOVE "WR-LARGE-ELY-NO-CUST" TO D01-TEXT
              MOVE OWR-LARGE-ELY-NO-CUST  TO D01-A15 
              MOVE WR-LARGE-ELY-NO-CUST   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-LARGE-OUTS-BALANCE NOT NUMERIC
              MOVE 0 TO OWR-LARGE-OUTS-BALANCE.
           IF WR-LARGE-OUTS-BALANCE NOT = OWR-LARGE-OUTS-BALANCE
              MOVE "WR-LARGE-OUTS-BALANCE" TO D01-TEXT
              MOVE OWR-LARGE-OUTS-BALANCE  TO D01-A15-NUM
              MOVE WR-LARGE-OUTS-BALANCE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-LARGE-ELM-OUTS-BAL NOT NUMERIC
              MOVE 0 TO OWR-LARGE-ELM-OUTS-BAL.
           IF WR-LARGE-ELM-OUTS-BAL NOT = OWR-LARGE-ELM-OUTS-BAL
              MOVE "WR-LARGE-ELM-OUTS-BAL" TO D01-TEXT
              MOVE OWR-LARGE-ELM-OUTS-BAL  TO D01-A15-NUM
              MOVE WR-LARGE-ELM-OUTS-BAL   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-LARGE-ELY-OUTS-BAL NOT NUMERIC
              MOVE 0 TO OWR-LARGE-ELY-OUTS-BAL.
           IF WR-LARGE-ELY-OUTS-BAL NOT = OWR-LARGE-ELY-OUTS-BAL
              MOVE "WR-LARGE-ELY-OUTS-BAL" TO D01-TEXT
              MOVE OWR-LARGE-ELY-OUTS-BAL  TO D01-A15-NUM
              MOVE WR-LARGE-ELY-OUTS-BAL   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 2
              IF OWR-LARGE-NO-NB(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-LARGE-NO-NB(SUB1)
              END-IF
              IF WR-LARGE-NO-NB(SUB1) NOT = OWR-LARGE-NO-NB(SUB1)
                 STRING "WR-LARGE-NO-NB(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-LARGE-NO-NB(SUB1)  TO D01-A15 
                 MOVE WR-LARGE-NO-NB(SUB1)   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-LARGE-NO-FB(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-LARGE-NO-FB(SUB1)
              END-IF
              IF WR-LARGE-NO-FB(SUB1) NOT = OWR-LARGE-NO-FB(SUB1)
                 STRING "WR-LARGE-NO-FB(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-LARGE-NO-FB(SUB1)  TO D01-A15 
                 MOVE WR-LARGE-NO-FB(SUB1)   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-LARGE-NO-PB(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-LARGE-NO-PB(SUB1)
              END-IF
              IF WR-LARGE-NO-PB(SUB1) NOT = OWR-LARGE-NO-PB(SUB1)
                 STRING "WR-LARGE-NO-PB(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-LARGE-NO-PB(SUB1)  TO D01-A15 
                 MOVE WR-LARGE-NO-PB(SUB1)   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-LARGE-GROSS-VOLUME(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-LARGE-GROSS-VOLUME(SUB1)
              END-IF
              IF WR-LARGE-GROSS-VOLUME(SUB1) NOT = 
                 OWR-LARGE-GROSS-VOLUME(SUB1)
                 STRING "WR-LARGE-GROSS-VOLUME(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-LARGE-GROSS-VOLUME(SUB1)  TO D01-A15-NUM
                 MOVE WR-LARGE-GROSS-VOLUME(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.

           IF OWR-LARGE-NO-30-DAY-ACCTS NOT NUMERIC
              MOVE 0 TO OWR-LARGE-NO-30-DAY-ACCTS.
           IF WR-LARGE-NO-30-DAY-ACCTS NOT = OWR-LARGE-NO-30-DAY-ACCTS
              MOVE "WR-LARGE-NO-30-DAY-ACCTS" TO D01-TEXT
              MOVE OWR-LARGE-NO-30-DAY-ACCTS  TO D01-A15 
              MOVE WR-LARGE-NO-30-DAY-ACCTS   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-LARGE-AMT-30-DAY-ACCTS NOT NUMERIC
              MOVE 0 TO OWR-LARGE-AMT-30-DAY-ACCTS.
           IF WR-LARGE-AMT-30-DAY-ACCTS NOT = OWR-LARGE-AMT-30-DAY-ACCTS
              MOVE "WR-LARGE-AMT-30-DAY-ACCTS" TO D01-TEXT
              MOVE OWR-LARGE-AMT-30-DAY-ACCTS  TO D01-A15-NUM
              MOVE WR-LARGE-AMT-30-DAY-ACCTS   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-LARGE-NO-60-DAY-ACCTS NOT NUMERIC
              MOVE 0 TO OWR-LARGE-NO-60-DAY-ACCTS.
           IF WR-LARGE-NO-60-DAY-ACCTS NOT = OWR-LARGE-NO-60-DAY-ACCTS
              MOVE "WR-LARGE-NO-60-DAY-ACCTS" TO D01-TEXT
              MOVE OWR-LARGE-NO-60-DAY-ACCTS  TO D01-A15 
              MOVE WR-LARGE-NO-60-DAY-ACCTS   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-LARGE-AMT-60-DAY-ACCTS NOT NUMERIC
              MOVE 0 TO OWR-LARGE-AMT-60-DAY-ACCTS.
           IF WR-LARGE-AMT-60-DAY-ACCTS NOT = OWR-LARGE-AMT-60-DAY-ACCTS
              MOVE "WR-LARGE-AMT-60-DAY-ACCTS" TO D01-TEXT
              MOVE OWR-LARGE-AMT-60-DAY-ACCTS  TO D01-A15-NUM
              MOVE WR-LARGE-AMT-60-DAY-ACCTS   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-LARGE-NO-90-DAY-ACCTS NOT NUMERIC
              MOVE 0 TO OWR-LARGE-NO-90-DAY-ACCTS.
           IF WR-LARGE-NO-90-DAY-ACCTS NOT = OWR-LARGE-NO-90-DAY-ACCTS
              MOVE "WR-LARGE-NO-90-DAY-ACCTS" TO D01-TEXT
              MOVE OWR-LARGE-NO-90-DAY-ACCTS  TO D01-A15 
              MOVE WR-LARGE-NO-90-DAY-ACCTS   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-LARGE-AMT-90-DAY-ACCTS NOT NUMERIC
              MOVE 0 TO OWR-LARGE-AMT-90-DAY-ACCTS.
           IF WR-LARGE-AMT-90-DAY-ACCTS NOT = 
              OWR-LARGE-AMT-90-DAY-ACCTS
              MOVE "WR-LARGE-AMT-90-DAY-ACCTS" TO D01-TEXT
              MOVE OWR-LARGE-AMT-90-DAY-ACCTS  TO D01-A15-NUM
              MOVE WR-LARGE-AMT-90-DAY-ACCTS   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-LARGE-ELM-AMT-90-DAYS NOT NUMERIC
              MOVE 0 TO OWR-LARGE-ELM-AMT-90-DAYS.
           IF WR-LARGE-ELM-AMT-90-DAYS NOT = OWR-LARGE-ELM-AMT-90-DAYS
              MOVE "WR-LARGE-ELM-AMT-90-DAYS" TO D01-TEXT
              MOVE OWR-LARGE-ELM-AMT-90-DAYS  TO D01-A15-NUM
              MOVE WR-LARGE-ELM-AMT-90-DAYS   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-LARGE-ELY-AMT-90-DAYS NOT NUMERIC
              MOVE 0 TO OWR-LARGE-ELY-AMT-90-DAYS.
           IF WR-LARGE-ELY-AMT-90-DAYS NOT = OWR-LARGE-ELY-AMT-90-DAYS
              MOVE "WR-LARGE-ELY-AMT-90-DAYS" TO D01-TEXT
              MOVE OWR-LARGE-ELY-AMT-90-DAYS  TO D01-A15-NUM
              MOVE WR-LARGE-ELY-AMT-90-DAYS   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 2

              IF OWR-LIFE-CLAIMS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-LIFE-CLAIMS(SUB1)
              END-IF
              IF WR-LIFE-CLAIMS(SUB1) NOT = OWR-LIFE-CLAIMS(SUB1)
                 STRING "WR-LIFE-CLAIMS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-LIFE-CLAIMS(SUB1)  TO D01-A15-NUM
                 MOVE WR-LIFE-CLAIMS(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-AH-CLAIMS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-AH-CLAIMS(SUB1)
              END-IF
              IF WR-AH-CLAIMS(SUB1) NOT = OWR-AH-CLAIMS(SUB1)
                 STRING "WR-AH-CLAIMS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-AH-CLAIMS(SUB1)  TO D01-A15-NUM
                 MOVE WR-AH-CLAIMS(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-FIRE-CLAIMS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-FIRE-CLAIMS(SUB1)
              END-IF
              IF WR-FIRE-CLAIMS(SUB1) NOT = OWR-FIRE-CLAIMS(SUB1)
                 STRING "WR-FIRE-CLAIMS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-FIRE-CLAIMS(SUB1)  TO D01-A15-NUM
                 MOVE WR-FIRE-CLAIMS(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-AUTO-CLAIMS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-AUTO-CLAIMS(SUB1)
              END-IF
              IF WR-AUTO-CLAIMS(SUB1) NOT = OWR-AUTO-CLAIMS(SUB1)
                 STRING "WR-AUTO-CLAIMS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-AUTO-CLAIMS(SUB1)  TO D01-A15-NUM
                 MOVE WR-AUTO-CLAIMS(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-UNEMP-CLAIMS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-UNEMP-CLAIMS(SUB1)
              END-IF
              IF WR-UNEMP-CLAIMS(SUB1) NOT = OWR-UNEMP-CLAIMS(SUB1)
                 STRING "WR-UNEMP-CLAIMS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-UNEMP-CLAIMS(SUB1)  TO D01-A15-NUM
                 MOVE WR-UNEMP-CLAIMS(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-AD-D-CLAIMS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-AD-D-CLAIMS(SUB1)
              END-IF
              IF WR-AD-D-CLAIMS(SUB1) NOT = OWR-AD-D-CLAIMS(SUB1)
                 STRING "WR-AD-D-CLAIMS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-AD-D-CLAIMS(SUB1)  TO D01-A15-NUM
                 MOVE WR-AD-D-CLAIMS(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-L-PROT-CLAIMS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-L-PROT-CLAIMS(SUB1)
              END-IF
              IF WR-L-PROT-CLAIMS(SUB1) NOT = OWR-L-PROT-CLAIMS(SUB1)
                 STRING "WR-L-PROT-CLAIMS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-L-PROT-CLAIMS(SUB1)  TO D01-A15-NUM
                 MOVE WR-L-PROT-CLAIMS(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-NF-NON-BK-CLAIMS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-NF-NON-BK-CLAIMS(SUB1)
              END-IF
              IF WR-NF-NON-BK-CLAIMS(SUB1) NOT = 
                 OWR-NF-NON-BK-CLAIMS(SUB1)
                 STRING "WR-NF-NON-BK-CLAIMS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-NF-NON-BK-CLAIMS(SUB1)  TO D01-A15-NUM
                 MOVE WR-NF-NON-BK-CLAIMS(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-NF-BK-CLAIMS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-NF-BK-CLAIMS(SUB1)
              END-IF
              IF WR-NF-BK-CLAIMS(SUB1) NOT = OWR-NF-BK-CLAIMS(SUB1)
                 STRING "WR-NF-BK-CLAIMS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-NF-BK-CLAIMS(SUB1)  TO D01-A15-NUM
                 MOVE WR-NF-BK-CLAIMS(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-ROAD-GARD-CLAIMS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-ROAD-GARD-CLAIMS(SUB1)
              END-IF
              IF WR-ROAD-GARD-CLAIMS(SUB1) NOT = 
                 OWR-ROAD-GARD-CLAIMS(SUB1)
                 STRING "WR-ROAD-GARD-CLAIMS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-ROAD-GARD-CLAIMS(SUB1)  TO D01-A15-NUM
                 MOVE WR-ROAD-GARD-CLAIMS(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-NO-LIFE-CLAIMS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-NO-LIFE-CLAIMS(SUB1)
              END-IF
              IF WR-NO-LIFE-CLAIMS(SUB1) NOT = OWR-NO-LIFE-CLAIMS(SUB1)
                 STRING "WR-NO-LIFE-CLAIMS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-NO-LIFE-CLAIMS(SUB1)  TO D01-A15-NUM
                 MOVE WR-NO-LIFE-CLAIMS(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-NO-AH-CLAIMS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-NO-AH-CLAIMS(SUB1)
              END-IF
              IF WR-NO-AH-CLAIMS(SUB1) NOT = OWR-NO-AH-CLAIMS(SUB1)
                 STRING "WR-NO-AH-CLAIMS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-NO-AH-CLAIMS(SUB1)  TO D01-A15-NUM
                 MOVE WR-NO-AH-CLAIMS(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-NO-FIRE-CLAIMS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-NO-FIRE-CLAIMS(SUB1)
              END-IF
              IF WR-NO-FIRE-CLAIMS(SUB1) NOT = OWR-NO-FIRE-CLAIMS(SUB1)
                 STRING "WR-NO-FIRE-CLAIMS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-NO-FIRE-CLAIMS(SUB1)  TO D01-A15-NUM
                 MOVE WR-NO-FIRE-CLAIMS(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-NO-AUTO-CLAIMS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-NO-AUTO-CLAIMS(SUB1)
              END-IF
              IF WR-NO-AUTO-CLAIMS(SUB1) NOT = OWR-NO-AUTO-CLAIMS(SUB1)
                 STRING "WR-NO-AUTO-CLAIMS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-NO-AUTO-CLAIMS(SUB1)  TO D01-A15-NUM
                 MOVE WR-NO-AUTO-CLAIMS(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-NO-UNEMP-CLAIMS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-NO-UNEMP-CLAIMS(SUB1)
              END-IF
              IF WR-NO-UNEMP-CLAIMS(SUB1) NOT = 
                 OWR-NO-UNEMP-CLAIMS(SUB1)
                 STRING "WR-NO-UNEMP-CLAIMS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-NO-UNEMP-CLAIMS(SUB1)  TO D01-A15-NUM
                 MOVE WR-NO-UNEMP-CLAIMS(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-NO-AD-D-CLAIMS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-NO-AD-D-CLAIMS(SUB1)
              END-IF
              IF WR-NO-AD-D-CLAIMS(SUB1) NOT = OWR-NO-AD-D-CLAIMS(SUB1)
                 STRING "WR-NO-AD-D-CLAIMS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-NO-AD-D-CLAIMS(SUB1)  TO D01-A15-NUM
                 MOVE WR-NO-AD-D-CLAIMS(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-NO-L-PROT-CLAIMS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-NO-L-PROT-CLAIMS(SUB1)
              END-IF
              IF WR-NO-L-PROT-CLAIMS(SUB1) NOT = 
                 OWR-NO-L-PROT-CLAIMS(SUB1)
                 STRING "WR-NO-L-PROT-CLAIMS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-NO-L-PROT-CLAIMS(SUB1)  TO D01-A15-NUM
                 MOVE WR-NO-L-PROT-CLAIMS(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-NO-NF-NON-BK-CLAIMS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-NO-NF-NON-BK-CLAIMS(SUB1)
              END-IF
              IF WR-NO-NF-NON-BK-CLAIMS(SUB1) NOT = 
                 OWR-NO-NF-NON-BK-CLAIMS(SUB1)
                 STRING "WR-NO-NF-NON-BK-CLAIMS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-NO-NF-NON-BK-CLAIMS(SUB1)  TO D01-A15-NUM
                 MOVE WR-NO-NF-NON-BK-CLAIMS(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-NO-NF-BK-CLAIMS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-NO-NF-BK-CLAIMS(SUB1)
              END-IF
              IF WR-NO-NF-BK-CLAIMS(SUB1) NOT = 
                 OWR-NO-NF-BK-CLAIMS(SUB1)
                 STRING "WR-NO-NF-BK-CLAIMS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-NO-NF-BK-CLAIMS(SUB1)  TO D01-A15-NUM
                 MOVE WR-NO-NF-BK-CLAIMS(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-NO-ROAD-GARD-CLAIMS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-NO-ROAD-GARD-CLAIMS(SUB1)
              END-IF
              IF WR-NO-ROAD-GARD-CLAIMS(SUB1) NOT = 
                 OWR-NO-ROAD-GARD-CLAIMS(SUB1)
                 STRING "WR-NO-ROAD-GARD-CLAIMS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-NO-ROAD-GARD-CLAIMS(SUB1)  TO D01-A15-NUM
                 MOVE WR-NO-ROAD-GARD-CLAIMS(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-LIFE-PREMS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-LIFE-PREMS(SUB1)
              END-IF
              IF WR-LIFE-PREMS(SUB1) NOT = OWR-LIFE-PREMS(SUB1)
                 STRING "WR-LIFE-PREMS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-LIFE-PREMS(SUB1)  TO D01-A15-NUM
                 MOVE WR-LIFE-PREMS(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-AH-PREMS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-AH-PREMS(SUB1)
              END-IF
              IF WR-AH-PREMS(SUB1) NOT = OWR-AH-PREMS(SUB1)
                 STRING "WR-AH-PREMS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-AH-PREMS(SUB1)  TO D01-A15-NUM
                 MOVE WR-AH-PREMS(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-FIRE-PREMS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-FIRE-PREMS(SUB1)
              END-IF
              IF WR-FIRE-PREMS(SUB1) NOT = OWR-FIRE-PREMS(SUB1)
                 STRING "WR-FIRE-PREMS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-FIRE-PREMS(SUB1)  TO D01-A15-NUM
                 MOVE WR-FIRE-PREMS(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-AUTO-PREMS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-AUTO-PREMS(SUB1)
              END-IF
              IF WR-AUTO-PREMS(SUB1) NOT = OWR-AUTO-PREMS(SUB1)
                 STRING "WR-AUTO-PREMS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-AUTO-PREMS(SUB1)  TO D01-A15-NUM
                 MOVE WR-AUTO-PREMS(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-UNEMP-PREMS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-UNEMP-PREMS(SUB1)
              END-IF
              IF WR-UNEMP-PREMS(SUB1) NOT = OWR-UNEMP-PREMS(SUB1)
                 STRING "WR-UNEMP-PREMS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-UNEMP-PREMS(SUB1)  TO D01-A15-NUM
                 MOVE WR-UNEMP-PREMS(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-AD-D-PREMS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-AD-D-PREMS(SUB1)
              END-IF
              IF WR-AD-D-PREMS(SUB1) NOT = OWR-AD-D-PREMS(SUB1)
                 STRING "WR-AD-D-PREMS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-AD-D-PREMS(SUB1)  TO D01-A15-NUM
                 MOVE WR-AD-D-PREMS(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-L-PROT-PREMS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-L-PROT-PREMS(SUB1)
              END-IF
              IF WR-L-PROT-PREMS(SUB1) NOT = OWR-L-PROT-PREMS(SUB1)
                 STRING "WR-L-PROT-PREMS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-L-PROT-PREMS(SUB1)  TO D01-A15-NUM
                 MOVE WR-L-PROT-PREMS(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-NON-FILE-PREMS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-NON-FILE-PREMS(SUB1)
              END-IF
              IF WR-NON-FILE-PREMS(SUB1) NOT = OWR-NON-FILE-PREMS(SUB1)
                 STRING "WR-NON-FILE-PREMS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-NON-FILE-PREMS(SUB1)  TO D01-A15-NUM
                 MOVE WR-NON-FILE-PREMS(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-ROAD-GARD-PREMS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-ROAD-GARD-PREMS(SUB1)
              END-IF
              IF WR-ROAD-GARD-PREMS(SUB1) NOT = 
                 OWR-ROAD-GARD-PREMS(SUB1)
                 STRING "WR-ROAD-GARD-PREMSB(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-ROAD-GARD-PREMS(SUB1)  TO D01-A15-NUM
                 MOVE WR-ROAD-GARD-PREMS(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-NO-LIFE-PREMS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-NO-LIFE-PREMS(SUB1)
              END-IF
              IF WR-NO-LIFE-PREMS(SUB1) NOT = OWR-NO-LIFE-PREMS(SUB1)
                 STRING "WR-NO-LIFE-PREMS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-NO-LIFE-PREMS(SUB1)  TO D01-A15 
                 MOVE WR-NO-LIFE-PREMS(SUB1)   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

             IF OWR-NO-AH-PREMS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-NO-AH-PREMS(SUB1)
              END-IF
              IF WR-NO-AH-PREMS(SUB1) NOT = OWR-NO-AH-PREMS(SUB1)
                 STRING "WR-NO-AH-PREMS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-NO-AH-PREMS(SUB1)  TO D01-A15 
                 MOVE WR-NO-AH-PREMS(SUB1)   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

             IF OWR-NO-FIRE-PREMS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-NO-FIRE-PREMS(SUB1)
              END-IF
              IF WR-NO-FIRE-PREMS(SUB1) NOT = OWR-NO-FIRE-PREMS(SUB1)
                 STRING "WR-NO-FIRE-PREMS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-NO-FIRE-PREMS(SUB1)  TO D01-A15 
                 MOVE WR-NO-FIRE-PREMS(SUB1)   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

             IF OWR-NO-AUTO-PREMS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-NO-AUTO-PREMS(SUB1)
              END-IF
              IF WR-NO-AUTO-PREMS(SUB1) NOT = OWR-NO-AUTO-PREMS(SUB1)
                 STRING "WR-NO-AUTO-PREMS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-NO-AUTO-PREMS(SUB1)  TO D01-A15 
                 MOVE WR-NO-AUTO-PREMS(SUB1)   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-NO-UNEMP-PREMS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-NO-UNEMP-PREMS(SUB1)
              END-IF
              IF WR-NO-UNEMP-PREMS(SUB1) NOT = OWR-NO-UNEMP-PREMS(SUB1)
                 STRING "WR-NO-UNEMP-PREMS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-NO-UNEMP-PREMS(SUB1)  TO D01-A15 
                 MOVE WR-NO-UNEMP-PREMS(SUB1)   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-NO-AD-D-PREMS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-NO-AD-D-PREMS(SUB1)
              END-IF
              IF WR-NO-AD-D-PREMS(SUB1) NOT = OWR-NO-AD-D-PREMS(SUB1)
                 STRING "WR-NO-AD-D-PREMS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-NO-AD-D-PREMS(SUB1)  TO D01-A15 
                 MOVE WR-NO-AD-D-PREMS(SUB1)   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-NO-L-PROT-PREMS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-NO-L-PROT-PREMS(SUB1)
              END-IF
              IF WR-NO-L-PROT-PREMS(SUB1) NOT = 
                 OWR-NO-L-PROT-PREMS(SUB1)
                 STRING "WR-NO-L-PROT-PREMS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-NO-L-PROT-PREMS(SUB1)  TO D01-A15 
                 MOVE WR-NO-L-PROT-PREMS(SUB1)   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-NO-NON-FILE-PREMS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-NO-NON-FILE-PREMS(SUB1)
              END-IF
              IF WR-NO-NON-FILE-PREMS(SUB1) NOT = 
                 OWR-NO-NON-FILE-PREMS(SUB1)
                 STRING "WR-NO-NON-FILE-PREMS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-NO-NON-FILE-PREMS(SUB1)  TO D01-A15 
                 MOVE WR-NO-NON-FILE-PREMS(SUB1)   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-NO-ROAD-GARD-PREMS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-NO-ROAD-GARD-PREMS(SUB1)
              END-IF
              IF WR-NO-ROAD-GARD-PREMS(SUB1) NOT = 
                 OWR-NO-ROAD-GARD-PREMS(SUB1)
                 STRING "WR-NO-ROAD-GARD-PREMS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-NO-ROAD-GARD-PREMS(SUB1)  TO D01-A15 
                 MOVE WR-NO-ROAD-GARD-PREMS(SUB1)   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-LIFE-REFS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-LIFE-REFS(SUB1)
              END-IF
              IF WR-LIFE-REFS(SUB1) NOT = OWR-LIFE-REFS(SUB1)
                 STRING "WR-LIFE-REFS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-LIFE-REFS(SUB1)  TO D01-A15-NUM
                 MOVE WR-LIFE-REFS(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-AH-REFS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-AH-REFS(SUB1)
              END-IF
              IF WR-AH-REFS(SUB1) NOT = OWR-AH-REFS(SUB1)
                 STRING "WR-AH-REFS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-AH-REFS(SUB1)  TO D01-A15-NUM
                 MOVE WR-AH-REFS(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-FIRE-REFS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-FIRE-REFS(SUB1)
              END-IF
              IF WR-FIRE-REFS(SUB1) NOT = OWR-FIRE-REFS(SUB1)
                 STRING "WR-FIRE-REFS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-FIRE-REFS(SUB1)  TO D01-A15-NUM
                 MOVE WR-FIRE-REFS(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-AUTO-REFS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-AUTO-REFS(SUB1)
              END-IF
              IF WR-AUTO-REFS(SUB1) NOT = OWR-AUTO-REFS(SUB1)
                 STRING "WR-AUTO-REFS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-AUTO-REFS(SUB1)  TO D01-A15-NUM
                 MOVE WR-AUTO-REFS(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-UNEMP-REFS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-UNEMP-REFS(SUB1)
              END-IF
              IF WR-UNEMP-REFS(SUB1) NOT = OWR-UNEMP-REFS(SUB1)
                 STRING "WR-UNEMP-REFS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-UNEMP-REFS(SUB1)  TO D01-A15-NUM
                 MOVE WR-UNEMP-REFS(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-AD-D-REFS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-AD-D-REFS(SUB1)
              END-IF
              IF WR-AD-D-REFS(SUB1) NOT = OWR-AD-D-REFS(SUB1)
                 STRING "WR-AD-D-REFS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-AD-D-REFS(SUB1)  TO D01-A15-NUM
                 MOVE WR-AD-D-REFS(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-L-PROT-REFS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-L-PROT-REFS(SUB1)
              END-IF
              IF WR-L-PROT-REFS(SUB1) NOT = OWR-L-PROT-REFS(SUB1)
                 STRING "WR-L-PROT-REFS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-L-PROT-REFS(SUB1)  TO D01-A15-NUM
                 MOVE WR-L-PROT-REFS(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-NON-FILE-REFS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-NON-FILE-REFS(SUB1)
              END-IF
              IF WR-NON-FILE-REFS(SUB1) NOT = OWR-NON-FILE-REFS(SUB1)
                 STRING "WR-NON-FILE-REFS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-NON-FILE-REFS(SUB1)  TO D01-A15-NUM
                 MOVE WR-NON-FILE-REFS(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-ROAD-GARD-REFS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-ROAD-GARD-REFS(SUB1)
              END-IF
              IF WR-ROAD-GARD-REFS(SUB1) NOT = OWR-ROAD-GARD-REFS(SUB1)
                 STRING "WR-ROAD-GARD-REFS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-ROAD-GARD-REFS(SUB1)  TO D01-A15-NUM
                 MOVE WR-ROAD-GARD-REFS(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-NF-RECOVERIES(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-NF-RECOVERIES(SUB1)
              END-IF
              IF WR-NF-RECOVERIES(SUB1) NOT = OWR-NF-RECOVERIES(SUB1)
                 STRING "WR-NF-RECOVERIES(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-NF-RECOVERIES(SUB1)  TO D01-A15-NUM
                 MOVE WR-NF-RECOVERIES(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.

           IF OWR-NONFILE-ENDING-BALANCE NOT NUMERIC
              MOVE 0 TO OWR-NONFILE-ENDING-BALANCE.
           IF WR-NONFILE-ENDING-BALANCE NOT = OWR-NONFILE-ENDING-BALANCE
              MOVE "WR-NONFILE-ENDING-BALANCE" TO D01-TEXT
              MOVE OWR-NONFILE-ENDING-BALANCE  TO D01-A15-NUM
              MOVE WR-NONFILE-ENDING-BALANCE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-NO-CUST-1-YR-AGO NOT NUMERIC
              MOVE 0 TO OWR-NO-CUST-1-YR-AGO.
           IF WR-NO-CUST-1-YR-AGO NOT = OWR-NO-CUST-1-YR-AGO
              MOVE "WR-NO-CUST-1-YR-AGO" TO D01-TEXT
              MOVE OWR-NO-CUST-1-YR-AGO  TO D01-A15 
              MOVE WR-NO-CUST-1-YR-AGO   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-OUTS-BAL-1-YR-AGO NOT NUMERIC
              MOVE 0 TO OWR-OUTS-BAL-1-YR-AGO.
           IF WR-OUTS-BAL-1-YR-AGO NOT = OWR-OUTS-BAL-1-YR-AGO
              MOVE "WR-OUTS-BAL-1-YR-AGO" TO D01-TEXT
              MOVE OWR-OUTS-BAL-1-YR-AGO  TO D01-A15-NUM
              MOVE WR-OUTS-BAL-1-YR-AGO   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-WCBC-NO-CUST-1-YR-AGO NOT NUMERIC
              MOVE 0 TO OWR-WCBC-NO-CUST-1-YR-AGO.
           IF WR-WCBC-NO-CUST-1-YR-AGO NOT = OWR-WCBC-NO-CUST-1-YR-AGO
              MOVE "WR-WCBC-NO-CUST-1-YR-AGO" TO D01-TEXT
              MOVE OWR-WCBC-NO-CUST-1-YR-AGO  TO D01-A15 
              MOVE WR-WCBC-NO-CUST-1-YR-AGO   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-WCBC-OUTS-BAL-1-YR-AGO NOT NUMERIC
              MOVE 0 TO OWR-WCBC-OUTS-BAL-1-YR-AGO.
           IF WR-WCBC-OUTS-BAL-1-YR-AGO NOT = OWR-WCBC-OUTS-BAL-1-YR-AGO
              MOVE "WR-WCBC-OUTS-BAL-1-YR-AGO" TO D01-TEXT
              MOVE OWR-WCBC-OUTS-BAL-1-YR-AGO  TO D01-A15-NUM
              MOVE WR-WCBC-OUTS-BAL-1-YR-AGO   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-LARGE-NO-CUST-1-YR-AGO NOT NUMERIC
              MOVE 0 TO OWR-LARGE-NO-CUST-1-YR-AGO.
           IF WR-LARGE-NO-CUST-1-YR-AGO NOT = OWR-LARGE-NO-CUST-1-YR-AGO
              MOVE "WR-LARGE-NO-CUST-1-YR-AGO" TO D01-TEXT
              MOVE OWR-LARGE-NO-CUST-1-YR-AGO  TO D01-A15 
              MOVE WR-LARGE-NO-CUST-1-YR-AGO   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-LARGE-OUTS-BAL-1-YR-AGO NOT NUMERIC
              MOVE 0 TO OWR-LARGE-OUTS-BAL-1-YR-AGO.
           IF WR-LARGE-OUTS-BAL-1-YR-AGO NOT = 
              OWR-LARGE-OUTS-BAL-1-YR-AGO
              MOVE "WR-LARGE-OUTS-BAL-1-YR-AGO" TO D01-TEXT
              MOVE OWR-LARGE-OUTS-BAL-1-YR-AGO  TO D01-A15-NUM
              MOVE WR-LARGE-OUTS-BAL-1-YR-AGO   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-CUR-NO-CONT-SLOW NOT NUMERIC
              MOVE 0 TO OWR-CUR-NO-CONT-SLOW.
           IF WR-CUR-NO-CONT-SLOW NOT = OWR-CUR-NO-CONT-SLOW
              MOVE "WR-CUR-NO-CONT-SLOW" TO D01-TEXT
              MOVE OWR-CUR-NO-CONT-SLOW  TO D01-A15 
              MOVE WR-CUR-NO-CONT-SLOW   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-WCBC-CUR-NO-CONT-SLOW NOT NUMERIC
              MOVE 0 TO OWR-WCBC-CUR-NO-CONT-SLOW.
           IF WR-WCBC-CUR-NO-CONT-SLOW NOT = OWR-WCBC-CUR-NO-CONT-SLOW
              MOVE "WR-WCBC-CUR-NO-CONT-SLOW" TO D01-TEXT
              MOVE OWR-WCBC-CUR-NO-CONT-SLOW  TO D01-A15 
              MOVE WR-WCBC-CUR-NO-CONT-SLOW   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-LARGE-CUR-NO-CONT-SLOW NOT NUMERIC
              MOVE 0 TO OWR-LARGE-CUR-NO-CONT-SLOW.
           IF WR-LARGE-CUR-NO-CONT-SLOW NOT = OWR-LARGE-CUR-NO-CONT-SLOW
              MOVE "WR-LARGE-CUR-NO-CONT-SLOW" TO D01-TEXT
              MOVE OWR-LARGE-CUR-NO-CONT-SLOW  TO D01-A15 
              MOVE WR-LARGE-CUR-NO-CONT-SLOW   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF WR-CASHIER-BONUS NOT NUMERIC
              MOVE 0 TO WR-CASHIER-BONUS.
           IF WR-CASHIER-BONUS NOT = WR-CASHIER-BONUS
              MOVE "WR-CASHIER-BONUS" TO D01-TEXT
              MOVE OWR-CASHIER-BONUS  TO D01-A15 
              MOVE WR-CASHIER-BONUS   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-ASST-MGR-BONUS NOT NUMERIC
              MOVE 0 TO OWR-ASST-MGR-BONUS.
           IF WR-ASST-MGR-BONUS NOT = OWR-ASST-MGR-BONUS
              MOVE "WR-ASST-MGR-BONUS" TO D01-TEXT
              MOVE OWR-ASST-MGR-BONUS  TO D01-A15 
              MOVE WR-ASST-MGR-BONUS   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OWR-MGR-BONUS NOT NUMERIC
              MOVE 0 TO OWR-MGR-BONUS.
           IF WR-MGR-BONUS NOT = OWR-MGR-BONUS
              MOVE "WR-MGR-BONUS" TO D01-TEXT
              MOVE OWR-MGR-BONUS  TO D01-A15-NUM
              MOVE WR-MGR-BONUS   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 2

              IF OWR-LOTS-NF-CLAIMS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-LOTS-NF-CLAIMS(SUB1)
              END-IF
              IF WR-LOTS-NF-CLAIMS(SUB1) NOT = OWR-LOTS-NF-CLAIMS(SUB1)
                 STRING "WR-LOTS-NF-CLAIMS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-LOTS-NF-CLAIMS(SUB1)  TO D01-A15-NUM
                 MOVE WR-LOTS-NF-CLAIMS(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-LOTS-NO-NF-CLAIMS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-LOTS-NO-NF-CLAIMS(SUB1)
              END-IF
              IF WR-LOTS-NO-NF-CLAIMS(SUB1) NOT = 
                 OWR-LOTS-NO-NF-CLAIMS(SUB1)
                 STRING "WR-LOTS-NO-NF-CLAIMS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-LOTS-NO-NF-CLAIMS(SUB1)  TO D01-A15 
                 MOVE WR-LOTS-NO-NF-CLAIMS(SUB1)   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-LOTS-NON-FILE-PREMS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-LOTS-NON-FILE-PREMS(SUB1)
              END-IF
              IF WR-LOTS-NON-FILE-PREMS(SUB1) NOT = 
                 OWR-LOTS-NON-FILE-PREMS(SUB1)
                 STRING "WR-LOTS-NON-FILE-PREMS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-LOTS-NON-FILE-PREMS(SUB1)  TO D01-A15-NUM
                 MOVE WR-LOTS-NON-FILE-PREMS(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-LOTS-NO-NON-FILE-PREMS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-LOTS-NO-NON-FILE-PREMS(SUB1)
              END-IF
              IF WR-LOTS-NO-NON-FILE-PREMS(SUB1) NOT = 
                 OWR-LOTS-NO-NON-FILE-PREMS(SUB1)
                 STRING "WR-LOTS-NO-NON-FILE-PREMS(",SUB1,")" 
                    INTO D01-TEXT
                 MOVE OWR-LOTS-NO-NON-FILE-PREMS(SUB1)  TO D01-A15
                 MOVE WR-LOTS-NO-NON-FILE-PREMS(SUB1)   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-LOTS-NF-RECOVERIES(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-LOTS-NF-RECOVERIES(SUB1)
              END-IF
              IF WR-LOTS-NF-RECOVERIES(SUB1) NOT = 
                 OWR-LOTS-NF-RECOVERIES(SUB1)
                 STRING "WR-LOTS-NF-RECOVERIES(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-LOTS-NF-RECOVERIES(SUB1)  TO D01-A15-NUM
                 MOVE WR-LOTS-NF-RECOVERIES(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.

           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 2

              IF OWR-PRETAX-PROFIT(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-PRETAX-PROFIT(SUB1)
              END-IF
              IF WR-PRETAX-PROFIT(SUB1) NOT = OWR-PRETAX-PROFIT(SUB1)
                 STRING "WR-PRETAX-PROFIT(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-PRETAX-PROFIT(SUB1)  TO D01-A15-NUM
                 MOVE WR-PRETAX-PROFIT(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF

              IF OWR-SUPERVISION(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-SUPERVISION(SUB1)
              END-IF
              IF WR-SUPERVISION(SUB1) NOT = OWR-SUPERVISION(SUB1)
                 STRING "WR-SUPERVISION(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-SUPERVISION(SUB1)  TO D01-A15-NUM
                 MOVE WR-SUPERVISION(SUB1)   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.

           IF WR-MGR-PENALTY-FG NOT = OWR-MGR-PENALTY-FG
              MOVE "WR-MGR-PENALTY-FG" TO D01-TEXT
              MOVE OWR-MGR-PENALTY-FG  TO D01-A15 
              MOVE WR-MGR-PENALTY-FG   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.
 
           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 2

              IF OWR-NO-TAX-RETURNS(SUB1) NOT NUMERIC
                 MOVE 0 TO OWR-NO-TAX-RETURNS(SUB1) 
              END-IF
              IF WR-NO-TAX-RETURNS(SUB1) NOT = OWR-NO-TAX-RETURNS(SUB1)
                 STRING "WR-NO-TAX-RETURNS(",SUB1,")" INTO D01-TEXT
                 MOVE OWR-NO-TAX-RETURNS(SUB1)  TO D01-A15 
                 MOVE WR-NO-TAX-RETURNS(SUB1)   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.

       EXIT-COMPARE-FIELDS.
           EXIT.

       WRITE-DETAIL-LINE-ERROR-KEY SECTION.

              MOVE WR-YYYYMM TO D01-DATE.
              MOVE WR-BRNO   TO D01-BRNO.

       WRITE-DETAIL-LINE-ERROR SECTION.

           ADD 1    TO TOTAL-ERRORS-CTR.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************

           ADD  LN-FEED                   TO LN-COUNT.

           IF ( LN-COUNT > EXT-CONAME-RPT-LINES )
              PERFORM WRITE-HEADER-LINES.

           MOVE DETAIL-LINE                TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 01                         TO LN-FEED.
           MOVE SPACES                     TO DETAIL-LINE.

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

           MOVE HEADER-LINE-03             TO PR-REC.
           ADD  02                         TO LN-COUNT.
           MOVE 02                         TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE 2                          TO LN-FEED.

       WRITE-HEADER-LINES-EXIT.
           EXIT.

      ******************************************************************
       RANGE-LINES SECTION.
      ******************************************************************

           MOVE 88                                    TO LN-COUNT.
 
           MOVE "BEGINNING DATE (MMYY)         : "    TO R01-PROMPT.
           MOVE BEG-MMYY                              TO R01-NUMERIC.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING DATE (MMYY)            : "    TO R01-PROMPT.
           MOVE END-MMYY                              TO R01-NUMERIC.
           MOVE 01                                    TO LN-FEED.
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
           MOVE EXT-CONAME-CONAME TO WRFILE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO WRFILE-SYSDATE.
           DISPLAY WRFILE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE WRFILE-FORM-FIELDS TO WR-BUF.
           MOVE WRFILE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE WRFILE-HELP--FILE TO HELP-LINK-FILE.
           MOVE WRFILE-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/WRFILE_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ****************************************
       TEST-INVALID-DATE SECTION.
      ****************************************
           IF DATE-YYYYMMDD NOT NUMERIC
              MOVE 0     TO DATE-YYYYMMDD
              GO TO EXIT-TEST-INVALID-DATE
           ELSE
           IF DATE-YYYYMMDD = 0 OR 20202020
              MOVE 0     TO DATE-YYYYMMDD
              GO TO EXIT-TEST-INVALID-DATE.

      * CANT FIX BAD YEAR
           IF (DATE-YYYYMMDD-YYYY = 0) AND
               (DATE-YYYYMMDD-MM NOT = 0 OR DATE-YYYYMMDD-DD NOT = 0)
              MOVE 0 TO DATE-YYYYMMDD
              GO TO EXIT-TEST-INVALID-DATE.
      * 20000003  00/03/00 A15

      * 19638133
      * 19750000
      * 19970229

      * CANT FIX A BAD MONTH, SO FORCE IT TO 12 IF ONLY THING IS WRONG
      * DONT CARE IF DAY IS INVALID, IT WILL GET FIXED IN THE NEXT 'IF'
           IF (DATE-YYYYMMDD-YYYY NOT = 0) AND
                (DATE-YYYYMMDD-MM < 1 OR > 12)
              MOVE 12 TO DATE-YYYYMMDD-MM.

           IF (DATE-YYYYMMDD-DD < 1 OR > 31) OR
              ((DATE-YYYYMMDD-MM = 02) AND (DATE-YYYYMMDD-DD > 28)) OR
              ((DATE-YYYYMMDD-DD > 30) AND
                            (DATE-YYYYMMDD-MM = 4 OR 6 OR 9 OR 11))
                 MOVE 33            TO DATE-YYYYMMDD-DD
                 MOVE DATE-YYYYMMDD TO NDTE-DATE
                 PERFORM NEW-DATE-CALCULATION
                 MOVE NDTE-DATE     TO DATE-YYYYMMDD.

       EXIT-TEST-INVALID-DATE.
           EXIT.

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

      *-----------------------------------------------------------------
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

           MOVE WR-BRNO                            TO PROGID-BRANCH.
           STRING "PROCESSING YYMM: ", WR-YYYYMM INTO PROGID-TEXT.

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
       COPY "LIBSP/SPWRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBSP/SPWR1RN.CPY".

       OPEN-OWR1-FILE.
           PERFORM OPEN-IT.
           MOVE OWR-PATH TO E-FILE.
           OPEN INPUT OWR-FILE.
           IF IO-FG = 8
              GO TO OPEN-OWR1-FILE.
           IF IO-FG = 7
              CLOSE OWR-FILE
              GO TO OPEN-OWR1-FILE.

       START-OWR1-FILE.
           PERFORM START-IT.
           MOVE OWR-PATH TO E-FILE.
           MOVE OWR1-KEY TO E-KEYX.
           START OWR-FILE KEY NOT < OWR1-KEY.

       READ-OWR1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OWR-PATH TO E-FILE.
           MOVE OWR1-KEY TO E-KEYX.
           READ OWR-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OWR1-FILE-NEXT.

       CLOSE-OWR1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OWR-FILE.

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
              DESTROY WRFILE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
