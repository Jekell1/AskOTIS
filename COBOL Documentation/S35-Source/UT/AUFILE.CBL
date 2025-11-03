       IDENTIFICATION DIVISION.
       PROGRAM-ID.    AUFILE.
       DATE-WRITTEN.  02-01-2024.
       AUTHOR. RON ZAMISKA.
      ******************************************************************
      *
      * DESCRIPTION   : COMPARE S35 AUFILE WITH A15 AUFILE
      *            
      *                 TO BE USED AFTER AN UPDATE
      *                 WILL COMPARE EACH FIELD AND REPORT ANY 
      *                 DIFFERENCES, AS LONG AS FIELD EXIST IN BOTH FILES
      *
      *  2 FILE MATCH PROCESSING.
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY "LIBGB/GBFSPR.CPY".

      * COPYMEMBER: LIBGB/GBFSAU
           SELECT OAU-FILE ASSIGN TO OAU-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OAU1-KEY
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  OAU-FILE
           LABEL RECORDS ARE STANDARD.
      *-----------------------------------------------------------------
       COPY "LIBUP/A15SRC/GB01AU.CPY" REPLACING LEADING "AU" BY "OAU".
      * COPYMEMBER: LIBGB/GB01AU
      **************************************************************************
      *        AUDIT CHANGE FILE BY FILE         BYTES = 335
      *
      *        KEY 1 - FILE CODE, DATE, SEQ
      * REV:
      *  20220207 BAH NEW #1542
      **************************************************************************
      *01  AU-REC.
      *    03  AU1-KEY.
      *        05  AU-FILE-NAME          PIC X(7).
      *        05  AU-DATE               PIC 9(8).
      *        05  AU-SEQ                PIC 9(8).
      *
      *    03  AU-ACTION                 PIC X.
      *        88  AU-ACTION-DELETE           VALUE "D".
      *        88  AU-ACTION-ADD              VALUE "A".
      *        88  AU-ACTION-CHANGE           VALUE "C".
      * FOR WHEN NEEDED WITH FILES LIKE THE CDFILE
      *    03  AU-FILE-TYPE              PIC X(2).
      *    03  AU-BRANCH                 PIC 9(4).
      *    03  AU-FILE-KEY               PIC X(50).
      *    03  AU-FIELD-DESC             PIC X(30).
      *    03  AU-ORIG-VALUE             PIC X(80).
      *    03  AU-NEW-VALUE              PIC X(80).
      *    03  AU-TICKET                 PIC X(20).
      *    03  AU-PROGRAM                PIC X(8).
      *    03  AU-TIME                   PIC 9(6).
      *    03  AU-USERID                 PIC X(8).
      *    03  FILLER                    PIC X(23).

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/AUFILE S35 02/28/24-11:45:37 >".

      *COPY "LIBGB/GB01BR.CPY".
      *COPY "LIBGB/GB01BR_SQL.CPY".
      *COPY "LIBGB/GBWSBR.CPY".
      *COPY "LIBGB/GB01GR.CPY".
      *COPY "LIBGB/GB01GR_SQL.CPY".
      *COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGB/GB01AU.CPY".
       COPY "LIBGB/GB01AU_SQL.CPY".
       COPY "LIBGB/GBWSAU.CPY".
   

      *********************************************************
      *        LIBGB/GBWSAU: AUFILE PATH WORKERS
      *********************************************************
   
       01  OAU-PATH                    PIC X(35) VALUE SPACES.
       01  OAU-ALL-PATH.
           03  OAU-ALL-BASE     PIC X(9).
           03  FILLER           PIC X     VALUE "/".
           03  OAU-ALL-MACHINE  PIC XX.
           03  FILLER           PIC X(10) VALUE
               "/GB/AUFILE".


 
      ******************************************************************
      *
      *    M I S C E L L A N E O U S - W O R K E R S
      *
      ******************************************************************
       01 BEG-BY-DATE-CCYYMMDD.
           03  BEG-BY-DATE-CCYYMM.
             05 BEG-BY-DATE-CC       PIC 99.
             05 BEG-BY-DATE-YYMM.
                07 BEG-BY-DATE-YY    PIC 99.
                07 BEG-BY-DATE-MM    PIC 99.
           03  BEG-BY-DATE-DD        PIC 99        VALUE 01.

       01 END-BY-DATE-CCYYMMDD.
          03  END-BY-DATE-CCYYMM.
              05 END-BY-DATE-CC      PIC 99.
              05 END-BY-DATE-YYMM.
                07 END-BY-DATE-YY    PIC 99.
                07 END-BY-DATE-MM    PIC 99.
           03  END-BY-DATE-DD        PIC 99.

       01 SQL-AU-DATE-CCYY-MM-DD. 
           03  SQL-AU-DATE-CCYY       PIC X(04).
           03  FILLER                 PIC X(01).
           03  SQL-AU-DATE-MM         PIC X(02).
           03  FILLER                 PIC X(01).
           03  SQL-AU-DATE-DD         PIC X(02).   

       01 SQL-AU-DATE-CCYYMMDD. 
           03  SQL-AU-DATE-CCYY2      PIC X(04).
           03  SQL-AU-DATE-MM2        PIC X(02). 
           03  SQL-AU-DATE-DD2        PIC X(02).   

       01  SQL-ROW-KEY.
           03  SQL-ROW-NAME           PIC X(07).    
           03  SQL-ROW-DATE           PIC X(08).
           03  SQL-ROW-SEQ            PIC 9(08).

       01  PROC-YYMM                   PIC 9(4).
       01  PROC-YYMM-RDF  REDEFINES PROC-YYMM.
           03 PROC-YYMM-YY             PIC 99.
           03 PROC-YYMM-MM             PIC 99.

       01  AU-EOF-SW                   PIC X      VALUE "N".
       01  OAU-EOF-SW                  PIC X      VALUE "N".
       01  TOTAL-ERRORS-CTR            PIC 9(9)   VALUE 0.
       01  SUB1                        PIC 99     VALUE 0.
       01  A15-FILE-FOUND-SW           PIC X      VALUE "N".

       01  WORK-TIME.
           03  WORK-HR                 PIC 9(02).
           03  WORK-MIN                PIC 9(02).
           03  WORK-SEC                PIC 9(02).

       01  CONV-TYPE-CHAR              PIC X.
       01  CONV-TYPE-NUM    REDEFINES CONV-TYPE-CHAR
                                       PIC 9.

       01  TEST-ACCTNO                 PIC 9(8).
       01  FILLER REDEFINES TEST-ACCTNO.
             03  TEST-NUMBER           PIC 9(6).
             03  TEST-SEQ              PIC 9(2).    

      * 01  OAU-USE-KEY.
      *     03 OAU-USE-KEY-ID           PIC 9(12).
 
      * 01  AL-USE-KEY.
      *     03 AL-USE-KEY-ID            PIC 9(12).
 
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

       01  LEGEND-BEG-NAME.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F6-SCAN, F7-".
           03  FILLER PIC X(30) VALUE "EXIT, ENTER BEGINNING AU-FILE-".
           03  FILLER PIC X(19) VALUE "NAME               ".

       01  LEGEND-END-NAME.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER ENDIN".
           03  FILLER PIC X(30) VALUE "G AU-FILE-NAME                ".
           03  FILLER PIC X(19) VALUE "                   ".

       01  LEGEND-BEG-DATE.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F6-SCAN, F7-".
           03  FILLER PIC X(30) VALUE "EXIT, ENTER BEGINNING AU-DATE ".
           03  FILLER PIC X(19) VALUE "                   ".

       01  LEGEND-END-DATE.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER ENDIN".
           03  FILLER PIC X(30) VALUE "G AU-DATE                     ".
           03  FILLER PIC X(19) VALUE "                   ".

       01  LEGEND-BEG-SEQ.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F6-SCAN, F7-".
           03  FILLER PIC X(30) VALUE "EXIT, ENTER BEGINNING AU-SEQ  ".
           03  FILLER PIC X(19) VALUE "                   ".

       01  LEGEND-END-SEQ.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER ENDIN".
           03  FILLER PIC X(30) VALUE "G AU-SEQ                      ".
           03  FILLER PIC X(19) VALUE "                   ".

      *-----------------------------------------------------------------
       01  LEGEND-ACCEPT-YN.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT             ".
           03  FILLER PIC X(30) VALUE "                              ".
           03  FILLER PIC X(19) VALUE "     ACCEPT (Y/N): ".

       01  SOURCE-PATH-LIST            PIC X(07)     VALUE "S-PATH.".

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  SOURCE-PATH             PIC X(09).
           03  BEG-NAME                PIC X(7).
           03  END-NAME                PIC X(7).
           03  BEG-DATE                PIC X(10).
           03  END-DATE                PIC X(10).
           03  BEG-SEQ                 PIC 9(8).
           03  END-SEQ                 PIC 9(8).

       01  VERSION-CONTROL             PIC X(01)     VALUE "A".

      *-----------------------------------------------------------------
       01  SV-BEG-NAME                 PIC X(7).
       01  SV-END-NAME                 PIC X(7).
       01  SV-BEG-DATE                 PIC X(10).
       01  SV-END-DATE                 PIC X(10).
       01  SV-BEG-SEQ                  PIC 9(8).
       01  SV-END-SEQ                  PIC 9(8). 
       
       01  LN-COUNT            PIC 9(03)     VALUE 99.
       01  LN-FEED             PIC 9(02).
       01  PAGE-NUMBER         PIC 9(04)     VALUE ZEROES.

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
           "AUDIT CHANGES (AUFILE) COMPARE A15 TO S35".

       01  HEADER-LINE-03.
           03  FILLER                PIC X(23)  VALUE "AU-KEY        ".
           03  FILLER                PIC X(02)  VALUE SPACES.
           03  FILLER                PIC X(11)  VALUE "DESCRIPTION".
           03  FILLER                PIC X(11)  VALUE SPACES.
           03  FILLER                PIC X(27)  VALUE "A15".
           03  FILLER                PIC X(03)  VALUE "S35".

      ******************************************************************
      *
      *    D E T A I L - L I N E - W O R K E R S
      *
      ******************************************************************
       01  DETAIL-LINE                        PIC X(120)   VALUE SPACES.

       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-KEY                        PIC X(23).
           03  FILLER                         PIC X(2).
           03  D01-TEXT                       PIC X(20).
           03  FILLER                         PIC X(2).
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
       COPY "LIBUT/AUFILE_DEF.CPY".
       COPY "LIBUT/AUFILE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/AUFILE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE OAU-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-AU1-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE OAU-FILE.
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

           PERFORM PROCESS-AU-DRIVER.

           PERFORM RANGE-LINES.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

           MOVE STAT---GOOD                TO PROCESS-STAT.

           MOVE FORM-PATHNAME              TO FORM-PATH.
           MOVE "AUFILE" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE AUFILE-QUEUE-POS  TO Q-POS.
           MOVE AUFILE-COPIES-POS TO C-POS.
           MOVE AUFILE-PRU-POS    TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           PERFORM GET-TIME.
           MOVE TIME-EDIT                  TO STARTING-TIME.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS......" TO LEGEND
              PERFORM SEND-LEGEND.

           PERFORM OPEN-PR-FILE.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO SYSDATE-YYYYMMDD.

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
              GO TO ENTER-BEG-NAME.
			  
       ENTER-SOURCE-PATH.
       
           IF SOURCE-PATH = SPACES
              MOVE "/US2/DA15" TO SOURCE-PATH
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

      *-----------------------------------------------------------------
       ENTER-BEG-NAME.

           MOVE LEGEND-BEG-NAME  TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E RA000BEG-NAME." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F3-KEY ) GO TO ENTER-ALL-NAME.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-NAME.

      *-----------------------------------------------------------------
       ENTER-END-NAME.

           MOVE LEGEND-END-NAME TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E RA000END-NAME." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-NAME.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-NAME.

           MOVE VDUNUM0 TO END-NAME.

           IF ( BEG-NAME > END-NAME )
              MOVE "INVALID NAME RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-NAME.    

      *-----------------------------------------------------------------
       ENTER-ALL-NAME.

           MOVE "S RA000BEG-NAME."   TO VDU.
           MOVE SPACES               TO BEG-NAME  VDUBUF.
           PERFORM SCREENIO.

           MOVE "S RA000END-NAME."   TO VDU.
           MOVE 'ZZZZZZZ'            TO END-NAME  VDUBUF.
           PERFORM SCREENIO.


      *-----------------------------------------------------------------
       ENTER-BEG-DATE.

           MOVE LEGEND-BEG-DATE  TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E RA000BEG-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F3-KEY ) GO TO ENTER-ALL-DATE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-DATE.

      *-----------------------------------------------------------------
       ENTER-END-DATE.

           MOVE LEGEND-END-DATE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E RA000END-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-DATE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-DATE.

           MOVE VDUNUM0 TO END-DATE.

           IF ( BEG-DATE > END-DATE )
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-DATE.    

      *-----------------------------------------------------------------
       ENTER-ALL-DATE.

           MOVE "S RA000BEG-DATE."   TO VDU.
           MOVE "1950-01-01"         TO BEG-DATE  VDUBUF.
           PERFORM SCREENIO.

           MOVE "S RA000END-DATE."   TO VDU.
           MOVE "2049-12-31"         TO END-DATE  VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       ENTER-BEG-SEQ.

           MOVE LEGEND-BEG-SEQ  TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E RA000BEG-SEQ." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F3-KEY ) GO TO ENTER-ALL-SEQ.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-SEQ.

      *-----------------------------------------------------------------
       ENTER-END-SEQ.

           MOVE LEGEND-END-SEQ TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E RA000END-SEQ." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-SEQ.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-SEQ.

           MOVE VDUNUM0 TO END-SEQ.

           IF ( BEG-SEQ > END-SEQ )
              MOVE "INVALID SEQUENCE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-SEQ.    

      *-----------------------------------------------------------------
       ENTER-ALL-SEQ.

           MOVE "S RA000BEG-SEQ."    TO VDU.
           MOVE "00000000"           TO BEG-SEQ  VDUBUF.
           PERFORM SCREENIO.

           MOVE "S RA000END-SEQ."    TO VDU.
           MOVE "99999999"           TO END-SEQ  VDUBUF.
           PERFORM SCREENIO.

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

      ****************************************
       DISPLAY-PARAMETERS SECTION.
      ****************************************

           MOVE "S  A000BEG-NAME."      TO VDU.
           MOVE BEG-NAME                TO VDUBUF.  
           PERFORM SCREENIO.
           MOVE "S  A000END-NAME."      TO VDU.
           MOVE END-NAME                TO VDUNUM0.

           MOVE "S  A000BEG-DATE."      TO VDU.
           MOVE BEG-DATE                TO VDUBUF.  
           PERFORM SCREENIO.
           MOVE "S  A000END-DATE."      TO VDU.
           MOVE END-DATE                TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  A000BEG-SEQ."       TO VDU.
           MOVE BEG-SEQ                 TO VDUBUF.  
           PERFORM SCREENIO.
           MOVE "S  A000END-SEQ."       TO VDU.
           MOVE END-SEQ                 TO VDUNUM0.

       DISPLAY-PARAMETERS-EXIT.
           EXIT.

      *******************************************************
       PROCESS-AU-DRIVER SECTION.
      *********************************************************
      D    STOP MESS.

           MOVE 9999 TO PROGID-SLEEP-COUNTER.
      *    PERFORM SEND-PROGRESS-INDICATOR.

      *  OPEN/READ S35 - ALFILE

      *    MOVE EXT-FILPATH-BASE  TO FILPATH-BASE.
           MOVE BEG-NAME            TO QAU1-WBEG-FILE-NAME.
           MOVE END-NAME            TO QAU1-WEND-FILE-NAME.
           MOVE BEG-DATE            TO QAU1-WBEG-DATE.
           MOVE END-DATE            TO QAU1-WEND-DATE.
           MOVE BEG-SEQ             TO QAU1-WBEG-SEQ
           MOVE END-SEQ             TO QAU1-WEND-SEQ.
 
           MOVE "N"  TO AU-EOF-SW.
           PERFORM OPEN-AU1-FILE.
           PERFORM START-AU1-FILE.  
           PERFORM PROCESS-READ-AU1. 

      * OPEN/READ A15 - AUFILE          
      * THIS IS A GLOBAL FILE BUT FOLLOWING SAME LOGIC.
      * ONLY PERFORMS THIS SECTION ONCE


           MOVE SOURCE-PATH           TO OAU-ALL-BASE.
           MOVE MACHINE-R1            TO OAU-ALL-MACHINE.
           MOVE OAU-ALL-PATH          TO OAU-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE "MISSING A15 FILE" TO MESS
              PERFORM SEND-MESS
              PERFORM CLOSE-AU1-FILE
              GO TO EXIT-PROCESS-READ-AU1.

           PERFORM OPEN-OAU1-FILE.
           PERFORM PROCESS-READ-OAU1.

       CONTINUE-AU-DRIVER.

           IF AU-EOF-SW  = "Y" AND
              OAU-EOF-SW = "Y"
              GO TO PROCESS-AU-DRIVER-END.              

      *     IF OAU1-KEY < SQL-ROW-KEY 
           IF OAU1-KEY < AU1-KEY 
              MOVE "MISSING S35 RECORD" TO D01-TEXT
              MOVE OAU1-KEY             TO D01-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR
              PERFORM PROCESS-READ-OAU1 
              GO TO CONTINUE-AU-DRIVER.

      *     IF SQL-ROW-KEY < OAU1-KEY 
           IF AU1-KEY < OAU1-KEY 
              MOVE "MISSING A15 RECORD" TO D01-TEXT
              MOVE AU1-KEY              TO D01-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR 
              PERFORM PROCESS-READ-AU1 
              GO TO CONTINUE-AU-DRIVER.

            PERFORM COMPARE-FIELDS.

            PERFORM PROCESS-READ-OAU1. 
            PERFORM PROCESS-READ-AU1. 

            GO TO CONTINUE-AU-DRIVER.   

       PROCESS-AU-DRIVER-END.
           EXIT. 

       PROCESS-READ-AU1 SECTION.
           PERFORM READ-AU1-FILE-NEXT.
           IF IO-BAD
              MOVE "ZZZZZZZ"      TO AU-FILE-NAME
              MOVE 99999999       TO AU-DATE
              MOVE 99999999       TO AU-SEQ
              MOVE "Y"            TO AU-EOF-SW
           ELSE
              ADD 1               TO SQL-RECORDS-PROCESSED.
         
      *     MOVE AU-DATE              TO SQL-AU-DATE-CCYY-MM-DD. 
      *     MOVE SQL-AU-DATE-CCYY     TO SQL-AU-DATE-CCYY2.
      *     MOVE SQL-AU-DATE-MM       TO SQL-AU-DATE-MM2.
      *     MOVE SQL-AU-DATE-DD       TO SQL-AU-DATE-DD2.   
      *
      *     MOVE AU-FILE-NAME         TO SQL-ROW-NAME.
      *     MOVE SQL-AU-DATE-CCYYMMDD TO SQL-ROW-DATE.
      *     MOVE AU-SEQ               TO SQL-ROW-SEQ.

       EXIT-PROCESS-READ-AU1.
           EXIT.

       PROCESS-READ-OAU1 SECTION.
           PERFORM READ-OAU1-FILE-NEXT.
           IF IO-BAD
              MOVE "ZZZZZZZ"      TO OAU-FILE-NAME
              MOVE 99999999       TO OAU-DATE
              MOVE 99999999       TO OAU-SEQ
              MOVE "Y"            TO OAU-EOF-SW
           ELSE
              ADD 1               TO A15-RECORDS-PROCESSED.

       EXIT-PROCESS-READ-OAU1.
           EXIT.

      ******************************************************************
       COMPARE-FIELDS SECTION.
      ******************************************************************

      D    STOP MESS.

           MOVE AU1-KEY           TO D01-KEY.

           IF AU-ACTION        NOT = OAU-ACTION 
              MOVE "AU-ACTION"    TO D01-TEXT
              MOVE OAU-ACTION     TO D01-A15
              MOVE AU-ACTION      TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF AU-FILE-TYPE     NOT = OAU-FILE-TYPE 
              MOVE "AU-FILE-TYPE" TO D01-TEXT
              MOVE OAU-FILE-TYPE  TO D01-A15
              MOVE AU-FILE-TYPE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF AU-BRANCH        NOT = OAU-BRANCH 
              MOVE "AU-BRANCH"    TO D01-TEXT
              MOVE OAU-BRANCH     TO D01-A15
              MOVE AU-BRANCH      TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF AU-FILE-KEY      NOT = OAU-FILE-KEY 
              MOVE "AU-FILE-KEY"  TO D01-TEXT
              MOVE OAU-FILE-KEY   TO D01-A15
              MOVE AU-FILE-KEY    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF AU-FIELD-DESC    NOT = OAU-FIELD-DESC 
              MOVE "AU-FIELD-DESC" TO D01-TEXT
              MOVE OAU-FIELD-DESC  TO D01-A15
              MOVE AU-FIELD-DESC   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF AU-ORIG-VALUE    NOT = OAU-ORIG-VALUE 
              MOVE "AU-ORIG-VALUE" TO D01-TEXT
              MOVE OAU-ORIG-VALUE  TO D01-A15
              MOVE AU-ORIG-VALUE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF AU-NEW-VALUE      NOT = OAU-NEW-VALUE 
              MOVE "AU-NEW-VALUE"  TO D01-TEXT
              MOVE OAU-NEW-VALUE   TO D01-A15
              MOVE AU-NEW-VALUE    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF AU-TICKET         NOT = OAU-TICKET 
              MOVE "AU-TICKET"     TO D01-TEXT
              MOVE OAU-TICKET      TO D01-A15
              MOVE AU-TICKET       TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF AU-PROGRAM        NOT = OAU-PROGRAM 
              MOVE "AU-PROGRAM"    TO D01-TEXT
              MOVE OAU-PROGRAM     TO D01-A15
              MOVE AU-PROGRAM      TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF AU-TIME           NOT = OAU-TIME 
              MOVE "AU-TIME"       TO D01-TEXT
              MOVE OAU-TIME        TO D01-A15
              MOVE AU-TIME         TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF AU-USERID         NOT = OAU-USERID 
              MOVE "AU-USERID"     TO D01-TEXT
              MOVE OAU-USERID      TO D01-A15
              MOVE AU-USERID       TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

       EXIT-COMPARE-FIELDS.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE-ERROR SECTION.
      ******************************************************************

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

           MOVE "BEGINNING NAME: "                    TO R01-PROMPT.
           MOVE BEG-NAME                              TO R01-TEXT.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING NAME:    "                    TO R01-PROMPT.
           MOVE END-NAME                              TO R01-TEXT.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING DATE: "                    TO R01-PROMPT.
           MOVE BEG-DATE                              TO R01-TEXT.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING DATE:    "                    TO R01-PROMPT.
           MOVE END-DATE                              TO R01-TEXT.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING SEQ:  "                    TO R01-PROMPT.
           MOVE BEG-SEQ                               TO R01-TEXT.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING SEQ:     "                    TO R01-PROMPT.
           MOVE END-SEQ                               TO R01-TEXT.
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

       RANGE-LINES-EXIT.
           EXIT.

      ******************************************************************
      *
      *    C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/PROGID.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME  TO AUFILE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO AUFILE-SYSDATE.
           DISPLAY AUFILE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE AUFILE-FORM-FIELDS TO WR-BUF.
           MOVE AUFILE-FORM-LIST   TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE AUFILE-HELP--FILE TO HELP-LINK-FILE.
           MOVE AUFILE-HELP--DIR  TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/AUFILE_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISCELLANEOUS-PARAGRAPHS SECTION.
      ******************************************************************
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
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
      *SEND-PROGRESS-INDICATOR.

      *   MOVE BR-NO                      TO PROGID-BRANCH.
      *   STRING "PROCESSING BRANCH: ", BR-NO INTO PROGID-TEXT.

      *    PERFORM GET-TIME.
      *    MOVE TIME-EDIT                  TO PROGID-TIME.

      *    IF EXT-ROUTE-BUF = "00"
      *       PERFORM PROGID-DISPLAY.

       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
       IO-ROUTINE SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBGB/GBAUGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBAU1RN.CPY".

       OPEN-OAU1-FILE.
           PERFORM OPEN-IT.
           MOVE OAU-PATH TO E-FILE.
           OPEN INPUT OAU-FILE.
           IF IO-FG = 8
              GO TO OPEN-OAU1-FILE.
           IF IO-FG = 7
              CLOSE OAU-FILE
              GO TO OPEN-OAU1-FILE.
  
      * START-OCH1-FILE.    * CAN'T START SEQUENTIAL FILE
      *     PERFORM START-IT.
      *     MOVE OAU-PATH TO E-FILE.
      *     MOVE OAU-KEY  TO E-KEYX.
      *     START OAU-FILE KEY NOT < OCH1-KEY.

       READ-OAU1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OAU-PATH TO E-FILE.
      *     MOVE OAU-KEY  TO E-KEYX.
           READ OAU-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OAU1-FILE-NEXT.

       CLOSE-OAU1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OAU-FILE.

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************

           PERFORM CLOSE-FILES.

           IF ( EXT-ROUTE-BUF = "00" )
              PERFORM PRU-OUT.

           IF A15-RECORDS-PROCESSED = 0 AND SQL-RECORDS-PROCESSED = 0
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

       EXIT-PROG.

           MOVE SV-BEG-NAME  TO BEG-NAME.
           MOVE SV-END-NAME  TO END-NAME.
           MOVE SV-BEG-DATE  TO BEG-DATE. 
           MOVE SV-END-DATE  TO END-DATE.
           MOVE SV-BEG-SEQ   TO BEG-SEQ. 
           MOVE SV-END-SEQ   TO END-SEQ.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY AUFILE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
