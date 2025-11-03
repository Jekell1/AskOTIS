       IDENTIFICATION DIVISION.
       PROGRAM-ID.    ALFILE.
       DATE-WRITTEN.  02-01-2024.
       AUTHOR. RON ZAMISKA.
      ******************************************************************
      *
      * DESCRIPTION   : COMPARE S35 ALFILE WITH A15 ALFILE
      *            
      *                 TO BE USED AFTER AN UPDATE
      *                 WILL COMPARE EACH FIELD AND REPORT ANY 
      *                 DIFFERENCES, AS LONG AS FIELD EXIST IN BOTH FILES
      *
      *  2 FILE MATCH PROCESSING.
      *
      *  01-06-25  KC  ADDED 'BTRACK' - PROCESS TRACKING TO PROGRAM.
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      * COPYMEMBER: LIBGL/GLFSAL
           SELECT OAL-FILE ASSIGN OAL-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OAL1-KEY
                  FILE STATUS FILE-STAT.

       COPY "LIBGB/GBFSPR.CPY".

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
        FD  OAL-FILE
           LABEL RECORDS ARE STANDARD.
      *-----------------------------------------------------------------
       COPY "LIBUP/A15SRC/GL01AL.CPY" REPLACING LEADING "AL" BY "OAL".
      *-----------------------------------------------------------------
      * COPYMEMBER: LIBGL/GL01AL
      ***************************************************
      *  GENERAL LEDGER ALLOCATION FILE
      *  KEY 1 - ALLOCATION I.D.
      ***************************************************
      *01  OAL-REC.
      *    03  OAL1-KEY.
      *        05  OAL-ID     PIC X(12).
      *    03  OAL-DESC       PIC X(30).
      *    03  OAL-NO-ACCTS   PIC 99.
      *    03  FILLER         OCCURS 10 TIMES.
      *        05  OAL-ACCT   PIC 9(11)      COMP-3.
      *        05  OAL-PCT    PIC S9(3)V99   COMP-3.
      *    03  OAL-TYPE       PIC X.
      *    03  FILLER         PIC X(29).

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)UT/ALFILE.CBL S35 01/06/25-13:48:12 >".

      *COPY "LIBGB/GB01BR.CPY".
      *COPY "LIBGB/GB01BR_SQL.CPY".
      *COPY "LIBGB/GBWSBR.CPY".
      *COPY "LIBGB/GB01GR.CPY".
      *COPY "LIBGB/GB01GR_SQL.CPY".
      *COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGL/GL01AL.CPY".
       COPY "LIBGL/GL01AL_SQL.CPY".
       COPY "LIBGL/GLWSAL.CPY".

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

      *********************************************************
      *        LIBGL/GLWSAL: ALFILE PATH WORKERS
      *********************************************************
   
       01  OAL-PATH                    PIC X(35) VALUE SPACES.
       01  OAL-ALL-PATH.
           03  OAL-ALL-BASE     PIC X(9).
           03  FILLER           PIC X     VALUE "/".
           03  OAL-ALL-MACHINE  PIC XX.
           03  FILLER           PIC X(10) VALUE
               "/GL/ALFILE".
 
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

       01  PROC-YYMM                   PIC 9(4).
       01  PROC-YYMM-RDF  REDEFINES PROC-YYMM.
           03 PROC-YYMM-YY             PIC 99.
           03 PROC-YYMM-MM             PIC 99.

       01  AL-EOF-SW                   PIC X      VALUE "N".
       01  OAL-EOF-SW                  PIC X      VALUE "N".
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

       01  OAL-USE-KEY.
           03 OAL-USE-KEY-ID           PIC 9(12).
 
       01  AL-USE-KEY.
           03 AL-USE-KEY-ID            PIC 9(12).
 
       01  MACHINE-R1.
           03  FILLER                  PIC X VALUE X"72".
           03  FILLER                  PIC 9 VALUE 1.
       01  MACHINE-R2.
           03  FILLER                  PIC X VALUE X"72".
           03  FILLER                  PIC 9 VALUE 2.

       01  PROCESS-STAT                PIC X(8) VALUE SPACES.
           88  PROCESS-STAT-GOOD                VALUE "00".
           88  PROCESS-STAT-BAD                 VALUE "  " THRU "//"
                                                      "01" THRU "~~".

       01  MISC-CURSOR-STAT            PIC X(2) VALUE SPACES.
           88  MISC-CURSOR-STAT-GOOD            VALUE "00".
           88  MISC-CURSOR-STAT-BAD             VALUE "  " THRU "//"
                                                      "01" THRU "~~".


       01  A15-RECORDS-PROCESSED       PIC 9(9)     VALUE 0.
       01  SQL-RECORDS-PROCESSED       PIC 9(9)     VALUE 0.

       01  STARTING-TIME               PIC X(08)     VALUE SPACES.
       01  SUB                         PIC 9(04)     VALUE ZEROES.

       01  SYSDATE-YYYYMMDD            PIC 9(8)     VALUE ZEROES.

       01  LEGEND                      PIC X(79)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(07)     VALUE "LEGEND.".

       01  LEGEND-BEG-ID.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F6-SCAN, F7-".
           03  FILLER PIC X(30) VALUE "EXIT, ENTER BEGINNING  AL-ID/G".
           03  FILLER PIC X(19) VALUE "ROUP               ".

       01  LEGEND-END-ID.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER ENDIN".
           03  FILLER PIC X(30) VALUE "G  AL-ID NUMBER               ".
           03  FILLER PIC X(19) VALUE "                   ".

      *-----------------------------------------------------------------
       01  LEGEND-ACCEPT-YN.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT             ".
           03  FILLER PIC X(30) VALUE "                              ".
           03  FILLER PIC X(19) VALUE "     ACCEPT (Y/N): ".

       01  SOURCE-PATH-LIST            PIC X(07)     VALUE "S-PATH.".

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  SOURCE-PATH             PIC X(09).
           03  BEG-ID                  PIC X(12).
           03  END-ID                  PIC X(12).

       01  VERSION-CONTROL             PIC X(01)     VALUE "C".

      *-----------------------------------------------------------------
       01  SV-BEG-ID                   PIC X(12)     VALUE SPACES.
       01  SV-END-ID                   PIC X(12)     VALUE SPACES. 
       
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
           "DAILY CHANGES (ALFILE) COMPARE A15 TO S35".

       01  HEADER-LINE-03.
           03  FILLER                PIC X(12)  VALUE "AL-ID        ".
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
           03  D01-ID                         PIC X(12).
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
       COPY "LIBUT/ALFILE_DEF.CPY".
       COPY "LIBUT/ALFILE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/ALFILE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE OAL-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-AL1-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE OAL-FILE.
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
           PERFORM PROCESS-AL-DRIVER.

           PERFORM RANGE-LINES.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

           MOVE STAT---GOOD                TO PROCESS-STAT.

           MOVE FORM-PATHNAME              TO FORM-PATH.
           MOVE "ALFILE" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE ALFILE-QUEUE-POS TO Q-POS.
           MOVE ALFILE-COPIES-POS TO C-POS.
           MOVE ALFILE-PRU-POS   TO P-POS.
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

           PERFORM START-MISC-CSR.
           PERFORM READ-MISC-CSR-NEXT.
           PERFORM CLOSE-MISC-CSR.

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
              GO TO ENTER-BEG-ID.

       ENTER-SOURCE-PATH.
       
           IF SOURCE-PATH = SPACES
              MOVE "A15_PATH" TO GETENV-BUF
              PERFORM GETENV-CALL
              IF ( STAT-GOOD )
                 MOVE GETENV-BUF TO SOURCE-PATH
              ELSE
              MOVE "/USR/DATA" TO SOURCE-PATH
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

           MOVE "E  A120BEG-ID." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F3-KEY ) GO TO ENTER-ALL-ID.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-ID.

           MOVE VDUBUF TO BEG-ID.

      *-----------------------------------------------------------------
       ENTER-END-ID.

           MOVE LEGEND-END-ID TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A120END-ID." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-ID.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-ID.

           IF ( VDUBUF = SPACES )
              GO TO ENTER-END-ID.

           MOVE VDUBUF TO END-ID.

           IF ( BEG-ID > END-ID )
              MOVE "INVALID ID RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-ID.    

           GO TO ENTER-PARAMETERS-EXIT.

      *-----------------------------------------------------------------
       ENTER-ALL-ID.

           MOVE "S  A000BEG-ID."     TO VDU.
           MOVE SPACES               TO BEG-ID  VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A000END-ID."     TO VDU.
           MOVE 'ZZZZZZZZZZZZ'       TO END-ID  VDUBUF.
           PERFORM SCREENIO.

       ENTER-PARAMETERS-END.

       ENTER-PARAMETERS-EXIT.
           EXIT.

      ****************************************
       EO-EXEC-AUDIT SECTION.
      ****************************************

      ****************************************
       DISPLAY-PARAMETERS SECTION.
      ****************************************

           MOVE "S  A000BEG-ID."        TO VDU.
           MOVE BEG-ID                  TO VDUBUF.  
           PERFORM SCREENIO.

           MOVE "S  A000END-ID."        TO VDU.
           MOVE END-ID                  TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A090S-PATH."        TO VDU.     
           MOVE SOURCE-PATH             TO VDUBUF.
           PERFORM SCREENIO.


       DISPLAY-PARAMETERS-EXIT.
           EXIT.

      *******************************************************
       PROCESS-AL-DRIVER SECTION.
      *********************************************************

           MOVE 9999 TO PROGID-SLEEP-COUNTER.
      *    PERFORM SEND-PROGRESS-INDICATOR.

      *  OPEN/READ S35 - ALFILE

      *    MOVE EXT-FILPATH-BASE  TO FILPATH-BASE.
           MOVE BEG-ID            TO QAL1-WBEG-ID.
           MOVE END-ID            TO QAL1-WEND-ID.

           MOVE "PROCESSING S35 DATA" TO BTRACK-MESS.
           PERFORM BTRACK-GLOBAL.

           MOVE "N"  TO AL-EOF-SW.
           PERFORM OPEN-AL1-FILE.
           PERFORM START-AL1-FILE.  
           PERFORM PROCESS-READ-AL1. 

      *  OPEN/READ A15 - ALFILE          
      * THIS IS A GLOBAL FILE BUT FOLLOWING SAME LOGIC.
      * ONLY PERFORMS THIS SECTION ONCE

           MOVE SOURCE-PATH           TO OAL-ALL-BASE.
           MOVE MACHINE-R1            TO OAL-ALL-MACHINE.
           MOVE OAL-ALL-PATH          TO OAL-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE "MISSING A15 FILE" TO D01-TEXT
              MOVE SPACES             TO D01-ID 
              MOVE OAL-ALL-PATH       TO D01-A15
              MOVE SPACES             TO D01-S35
              PERFORM WRITE-DETAIL-LINE
              GO TO EXIT-PROCESS-READ-AL1.

           MOVE "PROCESSING A15 DATA" TO BTRACK-MESS.
           PERFORM BTRACK-GLOBAL.

           PERFORM OPEN-OAL1-FILE.
           PERFORM PROCESS-READ-OAL1.


       CONTINUE-AL-DRIVER.

           IF AL-EOF-SW  = "Y" AND
              OAL-EOF-SW = "Y"
              GO TO PROCESS-AL-DRIVER-END.              

           IF OAL-ID < AL-ID 
              MOVE "MISSING S35 RECORD" TO D01-TEXT
              MOVE OAL-ID               TO D01-ID 
              PERFORM WRITE-DETAIL-LINE-ERROR
              PERFORM PROCESS-READ-OAL1 
              GO TO CONTINUE-AL-DRIVER.

           IF AL-ID < OAL-ID 
              MOVE "MISSING A15 RECORD" TO D01-TEXT
              MOVE AL-ID                TO D01-ID
              PERFORM WRITE-DETAIL-LINE-ERROR 
              PERFORM PROCESS-READ-AL1 
              GO TO CONTINUE-AL-DRIVER.

            PERFORM COMPARE-FIELDS.

            PERFORM PROCESS-READ-OAL1. 
            PERFORM PROCESS-READ-AL1. 

            GO TO CONTINUE-AL-DRIVER.

       PROCESS-AL-DRIVER-END.
           EXIT. 

       PROCESS-READ-AL1 SECTION.
           PERFORM READ-AL1-FILE-NEXT.
           IF IO-BAD
              MOVE "ZZZZZZZZZZZZ" TO AL-ID 
              MOVE "Y"            TO AL-EOF-SW
           ELSE
              ADD 1               TO SQL-RECORDS-PROCESSED.

       EXIT-PROCESS-READ-AL1.
           EXIT.

       PROCESS-READ-OAL1 SECTION.
           PERFORM READ-OAL1-FILE-NEXT.
           IF IO-BAD
              MOVE 'ZZZZZZZZZZZZ' TO OAL-ID
              MOVE "Y"            TO OAL-EOF-SW
           ELSE
              ADD 1               TO A15-RECORDS-PROCESSED.

       EXIT-PROCESS-READ-OAL1.
           EXIT.

      ******************************************************************
       COMPARE-FIELDS SECTION.
      ******************************************************************

           MOVE AL-ID             TO D01-ID.

           IF AL-DESC          NOT = OAL-DESC 
              MOVE "AL-DESC"      TO D01-TEXT
              MOVE OAL-DESC       TO D01-A15
              MOVE AL-DESC        TO D01-S35
              MOVE AL-ID          TO D01-ID 
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF AL-NO-ACCTS      NOT = OAL-NO-ACCTS 
              MOVE "AL-NO-ACCTS"  TO D01-TEXT
              MOVE OAL-NO-ACCTS   TO D01-A15
              MOVE AL-NO-ACCTS    TO D01-S35
              MOVE AL-ID          TO D01-ID 
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
             IF AL-ACCT(SUB)             NOT = OAL-ACCT(SUB)
                STRING "AL-ACCT(",SUB,")" INTO D01-TEXT 
                MOVE OAL-ACCT(SUB)          TO D01-A15
                MOVE AL-ACCT(SUB)           TO D01-S35
                MOVE AL-ID                  TO D01-ID 
                PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
             IF AL-PCT(SUB)              NOT = OAL-PCT(SUB)
                STRING "AL-PCT(",SUB,")"  INTO D01-TEXT 
                MOVE OAL-PCT(SUB)           TO D01-A15-NUM
                MOVE AL-PCT(SUB)            TO D01-S35-NUM 
                MOVE AL-ID                  TO D01-ID
                PERFORM WRITE-DETAIL-LINE-ERROR.

           IF AL-TYPE          NOT = OAL-TYPE 
              MOVE "AL-TYPE"      TO D01-TEXT
              MOVE OAL-TYPE       TO D01-A15
              MOVE AL-TYPE        TO D01-S35
              MOVE AL-ID          TO D01-ID
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

           MOVE "BEGINNING ID :  "                    TO R01-PROMPT.
           MOVE BEG-ID                                TO R01-TEXT.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING ID :     "                    TO R01-PROMPT.
           MOVE END-ID                                TO R01-TEXT.
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
           MOVE EXT-CONAME-CONAME TO ALFILE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO ALFILE-SYSDATE.
           DISPLAY ALFILE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE ALFILE-FORM-FIELDS TO WR-BUF.
           MOVE ALFILE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE ALFILE-HELP--FILE TO HELP-LINK-FILE.
           MOVE ALFILE-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/ALFILE_EVA.CPY".
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
       COPY "LIBGL/GLALGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGL/GLAL1RN.CPY".

       OPEN-OAL1-FILE.
           PERFORM OPEN-IT.
           MOVE OAL-PATH TO E-FILE.
           OPEN INPUT OAL-FILE.
           IF IO-FG = 8
              GO TO OPEN-OAL1-FILE.
           IF IO-FG = 7
              CLOSE OAL-FILE
              GO TO OPEN-OAL1-FILE.
  
      * START-OCH1-FILE.    * CAN'T START SEQUENTIAL FILE
      *     PERFORM START-IT.
      *     MOVE OAL-PATH TO E-FILE.
      *     MOVE OAL-KEY  TO E-KEYX.
      *     START OAL-FILE KEY NOT < OCH1-KEY.

       READ-OAL1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OAL-PATH TO E-FILE.
      *     MOVE OAL-KEY  TO E-KEYX.
           READ OAL-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OAL1-FILE-NEXT.

       CLOSE-OAL1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OAL-FILE.

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
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY ALFILE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
