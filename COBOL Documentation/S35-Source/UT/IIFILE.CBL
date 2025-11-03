       IDENTIFICATION DIVISION.
       PROGRAM-ID.    IIFILE.
       DATE-WRITTEN.  02-22-2024.
       AUTHOR. MARK BOOMGAARD.
      ******************************************************************
      *
      * DESCRIPTION   : COMPARE S35 IIFILE WITH A15 IIFILE
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

      * COPYMEMBER: LIBSP/SPFSII
      *******************************************************************
      *  IRS IDENTIFICATION FILE
      *  REV:
      *  BLV 010125 CHANGE ASSIGN FROM HARD-CODED PATH TO II-PATH
      *             NOTE, SOMEHOW IN A15 THIS HARD-CODED PATH WAS CHANGED 
      *             FROM GLOBAL GB TO LOCAL GB, I AM CHANGING IT BACK TO
      *             GLOBAL GB NOW
      *******************************************************************
           SELECT OII-FILE ASSIGN OII-PATH
                  ORGANIZATION RELATIVE
                  ACCESS MODE DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RELATIVE KEY OII-KEY
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  OII-FILE
           LABEL RECORDS ARE STANDARD.
      *-----------------------------------------------------------------
       COPY "LIBUP/A15SRC/SP01II.CPY" REPLACING LEADING "II" BY "OII".
      *-----------------------------------------------------------------

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/IIFILE S35 02/08/24-15:21:09 >".

       COPY "LIBSP/SP01II.CPY".
       COPY "LIBSP/SP01II_SQL.CPY".
       COPY "LIBSP/SPWSII.CPY".

       01  OII-KEY              PIC 99 VALUE 1.

       01  OII-PATH             PIC X(35) VALUE SPACES.
       01  OII-ALL-PATH.
           03  OII-ALL-BASE     PIC X(9).
           03  FILLER           PIC X     VALUE "/".
           03  OII-ALL-MACHINE  PIC XX.
           03  FILLER           PIC X(10) VALUE
               "/GB/IIFILE".

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
       01  II-EOF-SW                   PIC X      VALUE "N".
       01  OII-EOF-SW                  PIC X      VALUE "N".
       01  A15-FILE-FOUND-SW           PIC X      VALUE "N".
       01  TOTAL-ERRORS-CTR            PIC 9(9)   VALUE 0.
       01  PRINTING-RANGES-SW          PIC X      VALUE "N".
       01  SUB1                        PIC 9      VALUE 0.
       01  SUB2                        PIC 99     VALUE 0.

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

       01  LEGEND-BEG-KEY.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F7-EXIT, ENT".
           03  FILLER PIC X(30) VALUE "ER BEGINNING KEY              ".
           03  FILLER PIC X(19) VALUE "                   ".

       01  LEGEND-END-KEY.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER ENDIN".
           03  FILLER PIC X(30) VALUE "G KEY                         ".
           03  FILLER PIC X(19) VALUE "                   ".

      *-----------------------------------------------------------------
       01  LEGEND-ACCEPT-YN.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT             ".
           03  FILLER PIC X(30) VALUE "                              ".
           03  FILLER PIC X(19) VALUE "     ACCEPT (Y/N): ".

       01  SOURCE-PATH-LIST            PIC X(07)     VALUE "S-PATH.".

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  SOURCE-PATH             PIC X(9).
           03  BEG-KEY                 PIC 99.
           03  END-KEY                 PIC 99.

       01  VERSION-CONTROL             PIC X(01)     VALUE "A".

      *-----------------------------------------------------------------
       01  SV-BEG-KEY                  PIC 99        VALUE 0.
       01  SV-END-KEY                  PIC 99        VALUE 0.

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
           "IRS IDENTIFICATION (IIFILE) COMPARE A15 TO S35".

       01  HEADER-LINE-03.
           03  FILLER                PIC X(4)  VALUE "KEY ".
           03  FILLER                PIC X(2)  VALUE SPACES.
           03  FILLER                PIC X(22) VALUE "DESCRIPTION".
           03  FILLER                PIC X(27) VALUE "A15".
           03  FILLER                PIC X(3)  VALUE "S35".

      ******************************************************************
      *
      *    D E T A I L - L I N E - W O R K E R S
      *
      ******************************************************************
       01  DETAIL-LINE                        PIC X(100)   VALUE SPACES.

       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  FILLER                         PIC X(1).
           03  D01-KEY                        PIC 99.
           03  FILLER                         PIC X(3).
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
       COPY "LIBUT/IIFILE_DEF.CPY".
       COPY "LIBUT/IIFILE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/IIFILE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE OII-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-II-FILE.
           PERFORM CLOSE-OII-FILE.
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
           MOVE "IIFILE" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE IIFILE-QUEUE-POS  TO Q-POS.
           MOVE IIFILE-COPIES-POS TO C-POS.
           MOVE IIFILE-PRU-POS    TO P-POS.
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

           MOVE BEG-KEY              TO SV-BEG-KEY.
           MOVE END-KEY              TO SV-END-KEY.

           PERFORM OPEN-II-FILE.
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
              GO TO ENTER-END-KEY.

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

      *-----------------------------------------------------------------
       ENTER-BEG-KEY.

           MOVE LEGEND-BEG-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN020BEG-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F3-KEY ) GO TO ENTER-ALL-KEY.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-KEY.

           MOVE VDUNUM0 TO BEG-KEY.

      *-----------------------------------------------------------------
       ENTER-END-KEY.

           MOVE LEGEND-END-KEY TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN020END-KEY." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-KEY.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-KEY.

           MOVE VDUNUM0 TO END-KEY.

           IF ( BEG-KEY > END-KEY )
              MOVE "INVALID KEY RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-KEY.

           GO TO ENTER-PARAMETERS-END.

      *-----------------------------------------------------------------
       ENTER-ALL-KEY.

           MOVE "SNNN020BEG-KEY." TO VDU.
           MOVE 0                 TO BEG-KEY VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020END-KEY." TO VDU.
           MOVE 99                TO END-KEY VDUNUM0.
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

           MOVE "SNNN020BEG-KEY." TO VDU.
           MOVE BEG-KEY              TO VDUNUM0.
           PERFORM SCREENIO

           MOVE "SNNN020END-KEY." TO VDU.
           MOVE END-KEY              TO VDUNUM0.
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

           MOVE "B"                        TO II-PATH-OVERRIDE.

           MOVE BEG-KEY  TO QII-WBEG-KEY.
           MOVE END-KEY  TO QII-WEND-KEY.

           MOVE "PROCESSING S35 DATA" TO BTRACK-MESS.
           PERFORM BTRACK-GLOBAL.
 
           PERFORM OPEN-II-FILE.
           PERFORM START-II-FILE.
           PERFORM M-READ-II.

           MOVE SOURCE-PATH   TO OII-ALL-BASE.
           MOVE MACHINE-R1    TO OII-ALL-MACHINE.
           MOVE OII-ALL-PATH  TO OII-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE "MISSING A15 FILE" TO D01-TEXT
              MOVE OII-ALL-PATH       TO D01-A15
              MOVE SPACES             TO D01-S35
              PERFORM WRITE-DETAIL-LINE
              GO TO PROCESS-DRIVER-END.

           MOVE "PROCESSING A15 DATA" TO BTRACK-MESS.  
           PERFORM BTRACK-GLOBAL. 

           PERFORM OPEN-OII-FILE.
           PERFORM M-READ-OII.

           PERFORM PROCESS-NEXT-II
              UNTIL II-EOF-SW = "Y" AND OII-EOF-SW = "Y".
 
           PERFORM CLOSE-II-FILE.
           PERFORM CLOSE-OII-FILE.

           GO TO PROCESS-DRIVER-END.

       PROCESS-NEXT-II.

           PERFORM SEND-PROGRESS-INDICATOR.

           PERFORM COMPARE-FIELDS.

           IF OII-KEY < II-KEY
              PERFORM M-READ-OII
           ELSE
              IF OII-KEY > II-KEY
                 PERFORM M-READ-II
              ELSE
                 PERFORM M-READ-OII
                 PERFORM M-READ-II.

       PROCESS-DRIVER-END.
           EXIT. 

       M-READ-II SECTION.
           PERFORM READ-II-FILE-NEXT.
           IF IO-BAD
              MOVE 99    TO II-KEY
              MOVE "Y"   TO II-EOF-SW
           ELSE
              ADD 1 TO SQL-RECORDS-PROCESSED.

       EXIT-M-READ-II.
           EXIT.

       M-READ-OII SECTION.
           PERFORM READ-OII-FILE-NEXT.
           IF IO-BAD  
              MOVE 99  TO OII-KEY
              MOVE "Y" TO OII-EOF-SW
           ELSE
              ADD 1 TO A15-RECORDS-PROCESSED.
 
       EXIT-M-READ-OII.
           EXIT.

      ******************************************************************
       COMPARE-FIELDS SECTION.
      ******************************************************************

           IF OII-KEY < II-KEY
              MOVE "MISSING S35 RECORD" TO D01-TEXT
              MOVE OII-KEY              TO D01-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.

           IF OII-KEY > II-KEY
              MOVE "MISSING A15 RECORD" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.

           IF II-EMPLOYER-ID NOT = OII-EMPLOYER-ID
              MOVE "II-EMPLOYER-ID" TO D01-TEXT
              MOVE OII-EMPLOYER-ID  TO D01-A15
              MOVE II-EMPLOYER-ID   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF II-NAME-CONTROL-NO NOT = OII-NAME-CONTROL-NO
              MOVE "II-NAME-CONTROL-NO" TO D01-TEXT
              MOVE OII-NAME-CONTROL-NO  TO D01-A15
              MOVE II-NAME-CONTROL-NO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF II-TRANS-CONTROL-CODE NOT = OII-TRANS-CONTROL-CODE
              MOVE "II-TRANS-CONTROL-CODE"  TO D01-TEXT
              MOVE OII-TRANS-CONTROL-CODE   TO D01-A15
              MOVE II-TRANS-CONTROL-CODE    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF II-FOREIGN-CORP NOT = OII-FOREIGN-CORP
              MOVE "II-FOREIGN-CORP" TO D01-TEXT
              MOVE OII-FOREIGN-CORP  TO D01-A15
              MOVE II-FOREIGN-CORP   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF II-TRANSFER-AGENT-FG NOT = OII-TRANSFER-AGENT-FG
              MOVE "II-TRANSFER-AGENT-FG" TO D01-TEXT
              MOVE OII-TRANSFER-AGENT-FG  TO D01-A15
              MOVE II-TRANSFER-AGENT-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.
 
           IF II-PAYER-NAME NOT = OII-PAYER-NAME
              MOVE "II-PAYER-NAME"   TO D01-TEXT
              MOVE OII-PAYER-NAME    TO D01-A15
              MOVE II-PAYER-NAME     TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.
 
           IF II-SECOND-PAYER NOT = OII-SECOND-PAYER
              MOVE "II-SECOND-PAYER" TO D01-TEXT
              MOVE OII-SECOND-PAYER  TO D01-A15 
              MOVE II-SECOND-PAYER   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR. 
 
           IF II-PAYER-ADDRESS NOT = OII-PAYER-ADDRESS
              MOVE "II-PAYER-ADDRESS" TO D01-TEXT
              MOVE OII-PAYER-ADDRESS  TO D01-A15 
              MOVE II-PAYER-ADDRESS   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.
 
           IF II-PAYER-CITY NOT = OII-PAYER-CITY
              MOVE "II-PAYER-CITY" TO D01-TEXT
              MOVE OII-PAYER-CITY  TO D01-A15 
              MOVE II-PAYER-CITY   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.
 
           IF II-PAYER-STATE NOT = OII-PAYER-STATE
              MOVE "II-PAYER-STATE" TO D01-TEXT
              MOVE OII-PAYER-STATE   TO D01-A15 
              MOVE II-PAYER-STATE   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.
 
           IF II-PAYER-ZIP NOT = OII-PAYER-ZIP
              MOVE "II-PAYER-ZIP" TO D01-TEXT
              MOVE OII-PAYER-ZIP   TO D01-A15 
              MOVE II-PAYER-ZIP   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.
 
            IF II-PAYER-PHONE NOT = OII-PAYER-PHONE
              MOVE "II-PAYER-PHONE" TO D01-TEXT
              MOVE OII-PAYER-PHONE  TO D01-A15 
              MOVE II-PAYER-PHONE   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF II-PAYER-EXT NOT = OII-PAYER-EXT
              MOVE "II-PAYER-EXT" TO D01-TEXT
              MOVE OII-PAYER-EXT  TO D01-A15 
              MOVE II-PAYER-EXT   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF II-CONTACT NOT = OII-CONTACT
              MOVE "II-CONTACT" TO D01-TEXT
              MOVE OII-CONTACT  TO D01-A15 
              MOVE II-CONTACT   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF II-CONTACT-EMAIL NOT = OII-CONTACT-EMAIL
              MOVE "II-CONTACT-EMAIL" TO D01-TEXT
              MOVE OII-CONTACT-EMAIL  TO D01-A15 
              MOVE II-CONTACT-EMAIL   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OII-TAX-YEAR NOT NUMERIC
              MOVE 0  TO OII-TAX-YEAR.
           IF II-TAX-YEAR NOT = OII-TAX-YEAR
              MOVE "II-TAX-YEAR" TO D01-TEXT
              MOVE OII-TAX-YEAR  TO D01-A15 
              MOVE II-TAX-YEAR   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF II-CURRENT-LAST NOT = OII-CURRENT-LAST
              MOVE "II-CURRENT-LAST" TO D01-TEXT
              MOVE OII-CURRENT-LAST  TO D01-A15 
              MOVE II-CURRENT-LAST   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF II-LASER-FORM NOT = OII-LASER-FORM
              MOVE "II-LASER-FORM" TO D01-TEXT
              MOVE OII-LASER-FORM  TO D01-A15 
              MOVE II-LASER-FORM   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

        EXIT-COMPARE-FIELDS.
           EXIT.

       WRITE-DETAIL-LINE-ERROR-KEY SECTION.

           MOVE II-KEY TO D01-KEY.

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

           MOVE "BEGINNING KEY                 : " TO R01-PROMPT.
           MOVE BEG-KEY                            TO R01-NUMERIC.
           MOVE 01                                 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING KEY                    : " TO R01-PROMPT.
           MOVE END-KEY                            TO R01-NUMERIC.
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
           MOVE EXT-CONAME-CONAME TO IIFILE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO IIFILE-SYSDATE.
           DISPLAY IIFILE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE IIFILE-FORM-FIELDS TO WR-BUF.
           MOVE IIFILE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE IIFILE-HELP--FILE TO HELP-LINK-FILE.
           MOVE IIFILE-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/IIFILE_EVA.CPY".
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
       COPY "LIBSP/SPIIGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBSP/SPIIRN.CPY".

       OPEN-OII-FILE.
           PERFORM OPEN-IT.
           MOVE OII-PATH TO E-FILE.
           OPEN INPUT OII-FILE.
           IF IO-FG = 8
              GO TO OPEN-OII-FILE.
           IF IO-FG = 7
              CLOSE OII-FILE
              GO TO OPEN-OII-FILE.

        READ-OII-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OII-PATH TO E-FILE.
           READ OII-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OII-FILE-NEXT.

       CLOSE-OII-FILE.
           PERFORM CLOSE-IT.
           CLOSE OII-FILE.

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

           IF ( SV-BEG-KEY NOT = 0 )
              MOVE SV-BEG-KEY        TO BEG-KEY
              MOVE SV-END-KEY        TO END-KEY.

           IF ( EXT-ROUTE-BUF = 0 )
              MOVE EXIT-PATH               TO FORM-NAM
              MOVE FORM-PATH               TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY IIFILE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
