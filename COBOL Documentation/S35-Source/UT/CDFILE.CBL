       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CDFILE.
       DATE-WRITTEN.  02-13-2024.
       AUTHOR. MARK BOOMGAARD.
      ******************************************************************
      *
      * DESCRIPTION   : COMPARE S35 CDFILE WITH A15 CDFILE
      *            
      *                 TO BE USED AFTER AN UPDATE
      *                 WILL COMPARE EACH FIELD AND REPORT ANY 
      *                 DIFFERENCES, AS LONG AS FIELD EXIST IN BOTH FILES
      *
      *  2 FILE MATCH PROCESSING.
      *
      *
      *  01-07-25  RMZ ADDED 'BTRACK' - PROCESS TRACKING TO PROGRAM.
      *  02-25-25  MB  ADDED CHECK FOR 'A2' A15 CODE                 
      *  02-28-25  MB  CHANGED F3 ALL FOR TYPE TO SPACE/ZZ - WAS AA/ZZ                 
      *                 AND 'A2' WAS NOT RETURNED BY A2 < AA IN SQL SERVER
      *  04-16-25  MB  USE 0 FOR OCD-CT-INSTAXAMT(X) AND OCD-CY-INSTAXAMT(X)
      *                 WHEN = 20.2
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

      * COPYMEMBER: LIBLP/LPFSCD
           SELECT OCD-FILE ASSIGN TO OCD-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OCD1-KEY
                  FILE STATUS FILE-STAT.

           SELECT WK-FILE ASSIGN TO WK-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK-KEY
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  OCD-FILE
           LABEL RECORDS ARE STANDARD.
      *-----------------------------------------------------------------
       COPY "LIBUP/A15SRC/LP01CD.CPY" REPLACING LEADING "CD" BY "OCD".
      *-----------------------------------------------------------------

       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
              05  WK-KEY-TYPE         PIC XX.
              05  WK-KEY-CODE         PIC X(8).
           03  WK-DATA                PIC X(OCD-SIZE).

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)UT/CDFILE.CBL S35 01/02/25-12:12:56 >".

       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
 
       01  OCD-PATH             PIC X(35) VALUE SPACES.
       01  OCD-ALL-PATH.
           03  OCD-ALL-BASE     PIC X(9).
           03  FILLER           PIC X     VALUE "/".
           03  OCD-ALL-MACHINE  PIC XX.
           03  FILLER           PIC X(10) VALUE
               "/LP/CDFILE".

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
       01  CD-EOF-SW                   PIC X      VALUE "N".
       01  OCD-EOF-SW                  PIC X      VALUE "N".
       01  A15-FILE-FOUND-SW           PIC X      VALUE "N".
       01  TOTAL-ERRORS-CTR            PIC 9(9)   VALUE 0.
       01  PRINTING-RANGES-SW          PIC X      VALUE "N".
       01  SUB1                        PIC 9      VALUE 0.
       01  SUB2                        PIC 99     VALUE 0.
       01  WK-EOF-SW                   PIC X      VALUE "N".
       01  WK-RECS-CTR                 PIC 9(9)   VALUE 0.
       01  WS-NUM                      PIC 9(13)V999  VALUE 0.

       01  WS-N2-TO-CHAR-NUM              PIC 99.
       01  WS-N2-TO-CHAR-CHAR         REDEFINES WS-N2-TO-CHAR-NUM
                                           PIC XX.

       01  WS-N5-TO-CHAR-NUM              PIC 99999.
       01  WS-N5-TO-CHAR-CHAR         REDEFINES WS-N5-TO-CHAR-NUM
                                           PIC XXXXX.

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

       01  LEGEND-BEG-TYPE.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F7-EXIT, EN".
           03  FILLER PIC X(30) VALUE "TER BEGINNING TYPE           ".
           03  FILLER PIC X(19) VALUE "                   ".

       01  LEGEND-END-TYPE.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER ENDIN".
           03  FILLER PIC X(30) VALUE "G TYPE                        ".
           03  FILLER PIC X(19) VALUE "                   ".

      *-----------------------------------------------------------------
       01  LEGEND-ACCEPT-YN.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT             ".
           03  FILLER PIC X(30) VALUE "                              ".
           03  FILLER PIC X(19) VALUE "     ACCEPT (Y/N): ".

       01  SOURCE-PATH-LIST            PIC X(07)     VALUE "S-PATH.".

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  SOURCE-PATH             PIC X(9).
           03  BEG-TYPE                PIC XX.
           03  END-TYPE                PIC XX.

       01  VERSION-CONTROL             PIC X(01)     VALUE "A".

      *-----------------------------------------------------------------
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
           "CODE DESCRIPTION MASTER FILE (CDFILE) COMPARE A15 TO S35".

       01  HEADER-LINE-03.
           03  FILLER                PIC X(4)  VALUE "TYPE".
           03  FILLER                PIC X(2)  VALUE SPACES.
           03  FILLER                PIC X(8)  VALUE "  CODE  ".
           03  FILLER                PIC X(3)  VALUE SPACES.
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
           03  FILLER                         PIC X(1).
           03  D01-TYPE                       PIC X(2).
           03  FILLER                         PIC X(3).
           03  D01-CODE                       PIC X(8).
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
       COPY "LIBUT/CDFILE_DEF.CPY".
       COPY "LIBUT/CDFILE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/CDFILE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE OCD-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-OCD1-FILE.
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
           MOVE "CDFILE" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE CDFILE-QUEUE-POS  TO Q-POS.
           MOVE CDFILE-COPIES-POS TO C-POS.
           MOVE CDFILE-PRU-POS    TO P-POS.
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

           PERFORM OPEN-CD1-FILE.
           PERFORM OPEN-PR-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WK-PATH.

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
              GO TO ENTER-END-TYPE.

       ENTER-SOURCE-PATH.

           IF SOURCE-PATH = SPACES
              MOVE "A15_PATH" TO GETENV-BUF
              PERFORM GETENV-CALL
              IF ( STAT-GOOD )
                 MOVE GETENV-BUF TO SOURCE-PATH
              ELSE
                 MOVE "/US2/DA15" TO SOURCE-PATH
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

       ENTER-BEG-TYPE.

           MOVE LEGEND-BEG-TYPE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A020BEG-TYPE." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F3-KEY ) GO TO ENTER-ALL-TYPE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-TYPE.

           MOVE VDUBUF TO BEG-TYPE.

      *-----------------------------------------------------------------
       ENTER-END-TYPE.

           MOVE LEGEND-END-TYPE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A020END-TYPE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-TYPE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-TYPE.

           MOVE VDUBUF TO END-TYPE.

           IF ( BEG-TYPE > END-TYPE )
              MOVE "INVALID TYPE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-TYPE.

           GO TO ENTER-PARAMETERS-END.

      *-----------------------------------------------------------------
       ENTER-ALL-TYPE.

           MOVE "S RA000BEG-TYPE." TO VDU.
           MOVE SPACES             TO BEG-TYPE VDUBUF.
           PERFORM SCREENIO.

           MOVE "S RA000END-TYPE." TO VDU.
           MOVE "ZZ"               TO END-TYPE VDUBUF.
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

           MOVE "S  A090S-PATH."           TO VDU.
           MOVE SOURCE-PATH                TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A020BEG-TYPE." TO VDU.
           MOVE BEG-TYPE           TO VDUBUF.
           PERFORM SCREENIO

           MOVE "S  A020END-TYPE." TO VDU.
           MOVE END-TYPE           TO VDUBUF.
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
           MOVE "B"                        TO CD-PATH-OVERRIDE.

           MOVE "N"       TO OCD-EOF-SW CD-EOF-SW WK-EOF-SW.

           MOVE BEG-TYPE  TO QCD1-WBEG-TYPE.
           MOVE END-TYPE  TO QCD1-WEND-TYPE.
           MOVE SPACES    TO QCD1-WBEG-CODE.
           MOVE ALL 'Z'   TO QCD1-WEND-CODE.

           MOVE "PROCESSING S35 DATA" TO BTRACK-MESS.
           PERFORM BTRACK-GLOBAL.

           PERFORM OPEN-CD1-FILE.
           PERFORM START-CD1-FILE.
           PERFORM M-READ-CD.

      * DELETE/EMPTY TEMP FILE IF EXIST
           PERFORM OPEN-WK1-FILE-OUTPUT.
           PERFORM CLOSE-WK1-FILE.
           PERFORM OPEN-WK1-FILE.

      *** MUST LOAD R1 AND R2 CDFILE ***
      *** DO NOT REWRITE ***
           MOVE SOURCE-PATH   TO OCD-ALL-BASE.
           MOVE MACHINE-R1    TO OCD-ALL-MACHINE.
           MOVE OCD-ALL-PATH  TO OCD-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE "MISSING R1 FILE" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO PROCESS-DRIVER-END.

           MOVE "PROCESSING A15 DATA" TO BTRACK-MESS.  
           PERFORM BTRACK-GLOBAL.

           MOVE "N" TO OCD-EOF-SW.
           PERFORM OPEN-OCD1-FILE.
           PERFORM LOAD-WORK-FILE.
           PERFORM CLOSE-OCD1-FILE. 
 
      *** LOAD R2 DO NOT UPDATE IF REC ALREADY IN WKFILE ***
           MOVE MACHINE-R2   TO OCD-ALL-MACHINE.
           MOVE OCD-ALL-PATH TO OCD-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE "MISSING R2 FILE" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO PROCESS-DRIVER-END.

           MOVE "N" TO OCD-EOF-SW.
           PERFORM OPEN-OCD1-FILE.
           PERFORM LOAD-WORK-FILE.
           PERFORM CLOSE-OCD1-FILE. 

           PERFORM CLOSE-WK1-FILE.
 
           PERFORM OPEN-WK1-FILE.
           PERFORM M-READ-WK1.
      D    STOP MESS.
           PERFORM PROCESS-NEXT-ST
              UNTIL CD-EOF-SW = "Y" AND WK-EOF-SW = "Y".
 
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-WK1-FILE.

           GO TO PROCESS-DRIVER-END.

       PROCESS-NEXT-ST.

           PERFORM SEND-PROGRESS-INDICATOR.
      *     PERFORM TRAP-INTERRUPT.

           PERFORM COMPARE-FIELDS.

           IF WK-KEY < CD1-KEY
              PERFORM M-READ-WK1
           ELSE
              IF WK-KEY > CD1-KEY
                 PERFORM M-READ-CD
              ELSE
                 PERFORM M-READ-WK1
                 PERFORM M-READ-CD.

       PROCESS-DRIVER-END.
           EXIT. 

       LOAD-WORK-FILE SECTION.
           PERFORM M-READ-OCD1.
           IF OCD-EOF-SW = "Y"
              GO TO LOAD-WORK-FILE-END.

           IF OCD-TYPE = "CN" OR "GC"
              GO TO LOAD-WORK-FILE.

           IF BEG-TYPE = SPACES AND END-TYPE = "ZZ"
              NEXT SENTENCE
           ELSE 
              IF OCD-TYPE < BEG-TYPE OR OCD-TYPE > END-TYPE
                 GO TO LOAD-WORK-FILE.

           INITIALIZE WK-REC.

           MOVE OCD1-KEY TO OCD-ACTIONKEY.
           IF OCD-ACTIONTYPE = "{"
              MOVE "AC"           TO WK-KEY-TYPE
              MOVE OCD-ACTIONCODE TO WK-KEY-CODE.

           IF OCD-TYPE = "AT" OR "CH" OR "CO" OR "ML" OR "PH" OR "PU"
                     OR "RE" OR "SR"
              MOVE OCD-TYPE  TO WK-KEY-TYPE
              MOVE OCD-CODE  TO WK-KEY-CODE.

           IF OCD-TYPE = "A2"
             MOVE OCD-REC      TO OCD-BW-REC
             MOVE OCD-BW-TYPE  TO WK-KEY-TYPE
             MOVE OCD-BW-CODE  TO WK-KEY-CODE.

           IF OCD-TYPE = "BR"
             MOVE OCD-REC TO OCD-BR-REC
             MOVE OCD-BR-TYPE  TO WK-KEY-TYPE
             MOVE OCD-BR-REFCD TO WK-KEY-CODE.

           IF OCD-TYPE = "BS"
             MOVE OCD-REC      TO OCD-BS-REC
             MOVE OCD-BS-TYPE  TO WK-KEY-TYPE
             MOVE OCD-BS-REFNO TO WK-KEY-CODE.

           IF OCD-TYPE = "BW"
             MOVE OCD-REC      TO OCD-BW-REC
             MOVE OCD-BW-TYPE  TO WK-KEY-TYPE
             MOVE OCD-BW-CODE TO WK-KEY-CODE.

           IF OCD-TYPE = "CL"
             MOVE OCD-REC         TO OCD-CLREC
             MOVE OCD-CLTYPE      TO WK-KEY-TYPE
             MOVE OCD-CLGROUP-ID  TO WK-KEY-CODE.

           IF OCD-TYPE = "CT"
             MOVE OCD-REC         TO OCD-CT-REC
             MOVE OCD-CT-TYPE     TO WK-KEY-TYPE
             STRING OCD-CT-STATE, OCD-CT-COUNTY INTO WK-KEY-CODE.

           IF OCD-TYPE = "CY"
             MOVE OCD-REC         TO OCD-CY-REC
             MOVE OCD-CY-TYPE     TO WK-KEY-TYPE
             STRING OCD-CY-STATE, OCD-CY-CITY INTO WK-KEY-CODE.

           IF OCD-TYPE = "EA"
             MOVE OCD-REC      TO OCD-EA-REC
             MOVE OCD-EA-TYPE  TO WK-KEY-TYPE
             MOVE OCD-EA-ID    TO WK-KEY-CODE.

           IF OCD-TYPE = "EL"
             MOVE OCD-REC      TO OCD-ELREC
             MOVE OCD-EL-TYPE  TO WK-KEY-TYPE
             MOVE OCD-EL-CODE  TO WK-KEY-CODE.

           IF OCD-TYPE = "FZ"
             MOVE OCD-REC      TO OCD-FZ-REC
             MOVE OCD-FZ-TYPE  TO WK-KEY-TYPE
             MOVE OCD-FZ-CODE  TO WK-KEY-CODE.

           IF OCD-TYPE = "JT"
             MOVE OCD-REC      TO OCD-JT-REC
             MOVE OCD-JT-TYPE  TO WK-KEY-TYPE
             MOVE OCD-JT-CODE  TO WK-KEY-CODE.

           IF OCD-TYPE = "LO"
             MOVE OCD-REC      TO OCD-BW-REC
             MOVE OCD-BW-TYPE  TO WK-KEY-TYPE
             MOVE OCD-BW-CODE  TO WK-KEY-CODE.

           IF OCD-TYPE = "LV"
             MOVE OCD-REC      TO OCD-LVREC
             MOVE OCD-LVTYPE   TO WK-KEY-TYPE
             MOVE OCD-LVLEVEL  TO WK-KEY-CODE.

           IF OCD-TYPE = "OR"
             MOVE OCD-REC      TO OCD-BW-REC
             MOVE OCD-BW-TYPE  TO WK-KEY-TYPE
             MOVE OCD-BW-CODE  TO WK-KEY-CODE.

           IF OCD-TYPE = "PA"
             MOVE OCD-REC      TO OCD-PAREC
             MOVE OCD-PA-TYPE  TO WK-KEY-TYPE
             MOVE OCD-PA-CODE  TO WK-KEY-CODE.

           IF OCD-TYPE = "PL"
             MOVE OCD-REC      TO OCD-POOLREC
             MOVE OCD-POOLTYPE TO WK-KEY-TYPE
             MOVE OCD-POOLID   TO WS-N5-TO-CHAR-NUM
             MOVE WS-N5-TO-CHAR-CHAR TO WK-KEY-CODE.

           IF OCD-TYPE = "PO"
              MOVE OCD-REC      TO OCD-PO-REC
              MOVE OCD-PO-TYPE  TO WK-KEY-TYPE
              MOVE OCD-PO-POCD  TO WK-KEY-CODE.

           IF OCD-TYPE = "PX"
              MOVE OCD-REC      TO OCD-PXREC
              MOVE OCD-PXTYPE   TO WK-KEY-TYPE
              MOVE OCD-PXPLTYPE TO WK-KEY-CODE.

           IF OCD-TYPE = "RC"
              MOVE OCD-REC      TO OCD-RC-REC
               MOVE OCD-RC-TYPE   TO WK-KEY-TYPE
              MOVE OCD-RC-CODE-ALPHA TO WK-KEY-CODE.

           IF OCD-TYPE = "TS"
             MOVE OCD-REC      TO OCD-PO-REC
             MOVE OCD-PO-TYPE  TO WK-KEY-TYPE
             MOVE OCD-PO-POCD  TO WK-KEY-CODE.

           MOVE OCD-REC TO WK-DATA.
           PERFORM WRITE-WK1-FILE.
 
           IF IO-GOOD 
              ADD 1 TO A15-RECORDS-PROCESSED.

           IF WK-KEY-TYPE = SPACES
      D      STOP MESS
             MOVE SPACES TO MESS.

           GO TO LOAD-WORK-FILE.

       LOAD-WORK-FILE-END.
           EXIT. 

       M-READ-WK1 SECTION.
           PERFORM READ-WK1-FILE-NEXT.
           IF IO-BAD 
              MOVE HIGH-VALUES TO WK-KEY
              MOVE "Y"         TO WK-EOF-SW
           ELSE
              MOVE WK-DATA     TO OCD-REC.

       EXIT-M-READ-WK1.
           EXIT.

       M-READ-CD SECTION.
           PERFORM READ-CD1-FILE-NEXT.
           IF IO-BAD
              MOVE HIGH-VALUES  TO CD1-KEY
              MOVE "Y"          TO CD-EOF-SW
           ELSE
              ADD 1 TO SQL-RECORDS-PROCESSED.

       EXIT-M-READ-CD.
           EXIT.

       M-READ-OCD1 SECTION.
           PERFORM READ-OCD1-FILE-NEXT.
           IF IO-BAD  
              MOVE "Y" TO OCD-EOF-SW.
 
       EXIT-M-READ-OCD1.
           EXIT.

      ******************************************************************
       COMPARE-FIELDS SECTION.
      ******************************************************************

           IF WK-KEY < CD1-KEY
      D       STOP MESS
              MOVE "MISSING S35 RECORD" TO D01-TEXT
              MOVE WK-KEY-TYPE TO D01-TYPE
              MOVE WK-KEY-CODE TO D01-CODE
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.

           IF WK-KEY > CD1-KEY
              MOVE "MISSING A15 RECORD" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.

           IF OCD-TYPE = "BR"
             MOVE OCD-REC TO OCD-BR-REC.
            IF OCD-TYPE = "BS"
             MOVE OCD-REC      TO OCD-BS-REC.
           IF OCD-TYPE = "BW"
             MOVE OCD-REC      TO OCD-BW-REC.
           IF OCD-TYPE = "CL"
             MOVE OCD-REC         TO OCD-CLREC.
           IF OCD-TYPE = "CT"
             MOVE OCD-REC         TO OCD-CT-REC.
           IF OCD-TYPE = "CY"
             MOVE OCD-REC         TO OCD-CY-REC.
           IF OCD-TYPE = "EA"
             MOVE OCD-REC      TO OCD-EA-REC.
            IF OCD-TYPE = "EL"
             MOVE OCD-REC      TO OCD-ELREC.
           IF OCD-TYPE = "FZ"
             MOVE OCD-REC      TO OCD-FZ-REC.
           IF OCD-TYPE = "JT"
             MOVE OCD-REC      TO OCD-JT-REC.
           IF OCD-TYPE = "LO"
             MOVE OCD-REC      TO OCD-BW-REC.
           IF OCD-TYPE = "LV"
             MOVE OCD-REC      TO OCD-LVREC.
           IF OCD-TYPE = "OR"
             MOVE OCD-REC      TO OCD-BW-REC.
           IF OCD-TYPE = "PA"
             MOVE OCD-REC      TO OCD-PAREC.
           IF OCD-TYPE = "PL"
             MOVE OCD-REC      TO OCD-POOLREC.
           IF OCD-TYPE = "PO"
              MOVE OCD-REC      TO OCD-PO-REC.
           IF OCD-TYPE = "PX"
              MOVE OCD-REC      TO OCD-PXREC.
           IF OCD-TYPE = "RC"
              MOVE OCD-REC      TO OCD-RC-REC.
           IF OCD-TYPE = "TS"
              MOVE OCD-REC      TO OCD-PO-REC.
 
 
           IF CD-TYPE = "AT"
              IF CD-DESC NOT = OCD-DESC
                 MOVE "CD-DESC" TO D01-TEXT
                 MOVE OCD-DESC  TO D01-A15
                 MOVE CD-DESC   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CD-AT-ACTIONCD NOT = OCD-ACTIONCD
                 MOVE "CD-AT-ACTIONCD" TO D01-TEXT
                 MOVE OCD-ACTIONCD     TO D01-A15
                 MOVE CD-AT-ACTIONCD   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CD-AT-MEMO-REQUIRED NOT = OCD-MEMO-REQUIRED
                 MOVE "CD-AT-MEMO-REQUIRED"  TO D01-TEXT
                 MOVE OCD-MEMO-REQUIRED      TO D01-A15
                 MOVE CD-AT-MEMO-REQUIRED    TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CD-AT-SUPERVISOR-ONLY NOT = OCD-SUPERVISOR-ONLY
                 MOVE "CD-AT-SUPERVISOR-ONLY" TO D01-TEXT
                 MOVE OCD-SUPERVISOR-ONLY     TO D01-A15
                 MOVE CD-AT-SUPERVISOR-ONLY   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CD-AT-RESULT-REQUIRED NOT = OCD-RESULT-REQUIRED
                 MOVE "CD-AT-RESULT-REQUIRED" TO D01-TEXT
                 MOVE OCD-RESULT-REQUIRED     TO D01-A15
                 MOVE CD-AT-RESULT-REQUIRED   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-IF.
   
           IF CD-TYPE = "BR"
              IF CD-DESC NOT = OCD-BR-DESC
                 MOVE "CD-DESC"   TO D01-TEXT
                 MOVE OCD-BR-DESC TO D01-A15
                 MOVE CD-DESC     TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CD-BR-FILE-TYPE NOT = OCD-BR-FILE-TYPE
                 MOVE "CD-BR-FILE-TYPE" TO D01-TEXT
                 MOVE OCD-BR-FILE-TYPE  TO D01-A15 
                 MOVE CD-BR-FILE-TYPE   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CD-BR-FILE-NAME NOT = OCD-BR-FILE-NAME
                 MOVE "CD-BR-FILE-NAME" TO D01-TEXT
                 MOVE OCD-BR-FILE-NAME  TO D01-A15 
                 MOVE CD-BR-FILE-NAME   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CD-BR-PAYOFF-FG NOT = OCD-BR-PAYOFF-FG
                 MOVE "CD-BR-PAYOFF-FG" TO D01-TEXT
                 MOVE OCD-BR-PAYOFF-FG  TO D01-A15 
                 MOVE CD-BR-PAYOFF-FG   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CD-BR-POST-SS-TRCD NOT = OCD-BR-POST-SS-TRCD
                 MOVE "CD-BR-POST-SS-TRCD" TO D01-TEXT
                 MOVE OCD-BR-POST-SS-TRCD  TO D01-A15 
                 MOVE CD-BR-POST-SS-TRCD   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CD-BR-GL-BRANCH NOT = OCD-BR-GL-BRANCH
                 MOVE "CD-BR-GL-BRANCH" TO D01-TEXT
                 MOVE OCD-BR-GL-BRANCH  TO D01-A15 
                 MOVE CD-BR-GL-BRANCH   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CD-BR-GL-ACCT NOT = OCD-BR-GL-ACCT
                 MOVE "CD-BR-GL-ACCT" TO D01-TEXT
                 MOVE OCD-BR-GL-ACCT  TO D01-A15 
                 MOVE CD-BR-GL-ACCT   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CD-BR-ALLOT-OPTION NOT = OCD-BR-ALLOT-OPTION
                 MOVE "CD-BR-ALLOT-OPTION" TO D01-TEXT
                 MOVE OCD-BR-ALLOT-OPTION  TO D01-A15 
                 MOVE CD-BR-ALLOT-OPTION   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CD-BR-LBOX-OPTION NOT = OCD-BR-LBOX-OPTION
                 MOVE "CD-BR-LBOX-OPTION" TO D01-TEXT
                 MOVE OCD-BR-LBOX-OPTION  TO D01-A15 
                 MOVE CD-BR-LBOX-OPTION   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CD-BR-LC-FG NOT = OCD-BR-LC-FG
                 MOVE "CD-BR-LC-FG" TO D01-TEXT
                 MOVE OCD-BR-LC-FG  TO D01-A15 
                 MOVE CD-BR-LC-FG   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CD-BR-OT-GL-BRANCH NOT = OCD-BR-OT-GL-BRANCH
                 MOVE "CD-BR-OT-GL-BRANCH" TO D01-TEXT
                 MOVE OCD-BR-OT-GL-BRANCH  TO D01-A15 
                 MOVE CD-BR-OT-GL-BRANCH   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CD-BR-OT-GL-ACCT NOT = OCD-BR-OT-GL-ACCT
                 MOVE "CD-BR-OT-GL-ACCT" TO D01-TEXT
                 MOVE OCD-BR-OT-GL-ACCT  TO D01-A15 
                 MOVE CD-BR-OT-GL-ACCT   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CD-BR-MULTIPLE-REFCDS NOT = OCD-BR-MULTIPLE-REFCDS
                 MOVE "CD-BR-MULTIPLE-REFCDS" TO D01-TEXT
                 MOVE OCD-BR-MULTIPLE-REFCDS  TO D01-A15 
                 MOVE CD-BR-MULTIPLE-REFCDS   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CD-BR-COLLECT-LC NOT = OCD-BR-COLLECT-LC
                 MOVE "CD-BR-COLLECT-LC" TO D01-TEXT
                 MOVE OCD-BR-COLLECT-LC  TO D01-A15 
                 MOVE CD-BR-COLLECT-LC   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-IF.

           IF CD-TYPE = "CO"
              IF CD-DESC NOT = OCD-DESC
                 MOVE "CD-DESC" TO D01-TEXT
                 MOVE OCD-DESC  TO D01-A15
                 MOVE CD-DESC   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CD-CO-SECURED NOT = OCD-SECURED
                 MOVE "CD-SECURED" TO D01-TEXT
                 MOVE OCD-SECURED  TO D01-A15
                 MOVE CD-CO-SECURED   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CD-CO-TRW NOT = OCD-TRW
                 MOVE "CD-CO-TRW" TO D01-TEXT
                 MOVE OCD-TRW  TO D01-A15
                 MOVE CD-CO-TRW   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CD-CO-GROUP NOT = OCD-GROUP
                 MOVE "CD-CO-GROUP" TO D01-TEXT
                 MOVE OCD-GROUP  TO D01-A15
                 MOVE CD-CO-GROUP   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-IF.

           IF CD-TYPE = "CT"
              IF CD-DESC NOT = OCD-CT-COUNTY-DESC
                 MOVE "CD-DESC"          TO D01-TEXT
                 MOVE OCD-CT-COUNTY-DESC TO D01-A15
                 MOVE CD-DESC            TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CD-CT-BOTH-TAX-FG NOT = OCD-CT-BOTH-TAX-FG
                 MOVE "CD-CT-BOTH-TAX-FG" TO D01-TEXT
                 MOVE OCD-CT-BOTH-TAX-FG  TO D01-A15
                 MOVE CD-CT-BOTH-TAX-FG   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              PERFORM VARYING SUB2 FROM 1 BY 1 UNTIL SUB2 > 12
                 IF CD-CT-INSTAXFRMLA(SUB2) NOT = 
                    OCD-CT-INSTAXFRMLA(SUB2)
                    STRING "CD-CT-INSTAXFRMLA(",SUB2,")" INTO D01-TEXT
                    MOVE OCD-CT-INSTAXFRMLA(SUB2)  TO D01-A15 
                    MOVE CD-CT-INSTAXFRMLA(SUB2)   TO D01-S35 
                    PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                    PERFORM WRITE-DETAIL-LINE-ERROR
                 END-IF
                 MOVE OCD-CT-INSTAXAMT(SUB2) TO WS-NUM
                 IF WS-NUM = 20.2
                   MOVE 0 TO OCD-CT-INSTAXAMT(SUB2)
                 END-IF
                 IF CD-CT-INSTAXAMT(SUB2) NOT = OCD-CT-INSTAXAMT(SUB2)
                    STRING "CD-CT-INSTAXAMT(",SUB2,")" INTO D01-TEXT
                    MOVE OCD-CT-INSTAXAMT(SUB2)  TO D01-A15-NUM
                    MOVE CD-CT-INSTAXAMT(SUB2)   TO D01-S35-NUM
                    PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                    PERFORM WRITE-DETAIL-LINE-ERROR
                 END-IF
              END-PERFORM
           END-IF.

           IF CD-TYPE = "CY"
              IF CD-DESC NOT = OCD-CY-CITY-DESC
                 MOVE "CD-DESC"         TO D01-TEXT
                 MOVE OCD-CY-CITY-DESC  TO D01-A15
                 MOVE CD-DESC           TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CD-CY-BOTH-TAX-FG NOT = OCD-CY-BOTH-TAX-FG
                 MOVE "CD-CY-BOTH-TAX-FG" TO D01-TEXT
                 MOVE OCD-CY-BOTH-TAX-FG  TO D01-A15
                 MOVE CD-CY-BOTH-TAX-FG   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              PERFORM VARYING SUB2 FROM 1 BY 1 UNTIL SUB2 > 12
                 IF CD-CY-INSTAXFRMLA(SUB2) NOT = 
                    OCD-CY-INSTAXFRMLA(SUB2)
                    STRING "CD-CY-INSTAXFRMLA(",SUB2,")" INTO D01-TEXT
                    MOVE OCD-CY-INSTAXFRMLA(SUB2) TO D01-A15 
                    MOVE CD-CY-INSTAXFRMLA(SUB2)  TO D01-S35 
                    PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                    PERFORM WRITE-DETAIL-LINE-ERROR
                 END-IF
                 MOVE OCD-CY-INSTAXAMT(SUB2) TO WS-NUM
                 IF WS-NUM = 20.2
                    MOVE 0 TO OCD-CY-INSTAXAMT(SUB2)
                 END-IF
                 IF CD-CY-INSTAXAMT(SUB2) NOT = OCD-CY-INSTAXAMT(SUB2)
                    STRING "CD-CY-INSTAXAMT(",SUB2,")" INTO D01-TEXT
                    MOVE OCD-CY-INSTAXAMT(SUB2) TO D01-A15-NUM 
                    MOVE CD-CY-INSTAXAMT(SUB2)  TO D01-S35-NUM
                    PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                    PERFORM WRITE-DETAIL-LINE-ERROR
                 END-IF
              END-PERFORM
           END-IF.

           IF CD-TYPE = "EA"
              IF CD-DESC NOT = OCD-EA-DESC
                 MOVE "CD-DESC"    TO D01-TEXT
                 MOVE OCD-EA-DESC  TO D01-A15
                 MOVE CD-DESC      TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CD-EA-CQ-FG NOT = OCD-EA-CQ-FG
                 MOVE "CD-EA-CQ-FG" TO D01-TEXT
                 MOVE OCD-EA-CQ-FG  TO D01-A15
                 MOVE CD-EA-CQ-FG   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CD-EA-GQ-FG NOT = OCD-EA-GQ-FG
                 MOVE "CD-EA-GQ-FG" TO D01-TEXT
                 MOVE OCD-EA-GQ-FG  TO D01-A15
                 MOVE CD-EA-GQ-FG   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-IF.

           IF CD-TYPE = "EL"
              IF CD-ELGRACE NOT = OCD-ELGRACE
                 MOVE "CD-ELGRACE" TO D01-TEXT
                 MOVE OCD-ELGRACE  TO D01-A15
                 MOVE CD-ELGRACE   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CD-ELGRACECD NOT = OCD-ELGRACECD
                 MOVE "CD-ELGRACECD" TO D01-TEXT
                 MOVE OCD-ELGRACECD  TO D01-A15
                 MOVE CD-ELGRACECD   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CD-ELTYPE NOT = OCD-ELTYPE
                 MOVE "CD-ELTYPE" TO D01-TEXT
                 MOVE OCD-ELTYPE  TO D01-A15
                 MOVE CD-ELTYPE   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CD-ELRATE NOT = OCD-ELRATE
                 MOVE "CD-ELRATE" TO D01-TEXT
                 MOVE OCD-ELRATE  TO D01-A15-NUM
                 MOVE CD-ELRATE   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CD-ELFRMLA NOT = OCD-ELFRMLA
                 MOVE "CD-ELFRMLA" TO D01-TEXT
                 MOVE OCD-ELFRMLA  TO D01-A15
                 MOVE CD-ELFRMLA   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CD-ELMINCHG NOT = OCD-ELMINCHG
                 MOVE "CD-ELMINCHG" TO D01-TEXT
                 MOVE OCD-ELMINCHG  TO D01-A15-NUM 
                 MOVE CD-ELMINCHG   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CD-ELMAXAMT NOT = OCD-ELMAXAMT
                 MOVE "CD-ELMAXAMT" TO D01-TEXT
                 MOVE OCD-ELMAXAMT  TO D01-A15-NUM 
                 MOVE CD-ELMAXAMT   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CD-ELDELFAC NOT = OCD-ELDELFAC
                 MOVE "CD-ELDELFAC" TO D01-TEXT
                 MOVE OCD-ELDELFAC  TO D01-A15 
                 MOVE CD-ELDELFAC   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CD-ELYRTYPE NOT = OCD-ELYRTYPE
                 MOVE "CD-ELYRTYPE" TO D01-TEXT
                 MOVE OCD-ELYRTYPE  TO D01-A15 
                 MOVE CD-ELYRTYPE   TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CD-ELUNITPER-CD NOT = OCD-ELUNITPER-CD
                 MOVE "CD-ELUNITPER-CD" TO D01-TEXT
                 MOVE OCD-ELUNITPER-CD  TO D01-A15
                 MOVE CD-ELUNITPER-CD TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-IF.

           IF CD-TYPE = "PO"
              IF CD-DESC NOT = OCD-POOLDESC
                 MOVE "CD-DESC"    TO D01-TEXT
                 MOVE OCD-POOLDESC  TO D01-A15
                 MOVE CD-DESC      TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OCD-TITLE-HOLD-DAYS NOT NUMERIC
                 MOVE 0  TO OCD-TITLE-HOLD-DAYS
              END-IF
              IF CD-PO-TITLE-HOLD-DAYS NOT = OCD-TITLE-HOLD-DAYS
                 MOVE "CD-PO-TITLE-HOLD-DAYS" TO D01-TEXT
                 MOVE OCD-TITLE-HOLD-DAYS  TO D01-A15
                 MOVE CD-PO-TITLE-HOLD-DAYS   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-IF.

           IF CD-TYPE = "RE"
              IF CD-DESC NOT = OCD-DESC
                 MOVE "CD-DESC" TO D01-TEXT
                 MOVE OCD-DESC  TO D01-A15
                 MOVE CD-DESC   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CD-RE-ACTIONCD  NOT = OCD-ACTIONCD 
                 MOVE "CD-RE-ACTIONCD " TO D01-TEXT
                 MOVE OCD-ACTIONCD   TO D01-A15
                 MOVE CD-RE-ACTIONCD    TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CD-RE-MEMO-REQUIRED NOT = OCD-MEMO-REQUIRED
                 MOVE "CD-RE-MEMO-REQUIRED" TO D01-TEXT
                 MOVE OCD-MEMO-REQUIRED  TO D01-A15
                 MOVE CD-RE-MEMO-REQUIRED   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CD-RE-SUPERVISOR-ONLY NOT = OCD-SUPERVISOR-ONLY
                 MOVE "CD-RE-SUPERVISOR-ONLY" TO D01-TEXT
                 MOVE OCD-SUPERVISOR-ONLY  TO D01-A15
                 MOVE CD-RE-SUPERVISOR-ONLY   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CD-RE-ACTIVITY-REQUIRED NOT = OCD-RE-ACTIVITY-REQUIRED
                 MOVE "CD-RE-ACTIVITY-REQUIRED" TO D01-TEXT
                 MOVE OCD-RE-ACTIVITY-REQUIRED  TO D01-A15
                 MOVE CD-RE-ACTIVITY-REQUIRED   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CD-RE-RESCHEDULE-REQ NOT = OCD-RE-RESCHEDULE-REQ
                 MOVE "CD-RE-RESCHEDULE-REQ" TO D01-TEXT
                 MOVE OCD-RE-RESCHEDULE-REQ  TO D01-A15
                 MOVE CD-RE-RESCHEDULE-REQ   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OCD-RE-RESCHEDULE-TIME NOT NUMERIC
                 MOVE 0  TO OCD-RE-RESCHEDULE-TIME
              END-IF
              IF CD-RE-RESCHEDULE-TIME NOT = OCD-RE-RESCHEDULE-TIME
                 MOVE "CD-RE-RESCHEDULE-TIME" TO D01-TEXT
                 MOVE OCD-RE-RESCHEDULE-TIME  TO D01-A15
                 MOVE CD-RE-RESCHEDULE-TIME   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-IF.

           IF CD-TYPE = "FC"
              IF CD-DESC NOT = OCD-FCDESC
                 MOVE "CD-DESC" TO D01-TEXT
                 MOVE OCD-FCDESC  TO D01-A15
                 MOVE CD-DESC   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF OCD-FCFEEAMT NOT NUMERIC
                 MOVE 0  TO OCD-FCFEEAMT
              END-IF
              IF CD-FC-FEEAMT NOT = OCD-FCFEEAMT
                 MOVE "CD-FCFEEAMT" TO D01-TEXT
                 MOVE OCD-FCFEEAMT  TO D01-A15-NUM 
                 MOVE CD-FC-FEEAMT   TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-IF.

           IF CD-TYPE = "TC"
              IF CD-DESC NOT = OCD-TC-DESC
                 MOVE "CD-DESC"    TO D01-TEXT
                 MOVE OCD-TC-DESC  TO D01-A15
                 MOVE CD-DESC      TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              PERFORM VARYING SUB2 FROM 1 BY 1 UNTIL SUB2 > 10
                 IF CD-TC-REFNO(SUB2) NOT = 
                    OCD-TC-REFNO(SUB2)
                    STRING "CD-TC-REFNO(",SUB2,")" INTO D01-TEXT
                    MOVE OCD-TC-REFNO(SUB2) TO D01-A15 
                    MOVE CD-TC-REFNO(SUB2)  TO D01-S35 
                    PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                    PERFORM WRITE-DETAIL-LINE-ERROR
                 END-IF
              END-PERFORM
           END-IF.

           IF CD-TYPE = "PS"
              IF CD-DESC NOT = OCD-PS-DESC
                 MOVE "CD-DESC"    TO D01-TEXT
                 MOVE OCD-PS-DESC  TO D01-A15
                 MOVE CD-DESC      TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CD-PS-TDR-FLAG NOT = OCD-PS-TDR-FLAG
                 MOVE "CD-PS-TDR-FLAG" TO D01-TEXT
                 MOVE OCD-PS-TDR-FLAG  TO D01-A15
                 MOVE CD-PS-TDR-FLAG   TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-IF.

        EXIT-COMPARE-FIELDS.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE-ERROR-KEY SECTION.
      ******************************************************************

           MOVE CD-TYPE  TO D01-TYPE.
           MOVE CD-CODE  TO D01-CODE.

      ******************************************************************
       WRITE-DETAIL-LINE-ERROR SECTION.
      ******************************************************************

           ADD 1    TO TOTAL-ERRORS-CTR.
           PERFORM WRITE-DETAIL-LINE.

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

           MOVE "BEGINNING TYPE                : " TO R01-PROMPT.
           MOVE BEG-TYPE                           TO R01-NUMERIC.
           MOVE 01                                 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING TYPE                   : " TO R01-PROMPT.
           MOVE END-TYPE                           TO R01-NUMERIC.
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
       COPY "LIBGB/GETENV.CPY".
       COPY "LIBGB/PROGID.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO CDFILE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CDFILE-SYSDATE.
           DISPLAY CDFILE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE CDFILE-FORM-FIELDS TO WR-BUF.
           MOVE CDFILE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE CDFILE-HELP--FILE TO HELP-LINK-FILE.
           MOVE CDFILE-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/CDFILE_EVA.CPY".
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
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPCD1RN.CPY".

      *-----------------------------------------------------------------
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
       OPEN-OCD1-FILE.
           PERFORM OPEN-IT.
           MOVE OCD-PATH TO E-FILE.
           OPEN INPUT OCD-FILE.
           IF IO-FG = 8
              GO TO OPEN-OCD1-FILE.
           IF IO-FG = 7
              CLOSE OCD-FILE
              GO TO OPEN-OCD1-FILE.
        READ-OCD1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OCD-PATH TO E-FILE.
           READ OCD-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OCD1-FILE-NEXT.
       CLOSE-OCD1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OCD-FILE.

       OPEN-WK1-FILE.
           PERFORM OPEN-IT.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           OPEN I-O WK-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK1-FILE.
           IF IO-FG = 7
              CLOSE WK-FILE
              GO TO OPEN-WK1-FILE.
       OPEN-WK1-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           OPEN OUTPUT WK-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK1-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK-FILE
              GO TO OPEN-WK1-FILE-OUTPUT.
       READ-WK1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           MOVE WK-KEY  TO E-KEYX.
           READ WK-FILE NEXT
              AT END  MOVE 9 TO IO-STATUS IO-FG
           END-READ.
           IF IO-FG = 8
              GO TO READ-WK1-FILE-NEXT.
       WRITE-WK1-FILE.
           PERFORM WRITE-IT.
           MOVE 0 TO IO-FG IO-STATUS.
           MOVE WK-PATH TO E-FILE.
           MOVE WK-KEY  TO E-KEYX.
           WRITE WK-REC 
             INVALID KEY 
                MOVE 2 TO IO-STATUS IO-FG
           END-WRITE.
           IF IO-FG = 8
             GO TO WRITE-WK1-FILE.
           UNLOCK WK-FILE.
       CLOSE-WK1-FILE.
           PERFORM CLOSE-IT.
           MOVE 0 TO IO-FG IO-STATUS.
           CLOSE WK-FILE.


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

           MOVE WK-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY CDFILE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
