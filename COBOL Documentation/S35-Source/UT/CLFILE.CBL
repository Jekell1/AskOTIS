       IDENTIFICATION DIVISION.
       PROGRAM-ID.    CLFILE.
       DATE-WRITTEN.  02-19-2024.
       AUTHOR. MARK BOOMGAARD.
      ******************************************************************
      *
      * DESCRIPTION   : COMPARE S35 CLFILE WITH A15 CLFILE
      *            
      *                 TO BE USED AFTER AN UPDATE
      *                 WILL COMPARE EACH FIELD AND REPORT ANY 
      *                 DIFFERENCES, AS LONG AS FIELD EXIST IN BOTH FILES
      *
      *  2 FILE MATCH PROCESSING.
      *
      *  01-07-25  RMZ ADDED 'BTRACK' - PROCESS TRACKING TO PROGRAM.
      *  04-16-25  MB  USE 0 FOR OCL-PD-PRPAID-AHEAD, OCL-PD-RESTRICT-ADJ
                        AND OCL-PD-DUAL-LOAN-ADJ IF = 5389762.880
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".

      * COPYMEMBER: LIBLP/LPFSCD
      * COPYMEMBER: LIBLP/LPFSCL
           SELECT OCL-FILE ASSIGN TO OCL-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OCL1-KEY
                  ALTERNATE RECORD KEY OL2-KEY
                               = OCL-GROUP-ID
                                 OCL-DESC
                            WITH DUPLICATES
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
       FD  OCL-FILE
           LABEL RECORDS ARE STANDARD.
      *-----------------------------------------------------------------
       COPY "LIBUP/A15SRC/LP01CL.CPY" REPLACING LEADING "CL" BY "OCL".
      *-----------------------------------------------------------------

       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
              05  WK-KEY-GROUP-ID.
                07  WK-KEY-GROUP-ID12  PIC X(2).
                07  WK-KEY-GROUP-ID34  PIC X(2).
              05  WK-KEY-CAN-NO        PIC XX.
           03  WK-DATA                 PIC X(OCL-SIZE).

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)UT/CLFILE.CBL S35 12/17/24-17:54:14 >".

       COPY "LIBLP/LP01CL.CPY".
       COPY "LIBLP/LP01CL_SQL.CPY".
       COPY "LIBLP/LPWSCL.CPY".

       COPY "LIBUP/A15SRC/LP01PD.CPY" 
                     REPLACING LEADING "PD" BY "OCLPD".

       01  OCL-PATH             PIC X(35) VALUE SPACES.
       01  OCL-ALL-PATH.
           03  OCL-ALL-BASE     PIC X(9).
           03  FILLER           PIC X     VALUE "/".
           03  OCL-ALL-MACHINE  PIC XX.
           03  FILLER           PIC X(10) VALUE
               "/LP/CLFILE".

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
       01  CL-EOF-SW                   PIC X      VALUE "N".
       01  OCL-EOF-SW                  PIC X      VALUE "N".
       01  A15-FILE-FOUND-SW           PIC X      VALUE "N".
       01  TOTAL-ERRORS-CTR            PIC 9(9)   VALUE 0.
       01  PRINTING-RANGES-SW          PIC X      VALUE "N".
       01  SUB1                        PIC 9      VALUE 0.
       01  SUB2                        PIC 99     VALUE 0.
       01  WK-EOF-SW                   PIC X      VALUE "N".
       01  WK-RECS-CTR                 PIC 9(9)   VALUE 0.
       01  COMP-TILLNO                 PIC 99.
       01  WS-NUM                      PIC 9(13)V999 VALUE 0.

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

       01  LEGEND-BEG-GROUPID.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F7-EXIT, EN".
           03  FILLER PIC X(30) VALUE "TER BEGINNING GROUP ID       ".
           03  FILLER PIC X(19) VALUE "                   ".

       01  LEGEND-END-GROUPID.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER ENDIN".
           03  FILLER PIC X(30) VALUE "G GROUP ID                    ".
           03  FILLER PIC X(19) VALUE "                   ".

      *-----------------------------------------------------------------
       01  LEGEND-ACCEPT-YN.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT             ".
           03  FILLER PIC X(30) VALUE "                              ".
           03  FILLER PIC X(19) VALUE "     ACCEPT (Y/N): ".

       01  SOURCE-PATH-LIST            PIC X(07)     VALUE "S-PATH.".

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  SOURCE-PATH             PIC X(9).
           03  BEG-GROUPID             PIC X(4).
           03  END-GROUPID             PIC X(4).

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
           "CAN LOAN FILE (CLFILE) COMPARE A15 TO S35".

       01  HEADER-LINE-03.
           03  FILLER                PIC X(6)  VALUE "GRP ID".
           03  FILLER                PIC X(2)  VALUE SPACES.
           03  FILLER                PIC X(6)  VALUE "CAN NO".
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
           03  D01-GROUP-ID                   PIC X(4).
           03  FILLER                         PIC X(5).
           03  D01-CAN-NO                     PIC X(2).
           03  FILLER                         PIC X(5).
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
       COPY "LIBUT/CLFILE_DEF.CPY".
       COPY "LIBUT/CLFILE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/CLFILE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE OCL-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-CL1-FILE.
           PERFORM CLOSE-OCL1-FILE.
           PERFORM CLOSE-PR-FILE.
           PERFORM CLOSE-WK1-FILE.
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
           MOVE "CLFILE" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE CLFILE-QUEUE-POS  TO Q-POS.
           MOVE CLFILE-COPIES-POS TO C-POS.
           MOVE CLFILE-PRU-POS    TO P-POS.
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

           PERFORM OPEN-CL1-FILE.
           PERFORM OPEN-PR-FILE.

           PERFORM START-MISC-CSR.
           PERFORM READ-MISC-CSR-NEXT.
           PERFORM CLOSE-MISC-CSR.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WK-PATH.

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
              GO TO ENTER-END-GROUPID.

       ENTER-SOURCE-PATH.

           IF SOURCE-PATH = SPACES
              MOVE "A15_PATH"       TO GETENV-BUF
              PERFORM GETENV-CALL
              IF ( STAT-GOOD )
                 MOVE GETENV-BUF    TO SOURCE-PATH
              ELSE
                 MOVE "/USR/DATA"   TO SOURCE-PATH
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

       ENTER-BEG-GROUPID.

           MOVE LEGEND-BEG-GROUPID TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E RA000BEG-GROUPID." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F3-KEY ) GO TO ENTER-ALL-GROUPID.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-GROUPID.

           MOVE VDUBUF TO BEG-GROUPID.

      *-----------------------------------------------------------------
       ENTER-END-GROUPID.

           MOVE LEGEND-END-GROUPID TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E RA000END-GROUPID." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-GROUPID.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-GROUPID.

           MOVE VDUBUF TO END-GROUPID.

           IF ( BEG-GROUPID > END-GROUPID )
              MOVE "INVALID GROUP ID RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-GROUPID.

           GO TO ENTER-PARAMETERS-END.

      *-----------------------------------------------------------------
       ENTER-ALL-GROUPID.

           MOVE "S RA000BEG-GROUPID." TO VDU.
           MOVE SPACES                TO BEG-GROUPID VDUBUF.
           PERFORM SCREENIO.

           MOVE "S RA000END-GROUPID." TO VDU.
           MOVE "ZZZZ"                TO END-GROUPID VDUBUF.
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

           MOVE "S  A090S-PATH." TO VDU.
           MOVE SOURCE-PATH      TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A020BEG-GROUPID." TO VDU.
           MOVE BEG-GROUPID              TO VDUBUF.
           PERFORM SCREENIO

           MOVE "S  A020END-GROUPID." TO VDU.
           MOVE END-GROUPID              TO VDUBUF.
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
           MOVE "B"                        TO CL-PATH-OVERRIDE.

           MOVE BEG-GROUPID  TO QCL1-WBEG-GROUP.
           MOVE END-GROUPID  TO QCL1-WEND-GROUP.
           MOVE SPACES       TO QCL1-WBEG-CAN-NO.
           MOVE ALL 'Z'      TO QCL1-WEND-CAN-NO.

           MOVE "PROCESSING S35 DATA" TO BTRACK-MESS.
           PERFORM BTRACK-GLOBAL.

           PERFORM OPEN-CL1-FILE.
           PERFORM START-CL1-FILE.
           PERFORM M-READ-CL.

      * DELETE/EMPTY TEMP FILE IF EXIST
           PERFORM OPEN-WK1-FILE-OUTPUT.
           PERFORM CLOSE-WK1-FILE.
           PERFORM OPEN-WK1-FILE.

      *** MUST LOAD R1 AND R2 CLFILE ***
      *** UPDATE FROM R2 IF NOT STATE OK REC***
           MOVE SOURCE-PATH   TO OCL-ALL-BASE.
           MOVE MACHINE-R1    TO OCL-ALL-MACHINE.
           MOVE OCL-ALL-PATH  TO OCL-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE "MISSING R1 FILE" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE 
              GO TO PROCESS-DRIVER-END.

           MOVE "PROCESSING A15 DATA" TO BTRACK-MESS.  
           PERFORM BTRACK-GLOBAL.

           MOVE "N" TO OCL-EOF-SW.
           PERFORM OPEN-OCL1-FILE.
           PERFORM LOAD-WORK-FILE.
           PERFORM CLOSE-OCL1-FILE. 
 
      *** LOAD R2 DO NOT UPDATE IF REC ALREADY IN WKFILE IF OK GROUP***
           MOVE MACHINE-R2   TO OCL-ALL-MACHINE.
           MOVE OCL-ALL-PATH TO OCL-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE "MISSING R2 FILE" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE 
              GO TO PROCESS-DRIVER-END.

           MOVE "N" TO OCL-EOF-SW.
           PERFORM OPEN-OCL1-FILE.
           PERFORM LOAD-WORK-FILE.
           PERFORM CLOSE-OCL1-FILE. 

           PERFORM CLOSE-WK1-FILE.
 
           PERFORM OPEN-WK1-FILE.
           PERFORM M-READ-WK1.

           PERFORM PROCESS-NEXT-CL
              UNTIL CL-EOF-SW = "Y" AND WK-EOF-SW = "Y".
 
           PERFORM CLOSE-CL1-FILE.
           PERFORM CLOSE-WK1-FILE.

           GO TO PROCESS-DRIVER-END.

       PROCESS-NEXT-CL.

           PERFORM SEND-PROGRESS-INDICATOR.

           PERFORM COMPARE-FIELDS.

           IF WK-KEY < CL1-KEY
              PERFORM M-READ-WK1
           ELSE
              IF WK-KEY > CL1-KEY
                 PERFORM M-READ-CL
              ELSE
                 PERFORM M-READ-WK1
                 PERFORM M-READ-CL.

       PROCESS-DRIVER-END.
           EXIT. 

       LOAD-WORK-FILE SECTION.
           PERFORM M-READ-OCL1.

           IF OCL-EOF-SW = "Y"
              GO TO LOAD-WORK-FILE-END.

           IF OCL-GROUP-ID < BEG-GROUPID OR OCL-GROUP-ID > END-GROUPID
              GO TO LOAD-WORK-FILE.

           IF OCL-ALL-MACHINE = MACHINE-R2
              IF OCL-GROUP-LOC = "OK  " OR "OKL "
                 GO TO LOAD-WORK-FILE.

           INITIALIZE WK-REC.

           MOVE OCL-GROUP-ID  TO WK-KEY-GROUP-ID
           MOVE OCL-CAN-NO    TO WK-KEY-CAN-NO.
           MOVE OCL-REC       TO WK-DATA.
           PERFORM WRITE-WK1-FILE.

           IF IO-STATUS = 9
              PERFORM REWRITE-WK1-FILE
           ELSE
              ADD 1 TO A15-RECORDS-PROCESSED.

           GO TO LOAD-WORK-FILE.

       LOAD-WORK-FILE-END.
           EXIT. 

       M-READ-WK1 SECTION.
           PERFORM READ-WK1-FILE-NEXT.

           IF IO-BAD 
              MOVE HIGH-VALUES TO WK-KEY
              MOVE "Y"         TO WK-EOF-SW
           ELSE
              MOVE WK-DATA     TO OCL-REC.

       EXIT-M-READ-WK1.
           EXIT.

       M-READ-CL SECTION.
           PERFORM READ-CL1-FILE-NEXT.
           IF IO-BAD
              MOVE HIGH-VALUES  TO CL1-KEY
              MOVE "Y"          TO CL-EOF-SW
           ELSE
              ADD 1 TO SQL-RECORDS-PROCESSED.

       EXIT-M-READ-CL.
           EXIT.

       M-READ-OCL1 SECTION.
           PERFORM READ-OCL1-FILE-NEXT.
           IF IO-BAD  
              MOVE "Y" TO OCL-EOF-SW.
 
       EXIT-M-READ-OCL1.
           EXIT.

      ******************************************************************
       COMPARE-FIELDS SECTION.
      ******************************************************************

           IF WK-KEY < CL1-KEY
              MOVE "MISSING S35 RECORD" TO D01-TEXT
              MOVE WK-KEY-GROUP-ID      TO D01-GROUP-ID
              MOVE WK-KEY-CAN-NO        TO D01-CAN-NO
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.

           IF WK-KEY > CL1-KEY
              MOVE "MISSING A15 RECORD" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-COMPARE-FIELDS.

           MOVE WK-DATA    TO OCL-REC.
           MOVE OCL-PD-REC TO OCLPD-REC.

           IF CL-DESC NOT = OCL-DESC
              MOVE "CL-DESC" TO D01-TEXT
              MOVE OCL-DESC  TO D01-A15
              MOVE CL-DESC   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-GIFTNET NOT = OCL-GIFTNET
              MOVE "CL-GIFTNET" TO D01-TEXT
              MOVE OCL-GIFTNET  TO D01-A15
              MOVE CL-GIFTNET   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-OWNBR NOT = OCLPD-OWNBR
              MOVE "CL-PD-OWNBR"  TO D01-TEXT
              MOVE OCLPD-OWNBR    TO D01-A15
              MOVE CL-PD-OWNBR    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-CLASS NOT = OCLPD-CLASS
              MOVE "CL-PD-CLASS" TO D01-TEXT
              MOVE OCLPD-CLASS   TO D01-A15
              MOVE CL-PD-CLASS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

      * COMPARES TO OPD-NUMBER
           IF CL-PD-ACCTNO NOT = OCLPD-NUMBER
              MOVE "CL-PD-ACCTNO" TO D01-TEXT
              MOVE OCLPD-NUMBER   TO D01-A15
              MOVE CL-PD-ACCTNO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.
 
           IF CL-PD-SUBCLASS NOT = OCLPD-SUBCLASS
              MOVE "CL-PD-SUBCLASS"   TO D01-TEXT
              MOVE OCLPD-SUBCLASS     TO D01-A15
              MOVE CL-PD-SUBCLASS     TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.
 
           IF CL-PD-LAWCODE NOT = OCLPD-LAWCODE
              MOVE "CL-PD-LAWCODE" TO D01-TEXT
              MOVE OCLPD-LAWCODE   TO D01-A15 
              MOVE CL-PD-LAWCODE   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR. 

           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 4
              IF CL-PD-SSNO(SUB1) NOT = OCLPD-SSNO(SUB1)
                 STRING "CL-PD-SSNO(",SUB1,")" INTO D01-TEXT
                 MOVE OCLPD-SSNO(SUB1)  TO D01-A15 
                 MOVE CL-PD-SSNO(SUB1)  TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CL-PD-MAKERCD(SUB1) NOT = OCLPD-MAKERCD(SUB1)
                 STRING "CL-PD-MAKERCD(",SUB1,")" INTO D01-TEXT
                 MOVE OCLPD-MAKERCD(SUB1)  TO D01-A15 
                 MOVE CL-PD-MAKERCD(SUB1)  TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.
 
           IF CL-PD-ORGST NOT = OCLPD-ORGST
              MOVE "CL-PD-ORGST" TO D01-TEXT
              MOVE OCLPD-ORGST   TO D01-A15 
              MOVE CL-PD-ORGST   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.
 
           IF CL-PD-ORGBR NOT = OCLPD-ORGBR
              MOVE "CL-PD-ORGBR" TO D01-TEXT
              MOVE OCLPD-ORGBR   TO D01-A15 
              MOVE CL-PD-ORGBR   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.
 
           IF CL-PD-OWNST NOT = OCLPD-OWNST
              MOVE "CL-PD-OWNST" TO D01-TEXT
              MOVE OCLPD-OWNST   TO D01-A15 
              MOVE CL-PD-OWNST   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.
 
           IF CL-PD-SPRCLASS NOT = OCLPD-SPRCLASS
              MOVE "CL-PD-SPRCLASS" TO D01-TEXT
              MOVE OCLPD-SPRCLASS   TO D01-A15 
              MOVE CL-PD-SPRCLASS   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.
 
            IF CL-PD-CONVERCD NOT = OCLPD-CONVERCD
              MOVE "CL-PD-CONVERCD" TO D01-TEXT
              MOVE OCLPD-CONVERCD  TO D01-A15 
              MOVE CL-PD-CONVERCD   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.
 
           IF CL-PD-PURCD NOT = OCLPD-PURCD
              MOVE "CL-PD-PURCD" TO D01-TEXT
              MOVE OCLPD-PURCD   TO D01-A15 
              MOVE CL-PD-PURCD   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.
 
           IF CL-PD-SRCD NOT = OCLPD-SRCD
              MOVE "CL-PD-SRCD" TO D01-TEXT
              MOVE OCLPD-SRCD   TO D01-A15 
              MOVE CL-PD-SRCD   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.
 
           IF CL-PD-COLLCD NOT = OCLPD-COLLCD
              MOVE "CL-PD-COLLCD" TO D01-TEXT
              MOVE OCLPD-COLLCD   TO D01-A15 
              MOVE CL-PD-COLLCD   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.
 
           IF CL-PD-COLLID NOT = OCLPD-COLLID
              MOVE "CL-PD-COLLID" TO D01-TEXT
              MOVE OCLPD-COLLID   TO D01-A15 
              MOVE CL-PD-COLLID   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.
 
           IF CL-PD-POCD NOT = OCLPD-POCD
              MOVE "CL-PD-POCD" TO D01-TEXT
              MOVE OCLPD-POCD   TO D01-A15 
              MOVE CL-PD-POCD   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.
 
           IF CL-PD-PLCD NOT = OCLPD-PLCD
              MOVE "CL-PD-PLCD" TO D01-TEXT
              MOVE OCLPD-PLCD   TO D01-A15 
              MOVE CL-PD-PLCD   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-ADDON-FG NOT = OCLPD-ADDON-FG
              MOVE "CL-PD-ADDON-FG" TO D01-TEXT
              MOVE OCLPD-ADDON-FG   TO D01-A15 
              MOVE CL-PD-ADDON-FG   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-CONFIDENTCD NOT = OCLPD-CONFIDENTCD
              MOVE "CL-PD-CONFIDENTCD" TO D01-TEXT
              MOVE OCLPD-CONFIDENTCD   TO D01-A15 
              MOVE CL-PD-CONFIDENTCD   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-ALLOTCD NOT = OCLPD-ALLOTCD
              MOVE "CL-PD-ALLOTCD" TO D01-TEXT
              MOVE OCLPD-ALLOTCD   TO D01-A15 
              MOVE CL-PD-ALLOTCD   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-RECASTCD NOT = OCLPD-RECASTCD
              MOVE "CL-PD-RECASTCD" TO D01-TEXT
              MOVE OCLPD-RECASTCD   TO D01-A15 
              MOVE CL-PD-RECASTCD   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-SKIP-TRW NOT = OCLPD-SKIP-TRW
              MOVE "CL-PD-SKIP-TRW" TO D01-TEXT
              MOVE OCLPD-SKIP-TRW   TO D01-A15 
              MOVE CL-PD-SKIP-TRW   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-STATUSFG NOT = OCLPD-STATUSFG
              MOVE "CL-PD-STATUSFG" TO D01-TEXT
              MOVE OCLPD-STATUSFG   TO D01-A15 
              MOVE CL-PD-STATUSFG   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-SMALLOAN NOT = OCLPD-SMALLOAN
              MOVE "CL-PD-SMALLOAN" TO D01-TEXT
              MOVE OCLPD-SMALLOAN   TO D01-A15 
              MOVE CL-PD-SMALLOAN   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-SALEFIN NOT = OCLPD-SALEFIN
              MOVE "CL-PD-SALEFIN" TO D01-TEXT
              MOVE OCLPD-SALEFIN   TO D01-A15 
              MOVE CL-PD-SALEFIN   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-SERVACCT NOT = OCLPD-SERVACCT
              MOVE "CL-PD-SERVACCT" TO D01-TEXT
              MOVE OCLPD-SERVACCT   TO D01-A15 
              MOVE CL-PD-SERVACCT   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-LOANTYPE NOT = OCLPD-LOANTYPE
              MOVE "CL-PD-LOANTYPE" TO D01-TEXT
              MOVE OCLPD-LOANTYPE   TO D01-A15 
              MOVE CL-PD-LOANTYPE   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-ADDON-FG NOT = OCLPD-ADDON-FG
              MOVE "CL-PD-ADDON-FG" TO D01-TEXT
              MOVE OCLPD-ADDON-FG   TO D01-A15 
              MOVE CL-PD-ADDON-FG   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-PAY-SCHLD-FG NOT = OCLPD-PAY-SCHLD-FG
              MOVE "CL-PD-PAY-SCHLD-FG" TO D01-TEXT
              MOVE OCLPD-PAY-SCHLD-FG   TO D01-A15 
              MOVE CL-PD-PAY-SCHLD-FG   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-LC-CODE NOT = OCLPD-LC-CODE
              MOVE "CL-PD-LC-CODE"  TO D01-TEXT
              MOVE OCLPD-LC-CODE    TO D01-A15 
              MOVE CL-PD-LC-CODE    TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-CAN-LOAN-NO NOT = OCLPD-CAN-LOAN-NO
              MOVE "CL-PD-CAN-LOAN-NO"  TO D01-TEXT
              MOVE OCLPD-CAN-LOAN-NO    TO D01-A15 
              MOVE CL-PD-CAN-LOAN-NO    TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-STATFLAGS NOT = OCLPD-STATFLAGS
              MOVE "CL-PD-STATFLAGS"    TO D01-TEXT
              MOVE OCLPD-STATFLAGS   TO D01-A15 
              MOVE CL-PD-STATFLAGS   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-ENTDATE NOT = OCLPD-ENTDATE
              MOVE "CL-PD-ENTDATE" TO D01-TEXT
              MOVE OCLPD-ENTDATE   TO D01-A15 
              MOVE CL-PD-ENTDATE   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OCLPD-LOANDATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           IF CL-PD-LOANDATE NOT = DATE-YYYYMMDD
              MOVE "CL-PD-LOANDATE"  TO D01-TEXT
              MOVE OCLPD-LOANDATE    TO D01-A15 
              MOVE CL-PD-LOANDATE    TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-PURDATE NOT = OCLPD-PURDATE
              MOVE "CL-PD-PURDATE"  TO D01-TEXT
              MOVE OCLPD-PURDATE    TO D01-A15 
              MOVE CL-PD-PURDATE    TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OCLPD-INTDATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           IF CL-PD-INTDATE NOT = DATE-YYYYMMDD
              MOVE "CL-PD-INTDATE" TO D01-TEXT
              MOVE OCLPD-INTDATE   TO D01-A15 
              MOVE CL-PD-INTDATE      TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-CASHDATE NOT = OCLPD-CASHDATE
              MOVE "CL-PD-CASHDATE" TO D01-TEXT
              MOVE OCLPD-CASHDATE   TO D01-A15 
              MOVE CL-PD-CASHDATE   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-FILEDATE NOT = OCLPD-FILEDATE
              MOVE "CL-PD-FILEDATE" TO D01-TEXT
              MOVE OCLPD-FILEDATE   TO D01-A15 
              MOVE CL-PD-FILEDATE   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OCLPD-ORIG-1STPAY TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           IF CL-PD-ORIG-1STPYDATE NOT = DATE-YYYYMMDD
              MOVE "CL-PD-ORIG-1STPYDATE" TO D01-TEXT
              MOVE OCLPD-ORIG-1STPAY      TO D01-A15 
              MOVE CL-PD-ORIG-1STPYDATE   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OCLPD-1STPYDATE TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           IF CL-PD-1STPYDATE NOT = DATE-YYYYMMDD
              MOVE "CL-PD-1STPAY-DATE" TO D01-TEXT
              MOVE OCLPD-1STPYDATE     TO D01-A15 
              MOVE CL-PD-1STPYDATE     TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.


           IF CL-PD-JDDATE NOT = OCLPD-JDDATE
              MOVE "CL-PD-1STPAY-DATE" TO D01-TEXT
              MOVE OCLPD-JDDATE     TO D01-A15 
              MOVE CL-PD-JDDATE     TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-PLDATE NOT = OCLPD-PLDATE
              MOVE "CL-PD-1STPAY-DATE" TO D01-TEXT
              MOVE OCLPD-PLDATE     TO D01-A15 
              MOVE CL-PD-PLDATE     TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-ACCELDATE NOT = OCLPD-ACCELDATE
              MOVE "CL-PD-1STPAY-DATE" TO D01-TEXT
              MOVE OCLPD-ACCELDATE     TO D01-A15 
              MOVE CL-PD-ACCELDATE     TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-REDUDATE NOT = OCLPD-REDUDATE
              MOVE "CL-PD-1STPAY-DATE" TO D01-TEXT
              MOVE OCLPD-REDUDATE     TO D01-A15 
              MOVE CL-PD-REDUDATE     TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.
 
           IF CL-PD-POFFDATE NOT = OCLPD-POFFDATE
              MOVE "CL-PD-1STPAY-DATE" TO D01-TEXT
              MOVE OCLPD-POFFDATE     TO D01-A15 
              MOVE CL-PD-POFFDATE     TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-LEGALDATE NOT = OCLPD-LEGALDATE
              MOVE "CL-PD-1STPAY-DATE" TO D01-TEXT
              MOVE OCLPD-LEGALDATE     TO D01-A15 
              MOVE CL-PD-LEGALDATE     TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-BNKRPTDATE NOT = OCLPD-BNKRPTDATE
              MOVE "CL-PD-1STPAY-DATE" TO D01-TEXT
              MOVE OCLPD-BNKRPTDATE     TO D01-A15 
              MOVE CL-PD-BNKRPTDATE     TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-DEBTAIDATE NOT = OCLPD-DEBTAIDATE
              MOVE "CL-PD-1STPAY-DATE" TO D01-TEXT
              MOVE OCLPD-DEBTAIDATE     TO D01-A15 
              MOVE CL-PD-DEBTAIDATE     TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-REPODATE NOT = OCLPD-REPODATE
              MOVE "CL-PD-1STPAY-DATE" TO D01-TEXT
              MOVE OCLPD-REPODATE     TO D01-A15 
              MOVE CL-PD-REPODATE     TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-REDEEMDATE NOT = OCLPD-REDEEMDATE
              MOVE "CL-PD-1STPAY-DATE" TO D01-TEXT
              MOVE OCLPD-REDEEMDATE     TO D01-A15 
              MOVE CL-PD-REDEEMDATE     TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-SOLDDATE NOT = OCLPD-SOLDDATE
              MOVE "CL-PD-1STPAY-DATE" TO D01-TEXT
              MOVE OCLPD-SOLDDATE      TO D01-A15 
              MOVE CL-PD-SOLDDATE      TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-INS-CONFIRM-DATE NOT = OCLPD-INS-CONFIRMATION
              MOVE "CL-PD-INS-CONFIRM-DATE" TO D01-TEXT
              MOVE OCLPD-INS-CONFIRMATION   TO D01-A15 
              MOVE CL-PD-INS-CONFIRM-DATE   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.
 
           IF CL-PD-APRATE NOT = OCLPD-APRATE
              MOVE "CL-PD-APRATE" TO D01-TEXT
              MOVE OCLPD-APRATE     TO D01-A15-NUM 
              MOVE CL-PD-APRATE     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-EFFRATE NOT = OCLPD-EFFRATE
              MOVE "CL-PD-EFFRATE"   TO D01-TEXT
              MOVE OCLPD-EFFRATE     TO D01-A15-NUM 
              MOVE CL-PD-EFFRATE     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-SMPRATE NOT = OCLPD-SMPRATE
              MOVE "CL-PD-SMPRATE"   TO D01-TEXT
              MOVE OCLPD-SMPRATE     TO D01-A15-NUM 
              MOVE CL-PD-SMPRATE     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-YLDRATE NOT = OCLPD-YLDRATE
              MOVE "CL-PD-YLDRATE"   TO D01-TEXT
              MOVE OCLPD-YLDRATE     TO D01-A15-NUM 
              MOVE CL-PD-YLDRATE     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-OVRDRATE NOT = OCLPD-OVRDRATE
              MOVE "CL-PD-OVRDRATE"   TO D01-TEXT
              MOVE OCLPD-OVRDRATE     TO D01-A15-NUM 
              MOVE CL-PD-OVRDRATE     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-ENDORATE NOT = OCLPD-ENDORATE
              MOVE "CL-PD-ENDORATE"   TO D01-TEXT
              MOVE OCLPD-ENDORATE     TO D01-A15-NUM 
              MOVE CL-PD-ENDORATE     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-JDRATE NOT = OCLPD-JDRATE
              MOVE "CL-PD-JDRATE"   TO D01-TEXT
              MOVE OCLPD-JDRATE     TO D01-A15-NUM 
              MOVE CL-PD-JDRATE     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-ACCRATE NOT = OCLPD-ACCRATE
              MOVE "CL-PD-ACCRATE"   TO D01-TEXT
              MOVE OCLPD-ACCRATE     TO D01-A15-NUM 
              MOVE CL-PD-ACCRATE     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-REDURATE NOT = OCLPD-REDURATE
              MOVE "CL-PD-REDURATE"   TO D01-TEXT
              MOVE OCLPD-REDURATE     TO D01-A15-NUM 
              MOVE CL-PD-REDURATE     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-SERVPER NOT = OCLPD-SERVPER
              MOVE "CL-PD-SERVPER"   TO D01-TEXT
              MOVE OCLPD-SERVPER     TO D01-A15-NUM 
              MOVE CL-PD-SERVPER     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-SELLPR NOT = OCLPD-SELLPR
              MOVE "CL-PD-SELLPR"   TO D01-TEXT
              MOVE OCLPD-SELLPR     TO D01-A15-NUM 
              MOVE CL-PD-SELLPR     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-DOWNPY NOT = OCLPD-DOWNPY
              MOVE "CL-PD-DOWNPY"   TO D01-TEXT
              MOVE OCLPD-DOWNPY     TO D01-A15-NUM 
              MOVE CL-PD-DOWNPY     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-TRADEIN NOT = OCLPD-TRADEIN
              MOVE "CL-PD-TRADEIN" TO D01-TEXT
              MOVE OCLPD-TRADEIN   TO D01-A15-NUM 
              MOVE CL-PD-TRADEIN   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-LNAMT NOT = OCLPD-LNAMT
              MOVE "CL-PD-LNAMT" TO D01-TEXT
              MOVE OCLPD-LNAMT   TO D01-A15-NUM 
              MOVE CL-PD-LNAMT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-FINCHG NOT = OCLPD-FINCHG
              MOVE "CL-PD-FINCHG" TO D01-TEXT
              MOVE OCLPD-FINCHG   TO D01-A15-NUM 
              MOVE CL-PD-FINCHG   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-INTCHG NOT = OCLPD-INTCHG
              MOVE "CL-PD-INTCHG" TO D01-TEXT
              MOVE OCLPD-INTCHG   TO D01-A15-NUM 
              MOVE CL-PD-INTCHG   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-SERCHG NOT = OCLPD-SERCHG
              MOVE "CL-PD-SERCHG" TO D01-TEXT
              MOVE OCLPD-SERCHG   TO D01-A15-NUM 
              MOVE CL-PD-SERCHG   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-EXTCHG NOT = OCLPD-EXTCHG
              MOVE "CL-PD-EXTCHG" TO D01-TEXT
              MOVE OCLPD-EXTCHG   TO D01-A15-NUM 
              MOVE CL-PD-EXTCHG   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-MAINTFEE NOT = OCLPD-MAINTFEE
              MOVE "CL-PD-MAINTFEE" TO D01-TEXT
              MOVE OCLPD-MAINTFEE   TO D01-A15-NUM 
              MOVE CL-PD-MAINTFEE   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-POINTS NOT = OCLPD-POINTS
              MOVE "CL-PD-POINTS" TO D01-TEXT
              MOVE OCLPD-POINTS   TO D01-A15-NUM 
              MOVE CL-PD-POINTS   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-PROCEEDS NOT = OCLPD-PROCEEDS
              MOVE "CL-PD-PROCEEDS" TO D01-TEXT
              MOVE OCLPD-PROCEEDS   TO D01-A15-NUM 
              MOVE CL-PD-PROCEEDS   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-ORGTERM NOT = OCLPD-ORGTERM
              MOVE "CL-PD-ORGTERM" TO D01-TEXT
              MOVE OCLPD-ORGTERM   TO D01-A15
              MOVE CL-PD-ORGTERM   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-1STPYAMT NOT = OCLPD-1STPYAMT
              MOVE "CL-PD-1STPYAMT" TO D01-TEXT
              MOVE OCLPD-1STPYAMT   TO D01-A15-NUM 
              MOVE CL-PD-1STPYAMT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-NOREGPY NOT = OCLPD-NOREGPY
              MOVE "CL-PD-NOREGPY" TO D01-TEXT
              MOVE OCLPD-NOREGPY   TO D01-A15
              MOVE CL-PD-NOREGPY   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-REGPYAMT NOT = OCLPD-REGPYAMT
              MOVE "CL-PD-REGPYAMT" TO D01-TEXT
              MOVE OCLPD-REGPYAMT   TO D01-A15-NUM 
              MOVE CL-PD-REGPYAMT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-LASTPYAMT NOT = OCLPD-LASTPYAMT
              MOVE "CL-PD-LASTPYAMT" TO D01-TEXT
              MOVE OCLPD-LASTPYAMT   TO D01-A15-NUM 
              MOVE CL-PD-LASTPYAMT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.


           IF CL-PD-PROCASH NOT = OCLPD-PROCASH
              MOVE "CL-PD-PROCASH" TO D01-TEXT
              MOVE OCLPD-PROCASH   TO D01-A15-NUM 
              MOVE CL-PD-PROCASH   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OCLPD-TILLNO = SPACE
               MOVE "0" TO COMP-TILLNO
           ELSE
           IF OCLPD-TILLNO = "1"
              MOVE 1  TO COMP-TILLNO
           ELSE
           IF OCLPD-TILLNO = "2"
              MOVE 2  TO COMP-TILLNO
           ELSE
           IF OCLPD-TILLNO = "3"
              MOVE 3  TO COMP-TILLNO
           ELSE
           IF OCLPD-TILLNO = "4"
              MOVE 4  TO COMP-TILLNO
           ELSE
           IF OCLPD-TILLNO = "5"
              MOVE 5  TO COMP-TILLNO
           ELSE
           IF OCLPD-TILLNO = "6"
              MOVE 6  TO COMP-TILLNO
           ELSE
           IF OCLPD-TILLNO = "7"
              MOVE 7  TO COMP-TILLNO
           ELSE
           IF OCLPD-TILLNO = "8"
              MOVE 8  TO COMP-TILLNO
           ELSE
           IF OCLPD-TILLNO = "9"
              MOVE 9  TO COMP-TILLNO
           ELSE
           IF OCLPD-TILLNO = "A"
              MOVE 10 TO COMP-TILLNO
           ELSE
           IF OCLPD-TILLNO = "B"
              MOVE 11 TO COMP-TILLNO
           ELSE
           IF OCLPD-TILLNO = "C"
              MOVE 12 TO COMP-TILLNO
           ELSE
           IF OCLPD-TILLNO = "D"
              MOVE 13 TO COMP-TILLNO
           ELSE
           IF OCLPD-TILLNO = "E"
              MOVE 14 TO COMP-TILLNO
           ELSE
           IF OCLPD-TILLNO = "F"
              MOVE 15 TO COMP-TILLNO
           ELSE
           IF OCLPD-TILLNO = "G"
              MOVE 16 TO COMP-TILLNO
           ELSE
           IF OCLPD-TILLNO = "H"
              MOVE 17 TO COMP-TILLNO
           ELSE
           IF OCLPD-TILLNO = "I"
              MOVE 18 TO COMP-TILLNO
           ELSE
           IF OCLPD-TILLNO = "J"
              MOVE 19 TO COMP-TILLNO
           ELSE
           IF OCLPD-TILLNO = "K"
              MOVE 20 TO COMP-TILLNO
           ELSE
           IF OCLPD-TILLNO = "L"
              MOVE 21 TO COMP-TILLNO
           ELSE
           IF OCLPD-TILLNO = "M"
              MOVE 22 TO COMP-TILLNO
           ELSE
           IF OCLPD-TILLNO = "N"
              MOVE 23 TO COMP-TILLNO
           ELSE
           IF OCLPD-TILLNO = "O"
              MOVE 24 TO COMP-TILLNO
           ELSE
           IF OCLPD-TILLNO = "P"
              MOVE 25 TO COMP-TILLNO
           ELSE
           IF OCLPD-TILLNO = "Q"
              MOVE 26 TO COMP-TILLNO
           ELSE
           IF OCLPD-TILLNO = "R"
              MOVE 27 TO COMP-TILLNO
           ELSE
           IF OCLPD-TILLNO = "S"
              MOVE 28 TO COMP-TILLNO
           ELSE
           IF OCLPD-TILLNO = "T"
              MOVE 29 TO COMP-TILLNO
           ELSE
           IF OCLPD-TILLNO = "U"
              MOVE 30 TO COMP-TILLNO
           ELSE
           IF OCLPD-TILLNO = "V"
              MOVE 31 TO COMP-TILLNO
           ELSE
           IF OCLPD-TILLNO = "W"
              MOVE 32 TO COMP-TILLNO
           ELSE
           IF OCLPD-TILLNO = "X"
              MOVE 33 TO COMP-TILLNO
           ELSE
           IF OCLPD-TILLNO = "Y"
              MOVE 34 TO COMP-TILLNO
           ELSE
           IF OCLPD-TILLNO = "Z"
              MOVE 35 TO COMP-TILLNO.
           IF CL-PD-TILLNO NOT = COMP-TILLNO
              MOVE "CL-PD-TILLNO" TO D01-TEXT
              MOVE OCLPD-TILLNO   TO D01-A15
              MOVE CL-PD-TILLNO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-STATE-FLAG NOT = OCLPD-STATE-FLAG
              MOVE "CL-PD-STATE-FLAG" TO D01-TEXT
              MOVE OCLPD-STATE-FLAG   TO D01-A15
              MOVE CL-PD-STATE-FLAG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-STATE-WORKER NOT = OCLPD-STATE-WORKER
              MOVE "CL-PD-STATE-WORKER" TO D01-TEXT
              MOVE OCLPD-STATE-WORKER   TO D01-A15-NUM 
              MOVE CL-PD-STATE-WORKER   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-STATE-WORKER2 NOT = OCLPD-STATE-WORKER2
              MOVE "CL-PD-STATE-WORKER2" TO D01-TEXT
              MOVE OCLPD-STATE-WORKER2   TO D01-A15-NUM 
              MOVE CL-PD-STATE-WORKER2   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB2 FROM 1 BY 1 UNTIL SUB2 > 10
              IF CL-PD-FEECODE(SUB2) NOT = OCLPD-FEECODE(SUB2)
                 STRING "CL-PD-FEECODE(",SUB2,")" INTO D01-TEXT
                 MOVE OCLPD-FEECODE(SUB2)  TO D01-A15
                 MOVE CL-PD-FEECODE(SUB2)  TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CL-PD-FEEAMT(SUB2) NOT = OCLPD-FEEAMT(SUB2)
                 STRING "CL-PD-FEEAMT(",SUB2,")" INTO D01-TEXT
                 MOVE OCLPD-FEEAMT(SUB2)  TO D01-A15-NUM
                 MOVE CL-PD-FEEAMT(SUB2)  TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CL-PD-FEEOURS(SUB2) NOT = OCLPD-FEEOURS(SUB2)
                 STRING "CL-PD-FEEOURS(",SUB2,")" INTO D01-TEXT
                 MOVE OCLPD-FEEOURS(SUB2)  TO D01-A15
                 MOVE CL-PD-FEEOURS(SUB2)  TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CL-PD-FEEFINCHG(SUB2) NOT = OCLPD-FEEFINCHG(SUB2)
                 STRING "CL-PD-FEEFINCHG(",SUB2,")" INTO D01-TEXT
                 MOVE OCLPD-FEEFINCHG(SUB2)  TO D01-A15
                 MOVE CL-PD-FEEFINCHG(SUB2)  TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.

           IF CL-PD-INSURED NOT = OCLPD-INSURED
              MOVE "CL-PD-INSURED" TO D01-TEXT
              MOVE OCLPD-INSURED   TO D01-A15
              MOVE CL-PD-INSURED   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-INSTYPES NOT = OCLPD-INSTYPES
              MOVE "CL-PD-INSTYPES" TO D01-TEXT
              MOVE OCLPD-INSTYPES   TO D01-A15  
              MOVE CL-PD-INSTYPES   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-INSELIGIBLE NOT = OCLPD-INSELIGIBLE
              MOVE "CL-PD-INSELIGIBLE" TO D01-TEXT
              MOVE OCLPD-INSELIGIBLE   TO D01-A15
              MOVE CL-PD-INSELIGIBLE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 8
              IF CL-PD-INSCOMP(SUB1) NOT = OCLPD-INSCOMP(SUB1)
                 STRING "CL-PD-INSCOMP(",SUB1,")" INTO D01-TEXT
                 MOVE OCLPD-INSCOMP(SUB1)  TO D01-A15-NUM
                 MOVE CL-PD-INSCOMP(SUB1)  TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CL-PD-INSOURS(SUB1) NOT = OCLPD-INSOURS(SUB1)
                 STRING "CL-PD-INSOURS(",SUB1,")" INTO D01-TEXT
                 MOVE OCLPD-INSOURS(SUB1)  TO D01-A15-NUM
                 MOVE CL-PD-INSOURS(SUB1)  TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
                IF CL-PD-INSEFF-DATE(SUB1) NOT = OCLPD-INSEFF(SUB1)
                 STRING "CL-PD-INSEFF-DATE(",SUB1,")" INTO D01-TEXT
                 MOVE OCLPD-INSEFF(SUB1)  TO D01-A15
                 MOVE CL-PD-INSEFF-DATE(SUB1)  TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
               IF CL-PD-INSEXP-DATE(SUB1) NOT = OCLPD-INSEXP(SUB1)
                 STRING "CL-PD-INSEXP-DATE(",SUB1,")" INTO D01-TEXT
                 MOVE OCLPD-INSEXP(SUB1)  TO D01-A15
                 MOVE CL-PD-INSEXP-DATE(SUB1)  TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
               IF CL-PD-INSPREM(SUB1) NOT = OCLPD-INSPREM(SUB1)
                 STRING "CL-PD-INSPREM(",SUB1,")" INTO D01-TEXT
                 MOVE OCLPD-INSPREM(SUB1)  TO D01-A15-NUM
                 MOVE CL-PD-INSPREM(SUB1)  TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
               IF CL-PD-INSCOVR(SUB1) NOT = OCLPD-INSCOVR(SUB1)
                 STRING "CL-PD-INSCOVR(",SUB1,")" INTO D01-TEXT
                 MOVE OCLPD-INSCOVR(SUB1)  TO D01-A15-NUM
                 MOVE CL-PD-INSCOVR(SUB1)  TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
               IF CL-PD-INSCOMM(SUB1) NOT = OCLPD-INSCOMM(SUB1)
                 STRING "CL-PD-INSCOMM(",SUB1,")" INTO D01-TEXT
                 MOVE OCLPD-INSCOMM(SUB1)  TO D01-A15-NUM
                 MOVE CL-PD-INSCOMM(SUB1)  TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
                IF CL-PD-INSANTICERN(SUB1) NOT = OCLPD-INSANTICERN(SUB1)
                 STRING "CL-PD-INSANTICERN(",SUB1,")" INTO D01-TEXT
                 MOVE OCLPD-INSANTICERN(SUB1)  TO D01-A15-NUM
                 MOVE CL-PD-INSANTICERN(SUB1)  TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
                IF CL-PD-INSANTICADJ(SUB1) NOT = OCLPD-INSANTICADJ(SUB1)
                 STRING "CL-PD-INSANTICADJ(",SUB1,")" INTO D01-TEXT
                 MOVE OCLPD-INSANTICADJ(SUB1)  TO D01-A15-NUM
                 MOVE CL-PD-INSANTICADJ(SUB1)  TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
               IF CL-PD-INSUFERN(SUB1) NOT = OCLPD-INSUFERN(SUB1)
                 STRING "CL-PD-INSUFERN(",SUB1,")" INTO D01-TEXT
                 MOVE OCLPD-INSUFERN(SUB1)  TO D01-A15-NUM
                 MOVE CL-PD-INSUFERN(SUB1)  TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
               IF CL-PD-INSACCERN(SUB1) NOT = OCLPD-INSACCERN(SUB1)
                 STRING "CL-PD-INSACCERN(",SUB1,")" INTO D01-TEXT
                 MOVE OCLPD-INSACCERN(SUB1)  TO D01-A15-NUM
                 MOVE CL-PD-INSACCERN(SUB1)  TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
               IF CL-PD-INSEARN-ORDER(SUB1) NOT = 
                    OCLPD-INSEARN-ORDER(SUB1)
                 STRING "CL-PD-INSEARN-ORDER(",SUB1,")" INTO D01-TEXT
                 MOVE OCLPD-INSEARN-ORDER(SUB1)  TO D01-A15
                 MOVE CL-PD-INSEARN-ORDER(SUB1)  TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
               IF CL-PD-INSFINCHG(SUB1) NOT = OCLPD-INSFINCHG(SUB1)
                 STRING "CL-PD-INSFINCHG(",SUB1,")" INTO D01-TEXT
                 MOVE OCLPD-INSFINCHG(SUB1)  TO D01-A15
                 MOVE CL-PD-INSFINCHG(SUB1)  TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.

           IF CL-PD-PY-YYYYMM-RECDEL NOT = OCLPD-PYMMYY-RECDEL
              MOVE "CL-PD-PY-YYYYMM-RECDEL" TO D01-TEXT
              MOVE OCLPD-PYMMYY-RECDEL      TO D01-A15
              MOVE CL-PD-PY-YYYYMM-RECDEL   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-MTD-PAYS-RECDEL NOT = OCLPD-MTD-PAYS-RECDEL
              MOVE "CL-PD-MTD-PAYS-RECDEL" TO D01-TEXT
              MOVE OCLPD-MTD-PAYS-RECDEL   TO D01-A15-NUM 
              MOVE CL-PD-MTD-PAYS-RECDEL   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-SHIP-SAME-FG NOT = OCLPD-SHIP-SAME-FG
              MOVE "CL-PD-SHIP-SAME-FG" TO D01-TEXT
              MOVE OCLPD-SHIP-SAME-FG   TO D01-A15
              MOVE CL-PD-SHIP-SAME-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-AD-D-INS-COVR NOT = OCLPD-AD-D-INS-COVR
              MOVE "CL-PD-AD-D-INS-COVR" TO D01-TEXT
              MOVE OCLPD-AD-D-INS-COVR   TO D01-A15-NUM 
              MOVE CL-PD-AD-D-INS-COVR   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-LPO NOT = OCLPD-LPO
              MOVE "CL-PD-LPO" TO D01-TEXT
              MOVE OCLPD-LPO   TO D01-A15
              MOVE CL-PD-LPO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-REP-CD NOT = OCLPD-REP-CD
              MOVE "CL-PD-REP-CD" TO D01-TEXT
              MOVE OCLPD-REP-CD   TO D01-A15
              MOVE CL-PD-REP-CD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-PROCESS-CD NOT = OCLPD-PROCESS-CD
              MOVE "CL-PD-PROCESS-CD" TO D01-TEXT
              MOVE OCLPD-PROCESS-CD   TO D01-A15
              MOVE CL-PD-PROCESS-CD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-PRIOR-RN-FOR10-DATE NOT = OCLPD-PRIOR-RN-FOR10
              MOVE "CL-PD-PRIOR-RN-FOR10-DATE" TO D01-TEXT
              MOVE OCLPD-PRIOR-RN-FOR10        TO D01-A15
              MOVE CL-PD-PRIOR-RN-FOR10-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-LAST-RN-FOR10-DATE NOT = OCLPD-LAST-RN-FOR10
              MOVE "CL-PD-LAST-RN-FOR10-DATE" TO D01-TEXT
              MOVE OCLPD-LAST-RN-FOR10        TO D01-A15
              MOVE CL-PD-LAST-RN-FOR10-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OCLPD-WCBC-CREDITLIM NOT NUMERIC
              MOVE 0 TO OCLPD-WCBC-CREDITLIM.
           IF CL-PD-WCBC-CREDITLIM NOT = OCLPD-WCBC-CREDITLIM
              MOVE "CL-PD-WCBC-CREDITLIM" TO D01-TEXT
              MOVE OCLPD-WCBC-CREDITLIM   TO D01-A15
              MOVE CL-PD-WCBC-CREDITLIM   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-FOLLOWUP-CD NOT = OCLPD-FOLLOWUP-CD
              MOVE "CL-PD-FOLLOWUP-CD" TO D01-TEXT
              MOVE OCLPD-FOLLOWUP-CD   TO D01-A15
              MOVE CL-PD-FOLLOWUP-CD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-MIP-FG NOT = OCLPD-MIP-FG
              MOVE "CL-PD-MIP-FG" TO D01-TEXT
              MOVE OCLPD-MIP-FG   TO D01-A15
              MOVE CL-PD-MIP-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OCLPD-MIP-MTD-CLAHPAID NOT NUMERIC
              MOVE 0 TO OCLPD-MIP-MTD-CLAHPAID. 
           IF CL-PD-MIP-MTD-CLAHPAID NOT = OCLPD-MIP-MTD-CLAHPAID
              MOVE "CL-PD-MIP-MTD-CLAHPAID" TO D01-TEXT
              MOVE OCLPD-MIP-MTD-CLAHPAID   TO D01-A15-NUM 
              MOVE CL-PD-MIP-MTD-CLAHPAID   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OCLPD-MIP-CLPDTH NOT NUMERIC 
            OR OCLPD-MIP-CLPDTH = 202020
              MOVE 0 TO OCLPD-MIP-CLPDTH.
           IF CL-PD-MIP-CLPDTH-DATE NOT = OCLPD-MIP-CLPDTH
              MOVE "CL-PD-MIP-CLPDTH-DATE" TO D01-TEXT
              MOVE OCLPD-MIP-CLPDTH        TO D01-A15
              MOVE CL-PD-MIP-CLPDTH-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OCLPD-MIP-TOT-CLPAID NOT NUMERIC
              MOVE 0 TO OCLPD-MIP-TOT-CLPAID.
           IF CL-PD-MIP-TOT-CLPAID NOT = OCLPD-MIP-TOT-CLPAID
              MOVE "CL-PD-MIP-TOT-CLPAID" TO D01-TEXT
              MOVE OCLPD-MIP-TOT-CLPAID   TO D01-A15-NUM 
              MOVE CL-PD-MIP-TOT-CLPAID   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OCLPD-MIP-AHPDTH NOT NUMERIC
            OR OCLPD-MIP-AHPDTH = 202020
              MOVE 0 TO OCLPD-MIP-AHPDTH.
           IF CL-PD-MIP-AHPDTH-DATE NOT = OCLPD-MIP-AHPDTH
              MOVE "CL-PD-MIP-AHPDTH-DATE" TO D01-TEXT
              MOVE OCLPD-MIP-AHPDTH        TO D01-A15
              MOVE CL-PD-MIP-AHPDTH-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OCLPD-MIP-TOT-AHPAID NOT NUMERIC
              MOVE 0 TO OCLPD-MIP-TOT-AHPAID.
           IF CL-PD-MIP-TOT-AHPAID NOT = OCLPD-MIP-TOT-AHPAID
              MOVE "CL-PD-MIP-TOT-AHPAID" TO D01-TEXT
              MOVE OCLPD-MIP-TOT-AHPAID   TO D01-A15-NUM 
              MOVE CL-PD-MIP-TOT-AHPAID   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-RESCISSION NOT = OCLPD-RESCISSION
              MOVE "CL-PD-RESCISSION" TO D01-TEXT
              MOVE OCLPD-RESCISSION   TO D01-A15
              MOVE CL-PD-RESCISSION   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-INTERIM-TRW NOT = OCLPD-INTERIM-TRW
              MOVE "CL-PD-INTERIM-TRW" TO D01-TEXT
              MOVE OCLPD-INTERIM-TRW   TO D01-A15
              MOVE CL-PD-INTERIM-TRW   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-DLNO NOT = OCLPD-DLNO
              MOVE "CL-PD-DLNO" TO D01-TEXT
              MOVE OCLPD-DLNO   TO D01-A15-NUM 
              MOVE CL-PD-DLNO   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-DLPOOL NOT = OCLPD-DLPOOL
              MOVE "CL-PD-DLPOOL" TO D01-TEXT
              MOVE OCLPD-DLPOOL   TO D01-A15
              MOVE CL-PD-DLPOOL   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-DLRECOURSE NOT = OCLPD-DLRECOURSE
              MOVE "CL-PD-DLRECOURSE" TO D01-TEXT
              MOVE OCLPD-DLRECOURSE   TO D01-A15
              MOVE CL-PD-DLRECOURSE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-DLPARTRCAMT NOT = OCLPD-DLPARTRCAMT
              MOVE "CL-PD-DLPARTRCAMT" TO D01-TEXT
              MOVE OCLPD-DLPARTRCAMT   TO D01-A15-NUM 
              MOVE CL-PD-DLPARTRCAMT   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-ORIGPURAMT NOT = OCLPD-ORIGPURAMT
              MOVE "CL-PD-ORIGPURAMT" TO D01-TEXT
              MOVE OCLPD-ORIGPURAMT   TO D01-A15-NUM 
              MOVE CL-PD-ORIGPURAMT   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-PURBAL NOT = OCLPD-PURBAL
              MOVE "CL-PD-PURBAL" TO D01-TEXT
              MOVE OCLPD-PURBAL   TO D01-A15-NUM 
              MOVE CL-PD-PURBAL   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 3
              IF CL-PD-DLRVPRIN(SUB1) NOT = OCLPD-DLRVPRIN(SUB1)
                 STRING "CL-PD-DLRVPRIN(",SUB1,")" INTO D01-TEXT
                 MOVE OCLPD-DLRVPRIN(SUB1)  TO D01-A15-NUM
                 MOVE CL-PD-DLRVPRIN(SUB1)  TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CL-PD-DLRVCHGS(SUB1) NOT = OCLPD-DLRVCHGS(SUB1)
                 STRING "CL-PD-DLRVCHGS(",SUB1,")" INTO D01-TEXT
                 MOVE OCLPD-DLRVCHGS(SUB1)  TO D01-A15-NUM
                 MOVE CL-PD-DLRVCHGS(SUB1)  TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF 
           END-PERFORM.

           IF CL-PD-DLPARVHELD NOT = OCLPD-DLPARVHELD
              MOVE "CL-PD-DLPARVHELD" TO D01-TEXT
              MOVE OCLPD-DLPARVHELD   TO D01-A15-NUM 
              MOVE CL-PD-DLPARVHELD   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-DLPARVPAID NOT = OCLPD-DLPARVPAID
              MOVE "CL-PD-DLPARVPAID" TO D01-TEXT
              MOVE OCLPD-DLPARVPAID   TO D01-A15-NUM 
              MOVE CL-PD-DLPARVPAID   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-DLDISC NOT = OCLPD-DLDISC
              MOVE "CL-PD-DLDISC" TO D01-TEXT
              MOVE OCLPD-DLDISC   TO D01-A15-NUM 
              MOVE CL-PD-DLDISC   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE OCLPD-LCPDTH TO DATE-MMDDYY.
           IF (DATE-MMDDYY-MM = 0) AND (DATE-MMDDYY-YY = 0) 
               MOVE OCLPD-LOANDATE TO OCLPD-LCPDTH.
           MOVE OCLPD-LCPDTH TO DATE-MMDDYY.
           PERFORM CONVERT-MMDDYY-TO-YYYYMMDD.
           IF CL-PD-LCPDTH-DATE NOT = DATE-YYYYMMDD
              MOVE "CL-PD-LCPDTH-DATE" TO D01-TEXT
              MOVE OCLPD-LCPDTH        TO D01-A15
              MOVE CL-PD-LCPDTH-DATE   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-INTPDTH-DATE NOT = OCLPD-INTPDTH
              MOVE "CL-PD-INTPDTH-DATE" TO D01-TEXT
              MOVE OCLPD-INTPDTH        TO D01-A15
              MOVE CL-PD-INTPDTH-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-CURBAL NOT = OCLPD-CURBAL
              MOVE "CL-PD-CURBAL" TO D01-TEXT
              MOVE OCLPD-CURBAL   TO D01-A15-NUM 
              MOVE CL-PD-CURBAL   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-INTBAL NOT = OCLPD-INTBAL
              MOVE "CL-PD-INTBAL" TO D01-TEXT
              MOVE OCLPD-INTBAL   TO D01-A15-NUM 
              MOVE CL-PD-INTBAL   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-LCBAL NOT = OCLPD-LCBAL
              MOVE "CL-PD-LCBAL" TO D01-TEXT
              MOVE OCLPD-LCBAL   TO D01-A15-NUM 
              MOVE CL-PD-LCBAL   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-OTHBAL NOT = OCLPD-OTHBAL
              MOVE "CL-PD-OTHBAL" TO D01-TEXT
              MOVE OCLPD-OTHBAL   TO D01-A15-NUM 
              MOVE CL-PD-OTHBAL   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-OT2BAL NOT = OCLPD-OT2BAL
              MOVE "CL-PD-OT2BAL" TO D01-TEXT
              MOVE OCLPD-OT2BAL   TO D01-A15-NUM 
              MOVE CL-PD-OT2BAL   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-LCPAID NOT = OCLPD-LCPAID
              MOVE "CL-PD-LCPAID" TO D01-TEXT
              MOVE OCLPD-LCPAID   TO D01-A15-NUM 
              MOVE CL-PD-LCPAID   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-LCPARTIALS NOT = OCLPD-LCPARTIALS
              MOVE "CL-PD-LCPARTIALS" TO D01-TEXT
              MOVE OCLPD-LCPARTIALS   TO D01-A15-NUM 
              MOVE CL-PD-LCPARTIALS   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-STMT-COUNTER NOT = OCLPD-STMT-COUNTER
              MOVE "CL-PD-STMT-COUNTER" TO D01-TEXT
              MOVE OCLPD-STMT-COUNTER   TO D01-A15
              MOVE CL-PD-STMT-COUNTER   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-MONTHEND-BAL NOT = OCLPD-MONTHEND-BAL
              MOVE "CL-PD-MONTHEND-BAL" TO D01-TEXT
              MOVE OCLPD-MONTHEND-BAL   TO D01-A15-NUM 
              MOVE CL-PD-MONTHEND-BAL   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-ANTICTERM NOT = OCLPD-ANTICTERM
              MOVE "CL-PD-ANTICTERM" TO D01-TEXT
              MOVE OCLPD-ANTICTERM   TO D01-A15
              MOVE CL-PD-ANTICTERM   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 6
              IF CL-PD-ANTICERN(SUB1) NOT = OCLPD-ANTICERN(SUB1)
                 STRING "CL-PD-ANTICERN(",SUB1,")" INTO D01-TEXT
                 MOVE OCLPD-ANTICERN(SUB1)  TO D01-A15-NUM
                 MOVE CL-PD-ANTICERN(SUB1)  TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CL-PD-ANTICADJ(SUB1) NOT = OCLPD-ANTICADJ(SUB1)
                 STRING "CL-PD-ANTICADJ(",SUB1,")" INTO D01-TEXT
                 MOVE OCLPD-ANTICADJ(SUB1)  TO D01-A15-NUM
                 MOVE CL-PD-ANTICADJ(SUB1)  TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF 
              IF CL-PD-UFERN(SUB1) NOT = OCLPD-UFERN(SUB1)
                 STRING "CL-PD-UFERN(",SUB1,")" INTO D01-TEXT
                 MOVE OCLPD-UFERN(SUB1)  TO D01-A15-NUM
                 MOVE CL-PD-UFERN(SUB1)  TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF 
              IF CL-PD-ACCERN(SUB1) NOT = OCLPD-ACCERN(SUB1)
                 STRING "CL-PD-ACCERN(",SUB1,")" INTO D01-TEXT
                 MOVE OCLPD-ACCERN(SUB1)  TO D01-A15-NUM
                 MOVE CL-PD-ACCERN(SUB1)  TO D01-S35-NUM
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF 
              IF CL-PD-EARN-ORDER(SUB1) NOT = OCLPD-EARN-ORDER(SUB1)
                 STRING "CL-PD-EARN-ORDER(",SUB1,")" INTO D01-TEXT
                 MOVE OCLPD-EARN-ORDER(SUB1)  TO D01-A15
                 MOVE CL-PD-EARN-ORDER(SUB1)  TO D01-S35
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF 
           END-PERFORM.

           IF CL-PD-PRIOR-UNEARNED NOT = OCLPD-PRIOR-UNEARNED
              MOVE "CL-PD-PRIOR-UNEARNED" TO D01-TEXT
              MOVE OCLPD-PRIOR-UNEARNED   TO D01-A15-NUM 
              MOVE CL-PD-PRIOR-UNEARNED   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-ERNDELQ-STOP NOT = OCLPD-ERNDELQ-STOP
              MOVE "CL-PD-ERNDELQ-STOP" TO D01-TEXT
              MOVE OCLPD-ERNDELQ-STOP   TO D01-A15
              MOVE CL-PD-ERNDELQ-STOP   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-NONACRERN NOT = OCLPD-NONACRERN
              MOVE "CL-PD-NONACRERN" TO D01-TEXT
              MOVE OCLPD-NONACRERN   TO D01-A15-NUM 
              MOVE CL-PD-NONACRERN   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-PRIOR-NONACRERN NOT = OCLPD-PRIOR-NONACRERN
              MOVE "CL-PD-PRIOR-NONACRERN" TO D01-TEXT
              MOVE OCLPD-PRIOR-NONACRERN   TO D01-A15-NUM 
              MOVE CL-PD-PRIOR-NONACRERN   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-ERN-IB-PDTH-DATE NOT = OCLPD-ERN-IB-PDTH
              MOVE "CL-PD-ERN-IB-PDTH-DATE" TO D01-TEXT
              MOVE OCLPD-ERN-IB-PDTH        TO D01-A15
              MOVE CL-PD-ERN-IB-PDTH-DATE   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-ERN-IB-PRINBAL NOT = OCLPD-ERN-IB-PRINBAL
              MOVE "CL-PD-ERN-IB-PRINBAL" TO D01-TEXT
              MOVE OCLPD-ERN-IB-PRINBAL   TO D01-A15-NUM 
              MOVE CL-PD-ERN-IB-PRINBAL   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-ERN-IB-INTOWE NOT = OCLPD-ERN-IB-INTOWE
              MOVE "CL-PD-ERN-IB-INTOWE" TO D01-TEXT
              MOVE OCLPD-ERN-IB-INTOWE   TO D01-A15-NUM 
              MOVE CL-PD-ERN-IB-INTOWE   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-TOTERN NOT = OCLPD-TOTERN
              MOVE "CL-PD-TOTERN" TO D01-TEXT
              MOVE OCLPD-TOTERN   TO D01-A15-NUM 
              MOVE CL-PD-TOTERN   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-TOTOTH NOT = OCLPD-TOTOTH
              MOVE "CL-PD-TOTOTH" TO D01-TEXT
              MOVE OCLPD-TOTOTH   TO D01-A15-NUM 
              MOVE CL-PD-TOTOTH   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-TOTINT NOT = OCLPD-TOTINT
              MOVE "CL-PD-TOTINT" TO D01-TEXT
              MOVE OCLPD-TOTINT   TO D01-A15-NUM 
              MOVE CL-PD-TOTINT   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-TOTLCHG NOT = OCLPD-TOTLCHG
              MOVE "CL-PD-TOTLCHG" TO D01-TEXT
              MOVE OCLPD-TOTLCHG   TO D01-A15-NUM 
              MOVE CL-PD-TOTLCHG   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-TOTDEF NOT = OCLPD-TOTDEF
              MOVE "CL-PD-TOTDEF" TO D01-TEXT
              MOVE OCLPD-TOTDEF   TO D01-A15-NUM 
              MOVE CL-PD-TOTDEF   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-TOTSERACCT NOT = OCLPD-TOTSERACCT
              MOVE "CL-PD-TOTSERACCT" TO D01-TEXT
              MOVE OCLPD-TOTSERACCT   TO D01-A15-NUM 
              MOVE CL-PD-TOTSERACCT   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-LCWAIVERS NOT = OCLPD-LCWAIVERS
              MOVE "CL-PD-LCWAIVERS" TO D01-TEXT
              MOVE OCLPD-LCWAIVERS   TO D01-A15-NUM 
              MOVE CL-PD-LCWAIVERS   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-INTWAIVERS NOT = OCLPD-INTWAIVERS
              MOVE "CL-PD-INTWAIVERS" TO D01-TEXT
              MOVE OCLPD-INTWAIVERS   TO D01-A15-NUM 
              MOVE CL-PD-INTWAIVERS   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-TOTPAYMNTD NOT = OCLPD-TOTPAYMNTD
              MOVE "CL-PD-TOTPAYMNTD" TO D01-TEXT
              MOVE OCLPD-TOTPAYMNTD   TO D01-A15-NUM 
              MOVE CL-PD-TOTPAYMNTD   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-TOTEXCPAYMNTD NOT = OCLPD-TOTEXCPAYMNTD
              MOVE "CL-PD-TOTEXCPAYMNTD" TO D01-TEXT
              MOVE OCLPD-TOTEXCPAYMNTD   TO D01-A15-NUM 
              MOVE CL-PD-TOTEXCPAYMNTD   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.


           IF CL-PD-TOTNODEF NOT = OCLPD-TOTNODEF
              MOVE "CL-PD-TOTNODEF" TO D01-TEXT
              MOVE OCLPD-TOTNODEF   TO D01-A15
              MOVE CL-PD-TOTNODEF   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-YTDNODEF NOT = OCLPD-YTDNODEF
              MOVE "CL-PD-YTDNODEF" TO D01-TEXT
              MOVE OCLPD-YTDNODEF   TO D01-A15
              MOVE CL-PD-YTDNODEF   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-TOTNOALDEL NOT = OCLPD-TOTNOALDEL
              MOVE "CL-PD-TOTNOALDEL" TO D01-TEXT
              MOVE OCLPD-TOTNOALDEL   TO D01-A15
              MOVE CL-PD-TOTNOALDEL   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-YTDINTPD NOT = OCLPD-YTDINTPD
              MOVE "CL-PD-YTDINTPD" TO D01-TEXT
              MOVE OCLPD-YTDINTPD   TO D01-A15-NUM 
              MOVE CL-PD-YTDINTPD   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-LYRINTPD NOT = OCLPD-LYRINTPD
              MOVE "CL-PD-LYRINTPD" TO D01-TEXT
              MOVE OCLPD-LYRINTPD   TO D01-A15-NUM 
              MOVE CL-PD-LYRINTPD   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-TOTINTPD NOT = OCLPD-TOTINTPD
              MOVE "CL-PD-TOTINTPD" TO D01-TEXT
              MOVE OCLPD-TOTINTPD   TO D01-A15-NUM 
              MOVE CL-PD-TOTINTPD   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-RECASTINT NOT = OCLPD-RECASTINT
              MOVE "CL-PD-RECASTINT" TO D01-TEXT
              MOVE OCLPD-RECASTINT   TO D01-A15-NUM 
              MOVE CL-PD-RECASTINT   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-RECASTPRIN NOT = OCLPD-RECASTPRIN
              MOVE "CL-PD-RECASTPRIN" TO D01-TEXT
              MOVE OCLPD-RECASTPRIN   TO D01-A15-NUM 
              MOVE CL-PD-RECASTPRIN   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-TRW-DOFD-DATE NOT = OCLPD-TRW-DOFD
              MOVE "CL-PD-TRW-DOFD-DATE" TO D01-TEXT
              MOVE OCLPD-TRW-DOFD        TO D01-A15
              MOVE CL-PD-TRW-DOFD-DATE   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-CL-REBATE NOT = OCLPD-CL-REBATE
              MOVE "CL-PD-CL-REBATE" TO D01-TEXT
              MOVE OCLPD-CL-REBATE   TO D01-A15
              MOVE CL-PD-CL-REBATE   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-AH-REBATE NOT = OCLPD-AH-REBATE
              MOVE "CL-PD-AH-REBATE" TO D01-TEXT
              MOVE OCLPD-AH-REBATE   TO D01-A15
              MOVE CL-PD-AH-REBATE   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-PP-REBATE NOT = OCLPD-PP-REBATE
              MOVE "CL-PD-PP-REBATE" TO D01-TEXT
              MOVE OCLPD-PP-REBATE   TO D01-A15
              MOVE CL-PD-PP-REBATE   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-INT-REBATE NOT = OCLPD-INT-REBATE
              MOVE "CL-PD-INT-REBATE" TO D01-TEXT
              MOVE OCLPD-INT-REBATE   TO D01-A15
              MOVE CL-PD-INT-REBATE   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-SC-REBATE NOT = OCLPD-SC-REBATE
              MOVE "CL-PD-SC-REBATE" TO D01-TEXT
              MOVE OCLPD-SC-REBATE   TO D01-A15
              MOVE CL-PD-SC-REBATE   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-MF-REBATE NOT = OCLPD-MF-REBATE
              MOVE "CL-PD-MF-REBATE" TO D01-TEXT
              MOVE OCLPD-MF-REBATE   TO D01-A15
              MOVE CL-PD-MF-REBATE   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-DF-REBATE NOT = OCLPD-DF-REBATE
              MOVE "CL-PD-DF-REBATE" TO D01-TEXT
              MOVE OCLPD-DF-REBATE   TO D01-A15
              MOVE CL-PD-DF-REBATE   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-EX-REBATE NOT = OCLPD-EX-REBATE
              MOVE "CL-PD-EX-REBATE" TO D01-TEXT
              MOVE OCLPD-EX-REBATE   TO D01-A15
              MOVE CL-PD-EX-REBATE   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-O1-REBATE NOT = OCLPD-O1-REBATE
              MOVE "CL-PD-O1-REBATE" TO D01-TEXT
              MOVE OCLPD-O1-REBATE   TO D01-A15
              MOVE CL-PD-O1-REBATE   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-O2-REBATE NOT = OCLPD-O2-REBATE
              MOVE "CL-PD-O2-REBATE" TO D01-TEXT
              MOVE OCLPD-O2-REBATE   TO D01-A15
              MOVE CL-PD-O2-REBATE   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-O3-REBATE NOT = OCLPD-O3-REBATE
              MOVE "CL-PD-O3-REBATE" TO D01-TEXT
              MOVE OCLPD-O3-REBATE   TO D01-A15
              MOVE CL-PD-O3-REBATE   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-O4-REBATE NOT = OCLPD-O4-REBATE
              MOVE "CL-PD-O4-REBATE" TO D01-TEXT
              MOVE OCLPD-O4-REBATE   TO D01-A15
              MOVE CL-PD-O4-REBATE   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-O5-REBATE NOT = OCLPD-O5-REBATE
              MOVE "CL-PD-O5-REBATE" TO D01-TEXT
              MOVE OCLPD-O5-REBATE   TO D01-A15
              MOVE CL-PD-O5-REBATE   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-PLREASON NOT = OCLPD-PLREASON
              MOVE "CL-PD-PLREASON" TO D01-TEXT
              MOVE OCLPD-PLREASON   TO D01-A15
              MOVE CL-PD-PLREASON   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-COLLECTOR NOT = OCLPD-COLLECTOR
              MOVE "CL-PD-COLLECTOR" TO D01-TEXT
              MOVE OCLPD-COLLECTOR   TO D01-A15
              MOVE CL-PD-COLLECTOR   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-ACTIONCD NOT = OCLPD-ACTIONCD
              MOVE "CL-PD-ACTIONCD" TO D01-TEXT
              MOVE OCLPD-ACTIONCD   TO D01-A15
              MOVE CL-PD-ACTIONCD   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-ACTIONCD2 NOT = OCLPD-ACTIONCD2
              MOVE "CL-PD-ACTIONCD2" TO D01-TEXT
              MOVE OCLPD-ACTIONCD2   TO D01-A15
              MOVE CL-PD-ACTIONCD2   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-ATCODE NOT = OCLPD-ATCODE
              MOVE "CL-PD-ATCODE" TO D01-TEXT
              MOVE OCLPD-ATCODE   TO D01-A15
              MOVE CL-PD-ATCODE   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-RECODE NOT = OCLPD-RECODE
              MOVE "CL-PD-RECODE" TO D01-TEXT
              MOVE OCLPD-RECODE   TO D01-A15
              MOVE CL-PD-RECODE   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-LAST-PYPA-DATE NOT = OCLPD-LAST-PYPA-DATE
              MOVE "CL-PD-LAST-PYPA-DATE" TO D01-TEXT
              MOVE OCLPD-LAST-PYPA-DATE   TO D01-A15
              MOVE CL-PD-LAST-PYPA-DATE   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-PROMDAYS-PASTDUE NOT = OCLPD-PROMDAYS-PASTDUE
              MOVE "CL-PD-PROMDAYS-PASTDUE" TO D01-TEXT
              MOVE OCLPD-PROMDAYS-PASTDUE   TO D01-A15
              MOVE CL-PD-PROMDAYS-PASTDUE   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-PROMID NOT = OCLPD-PROMID
              MOVE "CL-PD-PROMID" TO D01-TEXT
              MOVE OCLPD-PROMID   TO D01-A15
              MOVE CL-PD-PROMID   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-PROMDATE NOT = OCLPD-PROMDATE
              MOVE "CL-PD-PROMDATE" TO D01-TEXT
              MOVE OCLPD-PROMDATE   TO D01-A15
              MOVE CL-PD-PROMDATE   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-PROMAMT NOT = OCLPD-PROMAMT
              MOVE "CL-PD-PROMAMT" TO D01-TEXT
              MOVE OCLPD-PROMAMT   TO D01-A15-NUM 
              MOVE CL-PD-PROMAMT   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-DATE-PAID-LAST NOT = OCLPD-DATE-PAID-LAST
              MOVE "CL-PD-DATE-PAID-LAST" TO D01-TEXT
              MOVE OCLPD-DATE-PAID-LAST   TO D01-A15
              MOVE CL-PD-DATE-PAID-LAST   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-AMT-PAID-LAST NOT = OCLPD-AMT-PAID-LAST
              MOVE "CL-PD-AMT-PAID-LAST" TO D01-TEXT
              MOVE OCLPD-AMT-PAID-LAST   TO D01-A15-NUM 
              MOVE CL-PD-AMT-PAID-LAST   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-LASTCFORMNO NOT = OCLPD-LASTCFORMNO
              MOVE "CL-PD-LASTCFORMNO" TO D01-TEXT
              MOVE OCLPD-LASTCFORMNO   TO D01-A15
              MOVE CL-PD-LASTCFORMNO   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-LASTCFORMDATE NOT = OCLPD-LASTCFORMDATE
              MOVE "CL-PD-LASTCFORMDATE" TO D01-TEXT
              MOVE OCLPD-LASTCFORMDATE   TO D01-A15
              MOVE CL-PD-LASTCFORMDATE   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-DELCNT NOT = OCLPD-DELCNT
              MOVE "CL-PD-DELCNT" TO D01-TEXT
              MOVE OCLPD-DELCNT   TO D01-A15
              MOVE CL-PD-DELCNT   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-DELCNT2 NOT = OCLPD-DELCNT2
              MOVE "CL-PD-DELCNT2" TO D01-TEXT
              MOVE OCLPD-DELCNT2   TO D01-A15
              MOVE CL-PD-DELCNT2   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-CREDITLIM NOT = OCLPD-CREDITLIM
              MOVE "CL-PD-CREDITLIM" TO D01-TEXT
              MOVE OCLPD-CREDITLIM   TO D01-A15
              MOVE CL-PD-CREDITLIM   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-CRSCORE NOT = OCLPD-CRSCORE
              MOVE "CL-PD-CRSCORE" TO D01-TEXT
              MOVE OCLPD-CRSCORE   TO D01-A15
              MOVE CL-PD-CRSCORE   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-LASTSFORMNO NOT = OCLPD-LASTSFORMNO
              MOVE "CL-PD-LASTSFORMNO" TO D01-TEXT
              MOVE OCLPD-LASTSFORMNO   TO D01-A15
              MOVE CL-PD-LASTSFORMNO   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-LASTSFORMDATE NOT = OCLPD-LASTSFORMDATE
              MOVE "CL-PD-LASTSFORMDATE" TO D01-TEXT
              MOVE OCLPD-LASTSFORMDATE   TO D01-A15 
              MOVE CL-PD-LASTSFORMDATE   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-SOLCNT NOT = OCLPD-SOLCNT
              MOVE "CL-PD-SOLCNT" TO D01-TEXT
              MOVE OCLPD-SOLCNT   TO D01-A15
              MOVE CL-PD-SOLCNT   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-MESSAGE NOT = OCLPD-MESSAGE
              MOVE "CL-PD-MESSAGE" TO D01-TEXT
              MOVE OCLPD-MESSAGE   TO D01-A15
              MOVE CL-PD-MESSAGE   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-PROMDATE2 NOT = OCLPD-PROMDATE2
              MOVE "CL-PD-PROMDATE2" TO D01-TEXT
              MOVE OCLPD-PROMDATE2   TO D01-A15
              MOVE CL-PD-PROMDATE2   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-OLDACCTNO NOT = OCLPD-OLDACCTNO
              MOVE "CL-PD-OLDACCTNO" TO D01-TEXT
              MOVE OCLPD-OLDACCTNO   TO D01-A15
              MOVE CL-PD-OLDACCTNO   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-PRLNNO NOT = OCLPD-PRLNNO
              MOVE "CL-PD-PRLNNO" TO D01-TEXT
              MOVE OCLPD-PRLNNO   TO D01-A15
              MOVE CL-PD-PRLNNO   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-PRBAL NOT = OCLPD-PRBAL
              MOVE "CL-PD-PRBAL" TO D01-TEXT
              MOVE OCLPD-PRBAL   TO D01-A15-NUM 
              MOVE CL-PD-PRBAL   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-PRINTDUE NOT = OCLPD-PRINTDUE
              MOVE "CL-PD-PRINTDUE" TO D01-TEXT
              MOVE OCLPD-PRINTDUE   TO D01-A15-NUM 
              MOVE CL-PD-PRINTDUE   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-PRFINCHGRB NOT = OCLPD-PRFINCHGRB
              MOVE "CL-PD-PRFINCHGRB" TO D01-TEXT
              MOVE OCLPD-PRFINCHGRB   TO D01-A15-NUM 
              MOVE CL-PD-PRFINCHGRB   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-PRINSRB NOT = OCLPD-PRINSRB
              MOVE "CL-PD-PRINSRB" TO D01-TEXT
              MOVE OCLPD-PRINSRB   TO D01-A15-NUM 
              MOVE CL-PD-PRINSRB   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-PRPOCD NOT = OCLPD-PRPOCD
              MOVE "CL-PD-PRPOCD" TO D01-TEXT
              MOVE OCLPD-PRPOCD   TO D01-A15
              MOVE CL-PD-PRPOCD   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-PRPODATE NOT = OCLPD-PRPODATE
              MOVE "CL-PD-PRPODATE" TO D01-TEXT
              MOVE OCLPD-PRPODATE   TO D01-A15 
              MOVE CL-PD-PRPODATE   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-PRCONPROFILE NOT = OCLPD-PRCONPROFILE
              MOVE "CL-PD-PRCONPROFILE" TO D01-TEXT
              MOVE OCLPD-PRCONPROFILE   TO D01-A15 
              MOVE CL-PD-PRCONPROFILE   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-PRDELINQAMT NOT = OCLPD-PRDELINQAMT
              MOVE "CL-PD-PRDELINQAMT" TO D01-TEXT
              MOVE OCLPD-PRDELINQAMT   TO D01-A15-NUM 
              MOVE CL-PD-PRDELINQAMT   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-SUM-ORIGPURAMT NOT = OCLPD-SUM-ORIGPURAMT
              MOVE "CL-PD-SUM-ORIGPURAMT" TO D01-TEXT
              MOVE OCLPD-SUM-ORIGPURAMT   TO D01-A15-NUM 
              MOVE CL-PD-SUM-ORIGPURAMT   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-SUM-LNAMT NOT = OCLPD-SUM-LNAMT
              MOVE "CL-PD-SUM-LNAMT" TO D01-TEXT
              MOVE OCLPD-SUM-LNAMT   TO D01-A15-NUM 
              MOVE CL-PD-SUM-LNAMT   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.


           IF CL-PD-SUM-FINCHG NOT = OCLPD-SUM-FINCHG
              MOVE "CL-PD-SUM-FINCHG" TO D01-TEXT
              MOVE OCLPD-SUM-FINCHG   TO D01-A15-NUM 
              MOVE CL-PD-SUM-FINCHG   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-SUM-INTCHG NOT = OCLPD-SUM-INTCHG
              MOVE "CL-PD-SUM-INTCHG" TO D01-TEXT
              MOVE OCLPD-SUM-INTCHG   TO D01-A15-NUM 
              MOVE CL-PD-SUM-INTCHG   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-SUM-ORGTERM NOT = OCLPD-SUM-ORGTERM
              MOVE "CL-PD-SUM-ORGTERM" TO D01-TEXT
              MOVE OCLPD-SUM-ORGTERM   TO D01-A15
              MOVE CL-PD-SUM-ORGTERM   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-SUM-REGPYAMT NOT = OCLPD-SUM-REGPYAMT
              MOVE "CL-PD-SUM-REGPYAMT" TO D01-TEXT
              MOVE OCLPD-SUM-REGPYAMT   TO D01-A15-NUM 
              MOVE CL-PD-SUM-REGPYAMT   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-SUM-EXTCHG NOT = OCLPD-SUM-EXTCHG
              MOVE "CL-PD-SUM-EXTCHG" TO D01-TEXT
              MOVE OCLPD-SUM-EXTCHG   TO D01-A15-NUM 
              MOVE CL-PD-SUM-EXTCHG   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-SUM-SERCHG NOT = OCLPD-SUM-SERCHG
              MOVE "CL-PD-SUM-SERCHG" TO D01-TEXT
              MOVE OCLPD-SUM-SERCHG   TO D01-A15-NUM 
              MOVE CL-PD-SUM-SERCHG   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-SUM-MAINTFEE NOT = OCLPD-SUM-MAINTFEE
              MOVE "CL-PD-SUM-MAINTFEE" TO D01-TEXT
              MOVE OCLPD-SUM-MAINTFEE   TO D01-A15-NUM 
              MOVE CL-PD-SUM-MAINTFEE   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-UNITPER-CD NOT = OCLPD-UNITPER-CD
              MOVE "CL-PD-UNITPER-CD" TO D01-TEXT
              MOVE OCLPD-UNITPER-CD   TO D01-A15
              MOVE CL-PD-UNITPER-CD   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-UNITPER-FREQ NOT = OCLPD-UNITPER-FREQ
              MOVE "CL-PD-UNITPER-FREQ" TO D01-TEXT
              MOVE OCLPD-UNITPER-FREQ   TO D01-A15
              MOVE CL-PD-UNITPER-FREQ   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-DAYS-IN-UNITPER NOT = OCLPD-DAYS-IN-UNITPER
              MOVE "CL-PD-DAYS-IN-UNITPER" TO D01-TEXT
              MOVE OCLPD-DAYS-IN-UNITPER   TO D01-A15
              MOVE CL-PD-DAYS-IN-UNITPER   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-UNITPER-PER-YEAR NOT = OCLPD-UNITPER-PER-YEAR
              MOVE "CL-PD-UNITPER-PER-YEAR" TO D01-TEXT
              MOVE OCLPD-UNITPER-PER-YEAR   TO D01-A15-NUM 
              MOVE CL-PD-UNITPER-PER-YEAR   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-REPOCD NOT = OCLPD-REPOCD
              MOVE "CL-PD-REPOCD" TO D01-TEXT
              MOVE OCLPD-REPOCD   TO D01-A15
              MOVE CL-PD-REPOCD   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-BR-TFR-FG NOT = OCLPD-BR-TFR-FG
              MOVE "CL-PD-BR-TFR-FG" TO D01-TEXT
              MOVE OCLPD-BR-TFR-FG   TO D01-A15
              MOVE CL-PD-BR-TFR-FG   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-O3INS-DLPROC NOT = OCLPD-O3INS-DLPROC
              MOVE "CL-PD-O3INS-DLPROC" TO D01-TEXT
              MOVE OCLPD-O3INS-DLPROC   TO D01-A15-NUM 
              MOVE CL-PD-O3INS-DLPROC   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-INSPREM-DLADJ NOT = OCLPD-INSPREM-DLADJ
              MOVE "CL-PD-INSPREM-DLADJ" TO D01-TEXT
              MOVE OCLPD-INSPREM-DLADJ   TO D01-A15-NUM 
              MOVE CL-PD-INSPREM-DLADJ   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-PRLNAMT NOT = OCLPD-PRLNAMT
              MOVE "CL-PD-PRLNAMT" TO D01-TEXT
              MOVE OCLPD-PRLNAMT   TO D01-A15-NUM 
              MOVE CL-PD-PRLNAMT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-PRPDUE-DAYS NOT = OCLPD-PRPDUE-DAYS
              MOVE "CL-PD-PRPDUE-DAYS" TO D01-TEXT
              MOVE OCLPD-PRPDUE-DAYS   TO D01-A15
              MOVE CL-PD-PRPDUE-DAYS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-PLAMT NOT = OCLPD-PLAMT
              MOVE "CL-PD-PLAMT" TO D01-TEXT
              MOVE OCLPD-PLAMT   TO D01-A15-NUM 
              MOVE CL-PD-PLAMT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-MTD-PRIN NOT = OCLPD-MTD-PRIN
              MOVE "CL-PD-MTD-PRIN" TO D01-TEXT
              MOVE OCLPD-MTD-PRIN   TO D01-A15-NUM 
              MOVE CL-PD-MTD-PRIN   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-MTD-LC NOT = OCLPD-MTD-LC
              MOVE "CL-PD-MTD-LC" TO D01-TEXT
              MOVE OCLPD-MTD-LC   TO D01-A15-NUM 
              MOVE CL-PD-MTD-LC   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-MTD-INT NOT = OCLPD-MTD-INT
              MOVE "CL-PD-MTD-INT" TO D01-TEXT
              MOVE OCLPD-MTD-INT   TO D01-A15-NUM 
              MOVE CL-PD-MTD-INT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-MTD-OT NOT = OCLPD-MTD-OT
              MOVE "CL-PD-MTD-OT" TO D01-TEXT
              MOVE OCLPD-MTD-OT   TO D01-A15-NUM 
              MOVE CL-PD-MTD-OT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-MTD-EX NOT = OCLPD-MTD-EX
              MOVE "CL-PD-MTD-EX" TO D01-TEXT
              MOVE OCLPD-MTD-EX   TO D01-A15-NUM 
              MOVE CL-PD-MTD-EX   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-BNKRPT-CHAPTER NOT = OCLPD-BNKRPT-CHAPTER
              MOVE "CL-PD-BNKRPT-CHAPTER" TO D01-TEXT
              MOVE OCLPD-BNKRPT-CHAPTER   TO D01-A15
              MOVE CL-PD-BNKRPT-CHAPTER   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-BMO-CON-STATUS NOT = OCLPD-BMO-CON-STATUS
              MOVE "CL-PD-BMO-CON-STATUS" TO D01-TEXT
              MOVE OCLPD-BMO-CON-STATUS   TO D01-A15
              MOVE CL-PD-BMO-CON-STATUS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-BMO-REC-STATUS NOT = OCLPD-BMO-REC-STATUS
              MOVE "CL-PD-BMO-REC-STATUS" TO D01-TEXT
              MOVE OCLPD-BMO-REC-STATUS   TO D01-A15
              MOVE CL-PD-BMO-REC-STATUS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-GIFTNET NOT = OCLPD-GIFTNET
              MOVE "CL-PD-GIFTNET" TO D01-TEXT
              MOVE OCLPD-GIFTNET   TO D01-A15
              MOVE CL-PD-GIFTNET   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OCLPD-GIFTNET-OUR-PO NOT NUMERIC OR
               (OCLPD-GIFTNET-OUR-PO = 202020)
              MOVE 0 TO OCLPD-GIFTNET-OUR-PO.
           IF CL-PD-GIFTNET-OUR-PO NOT = OCLPD-GIFTNET-OUR-PO
              MOVE "CL-PD-GIFTNET-OUR-PO" TO D01-TEXT
              MOVE OCLPD-GIFTNET-OUR-PO   TO D01-A15
              MOVE CL-PD-GIFTNET-OUR-PO   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-PROC-DISBURSED NOT = OCLPD-PROC-DISBURSED
              MOVE "CL-PD-PROC-DISBURSED" TO D01-TEXT
              MOVE OCLPD-PROC-DISBURSED   TO D01-A15
              MOVE CL-PD-PROC-DISBURSED   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-NOODDPY NOT = OCLPD-NOODDPY
              MOVE "CL-PD-NOODDPY" TO D01-TEXT
              MOVE OCLPD-NOODDPY   TO D01-A15
              MOVE CL-PD-NOODDPY   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 5
              IF CL-PD-CONPROFILE(SUB1) NOT = OCLPD-CONPROFILE(SUB1)
                 STRING "CL-PD-CONPROFILE(",SUB1,")" INTO D01-TEXT
                 MOVE OCLPD-CONPROFILE(SUB1)  TO D01-A15 
                 MOVE CL-PD-CONPROFILE(SUB1)  TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CL-PD-RECPROFILE(SUB1) NOT = OCLPD-RECPROFILE(SUB1)
                 STRING "CL-PD-RECPROFILE(",SUB1,")" INTO D01-TEXT
                 MOVE OCLPD-RECPROFILE(SUB1)  TO D01-A15 
                 MOVE CL-PD-RECPROFILE(SUB1)  TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF 
           END-PERFORM.

           IF CL-PD-TRW-STATUS NOT = OCLPD-TRW-STATUS
              MOVE "CL-PD-TRW-STATUS" TO D01-TEXT
              MOVE OCLPD-TRW-STATUS   TO D01-A15
              MOVE CL-PD-TRW-STATUS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-ORIGIN NOT = OCLPD-ORIGIN
              MOVE "CL-PD-ORIGIN" TO D01-TEXT
              MOVE OCLPD-ORIGIN   TO D01-A15
              MOVE CL-PD-ORIGIN   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-FOLLOWUP-DATE NOT = OCLPD-FOLLOWUP-DATE
              MOVE "CL-PD-FOLLOWUP-DATE" TO D01-TEXT
              MOVE OCLPD-FOLLOWUP-DATE   TO D01-A15
              MOVE CL-PD-FOLLOWUP-DATE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-PO-TRCD NOT = OCLPD-PO-TRCD
              MOVE "CL-PD-PO-TRCD" TO D01-TEXT
              MOVE OCLPD-PO-TRCD   TO D01-A15
              MOVE CL-PD-PO-TRCD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-NEXIS-CRSCORE NOT = OCLPD-NEXIS-CRSCORE
              MOVE "CL-PD-NEXIS-CRSCORE" TO D01-TEXT
              MOVE OCLPD-NEXIS-CRSCORE   TO D01-A15
              MOVE CL-PD-NEXIS-CRSCORE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-TRW-B-CONSUMER-IND NOT = OCLPD-TRW-B-CONSUMER-IND
              MOVE "CL-PD-TRW-B-CONSUMER-IND" TO D01-TEXT
              MOVE OCLPD-TRW-B-CONSUMER-IND   TO D01-A15
              MOVE CL-PD-TRW-B-CONSUMER-IND   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-ACCT-FROZEN NOT = OCLPD-ACCT-FROZEN
              MOVE "CL-PD-ACCT-FROZEN" TO D01-TEXT
              MOVE OCLPD-ACCT-FROZEN   TO D01-A15-NUM 
              MOVE CL-PD-ACCT-FROZEN   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-TRW-SPECIAL-CODE NOT = OCLPD-TRW-SPECIAL-CODE
              MOVE "CL-PD-TRW-SPECIAL-CODE" TO D01-TEXT
              MOVE OCLPD-TRW-SPECIAL-CODE   TO D01-A15
              MOVE CL-PD-TRW-SPECIAL-CODE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.
 
           IF CL-PD-CREATION-STATUS NOT = OCLPD-CREATION-STATUS
              MOVE "CL-PD-CREATION-STATUS" TO D01-TEXT
              MOVE OCLPD-CREATION-STATUS   TO D01-A15
              MOVE CL-PD-CREATION-STATUS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OCLPD-TOTNOLCHG NOT NUMERIC
              MOVE 0 TO OCLPD-TOTNOLCHG.
           IF CL-PD-TOTNOLCHG NOT = OCLPD-TOTNOLCHG
              MOVE "CL-PD-TOTNOLCHG" TO D01-TEXT
              MOVE OCLPD-TOTNOLCHG   TO D01-A15-NUM 
              MOVE CL-PD-TOTNOLCHG   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-LBOX NOT = OCLPD-LBOX
              MOVE "CL-PD-LBOX" TO D01-TEXT
              MOVE OCLPD-LBOX   TO D01-A15
              MOVE CL-PD-LBOX   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-EXTRA-PRIN-LFP NOT = OCLPD-EXTRA-PRIN-LFP
              MOVE "CL-PD-EXTRA-PRIN-LFP" TO D01-TEXT
              MOVE OCLPD-EXTRA-PRIN-LFP   TO D01-A15-NUM 
              MOVE CL-PD-EXTRA-PRIN-LFP   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-REMAIN-INT-SINCE-LFP NOT = 
               OCLPD-REMAIN-INT-SINCE-LFP
              MOVE "CL-PD-REMAIN-INT-SINCE-LFP" TO D01-TEXT
              MOVE OCLPD-REMAIN-INT-SINCE-LFP   TO D01-A15-NUM 
              MOVE CL-PD-REMAIN-INT-SINCE-LFP   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-REMAIN-PRIN-SINCE-LFP NOT = 
               OCLPD-REMAIN-PRIN-SINCE-LFP
              MOVE "CL-PD-REMAIN-PRIN-SINCE-LFP" TO D01-TEXT
              MOVE OCLPD-REMAIN-PRIN-SINCE-LFP   TO D01-A15-NUM 
              MOVE CL-PD-REMAIN-PRIN-SINCE-LFP   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-INTERIM-COUPON NOT = OCLPD-INTERIM-COUPON
              MOVE "CL-PD-INTERIM-COUPON" TO D01-TEXT
              MOVE OCLPD-INTERIM-COUPON   TO D01-A15
              MOVE CL-PD-INTERIM-COUPON   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-DLCOMM-O3-CMSI NOT = OCLPD-DLCOMM-O3-CMSI
              MOVE "CL-PD-DLCOMM-O3-CMSI" TO D01-TEXT
              MOVE OCLPD-DLCOMM-O3-CMSI   TO D01-A15-NUM 
              MOVE CL-PD-DLCOMM-O3-CMSI   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-DLCOMM-PP-CMSI NOT = OCLPD-DLCOMM-PP-CMSI
              MOVE "CL-PD-DLCOMM-PP-CMSI" TO D01-TEXT
              MOVE OCLPD-DLCOMM-PP-CMSI   TO D01-A15-NUM 
              MOVE CL-PD-DLCOMM-PP-CMSI   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-TRW-J-CONSUMER-IND NOT = OCLPD-TRW-J-CONSUMER-IND
              MOVE "CL-PD-TRW-J-CONSUMER-IND" TO D01-TEXT
              MOVE OCLPD-TRW-J-CONSUMER-IND   TO D01-A15
              MOVE CL-PD-TRW-J-CONSUMER-IND   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-ACH NOT = OCLPD-ACH
              MOVE "CL-PD-ACH" TO D01-TEXT
              MOVE OCLPD-ACH   TO D01-A15
              MOVE CL-PD-ACH   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.


           IF CL-PD-MSG1 NOT = OCLPD-MSG1
              MOVE "CL-PD-MSG1" TO D01-TEXT
              MOVE OCLPD-MSG1   TO D01-A15
              MOVE CL-PD-MSG1   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-MSG2 NOT = OCLPD-MSG2
              MOVE "CL-PD-MSG2" TO D01-TEXT
              MOVE OCLPD-MSG2   TO D01-A15
              MOVE CL-PD-MSG2   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-DLPAID NOT = OCLPD-DLPAID
              MOVE "CL-PD-DLPAID" TO D01-TEXT
              MOVE OCLPD-DLPAID   TO D01-A15
              MOVE CL-PD-DLPAID   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OCLPD-CKNO NOT NUMERIC
              MOVE 0  TO OCLPD-CKNO.
           IF CL-PD-CKNO NOT = OCLPD-CKNO
              MOVE "CL-PD-CKNO" TO D01-TEXT
              MOVE OCLPD-CKNO   TO D01-A15
              MOVE CL-PD-CKNO   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-FINAMT NOT = OCLPD-FINAMT
              MOVE "CL-PD-FINAMT" TO D01-TEXT
              MOVE OCLPD-FINAMT   TO D01-A15-NUM 
              MOVE CL-PD-FINAMT   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-PROCEEDS-FG NOT = OCLPD-PROCEEDS-FG
              MOVE "CL-PD-PROCEEDS-FG" TO D01-TEXT
              MOVE OCLPD-PROCEEDS-FG   TO D01-A15
              MOVE CL-PD-PROCEEDS-FG   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-POINTS-FG NOT = OCLPD-POINTS-FG
              MOVE "CL-PD-POINTS-FG" TO D01-TEXT
              MOVE OCLPD-POINTS-FG   TO D01-A15
              MOVE CL-PD-POINTS-FG   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-OVRDRATE-FG NOT = OCLPD-OVRDRATE-FG
              MOVE "CL-PD-OVRDRATE-FG" TO D01-TEXT
              MOVE OCLPD-OVRDRATE-FG   TO D01-A15
              MOVE CL-PD-OVRDRATE-FG   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB2 FROM 1 BY 1 UNTIL SUB2 > 10
              IF CL-PD-INS-FG(SUB2) NOT = OCLPD-INS-FG(SUB2)
                 STRING "CL-PD-INS-FG(",SUB2,")" INTO D01-TEXT
                 MOVE OCLPD-INS-FG(SUB2)  TO D01-A15 
                 MOVE CL-PD-INS-FG(SUB2)  TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.

           IF CL-PD-SERCHG-FG NOT = OCLPD-SERCHG-FG
              MOVE "CL-PD-SERCHG-FG" TO D01-TEXT
              MOVE OCLPD-SERCHG-FG   TO D01-A15
              MOVE CL-PD-SERCHG-FG   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-MAINTFEE-FG NOT = OCLPD-MAINTFEE-FG
              MOVE "CL-PD-MAINTFEE-FG" TO D01-TEXT
              MOVE OCLPD-MAINTFEE-FG   TO D01-A15
              MOVE CL-PD-MAINTFEE-FG   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-EXTCHG-FG NOT = OCLPD-EXTCHG-FG
              MOVE "CL-PD-EXTCHG-FG" TO D01-TEXT
              MOVE OCLPD-EXTCHG-FG   TO D01-A15
              MOVE CL-PD-EXTCHG-FG   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-REGPYAMT-FG NOT = OCLPD-REGPYAMT-FG
              MOVE "CL-PD-REGPYAMT-FG" TO D01-TEXT
              MOVE OCLPD-REGPYAMT-FG   TO D01-A15
              MOVE CL-PD-REGPYAMT-FG   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB2 FROM 1 BY 1 UNTIL SUB2 > 10
              IF CL-PD-FEECD(SUB2) NOT = OCLPD-FEECD(SUB2)
                 STRING "CL-PD-FEECD(",SUB2,")" INTO D01-TEXT
                 MOVE OCLPD-FEECD(SUB2)  TO D01-A15 
                 MOVE CL-PD-FEECD(SUB2)  TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.

           MOVE OCLPD-PRPAID-AHEAD TO WS-NUM.
           IF WS-NUM = 2020202.02 OR 5389762.880
              MOVE 0 TO OCLPD-PRPAID-AHEAD.
           IF CL-PD-PRPAID-AHEAD NOT = OCLPD-PRPAID-AHEAD
              MOVE "CL-PD-PRPAID-AHEAD" TO D01-TEXT
              MOVE OCLPD-PRPAID-AHEAD   TO D01-A15-NUM 
              MOVE CL-PD-PRPAID-AHEAD   TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OCLPD-RESTRICT-ADJ = 5389762.880
              MOVE 0 TO OCLPD-RESTRICT-ADJ.
           IF CL-PD-RESTRICT-ADJ NOT = OCLPD-RESTRICT-ADJ
              MOVE "CL-PD-RESTRICT-ADJ" TO D01-TEXT
              MOVE OCLPD-RESTRICT-ADJ   TO D01-A15-NUM 
              MOVE CL-PD-RESTRICT-ADJ   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OCLPD-DUAL-LOAN-ADJ = 5389762.880
              MOVE 0 TO OCLPD-DUAL-LOAN-ADJ.
           IF CL-PD-DUAL-LOAN-ADJ NOT = OCLPD-DUAL-LOAN-ADJ
              MOVE "CL-PD-DUAL-LOAN-ADJ" TO D01-TEXT
              MOVE OCLPD-DUAL-LOAN-ADJ   TO D01-A15-NUM 
              MOVE CL-PD-DUAL-LOAN-ADJ   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-CAN-LOAN-MAX NOT = OCLPD-CAN-LOAN-MAX
              MOVE "CL-PD-CAN-LOAN-MAX" TO D01-TEXT
              MOVE OCLPD-CAN-LOAN-MAX   TO D01-A15-NUM 
              MOVE CL-PD-CAN-LOAN-MAX   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-PERDOWN NOT = OCLPD-PERDOWN
              MOVE "CL-PD-PERDOWN" TO D01-TEXT
              MOVE OCLPD-PERDOWN   TO D01-A15-NUM 
              MOVE CL-PD-PERDOWN   TO D01-S35-NUM 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-SELLPR-FG NOT = OCLPD-SELLPR-FG
              MOVE "CL-PD-SELLPR-FG" TO D01-TEXT
              MOVE OCLPD-SELLPR-FG   TO D01-A15
              MOVE CL-PD-SELLPR-FG   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-DOWNPY-FG NOT = OCLPD-DOWNPY-FG
              MOVE "CL-PD-DOWNPY-FG" TO D01-TEXT
              MOVE OCLPD-DOWNPY-FG   TO D01-A15
              MOVE CL-PD-DOWNPY-FG   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-PERDOWN-FG NOT = OCLPD-PERDOWN-FG
              MOVE "CL-PD-PERDOWN-FG" TO D01-TEXT
              MOVE OCLPD-PERDOWN-FG   TO D01-A15
              MOVE CL-PD-PERDOWN-FG   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-SELL-DOWN-FG NOT = OCLPD-SELL-DOWN-FG
              MOVE "CL-PD-SELL-DOWN-FG" TO D01-TEXT
              MOVE OCLPD-SELL-DOWN-FG   TO D01-A15
              MOVE CL-PD-SELL-DOWN-FG   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-BALLOON-FG NOT = OCLPD-BALLOON-FG
              MOVE "CL-PD-BALLOON-FG" TO D01-TEXT
              MOVE OCLPD-BALLOON-FG   TO D01-A15
              MOVE CL-PD-BALLOON-FG   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-BALLOON-LAST-FG NOT = OCLPD-BALLOON-LAST-FG
              MOVE "CL-PD-BALLOON-LAST-FG" TO D01-TEXT
              MOVE OCLPD-BALLOON-LAST-FG   TO D01-A15
              MOVE CL-PD-BALLOON-LAST-FG   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 2
              IF CL-PD-FEECD-FG(SUB1) NOT = OCLPD-FEECD-FG(SUB1)
                 STRING "CL-PD-FEECD-FG(",SUB1,")" INTO D01-TEXT
                 MOVE OCLPD-FEECD-FG(SUB1)  TO D01-A15 
                 MOVE CL-PD-FEECD-FG(SUB1)  TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.

           PERFORM VARYING SUB2 FROM 1 BY 1 UNTIL SUB2 > 10
              IF CL-PD-INS-TERM-SW(SUB2) NOT = OCLPD-INS-TERM-SW(SUB2)
                 STRING "CL-PD-INS-TERM-SW(",SUB2,")" INTO D01-TEXT
                 MOVE OCLPD-INS-TERM-SW(SUB2)  TO D01-A15 
                 MOVE CL-PD-INS-TERM-SW(SUB2)  TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CL-PD-COVERAGE-SW(SUB2) NOT = OCLPD-COVERAGE-SW(SUB2)
                 STRING "CL-PD-COVERAGE-SW(",SUB2,")" INTO D01-TEXT
                 MOVE OCLPD-COVERAGE-SW(SUB2)  TO D01-A15 
                 MOVE CL-PD-COVERAGE-SW(SUB2)  TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CL-PD-PREMIUM-SW(SUB2) NOT = OCLPD-PREMIUM-SW(SUB2)
                 STRING "CL-PD-PREMIUM-SW(",SUB2,")" INTO D01-TEXT
                 MOVE OCLPD-PREMIUM-SW(SUB2)  TO D01-A15 
                 MOVE CL-PD-PREMIUM-SW(SUB2)  TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
           END-PERFORM.

           IF CL-PD-DLDISC-FG NOT = OCLPD-DLDISC-FG
              MOVE "CL-PD-DLDISC-FG" TO D01-TEXT
              MOVE OCLPD-DLDISC-FG   TO D01-A15
              MOVE CL-PD-DLDISC-FG   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 3
              IF CL-PD-DLRVPRIN-FG(SUB1) NOT = OCLPD-DLRVPRIN-FG(SUB1)
                 STRING "CL-PD-DLRVPRIN-FG(",SUB1,")" INTO D01-TEXT
                 MOVE OCLPD-DLRVPRIN-FG(SUB1)  TO D01-A15 
                 MOVE CL-PD-DLRVPRIN-FG(SUB1)  TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF
              IF CL-PD-DLRVCHGS-FG(SUB1) NOT = OCLPD-DLRVCHGS-FG(SUB1)
                 STRING "CL-PD-DLRVCHGS-FG(",SUB1,")" INTO D01-TEXT
                 MOVE OCLPD-DLRVCHGS-FG(SUB1)  TO D01-A15 
                 MOVE CL-PD-DLRVCHGS-FG(SUB1)  TO D01-S35 
                 PERFORM WRITE-DETAIL-LINE-ERROR-KEY
                 PERFORM WRITE-DETAIL-LINE-ERROR
              END-IF 
           END-PERFORM.

           IF CL-PD-DLPARVHELD-FG NOT = OCLPD-DLPARVHELD-FG
              MOVE "CL-PD-DLPARVHELD-FG" TO D01-TEXT
              MOVE OCLPD-DLPARVHELD-FG   TO D01-A15
              MOVE CL-PD-DLPARVHELD-FG   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-DLPARVPAID-FG NOT = OCLPD-DLPARVPAID-FG
              MOVE "CL-PD-DLPARVPAID-FG" TO D01-TEXT
              MOVE OCLPD-DLPARVPAID-FG   TO D01-A15
              MOVE CL-PD-DLPARVPAID-FG   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-ONE-MTH-NO-DAY-EXT-SW NOT = 
               OCLPD-ONE-MTH-NO-DAY-EXT-SW
              MOVE "CL-PD-ONE-MTH-NO-DAY-EXT-SW" TO D01-TEXT
              MOVE OCLPD-ONE-MTH-NO-DAY-EXT-SW   TO D01-A15
              MOVE CL-PD-ONE-MTH-NO-DAY-EXT-SW   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-ASSIGN-RN-ACCTNO NOT = OCLPD-ASSIGN-RN-ACCTNO
              MOVE "CL-PD-ASSIGN-RN-ACCTNO"  TO D01-TEXT
              MOVE OCLPD-ASSIGN-RN-ACCTNO    TO D01-A15
              MOVE CL-PD-ASSIGN-RN-ACCTNO    TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-APR-FINAMT NOT = OCLPD-APR-FINAMT
              MOVE "CL-PD-APR-FINAMT"  TO D01-TEXT
              MOVE OCLPD-APR-FINAMT    TO D01-A15-NUM 
              MOVE CL-PD-APR-FINAMT    TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-DLCOMM-PP-CMSI-FG NOT = OCLPD-DLCOMM-PP-CMSI-FG
              MOVE "CL-PD-DLCOMM-PP-CMSI-FG" TO D01-TEXT
              MOVE OCLPD-DLCOMM-PP-CMSI-FG   TO D01-A15
              MOVE CL-PD-DLCOMM-PP-CMSI-FG   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-DLCOMM-O3-CMSI-FG NOT = OCLPD-DLCOMM-O3-CMSI-FG
              MOVE "CL-PD-DLCOMM-O3-CMSI-FG" TO D01-TEXT
              MOVE OCLPD-DLCOMM-O3-CMSI-FG   TO D01-A15
              MOVE CL-PD-DLCOMM-O3-CMSI-FG   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-POSTING-BR NOT = OCLPD-POSTING-BR
              MOVE "CL-PD-POSTING-BR" TO D01-TEXT
              MOVE OCLPD-POSTING-BR   TO D01-A15
              MOVE CL-PD-POSTING-BR   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-OTHBR-FG NOT = OCLPD-OTHBR-FG
              MOVE "CL-PD-OTHBR-FG" TO D01-TEXT
              MOVE OCLPD-OTHBR-FG   TO D01-A15
              MOVE CL-PD-OTHBR-FG   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-APPROVAL-LIMIT NOT = OCLPD-APPROVAL-LIMIT
              MOVE "CL-PD-APPROVAL-LIMIT" TO D01-TEXT
              MOVE OCLPD-APPROVAL-LIMIT   TO D01-A15
              MOVE CL-PD-APPROVAL-LIMIT   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-APPROVAL-USERID NOT = OCLPD-APPROVAL-USERID
              MOVE "CL-PD-APPROVAL-USERID" TO D01-TEXT
              MOVE OCLPD-APPROVAL-USERID   TO D01-A15
              MOVE CL-PD-APPROVAL-USERID   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-MIN-CRED-SCORE NOT = OCLPD-MIN-CRED-SCORE
              MOVE "CL-PD-MIN-CRED-SCORE" TO D01-TEXT
              MOVE OCLPD-MIN-CRED-SCORE   TO D01-A15
              MOVE CL-PD-MIN-CRED-SCORE   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF CL-PD-CRED-SCORE-USERID NOT = OCLPD-CRED-SCORE-USERID
              MOVE "CL-PD-CRED-SCORE-USERID" TO D01-TEXT
              MOVE OCLPD-CRED-SCORE-USERID   TO D01-A15
              MOVE CL-PD-CRED-SCORE-USERID   TO D01-S35 
              PERFORM WRITE-DETAIL-LINE-ERROR-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR.

        EXIT-COMPARE-FIELDS.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE-ERROR-KEY SECTION.
      ******************************************************************

           MOVE CL-GROUP-ID TO D01-GROUP-ID.
           MOVE CL-CAN-NO   TO D01-CAN-NO.

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

           MOVE "BEGINNING GROUP ID            : " TO R01-PROMPT.
           MOVE BEG-GROUPID                        TO R01-NUMERIC.
           MOVE 01                                 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING GROUP ID               : " TO R01-PROMPT.
           MOVE END-GROUPID                        TO R01-NUMERIC.
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
           MOVE EXT-CONAME-CONAME TO CLFILE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CLFILE-SYSDATE.
           DISPLAY CLFILE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE CLFILE-FORM-FIELDS TO WR-BUF.
           MOVE CLFILE-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE CLFILE-HELP--FILE TO HELP-LINK-FILE.
           MOVE CLFILE-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/CLFILE_EVA.CPY".
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
       COPY "LIBLP/LPCLGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPCL1RN.CPY".

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

      *----------------------------------------------------------------
       OPEN-OCL1-FILE.
           PERFORM OPEN-IT.
           MOVE OCL-PATH TO E-FILE.
           OPEN INPUT OCL-FILE.
           IF IO-FG = 8
              GO TO OPEN-OCL1-FILE.
           IF IO-FG = 7
              CLOSE OCL-FILE
              GO TO OPEN-OCL1-FILE.
        READ-OCL1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OCL-PATH TO E-FILE.
           READ OCL-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OCL1-FILE-NEXT.
       CLOSE-OCL1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OCL-FILE.

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
       REWRITE-WK1-FILE.
           PERFORM REWRITE-IT.
           MOVE WK-PATH TO E-FILE.
           MOVE WK-KEY  TO E-KEYX.
           REWRITE WK-REC.
           IF IO-FG = 8
             GO TO REWRITE-WK1-FILE.
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
              DESTROY CLFILE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
