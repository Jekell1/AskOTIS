       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SPFILE.
       DATE-WRITTEN.  02-12-2024.
       AUTHOR. RON ZAMISKA.
      ******************************************************************
      *
      * DESCRIPTION   : COMPARE S35 (SPA/SPB/SPB)FILE WITH A15 SPUFILE
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

      * COPYMEMBER: LIBLP/LPFSSP
           SELECT OSP-FILE ASSIGN TO OSP-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OSP1-KEY
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
       FD  OSP-FILE
           LABEL RECORDS ARE STANDARD.
      *-----------------------------------------------------------------
       COPY "LIBUP/A15SRC/LP01SP.CPY" REPLACING LEADING "SP" BY "OSP".

       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
              05  WK-ORGST               PIC XX.
              05  WK-SPRCLASS            PIC 999.
              05  WK-SUBCLASS            PIC 99.
              05  WK-LAWCODE             PIC 99.
           03  WK-DATA                   PIC X(OSP-SIZE).

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/SPFILE S35 03/28/24-12:04:36 >".

      *COPY "LIBGB/GB01BR.CPY".
      *COPY "LIBGB/GB01BR_SQL.CPY".
      *COPY "LIBGB/GBWSBR.CPY".
      *COPY "LIBGB/GB01GR.CPY".
      *COPY "LIBGB/GB01GR_SQL.CPY".
      *COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".
   

      *********************************************************
      *        LIBLP/LPWSSP: SPFILE PATH WORKERS
      *********************************************************
   
       01  OSP-PATH                    PIC X(35) VALUE SPACES.
       01  OSP-ALL-PATH.
           03  OSP-ALL-BASE     PIC X(9).
           03  FILLER           PIC X     VALUE "/".
           03  OSP-ALL-MACHINE  PIC XX.
           03  FILLER           PIC X(10) VALUE
               "/LP/SPFILE".

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

       01  SQL-ROW-KEY.
           03  SQL-ROW-NAME           PIC X(07).    
           03  SQL-ROW-DATE           PIC X(08).
           03  SQL-ROW-SEQ            PIC 9(08).

       01  PROC-YYMM                   PIC 9(4).
       01  PROC-YYMM-RDF  REDEFINES PROC-YYMM.
           03 PROC-YYMM-YY             PIC 99.
           03 PROC-YYMM-MM             PIC 99.

       01  WK-EOF-SW                   PIC X      VALUE "N".
       01  SP-EOF-SW                   PIC X      VALUE "N".
       01  OSP-EOF-SW                  PIC X      VALUE "N".
       01  TOTAL-ERRORS-CTR            PIC 9(9)   VALUE 0.
       01  OSP-DUPS-CTR                PIC 9(9)   VALUE 0.
       01  FILE-FOUND-SW               PIC X      VALUE "N".
       01  WS-NUM33                    PIC 9(3)V999  VALUE 0.

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
       01  SUB1                        PIC 9(04)     VALUE ZEROES.

       01  SYSDATE-YYYYMMDD            PIC 9(8)     VALUE ZEROES.

       01  TEST-DELQ-IB-AGE            PIC 999     VALUE ZEROS.
       01  DELQ-IB-MAX-DAYS-REM        PIC 999V99  VALUE ZEROS.
       01  TEST-DIVIDE-BY-30           PIC 999     VALUE ZEROS.
       01  TEST-DIVIDE-BY-30-REM       PIC 999V99  VALUE ZEROS.

       01  LEGEND                      PIC X(79)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(07)     VALUE "LEGEND.".

       01  LEGEND-BEG-ORGST.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F6-SCAN, F7-".
           03  FILLER PIC X(30) VALUE "EXIT, ENTER BEGINNING SP-ORGST".
           03  FILLER PIC X(19) VALUE "                   ".

       01  LEGEND-END-ORGST.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER ENDIN".
           03  FILLER PIC X(30) VALUE "G SP-ORGST                    ".
           03  FILLER PIC X(19) VALUE "                   ".

       01  LEGEND-BEG-SPRCLASS.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F6-SCAN, F7-".
           03  FILLER PIC X(30) VALUE "EXIT, ENTER BEGINNING SP-SPRCL".
           03  FILLER PIC X(19) VALUE "ASS                ".

       01  LEGEND-END-SPRCLASS.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER ENDIN".
           03  FILLER PIC X(30) VALUE "G SP-SPRCLASS                 ".
           03  FILLER PIC X(19) VALUE "                   ".

       01  LEGEND-BEG-SUBCLASS.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F6-SCAN, F7-".
           03  FILLER PIC X(30) VALUE "EXIT, ENTER BEGINNING SP-SUBCL".
           03  FILLER PIC X(19) VALUE "ASS                ".

       01  LEGEND-END-SUBCLASS.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER ENDIN".
           03  FILLER PIC X(30) VALUE "G SP-SUBCLASS                 ".
           03  FILLER PIC X(19) VALUE "                   ".

       01  LEGEND-BEG-LAWCODE.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F6-SCAN, F7-".
           03  FILLER PIC X(30) VALUE "EXIT, ENTER BEGINNING SP-LAWCO".
           03  FILLER PIC X(19) VALUE "DE                 ".

       01  LEGEND-END-LAWCODE.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER ENDIN".
           03  FILLER PIC X(30) VALUE "G SP-LAWCODE                  ".
           03  FILLER PIC X(19) VALUE "                   ".

      *-----------------------------------------------------------------
       01  LEGEND-ACCEPT-YN.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT             ".
           03  FILLER PIC X(30) VALUE "                              ".
           03  FILLER PIC X(19) VALUE "     ACCEPT (Y/N): ".

       01  SOURCE-PATH-LIST            PIC X(07)     VALUE "S-PATH.".

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  SOURCE-PATH             PIC X(09).

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
           "STATE PARAMETER (SPFILE) COMPARE A15 TO S35".

       01  HEADER-LINE-03.
           03  FILLER                PIC X(09)  VALUE "SP-KEY        ".
           03  FILLER                PIC X(02)  VALUE SPACES.
           03  FILLER                PIC X(11)  VALUE "DESCRIPTION".
           03  FILLER                PIC X(31)  VALUE SPACES.
           03  FILLER                PIC X(27)  VALUE "A15".
           03  FILLER                PIC X(03)  VALUE "S35".

      ******************************************************************
      *
      *    D E T A I L - L I N E - W O R K E R S
      *
      ******************************************************************
       01  DETAIL-LINE                        PIC X(120)   VALUE SPACES.

       01  DETAIL-LINE-01                     REDEFINES DETAIL-LINE.
           03  D01-KEY                        PIC X(09).
           03  FILLER                         PIC X(2).
           03  D01-TEXT                       PIC X(40).
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
       COPY "LIBUT/SPFILE_DEF.CPY".
       COPY "LIBUT/SPFILE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/SPFILE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE OSP-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE OSP-FILE WK-FILE.
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

           PERFORM PROCESS-SP-DRIVER.

           PERFORM RANGE-LINES.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

           MOVE STAT---GOOD                TO PROCESS-STAT.

           MOVE FORM-PATHNAME              TO FORM-PATH.
           MOVE "SPFILE" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE SPFILE-QUEUE-POS  TO Q-POS.
           MOVE SPFILE-COPIES-POS TO C-POS.
           MOVE SPFILE-PRU-POS    TO P-POS.
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
       
           MOVE VDUBUF TO SOURCE-PATH.

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

           MOVE "S  A090S-PATH."           TO VDU.
           MOVE SOURCE-PATH                TO VDUBUF.
           PERFORM SCREENIO.

       DISPLAY-PARAMETERS-EXIT.
           EXIT.

      *******************************************************
       PROCESS-SP-DRIVER SECTION.
      *********************************************************
      D    STOP MESS.

           MOVE 9999 TO PROGID-SLEEP-COUNTER.
      *    PERFORM SEND-PROGRESS-INDICATOR.

      *  OPEN/READ S35 - SPFILE

      *    MOVE EXT-FILPATH-BASE  TO FILPATH-BASE.

           MOVE SPACES               TO QSP1-WBEG-ORGST.
           MOVE ALL 'Z'              TO QSP1-WEND-ORGST.
           MOVE ZEROS                TO QSP1-WBEG-SPRCLASS.
           MOVE 999                  TO QSP1-WEND-SPRCLASS.
           MOVE ZEROS                TO QSP1-WBEG-SUBCLASS.
           MOVE 99                   TO QSP1-WEND-SUBCLASS.
           MOVE ZEROS                TO QSP1-WBEG-LAWCODE.
           MOVE 99                   TO QSP1-WEND-LAWCODE.

           MOVE "PROCESSING S35 DATA" TO BTRACK-MESS. 
           PERFORM BTRACK-GLOBAL.
 
           PERFORM OPEN-SP1-FILE.
           PERFORM START-SP1-FILE.
           PERFORM PROCESS-READ-SP1.

           MOVE "PROCESSING A15 DATA" TO BTRACK-MESS.
           PERFORM BTRACK-GLOBAL.

      * LOAD A15 R1 AND R2 SP INTO WORK FILE FIRST TIME THRU
      *** BYPASS DUPS FROM R2        ***
           PERFORM OPEN-WK1-FILE-OUTPUT.

           MOVE SOURCE-PATH    TO OSP-ALL-BASE.
           MOVE MACHINE-R1     TO OSP-ALL-MACHINE.
           MOVE OSP-ALL-PATH   TO OSP-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE "MISSING R1 FILE" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-ERROR
              PERFORM CLOSE-WK1-FILE
              GO TO EXIT-PROCESS-READ-SP1.

           MOVE "N" TO OSP-EOF-SW.
           PERFORM OPEN-OSP1-FILE.
           PERFORM LOAD-WORK-FILE.
           PERFORM CLOSE-OSP1-FILE.

           MOVE SOURCE-PATH      TO OSP-ALL-BASE.
           MOVE MACHINE-R2       TO OSP-ALL-MACHINE.
           MOVE OSP-ALL-PATH     TO OSP-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE "MISSING R2 FILE" TO D01-TEXT
              PERFORM WRITE-DETAIL-LINE-ERROR
              PERFORM CLOSE-WK1-FILE
              GO TO EXIT-PROCESS-READ-SP1.

           MOVE "N" TO OSP-EOF-SW.
           PERFORM OPEN-OSP1-FILE.
           PERFORM LOAD-WORK-FILE.
           PERFORM CLOSE-OSP1-FILE.

           PERFORM CLOSE-WK1-FILE.
           PERFORM OPEN-WK1-FILE.
           PERFORM PROCESS-READ-WK1.

       CONTINUE-SP-DRIVER.

           IF SP-EOF-SW  = "Y" AND WK-EOF-SW = "Y"
              GO TO PROCESS-SP-DRIVER-END.

           IF WK-KEY < SP1-KEY 
              MOVE "MISSING S35 RECORD" TO D01-TEXT
              MOVE WK-KEY               TO D01-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR
              PERFORM PROCESS-READ-WK1 
              GO TO CONTINUE-SP-DRIVER.
 
           IF SP1-KEY < WK-KEY 
              MOVE "MISSING A15 RECORD" TO D01-TEXT
              MOVE SP1-KEY              TO D01-KEY
              PERFORM WRITE-DETAIL-LINE-ERROR 
              PERFORM PROCESS-READ-SP1 
              GO TO CONTINUE-SP-DRIVER.

            PERFORM COMPARE-FIELDS.

            PERFORM PROCESS-READ-SP1.
            PERFORM PROCESS-READ-WK1. 

            GO TO CONTINUE-SP-DRIVER.

       PROCESS-SP-DRIVER-END.
           EXIT. 

       LOAD-WORK-FILE SECTION.
           PERFORM PROCESS-READ-OSP1.

           IF OSP-EOF-SW = "N"
              INITIALIZE WK-REC
              MOVE OSP-ORGST    TO WK-ORGST
              MOVE OSP-SPRCLASS TO WK-SPRCLASS
              MOVE OSP-SUBCLASS TO WK-SUBCLASS
              MOVE OSP-LAWCODE  TO WK-LAWCODE
              MOVE OSP-REC      TO WK-DATA
              PERFORM WRITE-WK1-FILE
              IF IO-STATUS = 9 
                 ADD 1 TO OSP-DUPS-CTR
              END-IF
              GO TO LOAD-WORK-FILE.

       LOAD-WORK-FILE-END.
           EXIT. 

       PROCESS-READ-WK1 SECTION.
           PERFORM READ-WK1-FILE-NEXT.

           IF IO-BAD 
              MOVE "Y"     TO WK-EOF-SW
              MOVE ALL '9' TO WK-KEY
              MOVE "ZZ"    TO WK-ORGST
           ELSE
              MOVE WK-DATA TO OSP-REC.

       EXIT-PROCESS-READ-WK1.
           EXIT.


       PROCESS-READ-SP1 SECTION.
           PERFORM READ-SP1-FILE-NEXT.
           IF IO-BAD
              MOVE "ZZ"   TO SP-ORGST
              MOVE 999    TO SP-SPRCLASS
              MOVE 99     TO SP-SUBCLASS
              MOVE 99     TO SP-LAWCODE 
              MOVE "Y"    TO SP-EOF-SW
           ELSE
              ADD 1       TO SQL-RECORDS-PROCESSED.
         
       EXIT-PROCESS-READ-SP1.
           EXIT.

       PROCESS-READ-OSP1 SECTION.
           PERFORM READ-OSP1-FILE-NEXT.
           IF IO-BAD
              MOVE "Y"  TO OSP-EOF-SW
           ELSE
              ADD 1     TO A15-RECORDS-PROCESSED.

       EXIT-PROCESS-READ-OSP1.
           EXIT.

      ******************************************************************
       COMPARE-FIELDS SECTION.
      ******************************************************************

           MOVE SP1-KEY           TO D01-KEY.

           IF SP-DESC        NOT = OSP-DESC 
              MOVE "SP-DESC"    TO D01-TEXT
              MOVE OSP-DESC     TO D01-A15
              MOVE SP-DESC      TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-NOTES     NOT = OSP-NOTES 
              MOVE "SP-NOTES" TO D01-TEXT
              MOVE OSP-NOTES  TO D01-A15
              MOVE SP-NOTES   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE SP-EFFDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.

           IF DATE-MMDDYY       NOT = OSP-EFFDATE
              MOVE "SP-EFFDATE"    TO D01-TEXT
              MOVE OSP-EFFDATE     TO D01-A15
              MOVE DATE-MMDDYY     TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE SP-EXPDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.

           IF DATE-MMDDYY       NOT = OSP-EXPDATE 
              MOVE "SP-EXPDATE"  TO D01-TEXT
              MOVE OSP-EXPDATE   TO D01-A15
              MOVE DATE-MMDDYY   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-EXPD-MONTHS     NOT = OSP-EXPD-MONTHS 
              MOVE "SP-EXPD-MONTHS" TO D01-TEXT
              MOVE OSP-EXPD-MONTHS  TO D01-A15
              MOVE SP-EXPD-MONTHS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-EXPD-DAYS     NOT = OSP-EXPD-DAYS 
              MOVE "SP-EXPD-DAYS" TO D01-TEXT
              MOVE OSP-EXPD-DAYS  TO D01-A15
              MOVE SP-EXPD-DAYS   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-EXPD-FRMLA      NOT = OSP-EXPD-FRMLA 
              MOVE "SP-EXPD-FRMLA"  TO D01-TEXT
              MOVE OSP-EXPD-FRMLA   TO D01-A15
              MOVE SP-EXPD-FRMLA    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-EXPD-EXCEPT         NOT = OSP-EXPD-EXCEPT 
              MOVE "SP-EXPD-EXCEPT"     TO D01-TEXT
              MOVE OSP-EXPD-EXCEPT      TO D01-A15
              MOVE SP-EXPD-EXCEPT       TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-EXPR-FRMLA        NOT = OSP-EXPR-FRMLA 
              MOVE "SP-EXPR-FRMLA"    TO D01-TEXT
              MOVE OSP-EXPR-FRMLA     TO D01-A15
              MOVE SP-EXPR-FRMLA      TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-EXPR-REDU-FRMLA     NOT = OSP-EXPR-REDU-FRMLA 
              MOVE "SP-EXPR-REDU-FRMLA" TO D01-TEXT
              MOVE OSP-EXPR-REDU-FRMLA  TO D01-A15
              MOVE SP-EXPR-REDU-FRMLA   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
             IF SP-EXPR-RATE(SUB)   NOT = OSP-EXPR-RATE(SUB)
                STRING "SP-EXPR-RATE(",SUB,")" INTO D01-TEXT 
                MOVE OSP-EXPR-RATE(SUB)          TO D01-A15-NUM
                MOVE SP-EXPR-RATE(SUB)           TO D01-S35-NUM
                PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
             IF SP-EXPR-BREAK(SUB)   NOT = OSP-EXPR-BREAK(SUB)
                STRING "SP-EXPR-BREAK(",SUB,")" INTO D01-TEXT 
                MOVE OSP-EXPR-BREAK(SUB)          TO D01-A15-NUM
                MOVE SP-EXPR-BREAK(SUB)           TO D01-S35-NUM
                PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
             IF SP-EXPR-YRTYPE(SUB)   NOT = OSP-EXPR-YRTYPE(SUB)
                STRING "SP-EXPR-YRTYPE(",SUB,")" INTO D01-TEXT 
                MOVE OSP-EXPR-YRTYPE(SUB)          TO D01-A15
                MOVE SP-EXPR-YRTYPE(SUB)           TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
             IF SP-EXPR-INTMETH(SUB)   NOT = OSP-EXPR-INTMETH(SUB)
                STRING "SP-EXPR-INTMETH(",SUB,")" INTO D01-TEXT 
                MOVE OSP-EXPR-INTMETH(SUB)          TO D01-A15
                MOVE SP-EXPR-INTMETH(SUB)           TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-EXPR-LCHG      NOT = OSP-EXPR-LCHG  
              MOVE "SP-EXPR-LCHG"  TO D01-TEXT
              MOVE OSP-EXPR-LCHG   TO D01-A15
              MOVE SP-EXPR-LCHG    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-JUDGFRMLA      NOT = OSP-JUDGFRMLA  
              MOVE "SP-JUDGFRMLA"  TO D01-TEXT
              MOVE OSP-JUDGFRMLA   TO D01-A15
              MOVE SP-JUDGFRMLA    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-JUDGRATE       NOT = OSP-JUDGRATE  
              MOVE "SP-JUDGRATE"   TO D01-TEXT
              MOVE OSP-JUDGRATE    TO D01-A15-NUM
              MOVE SP-JUDGRATE     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-JUDGYRTYPE      NOT = OSP-JUDGYRTYPE  
              MOVE "SP-JUDGYRTYPE"  TO D01-TEXT
              MOVE OSP-JUDGYRTYPE   TO D01-A15
              MOVE SP-JUDGYRTYPE    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.


           IF SP-JUDGINTMETH     NOT = OSP-JUDGINTMETH  
              MOVE "SP-JUDGINTMETH" TO D01-TEXT
              MOVE OSP-JUDGINTMETH  TO D01-A15
              MOVE SP-JUDGINTMETH   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-ACC-FRMLA      NOT = OSP-ACC-FRMLA  
              MOVE "SP-ACC-FRMLA"  TO D01-TEXT
              MOVE OSP-ACC-FRMLA   TO D01-A15
              MOVE SP-ACC-FRMLA    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-ACC-YRTYPE     NOT = OSP-ACC-YRTYPE  
              MOVE "SP-ACC-YRTYPE" TO D01-TEXT
              MOVE OSP-ACC-YRTYPE  TO D01-A15
              MOVE SP-ACC-YRTYPE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-ACC-INTMETH      NOT = OSP-ACC-INTMETH  
              MOVE "SP-ACC-INTMETH"  TO D01-TEXT
              MOVE OSP-ACC-INTMETH   TO D01-A15
              MOVE SP-ACC-INTMETH    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
             IF SP-ACC-RATE(SUB)   NOT = OSP-ACC-RATE(SUB)
                STRING "ACC-RATE(",SUB,")" INTO D01-TEXT 
                MOVE OSP-ACC-RATE(SUB)          TO D01-A15-NUM
                MOVE SP-ACC-RATE(SUB)           TO D01-S35-NUM
                PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 5
             IF SP-ACC-BREAK(SUB)   NOT = OSP-ACC-BREAK(SUB)
                STRING "SP-ACC-BREAK(",SUB,")" INTO D01-TEXT 
                MOVE OSP-ACC-BREAK(SUB)          TO D01-A15-NUM
                MOVE SP-ACC-BREAK(SUB)           TO D01-S35-NUM
                PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-1098FRMLA          NOT = OSP-1098FRMLA   
              MOVE "SP-1098FRMLA "     TO D01-TEXT
              MOVE OSP-1098FRMLA       TO D01-A15
              MOVE SP-1098FRMLA        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-DISFRMLA          NOT = OSP-DISFRMLA  
              MOVE "SP-DISFRMLA"      TO D01-TEXT
              MOVE OSP-DISFRMLA       TO D01-A15
              MOVE SP-DISFRMLA        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-CONTRFRMLA          NOT = OSP-CONTRFRMLA  
              MOVE "SP-CONTRFRMLA"      TO D01-TEXT
              MOVE OSP-CONTRFRMLA       TO D01-A15
              MOVE SP-CONTRFRMLA        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-BANKRULE          NOT = OSP-BANKRULE  
              MOVE "SP-BANKRULE"      TO D01-TEXT
              MOVE OSP-BANKRULE       TO D01-A15
              MOVE SP-BANKRULE        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-CONTPDTH          NOT = OSP-CONTPDTH  
              MOVE "SP-CONTPDTH"      TO D01-TEXT
              MOVE OSP-CONTPDTH       TO D01-A15
              MOVE SP-CONTPDTH        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-FINCHG          NOT = OSP-FINCHG  
              MOVE "SP-FINCHG"      TO D01-TEXT
              MOVE OSP-FINCHG       TO D01-A15
              MOVE SP-FINCHG        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-BANKUNITPER-CD     NOT = OSP-BANKUNITPER-CD  
              MOVE "SP-BANKUNITPER-CD" TO D01-TEXT
              MOVE OSP-BANKUNITPER-CD  TO D01-A15
              MOVE SP-BANKUNITPER-CD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-TRWBANKRULE     NOT = OSP-TRWBANKRULE  
              MOVE "SP-TRWBANKRULE" TO D01-TEXT
              MOVE OSP-TRWBANKRULE  TO D01-A15
              MOVE SP-TRWBANKRULE   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-TRWCONTPDTH      NOT = OSP-TRWCONTPDTH  
              MOVE "SP-TRWCONTPDTH"  TO D01-TEXT
              MOVE OSP-TRWCONTPDTH   TO D01-A15
              MOVE SP-TRWCONTPDTH    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-TRWBANKUNITPER-CD     NOT = OSP-TRWBANKUNITPER-CD  
              MOVE "SP-TRWBANKUNITPER-CD" TO D01-TEXT
              MOVE OSP-TRWBANKUNITPER-CD  TO D01-A15
              MOVE SP-TRWBANKUNITPER-CD   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-RECFRMLA        NOT = OSP-RECFRMLA  
              MOVE "SP-RECFRMLA"    TO D01-TEXT
              MOVE OSP-RECFRMLA     TO D01-A15
              MOVE SP-RECFRMLA      TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-RECPAYCD      NOT = OSP-RECPAYCD  
              MOVE "SP-RECPAYCD"  TO D01-TEXT
              MOVE OSP-RECPAYCD   TO D01-A15
              MOVE SP-RECPAYCD    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-RECPDTH     NOT = OSP-RECPDTH  
              MOVE "SP-RECPDTH" TO D01-TEXT
              MOVE OSP-RECPDTH  TO D01-A15
              MOVE SP-RECPDTH   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-DISFRMLA-SPAY-FG     NOT = OSP-DISFRMLA-SPAY-FG  
              MOVE "SP-DISFRMLA-SPAY-FG" TO D01-TEXT
              MOVE OSP-DISFRMLA-SPAY-FG  TO D01-A15
              MOVE SP-DISFRMLA-SPAY-FG   TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
             IF SP-ERNFRMLA(SUB)     NOT = OSP-ERNFRMLA(SUB)
                STRING "SP-ERNFRMLA(",SUB,")" INTO D01-TEXT 
                MOVE OSP-ERNFRMLA(SUB)  TO D01-A15
                MOVE SP-ERNFRMLA(SUB)   TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR. 
  
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
             IF SP-ERNAMT(SUB)  NOT = OSP-ERNAMT(SUB)
                STRING "SP-ERNAMT(",SUB,")" INTO D01-TEXT 
                MOVE OSP-ERNAMT(SUB)          TO D01-A15-NUM
                MOVE SP-ERNAMT(SUB)           TO D01-S35-NUM
                PERFORM WRITE-DETAIL-LINE-ERROR. 
  
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
             IF SP-ERNPERCT(SUB)   NOT = OSP-ERNPERCT(SUB)
                STRING "SP-ERNPERCT(",SUB,")" INTO D01-TEXT 
                MOVE OSP-ERNPERCT(SUB)          TO D01-A15-NUM
                MOVE SP-ERNPERCT(SUB)           TO D01-S35-NUM
                PERFORM WRITE-DETAIL-LINE-ERROR. 
  
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
             IF SP-ERNPERCD(SUB)  NOT = OSP-ERNPERCD(SUB)
                STRING "SP-ERNPERCD(",SUB,")" INTO D01-TEXT 
                MOVE OSP-ERNPERCD(SUB)          TO D01-A15
                MOVE SP-ERNPERCD(SUB)           TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR. 
  
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
             IF SP-ERNACCRL-FG(SUB)  NOT = OSP-ERNACCRL-FG(SUB)
                STRING "AL-ERNACCRL-FG(",SUB,")" INTO D01-TEXT 
                MOVE OSP-ERNACCRL-FG(SUB) TO D01-A15
                MOVE SP-ERNACCRL-FG(SUB)  TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR. 
  
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
             IF SP-ERNACCRL-START-CD(SUB) 
                      NOT = OSP-ERNACCRL-START-CD(SUB)
                STRING "AL-ERNACCRL-START-CD(",SUB,")" INTO D01-TEXT 
                MOVE OSP-ERNACCRL-START-CD(SUB)          TO D01-A15
                MOVE SP-ERNACCRL-START-CD(SUB)           TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR. 
  
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
             IF SP-ERNACCRL-DFCD(SUB) NOT = OSP-ERNACCRL-DFCD(SUB)
                STRING "AL-ERNACCRL-DFCD(",SUB,")" INTO D01-TEXT 
                MOVE OSP-ERNACCRL-DFCD(SUB)          TO D01-A15
                MOVE SP-ERNACCRL-DFCD(SUB)           TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR. 
  
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
             IF SP-ERN-DAYS(SUB) NOT = OSP-ERN-DAYS(SUB)
                STRING "AL-ERN-DAYS(",SUB,")" INTO D01-TEXT 
                MOVE OSP-ERN-DAYS(SUB)          TO D01-A15
                MOVE SP-ERN-DAYS(SUB)           TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR. 
    
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
             IF SP-ERN-TERM(SUB) NOT = OSP-ERN-TERM(SUB)
                STRING "AL-ERN-TERM(",SUB,")" INTO D01-TEXT 
                MOVE OSP-ERN-TERM(SUB)          TO D01-A15
                MOVE SP-ERN-TERM(SUB)           TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR. 
 
      *    IF SP-ANTICIPATED         NOT = OSP-ANTICIPATED   
      *       MOVE "SP-ANTICIPATED "     TO D01-TEXT
      *       MOVE OSP-ANTICIPATED       TO D01-A15
      *       MOVE SP-ANTICIPATED        TO D01-S35
      *       PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-ERNACCEL-CD          NOT = OSP-ERNACCEL-CD  
              MOVE "SP-ERNACCEL-CD"      TO D01-TEXT
              MOVE OSP-ERNACCEL-CD       TO D01-A15
              MOVE SP-ERNACCEL-CD        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-ERNJD-CD          NOT = OSP-ERNJD-CD  
              MOVE "SP-ERNJD-CD"      TO D01-TEXT
              MOVE OSP-ERNJD-CD       TO D01-A15
              MOVE SP-ERNJD-CD        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-MIDSTRERNCD          NOT = OSP-MIDSTRERNCD  
              MOVE "SP-MIDSTRERNCD"      TO D01-TEXT
              MOVE OSP-MIDSTRERNCD       TO D01-A15
              MOVE SP-MIDSTRERNCD        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 6
             IF SP-EARN-ORDER(SUB) NOT = OSP-EARN-ORDER(SUB)
                STRING "SP-EARN-ORDER(",SUB,")" INTO D01-TEXT 
                MOVE OSP-EARN-ORDER(SUB)          TO D01-A15
                MOVE SP-EARN-ORDER(SUB)           TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR.  

           IF SP-ERNDELQFRMLA          NOT = OSP-ERNDELQFRMLA  
              MOVE "SP-ERNDELQFRMLA"      TO D01-TEXT
              MOVE OSP-ERNDELQFRMLA       TO D01-A15
              MOVE SP-ERNDELQFRMLA        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-ERNDELQ-START          NOT = OSP-ERNDELQ-START  
              MOVE "SP-ERNDELQ-START"      TO D01-TEXT
              MOVE OSP-ERNDELQ-START       TO D01-A15
              MOVE SP-ERNDELQ-START        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-ERNDELQ-STOP          NOT = OSP-ERNDELQ-STOP  
              MOVE "SP-ERNDELQ-STOP"      TO D01-TEXT
              MOVE OSP-ERNDELQ-STOP       TO D01-A15
              MOVE SP-ERNDELQ-STOP        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-1098-INC-LC          NOT = OSP-1098-INC-LC  
              MOVE "SP-1098-INC-LC"      TO D01-TEXT
              MOVE OSP-1098-INC-LC       TO D01-A15
              MOVE SP-1098-INC-LC        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-MAX-IB-ACR-DAYS  NOT = OSP-MAX-IB-ACR-DAYS  
              MOVE "SP-MAX-IB-ACR-DAYS"      TO D01-TEXT
              MOVE OSP-MAX-IB-ACR-DAYS       TO D01-A15
              MOVE SP-MAX-IB-ACR-DAYS        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-LCGRACE          NOT = OSP-LCGRACE  
              MOVE "SP-LCGRACE"      TO D01-TEXT
              MOVE OSP-LCGRACE       TO D01-A15
              MOVE SP-LCGRACE        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-LCGRACECD          NOT = OSP-LCGRACECD  
              MOVE "SP-LCGRACECD"      TO D01-TEXT
              MOVE OSP-LCGRACECD       TO D01-A15
              MOVE SP-LCGRACECD        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-LCTYPE          NOT = OSP-LCTYPE  
              MOVE "SP-LCTYPE"      TO D01-TEXT
              MOVE OSP-LCTYPE       TO D01-A15
              MOVE SP-LCTYPE        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-LCRATE          NOT = OSP-LCRATE  
              MOVE "SP-LCRATE"      TO D01-TEXT
              MOVE OSP-LCRATE       TO D01-A15-NUM
              MOVE SP-LCRATE        TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-LCFRMLA          NOT = OSP-LCFRMLA  
              MOVE "SP-LCFRMLA"      TO D01-TEXT
              MOVE OSP-LCFRMLA       TO D01-A15
              MOVE SP-LCFRMLA        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-LCMINCHG          NOT = OSP-LCMINCHG  
              MOVE "SP-LCMINCHG"      TO D01-TEXT
              MOVE OSP-LCMINCHG       TO D01-A15-NUM
              MOVE SP-LCMINCHG        TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-LCMAXAMT          NOT = OSP-LCMAXAMT  
              MOVE "SP-LCMAXAMT"      TO D01-TEXT
              MOVE OSP-LCMAXAMT       TO D01-A15-NUM
              MOVE SP-LCMAXAMT        TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-LCDELFAC          NOT = OSP-LCDELFAC  
              MOVE "SP-LCDELFAC"      TO D01-TEXT
              MOVE OSP-LCDELFAC       TO D01-A15
              MOVE SP-LCDELFAC        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-LCYRTYPE          NOT = OSP-LCYRTYPE  
              MOVE "SP-LCYRTYPE"      TO D01-TEXT
              MOVE OSP-LCYRTYPE       TO D01-A15
              MOVE SP-LCYRTYPE        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-LC-CODE          NOT = OSP-LC-CODE  
              MOVE "SP-LC-CODE"      TO D01-TEXT
              MOVE OSP-LC-CODE       TO D01-A15
              MOVE SP-LC-CODE        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-LCUNITPER-CD   NOT = OSP-LCUNITPER-CD  
              MOVE "SP-LCUNITPER-CD"      TO D01-TEXT
              MOVE OSP-LCUNITPER-CD       TO D01-A15
              MOVE SP-LCUNITPER-CD        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-DEFFRMLA   NOT = OSP-DEFFRMLA  
              MOVE "SP-DEFFRMLA"      TO D01-TEXT
              MOVE OSP-DEFFRMLA       TO D01-A15
              MOVE SP-DEFFRMLA        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-DEFEXCEPT   NOT = OSP-DEFEXCEPT  
              MOVE "SP-DEFEXCEPT"      TO D01-TEXT
              MOVE OSP-DEFEXCEPT       TO D01-A15
              MOVE SP-DEFEXCEPT        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-DEFWORKER   NOT = OSP-DEFWORKER  
              MOVE "SP-DEFWORKER"      TO D01-TEXT
              MOVE OSP-DEFWORKER       TO D01-A15-NUM
              MOVE SP-DEFWORKER        TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-DEFPERCD   NOT = OSP-DEFPERCD  
              MOVE "SP-DEFPERCD"      TO D01-TEXT
              MOVE OSP-DEFPERCD       TO D01-A15
              MOVE SP-DEFPERCD        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-DEFBASE   NOT = OSP-DEFBASE  
              MOVE "SP-DEFBASE"      TO D01-TEXT
              MOVE OSP-DEFBASE       TO D01-A15
              MOVE SP-DEFBASE        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-DEFPOLCD   NOT = OSP-DEFPOLCD  
              MOVE "SP-DEFPOLCD"      TO D01-TEXT
              MOVE OSP-DEFPOLCD       TO D01-A15
              MOVE SP-DEFPOLCD        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
             PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
             IF SP-DEFPOLMX(SUB)  NOT = OSP-DEFPOLMX(SUB)
                STRING "SP-DEFPOLMX(",SUB,")" INTO D01-TEXT 
                MOVE OSP-DEFPOLMX(SUB)          TO D01-A15
                MOVE SP-DEFPOLMX(SUB)           TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-DEFMIN   NOT = OSP-DEFMIN  
              MOVE "SP-DEFMIN"      TO D01-TEXT
              MOVE OSP-DEFMIN       TO D01-A15-NUM
              MOVE SP-DEFMIN        TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-DEFMAX   NOT = OSP-DEFMAX  
              MOVE "SP-DEFMAX"      TO D01-TEXT
              MOVE OSP-DEFMAX       TO D01-A15-NUM
              MOVE SP-DEFMAX        TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-DEFUNITPER-CD   NOT = OSP-DEFUNITPER-CD  
              MOVE "SP-DEFUNITPER-CD"      TO D01-TEXT
              MOVE OSP-DEFUNITPER-CD       TO D01-A15
              MOVE SP-DEFUNITPER-CD        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 20
             IF SP-PRFORM(SUB)  NOT = OSP-PRFORM(SUB)
                STRING "SP-PRFORM(",SUB,")" INTO D01-TEXT 
                MOVE OSP-PRFORM(SUB)          TO D01-A15
                MOVE SP-PRFORM(SUB)           TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR. 

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 20
             IF SP-PRFLAG(SUB)  NOT = OSP-PRFLAG(SUB)
                STRING "SP-PRFLAG(",SUB,")" INTO D01-TEXT 
                MOVE OSP-PRFLAG(SUB)          TO D01-A15
                MOVE SP-PRFLAG(SUB)           TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR. 
 
           IF SP-IMED-REC   NOT = OSP-IMED-REC  
              MOVE "SP-IMED-REC"      TO D01-TEXT
              MOVE OSP-IMED-REC       TO D01-A15
              MOVE SP-IMED-REC        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-SCFRMLA   NOT = OSP-SCFRMLA  
              MOVE "SP-SCFRMLA"      TO D01-TEXT
              MOVE OSP-SCFRMLA       TO D01-A15
              MOVE SP-SCFRMLA        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-SCRATE   NOT = OSP-SCRATE  
              MOVE "SP-SCRATE"      TO D01-TEXT
              MOVE OSP-SCRATE       TO D01-A15-NUM
              MOVE SP-SCRATE        TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-SCMIN   NOT = OSP-SCMIN  
              MOVE "SP-SCMIN"      TO D01-TEXT
              MOVE OSP-SCMIN       TO D01-A15-NUM
              MOVE SP-SCMIN        TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-SCMAX   NOT = OSP-SCMAX  
              MOVE "SP-SCMAX"      TO D01-TEXT
              MOVE OSP-SCMAX       TO D01-A15-NUM
              MOVE SP-SCMAX        TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-MFFRMLA   NOT = OSP-MFFRMLA  
              MOVE "SP-MFFRMLA"      TO D01-TEXT
              MOVE OSP-MFFRMLA       TO D01-A15
              MOVE SP-MFFRMLA        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-MFRATE   NOT = OSP-MFRATE  
              MOVE "SP-MFRATE"      TO D01-TEXT
              MOVE OSP-MFRATE       TO D01-A15-NUM
              MOVE SP-MFRATE        TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-MFMIN   NOT = OSP-MFMIN  
              MOVE "SP-MFMIN"      TO D01-TEXT
              MOVE OSP-MFMIN       TO D01-A15-NUM
              MOVE SP-MFMIN        TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-MFMAX   NOT = OSP-MFMAX  
              MOVE "SP-MFMAX"      TO D01-TEXT
              MOVE OSP-MFMAX       TO D01-A15-NUM
              MOVE SP-MFMAX        TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-INTMETHOD   NOT = OSP-INTMETHOD  
              MOVE "SP-INTMETHOD"      TO D01-TEXT
              MOVE OSP-INTMETHOD       TO D01-A15
              MOVE SP-INTMETHOD        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-YEARTYPE    NOT = OSP-YEARTYPE       
              MOVE "SP-YEARTYPE     "      TO D01-TEXT
              MOVE OSP-YEARTYPE            TO D01-A15
              MOVE SP-YEARTYPE             TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-PYEXFRMLA   NOT = OSP-PYEXFRMLA  
              MOVE "SP-PYEXFRMLA"      TO D01-TEXT
              MOVE OSP-PYEXFRMLA       TO D01-A15
              MOVE SP-PYEXFRMLA        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-PYEXSPCODE   NOT = OSP-PYEXSPCODE  
              MOVE "SP-PYEXSPCODE"      TO D01-TEXT
              MOVE OSP-PYEXSPCODE       TO D01-A15
              MOVE SP-PYEXSPCODE        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-PYEXYEARTP   NOT = OSP-PYEXYEARTP  
              MOVE "SP-PYEXYEARTP"      TO D01-TEXT
              MOVE OSP-PYEXYEARTP       TO D01-A15
              MOVE SP-PYEXYEARTP        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-PYEXFMDATE   NOT = OSP-PYEXFMDATE  
              MOVE "SP-PYEXFMDATE"      TO D01-TEXT
              MOVE OSP-PYEXFMDATE       TO D01-A15
              MOVE SP-PYEXFMDATE        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-PYEXBASECD   NOT = OSP-PYEXBASECD  
              MOVE "SP-PYEXBASECD"      TO D01-TEXT
              MOVE OSP-PYEXBASECD       TO D01-A15
              MOVE SP-PYEXBASECD        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-PYEXBASE   NOT = OSP-PYEXBASE  
              MOVE "SP-PYEXBASE"      TO D01-TEXT
              MOVE OSP-PYEXBASE       TO D01-A15
              MOVE SP-PYEXBASE        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-PYEXDAYS   NOT = OSP-PYEXDAYS  
              MOVE "SP-PYEXDAYS"      TO D01-TEXT
              MOVE OSP-PYEXDAYS       TO D01-A15
              MOVE SP-PYEXDAYS        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-CAL-SPFRMLA   NOT = OSP-CAL-SPFRMLA  
              MOVE "SP-CAL-SPFRMLA"      TO D01-TEXT
              MOVE OSP-CAL-SPFRMLA       TO D01-A15
              MOVE SP-CAL-SPFRMLA        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
             IF SP-CAL-RATETYPE(SUB)  NOT = OSP-CAL-RATETYPE(SUB)
                STRING "SP-CAL-RATETYPE(",SUB,")" INTO D01-TEXT 
                MOVE OSP-CAL-RATETYPE(SUB)          TO D01-A15
                MOVE SP-CAL-RATETYPE(SUB)           TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR. 

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
             IF SP-CAL-BREAKTYPE(SUB)  NOT = OSP-CAL-BREAKTYPE(SUB)
                STRING "SP-CAL-BREAKTYPE(",SUB,")" INTO D01-TEXT 
                MOVE OSP-CAL-BREAKTYPE(SUB)          TO D01-A15
                MOVE SP-CAL-BREAKTYPE(SUB)           TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR. 

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
             IF SP-CAL-CHGABLE(SUB)  NOT = OSP-CAL-CHGABLE(SUB)
                STRING "SP-CAL-CHGABLE(",SUB,")" INTO D01-TEXT 
                MOVE OSP-CAL-CHGABLE(SUB)          TO D01-A15
                MOVE SP-CAL-CHGABLE(SUB)           TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR. 

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
             IF SP-CAL-NORATES(SUB)  NOT = OSP-CAL-NORATES(SUB)
                STRING "SP-CAL-NORATES(",SUB,")" INTO D01-TEXT 
                MOVE OSP-CAL-NORATES(SUB)          TO D01-A15
                MOVE SP-CAL-NORATES(SUB)           TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR. 

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
             IF SP-CAL-TBFRMLA(SUB)  NOT = OSP-CAL-TBFRMLA(SUB)
                STRING "SP-CAL-TBFRMLA(",SUB,")" INTO D01-TEXT 
                MOVE OSP-CAL-TBFRMLA(SUB)          TO D01-A15
                MOVE SP-CAL-TBFRMLA(SUB)           TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR. 

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
             PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 5
                IF SP-CAL-RATE(SUB,SUB1) NOT = OSP-CAL-RATE(SUB,SUB1)
                   STRING "SP-CAL-RATE(",SUB,")-",SUB1
                                        INTO D01-TEXT
                   MOVE OSP-CAL-RATE(SUB,SUB1)   TO D01-A15-NUM
                   MOVE SP-CAL-RATE(SUB,SUB1)    TO D01-S35-NUM
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
             END-PERFORM
           END-PERFORM.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
             PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 5
                IF SP-CAL-BREAK(SUB,SUB1) NOT = OSP-CAL-BREAK(SUB,SUB1)
                   STRING "SP-CAL-BREAK(",SUB,")-",SUB1
                                        INTO D01-TEXT
                   MOVE OSP-CAL-BREAK(SUB,SUB1)  TO D01-A15-NUM
                   MOVE SP-CAL-BREAK(SUB,SUB1)   TO D01-S35-NUM
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
             END-PERFORM
           END-PERFORM.
    
           IF SP-MINSF-FRMLA   NOT = OSP-MINSF-FRMLA  
              MOVE "SP-MINSF-FRMLA"      TO D01-TEXT
              MOVE OSP-MINSF-FRMLA       TO D01-A15
              MOVE SP-MINSF-FRMLA        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.               
  
           IF SP-MINSF-CHG   NOT = OSP-MINSF-CHG  
              MOVE "SP-MINSF-CHG"      TO D01-TEXT
              MOVE OSP-MINSF-CHG       TO D01-A15-NUM
              MOVE SP-MINSF-CHG        TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-MAXAPR-FRMLA   NOT = OSP-MAXAPR-FRMLA  
              MOVE "SP-MAXAPR-FRMLA"      TO D01-TEXT
              MOVE OSP-MAXAPR-FRMLA       TO D01-A15
              MOVE SP-MAXAPR-FRMLA        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2 
             IF SP-EDT-NOEDITS(SUB)  NOT = OSP-EDT-NOEDITS(SUB)
                STRING "SP-EDT-NOEDITS(",SUB,")" INTO D01-TEXT 
                MOVE OSP-EDT-NOEDITS(SUB)          TO D01-A15
                MOVE SP-EDT-NOEDITS(SUB)           TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
             PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 6
                IF SP-EDT-TYPE(SUB,SUB1) NOT = OSP-EDT-TYPE(SUB,SUB1)
                   STRING "SP-EDT-TYPE(",SUB,")-",SUB1
                                        INTO D01-TEXT
                   MOVE OSP-EDT-TYPE(SUB,SUB1) TO D01-A15
                   MOVE SP-EDT-TYPE(SUB,SUB1)  TO D01-S35
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
             END-PERFORM
           END-PERFORM.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
             PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 6
                IF SP-EDT-HL(SUB,SUB1) NOT = OSP-EDT-HL(SUB,SUB1)
                   STRING "SP-EDT-HL(",SUB,")-",SUB1
                                        INTO D01-TEXT
                   MOVE OSP-EDT-HL(SUB,SUB1) TO D01-A15
                   MOVE SP-EDT-HL(SUB,SUB1)  TO D01-S35
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
             END-PERFORM
           END-PERFORM.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
             PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 6
                IF SP-EDT-COLL(SUB,SUB1) NOT = OSP-EDT-COLL(SUB,SUB1)
                   STRING "SP-EDT-COLL(",SUB,")-",SUB1
                                        INTO D01-TEXT
                   MOVE OSP-EDT-COLL(SUB,SUB1) TO D01-A15
                   MOVE SP-EDT-COLL(SUB,SUB1)  TO D01-S35
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
             END-PERFORM
           END-PERFORM.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
             PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 6
                IF SP-EDT-CD(SUB,SUB1) NOT = OSP-EDT-CD(SUB,SUB1)
                   STRING "SP-EDT-CD(",SUB,")-",SUB1
                                        INTO D01-TEXT
                   MOVE OSP-EDT-CD(SUB,SUB1) TO D01-A15
                   MOVE SP-EDT-CD(SUB,SUB1)  TO D01-S35
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
             END-PERFORM
           END-PERFORM.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
             PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 6
                IF SP-EDT-RATE(SUB,SUB1) NOT = OSP-EDT-RATE(SUB,SUB1)
                   STRING "SP-EDT-RATE(",SUB,")-",SUB1
                                        INTO D01-TEXT
                   MOVE OSP-EDT-RATE(SUB,SUB1) TO D01-A15-NUM
                   MOVE SP-EDT-RATE(SUB,SUB1)  TO D01-S35-NUM
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
             END-PERFORM
           END-PERFORM.

             PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
             PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 6
                IF SP-EDT-RATETYPE(SUB,SUB1) 
                      NOT = OSP-EDT-RATETYPE(SUB,SUB1)
                   STRING "SP-EDT-RATETYPE(",SUB,")-",SUB1
                                        INTO D01-TEXT
                   MOVE OSP-EDT-RATETYPE(SUB,SUB1) TO D01-A15
                   MOVE SP-EDT-RATETYPE(SUB,SUB1)  TO D01-S35
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
             END-PERFORM
           END-PERFORM.
  
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
             PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 6
                IF SP-EDT-AMT(SUB,SUB1) NOT = OSP-EDT-AMT(SUB,SUB1)
                   STRING "SP-EDT-AMT(",SUB,")-",SUB1
                                        INTO D01-TEXT
                   MOVE OSP-EDT-AMT(SUB,SUB1) TO D01-A15-NUM
                   MOVE SP-EDT-AMT(SUB,SUB1)  TO D01-S35-NUM
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
             END-PERFORM
           END-PERFORM.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
             PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 6
                IF SP-EDT-AMTYPE(SUB,SUB1) 
                      NOT = OSP-EDT-AMTYPE(SUB,SUB1)
                   STRING "SP-EDT-AMTYPE(",SUB,")-",SUB1
                                        INTO D01-TEXT
                   MOVE OSP-EDT-AMTYPE(SUB,SUB1) TO D01-A15
                   MOVE SP-EDT-AMTYPE(SUB,SUB1)  TO D01-S35
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
             END-PERFORM
           END-PERFORM.

             PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
             PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 6
                IF SP-EDT-TERM(SUB,SUB1) NOT = OSP-EDT-TERM(SUB,SUB1)
                   STRING "SP-EDT-TERM(",SUB,")-",SUB1
                                        INTO D01-TEXT
                   MOVE OSP-EDT-TERM(SUB,SUB1) TO D01-A15-NUM
                   MOVE SP-EDT-TERM(SUB,SUB1)  TO D01-S35-NUM
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
             END-PERFORM
           END-PERFORM.
 
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
             IF SP-INSCOM(SUB) NOT = OSP-INSCOM(SUB)
                STRING "SP-INSCOM(",SUB,")" INTO D01-TEXT 
                MOVE OSP-INSCOM(SUB) TO D01-A15
                MOVE SP-INSCOM(SUB)  TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR. 
  
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
             IF SP-INSRATECD(SUB) NOT = OSP-INSRATECD(SUB)
                STRING "SP-INSRATECD(",SUB,")" INTO D01-TEXT 
                MOVE OSP-INSRATECD(SUB) TO D01-A15
                MOVE SP-INSRATECD(SUB)  TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
             IF SP-INSJFAC(SUB) NOT = OSP-INSJFAC(SUB)
                STRING "SP-INSJFAC(",SUB,")" INTO D01-TEXT 
                MOVE OSP-INSJFAC(SUB) TO D01-A15-NUM
                MOVE SP-INSJFAC(SUB)  TO D01-S35-NUM
                PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
             IF SP-INSDISCNT(SUB) NOT = OSP-INSDISCNT(SUB)
                STRING "SP-INSDISCNT(",SUB,")" INTO D01-TEXT 
                MOVE OSP-INSDISCNT(SUB) TO D01-A15-NUM
                MOVE SP-INSDISCNT(SUB)  TO D01-S35-NUM
                PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
             IF SP-INSCHGABL(SUB) NOT = OSP-INSCHGABL(SUB)
                STRING "SP-INSCHGABL(",SUB,")" INTO D01-TEXT 
                MOVE OSP-INSCHGABL(SUB) TO D01-A15
                MOVE SP-INSCHGABL(SUB)  TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
             PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 20
                IF SP-INSTERM(SUB,SUB1) NOT = OSP-INSTERM(SUB,SUB1)
                   STRING "SP-INSTERM(",SUB,")-",SUB1
                                        INTO D01-TEXT
                   MOVE OSP-INSTERM(SUB,SUB1) TO D01-A15
                   MOVE SP-INSTERM(SUB,SUB1)  TO D01-S35
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
             END-PERFORM
           END-PERFORM.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
             PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 20
                IF SP-INSRATE(SUB,SUB1) NOT = OSP-INSRATE(SUB,SUB1)
                   STRING "SP-INSRATE(",SUB,")-",SUB1
                                        INTO D01-TEXT
                   MOVE OSP-INSRATE(SUB,SUB1) TO D01-A15-NUM
                   MOVE SP-INSRATE(SUB,SUB1)  TO D01-S35-NUM
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
             END-PERFORM
           END-PERFORM.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
             IF SP-INSMINPREM(SUB) NOT = OSP-INSMINPREM(SUB)
                STRING "SP-INSMINPREM(",SUB,")" INTO D01-TEXT 
                MOVE OSP-INSMINPREM(SUB) TO D01-A15-NUM
                MOVE SP-INSMINPREM(SUB)  TO D01-S35-NUM
                PERFORM WRITE-DETAIL-LINE-ERROR. 

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
             IF SP-INSMINCHGABL(SUB) NOT = OSP-INSMINCHGABL(SUB)
                STRING "SP-INSMINCHGABL(",SUB,")" INTO D01-TEXT 
                MOVE OSP-INSMINCHGABL(SUB) TO D01-A15
                MOVE SP-INSMINCHGABL(SUB)  TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR. 

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
             IF SP-INSMAXCHGABL(SUB) NOT = OSP-INSMAXCHGABL(SUB)
                STRING "SP-INSMAXCHGABL(",SUB,")" INTO D01-TEXT 
                MOVE OSP-INSMAXCHGABL(SUB) TO D01-A15
                MOVE SP-INSMAXCHGABL(SUB)  TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR. 

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
             IF SP-INSMAXBENIF(SUB) NOT = OSP-INSMAXBENIF(SUB)
                STRING "SP-INSMAXBENIF(",SUB,")" INTO D01-TEXT 
                MOVE OSP-INSMAXBENIF(SUB) TO D01-A15
                MOVE SP-INSMAXBENIF(SUB)  TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR. 

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
             IF SP-INSMAXTERM(SUB) NOT = OSP-INSMAXTERM(SUB)
                STRING "SP-INSMAXTERM(",SUB,")" INTO D01-TEXT 
                MOVE OSP-INSMAXTERM(SUB) TO D01-A15
                MOVE SP-INSMAXTERM(SUB)  TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR. 

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
             IF SP-INSCHGBEN(SUB) NOT = OSP-INSCHGBEN(SUB)
                STRING "SP-INSCHGBEN(",SUB,")" INTO D01-TEXT 
                MOVE OSP-INSCHGBEN(SUB) TO D01-A15
                MOVE SP-INSCHGBEN(SUB)  TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR. 

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
             IF SP-INSEXCEPT(SUB) NOT = OSP-INSEXCEPT(SUB)
                STRING "SP-INSEXCEPT(",SUB,")" INTO D01-TEXT 
                MOVE OSP-INSEXCEPT(SUB) TO D01-A15
                MOVE SP-INSEXCEPT(SUB)  TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR. 

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
             IF SP-INSNOVER(SUB) NOT = OSP-INSNOVER(SUB)
                STRING "SP-INSNOVER(",SUB,")" INTO D01-TEXT 
                MOVE OSP-INSNOVER(SUB) TO D01-A15
                MOVE SP-INSNOVER(SUB)  TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR. 

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
             PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 5
                IF SP-INSMINAGE(SUB,SUB1) NOT = OSP-INSMINAGE(SUB,SUB1)
                   STRING "SP-INSMINAGE(",SUB,")-",SUB1
                                        INTO D01-TEXT
                   MOVE OSP-INSMINAGE(SUB,SUB1) TO D01-A15
                   MOVE SP-INSMINAGE(SUB,SUB1)  TO D01-S35
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
             END-PERFORM
           END-PERFORM.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
             PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 5
             IF SP-INSMAXAGE(SUB,SUB1) NOT = OSP-INSMAXAGE(SUB,SUB1)
                   STRING "SP-INSMAXAGE(",SUB,")-",SUB1
                                        INTO D01-TEXT
                   MOVE OSP-INSMAXAGE(SUB,SUB1) TO D01-A15
                   MOVE SP-INSMAXAGE(SUB,SUB1)  TO D01-S35
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
             END-PERFORM
           END-PERFORM.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
             PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 5
                IF SP-INSMAXCOV(SUB,SUB1) NOT = OSP-INSMAXCOV(SUB,SUB1)
                   STRING "SP-INSMAXCOV(",SUB,")-",SUB1
                                        INTO D01-TEXT
                   MOVE OSP-INSMAXCOV(SUB,SUB1) TO D01-A15
                   MOVE SP-INSMAXCOV(SUB,SUB1)  TO D01-S35
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
             END-PERFORM
           END-PERFORM.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
             PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 5
                IF SP-INSMAXTRM(SUB,SUB1) NOT = OSP-INSMAXTRM(SUB,SUB1)
                   STRING "SP-INSMAXTRM(",SUB,")-",SUB1
                                        INTO D01-TEXT
                   MOVE OSP-INSMAXTRM(SUB,SUB1) TO D01-A15
                   MOVE SP-INSMAXTRM(SUB,SUB1)  TO D01-S35
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
             END-PERFORM
           END-PERFORM.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
             IF SP-INSFEECD(SUB) NOT = OSP-INSFEECD(SUB)
                STRING "SP-INSFEECD(",SUB,")" INTO D01-TEXT 
                MOVE OSP-INSFEECD(SUB) TO D01-A15
                MOVE SP-INSFEECD(SUB)  TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
             IF SP-INSFEE-FRMLA(SUB) NOT = OSP-INSFEE-FRMLA(SUB)
                STRING "SP-INSFEE-FRMLA(",SUB,")" INTO D01-TEXT 
                MOVE OSP-INSFEE-FRMLA(SUB) TO D01-A15
                MOVE SP-INSFEE-FRMLA(SUB)  TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
             IF SP-INSCHGABL-FG(SUB) NOT = OSP-INSCHGABL-FG(SUB)
                STRING "SP-INSCHGABL-FG(",SUB,")" INTO D01-TEXT 
                MOVE OSP-INSCHGABL-FG(SUB) TO D01-A15
                MOVE SP-INSCHGABL-FG(SUB)  TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
             IF SP-INSREJECT-FG(SUB) NOT = OSP-INSREJECT-FG(SUB)
                STRING "SP-INSREJECT-FG(",SUB,")" INTO D01-TEXT 
                MOVE OSP-INSREJECT-FG(SUB) TO D01-A15
                MOVE SP-INSREJECT-FG(SUB)  TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
             IF SP-INSFINCHG(SUB) NOT = OSP-INSFINCHG(SUB)
                STRING "SP-INSFINCHG(",SUB,")" INTO D01-TEXT 
                MOVE OSP-INSFINCHG(SUB) TO D01-A15
                MOVE SP-INSFINCHG(SUB)  TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-INSAH-JOINT-FG   NOT = OSP-INSAH-JOINT-FG  
              MOVE "SP-INSAH-JOINT-FG"      TO D01-TEXT
              MOVE OSP-INSAH-JOINT-FG       TO D01-A15
              MOVE SP-INSAH-JOINT-FG        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-INS-O1-EDIT   NOT = OSP-INS-O1-EDIT  
              MOVE "SP-INS-O1-EDIT"      TO D01-TEXT
              MOVE OSP-INS-O1-EDIT       TO D01-A15
              MOVE SP-INS-O1-EDIT        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-INS-O2-EDIT   NOT = OSP-INS-O2-EDIT  
              MOVE "SP-INS-O2-EDIT"      TO D01-TEXT
              MOVE OSP-INS-O2-EDIT       TO D01-A15
              MOVE SP-INS-O2-EDIT        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
             PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 16
             IF SP-RBFRMLA(SUB) NOT = OSP-RBFRMLA(SUB)
                STRING "SP-RBFRMLA(",SUB,")" INTO D01-TEXT 
                MOVE OSP-RBFRMLA(SUB) TO D01-A15
                MOVE SP-RBFRMLA(SUB)  TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR.
  
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 16
             PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 2
                IF SP-RBDTE(SUB,SUB1) NOT = OSP-RBDTE(SUB,SUB1)
                   STRING "SP-RBDTE(",SUB,")-",SUB1
                                        INTO D01-TEXT
                   MOVE OSP-RBDTE(SUB,SUB1) TO D01-A15
                   MOVE SP-RBDTE(SUB,SUB1)  TO D01-S35
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
             END-PERFORM
           END-PERFORM. 
  
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 16
             PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 2
                IF SP-RBDAYS(SUB,SUB1) NOT = OSP-RBDAYS(SUB,SUB1)
                   STRING "SP-RBDAYS(",SUB,")-",SUB1
                                        INTO D01-TEXT
                   MOVE OSP-RBDAYS(SUB,SUB1) TO D01-A15
                   MOVE SP-RBDAYS(SUB,SUB1)  TO D01-S35
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
             END-PERFORM
           END-PERFORM.  
  
            PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 16
             PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 2
                IF SP-RBYRTYPE(SUB,SUB1) NOT = OSP-RBYRTYPE(SUB,SUB1)
                   STRING "SP-RBYRTYPE(",SUB,")-",SUB1
                                        INTO D01-TEXT
                   MOVE OSP-RBYRTYPE(SUB,SUB1) TO D01-A15
                   MOVE SP-RBYRTYPE(SUB,SUB1)  TO D01-S35
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
             END-PERFORM
           END-PERFORM.  
  
  
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 16
             IF SP-RBMINRET(SUB) NOT = OSP-RBMINRET(SUB)
                STRING "SP-RBMINRET(",SUB,")" INTO D01-TEXT 
                MOVE OSP-RBMINRET(SUB) TO D01-A15-NUM
                MOVE SP-RBMINRET(SUB)  TO D01-S35-NUM
                PERFORM WRITE-DETAIL-LINE-ERROR. 
  
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 16
             IF SP-RBNOMON(SUB) NOT = OSP-RBNOMON(SUB)
                STRING "SP-RBNOMON(",SUB,")" INTO D01-TEXT 
                MOVE OSP-RBNOMON(SUB) TO D01-A15
                MOVE SP-RBNOMON(SUB)  TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR. 
  
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 16
             IF SP-RBDEDCHG(SUB) NOT = OSP-RBDEDCHG(SUB)
                STRING "SP-RBDEDCHG(",SUB,")" INTO D01-TEXT 
                MOVE OSP-RBDEDCHG(SUB) TO D01-A15-NUM
                MOVE SP-RBDEDCHG(SUB)  TO D01-S35-NUM
                PERFORM WRITE-DETAIL-LINE-ERROR. 
  
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 16
             IF SP-RBRECAST(SUB) NOT = OSP-RBRECAST(SUB)
                STRING "SP-RBRECAST(",SUB,")" INTO D01-TEXT 
                MOVE OSP-RBRECAST(SUB) TO D01-A15
                MOVE SP-RBRECAST(SUB)  TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR. 
  
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 16
             IF SP-RBEARLY(SUB) NOT = OSP-RBEARLY(SUB)
                STRING "SP-RBEARLY(",SUB,")" INTO D01-TEXT 
                MOVE OSP-RBEARLY(SUB) TO D01-A15
                MOVE SP-RBEARLY(SUB)  TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR. 
  
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 16
             IF SP-RBDEFER(SUB) NOT = OSP-RBDEFER(SUB)
                STRING "SP-RBDEFER(",SUB,")" INTO D01-TEXT 
                MOVE OSP-RBDEFER(SUB) TO D01-A15
                MOVE SP-RBDEFER(SUB)  TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR. 
  
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 16
             IF SP-RBPCT(SUB) NOT = OSP-RBPCT(SUB)
                STRING "SP-RBPCT(",SUB,")" INTO D01-TEXT 
                MOVE OSP-RBPCT(SUB) TO D01-A15-NUM
                MOVE SP-RBPCT(SUB)  TO D01-S35-NUM
                PERFORM WRITE-DETAIL-LINE-ERROR. 
  
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 16
             IF SP-RBREDUC(SUB) NOT = OSP-RBREDUC(SUB)
                STRING "SP-RBREDUC(",SUB,")" INTO D01-TEXT 
                MOVE OSP-RBREDUC(SUB) TO D01-A15-NUM
                MOVE SP-RBREDUC(SUB)  TO D01-S35-NUM
                PERFORM WRITE-DETAIL-LINE-ERROR. 
  
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 16
             IF SP-RBMIN(SUB) NOT = OSP-RBMIN(SUB)
                STRING "SP-RBMIN(",SUB,")" INTO D01-TEXT 
                MOVE OSP-RBMIN(SUB) TO D01-A15-NUM
                MOVE SP-RBMIN(SUB)  TO D01-S35-NUM
                PERFORM WRITE-DETAIL-LINE-ERROR. 
  
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 16
             IF SP-RBSPOPT1(SUB) NOT = OSP-RBSPOPT1(SUB)
                STRING "SP-RBSPOPT1(",SUB,")" INTO D01-TEXT 
                MOVE OSP-RBSPOPT1(SUB) TO D01-A15
                MOVE SP-RBSPOPT1(SUB)  TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR. 
  
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 16
             IF SP-RBSPOPT2(SUB) NOT = OSP-RBSPOPT2(SUB)
                STRING "SP-RBSPOPT2(",SUB,")" INTO D01-TEXT 
                MOVE OSP-RBSPOPT2(SUB) TO D01-A15
                MOVE SP-RBSPOPT2(SUB)  TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR. 
  
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 16
             IF SP-RBMANAGER(SUB) NOT = OSP-RBMANAGER(SUB)
                STRING "SP-RBMANAGER(",SUB,")" INTO D01-TEXT 
                MOVE OSP-RBMANAGER(SUB) TO D01-A15
                MOVE SP-RBMANAGER(SUB)  TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR. 
  
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 16
             IF SP-RBUNITPER-CD(SUB) NOT = OSP-RBUNITPER-CD(SUB)
                STRING "SP-RBUNITPER-CD(",SUB,")" INTO D01-TEXT 
                MOVE OSP-RBUNITPER-CD(SUB) TO D01-A15
                MOVE SP-RBUNITPER-CD(SUB)  TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR. 
  
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 16
             IF SP-RBFRMLA2(SUB) NOT = OSP-RBFRMLA2(SUB)
                STRING "SP-RBFRMLA2(",SUB,")" INTO D01-TEXT 
                MOVE OSP-RBFRMLA2(SUB) TO D01-A15
                MOVE SP-RBFRMLA2(SUB)  TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR. 
  
           IF SP-RBCOMCD   NOT = OSP-RBCOMCD  
              MOVE "SP-RBCOMCD"      TO D01-TEXT
              MOVE OSP-RBCOMCD       TO D01-A15
              MOVE SP-RBCOMCD        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-ADDON-DISFRMLA   NOT = OSP-ADDON-DISFRMLA  
              MOVE "SP-ADDON-DISFRMLA"      TO D01-TEXT
              MOVE OSP-ADDON-DISFRMLA       TO D01-A15
              MOVE SP-ADDON-DISFRMLA        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-ADDON-WHOLE-FG   NOT = OSP-ADDON-WHOLE-FG  
              MOVE "SP-ADDON-WHOLE-FG"      TO D01-TEXT
              MOVE OSP-ADDON-WHOLE-FG       TO D01-A15
              MOVE SP-ADDON-WHOLE-FG        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-ADDON-ORG-MATURITY   NOT = OSP-ADDON-ORG-MATURITY  
              MOVE "SP-ADDON-ORG-MATURITY"      TO D01-TEXT
              MOVE OSP-ADDON-ORG-MATURITY       TO D01-A15
              MOVE SP-ADDON-ORG-MATURITY        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-CONSOLIDATE-RT-FG   NOT = OSP-CONSOLIDATE-RT-FG  
              MOVE "SP-CONSOLIDATE-RT-FG"      TO D01-TEXT
              MOVE OSP-CONSOLIDATE-RT-FG       TO D01-A15
              MOVE SP-CONSOLIDATE-RT-FG        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-1STPY-ADDON-DAYS   NOT = OSP-1STPY-ADDON-DAYS  
              MOVE "SP-1STPY-ADDON-DAYS"      TO D01-TEXT
              MOVE OSP-1STPY-ADDON-DAYS       TO D01-A15
              MOVE SP-1STPY-ADDON-DAYS        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-1STPY-ADDON-YRTYPE   NOT = OSP-1STPY-ADDON-YRTYPE  
              MOVE "SP-1STPY-ADDON-YRTYPE"      TO D01-TEXT
              MOVE OSP-1STPY-ADDON-YRTYPE       TO D01-A15
              MOVE SP-1STPY-ADDON-YRTYPE        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-ADDON-RATE-FG   NOT = OSP-ADDON-RATE-FG  
              MOVE "SP-ADDON-RATE-FG"      TO D01-TEXT
              MOVE OSP-ADDON-RATE-FG       TO D01-A15
              MOVE SP-ADDON-RATE-FG        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-ADDON-RATE   NOT = OSP-ADDON-RATE  
              MOVE "SP-ADDON-RATE"      TO D01-TEXT
              MOVE OSP-ADDON-RATE       TO D01-A15-NUM
              MOVE SP-ADDON-RATE        TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
             IF SP-FEEFRMLA(SUB) NOT = OSP-FEEFRMLA(SUB)
                STRING "SP-FEEFRMLA(",SUB,")" INTO D01-TEXT 
                MOVE OSP-FEEFRMLA(SUB) TO D01-A15
                MOVE SP-FEEFRMLA(SUB)  TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR. 

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
             IF SP-FEECD(SUB) NOT = OSP-FEECD(SUB)
                STRING "SP-FEECD(",SUB,")" INTO D01-TEXT 
                MOVE OSP-FEECD(SUB) TO D01-A15
                MOVE SP-FEECD(SUB)  TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR. 

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
             IF SP-FEEID(SUB) NOT = OSP-FEEID(SUB)
                STRING "SP-FEEID(",SUB,")" INTO D01-TEXT 
                MOVE OSP-FEEID(SUB) TO D01-A15
                MOVE SP-FEEID(SUB)  TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR.  

 
           IF SP-ROUND-PAY-FG   NOT = OSP-ROUND-PAY-FG  
              MOVE "SP-ROUND-PAY-FG"      TO D01-TEXT
              MOVE OSP-ROUND-PAY-FG       TO D01-A15
              MOVE SP-ROUND-PAY-FG        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-IRREGULAR-PAY-FG   NOT = OSP-IRREGULAR-PAY-FG  
              MOVE "SP-IRREGULAR-PAY-FG"      TO D01-TEXT
              MOVE OSP-IRREGULAR-PAY-FG       TO D01-A15
              MOVE SP-IRREGULAR-PAY-FG        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
             IF SP-DLRVCHGS-RB-CD(SUB) NOT = OSP-DLRVCHGS-RB-CD(SUB)
                STRING "SP-DLRVCHGS-RB-CD(",SUB,")" INTO D01-TEXT 
                MOVE OSP-DLRVCHGS-RB-CD(SUB) TO D01-A15
                MOVE SP-DLRVCHGS-RB-CD(SUB)  TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR. 
  
           IF SP-PARVHELD-RB-CD   NOT = OSP-PARVHELD-RB-CD  
              MOVE "SP-PARVHELD-RB-CD"      TO D01-TEXT
              MOVE OSP-PARVHELD-RB-CD       TO D01-A15
              MOVE SP-PARVHELD-RB-CD        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-PARVPAID-RB-CD   NOT = OSP-PARVPAID-RB-CD  
              MOVE "SP-PARVPAID-RB-CD"      TO D01-TEXT
              MOVE OSP-PARVPAID-RB-CD       TO D01-A15
              MOVE SP-PARVPAID-RB-CD        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-CONTR-CREDIT   NOT = OSP-CONTR-CREDIT  
              MOVE "SP-CONTR-CREDIT"      TO D01-TEXT
              MOVE OSP-CONTR-CREDIT       TO D01-A15-NUM
              MOVE SP-CONTR-CREDIT        TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-CONTR-CREDIT-CD   NOT = OSP-CONTR-CREDIT-CD  
              MOVE "SP-CONTR-CREDIT-CD"      TO D01-TEXT
              MOVE OSP-CONTR-CREDIT-CD       TO D01-A15
              MOVE SP-CONTR-CREDIT-CD        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-CONTR-CREDIT-MIN-DEL-PER  
                     NOT = OSP-CONTR-CREDIT-MIN-DEL-PER  
              MOVE "SP-CONTR-CREDIT-MIN-DEL-PER"  TO D01-TEXT
              MOVE OSP-CONTR-CREDIT-MIN-DEL-PER   TO D01-A15-NUM
              MOVE SP-CONTR-CREDIT-MIN-DEL-PER    TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-OPT-CAL-CHGABLE   NOT = OSP-OPT-CAL-CHGABLE  
              MOVE "SP-OPT-CAL-CHGABLE"      TO D01-TEXT
              MOVE OSP-OPT-CAL-CHGABLE       TO D01-A15
              MOVE SP-OPT-CAL-CHGABLE        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-PBDEL-DAYS   NOT = OSP-PBDEL-DAYS  
              MOVE "SP-PBDEL-DAYS"      TO D01-TEXT
              MOVE OSP-PBDEL-DAYS       TO D01-A15
              MOVE SP-PBDEL-DAYS        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-PBINCR-PERCENT   NOT = OSP-PBINCR-PERCENT  
              MOVE "SP-PBINCR-PERCENT"      TO D01-TEXT
              MOVE OSP-PBINCR-PERCENT       TO D01-A15
              MOVE SP-PBINCR-PERCENT        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-CPOT30-CD   NOT = OSP-CPOT30-CD  
              MOVE "SP-CPOT30-CD"      TO D01-TEXT
              MOVE OSP-CPOT30-CD       TO D01-A15
              MOVE SP-CPOT30-CD        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-CPOTCHGOFF-CD   NOT = OSP-CPOTCHGOFF-CD  
              MOVE "SP-CPOTCHGOFF-CD"      TO D01-TEXT
              MOVE OSP-CPOTCHGOFF-CD       TO D01-A15
              MOVE SP-CPOTCHGOFF-CD        TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-RDPOT30-CD   NOT = OSP-RDPOT30-CD  
              MOVE "SP-RDPOT30-CD"  TO D01-TEXT
              MOVE OSP-RDPOT30-CD   TO D01-A15
              MOVE SP-RDPOT30-CD    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-RDPOTCHGOFF-CD   NOT = OSP-RDPOTCHGOFF-CD  
              MOVE "SP-RDPOTCHGOFF-CD"  TO D01-TEXT
              MOVE OSP-RDPOTCHGOFF-CD   TO D01-A15
              MOVE SP-RDPOTCHGOFF-CD    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OSP-MAX-ALLOW-DELINQ NOT NUMERIC
              MOVE 0                       TO OSP-MAX-ALLOW-DELINQ.

           IF SP-MAX-ALLOW-DELINQ   NOT = OSP-MAX-ALLOW-DELINQ  
              MOVE "SP-MAX-ALLOW-DELINQ"  TO D01-TEXT
              MOVE OSP-MAX-ALLOW-DELINQ   TO D01-A15
              MOVE SP-MAX-ALLOW-DELINQ    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
    
           IF SP-RESCIND-BUS-DAYS   NOT = OSP-RESCIND-BUS-DAYS  
              MOVE "SP-RESCIND-BUS-DAYS"  TO D01-TEXT
              MOVE OSP-RESCIND-BUS-DAYS   TO D01-A15
              MOVE SP-RESCIND-BUS-DAYS    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-RESCIND-SATURDAY   NOT = OSP-RESCIND-SATURDAY  
              MOVE "SP-RESCIND-SATURDAY"  TO D01-TEXT
              MOVE OSP-RESCIND-SATURDAY   TO D01-A15
              MOVE SP-RESCIND-SATURDAY    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-MIN-CREDIT-SCORE   NOT = OSP-MIN-CREDIT-SCORE  
              MOVE "SP-MIN-CREDIT-SCORE"  TO D01-TEXT
              MOVE OSP-MIN-CREDIT-SCORE   TO D01-A15
              MOVE SP-MIN-CREDIT-SCORE    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-DUAL-IN-BRANCH   NOT = OSP-DUAL-IN-BRANCH  
              MOVE "SP-DUAL-IN-BRANCH"  TO D01-TEXT
              MOVE OSP-DUAL-IN-BRANCH   TO D01-A15
              MOVE SP-DUAL-IN-BRANCH    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-DUAL-IN-STATE   NOT = OSP-DUAL-IN-STATE  
              MOVE "SP-DUAL-IN-STATE"  TO D01-TEXT
              MOVE OSP-DUAL-IN-STATE   TO D01-A15
              MOVE SP-DUAL-IN-STATE    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-PACESETTER-FG   NOT = OSP-PACESETTER-FG  
              MOVE "SP-PACESETTER-FG"  TO D01-TEXT
              MOVE OSP-PACESETTER-FG   TO D01-A15
              MOVE SP-PACESETTER-FG    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-WAGE-ASSIGN-DAYS   NOT = OSP-WAGE-ASSIGN-DAYS  
              MOVE "SP-WAGE-ASSIGN-DAYS"  TO D01-TEXT
              MOVE OSP-WAGE-ASSIGN-DAYS   TO D01-A15
              MOVE SP-WAGE-ASSIGN-DAYS    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-CAL-PAYMENT-ROUND-FG   NOT = OSP-CAL-PAYMENT-ROUND-FG  
              MOVE "SP-CAL-PAYMENT-ROUND-FG"  TO D01-TEXT
              MOVE OSP-CAL-PAYMENT-ROUND-FG   TO D01-A15
              MOVE SP-CAL-PAYMENT-ROUND-FG    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-CAN-LOANS-FG   NOT = OSP-CAN-LOANS-FG  
              MOVE "SP-CAN-LOANS-FG"  TO D01-TEXT
              MOVE OSP-CAN-LOANS-FG   TO D01-A15
              MOVE SP-CAN-LOANS-FG    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF OSP-YTD-MAX-ALLOW-DELINQ NOT NUMERIC
              MOVE 0                     TO OSP-YTD-MAX-ALLOW-DELINQ.
 
           IF SP-ALLOW-DELINQ-POLCD   NOT = OSP-ALLOW-DELINQ-POLCD  
              MOVE "SP-ALLOW-DELINQ-POLCD"  TO D01-TEXT
              MOVE OSP-ALLOW-DELINQ-POLCD   TO D01-A15
              MOVE SP-ALLOW-DELINQ-POLCD    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-YTD-MAX-ALLOW-DELINQ   NOT = OSP-YTD-MAX-ALLOW-DELINQ  
              MOVE "SP-YTD-MAX-ALLOW-DELINQ"  TO D01-TEXT
              MOVE OSP-YTD-MAX-ALLOW-DELINQ   TO D01-A15
              MOVE SP-YTD-MAX-ALLOW-DELINQ    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

      *    PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 15
      *      IF SP-REVOL-IS-FINCHG(SUB) NOT = OSP-REVOL-IS-FINCHG(SUB)
      *         STRING "SP-REVOL-IS-FINCHG(",SUB,")" INTO D01-TEXT 
      *         MOVE OSP-REVOL-IS-FINCHG(SUB) TO D01-A15
      *         MOVE SP-REVOL-IS-FINCHG(SUB)  TO D01-S35
      *         PERFORM WRITE-DETAIL-LINE-ERROR.

      *    PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 15
      *      IF SP-SP-REVOL-APPLY-ORDER(SUB) 
      *            NOT = OSP-SP-REVOL-APPLY-ORDER(SUB)
      *         STRING "SP-SP-REVOL-APPLY-ORDER(",SUB,")" INTO D01-TEXT 
      *         MOVE OSP-SP-REVOL-APPLY-ORDER(SUB) TO D01-A15
      *         MOVE SP-SP-REVOL-APPLY-ORDER(SUB)  TO D01-S35
      *         PERFORM WRITE-DETAIL-LINE-ERROR.
  
      *    IF SP-REVOL-APPLY-PURCHASE   NOT = OSP-REVOL-APPLY-PURCHASE 
      *       MOVE "SP-REVOL-APPLY-PURCHASE"  TO D01-TEXT
      *       MOVE OSP-REVOL-APPLY-PURCHASE   TO D01-A15
      *       MOVE SP-REVOL-APPLY-PURCHASE    TO D01-S35
      *       PERFORM WRITE-DETAIL-LINE-ERROR.
  
      *    IF SP-SP-REVOL-BAL-FRMLA   NOT = OSP-SP-REVOL-BAL-FRMLA  
      *       MOVE "SP-SP-REVOL-BAL-FRMLA"  TO D01-TEXT
      *       MOVE OSP-SP-REVOL-BAL-FRMLA   TO D01-A15
      *       MOVE SP-SP-REVOL-BAL-FRMLA    TO D01-S35
      *       PERFORM WRITE-DETAIL-LINE-ERROR.
  
      *    IF SP-REVOL-CYCLE-DATE-FRMLA   
      *          NOT = OSP-REVOL-CYCLE-DATE-FRMLA  
      *       MOVE "SP-REVOL-CYCLE-DATE-FRMLA"  TO D01-TEXT
      *       MOVE OSP-REVOL-CYCLE-DATE-FRMLA   TO D01-A15
      *       MOVE SP-REVOL-CYCLE-DATE-FRMLA    TO D01-S35
      *       PERFORM WRITE-DETAIL-LINE-ERROR.

      *    PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 31
      *      IF SP-REVOL-CYCLE-DATE-ELE(SUB) 
      *           NOT = OSP-REVOL-CYCLE-DATE-ELE(SUB)
      *         STRING "SP-REVOL-CYCLE-DATE-ELE(",SUB,")" INTO D01-TEXT 
      *         MOVE OSP-REVOL-CYCLE-DATE-ELE(SUB) TO D01-A15
      *         MOVE SP-REVOL-CYCLE-DATE-ELE(SUB)  TO D01-S35
      *         PERFORM WRITE-DETAIL-LINE-ERROR. 
  
      *    IF SP-REVOL-PYMT-DUE-FRMLA   NOT = OSP-REVOL-PYMT-DUE-FRMLA  
      *       MOVE "SP-REVOL-PYMT-DUE-FRMLA"  TO D01-TEXT
      *       MOVE OSP-REVOL-PYMT-DUE-FRMLA   TO D01-A15
      *       MOVE SP-REVOL-PYMT-DUE-FRMLA    TO D01-S35
      *       PERFORM WRITE-DETAIL-LINE-ERROR.
  
      *    IF SP-REVOL-PYMT-DUE-WORK   NOT = OSP-REVOL-PYMT-DUE-WORK  
      *       MOVE "SP-REVOL-PYMT-DUE-WORK"  TO D01-TEXT
      *       MOVE OSP-REVOL-PYMT-DUE-WORK   TO D01-A15
      *       MOVE SP-REVOL-PYMT-DUE-WORK    TO D01-S35
      *       PERFORM WRITE-DETAIL-LINE-ERROR.
  
      *    IF SP-REVOL-PYMT-EXCEPTION   NOT = OSP-REVOL-PYMT-EXCEPTION  
      *       MOVE "SP-REVOL-PYMT-EXCEPTION"  TO D01-TEXT
      *       MOVE OSP-REVOL-PYMT-EXCEPTION   TO D01-A15
      *       MOVE SP-REVOL-PYMT-EXCEPTION    TO D01-S35
      *       PERFORM WRITE-DETAIL-LINE-ERROR.
  
      *    IF SP-REVOL-PYMT-MINIMUM  NOT = OSP-REVOL-PYMT-MINIMUM  
      *       MOVE "SP-REVOL-PYMT-MINIMUM"  TO D01-TEXT
      *       MOVE OSP-REVOL-PYMT-MINIMUM   TO D01-A15-NUM
      *       MOVE SP-REVOL-PYMT-MINIMUM    TO D01-S35-NUM
      *       PERFORM WRITE-DETAIL-LINE-ERROR.
  
      *    IF SP-REVOL-PYMT-MAXIMUM 
      *          NOT = OSP-SP-REVOL-PYMT-MAXIMUM  
      *       MOVE "SP-REVOL-PYMT-MAXIMUM"  TO D01-TEXT
      *       MOVE OSP-REVOL-PYMT-MAXIMUM   TO D01-A15-NUM
      *       MOVE SP-REVOL-PYMT-MAXIMUM    TO D01-S35-NUM
      *       PERFORM WRITE-DETAIL-LINE-ERROR.
  
      *    IF SP-REVOL-PYMT-PERCENT   NOT = OSP-REVOL-PYMT-PERCENT  
      *       MOVE "SP-REVOL-PYMT-PERCENT"  TO D01-TEXT
      *       MOVE OSP-REVOL-PYMT-PERCENT   TO D01-A15-NUM
      *       MOVE SP-REVOL-PYMT-PERCENT    TO D01-S35-NUM
      *       PERFORM WRITE-DETAIL-LINE-ERROR.
  
      *    IF SP-REVOL-PYMT-ROUND-CODE NOT = OSP-REVOL-PYMT-ROUND-CODE  
      *       MOVE "SP-REVOL-PYMT-ROUND-CODE"  TO D01-TEXT
      *       MOVE OSP-REVOL-PYMT-ROUND-CODE   TO D01-A15
      *       MOVE SP-REVOL-PYMT-ROUND-CODE    TO D01-S35
      *       PERFORM WRITE-DETAIL-LINE-ERROR.

      *    PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
      *      IF SP-REVOL-PROGRAM-ELE(SUB) 
      *              NOT = OSP-REVOL-PROGRAM-ELE(SUB)
      *         STRING "SP-REVOL-PROGRAM-ELE(",SUB,")" INTO D01-TEXT 
      *         MOVE OSP-REVOL-PROGRAM-ELE(SUB) TO D01-A15
      *         MOVE SP-REVOL-PROGRAM-ELE(SUB)  TO D01-S35
      *         PERFORM WRITE-DETAIL-LINE-ERROR. 
  
      *    IF SP-REVOL-NO-MTHS-NA-PRINT-STMT 
      *          NOT = OSP-REVOL-NO-MTHS-NA-PRINT-STMT  
      *       MOVE "SP-REVOL-NO-MTHS-NA-PRINT-STMT"  TO D01-TEXT
      *       MOVE OSP-REVOL-NO-MTHS-NA-PRINT-STMT   TO D01-A15
      *       MOVE SP-REVOL-NO-MTHS-NA-PRINT-STMT    TO D01-S35
      *       PERFORM WRITE-DETAIL-LINE-ERROR.
  
      *    IF SP-REVOL-MEMBERSHIP-FEE   NOT = OSP-REVOL-MEMBERSHIP-FEE  
      *       MOVE "SP-REVOL-MEMBERSHIP-FEE"  TO D01-TEXT
      *       MOVE OSP-REVOL-MEMBERSHIP-FEE   TO D01-A15-NUM
      *       MOVE SP-REVOL-MEMBERSHIP-FEE    TO D01-S35-NUM
      *       PERFORM WRITE-DETAIL-LINE-ERROR.
  
      *    IF SP-REVOL-MEMBERSHIP-FRMLA 
      *           NOT = OSP-REVOL-MEMBERSHIP-FRMLA  
      *       MOVE "SP-REVOL-MEMBERSHIP-FRMLA"  TO D01-TEXT
      *       MOVE OSP-REVOL-MEMBERSHIP-FRMLA   TO D01-A15
      *       MOVE SP-REVOL-MEMBERSHIP-FRMLA    TO D01-S35
      *       PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-LNCOST-FRMLA   NOT = OSP-LNCOST-FRMLA  
              MOVE "SP-LNCOST-FRMLA"  TO D01-TEXT
              MOVE OSP-LNCOST-FRMLA   TO D01-A15
              MOVE SP-LNCOST-FRMLA    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-LNCOST-AMT    NOT = OSP-LNCOST-AMT   
              MOVE "SP-LNCOST-AMT "  TO D01-TEXT
              MOVE OSP-LNCOST-AMT    TO D01-A15-NUM
              MOVE SP-LNCOST-AMT     TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-DLDISC-RVPRIN1-PCNT   NOT = OSP-DLDISC-RVPRIN1-PCNT  
              MOVE "SP-DLDISC-RVPRIN1-PCNT"  TO D01-TEXT
              MOVE OSP-DLDISC-RVPRIN1-PCNT   TO D01-A15-NUM
              MOVE SP-DLDISC-RVPRIN1-PCNT    TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
      *    IF SP-REVOL-STMT-FORMAT   NOT = OSP-REVOL-STMT-FORMAT  
      *       MOVE "SP-REVOL-STMT-FORMAT"  TO D01-TEXT
      *       MOVE OSP-REVOL-STMT-FORMAT   TO D01-A15
      *       MOVE SP-REVOL-STMT-FORMAT    TO D01-S35
      *       PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-RENEWABLE-PAYS   NOT = OSP-RENEWABLE-PAYS  
              MOVE "SP-RENEWABLE-PAYS"  TO D01-TEXT
              MOVE OSP-RENEWABLE-PAYS   TO D01-A15
              MOVE SP-RENEWABLE-PAYS    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-RENEWABLE-DAYS    NOT = OSP-RENEWABLE-DAYS   
              MOVE "SP-RENEWABLE-DAYS "  TO D01-TEXT
              MOVE OSP-RENEWABLE-DAYS    TO D01-A15
              MOVE SP-RENEWABLE-DAYS     TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.
  
           IF SP-ALLOW-LN-BACK-DATE   NOT = OSP-ALLOW-LN-BACK-DATE  
              MOVE "SP-ALLOW-LN-BACK-DATE"  TO D01-TEXT
              MOVE OSP-ALLOW-LN-BACK-DATE   TO D01-A15
              MOVE SP-ALLOW-LN-BACK-DATE    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-DLDISC-RVPRIN1-AF-PCNT  
                 NOT = OSP-DLDISC-RVPRIN1-AF-PCNT  
              MOVE "SP-DLDISC-RVPRIN1-AF-PCNT"  TO D01-TEXT
              MOVE OSP-DLDISC-RVPRIN1-AF-PCNT   TO D01-A15-NUM
              MOVE SP-DLDISC-RVPRIN1-AF-PCNT    TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-CAL-MAX-AT-ESTIMATE  NOT = OSP-CAL-MAX-AT-ESTIMATE   
              MOVE "SP-CAL-MAX-AT-ESTIMATE"  TO D01-TEXT
              MOVE OSP-CAL-MAX-AT-ESTIMATE   TO D01-A15
              MOVE SP-CAL-MAX-AT-ESTIMATE    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-INSAH-RETRO  NOT = OSP-INSAH-RETRO  
              MOVE "SP-INSAH-RETRO"  TO D01-TEXT
              MOVE OSP-INSAH-RETRO   TO D01-A15
              MOVE SP-INSAH-RETRO    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-INSAH-DAYS-WAIT  NOT = OSP-INSAH-DAYS-WAIT  
              MOVE "SP-INSAH-DAYS-WAIT"  TO D01-TEXT
              MOVE OSP-INSAH-DAYS-WAIT   TO D01-A15
              MOVE SP-INSAH-DAYS-WAIT    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-ADDON-EXCEPT-FG  NOT = OSP-ADDON-EXCEPT-FG  
              MOVE "SP-ADDON-EXCEPT-FG"  TO D01-TEXT
              MOVE OSP-ADDON-EXCEPT-FG   TO D01-A15
              MOVE SP-ADDON-EXCEPT-FG    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
             IF SP-REPO-TRCD(SUB) NOT = OSP-REPO-TRCD(SUB)
                STRING "SP-REPO-TRCD(",SUB,")" INTO D01-TEXT 
                MOVE OSP-REPO-TRCD(SUB) TO D01-A15
                MOVE SP-REPO-TRCD(SUB)  TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR. 

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 3
             PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 4
                IF SP-REPO-FMNO(SUB,SUB1)
                    NOT = OSP-REPO-FMNO(SUB,SUB1)
                   STRING "SP-REPO-FMNO(",SUB,")-",SUB1
                                        INTO D01-TEXT
                   MOVE OSP-REPO-FMNO(SUB,SUB1) TO D01-A15
                   MOVE SP-REPO-FMNO(SUB,SUB1)  TO D01-S35
                   PERFORM WRITE-DETAIL-LINE-ERROR
                END-IF
             END-PERFORM
           END-PERFORM.

           IF SP-CANCEL-ADDON-FG  NOT = OSP-CANCEL-ADDON-FG  
              MOVE "SP-CANCEL-ADDON-FG"  TO D01-TEXT
              MOVE OSP-CANCEL-ADDON-FG   TO D01-A15
              MOVE SP-CANCEL-ADDON-FG    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

      *    IF SP-REVOL-STMT-EXTRACTION NOT = OSP-REVOL-STMT-EXTRACTION  
      *       MOVE "SP-REVOL-STMT-EXTRACTION"  TO D01-TEXT
      *       MOVE OSP-REVOL-STMT-EXTRACTION   TO D01-A15
      *       MOVE SP-REVOL-STMT-EXTRACTION    TO D01-S35
      *       PERFORM WRITE-DETAIL-LINE-ERROR.

      *      IF SP-REVOL-ZERO-ADB  NOT = OSP-REVOL-ZERO-ADB  
      *       MOVE "SP-REVOL-ZERO-ADB"  TO D01-TEXT
      *       MOVE OSP-REVOL-ZERO-ADB   TO D01-A15
      *       MOVE SP-REVOL-ZERO-ADB    TO D01-S35
      *       PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OSP-DEFAULT-O2-INS-COMP NOT NUMERIC
              MOVE 0                     TO OSP-DEFAULT-O2-INS-COMP.
  
           IF SP-DEFAULT-O2-INS-COMP  NOT = OSP-DEFAULT-O2-INS-COMP  
              MOVE "SP-DEFAULT-O2-INS-COMP"  TO D01-TEXT
              MOVE OSP-DEFAULT-O2-INS-COMP   TO D01-A15
              MOVE SP-DEFAULT-O2-INS-COMP    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-LC-ON-JUDGEMENTS  NOT = OSP-LC-ON-JUDGEMENTS  
              MOVE "SP-LC-ON-JUDGEMENTS"  TO D01-TEXT
              MOVE OSP-LC-ON-JUDGEMENTS   TO D01-A15
              MOVE SP-LC-ON-JUDGEMENTS    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE 0                          TO DELQ-IB-MAX-DAYS-REM
                                              TEST-DELQ-IB-AGE.

      *     IF OSP-DELQ-IB-AGE NOT NUMERIC
      *        MOVE 0 TO OSP-DELQ-IB-AGE.

           MOVE OSP-DELQ-IB-AGE TO TEST-DELQ-IB-AGE.

           IF TEST-DELQ-IB-AGE NOT = 0
              DIVIDE TEST-DELQ-IB-AGE BY 30 GIVING TEST-DELQ-IB-AGE
                                   REMAINDER DELQ-IB-MAX-DAYS-REM
           ELSE
              MOVE 1 TO DELQ-IB-MAX-DAYS-REM.

           IF DELQ-IB-MAX-DAYS-REM NOT = 0
               MOVE 0 TO OSP-DELQ-IB-AGE
               MOVE 0 TO OSP-DELQ-IB-MAX-DAYS.
      *     ELSE
      *         IF OSP-DELQ-IB-MAX-DAYS NOT NUMERIC
      *             MOVE 0 TO OSP-DELQ-IB-MAX-DAYS.

           IF SP-DELQ-IB-AGE  NOT = OSP-DELQ-IB-AGE  
              MOVE "SP-DELQ-IB-AGE"  TO D01-TEXT
              MOVE OSP-DELQ-IB-AGE   TO D01-A15
              MOVE SP-DELQ-IB-AGE    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-DELQ-IB-MAX-DAYS  NOT = OSP-DELQ-IB-MAX-DAYS  
              MOVE "SP-DELQ-IB-MAX-DAYS"  TO D01-TEXT
              MOVE OSP-DELQ-IB-MAX-DAYS   TO D01-A15
              MOVE SP-DELQ-IB-MAX-DAYS    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-INSURANCE-WRITTEN  NOT = OSP-INSURANCE-WRITTEN  
              MOVE "SP-INSURANCE-WRITTEN"  TO D01-TEXT
              MOVE OSP-INSURANCE-WRITTEN   TO D01-A15
              MOVE SP-INSURANCE-WRITTEN    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-INTERIM-COUPON  NOT = OSP-INTERIM-COUPON  
              MOVE "SP-INTERIM-COUPON"  TO D01-TEXT
              MOVE OSP-INTERIM-COUPON   TO D01-A15
              MOVE SP-INTERIM-COUPON    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-AUTO-ASSESS-LCHG  NOT = OSP-AUTO-ASSESS-LCHG  
              MOVE "SP-AUTO-ASSESS-LCHG"  TO D01-TEXT
              MOVE OSP-AUTO-ASSESS-LCHG   TO D01-A15
              MOVE SP-AUTO-ASSESS-LCHG    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF (OSP-MAX-CASH-PROCEEDS NOT NUMERIC) OR
              (OSP-MAX-CASH-PROCEEDS = 202020.20 OR 020202.02)
              MOVE 999999.99              TO OSP-MAX-CASH-PROCEEDS.

           IF SP-MAX-CASH-PROCEEDS  NOT = OSP-MAX-CASH-PROCEEDS  
              MOVE "SP-MAX-CASH-PROCEEDS"  TO D01-TEXT
              MOVE OSP-MAX-CASH-PROCEEDS   TO D01-A15-NUM
              MOVE SP-MAX-CASH-PROCEEDS    TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-ALLOW-ZERO-PROC  NOT = OSP-ALLOW-ZERO-PROC  
              MOVE "SP-ALLOW-ZERO-PROC"  TO D01-TEXT
              MOVE OSP-ALLOW-ZERO-PROC   TO D01-A15
              MOVE SP-ALLOW-ZERO-PROC    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-BANKRULE-RATE  NOT = OSP-BANKRULE-RATE  
              MOVE "SP-BANKRULE-RATE"  TO D01-TEXT
              MOVE OSP-BANKRULE-RATE   TO D01-A15
              MOVE SP-BANKRULE-RATE    TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
             IF SP-AUTO-SEND-TRCD(SUB) NOT = OSP-AUTO-SEND-TRCD(SUB)
                STRING "SP-AUTO-SEND-TRCD(",SUB,")" INTO D01-TEXT 
                MOVE OSP-AUTO-SEND-TRCD(SUB) TO D01-A15
                MOVE SP-AUTO-SEND-TRCD(SUB)  TO D01-S35
                PERFORM WRITE-DETAIL-LINE-ERROR.

           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 2
             PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 4
                IF OSP-AUTO-SEND-FMNO(SUB,SUB1) NOT NUMERIC
                   MOVE 0 TO OSP-AUTO-SEND-FMNO(SUB,SUB1) 
                END-IF
                   IF SP-AUTO-SEND-FMNO(SUB,SUB1) 
                        NOT = OSP-AUTO-SEND-FMNO(SUB,SUB1)
                      STRING "SP-AUTO-SEND-FMNO(",SUB,")-",SUB1
                                           INTO D01-TEXT
                      MOVE OSP-AUTO-SEND-FMNO(SUB,SUB1) TO D01-A15
                      MOVE SP-AUTO-SEND-FMNO(SUB,SUB1)  TO D01-S35
                      PERFORM WRITE-DETAIL-LINE-ERROR
                   END-IF
              END-PERFORM
           END-PERFORM.

      * NEED THIS MOVE BECAUSE THERE ARE A FEW RECORDS IN A15 
      *  OSP-DLDISC-CONV-BF-PCNT > 999.999 EVEN THOUG THE FIELD
      *  IS PIC 999.999 COMP.  EX. A15 = 2105.376 -> S35 = 105.376
      *  JAY SAID TO DISREGARD THE DIFF.
           MOVE OSP-DLDISC-CONV-BF-PCNT TO WS-NUM33.
           MOVE WS-NUM33 TO OSP-DLDISC-CONV-BF-PCNT.
           IF SP-DLDISC-CONV-BF-PCNT  NOT = OSP-DLDISC-CONV-BF-PCNT
              MOVE "SP-DLDISC-CONV-BF-PCNT"  TO D01-TEXT
              MOVE OSP-DLDISC-CONV-BF-PCNT   TO D01-A15-NUM
              MOVE SP-DLDISC-CONV-BF-PCNT    TO D01-S35-NUM
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-UNITPER-CD  NOT = OSP-UNITPER-CD  
            MOVE "SP-UNITPER-CD"  TO D01-TEXT
            MOVE OSP-UNITPER-CD   TO D01-A15
            MOVE SP-UNITPER-CD    TO D01-S35
            PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-RENEWAL-EDIT  NOT = OSP-RENEWAL-EDIT  
            MOVE "SP-RENEWAL-EDIT"  TO D01-TEXT
            MOVE OSP-RENEWAL-EDIT   TO D01-A15
            MOVE SP-RENEWAL-EDIT    TO D01-S35
            PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-ROUND-MFEE  NOT = OSP-ROUND-MFEE  
            MOVE "SP-ROUND-MFEE"  TO D01-TEXT
            MOVE OSP-ROUND-MFEE   TO D01-A15
            MOVE SP-ROUND-MFEE    TO D01-S35
            PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-ADV-TO-1ST-NO-CORR NOT = OSP-ADV-TO-1ST-NO-CORR  
            MOVE "SP-ADV-TO-1ST-NO-CORR"  TO D01-TEXT
            MOVE OSP-ADV-TO-1ST-NO-CORR   TO D01-A15
            MOVE SP-ADV-TO-1ST-NO-CORR    TO D01-S35
            PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-LC-INT-ON-REPO-NON-PL  
               NOT = OSP-LC-INT-ON-REPO-NON-PL  
            MOVE "SP-LC-INT-ON-REPO-NON-PL"  TO D01-TEXT
            MOVE OSP-LC-INT-ON-REPO-NON-PL   TO D01-A15
            MOVE SP-LC-INT-ON-REPO-NON-PL    TO D01-S35
            PERFORM WRITE-DETAIL-LINE-ERROR.

           MOVE 0   TO TEST-DIVIDE-BY-30
                       TEST-DIVIDE-BY-30-REM.

           IF (OSP-CONTRACTUAL-SET-NONACCRUAL NOT NUMERIC) OR
                 (OSP-CONTRACTUAL-SET-NONACCRUAL = 0)
              MOVE 0 TO OSP-CONTRACTUAL-SET-NONACCRUAL
           ELSE
              DIVIDE OSP-CONTRACTUAL-SET-NONACCRUAL BY 30
                  GIVING TEST-DIVIDE-BY-30,
                        REMAINDER TEST-DIVIDE-BY-30-REM
                  IF TEST-DIVIDE-BY-30-REM NOT = 0
                     MOVE 0 TO OSP-CONTRACTUAL-SET-NONACCRUAL.

           IF SP-CONTRACTUAL-SET-NONACCRUAL  
               NOT = OSP-CONTRACTUAL-SET-NONACCRUAL  
            MOVE "SP-CONTRACTUAL-SET-NONACCRUAL"  TO D01-TEXT
            MOVE OSP-CONTRACTUAL-SET-NONACCRUAL   TO D01-A15
            MOVE SP-CONTRACTUAL-SET-NONACCRUAL    TO D01-S35
            PERFORM WRITE-DETAIL-LINE-ERROR.

            MOVE 0   TO TEST-DIVIDE-BY-30
                        TEST-DIVIDE-BY-30-REM.

           IF (OSP-CONTRACTUAL-RESET-ACCRUAL NOT NUMERIC) OR
                 (OSP-CONTRACTUAL-RESET-ACCRUAL = 0)
              MOVE 0 TO OSP-CONTRACTUAL-RESET-ACCRUAL
           ELSE
              DIVIDE OSP-CONTRACTUAL-RESET-ACCRUAL BY 30
                                   GIVING TEST-DIVIDE-BY-30
                           REMAINDER TEST-DIVIDE-BY-30-REM
              IF TEST-DIVIDE-BY-30-REM NOT = 0
                  MOVE 0 TO OSP-CONTRACTUAL-RESET-ACCRUAL.

           IF SP-CONTRACTUAL-RESET-ACCRUAL
               NOT = OSP-CONTRACTUAL-RESET-ACCRUAL  
            MOVE "SP-CONTRACTUAL-RESET-ACCRUAL"  TO D01-TEXT
            MOVE OSP-CONTRACTUAL-RESET-ACCRUAL   TO D01-A15
            MOVE SP-CONTRACTUAL-RESET-ACCRUAL    TO D01-S35
            PERFORM WRITE-DETAIL-LINE-ERROR.

           IF (OSP-MONTHS-PAY-NAPP-COUNTER = 20 OR 02) OR
               (OSP-MONTHS-PAY-NAPP-COUNTER NOT NUMERIC)
              MOVE 0 TO OSP-MONTHS-PAY-NAPP-COUNTER. 

           IF SP-MONTHS-PAY-NAPP-COUNTER  
               NOT = OSP-MONTHS-PAY-NAPP-COUNTER  
            MOVE "SP-MONTHS-PAY-NAPP-COUNTER"  TO D01-TEXT
            MOVE OSP-MONTHS-PAY-NAPP-COUNTER   TO D01-A15
            MOVE SP-MONTHS-PAY-NAPP-COUNTER    TO D01-S35
            PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OSP-SHORT-PAY-THRESHOLD NOT NUMERIC
              MOVE 0 TO OSP-SHORT-PAY-THRESHOLD. 

           IF SP-SHORT-PAY-THRESHOLD  NOT = OSP-SHORT-PAY-THRESHOLD  
            MOVE "SP-SHORT-PAY-THRESHOLD"  TO D01-TEXT
            MOVE OSP-SHORT-PAY-THRESHOLD   TO D01-A15
            MOVE SP-SHORT-PAY-THRESHOLD    TO D01-S35
            PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OSP-CASH-AVAIL-PERCENT NOT NUMERIC
              MOVE 0 TO OSP-CASH-AVAIL-PERCENT 

           IF SP-CASH-AVAIL-PERCENT  NOT = OSP-CASH-AVAIL-PERCENT  
            MOVE "SP-CASH-AVAIL-PERCENT"  TO D01-TEXT
            MOVE OSP-CASH-AVAIL-PERCENT   TO D01-A15
            MOVE SP-CASH-AVAIL-PERCENT    TO D01-S35
            PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-O1-JOINT-FG  NOT = OSP-O1-JOINT-FG  
            MOVE "SP-O1-JOINT-FG"  TO D01-TEXT
            MOVE OSP-O1-JOINT-FG   TO D01-A15
            MOVE SP-O1-JOINT-FG    TO D01-S35
            PERFORM WRITE-DETAIL-LINE-ERROR.

           IF (OSP-NET-BAL-REQ = 202 OR 020) OR
                   OSP-NET-BAL-REQ NOT NUMERIC
              MOVE 0               TO OSP-NET-BAL-REQ.

           IF SP-NET-BAL-REQ  NOT = OSP-NET-BAL-REQ  
            MOVE "SP-NET-BAL-REQ"  TO D01-TEXT
            MOVE OSP-NET-BAL-REQ   TO D01-A15
            MOVE SP-NET-BAL-REQ    TO D01-S35
            PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OSP-AMT-PROCEEDS-REQ NOT NUMERIC
              MOVE 0                      TO OSP-AMT-PROCEEDS-REQ.

           IF SP-AMT-PROCEEDS-REQ  NOT = OSP-AMT-PROCEEDS-REQ  
            MOVE "SP-AMT-PROCEEDS-REQ"  TO D01-TEXT
            MOVE OSP-AMT-PROCEEDS-REQ   TO D01-A15
            MOVE SP-AMT-PROCEEDS-REQ    TO D01-S35
            PERFORM WRITE-DETAIL-LINE-ERROR.

           IF OSP-ALT-PREPAY-MONTHS NOT NUMERIC
              MOVE 0                      TO OSP-ALT-PREPAY-MONTHS.

           IF SP-ALT-PREPAY-MONTHS  NOT = OSP-ALT-PREPAY-MONTHS  
            MOVE "SP-ALT-PREPAY-MONTHS"  TO D01-TEXT
            MOVE OSP-ALT-PREPAY-MONTHS   TO D01-A15
            MOVE SP-ALT-PREPAY-MONTHS    TO D01-S35
            PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-PERCENT-REGPAY-AVAIL NOT = OSP-PERCENT-REGPAY-AVAIL  
            MOVE "SP-PERCENT-REGPAY-AVAIL"  TO D01-TEXT
            MOVE OSP-PERCENT-REGPAY-AVAIL   TO D01-A15
            MOVE SP-PERCENT-REGPAY-AVAIL    TO D01-S35
            PERFORM WRITE-DETAIL-LINE-ERROR.

      *  PROGRAM UP/UP1534.CBL MOVES 0'S TO THE NEXT 3 FIELDS

      *    IF SP-ALT-DEFFRMLA  NOT = OSP-ALT-DEFFRMLA  
      *     MOVE "SP-ALT-DEFFRMLA"  TO D01-TEXT
      *     MOVE OSP-ALT-DEFFRMLA   TO D01-A15
      *     MOVE SP-ALT-DEFFRMLA    TO D01-S35
      *     PERFORM WRITE-DETAIL-LINE-ERROR.

      *    IF SP-ALT-DEFMIN  NOT = OSP-ALT-DEFMIN  
      *     MOVE "SP-ALT-DEFMIN"  TO D01-TEXT
      *     MOVE OSP-ALT-DEFMIN   TO D01-A15-NUM
      *     MOVE SP-ALT-DEFMIN    TO D01-S35-NUM
      *     PERFORM WRITE-DETAIL-LINE-ERROR.

      *    IF SP-ALT-DEFMAX  NOT = OSP-ALT-DEFMAX  
      *     MOVE "SP-ALT-DEFMAX"  TO D01-TEXT
      *     MOVE OSP-ALT-DEFMAX   TO D01-A15-NUM
      *     MOVE SP-ALT-DEFMAX    TO D01-S35-NUM
      *     PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-NON-LOAN-FLAG  NOT = OSP-NON-LOAN-FLAG  
            MOVE "SP-NON-LOAN-FLAG"  TO D01-TEXT
            MOVE OSP-NON-LOAN-FLAG   TO D01-A15
            MOVE SP-NON-LOAN-FLAG    TO D01-S35
            PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SP-SKIP-TRW  NOT = OSP-SKIP-TRW  
            MOVE "SP-SKIP-TRW"  TO D01-TEXT
            MOVE OSP-SKIP-TRW   TO D01-A15
            MOVE SP-SKIP-TRW    TO D01-S35
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

           MOVE "A15 RECORDS PROCESSED         :"     TO R01-PROMPT.
           MOVE A15-RECORDS-PROCESSED                 TO R01-NUMERIC.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "A15 DUP RECORDS BYPASSED      :"     TO R01-PROMPT.
           MOVE OSP-DUPS-CTR                          TO R01-NUMERIC.
           MOVE 01                                    TO LN-FEED.
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
           MOVE EXT-CONAME-CONAME  TO SPFILE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO SPFILE-SYSDATE.
           DISPLAY SPFILE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE SPFILE-FORM-FIELDS TO WR-BUF.
           MOVE SPFILE-FORM-LIST   TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE SPFILE-HELP--FILE TO HELP-LINK-FILE.
           MOVE SPFILE-HELP--DIR  TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/SPFILE_EVA.CPY".
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
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPSP1RN.CPY".

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
       OPEN-OSP1-FILE.
           PERFORM OPEN-IT.
           MOVE OSP-PATH TO E-FILE.
           OPEN INPUT OSP-FILE.
           IF IO-FG = 8
              GO TO OPEN-OSP1-FILE.
           IF IO-FG = 7
              CLOSE OSP-FILE
              GO TO OPEN-OSP1-FILE.

       READ-OSP1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OSP-PATH TO E-FILE.
           READ OSP-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OSP1-FILE-NEXT.

       CLOSE-OSP1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OSP-FILE.

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
                MOVE 9 TO IO-STATUS IO-FG
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
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

           MOVE WK-PATH TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY SPFILE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
