       IDENTIFICATION DIVISION.
       PROGRAM-ID.    SMFILE.
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
      * 02-13-2025 MB  - CHANGED TO USE 19500101 AND 20491231 FOR BATCH
      *                   REQUEST BEG/END DATES
      * 
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       COPY "LIBGB/GBFSPR.CPY".

      * COPYMEMBER: LIBLP/LPFSSM
           SELECT OSM-FILE ASSIGN TO OSM-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY OSM1-KEY
                  FILE STATUS FILE-STAT.

           SELECT WK2-FILE ASSIGN WK2-PATH
                  ORGANIZATION INDEXED
                  ACCESS DYNAMIC
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  RECORD KEY WK2-KEY
                  FILE STATUS FILE-STAT. 

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

       FD  OSM-FILE
           LABEL RECORDS ARE STANDARD.
      *-----------------------------------------------------------------
       COPY "LIBUP/A15SRC/LP01SM.CPY" REPLACING LEADING "SM" BY "OSM".


      ******************************************************************
      *    FOR READING / WRITING WORK SMFILE RECORDS
      ******************************************************************
       FD  WK2-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK2-REC.
           03  WK2-KEY.
               05  WK2-SM-KEY.
                   07 WK2-ORGST           PIC X(02).
                   07 WK2-SPRCLASS        PIC 9(02).
                   07 WK2-SUBCLASS        PIC 9(02).
                   07 WK2-LAWCODE         PIC 9(02).
               05  WK2-DATE               PIC 9(8).
               05  FILLER REDEFINES WK2-DATE.
                   07  WK2-CC             PIC 99.
                   07  WK2-YY             PIC 99.
                   07  WK2-MM             PIC 99.
                   07  WK2-DD             PIC 99.
               05  WK2-SEQ                PIC 99.
           03  WK2-MEMO.
               05  WK2-MEMO1              PIC X(30).
               05  WK2-MEMO2              PIC X(30).
           03  WK2-MEMO-TYPE              PIC X.
           03  FILLER                     PIC X(1).

      ******************************************************************
      *
      *     W O R K I N G - S T O R A G E   S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)UT/SMFILE.CBL S35 01/02/25-14:52:25 >".

      *COPY "LIBGB/GB01BR.CPY".
      *COPY "LIBGB/GB01BR_SQL.CPY".
      *COPY "LIBGB/GBWSBR.CPY".
      *COPY "LIBGB/GB01GR.CPY".
      *COPY "LIBGB/GB01GR_SQL.CPY".
      *COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01SM.CPY".
       COPY "LIBLP/LP01SM_SQL.CPY".
       COPY "LIBLP/LPWSSM.CPY".

      *********************************************************
      *        LIBLP/LPWSSM: SMFILE PATH WORKERS
      *********************************************************
   
       01  OSM-PATH                  PIC X(35) VALUE SPACES.
       01  OSM-ALL-PATH.
          03  OSM-ALL-BASE           PIC X(09).
          03  FILLER                 REDEFINES OSM-ALL-BASE.
              05  OSM-ALL-BASE-USR   PIC X(05).
              05  OSM-ALL-BASE-DIR   PIC X(04).
          03  FILLER                 PIC X(01)  VALUE "/".
          03  OSM-ALL-MACHINE        PIC X(02).
          03  FILLER                 PIC X(23)  VALUE "/LP/SMFILE".

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

       01  WK2-IFN                   PIC X(3)     VALUE "WK2".
       01  WK2-PATH                  PIC X(35)    VALUE SPACES.

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

       01  COMPARE-DATE              PIC X(8).

       01  SQL-ROW-KEY.
           03  SQL-ROW-NAME           PIC X(07).    
           03  SQL-ROW-DATE           PIC X(08).
           03  SQL-ROW-SEQ            PIC 9(08).

       01  PROC-YYMM                   PIC 9(4).
       01  PROC-YYMM-RDF  REDEFINES PROC-YYMM.
           03 PROC-YYMM-YY             PIC 99.
           03 PROC-YYMM-MM             PIC 99.

       01  SM-EOF-SW                   PIC X      VALUE "N".
       01  WK2-EOF-SW                  PIC X      VALUE "N".
       01  TOTAL-ERRORS-CTR            PIC 9(9)   VALUE 0.

        01  WORK-TIME.
           03  WORK-HR                 PIC 9(02).
           03  WORK-MIN                PIC 9(02).
           03  WORK-SEC                PIC 9(02).

       01  CONV-TYPE-CHAR              PIC X.
       01  CONV-TYPE-NUM    REDEFINES CONV-TYPE-CHAR
                                       PIC 9.

       01  WS-OSM1-KEY.
           03  WS-OSM-ORGST            PIC XX.
           03  WS-OSM-SPRCLASS         PIC 999.
           03  WS-OSM-SUBCLASS         PIC 99.
           03  WS-OSM-LAWCODE          PIC 99.
           03  WS-OSM-DATE-REVERSED    PIC 9(8).
           03  WS-OSM-SEQ              PIC 999.

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
       01  SUB1                        PIC 9(04)     VALUE ZEROES.

       01  SYSDATE-YYYYMMDD            PIC 9(8)     VALUE ZEROES.

       01  LEGEND                      PIC X(79)     VALUE SPACES.
       01  LEGEND-LIST                 PIC X(07)     VALUE "LEGEND.".

       01  LEGEND-BEG-ORGST.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F6-SCAN, F7-".
           03  FILLER PIC X(30) VALUE "EXIT, ENTER BEGINNING SM-SP-OR".
           03  FILLER PIC X(19) VALUE "GST                ".

       01  LEGEND-END-ORGST.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER ENDIN".
           03  FILLER PIC X(30) VALUE "G SM-SP-ORGST                 ".
           03  FILLER PIC X(19) VALUE "                   ".

       01  LEGEND-BEG-SPRCLASS.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F6-SCAN, F7-".
           03  FILLER PIC X(30) VALUE "EXIT, ENTER BEGINNING SM-SPRCL".
           03  FILLER PIC X(19) VALUE "ASS                ".

       01  LEGEND-END-SPRCLASS.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER ENDIN".
           03  FILLER PIC X(30) VALUE "G SM-SP-SPRCLASS              ".
           03  FILLER PIC X(19) VALUE "                   ".

       01  LEGEND-BEG-SUBCLASS.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F6-SCAN, F7-".
           03  FILLER PIC X(30) VALUE "EXIT, ENTER BEGINNING SM-SUBCL".
           03  FILLER PIC X(19) VALUE "ASS                ".

       01  LEGEND-END-SUBCLASS.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER ENDIN".
           03  FILLER PIC X(30) VALUE "G SM-SP-SUBCLASS              ".
           03  FILLER PIC X(19) VALUE "                   ".

       01  LEGEND-BEG-LAWCODE.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F6-SCAN, F7-".
           03  FILLER PIC X(30) VALUE "EXIT, ENTER BEGINNING SM-SP-LA".
           03  FILLER PIC X(19) VALUE "WCODE              ".

       01  LEGEND-END-LAWCODE.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER ENDIN".
           03  FILLER PIC X(30) VALUE "G SM-SP-LAWCODE               ".
           03  FILLER PIC X(19) VALUE "                   ".

       01  LEGEND-BEG-DATE.   
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F6-SCAN, F7-".
           03  FILLER PIC X(30) VALUE "EXIT, ENTER BEGINNING SM-DATE-".
           03  FILLER PIC X(19) VALUE "REVERSED           ".

       01  LEGEND-END-DATE.   
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER ENDIN".
           03  FILLER PIC X(30) VALUE "G SM-DATE-REVERSED            ".
           03  FILLER PIC X(19) VALUE "                   ".

       01  LEGEND-BEG-SEQ.
           03  FILLER PIC X(30) VALUE "F1-RESET, F3-ALL, F6-SCAN, F7-".
           03  FILLER PIC X(30) VALUE "EXIT, ENTER BEGINNING SM-SEQ  ".
           03  FILLER PIC X(19) VALUE "                   ".

       01  LEGEND-END-SEQ.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT, ENTER ENDIN".
           03  FILLER PIC X(30) VALUE "G SM-SEQ                      ".
           03  FILLER PIC X(19) VALUE "                   ".

      *-----------------------------------------------------------------
       01  LEGEND-ACCEPT-YN.
           03  FILLER PIC X(30) VALUE "F1-RESET, F7-EXIT             ".
           03  FILLER PIC X(30) VALUE "                              ".
           03  FILLER PIC X(19) VALUE "     ACCEPT (Y/N): ".

       01  SOURCE-PATH-LIST            PIC X(07)     VALUE "S-PATH.".

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  SOURCE-PATH             PIC X(09).
           03  BEG-ORGST               PIC X(02).
           03  END-ORGST               PIC X(02).
           03  BEG-SPRCLASS            PIC 9(03).
           03  END-SPRCLASS            PIC 9(03).
           03  BEG-SUBCLASS            PIC 9(02).
           03  END-SUBCLASS            PIC 9(02).
           03  BEG-LAWCODE             PIC 9(02).
           03  END-LAWCODE             PIC 9(02).
           03  BEG-DATE                PIC 9(08).
           03  END-DATE                PIC 9(08).
           03  BEG-SEQ                 PIC 9(03).
           03  END-SEQ                 PIC 9(03).

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
           "STATE PARAMETER MEMO (SMFILE) COMPARE A15 TO S35".

       01  HEADER-LINE-03.
           03  FILLER                PIC X(20)  VALUE "SP-KEY        ".
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
           03  D01-KEY                        PIC X(20).
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
       COPY "LIBUT/SMFILE_DEF.CPY".
       COPY "LIBUT/SMFILE_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBUT/SMFILE_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
                 PR-FILE OSM-FILE WK2-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SM1-FILE.
           PERFORM CLOSE-PR-FILE.
           PERFORM CLOSE-WK2-FILE.
           CLOSE PR-FILE OSM-FILE WK2-FILE.
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

           PERFORM LOAD-WK2-FILE.

           PERFORM PROCESS-SM-DRIVER.

           PERFORM RANGE-LINES.

       MAIN-PROGRAM-EXIT.
           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************

           MOVE STAT---GOOD                TO PROCESS-STAT.

           MOVE FORM-PATHNAME              TO FORM-PATH.
           MOVE "SMFILE" TO VDU-FORM-NAME.

           PERFORM GET-GPENV.

           MOVE SMFILE-QUEUE-POS  TO Q-POS.
           MOVE SMFILE-COPIES-POS TO C-POS.
           MOVE SMFILE-PRU-POS    TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           PERFORM GET-TIME.
           MOVE TIME-EDIT                  TO STARTING-TIME.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS......" TO LEGEND
              PERFORM SEND-LEGEND.


           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WK2-PATH.
           PERFORM OPEN-WK2-FILE-OUTPUT.
           PERFORM CLOSE-WK2-FILE.
           PERFORM OPEN-WK2-FILE.

           PERFORM OPEN-PR-FILE.

           PERFORM START-MISC-CSR.
           PERFORM READ-MISC-CSR-NEXT.
           PERFORM CLOSE-MISC-CSR.

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
              GO TO ENTER-BEG-ORGST.
  
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
       ENTER-BEG-ORGST.

           MOVE LEGEND-BEG-ORGST  TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A020BEG-ORGST." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F3-KEY ) GO TO ENTER-ALL-ORGST.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-ORGST.

           MOVE VDUBUF TO BEG-ORGST.

      *-----------------------------------------------------------------
       ENTER-END-ORGST.

           MOVE LEGEND-END-ORGST TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "E  A020END-ORGST." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-ORGST.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-ORGST.

           MOVE VDUBUF TO END-ORGST.

           IF ( BEG-ORGST > END-ORGST )
              MOVE "INVALID ORGST RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-ORGST.    

           GO TO ENTER-ALL-SPRCLASS.

      *-----------------------------------------------------------------
       ENTER-ALL-ORGST.

           MOVE "S  A020BEG-ORGST."   TO VDU.
           MOVE SPACES                TO BEG-ORGST  VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A020END-ORGST."   TO VDU.
           MOVE "ZZ"                  TO END-ORGST  VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       ENTER-BEG-SPRCLASS.

           MOVE LEGEND-BEG-SPRCLASS  TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN030BEG-SPRCLASS." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F3-KEY ) GO TO ENTER-ALL-SPRCLASS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-SPRCLASS.

           MOVE VDUNUM0 TO BEG-SPRCLASS.

      *-----------------------------------------------------------------
       ENTER-END-SPRCLASS.

           MOVE LEGEND-END-SPRCLASS TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN030END-SPRCLASS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-SPRCLASS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-SPRCLASS.

           MOVE VDUNUM0 TO END-SPRCLASS.

           IF ( BEG-SPRCLASS > END-SPRCLASS )
              MOVE "INVALID SPRCLASS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-SPRCLASS.    

      *-----------------------------------------------------------------
       ENTER-ALL-SPRCLASS.

           MOVE "SN N030BEG-SPRCLASS."   TO VDU.
           MOVE ZEROES                   TO BEG-SPRCLASS  VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N030END-SPRCLASS."   TO VDU.
           MOVE 999                      TO END-SPRCLASS  VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       ENTER-BEG-SUBCLASS.

           MOVE LEGEND-BEG-SUBCLASS  TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN020BEG-SUBCLASS." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F3-KEY ) GO TO ENTER-ALL-SUBCLASS.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-SUBCLASS.

           MOVE VDUNUM0 TO BEG-SUBCLASS.

      *-----------------------------------------------------------------
       ENTER-END-SUBCLASS.

           MOVE LEGEND-END-SUBCLASS TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN020END-SUBCLASS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-SUBCLASS.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-SUBCLASS.

           MOVE VDUNUM0 TO END-SUBCLASS.

           IF ( BEG-SUBCLASS > END-SUBCLASS )
              MOVE "INVALID SUBCLASS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-SUBCLASS.    

      *-----------------------------------------------------------------
       ENTER-ALL-SUBCLASS.

           MOVE "SN N020BEG-SUBCLASS."   TO VDU.
           MOVE ZEROES                   TO BEG-SUBCLASS  VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N020END-SUBCLASS."   TO VDU.
           MOVE 99                       TO END-SUBCLASS  VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       ENTER-BEG-LAWCODE.

           MOVE LEGEND-BEG-LAWCODE  TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN020BEG-LAWCODE." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F3-KEY ) GO TO ENTER-ALL-LAWCODE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-LAWCODE.

           MOVE VDUNUM0 TO BEG-LAWCODE.

      *-----------------------------------------------------------------
       ENTER-END-LAWCODE.

           MOVE LEGEND-END-LAWCODE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN020END-LAWCODE." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-LAWCODE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-LAWCODE.

           MOVE VDUNUM0 TO END-LAWCODE.

           IF ( BEG-LAWCODE > END-LAWCODE )
              MOVE "INVALID LAWCODE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-LAWCODE.    

      *-----------------------------------------------------------------
       ENTER-ALL-LAWCODE.

           MOVE "SN N020BEG-LAWCODE." TO VDU.
           MOVE ZEROES                TO BEG-LAWCODE  VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N020END-LAWCODE." TO VDU.
           MOVE 99                    TO END-LAWCODE  VDUNUM0.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       ENTER-BEG-DATE.

           IF ( EXT-ROUTE-BUF = "40" OR "50" )
              GO TO ENTER-BEG-SEQ.

           MOVE LEGEND-BEG-DATE  TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN080BEG-DATE." TO VDU.
           PERFORM SCREENIO.

           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F3-KEY ) GO TO ENTER-ALL-DATE.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-DATE.

           MOVE VDUNUM0 TO BEG-DATE.

      *-----------------------------------------------------------------
       ENTER-END-DATE.

           IF ( UP-KEY ) AND ( EXT-ROUTE-BUF = "40" OR "50" )
              GO TO ENTER-END-LAWCODE.

           MOVE LEGEND-END-DATE TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN080END-DATE." TO VDU.
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

           MOVE "S RA000BEG-DATE." TO VDU.
           MOVE "19000101"         TO BEG-DATE  VDUBUF.
           PERFORM SCREENIO.

           MOVE "S RA000END-DATE." TO VDU.
           MOVE "20491231"         TO END-DATE  VDUBUF.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       ENTER-BEG-SEQ.

           MOVE LEGEND-BEG-SEQ  TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN020BEG-SEQ." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-END-DATE.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F3-KEY ) GO TO ENTER-ALL-SEQ.  
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-SEQ.  

           MOVE VDUNUM0 TO BEG-SEQ.

      *-----------------------------------------------------------------
       ENTER-END-SEQ.

           MOVE LEGEND-END-SEQ TO LEGEND.
           PERFORM SEND-LEGEND.

           MOVE "ENNN020END-SEQ." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-SEQ.
           IF (        F1-KEY ) GO TO ENTER-PARAMETERS-END.
           IF (        F7-KEY ) GO TO ENTER-PARAMETERS-END.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-SEQ.

           MOVE VDUNUM0 TO END-SEQ.

           IF ( BEG-SEQ > END-SEQ )
              MOVE "INVALID SEQ RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-SEQ.    

      *-----------------------------------------------------------------
       ENTER-ALL-SEQ.

           MOVE "SNNN020BEG-SEQ."  TO VDU.
           MOVE ZEROES             TO BEG-SEQ  VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020END-SEQ."  TO VDU.
           MOVE 999                TO END-SEQ  VDUNUM0.
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
           MOVE 19500101 TO BEG-DATE.
           MOVE 20491231 TO END-DATE.

      ****************************************
       DISPLAY-PARAMETERS SECTION.
      ****************************************

           MOVE "S  A090S-PATH."        TO VDU.     
           MOVE SOURCE-PATH             TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A020BEG-ORGST."     TO VDU.
           MOVE BEG-ORGST               TO VDUBUF.  
           PERFORM SCREENIO.
           MOVE "S  A020END-ORGST."     TO VDU.
           MOVE END-ORGST               TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "SN N030BEG-SPRCLASS."  TO VDU.
           MOVE BEG-SPRCLASS            TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N030END-SPRCLASS."  TO VDU.
           MOVE END-SPRCLASS            TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N020BEG-SUBCLASS."  TO VDU.
           MOVE BEG-SUBCLASS            TO VDUNUM0. 
           PERFORM SCREENIO.
           MOVE "SN N020END-SUBCLASS."  TO VDU.
           MOVE END-SUBCLASS            TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N020BEG-LAWCODE."   TO VDU.
           MOVE BEG-LAWCODE             TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SN N020END-LAWCODE."   TO VDU.
           MOVE END-LAWCODE             TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SN N020BEG-SEQ."       TO VDU.
           MOVE BEG-SEQ                 TO VDUNUM0. 
           PERFORM SCREENIO.
           MOVE "SN N020END-SEQ."       TO VDU.
           MOVE END-SEQ                 TO VDUNUM0.
           PERFORM SCREENIO.

       DISPLAY-PARAMETERS-EXIT.
           EXIT.

      *********************************************************
       LOAD-WK2-FILE SECTION.
      *********************************************************

           MOVE "PROCESSING A15 DATA" TO BTRACK-MESS.
           PERFORM BTRACK-GLOBAL.

           MOVE SOURCE-PATH           TO OSM-ALL-BASE.
           MOVE MACHINE-R1            TO OSM-ALL-MACHINE.
           MOVE OSM-ALL-PATH          TO OSM-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE "MISSING R1 A15 FILE" TO D01-TEXT 
              MOVE OSM-ALL-PATH          TO D01-A15
              MOVE SPACES                TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO LOAD-R2-PROCESS.

           PERFORM OPEN-OSM1-FILE.

       CONTINUE-R1-PROCESS.

           PERFORM READ-OSM1-FILE-NEXT.
           IF IO-BAD
              PERFORM CLOSE-OSM1-FILE
              GO TO LOAD-R2-PROCESS.

           MOVE OSM-REC TO WK2-REC.
           PERFORM WRITE-WK2-FILE *> EMBEDDED - WK2 WORK FILE
 
           IF IO-FG NOT = 0
              GO TO IO-ERROR
             ELSE
              GO TO CONTINUE-R1-PROCESS.   

      ************************************************************
      * PROCESS SMFILE IN R2 DIRECTORY - BYPASS DUPLICATE RECORDS.
      ************************************************************

       LOAD-R2-PROCESS.   

           MOVE SOURCE-PATH           TO OSM-ALL-BASE.
           MOVE MACHINE-R2            TO OSM-ALL-MACHINE.
           MOVE OSM-ALL-PATH          TO OSM-PATH ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF ( STAT-BAD )
              MOVE "MISSING R2 A15 FILE" TO D01-TEXT 
              MOVE OSM-ALL-PATH          TO D01-A15
              MOVE SPACES                TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR
              GO TO EXIT-LOAD-WK2-FILE.

           PERFORM OPEN-OSM1-FILE.

       CONTINUE-R2-PROCESS.

           PERFORM READ-OSM1-FILE-NEXT.
           IF IO-BAD
              PERFORM CLOSE-OSM1-FILE
              PERFORM CLOSE-WK2-FILE
              PERFORM OPEN-WK2-FILE
              GO TO EXIT-LOAD-WK2-FILE.

           MOVE OSM-REC TO WK2-REC.
           PERFORM WRITE-WK2-FILE *> EMBEDDED - WK2 WORK FILE
 
           IF IO-FG = 22
              GO TO CONTINUE-R2-PROCESS. 
 
           IF IO-FG NOT = 0 
              GO TO IO-ERROR
           ELSE
              GO TO CONTINUE-R2-PROCESS.   

       EXIT-LOAD-WK2-FILE.
           EXIT. 

      *********************************************************
       PROCESS-SM-DRIVER SECTION.
      *********************************************************

           MOVE 9999 TO PROGID-SLEEP-COUNTER.
      *    PERFORM SEND-PROGRESS-INDICATOR.

      *  OPEN/READ S35 - SMFILE

      *    MOVE EXT-FILPATH-BASE  TO FILPATH-BASE.

           MOVE BEG-ORGST            TO QSM1-WBEG-ORGST
           MOVE END-ORGST            TO QSM1-WEND-ORGST
           MOVE BEG-SPRCLASS         TO QSM1-WBEG-SPRCLASS
           MOVE END-SPRCLASS         TO QSM1-WEND-SPRCLASS
           MOVE BEG-SUBCLASS         TO QSM1-WBEG-SUBCLASS
           MOVE END-SUBCLASS         TO QSM1-WEND-SUBCLASS
           MOVE BEG-LAWCODE          TO QSM1-WBEG-LAWCODE
           MOVE END-LAWCODE          TO QSM1-WEND-LAWCODE.

           IF BEG-DATE = 19500101
              MOVE ALL "0"           TO QSM1-WBEG-DATE-REVERSED
           ELSE
              SUBTRACT BEG-DATE FROM 99999999 GIVING
                                     QSM1-WBEG-DATE-REVERSED.

           IF END-DATE = 20491231
              MOVE ALL "9"           TO QSM1-WEND-DATE-REVERSED
           ELSE
              SUBTRACT END-DATE FROM 99999999 GIVING
                                     QSM1-WEND-DATE-REVERSED.

           MOVE BEG-SEQ              TO QSM1-WBEG-SEQ
           MOVE END-SEQ              TO QSM1-WEND-SEQ.

           MOVE "PROCESSING S35 DATA" TO BTRACK-MESS. 
           PERFORM BTRACK-GLOBAL.
 
           MOVE "N"  TO SM-EOF-SW.
           PERFORM OPEN-SM1-FILE.
           PERFORM START-SM1-FILE.  
           PERFORM PROCESS-READ-SM1. 

      * OPEN/READ A15 - WK2-FILE (WORK SMFILE COMINED R1 & R2)          
      * THIS IS A GLOBAL FILE BUT FOLLOWING SAME LOGIC.
      * ONLY PERFORMS THIS SECTION ONCE

           PERFORM OPEN-WK2-FILE.
           PERFORM PROCESS-READ-WK2.

       CONTINUE-SM-DRIVER.

           IF SM-EOF-SW  = "Y" AND
              WK2-EOF-SW = "Y"
              GO TO PROCESS-SM-DRIVER-END.

           IF WS-OSM1-KEY < SM1-KEY 
              MOVE "MISSING S35 RECORD" TO D01-TEXT
              MOVE WS-OSM1-KEY          TO D01-KEY
              MOVE WS-OSM1-KEY          TO D01-A15
              MOVE SM1-KEY              TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR
              PERFORM PROCESS-READ-WK2  
              GO TO CONTINUE-SM-DRIVER.
 
           IF SM1-KEY < WS-OSM1-KEY 
              MOVE "MISSING A15 RECORD" TO D01-TEXT
              MOVE SM1-KEY              TO D01-KEY
              MOVE WS-OSM1-KEY          TO D01-A15
              MOVE SM1-KEY              TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR 
              PERFORM PROCESS-READ-SM1 
              GO TO CONTINUE-SM-DRIVER.

            PERFORM COMPARE-FIELDS.

            PERFORM PROCESS-READ-WK2.
            PERFORM PROCESS-READ-SM1.

            GO TO CONTINUE-SM-DRIVER.

       PROCESS-SM-DRIVER-END.
           EXIT. 

       PROCESS-READ-SM1 SECTION.
 
       PROCESS-BRANCH-NEXT-SM. 

           PERFORM READ-SM1-FILE-NEXT.
           IF IO-BAD
              MOVE "ZZ"           TO SM-SP-ORGST
              MOVE 999            TO SM-SP-SPRCLASS
              MOVE 99             TO SM-SP-SUBCLASS
              MOVE 99             TO SM-SP-LAWCODE 
              MOVE 99999999       TO SM-DATE-REVERSED
              MOVE 999            TO SM-SEQ  
              MOVE "Y"            TO SM-EOF-SW
              GO TO EXIT-PROCESS-READ-SM1.
 
           ADD 1                  TO SQL-RECORDS-PROCESSED.
    
       EXIT-PROCESS-READ-SM1.
           EXIT.

       PROCESS-READ-WK2 SECTION.
           PERFORM READ-WK2-FILE-NEXT.
           IF IO-BAD
              MOVE "ZZ"           TO WK2-ORGST    WS-OSM-ORGST
              MOVE 99             TO WK2-SPRCLASS WS-OSM-SPRCLASS
              MOVE 99             TO WK2-SUBCLASS WS-OSM-SUBCLASS
              MOVE 99             TO WK2-LAWCODE  WS-OSM-LAWCODE 
              MOVE 99999999       TO WK2-DATE     WS-OSM-DATE-REVERSED
              MOVE 999            TO WK2-SEQ      WS-OSM-SEQ  
              MOVE "Y"            TO WK2-EOF-SW
           ELSE
              ADD 1               TO A15-RECORDS-PROCESSED
              MOVE WK2-ORGST      TO WS-OSM-ORGST        
              MOVE WK2-SPRCLASS   TO WS-OSM-SPRCLASS     
              MOVE WK2-SUBCLASS   TO WS-OSM-SUBCLASS     
              MOVE WK2-LAWCODE    TO WS-OSM-LAWCODE.
              MOVE WK2-DATE       TO WS-OSM-DATE-REVERSED.
              MOVE WK2-SEQ        TO WS-OSM-SEQ. 

       EXIT-PROCESS-READ-WK2.
           EXIT.

      ******************************************************************
       COMPARE-FIELDS SECTION.
      ******************************************************************

           IF SM-MEMO1        NOT = WK2-MEMO1 
              MOVE SM1-KEY       TO D01-KEY
              MOVE "SM-MEMO1"    TO D01-TEXT
              MOVE WK2-MEMO1     TO D01-A15
              MOVE SM-MEMO1      TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SM-MEMO2        NOT = WK2-MEMO2 
              MOVE SM1-KEY       TO D01-KEY
              MOVE "SM-MEMO2"    TO D01-TEXT
              MOVE WK2-MEMO2     TO D01-A15
              MOVE SM-MEMO2      TO D01-S35
              PERFORM WRITE-DETAIL-LINE-ERROR.

           IF SM-MEMO-TYPE     NOT = WK2-MEMO-TYPE
              MOVE SM1-KEY        TO D01-KEY
              MOVE "SM-MEMO-TYPE" TO D01-TEXT
              MOVE WK2-MEMO-TYPE  TO D01-A15
              MOVE SM-MEMO-TYPE   TO D01-S35
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

           MOVE "BEGINNING ORGST: "                   TO R01-PROMPT.
           MOVE BEG-ORGST                             TO R01-TEXT.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING ORGST:   "                    TO R01-PROMPT.
           MOVE END-ORGST                             TO R01-TEXT.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING SPRCLASS: "                TO R01-PROMPT.
           MOVE BEG-SPRCLASS                          TO R01-TEXT.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING SPRCLASS:    "                TO R01-PROMPT.
           MOVE END-SPRCLASS                          TO R01-TEXT.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING SUBCLASS: "                TO R01-PROMPT.
           MOVE BEG-SUBCLASS                          TO R01-TEXT.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING SUBCLASS:    "                TO R01-PROMPT.
           MOVE END-SUBCLASS                          TO R01-TEXT.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING LAWCODE:  "                TO R01-PROMPT.
           MOVE BEG-LAWCODE                           TO R01-TEXT.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING LAWCODE:     "                TO R01-PROMPT.
           MOVE END-LAWCODE                           TO R01-TEXT.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING DATE:     "                TO R01-PROMPT.
           MOVE BEG-DATE                              TO R01-TEXT.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING DATE:        "                TO R01-PROMPT.
           MOVE END-DATE                              TO R01-TEXT.
           MOVE 01                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "BEGINNING SEQ:      "                TO R01-PROMPT.
           MOVE BEG-SEQ                               TO R01-TEXT.
           MOVE 02                                    TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ENDING SEQ:         "                TO R01-PROMPT.
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
           MOVE EXT-CONAME-CONAME  TO SMFILE-CONAME.
           MOVE EXT-CONAME-SYSDATE TO SMFILE-SYSDATE.
           DISPLAY SMFILE-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE SMFILE-FORM-FIELDS TO WR-BUF.
           MOVE SMFILE-FORM-LIST   TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE SMFILE-HELP--FILE TO HELP-LINK-FILE.
           MOVE SMFILE-HELP--DIR  TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBUT/SMFILE_EVA.CPY".
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
       COPY "LIBLP/LPSMGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPSM1RN.CPY".

       OPEN-OSM1-FILE.
           PERFORM OPEN-IT.
           MOVE OSM-PATH TO E-FILE.
           OPEN INPUT OSM-FILE.
           IF IO-FG = 8
              GO TO OPEN-OSM1-FILE.
           IF IO-FG = 7
              CLOSE OSM-FILE
              GO TO OPEN-OSM1-FILE.

       READ-OSM1-FILE-NEXT.
           PERFORM READ-IT.
           MOVE OSM-PATH TO E-FILE.
      *     MOVE OAU-KEY  TO E-KEYX.
           READ OSM-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-OSM1-FILE-NEXT.

       CLOSE-OSM1-FILE.
           PERFORM CLOSE-IT.
           CLOSE OSM-FILE.

      ******************************************************************
      * WK2 I-O SECTION
      ******************************************************************

       OPEN-WK2-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE "WK2" TO E-FILE.
           OPEN OUTPUT WK2-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK2-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE WK2-FILE
              GO TO OPEN-WK2-FILE-OUTPUT.
           UNLOCK WK2-FILE.
       OPEN-WK2-FILE.
           PERFORM OPEN-IT.
           MOVE "WK2" TO E-FILE.
           OPEN I-O WK2-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK2-FILE.
           IF IO-FG = 7
              CLOSE WK2-FILE
              GO TO OPEN-WK2-FILE.
           UNLOCK WK2-FILE.
       READ-WK2-FILE.
           PERFORM READ-IT.
           MOVE "WK2" TO E-FILE.
           MOVE WK2-KEY TO E-KEYX.
           READ WK2-FILE.
           IF IO-FG = 8
              GO TO READ-WK2-FILE.
       READ-WK2-FILE-NEXT.
           PERFORM READ-IT.
           MOVE "WK2" TO E-FILE.
           MOVE WK2-KEY TO E-KEYX.
           READ WK2-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-WK2-FILE-NEXT.
       READ-WK2-FILE-PREVIOUS.
           PERFORM READ-IT.
           MOVE "WK2" TO E-FILE.
           MOVE WK2-KEY TO E-KEYX.
           READ WK2-FILE PREVIOUS.
           IF IO-FG = 8
              GO TO READ-WK2-FILE-PREVIOUS.
       START-WK2-FILE-LE.
           PERFORM START-IT.
           MOVE "WK2" TO E-FILE.
           MOVE WK2-KEY TO E-KEYX.
           START WK2-FILE KEY <= WK2-KEY.
           UNLOCK WK2-FILE.
       START-WK2-FILE-GREATER.
           PERFORM START-IT.
           MOVE "WK2" TO E-FILE.
           MOVE WK2-KEY TO E-KEYX.
           START WK2-FILE KEY > WK2-KEY.
           UNLOCK WK2-FILE.
       START-WK2-FILE.
           PERFORM START-IT.
           MOVE "WK2" TO E-FILE.
           MOVE WK2-KEY TO E-KEYX.
           START WK2-FILE KEY NOT < WK2-KEY.
           UNLOCK WK2-FILE.
       WRITE-WK2-FILE.
           PERFORM WRITE-IT.
           MOVE "WK2" TO E-FILE.
           MOVE WK2-KEY TO E-KEYX.
           WRITE WK2-REC 
             INVALID KEY
                MOVE 22 TO IO-STATUS IO-FG
           END-WRITE.
           IF IO-FG = 8
              GO TO WRITE-WK2-FILE.
           UNLOCK WK2-FILE.
       REWRITE-WK2-FILE.
           PERFORM REWRITE-IT.
           MOVE "WK2" TO E-FILE.
           MOVE WK2-KEY TO E-KEYX.
           REWRITE WK2-REC.
           IF IO-FG = 8
              GO TO REWRITE-WK2-FILE.
           UNLOCK WK2-FILE.
       DELETE-WK2-FILE.
           PERFORM DELETE-IT.
           MOVE "WK2" TO E-FILE.
           MOVE WK2-KEY TO E-KEYX.
           DELETE WK2-FILE.
           IF IO-FG = 8
              GO TO DELETE-WK2-FILE.
           UNLOCK WK2-FILE.
       CLOSE-WK2-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK2-FILE.

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

      D    STOP MESS.
           PERFORM CLOSE-FILES.

           IF ( EXT-ROUTE-BUF = "00" )
              PERFORM PRU-OUT.

      *     IF A15-RECORDS-PROCESSED = 0 AND SQL-RECORDS-PROCESSED = 0
      *        MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
      *        PERFORM SEND-MESS.

           MOVE WK2-PATH  TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

       EXIT-PROG.

           PERFORM BTRACK-DONE.

           IF ( EXT-ROUTE-BUF = "00" )
              MOVE EXIT-PATH TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY SMFILE-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
