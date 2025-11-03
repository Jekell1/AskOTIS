       IDENTIFICATION DIVISION.
       PROGRAM-ID.      WACMEM.
      *****************************************************
      *  DESCRIPTION:    WAC MEMO IMPORT
      *
      *
      * INPUT FILE = /USR/DATA/R1/FTP/MEMO/WACMEMO
      * DONE FILE = /USR/DATA/R1/FTP/MEMO/DONE/WACMEMO.YYMMDD.HHMM
      *
      * FIXED LENGTH FILE BUT READING AS VARIABLE IN CASE THEY DONT
      * FILL IN ALL THE MEMO FIELDS
      *
      * REV:
      *  2017.09.11 BAH NEW, WORLD PR# 1275
      *  BAH 181130 GLOBAL LMFILE
      * BAH 181217 GLOBAL LNFILE
      * BAH 2019.0930 REMOVED ACCESS-CALL LNFILE, LMFILE
      * BAH 2023.1208 VALIDATE NON NUMERIC IMPORT FIELDS
      * BAH 2024.0530 REMOVE BR-MACHINE NOT = DATA-PATH-FIL-MACHINE S35Q-57
      *****************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MEMO-FILE ASSIGN TO MEMO-PATH
                           ORGANIZATION LINE SEQUENTIAL
                           ACCESS SEQUENTIAL
                           LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                           FILE STATUS FILE-STAT.
       COPY "LIBGB/GBFSPR.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  MEMO-FILE
           RECORD IS VARYING IN SIZE FROM 0 TO 400 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  MEMO-REC.
           03  MEMO-BYTE          PIC X OCCURS 400 TIMES.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/WACMEM.CBL S35 02/22/24-07:34:22 >".
      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       01  MEMO-IFN              PIC X(4) VALUE "MEMO".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".

       01  MEMO-PATH.
           03  MEMO-PATH-BASE          PIC X(09).
           03  FILLER                  PIC X(01)   VALUE "/".
           03  MEMO-PATH-MACHINE       PIC X(02).
           03  MEMO-PATH-DIR           PIC X(10)   VALUE "/FTP/MEMO/".
           03  FILLER                  PIC X(7)    VALUE "WACMEMO".

       01  MEMO-DONE-PATH.
           03  MEMO-DONE-BASE          PIC X(09).
           03  FILLER                  PIC X(01)  VALUE "/".
           03  MEMO-DONE-MACHINE       PIC X(02).
           03  MEMO-DONE-DIR           PIC X(10)  VALUE "/FTP/MEMO/".
           03  FILLER                  PIC X(13)  VALUE "DONE/WACMEMO.".
           03  MEMO-DONE-YYMMDD        PIC 9(6).          
           03  FILLER                  PIC X      VALUE ".".
           03  MEMO-DONE-HHMM          PIC 9(4).
           
       01  MEMO-RECORD.
           03  MEMO-BRNO       PIC 9(4).
           03  MEMO-NUMBER     PIC 9(6).
           03  MEMO-DATE       PIC X(8).
           03  MEMO-TYPE       PIC X.
           03  MEMO-PRIORITY   PIC X.
           03  MEMO-TEXT1      PIC X(60).
           03  MEMO-TEXT2      PIC X(60).
           03  MEMO-TEXT3      PIC X(60).
           03  MEMO-TEXT4      PIC X(60).

       01  LN-WORKERS.
           03  LN-FEED          PIC 99     VALUE 0.
           03  LN-COUNT         PIC 999    VALUE 99.
           03  LN-WK            PIC 999    VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)   VALUE 0.

       01  DATA-PATH.
           03  FILLER                          PIC X(9).
           03  DATA-PATH-FIL-MACHINE           PIC X(02).
           03  FILLER                          PIC X(23).

       COPY "LIBGB/EOBUF_EXT.CPY".
       01  VERSION-CONTROL      PIC X VALUE "A".

       01  RECORD-COUNTER       PIC 9(10)       VALUE 0.
       01  BR-TOTAL-READ        PIC 9(10)       VALUE 0.
       01  BR-TOTAL-PROC        PIC 9(10)       VALUE 0.
       01  GR-TOTAL-READ        PIC 9(10)       VALUE 0.
       01  GR-TOTAL-PROC        PIC 9(10)       VALUE 0.

       01  HOLD-BRNO            PIC 9(4) VALUE 0.

       01  MESS-LIST            PIC X(05) VALUE "MESS.".

      *----------------------------------------------------------------
       01  HEAD1.
           03  H-DATE               PIC X(8) VALUE SPACES.
           03  FILLER               PIC XX   VALUE SPACES.
           03  H-HR                 PIC 99   VALUE 0.
           03  FILLER               PIC X    VALUE ":".
           03  H-MIN                PIC 99   VALUE 0.
           03  FILLER               PIC X(10) VALUE SPACES.
           03  H-NAME               PIC X(40).
           03  FILLER               PIC X(10).
           03  FILLER               PIC X(5)  VALUE "PAGE ".
           03  H-PAGE-NO            PIC ZZZ9.

       01  HEAD2.
           03  FILLER               PIC X(10) VALUE "USER ID:".
           03  H-USERID             PIC X(10).
           03  FILLER               PIC X(10) VALUE SPACES.
           03  FILLER               PIC X(40) VALUE
               "WAC UPLOAD MEMO LISTING".
       01  HEAD3.
           03  FILLER               PIC X(15) VALUE "BRANCH".

       01  DETAIL-LINE              PIC X(80).
       01  DETAIL-LINE1 REDEFINES DETAIL-LINE.
           03  D-BRNO           PIC Z(4).
           03  FILLER           PIC X(2).
           03  D-NUMBER         PIC Z(6).
           03  FILLER           PIC X(2).
           03  D-MSG1           PIC X(40).

       01  DETAIL-LINE2 REDEFINES DETAIL-LINE.
           03  D-TOTAL-TAG      PIC X(19).
           03  D-TOTAL-READ     PIC Z(8)9.
           03  D-TOTAL-PMT1     PIC X(5).
           03  D-TOTAL-PROC     PIC Z(8)9.
           03  D-TOTAL-PMT2     PIC X(10).

       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/WACMEM_DEF.CPY".
       COPY "LIBSP/WACMEM_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/WACMEM_SCN.CPY".

      *----------------------------------------------------------------
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               MEMO-FILE PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE
               MEMO-FILE PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ***************************
       MAIN-MODULE SECTION.
      ***************************
           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "WACMEM" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.


           MOVE "PROCESSING MEMO UPLOAD" TO MESS.
           PERFORM SEND-LEGEND.

           MOVE 0 TO HOLD-BRNO.

           PERFORM OPEN-MEMO-FILE.
       NEXT-RECORD.
           MOVE SPACES TO MEMO-REC MEMO-RECORD.
           PERFORM READ-MEMO-FILE.
           IF IO-FG NOT = 0
              PERFORM PRINT-BR-TOTALS
              GO TO END-ROUTINE.

           IF MEMO-BYTE(1) = X"1A"
              GO TO END-ROUTINE.

           ADD 1 TO RECORD-COUNTER.

      * REPORT HEADERS ARE THE FIRST RECORD
           IF MEMO-BYTE(1) NOT NUMERIC
              GO TO NEXT-RECORD.

           MOVE MEMO-REC TO MEMO-RECORD.

           IF MEMO-BRNO NOT NUMERIC OR MEMO-NUMBER NOT NUMERIC
              MOVE "NON NUMERICS" TO D-MSG1
              MOVE MEMO-BRNO      TO D-BRNO
              MOVE MEMO-NUMBER    TO D-NUMBER
              PERFORM WRITE-DETAIL-LINE
              GO TO NEXT-RECORD.

           IF MEMO-BRNO NOT = HOLD-BRNO
              IF HOLD-BRNO NOT = 0
                 PERFORM CLOSE-LN1-FILE
                 PERFORM CLOSE-LM1-FILE
                 PERFORM PRINT-BR-TOTALS
              END-IF
              PERFORM GET-BR-SET-PATHS
              IF IO-FG  = 9
                 MOVE "MISSING BRANCH" TO D-MSG1
                 MOVE MEMO-BRNO        TO D-BRNO
                 MOVE MEMO-NUMBER      TO D-NUMBER
                 PERFORM WRITE-DETAIL-LINE
                 MOVE 0 TO HOLD-BRNO
                 ADD 1 TO GR-TOTAL-READ
                 GO TO NEXT-RECORD
              ELSE
                 IF IO-FG  = 8
                    MOVE 0 TO HOLD-BRNO
                    ADD 1 TO GR-TOTAL-READ
                    GO TO NEXT-RECORD.

           ADD 1 TO BR-TOTAL-READ
                    GR-TOTAL-READ.

           MOVE MEMO-BRNO   TO LN-OWNBR.
           MOVE MEMO-NUMBER TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              MOVE BR-NO TO D-BRNO
              MOVE MEMO-NUMBER TO D-NUMBER
              MOVE "INVALID ACCTNO" TO D-MSG1
              PERFORM WRITE-DETAIL-LINE
              GO TO NEXT-RECORD.

      * DELETED LOAN, RESCISSION
           IF LN-POCD-NONLOAN
              MOVE BR-NO TO D-BRNO
              MOVE LN-ACCTNO TO D-NUMBER
              MOVE "ACCOUNT VOIDED" TO D-MSG1
              PERFORM WRITE-DETAIL-LINE
              GO TO NEXT-RECORD.

           ADD 1 TO BR-TOTAL-PROC GR-TOTAL-PROC.

           MOVE LM-PATH-OWNBR TO LM-BRNO.
           MOVE LN-ACCTNO     TO LM-ACCTNO.
           MOVE MEMO-DATE     TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           SUBTRACT DATE-YYYYMMDD FROM 99999999 GIVING LM-DATE.
      * MAKE PRIORITY IF FLAG IS SET
           IF MEMO-PRIORITY = "P"
              MOVE 0          TO LM-DATE.

           MOVE MEMO-TEXT1 TO LM-MEMO.
           IF MEMO-TYPE NOT = SPACES
              MOVE MEMO-TYPE TO LM-MEMO-TYPE
           ELSE
              MOVE "S" TO LM-MEMO-TYPE.
           MOVE 1 TO LM-SEQ.

       WRITE-MEMO.
           PERFORM WRITE-LM1-FILE.
           IF IO-FG NOT = 0
              IF LM-SEQ NOT = 999
                 ADD 1 TO LM-SEQ
                 GO TO WRITE-MEMO
              ELSE
                 MOVE BR-NO TO D-BRNO
                 MOVE LN-ACCTNO TO D-NUMBER
                 MOVE "MAX LINES FOR MEMO REACHED" TO D-MSG1
                 PERFORM WRITE-DETAIL-LINE
                 GO TO NEXT-RECORD.

           IF MEMO-TEXT2 = SPACES OR LM-SEQ = 999
              GO TO NEXT-RECORD.

           MOVE MEMO-TEXT2 TO LM-MEMO.
           ADD 1 TO LM-SEQ.
       WRITE-MEMO2.
           PERFORM WRITE-LM1-FILE.
           IF IO-FG NOT = 0
              IF LM-SEQ NOT = 999
                 ADD 1 TO LM-SEQ
                 GO TO WRITE-MEMO2
              ELSE
                 MOVE BR-NO TO D-BRNO
                 MOVE LN-ACCTNO TO D-NUMBER
                 MOVE "MAX LINES FOR MEMO2 REACHED" TO D-MSG1
                 PERFORM WRITE-DETAIL-LINE
                 GO TO NEXT-RECORD.

           IF MEMO-TEXT3 = SPACES OR LM-SEQ = 999
              GO TO NEXT-RECORD.

           MOVE MEMO-TEXT3 TO LM-MEMO.
           ADD 1 TO LM-SEQ.
       WRITE-MEMO3.
           PERFORM WRITE-LM1-FILE.
           IF IO-FG NOT = 0
              IF LM-SEQ NOT = 999
                 ADD 1 TO LM-SEQ
                 GO TO WRITE-MEMO3
              ELSE
                 MOVE BR-NO TO D-BRNO
                 MOVE LN-ACCTNO TO D-NUMBER
                 MOVE "MAX LINES FOR MEMO3 REACHED" TO D-MSG1
                 PERFORM WRITE-DETAIL-LINE
                 GO TO NEXT-RECORD.

           IF MEMO-TEXT4 = SPACES OR LM-SEQ = 999
              GO TO NEXT-RECORD.

           MOVE MEMO-TEXT4 TO LM-MEMO.
           ADD 1 TO LM-SEQ.
       WRITE-MEMO4.
           PERFORM WRITE-LM1-FILE.
           IF IO-FG NOT = 0
              IF LM-SEQ NOT = 999
                 ADD 1 TO LM-SEQ
                 GO TO WRITE-MEMO4
              ELSE
                 MOVE BR-NO TO D-BRNO
                 MOVE LN-ACCTNO TO D-NUMBER
                 MOVE "MAX LINES FOR MEMO4 REACHED" TO D-MSG1
                 PERFORM WRITE-DETAIL-LINE
                 GO TO NEXT-RECORD.

           GO TO NEXT-RECORD.

       END-ROUTINE.
           PERFORM PRINT-GRAND-TOTALS.

           MOVE MEMO-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT = "00"
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-YYMMDD
              MOVE DATE-YYMMDD TO MEMO-DONE-YYMMDD
              PERFORM GET-TIME
              MOVE H-AND-M TO MEMO-DONE-HHMM
              MOVE SPACES TO SYSTEM-BUF
              STRING MV-CMD," ",MEMO-PATH," ",MEMO-DONE-PATH
                     DELIMITED BY "  " INTO SYSTEM-BUF
              PERFORM SYSTEM-CALL.

           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE WACMEM-QUEUE-POS TO Q-POS.
           MOVE WACMEM-COPIES-POS TO C-POS.
           MOVE WACMEM-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           MOVE EXT-FILPATH-BASE     TO MEMO-PATH-BASE
                                        MEMO-DONE-BASE.
           MOVE EXT-FILPATH-MACHINE  TO MEMO-PATH-MACHINE
                                        MEMO-DONE-MACHINE.

           CALL "C$TOLOWER" USING MEMO-PATH-DIR, VALUE 10.
           CALL "C$TOLOWER" USING MEMO-DONE-DIR, VALUE 10.

           MOVE MEMO-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "WACMEM FILE NOT FOUND" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.

           MOVE 0 TO BR-TOTAL-READ GR-TOTAL-READ
                     BR-TOTAL-PROC GR-TOTAL-PROC.

           MOVE SPACES TO DETAIL-LINE.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           MOVE "FIL"           TO GETENV-BUF.
           PERFORM GETENV-CALL.
           MOVE GETENV-BUF      TO DATA-PATH.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-BR-FILE.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************
           EXIT.


      *************************************
       PRINT-BR-TOTALS SECTION.
      *************************************
           MOVE 2 TO LN-FEED.
           MOVE "BRANCH &&&& TOTAL: " TO D-TOTAL-TAG.
           INSPECT D-TOTAL-TAG REPLACING FIRST "&&&&" BY BR-NO.
           MOVE BR-TOTAL-READ         TO D-TOTAL-READ.
           MOVE " READ"       TO D-TOTAL-PMT1.
           MOVE BR-TOTAL-PROC         TO D-TOTAL-PROC.
           MOVE " PROCESSED"  TO D-TOTAL-PMT2.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 2 TO LN-FEED.

           MOVE 0 TO BR-TOTAL-READ BR-TOTAL-PROC.


      *************************************
       PRINT-GRAND-TOTALS SECTION.
      *************************************
           MOVE 2 TO LN-FEED.
           MOVE "GRAND TOTALS     : " TO D-TOTAL-TAG.
           MOVE GR-TOTAL-READ TO D-TOTAL-READ.
           MOVE " READ"       TO D-TOTAL-PMT1.
           MOVE GR-TOTAL-PROC TO D-TOTAL-PROC.
           MOVE " PROCESSED"  TO D-TOTAL-PMT2.
           PERFORM WRITE-DETAIL-LINE.


      **************************************
       WRITE-DETAIL-LINE SECTION.
      **************************************
           ADD LN-COUNT LN-FEED GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      **************************************
       HEAD-PAGE SECTION.
      **************************************
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.
           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE HEAD3 TO PR-REC.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-PR-REC.
       END-HEAD-PAGE.
           MOVE 4 TO LN-FEED.
           MOVE 7 TO LN-COUNT.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO WACMEM-CONAME.
           MOVE EXT-CONAME-SYSDATE TO WACMEM-SYSDATE.
           DISPLAY WACMEM-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE WACMEM-FORM-FIELDS TO WR-BUF.
           MOVE WACMEM-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE WACMEM-HELP--FILE TO HELP-LINK-FILE.
           MOVE WACMEM-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/WACMEM_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ************************************************
       GET-BR-SET-PATHS SECTION.
      ************************************************
           MOVE MEMO-BRNO TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG = 0
      *       IF BR-MACHINE NOT = DATA-PATH-FIL-MACHINE
      *          MOVE 8 TO IO-FG
      *          ADD 1 TO GR-TOTAL-OTHER
      *          GO TO EXIT-GET-BR-SET-PATHS
      *       END-IF
              MOVE EXT-FILPATH-BASE TO FILPATH-BASE
              MOVE BR-MACHINE       TO FILPATH-MACHINE
              MOVE BR-DATA-PATH     TO FILPATH-DATA
              MOVE "B"        TO LN-PATH-OVERRIDE
                                 LM-PATH-OVERRIDE

              PERFORM OPEN-LN1-FILE
              MOVE MEMO-BRNO TO HOLD-BRNO
              PERFORM OPEN-LM1-FILE.

       EXIT-GET-BR-SET-PATHS.
           EXIT.

      ************************************
       MISC-PARAGRAPH SECTION.
      ************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".

       OPEN-MEMO-FILE.
           PERFORM OPEN-IT.
           MOVE MEMO-IFN TO E-FILE.
           OPEN INPUT MEMO-FILE.
           IF IO-FG = 8
              GO TO OPEN-MEMO-FILE.
           IF IO-FG = 7
              CLOSE MEMO-FILE
              GO TO OPEN-MEMO-FILE.
       READ-MEMO-FILE.
           PERFORM READ-IT.
           MOVE MEMO-IFN TO E-FILE.
           READ MEMO-FILE.
           IF IO-FG = 8
              GO TO READ-MEMO-FILE.
       CLOSE-MEMO-FILE.
           PERFORM CLOSE-IT.
           CLOSE MEMO-FILE.
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPLM1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      *********************************
       END-PROGRAM SECTION.
      *********************************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.
       EXIT-PROG.
           IF EXT-ROUTE-BUF = "00"
              MOVE "SP/EXTRMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY WACMEM-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
