       IDENTIFICATION     DIVISION.
       PROGRAM-ID.          PHOIMP.
       DATE-WRITTEN.    01/18/2023.
      **************************************************************************
      *
      * DESCRIPTION:  UPDATE TEXT OPT IN AND/OR CLEAR BAD PHONE NUMBERS.
      *               BASED ON COMBINATION OF CODE/ACTION, SCAN ALL BORROWERS
      *               ON LOAN, AND TAKE ACTION TO SAVE/CLEAR/UPDATE PHONE
      *               NUMBER SENT IN FILE. IN SOME CASES UPDATE TEXT OPT-IN
      *               FLAG AS WELL. CREATE BW MEMO.
      *
      * INPUT FILE = /USR/DATA/R1/FTP/PHONE/PHONESTATUS
      * DONE FILE = /USR/DATA/R1/FTP/PHONE/DONE/PHONESTATUS.YYMMDD.HHMM
      *
      *   READS FILE IN AS VARIABLE, BUT ONLY THE LAST FIELD IS VARIABLE
      *    MOVE INPUT RECORD TO A FIXED LENGTH WORKING STORAGE
      *
      * REV:
      * BAH 2023.0118 NEW FOR WORLD PR#1587
      * BAH 2024.0530 REMOVE BR-MACHINE NOT = DATA-PATH-FIL-MACHINE S35Q-57
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT PHONE-FILE ASSIGN PHONE-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  ACCESS SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
 
       COPY "LIBGB/GBFDPR.CPY".

       FD  PHONE-FILE
           RECORD IS VARYING IN SIZE FROM 0 TO 400 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  PHONE-REC.
           03  PHONE-BYTE          PIC X OCCURS 400 TIMES.


       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/PHOIMP.CBL S35 05/12/23-07:09:19 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01BM.CPY".
       COPY "LIBLP/LP01BM_SQL.CPY".
       COPY "LIBLP/LPWSBM.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".

       01  PHONE-IFN              PIC X(5) VALUE "PHONE".

       01  PHONE-PATH.
           03  PHONE-PATH-BASE    PIC X(9).
           03  FILLER             PIC X     VALUE "/".
           03  PHONE-PATH-MACHINE PIC X(2).
           03  PHONE-PATH-FTP     PIC X(11) VALUE "/FTP/PHONE/".
           03  FILLER             PIC X(11) VALUE "PHONESTATUS".
           
       01  PHONE-DONE-PATH.
           03  PHONE-DONE-BASE          PIC X(09).
           03  FILLER                   PIC X(01)  VALUE "/".
           03  PHONE-DONE-MACHINE       PIC X(02).
           03  PHONE-DONE-DIR           PIC X(11)  VALUE "/FTP/PHONE/".
           03  FILLER              PIC X(17)  VALUE "DONE/PHONESTATUS.".
           03  PHONE-DONE-YYMMDD        PIC 9(6).
           03  FILLER                   PIC X      VALUE ".".
           03  PHONE-DONE-HHMM          PIC 9(4).

       01  DATA-PATH.
           03  FILLER                          PIC X(9).
           03  DATA-PATH-FIL-MACHINE           PIC X(02).
           03  FILLER                          PIC X(23).

       01  PHONE-RECORD.
           03  PHONE-BRNO       PIC 9(4).
           03  PHONE-NUMBER     PIC 9(6).
           03  PHONE-PHONE      PIC 9(10).
           03  PHONE-CODE       PIC X.
           03  PHONE-ACTION     PIC X.
           03  PHONE-REASON     PIC X(80).

       01  HOLD-BRNO            PIC 9(4) VALUE 0.
       01  HOLD-SYSDATE         PIC 9(8) VALUE 0.

       01  PHONE-MEMO-AREA.
           03  FILLER          PIC X(5)  VALUE "OLD: ".
           03  MEMO-DESCRIPT   PIC X(20).
           03  FILLER          PIC X     VALUE " ".
           03  MEMO-ORG-PHONE  PIC 9(10).
           03  FILLER          PIC X     VALUE " ".
           03  MEMO-USERID     PIC X(10).

       01  OPT-MEMO-AREA.
           03  FILLER              PIC X(5)  VALUE "OLD: ".
           03  OPT-MEMO-DESCRIPT   PIC X(20).
           03  FILLER              PIC X     VALUE " ".
           03  OPT-ORG-OPT         PIC X.
           03  FILLER              PIC X(9)  VALUE SPACES.
           03  OPT-USERID         PIC X(10).

       01  REASON-MEMO-AREA.
           03  REASON-MEMO-REASON   PIC X(60).

       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.
 
       01  BR-TOTAL-READ        PIC 9(10)       VALUE 0.
       01  BR-TOTAL-PROC        PIC 9(10)       VALUE 0.
       01  GR-TOTAL-READ        PIC 9(10)       VALUE 0.
       01  GR-TOTAL-PROC        PIC 9(10)       VALUE 0.
 
       COPY "LIBGB/EOBUF_EXT.CPY".
       01  VERSION-CONTROL        PIC X    VALUE "A".

       01  MESS-LIST            PIC X(05) VALUE "MESS.".

       01  HEAD1.
           03  H-DATE           PIC X(08)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(08)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(08)    VALUE " ".
           03  FILLER           PIC X(05)     VALUE "PAGE:".
           03  H-PAGE-NO        PIC ZZZ9.
 
       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(12).
           03  FILLER           PIC X(13)    VALUE " ".
           03  FILLER           PIC X(40) VALUE
           "    PHONE STATUS IMPORT  ".

       01  HEAD3.
           03  FILLER           PIC X(06)    VALUE "BRNO".   
           03  FILLER           PIC X(06)    VALUE "LOAN#".
           03  FILLER           PIC X(02)    VALUE SPACES.
           03  FILLER           PIC X(32)    VALUE "LAST4".
           03  FILLER           PIC X(10)    VALUE SPACES.
           03  FILLER           PIC X(8)     VALUE "CRITERIA".
 
       01  DETAIL-LINE              PIC X(80) VALUE SPACES.
       01  DETAIL-LINE1 REDEFINES DETAIL-LINE.
           03  D-BRNO               PIC Z(4).
           03  FILLER               PIC X(2).
           03  D-NUMBER             PIC Z(6).
           03  FILLER               PIC X(2).
           03  D-LAST-4-SSNO        PIC 9(4).
           03  FILLER               PIC X(10).
           03  D-MSG1               PIC X(22).

       01  DETAIL-LINE2 REDEFINES DETAIL-LINE.
           03  D-TOTAL-TAG      PIC X(19).
           03  D-TOTAL-READ     PIC Z(8)9.
           03  D-TOTAL-PMT1     PIC X(5).
           03  D-TOTAL-PROC     PIC Z(8)9.
           03  D-TOTAL-PMT2     PIC X(10).
 
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/PHOIMP_DEF.CPY".
       COPY "LIBSP/PHOIMP_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
 
      ****************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/PHOIMP_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE PHONE-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BM1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE
               PHONE-FILE.
       COPY "LIBGB/DECLRP2.CPY".
 
      *******************************************
       MAIN-MODULE SECTION.
      *******************************************
           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "PHOIMP" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE 0 TO HOLD-BRNO.

           PERFORM OPEN-PHONE-FILE.

       NEXT-PHONE.
           PERFORM READ-PHONE-FILE.
           IF IO-FG NOT = 0
              PERFORM PRINT-BR-TOTALS
              GO TO END-ROUTINE.

           IF PHONE-BYTE(1) = X"1A"
              GO TO END-ROUTINE.

      * IN CASE THERE IS A HEADER RECORD
           IF PHONE-BYTE(1) NOT NUMERIC
              GO TO NEXT-PHONE.

           MOVE PHONE-REC TO PHONE-RECORD.

           IF PHONE-BRNO NOT = HOLD-BRNO
              IF HOLD-BRNO NOT = 0
                 PERFORM CLOSE-BW1-FILE
                 PERFORM CLOSE-LN1-FILE
                 PERFORM PRINT-BR-TOTALS
              END-IF
              PERFORM GET-BR-SET-PATHS
              IF IO-FG  = 9
                 MOVE "MISSING BRANCH" TO D-MSG1
                 MOVE PHONE-BRNO       TO D-BRNO
                 MOVE PHONE-NUMBER     TO D-NUMBER
                 PERFORM WRITE-DETAIL-LINE
                 MOVE 0 TO HOLD-BRNO
                 ADD 1  TO GR-TOTAL-READ
                 GO TO NEXT-PHONE
              ELSE
                 IF IO-FG = 8
                    MOVE 0 TO HOLD-BRNO
                    ADD 1 TO GR-TOTAL-READ
                    GO TO NEXT-PHONE.

           ADD 1 TO BR-TOTAL-READ
                    GR-TOTAL-READ.
              
           MOVE PHONE-BRNO     TO LN-OWNBR.              
           MOVE PHONE-NUMBER   TO LN-ACCTNO.              
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              MOVE BR-NO            TO D-BRNO
              MOVE PHONE-NUMBER     TO D-NUMBER
              MOVE "INVALID ACCTNO" TO D-MSG1
              PERFORM WRITE-DETAIL-LINE
              GO TO NEXT-PHONE.

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
              MOVE BR-NO          TO D-BRNO
              MOVE PHONE-NUMBER   TO D-NUMBER
              MOVE LN-SSNO(1)     TO D-LAST-4-SSNO
              MOVE "INVALID SSNO 1" TO D-MSG1
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF (BW-PHONE = PHONE-PHONE) OR
                  (BW-CELL-PHONE = PHONE-PHONE)
                 PERFORM UPDATE-BORROWER.

           IF LN-SSNO(2) = 0
              GO TO NEXT-PHONE.

           MOVE LN-SSNO(2) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
              MOVE BR-NO          TO D-BRNO
              MOVE PHONE-NUMBER   TO D-NUMBER
              MOVE LN-SSNO(2)     TO D-LAST-4-SSNO
              MOVE "INVALID SSNO 2" TO D-MSG1
              PERFORM WRITE-DETAIL-LINE
           ELSE
              IF (BW-PHONE = PHONE-PHONE) OR
                  (BW-CELL-PHONE = PHONE-PHONE)
                 PERFORM UPDATE-BORROWER.

           GO TO NEXT-PHONE.

       END-ROUTINE.
           PERFORM PRINT-GRAND-TOTALS.

      D    STOP MESS.
           MOVE PHONE-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT = "00"
              MOVE EXT-CONAME-SYSDATE TO ALP-DATE
              PERFORM CONVERT-ALPDATE-TO-YYMMDD
              MOVE DATE-YYMMDD TO PHONE-DONE-YYMMDD
              PERFORM GET-TIME
              MOVE H-AND-M TO PHONE-DONE-HHMM
              MOVE SPACES TO SYSTEM-BUF
              STRING MV-CMD," ",PHONE-PATH," ",PHONE-DONE-PATH
                     DELIMITED BY "  " INTO SYSTEM-BUF
              PERFORM SYSTEM-CALL.

           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE PHOIMP-QUEUE-POS TO Q-POS.
           MOVE PHOIMP-COPIES-POS TO C-POS.
           MOVE PHOIMP-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".
 
           MOVE EXT-FILPATH-BASE     TO PHONE-PATH-BASE
                                        PHONE-DONE-BASE.
           MOVE EXT-FILPATH-MACHINE  TO PHONE-PATH-MACHINE
                                        PHONE-DONE-MACHINE.

      * FTP/PHONE NEEDS TO BE LOWER
           CALL "C$TOLOWER" USING PHONE-PATH-FTP, VALUE 11.
           CALL "C$TOLOWER" USING PHONE-DONE-DIR, VALUE 11.

           MOVE PHONE-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "PHONESTATUS FILE MISSING" TO MESS
              PERFORM SEND-MESS
              GO TO INITIALIZE-ENDIT.

           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE EXT-CONAME-SYSDATE TO H-DATE ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO HOLD-SYSDATE.

           MOVE 0 TO BR-TOTAL-READ GR-TOTAL-READ
                     BR-TOTAL-PROC GR-TOTAL-PROC.
               
           IF EXT-ROUTE-BUF = "00"
              MOVE "PROCESSING PHONE STATUS FILE" TO MESS
              PERFORM SEND-LEGEND.

           MOVE "FIL"           TO GETENV-BUF.
           PERFORM GETENV-CALL.
           MOVE GETENV-BUF      TO DATA-PATH.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-PR-FILE.

           GO TO INITIALIZE-EXIT.

       INITIALIZE-ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       INITIALIZE-EXIT.
           EXIT.

      *******************************************************
       EO-EXEC-AUDIT SECTION.
      *******************************************************
           MOVE EXT-FILPATH-BASE     TO PHONE-PATH-BASE
                                        PHONE-DONE-BASE.
           MOVE EXT-FILPATH-MACHINE  TO PHONE-PATH-MACHINE
                                        PHONE-DONE-MACHINE.
           CALL "C$TOLOWER" USING PHONE-PATH-FTP, VALUE 11.
           CALL "C$TOLOWER" USING PHONE-DONE-DIR, VALUE 11.

           MOVE PHONE-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "PHONESTATUS FILE MISSING" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO EXT-EOBUF-ERRCD.
          
      *******************************************************
       ENTER-PARAMETERS SECTION.
      *******************************************************
           EXIT.

      *************************************************
       DISPLAY-PARAMETERS SECTION.
      *************************************************
           EXIT.
 
      *************************************************
       UPDATE-BORROWER SECTION.
      *************************************************

      * ONLY HERE IF BW-PHONE = PHONE-PHONE OR BW-CELL-PHONE = PHONE-PHONE

           ADD 1 TO BR-TOTAL-PROC GR-TOTAL-PROC.

           IF PHONE-CODE = "N" AND PHONE-ACTION = "R"
              IF BW-CELL-PHONE = PHONE-PHONE
                 MOVE "CELL PHONE"       TO MEMO-DESCRIPT
                 MOVE BW-CELL-PHONE      TO MEMO-ORG-PHONE
                 MOVE EXT-CONAME-USER-ID TO MEMO-USERID
                 MOVE PHONE-MEMO-AREA    TO BM-MEMO
                 PERFORM UPDATE-BM-FILE
                 MOVE "OPT OUT"          TO OPT-MEMO-DESCRIPT
                 MOVE BW-PRIVACY-OPT-OUT TO OPT-ORG-OPT
                 MOVE EXT-CONAME-USER-ID TO OPT-USERID
                 MOVE OPT-MEMO-AREA      TO BM-MEMO
                 PERFORM UPDATE-BM-FILE
                 MOVE PHONE-REASON     TO REASON-MEMO-REASON
                 MOVE REASON-MEMO-AREA TO BM-MEMO
                 PERFORM UPDATE-BM-FILE
                 MOVE "N"           TO BW-PRIVACY-OPT-OUT
                 MOVE 0             TO BW-CELL-PHONE
                 PERFORM REWRITE-BW1-FILE.

           IF PHONE-CODE = "N" AND PHONE-ACTION = "R"
              IF BW-PHONE = PHONE-PHONE
                 MOVE "HOME PHONE"       TO MEMO-DESCRIPT
                 MOVE BW-PHONE           TO MEMO-ORG-PHONE
                 MOVE EXT-CONAME-USER-ID TO MEMO-USERID
                 MOVE PHONE-MEMO-AREA    TO BM-MEMO
                 PERFORM UPDATE-BM-FILE
                 MOVE PHONE-REASON  TO REASON-MEMO-REASON
                 MOVE REASON-MEMO-AREA TO BM-MEMO
                 PERFORM UPDATE-BM-FILE
                 MOVE 0             TO BW-PHONE
                 PERFORM REWRITE-BW1-FILE.

      * ONLY IF CELL PHONE MATCHES. CREATE BORROWER MEMO WITH PREVIOUS OPT-OUT
      * AND SEPARATE MEMO WITH REASON
           IF PHONE-CODE = "S"
              IF BW-CELL-PHONE = PHONE-PHONE
                 MOVE "OPT OUT"          TO OPT-MEMO-DESCRIPT
                 MOVE BW-PRIVACY-OPT-OUT TO OPT-ORG-OPT
                 MOVE EXT-CONAME-USER-ID TO OPT-USERID
                 MOVE OPT-MEMO-AREA      TO BM-MEMO
                 PERFORM UPDATE-BM-FILE
                 MOVE PHONE-REASON       TO REASON-MEMO-REASON
                 MOVE REASON-MEMO-AREA   TO BM-MEMO
                 PERFORM UPDATE-BM-FILE
                 MOVE "S"           TO BW-PRIVACY-OPT-OUT
                 PERFORM REWRITE-BW1-FILE.


      *******************************
       UPDATE-BM-FILE SECTION.
      *******************************

           MOVE BW-OWNBR TO BM-BRNO.
           MOVE BW-SSNO  TO BM-SSNO.

           SUBTRACT HOLD-SYSDATE FROM 99999999 GIVING BM-DATE.
           MOVE 1 TO BM-SEQ.

           MOVE "S" TO BM-MEMO-TYPE.

           MOVE 1 TO BM-SEQ.
       WRITE-BM-MEMO.
           PERFORM WRITE-BM1-FILE.
           IF IO-FG NOT = 0
              IF BM-SEQ NOT = 999
                 ADD 1 TO BM-SEQ
                 GO TO WRITE-BM-MEMO
              ELSE
                 MOVE BW-OWNBR TO D-BRNO
                 MOVE BW-SSNO  TO D-NUMBER
                 MOVE "MAX LINES FOR MEMO REACHED" TO D-MSG1
                 PERFORM WRITE-DETAIL-LINE.

      ****************************
       WRITE-DETAIL-LINE SECTION.
      ****************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.
 
      *******************************
       HEAD-PAGE SECTION.
      *******************************
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
           MOVE 4 TO LN-COUNT.
           MOVE 2 TO LN-FEED.

      *************************************
      *      PRINT TOTALS
      *************************************
       PRINT-BR-TOTALS SECTION.
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

           PERFORM WRITE-DETAIL-LINE.

      ************************************************
       GET-BR-SET-PATHS SECTION.
      ************************************************
           MOVE PHONE-BRNO TO BR-KEY.
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

              MOVE "B"        TO BW-PATH-OVERRIDE
                                 BM-PATH-OVERRIDE
                                 LN-PATH-OVERRIDE
              MOVE BR-NO TO HOLD-BRNO
              PERFORM OPEN-BW1-FILE
              PERFORM OPEN-BM1-FILE 
              PERFORM OPEN-LN1-FILE.
       EXIT-GET-BR-SET-PATHS.
           EXIT.


       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
 
      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO PHOIMP-CONAME.
           MOVE EXT-CONAME-SYSDATE TO PHOIMP-SYSDATE.
           DISPLAY PHOIMP-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE PHOIMP-FORM-FIELDS TO WR-BUF.
           MOVE PHOIMP-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE PHOIMP-HELP--FILE TO HELP-LINK-FILE.
           MOVE PHOIMP-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/PHOIMP_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************************
       MISC-PARA SECTION.
      **************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
 
      ***************************************
       IO-ROUTINES SECTION.
      ***************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPBMGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       OPEN-PHONE-FILE.
           PERFORM OPEN-IT.
           MOVE PHONE-PATH TO E-FILE.
           OPEN INPUT PHONE-FILE.
           IF IO-FG = 8
              GO TO OPEN-PHONE-FILE.
           IF IO-FG = 7
              CLOSE PHONE-FILE
              GO TO OPEN-PHONE-FILE.
       READ-PHONE-FILE.
           PERFORM READ-IT.
           MOVE PHONE-PATH TO E-FILE.
           READ PHONE-FILE.
           IF IO-FG = 8
              GO TO READ-PHONE-FILE.
       CLOSE-PHONE-FILE.
           PERFORM CLOSE-IT.
           CLOSE PHONE-FILE.

       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPBW1IN.CPY".
       COPY "LIBLP/LPBM1IN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.
 
      ***************************************
       END-PROGRAM SECTION.
      ***************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF EXT-ROUTE-BUF = "00"
              MOVE "SP/EXTRMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       EXIT-PROG.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY PHOIMP-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
