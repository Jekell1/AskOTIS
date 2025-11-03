       IDENTIFICATION     DIVISION.
       PROGRAM-ID.          LIVEBW.
       DATE-WRITTEN.    11/15/2016.
      **************************************************************************
      *
      * DESCRIPTION:  READ IN FIXED LENGTH FILE OF BORROWERS TO BE COPIED
      *               FROM ORIGINAL BRANCH TO NEW TARGET BRANCH.
      *               TX LIVE CHECKS WENT TO CUSTOMERS THAT HAD MOVED TO A
      *               CLOSER BRANCH BUT THEY DIDNT EXIST IN THE NEW BRANCH
      *               SO LIVE CHECK UPDATE WAS REJECTED WITH MISSING BW
      *                 WORLD PR# 1206
      *
      *               INPUT FILE FOR WORLD  ../../FTP/LIVEBW
      *
      * REV: 
      *  2016.1115 BAH NEW FOR WORLD PR# 1206
      *  2017.1002 BAH ACTIVATE 8 DIGIT DATES, PD0006
      *  2018.1004 BAH GLOBAL BWFILE
      *  2019.1004 BAH REMOVED ACCESS-CALL BWFILE
      *  2023.1208 BAH VALIDATE NON NUMERIC IMPORT FIELDS
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT LIVEBW-FILE ASSIGN LIVEBW-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  ACCESS SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
 
       COPY "LIBGB/GBFDPR.CPY".

      *******************************************************
      *   LIVE CHECK BORROWER IN, ASCII INPUT FILE 
      *******************************************************
       FD  LIVEBW-FILE
           LABEL RECORDS ARE STANDARD.
       01  LIVEBW-REC.
           03  LIVEBW-ORGBR      PIC 9999.
           03  LIVEBW-SSNO       PIC 9(9).
           03  LIVEBW-NEWBR      PIC 9999.
           03  LIVEBW-CR         PIC X.
           03  LIVEBW-LF         PIC X.


       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(43) VALUE
           "@(#)./SP/LIVEBW.CBL S34 06/19/21-15:39:26 >".
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

       01  LIVEBW-PATH.
           03  LIVEBW-PATH-BASE     PIC X(9).
           03  FILLER               PIC X     VALUE "/".
           03  LIVEBW-PATH-MACHINE  PIC X(2).
           03  LIVEBW-PATH-FTP      PIC X(5)  VALUE "/FTP/".
           03  FILLER               PIC X(6) VALUE "LIVEBW".


       01  HOLD-SYSDATE         PIC 9(8) VALUE 0.

       01  SAVE-BW-REC          PIC X(BW-SIZE).

      *-----------------------------------------------------------------
      * LINE WORKERS
      *-----------------------------------------------------------------
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.
 
       01  GRAND-TOTAL-RECS     PIC 9(10)       VALUE 0.

       COPY "LIBGB/EOBUF_EXT.CPY".
       01  VERSION-CONTROL        PIC X    VALUE "A".

       01  MESS-LIST            PIC X(05) VALUE "MESS.".

      *-----------------------------------------------------------------
      * PRINT BUFFERS
      *-----------------------------------------------------------------
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
           03  FILLER           PIC X(6)    VALUE " ".
           03  FILLER           PIC X(40) VALUE
           "LIVE CHECK BORROWER TRANSFER ".

       01  HEAD3.
           03  FILLER           PIC X(07)    VALUE "ORGBR".   
           03  FILLER           PIC X(12)    VALUE "SSNO#".
           03  FILLER           PIC X(8)     VALUE "NEWBR".
           03  FILLER           PIC X(8)     VALUE "CRITERIA".
 
       01  DETAIL-LINE              PIC X(80) VALUE SPACES.
       01  DETAIL-LINE1 REDEFINES DETAIL-LINE.
           03  D-ORGBR              PIC Z(4).
           03  FILLER               PIC X(2).
           03  D-SSNO               PIC 999/99/9999.
           03  FILLER               PIC X(2).
           03  D-NEWBR              PIC Z(4).
           03  FILLER               PIC X(2).
           03  D-MSG1               PIC X(30).


       01  DETAIL-LINE2 REDEFINES DETAIL-LINE.
           03  D-TOTAL-TAG      PIC X(16).
           03  FILLER           PIC X(03).
           03  D-TOTAL-REC      PIC Z(8)9.

 
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/EOSPOOLW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/LIVEBW_DEF.CPY".
       COPY "LIBSP/LIVEBW_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
 
      ****************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/LIVEBW_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE LIVEBW-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BM1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE
               LIVEBW-FILE.
       COPY "LIBGB/DECLRP2.CPY".
 
      *******************************************
       MAIN-MODULE SECTION.
      *******************************************
           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "LIVEBW" TO VDU-FORM-NAME.

      * FTP NEEDS TO BE LOWER
           CALL "C$TOLOWER" USING LIVEBW-PATH-FTP, VALUE 5.
           MOVE EXT-FILPATH-BASE TO LIVEBW-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE TO LIVEBW-PATH-MACHINE.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           PERFORM OPEN-LIVEBW-FILE.

       NEXT-LIVEBW.
           PERFORM READ-LIVEBW-FILE.
           IF IO-FG NOT = 0
              GO TO END-ROUTINE.

           IF LIVEBW-ORGBR NOT NUMERIC OR
              LIVEBW-SSNO NOT NUMERIC OR
              LIVEBW-NEWBR NOT NUMERIC
              MOVE "NON NUMERIC IMPORT FIELDS" TO D-MSG1
              MOVE LIVEBW-NEWBR TO D-NEWBR
              PERFORM WRITE-DETAIL-LINE
              GO TO NEXT-LIVEBW.

      * OPEN ORIGINAL BWFILE
           MOVE LIVEBW-ORGBR TO BR-NO.
           PERFORM GET-BR-SET-PATHS.
           IF IO-FG NOT = 0
              MOVE LIVEBW-ORGBR TO D-ORGBR
              MOVE LIVEBW-SSNO TO D-SSNO
              INSPECT D-SSNO REPLACING ALL "/" BY "-"
              MOVE "ORIG BWFILE DOES NOT EXIST" TO D-MSG1
              MOVE LIVEBW-NEWBR TO D-NEWBR
              PERFORM WRITE-DETAIL-LINE
              GO TO NEXT-LIVEBW.
              
           MOVE LIVEBW-SSNO TO BW-SSNO.              
           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
              MOVE LIVEBW-ORGBR TO D-ORGBR
              MOVE LIVEBW-SSNO  TO D-SSNO
              INSPECT D-SSNO REPLACING ALL "/" BY "-"
              MOVE LIVEBW-NEWBR TO D-NEWBR
              MOVE "ORIG BW SSNO DOES NOT EXIST" TO D-MSG1
              PERFORM WRITE-DETAIL-LINE
              PERFORM CLOSE-BW1-FILE
              GO TO NEXT-LIVEBW.

      * SAVE ORIGINAL BW RECORD
           MOVE BW-REC TO SAVE-BW-REC.

           PERFORM CLOSE-BW1-FILE.

      * OPEN NEW BWFILE
           MOVE LIVEBW-NEWBR TO BR-NO.
           PERFORM GET-BR-SET-PATHS.
           IF IO-FG NOT = 0
              MOVE LIVEBW-ORGBR TO D-ORGBR
              MOVE LIVEBW-SSNO TO D-SSNO
              INSPECT D-SSNO REPLACING ALL "/" BY "-"
              MOVE LIVEBW-NEWBR TO D-NEWBR
              MOVE "NEW BWFILE DOES NOT EXIST" TO D-MSG1
              PERFORM WRITE-DETAIL-LINE
              GO TO NEXT-LIVEBW.

           PERFORM OPEN-BM1-FILE.

           MOVE LIVEBW-SSNO TO BW-SSNO.              
           PERFORM READ-BW1-FILE.
           IF IO-FG = 0
              MOVE LIVEBW-ORGBR TO D-ORGBR
              MOVE LIVEBW-SSNO  TO D-SSNO
              INSPECT D-SSNO REPLACING ALL "/" BY "-"
              MOVE LIVEBW-NEWBR TO D-NEWBR
              MOVE "BORROWER ALREADY EXISTS" TO D-MSG1
              PERFORM WRITE-DETAIL-LINE
              PERFORM CLOSE-BW1-FILE
              PERFORM CLOSE-BM1-FILE
              GO TO NEXT-LIVEBW.
          
           MOVE SAVE-BW-REC  TO BW-REC.

           PERFORM WRITE-BW1-FILE.
           IF IO-FG NOT = 0
              MOVE LIVEBW-ORGBR TO D-ORGBR
              MOVE LIVEBW-SSNO  TO D-SSNO
              INSPECT D-SSNO REPLACING ALL "/" BY "-"
              MOVE LIVEBW-NEWBR TO D-NEWBR
              MOVE "NEW BORROWER WRITE ERROR" TO D-MSG1
              PERFORM WRITE-DETAIL-LINE
              PERFORM CLOSE-BW1-FILE
              PERFORM CLOSE-BM1-FILE
              GO TO NEXT-LIVEBW.
          

           PERFORM UPDATE-BM-FILE.

           PERFORM CLOSE-BM1-FILE.
           PERFORM CLOSE-BW1-FILE.

           ADD 1 TO GRAND-TOTAL-RECS.

           GO TO NEXT-LIVEBW.

       END-ROUTINE.
           PERFORM PRINT-GRAND-TOTALS.

           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE LIVEBW-QUEUE-POS TO Q-POS.
           MOVE LIVEBW-COPIES-POS TO C-POS.
           MOVE LIVEBW-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".
 
           PERFORM OPEN-BR-FILE.
 
           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE EXT-CONAME-SYSDATE TO H-DATE ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD      TO HOLD-SYSDATE.

           MOVE 0 TO  GRAND-TOTAL-RECS.
               
           PERFORM OPEN-PR-FILE.

           GO TO INITIALIZE-EXIT.

       INITIALIZE-ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       INITIALIZE-EXIT.
           EXIT.

      *******************************************************
       EO-EXEC-AUDIT SECTION.
      *******************************************************
           MOVE LIVEBW-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "LIVEBW FILE MISSING" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO EXT-EOBUF-ERRCD.
          
      *******************************************************
       ENTER-PARAMETERS SECTION.
      *******************************************************
       ENTER-PARAM.
           MOVE LIVEBW-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "LIVEBW FILE MISSING" TO MESS
              PERFORM SEND-MESS
              MOVE "1>" TO VDUSTATUS
              GO TO EXIT-PARM.

       EXIT-PARM.
           EXIT.


      *************************************************
       DISPLAY-PARAMETERS SECTION.
      *************************************************

 
      *******************************
       UPDATE-BM-FILE SECTION.
      *******************************

           MOVE BW-OWNBR TO BM-BRNO.
           MOVE BW-SSNO  TO BM-SSNO.

           SUBTRACT HOLD-SYSDATE FROM 99999999 GIVING BM-DATE.
           MOVE 1 TO BM-SEQ.

           MOVE SPACES TO BM-MEMO.
           STRING "BORROWER COPIED FROM BRANCH ",
                  LIVEBW-ORGBR,
                  ". CREATED BY LIVEBW." DELIMITED BY "  " INTO BM-MEMO.
           MOVE "S" TO BM-MEMO-TYPE.

           MOVE 1 TO BM-SEQ.
       WRITE-BM-MEMO.
           PERFORM WRITE-BM1-FILE.
           IF IO-FG NOT = 0
              IF BM-SEQ NOT = 999
                 ADD 1 TO BM-SEQ
                 GO TO WRITE-BM-MEMO
              ELSE
                 MOVE LIVEBW-ORGBR TO D-ORGBR
                 MOVE LIVEBW-SSNO  TO D-SSNO
                 INSPECT D-SSNO REPLACING ALL "/" BY "-"
                 MOVE LIVEBW-NEWBR TO D-NEWBR
                 MOVE "MAX LINES FOR BW MEMO REACHED" TO D-MSG1
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
       PRINT-GRAND-TOTALS SECTION.
      *************************************
           MOVE 2 TO LN-FEED.
           MOVE "TOTAL UPDATED : " TO D-TOTAL-TAG.
           MOVE GRAND-TOTAL-RECS TO D-TOTAL-REC.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM WRITE-DETAIL-LINE.

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
 
      *********************************************************
       FORM-RESET SECTION.
      *********************************************************
           MOVE EXT-CONAME-CONAME TO LIVEBW-CONAME.
           MOVE EXT-CONAME-SYSDATE TO LIVEBW-SYSDATE.
           DISPLAY LIVEBW-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE LIVEBW-FORM-FIELDS TO WR-BUF.
           MOVE LIVEBW-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE LIVEBW-HELP--FILE TO HELP-LINK-FILE.
           MOVE LIVEBW-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/LIVEBW_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      **************************************
       MISC-PARA SECTION.
      **************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       INVALID-RANGE.
           MOVE "INVALID RANGE" TO MESS.
           PERFORM SEND-MESS.
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       GET-BR-SET-PATHS.
           PERFORM READ-BR-FILE.
           IF IO-FG = 0
              MOVE EXT-FILPATH-BASE TO FILPATH-BASE
              MOVE BR-MACHINE       TO FILPATH-MACHINE
              MOVE BR-DATA-PATH     TO FILPATH-DATA

              MOVE "B"        TO BW-PATH-OVERRIDE
                                 BM-PATH-OVERRIDE
              PERFORM OPEN-BW1-FILE.

 
      ***********************************************
       IO-ROUTINES SECTION.
      ***********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBLP/LPBMGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       OPEN-LIVEBW-FILE.
           PERFORM OPEN-IT.
           MOVE LIVEBW-PATH TO E-FILE.
           OPEN INPUT LIVEBW-FILE.
           IF IO-FG = 8
              GO TO OPEN-LIVEBW-FILE.
           IF IO-FG = 7
              CLOSE LIVEBW-FILE
              GO TO OPEN-LIVEBW-FILE.
       READ-LIVEBW-FILE.
           PERFORM READ-IT.
           MOVE LIVEBW-PATH TO E-FILE.
           READ LIVEBW-FILE.
           IF IO-FG = 8
              GO TO READ-LIVEBW-FILE.
       CLOSE-LIVEBW-FILE.
           PERFORM CLOSE-IT.
           CLOSE LIVEBW-FILE.

       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPBW1IN.CPY".
       COPY "LIBLP/LPBM1IN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.
 
      ***********************************************
       END-PROGRAM SECTION.
      ***********************************************
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
              DESTROY LIVEBW-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
