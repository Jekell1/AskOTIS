       IDENTIFICATION     DIVISION.
       PROGRAM-ID.          TEXTIN.
       DATE-WRITTEN.    05/30/2014.
      **************************************************************************
      *
      * DESCRIPTION:  READ IN RETURNED FILE FROM REJECTED TEXT MESSAGES
      *                AND UPDATE BW-PRIVACY-OPT-OUT
      *                 WORLD PR# 987
      *
      *               INPUT FILE FOR WORLD  /USR/DATA/R?/FTP/TEXT.RETURN
      *
      *    IN PITBOW, WE ARE CREATING FILES TO SEND TO AN OUTSIDE COMPANY
      *    TO SEND TEXT MESSAGES TO CUSTOMERS. TEXT-OUT-RETURN-CD WILL BE
      *    BLANK. IF AFTER THEY SEND THE TEXT, THEY GET A RETURN THAT SAYS
      *    THE TEXT WAS REJECTED, THE CUSTOMER RETURNED AND SAID DO NOT
      *    SEND ANYMORE TEXT, OR OTHER REASONS, THE TEXTING COMPANY WILL
      *    RETURN THE RECORD WITH A CODE 25 IN THAT FIELD, TEXT-OUT-RETURN-CD
      *
      *    FIND THE BORROWER # 1 ON THAT LOAN, AND CHANGE BW-PRIVACY-OPT-OUT
      *    TO A "S" AND WRITE A MEMO FOR THAT BORROWER
      *
      *
      * REV: 
      *  140530 BAH NEW FOR WORLD PR# 987
      *  141201 BAH REMOVE TEST FOR TEXT-IN-RETURN-CD = "25", PR# 1037
      *  20171018 BAH ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181217 GLOBAL LNFILE
      * BAH 20191003 REMOVED ACCESS-CALL LNFILE, BWFILE
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT TEXT-IN-FILE ASSIGN TEXT-IN-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  ACCESS SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
 
       COPY "LIBGB/GBFDPR.CPY".

      *******************************************************
      *   TEXT IN, ASCII INPUT FILE FOR TEXT FORMAT
      *       FILE CREATED IN SP/PITBOW, THEN SENT BACK
      *******************************************************
       FD  TEXT-IN-FILE
           LABEL RECORDS ARE STANDARD.
       01  TEXT-IN-REC.
           03  TEXT-IN-TYPE       PIC X.
           03  TEXT-IN-OWNBR      PIC 9999.
           03  TEXT-IN-ACCTNO     PIC 9(6).
           03  TEXT-IN-CELL       PIC 9(10).
           03  TEXT-IN-FNAME      PIC X(16).
           03  TEXT-IN-LNAME      PIC X(16).
           03  TEXT-IN-BR-NAME    PIC X(10).
           03  TEXT-IN-BR-PHONE   PIC 9(10).
           03  TEXT-IN-AVAILCR    PIC 9(4).99.
           03  TEXT-IN-1ST-DUE    PIC ZZ.
           03  TEXT-IN-DUE-EXT    PIC XX.
           03  TEXT-IN-RETURN-CD  PIC XX.
           03  TEXT-CR            PIC X.
           03  TEXT-LF            PIC X.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/TEXTIN.CBL S34 06/21/21-11:04:02 >".
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
       01  TEXT-IN-PATH.
           03  TEXT-IN-PATH-BASE    PIC X(9).
           03  FILLER               PIC X     VALUE "/".
           03  TEXT-IN-PATH-MACHINE PIC X(2).
           03  TEXT-IN-PATH-FTP     PIC X(5)  VALUE "/FTP/".
           03  FILLER               PIC X(11) VALUE "TEXT.RETURN".
           


       01  HOLD-BRNO            PIC 9(4) VALUE 0.
       01  HOLD-SYSDATE         PIC 9(8) VALUE 0.

      *-----------------------------------------------------------------
      * LINE WORKERS
      *-----------------------------------------------------------------
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.
 
       01  GRAND-TOTAL-RECS     PIC 9(10)       VALUE 0.
 
      *-----------------------------------------------------------------
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
           03  FILLER           PIC X(13)    VALUE " ".
           03  FILLER           PIC X(40) VALUE
           "    RETURNED TEXTING FILE UPLOAD  ".
       01  HEAD3.
           03  FILLER           PIC X(06)    VALUE "BRNO".   
           03  FILLER           PIC X(06)    VALUE "LOAN#".
           03  FILLER           PIC X(02)    VALUE SPACES.
           03  FILLER           PIC X(32)    VALUE "NAME".
           03  FILLER           PIC X(10)    VALUE SPACES.
           03  FILLER           PIC X(8)     VALUE "CRITERIA".
 
       01  DETAIL-LINE              PIC X(80) VALUE SPACES.
       01  DETAIL-LINE1 REDEFINES DETAIL-LINE.
           03  D-BRNO               PIC Z(4).
           03  FILLER               PIC X(2).
           03  D-NUMBER             PIC Z(6).
           03  FILLER               PIC X(2).
           03  D-BORROWER           PIC X(32).
           03  FILLER               PIC X(10).
           03  D-MSG1               PIC X(12).

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
       COPY "LIBSP/TEXTIN_DEF.CPY".
       COPY "LIBSP/TEXTIN_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
 
      ****************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/TEXTIN_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE TEXT-IN-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BM1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE
               TEXT-IN-FILE.
       COPY "LIBGB/DECLRP2.CPY".
 
      *******************************************
       MAIN-MODULE SECTION.
      *******************************************
           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "TEXTIN" TO VDU-FORM-NAME.

      * FTP NEEDS TO BE LOWER
           CALL "C$TOLOWER" USING TEXT-IN-PATH-FTP, VALUE 5.
           MOVE EXT-FILPATH-BASE TO TEXT-IN-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE TO TEXT-IN-PATH-MACHINE.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE 0 TO HOLD-BRNO.

           PERFORM OPEN-TEXT-IN-FILE.

       NEXT-TEXT.
           PERFORM READ-TEXT-IN-FILE.
           IF IO-FG NOT = 0
              PERFORM CLOSE-BW1-FILE
              PERFORM CLOSE-LN1-FILE
              GO TO END-ROUTINE.

           IF TEXT-IN-OWNBR NOT = HOLD-BRNO
              IF HOLD-BRNO NOT = 0
                 PERFORM CLOSE-BW1-FILE
                 PERFORM CLOSE-LN1-FILE
              END-IF
              MOVE TEXT-IN-OWNBR TO BR-NO
              PERFORM GET-BR-SET-PATHS
              IF IO-FG NOT = 0
                 MOVE 0 TO HOLD-BRNO
                 GO TO NEXT-TEXT.
              

           MOVE TEXT-IN-OWNBR  TO LN-OWNBR.              
           MOVE TEXT-IN-ACCTNO TO LN-ACCTNO.              
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              MOVE BR-NO TO D-BRNO
              MOVE TEXT-IN-ACCTNO TO D-NUMBER
              STRING TEXT-IN-LNAME," ",TEXT-IN-FNAME DELIMITED BY "   "
                                    INTO D-BORROWER 
              MOVE "INVALID ACCTNO" TO D-MSG1
              PERFORM WRITE-DETAIL-LINE
              GO TO NEXT-TEXT.

           MOVE LN-SSNO(1) TO BW-SSNO.
           PERFORM READ-BW1-FILE.
           IF IO-FG NOT = 0
              MOVE BR-NO TO D-BRNO
              MOVE TEXT-IN-ACCTNO TO D-NUMBER
              STRING TEXT-IN-LNAME," ",TEXT-IN-FNAME DELIMITED BY "   "
                                    INTO D-BORROWER 
              MOVE "INVALID SSNO" TO D-MSG1
              PERFORM WRITE-DETAIL-LINE
              GO TO NEXT-TEXT.
          
      * DO THIS FIRST SO YOU STILL HAVE ORIGINAL PRIVACY VALUE FOR MEMO
           PERFORM UPDATE-BM-FILE.

           MOVE "S" TO BW-PRIVACY-OPT-OUT.
           PERFORM REWRITE-BW1-FILE.


           ADD 1 TO GRAND-TOTAL-RECS.

           GO TO NEXT-TEXT.

       END-ROUTINE.
           PERFORM PRINT-GRAND-TOTALS.

           GO TO END-PROGRAM.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************
           MOVE TEXTIN-QUEUE-POS TO Q-POS.
           MOVE TEXTIN-COPIES-POS TO C-POS.
           MOVE TEXTIN-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".
 
           PERFORM OPEN-BR-FILE.
 
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE EXT-CONAME-SYSDATE TO H-DATE ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO HOLD-SYSDATE.

           MOVE 0 TO  GRAND-TOTAL-RECS.
               
           IF EXT-ROUTE-BUF = "00"
              MOVE "PROCESSING TEXT RETURN FILE" TO MESS
              PERFORM SEND-LEGEND.

           PERFORM OPEN-PR-FILE.

           GO TO INITIALIZE-EXIT.

       INITIALIZE-ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       INITIALIZE-EXIT.
           EXIT.

      *******************************************************
       EO-EXEC-AUDIT SECTION.
      *******************************************************
           MOVE TEXT-IN-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "TEXT.RETURN FILE MISSING" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO EXT-EOBUF-ERRCD.
          
      *******************************************************
       ENTER-PARAMETERS SECTION.
      *******************************************************
       ENTER-PARAM.
           MOVE TEXT-IN-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "TEXT.RETURN FILE MISSING" TO MESS
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
           STRING "TEXT RETURNED, CHANGED TXT OPTION TO 'S' FROM ",
                   BW-PRIVACY-OPT-OUT
                   DELIMITED BY "  " INTO BM-MEMO.
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
           MOVE EXT-CONAME-CONAME TO TEXTIN-CONAME.
           MOVE EXT-CONAME-SYSDATE TO TEXTIN-SYSDATE.
           DISPLAY TEXTIN-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE TEXTIN-FORM-FIELDS TO WR-BUF.
           MOVE TEXTIN-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      *********************************************************
       SETUP-LINK-INFO SECTION.
      *********************************************************
           MOVE TEXTIN-HELP--FILE TO HELP-LINK-FILE.
           MOVE TEXTIN-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/TEXTIN_EVA.CPY".
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
                                 LN-PATH-OVERRIDE
              MOVE BR-NO TO HOLD-BRNO
              PERFORM OPEN-BW1-FILE
              PERFORM OPEN-BM1-FILE 
              PERFORM OPEN-LN1-FILE.

 
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
       OPEN-TEXT-IN-FILE.
           PERFORM OPEN-IT.
           MOVE TEXT-IN-PATH TO E-FILE.
           OPEN INPUT TEXT-IN-FILE.
           IF IO-FG = 8
              GO TO OPEN-TEXT-IN-FILE.
           IF IO-FG = 7
              CLOSE TEXT-IN-FILE
              GO TO OPEN-TEXT-IN-FILE.
       READ-TEXT-IN-FILE.
           PERFORM READ-IT.
           MOVE TEXT-IN-PATH TO E-FILE.
           READ TEXT-IN-FILE.
           IF IO-FG = 8
              GO TO READ-TEXT-IN-FILE.
       CLOSE-TEXT-IN-FILE.
           PERFORM CLOSE-IT.
           CLOSE TEXT-IN-FILE.

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
              DESTROY TEXTIN-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
