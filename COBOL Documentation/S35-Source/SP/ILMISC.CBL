       IDENTIFICATION DIVISION.
       PROGRAM-ID.       ILMISC.
       DATE-WRITTEN. 03/17/2020.
      **************************************************************************
      *
      *                      (SP)
      *    DESCRIPTION:  UPDATE LSM-MISC-FIELD(15) WITH VERITEC ID
      *
      * BATCHABLE
      *        PIPE DELIMITED FILE  ../../FTP/ILL/VERITECID
      *        MUST USE ILNEXT EXTRACT FILE AS INPUT WITH VERITEC ID ON THE
      *         END OF EACH RECORD
      *
      * REV:
      *  03.17.2020 BAH WORLD #1426
      *  20220916 BAH ALL NEW FORMAT, WORLD #1570
      *  20221108 BAH SKIP NEGATIVE VERITEC IDS THAT ARE RETURNED, INDICATES
      *               ERROR, PRINT EXCEPTION #1577
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT WK-FILE ASSIGN TO WK-PATH
                           ORGANIZATION LINE SEQUENTIAL
                           ACCESS SEQUENTIAL
                           LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                           FILE STATUS FILE-STAT.
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
       FD  WK-FILE
           RECORD IS VARYING IN SIZE FROM 5 TO 2000 CHARACTERS
           DEPENDING ON REC-LENGTH
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03 WK-BYTE           PIC X  OCCURS 2000 TIMES.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/ILMISC.CBL S34 06/19/21-12:31:04 >".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LSM.CPY".
       COPY "LIBLP/LP01LSM_SQL.CPY".
       COPY "LIBLP/LPWSLSM.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       01  WK-PATH.
           03  WK-PATH-BASE        PIC X(9).
           03  FILLER              PIC X VALUE "/".
           03  WK-PATH-MACHINE     PIC X(2).
           03  WK-PATH-FTP         PIC X(9) VALUE "/FTP/ILL/".
           03  FILLER              PIC X(9) VALUE "VERITECID".

       01  REC-LENGTH           PIC 9(6).
       01  SUB                  PIC 9(6).
       01  OUT-SUB              PIC 9(4).
       01  PR-IFN               PIC XX      VALUE "PR".
       01  WK-IFN               PIC XX      VALUE "WK".
       01  HOLD-BRNO            PIC 9(4)    VALUE 0.
       01  RECORD-COUNTER       PIC 9(8)    VALUE 0.
       01  RECORDS-WRITTEN      PIC 9(8)    VALUE 0.
       01  RECORD-EXCEPTIONS    PIC 9(8)    VALUE 0.
       01  NEGATIVE-VERITEC-ID  PIC X       VALUE "N".

      **********************
      *   LINE WORKERS     *
      **********************
       01  LN-WORKERS.
           03  LN-FEED          PIC 99     VALUE 0.
           03  LN-COUNT         PIC 999    VALUE 99.
           03  LN-WK            PIC 999    VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)   VALUE 0.
 
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  REPORT-ONLY      PIC X.

       01  VERSION-CONTROL      PIC X VALUE "A".

       01  FIRST-TIME           PIC X     VALUE "Y".
       01  MESS-LIST            PIC X(5) VALUE "MESS.".

       01  LINE-WK.
           03  LINE-ACTION       PIC X(6).
           03  LINE-LICENSE-ID   PIC X(4).
           03  LINE-LENDER-ID.
               05  LINE-BRNO     PIC 9(4).
               05  LINE-ACCTNO   PIC 9(6).
           03  LINE-MISC         PIC X(15).
           03  LINE-MISC-BYTE REDEFINES LINE-MISC PIC X OCCURS 15.

       01  HOLD-EXCEPTION  PIC X(30).

      ***********************************
      *     P R I N T   B U F F E R S   *
      ***********************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(10)    VALUE " ".
           03  H-NAME           PIC X(35)    VALUE " ".
           03  FILLER           PIC X(10)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(5)     VALUE " ".
           03  FILLER           PIC X(35)    VALUE
           "- - - VERITEC  MISC FIELD 15 - - - ".

       01  HEAD3.
           03  FILLER           PIC X(4)      VALUE "BRNO".
           03  FILLER           PIC X(04).
           03  FILLER           PIC X(06)     VALUE "ACCTNO".
           03  FILLER           PIC X(04).
           03  FILLER           PIC X(34)     VALUE "EXCEPTION".
           03  FILLER           PIC X(4)      VALUE "ID".

       01  DETAIL-LINE          PIC X(80) VALUE SPACES.
       01  D-LINE1  REDEFINES DETAIL-LINE.
           03  D-BRNO           PIC Z(4).
           03  FILLER           PIC X(04).
           03  D-ACCTNO         PIC Z(06).
           03  FILLER           PIC X(04).
           03  D-EXCEPTION      PIC X(30).
           03  FILLER           PIC X(04).
           03  D-ID             PIC X(17).

       01  DETAIL-LINE2 REDEFINES DETAIL-LINE.
           03  R-COUNTERX       PIC X(20).
           03  R-COUNTER        PIC Z(8)9.
           03  R-COUNTER-ALPHA REDEFINES R-COUNTER PIC X(9).

       COPY "LIBGB/SYSTEMW.CPY".

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/ILMISC_DEF.CPY".
       COPY "LIBSP/ILMISC_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/ILMISC_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
              PR-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LSM1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE
              PR-FILE WK-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - P R O G R A M
      *
      ******************************************************************
       MAIN-PROGRAM SECTION.
           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "ILMISC"      TO VDU-FORM-NAME.

      * FTP NEEDS TO BE LOWER
           CALL "C$TOLOWER" USING WK-PATH-FTP VALUE 9.
           MOVE EXT-FILPATH-BASE    TO WK-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE TO WK-PATH-MACHINE.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
               GO TO END-PROGRAM.

           PERFORM SET-MISC-FIELD.

           MOVE "N" TO FIRST-TIME.
           PERFORM PRINT-TOTALS.

           GO TO END-PROGRAM.

      *****************************
       SET-MISC-FIELD SECTION.
      *****************************
           PERFORM OPEN-WK-FILE.

           MOVE 0 TO HOLD-BRNO RECORD-COUNTER RECORD-EXCEPTIONS.
       NEXT-REC.
           MOVE SPACES TO WK-REC.
           PERFORM READ-WK-FILE.
           IF IO-FG NOT = 0 OR WK-REC(1:1) = X"1A"
              GO TO EXIT-SET-MISC.
 
           ADD 1 TO RECORD-COUNTER.

           UNSTRING WK-REC
             DELIMITED BY "|" INTO    LINE-ACTION
                                      LINE-LICENSE-ID
                                      LINE-LENDER-ID.

           PERFORM FIND-VERITEC-ID.

      * SKIP NEGATIVE VERITEC ID'S THAT ARE RETURNED. INDICATES ERROR!
           IF NEGATIVE-VERITEC-ID = "Y"
              ADD 1 TO RECORD-EXCEPTIONS
              MOVE "*** NEGATIVE VERITEC ID" TO HOLD-EXCEPTION
              PERFORM DETAIL-LINE-PROCESS
              GO TO NEXT-REC.

           IF FIRST-TIME = "Y" OR LINE-BRNO NOT = HOLD-BRNO
              MOVE LINE-BRNO TO BR-NO
              PERFORM GET-LN-PATH
              IF IO-FG NOT = 0
                 MOVE 0                TO HOLD-BRNO
                 GO TO NEXT-REC
              ELSE
                 MOVE "N"    TO FIRST-TIME
                 MOVE BR-NO  TO HOLD-BRNO
                 PERFORM OPEN-LN1-FILE
                 PERFORM OPEN-LSM1-FILE.

           MOVE LINE-BRNO   TO LN-OWNBR.
           MOVE LINE-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              ADD 1 TO RECORD-EXCEPTIONS
              MOVE "*** INVALID ACCTNO" TO HOLD-EXCEPTION
              PERFORM DETAIL-LINE-PROCESS
              GO TO NEXT-REC.

           IF REPORT-ONLY NOT = "Y"
              PERFORM CREATE-TRAILER-MISC.

      *    PERFORM DETAIL-LINE-PROCESS.

           GO TO NEXT-REC.

       EXIT-SET-MISC.
           EXIT.

      ***********************************
       FIND-VERITEC-ID SECTION.
      ***********************************

           MOVE SPACES TO LINE-MISC.

           MOVE "N" TO NEGATIVE-VERITEC-ID.

           MOVE REC-LENGTH TO SUB.
       FIND-VERITEC-NEXT.
           IF WK-BYTE(SUB) NOT NUMERIC
              SUBTRACT 1 FROM SUB
              IF SUB > 0
                 GO TO FIND-VERITEC-NEXT
              ELSE
                 GO TO EXIT-FIND-VERITEC.

      * LAST NUMERIC BYTE, NOW FIND THE TILDA IN FRONT OF THIS NUMBER

       FIND-VERITEC-PIPE.
           IF WK-BYTE(SUB) NOT = "|"
              SUBTRACT 1 FROM SUB
              IF SUB > 0
                 GO TO FIND-VERITEC-PIPE
              ELSE
                 GO TO EXIT-FIND-VERITEC.

      * NOW YOU ARE SITTING ON THE PIPE IN FRONT OF THE VERITEC ID
           ADD 1  TO SUB.
           MOVE 1 TO OUT-SUB.
       FIND-VERITEC-NUMBER.
           IF WK-BYTE(SUB) = "-"
              MOVE "Y" TO NEGATIVE-VERITEC-ID.
           MOVE WK-BYTE(SUB) TO LINE-MISC-BYTE(OUT-SUB).
           ADD 1 TO OUT-SUB.
           IF SUB NOT > REC-LENGTH
              ADD 1 TO SUB
              GO TO FIND-VERITEC-NUMBER.

      * NOW YOU HAVE THE LINE-MISC VERITEC ID.

       EXIT-FIND-VERITEC.
           EXIT.

      *********************************************
       CREATE-TRAILER-MISC SECTION.
      *********************************************

           MOVE LN-OWNBR  TO LSM-BRNO.
           MOVE LN-ACCTNO TO LSM-ACCTNO.
           MOVE 1         TO LSM-SEQNO.
           PERFORM READ-LSM1-FILE.
           IF IO-FG = 0
              MOVE LINE-MISC TO LSM-MISC-FIELD(15)
              PERFORM REWRITE-LSM1-FILE
           ELSE
              PERFORM CLEAR-LSM-FILE
              MOVE LN-OWNBR  TO LSM-BRNO
              MOVE LN-ACCTNO TO LSM-ACCTNO
              MOVE 1         TO LSM-SEQNO
              MOVE LINE-MISC TO LSM-MISC-FIELD(15)
              PERFORM WRITE-LSM1-FILE.

           IF IO-FG NOT = 0 
              GO TO IO-ERROR.

           ADD 1 TO RECORDS-WRITTEN.

       EXIT-CREATE-TRAILER-MISC.
           EXIT.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE ILMISC-QUEUE-POS TO Q-POS.
           MOVE ILMISC-COPIES-POS TO C-POS.
           MOVE ILMISC-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           PERFORM OPEN-BR-FILE.

           MOVE EXT-CONAME-CONAME  TO H-NAME.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.

           IF EXT-ROUTE-BUF = "00"
              MOVE "PROCESSING VERITEC MISC FILE" TO MESS
              PERFORM SEND-LEGEND.

           PERFORM OPEN-PR-FILE.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.


      *******************************************************
       EO-EXEC-AUDIT SECTION.
      *******************************************************
           MOVE WK-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "VERITECID FILE MISSING" TO MESS
              PERFORM SEND-MESS
              MOVE "X" TO EXT-EOBUF-ERRCD.

      ***********************************
       ENTER-PARAMETERS SECTION.
      ***********************************
           MOVE WK-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "VERITECID FILE MISSING" TO MESS
              PERFORM SEND-MESS
              MOVE "1>" TO VDUSTATUS
              GO TO EXIT-PARM.

       RPT-ONLY.
           MOVE "E  A010RPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS NOT = "00" GO TO RPT-ONLY.
           MOVE VDUBUF TO REPORT-ONLY.
           IF NOT (REPORT-ONLY = "Y" OR "N")
              GO TO RPT-ONLY.

       EXIT-PARM.
           EXIT.

      *************************************************
       DISPLAY-PARAMETERS SECTION.
      *************************************************
           MOVE "S  A010RPT." TO VDU.
           MOVE REPORT-ONLY TO VDUBUF.
           PERFORM SCREENIO.


      ****************************
       DETAIL-LINE-PROCESS SECTION.
      ****************************
           MOVE LINE-BRNO   TO D-BRNO.
           MOVE LINE-ACCTNO TO D-ACCTNO.
           IF HOLD-EXCEPTION NOT = SPACES
              MOVE LINE-MISC      TO D-ID
              MOVE HOLD-EXCEPTION TO D-EXCEPTION.
           MOVE SPACES TO HOLD-EXCEPTION.
           PERFORM WRITE-DETAIL-LINE.
       END-DETAIL-LN-PROCESS.
           EXIT.

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

           MOVE "N" TO FIRST-TIME.
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
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.

       END-HEAD-PAGE.
           MOVE 2 TO LN-FEED.
           MOVE 3 TO LN-COUNT.

      ****************************************
       PRINT-TOTALS SECTION.
      ****************************************

           MOVE "REPORT ONLY ?" TO R-COUNTERX.
           MOVE REPORT-ONLY     TO R-COUNTER-ALPHA.
           MOVE 4 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL RECORDS......." TO R-COUNTERX.
           MOVE RECORD-COUNTER         TO R-COUNTER.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "RECORDS UPDATED....." TO R-COUNTERX.
           MOVE RECORDS-WRITTEN      TO R-COUNTER.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL EXCEPTIONS...." TO R-COUNTERX.
           MOVE RECORD-EXCEPTIONS      TO R-COUNTER.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.


       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPLSMCL.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO ILMISC-CONAME.
           MOVE EXT-CONAME-SYSDATE TO ILMISC-SYSDATE.
           DISPLAY ILMISC-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE ILMISC-FORM-FIELDS TO WR-BUF. 
           MOVE ILMISC-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM. 


      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE ILMISC-HELP--FILE TO HELP-LINK-FILE.
           MOVE ILMISC-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/ILMISC_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ************************************
       MISC-PARAGRAPH SECTION.
      ************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       SEND-LEGEND.
           MOVE MESS TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       GET-LN-PATH.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LSM1-FILE.

           PERFORM READ-BR-FILE.
           IF IO-FG = 0
              MOVE EXT-FILPATH-BASE TO FILPATH-BASE
              MOVE BR-MACHINE       TO FILPATH-MACHINE
              MOVE BR-DATA-PATH     TO FILPATH-DATA
              MOVE "B"              TO LN-PATH-OVERRIDE
                                       LSM-PATH-OVERRIDE
           ELSE
              ADD 1 TO RECORD-EXCEPTIONS
              MOVE "*** INVALID BRANCH" TO HOLD-EXCEPTION
              PERFORM DETAIL-LINE-PROCESS
              MOVE 9 TO IO-FG.


      *********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLSMGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       OPEN-WK-FILE.
           PERFORM OPEN-IT.
           MOVE "ILMISC" TO E-FILE.
           OPEN INPUT WK-FILE.
           IF IO-FG = 8
              GO TO OPEN-WK-FILE.
           IF IO-FG = 7
              CLOSE WK-FILE
              GO TO OPEN-WK-FILE.
       READ-WK-FILE.
           PERFORM READ-IT.
           MOVE "ILMISC" TO E-FILE.
           READ WK-FILE.
           IF IO-FG = 8
              GO TO READ-WK-FILE.
       CLOSE-WK-FILE.
           PERFORM CLOSE-IT.
           CLOSE WK-FILE.
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBLP/LPLSM1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.
 
      *********************************
       END-PROGRAM SECTION.
      *********************************
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT
              MOVE "SP/EXTRMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       EXIT-PROG.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY ILMISC-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
