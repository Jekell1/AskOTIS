       IDENTIFICATION     DIVISION.
       PROGRAM-ID.          SKIPTW.
       DATE-WRITTEN.    12/23/2014.
      ******************************************************************
      *
      * SET LN-SKIP-TRW TO "Y" FROM FILE
      *
      * INPUT FILE = ../../SP/SKIPTW(GLOBAL SP)
      * MOVES FILE TO SKIPTW.YYMMDD WHEN ALL DONE
      *
      *
      * * * NOTE: WORLD USES THIS AS PART OF THE MONTHLY DEBT SALE PROCESS
      *           5/17/2023 RANDY
      *
      * REV:
      *  141223 BAH MIDATL, PR# 4674
      *             REGACC PR# 1708
      *  BAH 160218 EXPANDED LM-REC ADDED LM-BRNO, PD0003
      * 20171106 BAH ACTIVATE 8 DIGIT DATES, PD0006
      *
      * KEC 2018.0425 PR#00000 PARADATA A30 JAY & BETH
      *         REMOVED USE OF ENT-GROUP AND/OR GROUP-FLAG.
      *         THIS WAS DONE IN RELATION TO THE EOBUF EXTERNAL ISSUE
      *         WHICH IF NOT BEING SET WILL CAUSE ISSUES FROM THOSE
      *         BYTE POSITIONS COULD BE SET FROM PREVIOUS
      *         PROGRAM WITH EOBUF THAT SET SOMETHING IN THOSE BYTES.
      *
      * BAH 181127 GLOBAL LMFILE
      * BAH 181203 GLOBAL LNFILE
      * BAH 190918 REMOVED ACCESS-CALL ON LNFILE, LMFILE
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
           SELECT SKIPTW-FILE ASSIGN SKIPTW-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  ACCESS SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.

       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
 
       COPY "LIBGB/GBFDPR.CPY".

       FD  SKIPTW-FILE
           LABEL RECORDS ARE STANDARD.
       01  SKIPTW-REC.
           03  SKIPTW-BRNO                    PIC 9(4).
           03  SKIPTW-BRNOX REDEFINES SKIPTW-BRNO PIC X(4).
           03  SKIPTW-NUMBER                  PIC 9(6).
           03  SKIPTW-NUMBERX REDEFINES SKIPTW-NUMBER PIC X(6).


       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)FX/SKIPTW.CBL S34 05/21/21-14:47:35 >".
       COPY "LIBGB/GB01GB.CPY".
       COPY "LIBGB/GB01GB_SQL.CPY".
       COPY "LIBGB/GBWSGB.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01LM.CPY".
       COPY "LIBLP/LP01LM_SQL.CPY".
       COPY "LIBLP/LPWSLM.CPY".
       01  SKIPTW-PATH.
           03  SKIPTW-PATH-BASE     PIC X(9).
           03  FILLER               PIC X VALUE "/".
           03  SKIPTW-PATH-MACHINE  PIC X(2).
           03  FILLER               PIC X(10) VALUE "/SP/SKIPTW".

       01  SKIPTW-SAVE-PATH.
           03  SKIPTW-SAVE-BASE     PIC X(9).
           03  FILLER               PIC X VALUE "/".
           03  SKIPTW-SAVE-MACHINE  PIC X(2).
           03  FILLER               PIC X(11) VALUE "/SP/SKIPTW.".
           03  SKIPTW-SAVE-DATE     PIC 9(6).
           

       01  HOLD-BRNO            PIC 9(4) VALUE 0.
       01  HOLD-SYSDATE         PIC 9(8).
      *-----------------------------------------------------------------
      * LINE WORKERS
      *-----------------------------------------------------------------
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.
 
       01  WS-MEMO.
           03  FILLER        PIC X(35) VALUE 
               "SKIPTW FIX - SET LN-SKIP-TRW TO 'Y'".

       01  BR-TOTAL-RECS        PIC 9(10)       VALUE 0.
       01  GRAND-TOTAL-RECS     PIC 9(10)       VALUE 0.

      *-----------------------------------------------------------------
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BR          PIC 9(04).
           03  END-BR          PIC 9(04).
       01  VERSION-CONTROL     PIC X    VALUE "A".

       01  MESS-LIST            PIC X(05) VALUE "MESS.".

       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.

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
           "   SET LN-SKIP-TRW TO '   '      ".
       01  HEAD3.
           03  FILLER           PIC X(04)    VALUE "BRNO".
           03  FILLER           PIC X(06)    VALUE SPACES.
           03  FILLER           PIC X(13)    VALUE "ACCTNO".
 
 
       01  DETAIL-LINE          PIC X(80) VALUE SPACES.
       01  DETAIL-LINE1 REDEFINES DETAIL-LINE.
           03  D-BRNO           PIC Z(4).
           03  FILLER           PIC X(2).
           03  D-NUMBER         PIC Z(9).
           03  FILLER           PIC X(2).
           03  D-MSG1           PIC X(40).

       01  DETAIL-LINE2 REDEFINES DETAIL-LINE.
           03  D-TOTAL-BRNO     PIC Z(4).
           03  FILLER           PIC X(2).
           03  D-TOTAL-TAG      PIC X(15).
           03  FILLER           PIC X(08).                 
           03  D-TOTAL-REC      PIC Z(8)9.

       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  RANGE-SELECTION  PIC X(30).
           03  FILLER           PIC X(05).
           03  RANGE1N          PIC Z(9)9.
 
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
 
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBFX/SKIPTW_DEF.CPY".
       COPY "LIBFX/SKIPTW_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ****************************************
      *     P R O G R A M   C O N T R O L    *
      ****************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBFX/SKIPTW_SCN.CPY".

       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE SKIPTW-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-LM1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE SKIPTW-FILE.
       COPY "LIBGB/DECLRP2.CPY".
 
      ********************************************
       MAIN-MODULE SECTION.
      ********************************************

           PERFORM SQL-CONNECT.

           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "SKIPTW" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE EXT-FILPATH-BASE    TO SKIPTW-PATH-BASE
                                       SKIPTW-SAVE-BASE.
           MOVE EXT-FILPATH-MACHINE TO SKIPTW-PATH-MACHINE
                                       SKIPTW-SAVE-MACHINE.
           MOVE SKIPTW-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              MOVE "SKIPTW UPLOAD FILE NOT AVAILABLE" TO MESS
              PERFORM SEND-MESS
              GO TO END-PROGRAM.

           IF  EXT-ROUTE-BUF = "00"
              MOVE "PROCESSING SKIPTW UPLOAD" TO MESS
              PERFORM SEND-LEGEND.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-PR-FILE.
           MOVE 0 TO HOLD-BRNO.

           PERFORM OPEN-SKIPTW-FILE.
   
       NEXT-SKIPTW.
           PERFORM READ-SKIPTW-FILE-NEXT.
           IF IO-FG NOT = 0 OR SKIPTW-BRNO > END-BR
              PERFORM CLOSE-LN1-FILE
              PERFORM CLOSE-LM1-FILE
              PERFORM PRINT-BR-TOTALS
              GO TO END-ROUTINE.

           IF SKIPTW-BRNO < BEG-BR
              GO TO NEXT-SKIPTW.


           INSPECT SKIPTW-BRNOX REPLACING ALL " " BY "0".
           IF SKIPTW-BRNOX NOT NUMERIC
              GO TO NEXT-SKIPTW.

           INSPECT SKIPTW-NUMBERX REPLACING ALL " " BY "0".
           IF SKIPTW-NUMBERX NOT NUMERIC
              GO TO NEXT-SKIPTW.

              
           IF SKIPTW-BRNO NOT = HOLD-BRNO
              IF HOLD-BRNO NOT = 0
                 PERFORM CLOSE-LN1-FILE
                 PERFORM CLOSE-LM1-FILE
                 PERFORM PRINT-BR-TOTALS
              END-IF
              PERFORM GET-BR-SET-PATHS
              IF IO-FG NOT = 0
                 MOVE 0 TO HOLD-BRNO
                 GO TO NEXT-SKIPTW.

              
           MOVE SKIPTW-BRNO   TO LN-OWNBR.
           MOVE SKIPTW-NUMBER TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-FG NOT = 0
              MOVE BR-NO TO D-BRNO
              MOVE SKIPTW-NUMBER TO D-NUMBER
              MOVE "INVALID ACCTNO" TO D-MSG1
              PERFORM WRITE-DETAIL-LINE
              GO TO NEXT-SKIPTW.

      *    -------------------------------------------------------------
      *    DELETED LOAN, RESCISSION
      *    -------------------------------------------------------------
           IF LN-POCD-NONLOAN
              MOVE BR-NO TO D-BRNO
              MOVE LN-ACCTNO TO D-NUMBER
              MOVE "ACCOUNT VOIDED" TO D-MSG1
              PERFORM WRITE-DETAIL-LINE
              GO TO NEXT-SKIPTW.


           MOVE "Y" TO LN-SKIP-TRW.
           PERFORM REWRITE-LN1-FILE.

           MOVE BR-NO TO D-BRNO.
           MOVE LN-ACCTNO TO D-NUMBER.
           PERFORM WRITE-DETAIL-LINE.

           PERFORM UPDATE-LM-FILE.

           ADD 1 TO BR-TOTAL-RECS GRAND-TOTAL-RECS.

           GO TO NEXT-SKIPTW.
 
       END-ROUTINE.
           MOVE SKIPTW-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT = "00"
              MOVE HOLD-SYSDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-YYMMDD
              MOVE DATE-YYMMDD TO SKIPTW-SAVE-DATE
              MOVE SPACES TO SYSTEM-BUF
              STRING MV-CMD," ",SKIPTW-PATH," ",SKIPTW-SAVE-PATH
                     DELIMITED BY "  " INTO SYSTEM-BUF
              PERFORM SYSTEM-CALL.
           PERFORM PRINT-GRAND-TOTALS.
           PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.

      *******************************
       UPDATE-LM-FILE SECTION.
      *******************************

           MOVE LN-OWNBR  TO LM-BRNO.
           MOVE LN-ACCTNO TO LM-ACCTNO.
           SUBTRACT HOLD-SYSDATE FROM 99999999 GIVING LM-DATE.
           MOVE 1 TO LM-SEQ.

           MOVE WS-MEMO TO LM-MEMO.
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
                 PERFORM WRITE-DETAIL-LINE.

      ***********************************
      *    I N I T I A L I Z A T I O N
      ***********************************
       INITIALIZATION SECTION.
           MOVE SKIPTW-QUEUE-POS TO Q-POS.
           MOVE SKIPTW-COPIES-POS TO C-POS.
           MOVE SKIPTW-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".
 
           IF BEG-BR = 0
              IF END-BR = 0
                 MOVE BEG-BR TO SV-BEG-BR
                 MOVE END-BR TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BR
                                             END-BR.
           MOVE 0 TO BR-TOTAL-RECS GRAND-TOTAL-RECS.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE EXT-CONAME-SYSDATE TO H-DATE ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO HOLD-SYSDATE.

           GO TO INITIALIZE-EXIT.

       INITIALIZE-ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       INITIALIZE-EXIT.
           EXIT.

      *******************************************************
      *    SETUP DATES FOR BATCH PROCESS
      *******************************************************
       EO-EXEC-AUDIT SECTION.

      *******************************************************
      *    ENTER REPORT PARAMETERS
      *******************************************************
       ENTER-PARAMETERS SECTION.
       ENTER-PARAM.
      * FOR RE-ENTRY ON F4 FROM ACCEPT.
           IF VDUSTATUS = "1U"
              GO TO BR-02.
       BR-01.
           MOVE "ENNN040BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-01.
           MOVE VDUNUM0 TO BEG-BR.
       BR-02.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
           MOVE VDUNUM0 TO END-BR.
           IF BEG-BR > END-BR
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           GO TO EXIT-PARM.
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BR VDUNUM0.
           PERFORM SCREENIO.
      *----------------------------------------------------------------

       EXIT-PARM.
           EXIT.

      *************************************************
      *   DISPLAY REPORT PARAMETERS
      *************************************************
       DISPLAY-PARAMETERS SECTION.
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BR TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BR TO VDUNUM0
              PERFORM SCREENIO.

      *************************************
      *      PRINT TOTALS 
      *************************************
       PRINT-BR-TOTALS SECTION.
           MOVE 2 TO LN-FEED.
           MOVE BR-NO TO D-TOTAL-BRNO.
           MOVE "BRANCH TOTAL: " TO D-TOTAL-TAG.
           MOVE BR-TOTAL-RECS TO D-TOTAL-REC.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 2 TO LN-FEED.

      *************************************
      *      PRINT GRAND TOTALS 
      *************************************
       PRINT-GRAND-TOTALS SECTION.
           MOVE 2 TO LN-FEED.
           MOVE "GRAND TOTAL :" TO D-TOTAL-TAG.
           MOVE GRAND-TOTAL-RECS TO D-TOTAL-REC.
           PERFORM WRITE-DETAIL-LINE.

      *******************************
      *       RANGE LINES           *
      *******************************
       PRINT-RANGE-LINES SECTION.
           MOVE 2 TO LN-FEED.
              MOVE "BEGINNING BRANCH     "          TO RANGE-SELECTION
              MOVE BEG-BR                           TO RANGE1N
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING    BRANCH     "          TO RANGE-SELECTION
              MOVE END-BR                           TO RANGE1N
              PERFORM WRITE-DETAIL-LINE.
 
           
      ****************************
      *    WRITE DETAIL LINE     *
      ****************************
       WRITE-DETAIL-LINE SECTION.
       WRITE-DET-LINE.
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.
 
      *******************************
      *    HEAD PAGE ROUTINE        *
      *******************************
       HEAD-PAGE SECTION.
       HEAD-ROUTINE.
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
 
      *********************************************
       FORM-RESET SECTION.
      *********************************************
           MOVE EXT-CONAME-CONAME TO SKIPTW-CONAME.
           MOVE EXT-CONAME-SYSDATE TO SKIPTW-SYSDATE.
           DISPLAY SKIPTW-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE SKIPTW-FORM-FIELDS TO WR-BUF.
           MOVE SKIPTW-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.


      *********************************************
       SETUP-LINK-INFO SECTION.
      *********************************************
           MOVE SKIPTW-HELP--FILE TO HELP-LINK-FILE.
           MOVE SKIPTW-HELP--DIR  TO HELP-LINK-DIR.

      *********************************************
       VDU-CONVERT-FIELD SECTION.
      *********************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE  
              COPY "LIBFX/SKIPTW_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/SCREEN.CPY".
 
      **************************************
      *    M I S C   P A R A G R A P H S   *
      **************************************
       MISC-PARA SECTION.
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
           MOVE SKIPTW-BRNO TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG = 0
              MOVE EXT-FILPATH-BASE TO FILPATH-BASE
              MOVE BR-MACHINE       TO FILPATH-MACHINE
              MOVE BR-DATA-PATH     TO FILPATH-DATA

              MOVE "B" TO LN-PATH-OVERRIDE
                          LM-PATH-OVERRIDE
              MOVE SKIPTW-BRNO TO HOLD-BRNO 
              PERFORM OPEN-LN1-FILE
              PERFORM OPEN-LM1-FILE.


           MOVE 0 TO BR-TOTAL-RECS.

 
      **************************************************
       IO-ROUTINES SECTION.
      **************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLMGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       OPEN-SKIPTW-FILE.
           PERFORM OPEN-IT.
           MOVE SKIPTW-PATH TO E-FILE.
           OPEN INPUT SKIPTW-FILE.
           IF IO-FG = 8
              GO TO OPEN-SKIPTW-FILE.
           IF IO-FG = 7
              CLOSE SKIPTW-FILE
              GO TO OPEN-SKIPTW-FILE.
       READ-SKIPTW-FILE-NEXT.
           PERFORM READ-IT.
           MOVE SKIPTW-PATH TO E-FILE.
           READ SKIPTW-FILE NEXT.
           IF IO-FG = 8
              GO TO READ-SKIPTW-FILE-NEXT.
       CLOSE-SKIPTW-FILE.
           PERFORM CLOSE-IT.
           CLOSE SKIPTW-FILE.
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPLN1IN.CPY".
       COPY "LIBLP/LPLM1IN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.
 
       END-PROGRAM SECTION.
       END-RUN.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

       EXIT-PROG.
           IF SV-BEG-BR NOT = 9999
              MOVE SV-BEG-BR TO BEG-BR
              MOVE SV-END-BR TO END-BR.
           IF EXT-ROUTE-BUF = "00"
              MOVE "FX/FXMENU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY SKIPTW-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
