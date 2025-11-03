       IDENTIFICATION  DIVISION.
       PROGRAM-ID.     BWDUP.
       DATE-WRITTEN.   2009/09/17.
      ******************************************************************
      *  WORLD: READ LOANS BY SSNO IN 1 BRANCH, LOOK FOR BORROWERS
      *         WITH MORE THAN 1 LOAN, INCLUDING COMAKERS & ENDORSERS.
      *
      *  *** NOTE: ***
      *     THE SORT ORDER OF THE REPORT ON S35 DOES NOT MATCH THE A15  
      *  REPORT.  A15 READS THROUGH LN4FILE IN SSNO/ACTNO ORDER AND
      *  PRINTS THE REPORT. AFTER COMPLETING LN4FILE, THE PROGRAM READS
      *  THROUGH LN5FILE TO DO THE SAME THING. BECAUSE OF THIS, THE 
      *  REPORT IS NOT IN TRUE SSNO/ACTNO ORDER.  S35 HAS A STORED 
      *  PROCEDURE THAT COMBINES BOTH LN4FILE & LN5FILE BEFORE PRODUCING
      *  DATA FOR THE REPORT. SINCE S35 IS NOT IN THE SAME ORDER AS A15,
      *  THE DECISSION WAS MADE TO SORT THE S35 REPORT IN SSNO/LOAN-DATE/
      *  ACCTNO SEQUENCE, MUCH LIKE MOST REPORTS DO.
      *  *** NOTE ***
      *
      * REV:
      *  BAH 160203 REMOVE LTFILE, PD0003
      * BAH 170808 ACTIVATE 8 DIGIT DATES, PD0006
      * KEC 2018.0514 PR#00000 PARADATA A30 JAY & BETH
      *         REMOVED USE OF ENT-GROUP AND/OR GROUP-FLAG.
      *         THIS WAS DONE IN RELATION TO THE EOBUF EXTERNAL ISSUE
      *         WHICH IF NOT BEING SET WILL CAUSE ISSUES FROM THOSE
      *         BYTE POSITIONS COULD BE SET FROM PREVIOUS
      *         PROGRAM WITH EOBUF THAT SET SOMETHING IN THOSE BYTES.
      * BAH 181012 GLOBAL BWFILE
      * BAH 181212 GLOBAL LNFILE
      * BAH 20191004 REMOVED ACCESS-CALL LNFILE
      * BAH 20220815 HARD CODED START
      * BAH 20220815 HARD CODED START
      * BAH 20230714 COMMENT OUT 3RD AND 4TH BORROWER TESTS, LN6 AND LN7
      * RMZ 20230927 PERFORMANCE TUNING - ADDED STORED PROCEDURE
      *  BAH 2024.0815 PRINT LAST 4 OF SSNO #1673 S35Q-118
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(40) VALUE
           "@(#)SP/BWDUP.CBL S35 02/22/24-08:16:56 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".

      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.      
       01  QWS-D-2-LOANS        PIC 9(8) VALUE 0.
       01  QWS-D-3-LOANS        PIC 9(8) VALUE 0.
       01  QWS-D-4-PLUS-LOANS   PIC 9(8) VALUE 0. 
       01  QWS-D-TOTAL-LOANS    PIC 9(8) VALUE 0. 
       01  QWS-D-WITH-PL-LOANS  PIC 9(8) VALUE 0.   
       EXEC SQL END DECLARE SECTION END-EXEC.

       01  PR-IFN               PIC XX   VALUE "PR".
       01  BW-CONN-OPEN-SW      PIC X    VALUE "N".
       01  FIRST-BW-SW          PIC X    VALUE "Y".

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BRANCH               PIC 9(4).

       01  SV-BRANCH            PIC 9(4)     VALUE 9999.
       01  VERSION-CONTROL      PIC X VALUE "I".
       01  REMAINDER-WK         PIC S9(12) VALUE 0.
       01  RESULT-WK            PIC S9(9).


       01  LN-WORKERS.
           03  LN-FEED          PIC 99     VALUE 0.
           03  LN-COUNT         PIC 999    VALUE 99.
           03  LN-WK            PIC 999    VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)   VALUE 0.

       01  PL-COUNT             PIC 9(9)   VALUE 0.
       01  LOAN-COUNT           PIC S9(9)  VALUE +0.
       01  READ-COUNT           PIC S9(9)  VALUE +0.
       01  SSNO-SUB             PIC 99     VALUE 0.
       01  MESS-LIST            PIC X(5)   VALUE "MESS.".
       01  PREV-LN-SSNO         PIC 9(9)   VALUE 0.

       01  DISPLAY-LIST         PIC X(4) VALUE "MES.".
       01  DISPLAY-MESS.
           03  FILLER           PIC X(15) VALUE
               "NOW PROCESSING:".
           03  FILLER           PIC X(1)   VALUE " ".
           03  FILLER           PIC X(7)   VALUE "XXX-XX-".
           03  DISPLAY-SSNO     PIC 9999.
           03  FILLER           PIC X(34)   VALUE " ".

       01  TOTAL-TABLE.
          03  TOTAL-1-LOAN         PIC 9(08).
          03  TOTAL-2-LOANS        PIC 9(08).
          03  TOTAL-3-LOANS        PIC 9(08).
          03  TOTAL-4-PLUS-LOANS   PIC 9(08).
          03  TOTAL-WITH-PL-LOANS  PIC 9(08).
          03  TOTAL-LOANS          PIC 9(08).

       01  HEAD1.
           03  H-DATE               PIC X(8) VALUE SPACES.
           03  FILLER               PIC XX   VALUE SPACES.
           03  H-HR                 PIC 99   VALUE 0.
           03  FILLER               PIC X    VALUE ":".
           03  H-MIN                PIC 99   VALUE 0.
           03  FILLER               PIC X(23) VALUE SPACES.
           03  H-NAME               PIC X(40).
           03  FILLER               PIC X(38).
           03  FILLER               PIC X(5)  VALUE "PAGE ".
           03  H-PAGE-NO            PIC ZZZ9.
       01  HEAD2.
           03  FILLER               PIC X(10) VALUE "USER ID:".
           03  H-USERID             PIC X(10).
           03  FILLER               PIC X(20) VALUE SPACES.
           03  FILLER               PIC X(43) VALUE
               "BRANCH BORROWER DUPLICATE LOAN REPORT FOR: ".
           03  H-BRNO               PIC 9(4).

       01  HEAD3.
           03  FILLER               PIC X(54) VALUE
           "BRNO  SSNO         BORROWER NAME".
           03  FILLER               PIC X(41) VALUE
           "ACCT #               DATE         BALANCE".
           03  FILLER               PIC X(26) VALUE
           "    PLCD  MAKER CONV  WCBC".

       01  TOTAL-HEAD.
           03  FILLER               PIC X(32) VALUE
           " 2 LOANS     3 LOANS    4+ LOANS".
           03  FILLER               PIC X(23) VALUE
           "    TOTAL  1+LOAN & P&L".

       01  DETAIL-LINE              PIC X(132).
       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-BRNO               PIC 9(04).
           03  FILLER               PIC X(02).
           03  D-SSNO5              PIC X(7).
           03  D-SSNO               PIC 9999.
           03  FILLER               PIC X(02).
           03  D-LNAME              PIC X(17).
           03  D-FNAME              PIC X(16).
           03  FILLER               PIC X(02).
           03  D-ACCTNO             PIC X(20).
           03  D-LNDATE             PIC 99/99/99.
           03  FILLER               PIC X.
           03  D-CURBAL             PIC ZZZZZ,ZZZ.99.
           03  FILLER               PIC X(05).
           03  D-PLCD               PIC X.
           03  FILLER               PIC X(05).
           03  D-MAKERCD            PIC X.
           03  FILLER               PIC X(05).
           03  D-CONVERCD           PIC X.
           03  FILLER               PIC X(05).
           03  D-GIFTNET            PIC X.
       01  D-LINE2 REDEFINES DETAIL-LINE.
           03  D-2-LOANS            PIC ZZZZ,ZZZ.
           03  FILLER               PIC X(03).
           03  D-3-LOANS            PIC ZZZZ,ZZZ.
           03  FILLER               PIC X(03).
           03  D-4-PLUS-LOANS       PIC ZZZZ,ZZZ.
           03  FILLER               PIC X(03).
           03  D-TOTAL-LOANS        PIC ZZZZ,ZZZ.
           03  FILLER               PIC X(03).
           03  D-WITH-PL-LOANS      PIC ZZZZ,ZZZ.
           03  FILLER               PIC X(03).

       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/BWDUP_DEF.CPY".
       COPY "LIBSP/BWDUP_WKS.CPY".
       COPY "LIBLP/ARRAYSPW.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/BWDUP_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
            PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
      *    PERFORM CLOSE-LN7-FILE.
      *    PERFORM CLOSE-LN6-FILE.
           PERFORM CLOSE-LN5-FILE.
           PERFORM CLOSE-LN4-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-BW1-CONNECTION.
           PERFORM CLOSE-BR-FILE.
           CLOSE
            PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ****************************************
       MAIN-MODULE SECTION.
      ****************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BWDUP" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           MOVE BRANCH TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG = 9
              GO TO END-ROUTINE.

           IF EXT-ROUTE-BUF = "00"
              MOVE SPACES TO WR-BUF
              STRING "REPORT IN PROGRESS.............", BR-NO
                     DELIMITED "  " INTO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           MOVE "B"          TO BW-PATH-OVERRIDE
                                LN-PATH-OVERRIDE.

      *    PERFORM OPEN-SP1-FILE.

           PERFORM OPEN-LN4-FILE.
           PERFORM OPEN-BW1-FILE.

           MOVE BR-NO TO BW-OWNBR.
           MOVE 0     TO BW-SSNO.
           MOVE "Y"   TO FIRST-BW-SW. 

           PERFORM START-TOTAL-BW1-FILE. *> SSP - BWFILE
           PERFORM READ-TOTAL-BW1-FILE-NEXT. *> EMBEDDED - BWFILE
           PERFORM CLOSE-TOTAL-BW1-FILE.

           MOVE QWS-D-2-LOANS       TO TOTAL-2-LOANS.     
           MOVE QWS-D-3-LOANS       TO TOTAL-3-LOANS.      
           MOVE QWS-D-4-PLUS-LOANS  TO TOTAL-4-PLUS-LOANS. 
           MOVE QWS-D-TOTAL-LOANS   TO TOTAL-LOANS.   
           MOVE QWS-D-WITH-PL-LOANS TO TOTAL-WITH-PL-LOANS.
       
           PERFORM START-BW1-FILE. *> SSP - BWFILE

       NEXT-BORROWER.

           IF FIRST-BW-SW = "Y"
               MOVE "N" TO FIRST-BW-SW
               PERFORM READ-BW1-FILE-NEXT. *> EMBEDDED - BWFILE

           IF (IO-FG = 9) OR (BW-OWNBR NOT = BW-PATH-OWNBR)
              GO TO END-ROUTINE.

           ADD 1 TO READ-COUNT.
      *    DIVIDE READ-COUNT BY 1500 GIVING RESULT-WK
      *       REMAINDER REMAINDER-WK.
      *    IF REMAINDER-WK = 0
      *       MOVE BW-SSNO TO DISPLAY-SSNO
      *       MOVE DISPLAY-MESS TO WR-BUF
      *       MOVE DISPLAY-LIST TO WR-BUF
      *       PERFORM CALL-WRFORM.

      *    MOVE 0 TO LOAN-COUNT PL-COUNT.

      *    MOVE LN-PATH-OWNBR    TO LN-OWNBR
      *                             QLN4-WBEG-OWNBR
      *                             QLN4-WEND-OWNBR.
      *    MOVE BW-SSNO          TO LN-SSNO(1)
      *                             QLN4-WBEG-SSNO1
      *                             QLN4-WEND-SSNO1.

      *    PERFORM START-LN4-FILE.
      *NEXT-LOAN-KEY4.
      *    PERFORM READ-LN4-FILE-NEXT.
      *    IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
      *               OR (LN-SSNO(1) NOT = BW-SSNO)
      *       GO TO LN-FILE5.

      *    IF LN-POFFDATE NOT = 0
      *       GO TO NEXT-LOAN-KEY4.
      *    IF LN-POCD-NONLOAN
      *       GO TO NEXT-LOAN-KEY4.

      *    ADD 1 TO LOAN-COUNT.
      *    IF LN-PLDATE NOT = 0
      *       ADD 1 TO PL-COUNT.
      *    GO TO NEXT-LOAN-KEY4.

      *LN-FILE5.
      *    MOVE LN-PATH-OWNBR    TO LN-OWNBR
      *                             QLN5-WBEG-OWNBR
      *                             QLN5-WEND-OWNBR.
      *    MOVE BW-SSNO          TO LN-SSNO(2)
      *                             QLN5-WBEG-SSNO2
      *                             QLN5-WEND-SSNO2.
      *    PERFORM START-LN5-FILE.
      *NEXT-LOAN-KEY5.
      *    PERFORM READ-LN5-FILE-NEXT.
      *    IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
      *               OR (LN-SSNO(2) NOT = BW-SSNO)
      *       GO TO LN-FILE6.
      *       GO TO TEST-BORROWER.

      *    IF LN-POFFDATE NOT = 0
      *       GO TO NEXT-LOAN-KEY5.
      *    IF LN-POCD-NONLOAN
      *       GO TO NEXT-LOAN-KEY5.

      *    ADD 1 TO LOAN-COUNT.
      *    IF LN-PLDATE NOT = 0
      *       ADD 1 TO PL-COUNT.

      *    GO TO NEXT-LOAN-KEY5.

      *LN-FILE6.
      *    MOVE LN-PATH-OWNBR    TO LN-OWNBR
      *                             QLN6-WBEG-OWNBR
      *                             QLN6-WEND-OWNBR.
      *    MOVE BW-SSNO          TO LN-SSNO(3)
      *                             QLN6-WBEG-SSNO3
      *                             QLN6-WEND-SSNO3.
      *    PERFORM START-LN6-FILE.
      *NEXT-LOAN-KEY6.
      *    PERFORM READ-LN6-FILE-NEXT.
      *    IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
      *               OR (LN-SSNO(3) NOT = BW-SSNO)
      *       GO TO LN-FILE7.

      *    IF LN-POFFDATE NOT = 0
      *       GO TO NEXT-LOAN-KEY6.
      *    IF LN-POCD-NONLOAN
      *       GO TO NEXT-LOAN-KEY6.

      *    ADD 1 TO LOAN-COUNT.
      *    IF LN-PLDATE NOT = 0
      *       ADD 1 TO PL-COUNT.
      *    GO TO NEXT-LOAN-KEY6.

      *LN-FILE7.
      *    MOVE LN-PATH-OWNBR    TO LN-OWNBR
      *                             QLN7-WBEG-OWNBR
      *                             QLN7-WEND-OWNBR.
      *    MOVE BW-SSNO          TO LN-SSNO(4)
      *                             QLN7-WBEG-SSNO4
      *                             QLN7-WEND-SSNO4.
      *    PERFORM START-LN7-FILE.
      *NEXT-LOAN-KEY7.
      *    PERFORM READ-LN7-FILE-NEXT.
      *    IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
      *               OR (LN-SSNO(4) NOT = BW-SSNO)
      *       GO TO TEST-BORROWER.

      *    IF LN-POFFDATE NOT = 0
      *       GO TO NEXT-LOAN-KEY7.
      *    IF LN-POCD-NONLOAN
      *       GO TO NEXT-LOAN-KEY7.

      *    ADD 1 TO LOAN-COUNT.
      *    IF LN-PLDATE NOT = 0
      *       ADD 1 TO PL-COUNT.
      *    GO TO NEXT-LOAN-KEY7.

      *TEST-BORROWER.
      *    IF LOAN-COUNT > 1
      *       PERFORM ADD-TOTALS
      *       PERFORM PROCESS-DETAIL.

           PERFORM PROCESS-DETAIL.

           GO TO NEXT-BORROWER.

       END-ROUTINE.
           PERFORM TOTAL-ROUTINE.
           GO TO END-PROGRAM.

      ***********************************
      *ADD-TOTALS SECTION.
      ***********************************
      *    IF LOAN-COUNT = 0
      *       GO TO EXIT-ADD-TOTALS.

      *    IF PL-COUNT NOT = 0
      *       ADD 1 TO TOTAL-WITH-PL-LOANS.

      *    IF LOAN-COUNT = 1
      *       ADD 1 TO TOTAL-1-LOAN
      *    ELSE
      *    IF LOAN-COUNT = 2
      *       ADD 1 TO TOTAL-2-LOANS
      *    ELSE
      *    IF LOAN-COUNT = 3
      *       ADD 1 TO TOTAL-3-LOANS
      *    ELSE
      *    IF LOAN-COUNT > 3
      *       ADD 1 TO TOTAL-4-PLUS-LOANS.

      *EXIT-ADD-TOTALS.
      *    EXIT.

      ***********************************
       INITIALIZATION SECTION.
      ***********************************

           MOVE BWDUP-QUEUE-POS TO Q-POS.
           MOVE BWDUP-COPIES-POS TO C-POS.
           MOVE BWDUP-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS............." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.


           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-BR-FILE.

           PERFORM ARRAYSP-LOAD.

           MOVE 0 TO TOTAL-1-LOAN
                     TOTAL-2-LOANS
                     TOTAL-3-LOANS
                     TOTAL-4-PLUS-LOANS
                     TOTAL-WITH-PL-LOANS.

           IF BRANCH = 0
              MOVE BRANCH TO SV-BRANCH
              MOVE EXT-CONAME-USER-LOC TO BRANCH.

           MOVE SPACES TO DETAIL-LINE.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.
           MOVE BRANCH TO H-BRNO.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************

       ENTER-BRANCH.

           MOVE "ENNN040BRNO." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-BRANCH.

           MOVE VDUNUM0 TO BRANCH.
           IF BRANCH = 0
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************

           MOVE "SNNN040BRNO." TO VDU.
           MOVE BRANCH    TO VDUNUM0.
           PERFORM SCREENIO.

      **************************************
       PROCESS-DETAIL SECTION.
      **************************************
           MOVE 2 TO LN-FEED.
           MOVE 1 TO LOAN-COUNT.
           MOVE 1 TO SSNO-SUB.
           PERFORM PRINT-DETAIL.
           

      *    MOVE LN-PATH-OWNBR    TO LN-OWNBR
      *                             QLN4-WBEG-OWNBR
      *                             QLN4-WEND-OWNBR.
      *    MOVE BW-SSNO          TO LN-SSNO(1)
      *                             QLN4-WBEG-SSNO1
      *                             QLN4-WEND-SSNO1.
      *    PERFORM START-LN4-FILE.
       NEXT-DETAIL-KEY4.
      *    PERFORM READ-LN4-FILE-NEXT.
      *    IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
      *               OR (LN-SSNO(1) NOT = BW-SSNO)
      *       GO TO DETAIL-FILE5.

           MOVE LN-SSNO(SSNO-SUB) TO PREV-LN-SSNO.

           PERFORM READ-BW1-FILE-NEXT. *> EMBEDDED - BWFILE

           IF (IO-FG = 9) OR (LN-SSNO(1) NOT = PREV-LN-SSNO)
              GO TO END-PROCESS-DETAIL.

      *    IF LN-POFFDATE NOT = 0
      *       GO TO NEXT-DETAIL-KEY4.
      *    IF LN-POCD-NONLOAN
      *       GO TO NEXT-DETAIL-KEY4.
           ADD 1 TO LOAN-COUNT.

           PERFORM PRINT-DETAIL.
           GO TO NEXT-DETAIL-KEY4.

      *DETAIL-FILE5.
      *    MOVE 2 TO SSNO-SUB.

      *    MOVE LN-PATH-OWNBR    TO LN-OWNBR
      *                             QLN5-WBEG-OWNBR
      *                             QLN5-WEND-OWNBR.
      *    MOVE BW-SSNO          TO LN-SSNO(2)
      *                             QLN5-WBEG-SSNO2
      *                             QLN5-WEND-SSNO2.

      *    PERFORM START-LN5-FILE.
      *NEXT-DETAIL-KEY5.
      *    PERFORM READ-LN5-FILE-NEXT.
      *    IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
      *               OR (LN-SSNO(2) NOT = BW-SSNO)
      *       GO TO DETAIL-FILE6.
      *       GO TO END-PROCESS-DETAIL.

      *    IF LN-POFFDATE NOT = 0
      *       GO TO NEXT-DETAIL-KEY5.
      *    IF LN-POCD-NONLOAN
      *       GO TO NEXT-DETAIL-KEY5.
      *    ADD 1 TO LOAN-COUNT.

      *    PERFORM PRINT-DETAIL.
      *    GO TO NEXT-DETAIL-KEY5.

      *DETAIL-FILE6.
      *    MOVE 3 TO SSNO-SUB.

      *    MOVE LN-PATH-OWNBR    TO LN-OWNBR
      *                             QLN6-WBEG-OWNBR
      *                             QLN6-WEND-OWNBR.
      *    MOVE BW-SSNO          TO LN-SSNO(3)
      *                             QLN6-WBEG-SSNO3
      *                             QLN6-WEND-SSNO3.

      *    PERFORM START-LN6-FILE.
      *NEXT-DETAIL-KEY6.
      *    PERFORM READ-LN6-FILE-NEXT.
      *    IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
      *               OR (LN-SSNO(3) NOT = BW-SSNO)
      *       GO TO DETAIL-FILE7.
      *    IF LN-POFFDATE NOT = 0
      *       GO TO NEXT-DETAIL-KEY6.
      *    IF LN-POCD-NONLOAN
      *       GO TO NEXT-DETAIL-KEY6.
      *    ADD 1 TO LOAN-COUNT.
      *    PERFORM PRINT-DETAIL.
      *    GO TO NEXT-DETAIL-KEY6.

      *DETAIL-FILE7.
      *    MOVE 4 TO SSNO-SUB.

      *    MOVE LN-PATH-OWNBR    TO LN-OWNBR
      *                             QLN7-WBEG-OWNBR
      *                             QLN7-WEND-OWNBR.
      *    MOVE BW-SSNO          TO LN-SSNO(4)
      *                             QLN7-WBEG-SSNO4
      *                             QLN7-WEND-SSNO4.

      *    PERFORM START-LN7-FILE.
      *NEXT-DETAIL-KEY7.
      *    PERFORM READ-LN7-FILE-NEXT.
      *    IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
      *               OR (LN-SSNO(4) NOT = BW-SSNO)
      *       GO TO END-PROCESS-DETAIL.

      *    IF LN-POFFDATE NOT = 0
      *       GO TO NEXT-DETAIL-KEY7.
      *    IF LN-POCD-NONLOAN
      *       GO TO NEXT-DETAIL-KEY7.
      *    ADD 1 TO LOAN-COUNT.

      *    PERFORM PRINT-DETAIL.

      *    GO TO NEXT-DETAIL-KEY7.

       END-PROCESS-DETAIL.
           EXIT.

      **************************************
       PRINT-DETAIL SECTION.
      **************************************

           PERFORM GET-SP.
           PERFORM LPSTAT-DISPLAY.
           IF LOAN-COUNT = 1
              MOVE 2 TO LN-FEED
              MOVE LN-OWNBR          TO D-BRNO
              MOVE "XXX-XX-"         TO D-SSNO5
              MOVE LN-SSNO(SSNO-SUB) TO D-SSNO
              MOVE BW-LNAME          TO D-LNAME
              MOVE BW-FNAME          TO D-FNAME.
           MOVE LN-LOANDATE          TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY          TO D-LNDATE.
           MOVE LPS-DISPLAY-LNNO     TO D-ACCTNO.
           MOVE LN-CURBAL            TO D-CURBAL.
           MOVE LN-PLCD              TO D-PLCD.
           MOVE LN-MAKERCD(SSNO-SUB) TO D-MAKERCD.
           MOVE LN-CONVERCD          TO D-CONVERCD.
           MOVE LN-GIFTNET           TO D-GIFTNET.
           PERFORM WRITE-DETAIL-LINE.
       EXIT-DETAIL-ROUTINE.
           EXIT.

      **************************************
       TOTAL-ROUTINE SECTION.
      **************************************
           MOVE TOTAL-HEAD TO PR-REC.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-PR-REC.

           MOVE 2 TO LN-FEED.
           MOVE TOTAL-2-LOANS TO D-2-LOANS.
           MOVE TOTAL-3-LOANS TO D-3-LOANS.
           MOVE TOTAL-4-PLUS-LOANS TO D-4-PLUS-LOANS.
           ADD TOTAL-1-LOAN TOTAL-2-LOANS
               TOTAL-3-LOANS TOTAL-4-PLUS-LOANS
               GIVING TOTAL-LOANS.
           MOVE TOTAL-LOANS TO D-TOTAL-LOANS.
           MOVE TOTAL-WITH-PL-LOANS TO D-WITH-PL-LOANS.
           PERFORM WRITE-DETAIL-LINE.
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
           MOVE 2 TO LN-FEED.
           MOVE 3 TO LN-COUNT.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/ARRAYSP.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPMDTE.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO BWDUP-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BWDUP-SYSDATE.
           DISPLAY BWDUP-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE BWDUP-FORM-FIELDS TO WR-BUF.
           MOVE BWDUP-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.


      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE BWDUP-HELP--FILE TO HELP-LINK-FILE.
           MOVE BWDUP-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/BWDUP_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ************************************
       MISC-PARAGRAPH SECTION.
      ************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/GETENV.CPY".
       GET-SP.
           MOVE LN-ORGST    TO SP-ORGST.
           MOVE LN-SPRCLASS TO SP-SPRCLASS.
           MOVE LN-SUBCLASS TO SP-SUBCLASS.
           MOVE LN-LAWCODE  TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
      *       PERFORM READ-SP1-FILE.
              PERFORM ARRAYSP-READ.

      ********************************
       IO-ROUTINE SECTION.
      *********************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".

      * COPY "LIBLP/LPBW1RN.CPY".
      * COPYMEMBER: LIBLP/LPBW1RN
       LOAD-BW1-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( BW-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( BW-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO BW-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO BW-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO BW-ALL-DATA
              MOVE BW-ALL-PATH             TO BW-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( BW-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO BW-ALL-BASE
              MOVE FILPATH-MACHINE         TO BW-ALL-MACHINE
              MOVE FILPATH-DATA            TO BW-ALL-DATA
              MOVE BW-ALL-PATH             TO BW-PATH.

      *-----------------------------------------------------------------
       OPEN-BW1-FILE.
           PERFORM LOAD-BW1-FILE.
           PERFORM OPEN-IT.
           MOVE BW-PATH-SQL TO E-FILE.

            IF BW-CONN-OPEN-SW = "N"
              MOVE "Y" TO BW-CONN-OPEN-SW
              EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS BW_CONNECT
                    USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
           END-EXEC.

      *    IF ( SQL-CONNECT-STAT-BAD )
      *       PERFORM SQL-CONNECT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-BW1-FILE. *> SSP - BW,LN
           PERFORM START-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR.
           MOVE BW-PATH-SQL    TO E-FILE.
           MOVE BW1-KEY        TO E-KEYX.

           IF ( BW1-CURSOR-STAT-GOOD )
              PERFORM CLOSE-BW1-FILE.

           IF ( QBW1-WEND-SSNO = ZEROES    ) OR
              ( QBW1-WEND-SSNO NOT NUMERIC )
              MOVE BW-OWNBR             TO QBW1-WBEG-OWNBR
                                           QBW1-WEND-OWNBR
              MOVE BW-SSNO              TO QBW1-WBEG-SSNO
              MOVE ALL "9"              TO QBW1-WEND-SSNO.

      *  STORED PROCEDURE
      *
      *   SELECT BWFILE.BW_OWNBR,
      *          BWFILE.BW_SSNO,             
      *          BWFILE.BW_LNAME, 
      *          BWFILE.BW_FNAME,
      *          BWFILE.BW_SOLICIT
      *     FROM DBO.BWFILE BWFILE
      *    WHERE (  BWFILE.BW_OWNBR    = @QBW1_WBEG_OWNBR  )
      *    ORDER BY BWFILE.BW_OWNBR,
      *             BWFILE.BW_SSNO  
 
           EXEC SQL SET CONNECTION BW_CONNECT END-EXEC.

           EXEC SQL
            DECLARE BW1_CURSOR CURSOR FOR
                :BW1-RETURN-CODE = EXEC SSP_BWDUP_BWFILE_SELECT
              ( :QBW1-WBEG-OWNBR )                    
           END-EXEC.


           EXEC SQL
               OPEN BW1_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           MOVE ZEROES TO QBW1-WBEG-OWNBR 
                          QBW1-WEND-OWNBR
                          QBW1-WBEG-SSNO
                          QBW1-WEND-SSNO.

           MOVE STAT---GOOD TO BW1-CURSOR-STAT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-BW1-FILE-NEXT. *> EMBEDDED - BW,LN
           PERFORM READ-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR.
           MOVE BW-PATH-SQL    TO E-FILE.
           MOVE BW1-KEY        TO E-KEYX.
           INITIALIZE QBW-REC.
           INITIALIZE QLN-REC.

           EXEC SQL SET CONNECTION BW_CONNECT END-EXEC.

           IF ( BW1-CURSOR-STAT-GOOD )
              EXEC SQL
                   FETCH BW1_CURSOR INTO 
                     :QBW-OWNBR,
                     :QBW-SSNO,             
                     :QBW-LNAME, 
                     :QBW-FNAME,
                     :QBW-SOLICIT,
                     :QLN-OWNBR,
                     :QLN-ACCTNO,
                     :QLN-CLASS,
                     :QLN-SUBCLASS,
                     :QLN-LAWCODE,
                     :QLN-SSNO1,
                     :QLN-MAKERCD1,
                     :QLN-ORGST,
                     :QLN-SPRCLASS,
                     :QLN-CONVERCD,
                     :QLN-POCD, 
                     :QLN-PLCD, 
                     :QLN-STATFLAGS, 
                     :QLN-LOANDATE,         
                     :QLN-ORIG-1STPYDATE,   
                     :QLN-1STPYDATE,        
                     :QLN-PLDATE,           
                     :QLN-POFFDATE,         
                     :QLN-ORGTERM,                             
                     :QLN-CURBAL,
                     :QLN-TOTNODEF,
                     :QLN-SUM-ORGTERM,
                     :QLN-UNITPER-CD,
                     :QLN-UNITPER-FREQ,
                     :QLN-GIFTNET
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-BW1-FILE-NEXT.

           IF ( IO-FG = 0 )
              PERFORM GET-BW-FIELDS
              PERFORM GET-LN-FIELDS.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-BW1-FILE.

           PERFORM CLOSE-IT.
           IF ( BW1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION BW_CONNECT END-EXEC
              EXEC SQL
                   CLOSE BW1_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC 
              MOVE STAT---BAD TO BW1-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-BW1-CONNECTION.

           IF BW-CONN-OPEN-SW = "Y"
              EXEC SQL SET CONNECTION BW_CONNECT END-EXEC
              EXEC SQL COMMIT END-EXEC
              EXEC SQL DISCONNECT BW_CONNECT END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE "N" TO BW-CONN-OPEN-SW.

      *-----------------------------------------------------------------
       START-TOTAL-BW1-FILE. *> SSP - BW,LN TOTALS

           PERFORM START-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR.
           MOVE BW-PATH-SQL    TO E-FILE.
           MOVE BW1-KEY        TO E-KEYX.

           MOVE BR-NO TO QBW1-WBEG-OWNBR.

      *  STORED PROCEDURE:
      *
      *   SELECT C.QWS_D_2_LOANS,
      *          C.QWS_D_3_LOANS,
      *          C.QWS_D_4_PLUS_LOANS,
      *          C.QWS_D_TOTAL_LOANS,
      *          C.QWS_D_WITH_PL_LOANS
      * 
      *     FROM 
      *     ( SELECT SUM(CASE WHEN B.CNT  = 2
      *                       THEN 1
      *                       ELSE 0
      *                   END)  AS QWS_D_2_LOANS,
      *              SUM(CASE WHEN B.CNT  = 3
      *                       THEN 1
      *                       ELSE 0
      *                   END)  AS QWS_D_3_LOANS,
      *              SUM(CASE WHEN B.CNT  > 3
      *                       THEN 1
      *                       ELSE 0
      *                   END)  AS QWS_D_4_PLUS_LOANS,
      *              SUM(CASE WHEN B.CNT  > 1
      *                       THEN 1
      *                       ELSE 0
      *                   END)  AS QWS_D_TOTAL_LOANS,
      *              SUM(CASE WHEN PL_IND = 'Y'
      *                       THEN 1
      *                       ELSE 0
      *                   END)  AS QWS_D_WITH_PL_LOANS
      *       FROM
      *     ( SELECT A.SSNO,
      *              SUM(A.CNT)       AS CNT,
      *              MAX(A.PL_IND)    AS PL_IND
      *         FROM
      *     ( SELECT BWFILE.BW_SSNO  AS SSNO,
      *              1               AS CNT,
      *              CASE WHEN ISNULL(LNFILE.LN_PLDATE, '1900-01-01')
      *                                             !=  '1900-01-01'
      *                   THEN 'Y'
      *                   ELSE 'N'
      *               END            AS PL_IND
      *         FROM DBO.BWFILE BWFILE
      *         LEFT OUTER JOIN
      *              DBO.LNFILE LNFILE
      *           ON (( BWFILE.BW_OWNBR       = LNFILE.LN_OWNBR
      *          AND    BWFILE.BW_SSNO        = LNFILE.LN_SSNO1 )
      *          AND (  LNFILE.LN_POFFDATE    = '1900-01-01' )
      *          AND (  LNFILE.LN_POCD   NOT IN
      *              ('**', '*I', '*R', '*U') ))
      *        WHERE (  BWFILE.BW_OWNBR       = :QBW1-WBEG-OWNBR
      *          AND    ISNULL(LNFILE.LN_SSNO1, 0)
      *                                 != 0 )
      *        UNION ALL
      * 
      *       SELECT BWFILE.BW_SSNO  AS SSNO,
      *              1               AS CNT,
      *              CASE WHEN ISNULL(LNFILE.LN_PLDATE, '1900-01-01')
      *                                             !=  '1900-01-01'
      *                   THEN 'Y'
      *                   ELSE 'N'
      *               END            AS PL_IND
      *         FROM DBO.BWFILE BWFILE
      *         LEFT OUTER JOIN
      *              DBO.LNFILE LNFILE
      *           ON (( BWFILE.BW_OWNBR       = LNFILE.LN_OWNBR
      *          AND    BWFILE.BW_SSNO        = LNFILE.LN_SSNO2 )
      *          AND (  LNFILE.LN_POFFDATE    = '1900-01-01' )
      *          AND (  LNFILE.LN_POCD   NOT IN
      *              ('**', '*I', '*R', '*U') ))
      *        WHERE (  BWFILE.BW_OWNBR       = :QBW1-WBEG-OWNBR
      *          AND    ISNULL(LNFILE.LN_SSNO2, 0)
      *                                      != 0 ) ) A
      *        GROUP BY A.SSNO
      *       HAVING SUM(A.CNT)               > 1   ) B
      *                                        ) C   
        
           EXEC SQL SET CONNECTION BW_CONNECT END-EXEC.
        
           EXEC SQL
           DECLARE BW1_TOTAL_CURSOR CURSOR FOR
              :BW1-RETURN-CODE = EXEC SSP_BWDUP_TOTAL_BWFILE_SELECT
            ( :QBW1-WBEG-OWNBR )                    
           END-EXEC.

           EXEC SQL
               OPEN BW1_TOTAL_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       READ-TOTAL-BW1-FILE-NEXT. *> EMBEDDED - BWFILE TOTALS

           PERFORM READ-IT.
           MOVE BW-PATH-OWNBR  TO BW-OWNBR.
           MOVE BW-PATH-SQL    TO E-FILE.
           MOVE BW1-KEY        TO E-KEYX.

           EXEC SQL SET CONNECTION BW_CONNECT END-EXEC.
 
           EXEC SQL
                FETCH BW1_TOTAL_CURSOR 
               INTO 
                 :QWS-D-2-LOANS,
                 :QWS-D-3-LOANS,
                 :QWS-D-4-PLUS-LOANS,
                 :QWS-D-TOTAL-LOANS,
                 :QWS-D-WITH-PL-LOANS
           END-EXEC.
              
           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-TOTAL-BW1-FILE-NEXT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       CLOSE-TOTAL-BW1-FILE.

           PERFORM CLOSE-IT.
           IF ( BW1-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION BW_CONNECT END-EXEC
              EXEC SQL
                   CLOSE BW1_TOTAL_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC 
              MOVE STAT---BAD TO BW1-CURSOR-STAT.

      *----------------------------------------------------------------- 
       
       COPY "LIBLP/LPLN4RN.CPY".
       COPY "LIBLP/LPLN5RN.CPY".
      *COPY "LIBLP/LPLN6RN.CPY".
      *COPY "LIBLP/LPLN7RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      *********************************
       END-PROGRAM SECTION.
      *********************************
       EXIT-PROG.
           PERFORM CLOSE-FILES.
           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF SV-BRANCH NOT = 9999
              MOVE SV-BRANCH TO BRANCH.
           IF EXT-ROUTE-BUF = "00"
              MOVE "LP/LPRPMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY BWDUP-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
