       IDENTIFICATION  DIVISION.
       PROGRAM-ID.       BIDUP.
       AUTHOR.              MJD.
       DATE-WRITTEN. 2000/12/18.
      *******************************************************************
      *    WORLD: READ BI, LOOK FOR SSNO'S WITH MULTIPLE P&L ACCOUNTS
      *
      * REV:
      * BLF 070213 FIX TOTAL HEADINGS PRINTING BEFORE PAGE HEADINGS IF
      *            YOU SAY "N" TO DETAIL, FOR WORLD PER JR
      * BAH 070302 ADDED INCLUDE WCBC OPTION FOR WORLD PR# 530
      *            IF BORROWER HAS AN OPEN LOAN AND AN OPEN WCBC, JUST COUNT
      *            THE BORROWER IN THE 1 LOAN COLUMN
      * BAH 170725 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 171205 SKIP ACCOUNTS WITH BI-GIFTNET = "X", THESE ARE TAX
      *            ADVANCE LOANS AND THE SP-NON-LOAN-FLAG = "Y"
      *            LOAN UPDATE SETS LN-GIFTNET TO "X", WORLD PR#1288
      *******************************************************************
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
       77  SCCS-IDENTIFIER PIC X(42) VALUE
           "@(#)./SP/BIDUP.CBL S34 06/15/21-14:43:28 >".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01BI.CPY".
       COPY "LIBLP/LP01BI_SQL.CPY".
       COPY "LIBLP/LPWSBI.CPY".

      *    **********************************
      *    *      PROGRAM WORKERS
      *    **********************************
       01  PR-IFN               PIC XX   VALUE "PR".

       01  FIRST-TIME           PIC X VALUE "Y".

       01  ENT-GROUP            PIC X(4).
       01  INCLUDE-DETAIL-FG    PIC X.
       01  INCLUDE-3-PLUS-FG    PIC X.
       01  INCLUDE-CHGOFFS-FG   PIC X.
       01  INCLUDE-WCBC         PIC X.
       01  GROUP-FLAG           PIC X.
       01  REMAINDER-WK         PIC S9(9)V99 VALUE 0.
       01  RESULT-WK            PIC S9(9)V99 VALUE 0.

       01  LN-WORKERS.
           03  LN-FEED          PIC 99     VALUE 0.
           03  LN-COUNT         PIC 999    VALUE 99.
           03  LN-WK            PIC 999    VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)   VALUE 0.

       01  RECORD-COUNTER       PIC 9(7)   VALUE 0.
       01  WORK-ACCTNO          PIC 9(8).
       01  FILLER               REDEFINES WORK-ACCTNO.
           03  WORK-NUMBER      PIC 9(6).
           03  FILLER           PIC 99.

       01  SAVE-BI1-KEY.
           03  SAVE-BI-SSNO     PIC 9(9) COMP.
           03  SAVE-BI-ACCTNO   PIC 9(8) COMP.
           03  SAVE-BI-OWNBR    PIC 9(4) COMP.
       01  NEXT-BI1-KEY.
           03  NEXT-BI-SSNO     PIC 9(9) COMP.
           03  NEXT-BI-ACCTNO   PIC 9(8) COMP.
           03  NEXT-BI-OWNBR    PIC 9(4) COMP.
       01  PL-COUNT             PIC 9(9)   VALUE 0.
       01  LOAN-COUNT           PIC S9(9)  VALUE +0.
       01  READ-COUNT           PIC S9(9)  VALUE +0.

       01  DISPLAY-LIST         PIC X(4) VALUE "MES.".
       01  DISPLAY-MESS.
           03  FILLER           PIC X(15) VALUE
               "NOW PROCESSING:".
           03  FILLER           PIC X(1)   VALUE " ".
           03  DISPLAY-SSNO     PIC 999/99/9999.
           03  FILLER           PIC X(34)   VALUE " ".

       01  ERRCD          PIC X.

       01  TOTAL-TABLE.
          03  TOTAL-1-LOAN         PIC 9(08).
          03  TOTAL-2-LOANS        PIC 9(08).
          03  TOTAL-3-LOANS        PIC 9(08).
          03  TOTAL-4-PLUS-LOANS   PIC 9(08).
          03  TOTAL-WITH-PL-LOANS  PIC 9(08).
          03  TOTAL-LOANS          PIC 9(08).

      *----------------------------------------------------------------
       01  HEAD1.
           03  H-DATE               PIC X(8) VALUE SPACES.
           03  FILLER               PIC XX   VALUE SPACES.
           03  H-HR                 PIC 99   VALUE 0.
           03  FILLER               PIC X    VALUE ":".
           03  H-MIN                PIC 99   VALUE 0.
           03  FILLER               PIC X(05) VALUE SPACES.
           03  H-NAME               PIC X(40).
           03  FILLER               PIC X(11).
           03  FILLER               PIC X(5)  VALUE "PAGE ".
           03  H-PAGE-NO            PIC ZZZ9.
       01  HEAD2.
           03  FILLER               PIC X(10) VALUE "USER ID:".
           03  H-USERID             PIC X(10).
           03  FILLER               PIC X(22) VALUE SPACES.
           03  FILLER               PIC X(43) VALUE
               "GLOBAL BORROWER DUPLICATE LOAN REPORT FOR: ".
           03  H-GROUP              PIC X(4).

       01  HEAD3.
           03  FILLER               PIC X(54) VALUE
           "BRNO  SSNO         BORROWER NAME".
           03  FILLER               PIC X(46) VALUE
           "ACCT #    DATE    PO-DATE PLCD MAKCD CONV WCBC".

       01  TOTAL-HEAD.
           03  FILLER               PIC X(45) VALUE
           "   1 LOAN     2 LOANS     3 LOANS    4+ LOANS".
           03  FILLER               PIC X(23) VALUE
           "    TOTAL  1+LOAN & P&L".

       01  DETAIL-LINE              PIC X(132).
       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-BRNO               PIC 9(04).
           03  FILLER               PIC X(02).
           03  D-SSNO               PIC 999/99/9999.
           03  FILLER               PIC X(02).
           03  D-BWNAME             PIC X(33).
           03  FILLER               PIC X(02).
           03  D-ACCTNO             PIC ZZ9999.
           03  FILLER               PIC X(02).
           03  D-ENTDATE            PIC 99/99/99.
           03  FILLER               PIC X(02).
           03  D-POFFDATE           PIC 99/99/99 BLANK WHEN ZERO.
           03  FILLER               PIC X(02).
           03  D-PLCD               PIC X.
           03  FILLER               PIC X(04).
           03  D-MAKERCD            PIC X.
           03  FILLER               PIC X(04).
           03  D-CONVERCD           PIC X.
           03  FILLER               PIC X(04).
           03  D-GIFTNET            PIC X.
       01  D-LINE2 REDEFINES DETAIL-LINE.
           03  D-1-LOAN             PIC ZZZZ,ZZZ.
           03  FILLER               PIC X(03).
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
       01  RANGE-LN REDEFINES DETAIL-LINE.
           03  RANGE-LINE       PIC X(20).
           03  RANGE-SELECTION  PIC X(112).


       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       01  BIDUP-WINDOW-HANDLE PIC X(10).
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/BIDUP_DEF.CPY".
       COPY "LIBSP/BIDUP_WKS.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBSP/BIDUP_SCN.CPY".

       PROCEDURE DIVISION
             USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
            PR-FILE.
       COPY "LIBGB/DECLARE.CPY".
           PERFORM CLOSE-BI1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           CLOSE
            PR-FILE.
       COPY "LIBGB/DECLAR2.CPY".

      ******************************************
       MAIN-MODULE SECTION.
      ******************************************
           PERFORM SQL-CONNECT.

       COPY "LIBGB/CHKSEC.CPY".
      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BIDUP" TO VDU-FORM-NAME.

           DISPLAY FLOATING GRAPHICAL WINDOW
                   SCREEN LINE 01 * VDU-WIN-L-OFFSET
                   SCREEN COL 01 * VDU-WIN-C-OFFSET
                   CELL SIZE IS LABEL FONT
                   SIZE 80 + VDU-WIN-W-MOD
                   LINES 24 + VDU-WIN-H-MOD
                   BLANK SCREEN
                   HANDLE IS BIDUP-WINDOW-HANDLE.

           PERFORM FORM-RESET.

           PERFORM INITIALIZATION.
           IF ERRCD NOT = " "
              GO TO END-PROGRAM.

           PERFORM START-BI1-FILE.
       NEXT-BI.
           PERFORM READ-BI1-FILE-NEXT.
           IF IO-BAD
              GO TO END-ROUTINE.

           IF BI-MAKERCD NOT = "M"
              GO TO NEXT-BI.

           IF (BI-POFFDATE NOT = 0)
              GO TO NEXT-BI.

      * THESE ACCOUNTS ARE TAX ADVANCE LOANS AND THE SP-NON-LOAN-FLAG = "Y"
      * LOAN UPDATE SETS LN-GIFTNET TO "X", ALSO SKIP "A" (FUTURE)
           IF BI-GIFTNET = "X" OR "A"
              GO TO NEXT-BI.

           ADD 1 TO READ-COUNT.
           DIVIDE READ-COUNT BY 500 GIVING RESULT-WK
              REMAINDER REMAINDER-WK.
           IF REMAINDER-WK = 0
              MOVE BI-SSNO TO DISPLAY-SSNO
              MOVE DISPLAY-MESS TO WR-BUF
              MOVE DISPLAY-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           MOVE BI-OWNBR TO WGR-SRESULT.
           PERFORM SEARCH-GR-VERIFY.
           IF WGR-ERR NOT = 0
              GO TO NEXT-BI.

           IF FIRST-TIME = "Y"
              MOVE "N" TO FIRST-TIME
              MOVE 0 TO PL-COUNT LOAN-COUNT
              MOVE BI1-KEY TO SAVE-BI1-KEY.

           IF BI-SSNO NOT = SAVE-BI-SSNO
              PERFORM ADD-TOTALS
              IF INCLUDE-DETAIL-FG = "Y"
                 PERFORM DETAIL-ROUTINE
              END-IF
              MOVE BI1-KEY TO SAVE-BI1-KEY
              MOVE 0 TO PL-COUNT
              MOVE 0 TO LOAN-COUNT.

           IF BI-PLCD NOT = " "
              ADD 1 TO PL-COUNT
           ELSE
              ADD 1 TO LOAN-COUNT
              IF INCLUDE-WCBC = "N" AND BI-GIFTNET = "Y"
                 SUBTRACT 1 FROM LOAN-COUNT.

           GO TO NEXT-BI.

       END-ROUTINE.
           PERFORM TOTAL-ROUTINE.
           PERFORM PRINT-RANGES.
           GO TO END-PROGRAM.

      ******************************************
       ADD-TOTALS SECTION.
      ******************************************
           IF LOAN-COUNT = 0
              GO TO EXIT-ADD-TOTALS.

           IF PL-COUNT NOT = 0
              ADD 1 TO TOTAL-WITH-PL-LOANS.

           IF LOAN-COUNT = 1
              ADD 1 TO TOTAL-1-LOAN
           ELSE
           IF LOAN-COUNT = 2
              ADD 1 TO TOTAL-2-LOANS
           ELSE
           IF LOAN-COUNT = 3
              ADD 1 TO TOTAL-3-LOANS
           ELSE
           IF LOAN-COUNT > 3
              ADD 1 TO TOTAL-4-PLUS-LOANS.

       EXIT-ADD-TOTALS.
           EXIT.

      ******************************************
       INITIALIZATION SECTION.
      ******************************************
           MOVE " " TO ERRCD.

       GETPR.
           MOVE BIDUP-QUEUE-POS TO Q-POS.
           MOVE BIDUP-COPIES-POS TO C-POS.
           MOVE BIDUP-PRU-POS TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
           PERFORM PRU-IN.
           IF STAT NOT = "00"
              GO TO ENDIT.

       ENTER-GROUP.

           MOVE "E  A040GROUP." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO ENTER-GROUP
              ELSE
                 MOVE "S  A000GROUP." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.

           IF VDUSTATUS = "1>" GO TO ENDIT.

           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO ENTER-GROUP.

           MOVE VDUBUF TO WGR-WORD
                          ENT-GROUP
                          H-GROUP.

           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.

           IF WGR-ERR > 0
                   AND WGR-ERR NOT = 1
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-GROUP.

           MOVE WGR-WORD TO ENT-GROUP.
           MOVE "Y" TO GROUP-FLAG.

      *-----------------------------------------------------------------
       INCLUDE-DETAIL-YN.
           MOVE "E  A010INCDET." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO GETPR.
           IF VDUSTATUS = "1U" GO TO ENTER-GROUP.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" GO TO INCLUDE-DETAIL-YN.
           IF VDUBUF NOT = "N"
              IF VDUBUF NOT = "Y"
                 GO TO INCLUDE-DETAIL-YN.
           MOVE VDUBUF TO INCLUDE-DETAIL-FG.

      *-----------------------------------------------------------------
       INCLUDE-3-PLUS-YN.
           MOVE "E  A0103PLUS." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO GETPR.
           IF VDUSTATUS = "1U" GO TO INCLUDE-DETAIL-YN.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" GO TO INCLUDE-3-PLUS-YN.
           IF VDUBUF NOT = "N"
              IF VDUBUF NOT = "Y"
                 GO TO INCLUDE-3-PLUS-YN.
           MOVE VDUBUF TO INCLUDE-3-PLUS-FG.

      *-----------------------------------------------------------------
       INCLUDE-CHGOFFS-YN.
           MOVE "E  A010CHGOFFS." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO GETPR.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" GO TO INCLUDE-CHGOFFS-YN.
           IF VDUBUF NOT = "N"
              IF VDUBUF NOT = "Y"
                 GO TO INCLUDE-CHGOFFS-YN.
           MOVE VDUBUF TO INCLUDE-CHGOFFS-FG.
           IF INCLUDE-3-PLUS-FG = "N"
              IF INCLUDE-CHGOFFS-FG = "N"
                 MOVE "WARNING - YOU ARE NOT INCLUDING ANY DETAIL!"
                    TO MESS
                 PERFORM SEND-MESS.

       INCLUDE-WCBC-YN.
           MOVE "E  A010WCBC." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO GETPR.
           IF VDUSTATUS = "1U" GO TO INCLUDE-CHGOFFS-YN.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" GO TO INCLUDE-WCBC-YN.
           IF VDUBUF NOT = "N"
              IF VDUBUF NOT = "Y"
                 GO TO INCLUDE-WCBC-YN.
           MOVE VDUBUF TO INCLUDE-WCBC.

      *-----------------------------------------------------------------
       OK-YN.
           MOVE "E  A010SEL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO GETPR.
           IF VDUSTATUS = "1U" GO TO INCLUDE-WCBC-YN.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" GO TO OK-YN.
           IF VDUBUF = "N" GO TO ENDIT.
           IF VDUBUF NOT = "Y" GO TO OK-YN.

           MOVE ZEROS TO RECORD-COUNTER.

           MOVE SPACES TO DETAIL-LINE.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           PERFORM OPEN-BI1-FILE.
           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-BR-FILE.

           PERFORM OPEN-GR1-FILE.
           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.

           MOVE 0 TO TOTAL-1-LOAN
                     TOTAL-2-LOANS
                     TOTAL-3-LOANS
                     TOTAL-4-PLUS-LOANS
                     TOTAL-WITH-PL-LOANS.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      **************************************
       DETAIL-ROUTINE SECTION.
      **************************************
           IF INCLUDE-3-PLUS-FG = "Y"
              IF LOAN-COUNT > 2
                 GO TO CONTINUE-DETAIL.

           IF INCLUDE-CHGOFFS-FG = "Y"
              IF PL-COUNT > 0
                 IF LOAN-COUNT > 0
                    GO TO CONTINUE-DETAIL.

           GO TO EXIT-DETAIL-ROUTINE.

       CONTINUE-DETAIL.
           MOVE 2 TO LN-FEED.

           MOVE BI1-KEY TO NEXT-BI1-KEY.
           MOVE SAVE-BI1-KEY TO BI1-KEY.

           MOVE BI-SSNO TO QBI1-WBEG-SSNO
                           QBI1-WEND-SSNO.
           MOVE 0       TO BI-OWNBR BI-ACCTNO
                           QBI1-WBEG-ACCTNO
                           QBI1-WBEG-OWNBR.
           MOVE ALL "9" TO QBI1-WEND-ACCTNO
                           QBI1-WEND-OWNBR.

           PERFORM START-BI1-FILE.
       NEXT-DETAIL-BI.
           PERFORM READ-BI1-FILE-NEXT.
           IF IO-BAD
              GO TO RESTART-BI.

           IF BI-SSNO NOT = SAVE-BI-SSNO
              GO TO RESTART-BI.

           IF (BI-POFFDATE NOT = 0)
              GO TO NEXT-DETAIL-BI.

           IF BI-MAKERCD NOT = "M"
              GO TO NEXT-DETAIL-BI.

           MOVE BI-OWNBR    TO D-BRNO.
           MOVE BI-SSNO     TO D-SSNO.
           INSPECT D-SSNO REPLACING ALL "/" BY "-".
           MOVE BI2-KEY     TO D-BWNAME.
           MOVE BI-ENTDATE  TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-ENTDATE.
           MOVE BI-ACCTNO   TO WORK-ACCTNO.
           MOVE WORK-NUMBER TO D-ACCTNO.
           MOVE BI-POFFDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-POFFDATE.
           MOVE BI-PLCD     TO D-PLCD.
           MOVE BI-MAKERCD  TO D-MAKERCD.
           MOVE BI-CONVERCD TO D-CONVERCD.
           MOVE BI-GIFTNET  TO D-GIFTNET.
           PERFORM WRITE-DETAIL-LINE.

           GO TO NEXT-DETAIL-BI.
       RESTART-BI.
           MOVE NEXT-BI1-KEY TO BI1-KEY.

           PERFORM START-BI1-FILE.
           PERFORM READ-BI1-FILE-NEXT.
       EXIT-DETAIL-ROUTINE.
           EXIT.

      ******************************************
       TOTAL-ROUTINE SECTION.
      ******************************************
           MOVE TOTAL-HEAD TO DETAIL-LINE.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           MOVE TOTAL-1-LOAN TO D-1-LOAN.
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

      *************************
       PRINT-RANGES SECTION.
      *************************
           MOVE "GROUP ID:" TO RANGE-LINE.
           MOVE  ENT-GROUP  TO RANGE-SELECTION.
           MOVE 3 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "INCLUDE 3+ OPENS: " TO RANGE-LINE.
           MOVE INCLUDE-3-PLUS-FG    TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "INCLUDE 1+ OPEN & PL: " TO RANGE-LINE.
           MOVE INCLUDE-CHGOFFS-FG       TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "INCLUDE 1+ OPEN & WCBC: " TO RANGE-LINE.
           MOVE INCLUDE-WCBC               TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           PERFORM WRITE-DETAIL-LINE.

      ******************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************
           ADD LN-COUNT LN-FEED GIVING LN-WK.
           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ******************************************
       HEAD-PAGE SECTION.
      ******************************************
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

           IF INCLUDE-DETAIL-FG = "Y"
              MOVE HEAD3 TO PR-REC
              MOVE 2 TO LN-FEED
              PERFORM WRITE-PR-REC
              MOVE 2 TO LN-FEED
              MOVE 3 TO LN-COUNT
           ELSE
              MOVE 2 TO LN-FEED
              MOVE 1 TO LN-COUNT.

       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/DATER.CPY".

      ****************************************************
       FORM-RESET SECTION.
      ****************************************************
           MOVE EXT-CONAME-CONAME TO BIDUP-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BIDUP-SYSDATE.
           DISPLAY BIDUP-SCREEN UPON BIDUP-WINDOW-HANDLE.
           MOVE BIDUP-FORM-FIELDS TO WR-BUF.
           MOVE BIDUP-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ****************************************************
       SETUP-LINK-INFO SECTION.
      ****************************************************
           MOVE BIDUP-HELP--FILE TO HELP-LINK-FILE.
           MOVE BIDUP-HELP--DIR  TO HELP-LINK-DIR.

      ****************************************************
       VDU-CONVERT-FIELD SECTION.
      ****************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/BIDUP_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************
       MISC-PARAGRAPH SECTION.
      ******************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/GETENV.CPY".

      ******************************************
       IO-ROUTINE SECTION.
      ******************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPBIGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPBI1RN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO ERRCD.

      ******************************************
       END-PROGRAM SECTION.
      ******************************************
       END-RUN.
           PERFORM CLOSE-FILES.
           PERFORM PRU-OUT.
       EXIT-PROG.
           MOVE "LP/LPMENU" TO FORM-NAM.
           MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           DESTROY BIDUP-WINDOW-HANDLE.
           EXIT PROGRAM.
