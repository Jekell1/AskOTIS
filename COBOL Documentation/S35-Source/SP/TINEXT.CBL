       IDENTIFICATION DIVISION.
       PROGRAM-ID.       TINEXT.
       DATE-WRITTEN.   11/21/08.
      **********************************************************************
      *                      (SP)
      *       DESCRIPTION:  MONTHLY TIN EXTRACTION FOR IRS
      *       FROM SP/LN1099.C, WORLD PR# 607
      *
      *       WORLD WILL RUN MONTHLY FOR NEW BORROWERS & FOR P&L'S
      *           THIS WAY THEY CAN STAY ON TOP OF INVALID SSNO'S
      *           THROUGHOUT THE YEAR & FIX INSTEAD OF ALL AT THE
      *           END OF THE YEAR (THEY GET FINED IF THEY SEND IN
      *           1099'S FOR INVALID SSNO'S
      *
      * REV:
      * BLM 131212 FILE IS GETTING REJECTED BY IRS.  TRY ADDING CARRIAGE
      *            RETURN, SO EACH RECORD ENDS WITH CR/LF.  WORLD PR# 952
      * BLM 131213 MAKE FILE VARIABLE LENGTH BY ADDING "OUTPUT" TO
      *            ASSIGN IN FILE SELECT.  WORLD PR# 952A
      * BLM 141216 CREATE WORK FILE KEYED BY BRNO, SSNO & WRITE EACH RECORD
      *            AS IT IS EXTRACTED.  IF IT'S ALREADY IN FILE, DON'T 
      *            INCLUDE IN REPORT OR IN TIN MATCH FILE.  IRS REJECTES DUPS.
      *            CORRECT TITLE ON REPORT.  WORLD PR# 1040
      *  BAH 160201 REMOVED LTFILE AND TONS OF COPY MEMBERS NOT NEEDED PD0003
      * BLM 170406 EXPAND WORK-FNAME & WORK-LNAME FROM 16 TO 18; NAMES LONGER
      *            THAN 14 CAUSED A SPACE AT THE END OF THE NAME FIELD AND IRS
      *            REJECTED FILE.  FOR EX: LAST NAME OF "GRIER STILLWELL"
      *            CAME OUT OF STRING AS "SHACOLBIAN K GRIER STILLWELL ".
      *            WORLD PR# 1239
      * BAH 181217 GLOBAL LNFILE
      *  20190715 BAH MAKE LN-ACCTNO LOOK LIKE A15 IN THE TIN FILE, MOVE
      *               TO A 9(6) FIRST SO THERE ARE NO LEADING ZEROS, QA0058
      * BAH 20191007 REMOVED ACCESS-CALL LNFILE
      * BAH 20220824 HARD CODED START
      * MB  20240807 CHANGES FOR S35Q-121 
      *              PERFORMANCE TUNING. CHANGED TO USE SSP FOR LNFILE RETRIEVAL
      *              CHANGED TO USE ARRAYSP PROCESSING.
      **************************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSWKI.CPY".
           SELECT TIN-FILE ASSIGN OUTPUT TIN-PATH
                  ORGANIZATION LINE SEQUENTIAL
                  ACCESS MODE SEQUENTIAL
                  LOCK MODE AUTOMATIC WITH LOCK ON RECORD
                  FILE STATUS FILE-STAT.
       COPY "LIBGB/GBFSPR.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.

       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-BRNO  PIC 9(4).
               05  WK-SSNO  PIC 9(9).

       FD  TIN-FILE
           RECORD IS VARYING IN SIZE FROM 1 TO 63 CHARACTERS
           LABEL RECORDS ARE STANDARD.
       01  TIN-REC.
           03  TIN-BYTE                PIC X OCCURS 63 TIMES.

       COPY "LIBGB/GBFDPR.CPY".

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)SP/TINEXT.CBL S35 02/22/24-08:19:09 >".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
       COPY "LIBLP/LP01SP.CPY".
       COPY "LIBLP/LP01SPA.CPY".
       COPY "LIBLP/LP01SPB.CPY".
       COPY "LIBLP/LP01SPC.CPY".
       COPY "LIBLP/LP01SP_SQL.CPY".
       COPY "LIBLP/LPWSSP.CPY".

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  BEG-LOANDATE                PIC X(10).
       01  END-LOANDATE                PIC X(10).
       01  BEG-PLDATE                  PIC X(10).
       01  END-PLDATE                  PIC X(10).
       EXEC SQL END DECLARE SECTION END-EXEC.


       01  TIN-PATH.
           03  TIN-PATH-BASE          PIC X(09).
           03  FILLER                 PIC X(01)  VALUE "/".
           03  TIN-PATH-MACHINE       PIC X(02).
           03  FILLER                 PIC X(04)  VALUE "/ED/".
           03  TIN-PATH-NAME.
               05  TIN-PATH-EXTR        PIC X(03)  VALUE "TIN".
               05  TIN-PATH-DOT         PIC X(01)  VALUE ".".
               05  TIN-PATH-DATE-MMDDYY PIC 9(6).
               05  TIN-PATH-DOT2        PIC X(01)  VALUE ".".
               05  TIN-PATH-BRNO        PIC 9(04).
               05  TIN-PATH-GROUP  REDEFINES TIN-PATH-BRNO PIC X(04).

       01  WK-IFN               PIC X(2)     VALUE "WK".
       COPY "LIBLP/LP01LP.CPY".

      ******************************************************************
      *
      *    L I N E    W O R K E R S
      *
      ******************************************************************
       01  LN-FEED          PIC 99    VALUE 0.
       01  LN-COUNT         PIC 999   VALUE 99.

       01  PAGE-NUMBER          PIC 9(4)  VALUE 0.

       01  SUB                  PIC 9(6) COMP-3       VALUE 0.
       01  SUB2                 PIC 9(7) COMP-3       VALUE 0.
       01  MATCH                PIC X        VALUE "N".

       01  TEST-CHAR            PIC X.
           88 VALID-CHAR        VALUE " " "-" "&".

       01  WORK-NAME            PIC X(16).
       01  FILLER REDEFINES WORK-NAME.
           03  NAME-CHAR        PIC X OCCURS 16 TIMES.

       01  WORK-FNAME           PIC X(18).
       01  FILLER REDEFINES WORK-FNAME.
           03  FNAME-CHAR       PIC X OCCURS 18 TIMES.

       01  WORK-LNAME           PIC X(18).
       01  FILLER REDEFINES WORK-LNAME.
           03  LNAME-CHAR       PIC X OCCURS 18 TIMES.

       01  WORK-ACCTNO          PIC 9(6).
       01  BAD-NAME-FG          PIC X    VALUE "N".
       01  BAD-NAME-COUNTER     PIC 9(8) VALUE 0.
       01  DUP-COUNTER          PIC 9(8) VALUE 0.

       01  SOCSEC-KEY      PIC 9(9).
       01  FILLER REDEFINES SOCSEC-KEY.
           03  SS-1        PIC 9(3).
           03  SS-2        PIC 99.
           03  SS-3        PIC 9(4).
       01  FILLER REDEFINES SOCSEC-KEY.
           03  SS-FIRST4   PIC 9(4).
           03  SS-LAST5    PIC 9(5).
       01  FILLER REDEFINES SOCSEC-KEY.
           03  SS-CHAR1    PIC 9.
           03  FILLER      PIC 9(8).

       01  CURBAL-LESS-UNEARNED PIC S9(9)V99 VALUE 0.

       01  BR-TOTAL-AMT         PIC 9(9)V99 VALUE 0.
       01  BR-TOTAL-ACCOUNTS    PIC 9(9)V99 VALUE 0.
       01  GRAND-TOTAL-AMT      PIC 9(9)V99 VALUE 0.
       01  GRAND-TOTAL-ACCOUNTS PIC 9(9)V99 VALUE 0.

       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH           PIC 9(4).
           03  END-BRANCH           PIC 9(4).
           03  BEG-CLASS            PIC 9(3).
           03  END-CLASS            PIC 9(3).
           03  BEG-LN-DATE          PIC 9(8).
           03  END-LN-DATE          PIC 9(8).
           03  INC-PL-FG            PIC X.
           03  BEG-PL-DATE          PIC 9(8).
           03  END-PL-DATE          PIC 9(8).
           03  SRC-CODE-TABLE.
               05  SRC-CODE         PIC XX OCCURS 10.
           03  PRINT-DETAIL-FG      PIC X.
           03  BRANCH-PAGE-FG       PIC X.
           03  GROUP-FLAG           PIC X.
           03  ENT-GROUP            PIC X(4).
           03  CREATE-TIN-FG        PIC X.

       01  VERSION-CONTROL          PIC X VALUE "A".

       01  SRC-CODE-LIST            PIC X(36) VALUE
           "SC1 SC2 SC3 SC4 SC5 SC6 SC7 SC8 SC9.".

       01  MESS-LIST                PIC X(5) VALUE "MESS.".

       01  LN3-CONN-OPEN-SW         PIC X  VALUE "N".

      ******************************************************************
      *
      *     P R I N T   B U F F E R S
      *
      ******************************************************************
       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(35)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(33)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(35)    VALUE " ".
           03  FILLER           PIC X(21)    VALUE
               "TIN EXTRACTION REPORT".
           03  FILLER           PIC X(35)    VALUE " ".
           03  FILLER           PIC X(17)    VALUE
               "* = INVALID NAME ".

       01  HEAD3.
           03  FILLER      PIC X(54)    VALUE
               "BRNO  ACCOUNT NO.   BORROWER".
           03  FILLER      PIC X(40)    VALUE
               "SRC LOANDATE  PLDATE  LAST PMT    LEGAL ".
           03  FILLER      PIC X(27)    VALUE
               "JUDGMENT DEBTAID    BALANCE".
           03  FILLER      PIC X(11)    VALUE
               "           ".

      ******************************************************************
      *
      *    D E T A I L   L I N E
      *
      ******************************************************************
       01  DETAIL-LINE          PIC X(132) VALUE SPACES.

       01  D-LINE1 REDEFINES DETAIL-LINE.
           03  D-BRNO      PIC 9999.
           03  FILLER      PIC XX.
           03  D-LOAN-NUM  PIC X(13).
           03  FILLER      PIC X.
           03  D-SSNO      PIC 999/99/9999.
           03  FILLER      PIC X.
           03  D-NAME      PIC X(21).
           03  D-STAR      PIC X.
           03  D-SRCD      PIC X(2).
           03  FILLER      PIC XX.
           03  D-LOANDATE  PIC 99/99/99 BLANK WHEN ZERO.
           03  FILLER      PIC X.
           03  D-PLDATE    PIC 99/99/99 BLANK WHEN ZERO.
           03  FILLER      PIC X.
           03  D-PAYDATE   PIC 99/99/99 BLANK WHEN ZERO.
           03  FILLER      PIC X.
           03  D-LEGALDATE PIC 99/99/99 BLANK WHEN ZERO.
           03  FILLER      PIC X.
           03  D-JDDATE    PIC 99/99/99 BLANK WHEN ZERO.
           03  FILLER      PIC X.
           03  D-DEBTDATE  PIC 99/99/99 BLANK WHEN ZERO.
           03  FILLER      PIC X.
           03  D-CURBAL    PIC ZZZZZZ.99.
           03  FILLER      PIC X(11).
       01  TOTAL-LINE1 REDEFINES DETAIL-LINE.
           03  FILLER      PIC X(32).
           03  T-PROMPT1   PIC X(15).
           03  T-BRNO      PIC ZZZZ.
           03  FILLER      PIC X(8).
           03  T-PROMPT2   PIC X(20).
           03  T-ACCOUNTS  PIC ZZZZZ9.
           03  FILLER      PIC X(35).
       01  RANGE-LN REDEFINES DETAIL-LINE.
           03  RANGE-LINE       PIC X(24).
           03  RANGE-SELECTION  PIC X(108).
           03  RANGE-DATE REDEFINES RANGE-SELECTION PIC 9999/99/99.

       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBSP/TINEXT_DEF.CPY".
       COPY "LIBSP/TINEXT_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".
       COPY "LIBLP/ARRAYSPW.CPY".

      *****************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      *****************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBSP/TINEXT_SCN.CPY".

      *----------------------------------------------------------------
       PROCEDURE DIVISION
           USING FORM-PATHNAME EXIT-PATHNAME.

       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE TIN-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-LN3-CONNECTION.
           CLOSE
               PR-FILE TIN-FILE WK-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *****************************************
       MAIN-MODULE SECTION.
      *****************************************
           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "TINEXT" TO VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF GROUP-FLAG = "N"
              MOVE BEG-BRANCH TO BR-NO
              PERFORM START-BR-FILE
              GO TO NEXT-BRANCH-NO.

           MOVE ENT-GROUP TO GR-ID.
           PERFORM SEARCH-GR-INIT.
       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.
           PERFORM SEARCH-GR-NEXT.
           IF WGR-SRESULT = 0
              GO TO END-ROUTINE.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-FG = 9 OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.
       MOVE-BRANCH-PATH-INFO.
           MOVE EXT-FILPATH-BASE TO FILPATH-BASE.
           MOVE BR-MACHINE       TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH     TO FILPATH-DATA.

           MOVE "B"        TO LN-PATH-OVERRIDE
                              BW-PATH-OVERRIDE.

           IF EXT-ROUTE-BUF = "00"
              IF PRU-LP NOT = "VDU" AND NOT = "PRU"
                 MOVE "PROCESSING BRANCH &&&&..." TO MESS
                 INSPECT MESS REPLACING FIRST "&&&&" BY BR-NO
                 MOVE MESS TO WR-BUF
                 MOVE MESS-LIST TO WR-LIST
                 PERFORM CALL-WRFORM.

           PERFORM OPEN-LN3-FILE.

           MOVE BR-NO     TO LN-OWNBR
                             QLN3-WBEG-OWNBR
                             QLN3-WEND-OWNBR.
           MOVE BEG-CLASS TO LN-CLASS
                             QLN3-WBEG-CLASS.
           MOVE END-CLASS TO QLN3-WEND-CLASS.
      *     MOVE 0         TO LN-ACCTNO
      *                       QLN3-WBEG-ACCTNO.
      *     MOVE ALL "9"   TO QLN3-WEND-ACCTNO.


           MOVE BEG-LN-DATE TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO BEG-LOANDATE.

           MOVE END-LN-DATE TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO END-LOANDATE.

           IF BEG-PL-DATE = 19500101
              MOVE '1900-01-01' TO BEG-PLDATE
           ELSE
              MOVE BEG-PL-DATE TO SQL-DATE-YYYYMMDD
              PERFORM SQL-SET-DATE
              MOVE SQL-DATE-YYYY-MM-DD TO BEG-PLDATE.

           MOVE END-PL-DATE TO SQL-DATE-YYYYMMDD.
           PERFORM SQL-SET-DATE.
           MOVE SQL-DATE-YYYY-MM-DD TO END-PLDATE.

           PERFORM START-LN3-FILE.  *>  SSP LNFILE BWFILE

       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT.   *>  SSP LNFILE BWFILE
           IF IO-BAD       *> OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-OF-GROUP.

      * THIS ALSO CHECKED IN SSP
           IF LN-OWNBR > END-BRANCH
              GO TO END-OF-GROUP.

      * THIS ALSO CHECKED IN SSP
      * VOIDED LOAN, RESCISSION
           IF ( LN-POCD-NONLOAN )
              GO TO NEXT-LN.

      * THIS ALSO CHECKED IN SSP
           IF LN-CLASS < BEG-CLASS OR LN-CLASS > END-CLASS
              GO TO NEXT-LN.

           IF INC-PL-FG = "S"
              IF LN-PLDATE NOT = 0
                 GO TO NEXT-LN.

           IF INC-PL-FG = "O"
              IF LN-PLDATE = 0
                 GO TO NEXT-LN.

      * THIS ALSO CHECKED IN SSP
           IF LN-LOANDATE < BEG-LN-DATE OR
               LN-LOANDATE > END-LN-DATE
              GO TO NEXT-LN.


       TEST-SSNO.
           IF LN-SSNO(1) = 0
              GO TO NEXT-LN.
           MOVE LN-SSNO(1) TO SOCSEC-KEY.
           IF SS-1 = 0      GO TO NEXT-LN.
           IF SS-2 = 0      GO TO NEXT-LN.
           IF SS-3 = 0      GO TO NEXT-LN.
           IF SS-FIRST4 = 0 GO TO NEXT-LN.
           IF SS-1 = 666    GO TO NEXT-LN.
           IF SS-CHAR1 = 8  GO TO NEXT-LN.
           IF SS-CHAR1 = 9  GO TO NEXT-LN.
           IF (SS-1 > 728 AND SS-1 < 750)
              GO TO NEXT-LN.
           IF (SS-1 > 771 AND SS-1 < 800)
              GO TO NEXT-LN.

           MOVE LN-ORGST     TO SP-ORGST.
           MOVE LN-SPRCLASS  TO SP-SPRCLASS.
           MOVE LN-SUBCLASS  TO SP-SUBCLASS.
           MOVE LN-LAWCODE   TO SP-LAWCODE.
           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
      *        PERFORM READ-SP1-FILE
              PERFORM ARRAYSP-READ
              IF IO-FG NOT = 0
                 GO TO IO-ERROR.

           PERFORM LOAN-CALCULATIONS.

           IF LN-PLDATE = 0
              GO TO CHECK-SOURCE-CODES.

      * THIS ALSO CHECKED IN SSP
           IF LN-PLDATE < BEG-PL-DATE OR
               LN-PLDATE > END-PL-DATE
              GO TO NEXT-LN.

       CHECK-SOURCE-CODES.
           IF SRC-CODE(1) = "**"
              GO TO PRINT-ACCOUNT.
           MOVE 1 TO SUB.
       CHECK-NEXT-SOURCE-CODE.
           IF LN-SRCD = SRC-CODE(SUB)
              GO TO PRINT-ACCOUNT.
           ADD 1 TO SUB.
           IF SUB > 9
              GO TO NEXT-LN
           ELSE
              GO TO CHECK-NEXT-SOURCE-CODE.

       PRINT-ACCOUNT.

      * BWFILE DATA RETRIEVE IN LNFILE SSP  
           MOVE LN-SSNO(1) TO BW-SSNO.
      *     PERFORM OPEN-BW1-FILE.
      *     PERFORM READ-BW1-FILE.
      *     PERFORM CLOSE-BW1-FILE.
      *     IF IO-FG NOT = 0
           IF BW-OWNBR = 0   *> SSP SET TO 0 IF BW REC NOT FOUND
              MOVE SPACES          TO BW-ZIP
              MOVE "NOT ON FILE!!" TO BW-LNAME.

      * IRS REJECTS IF DUPLICATES, ONLY WRITE SSNO ONCE FOR BRANCH
       CHECK-FOR-DUPS.
           MOVE LN-OWNBR   TO WK-BRNO.
           MOVE LN-SSNO(1) TO WK-SSNO.
           PERFORM WRITE-WK-FILE.
           IF IO-BAD
              IF E-CODE = "22"
                 ADD 1 TO DUP-COUNTER
                 GO TO NEXT-LN
              ELSE
                 GO TO IO-ERROR.

           PERFORM CREATE-TIN-REC.
           PERFORM CREATE-DETAIL-LINE.

           GO TO NEXT-LN.

       END-OF-GROUP.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.
           PERFORM PRINT-BRANCH-TOTALS.
           IF BRANCH-PAGE-FG = "Y"
              MOVE 99 TO LN-COUNT.
           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           PERFORM PRINT-GRAND-TOTALS.
           PERFORM PRINT-RANGE-LINES.
           GO TO END-PROGRAM.

      *************************************************
       CREATE-DETAIL-LINE SECTION.
      *************************************************
           MOVE LN-OWNBR TO D-BRNO.
           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO D-LOAN-NUM.
           MOVE LN-SSNO(1) TO D-SSNO.
           INSPECT D-SSNO REPLACING ALL "/" BY "-".

           MOVE SPACES TO D-NAME.
           STRING BW-LNAME,", ",BW-FNAME DELIMITED BY "   "
                                         INTO D-NAME.
           IF BAD-NAME-FG = "Y"
              MOVE "*" TO D-STAR.
           MOVE LN-SRCD TO D-SRCD.
           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-LOANDATE.
           MOVE LN-PLDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-PLDATE.
           MOVE LN-LAST-PYPA-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-PAYDATE.
           MOVE LN-LEGALDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-LEGALDATE.
           MOVE LN-JDDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-JDDATE.
           MOVE LN-DEBTAIDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-DEBTDATE.
           MOVE LN-CURBAL TO D-CURBAL.
      *    MOVE CURBAL-LESS-UNEARNED TO D-REPORTED.
           ADD CURBAL-LESS-UNEARNED TO BR-TOTAL-AMT GRAND-TOTAL-AMT.
           ADD 1 TO BR-TOTAL-ACCOUNTS GRAND-TOTAL-ACCOUNTS.
           MOVE 1 TO LN-FEED.
           IF PRINT-DETAIL-FG = "Y"
              PERFORM WRITE-DETAIL-LINE
           ELSE
           IF (PRINT-DETAIL-FG = "B" AND BAD-NAME-FG = "Y")
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE SPACES TO DETAIL-LINE.

      *************************************************
       PRINT-BRANCH-TOTALS SECTION.
      *************************************************
           MOVE "TOTAL BRANCH #" TO T-PROMPT1.
           MOVE BR-NO TO T-BRNO.
           MOVE "NO. OF ACCOUNTS" TO T-PROMPT2.
           MOVE BR-TOTAL-ACCOUNTS TO T-ACCOUNTS.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           IF PRINT-DETAIL-FG = "Y"
              MOVE 1 TO LN-FEED
              PERFORM WRITE-DETAIL-LINE.
           MOVE 0 TO BR-TOTAL-AMT BR-TOTAL-ACCOUNTS.

      *************************************************
       PRINT-GRAND-TOTALS SECTION.
      *************************************************
           MOVE "GRAND TOTALS" TO T-PROMPT1.
           MOVE "NO. OF ACCOUNTS" TO T-PROMPT2.
           MOVE GRAND-TOTAL-ACCOUNTS TO T-ACCOUNTS.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "NO OF BAD NAMES" TO T-PROMPT2.
           MOVE BAD-NAME-COUNTER TO T-ACCOUNTS.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "NO OF SKIPPED DUPS" TO T-PROMPT2.
           MOVE DUP-COUNTER TO T-ACCOUNTS.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *********************************************
       INITIALIZATION SECTION.
      *********************************************
           MOVE TINEXT-QUEUE-POS TO Q-POS.
           MOVE TINEXT-COPIES-POS TO C-POS.
           MOVE TINEXT-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           IF CREATE-TIN-FG = "Y"
              PERFORM SET-TIN-PATH
              IF EXT-EOBUF-ERRCD NOT = " "
                 GO TO ENDIT.

           IF EXT-ROUTE-BUF = "00"
              MOVE "1099-C REPORT/EXTRACTION IN PROGRESS..." TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-PR-FILE.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT NOT = "00"
              MOVE "CANNOT MAKE WORK FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.
          
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           PERFORM ARRAYSP-LOAD.

           GO TO EXIT-INITIALIZE.
       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.

       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       SET-TIN-PATH SECTION.
      ******************************************************************

           MOVE EXT-FILPATH-BASE    TO TIN-PATH-BASE.
           MOVE EXT-FILPATH-MACHINE TO TIN-PATH-MACHINE.

           ACCEPT DATE-YYYYMMDD FROM CENTURY-DATE.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO TIN-PATH-DATE-MMDDYY.

           IF GROUP-FLAG = "Y"
              MOVE ENT-GROUP        TO TIN-PATH-GROUP
           ELSE
              MOVE BEG-BRANCH       TO TIN-PATH-BRNO.

           MOVE TIN-PATH TO ACCESS-BUF.
           PERFORM ACCESS-CALL.
           IF STAT NOT = "00"
              PERFORM OPEN-TIN-FILE-OUTPUT
              GO TO SET-TIN-PATH-EXIT.

       SET-TIN-PATH-ASK.
           PERFORM ALARM.
           MOVE "TIN MATCH FILE ALREADY EXISTS, CONTINUE (Y/N):"
                                                        TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "R RA010ACCEPT." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS NOT = "00"
              GO TO SET-TIN-PATH-ASK.
           IF VDUBUF = "N"
              MOVE "E" TO EXT-EOBUF-ERRCD
              GO TO SET-TIN-PATH-EXIT.
           IF VDUBUF NOT = "Y"
              GO TO SET-TIN-PATH-ASK.
       OPEN-TIN-OUTPUT.
           PERFORM OPEN-TIN-FILE-OUTPUT.

           MOVE SPACES TO WR-BUF.
           MOVE MESS-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       SET-TIN-PATH-EXIT.
           EXIT.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO PAGE-ON-BRANCH-YN.

       BR-01.
           MOVE "E RA000BEGBR." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO BR-01
              ELSE
                 MOVE "S  A000BEGBR." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-01.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.
           IF WGR-WORD-ZERO
              GO TO BRANCH-IS-NUMERIC.
           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.
           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           IF WGR-TYPE = "A"
              MOVE "S  A000ENDBR." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO CL-01.
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BEGBR." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
       BR-02.
           MOVE "ENNN040ENDBR." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.
           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           GO TO CL-01.
       ALL-BR.
           MOVE "SNNN040BEGBR." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040ENDBR." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.
       CL-01.
           MOVE "ENNN020BEGCL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-01.
           MOVE VDUNUM0 TO BEG-CLASS.
       CL-02.
           MOVE "ENNN020ENDCL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO CL-02.
           MOVE VDUNUM0 TO END-CLASS.
           IF BEG-CLASS > END-CLASS
              MOVE "INVALID CLASS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO CL-01.
           GO TO LN-DATE-01.
       ALL-CL.
           MOVE "SNNN020BEGCL." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020ENDCL." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.

       LN-DATE-01.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO PL-SKIP-INC-ONLY.
           MOVE "ERNM060LNDATE1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "16" GO TO ALL-DATES.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO LN-DATE-01.
           MOVE VDU-DATE-YYYYMMDD TO BEG-LN-DATE.
       LN-DATE-02.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO PL-SKIP-INC-ONLY.
           MOVE "ERNM060LNDATE2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO LN-DATE-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO LN-DATE-02.
           MOVE VDU-DATE-YYYYMMDD TO END-LN-DATE.
           GO TO PL-SKIP-INC-ONLY.

       ALL-DATES.
           MOVE "S  8080LNDATE1." TO VDU.
           MOVE EXT-JULIAN-BEG TO BEG-LN-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080LNDATE2."   TO VDU.
           MOVE EXT-JULIAN-END TO END-LN-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

       PL-SKIP-INC-ONLY.
           MOVE "E  A010PL." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO PL-SKIP-INC-ONLY.
           MOVE VDUBUF TO INC-PL-FG.
           IF INC-PL-FG NOT = "I"
              IF INC-PL-FG NOT = "S"
                 IF INC-PL-FG NOT = "O"
                 MOVE "PLEASE ENTER I, S, OR O!" TO MESS
                 PERFORM SEND-MESS
                 GO TO PL-SKIP-INC-ONLY.

       PLDATE-01.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO SOURCE-CODES.
           IF INC-PL-FG = "S"
              GO TO ALL-PL-DATES.
           MOVE "ERNM060PLDATE1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "16" GO TO ALL-PL-DATES.
           IF VDUSTATUS = "1U" GO TO PL-SKIP-INC-ONLY.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO PLDATE-01.
           MOVE VDU-DATE-YYYYMMDD TO BEG-PL-DATE.
       PLDATE-02.
           IF EXT-ROUTE-BUF = "40" OR "50"
              GO TO SOURCE-CODES.
           MOVE "ERNM060PLDATE2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO PLDATE-01.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D" GO TO PLDATE-02.
           MOVE VDU-DATE-YYYYMMDD TO END-PL-DATE.
           GO TO SOURCE-CODES.

       ALL-PL-DATES.
           MOVE "S  8080PLDATE1." TO VDU.
           MOVE EXT-JULIAN-BEG TO BEG-PL-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080PLDATE2."   TO VDU.
           MOVE EXT-JULIAN-END TO END-PL-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

       SOURCE-CODES.
           MOVE SPACES TO SRC-CODE-TABLE MESS.
           MOVE MESS TO WR-BUF.
           MOVE SRC-CODE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           IF INC-PL-FG = "O"
              MOVE "S  A020SC1." TO VDU
              MOVE "**" TO SRC-CODE(1) VDUBUF
              PERFORM SCREENIO
              GO TO PRINT-DETAIL-YN.
           MOVE 1 TO SUB.
       ENTER-NEXT-SRC-CODE.
           MOVE "E  A020SC ." TO VDU.
           MOVE SUB TO VDUCOL.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U"
              IF SUB = 1
                 GO TO PLDATE-02
              ELSE
                 SUBTRACT 1 FROM SUB
                 GO TO ENTER-NEXT-SRC-CODE.
           IF VDUSTATUS = "16"
              IF SUB = 1
                 MOVE "S  A020SC1." TO VDU
                 MOVE "**" TO SRC-CODE(1) VDUBUF
                 PERFORM SCREENIO
                 GO TO PRINT-DETAIL-YN.
           IF VDUSTATUS = "1>" GO TO PRINT-DETAIL-YN.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO ENTER-NEXT-SRC-CODE.
           MOVE VDUBUF TO SRC-CODE(SUB).
           ADD 1 TO SUB.
           IF SUB < 10 GO TO ENTER-NEXT-SRC-CODE.
       PRINT-DETAIL-YN.
           MOVE "E  A010DETAILYN." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO SOURCE-CODES.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO PRINT-DETAIL-YN.
           MOVE VDUBUF TO PRINT-DETAIL-FG.
           IF PRINT-DETAIL-FG NOT = "Y"
              IF PRINT-DETAIL-FG NOT = "N"
                 IF PRINT-DETAIL-FG NOT = "B"
                    MOVE "PLEASE ENTER Y OR N!" TO MESS
                    PERFORM SEND-MESS
                    GO TO PRINT-DETAIL-YN.

       PAGE-ON-BRANCH-YN.
           MOVE "E  A010PAGEFG." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO PRINT-DETAIL-YN.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO PAGE-ON-BRANCH-YN.
           MOVE VDUBUF TO BRANCH-PAGE-FG.
           IF BRANCH-PAGE-FG NOT = "Y"
              IF BRANCH-PAGE-FG NOT = "N"
                 MOVE "PLEASE ENTER Y OR N!" TO MESS
                 PERFORM SEND-MESS
                 GO TO PAGE-ON-BRANCH-YN.

       CREATE-TIN-FILE-YN.
           MOVE "E  A010TINFG." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO BR-01.
           IF VDUSTATUS = "1U" GO TO PAGE-ON-BRANCH-YN.
           IF VDUSTATUS = "1>" GO TO ENDIT.
           IF VDUSTATUS NOT = "00" AND NOT = "1D"
              GO TO CREATE-TIN-FILE-YN.
           MOVE VDUBUF TO CREATE-TIN-FG.
           IF CREATE-TIN-FG NOT = "Y"
              IF CREATE-TIN-FG NOT = "N"
                 MOVE "PLEASE ENTER Y OR N!" TO MESS
                 PERFORM SEND-MESS
                 GO TO CREATE-TIN-FILE-YN.

       EXIT-PARM.
           EXIT.

      ****************************************
       EO-EXEC-AUDIT SECTION.
      ****************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-LN-DATE BEG-PL-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-LN-DATE END-PL-DATE.

      ****************************************
       DISPLAY-PARAMETERS SECTION.
      ****************************************
           IF GROUP-FLAG = "Y"
              MOVE "S  A000BEGBR." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000ENDBR." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BEGBR." TO VDU
              MOVE BEG-BRANCH TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040ENDBR." TO VDU
              MOVE END-BRANCH TO VDUNUM0
              PERFORM SCREENIO.

           MOVE "SNNN020BEGCL." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020ENDCL." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "S  8000LNDATE1." TO VDU.
           MOVE BEG-LN-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8000LNDATE2." TO VDU.
           MOVE END-LN-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE "S  A000PL." TO VDU.
           MOVE INC-PL-FG TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A000PLDATE1." TO VDU.
           MOVE BEG-PL-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  A000PLDATE2." TO VDU.
           MOVE END-PL-DATE TO VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

           MOVE SRC-CODE-TABLE TO MESS.
           MOVE MESS TO WR-BUF.
           MOVE SRC-CODE-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

           MOVE "S  A000DETAILYN." TO VDU.
           MOVE PRINT-DETAIL-FG TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A000PAGEFG." TO VDU.
           MOVE BRANCH-PAGE-FG TO VDUBUF.
           PERFORM SCREENIO.

           MOVE "S  A000TINFG." TO VDU.
           MOVE CREATE-TIN-FG TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       CREATE-TIN-REC SECTION.
      ******************************************************************

           MOVE 1 TO SUB SUB2.
           MOVE "N" TO BAD-NAME-FG.

           MOVE SPACES TO WORK-FNAME WORK-LNAME.

           MOVE BW-FNAME TO WORK-NAME.
       EDIT-FNAME-LOOP.
           IF SUB > 16
              GO TO EDIT-LNAME.

           MOVE NAME-CHAR(SUB) TO TEST-CHAR.
           IF TEST-CHAR NOT ALPHABETIC
               IF TEST-CHAR NOT NUMERIC
                  IF NOT VALID-CHAR
                     MOVE "Y" TO BAD-NAME-FG
                     ADD 1 TO SUB
                     GO TO EDIT-FNAME-LOOP.

           MOVE NAME-CHAR(SUB) TO FNAME-CHAR(SUB2).
           ADD 1 TO SUB SUB2.

           GO TO EDIT-FNAME-LOOP.

       EDIT-LNAME.
           MOVE 1 TO SUB SUB2.
           MOVE BW-LNAME TO WORK-NAME.
       EDIT-LNAME-LOOP.
           IF SUB > 16
              GO TO CREATE-TIN.

           MOVE NAME-CHAR(SUB) TO TEST-CHAR.
           IF TEST-CHAR NOT ALPHABETIC
               IF TEST-CHAR NOT NUMERIC
                  IF NOT VALID-CHAR
                     MOVE "Y" TO BAD-NAME-FG
                     ADD 1 TO SUB
                     GO TO EDIT-LNAME-LOOP.

           MOVE NAME-CHAR(SUB) TO LNAME-CHAR(SUB2).
           ADD 1 TO SUB SUB2.
           GO TO EDIT-LNAME-LOOP.

       CREATE-TIN.
           IF BAD-NAME-FG = "Y"
              ADD 1 TO BAD-NAME-COUNTER.

      *  20190715 BAH MAKE LN-ACCTNO LOOK LIKE A15 IN THE TIN FILE, MOVE
      *               TO A 9(6) FIRST SO THERE ARE NO LEADING ZEROS, QA0050
           MOVE LN-ACCTNO TO WORK-ACCTNO.

           MOVE SPACES TO TIN-REC.
           STRING "2", ";",
           LN-SSNO(1), ";",
           WORK-FNAME, " ", WORK-LNAME, ";",
           LN-OWNBR, WORK-ACCTNO,
           X"0D"
           DELIMITED BY "  "
           INTO TIN-REC.
           IF CREATE-TIN-FG = "Y"
              PERFORM WRITE-TIN-FILE.
       EXIT-CREATE-TIN.
           EXIT.

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************

           ADD 1            TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH        TO H-HR.
           MOVE T-MM        TO H-MIN.
           MOVE HEAD1       TO PR-REC.
           MOVE 99          TO LN-FEED.
           PERFORM WRITE-PR-REC.
      
           MOVE HEAD2       TO PR-REC.
           MOVE 1           TO LN-FEED.
           PERFORM WRITE-PR-REC.
      
           MOVE HEAD3        TO PR-REC.
           MOVE 2           TO LN-FEED.
           PERFORM WRITE-PR-REC.
      
           MOVE 4 TO LN-COUNT.
           MOVE 2 TO LN-FEED.
       END-HEAD-PAGE.
           EXIT.

      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************
           ADD LN-FEED       TO LN-COUNT.

           IF LN-COUNT > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE  TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE 1            TO LN-FEED.
           MOVE SPACES       TO DETAIL-LINE.

      ******************************************************************
       PRINT-RANGE-LINES SECTION.
      ******************************************************************

           MOVE 4                      TO LN-FEED.

           IF GROUP-FLAG = "Y"
              MOVE "SELECTED GROUP        :" TO RANGE-LINE
              MOVE ENT-GROUP                 TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEGINNING BRANCH      :" TO RANGE-LINE
              MOVE BEG-BRANCH                TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
              MOVE "ENDING                :" TO RANGE-LINE
              MOVE END-BRANCH                TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING CLASS       :" TO RANGE-LINE.
           MOVE BEG-CLASS                 TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING                :" TO RANGE-LINE.
           MOVE END-CLASS                 TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING LN DATE     :" TO RANGE-LINE.
           MOVE  BEG-LN-DATE              TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING LN DATE        :" TO RANGE-LINE.
           MOVE  END-LN-DATE              TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "INCLUDE CHARGEOFFS    :" TO RANGE-LINE.
           MOVE  INC-PL-FG                TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEGINNING PL DATE     :" TO RANGE-LINE.
           MOVE  BEG-PL-DATE              TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "ENDING PL DATE        :" TO RANGE-LINE.
           MOVE  END-PL-DATE              TO RANGE-DATE.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SOURCE CODES          :" TO RANGE-LINE.
           STRING SRC-CODE(1) " " SRC-CODE(2) " "
                  SRC-CODE(3) " " SRC-CODE(4) " "
                  SRC-CODE(5) " " SRC-CODE(6) " "
                  SRC-CODE(7) " " SRC-CODE(8) " "
                  SRC-CODE(9) " "
                  DELIMITED BY "  "
              INTO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PRINT DETAIL          :" TO RANGE-LINE.
           MOVE  PRINT-DETAIL-FG          TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "PAGE ON BRANCH        :" TO RANGE-LINE.
           MOVE  BRANCH-PAGE-FG           TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "CREATE TIN MATCH FILE :" TO RANGE-LINE.
           MOVE  CREATE-TIN-FG            TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF CREATE-TIN-FG = "Y"
              MOVE "TIN MATCHING FILE PATH: " TO RANGE-LINE
              MOVE  TIN-PATH                 TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE.

       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPAMTS.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/ARRAYSP.CPY".

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO TINEXT-CONAME.
           MOVE EXT-CONAME-SYSDATE TO TINEXT-SYSDATE.
           DISPLAY TINEXT-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE TINEXT-FORM-FIELDS TO WR-BUF.
           MOVE TINEXT-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE TINEXT-HELP--FILE TO HELP-LINK-FILE.
           MOVE TINEXT-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBSP/TINEXT_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ********************************************
       MISC-ROUTINE SECTION.
      ********************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".

      ********************************************
       IO-ROUTINE SECTION.
      ********************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBWKIN.CPY".

      * COPY "LIBLP/LPLN3RN.CPY".
      * COPYMEMBER: LIBLP/LPLN3RN
      *-----------------------------------------------------------------
      *    STILL NEED TO LOAD THE FILE PATH WORKERS
      *-----------------------------------------------------------------
       LOAD-LN3-FILE.
      *- - - -{ SET OVERRIDE TO A "Y" TO NOT TOUCH THE PATH AT ALL  }- -
           IF ( LN-PATH-OVERRIDE = "Y" ) 
              NEXT SENTENCE
           ELSE
      *- - - -{ SPACES IS THE DEFAULT, NO CROSSING BRANCHES }- - - - - -
           IF ( LN-PATH-OVERRIDE = SPACES ) 
              MOVE EXT-FILPATH-BASE        TO LN-ALL-BASE
              MOVE EXT-FILPATH-MACHINE     TO LN-ALL-MACHINE
              MOVE EXT-FILPATH-DATA        TO LN-ALL-DATA
              MOVE LN-ALL-PATH             TO LN-PATH
           ELSE
      *- - - -{ USE BRANCH VALUES }- - - - - - - - - - - - - - - - - - -
           IF ( LN-PATH-OVERRIDE = "B" ) 
              MOVE FILPATH-BASE            TO LN-ALL-BASE
              MOVE FILPATH-MACHINE         TO LN-ALL-MACHINE
              MOVE FILPATH-DATA            TO LN-ALL-DATA
              MOVE LN-ALL-PATH             TO LN-PATH.

      *-----------------------------------------------------------------
       OPEN-LN3-FILE.
           PERFORM LOAD-LN3-FILE.
           PERFORM OPEN-IT.
           MOVE LN-PATH-SQL TO E-FILE.

           IF LN3-CONN-OPEN-SW = "N" 
               EXEC SQL CONNECT TO :SQL-CONNECT-SERVER AS LN3_CONNECT
                    USER :SQL-CONNECT-USERID USING :SQL-CONNECT-PSWD
               END-EXEC
               MOVE "Y" TO LN3-CONN-OPEN-SW
               EXEC SQL SET CONNECTION C1 END-EXEC.

      *-----------------------------------------------------------------
       START-LN3-FILE.
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.

           IF ( LN3-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LN3-FILE.

           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

      *     SELECT LNFILE.LN_OWNBR,
      *            LNFILE.LN_ACCTNO,
      *            LNFILE.LN_CLASS,
      *            LNFILE.LN_SUBCLASS,
      *            LNFILE.LN_LAWCODE,
      *            LNFILE.LN_SSNO1,
      *            LNFILE.LN_MAKERCD1,
      *            LNFILE.LN_SSNO2,
      *            LNFILE.LN_MAKERCD2,
      *            LNFILE.LN_SSNO3,
      *            LNFILE.LN_MAKERCD3,
      *            LNFILE.LN_SSNO4,
      *            LNFILE.LN_MAKERCD4,
      *            LNFILE.LN_ORGST,
      *            LNFILE.LN_SPRCLASS,
      *            LNFILE.LN_SRCD,
      *            LNFILE.LN_POCD,
      *            LNFILE.LN_LOANTYPE,
      *            LNFILE.LN_STATFLAGS,
      *            CAST(LNFILE.LN_LOANDATE       AS VARCHAR(10)),
      *            CAST(LNFILE.LN_ORIG_1STPYDATE AS VARCHAR(10)),
      *            CAST(LNFILE.LN_1STPYDATE      AS VARCHAR(10)),
      *            CAST(LNFILE.LN_JDDATE         AS VARCHAR(10)),
      *            CAST(LNFILE.LN_PLDATE         AS VARCHAR(10)),
      *            CAST(LNFILE.LN_LEGALDATE      AS VARCHAR(10)),
      *            CAST(LNFILE.LN_DEBTAIDATE     AS VARCHAR(10)),
      *            CAST(LNFILE.LN_LAST_PYPA_DATE AS VARCHAR(10)),
      *            LNFILE.LN_INTCHG,
      *            LNFILE.LN_SERCHG,
      *            LNFILE.LN_EXTCHG,
      *            LNFILE.LN_MAINTFEE,
      *            LNFILE.LN_ORGTERM,
      *            LNFILE.LN_1STPYAMT,
      *            LNFILE.LN_NOREGPY,
      *            LNFILE.LN_REGPYAMT,
      *            LNFILE.LN_LASTPYAMT,
      *            LNFILE.LN_FEECODE_1,
      *            LNFILE.LN_FEECODE_2,
      *            LNFILE.LN_FEECODE_3,
      *            LNFILE.LN_FEECODE_4,
      *            LNFILE.LN_FEECODE_5,
      *            LNFILE.LN_FEECODE_6,
      *            LNFILE.LN_FEECODE_7,
      *            LNFILE.LN_FEECODE_8,
      *            LNFILE.LN_FEECODE_9,
      *            LNFILE.LN_FEECODE_10,
      *            LNFILE.LN_FEECODE_11,
      *            LNFILE.LN_FEECODE_12,
      *            LNFILE.LN_FEECODE_13,
      *            LNFILE.LN_FEECODE_14,
      *            LNFILE.LN_FEECODE_15,
      *            LNFILE.LN_FEECODE_16,
      *            LNFILE.LN_FEECODE_17,
      *            LNFILE.LN_FEECODE_18,
      *            LNFILE.LN_FEECODE_19,
      *            LNFILE.LN_FEECODE_20,
      *            LNFILE.LN_FEEAMT_1,
      *            LNFILE.LN_FEEAMT_2,
      *            LNFILE.LN_FEEAMT_3,
      *            LNFILE.LN_FEEAMT_4,
      *            LNFILE.LN_FEEAMT_5,
      *            LNFILE.LN_FEEAMT_6,
      *            LNFILE.LN_FEEAMT_7,
      *            LNFILE.LN_FEEAMT_8,
      *            LNFILE.LN_FEEAMT_9,
      *            LNFILE.LN_FEEAMT_10,
      *            LNFILE.LN_FEEAMT_11,
      *            LNFILE.LN_FEEAMT_12,
      *            LNFILE.LN_FEEAMT_13,
      *            LNFILE.LN_FEEAMT_14,
      *            LNFILE.LN_FEEAMT_15,
      *            LNFILE.LN_FEEAMT_16,
      *            LNFILE.LN_FEEAMT_17,
      *            LNFILE.LN_FEEAMT_18,
      *            LNFILE.LN_FEEAMT_19,
      *            LNFILE.LN_FEEAMT_20,
      *            LNFILE.LN_FEEOURS_1,
      *            LNFILE.LN_FEEOURS_2,
      *            LNFILE.LN_FEEOURS_3,
      *            LNFILE.LN_FEEOURS_4,
      *            LNFILE.LN_FEEOURS_5,
      *            LNFILE.LN_FEEOURS_6,
      *            LNFILE.LN_FEEOURS_7,
      *            LNFILE.LN_FEEOURS_8,
      *            LNFILE.LN_FEEOURS_9,
      *            LNFILE.LN_FEEOURS_10,
      *            LNFILE.LN_FEEOURS_11,
      *            LNFILE.LN_FEEOURS_12,
      *            LNFILE.LN_FEEOURS_13,
      *            LNFILE.LN_FEEOURS_14,
      *            LNFILE.LN_FEEOURS_15,
      *            LNFILE.LN_FEEOURS_16,
      *            LNFILE.LN_FEEOURS_17,
      *            LNFILE.LN_FEEOURS_18,
      *            LNFILE.LN_FEEOURS_19,
      *            LNFILE.LN_FEEOURS_20,
      *            LNFILE.LN_FEEFINCHG_1,
      *            LNFILE.LN_FEEFINCHG_2,
      *            LNFILE.LN_FEEFINCHG_3,
      *            LNFILE.LN_FEEFINCHG_4,
      *            LNFILE.LN_FEEFINCHG_5,
      *            LNFILE.LN_FEEFINCHG_6,
      *            LNFILE.LN_FEEFINCHG_7,
      *            LNFILE.LN_FEEFINCHG_8,
      *            LNFILE.LN_FEEFINCHG_9,
      *            LNFILE.LN_FEEFINCHG_10,
      *            LNFILE.LN_FEEFINCHG_11,
      *            LNFILE.LN_FEEFINCHG_12,
      *            LNFILE.LN_FEEFINCHG_13,
      *            LNFILE.LN_FEEFINCHG_14,
      *            LNFILE.LN_FEEFINCHG_15,
      *            LNFILE.LN_FEEFINCHG_16,
      *            LNFILE.LN_FEEFINCHG_17,
      *            LNFILE.LN_FEEFINCHG_18,
      *            LNFILE.LN_FEEFINCHG_19,
      *            LNFILE.LN_FEEFINCHG_20,
      *            LNFILE.LN_INSOURS_1,
      *            LNFILE.LN_INSOURS_2,
      *            LNFILE.LN_INSOURS_3,
      *            LNFILE.LN_INSOURS_4,
      *            LNFILE.LN_INSOURS_5,
      *            LNFILE.LN_INSOURS_6,
      *            LNFILE.LN_INSOURS_7,
      *            LNFILE.LN_INSOURS_8,
      *            LNFILE.LN_INSOURS_9,
      *            LNFILE.LN_INSOURS_10,
      *            LNFILE.LN_INSPREM_1,
      *            LNFILE.LN_INSPREM_2,
      *            LNFILE.LN_INSPREM_3,
      *            LNFILE.LN_INSPREM_4,
      *            LNFILE.LN_INSPREM_5,
      *            LNFILE.LN_INSPREM_6,
      *            LNFILE.LN_INSPREM_7,
      *            LNFILE.LN_INSPREM_8,
      *            LNFILE.LN_INSPREM_9,
      *            LNFILE.LN_INSPREM_10,
      *            LNFILE.LN_INSFINCHG_1,
      *            LNFILE.LN_INSFINCHG_2,
      *            LNFILE.LN_INSFINCHG_3,
      *            LNFILE.LN_INSFINCHG_4,
      *            LNFILE.LN_INSFINCHG_5,
      *            LNFILE.LN_INSFINCHG_6,
      *            LNFILE.LN_INSFINCHG_7,
      *            LNFILE.LN_INSFINCHG_8,
      *            LNFILE.LN_INSFINCHG_9,
      *            LNFILE.LN_INSFINCHG_10,
      *            LNFILE.LN_CURBAL,
      *            LNFILE.LN_ANTICERN_1,
      *            LNFILE.LN_ANTICERN_2,
      *            LNFILE.LN_ANTICERN_3,
      *            LNFILE.LN_ANTICERN_4,
      *            LNFILE.LN_ANTICERN_5,
      *            LNFILE.LN_ANTICERN_6,
      *            LNFILE.LN_ANTICERN_7,
      *            LNFILE.LN_ANTICERN_8,
      *            LNFILE.LN_ANTICERN_9,
      *            LNFILE.LN_ANTICERN_10,
      *            LNFILE.LN_TOTNODEF,
      *            LNFILE.LN_CREDITLIM,
      *            LNFILE.LN_SUM_ORGTERM,
      *            LNFILE.LN_UNITPER_CD,
      *            LNFILE.LN_UNITPER_FREQ,
      *            ISNULL(BWFILE.BW_OWNBR,0),
      *            ISNULL(BWFILE.BW_SSNO,0),
      *            ISNULL(BWFILE.BW_LNAME,''),
      *            ISNULL(BWFILE.BW_FNAME,''),
      *            ISNULL(BWFILE.BW_ZIP,''),
      *            ISNULL(BWFILE.BW_SOLICIT,'')
      * FROM DBO.LNFILE LNFILE
      * LEFT OUTER JOIN DBO.BWFILE BWFILE
      *  ON LNFILE.LN_OWNBR = BWFILE.BW_OWNBR	
      *  AND LNFILE.LN_SSNO1 = BWFILE.BW_SSNO  
      *WHERE LNFILE.LN_OWNBR     = @QLN3_WBEG_OWNBR
      *  AND LNFILE.LN_CLASS BETWEEN @QLN3_WBEG_CLASS AND @QLN3_WEND_CLASS
      *  AND LNFILE.LN_POCD NOT IN ('*I','*R','**', '*U')
      *  AND LNFILE.LN_LOANDATE BETWEEN @BEG_LOANDATE AND @END_LOANDATE
      *  AND LNFILE.LN_PLDATE   BETWEEN @BEG_PLDATE   AND @END_PLDATE
      *ORDER BY LNFILE.LN_CLASS, LNFILE.LN_ACCTNO

           EXEC SQL
            DECLARE LN3_CURSOR CURSOR FOR
                :LN3-RETURN-CODE = 
                 EXEC SSP_TINEXT_LNFILE_SELECT
                  (:QLN3-WBEG-OWNBR,
                   :QLN3-WBEG-CLASS,
                   :QLN3-WEND-CLASS,
                   :BEG-LOANDATE,
                   :END-LOANDATE,
                   :BEG-PLDATE,
                   :END-PLDATE)
                END-EXEC.

           EXEC SQL
               OPEN LN3_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           MOVE STAT---GOOD TO LN3-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LN3-FILE-NEXT.
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS,LN-ACCTNO
              DELIMITED BY " " INTO E-KEYX.
           INITIALIZE QLN-REC QBW-REC.

           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

           IF ( LN3-CURSOR-STAT-GOOD )
              EXEC SQL
                FETCH LN3_CURSOR INTO 
                   :QLN-OWNBR,
                   :QLN-ACCTNO,
                   :QLN-CLASS,
                   :QLN-SUBCLASS,
                   :QLN-LAWCODE,
                   :QLN-SSNO1,
                   :QLN-MAKERCD1,
                   :QLN-SSNO2,
                   :QLN-MAKERCD2,
                   :QLN-SSNO3,
                   :QLN-MAKERCD3,
                   :QLN-SSNO4,
                   :QLN-MAKERCD4,
                   :QLN-ORGST,
                   :QLN-SPRCLASS,
                   :QLN-SRCD,
                   :QLN-POCD,
                   :QLN-LOANTYPE,
                   :QLN-STATFLAGS,
                   :QLN-LOANDATE,
                   :QLN-ORIG-1STPYDATE,
                   :QLN-1STPYDATE,
                   :QLN-JDDATE,
                   :QLN-PLDATE,
                   :QLN-LEGALDATE,
                   :QLN-DEBTAIDATE,
                   :QLN-LAST-PYPA-DATE,
                   :QLN-INTCHG,
                   :QLN-SERCHG,
                   :QLN-EXTCHG,
                   :QLN-MAINTFEE,
                   :QLN-ORGTERM,
                   :QLN-1STPYAMT,
                   :QLN-NOREGPY,
                   :QLN-REGPYAMT,
                   :QLN-LASTPYAMT,
                   :QLN-FEECODE-1,
                   :QLN-FEECODE-2,
                   :QLN-FEECODE-3,
                   :QLN-FEECODE-4,
                   :QLN-FEECODE-5,
                   :QLN-FEECODE-6,
                   :QLN-FEECODE-7,
                   :QLN-FEECODE-8,
                   :QLN-FEECODE-9,
                   :QLN-FEECODE-10,
                   :QLN-FEECODE-11,
                   :QLN-FEECODE-12,
                   :QLN-FEECODE-13,
                   :QLN-FEECODE-14,
                   :QLN-FEECODE-15,
                   :QLN-FEECODE-16,
                   :QLN-FEECODE-17,
                   :QLN-FEECODE-18,
                   :QLN-FEECODE-19,
                   :QLN-FEECODE-20,
                   :QLN-FEEAMT-1,
                   :QLN-FEEAMT-2,
                   :QLN-FEEAMT-3,
                   :QLN-FEEAMT-4,
                   :QLN-FEEAMT-5,
                   :QLN-FEEAMT-6,
                   :QLN-FEEAMT-7,
                   :QLN-FEEAMT-8,
                   :QLN-FEEAMT-9,
                   :QLN-FEEAMT-10,
                   :QLN-FEEAMT-11,
                   :QLN-FEEAMT-12,
                   :QLN-FEEAMT-13,
                   :QLN-FEEAMT-14,
                   :QLN-FEEAMT-15,
                   :QLN-FEEAMT-16,
                   :QLN-FEEAMT-17,
                   :QLN-FEEAMT-18,
                   :QLN-FEEAMT-19,
                   :QLN-FEEAMT-20,
                   :QLN-FEEOURS-1,
                   :QLN-FEEOURS-2,
                   :QLN-FEEOURS-3,
                   :QLN-FEEOURS-4,
                   :QLN-FEEOURS-5,
                   :QLN-FEEOURS-6,
                   :QLN-FEEOURS-7,
                   :QLN-FEEOURS-8,
                   :QLN-FEEOURS-9,
                   :QLN-FEEOURS-10,
                   :QLN-FEEOURS-11,
                   :QLN-FEEOURS-12,
                   :QLN-FEEOURS-13,
                   :QLN-FEEOURS-14,
                   :QLN-FEEOURS-15,
                   :QLN-FEEOURS-16,
                   :QLN-FEEOURS-17,
                   :QLN-FEEOURS-18,
                   :QLN-FEEOURS-19,
                   :QLN-FEEOURS-20,
                   :QLN-FEEFINCHG-1,
                   :QLN-FEEFINCHG-2,
                   :QLN-FEEFINCHG-3,
                   :QLN-FEEFINCHG-4,
                   :QLN-FEEFINCHG-5,
                   :QLN-FEEFINCHG-6,
                   :QLN-FEEFINCHG-7,
                   :QLN-FEEFINCHG-8,
                   :QLN-FEEFINCHG-9,
                   :QLN-FEEFINCHG-10,
                   :QLN-FEEFINCHG-11,
                   :QLN-FEEFINCHG-12,
                   :QLN-FEEFINCHG-13,
                   :QLN-FEEFINCHG-14,
                   :QLN-FEEFINCHG-15,
                   :QLN-FEEFINCHG-16,
                   :QLN-FEEFINCHG-17,
                   :QLN-FEEFINCHG-18,
                   :QLN-FEEFINCHG-19,
                   :QLN-FEEFINCHG-20,
                   :QLN-INSOURS-1,
                   :QLN-INSOURS-2,
                   :QLN-INSOURS-3,
                   :QLN-INSOURS-4,
                   :QLN-INSOURS-5,
                   :QLN-INSOURS-6,
                   :QLN-INSOURS-7,
                   :QLN-INSOURS-8,
                   :QLN-INSOURS-9,
                   :QLN-INSOURS-10,
                   :QLN-INSPREM-1,
                   :QLN-INSPREM-2,
                   :QLN-INSPREM-3,
                   :QLN-INSPREM-4,
                   :QLN-INSPREM-5,
                   :QLN-INSPREM-6,
                   :QLN-INSPREM-7,
                   :QLN-INSPREM-8,
                   :QLN-INSPREM-9,
                   :QLN-INSPREM-10,
                   :QLN-INSFINCHG-1,
                   :QLN-INSFINCHG-2,
                   :QLN-INSFINCHG-3,
                   :QLN-INSFINCHG-4,
                   :QLN-INSFINCHG-5,
                   :QLN-INSFINCHG-6,
                   :QLN-INSFINCHG-7,
                   :QLN-INSFINCHG-8,
                   :QLN-INSFINCHG-9,
                   :QLN-INSFINCHG-10,
                   :QLN-CURBAL,
                   :QLN-ANTICERN-1,
                   :QLN-ANTICERN-2,
                   :QLN-ANTICERN-3,
                   :QLN-ANTICERN-4,
                   :QLN-ANTICERN-5,
                   :QLN-ANTICERN-6,
                   :QLN-ANTICERN-7,
                   :QLN-ANTICERN-8,
                   :QLN-ANTICERN-9,
                   :QLN-ANTICERN-10,
                   :QLN-TOTNODEF,
                   :QLN-CREDITLIM,
                   :QLN-SUM-ORGTERM,
                   :QLN-UNITPER-CD,
                   :QLN-UNITPER-FREQ,
                   :QBW-OWNBR,
                   :QBW-SSNO,
                   :QBW-LNAME,
                   :QBW-FNAME,
                   :QBW-ZIP,
                   :QBW-SOLICIT
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN3-FILE-NEXT.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           IF ( IO-FG = 0 )
              PERFORM GET-LN-FIELDS
              PERFORM GET-BW-FIELDS.

      *-----------------------------------------------------------------
       CLOSE-LN3-FILE.

           PERFORM CLOSE-IT.

           IF ( LN3-CURSOR-STAT-GOOD )
              EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC
              EXEC SQL
                   CLOSE LN3_CURSOR
              END-EXEC
              EXEC SQL SET CONNECTION C1 END-EXEC
              MOVE STAT---BAD TO LN3-CURSOR-STAT.

      *-----------------------------------------------------------------
       CLOSE-LN3-CONNECTION.

           IF LN3-CONN-OPEN-SW = "Y"
               EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC
               EXEC SQL COMMIT END-EXEC
               EXEC SQL DISCONNECT LN3_CONNECT END-EXEC
               EXEC SQL SET CONNECTION C1 END-EXEC
               MOVE "N" TO LN3-CONN-OPEN-SW.
      *-----------------------------------------------------------------

       OPEN-TIN-FILE-OUTPUT.
           PERFORM OPEN-IT.
           MOVE TIN-PATH TO E-FILE.
           OPEN OUTPUT TIN-FILE.
           IF IO-FG = 8
              GO TO OPEN-TIN-FILE-OUTPUT.
           IF IO-FG = 7
              CLOSE TIN-FILE
              GO TO OPEN-TIN-FILE-OUTPUT.
       WRITE-TIN-FILE.
           PERFORM WRITE-IT.
           MOVE TIN-PATH TO E-FILE.
           WRITE TIN-REC.
           IF IO-FG = 8
              GO TO WRITE-TIN-FILE.
       CLOSE-TIN-FILE.
           PERFORM CLOSE-IT.
           MOVE TIN-PATH TO E-FILE.
           CLOSE TIN-FILE.
       COPY "LIBGB/FERRORS.CPY".

      ********************************************
       END-PROGRAM SECTION.
      ********************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

       EXIT-PROG.
           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.
           IF EXT-ROUTE-BUF = "00"
              MOVE "SP/EXTRMU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY TINEXT-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
