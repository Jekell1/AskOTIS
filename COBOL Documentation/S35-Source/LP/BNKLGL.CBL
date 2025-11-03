       IDENTIFICATION DIVISION.
       PROGRAM-ID.   BNKLGL.
       DATE-WRITTEN. 060110.
      ******************************************************************
      * WALKS ACROSS BRANCHES
      *       DESCRIPTION:  REPOSSESSION/LEGAL REPORT
      *
      *   IF BOTH BK AND LEGAL, BK STATUS TAKES PRIORITY
      *   IF INCLUDE BK AND SKIP LEGAL, AND ACCOUNT IS BOTH BK AND LEGAL, IT
      *      WILL BE INCLUDED
      *   IF SKIP BK AND INCLUDE LEGAL, AND ACCOUNT IS BOTH BK AND LEGAL, IT
      *      WILL NOT BE INCLUDED BECAUSE BK SKIP TAKES PRIORITY
      *
      *=================================================================
      * REV:
      * BAH 060108 NEW FOR WORLD, PR# 452
      * BAH 061215 SCREEN HAD F3 FOR DATE RANGE BUT DIDNT ALLOW IT, SO
      *            ALLOW IT ! SUPPORT PL# 563
      * BLF 070827 START ON LOAN REC DIDN'T INITIALIZE LN-SEQ, SO
      *            RESULTS WERE UNPREDICTABLE
      * BAH 160125 SPLIT OUT LTFILE, PD0003
      * BAH 170509 ACTIVATE 8 DIGIT DATES, PD0006
      *
      * KEC 2017.0605 {PR#00000} PARADATA <A30> [JAY CISZEWSKI]
      *          ADDED 'LN' FILE PREFIX FOR WORKING STORAGE FIELDS.
      *
      * KEC 2018.0423 PR#00000 PARADATA A30 JAY & BETH
      *         REMOVED USE OF LOCAL-PRINT-FG SINCE NOT USED;
      *         LOCAL-PRINT-FG NOT BEING SET; IN EOBUF WHICH IS EXTERNAL
      *         WHERE IF NOT BEING SET WILL CAUSE ISSUES FROM THAT
      *         LOCAL-PRINT-FG BYTE POSITION COULD BE SET FROM PREVIOUS
      *         PROGRAM WITH EOBUF THAT SET SOMETHING IN THAT BYTE.
      *
      * BAH 181203 GLOBAL LNFILE, L2G
      * BAH 20190920 REMOVED ACCESS-CALL ON LNFILE
      * BAH 20200827 REMOVED RESET ON BR1 
      * MB  20240220 PERFORMANCE TUNING - ADDED STORED PROCEDURE
      *               ADDED STATE PARAMETER ARRAY PROCESSING
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBGB/GBFSWKI.CPY".
       I-O-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       COPY "LIBGB/GBFDPR.CPY".
      ******************************************************************
      *  SUMMARY TOTALS WORK RECORD
      ******************************************************************
       FD  WK-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK-REC.
           03  WK-KEY.
               05  WK-BR    PIC 9(4).
               05  WK-CLASS PIC 9(3).
           03  WK-NOLOANS         PIC S9(7)     COMP-3.

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/BNKLGL.CBL J35 12/21/23-08:46:47 >".
      ****************************
      *     F I L E   I F N      *
      ****************************
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
       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LC.CPY".
       COPY "LIBLP/LP01LC_SQL.CPY".
       COPY "LIBLP/LPWSLC.CPY".
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".

       01  LN3-CONN-OPEN-SW            PIC X     VALUE "N".

       01  WK-IFN                      PIC X(4)  VALUE "WK".

      *-----------------------------------------------------------------
      * LINE WORKERS
      *-----------------------------------------------------------------
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  ER-FEED              PIC 9        VALUE 1.

       01  WS-GTT-TOTAL-TABLE.
           03  WS-GTT-TABLE-GROUP-LEVEL                 OCCURS 10 TIMES.
               05  WS-GTT-NOLOANS         PIC S9(7)     COMP.

       01  WS-HOLD-PROMPT       PIC X(7)  VALUE SPACES.
      ******************************************************************
      *  TOTALS LEV1 = CLASS TOTALS
      *            2 = BRANCH TOTALS
      *            3 = GRAND TOTALS
      ******************************************************************
       01  TOTALS-TAB.
           03  TOTAL-TBL       OCCURS 3.
               05  T-NOLOANS         PIC S9(7)     COMP-3.

      *-----------------------------------------------------------------
      * MISC WORKERS
      *-----------------------------------------------------------------
       01  MSEL-LIST            PIC X(5)      VALUE "MSEL.".
       01  SUB                  PIC 9(2)  COMP.
       01  BANKRUPT-TEST        PIC X VALUE SPACES.
       01  LEGAL-TEST           PIC X VALUE SPACES.
       01  NUM-BW               PIC 9(9).
       01  NUM-BW-WK            REDEFINES NUM-BW.
           03  NUM-BW1          PIC 999.
           03  NUM-BW2          PIC 99.
           03  NUM-BW3          PIC 9999.
       01  ALP-BW.
           03  ALP-BW1          PIC 999.
           03  ALP-HYPHEN1      PIC X.
           03  ALP-BW2          PIC 99.
           03  ALP-HYPHEN2      PIC X.
           03  ALP-BW3          PIC 9999.
       01  NAME-WORKER          PIC X(34).

       01  HOLD-TODAY-DATE      PIC 9(8)  VALUE 0.
       01  HOLD-BRNO            PIC 9(4)  VALUE 0.
       01  HOLD-BRNAME          PIC X(20) VALUE SPACES.
       01  HOLD-LN-CLASS        PIC 9(3)  VALUE 0.

       01  SAVE-WK-KEY.
           03  SAVE-WK-BR       PIC 9(4).
           03  SAVE-WK-CL       PIC 9(3).

       01  RECORDS-FOUND        PIC X     VALUE " ".
           88  NO-RECORDS-FOUND           VALUE "N".
           88  SKIP-RECORDS-FOUND         VALUE " ".

       01  C-S-Z           PIC X(66).
       01  STATE           PIC XX.
       01  ZIP             PIC X(10).

       01  FIRST-NAME           PIC X(16).

       01  FIRST-TIME           PIC X        VALUE "Y".
       01  MESS-LIST            PIC X(5)     VALUE "MESS.".


       01  LEGEND               PIC X(70).
       COPY "LIBLP/EMSFRG_CMD.CPY".
       01  LEGEND-DTE-01.
           03  FILLER           PIC X(70)  VALUE
               " F7-EXIT, ^, V, ENTER BEGINNING PAYOFF DATE:".
       01  LEGEND-DTE-02.
           03  FILLER           PIC X(70)  VALUE
               " F7-EXIT, ^, V, ENTER ENDING PAYOFF DATE:".
       01  LEGEND-AC-01.
           03  FILLER           PIC X(40)  VALUE
               " F7-EXIT, ^, V, ENTER BEGINNING ACCOUNT ".
           03  FILLER           PIC X(17)  VALUE
               "NUMBER. (F3-ALL):".
       01  LEGEND-AC-02.
           03  FILLER           PIC X(37)  VALUE
               " F7-EXIT, ^, V, ENTER ENDING ACCOUNT ".
           03  FILLER           PIC X(8)  VALUE
               "NUMBER.:".

      *-----------------------------------------------------------------
      *     S C R E E N   F I E L D S   *
      *-----------------------------------------------------------------
       COPY "LIBGB/EOBUF_EXT.CPY".
           03  BEG-BRANCH       PIC 9(4).
           03  END-BRANCH       PIC 9(4).
           03  BEG-CLASS        PIC 9(3).
           03  END-CLASS        PIC 9(3).
           03  BEG-DATE         PIC 9(8).
           03  END-DATE         PIC 9(8).
           03  INCLUDE-BANKRUPT PIC X.
           03  INCLUDE-LEGAL    PIC X.
           03  GROUP-FLAG       PIC X.
           03  ENT-GROUP        PIC X(4).
           03  GTT-ISO          PIC X.
           03  TOTAL-LEV-TABLE.
               05  TOTAL-LEVEL-TABLE PIC X OCCURS 10.

       01  VERSION-CONTROL     PIC X VALUE "A".

       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.

      *-----------------------------------------------------------------
      *     P R I N T   B U F F E R S   *
      *-----------------------------------------------------------------

       01  HEAD1.
           03  H-DATE           PIC X(8)     VALUE " ".
           03  FILLER           PIC XX       VALUE " ".
           03  H-HR             PIC 99       VALUE 0.
           03  FILLER           PIC X        VALUE ":".
           03  H-MIN            PIC 99       VALUE 0.
           03  FILLER           PIC X(8)    VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(8)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

       01  HEAD2.
           03  FILLER           PIC X(10)    VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X(15)    VALUE " ".
           03  FILLER           PIC X(35) VALUE
               "BANKRUPT / LEGAL REPORT".

       01  HEAD3.
           03  FILLER           PIC X(10) VALUE "BRANCH :".
           03  H-BRNO           PIC ZZZ9.
           03  FILLER           PIC X VALUE " ".
           03  H-BRDESC         PIC X(20).

       01  HEAD4.
           03  FILLER           PIC X(10) VALUE "CLASS  :".
           03  H-LCNO           PIC 99.
           03  FILLER           PIC X VALUE " ".
           03  H-LCDESC         PIC X(20).

       01  HEAD5.
           03  FILLER             PIC X(66) VALUE  SPACES.
           03  FILLER             PIC X(12) VALUE  "   LEGAL".

       01  HEAD6.
           03  FILLER             PIC X(19) VALUE  "ACCOUNT #".
           03  FILLER             PIC X(23) VALUE  "NAME".
           03  FILLER             PIC X(13) VALUE  "LOANDATE".
           03  FILLER             PIC X(11) VALUE  "BALANCE".
           03  FILLER             PIC X(11) VALUE  "BANKRUPT".
           03  FILLER             PIC X(4)  VALUE  "CON".


       01  DETAIL-LINE        PIC X(80)  VALUE SPACES.
       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  D-SLOAN-NUM    PIC X(16).
           03  FILLER         PIC X(3).
           03  D-SNAME        PIC X(20).
           03  FILLER         PIC X(3).
           03  D-SLNDATE      PIC 99/99/99     BLANK ZERO.
           03  FILLER         PIC X(3).
           03  D-SBALANCE     PIC Z(6).99-    BLANK ZERO.
           03  FILLER         PIC XX.
           03  D-SDATE        PIC 99/99/99     BLANK ZERO.
           03  D-STYPE        PIC X.
           03  FILLER         PIC X(2).
           03  D-SCONX.
               05  D-SCON     PIC ZZZ9         BLANK ZERO.

       01  D-LINE2 REDEFINES DETAIL-LINE.
           03  G-TOTNAME        PIC X(14).
           03  G-GROUP-2 REDEFINES G-TOTNAME.
               05  G-PROMPT1        PIC X(7).
               05  FILLER           PIC X.
               05  G-GRNO           PIC X(4).
               05  G-BRNO           REDEFINES G-GRNO
                                    PIC 9(4).
               05  FILLER           PIC X.
               05  G-GR-LEVEL       PIC 9(1).
           03  FILLER           PIC X(01).
           03  G-HEADING1       PIC X(12).
           03  FILLER           PIC X(03).
           03  G-LOANS          PIC ZZZZZ9-    BLANK WHEN ZERO.
           03  FILLER           PIC X(03).

      *-----------------------------------------------------------------
       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(35).
           03  RANGE-SELECTION.
               04  R-NUM        PIC Z(9)9.
               04  R-NUM-REDEF2 REDEFINES R-NUM.
                   05  R-DATE   PIC 9999/99/99.

       COPY "LIBGB/PASSWDW.CPY".
       COPY "LIBWI/GRTOTLW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBLP/LPAPRSW.CPY".
       COPY "LIBLP/LPITRMW.CPY".
       COPY "LIBLP/LPAPRZW.CPY".
       COPY "LIBLP/LPUPERW.CPY".
       COPY "LIBLP/LPCEPPW.CPY".
       COPY "LIBLP/LPPDUEW.CPY".
       COPY "LIBGB/SPPDTHW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBLP/SETCTBLW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/BRSECUREW.CPY".
       COPY "LIBLP/LPAMTSW.CPY".
       COPY "LIBGB/AGEINGW.CPY".
       COPY "LIBLP/LPAVCRW.CPY".
       COPY "LIBLP/LPMDTEW.CPY".
       COPY "LIBLP/LPCDELW.CPY".
       COPY "LIBLP/LPWDELW.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBLP/LPSTATW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/RBACTW.CPY".
       COPY "LIBGB/GPENVW.CPY".

       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/BNKLGL_DEF.CPY".
       COPY "LIBLP/BNKLGL_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBLP/ARRAYSPW.CPY". 

       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/BNKLGL_SCN.CPY".

      /*****************************************************************
      * P R O G R A M   C O N T R O L
      ******************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-LC1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           PERFORM CLOSE-LN3-CONNECTION.
           CLOSE WK-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *****************************************
       MAIN-MODULE SECTION.
      *****************************************

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BNKLGL" TO FORM-PROG VDU-FORM-NAME.

           PERFORM GET-GPENV.

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
           IF NOT IO-GOOD
              GO TO IO-ERROR.
           GO TO MOVE-BRANCH-PATH-INFO.
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-BAD OR BR-NO > END-BRANCH
              GO TO END-ROUTINE.
       MOVE-BRANCH-PATH-INFO.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE
                                              LTP-PATH-OVERRIDE.

           PERFORM OPEN-BW1-FILE.
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

           IF EXT-ROUTE-BUF = "00"
              STRING "REPORT IN PROGRESS............", BR-NO INTO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           PERFORM START-LN3-FILE.  *> SSP - LN BW

      *-----------------------------------------------------------------
       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT.  *> SSP - LN BW
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO END-OF-GROUP.
 
           IF LN-POCD-NONLOAN
              GO TO NEXT-LN.

           IF LN-CLASS > END-CLASS
              GO TO END-OF-GROUP.

           IF LN-BNKRPTDATE = 0 AND LN-LEGALDATE = 0
              GO TO NEXT-LN.

           IF INCLUDE-BANKRUPT = "N"
              IF LN-BNKRPTDATE NOT = 0
                 GO TO NEXT-LN.
           IF INCLUDE-BANKRUPT = "Y"
              IF LN-BNKRPTDATE = 0
                 GO TO NEXT-LN.

      * AT THIS POINT, YOU HAVE A VALID ACCOUNT BASED UPON
      * BANKRUPT STATUS.
      * IF BOTH DATES SET, AND INCLUDE BK AND SKIP LEGAL, THE
      * ACCOUNT WOULD BE INCLUDED.
      * IF BOTH DATES SET, AND SKIPPING BANKRUPT AND INCLUDE LEGAL,
      * IT WILL BE SKIPPED.
      * EFFECTIVE DATED REBATES
           IF INCLUDE-LEGAL = "N"
              IF LN-LEGALDATE NOT = 0
                 IF (LN-BNKRPTDATE NOT = 0) AND
                        (INCLUDE-BANKRUPT = "Y")
                    NEXT SENTENCE
                 ELSE
                    GO TO NEXT-LN.

           IF INCLUDE-LEGAL = "N"
              IF INCLUDE-BANKRUPT = "Y"
                 IF LN-BNKRPTDATE < BEG-DATE OR LN-BNKRPTDATE > END-DATE
                    GO TO NEXT-LN.

           IF INCLUDE-BANKRUPT = "N"
              IF INCLUDE-LEGAL = "Y"
                 IF LN-LEGALDATE < BEG-DATE OR LN-LEGALDATE > END-DATE
                    GO TO NEXT-LN.

           MOVE "Y" TO LEGAL-TEST BANKRUPT-TEST.

      * ONLY OTHER OPTION IS BOTH FLAGS SET TO "Y"
      * ONLY ONE HAS TO BE WITHIN RANGE
           IF LN-BNKRPTDATE = 0
              MOVE "N" TO BANKRUPT-TEST
           ELSE
              IF LN-BNKRPTDATE < BEG-DATE OR LN-BNKRPTDATE > END-DATE
                 MOVE "N" TO BANKRUPT-TEST.

           IF LN-LEGALDATE = 0
              MOVE "N" TO LEGAL-TEST
           ELSE
              IF LN-LEGALDATE < BEG-DATE OR LN-LEGALDATE > END-DATE
                 MOVE "N" TO LEGAL-TEST.


      * NEITHER PASSED DATE RANGE TEST
           IF LEGAL-TEST = "N" AND BANKRUPT-TEST = "N"
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

      *-----------------------------------------------------------------
      * TEST FIRST TIME THROUGH
      *-----------------------------------------------------------------
       CONTINUE-PROCESSING.
           IF FIRST-TIME = "Y"
              MOVE BR-NO TO HOLD-BRNO
              MOVE BR-NAME TO HOLD-BRNAME
              MOVE LN-CLASS TO HOLD-LN-CLASS
              PERFORM SET-CLASS-HEADINGS
              PERFORM SET-BRANCH-HEADINGS.

      *-----------------------------------------------------------------
      * TEST BREAK IN BRANCH
      *-----------------------------------------------------------------
           IF BR-NO NOT = HOLD-BRNO
              PERFORM CLASS-TOTALS
              PERFORM BRANCH-TOTALS
              MOVE BR-NO TO HOLD-BRNO
              MOVE BR-NAME TO HOLD-BRNAME
              MOVE LN-CLASS TO HOLD-LN-CLASS
              PERFORM SET-CLASS-HEADINGS
              PERFORM SET-BRANCH-HEADINGS
              PERFORM HEAD-PAGE.

      *-----------------------------------------------------------------
      * TEST BREAK IN CLASS
      *-----------------------------------------------------------------
           IF LN-CLASS NOT = HOLD-LN-CLASS
              PERFORM CLASS-TOTALS
              MOVE LN-CLASS TO HOLD-LN-CLASS
              PERFORM SET-CLASS-HEADINGS
              PERFORM HEAD-PAGE.

           PERFORM LN-DETAIL1.

           GO TO NEXT-LN.

      *-----------------------------------------------------------------
       END-OF-GROUP.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-BW1-FILE.

           GO TO NEXT-GROUP-LOC.

      *-----------------------------------------------------------------
       END-ROUTINE.
           IF FIRST-TIME = "Y"
              GO TO END-PROGRAM.

           PERFORM CLASS-TOTALS
           PERFORM BRANCH-TOTALS
           PERFORM GRAND-TOTALS
           IF ( GTT-ISO NOT = "S" )
              MOVE 99 TO LN-COUNT
              PERFORM GROUP-TOTAL-TABLE
              PERFORM PRINT-RANGE-LINES
           ELSE
              PERFORM PRINT-RANGE-LINES.


           GO TO END-PROGRAM.

      *****************************************
       INITIALIZATION SECTION.
      *****************************************
           MOVE BNKLGL-QUEUE-POS TO Q-POS.
           MOVE BNKLGL-COPIES-POS TO C-POS.
           MOVE BNKLGL-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           MOVE 0 TO PAGE-NUMBER.

       SET-HEAD.
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO HOLD-TODAY-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.


           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS............" TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           IF BEG-BRANCH = 0
              IF END-BRANCH = 0
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH
                                             END-BRANCH.

           MOVE 0 TO T-NOLOANS(1) T-NOLOANS(2) T-NOLOANS(3).

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           IF STAT NOT = "00"
              MOVE "CANNOT MAKE SORT KEY FILE" TO MESS
              PERFORM SEND-MESS
              GO TO ENDIT.
           PERFORM OPEN-WK-FILE-OUTPUT.
           PERFORM CLOSE-WK-FILE.
           PERFORM OPEN-WK-FILE.

           PERFORM OPEN-PR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-SP1-FILE.
           PERFORM OPEN-LC1-FILE.

           MOVE "N" TO RECORDS-FOUND.

           PERFORM ARRAYSP-LOAD.

           IF ( GROUP-FLAG = "Y" )
              MOVE LOW-VALUES TO WS-GTT-TOTAL-TABLE
              MOVE ENT-GROUP TO GR-ID
              PERFORM SEARCH-GR-INIT
              GO TO EXIT-INITIALIZE
           ELSE
              MOVE SPACES TO TOTAL-LEV-TABLE
              MOVE "Y"    TO TOTAL-LEVEL-TABLE(9).

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-DATE.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.
           IF VDUSTATUS = "1U"
              GO TO ENTER-LEGAL.

      *-----------------------------------------------------------------
       BR-01.
           MOVE LEGEND-BR-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              MOVE "WI/GRSCAN" TO FORM-NAM
              CALL FORM-PROGX USING FORM-PATH GRSCAN-BUF EXIT-PATHNAME
              CANCEL FORM-PROGX
              IF GRSCAN-STATUS NOT = "00"
                 GO TO BR-01
              ELSE
                 MOVE "S  A000BR1." TO VDU
                 MOVE GRSCAN-KEY TO VDUBUF GR1-KEY
                 PERFORM SCREENIO
                 MOVE "00" TO VDUSTATUS.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-01.

           MOVE VDUBUF TO WGR-WORD ENT-GROUP.
           IF WGR-WORD-ZERO
              MOVE 0 TO WGR-SRESULT
              GO TO BRANCH-IS-NUMERIC.
           MOVE 0 TO WGR-MIN-LEVEL.
           PERFORM CHECK-MEMBER.
           IF WGR-ERR > 0
              MOVE WGR-MSG(WGR-ERR) TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           IF WGR-TYPE = "A"
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
              MOVE "Y" TO GROUP-FLAG
              MOVE 0 TO BEG-BRANCH
              MOVE 9999 TO END-BRANCH
              GO TO CHECK-BRANCH-SECURITY.
       BRANCH-IS-NUMERIC.
           MOVE "N" TO GROUP-FLAG.
           MOVE WGR-SRESULT TO BEG-BRANCH.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BRANCH TO VDUNUM0.
           PERFORM SCREENIO.
      *-----------------------------------------------------------------
       BR-02.
           MOVE LEGEND-BR-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN040BR2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BR-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BR-02.
           MOVE VDUNUM0 TO END-BRANCH.
           IF BEG-BRANCH > END-BRANCH
              MOVE "INVALID BRANCH RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BR-01.
           GO TO CHECK-BRANCH-SECURITY.

       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BRANCH VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.
      *----------------------------------------------------------------
       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

      *-----------------------------------------------------------------
       CL-01.
           MOVE LEGEND-CL-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020CL1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-CL.
           IF VDUSTATUS = "1U" GO TO BR-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-01.
           MOVE VDUNUM0 TO BEG-CLASS.
      *-----------------------------------------------------------------
       CL-02.
           MOVE LEGEND-CL-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ENNN020CL2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO CL-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO CL-02.
           MOVE VDUNUM0 TO END-CLASS.
           IF BEG-CLASS > END-CLASS
              MOVE "INVALID CLASS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO CL-01.
           GO TO BEGIN-DATE.
       ALL-CL.
           MOVE "SNNN020CL1." TO VDU.
           MOVE 0 TO BEG-CLASS VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN020CL2." TO VDU.
           MOVE 99 TO END-CLASS VDUNUM0.
           PERFORM SCREENIO.
      *-----------------------------------------------------------------
       BEGIN-DATE.
           IF EXT-ROUTE-BUF NOT = "00" GO TO ENTER-BANKRUPT.

           MOVE LEGEND-DTE-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ERNM060BEGDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-DATE.
           IF VDUSTATUS = "1U" GO TO CL-02.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO BEGIN-DATE.
           MOVE VDU-DATE-YYYYMMDD TO BEG-DATE.
      *-----------------------------------------------------------------
       ENDING-DATE.
           IF EXT-ROUTE-BUF NOT = "00" GO TO ENTER-BANKRUPT.
           MOVE LEGEND-DTE-02 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "ERNM060ENDDATE." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO BEGIN-DATE.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO ENDING-DATE.
           MOVE VDU-DATE-YYYYMMDD TO END-DATE.

           IF END-DATE < BEG-DATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO BEGIN-DATE.

           GO TO ENTER-BANKRUPT.

       ALL-DATE.
           MOVE "S  8080BEGDATE." TO VDU.
           MOVE EXT-JULIAN-BEG TO BEG-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080ENDDATE."   TO VDU.
           MOVE EXT-JULIAN-END TO END-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

       ENTER-BANKRUPT.
           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010BANKRUPT." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1>" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1U" ) GO TO ENDING-DATE.
           IF NOT ( VDUSTATUS = "00" OR "1D" )
              GO TO ENTER-BANKRUPT.

           IF NOT ( VDUBUF = "Y" OR "N")
              GO TO ENTER-BANKRUPT.

           MOVE VDUBUF TO INCLUDE-BANKRUPT.

       ENTER-LEGAL.
           MOVE SPACES TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010LEGAL." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1>" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1U" ) GO TO ENTER-BANKRUPT.
           IF NOT ( VDUSTATUS = "00" OR "1D" )
              GO TO ENTER-LEGAL.

           IF NOT ( VDUBUF = "Y" OR "N")
              GO TO ENTER-LEGAL.

           MOVE VDUBUF TO INCLUDE-LEGAL.
           IF INCLUDE-LEGAL = "N" AND INCLUDE-BANKRUPT = "N"
              MOVE "MUST INCLUDE SOMETHING" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-LEGAL.

       GTT-ISO-PARAGRAPH.

           IF ( GROUP-FLAG NOT = "Y" )
              MOVE SPACES TO TOTAL-LEV-TABLE
              MOVE "S  A010GTT-ISO." TO VDU
              MOVE "S" TO VDUBUF GTT-ISO
              PERFORM SCREENIO
              GO TO END-PARM.

           MOVE LEGEND-GTT-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A010GTT-ISO." TO VDU.
           PERFORM SCREENIO.

           IF ( VDUSTATUS = "10" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1>" ) GO TO EXIT-PARM.
           IF ( VDUSTATUS = "1U" ) GO TO ENTER-LEGAL.
           IF NOT ( VDUSTATUS = "00" OR "1D" )
              GO TO GTT-ISO-PARAGRAPH.

           IF NOT ( VDUBUF = "I" OR "S" OR "O" )
              GO TO GTT-ISO-PARAGRAPH.

           MOVE VDUBUF TO GTT-ISO.

           IF ( GTT-ISO = "I" OR "O" )
              PERFORM GRTOTL-WINDOW
           ELSE
              MOVE SPACES TO TOTAL-LEV-TABLE.

      *-----------------------------------------------------------------
       END-PARM.
           MOVE LEGEND-ACCEPT TO LEGEND.
           PERFORM SEND-LEGEND.
       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************
           IF GROUP-FLAG = "Y"
              MOVE "S  A000BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A000BR2." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BRANCH TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BRANCH TO VDUNUM0
              PERFORM SCREENIO.
      *    --------------------------------------
           MOVE "SNNN020CL1." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      *    --------------------------------------
           MOVE "SNNN020CL2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      *    --------------------------------------
           MOVE "S  A010BANKRUPT." TO VDU.
           MOVE INCLUDE-BANKRUPT TO VDUBUF.
           PERFORM SCREENIO.
      *    --------------------------------------
           MOVE "S  A010LEGAL." TO VDU.
           MOVE INCLUDE-LEGAL TO VDUBUF.
           PERFORM SCREENIO.
      *    --------------------------------------
           MOVE "S  A010GTT-ISO." TO VDU.
           MOVE GTT-ISO TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       LN-DETAIL1 SECTION.
      ******************************************************************
           IF FIRST-TIME = "Y"
              MOVE EXT-CONAME-RPT-LINES TO LN-COUNT
              MOVE 1 TO LN-FEED
              MOVE "N" TO FIRST-TIME
              MOVE "Y" TO RECORDS-FOUND.

           MOVE 1 TO SUB.
           PERFORM PROCESS-BORROWER.

           PERFORM LPSTAT-DISPLAY.
           MOVE LPS-DISPLAY-LNNO TO D-SLOAN-NUM.

           MOVE HOLD-TODAY-DATE TO AGEING-DATE.
           PERFORM COMPUTE-CONTRACTUAL.
           IF CPOT30 = "Y"
              MOVE "<P>" TO D-SCONX
           ELSE
           IF CPOT30 = "M"
              MOVE "<M>" TO D-SCONX
           ELSE
           IF CPOTCHGOFF = "Y"
              MOVE "<PC>" TO D-SCONX
           ELSE
           IF CPOTCHGOFF = "M"
              MOVE "<PL>" TO D-SCONX
           ELSE
              MOVE CONTRACTUAL TO D-SCON.

           MOVE LN-LOANDATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO D-SLNDATE.
           IF INCLUDE-LEGAL = "Y"
              MOVE LN-LEGALDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY  TO D-SDATE
              MOVE "L" TO D-STYPE.
           IF INCLUDE-BANKRUPT = "Y"
              MOVE LN-BNKRPTDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-SDATE
              MOVE "B" TO D-STYPE
              IF LN-BNKRPTDATE = 0
                 MOVE LN-LEGALDATE TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY  TO D-SDATE
                 MOVE "L" TO D-STYPE.

           IF GP-CPCORP-CURBAL = "Y"
              IF GP-INCLUDE-O2-IN-BAL = "Y"
                 MOVE LN-CURBAL TO D-SBALANCE
              ELSE
                 COMPUTE D-SBALANCE = LN-CURBAL - LN-OT2BAL
              END-IF
           ELSE
              IF GP-INCLUDE-O2-IN-BAL = "Y"
                 COMPUTE D-SBALANCE = LN-CURBAL + LN-OTHBAL
                                   + LN-INTBAL + LN-LCBAL
              ELSE
                 COMPUTE D-SBALANCE = LN-CURBAL + LN-OTHBAL
                                   + LN-INTBAL + LN-LCBAL
                                   - LN-OT2BAL.

           PERFORM WRITE-DETAIL-LINE.

           ADD 1 TO T-NOLOANS(1).
           PERFORM CREATE-WK.

       EXIT-LN-DETAIL.
           EXIT.

      **********************
       CREATE-WK SECTION.
      **********************

           MOVE BR-NO TO SAVE-WK-BR.
           MOVE LN-CLASS TO SAVE-WK-CL.

       SAVE-WORK-KEY.
           MOVE SAVE-WK-KEY TO WK-KEY.
           PERFORM READ-WK-FILE.
           IF IO-FG = 0
              GO TO CREATE-WK-UPDATE.

           MOVE " " TO WK-REC.
           MOVE SAVE-WK-KEY TO WK-KEY.

           MOVE 0 TO WK-NOLOANS.

           PERFORM WRITE-WK-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO SAVE-WORK-KEY.

       CREATE-WK-UPDATE.
           ADD 1 TO WK-NOLOANS.

           PERFORM REWRITE-WK-FILE.

       EXIT-CREATE-WK.
           EXIT.


      **********************
       CLASS-TOTALS SECTION.
      **********************

           IF ( GTT-ISO = "O" )
              GO TO END-CLASS-TOTALS.

           MOVE HOLD-BRNO TO WK-BR.
           MOVE HOLD-LN-CLASS TO WK-CLASS.
           PERFORM START-WK-FILE.

       PRINT-NEXT-CODE.
           PERFORM READ-WK-FILE-NEXT.
           IF IO-FG = 9
              GO TO END-CLASS-TOTALS.

           IF WK-CLASS NOT = HOLD-LN-CLASS
              GO TO END-CLASS-TOTALS.

           IF WK-BR NOT = HOLD-BRNO
              GO TO END-CLASS-TOTALS.

           MOVE "LOANS   :"     TO G-HEADING1.
           MOVE "CLASS TOTALS:" TO G-TOTNAME.
           MOVE WK-NOLOANS      TO G-LOANS.
           MOVE 2           TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           GO TO PRINT-NEXT-CODE.

       END-CLASS-TOTALS.
           ADD T-NOLOANS(1) TO T-NOLOANS(2).

           MOVE 0 TO T-NOLOANS(1).
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *****************************
       BRANCH-TOTALS SECTION.
      *****************************

           IF ( GTT-ISO = "O" )
              GO TO END-BRANCH-TOTALS.

           MOVE 0 TO HOLD-LN-CLASS.
           MOVE "BRANCH TOTALS:" TO G-TOTNAME.

           MOVE "LOANS   :"      TO G-HEADING1.
           MOVE T-NOLOANS(2)     TO G-LOANS.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       END-BRANCH-TOTALS.
           ADD T-NOLOANS(2) TO T-NOLOANS(3).
           MOVE 0 TO T-NOLOANS(2).
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      ***********************
       GRAND-TOTALS SECTION.
      ***********************

           IF ( GTT-ISO = "O" )
              GO TO END-GRAND-TOTALS.

           MOVE "GRAND  TOTALS:" TO G-TOTNAME.
           MOVE "LOANS   :"      TO G-HEADING1.
           MOVE T-NOLOANS(3)     TO G-LOANS.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

       END-GRAND-TOTALS.
           MOVE 3 TO SUB.
           MOVE 0 TO T-NOLOANS(3).

      ******************************************************************
       GTT-ADD-TOTALS SECTION.
      ******************************************************************
       GTT-ADD-TOTS.
           ADD WK-NOLOANS     TO  WS-GTT-NOLOANS   ( GTT-TOTAL-SUB ).

      ***********************************
       GTT-COMPILE-GROUP-TOTALS SECTION.
      ***********************************
       GTT-START-WK1.
           MOVE SPACES      TO WK-KEY.
           MOVE WGR-SRESULT TO WK-BR.
           MOVE ZEROES      TO WK-CLASS.
           PERFORM START-WK-FILE.

       GTT-NEXT-WK1.
           PERFORM READ-WK-FILE-NEXT.
           IF ( IO-FG   NOT = 0           ) OR
              ( WK-BR NOT = WGR-SRESULT )
                 GO TO EXIT-GTT-COMPILE-GROUP-TOTALS.

           MOVE "N" TO GTT-FIRST-TIME.

       GTT-OK-TO-PROCESS.

           PERFORM GTT-CHECK-ADD-TOTALS.
           GO TO GTT-NEXT-WK1.

       EXIT-GTT-COMPILE-GROUP-TOTALS.
           EXIT.

      ************************************
       GTT-PRINT-GROUP-BR-TOTALS SECTION.
      ************************************

           MOVE WGR-SID(GTT-PREV-SUB) TO WGR-WORD.
           MOVE WGR-NUMB              TO BR-NO.
           PERFORM READ-BR-FILE.
           IF ( IO-FG NOT = 0 )
              MOVE "BRANCH #&&&& NOT ON FILE" TO BR-NAME
              INSPECT BR-NAME REPLACING FIRST "&&&&" BY BR-NO.

           MOVE SPACES    TO DETAIL-LINE.
           MOVE 9         TO GTT-TOTAL-SUB.
           MOVE "BRANCH:" TO G-PROMPT1 WS-HOLD-PROMPT.
           MOVE BR-NO     TO G-BRNO.

           PERFORM GTT-PRINT-TOTALS.

       EXIT-GTT-PRINT-GROUP-BR-TOTALS.
           EXIT.

      *********************************
       GTT-PRINT-GROUP-TOTALS SECTION.
      *********************************

           MOVE SPACES    TO DETAIL-LINE.
           MOVE "GROUP :" TO G-PROMPT1 WS-HOLD-PROMPT.
           MOVE GR-ID     TO G-GRNO.
           MOVE GR-LEVEL  TO G-GR-LEVEL.

           PERFORM GTT-PRINT-TOTALS.

       EXIT-GTT-PRINT-GROUP-TOTALS.
           EXIT.

      *****************************
       GTT-PRINT-TOTALS SECTION.
      *****************************
       GTT-PRT-TOTALS-NEXT.
           MOVE "LOANS   :"                     TO G-HEADING1.
           MOVE WS-GTT-NOLOANS  (GTT-TOTAL-SUB) TO G-LOANS.
           MOVE 2                               TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           IF ( WS-HOLD-PROMPT = "GROUP :" )
              MOVE ALL "-" TO DETAIL-LINE
              PERFORM WRITE-DETAIL-LINE.


           MOVE LOW-VALUES TO WS-GTT-TABLE-GROUP-LEVEL(GTT-TOTAL-SUB).

       EXIT-GTT-PRINT-TOTALS.
           EXIT.

      ****************************
       PROCESS-BORROWER SECTION.
      ****************************
      *** BW DATA RETURNED IN SSP ***

           MOVE SPACES TO D-SNAME.
           MOVE LN-SSNO(SUB) TO BW-SSNO.

           IF LN-SSNO(SUB) = 0
              GO TO END-PROCESS-BORROWER.

           MOVE BW-SSNO TO NUM-BW.
           PERFORM NUMBW-TO-ALPBW.

      *     PERFORM READ-BW1-FILE.
      *     IF IO-BAD
      * BW-SSNO WILL = 0 IF BW REC NOT FOUND IN SSP *
           IF BW-SSNO = 0
               MOVE "**NOT ON FILE**" TO D-SNAME
               GO TO END-PROCESS-BORROWER.

           MOVE BW-LNAME TO NAME-WORKER.
           MOVE BW-FNAME TO FIRST-NAME.
           INSPECT NAME-WORKER REPLACING FIRST "  " BY ",?".

           INSPECT NAME-WORKER REPLACING FIRST "?               "
               BY FIRST-NAME.

           MOVE NAME-WORKER TO D-SNAME.

       END-PROCESS-BORROWER.
           EXIT.


      ******************************************************************
       WRITE-DETAIL-LINE SECTION.
      ******************************************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.
           IF LN-WK GREATER THAN EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.
           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      ******************************************************************
       SET-BRANCH-HEADINGS SECTION.
      ******************************************************************
           MOVE HOLD-BRNO TO H-BRNO.
           MOVE HOLD-BRNAME TO H-BRDESC.

      ******************************************************************
       SET-CLASS-HEADINGS SECTION.
      ******************************************************************
           MOVE HOLD-LN-CLASS TO H-LCNO LC-CODE.
           PERFORM READ-LC1-FILE.
           IF NOT IO-GOOD
              MOVE "INVALID" TO LC-DESC.
           MOVE LC-DESC TO H-LCDESC.

      ******************************************************************
       HEAD-PAGE SECTION.
      ******************************************************************
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
           PERFORM WRITE-PR-REC.
           MOVE HEAD4 TO PR-REC.
           PERFORM WRITE-PR-REC.
           MOVE HEAD5 TO PR-REC
           MOVE 2 TO LN-FEED
           PERFORM WRITE-PR-REC.
           MOVE HEAD6 TO PR-REC
           MOVE 1 TO LN-FEED
           PERFORM WRITE-PR-REC.
           MOVE 4 TO LN-COUNT
           MOVE 2 TO LN-FEED.
       END-HEAD-PAGE.
           EXIT.

      ******************************************************************
      *
      *       RANGE LINES
      *
      ******************************************************************
       PRINT-RANGE-LINES SECTION.
           MOVE 4 TO LN-FEED.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF GROUP-FLAG = "Y"
              MOVE "BRANCH GROUP" TO RANGE-LINE
              MOVE ENT-GROUP TO RANGE-SELECTION
              PERFORM WRITE-DETAIL-LINE
           ELSE
              MOVE "BEG BRANCH" TO RANGE-LINE
              MOVE BEG-BRANCH TO R-NUM
              PERFORM WRITE-DETAIL-LINE
              MOVE "END BRANCH" TO RANGE-LINE
              MOVE END-BRANCH TO R-NUM
              PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEG CLASS" TO RANGE-LINE.
           MOVE BEG-CLASS TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "END CLASS" TO RANGE-LINE.
           MOVE END-CLASS TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "BEG DATE" TO RANGE-LINE.
           MOVE BEG-DATE TO R-DATE.
           PERFORM WRITE-DETAIL-LINE.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "END DATE" TO RANGE-LINE.
           MOVE END-DATE TO R-DATE.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "INCLUDE BANKRUPTS" TO RANGE-LINE.
           MOVE INCLUDE-BANKRUPT TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "INCLUDE LEGALS" TO RANGE-LINE.
           MOVE INCLUDE-LEGAL TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "GROUP TOTALS" TO RANGE-LINE.
           MOVE GTT-ISO TO RANGE-SELECTION.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.


       COPY "LIBGB/GPENV.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBLP/LPSTAT.CPY".
       COPY "LIBLP/LPMDTE.CPY".
       COPY "LIBLP/LPCEPP.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBLP/LPPDUE.CPY".
       COPY "LIBGB/AGEING.CPY".
       COPY "LIBGB/SPPDTH.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRTOTALS.CPY".
       COPY "LIBWI/GRTOTL.CPY".

       COPY "LIBLP/ARRAYSP.CPY".

      ************************************
       FORM-RESET SECTION.
      ************************************
           MOVE EXT-CONAME-CONAME TO BNKLGL-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BNKLGL-SYSDATE.
           DISPLAY BNKLGL-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE BNKLGL-FORM-FIELDS TO WR-BUF.
           MOVE BNKLGL-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.


      ************************************
       SETUP-LINK-INFO SECTION.
      ************************************
           MOVE BNKLGL-HELP--FILE TO HELP-LINK-FILE.
           MOVE BNKLGL-HELP--DIR TO HELP-LINK-DIR.

      ************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/BNKLGL_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
      * M I S C   P A R A G R A P H S
      ******************************************************************
       MISC-PARA SECTION.
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       NUMBW-TO-ALPBW.
           MOVE NUM-BW1 TO ALP-BW1.
           MOVE NUM-BW2 TO ALP-BW2.
           MOVE NUM-BW3 TO ALP-BW3.
           MOVE "-" TO ALP-HYPHEN1 ALP-HYPHEN2.

       SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE MSEL-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       CAT-CSZ.
           INSPECT C-S-Z REPLACING FIRST "      " BY ", ++ +".
           INSPECT C-S-Z REPLACING FIRST "++" BY STATE.
           INSPECT C-S-Z REPLACING FIRST "+         " BY ZIP.

      ******************************************************
       IO-ROUTINES SECTION.
      ******************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPSPGS_SQL.CPY".
       COPY "LIBLP/LPLTPGS_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPLCGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
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
       START-LN3-FILE.   *> SSP - LN BW
           PERFORM START-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS 
              DELIMITED BY " " INTO E-KEYX.

           IF ( LN3-CURSOR-STAT-GOOD )
              PERFORM CLOSE-LN3-FILE.

           EXEC SQL SET CONNECTION LN3_CONNECT END-EXEC.

      *      SELECT LNFILE.LN_OWNBR,
      *           LNFILE.LN_ACCTNO,
      *           LNFILE.LN_CLASS,
      *           LNFILE.LN_SUBCLASS,
      *           LNFILE.LN_LAWCODE,
      *           LNFILE.LN_SSNO1,
      *           LNFILE.LN_MAKERCD1,
      *           LNFILE.LN_SSNO2,
      *           LNFILE.LN_MAKERCD2,
      *           LNFILE.LN_SSNO3,
      *           LNFILE.LN_MAKERCD3,
      *           LNFILE.LN_SSNO4,
      *           LNFILE.LN_MAKERCD4,
      *           LNFILE.LN_ORGST,
      *           LNFILE.LN_SPRCLASS,
      *           LNFILE.LN_POCD,
      *           LNFILE.LN_PLCD,
      *           LNFILE.LN_PAY_SCHLD_FG,
      *           LNFILE.LN_STATFLAGS,
      *           CAST(LNFILE.LN_LOANDATE       AS VARCHAR(10)),
      *           CAST(LNFILE.LN_ORIG_1STPYDATE AS VARCHAR(10)),
      *           CAST(LNFILE.LN_1STPYDATE     AS VARCHAR(10)),
      *           CAST(LNFILE.LN_LEGALDATE     AS VARCHAR(10)),
      *           CAST(LNFILE.LN_BNKRPTDATE    AS VARCHAR(10)),
      *           LNFILE.LN_ORGTERM,
      *           LNFILE.LN_1STPYAMT,
      *           LNFILE.LN_NOREGPY,
      *           LNFILE.LN_REGPYAMT,
      *           LNFILE.LN_LASTPYAMT,
      *           LNFILE.LN_PY_YYYYMM_RECDEL,
      *           LNFILE.LN_MTD_PAYS_RECDEL,
      *           LNFILE.LN_CURBAL,
      *           LNFILE.LN_INTBAL,
      *           LNFILE.LN_LCBAL,
      *           LNFILE.LN_OTHBAL,
      *           LNFILE.LN_OT2BAL,
      *           LNFILE.LN_TOTPAYMNTD,
      *           LNFILE.LN_TOTNODEF,
      *           LNFILE.LN_TOTNOALDEL,
      *           CAST(LNFILE.LN_DATE_PAID_LAST AS VARCHAR(10)),
      *           LNFILE.LN_SUM_ORGTERM,
      *           LNFILE.LN_UNITPER_CD,
      *           LNFILE.LN_UNITPER_FREQ,
      *           ISNULL(BWFILE.BW_OWNBR,0),
      *           ISNULL(BWFILE.BW_SSNO,0),
      *           ISNULL(BWFILE.BW_LNAME,''''),
      *           ISNULL(BWFILE.BW_FNAME,''''),
      *           ISNULL(BWFILE.BW_SOLICIT,'''')
      *   FROM DBO.LNFILE LNFILE
      *   LEFT OUTER JOIN DBO.BWFILE BWFILE 
      *    ON BWFILE.BW_OWNBR = LNFILE.LN_OWNBR  
      *   AND BWFILE.BW_SSNO  = LNFILE.LN_SSNO1   
      *   WHERE LNFILE.LN_OWNBR = @QLN3_WBEG_OWNBR
      *    AND LNFILE.LN_CLASS BETWEEN @QLN3_WBEG_CLASS AND @QLN3_WEND_CLASS
      *    AND (LNFILE.LN_POCD NOT IN (''**'', ''*I'', ''*R'', ''*U''))
      *    AND (LNFILE.LN_BNKRPTDATE != ''1900-01-01'' OR LNFILE.LN_LEGALDATE != ''1900-01-01'')'
      *    ORDER BY LNFILE.LN_OWNBR, LNFILE.LN_CLASS, LNFILE.LN_ACCTNO

           EXEC SQL
            DECLARE LN3_CURSOR CURSOR FOR
                :LN3-RETURN-CODE = 
                 EXEC SSP_BNKLGL_LNFILE_SELECT
                  ( :QLN3-WBEG-OWNBR, :QLN3-WBEG-CLASS, 
                    :QLN3-WEND-CLASS)
                END-EXEC.

           EXEC SQL
               OPEN LN3_CURSOR
           END-EXEC.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           MOVE ZEROES TO QLN3-WBEG-OWNBR
                          QLN3-WBEG-CLASS
                          QLN3-WEND-CLASS.
 
           MOVE STAT---GOOD TO LN3-CURSOR-STAT.

      *-----------------------------------------------------------------
       READ-LN3-FILE-NEXT.   *> SSP - LN BW
           PERFORM READ-IT.
           MOVE LN-PATH-OWNBR  TO LN-OWNBR.
           MOVE LN-PATH-SQL    TO E-FILE.
           MOVE SPACES         TO E-KEYX.
           STRING LN-OWNBR, LN-CLASS
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
                  :QLN-POCD,
                  :QLN-PLCD,
                  :QLN-PAY-SCHLD-FG,
                  :QLN-STATFLAGS,
                  :QLN-LOANDATE,
                  :QLN-ORIG-1STPYDATE, 
                  :QLN-1STPYDATE,
                  :QLN-LEGALDATE,
                  :QLN-BNKRPTDATE,
                  :QLN-ORGTERM,
                  :QLN-1STPYAMT,
                  :QLN-NOREGPY,
                  :QLN-REGPYAMT,
                  :QLN-LASTPYAMT,
                  :QLN-PY-YYYYMM-RECDEL,
                  :QLN-MTD-PAYS-RECDEL,
                  :QLN-CURBAL,
                  :QLN-INTBAL,
                  :QLN-LCBAL,
                  :QLN-OTHBAL,
                  :QLN-OT2BAL,
                  :QLN-TOTPAYMNTD,
                  :QLN-TOTNODEF,
                  :QLN-TOTNOALDEL,
                  :QLN-DATE-PAID-LAST, 
                  :QLN-SUM-ORGTERM,
                  :QLN-UNITPER-CD,
                  :QLN-UNITPER-FREQ,
                  :QBW-OWNBR, 
                  :QBW-SSNO, 
                  :QBW-LNAME, 
                  :QBW-FNAME, 
                  :QBW-SOLICIT
              END-EXEC
           ELSE
              MOVE 9        TO SQLCODE
              MOVE "RDNXCR" TO SQLSTATE.

           PERFORM SQL-IO-VALIDATION.

           EXEC SQL SET CONNECTION C1 END-EXEC.

           IF ( IO-FG = 8 )     *>LOCKED
              GO TO READ-LN3-FILE-NEXT.

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



       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBLP/LPLC1RN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
      * END OF PROGRAM PROCEDURES SECTION
      ******************************************************************
       END-PROGRAM SECTION.
       END-RUN.
           PERFORM CLOSE-FILES.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

           IF ( SKIP-RECORDS-FOUND )
              GO TO EXIT-PROG.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO RECORDS FOUND FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

       EXIT-PROG.

           IF SV-BEG-BR NOT = 9999
              MOVE SV-BEG-BR TO BEG-BRANCH
              MOVE SV-END-BR TO END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE "LP/LPR2MU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY BNKLGL-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
