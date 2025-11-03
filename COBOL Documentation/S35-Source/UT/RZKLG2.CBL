       IDENTIFICATION DIVISION.
       PROGRAM-ID.   BNKLG2.
       DATE-WRITTEN. 070409.
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
      *   SAME AS BNKLGL EXCEPT SORTED BY BK/LEGAL DATE, SO NO CLASS TOTALS
      *
      *
      *  REV:
      *  CS  070409 NEW FOR WORLD, PR# 537
      *  BLF 070827 FIX CLASS RANGES; REPORT TOOK LOGIC FROM BNKLGL BUT
      *             CHANGED TO READ LN1 INSTEAD OF LN3, SO NEEDED TO
      *             CHECK BEG & ENDING CLASS DIFFERENTLY, WORLD PR# 553
      *             ALSO FIXED SORT TO BE BY YYYYMMDD, NOT MMDDYYYY
      *  BLF 070830 FIX ANOTHER RANGE PROBLEM, CHANGE TITLE TO BE
      *             BANKRUPT/LEGAL BY STATUS DATE
      *  BAH 160125 SPLIT OUT LTFILE, PD0003
      *   CS 160825 CHANGED SETUP OF LN-OWNBR TO USE BR-NO AND NOT EXT-
      *             FILPATH-BRNO
      *  BAH 170508 ACTIVATE 8 DIGIT DATES, PD0006
      *
      * KEC 2018.0423 PR#00000 PARADATA A30 JAY & BETH
      *         REMOVED USE OF LOCAL-PRINT-FG SINCE NOT USED;
      *         LOCAL-PRINT-FG NOT BEING SET; IN EOBUF WHICH IS EXTERNAL
      *         WHERE IF NOT BEING SET WILL CAUSE ISSUES FROM THAT
      *         LOCAL-PRINT-FG BYTE POSITION COULD BE SET FROM PREVIOUS
      *         PROGRAM WITH EOBUF THAT SET SOMETHING IN THAT BYTE.
      *
      * BAH 181203 GLOBAL LNFILE, L2G
      * BAH 20190922 REMOVED ACCESS-CALL LNFILE
      * BAH 20200827 REMOVED RESET ON FIRST FIELD
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. 3B.
       OBJECT-COMPUTER. 3B.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       COPY "LIBGB/GBFSPR.CPY".
       COPY "LIBGB/GBFSWKI.CPY".
           SELECT WK2-FILE ASSIGN WK2-PATH
           ORGANIZATION INDEXED
           ACCESS DYNAMIC
           LOCK MODE AUTOMATIC WITH LOCK ON RECORD
           RECORD KEY WK2-KEY
           FILE STATUS FILE-STAT.

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
               05  WK-BR      PIC 9(4).

           03  WK-NOLOANS         PIC S9(7)     COMP-3.
           03  WK-BALANCE         PIC S9(9)V99  COMP-3.
           03  WK-7-NOLOANS       PIC S9(7)     COMP-3.
           03  WK-7-BALANCE       PIC S9(9)V99  COMP-3.
           03  WK-11-NOLOANS      PIC S9(7)     COMP-3.
           03  WK-11-BALANCE      PIC S9(9)V99  COMP-3.
           03  WK-12-NOLOANS      PIC S9(7)     COMP-3.
           03  WK-12-BALANCE      PIC S9(9)V99  COMP-3.
           03  WK-13-NOLOANS      PIC S9(7)     COMP-3.
           03  WK-13-BALANCE      PIC S9(9)V99  COMP-3.

       FD  WK2-FILE
           LABEL RECORDS ARE STANDARD.
       01  WK2-REC.
           03  WK2-KEY.
               05  WK2-BRNO          PIC 9(4).
               05  WK2-BK-LGL-DATE   PIC 9(8).
               05  WK2-ACCTNO        PIC 9(8).

       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(37) VALUE
           "@(#)UT/RZKLG2 S35 08/18/23-09:18:33 >".
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
       COPY "LIBLP/LP01LTP.CPY".
       COPY "LIBLP/LP01LTP_SQL.CPY".
       COPY "LIBLP/LPWSLTP.CPY".

       01  WK-IFN               PIC X(4)  VALUE "WK".
       01  WK2-IFN              PIC X(3)  VALUE "WK2".

       01  WK2-PATH             PIC X(35) VALUE SPACES.

      *-----------------------------------------------------------------
      * LINE WORKERS
      *-----------------------------------------------------------------
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

       01  ER-FEED              PIC 9        VALUE 1.

       01  HOLD-BALANCE         PIC S9(9)V99 COMP-3 VALUE 0.
      * (1) ALL LOANS
      * (2) CHAPTER 7
      * (3) CHAPTER 11
      * (4) CHAPTER 12
      * (5) CHAPTER 13
       01  WS-GTT-TOTAL-TABLE.
           03  WS-GTT-TABLE-GROUP-LEVEL                 OCCURS 10 TIMES.
               05  WS-GTT-TABLE                         OCCURS 5 TIMES.
                   07  WS-GTT-NOLOANS         PIC S9(7)     COMP.
                   07  WS-GTT-BALANCE         PIC S9(9)V99  COMP.

       01  WS-HOLD-PROMPT       PIC X(7)  VALUE SPACES.
      ******************************************************************
      *  TOTALS LEV1 = CLASS TOTALS
      *            2 = BRANCH TOTALS
      *            3 = GRAND TOTALS
      ******************************************************************
       01  TOTALS-TAB.
           03  TOTAL-TBL       OCCURS 3.
               05  TOTAL-TABLE OCCURS 5 TIMES.
                   07  T-NOLOANS         PIC S9(7)     COMP-3.
                   07  T-BALANCE         PIC S9(9)V99  COMP-3.

      *-----------------------------------------------------------------
      * MISC WORKERS
      *-----------------------------------------------------------------
       01  MSEL-LIST            PIC X(5)      VALUE "MSEL.".
       01  SUB                  PIC 9(2)  COMP.
       01  SUB1               PIC 9(6)  COMP.
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

       01  BEG-COUNTER          PIC 9(9)  VALUE 0.
       01  MID-COUNTER          PIC 9(9)  VALUE 0.
       01  END-COUNTER          PIC 9(9)  VALUE 0.

       01  SAVE-WK-KEY.
           03  SAVE-WK-BR       PIC 9(4).
           03  SAVE-WK-CL       PIC 9(2).

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
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.

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
           03  FILLER           PIC X(6)    VALUE " ".
           03  FILLER           PIC X(29) VALUE
               "BANKRUPT/LEGAL BY STATUS DATE".

       01  HEAD3.
           03  FILLER           PIC X(10) VALUE "BRANCH :".
           03  H-BRNO           PIC ZZZ9.
           03  FILLER           PIC X VALUE " ".
           03  H-BRDESC         PIC X(20).

       01  HEAD5.
           03  FILLER             PIC X(65) VALUE  SPACES.
           03  FILLER             PIC X(12) VALUE  "LEGAL".

       01  HEAD6.
           03  FILLER             PIC X(17) VALUE  "ACCOUNT #".
           03  FILLER             PIC X(22) VALUE  "NAME".
           03  FILLER             PIC X(12) VALUE  "LOANDATE".
           03  FILLER             PIC X(11) VALUE  "BALANCE".
           03  FILLER             PIC X(10) VALUE  "BANKRUPT".
           03  FILLER             PIC X(4)  VALUE  "CH".
           03  FILLER             PIC X(4)  VALUE  "CON".


       01  DETAIL-LINE        PIC X(80)  VALUE SPACES.
       01  DETAIL1 REDEFINES DETAIL-LINE.
           03  D-SLOAN-NUM    PIC X(16).
           03  FILLER         PIC X.
           03  D-SNAME        PIC X(20).
           03  FILLER         PIC X(2).
           03  D-SLNDATE      PIC 99/99/99     BLANK ZERO.
           03  FILLER         PIC X(2).
           03  D-SBALANCE     PIC Z(6).99-    BLANK ZERO.
           03  FILLER         PIC XX.
           03  D-SDATE        PIC 99/99/99     BLANK ZERO.
           03  D-STYPE        PIC X.
           03  FILLER         PIC X(2).
           03  D-SCHAPTER     PIC 9(2).
           03  FILLER         PIC X.
           03  D-SCONX.
               05  D-SCON     PIC ZZZ9         BLANK ZERO.

       01  D-LINE2 REDEFINES DETAIL-LINE.
           03  G-GROUP              PIC X(14).
           03  G-GROUP-1 REDEFINES G-GROUP.
               05  G-TOTNAME        PIC X(14).
               05  G-CODENAME REDEFINES G-TOTNAME.
                   07  FILLER       PIC X(13).
                   07  G-COLON      PIC X.
           03  G-GROUP-2 REDEFINES G-GROUP.
               05  G-PROMPT1        PIC X(7).
               05  FILLER           PIC X.
               05  G-GRNO           PIC X(4).
               05  G-BRNO           REDEFINES G-GRNO
                                    PIC 9(4).
               05  FILLER           PIC X.
               05  G-GR-LEVEL       PIC 9(1).
           03  FILLER           PIC X(01).
           03  G-HEADING1       PIC X(12).
           03  G-AMOUNT-1       PIC ZZZZZZZZ9.99-  BLANK WHEN ZERO.
           03  FILLER REDEFINES G-AMOUNT-1.
               05  FILLER       PIC X(03).
               05  G-LOANS      PIC ZZZZZ9-    BLANK WHEN ZERO.
               05  FILLER       PIC X(03).
           03  FILLER           PIC X(9).
           03  G-TOTBAL         PIC Z,ZZZ,ZZ9.99-  BLANK WHEN ZERO.

      *-----------------------------------------------------------------
       01  RANGE-LINE REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(35).
           03  RANGE-SELECTION.
               04  R-NUM        PIC Z(9)9.
               04  R-NUM-REDEF2 REDEFINES R-NUM.
                   05  R-DATE   PIC 9999/99/99.

       COPY "LIBGB/PASSWDW.CPY".

       COPY "LIBWI/PFSWINDOWSW.CPY".
       COPY "LIBWI/TSTINTW.CPY".
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBWI/GRTOTLW.CPY".

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
       COPY "LIBLP/BNKLG2_DEF.CPY".
       COPY "LIBLP/BNKLG2_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

       COPY "LIBGB/GETFMW.CPY".

       SCREEN SECTION.
       COPY "LIBLP/BNKLG2_SCN.CPY".

      ******************************************************************
      * P R O G R A M   C O N T R O L
      ******************************************************************
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE WK-FILE WK2-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-SP1-FILE.
           PERFORM CLOSE-LTP1-FILE.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
           CLOSE WK-FILE WK2-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      *-----------------------------------------------------------------
      * MAIN PROGRAM SECTION
      *-----------------------------------------------------------------
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "BNKLG2" TO FORM-PROG VDU-FORM-NAME.

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

           PERFORM OPEN-WK2-FILE-OUTPUT.
           PERFORM CLOSE-WK2-FILE.
           PERFORM OPEN-WK2-FILE.

           PERFORM OPEN-BW1-FILE.
           PERFORM OPEN-LN1-FILE.

           MOVE LN-PATH-OWNBR TO LN-OWNBR.
           MOVE 0             TO LN-ACCTNO.

           PERFORM START-LN1-FILE.

       NEXT-LN.
           PERFORM READ-LN1-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              GO TO CONTINUE-WK2-PROCESSING.

           ADD 1 TO BEG-COUNTER.               

           PERFORM TRAP-INTERRUPT.

           IF LN-POCD-NONLOAN
              GO TO NEXT-LN.

           IF LN-OWNBR > END-BRANCH
              GO TO END-OF-GROUP.

           IF LN-CLASS < BEG-CLASS OR LN-CLASS > END-CLASS
              GO TO NEXT-LN.

           IF LN-BNKRPTDATE = 0 AND LN-LEGALDATE = 0
              GO TO NEXT-LN.

           ADD 1 TO MID-COUNTER.

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

           PERFORM CREATE-SORTED-FILE.

           ADD 1 TO END-COUNTER.

           GO TO NEXT-LN.

       CONTINUE-WK2-PROCESSING.
      D    STOP MESS. 
           PERFORM CLOSE-WK2-FILE.
           PERFORM OPEN-WK2-FILE.

           MOVE ZEROS TO WK2-BRNO.
           MOVE ZEROS TO WK2-BK-LGL-DATE.
           MOVE ZEROS TO WK2-ACCTNO.
           PERFORM START-WK2-FILE.

       NEXT-WK2-REC.
           PERFORM READ-WK2-FILE-NEXT.
           IF IO-BAD
              GO TO END-OF-GROUP.

           IF FIRST-TIME = "Y"
              MOVE BR-NO TO HOLD-BRNO
              MOVE BR-NAME TO HOLD-BRNAME
              PERFORM SET-BRANCH-HEADINGS.

      * TEST BREAK IN BRANCH
           IF BR-NO NOT = HOLD-BRNO
              PERFORM BRANCH-TOTALS
              MOVE BR-NO TO HOLD-BRNO
              MOVE BR-NAME TO HOLD-BRNAME
              PERFORM SET-BRANCH-HEADINGS
              PERFORM HEAD-PAGE.

           MOVE WK2-ACCTNO TO LN-ACCTNO.
           PERFORM READ-LN1-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           MOVE LN-ORGST     TO SP-ORGST.
           MOVE LN-SPRCLASS  TO SP-SPRCLASS.
           MOVE LN-SUBCLASS  TO SP-SUBCLASS.
           MOVE LN-LAWCODE   TO SP-LAWCODE.

           IF LPWSSP-SP1-KEY NOT = SP1-KEY
              MOVE SP1-KEY TO LPWSSP-SP1-KEY
              PERFORM READ-SP1-FILE
              IF IO-FG NOT = 0
                 GO TO IO-ERROR.

           PERFORM LN-DETAIL1.

           GO TO  NEXT-WK2-REC.

       END-OF-GROUP.
           PERFORM CLOSE-LN1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-WK2-FILE.

           GO TO NEXT-GROUP-LOC.

       END-ROUTINE.
           IF FIRST-TIME = "Y"
              GO TO END-PROGRAM.

           PERFORM BRANCH-TOTALS.
           PERFORM GRAND-TOTALS.
           IF ( GTT-ISO NOT = "S" )
              MOVE 99 TO LN-COUNT
              PERFORM GROUP-TOTAL-TABLE
              PERFORM PRINT-RANGE-LINES
           ELSE
              PERFORM PRINT-RANGE-LINES.

           GO TO END-PROGRAM.

      ********************************************
       CREATE-SORTED-FILE SECTION.
      ********************************************

      * CREATE SORTED FILE BY BANKRUPT/LEGAL DATE

           MOVE SPACES      TO WK2-REC.
           MOVE BR-NO       TO WK2-BRNO.
           MOVE LN-ACCTNO   TO WK2-ACCTNO.

           IF INCLUDE-LEGAL = "Y"
              MOVE LN-LEGALDATE TO WK2-BK-LGL-DATE.

           IF INCLUDE-BANKRUPT = "Y"
              MOVE LN-BNKRPTDATE TO WK2-BK-LGL-DATE.

           PERFORM WRITE-WK2-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

        CREATE-SORTED-FILE-EXIT.
           EXIT.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE BNKLG2-QUEUE-POS TO Q-POS.
           MOVE BNKLG2-COPIES-POS TO C-POS.
           MOVE BNKLG2-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.

       COPY "LIBGB/EOINIT.CPY".

           MOVE 0 TO PAGE-NUMBER.

           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD TO HOLD-TODAY-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS............" TO WR-BUF
              MOVE MESS-LIST TO WR-LIST
              PERFORM CALL-WRFORM.

           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BRANCH TO SV-BEG-BR
              MOVE END-BRANCH TO SV-END-BR
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP  TO SV-ENT-GROUP
              MOVE EXT-EOBUF-BRNO TO BEG-BRANCH END-BRANCH
              MOVE "N" TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE
           ELSE
              IF (BEG-BRANCH = 0 AND END-BRANCH = 0 )
                 MOVE BEG-BRANCH TO SV-BEG-BR
                 MOVE END-BRANCH TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BRANCH
                                             END-BRANCH.

           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 5
              MOVE 0 TO T-NOLOANS(1,SUB1) T-NOLOANS(2,SUB1)
                                         T-NOLOANS(3,SUB1)
              MOVE 0 TO T-BALANCE(1,SUB1) T-BALANCE(2,SUB1)
                                         T-BALANCE(3,SUB1)
           END-PERFORM.

           MOVE MKTEMP-DEFAULT TO MKTEMP-BUF.
           PERFORM MKTEMP-CALL.
           MOVE MKTEMP-BUF TO WK2-PATH.
           PERFORM OPEN-WK2-FILE-OUTPUT.
           PERFORM CLOSE-WK2-FILE.

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

           PERFORM ENABLE-INTERRUPT.
           MOVE "N" TO RECORDS-FOUND.

           IF ( GROUP-FLAG = "Y" )
              MOVE LOW-VALUES TO WS-GTT-TOTAL-TABLE
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

       BR-01.
           MOVE LEGEND-BR-01 TO LEGEND.
           PERFORM SEND-LEGEND.
           MOVE "E  A000BR1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-BR.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "1<"
              MOVE GRSCAN-BUF TO SCAN-BUF
              PERFORM CALL-WINDOW-PROGRAM
              MOVE SCAN-BUF TO GRSCAN-BUF
              IF SCAN-STATUS NOT = "00"
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

       CHECK-BRANCH-SECURITY.
           PERFORM BR-RANGE-SECURITY.

           IF BR-SECURITY-OKAY = "N"
              GO TO BR-01.

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
           MOVE JULIAN-BEG TO BEG-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080ENDDATE."   TO VDU.
           MOVE JULIAN-END TO END-DATE VDU-DATE-YYYYMMDD.
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
              MOVE ENT-GROUP TO VDUBUF GTT-SV-ENT-GROUP
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
      
           MOVE "SNNN020CL1." TO VDU.
           MOVE BEG-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      
           MOVE "SNNN020CL2." TO VDU.
           MOVE END-CLASS TO VDUNUM0.
           PERFORM SCREENIO.
      
           MOVE "S  A010BANKRUPT." TO VDU.
           MOVE INCLUDE-BANKRUPT TO VDUBUF.
           PERFORM SCREENIO.
      
           MOVE "S  A010LEGAL." TO VDU.
           MOVE INCLUDE-LEGAL TO VDUBUF.
           PERFORM SCREENIO.
      
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
              MOVE DATE-MMDDYY TO D-SDATE
              MOVE "L" TO D-STYPE.
           IF INCLUDE-BANKRUPT = "Y"
              MOVE LN-BNKRPTDATE TO DATE-YYYYMMDD
              PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
              MOVE DATE-MMDDYY TO D-SDATE
              MOVE "B" TO D-STYPE
              IF LN-BNKRPTDATE = 0
                 MOVE LN-LEGALDATE TO DATE-YYYYMMDD
                 PERFORM CONVERT-YYYYMMDD-TO-MMDDYY
                 MOVE DATE-MMDDYY TO D-SDATE
                 MOVE "L" TO D-STYPE.

           IF GP-CPCORP-CURBAL = "Y"
              IF GP-INCLUDE-O2-IN-BAL = "Y"
                 MOVE LN-CURBAL TO HOLD-BALANCE
              ELSE
                 COMPUTE HOLD-BALANCE = LN-CURBAL - LN-OT2BAL
              END-IF
           ELSE
              IF GP-INCLUDE-O2-IN-BAL = "Y"
                 COMPUTE HOLD-BALANCE = LN-CURBAL + LN-OTHBAL
                                   + LN-INTBAL + LN-LCBAL
              ELSE
                 COMPUTE HOLD-BALANCE = LN-CURBAL + LN-OTHBAL
                                   + LN-INTBAL + LN-LCBAL
                                   - LN-OT2BAL.

           MOVE HOLD-BALANCE TO D-SBALANCE.
           MOVE LN-BNKRPT-CHAPTER TO  D-SCHAPTER.

           PERFORM WRITE-DETAIL-LINE.

           ADD 1 TO T-NOLOANS(2,1).
           ADD HOLD-BALANCE TO T-BALANCE(2,1).
           IF LN-BNKRPT-CHAPTER = 7
              ADD 1 TO T-NOLOANS(2,2)
              ADD HOLD-BALANCE TO T-BALANCE(2,2)
           ELSE
           IF LN-BNKRPT-CHAPTER = 11
              ADD 1 TO T-NOLOANS(2,3)
              ADD HOLD-BALANCE TO T-BALANCE(2,3)
           ELSE
           IF LN-BNKRPT-CHAPTER = 12
              ADD 1 TO T-NOLOANS(2,4)
              ADD HOLD-BALANCE TO T-BALANCE(2,4)
           ELSE
           IF LN-BNKRPT-CHAPTER = 13
              ADD 1 TO T-NOLOANS(2,5)
              ADD HOLD-BALANCE TO T-BALANCE(2,5).

           PERFORM CREATE-WK.

       EXIT-LN-DETAIL.
           EXIT.

      *****************************************
       CREATE-WK SECTION.
      *****************************************

           MOVE BR-NO TO SAVE-WK-BR.
           MOVE LN-CLASS TO SAVE-WK-CL.

       SAVE-WORK-KEY.
           MOVE SAVE-WK-KEY TO WK-KEY.
           PERFORM READ-WK-FILE.
           IF IO-FG = 0
              GO TO CREATE-WK-UPDATE.

           MOVE " " TO WK-REC.
           MOVE SAVE-WK-KEY TO WK-KEY.

           MOVE 0 TO WK-NOLOANS WK-7-NOLOANS WK-11-NOLOANS
                                WK-12-NOLOANS WK-13-NOLOANS.
           MOVE 0 TO WK-BALANCE WK-7-BALANCE WK-11-BALANCE
                                WK-12-BALANCE WK-13-BALANCE.

           PERFORM WRITE-WK-FILE.
           IF IO-FG NOT = 0
              GO TO IO-ERROR.

           GO TO SAVE-WORK-KEY.

       CREATE-WK-UPDATE.
           ADD 1 TO WK-NOLOANS.
           ADD HOLD-BALANCE TO WK-BALANCE.
           IF LN-BNKRPT-CHAPTER = 7
              ADD 1 TO WK-7-NOLOANS
              ADD HOLD-BALANCE TO WK-7-BALANCE
           ELSE
           IF LN-BNKRPT-CHAPTER = 11
              ADD 1 TO WK-11-NOLOANS
              ADD HOLD-BALANCE TO WK-11-BALANCE
           ELSE
           IF LN-BNKRPT-CHAPTER = 12
              ADD 1 TO WK-12-NOLOANS
              ADD HOLD-BALANCE TO WK-12-BALANCE
           ELSE
           IF LN-BNKRPT-CHAPTER = 13
              ADD 1 TO WK-13-NOLOANS
              ADD HOLD-BALANCE TO WK-13-BALANCE.

           PERFORM REWRITE-WK-FILE.

       EXIT-CREATE-WK.
           EXIT.

      ****************************************************
       BRANCH-TOTALS SECTION.
      ****************************************************

           IF ( GTT-ISO = "O" )
              GO TO END-BRANCH-TOTALS.

           MOVE "BRANCH TOTALS:" TO G-TOTNAME.

           MOVE "CHAPTER  7 :" TO G-HEADING1.
           MOVE T-NOLOANS(2,2) TO G-LOANS.
           MOVE T-BALANCE(2,2) TO G-TOTBAL.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CHAPTER 11 :" TO G-HEADING1.
           MOVE T-NOLOANS(2,3) TO G-LOANS.
           MOVE T-BALANCE(2,3) TO G-TOTBAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CHAPTER 12 :" TO G-HEADING1.
           MOVE T-NOLOANS(2,4) TO G-LOANS.
           MOVE T-BALANCE(2,4) TO G-TOTBAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CHAPTER 13 :" TO G-HEADING1.
           MOVE T-NOLOANS(2,5) TO G-LOANS.
           MOVE T-BALANCE(2,5) TO G-TOTBAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ALL LOANS  :" TO G-HEADING1.
           MOVE T-NOLOANS(2,1) TO G-LOANS.
           MOVE T-BALANCE(2,1) TO G-TOTBAL.
           PERFORM WRITE-DETAIL-LINE.

       END-BRANCH-TOTALS.
           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 5
              ADD T-NOLOANS(2,SUB1) TO T-NOLOANS(3,SUB1)
              ADD T-BALANCE(2,SUB1) TO T-BALANCE(3,SUB1)
           END-PERFORM.

           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 5
              MOVE 0 TO T-NOLOANS(2,SUB1) T-BALANCE(2,SUB1)
           END-PERFORM.

           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

      *****************************
       GRAND-TOTALS SECTION.
      *****************************

           IF ( GTT-ISO = "O" )
              GO TO END-GRAND-TOTALS.

           MOVE "GRAND  TOTALS:" TO G-TOTNAME.

           MOVE "CHAPTER  7 :" TO G-HEADING1.
           MOVE T-NOLOANS(3,2) TO G-LOANS.
           MOVE T-BALANCE(3,2) TO G-TOTBAL.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CHAPTER 11 :" TO G-HEADING1.
           MOVE T-NOLOANS(3,3) TO G-LOANS.
           MOVE T-BALANCE(3,3) TO G-TOTBAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CHAPTER 12 :" TO G-HEADING1.
           MOVE T-NOLOANS(3,4) TO G-LOANS.
           MOVE T-BALANCE(3,4) TO G-TOTBAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CHAPTER 13 :" TO G-HEADING1.
           MOVE T-NOLOANS(3,5) TO G-LOANS.
           MOVE T-BALANCE(3,5) TO G-TOTBAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ALL LOANS  :" TO G-HEADING1.
           MOVE T-NOLOANS(3,1) TO G-LOANS.
           MOVE T-BALANCE(3,1) TO G-TOTBAL.
           PERFORM WRITE-DETAIL-LINE.

       END-GRAND-TOTALS.
           PERFORM VARYING SUB1 FROM 1 BY 1 UNTIL SUB1 > 5
              MOVE 0 TO T-NOLOANS(3,SUB1) T-BALANCE(3,SUB1)
           END-PERFORM.
           MOVE 3 TO SUB.

      ******************************************************************
       GTT-ADD-TOTALS SECTION.
      ******************************************************************
           ADD WK-NOLOANS TO WS-GTT-NOLOANS (GTT-TOTAL-SUB,1).
           ADD WK-BALANCE TO WS-GTT-BALANCE (GTT-TOTAL-SUB,1).
           ADD WK-7-NOLOANS TO WS-GTT-NOLOANS(GTT-TOTAL-SUB,2).
           ADD WK-7-BALANCE TO WS-GTT-BALANCE(GTT-TOTAL-SUB,2).
           ADD WK-11-NOLOANS TO WS-GTT-NOLOANS(GTT-TOTAL-SUB,3).
           ADD WK-11-BALANCE TO WS-GTT-BALANCE(GTT-TOTAL-SUB,3).
           ADD WK-12-NOLOANS TO WS-GTT-NOLOANS(GTT-TOTAL-SUB,4).
           ADD WK-12-BALANCE TO WS-GTT-BALANCE(GTT-TOTAL-SUB,4).
           ADD WK-13-NOLOANS TO WS-GTT-NOLOANS(GTT-TOTAL-SUB,5).
           ADD WK-13-BALANCE TO WS-GTT-BALANCE(GTT-TOTAL-SUB,5).

      ***********************************
       GTT-COMPILE-GROUP-TOTALS SECTION.
      ***********************************
           MOVE SPACES      TO WK-KEY.
           MOVE WGR-SRESULT TO WK-BR.
           PERFORM START-WK-FILE.

       GTT-NEXT-WK1.
           PERFORM READ-WK-FILE-NEXT.
           IF ( IO-FG NOT = 0 ) OR
              ( WK-BR NOT = WGR-SRESULT )
                 GO TO EXIT-GTT-COMPILE-GROUP-TOTALS.

           MOVE "N" TO GTT-FIRST-TIME.
           PERFORM TRAP-INTERRUPT.

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

           MOVE "CHAPTER  7 :" TO G-HEADING1.
           MOVE WS-GTT-NOLOANS(GTT-TOTAL-SUB,2) TO G-LOANS.
           MOVE WS-GTT-BALANCE(GTT-TOTAL-SUB,2) TO G-TOTBAL.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CHAPTER 11 :" TO G-HEADING1.
           MOVE WS-GTT-NOLOANS(GTT-TOTAL-SUB,3) TO G-LOANS.
           MOVE WS-GTT-BALANCE(GTT-TOTAL-SUB,3) TO G-TOTBAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CHAPTER 12 :" TO G-HEADING1.
           MOVE WS-GTT-NOLOANS(GTT-TOTAL-SUB,4) TO G-LOANS.
           MOVE WS-GTT-BALANCE(GTT-TOTAL-SUB,4) TO G-TOTBAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CHAPTER 13 :" TO G-HEADING1.
           MOVE WS-GTT-NOLOANS(GTT-TOTAL-SUB,5) TO G-LOANS.
           MOVE WS-GTT-BALANCE(GTT-TOTAL-SUB,5) TO G-TOTBAL.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "ALL LOANS  :" TO G-HEADING1.
           MOVE WS-GTT-NOLOANS(GTT-TOTAL-SUB,1) TO G-LOANS.
           MOVE WS-GTT-BALANCE(GTT-TOTAL-SUB,1) TO G-TOTBAL.
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
           MOVE SPACES TO D-SNAME.

           MOVE LN-SSNO(SUB) TO BW-SSNO.

           IF BW-SSNO = 0
              GO TO END-PROCESS-BORROWER.

           MOVE BW-SSNO TO NUM-BW.
           PERFORM NUMBW-TO-ALPBW.

           PERFORM READ-BW1-FILE.
           IF IO-BAD
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
       PRINT-RANGE-LINES SECTION.
      ******************************************************************
           MOVE 4 TO LN-FEED.
      
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
      
           MOVE "BEG CLASS" TO RANGE-LINE.
           MOVE BEG-CLASS TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "END CLASS" TO RANGE-LINE.
           MOVE END-CLASS TO R-NUM.
           PERFORM WRITE-DETAIL-LINE.
      
           MOVE "BEG DATE" TO RANGE-LINE.
           MOVE BEG-DATE TO R-DATE.
           PERFORM WRITE-DETAIL-LINE.
      
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
       COPY "LIBWI/PFSWINDOWS.CPY".

       COPY "LIBGB/BRSECURE.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".
       COPY "LIBGB/GRTOTALS.CPY".
       COPY "LIBWI/GRTOTL.CPY".

      ************************************
       FORM-RESET SECTION.
      ************************************
           MOVE EXT-CONAME-CONAME TO BNKLG2-CONAME.
           MOVE EXT-CONAME-SYSDATE TO BNKLG2-SYSDATE.
           DISPLAY BNKLG2-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE BNKLG2-FORM-FIELDS TO WR-BUF.
           MOVE BNKLG2-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.
       FORM-RESET-EXIT.
           EXIT.

      ************************************
       SETUP-LINK-INFO SECTION.
      ************************************
           MOVE BNKLG2-HELP--FILE TO HELP-LINK-FILE.
           MOVE BNKLG2-HELP--DIR TO HELP-LINK-DIR.

      ************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/BNKLG2_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARA SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       SET-BRANCH-HEADINGS.
           MOVE HOLD-BRNO TO H-BRNO.
           MOVE HOLD-BRNAME TO H-BRDESC.
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
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".

       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBLP/LPSP1RN.CPY".
       COPY "LIBLP/LPLN1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBWKIN.CPY".
       COPY "LIBGB/GBWK2IN.CPY".
       COPY "LIBLP/LPLTP1RN.CPY".

       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
           PERFORM CLOSE-FILES.

           MOVE MKTEMP-BUF TO ACCESS-BUF.
           PERFORM REMOVE-WORKFILE.

           MOVE WK2-PATH TO ACCESS-BUF.
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
              IF EXT-EOBUF-GROUP-FG = "Y"
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH
                 MOVE SV-GROUP-FLAG TO GROUP-FLAG
                 MOVE SV-ENT-GROUP TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BR TO BEG-BRANCH
                 MOVE SV-END-BR TO END-BRANCH.

           IF EXT-ROUTE-BUF = "00"
              MOVE "LP/LPR2MU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.
       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY BNKLG2-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
