       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CIREPT.
       DATE-WRITTEN.    092111.
      ******************************************************************
      *
      *                      (LP)
      *    DESCRIPTION:  CUSTOMER INFORMATION STATS REPORTS
      *                  WORLD PR# 768
      *
      *=================================================================
      * REV:
      * BLM 111018 FIX LOWS PRINTING 99999999 IF NO DATA
      * BLM 130429 INCREASE TOTAL WORKERS TO ACCOMODATE LARGER NUMBERS
      *            & AMTS, WORLD PR# 873
      * KEC 2016.1003 {PD#00003} PARADATA <A30> [JAY CISZEWSKI]
      *         FIXED CI-DATE FIELDS TO BE IN CCYYMMDD FORMAT.
      * BAH 170517 ACTIVATE 8 DIGIT DATES, PD0006
      * BAH 181009 GLOBAL CIFILE
      * BAH 181205 GLOBAL LNFILE
      * BAH 20190919 REMOVED ACCESS-CALL ON CIFILE
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

      ******************************************************************
      *
      *    W O R K I N G - S T O R A G E - S E C T I O N
      *
      ******************************************************************
       WORKING-STORAGE SECTION.
       77  SCCS-IDENTIFIER PIC X(41) VALUE
           "@(#)LP/CIREPT.CBL J35 12/21/23-08:56:45 >".

       COPY "LIBGB/GB01BR.CPY".
       COPY "LIBGB/GB01BR_SQL.CPY".
       COPY "LIBGB/GBWSBR.CPY".
       COPY "LIBGB/GB01GR.CPY".
       COPY "LIBGB/GB01GR_SQL.CPY".
       COPY "LIBGB/GBWSGR.CPY".
       COPY "LIBLP/LP01LN.CPY".
       COPY "LIBLP/LP01LN_SQL.CPY".
       COPY "LIBLP/LPWSLN.CPY".
       COPY "LIBLP/LP01CI.CPY".
       COPY "LIBLP/LP01CI_SQL.CPY".
       COPY "LIBLP/LPWSCI.CPY".
       COPY "LIBLP/LP01CD.CPY".
       COPY "LIBLP/LP01CD_SQL.CPY".
       COPY "LIBLP/LPWSCD.CPY".
       COPY "LIBLP/LP01BW.CPY".
       COPY "LIBLP/LP01BW_SQL.CPY".
       COPY "LIBLP/LPWSBW.CPY".
      *-----------------------------------------------------------------
       01  PR-IFN               PIC X(2)     VALUE "PR".

       01  PREV-SCURR-TABLE.
           03  PREV-SCURR       PIC 9999 COMP OCCURS 15.

      ******************************************************************
      *
      *    L I N E - W O R K E R S
      *
      ******************************************************************
       01  LN-WORKERS.
           03  LN-FEED          PIC 999      VALUE 0.
           03  LN-COUNT         PIC 999      VALUE 99.
           03  LN-WK            PIC 999      VALUE 0.
       01  PAGE-NUMBER          PIC 9(4)     VALUE 0.

      ******************************************************************
      *
      *   M I S C E L L E N O U S - W O R K E R S
      *
      ******************************************************************
       01  NEW-BRANCH           PIC X        VALUE "Y".
       01  SUB                  PIC 9(2)     COMP.
       01  LEVEL-SUB            PIC 9999.
       01  TOT-SUB              PIC 99.
       01  GROUP-SUB            PIC 9999.
       01  PREV-SUB             PIC 9999.
       01  POS-SUB              PIC 9(2).
       01  LOW-SUB              PIC 99.

       01  RECORDS-FOUND        PIC X         VALUE " ".
           88  NO-RECORDS-FOUND               VALUE "N".

       01  FIRST-TIME           PIC X         VALUE "Y".
       01  LEGEND               PIC X(80)     VALUE SPACES.
       01  LEGEND-LIST          PIC X(09)     VALUE "LEGEND:B.".
       01  WORK-PCT             PIC S9(3)V99   COMP.
       01  WORK-YEARS           PIC S9(10)V9   COMP.
       01  WK-AVG-0DEC          PIC S9(10)     COMP.
       01  WK-AVG-1DEC          PIC S9(10)V9   COMP.
       01  YES-COUNT            PIC S9(10)     COMP.
       01  WORK-COUNT           PIC S9(10)     COMP.
       01  ELAPSED-MNTHS        PIC S9(5)      COMP.
       01  HOLD-BRANCH          PIC 9(4)       COMP   VALUE 0.
       01  HOLD-TYPE            PIC 9(2)       COMP   VALUE 0.
       01  CI-FOUND             PIC X.
       01  POSITION-TEST.
           03  POSITION-NUM     PIC 99.
           03  FILLER           PIC X(3).

       01  SYSDATE-YYYYMMDD              PIC 9(08)     VALUE ZEROES.


      ******************************************************************
      *
      *     S C R E E N   F I E L D S
      *
      ******************************************************************
       COPY "LIBGB/EOBUF_EXT.CPY".
      *-----------------------------------------------------------------
           03  BEG-BR                         PIC 9(4).
           03  END-BR                         PIC 9(4).
           03  BEG-CLASS                      PIC 9(2).
           03  END-CLASS                      PIC 9(2).
           03  BEG-LN-DATE                    PIC 9(8).
           03  END-LN-DATE                    PIC 9(8).
           03  PLS                            PIC X.
           03  PAIDOUTS                       PIC X.
           03  GROUP-FLAG                     PIC X.
           03  ENT-GROUP                      PIC X(4).
           03  TOTAL-LEV-TABLE.
               05  TOTAL-LEVEL-TABLE  PIC X OCCURS 10.
           03  DOC-FG                 PIC X.

       01  VERSION-CONTROL                    PIC X VALUE "A".

       01  SV-BEG-BR           PIC 9999  VALUE 9999.
       01  SV-END-BR           PIC 9999  VALUE 9999.
       01  SV-GROUP-FLAG       PIC X     VALUE SPACES.
       01  SV-ENT-GROUP        PIC X(4)  VALUE SPACES.

      ******************************************************************
      *    T O T A L   T A B L E
      ******************************************************************
       01  TOTAL-TABLE.
           03  TOTAL-TAB           OCCURS 10.
               05  TOT-COUNT                PIC S9(10)    COMP.
               05  TOT-WITH-CI              PIC S9(10)    COMP.
               05  TOT-MARRIED              PIC S9(10)    COMP.
               05  TOT-HOMEOWNER            PIC S9(10)    COMP.
               05  TOT-JOINT                PIC S9(10)    COMP.
               05  TOT-CHECKING             PIC S9(10)    COMP.
               05  TOT-SAVINGS              PIC S9(10)    COMP.
               05  TOT-YRS-AREA             PIC S9(10)    COMP.
               05  TOT-YRS-AREA-HIGH        PIC S9(10)    COMP.
               05  TOT-YRS-AREA-LOW         PIC S9(10)    COMP.
               05  TOT-MONTHS-ADDR          PIC S9(10)    COMP.
               05  TOT-MONTHS-ADDR-HIGH     PIC S9(10)    COMP.
               05  TOT-MONTHS-ADDR-LOW      PIC S9(10)    COMP.
               05  TOT-RENT                 PIC S9(10)V99 COMP.
               05  TOT-RENT-HIGH            PIC S9(10)V99 COMP.
               05  TOT-RENT-LOW             PIC S9(10)V99 COMP.
               05  TOT-EMPLOYMENT-TABLE OCCURS 2.
                   07  TOT-MONTHS-EMPL      PIC S9(10)    COMP.
                   07  TOT-MONTHS-EMPL-HIGH PIC S9(10)    COMP.
                   07  TOT-MONTHS-EMPL-LOW  PIC S9(10)    COMP.
                   07  TOT-INCOME           PIC S9(10)V99 COMP.
                   07  TOT-INCOME-HIGH      PIC S9(10)V99 COMP.
                   07  TOT-INCOME-LOW       PIC S9(10)V99 COMP.
               05  TOT-MO-INCOME            PIC S9(10)V99 COMP.
               05  TOT-MO-INCOME-HIGH       PIC S9(10)V99 COMP.
               05  TOT-MO-INCOME-LOW        PIC S9(10)V99 COMP.
               05  TOT-MO-PMTS              PIC S9(10)V99 COMP.
               05  TOT-MO-PMTS-HIGH         PIC S9(10)V99 COMP.
               05  TOT-MO-PMTS-LOW          PIC S9(10)V99 COMP.
               05  TOT-POSITION-TABLE OCCURS 26.
                   07  TOT-BOR1-COUNT       PIC S9(10)    COMP.
                   07  TOT-BOR2-COUNT       PIC S9(10)    COMP.
                   07  TOT-BOR1-INCOME      PIC S9(10)V99 COMP.
                   07  TOT-BOR2-INCOME      PIC S9(10)V99 COMP.
       01  DOCUMENT-LOW-HIGH.
           03  YRS-AREA-HIGH          PIC S9(10)   COMP.
           03  YRS-AREA-HIGH-BRNO     PIC 9(4)     COMP.
           03  YRS-AREA-HIGH-ACCT     PIC 9(6)     COMP.
           03  YRS-AREA-LOW           PIC S9(8)    COMP.
           03  YRS-AREA-LOW-BRNO      PIC 9(4)     COMP.
           03  YRS-AREA-LOW-ACCT      PIC 9(6)     COMP.
           03  MONTHS-ADDR-HIGH       PIC S9(10)   COMP.
           03  MONTHS-ADDR-HIGH-BRNO  PIC 9(4)     COMP.
           03  MONTHS-ADDR-HIGH-ACCT  PIC 9(6)     COMP.
           03  MONTHS-ADDR-LOW        PIC S9(8)    COMP.
           03  MONTHS-ADDR-LOW-BRNO   PIC 9(4)     COMP.
           03  MONTHS-ADDR-LOW-ACCT   PIC 9(6)     COMP.
           03  RENT-HIGH              PIC S9(8)V99 COMP.
           03  RENT-HIGH-BRNO         PIC 9(4)     COMP.
           03  RENT-HIGH-ACCT         PIC 9(6)     COMP.
           03  RENT-LOW               PIC S9(8)V99 COMP.
           03  RENT-LOW-BRNO          PIC 9(4)     COMP.
           03  RENT-LOW-ACCT          PIC 9(6)     COMP.
           03  MONTHS-EMPL-HIGH1      PIC S9(8)    COMP.
           03  MONTHS-EMPL-HIGH1-BRNO PIC 9(4)     COMP.
           03  MONTHS-EMPL-HIGH1-ACCT PIC 9(6)     COMP.
           03  MONTHS-EMPL-LOW1       PIC S9(8)    COMP.
           03  MONTHS-EMPL-LOW1-BRNO  PIC 9(4)     COMP.
           03  MONTHS-EMPL-LOW1-ACCT  PIC 9(6)     COMP.
           03  MONTHS-EMPL-HIGH2      PIC S9(8)    COMP.
           03  MONTHS-EMPL-HIGH2-BRNO PIC 9(4)     COMP.
           03  MONTHS-EMPL-HIGH2-ACCT PIC 9(6)     COMP.
           03  MONTHS-EMPL-LOW2       PIC S9(8)    COMP.
           03  MONTHS-EMPL-LOW2-BRNO  PIC 9(4)     COMP.
           03  MONTHS-EMPL-LOW2-ACCT  PIC 9(6)     COMP.
           03  INCOME-HIGH1           PIC S9(8)V99 COMP.
           03  INCOME-HIGH1-BRNO      PIC 9(4)     COMP.
           03  INCOME-HIGH1-ACCT      PIC 9(6)     COMP.
           03  INCOME-HIGH2           PIC S9(8)V99 COMP.
           03  INCOME-HIGH2-BRNO      PIC 9(4)     COMP.
           03  INCOME-HIGH2-ACCT      PIC 9(6)     COMP.
           03  INCOME-LOW1            PIC S9(8)V99 COMP.
           03  INCOME-LOW1-BRNO       PIC 9(4)     COMP.
           03  INCOME-LOW1-ACCT       PIC 9(6)     COMP.
           03  INCOME-LOW2            PIC S9(8)V99 COMP.
           03  INCOME-LOW2-BRNO       PIC 9(4)     COMP.
           03  INCOME-LOW2-ACCT       PIC 9(6)     COMP.
           03  MO-INCOME-HIGH         PIC S9(8)V99 COMP.
           03  MO-INCOME-HIGH-BRNO    PIC 9(4)     COMP.
           03  MO-INCOME-HIGH-ACCT    PIC 9(6)     COMP.
           03  MO-INCOME-LOW          PIC S9(8)V99 COMP.
           03  MO-INCOME-LOW-BRNO     PIC 9(4)     COMP.
           03  MO-INCOME-LOW-ACCT     PIC 9(6)     COMP.
           03  MO-PMTS-HIGH           PIC S9(8)V99 COMP.
           03  MO-PMTS-HIGH-BRNO      PIC 9(4)     COMP.
           03  MO-PMTS-HIGH-ACCT      PIC 9(6)     COMP.
           03  MO-PMTS-LOW            PIC S9(8)V99 COMP.
           03  MO-PMTS-LOW-BRNO       PIC 9(4)     COMP.
           03  MO-PMTS-LOW-ACCT       PIC 9(6)     COMP.
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
           03  FILLER           PIC X(5)     VALUE " ".
           03  H-NAME           PIC X(40)    VALUE " ".
           03  FILLER           PIC X(11)    VALUE " ".
           03  FILLER           PIC X(5)     VALUE "PAGE ".
           03  H-PAGE-NO        PIC ZZZ9.

      *-----------------------------------------------------------------
       01  HEAD2.
           03  FILLER           PIC X(9)     VALUE "USER ID:".
           03  H-USERID         PIC X(10).
           03  FILLER           PIC X        VALUE " ".
           03  FILLER           PIC X(40)    VALUE
           " CUSTOMER INFORMATION STATISTICS REPORT ".
           03  FILLER           PIC X(20)    VALUE SPACES.

      *-----------------------------------------------------------------
       01  HEAD3.
           03  H-BRGR           PIC X(9)  VALUE "BRANCH  :".
           03  H-GRID.
               05  H-BRNO       PIC Z(4).
           03  FILLER           PIC X VALUE " ".
           03  H-BRNAME         PIC X(20).
           03  FILLER           PIC X(2) VALUE " ".
           03  FILLER           PIC X(35)    VALUE SPACES.
           03  H-ISO1           PIC X(5).
           03  H-TYPE1          PIC X(4).

      *-----------------------------------------------------------------
       01  HEAD4.
           03  FILLER           PIC X(10)   VALUE "CLASS  :".
           03  H-BEG-CLASS      PIC 9(2)    VALUE ZEROES.
           03  FILLER           PIC X(1)    VALUE SPACES.
           03  H-TO-CLASS       PIC X(2).
           03  FILLER           PIC X(1).
           03  H-END-CLASS      PIC 9(2).
           03  FILLER           PIC X(2).
           03  H-BEG-DATE       PIC 99/99/99.
           03  FILLER           PIC X(6)    VALUE " THRU ".
           03  H-END-DATE       PIC 99/99/99.
           03  FILLER           PIC X(29)    VALUE SPACES.
           03  H-ISO2           PIC X(5).
           03  H-TYPE2          PIC X(4).

      *-----------------------------------------------------------------
       01  STAR-LINE.
           03  FILLER          PIC X(40) VALUE
           "________________________________________".
           03  FILLER          PIC X(40) VALUE
           "________________________________________".

       01  YES-NO-HEAD.
           03  FILLER           PIC X(35) VALUE SPACES.
           03  FILLER           PIC X(3)  VALUE "YES".
           03  FILLER           PIC X(20) VALUE SPACES.
           03  FILLER           PIC X(2)  VALUE "NO".
       01  YES-NO-HEAD2.
           03  FILLER           PIC X(32) VALUE SPACES.
           03  FILLER           PIC X(3)  VALUE "#".
           03  FILLER           PIC X(5)  VALUE SPACES.
           03  FILLER           PIC X(2)  VALUE "%".
           03  FILLER           PIC X(12) VALUE SPACES.
           03  FILLER           PIC X(3)  VALUE "#".
           03  FILLER           PIC X(6)  VALUE SPACES.
           03  FILLER           PIC X(2)  VALUE "%".
       01  HIGH-LOW-HEAD.
           03  FILLER           PIC X(35) VALUE SPACES.
           03  FILLER           PIC X(4)  VALUE "HIGH".
           03  FILLER           PIC X(11) VALUE SPACES.
           03  FILLER           PIC X(3)  VALUE "LOW".
           03  FILLER           PIC X(7)  VALUE SPACES.
           03  FILLER           PIC X(7)  VALUE "AVERAGE".
       01  POSITION-HEAD1.
           03  FILLER           PIC X(45) VALUE SPACES.
           03  FILLER           PIC X(20) VALUE "BORROWER 1".
           03  FILLER           PIC X(4)  VALUE SPACES.
           03  FILLER           PIC X(20) VALUE "BORROWER 2".
       01  POSITION-HEAD2.
           03  FILLER           PIC X(47) VALUE "POSITION".
           03  FILLER           PIC X(1)  VALUE "#".
           03  FILLER           PIC X(13) VALUE "   AVG INCOME".
           03  FILLER           PIC X(10) VALUE SPACES.
           03  FILLER           PIC X(1)  VALUE "#".
           03  FILLER           PIC X(11) VALUE " AVG INCOME".
       01  DOC-HEAD.
           03  FILLER           PIC X(36) VALUE "ITEM".
           03  FILLER           PIC X(4)  VALUE "BRNO".
           03  FILLER           PIC X(5)  VALUE SPACES.
           03  FILLER           PIC X(4)  VALUE "ACCT".
           03  FILLER           PIC X(7)  VALUE SPACES.
           03  FILLER           PIC X(5)  VALUE "VALUE".

      ******************************************************************
      *
      *    D E T A I L - L I N E
      *
      ******************************************************************
       01  DETAIL-LINE          PIC X(80) VALUE SPACES.
      *-----------------------------------------------------------------
       01  DETAIL1             REDEFINES DETAIL-LINE.
           03  D-PROMPT1        PIC X(25).
           03  D-TOT            PIC ZZZZZ,ZZZ-.
           03  FILLER           PIC X.
           03  D-PROMPT2        PIC X(20).
           03  FILLER           PIC X.
           03  D-TOT-WITH-CI    PIC ZZZZ,ZZZ-.
       01  DETAIL2             REDEFINES DETAIL-LINE.
           03  D-PROMPT         PIC X(25).
           03  D-YES            PIC ZZZZZ,ZZ9-.
           03  FILLER           PIC X.
           03  D-YES-PCT        PIC ZZZZ.Z9-.
           03  FILLER REDEFINES D-YES-PCT.
               05  FILLER       PIC X(7).
               05  D-PCT1       PIC X.
           03  FILLER           PIC X(3).
           03  D-NO             PIC ZZZZZ,ZZ9-.
           03  FILLER           PIC X.
           03  D-NO-PCT         PIC ZZZZ.Z9-.
           03  FILLER REDEFINES D-NO-PCT.
               05  FILLER       PIC X(7).
               05  D-PCT2       PIC X.
           03  FILLER           PIC X(7).
       01  DETAIL3             REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(30).
           03  D-HIGH-NO        PIC ZZZ,ZZZ.9-.
           03  D-HIGH-AMT REDEFINES D-HIGH-NO
                                PIC $$$$$,$$9-.
           03  FILLER           PIC X(4).
           03  D-LOW-NO         PIC ZZZ,ZZZ.9-.
           03  D-LOW-AMT REDEFINES D-LOW-NO
                                PIC $$$$$,$$9-.
           03  FILLER           PIC X(4).
           03  D-AVG-NO         PIC ZZZ,ZZZ.9-.
           03  D-AVG-AMT REDEFINES D-AVG-NO
                                PIC $$$$$,$$9-.
           03  FILLER           PIC X(12).
       01  DETAIL4             REDEFINES DETAIL-LINE.
           03  D-POS-CODE       PIC 99.
           03  FILLER           PIC X.
           03  D-DASH           PIC X.
           03  FILLER           PIC X.
           03  D-POSITION       PIC X(35).
           03  FILLER           PIC X.
           03  D-POS-COUNT1     PIC ZZZZ,ZZ9-.
           03  D-AVG-INCOME1    PIC ZZZZ,ZZ9- BLANK ZERO.
           03  FILLER           PIC X(3).
           03  D-POS-COUNT2     PIC ZZZZ,ZZ9-.
           03  D-AVG-INCOME2    PIC ZZZZ,ZZ9- BLANK ZERO.
       01  DETAIL-DOC           REDEFINES DETAIL-LINE.
           03  FILLER           PIC X(30).
           03  D-HI-LOW         PIC X(6).
           03  D-BRNO           PIC 9(4).
           03  FILLER           PIC X(3).
           03  D-ACCT           PIC 9(6).
           03  FILLER           PIC X(3).
           03  D-DOC-NO         PIC ZZZ,ZZZ.9-.
           03  D-DOC-AMT REDEFINES D-DOC-NO
                                PIC $$$$$,$$9-.

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - W O R K E R S
      *
      ******************************************************************
       COPY "LIBWI/GRSCANW.CPY".
       COPY "LIBGB/PROGIDW.CPY".
       COPY "LIBGB/GRWORKW.CPY".
       COPY "LIBWI/GRTOTLW.CPY".
       COPY "LIBGB/CONNECTW_SQL.CPY".
       COPY "LIBGB/FILEWK.CPY".
       COPY "LIBGB/MESSWK.CPY".
       COPY "LIBGB/DATERW.CPY".
       COPY "LIBGB/TIMEWK.CPY".
       COPY "LIBGB/GETPRW.CPY".
       COPY "LIBGB/MKTEMPW_CMD.CPY".
       COPY "LIBGB/SYSTEMW.CPY".
       COPY "LIBGB/ACCESSW.CPY".
       COPY "LIBGB/DACCESSW.CPY".
       COPY "LIBGB/GETENVW.CPY".
       COPY "LIBGB/CONAME_EXT.CPY".
       COPY "LIBGB/SCREENW.CPY".
       COPY "LIBLP/CIREPT_DEF.CPY".
       COPY "LIBLP/CIREPT_WKS.CPY".
       COPY "LIBGB/HELPLINK.CPY".
       COPY "LIBGB/PRU_LNK.CPY".

      ******************************************************************
      *
      *     P R O G R A M   C O N T R O L
      *
      ******************************************************************
       COPY "LIBGB/GETFMW.CPY".
       SCREEN SECTION.
       COPY "LIBLP/CIREPT_SCN.CPY".

      *-----------------------------------------------------------------
       PROCEDURE DIVISION
            USING FORM-PATHNAME EXIT-PATHNAME.
       DECLARATIVES.
       DECLARE-ERROR SECTION.
           USE AFTER ERROR PROCEDURE ON
               PR-FILE.
       COPY "LIBGB/DECLRP.CPY".
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-CI1-FILE.
           PERFORM CLOSE-CD1-FILE.
           PERFORM CLOSE-BW1-FILE.
           PERFORM CLOSE-GR1-FILE.
           PERFORM CLOSE-BR-FILE.
           PERFORM CLOSE-PR-FILE.
       COPY "LIBGB/DECLRP2.CPY".

      ******************************************************************
      *
      *    M A I N - M O D U L E
      *
      ******************************************************************
       MAIN-MODULE SECTION.

           PERFORM SQL-CONNECT.

      D    STOP MESS.
           MOVE FORM-PATHNAME TO FORM-PATH.
           MOVE "CIREPT" TO FORM-PROG VDU-FORM-NAME.

           PERFORM INITIALIZATION.
           IF EXT-EOBUF-ERRCD NOT = " "
              GO TO END-PROGRAM.

           IF EXT-ROUTE-BUF = "00"
              MOVE "REPORT IN PROGRESS.........." TO LEGEND
              PERFORM SEND-LEGEND.

           PERFORM OPEN-PR-FILE.

           IF GROUP-FLAG = "N"
              MOVE BEG-BR TO BR-NO
              PERFORM START-BR-FILE
           ELSE
              MOVE ENT-GROUP TO GR-ID
              PERFORM SEARCH-GR-INIT.

      *----------------------------------------------------------------
       NEXT-GROUP-LOC.
           IF GROUP-FLAG NOT = "Y"
              GO TO NEXT-BRANCH-NO.

           PERFORM SEARCH-GR-NEXT.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * AT END OF FILE, FORCE WGR-SCURR -TABLE & TPLUS TO CHANGE FROM
      * PREVIOUS SO GRAND TOTALS WILL PRINT - MUST FORCE BECAUSE ALL
      * LEVELS START AT THE SAME POINT (THE SELECTED PARENT GROUP)
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF WGR-SRESULT = 0
              MOVE ZEROS TO WGR-SCURR-TABLE
              MOVE 15 TO WGR-TPLUS
              PERFORM TOTAL-LEVEL-CHECK
              GO TO END-ROUTINE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF FIRST-TIME = "Y"
              MOVE "N" TO FIRST-TIME
              MOVE WGR-SCURR-TABLE TO PREV-SCURR-TABLE
           ELSE
              PERFORM TOTAL-LEVEL-CHECK.

           MOVE WGR-SRESULT TO BR-NO.
           PERFORM READ-BR-FILE.
           IF IO-BAD
              GO TO IO-ERROR.

           GO TO MOVE-BRANCH-PATH-INFO.

      *----------------------------------------------------------------
       NEXT-BRANCH-NO.
           PERFORM READ-BR-FILE-NEXT.
           IF IO-BAD OR BR-NO > END-BR
              PERFORM PRINT-BRANCH-GRAND-TOTALS
              GO TO END-ROUTINE.

      *----------------------------------------------------------------
       MOVE-BRANCH-PATH-INFO.
           MOVE "Y" TO NEW-BRANCH.

           MOVE BR-NO TO HOLD-BRANCH.
           MOVE 0 TO HOLD-TYPE.

           MOVE EXT-FILPATH-BASE           TO FILPATH-BASE.
           MOVE BR-MACHINE                 TO FILPATH-MACHINE.
           MOVE BR-DATA-PATH               TO FILPATH-DATA.

           MOVE "B"                        TO BW-PATH-OVERRIDE
                                              CI-PATH-OVERRIDE
                                              LN-PATH-OVERRIDE.

           IF EXT-ROUTE-BUF = "00"
              MOVE 9999 TO PROGID-SLEEP-COUNTER
              PERFORM SEND-PROGRESS-INDICATOR.

           PERFORM OPEN-LN3-FILE.
           PERFORM OPEN-CI1-FILE.
           PERFORM OPEN-BW1-FILE.


           MOVE BR-NO     TO LN-OWNBR
                             QLN3-WBEG-OWNBR
                             QLN3-WEND-OWNBR.
           MOVE BEG-CLASS TO LN-CLASS
                             QLN3-WBEG-CLASS.
           MOVE END-CLASS TO QLN3-WEND-CLASS.
           MOVE 0         TO LN-ACCTNO
                             QLN3-WBEG-ACCTNO.
           MOVE ALL "9"   TO QLN3-WEND-ACCTNO.

           PERFORM START-LN3-FILE.

       NEXT-LN.
           PERFORM READ-LN3-FILE-NEXT.
           IF IO-BAD OR (LN-OWNBR NOT = LN-PATH-OWNBR)
              IF GROUP-FLAG = "Y"
                 GO TO END-OF-GROUP
              ELSE
                 PERFORM PRINT-ONE-BRANCH-TOTALS
                 GO TO END-OF-GROUP.

           PERFORM SEND-PROGRESS-INDICATOR.

      *  POCD - **  IS A DELETED CONTRACT
           IF ( LN-POCD-NONLOAN )
              GO TO NEXT-LN.

           IF ( LN-CLASS < BEG-CLASS ) GO TO NEXT-LN.

           IF ( LN-CLASS > END-CLASS )
              IF GROUP-FLAG = "Y"
                 GO TO END-OF-GROUP
              ELSE
                 PERFORM PRINT-ONE-BRANCH-TOTALS
                 GO TO END-OF-GROUP.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF PAIDOUTS = "O"
              IF LN-POFFDATE = 0
                 GO TO NEXT-LN.

           IF PAIDOUTS = "S"
              IF LN-POFFDATE NOT = 0
                 GO TO NEXT-LN.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           IF PLS = "O"
              IF LN-PLDATE = 0
                 GO TO NEXT-LN.

           IF PLS = "S"
              IF LN-PLDATE NOT = 0
                 GO TO NEXT-LN.

           IF LN-LOANDATE < BEG-LN-DATE OR LN-LOANDATE > END-LN-DATE
              GO TO NEXT-LN.

           MOVE LN-OWNBR  TO CI-BRNO.
           MOVE LN-ACCTNO TO CI-ACCTNO.
           PERFORM READ-CI1-FILE.
           IF IO-BAD
              MOVE "N" TO CI-FOUND
           ELSE
              MOVE "Y" TO CI-FOUND
              MOVE LN-SSNO(1) TO BW-SSNO
              PERFORM READ-BW1-FILE
              IF IO-BAD
                 MOVE SPACES TO BW-RENTCD.

           MOVE "N" TO FIRST-TIME.
           PERFORM CHECK-ADD-TOTALS.
           GO TO NEXT-LN.

      *-----------------------------------------------------------------
       END-OF-GROUP.
           PERFORM CLOSE-LN3-FILE.
           PERFORM CLOSE-CI1-FILE.
           PERFORM CLOSE-BW1-FILE.
           GO TO NEXT-GROUP-LOC.

      *-----------------------------------------------------------------
       END-ROUTINE.
           IF FIRST-TIME = "Y"
              MOVE "N" TO RECORDS-FOUND.

           IF DOC-FG = "Y"
              PERFORM PRINT-DOC.

           GO TO END-PROGRAM.

      ******************************************************************
       INITIALIZATION SECTION.
      ******************************************************************
           MOVE CIREPT-QUEUE-POS TO Q-POS.
           MOVE CIREPT-COPIES-POS TO C-POS.
           MOVE CIREPT-PRU-POS   TO P-POS.
           MOVE SPACES TO Q-BUF C-BUF P-BUF.
       COPY "LIBGB/EOINIT.CPY".

           IF EXT-EOBUF-GROUP-FG = "Y"
              MOVE BEG-BR TO SV-BEG-BR
              MOVE END-BR TO SV-END-BR
              MOVE GROUP-FLAG TO SV-GROUP-FLAG
              MOVE ENT-GROUP TO SV-ENT-GROUP
              MOVE EXT-EOBUF-BRNO TO BEG-BR END-BR
              MOVE "N" TO GROUP-FLAG
              MOVE EXT-EOBUF-GROUP-PRPATH TO PRU-FILE
           ELSE
              IF ( BEG-BR = 0 AND END-BR = 0 )
                 MOVE BEG-BR TO SV-BEG-BR
                 MOVE END-BR TO SV-END-BR
                 MOVE EXT-CONAME-USER-LOC TO BEG-BR END-BR.

           PERFORM OPEN-BR-FILE.
           PERFORM OPEN-GR1-FILE.
           PERFORM OPEN-CD1-FILE.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE EXT-CONAME-CONAME TO H-NAME.
           MOVE EXT-CONAME-SYSDATE TO H-DATE.
           MOVE EXT-CONAME-USER-ID TO H-USERID.

           MOVE BEG-LN-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO H-BEG-DATE.
           MOVE END-LN-DATE TO DATE-YYYYMMDD.
           PERFORM CONVERT-YYYYMMDD-TO-MMDDYY.
           MOVE DATE-MMDDYY TO H-END-DATE.

           MOVE BEG-CLASS TO H-BEG-CLASS.
           MOVE "TO"      TO H-TO-CLASS.
           MOVE END-CLASS TO H-END-CLASS.
           IF PLS = "I"
              MOVE "INCL" TO H-ISO1
           ELSE IF PLS = "O"
              MOVE "ONLY" TO H-ISO1
           ELSE
              MOVE "SKIP" TO H-ISO1.
           MOVE "PL'S" TO H-TYPE1.
           IF PAIDOUTS = "I"
              MOVE "INCL" TO H-ISO2
           ELSE IF PAIDOUTS = "O"
              MOVE "ONLY" TO H-ISO2
           ELSE
              MOVE "SKIP" TO H-ISO2.
           MOVE "PO'S" TO H-TYPE2.
      *-----------------------------------------------------------------
       INITIALIZATION-MISC.

           MOVE EXT-CONAME-SYSDATE TO ALP-DATE.
           PERFORM CONVERT-ALPDATE-TO-YYYYMMDD.
           MOVE DATE-YYYYMMDD      TO SYSDATE-YYYYMMDD.

           MOVE LOW-VALUES TO TOTAL-TABLE.
           PERFORM VARYING SUB FROM 1 BY 1 UNTIL SUB > 10
               MOVE 99999999 TO TOT-YRS-AREA-LOW(SUB)
                                TOT-MONTHS-ADDR-LOW(SUB)
                                TOT-RENT-LOW(SUB)
                                TOT-MONTHS-EMPL-LOW(SUB, 1)
                                TOT-MONTHS-EMPL-LOW(SUB, 2)
                                TOT-INCOME-LOW(SUB, 1)
                                TOT-INCOME-LOW(SUB, 2)
                                TOT-MO-INCOME-LOW(SUB)
                                TOT-MO-PMTS-LOW(SUB)
           END-PERFORM.

           IF DOC-FG = "Y"
              MOVE 99999999 TO
                    YRS-AREA-LOW
                    MONTHS-ADDR-LOW
                    RENT-LOW
                    MONTHS-EMPL-LOW1
                    MONTHS-EMPL-LOW2
                    INCOME-LOW1
                    INCOME-LOW2
                    MO-INCOME-LOW
                    MO-PMTS-LOW
              MOVE 0 TO
                    YRS-AREA-HIGH
                    MONTHS-ADDR-HIGH
                    RENT-HIGH
                    MONTHS-EMPL-HIGH1
                    MONTHS-EMPL-HIGH2
                    INCOME-HIGH1
                    INCOME-HIGH2
                    MO-INCOME-HIGH
                    MO-PMTS-HIGH.

           IF GROUP-FLAG = "Y"
              IF DOC-FG = "Y"
                 MOVE 1 TO LOW-SUB
                 PERFORM VARYING SUB FROM 10 BY -1
                                 UNTIL SUB < 1
                    IF TOTAL-LEVEL-TABLE(SUB) = "Y"
                       MOVE SUB TO LOW-SUB
                     END-IF
                 END-PERFORM.

           GO TO EXIT-INITIALIZE.

       ENDIT.
           MOVE "X" TO EXT-EOBUF-ERRCD.
       EXIT-INITIALIZE.
           EXIT.

      ******************************************************************
       EO-EXEC-AUDIT SECTION.
      ******************************************************************
           MOVE EXT-EOBUF-DATE1 TO BEG-LN-DATE.
           MOVE EXT-EOBUF-DATE2 TO END-LN-DATE.

      ******************************************************************
       ENTER-PARAMETERS SECTION.
      ******************************************************************
       ENTER-PARM.

           IF VDUSTATUS = "1U"
              GO TO DOC-YN.

       BR-01.
           MOVE "E  A040BR1." TO VDU.
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
              MOVE 0 TO BEG-BR
              MOVE 9999 TO END-BR
              GO TO ENTER-BEG-CLASS.

      *----------------------------------------------------------------
       BRANCH-IS-NUMERIC.
           MOVE "N"           TO GROUP-FLAG.
           MOVE WGR-SRESULT   TO BEG-BR.
           MOVE "SN N040BR1." TO VDU.
           MOVE BEG-BR        TO VDUNUM0.
           PERFORM SCREENIO.

      *----------------------------------------------------------------
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

           IF ( (BEG-BR = 0) AND (END-BR = 0) )
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE "USER BRANCH ONLY" TO VDUBUF
              PERFORM SCREENIO
           ELSE
              MOVE "S  A160DISPLAY-USER." TO VDU
              MOVE SPACES TO VDUBUF
              PERFORM SCREENIO.

           GO TO ENTER-BEG-CLASS.

      *-----------------------------------------------------------------
       ALL-BR.
           MOVE "SNNN040BR1." TO VDU.
           MOVE 0 TO BEG-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "SNNN040BR2." TO VDU.
           MOVE 9999 TO END-BR VDUNUM0.
           PERFORM SCREENIO.
           MOVE "N" TO GROUP-FLAG.

      *=================================================================
       ENTER-BEG-CLASS.

           MOVE "ENNN020BEG-CLASS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO BR-02.
           IF (        F1-KEY ) GO TO EXIT-PARM.
           IF (        F3-KEY ) GO TO ENTER-ALL-CLASS.
           IF (        F7-KEY ) GO TO EXIT-PARM.
           IF ( NOT ENTER-KEY ) GO TO ENTER-BEG-CLASS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO BEG-CLASS.

      *-----------------------------------------------------------------
       ENTER-END-CLASS.

           MOVE "ENNN020END-CLASS." TO VDU.
           PERFORM SCREENIO.

           IF (        UP-KEY ) GO TO ENTER-BEG-CLASS.
           IF (        F1-KEY ) GO TO EXIT-PARM.
           IF (        F3-KEY ) GO TO ENTER-ALL-CLASS.
           IF (        F7-KEY ) GO TO EXIT-PARM.
           IF ( NOT ENTER-KEY ) GO TO ENTER-END-CLASS.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE VDUNUM0 TO END-CLASS.

           IF ( END-CLASS = ZEROES ) GO TO ENTER-END-CLASS.

           IF ( BEG-CLASS > END-CLASS )
              MOVE "INVALID CLASS RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO ENTER-BEG-CLASS.

           GO TO DT-01.

      *-----------------------------------------------------------------
       ENTER-ALL-CLASS.

           MOVE "SNNN020BEG-CLASS." TO VDU.
           MOVE ZEROES              TO VDUNUM0 BEG-CLASS.
           PERFORM SCREENIO.

           MOVE "SNNN020END-CLASS." TO VDU.
           MOVE 99                  TO VDUNUM0 END-CLASS.
           PERFORM SCREENIO.

      *=================================================================
       DT-01.
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO PLS-YN.
           MOVE "ERNM060DT1." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "16" GO TO ALL-DT.
           IF VDUSTATUS = "1U" GO TO ENTER-END-CLASS.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DT-01.
           MOVE VDU-DATE-YYYYMMDD TO BEG-LN-DATE.

      *-----------------------------------------------------------------
       DT-02.
           IF EXT-ROUTE-BUF NOT = "00"
              GO TO PLS-YN.
           MOVE "ERNM060DT2." TO VDU.
           PERFORM SCREENIO.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO DT-01.
           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF NOT (VDUSTATUS = "00" OR "1D") GO TO DT-02.
           MOVE VDU-DATE-YYYYMMDD TO END-LN-DATE.
           IF END-LN-DATE < BEG-LN-DATE
              MOVE "INVALID DATE RANGE" TO MESS
              PERFORM SEND-MESS
              GO TO DT-01.

           GO TO PLS-YN.

      *-----------------------------------------------------------------
       ALL-DT.
           MOVE "S  8080DT1." TO VDU.
           MOVE EXT-JULIAN-BEG TO BEG-LN-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.
           MOVE "S  8080DT2." TO VDU.
           MOVE EXT-JULIAN-END TO END-LN-DATE VDU-DATE-YYYYMMDD.
           PERFORM SCREENIO.

      *-----------------------------------------------------------------
       PLS-YN.
           MOVE "E  A010PL." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF UP-KEY
              IF EXT-ROUTE-BUF = "00"
                 GO TO DT-02
              ELSE
                 GO TO ENTER-END-CLASS.

           IF NOT (VDUSTATUS = "00" OR "1D") GO TO PLS-YN.

           MOVE VDUBUF TO PLS.

           IF PLS = "I" OR "O"
              GO TO PAIDOUTS-YN.

           IF PLS NOT = "S" GO TO PLS-YN.
      *-----------------------------------------------------------------
       PAIDOUTS-YN.
           MOVE "E  A010PAID." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO PLS-YN.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO PAIDOUTS-YN.

           MOVE VDUBUF TO PAIDOUTS.

           IF PAIDOUTS = "I" OR "O"
              GO TO CHECK-LEVEL-PRINTS.

           IF PAIDOUTS NOT = "S" GO TO PAIDOUTS-YN.

      *-----------------------------------------------------------------
       DOC-YN.
           IF GROUP-FLAG = "N"
              GO TO CHECK-LEVEL-PRINTS.
           MOVE "E  A010HILO." TO VDU.
           PERFORM SCREENIO.

           IF VDUSTATUS = "1>" GO TO EXIT-PARM.
           IF VDUSTATUS = "10" GO TO EXIT-PARM.
           IF VDUSTATUS = "1U" GO TO PAIDOUTS-YN.
           IF NOT (VDUSTATUS = "00" OR "1D")
              GO TO DOC-YN.

           MOVE VDUBUF TO DOC-FG.

           IF DOC-FG NOT = "Y"
              IF DOC-FG NOT = "N"
                 GO TO DOC-YN.

       CHECK-LEVEL-PRINTS.
           IF ( GROUP-FLAG = "Y" )
              PERFORM OPEN-GR1-FILE
              MOVE ENT-GROUP TO GR-ID
              PERFORM SEARCH-GR-INIT
              PERFORM GRTOTL-WINDOW
           ELSE
              MOVE SPACES TO TOTAL-LEV-TABLE
              MOVE "Y" TO TOTAL-LEVEL-TABLE(1)
                          TOTAL-LEVEL-TABLE(9).
       EXIT-PARM.
           EXIT.

      ******************************************************************
       DISPLAY-PARAMETERS SECTION.
      ******************************************************************

           IF GROUP-FLAG = "Y"
              MOVE "S  A040BR1." TO VDU
              MOVE ENT-GROUP TO VDUBUF
              PERFORM SCREENIO
              MOVE "S  A040BR2." TO VDU
              PERFORM SCREENIO
           ELSE
              MOVE "SNNN040BR1." TO VDU
              MOVE BEG-BR TO VDUNUM0
              PERFORM SCREENIO
              MOVE "SNNN040BR2." TO VDU
              MOVE END-BR TO VDUNUM0
              PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "SNNN020BEG-CLASS." TO VDU.
           MOVE BEG-CLASS           TO VDUNUM0.
           PERFORM SCREENIO.

           MOVE "SNNN020END-CLASS." TO VDU.
           MOVE END-CLASS           TO VDUNUM0.
           PERFORM SCREENIO.

      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010PL." TO VDU.
           MOVE PLS TO VDUBUF.
           PERFORM SCREENIO.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE "S  A010PAID." TO VDU.
           MOVE PAIDOUTS TO VDUBUF.
           PERFORM SCREENIO.

      ******************************************************************
       CHECK-ADD-TOTALS SECTION.
      ******************************************************************

           IF GROUP-FLAG = "N"
              MOVE 9 TO TOT-SUB
              PERFORM ADD-TOTALS
              MOVE 1 TO TOT-SUB
              PERFORM ADD-TOTALS
              GO TO EXIT-CHECK-TOTALS.

           MOVE WGR-TPLUS TO GROUP-SUB.

      *----------------------------------------------------------------
       CHECK-TOTAL-LOOP.
           IF GROUP-SUB = 0
              GO TO EXIT-CHECK-TOTALS.

           MOVE WGR-SCURR(GROUP-SUB) TO LEVEL-SUB.

           IF WGR-SID(LEVEL-SUB) IS NUMERIC
              MOVE 9 TO TOT-SUB
              GO TO CHECK-TOTAL-FLAG.

           MOVE WGR-SID(LEVEL-SUB) TO GR-ID.
           PERFORM READ-GR1-FILE.
           MOVE GR-LEVEL TO TOT-SUB.

      *----------------------------------------------------------------
       CHECK-TOTAL-FLAG.
           IF TOTAL-LEVEL-TABLE(TOT-SUB) NOT = "Y"
              GO TO CONTINUE-CHECK-TOTALS.

           PERFORM ADD-TOTALS.

      *----------------------------------------------------------------
       CONTINUE-CHECK-TOTALS.
           SUBTRACT 1 FROM GROUP-SUB.

           IF GROUP-SUB > 0
              GO TO CHECK-TOTAL-LOOP.

       EXIT-CHECK-TOTALS.
           EXIT.

      ******************************************************************
       ADD-TOTALS SECTION.
      ******************************************************************
           ADD 1 TO TOT-COUNT(TOT-SUB).
           IF CI-FOUND = "Y"
              ADD 1 TO TOT-WITH-CI(TOT-SUB)
           ELSE
              GO TO EXIT-ADD-TOTALS.
           IF CI-MARITAL-STAT = "M"
              ADD 1 TO TOT-MARRIED(TOT-SUB).
           IF BW-RENTCD = "BUY" OR "OWN" OR "OWNS"
              ADD 1 TO TOT-HOMEOWNER(TOT-SUB).
           IF CI-SSNO-2 NOT = 0
              ADD 1 TO TOT-JOINT(TOT-SUB).
           IF CI-CHECKING-ACCOUNT = "Y"
              ADD 1 TO TOT-CHECKING(TOT-SUB).
           IF CI-SAVINGS-ACCOUNT = "Y"
              ADD 1 TO TOT-SAVINGS(TOT-SUB).
           ADD CI-YEARS-IN-AREA TO TOT-YRS-AREA(TOT-SUB).
           IF CI-YEARS-IN-AREA > TOT-YRS-AREA-HIGH(TOT-SUB)
              MOVE CI-YEARS-IN-AREA TO TOT-YRS-AREA-HIGH(TOT-SUB).
           IF DOC-FG = "Y"
              IF TOT-SUB = LOW-SUB
                 IF CI-YEARS-IN-AREA > YRS-AREA-HIGH
                    MOVE CI-YEARS-IN-AREA TO YRS-AREA-HIGH
                    MOVE BR-NO TO YRS-AREA-HIGH-BRNO
                    MOVE CI-ACCTNO TO YRS-AREA-HIGH-ACCT.
           IF CI-YEARS-IN-AREA < TOT-YRS-AREA-LOW(TOT-SUB)
              MOVE CI-YEARS-IN-AREA TO TOT-YRS-AREA-LOW(TOT-SUB).
           IF DOC-FG = "Y"
              IF TOT-SUB = LOW-SUB
                 IF CI-YEARS-IN-AREA < YRS-AREA-LOW
                    MOVE CI-YEARS-IN-AREA TO YRS-AREA-LOW
                    MOVE BR-NO TO YRS-AREA-LOW-BRNO
                    MOVE CI-ACCTNO TO YRS-AREA-LOW-ACCT.

           IF ( CI-DATE-AT-PRESENT-ADR NOT = ZEROES )
              MOVE CI-DATE-AT-PRESENT-ADR TO NUM-DATE
              MOVE SYSDATE-YYYYMMDD       TO SYS-DATE
              PERFORM TIMALL
              MOVE ELAPSED-MONTHS TO ELAPSED-MNTHS
           ELSE
              MOVE ZEROES                 TO ELAPSED-MNTHS.

           ADD ELAPSED-MNTHS TO TOT-MONTHS-ADDR(TOT-SUB).
           IF ELAPSED-MNTHS > TOT-MONTHS-ADDR-HIGH(TOT-SUB)
              MOVE ELAPSED-MNTHS TO TOT-MONTHS-ADDR-HIGH(TOT-SUB).
           IF DOC-FG = "Y"
              IF TOT-SUB = LOW-SUB
                 IF ELAPSED-MNTHS > MONTHS-ADDR-HIGH
                    MOVE ELAPSED-MNTHS TO MONTHS-ADDR-HIGH
                    MOVE BR-NO TO MONTHS-ADDR-HIGH-BRNO
                    MOVE CI-ACCTNO TO MONTHS-ADDR-HIGH-ACCT.

           IF ELAPSED-MNTHS < TOT-MONTHS-ADDR-LOW(TOT-SUB)
              MOVE ELAPSED-MNTHS TO TOT-MONTHS-ADDR-LOW(TOT-SUB).
           IF DOC-FG = "Y"
              IF TOT-SUB = LOW-SUB
                 IF ELAPSED-MNTHS < MONTHS-ADDR-LOW
                    MOVE ELAPSED-MNTHS TO MONTHS-ADDR-LOW
                    MOVE BR-NO TO MONTHS-ADDR-LOW-BRNO
                    MOVE CI-ACCTNO TO MONTHS-ADDR-LOW-ACCT.

           ADD CI-MONTHLY-RENT-MTG TO TOT-RENT(TOT-SUB).
           IF CI-MONTHLY-RENT-MTG > TOT-RENT-HIGH(TOT-SUB)
              MOVE CI-MONTHLY-RENT-MTG TO TOT-RENT-HIGH(TOT-SUB).
           IF DOC-FG = "Y"
              IF TOT-SUB = LOW-SUB
                 IF CI-MONTHLY-RENT-MTG > RENT-HIGH
                    MOVE CI-MONTHLY-RENT-MTG TO RENT-HIGH
                    MOVE BR-NO TO RENT-HIGH-BRNO
                    MOVE CI-ACCTNO TO RENT-HIGH-ACCT.
           IF CI-MONTHLY-RENT-MTG < TOT-RENT-LOW(TOT-SUB)
              MOVE CI-MONTHLY-RENT-MTG TO TOT-RENT-LOW(TOT-SUB).
           IF DOC-FG = "Y"
              IF TOT-SUB = LOW-SUB
                 IF CI-MONTHLY-RENT-MTG < RENT-LOW
                    MOVE CI-MONTHLY-RENT-MTG TO RENT-LOW
                    MOVE BR-NO TO RENT-LOW-BRNO
                    MOVE CI-ACCTNO TO RENT-LOW-ACCT.

           IF ( CI-DATE-HIRE-1 NOT = ZEROES )
              MOVE CI-DATE-HIRE-1          TO NUM-DATE
              MOVE SYSDATE-YYYYMMDD        TO SYS-DATE
              PERFORM TIMALL
              MOVE ELAPSED-MONTHS TO ELAPSED-MNTHS
           ELSE
              MOVE ZEROES                  TO ELAPSED-MNTHS.

           ADD ELAPSED-MNTHS TO TOT-MONTHS-EMPL(TOT-SUB, 1).
           IF ELAPSED-MNTHS > TOT-MONTHS-EMPL-HIGH(TOT-SUB, 1)
              MOVE ELAPSED-MNTHS TO TOT-MONTHS-EMPL-HIGH(TOT-SUB, 1).
           IF DOC-FG = "Y"
              IF TOT-SUB = LOW-SUB
                 IF ELAPSED-MNTHS > MONTHS-EMPL-HIGH1
                    MOVE ELAPSED-MNTHS TO MONTHS-EMPL-HIGH1
                    MOVE BR-NO TO MONTHS-EMPL-HIGH1-BRNO
                    MOVE CI-ACCTNO TO MONTHS-EMPL-HIGH1-ACCT.
           IF ELAPSED-MNTHS < TOT-MONTHS-EMPL-LOW(TOT-SUB, 1)
              MOVE ELAPSED-MNTHS TO TOT-MONTHS-EMPL-LOW(TOT-SUB, 1).
           IF DOC-FG = "Y"
              IF TOT-SUB = LOW-SUB
                 IF ELAPSED-MNTHS < MONTHS-EMPL-LOW1
                    MOVE ELAPSED-MNTHS TO MONTHS-EMPL-LOW1
                    MOVE BR-NO TO MONTHS-EMPL-LOW1-BRNO
                    MOVE CI-ACCTNO TO MONTHS-EMPL-LOW1-ACCT.

           IF ( CI-DATE-HIRE-2 NOT = ZEROES )
              MOVE CI-DATE-HIRE-2          TO NUM-DATE
              MOVE SYSDATE-YYYYMMDD        TO SYS-DATE
              PERFORM TIMALL
              MOVE ELAPSED-MONTHS TO ELAPSED-MNTHS
           ELSE
              MOVE ZEROES                  TO ELAPSED-MNTHS.

           ADD ELAPSED-MNTHS TO TOT-MONTHS-EMPL(TOT-SUB, 2).
           IF CI-SSNO-2 NOT = 0
              IF ELAPSED-MNTHS > TOT-MONTHS-EMPL-HIGH(TOT-SUB, 2)
                 MOVE ELAPSED-MNTHS TO TOT-MONTHS-EMPL-HIGH(TOT-SUB, 2).
           IF CI-SSNO-2 NOT = 0
              IF DOC-FG = "Y"
                 IF TOT-SUB = LOW-SUB
                    IF ELAPSED-MNTHS > MONTHS-EMPL-HIGH2
                       MOVE ELAPSED-MNTHS TO MONTHS-EMPL-HIGH2
                       MOVE BR-NO TO MONTHS-EMPL-HIGH2-BRNO
                       MOVE CI-ACCTNO TO MONTHS-EMPL-HIGH2-ACCT.
           IF CI-SSNO-2 NOT = 0
              IF ELAPSED-MNTHS < TOT-MONTHS-EMPL-LOW(TOT-SUB, 2)
                 MOVE ELAPSED-MNTHS TO TOT-MONTHS-EMPL-LOW(TOT-SUB, 2).
           IF CI-SSNO-2 NOT = 0
              IF DOC-FG = "Y"
                 IF TOT-SUB = LOW-SUB
                    IF ELAPSED-MNTHS < MONTHS-EMPL-LOW2
                       MOVE ELAPSED-MNTHS TO MONTHS-EMPL-LOW2
                       MOVE BR-NO TO MONTHS-EMPL-LOW2-BRNO
                       MOVE CI-ACCTNO TO MONTHS-EMPL-LOW2-ACCT.

           ADD CI-GROSS-MO-INCOME-1 TO TOT-INCOME(TOT-SUB, 1).
           IF CI-GROSS-MO-INCOME-1 > TOT-INCOME-HIGH(TOT-SUB, 1)
              MOVE CI-GROSS-MO-INCOME-1 TO TOT-INCOME-HIGH(TOT-SUB, 1).
           IF DOC-FG = "Y"
              IF TOT-SUB = LOW-SUB
                 IF CI-GROSS-MO-INCOME-1 > INCOME-HIGH1
                    MOVE CI-GROSS-MO-INCOME-1 TO INCOME-HIGH1
                    MOVE BR-NO TO INCOME-HIGH1-BRNO
                    MOVE CI-ACCTNO TO INCOME-HIGH1-ACCT.
           IF CI-GROSS-MO-INCOME-1 < TOT-INCOME-LOW(TOT-SUB, 1)
              MOVE CI-GROSS-MO-INCOME-1 TO TOT-INCOME-LOW(TOT-SUB, 1).
           IF DOC-FG = "Y"
              IF TOT-SUB = LOW-SUB
                 IF CI-GROSS-MO-INCOME-1 < INCOME-LOW1
                    MOVE CI-GROSS-MO-INCOME-1 TO INCOME-LOW1
                    MOVE BR-NO TO INCOME-LOW1-BRNO
                    MOVE CI-ACCTNO TO INCOME-LOW1-ACCT.

           ADD CI-GROSS-MO-INCOME-2 TO TOT-INCOME(TOT-SUB, 2).
           IF CI-SSNO-2 NOT = 0
              IF CI-GROSS-MO-INCOME-2 > TOT-INCOME-HIGH(TOT-SUB, 2)
                 MOVE CI-GROSS-MO-INCOME-2 TO
                       TOT-INCOME-HIGH(TOT-SUB, 2).
           IF CI-SSNO-2 NOT = 0
              IF DOC-FG = "Y"
                 IF TOT-SUB = LOW-SUB
                    IF CI-GROSS-MO-INCOME-2 > INCOME-HIGH2
                       MOVE CI-GROSS-MO-INCOME-2 TO INCOME-HIGH2
                       MOVE BR-NO TO INCOME-HIGH2-BRNO
                       MOVE CI-ACCTNO TO INCOME-HIGH2-ACCT.
           IF CI-SSNO-2 NOT = 0
              IF CI-GROSS-MO-INCOME-2 < TOT-INCOME-LOW(TOT-SUB, 2)
                 MOVE CI-GROSS-MO-INCOME-2 TO
                       TOT-INCOME-LOW(TOT-SUB, 2).
           IF CI-SSNO-2 NOT = 0
              IF DOC-FG = "Y"
                 IF TOT-SUB = LOW-SUB
                    IF CI-GROSS-MO-INCOME-2 < INCOME-LOW2
                       MOVE CI-GROSS-MO-INCOME-2 TO INCOME-LOW2
                       MOVE BR-NO TO INCOME-LOW2-BRNO
                       MOVE CI-ACCTNO TO INCOME-LOW2-ACCT.

           ADD CI-TOTAL-MO-INCOME TO TOT-MO-INCOME(TOT-SUB).
           IF CI-TOTAL-MO-INCOME > TOT-MO-INCOME-HIGH(TOT-SUB)
              MOVE CI-TOTAL-MO-INCOME TO TOT-MO-INCOME-HIGH(TOT-SUB).
           IF DOC-FG = "Y"
              IF TOT-SUB = LOW-SUB
                 IF CI-TOTAL-MO-INCOME > MO-INCOME-HIGH
                    MOVE CI-TOTAL-MO-INCOME TO MO-INCOME-HIGH
                    MOVE BR-NO TO MO-INCOME-HIGH-BRNO
                    MOVE CI-ACCTNO TO MO-INCOME-HIGH-ACCT.
           IF CI-TOTAL-MO-INCOME < TOT-MO-INCOME-LOW(TOT-SUB)
              MOVE CI-TOTAL-MO-INCOME TO TOT-MO-INCOME-LOW(TOT-SUB).
           IF DOC-FG = "Y"
              IF TOT-SUB = LOW-SUB
                 IF CI-TOTAL-MO-INCOME < MO-INCOME-LOW
                    MOVE CI-TOTAL-MO-INCOME TO MO-INCOME-LOW
                    MOVE BR-NO TO MO-INCOME-LOW-BRNO
                    MOVE CI-ACCTNO TO MO-INCOME-LOW-ACCT.

           ADD CI-LESS-MO-PAY TO TOT-MO-PMTS(TOT-SUB).
           IF CI-LESS-MO-PAY > TOT-MO-PMTS-HIGH(TOT-SUB)
              MOVE CI-LESS-MO-PAY TO TOT-MO-PMTS-HIGH(TOT-SUB).
           IF DOC-FG = "Y"
              IF TOT-SUB = LOW-SUB
                 IF CI-LESS-MO-PAY > MO-PMTS-HIGH
                    MOVE CI-LESS-MO-PAY TO MO-PMTS-HIGH
                    MOVE BR-NO TO MO-PMTS-HIGH-BRNO
                    MOVE CI-ACCTNO TO MO-PMTS-HIGH-ACCT.
           IF CI-LESS-MO-PAY < TOT-MO-PMTS-LOW(TOT-SUB)
              MOVE CI-LESS-MO-PAY TO TOT-MO-PMTS-LOW(TOT-SUB).
           IF DOC-FG = "Y"
              IF TOT-SUB = LOW-SUB
                 IF CI-LESS-MO-PAY < MO-PMTS-LOW
                    MOVE CI-LESS-MO-PAY TO MO-PMTS-LOW
                    MOVE BR-NO TO MO-PMTS-LOW-BRNO
                    MOVE CI-ACCTNO TO MO-PMTS-LOW-ACCT.

           MOVE CI-POSITION-CODE-1 TO POSITION-TEST.
           IF POSITION-NUM NOT NUMERIC
              MOVE 26 TO POS-SUB
           ELSE
           IF (POSITION-NUM < 1 OR POSITION-NUM > 25)
              MOVE 26 TO POS-SUB
           ELSE
              MOVE POSITION-NUM TO POS-SUB.
           ADD 1 TO TOT-BOR1-COUNT(TOT-SUB, POS-SUB).
      *    IF POS-SUB = 26
      *       MOVE "INVALID POSITION: #### &&&&&&" TO MESS
      *       INSPECT MESS REPLACING FIRST "####" BY BR-NO
      *       INSPECT MESS REPLACING FIRST "&&&&&&" BY CI-ACCTNO
      *       PERFORM SEND-MESS.
           ADD CI-GROSS-MO-INCOME-1 TO
                   TOT-BOR1-INCOME(TOT-SUB, POS-SUB).

           IF CI-SSNO-2 = 0
              GO TO EXIT-ADD-TOTALS.
           MOVE CI-POSITION-CODE-2 TO POSITION-TEST.
           IF POSITION-NUM NOT NUMERIC
              MOVE 26 TO POS-SUB
           ELSE
           IF (POSITION-NUM < 1 OR POSITION-NUM > 25)
              MOVE 26 TO POS-SUB
           ELSE
              MOVE POSITION-NUM TO POS-SUB.
           ADD 1 TO TOT-BOR2-COUNT(TOT-SUB, POS-SUB).
           ADD CI-GROSS-MO-INCOME-2 TO
                    TOT-BOR2-INCOME(TOT-SUB, POS-SUB).
       EXIT-ADD-TOTALS.
           EXIT.

      ******************************************************************
       TOTAL-LEVEL-CHECK SECTION.
      ******************************************************************
           MOVE 15 TO GROUP-SUB.

       LEVEL-TOTAL-LOOP.
           IF GROUP-SUB = 0
              GO TO EXIT-TOTAL-LEVEL-CHECK.

           IF WGR-SCURR(GROUP-SUB) = PREV-SCURR(GROUP-SUB)
              GO TO CONTINUE-LOOP.

           MOVE PREV-SCURR(GROUP-SUB) TO PREV-SUB.

           IF PREV-SUB = 0
              GO TO CONTINUE-LOOP.

           IF WGR-SID(PREV-SUB) NUMERIC
              GO TO CHECK-BRANCH-SID.

           MOVE WGR-SID(PREV-SUB) TO GR-ID.
           PERFORM READ-GR1-FILE.

           IF TOTAL-LEVEL-TABLE(GR-LEVEL) NOT = "Y"
              GO TO CONTINUE-LOOP.

           MOVE GR-LEVEL TO TOT-SUB.

      *----------------------------------------------------------------
       DO-GROUP-TOTALS.
           MOVE "GROUP   :" TO H-BRGR.
           MOVE GR-ID       TO H-GRID.
           MOVE GR-NAME     TO H-BRNAME.
           PERFORM HEAD-PAGE.

           PERFORM PRINT-TOTALS.

           GO TO CONTINUE-LOOP.

      *----------------------------------------------------------------
       CHECK-BRANCH-SID.
           IF TOTAL-LEVEL-TABLE(9) NOT = "Y"
              GO TO CONTINUE-LOOP.

           MOVE WGR-SID(PREV-SUB) TO WGR-WORD.
           MOVE WGR-NUMB TO BR-NO.
           PERFORM READ-BR-FILE.
           MOVE "BRANCH:" TO H-BRGR.
           MOVE BR-NO     TO H-BRNO.
           MOVE BR-NAME TO H-BRNAME.
           MOVE 9 TO TOT-SUB.
           PERFORM HEAD-PAGE.
           PERFORM PRINT-TOTALS.

      *----------------------------------------------------------------
       CONTINUE-LOOP.
           SUBTRACT 1 FROM GROUP-SUB.

           IF GROUP-SUB < WGR-SSTABLE
              GO TO EXIT-TOTAL-LEVEL-CHECK.

           GO TO LEVEL-TOTAL-LOOP.

       EXIT-TOTAL-LEVEL-CHECK.
           MOVE WGR-SCURR-TABLE TO PREV-SCURR-TABLE.

      *******************************************
       PRINT-ONE-BRANCH-TOTALS SECTION.
      *******************************************
           MOVE "BRANCH:" TO H-BRGR.
           MOVE BR-NO     TO H-BRNO.
           MOVE BR-NAME TO H-BRNAME.
           MOVE 9 TO TOT-SUB.
           PERFORM HEAD-PAGE.
           PERFORM PRINT-TOTALS.

      *******************************************
       PRINT-BRANCH-GRAND-TOTALS SECTION.
      *******************************************

           IF BEG-BR = END-BR
              GO TO EXIT-PRINT-BRANCH-GRAND.

           MOVE 0 TO H-BRNO.
           MOVE "GRAND TOTALS" TO H-BRNAME.
           MOVE 1 TO TOT-SUB.
           PERFORM HEAD-PAGE.
           PERFORM PRINT-TOTALS.
       EXIT-PRINT-BRANCH-GRAND.
           EXIT.

      *******************************************
       PRINT-TOTALS SECTION.
      *******************************************
           MOVE STAR-LINE TO DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE "TOTAL ACCOUNTS:" TO D-PROMPT1.
           MOVE TOT-COUNT(TOT-SUB) TO D-TOT.
           MOVE "TOTAL WITH CIS DATA:" TO D-PROMPT2.
           MOVE TOT-WITH-CI(TOT-SUB) TO D-TOT-WITH-CI.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE STAR-LINE TO DETAIL-LINE.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE YES-NO-HEAD TO DETAIL-LINE.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE YES-NO-HEAD2 TO DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MARRIED" TO D-PROMPT.
           MOVE TOT-MARRIED(TOT-SUB) TO YES-COUNT.
           PERFORM FORMAT-YES-NO-LINE.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "HOMEOWNER" TO D-PROMPT.
           MOVE TOT-HOMEOWNER(TOT-SUB) TO YES-COUNT.
           PERFORM FORMAT-YES-NO-LINE.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "JOINT LOAN" TO D-PROMPT.
           MOVE TOT-JOINT(TOT-SUB) TO YES-COUNT.
           PERFORM FORMAT-YES-NO-LINE.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "CHECKING ACCT" TO D-PROMPT.
           MOVE TOT-CHECKING(TOT-SUB) TO YES-COUNT.
           PERFORM FORMAT-YES-NO-LINE.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "SAVINGS ACCT" TO D-PROMPT.
           MOVE TOT-SAVINGS(TOT-SUB) TO YES-COUNT.
           PERFORM FORMAT-YES-NO-LINE.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE STAR-LINE TO DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE HIGH-LOW-HEAD TO DETAIL-LINE.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "YEARS IN AREA" TO D-PROMPT.
           MOVE TOT-YRS-AREA-HIGH(TOT-SUB) TO D-HIGH-NO.
           IF TOT-YRS-AREA-LOW(TOT-SUB) = 99999999
              MOVE 0 TO D-LOW-NO
           ELSE
              MOVE TOT-YRS-AREA-LOW(TOT-SUB) TO D-LOW-NO.
           COMPUTE WK-AVG-1DEC ROUNDED = TOT-YRS-AREA(TOT-SUB) /
                                         TOT-WITH-CI(TOT-SUB).
           MOVE WK-AVG-1DEC TO D-AVG-NO.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "YEARS CURRENT ADDRESS" TO D-PROMPT.
           COMPUTE WORK-YEARS ROUNDED = TOT-MONTHS-ADDR-HIGH(TOT-SUB)
                                        / 12.
           MOVE WORK-YEARS TO D-HIGH-NO.
           IF TOT-MONTHS-ADDR-LOW(TOT-SUB) = 99999999
              MOVE 0 TO D-LOW-NO
           ELSE
              COMPUTE WORK-YEARS ROUNDED = TOT-MONTHS-ADDR-LOW(TOT-SUB)
                                           / 12
              MOVE WORK-YEARS TO D-LOW-NO.
           COMPUTE WK-AVG-1DEC ROUNDED = (TOT-MONTHS-ADDR(TOT-SUB) / 12)
                                         / TOT-WITH-CI(TOT-SUB).
           MOVE WK-AVG-1DEC TO D-AVG-NO.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "RENT/MORTGAGE PAYMENT" TO D-PROMPT.
           MOVE TOT-RENT-HIGH(TOT-SUB) TO D-HIGH-AMT.
           IF TOT-RENT-LOW(TOT-SUB) = 99999999
              MOVE 0 TO D-LOW-AMT
           ELSE
              MOVE TOT-RENT-LOW(TOT-SUB) TO D-LOW-AMT.
           COMPUTE WK-AVG-0DEC ROUNDED = TOT-RENT(TOT-SUB) /
                                         TOT-WITH-CI(TOT-SUB).
           MOVE WK-AVG-0DEC TO D-AVG-AMT.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "(1) LENGTH OF EMPLOYMENT" TO D-PROMPT.
           COMPUTE WORK-YEARS ROUNDED =
               TOT-MONTHS-EMPL-HIGH(TOT-SUB, 1) / 12.
           MOVE WORK-YEARS TO D-HIGH-NO.
           IF TOT-MONTHS-EMPL-LOW(TOT-SUB, 1) = 99999999
              MOVE 0 TO D-LOW-NO
           ELSE
              COMPUTE WORK-YEARS ROUNDED =
                  TOT-MONTHS-EMPL-LOW(TOT-SUB, 1) / 12
              MOVE WORK-YEARS TO D-LOW-NO.
           COMPUTE WK-AVG-1DEC ROUNDED =
                          (TOT-MONTHS-EMPL(TOT-SUB, 1) / 12)
                         / TOT-WITH-CI(TOT-SUB).
           MOVE WK-AVG-1DEC TO D-AVG-NO.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "(1) NET MONTHLY INCOME" TO D-PROMPT.
           MOVE TOT-INCOME-HIGH(TOT-SUB, 1) TO D-HIGH-AMT.
           IF TOT-INCOME-LOW(TOT-SUB, 1) = 99999999
              MOVE 0 TO D-LOW-AMT
           ELSE
              MOVE TOT-INCOME-LOW(TOT-SUB, 1) TO D-LOW-AMT.
           COMPUTE WK-AVG-0DEC ROUNDED = TOT-INCOME(TOT-SUB, 1) /
                                         TOT-WITH-CI(TOT-SUB).
           MOVE WK-AVG-0DEC TO D-AVG-AMT.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "(2) LENGTH OF EMPLOYMENT" TO D-PROMPT.
           COMPUTE WORK-YEARS ROUNDED =
               TOT-MONTHS-EMPL-HIGH(TOT-SUB, 2) / 12.
           MOVE WORK-YEARS TO D-HIGH-NO.
           IF TOT-MONTHS-EMPL-LOW(TOT-SUB, 2) = 99999999
              MOVE 0 TO D-LOW-NO
           ELSE
              COMPUTE WORK-YEARS ROUNDED =
                  TOT-MONTHS-EMPL-LOW(TOT-SUB, 2) / 12
              MOVE WORK-YEARS TO D-LOW-NO.
           COMPUTE WK-AVG-1DEC ROUNDED =
                      (TOT-MONTHS-EMPL(TOT-SUB, 2) / 12)
                     / TOT-JOINT(TOT-SUB).
           MOVE WK-AVG-1DEC TO D-AVG-NO.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "(2) NET MONTHLY INCOME" TO D-PROMPT.
           MOVE TOT-INCOME-HIGH(TOT-SUB, 2) TO D-HIGH-AMT.
           IF TOT-INCOME-LOW(TOT-SUB, 2) = 99999999
              MOVE 0 TO D-LOW-AMT
           ELSE
              MOVE TOT-INCOME-LOW(TOT-SUB, 2) TO D-LOW-AMT.
           COMPUTE WK-AVG-0DEC ROUNDED = TOT-INCOME(TOT-SUB, 2) /
                                         TOT-JOINT(TOT-SUB).
           MOVE WK-AVG-0DEC TO D-AVG-AMT.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL MONTHLY INCOME" TO D-PROMPT.
           MOVE TOT-MO-INCOME-HIGH(TOT-SUB) TO D-HIGH-AMT.
           IF TOT-MO-INCOME-LOW(TOT-SUB) = 99999999
              MOVE 0 TO D-LOW-AMT
           ELSE
              MOVE TOT-MO-INCOME-LOW(TOT-SUB) TO D-LOW-AMT.
           COMPUTE WK-AVG-0DEC ROUNDED = TOT-MO-INCOME(TOT-SUB) /
                                         TOT-WITH-CI(TOT-SUB).
           MOVE WK-AVG-0DEC TO D-AVG-AMT.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "MONTHLY PAYMENTS" TO D-PROMPT.
           MOVE TOT-MO-PMTS-HIGH(TOT-SUB) TO D-HIGH-AMT.
           IF TOT-MO-PMTS-LOW(TOT-SUB) = 99999999
              MOVE 0 TO D-LOW-AMT
           ELSE
              MOVE TOT-MO-PMTS-LOW(TOT-SUB) TO D-LOW-AMT.
           COMPUTE WK-AVG-0DEC ROUNDED = TOT-MO-PMTS(TOT-SUB) /
                                       TOT-WITH-CI(TOT-SUB).
           MOVE WK-AVG-0DEC TO D-AVG-AMT.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE STAR-LINE TO DETAIL-LINE.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE POSITION-HEAD1 TO DETAIL-LINE.
           MOVE 2 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE POSITION-HEAD2 TO DETAIL-LINE.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE 0 TO SUB.
       NEXT-POSITION-CODE.
           ADD 1 TO SUB.
           IF SUB > 26
              GO TO EXIT-PRINT-TOTALS.

           MOVE "JT" TO CD-TYPE.
           MOVE SUB  TO CD-CODE.
           PERFORM READ-CD1-FILE.
           IF IO-BAD
              IF SUB < 26
                 GO TO NEXT-POSITION-CODE.

           IF TOT-BOR1-COUNT(TOT-SUB, SUB) = 0
              IF TOT-BOR2-COUNT(TOT-SUB, SUB) = 0
                 GO TO NEXT-POSITION-CODE.
           IF SUB < 26
              MOVE SUB TO D-POS-CODE
              MOVE "-" TO D-DASH
              MOVE CD-DESC TO D-POSITION
           ELSE
              MOVE "INVALID POSITION CODES" TO D-POSITION.
           MOVE TOT-BOR1-COUNT(TOT-SUB, SUB) TO D-POS-COUNT1.
           COMPUTE WK-AVG-0DEC ROUNDED =
                      TOT-BOR1-INCOME(TOT-SUB, SUB) /
                      TOT-BOR1-COUNT(TOT-SUB, SUB).
           MOVE WK-AVG-0DEC TO D-AVG-INCOME1.
           MOVE TOT-BOR2-COUNT(TOT-SUB, SUB) TO D-POS-COUNT2.
           COMPUTE WK-AVG-0DEC ROUNDED =
                      TOT-BOR2-INCOME(TOT-SUB, SUB) /
                      TOT-BOR2-COUNT(TOT-SUB, SUB).
           MOVE WK-AVG-0DEC TO D-AVG-INCOME2.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           GO TO NEXT-POSITION-CODE.
       EXIT-PRINT-TOTALS.
           MOVE LOW-VALUES TO TOTAL-TAB(TOT-SUB).
           MOVE 99999999 TO TOT-YRS-AREA-LOW(TOT-SUB)
                            TOT-MONTHS-ADDR-LOW(TOT-SUB)
                            TOT-RENT-LOW(TOT-SUB)
                            TOT-MONTHS-EMPL-LOW(TOT-SUB, 1)
                            TOT-MONTHS-EMPL-LOW(TOT-SUB, 2)
                            TOT-INCOME-LOW(TOT-SUB, 1)
                            TOT-INCOME-LOW(TOT-SUB, 2)
                            TOT-MO-INCOME-LOW(TOT-SUB)
                            TOT-MO-PMTS-LOW(TOT-SUB).

      *******************************************
       FORMAT-YES-NO-LINE SECTION.
      *******************************************
           MOVE YES-COUNT TO D-YES.
           COMPUTE WORK-PCT ROUNDED = YES-COUNT /
                                      TOT-WITH-CI(TOT-SUB) * 100.
           MOVE WORK-PCT TO D-YES-PCT.
           IF WORK-PCT > 0
              MOVE "%" TO D-PCT1.
           COMPUTE WORK-COUNT = TOT-WITH-CI(TOT-SUB) - YES-COUNT.
           MOVE WORK-COUNT TO D-NO.
           COMPUTE WORK-PCT ROUNDED = WORK-COUNT /
                                      TOT-WITH-CI(TOT-SUB) * 100.
           MOVE WORK-PCT TO D-NO-PCT.
           IF WORK-PCT > 0
              MOVE "%" TO D-PCT2.

      *****************************************
       PRINT-DOC SECTION.
      *****************************************
           MOVE STAR-LINE TO DETAIL-LINE.
           MOVE 77 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.
           MOVE DOC-HEAD TO DETAIL-LINE.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "YEARS IN AREA" TO D-PROMPT.
           MOVE "HIGH" TO D-HI-LOW.
           MOVE YRS-AREA-HIGH TO D-DOC-NO.
           MOVE YRS-AREA-HIGH-BRNO TO D-BRNO.
           MOVE YRS-AREA-HIGH-ACCT TO D-ACCT.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "YEARS IN AREA" TO D-PROMPT.
           MOVE "LOW" TO D-HI-LOW.
           MOVE YRS-AREA-LOW TO D-DOC-NO.
           MOVE YRS-AREA-LOW-BRNO TO D-BRNO.
           MOVE YRS-AREA-LOW-ACCT TO D-ACCT.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "YEARS CURRENT ADDRESS" TO D-PROMPT.
           MOVE "HIGH" TO D-HI-LOW.
           MOVE MONTHS-ADDR-HIGH TO D-DOC-NO.
           MOVE MONTHS-ADDR-HIGH-BRNO TO D-BRNO.
           MOVE MONTHS-ADDR-HIGH-ACCT TO D-ACCT.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "YEARS CURRENT ADDRESS" TO D-PROMPT.
           MOVE "LOW" TO D-HI-LOW.
           MOVE MONTHS-ADDR-LOW TO D-DOC-NO.
           MOVE MONTHS-ADDR-LOW-BRNO TO D-BRNO.
           MOVE MONTHS-ADDR-LOW-ACCT TO D-ACCT.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "RENT/MORTGAGE PAYMENT" TO D-PROMPT.
           MOVE "HIGH" TO D-HI-LOW.
           MOVE RENT-HIGH TO D-DOC-AMT.
           MOVE RENT-HIGH-BRNO TO D-BRNO.
           MOVE RENT-HIGH-ACCT TO D-ACCT.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "RENT/MORTGAGE PAYMENT" TO D-PROMPT.
           MOVE "LOW" TO D-HI-LOW.
           MOVE RENT-LOW TO D-DOC-AMT.
           MOVE RENT-LOW-BRNO TO D-BRNO.
           MOVE RENT-LOW-ACCT TO D-ACCT.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "(1) LENGTH OF EMPLOYMENT" TO D-PROMPT.
           MOVE "HIGH" TO D-HI-LOW.
           MOVE MONTHS-EMPL-HIGH1 TO D-DOC-NO.
           MOVE MONTHS-EMPL-HIGH1-BRNO TO D-BRNO.
           MOVE MONTHS-EMPL-HIGH1-ACCT TO D-ACCT.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "(1) LENGTH OF EMPLOYMENT" TO D-PROMPT.
           MOVE "LOW" TO D-HI-LOW.
           MOVE MONTHS-EMPL-LOW1 TO D-DOC-NO.
           MOVE MONTHS-EMPL-LOW1-BRNO TO D-BRNO.
           MOVE MONTHS-EMPL-LOW1-ACCT TO D-ACCT.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "(1) NET MONTHLY INCOME" TO D-PROMPT.
           MOVE "HIGH" TO D-HI-LOW.
           MOVE INCOME-HIGH1 TO D-DOC-AMT.
           MOVE INCOME-HIGH1-BRNO TO D-BRNO.
           MOVE INCOME-HIGH1-ACCT TO D-ACCT.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "(1) NET MONTHLY INCOME" TO D-PROMPT.
           MOVE "LOW" TO D-HI-LOW.
           MOVE INCOME-LOW1 TO D-DOC-AMT.
           MOVE INCOME-LOW1-BRNO TO D-BRNO.
           MOVE INCOME-LOW1-ACCT TO D-ACCT.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "(2) LENGTH OF EMPLOYMENT" TO D-PROMPT.
           MOVE "HIGH" TO D-HI-LOW.
           MOVE MONTHS-EMPL-HIGH2 TO D-DOC-NO.
           MOVE MONTHS-EMPL-HIGH2-BRNO TO D-BRNO.
           MOVE MONTHS-EMPL-HIGH2-ACCT TO D-ACCT.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "(2) LENGTH OF EMPLOYMENT" TO D-PROMPT.
           MOVE "LOW" TO D-HI-LOW.
           MOVE MONTHS-EMPL-LOW2 TO D-DOC-NO.
           MOVE MONTHS-EMPL-LOW2-BRNO TO D-BRNO.
           MOVE MONTHS-EMPL-LOW2-ACCT TO D-ACCT.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "(2) NET MONTHLY INCOME" TO D-PROMPT.
           MOVE "HIGH" TO D-HI-LOW.
           MOVE INCOME-HIGH2 TO D-DOC-AMT.
           MOVE INCOME-HIGH2-BRNO TO D-BRNO.
           MOVE INCOME-HIGH2-ACCT TO D-ACCT.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "(2) NET MONTHLY INCOME" TO D-PROMPT.
           MOVE "LOW" TO D-HI-LOW.
           MOVE INCOME-LOW2 TO D-DOC-AMT.
           MOVE INCOME-LOW2-BRNO TO D-BRNO.
           MOVE INCOME-LOW2-ACCT TO D-ACCT.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "TOTAL MONTHLY INCOME" TO D-PROMPT.
           MOVE "HIGH" TO D-HI-LOW.
           MOVE MO-INCOME-HIGH TO D-DOC-AMT.
           MOVE MO-INCOME-HIGH-BRNO TO D-BRNO.
           MOVE MO-INCOME-HIGH-ACCT TO D-ACCT.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "RENT/MORTGAGE PAYMENT" TO D-PROMPT.
           MOVE "LOW" TO D-HI-LOW.
           MOVE MO-INCOME-LOW TO D-DOC-AMT.
           MOVE MO-INCOME-LOW-BRNO TO D-BRNO.
           MOVE MO-INCOME-LOW-ACCT TO D-ACCT.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.


           MOVE "MONTHLY PAYMENTS" TO D-PROMPT.
           MOVE MO-PMTS-HIGH TO D-DOC-AMT.
           MOVE MO-PMTS-HIGH-BRNO TO D-BRNO.
           MOVE MO-PMTS-HIGH-ACCT TO D-ACCT.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.

           MOVE "RENT/MORTGAGE PAYMENT" TO D-PROMPT.
           MOVE "LOW" TO D-HI-LOW.
           MOVE MO-PMTS-LOW TO D-DOC-AMT.
           MOVE MO-PMTS-LOW-BRNO TO D-BRNO.
           MOVE MO-PMTS-LOW-ACCT TO D-ACCT.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-DETAIL-LINE.


      *******************************************
       WRITE-DETAIL-LINE SECTION.
      *******************************************
           ADD LN-FEED LN-COUNT GIVING LN-WK.

           IF LN-WK > EXT-CONAME-RPT-LINES
              PERFORM HEAD-PAGE.

           MOVE DETAIL-LINE TO PR-REC.
           PERFORM WRITE-PR-REC.
           ADD LN-FEED TO LN-COUNT.
           MOVE 1 TO LN-FEED.
           MOVE SPACES TO DETAIL-LINE.

      *******************************************
       HEAD-PAGE SECTION.
      *******************************************
           ADD 1 TO PAGE-NUMBER.
           MOVE PAGE-NUMBER TO H-PAGE-NO.
           PERFORM GET-TIME.
           MOVE T-HH TO H-HR.
           MOVE T-MM TO H-MIN.

           MOVE HEAD1 TO PR-REC.
           MOVE 99 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD2 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD3 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           MOVE HEAD4 TO PR-REC.
           MOVE 1 TO LN-FEED.
           PERFORM WRITE-PR-REC.
           MOVE 4 TO LN-COUNT.

       END-HEAD-PAGE.
           MOVE 2 TO LN-FEED.

      *******************************************
       SEND-PROGRESS-INDICATOR SECTION.
      *******************************************

           MOVE BR-NO                  TO PROGID-BRANCH.
           MOVE "LEGEND:B."            TO PROGID-LINE-LIST.
           MOVE PRU-LP                 TO PROGID-PRU-LP.
           MOVE EXT-ROUTE-BUF          TO PROGID-ROUTE-BUF.
           MOVE 250                    TO PROGID-SLEEP-COUNTER-MAX.

           PERFORM GET-TIME.
           MOVE TIME-EDIT              TO PROGID-TIME.

           MOVE "B"                    TO PROGID-TYPE.
           MOVE PROGID-TEXT-PROCESSING TO PROGID-TEXT.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PROGID-DISPLAY.

       SEND-PROGID-INDICATOR-EXIT.
           EXIT.

      ************************************************
       FORM-RESET SECTION.
      ************************************************
           MOVE EXT-CONAME-CONAME TO CIREPT-CONAME.
           MOVE EXT-CONAME-SYSDATE TO CIREPT-SYSDATE.
           DISPLAY CIREPT-SCREEN UPON EXT-EOBUF-WINDOW-HANDLE.
           MOVE CIREPT-FORM-FIELDS TO WR-BUF.
           MOVE CIREPT-FORM-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

       FORM-RESET-EXIT.
           EXIT.

      ************************************************
       SETUP-LINK-INFO SECTION.
      ************************************************
           MOVE CIREPT-HELP--FILE TO HELP-LINK-FILE.
           MOVE CIREPT-HELP--DIR TO HELP-LINK-DIR.

      ************************************************
       VDU-CONVERT-FIELD SECTION.
      ************************************************
           MOVE "00" TO STAT.
           MOVE VDU-FIELD-TO-CONVERT TO VDU-ALP-FIELD.
           EVALUATE TRUE
              COPY "LIBLP/CIREPT_EVA.CPY".
              WHEN OTHER       MOVE   "2A" TO STAT
           END-EVALUATE.

      ******************************************************************
       MISC-PARA SECTION.
      ******************************************************************
       COPY "LIBGB/GETPR.CPY".
       COPY "LIBGB/MKTEMP.CPY".
       COPY "LIBGB/ACCESS.CPY".
       COPY "LIBGB/DACCESS.CPY".
       COPY "LIBGB/SYSTEM.CPY".
       COPY "LIBGB/RMFILE.CPY".
       COPY "LIBGB/GETENV.CPY".
      *=================================================================
        SEND-LEGEND.
           MOVE LEGEND TO WR-BUF.
           MOVE LEGEND-LIST TO WR-LIST.
           PERFORM CALL-WRFORM.

      ******************************************************************
      *
      *     C O P Y - L I B R A R Y - S E C T I O N S
      *
      ******************************************************************
       COPY "LIBWI/GRTOTL.CPY".
       COPY "LIBGB/PROGID.CPY".
       COPY "LIBGB/TIMEIO.CPY".
       COPY "LIBGB/SCREEN.CPY".
       COPY "LIBGB/DATER.CPY".
       COPY "LIBGB/GRWORK.CPY".
       COPY "LIBGB/GRSERCH.CPY".

      ******************************************************************
       IO-ROUTINES SECTION.
      ******************************************************************
       COPY "LIBGB/CONNECT_SQL.CPY".
       COPY "LIBLP/LPLNGS_SQL.CPY".
       COPY "LIBLP/LPCIGS_SQL.CPY".
       COPY "LIBLP/LPCDGS_SQL.CPY".
       COPY "LIBLP/LPBWGS_SQL.CPY".
       COPY "LIBGB/GBGRGS_SQL.CPY".
       COPY "LIBGB/GBBRGS_SQL.CPY".
       COPY "LIBGB/GBPRINT.CPY".
       COPY "LIBGB/GBGR1RN.CPY".
       COPY "LIBGB/GBBRRN.CPY".
       COPY "LIBLP/LPLN3RN.CPY".
       COPY "LIBLP/LPCD1RN.CPY".
       COPY "LIBLP/LPCI1RN.CPY".
       COPY "LIBLP/LPBW1RN.CPY".
       COPY "LIBGB/FERRORS.CPY".
           MOVE "E" TO EXT-EOBUF-ERRCD.

      ******************************************************************
       END-PROGRAM SECTION.
      ******************************************************************
       END-RUN.
           PERFORM CLOSE-FILES.

           IF EXT-ROUTE-BUF = "00"
              PERFORM PRU-OUT.

           IF NO-RECORDS-FOUND
              MOVE "NO REPORT FOR SELECTED RANGE" TO MESS
              PERFORM SEND-MESS.

      *-----------------------------------------------------------------
       EXIT-PROG.
           IF SV-BEG-BR NOT = 9999
              IF EXT-EOBUF-GROUP-FG = "Y"
                 MOVE SV-BEG-BR TO BEG-BR
                 MOVE SV-END-BR TO END-BR
                 MOVE SV-GROUP-FLAG TO GROUP-FLAG
                 MOVE SV-ENT-GROUP TO ENT-GROUP
              ELSE
                 MOVE SV-BEG-BR TO BEG-BR
                 MOVE SV-END-BR TO END-BR.

           IF EXT-ROUTE-BUF = "00"
              MOVE "LP/LPR2MU" TO FORM-NAM
              MOVE FORM-PATH TO FORM-PATHNAME.

       STOP-RUN.
           IF EXT-ROUTE-BUF = "00" OR "40" OR "50"
              DISPLAY SPACES LINE 0 COL 0 ERASE SCREEN
              DESTROY CIREPT-SCREEN
              DESTROY EXT-EOBUF-WINDOW-HANDLE.
           EXIT PROGRAM.
